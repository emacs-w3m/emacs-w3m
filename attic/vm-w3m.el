;;; vm-w3m.el --- additional functions to make VM use emacs-w3m for HTML mails

;; Copyright (C) 2003 Katsumi Yamaoka

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
;; USA.

;;; Commentary:

;; You need to have w3m and emacs-w3m installed for this module to
;; work.  Visit <URL:http://emacs-w3m.namazu.org/> for details.
;; You don't have to change VM at all.  Simply load this module and
;; you will see HTML mails inlined by emacs-w3m in the VM presentation
;; buffer.  It was tested with VM 7.17, 7.18 and 7.19.

;; Please don't complain to the author or the emacs-w3m team, even if
;; it becomes impossible to use this module in the future.  There is
;; currently no person familiar with VM in the emacs-w3m developers.
;; Supposing you volunteer for the development, it will greatly be
;; welcomed.  Or if you incorporate these functions in VM, we need to
;; offer the maximum support.

;;; Code:

(require 'vm-mime)
(require 'w3m)

(defvar vm-message-pointer)
(defvar vm-mode-map)
(defvar w3m-minor-mode-map)

(defcustom vm-w3m-display-inline-images t
  "Non-nil means VM will allow retrieving images in the HTML contents
with the <img> tags.  See also the documentation for the variable
`vm-w3m-safe-url-regexp'."
  :group 'w3m
  :type 'boolean)

(defcustom vm-w3m-safe-url-regexp "\\`cid:"
  "Regexp matching URLs which are considered to be safe.
Some HTML mails might contain a nasty trick used by spammers, using
the <img> tag which is far more evil than the [Click Here!] button.
It is most likely intended to check whether the ominous spam mail has
reached your eyes or not, in which case the spammer knows for sure
that your email address is valid.  It is done by embedding an
identifier string into a URL that you might automatically retrieve
when displaying the image.  The default value is \"\\\\`cid:\" which only
matches parts embedded to the Multipart/Related type MIME contents and
VM will never connect to the spammer's site arbitrarily.  You may set
this variable to nil if you consider all urls to be safe."
  :group 'w3m
  :type '(choice (regexp :tag "Regexp")
		 (const :tag "All URLs are safe" nil)))

(eval-and-compile
  (or (featurep 'xemacs) (>= emacs-major-version 21)
      (defvar vm-w3m-mode-map nil
	"Keymap for text/html parts inlined by emacs-w3m.
This keymap will be bound only when Emacs 20 is running and overwritten
by the value of `w3m-minor-mode-map'.  In order to add some commands to
this keymap, add them to `w3m-minor-mode-map' instead of this keymap.")))

(defun vm-w3m-cid-retrieve (url &rest args)
  "Insert a content pointed by URL if it has the cid: scheme."
  (if (string-match "\\`cid:" url)
      (let ((part-list (save-excursion
			 (set-buffer w3m-current-buffer)
			 (vm-mm-layout-parts
			  (vm-mm-layout (car vm-message-pointer)))))
	    layout start type)
	(setq url (concat "<" (substring url (match-end 0)) ">"))
	(while (and part-list
		    (not type))
	  (setq layout (car part-list)
		part-list (cdr part-list))
	  (if (equal url (vm-mm-layout-id layout))
	      (progn
		(setq start (point))
		(vm-mime-insert-mime-body layout)
		(vm-mime-transfer-decode-region layout start (point))
		(setq type (car (vm-mm-layout-type layout))))))
	type)))

(or (assq 'vm-presentation-mode w3m-cid-retrieve-function-alist)
    (setq w3m-cid-retrieve-function-alist
	  (cons '(vm-presentation-mode . vm-w3m-cid-retrieve)
		w3m-cid-retrieve-function-alist)))

(defun vm-w3m-local-map-property ()
  (if (and (boundp 'w3m-minor-mode-map) w3m-minor-mode-map)
      (if (or (featurep 'xemacs) (>= emacs-major-version 21))
	  (list 'keymap w3m-minor-mode-map)
	(list 'local-map
	      (or vm-w3m-mode-map
		  (progn
		    (setq vm-w3m-mode-map
			  (copy-keymap w3m-minor-mode-map))
		    (set-keymap-parent vm-w3m-mode-map vm-mode-map)
		    vm-w3m-mode-map))))))

(defadvice vm-mime-can-display-internal (around
					 return-t-for-text/html
					 (layout &optional deep)
					 activate)
  "Always return non-nil for the text/html MIME type."
  (or (setq ad-return-value
	    (vm-mime-types-match "text/html"
				 (car (vm-mm-layout-type layout))))
      ad-do-it))

(defadvice vm-mime-display-internal-text/html (around
					       use-emacs-w3m (layout)
					       activate)
  "Use emacs-w3m to inline HTML mails in the VM presentation buffer."
  (setq w3m-display-inline-images vm-w3m-display-inline-images)
  (condition-case error-data
      (let ((buffer-read-only nil)
	    (start (point))
	    (charset (or (vm-mime-get-parameter layout "charset")
			 "us-ascii"))
	    end
	    (w3m-safe-url-regexp vm-w3m-safe-url-regexp)
	    w3m-force-redisplay)
	(message "Inlining text/html...")
	(vm-mime-insert-mime-body layout)
	(setq end (point-marker))
	(vm-mime-transfer-decode-region layout start end)
	(vm-mime-charset-decode-region charset start end)
	;; See the comment about "z" in the original function.
	(goto-char end)
	(insert-before-markers "z")
	(w3m-region start (1- end))
	(goto-char end)
	(delete-char -1)
	(add-text-properties
	 start end
	 (nconc (vm-w3m-local-map-property)
		;; Put the mark meaning that this part was
		;; inlined by emacs-w3m.
		'(text-rendered-by-emacs-w3m t)))
	(goto-char end)
	(message "Inlining text/html... done")
	(setq ad-return-value t))
    (error (vm-set-mm-layout-display-error
	    layout
	    (format "Inline HTML display failed: %s"
		    (prin1-to-string error-data)))
	   (message "%s" (vm-mm-layout-display-error layout))
	   (sleep-for 2)
	   (setq ad-return-value nil))))

(defun vm-mime-display-internal-multipart/related (layout)
  "Decode Multipart/Related body parts.
This function decodes the ``start'' part (see RFC2387) only.  The
other parts will be decoded by the other VM functions through
emacs-w3m."
  (let* ((part-list (vm-mm-layout-parts layout))
	 (start-part (car part-list))
	 (start-id (vm-mime-get-parameter layout "start"))
	 layout)
    ;; Look for the start part.
    (if start-id
	(while part-list
	  (setq layout (car part-list))
	  (if (equal start-id (vm-mm-layout-id layout))
	      (setq start-part layout
		    part-list nil)
	    (setq part-list (cdr part-list)))))
    (vm-decode-mime-layout start-part)))

(eval-when-compile
  (defvar vm-mail-buffer)
  (defvar vm-presentation-buffer)
  (autoload 'vm-buffer-variable-value "vm-misc"))

(defun vm-w3m-safe-toggle-inline-images (&optional arg)
  "Toggle displaying of all images in the presentation buffer.
If the prefix arg is given, all images are considered to be safe."
  (interactive "P")
  (let ((buffer (cond ((eq major-mode 'vm-summary-mode)
		       (vm-buffer-variable-value vm-mail-buffer
						 'vm-presentation-buffer))
		      ((eq major-mode 'vm-presentation-mode)
		       (current-buffer))
		      ((eq major-mode 'vm-mode)
		       vm-presentation-buffer))))
    (if (buffer-live-p buffer)
	(save-excursion
	  (set-buffer buffer)
	  (w3m-safe-toggle-inline-images arg)))))

(defun vm-w3m-uninstall ()
  "Don't let VM use emacs-w3m.
To re-install it, load the vm-w3m module again."
  (interactive)
  (ad-unadvise 'vm-mime-can-display-internal)
  (ad-unadvise 'vm-mime-display-internal-text/html)
  (fmakunbound 'vm-mime-display-internal-multipart/related))

;;; vm-w3m.el ends here

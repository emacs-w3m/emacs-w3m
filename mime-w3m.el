;;; mime-w3m.el --- mime-view content filter for text

;; Copyright (C) 2001, 2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Akihiro Arisawa    <ari@mbf.sphere.ne.jp>
;; Keywords: HTML, MIME, multimedia, mail, news

;; This file is *NOT* yet part of SEMI (Suite of Emacs MIME Interfaces).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;;; Install:

;; (1) Install SEMI.
;; (2) Put this file to appropriate directory.
;; (3) Write these following code to your ~/.emacs or ~/.gnus.
;;
;;        (require 'mime-w3m)


;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'w3m)
  (require 'mime)
  (defvar mime-setup-enable-inline-html))

(defcustom mime-w3m-display-inline-images 'default
  "Non-nil means that inline images are displayed."
  :group 'w3m
  :group 'mime-view
  :type 'boolean)

(defvar mime-w3m-mode-map nil)
(defvar mime-w3m-message-structure nil)
(make-variable-buffer-local 'mime-w3m-message-structure)

(eval-and-compile
  (when (featurep 'xemacs)
    (autoload 'font-set-face-background "font" nil t)))

(defun mime-w3m-insinuate ()
  "Insinuate `mime-w3m' module to SEMI."
  (setq mime-setup-enable-inline-html nil)
  (eval-after-load "mime-view"
    '(progn
       (ctree-set-calist-strictly
	'mime-preview-condition
	'((type . text)
	  (subtype . html)
	  (body . visible)
	  (body-presentation-method . mime-w3m-preview-text/html)))
       (set-alist 'mime-view-type-subtype-score-alist '(text . html) 3))))

(defvar mime-w3m-mode-command-alist
  '((backward-char)
    (describe-mode)
    (forward-char)
    (goto-line)
    (next-line)
    (previous-line)
    (w3m-antenna)
    (w3m-antenna-add-current-url)
    (w3m-bookmark-add-current-url)
    (w3m-bookmark-add-this-url)
    (w3m-bookmark-view)
    (w3m-close-window)
    (w3m-copy-buffer)
    (w3m-delete-buffer)
    (w3m-dtree)
    (w3m-edit-current-url)
    (w3m-edit-this-url)
    (w3m-gohome)
    (w3m-goto-url)
    (w3m-goto-url-new-session)
    (w3m-history)
    (w3m-history-restore-position)
    (w3m-history-store-position)
    (w3m-mouse-view-this-url . mime-w3m-mouse-view-this-url)
    (w3m-namazu)
    (w3m-next-buffer)
    (w3m-previous-buffer)
    (w3m-quit)
    (w3m-redisplay-with-charset)
    (w3m-reload-this-page)
    (w3m-scroll-down-or-previous-url)
    (w3m-scroll-up-or-next-url)
    (w3m-search)
    (w3m-select-buffer)
    (w3m-switch-buffer)
    (w3m-view-header)
    (w3m-view-parent-page)
    (w3m-view-previous-page)
    (w3m-view-source)
    (w3m-view-this-url . mime-w3m-view-this-url)
    (w3m-weather))
  "Alist of commands to use for emacs-w3m in the MIME-View buffer.  Each
element looks like (FROM-COMMAND . TO-COMMAND); FROM-COMMAND should be
registered in `w3m-mode-map' which will be substituted by TO-COMMAND
in `mime-w3m-mode-map'.  If TO-COMMAND is nil, a MIME-View command key
will not be substituted.")

(defsubst mime-w3m-setup ()
  "Setup `mime-w3m' module."
  (require 'w3m)
  (when (eq mime-w3m-display-inline-images 'default)
    (setq mime-w3m-display-inline-images w3m-default-display-inline-images))
  (unless (assq 'mime-view-mode w3m-cid-retrieve-function-alist)
    (push (cons 'mime-view-mode 'mime-w3m-cid-retrieve)
	  w3m-cid-retrieve-function-alist))
  (unless mime-w3m-mode-map
    (setq mime-w3m-mode-map (copy-keymap w3m-mode-map))
    (dolist (def mime-w3m-mode-command-alist)
      (condition-case nil
	  (substitute-key-definition (car def) (cdr def) mime-w3m-mode-map)
	(error)))
    ;; override widget.
    (if (featurep 'xemacs)
	(define-key mime-w3m-mode-map [(button2-down)] 'ignore)
      (define-key mime-w3m-mode-map [down-mouse-2] 'ignore))))

(def-edebug-spec mime-w3m-save-background-color t)
(defmacro mime-w3m-save-background-color (&rest body)
  (if (featurep 'xemacs)
      `(let ((color (color-name (face-background 'default))))
	 (prog1
	     (progn ,@body)
	   (font-set-face-background 'default color (current-buffer))
	   ))
    (cons 'progn body)))

;;;###autoload
(defun mime-w3m-preview-text/html (entity situation)
  (mime-w3m-setup)
  (setq mime-w3m-message-structure (mime-find-root-entity entity))
  (let ((p (point))
	(xref
	 (or (mime-entity-fetch-field entity "xref")
	     (mime-entity-fetch-field mime-w3m-message-structure "xref"))))
    (goto-char p)
    (insert "\n")
    (goto-char p)
    (mime-w3m-save-background-color
     (save-restriction
       (narrow-to-region p p)
       (mime-insert-text-content entity)
       (run-hooks 'mime-text-decode-hook)
       (condition-case err
	   (let ((w3m-safe-url-regexp "\\`cid:")
		 (w3m-display-inline-images mime-w3m-display-inline-images))
	     (w3m-region p
			 (point-max)
			 (and (stringp xref)
			      (string-match "\\`http://" xref)
			      xref))
	     (add-text-properties p (point-max)
				  (list (if (or (featurep 'xemacs)
						(>= emacs-major-version 21))
					    'keymap
					  'local-map)
					mime-w3m-mode-map
					'text-rendered-by-mime-w3m t)))
	 (error (message (format "%s" err))))))))

;; To avoid byte-compile warning in `mime-w3m-cid-retrieve'.
(autoload 'mime-uri-parse-cid "mime-parse")

(defun mime-w3m-cid-retrieve (url &rest args)
  (let ((entity (mime-find-entity-from-content-id
		 (mime-uri-parse-cid url)
		 (with-current-buffer w3m-current-buffer
		   mime-w3m-message-structure))))
    (when entity
      (mime-insert-entity-content entity)
      (mime-entity-type/subtype entity))))

(defun mime-w3m-view-this-url ()
  "View the URL of the link under point."
  (interactive)
  (let ((url (w3m-anchor)))
    (cond
     (url (w3m url))
     ((w3m-image)
      (if (w3m-display-graphic-p)
	  (w3m-toggle-inline-image)
	(w3m-view-image))))))

(defun mime-w3m-mouse-view-this-url (event)
  (interactive "e")
  (mouse-set-point event)
  (mime-w3m-view-this-url))

(let (current-load-list)
  (defadvice kill-new (before strip-keymap-properties-from-kill
			      (string &optional replace) activate)
    "Advised by emacs-w3m.
Strip `keymap' or `local-map' properties from a killed string."
    (if (text-property-any 0 (length string)
			   'text-rendered-by-mime-w3m t string)
	(remove-text-properties 0 (length string)
				'(keymap nil local-map nil)
				string))))

(mime-w3m-insinuate)

(provide 'mime-w3m)

;;; mime-w3m.el ends here

;;; w3m-bookmark.el --- Functions to operate bookmark file of w3m

;; Copyright (C) 2001, 2002, 2003 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: Shun-ichi GOTO     <gotoh@taiyo.co.jp>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: w3m, WWW, hypermedia

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;;; Commentary:

;; w3m-bookmark.el is the add-on program of emacs-w3m to operate
;; bookmark file.  For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/


;;; Code:

(eval-when-compile (require 'cl))
(require 'w3m-util)
(require 'w3m)

(defcustom w3m-bookmark-file
  (expand-file-name "bookmark.html" w3m-profile-directory)
  "Bookmark file of w3m."
  :group 'w3m
  :type '(file :size 0))

(defcustom w3m-bookmark-file-coding-system 'euc-japan
  "Coding system for a created bookmark file.
This option is used when a new bookmark file is created, or when an
existing bookmark file includes ASCII characters only.  If the coding
system which is used to encode your using bookmark file is different
from the value of this option, emacs-w3m does not change the encoding
of your bookmark file."
  :group 'w3m
  :type '(coding-system :size 0))

(defcustom w3m-bookmark-default-section
  nil
  "Default section to add new entry."
  :group 'w3m
  :type '(radio (const :tag "Not specified" nil)
		(string :format "Default section name: %v\n" :size 0)))

(defcustom w3m-bookmark-mode-hook nil
  "*Hook run at the end of function `w3m-bookmark-mode'."
  :group 'w3m
  :type 'hook)

(eval-and-compile
  (defconst w3m-bookmark-section-delimiter
    "<!--End of section (do not delete this comment)-->\n"))

(eval-and-compile
  (defconst w3m-bookmark-section-format
    (eval-when-compile
      (concat "<h2>%s</h2>\n<ul>\n"
	      "<li><a href=\"%s\">%s</a>\n"
	      w3m-bookmark-section-delimiter
	      "</ul>\n"))))

(defconst w3m-bookmark-initial-format
  (eval-when-compile
    (concat "<html><head><title>Bookmarks</title></head>\n"
	    "<body>\n<h1>Bookmarks</h1>\n"
	    w3m-bookmark-section-format
	    "</body>\n</html>\n")))

(defvar w3m-bookmark-section-history nil)
(defvar w3m-bookmark-title-history nil)

(defvar w3m-bookmark-buffer-file-name nil
  "Non-nil means that `w3m-bookmark-file' has been loaded to this buffer.")
(make-variable-buffer-local 'w3m-bookmark-buffer-file-name)

(defvar w3m-bookmark-mode-map
  (let ((map (make-sparse-keymap))
	(table '((kill-line . w3m-bookmark-kill-entry)
		 (undo . w3m-bookmark-undo))))
    (dolist (pair table)
      (substitute-key-definition (car pair) (cdr pair) map global-map))
    (substitute-key-definition 'w3m-edit-current-url 'w3m-bookmark-edit
			       map w3m-mode-map)
    map)
  "*Keymap for `w3m-bookmark-mode'.")

(defvar w3m-bookmark-mode nil "Non-nil if w3m bookmark mode is enabled.")
(make-variable-buffer-local 'w3m-bookmark-mode)
(unless (assq 'w3m-bookmark-mode minor-mode-alist)
  (push (list 'w3m-bookmark-mode " bookmark") minor-mode-alist))
(unless (assq 'w3m-bookmark-mode minor-mode-map-alist)
  (push (cons 'w3m-bookmark-mode w3m-bookmark-mode-map) minor-mode-map-alist))

(defun w3m-bookmark-mode (&optional arg)
  "\\<w3m-bookmark-mode-map>
Minor mode to edit bookmark.

\\[w3m-bookmark-kill-entry]	Kill the current entry of this bookmark.
\\[w3m-bookmark-undo]	Undo some previous changes on this bookmark.
\\[w3m-bookmark-edit]	Open `w3m-bookmark-file'.
"
  (interactive "P")
  (when (setq w3m-bookmark-mode
	      (if arg
		  (> (prefix-numeric-value arg) 0)
		(not w3m-bookmark-mode)))
    (run-hooks 'w3m-bookmark-mode-hook)))

(defun w3m-bookmark-mode-setter (url)
  "Activate `w3m-bookmark-mode', when visiting page shows bookmark."
  (w3m-bookmark-mode (if (string-match "\\`about://bookmark/" url)
			 (progn
			   (setq default-directory
				 (file-name-directory w3m-bookmark-file))
			   1)
		       0)))
(add-hook 'w3m-display-functions 'w3m-bookmark-mode-setter)

(defun w3m-bookmark-file-modtime ()
  "Return the bookmark file modification time.
The value is a list of the form (HIGH . LOW), like the time values
that `visited-file-modtime' returns.  When the bookmark file does not
exist, returns (0 . 0)."
  (if (file-exists-p w3m-bookmark-file)
      (let ((time (nth 5 (file-attributes w3m-bookmark-file))))
	(cons (car time) (cadr time)))
    (cons 0 0)))

(defun w3m-bookmark-buffer (&optional no-verify-modtime)
  "Return the buffer reading `w3m-bookmark-file' current."
  (let ((buffer (get-file-buffer w3m-bookmark-file)))
    (if buffer
	;; When a buffer visiting `w3m-bookmark-file' is found, return
	;; it instead of a working buffer.  In this case, kill the
	;; working buffer which was generated in the past.
	(progn (w3m-kill-buffer " *w3m bookmark*") buffer)
      ;; Generate a working buffer.
      (with-current-buffer (w3m-get-buffer-create " *w3m bookmark*")
	(unless (and w3m-bookmark-buffer-file-name
		     (or no-verify-modtime
			 (equal (visited-file-modtime)
				(w3m-bookmark-file-modtime))))
	  (when (file-readable-p w3m-bookmark-file)
	    (erase-buffer)
	    (let ((coding-system-for-read 'binary)
		  (file-coding-system-for-read 'binary))
	      (insert-file-contents w3m-bookmark-file))
	    (w3m-decode-buffer
	     (w3m-expand-file-name-as-url w3m-bookmark-file))
	    (set-buffer-file-coding-system
	     (if (memq w3m-current-coding-system
		       '(undecided undecided-dos undecided-mac undecided-unix))
		 w3m-bookmark-file-coding-system
	       w3m-current-coding-system))
	    (set-buffer-modified-p nil))
	  (setq w3m-bookmark-buffer-file-name w3m-bookmark-file)
	  (set-visited-file-modtime (w3m-bookmark-file-modtime))
	  (buffer-enable-undo))
	(current-buffer)))))

(defun w3m-bookmark-verify-modtime ()
  (unless (equal (visited-file-modtime)
		 (w3m-bookmark-file-modtime))
    (if (buffer-file-name)
	(ask-user-about-supersession-threat w3m-bookmark-file)
      (let ((modified (buffer-modified-p))
	    (name (buffer-name)))
	(unwind-protect
	    (progn
	      (set-visited-file-name w3m-bookmark-file)
	      (ask-user-about-supersession-threat w3m-bookmark-file))
	  (set-visited-file-name nil)
	  (rename-buffer name)
	  (set-buffer-modified-p modified))))))

(defun w3m-bookmark-sections ()
  "Return collection of registered sections."
  (let (sections)
    (save-excursion
      (set-buffer (w3m-bookmark-buffer))
      (goto-char (point-min))
      (while (search-forward "<h2>" nil t)
	(push (cons (buffer-substring-no-properties
		     (point)
		     (if (search-forward "</h2>" nil t)
			 (match-beginning 0)
		       (line-end-position)))
		    nil)
	      sections)))
    (nreverse sections)))

(defun w3m-bookmark-save-buffer ()
  "Save this current buffer to `w3m-bookmark-file'."
  (cond
   ((buffer-file-name)
    (basic-save-buffer))
   ((buffer-modified-p)
    (let ((backup-info (find-backup-file-name w3m-bookmark-file))
	  (modes (when (file-exists-p w3m-bookmark-file)
		   (file-modes w3m-bookmark-file))))
      (when (and modes ; means that `w3m-bookmark-file' exists.
		 make-backup-files
		 (funcall backup-enable-predicate w3m-bookmark-file))
	(rename-file w3m-bookmark-file (car backup-info) t))
      (write-region (point-min) (point-max) w3m-bookmark-file)
      (when modes
	(set-file-modes w3m-bookmark-file modes))
      (set-visited-file-modtime (w3m-bookmark-file-modtime))
      (set-buffer-modified-p nil)
      (dolist (file (cdr backup-info))
	(condition-case ()
	    (delete-file file)
	  (file-error nil)))))))

(defun w3m-bookmark-safe-string (string format)
  (labels ((filter (s c) (decode-coding-string (encode-coding-string s c) c)))
    (if (let ((encoding
	       (w3m-static-cond
		((boundp 'MULE) file-coding-system)
		((featurep 'mule) buffer-file-coding-system))))
	  (or (string= string (filter string encoding))
	      (when w3m-use-mule-ucs
		(string= (setq string
			       (filter string
				       (if w3m-accept-japanese-characters
					   'w3m-euc-japan
					 'w3m-iso-latin-1)))
			 (filter string encoding)))))
	string
      (error format string))))

(defun w3m-bookmark-write-file (url title section)
  "Make new bookmark with specified spec, and save it."
  (save-excursion
    (set-buffer (w3m-bookmark-buffer))
    (setq title (w3m-bookmark-safe-string
		 title
		 "Specified title includes unsafe character(s): %s")
	  section (w3m-bookmark-safe-string
		   section
		   "Specified section includes unsafe character(s): %s"))
    (if (zerop (buffer-size))
	;; New bookmark file.
	(progn
	  (insert (format w3m-bookmark-initial-format section url title))
	  (set-buffer-file-coding-system w3m-bookmark-file-coding-system))
      (goto-char (point-min))
      (if (search-forward (format "<h2>%s</h2>" section) nil t)
	  (progn
	    (unless (search-forward w3m-bookmark-section-delimiter nil t)
	      (error "Can't find section delimiter: %s" section))
	    (goto-char (match-beginning 0))
	    (insert (format "<li><a href=\"%s\">%s</a>\n" url title)))
	;; New section.
	(unless (search-forward "</body>\n" nil t)
	  (error "%s" "Can't find terminator of bookmark"))
	(goto-char (match-beginning 0))
	(insert (format w3m-bookmark-section-format
			section url title))))
    (w3m-bookmark-save-buffer)))

(defun w3m-bookmark-add (url &optional title)
  "Add URL to bookmark.
Optional argument TITLE is title of link."
  (let ((section (completing-read
		  (if w3m-bookmark-default-section
		      (format "Section (default %s): "
			      w3m-bookmark-default-section)
		    "Section: ")
		  (w3m-bookmark-sections) nil nil nil
		  'w3m-bookmark-section-history)))
    (and (string= section "")
	 (setq section w3m-bookmark-default-section))
    (when (or (not section)
	      (string-match section "^ *$"))
      (error "%s" "You must specify section name"))
    (setq title (read-string "Title: " title 'w3m-bookmark-title-history))
    (when (or (not title)
	      (string-match title "^ *$"))
      (error "%s" "You must specify title"))
    (w3m-bookmark-write-file url
			     (w3m-encode-specials-string title)
			     (w3m-encode-specials-string section))))

;;;###autoload
(defun w3m-bookmark-add-this-url ()
  "Add link under cursor to bookmark."
  (interactive)
  (if (null (w3m-anchor))
      (message "No anchor")		; nothing to do
    (let ((url (w3m-anchor))
	  (title (buffer-substring-no-properties
		  (previous-single-property-change (1+ (point)) 'w3m-href-anchor)
		  (next-single-property-change (point) 'w3m-href-anchor))))
      (w3m-bookmark-add url title))
    (message "Added")))

;;;###autoload
(defun w3m-bookmark-add-current-url (&optional arg)
  "Add link of current page to bookmark.
With prefix, ask new url to add instead of current page."
  (interactive "P")
  (w3m-bookmark-add (if arg (w3m-input-url) w3m-current-url)
		    w3m-current-title)
  (message "Added"))

;;;###autoload
(defun w3m-bookmark-add-current-url-group ()
  "Add link of the group of current urls to the bookmark."
  (interactive)
  (w3m-bookmark-add
   (concat "group:"
	   (mapconcat
	    'w3m-url-encode-string
	    (mapcar (lambda (buffer)
		      (with-current-buffer buffer w3m-current-url))
		    (w3m-list-buffers))
	    "&")))
  (message "Added as URL group"))

;;;###autoload
(defun w3m-bookmark-view (&optional reload)
  (interactive "P")
  (w3m-goto-url "about://bookmark/" reload))

;;;###autoload
(defun w3m-about-bookmark (&rest args)
  (insert-buffer (w3m-bookmark-buffer))
  (let ((ident) (i 0) (j 0))
    (goto-char (point-min))
    (while (search-forward (setq ident (format "w3mbk%d." i)) nil t)
      (incf i))
    (setq i 0)
    (goto-char (point-min))
    (while (re-search-forward "\n<\\(h2\\|\\(li\\)\\)>" nil t)
      (forward-char -1)
      (insert (if (match-beginning 2)
		  (format " id=\"%s%d.%d\"" ident i (incf j))
		(format " id=\"%s%d\"" ident (incf i))))))
  "text/html")

(defun w3m-bookmark-current-number ()
  "Return the ordinal number of the current bookmark entry."
  (let ((x (car (get-text-property (line-end-position) 'w3m-name-anchor))))
    (and x
	 (string-match "\\`w3mbk[0-9]+\\.[0-9]+\\.\\([0-9]+\\)\\'" x)
	 (string-to-number (match-string 1 x)))))

(defun w3m-bookmark-kill-entry (num)
  "Kill the bookmark entry of the current line.
With prefix argument, kill that many entries from point."
  (interactive "p")
  (let ((entries (w3m-bookmark-current-number)))
    (when entries
      (setq entries (list entries))
      (while (> (decf num) 0)
	(push (1+ (car entries)) entries))
      (condition-case nil
	  (w3m-bookmark-kill-entries entries)
	(file-supersession nil))
      (w3m-bookmark-view t))))

(defun w3m-bookmark-kill-entries (entries)
  (save-excursion
    (set-buffer (w3m-bookmark-buffer t))
    (w3m-bookmark-verify-modtime)
    (goto-char (point-min))
    (let ((i 0))
      (while (search-forward "\n<li>" nil t)
	(when (memq (incf i) entries)
	  (let ((beg (line-beginning-position))
		(end (progn
		       (search-forward w3m-bookmark-section-delimiter)
		       (match-beginning 0))))
	    (delete-region (goto-char beg)
			   (if (search-forward "\n<li>" end t)
			       (line-beginning-position)
			     end))
	    (goto-char (1- beg))))))
    (w3m-bookmark-save-buffer)))

(defun w3m-bookmark-undo (&optional arg)
  "Undo some previous changes on bookmark."
  (interactive "p")
  (condition-case nil
      (save-excursion
	(set-buffer (w3m-bookmark-buffer t))
	(w3m-bookmark-verify-modtime)
	(undo arg)
	(w3m-bookmark-save-buffer))
    (file-supersession nil))
  (w3m-bookmark-view t))

(defun w3m-bookmark-edit ()
  "Edit the bookmark file."
  (interactive)
  (w3m-edit-url (w3m-expand-file-name-as-url w3m-bookmark-file)))


(provide 'w3m-bookmark)

;;; w3m-bookmark.el ends here

;;; w3m-bookmark.el --- Functions to operate bookmark file of w3m

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

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

(require 'w3m-util)
(require 'w3m)

(defcustom w3m-bookmark-file
  (expand-file-name "bookmark.html" w3m-profile-directory)
  "Bookmark file of w3m."
  :group 'w3m
  :type 'file)

(defcustom w3m-bookmark-file-coding-system 'euc-japan
  "Coding system for bookmark file."
  :group 'w3m
  :type 'coding-system)

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

(defcustom w3m-bookmark-default-section
  nil
  "Default section to add new entry."
  :group 'w3m
  :type 'string)


;; internal variables
(defvar w3m-bookmark-section-history nil)
(defvar w3m-bookmark-title-history nil)


(defun w3m-bookmark-buffer (&optional no-verify-modtime)
  (with-current-buffer (w3m-get-buffer-create " *w3m bookmark*")
    (unless (and (buffer-file-name)
		 (or no-verify-modtime
		     (verify-visited-file-modtime (current-buffer))))
      (erase-buffer)
      (when (file-readable-p w3m-bookmark-file)
	(let ((file-coding-system-for-read w3m-bookmark-file-coding-system)
	      (coding-system-for-read w3m-bookmark-file-coding-system))
	  (insert-file-contents w3m-bookmark-file t)))
      (buffer-enable-undo (current-buffer)))
    (current-buffer)))

(defun w3m-bookmark-sections ()
  "Return collection of registered sections."
  (let (sections)
    (with-current-buffer (w3m-bookmark-buffer)
      (goto-char (point-min))
      (while (search-forward "<h2>" nil t)
	(push (cons (buffer-substring (point)
				      (and (search-forward "</h2>" nil t)
					   (match-beginning 0)))
		    nil)
	      sections)))
    (nreverse sections)))

(unless w3m-bookmark-default-section
  (setq w3m-bookmark-default-section (caar (w3m-bookmark-sections))))

(defun w3m-bookmark-write-file (url title section)
  "Make new bookmark with specified spec, and save it."
  (with-current-buffer (w3m-bookmark-buffer)
    (if (zerop (buffer-size))
	;; New bookmark file.
	(insert (format w3m-bookmark-initial-format
			section url title))
      (goto-char (point-min))
      (if (search-forward (format "<h2>%s</h2>" section) nil t)
	  (progn
	    (unless (search-forward w3m-bookmark-section-delimiter nil t)
	      (error "Can't find section delimiter: %s" section))
	    (goto-char (match-beginning 0))
	    (insert (format "<li><a href=\"%s\">%s</a>\n" url title)))
	;; New section.
	(unless (search-forward "</body>\n" nil t)
	  (error "%s" "Can't find delimiter of bookmark contents"))
	(goto-char (match-beginning 0))
	(insert (format w3m-bookmark-section-format
			section url title))))
    (basic-save-buffer)))

(defun w3m-bookmark-add (url &optional title)
  "Add URL to bookmark.
Optional argument TITLE is title of link."
  (let ((section (completing-read
		  (if w3m-bookmark-default-section
		      (format "Section (default %s): " w3m-bookmark-default-section)
		    "Section: ")
		  (w3m-bookmark-sections) nil nil nil
		  'w3m-bookmark-section-history)))
    (and (string= section "")
	 (setq section w3m-bookmark-default-section))
    (if (string-match section "^ *$")
	(error "%s" "You must specify section name"))
    (setq title (read-string "Title: " title 'w3m-bookmark-title-history))
    (if (string-match title "^ *$")
	(error "%s" "You must specify title"))
    (w3m-bookmark-write-file url title section)))

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
	    "&"))
   "")
  (message "Added as URL group"))

;;;###autoload
(defun w3m-bookmark-view (&optional reload)
  (interactive)
  (w3m-goto-url "about://bookmark/" reload))

;;;###autoload
(defun w3m-about-bookmark (&rest args)
  (insert-buffer (w3m-bookmark-buffer))
  (let ((ident) (i 0))
    (goto-char (point-min))
    (while (search-forward (setq ident (format "w3mbk%d." i)) nil t)
      (incf i))
    (setq i 0)
    (while (search-forward "\n<li>" nil t)
      (forward-char -1)
      (insert (format " id=\"%s%d\"" ident (incf i)))))
  "text/html")

(defun w3m-bookmark-current-number ()
  "Return the ordinal number of the current bookmark entry."
  (let ((pos (line-beginning-position))
	(eol (line-end-position))
	(val))
    (catch 'found
      (while (and (< pos eol)
		  (setq pos
			(next-single-property-change pos 'w3m-name-anchor
						     nil eol)))
	(and (setq val (car (get-text-property pos 'w3m-name-anchor)))
	     (string-match "\\`w3mbk[0-9]+\\.\\([0-9]+\\)\\'" val)
	     (throw 'found (string-to-number (match-string 1 val))))))))

(defun w3m-bookmark-kill-entry (num)
  "Kill the bookmark entry of the current line.
With prefix argument, kill that many entries from point."
  (interactive "p")
  (let ((entries (list (w3m-bookmark-current-number))))
    (while (> (decf num) 0)
      (push (1+ (car entries)) entries))
    (w3m-bookmark-kill-entries entries)
    (w3m-bookmark-view t)))

(defun w3m-bookmark-kill-entries (entries)
  (with-current-buffer (w3m-bookmark-buffer t)
    (unless (verify-visited-file-modtime (current-buffer))
      (ask-user-about-supersession-threat w3m-bookmark-file))
    (goto-char (point-min))
    (let ((i 0))
      (while (search-forward "\n<li>" nil t)
	(when (memq (incf i) entries)
	  (let* ((beg (line-beginning-position))
		 (end (if (search-forward "\n<li>" nil t)
			  (min
			   (line-beginning-position)
			   (and (goto-char beg)
				(search-forward w3m-bookmark-section-delimiter)
				(match-beginning 0)))
			(search-forward w3m-bookmark-section-delimiter))))
	    (delete-region beg end)
	    (goto-char (1- beg))))))
    (basic-save-buffer)))

(defun w3m-bookmark-undo (&optional arg)
  "Undo some previous changes on bookmark."
  (interactive "p")
  (with-current-buffer (w3m-bookmark-buffer)
    (undo-more arg)
    (basic-save-buffer))
  (w3m-bookmark-view t))

(defvar w3m-bookmark-mode-map
  (let ((map (make-sparse-keymap))
	(table '((kill-line . w3m-bookmark-kill-entry)
		 (undo . w3m-bookmark-undo))))
    (dolist (pair table)
      (substitute-key-definition (car pair) (cdr pair) map global-map))
    map)
  "*Keymap for `w3m-bookmark-mode'.")

(defcustom w3m-bookmark-mode-hook nil
  "*Hook run at the end of function `w3m-bookmark-mode'."
  :group 'w3m
  :type 'hook)

(defvar w3m-bookmark-mode nil "Non-nil if w3m bookmark mode is enabled.")
(make-variable-buffer-local 'w3m-bookmark-mode)
(unless (assq 'w3m-bookmark-mode minor-mode-map-alist)
  (push (cons 'w3m-bookmark-mode w3m-bookmark-mode-map) minor-mode-map-alist))

(defun w3m-bookmark-mode (&optional arg)
  "Minor mode to edit bookmark."
  (interactive)
  (prog1 (setq w3m-bookmark-mode
	       (if arg
		   (> (prefix-numeric-value arg) 0)
		 (not w3m-bookmark-mode)))
    (run-hooks 'w3m-bookmark-mode-hook)))

(defun w3m-bookmark-mode-setter (url)
  (w3m-bookmark-mode (if (equal "about://bookmark/" url) 1 0)))

(add-hook 'w3m-display-functions 'w3m-bookmark-mode-setter)


(provide 'w3m-bookmark)

;;; w3m-bookmark.el ends here

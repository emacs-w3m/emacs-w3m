;;; w3m-bookmark.el --- Functions to operate bookmark file of w3m.

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>

;; Authors: Shun-ichi GOTO     <gotoh@taiyo.co.jp>,
;;          TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;; Keywords: w3m, WWW, hypermedia

;; w3m-bookmark.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; w3m-bookmark.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with w3m.el; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


;;; Commentary:

;; w3m-bookmark.el is the add-on program to operate bookmark file.
;; For more detail about w3m.el, see:
;;
;;    http://namazu.org/~tsuchiya/emacs-w3m/


;;; Code:
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
  (caar (w3m-bookmark-sections))
  "Default section to add new entry."
  :group 'w3m
  :type 'string)


;; internal variables
(defvar w3m-bookmark-section-history nil)
(defvar w3m-bookmark-title-history nil)


(defsubst w3m-bookmark-read-file ()
  "Insert contents of bookmark file to this buffer."
  (when (file-readable-p w3m-bookmark-file)
    (let ((file-coding-system-for-read w3m-bookmark-file-coding-system)
	  (coding-system-for-read w3m-bookmark-file-coding-system))
      (insert-file-contents w3m-bookmark-file))))

(defun w3m-bookmark-sections ()
  "Return collection of registered sections."
  (let (sections)
    (with-temp-buffer
      (w3m-bookmark-read-file)
      (goto-char (point-min))
      (while (search-forward "<h2>" nil t)
	(push (cons (buffer-substring (point)
				      (and (search-forward "</h2>" nil t)
					   (match-beginning 0)))
		    nil)
	      sections)))
    (nreverse sections)))

(defun w3m-bookmark-write-file (url title section)
  "Make new bookmark with specified spec, and save it."
  (with-temp-buffer
    (w3m-bookmark-read-file)
    (if (zerop (buffer-size))
	;; New bookmark file.
	(insert (format w3m-bookmark-initial-format
			section url title))
      (if (search-forward (format "<h2>%s</h2>" section) nil t)
	  (progn
	    (unless (search-forward w3m-bookmark-section-delimiter nil t)
	      (error "Can't find section delimiter: %s" section))
	    (goto-char (match-beginning 0))
	    (insert (format "<li><a href=\"%s\">%s</a>\n" url title)))
	;; New section.
	(unless (search-forward "</body>\n" nil t)
	  (error "%s" "Can't find delimiter of bookmark contents."))
	(goto-char (match-beginning 0))
	(insert (format w3m-bookmark-section-format
			section url title))))
    (let ((file-coding-system w3m-bookmark-file-coding-system)
	  (coding-system-for-write w3m-bookmark-file-coding-system)
	  (mode (and (file-exists-p w3m-bookmark-file)
		     (file-modes w3m-bookmark-file))))
      ;; Make backup.
      (when (file-exists-p w3m-bookmark-file)
	(rename-file w3m-bookmark-file
		     (make-backup-file-name w3m-bookmark-file)
		     t))
      (write-region (point-min) (point-max) w3m-bookmark-file)
      (when mode (set-file-modes w3m-bookmark-file mode)))))

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
	(error "You must specify section name."))
    (setq title (read-string "Title: " title 'w3m-bookmark-title-history))
    (if (string-match title "^ *$")
	(error "You must specify title."))
    (w3m-bookmark-write-file url title section)))

(defun w3m-bookmark-add-this-url ()
  "Add link under cursor to bookmark."
  (interactive)
  (if (null (w3m-anchor))
      (message "No anchor.")		; nothing to do
    (let ((url (w3m-anchor))
	  (title (buffer-substring-no-properties
		  (previous-single-property-change (1+ (point)) 'w3m-href-anchor)
		  (next-single-property-change (point) 'w3m-href-anchor))))
      (w3m-bookmark-add url title))
    (message "Added.")))

(defun w3m-bookmark-add-current-url (&optional arg)
  "Add link of current page to bookmark.
With prefix, ask new url to add instead of current page."
  (interactive "P")
  (w3m-bookmark-add (if arg (w3m-input-url) w3m-current-url)
		    w3m-current-title)
  (message "Added."))

(defun w3m-bookmark-view ()
  (interactive)
  (if (file-readable-p w3m-bookmark-file)
      (w3m (w3m-expand-file-name-as-url w3m-bookmark-file))))


(provide 'w3m-bookmark)
;;; w3m-bookmark.el ends here.

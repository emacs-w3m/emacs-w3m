;;; -*- mode: Emacs-Lisp; coding: iso-2022-7bit -*-
;;; w3m-dtree.el --- The add-on program to display local directory tree.

;; Copyright (C) 2001  Free Software Foundation, Inc.

;; Author: Hideyuki SHIRAI    <shirai@meadowy.org>,
;;         TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;; Keywords:  w3m, WWW, hypermedia, directory, tree

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; w3m-dtree.el is The add-on program to display local directory tree.
;; For more detail about w3m.el, see:
;;
;;    http://namazu.org/~tsuchiya/emacs-w3m/

;;; Code:
(require 'w3m)

(defcustom w3m-dtree-default-allfiles nil
  "*If non-nil, set 'allfiles' to default."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-dtree-indent-strings ["|-" "+-" "|  " "   "]
  "*Vector of strings to be used for indentation with w3m-dtree.

If use default value or choice 'ASCII', display like this,
/home/shirai/work/emacs-w3m/
 |-CVS/
 |-icons/
 |  +-CVS/
 +-shimbun/
    +-CVS/

If choice 'Japanese', display like this,
/home/shirai/work/emacs-w3m/
 ├CVS/
 ├icons/
 │ └CVS/
 └shimbun/
 　 └CVS/

If you care for another style, set manually and try it :-).
"
  :group 'w3m
  :type '(choice
	  (const :tag "ASCII" ["|-" "+-" "|  " "   "])
	  (const :tag "Japanese" ["├" "└" "│ " "　 "])
	  (vector :tag "Ohters" string string string string)))

(defsubst w3m-dtree-expand-file-name (path)
  (if (string-match "^\\(.\\):\\(.*\\)" path)
      (if w3m-use-cygdrive
	  (concat "/cygdrive/"
		  (match-string 1 path) (match-string 2 path))
	(concat "/" (match-string 1 path) "|" (match-string 2 path)))
    path))

(defsubst w3m-dtree-directory-name (path)
  (when (string-match "^/\\(\\([A-Za-z]\\)[|:]?\\|cygdrive/\\([A-Za-z]\\)\\)/" path)
    (setq path (concat
		(or (match-string 2 path)
		    (match-string 3 path))
		":/"
		(substring path (match-end 0)))))
  path)

(defmacro w3m-dtree-has-child (path)
  (` (let ((w32-get-true-file-link-count t)) ;; true link count for Meadow
       (and (nth 1 (file-attributes (, path)))
	    (/= (nth 1 (file-attributes (, path))) 2)))))

(defun w3m-dtree-create-sub (path allfiles dirprefix fileprefix indent)
  (let ((files (directory-files path nil "[^.]"))
	(indent-sub1 (aref w3m-dtree-indent-strings 0))
	(indent-sub2 (aref w3m-dtree-indent-strings 2))
	file fullpath tmp)
    (unless allfiles
      (setq tmp files)
      (while (setq file (car tmp))
	(unless (file-directory-p (expand-file-name file path))
	  (setq files (delete file files)))
	(setq tmp (cdr tmp))))
    (while (setq file (car files))
      (when (= (length files) 1)
	(setq indent-sub1 (aref w3m-dtree-indent-strings 1))
	(setq indent-sub2 (aref w3m-dtree-indent-strings 3)))
      (setq fullpath (expand-file-name file path))
      (cond
       ((or (not allfiles) (file-directory-p fullpath))
	(insert (format "%s%s%s<A HREF=\"%s%s\">%s</A>\n"
			indent indent-sub1
			(if allfiles "<B>[d]</B>" "")
			dirprefix
			(w3m-dtree-expand-file-name (file-name-as-directory fullpath))
			(concat file "/")))
	(when (or allfiles (w3m-dtree-has-child fullpath))
	  (w3m-dtree-create-sub fullpath allfiles dirprefix fileprefix
				(concat indent indent-sub2))))
       ((and allfiles (file-exists-p fullpath))
	(insert (format "%s%s%s<A HREF=\"%s%s\">%s</A>\n"
			indent indent-sub1
			(if allfiles "(f)" "")
			fileprefix (w3m-dtree-expand-file-name fullpath)
			file))))
      (setq files (cdr files)))))

(defun w3m-dtree-create (path allfiles dirprefix fileprefix)
  (insert "<!doctype html public \"-//W3C//DTD HTML 3.2//EN\">\n"
	  "<html>\n<head>\n<title>"
	  path
	  "</title>\n</head>\n<body>\n<pre>\n")
  (insert (format "<A HREF=\"%s%s\">%s</A>%s\n"
		  dirprefix (w3m-dtree-expand-file-name path) path
		  (if allfiles " (allfiles)" "")))
  (if (file-directory-p path)
      (w3m-dtree-create-sub path allfiles dirprefix fileprefix " ")
    (insert (format "\n<h3>Warning: Directory not found.</h3>\n")))
  (insert "</pre>\n</body>\n</html>\n"))

(defun w3m-about-dtree (url &optional nodecode allfiles)
  (let ((prelen (length "about://dtree"))
	(dirprefix "about://dtree")
	(fileprefix "file://")
	path)
    (if (string-match "\\?allfiles=\\(\\(true\\)\\|false\\)$" url)
	(progn
	  (setq path (substring url prelen (match-beginning 0)))
	  (if (match-beginning 2) (setq allfiles t)))
      (if w3m-dtree-default-allfiles
	  (setq allfiles (not allfiles)))
      (setq path (substring url prelen)))
    ;; counter drive letter 
    (w3m-with-work-buffer
      (setq path (file-name-as-directory (w3m-dtree-directory-name path)))
      (setq default-directory path)
      (erase-buffer)
      (set-buffer-multibyte t)
      (w3m-message "Dtree (%s) ..." path)
      (w3m-dtree-create path allfiles dirprefix fileprefix)
      (w3m-message "Dtree ... done.")
      (encode-coding-region (point-min) (point-max) w3m-input-coding-system)
      "text/html")))

(defun w3m-dtree (allfiles path)
  "Display directory tree on local file system.
If called with 'prefix argument', display all directorys and files."
  (interactive "P\nDDtree directory: ")
  (if w3m-dtree-default-allfiles
      (setq allfiles (not allfiles)))
  (w3m-goto-url (format "about://dtree%s%s"
			(w3m-dtree-expand-file-name
			 (file-name-as-directory
			  (expand-file-name path)))
			(if allfiles "?allfiles=true" ""))))

(provide 'w3m-dtree)
;;; w3m-dtree.el ends here

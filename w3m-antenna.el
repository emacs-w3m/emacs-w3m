;;; w3m-antenna.el --- Utility to detect changes of WEB.

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;; Keywords: w3m, WWW, hypermedia

;; w3m-antenna.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; w3m-antenna.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with w3m.el; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


;;; Commentary:

;; w3m-antenna.el is the add-on utility to detect changes of WEB.  For
;; more detail about w3m.el, see:
;;
;;    http://namazu.org/~tsuchiya/emacs-w3m/


;;; How to install:

;; Please put this file to appropriate directory, and if you want
;; byte-compile it.  And add following lisp expressions to your
;; ~/.emacs.
;;
;;     (autoload 'w3m-antenna "w3m-antenna" "*Report chenge of WEB sites." t)


;;; Code:

(eval-when-compile (require 'cl))
(require 'w3m)

(defgroup w3m-antenna nil
  "w3m-antenna - Utility to detect changes of WEB."
  :group 'w3m
  :prefix "w3m-antenna-")

(defcustom w3m-antenna-sites
  nil
  "List of WEB sites, watched by `w3m-antenna'."
  :group 'w3m-antenna
  :type '(repeat
	  (list
	   (string :tag "URL")
	   (string :tag "Title")
	   (choice :tag "Class"
		   (const :tag "Normal" nil)
		   (const :tag "HNS" hns)))))

(defcustom w3m-antenna-html-skelton
  (eval-when-compile
    (concat "<!doctype html public \"-//W3C//DTD HTML 3.2//EN\">\n"
	    "<html>\n<head>\n<title>Antenna</title>\n</head>\n<body>\n"
	    "<h1>Antenna</h1>\n<h2>Updated</h2>\n<ul>\n%C</ul>\n"
	    "<h2>Visited</h2>\n<ul>\n%U</ul>\n"
	    "</body>\n</html>\n"))
  "HTML skelton of antenna."
  :group 'w3m-antenna
  :type 'string)

(defcustom w3m-antenna-make-summary-function
  'w3m-antenna-make-summary
  "Function to make summary of site information."
  :group 'w3m-antenna
  :type 'function)

(defcustom w3m-antenna-sort-changed-sites-function
  'w3m-antenna-sort-sites-by-time
  "Function to sort list of changed sites."
  :group 'w3m-antenna
  :type '(choice
	  (const :tag "Sort by last modification time." w3m-antenna-sort-sites-by-time)
	  (const :tag "Sort by title." w3m-antenna-sort-sites-by-title)
	  (const :tag "Do nothing." identity)
	  (function :tag "User function.")))

(defcustom w3m-antenna-sort-unchanged-sites-function
  'w3m-antenna-sort-sites-by-time
  "Function to sort list of unchanged sites."
  :group 'w3m-antenna
  :type '(choice
	  (const :tag "Sort by last modification time." w3m-antenna-sort-sites-by-time)
	  (const :tag "Sort by title." w3m-antenna-sort-sites-by-title)
	  (const :tag "Do nothing." identity)
	  (function :tag "User function.")))

(defcustom w3m-antenna-file
  (expand-file-name ".antenna" w3m-profile-directory)
  "File which has list of antenna URLs."
  :group 'w3m-antenna
  :type 'file)

(defcustom w3m-antenna-file-coding-system 'euc-japan
  "Coding system for antenna file."
  :group 'w3m-antenna
  :type 'coding-system)

(defvar w3m-antenna-alist nil
  "A list of site information.  nil means that antenna database is not
initialized.  Each site information is a list whose elements are:
 0. URL.
 1. Title.
 2. Class (Normal or HNS).
 3. Last modification time.
 4. Size in bytes.
 5. Size modification detected time.
")


(defun w3m-antenna-setup ()
  (unless w3m-antenna-alist
    (setq w3m-antenna-alist
	  (w3m-load-list w3m-antenna-file w3m-antenna-file-coding-system))
    (unless w3m-antenna-sites
      (dolist (elem w3m-antenna-alist)
	(push (list (car elem) (nth 1 elem) (nth 2 elem)) w3m-antenna-sites)))))

(defun w3m-antenna-shutdown ()
  (when w3m-antenna-alist
    (w3m-save-list w3m-antenna-file w3m-antenna-file-coding-system w3m-antenna-alist)
    (setq w3m-antenna-alist nil)))

(defun w3m-antenna-hns-last-modified (url &optional no-cache)
  (when (w3m-retrieve (w3m-expand-url "di.cgi" url) nil no-cache)
    (w3m-with-work-buffer
      (let (start)
	(or
	 ;; Process a line such as "Last-modified: 2001, 15 03   GMT<br>".
	 (and (progn
		(goto-char (point-min))
		(search-forward "\nLast-Modified: " nil t))
	      (setq start (match-end 0))
	      (search-forward "<br>" nil t)
	      (ignore-errors
		(apply (function encode-time)
		       (w3m-time-parse-string
			(buffer-substring start
					  (match-beginning 0))))))
	 ;; Process a line such as "newest day is 2001/03/15".
	 (and (progn
		(goto-char (point-min))
		(re-search-forward
		 "^newest day is \\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)$"
		 nil t))
	      (encode-time 0 0 0
			   (string-to-number (match-string 3))
			   (string-to-number (match-string 2))
			   (string-to-number (match-string 1))
			   "JST")))))))

(defun w3m-antenna-last-modified (url class &optional no-cache)
  (if (eq class 'hns)
      (w3m-antenna-hns-last-modified url no-cache)
    (w3m-last-modified url no-cache)))

(defun w3m-antenna-size (url class &optional no-cache)
  (unless (eq class 'hns)
    (or (w3m-content-length url no-cache)
	(progn
	  (w3m-retrieve url t no-cache)
	  (w3m-with-work-buffer
	    (buffer-size))))))

(defun w3m-antenna-make-summary (site)
  (format "<li><a href=\"%s\">%s</a> %s"
	  (car site)
	  (nth 1 site)
	  (cond
	   ((nth 3 site) (current-time-string (nth 3 site)))
	   ((nth 4 site) "Size")
	   (t ""))))

(defun w3m-antenna-sort-sites-by-time (sites)
  (sort sites (lambda (a b)
		(w3m-time-newer-p (or (nth 3 a) (nth 5 a))
				  (or (nth 3 b) (nth 5 b))))))

(defun w3m-antenna-sort-sites-by-title (sites)
  (sort sites (lambda (a b)
		(string< (nth 1 a) (nth 1 b)))))

(defun w3m-antenna-make-contents (changed-sites unchanged-sites)
  (insert w3m-antenna-html-skelton)
  (goto-char (point-min))
  (while (re-search-forward "%\\(.\\)" nil t)
    (let ((c (char-after (match-beginning 1))))
      (if (memq c '(?C ?U))
	  (save-restriction
	    (narrow-to-region (match-beginning 0) (match-end 0))
	    (delete-region (point-min) (point-max))
	    (goto-char (point-min))
	    (dolist (site (if (eq c ?C)
			      changed-sites
			    unchanged-sites))
	      (insert (funcall w3m-antenna-make-summary-function site)
		      "\n"))
	    (goto-char (point-max)))
	(delete-region (match-beginning 1) (match-end 1))))))

(defun w3m-about-antenna (url &optional no-decode no-cache)
  (unwind-protect
    (let (alist changed unchanged)
      (w3m-antenna-setup)
      ;; Check sites.
      (dolist (site w3m-antenna-sites)
	(let* ((url  (format-time-string (car site) (current-time)))
	       (time (w3m-antenna-last-modified url (nth 2 site) no-cache))
	       (size (w3m-antenna-size url (nth 2 site)))
	       (pre (assoc (car site) w3m-antenna-alist)))
	  (push (append site
			(list time
			      size
			      (and size
				   (nth 4 pre)
				   (if (/= size (nth 4 pre))
				       (current-time)
				     (nth 5 pre)))))
		alist)
	  (setq site (cons url (cdar alist)))
	  (if (w3m-time-newer-p (or time (nth 5 site))
				(w3m-arrived-last-modified url))
	      (push site changed)
	    (push site unchanged))))
      (setq w3m-antenna-alist alist)
      (w3m-with-work-buffer
	(delete-region (point-min) (point-max))
	(set-buffer-multibyte t)
	(w3m-antenna-make-contents
	 (funcall w3m-antenna-sort-changed-sites-function (nreverse changed))
	 (funcall w3m-antenna-sort-unchanged-sites-function (nreverse unchanged))))
      (w3m-antenna-shutdown)
      "text/html")
    (setq w3m-antenna-alist nil)))

(defun w3m-antenna ()
  "*Report chenge of WEB sites, which is specified in `w3m-antenna-sites'."
  (interactive)
  (w3m "about://antenna/"))


(provide 'w3m-antenna)
;;; w3m-antenna.el ends here.

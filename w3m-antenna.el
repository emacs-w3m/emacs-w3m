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
;;     (autoload 'w3m-antenna "w3m-antenna" "*Report change of WEB sites." t)


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

(defcustom w3m-antenna-html-skeleton
  (eval-when-compile
    (concat "<!doctype html public \"-//W3C//DTD HTML 3.2//EN\">\n"
	    "<html>\n<head>\n<title>Antenna</title>\n</head>\n<body>\n"
	    "<h1>Antenna</h1>\n<h2>Updated</h2>\n<ul>\n%C</ul>\n"
	    "<h2>Visited</h2>\n<ul>\n%U</ul>\n"
	    "</body>\n</html>\n"))
  "HTML skeleton of antenna."
  :group 'w3m-antenna
  :type 'string)

(defcustom w3m-antenna-make-summary-function
  'w3m-antenna-make-summary-like-natsumikan
  "Function to make summary of site information."
  :group 'w3m-antenna
  :type '(choice
	  (const :tag "Simple style." w3m-antenna-make-summary)
	  (const :tag "Natsumikan style." w3m-antenna-make-summary-like-natsumikan)
	  (function :tag "User function.")))

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
  "A list of site information (internal variable).  nil means that
antenna database is not initialised.  Each site information is a list
whose elements are:
 0. Format string of URL.
 1. Title.
 2. Class (Normal or HNS).
 3. Real URL.
 4. Last modification time.
 5. Size in bytes.
 6. Time when size modification is detected.
")

(defmacro w3m-antenna-site-key (site)
  (` (car (, site))))
(defmacro w3m-antenna-site-title (site)
  (` (nth 1 (, site))))
(defmacro w3m-antenna-site-class (site)
  (` (nth 2 (, site))))
(defmacro w3m-antenna-site-url (site)
  (` (nth 3 (, site))))
(defmacro w3m-antenna-site-last-modified (site)
  (` (nth 4 (, site))))
(defmacro w3m-antenna-site-size (site)
  (` (nth 5 (, site))))
(defmacro w3m-antenna-site-size-detected (site)
  (` (nth 6 (, site))))

(defun w3m-antenna-setup ()
  (unless w3m-antenna-alist
    (setq w3m-antenna-alist
	  (w3m-load-list w3m-antenna-file w3m-antenna-file-coding-system))
    ;; Convert old format of antenna database file, which is used
    ;; before revision 1.5.
    (and w3m-antenna-alist
	 (not (stringp (nth 3 (car w3m-antenna-alist))))
	 (setq w3m-antenna-alist
	       (mapcar
		(lambda (site)
		  (append (list (car site)
				(nth 1 site)
				(nth 2 site)
				(format-time-string
				 (car site)
				 (current-time)))
			  (nthcdr 3 site)))
		w3m-antenna-alist)))
    (unless w3m-antenna-sites
      (dolist (site w3m-antenna-alist)
	(push (list (w3m-antenna-site-key site)
		    (w3m-antenna-site-title site)
		    (w3m-antenna-site-class site))
	      w3m-antenna-sites)))))

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

(defun w3m-antenna-check-sites ()
  "Check all sites specified in `w3m-antenna-sites' and return the antenna database alist."
  (let (alist)
    (dolist (site w3m-antenna-sites)
      (let* ((url  (format-time-string (car site) (current-time)))
	     (time (w3m-antenna-last-modified
		    url (w3m-antenna-site-class site) t))
	     (size (w3m-antenna-size
		    url (w3m-antenna-site-class site)))
	     (pre (assoc (w3m-antenna-site-key site) w3m-antenna-alist)))
	(push (append site
		      (list url
			    time
			    size
			    (and size
				 (nth 5 pre)
				 (if (/= size (nth 5 pre))
				     (current-time)
				   (nth 6 pre)))))
	      alist)))
    alist))

(defun w3m-antenna-make-summary (site)
  (format "<li><a href=\"%s\">%s</a> %s"
	  (w3m-antenna-site-url site)
	  (w3m-antenna-site-title site)
	  (cond
	   ((w3m-antenna-site-last-modified site)
	    (current-time-string (w3m-antenna-site-last-modified site)))
	   ((w3m-antenna-site-size site) "Size")
	   (t ""))))

(defun w3m-antenna-make-summary-like-natsumikan (site)
  (let ((t1 (w3m-antenna-site-last-modified site))
	(t2 (w3m-antenna-site-size-detected site)))
    (format "<li>%20s&nbsp;&nbsp;(%s)&nbsp;&nbsp;<a href=\"%s\">%s</a>"
	    (if (or t1 t2)
		(format-time-string "%Y/%m/%d %R" (or t1 t2))
	      "----/--/-- --:--")
	    (cond
	     (t1 "T")
	     (t2 "S")
	     (t "?"))
	    (w3m-antenna-site-url site)
	    (w3m-antenna-site-title site))))

(defun w3m-antenna-sort-sites-by-time (sites)
  (sort sites
	(lambda (a b)
	  (w3m-time-newer-p
	   (or (w3m-antenna-site-last-modified a)
	       (w3m-antenna-site-size-detected a))
	   (or (w3m-antenna-site-last-modified b)
	       (w3m-antenna-site-size-detected b))))))

(defun w3m-antenna-sort-sites-by-title (sites)
  (sort sites
	(lambda (a b)
	  (string< (w3m-antenna-site-title a)
		   (w3m-antenna-site-title b)))))

(defun w3m-antenna-make-contents (changed-sites unchanged-sites)
  (insert w3m-antenna-html-skeleton)
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
    (let (changed unchanged)
      (w3m-antenna-setup)
      ;; Check sites.
      (if no-cache
	  (setq w3m-antenna-alist (w3m-antenna-check-sites)))
      (dolist (site w3m-antenna-alist)
	(if (w3m-time-newer-p (or (w3m-antenna-site-last-modified site)
				  (w3m-antenna-site-size-detected site))
			      (w3m-arrived-last-modified
			       (w3m-antenna-site-url site)))
	    (push site changed)
	  (push site unchanged)))
      (w3m-with-work-buffer
	(delete-region (point-min) (point-max))
	(set-buffer-multibyte t)
	(w3m-antenna-make-contents
	 (funcall w3m-antenna-sort-changed-sites-function (nreverse changed))
	 (funcall w3m-antenna-sort-unchanged-sites-function (nreverse unchanged))))
      (w3m-antenna-shutdown)
      "text/html")
    (setq w3m-antenna-alist nil)))

(defun w3m-antenna (&optional no-cache)
  "*Report change of WEB sites, which is specified in `w3m-antenna-sites'."
  (interactive "P")
  (w3m-goto-url "about://antenna/" no-cache))


(provide 'w3m-antenna)
;;; w3m-antenna.el ends here.

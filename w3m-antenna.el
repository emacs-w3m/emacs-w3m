;;; w3m-antenna.el --- Utility to detect changes of WEB

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
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

;; w3m-antenna.el is the add-on utility of emacs-w3m to detect changes
;; of WEB pages.  For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/


;;; How to install:

;; Please put this file to appropriate directory, and if you want
;; byte-compile it.  And add following lisp expressions to your
;; ~/.emacs.
;;
;;     (autoload 'w3m-antenna "w3m-antenna" "Report changes of WEB sites." t)


;;; Code:

(eval-when-compile (require 'cl))
(require 'w3m-util)
(require 'w3m)

(defgroup w3m-antenna nil
  "w3m-antenna - Utility to detect changes of WEB."
  :group 'w3m
  :prefix "w3m-antenna-")

(define-widget 'string-with-default 'string
  "String widget with default value.
When creating a new widget, its value is given by an expression specified
with :default-value-from."
  :tag "URL"
  :value-from nil
  :create 'string-with-default-value-create)

(defun string-with-default-value-create (widget)
  (if (string= "" (widget-get widget :value))
      ;; No value is given.
      (widget-put widget :value
		  (let* ((symbol (widget-get widget :value-from))
			 (value  (eval symbol)))
		    (if value
			(set symbol nil)
		      (setq value ""))
		    value)))
  (widget-default-create widget))

(defcustom w3m-antenna-sites
  nil
  "List of WEB sites, watched by `w3m-antenna'."
  :group 'w3m-antenna
  :type '(repeat
	  (list
	   (string-with-default :tag "URL"
				:value-from w3m-antenna-tmp-url)
	   (string-with-default :tag "Title"
				:value-from w3m-antenna-tmp-title)
	   (choice :tag "Class"
		   (const :tag "Normal" nil)
		   (const :tag "Modified Time" time)
		   (const :tag "HNS" hns)))))

(defcustom w3m-antenna-html-skeleton
  (eval-when-compile
    (concat "<!doctype html public \"-//W3C//DTD HTML 3.2//EN\">\n"
	    "<html>\n<head>\n<title>Antenna</title>\n</head>\n<body>\n"
	    "<h1>Antenna</h1>\n<h2>Updated</h2>\n<ul>\n%C</ul>\n"
	    "<h2>Visited</h2>\n<ul>\n%U</ul>\n"
	    "<p align=\"left\">[<a href=\"about://antenna-edit/\">Edit</a>]"
	    "</p>\n</body>\n</html>\n"))
  "HTML skeleton of antenna."
  :group 'w3m-antenna
  :type 'string)

(defcustom w3m-antenna-make-summary-function
  'w3m-antenna-make-summary-like-natsumican
  "Function to make summary of site information."
  :group 'w3m-antenna
  :type '(choice
	  (function-item :tag "Simple style." w3m-antenna-make-summary)
	  (function-item :tag "Natsumican style."
			 w3m-antenna-make-summary-like-natsumican)
	  (function :tag "User function.")))

(defcustom w3m-antenna-sort-changed-sites-function
  'w3m-antenna-sort-sites-by-time
  "Function to sort list of changed sites."
  :group 'w3m-antenna
  :type '(choice
	  (function-item :tag "Sort by last modification time."
			 w3m-antenna-sort-sites-by-time)
	  (function-item :tag "Sort by title." w3m-antenna-sort-sites-by-title)
	  (function-item :tag "Do nothing." identity)
	  (function :tag "User function.")))

(defcustom w3m-antenna-sort-unchanged-sites-function
  'w3m-antenna-sort-sites-by-time
  "Function to sort list of unchanged sites."
  :group 'w3m-antenna
  :type '(choice
	  (function-item :tag "Sort by last modification time."
			 w3m-antenna-sort-sites-by-time)
	  (function-item :tag "Sort by title." w3m-antenna-sort-sites-by-title)
	  (function-item :tag "Do nothing." identity)
	  (function :tag "User function.")))

(defcustom w3m-antenna-file
  (expand-file-name ".antenna" w3m-profile-directory)
  "File which has list of antenna URLs."
  :group 'w3m-antenna
  :type 'file)

(defvar w3m-antenna-alist nil
  "A list of site information (internal variable).  nil means that
antenna database is not initialised.  Each site information is a list
whose elements are:
 0. Format string of URL.
 1. Title.
 2. Class (Normal, HNS or TIME).
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
    (setq w3m-antenna-alist (w3m-load-list w3m-antenna-file))
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
    (w3m-save-list w3m-antenna-file w3m-antenna-alist nil t)
    (setq w3m-antenna-alist nil)))

(defun w3m-antenna-hns-last-modified (url no-cache handler)
  (w3m-process-do-with-temp-buffer
      (type (w3m-retrieve (w3m-expand-url "di.cgi" url)
			  nil no-cache nil nil handler))
    (when type
      (or
       (let (start str)
	 ;; Process a line such as "Tue, 27 Mar 2001 12:43:16 GMT<br>".
	 (goto-char (point-min))
	 (and
	  (search-forward "\nLast-Modified: " nil t)
	  (setq start (match-end 0))
	  (search-forward "<br>" nil t)
	  (setq str (buffer-substring start (match-beginning 0)))
	  ;; Ignore format such as "2001, 27 03 GMT", which is used
	  ;; by old HNS.
	  (not (string-match
		" *[0-9][0-9][0-9][0-9], +[0-9][0-9] +[0-9][0-9] +" str))
	  (w3m-time-parse-string str)))
       (progn
	 ;; Process a line such as "newest day is 2001/03/15".
	 (goto-char (point-min))
	 (and
	  (re-search-forward "\
^newest day is \\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)$"
			     nil t)
	  (encode-time 0 0 0
		       (string-to-number (match-string 3))
		       (string-to-number (match-string 2))
		       (string-to-number (match-string 1))
		       "JST")))))))

;; To avoid byte-compile warning.
(eval-and-compile
  (autoload 'w3m-filter "w3m-filter"))

(defun w3m-antenna-check-site (site no-cache handler)
  (lexical-let ((site site)
		(no-cache no-cache)
		(url (format-time-string (w3m-antenna-site-key site)
					 (current-time))))
    (if (eq 'hns (w3m-antenna-site-class site))
	(w3m-process-do
	    (time (w3m-antenna-hns-last-modified url no-cache handler))
	  (when time
	    (w3m-antenna-check-site-after site url time nil)))
      (w3m-process-do
	  (attr (w3m-attributes url no-cache handler))
	(when attr
	  (if (nth 4 attr) ; Use the value of Last-modified header.
	      (w3m-antenna-check-site-after site url (nth 4 attr) (nth 2 attr))
	    (unless (eq 'time (w3m-antenna-site-class site))
	      (cond
	       ((nth 2 attr) ; Use the value of Content-Length header.
		(w3m-antenna-check-site-after site url nil (nth 2 attr)))
	       (t ; Get the real content of the SITE, and calculate its size.
		(w3m-process-do-with-temp-buffer
		    (type (w3m-retrieve url nil no-cache nil nil handler))
		  (when type
		    (w3m-decode-buffer url nil type)
		    (w3m-remove-comments)
		    (when w3m-use-filter
		      (w3m-filter url))
		    (w3m-antenna-check-site-after site url nil
						  (buffer-size)))))))))))))

(defun w3m-antenna-check-site-after (site url time size)
  (let ((pre (assoc (w3m-antenna-site-key site) w3m-antenna-alist)))
    (if pre
	(progn
	  (setcdr pre
		  (append (cdr site)
			  (list url
				time
				size
				(and size
				     (w3m-antenna-site-size pre)
				     (if (/= size (w3m-antenna-site-size pre))
					 (current-time)
				       (or (w3m-antenna-site-size-detected pre)
					   (current-time)))))))
	  pre)
      (append site (list url time size nil)))))

(defun w3m-antenna-check-all-sites (&optional handler)
  "Check all sites specified in `w3m-antenna-sites'."
  (if (not handler)
      (w3m-process-with-wait-handler
	(w3m-antenna-check-all-sites handler))
    (let ((count (gensym "--antenna-count--"))
	  (buffer (gensym "--antenna-buffer--")))
      (set count 0)
      (set buffer (current-buffer))
      (let ((tmp)
	    (processes)
	    (handler `(lambda (x)
			(and (= 0 (setq ,count (1- ,count)))
			     (buffer-name ,buffer)
			     (with-current-buffer ,buffer
			       (funcall ,handler nil))))))
	(dolist (site w3m-antenna-sites)
	  (when (w3m-process-p
		 (setq tmp (w3m-antenna-check-site site t handler)))
	    (push tmp processes)
	    (set count (1+ (symbol-value count)))))
	(when processes
	  (w3m-process-start-queued-processes)
	  (car processes))))))

(defun w3m-antenna-make-summary (site)
  (format "<li><a href=\"%s\">%s</a> %s"
	  (w3m-antenna-site-url site)
	  (w3m-antenna-site-title site)
	  (cond
	   ((w3m-antenna-site-last-modified site)
	    (current-time-string (w3m-antenna-site-last-modified site)))
	   ((w3m-antenna-site-size site) "Size")
	   (t ""))))

(defun w3m-antenna-make-summary-like-natsumican (site)
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

;;;###autoload
(defun w3m-about-antenna (url &optional no-decode no-cache
			      post-data referer handler)
  (w3m-antenna-setup)
  (w3m-process-do
      (val (when no-cache
	     (w3m-antenna-check-all-sites handler)))
    (let (changed unchanged)
      (dolist (site w3m-antenna-alist)
	(if (w3m-time-newer-p (or (w3m-antenna-site-last-modified site)
				  (w3m-antenna-site-size-detected site))
			      (w3m-arrived-last-modified
			       (w3m-antenna-site-url site)))
	    (push site changed)
	  (push site unchanged)))
      (w3m-antenna-make-contents
       (funcall w3m-antenna-sort-changed-sites-function (nreverse changed))
       (funcall w3m-antenna-sort-unchanged-sites-function (nreverse unchanged)))
      (w3m-antenna-shutdown)
      "text/html")))

;;;###autoload
(defun w3m-antenna (&optional no-cache)
  "Report changes of WEB sites, which is specified in `w3m-antenna-sites'."
  (interactive "P")
  (w3m-goto-url "about://antenna/" no-cache))

(defvar w3m-antenna-tmp-url nil)
(defvar w3m-antenna-tmp-title nil)

(defun w3m-antenna-add-current-url (&optional arg)
  "Add link of current page to antenna.
With prefix, ask new url to add instead of current page."
  (interactive "P")
  (w3m-antenna-add (if arg (w3m-input-url) w3m-current-url)
		   w3m-current-title))

(defun w3m-antenna-add (url &optional title)
  "Add URL to antenna.
Optional argument TITLE is title of link."
  (setq w3m-antenna-tmp-url url)
  (setq w3m-antenna-tmp-title title)
  (customize-variable 'w3m-antenna-sites)
  ;; dirty...
  (re-search-forward "INS")
  (backward-char 1)
  (widget-button-press (point))
  (re-search-forward "State:")
  (backward-char 2))

(defun w3m-about-antenna-edit (url &optional no-decode no-cache post-data
				   &rest args)
  (let ((alist (w3m-load-list w3m-antenna-file))
	(width (- (if (< 0 w3m-fill-column)
		      w3m-fill-column
		    (+ (window-width) (or w3m-fill-column -1)))
		  28)))
    (when (and post-data
	       (string= "about://antenna-edit/"
			(with-current-buffer w3m-current-buffer
			  w3m-current-url)))
      (let (add delete id key title class unknown site)
	(dolist (pair (split-string post-data "&"))
	  (cond
	   ((string-match "\\`op=\\(OK\\|\\(NEW\\|\\(DEL\\)\\)\\)\\'" pair)
	    (or (setq delete (match-beginning 3))
		(setq add (match-beginning 2))))
	   ((string-match "\\`id=\\([0-9]+\\)\\'" pair)
	    (setq id (string-to-number (match-string 1 pair))))
	   ((string-match "\\`key=\\(.*\\)\\'" pair)
	    (setq key (w3m-url-decode-string (match-string 1 pair)
					     w3m-coding-system)))
	   ((string-match "\\`title=\\(.*\\)\\'" pair)
	    (setq title (w3m-url-decode-string (match-string 1 pair)
					       w3m-coding-system)))
	   ((string-match "\\`class=\\(nil\\|time\\|hns\\)\\'" pair)
	    (setq class (intern (match-string 1 pair))))
	   (t (setq unknown t))))
	(cond
	 (unknown)
	 (add
	  (and key
	       title
	       (push (list key
			   title
			   class
			   (format-time-string key (current-time))
			   nil		; last-modified
			   nil		; size
			   nil)		; size-detected
		     alist)
	       (w3m-save-list w3m-antenna-file alist nil t)))
	 (delete
	  (when (and (setq site (nth id alist))
		     (yes-or-no-p (format "Delete %s from Antenna ? "
					  (w3m-antenna-site-title site))))
	    (setq alist (delq site alist))
	    (w3m-save-list w3m-antenna-file alist nil t)))
	 ((setq site (nth id alist))	; Modify
	  (setf (w3m-antenna-site-key site) key)
	  (setf (w3m-antenna-site-title site) title)
	  (setf (w3m-antenna-site-class site) class)
	  (w3m-save-list w3m-antenna-file alist nil t)))))
    (insert "\
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html><head><title>w3m-antenna-sites editor</title></head><body><ul>
")
    (let ((id -1))
      (dolist (site alist)
	(insert
	 (format "\
<li><form method=\"post\" action=\"about://antenna-edit/\">
    <input type=\"hidden\" name=\"id\" value=\"%d\">
    <a href=\"%s\">%s</a>
    <table>
      <tr>
        <th align=\"left\">Key:</th>
        <td><input type=\"text\" name=\"key\" size=\"%d\" value=\"%s\"></td>
      </tr>
      <tr>
        <th>Title:</th>
        <td><input type=\"text\" name=\"title\" size=\"%d\" value=\"%s\"></td>
      </tr>
      <tr>
        <th>Class:</th>
        <td>
          <input type=\"radio\" name=\"class\" value=\"nil\"%s>Normal</input>
          <input type=\"radio\" name=\"class\" value=\"time\"%s>Time</input>
          <input type=\"radio\" name=\"class\" value=\"hns\"%s>HNS</input>
        </td>
      </tr>
      <tr>
        <th></th>
        <td>
          <input type=\"submit\" name=\"op\" value=\"OK\">
          <input type=\"submit\" name=\"op\" value=\"DEL\">
        </td>
      </tr>
    </table>
    </form>
    </li>
"
		 (incf id)
		 (or (w3m-antenna-site-url site)
		     (w3m-antenna-site-key site))
		 (w3m-antenna-site-title site)
		 width (w3m-antenna-site-key site)
		 width (w3m-antenna-site-title site)
		 (if (not (w3m-antenna-site-class site)) " checked" "")
		 (if (eq (w3m-antenna-site-class site) 'time) " checked" "")
		 (if (eq (w3m-antenna-site-class site) 'hns) " checked" "")))))
    (insert
     (format "\
<li><form method=\"post\" action=\"about://antenna-edit/\">
    New site
    <table>
      <tr>
        <th align=\"left\">Key:</th>
        <td><input type=\"text\" name=\"key\" size=\"%d\"></td>
      </tr>
      <tr>
        <th>Title:</th>
        <td><input type=\"text\" name=\"title\" size=\"%d\"></td>
      </tr>
      <tr>
        <th>Class:</th>
        <td>
          <input type=\"radio\" name=\"class\" value=\"nil\" checked>Normal</input>
          <input type=\"radio\" name=\"class\" value=\"time\">Time</input>
          <input type=\"radio\" name=\"class\" value=\"hns\">HNS</input>
        </td>
      </tr>
      <tr>
        <th></th>
        <td>
          <input type=\"submit\" name=\"op\" value=\"NEW\">
        </td>
      </tr>
    </table>
    </form>
    </li>
</ul></body></html>\n" width width))
    (add-hook 'w3m-display-functions 'w3m-antenna-edit-reset-post-data)
    "text/html"))

(defun w3m-antenna-edit-reset-post-data (url)
  (remove-hook 'w3m-display-functions 'w3m-antenna-edit-reset-post-data)
  (when (string= "about://antenna-edit/" url)
    (w3m-history-plist-put :post-data nil)))

(provide 'w3m-antenna)

;;; w3m-antenna.el ends here

;;; sb-security-memo.el --- shimbun backend for geocrawler.com.

;; Copyright (C) 2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: news

;; This file is a part of shimbun.

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

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-geocrawler (shimbun) ())

(defvar shimbun-geocrawler-url "http://www.geocrawler.com/archives/")

(defvar shimbun-geocrawler-group-alist
  '(("acpi-devel" "3/17428" nil nil)
    ("linux-kernel" "3/35" nil nil)
    ("linux-security" "3/92" nil nil)
    ("spamassasin-devel" "3/20914" nil nil))
  "Table of mailing lists which is archived by geocrawler.com.
Each element consists 4 entries:
    (0) name of a mailing list,
    (1) its identification string,
    (2) its address and
    (3) its x-face header.")

(defvar shimbun-geocrawler-content-start "<P>&nbsp;<P>")
(defvar shimbun-geocrawler-content-end "</BODY>")

(luna-define-method shimbun-groups ((shimbun shimbun-geocrawler))
  (mapcar 'car shimbun-geocrawler-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-geocrawler))
  (shimbun-expand-url
   (concat (nth 1 (assoc (shimbun-current-group-internal shimbun)
			 shimbun-geocrawler-group-alist))
	   "/")
   shimbun-geocrawler-url))

(luna-define-method shimbun-reply-to ((shimbun shimbun-geocrawler))
  (nth 2 (assoc (shimbun-current-group-internal shimbun)
		shimbun-geocrawler-group-alist)))

(luna-define-method shimbun-x-face ((shimbun shimbun-geocrawler))
  (nth 3 (assoc (shimbun-current-group-internal shimbun)
		shimbun-geocrawler-group-alist)))

(luna-define-method shimbun-headers ((shimbun shimbun-geocrawler)
				     &optional range)
  (shimbun-geocrawler-headers shimbun range))

(defun shimbun-geocrawler-headers (shimbun &optional range)
  (cond
   ((eq range 'last) (setq range 1))
   ((eq range 'all) (setq range nil)))
  (let ((url (shimbun-index-url shimbun))
	(years)
	(headers)
	(newest-index t)
	(case-fold-search t))
    (catch 'stop
      (with-temp-buffer
	(shimbun-retrieve-url url)
	(while (re-search-forward
		"<a href=\"\\([0-9][0-9][0-9][0-9]\\)\">" nil t)
	  (push (match-string 1) years))
	(dolist (year years)
	  (let ((url (concat url year "/"))
		(indexes))
	    (erase-buffer)
	    (shimbun-retrieve-url url)
	    (while (re-search-forward
		    "<a href=\"\\(\\([1-9]\\|1[012]\\)/0/\\)\">" nil t)
	      (push (match-string 1) indexes))
	    (dolist (index indexes)
	      (and range
		   (< (setq range (1- range)) 0)
		   (throw 'stop nil))
	      (let ((url (concat url index)))
		(while (when url
			 (erase-buffer)
			 (shimbun-retrieve-url url newest-index))
		  (setq newest-index nil)
		  (while (re-search-forward
			  "<a href=\"\\([0-9]+\\)/\"><img src=\"/img/msg.gif\"[^>]*> *&nbsp; *"
			  nil t)
		    (let ((xref
			   (concat
			    "http://www.geocrawler.com/mail/msg_raw.php3?msg_id="
			    (match-string 1)))
			  (id (concat "<" (match-string 1) "@geocrawler.com>"))
			  (eol (line-end-position)))
		      (when (shimbun-search-id shimbun id)
			(throw 'stop nil))
		      (push (shimbun-make-header
			     0
			     (shimbun-mime-encode-string
			      (buffer-substring
			       (point)
			       (progn
				 (search-forward "</td><td>" nil eol)
				 (match-beginning 0))))
			     (shimbun-mime-encode-string
			      (buffer-substring
			       (point)
			       (progn
				 (search-forward "</td><td>" nil eol)
				 (match-beginning 0))))
			     (when (looking-at "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)&nbsp;\\([0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\)</td>")
			       (shimbun-make-date-string
				(string-to-number (match-string 3))
				(string-to-number (match-string 1))
				(string-to-number (match-string 2))
				(match-string 4)))
			     id nil nil nil xref)
			    headers)))
		  (setq url
			(when (re-search-forward
			       "<a href=\"\\(../[0-9]+\\)\"><b>Next Results"
			       nil t)
			  (shimbun-expand-url (concat (match-string 1) "/")
					      url))))))))))
    headers))

(provide 'sb-geocrawler)

;;; sb-geocrawler.el ends here.

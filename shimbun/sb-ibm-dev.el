;;; sb-ibm-dev.el --- shimbun backend for www-6.ibm.com/ja/developerworks -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2003 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
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

(luna-define-class shimbun-ibm-dev (shimbun) ())

(defvar shimbun-ibm-dev-url "http://www-6.ibm.com/jp/developerworks/")
(defvar shimbun-ibm-dev-groups
  '("java" "linux" "opensource" "webservices" "xml"))
(defvar shimbun-ibm-dev-from-address "webmaster@www-6.ibm.com")
(defvar shimbun-ibm-dev-coding-system 'japanese-shift-jis-unix)
(defvar shimbun-ibm-dev-content-start "<!--[ 　]*Title[ 　]*-->")
(defvar shimbun-ibm-dev-content-end
  "^\\(<td colspan=\"[0-9]+\"><b>この記事についてどう思われますか？</b>\\|<!--footer information start-->\\)")

(luna-define-method shimbun-headers
  ((shimbun shimbun-ibm-dev) header &optional outbuf)
  (let* ((case-fold-search t)
	 (from (shimbun-from-address shimbun))
	 (group (shimbun-current-group-internal shimbun))
	 (baseurl (concat (shimbun-url-internal shimbun) group "/"))
	 aux headers id url subject date datelist)
    (catch 'stop
      (with-temp-buffer
	(shimbun-retrieve-url
	 (shimbun-expand-url "library.html" baseurl)
	 'reload)
	(subst-char-in-region (point-min) (point-max) ?\t ?\  t)
	(goto-char (point-min))
	(while (re-search-forward
		;; getting URL and SUBJECT
		"<td><a href=\"\\(.*\\.html\\)\">\\(.*\\)</a>"
		nil t)
	  (setq url (match-string 1)
		subject (match-string 2))
	  ;; adjusting URL
	  (setq url (shimbun-expand-url url baseurl))
	  ;; getting DATE
	  (if (re-search-forward
	       "(\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)) *<br */?>"
	       nil t)
	      (setq datelist (list (string-to-number (match-string 1))
				   (string-to-number (match-string 2))
				   (string-to-number (match-string 3)))
		    date (apply 'shimbun-make-date-string datelist)))
	  ;; building ID
	  (setq aux (if (string-match "\\([^/]+\\)\\.html" url)
			(match-string 1 url)
		      url))
	  (setq id (format "<%s%%%02d%02d%02d%%%s@www-6.ibm.com>" aux
			   (car datelist) (car (cdr datelist))
			   (car (cdr (cdr datelist)))
			   group))
	  (if (shimbun-search-id shimbun id)
	      (throw 'stop nil))
	  (push (shimbun-make-header
		 0 (shimbun-mime-encode-string subject)
		 from date id "" 0 0 url)
		headers)
	  (forward-line 1))))
    headers))

(luna-define-method shimbun-article ((shimbun shimbun-ibm-dev)
				     header &optional outbuf)
  (when (shimbun-current-group-internal shimbun)
    (with-current-buffer (or outbuf (current-buffer))
      (insert
       (or (with-temp-buffer
	     (shimbun-retrieve-url (shimbun-article-url shimbun header))
	     (message "shimbun: Make contents...")
	     (goto-char (point-min))
	     (if (re-search-forward
		  "<meta http-equiv=\"refresh\" +content=\"0;URL=\\(.+\\)\">"
		  nil t)
		 (let ((url (match-string 1)))
		   (message "shimbun: Redirecting...")
		   ;;(shimbun-set-url-internal shimbun url)
		   (erase-buffer)
		   (shimbun-retrieve-url url 'no-cache)
		   (message "shimbun: Redirecting...done")
		   (message "shimbun: Make contents...")))
	     (goto-char (point-min))
	     (prog1
		 (shimbun-make-contents shimbun header)
	       (message "shimbun: Make contents...done")))
	   "")))))

(luna-define-method shimbun-make-contents :before
  ((shimbun shimbun-ibm-dev) header)
  (save-excursion
    (catch 'stop
      (subst-char-in-region (point-min) (point-max) ?\t ?\  t)
      ;; cleaning up
      (let (beg end pdflink url label)
	(while (re-search-forward "<!--[ 　]*PDF Mail[ 　]*-->" nil t)
	  (setq beg (progn
		      (beginning-of-line)
		      (point))
		end (progn
		      (search-forward "</noscript>" nil t)
		      (point)))
	  (goto-char beg)
	  (unless pdflink
	    (when (re-search-forward "\
<a href=\"\\(.*\\.pdf\\)\">.+alt=\"\\(PDF *- *[0-9]+[A-Z]+\\)\".*>"
				     end t)
	      (setq url (match-string 1)
		    label (match-string 2)
		    pdflink (format "<a href=\"%s\">この記事の%s</a>"
				    (shimbun-expand-url
				     url
				     (shimbun-article-url shimbun header))
				    label))))
	  (delete-region beg end))
	(goto-char (point-min))
	;; Remove sidebar if exist
	(when (and (re-search-forward "<!--[ 　]*Contents[ 　]*-->" nil t)
		   (re-search-forward "<!--[ 　]*Sidebar Gutter[ 　]*-->" nil t))
	  (let ((sidebar-start (search-backward "<table")))
	    (if (re-search-forward "<!--[ 　]*Start TOC[ 　]*-->" nil t)
		(delete-region (point)
			       (search-forward "</table>" nil t)))
	    (delete-region sidebar-start
			   (search-forward "</table>" nil t))))
	(when pdflink
	  (goto-char (point-max))
	  (insert pdflink)))
      (goto-char (point-min))
      ;; getting SUBJECT field infomation
      (when (re-search-forward "<h1>\\(.*\\)</h1>" nil t)
	(let ((subject (match-string 1)))
	  (shimbun-header-set-subject
	   header
	   (shimbun-mime-encode-string
	    (mapconcat 'identity
		       (split-string subject "</?\\(font\\|span\\)[^>]*>")
		       "")))))
      ;; getting FROM field information
      (let (author address)
	(if (re-search-forward "\
<a href=\"#author.*\">\\(.*\\)</a> (<a href=\"mailto:\\(.*\\)\">\\2</a>) *\n*"
			       nil t)
	    (progn
	      (setq author (match-string 1)
		    address (match-string 2))
	      (delete-region (match-beginning 0) (match-end 0)))
	  (when (re-search-forward "<a href=\"#author.*\">\\(.+\\)</a>" nil t)
	    (setq author (match-string 1))
	    (goto-char (point-min)))
	  (when (re-search-forward "<a href=\"mailto:\\(.+\\)\">\\1</a>" nil t)
	    (setq address (match-string 1))))
	(when address
	  (shimbun-header-set-from
	   header (shimbun-mime-encode-string
		   (if author
		       (format "%s <%s>" author address)
		     address))))))))

(provide 'sb-ibm-dev)

;;; sb-ibm-dev.el ends here

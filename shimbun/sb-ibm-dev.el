;;; sb-ibm-dev.el --- shimbun backend for www-6.ibm.com/ja/developerworks -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2003, 2005 NAKAJIMA Mikio <minakaji@namazu.org>

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

(eval-when-compile
  (require 'cl))

(require 'shimbun)

(luna-define-class shimbun-ibm-dev (shimbun) ())

(defvar shimbun-ibm-dev-url "http://www-6.ibm.com/jp/developerworks/")
(defvar shimbun-ibm-dev-groups
  '("autonomic" "java" "linux" "opensource" "webservices" "xml"))
(defvar shimbun-ibm-dev-from-address "webmaster@www-6.ibm.com")
(defvar shimbun-ibm-dev-coding-system 'japanese-shift-jis-unix)
(defvar shimbun-ibm-dev-content-start "<!--[ 　]*Contents[ 　]*-->")
(defvar shimbun-ibm-dev-content-end
  "<!--[ 　]*\\(\\(//\\|End[ 　]+of[ 　]\\)[ 　]*Contents\\|PDF Mail\\)[ 　]*-->")

(luna-define-method shimbun-get-headers ((shimbun shimbun-ibm-dev)
					 &optional range)
  (with-temp-buffer
    (let* ((case-fold-search t)
	   (from (shimbun-from-address shimbun))
	   (pages (shimbun-header-index-pages range))
	   (group (shimbun-current-group-internal shimbun))
	   (baseurl (concat (shimbun-url-internal shimbun) group "/"))
	   (count 0)
	   aux headers id url subject date datelist indexes index)
      (setq index (shimbun-expand-url "library.html" baseurl))
      (shimbun-retrieve-url index 'reload)
      (subst-char-in-region (point-min) (point-max) ?\t ?\  t)
      (goto-char (point-min))
      (push index indexes) ;; push latest
      ;; check old lib
      (while (re-search-forward "<a +href=\".*\\(library.+\\.html\\)\"" nil t)
	(push (shimbun-expand-url (match-string-no-properties 1) baseurl)
	      indexes))
      (setq indexes (nreverse indexes))
      (catch 'stop
	(while (or (not pages)
		   (<= (incf count) pages))
	  (setq index (pop indexes))
	  (unless index
	    (throw 'stop nil))
	  (erase-buffer)
	  (shimbun-retrieve-url index 'reload)
	  (subst-char-in-region (point-min) (point-max) ?\t ?\  t)
	  (goto-char (point-min))
	  (while (re-search-forward
		  ;; getting URL, SUBJECT and DATE
		  "<td><a href=\"\\(.*\\.html\\)\"\\([^>]+\\)?>\\(.*\\)</a> (\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)) *<br */?>"
		  nil t)
	    (setq url (match-string 1)
		  subject (match-string 3)
		  datelist (list (string-to-number (match-string 4))
				 (string-to-number (match-string 5))
				 (string-to-number (match-string 6)))
		  date (apply 'shimbun-make-date-string datelist))
	    ;; remove <b>...</b>(bold tag) if exist in subject
	    (let ((start 0))
	      (while (string-match "</?b>" subject start)
		(setq subject (replace-match "" nil nil subject)
		      start (match-beginning 0))))
	    ;; adjusting URL
	    (setq url (shimbun-expand-url url baseurl))
	    ;; building ID
	    (setq aux (if (string-match "\\([^/]+\\)\\.html" url)
			  (match-string 1 url)
			url))
	    (setq id (format "<%s%%%02d%02d%02d%%%s@www-6.ibm.com>" aux
			     (car datelist) (car (cdr datelist))
			     (car (cdr (cdr datelist)))
			     group))
	    (when (shimbun-search-id shimbun id)
	      (throw 'stop nil))
	    (push (shimbun-make-header
		   0 (shimbun-mime-encode-string subject)
		   from date id "" 0 0 url)
		  headers)
	    (forward-line 1))))
      headers)))

(luna-define-method shimbun-article-url ((shimbun shimbun-ibm-dev) header)
  (with-temp-buffer
    (let ((url (shimbun-article-base-url shimbun header)))
      (shimbun-fetch-url shimbun url)
      (if (re-search-forward "\
<meta http-equiv=\"refresh\" +content=\"0;URL=\\(.+\\)\" */?>" nil t)
	  (match-string 1)
	url))))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-ibm-dev)
						    header)
  (when (luna-call-next-method)
    (goto-char (point-min))
    (when (re-search-forward "<p><a href=\"[^\"]*#author1\">" nil t)
      (search-backward "<h2>" nil t)
      (delete-region (point-min) (match-beginning 0)))
    t))

(provide 'sb-ibm-dev)

;;; sb-ibm-dev.el ends here

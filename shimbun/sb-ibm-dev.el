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
;; Inc.; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)

(luna-define-class shimbun-ibm-dev (shimbun) ())

(defvar shimbun-ibm-dev-url "http://www-6.ibm.com/jp/developerworks/")
(defvar shimbun-ibm-dev-groups
  '("autonomic" "java" "linux" "opensource" "webservices" "xml"))
(defvar shimbun-ibm-dev-coding-system 'japanese-shift-jis-unix)
(defvar shimbun-ibm-dev-content-start "<!--[ 　]*Contents[ 　]*-->")
(defvar shimbun-ibm-dev-content-end
  "\\(<form name=\"DW_TEST\" action=\"/cgi-bin/jp/feedback\\.pl\" method=\"post\">\\|\
<!--[ 　]*\\(\\(//\\|End[ 　]+of[ 　]\\)[ 　]*Contents\\|PDF Mail\\)[ 　]*-->\\)")

(luna-define-method shimbun-index-url ((shimbun shimbun-ibm-dev))
  (shimbun-expand-url (concat (shimbun-current-group shimbun) "/library.html")
		      (shimbun-url-internal shimbun)))

(luna-define-method shimbun-from-address ((shimbun shimbun-ibm-dev))
  (concat "IBM developerWorks (" (shimbun-current-group shimbun) ")"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-ibm-dev)
					 &optional range)
  (catch 'stop
    (let ((case-fold-search t)
	  (base (shimbun-index-url shimbun))
	  (indexes)
	  (headers))
      (let ((pages (shimbun-header-index-pages range)))
	(goto-char (point-min))
	(while (when (or (not pages)
			 (< (length indexes) (1- pages)))
		 (re-search-forward "<a +class=\"[^\"]+\" +\
href=\"\\(library[0-9]*\\.s?html\\)\">\\([0-9]+\\)年</a>" nil t))
	  (push (cons (string-to-number (match-string 2))
		      (shimbun-expand-url (match-string 1) base))
		indexes)))
      (dolist (pair (nreverse indexes))
	(let ((year (car pair))
	      (index (cdr pair)))
	  (unless (string= index base)
	    (erase-buffer)
	    (shimbun-fetch-url shimbun index))
	  (goto-char (point-min))
	  (while (re-search-forward ">\\([0-9]+\\)月\\([0-9]+\\)日</th>
<td[^>]*><a href=\"\\([^\"]+\\)\" class=\"fbox\">\\(.*\\)</a>" nil t)
	    (let* ((url (shimbun-expand-url (match-string 3) index))
		   (id (concat "<" (md5 url)
			       "%" (shimbun-current-group shimbun)
			       "@www-6.ibm.com>")))
	      (when (shimbun-search-id shimbun id)
		(throw 'stop headers))
	      (push (shimbun-create-header nil
					   (match-string 4)
					   (shimbun-from-address shimbun)
					   (shimbun-make-date-string
					    year
					    (string-to-number (match-string 1))
					    (string-to-number (match-string 2)))
					   id "" 0 0 url)
		    headers)))))
      headers)))

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

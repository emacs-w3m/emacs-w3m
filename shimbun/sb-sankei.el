;;; sb-sankei.el --- shimbun backend for the Sankei Shimbun -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003, 2004, 2005, 2006 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
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

(eval-when-compile (require 'cl))

(require 'shimbun)

(luna-define-class shimbun-sankei (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-sankei-top-level-domain "sankei.co.jp"
  "Name of the top level domain for the Sankei shimbun.")

(defvar shimbun-sankei-url
  (concat "http://www." shimbun-sankei-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-sankei-server-name "産経新聞")

(defvar shimbun-sankei-from-address "nobody@example.com")

(defvar shimbun-sankei-content-start
  "<!--[\t\n ]*\\(?:photo\\.sta\\|\\(?:ad--\\)?hombun\\)[\t\n ]*-->[\t\n ]*")

(defvar shimbun-sankei-content-end
  "[\t\n ]*<!--[\t\n ]*hbnend[\t\n ]*-->\\|<!--[\t\n ]*KIJI END[\t\n ]*-+>")

(defvar shimbun-sankei-group-table
  '(("shakai" "社会" "news/shakai.htm")
    ("sports" "スポーツ" "news/sports.htm")
    ("keizai" "経済" "news/keizai.htm")
    ("seiji" "政治" "news/seiji.htm")
    ("kokusai" "国際" "news/kokusai.htm")
    ("bungei" "文化・芸能" "news/bungei.htm")
    ("book" "読書" "news/book.htm")
    ("person" "ひと" "news/person.htm")
    ("dead" "おくやみ" "news/dead.htm")
    ("editoria" "主張" "news/editoria.htm")
    ("column" "産経抄" "news/column.htm")
    ("seiron" "正論" "news/seiron.htm")))

(defvar shimbun-sankei-x-face-alist
  '(("default" . "X-Face: L%Y'jtF~2k?#oXVB%?3t/WE),tU/UwT%tl-omu#\
6TiJ]WFcLa>}caCBN(M\\vvoi,&hpVy?p\n T{\"&&d2,fOsFk+j2}\\q6eSg5b$2\
\"w'f{${CR\"PDL/E:Jo32\"T,k)!t666xNSH]?~M8gco<a$wp-$d;\n z#a%y#c}|")))

(defvar shimbun-sankei-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-sankei))
  (mapcar 'car shimbun-sankei-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-sankei))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-sankei-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-sankei))
  (shimbun-expand-url (nth 2 (assoc (shimbun-current-group-internal shimbun)
				    shimbun-sankei-group-table))
		      (shimbun-url-internal shimbun)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-sankei)
					 &optional range)
  (cond
   ((string-equal (shimbun-current-group-internal shimbun) "editoria")
    (shimbun-sankei-get-headers-editoria shimbun range))
   ((string-equal (shimbun-current-group-internal shimbun) "column")
    (shimbun-sankei-get-headers-column shimbun range))
   ((string-equal (shimbun-current-group-internal shimbun) "seiron")
    (shimbun-sankei-get-headers-seiron shimbun range))
   (t
    (when (re-search-forward "<!--[\t\n ]*mlist[\t\n ]*-->[\t\n ]*" nil t)
      (delete-region (point-min) (point)))
    (when (re-search-forward "[\t\n ]*<!--" nil t)
      (delete-region (match-beginning 0) (point-max)))
    (goto-char (point-min))
    (let ((from (concat (shimbun-server-name shimbun)
			" (" (shimbun-current-group-name shimbun) ")"))
	  (group (shimbun-current-group-internal shimbun))
	  (case-fold-search t)
	  cyear cmonth month year headers)
      (setq cyear (shimbun-decode-time nil 32400)
	    cmonth (nth 4 cyear)
	    cyear (nth 5 cyear))
      (while (re-search-forward
	      ;; 1:url 2:serial 3:subject 4:month 5:day 6:time
	      "<a[\t\n ]+href=\"\
\\(\
\\([/0-9a-z]+\\)\
\\.htm\\)\
\">[\t\n ]*\
\\([^\n<>]+\\)\
\[\t\n ]*</a>\\(?:[\t\n ]*<[^<>]+>\\)*[\t\n ]*(\
\\([01][0-9]\\)\
/\
\\([0-3][0-9]\\)\
\[\t\n ]+\
\\([012][0-9]:[0-5][0-9]\\)\
)"
	      nil t)
	(setq month (string-to-number (match-string 4))
	      year (cond ((>= (- month cmonth) 2)
			  (1- cyear))
			 ((and (= 1 month) (= 12 cmonth))
			  (1+ cyear))
			 (t
			  cyear)))
	(push (shimbun-create-header
	       ;; number
	       0
	       ;; subject
	       (match-string 3)
	       ;; from
	       from
	       ;; date
	       (shimbun-make-date-string year month
					 (string-to-number (match-string 5))
					 (match-string 6))
	       ;; id
	       (concat "<"
		       (mapconcat 'identity
				  (save-match-data
				    (split-string (match-string 2) "/"))
				  ".")
		       "%" group "."
		       shimbun-sankei-top-level-domain ">")
	       ;; references, chars, lines
	       "" 0 0
	       ;; xref
	       (concat shimbun-sankei-url "news/" (match-string 1)))
	      headers))
      headers))))

(defun shimbun-sankei-get-headers-editoria (shimbun range)
  (let ((from (concat (shimbun-server-name shimbun)
		      " (" (shimbun-current-group-name shimbun) ")"))
	(group (shimbun-current-group-internal shimbun))
	url urls year serial month day headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat
		 "<a" s1 "href=\""
		 ;; 1. url
		 "\\("
		 ;; 2. year
		 "\\(\\(?:20\\)?[0-9][0-9]\\)"
		 "[01][0-9][0-3][0-9]/\\(?:edi"
		 ;; 3. serial
		 "\\([0-9]+\\)"
		 "\\|morning/editoria\\)\\.html?\\)"
		 "\"" s0 ">" s0 "【主張】" s0
		 ;; 4. subject
		 "\\([^<]+\\)"
		 "\\(?:" s0 "<[^>]+>\\)*" s0 "(" s0
		 ;; 5. month
		 "\\([01]?[0-9]\\)"
		 "/"
		 ;; 6. day
		 "\\([0-3]?[0-9]\\)"
		 s1
		 ;; 7. time
		 "\\([012]?[0-9]:[0-5]?[0-9]\\(?:[0-5]?[0-9]\\)?\\)"
		 s0 ")")))
	    nil t)
      (unless (member (setq url (match-string 1)) urls)
	(push url urls)
	(when (< (setq year (string-to-number (match-string 2))) 100)
	  (setq year (+ year 2000)))
	(setq serial (match-string 3)
	      month (string-to-number (match-string 5))
	      day (string-to-number (match-string 6)))
	(push (shimbun-create-header
	       0
	       (match-string 4)
	       from
	       (shimbun-make-date-string year month day (match-string 7))
	       (format "<%04d%02d%02d%s%%%s.%s>"
		       year month day
		       (if serial
			   (concat "." serial)
			 "")
		       group shimbun-sankei-top-level-domain)
	       "" 0 0
	       (concat shimbun-sankei-url "news/" url))
	      headers)))
    headers))

(defun shimbun-sankei-get-headers-column (shimbun range)
  (let* ((name (shimbun-current-group-name shimbun))
	 (from (concat (shimbun-server-name shimbun) " (" name ")"))
	 (group (shimbun-current-group-internal shimbun))
	 url year serial month day headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat
		 "<a" s1 "href=\"\\(?:\\./\\)?"
		 ;; 1. url
		 "\\("
		 ;; 2. year
		 "\\(\\(?:20\\)?[0-9][0-9]\\)"
		 "[01][0-9][0-3][0-9]/\\(?:col"
		 ;; 3. serial
		 "\\([0-9]+\\)"
		 "\\|morning/column\\).html?\\)"
		 "\"" s0 ">" s0 "【産経抄】\\(?:" s0 "<[^>]+>\\)*" s0 "(" s0
		 ;; 4. month
		 "\\([01]?[0-9]\\)"
		 "/"
		 ;; 5. day
		 "\\([0-3]?[0-9]\\)"
		 s1
		 ;; 6. time
		 "\\([012]?[0-9]:[0-5]?[0-9]\\(?:[0-5]?[0-9]\\)?\\)"
		 s0 ")")))
	    nil t)
      (setq url (concat shimbun-sankei-url "news/" (match-string 1)))
      (when (< (setq year (string-to-number (match-string 2))) 100)
	(setq year (+ year 2000)))
      (setq serial (match-string 3)
	    month (string-to-number (match-string 4))
	    day (string-to-number (match-string 5)))
      (push (shimbun-create-header
	     0
	     (format "%s (%d/%d)" name month day)
	     from
	     (shimbun-make-date-string year month day (match-string 6))
	     (format "<%04d%02d%02d%s%%%s.%s>"
		     year month day
		     (if serial
			 (concat "." serial)
		       "")
		     group shimbun-sankei-top-level-domain)
	     "" 0 0 url)
	    headers))
    headers))

(defun shimbun-sankei-get-headers-seiron (shimbun range)
  (let ((from (concat (shimbun-server-name shimbun)
		      " (" (shimbun-current-group-name shimbun) ")"))
	(group (shimbun-current-group-internal shimbun))
	url year serial month day headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat
		 "<a" s1 "href=\"\\(?:\\./\\)?"
		 ;; 1. url
		 "\\("
		 ;; 2. year
		 "\\(\\(?:20\\)?[0-9][0-9]\\)"
		 "[01][0-9][0-3][0-9]/\\(?:sir"
		 ;; 3. serial
		 "\\([0-9]+\\)"
		 "\\|morning/seiron\\)\\.html?\\)"
		 "\"" s0 ">" s0 "【正論】" s0
		 ;; 4. subject
		 "\\([^<]+\\)"
		 "\\(?:" s0 "<[^>]+>\\)*" s0 "(" s0
		 ;; 5. month
		 "\\([01]?[0-9]\\)"
		 "/"
		 ;; 6. day
		 "\\([0-3]?[0-9]\\)"
		 s1
		 ;; 7. time
		 "\\([012]?[0-9]:[0-5]?[0-9]\\(?:[0-5]?[0-9]\\)?\\)"
		 s0 ")")))
	    nil t)
      (setq url (concat shimbun-sankei-url "news/" (match-string 1)))
      (when (< (setq year (string-to-number (match-string 2))) 100)
	(setq year (+ year 2000)))
      (setq serial (match-string 3)
	    month (string-to-number (match-string 5))
	    day (string-to-number (match-string 6)))
      (push (shimbun-create-header
	     0
	     (match-string 4)
	     from
	     (shimbun-make-date-string year month day (match-string 7))
	     (format "<%04d%02d%02d%s%%%s.%s>"
		     year month day
		     (if serial
			 (concat "." serial)
		       "")
		     group shimbun-sankei-top-level-domain)
	     "" 0 0 url)
	    headers))
    headers))

(luna-define-method shimbun-clear-contents :before ((shimbun shimbun-sankei)
						    header)
  (let ((group (shimbun-current-group-internal shimbun)))
    (cond ((string-equal group "editoria")
	   ;; Rearrange subject header.
	   (let (posns subjects)
	     (while (re-search-forward
		     (eval-when-compile
		       (let ((s0 "[\t\n 　]*")
			     (s1 "[\t\n ]+"))
			 (concat "[\t ]*<font" s1 "[^>]+>" s0 "■" s0
				 "</font>" s0 "<\\(?:strong\\|b\\)>" s0
				 "【主張】" s0 "\\([^<]+\\)" s0
				 "</\\(?:strong\\|b\\)>" s0 "<p>" s0)))
		     nil t)
	       (push (list (match-beginning 0) (match-end 0)) posns)
	       (push (match-string 1) subjects))
	     (cond ((= (length subjects) 1)
		    (delete-region (caar posns) (cadar posns)))
		   ((>= (length subjects) 2)
		    (shimbun-header-set-subject header
						(mapconcat
						 'identity
						 (nreverse subjects)
						 " / "))))))
	  ((string-equal group "column")
	   (while (re-search-forward "\\([^\n>]\\)\\(　▼\\)" nil t)
	     (replace-match "\\1。\n<p>\\2")))
	  ((string-equal group "seiron")
	   ;; Remove headline.
	   (when (re-search-forward
		  (eval-when-compile
		    (let ((s0 "[\t\n ]*"))
		      (concat s0 "<font[^>]+>" s0 "■" s0 "</font>" s0
			      "<\\(?:strong\\|b\\)>[^<]+</\\(?:strong\\|b\\)>"
			      s0 "\\(?:</font>" s0 "\\)?" "<hr>" s0 "\\(?:<p>"
			      s0 "\\)?")))
		  nil t)
	     (delete-region (match-beginning 0) (match-end 0)))
	   ;; Remove blockquote.
	   (when (re-search-forward "[\t ]*<BLOCKQUOTE>[\t ]*" nil t)
	     (delete-region (match-beginning 0) (match-end 0))))
	  (t
	   ;; Remove advertisements.
	   (shimbun-remove-tags "<!--[\t\n ]*AdSpace\\(?:.+=.+\\)+-->"
				"<!--[\t\n ]*/AdSpace[\t\n ]*-->")
	   ;; Remove an advertisement between photo and hombun.
	   (shimbun-remove-tags "<!--[\t\n ]*photo\\.end[\t\n ]*-->"
				"<!--[\t\n ]*hombun[\t\n ]*-->"))))
  (goto-char (point-min)))

(provide 'sb-sankei)

;;; sb-sankei.el ends here

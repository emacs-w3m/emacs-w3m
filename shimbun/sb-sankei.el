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
    ("editoria" "主張" "http://ez.st37.arena.ne.jp/cgi-bin/search/namazu.cgi\
?query=%8E%E5%92%A3&whence=0&max=20&sort=date%3Alate&idxname=edit")
    ("column" "産経抄" "http://ez.st37.arena.ne.jp/cgi-bin/search/namazu.cgi\
?query=%8EY%8Co%8F%B4&whence=0&max=50&sort=date%3Alate&result=normal&idxname\
=clm")
    ("seiron" "正論" "http://www.sankei.co.jp/special/opi/past_seiron.htm")))

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
  (let ((today (decode-time))
	(from (concat (shimbun-server-name shimbun)
		      " (" (shimbun-current-group-name shimbun) ")"))
	year month day url time headers)
    (while (or (and today
		    (if (and (>= (nth 2 today) 5)
			     (< (nth 2 today) 10))
			(prog1
			    t
			  (setq year (nth 5 today)
				month (nth 4 today)
				day (nth 3 today)
				url (format "\
http://www.sankei.co.jp/news/%02d%02d%02d/morning/editoria.htm"
					    (% year 100) month day)
				time "05:00"
				today nil))
		      (setq today nil)))
	       (and (re-search-forward
		     ;; 1:url 2:year 3:month 4:day 5:time
		     "<a[\t\n ]+href=\"\
\\(http://www\\.sankei\\.co\\.jp/news/\
\\([0-9][0-9]\\)\\([01][0-9]\\)\\([0-3][0-9]\\)\
/morning/editoria\\.htm\\)\
\">[\t\n 　]*\\(?:\\(?:<[^>]+>\\|主張\\)[\t\n 　]*\\)\
+([01][0-9]/[0-3][0-9][\t\n ]+\
\\([012][0-9]:[0-5][0-9]\\))"
				       nil t)
		    (prog1
			t
		      (setq year (+ 2000 (string-to-number (match-string 2)))
			    month (string-to-number (match-string 3))
			    day (string-to-number (match-string 4))
			    url (match-string 1)
			    time (match-string 5)))))
      (push (shimbun-create-header
	     0
	     (format "主張 (%d/%d)" month day)
	     from
	     (shimbun-make-date-string year month day time)
	     (format "<%d%02d%02d%%editoria.sankei.co.jp>" year month day)
	     "" 0 0
	     url)
	    headers))
    headers))

(defun shimbun-sankei-get-headers-column (shimbun range)
  (let ((today (decode-time))
	(from (concat (shimbun-server-name shimbun)
		      " (" (shimbun-current-group-name shimbun) ")"))
	year month day url time headers)
    (while (or (and today
		    (if (and (>= (nth 2 today) 5)
			     (< (nth 2 today) 10))
			(prog1
			    t
			  (setq year (nth 5 today)
				month (nth 4 today)
				day (nth 3 today)
				url (format "\
http://www.sankei.co.jp/news/%02d%02d%02d/morning/column.htm"
					    (% year 100) month day)
				time "05:00"
				today nil))
		      (setq today nil)))
	       (and (re-search-forward
		     ;; 1:url 2:year 3:month 4:day 5:time
		     "<a[\t\n ]+href=\"\
\\(http://www\\.sankei\\.co\\.jp/news/\
\\([0-9][0-9]\\)\\([01][0-9]\\)\\([0-3][0-9]\\)\
/morning/column\\.htm\\)\
\">[\t\n 　]*\\(?:\\(?:<[^>]+>\\|産経抄\\)[\t\n 　]*\\)\
+([01][0-9]/[0-3][0-9][\t\n ]+\
\\([012][0-9]:[0-5][0-9]\\))"
				       nil t)
		    (prog1
			t
		      (setq year (+ 2000 (string-to-number (match-string 2)))
			    month (string-to-number (match-string 3))
			    day (string-to-number (match-string 4))
			    url (match-string 1)
			    time (match-string 5)))))
      (push (shimbun-create-header
	     0
	     (format "産経抄 (%d/%d)" month day)
	     from
	     (shimbun-make-date-string year month day time)
	     (format "<%d%02d%02d%%editoria.sankei.co.jp>" year month day)
	     "" 0 0
	     url)
	    headers))
    headers))

(defun shimbun-sankei-get-headers-seiron (shimbun range)
  (let ((from (concat (shimbun-server-name shimbun)
		      " (" (shimbun-current-group-name shimbun) ")"))
	url year month day subject hour minute headers)
    (while (re-search-forward
	    (eval-when-compile
	      (concat
	       "<a href=\""
	       ;; 1. url
	       "\\(http://www\\.sankei\\.co\\.jp/news/"
	       ;; 2. year
	       "\\([0-9][0-9]\\)"
	       ;; 3. month
	       "\\([01][0-9]\\)"
	       ;; 4. day
	       "\\([0-3][0-9]\\)"
	       "/morning/seiron\\.htm\\)"
	       "\">■?【.*正論.*>】"
	       ;; 5. subject
	       "\\([^<]+\\)"
	       "</a></b>　([01][0-9]/[0-3][0-9] "
	       ;; 6. hour
	       "\\([012][0-9]\\)"
	       ":"
	       ;; 7. minute
	       "\\([0-5][0-9]\\)"))
	    nil t)
      (setq url (match-string 1)
	    year (+ 2000 (string-to-number (match-string 2)))
	    month (string-to-number (match-string 3))
	    day (string-to-number (match-string 4))
	    subject (match-string 5)
	    hour (match-string 6)
	    minute (match-string 7))
      (push (shimbun-create-header
	     0 subject from
	     (shimbun-make-date-string year month day
				       (concat hour ":" minute))
	     (format "<%d%02d%02d%s%s%%seiron.sankei.co.jp>"
		     year month day hour minute)
	     "" 0 0 url)
	    headers))
    headers))

(luna-define-method shimbun-clear-contents :before ((shimbun shimbun-sankei)
						    header)
  (if (string-equal (shimbun-current-group-internal shimbun) "column")
      (while (re-search-forward "\\([^\n>]\\)\\(　▼\\)" nil t)
	(replace-match "\\1。\n<p>\\2"))
    ;; Remove advertisements.
    (shimbun-remove-tags "<!--[\t\n ]*AdSpace\\(?:.+=.+\\)+-->"
			 "<!--[\t\n ]*/AdSpace[\t\n ]*-->")
    ;; Remove an advertisement between photo and hombun.
    (shimbun-remove-tags "<!--[\t\n ]*photo\\.end[\t\n ]*-->"
			 "<!--[\t\n ]*hombun[\t\n ]*-->"))
  (goto-char (point-min)))

(provide 'sb-sankei)

;;; sb-sankei.el ends here

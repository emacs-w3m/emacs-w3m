;;; sb-sankei.el --- shimbun backend for the Sankei Shimbun -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003, 2004, 2005 Katsumi Yamaoka

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
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

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
  "<!--[\t\n ]*\\(photo\\.sta\\|\\(ad--\\)?hombun\\)[\t\n ]*-->[\t\n ]*")

(defvar shimbun-sankei-content-end
  "[\t\n ]*<!--[\t\n ]*hbnend[\t\n ]*-->")

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
?query=%8E%E5%92%A3&whence=0&max=20&sort=date%3Alate&idxname=edit")))

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
  (if (string-equal (shimbun-current-group-internal shimbun) "editoria")
      (shimbun-sankei-get-headers-editoria shimbun range)
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
      (setq cyear (decode-time)
	    cmonth (nth 4 cyear)
	    cyear (nth 5 cyear))
      (while (re-search-forward "<a[\t\n ]+href=\"\
\\(\
\\([/0-9a-z]+\\)\
\\.htm\\)\
\">[\t\n ]*\
\\([^\n<>]+\\)\
\[\t\n ]*</a>\\([\t\n ]*<[^<>]+>\\)*[\t\n ]*(\
\\([01][0-9]\\)\
/\
\\([0-3][0-9]\\)\
\[\t\n ]+\
\\([012][0-9]:[0-5][0-9]\\)\
)"
				nil t)
	(setq month (string-to-number (match-string 5))
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
					 (string-to-number (match-string 6))
					 (match-string 7))
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
      headers)))

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
	       (and (re-search-forward "<a[\t\n ]+href=\"\
\\(http://www\\.sankei\\.co\\.jp/news/\
\\([0-9][0-9]\\)\\([01][0-9]\\)\\([0-3][0-9]\\)\
/morning/editoria\\.htm\\)\
\">[\t\n 　]*\\(\\(<[^>]+>\\|主張\\)[\t\n 　]*\\)\
+([01][0-9]/[0-3][0-9][\t\n ]+\
\\([012][0-9]:[0-5][0-9]\\))"
				       nil t)
		    (prog1
			t
		      (setq year (+ 2000 (string-to-number (match-string 2)))
			    month (string-to-number (match-string 3))
			    day (string-to-number (match-string 4))
			    url (match-string 1)
			    time (match-string 7)))))
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

(luna-define-method shimbun-clear-contents :before ((shimbun shimbun-sankei)
						    header)
  (if (string-equal (shimbun-current-group-internal shimbun) "editoria")
      (when (and (re-search-forward shimbun-sankei-content-start nil t)
		 (re-search-forward "[\t\n ]*<p>[\t\n ]*<p>" nil t))
	(goto-char (match-beginning 0))
	(insert "\n<!--hbnend-->"))
    ;; Remove advertisements.
    (shimbun-remove-tags "<!--[\t\n ]*AdSpace\\(.+=.+\\)+-->"
			 "<!--[\t\n ]*/AdSpace[\t\n ]*-->")
    ;; Remove an advertisement between photo and hombun.
    (shimbun-remove-tags "<!--[\t\n ]*photo\\.end[\t\n ]*-->"
			 "<!--[\t\n ]*hombun[\t\n ]*-->"))
  (goto-char (point-min)))

(provide 'sb-sankei)

;;; sb-sankei.el ends here

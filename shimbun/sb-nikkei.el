;;; sb-nikkei.el --- shimbun backend for nikkei.co.jp -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003
;; Kazuyoshi KOREEDA <Kazuyoshi.Koreeda@rdmg.mgcs.mei.co.jp>

;; Author: Kazuyoshi KOREEDA <Kazuyoshi.Koreeda@rdmg.mgcs.mei.co.jp>,
;;         Katsumi Yamaoka   <yamaoka@jpl.org>
;; Keywords: news

;; This file is a part of shimbun.

;; This program is free software; you can redistribute it a>nd/or modify
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

;; Original code was sb-asahi.el which is written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org> and
;; Yuuichi Teranishi <teranisi@gohome.org>.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)

(luna-define-class shimbun-nikkei (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-nikkei-top-level-domain "nikkei.co.jp"
  "Name of the top level domain for the Nikkei Net.")

(defvar shimbun-nikkei-url
  (concat "http://www." shimbun-nikkei-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-nikkei-group-table
  (let* ((s0 "[\t\n ]*")
	 (s1 "[\t\n ]+")
	 (default (list
		   (concat
		    "<\\(!--" s0 "\\)?a" s1 "href=\""
		    ;; 2. url
		    "\\("
		    ;; 3. serial number
		    "\\("
		    ;; 4. year
		    "\\(20[0-9][0-9]\\)"
		    ;; 5. month
		    "\\([01][0-9]\\)"
		    ;; 6. day
		    "\\([0-3][0-9]\\)"
		    "[0-9A-Z]+\\)"
		    "\\.html\\)"
		    "\"\\(" s1 "class=\"txt\"\\|" s0 "--\\)>" s0
		    "\\(<!--" s0 "FJZONE" s1 "START" s1 "NAME=\"MIDASHI\"" s0
		    "-->\\|・\\)" s0 "<!--" s0 "headline" s0 "-->" s0
		    ;; 9. subject
		    "\\([^<>]+\\)"
		    s0 "<!--" s0 "/headline" s0 "-->"
		    "\\(" s0 "</a>" s0 "("
		    ;; 11. hour:minute
		    ;; Note: there may not be an hour:minute data in the
		    ;; headline news, so we should do a special treatment.
		    "\\([012][0-9]:[0-5][0-9]\\)"
		    ")\\)?")
		   2 3 4 5 6 9 11))
	 ;; For okuyami and shasetsu.
	 (oku-sha (list
		   (concat
		    "<a" s1 "href=\""
		    ;; 1. url
		    "\\("
		    ;; 2. serial number
		    "\\("
		    ;; 3. year
		    "\\(20[0-9][0-9]\\)"
		    ;; 4. month
		    "\\([01][0-9]\\)"
		    ;; 5. day
		    "\\([0-3][0-9]\\)"
		    "[0-9A-Z]+\\)"
		    "\\.html\\)"
		    "\"" s1 "class=\"txt\">" s0 "[^<>]+" s0
		    "<!--" s0 "headline" s0 "-->"
		    ;; 6. subject
		    "\\([^<>]+\\)"
		    s0 "<!--" s0 "/headline" s0 "-->")
		   1 2 3 4 5 6)))
    `(("main" "主要" "news/main/" ,@default)
      ("keizai" "経済" "news/keizai/" ,@default)
      ("market" "株・為替" "news/market/index.html"
       ,(concat "<a" s1 "HREF=\""
		;; 1. url
		"\\("
		;; 2. serial number
		"\\("
		;; 3. year
		"\\(20[0-9][0-9]\\)"
		"[01][0-9][0-3][0-9]"
		"[0-9a-z]+\\)"
		"\\.html\\)"
		"\"" s1 "class=\"txt\">" s0
		;; 4. month
		"\\([01][0-9]\\)"
		"/"
		;; 5. day
		"\\([0-3][0-9]\\)"
		s1
		;; 6. hour:minute
		"\\([012][0-9]:[0-5][0-9]\\)"
		s0 "-" s0 "<!--" s0 "headline" s0 "-->" s0
		;; 7. subject
		"\\([^<>]+\\)"
		s0 "<!--" s0 "/headline" s0 "-->")
       1 2 3 4 5 7 6)
      ("kaigai" "国際" "news/kaigai/" ,@default)
      ("sangyo" "企業" "news/sangyo/" ,@default)
      ("tento" "ベンチャー" "news/tento/" ,@default)
      ("seiji" "政治" "news/seiji/" ,@default)
      ("shakai" "社会" "news/shakai/" ,@default)
      ("newpro" "新製品" "newpro/news/index.html"
       ;; There is currently no way to view the headline news. :(
       ,(concat "<a" s1 "href=\""
		;; 1. url
		"\\("
		;; 2. serial number
		"\\("
		;; 3. year
		"\\(20[0-9][0-9]\\)"
		;; 4. month
		"\\([01][0-9]\\)"
		;; 5. day
		"\\([0-3][0-9]\\)"
		"[0-9a-z]+\\)"
		"\\.html\\)"
		"\"" s1 "class=\"txt\">" s0
		;; 6. subject
		"\\([^<>]+\\)"
		s0)
       1 2 3 4 5 6)
      ("shasetsu" "社説・春秋" "news/shasetsu/" ,@oku-sha)
      ("zinzi" "トップ人事" "news/zinzi/" ,@default)
      ("okuyami" "おくやみ" "news/okuyami/" ,@oku-sha)
      ("retto" "地域経済" "news/retto/index.html"
       ,(concat "<AREA21" s1 "HEADLINE=\""
		;; 1. subject
		"\\([^\"]+\\)"
		"\"" s1 "URL=\""
		;; 2. url
		"\\("
		;; 3. serial number
		"\\(20[0-9][0-9][01][0-9][0-9a-z]+\\)"
		"\\.html\\)"
		"\"" s1 "ARTICLE_TIME=\""
		;; 4. year
		"\\(20[0-9][0-9]\\)" "/"
		;; 5. month
		"\\([01][0-9]\\)" "/"
		;; 6. day
		"\\([0-3][0-9]\\)" s1
		;; 7. hour:minute
		"\\([012][0-9]:[0-5][0-9]\\)"
		"\"" s1 "\\([A-Z]+=\"[^\"]*\"" s1 "\\)+BASE_URL=\""
		;; 9. base-url
		"\\([^\"]+\\)" "\"")
       2 3 4 5 6 1 7 9)))
  "Alist of group names, their Japanese translations, index pages,
regexps and numbers.  Numbers point to the search result in order of
a url, a serial number, a year, a month, a day, a subject, an
hour:minute, and a folder.")

(defvar shimbun-nikkei-server-name "日本経済新聞")
(defvar shimbun-nikkei-from-address "webmaster@nikkei.co.jp")
(defvar shimbun-nikkei-content-start
  "<!--[\t\n _]*FJZONE[\t\n _]+START[\t\n _]+NAME=\"HONBUN\"[\t\n _]*-->")
(defvar shimbun-nikkei-content-end
  "<!--[\t\n _]*FJZONE[\t\n _]+END[\t\n _]+NAME=\"HONBUN\"[\t\n _]*-->")
(defvar shimbun-nikkei-x-face-alist
  '(("default" . "X-Face: \"e7z+~O:s!)$84Dc68C##jE/~I8U:HDUkL@P\
euEhS<ijhd\"jc63do:naCRWPEr{Y5M?|]5g\n sa8m5@=sm%AIsSRA9*k08-`=\
w?yVB`L_vBG:j~~vhEoHC^Hjq`V(RMFQqa>9jqkt1<G[FMZTb:F@NT\n mcE[_Z\
_hl5zM,zn?WC*iun#*nJ'YRj}%;:|Y&X)kTXeM#lE*Y^E5}QMe?<pJjd</ktdg\\\
w9O17:Z>!\n vmZQ.BUpki=FZ:m[;]TP%D\\#uN6/)}c`/DPxKB?rQhBc\"")))
(defvar shimbun-nikkei-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-nikkei))
  (mapcar 'car shimbun-nikkei-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-nikkei))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-nikkei-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-nikkei))
  (concat shimbun-nikkei-url
	  (nth 2 (assoc (shimbun-current-group-internal shimbun)
			shimbun-nikkei-group-table))))

(defun shimbun-nikkei-get-headers (shimbun range)
  (let ((from (shimbun-from-address shimbun))
	(group (shimbun-current-group-internal shimbun))
	(case-fold-search t)
	regexp folder numbers stamptime subject year month day time headers)
    (setq regexp (assoc group shimbun-nikkei-group-table)
	  folder (file-name-directory (nth 2 regexp))
	  numbers (nthcdr 4 regexp)
	  regexp (nth 3 regexp))
    ;; Extracting the headline news for the okuyami or the shasetsu group.
    (when (and (member group '("okuyami" "shasetsu"))
	       (boundp 'w3m-current-url)
	       (re-search-forward
		(eval-when-compile
		  (let ((s0 "[\t\n ]*")
			(s1 "[\t\n ]+"))
		    (concat "<!--" s0 "FJZONE" s1 "START" s1
			    "NAME=\"MIDASHI\"" s0 "-->" s0
			    "<!--" s0 "headline" s0 "-->" s0
			    "\\([^<>]+\\)" s0)))
		nil t))
      (setq subject (match-string 1))
      (goto-char (point-min))
      (let* ((url (symbol-value 'w3m-current-url))
	     (id (file-name-sans-extension
		  (file-name-nondirectory url))))
	(when (string-match "\\`[^0-9]*\\(20[0-9][0-9]\\)\
\\([01][0-9]\\)\\([0-3][0-9]\\)" id)
	  (push (shimbun-make-header
		 0 subject from
		 (shimbun-make-date-string
		  (string-to-number (match-string 1 id))
		  (string-to-number (match-string 2 id))
		  (string-to-number (match-string 3 id)))
		 (concat "<" (substring id (match-beginning 1))
			 "%" group "."
			 shimbun-nikkei-top-level-domain ">")
		 "" 0 0 url)
		headers))))
    ;; Extracting the timestamp of the page.
    (when (re-search-forward "<!--[\t\n ]*timestamp[\t\n ]*-->[\t\n ]*\
\\(<!--[\t\n ]*theTime[\t\n ]*-->[\t\n ]*\\)?\
20[0-9][0-9]/[01][0-9]/[0-3][0-9][\t\n ]+\
\\([012][0-9]:[0-5][0-9]\\)[\t\n ]*<!--[\t\n ]*/timestamp[\t\n ]*-->"
			     nil t)
      (setq stamptime (match-string 2)))
    ;; Generating headers.
    (while (re-search-forward regexp nil t)
      (setq subject (match-string (nth 5 numbers))
	    year (string-to-number (match-string (nth 2 numbers)))
	    month (string-to-number (match-string (nth 3 numbers)))
	    day (string-to-number (match-string (nth 4 numbers)))
	    time
	    (or (and (nth 6 numbers)
		     (match-beginning (nth 6 numbers))
		     (match-string (nth 6 numbers)))
		;; Extracting the hour:minute from the headline article.
		(and
		 (member group '("kaigai" "keizai" "main" "sangyo" "seiji"
				 "shakai"))
		 (save-match-data
		   (and (or (re-search-forward
			     (concat "(\\([012][0-9]:[0-5][0-9]\\))[\t\n ]*"
				     (shimbun-content-end-internal shimbun))
			     nil t)
			    (re-search-forward
			     (concat (shimbun-content-end-internal shimbun)
				     "[\t\n ]*(\\([012][0-9]:[0-5][0-9]\\))")
			     nil t))
			(match-string 1))))))
      (when (and (not (member group '("market" "retto")))
		 stamptime
		 time
		 (string-lessp stamptime time))
	;; It may be yesterday's article, so we should fix a date.
	(when (zerop (decf day))
	  (if (zerop (decf month))
	      (setq day 31
		    month 12
		    year (1- year))
	    (setq day (cond ((= month 2)
			     ;; It should be fixed by the end of this century.
			     (if (zerop (% year 4)) 29 28))
			    ((memq month '(4 6 9 11)) 30)
			    (t 31))))))
      (save-match-data
	(cond ((string-equal group "retto")
	       (save-excursion
		 (when (re-search-backward "【[^<>]+】" nil t)
		   (setq subject (concat (match-string 0) subject)))))
	      ((string-equal group "tento")
	       (when (string-match "\\`([01]?[0-9]/[0-3]?[0-9])[\t\n ]*"
				   subject)
		 (setq subject (substring subject (match-end 0)))))))
      (when (and (nth 7 numbers)
		 (match-beginning (nth 7 numbers)))
	(setq folder (match-string (nth 7 numbers))))
      (push (shimbun-make-header
	     ;; number
	     0
	     ;; subject
	     subject
	     ;; from
	     from
	     ;; date
	     (shimbun-make-date-string year month day time)
	     ;; id
	     (concat "<" (match-string (nth 1 numbers)) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     ;; references, chars, lines
	     "" 0 0
	     ;; xref
	     (concat shimbun-nikkei-url folder
		     (match-string (nth 0 numbers))))
	    headers))
    (if (string-equal group "shasetsu")
	(nreverse headers)
      headers)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-nikkei)
					 &optional range)
  (shimbun-nikkei-get-headers shimbun range))

(defun shimbun-nikkei-prepare-article (shimbun header)
  "Prepare an article: adjusting a date header if there is a correct
information available, removing useless contents, etc."
  (let ((group (shimbun-current-group-internal shimbun))
	(case-fold-search t))
    (cond
     ((member group '("kaigai" "keizai" "main" "sangyo" "seiji" "shakai"))
      ;; Adjust a date header.
      (let (date start)
	(when (and (string-match " \\(00:00\\) "
				 (setq date (shimbun-header-date header)))
		   (setq start (match-beginning 1))
		   (re-search-forward (shimbun-content-start-internal shimbun)
				      nil t)
		   (re-search-forward
		    (concat "(\\([012][0-9]:[0-5][0-9]\\))[\t\n ]*"
			    (shimbun-content-end-internal shimbun))
		    nil t))
	  (shimbun-header-set-date
	   header (concat (substring date 0 start)
			  (match-string 1)
			  (substring date (+ start 5)))))))
     ((string-equal group "newpro")
      ;; Remove useless contents.
      (when (re-search-forward
	     "<!--[\t\n ]*top[\t\n _]+news[\t\n ]*-->[\t\n ]*" nil t)
	(delete-region (point-min) (point)))
      (when (re-search-forward
	     "[\t\n ]*<!--[\t\n ]*/top[\t\n _]+news[\t\n ]*-->" nil t)
	(delete-region (match-beginning 0) (point-max))))
     ((string-equal group "retto")
      ;; Remove useless contents.
      (while (re-search-forward "<div[\t\n ]+class=\"news\">" nil t))
      (delete-region (point-min) (point))
      (when (search-forward "</div>" nil t)
	(delete-region (match-beginning 0) (point-max))))))
  (goto-char (point-min)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-nikkei)
						   header)
  (shimbun-nikkei-prepare-article shimbun header))

(provide 'sb-nikkei)

;;; sb-nikkei.el ends here

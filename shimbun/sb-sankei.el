;;; sb-sankei.el --- shimbun backend for the Sankei Shimbun -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003, 2004, 2005, 2006, 2007 Katsumi Yamaoka

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
  "<!--[\t\n ]*\\(?:photo\\.sta\\|hombun\\)[\t\n ]*-->[\t\n ]*")

(defvar shimbun-sankei-content-end "[\t\n ]*<!--[\t\n ]*hbnend[\t\n ]*-->")

(defvar shimbun-sankei-group-table
  (let* ((s0 "[\t\n ]*")
	 (s1 "[\t\n ]+")
	 (news
	  (list
	   (concat
	    "<a" s1 "href=\""
	    ;; 1. url
	    "\\("
	    "\\(?:\\./\\)?" ;; news.chiho
	    ;; 2. category
	    "\\([^/]+\\)"
	    "/"
	    ;; 3. year
	    "\\([0-9][0-9]\\)"
	    "[01][0-9][0-3][0-9]/"
	    ;; 4. serial number
	    "\\([^\"]+\\)"
	    "\\.htm\\)"
	    "\">" s0
	    ;; 5. subject
	    "\\([^<]+\\)"
	    s0 "</a>" s0 "("
	    ;; 6. month
	    "\\([01][0-9]\\)"
	    "/"
	    ;; 7. day
	    "\\([0-3][0-9]\\)"
	    s1
	    ;; 8. time
	    "\\([012][0-9]:[0-5][0-9]\\)"
	    s0 ")")
	   1 2 3 4 5 6 7 8))
	 (special
	  (list
	   (concat
	    "<a" s1 "href=\""
	    ;; 1. url
	    "\\(\\(?:[^0-9/]+/\\)+"
	    ;; 2. year
	    "\\([0-9][0-9]\\)"
	    "[01][0-9][0-3][0-9]/"
	    ;; 3. serial number
	    "\\([^\"]+\\)"
	    "\\.htm\\)"
	    "\">" s0
	    ;; 4. subject
	    "\\([^<]+\\)"
	    s0 "</a>" s0 "("
	    ;; 5. month
	    "\\([01][0-9]\\)"
	    "/"
	    ;; 6. day
	    "\\([0-3][0-9]\\)"
	    s1
	    ;; 7. time
	    "\\([012][0-9]:[0-5][0-9]\\)"
	    s0 ")")
	   1 nil 2 3 4 5 6 7))
	 (kiko
	  (list
	   (concat
	    "<a" s1 "href=\""
	    ;; 1. url
	    "\\(\\./tanbo/"
	    ;; 2. serial number
	    "\\(tanbou_[0-9]+\\)"
	    "\\.htm\\)"
	    "\">" s0 "20"
	    ;; 3. year
	    "\\([0-9][0-9]\\)"
	    "\\."
	    ;; 4. month
	    "\\([01][0-9]\\)"
	    "\\."
	    ;; 5. day
	    "\\([0-3][0-9]\\)"
	    "[\t\n 　]*"
	    ;; 6. subject
	    "\\([^<]+\\)")
	   1 nil 3 2 6 4 5 nil))
	 (ronsetsu
	  (list
	   (concat
	    "<a" s1 "href=\""
	    ;; 1. url
	    "\\("
	    ;; 2. year
	    "\\([0-9][0-9]\\)"
	    "[01][0-9][0-3][0-9]/"
	    ;; 3. serial number
	    "\\([a-z]+[0-9]+\\)"
	    "\\.htm\\)"
	    "\">" s0 "【[^】]+】" s0
	    ;; 4. subject
	    "\\([^<]+\\)?"
	    s0 "</a>" s0 "("
	    ;; 5. month
	    "\\([01][0-9]\\)"
	    "/"
	    ;; 6. day
	    "\\([0-3][0-9]\\)"
	    s1
	    ;; 7. time
	    "\\([012][0-9]:[0-5][0-9]\\)"
	    ")")
	   1 nil 2 3 4 5 6 7)))
    `(("news.shakai" "社会" "shakai/shakai.htm" ,@news)
      ("news.kokusai" "国際" "kokusai/kokusai.htm" ,@news)
      ("news.seiji" "政治" "seiji/seiji.htm" ,@news)
      ("news.keizai" "経済" "keizai/keizai.htm" ,@news)
      ("news.seikatsu" "生活・健康" "seikatsu/seikatsu.htm" ,@news)
      ("news.kyouiku" "教育・福祉" "kyouiku/kyouiku.htm" ,@news)
      ("news.sports" "スポーツ" "sports/sports.htm" ,@news)
      ("news.cutlure" "カルチャー" "culture/culture.htm" ,@news)
      ("news.chiho" "地方" "chiho/chiho.htm" ,@news)
      ("special.komori" "古森義久のワシントン報告" "special/komori/komori.htm"
       ,@special)
      ("special.kuroda" "黒田勝弘のソウル報告" "special/kuroda/kuroda.htm"
       ,@special)
      ("special.ito" "伊藤正の北京報告" "special/ito/ito.htm" ,@special)
      ("special.tamura" "田村秀男の「経済がわかれば世界がわかる」"
       "special/tamura/tamura.htm" ,@special)
      ("special.jieitai" "自衛隊特集" "special/jieitai/jieitai.htm" ,@special)
      ("special.kenpo" "憲法" "special/kenpo/kenpo.htm" ,@special)
      ("special.kyouiku" "教育を考える" "special/kyouiku/kyouiku.htm"
       ,@special)
      ("special.kiko" "紀行" "special/kiko/kiko.htm" ,@kiko)
      ("ronsetsu.shucho" "主張" "ronsetsu/shucho/shucho.htm" ,@ronsetsu)
      ("ronsetsu.sankeisho" "産経抄" "ronsetsu/sankeisho/sankeisho.htm"
       ,@ronsetsu)
      ("ronsetsu.seiron" "正論" "ronsetsu/seiron/seiron.htm" ,@ronsetsu)))
  "Alist of group names, their Japanese translations, index pages,
regexps and numbers.  Where numbers point to the search result in order
of [0]a url, [1]a category, [2]a year, [3]a serial number, [4]a subject,
\[5]a month, [6]a day and [7]an hour:minute.")

(defvar shimbun-sankei-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAACUAAAAlAgMAAAC48MiQAAAADFBMVEUDU4CnwdA3dpr///9
 livkjAAABOUlEQVQY00XRPUvDUBQG4Nc4qAhO1SAOUQcpDrq4iTR0dwwRKlZcBKV0aqvVkGyCk5O
 LSn6BulTBQSMOzg62DiKp4MeggkPbDDU53nOj9C734fByzv2A8b906PhbgpO8T0h6Kw6gOUztowH
 FXhZME4VQKdRF9axgdVFId4J7F1HfgrfmCFp0D9WvCaaz/hVSRFx9cFtIWQY3s4NvDL4lZd9VwbA
 ipzU3MEQNpvtaQcKuy8DtDBJZybnPKQz7Bjc7fxnALNG14GiyF+OOwoGisoT++BaUj9CzU+Vs++k
 QWl5mbX8Rl5s1DgT0g+3SvsyGJ7ADW3ZwI0yvEzPTjKA9hkyzeoTyVoHpm8colmJSDl6bA+mvzDy
 05xbzXVS1epkH3xzkoJpysNO9i5FTg98MCjAWH7Lz6p2/+AUCpou5oeyt8wAAAABJRU5ErkJggg=
 =")))

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
  (let* ((group (shimbun-current-group-internal shimbun))
	 (numbers (cdr (assoc group shimbun-sankei-group-table)))
	 (name (pop numbers))
	 (base (shimbun-expand-url (file-name-directory (pop numbers))
				   (shimbun-url-internal shimbun)))
	 (regexp (pop numbers))
	 (rgroup (mapconcat 'identity
			    (nreverse (split-string group "\\."))
			    "."))
	 start categories
	 url category year subject month day time from id headers)
    ;; Make an alist of categories and those Japanese names.
    (when (and (nth 1 numbers)
	       (search-forward "<!--▼カテゴリタイトル▼-->" nil t)
	       (progn
		 (setq start (point))
		 (search-forward "<!--▲カテゴリタイトル▲-->" nil t)))
      (narrow-to-region start (match-beginning 0))
      (goto-char (point-min))
      (while (re-search-forward
	      (eval-when-compile
		(let ((s0 "[\t\n ]*")
		      (s1 "[\t\n ]+"))
		  (concat "<a" s1 "href=\"[^\"]+/"
			  ;; 1. category
			  "\\([^/]+\\)"
			  "\\.htm\">" s0 "\\(?:<[^>]+>" s0 "\\)*"
			  ;; 2. name
			  "\\([^<]+\\)"
			  s0 "</a>")))
	      nil t)
	(push (cons (match-string 1) (match-string 2)) categories))
      (widen))
    ;; Look for article headers.
    (while (re-search-forward regexp nil t)
      (setq url (shimbun-expand-url (match-string (nth 0 numbers)) base)
	    category (when (nth 1 numbers)
		       (match-string (nth 1 numbers)))
	    year (+ 2000 (string-to-number (match-string (nth 2 numbers))))
	    subject (match-string (nth 4 numbers))
	    month (string-to-number (match-string (nth 5 numbers)))
	    day (string-to-number (match-string (nth 6 numbers)))
	    time (when (nth 7 numbers)
		   (match-string (nth 7 numbers)))
	    from (concat name "/" shimbun-sankei-server-name
			 (when (and category categories)
			   (concat " (" (or (cdr (assoc category categories))
					    category)
				   ")")))
	    id (concat "<" (match-string (nth 3 numbers)) ;; serial number
		       "%" (when category
			     (concat category "."))
		       rgroup "." shimbun-sankei-top-level-domain ">"))
      (setq subject
	    (if subject
		(shimbun-replace-in-string subject "[\t\n 　]+\\'" "")
	      (format "%4d.%02d.%02d" year month day)))
      (unless (shimbun-search-id shimbun id)
	(push (shimbun-create-header
	       0 subject from
	       (shimbun-make-date-string year month day time)
	       id "" 0 0 url)
	      headers)))
    (shimbun-sort-headers headers)))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-sankei)
						    header)
  (let ((group (shimbun-current-group-internal shimbun))
	start)
    (cond ((string-equal group "news.sports")
	   (when (re-search-forward "\
\\(?:[\t\n ]*\\(?:&nbsp\;[\t\n ]*\\)?<[^>]+>\\)*\
\[\t\n ]*\\(?:プロ野球\\|大リーグ\\)関連サイト＆ＰＲ[\t\n ]*<"
				    nil t)
	     (goto-char (match-beginning 0))
	     (insert "<!--hbnend-->")))
	  ((string-equal group "special.kiko")
	   ;; Remove commented sections.
	   (when (re-search-forward "\
\[\t\n ]*<[\t\n ]*!-+\\(?:[^<>]*<[^>]+>\\)+[^<>]*-+[\t\n ]*>[\t\n ]*"
				    nil t)
	     (delete-region (match-beginning 0) (match-end 0))
	     (goto-char (point-min)))
	   (if (re-search-forward
		"<p[\t\n ]+class=\\(?:\"paragraph\"\\|'paragraph'\\)>"
		nil t)
	       (progn
		 (goto-char (match-beginning 0))
		 (insert "<!--hombun-->")
		 (setq start (point))
		 (forward-line 1))
	     (re-search-forward shimbun-sankei-content-start nil t)
	     (setq start (point)))
	   (when (re-search-forward "\
\[\t\n ]*\\(?:</div>\\)?[\t\n ]*<!--▲記事本文▲-->"
				    nil 'move)
	     (goto-char (match-beginning 0)))
	   ;; Break lines after images.
	   (narrow-to-region start (point))
	   (goto-char (point-min))
	   (while (re-search-forward "<img[\t\n ]+src=[^>]+>" nil 'move)
	     (insert "<br>"))
	   (widen)
	   (insert "<!--hbnend-->"))))
  (when (luna-call-next-method)
    ;; Break long lines.
    (unless (shimbun-prefer-text-plain-internal shimbun)
      (shimbun-break-long-japanese-lines))
    t))

(provide 'sb-sankei)

;;; sb-sankei.el ends here

;;; sb-nikkei.el --- shimbun backend for nikkei.co.jp -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004
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
  `(("top" "トップ" ,shimbun-nikkei-url
     shimbun-nikkei-get-headers-top
     shimbun-nikkei-prepare-article-default)
    ("main" "主要" ,(concat shimbun-nikkei-url "news/main/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("keizai" "経済" ,(concat shimbun-nikkei-url "news/keizai/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("sangyo" "企業" ,(concat shimbun-nikkei-url "news/sangyo/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)

    ("stockjp" "国内株"
     "http://markets.nikkei.co.jp/stockjp/jpnews/"
     shimbun-nikkei-get-headers-markets
     shimbun-nikkei-prepare-article-markets)
    ("exchange" "為替"
     "http://markets.nikkei.co.jp/exchange/exchangenews/"
     shimbun-nikkei-get-headers-markets
     shimbun-nikkei-prepare-article-markets)
    ("stockwo-us" "米国株"
     "http://markets.nikkei.co.jp/stockwo/usnews/"
     shimbun-nikkei-get-headers-markets
     shimbun-nikkei-prepare-article-markets)
    ("stockwo-eu" "欧州株"
     "http://markets.nikkei.co.jp/stockwo/euroasia/"
     shimbun-nikkei-get-headers-markets
     shimbun-nikkei-prepare-article-markets)

    ("kaigai" "国際" ,(concat shimbun-nikkei-url "news/kaigai/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("seiji" "政治" ,(concat shimbun-nikkei-url "news/seiji/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("shakai" "社会" ,(concat shimbun-nikkei-url "news/shakai/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("sports" "スポーツ" "http://sports.nikkei.co.jp/"
     shimbun-nikkei-get-headers-sports
     shimbun-nikkei-prepare-article-sports)
    ("newpro" "新製品" ,(concat shimbun-nikkei-url "newpro/news/")
     shimbun-nikkei-get-headers-newpro
     shimbun-nikkei-prepare-article-newpro)
    ("release" "プレスリリース" "http://release.nikkei.co.jp/"
     shimbun-nikkei-get-headers-release
     shimbun-nikkei-prepare-article-release)
    ("shasetsu" "社説・春秋" ,(concat shimbun-nikkei-url "news/shasetsu/")
     shimbun-nikkei-get-headers-shasetsu
     shimbun-nikkei-prepare-article-default)
    ("okuyami" "おくやみ" ,(concat shimbun-nikkei-url "news/okuyami/")
     shimbun-nikkei-get-headers-okuyami
     shimbun-nikkei-prepare-article-okuyami))
  "Alist of group names and parameters.
Each parameters include a Japanese group name, an index page, a
function used to get headers and a function used to prepare an article.")

(defvar shimbun-nikkei-server-name "日本経済新聞")
(defvar shimbun-nikkei-from-address "webmaster@nikkei.co.jp")
(defvar shimbun-nikkei-content-start
  "<!--emacs-w3m-shimbun-nikkei-content-start-->")
(defvar shimbun-nikkei-content-end
  "<!--emacs-w3m-shimbun-nikkei-content-end-->")
(defvar shimbun-nikkei-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAWAgMAAAD7mfc/AAAABGdBTUEAALGPC/xhBQAAAAx
 QTFRFBjKeZ4rcxdHp+/z7lhoK9wAAAC90RVh0U29mdHdhcmUAWFYgVmVyc2lvbiAzLjEwYStGTG1
 hc2sgIFJldjogMTIvMjkvOTQbx6p8AAABBUlEQVR4nD3MIU/DQBwF8D8d62W9wGqalYRSREUV9xX
 aC0snKzgQC8GgmUFg2Lh+AhD11dUYEsj2Ea6i/swEbqKiTZYeHIJnXn7iPehwXRUfs7tpbL5Biyo
 uZOiZYHJoUMb5y3twhhSFnZHR2NjZz7hHIGkyiJFMP8FCILhpIiqKp+EthkqMLMQ3TUD2Y8jEyQi
 LuLFJ6wMVrjsWRgvOPYFEBqEvjSuAcwJK55uVuv7Qs7n6xzYlSWrYoNHn6YV1cJ06Gh2LvCOs5Eo
 jZ9Gph5VYg57fTN0Q/I1Gx+bDw9BZcP22ZQ8WPBKVadTs6xiKlaIaOdv70SssB7/oy7JbxPXlcqJ
 +AFOYhEr5ENrbAAAAB3RJTUUH1AQGFzot7I86fAAAAABJRU5ErkJggg==")
    ("release" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAATBAMAAAAkFJMsAAAABGdBTUEAALGPC/xhBQAAABJ
 QTFRFAAAAZI9jnMGYtNiv1+3U////kl1YDAAAAC90RVh0U29mdHdhcmUAWFYgVmVyc2lvbiAzLjE
 wYStGTG1hc2sgIFJldjogMTIvMjkvOTQbx6p8AAABM0lEQVR4nDVRQZKEIAwMg97DoHddnTsI3NG
 BOwj5/1c2WzvTKZJ0NZWiA/hVCsewf5mPAUAAEOAOoVFrZ7V6OrdZ+IcANVFRtNEP2UChP3LNRHW
 ugDPVB000cJzhngnoVJgLqEBtanMbe26Qb4oP4klkQJHvcwllblQEdVpGSkmzKEhSiDnmMveJkLi
 4Y+wAsgtijmRGmknkxuNhrAJelQXJAZKoPQLRfWAoAvYI/hwu+QZIKWqfGIvH578d9bW1HZ/uyf7
 F+pcQEBfl3MrXmD7h1fXBnVqEzNemmxWoEEHDPOxpX/3xxsFUb6sz0mqrN3jl4eq1uYZDKr3WkPS
 VbK8QYMBqXNxxWMpti1fTpettwLPgTcIEEs/dxClO5xQ970ogfMGv+TL+D/UVhFBKreuH/QJ8OEG
 oiorzBAAAAAd0SU1FB9QECQYoFI75G7YAAABodEVYdENvbW1lbnQAQ1JFQVRPUjogWFYgVmVyc2l
 vbiAzLjEwYStGTG1hc2sgIFJldjogMTIvMjkvOTQKQ1JFQVRPUjogWFYgVmVyc2lvbiAzLjEwYSt
 GTG1hc2sgIFJldjogMTIvMjkvOTQKYD1XBQAAAABJRU5ErkJggg==")))

(defvar shimbun-nikkei-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-nikkei))
  (mapcar 'car shimbun-nikkei-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-nikkei))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-nikkei-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-nikkei))
  (nth 2 (assoc (shimbun-current-group-internal shimbun)
		shimbun-nikkei-group-table)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-nikkei)
					 &optional range)
  (let* ((group (shimbun-current-group-internal shimbun))
	 (fn (nth 3 (assoc group shimbun-nikkei-group-table)))
	 (shimbun-nikkei-from-address (shimbun-from-address shimbun))
	 (case-fold-search t))
    (while (search-forward "\r" nil t)
      (delete-backward-char 1))
    (goto-char (point-min))
    (when (fboundp fn)
      (funcall fn group (nth 2 (assoc group shimbun-nikkei-group-table))))))

(defun shimbun-nikkei-expand-url (url folder)
  "Make a fullname of URL relative to FOLDER.
If URL begins with `http://', FOLDER is ignored."
  (save-match-data
    (cond ((string-match "\\`http://" url)
	   url)
	  ((string-match "\\`/" url)
	   (concat folder (substring url 1)))
	  (t
	   (concat folder url)))))

(defun shimbun-nikkei-make-date-string (&rest args)
  "Run `shimbun-make-date-string' with ARGS and fix a day if needed.

\(shimbun-nikkei-make-date-string YEAR MONTH DAY &optional TIME TIMEZONE)"
  (save-match-data
    (let* ((ctime (current-time))
	   (date (apply 'shimbun-make-date-string args))
	   (time (shimbun-time-parse-string date))
	   (ms (car time))
	   (ls (cadr time))
	   (system-time-locale "C"))
      (if (or (> ms (car ctime))
	      (and (= ms (car ctime))
		   (> ls (cadr ctime))))
	  ;; It should be yesterday's same time.
	  (progn
	    (setq ms (1- ms))
	    (when (< (setq ls (- ls (eval-when-compile (- (* 60 60 24) 65536))))
		     0)
	      (setq ms (1- ms)
		    ls (+ ls 65536)))
	    (format-time-string "%a, %d %b %Y %R +0900" (list ms ls)))
	date))))

(defun shimbun-nikkei-get-headers-default (group folder &optional headers)
  "Default function used to fetch headers.
GROUP is a group name.  FOLDER is a parent url.
If HEADERS is non-nil, it is appended to newly fetched headers."
  (while (re-search-forward
	  (eval-when-compile
	    (let ((s0 "[\t\n ]*")
		  (s1 "[\t\n ]+"))
	      (concat "<a" s1 "href=\""
		      ;; 1. url
		      "\\(\\([^\"<>]+/\\)?"
		      ;; 3. serial number
		      "\\("
		      ;; 4. year
		      "\\(20[0-9][0-9]\\)"
		      ;; 5. month
		      "\\([01][0-9]\\)"
		      ;; 6. day
		      "\\([0-3][0-9]\\)"
		      "[0-9a-z]+\\)"
		      "\\.html\\)\"" s0 ">\\(" s0 "<[^<>]+>" s0 "\\)*" s0
		      ;; 8. subject
		      "\\([^<>]+\\)"
		      "\\(" s0 "<[^<>]+>" s0 "\\)*"
		      "("
		      ;; 10. hour:minute
		      "\\([012][0-9]:[0-5][0-9]\\)"
		      ")")))
	  nil t)
    (push (shimbun-create-header
	   0
	   (match-string 8)
	   shimbun-nikkei-from-address
	   (shimbun-nikkei-make-date-string
	    (string-to-number (match-string 4))
	    (string-to-number (match-string 5))
	    (string-to-number (match-string 6))
	    (match-string 10))
	   (concat "<" (match-string 3) "%" group "."
		   shimbun-nikkei-top-level-domain ">")
	   "" 0 0
	   (shimbun-nikkei-expand-url (match-string 1) folder))
	  headers))
  (shimbun-sort-headers headers))

(defun shimbun-nikkei-get-headers-top (group folder)
  "Function used to fetch headers for the `top' group."
  (let (headers)
    (when (re-search-forward
	   (eval-when-compile
	     (let ((s0 "[\t\n ]*")
		   (s1 "[\t\n ]+"))
	       (concat "<a" s1 "href=\"/?"
		       ;; 1. url
		       "\\([^\"]+/"
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
		       ".+>" s0
		       "<!--" s0 "FJZONE" s1 "START" s1 "NAME=\"MIDASHI\""
		       s0 "-->" s0
		       ;; 6. subject
		       "\\([^<]+\\)"
		       s0
		       "<!--" s0 "FJZONE" s1 "END" s1 "NAME=\"MIDASHI\""
		       s0 "-->\\(" s0 "<[^!]+>\\)*" s0
		       "<!--" s0 "FJZONE" s1 "START" s1 "NAME=\"HONBUN\""
		       s0 "-->[^<]+("
		       ;; 8. hour:minute
		       "\\([0-2][0-9]:[0-5][0-9]\\)")))
	   nil t)
      (push (shimbun-create-header
	     0
	     (match-string 6)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 5))
	      (match-string 8))
	     (concat "<" (match-string 2) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    (shimbun-nikkei-get-headers-default group folder headers)))

(defun shimbun-nikkei-get-headers-markets (group folder)
  "Function used to fetch headers for the markets groups."
  (let (cyear cmonth day month year url headers)
    (setq cyear (decode-time)
	  cmonth (nth 4 cyear)
	  cyear (nth 5 cyear))
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat ">" s0
			;; 1. month
			"\\([01][0-9]\\)"
			"/"
			;; 2. day
			"\\([0-3][0-9]\\)"
			s1
			;; 3. hour:minute
			"\\("
			;; 4. hour
			"\\([012][0-9]\\)"
			":"
			;; 5. minute
			"\\([0-5][0-9]\\)"
			"\\)\\(" s0 "\\(<[^a]\\|<a[^\t\n ]\\)[^>]+>\\)+"
			s0 "<a" s1 "href=\""
			;; 8. url
			"\\([^\"<>]+\\)"
			"\"" s0 ">\\(" s0 "<[^>]+>\\)*" s0
			;; 10. subject
			"\\([^<]+\\)")))
	    nil t)
      (setq day (string-to-number (match-string 2))
	    month (string-to-number (match-string 1))
	    year (cond ((>= (- month cmonth) 2)
			(1- cyear))
		       ((and (= 1 month) (= 12 cmonth))
			(1+ cyear))
		       (t
			cyear))
	    url (match-string 8))
      (push (shimbun-create-header
	     0
	     (match-string 10)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string year month day (match-string 3))
	     (format "<%d%02d%02d%s%s.%s%%%s.markets.%s>"
		     year month day (match-string 4) (match-string 5)
		     (save-match-data
		       (if (string-match "[&?]id=\\([^&]+\\)" url)
			   (match-string 1 url)
			 "no-id"))
		     group shimbun-nikkei-top-level-domain)
	     "" 0 0
	     (shimbun-nikkei-expand-url url folder))
	    headers))
    (shimbun-sort-headers headers)))

(defun shimbun-nikkei-get-headers-sports (group folder)
  "Function used to fetch headers for the sports group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. url
			"\\(\\./news\\.cfm\\?i="
			;; 2. serial number
			"\\("
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"[01][0-9][0-3][0-9][^\"]+\\)\\)"
			"\"[^>]+>" "\\(" s0 "<[^>]+>\\)*" s0 "(" s0
			;; 5. month
			"\\([0-9]+\\)"
			s0 "/" s0
			;; 6. day
			"\\([0-9]+\\)"
			s0 ")" s0
			;; 7. subject
			"\\([^<]+\\)")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 7)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6)))
	     (concat "<" (match-string 2) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-newpro (group folder)
  "Function used to fetch headers for the newpro group."
  (when (re-search-forward ">[\t\n ]*● 新製品記事一覧[\t\n ]*<" nil t)
    (narrow-to-region (point) (or (search-forward "</ul>" nil t)
				  (point-max)))
    (goto-char (point-min))
    (let (headers)
      (while (re-search-forward
	      (eval-when-compile
		(let ((s0 "[\t\n ]*")
		      (s1 "[\t\n ]+"))
		  (concat "<a" s1 "href=\""
			  ;; 1. url
			  "\\(\\([^\"]+/\\)?"
			  ;; 3. serial number
			  "\\("
			  ;; 4. year
			  "\\(20[0-9][0-9]\\)"
			  ;; 5. month
			  "\\([01][0-9]\\)"
			  ;; 6. day
			  "\\([0-3][0-9]\\)"
			  "[0-9a-z]+\\)"
			  "\\.html\\)"
			  "\"" s0 ">" s0
			  ;; 7. subject
			  "\\([^<]+\\)")))
	      nil t)
	(push (shimbun-create-header
	       0
	       (match-string 7)
	       shimbun-nikkei-from-address
	       (shimbun-nikkei-make-date-string
		(string-to-number (match-string 4))
		(string-to-number (match-string 5))
		(string-to-number (match-string 6)))
	       (concat "<" (match-string 3) "%" group "."
		       shimbun-nikkei-top-level-domain ">")
	       "" 0 0
	       (shimbun-nikkei-expand-url (match-string 1) folder))
	      headers))
      (widen)
      headers)))

(defun shimbun-nikkei-get-headers-release (group folder)
  "Function used to fetch headers for the release group."
  (let (url id subject sub-end year month day headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. url
			"\\(detail\\.cfm\\?relID="
			;; 2. serial number
			"\\([^\"]+\\)"
			"\\)\"" s0 ">" s0
			;; 3. subject
			"\\([^<]+\\)")))
	    nil t)
      (setq url (match-string 1)
	    id (match-string 2)
	    subject (match-string 3)
	    sub-end (point))
      (when (re-search-backward "\
>[\t\n ]*\\(20[0-9][0-9]\\)/\\([01][0-9]\\)/\\([0-3][0-9]\\)[^0-9]"
				nil t)
	(push (shimbun-create-header
	       0
	       subject
	       shimbun-nikkei-from-address
	       (shimbun-nikkei-make-date-string
		(setq year (string-to-number (match-string 1)))
		(setq month (string-to-number (match-string 2)))
		(setq day (string-to-number (match-string 3))))
	       (format "<%d%02d%02d.%s%%%s.%s>"
		       year month day id group shimbun-nikkei-top-level-domain)
	       "" 0 0
	       (shimbun-nikkei-expand-url url folder))
	      headers)
	(goto-char sub-end)))
    headers))

(defun shimbun-nikkei-get-headers-shasetsu (group folder)
  "Function used to fetch headers for the shasetsu group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. url
			"\\([^\"]+/"
			;; 2. serial number
			"\\("
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			;; 4. month
			"\\([01][0-9]\\)"
			;; 5. day
			"\\([0-3][0-9]\\)"
			"[^/]+\\)"
			"\\.html\\)"
			"\"" s0 ">" s0
			;; 6. subject
			"\\(\\(社説\\|春秋\\)[^<]+\\)")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 6)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 5)))
	     (concat "<" (match-string 2) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    (nreverse headers)))

(defun shimbun-nikkei-get-headers-okuyami (group folder)
  "Function used to fetch headers for the okuyami group."
  (when (re-search-forward ">[\t\n ]*▼おくやみ[\t\n ]*<" nil t)
    (let (headers)
      (while (re-search-forward
	      (eval-when-compile
		(let ((s0 "[\t\n ]*")
		      (s1 "[\t\n ]+"))
		  (concat "<a" s1 "href=\""
			  ;; 1. url
			  "\\([^\"]+/"
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
			  "\"" s0 ">" s0
			  ;; 6. subject
			  "\\([^<]+\\)")))
	      nil t)
	(push (shimbun-create-header
	       0
	       (match-string 6)
	       shimbun-nikkei-from-address
	       (shimbun-nikkei-make-date-string
		(string-to-number (match-string 3))
		(string-to-number (match-string 4))
		(string-to-number (match-string 5)))
	       (concat "<" (match-string 2) "%" group "."
		       shimbun-nikkei-top-level-domain ">")
	       "" 0 0
	       (shimbun-nikkei-expand-url (match-string 1) folder))
	      headers))
      headers)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-nikkei)
						   header)
  (let ((fn (nth 4 (assoc (shimbun-current-group-internal shimbun)
			  shimbun-nikkei-group-table)))
	(case-fold-search t))
    (while (search-forward "\r" nil t)
      (delete-backward-char 1))
    (goto-char (point-min))
    (when (fboundp fn)
      (funcall fn)
      (goto-char (point-min)))))

(defun shimbun-nikkei-prepare-article-default ()
  "Default function used to prepare contents of an article."
  (let (photo-end body)
    (when (re-search-forward "<table[\t\n ]+id=\"photonews" nil t)
      (delete-region (point-min) (match-beginning 0))
      (when (search-forward "</table>" nil t)
	(setq photo-end (point))))
    (when (or (and (re-search-forward "\
<!--[\n\t ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]+-->[\t\n ]*"
				      nil t)
		   (setq body (point))
		   (re-search-forward "\
\[\t\n ]*<!--[\n\t ]*FJZONE[\t\n ]*END[\t\n ]*NAME=\"HONBUN\""
				      nil t))
	      (and (re-search-forward "\
<!--[\n\t ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"MIDASHI\""
				      nil t)
		   (search-forward "<p>" nil t)
		   (setq body (match-beginning 0))
		   (re-search-forward "<p[^>]\\|\n\n+" nil t)))
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end)
      (if photo-end
	  (progn
	    (delete-region photo-end body)
	    ;; Replace <img src='...'> with <img src="...">.
	    (goto-char (point-min))
	    (while (re-search-forward "<img[\t\n ]+src='\\([^\"']+\\)'"
				      nil t)
	      (replace-match "<img src=\"\\1\""))
	    (goto-char (point-min)))
	(goto-char body))
      (insert shimbun-nikkei-content-start))))

(defun shimbun-nikkei-prepare-article-sports ()
  "Function used to prepare contents of an article for the sports group."
  (when (re-search-forward "\
<!--[\n\t ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"MIDASHI\"[\t\n ]*-->"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "\
<!--[\n\t ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-->"
			     nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end))))

(defun shimbun-nikkei-prepare-article-newpro ()
  "Function used to prepare contents of an article for the newpro group."
  (let (photo-end body)
    (when (re-search-forward "<table[\t\n ]+id=\"photonews" nil t)
      (delete-region (point-min) (match-beginning 0))
      (when (search-forward "</table>" nil t)
	(setq photo-end (point))))
    (when (and (re-search-forward "\
<!--[\n\t ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]+-->[\t\n ]*"
				  nil t)
	       (setq body (point))
	       (re-search-forward "\
\[\t\n ]*<!--[\n\t ]*FJZONE[\t\n ]*END[\t\n ]*NAME=\"HONBUN\""
				  nil t))
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end)
      (if photo-end
	  (progn
	    (delete-region photo-end body)
	    (goto-char (point-min)))
	(goto-char body))
      (insert shimbun-nikkei-content-start))))

(defun shimbun-nikkei-prepare-article-release ()
  "Function used to prepare contents of an article for the release group."
  (when (re-search-forward "<span[\t\n ]+class=\"midasi\">[\t\n ]*" nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "\\([\t\n ]*<[^>]+>\\)*[\t\n ]*</span>" nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end))))

(defun shimbun-nikkei-prepare-article-markets ()
  "Function used to prepare contents of an article for the markets groups."
  (when (re-search-forward
	 "<DIV[\t\n ]+ID=\"topic\"[^>]+>[\t\n ]*\
\\(<\\([^/]\\|/[^D]\\|/D[^I]\\|/DI[^V]\\|/DIV[^>]\\)[^>]*>[\t\n ]*\\)*"
	 nil t)
    (insert shimbun-nikkei-content-start)
    (let ((pt (point)))
      (when (re-search-forward "[\t\n ]*\\(<[^>]+>[\t\n ]*\\)*</DIV>" nil t)
	(goto-char (match-beginning 0))
	(when (= pt (point))
	  (insert "This article seems to have been expired in the server."))
	(insert shimbun-nikkei-content-end)))))

(defun shimbun-nikkei-prepare-article-okuyami ()
  "Function used to prepare contents of an article for the okuyami group."
  (when (re-search-forward "\
<!--[\n\t ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-->"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "\
<!--[\n\t ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-->"
			     nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end))))

(provide 'sb-nikkei)

;;; sb-nikkei.el ends here

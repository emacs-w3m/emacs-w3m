;;; sb-nikkei.el --- shimbun backend for nikkei.co.jp -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005
;; Kazuyoshi KOREEDA <Kazuyoshi.Koreeda@rdmg.mgcs.mei.co.jp>

;; Author: Kazuyoshi KOREEDA <Kazuyoshi.Koreeda@rdmg.mgcs.mei.co.jp>,
;;         Katsumi Yamaoka   <yamaoka@jpl.org>,
;;         NOMIYA Masaru     <nomiya@ttmy.ne.jp>
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
    ("tento" "ベンチャー" ,(concat shimbun-nikkei-url "news/tento/")
     shimbun-nikkei-get-headers-default2
     shimbun-nikkei-prepare-article-default2)
    ("zinzi" "トップ人事" ,(concat shimbun-nikkei-url "news/zinzi/")
     shimbun-nikkei-get-headers-default2
     shimbun-nikkei-prepare-article-default2)
    ("report" "日経の調査" ,(concat shimbun-nikkei-url "report/")
     shimbun-nikkei-get-headers-report
     shimbun-nikkei-prepare-article-report)
    ("kansai" "関西" ,(concat shimbun-nikkei-url "kansai/")
     shimbun-nikkei-get-headers-kansai
     shimbun-nikkei-prepare-article-kansai)
    ("it" "IT" "http://it.nikkei.co.jp/it/news/"
     shimbun-nikkei-get-headers-it
     shimbun-nikkei-prepare-article-default2)
    ("it.zensen" "IT最前線"
     "http://it.nikkei.co.jp/it/column/zensen.cfm?ichiran=true"
     shimbun-nikkei-get-headers-it-zensen
     shimbun-nikkei-prepare-article-default)
    ("it.manage" "ITニュースの焦点"
     "http://it.nikkei.co.jp/it/manage/foc.cfm?ichiran=true"
     shimbun-nikkei-get-headers-it-manage
     shimbun-nikkei-prepare-article-default)
    ("it.seisaku" "IT政策" "http://it.nikkei.co.jp/it/news/seisaku.cfm"
     shimbun-nikkei-get-headers-it-seisaku
     shimbun-nikkei-prepare-article-default)
    ("it.digicore" "デジタルコア・レポート"
     "http://it.nikkei.co.jp/it/column/digicore.cfm?ichiran=true"
     shimbun-nikkei-get-headers-it-digicore
     shimbun-nikkei-prepare-article-default)
    ("kokunai" "市場概況" "http://markets.nikkei.co.jp/kokunai/summary.cfm"
     shimbun-nikkei-get-headers-kawase
     shimbun-nikkei-prepare-article-default3)
    ("markets" "海外株概況" "http://markets.nikkei.co.jp/kaigai/summary.cfm"
     shimbun-nikkei-get-headers-markets
     shimbun-nikkei-prepare-article-default3)
    ("kawase" "為替概況" "http://markets.nikkei.co.jp/kawase/summary.cfm"
     shimbun-nikkei-get-headers-kawase
     shimbun-nikkei-prepare-article-default3)
    ("kinri" "短期金利・債権・ＣＢ概況"
     "http://markets.nikkei.co.jp/kawase/kinri.cfm"
     shimbun-nikkei-get-headers-kinri
     shimbun-nikkei-prepare-article-default3)
    ("ft" "英フィナンシャル・タイムズ"
     "http://markets.nikkei.co.jp/kaigai/ft.cfm"
     shimbun-nikkei-get-headers-ft
     shimbun-nikkei-prepare-article-default3)
    ("dj" "米ダウ・ジョーンズ" "http://markets.nikkei.co.jp/kaigai/dj.cfm"
     shimbun-nikkei-get-headers-dj
     shimbun-nikkei-prepare-article-default3)
    ("ngyoseki" "企業業績ニュース"
     "http://markets.nikkei.co.jp/kokunai/gyoseki.cfm"
     shimbun-nikkei-get-headers-gyoseki
     shimbun-nikkei-prepare-article-default3)
    ("gyosuuchi" "業績数値"
     "http://markets.nikkei.co.jp/kokunai/bunkatsu3.cfm?genre=m4"
     shimbun-nikkei-get-headers-bunkatsu2
     shimbun-nikkei-prepare-article-bunkatsu2)
    ("gyoseki" "海外企業業績" "http://markets.nikkei.co.jp/kaigai/gyoseki.cfm"
     shimbun-nikkei-get-headers-gyoseki
     shimbun-nikkei-prepare-article-default3)
    ("market" "株・為替" ,(concat shimbun-nikkei-url "news/market/")
     shimbun-nikkei-get-headers-market
     shimbun-nikkei-prepare-article-market)
    ("kaigai" "国際" ,(concat shimbun-nikkei-url "news/kaigai/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("seiji" "政治" ,(concat shimbun-nikkei-url "news/seiji/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("shakai" "社会" ,(concat shimbun-nikkei-url "news/shakai/")
     shimbun-nikkei-get-headers-default
     shimbun-nikkei-prepare-article-default)
    ("retto" "地域経済" ,(concat shimbun-nikkei-url "news/retto/")
     shimbun-nikkei-get-headers-retto
     shimbun-nikkei-prepare-article-default2)
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
	    (when (< (setq ls (- ls (eval-when-compile
				      (- (* 60 60 24) 65536))))
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

(defun shimbun-nikkei-get-headers-default2 (group folder)
  "Function used to fetch headers for the tento and the zinzi groups."
  (let (headers)
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
			"\\.html\\)\"" s0 "\\(>\\)" s0
			;; 8. subject
			"\\([^<>]+\\)")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 8)
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
    (shimbun-sort-headers headers)))

(defun shimbun-nikkei-get-headers-report (group folder)
  "Function used to fetch headers for the report group."
  (let ((date (if (re-search-forward
		   (eval-when-compile
		     (let ((s0 "[\t\n ]*")
			   (s1 "[\t\n ]+"))
		       (concat "<p" s1 "id=\"title_description\">"
			       s0 "更新" s0 "[:：]" s0
			       ;; 1. year
			       "\\(20[0-9][0-9]\\)"
			       "/"
			       ;; 2. month
			       "\\([01]?[0-9]\\)"
			       "/"
			       ;; 3. day
			       "\\([0-3]?[0-9]\\)"
			       s0 "</p>")))
		   nil t)
		  (prog1
		      (shimbun-make-date-string
		       (string-to-number (match-string 1))
		       (string-to-number (match-string 2))
		       (string-to-number (match-string 3)))
		    (goto-char (point-min)))
		(let ((cts (current-time-string)))
		  (format "%s, %02d %s %s 00:00 +0900"
			  (substring cts 0 3)
			  (string-to-number (substring cts 8 10))
			  (substring cts 4 7)
			  (substring cts 20)))))
	headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"/report/"
			;; 1. url
			"\\("
			;; 2. serial number
			"\\([^\t\n ]+\\)"
			"\\.html\\)"
			s0 "\">" s0
			;; 3. subject
			"\\([^<]+\\)"
			s0 "<")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 3)
	     shimbun-nikkei-from-address
	     date
	     (concat "<" (match-string 2) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-kansai (group folder)
  "Function used to fetch headers for the kansai group."
  (let ((date (if (re-search-forward
		   (eval-when-compile
		     (let ((s0 "[\t\n ]*")
			   (s1 "[\t\n ]+"))
		       (concat "class=\"date\"><strong>" s0
			       ;; 1. year
			       "\\(20[0-9][0-9]\\)"
			       "年"
			       ;; 2. month
			       "\\([01]?[0-9]\\)"
			       "月"
			       ;; 3. day
			       "\\([0-3]?[0-9]\\)"
			       "日" s0 "(" "\\([^<]+\\)" ")"
			       s0 "</strong></td>")))
		   nil t)
		  (prog1
		      (shimbun-make-date-string
		       (string-to-number (match-string 1))
		       (string-to-number (match-string 2))
		       (string-to-number (match-string 3)))
		    (goto-char (point-min)))
		(let ((cts (current-time-string)))
		  (format "%s, %02d %s %s 00:00 +0900"
			  (substring cts 0 3)
			  (string-to-number (substring cts 8 10))
			  (substring cts 4 7)
			  (substring cts 20)))))
	headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"."
			;; 1. url
			"\\("
			;; 2.
			"\\([^\"<>]+/\\)"
			;; 3. serial number
			"\\([^\t\n ]+\\)\\)"
			s0 "-frame" s0 "\\.html"
			s0 "\">" s0
			;; 4. subject
			"\\([^<]+\\)"
			s0 "<")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 4)
	     shimbun-nikkei-from-address
	     date
	     (concat "<" (match-string 3) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (concat (match-string 1) ".html")
					folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-it (group folder)
  "Function used to fetch headers for the it group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. url
			"\\(\\./index\\.cfm\\?i="
			;; 2. serial number
			"\\("
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			;; 4. month
			"\\([01][0-9]\\)"
			;; 5. day
			"\\([0-3][0-9]\\)"
			;; 6. serial
			"\\([0-9a-z]+\\)\\)\\)"
			;;"\"\\)"
			s0 "\">" s0
			;; 7. subject
			"\\([^<]+\\)" "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 7)
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
    headers))

(defun shimbun-nikkei-get-headers-it-zensen (group folder)
  "Function used to fetch headers for the it.zensen group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. url
			"\\(\\./zensen\\.cfm"
			"\\(\\?i="
			;; 3. serial number
			"\\("
			;; 4. year
			"\\(20[0-9][0-9]\\)"
			;; 5. month
			"\\([01][0-9]\\)"
			;; 6. day
			"\\([0-3][0-9]\\)"
			;; 7. serial
			"\\([0-9a-z]+\\)\\)\\)\\)"
			;;"\"\\)"
			"\"" s0 ">" s0 "\\(([01]?[0-9]/[0-3]?[0-9])\\)?" s0
			;; 9. subject
			"\\([^<]+\\)" "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 9)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6)))
	     (concat "<" (match-string 3) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url
	      (concat "http://it.nikkei.co.jp/it/column/zensen.cfm"
		      (match-string 2)) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-it-manage (group folder)
  "Function used to fetch headers for the it.manage group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. url
			"\\(\\./foc\\.cfm"
			"\\(\\?i="
			;; 3. serial number
			"\\("
			;; 4. year
			"\\(20[0-9][0-9]\\)"
			;; 5. month
			"\\([01][0-9]\\)"
			;; 6. day
			"\\([0-3][0-9]\\)"
			;; 7. serial
			"\\([0-9a-z]+\\)\\)\\)\\)"
			;;"\"\\)"
			"\"" s0 ">" s0
			;; 8. subject
			"\\([^<]+\\)" "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 8)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6)))
	     (concat "<" (match-string 3) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url
	      (concat "http://it.nikkei.co.jp/it/manage/foc.cfm"
		      (match-string 2)) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-it-seisaku (group folder)
  "Function used to fetch headers for the it.seisaku group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. url
			"\\(\\./seisaku\\.cfm"
			"\\(\\?i="
			;; 3. serial number
			"\\("
			;; 4. year
			"\\(20[0-9][0-9]\\)"
			;; 5. month
			"\\([01][0-9]\\)"
			;; 6. day
			"\\([0-3][0-9]\\)"
			;; 7. serial
			"\\([0-9a-z]+\\)\\)\\)\\)"
			;;"\"\\)"
			"\"" s0 ">" s0
			;; 8. subject
			"\\([^<]+\\)" "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 8)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6)))
	     (concat "<" (match-string 3) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 2) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-it-digicore (group folder)
  "Function used to fetch headers for the it.digicore group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. url
			"\\(./digicore\\.cfm"
			"\\(\\?i="
			;; 3. serial number
			"\\("
			;; 4. year
			"\\(20[0-9][0-9]\\)"
			;; 5. month
			"\\([01][0-9]\\)"
			;; 6. day
			"\\([0-3][0-9]\\)"
			;; 7. serial
			"\\([0-9a-z]+\\)\\)\\)\\)"
			;;"\"\\)"
			"\"" s0 ">" s0 "\\(([01]?[0-9]/[0-3]?[0-9])\\)?" s0
			;; 9. subject
			"\\([^<]+\\)" "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 9)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6)))
	     (concat "<" (match-string 3) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url
	      (concat "http://it.nikkei.co.jp/it/column/digicore.cfm"
		      (match-string 2)) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-markets (group folder)
  "Function used to fetch headers for the markets group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. base
			"\\(summary\\.cfm"
			;; 2. url
			"\\(\\?genre="
			;; 3. serial number
			"\\([^\"]+date=\\)"
			;; 4. year
			"\\(20[0-9][0-9]\\)"
			;; 5. serial number
			"\\([01][0-9][0-3][0-9]\\)\\)\\)"
			"\"" s0 ">" s0
			;; 6. subject
			"\\([^<]+\\)" s0 "（"
			;; 7. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 8. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 9. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 10. minute
			"\\([0-5]?[0-9]\\)"
			"）" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 6)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 7))
	      (string-to-number (match-string 8))
	      (format "%02d:%02d"
		      (string-to-number (match-string 9))
		      (string-to-number (match-string 10))))
	     (concat "<" (match-string 3) (match-string 4) (match-string 5)
		     "%" group "." shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 2) folder))
	    headers))
    (shimbun-sort-headers headers)))

(defun shimbun-nikkei-get-headers-kawase (group folder)
  "Function used to fetch headers for the kawase group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. base
			"\\(summary\\.cfm"
			;; 2. url
			"\\(\\?id="
			;; 3. serial number
			"\\([^\"]+date=\\)"
			;; 4. year
			"\\(20[0-9][0-9]\\)"
			;; 5. serial number
			"\\([01][0-9][0-3][0-9]\\)\\)\\)"
			"\"" s0 ">" s0
			;; 6. subject
			"\\([^<]+\\)" s0 "（"
			;; 7. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 8. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 9. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 10. minute
			"\\([0-5]?[0-9]\\)"
			"）" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 6)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 7))
	      (string-to-number (match-string 8))
	      (format "%02d:%02d"
		      (string-to-number (match-string 9))
		      (string-to-number (match-string 10))))
	     (concat "<" (match-string 3) (match-string 4) (match-string 5)
		     "%" group "." shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 2) folder))
	    headers))
    (shimbun-sort-headers headers)))

(defun shimbun-nikkei-get-headers-bunkatsu2 (group folder)
  "Function used to fetch headers for the kawase group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. base
			"\\(bunkatsu3\\.cfm\\?genre=m4"
			;; 2. url
			"\\(&id="
			;; 3. serial number
			"\\([^\"]+date=\\)"
			;; 4. year
			"\\(20[0-9][0-9]\\)"
			;; 5. serial number
			"\\([01][0-9][0-3][0-9]"
			;; 6. serial number
			"\\([^\"]+\\)\\)\\)\\)"
			"\"" s0 ">" s0
			;; 7. subject
			"\\([^<]+\\)" s0 "（"
			;; 8. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 9. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 10. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 11. minute
			"\\([0-5]?[0-9]\\)"
			"）" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 7)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 8))
	      (string-to-number (match-string 9))
	      (format "%02d:%02d"
		      (string-to-number (match-string 10))
		      (string-to-number (match-string 11))))
	     (concat "<" (match-string 3) (match-string 4) (match-string 5)
		     "%" group "." shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 2) folder))
	    headers))
    (shimbun-sort-headers headers)))

(defun shimbun-nikkei-get-headers-kinri (group folder)
  "Function used to fetch headers for the kinri group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. base
			"\\(kinri\\.cfm"
			;; 2. url
			"\\(\\?id="
			;; 3. serial number
			"\\([^\"]+date=\\)"
			;; 4. year
			"\\(20[0-9][0-9]\\)"
			;; 5. serial number
			"\\([01][0-9][0-3][0-9]\\)\\)\\)"
			"\"" s0 ">" s0
			;; 6. subject
			"\\([^<]+\\)" s0 "（"
			;; 7. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 8. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 9. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 10. minute
			"\\([0-5]?[0-9]\\)"
			"）" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 6)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 7))
	      (string-to-number (match-string 8))
	      (format "%02d:%02d"
		      (string-to-number (match-string 9))
		      (string-to-number (match-string 10))))
	     (concat "<" (match-string 3) (match-string 4) (match-string 5)
		     "%" group "." shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 2) folder))
	    headers))
    (shimbun-sort-headers headers)))

(defun shimbun-nikkei-get-headers-ft (group folder)
  "Function used to fetch headers for the ft group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. base
			"\\(ft\\.cfm"
			;; 2. url
			"\\(\\?id="
			;; 3. serial number
			"\\([^\"]+date=\\)"
			;; 4. year
			"\\(20[0-9][0-9]\\)"
			;; 5. serial number
			"\\([01][0-9][0-3][0-9]\\)\\)\\)"
			"\"" s0 ">" s0
			;; 6. subject
			"\\([^<]+\\)"
			s0 "\\([(|（]\\)"
			;; 8. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 9. day
			"\\([0-3]?[0-9]\\)"
			"\\([）|)]\\)" s0 "※" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 6)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 8))
	      (string-to-number (match-string 9)))
;;	      (match-string 9))
	     (concat "<" (match-string 3) (match-string 4) (match-string 5)
		     "%" group "." shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 2) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-dj (group folder)
  "Function used to fetch headers for the dj group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. base
			"\\(dj\\.cfm"
			;; 2. url
			"\\(\\?id="
			;; 3. serial number
			"\\([^\"]+date=\\)"
			;; 4. year
			"\\(20[0-9][0-9]\\)"
			;; 5. serial number
			"\\([01][0-9][0-3][0-9]\\)\\)\\)"
			"\"" s0 ">" s0
			;; 6. subject
			"\\([^<]+\\)" s0 "（"
			;; 7. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 8. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 9. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 10. minute
			"\\([0-5]?[0-9]\\)"
			"）" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 6)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 7))
	      (string-to-number (match-string 8))
	      (format "%02d:%02d"
		      (string-to-number (match-string 9))
		      (string-to-number (match-string 10))))
	     (concat "<" (match-string 3) (match-string 4) (match-string 5)
		     "%" group "." shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 2) folder))
	    headers))
    (shimbun-sort-headers headers)))

(defun shimbun-nikkei-get-headers-gyoseki (group folder)
  "Function used to fetch headers for the gyoseki group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
			;; 1. base
			"\\(gyoseki\\.cfm"
			;; 2. url
			"\\(\\?id="
			;; 3. serial number
			"\\([^\"]+date=\\)"
			;; 4. year
			"\\(20[0-9][0-9]\\)"
			;; 5. serial number
			"\\([01][0-9][0-3][0-9]\\)\\)\\)"
			"\"" s0 ">" s0
			;; 6. subject
			"\\([^<]+\\)" s0 "（"
			;; 7. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 8. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 9. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 10. minute
			"\\([0-5]?[0-9]\\)"
			"）" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 6)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 7))
	      (string-to-number (match-string 8))
	      (format "%02d:%02d"
		      (string-to-number (match-string 9))
		      (string-to-number (match-string 10))))
	     (concat "<" (match-string 3) (match-string 4) (match-string 5)
		     "%" group "." shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 2) folder))
	    headers))
    (shimbun-sort-headers headers)))

(defun shimbun-nikkei-get-headers-market (group folder)
  "Function used to fetch headers for the market group."
  (let ((subregexp
	 (eval-when-compile
	   (let ((s0 "[\t\n ]*")
		 (s1 "[\t\n ]+"))
	     (concat "class=\"sub_bar\"" s0 ">" s0
		     ;; 1. subtitle
		     "\\([^\t\n <]+\\)"
		     ".+class=\"sub_bar_time\"" s0 ">" s0
		     "更新" s0 "：" s0
		     ;; 2. month
		     "\\([01]?[0-9]\\)"
		     "月"
		     ;; 3. day
		     "\\([0-3]?[0-9]\\)"
		     "日\\(" s1
		     ;; 5. hour:minute
		     "\\([012]?[0-9]:[0-5]?[0-9]\\)"
		     "\\)?"))))
	subdata start end subtitle month day time from year headers)
    (when (re-search-forward subregexp nil t)
      (setq subdata (copy-sequence (match-data))
	    start (point))
      (while start
	(if (re-search-forward subregexp nil t)
	    (progn
	      (setq subdata (prog1
				(copy-sequence (match-data))
			      (set-match-data subdata))
		    end (point))
	      (goto-char start))
	  (set-match-data subdata)
	  (setq end nil))
	(setq subtitle (match-string 1)
	      month (string-to-number (match-string 2))
	      day (string-to-number (match-string 3))
	      time (match-string 5))
	(setq from (shimbun-replace-in-string
		    shimbun-nikkei-from-address
		    ")" (concat "/"
				(shimbun-replace-in-string
				 subtitle "\\(&nbsp;\\)+" "")
				")")))
	(while (re-search-forward
		(eval-when-compile
		  (let ((s0 "[\t\n ]*")
			(s1 "[\t\n ]+"))
		    (concat "<a" s1 "href=\""
			    ;; 1. url
			    "\\([^\">]+/"
			    ;; 2. id
			    "\\("
			    ;; 3. year
			    "\\(20[0-9][0-9]\\)"
			    "\\([^.]+\\)"
			    "\\)"
			    "\\.html\\)\""
			    s0 ">\\(" s0 "<[^>]+>\\)*" s0
			    ;; 6. subject

			    "\\([^<]+\\)"
			    s0)))
		end t)
	  (setq year (string-to-number (match-string 3)))
	  (push (shimbun-create-header
		 0
		 (match-string 6)
		 from
		 (shimbun-nikkei-make-date-string year month day time)
		 (format "<%s%%%s.%s>"
			 (match-string 2) group
			 shimbun-nikkei-top-level-domain)
		 "" 0 0
		 (shimbun-nikkei-expand-url (match-string 1)
					    shimbun-nikkei-url))
		headers))
	(setq start end))
      (shimbun-sort-headers headers))))

(defun shimbun-nikkei-get-headers-retto (group folder)
  "Function used to fetch headers for the retto group."
  (let ((region "")
	headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<p[^>]*>" s0 "【"
			;; 1. region
			"\\([^\t\n ]+\\)"
			"】" s0 "</p>"
			"\\|"
			"<AREA21" s1 "HEADLINE=\""
			;; 2. subject
			"\\([^\"]+\\)"
			"\"" s1 "URL=\""
			;; 3. url
			"\\("
			;; 4. serial number
			"\\(20[0-9][0-9][01][0-9][0-9a-z]+\\)"
			"\\.html\\)"
			"\"" s1 "ARTICLE_TIME=\""
			;; 5. year
			"\\(20[0-9][0-9]\\)" "/"
			;; 6. month
			"\\([01][0-9]\\)" "/"
			;; 7. day
			"\\([0-3][0-9]\\)" s1
			;; 8. hour:minute
			"\\([012][0-9]:[0-5][0-9]\\)")))
	    nil t)
      (if (match-beginning 1)
	  (setq region (match-string 1))
	(push (shimbun-create-header
	       0
	       (concat "[" region "] " (match-string 2))
	       shimbun-nikkei-from-address
	       (shimbun-nikkei-make-date-string
		(string-to-number (match-string 5))
		(string-to-number (match-string 6))
		(string-to-number (match-string 7))
		(match-string 8))
	       (concat "<" (match-string 4) "%" group "."
		       shimbun-nikkei-top-level-domain ">")
	       "" 0 0
	       (shimbun-nikkei-expand-url (match-string 3) folder))
	      headers)))
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
      (funcall fn header)
      (goto-char (point-min)))))

(defun shimbun-nikkei-prepare-article-default (&rest args)
  "Default function used to prepare contents of an article."
  (let (photo-end body)
    (when (re-search-forward "<table[\t\n ]+id=\"photonews" nil t)
      (delete-region (point-min) (match-beginning 0))
      (when (search-forward "</table>" nil t)
	(setq photo-end (point))))
    (when (or (and (re-search-forward "\
<!--[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]+-->[\t\n ]*"
				      nil t)
		   (setq body (point))
		   (re-search-forward "\
\[\t\n ]*<!--[\t\n ]*FJZONE[\t\n ]*END[\t\n ]*NAME=\"HONBUN\""
				      nil t))
	      ;; The following section will be used for the `main' group.
	      (and (re-search-forward "\
<!--[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"MIDASHI\""
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

(defun shimbun-nikkei-prepare-article-default2 (&rest args)
  "Function used to prepare contents of an article for some groups."
  (when (re-search-forward "\
<!--[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]+-->"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "\
<!--[\t\n ]*FJZONE[\t\n ]*END[\t\n ]*NAME=\"HONBUN\"[\t\n ]+-->"
			     nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end))))

(defun shimbun-nikkei-prepare-article-kansai (&rest args)
  "Function used to prepare contents of an article for some groups."
  (when (re-search-forward "\
<td[\t\n ]+colspan=\"2\"[\t\n ]+class=\"textm\">"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "\
<table[\t\n ]+border=\"0\"[\t\n ]+cellspacing=\
\"0\"[\t\n ]+cellpadding=\"0\"[\t\n ]+width=\"100%\">"
			     nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end))))

(defun shimbun-nikkei-prepare-article-default3 (&rest args)
  "Function used to prepare contents of an article for some groups."
  (when (re-search-forward "\
\[\t\n ]<div[\t\n ]+class=\"news\"[\t\n ]+id=\"report\">"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "[\t\n ]<ul[\t\n ]+id=\"tool\">" nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end))))

(defun shimbun-nikkei-prepare-article-bunkatsu2 (&rest args)
  "Function used to prepare contents of an article for some groups."
  (when (re-search-forward "[\t\n ]<div[\t\n ]+class=\"bg_gray\">" nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "[\t\n ]<div[\t\n ]+class=\"column\">" nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end))))

(defun shimbun-nikkei-prepare-article-sports (&rest args)
  "Function used to prepare contents of an article for the sports group."
  (when (re-search-forward "\
<!--[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"MIDASHI\"[\t\n ]*-->"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "\
<!--[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-->"
			     nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end))))

(defun shimbun-nikkei-prepare-article-newpro (&rest args)
  "Function used to prepare contents of an article for the newpro group."
  (let (photo-end body)
    (when (re-search-forward "<table[\t\n ]+id=\"photonews" nil t)
      (delete-region (point-min) (match-beginning 0))
      (when (search-forward "</table>" nil t)
	(setq photo-end (point))))
    (when (and (re-search-forward "\
<!--[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]+-->[\t\n ]*"
				  nil t)
	       (setq body (point))
	       (re-search-forward "\
\[\t\n ]*<!--[\t\n ]*FJZONE[\t\n ]*END[\t\n ]*NAME=\"HONBUN\""
				  nil t))
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end)
      (if photo-end
	  (progn
	    (delete-region photo-end body)
	    (goto-char (point-min)))
	(goto-char body))
      (insert shimbun-nikkei-content-start))))

(defun shimbun-nikkei-prepare-article-release (&rest args)
  "Function used to prepare contents of an article for the release group."
  (shimbun-remove-tags "<p[\t\n ]+class=\"re_print\"" "</p>")
  (goto-char (point-min))
  (when (re-search-forward "<[\t\n ]*TD[\t\n ]+colspan=\"3\">" nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "[\t\n ]*<div[\t\n ]+class=\"tokushu\">" nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end))))

(defun shimbun-nikkei-prepare-article-market (header)
  "Function used to prepare contents of an article for the market group."
  (when (re-search-forward "\
<!--[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-->"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "\\((\\([012]?[0-9]:[0-5]?[0-9]\\))[\t\n ]*\\)?\
<!--[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-->"
			     nil t)
      (if (match-beginning 2)
	  (progn
	    (goto-char (1+ (match-end 2)))
	    (let ((new (match-string 2))
		  (date (shimbun-header-date header)))
	      (when (string-match "[012]?[0-9]:[0-5]?[0-9]" date)
		(shimbun-header-set-date
		 header (replace-match new nil nil date)))))
	(goto-char (match-beginning 0)))
      (insert shimbun-nikkei-content-end))))

(defun shimbun-nikkei-prepare-article-okuyami (&rest args)
  "Function used to prepare contents of an article for the okuyami group."
  (when (re-search-forward "\
<!--[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-->"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "\
<!--[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-->"
			     nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end))))

(eval-and-compile
  (autoload 'japanese-hankaku "japan-util"))

(defun shimbun-nikkei-prepare-article-report (header)
  "Function used to prepare contents of an article for the report group."
  (when (re-search-forward "\
<!--[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]+-->"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (let ((start (point)))
      (when (re-search-forward "\
<!--[\t\n ]*FJZONE[\t\n ]*END[\t\n ]*NAME=\"HONBUN\"[\t\n ]+-->"
			       nil t)
	(goto-char (match-beginning 0))
	(insert shimbun-nikkei-content-end)
	(when (and (re-search-backward "\
\\([2２][0０][0-9０-９][0-9０-９]\\)年\
\\([01０１]?[0-9０-９]\\)月\
\\([0-3０-３]?[0-9０-９]\\)日"
				       start t)
		   (or (memq (char-before (match-beginning 0))
			     '(?\[ ?\〔 ?\［))
		       (eq (char-after (match-end 0)) ?\〕)))
	  ;; Note: `japanese-hankaku' breaks `match-data'.
	  (let ((year (match-string 1))
		(month (match-string 2))
		(day (match-string 3)))
	    (shimbun-header-set-date
	     header
	     (shimbun-make-date-string
	      (string-to-number (japanese-hankaku year))
	      (string-to-number (japanese-hankaku month))
	      (string-to-number (japanese-hankaku day))))))))))

(provide 'sb-nikkei)

;;; sb-nikkei.el ends here

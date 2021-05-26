;;; sb-asahi.el --- shimbun backend for asahi.com

;; Copyright (C) 2001-2011, 2013-2021 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>,
;;         Katsumi Yamaoka    <yamaoka@jpl.org>,
;;         NOMIYA Masaru      <nomiya@ttmy.ne.jp>
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(require 'shimbun)
(require 'sb-multi)

(luna-define-class shimbun-asahi (shimbun-japanese-newspaper shimbun-multi
							     shimbun) ())

(defvar shimbun-asahi-prefer-text-plain t
  "*Non-nil means prefer text/plain articles rather than html articles.")

(defvar shimbun-asahi-top-level-domain "asahi.com"
  "Name of the top level domain for the Asahi shimbun.")

(defvar shimbun-asahi-url
  (concat "http://www." shimbun-asahi-top-level-domain "/")
  "Name of the parent url.")

(defun shimbun-asahi-make-regexp (name)
  "Return a list of a regexp and numbers for the group NAME.
Every `.' in NAME will be replaced with `/'."
  (list (let ((s0 "[\t\n 　]*")
	      (s1 "[\t\n ]+")
	      (no-nl "[^\n<>]+"))
	  (concat "<a" s1 "href=\"/"
		  ;; 1. url
		  "\\(" (subst-char-in-string ?. ?/ name) "/"
		  ;; 2. serial number
		  "\\([a-z]*"
		  ;; 3. year
		  "\\(20[0-9][0-9]\\)"
		  ;; 4. month
		  "\\([01][0-9]\\)"
		  ;; 5. day
		  "\\([0-3][0-9]\\)"
		  "[0-9]+\\)"
		  "\\.html\\)"
		  "\">" s0
		  ;; 6. subject
		  "\\(" no-nl "\\)"
		  s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>"))
	1 nil 2 6 3 4 5))

(defvar shimbun-asahi-group-table
  (let* ((s0 "[\t\n 　]*")
	 (s1 "[\t\n ]+")
	 (no-nl "[^\n<>]+")
	 (default (list
		   (concat
		    "<a" s1 "href=\"/"
		    ;; 1. url
		    "\\(%s/update/"
		    ;; 2. month
		    "\\([01][0-9]\\)"
		    ;; 3. day
		    "\\([0-3][0-9]\\)"
		    "/"
		    ;; 4. serial number
		    "\\([a-z]*[0-9]+\\)"
		    "\\.html\\)"
		    "\">" s0
		    ;; 5. subject
		    "\\(" no-nl "\\)"
		    s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>")
		   1 4 nil 5 nil 2 3))
	 (default2 (shimbun-asahi-make-regexp "%s"))
	 (book (list
		(concat
		 "<a" s1 "href=\"/"
		 ;; 1. url
		 "\\(%s/"
		 ;; 2. serial number
		 "\\([a-z]*"
		 ;; 3. year
		 "\\(20[0-9][0-9]\\)"
		 ;; 4. month
		 "\\([01][0-9]\\)"
		 ;; 5. day
		 "\\([0-3][0-9]\\)"
		 "[0-9]+\\)"
		 "\\.html\\)"
		 "\"" s0 ">" s0
		 ;; 6. subject
		 "\\(" no-nl "\\)"
		 s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>" s0 "</dt>")
		1 2 nil 6 3 4 5))
	 (edu (shimbun-asahi-make-regexp "edu.news"))
	 (international (list
			 (concat
			  "<a" s1 "href=\"/"
			  ;; 1. url
			  "\\(international/update/"
			  ;; 2. month
			  "\\([01][0-9]\\)"
			  ;; 3. day
			  "\\([0-3][0-9]\\)"
			  "/"
			  ;; 4. serial number
			  "\\([a-z]*[0-9]+\\)"
			  "\\.html\\)"
			  "\">" s0
			  ;; 5. subject
			  "\\(" no-nl "\\)"
			  s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>")
			 1 4 nil 5 nil 2 3))
	 (rss (list
	       (concat
		"<item" s1 "rdf:about=\""
		;; 1. url
		"\\(http://www.asahi\\.com/articles/"
		;; 2. serial number
		"\\([0-9a-z]+\\)"
		"\\.html\\?ref=rss\\)"
		"\"" s0 ">" s0 "<title>" s0
		;; 3. subject
		"\\([^<]+\\)"
		s0 "</title>\\(?:" s0 "<[^>]+>[^<>]*<" s0 "/[^>]+>\\)+"
		s0 "<dc:subject>" s0
		;; 4. extra keyword (ja)
		"\\([^<]+\\)"
		s0 "</dc:subject>" s0 "<dc:date>" s0
		;; 5. year
		"\\(20[0-9][0-9]\\)"
		"-"
		;; 6. month
		"\\([01][0-9]\\)"
		"-"
		;; 7. day
		"\\([0-3][0-9]\\)"
		"T"
		;; 8. hour:min:sec
		"\\([012][0-9]:[0-5][0-9]:[0-5][0-9]\\)")
	       1 nil 2 3 5 6 7 8 nil nil 4)))
    `(("book" "RSS" "http://www3.asahi.com/rss/book.rdf" ,@rss)
      ("book.column" "コラム")
      ("book.news" "出版ニュース" nil ,@book)
      ("book.paperback" "文庫・新書")
      ("book.review" "書評" nil ,@book)
      ("book.special" "特集" nil
       ,(concat
	 "<a" s1 "href=\"\\(?:http://book\\.asahi\\.com\\)?/"
	 ;; 1. url
	 "\\([^/]+/"
	 ;; 2. serial number
	 "\\([a-z]*"
	 ;; 3. year
	 "\\(20[0-9][0-9]\\)"
	 ;; 4. month
	 "\\([01][0-9]\\)"
	 ;; 5. day
	 "\\([0-3][0-9]\\)"
	 "[0-9]+\\)"
	 "\\.html\\)"
	 "\"" s0 ">" s0
	 ;; 6. subject
	 "\\(" no-nl "\\)")
       1 2 nil 6 3 4 5)
      ("business" "ビジネス" "%s/list.html" ,@default)
      ("car" "愛車" "%s/news/" ,@(shimbun-asahi-make-regexp "car.news"))
      ("culture" "文化・芸能" "%s/list.html"
       ,(concat "<a" s1 "href=\"/"
		;; 1. url
		"\\(culture/"
		"\\(?:[^/]+/\\)?"
		;; 2. serial number
		"\\([a-z]*"
		;; 3. year
		"\\(20[0-9][0-9]\\)"
		;; 4. month
		"\\([01][0-9]\\)"
		;; 5. day
		"\\([0-3][0-9]\\)"
		"[0-9]+\\)"
		"\\.html\\)"
		"\">" s0
		;; 6. subject
		"\\([^<]+\\)"
		s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>" s0
		"<\\(?:/dt\\|span\\)")
       1 nil 2 6 3 4 5)
      ("digital" "デジタル" "%s/list.html"
       ,@(shimbun-asahi-make-regexp "digital/[^\"/]+"))
      ("editorial" "社説" "news/editorial.html"
       ,(concat "<dd" s1 "data-num=\"[0-9]+\">" s0 "<a" s1 "href=\""
		;; 1. url
		"\\(/articles/"
		;; 2. serial
		"\\([^./]+\\)" "\\.html[^\"]*\\)" "[^>]*>" s0
		"\\(?:<em>" s0
		;; 3. subject
		"\\([^<]+\\)" s0 "</em>.+\\|"
		;; 4. subject
		"\\([^<]+\\)" s0 "\\)</a>")
       1 2 nil 3 nil nil nil nil nil nil nil nil nil 4)
      ("edu" "教育" "%s/list.html" ,@edu)
      ("english" "ENGLISH" "%s/index.html"
       ,@(let ((rest (shimbun-asahi-make-regexp "english.Herald-asahi")))
	   (cons (concat
		  (car rest)
		  "\\(?:" s0 "<[^>]+>\\)*" s0 "([01]?[0-9]/[0-3]?[0-9])")
		 (cdr rest))))
      ("food" "食" "%s/news/" ,@(shimbun-asahi-make-regexp "food.news"))
      ("health" "健康" "%s/list.html"
       ,@(shimbun-asahi-make-regexp "health.news"))
      ("housing" "住まい" "%s/news/"
       ,@(shimbun-asahi-make-regexp "housing.news"))
      ("igo" "囲碁" "%s/news/" ,@(shimbun-asahi-make-regexp "igo.news"))
      ("international" "国際" "%s/list.html" ,@default)
      ("international.asia" "アジア" "international/asia.html" ,@international)
      ("international.column" "コラム")
      ("international.special" "特集")
      ("international.world" "世界")
      ("job" "就職・転職" "%s/news/"
       ,@(shimbun-asahi-make-regexp "job.news"))
      ("kansai" "関西" "%s/news/" ,@(shimbun-asahi-make-regexp "kansai.news"))
      ("kansai.entertainment" "楽しむ")
      ("kansai.kokoro" "こころ")
      ("kansai.sumai" "住まい")
      ("kansai.taberu" "食べる")
      ("komimi" "コミミ口コミ" "%s/list.html" ,@default2)
      ("life" "暮らし" "%s/list.html" ,@default)
      ("life.column" "コラム")
      ("national" "社会" "%s/list.html" ,@default)
      ("politics" "政治" "%s/list.html" ,@default)
      ("rss" "RSS" "http://www3.asahi.com/rss/index.rdf" ,@rss)
      ("science" "サイエンス" "%s/list.html" ,@default)
      ("shopping" "ショッピング" "%s/news/"
       ,@(shimbun-asahi-make-regexp "shopping.news"))
      ("shopping.column" "コラム")
      ("shopping.yakimono" "やきもの")
      ("shougi" "将棋")
      ("sports" "スポーツ" "%s/list.html" ,@default)
      ("sports.baseball" "野球")
      ("sports.battle" "格闘技・相撲")
      ("sports.etc" "その他")
      ("sports.football" "サッカー")
      ("sports.golf" "ゴルフ")
      ("sports.rugby" "ラグビー")
      ("sports.usa" "米プロスポーツ")
      ("sports.winter" "ウインタースポーツ")
      ("tenjin" "天声人語" "paper/column.html" ;; WON'T WORK
       ,(concat "<meta" s1 "\\(?:[^\t\n >]+" s1 "\\)*name=\"RELEASE_DATE\""
		s1 "\\(?:[^\t\n >]+" s1 "\\)*CONTENT=\""
		;; 1. year
		"\\(20[1-9][0-9]\\)-"
		;; 2. month
		"\\([01]?[0-9]\\)-"
		;; 3. day
		"\\([0-3]?[0-9]\\)\"")
       nil nil nil nil 1 2 3)
      ("travel" "トラベル" "%s/news/"
       ,@(shimbun-asahi-make-regexp "travel.news"))
      ("wakata" "若田さんきぼう滞在記" "%s/"
       ,(concat
	 "<a" s1 "href=\"/"
	 ;; 1. url
	 "\\(wakata\\(?:/[^\"/]+\\)*/"
	 ;; 2. serial number
	 "\\([a-z]*"
	 ;; 3. year
	 "\\(20[0-9][0-9]\\)"
	 ;; 4. month
	 "\\([01][0-9]\\)"
	 ;; 5. day
	 "\\([0-3][0-9]\\)"
	 "[0-9]+\\)"
	 "\\.html\\)"
	 "\">" s0
	 ;; 6. subject
	 "\\([^\n<>]+\\)"
	 s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)*</a>")
       1 nil 2 6 3 4 5)))
  "Alist of group names, their Japanese translations, index pages,
regexps and numbers.  Where index pages and regexps may contain the
\"%s\" token which is replaced with group names, numbers point to the
search result in order of [0]a url, [1,2]a serial number, [3]a subject,
[4]a year, [5]a month, [6]a day, [7]an hour:minute, [8,9,10]an extra
keyword, [11]hour, [12]minute, and [13]an alternative subject.
If an index page is nil, a group name in which \".\" is substituted with
\"/\" is used instead.")

(defvar shimbun-asahi-subgroups-alist
  (let* ((s0 "[\t\n 　]*")
	 (s1 "[\t\n ]+")
	 (no-nl "[^\n<>]+")
	 (default (list
		   (concat
		    "<a" s1 "href=\"/"
		    ;; 1. url
		    "\\(%s/update/"
		    ;; 2. month
		    "\\([01][0-9]\\)"
		    ;; 3. day
		    "\\([0-3][0-9]\\)"
		    "/"
		    ;; 4. serial number
		    "\\([a-z]*[0-9]+\\)"
		    "\\.html\\)"
		    "\">" s0
		    ;; 5. subject
		    "\\(" no-nl "\\)"
		    s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>")
		   1 4 nil 5 nil 2 3))
	 (baseball (shimbun-asahi-make-regexp "sports.bb"))
	 (book1 (list
		 (concat
		  "<a" s1 "href=\"/"
		  ;; 1. url
		  "\\(%s/"
		  ;; 2. serial number
		  "\\([a-z]*"
		  ;; 3. year
		  "\\(20[0-9][0-9]\\)"
		  ;; 4. month
		  "\\([01][0-9]\\)"
		  ;; 5. day
		  "\\([0-3][0-9]\\)"
		  "[0-9]+\\)"
		  "\\.html\\)"
		  "\"" s0 ">" s0
		  ;; 6. subject
		  "\\(" no-nl "\\)"
		  s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>" s0 "</dt>")
		 1 2 nil 6 3 4 5))
	 (book2 (list
		 (concat
		  "<a" s1 "href=\"http://book\\.asahi\\.com/"
		  ;; 1. url
		  "\\([^/]+/"
		  ;; 2. serial number
		  "\\([a-z]*"
		  ;; 3. year
		  "\\(20[0-9][0-9]\\)"
		  ;; 4. month
		  "\\([01][0-9]\\)"
		  ;; 5. day
		  "\\([0-3][0-9]\\)"
		  "[0-9]+\\)"
		  "\\.html\\)"
		  "\"" s0 ">" s0
		  ;; 6. subject
		  "\\(" no-nl "\\)"
		  s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>" s0 "</dt>")
		 1 2 nil 6 3 4 5))
	 (business (list
		    (concat
		     "<a" s1 "href=\""
		     ;; 1. url
		     "\\(/business/%s/\\(?:[^/]+/\\)?"
		     ;; 2. serial number
		     "\\([a-z]+"
		     ;; 3. year
		     "\\(20[0-9][0-9]\\)"
		     ;; 4. month
		     "\\([01][0-9]\\)"
		     ;; 5. day
		     "\\([0-3][0-9]\\)"
		     "\\(?:[0-9]+\\)"
		     "\\.html\\)\\)"
		     "\"" s0 ">" s0
		     ;; 6. subject
		     "\\(" no-nl "\\)"
		     "</a><span class=\"")
		    1 2 nil 6 3 4 5))
	 (culture (list
		   (concat
		    "<a" s1 "href=\"/"
		    ;; 1. url
		    "\\(culture\\(?:/[^\"/]+\\)+/"
		    ;; 2. serial number
		    "\\([a-z]*"
		    ;; 3. year
		    "\\(20[0-9][0-9]\\)"
		    ;; 4. month
		    "\\([01][0-9]\\)"
		    ;; 5. day
		    "\\([0-3][0-9]\\)"
		    "[0-9]+\\)"
		    "\\.html\\)"
		    "\">" s0
		    ;; 6. subject
		    "\\(" no-nl "\\)"
		    s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>"
		    "[\t\n ]*\\(?:</dt>\\|<span[\t\n ]+\\)")
		   1 nil 2 6 3 4 5))
	 (edu (nthcdr 3 (assoc "edu" shimbun-asahi-group-table)))
	 (football (shimbun-asahi-make-regexp "sports.fb"))
	 (health (nthcdr 3 (assoc "health" shimbun-asahi-group-table)))
	 (international (nthcdr 3 (assoc "international.asia"
					 shimbun-asahi-group-table)))
	 (national (cons (format (car default) "national") (cdr default)))
	 (paperback (list
		     (concat
		      "<a" s1 "href=\"/"
		      ;; 1. url
		      "\\(paperback/"
		      ;; 2. serial number
		      "\\([a-z]*"
		      ;; 3. year
		      "\\(20[0-9][0-9]\\)"
		      ;; 4. month
		      "\\([01][0-9]\\)"
		      ;; 5. day
		      "\\([0-3][0-9]\\)"
		      "[0-9]+\\)"
		      "\\.html\\)"
		      "\"" s0 ">" s0
		      ;; 6. subject
		      "\\(" no-nl "\\)"
		      s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>" s0 "</dt>")
		     1 2 nil 6 3 4 5))
	 (politics (cons (format (car default) "politics") (cdr default)))
	 (shopping (list
		    (concat
		     "<a" s1 "href=\"/"
		     ;; 1. url
		     "\\(shopping/\\(?:[^\"./>]+/\\)+"
		     ;; 2. serial number
		     "\\([a-z]*"
		     ;; 3. year
		     "\\(20[0-9][0-9]\\)"
		     ;; 4. month
		     "\\([01][0-9]\\)"
		     ;; 5. day
		     "\\([0-3][0-9]\\)"
		     "[0-9]*\\)"
		     "\\.html\\)"
		     "\">" s0
		     ;; 6. subject
		     "\\([^<]+\\)"
		     "\\(?:" s0 "<[^>]+>\\)*" s0 "([01]?[0-9]/[0-3]?[0-9])")
		    1 nil 2 6 3 4 5))
	 (shopping2 (list
		     (concat
		      "<a" s1 "href=\"/"
		      ;; 1. url
		      "\\(shopping/yakimono/\\(?:ono\\|yellin\\)/"
		      ;; 2. serial number
		      "\\([a-z]*"
		      ;; 3. year
		      "\\(20[0-9][0-9]\\)"
		      ;; 4. month
		      "\\([01][0-9]\\)"
		      ;; 5. day
		      "\\([0-3][0-9]\\)"
		      "[0-9]*\\)"
		      "\\.html\\)"
		      "\">\\(?:" s0
		      "<div" s1 "class=\"keyword\">[^<]+</div>\\)?" s0
		      ;; 6. subject
		      "\\(" no-nl "\\)"
		      "\\(?:" s0 "&#[0-9]+;\\|&#[0-9]+;" s0 "\\)*"
		      "\\(?:" s0 "<[^>]+>\\)*" s0 "([01]?[0-9]/[0-3]?[0-9])")
		     1 nil 2 6 3 4 5))
	 (sports (shimbun-asahi-make-regexp "sports.spo"))
	 (travel (list
		  (concat "<a" s1 "href=\"/"
			  ;; 1. url
			  "\\(%s/"
			  ;; 2. serial number
			  "\\([a-z]*"
			  ;; 3. year
			  "\\(20[0-9][0-9]\\)"
			  ;; 4. month
			  "\\([01][0-9]\\)"
			  ;; 5. day
			  "\\([0-3][0-9]\\)"
			  "[0-9]+\\)"
			  "\\.html\\)"
			  "\">" s0
			  ;; 6. subject
			  "\\([^<]+\\)"
			  s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>")
		  1 nil 2 6 3 4 5)))
    `(("book.column"
       ("著者に会いたい" "http://book.asahi.com/author/"
	,(format (car book1) "author") ,@(cdr book1))
       ("売れてる本" "http://book.asahi.com/bestseller/"
	,(format (car book1) "bestseller") ,@(cdr book1))
       ("愛でたい文庫" "http://book.asahi.com/bunko/"
	,(format (car book1) "bunko") ,@(cdr book1))
       ("ビジネス書" "http://book.asahi.com/business/"
	,(format (car book1) "business") ,@(cdr book1))
       ("コミックガイド" "http://book.asahi.com/comic/"
	,(format (car book1) "comic") ,@(cdr book1))
       ("話題の本棚" "http://book.asahi.com/hondana/"
	,(format (car book1) "hondana") ,@(cdr book1))
       ("暮らしのお役立ち" "http://book.asahi.com/life/"
	,(format (car book1) "life") ,@(cdr book1))
       ("たいせつな本" "http://book.asahi.com/mybook/"
	,(format (car book1) "mybook") ,@(cdr book1))
       ("ニュースな新刊" "http://book.asahi.com/newstar/"
	,(format (car book1) "newstar") ,@(cdr book1))
       ("新書の穴" "http://book.asahi.com/shinsho/"
	,(format (car book1) "shinsho") ,@(cdr book1))
       ("ニュースな本" "http://book.asahi.com/topics/"
	,(format (car book1) "topics") ,@(cdr book1))
       ("デジタル読書" "http://book.asahi.com/trendwatch/"
	,(format (car book1) "trendwatch") ,@(cdr book1)))
      ("book.news"
       ("朝日新聞社の新刊" "http://book.asahi.com/asahi/"
	,(format (car book1) "asahi") ,@(cdr book1))
       ("ひと・流行・話題" "http://book.asahi.com/clip/"
	,(format (car book1) "clip") ,@(cdr book1))
       ("オンラインブックフェア" "http://book.asahi.com/fair/"
	,(format (car book1) "fair") ,@(cdr book1)))
      ("book.paperback"
       ("文庫" "http://book.asahi.com/paperback/bunko.html" ,@paperback)
       ("新書" "http://book.asahi.com/paperback/shinsho.html" ,@paperback))
      ("book.review"
       ("ビジネス" "http://book.asahi.com/review/business.html" ,@book2)
       ("デジタル" "http://book.asahi.com/review/digital.html" ,@book2)
       ("教育 (児童書)" "http://book.asahi.com/review/edu.html" ,@book2)
       ("国際" "http://book.asahi.com/review/international.html" ,@book2)
       ("暮らし" "http://book.asahi.com/review/life.html" ,@book2))
      ("book.special"
       ("BOOK TIMES" "http://book.asahi.com/booktimes/"
	,(format (car book1) "booktimes") ,@(cdr book1))
       ("売れ筋ランキング" "http://book.asahi.com/ranking/"
	,(format (car book1) "ranking") ,@(cdr book1)))
      ("business"
       ("ＡＥＲＡ発マネー" "http://www.asahi.com/business/aera/"
	,(format (car business) "aera") ,@(cdr business))
       ("投資信託" "http://www.asahi.com/business/fund/"
	,(format (car business) "fund") ,@(cdr business))
       ("商品ファイル" "http://www.asahi.com/business/products/"
	,(format (car business) "products") ,@(cdr business))
       ("ロイターニュース" "http://www.asahi.com/business/list_reuters.html"
	,(format (car business) "reuters") ,@(cdr business))
       ("今日の視点" "http://www.asahi.com/business/today_eye/"
	,(format (car business) "today_eye") ,@(cdr business))
       ("今日の市況" "http://www.asahi.com/business/today_shikyo/"
	,(format (car business) "today_shikyo") ,@(cdr business))
       ("経済を読む" "http://www.asahi.com/business/topics/"
	,(format (car business) "topics") ,@(cdr business))
       ("東洋経済ニュース" "http://www.asahi.com/business/list_toyo.html"
	,(format (car business) "toyo") ,@(cdr business)))
      ("car"
       ("新車発表会" "http://www.asahi.com/car/cg/"
	,@(shimbun-asahi-make-regexp "car.cg"))
       ("イタリア発アモーレ！モトーレ！"
	"http://www.asahi.com/car/italycolumn/"
	,@(shimbun-asahi-make-regexp "car.italycolumn"))
       ("モータースポーツ" "http://www.asahi.com/car/motorsports/"
	,@(shimbun-asahi-make-regexp "car.motorsports"))
       ("新車情報" "http://www.asahi.com/car/newcar/"
	,@(shimbun-asahi-make-regexp "car.newcar")))
      ("culture"
       ("文化" "http://www.asahi.com/culture/list_culture.html" ,@culture)
       ("芸能" "http://www.asahi.com/culture/list_entertainment.html"
	,@culture)
       ("藤沢周平の世界" "http://www.asahi.com/culture/fujisawa/"
	,@(shimbun-asahi-make-regexp "culture.fujisawa"))
       ("人間国宝" "http://www.asahi.com/culture/kokuhou/"
	,@(shimbun-asahi-make-regexp "culture.kokuhou"))
       ("いつかは名人会" "http://www.asahi.com/culture/column/rakugo/guide/"
	,@(shimbun-asahi-make-regexp "culture.column.rakugo.guide"))
       ("落語って" "http://www.asahi.com/culture/column/rakugo/kyosu/"
	,@(shimbun-asahi-make-regexp "culture.column.rakugo.kyosu"))
       ("ラクゴロク" "http://www.asahi.com/culture/column/rakugo/rakugoroku/"
	,@(shimbun-asahi-make-regexp "culture.column.rakugo.rakugoroku"))
       ("連載記事" "http://www.asahi.com/culture/serial_backnumber.html"
	,@culture)
       ("ゆるゆるフェミニン" "http://www.asahi.com/culture/column/yurufemi/"
	,@(shimbun-asahi-make-regexp "culture.column.yurufemi")))
      ("digital"
       ("機器" "http://www.asahi.com/digital/av/"
	,@(shimbun-asahi-make-regexp "digital.av"))
       ("e-ビジネス情報 (提供: BCN)" "http://www.asahi.com/digital/bcnnews/"
	,@(shimbun-asahi-make-regexp "digital.bcnnews"))
       ("コラム" "http://www.asahi.com/digital/column01/"
	,@(shimbun-asahi-make-regexp "digital.column01"))
       ("ネット・ウイルス" "http://www.asahi.com/digital/internet/"
	,@(shimbun-asahi-make-regexp "digital.internet"))
       ("携帯電話" "http://www.asahi.com/digital/mobile/"
	,@(shimbun-asahi-make-regexp "digital.mobile"))
       ("日刊工業新聞ニュース" "http://www.asahi.com/digital/nikkanko/"
	,@(shimbun-asahi-make-regexp "digital.nikkanko"))
       ("ＰＣ・ゲーム" "http://www.asahi.com/digital/pc/"
	,@(shimbun-asahi-make-regexp "digital.pc")))
      ("edu"
       ("入試" "http://www.asahi.com/edu/news/examination.html" ,@edu)
       ("ゆき姐の子育て応援エッセー" "http://www.asahi.com/edu/column/ikuji/"
	,@(shimbun-asahi-make-regexp "edu.column.ikuji"))
       ("教育問題" "http://www.asahi.com/edu/news/issue.html" ,@edu)
       ("この記事を手がかりに" "http://www.asahi.com/edu/nie/kiji/"
	,@(shimbun-asahi-make-regexp "edu.nie.kiji.kiji"))
       ("子育て" "http://www.asahi.com/edu/news/kosodate.html" ,@edu)
       ("教育制度・話題" "http://www.asahi.com/edu/news/system.html" ,@edu)
       ("ののちゃんのＤＯ科学" "http://www.asahi.com/edu/nie/tamate/"
	,@(shimbun-asahi-make-regexp "edu.nie.tamate.kiji"))
       ("大学" "http://www.asahi.com/edu/news/university.html" ,@edu))
      ("food"
       ("季節のおいしいコラム" "http://www.asahi.com/food/cooking/"
	,@(shimbun-asahi-make-regexp "food.cooking"))
       ("岸朝子「暮らしを楽しむお取り寄せ」"
	"http://www.asahi.com/shopping/food/kishi/"
	,(concat "<a" s1 "href=\"/"
		 ;; 1. url
		 "\\(shopping/food/kishi/"
		 ;; 2. serial number
		 "\\([a-z]*"
		 ;; 3. year
		 "\\(20[0-9][0-9]\\)"
		 ;; 4. month
		 "\\([01][0-9]\\)"
		 ;; 5. day
		 "\\([0-3][0-9]\\)"
		 "[0-9]+\\)"
		 "\\.html\\)"
		 "\">" s0
		 ;; 6. subject
		 "\\(" no-nl "\\)"
		 "\\(?:" s0 "<[^>]+>\\)*[^<]*([01]?[0-9]/[0-3]?[0-9])")
	1 nil 2 6 3 4 5)
       ("料理メモ" "http://www.asahi.com/food/memo/"
	,@(shimbun-asahi-make-regexp "food.memo"))
       ("「神の雫」作者のノムリエ日記"
	"http://www.asahi.com/food/column/nommelier/"
	,@(shimbun-asahi-make-regexp "food.column.nommelier"))
       ("おいしさ発見" "http://www.asahi.com/food/column/oishisa/"
	,@(shimbun-asahi-make-regexp "food.column.oishisa"))
       ("論より、おやつ" "http://www.asahi.com/food/column/oyatsu/"
	,@(shimbun-asahi-make-regexp "food.column.oyatsu"))
       ("スイーツの心得" "http://www.asahi.com/food/column/sweets/"
	,@(shimbun-asahi-make-regexp "food.column.sweets"))
       ("ワインの歳時記" "http://www.asahi.com/food/column/wine_saijiki/"
	,@(shimbun-asahi-make-regexp "food.column.wine_saijiki")))
      ("health"
       ("健康・生活" "http://www.asahi.com/health/news/" ,@health)
       ("福祉・高齢" "http://www.asahi.com/health/news/aged.html" ,@health)
       ("認知症特集" "http://www.asahi.com/health/news/alz.html" ,@health)
       ("医療・病気" "http://www.asahi.com/health/news/medical.html" ,@health))
      ("housing"
       ("戸建て" "http://www.asahi.com/ad/clients/kodatenavi/news/"
	,@(shimbun-asahi-make-regexp "ad.clients.kodatenavi.news"))
       ("マンション" "http://www.asahi.com/ad/clients/mansionnavi/news/"
	,@(shimbun-asahi-make-regexp "ad.clients.mansionnavi.news"))
       ("天野彰のいい家いい家族" "http://www.asahi.com/housing/amano/"
	,@(shimbun-asahi-make-regexp "housing.amano"))
       ("住まいのお役立ちコラム" "http://www.asahi.com/housing/column/"
	,@(shimbun-asahi-make-regexp "housing.column"))
       ("小さな家の生活日記" "http://www.asahi.com/housing/diary/"
	,@(shimbun-asahi-make-regexp "housing.diary"))
       ("住宅新報社ニュース" "http://www.asahi.com/housing/jutaku-s/"
	,@(shimbun-asahi-make-regexp "housing.jutaku-s"))
       ("ここが知りたい" "http://www.asahi.com/housing/soudan/index_s.html"
	,@(shimbun-asahi-make-regexp "housing.soudan"))
       ("世界のウチ" "http://www.asahi.com/housing/world/"
	,@(shimbun-asahi-make-regexp "housing.world")))
      ("igo"
       ("トピックス" "http://www.asahi.com/igo/topics/"
	,@(shimbun-asahi-make-regexp "igo.topics")))
      ("international.asia"
       ("アジアの街角" "http://www.asahi.com/international/asiamachi/"
	,@(shimbun-asahi-make-regexp "international.asiamachi"))
       ("人民日報" "http://www.asahi.com/international/jinmin/"
	,@(shimbun-asahi-make-regexp "international.jinmin"))
       ("コリアうめーや！！" "http://www.asahi.com/international/korea/"
	,@(shimbun-asahi-make-regexp "international.korea"))
       ("スパイシー！ソウル" "http://www.asahi.com/international/seoul/"
	,@(shimbun-asahi-make-regexp "international.seoul"))
       ("週刊アジア" "http://www.asahi.com/international/weekly-asia/"
	,@(shimbun-asahi-make-regexp "international.weekly-asia")))
      ("international.column"
       ("船橋洋一の世界ブリーフィング"
	"http://opendoors.asahi.com/syukan/briefing/index.shtml"
	,(concat
	  "<a href=\""
	  ;; 1. url
	  "\\("
	  ;; 2. serial number
	  "\\([0-9]+\\)"
	  "\\.shtml\\)"
	  "\">No\\.[0-9]+　\\[ 週刊朝日"
	  ;; 3. year
	  "\\(20[0-9][0-9]\\)"
	  "年"
	  ;; 4. month
	  "\\([01]?[0-9]\\)"
	  "月"
	  ;; 5. day
	  "\\([0-3]?[0-9]\\)"
	  "[^]]+\\] <br>"
	  ;; 6. subject
	  "\\([^<]+\\)")
	1 2 nil 6 3 4 5)
       ("姿月あさとの「独り言」" "http://www.asahi.com/international/shizuki/"
	,@(shimbun-asahi-make-regexp "international.shizuki")))
      ("international.special"
       ("国際支援の現場から" "http://www.asahi.com/international/shien/"
	,(concat
	  "【＠[^】]+】[\t\n -]*<a" s1 "href=\"/"
	  ;; 1. url
	  "\\(international/shien/"
	  ;; 2. serial number
	  "\\([a-z]*"
	  ;; 3. year
	  "\\(20[0-9][0-9]\\)"
	  ;; 4. month
	  "\\([01][0-9]\\)"
	  ;; 5. day
	  "\\([0-3][0-9]\\)"
	  "[0-9]+\\)"
	  "\\.html\\)"
	  "\">\\(?:" s0 "<[^>]+>\\)*" s0
	  ;; 6. subject
	  "\\([^\n<>]+\\)")
	1 nil 2 6 3 4 5)
       ("鳥インフルエンザ" "http://www.asahi.com/special/051102/"
	,@(shimbun-asahi-make-regexp "special.051102"))
       ("日中関係" "http://www.asahi.com/special/050410/"
	,@(shimbun-asahi-make-regexp "special.050410"))
       ("地球環境" "http://www.asahi.com/special/070110/"
	,@(shimbun-asahi-make-regexp "special.070110"))
       ("北朝鮮拉致事件" "http://www.asahi.com/special/abductees/"
	,@(shimbun-asahi-make-regexp "special.abductees"))
       ("ＢＳＥ問題" "http://www.asahi.com/special/bse/"
	,@(shimbun-asahi-make-regexp "special.bse"))
       ("イラク情勢" "http://www2.asahi.com/special/iraq/"
	,@(shimbun-asahi-make-regexp "special.iraq"))
       ("中東和平" "http://www.asahi.com/special/MiddleEast/"
	,@(shimbun-asahi-make-regexp "special.MiddleEast"))
       ("北朝鮮核問題" "http://www.asahi.com/special/nuclear/"
	,@(shimbun-asahi-make-regexp "special.nuclear"))
       ("中国特集" "http://www.asahi.com/world/china/"
	,@(shimbun-asahi-make-regexp "world.china.news")))
      ("international.world"
       ("アフリカ" "http://www.asahi.com/international/africa.html"
	,@international)
       ("国連・その他" "http://www.asahi.com/international/etc.html"
	,@international)
       ("ヨーロッパ" "http://www.asahi.com/international/europe.html"
	,@international)
       ("中東" "http://www.asahi.com/international/middleeast.html"
	,@international)
       ("北米" "http://www.asahi.com/international/namerica.html"
	,@international)
       ("オセアニア" "http://www.asahi.com/international/oceania.html"
	,@international)
       ("中南米" "http://www.asahi.com/international/samerica.html"
	,@international))
      ("job"
       ("週刊朝日・ＡＥＲＡから" "http://www.asahi.com/job/special/"
	,@(let ((def (shimbun-asahi-make-regexp "job.special")))
	    (cons (concat (car def)
			  "\\(?:" s0 "<[^>]+>\\)*" s0 "（" s0
			  ;; 7. extra
			  "\\(" no-nl "\\)"
			  "：")
		  (append (cdr def) '(nil 7))))))
      ("kansai"
       ("関西芸能ニュース" "http://www.asahi.com/kansai/entertainment/news/"
	,@(shimbun-asahi-make-regexp "kansai.entertainment.news"))
       ("イベント" "http://www.asahi.com/kansai/event/"
	,@(shimbun-asahi-make-regexp "kansai.event")))
      ("kansai.entertainment"
       ("米朝口まかせ" "http://www.asahi.com/kansai/entertainment/beichou/"
	,@(shimbun-asahi-make-regexp "kansai.entertainment.beichou"))
       ("勝手に関西世界遺産"
	"http://www.asahi.com/kansai/entertainment/kansaiisan/"
	,@(shimbun-asahi-make-regexp "kansai.entertainment.kansaiisan"))
       ("教えて！関西っ子"
	"http://www.asahi.com/kansai/entertainment/kansaikko/"
	,@(shimbun-asahi-make-regexp "kansai.entertainment.kansaikko"))
       ("三枝の笑ウインドウ"
	"http://www.asahi.com/kansai/entertainment/sanshi/"
	,@(shimbun-asahi-make-regexp "kansai.entertainment.sanshi"))
       ("千客万歳！" "http://www.asahi.com/kansai/entertainment/senkyaku/"
	,@(shimbun-asahi-make-regexp "kansai.entertainment.senkyaku")))
      ("kansai.kokoro"
       ("○○のひみつ" "http://www.asahi.com/kansai/kokoro/himitsu/"
	,@(shimbun-asahi-make-regexp "kansai.kokoro.himitsu"))
       ("祈りの美" "http://www.asahi.com/kansai/kokoro/inori/"
	,@(shimbun-asahi-make-regexp "kansai.kokoro.inori"))
       ("ピンホールの目" "http://www.asahi.com/kansai/kokoro/pinhole/"
	,@(shimbun-asahi-make-regexp "kansai.kokoro.pinhole"))
       ("語りあう" "http://www.asahi.com/kansai/kokoro/taidan/"
	,@(shimbun-asahi-make-regexp "kansai.kokoro.taidan"))
       ("とみこうみ" "http://www.asahi.com/kansai/kokoro/tomikoumi/"
	,@(shimbun-asahi-make-regexp "kansai.kokoro.tomikoumi")))
      ("kansai.sumai"
       ("ぷらっと沿線紀行" "http://www.asahi.com/kansai/sumai/ensen/"
	,@(shimbun-asahi-make-regexp "kansai.sumai.ensen"))
       ("街を恋う" "http://www.asahi.com/kansai/sumai/machi/"
	,@(shimbun-asahi-make-regexp "kansai.sumai.machi"))
       ("関西の住まい" "http://www.asahi.com/kansai/sumai/news/"
	,@(shimbun-asahi-make-regexp "kansai.sumai.news")))
      ("kansai.taberu"
       ("デパ地下ＮＥＷＳ" "http://www.asahi.com/kansai/taberu/depa/"
	,@(shimbun-asahi-make-regexp "kansai.taberu.depa"))
       ("季語をいただく" "http://www.asahi.com/kansai/taberu/kigo/"
	,@(shimbun-asahi-make-regexp "kansai.taberu.kigo"))
       ("まんぷく会のランチタイム"
	"http://www.asahi.com/kansai/taberu/manpuku/"
	,@(shimbun-asahi-make-regexp "kansai.taberu.manpuku"))
       ("パーンとスイーツ" "http://www.asahi.com/kansai/taberu/pan_sweets/"
	,@(shimbun-asahi-make-regexp "kansai.taberu.pan_sweets"))
       ("やさしい肴" "http://www.asahi.com/kansai/taberu/sakana/"
	,@(shimbun-asahi-make-regexp "kansai.taberu.sakana")))
      ("life.column"
       ("心地よい生活の知恵" "http://www.asahi.com/life/column/chie/"
	,@(shimbun-asahi-make-regexp "life.column.chie"))
       ("私のミカタ" "http://www.asahi.com/life/column/mikata/"
	,@(shimbun-asahi-make-regexp "life.column.mikata\\(?:/sasaki\\)?"))
       ("荻原博子の“がんばれ！家計”"
	"http://www.asahi.com/life/column/ogiwara/"
	,@(shimbun-asahi-make-regexp "life.column.ogiwara")))
      ("national"
       ("災害・交通情報" "http://www.asahi.com/national/calamity.html"
	,@national)
       ("その他・話題" "http://www.asahi.com/national/etc.html" ,@national)
       ("事件・事故" "http://www.asahi.com/national/incident.html" ,@national)
       ("裁判" "http://www.asahi.com/national/trial.html" ,@national)
       ("おくやみ" "http://www.asahi.com/obituaries/"
	,(format (car default) "obituaries") ,@(cdr default)))
      ("politics"
       ("国政" "http://www.asahi.com/politics/government.html" ,@politics)
       ("地方政治" "http://www.asahi.com/politics/local.html" ,@politics))
      ("shopping.column"
       ("楽天こだわり店長に聞く" "http://www.asahi.com/shopping/column/master/"
	,@shopping)
       ("暮らしを楽しむ" "http://www.asahi.com/shopping/column/tatsujin/"
	,@shopping))
      ("shopping.yakimono"
       ("小野公久「やきものガイド」"
	"http://www.asahi.com/shopping/yakimono/ono/" ,@shopping2)
       ("ロバート・イエリン「やきもの散歩道」"
	"http://www.asahi.com/shopping/yakimono/yellin/" ,@shopping2))
      ("shougi"
       ("本" "http://www.asahi.com/shougi/books/"
	,@(shimbun-asahi-make-regexp "shougi.books"))
       ("ニュース" "http://www.asahi.com/shougi/news/"
	,@(shimbun-asahi-make-regexp "shougi.news"))
       ("トピックス" "http://www.asahi.com/shougi/topics/"
	,@(shimbun-asahi-make-regexp "shougi.topics")))
      ("sports.baseball"
       ("アマチュア野球" "http://www.asahi.com/sports/bb/ama.html" ,@baseball)
       ("大リーグ" "http://www.asahi.com/sports/bb/mlb.html" ,@baseball)
       ("プロ野球" "http://www.asahi.com/sports/bb/pro.html" ,@baseball))
      ("sports.battle"
       ("格闘技" "http://www.asahi.com/sports/spo/battle.html"
	,@(shimbun-asahi-make-regexp "sports.\\(?:column\\|spo\\)"))
       ("相撲" "http://www.asahi.com/sports/spo/sumo.html" ,@sports))
      ("sports.etc"
       ("コラム" "http://www.asahi.com/sports/column/"
	,@(shimbun-asahi-make-regexp "sports.column"))
       ("国外一般スポーツ" "http://www.asahi.com/sports/spo/kaigai.html"
	,@sports)
       ("国内一般スポーツ" "http://www.asahi.com/sports/spo/kokunai.html"
	,@sports)
       ("レーシング" "http://www.asahi.com/sports/spo/motor.html" ,@sports))
      ("sports.football"
       ("日本代表" "http://www.asahi.com/sports/fb/japan/list.html"
	,@(shimbun-asahi-make-regexp "sports.fb.japan"))
       ("Ｊリーグ・国内" "http://www.asahi.com/sports/fb/national.html"
	,@football)
       ("海外" "http://www.asahi.com/sports/fb/world.html" ,@football))
      ("sports.golf"
       ("男子ゴルフ" "http://www.asahi.com/sports/spo/golf_man.html" ,@sports)
       ("女子ゴルフ" "http://www.asahi.com/sports/spo/golf_woman.html"
	,@sports))
      ("sports.rugby"
       ("日本代表" "http://www.asahi.com/sports/spo/rugby_japan.html" ,@sports)
       ("国内・その他" "http://www.asahi.com/sports/spo/rugby_national.html"
	,@sports))
      ("sports.usa"
       ("ＮＢＡ" "http://www.asahi.com/sports/spo/nba.html" ,@sports)
       ("ＮＦＬ" "http://www.asahi.com/sports/spo/nfl.html" ,@sports)
       ("ＮＨＬ" "http://www.asahi.com/sports/spo/nhl.html" ,@sports))
      ("sports.winter"
       ("スケート・氷上競技" "http://www.asahi.com/sports/spo/skate.html"
	,@sports)
       ("スキー・雪上競技" "http://www.asahi.com/sports/spo/ski.html"
	,@sports))
      ("travel"
       ("旅する人のアペリティフ" "http://www.asahi.com/travel/aperitif/"
	,(format (car travel) "travel/aperitif") ,@(cdr travel))
       ("ぽれぽれサファリ" "http://www.asahi.com/travel/porepore/"
	,@(shimbun-asahi-make-regexp "travel.porepore"))
       ("島旅たび" "http://www.asahi.com/travel/shima/"
	,(format (car travel) "travel/shima") ,@(cdr travel))
       ("週刊シルクロード紀行" "http://www.asahi.com/travel/silkroad/"
	,@(shimbun-asahi-make-regexp "travel.silkroad"))
       ("愛の旅人" "http://www.asahi.com/travel/traveler/"
	,(format (car travel) "travel/traveler") ,@(cdr travel)))))
  "Alist of parent groups and lists of tables for subgroups.
Each table is the same as the `cdr' of the element of
`shimbun-asahi-group-table'.")

(defvar shimbun-asahi-content-start "<main\\(?:[\t\n ]+[^>]+\\)*>")

(defvar shimbun-asahi-content-end "</main>")

(defvar shimbun-asahi-text-content-start
  "<!-+[\t\n ]*ArticleText[\t\n ]*BGN[\t\n ]*-+>")

(defvar shimbun-asahi-text-content-end
  "<!-+[\t\n ]*ArticleText[\t\n ]*END[\t\n ]*-+>")

(defvar shimbun-asahi-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAAEIAAAAQBAMAAABQPLQnAAAAElBMVEX8rKjd3Nj+7utdXFr
 ////oOTn0UMNfAAAA90lEQVQoz21SQWoDMQwUprlXWA8IZvsAd/yA2KsHLKX+/1c6cklI2EgGeTW
 jwYxW/F2Uq0zGIcWHPAO5ut+SF7nMeyQ/Mb6OqwhnGSOE0gtjb9Q4JG33xqCYeM/mvVqUXOtY+Lp
 37XqbIp4BB9p/wef8IAF1BxQRRoaiIzcWQ4Vdfp2f0aiItKXRSWUxjhk1OjqR3NDYMomBkEVIdYy
 56Q6SbOEKlz0kQy5jZ95mYVvrQpWN0ADB9axMje/5QzAeXvsSbbQvqavxKA3QIpt655XHmOphcHo
 YJJJOO5Kw7QiXSrh9JnAv47GmJ7tfGD7Wqrnp7e2f8AfqrGxn9j9f+QAAAABJRU5ErkJggg==")))
;;  '(("default" . "X-Face: +Oh!C!EFfmR$+Zw{dwWW]1e_>S0rnNCA*CX|\
;;bIy3rr^<Q#lf&~ADU:X!t5t>gW5)Q]N{Mmn\n L]suPpL|gFjV{S|]a-:)\\FR\
;;7GRf9uL:ue5_=;h{V%@()={uTd@l?eXBppF%`6W%;h`#]2q+f*81n$B\n h|t")))

(defvar shimbun-asahi-expiration-days 6)

(luna-define-method initialize-instance :after ((shimbun shimbun-asahi)
						&rest init-args)
  (shimbun-set-server-name-internal shimbun "朝日新聞")
  (shimbun-set-from-address-internal shimbun "nobody@example.com")
  ;; To share class variables between `shimbun-asahi' and its
  ;; successor `shimbun-asahi-html'.
  (shimbun-set-x-face-alist-internal shimbun shimbun-asahi-x-face-alist)
  (shimbun-set-expiration-days-internal shimbun shimbun-asahi-expiration-days)
  (shimbun-set-content-start-internal shimbun shimbun-asahi-content-start)
  (shimbun-set-content-end-internal shimbun shimbun-asahi-content-end)
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-asahi))
  (mapcar 'car shimbun-asahi-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-asahi))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-asahi-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-asahi))
  (let* ((group (shimbun-current-group-internal shimbun))
	 (index (or (nth 2 (assoc group shimbun-asahi-group-table))
		    (concat (subst-char-in-string ?. ?/ group) "/"))))
    (cond ((not index)
	   "about:blank")
	  ((string-match "\\`http:" index)
	   index)
	  ((string-match "\\`book\\." group)
	   (shimbun-expand-url (substring index 5) "http://book.asahi.com/"))
	  (t
	   (shimbun-expand-url (format index group) shimbun-asahi-url)))))

(defun shimbun-asahi-article-contents (group shimbun)
  "Get article's contents for the tenjin group [WON'T WORK].
Contents will be saved in the shimbun header as the extra element."
  (let ((case-fold-search t)
	contents)
    (cond ((string-equal group "tenjin")
	   (goto-char (point-min))
	   (when (and (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"ArticleBody\"" nil t)
		      (search-forward "▼" nil t)
		      (re-search-backward
		       "<p\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]*>" nil t)
		      (shimbun-end-of-tag "p" t))
	     (setq contents
		   (concat "<p>\n"
			   (mapconcat 'identity
				      (split-string (match-string 3)
						    "[\t\n 　]*▼[\t\n 　]*" t)
				      "。\n</p>\n<p>\n　")
			   "\n</p>\n")))
	   (goto-char (point-max))))
    (with-temp-buffer
      (if contents
	  (progn
	    (set-buffer-multibyte t)
	    (insert contents)
	    (unless (memq (shimbun-japanese-hankaku shimbun)
			  '(header subject nil))
	      (shimbun-japanese-hankaku-buffer t))
	    (insert (encode-coding-string (prog1
					      (buffer-string)
					    (erase-buffer)
					    (set-buffer-multibyte nil))
					  'utf-8)))
	(set-buffer-multibyte nil)
	(insert "<h2>No content retrieved.</h2>\n"))
      (let ((coding-system-for-read 'binary)
	    (coding-system-for-write 'binary))
	(shell-command-on-region (point-min) (point-max) "gzip -f9" nil t))
      (base64-encode-region (point-min) (point-max))
      (goto-char (point-min))
      (while (re-search-forward "[\n\r]+" nil t)
	(delete-region (match-beginning 0) (match-end 0)))
      `((Contents . ,(buffer-string))))))

(defun shimbun-asahi-get-headers (shimbun)
  "Return a list of headers."
  (let ((group (shimbun-current-group-internal shimbun))
	(from (concat (shimbun-server-name shimbun)
		      " (" (shimbun-current-group-name shimbun) ")"))
	(case-fold-search t)
	regexp jname numbers book-p cyear cmonth cday rss-p paper-p en-category
	hour-min month year day serial num extra rgroup id headers
	travel-p subgroups iraq-p)
    (setq regexp (assoc group shimbun-asahi-group-table)
	  jname (nth 1 regexp)
	  numbers (nthcdr 4 regexp)
	  book-p (string-match "\\`book\\." group))
    (when (setq regexp (nth 3 regexp))
      (setq regexp (format regexp
			   (regexp-quote (subst-char-in-string
					  ?. ?/ (if book-p
						    (substring group 5)
						  group))))))
    (setq cyear (shimbun-decode-time nil 32400)
	  cday (nth 3 cyear)
	  cmonth (nth 4 cyear)
	  cyear (nth 5 cyear)
	  rss-p (member group '("book" "rss"))
	  paper-p (member group '("editorial" "tenjin"))
	  travel-p (string-equal group "travel")
	  subgroups (cdr (assoc group shimbun-asahi-subgroups-alist)))
    (shimbun-strip-cr)
    (goto-char (point-min))
    (catch 'stop
      ;; The loop for fetching all the articles in the subgroups.
      (while t
	(when regexp
	  (while (re-search-forward regexp nil t)
	    (cond ((string-equal group "editorial")
		   (let ((pt (point)))
		     (save-match-data
		       (if (re-search-backward "<li\\(?:[\t\n ]+[^\t\n >]+\\)*\
[\t\n ]+data-date=\"[^\"0-9]*\\([01]?[0-9]\\)[^\"0-9]+\\([0-3]?[0-9]\\)" nil t)
			   (progn
			     (goto-char pt)
			     (setq day (string-to-number (match-string 2))
				   month (string-to-number (match-string 1))
				   year (if (> month cmonth)
					    (1- cyear)
					  cyear)))
			 (setq day cday month cmonth year cyear)))))
		  ((string-equal group "english")
		   (setq en-category
			 (save-excursion
			   (save-match-data
			     (if (re-search-backward "\
<h[0-9]\\(?:[\n\t ]+[^>]+\\)?>[\t\n ]*\\([^&]+\\)[\t\n ]*&#[0-9]+"
						     nil t)
				 (downcase (match-string 1)))))))
		  (paper-p)
		  (t
		   (setq hour-min
			 (save-excursion
			   (save-match-data
			     (if (re-search-forward "\
<span[\t\n ]+[^>]+>[\t\n ]*(\\(?:[01]?[0-9]/[0-3]?[0-9][\t\n ]+\\)?
\\([012]?[0-9]:[0-5][0-9]\\))[\t\n ]*</span>"
						    nil t)
				 (match-string 1)))))))
	    (unless (string-equal group "editorial")
	      (setq month (string-to-number (match-string (nth 5 numbers)))
		    year (if (setq num (nth 4 numbers))
			     (string-to-number (match-string num))
			   (cond ((>= (- month cmonth) 2)
				  (1- cyear))
				 ((and (= 1 month) (= 12 cmonth))
				  (1+ cyear))
				 (t
				  cyear)))
		    day (string-to-number (match-string (nth 6 numbers)))))
	    (setq serial (cond (rss-p
				(if (and (setq num (nth 1 numbers))
					 (match-beginning num))
				    (format "%d%s.%s"
					    year
					    (match-string (nth 1 numbers))
					    (match-string (nth 2 numbers)))
				  (match-string (nth 2 numbers))))
			       ((string-equal group "tenjin") ;; WON'T WORK
				(format "%d%02d%02d" year month day))
			       ((and (setq num (nth 1 numbers))
				     (match-beginning num))
				(format "%d%02d%02d.%s"
					year month day (match-string num)))
			       (t
				(subst-char-in-string
				 ?/ ?.
				 (downcase (match-string (nth 2 numbers))))))
		  extra (or (and (setq num (nth 8 numbers))
				 (match-beginning num)
				 (match-string num))
			    (and (setq num (nth 9 numbers))
				 (match-beginning num)
				 (match-string num)))
		  rgroup (mapconcat 'identity
				    (nreverse (save-match-data
						(split-string group "\\.")))
				    ".")
		  id (if (and extra
			      (not (save-match-data
				     (string-match "週刊朝日・ＡＥＲＡから"
						   from))))
			 (concat "<" serial "%" extra "." rgroup "."
				 shimbun-asahi-top-level-domain ">")
		       (concat "<" serial "%" rgroup "."
			       shimbun-asahi-top-level-domain ">")))
	    (setq extra (and (string-equal group "tenjin") ;; WON'T WORK
			     (shimbun-asahi-article-contents group shimbun)))
	    (unless (shimbun-search-id shimbun id)
	      (push (shimbun-create-header
		     ;; number
		     0
		     ;; subject
		     (cond (rss-p
			    (match-string (nth 3 numbers)))
			   (en-category
			    (concat "[" en-category "] "
				    (match-string (nth 3 numbers))))
			   ((and (setq num (nth 8 numbers))
				 (match-beginning num))
			    (concat "[" (match-string num) "] "
				    (match-string (nth 3 numbers))))
			   ((and (setq num (nth 9 numbers))
				 (match-beginning num))
			    (concat "[" (match-string num) "] "
				    (match-string (nth 3 numbers))))
			   ((string-equal group "editorial")
			    (format "%d/%d %s" month day
				    (or (match-string (nth 3 numbers))
					(match-string (nth 13 numbers)))))
			   (paper-p ;; tenjin WON'T WORK
			    (concat jname (format " (%d/%d)" month day)))
			   (travel-p
			    (save-match-data
			      (replace-regexp-in-string
			       "\\(?:[\t\n 　]*&#[0-9]+;\\)*[\t\n 　]*" ""
			       (match-string (nth 3 numbers)))))
			   (t
			    (match-string (nth 3 numbers))))
		     ;; from
		     (if (and rss-p
			      (setq num (nth 10 numbers))
			      (setq num (match-string num)))
			 (save-match-data
			   (when (and book-p
				      (string-match
				       "\\`書評　\\[評者\\]\\(その他\\)?" num))
			     (setq num (if (match-beginning 1)
					   "書評"
					 (substring num (match-end 0)))))
			   (replace-regexp-in-string
			    "(RSS" (concat "(" num) from))
		       from)
		     ;; date
		     (shimbun-make-date-string
		      year month day
		      (cond ((and (setq num (nth 11 numbers))
				  (match-beginning num))
			     (concat (match-string num) ":"
				     (match-string (nth 12 numbers))))
			    ((and (setq num (nth 7 numbers))
				  (match-beginning num))
			     (match-string num))
			    (paper-p
			     "07:00")
			    (t
			     hour-min)))
		     ;; id
		     id
		     ;; references, chars, lines
		     "" 0 0
		     ;; xref
		     (if (string-equal group "tenjin") ;; WON'T WORK
			 (shimbun-index-url shimbun)
		       (shimbun-expand-url
			(match-string (nth 0 numbers))
			(cond (book-p
			       "http://book.asahi.com/")
			      ((string-match "船橋洋一の世界ブリーフィング"
					     from)
			       "http://opendoors.asahi.com/syukan/briefing/")
			      (iraq-p
			       "http://www2.asahi.com/")
			      (t
			       shimbun-asahi-url))))
		     ;; extra
		     extra)
		    headers)))
	  (when (string-match "私のミカタ" from)
	    (setq headers (nreverse headers))))
	(if subgroups
	    (progn
	      (erase-buffer)
	      (setq from (concat (shimbun-server-name shimbun)
				 " (" (caar subgroups) ")")
		    iraq-p (string-equal (caar subgroups) "イラク情勢"))
	      (shimbun-retrieve-url (cadar subgroups))
	      (setq regexp (caddar subgroups)
		    numbers (cdddar subgroups)
		    subgroups (cdr subgroups)))
	  (throw 'stop nil))))
    (shimbun-sort-headers headers)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-asahi)
					 &optional range)
  (shimbun-asahi-get-headers shimbun))

(defun shimbun-asahi-multi-next-url (shimbun header url)
  (goto-char (point-min))
  (when (and (re-search-forward "\
<div[\t\n ]\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"SeqNav forSplit\""
				nil t)
	     (shimbun-end-of-tag "div" t))
    (let ((end (match-beginning 0)))
      (prog1
	  (when (re-search-backward "\
<a[\t\n ]+href=\"\\([^\"]+\\)\"[^>]*>[\t\n ]*次ページ[\t\n ]*</a>"
				    end 'move)
	    (goto-char end)
	    (shimbun-expand-url (match-string 1) url))
	(if (and (re-search-backward "[^\t\n >]\\([\t\n ]*<\\)" nil t)
		 (re-search-forward "[\t\n ]*<[\t\n ]*[^/]" end t))
	    (delete-region (match-beginning 0) end)
	  (goto-char end))
	(insert "<!-- End of Kiji -->\n")))))

(luna-define-method shimbun-multi-next-url ((shimbun shimbun-asahi)
					    header url)
  (shimbun-asahi-multi-next-url shimbun header url))

(declare-function zlib-available-p "decompress.c")
(declare-function zlib-decompress-region "decompress.c"
		  (start end &optional allow-partial))

(defun shimbun-asahi-article (shimbun header outbuf)
  "Fetch article contents."
  (if (member (shimbun-current-group-internal shimbun) '("tenjin"));; WON'T WORK
      (let ((contents (cdr (assq 'Contents (shimbun-header-extra header)))))
	(insert
	 (with-temp-buffer
	   (if contents
	       (progn
		 (set-buffer-multibyte nil)
		 (insert contents)
		 (base64-decode-region (point-min) (point-max))
		 (if (and (fboundp 'zlib-available-p) (zlib-available-p))
		     (zlib-decompress-region (point-min) (point-max))
		   (let ((coding-system-for-read 'binary)
			 (coding-system-for-write 'binary))
		     (shell-command-on-region (point-min) (point-max)
					      "gzip -d" nil t)))
		 (setq contents (decode-coding-string (buffer-string) 'utf-8))
		 (erase-buffer)
		 (set-buffer-multibyte t)
		 (insert "<div class=\"ArticleBody\">"
			 contents
			 "</div><!-- ArticleBody END -->"))
	     (insert "<h2>No content found.</h2>\n"))
	   (shimbun-make-contents shimbun header))))
    (luna-call-next-method)))

(luna-define-method shimbun-article :around ((shimbun shimbun-asahi)
					     header &optional outbuf)
  (shimbun-asahi-article shimbun header outbuf))

(defun shimbun-asahi-prepare-article (shimbun header)
  "Prepare an article.
For the tenjin group [WON'T WORK], use the extra element of HEADER as
article contents."
  (let ((case-fold-search t)
	(group (shimbun-current-group-internal shimbun))
	(from (shimbun-header-from-internal header)))
    (cond
     ((string-equal group "car")
      (shimbun-remove-tags "\
[\t\n ]*<![\t\n ]*-+[\t\n ]*[★☆]+[\t\n ]*AD[\t\n ]*[★☆]+[\t\n ]*-+>"
			   "\
<![\t\n ]*-+[\t\n ]*/[\t\n ]*[★☆]+[\t\n ]*AD[\t\n ]*[★☆]+[\t\n ]*-+>\
[\t\n ]*")
      (goto-char (point-min))
      (when (and (re-search-forward (shimbun-content-start shimbun) nil t)
		 (re-search-forward "\
[\t\n ]*<!-+[\t\n ]*Creative[\t\n ]+for[\t\n ]+"
				    nil t))
	(goto-char (match-beginning 0))
	(insert "\n<!-- End of Kiji -->\n")))
     ((string-equal group "digital")
      (shimbun-remove-tags "\
[\t\n ]*<![\t\n ]*-+[\t\n ]*[★☆]+[\t\n ]*AD[\t\n ]*[★☆]+[\t\n ]*-+>"
			   "\
<![\t\n ]*-+[\t\n ]*/[\t\n ]*[★☆]+[\t\n ]*AD[\t\n ]*[★☆]+[\t\n ]*-+>\
[\t\n ]*")
      (cond ((string-match "コラム" from)
	     (unless (re-search-forward (shimbun-content-end shimbun) nil t)
	       (when (re-search-forward "\\(?:[\t\b ]*<[^>]+>\\)*[\t\n ]*\
\\(?:<img[\t\n ]+src=\"[^>]*[\t\n ]*alt=\"プロフィール\"\
\\|<h[0-9]>プロフィール</h[0-9]>\\)"
					nil t)
		 (goto-char (match-beginning 0))
		 (insert "\n<!-- End of Kiji -->\n"))))))
     ((member group '("editorial" "tenjin"))
      (when (and (re-search-forward "<p class=\"SubTitle\"" nil t)
		 (shimbun-end-of-tag "p" t))
	(replace-match "\n")))
     ((string-equal group "food")
      (when (and (re-search-forward (shimbun-content-start shimbun) nil t)
		 (re-search-forward "[\t\n ]*<!-+[\t\n ]+Creative[\t\n ]+for"
				    nil t))
	(goto-char (match-beginning 0))
	(insert "\n<!-- End of Kiji -->\n")))
     ((string-equal group "housing")
      (shimbun-remove-tags
       "\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*<!--広告スキップ -->"
       "<!--/広告スキップのとび先-->[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*")
      (when (string-match "マンション\\|戸建て" from)
	(goto-char (point-min))
	(re-search-forward "<td[\t\n ]+valign=\"top\">[\t\n ]*\
\\(?:<[^>]+>[\t\n ]*\\)*"
			   nil t)
	(insert "<!-- Start of Kiji -->")
	(when (re-search-forward "[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*</td>"
				 nil 'move)
	  (goto-char (match-beginning 0))
	  (insert "<!-- End of Kiji -->"))))
     ((string-equal group "rss"))
     ((string-match "\\`book\\(?:\\.\\|\\'\\)" group)
      (while (re-search-forward "\\(<a[\t\n ]+[^>]+>\\)\
[\t\n ]*<img[\t\n ]+[^>]+>[\t\n ]*</a>"
				nil t)
	(when (save-match-data
		(search-backward "alt=\"No image\"" (match-beginning 0) t))
	  (replace-match "\\1No image</a>"))))
     ((string-match "\\`shopping\\." group)
      (when (re-search-forward "\
<!-+[\t\n ]*end[\t\n ]+of[\t\n ]+headline[\t\n ]*-+>[\t\n ]*\
<p[\t\n ]+class=[^>]+>[\t\n ]*\\([^<]+\\)</p>[\t\n ]*\
<p[\t\n ]+id=\"date\">[\t\n ]*20[0-9][0-9]年[01]?[0-9]月[0-3]*[0-9]日[\t\n ]*\
</p>[\t\n ]*"
			       nil t)
	(replace-match "<!-- Start of Kiji -->\\1<br>\n")))
     ((string-match "ののちゃんのＤＯ科学" from)
      ;; Remove furigana.
      (while (re-search-forward "\\(\\cj\\)（\\cH+）" nil t)
	(replace-match "\\1")))
     ((string-match "ゆるゆるフェミニン" from)
      (let (comics)
	(while (re-search-forward
		"<img[\t\n ]+src=\"[^>]+alt=\"マンガ\"[^>]*>"
		nil t)
	  (push (match-string 0) comics))
	(erase-buffer)
	(when comics
	  (insert "<!-- Start of Kiji -->\n"
		  (mapconcat 'identity comics "<br>\n")
		  "\n<!-- End of Kiji -->\n"))))
     ((string-match "船橋洋一の世界ブリーフィング" from)
      (when (re-search-forward "\
<img[\t\n ]+src=\"[^>]+[\t\n ]+alt=\"船橋洋一顔写真\">"
			       nil t)
	(goto-char (match-beginning 0))
	(insert "<!-- Start of Kiji -->")
	(when (re-search-forward "\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*\
\\(?:<TD[\t\n ]+id=\"sidebar\">\
\\|<a[\t\n ]+href=\"http://opendoors\\.asahi\\.com/data/detail/\
\\|<!-+[\t\n ]*トピックス[\t\n ]*-+>\\)"
				 nil t)
	  (goto-char (match-beginning 0))
	  (insert "\n<!-- End of Kiji -->"))))
     ((string-match "中国特集" from)
      (let (start)
	(when (and (re-search-forward "\
<H2>中国最新ニュース</H2>[\t\n ]*<H1>[^>]+</H1>[\t\n ]*"
				      nil t)
		   (progn
		     (setq start (match-end 0))
		     (re-search-forward "\
<p[^>]*>[\t\n ]*([01][0-9]/[0-3][0-9])[\t\n ]*</p>"
					nil t)))
	  (delete-region (match-end 0) (point-max))
	  (insert "\n<!-- End of Kiji -->")
	  (delete-region (point-min) (goto-char start))
	  (insert "<!-- Start of Kiji -->\n"))))
     ((string-match "東洋経済ニュース" from)
      ;; Insert newlines.
      (shimbun-with-narrowed-article
       shimbun
       (while (re-search-forward "。　?\\(\\cj\\)" nil t)
	 (replace-match "。<br><br>　\\1"))))
     ((string-match "藤沢周平の世界\\|人間国宝" from)
      (when (re-search-forward "\
<div[\t\n ]+\\(?:class=\"kiji\"\\|id=\"kokuhou-waza\"\\)>[\t\n ]*"
			       nil t)
	(insert "\n<!-- Start of Kiji -->\n")
	(when (re-search-forward "\
\\(?:[\t\n ]*<[^>]+>\\)?\\(?:[\t\n ]*20[0-9][0-9]年[01]?[0-9]月[0-3]?[0-9]日\
\\(?:[\t\n ]*<[^>]+>\\)*\\)?[\t\n ]*<!-+[\t\n ]*google"
				 nil t)
	  (goto-char (match-beginning 0))
	  (insert "\n<!-- End of Kiji -->\n")))))
    (shimbun-with-narrowed-article
     shimbun
     ;; Remove sitesearch area.
     (when (re-search-forward "[\t\n ]*\\(?:<div[\t\n ]+[^>]+>[\t\n ]*\\)+\
この記事の関連情報をアサヒ・コム内で検索する"
			      nil t)
       (goto-char (match-beginning 0))
       (insert "\n<!-- End of Kiji -->"))
     ;; Remove ads.
     (goto-char (point-min))
     (when (re-search-forward "[\t\n ]*<p[\t\n ]+class=\"hide\">[\t\n ]\
*ここから広告です[\t\n ]*</p>"
			      nil t)
       (let ((start (match-beginning 0)))
	 (when (re-search-forward "<p[\t\n ]+class=\"hide\">[\t\n ]*\
広告終わり\\(?:[\t\n ]*</p>[\t\n ]*\\|\\'\\)"
				  nil t)
	   (delete-region start (match-end 0)))))
     ;; Remove trailing garbage.
     (goto-char (point-min))
     (when (and (not (string-match "ゆるゆるフェミニン" from))
		(re-search-forward
		 "\\(?:</p>\\)?\\(\\(?:[\t\n 　]*<[^>]+>\\)+[\t\n 　]*\\'\\)"
		 nil t))
       (goto-char (match-beginning 0))
       (while (or (and (looking-at "[\t\n 　]*\\(</[^>]+>\\)[\t\n 　]*")
		       ;; Don't remove close tags.
		       (progn (replace-match "\\1") t))
		  (and (looking-at "[\t\n 　]*<[^>]+>[\t\n 　]*\\|[\t\n 　]+")
		       (progn (replace-match "") t))))
       (goto-char (point-max))
       (unless (bolp) (insert "\n"))))))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-asahi)
						   header)
  (shimbun-asahi-prepare-article shimbun header))

(defun shimbun-asahi-clear-contents (shimbun header)
  (when (luna-call-next-method)
    (goto-char (point-min))
    (let (st)
      (while (and (re-search-forward "[\t\n ]*<li>[\t\n ]*\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*x-component-name=" nil t)
		  (progn
		    (setq st (match-beginning 0))
		    (shimbun-end-of-tag "div" t)))
	(delete-region st (match-end 0))))
    (goto-char (point-min))
    (while (re-search-forward "[\t\n ]*<li>[\t\n ]*\\(\
\\(?:<[^>]+[\t\n ]*>\\)\\{,3\\}<img[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*\
alt=\"写真・図版\"\\)" nil t)
      (delete-region (match-beginning 0) (match-beginning 1)))
    (goto-char (point-min))
    (when (re-search-forward "[\t\n ]*<span>\\[PR]</span>[\t\n ]*" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (when (re-search-forward "[\t\n ]+alt=\"\\(?:有料\\)?会員記事\"" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    t))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-asahi)
						    header)
  (shimbun-asahi-clear-contents shimbun header))

(defun shimbun-asahi-multi-clear-contents (shimbun header
						   has-previous-page
						   has-next-page)
  (when (luna-call-next-method)
    (when has-previous-page
      (goto-char (point-min))
      (insert "&#012;\n")
      (when (looking-at "[\t\n ]*<p>[\t\n ]*")
	(delete-region (match-beginning 0) (match-end 0))))
    t))

(luna-define-method shimbun-multi-clear-contents :around ((shimbun
							   shimbun-asahi)
							  header
							  has-previous-page
							  has-next-page)
  (shimbun-asahi-multi-clear-contents shimbun header
				      has-previous-page has-next-page))

(provide 'sb-asahi)

;;; sb-asahi.el ends here

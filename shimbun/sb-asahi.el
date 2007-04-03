;;; sb-asahi.el --- shimbun backend for asahi.com -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007
;; Yuuichi Teranishi <teranisi@gohome.org>

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
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(eval-when-compile (require 'cl))

(require 'shimbun)

(luna-define-class shimbun-asahi (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-asahi-prefer-text-plain t
  "*Non-nil means prefer text/plain articles rather than html articles.")

(defvar shimbun-asahi-top-level-domain "asahi.com"
  "Name of the top level domain for the Asahi shimbun.")

(defvar shimbun-asahi-url
  (concat "http://www." shimbun-asahi-top-level-domain "/")
  "Name of the parent url.")

(defun shimbun-asahi-make-regexp (name)
  "Return a list of a regexp and numbers for the kansai.NAME group.
Every `.' in NAME will be replaced with `/'."
  (list (let ((s0 "[\t\n 　]*")
	      (s1 "[\t\n ]+")
	      (no-nl "[^\n<>]+"))
	  (concat "<a" s1 "href=\"/"
		  ;; 1. url
		  "\\(" (shimbun-subst-char-in-string ?. ?/ name) "/"
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
	 (default3 (list
		    (concat
		     "<li>" s0 "<a" s1 "href=\"/+"
		     ;; 1. url
		     "\\(\\(?:[^\"/<>]+/\\)+"
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
		     s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>")
		    1 nil 2 6 3 4 5))
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
	 (health (shimbun-asahi-make-regexp "health.news"))
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
		"\\(http://\\(?:book\\|www\\)\\.asahi\\.com/"
		;; 2. extra keyword (en)
		"\\([^/]+\\)"
		"\\(?:/update/"
		;; 3 and 4. serial number
		"\\([0-9]+\\)\\)?/\\([a-z]*[0-9]+\\)"
		"\\.html\\?ref=rss\\)"
		"\"" s0 ">" s0 "<title>" s0
		;; 5. subject
		"\\([^<]+\\)"
		s0 "</title>\\(?:"
		s0 "\\(?:<[^>]+/>\\|<[^>]+>[^<]+</[^>]+>\\)\\)*"
		s0 "<dc:subject>" s0
		;; 6. extra keyword (ja)
		"\\([^<]+\\)"
		s0 "</dc:subject>" s0 "<dc:date>" s0
		;; 7. year
		"\\(20[0-9][0-9]\\)"
		"-"
		;; 8. month
		"\\([01][0-9]\\)"
		"-"
		;; 9. day
		"\\([0-3][0-9]\\)"
		"T"
		;; 10. hour:min:sec
		"\\([012][0-9]:[0-5][0-9]:[0-5][0-9]\\)")
	       1 3 4 5 7 8 9 10 2 nil 6))
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
	 (sports (shimbun-asahi-make-regexp "sports.spo")))
    `(("book.column" "コラム")
      ("book.news" "出版ニュース" nil ,@book)
      ("book.paperback" "文庫・新書")
      ("book.review" "書評" nil ,@book)
      ("book.rss" "RSS" "http://feeds.asahi.com/asahi/Book" ,@rss)
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
      ("editorial" "社説" "paper/editorial.html"
       ,(concat
	 "<a" s1 "href=\"\\./"
	 ;; 1. url
	 "\\(editorial"
	 ;; 2. year
	 "\\(20[0-9][0-9]\\)"
	 ;; 3. month
	 "\\([01][0-9]\\)"
	 ;; 4. day
	 "\\([0-3][0-9]\\)"
	 "\\.html\\)"
	 "\"")
       1 nil nil nil 2 3 4)
      ("edu.examination" "入試" "edu/news/examination.html" ,@edu)
      ("edu.ikuji" "子育て応援エッセー" "edu/column/ikuji/"
       ,@(shimbun-asahi-make-regexp "edu.column.ikuji"))
      ("edu.issue" "教育問題" "edu/news/issue.html" ,@edu)
      ("edu.kiji" "この記事を手がかりに" "edu/nie/kiji/"
       ,@(shimbun-asahi-make-regexp "edu.nie.kiji.kiji"))
      ("edu.kosodate" "子育て" "edu/news/kosodate.html" ,@edu)
      ("edu.system" "教育制度・話題" "edu/news/system.html" ,@edu)
      ("edu.university" "大学" "edu/news/university.html" ,@edu)
      ("edu.tamate" "ののちゃんのふしぎ玉手箱" "edu/nie/tamate/"
       ,@(shimbun-asahi-make-regexp "edu.nie.tamate.kiji"))
      ("english" "ENGLISH" "%s/index.html"
       ,@(let ((rest (shimbun-asahi-make-regexp "english.Herald-asahi")))
	   (cons (concat
		  (car rest)
		  "\\(?:" s0 "<[^>]+>\\)*" s0 "([01]?[0-9]/[0-3]?[0-9])")
		 (cdr rest))))
      ("health" "健康・生活" "%s/news/" ,@health)
      ("health.aged" "福祉・高齢" "health/news/aged.html" ,@health)
      ("health.alz" "認知症特集" "health/news/alz.html" ,@health)
      ("health.medical" "医療・病気" "health/news/medical.html" ,@health)
      ("housing" "住まい" "%s/news/"
       ,@(shimbun-asahi-make-regexp "housing.news"))
      ("housing.amano" "天野彰のいい家いい家族" nil ,@default2)
      ("housing.column" "住まいのお役立ちコラム" nil ,@default2)
      ("housing.diary" "小さな家の生活日記" nil ,@default2)
      ("housing.jutaku-s" "住宅新報社ニュース" nil ,@default2)
      ("housing.kansai" "関西の住まい" "kansai/sumai/news/"
       ,@(shimbun-asahi-make-regexp "kansai.sumai.news"))
      ("housing.machi" "街を恋う" "kansai/sumai/machi/"
       ,@(shimbun-asahi-make-regexp "kansai.sumai.machi"))
      ("housing.soudan" "ここが知りたい！" nil ,@default2)
      ("housing.world" "世界のウチ" nil ,@default2)
      ("igo" "囲碁" "%s/news/"
       ,@(shimbun-asahi-make-regexp "igo/\\(?:news\\|topics\\)"))
      ("international" "国際" "%s/list.html" ,@default)
      ("international.africa" "アフリカ" "international/africa.html"
       ,@international)
      ("international.asia" "アジア" "international/asia.html" ,@international)
      ("international.asiamachi" "アジアの街角" nil ,@default2)
      ("international.briefing" "船橋洋一の世界ブリーフィング"
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
      ("international.europe" "ヨーロッパ" "international/europe.html"
       ,@international)
      ("international.etc" "国連・その他" "international/etc.html"
       ,@international)
      ("international.jinmin" "人民日報" "international/jinmin/index.html"
       ,@default2)
      ("international.kawakami" "中東ウオッチ" nil ,@default2)
      ("international.korea" "コリアうめーや！！" nil ,@default2)
      ("international.middleeast" "中東" "international/middleeast.html"
       ,@international)
      ("international.namerica" "北米" "international/namerica.html"
       ,@international)
      ("international.oceania" "オセアニア" "international/oceania.html"
       ,@international)
      ("international.samerica" "中南米" "international/samerica.html"
       ,@international)
      ("international.seoul" "スパイシー！ソウル" nil ,@default2)
      ("international.shien" "国際支援の現場から" nil
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
      ("international.shizuki" "姿月あさとの「独り言」" nil ,@default2)
      ("international.weekly-asia" "週刊アジア" nil ,@default2)
      ("job" "就職・転職" "%s/news/"
       ,@(shimbun-asahi-make-regexp "job.news"))
      ("job.special" "週刊朝日・ＡＥＲＡから" nil
       ,(concat
	 (car default2)
	 "\\(?:" s0 "<[^>]+>\\)*" s0 "（" s0
	 ;; 7. extra
	 "\\(" no-nl "\\)"
	 "：")
       ,@(cdr default2) nil 7)
      ("kansai" "関西" "%s/news/" ,@(shimbun-asahi-make-regexp "kansai.news"))
      ("kansai.beichou" "米朝口まかせ" "kansai/entertainment/beichou/"
       ,@(shimbun-asahi-make-regexp "kansai.entertainment.beichou"))
      ("kansai.depa" "デパ地下ＮＥＷＳ" "kansai/taberu/depa/"
       ,@(shimbun-asahi-make-regexp "kansai.taberu.depa"))
      ("kansai.fuukei" "新 風景を歩く" "kansai/sumai/fuukei2/"
       ,@(shimbun-asahi-make-regexp "kansai.sumai.fuukei2"))
      ("kansai.heibon" "週刊★平凡★女性" "kansai/entertainment/heibon/"
       ,@(shimbun-asahi-make-regexp "kansai.entertainment.heibon"))
      ("kansai.honma" "かきくけこどものはひふへほんま？" nil ,@default2)
      ("kansai.kansaiisan" "勝手に関西世界遺産"
       "kansai/entertainment/kansaiisan/"
       ,@(shimbun-asahi-make-regexp "kansai.entertainment.kansaiisan"))
      ("kansai.madam" "夕刊マダム" nil ,@default2)
      ("kansai.okan" "母さんの知恵袋" "kansai/sumai/chiebukuro/"
       ,@(shimbun-asahi-make-regexp "kansai.sumai.chiebukuro"))
      ("kansai.otoriyose" "わくわくお取り寄せ" "kansai/taberu/otoriyose/"
       ,@(shimbun-asahi-make-regexp "kansai.taberu.otoriyose"))
      ("kansai.sakana" "やさしい肴" "kansai/taberu/sakana/"
       ,@(shimbun-asahi-make-regexp "kansai.taberu.sakana"))
      ("kansai.sanshi" "三枝の笑ウインドウ" "kansai/entertainment/sanshi/"
       ,@(shimbun-asahi-make-regexp "kansai.entertainment.sanshi"))
      ("kansai.sweets" "粋・すいーつ" "kansai/taberu/sweets/"
       ,@(shimbun-asahi-make-regexp "kansai.taberu.sweets"))
      ("kansai.umaimon" "うまいもん" "kansai/taberu/umaimon/"
       ,@(shimbun-asahi-make-regexp "kansai.taberu.umaimon"))
      ("life" "暮らし" "%s/list.html" ,@default)
      ("life.column" "暮らしコラム" nil
       ,(concat
	 "<a" s1 "href=\"/"
	 ;; 1. url
	 "\\(life/column/"
	 ;; 2. serial number
	 "\\(.+/[a-z]*"
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
	 "\\(" no-nl "\\)"
	 s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>")
       1 nil 2 6 3 4 5)
      ("life.food" "食と料理" nil
       ,(concat
	 "<a" s1 "href=\"/"
	 ;; 1. url
	 "\\(life/food/"
	 ;; 2. serial number
	 "\\(.+/[a-z]*"
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
	 s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>")
       1 nil 2 6 3 4 5)
      ("national" "社会" "%s/list.html" ,@default)
      ("national.calamity" "災害・交通情報" "national/calamity.html"
       ,@default3)
      ("national.etc" "その他・話題" "national/etc.html" ,@default3)
      ("national.incident" "事件・事故" "national/incident.html" ,@default3)
      ("national.trial" "裁判" "national/trial.html" ,@default3)
      ("obituaries" "おくやみ" "obituaries" ,@default)
      ("politics" "政治" "%s/list.html" ,@default)
      ("politics.government" "国政" "politics/government.html"
       ,(format (car default) "politics") ,@(cdr default))
      ("politics.local" "地方政治" "politics/local.html"
       ,(format (car default) "politics") ,@(cdr default))
      ("rss" "RSS" "http://feeds.asahi.com/asahi/TopHeadlines" ,@rss)
      ("science" "サイエンス" "%s/list.html"
       ,@(shimbun-asahi-make-regexp "science.news"))
      ("shopping" "ショッピング" "%s/"
       ,(concat
	 "<a" s1 "href=\"/"
	 ;; 1. url
	 "\\(\\(?:[^\"/]+/\\)+"
	 ;; 2. extra
	 "\\([^\"/]+\\)"
	 "/"
	 ;; 3. serial number
	 "\\([a-z]*"
	 ;; 4. year
	 "\\(20[0-9][0-9]\\)"
	 ;; 5. month
	 "\\([01][0-9]\\)"
	 ;; 6. day
	 "\\([0-3][0-9]\\)"
	 "[0-9]+\\)"
	 "\\.html\\)"
	 "\">\\(?:" s0 "<[^<>]+>\\)*" s0
	 ;; 7. subject
	 "\\([^>]+\\)"
	 "\\(?:" s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>\\)?"
	 s0 "<span" s1 "class=\"s\">")
       1 3 nil 7 4 5 6 nil 2)
      ("shopping.interiorlife" "充実の雑貨・インテリア生活"
       "shopping/living/interiorlife/"
       ,@shopping)
      ("shopping.kishi" "岸朝子の日本の名産お取り寄せ12カ月"
       "shopping/food/kishi/"
       ,(concat
	 "<a" s1 "href=\"/"
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
	 "[0-9]*\\)"
	 "\\.html\\)"
	 "\">" s0 "<div" s1 "class=\"keyword\">[^【]+"
	 ;; 6. subject
	 "\\(【\\(?:[^<]+\\(?:<[^>]+>\\)?\\)+\\(?:[^<]+\\)?\\)"
	 s0 "</a>")
       1 nil 2 6 3 4 5)
      ("shopping.master" "楽天こだわり店長に聞く" "shopping/column/master/"
       ,@shopping)
      ("shopping.otoriyose" "わくわくお取り寄せ" "shopping/food/otoriyose/"
       ,@shopping)
      ("shopping.protalk" "プロの語りごと" "shopping/column/protalk/"
       ,@shopping)
      ("shopping.yakimono.ono" "小野公久「やきものガイド」" nil ,@shopping2)
      ("shopping.yakimono.yellin" "ロバート・イエリン「やきもの散歩道」" nil
       ,@shopping2)
      ("shougi" "将棋" nil
       ,@(shimbun-asahi-make-regexp "shougi.\\(?:books\\|news\\|topics\\)"))
      ("sports" "スポーツ" "%s/list.html" ,@default)
      ("sports.baseball" "野球" "sports/bb/"
       ,@(shimbun-asahi-make-regexp "sports.bb"))
      ("sports.battle" "格闘技" "sports/spo/battle/list.html"
       ,@(shimbun-asahi-make-regexp "sports.spo.battle"))
      ("sports.battle.column" "格闘技コラム" "sports/spo/battle_column.html"
       ,@(shimbun-asahi-make-regexp "sports.column"))
      ("sports.column" "スポーツコラム" nil ,@default2)
      ("sports.football" "サッカー" "sports/fb/"
       ,@(shimbun-asahi-make-regexp "sports.fb"))
      ("sports.golf" "ゴルフ" nil
       ,@(shimbun-asahi-make-regexp "sports.\\(?:column\\|golf\\)"))
      ("sports.motor" "レーシング" "sports/spo/motor.html" ,@sports)
      ("sports.rugby" "ラグビー" "sports/spo/rugby.html" ,@sports)
      ("sports.spo" "一般スポーツ" nil ,@default2)
      ("sports.sumo" "相撲" "sports/spo/sumo.html" ,@sports)
      ("sports.usa" "米プロスポーツ" "sports/spo/usa.html"
       ,@(shimbun-asahi-make-regexp "sports.\\(?:nfl\\|spo\\)"))
      ("sports.winter" "ウインタースポーツ" "sports/spo/winter.html" ,@sports)
      ("tenjin" "天声人語" "paper/column.html"
       ,(concat
	 "<a" s1 "href=\"\\./"
	 ;; 1. url
	 "\\(column"
	 ;; 2. year
	 "\\(20[0-9][0-9]\\)"
	 ;; 3. month
	 "\\([01][0-9]\\)"
	 ;; 4. day
	 "\\([0-3][0-9]\\)"
	 "\\.html\\)"
	 "\"")
       1 nil nil nil 2 3 4)
      ("travel" "トラベル" "%s/news/"
       ,@(shimbun-asahi-make-regexp "travel.news"))
      ("world.china" "中国特集" nil
       ,@(shimbun-asahi-make-regexp "world.china.news"))))
  "Alist of group names, their Japanese translations, index pages,
regexps and numbers.  Where index pages and regexps may contain the
\"%s\" token which is replaced with group names, numbers point to the
search result in order of [0]a url, [1,2]a serial number, [3]a subject,
\[4]a year, [5]a month, [6]a day, [7]an hour:minute, [8,9,10]an extra
keyword, [11]hour and [12]minute.  If an index page is nil, a group
name in which \".\" is substituted with \"/\" is used instead.")

(defvar shimbun-asahi-subgroups-alist
  (let* ((s0 "[\t\n 　]*")
	 (s1 "[\t\n ]+")
	 (no-nl "[^\n<>]+")
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
      ("travel"
       ("旅する人のアペリティフ" "http://www.asahi.com/travel/aperitif/"
	,(format (car travel) "travel/aperitif") ,@(cdr travel))
       ("ぽれぽれサファリ" "http://www.asahi.com/travel/porepore/"
	,@(shimbun-asahi-make-regexp "travel.porepore"))
       ("スパイシー！ソウル" "http://www.asahi.com/travel/seoul/"
	,@(shimbun-asahi-make-regexp "travel.seoul"))
       ("島旅たび" "http://www.asahi.com/travel/shima/"
	,(format (car travel) "travel/shima") ,@(cdr travel))
       ("週刊シルクロード紀行" "http://www.asahi.com/travel/silkroad/"
	,@(shimbun-asahi-make-regexp "travel.silkroad"))
       ("愛の旅人" "http://www.asahi.com/travel/traveler/"
	,(format (car travel) "travel/traveler") ,@(cdr travel)))))
  "Alist of parent groups and lists of tables for subgroups.
Each table is the same as the `cdr' of the element of
`shimbun-asahi-group-table'.")

(defvar shimbun-asahi-content-start
  "<!--[\t\n ]*End of Headline[\t\n ]*-->\
\\(?:[\t\n ]*<div[\t\n ]+[^<]+</div>[\t\n ]*\
\\|[\t\n ]*<p[\t\n ]+[^<]+</p>[\t\n ]*\\)?\
\\|<!--[\t\n ]*Start of \\(Kiji\\|photo\\)[\t\n ]*-->\
\\|<!--[\t\n ]*FJZONE START NAME=\"HONBUN\"[\t\n ]*-->")

(defvar shimbun-asahi-content-end
  "\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*<!--[\t\n ]*Start of hatenab[\t\n ]*-->\
\\|<!--[\t\n ]*\\(?:google_ad_section\\|[AD★☆]+\\)\
\\|<!--[\t\n ]*End of Kiji[\t\n ]*-->\
\\|<!--[\t\n ]*End of related link[\t\n ]*-->\
\\|<!--[\t\n ]*FJZONE END NAME=\"HONBUN\"[\t\n ]*-->")

(defvar shimbun-asahi-text-content-start
  "<!--[\t\n ]*End of Headline[\t\n ]*-->\
\\(?:[\t\n ]*<div[\t\n ]+[^<]+</div>[\t\n ]*\
\\|[\t\n ]*<p[\t\n ]+[^<]+</p>[\t\n ]*\\)?\
\\|<!--[\t\n ]*Start of Kiji[\t\n ]*-->\
\\|<!--[\t\n ]*FJZONE START NAME=\"HONBUN\"[\t\n ]*-->")

(defvar shimbun-asahi-text-content-end
  "\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*<!--[\t\n ]*Start of hatenab[\t\n ]*-->\
\\|<!--[\t\n ]*\\(?:google_ad_section\\|[AD★☆]+\\)\
\\|<!--[\t\n ]*End of Kiji[\t\n ]*-->\
\\|<!--[\t\n ]*FJZONE END NAME=\"HONBUN\"[\t\n ]*-->")

(defvar shimbun-asahi-x-face-alist
  '(("default" . "X-Face: +Oh!C!EFfmR$+Zw{dwWW]1e_>S0rnNCA*CX|\
bIy3rr^<Q#lf&~ADU:X!t5t>gW5)Q]N{Mmn\n L]suPpL|gFjV{S|]a-:)\\FR\
7GRf9uL:ue5_=;h{V%@()={uTd@l?eXBppF%`6W%;h`#]2q+f*81n$B\n h|t")))

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
		    (concat (shimbun-subst-char-in-string ?. ?/ group) "/"))))
    (cond ((not index)
	   "about:blank")
	  ((string-match "\\`http:" index)
	   index)
	  ((string-match "\\`book\\." group)
	   (shimbun-expand-url (substring index 5) "http://book.asahi.com/"))
	  (t
	   (shimbun-expand-url (format index group) shimbun-asahi-url)))))

(defun shimbun-asahi-get-headers (shimbun)
  "Return a list of headers."
  (let ((group (shimbun-current-group-internal shimbun))
	(from (concat (shimbun-server-name shimbun)
		      " (" (shimbun-current-group-name shimbun) ")"))
	(case-fold-search t)
	regexp jname numbers book-p cyear cmonth rss-p paper-p en-category
	hour-min month year day serial num extra rgroup id headers
	kishi-p travel-p subgroups)
    (setq regexp (assoc group shimbun-asahi-group-table)
	  jname (nth 1 regexp)
	  numbers (nthcdr 4 regexp)
	  book-p (string-match "\\`book\\." group))
    (when (setq regexp (nth 3 regexp))
      (setq regexp (format regexp
			   (regexp-quote (shimbun-subst-char-in-string
					  ?. ?/ (if book-p
						    (substring group 5)
						  group))))))
    (setq cyear (shimbun-decode-time nil 32400)
	  cmonth (nth 4 cyear)
	  cyear (nth 5 cyear)
	  rss-p (member group '("book.rss" "rss"))
	  paper-p (member group '("editorial" "tenjin"))
	  kishi-p (string-equal group "shopping.kishi")
	  travel-p (string-equal group "travel")
	  subgroups (cdr (assoc group shimbun-asahi-subgroups-alist)))
    (shimbun-strip-cr)
    (goto-char (point-min))
    (catch 'stop
      ;; The loop for fetching all the articles in the whitemail group.
      (while t
	(when regexp
	  (while (re-search-forward regexp nil t)
	    (cond ((string-equal group "english")
		   (setq en-category
			 (save-excursion
			   (save-match-data
			     (if (re-search-backward "\
<h[0-9]\\(?:[\n\t ]+[^>]+\\)?>[\t\n ]*\\([^&]+\\)[\t\n ]*&#[0-9]+"
						     nil t)
				 (downcase (match-string 1)))))))
		  (t
		   (setq hour-min
			 (save-excursion
			   (save-match-data
			     (if (re-search-forward "\
<span[\t\n ]+[^>]+>[\t\n ]*(\\(?:[01]?[0-9]/[0-3]?[0-9][\t\n ]+\\)?
\\([012]?[0-9]:[0-5][0-9]\\))[\t\n ]*</span>"
						    nil t)
				 (match-string 1)))))))
	    (setq month (string-to-number (match-string (nth 5 numbers)))
		  year (if (setq num (nth 4 numbers))
			   (string-to-number (match-string num))
			 (cond ((>= (- month cmonth) 2)
				(1- cyear))
			       ((and (= 1 month) (= 12 cmonth))
				(1+ cyear))
			       (t
				cyear)))
		  day (string-to-number (match-string (nth 6 numbers)))
		  serial (cond (rss-p
				(format "%d%s.%s"
					year
					(match-string (nth 1 numbers))
					(match-string (nth 2 numbers))))
			       (paper-p
				(format "%d%02d%02d" year month day))
			       ((and (setq num (nth 1 numbers))
				     (match-beginning num))
				(format "%d%02d%02d.%s"
					year month day (match-string num)))
			       (t
				(shimbun-subst-char-in-string
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
			      (not (member group '("job.special"))))
			 (concat "<" serial "%" extra "." rgroup "."
				 shimbun-asahi-top-level-domain ">")
		       (concat "<" serial "%" rgroup "."
			       shimbun-asahi-top-level-domain ">")))
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
			   (paper-p
			    (concat jname (format " (%d/%d)" month day)))
			   (kishi-p
			    (save-match-data ;; XEmacs 21.4 needs it.
			      (shimbun-replace-in-string
			       (match-string (nth 3 numbers))
			       "[\t\n 　]+" " ")))
			   (travel-p
			    (save-match-data
			      (shimbun-replace-in-string
			       (match-string (nth 3 numbers))
			       "\\(?:[\t\n 　]*&#[0-9]+;\\)*[\t\n 　]*" "")))
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
			   (shimbun-replace-in-string
			    from "(RSS" (concat "(" num)))
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
		     (shimbun-expand-url
		      (match-string (nth 0 numbers))
		      (cond (paper-p
			     (concat shimbun-asahi-url "paper/"))
			    (book-p
			     "http://book.asahi.com/")
			    ((string-equal group "international.briefing")
			     "http://opendoors.asahi.com/syukan/briefing/")
			    (t
			     shimbun-asahi-url))))
		    headers))))
	(if subgroups
	    (progn
	      (erase-buffer)
	      (setq from (concat (shimbun-server-name shimbun)
				 " (" (caar subgroups) ")"))
	      (shimbun-retrieve-url (cadar subgroups))
	      (setq regexp (caddar subgroups)
		    numbers (cdddar subgroups)
		    subgroups (cdr subgroups)))
	  (throw 'stop nil))))
    (append (shimbun-sort-headers headers)
	    (shimbun-asahi-get-headers-for-today group jname from))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-asahi)
					 &optional range)
  (shimbun-asahi-get-headers shimbun))

(defun shimbun-asahi-get-headers-for-today (group jname from)
  "Return a list of the header for today's article.
It works for only the groups `editorial' and `tenjin'."
  (goto-char (point-min))
  (let ((basename (cdr (assoc group '(("editorial" . "editorial")
				      ("tenjin" . "column")))))
	year month day url)
    (when (and basename
	       (re-search-forward
		(concat
		 ;; 1. year
		 "\\(20[0-9][0-9]\\)" "年"
		 ;; 2. month
		 "\\([01]?[0-9]\\)" "月"
		 ;; 3. day
		 "\\([0-3]?[0-9]\\)" "日"
		 "（.曜日）付")
		nil t))
      (setq year (string-to-number (match-string 1))
	    month (string-to-number (match-string 2))
	    day (string-to-number (match-string 3))
	    url (format "paper/%s%d%02d%02d.html" basename year month day))
      (list
       (shimbun-make-header
	;; number
	0
	;; subject
	(shimbun-mime-encode-string (concat jname
					    (format " (%d/%d)" month day)))
	;; from
	from
	;; date
	(shimbun-make-date-string year month day "07:00")
	;; id
	(format "<%d%02d%02d%%%s.%s>"
		year month day group shimbun-asahi-top-level-domain)
	;; references, chars, lines
	"" 0 0
	;; xref
	(shimbun-expand-url url shimbun-asahi-url))))))

(defun shimbun-asahi-prepare-article (shimbun header)
  "Prepare an article.
Extract the article core on some groups or adjust a date header if
there is a correct information available.  For the groups editorial
and tenjin, it tries to fetch the article for that day if it failed."
  (let ((case-fold-search t)
	(group (shimbun-current-group-internal shimbun))
	(from (shimbun-header-from-internal header)))
    (cond
     ((string-match "\\`book\\." group)
      (when (re-search-forward "<p class=\"midasi13\">[^<>]+<br>\
\\[\\(?:文\\(?:・写真\\)?\\|評者\\)\\]\\([^<>[　]+\\)"
			       nil t)
	(let ((author (match-string 1)))
	  (when (and (string-match "（" author)
		     (not (string-match "）\\'" author)))
	    (setq author (concat author "）")))
	  (shimbun-header-set-from header author))
	(goto-char (point-min)))
      ;; Collect images.
      (let (start end images)
	(while (re-search-forward "<div[\t\n ]+class=\"bokp\">" nil t)
	  (setq start (match-beginning 0))
	  (when (and (search-forward "</div>" nil t)
		     (re-search-forward "\\([\t\n ]*<!--購入-->\\)\\|</div>"
					nil t))
	    (setq images
		  (nconc images
			 (list (buffer-substring start (or (match-beginning 1)
							   (match-end 0))))))
	    (delete-region start (point))))
	(when (and images
		   (progn
		     (goto-char (point-min))
		     (re-search-forward shimbun-asahi-content-start nil t)))
	  (insert "\n")
	  (while images
	    (insert (pop images)
		    (if images
			"<br>\n"
		      "\n"))))))
     ((string-equal group "car")
      (shimbun-remove-tags "\
\[\t\n ]*<![\t\n ]*-+[\t\n ]*[★☆]+[\t\n ]*AD[\t\n ]*[★☆]+[\t\n ]*-+>"
			   "\
<![\t\n ]*-+[\t\n ]*/[\t\n ]*[★☆]+[\t\n ]*AD[\t\n ]*[★☆]+[\t\n ]*-+>\
\[\t\n ]*")
      (shimbun-remove-tags "[\t\n ]*<form[\t\n ]+" "</form>[\t\n ]*")
      (goto-char (point-min))
      (when (and (re-search-forward (shimbun-content-start shimbun) nil t)
		 (re-search-forward "[\t\n ]*\\(?:\
<!-+[\t\n ]*Creative[\t\n ]+for[\t\n ]+\\|\
<script[\t\n ]+type=\"text/javascript\"\\)"
				    nil t))
	(goto-char (match-beginning 0))
	(insert "\n<!-- End of Kiji -->\n")))
     ((string-equal group "digital")
      (shimbun-remove-tags "\
\[\t\n ]*<![\t\n ]*-+[\t\n ]*[★☆]+[\t\n ]*AD[\t\n ]*[★☆]+[\t\n ]*-+>"
			   "\
<![\t\n ]*-+[\t\n ]*/[\t\n ]*[★☆]+[\t\n ]*AD[\t\n ]*[★☆]+[\t\n ]*-+>\
\[\t\n ]*")
      (cond ((string-match "コラム" from)
	     (unless (re-search-forward (shimbun-content-end shimbun) nil t)
	       (when (re-search-forward "\\(?:[\t\b ]*<[^>]+>\\)*[\t\n ]*\
\\(?:<img[\t\n ]+src=\"[^>]*[\t\n ]*alt=\"プロフィール\"\
\\|<h[0-9]>プロフィール</h[0-9]>\\)"
					nil t)
		 (goto-char (match-beginning 0))
		 (insert "\n<!-- End of Kiji -->\n"))))))
     ((string-equal group "editorial")
      (let ((regexp "<h[0-9][\t\n ]+class=\"topi\">")
	    (retry 0)
	    index from)
	(while (<= retry 1)
	  (if (re-search-forward regexp nil t)
	      (progn
		(goto-char (match-beginning 0))
		(insert "<!-- Start of Kiji -->")
		(when index
		  (insert "\
\n<p>(指定された&nbsp;url&nbsp;が&nbsp;まだ/すでに&nbsp;無いので、\
<a href=\"" index "\">トップページ</a> から記事を取得しました)</p>\n"))
		(while (and (search-forward "</p>" nil t)
			    (progn
			      (setq from (match-end 0))
			      (re-search-forward regexp nil t)))
		  (delete-region from (match-beginning 0)))
		(insert "\n<!-- End of Kiji -->")
		(setq retry 255))
	    (erase-buffer)
	    (if (zerop retry)
		(progn
		  (shimbun-retrieve-url (setq index
					      (shimbun-index-url shimbun)))
		  (goto-char (point-min)))
	      (insert "Couldn't retrieve the page.\n")))
	  (setq retry (1+ retry)))))
     ((string-match "\\`housing\\'\\|\\`housing\\." group)
      (shimbun-remove-tags
       "\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*<!--広告スキップ -->"
       "<!--/広告スキップのとび先-->[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*"))
     ((string-equal group "international.briefing")
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
     ((string-equal group "tenjin")
      (let ((retry 0)
	    index)
	(while (<= retry 1)
	  (if (re-search-forward "<SPAN STYLE=[^>]+>[\t\n ]*" nil t)
	      (progn
		(insert "<!-- Start of Kiji -->")
		(when index
		  (insert "\
\n<p>(指定された&nbsp;url&nbsp;が&nbsp;まだ/すでに&nbsp;無いので、\
<a href=\"" index "\">トップページ</a> から記事を取得しました)</p>\n"))
		(while (re-search-forward "[\t\n ]*<SPAN STYLE=[^>]+>[\t\n ]*"
					  nil t)
		  (delete-region (match-beginning 0) (match-end 0)))
		(when (re-search-forward "[\t\n ]*</SPAN>" nil t)
		  (goto-char (match-beginning 0))
		  (insert "\n<!-- End of Kiji -->"))
		(setq retry 255))
	    (erase-buffer)
	    (if (zerop retry)
		(progn
		  (shimbun-retrieve-url (setq index
					      (shimbun-index-url shimbun)))
		  (goto-char (point-min)))
	      (insert "Couldn't retrieve the page.\n")))
	  (setq retry (1+ retry)))))
     ((string-equal group "shopping")
      (let ((subgroup (shimbun-header-xref header)))
	(when (string-match "\\([^/]+\\)/[^/]+\\'" subgroup)
	  (setq subgroup (match-string 1 subgroup))
	  (cond ((string-equal subgroup "asapaso")
		 (when (re-search-forward
			"<div[\t\n ]+id=\"photo[0-9]+\">[\t\n ]*"
			nil t)
		   (delete-region (point-min) (point))
		   (insert "<!-- Start of Kiji -->")
		   (when (re-search-forward "\
\\(?:[\t\n ]*<[^>]+>\\)*<div[\t\n ]+class=\"gotobacknumber\">"
					    nil t)
		     (goto-char (match-beginning 0))
		     (insert "<!-- End of Kiji -->"))))
		((string-equal subgroup "interiorlife")
		 (when (re-search-forward
			"<p[\t\n ]+class=\"intro\">[\t\n ]*"
			nil t))
		 (delete-region (point-min) (point))
		 (insert "<!-- Start of Kiji -->")
		 (when (re-search-forward "\
\\(?:[\t\n ]*<[^>]+>\\)*<div[\t\n ]+class=\"gotobacknumber\">"
					  nil t)
		   (goto-char (match-beginning 0))
		   (insert "<!-- End of Kiji -->")))
		((string-equal subgroup "dvd")
		 (let ((regexp (shimbun-content-end shimbun)))
		   (while (re-search-forward regexp nil t)
		     (replace-match "\n")))
		 (goto-char (point-min))
		 (when (re-search-forward "\\(?:\
<!--特集-->\\|<div[\t\n ]+id=kijih>\\|<!--[\t\n ]+Start of Headline[\t\n ]+-->\
\\)[\t\n ]*"
					  nil t)
		   (insert "<!-- Start of Kiji -->")
		   (when (re-search-forward "\
\[\t\n ]*\\(?:<!--/作品紹介ここまで-->\\|<!--/特集-->\\)"
					    nil t)
		     (insert "<!-- End of Kiji -->"))))
		((member subgroup '("hobby" "music"))
		 (let ((regexp (shimbun-content-end shimbun)))
		   (while (re-search-forward regexp nil t)
		     (replace-match "\n")))
		 (goto-char (point-min))
		 (when (re-search-forward "\
\[\t\n ]*\\(?:<!--/作品紹介ここまで-->\\|<!--/特集-->\\)"
					  nil t)
		   (insert "<!-- End of Kiji -->")))))))
     ((string-equal group "shopping.interiorlife")
      (when (re-search-forward "<p[\t\n ]+class=\"intro\">[\t\n ]*" nil t)
	(delete-region (point-min) (match-end 0))
	(insert "<!-- Start of Kiji -->")
	(when (re-search-forward
	       "[\t\n ]*<div[\t\n ]+class=\"gotobacknumber\">"
	       nil t)
	  (goto-char (match-beginning 0))
	  (insert "<!-- End of Kiji -->"))))
     ((string-equal group "shopping.kishi")
      (when (re-search-forward "<div[\t\n ]+id=\"kijih\">[\t\n ]*" nil t)
	(insert "<!-- Start of Kiji -->")))
     ((string-equal group "rss"))
     ((string-equal group "world.china")
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
	  (insert "\n<!-- End of Kiji -->\n"))))
     (t
      (when (re-search-forward
	     (eval-when-compile
	       (let ((s0 "[\t\n ]*")
		     (s1 "[\t\n ]+"))
		 (concat "<\\(?:div\\|p\\)"
			 s1 "class" s0 "=" s0 "\"day\"" s0 ">" s0
			 ;; 1. year
			 "\\(20[0-9][0-9]\\)年"
			 ;; 2. month
			 "\\([01]?[0-9]\\)月"
			 ;; 3. day
			 "\\([0-3]?[0-9]\\)日"
			 ;; 4. hour
			 "\\([012]?[0-9]\\)時"
			 ;; 5. minute
			 "\\([0-5]?[0-9]\\)分"
			 s0 "</\\(?:div\\|p\\)>")))
	     nil t)
	(shimbun-header-set-date
	 header
	 (shimbun-make-date-string
	  (string-to-number (match-string 1))
	  (string-to-number (match-string 2))
	  (string-to-number (match-string 3))
	  (concat (match-string 4) ":" (match-string 5))
	  "+0900")))))
    (shimbun-with-narrowed-article
     shimbun
     ;; Remove sitesearch area.
     (when (re-search-forward "[\t\n ]*\\(?:<div[\t\n ]+[^>]+>[\t\n ]*\\)+\
この記事の関連情報をアサヒ・コム内で検索する"
			      nil t)
       (goto-char (match-beginning 0))
       (insert "\n<!-- End of Kiji -->"))
     ;; Remove adv.
     (goto-char (point-min))
     (when (re-search-forward "[\t\n ]*<p[\t\n ]+class=\"hide\">[\t\n ]\
*ここから広告です[\t\n ]*</p>"
			      nil t)
       (let ((start (match-beginning 0)))
	 (when (re-search-forward "<p[\t\n ]+class=\"hide\">[\t\n ]*\
広告終わり\\(?:[\t\n ]*</p>[\t\n ]*\\|\\'\\)"
				  nil t)
	   (delete-region start (match-end 0))))))))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-asahi)
						   header)
  (shimbun-asahi-prepare-article shimbun header))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-asahi)
						    header)
  (when (luna-call-next-method)
    (unless (shimbun-prefer-text-plain-internal shimbun)
      (shimbun-break-long-japanese-lines))
    t))

(provide 'sb-asahi)

;;; sb-asahi.el ends here

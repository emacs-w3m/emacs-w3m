;;; sb-asahi.el --- shimbun backend for asahi.com -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005
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
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(require 'shimbun)
(require 'sb-text)

(luna-define-class shimbun-asahi
		   (shimbun-japanese-newspaper shimbun-text) ())

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
	      (no-nl "[^\n]+"))
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
		  s0 "</a>"))
	1 nil 2 6 3 4 5))

(defvar shimbun-asahi-group-table
  (let* ((s0 "[\t\n 　]*")
	 (s1 "[\t\n ]+")
	 (no-nl "[^\n]+")
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
		    s0 "</a>")
		   1 4 nil 5 nil 2 3))
	 (default2 (shimbun-asahi-make-regexp "%s"))
	 (default3 (list
		    (concat
		     "<a" s1 "href=\"/+"
		     ;; 1. url
		     "\\(.+/"
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
		     s0 "</a>")
		    1 nil 2 6 3 4 5))
	 (edu (shimbun-asahi-make-regexp "edu.news"))
	 (health (shimbun-asahi-make-regexp "health.news")))
    `(("book" "出版ニュース" "book/news/"
       ,@(shimbun-asahi-make-regexp "book.news"))
      ("business" "ビジネス" "%s/list.html" ,@default)
      ;; The url should be ended with "index.html".
      ("business.column" "経済気象台" "business/column/index.html" ,@default2)
      ("car" "愛車" "%s/news/" ,@(shimbun-asahi-make-regexp "car.news"))
      ("car.italycolumn" "イタリア発アモーレ！モトーレ！" "car/italycolumn/"
       ,@default2)
      ("car.motorsports" "モータースポーツ" "car/motorsports/" ,@default2)
      ("car.newcar" "新車情報" "car/newcar/" ,@default2)
      ("car.newcarbywebcg" "新車発表会" "car/newcarbywebcg/" ,@default2)
      ("culture" "文化・芸能" "%s/list.html" ,@default)
      ("culture.column" "もやしのひげ" "culture/list_moyashi.html"
       ,@(shimbun-asahi-make-regexp "culture.column.moyashi"))
      ("digital" "デジタル機器" "digital/av/"
       ,@(shimbun-asahi-make-regexp "digital.av"))
      ("digital.apc" "雑誌「ASAHIパソコン」ニュース" "digital/apc/" ,@default2)
      ("digital.bcnnews" "eビジネス情報 (提供：ＢＣＮ)" "digital/bcnnews/"
       ,@default2)
      ("digital.column01" "デジタルコラム" "digital/column01/"
       ,@default2)
      ("digital.hotwired" "HotWired Japan" "digital/hotwired/" ,@default2)
      ("digital.internet" "ネット・ウイルス" "digital/internet/" ,@default2)
      ("digital.mobile" "携帯電話" "digital/mobile/" ,@default2)
      ("digital.nikkanko" "日刊工業新聞ニュース" "digital/nikkanko/"
       ,@default2)
      ("digital.pc" "パソコン" "digital/pc/" ,@default2)
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
      ("edu" "教育" "%s/news/index.html" ,@edu)
      ("edu.column" "教育コラム" "edu/column/ikuji/"
       ,@(shimbun-asahi-make-regexp "edu.column.ikuji"))
      ("edu.it" "IT教育" "edu/news/it.html" ,@edu)
      ("edu.kosodate" "子育て" "edu/news/kosodate.html" ,@edu)
      ("edu.nyushi" "大学・入試" "edu/news/nyushi.html" ,@edu)
      ("edu.tamate" "ののちゃんのふしぎ玉手箱" "edu/nie/tamate/"
       ,@(shimbun-asahi-make-regexp "edu.nie.tamate.kiji"))
      ("english" "ENGLISH" "%s/index.html"
       ,@(let ((rest (shimbun-asahi-make-regexp "english.Herald-asahi")))
	   (cons (concat (car rest)
			 "\\(" s0 "<[^>]+>\\)*" s0 "([01]?[0-9]/[0-3]?[0-9])")
		 (cdr rest))))
      ("health" "健康・生活" "%s/news/" ,@health)
      ("health.aged" "福祉・高齢" "health/news/aged.html" ,@health)
      ("health.alz" "認知症特集" "health/news/alz.html" ,@health)
      ("health.medical" "医療・病気" "health/news/medical.html" ,@health)
      ("housing" "住まい" "%s/news/"
       ,@(shimbun-asahi-make-regexp "housing.news"))
      ("housing.amano" "天野彰のいい家いい家族" "housing/amano/" ,@default2)
      ("housing.column" "住まいのお役立ちコラム" "housing/column/" ,@default2)
      ("housing.diary" "小さな家の生活日記" "housing/diary/" ,@default2)
      ("housing.world" "世界のウチ" "housing/world/" ,@default2)
      ("igo" "囲碁" "%s/news/" ,@(shimbun-asahi-make-regexp "igo.news"))
      ("international" "国際" "%s/list.html" ,@default)
      ("international.jinmin" "人民日報" "international/jinmin/index.html"
       ,@default2)
      ("job" "就職・転職" "%s/news/"
       ,@(shimbun-asahi-make-regexp "job.news"))
      ("job.special" "週刊朝日・ＡＥＲＡから" "job/special/"
       ,(concat
	 (car default2)
	 "\\(" s0 "<[^>]+>\\)*" s0 "（" s0
	 ;; 8. extra
	 "\\(" no-nl "\\)"
	 "：")
       ,@(cdr default2) nil 8)
      ("kansai" "関西" "%s/news/" ,@(shimbun-asahi-make-regexp "kansai.news"))
      ("kansai.horiekenichi" "堀江謙一の世界一周ひとりぼっち"
       "kansai/horiekenichi/" ,@default2)
      ("kansai.umaimon" "うまいもん" "kansai/umaimon/" ,@default2)
      ("kansai.fuukei" "風景を歩く" "kansai/fuukei/" ,@default2)
      ("kansai.yotsuba" "よつ葉びより" "kansai/yotsuba/" ,@default2)
      ("kansai.smile" "スマイルスタイル" "kansai/smile/" ,@default2)
      ("kansai.keiki" "け〜きの“ええ話”" "kansai/keiki/" ,@default2)
      ("kansai.okiniiri" "DJのお気に入り" "kansai/okiniiri/" ,@default2)
      ("kansai.syun" "旬の顔" "kansai/syun/" ,@default2)
      ("kansai.takara" "たから図鑑" "kansai/takara/" ,@default2)
      ("kansai.kansaiisan" "勝手に関西世界遺産" "kansai/kansaiisan/"
       ,@default2)
      ("kansai.depa" "デパ地下ＮＥＷＳ" "kansai/depa/" ,@default2)
      ("kansai.kaban" "かばんの中身" "kansai/kaban/" ,@default2)
      ("kansai.kyosho" "巨匠に学べ" "kansai/kyosho/" ,@default2)
      ("kansai.okan" "母さんの知恵袋" "kansai/okan/" ,@default2)
      ("kansai.densetsu" "ほんま？関西伝説" "kansai/densetsu/" ,@default2)
      ("kansai.onayami" "みうらじゅんのお悩み祭り" "kansai/onayami/"
       ,@default2)
      ("kansai.sanshi" "三枝の笑ウインドウ" "kansai/sanshi/" ,@default2)
      ("life" "暮らし" "%s/list.html" ,@default)
      ("life.column" "暮らしコラム" "life/column/"
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
	 s0 "</a>")
       1 nil 2 6 3 4 5)
      ("life.food" "食と料理" "life/food/"
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
	 s0 "</a>")
       1 nil 2 6 3 4 5)
      ("nankyoku" "南極プロジェクト" "%s/news/"
       ,@(shimbun-asahi-make-regexp "nankyoku.news"))
      ("nankyoku.borderless" "国境のない大陸から" "nankyoku/borderless/"
       ,@default2)
      ("nankyoku.people" "越冬隊の人びと" "nankyoku/people/" ,@default2)
      ("nankyoku.whitemail" "WhiteMail＠南極" "nankyoku/whitemail/" ,@default2)
      ("national" "社会" "%s/list.html" ,@default)
      ("national.calamity" "災害・交通情報" "national/calamity.html"
       ,@default3)
      ("national.etc" "その他・話題" "national/etc.html" ,@default3)
      ("national.trial" "裁判" "national/trial.html" ,@default3)
      ("obituaries" "おくやみ" "obituaries" ,@default)
      ("politics" "政治" "%s/list.html" ,@default)
      ("rss" "RSS" "http://www3.asahi.com/rss/index.rdf"
       ,(concat
	 "<title>"
	 ;; 1. subject
	 "\\([^<]+\\)"
	 "</title>\n<link>"
	 ;; 2. url
	 "\\(http://www\\.asahi\\.com/"
	 ;; 3. extra keyword (en)
	 "\\([^/]+\\)"
	 "/update/"
	 ;; 4 and 5. serial number
	 "\\([0-9]+\\)/\\([0-9]+\\)"
	 "\\.html\\?ref=rss\\)"
	 "</link>\n<description/>\n<dc:subject>"
	 ;; 6. extra keyword (ja)
	 "\\([^<]+\\)"
	 "</dc:subject>\n<dc:date>20[0-9][0-9]-"
	 ;; 7. month
	 "\\([01][0-9]\\)"
	 "-"
	 ;; 8. day.
	 "\\([0-3][0-9]\\)"
	 "T"
	 ;; 9. hour:min:sec
	 "\\([012][0-9]:[0-5][0-9]:[0-5][0-9]\\)")
       2 4 5 1 nil 7 8 9 3 nil 6)
      ("science" "サイエンス" "%s/list.html"
       ,@(shimbun-asahi-make-regexp "science.news"))
      ("shopping" "ショッピング" "%s/news/"
       ,@(shimbun-asahi-make-regexp "shopping.news"))
      ("shopping.kishi" "岸朝子の気になるお取り寄せ12カ月" "shopping/kishi/"
       ,@default2)
      ("shopping.ryouhin" "くらしの良品探訪" "shopping/ryouhin/"
       ,@default2)
      ("shougi" "将棋" "%s/news/" ,@(shimbun-asahi-make-regexp "shougi.news"))
      ("sports" "スポーツ" "%s/list.html" ,@default)
      ("sports.baseball" "野球" "sports/bb/"
       ,@(shimbun-asahi-make-regexp "sports.bb"))
      ("sports.column" "スポーツコラム" "sports/column/" ,@default2)
      ("sports.football" "サッカー" "sports/fb/"
       ,@(shimbun-asahi-make-regexp "sports.fb"))
      ("sports.spo" "一般スポーツ" "sports/spo/" ,@default2)
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
      ("travel.kaido" "司馬遼太郎・街道をゆく" "travel/kaido/" ,@default2)
      ("travel.matsuri" "日本の祭り" "travel/matsuri/"
       ,@default2)
      ("travel.zeitaku" "地球の贅たく" "travel/zeitaku/"
       ,@default2)
      ("wakamiya" "風考計 (論説主幹・若宮啓文)" "column/wakamiya/"
       ,@(shimbun-asahi-make-regexp "column.wakayama"))))
  "Alist of group names, their Japanese translations, index pages,
regexps and numbers.  Where index pages and regexps may contain the
\"%s\" token which is replaced with group names, numbers point to the
search result in order of [0]a url, [1,2]a serial number, [3]a subject,
\[4]a year, [5]a month, [6]a day, [7]an hour:minute and [8,9,10]an
extra keyword.")

(defvar shimbun-asahi-content-start
  "<!--[\t\n ]*Start of Kiji[\t\n ]*-->\
\\|<!--[\t\n ]*FJZONE START NAME=\"HONBUN\"[\t\n ]*-->")

(defvar shimbun-asahi-content-end
  "<!--[\t\n ]*End of Kiji[\t\n ]*-->\
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
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-asahi))
  (mapcar 'car shimbun-asahi-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-asahi))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-asahi-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-asahi))
  (let* ((group (shimbun-current-group-internal shimbun))
	 (index (nth 2 (assoc group shimbun-asahi-group-table))))
    (if (string-match "\\`http:" index)
	index
      (concat shimbun-asahi-url (format index group)))))

(defun shimbun-asahi-get-headers (shimbun)
  "Return a list of headers."
  (let ((group (shimbun-current-group-internal shimbun))
	(from (concat (shimbun-server-name shimbun)
		      " (" (shimbun-current-group-name shimbun) ")"))
	(case-fold-search t)
	regexp jname numbers cyear cmonth rss-p paper-p en-category
	hour-min month year day serial num extra rgroup id headers
	backnumbers)
    (setq regexp (assoc group shimbun-asahi-group-table)
	  jname (nth 1 regexp)
	  numbers (nthcdr 4 regexp)
	  regexp (format (nth 3 regexp)
			 (regexp-quote (shimbun-subst-char-in-string
					?. ?/ group)))
	  cyear (decode-time)
	  cmonth (nth 4 cyear)
	  cyear (nth 5 cyear)
	  rss-p (string-equal group "rss")
	  paper-p (member group '("editorial" "tenjin")))
    (catch 'stop
      ;; The loop for fetching all the articles in the whitemail group.
      (while t
	(while (re-search-forward regexp nil t)
	  (cond ((string-equal group "english")
		 (setq en-category
		       (save-excursion
			 (save-match-data
			   (if (re-search-backward "\
<h[0-9]\\([\n\t ]+[^>]+\\)?>[\t\n ]*\\([^&]+\\)[\t\n ]*&#[0-9]+"
						   nil t)
			       (downcase (match-string 2)))))))
		(t
		 (setq hour-min
		       (save-excursion
			 (save-match-data
			   (if (re-search-forward "\
<span[\t\n ]+[^>]+>[\t\n ]*(\\([01]?[0-9]/[0-3]?[0-9][\t\n ]+\\)?
\\([012]?[0-9]:[0-5][0-9]\\))[\t\n ]*</span>"
						  nil t)
			       (match-string 2)))))))
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
	  (unless (and (shimbun-search-id shimbun id)
		       (if backnumbers
			   (throw 'stop nil)
			 ;; Don't stop it since there might be more new
			 ;; articles even if the same links are repeated.
			 t))
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
			 (t
			  (match-string (nth 3 numbers))))
		   ;; from
		   (if (and rss-p
			    (setq num (nth 10 numbers))
			    (setq num (match-string num)))
		       (save-match-data
			 (shimbun-replace-in-string
			  from "(RSS" (concat "\\&:" num)))
		     from)
		   ;; date
		   (shimbun-make-date-string
		    year month day (cond ((and (setq num (nth 7 numbers))
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
		    (if paper-p
			(concat shimbun-asahi-url "paper/")
		      shimbun-asahi-url)))
		  headers)))
	(if (string-equal group "nankyoku.whitemail")
	    (progn
	      (cond ((eq backnumbers 'stop)
		     (throw 'stop nil))
		    ((null backnumbers)
		     (while (re-search-forward "<a[\t\n ]+href=\"\
\\(http://www\\.asahi\\.com/nankyoku/whitemail/\
backnum0[345][01][0-9]\\.html\\)\">"
					       nil t)
		       (unless (member (setq id (match-string 1)) backnumbers)
			 (push id backnumbers)))))
	      (if backnumbers
		  (progn
		    (shimbun-retrieve-url
		     (prog1
			 (car backnumbers)
		       (erase-buffer)
		       (unless (setq backnumbers (cdr backnumbers))
			 (setq backnumbers 'stop)))))
		(throw 'stop nil)))
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
	(group (shimbun-current-group-internal shimbun)))
    (cond
     ((string-equal group "editorial")
      (let ((regexp "\
<h[0-9]\\([\t\n ]+[^>]+\\)?>[\t\n ]*<a[\t\n ]+name=\"syasetu[0-9]+\">")
	    (retry 0)
	    index)
	(while (<= retry 1)
	  (if (re-search-forward regexp nil t)
	      (progn
		(goto-char (match-beginning 0))
		(insert "<!-- Start of Kiji -->")
		(when index
		  (insert "\
\n<p>(指定された&nbsp;url&nbspが&nbspまだ/すでに&nbsp無いので、\
<a href=\"" index "\">トップページ</a> から記事を取得しました)</p>\n"))
		(search-forward "</a>" nil t)
		(while (re-search-forward regexp nil t))
		(when (re-search-forward "[\n\t ]*</p>" nil t)
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
     ((string-equal group "tenjin")
      (let ((retry 0)
	    index)
	(while (<= retry 1)
	  (if (and (search-forward "【天声人語】" nil t)
		   (re-search-forward "<SPAN STYLE=[^>]+>[\t\n ]*" nil t))
	      (progn
		(insert "<!-- Start of Kiji -->")
		(when index
		  (insert "\
\n<p>(指定された&nbsp;url&nbspが&nbspまだ/すでに&nbsp無いので、\
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
     (t
      (when (re-search-forward
	     (eval-when-compile
	       (let ((s0 "[\t\n ]*")
		     (s1 "[\t\n ]+"))
		 (concat "<p" s1 "class" s0 "=" s0 "\"day\"" s0 ">" s0
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
			 s0 "</p>")))
	     nil t)
	(shimbun-header-set-date
	 header
	 (shimbun-make-date-string
	  (string-to-number (match-string 1))
	  (string-to-number (match-string 2))
	  (string-to-number (match-string 3))
	  (concat (match-string 4) ":" (match-string 5))
	  "+0900"))))))
  (goto-char (point-min)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-asahi)
						   header)
  (shimbun-asahi-prepare-article shimbun header))

(provide 'sb-asahi)

;;; sb-asahi.el ends here

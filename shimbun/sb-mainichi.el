;;; sb-mainichi.el --- shimbun backend for MSN-Mainichi -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007
;; Koichiro Ohba <koichiro@meadowy.org>

;; Author: Koichiro Ohba <koichiro@meadowy.org>
;;         Katsumi Yamaoka <yamaoka@jpl.org>
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

;; Original code was sb-yomiuri.el which is written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org> and
;; Yuuichi Teranishi <teranisi@gohome.org>

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-mainichi (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-mainichi-prefer-text-plain nil
  "*Non-nil means prefer text/plain articles rather than html articles.")

(defvar shimbun-mainichi-top-level-domain "mainichi-msn.co.jp"
  "Name of the top level domain for the MSN-Mainichi INTERACTIVE.")

(defvar shimbun-mainichi-url
  (concat "http://www." shimbun-mainichi-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-mainichi-header-regexp-default
  (let ((s0 "[\t\n ]*")
	(s1 "[\t\n ]+"))
    (list
     (concat
      "<a" s1 "href=\"/"
      ;; 1. url
      "\\("
      "\\(?:[^\t\n \"/]+/\\)+news/\\(?:20[0-9][0-9]/\\(?:[01]?[0-9]/\\)?\\)?"
      ;; 2. serial number
      "\\("
      ;; 3. year
      "\\(20[0-9][0-9]\\)"
      ;; 4. month
      "\\([01][0-9]\\)"
      ;; 5. day
      "\\([0-3][0-9]\\)"
      "\\(?:[^\t\n \"./]\\)+\\)"
      "\\.html\\)"
      "[^>]*>" s0
      ;; 6 subject
      "\\([^<]+\\)"
      s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>"
      "\\(?:" s0 "</td>" s0 "<td" s1 "class=\"time\">"
      s0 "<span" s1 "[^>]+>" s0
      ;; 7?. hour
      "\\([012]?[0-9]\\)"
      ":"
      ;; 8?. minute
      "\\([0-5]?[0-9]\\)"
      "\\)?")
     1 2 3 4 5 6 7 8))
  "List of the default regexp used to extract headers and matching numbers.")

;; Emacs 21 needs increasing `max-lisp-eval-depth' and `max-specpdl-size'
;; when compiling `shimbun-mainichi-group-table' for some unknown reason.
(eval-when-compile
  (if (and (not (featurep 'xemacs))
	   (= emacs-major-version 21)
	   (fboundp 'byte-compile-file-form-defvar))
      (fset 'byte-compile-file-form-defvar
	    `(lambda (form)
	       (let ((fn ,(symbol-function 'byte-compile-file-form-defvar)))
		 (unwind-protect
		     (let ((max-lisp-eval-depth 1000)
			   (max-specpdl-size 1000))
		       (funcall fn form))
		   (fset 'byte-compile-file-form-defvar fn)))))))

(defvar shimbun-mainichi-group-table
  `(("entertainment" "エンターテインメント" "about:blank" none)
    ("eye" "毎日の視点" "about:blank" none)
    ("eye.shasetsu" "社説" nil
     ,(let ((s0 "[\t\n ]*")
	    (s1 "[\t\n ]+"))
	(concat
	 "<a" s1 "href=\"/"
	 ;; 1. url
	 "\\(eye/shasetsu/\\(?:news\\|archive/news/20[0-9][0-9]/[01][0-9]\\)/"
	 ;; 2. serial number
	 "\\("
	 ;; 3. year
	 "\\(20[0-9][0-9]\\)"
	 ;; 4. month
	 "\\([01][0-9]\\)"
	 ;; 5. day
	 "\\([0-3][0-9]\\)"
	 "\\)"
	 "[^\"]+\\)"
	 "\"[^>]*>" s0
	 ;; 6 subject
	 "\\([^<]+\\)"
	 s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>"
	 "\\(?:" s0 "</td>" s0 "<td" s1 "class=\"time\">"
	 s0 "<span" s1 "[^>]+>" s0
	 ;; 7?. hour
	 "\\([012]?[0-9]\\)"
	 ":"
	 ;; 8?. minute
	 "\\([0-5]?[0-9]\\)"
	 "\\)?"))
     1 2 3 4 5 6 7 8)
    ("kansai" "めっちゃ関西" "about:blank" none)
    ("kansai.tigers" "がんばれ！タイガース" "about:blank" none)
    ("keizai" "経済・IT" "about:blank" none)
    ("kokusai" "国際" "about:blank" none)
    ("kurashi" "暮らし" "about:blank" none)
    ("science" "サイエンス" "about:blank" none)
    ("seiji" "政治" "about:blank" none)
    ("shakai" "社会" "about:blank" none)
    ("shakai.edu" "教育" "about:blank" none)
    ("shakai.edu.campal" "キャンパる"
     "http://www.mainichi-msn.co.jp/shakai/edu/campal/archive/")
    ("sokuhou" "速報")
    ("sports" "スポーツ" "about:blank" none)
    ("sports.column" "コラム" "about:blank" none)
    ("today" "今日の話題" "rss/wadai.rdf"
     ,(let ((s0 "[\t\n ]*"))
	(concat
	 "<title>" s0
	 ;; 1. subject
	 "\\([^<]+\\)"
	 s0 "</title>" s0 "<link>" s0
	 ;; 2. url
	 "\\([^<]+/"
	 ;; 3. serial number
	 "\\([^./]+\\)"
	 "\\.html\\?in=rssw\\)"
	 s0 "</link>" s0 "<description/>" s0 "<dc:subject>[^<]+</dc:subject>"
	 s0 "<dc:date>" s0
	 ;; 4. year
	 "\\(20[0-9][0-9]\\)"
	 "-"
	 ;; 5. month
	 "\\([01]?[0-9]\\)"
	 "-"
	 ;; 6. day
	 "\\([0-3]?[0-9]\\)"
	 "T"
	 ;; 7. hour
	 "\\([0-2]?[0-9]\\)"
	 ":"
	 ;; 8. minute
	 "\\([0-5]?[0-9]\\)"))
     2 3 4 5 6 1 7 8)
    ("yougo" "ニュースな言葉"))
  "Alist of group names, their Japanese translations, index pages, regexps
and numbers.  Where numbers point to the regexp search result in order
of [0]a url, [1]a serial number, [2]a year, [3]a month, [4]a day,
\[5]a subject, \[6]an hour and [7]a minute.  If regexp and number are
omitted, the value of `shimbun-mainichi-header-regexp-default' is used
by default.

The value `none' for regexp means there is no header in the group (they
can be found in the subgroups).  In that case, the index page should be
set to \"about:blank\".")

(defvar shimbun-mainichi-subgroups-alist
  (let* ((s0 "[\t\n ]*")
	 (s1 "[\t\n ]+")
	 (kansai
	  (list
	   (concat
	    "<a" s1 "href=\"/"
	    ;; 1. url
	    "\\(kansai\\(?:/[^\"/]+\\)+/archive/news/"
	    "\\(?:20[0-9][0-9]/\\(?:[01][0-9]/\\)?\\)?"
	    ;; 2. serial number
	    "\\("
	    ;; 3. year
	    "\\(20[0-9][0-9]\\)"
	    ;; 4. month
	    "\\([01][0-9]\\)"
	    ;; 5. day
	    "\\([0-3][0-9]\\)"
	    "[^\".]+\\)"
	    "[^\"]+\\)"
	    "[^>]*>" s0
	    ;; 6. subject
	    "\\([^<]+\\)")
	   1 2 3 4 5 6)))
    `(("entertainment"
       ("車" "car" "http://www.mainichi-msn.co.jp/entertainment/car/")
       ("映画" "cinema" "http://www.mainichi-msn.co.jp/entertainment/cinema/")
       ("ゲーム" "game" "http://www.mainichi-msn.co.jp/entertainment/game/")
       ("芸能" "geinou" "http://www.mainichi-msn.co.jp/entertainment/geinou/")
       ("碁" "igo" "http://www.mainichi-msn.co.jp/entertainment/igo/")
       ("アニメ・マンガ" "manga"
	"http://www.mainichi-msn.co.jp/entertainment/manga/")
       ("音楽" "music" "http://www.mainichi-msn.co.jp/entertainment/music/")
       ("将棋" "shougi" "http://www.mainichi-msn.co.jp/entertainment/shougi/")
       ("テレビ" "tv" "http://www.mainichi-msn.co.jp/entertainment/tv/"))
      ("eye"
       ("クローズアップ" "closeup"
	"http://www.mainichi-msn.co.jp/eye/closeup/")
       ("発信箱" "hassinbako" "http://www.mainichi-msn.co.jp/eye/hassinbako/")
       ("ひと" "hito" "http://www.mainichi-msn.co.jp/eye/hito/")
       ("土曜解説" "kaisetsu" "http://www.mainichi-msn.co.jp/eye/kaisetsu/")
       ("近事片々" "kinji" "http://www.mainichi-msn.co.jp/eye/kinji/")
       ("記者の目" "kishanome" "http://www.mainichi-msn.co.jp/eye/kishanome/")
       ("！" "mieru" "http://www.mainichi-msn.co.jp/eye/mieru/")
       ("ニュースＵＰ" "newsup" "http://www.mainichi-msn.co.jp/eye/newsup/")
       ("余録" "yoroku" "http://www.mainichi-msn.co.jp/eye/yoroku/")
       ("憂楽帳" "yuuraku" "http://www.mainichi-msn.co.jp/eye/yuuraku/"))
      ("eye.shasetsu"
       ("社説" nil "http://www.mainichi-msn.co.jp/eye/shasetsu/archive/"
	,@(nthcdr 3 (assoc "eye.shasetsu" shimbun-mainichi-group-table))))
      ("kansai"
       ("21世紀フォーラム" "21cen.wukansai"
	"http://www.mainichi-msn.co.jp/kansai/wukansai/21cen/archive/"
	,@kansai)
       ("あなたと歩きたい" "aruki"
	"http://www.mainichi-msn.co.jp/kansai/aruki/archive/" ,@kansai)
       ("元気が出る商売の話" "genki.salon"
	"http://www.mainichi-msn.co.jp/kansai/salon/genki/archive/" ,@kansai)
       ("ふらっと〜女ひとり旅" "hitoritabi"
	"http://www.mainichi-msn.co.jp/kansai/hitoritabi/archive/" ,@kansai)
       ("酩酊八十八カ所〜大人の遠足" "meitei"
	"http://www.mainichi-msn.co.jp/kansai/meitei/archive/" ,@kansai)
       ("フォトジャーナル" "photojournal"
	"http://www.mainichi-msn.co.jp/kansai/photojournal/archive/" ,@kansai)
       ("三枝の楽屋へいらっしゃ〜い！" "sanshi"
	"http://www.mainichi-msn.co.jp/kansai/sanshi/archive/" ,@kansai))
      ("kansai.tigers"
       ("熱闘録2007" "netouroku2007"
	"http://www.mainichi-msn.co.jp/kansai/tigers/netouroku2007/archive/"
	,@kansai)
       ("トラ・とぴ" "topics"
	"http://www.mainichi-msn.co.jp/kansai/tigers/topics/archive/"
	,@kansai)
       ("トラ・トラ・フィーバー" "toratora2"
	"http://www.mainichi-msn.co.jp/kansai/tigers/toratora2/archive/"
	,@kansai)
       ("やじうまタイガース2007" "yajiuma2007"
	"http://www.mainichi-msn.co.jp/kansai/tigers/yajiuma2007/archive/"
	,@kansai))
      ("keizai"
       ("IT" "it" "http://www.mainichi-msn.co.jp/keizai/it/")
       ("海外" "kaigai" "http://www.mainichi-msn.co.jp/keizai/kaigai/")
       ("企業" "kigyou" "http://www.mainichi-msn.co.jp/keizai/kigyou/")
       ("企業情報" "info.kigyou"
	"http://www.mainichi-msn.co.jp/keizai/kigyou/info/")
       ("金融・株" "kinyu" "http://www.mainichi-msn.co.jp/keizai/kinyu/")
       ("政策" "seisaku" "http://www.mainichi-msn.co.jp/keizai/seisaku/")
       ("話題" "wadai" "http://www.mainichi-msn.co.jp/keizai/wadai/")
       ("経済観測" "kansoku.wadai"
	"http://www.mainichi-msn.co.jp/keizai/wadai/kansoku/archive/"))
      ("kokusai"
       ("アフリカ・オセアニア" "afro-ocea"
	"http://www.mainichi-msn.co.jp/kokusai/afro-ocea/")
       ("南北アメリカ" "america"
	"http://www.mainichi-msn.co.jp/kokusai/america/")
       ("アジア" "asia" "http://www.mainichi-msn.co.jp/kokusai/asia/")
       ("ヨーロッパ" "europe" "http://www.mainichi-msn.co.jp/kokusai/europe/")
       ("中近東・ロシア" "mideast"
	"http://www.mainichi-msn.co.jp/kokusai/mideast/"))
      ("kurashi"
       ("子育て" "bebe"	"http://www.mainichi-msn.co.jp/kurashi/bebe/")
       ("ファッション" "fashion"
	"http://www.mainichi-msn.co.jp/kurashi/fashion/")
       ("家庭" "katei" "http://www.mainichi-msn.co.jp/kurashi/katei/")
       ("健康" "kenko" "http://www.mainichi-msn.co.jp/kurashi/kenko/")
       ("こころ" "kokoro" "http://www.mainichi-msn.co.jp/kurashi/kokoro/")
       ("食" "shoku" "http://www.mainichi-msn.co.jp/kurashi/shoku/")
       ("趣味" "shumi" "http://www.mainichi-msn.co.jp/kurashi/shumi/")
       ("旅" "travel" "http://www.mainichi-msn.co.jp/kurashi/travel/")
       ("女と男" "women" "http://www.mainichi-msn.co.jp/kurashi/women/"))
      ("science"
       ("環境" "env" "http://www.mainichi-msn.co.jp/science/env/")
       ("科学" "kagaku" "http://www.mainichi-msn.co.jp/science/kagaku/")
       ("医療" "medical" "http://www.mainichi-msn.co.jp/science/medical/")
       ("理系白書" "rikei" "http://www.mainichi-msn.co.jp/science/rikei/"))
      ("seiji"
       ("その他" "feature" "http://www.mainichi-msn.co.jp/seiji/feature/")
       ("行政" "gyousei" "http://www.mainichi-msn.co.jp/seiji/gyousei/")
       ("国会" "kokkai" "http://www.mainichi-msn.co.jp/seiji/kokkai/")
       ("政党" "seitou" "http://www.mainichi-msn.co.jp/seiji/seitou/")
       ("選挙" "senkyo" "http://www.mainichi-msn.co.jp/seiji/senkyo/"))
      ("shakai"
       ("訃報" "fu" "http://www.mainichi-msn.co.jp/shakai/fu/")
       ("学芸" "gakugei" "http://www.mainichi-msn.co.jp/shakai/gakugei/")
       ("人事" "ji" "http://www.mainichi-msn.co.jp/shakai/ji/")
       ("事件" "jiken" "http://www.mainichi-msn.co.jp/shakai/jiken/")
       ("皇室" "koushitsu" "http://www.mainichi-msn.co.jp/shakai/koushitsu/")
       ("天気" "tenki" "http://www.mainichi-msn.co.jp/shakai/tenki/")
       ("話題" "wadai" "http://www.mainichi-msn.co.jp/shakai/wadai/"))
      ("shakai.edu"
       ("本と読書" "book"
	"http://www.mainichi-msn.co.jp/shakai/edu/book/archive/")
       ("'07年センター試験" "center07"
	"http://www.mainichi-msn.co.jp/shakai/edu/jyuken/center07/")
       ("ITのある教室" "class.elearningschool" "\
ttp://www.mainichi-msn.co.jp/shakai/edu/elearningschool/class/archive/")
       ("学校と私" "gakkou"
	"http://www.mainichi-msn.co.jp/shakai/edu/gakkou/archive/")
       ("学力とは何か" "gakuryoku"
	"http://www.mainichi-msn.co.jp/shakai/edu/gakuryoku/archive/")
       ("教育行政" "gyousei"
	"http://www.mainichi-msn.co.jp/shakai/edu/gyousei/archive/")
       ("高校" "high" "http://www.mainichi-msn.co.jp/shakai/edu/high/archive/")
       ("中学校" "junior"
	"http://www.mainichi-msn.co.jp/shakai/edu/junior/archive/")
       ("受験・入試" "jyuken"
	"http://www.mainichi-msn.co.jp/shakai/edu/jyuken/")
       ("毎日小学生新聞" "maishou"
	"http://www.mainichi-msn.co.jp/shakai/edu/maishou/")
       ("ネット社会と子供たち" "morals.net"
	"http://www.mainichi-msn.co.jp/shakai/edu/net/morals/archive/")
       ("教育の森" "mori"
	"http://www.mainichi-msn.co.jp/shakai/edu/mori/archive/")
       ("ITで入試が変わる" "nyushi.elearningschool" "\
ttp://www.mainichi-msn.co.jp/shakai/edu/elearningschool/nyushi/archive/")
       ("情報パケット" "packet"
	"http://www.mainichi-msn.co.jp/shakai/edu/packet/archive/")
       ("小学校" "primary"
	"http://www.mainichi-msn.co.jp/shakai/edu/primary/archive/")
       ("育ち直しの歌" "sodachi"
	"http://www.mainichi-msn.co.jp/shakai/edu/sodachi/")
       ("単位不足問題" "tanni"
	"http://www.mainichi-msn.co.jp/shakai/edu/tanni/archive/")
       ("大学" "university"
	"http://www.mainichi-msn.co.jp/shakai/edu/university/archive/")
       ("話題" "wadai"
	"http://www.mainichi-msn.co.jp/shakai/edu/wadai/archive/"))
      ("shakai.edu.campal"
       ("大楽人" "dairakujin"
	"http://www.mainichi-msn.co.jp/shakai/edu/campal/dairakujin/archive/")
       ("読見しました。" "dokumi"
	"http://www.mainichi-msn.co.jp/shakai/edu/campal/dokumi/archive/")
       ("情報伝言板" "jouho"
	"http://www.mainichi-msn.co.jp/shakai/edu/campal/jouho/archive/")
       ("斬る" "kiru"
	"http://www.mainichi-msn.co.jp/shakai/edu/campal/kiru/archive/")
       ("なにコレ？！" "nanikore"
	"http://www.mainichi-msn.co.jp/shakai/edu/campal/nanikore/archive/")
       ("すた・こら" "sutakora"
	"http://www.mainichi-msn.co.jp/shakai/edu/campal/sutakora/archive/"))
      ("sports"
       ("アマ野球" "ama" "http://www.mainichi-msn.co.jp/sports/ama/")
       ("格闘技" "battle" "http://www.mainichi-msn.co.jp/sports/battle/")
       ("その他" "feature" "http://www.mainichi-msn.co.jp/sports/feature/")
       ("陸上競技" "field" "http://www.mainichi-msn.co.jp/sports/field/")
       ("競馬" "keiba" "http://www.mainichi-msn.co.jp/sports/keiba/")
       ("大リーグ" "major" "http://www.mainichi-msn.co.jp/sports/major/")
       ("プロ野球" "pro" "http://www.mainichi-msn.co.jp/sports/pro/")
       ("サッカー" "soccer" "http://www.mainichi-msn.co.jp/sports/soccer/"))
      ("sports.column"
       ("ベンチ" "benchi"
	"http://www.mainichi-msn.co.jp/sports/benchi/archive/")
       ("遺伝子は飛ぶ" "gene2.keiba"
	"http://www.mainichi-msn.co.jp/sports/keiba/gene2/")
       ("冨重圭以子の納得の一言" "hitokoto"
	"http://www.mainichi-msn.co.jp/sports/hitokoto/archive/")
       ("佐藤典夫の超ウルトラ馬券" "horsenews.keiba"
	"http://www.mainichi-msn.co.jp/sports/keiba/horsenews/")
       ("井崎脩五郎の予想上手の馬券ベタ" "jouzu.keiba"
	"http://www.mainichi-msn.co.jp/sports/keiba/jouzu/")
       ("マリヨンのＮＹ発・球界インサイド" "mariyon.major"
	"http://www.mainichi-msn.co.jp/sports/major/mariyon/archive/")
       ("予想小説" "novel.keiba"
	"http://www.mainichi-msn.co.jp/sports/keiba/novel/archive/")
       ("プレスルーム" "pressroom"
	"http://www.mainichi-msn.co.jp/sports/pressroom/archive/")
       ("松沢一憲のＶデータ" "vdata.keiba"
	"http://www.mainichi-msn.co.jp/sports/keiba/vdata/"))))
  "Alist of parent groups and lists of subgenres and tables for subgroups.
Each table is the same as the `cdr' of the element of
`shimbun-mainichi-group-table'.  If a table is omitted the value of
`shimbun-mainichi-header-regexp-default' is used by default.")

(defvar shimbun-mainichi-server-name "毎日新聞")

(defvar shimbun-mainichi-from-address "nobody@example.com")

(defvar shimbun-mainichi-content-start
  (let ((s0 "[\t\n ]*"))
    (concat "<!--emacs-w3m-shimbun-mainichi-content-start-->\\|</div>" s0
	    "\\(?:<div" s0 "class=\"img_[^\"]+\"" s0 ">" s0 "\\|<p>\\)")))

(defvar shimbun-mainichi-content-end
  (let ((s0 "[\t\n ]*"))
    (concat "\\(?:" s0 "</[^>]+>\\)*" s0
	    "<!--" s0 "||" s0 "/todays_topics" s0 "||-->")))

(defvar shimbun-mainichi-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAABwAAAAcBAMAAACAI8KnAAAABGdBTUEAALGPC/xhBQAAABh
 QTFRFC2zfDGzfEnDgJn3iU5fnj7vvzuH4+vz++nlsOQAAAC90RVh0U29mdHdhcmUAWFYgVmVyc2l
 vbiAzLjEwYStGTG1hc2sgIFJldjogMTIvMjkvOTQbx6p8AAABA0lEQVR4nGWRPW+DMBiED5vuqK0
 60yppVwQ0rFTYZo348poSA3uDzd/vCxNNT14en1/5zgZYEoCF69rk2zhWPR6GADwCl864tsaHFUJ
 dweTJuMcQZZ0kRQxkqnaHik2DbBwdVuPgtPG7WTcuhPdshdM5f7lp4SpyXUPoazu1i6HZpbY6R3a
 ZhAW8ztmZsDxPqf0Cb6zsVzQjJQA/2GNE2OWHbqaQvEggI7wFfOmxk1esLUL2GrJg2yBkrTSDqvB
 eJKmhqtNpttk3sllICskmdbXlGdkPNcd/TIuuvOxcM65IsxvSa2Q79w7V8AfL2u1nY9ZquuiWfK7
 1BSVNQzxF9B+40y/ui1KdNxt0ugAAAAd0SU1FB9QEDQAjJMA7GTQAAAAASUVORK5CYII=")))

(defvar shimbun-mainichi-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-mainichi))
  (mapcar 'car shimbun-mainichi-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-mainichi))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-mainichi-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-mainichi))
  (let* ((group (shimbun-current-group-internal shimbun))
	 (index (nth 2 (assoc group shimbun-mainichi-group-table))))
    (shimbun-expand-url
     (or index
	 (concat (shimbun-subst-char-in-string
		  ?. ?/ (shimbun-current-group-internal shimbun))
		 "/"))
     (shimbun-url-internal shimbun))))

(defun shimbun-mainichi-make-date-string (&rest args)
  "Run `shimbun-make-date-string' with ARGS and fix a day if needed.

\(shimbun-mainichi-make-date-string YEAR MONTH DAY &optional TIME TIMEZONE)"
  (if (equal (nth 3 args) "23:59:59")
      (apply 'shimbun-make-date-string args)
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
	  date)))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-mainichi)
					 &optional range)
  (shimbun-mainichi-get-headers shimbun))

(defun shimbun-mainichi-get-headers (shimbun)
  (let* ((case-fold-search t)
	 (group (shimbun-current-group-internal shimbun))
	 (regexp (or (nthcdr 3 (assoc group shimbun-mainichi-group-table))
		     shimbun-mainichi-header-regexp-default))
	 (from (concat (shimbun-server-name shimbun)
		       " (" (shimbun-current-group-name shimbun) ")"))
	 (editorial (when (string-equal group "eye.shasetsu")
		      (list nil)))
	 (subgroups (cdr (assoc group shimbun-mainichi-subgroups-alist)))
	 numbers start serial snum id subgenre url month day subject urls
	 subjects headers header date subgrp)
    (if (eq (car regexp) 'none)
	(setq regexp nil)
      (setq numbers (cdr regexp)
	    regexp (car regexp)))
    (catch 'stop
      ;; The loop for fetching all the articles in the subgroups.
      (while t
	(when regexp
	  (shimbun-strip-cr)
	  ;; Ignore unwanted links.
	  (cond
	   ((and (string-equal group "eye")
		 (progn
		   (goto-char (point-min))
		   (re-search-forward "<td[\t\n ]+class=\"date_md\">" nil t))))
	   (t
	    (goto-char (point-max))
	    (when (re-search-backward "<!--[\t\n ]*||[\t\n ]*\
\\(?:todays_topics\\|/movie_news\\|/photo_news\\|/top_navi\\)[\t\n ]*||-->"
				      nil 'move))))
	  (delete-region (point-min) (point))

	  ;; Remove special sections.
	  (while (and (re-search-forward "\
\[\t\n ]*\\(<!--[\t\n ]*|[\t\n ]*\\)\\(special[\t\n ]*|-->\\)"
					 nil t)
		      (progn
			(setq start (match-beginning 0))
			(re-search-forward
			 (concat (regexp-quote (match-string 1))
				 "/"
				 (regexp-quote (match-string 2))
				 "[\t\n ]*")
			 nil t)))
	    (delete-region start (match-end 0)))
	  (when (and (string-match "理系白書" from)
		     (progn
		       (goto-char (point-min))
		       (re-search-forward "\
\[\t\n ]*<[^>]+>[\t\n ]*理系白書\\(?:の特集\\|シンポ\\)[\t\n ]*<"
					  nil t)))
	    (delete-region (match-beginning 0) (point-max)))

	  ;; Remove ranking sections.
	  (goto-char (point-min))
	  (while (and (re-search-forward "[\t ]*<div[\t\n ]+class=\"ranking\">"
					 nil t)
		      (progn
			(setq start (match-beginning 0))
			(re-search-forward "</div>[\t\n ]*" nil t)))
	    (delete-region start (match-end 0)))

	  ;; Remove commented sections.
	  (goto-char (point-min))
	  (while (re-search-forward "\
\[\t\n ]*<[\t\n ]*!\\(?:[^<>]*<[^<>]+>\\)+[^<>]+>[\t\n ]*"
				    nil t)
	    (delete-region (match-beginning 0) (match-end 0)))

	  (goto-char (point-min))
	  (while (re-search-forward regexp nil t)
	    (setq serial (match-string (nth 1 numbers)))
	    (when editorial
	      (setq serial
		    (concat serial "."
			    (if (setq snum (assoc serial editorial))
				(number-to-string
				 (setcdr snum (1+ (cdr snum))))
			      (push (setq snum (cons serial 1)) editorial)
			      "1"))))
	    (setq id (concat "<" serial "%"
			     (when subgenre (concat subgenre "."))
			     (save-match-data
			       (if (string-match "\\." group)
				   (mapconcat
				    'identity
				    (nreverse (split-string group "\\."))
				    ".")
				 group))
			     "." shimbun-mainichi-top-level-domain ">")
		  url (match-string (nth 0 numbers))
		  month (string-to-number (match-string (nth 3 numbers)))
		  day (string-to-number (match-string (nth 4 numbers)))
		  subject (match-string (nth 5 numbers)))
	    (when editorial
	      (setq subject (format "%02d/%02d %s" month day subject)))
	    ;; Exclude duplications.
	    (unless (or (if editorial
			    (and (or (or (member subject subjects)
					 (progn
					   (push subject subjects)
					   nil))
				     (member url urls))
				 (progn
				   (setcdr snum (1- (cdr snum)))
				   t))
			  (member url urls))
			(prog1
			    (shimbun-search-id shimbun id)
			  (push url urls)))
	      (push
	       (shimbun-create-header
		0 subject from
		(shimbun-mainichi-make-date-string
		 (string-to-number (match-string (nth 2 numbers)))
		 month day
		 (when (nth 7 numbers)
		   (if (match-beginning (nth 7 numbers))
		       (format
			"%02d:%02d"
			(string-to-number (match-string (nth 6 numbers)))
			(string-to-number (match-string (nth 7 numbers))))
		     "23:59:59")))
		id "" 0 0
		(shimbun-expand-url url shimbun-mainichi-url))
	       headers))))
	(if subgroups
	    (progn
	      (erase-buffer)
	      (setq subgrp (pop subgroups)
		    from (concat (shimbun-server-name shimbun)
				 " (" (car subgrp) ")")
		    subgenre (cadr subgrp))
	      (shimbun-retrieve-url (caddr subgrp))
	      (setq regexp (or (cadddr subgrp)
			       (car shimbun-mainichi-header-regexp-default))
		    numbers (or (cddddr subgrp)
				(cdr shimbun-mainichi-header-regexp-default))))
	  (throw 'stop nil))))
    (prog1
	(setq headers (shimbun-sort-headers headers))
      (while headers
	(setq header (pop headers)
	      date (shimbun-header-date header))
	(when (string-match "23:59:59" date)
	  (shimbun-header-set-date header
				   (replace-match "00:00" nil nil date)))))))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-mainichi)
						   header)
  (shimbun-mainichi-prepare-article shimbun header))

(defun shimbun-mainichi-prepare-article (shimbun header)
  (shimbun-with-narrowed-article
   shimbun
   ;; Fix the Date header.
   (when (re-search-forward "<p>毎日新聞　\
\\(20[0-9][0-9]\\)年\\([01]?[0-9]\\)月\\([0-3]?[0-9]\\)日　\
\\([012]?[0-9]\\)時\\([0-5]?[0-9]\\)分\\(?:　（最終更新時間　\
\\([01]?[0-9]\\)月\\([0-3]?[0-9]\\)日　\
\\([012]?[0-9]\\)時\\([0-5]?[0-9]\\)分）\\)?\\'"
			    nil t)
     (shimbun-header-set-date
      header
      (shimbun-make-date-string
       (string-to-number (match-string 1))
       (string-to-number (or (match-string 6) (match-string 2)))
       (string-to-number (or (match-string 7) (match-string 3)))
       (format "%02d:%02d"
	       (string-to-number (or (match-string 8) (match-string 4)))
	       (string-to-number (or (match-string 9) (match-string 5)))))))
   (goto-char (point-min))
   (let ((from (shimbun-header-from-internal header))
	 (group (shimbun-current-group-internal shimbun))
	 (subject (shimbun-header-subject header 'no-encode)))
     (cond ((or (string-match "近事片々" from)
		(string-match "\\`近事片々：" subject))
	    ;; Shorten paragraph separators.
	    (while (search-forward "</p><p>　　　◇</p><p>" nil t)
	      (replace-match "<br>　　　◇<br>")))
	   ((or (string-match "余録" from)
		(string-match "\\`余録：" subject))
	    ;; Break continuous lines.
	    (while (search-forward "▲" nil t)
	      (replace-match "。<br><br>　")))
	   ((string-equal group "kansai")
	    ;; Include images.
	    (let ((start (point-min))
		  (end (point-max)))
	      (widen)
	      (forward-line -3)
	      (when (prog1
			(if (re-search-forward "<img[\t\n ]+src=" start t)
			    (goto-char (match-beginning 0))
			  (goto-char start)
			  nil)
		      (narrow-to-region (point) end))
		(insert "<!--emacs-w3m-shimbun-mainichi-content-start-->"))))))
   (when (shimbun-prefer-text-plain-internal shimbun)
     ;; Replace images with text.
     (goto-char (point-min))
     (while (re-search-forward "[\t\n ]*<img[\t\n ]+[^>]+>[\t\n ]*" nil t)
       (replace-match "(写真)")))))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-mainichi)
						    header)
  (when (luna-call-next-method)
    ;; Break long lines.
    (unless (shimbun-prefer-text-plain-internal shimbun)
      (shimbun-break-long-japanese-lines))
    t))

(provide 'sb-mainichi)

;;; sb-mainichi.el ends here

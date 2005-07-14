;;; sb-nikkei.el --- shimbun backend for nikkei.co.jp -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005
;; Kazuyoshi KOREEDA <Koreeda.Kazuyoshi@jp.panasonic.com>

;; Author: Kazuyoshi KOREEDA <Koreeda.Kazuyoshi@jp.panasonic.com>,
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
;; Inc.; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Original code was sb-asahi.el which is written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org> and
;; Yuuichi Teranishi <teranisi@gohome.org>.

;;; Code:

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
    ("it.business" "ITビジネス"
     "http://it.nikkei.co.jp/business/news/index.aspx"
     shimbun-nikkei-get-headers-it-business
     shimbun-nikkei-prepare-article-default2)
    ("it.internet" "インターネット"
     "http://it.nikkei.co.jp/internet/news/index.aspx"
     shimbun-nikkei-get-headers-it-internet
     shimbun-nikkei-prepare-article-default2)
    ("it.mobile" "モバイル"
     "http://it.nikkei.co.jp/mobile/news/index.aspx"
     shimbun-nikkei-get-headers-it-mobile
     shimbun-nikkei-prepare-article-default2)
    ("it.security" "セキュリティ"
     "http://it.nikkei.co.jp/security/news/index.aspx"
     shimbun-nikkei-get-headers-it-security
     shimbun-nikkei-prepare-article-default2)
    ("it.digital" "デジタル家電＆エンタメ"
     "http://it.nikkei.co.jp/digital/news/index.aspx"
     shimbun-nikkei-get-headers-it-digital
     shimbun-nikkei-prepare-article-default2)
    ("it.pc" "PC＆デジタルカメラ"
     "http://it.nikkei.co.jp/pc/news/index.aspx"
     shimbun-nikkei-get-headers-it-pc
     shimbun-nikkei-prepare-article-default2)
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
    ("china" "中国ビジネス事情" ,(concat shimbun-nikkei-url "china/news/")
     shimbun-nikkei-get-headers-china
     shimbun-nikkei-prepare-article-okuyami)
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
     shimbun-nikkei-prepare-article-okuyami)
    ("sports" "スポーツ" "http://sports.nikkei.co.jp/"
     shimbun-nikkei-get-headers-sports
     shimbun-nikkei-prepare-article-sports)
    ("newpro" "新製品" ,(concat shimbun-nikkei-url "newpro/news/")
     shimbun-nikkei-get-headers-newpro
     shimbun-nikkei-prepare-article-newpro)
    ("release" "プレスリリース" "http://release.nikkei.co.jp/"
     shimbun-nikkei-get-headers-release
     shimbun-nikkei-prepare-article-release)
    ("release.it.comp" "プレスリリース(ＩＴ；コンピューター)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=1"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.peri" "プレスリリース(ＩＴ；周辺機器)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=2"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.sys" "プレスリリース(ＩＴ；システム・ソフト開発)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=3"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.cont" "プレスリリース(ＩＴ；情報・コンテンツ)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=4"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.net" "プレスリリース(ＩＴ；通信・インターネット)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=5"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.lsi" "プレスリリース(ＩＴ；半導体)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=6"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.game" "プレスリリース(ＩＴ；ゲーム・娯楽)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=7"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.it.etc" "プレスリリース(ＩＴ；その他ＩＴ関連)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=1&sindID=8"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.depart" "プレスリリース(流通；百貨店・スーパー)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=9"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.ryohan" "プレスリリース(流通；量販店)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=10"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.zakka" "プレスリリース(流通；生活雑貨)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=11"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.cosme" "プレスリリース(流通；医薬品・化粧品)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=12"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.car" "プレスリリース(流通；自動車)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=13"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.book" "プレスリリース(流通；書籍)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=14"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.record" "プレスリリース(流通；レコード・ゲーム)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=15"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.food" "プレスリリース(流通；食品・飲料)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=16"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.mercha" "プレスリリース(流通；商社・卸売業)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=17"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.mail" "プレスリリース(流通；通信・訪問販売)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=18"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.netshop" "プレスリリース(流通；ネットショッピング)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=19"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.dist.etc" "プレスリリース(流通；その他商業)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=2&sindID=20"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.money.bank" "プレスリリース(金融；銀行・信金)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=3&sindID=57"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.money.sec" "プレスリリース(金融；証券会社)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=3&sindID=58"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.money.am" "プレスリリース(金融；投資信託運用会社)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=3&sindID=59"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.money.insu" "プレスリリース(金融；保険会社)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=3&sindID=60"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.money.etc" "プレスリリース(金融；その他金融)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=3&sindID=61"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.chemi" "プレスリリース(メーカー；化学・医薬品)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=31"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.mecha" "プレスリリース(メーカー；機械・金属)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=32"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.car" "プレスリリース(メーカー；自動車・自動車部品)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=33"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.elec" "プレスリリース(メーカー；家電・電機)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=34"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.food" "プレスリリース(メーカー；食品・飲料)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=35"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.sports" "プレスリリース(メーカー；スポーツ・娯楽用品)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=36"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.apparel" "プレスリリース(メーカー；アパレル・生活用品)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=37"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.commu" "プレスリリース(メーカー；通信機器・精密機械)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=38"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.maker.etc" "プレスリリース(メーカー；その他メーカー)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=4&sindID=39"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.medic" "プレスリリース(サービス；医療・福祉)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=40"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.rest" "プレスリリース(サービス；飲食)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=41"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.trans" "プレスリリース(サービス；運輸・運送)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=42"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.energy" "プレスリリース(サービス；エネルギー)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=43"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.enter" "プレスリリース(サービス；エンターテインメント)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=44"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.env" "プレスリリース(サービス；環境)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=45"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.consul" "プレスリリース(サービス；コンサルティング)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=46"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.edu" "プレスリリース(サービス；教育・研修)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=47"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.haken" "プレスリリース(サービス；人材派遣)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=48"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.life" "プレスリリース(サービス；生活関連)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=49"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.media" "プレスリリース(サービス；メディア)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=50"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.lease" "プレスリリース(サービス；リース)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=51"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.travel" "プレスリリース(サービス；旅行・ホテル)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=52"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.service.etc" "プレスリリース(サービス；その他サービス業)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=53"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.const.const" "プレスリリース(サービス；建設・土木)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=6&sindID=54"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.const.house" "プレスリリース(サービス；住宅)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=6&sindID=56"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
    ("release.const.etc" "プレスリリース(サービス；その他建設関連)"
     "http://release.nikkei.co.jp/isclassList.cfm?lindID=5&sindID=53"
     shimbun-nikkei-get-headers-release2
     shimbun-nikkei-prepare-article-release2)
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
(defvar shimbun-nikkei-from-address "nobody@example.com")
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
    ("\\`release" . "\
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
	 (shimbun-nikkei-from-address
	  (concat (shimbun-server-name shimbun)
		  " (" (shimbun-current-group-name shimbun) ")"))
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
		      "\\(\\(?:[^\"<>]+/\\)?"
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
		      "\"" s0 ">\\(?:" s0 "<[^<>]+>" s0 "\\)*" s0
		      ;; 6. subject
		      "\\([^<>]+\\)"
		      "\\(?:" s0 "<[^<>]+>" s0 "\\)*"
		      "("
		      ;; 7. hour:minute
		      "\\([012][0-9]:[0-5][0-9]\\)"
		      ")")))
	  nil t)
    (push (shimbun-create-header
	   0
	   (match-string 6)
	   shimbun-nikkei-from-address
	   (shimbun-nikkei-make-date-string
	    (string-to-number (match-string 3))
	    (string-to-number (match-string 4))
	    (string-to-number (match-string 5))
	    (match-string 7))
	   (concat "<" (match-string 2) "%" group "."
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
		       "\\([^<]+[^\t\n ]\\)"
		       s0
		       "<!--" s0 "FJZONE" s1 "END" s1 "NAME=\"MIDASHI\""
		       s0 "-->\\(?:" s0 "<[^!]+>\\)*" s0
		       "<!--" s0 "FJZONE" s1 "START" s1 "NAME=\"HONBUN\""
		       s0 "-->[^<]+("
		       ;; 7. hour:minute
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
	      (match-string 7))
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
			"\\(\\(?:[^\"<>]+/\\)?"
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
			"\\([^<>]+\\)")))
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
			"\">" s0
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
			       "日" s0 "([^<]+)"
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
		(concat "<a" s1 "href=\"\\./"
			;; 1. url
			"\\([^\"<>]+/"
			;; 2. serial number
			"\\([^\t\n ]+\\)"
			"\\)"
			s0 "-frame" s0 "\\.html"
			"\">" s0
			;; 3. subject
			"\\([^<]+\\)"
			s0 "<")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 3)
	     shimbun-nikkei-from-address
	     date
	     (concat "<" (match-string 1) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (concat (match-string 1) ".html")
					folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-it-business (group folder)
  "Function used to fetch headers for the it.business group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"/"
			;; 1. url
			"\\(business/news/index\\.aspx\\?i="
			;; 2. serial number
			"\\("
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			;; 4. month
			"\\([01][0-9]\\)"
			;; 5. day
			"\\([0-3][0-9]\\)"
			"[0-9a-z]+"
			"\\)"
			"\\)"
			"\">" s0
			;; 6. subject
			"\\([^<]+\\)"
			"</a>")))
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
	     (shimbun-nikkei-expand-url
	      (concat "http://it.nikkei.co.jp/" (match-string 1)) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-it-internet (group folder)
  "Function used to fetch headers for the it.internet group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"/"
			;; 1. url
			"\\(internet/news/index\\.aspx\\?i="
			;; 2. serial number
			"\\("
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			;; 4. month
			"\\([01][0-9]\\)"
			;; 5. day
			"\\([0-3][0-9]\\)"
			"[0-9a-z]+"
			"\\)"
			"\\)"
			"\">" s0
			;; 6. subject
			"\\([^<]+\\)"
			"</a>")))
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
	     (shimbun-nikkei-expand-url
	      (concat "http://it.nikkei.co.jp/" (match-string 1)) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-it-security (group folder)
  "Function used to fetch headers for the it.security group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"/"
			;; 1. url
			"\\(security/news/index\\.aspx\\?i="
			;; 2. serial number
			"\\("
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			;; 4. month
			"\\([01][0-9]\\)"
			;; 5. day
			"\\([0-3][0-9]\\)"
			"[0-9a-z]+"
			"\\)"
			"\\)"
			"\">" s0
			;; 6. subject
			"\\([^<]+\\)"
			"</a>")))
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
	     (shimbun-nikkei-expand-url
	      (concat "http://it.nikkei.co.jp/" (match-string 1)) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-it-mobile (group folder)
  "Function used to fetch headers for the it.mobile group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"/"
			;; 1. url
			"\\(mobile/news/index\\.aspx\\?i="
			;; 2. serial number
			"\\("
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			;; 4. month
			"\\([01][0-9]\\)"
			;; 5. day
			"\\([0-3][0-9]\\)"
			"[0-9a-z]+"
			"\\)"
			"\\)"
			"\">" s0
			;; 6. subject
			"\\([^<]+\\)"
			"</a>")))
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
	     (shimbun-nikkei-expand-url
	      (concat "http://it.nikkei.co.jp/" (match-string 1)) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-it-digital (group folder)
  "Function used to fetch headers for the it.digital group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"/"
			;; 1. url
			"\\(digital/news/index\\.aspx\\?i="
			;; 2. serial number
			"\\("
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			;; 4. month
			"\\([01][0-9]\\)"
			;; 5. day
			"\\([0-3][0-9]\\)"
			"[0-9a-z]+"
			"\\)"
			"\\)"
			"\">" s0
			;; 6. subject
			"\\([^<]+\\)"
			"</a>")))
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
	     (shimbun-nikkei-expand-url
	      (concat "http://it.nikkei.co.jp/" (match-string 1)) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-it-pc (group folder)
  "Function used to fetch headers for the it.pc group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"/"
			;; 1. url
			"\\(pc/news/index\\.aspx\\?i="
			;; 2. serial number
			"\\("
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			;; 4. month
			"\\([01][0-9]\\)"
			;; 5. day
			"\\([0-3][0-9]\\)"
			"[0-9a-z]+"
			"\\)"
			"\\)"
			"\">" s0
			;; 6. subject
			"\\([^<]+\\)"
			"</a>")))
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
	     (shimbun-nikkei-expand-url
	      (concat "http://it.nikkei.co.jp/" (match-string 1)) folder))
	    headers))
    headers))

(defun shimbun-nikkei-get-headers-markets (group folder)
  "Function used to fetch headers for the markets group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"summary\\.cfm"
			;; 1. url
			"\\(\\?genre="
			;; 2. serial number
			"\\([^\"]+date="
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"[01][0-9][0-3][0-9]"
			"\\)"
			"\\)"
			"\"" s0 ">" s0
			;; 4. subject
			"\\([^<]+\\)"
			s0 "（"
			;; 5. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 7. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 8. minute
			"\\([0-5]?[0-9]\\)"
			"）" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 4)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6))
	      (format "%02d:%02d"
		      (string-to-number (match-string 7))
		      (string-to-number (match-string 8))))
	     (concat "<" (match-string 2) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    (shimbun-sort-headers headers)))

(defun shimbun-nikkei-get-headers-kawase (group folder)
  "Function used to fetch headers for the kawase group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"summary\\.cfm"
			;; 1. url
			"\\(\\?id="
			;; 2. serial number
			"\\([^\"]+date="
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"[01][0-9][0-3][0-9]"
			"\\)"
			"\\)"
			"\"" s0 ">" s0
			;; 4. subject
			"\\([^<]+\\)"
			s0 "（"
			;; 5. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 7. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 8. minute
			"\\([0-5]?[0-9]\\)"
			"）" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 4)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6))
	      (format "%02d:%02d"
		      (string-to-number (match-string 7))
		      (string-to-number (match-string 8))))
	     (concat "<" (match-string 2) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    (shimbun-sort-headers headers)))

(defun shimbun-nikkei-get-headers-bunkatsu2 (group folder)
  "Function used to fetch headers for the gyosuuchi group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"bunkatsu3\\.cfm\\?genre=m4"
			;; 1. url
			"\\(&id="
			;; 2. serial number
			"\\([^\"]+date="
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"[01][0-9][0-3][0-9][^\"]+"
			"\\)"
			"\\)"
			"\"" s0 ">" s0
			;; 4. subject
			"\\([^<]+\\)"
			s0 "（"
			;; 5. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 7. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 8. minute
			"\\([0-5]?[0-9]\\)"
			"）" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 4)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6))
	      (format "%02d:%02d"
		      (string-to-number (match-string 7))
		      (string-to-number (match-string 8))))
	     (concat "<" (match-string 2) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    (shimbun-sort-headers headers)))

(defun shimbun-nikkei-get-headers-kinri (group folder)
  "Function used to fetch headers for the kinri group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"kinri\\.cfm"
			;; 1. url
			"\\(\\?id="
			;; 2. serial number
			"\\([^\"]+date="
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"[01][0-9][0-3][0-9]"
			"\\)"
			"\\)"
			"\"" s0 ">" s0
			;; 4. subject
			"\\([^<]+\\)"
			s0 "（"
			;; 5. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 7. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 8. minute
			"\\([0-5]?[0-9]\\)"
			"）" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 4)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6))
	      (format "%02d:%02d"
		      (string-to-number (match-string 7))
		      (string-to-number (match-string 8))))
	     (concat "<" (match-string 2) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    (shimbun-sort-headers headers)))

(defun shimbun-nikkei-get-headers-ft (group folder)
  "Function used to fetch headers for the ft group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"ft\\.cfm"
			;; 1. url
			"\\(\\?id="
			;; 2. serial number
			"\\([^\"]+date="
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"[01][0-9][0-3][0-9]"
			"\\)"
			"\\)"
			"\"" s0 ">" s0
			;; 4. subject
			"\\([^<]+\\)"
			s0 "[(|（]"
			;; 5. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			"[）|)]" s0 "※" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 4)
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

(defun shimbun-nikkei-get-headers-dj (group folder)
  "Function used to fetch headers for the dj group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"dj\\.cfm"
			;; 1. url
			"\\(\\?id="
			;; 2. serial number
			"\\([^\"]+date="
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"[01][0-9][0-3][0-9]"
			"\\)"
			"\\)"
			"\"" s0 ">" s0
			;; 4. subject
			"\\([^<]+\\)"
			s0 "（"
			;; 5. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 7. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 8. minute
			"\\([0-5]?[0-9]\\)"
			"）" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 4)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6))
	      (format "%02d:%02d"
		      (string-to-number (match-string 7))
		      (string-to-number (match-string 8))))
	     (concat "<" (match-string 2) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
	    headers))
    (shimbun-sort-headers headers)))

(defun shimbun-nikkei-get-headers-gyoseki (group folder)
  "Function used to fetch headers for the gyoseki group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\"gyoseki\\.cfm"
			;; 1. url
			"\\(\\?id="
			;; 2. serial number
			"\\([^\"]+date="
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"[01][0-9][0-3][0-9]"
			"\\)"
			"\\)"
			"\"" s0 ">" s0
			;; 4. subject
			"\\([^<]+\\)"
			s0 "（"
			;; 5. month
			"\\([01]?[0-9]\\)"
			"/"
			;; 6. day
			"\\([0-3]?[0-9]\\)"
			s1
			;; 7. hour
			"\\([0-2]?[0-9]\\)"
			":"
			;; 8. minute
			"\\([0-5]?[0-9]\\)"
			"）" s0 "</a>")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 4)
	     shimbun-nikkei-from-address
	     (shimbun-nikkei-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 5))
	      (string-to-number (match-string 6))
	      (format "%02d:%02d"
		      (string-to-number (match-string 7))
		      (string-to-number (match-string 8))))
	     (concat "<" (match-string 2) "%" group "."
		     shimbun-nikkei-top-level-domain ">")
	     "" 0 0
	     (shimbun-nikkei-expand-url (match-string 1) folder))
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
		     "日\\(?:" s1
		     ;; 4. hour:minute
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
	      time (match-string 4))
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
			    "[^.]+"
			    "\\)"
			    "\\.html\\)"
			    "\"" s0 ">\\(?:" s0 "<[^>]+>\\)*" s0
			    ;; 4. subject
			    "\\([^<]+\\)"
			    s0)))
		end t)
	  (setq year (string-to-number (match-string 3)))
	  (push (shimbun-create-header
		 0
		 (match-string 4)
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

(defun shimbun-nikkei-get-headers-china (group folder)
  "Function used to fetch headers for the china group."
  (let (headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*")
		    (s1 "[\t\n ]+"))
		(concat "<a" s1 "href=\""
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
			"[0-9_a-z]+"
			"\\)"
			"\\.html\\)"
			"\"" s0 ">" s0 "\\(?:([01]?[0-9]/[0-3]?[0-9])\\)?" s0
			;; 7. subject
			"\\([^<]+\\)"
			"</a>")))
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
    headers))

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
			"\\(20[0-9][0-9]\\)"
			"/"
			;; 6. month
			"\\([01][0-9]\\)"
			"/"
			;; 7. day
			"\\([0-3][0-9]\\)"
			s1
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
		(concat "<a" s1 "href=\"\\./"
			;; 1. url
			"\\(news\\.cfm\\?i="
			;; 2. serial number
			"\\("
			;; 3. year
			"\\(20[0-9][0-9]\\)"
			"[01][0-9][0-3][0-9][^\"]+\\)"
			"\\)"
			"\"[^>]+>" "\\(?:" s0 "<[^>]+>\\)*" s0 "(" s0
			;; 4. month
			"\\([0-9]+\\)"
			s0 "/" s0
			;; 5. day
			"\\([0-9]+\\)"
			s0 ")" s0
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
			  "\\(\\(?:[^\"]+/\\)?"
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
			"\\)"
			"\"" s0 ">" s0
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

(defun shimbun-nikkei-get-headers-release2 (group folder)
  "Function used to fetch headers for the release-in-detail groups."
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
			"\\)"
			"\"" s0 ">" s0
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
	       (shimbun-nikkei-expand-url
		(concat "http://release.nikkei.co.jp/" url) folder))
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
			"\\(\\(?:社説\\|春秋\\)[^<]+\\)")))
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
  ;; Remove unwanted images.
  (let (start end)
    (while (re-search-forward "[\t\n ]*<div[\t\n ]+[^>]+>[\t\n ]*<img[\t\n ]+\
\[^>]+>[\t\n ]*</div>[\t\n ]*"
			      nil t)
      (setq start (match-beginning 0)
	    end (match-end 0))
      (goto-char start)
      (when (re-search-forward
	     "src=\"http://parts\\.nikkei\\.co\\.jp/parts/s\\.gif\""
	     end t)
	(delete-region start end))))
  (goto-char (point-min))
  (when (re-search-forward "\
<!--[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]+-->"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "\
<a[\t\n ]+name=\"newslist\"></a>\n"
			     nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end))))

(defun shimbun-nikkei-prepare-article-kansai (&rest args)
  "Function used to prepare contents of an article for the kansai group."
  (when (re-search-forward "\
<td[\t\n ]+colspan=\"2\"[\t\n ]+class=\"textm\">"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "\
<table[\t\n ]+border=\"0\"[\t\n ]+cellspacing=\
\"0\"[\t\n ]+cellpadding=\"0\"[\t\n ]+width=\"720\">"
			     nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end))))

(defun shimbun-nikkei-prepare-article-default3 (&rest args)
  "Function used to prepare contents of an article for some groups."
  (when (re-search-forward "</h1>\n"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "[\t\n ]<ul[\t\n ]+id=\"tool\">" nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end))))

(defun shimbun-nikkei-prepare-article-bunkatsu2 (&rest args)
  "Function used to prepare contents of an article for the gyosuuchi group."
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

(defun shimbun-nikkei-prepare-article-release2 (&rest args)
  "Function used to prepare contents of an article for the release groups."
  (when (re-search-forward "</h1>\n"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "[\t\n ]*</a></li>\n" nil t)
      (goto-char (match-beginning 0))
      (insert shimbun-nikkei-content-end))))

(defun shimbun-nikkei-prepare-article-market (header)
  "Function used to prepare contents of an article for the market group."
  (when (re-search-forward "\
<!--[\t\n ]*FJZONE[\t\n ]+START[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-->"
			   nil t)
    (insert shimbun-nikkei-content-start)
    (when (re-search-forward "\\(?:(\\([012]?[0-9]:[0-5]?[0-9]\\))[\t\n ]*\\)?\
<!--[\t\n ]*FJZONE[\t\n ]+END[\t\n ]+NAME=\"HONBUN\"[\t\n ]*-->"
			     nil t)
      (if (match-beginning 1)
	  (progn
	    (goto-char (1+ (match-end 1)))
	    (let ((new (match-string 1))
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

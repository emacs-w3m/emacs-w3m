;;; sb-yahoo.el --- shimbun backend for news.yahoo.co.jp -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2001, 2002, 2003, 2005
;; Kazuyoshi KOREEDA <Kazuyoshi.Koreeda@rdmg.mgcs.mei.co.jp>

;; Author: Kazuyoshi KOREEDA <Kazuyoshi.Koreeda@rdmg.mgcs.mei.co.jp>
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

;; Original code was sb-asahi.el which is written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org> and
;; Yuuichi Teranishi <teranisi@gohome.org>

;;; Code:

(require 'shimbun)
(require 'sb-text)

(luna-define-class shimbun-yahoo (shimbun shimbun-text) ())

(defvar shimbun-yahoo-url "http://headlines.yahoo.co.jp/")

(defvar shimbun-yahoo-groups-alist
  '(("topnews" . "topnews")
    ("politics" . "pol")
    ("society" . "soci")
    ("people" . "peo")
    ("business-all" . "bus_all")
    ("market" . "brf")
    ("stock" . "biz")
    ("industry" . "ind")
    ("international" . "int")
    ("entertainment" . "ent")
    ("sports" . "spo")
    ("computer" . "sci")
    ("hokkaido" . "hok")
    ("tohoku" . "toh")
    ("kanto" . "kan")
    ("sinetsu" . "sin")
    ("hokuriku" . "hor")
    ("tokai" . "tok")
    ("kinki" . "kin")
    ("chugoku" . "chu")
    ("sikoku" . "sik")
    ("kyushu" . "kyu")
    ("okinawa" . "oki")))
(defvar shimbun-yahoo-groups
  (mapcar 'car shimbun-yahoo-groups-alist))

(defvar shimbun-yahoo-from-address "news-admin@mail.yahoo.co.jp")
(defvar shimbun-yahoo-content-start "</font><br><br>\n")
(defvar shimbun-yahoo-content-end   "\n<center>\n")

(defvar shimbun-yahoo-x-face-alist
  '(("default" . "X-Face: \"Qj}=TahP*`:b#4o_o63:I=\"~wbql=kpF1a>Sp62\
fpAsVY`saZV[b*GqI!u|i|xKPjNh&P=\n R?n}rh38mkp_:')h=Bh:Rk>0pYF\\I?f\\\
PvPs3>/KG:03n47U?FC[?DNAR4QAQxE3L;m!L10OM$-]kF\n YD\\]-^qzd#'{(o2cu,\
(}CMi|3b9JDQ(^D\\:@DE}d2+0S2G{VS@E*1Og7Vj#35[77\"z9XBq9$1uF$+W\n u")))
(defvar shimbun-yahoo-expiration-days 7)

(luna-define-method shimbun-index-url ((shimbun shimbun-yahoo))
  (format "%shl?c=%s&t=l"
	  (shimbun-url-internal shimbun)
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-yahoo-groups-alist))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-yahoo)
					 &optional range)
  (let ((case-fold-search t)
	headers)
    (catch 'stop
      (while t
	(while (re-search-forward "<a href=\"\\(http://headlines.yahoo.co.jp/hl\\?a=\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)-\\([0-9]+-[^\"]+\\)\\)\">\\([^<]+\\)</a>\\([^0-9]\\|[\n\r]\\)*\\([0-9]+日[^0-9]*\\)?\\([0-9]+\\)時\\([0-9]+\\)分" nil t)
	  (let ((url (match-string 1))
		(year (match-string 2))
		(month (match-string 3))
		(day (match-string 4))
		(no (match-string 5))
		(subject (match-string 6))
		(hour (string-to-number (match-string 9)))
		(min (string-to-number (match-string 10)))
		id time)
	    (setq id (format "<%s%s%s%s.%s@headlines.yahoo.co.jp>"
			     year month day no
			     (shimbun-current-group-internal shimbun)))
	    (if (shimbun-search-id shimbun id)
		(throw 'stop nil))
	    (setq time (format "%02d:%02d" hour min))
	    (push (shimbun-make-header
		   0
		   (shimbun-mime-encode-string subject)
		   (shimbun-from-address shimbun)
		   (shimbun-make-date-string (string-to-number year)
					     (string-to-number month)
					     (string-to-number day) time)
		   id "" 0 0 url)
		  headers)))
	(if (re-search-forward "<a href=\"\\([^\"]+\\)\">次のページ</a>" nil t)
	    (progn
	      (shimbun-retrieve-url (prog1
					(match-string 1)
				      (erase-buffer))
				    t)
	      (goto-char (point-min)))
	  (throw 'stop nil))))
    headers))

(provide 'sb-yahoo)

;;; sb-yahoo.el ends here

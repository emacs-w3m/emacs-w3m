;;; sb-ohmynews.el --- shimbun backend for OhmyNews Japan -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2006 Katsumi Yamaoka

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

(require 'shimbun)

(luna-define-class shimbun-ohmynews-jp (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-ohmynews-jp-top-level-domain "ohmynews.co.jp"
  "Name of the top level domain for Ohmynews Japan.")

(defvar shimbun-ohmynews-jp-url
  (concat "http://www." shimbun-ohmynews-jp-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-ohmynews-jp-server-name "オーマイニュース")

(defvar shimbun-ohmynews-jp-from-address "nobody@example.com")

(defvar shimbun-ohmynews-jp-content-start
  "\\(?:<div[\t\n\r ]+class=\"news-sub[bt]itle\">[\t\n\r ]*\
\\|<!-+[\t\n\r ]*\\(?:編集者注\\|記事本文\\)[\t\n\r ]*-+>\
\[\t\n\r ]*\\(?:<[^>]+>[\t\n\r ]*\\)*\\)")

(defvar shimbun-ohmynews-jp-content-end
  "\\(?:[\t\n\r ]*<[^>]+>\\)*[\t\n\r ]*<!-+[\t\n\r ]*記事フッタ[\t\n\r ]*-+>")

(defvar shimbun-ohmynews-jp-group-table
  '(("shakai" "社会" "NewsList.aspx?newstype_id=0&type_id=COM")
    ("keizai" "経済" "NewsList.aspx?newstype_id=0&type_id=ECN")
    ("seiji" "政治" "NewsList.aspx?newstype_id=0&type_id=POL")
    ("kokusai" "国際" "NewsList.aspx?newstype_id=0&type_id=INT")
    ("entame" "エンタメ" "NewsList.aspx?newstype_id=0&type_id=ENT")
    ("life" "ライフ" "NewsList.aspx?newstype_id=0&type_id=LIF")
    ("science" "サイエンス" "NewsList.aspx?newstype_id=0&type_id=SIE")
    ("sports" "スポーツ" "NewsList.aspx?newstype_id=0&type_id=SPO")
    ("books" "BOOKS" "NewsList.aspx?newstype_id=0&type_id=BOK")
    ("all" "全記事" "NewsList.aspx")))

(defvar shimbun-ohmynews-jp-x-face-alist
  '(("default" . "X-Face: o|vpDA-})w*TrtnFk9lZ\",j\"y_kn<xZy+LC\\zH(wC$\
Q^ur1c4B3)t\\tK\\yi-Qku8$*\\d<m]\n x;<6rdcYugs1o1w2dObSQ.INk`9f1x!hNe\\\
v*[xW.y6Tt/r=U{a?+nH20N{)a/w145kJxfhqf}Jd<p\n `bP:u\\Awi^xGQ3pUOrsPL.';\
|}zKE@+4GE4!+rd4[>dSxnHe#Z4#\\hy*R&}uSO=(,5UM)-jERou2]H\n ,5\"$Ka&<hoeL")))

(defvar shimbun-ohmynews-jp-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-ohmynews-jp))
  (mapcar 'car shimbun-ohmynews-jp-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-ohmynews-jp))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-ohmynews-jp-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-ohmynews-jp))
  (shimbun-expand-url (nth 2 (assoc (shimbun-current-group-internal shimbun)
				    shimbun-ohmynews-jp-group-table))
		      (shimbun-url-internal shimbun)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-ohmynews-jp)
					 &optional range)
  (let ((regexp1
	 (eval-when-compile
	   (let ((s0 "[\t\n\r ]*")
		 (s1 "[\t\n\r ]+"))
	     (concat
	      "<a" s1 "href=[\"']?"
	      ;; 1. url
	      "\\(http://www\\.ohmynews\\.co\\.jp/News\\.aspx\\?news_id="
	      ;; 2. id
	      "\\([0-9]+\\)"
	      "\\)"
	      "[\"']?" s0 ">" s0 "<h1>" s0
	      ;; 3. subject
	      "\\([^<]+\\)"
	      s0 "</h1>"))))
	(regexp2
	   (let ((s0 "[\t\n\r ]*")
		 (s1 "[\t\n\r ]+"))
	     (concat
	      "/" s0 ">" s0
	      ;; 1. author
	      "\\([^(]+\\)"
	      s0 "("
	      ;; 2. year
	      "\\(20[0-9][0-9]\\)"
	      "/"
	      ;; 3. month
	      "\\([01]?[0-9]\\)"
	      "/"
	      ;; 4. day
	      "\\([0-3]?[0-9]\\)"
	      s1
	      ;; 5. time
	      "\\([012]?[0-9]:[0-5]?[0-9]\\(?::[0-5]?[0-9]\\)\\)"
	      s0 ")" s0 "<")))
	(group (shimbun-current-group-internal shimbun))
	md start url id subject end author year month day time headers)
    (while (cond ((eq md 'end)
		  nil)
		 (md
		  (set-match-data md)
		  (goto-char (setq start (match-end 0))))
		 ((re-search-forward regexp1 nil t)
		  (setq start (match-end 0))))
      (setq url (match-string 1)
	    id (match-string 2)
	    subject (match-string 3))
      (if (re-search-forward regexp1 nil t)
	  (progn
	    (setq end (match-beginning 0)
		  md (match-data))
	    (goto-char start))
	(setq end nil
	      md 'end))
      (when (re-search-forward regexp2 end t)
	(setq author (match-string 1)
	      year (string-to-number (match-string 2))
	      month (string-to-number (match-string 3))
	      day (string-to-number (match-string 4))
	      time (match-string 5))
	(push (shimbun-create-header
	       0 subject
	       (concat shimbun-ohmynews-jp-server-name " (" author ")")
	       (shimbun-make-date-string year month day time)
	       (format "<%d%02d%02d.%s.%s%%%s.%s>"
		       year month day
		       (mapconcat (lambda (elem)
				    (format "%02d" (string-to-number elem)))
				  (split-string time ":")
				  "")
		       id group shimbun-ohmynews-jp-top-level-domain)
	       "" 0 0 url)
	      headers)))
    (shimbun-sort-headers headers)))

(provide 'sb-ohmynews-jp)

;;; sb-ohmynews-jp.el ends here

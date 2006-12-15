;;; sb-yahoo.el --- shimbun backend for news.yahoo.co.jp -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2001, 2002, 2003, 2005, 2006 Kazuyoshi KOREEDA

;; Author: Kazuyoshi KOREEDA <Koreeda.Kazuyoshi@jp.panasonic.com>,
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

;; Original code was sb-asahi.el which is written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org> and
;; Yuuichi Teranishi <teranisi@gohome.org>

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-yahoo (shimbun) ())

(defvar shimbun-yahoo-prefer-text-plain nil
  "*Non-nil means prefer text/plain articles rather than html articles.")

(defvar shimbun-yahoo-url "http://headlines.yahoo.co.jp/")

(defvar shimbun-yahoo-groups-table
  '(("topnews" "トップ" "topnews")
    ("politics" "政治" "pol")
    ("society" "社会" "soci")
    ("people" "人" "peo")
    ("business-all" "経済総合" "bus_all")
    ("market" "市況" "brf")
    ("stock" "株式" "biz")
    ("industry" "産業" "ind")
    ("international" "海外" "int")
    ("entertainment" "エンターテインメント" "ent")
    ("sports" "スポーツ" "spo")
    ("computer" "コンピュータ" "sci")
    ("hokkaido" "北海道" "hok")
    ("tohoku" "東北" "toh")
    ("kanto" "関東" "kan")
    ("sinetsu" "信越" "sin")
    ("hokuriku" "北陸" "hor")
    ("tokai" "東海" "tok")
    ("kinki" "近畿" "kin")
    ("chugoku" "中国" "chu")
    ("sikoku" "四国" "sik")
    ("kyushu" "九州" "kyu")
    ("okinawa" "沖縄" "oki")))
(defvar shimbun-yahoo-groups
  (mapcar 'car shimbun-yahoo-groups-table))

(defvar shimbun-yahoo-from-address "nobody@example.com")
(defvar shimbun-yahoo-content-start "<!---記事-->\
\\(?:[\t\n ]*<h[0-9][^>]*>[^[<]+</h[0-9]>[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*\
\[^<]+[0-9]分配信[^<]*<a[\t\n ]+href=[^>]+>[^<]+</a>\\)?\
\\(?:[\t\n ]*<[^>i][^>]*>\\)*[\t\n ]*")
(defvar shimbun-yahoo-content-end "[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*\
\\(?:最終更新:[01]?[0-9]月\\|<!---コブランド-->\\|<!---/記事-->\\)")

(defvar shimbun-yahoo-x-face-alist
  '(("default" . "X-Face: \"Qj}=TahP*`:b#4o_o63:I=\"~wbql=kpF1a>Sp62\
fpAsVY`saZV[b*GqI!u|i|xKPjNh&P=\n R?n}rh38mkp_:')h=Bh:Rk>0pYF\\I?f\\\
PvPs3>/KG:03n47U?FC[?DNAR4QAQxE3L;m!L10OM$-]kF\n YD\\]-^qzd#'{(o2cu,\
\(}CMi|3b9JDQ(^D\\:@DE}d2+0S2G{VS@E*1Og7Vj#35[77\"z9XBq9$1uF$+W\n u")))
(defvar shimbun-yahoo-expiration-days 7)

(luna-define-method shimbun-index-url ((shimbun shimbun-yahoo))
;;;<DEBUG>
;;  (shimbun-yahoo-index-url shimbun))
;;
;;(defun shimbun-yahoo-index-url (shimbun)
;;;</DEBUG>
  (format "%shl?c=%s&t=l"
	  (shimbun-url-internal shimbun)
	  (nth 2 (assoc (shimbun-current-group-internal shimbun)
			shimbun-yahoo-groups-table))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-yahoo)
					 &optional range)
;;;<DEBUG>
;;  (shimbun-yahoo-get-headers shimbun range))
;;
;;(defun shimbun-yahoo-get-headers (shimbun range)
;;;</DEBUG>
  (let* ((case-fold-search t)
	 (from "Yahoo!ニュース")
	 (group (shimbun-current-group-internal shimbun))
	 (jname (nth 1 (assoc group shimbun-yahoo-groups-table)))
	 id headers)
    (catch 'stop
      (while t
	(while (re-search-forward
		(eval-when-compile
		  (let ((s0 "[\t\n\r ]*")
			(s1 "[\t\n\r ]+"))
		    (concat
		     "<a" s1 "href=\""
		     ;; 1. url
		     "\\(http://headlines\\.yahoo\\.co\\.jp/hl\\?a="
		     ;; 2. serial number
		     "\\("
		     ;; 3. year
		     "\\(20[0-9][0-9]\\)"
		     ;; 4. month
		     "\\([01][0-9]\\)"
		     ;; 5. day
		     "\\([0-3][0-9]\\)"
		     "[^\"]*\\)\\)"
		     "\"" s0 ">" s0
		     ;; 6. subject
		     "\\([^<]+\\)"
		     s0 "</a>\\(?:" s0 "<[^>]+>\\)+" s0 "（" s0
		     ;; 7. source
		     "\\([^）]+\\)"
		     s0 "）" s0 "-" s0 "\\(?:[^<]+)" s0 "\\)?"
		     ;; 8. hour
		     "\\([012]?[0-9]\\)"
		     s0 "時" s0
		     ;; 9. minute
		     "\\([0-5]?[0-9]\\)"
		     s0 "分")))
		nil t)
	  (setq id (concat "<"
			   (save-match-data
			     (shimbun-replace-in-string (match-string 2)
							"-" "."))
			   "%" group ".headlines.yahoo.co.jp>"))
	  (if (shimbun-search-id shimbun id)
	      (throw 'stop nil))
	  (push (shimbun-create-header
		 0
		 (match-string 6)
		 (concat from " (" jname "/" (match-string 7) ")")
		 (shimbun-make-date-string
		  (string-to-number (match-string 3))
		  (string-to-number (match-string 4))
		  (string-to-number (match-string 5))
		  (format "%02d:%02d"
			  (string-to-number (match-string 8))
			  (string-to-number (match-string 9))))
		 id "" 0 0
		 (match-string 1))
		headers))
	(if (re-search-forward "<a href=\"\\([^\"]+\\)\">次のページ</a>" nil t)
	    (progn
	      (shimbun-retrieve-url (prog1
					(match-string 1)
				      (erase-buffer))
				    t)
	      (goto-char (point-min)))
	  (throw 'stop nil))))
    (shimbun-sort-headers headers)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-yahoo)
						   header)
;;;<DEBUG>
;;  (shimbun-yahoo-prepare-article shimbun header))
;;
;;(defun shimbun-yahoo-prepare-article (shimbun header)
;;;</DEBUG>
  (shimbun-with-narrowed-article
   shimbun
   ;; Fix the picture tag.
   (cond ((re-search-forward "[\t\n ]*<center>[\t\n ]*<font[^>]+>\
\[\t\n ]*拡大写真[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)+"
			     nil t)
	  (delete-region (match-beginning 0) (match-end 0))
	  (when (and (shimbun-prefer-text-plain-internal shimbun)
		     (looking-at "[^<>]+"))
	    (replace-match "(写真: \\&)<br>"))
	  (goto-char (point-min)))
	 ((and (shimbun-prefer-text-plain-internal shimbun)
	       (re-search-forward "<img[\t\n ]+[^>]+>\
\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*<font[\t\n ]+[^>]+>[\t\n 　]*\
\\([^<>]+\\)[\t\n ]*</font>"
				  nil t))
	  (if (string-equal (match-string 1) "写真")
	      (replace-match "(写真)<br>")
	    (replace-match "(写真: \\1)<br>"))))
   (if (shimbun-prefer-text-plain-internal shimbun)
       (require 'sb-text) ;; `shimbun-fill-column'
     ;; Open paragraphs.
     (while (re-search-forward "。<br>[\t ]*\n　" nil t)
       (replace-match "。<br><br>\n　"))
     (goto-char (point-min)))
   ;; Correct the Date header and the position of the footer.
   (let (year footer)
     (when (and
	    (setq year (shimbun-header-date header))
	    (string-match " \\(20[0-9][0-9]\\) " year)
	    (progn
	      (setq year (string-to-number (match-string 1 year)))
	      (re-search-forward
	       (eval-when-compile
		 (let ((s0 "[\t\n ]*")
		       (s1 "[\t\n ]+"))
		   (concat
		    "[\t\n 　]*<div" s1 "align=right>" s0
		    ;; 1. footer
		    "\\("
		    "（[^）]+）" s1 "-" s1
		    ;; 2. month
		    "\\([01]?[0-9]\\)"
		    s0 "月" s0
		    ;; 3. day
		    "\\([0-3]?[0-9]\\)"
		    s0 "日" s0
		    ;; 4. hour
		    "\\([012]?[0-9]\\)"
		    s0 "時" s0
		    ;; 5. minute
		    "\\([0-5]?[0-9]\\)"
		    s0 "分" s0 "更新"
		    "\\)"
		    s0 "</div>\\(?:" s0 "<br>\\)*")))
	       nil t)))
       (shimbun-header-set-date
	header
	(shimbun-make-date-string
	 year
	 (string-to-number (match-string 2))
	 (string-to-number (match-string 3))
	 (format "%02d:%02d"
		 (string-to-number (match-string 4))
		 (string-to-number (match-string 5)))))
       (setq footer (match-string 1))
       (delete-region (match-beginning 0) (match-end 0))
       (if (shimbun-prefer-text-plain-internal shimbun)
	   (insert "<br><br>"
		   (make-string (max (- (symbol-value 'shimbun-fill-column)
					(string-width footer))
				     0)
				? )
		   footer "<br>")
	 (insert "<br><br><div align=right>" footer "</div>")
	 ;; Break long Japanese lines.
	 (goto-char (point-min))
	 (while (re-search-forward "<p[^>]*>\\|</p>\\|[、。）」]+" nil t)
	   (unless (eolp)
	     (insert "\n"))))))))

(provide 'sb-yahoo)

;;; sb-yahoo.el ends here

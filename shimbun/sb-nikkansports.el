;;; sb-nikkansports.el --- shimbun backend for www.nikkansports.com -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001 MIYOSHI Masanori <miyoshi@boreas.dti.ne.jp>

;; Author: MIYOSHI Masanori <miyoshi@boreas.dti.ne.jp>
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

(luna-define-class shimbun-nikkansports
		   (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-nikkansports-url "http://www.nikkansports.com/")
(defvar shimbun-nikkansports-server-name "日刊スポーツ")
(defvar shimbun-nikkansports-group-table
  '(("baseball" "野球" "ns/baseball/top-bb.html")
    ("mlb" "大リーグ" "ns/baseball/mlb/top-tp2-bb.html")
    ("soccer" "サッカー" "ns/soccer/top-sc.html")
    ("world-soccer" "海外サッカー" "ns/soccer/world/top-tp2-sc.html")
    ("sports" "スポーツ" "ns/sports/top-sp.html")
    ("battle" "バトル" "ns/battle/top-bt.html")
    ("horseracing" "競馬" "ns/horseracing/top-hr.html")
    ("entertainment" "芸能" "ns/entertainment/top-et.html")
    ("society" "社会" "ns/general/top-so.html")
    ("leisure" "釣り" "ns/leisure/top-ls.html")))
(defvar shimbun-nikkansports-from-address "webmast@nikkansports.co.jp")
(defvar shimbun-nikkansports-content-start
  "<H2>[^<]+</H2>\n\\(<img[^>]*>\n\\)?")
(defvar shimbun-nikkansports-content-end
      "［[0-9]+/[0-9]+/[0-9]+/[0-9]+:[0-9]+")
(defvar shimbun-nikkansports-expiration-days 17)

(defvar shimbun-nikkansports-end-of-header-regexp
  (concat "\n<!--\\("
	  " End of Header "	"\\|"
	  "header"		"\\|"
	  "◆◆◆◆◆◆ここまでヘッダー◆◆◆◆◆◆"
	  "\\)-->\n")
  "*Regexp used to look for the end of the header in a html contents.")

(luna-define-method shimbun-groups ((shimbun shimbun-nikkansports))
  (mapcar 'car shimbun-nikkansports-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-nikkansports))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-nikkansports-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-nikkansports))
  (concat (shimbun-url-internal shimbun)
	  (nth 2 (assoc (shimbun-current-group-internal shimbun)
			shimbun-nikkansports-group-table))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-nikkansports)
					 &optional range)
  (when (re-search-forward shimbun-nikkansports-end-of-header-regexp nil t)
    (delete-region (point-min) (point))
    (when (re-search-forward
	   "\n<!--\\( Start of Footer \\|footer-\\)-->\n" nil t)
      (forward-line -1)
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (let ((case-fold-search t) headers)
	(while (re-search-forward
		"<li><a href=\"/\\(.+\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)-\\([0-9]+\\)\\.html\\)\">\\([^<]+\\)</a>" nil t)
	  (let ((url (match-string 1))
		(year (match-string 2))
		(month (match-string 3))
		(day (match-string 4))
		(no (match-string 5))
		(subject (match-string 6))
		id date)
	    (setq id (format "<%s%s%s%s.%s@nikkansports.co.jp>"
			     year month day no
			     (shimbun-current-group-internal shimbun)))
	    (setq date (shimbun-make-date-string
			(string-to-number year)
			(string-to-number month)
			(string-to-number day)))
	    (push (shimbun-make-header
		   0
		   (shimbun-mime-encode-string subject)
		   (shimbun-from-address shimbun)
		   date id "" 0 0
		   (concat
		    (shimbun-url-internal shimbun)
		    url))
		  headers)))
	headers))))

(provide 'sb-nikkansports)

;;; sb-nikkansports.el ends here

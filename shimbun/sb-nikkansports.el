;;; sb-nikkansports.el --- shimbun backend for www.nikkansports.com -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2007
;; MIYOSHI Masanori <miyoshi@meadowy.org>

;; Author: MIYOSHI Masanori <miyoshi@meadowy.org>
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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-nikkansports
		   (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-nikkansports-url "http://www.nikkansports.com/")

(defvar shimbun-nikkansports-server-name "日刊スポーツ")

(defvar shimbun-nikkansports-group-table
  '(("baseball" "野球" "baseball/top-baseball.html")
    ("soccer" "サッカー" "soccer/top-soccer.html")
    ("sports" "スポーツ" "sports/top-sports.html")
    ("battle" "バトル" "battle/top-battle.html")
    ("horseracing" "競馬" "race/top-race.html")
    ("entertainment" "芸能" "entertainment/top-entertainment.html")
    ("society" "社会" "general/top-general.html")
    ("lifestyle" "ライフ" "http://lifestyle.nikkansports.com/")))

(defvar shimbun-nikkansports-content-start
  "<!-+[\t\n ]*\\++[\t\n ]*\
\\(?:ニュース本文エリア\\|トップニュース\\|コンテンツここから\\)[\t\n ]*\
\\++[\t\n ]*-+>\\(?:[\t\n ]*<[^>h][^>]*>\\)*[\t\n ]*<h[0-9]\
\\(?:[\t\n ]+[^>]+\\)*>\[^<]+</h[0-9]>\\(?:[\t\n ]*<![^>]*>\\)*[\t\n ]*")

(defvar shimbun-nikkansports-content-end
  "\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*\\(?:\
\［20[0-9][0-9]年[ 01]?[0-9]月[ 0-3]?[0-9]日\
\\(?:[012]?[0-9]時[0-5]?[0-9]分\\|[\t\n ]*[012]?[0-9]:[0-5][0-9]\\)[^］]*］\
\\|<!-+[\t\n ]*google_ad_section_end\
\\|<script[\t\n ]+type=\"text/javascript\"\
\\|<!-+[\t\n ]*\\++[\t\n ]*関連情報[\t\n ]*\\++[\t\n ]*-+>\
\\|<!-+[\t\n ]*/\\++[\t\n ]*ニュース本文エリア[\t\n ]*\\++[\t\n ]*-+>\\)")

(defvar shimbun-nikkansports-expiration-days 17)

(luna-define-method shimbun-groups ((shimbun shimbun-nikkansports))
  (mapcar 'car shimbun-nikkansports-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-nikkansports))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-nikkansports-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-nikkansports))
  (let ((index (nth 2 (assoc (shimbun-current-group-internal shimbun)
			     shimbun-nikkansports-group-table))))
    (if (string-match "\\`http:" index)
	index
      (concat shimbun-nikkansports-url index))))

(defun shimbun-nikkansports-expand-url (url group)
  (cond ((string-match "\\`http:" url)
	 url)
	((string-equal group "life")
	 (shimbun-expand-url url "http://lifestyle.nikkansports.com/"))
	(t
	 (shimbun-expand-url url shimbun-nikkansports-url))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-nikkansports)
					 &optional range)
  (let* ((case-fold-search t)
	 (group (shimbun-current-group-internal shimbun))
	 (from (concat shimbun-nikkansports-server-name " ("
		       (nth 1 (assoc group shimbun-nikkansports-group-table))
		       ")"))
	 headers)
    (while (re-search-forward
	    (eval-when-compile
	      (let ((s0 "[\t\n ]*") (s1 "[\t\n ]+"))
		(concat
		 "<a" s1 "href=\""
		 ;; 1. url
		 "\\(\\(?:http:/\\)?/\\(?:[^\"/>]+/\\)+"
		 ;; 2. serial number
		 "\\(\\(?:[^\"/>]+-\\)?"
		 ;; 3. year
		 "\\(20[0-9][0-9]\\)"
		 ;; 4. month
		 "\\([01][0-9]\\)"
		 "[0-3][0-9]-[0-9]+\\)"
		 "\\.html\\)"
		 "\">" s0
		 ;; 5. subject
		 "\\([^<]+\\)"
		 s0 "</a>[^<]*［" s0
		 ;; 6. day
		 "\\([0-3]?[0-9]\\)"
		 s0 "日" s0
		 ;; 7. time
		 "\\([012][0-9]:[0-5][0-9]\\)"
		 s0 "］")))
	    nil t)
      (push (shimbun-create-header
	     0
	     (match-string 5)
	     from
	     (shimbun-make-date-string
	      (string-to-number (match-string 3))
	      (string-to-number (match-string 4))
	      (string-to-number (match-string 6))
	      (match-string 7))
	     (concat "<" (shimbun-subst-char-in-string ?- ?. (match-string 2))
		     "%" group ".nikkansports.com>")
	     "" 0 0
	     (shimbun-nikkansports-expand-url (match-string 1) group))
	    headers))
    (shimbun-sort-headers headers)))

(luna-define-method shimbun-clear-contents :around ((shimbun
						     shimbun-nikkansports)
						    header)
  (when (luna-call-next-method)
    ;; Remove garbage.
    (goto-char (point-min))
    (while (re-search-forward
	    "[\t\n ]*<p>[^<]画像クリックで拡大表示[\t\n ]*</p>[\t\n ]*"
	    nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    ;; Break long lines.
    (unless (shimbun-prefer-text-plain-internal shimbun)
      (shimbun-break-long-japanese-lines))
    t))

(provide 'sb-nikkansports)

;;; sb-nikkansports.el ends here

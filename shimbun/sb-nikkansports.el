;;; sb-nikkansports.el --- shimbun backend for www.nikkansports.com -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005
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
    ("sports" "スポーツ" "ns/sports/top-sp.html")
    ("battle" "バトル" "ns/battle/top-bt.html")
    ("horseracing" "競馬" "ns/horseracing/top-hr.html")
    ("entertainment" "芸能" "ns/entertainment/top-et.html")
    ("society" "社会" "ns/general/top-so.html")
    ("leisure" "釣り" "ns/leisure/top-ls.html")))
(defvar shimbun-nikkansports-content-start
  "<!--emacs-w3m-shimbun-nikkansports-content-start-->")
(defvar shimbun-nikkansports-content-end
  "<!--emacs-w3m-shimbun-nikkansports-content-end-->")
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
;;;<DEBUG>
;;  (shimbun-nikkansports-get-headers shimbun))
;;
;;(defun shimbun-nikkansports-get-headers (shimbun)
;;;</DEBUG>
  (when (re-search-forward shimbun-nikkansports-end-of-header-regexp nil t)
    (delete-region (point-min) (point))
    (when (re-search-forward
	   "\n<!--\\( Start of Footer \\|footer-\\)-->\n" nil t)
      (forward-line -1)
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (let* ((case-fold-search t)
	     (group (shimbun-current-group-internal shimbun))
	     (from (concat shimbun-nikkansports-server-name " ("
			   (nth 1 (assoc group
					 shimbun-nikkansports-group-table))
			   ")"))
	     year month day headers)
	(while (re-search-forward
		(eval-when-compile
		  (concat
		   "<li><a href=\"/"
		   ;; 1. url
		   "\\("
		   "[^>]+"
		   ;; 2. year
		   "\\([0-9][0-9]\\)"
		   ;; 3. month
		   "\\([0-9][0-9]\\)"
		   ;; 4. day
		   "\\([0-9][0-9]\\)"
		   "-"
		   ;; 5. serial number
		   "\\([0-9]+\\)"
		   "\\.html\\)"
		   "\">"
		   ;; 6. subject
		   "\\([^<]+\\)"
		   "</a>\\([^<]+［[0-3]?[0-9]日"
		   ;; 8. hour:minute
		   "\\([012]?[0-9]:[0-5]?[0-9]\\)"
		   "］\\)?"))
		nil t)
	  (setq year (string-to-number (match-string 2))
		month (string-to-number (match-string 3))
		day (string-to-number (match-string 4)))
	  (push (shimbun-create-header
		 0
		 (match-string 6)
		 from
		 (shimbun-make-date-string year month day (match-string 8))
		 (format "<20%02d%02d%02d%s.%s%%nikkansports.com>"
			 year month day (match-string 5) group)
		 "" 0 0
		 (concat (shimbun-url-internal shimbun)
			 (match-string 1)))
		headers))
	(shimbun-sort-headers headers)))))

(luna-define-method shimbun-make-contents
  :before ((shimbun shimbun-nikkansports) header)
;;;<DEBUG>
;;  (shimbun-nikkansports-prepare-article shimbun header))
;;
;;(defun shimbun-nikkansports-prepare-article (shimbun header)
;;;</DEBUG>
  (let ((case-fold-search t)
	start)
    (when (or (re-search-forward "<img[\t\n ]+[^>]*class=\"photo\"" nil t)
	      (re-search-forward "<p[\t\n ]+class=\"txt2h\">" nil t))
      (setq start (match-beginning 0))
      (goto-char (point-max))
      (when (and (re-search-backward "<p[\t\n ]+class=\"txt2h\">" nil t)
		 (search-forward "</p>" nil t))
	(save-restriction
	  (narrow-to-region start (point))
	  (shimbun-break-long-japanese-lines)
	  (goto-char (point-min))
	  (insert shimbun-nikkansports-content-start)
	  (goto-char (point-max))
	  (insert shimbun-nikkansports-content-end)))
      (goto-char (point-min)))))

(provide 'sb-nikkansports)

;;; sb-nikkansports.el ends here

;;; sb-nikkangendai.el --- shimbun backend for Nikkan Gendai -*- coding: utf-8; -*-

;; Copyright (C) 2015 Katsumi Yamaoka

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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Nikkan Gendai is an evening paper in Japan.

;;; Code:

(require 'shimbun)
(require 'sb-multi)

(luna-define-class shimbun-nikkangendai
		   (shimbun-japanese-newspaper shimbun-multi shimbun) ())

(defvar shimbun-nikkangendai-top-level-domain "www.nikkan-gendai.com"
  "Name of the top level domain for Nikkan Gendai.")

(defvar shimbun-nikkangendai-url
  (concat "http://" shimbun-nikkangendai-top-level-domain "/")
  "Name of the root url.")

(defvar shimbun-nikkangendai-server-name "日刊ゲンダイ")

(defvar shimbun-nikkangendai-group-table
  '(("top" "TOP")
    ("news" "ニュース")
    ("geino" "芸能")
    ("sports" "スポーツ")
    ("life" "ライフ")
    ("book" "BOOKS")
    ("gourmet" "グルメ")))

(defvar shimbun-nikkangendai-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAwAgMAAAAqbBEUAAAADFBMVEX////yo4H40cPrYQC
 60gGqAAABU0lEQVQoz5XSP07DMBQG8OdUKCodurBWniqiLjlCow4sLHAC2BjY2dCL4BCwQRkQt0i
 5QYREy9ZMzBmsNEJ1Pp7dqumEhKf89EXvj2XC3qF/wLZYIdf49LCjF9T98/IYYGp0yXXvzSjYzAG
 lOlkTGp8gD5eWYASjZxS65gA5qBmeYUakr/EO/1tK1Ac6HlZdHj6hCT3WYRkCeR+uQK3lA4n2ScF
 5jIpYxmFcYaZRBJupGcn4m3oOq0pyTigW/EwfuVHSl+EKRDI1CoUtCo0y9hhEt13AjB3mKrphlms
 w0nQ+jNjEOIWR2exAoOwUtRLoI14HixBl4GAkmSxRdB2+2Mimfmqrc9QHqSyTyWzsbjTVjVvBXWu
 RUVb7RE5iiYttYtUr4S7ZwMSJqh5mG3xwLmtuE2nYAkg7LRpSLWp13yKJFztUAWOHyQWwV82dfB/
 2r7fzC4CxojZUJhCfAAAAAElFTkSuQmCC")))

(defvar shimbun-nikkangendai-expiration-days nil)

(defvar shimbun-nikkangendai-japanese-hankaku t)

(luna-define-method shimbun-groups ((shimbun shimbun-nikkangendai))
  (mapcar 'car shimbun-nikkangendai-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-nikkangendai))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-nikkangendai-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-nikkangendai))
  (let ((group (shimbun-current-group-internal shimbun))
	(base (shimbun-url-internal shimbun)))
    (if (string-equal group "top")
	base
      (shimbun-expand-url (concat "articles/index/" group) base))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-nikkangendai)
					 &optional range)
  (if (string-equal (shimbun-current-group-internal shimbun) "top")
      (shimbun-nikkangendai-get-headers-top shimbun range)
    (shimbun-nikkangendai-get-headers shimbun range)))

(defun shimbun-nikkangendai-get-headers-top (shimbun range)
  (let ((regexp
	 (eval-when-compile
	   (concat
	    "<a[\t\n\r ]+href=\""
	    ;; 1. url
	    "\\(/articles/view/"
	    ;; 2. group
	    "\\([a-z]+\\)" "/"
	    ;; 3. serial
	    "\\([0-9]+\\)\\)"
	    "[^>]+>[\t\n\r ]*<span\\(?:[\t\n\r ]+[^\t\n\r >]+\\)*>[\t\n\r ]*"
	    ;; 4. subject
	    "\\([^<]+\\)")))
	(base (shimbun-url-internal shimbun))
	url group serial subject id year month day from headers)
    (when (re-search-forward
	   "[\t\n\r ]*<!-+[\t\n\r ]*top[\t\n\r ]+match[\t\n\r ]+" nil t)
      (delete-region (match-beginning 0) (point-max))
      (goto-char (point-min)))
    (while (re-search-forward regexp nil t)
      (setq url (shimbun-expand-url (match-string 1) base)
	    group (match-string 2)
	    serial (match-string 3)
	    subject (match-string 4))
      (setq id (concat "<" serial "." group "%"
		       shimbun-nikkangendai-top-level-domain ">"))
      (when (and (not (shimbun-search-id shimbun id))
		 (with-temp-buffer
		   (shimbun-retrieve-url url)
		   (goto-char (point-min))
		   (when (re-search-forward "\
<p[\t\n\r ]+\\(?:[^\t\n\r >]+[\t\n\r ]+\\)*class=\"date-create[^>]+>[\t\n\r ]*\
\\(20[1-9][0-9]\\)年\\([01]?[0-9]\\)月\\([0-3]?[0-9]\\)日" nil t)
		     (setq year (string-to-number (match-string 1))
			   month (string-to-number (match-string 2))
			   day (string-to-number (match-string 3))))))
	(when (or (string-match "[\t\n\r ]*&nbsp;[\t\n\r ]*\\'" subject)
		  (string-match "[\t\n\r ]+'" subject))
	  (setq subject (substring subject 0 (match-beginning 0))))
	(setq from (concat shimbun-nikkangendai-server-name " ("
			   (or (cadr (assoc group
					    shimbun-nikkangendai-group-table))
			       (cadr (assoc (substring group 0 -1)
					    shimbun-nikkangendai-group-table))
			       group)
			   ")"))
	(push (shimbun-create-header
	       0 subject from
	       (shimbun-make-date-string year month day)
	       id "" 0 0 url)
	      headers)))
    headers))

(defun shimbun-nikkangendai-get-headers (shimbun range)
  (let ((regexp
	 (eval-when-compile
	   (concat
	    "<a[\t\n\r ]+href=\""
	    ;; 1. url
	    "\\(/articles/view/"
	    ;; 2. group
	    "\\([a-z]+\\)" "/"
	    ;; 3. serial
	    "\\([0-9]+\\)\\)"
	    "[^>]+>[\t\n\r ]*"
	    ;; 4. subject
	    "\\([^<]+\\)"
	    "\\(?:<[/b-z][^>]*>[\t\n\r ]*\\)*"
	    "\\(?:"
	    ;; 5. year
	    "\\(20[1-9][0-9]\\)年"
	    ;; 6. month-1
	    "\\([01]?[0-9]\\)月"
	    ;; 7. day-1
	    "\\([0-3]?[0-9]\\)日"
	    "\\|\("
	    ;; 8. month-2
	    "\\([01]?[0-9]\\)月"
	    ;; 9. day-2
	    "\\([0-3]?[0-9]\\)日"
	    "\\)")))
	(base (shimbun-url-internal shimbun))
	cyear cmonth url group serial subject year month day id from headers)
    (setq cyear (shimbun-decode-time nil 32400)
	  cmonth (nth 4 cyear))
    (setq cyear (nth 5 cyear))
    (while (re-search-forward regexp nil t)
      (setq url (shimbun-expand-url (match-string 1) base)
	    group (match-string 2)
	    serial (match-string 3)
	    subject (match-string 4)
	    year (match-string 5))
      (if year
	  (setq year (string-to-number year)
		month (string-to-number (match-string 6))
		day (string-to-number (match-string 7)))
	(setq month (string-to-number (match-string 8))
	      day (string-to-number (match-string 9)))
	(setq year (if (and (= cmonth 1) (= month 12))
		       (1- cyear)
		     cyear)))
      (when (or (string-match "[\t\n\r ]*&nbsp;[\t\n\r ]*\\'" subject)
		(string-match "[\t\n\r ]+'" subject))
	(setq subject (substring subject 0 (match-beginning 0))))
      (setq id (concat "<" serial "." group "%"
		       shimbun-nikkangendai-top-level-domain ">")
	    from (concat shimbun-nikkangendai-server-name " ("
			 (or (cadr (assoc group
					  shimbun-nikkangendai-group-table))
			     (cadr (assoc (substring group 0 -1)
					  shimbun-nikkangendai-group-table))
			     group)
			 ")"))
      (push (shimbun-create-header
	     0 subject from
	     (shimbun-make-date-string year month day)
	     id "" 0 0 url)
	    headers))
    headers))

(luna-define-method shimbun-multi-next-url ((shimbun shimbun-nikkangendai)
					    header url)
;;  (shimbun-nikkangendai-multi-next-url shimbun header url))
;;(defun shimbun-nikkangendai-multi-next-url (shimbun header url)
  (goto-char (point-min))
  (when (re-search-forward "<a[\t\n\r ]+href=\"\\([^\"]+\\)\"[\t\n\r ]*>\
[\t\n\r ]*次へ&gt;&gt;[\t\n\r ]*</a>" nil t)
    (shimbun-expand-url (match-string 1) url)))

(luna-define-method shimbun-clear-contents :around
  ((shimbun shimbun-nikkangendai) header)
;;  (shimbun-nikkangendai-clear-contents shimbun header))
;;(defun shimbun-nikkangendai-clear-contents (shimbun header)
  (goto-char (point-min))
  (when (and (re-search-forward "<div[\t\n\r ]+\\(?:[^\t\n\r >]+[\t\n\r ]+\\)*\
class=\"detail-topic[\t\n\r ]+[^>]+>[\t\n\r ]*" nil t)
	     (progn
	       (delete-region (point-min) (match-end 0))
	       (re-search-forward "\
<span[\t\n\r ]+\\(?:[^\t\n\r >]+[\t\n\r ]+\\)*class=\"article_body\"" nil t)
	       (shimbun-end-of-tag "span")))
    (delete-region (goto-char (match-end 0)) (point-max))
    (insert "\n")
    (narrow-to-region (goto-char (point-min)) (match-beginning 0))
    (let ((head (when (re-search-forward "\
<div[\t\n\r ]+\\(?:[^\t\n\r >]+[\t\n\r ]+\\)*class=\"article_head\"" nil t)
		  (match-beginning 0))))
      (goto-char (point-min))
      (if (re-search-forward "<img[\t\n\r ]+\\(?:[^\t\n\r >]+[\t\n\r ]+\\)*\
\\(?:alt=\"\\([^\"]+\\)\"\\)?[^>]*>" nil t)
	  (insert
	   (prog1
	       (if (match-beginning 1)
		   (concat (match-string 1) "<br>\n"
			   (buffer-substring (match-beginning 0)
					     (match-beginning 1))
			   "[写真]"
			   (buffer-substring (match-end 1) (match-end 0)))
		 (match-string 0))
	     (delete-region (point-min) (or head (point-max))))
	   "<br><br>\n")
	(delete-region (point-min) (or head (point-max)))))
    (widen)
    (unless (memq (shimbun-japanese-hankaku shimbun)
		  '(header subject nil))
      (shimbun-japanese-hankaku-buffer t))
    t))

(luna-define-method shimbun-multi-clear-contents
  ((shimbun shimbun-nikkangendai) header has-previous-page has-next-page)
;;  (shimbun-nikkangendai-multi-clear-contents shimbun header
;;					     has-previous-page))
;;(defun shimbun-nikkangendai-multi-clear-contents (shimbun header
;;							  has-previous-page)
  (when (shimbun-clear-contents shimbun header)
    (when has-previous-page
      (goto-char (point-min))
      (insert "&#012;\n"))
    t))

(luna-define-method shimbun-footer :around ((shimbun shimbun-nikkangendai)
					    header &optional html)
;;  (shimbun-nikkangendai-footer shimbun header))
;;(defun shimbun-nikkangendai-footer (shimbun header)
  (concat "<div align=\"left\">\n--&nbsp;<br>\n\
この記事の著作権は株式会社・日刊現代に帰属します。<br>\n原物は <a href=\""
	  (shimbun-article-base-url shimbun header)
	  "\">ここ</a> で公開されています。\n</div>\n"))

(provide 'sb-nikkangendai)

;;; sb-nikkangendai.el ends here

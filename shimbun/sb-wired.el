;;; sb-wired.el --- shimbun backend for Hotwired Japan

;; Copyright (C) 2001, 2002, 2003, 2004, 2005
;; Yuuichi Teranishi <teranisi@gohome.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Yuuichi Teranishi <teranisi@gohome.org>
;;         NOMIYA Masaru <nomiya@ttmy.ne.jp>
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

(eval-when-compile
  (require 'cl))

(require 'shimbun)
(require 'sb-lump)

(luna-define-class shimbun-wired (shimbun-lump) ())

(defvar shimbun-wired-url "http://hotwired.goo.ne.jp/")
(defvar shimbun-wired-groups '("business" "culture" "technology"))
(defvar shimbun-wired-from-address "Hotwired Japan")
(defvar shimbun-wired-content-start
  "\\(\n<!-- ARTICLE/-->\\|<FONT color=\"#ff0000\" size=\"-1\">.*</FONT>\\)\n")
(defvar shimbun-wired-content-end "\\(\n<!-- /ARTICLE-->\n\\|<DIV ALIGN=\"RIGHT\">\\[\\)")
(defvar shimbun-wired-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAQBAMAAACigOGCAAAABGdBTUEAALGPC/xhBQAAADB
 QTFRFAAAAAgIBBgcFCgsJDA0MDhANEhYOHDALK0kQOGEUSn8aYaYies4piugulfkymf8zRL4klQA
 AAC90RVh0U29mdHdhcmUAWFYgVmVyc2lvbiAzLjEwYStGTG1hc2sgIFJldjogMTIvMjkvOTQbx6p
 8AAABfUlEQVR42h3MTyiDcRzH8e+ImT3KckBRNmGHPRfPRdpt5sBNe3bgynJRi5Q1jfL/T1moqSm
 i/Nth6jkMI8lh2pJQ6/c8K26PUTju+Un7+T5+h1effu9+P/D5fN6tbnRwQVec9+o3c17A3VucE0W
 x53Mc7c0NoQM5ETwed4hJLsE9wiSP4AmxTZfQtVachf7hsfiL2pkc22GqEBGu2W3nBSqBM7h3FdB
 4NnpyedZO2tNB1ZFrllefwTm5l2lW+N++8+iJjbS8HxUch22aXQV+e18+TNbnbWQ5ZiVtNEsdCTu
 VCPBf+/l00kJqyFLMQmrpIm34qKPZO+C/NzAAKc1HYkAqaOCHk016aLxcl88wlJAD1EhTlJup0L+
 qioYVmwIESCINpJymNO7IqLW+QdXK7lWHhiGZk/GdIhe43/L3oArmCX/8XoUUxFkBfWBP5ozhlT2
 CoRpC7AasMIVaMJ8adHcBj7k4jTawMDryv0NFvx7KjqtR40UTalL0XSnDH6TavYxiVdRgAAAAB3R
 JTUUH1AoWBQgh+MK8xgAAAABJRU5ErkJggg==")))

(defvar shimbun-wired-expiration-days 7)

(luna-define-method shimbun-get-group-header-alist ((shimbun shimbun-wired)
						    &optional range)
;;;<DEBUG>
;;  (shimbun-wired-get-group-header-alist shimbun))
;;
;;(defun shimbun-wired-get-group-header-alist (shimbun)
;;;</DEBUG>
  (let ((group-header-alist (mapcar (lambda (g) (cons g nil))
				    (shimbun-groups-internal shimbun)))
	(case-fold-search t)
	(regexp (format
		 "<a href=\"\\(%s\\|/\\)\\(news/\\(%s\\)/story/\\(\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)[0-9]+\\)\\.html\\)[^>]*\">"
		 (regexp-quote (shimbun-url-internal shimbun))
		 (shimbun-regexp-opt (shimbun-groups-internal shimbun))))
	ids)
    (dolist (xover (list (concat (shimbun-url-internal shimbun)
				 "news/index.html")
			 (concat (shimbun-url-internal shimbun)
				 "news/last_seven.html")))
      (with-temp-buffer
	(shimbun-retrieve-url xover t)
	(goto-char (point-min))
	(search-forward "<!-- ARTICLE/-->" nil t) ; Jump to article list.
	(while (re-search-forward regexp nil t)
	  (let* ((url   (concat (shimbun-url-internal shimbun)
				(match-string 2)))
		 (group (downcase (match-string 3)))
		 (id    (format "<%s%%%s.hotwired-japan>"
				(match-string 4) group))
		 (date  (shimbun-make-date-string
			 (string-to-number (match-string 5))
			 (string-to-number (match-string 6))
			 (string-to-number (match-string 7))))
		 (header (shimbun-make-header
			  0
			  (shimbun-mime-encode-string
			   (mapconcat 'identity
				      (split-string
				       (buffer-substring
					(match-end 0)
					(progn (search-forward "</span>" nil t) (point)))
				       "<[^>]+>")
				      ""))
			  (concat shimbun-wired-from-address " (" group ")")
			  date id "" 0 0 url))
		 (x (assoc group group-header-alist)))
	    (unless (member id ids)
	      (setq ids (cons id ids))
	      (setcdr x (cons header (cdr x))))))))
    group-header-alist))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-wired)
						   header)
  ;; Break long lines.
  (shimbun-break-long-japanese-lines shimbun))

(provide 'sb-wired)

;;; sb-wired.el ends here

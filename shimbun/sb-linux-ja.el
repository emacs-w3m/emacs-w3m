;;; sb-linux-ja.el --- shimbun backend for japan.linux.com.

;; Copyright (C) 2003, 2004 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
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

;;; Acknowledgment:

;; I would like to thank Hidetaka Iwai <tyuyu@bozu.sytes.net>, who
;; wrote sb-jlc.el <http://bozu.sytes.net/~tyuyu/diary/sb-jlc.el>.
;; I refered it to decide values of regular expressions in this
;; backend.

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-linux-ja (shimbun-rss) ())

(defvar shimbun-linux-ja-url "http://japan.linux.com/")
(defvar shimbun-linux-ja-groups '("news" "desktop" "enterprise"
				  "kernel" "opensource" "security"))
(defvar shimbun-linux-ja-from-address  "webmaster@japan.linux.com")
(defvar shimbun-linux-ja-content-start "^<table [^<>]* summary=\"article\">")
(defvar shimbun-linux-ja-content-end
  "<!-- start template: ID 186, vote;pollBooth;default -->")

(luna-define-method shimbun-index-url ((shimbun shimbun-linux-ja))
  (shimbun-expand-url (concat (shimbun-current-group-internal shimbun) ".rdf")
		      shimbun-linux-ja-url))

(luna-define-method shimbun-rss-build-message-id ((shimbun shimbun-linux-ja)
						  url date)
  (when (string-match "[?#]" url)
    (setq url (substring url 0 (match-beginning 0))))
  (unless (string-match "\\`http://japan.linux.com/\\(.*\\)\\.[^.]*\\'" url)
    (error "Cannot find message-id base"))
  (concat "<" (match-string 1 url) "@japan.linux.com>"))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-linux-ja)
						    header)
  (when (luna-call-next-method)
    (shimbun-remove-tags "<SCRIPT" "</SCRIPT>")
    (shimbun-remove-tags "<NOSCRIPT" "</NOSCRIPT>")
    (shimbun-remove-tags "<table [^<>]* summary=\"title\">" "</table>")
    (shimbun-remove-tags "<div[^<>]*><img src=\"/images/separate-dots.png\""
			 "</div>")
    (shimbun-remove-tags "<a href=\"/print.pl\\?" "</a>")
    t))

(provide 'sb-linux-ja)

;;; sb-linux-ja.el ends here

;;; sb-opentechpress-jp.el --- shimbun backend for japan.linux.com -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2006, 2007 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

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
;; Inc.; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'shimbun)
(require 'sb-multi)
(require 'sb-rss)

(luna-define-class shimbun-opentechpress-jp (shimbun-multi shimbun-rss) ())

(defvar shimbun-opentechpress-jp-table
  '(("general" . "http://opentechpress.jp/index.rss")
    ("enterprise" . "http://opentechpress.jp/enterprise.rss")
    ("opensource" . "http://opentechpress.jp/opensource.rss")
    ("developer" . "http://opentechpress.jp/developer.rss")
    ("security" . "http://opentechpress.jp/security.rss")
    ("news" . "http://opentechpress.jp/news.rss")
    ("pr" . "http://opentechpress.jp/pr.rss")))

(defvar shimbun-opentechpress-jp-content-start
  "<div class=\"\\(intro\\|full\\)\">")
(defvar shimbun-opentechpress-jp-content-end "<div class=\"google-ad\">")

(luna-define-method shimbun-groups ((shimbun shimbun-opentechpress-jp))
  (mapcar 'car shimbun-opentechpress-jp-table))

(luna-define-method shimbun-index-url ((shimbun shimbun-opentechpress-jp))
  (cdr (assoc (shimbun-current-group shimbun) shimbun-opentechpress-jp-table)))

(luna-define-method shimbun-rss-build-message-id ((shimbun
						   shimbun-opentechpress-jp)
						  url &optional date)
  (concat "<" (md5 (if (string-match "[?#]" url)
		       (substring url 0 (match-beginning 0))
		     url))
	  "+" (when (string-match "[?&]sid=\\([^&]+\\)\\(&\\|\\'\\)" url)
		(match-string 1 url))
	  "%" (shimbun-current-group shimbun)
	  "@" (shimbun-server shimbun) ".shimbun.namazu.org>"))

(luna-define-method shimbun-multi-next-url ((shimbun shimbun-opentechpress-jp)
					    header url)
  (goto-char (point-min))
  (when (re-search-forward "<a href=\"\\([^\"]+\\)\" title=\"次のページ\">"
			   nil t)
    (shimbun-expand-url (shimbun-decode-entities-string (match-string 1))
			url)))

(luna-define-method shimbun-multi-clear-contents ((shimbun
						   shimbun-opentechpress-jp)
						  header
						  has-previous-page
						  has-next-page)
  (when (shimbun-clear-contents shimbun header)
    (when (or has-previous-page has-next-page)
      (goto-char (point-max))
      (when (re-search-backward "<br>ページ:\\(<a href[^>]*>\\)?&lt;" nil t)
	(delete-region (point) (point-max))))
    t))

(luna-define-method shimbun-clear-contents :before ((shimbun
						     shimbun-opentechpress-jp)
						    header)
  (shimbun-remove-tags "<SCRIPT" "</SCRIPT>")
  (shimbun-remove-tags "<NOSCRIPT" "</NOSCRIPT>"))

(provide 'sb-opentechpress-jp)

;;; sb-opentechpress-jp.el ends here

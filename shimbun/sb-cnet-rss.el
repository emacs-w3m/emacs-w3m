;;; sb-cnet-rss.el --- shimbun backend for cnet-rss

;; Copyright (C) 2003 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Keywords: news
;; Created: Jun 14, 2003

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

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-cnet-rss (shimbun-rss) ())

(defvar shimbun-cnet-rss-group-alist
  '(("news" . "http://japan.cnet.com/rss/index.rdf")
    ("blog.umeda" . "http://blog.japan.cnet.com/umeda/index.rdf")
    ("blog.lessig" . "http://blog.japan.cnet.com/lessig/index.rdf")
    ("blog.mori" . "http://blog.japan.cnet.com/mori/index.rdf")
    ("blog.kenn" . "http://blog.japan.cnet.com/kenn/index.rdf")))

(defvar shimbun-cnet-rss-from-address  "webmaster@japan.cnet.com")
(defvar shimbun-cnet-rss-content-start "<div class=\"leaf_body\">")
(defvar shimbun-cnet-rss-content-end "<!--NEWS LETTER SUB-->")

(luna-define-method shimbun-groups ((shimbun shimbun-cnet-rss))
  (mapcar 'car shimbun-cnet-rss-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-cnet-rss))
  (cdr (assoc (shimbun-current-group shimbun) shimbun-cnet-rss-group-alist)))

(luna-define-method shimbun-clear-contents :before
  ((shimbun shimbun-cnet-rss) header)
  (goto-char (point-min))
  (while (search-forward "\r\n" nil t)
    (delete-region (match-beginning 0) (1+ (match-beginning 0))))
  (shimbun-remove-tags "<script" "</script>")
  (shimbun-remove-tags "<noscript" "</noscript>"))

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-cnet-rss) url date)
  (if (or
       ;; For news group
       (string-match "http://japan\\.cnet\\.com/\
\\(.+\\)/\\([,0-9]+\\)\\.htm\\?ref=rss" url)
       ;; For blog group
       (string-match "http://blog\\.japan\\.cnet\\.com/\
\\([^/]+\\)/archives/\\([0-9]+\\)\\.html" url))
      (concat "<" (match-string-no-properties 2 url) "%%"
	      (match-string-no-properties 1 url) "%%rss@japan.cnet.com>")
    (error "Cannot find message-id base")))

(provide 'sb-cnet-rss)

;;; sb-cnet-rss.el ends here

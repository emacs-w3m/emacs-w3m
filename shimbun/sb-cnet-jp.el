;;; sb-cnet-jp.el --- shimbun backend for CNET Japan

;; Copyright (C) 2003 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio     <minakaji@namazu.org>,
;;         TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Katsumi Yamaoka    <yamaoka@jpl.org>
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

(luna-define-class shimbun-cnet-jp (shimbun-japanese-newspaper shimbun-rss) ())

(defvar shimbun-cnet-jp-group-alist
  '(("news" . "http://japan.cnet.com/rss/index.rdf")
    ("blog.umeda" . "http://blog.japan.cnet.com/umeda/index.rdf")
    ("blog.lessig" . "http://blog.japan.cnet.com/lessig/index.rdf")
    ("blog.mori" . "http://blog.japan.cnet.com/mori/index.rdf")
    ("blog.kenn" . "http://blog.japan.cnet.com/kenn/index.rdf")))

(defvar shimbun-cnet-jp-server-name "CNET Japan")
(defvar shimbun-cnet-jp-from-address  "webmaster@japan.cnet.com")
(defvar shimbun-cnet-jp-content-start "<div class=\"leaf_body\">")
(defvar shimbun-cnet-jp-content-end "<!--NEWS LETTER SUB-->")
(defvar shimbun-cnet-jp-x-face-alist
  '(("default" . "X-Face: 0p7.+XId>z%:!$ahe?x%+AEm37Abvn]n\
*GGh+>v=;[3`a{1lqO[$,~3C3xU_ri>[JwJ!9l0\n ~Y`b*eXAQ:*q=bBI\
_=ro*?]4:|n>]ZiLZ2LEo^2nr('C<+`lO~/!R[lH'N'4X&%\\I}8T!wt")))

(luna-define-method shimbun-groups ((shimbun shimbun-cnet-jp))
  (mapcar 'car shimbun-cnet-jp-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-cnet-jp))
  (cdr (assoc (shimbun-current-group shimbun) shimbun-cnet-jp-group-alist)))

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-cnet-jp) url date)
  (if (or
       ;; For news group
       (string-match "http://japan\\.cnet\\.com/\
\\(.+\\)/\\([,0-9]+\\)\\.htm\\?ref=rss" url)
       ;; For blog group
       (string-match "http://blog\\.japan\\.cnet\\.com/\
\\([^/]+\\)/archives/\\([0-9]+\\)\\.html" url))
      (concat "<" (match-string-no-properties 2 url) "%"
	      (shimbun-current-group shimbun) "@japan.cnet.com>")
    (error "Cannot find message-id base")))

(luna-define-method shimbun-clear-contents :before
  ((shimbun shimbun-cnet-jp) header)
  (shimbun-strip-cr)
  (shimbun-remove-tags "<script" "</script>")
  (shimbun-remove-tags "<noscript" "</noscript>"))

(luna-define-method shimbun-footer :around ((shimbun shimbun-cnet-jp) header
					    &optional html)
  (if (string= "news" (shimbun-current-group shimbun))
      (luna-call-next-method)
    ""))

(provide 'sb-cnet-jp)

;;; sb-cnet-jp.el ends here

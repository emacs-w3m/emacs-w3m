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

(defvar shimbun-cnet-rss-url "http://japan.cnet.com/rss/index.rdf")
(defvar shimbun-cnet-rss-groups '("news"))
(defvar shimbun-cnet-rss-from-address  "webmaster@japan.cnet.com")
(defvar shimbun-cnet-rss-content-start "\n<!-- MAIN -->\n")
(defvar shimbun-cnet-rss-content-end "\n<!-- END MAIN -->\n")

(luna-define-method shimbun-index-url ((shimbun shimbun-cnet-rss))
  shimbun-cnet-rss-url)

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-cnet-rss) url date)
  (unless (string-match "http://japan.cnet.com/svc/rss\\?id=\\([.0-9]+\\)" url)
    (error "Cannot find message-id base"))
  (concat "<" (match-string-no-properties 1 url) "%%rss@japan.cnet.com>"))

(provide 'sb-cnet-rss)

;;; sb-cnet-rss.el ends here

;;; sb-slashdot-jp-rss.el --- shimbun backend for slashdot-jp-rss

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

(luna-define-class shimbun-slashdot-jp-rss (shimbun-rss) ())

(defvar shimbun-slashdot-jp-rss-url "http://slashdot.jp/slashdot.rdf")
(defvar shimbun-slashdot-jp-rss-groups '("story"))
(defvar shimbun-slashdot-jp-rss-from-address  "webmaster@slashdot.jp")
(defvar shimbun-slashdot-jp-rss-coding-system 'euc-japan)
(defvar shimbun-slashdot-jp-rss-content-start
  "\n<!-- start template: ID [0-9]+,.*dispStory.* -->\n")
(defvar shimbun-slashdot-jp-rss-content-end
  "\n<!-- end template: ID [0-9]+,.*dispStory.*-->\n")
  
(luna-define-method shimbun-index-url ((shimbun shimbun-slashdot-jp-rss))
  shimbun-slashdot-jp-rss-url)

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-slashdot-jp-rss) url)
  (unless (string-match
	   "http://slashdot.jp/article.pl\\?sid=[\/0-9]+\/\\([0-9]+\\)\\&topic=[0-9]+"
	   url)
    (error "Cannot find message-id base"))
  (concat (match-string-no-properties 1 url) "%%rss@slashdot.jp"))

(provide 'sb-slashdot-jp-rss)

;;; sb-slashdot-jp-rss.el ends here

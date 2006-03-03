;;; sb-yahoo-auctions.el --- shimbun backend for ITmedia +D

;; Copyright (C) 2006  ARISAWA Akihiro <ari@mbf.sphere.ne.jp>

;; Author: ARISAWA Akihiro <ari@mbf.sphere.ne.jp>
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

;;; Code:

(require 'shimbun)
(require 'sb-rss)
(require 'sb-itmedia)

(luna-define-class shimbun-itmedia+D (shimbun-rss) ())

(defvar shimbun-itmedia+D-groups
  '("plusd" "mobile" "pcupdate" "lifestyle" "games" "docomo" "au_kddi"
    "vodafon"))
(defvar shimbun-itmedia+D-x-face-alist
  '(("default" . "X-Face: #Ur~tK`JhZFFHPEVGKEi`MS{55^~&^0KUuZ;]-@WQ[8\
@,Ex'EeAAE}6xF<K#]pULF@5r24J
 |8/oP)(lCAzF0}.C@@}!k8!Qiz6b{]V")))

(luna-define-method shimbun-from-address ((shimbun shimbun-itmedia+D))
  (format "ITmedia +D (%s)" (shimbun-current-group shimbun)))

(luna-define-method shimbun-index-url ((shimbun shimbun-itmedia+D))
  (format "http://rss.rssad.jp/rss/itm/2.0/%s.xml"
	  (shimbun-current-group shimbun)))

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-itmedia+D) url date)
  (unless (string-match
	   "\\([0-9][0-9][0-1][0-9]/[0-3][0-9]/news[0-9]+\\).html$" url)
    (error "Cannot find message-id base"))
  (format "<%s@plusd.itmedia.co.jp>" (match-string 1 url)))

(luna-define-method shimbun-make-contents ((shimbun shimbun-itmedia+D) header)
  (shimbun-itmedia-make-contents shimbun header))

(provide 'sb-itmedia+D)

;;; sb-itmedia+D.el ends here

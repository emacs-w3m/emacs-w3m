;;; sb-laut-de.el --- shimbun backend for <http://www.laut.de/>

;; Copyright (C) 2004 Andreas Seltenreich <seltenreich@gmx.de>

;; Author: Andreas Seltenreich <seltenreich@gmx.de>
;; Keywords: news
;; Created: May 23, 2004

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

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-laut-de (shimbun-rss) ())

(defvar shimbun-laut-de-groups
  '("platten"
    "news"
    "platten_rock"
    "platten_pop"
    "platten_alternative"
    "platten_metal"
    "platten_rnb"
    "platten_dance"
    "platten_jazz"
    "platten_hiphop"))

(defvar shimbun-laut-de-content-start
  "<!-- headline -->\\|<span class=\"ueberschriftnormalgrau[^>]*>")

(defvar shimbun-laut-de-content-end
  (concat "<!-- /box weitere Links -->\\|"
	  "<!-- commercialflaeche -->\\|"
	  "<!-- link zu lautbar -->"))

(defvar shimbun-laut-de-from-address "redaktion@laut.de")

(luna-define-method shimbun-headers :before ((shimbun shimbun-laut-de)
					     &rest range)
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-laut-de))
  shimbun-laut-de-groups)

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-laut-de) url date)
  (unless (string-match "laut.de/\\(.*\\)/" url)
    (error "Cannot find message-id base"))
  (format "<%s@sb-laut-de.invalid>" 
	  (shimbun-replace-in-string
	   (match-string-no-properties 1 url)
	   "[^a-zA-Z0-9]" "%")))

(luna-define-method shimbun-index-url ((shimbun shimbun-laut-de))
  (concat "http://www.laut.de/partner/allgemein/"
	  (shimbun-current-group-internal shimbun) ".rdf"))

(provide 'sb-laut-de)

;;; sb-laut-de.el ends here

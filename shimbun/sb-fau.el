;;; sb-fau.el --- Freie ArbeiterInnen Union shimbun backend

;; Copyright (C) 2005 David Hansen

;; Author: David Hansen <david.hansen@physik.fu-berlin.de>
;; Keywords: news

;; This file is a part of shimbun.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-fau (shimbun-rss) ())

(defvar shimbun-fau-url
  "http://www.fau.org/faurss.rdf")
(defvar shimbun-fau-groups '("news"))
(defvar shimbun-fau-from-address  "www@fau.org")
(defvar shimbun-fau-x-face-alist
  '(("default" .
     "X-Face: 5Nxj%|<|TGh94(OzpKK*3XE{c=jFM9F7Mngs2BJva%Cs2jH>DThKw\
[h'n|GAMyNEJ~*y%59xj5d31q4(xg_<tOrJ_\"NaXys5yh~3P_u\\]VOC!3:N+O,>=>O+w}\
<l-KjY3.MV#@*}26P^'2$E_8<6'}rz:0,6K")))

(defvar shimbun-fau-content-start "<hr\\s-+size=\"1\">")
(defvar shimbun-fau-content-end "<hr\\s-+size=\"1\">\\s-*
<p>\\s-*
..*?<a\\s-+class=\"red\"\\s-+href=\"javascript:window.print()\"")

(luna-define-method shimbun-index-url ((shimbun shimbun-fau))
  shimbun-fau-url)

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-fau)
						 &optional range)
  (mapcar
   (lambda (header)
     (shimbun-header-set-xref
      header (concat (shimbun-header-xref header) "/print.html"))
     header)
   (luna-call-next-method)))

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-fau) url date)
  (unless (string-match "/[^/]*$" url)
    (error "Cannot find message-id base"))
  (concat "<" (match-string 0 url) "@fau.de>"))

(provide 'sb-fau)

;;; sb-fau.el ends here

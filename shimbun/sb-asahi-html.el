;;; sb-asahi-html.el --- shimbun backend for asahi.com (HTML version)

;; Author: Yuuichi Teranishi <teranisi@gohome.org>

;; Keywords: news

;;; Copyright:

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
(require 'sb-asahi)

(luna-define-class shimbun-asahi-html (shimbun shimbun-asahi) ())

(defvar shimbun-asahi-html-url "http://spin.asahi.com/")
(defvar shimbun-asahi-html-groups '("national" "business" "politics"
				    "international" "sports"))
(defvar shimbun-asahi-html-from-address "webmaster@www.asahi.com")

(defvar shimbun-asahi-html-content-start "\n<!-- Start of photo1 -->\n")
(defvar shimbun-asahi-html-content-end "\n<!-- End of kiji -->\n")
(defvar shimbun-asahi-html-x-face-alist
  '(("default" .
     "X-Face:  +Oh!C!EFfmR$+Zw{dwWW]1e_>S0rnNCA*CX|bIy3rr^<Q#lf&~ADU:X!t5t>
        gW5)Q]N{MmnL]suPpL|gFjV{S|]a-:)\\FR7GRf9uL:ue5_=;h{V%@()={u
        Td@l?eXBppF%`6W%;h`#]2q+f*81n$Bh|t")))

(defvar shimbun-asahi-html-expiration-days 6)

(provide 'sb-asahi-html)

;;; sb-asahi-html.el ends here

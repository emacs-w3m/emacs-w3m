;;; sb-asahi-html.el --- shimbun backend for asahi.com (HTML version)

;; Copyright (C) 2001, 2002, 2003, 2004, 2005
;; Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi  <teranisi@gohome.org>
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

;;; Code:

(require 'shimbun)
(require 'sb-asahi)

(luna-define-class shimbun-asahi-html (shimbun-asahi) ())

(defvar shimbun-asahi-html-content-start
  "<!--[\t\n ]*Start of \\(Kiji\\|photo\\)[\t\n ]*-->\
\\|<!--[\t\n ]*FJZONE START NAME=\"HONBUN\"[\t\n ]*-->")

(defvar shimbun-asahi-html-content-end
  "<!--[\t\n ]*End of Kiji[\t\n ]*-->\
\\|<!--[\t\n ]*End of related link[\t\n ]*-->\
\\|<!--[\t\n ]*FJZONE END NAME=\"HONBUN\"[\t\n ]*-->")

(luna-define-method shimbun-make-contents ((shimbun shimbun-asahi-html)
					   header)
  (shimbun-asahi-prepare-article shimbun header)
  (shimbun-make-html-contents shimbun header))

(provide 'sb-asahi-html)

;;; sb-asahi-html.el ends here

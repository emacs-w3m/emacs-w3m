;;; sb-asahi-html.el --- shimbun backend for asahi.com (HTML version)

;; Copyright (C) 2001, 2002, 2003 Yuuichi Teranishi  <teranisi@gohome.org>

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

(luna-define-class shimbun-asahi-html (shimbun) ())

(defvar shimbun-asahi-html-content-start
  "<!--[\t\n ]*Start of photo[\t\n ]*-->\
\\|<!--[\t\n ]*FJZONE START NAME=\"HONBUN\"[\t\n ]*-->")
(defvar shimbun-asahi-html-content-end
  "<!--[\t\n ]*End of related link[\t\n ]*-->\
\\|<!--[\t\n ]*FJZONE END NAME=\"HONBUN\"[\t\n ]*-->")
(defvar shimbun-asahi-html-x-face-alist shimbun-asahi-x-face-alist)

(defvar shimbun-asahi-html-expiration-days shimbun-asahi-expiration-days)

(luna-define-method shimbun-groups ((shimbun shimbun-asahi-html))
  (mapcar 'car shimbun-asahi-group-table))

(luna-define-method shimbun-from-address ((shimbun shimbun-asahi-html))
  (concat (shimbun-mime-encode-string
	   (concat "朝日新聞 ("
		   (nth 1 (assoc (shimbun-current-group-internal shimbun)
				 shimbun-asahi-group-table))
		   ")"))
	  " <webmaster@www." shimbun-asahi-top-level-domain ">"))

(luna-define-method shimbun-index-url ((shimbun shimbun-asahi-html))
  (let ((group (shimbun-current-group-internal shimbun)))
    (concat shimbun-asahi-url group "/"
	    (nth 2 (assoc group shimbun-asahi-group-table)))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-asahi-html)
					 &optional range)
  (shimbun-asahi-get-headers shimbun))

(luna-define-method shimbun-make-contents
  :before ((shimbun shimbun-asahi-html) header)
  (shimbun-asahi-adjust-date-header shimbun header))

(provide 'sb-asahi-html)

;;; sb-asahi-html.el ends here

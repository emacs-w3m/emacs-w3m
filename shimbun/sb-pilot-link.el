;;; sb-pilot-link.el --- shimbun backend for pilot-link

;; Copyright (C) 2002 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
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
(require 'sb-mailman)

(luna-define-class shimbun-pilot-link (shimbun-mailman) ())

(defvar shimbun-pilot-link-url "http://www.pilot-link.org/pipermail")

(defconst shimbun-pilot-link-group-path-alist
  '(("announce" . "pilot-link-announce")
    ("devel" . "pilot-link-devel")
    ("general" . "pilot-link-general")
    ("unix-ng" . "pilot-unix-ng")))

(defvar shimbun-pilot-link-groups
  (mapcar 'car shimbun-pilot-link-group-path-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-mailman))
  (concat (shimbun-url-internal shimbun) "/" 
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-pilot-link-group-path-alist))))

;;(luna-define-method shimbun-reply-to ((shimbun shimbun-pilot-link))
;;  "")

(provide 'sb-pilot-link)
;;; sb-pilot-link.el ends here

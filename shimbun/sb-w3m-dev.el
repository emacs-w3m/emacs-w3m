;;; sb-w3m-dev.el --- shimbun backend for w3m-dev

;; Copyright (C) 2001 NAKAJIMA Mikio
;; Copyright (C) 2005 Tsuyoshi CHO

;; Authors: NAKAJIMA Mikio <minakaji@namazu.org>,
;;          Tsuyoshi CHO <mfalcon_sky@emailuser.net>
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
(require 'sb-fml)

(luna-define-class shimbun-w3m-dev (shimbun-fml) ())

(defvar shimbun-w3m-dev-url "http://www.sic.med.tohoku.ac.jp/~satodai/")
(defvar shimbun-w3m-dev-group-alist
  '(("w3m-dev"    . "w3m-dev")
    ("w3m-dev-en" . "w3m-dev-en")))

(defvar shimbun-w3m-dev-groups
  (mapcar 'car shimbun-w3m-dev-group-alist))

(defvar shimbun-w3m-dev-coding-system 'euc-japan)

(luna-define-method shimbun-index-url ((shimbun shimbun-w3m-dev))
  (concat
   (shimbun-expand-url
    (cdr (assoc (shimbun-current-group-internal shimbun)
		shimbun-w3m-dev-group-alist))
    (shimbun-url-internal shimbun))
   "/"))

(luna-define-method shimbun-reply-to ((shimbun shimbun-w3m-dev))
  "Return w3m-dev mailing list address."
  (concat
   (cdr (assoc (shimbun-current-group-internal shimbun)
	       shimbun-w3m-dev-group-alist))
   (shimbun-url-internal shimbun)
   "@mi.med.tohoku.ac.jp"))

(provide 'sb-w3m-dev)

;;; sb-w3m-dev.el ends here

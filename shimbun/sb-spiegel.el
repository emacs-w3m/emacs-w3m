;;; sb-spiegel.el --- spiegel online shimbun backend

;; Copyright (C) 2004 David Hansen

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

(luna-define-class shimbun-spiegel (shimbun-rss) ())

(defvar shimbun-spiegel-url
  "http://www.spiegel.de/schlagzeilen/rss/0,5291,,00.xml")
(defvar shimbun-spiegel-groups '("news"))
(defvar shimbun-spiegel-from-address  "spiegel_online@spiegel.de")
(defvar shimbun-spiegel-content-start "<p>")
(defvar shimbun-spiegel-content-end
  "\\(<hr size=\"1\" noshade>\\|<div align=right>\\)")

(luna-define-method shimbun-index-url ((shimbun shimbun-spiegel))
  shimbun-spiegel-url)

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-spiegel)
						 &optional range)
  (mapcar
   (lambda (header)
     (let ((url (shimbun-header-xref header)))
       (when (string-match "\\([0-9]+\\),[0-9]+\\.html" url)
	 (shimbun-header-set-xref header
				  (replace-match "druck-\\1" t nil url 1))))
     header)
   (luna-call-next-method)))

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-spiegel) url date)
  (unless (string-match "[0-9,]+\\.html" url)
    (error "Cannot find message-id base"))
  (concat "<"
	  (shimbun-replace-in-string (match-string 0 url)
				     "\\(,\\|druck-\\|\\.html\\)"
				     "")
	  "@spiegel.de>"))

(provide 'sb-spiegel)

;;; sb-spiegel.el ends here

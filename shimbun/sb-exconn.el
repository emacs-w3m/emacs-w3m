;;; sb-exconn.el --- shimbun backend for eXperts Connection

;; Copyright (C) 2004, 2005 Yoichi NAKAYAMA <yoichi@geiin.org>

;; Author: Yoichi NAKAYAMA <yoichi@geiin.org>
;; Keywords: news
;; Created: Jun 27, 2004

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

(eval-when-compile
  (require 'cl))

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-exconn (shimbun-rss) ())

(defvar shimbun-exconn-url
  "http://www.exconn.net/NewestKB.asmx/GetNewestKB?span=w&mnemonic=all")
(defvar shimbun-exconn-groups '("news"))
(defvar shimbun-exconn-from-address "nobody@exconn.net")
(defvar shimbun-exconn-content-start
  "<!-- - -KB 3 start- - -->")
(defvar shimbun-exconn-content-end
  "<!-- - -KB 3 end- - -->")

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-exconn) url date)
  (unless (string-match "\
http://support.microsoft.com/default.aspx\\?scid=kb;ja;\\([0-9]+\\)" url)
    (error "Cannot find message-id base"))
  (concat "<" (match-string-no-properties 1 url) "@support.microsoft.com>"))

(luna-define-method shimbun-rss-process-date ((shimbun shimbun-exconn) date)
  (cond ((null date)
	 "")
	((string-match "\\(.*\\) GMT" date)
	 (concat (match-string-no-properties 1 date) " +0000"))
	(t
	 date)))

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-exconn)
						&optional range)
  (let ((headers (luna-call-next-method)))
    (dolist (header headers)
      (shimbun-header-set-from header (shimbun-from-address shimbun)))
    headers))

(provide 'sb-exconn)

;;; sb-exconn.el ends here

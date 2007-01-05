;;; sb-weeklyworldnews.el --- weekly world news shimbun backend

;; Copyright (C) 2004, 2005, 2007 David Hansen

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-weeklyworldnews (shimbun-rss) ())

(defconst shimbun-weeklyworldnews-url "http://www.weeklyworldnews.com/rss")
(defconst shimbun-weeklyworldnews-groups '("news"))
(defconst shimbun-weeklyworldnews-content-start "<h1>")
(defconst shimbun-weeklyworldnews-content-end "<a href=\"/\"")
;; (defconst shimbun-weeklyworldnews-from "Weekly World News")

(luna-define-method shimbun-index-url ((shimbun shimbun-weeklyworldnews))
  shimbun-weeklyworldnews-url)

(luna-define-method shimbun-get-headers
  :around ((shimbun shimbun-weeklyworldnews) &optional range)
  (mapcar
   (lambda (header)
     (let ((url (shimbun-header-xref header)))
       (when (string-match "/\\(stories\\)/" url)
	 (shimbun-header-set-xref
          header (replace-match "printstory" t nil url 1))))
     header)
   (luna-call-next-method)))

(provide 'sb-weeklyworldnews)

;;; sb-weeklyworldnews.el ends here

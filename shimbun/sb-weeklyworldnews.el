;;; sb-weeklyworldnews.el --- weekly world news shimbun backend

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

(luna-define-class shimbun-weeklyworldnews (shimbun) ())

(defvar shimbun-weeklyworldnews-url
  "http://www.weeklyworldnews.com/news/")

(defvar shimbun-weeklyworldnews-base-url
  "http://www.weeklyworldnews.com")

(defvar shimbun-weeklyworldnews-groups '("news"))

(defvar shimbun-weeklyworldnews-content-start
  "<table[^>]*bgcolor=\"#ffffff\">")

(defvar shimbun-weeklyworldnews-content-end "<\/body>")


(defconst shimbun-weeklyworldnews-index-re
  (concat
   "<a href=\"/\\(.*?\\)/\\([0-9]+\\)\">"         ; link
   "<span class=\"headsm\">\\(.+?\\)</span></a>") ; headline

  "Regexp to match a Weekly World News article on the summary page.")


(defconst shimbun-weeklyworldnews-from
  "Weekly World News <invalid@weeklyworldnews.com>"

  "From: header for the Weekly World News shimbun")


(luna-define-method shimbun-get-headers
  ((shimbun shimbun-weeklyworldnews) &optional range)
  (let ((date "") (id) (url) (subject) (headers))
    (catch 'stop
      (while (re-search-forward shimbun-weeklyworldnews-index-re nil t nil)
	(setq url (concat shimbun-weeklyworldnews-base-url
			  "/" (match-string 1) "/"
			  (match-string 2) "?printer=1"))
	(setq id (concat
		  "<"
		  (shimbun-replace-in-string (match-string 1) "/" "-")
		  "-" (match-string 2)
		  "@weeklyworldnews.com>"))
	(setq subject (match-string 3))
	(when (shimbun-search-id shimbun id)
	  (throw 'stop nil))
	(message id)
	(push (shimbun-make-header
	       0 (shimbun-mime-encode-string subject)
	       (shimbun-mime-encode-string shimbun-weeklyworldnews-from)
	       date id "" 0 0 url)
	      headers)))
    headers))


(provide 'sb-weeklyworldnews)

;;; sb-weeklyworldnews.el ends here

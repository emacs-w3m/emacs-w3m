;;; sb-h14m.el --- shimbun backend for h14m.org

;; Copyright (C) 2001 Akihiro Arisawa <ari@mbf.sphere.ne.jp>

;; Author: Akihiro Arisawa <ari@mbf.sphere.ne.jp>
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

;;; Code:

(require 'shimbun)
(require 'sb-mhonarc)

(luna-define-class shimbun-h14m (shimbun-mhonarc) ())

(defvar shimbun-h14m-url "http://www.h14m.org/ml/")
(defvar shimbun-h14m-groups '("hns-dev" "hns-users"))
(defvar shimbun-h14m-reverse-flag nil)
(defvar shimbun-h14m-litemplate-regexp
  "<STRONG><A NAME=\"\\([0-9]+\\)\" HREF=\"\\(msg[0-9]+.html\\)\">\\([^<]+\\)</A></STRONG>\n<UL><LI><EM>From</EM>: \\([^<]+\\)</LI></UL>")

(luna-define-method shimbun-index-url ((shimbun shimbun-h14m))
  (concat (shimbun-url-internal shimbun)
	  (shimbun-current-group-internal shimbun) "/maillist.cgi"))

(luna-define-method shimbun-reply-to ((shimbun shimbun-h14m))
  (concat (shimbun-current-group-internal shimbun) "@h14m.org"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-h14m)
					 &optional range)
  (let ((url (shimbun-index-url shimbun))
	(pages (shimbun-header-index-pages range))
	(count 0)
	headers)
    (catch 'stop
      (shimbun-mhonarc-get-headers shimbun url headers)
      (goto-char (point-min))
      (while (and (if pages (< (incf count) pages) t)
		  (re-search-forward
		   "<A HREF=\"\\(mail[0-9]+\\.html\\)\">Prev Page</A>"
		   nil t))
	(setq url (shimbun-expand-url (match-string 1) url))
	(erase-buffer)
	(shimbun-retrieve-url url)
	(shimbun-mhonarc-get-headers shimbun url headers)
	(goto-char (point-min)))
      headers)))

(provide 'sb-h14m)

;;; sb-h14m.el ends here

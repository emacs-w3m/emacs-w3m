;;; sb-namazu.el --- shimbun backend for namazu.org
;;
;; Copyright (C) 2001 Akihiro Arisawa   <ari@mbf.sphere.ne.jp>

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

(luna-define-class shimbun-namazu (shimbun-mhonarc) ())

(defvar shimbun-namazu-url "http://www.namazu.org/")

(defvar shimbun-namazu-group-url-alist
  '(("namazu-users-ja" . "ml/namazu-users-ja")
    ("namazu-win32-users-ja" . "ml/namazu-win32-users-ja")
    ("namazu-users-en" . "ml/namazu-users-en")
    ("namazu-devel-ja" . "ml/namazu-devel-ja")
    ("namazu-devel-en" . "ml/namazu-devel-en")
    ("emacs-w3m" . "http://emacs-w3m.namazu.org/ml")))

(defvar shimbun-namazu-groups (mapcar 'car shimbun-namazu-group-url-alist))
(defvar shimbun-namazu-use-entire-index nil)
(defvar shimbun-namazu-reverse-flag t)
(defvar shimbun-namazu-litemplate-regexp
  "<Strong><A NAME=\"\\([0-9]+\\)\" HREF=\"\\(msg[0-9]+.html\\)\"> \\([^<]+\\)</a></Strong> <EM>\\([^<]+\\)</EM>")

(luna-define-method shimbun-index-url ((shimbun shimbun-namazu))
  (concat
   (shimbun-expand-url
    (cdr (assoc (shimbun-current-group-internal shimbun)
		shimbun-namazu-group-url-alist))
    (shimbun-url-internal shimbun))
   "/"))

(luna-define-method shimbun-reply-to ((shimbun shimbun-namazu))
  (concat (shimbun-current-group-internal shimbun)
	  "@namazu.org"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-namazu)
					 &optional range)
  (let ((url (shimbun-index-url shimbun))
	(pages (shimbun-header-index-pages range))
	(count 0)
	headers aux)
    (catch 'stop
      (shimbun-mhonarc-get-headers shimbun url headers)
      (while (and (if pages (< (incf count) pages) t)
		  (re-search-forward
		   "<A href=\"\\(mail[0-9]+.html\\)\">Next Index</A>"
		   nil t)
		  (not (string-equal (match-string 1) aux)))
	(setq aux (match-string 1)
	      url (shimbun-expand-url aux url))
	(erase-buffer)
	(shimbun-retrieve-url url)
	(shimbun-mhonarc-get-headers shimbun url headers))
      headers)))

(provide 'sb-namazu)
;;; sb-namazu.el ends here

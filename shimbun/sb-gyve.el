;;; sb-gyve.el --- shimbun backend for www.gyve.org
;;
;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
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

(luna-define-class shimbun-gyve (shimbun-mhonarc) ())

(defvar shimbun-gyve-url "http://www.gyve.org/gs-cjk/archive/")
(defvar shimbun-gyve-groups '("gs-cjk"))

(luna-define-method shimbun-reply-to ((shimbun shimbun-gyve))
  "gs-cjk@gyve.org")

(luna-define-method shimbun-headers ((shimbun shimbun-gyve)
				     &optional range)
  (let ((case-fold-search t)
	(pages (shimbun-header-index-pages range))
	(i 0)
	url indexes headers)
    (catch 'stop
      (while (incf i)
	(if (shimbun-url-exists-p
	     (setq url (shimbun-expand-url
			(if (= i 1)
			    "maillist.html"
			  (format "mail%d.html" i))
			shimbun-gyve-url)))
	    (push url indexes)
	  (throw 'stop nil))))
    (setq i 0)
    (catch 'stop
      (dolist (index indexes)
	(with-temp-buffer
	  (shimbun-retrieve-url index (= 1 (incf i)))
	  (shimbun-mhonarc-get-headers shimbun index headers nil)
	  (and pages
	       (> i pages)
	       (throw 'stop headers))))
      headers)))

(provide 'sb-gyve)

;;; sb-gyve.el ends here


;;; sb-kde.el --- shimbun backend for www.KDE.gr.jp

;; Copyright (C) 2002 NAKAJIMA Mikio  <minakaji@osaka.email.ne.jp>

;; Authors: NAKAJIMA Mikio  <minakaji@osaka.email.ne.jp>
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
(require 'sb-mhonarc)

(luna-define-class shimbun-kde (shimbun-mhonarc) ())

(defvar shimbun-kde-url "http://www.KDE.gr.jp/ml/")
(defvar shimbun-kde-groups '("Kdeveloper" "Kuser"))
(defvar shimbun-kde-coding-system 'euc-jp)
(defvar shimbun-kde-reverse-flag t)
(defvar shimbun-kde-litemplate-regexp
  "<STRONG><A NAME=\"\\([0-9]+\\)\" href=\"\\(msg[0-9]+.html\\)\">\\([^<]+\\)</A></STRONG> <EM>\\([^<]+\\)</EM>")

(luna-define-method shimbun-index-url ((shimbun shimbun-kde))
  (concat (shimbun-url-internal shimbun)
	  (shimbun-current-group-internal shimbun) "/"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-kde)
					 &optional range)
  (let ((case-fold-search t)
	(pages (shimbun-header-index-pages range))
	(count 0)
	(months '("index.html"))
	headers)
    (shimbun-mhonarc-reverse-flag-internal shimbun)
    (goto-char (point-min))
    (when pages (incf count))
    (while (and (if pages (<= (incf count) pages) t)
		(re-search-forward "\\[Prev Page\\]\\[<a href=\"\\(.+\\.html\\)\">Next Page</a>\\]" nil t)
		(push (match-string 1) months)))
    (setq months (nreverse months))
    (erase-buffer)
    (catch 'stop
      (dolist (month months)
        (let ((url (concat (shimbun-index-url shimbun) month)))
	  (shimbun-retrieve-url url t)
	  (shimbun-mhonarc-get-headers shimbun url headers month))))
    headers))

(provide 'sb-kde)

;;; sb-kde.el ends here

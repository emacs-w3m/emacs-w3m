;;; sb-tdiary.el --- shimbun backend for www.tDiary.org

;; Copyright (C) 2003 Koichiro Ohba  <koichiro@meadowy.org>

;; Author: Koichiro Ohba  <koichiro@meadowy.org>
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

;; Original code was sb-airs.el which is written by
;; Yuuichi Teranishi <teranisi@gohome.org>

;;; Code:

(require 'shimbun)
(require 'sb-mhonarc)

(luna-define-class shimbun-tdiary (shimbun-mhonarc) ())

(defconst shimbun-tdiary-group-path-alist
  '(("devel" "archive/devel" "tDiary-devel@lists.sourceforge.net")
    ("theme" "archive/theme" "tDiary-theme@lists.sourceforge.net")))

(defvar shimbun-tdiary-url "http://www.tdiary.org/")
(defvar shimbun-tdiary-groups (mapcar 'car shimbun-tdiary-group-path-alist))
(defvar shimbun-tdiary-reverse-flag nil)
(defvar shimbun-tdiary-litemplate-regexp
  "<strong><a name=\"\\([0-9]+\\)\" href=\"\\(msg[0-9]+.html\\)\">\\([^<]+\\)</a></strong>,\n      <em>\\([^<]+\\)</em>")

(defmacro shimbun-tdiary-concat-url (shimbun url)
  (` (concat (shimbun-url-internal (, shimbun))
	     (nth 1 (assoc (shimbun-current-group-internal (, shimbun))
			   shimbun-tdiary-group-path-alist))
	     "/"
	     (, url))))

(luna-define-method shimbun-index-url ((shimbun shimbun-tdiary))
  (shimbun-tdiary-concat-url shimbun "index.html"))

(luna-define-method shimbun-reply-to ((shimbun shimbun-tdiary))
  (nth 2 (assoc (shimbun-current-group-internal shimbun)
		shimbun-tdiary-group-path-alist)))

(provide 'sb-tdiary)

;;; sb-tdiary.el ends here

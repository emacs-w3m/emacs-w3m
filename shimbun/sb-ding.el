;;; sb-ding.el --- shimbun backend for gnus.org

;; Author: Akihiro Arisawa <ari@mbf.sphere.ne.jp>

;; Keywords: news

;;; Copyright:

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
(require 'sb-glimpse)

(luna-define-class shimbun-ding (shimbun-glimpse) ())

(defvar shimbun-ding-url "http://www.gnus.org/list-archives/")
(defvar shimbun-ding-groups '("ding"))
(defvar shimbun-ding-coding-system 'iso-8859-1)
(defvar shimbun-ding-reverse-flag nil)
(defvar shimbun-ding-litemplate-regexp
  "<td><strong><a name=\"\\([0-9]+\\)\" href=\"\\(msg[0-9]+.html\\)\">\\([^<]+\\)</a></strong>\n<td><em>\\([^<]+\\)</em>")

(provide 'sb-ding)

;;; sb-ding.el ends here

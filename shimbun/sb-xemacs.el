;;; sb-xemacs.el --- shimbun backend for xemacs.org

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Akihiro Arisawa    <ari@mbf.sphere.ne.jp>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>

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

;;; Commentary:

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(require 'shimbun)
(require 'sb-glimpse)

(luna-define-class shimbun-xemacs (shimbun-glimpse) ())

(defvar shimbun-xemacs-url "http://list-archive.xemacs.org/")
(defvar shimbun-xemacs-groups '("xemacs-announce"
				"xemacs-beta-ja" "xemacs-beta"
				"xemacs-build-reports" "xemacs-cvs"
				"xemacs-mule" "xemacs-nt" "xemacs-patches"
				"xemacs-users-ja" "xemacs"))
(defvar shimbun-xemacs-coding-system 'euc-jp)
(defvar shimbun-xemacs-reverse-flag nil)
(defvar shimbun-xemacs-litemplate-regexp
  "<td><strong><a name=\"\\([0-9]+\\)\" href=\"\\(msg[0-9]+.html\\)\">\\([^<]+\\)</a></strong>\n<td><em>\\([^<]+\\)</em>")

(provide 'sb-xemacs)

;;; sb-xemacs.el ends here

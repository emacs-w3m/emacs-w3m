;;; sb-linuxce-jp.el --- shimbun backend for linuxce-jp ML

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

;;; Commentary:

;; This is shimbun backend for linuxce-jp ML.

;;; Code:

(require 'shimbun)
(require 'sb-fml)

(luna-define-class shimbun-linuxce-jp (shimbun-fml) ())

(defvar shimbun-linuxce-jp-url "http://www.peanuts.gr.jp/~kei/ml-archive/")
(defvar shimbun-linuxce-jp-groups '("users"))

(provide 'sb-linuxce-jp)

;;; sb-linuxce-jp.el ends here

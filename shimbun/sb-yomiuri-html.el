;;; sb-yomiuri-html.el --- shimbun backend for yomiuri online (HTML version)

;; Copyright (C) 2001, 2002, 2003 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;;         Katsumi Yamaoka   <yamaoka@jpl.org>
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
(require 'sb-yomiuri)

(luna-define-class shimbun-yomiuri-html (shimbun shimbun-yomiuri) ())

(defvar shimbun-yomiuri-html-url shimbun-yomiuri-url)
(defvar shimbun-yomiuri-html-groups shimbun-yomiuri-groups)
(defvar shimbun-yomiuri-html-from-address shimbun-yomiuri-from-address)
(defvar shimbun-yomiuri-html-content-start
  "\n<!-- ▼写真テーブル▼ -->\n\\|\n<!--  honbun start  -->\n")
(defvar shimbun-yomiuri-html-content-end
  "\n<!--  honbun end  -->\n")

(defvar shimbun-yomiuri-html-x-face-alist shimbun-yomiuri-x-face-alist)

(defvar shimbun-yomiuri-html-expiration-days shimbun-yomiuri-expiration-days)

(provide 'sb-yomiuri-html)

;;; sb-yomiuri-html.el ends here

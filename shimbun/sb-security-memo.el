;;; sb-security-memo.el --- shimbun backend for security-memo ML.

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>

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

;;; Code:

(require 'shimbun)
(require 'sb-w3m-dev)

(luna-define-class shimbun-security-memo (shimbun-w3m-dev) ())

(defvar shimbun-security-memo-url "http://memo.st.ryukoku.ac.jp/archive/")
(defvar shimbun-security-memo-groups '("memo"))
(defvar shimbun-security-memo-coding-system 'euc-japan)

(luna-define-method shimbun-reply-to ((shimbun shimbun-security-memo))
  "memo@memo.st.ryukoku.ac.jp")

(provide 'sb-security-memo)

;;; sb-security-memo.el ends here

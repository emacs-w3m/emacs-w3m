;;; sb-kyoko-np.el --- shimbun backend for Kyoko Shimbun News -*- coding: utf-8; -*-

;; Copyright (C) 2015 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-kyoko-np (shimbun-rss) ())

(defvar shimbun-kyoko-np-url "http://kyoko-np.net/")
(defvar shimbun-kyoko-np-server-name "虚構新聞社")

(defvar shimbun-kyoko-np-x-face-alist
  '(("default" . "\
X-Face: $5)ftJ]QIPSQ!1hvb}7KtjG$iAuk>KI.2ajuPB*&}act=uLSq#1kf\\|Y@8Zf\
h,:y~(ZRL6_\n !]+_+:*w'FH/kkX~]Wd>*Og6Q:)\"M&Kngqb%I\"V-k_@?Y5r5ESY8k>")))

(defvar shimbun-kyoko-np-groups '("rss"))

(luna-define-method shimbun-index-url ((shimbun shimbun-kyoko-np))
  (concat shimbun-kyoko-np-url "index.xml"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-kyoko-np)
					 &optional range)
  (let ((fn (symbol-function 'shimbun-rss-node-text)))
    (fset 'shimbun-rss-node-text
	  (lambda (namespace local-name element)
	    (if (eq local-name 'author)
		"虚構新聞社社主 ＵＫ"
	      (funcall fn namespace local-name element))))
    (unwind-protect
	(shimbun-rss-get-headers shimbun range t)
      (fset 'shimbun-rss-node-text fn))))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-kyoko-np)
						    header)
  (when (and (search-forward "<article>" nil t)
	     (shimbun-end-of-tag "article" t))
    (delete-region (match-end 3) (goto-char (point-max)))
    (insert "\n")
    (delete-region (point-min) (match-beginning 3))
    t))

(luna-define-method shimbun-footer :around ((shimbun shimbun-kyoko-np)
					    header &optional html)
  (concat "<div align=\"left\">\n--&nbsp;<br>\n\
この記事の諸権利は虚構新聞社に帰属します。原物は<a href=\""
	  (shimbun-article-base-url shimbun header)
	  "\"><u>ここ</u></a>で公開されています。\n</div>\n"))

(provide 'sb-kyoko-np)

;;; sb-kyoko-np.el ends here

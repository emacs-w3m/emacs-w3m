;;; sb-atmarkit-rss.el --- shimbun backend for atmarkit-rss

;; Copyright (C) 2003 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Keywords: news
;; Created: Jun 15, 2003

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
(require 'sb-rss)

(luna-define-class shimbun-atmarkit-rss (shimbun-rss) ())

(defvar shimbun-atmarkit-rss-from-address  "info@atmarkit.co.jp")
(defvar shimbun-atmarkit-rss-coding-system 'shift_jis-dos)
(defvar shimbun-atmarkit-rss-content-start
  "<!-- #BeginEditable \"%[%A-Z0-9]+\" -->")
(defvar shimbun-atmarkit-rss-content-end
  "\n<!-- #EndEditable -->")

(defvar shimbun-atmarkit-rss-group-path-alist
  '(
    ;;Windows Server Insiderフォーラム
    ("fwin2k" . "http://www.atmarkit.co.jp/rss/fwin2k/rss2dc.xml")
    ;;Insider.NETフォーラム
    ("fdotnet" . "http://www.atmarkit.co.jp/rss/fdotnet/rss2dc.xml")
    ;;System Insiderフォーラム
    ("fsys" . "http://www.atmarkit.co.jp/rss/fsys/rss2dc.xml")
    ;;XML &amp; Web Servicesフォーラム
    ("fxml" . "http://www.atmarkit.co.jp/rss/fxml/rss2dc.xml")
    ;;Linux Squareフォーラム
    ("flinux" . "http://www.atmarkit.co.jp/rss/flinux/rss2dc.xml")
    ;;Master of IP Networkフォーラム
    ("fnetwork" . "http://www.atmarkit.co.jp/rss/fnetwork/rss2dc.xml")
    ;;Java Solutionフォーラム
    ("fjava" . "http://www.atmarkit.co.jp/rss/fjava/rss2dc.xml")
    ;;Security&amp;Trustフォーラム
    ("fsecurity". "http://www.atmarkit.co.jp/rss/fsecurity/rss2dc.xml")
    ;;Business Computingフォーラム
    ("fbiz"  . "http://www.atmarkit.co.jp/rss/fbiz/rss2dc.xml")
    ;;＠IT自分戦略研究所
    ("jibun" . "http://jibun.atmarkit.co.jp/rss/rss2dc.xml")))

(defvar shimbun-atmarkit-rss-groups
  (mapcar 'car shimbun-atmarkit-rss-group-path-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-atmarkit-rss))
  (cdr (assoc (shimbun-current-group-internal shimbun)
	      shimbun-atmarkit-rss-group-path-alist)))

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-atmarkit-rss) url)
  (unless (string-match "\\([^\/]+\\)\\.html" url)
    (error "Cannot find message-id base"))
  (format "%s%%%s%%rss@atmarkit.co.jp" (match-string-no-properties 1 url)
	  (shimbun-current-group-internal shimbun)))

(provide 'sb-atmarkit-rss)

;;; sb-atmarkit-rss.el ends here

;;; sb-atmarkit.el --- shimbun backend for atmarkit

;; Copyright (C) 2003, 2004, 2005 NAKAJIMA Mikio <minakaji@namazu.org>

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

(luna-define-class shimbun-atmarkit (shimbun-rss) ())

(defvar shimbun-atmarkit-from-address  "info@atmarkit.co.jp")
(defvar shimbun-atmarkit-coding-system 'euc-japan)
(defvar shimbun-atmarkit-print-page-url
  "http://www.atmarkit.co.jp/club/print/print.php")

(defvar shimbun-atmarkit-group-path-alist
  '( ;; ニュース系
    ;; NewsInsight
    ("news". "http://www.atmarkit.co.jp/rss/news/rss2dc.xml")

    ;; フォーラム系
    ;; Windows Server Insiderフォーラム
    ("fwin2k" . "http://www.atmarkit.co.jp/rss/fwin2k/rss2dc.xml")
    ;; Insider.NETフォーラム
    ("fdotnet" . "http://www.atmarkit.co.jp/rss/fdotnet/rss2dc.xml")
    ;; System Insiderフォーラム
    ("fsys" . "http://www.atmarkit.co.jp/rss/fsys/rss2dc.xml")
    ;; XML & Web Servicesフォーラム
    ("fxml" . "http://www.atmarkit.co.jp/rss/fxml/rss2dc.xml")
    ;; Database Expertフォーラム
    ("fdb". "http://www.atmarkit.co.jp/rss/fdb/rss2dc.xml")
    ;; Linux Squareフォーラム
    ("flinux" . "http://www.atmarkit.co.jp/rss/flinux/rss2dc.xml")
    ;; Master of IP Networkフォーラム
    ("fnetwork" . "http://www.atmarkit.co.jp/rss/fnetwork/rss2dc.xml")
    ;; Java Solutionフォーラム
    ("fjava" . "http://www.atmarkit.co.jp/rss/fjava/rss2dc.xml")
    ;; Security&Trustフォーラム
    ("fsecurity". "http://www.atmarkit.co.jp/rss/fsecurity/rss2dc.xml")
    ;; IT Architectフォーラム
    ("farc" . "http://www.atmarkit.co.jp/rss/farc/rss2dc.xml")

    ;; obsolete フォーラム系
    ;; Business Computingフォーラム
    ("fbiz"  . "http://www.atmarkit.co.jp/rss/fbiz/rss2dc.xml")

    ;; ＠IT自分戦略研究所
    ("jibun" . "http://jibun.atmarkit.co.jp/rss/rss2dc.xml")
    ))

(defvar shimbun-atmarkit-groups
  (mapcar 'car shimbun-atmarkit-group-path-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-atmarkit))
  (cdr (assoc (shimbun-current-group-internal shimbun)
	      shimbun-atmarkit-group-path-alist)))

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-atmarkit) url date)
  (unless (string-match "http://[^\/]+/\\(.+\\)\\.html" url)
    (error "Cannot find message-id base"))
  (format "<%s%%%s@atmarkit.co.jp>" (match-string-no-properties 1 url)
	  (shimbun-current-group-internal shimbun)))

(luna-define-method shimbun-article ((shimbun shimbun-atmarkit) header &optional outbuf)
  (when (shimbun-current-group-internal shimbun)
    (with-current-buffer (or outbuf (current-buffer))
      (w3m-insert-string
       (or (with-temp-buffer
	     ;; get print page : referer is target page
	     (let ((w3m-coding-system-priority-list
		    (cons (shimbun-coding-system-internal shimbun)
			  w3m-coding-system-priority-list)))
	       (inline
		 (shimbun-retrieve-url shimbun-atmarkit-print-page-url t
				       nil (shimbun-article-url shimbun header))))
	     (shimbun-message shimbun "shimbun: Make contents...")
	     (goto-char (point-min))
	     (prog1 (shimbun-make-contents shimbun header)
	       (shimbun-message shimbun "shimbun: Make contents...done")))
	   "")))))

(luna-define-method shimbun-make-contents ((shimbun shimbun-atmarkit) header)
  (re-search-forward "<body[^>]*>" nil t)
  (delete-region (point-min) (point))
  (goto-char (point-max))
  (re-search-backward "</body[^>]*>" nil t)
  (delete-region (point) (point-max))
  (goto-char (point-min))
  (shimbun-make-html-contents shimbun header))

(luna-define-method shimbun-clear-contents ((shimbun shimbun-atmarkit) header)
  (shimbun-remove-tags "<script" "</script *>")
  (shimbun-remove-tags "<noscript" "</noscript *>")
  (shimbun-remove-tags "<form" "</form *>"))

(provide 'sb-atmarkit)

;;; sb-atmarkit.el ends here

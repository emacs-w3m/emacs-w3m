;;; sb-nikkei.el --- shimbun backend for nikkei.co.jp

;; Copyright (C) 2001, 2002
;; Kazuyoshi KOREEDA <Kazuyoshi.Koreeda@rdmg.mgcs.mei.co.jp>

;; Author: Kazuyoshi KOREEDA <Kazuyoshi.Koreeda@rdmg.mgcs.mei.co.jp>
;; Keywords: news

;; This file is a part of shimbun.

;; This program is free software; you can redistribute it a>nd/or modify
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

;; Original code was sb-asahi.el which is written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org> and
;; Yuuichi Teranishi <teranisi@gohome.org>.

;;; Code:

(require 'shimbun)
(require 'sb-text)

(luna-define-class shimbun-nikkei (shimbun shimbun-text) ())

(defvar shimbun-nikkei-url "http://www.nikkei.co.jp/news/")

(defvar shimbun-nikkei-groups '("keizai" "seiji" "kaigai" "market" "sangyo" "tento" "shakai" "retto" "shasetsu" "zinzi" "okuyami"))

(defvar shimbun-nikkei-from-address "webmaster@nikkei.co.jp")
(defvar shimbun-nikkei-content-start "<!--FJZONE START NAME=\"HONBUN\" -->")
(defvar shimbun-nikkei-content-end   "<!--FJZONE END NAME=\"HONBUN\" -->")
(defvar shimbun-nikkei-x-face-alist
  '(("default" . "X-Face: \"e7z+~O:s!)$84Dc68C##jE/~I8U:HDUkL@P\
euEhS<ijhd\"jc63do:naCRWPEr{Y5M?|]5g\n sa8m5@=sm%AIsSRA9*k08-`=\
w?yVB`L_vBG:j~~vhEoHC^Hjq`V(RMFQqa>9jqkt1<G[FMZTb:F@NT\n mcE[_Z\
_hl5zM,zn?WC*iun#*nJ'YRj}%;:|Y&X)kTXeM#lE*Y^E5}QMe?<pJjd</ktdg\\\
w9O17:Z>!\n vmZQ.BUpki=FZ:m[;]TP%D\\#uN6/)}c`/DPxKB?rQhBc\"")))
(defvar shimbun-nikkei-expiration-days 7)

(luna-define-method shimbun-index-url ((shimbun shimbun-nikkei))
  (format "%s%s/index.html"
	  (shimbun-url-internal shimbun)
	  (shimbun-current-group-internal shimbun)))

(defun shimbun-nikkei-get-headers (shimbun range)
  (let ((from (shimbun-from-address-internal shimbun))
	(group (shimbun-current-group-internal shimbun))
	(parent (shimbun-url-internal shimbun))
	(date "")
	(case-fold-search t)
	prefix basename headers)
    (goto-char (point-min))
    (when (re-search-forward "<!-- timeStamp -->\
\\(20[0-9][0-9]\\)/\\([01][0-9]\\)/\\([0-3][0-9]\\) \
\\([0-2][0-9]:[0-5][0-9]\\)\
<!-- /timeStamp -->"
			     nil t)
      (setq date
	    (shimbun-make-date-string (string-to-number (match-string 1))
				      (string-to-number (match-string 2))
				      (string-to-number (match-string 3))
				      (match-string 4)))
      (goto-char (point-min)))
    (while (re-search-forward "<!-- aLink --><\\(!-- \\)?\
a href=\"\\(20[0-9][0-9][01][0-9][0-3][0-9]\\)\\(.+\\)\\.html"
			      nil t)
      (setq prefix (match-string 2)
	    basename (match-string 3))
      (when (re-search-forward
	     "<!-- headline -->\\(.+\\)<!-- /headline -->"
	     (line-end-position) t)
	(push (shimbun-make-header
	       0
	       (shimbun-mime-encode-string (match-string 1))
	       from
	       date
	       (concat "<" basename "%" group ".nikkei.co.jp>")
	       ""
	       0
	       0
	       (concat parent group "/" prefix basename ".html"))
	      headers)))
    headers))

(luna-define-method shimbun-get-headers ((shimbun shimbun-nikkei)
					 &optional range)
  (shimbun-nikkei-get-headers shimbun range))

(provide 'sb-nikkei)

;;; sb-nikkei.el ends here

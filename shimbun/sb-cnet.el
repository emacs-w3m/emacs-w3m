;;; sb-cnet.el --- shimbun backend for cnet

;; Copyright (C) 2001, 2002, 2003, 2004 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>,
;;         Katsumi Yamaoka    <yamaoka@jpl.org>
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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-cnet (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-cnet-url "http://japan.cnet.com/")
(defvar shimbun-cnet-server-name "CNET Japan")
(defvar shimbun-cnet-groups '("news"))
(defvar shimbun-cnet-from-address  "webmaster@japan.cnet.com")
(defvar shimbun-cnet-content-start "")
(defvar shimbun-cnet-content-end "<!--NEWS LETTER SUB-->")
(defvar shimbun-cnet-x-face-alist
  '(("default" . "X-Face: 0p7.+XId>z%:!$ahe?x%+AEm37Abvn]n\
*GGh+>v=;[3`a{1lqO[$,~3C3xU_ri>[JwJ!9l0\n ~Y`b*eXAQ:*q=bBI\
_=ro*?]4:|n>]ZiLZ2LEo^2nr('C<+`lO~/!R[lH'N'4X&%\\I}8T!wt")))
(defvar shimbun-cnet-expiration-days 7)

(luna-define-method shimbun-index-url ((shimbun shimbun-cnet))
  (concat shimbun-cnet-url "news/archive.htm"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-cnet)
					 &optional range)
  (let (pt url subject headers)
    (while (re-search-forward "\
<a href=\"/\\(news/[^\"]+\\)\"[^>]*>\\([^<>]+\\)"
			      nil t)
      (setq pt (point)
	    url (match-string 1)
	    subject (match-string 2))
      (when (re-search-backward "\
>\\(20[0-9][0-9]\\)/\\([01][0-9]\\)/\\([0-3][0-9]\\)</"
				nil t)
	(goto-char pt)
	(push (shimbun-make-header
	       0
	       (shimbun-mime-encode-string subject)
	       (shimbun-from-address shimbun)
	       (shimbun-make-date-string (string-to-number (match-string 1))
					 (string-to-number (match-string 2))
					 (string-to-number (match-string 3)))
	       (concat "<"
		       (shimbun-replace-in-string
			(file-name-sans-extension url)
			"[,/]" ".")
		       "%japan.cnet.com>")
	       "" 0 0
	       (concat shimbun-cnet-url url))
	      headers)))
    headers))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-cnet)
						   header)
  (if (and (search-forward "<div class=\"plmain\">" nil t)
	   (search-forward "</noscript>" nil t))
      (delete-region (point-min) (point))
    (goto-char (point-min))))

(provide 'sb-cnet)

;;; sb-cnet.el ends here

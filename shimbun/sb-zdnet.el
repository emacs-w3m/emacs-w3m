;;; sb-zdnet.el --- shimbun backend for Zdnet Japan

;; Copyright (C) 2001 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>
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
(luna-define-class shimbun-zdnet (shimbun) ())

(defvar shimbun-zdnet-url "http://www.zdnet.co.jp/")

(defvar shimbun-zdnet-group-alist
  '(("comp" "news/past"
     "<A HREF=\"/\\(\\(news\\)/\\([0-9][0-9]\\)\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([^\\.\">]+\\)\\.html\\)[^>]*>")
    ("gamespot" "gamespot"
     "<A HREF=\"\\(\\(gsnews\\)/\\([0-9][0-9]\\)\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([^\\.\">]+\\)\\.html\\)[^>]*>")
    ("enterprise" "enterprise/archives"
     "<A HREF=\"/\\(\\(enterprise\\)/\\([0-9][0-9]\\)\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([^\\.\">]+\\)\\.html\\)[^>]*>")
    ("broadband" "broadband/news"
     "<A HREF=\"/\\(\\(broadband\\|broadband/rbb\\)/\\([0-9][0-9]\\)\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([^\\.\">]+\\)\\.html\\)[^>]*>")
    ("macwire" "macwire/news"
     "<A HREF=\"/\\(\\(macwire\\)/\\([0-9][0-9]\\)\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([^\\.\">]+\\)\\.html\\)[^>]*>")
    ("mobile" "mobile/news"
     "<A HREF=\"/\\(\\(mobile\\)/\\([0-9][0-9]\\)\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([^\\.\">]+\\)\\.html\\)[^>]*>")))

(defvar shimbun-zdnet-from-address "zdnn@softbank.co.jp")
(defvar shimbun-zdnet-content-start "\\(<!--BODY-->\\|<!--DATE-->\\)")
(defvar shimbun-zdnet-content-end "\\(<!--BODYEND-->\\|<!--BYLINEEND-->\\|<!--START RBB Logo -->\\)")
(defvar shimbun-zdnet-x-face-alist
  '(("default" . "X-Face: 88Zbg!1nj{i#[*WdSZNrn1$Cdfat,zsG`P)OLo=U05q:\
RM#72\\p;3XZ~j|7T)QC7\"(A;~Hr\n fP.D}o>Z.]=f)rOBz:A^G*M3Ea5JCB$a>BL/y!")))

(defvar shimbun-zdnet-expiration-days 31)

(luna-define-method shimbun-groups ((shimbun shimbun-zdnet))
  (mapcar 'car shimbun-zdnet-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-zdnet))
  (concat
   (shimbun-url-internal shimbun)
   (nth 1 (assoc (shimbun-current-group-internal shimbun)
		 shimbun-zdnet-group-alist))
   "/"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-zdnet)
					 &optional range)
  (let* ((case-fold-search t)
	 (group (shimbun-current-group-internal shimbun))
	 (url-regexp (nth 2 (assoc group shimbun-zdnet-group-alist)))
	 headers)
    (while (re-search-forward "\r" nil t)
      (replace-match "\n"))
    (goto-char (point-min))
    (while (re-search-forward url-regexp nil t)
      (unless (string= "index" (match-string 6))
	(let* ((url (match-string 1))
	       (year (+ 2000 (string-to-number (match-string 3))))
	       (month (string-to-number (match-string 4)))
	       (day (string-to-number (match-string 5)))
	       (id (format "<%s%s%s%s%%%s>"
			   (match-string 3)
			   (match-string 4)
			   (match-string 5)
			   (match-string 6)
			   group))
	       (subject (mapconcat 'identity
				   (split-string
				    (buffer-substring
				     (match-end 0)
				     (progn (search-forward "</A>" nil t) (point)))
				    "<[^>]+>")
				   "")))
	  (while (string-match "<[^>]+>" subject)
	    (setq subject (concat (substring subject 0 (match-beginning 0))
				  (substring subject (match-end 0)))))
	  (push (shimbun-make-header
		 0
		 (shimbun-mime-encode-string subject)
		 (shimbun-from-address-internal shimbun)
		 (shimbun-make-date-string year month day)
		 id  "" 0 0
		 (cond ((equal group "gamespot") (concat (shimbun-index-url shimbun) url))
		       (t (concat shimbun-zdnet-url url))))
		headers))))
    (nreverse headers)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-zdnet) header)
  (let ((case-fold-search t)
	(start))
    (while (and (search-forward "<!-- AD START -->" nil t)
		(setq start (match-beginning 0))
		(search-forward "<!-- AD END -->" nil t))
      (delete-region start (point)))
    (while (re-search-forward "<IMG [^>]*SRC=\"http://ad\\.zdnet\\.co\\.jp/[^>]+>" nil t)
      (replace-match ""))
    (goto-char (point-min))
    (while (re-search-forward "<A [^>]*HREF=\"http://ad\\.zdnet\\.co\\.jp/[^>]+>[^<]*</A>" nil t)
      (replace-match ""))
    (goto-char (point-min))))

(provide 'sb-zdnet)

;;; sb-zdnet.el ends here

;;; sb-f1express.el --- shimbun backend for www.f1express.co.jp
;;
;; Copyright (C) 2001 MIYOSHI Masanori <miyoshi@boreas.dti.ne.jp>

;; Author: MIYOSHI Masanori <miyoshi@boreas.dti.ne.jp>
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
(require 'sb-text)

(luna-define-class shimbun-f1express (shimbun shimbun-text) ())

(defvar shimbun-f1express-url "http://www.chunichi.ne.jp/")
(defvar shimbun-f1express-groups
  '("f1" "cart" "japan" "wrc" "bike" "paris" "other" "club"))
(defvar shimbun-f1express-from-address "f1exp@tokyo-np.co.jp")
(defvar shimbun-f1express-content-start "</td><td>")
(defvar shimbun-f1express-content-end  "</td></tr>")

(defvar shimbun-f1express-expiration-days 7)

(luna-define-method shimbun-index-url ((shimbun shimbun-f1express))
  (concat (shimbun-url-internal shimbun)
	  "f1express/"
	  (shimbun-current-group-internal shimbun)
	  "/headline.html"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-f1express)
					 &optional range)
  (let ((case-fold-search t) headers)
    (goto-char (point-min))
    (while (re-search-forward "<TR><TD width=\"[0-9]+\"></TD><TD bgcolor=\"#[0-9A-E]+\" CLASS=\"\\w+\" colspan=\"[0-9]+\" height=\"[0-9]+\"><A HREF=\"\\([^\"]+.html\\)\">\\([^[]+\\)\\[\\([0-9]+\\)\\.\\([0-9]+\\)/\\([0-9]+\\)\\.\\([0-9]+\\):\\([0-9]+\\)\\]</A></TD></TR>" nil t)
      (let* ((url (match-string 1))
	     (subject (match-string 2))
	     (year (match-string 3))
	     (month (match-string 4))
	     (day (match-string 5))
	     (hour (match-string 6))
	     (min (match-string 7))
	     id date)
	(setq id (format "<%s.%s.%s.%s.%s%%%s@www.chunichi.ne.jp>"
			 min hour day month year
			 (shimbun-current-group-internal shimbun)))
	(setq date (shimbun-make-date-string
		    (string-to-number year)
		    (string-to-number month)
		    (string-to-number day)
		    (concat hour ":" min)))
	(push (shimbun-make-header
	       0
	       (shimbun-mime-encode-string subject)
	       (shimbun-from-address-internal shimbun)
	       date id "" 0 0 (concat
			       (shimbun-url-internal shimbun)
			       url))
	      headers)))
    headers))

(provide 'sb-f1express)

;;; sb-f1express.el ends here

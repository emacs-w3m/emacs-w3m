;;; sb-pcweb-column.el --- shimbun backend for PC WEB COLUMN Square

;; Copyright (C) 2002 OHASHI Akira <bg66@koka-in.org>

;; Author: OHASHI Akira <bg66@koka-in.org>
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
(require 'sb-text)

(luna-define-class shimbun-pcweb-column (shimbun shimbun-text) ())

(defvar shimbun-pcweb-column-url "http://pcweb.mycom.co.jp/column/")
(defvar shimbun-pcweb-column-groups
  '(;; Under a series
    "gyokai" "benri" "hitech" "business" "winxp" "bytes" "newyork"
    "asia" "akiba" "rikei" "osx" "game"
    ;; Series end
    "smart98" "win2k" "msdos" "muteki" "mobile" "win98" "ayashii"
    "melon" "linux" "scrap" "siterev" "anime" "soho" "trouble"
    "renai" "dream" "tworld" "camera" "denshi" "tsushin" "shitumon"
    "nandemo" "hourou" "oshigoto" "tech" "speed"))
(defvar shimbun-pcweb-column-from-address "pcmail@pc.mycom.co.jp")
(defvar shimbun-pcweb-column-content-start "</i></font></p>")
(defvar shimbun-pcweb-column-content-end "<!-- #EndEditable -->")
(defvar shimbun-pcweb-column-coding-system 'shift_jis)

(luna-define-method shimbun-index-url ((shimbun shimbun-pcweb-column))
  (concat (shimbun-url-internal shimbun)
	  (shimbun-current-group-internal shimbun) ".html"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-pcweb-column)
					 &optional range)
  (let ((case-fold-search t)
	headers)
    (goto-char (point-min))
    (while (re-search-forward "<a href=\"\\(.+\\([0-9][0-9][0-9]\\).html\\)\"><font size=\"2\">\\([^<]+\\)</font></a>" nil t)
      (let ((url (match-string 1))
	    (num (match-string 2))
	    (subject (match-string 3))
	    id)
	(setq id (format "<%s.%s.column@pcweb.mycom.co.jp>"
			 num (shimbun-current-group-internal shimbun)))
	(push (shimbun-make-header
	       0
	       (shimbun-mime-encode-string subject)
	       (shimbun-from-address-internal shimbun)
	       "" id "" 0 0 (concat
			       (shimbun-url-internal shimbun)
			       url))
	      headers)))
    headers))

(luna-define-method shimbun-make-contents ((shimbun shimbun-pcweb-column)
					   header)
  (let ((case-fold-search t) (start))
    (save-excursion
      (when (re-search-forward
	     "<i>\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)</i>" nil t)
	(let ((year (string-to-number (match-string 1)))
	      (month (string-to-number (match-string 2)))
	      (day (string-to-number (match-string 3)))
	      date)
	  (setq date (shimbun-make-date-string year month day))
	  (shimbun-header-set-date header date))))
    (when (and (re-search-forward (shimbun-content-start-internal shimbun)
				  nil t)
	       (setq start (point))
	       (re-search-forward (shimbun-content-end-internal shimbun)
				  nil t))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start))
    (goto-char (point-min))
    (shimbun-header-insert shimbun header)
    (insert "Content-Type: text/html; charset=ISO-2022-JP\n"
	    "MIME-Version: 1.0\n\n")
    (encode-coding-string (buffer-string)
			  (mime-charset-to-coding-system "ISO-2022-JP"))))

(provide 'sb-pcweb-column)

;;; sb-pcweb-column.el ends here

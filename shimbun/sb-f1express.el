;;; sb-f1express.el --- shimbun backend for www.f1express.co.jp

;; Copyright (C) 2001, 2002 MIYOSHI Masanori <miyoshi@boreas.dti.ne.jp>

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

(luna-define-class shimbun-f1express (shimbun shimbun) ())

(defvar shimbun-f1express-url "http://f1express.cnc.ne.jp/")
(defvar shimbun-f1express-groups
  '("F1" "Pari-Dakar" "CART" "All-Japan" "WRC" "bike" "Tuka-CLUB"
    "etc" "F-Nippon" "JGTC" "LeMans24" "Indi500" "F3-Japan"
    "Super-Taikyu" "AJRC" "F-Toyota" "F-Dream" "etc-4-Japan"
    "F3-France" "F3-England" "F3-Germany" "LeMans-USA" "IRL" "FSC"
    "Int-F3000" "etc-4-int" "WGP" "WSB" "R2-1" "Suzuka8" "Motegi7"
    "AJ-Motocross" "AJ-Trial" "World-Motocross" "WCT" "etc-2"
    "Inter-GP-Cafe" "Takagi" "NASCAR" "Kato" "Endo" "Sato" "Imamiya"
    "Toyota-news" "F1-release" "info"))
(defvar shimbun-f1express-groups-alist
  (let ((index 0))
    (mapcar (lambda (x)
	      (setq index (+ 1 index))
	      (cons x (format "%d" index)))
	    shimbun-f1express-groups)))
(defvar shimbun-f1express-from-address "f1exp@tokyo-np.co.jp")
(defvar shimbun-f1express-content-start
  "<hr color=\"red\" width=\"100%\" size=\"1\"/><br/>")
(defvar shimbun-f1express-content-end
  "<hr size=\"1\" width=\"100%\" color=\"red\"/><br />")
(defvar shimbun-f1express-expiration-days 7)

(luna-define-method shimbun-index-url ((shimbun shimbun-f1express))
  (concat (shimbun-url-internal shimbun)
	  "TL001.php?cat_id="
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-f1express-groups-alist))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-f1express)
					 &optional range)
  (let ((case-fold-search t) headers)
    (goto-char (point-min))
    (while (re-search-forward
	    "<a href=\"/\\(config/VI001.php\\?teiko_id=\\([0-9]+\\)&amp;cat_id=[0-9]+&amp;top_flg=0\\)\"[^>]+>\\([^<]+\\)</a>\n.*\n.*\\[\\([0-9]*\\)/\\([0-9]+\\)/\\([0-9]+\\) \\([0-9]+\\):\\([0-9]+\\)\\]" nil t)
      (let ((url (match-string 1))
	    (id (match-string 2))
	    (subject (match-string 3))
	    (year  (match-string 4))
	    (month  (match-string 5))
	    (day  (match-string 6))
	    (hour  (match-string 7))
	    (min  (match-string 8))
	    date)
	(while (string-match "&amp;" url)
	  (setq url (replace-match "&" t t url)))
	(setq id (format "<%s%%%s@www.chunichi.ne.jp>"
			 id (shimbun-current-group-internal shimbun)))
	(setq date (shimbun-make-date-string
		    (string-to-number year)
		    (string-to-number month)
		    (string-to-number day)
		    (concat hour ":" min)))
	(push (shimbun-make-header
	       0
	       (shimbun-mime-encode-string subject)
	       (shimbun-from-address shimbun)
	       date id "" 0 0 (concat
			       (shimbun-url-internal shimbun)
			       url))
	      headers)))
    headers))

(provide 'sb-f1express)

;;; sb-f1express.el ends here

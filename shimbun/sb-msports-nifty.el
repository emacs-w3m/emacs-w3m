;;; sb-msports-nifty.el --- shimbun backend for motorsports.nifty.com

;; Copyright (C) 2004, 2005 MIYOSHI Masanori <miyoshi@meadowy.org>

;; Author: MIYOSHI Masanori <miyoshi@meadowy.org>
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

(luna-define-class shimbun-msports-nifty (shimbun shimbun-text) ())

(defvar shimbun-msports-nifty-url "http://forum.nifty.com/fmotor/")
(defvar shimbun-msports-nifty-server-name "@nifty:モータースポーツ")
(defvar shimbun-msports-nifty-group-alist
  '(("F1" . "f1")
    ("IRL" . "cart")
    ("WRC" . "wrc")
    ("Europe" . "europe")
    ("USA" . "usa")))
(defvar shimbun-msports-nifty-from-address "motorsports_post@nifty.com")
(defvar shimbun-msports-nifty-content-start
  "^<div class=\"entry-body-text\">\n<p><FONT[^>]+>.*\n?.*</FONT>.*\n<img[^>]+>")
(defvar shimbun-msports-nifty-content-end
  "^</div>\n</div>\n+<div class=\"entry-body-bottom\">")

(luna-define-method shimbun-groups ((shimbun shimbun-msports-nifty))
  (mapcar 'car shimbun-msports-nifty-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-msports-nifty))
  (concat (shimbun-url-internal shimbun)
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-msports-nifty-group-alist))
	  "/main.htm"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-msports-nifty)
					 &optional range)
  (let ((case-fold-search t) headers)
    (goto-char (point-min))
    (while (re-search-forward
	    "<A HREF='\\(http://.*/\\([0-9]+\\)/\\([0-9][0-9]\\)/index\.html#\\(.*\\)\\)'[^>]*>☆　\\([^<]+\\)<BR>" nil t)
      (let ((url (match-string 1))
	    (year (match-string 2))
	    (month (match-string 3))
	    (id (match-string 4))
	    (subject (match-string 5))
	    (day 1) date)
	(save-excursion
	  (when (re-search-backward "[0-9]+/[0-9][0-9]/\\([0-9][0-9]\\)" nil t)
	    (setq day (match-string 1))))
	(setq id (format "<%s%s%s%s%%%s%%msports@nifty.com>"
			 year month day
			 id (shimbun-current-group-internal shimbun)))
	(setq date (shimbun-make-date-string
		    (string-to-number year)
		    (string-to-number month)
		    (string-to-number day)))
	(push (shimbun-create-header
	       0
	       subject
	       (shimbun-from-address shimbun)
	       date id "" 0 0 url)
	      headers)))
    headers))

(luna-define-method shimbun-make-contents ((shimbun shimbun-msports-nifty)
					   header)
  (let ((id (shimbun-header-id header))
	start)
    (setq id (substring id 9 20))	; extract anchor
    (re-search-forward (format "<a id=\"%s\"></a>" id) nil t)
    (delete-region (point-min) (point))
    (shimbun-header-insert-and-buffer-string
     shimbun header "UTF-8"
     (if (shimbun-clear-contents shimbun header)
	 (shimbun-shallow-rendering)
       t))))

(provide 'sb-msports-nifty)

;;; sb-msportsn-nifty.el ends here

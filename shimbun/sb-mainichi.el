;;; sb-mainichi.el --- shimbun backend for www.mainichi.co.jp

;; Author: Koichiro Ohba <koichiro@meadowy.org>

;; Keywords: news

;;; Copyright:

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

;; Original code was sb-yomiuri.el which is written by
;; TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp> and 
;; Yuuichi Teranishi <teranisi@gohome.org>

;;; Code:

(require 'shimbun)
(require 'sb-text)

(luna-define-class shimbun-mainichi (shimbun shimbun-text) ())

(defvar shimbun-mainichi-url "http://www.mainichi.co.jp/")
(defvar shimbun-mainichi-groups '("shakai" "sports" "seiji" "keizai"
				  "kokusai" "fuho"))
(defvar shimbun-mainichi-from-address  "webmaster@mainichi.co.jp")
(defvar shimbun-mainichi-content-start "\n<font class=\"news-text\">\n")
(defvar shimbun-mainichi-content-end  "\n</font>\n")

(defvar shimbun-mainichi-group-path-alist
  '(("shakai" . "news/flash/shakai")
    ("sports" . "news/flash/sports")
    ("seiji"  . "news/flash/seiji")
    ("keizai" . "news/flash/keizai")
    ("kokusai" . "news/flash/kokusai")
    ("fuho"    . "news/flash/jinji")))

(luna-define-method shimbun-index-url ((shimbun shimbun-mainichi))
  (concat (shimbun-url-internal shimbun)
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-mainichi-group-path-alist))
	  "/index.html"))

(luna-define-method shimbun-article-expiration-days ((shimbun
						      shimbun-mainichi))
  7)

(luna-define-method shimbun-get-headers ((shimbun shimbun-mainichi))
  (let ((case-fold-search t)
	start headers)
    (goto-char (point-min))
    (when (and (search-forward
		(format "\n<table bgcolor=\"#FFFFFF\" width=\"564\" cellpadding=\"0\" cellspacing=\"0\" border=\"0\">\n"
			(shimbun-current-group-internal shimbun)) nil t)
	       (setq start (point))
	       (search-forward
		(format "\n</tr>\n</table>\n<br>\n<br>\n"
			(shimbun-current-group-internal shimbun)) nil t))
      (forward-line -1)
      (save-restriction
	(narrow-to-region start (point))
	(goto-char start)
	(while (re-search-forward
		"<a href=\"\\(\\./\\(\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([a-z]\\)\\([0-9][0-9][0-9][0-9]\\)\\([a-z]\\)\\([0-9][0-9][0-9][0-9][0-9]\\)\\([0-9][0-9][0-9][0-9]\\)\\([a-z]\\)\\)\\.html\\)\"[^>]*>" 
		nil t)
	  (let ((url   (concat 
			(cdr (assoc (shimbun-current-group-internal shimbun)
				    shimbun-mainichi-group-path-alist))
			"/"
			(match-string 1)))
		(id    (format "<%s%%%s>"
			       (match-string 2)
			       (shimbun-current-group-internal shimbun)))
		(year  (string-to-number (match-string 3)))
		(month (string-to-number (match-string 4)))
		(day   (string-to-number (match-string 5)))
		(subject (mapconcat
			  'identity
			  (split-string
			   (buffer-substring
			    (match-end 0)
			    (progn (search-forward "</FONT></td>" nil t) (point)))
			   "<[^>]+>")
			  ""))
		date)
	    (when (string-match "<FONT class=\"news-text\">" subject)
	      (setq subject (substring subject (match-end 0))))
	    (if (string-match "[0-9][0-9]:[0-9][0-9]" subject)
		(setq date (shimbun-make-date-string
			    year month day (match-string 0 subject))
		      subject (substring subject 0 (match-beginning 0)))
	      (setq date (shimbun-make-date-string year month day)))
	    (push (shimbun-make-header
		   0
		   (shimbun-mime-encode-string subject)
		   (shimbun-from-address-internal shimbun)
		   date id "" 0 0 (concat
				   (shimbun-url-internal shimbun)
				   url))
		  headers)))))
    headers))

(provide 'sb-mainichi)

;;; sb-mainichi.el ends here

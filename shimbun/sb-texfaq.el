;;; sb-texfaq.el --- shimbun backend for www.matsusaka-u.ac.jp

;; Author: Hidetaka Iwai <tyuyu@mb6.seikyou.ne.jp>

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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-texfaq (shimbun) ())

(defvar shimbun-texfaq-url "http://www.matsusaka-u.ac.jp/~okumura/texfaq/qa/")
(defvar shimbun-texfaq-groups
  '("qanda"))
(defvar shimbun-texfaq-content-start "</h2>\n")
(defvar shimbun-texfaq-content-end  "\n<hr>\n<p>")
(defvar shimbun-texfaq-group-path-alist
  '(("qanda" . "index.html")))
(defvar shimbun-texfaq-expiration-days 7)
(defvar shimbun-texfaq-coding-system 'euc-jp)

(luna-define-method shimbun-index-url ((shimbun shimbun-texfaq))
  (concat (shimbun-url-internal shimbun)
	  (cdr (assoc (shimbun-current-group-internal shimbun)
                      shimbun-texfaq-group-path-alist))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-texfaq)
					 &optional range)
  (let ((case-fold-search t) headers)
    (goto-char (point-min))
    (while (re-search-forward "[0-9]+: \\([0-9][0-9][0-9][0-9]\\)-\\([0-1][0-9]\\)-\\([0-3][0-9]\\) \\([0-2][0-9]\\):\\([0-5][0-9]\\):\\([0-5][0-9]\\) <a href=\"\\([0-9]+.html\\)\">\\([^<]+\\)</a>\\([^<]+\\)<br>" nil t)
      (let* ((url (match-string 7))
	     (year (match-string 1))
	     (month (match-string 2))
	     (day (match-string 3))
	     (hour (match-string 4))
	     (min (match-string 5))
	     (sec (match-string 6))
	     (subject (match-string 8))
	     (from (match-string 9))
	     id date)
	(setq id (format "<%s.%s.%s.%s.%s.%s%%%s@www.texfaq.ac.jp>"
			 sec min hour day month year
			 (shimbun-current-group-internal shimbun)))
	(setq date (shimbun-make-date-string
		    (string-to-number year)
		    (string-to-number month)
		    (string-to-number day)
		    (concat hour ":" min ":" sec)))
	(push (shimbun-make-header
	       0
	       (shimbun-mime-encode-string subject)
	       (shimbun-mime-encode-string from)
	       date id "" 0 0 (concat
			       (shimbun-url-internal shimbun)
			       url))
	      headers)))
      headers))

(provide 'sb-texfaq)

;;; sb-texfaq.el ends here

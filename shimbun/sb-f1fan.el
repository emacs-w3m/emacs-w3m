;;; sb-f1fan.el --- shimbun backend for www.ksky.ne.jp/~tahara/f1/

;; Author: MIYOSHI Masanori <miyoshi@boreas.dti.ne.jp>

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
(require 'sb-text)

(luna-define-class shimbun-f1fan (shimbun shimbun-text) ())

(defvar shimbun-f1fan-url "http://www.ksky.ne.jp/~tahara/f1/")
(defvar shimbun-f1fan-groups '("news"))
(defvar shimbun-f1fan-from-address "tahara@ps.ksky.ne.jp")
(defvar shimbun-f1fan-content-start "<blockquote>")
(defvar shimbun-f1fan-content-end  "</blockquote>")
(defvar shimbun-f1fan-coding-system 'shift_jis)

(luna-define-method shimbun-index-url ((shimbun shimbun-f1fan))
  (shimbun-url-internal shimbun))

(luna-define-method shimbun-get-headers ((shimbun shimbun-f1fan)
					 &optional range)
  (let ((case-fold-search t) headers)
    (goto-char (point-min))
    (while (re-search-forward "Ｆ１　*：<a href=\"\\(News\\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\.html\\)\">\\([^<]+\\)</a><br>" nil t)
      (let ((url (match-string 1))
	    (year (match-string 2))
	    (month (match-string 3))
	    (day (match-string 4))
	    (num (match-string 5))
	    (subject (match-string 6))
	    id date)
	(setq id (format "<%s%s%s%s.%s.tahara@ps.ksky.ne.jp>"
			 year month day num
			 (shimbun-current-group-internal shimbun)))
	(setq date (shimbun-make-date-string
		    (string-to-number year)
		    (string-to-number month)
		    (string-to-number day)))
	(push (shimbun-make-header
	       0
	       (shimbun-mime-encode-string subject)
	       (shimbun-from-address-internal shimbun)
	       date id "" 0 0 (concat
			       (shimbun-url-internal shimbun)
			       url))
	      headers)))
    headers))

(luna-define-method shimbun-make-contents ((shimbun shimbun-f1fan)
					   header)
  (let ((case-fold-search t) (html t) (start))
    (when (and (re-search-forward (shimbun-content-start-internal shimbun)
				  nil t)
	       (setq start (point))
	       (re-search-forward (shimbun-content-end-internal shimbun)
				  nil t))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start)
      (setq html nil))
    (goto-char (point-min))
    (shimbun-header-insert shimbun header)
    (insert "Content-Type: " (if html "text/html" "text/plain")
	    "; charset=ISO-2022-JP\nMIME-Version: 1.0\n\n")
    (encode-coding-string (buffer-string)
			  (mime-charset-to-coding-system "ISO-2022-JP"))))

(provide 'sb-f1fan)

;;; sb-f1fan.el ends here

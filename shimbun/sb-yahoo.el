;;; sb-yahoo.el --- shimbun backend for news.yahoo.co.jp

;; Author: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;         Akihiro Arisawa    <ari@atesoft.advantest.co.jp>
;;         Yuuichi Teranishi <teranisi@gohome.org>

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
;; TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>.

;;; Code:

(require 'shimbun)
(require 'sb-text)

(luna-define-class shimbun-yahoo (shimbun shimbun-text) ())

(defvar shimbun-yahoo-url "http://news.yahoo.co.jp/headlines/")

(defvar shimbun-yahoo-groups '("cpt" "dom" "int" "bus" "brf" "biz" "ent" "spo" "nkn" "clm"))
 
(defvar shimbun-yahoo-coding-system 'euc-jp)
(defvar shimbun-yahoo-from-address "news-admin@mail.yahoo.co.jp")
(defvar shimbun-yahoo-content-start "\n<!-- TextStart -->\n")
(defvar shimbun-yahoo-content-end   "\n<!-- TextEnd -->\n")

(defvar shimbun-yahoo-x-face-alist
  '(("default" . "X-Face: Ygq$6P.,%Xt$U)DS)cRY@k$VkW!7(X'X'?U{{osjjFG\"E]hND;SPJ-J?O?R|a?L
        g2$0rVng=O3Lt}?~IId8Jj&vP^3*o=LKUyk(`t%0c!;t6REk=JbpsEn9MrN7gZ%")))


(luna-define-method shimbun-index-url ((shimbun shimbun-yahoo))
      (format "%s%s/index.html"
	      (shimbun-url-internal shimbun)
	      (shimbun-current-group-internal shimbun)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-yahoo))
  (let ((case-fold-search t)
	p headers)
    (goto-char (point-min))
    (while (re-search-forward "<!-- GEN_HEADLINES -->" nil t)
      (delete-region (point-min) (point))
      (when (re-search-forward "<!-- GEN_BEFORE -->" nil t)
	(delete-region (point) (point-max))
	(goto-char (point-min))
	(while (re-search-forward "<a href=\\(/headlines/[a-z]*/\\(\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\)/[a-z]*/\\([0-9][0-9]\\)\\([0-9][0-9]\\)[0-9]*_\\([a-z]*[0-9]*\\)\\.html\\)>\\([^\n]*\\)</a><br>" nil t)
	  (let ((id (format "<%s%s%%%s>"
			    (match-string 2)
			    (match-string 8)
			    (shimbun-current-group-internal shimbun)))
		(url (match-string 1))
		(year (+ 2000 (string-to-number (match-string 3))))
		(month (string-to-number (match-string 4)))
		(day   (string-to-number (match-string 5)))
		(time  (format "%s:%s" (match-string 6) (match-string 7))))
	    (push (shimbun-make-header
		   0
		   (shimbun-mime-encode-string
		    (match-string 9))
		   (shimbun-from-address-internal shimbun)
		   (shimbun-make-date-string year month day time)
		   id "" 0 0 (concat "http://news.yahoo.co.jp" url))
		  headers)))
	(when (re-search-forward "<a href=\\([^>]+\\)>次ページ</a>" nil t)
	  (let ((url (concat "http://news.yahoo.co.jp" (match-string 1))))
	    (erase-buffer)
	    (shimbun-retrieve-url-buffer url t)
	    (goto-char (point-min))))))
    (nreverse headers)))

(provide 'sb-yahoo)

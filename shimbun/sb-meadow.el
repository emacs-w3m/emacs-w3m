;;; sb-meadow.el --- shimbun backend for meadow-ml

;; Author: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>,
;;         Akihiro Arisawa    <ari@mbf.sphere.ne.jp>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>

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

;; Original code was sb-mew.el written by
;; TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp> and
;; Yuuichi Teranishi <teranisi@gohome.org>.

;;; Code:

(require 'shimbun)
(require 'sb-mhonarc)
(luna-define-class shimbun-meadow (shimbun-mhonarc) ())

(defvar shimbun-meadow-url "http://www.ysnb.net/meadow/")
(defvar shimbun-meadow-groups '("meadow-develop" "meadow-users-jp"))
(defvar shimbun-meadow-reverse-flag nil)
(defvar shimbun-meadow-litemplate-regexp
  "<STRONG><A NAME=\"\\([0-9]+\\)\" HREF=\"\\(msg[0-9]+.html\\)\">\\([^<]+\\)</a> \\([^<]+\\)</STRONG>")

(luna-define-method shimbun-headers ((shimbun shimbun-meadow)
				     &optional range)
  (with-temp-buffer
    (shimbun-retrieve-url shimbun-meadow-url)
    (let* ((group (shimbun-current-group-internal shimbun))
	   (regexp (format
		    "<a href=\"\\(%s/\\([1-9][0-9][0-9][0-9]\\)/\\)\""
		    (regexp-quote group)))
	   (case-fold-search t)
	   (pages (shimbun-header-index-pages range))
	   (count 0)
	   (indexes) ; This should be `indices' ;-).
	   (headers))
      (while (re-search-forward regexp nil t)
	(push (cons (match-string 2)
		    (shimbun-expand-url (match-string 1) shimbun-meadow-url))
	      indexes))
      (catch 'stop
	(dolist (elem indexes)
	  (delete-region (point-min) (point-max))
	  (shimbun-retrieve-url (cdr elem) t)
	  (goto-char (point-min))
	  (when (re-search-forward
		 "<A[^>]*HREF=\"mail\\([0-9]+\\)\\.html\">\\[?Last Page\\]?</A>"
		 nil t)
	    (let ((aux (string-to-number (match-string 1)))
		  url)
	      (while (> aux 0)
		(setq url (if (= aux 1)
			      (cdr elem)
			    (shimbun-expand-url (format "mail%d.html" aux) (cdr elem))))
		(delete-region (point-min) (point-max))
		(shimbun-retrieve-url url)
		(unless (if pages (<= (incf count) pages) t)
		  (throw 'stop headers))
		(shimbun-mhonarc-get-headers shimbun url headers (car elem))
		(setq aux (1- aux))))))
	headers))))

(provide 'sb-meadow)

;;; sb-meadow.el ends here

;;; sb-asahi.el --- shimbun backend for asahi.com

;; Copyright (C) 2001, 2002, 2003 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>,
;;         Katsumi Yamaoka    <yamaoka@jpl.org>
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
(luna-define-class shimbun-asahi (shimbun-text) ())

(defvar shimbun-asahi-top-level-domain "asahi.com"
  "Name of the top level domain for the Asahi shimbun.")

(defvar shimbun-asahi-url (concat "http://www."
				  shimbun-asahi-top-level-domain
				  "/")
  "Name of the parent url.")

(defvar shimbun-asahi-groups '("national" "business" "politics"
			       "international" "sports" "culture")
  "List of available group names.  Each name should be a directory name
which is in existence under the parent url `shimbun-asahi-url'.")

(defvar shimbun-asahi-from-address (concat "webmaster@www."
					   shimbun-asahi-top-level-domain))

(defvar shimbun-asahi-content-start "\n<!-- FJZONE START NAME=\"HONBUN\"-->")
(defvar shimbun-asahi-content-end "\n<!-- FJZONE END NAME=\"HONBUN\"-->")
(defvar shimbun-asahi-x-face-alist
  '(("default" . "X-Face: +Oh!C!EFfmR$+Zw{dwWW]1e_>S0rnNCA*CX|\
bIy3rr^<Q#lf&~ADU:X!t5t>gW5)Q]N{Mmn\n L]suPpL|gFjV{S|]a-:)\\FR\
7GRf9uL:ue5_=;h{V%@()={uTd@l?eXBppF%`6W%;h`#]2q+f*81n$B\n h|t")))

(defvar shimbun-asahi-expiration-days 6)

(defun shimbun-asahi-index-url (entity)
  "Return a url for the list page corresponding to ENTITY."
  (concat (shimbun-url-internal entity)
	  (shimbun-current-group-internal entity)
	  "/list.html"))

(luna-define-method shimbun-index-url ((shimbun shimbun-asahi))
  (shimbun-asahi-index-url shimbun))

(defun shimbun-asahi-get-headers (entity)
  "Return a list of header objects from the top page."
  (let* ((group (shimbun-current-group-internal entity))
	 (regexp (concat "<a[\t\n ]+href=[\t\n ]*\"/\\("
			 (regexp-quote group)
			 "/update/\\([0-9][0-9]\\)\\([0-9][0-9]\\)"
			 "/\\([0-9]+\\).html\\)\"[\t\n ]*>[\t\n ]*"))
	 (case-fold-search t)
	 (next t)
	 year cmonth subject url id month headers)
    (setq year (decode-time)
	  cmonth (nth 4 year)
	  year (nth 5 year))
    (re-search-forward "images/gif/digest.gif" nil t) ; Skip top article.
    (while (and next
		(if (numberp next)
		    (goto-char (match-end 0))
		  (re-search-forward regexp nil t)))
      (setq subject (match-end 0)
	    url (match-string 1)
	    month (string-to-number (match-string 2))
	    id (format "<%d%02d%s.%s%%%s.%s>"
		       (cond ((and (= 12 month) (= 1 cmonth))
			      (1- year))
			     ((and (= 1 month) (= 12 cmonth))
			      (1+ year))
			     (t
			      year))
		       month
		       (match-string 3)
		       (match-string 4)
		       group
		       shimbun-asahi-top-level-domain)
	    next (when (re-search-forward regexp nil t)
		   (match-beginning 0)))
      (goto-char subject)
      (save-match-data
	(setq subject (if (re-search-forward "[\t\n ]*</a>" next t)
			  (shimbun-mime-encode-string
			   (buffer-substring subject (match-beginning 0)))
			"(none)")
	      month (when (re-search-forward "\
<span[\t\n ]+class=[\t\n ]*\"Time\"[\t\n ]*>[\t\n ]*([\t\n ]*\
\\([0-9][0-9]\\)/\\([0-9][0-9]\\)[^()0-9:;]+;[\t\n ]*\
\\([0-9][0-9]:[0-9][0-9]\\)[\t\n ]*)[\t\n ]*</span>" next t)
		      (string-to-number (match-string 1))))
	(push
	 (shimbun-make-header
	  0
	  subject
	  (shimbun-from-address-internal entity)
	  (if month
	      (shimbun-make-date-string
	       (cond ((and (= 12 month) (= 1 cmonth))
		      (1- year))
		     ((and (= 1 month) (= 12 cmonth))
		      (1+ year))
		     (t
		      year))
	       month
	       (string-to-number (match-string 2))
	       (match-string 3))
	    "")
	  id "" 0 0
	  (concat (shimbun-url-internal entity) url))
	 headers)))
    headers))

(luna-define-method shimbun-get-headers ((shimbun shimbun-asahi)
					 &optional range)
  (shimbun-asahi-get-headers shimbun))

(provide 'sb-asahi)

;;; sb-asahi.el ends here

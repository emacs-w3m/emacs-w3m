;;; sb-asahi.el --- shimbun backend for asahi.com

;; Copyright (C) 2001 Yuuichi Teranishi <teranisi@gohome.org>

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

(defvar shimbun-asahi-url "http://www.asahi.com/"
  "Name of the parent url.")

(defvar shimbun-asahi-groups '("national" "business" "politics"
			       "international" "sports" "culture")
  "List of available group names.  Each name should be a directory name
which is in existence under the parent url `shimbun-asahi-url'.")

(defvar shimbun-asahi-groups-should-use-list-page '("business" "sports")
  "List of groups that the new news can be found in the list page.  If a
group is included in this list, this program will look for the new
news in the url \"update/list.html\" under a group directory rather than
the top page of a group.")

(defvar shimbun-asahi-from-address "webmaster@www.asahi.com")

(defvar shimbun-asahi-content-start "\n<!-- Start of kiji -->\n")
(defvar shimbun-asahi-content-end "\n<!-- End of kiji -->\n")
(defvar shimbun-asahi-x-face-alist
  '(("default" . "X-Face: +Oh!C!EFfmR$+Zw{dwWW]1e_>S0rnNCA*CX|\
bIy3rr^<Q#lf&~ADU:X!t5t>gW5)Q]N{Mmn\n L]suPpL|gFjV{S|]a-:)\\FR\
7GRf9uL:ue5_=;h{V%@()={uTd@l?eXBppF%`6W%;h`#]2q+f*81n$B\n h|t")))

(defvar shimbun-asahi-expiration-days 6)

(luna-define-method shimbun-index-url ((shimbun shimbun-asahi))
  (let ((group (shimbun-current-group-internal shimbun)))
    (concat (shimbun-url-internal shimbun)
	    group
	    "/"
	    (if (member group shimbun-asahi-groups-should-use-list-page)
		"update/list.html"
	      ""))))

(defun shimbun-asahi-get-headers-in-the-list-page (entity)
  "Return a list of header objects from the list page."
  (when (search-forward "\n<!-- Start of past -->\n" nil t)
    (delete-region (point-min) (point))
    (when (search-forward "\n<!-- End of past -->\n" nil t)
      (forward-line -1)
      (delete-region (point) (point-max))
      (goto-char (point-min))
      (let ((case-fold-search t)
	    headers)
	(while (re-search-forward "\
<a href=\"\\(\\([0-9][0-9][0-9][0-9]\\)/\\([0-9]+\\)\\.html\\)\"> *"
				  nil t)
	  (let* ((group (shimbun-current-group-internal entity))
		(id (format "<%s%s%%%s>"
			    (match-string 2) (match-string 3) group))
		(url (match-string 1)))
	    (push (shimbun-make-header
		   0
		   (shimbun-mime-encode-string
		    (mapconcat 'identity
			       (split-string
				(buffer-substring
				 (match-end 0)
				 (progn
				   (search-forward "<br>" nil t) (point)))
				"\\(<[^>]+>\\|\r\\)")
			       ""))
		   (shimbun-from-address-internal entity)
		   "" id "" 0 0 (format "%s%s/update/%s"
					(shimbun-url-internal entity)
					group url))
		  headers)))
	(setq headers (nreverse headers))
	(let ((i 0))
	  (while (and (nth i headers)
		      (re-search-forward "\
^(\\([0-9][0-9]\\)/\\([0-9][0-9]\\) \\([0-9][0-9]:[0-9][0-9]\\))"
					 nil t))
	    (let ((month (string-to-number (match-string 1)))
		  (date (decode-time (current-time))))
	      (shimbun-header-set-date
	       (nth i headers)
	       (shimbun-make-date-string
		(if (and (eq 12 month) (eq 1 (nth 4 date)))
		    (1- (nth 5 date))
		  (nth 5 date))
		month
		(string-to-number (match-string 2))
		(match-string 3))))
	    (setq i (1+ i))))
	(nreverse headers)))))

(defun shimbun-asahi-get-headers-in-the-top-page (entity)
  "Return a list of header objects from the top page."
  (let* ((group (shimbun-current-group-internal entity))
	 (regexp (concat "<a[\t\n ]+href=[\t\n ]*\"/\\("
			 (regexp-quote group)
			 "/update/\\([0-9][0-9][0-9][0-9]\\)"
			 "/\\([0-9]+\\).html\\)\"[\t\n ]*>[\t\n ]*"))
	 (case-fold-search t)
	 (next t)
	 (date (decode-time))
	 subject url id month headers)
    (re-search-forward "images/gif/digest.gif" nil t) ; Skip top article.
    (while (and next
		(if (numberp next)
		    (goto-char (match-end 0))
		  (re-search-forward regexp nil t)))
      (setq subject (match-end 0)
	    url (match-string 1)
	    id (concat "<" (match-string 2) (match-string 3) "%" group ">")
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
	      (shimbun-make-date-string (if (and (eq 12 month)
						 (eq 1 (nth 4 date)))
					    (1- (nth 5 date))
					  (nth 5 date))
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
  (if (member (shimbun-current-group-internal shimbun)
	      shimbun-asahi-groups-should-use-list-page)
      (shimbun-asahi-get-headers-in-the-list-page shimbun)
    (shimbun-asahi-get-headers-in-the-top-page shimbun)))

(provide 'sb-asahi)

;;; sb-asahi.el ends here

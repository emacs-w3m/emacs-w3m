;;; sb-asahi.el --- shimbun backend for asahi.com

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>,
;;         Katsumi Yamaoka    <yamaoka@jpl.org>

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
(luna-define-class shimbun-asahi (shimbun-text) ())

(defvar shimbun-asahi-url "http://spin.asahi.com/")
(defvar shimbun-asahi-groups '("national" "business" "politics"
			       "international" "sports" "culture"))
(defvar shimbun-asahi-from-address "webmaster@www.asahi.com")

(defvar shimbun-asahi-content-start "\n<!-- Start of kiji -->\n")
(defvar shimbun-asahi-content-end "\n<!-- End of kiji -->\n")
(defvar shimbun-asahi-x-face-alist
  '(("default" .
     "X-Face:  +Oh!C!EFfmR$+Zw{dwWW]1e_>S0rnNCA*CX|bIy3rr^<Q#lf&~ADU:X!t5t>
        gW5)Q]N{MmnL]suPpL|gFjV{S|]a-:)\\FR7GRf9uL:ue5_=;h{V%@()={u
        Td@l?eXBppF%`6W%;h`#]2q+f*81n$Bh|t")))

(defvar shimbun-asahi-expiration-days 6)

(luna-define-method shimbun-index-url ((shimbun shimbun-asahi))
  (concat (shimbun-url-internal shimbun)
	  (shimbun-current-group-internal shimbun)
	  "/"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-asahi)
					 &optional range)
  (let* ((group (shimbun-current-group-internal shimbun))
	 (regexp (concat "<a[\t\n ]+href=[\t\n ]*\"/\\("
			 (regexp-quote group)
			 "/update/\\([0-9][0-9][0-9][0-9]\\)"
			 "/\\([0-9]+\\).html\\)\"[\t\n ]*>[\t\n ]*"))
	 (case-fold-search t)
	 (next t)
	 (date (decode-time))
	 subject url id month headers)
    (while (and next
		(if (numberp next)
		    (goto-char next)
		  (re-search-forward regexp nil t)))
      (setq subject (match-end 0)
	    url (match-string 1)
	    id (concat "<" (match-string 2) (match-string 3) "%" group ">")
	    next (re-search-forward regexp nil t))
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
	  (shimbun-from-address-internal shimbun)
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
	  (concat (shimbun-url-internal shimbun) url))
	 headers)))
    headers))

(provide 'sb-asahi)

;;; sb-asahi.el ends here

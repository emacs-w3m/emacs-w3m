;;; sb-asahi.el --- shimbun backend for asahi.com -*- coding: iso-2022-7bit; -*-

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

(luna-define-class shimbun-asahi
		   (shimbun-japanese-newspaper shimbun-text) ())

(defvar shimbun-asahi-top-level-domain "asahi.com"
  "Name of the top level domain for the Asahi shimbun.")

(defvar shimbun-asahi-url
  (concat "http://www." shimbun-asahi-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-asahi-group-table
  (let* ((s0 "[\t\n ]*")
	 (s1 "[\t\n ]+")
	 (no-nl "[^\n<>]+")
	 (default (list
		   (concat
		    "<a" s1 "href=\"/"
		    ;; 1. url
		    "\\(%s/"
		    ;; 3 or 4. serial number
		    "\\("
		    "update/[01][0-9][0-3][0-9]/\\([a-z]*[0-9]+\\)"
		    "\\|"
		    "\\(" no-nl "\\)"
		    "\\)"
		    "\\.html\\)"
		    "\">" s0
		    ;; 5. subject
		    "\\(" no-nl "\\)"
		    s0 "</a>" s0 "("
		    ;; 6. month
		    "\\([01][0-9]\\)"
		    "/"
		    ;; 7. day
		    "\\([0-3][0-9]\\)"
		    "\\(" s1
		    ;; 9. hour:minute
		    "\\([012][0-9]:[0-5][0-9]\\)"
		    "\\)?)")
		   1 3 4 5 6 7 9)))
    `(("business" "経済" "%s/" ,@default)
      ("culture" "文化・芸能" "%s/" ,@default)
      ("english" "ENGLISH" "%s/"
       ,(concat
	 "<a" s1 "href=\"/"
	 ;; 1. url
	 "\\(%s/"
	 ;; 2. extra keyword
	 "\\([a-z]+\\)"
	 "/[a-z]+20[0-9][0-9]"
	 ;; 3. month
	 "\\([01][0-9]\\)"
	 ;; 4. day
	 "\\([0-3][0-9]\\)"
	 ;; 5. serial number
	 "\\([0-9]+\\)"
	 "\\.html\\)"
	 "\">" s0
	 ;; 6. subject
	 "\\(" no-nl "\\)"
	 s0 "</a>")
       1 5 nil 6 3 4 nil 2)
      ("international" "国際" "%s/" ,@default)
      ("kansai" "関西" "%s/" ,@default)
      ("kansai-special" "関西特集" "kansai/special/"
       ,(concat
	 "<a" s1 "href=\"/"
	 ;; 1. url
	 "\\(kansai/special/"
	 ;; 2. serial number
	 "\\([a-z]+[0-9]+\\)"
	 "\\.html\\)"
	 "\">" s0
	 ;; 3. subject
	 "\\(" no-nl "\\)"
	 s0 "</a>" s0 "([0-9]+/"
	 ;; 4. month
	 "\\([01][0-9]\\)"
	 "/"
	 ;; 5. day
	 "\\([0-3][0-9]\\))")
       1 nil 2 3 4 5)
      ("national" "社会" "%s/" ,@default)
      ("politics" "政治" "%s/" ,@default)
      ("science" "科学" "%s/" ,@default)
      ("sports" "スポーツ" "%s/"
       ,(concat
	 "<a" s1 "href=\"/"
	 ;; 1. url
	 "\\("
	 ;; 3 or 5. extra keyword
	 "\\(%s/\\([a-z]+\\)\\(/" no-nl "\\)*\\|\\([a-z]+\\)/" no-nl "\\)"
	 "/"
	 ;; 6. serial number
	 "\\(" no-nl "\\)"
	 "\\.html\\)"
	 "\">" s0
	 ;; 7. subject
	 "\\(" no-nl "\\)"
	 s0 "</a>[\t\n 　]*("
	 ;; 8. month
	 "\\([01][0-9]\\)"
	 "/"
	 ;; 9. day
	 "\\([0-3][0-9]\\)"
	 "\\(" s1
	 ;; 11. hour:minute
	 "\\([012][0-9]:[0-5][0-9]\\)"
	 "\\)?)")
       1 nil 6 7 8 9 11 3 5)
      ("nankyoku" "南極" "%s/news/index.html"
       ,(concat
	 "<a" s1 "href=\""
	 ;; 1. url
	 "\\(/%s/news/"
	 ;; 2. serial number
	 "\\([A-Z0-9]+\\)"
	 "\\.html\\)\">" s0
	 ;; 3. subject
	 "\\(" no-nl "\\)" s0 "</a>" s0
	 "[（(][0-9]+/"
	 ;; 4. month
	 "\\([01][0-9]\\)"
	 "/"
	 ;; 5. day
	 "\\([0-3][0-9]\\)"
	 "[)）]")
       1 nil 2 3 4 5)
      ("whitemail" "WhiteMail＠南極" "nankyoku/%s/"
       ,(concat
	 "<a" s1 "href=\""
	 ;; 1. url
	 "\\(/nankyoku/%s/"
	 ;; 2. serial number
	 "\\([A-Z0-9]+\\)"
	 "\\.html\\)\">" s0
	 ;; 3. subject
	 "\\(" no-nl "\\)" s0 "</a>" s0
	 "[（(][0-9]+/"
	 ;; 4. month
	 "\\([01][0-9]\\)"
	 "/"
	 ;; 5. day
	 "\\([0-3][0-9]\\)"
	 "[)）]")
       1 nil 2 3 4 5)))
  "Alist of group names, their Japanese translations, index pages,
regexps and numbers.  Where index pages and regexps may contain the
\"%s\" token which is replaced with group names, numbers point to the
search result in order of [0]a url, [1,2]a serial number, [3]a subject,
\[4]a month, [5]a day, [6]an hour:minute and [7,8]an extra keyword.")

(defvar shimbun-asahi-content-start
  "<!--[\t\n ]*FJZONE START NAME=\"HONBUN\"[\t\n ]*-->")

(defvar shimbun-asahi-content-end
  "<!--[\t\n ]*FJZONE END NAME=\"HONBUN\"[\t\n ]*-->")

(defvar shimbun-asahi-x-face-alist
  '(("default" . "X-Face: +Oh!C!EFfmR$+Zw{dwWW]1e_>S0rnNCA*CX|\
bIy3rr^<Q#lf&~ADU:X!t5t>gW5)Q]N{Mmn\n L]suPpL|gFjV{S|]a-:)\\FR\
7GRf9uL:ue5_=;h{V%@()={uTd@l?eXBppF%`6W%;h`#]2q+f*81n$B\n h|t")))

(defvar shimbun-asahi-expiration-days 6)

(luna-define-method initialize-instance :after ((shimbun shimbun-asahi)
						 &rest init-args)
  (shimbun-set-server-name-internal shimbun "朝日新聞")
  (shimbun-set-from-address-internal shimbun
				     (concat "webmaster@www."
					     shimbun-asahi-top-level-domain))
  ;; To share class variables between `shimbun-asahi' and its
  ;; successor `shimbun-asahi-html'.
  (shimbun-set-x-face-alist-internal shimbun shimbun-asahi-x-face-alist)
  (shimbun-set-expiration-days-internal shimbun shimbun-asahi-expiration-days)
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-asahi))
  (mapcar 'car shimbun-asahi-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-asahi))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-asahi-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-asahi))
  (let ((group (shimbun-current-group-internal shimbun)))
    (concat shimbun-asahi-url
	    (format (nth 2 (assoc group shimbun-asahi-group-table))
		    group))))

(defun shimbun-asahi-get-headers (shimbun)
  "Return a list of headers."
  (let ((group (shimbun-current-group-internal shimbun))
	(from (shimbun-from-address shimbun))
	(case-fold-search t)
	regexp numbers cyear cmonth month year day serial num extra headers
	kansai-special)
    (setq regexp (assoc group shimbun-asahi-group-table)
	  numbers (nthcdr 4 regexp)
	  regexp (format (nth 3 regexp) (regexp-quote group))
	  cyear (decode-time)
	  cmonth (nth 4 cyear)
	  cyear (nth 5 cyear))
    (while (re-search-forward regexp nil t)
      (when (string-equal group "kansai-special")
	(save-excursion
	  (save-match-data
	    (setq kansai-special
		  (if (re-search-backward ">[\t\n ]*\\([^<>]+\\)[\t\n ]*</th>"
					  nil t)
		      (match-string 1))))))
      (setq month (string-to-number (match-string (nth 4 numbers)))
	    year (cond ((and (= 12 month) (= 1 cmonth))
			(1- cyear))
		       ((and (= 1 month) (= 12 cmonth))
			(1+ cyear))
		       (t
			cyear))
	    day (string-to-number (match-string (nth 5 numbers)))
	    serial (if (and (setq num (nth 1 numbers))
			    (match-beginning num))
		       (format "%d%02d%02d.%s"
			       year month day (match-string num))
		     (mapconcat 'identity
				(save-match-data
				  (split-string
				   (downcase (match-string (nth 2 numbers)))
				   "/"))
				"."))
	    extra (or (and (setq num (nth 7 numbers))
			   (match-beginning num)
			   (match-string num))
		      (and (setq num (nth 8 numbers))
			   (match-beginning num)
			   (match-string num))))
      (push (shimbun-make-header
	     ;; number
	     0
	     ;; subject
	     (shimbun-mime-encode-string
	      (cond (kansai-special
		     (concat "[" kansai-special "] "
			     (match-string (nth 3 numbers))))
		    ((and (string-equal group "international")
			  (string-equal (substring serial
						   0 (min 7 (length serial)))
					"jinmin."))
		     (concat "[人民日報] " (match-string (nth 3 numbers))))
		    ((and (setq num (nth 7 numbers))
			  (match-beginning num))
		     (concat "[" (match-string num) "] "
			     (match-string (nth 3 numbers))))
		    ((and (setq num (nth 8 numbers))
			  (match-beginning num))
		     (concat "[" (match-string num) "] "
			     (match-string (nth 3 numbers))))
		    (t
		     (match-string (nth 3 numbers)))))
	     ;; from
	     from
	     ;; date
	     (shimbun-make-date-string
	      year month day (when (and (setq num (nth 6 numbers))
					(match-beginning num))
			       (match-string num)))
	     ;; id
	     (if extra
		 (concat "<" serial "%" extra "." group "."
			 shimbun-asahi-top-level-domain ">")
	       (concat "<" serial "%" group "."
		       shimbun-asahi-top-level-domain ">"))
	     ;; references, chars, lines
	     "" 0 0
	     ;; xref
	     (shimbun-expand-url (match-string (nth 0 numbers))
				 shimbun-asahi-url))
	    headers))
    (shimbun-sort-headers headers)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-asahi)
					 &optional range)
  (shimbun-asahi-get-headers shimbun))

(defun shimbun-asahi-adjust-date-header (shimbun header)
  "Adjust a date header if there is a correct information available."
  (let ((case-fold-search t)
	date start end)
    (when (and (member (shimbun-current-group-internal shimbun)
		       '("science"))
	       (string-match " \\(00:00\\) "
			     (setq date (shimbun-header-date header)))
	       (setq start (match-beginning 1))
	       (re-search-forward (shimbun-content-start-internal shimbun)
				  nil t)
	       (re-search-forward (shimbun-content-end-internal shimbun)
				  nil t)
	       (progn
		 (goto-char (setq end (match-beginning 0)))
		 (forward-line -1)
		 (re-search-forward
		  "([01][0-9]/[0-3][0-9] \\([012][0-9]:[0-5][0-9]\\))"
		  end t)))
      (shimbun-header-set-date header
			       (concat (substring date 0 start)
				       (match-string 1)
				       (substring date (+ start 5))))))
  (goto-char (point-min)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-asahi)
						   header)
  (shimbun-asahi-adjust-date-header shimbun header))

(provide 'sb-asahi)

;;; sb-asahi.el ends here

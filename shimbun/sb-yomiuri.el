;;; sb-yomiuri.el --- shimbun backend for www.yomiuri.co.jp -*- coding: iso-2022-7bit; -*-

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>
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

(luna-define-class shimbun-yomiuri (shimbun-text) ())

(defvar shimbun-yomiuri-top-level-domain "yomiuri.co.jp"
  "Name of the top level domain for the Yomiuri On-line.")

(defvar shimbun-yomiuri-url
  (concat "http://www." shimbun-yomiuri-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-yomiuri-group-table
  (let ((default
	  (list
	   (concat
	    "<[\t\n ]*a[\t\n ]+href[\t\n ]*=[\t\n ]*\"/"
	    ;; 1. url
	    "\\(%s"
	    "/news/200[0-9][01][0-9][0-3][0-9]"
	    ;; 2. serial number
	    "\\([0-9a-z]+\\)" "\\.htm\\)"
	    "\"[\t\n ]*>[\t\n ]*"
	    ;; 3. subject
	    "\\(.+\\)" "[\t\n ]*([\t\n ]*"
	    ;; 4. month
	    "\\([0-9]+\\)" "[\t\n ]*/[\t\n ]*"
	    ;; 5. day
	    "\\([0-9]+\\)" "[\t\n ]+"
	    ;; 6. hour:minute
	    "\\([012][0-9]:[0-5][0-9]\\)"
	    "[\t\n ]*)[\t\n ]*<[\t\n ]*/a[\t\n ]*>")
	   1 2 3 4 5 6)))
    `(("business" "経済面" "index.htm" ,@default)
      ("editorial" "社説・コラム" "index.htm"
       ,(concat "<[\t\n ]*a[\t\n ]+href[\t\n ]*=[\t\n ]*\"/"
		;; 1. url
		"\\(%s"
		"/news/200[0-9][01][0-9][0-3][0-9]"
		;; 2. serial number
		"\\([0-9a-z]+\\)" "\\.htm\\)"
		"\"[\t\n ]*>[\t\n ]*"
		;; 3. month(ja)
		"\\([０１]?[０-９]\\)" "[\t\n 　]*月[\t\n 　]*"
		;; 4. day(ja)
		"\\([０-３]?[０-９]\\)"
		"[\t\n 　]*日[\t\n 　]*付[\t\n 　・]*"
		;; 5. subject
		"\\(.+\\)" "[\t\n ]*<[\t\n ]*/a[\t\n ]*>")
       1 2 5 nil nil nil 3 4)
      ("national" "社会面" "index.htm" ,@default)
      ("obit" "おくやみ" "index.htm"
       ,(concat "<[\t\n ]*a[\t\n ]+href[\t\n ]*=[\t\n ]*\"/"
		;; 1. url
		"\\(%s"
		"/news/200[0-9][01][0-9][0-3][0-9]"
		;; 2. serial number
		"\\([0-9a-z]+\\)" "\\.htm\\)"
		"\"[\t\n ]*>[\t\n ]*"
		;; 3. subject
		"\\(.+\\)" "[\t\n ]*([\t\n ]*"
		;; 4. month
		"\\([0-9]+\\)" "[\t\n ]*/[\t\n ]*"
		;; 5. day
		"\\([0-9]+\\)" "[\t\n ]*"
		;; 6. hour:minute
		"\\([012][0-9]:[0-5][0-9]\\)?"
		"[\t\n ]*)[\t\n ]*<[\t\n ]*/a[\t\n ]*>")
       1 2 3 4 5 6)
      ("politics" "政治面" "index.htm" ,@default)
      ("sports" "スポーツ面" "index.htm" ,@default)
      ("world" "国際面" "index.htm" ,@default)))
  "Alist of group names, their Japanese translations, index pages,
regexps and numbers.
Regexp may have the \"%s\" token which is replaced with a
regexp-quoted group name.  Numbers point to the search result in order
of a url, a serial number, a subject, a month, a day, an hour:minute
and an extra keyword.")

(defvar shimbun-yomiuri-content-start "\n<!--  honbun start  -->\n")

(defvar shimbun-yomiuri-content-end  "\n<!--  honbun end  -->\n")

(defvar shimbun-yomiuri-x-face-alist
  '(("default" . "X-Face: #sUhc'&(fVr$~<rt#?PkH,u-.fV(>y)\
i\"#,TNF|j.dEh2dAzfa4=IH&llI]S<-\"dznMW2_j\n [N1a%n{SU&E&\
Ex;xlc)9`]D07rPEsbgyjP@\"_@g-kw!~TJNilrSC!<D|<m=%Uf2:eebg")))

(defvar shimbun-yomiuri-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-yomiuri))
  (mapcar 'car shimbun-yomiuri-group-table))

(luna-define-method shimbun-from-address ((shimbun shimbun-yomiuri))
  (concat (shimbun-mime-encode-string
	   (concat "読売新聞 ("
		   (nth 1 (assoc (shimbun-current-group-internal shimbun)
				 shimbun-yomiuri-group-table))
		   ")"))
	  " <webmaster@www." shimbun-yomiuri-top-level-domain ">"))

(luna-define-method shimbun-index-url ((shimbun shimbun-yomiuri))
  (let ((group (shimbun-current-group-internal shimbun)))
    (concat shimbun-yomiuri-url group "/"
	    (nth 2 (assoc group shimbun-yomiuri-group-table)))))

(defmacro shimbun-yomiuri-japanese-string-to-number (string)
  "Convert a Japanese zenkaku number to just a number."
  (let ((alist ''((?０ . 0) (?１ . 1) (?２ . 2) (?３ . 3) (?４ . 4)
		  (?５ . 5) (?６ . 6) (?７ . 7) (?８ . 8) (?９ . 9))))
    (if (= (length "０") 1)
	`(let* ((str ,string)
		(alist ,alist)
		(len (length str))
		(idx 0)
		(num 0))
	   (while (< idx len)
	     (setq num (+ (cdr (assq (aref str idx) alist)) (* num 10))
		   idx (1+ idx)))
	   num)
      `(let* ((str ,string)
	      (alist ,alist)
	      (len (length str))
	      (idx 0)
	      (num 0)
	      char)
	 (while (< idx len)
	   (setq char (sref str idx)
		 num (+ (cdr (assq char alist)) (* num 10))
		 idx (+ idx (char-bytes char))))
	 num))))

(defun shimbun-yomiuri-shorten-brackets-in-string (string)
  "Replace Japanes zenkaku brackets with ascii characters in STRING.
It does also shorten too much spaces."
  (with-temp-buffer
    (insert string)
    (let ((alist '(("（" . " (") ("）" . ") ") ("［" . " [")
		   ("］" . "] ") ("｛" . " {") ("｝" . "} ")))
	  elem)
      (while alist
	(setq elem (pop alist))
	(goto-char (point-min))
	(while (search-forward (car elem) nil t)
	  (replace-match (cdr elem))))
      (goto-char (point-min))
      (while (re-search-forward "[\t 　]+" nil t)
	(replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward "\\([])}]\\) \\([])}]\\)" nil t)
	(replace-match "\\1\\2")
	(forward-char -1))
      (goto-char (point-min))
      (while (re-search-forward "\\([[({]\\) \\([[({]\\)" nil t)
	(replace-match "\\1\\2")
	(forward-char -1))
      (goto-char (point-min))
      (while (re-search-forward " ?\\([「」]\\) ?" nil t)
	(replace-match "\\1"))
      (goto-char (point-min))
      (while (re-search-forward "^ \\| $" nil t)
	(replace-match "")))
    (buffer-string)))

(defun shimbun-yomiuri-get-headers (shimbun)
  "Return a list of headers."
  (let ((group (shimbun-current-group-internal shimbun))
	(from (shimbun-from-address shimbun))
	(case-fold-search t)
	regexp numbers cyear cmonth month year day headers)
    (setq regexp (assoc group shimbun-yomiuri-group-table)
	  numbers (nthcdr 4 regexp)
	  regexp (format (nth 3 regexp) (regexp-quote group))
	  cyear (decode-time)
	  cmonth (nth 4 cyear)
	  cyear (nth 5 cyear))
    (while (re-search-forward regexp nil t)
      (setq month (cond ((nth 3 numbers)
			 (string-to-number (match-string (nth 3 numbers))))
			((nth 6 numbers)
			 (shimbun-yomiuri-japanese-string-to-number
			  (match-string (nth 6 numbers))))
			(t
			 1))
	    year (cond ((and (= 12 month) (= 1 cmonth))
			(1- cyear))
		       ((and (= 1 month) (= 12 cmonth))
			(1+ cyear))
		       (t
			cyear))
	    day (cond ((nth 4 numbers)
		       (string-to-number (match-string (nth 4 numbers))))
		      ((nth 7 numbers)
		       (shimbun-yomiuri-japanese-string-to-number
			(match-string (nth 7 numbers))))
		      (t
		       1)))
      (push (shimbun-make-header
	     ;; number
	     0
	     ;; subject
	     (if (and (nth 6 numbers) (nth 7 numbers))
		 (save-match-data
		   (shimbun-mime-encode-string
		    (format "%02d/%02d %s"
			    month day
			    (shimbun-yomiuri-shorten-brackets-in-string
			     (match-string (nth 2 numbers))))))
	       (save-match-data
		 (shimbun-mime-encode-string (match-string (nth 2 numbers)))))
	     ;; from
	     from
	     ;; date
	     (shimbun-make-date-string year month day
				       (when (nth 5 numbers)
					 (match-string (nth 5 numbers))))
	     ;; id
	     (format "<%d%02d%02d.%s%%%s.%s>"
		     year month day (match-string (nth 1 numbers)) group
		     shimbun-yomiuri-top-level-domain)
	     ;; references, chars, lines
	     "" 0 0
	     ;; xref
	     (concat shimbun-yomiuri-url (match-string (nth 0 numbers))))
	    headers))
    headers))

(luna-define-method shimbun-get-headers ((shimbun shimbun-yomiuri)
					 &optional range)
  (shimbun-yomiuri-get-headers shimbun))

(defun shimbun-yomiuri-adjust-date-header (shimbun header)
  "Adjust a date header if there is a correct information available."
  (let ((case-fold-search t)
	end)
    (when (and (re-search-forward (shimbun-content-start-internal shimbun)
				  nil t)
	       (re-search-forward (shimbun-content-end-internal shimbun)
				  nil t)
	       (progn
		 (goto-char (setq end (match-beginning 0)))
		 (forward-line -1)
		 (re-search-forward "\\(20[0-9][0-9]\\)/\\(1?[0-9]\\)/\
\\([123]?[0-9]\\)/\\([012][0-9]:[0-5][0-9]\\)"
				    end t)))
      (shimbun-header-set-date
       header
       (shimbun-make-date-string
	(string-to-number (match-string 1))
	(string-to-number (match-string 2))
	(string-to-number (match-string 3))
	(match-string 4)))))
  (goto-char (point-min)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-yomiuri)
						   header)
  (shimbun-yomiuri-adjust-date-header shimbun header))

(provide 'sb-yomiuri)

;;; sb-yomiuri.el ends here

;;; sb-yomiuri.el --- shimbun backend for www.yomiuri.co.jp -*- coding: iso-2022-7bit; -*-

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

(luna-define-class shimbun-yomiuri
		   (shimbun-japanese-newspaper shimbun-text) ())

(defvar shimbun-yomiuri-top-level-domain "yomiuri.co.jp"
  "Name of the top level domain for the Yomiuri On-line.")

(defvar shimbun-yomiuri-url
  (concat "http://www." shimbun-yomiuri-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-yomiuri-group-table
  (let* ((s0 "[\t\n ]*")
	 (s1 "[\t\n ]+")
	 (no-nl "[^\n<>]+")
	 (default
	   (list
	    (concat
	     "<a" s1 "href=\"/"
	     ;; 1. url
	     "\\(%s/news/"
	     ;; 2,3. serial number
	     "\\(20[0-9][0-9][01][0-9][0-3][0-9]\\)\\([0-9a-z]+\\)"
	     "\\.htm\\)\">" s0
	     ;; 4. subject
	     "\\(" no-nl "\\)"
	     s0 "("
	     ;; 5. month
	     "\\([01]?[0-9]\\)"
	     "/"
	     ;; 6. day
	     "\\([0-3]?[0-9]\\)"
	     s1
	     ;; 7. hour:minute
	     "\\([012][0-9]:[0-5][0-9]\\)"
	     ")" s0 "</a>")
	    1 2 3 4 5 6 7)))
    `(("business" "経済" "index.htm" ,@default)
      ("culture" "芸能・文化" "index.htm" ,@default)
      ("editorial" "社説・コラム" "index.htm"
       ,(concat "<a" s1 "href=\"/"
		;; 1. url
		"\\(%s/news/"
		;; 2,3. serial number
		"\\(20[0-9][0-9][01][0-9][0-3][0-9]\\)\\([0-9a-z]+\\)"
		"\\.htm\\)"
		"\">" s0
		;; 4. month(ja)
		"\\([０１]?[０-９]\\)"
		"月"
		;; 5. day(ja)
		"\\([０-３]?[０-９]\\)"
		"日付・"
		;; 6. subject
		"\\(" no-nl "\\)" s0 "</a>")
       1 2 3 6 4 5)
      ("kyoiku" "教育メール" "index.htm"
       ,(concat "<a" s1 "href=\"/"
		;; 1. url
		"\\(%s/\\([^\"/]+/\\)+"
		;; 3,4. serial number
		"\\(20[0-9][0-9][01][0-9][0-3][0-9]\\)\\([0-9a-z]+\\)"
		"\\.htm\\)"
		"\"[^>]+>" s0
		;; 5. subject
		"\\([^<]+\\)"
		"\\(" s0 "<[^>]+>\\)+" s0
		;; 7. month
		"\\([01]?[0-9]\\)"
		"月\\(" s0 "<[^>]+>\\)+" s0
		;; 9 day
		"\\([0-3]?[0-9]\\)"
		"日")
       1 3 4 5 7 9)
      ("national" "社会" "index.htm" ,@default)
      ("obit" "おくやみ" "index.htm"
       ,(concat "<a" s1 "href=\"/"
		;; 1. url
		"\\(%s/news/"
		;; 2,3. serial number
		"\\(20[0-9][0-9][01][0-9][0-3][0-9]\\)\\([0-9a-z]+\\)"
		"\\.htm\\)"
		"\">" s0
		;; 4. subject
		"\\(" no-nl "\\)"
		s0 "("
		;; 5. month
		"\\([01]?[0-9]\\)"
		"/"
		;; 6. day
		"\\([0-3]?[0-9]\\)"
		")" s0 "</a>")
       1 2 3 4 5 6)
      ("politics" "政治" "index.htm" ,@default)
      ("science" "科学" "index.htm" ,@default)
      ("sports" "スポーツ" "index.htm" ,@default)
      ("world" "国際" "index.htm" ,@default)))
  "Alist of group names, their Japanese translations, index pages,
regexps and numbers.
Regexp may contain the \"%s\" token which is replaced with a
regexp-quoted group name.  Numbers point to the search result in order
of [0]a url, [1,2]serial numbers, [3]a subject, [4]a month, [5]a day,
\[6]an hour:minute and [7-]extra keywords.")

(defvar shimbun-yomiuri-content-start "\n<!--  honbun start  -->\n")

(defvar shimbun-yomiuri-content-end  "\n<!--  honbun end  -->\n")

(defvar shimbun-yomiuri-x-face-alist
  '(("default" . "X-Face: #sUhc'&(fVr$~<rt#?PkH,u-.fV(>y)\
i\"#,TNF|j.dEh2dAzfa4=IH&llI]S<-\"dznMW2_j\n [N1a%n{SU&E&\
Ex;xlc)9`]D07rPEsbgyjP@\"_@g-kw!~TJNilrSC!<D|<m=%Uf2:eebg")))

(defvar shimbun-yomiuri-expiration-days 7)

(luna-define-method initialize-instance :after ((shimbun shimbun-yomiuri)
						 &rest init-args)
  (shimbun-set-server-name-internal shimbun "讀売新聞")
  (shimbun-set-from-address-internal shimbun
				     (concat "webmaster@www."
					     shimbun-yomiuri-top-level-domain))
  ;; To share class variables between `shimbun-yomiuri' and its
  ;; successor `shimbun-yomiuri-html'.
  (shimbun-set-x-face-alist-internal shimbun shimbun-yomiuri-x-face-alist)
  (shimbun-set-expiration-days-internal shimbun
					shimbun-yomiuri-expiration-days)
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-yomiuri))
  (mapcar 'car shimbun-yomiuri-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-yomiuri))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-yomiuri-group-table)))

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
  (save-match-data
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
      (buffer-string))))

(defun shimbun-yomiuri-get-headers (shimbun)
  "Return a list of headers."
  (let ((group (shimbun-current-group-internal shimbun))
	(from (shimbun-from-address shimbun))
	(case-fold-search t)
	cyear cmonth month day time regexp numbers headers)
    (setq cyear (decode-time)
	  cmonth (nth 4 cyear)
	  cyear (nth 5 cyear))
    ;; Extracting top news.
    (when (and (not (member group '("editorial" "kyoiku" "obit")))
	       (re-search-forward
		(format
		 (eval-when-compile
		   (concat
		    "<a[\t\n ]+href=\"/"
		    ;; 1. url
		    "\\(%s/news/"
		    ;; 2. serial number
		    "\\(20[0-9][0-9]"
		    ;; 3. month
		    "\\([01][0-9]\\)"
		    ;; 4. day
		    "\\([0-3][0-9]\\)"
		    "[0-9a-z]+\\)"
		    "\\.htm\\)"
		    "\">[\t\n ]*\\(<[^<>]+>[\t\n ]*\\)+"
		    ;; 6. subject
		    "\\([^<>]+\\)"
		    "[\t\n ]*<"))
		 (regexp-quote group))
		nil t))
      (setq month (string-to-number (match-string 3))
	    day (string-to-number (match-string 4)))
      (save-match-data
	(when (re-search-forward
	       (concat "(\\([01]?[0-9]\\)/\\([0-3]?[0-9]\\)[\t\n ]+\
\\([012][0-9]:[0-5][0-9]\\))[\t\n ]+<a[\t\n ]+href=\"/"
		       (regexp-quote (match-string 1)) "\"")
	       nil t)
	  (setq month (string-to-number (match-string 1))
		day (string-to-number (match-string 2))
		time (match-string 3))))
      (push (shimbun-create-header
	     ;; number
	     0
	     ;; subject
	     (match-string 6)
	     ;; from
	     from
	     ;; date
	     (shimbun-make-date-string (cond ((>= (- month cmonth) 2)
					      (1- cyear))
					     ((and (= 1 month) (= 12 cmonth))
					      (1+ cyear))
					     (t
					      cyear))
				       month day time)
	     ;; id
	     (concat "<" (buffer-substring (match-beginning 2)
					   (match-end 4))
		     "." (buffer-substring (match-end 4)
					   (match-end 2))
		     "%" group "." shimbun-yomiuri-top-level-domain ">")
	     ;; references, chars, lines
	     "" 0 0
	     ;; xref
	     (concat shimbun-yomiuri-url (match-string 1)))
	    headers))
    (setq regexp (assoc group shimbun-yomiuri-group-table)
	  numbers (nthcdr 4 regexp)
	  regexp (format (nth 3 regexp) (regexp-quote group)))
    ;; Generating headers.
    (while (re-search-forward regexp nil t)
      (if (string-equal group "editorial")
	  (setq month (shimbun-yomiuri-japanese-string-to-number
		       (match-string (nth 4 numbers)))
		day (shimbun-yomiuri-japanese-string-to-number
		     (match-string (nth 5 numbers))))
	(setq month (string-to-number (match-string (nth 4 numbers)))
	      day (string-to-number (match-string (nth 5 numbers)))))
      (push (shimbun-make-header
	     ;; number
	     0
	     ;; subject
	     (if (string-equal "editorial" group)
		 (shimbun-mime-encode-string
		  (format "%02d/%02d %s"
			  month day
			  (shimbun-yomiuri-shorten-brackets-in-string
			   (match-string (nth 3 numbers)))))
	       (shimbun-mime-encode-string (match-string (nth 3 numbers))))
	     ;; from
	     from
	     ;; date
	     (shimbun-make-date-string (cond ((>= (- month cmonth) 2)
					      (1- cyear))
					     ((and (= 1 month) (= 12 cmonth))
					      (1+ cyear))
					     (t
					      cyear))
				       month day
				       (when (nth 6 numbers)
					 (match-string (nth 6 numbers))))
	     ;; id
	     (concat "<" (match-string (nth 1 numbers))
		     "." (match-string (nth 2 numbers))
		     "%" group "." shimbun-yomiuri-top-level-domain ">")
	     ;; references, chars, lines
	     "" 0 0
	     ;; xref
	     (concat shimbun-yomiuri-url (match-string (nth 0 numbers))))
	    headers))
    (shimbun-sort-headers headers)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-yomiuri)
					 &optional range)
  (shimbun-yomiuri-get-headers shimbun))

(defun shimbun-yomiuri-prepare-article (shimbun header)
  "Prepare an article: adjusting a date header if there is a correct
information available, removing useless contents, etc."
  (let ((group (shimbun-current-group-internal shimbun))
	(case-fold-search t)
	start)
    (if (string-equal group "kyoiku")
	(progn
	  (when (or (re-search-forward "\
<!--[\t\n ]*▼写真▼[\t\n ]*-->[\t\n ]*"
				       nil t)
		    (re-search-forward "\
<!--[\t\n ]*InstanceBeginEditable[\t\n ]+name=\"docbody\"[\t\n ]*-->[\t\n ]*"
				       nil t))
	    (delete-region (point-min) (point)))
	  (when (or (re-search-forward
		     "[\t\n ]*<!--[\t\n ]*▲日付▲[\t\n ]*-->"
		     nil t)
		    (re-search-forward "[\t\n ]*\
<div[\t\n ]+.+</div>[\t\n ]*<!--[\t\n ]*▼フッタ−ナビ▼[\t\n ]*-->"
				       nil t)
		    (re-search-forward
		     "[\t\n ]*<!--[\t\n ]*InstanceEndEditable[\t\n ]*-->"
		     nil t))
	    (delete-region (match-beginning 0) (point-max)))
	  (goto-char (point-min))
	  (when (re-search-forward "\
<div[\t\n ]+class=\"enlargedphoto\">\\([\t\n ]*写真の拡大[\t\n ]*\
<img[\t\n ]*src=[^>]+>[\t\n ]*\\)"
				   nil t)
	    (delete-region (match-beginning 1) (match-end 1))))
      (when (and (re-search-forward (shimbun-content-start-internal shimbun)
				    nil t)
		 (setq start (point))
		 (re-search-forward (shimbun-content-end-internal shimbun)
				    nil t))
	(narrow-to-region start (match-beginning 0))
	(goto-char (point-max))
	(forward-line -1)
	(when (re-search-forward "\\(20[0-9][0-9]\\)/\\(1?[0-9]\\)/\
\\([123]?[0-9]\\)/\\([012][0-9]:[0-5][0-9]\\)"
				 nil t)
	  (shimbun-header-set-date
	   header
	   (shimbun-make-date-string
	    (string-to-number (match-string 1))
	    (string-to-number (match-string 2))
	    (string-to-number (match-string 3))
	    (match-string 4))))
	(goto-char (point-min))
	(shimbun-remove-tags "<!--  rectangle start  -->"
			     "<!--  rectangle end  -->")
	;; Break continuous lines.
	(goto-char (point-min))
	(when (and (string-equal group "editorial")
		   (string-match " \\(よみうり寸評\\|編集手帳\\)\\'"
				 (shimbun-header-subject header 'no-encode)))
	  (goto-char (point-min))
	  (while (search-forward "◆" nil t)
	    (replace-match "。<br>\\&<br>")))
	(widen))))
  (goto-char (point-min)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-yomiuri)
						   header)
  (shimbun-yomiuri-prepare-article shimbun header))

(provide 'sb-yomiuri)

;;; sb-yomiuri.el ends here

;;; shimbun.el --- interfacing with web newspapers

;; Copyright (C) 2001 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Akihiro Arisawa    <ari@mbf.sphere.ne.jp>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>
;; Keywords: news

;; This file is the main part of shimbun.

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

;; Shimbun API:
;;
;; shimbun-open
;; shimbun-groups
;; shimbun-open-group
;; shimbun-close-group
;; shimbun-headers
;; shimbun-reply-to
;; shimbun-x-face
;; shimbun-header-insert
;; shimbun-search-id
;; shimbun-article-expiration-days
;; shimbun-article
;; shimbun-close

;; Shimbun Header API:
;;
;; shimbun-header-subject
;; shimbun-header-set-subject
;; shimbun-header-from
;; shimbun-header-set-from
;; shimbun-header-date
;; shimbun-header-set-date
;; shimbun-header-id
;; shimbun-header-set-id
;; shimbun-header-references
;; shimbun-header-set-references
;; shimbun-header-chars
;; shimbun-header-set-chars
;; shimbun-header-lines
;; shimbun-header-set-lines
;; shimbun-header-xref
;; shimbun-header-set-xref
;; shimbun-header-extra
;; shimbun-header-set-extra

(eval-when-compile (require 'cl))
(eval-when-compile (require 'static))

(require 'mcharset)
(require 'eword-encode)
(require 'luna)
(require 'std11)

(eval-and-compile
  (luna-define-class shimbun ()
		     (mua server current-group groups
			  x-face x-face-alist
			  url coding-system from-address
			  content-start content-end
			  expiration-days))
  (luna-define-internal-accessors 'shimbun))

(defgroup shimbun nil
  "shimbun - the backend library to read web newspapers."
  :group 'w3m
  :group 'hypermedia)

(defcustom shimbun-x-face
  "X-Face: Ygq$6P.,%Xt$U)DS)cRY@k$VkW!7(X'X'?U{{osjjFG\"E]hND;SPJ-J?O?R\
|a?Lg2$0rVng\n =O3Lt}?~IId8Jj&vP^3*o=LKUyk(`t%0c!;t6REk=JbpsEn9MrN7gZ%"
  "*Default X-Face field for shimbun."
  :group 'shimbun
  :type 'string)

;;; Shimbun MUA
(eval-and-compile
  (luna-define-class shimbun-mua () (shimbun))
  (luna-define-internal-accessors 'shimbun-mua))

(luna-define-generic shimbun-mua-search-id (mua id)
  "Return non-nil when MUA found a message structure which corresponds to ID.")

;;; BASE 64
(require 'mel)
(eval-and-compile
  (fset 'shimbun-base64-encode-string
	(mel-find-function 'mime-encode-string "base64")))

;;; emacs-w3m implementation of url retrieval and entity decoding.
(require 'w3m)
(defun shimbun-retrieve-url (url &optional no-cache no-decode)
  "Rertrieve URL contents and insert to current buffer.
Return content-type of URL as string when retrieval succeeded."
  (let (type)
    (when (and url (setq type (w3m-retrieve url no-decode no-cache)))
      (unless no-decode
	(w3m-decode-buffer url))
      type)))

(defun shimbun-retrieve-url-buffer (url &optional no-cache no-decode)
  "Return a buffer which contains the URL contents."
  (with-current-buffer (get-buffer-create " *shimbun-work*")
    (erase-buffer)
    (if (and url (w3m-retrieve url no-decode no-cache))
	(inline
	  (unless no-decode
	    (w3m-decode-buffer url))
	  (current-buffer))
      (delete-region (point-min) (point-max))
      (current-buffer))))

(defalias 'shimbun-content-type 'w3m-content-type)
(defsubst shimbun-url-exists-p (url &optional no-cache)
  (string= "text/html"
	   (shimbun-content-type url no-cache)))

(defalias 'shimbun-decode-entities 'w3m-decode-entities)
(defalias 'shimbun-expand-url 'w3m-expand-url)

;;; Implementation of Header API.
(defun shimbun-make-header (&optional number subject from date id
				      references chars lines xref
				      extra)
  (vector number subject from date id references chars lines xref extra))

;;(defsubst shimbun-header-number (header)
;;  (aref header 0))

(defsubst shimbun-header-field-value ()
  (let ((pt (point)))
    (prog1
	(buffer-substring (match-end 0) (std11-field-end))
      (goto-char pt))))

(defsubst shimbun-header-subject (header)
  (aref header 1))

(defsubst shimbun-header-set-subject (header subject)
  (aset header 1 subject))

(defsubst shimbun-header-from (header)
  (aref header 2))

(defsubst shimbun-header-set-from (header from)
  (aset header 2 from))

(defsubst shimbun-header-date (header)
  (aref header 3))

(defsubst shimbun-header-set-date (header date)
  (aset header 3 date))

(defsubst shimbun-header-id (header)
  (aref header 4))

(defsubst shimbun-header-set-id (header id)
  (aset header 4 id))

(defsubst shimbun-header-references (header)
  (aref header 5))

(defsubst shimbun-header-set-references (header references)
  (aset header 5 references))

(defsubst shimbun-header-chars (header)
  (aref header 6))

(defsubst shimbun-header-set-chars (header chars)
  (aset header 6 chars))

(defsubst shimbun-header-lines (header)
  (aref header 7))

(defsubst shimbun-header-set-lines (header lines)
  (aset header 7 lines))

(defsubst shimbun-header-xref (header)
  (aref header 8))

(defsubst shimbun-header-set-xref (header xref)
  (aset header 8 xref))

(defsubst shimbun-header-extra (header)
  (aref header 9))

(defsubst shimbun-header-set-extra (header extra)
  (aset header 9 extra))

;; Inline functions for the internal use.
(defsubst shimbun-article-url (shimbun header)
  "Return URL string from SHIMBUN and HEADER."
  (if (and (shimbun-header-xref header)
	   (eq (aref (shimbun-header-xref header) 0) ?/))
      (concat (shimbun-url-internal shimbun)
	      (shimbun-header-xref header))
    (shimbun-header-xref header)))

(defcustom shimbun-encapsulate-images t
  "*If non-nil, inline images will be encapsulated in the articles.
Generated article have a multipart/related content-type."
  :group 'shimbun
  :type 'boolean)

(defun shimbun-make-mime-article (shimbun header)
  "Make a MIME article according to SHIMBUN and HEADER.
If article have inline images, generated article have a multipart/related
content-type if `shimbun-encapsulate-images' is non-nil."
  (let ((case-fold-search t)
	(count 0)
	beg end
	url type img imgs boundary charset)
    (current-buffer)
    (setq charset
	  (upcase (symbol-name
		   (detect-mime-charset-region (point-min)(point-max)))))
    (goto-char (point-min))
    (when shimbun-encapsulate-images
      (while (re-search-forward "<img" nil t)
	(setq beg (point))
	(when (search-forward ">" nil t)
	  (setq end (point))
	  (goto-char beg)
	  (when (re-search-forward
		 "src[ \t\r\f\n]*=[ \t\r\f\n]*\"\\([^\"]*\\)\"" end t)
	    (save-match-data
	      (setq url (match-string 1))
	      (unless (setq img (assoc url imgs))
		(setq imgs (cons
			    (setq img (list
				       url
				       (concat (format "shimbun.%d"
						       (incf count)))
				       (with-temp-buffer
					 (set-buffer-multibyte nil)
					 (setq
					  type
					  (shimbun-retrieve-url
					   (shimbun-expand-url
					    url
					    (shimbun-header-xref header))
					   'no-cache 'no-decode))
					 (buffer-string))
				       type))
			    imgs))))
	    (replace-match (concat "src=\"cid:" (nth 1 img) "\"")))))
      (setq imgs (nreverse imgs)))
    (goto-char (point-min))
    (shimbun-header-insert shimbun header)
    (if imgs
	(progn
	  (setq boundary (apply 'format "===shimbun_%d_%d_%d==="
				(current-time)))
	  (insert "Content-Type: multipart/related; type=\"text/html\"; "
		  "boundary=\"" boundary "\"; start=\"<shimbun.0>\""
		  "\nMIME-Version: 1.0\n\n"
		  "--" boundary
		  "\nContent-Type: text/html; charset=" charset
		  "\nContent-ID: <shimbun.0>\n\n"))
      (insert "Content-Type: text/html; charset=" charset "\n"
	      "MIME-Version: 1.0\n\n"))
    (encode-coding-region (point-min) (point-max)
			  (mime-charset-to-coding-system charset))
    (goto-char (point-max))
    (dolist (img imgs)
      (unless (eq (char-before) ?\n) (insert "\n"))
      (insert "--" boundary "\n"
	      "Content-Type: " (or (nth 3 img) "application/octed-stream")
	      "\nContent-Disposition: inline"
	      "\nContent-ID: <" (nth 1 img) ">"
	      "\nContent-Transfer-Encoding: base64"
	      "\n\n"
	      (shimbun-base64-encode-string (nth 2 img))))
    (when imgs
      (unless (eq (char-before) ?\n) (insert "\n"))
      (insert "--" boundary "--\n"))))

(defsubst shimbun-make-html-contents (shimbun header)
  (let ((case-fold-search t)
	start)
    (when (and (re-search-forward (shimbun-content-start-internal shimbun)
				  nil t)
	       (setq start (point))
	       (re-search-forward (shimbun-content-end-internal shimbun)
				  nil t))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start))
    (shimbun-make-mime-article shimbun header)
    (buffer-string)))

(eval-when-compile
  ;; Attempt to pick up the inline function `bbdb-search-simple'.
  (condition-case nil
      (require 'bbdb)
    (error
     (autoload 'bbdb-search-simple "bbdb")
     (autoload 'bbdb-get-field "bbdb"))))

(defcustom shimbun-use-bbdb-for-x-face nil
  "*Say whether to use BBDB for choosing the author's X-Face.  It will be
set to t when the initial value is nil and BBDB is loaded.  You can
set this to `never' if you never want to use BBDB."
  :group 'shimbun
  :type '(choice (const :tag "Default" nil)
		 (const :tag "Use BBDB" t)
		 (const :tag "No use BBDB" never)))

(defun shimbun-header-insert (shimbun header)
  (let ((from (shimbun-header-from header))
	(refs (shimbun-header-references header))
	x-face)
    (insert "Subject: " (or (shimbun-header-subject header) "(none)") "\n"
	    "From: " (or from "(nobody)") "\n"
	    "Date: " (or (shimbun-header-date header) "") "\n"
	    "Message-ID: " (shimbun-header-id header) "\n")
    (when (and refs
	       (string< "" refs))
      (insert "References: " refs "\n"))
    (insert "Lines: " (number-to-string (or (shimbun-header-lines header) 0))
	    "\n"
	    "Xref: " (or (shimbun-article-url shimbun header) "") "\n")
    (unless shimbun-use-bbdb-for-x-face
      (when (and (fboundp 'bbdb-get-field)
		 (not (eq 'autoload
			  (car-safe (symbol-function 'bbdb-get-field)))))
	(setq shimbun-use-bbdb-for-x-face t)))
    (when (setq x-face
		(or (and (eq t shimbun-use-bbdb-for-x-face)
			 from
			 ;; The library "mail-extr" will be autoload'ed.
			 (setq from
			       (cadr (mail-extract-address-components from)))
			 (setq x-face (bbdb-search-simple nil from))
			 (setq x-face (bbdb-get-field x-face 'face))
			 (not (zerop (length x-face)))
			 (concat "X-Face: "
				 (mapconcat 'identity
					    (split-string x-face)
					    "\nX-Face: ")))
		    (shimbun-x-face shimbun)))
      (insert x-face)
      (unless (bolp)
	(insert "\n")))))

;;; Implementation of Shimbun API.

(defconst shimbun-attributes
  '(url groups coding-system from-address content-start content-end
	x-face-alist expiration-days))

(defun shimbun-open (server &optional mua)
  "Open a shimbun for SERVER.
Optional MUA is a `shimbun-mua' instance."
  (require (intern (concat "sb-" server)))
  (let (url groups coding-system from-address content-start content-end
	    x-face-alist shimbun expiration-days)
    (dolist (attr shimbun-attributes)
      (set attr
	   (symbol-value (intern-soft
			  (concat "shimbun-" server "-" (symbol-name attr))))))
    (setq shimbun (luna-make-entity (intern (concat "shimbun-" server))
				    :mua mua
				    :server server
				    :url url
				    :groups groups
				    :coding-system coding-system
				    :from-address from-address
				    :content-start content-start
				    :content-end content-end
				    :expiration-days expiration-days
				    :x-face-alist x-face-alist))
    (when mua
      (shimbun-mua-set-shimbun-internal mua shimbun))
    shimbun))

(luna-define-generic shimbun-groups (shimbun)
  "Return a list of groups which are available in the SHIMBUN.")

(luna-define-method shimbun-groups ((shimbun shimbun))
  (shimbun-groups-internal shimbun))

(defun shimbun-open-group (shimbun group)
  "Open a SHIMBUN GROUP."
  (if (member group (shimbun-groups shimbun))
      (shimbun-set-current-group-internal shimbun group)
    (error "No such group %s" group)))

(defun shimbun-close-group (shimbun)
  "Close opened group of SHIMBUN."
  (when (shimbun-current-group-internal shimbun)
    (shimbun-set-current-group-internal shimbun nil)))

(luna-define-generic shimbun-headers (shimbun &optional range)
  "Return a SHIMBUN header list.
Optional argument RANGE is one of following:
nil or `all': Retrieve all header indices.
`last':       Retrieve the last header index.
integer n:    Retrieve n pages of header indices.")

(defmacro shimbun-header-index-pages (range)
  "Return number of pages to retrieve according to RANGE.
Return nil if all pages should be retrieved."
  (` (if (eq 'last (, range)) 1
       (if (eq 'all (, range)) nil
	 (, range)))))

(luna-define-method shimbun-headers ((shimbun shimbun) &optional range)
  (with-current-buffer (shimbun-retrieve-url-buffer
			(shimbun-index-url shimbun) 'reload)
    (shimbun-get-headers shimbun range)))

(luna-define-generic shimbun-reply-to (shimbun)
  "Return a reply-to field body for SHIMBUN.")

(luna-define-method shimbun-reply-to ((shimbun shimbun))
  nil)

(luna-define-generic shimbun-x-face (shimbun)
  "Return a X-Face field string for SHIMBUN.")

(luna-define-method shimbun-x-face ((shimbun shimbun))
  (or (shimbun-x-face-internal shimbun)
      (shimbun-set-x-face-internal
       shimbun
       (or (cdr (assoc (shimbun-current-group-internal shimbun)
		       (shimbun-x-face-alist-internal shimbun)))
	   (cdr (assoc "default" (shimbun-x-face-alist-internal shimbun)))
	   shimbun-x-face))))

(defun shimbun-search-id (shimbun id)
  "Return non-nil when MUA found a message structure which corresponds to ID."
  (when (shimbun-mua-internal shimbun)
    (shimbun-mua-search-id (shimbun-mua-internal shimbun) id)))

(defun shimbun-article-expiration-days (shimbun)
  "Return an expiration day number of SHIMBUN.
Return nil when articles are not expired."
  (shimbun-expiration-days-internal shimbun))

(luna-define-generic shimbun-article (shimbun header &optional outbuf)
  "Retrieve a SHIMBUN article which corresponds to HEADER to the OUTBUF.
HEADER is a shimbun-header which is obtained by `shimbun-headers'.
If OUTBUF is not specified, article is retrieved to the current buffer.")

(luna-define-method shimbun-article ((shimbun shimbun) header &optional outbuf)
  (when (shimbun-current-group-internal shimbun)
    (with-current-buffer (or outbuf (current-buffer))
      (insert
       (or (with-temp-buffer
	     (shimbun-retrieve-url (shimbun-article-url shimbun header))
	     (message "shimbun: Make contents...")
	     (goto-char (point-min))
	     (prog1 (shimbun-make-contents shimbun header)
	       (message "shimbun: Make contents...done")))
	   "")))))

(luna-define-generic shimbun-make-contents (shimbun header)
  "Return a content string of SHIMBUN article using current buffer content.
HEADER is a header structure obtained via `shimbun-headers'.")

(luna-define-method shimbun-make-contents ((shimbun shimbun) header)
  (shimbun-make-html-contents shimbun header))

(luna-define-generic shimbun-index-url (shimbun)
  "Return a index URL of SHIMBUN.")

(luna-define-generic shimbun-get-headers (shimbun &optional range)
  "Return a shimbun header list of SHIMBUN.
Optional argument RANGE is one of following:
nil or `all': Retrieve all header indices.
`last':       Retrieve the last header index.
integer n:    Retrieve n pages of header indices.")

(luna-define-generic shimbun-close (shimbun)
  "Close a SHIMBUN.")

(luna-define-method shimbun-close ((shimbun shimbun))
  (shimbun-close-group shimbun))

;;; Misc Functions
(defun shimbun-mime-encode-string (string)
  (mapconcat
   #'identity
   (split-string (or (eword-encode-string
		      (shimbun-decode-entities-string string)) ""))
   " "))

(defun shimbun-make-date-string (year month day &optional time)
  (format "%02d %s %04d %s +0900"
	  day
	  (aref [nil "Jan" "Feb" "Mar" "Apr" "May" "Jun"
		     "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
		month)
	  (cond ((< year 69)
		 (+ year 2000))
		((< year 100)
		 (+ year 1900))
		((< year 1000)	; possible 3-digit years.
		 (+ year 1900))
		(t year))
	  (or time "00:00")))

(if (fboundp 'regexp-opt)
    (defalias 'shimbun-regexp-opt 'regexp-opt)
  (defun shimbun-regexp-opt (strings &optional paren)
    "Return a regexp to match a string in STRINGS.
Each string should be unique in STRINGS and should not contain any regexps,
quoted or not.  If optional PAREN is non-nil, ensure that the returned regexp
is enclosed by at least one regexp grouping construct."
    (let ((open-paren (if paren "\\(" "")) (close-paren (if paren "\\)" "")))
      (concat open-paren (mapconcat 'regexp-quote strings "\\|") close-paren))))
(defun shimbun-decode-entities-string (string)
  "Decode entities in the STRING."
  (with-temp-buffer
    (insert string)
    (shimbun-decode-entities)
    (buffer-string)))

(defun shimbun-remove-markup ()
  "Remove all HTML markup, leaving just plain text."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "<!--" nil t)
      (delete-region (match-beginning 0)
		     (or (search-forward "-->" nil t)
			 (point-max))))
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t)
      (replace-match "" t t))))

(provide 'shimbun)

;;; shimbun.el ends here.

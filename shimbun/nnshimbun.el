;;; nnshimbun.el --- interfacing with web newspapers

;; Copyright (C) 2000,2001,2002,2003 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;          Akihiro Arisawa    <ari@atesoft.advantest.co.jp>,
;;          Katsumi Yamaoka    <yamaoka@jpl.org>,
;;          Yuuichi Teranishi  <teranisi@gohome.org>
;; Keywords: news

;; This file is a part of emacs-w3m.

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

;; This is a Gnus back end to read newspapers on the World Wide Web.
;; This module requires the shimbun libraries which is distributed
;; with emacs-w3m.  Visit the following pages for more information:
;;
;;	http://emacs-w3m.namazu.org/
;;	http://w3m.sourceforge.net/
;;
;; Install:
;;
;; You need to have installed in advance the latest APEL package, the
;; latest FLIM package and the latest Gnus that you can find at the
;; following places respectively:
;;
;;	ftp://ftp.m17n.org/pub/mule/apel/
;;	ftp://ftp.m17n.org/pub/mule/flim/
;;	ftp://ftp.gnus.org/pub/gnus/
;;
;; Consult the README file, install emacs-w3m and check whether this
;; module, the shimbun libraries (including shimbun.elc and many of
;; sb-*.elc files) and the emacs-w3m info files have also been
;; installed properly.
;;
;; Put the following expression in your ~/.gnus.el file:
;;
;;	(autoload 'gnus-group-make-shimbun-group "nnshimbun" nil t)
;;
;; Note: it isn't necessary if you are using T-gnus.
;;
;; Usage:
;;
;; See the emacs-w3m info (C-h i m emacs-w3m RET m Nnshimbun RET), you
;; can find the paragraph "The easiest way to get started...".


;;; Code:

(eval-when-compile (require 'cl))
(require 'nnoo)
(require 'nnheader)
(require 'nnmail)
(require 'gnus-bcklg)
(require 'shimbun)
(eval-and-compile
  (autoload 'gnus-declare-backend "gnus-start")
  (autoload 'gnus-group-make-group "gnus-group")
  (autoload 'message-make-date "message")
  (autoload 'parse-time-string "parse-time"))

(defgroup nnshimbun nil
  "Reading Web Newspapers with Gnus."
  :group 'gnus)


;; Customizable variables:

(defcustom nnshimbun-keep-backlog 300
  "*If non-nil, nnshimbun will keep read articles for later re-retrieval.
If it is a number N, then nnshimbun will only keep the last N articles
read.  If it is neither nil nor a number, nnshimbun will keep all read
articles.  This is not a good idea.

Note that smaller values may spoil the `prefetch-articles' feature,
since nnshimbun uses the backlog to keep the prefetched articles."
  :group 'nnshimbun
  :type '(choice (const :tag "off" nil)
		 integer
		 (sexp :format "all"
		       :value t)))

(defcustom nnshimbun-keep-unparsable-dated-articles t "\
*If non-nil, nnshimbun won't expire the articles of which the date is unknown."
  :group 'nnshimbun
  :type 'boolean)


;; The nnshimbun group parameter:

(defvar nnshimbun-group-parameters-custom
  '(list :format "Nnshimbun group parameters.\
  Check the leftmost button for customizing.\n%v"
	 (checklist :inline t
		    (list :inline t :format "%v"
			  (const :format "" index-range)
			  (choice :tag "Index range"
				  :value all
				  (const all)
				  (const last)
				  (integer :tag "pages")))
		    (list :inline t :format "%v"
			  (const :format "" prefetch-articles)
			  (choice :tag "Prefetch articles"
				  :value off
				  (const on)
				  (const off)))
		    (list :inline t :format "%v"
			  (const :format "" encapsulate-images)
			  (choice :tag "Encapsulate article"
				  :value on
				  (const on)
				  (const off)))
		    (list :inline t :format "%v"
			  (const :format "" expiry-wait)
			  (choice :tag "Expire wait"
				  :value never
				  (const never)
				  (const immediate)
				  (number :tag "days"))))
	 (repeat :inline t :tag "Others"
		 (list :inline t :format "%v"
		       (symbol :tag "Keyword")
		       (sexp :tag "Value"))))
  "A type definition for customizing the nnshimbun group parameters.")

(eval-and-compile
  (defconst nnshimbun-is-compiled-for-modern-gnus
    (eval-when-compile
      ;; The `gnus-define-group-parameter' macro isn't available in old Gnusae,
      ;; e.g. installed Emacs 21 may contain Gnus v5.9 which is the old Gnus.
      (fboundp 'gnus-define-group-parameter))
    "Non-nil means the nnshimbun.elc file is compiled for the modern Gnus.
Users should never modify the value."))

(eval-and-compile
  (let ((flag (if nnshimbun-is-compiled-for-modern-gnus
		  (if (fboundp 'gnus-define-group-parameter)
		      nil
		    (message "\
Warning: nnshimbun.elc is compiled for the newer Gnus,\
 you should recompile it")
		    (sit-for 1)
		    nil)
		(if (fboundp 'gnus-define-group-parameter)
		    (progn
		      (message "\
Warning: nnshimbun.elc is compiled for the old Gnus,\
 you should recompile it")
		      (sit-for 1)
		      '(nil . t))
		  '(t . t)))))
    (when (car flag)
      (defmacro gnus-define-group-parameter (&rest args) nil))
    (when (cdr flag)
      (defun nnshimbun-find-group-parameters (name)
	"Return an nnshimbun GROUP's group parameters."
	(when name
	  (or (gnus-group-find-parameter name 'nnshimbun-group-parameters t)
	      (assoc-default
	       name
	       (when (boundp 'nnshimbun-group-parameters-alist)
		 (symbol-value 'nnshimbun-group-parameters-alist))
	       (function string-match))))))))

(gnus-define-group-parameter
 ;; This definition provides the `nnshimbun-group-parameters' group
 ;; parameter, the `nnshimbun-group-parameters-alist' variable and the
 ;; `nnshimbun-find-group-parameters' function.
 nnshimbun-group-parameters
 :type list
 :function nnshimbun-find-group-parameters
 :function-document "\
Return nnshimbun GROUP's group parameters as a plist."
 :variable nnshimbun-group-parameters-alist
 :variable-default nil
 :variable-document "\
Alist of nnshimbun group parameters.  Each element should be a cons of
a group name regexp and a plist which contains a keyword and a value
pairs like the following:

'(\"^nnshimbun\\\\+asahi:\" index-range all prefetch-articles off
  encapsulate-images on expiry-wait 6)

`index-range' specifies a range of how many indices should be checked
for detecting new articles.  The valid values include:
      all: Retrieve all header indices.
     last: Retrieve the last header index.
integer N: Retrieve N pages of header indices.

`prefetch-articles' specifies whether to pre-fetch the unread articles
when scanning the group.

`encapsulate-images' specifies whether to embed the inline images in
the shimbun article.

`expiry-wait' has the same meaning as the standard `expiry-wait' group
parameter, but it takes precedence over the standard one."
 :variable-group nnshimbun
 :variable-type `(repeat (cons :format "%v" (regexp :tag "Group name regexp"
						    :value "^nnshimbun\\+")
			       ,nnshimbun-group-parameters-custom))
 :parameter-type nnshimbun-group-parameters-custom
 :parameter-document "\
Group parameters for the nnshimbun group.

`Index range' specifies a range of how many indices should be checked
for detecting new articles.  The valid values include:
      all: Retrieve all header indices.
     last: Retrieve the last header index.
integer N: Retrieve N pages of header indices.

`Prefetch articles' specifies whether to pre-fetch the unread articles
when scanning the group.

`Encapsulate article' specifies whether to embed inline images in the
shimbun article.

`Expire wait' has the same meaning as the standard `expiry-wait' group
parameter, but it takes precedence over the standard one.")


;; The back end definitions:

(gnus-declare-backend "nnshimbun" 'address)
(nnoo-declare nnshimbun)

(defvoo nnshimbun-directory (nnheader-concat gnus-directory "shimbun/")
  "*Directory where nnshimbun will store NOV files.
Actually, you can find the NOV file in the SERVER/GROUP/ subdirectory.")

(defvoo nnshimbun-nov-is-evil nil
  "*If non-nil, nnshimbun won't use the NOV databases to retrieve headers.")

(defvoo nnshimbun-nov-file-name ".overview")

(defvoo nnshimbun-pre-fetch-article 'off
  "*If it is neither `off' nor nil, nnshimbun fetches unread articles when
scanning the group.  This provides just a default value for all the
nnshimbun groups.  You can use the `prefecth-articles' nnshimbun group
parameter for each nnshimbun group.")

(defvoo nnshimbun-encapsulate-images shimbun-encapsulate-images
  "*If it is neither `off' nor nil, nnshimbun will embed the inline images
in the shimbun article.  This provides just a default value for all
the nnshimbun groups.  You can use the `encapsulate-images' nnshimbun
group parameter for each nnshimbun group.")

(defvoo nnshimbun-index-range nil
  "*Range of how many indices should be checked for detecting new articles.
`all' or nil is for all indices, `last' is for the last index, and a
integer N is for the last N pages of indices.  This provides just a
default value for all the nnshimbun groups.  You can use the
`index-range' nnshimbun group parameter for each nnshimbun group.")


;; This value will be set by the `nnshimbun-open-server' function.
(defvoo nnshimbun-shimbun nil)

(defvoo nnshimbun-status-string "")
(defvoo nnshimbun-backlog-articles nil)
(defvoo nnshimbun-backlog-hashtb nil)

(defmacro nnshimbun-current-server ()
  '(nnoo-current-server 'nnshimbun))

(defmacro nnshimbun-server-directory (&optional server)
  `(file-name-as-directory
    (expand-file-name ,(or server '(nnshimbun-current-server))
		      nnshimbun-directory)))

(defmacro nnshimbun-current-group ()
  '(shimbun-current-group-internal nnshimbun-shimbun))

(defmacro nnshimbun-current-directory (&optional group)
  `(nnmail-group-pathname ,(or group '(nnshimbun-current-group))
			  (nnshimbun-server-directory)))

(defmacro nnshimbun-backlog (&rest form)
  `(let ((gnus-keep-backlog nnshimbun-keep-backlog)
	 (gnus-backlog-buffer
	  (format " *nnshimbun backlog %s*" (nnshimbun-current-server)))
	 (gnus-backlog-articles nnshimbun-backlog-articles)
	 (gnus-backlog-hashtb nnshimbun-backlog-hashtb))
     (unwind-protect
	 (progn ,@form)
       (setq nnshimbun-backlog-articles gnus-backlog-articles
	     nnshimbun-backlog-hashtb gnus-backlog-hashtb))))
(put 'nnshimbun-backlog 'lisp-indent-function 0)
(put 'nnshimbun-backlog 'edebug-form-spec t)

(defmacro nnshimbun-find-parameter (group symbol &optional full-name-p)
  "Return the value of GROUP's nnshimbun group parameter which corresponds
to SYMBOL.  If FULL-NAME-P is non-nil, it treats GROUP as a full name."
  (let ((name (if full-name-p
		  group
		`(concat "nnshimbun+" (nnshimbun-current-server) ":" ,group))))
    (cond ((eq 'index-range (eval symbol))
	   `(or (plist-get (nnshimbun-find-group-parameters ,name)
			   'index-range)
		nnshimbun-index-range))
	  ((eq 'prefetch-articles (eval symbol))
	   `(let ((val (or (plist-get (nnshimbun-find-group-parameters ,name)
				      'prefetch-articles)
			   nnshimbun-pre-fetch-article)))
	      (if (eq 'off val)
		  nil
		val)))
	  ((eq 'encapsulate-images (eval symbol))
	   `(let ((val (or (plist-get (nnshimbun-find-group-parameters ,name)
				      'encapsulate-images)
			   nnshimbun-encapsulate-images)))
	      (if (eq 'off val)
		  nil
		val)))
	  ((eq 'expiry-wait (eval symbol))
	   (if full-name-p
	       `(or (plist-get (nnshimbun-find-group-parameters ,group)
			       'expiry-wait)
		    (gnus-group-find-parameter ,group 'expiry-wait))
	     `(let ((name ,name))
		(or (plist-get (nnshimbun-find-group-parameters name)
			       'expiry-wait)
		    (gnus-group-find-parameter name 'expiry-wait)))))
	  (t
	   `(plist-get (nnshimbun-find-group-parameters ,name) ,symbol)))))


;; Interface functions:

(nnoo-define-basics nnshimbun)

(defun nnshimbun-possibly-change-group (group &optional server)
  (when (if server
	    (nnshimbun-open-server server)
	  nnshimbun-shimbun)
    (or (not group)
	(when (condition-case err
		  (shimbun-open-group nnshimbun-shimbun group)
		(error
		 (nnheader-report 'nnshimbun "%s" (error-message-string err))))
	  (let ((file-name-coding-system nnmail-pathname-coding-system)
		(pathname-coding-system nnmail-pathname-coding-system)
		(dir (nnshimbun-current-directory group)))
	    (or (file-directory-p dir)
		(ignore-errors
		  (make-directory dir)
		  (file-directory-p dir))
		(nnheader-report 'nnshimbun
				 (if (file-exists-p dir)
				     "Not a directory: %s"
				   "Couldn't create directory: %s")
				 dir)))))))

(deffoo nnshimbun-open-server (server &optional defs)
  (or (nnshimbun-server-opened server)
      (let ((file-name-coding-system nnmail-pathname-coding-system)
	    (pathname-coding-system nnmail-pathname-coding-system)
	    (shimbun))
	(when (condition-case err
		  (setq shimbun
			(shimbun-open server
				      (luna-make-entity 'shimbun-gnus-mua)))
		(error
		 (nnheader-report 'nnshimbun "%s" (error-message-string err))))
	  (nnoo-change-server 'nnshimbun server
			      (cons (list 'nnshimbun-shimbun shimbun) defs))
	  (when (or (file-directory-p nnshimbun-directory)
		    (ignore-errors
		      (make-directory nnshimbun-directory)
		      (file-directory-p nnshimbun-directory))
		    (progn
		      (nnshimbun-close-server)
		      (nnheader-report 'nnshimbun
				       (if (file-exists-p nnshimbun-directory)
					   "Not a directory: %s"
					 "Couldn't create directory: %s")
				       nnshimbun-directory)))
	    (let ((dir (nnshimbun-server-directory server)))
	      (when (or (file-directory-p dir)
			(ignore-errors
			  (make-directory dir)
			  (file-directory-p dir))
			(progn
			  (nnshimbun-close-server)
			  (nnheader-report 'nnshimbun
					   (if (file-exists-p dir)
					       "Not a directory: %s"
					     "Couldn't create directory: %s")
					   dir)))
		(nnheader-report 'nnshimbun
				 "Opened server %s using directory %s"
				 server dir)
		t)))))))

(deffoo nnshimbun-close-server (&optional server)
  (when (nnshimbun-server-opened server)
    (when nnshimbun-shimbun
      (dolist (group (shimbun-groups nnshimbun-shimbun))
	(nnshimbun-write-nov group t))
      (shimbun-close nnshimbun-shimbun)))
  (nnshimbun-backlog (gnus-backlog-shutdown))
  (nnoo-close-server 'nnshimbun server)
  t)

(defun nnshimbun-replace-date-header (article header)
  "This function definition should be replaced with the proper one for
the running Gnus version to keep compatibility with the Gnusae variants
when it is called at the first time."
  (require 'gnus-sum)
  (require 'bytecomp)
  (defalias 'nnshimbun-replace-date-header
    (byte-compile
     '(lambda (article header)
	"Replace ARTICLE's date header with HEADER."
	(let ((x (gnus-summary-article-header article)))
	  (when x
	    (mail-header-set-date x (shimbun-header-date header)))))))
  (funcall 'nnshimbun-replace-date-header article header))

(defun nnshimbun-request-article-1 (article &optional group server to-buffer)
  (if (nnshimbun-backlog
	(gnus-backlog-request-article
	 group article (or to-buffer nntp-server-buffer)))
      (cons group article)
    (let* ((header (with-current-buffer (nnshimbun-open-nov group)
		     (and (nnheader-find-nov-line article)
			  (nnshimbun-parse-nov))))
	   (original-id (shimbun-header-id header)))
      (when header
	(with-current-buffer (or to-buffer nntp-server-buffer)
	  (erase-buffer)
	  (let ((shimbun-encapsulate-images
		 (nnshimbun-find-parameter group 'encapsulate-images)))
	    (shimbun-article nnshimbun-shimbun header))
	  (when (> (buffer-size) 0)
	    ;; Replace the date string in the `gnus-newsgroup-data' variable
	    ;; with the newly retrieved article's one.  It may be kludge.
	    (nnshimbun-replace-date-header article header)
	    (nnshimbun-replace-nov-entry group article header original-id)
	    (nnshimbun-backlog
	      (gnus-backlog-enter-article group article (current-buffer)))
	    (nnheader-report 'nnshimbun "Article %s retrieved"
			     (shimbun-header-id header))
	    (cons group article)))))))

(deffoo nnshimbun-request-article (article &optional group server to-buffer)
  (when (nnshimbun-possibly-change-group group server)
    (if (or (integerp article)
	    (when (stringp article)
	      (setq article
		    (or (when (or group
				  (setq group (nnshimbun-current-group)))
			  (nnshimbun-search-id group article))
			(catch 'found
			  (dolist (x (shimbun-groups nnshimbun-shimbun))
			    (and (nnshimbun-possibly-change-group x)
				 (setq x (nnshimbun-search-id x article))
				 (throw 'found x))))))))
	(nnshimbun-request-article-1 article group server to-buffer)
      (nnheader-report 'nnshimbun "Couldn't retrieve article: %s"
		       (prin1-to-string article)))))

(deffoo nnshimbun-request-group (group &optional server dont-check)
  (if (not (nnshimbun-possibly-change-group group server))
      (nnheader-report 'nnshimbun "Invalid group (no such directory)")
    (let (beg end lines)
      (with-current-buffer (nnshimbun-open-nov group)
	(goto-char (point-min))
	(setq beg (ignore-errors (read (current-buffer))))
	(goto-char (point-max))
	(forward-line -1)
	(setq end (ignore-errors (read (current-buffer)))
	      lines (count-lines (point-min) (point-max))))
      (nnheader-report 'nnshimbun "Selected group %s" group)
      (nnheader-insert "211 %d %d %d %s\n"
		       lines (or beg 0) (or end 0) group))))

(deffoo nnshimbun-request-scan (&optional group server)
  (when (nnshimbun-possibly-change-group nil server)
    (if group
	(nnshimbun-generate-nov-database group)
      (dolist (group (shimbun-groups nnshimbun-shimbun))
	(nnshimbun-generate-nov-database group)))))

(deffoo nnshimbun-close-group (group &optional server)
  (nnshimbun-write-nov group)
  t)

(deffoo nnshimbun-request-list (&optional server)
  (when (nnshimbun-possibly-change-group nil server)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (group (shimbun-groups nnshimbun-shimbun))
	(when (nnshimbun-possibly-change-group group)
	  (let (beg end)
	    (with-current-buffer (nnshimbun-open-nov group)
	      (goto-char (point-min))
	      (setq beg (ignore-errors (read (current-buffer))))
	      (goto-char (point-max))
	      (forward-line -1)
	      (setq end (ignore-errors (read (current-buffer)))))
	    (insert (format "%s %d %d n\n" group (or end 0) (or beg 0)))))))
    t)) ;; return value

(deffoo nnshimbun-retrieve-headers (articles &optional group server fetch-old)
  (when (nnshimbun-possibly-change-group group server)
    (if (nnshimbun-retrieve-headers-with-nov articles group fetch-old)
	'nov
      (with-current-buffer nntp-server-buffer
	(erase-buffer)
	(let (header)
	  (dolist (art articles)
	    (when (and (if (stringp art)
			   (setq art (nnshimbun-search-id group art))
			 (integerp art))
		       (setq header
			     (with-current-buffer (nnshimbun-open-nov group)
			       (and (nnheader-find-nov-line art)
				    (nnshimbun-parse-nov)))))
	      (insert (format "220 %d Article retrieved.\n" art))
	      (shimbun-header-insert nnshimbun-shimbun header)
	      (insert ".\n")
	      (delete-region (point) (point-max)))))
	'header))))

(defun nnshimbun-retrieve-headers-with-nov (articles &optional group fetch-old)
  (unless (or gnus-nov-is-evil nnshimbun-nov-is-evil)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (insert-buffer (nnshimbun-open-nov group))
      (unless (and fetch-old (not (numberp fetch-old)))
	(nnheader-nov-delete-outside-range
	 (if fetch-old
	     (max 1 (- (car articles) fetch-old))
	   (car articles))
	 (nth (1- (length articles)) articles)))
      t)))


;; Functions to manage the NOV databases:

(defvar nnshimbun-tmp-string nil
  "Internal variable used as a rest to keep a temporary string.
The macro `nnshimbun-string-or' uses it exclusively.")

(defmacro nnshimbun-string-or (&rest strings)
  "Return the first element of STRINGS that is a non-blank string.  It
should run fast, especially if two strings are given.  Each string can
also be nil."
  (cond ((null strings)
	 nil)
	((= 1 (length strings))
	 ;; Return irregularly nil if one blank string is given.
	 `(unless (zerop (length (setq nnshimbun-tmp-string ,(car strings))))
	    nnshimbun-tmp-string))
	((= 2 (length strings))
	 ;; Return the second string when the first string is blank.
	 `(if (zerop (length (setq nnshimbun-tmp-string ,(car strings))))
	      ,(cadr strings)
	    nnshimbun-tmp-string))
	(t
	 `(let ((strings (list ,@strings)))
	    (while strings
	      (setq strings (if (zerop (length (setq nnshimbun-tmp-string
						     (car strings))))
				(cdr strings))))
	    nnshimbun-tmp-string))))

(defsubst nnshimbun-insert-nov (number header &optional id)
  (insert "\n")
  (backward-char 1)
  (let ((header-id (nnshimbun-string-or (shimbun-header-id header)))
	;; Force `princ' to work in the current buffer.
	(standard-output (current-buffer))
	(xref (nnshimbun-string-or (shimbun-header-xref header)))
	(start (point)))
    (and (stringp id)
	 header-id
	 (string-equal id header-id)
	 (setq id nil))
    (princ number)
    (insert
     "\t"
     (nnshimbun-string-or (shimbun-header-subject header) "(none)") "\t"
     (nnshimbun-string-or (shimbun-header-from header) "(nobody)") "\t"
     (nnshimbun-string-or (shimbun-header-date header) (message-make-date))
     "\t"
     (or header-id (nnmail-message-id)) "\t"
     (or (shimbun-header-references header) "") "\t")
    (princ (or (shimbun-header-chars header) 0))
    (insert "\t")
    (princ (or (shimbun-header-lines header) 0))
    (insert "\t")
    (if xref
	(progn
	  (insert "Xref: " xref "\t")
	  (when id
	    (insert "X-Nnshimbun-Id: " id "\t")))
      (when id
	(insert "\tX-Nnshimbun-Id: " id "\t")))
    ;; Replace newlines with spaces in the current NOV line.
    (while (progn
	     (forward-line 0)
	     (> (point) start))
      (backward-delete-char 1)
      (insert " "))
    (forward-line 1)))

(defun nnshimbun-generate-nov-database (group)
  (when (nnshimbun-possibly-change-group group)
    (with-current-buffer (nnshimbun-open-nov group)
      (goto-char (point-max))
      (forward-line -1)
      (let* ((i (or (ignore-errors (read (current-buffer))) 0))
	     (name (concat "nnshimbun+" (nnshimbun-current-server) ":" group))
	     (pre-fetch (nnshimbun-find-parameter name 'prefetch-articles t)))
	(dolist (header
		 (shimbun-headers nnshimbun-shimbun
				  (nnshimbun-find-parameter name
							    'index-range t)))
	  (unless (nnshimbun-search-id group (shimbun-header-id header))
	    (goto-char (point-max))
	    (nnshimbun-insert-nov (setq i (1+ i)) header)
	    (when pre-fetch
	      (with-temp-buffer
		(nnshimbun-request-article-1 i group nil (current-buffer)))))))
      (nnshimbun-write-nov group))))

(defun nnshimbun-replace-nov-entry (group article header &optional id)
  (with-current-buffer (nnshimbun-open-nov group)
    (when (nnheader-find-nov-line article)
      (delete-region (point) (progn (forward-line 1) (point)))
      (nnshimbun-insert-nov article header id))))

(defun nnshimbun-search-id (group id)
  (with-current-buffer (nnshimbun-open-nov group)
    (goto-char (point-min))
    (let (found)
      (while (and (not found)
		  (search-forward id nil t)) ; We find the ID.
	;; And the id is in the fourth field.
	(if (not (and (search-backward "\t" nil t 4)
		      (not (search-backward "\t" (gnus-point-at-bol) t))))
	    (forward-line 1)
	  (forward-line 0)
	  (setq found t)))
      (unless found
	(goto-char (point-min))
	(setq id (concat "X-Nnshimbun-Id: " id))
	(while (and (not found)
		    (search-forward id nil t))
	  (if (not (search-backward "\t" (gnus-point-at-bol) t 8))
	      (forward-line 1)
	    (forward-line 0)
	    (setq found t))))
      (when found
	(ignore-errors (read (current-buffer)))))))

;; This function is defined as the alternative of `nnheader-parse-nov',
;; in order to keep compatibility between T-gnus and Oort Gnus.
(defun nnshimbun-parse-nov ()
  (let ((eol (gnus-point-at-eol)))
    (let ((number  (nnheader-nov-read-integer))
	  (subject (nnheader-nov-field))
	  (from    (nnheader-nov-field))
	  (date    (nnheader-nov-field))
	  (id      (nnheader-nov-read-message-id))
	  (refs    (nnheader-nov-field))
	  (chars   (nnheader-nov-read-integer))
	  (lines   (nnheader-nov-read-integer))
	  (xref    (unless (eq (char-after) ?\n)
		     (when (looking-at "Xref: ")
		       (goto-char (match-end 0)))
		     (nnheader-nov-field)))
	  (extra   (nnheader-nov-parse-extra)))
      (shimbun-make-header number subject from date
			   (or (cdr (assq 'X-Nnshimbun-Id extra)) id)
			   refs chars lines xref))))

(defsubst nnshimbun-nov-buffer-name (&optional group)
  (format " *nnshimbun overview %s %s*"
	  (nnshimbun-current-server)
	  (or group (nnshimbun-current-group))))

(defsubst nnshimbun-nov-file-name (&optional group)
  (nnmail-group-pathname (or group (nnshimbun-current-group))
			 (nnshimbun-server-directory)
			 nnshimbun-nov-file-name))

(defun nnshimbun-open-nov (group)
  (let ((buffer (nnshimbun-nov-buffer-name group)))
    (unless (gnus-buffer-live-p buffer)
      (with-current-buffer (gnus-get-buffer-create buffer)
	(erase-buffer)
	(let ((file-name-coding-system nnmail-pathname-coding-system)
	      (pathname-coding-system nnmail-pathname-coding-system)
	      (nov (nnshimbun-nov-file-name group)))
	  (when (file-exists-p nov)
	    (nnheader-insert-file-contents nov)))
	(set-buffer-modified-p nil)))
    buffer))

(defun nnshimbun-write-nov (group &optional close)
  (let ((buffer (nnshimbun-nov-buffer-name group)))
    (when (gnus-buffer-live-p buffer)
      (with-current-buffer buffer
	(let ((file-name-coding-system nnmail-pathname-coding-system)
	      (pathname-coding-system nnmail-pathname-coding-system)
	      (nov (nnshimbun-nov-file-name group)))
	  (when (and (buffer-modified-p)
		     (or (> (buffer-size) 0)
			 (file-exists-p nov)))
	    (nnmail-write-region 1 (point-max) nov nil 'nomesg)
	    (set-buffer-modified-p nil))))
      (when close
	(kill-buffer buffer)))))

(deffoo nnshimbun-request-expire-articles (articles group
						    &optional server force)
  "Do the expiration for the specified ARTICLES in the nnshimbun GROUP.
Note that nnshimbun does not actually delete any articles, it simply
deletes the entry in your own NOV database corresponding to the
article to be expired.  The optional fourth argument FORCE is ignored."
  (when (nnshimbun-possibly-change-group group server)
    (let* ((expirable (copy-sequence articles))
	   (name (concat "nnshimbun+" (nnshimbun-current-server) ":" group))
	   ;; If the group's `expiry-wait' parameter is non-nil, the value
	   ;; of `nnmail-expiry-wait' will be bound to that value, and the
	   ;; value of `nnmail-expiry-wait-function' will be bound to nil.
	   ;; See the `gnus-summary-expire-articles' function definition to
	   ;; know how does it work.  If the group's parameter is not
	   ;; specified by the user, the shimbun's default value will be
	   ;; used instead.
	   (expiry-wait
	    (or (nnshimbun-find-parameter name 'expiry-wait t)
		(shimbun-article-expiration-days nnshimbun-shimbun)))
	   (nnmail-expiry-wait (or expiry-wait nnmail-expiry-wait))
	   (nnmail-expiry-wait-function (if expiry-wait
					    nil
					  nnmail-expiry-wait-function))
	   article end time)
      (with-current-buffer (nnshimbun-open-nov group)
	(while expirable
	  (setq article (pop expirable))
	  (when (and (nnheader-find-nov-line article)
		     (setq end (gnus-point-at-eol))
		     (not (= (point-max) (1+ end))))
	    (setq time (and (search-forward "\t" end t)
			    (search-forward "\t" end t)
			    (search-forward "\t" end t)
			    (parse-time-string
			     (buffer-substring
			      (point)
			      (if (search-forward "\t" end t)
				  (1- (point))
				end)))))
	    (when (if (setq time (condition-case nil
				     (apply 'encode-time time)
				   (error nil)))
		      (nnmail-expired-article-p name time nil)
		    ;; Inhibit expiration if there's no parsable date
		    ;; and the following option is non-nil.
		    (not nnshimbun-keep-unparsable-dated-articles))
	      (forward-line 0)
	      (delete-region (point) (1+ end))
	      (setq articles (delq article articles)))))
	(nnshimbun-write-nov group))
      articles)))


;; Defining the `shimbun-gnus-mua':

(luna-define-class shimbun-gnus-mua (shimbun-mua) ())

(luna-define-method shimbun-mua-search-id ((mua shimbun-gnus-mua) id)
  (nnshimbun-search-id
   (shimbun-current-group-internal (shimbun-mua-shimbun-internal mua))
   id))


;; Command to create an nnshimbun group:

(defvar nnshimbun-server-history nil)

;;;###autoload
(defun gnus-group-make-shimbun-group ()
  "Create an nnshimbun group.
The user will be prompted for a server name and a group name."
  (interactive)
  (let* ((minibuffer-setup-hook
	  (append minibuffer-setup-hook '(beginning-of-line)))
	 (alist (shimbun-servers-alist))
	 (server (completing-read
		  "Shimbun address: "
		  alist nil t
		  (or (car nnshimbun-server-history)
		      (caar alist))
		  'nnshimbun-server-history))
	 (groups)
	 (nnshimbun-pre-fetch-article))
    (if (setq groups (shimbun-groups (shimbun-open server)))
	(gnus-group-make-group
	 (completing-read "Group name: " (mapcar 'list groups) nil t nil)
	 (list 'nnshimbun server))
      (error "%s" "Can't find group"))))


(provide 'nnshimbun)

;;; nnshimbun.el ends here

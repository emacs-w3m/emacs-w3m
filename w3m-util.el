;;; w3m-util.el --- Utility macros and functions for emacs-w3m

;; Copyright (C) 2001, 2002, 2003 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;          Shun-ichi GOTO     <gotoh@taiyo.co.jp>,
;;          Satoru Takabayashi <satoru-t@is.aist-nara.ac.jp>,
;;          Hideyuki SHIRAI    <shirai@meadowy.org>,
;;          Keisuke Nishida    <kxn30@po.cwru.edu>,
;;          Yuuichi Teranishi  <teranisi@gohome.org>,
;;          Akihiro Arisawa    <ari@mbf.sphere.ne.jp>,
;;          Katsumi Yamaoka    <yamaoka@jpl.org>
;; Keywords: w3m, WWW, hypermedia

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

;; This module is a part of emacs-w3m which provides utility macros
;; and inline functions.  Visit <URL:http://emacs-w3m.namazu.org/> for
;; more details of emacs-w3m.

;;; Code:

(eval-when-compile
  (require 'cl)
  ;; Variables and functions which are used in the following inline
  ;; functions.  They should be defined in the other module at run-time.
  (defvar w3m-current-url)
  (defvar w3m-current-title)
  (defvar w3m-html-string-regexp)
  (defvar w3m-work-buffer-list)
  (defvar w3m-current-refresh)
  (defvar w3m-refresh-timer)
  (defvar w3m-use-refresh))

(eval-and-compile
  (dont-compile
    (condition-case nil
	:symbol-for-testing-whether-colon-keyword-is-available-or-not
      (void-variable
       (let (w3m-colon-keywords)
	 (load "w3m-kwds.el" nil t t)
	 (while w3m-colon-keywords
	   (set (car w3m-colon-keywords) (car w3m-colon-keywords))
	   (setq w3m-colon-keywords (cdr w3m-colon-keywords))))))))

(eval-and-compile
  (cond
   ((or (featurep 'xemacs)
	(not (boundp 'emacs-major-version))
	(< emacs-major-version 20))
    (require 'poe)
    (require 'poem))))

(eval-and-compile
  (cond ((and (boundp 'emacs-major-version)
	      (= emacs-major-version 19))
	 (autoload 'cancel-timer "timer")
	 (require 'custom))))

;;; Things should be defined in advance:

;; (There are no objects so far.)


;;; Control structures:

(defmacro w3m-static-if (cond then &rest else)
  "Like `if', except that it evaluates COND at compile-time."
  (if (eval cond) then (` (progn  (,@ else)))))
(put 'w3m-static-if 'lisp-indent-function 2)

(put 'w3m-static-when 'lisp-indent-function 1)
(defmacro w3m-static-when (cond &rest body)
  "Like `when', but evaluate COND at compile time."
  (if (eval cond)
      (` (progn (,@ body)))))

(put 'w3m-static-unless 'lisp-indent-function 1)
(defmacro w3m-static-unless (cond &rest body)
  "Like `unless', but evaluate COND at compile time."
  (if (eval cond)
      nil
    (` (progn (,@ body)))))

(defmacro w3m-static-cond (&rest clauses)
  "Like `cond', except that it evaluates CONDITION part of each clause at
compile-time."
  (while (and clauses
	      (not (eval (car (car clauses)))))
    (setq clauses (cdr clauses)))
  (if clauses
      (cons 'progn (cdr (car clauses)))))

(put 'w3m-condition-case lisp-indent-function 2)
(defmacro w3m-condition-case (var bodyform &rest handlers)
  "Like `condition-case', except that signal an error if `debug-on-error'
or `debug-on-quit' is non-nil."
  (` (if (or debug-on-error debug-on-quit)
	 (, bodyform)
       (condition-case (, var)
	   (, bodyform)
	 (,@ handlers)))))


;;; Text props:

(defmacro w3m-add-text-properties (start end props &optional object)
  "Like `add-text-properties' but always add non-sticky properties."
  (let ((non-stickies
	 (if (featurep 'xemacs)
	     ;; Default to start-closed and end-open in XEmacsen.
	     '(list 'start-open t)
	   ;; Default to front-nonsticky and rear-sticky in FSF Emacsen.
	   '(list 'rear-nonsticky t))))
    (` (add-text-properties (, start) (, end)
			    (append (, non-stickies) (, props))
			    (, object)))))

(defmacro w3m-get-text-property-around (prop)
  "Search for the text property PROP in one character before and behind
the current position.  Return the value corresponding to PROP or nil.
If PROP is not found at the current position, point will move to the
position where PROP exists."
  (` (let ((position (point))
	   value)
       (or (get-text-property position (, prop))
	   (and (not (bolp))
		(setq value (get-text-property (1- position) (, prop)))
		(goto-char (1- position))
		value)
	   (and (not (eolp))
		(setq value (get-text-property (1+ position) (, prop)))
		(goto-char (1+ position))
		value)))))

(defmacro w3m-action (&optional position)
  "Return the value of the `w3m-action' property at the given POSITION.
NOTE: If POSITION is omitted, it searches for the property in one
character before and behind the current position, and point will move
to the position where the property exists."
  (if position
      (` (get-text-property (, position) 'w3m-action))
    (` (w3m-get-text-property-around 'w3m-action))))

(defmacro w3m-anchor (&optional position)
  "Return the value of the `w3m-href-anchor' property at the given POSITION.
NOTE: If POSITION is omitted, it searches for the property in one
character before and behind the current position, and point will move
to the position where the property exists."
  (if position
      (` (get-text-property (, position) 'w3m-href-anchor))
    (` (w3m-get-text-property-around 'w3m-href-anchor))))

(defmacro w3m-image (&optional position)
  "Return the value of the `w3m-image' property at the given POSITION.
NOTE: If POSITION is omitted, it searches for the property in one
character before and behind the current position, and point will move
to the position where the property exists."
  (if position
      (` (get-text-property (, position) 'w3m-image))
    (` (w3m-get-text-property-around 'w3m-image))))

(defmacro w3m-submit (&optional position)
  "Return the value of the `w3m-submit' property at the given POSITION.
NOTE: If POSITION is omitted, it searches for the property in one
character before and behind the current position, and point will move
to the position where the property exists."
  (if position
      (` (get-text-property (, position) 'w3m-submit))
    (` (w3m-get-text-property-around 'w3m-submit))))

(defmacro w3m-anchor-sequence (&optional position)
  "Return the value of the `w3m-anchor-sequence' property at POSITION.
If POSITION is omitted, the current position is assumed."
  (if position
      (` (get-text-property (, position) 'w3m-anchor-sequence))
    '(get-text-property (point) 'w3m-anchor-sequence)))

;;; Attributes:

(put 'w3m-parse-attributes 'lisp-indent-function '1)
(def-edebug-spec w3m-parse-attributes
  ((&rest &or (symbolp &optional symbolp) symbolp) body))
(defmacro w3m-parse-attributes (attributes &rest form)
  (` (let ((,@ (mapcar
		(lambda (attr)
		  (if (listp attr) (car attr) attr))
		attributes)))
       (skip-chars-forward " \t\r\f\n")
       (while
	   (cond
	    (,@ (mapcar
		 (lambda (attr)
		   (or (symbolp attr)
		       (and (listp attr)
			    (<= (length attr) 2)
			    (symbolp (car attr)))
		       (error "Internal error, type mismatch"))
		   (let ((sexp (quote
				(w3m-remove-redundant-spaces
				 (or (match-string-no-properties 2)
				     (match-string-no-properties 3)
				     (match-string-no-properties 1)))))
			 type)
		     (when (listp attr)
		       (setq type (nth 1 attr))
		       (cond
			((eq type :case-ignore)
			 (setq sexp (list 'downcase sexp)))
			((eq type :integer)
			 (setq sexp (list 'string-to-number sexp)))
			((eq type :bool)
			 (setq sexp t))
			((eq type :decode-entity)
			 (setq sexp (list 'w3m-decode-entities-string sexp)))
			((nth 1 attr)
			 (error "Internal error, unknown modifier")))
		       (setq attr (car attr)))
		     (` ((looking-at
			  (, (if (eq type :bool)
				 (symbol-name attr)
			       (format "%s[ \t\r\f\n]*=[ \t\r\f\n]*%s"
				       (symbol-name attr)
				       w3m-html-string-regexp))))
			 (setq (, attr) (, sexp))))))
		 attributes))
	    ((looking-at
	      (, (concat "[A-Za-z]*[ \t\r\f\n]*=[ \t\r\f\n]*"
			 w3m-html-string-regexp))))
	    ((looking-at "[^<> \t\r\f\n]+")))
	 (goto-char (match-end 0))
	 (skip-chars-forward " \t\r\f\n"))
       (skip-chars-forward "^>")
       (forward-char)
       (,@ form))))


;;; Working buffers:

(defsubst w3m-get-buffer-create (name)
  "Return the buffer named NAME, or create such a buffer and return it."
  (or (get-buffer name)
      (let ((buf (get-buffer-create name)))
	(setq w3m-work-buffer-list (cons buf w3m-work-buffer-list))
	(buffer-disable-undo buf)
	buf)))

(defsubst w3m-kill-buffer (buffer)
  "Kill the buffer BUFFER and remove it from `w3m-work-buffer-list'.
The argument may be a buffer or may be the name of a buffer.
An argument of nil means kill the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (when (stringp buffer)
    (setq buffer (get-buffer buffer)))
  (when (buffer-live-p buffer)
    (kill-buffer buffer))
  (setq w3m-work-buffer-list (delq buffer w3m-work-buffer-list))
  nil)

(defun w3m-kill-all-buffer ()
  "Kill all working buffer."
  (dolist (buf w3m-work-buffer-list)
    (when (buffer-live-p buf)
      (kill-buffer buf)))
  (setq w3m-work-buffer-list nil))

(defsubst w3m-buffer-title (buffer)
  "Return the title of the buffer BUFFER."
  (with-current-buffer buffer
    (cond
     ((and (stringp w3m-current-title)
	   (not (string= w3m-current-title "<no-title>")))
      w3m-current-title)
     ((stringp w3m-current-url)
      (directory-file-name
       (if (string-match "^[^/:]+:/+" w3m-current-url)
	   (substring w3m-current-url (match-end 0))
	 w3m-current-url)))
     (t "<no-title>"))))

(defsubst w3m-buffer-number (buffer)
  (when (and (bufferp buffer)
	     (string-match "\\`\\*w3m\\*\\(<\\([0-9]+\\)>\\)?\\'"
			   (buffer-name buffer)))
    (if (match-beginning 1)
	(string-to-number (match-string 2 (buffer-name buffer)))
      1))) ;; `1' should not be represented in the buffer name.

(defsubst w3m-buffer-set-number (buffer number)
  (unless (eq (w3m-buffer-number buffer) number)
    (with-current-buffer buffer
      (let ((newname (if (= number 1)
			 "*w3m*"
		       (format "*w3m*<%d>" number))))
	(unless (get-buffer newname)
	  (rename-buffer newname))))))

(defun w3m-buffer-name-lessp (x y)
  "Return t if first arg buffer's name is less than second."
  (when (bufferp x)
    (setq x (buffer-name x)))
  (when (bufferp y)
    (setq y (buffer-name y)))
  (if (and (string-match "\\`\\*w3m\\*\\(<\\([0-9]+\\)>\\)?\\'" x)
	   (setq x (cons x
			 (if (match-beginning 1)
			     (string-to-number (match-string 2 x))
			   1))))
      (if (string-match "\\`\\*w3m\\*\\(<\\([0-9]+\\)>\\)?\\'" y)
	  (< (cdr x)
	     (if (match-beginning 1)
		 (string-to-number (match-string 2 y))
	       1))
	(string< (car x) y))
    (string< x y)))

(defsubst w3m-list-buffers (&optional nosort)
  "Return list of w3m-mode buffers."
  (if nosort
      (save-current-buffer
	(delq nil
	      (mapcar
	       (lambda (buffer)
		 (set-buffer buffer)
		 (when (eq major-mode 'w3m-mode) buffer))
	       (buffer-list))))
    (sort (w3m-list-buffers t)
	  (function w3m-buffer-name-lessp))))


;;; Miscellaneous:
(defconst w3m-url-fallback-base "http:///")
(defconst w3m-url-invalid-regexp "\\`http:///")

(defsubst w3m-url-valid (url)
  (and url (not (string-match w3m-url-invalid-regexp url))
       url))

(defmacro w3m-tag-regexp-of (&rest names)
  "Return a regexp string, not a funtion form.  A regexp should match tags
which are started with \"<\" and one of NAMES.  NAMES should be string
constants, any other expressions are not allowed."
  (concat "<\\("
	  (mapconcat 'identity names "\\|")
	  "\\)\\([ \t\r\f\n]+[^>]*\\)?/?>"))

(defsubst w3m-time-newer-p (a b)
  "Return t, if A is newer than B.  Otherwise return nil.
A and B are lists which represent time in Emacs-style.  If value is
nil, it is regarded as the oldest time."
  (and a
       (or (not b)
	   (or (> (car a) (car b))
	       (and (= (car a) (car b))
		    (> (nth 1 a) (nth 1 b)))))))

(defsubst w3m-time-lapse-seconds (start end)
  "Return lapse seconds from START to END.
START and END are lists which represent time in Emacs-style."
  (+ (* (- (car end) (car start)) 65536)
     (cadr end)
     (- (cadr start))))

(defalias 'w3m-float-time
  (if (fboundp 'float-time)
      'float-time
    (lambda (&optional specified-time)
      "Return the current time, as a float number of seconds since the epoch.
If an argument is given, it specifies a time to convert to float
instead of the current time.  The argument should have the forms:
 (HIGH . LOW) or (HIGH LOW USEC) or (HIGH LOW . USEC).

WARNING: Since the result is floating point, it may not be exact.
Do not use this function if precise time stamps are required."
      (let ((time (or specified-time (current-time))))
	(+ (* (car time) 65536.0)
	   (cadr time)
	   (cond ((consp (setq time (cddr time)))
		  (/ (car time) 1000000.0))
		 (time
		  (/ time 1000000.0))
		 (t
		  0)))))))

(defsubst w3m-url-local-p (url)
  "If URL points a file on the local system, return non-nil value.
Otherwise return nil."
  (string-match "\\`file:" url))

(defconst w3m-url-authinfo-regexp
  "\\`\\([^:/?#]+:\\)?//\\([^/?#:]+\\)\\(:\\([^/?#@]+\\)\\)?@"
  "Regular expression for parsing the authentication part of a URI reference")

(defsubst w3m-url-authinfo (url)
  "Return a user name and a password to authenticate URL."
  (when (string-match w3m-url-authinfo-regexp url)
    (cons (match-string 2 url)
	  (match-string 4 url))))

(defsubst w3m-url-strip-authinfo (url)
  "Remove the authentication part from the URL."
  (if (string-match w3m-url-authinfo-regexp url)
      (concat (match-string 1 url)
	      "//"
	      (substring url (match-end 0)))
    url))

(defsubst w3m-url-strip-fragment (url)
  "Remove the fragment identifier from the URL."
  (if (string-match "\\`\\([^#]*\\)#" url)
      (match-string 1 url)
    url))

(defsubst w3m-url-strip-query (url)
  "Remove the query part and the fragment identifier from the URL."
  (if (string-match "\\`\\([^?#]*\\)[?#]" url)
      (match-string 1 url)
    url))

(defun w3m-get-server-hostname (url)
  "Extract a server root from URL."
  (when (string-match "\\`about://[^/?#]+/" url)
    (setq url (substring url (match-end 0))))
  (setq url (w3m-url-strip-authinfo url))
  (if (string-match "\\`[^:/?#]+://\\([^/?#]+\\)" url)
      (downcase (match-string 1 url))
    url))

(defsubst w3m-which-command (command)
  (when (stringp command)
    (if (and (file-name-absolute-p command)
	     (file-executable-p command))
	command
      (setq command (file-name-nondirectory command))
      (catch 'found-command
	(let (bin)
	  (dolist (dir exec-path)
	    (when (or (file-executable-p
		       (setq bin (expand-file-name command dir)))
		      (file-executable-p
		       (setq bin (expand-file-name (concat command ".exe") dir))))
	      (throw 'found-command bin))))))))

(defun w3m-cancel-refresh-timer (&optional buffer)
  "Cancel the timer for REFRESH attribute in META tag."
  (when w3m-use-refresh
    (with-current-buffer (or buffer (current-buffer))
      (setq w3m-current-refresh nil)
      (when w3m-refresh-timer
	(cancel-timer w3m-refresh-timer)
	(setq w3m-refresh-timer nil)))))

(defalias 'w3m-truncate-string
  (if (featurep 'xemacs)
      (lambda (str end-column)
	"Truncate string STR to end at column END-COLUMN."
	(let ((len (length str))
	      (column 0)
	      (idx 0))
	  (condition-case nil
	      (while (< column end-column)
		(setq column (+ column (char-width (aref str idx)))
		      idx (1+ idx)))
	  (args-out-of-range (setq idx len)))
	  (when (> column end-column)
	    (setq idx (1- idx)))
	  (substring str 0 idx)))
    'truncate-string))

(defsubst w3m-assoc-ignore-case (name alist)
  "Return the element of ALIST whose car equals NAME ignoring its case."
  (let ((dname (downcase name))
	match)
    (while alist
      (when (and (consp (car alist))
		 (string= dname (downcase (car (car alist)))))
	(setq match (car alist)
	      alist nil))
      (setq alist (cdr alist)))
    match))

(defun w3m-prin1 (object &optional stream)
  "Like `prin1', except that control chars will be represented with ^ as
`cat -v' does."
  (if (stringp object)
      (let (rest)
	(dolist (char (append object nil) rest)
	  (cond ((eq char ?\C-?)
		 (push "^?" rest))
		((or (memq char '(?\t ?\n))
		     (>= char ?\ ))
		 (push (char-to-string char) rest))
		(t
		 (push (concat "^" (char-to-string (+ 64 char))) rest))))
	(prin1 (apply 'concat (nreverse rest)) stream))
    (prin1 object stream)))

(defvar w3m-display-message-enable-logging nil
  "*If non-nil, preserve echo messages in the *Message* buffer.")

(defmacro w3m-display-message (string &rest args)
  "Like `message', except that message logging is controlled by the
variable `w3m-display-message-enable-logging'."
  (if (featurep 'xemacs)
      (if args
	  `(display-message (if w3m-display-message-enable-logging
				'message
			      'no-log)
	     (format ,string ,@args))
	`(display-message (if w3m-display-message-enable-logging
			      'message
			    'no-log)
	   ,string))
    `(let ((message-log-max (if w3m-display-message-enable-logging
				message-log-max)))
       (message ,string ,@args))))

(defun w3m-display-progress-message (url)
  "Show \"Reading URL...\" message in the middle of a buffer."
  (insert (make-string (max 0 (/ (1- (window-height)) 2)) ?\n)
	  "Reading " (w3m-url-strip-authinfo url) "...")
  (beginning-of-line)
  (let ((fill-column (window-width)))
    (center-region (point) (point-max)))
  (goto-char (point-min))
  (sit-for 0))

(if (featurep 'xemacs)
    (defalias 'w3m-function-max-args 'function-max-args)
  (eval-and-compile
    (require 'advice))
  (defun w3m-function-max-args (function)
    "Return the maximum number of arguments a function may be called with.
The function may be any form that can be passed to `funcall',
any special form, or any macro.
If the function takes an arbitrary number of arguments or is
a built-in special form, nil is returned."
    (let ((arglist (ad-arglist (if (symbolp function)
				   (symbol-function function)
				 function))))
      (cond ((memq '&rest arglist)
	     nil)
	    ((memq '&optional arglist)
	     (1- (length arglist)))
	    (t
	     (length arglist))))))

(defun w3m-modify-plist (plist &rest properties)
  "Change values in PLIST corresponding to PROPERTIES.  This is similar
to `plist-put', but handles plural symbol and value pairs and remove
pairs from PLIST whose value is nil."
  (while properties
    (setq plist (plist-put plist (car properties) (cadr properties))
	  properties (cddr properties)))
  (while plist
    (if (cadr plist)
	(setq properties (nconc properties (list (car plist) (cadr plist)))))
    (setq plist (cddr plist)))
  properties)

(defmacro w3m-insert-string (string)
  "Insert STRING at point without conversions in either case the
multibyteness of the buffer."
  (if (and (fboundp 'string-as-multibyte)
	   (subrp (symbol-function 'string-as-multibyte)))
      `(let ((string ,string))
	 (insert (if enable-multibyte-characters
		     (string-as-multibyte string)
		   (string-as-unibyte string))))
    `(insert ,string)))

(defconst w3m-default-face-colors
  (eval '(if (not (or (featurep 'xemacs)
		      (>= emacs-major-version 21)))
	     (let ((bg (face-background 'default))
		   (fg (face-foreground 'default)))
	       (append (if bg `(:background ,bg))
		       (if fg `(:foreground ,fg))))))
  "The initial `default' face color spec.  Since `defface' under FSF Emacs
versions prior to 21 won't inherit the `dafault' face colors by default,
we will use this value for the default `defface' color spec.")

(defun w3m-custom-hook-initialize (symbol value)
  "Initialize the hook option pointed by the SYMBOL with the default VALUE."
  (if (boundp symbol)
      (progn
	(setq value (eval value))
	(while value
	  (add-hook symbol (car value))
	  (setq value (cdr value))))
    (custom-initialize-set symbol value)))

(provide 'w3m-util)

;;; w3m-util.el ends here

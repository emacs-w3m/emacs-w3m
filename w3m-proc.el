;;; w3m-proc.el --- Generic functions to control sub-processes.

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

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

;; This module is a part of emacs-w3m which provides generic functions
;; to control sub-processes.  Visit <URL:http://emacs-w3m.namazu.org/>
;; for more details of emacs-w3m.

;;; Code:
(require 'w3m-macro)

(eval-when-compile
  (require 'cl))

(defcustom w3m-process-connection-type
  (not (and (featurep 'xemacs)
	    (string-match "solaris" system-configuration)))
  "*Process connection type for w3m execution."
  :group 'w3m
  :type 'boolean)

(eval-when-compile
  ;; Variable(s) which are used in the following inline functions.
  ;; They should be defined in the other module at run-time.
  (defvar w3m-current-url)
  (defvar w3m-current-buffer)
  (defvar w3m-profile-directory)
  (defvar w3m-terminal-coding-system)
  (defvar w3m-command)
  (defvar w3m-command-arguments)
  (defvar w3m-command-environment)
  (defvar w3m-async-exec))

(defconst w3m-process-max 5 "The maximum limit of the working processes.")
(defvar w3m-processes nil "List of working processes.")

(defvar w3m-process-exit-status nil "The last exit status of a process.")
(defvar w3m-process-user-alist nil)

(defvar w3m-process-user nil)
(defvar w3m-process-passwd nil)
(defvar w3m-process-user-counter 0)
(defvar w3m-process-realm nil)
(defvar w3m-process-object nil)
(defvar w3m-process-temp-file nil)
(make-variable-buffer-local 'w3m-process-user)
(make-variable-buffer-local 'w3m-process-passwd)
(make-variable-buffer-local 'w3m-process-user-counter)
(make-variable-buffer-local 'w3m-process-realm)
(make-variable-buffer-local 'w3m-process-object)
(make-variable-buffer-local 'w3m-process-temp-file)

(defvar w3m-process-proxy-user nil "User name of the proxy server.")
(defvar w3m-process-proxy-passwd nil "Password of the proxy server.")


(defmacro w3m-process-with-environment (alist &rest body)
  "Set the environment variables with ALIST, and evaluate BODY."
  (` (let ((process-environment process-environment)
	   (temporary-file-directory
	    (if (file-directory-p w3m-profile-directory)
		(file-name-as-directory w3m-profile-directory)
	      temporary-file-directory))
	   (default-directory
	    (cond ((file-directory-p w3m-profile-directory)
		   (file-name-as-directory w3m-profile-directory))
		  ((file-directory-p (expand-file-name "~/"))
		   (expand-file-name "~/"))
		  (t temporary-file-directory))))
       (dolist (pair (, alist))
	 (setenv (car pair) (cdr pair)))
       (,@ body))))
(put 'w3m-process-with-environment 'lisp-indent-function 1)
(put 'w3m-process-with-environment 'edebug-form-spec '(form body))

(defsubst w3m-process-p (object)
  "Return t if OBJECT is a `w3m-process' object."
  (and (consp object)
       (vectorp (cdr object))
       (eq 'w3m-process-object (aref (cdr object) 0))))

(defmacro w3m-process-new (arguments buffer &optional process handlers)
  "Return a new `w3m-process' object."
  `(cons ,arguments
	 (vector 'w3m-process-object
		 ,buffer
		 ,process
		 ,handlers)))

(defmacro w3m-process-arguments (object)
  `(car ,object))
(defmacro w3m-process-buffer (object)
  `(aref (cdr ,object) 1))
(defmacro w3m-process-process (object)
  `(aref (cdr ,object) 2))
(defmacro w3m-process-handlers (object)
  `(aref (cdr ,object) 3))

(defmacro w3m-process-handler-new (buffer parent-buffer function)
  `(vector ,buffer ,parent-buffer ,function))
(defmacro w3m-process-handler-buffer (handler)
  `(aref ,handler 0))
(defmacro w3m-process-handler-parent-buffer (handler)
  `(aref ,handler 1))
(defmacro w3m-process-handler-function (handler)
  `(aref ,handler 2))

(defun w3m-process-push (handler arguments)
  "Generate a new `w3m-process' object which is provided by HANDLER,
ARGUMENTS and this buffer, regist it to `w3m-processes', and return
it."
  (let ((x (assoc arguments w3m-processes)))
    (unless x
      (setq x (w3m-process-new arguments (current-buffer)))
      (push x w3m-processes))
    (push (w3m-process-handler-new (current-buffer) w3m-current-buffer handler)
	  (w3m-process-handlers x))
    (setq w3m-process-object x)))

(defsubst w3m-process-kill-process (process)
  "Kill process PROCESS safely."
  (set-process-filter process 'ignore)
  (set-process-sentinel process 'ignore)
  (kill-process process))

(defun w3m-process-start-internal ()
  "Start a process which is registerd in `w3m-process' if the number
of current working processes is less than `w3m-process-max'."
  (let ((num 0))
    (catch 'last
      (dolist (obj (reverse w3m-processes))
	(if (buffer-name (w3m-process-buffer obj))
	    (if (> (incf num) w3m-process-max)
		(throw 'last nil)
	      (unless (w3m-process-process obj)
		(with-current-buffer (w3m-process-buffer obj)
		  (w3m-process-with-environment w3m-command-environment
		    (let ((proc
			   (apply 'start-process w3m-command
				  (current-buffer) w3m-command
				  (w3m-process-arguments w3m-process-object))))
		      (setf (w3m-process-process obj) proc)
		      (setq w3m-process-user nil
			    w3m-process-passwd nil
			    w3m-process-user-counter 2
			    w3m-process-realm nil)
		      (set-process-filter proc 'w3m-process-filter)
		      (set-process-sentinel proc 'w3m-process-sentinel)
		      (process-kill-without-query proc))))))
	  ;; Something wrong has occuered ?
	  (setq w3m-processes (delq obj w3m-processes))
	  (when (w3m-process-process obj)
	    (w3m-process-kill-process (w3m-process-process obj))))))))

(defun w3m-process-stop (buffer)
  (interactive (list (current-buffer)))
  (setq w3m-processes
	(delq nil
	      (mapcar
	       (lambda (obj)
		 (let ((handlers
			(delq nil
			      (mapcar
			       (lambda (handler)
				 (unless (eq buffer
					     (w3m-process-handler-parent-buffer
					      handler))
				   handler))
			       (w3m-process-handlers obj)))))
		   (if handlers
		       (w3m-process-new
			(w3m-process-arguments obj)
			(w3m-process-buffer obj)
			(w3m-process-process obj)
			(if (memq (w3m-process-buffer obj)
				  (mapcar (lambda (x)
					    (w3m-process-handler-buffer x))
					  handlers))
			    handlers
			  (cons (lambda (x) (kill-buffer (current-buffer)))
				handlers)))
		     (when (w3m-process-process obj)
		       (w3m-process-kill-process (w3m-process-process obj)))
		     (dolist (handler (w3m-process-handlers obj))
		       (kill-buffer (w3m-process-handler-buffer handler)))
		     nil)))
	       w3m-processes))))

(defun w3m-process-shutdown ()
  (let ((list w3m-processes))
    (setq w3m-processes nil)
    (dolist (obj list)
      (when (buffer-name (w3m-process-buffer obj))
	(when (w3m-process-process obj)
	  (w3m-process-kill-process (w3m-process-process obj))))
      (w3m-kill-buffer (w3m-process-buffer obj)))))

(defmacro w3m-process-with-null-handler (&rest body)
  "Generate the null handler, and evaluate BODY.
When BODY is evaluated, the local variable `handler' is set to nil."
  `(let (handler) ,@body))
(put 'w3m-process-with-null-handler 'lisp-indent-function 0)
(put 'w3m-process-with-null-handler 'edebug-form-spec '(body))

(defmacro w3m-process-with-wait-handler (&rest body)
  "Generate the waiting handler, and evaluate BODY.
When BODY is evaluated, the local variable `handler' keeps the handler
which will wait for the end of the evaluation."
  (let ((tempvar (make-symbol "tempvar")))
    `(let ((,tempvar ',tempvar))
       (let ((handler (lambda (x) (setq ,tempvar x))))
	 (let (w3m-async-exec)
	   ,@body)
	 ,tempvar))))
;; ASYNC: 本当は、以下のように非同期的に実行して、非同期プロセスの終了
;; を待つように実装する必要があるのだが、どうしても、バグの原因が分か
;; らなかったので、adhoc に対処している。そのため、パスワードなどの入
;; 力が必要な場合に不具合が生じることがある。
;;
;;    (defmacro w3m-process-with-wait-handler (&rest body)
;;      "Generate the waiting handler, and evaluate BODY.
;;    When BODY is evaluated, the local variable `handler' keeps the handler
;;    which will wait for the end of the evaluation."
;;      (let ((tempvar (make-symbol "tempvar")))
;;        `(let ((,tempvar ',tempvar))
;;           (let ((handler (lambda (x) (setq ,tempvar x))))
;;             ,@body)
;;           (while (eq ,tempvar ',tempvar)
;;             (sit-for 0.2))
;;           ,tempvar)))
;;
;; 具体的には、[emacs-w3m:02167] で報告した以下の式が正常に評価できる
;; ように修正する必要がある。
;;
;;     (with-current-buffer (get-buffer-create "*TEST*")
;;       (pop-to-buffer (current-buffer))
;;       (require 'w3m)
;;       (w3m-process-with-null-handler
;;         (w3m-process-do
;;             (success (w3m-process-start handler "-version"))
;;           (w3m-process-with-wait-handler
;;             (w3m-process-start handler "-version")))))
;;
(put 'w3m-process-with-wait-handler 'lisp-indent-function 0)
(put 'w3m-process-with-wait-handler 'edebug-form-spec '(body))

(defmacro w3m-process-do (spec &rest body)
  "(w3m-process-do (VAR ASYNC-FORM) POST-BODY...): Evaluate ASYNC-FORM
and return a `w3m-process' object immediately.  After an asynchronous
sub-process which is started in ASYNC-FORM, POST-BODY will be
evaluated with the variable VAR which will be set to the result of
ASYNC-FORM."
  (let ((var (car spec))
	(form (cdr spec))
	(evaluated-p (gensym "--evaluated-p--")))
    `(lexical-let ((,evaluated-p ',evaluated-p))
       (labels ((post-handler
		 (,var handler)
		 (when (eq ,evaluated-p ',evaluated-p)
		   (setq ,evaluated-p (inline ,@body))
		   (if (w3m-process-p ,evaluated-p)
		       ,evaluated-p
		     (funcall (or handler (function identity))
			      ,evaluated-p)))))
	 (let ((handler (list 'lambda (list ',var)
			      (list 'post-handler ',var handler))))
	   (let ((,var (inline ,@form)))
	     (if (w3m-process-p ,var)
		 ,var
	       (funcall handler ,var))))))))
(put 'w3m-process-do 'lisp-indent-function 1)
(put 'w3m-process-do 'edebug-form-spec '((symbolp form) def-body))

(defmacro w3m-process-do-with-temp-buffer (spec &rest body)
  "(w3m-process-do-with-temp-buffer (VAR ASYNC-FORM) POST-BODY...):
Like `w3m-process-do', but all forms are evaluated in a temporary
buffer."
  (let ((var (car spec))
	(form (cdr spec))
	(evaluated-p (gensym "--evaluated-p--"))
	(temp-buffer (gensym "--temp-buffer--")))
    `(lexical-let ((,evaluated-p ',evaluated-p)
		   (,temp-buffer))
       (labels ((post-handler
		 (,var handler)
		 (and (eq ,evaluated-p ',evaluated-p)
		      (buffer-name ,temp-buffer)
		      ;; ASYNC: unwind-protect で保護されているのは、
		      ;; このマクロで登録されたハンドラ部分のみで、こ
		      ;; れ以外のマクロで登録された場合は対象になって
		      ;; いない。これは通常の unwind-protect の入れ子
		      ;; 構造とは異なり、確実に kill-buffer されないか
		      ;; もしれない。
		      (unwind-protect
			  (with-current-buffer ,temp-buffer
			    (setq ,evaluated-p (inline ,@body))
			    (if (w3m-process-p ,evaluated-p)
				,evaluated-p
			      (funcall (or handler (function identity))
				       ,evaluated-p)))
			(w3m-kill-buffer ,temp-buffer)))))
	 (let ((handler (list 'lambda (list ',var)
			      (list 'post-handler ',var handler))))
	   (let ((,var (with-current-buffer
			   (setq ,temp-buffer
				 (w3m-get-buffer-create
				  (generate-new-buffer-name
				   w3m-work-buffer-name)))
			 ,@form)))
	     (if (w3m-process-p ,var)
		 ,var
	       (funcall handler ,var))))))))
(put 'w3m-process-do-with-temp-buffer 'lisp-indent-function 1)
(put 'w3m-process-do-with-temp-buffer 'edebug-form-spec
     '((symbolp form) def-body))


(defun w3m-process-start (handler &rest arguments)
  "Run `w3m-command' with HANDLER and ARGUMENTS."
  (let ((output-buffer (current-buffer))
	(coding-system-for-read 'binary)
	(coding-system-for-write w3m-terminal-coding-system)
	(default-process-coding-system
	  (cons 'binary w3m-terminal-coding-system))
	(process-connection-type w3m-process-connection-type))
    (setq arguments (append w3m-command-arguments arguments))
    (w3m-process-do
	(exit-status
	 (if w3m-async-exec
	     (prog1 (w3m-process-push handler arguments)
	       (w3m-process-start-internal))
	   (w3m-process-with-environment w3m-command-environment
	     (apply 'call-process w3m-command nil t nil arguments))))
      (cond
       ((numberp exit-status)
	(zerop (setq w3m-process-exit-status exit-status)))
       ((not exit-status) nil)
       (t
	(setq w3m-process-exit-status
	      (string-as-multibyte (format "%s" exit-status)))
	nil)))))

(defun w3m-process-sentinel (process event)
  ;; Ensure that this function will be never called repeatedly.
  (set-process-sentinel process 'ignore)
  (unwind-protect
      (if (buffer-name (process-buffer process))
	  (with-current-buffer (process-buffer process)
	    (setq w3m-processes (delq w3m-process-object w3m-processes))
	    (let ((exit-status (process-exit-status process))
		  (buffer (current-buffer))
		  (realm  w3m-process-realm)
		  (user   w3m-process-user)
		  (passwd w3m-process-passwd)
		  (obj    w3m-process-object))
	      (setq w3m-process-object nil)
	      (dolist (x (w3m-process-handlers obj))
		(when (buffer-name (w3m-process-handler-buffer x))
		  (with-current-buffer (w3m-process-handler-buffer x)
		    (unless (eq buffer (current-buffer))
		      (insert-buffer buffer)))))
	      (dolist (x (w3m-process-handlers obj))
		(when (buffer-name (w3m-process-handler-buffer x))
		  (with-current-buffer (w3m-process-handler-buffer x)
		    (let ((w3m-process-exit-status)
			  (w3m-current-buffer
			   (w3m-process-handler-parent-buffer x)))
		      (w3m-process-set-user w3m-current-url realm user passwd)
		      (funcall (w3m-process-handler-function x)
			       exit-status)))))))
	;; Something wrong has been occured.
	(catch 'last
	  (dolist (obj (copy-sequence w3m-processes))
	    (when (eq process (w3m-process-process obj))
	      (setq w3m-processes (delq obj w3m-processes))))))
    (delete-process process)
    (w3m-process-start-internal)))

(defun w3m-process-filter (process string)
  (when (buffer-name (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((buffer-read-only nil)
	    (case-fold-search nil))
	(goto-char (process-mark process))
	(insert string)
	(set-marker (process-mark process) (point))
	(unless (string= "" string)
	  (goto-char (point-min))
	  (cond
	   ((and (looking-at
		  "\\(\n?Wrong username or password\n\\)?Proxy Username for \\(.*\\): Proxy Password: ")
		 (= (match-end 0) (point-max)))
	    (unless w3m-process-proxy-passwd
	      (setq w3m-process-proxy-passwd
		    (read-passwd "Proxy Password: ")))
	    (condition-case nil
		(progn
		  (process-send-string process
				       (concat w3m-process-proxy-passwd "\n"))
		  (delete-region (point-min) (point-max)))
	      (error nil)))
	   ((and (looking-at
		  "\\(\n?Wrong username or password\n\\)?Proxy Username for \\(.*\\): ")
		 (= (match-end 0) (point-max)))
	    (unless w3m-process-proxy-user
	      (setq w3m-process-proxy-user
		    (read-from-minibuffer (concat
					   "Proxy Username for "
					   (match-string 2) ": "))))
	    (condition-case nil
		(process-send-string process
				     (concat w3m-process-proxy-user "\n"))
	      (error nil)))
	   ((and (looking-at
		  "\\(\n?Wrong username or password\n\\)?Username for \\(.*\\)\n?: Password: ")
		 (= (match-end 0) (point-max)))
	    (setq w3m-process-realm (match-string 2))
	    (setq w3m-process-passwd
		  (or (and (stringp w3m-current-url)
			   (w3m-process-get-passwd
			    w3m-current-url w3m-process-realm w3m-process-user))
		      (read-passwd
		       (format "Password for %s: " w3m-process-realm))))
	    (condition-case nil
		(progn
		  (process-send-string process
				       (concat w3m-process-passwd "\n"))
		  (delete-region (point-min) (point-max)))
	      (error nil)))
	   ((and (looking-at
		  "\\(\n?Wrong username or password\n\\)?Username for \\(.*\\)\n?: ")
		 (= (match-end 0) (point-max)))
	    (setq w3m-process-realm (match-string 2))
	    (setq w3m-process-user
		  (or (and (stringp w3m-current-url)
			   (w3m-process-get-user w3m-current-url
						 w3m-process-realm))
		      (read-from-minibuffer (format "Username for %s: "
						    w3m-process-realm))))
	    (condition-case nil
		(process-send-string process
				     (concat w3m-process-user "\n"))
	      (error nil)))))))))

(defun w3m-process-get-server-root (url)
  "Get server root for realm."
  (if (string-match "^[^/]*/+\\([^/]+\\)" url)
      (downcase (match-string 1 url))
    url))

;; w3m-process-user-alist has an association list as below format.
;; (("root1" ("realm11" ("user11" "pass11")
;;                      ("user12" "pass12"))
;;           ("realm12" ("user13" "pass13")))
;;  ("root2" ("realm21" ("user21" "pass21"))))
(defun w3m-process-get-user (url realm &optional multi)
  "Get user from arrived-user-alist."
  (if (= w3m-process-user-counter 0)
      nil
    (let (userlst)
      (setq userlst
	    (cdr (assoc realm
			(cdr (assoc (w3m-process-get-server-root url)
				    w3m-process-user-alist)))))
      (when userlst
	(setq w3m-process-user-counter (1- w3m-process-user-counter))
	(cond
	 (multi userlst)
	 ((= (length userlst) 1)
	  ;; single user
	  (car (car userlst)))
	 (t
	  ;; have multi user
	  (completing-read (format "Select Username for %s: " realm)
			   (mapcar (lambda (x) (cons (car x) (car x)))
				   userlst)
			   nil t)))))))

(defun w3m-process-get-passwd (url realm user)
  "Get passwd from arrived-user-alist."
  (if (= w3m-process-user-counter 0)
      nil
    (let (pass)
      (setq pass
	    (cdr
	     (assoc user
		    (cdr
		     (assoc realm
			    (cdr (assoc (w3m-process-get-server-root url)
					w3m-process-user-alist)))))))
      (when pass
	(setq w3m-process-user-counter (1- w3m-process-user-counter)))
      pass)))

(defun w3m-process-set-user (url realm user pass)
  (when (and url realm user pass)
    (let* ((root (w3m-process-get-server-root url))
	   (tmproot (cdr (assoc root w3m-process-user-alist)))
	   (tmprealm (cdr (assoc realm tmproot)))
	   (tmpuser (assoc user tmprealm))
	   (tmppass (cdr tmpuser))
	   (w3m-process-user-counter 2))
      (cond
       ((and tmproot tmprealm tmpuser tmppass (string= pass tmppass))
	;; nothing to do
	nil)
       ((and tmproot tmprealm tmpuser)
	;; passwd change
	(setcdr tmpuser pass))
       ((and tmproot tmprealm)
	;; add user and passwd
	(nconc tmprealm (list (cons user pass))))
       (tmproot
	;; add realm, user, and passwd
	(nconc tmproot (list (cons realm (list (cons user pass))))))
       (t
	;; add root, realm, user, and passwd
	(setq w3m-process-user-alist
	      (append
	       (list (cons root (list (cons realm (list (cons user pass))))))
	       w3m-process-user-alist)))))))

(provide 'w3m-proc)
;;; w3m-proc.el ends here.

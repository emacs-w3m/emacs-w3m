;;; w3m-util.el --- Utility macros and functions for emacs-w3m

;; Copyright (C) 2001, 2002, 2003, 2004, 2005
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

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
  (require 'cl))

;; Variables and functions which are used in the following inline
;; functions.  They should be defined in the other module at run-time.
(eval-when-compile
  (defvar w3m-current-process)
  (defvar w3m-current-refresh)
  (defvar w3m-current-title)
  (defvar w3m-current-url)
  (defvar w3m-pop-up-frames)
  (defvar w3m-pop-up-windows)
  (defvar w3m-popup-frame-parameters)
  (defvar w3m-refresh-timer)
  (defvar w3m-select-buffer-name)
  (defvar w3m-use-refresh)
  (defvar w3m-use-tab)
  (defvar w3m-work-buffer-list)
  (unless (fboundp 'select-frame-set-input-focus)
    (defalias 'select-frame-set-input-focus 'ignore)))

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
	(< emacs-major-version 20))
    (require 'poe)
    (require 'poem))))

(eval-and-compile
  (cond ((= emacs-major-version 19)
	 (autoload 'cancel-timer "timer")
	 (autoload 'regexp-opt "regexp-opt")
	 (require 'custom))
	((featurep 'xemacs)
	 (autoload 'cancel-timer "w3m-xmas"))))

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
	   ;; Default to front-nonsticky and rear-sticky in Emacsen.
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

(eval-and-compile
  ;; `eval-and-compile' is necessary since the value of the constant
  ;; is referred to at the compile time.
  (defconst w3m-html-string-regexp
    "\\(\"\\([^\"]+\\)\"\\|'\\([^\']+\\)'\\|[^\"\'<> \t\r\f\n]*\\)"
    "Regexp matching a string of the field-value like <a href=\"VALUE\">."))

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
				 (format "%s\\([ \t\r\f\n]*=[ \t\r\f\n]*%s\\)?"
					 (symbol-name attr)
					 w3m-html-string-regexp)
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

(defsubst w3m-current-title ()
  "Return the title of the current buffer."
  (cond
   ((and (stringp w3m-current-title)
	 (not (string= w3m-current-title "<no-title>")))
    w3m-current-title)
   ((stringp w3m-current-url)
    (directory-file-name
     (if (string-match "^[^/:]+:/+" w3m-current-url)
	 (substring w3m-current-url (match-end 0))
       w3m-current-url)))
   (t "<no-title>")))

(defsubst w3m-buffer-title (buffer)
  "Return the title of the buffer BUFFER."
  (with-current-buffer buffer
    (w3m-current-title)))

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

(defun w3m-list-buffers (&optional nosort)
  "Return a list of buffers in which emacs-w3m sessions are open.
If the optional NOSORT is nil, the list is sorted in the order of
buffer names."
  (let ((buffers (buffer-list))
	buffer rest)
    (save-current-buffer
      (while buffers
	(set-buffer (setq buffer (pop buffers)))
	(when (eq major-mode 'w3m-mode)
	  (push buffer rest))))
    (if nosort
	(nreverse rest)
      (sort rest #'w3m-buffer-name-lessp))))


;;; Pop up and delete buffers, windows or frames:

(defmacro w3m-popup-frame-parameters ()
  "Return a pop-up frame plist if this file is compiled for XEmacs,
otherwise return an alist."
  (if (featurep 'xemacs)
      '(let ((params (or w3m-popup-frame-parameters pop-up-frame-plist)))
	 (if (consp (car-safe params))
	     (alist-to-plist params)
	   params))
    '(let ((params (or w3m-popup-frame-parameters pop-up-frame-alist))
	   alist)
       (if (consp (car-safe params))
	   params
	 (while params
	   (push (cons (car params) (cdr params)) alist)
	   (setq params (cddr params)))
	 (nreverse alist)))))

(defmacro w3m-popup-frame-p ()
  "Return non-nil if `w3m-pop-up-frames' is non-nil and Emacs really
supports separate frames."
  (if (featurep 'xemacs)
      '(and w3m-pop-up-frames (device-on-window-system-p))
    '(and w3m-pop-up-frames window-system)))

(defmacro w3m-use-tab-p ()
  "Return non-nil if `w3m-use-tab' is non-nil and Emacs really supports
the tabs line."
  (cond ((featurep 'xemacs)
	 '(and w3m-use-tab (device-on-window-system-p)))
	((<= emacs-major-version 19)
	 nil)
	(t
	 '(and w3m-use-tab (>= emacs-major-version 21)))))

(defmacro w3m-popup-window-p ()
  "Return non-nil if `w3m-pop-up-windows' is non-nil and the present
situation allows it."
  (cond ((featurep 'xemacs)
	 '(and w3m-pop-up-windows
	       (not (w3m-use-tab-p))
	       (not (get-buffer-window w3m-select-buffer-name))))
	((<= emacs-major-version 19)
	 '(and w3m-pop-up-windows
	       (not (get-buffer-window w3m-select-buffer-name))))
	(t
	 '(and w3m-pop-up-windows
	       (or (< emacs-major-version 21)
		   (not (w3m-use-tab-p)))
	       (not (get-buffer-window w3m-select-buffer-name))))))

(defvar w3m-initial-frames nil
  "Variable used to keep a list of the frame-IDs when emacs-w3m sessions
are popped-up as new frames.  This variable is used for the control
for not deleting frames made for aims other than emacs-w3m sessions.")
(make-variable-buffer-local 'w3m-initial-frames)

(defun w3m-popup-buffer (buffer)
  "Pop up BUFFER as a new window or a new frame
according to `w3m-pop-up-windows' and `w3m-pop-up-frames' (which see)."
  (let ((window (get-buffer-window buffer t))
	(oframe (selected-frame))
	(popup-frame-p (w3m-popup-frame-p))
	frame pop-up-frames buffers other)
    (if (setq
	 pop-up-frames
	 (if window ;; The window showing BUFFER already exists.
	     ;; Don't pop up a new frame if it is just the current frame.
	     (not (eq (setq frame (window-frame window)) oframe))
	   ;; There is no window for BUFFER, so look for the existing
	   ;; emacs-w3m window if the tabs line is enabled or the
	   ;; selection window exists (i.e., we can reuse it).
	   (if (or (w3m-use-tab-p)
		   (get-buffer-window w3m-select-buffer-name t))
	       (progn
		 (setq buffers (delq buffer (w3m-list-buffers t)))
		 (while (and (not window)
			     buffers)
		   (setq window
			 (get-buffer-window (setq other (pop buffers)) t)))
		 (if window ;; The window showing another buffer exists.
		     (not (eq (setq frame (window-frame window)) oframe))
		   (setq other nil)
		   ;; There is no window after all, so leave to the value
		   ;; of `w3m-pop-up-frames' whether to pop up a new frame.
		   popup-frame-p))
	     ;; Ditto.
	     popup-frame-p)))
	(progn
	  (cond (other
		 ;; Pop up another emacs-w3m buffer and switch to BUFFER.
		 (pop-to-buffer other)
		 ;; Change the value for BUFFER's `w3m-initial-frames'.
		 (setq w3m-initial-frames
		       (prog1
			   (copy-sequence w3m-initial-frames)
			 (switch-to-buffer buffer))))
		(frame
		 ;; Pop up the existing frame which shows BUFFER.
		 (pop-to-buffer buffer))
		(t
		 ;; Pop up a new frame.
		 (let* ((pop-up-frame-alist (w3m-popup-frame-parameters))
			(pop-up-frame-plist pop-up-frame-alist))
		   (pop-to-buffer buffer))
		 (setq frame (window-frame (get-buffer-window buffer t)))))
	  ;; Raise, select and focus the frame.
	  (if (fboundp 'select-frame-set-input-focus)
	      (select-frame-set-input-focus frame)
	    (raise-frame frame)
	    (select-frame frame)
	    (w3m-static-when (featurep 'xemacs)
	      (focus-frame frame))))
      ;; Simply switch to BUFFER in the current frame.
      (if (w3m-popup-window-p)
	  (let ((pop-up-windows t))
	    (pop-to-buffer buffer))
	(switch-to-buffer buffer)))))

(eval-when-compile
  (when (and (fboundp 'select-frame-set-input-focus)
	     (eq (symbol-function 'select-frame-set-input-focus) 'ignore))
    (fmakunbound 'select-frame-set-input-focus)))

(defun w3m-add-w3m-initial-frames (&optional frame)
  "Add FRAME into `w3m-initial-frames', the buffer-local variable.
It is done when FRAME is newly created for the emacs-w3m session.
This function is added to the hook which is different with the Emacs
version as follows:

XEmacs          `create-frame-hook'
Emacs 20-22     `after-make-frame-functions'
Emacs 19        `after-make-frame-hook'

Note that `after-make-frame-hook' doesn't take an argument."
  (unless frame
    (setq frame (if (and (= emacs-major-version 19)
			 ;; See frame.el in Emacs 19.
			 (boundp 'nframe)
			 (framep (symbol-value 'nframe)))
		    (symbol-value 'nframe)
		  (selected-frame))))
  ;; Share the opened frame in `w3m-initial-frames' over all emacs-w3m
  ;; buffers if `w3m-use-tab' is non-nil.  Otherwise, the frame is
  ;; appended into `w3m-initial-frames' only in the current buffer.
  (with-current-buffer (window-buffer (frame-first-window frame))
    (when (eq major-mode 'w3m-mode)
      (unless (memq frame w3m-initial-frames)
	(push frame w3m-initial-frames))
      (when w3m-use-tab
	(dolist (buffer (delq (current-buffer) (w3m-list-buffers t)))
	  (set-buffer buffer)
	  (unless (memq frame w3m-initial-frames)
	    (push frame w3m-initial-frames)))))))

(add-hook (cond ((featurep 'xemacs)
		 'create-frame-hook)
		((>= emacs-major-version 20)
		 'after-make-frame-functions)
		((= emacs-major-version 19)
		 'after-make-frame-hook))
	  'w3m-add-w3m-initial-frames)

(defun w3m-delete-w3m-initial-frames (frame)
  "Delete FRAME from `w3m-initial-frames', the buffer-local variable.
It is done when the FRAME in which emacs-w3m is running is deleted.
This function is added to `delete-frame-hook' (`delete-frame-functions'
is used instead in Emacs 22) or merged into the `delete-frame' function
using `defadvice'."
  (save-current-buffer
    (dolist (buffer (w3m-list-buffers t))
      (set-buffer buffer)
      (setq w3m-initial-frames (delq frame w3m-initial-frames)))))

(cond ((boundp 'delete-frame-functions)
       (add-hook 'delete-frame-functions 'w3m-delete-w3m-initial-frames))
      ((>= emacs-major-version 21)
       (add-hook 'delete-frame-hook 'w3m-delete-w3m-initial-frames))
      (t
       (defadvice delete-frame (before delete-w3m-initial-frames activate)
	 "Remove the frame to be deleted from `w3m-initial-frames'."
	 (w3m-delete-w3m-initial-frames (or (ad-get-arg 0)
					    (selected-frame))))))

(defun w3m-delete-frames-and-windows (&optional exception)
  "Delete all frames and windows related to emacs-w3m buffers.
If EXCEPTION is a buffer, a window or a frame, it and related visible
objects will not be deleted.  There are special cases; the following
objects will not be deleted:

1. The sole frame in the display device.
2. Frames created not for emacs-w3m sessions.
3. Frames showing not only emacs-w3m sessions but also other windows.\
"
  (let ((buffers (delq exception (w3m-list-buffers t)))
	buffer windows window frame one-window-p flag)
    (save-current-buffer
      (while buffers
	(setq buffer (pop buffers)
	      windows (delq exception
			    (get-buffer-window-list buffer 'no-minibuf t)))
	(set-buffer buffer)
	(while windows
	  (setq window (pop windows)
		frame (window-frame window))
	  (when (and frame
		     (not (eq frame exception)))
	    (setq one-window-p
		  (w3m-static-if (featurep 'xemacs)
		      (one-window-p t frame)
		    ;; Emulate XEmacs version's `one-window-p'.
		    (prog2
			(setq flag nil)
			(catch 'exceeded
			  (walk-windows (lambda (w)
					  (when (eq (window-frame w) frame)
					    (if flag
						(throw 'exceeded nil)
					      (setq flag t))))
					'no-minibuf t)
			  flag)
		      (set-buffer buffer))))
	    (if (and (memq frame w3m-initial-frames)
		     (not (eq (next-frame frame) frame)))
		(if (or
		     ;; A frame having the sole window can be deleted.
		     one-window-p
		     ;; Also a frame having only windows for emacs-w3m
		     ;; sessions or the buffer selection can be deleted.
		     (progn
		       (setq flag t)
		       (walk-windows
			(lambda (w)
			  (when flag
			    (if (eq w exception)
				(setq flag nil)
			      (set-buffer (window-buffer w))
			      (setq flag (memq major-mode
					       '(w3m-mode
						 w3m-select-buffer-mode))))))
			'no-minibuf)
		       (set-buffer buffer)
		       flag))
		    (delete-frame frame)
		  (delete-window window))
	      (unless one-window-p
		(delete-window window)))))))))


;;; Miscellaneous:

(defconst w3m-url-fallback-base "http:///")
(defconst w3m-url-invalid-regexp "\\`http:///")

(defsubst w3m-url-valid (url)
  (and url (not (string-match w3m-url-invalid-regexp url))
       url))

(defmacro w3m-set-match-data (list)
  "Same as the `set-match-data'; convert points into markers under XEmacs."
  (if (featurep 'xemacs)
      `(let ((list ,list))
	 (store-match-data (dolist (pt (prog1 list (setq list nil))
				       (nreverse list))
			     (push (if (markerp pt)
				       pt
				     (set-marker (make-marker) pt))
				   list))))
    `(set-match-data ,list)))

(defun w3m-search-tag-1 (regexp)
  "Subroutine used by `w3m-search-tag'."
  (let ((start (point))
	begin end)
    (if (and (re-search-forward regexp nil t)
	     (setq begin (match-beginning 0)
		   end (match-end 0))
	     (or (looking-at "/?>")
		 (and (looking-at "[ \t\f\n]")
		      (search-forward ">" nil t))))
	(prog1
	    (goto-char (match-end 0))
	  (w3m-set-match-data
	   (cond ((= end (match-beginning 0))
		  (list begin (match-end 0)
			(1+ begin) end))
		 ((eq (char-before (match-beginning 0)) ?/)
		  (if (= end (1- (match-beginning 0)))
		      (list begin (match-end 0)
			    (1+ begin) end)
		    (list begin (match-end 0)
			  (1+ begin) end
			  end (- (match-end 0) 2))))
		 (t
		  (list begin (match-end 0)
			(1+ begin) end
			end (1- (match-end 0)))))))
      (set-match-data nil)
      (goto-char start)
      nil)))

(defmacro w3m-search-tag (&rest names)
  "Search forward for a tag which begins with one of NAMES.
This macro generates the form equivalent to:

\(re-search-forward \"<\\\\(NAMES\\\\)\\\\([ \\t\\f\\n]+[^>]*\\\\)?/?>\" nil t)

but it works even if the tag is considerably large.

Note: this macro allows only strings for NAMES, that is, a form
something like `(if foo \"bar\" \"baz\")' cannot be used."
  `(w3m-search-tag-1 ,(concat "<" (regexp-opt names t))))

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
  (cond ((featurep 'xemacs)
	 ;; The function of the XEmacs version doesn't work correctly
	 ;; for wide characters.
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
	     (substring str 0 idx))))
	((fboundp 'truncate-string-to-width)
	 'truncate-string-to-width)
	(t
	 'truncate-string)))

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

(def-edebug-spec w3m-insert-string (form))
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
  "The initial `default' face color spec.  Since `defface' under Emacs
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

(defmacro w3m-keep-region-active ()
  "Keep the region active after evaluating this current command.
In XEmacs, `zmacs-region-stays' is set to nil everywhen a command is
evaluated.  This means that the region is always deactivated after
evaluating the current command.  This macro sets t to it, and keeps
the region active."
  (when (featurep 'xemacs)
    '(if (interactive-p)
	 (setq zmacs-region-stays t))))

(defmacro w3m-deactivate-region ()
  "Deactivate the region.
This macro does nothing in XEmacs, because the region is always
deactivated after evaluating the current command."
  (unless (featurep 'xemacs)
    '(deactivate-mark)))

(defmacro w3m-region-active-p ()
  "Say whether the region is active."
  (if (fboundp 'region-active-p)
      (list 'region-active-p)
    (list 'and 'transient-mark-mode 'mark-active)))

(eval-and-compile
  (cond
   ((fboundp 'replace-in-string)
    (defalias 'w3m-replace-in-string 'replace-in-string))
   ((fboundp 'replace-regexp-in-string)
    (defun w3m-replace-in-string  (string regexp newtext &optional literal)
      ;;(replace-regexp-in-string regexp newtext string nil literal)))
      ;;
      ;; Don't call the symbol function `replace-regexp-in-string' directly
      ;; in order to silence the byte-compiler when an Emacs which doesn't
      ;; provide it is used.  The following form generates exactly the same
      ;; byte-code.
      (funcall (symbol-function 'replace-regexp-in-string)
	       regexp newtext string nil literal)))
   (t
    (defun w3m-replace-in-string (string regexp newtext &optional literal)
      (let ((start 0) tail)
	(while (string-match regexp string start)
	  (setq tail (- (length string) (match-end 0)))
	  (setq string (replace-match newtext nil literal string))
	  (setq start (- (length string) tail))))
      string))))

(eval-and-compile
  (if (fboundp 'compare-strings)
      (defalias 'w3m-compare-strings 'compare-strings)
    (defun w3m-compare-strings (string1 start1 end1 string2 start2 end2)
      "Compare the contents of two strings."
      (let* ((str1 (substring string1 start1 end1))
	     (str2 (substring string2 start2 end2))
	     (len (min (length str1) (length str2)))
	     (i 0))
	(if (string= str1 str2)
	    t
	  (setq i (catch 'ignore
		    (while (< i len)
		      (when (not (eq (aref str1 i) (aref str2 i)))
			(throw 'ignore i))
		      (setq i (1+ i)))
		    i))
	  (1+ i))))))

(provide 'w3m-util)

;;; w3m-util.el ends here

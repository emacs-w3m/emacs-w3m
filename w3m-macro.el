;;; w3m-macro.el --- Generic macros and inline functions for emacs-w3m

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

;; This module is a part of emacs-w3m which provides generic macros
;; and inline functions.
;; Visit <URL:http://emacs-w3m.namazu.org/> for more details of emacs-w3m.

;;; Code:

(eval-when-compile
  (require 'cl)
  ;; Variables and functions which are used in the following inline
  ;; functions.  They should be defined in the other module at run-time.
  (defvar w3m-work-buffer-list))

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
  "Like `add-text-properties' but always add the non-sticky properties."
  (let ((non-stickies
	 (if (featurep 'xemacs)
	     ;; Default to start-closed and end-open in XEmacsen.
	     '(list 'start-open t)
	   ;; Default to front-nonsticky and rear-sticky in FSF Emacsen.
	   '(list 'rear-nonsticky t))))
    (` (add-text-properties (, start) (, end)
			    (append (, non-stickies) (, props))
			    (, object)))))

(defmacro w3m-get-text-property-around (prop &optional position)
  "Search for the text property PROP in the POSITION and return a value
or nil.  If POSITION is omitted, searching is performed in the current
cursor position and around there."
  (if position
      (` (get-text-property (, position) (, prop)))
    (` (let ((position (point)))
	 (or (get-text-property position (, prop))
	     (and (not (bolp))
		  (get-text-property (1- position) (, prop)))
	     (and (not (eolp))
		  (get-text-property (1+ position) (, prop))))))))

(defmacro w3m-action (&optional position)
  (` (w3m-get-text-property-around 'w3m-action (, position))))
(defmacro w3m-anchor (&optional position)
  (` (w3m-get-text-property-around 'w3m-href-anchor (, position))))
(defmacro w3m-image (&optional position)
  (` (w3m-get-text-property-around 'w3m-image (, position))))
(defmacro w3m-submit (&optional position)
  (` (w3m-get-text-property-around 'w3m-submit (, position))))

(defmacro w3m-cursor-anchor (&optional position)
  (if position
      (` (get-text-property (, position) 'w3m-cursor-anchor))
    (` (get-text-property (point) 'w3m-cursor-anchor))))


;;; Miscellaneous:

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
  (when (stringp buffer)
    (setq buffer (get-buffer buffer)))
  (when (buffer-live-p buffer)
    (kill-buffer buffer))
  (setq w3m-work-buffer-list (delq buffer w3m-work-buffer-list))
  nil)

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

(defsubst w3m-url-dtree-p (url)
  "If URL points a 'w3m-dtree', return non-nil value.  Otherwise return
nil."
  (string-match "^about://dtree/" url))

(defsubst w3m-url-local-p (url)
  "If URL points a file on the local system, return non-nil value.
Otherwise return nil."
  (string-match "^\\(file:\\|/\\)" url))

(defsubst w3m-which-command (command)
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
	    (throw 'found-command bin)))))))

(provide 'w3m-macro)

;;; w3m-macro.el ends here

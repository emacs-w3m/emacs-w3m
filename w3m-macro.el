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
  ;; Variable(s) which are used in the following inline functions.
  ;; They should be defined in the other module at run-time.
  (defvar w3m-work-buffer-list))

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

(defmacro w3m-with-work-buffer (&rest body)
  "Execute the forms in BODY with working buffer as the current buffer."
  (let ((temp-hist (make-symbol "hist"))
	(temp-flat (make-symbol "flat")))
    (` (let (((, temp-hist) w3m-history)
	     ((, temp-flat) w3m-history-flat))
	 (with-current-buffer
	     (w3m-get-buffer-create w3m-work-buffer-name)
	   (setq w3m-history (, temp-hist)
		 w3m-history-flat (, temp-flat))
	   (,@ body))))))
(put 'w3m-with-work-buffer 'lisp-indent-function 0)
(put 'w3m-with-work-buffer 'edebug-form-spec '(body))

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

(defsubst w3m-get-buffer-create (name)
  "Return the buffer named NAME, or create such a buffer and return it."
  (or (get-buffer name)
      (let ((buf (get-buffer-create name)))
	(setq w3m-work-buffer-list (cons buf w3m-work-buffer-list))
	(buffer-disable-undo buf)
	buf)))

(defmacro w3m-tag-regexp-of (&rest names)
  "Return a regexp string, not a funtion form.  A regexp should match tags
which are started with \"<\" and one of NAMES.  NAMES should be string
constants, any other expressions are not allowed."
  (concat "<\\("
	  (mapconcat 'identity names "\\|")
	  "\\)\\([ \t\r\f\n]+[^>]*\\)?/?>"))

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

;;; w3m-macro.el ends here.

;;; w3m-om.el --- Mule 2 specific functions for w3m

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>

;; Authors: Katsumi Yamaoka    <yamaoka@jpl.org>,
;;          TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;; Keywords: w3m, WWW, hypermedia

;; w3m-om.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2 of the License,
;; or (at your option) any later version.

;; w3m-om.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with w3m.el; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:

;; This file contains the stuffs to use w3m.el on Mule-2.  For more
;; detail about w3m.el, see:
;;
;;    http://namazu.org/~tsuchiya/emacs-w3m/

;;; Code:

(eval-when-compile (require 'cl))

(require 'poe)
(require 'poem)
(require 'pcustom)

(provide 'w3m-om);; It is needed to avoid circular dependencies.
(require 'w3m)


;; Generate some coding-systems which have a modern name.
;; No need to contain the eol-type variants in the following alist
;; because they will also be generated for each coding-system.
(dolist (elem '((*autoconv*	  undecided)
		(*ctext*	  ctext)
		(*euc-china*	  cn-gb-2312)
		(*euc-japan*	  euc-japan)
		(*iso-2022-jp*	  iso-2022-jp)
		(*iso-2022-ss2-7* iso-2022-7bit-ss2)
		(*sjis*		  shift_jis)
		(*tis620*	  tis-620)))
  (unless (coding-system-p (cadr elem))
    (let* ((from (car elem))
	   (to (cadr elem))
	   (info-vector (copy-sequence (get-code from)))
	   (document (aref info-vector 2))
	   (id "(generated automatically by `w3m')")
	   i)
      (aset info-vector 2 (if (and (stringp document)
				   (> (length document) 0))
			      (concat document "\n  " id)
			    id))
      (set to to)
      (put to 'coding-system info-vector)
      (put to 'post-read-conversion (get from 'post-read-conversion))
      (put to 'pre-write-conversion (get from 'pre-write-conversion))
      (when (vectorp (get from 'eol-type))
	(setq i 0)
	(dolist (variant (append
			  (put to 'eol-type
			       (vector (intern (format "%s-unix" to))
				       (intern (format "%s-dos" to))
				       (intern (format "%s-mac" to))))
			  nil))
	  (set variant variant)
	  (put variant 'coding-system to)
	  (put variant 'eol-type (setq i (1+ i))))))))


(unless (fboundp 'read-passwd)
  ;; This code is imported from subr.el of Emacs-20.7 and slightly modified.
  (defun read-passwd (prompt &optional confirm default)
    "Read a password, prompting with PROMPT.  Echo `.' for each character typed.
End with RET, LFD, or ESC.  DEL or C-h rubs out.  C-u kills line.
Optional argument CONFIRM, if non-nil, then read it twice to make sure.
Optional DEFAULT is a default password to use instead of empty input."
    (if confirm
	(let (success)
	  (while (not success)
	    (let ((first (read-passwd prompt nil default))
		  (second (read-passwd "Confirm password: " nil default)))
	      (if (equal first second)
		  (setq success first)
		(message "Password not repeated accurately; please start over")
		(sit-for 1))))
	  success)
      (let ((pass nil)
	    (c 0)
	    (echo-keystrokes 0)
	    (cursor-in-echo-area t)
	    (inhibit-input-event-recording t))
	(while (progn (message "%s%s"
			       prompt
			       (make-string (length pass) ?.))
		      (setq c (read-char-exclusive))
		      (and (/= c ?\r) (/= c ?\n) (/= c ?\e)))
	  (if (= c ?\C-u)
	      (setq pass "")
	    (if (and (/= c ?\b) (/= c ?\177))
		(setq pass (concat pass (char-to-string c)))
	      (if (> (length pass) 0)
		  (setq pass (substring pass 0 -1))))))
	(message nil)
	(or pass default "")))))


(defalias 'coding-system-category 'get-code-mnemonic)

(defun w3m-read-coding-system (prompt &optional default-coding-system)
  "Read a coding system from the minibuffer for w3m under Mule 2.3
, prompting with string PROMPT. If the user enters null input
, return second argument DEFAULT-CODING-SYSTEM."
  (let (comp ret)
    (mapatoms
     (function
      (lambda (x)
	(if (and (or (vectorp (get x 'coding-system))
		     (vectorp (get x 'eol-type)))
		 (string-match "^[^*]" (symbol-name x)))
	    (setq comp (cons (cons (symbol-name x) (symbol-name x)) comp))))))
    (setq ret (completing-read prompt comp))
    (if (string= ret "")
	default-coding-system
      (intern ret))))

;;; Widget:
(defun w3m-om-define-missing-widgets ()
  "Define some missing widget(s)."
  (unless (get 'other 'widget-type)
    ;; The following definition is imported from wid-edit.el of Emacs 20.7.
    (define-widget 'other 'sexp
      "Matches any value, but doesn't let the user edit the value.
This is useful as last item in a `choice' widget.
You should use this widget type with a default value,
as in (other DEFAULT) or (other :tag \"NAME\" DEFAULT).
If the user selects this alternative, that specifies DEFAULT
as the value."
      :tag "Other"
      :format "%t%n"
      :value 'other)))

(eval-after-load "wid-edit" '(w3m-om-define-missing-widgets))

;;; w3m-om.el ends here

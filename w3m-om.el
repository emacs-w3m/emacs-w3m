;;; w3m-om.el --- Mule 2 specific functions for w3m

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: Katsumi Yamaoka    <yamaoka@jpl.org>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>
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

;; This file contains the stuffs to use emacs-w3m on Mule-2.  For more
;; detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/

;;; Code:

(eval-when-compile (require 'cl))

(require 'poe)
(require 'poem)
(require 'pcustom)
(require 'pccl)

(eval-when-compile
  (unless (fboundp 'custom-declare-variable)
    (defconst w3m-icon-directory nil
      "This definition is used to avoid byte-compile warnings.")))

;; Variables which will be defined in the other module.
(eval-when-compile
  (defvar w3m-mode-map)
  (defvar w3m-menubar))

;; Dummy functions.
(defalias 'w3m-create-image 'ignore)
(defalias 'w3m-insert-image 'ignore)
(defalias 'w3m-image-type-available-p 'ignore)
(defalias 'w3m-setup-toolbar 'ignore)
(defalias 'w3m-update-toolbar 'ignore)
(defalias 'w3m-display-graphic-p 'ignore)
(defalias 'w3m-display-inline-image-p 'ignore)

;; Generate some coding-systems which have a modern name.
;; No need to contain the eol-type variants in the following alist
;; because they will also be generated for each coding-system.
(dolist (elem '((*autoconv*	  undecided)
		(*ctext*	  ctext)
		(*euc-china*	  cn-gb-2312)
		(*euc-japan*	  euc-japan)
		(*iso-2022-jp*	  iso-2022-jp)
		(*iso-2022-jp*	  iso-2022-7bit)
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


;; Functions to handle coding-system.
(defalias 'coding-system-category 'get-code-mnemonic)

(unless (fboundp 'coding-system-list)
  (defun coding-system-list ()
    "Return a list of all existing non-subsidiary coding systems."
    (let ((codings nil))
      (mapatoms
       (function
	(lambda (arg)
	  (if (eq arg '*noconv*)
	      nil
	    (if (and (or (vectorp (get arg 'coding-system))
			 (vectorp (get arg 'eol-type)))
		     (null (get arg 'pre-write-conversion))
		     (null (get arg 'post-read-conversion)))
		(setq codings (cons arg codings)))))))
      codings)))

(defsubst w3m-find-coding-system (obj)
  "Return OBJ if it is a coding-system."
  (if (coding-system-p obj) obj))

(defalias 'w3m-make-ccl-coding-system 'make-ccl-coding-system)


;;; Generic functions.
(defun w3m-expand-path-name (name &optional base)
  "Convert filename NAME to absolute, and canonicalize it.
This function is implemented to absorb the difference between
`expand-file-name' of Mule2 and the same one of other Emacsen.
They handle non-initial \"~\" in the different way."
  (let (start buf)
    (setq name
	  (apply
	   (function expand-file-name)
	   (mapcar
	    (lambda (str)
	      (when (stringp str)
		(setq start 1
		      buf (list (if (string-match "\\`_" str)
				    "_u"
				  (char-to-string
				   (string-to-char str)))))
		(while (string-match "[~_]" str start)
		  (setq buf
			(cons (if (string= "_" (match-string 0 str)) "_u" "_t")
			      (cons (substring str start (match-beginning 0))
				    buf))
			start (match-end 0)))
		(apply
		 (function concat)
		 (nreverse (cons (substring str start) buf)))))
	    (list name (or base default-directory)))))
    (setq start 0
	  buf nil)
    (while (string-match "_[ut]" name start)
      (setq buf
	    (cons (if (string= "_u" (match-string 0 name)) "_" "~")
		  (cons (substring name start (match-beginning 0))
			buf))
	    start (match-end 0)))
    (apply
     (function concat)
     (nreverse (cons (substring name start) buf)))))


(eval-and-compile
  (unless (fboundp 'read-passwd)
    ;; This code is imported from subr.el of Emacs-20.7 and slightly modified.
    (defun read-passwd (prompt &optional confirm default)
      "Read a password, prompting with PROMPT.  Echo `.' for each character
typed.  End with RET, LFD, or ESC.  DEL or C-h rubs out.  C-u kills
line.  Optional argument CONFIRM, if non-nil, then read it twice to
make sure. Optional DEFAULT is a default password to use instead of
empty input."
      (if confirm
	  (let (success)
	    (while (not success)
	      (let ((first (read-passwd prompt nil default))
		    (second (read-passwd "Confirm password: " nil default)))
		(if (equal first second)
		    (setq success first)
		  (message
		   "Password not repeated accurately; please start over")
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
	  (or pass default ""))))))


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

(provide 'w3m-om)

;;; w3m-om.el ends here

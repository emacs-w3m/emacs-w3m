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
  (defvar w3m-menubar)
  (defvar w3m-default-coding-system))

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
(defvar w3m-om-coding-system-alist
  '((big5		. *big5*)
    (binary		. *noconv*)
    (cn-gb-2312		. *euc-china*)
    (ctext		. *ctext*)
    (emacs-mule		. *internal*)
    (euc-japan		. *euc-japan*)
    (iso-2022-7bit	. *iso-2022-jp*)
    (iso-2022-7bit-ss2	. *iso-2022-ss2-7*)
    (iso-2022-jp	. *iso-2022-jp*)
    (iso-8859-1		. *iso-8859-1*)
    (iso-8859-2		. *iso-8859-2*)
    (iso-8859-3		. *iso-8859-3*)
    (iso-8859-4		. *iso-8859-4*)
    (iso-8859-5		. *iso-8859-5*)
    (iso-8859-7		. *iso-8859-7*)
    (iso-8859-8		. *iso-8859-8*)
    (iso-8859-9		. *iso-8859-9*)
    (shift_jis		. *sjis*)
    (tis-620		. *tis620*)
    (undecided		. *autoconv*))
  "*Alist of a modern coding-system and a traditional coding-system.")

(defconst w3m-om-coding-categories
  (sort (list '*coding-category-internal*
	      '*coding-category-sjis*
	      '*coding-category-iso-7*
	      '*coding-category-iso-8-1*
	      '*coding-category-iso-8-2*
	      '*coding-category-iso-else*
	      '*coding-category-big5*
	      '*coding-category-bin*)
	'coding-priority<)
  "List of coding categories.")

(defvar w3m-om-coding-category-alist
  (let ((defs (cons '(iso-2022-7bit . *junet*) w3m-om-coding-system-alist))
	pair rest)
    (dolist (category w3m-om-coding-categories)
      (when (setq pair (rassq (symbol-value category) defs))
	(push (cons (car pair) category) rest)))
    (nreverse rest))
  "*Alist of a modern coding-system and a traditional coding-category.")

(dolist (elem w3m-om-coding-system-alist)
  (unless (coding-system-p (car elem))
    (let* ((from (cdr elem))
	   (to (car elem))
	   (info-vector (copy-sequence (get-code from)))
	   (document (aref info-vector 2))
	   (id "(generated automatically by Emacs-W3M)")
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

(defun w3m-om-modernize-coding-system (coding-system)
  "Return a modern coding-system name of CODING-SYSTEM if it is available."
  (let ((base (and (coding-system-p coding-system)
		   (get-base-code coding-system)))
	name name-eol eol)
    (if base
	(if (setq name (car (rassq base w3m-om-coding-system-alist)))
	    (if (and (setq eol (get coding-system 'eol-type))
		     (integerp eol)
		     (coding-system-p
		      (setq name-eol
			    (intern (format "%s-%s"
					    name
					    (plist-get '(1 unix 2 dos 3 mac)
						       eol))))))
		name-eol
	      name)
	  coding-system)
      'binary)))

(defun w3m-detect-coding-region (start end &optional highest)
  "Detect coding system of the text in the region between START and END
Return a list of possible coding systems ordered by priority.

If optional argument HIGHEST is non-nil, return the coding system of
highest priority."
  (let ((category (assq w3m-default-coding-system
			w3m-om-coding-category-alist))
	opriority x)
    (when category
      (setq opriority (sort (copy-sequence w3m-om-coding-categories)
			    'coding-priority<))
      (set-coding-priority (list (cdr category))))
    (prog2
	(setq x (code-detect-region start end))
	(if highest
	    (progn
	      (when (consp x)
		(setq x (car x)))
	      (w3m-om-modernize-coding-system x))
	  (setq x (mapcar 'w3m-om-modernize-coding-system (if (consp x)
							      x
							    (list x))))
	  (if (= 1 (length x))
	      (car x)
	    x))
      (when opriority
	(set-coding-priority opriority)))))

;;; Generic functions.
(defun w3m-expand-path-name (name)
  "Convert path string NAME to the canonicalized one."
  (with-temp-buffer
    (insert name)
    (let (p q path)
      (goto-char (point-min))
      (save-match-data
	(while (search-forward "/" nil t)
	  (setq p (match-beginning 0)
		q (match-end 0))
	  (if (search-forward "/" nil t)
	      (goto-char (match-beginning 0))
	    (goto-char (point-max)))
	  (setq path (buffer-substring q (point)))
	  (cond
	   ((string= path ".")
	    (delete-region q (if (eobp) (point) (match-end 0))))
	   ((string= path "..")
	    (setq q (point))
	    (when (search-backward "/" nil t)
	      (search-backward "/" nil t)
	      (delete-region (match-end 0) q)))
	   ((eq (length path) 0)
	    (unless (eobp) (delete-region p (point))))))
	(setq path (buffer-string)))
      (if (eq (length path) 0) "/" path))))


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

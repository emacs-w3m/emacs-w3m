;;; w3m-time.el --- Parsing time strings

;; Copyright (C) 1996, 2000 by Free Software Foundation, Inc.

;; Author: Erik Naggum <erik@naggum.no>
;; Keywords: util

;; This file is imported from parse-time.el of T-gnus 6.14.6.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; With the introduction of the `encode-time', `decode-time', and
;; `format-time-string' functions, dealing with time became simpler in
;; Emacs.  However, parsing time strings is still largely a matter of
;; heuristics and no common interface has been designed.

;; `w3m-time-parse-string' parses a time in a string and returns a
;; list of 9 values, just like `decode-time', where unspecified
;; elements in the string are returned as nil.  `encode-time' may be
;; applied on these valuse to obtain an internal time value.

;;; Code:

(eval-when-compile (require 'cl))	;and ah ain't kiddin' 'bout it

(defvar w3m-time-syntax (make-vector 256 nil))
(defvar w3m-time-digits (make-vector 256 nil))

;; Byte-compiler warnings
(eval-when-compile
  (defvar elt)
  (defvar val))

(unless (aref w3m-time-digits ?0)
  (loop for i from ?0 to ?9
    do (aset w3m-time-digits i (- i ?0))))

(unless (aref w3m-time-syntax ?0)
  (loop for i from ?0 to ?9
    do (aset w3m-time-syntax i ?0))
  (loop for i from ?A to ?Z
    do (aset w3m-time-syntax i ?A))
  (loop for i from ?a to ?z
    do (aset w3m-time-syntax i ?a))
  (aset w3m-time-syntax ?+ 1)
  (aset w3m-time-syntax ?- -1)
  (aset w3m-time-syntax ?: ?d))

(defsubst w3m-time-digit-char-p (char)
  (aref w3m-time-digits char))

(defsubst w3m-time-string-chars (char)
  (aref w3m-time-syntax char))

(put 'parse-error 'error-conditions '(parse-error error))
(put 'parse-error 'error-message "Parsing error")

(defsubst w3m-time-parse-integer (string &optional start end)
  "[CL] Parse and return the integer in STRING, or nil if none."
  (let ((integer 0)
	(digit 0)
	(index (or start 0))
	(end (or end (length string))))
    (when (< index end)
      (let ((sign (aref string index)))
	(if (or (eq sign ?+) (eq sign ?-))
	    (setq sign (w3m-time-string-chars sign)
		  index (1+ index))
	  (setq sign 1))
	(while (and (< index end)
		    (setq digit (w3m-time-digit-char-p (aref string index))))
	  (setq integer (+ (* integer 10) digit)
		index (1+ index)))
	(if (/= index end)
	    (signal 'parse-error (list "not an integer"
				       (substring string (or start 0) end)))
	  (* sign integer))))))

(defun w3m-time-tokenize (string)
  "Tokenize STRING into substrings."
  (let ((start nil)
	(end (length string))
	(all-digits nil)
	(list ())
	(index 0)
	(c nil))
    (while (< index end)
      (while (and (< index end)		;skip invalid characters
		  (not (setq c (w3m-time-string-chars (aref string index)))))
	(incf index))
      (setq start index all-digits (eq c ?0))
      (while (and (< (incf index) end)	;scan valid characters
		  (setq c (w3m-time-string-chars (aref string index))))
	(setq all-digits (and all-digits (eq c ?0))))
      (if (<= index end)
	  (push (if all-digits (w3m-time-parse-integer string start index)
		  (substring string start index))
		list)))
    (nreverse list)))

(defvar w3m-time-months '(("jan" . 1) ("feb" . 2) ("mar" . 3)
			    ("apr" . 4) ("may" . 5) ("jun" . 6)
			    ("jul" . 7) ("aug" . 8) ("sep" . 9)
			    ("oct" . 10) ("nov" . 11) ("dec" . 12)))
(defvar w3m-time-weekdays '(("sun" . 0) ("mon" . 1) ("tue" . 2)
			      ("wed" . 3) ("thu" . 4) ("fri" . 5) ("sat" . 6)))
(defvar w3m-time-zoneinfo
  (list '("z" 0) '("ut" 0) '("gmt" 0)
	(list "pst" (* -8 3600)) (list "pdt" (* -7 3600) t)
	(list "mst" (* -7 3600)) (list "mdt" (* -6 3600) t)
	(list "cst" (* -6 3600)) (list "cdt" (* -5 3600) t)
	(list "est" (* -5 3600)) (list "edt" (* -4 3600) t))
  "(zoneinfo seconds-off daylight-savings-time-p)")

(defvar w3m-time-rules
  (list '((6) w3m-time-weekdays)
	'((3) (1 31))
	'((4) w3m-time-months)
	'((5) (100 4038))
	(list '(2 1 0)
	      (function (lambda () (and (stringp elt)
					(= (length elt) 8)
					(= (aref elt 2) ?:)
					(= (aref elt 5) ?:))))
	      [0 2] [3 5] [6 8])
	(list '(8 7) 'w3m-time-zoneinfo
	      (function (lambda () (car val)))
	      (function (lambda () (cadr val))))
	(list '(8)
	      (function (lambda ()
			  (and (stringp elt)
			       (= 5 (length elt))
			       (or (= (aref elt 0) ?+) (= (aref elt 0) ?-)))))
	      (function (lambda ()
			  (* 60 (+ (w3m-time-parse-integer elt 3 5)
				   (* 60 (w3m-time-parse-integer elt 1 3)))
			     (if (= (aref elt 0) ?-) -1 1)))))
	(list '(5 4 3)
	      (function (lambda () (and (stringp elt)
					(= (length elt) 10)
					(= (aref elt 4) ?-)
					(= (aref elt 7) ?-))))
	      [0 4] [5 7] [8 10])
	(list '(2 1 0)
	      (function (lambda ()
			  (and (stringp elt) (= (length elt) 5)
			       (= (aref elt 2) ?:))))
	      [0 2] [3 5] (function (lambda () 0)))
	(list '(2 1 0)
	      (function (lambda () (and (stringp elt)
					(= (length elt) 4)
					(= (aref elt 1) ?:))))
	      [0 1] [2 4] (function (lambda () 0)))
	(list '(2 1 0)
	      (function (lambda () (and (stringp elt)
					(= (length elt) 7)
					(= (aref elt 1) ?:))))
	      [0 1] [2 4] [5 7])
	(list '(5) '(50 110) (function (lambda () (+ 1900 elt))))
	(list '(5) '(0 49) (function (lambda () (+ 2000 elt)))))
  "(slots predicate extractor...)")

(defun w3m-time-parse-string (string)
  "Parse the time-string STRING into (SEC MIN HOUR DAY MON YEAR DOW DST TZ).
The values are identical to those of `decode-time', but any values that are
unknown are returned as nil."
  (let ((time (list nil nil nil nil nil nil nil nil nil))
	(temp (w3m-time-tokenize (downcase string))))
    (while temp
      (let ((elt (pop temp))
	    (rules w3m-time-rules)
	    (exit nil))
	(while (and (not (null rules)) (not exit))
	  (let* ((rule (pop rules))
		 (slots (pop rule))
		 (predicate (pop rule))
		 (val))
	    (when (and (not (nth (car slots) time)) ;not already set
		       (setq val (cond ((and (consp predicate)
					     (not (eq (car predicate)
						      'lambda)))
					(and (numberp elt)
					     (<= (car predicate) elt)
					     (<= elt (cadr predicate))
					     elt))
				       ((symbolp predicate)
					(cdr (assoc elt
						    (symbol-value predicate))))
				       ((funcall predicate)))))
	      (setq exit t)
	      (while slots
		(let ((new-val (and rule
				    (let ((this (pop rule)))
				      (if (vectorp this)
					  (w3m-time-parse-integer
					   elt (aref this 0) (aref this 1))
					(funcall this))))))
		  (rplaca (nthcdr (pop slots) time) (or new-val val)))))))))
    time))

(defsubst w3m-time-newer-p (a b)
  "Return t, if A is newer than B.  Otherwise return nil.
A and B are lists which represent time in Emacs-style.  If value is
nil, it is regarded as the oldest time."
  (and a
       (or (not b)
	   (or (> (car a) (car b))
	       (and (= (car a) (car b))
		    (> (nth 1 a) (nth 1 b)))))))


(provide 'w3m-time)
;;; w3m-time.el ends here

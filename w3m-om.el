;;; -*- mode: Emacs-Lisp; coding: euc-japan -*-

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
(require 'poe)
(require 'poem)
(require 'pcustom)


(dolist (elem '((*autoconv*		. undecided)
		(*ctext*		. ctext)
		(*euc-china*		. cn-gb-2312)
		(*euc-japan*		. euc-japan)
		(*iso-2022-jp*		. iso-2022-jp)
		(*iso-2022-ss2-7*	. iso-2022-7bit-ss2)
		(*sjis*			. shift_jis)
		(*tis620*		. tis-620)))
  (unless (coding-system-p (cdr elem))
    (condition-case nil
	(let* ((info-vector (copy-sequence (get-code (car elem))))
	       (document (aref info-vector 2))
	       (id "(generated automatically by `w3m')"))
	  (copy-coding-system (car elem) (cdr elem))
	  (aset info-vector 2
		(if (and (stringp document)
			 (> (length document) 0))
		    (if (string-match "\\.[\t\n ]*$" document)
			(concat (substring document
					   0 (match-beginning 0))
				" " id ".")
		      (concat document " " id "."))
		  id))
	  (put (cdr elem) 'coding-system info-vector))
      (error))))


(provide 'w3m-om)
;;; w3m-om.el ends here.

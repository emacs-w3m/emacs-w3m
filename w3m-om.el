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

;; Generate some coding-systems which have a modern name.
;; No need to contain the eol-type variants in the folloing alist
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

(provide 'w3m-om)

;;; w3m-om.el ends here

;;; w3m-ucs.el --- CCL codes to handle UCS characters.

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
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

;; This file contains CCL codes to handle UCS characters in emacs-w3m.
;; For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/

;; This module requires `Mule-UCS' package.  It can be downloaded from:
;;
;;    ftp://ftp.m17n.org/pub/mule/Mule-UCS/


;;; How to install:

;; Please put this file to appropriate directory, and if you want
;; byte-compile it.  And add following lisp expressions to your
;; ~/.emacs.
;;
;;     (setq w3m-use-mule-ucs t)


;;; Code:
(require 'un-define)
(require 'w3m-macro)


(defalias 'w3m-ucs-to-char 'ucs-to-char)


(define-ccl-program w3m-euc-japan-encoder
  `(4
    (loop
     (read-multibyte-character r1 r0)
     (if (r1 == 0)
	 ;; (1) ASCII characters
	 (write-repeat r0)
       (if (r1 == ,(w3m-static-if (boundp 'MULE)
		       lc-jp
		     (charset-id 'japanese-jisx0208)))
	   ;; (2) Characters of Japanese JISX0208.
	   ((r1 = ((r0 & 127) | 128))
	    (r0 = ((r0 >> 7) | 128))
	    (write r0)
	    (write-repeat r1))
	 (if (r1 == ,(w3m-static-if (boundp 'MULE)
			 lc-kana
		       (charset-id 'katakana-jisx0201)))
	     ;; (3) Katakana Part of Japanese JISX0201.1976
	     ((write ?\x8e)
	      (write-repeat r0))
	   ((if (r0 > 255)
		((r4 = (r0 & 127))
		 (r0 = (((r0 >> 7) * 96) + r4))
		 (r0 |= (r1 << 16)))
	      ((r0 |= (r1 << 16))))
	    ;; (4) Other characters are mapped into Unicode codepoint.
	    (call emacs-char-to-ucs-codepoint-conversion)
	    (r1 = 0)
	    (r2 = 0)
	    (loop
	     (r1 = (r1 << 4))
	     (r1 |= (r0 & 15))
	     (r0 = (r0 >> 4))
	     (if (r0 == 0)
		 (break)
	       ((r2 += 1)
		(repeat))))
	    (write "&#x")
	    (loop
	     (branch (r1 & 15)
		     ,@(mapcar
			(lambda (i)
			  (list 'write (string-to-char (format "%x" i))))
			'(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))
	     (r1 = (r1 >> 4))
	     (if (r2 == 0)
		 ((write ?\;)
		  (break))
	       ((r2 -= 1)
		(repeat))))
	    (repeat))))))))


(provide 'w3m-ucs)
;;; w3m-ucs.el ends here.

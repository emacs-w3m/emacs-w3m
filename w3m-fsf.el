;;; w3m-fsf.el --- Common functions through FSF Emacsen.

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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This module provides common functions through FSF Emacsen.  Visit
;; <URL:http://emacs-w3m.namazu.org/> for more details of emacs-w3m.

;;; Code:

(eval-when-compile
  (defvar w3m-default-coding-system))

(defsubst w3m-find-coding-system (obj)
  "Return OBJ if it is a coding-system."
  (if (coding-system-p obj) obj))

(defun w3m-detect-coding-region (start end &optional highest)
  "Detect coding system of the text in the region between START and END
Return a list of possible coding systems ordered by priority.

If optional argument HIGHEST is non-nil, return the coding system of
highest priority."
  (let (category)
    (if (and w3m-default-coding-system
	     (setq category
		   (coding-system-category w3m-default-coding-system)))
	(let ((orig coding-category-list))
	  (unwind-protect
	      (progn
		(set-coding-priority
		 (cons category
		       (delq category
			     (copy-sequence coding-category-list))))
		(detect-coding-region start end highest))
	    (set-coding-priority orig)))
      (detect-coding-region start end highest))))

(defun w3m-make-ccl-coding-system
  (coding-system mnemonic docstring decoder encoder)
  "Define a new CODING-SYSTEM by CCL programs DECODER and ENCODER.
CODING-SYSTEM, DECODER and ENCODER must be symbol."
  (make-coding-system coding-system 4 mnemonic docstring
		      (cons decoder encoder)))

(provide 'w3m-fsf)
;;; w3m-fsf.el ends here.

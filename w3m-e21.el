;;; -*- mode: Emacs-Lisp; coding: euc-japan -*-

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>

;; Authors: Yuuichi Teranishi  <teranisi@gohome.org>,
;;          TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;; Keywords: w3m, WWW, hypermedia

;; w3m-e21.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2 of the License,
;; or (at your option) any later version.

;; w3m-e21.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with w3m.el; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


;;; Commentary:

;; This file contains the stuffs to use w3m.el on Emacs-21.  For more
;; detail about w3m.el, see:
;;
;;    http://namazu.org/~tsuchiya/emacs-w3m/


;;; Code:

(defun w3m-create-image (url &optional no-cache)
  "Retrieve data from URL and create an image object.
If optional argument NO-CACHE is non-nil, cache is not used."
  (condition-case err
      (let ((type (w3m-retrieve url 'raw nil no-cache)))
	(when (w3m-image-type-available-p (setq type (w3m-image-type type)))
	  (w3m-with-work-buffer
	    (create-image (buffer-string) 
			  type
			  t
			  :ascent 'center))))
    (error nil)))

(defun w3m-insert-image (beg end image)
  "Display image on the current buffer.
Buffer string between BEG and END are replaced with IMAGE."
  (add-text-properties beg end
		       (list 'display image
			     'intangible image
			     'invisible nil)))

(defun w3m-remove-image (beg end)
  "Remove an image which is inserted between BEG and END."
  (remove-text-properties beg end '(display intangible)))

(defun w3m-image-type-available-p (image-type)
  "Return non-nil if an image with IMAGE-TYPE can be displayed inline."
  (and (display-graphic-p)
       (image-type-available-p image-type)))


(provide 'w3m-e21)
;;; w3m-e21.el ends here.

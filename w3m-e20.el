;;; w3m-e20.el --- Emacs 20 specific functions for w3m

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

;; This module provides Emacs 20 specific functions.  Visit
;; <URL:http://emacs-w3m.namazu.org/> for more details of emacs-w3m.

;;; Code:

(require 'w3m-fsf)
(require 'w3m-util)
(require 'w3m-proc)
(require 'w3m-image)

(condition-case nil
    (require 'bitmap)
  (error nil))

;; Functions and variables which should be defined in the other module
;; at run-time.
(eval-when-compile
  (defvar w3m-display-inline-images)
  (defvar w3m-work-buffer-name)
  (autoload 'w3m-retrieve "w3m"))

;;; Dummy functions.
(defalias 'w3m-setup-toolbar 'ignore)
(defalias 'w3m-update-toolbar 'ignore)

(w3m-static-unless (featurep 'bitmap)
  (defalias 'bitmap-decode-xbm 'ignore)
  (defalias 'bitmap-read-xbm-buffer 'ignore)
  (defalias 'bitmap-compose 'ignore))

;;; Bitmap image functions.
(defvar w3m-bitmap-image-cache-alist nil)
(defvar w3m-bitmap-image-use-cache t
  "*If non-nil, bitmap-image is cached to this alist")

(defun w3m-bitmap-image-cleanup ()
  "Clean up bitmap-image overlays."
  (dolist (ovr (overlays-in (point-min) (point-max)))
    (delete-overlay ovr)))

(defun w3m-bitmap-image-buffer (buffer)
  "Create bitmap-image from BUFFER."
  (let* ((cmp (bitmap-decode-xbm (bitmap-read-xbm-buffer buffer)))
	 (len (length cmp))
	 (i 0)
	 list)
    (while (< i len)
      (push (bitmap-compose (aref cmp i)) list)
      (setq i (1+ i)))
    (nreverse list)))

(defun w3m-bitmap-image-get-overlay (point)
  "Get bitmap-image overlay at POINT."
  (save-excursion
    (goto-char (point))
    (let ((bol (progn (beginning-of-line) (point)))
	  (eol (progn (end-of-line) (point))))
      (catch  'loop
	(dolist (o (overlays-in bol eol))
	  (when (overlay-get o 'w3m-bitmap-image-line)
	    (throw 'loop o)))))))
  
(defun w3m-bitmap-image-insert-internal (pos image)
  (save-excursion
    (goto-char pos)
    (let (ovr ovrbeg)
      (setq ovrbeg (progn (beginning-of-line) (point)))
      (setq ovr (w3m-bitmap-image-get-overlay ovrbeg))
      (unless ovr
	(setq ovr (make-overlay ovrbeg ovrbeg))
	(overlay-put ovr 'w3m-bitmap-image-line t))
      (goto-char pos)
      (let ((col (current-column))
	    (indent-tabs-mode nil)
	    end-col)
	(insert (car image))
	(setq end-col (current-column))
	(setq image (cdr image))
	(forward-line)
	(while (or image (< (point) (overlay-end ovr)))
	  (when (>= (point) (overlay-end ovr))
	    (beginning-of-line)
	    (insert "\n")
	    (forward-line -1))
	  (move-to-column-force col)
	  (if image
	      (insert (car image))
	    (indent-to-column end-col))
	  (setq image (cdr image))
	  (forward-line))
	(move-overlay ovr (min ovrbeg (overlay-start ovr))
		      (1- (point)))))))

(defun w3m-bitmap-image-insert (pos image)
  "Insert IMAGE to POS."
  (w3m-bitmap-image-insert-internal pos image)
  (let ((ovr (w3m-bitmap-image-get-overlay pos)))
    (overlay-put ovr 'w3m-bitmap-image-count
		 (1+ (or (overlay-get ovr 'w3m-bitmap-image-count) 0)))))

(defun w3m-bitmap-image-insert-string (pos str)
  "Insert STR to POS same as bitmap-image."
  (w3m-bitmap-image-insert-internal pos (list str)))

(defun w3m-bitmap-image-delete-internal (pos width)
  (save-excursion
    (goto-char pos)
    (let ((ovr (w3m-bitmap-image-get-overlay (point))))
      (if ovr
	  (let ((col (current-column)))
	    (while (< (point) (overlay-end ovr))
	      (move-to-column-force col)
	      (delete-region (point)
			     (min (+ (point) width)
				  (save-excursion (end-of-line) (point))))
	      (forward-line)))
	(delete-region (point)
		       (min (+ (point) width)
			    (save-excursion (end-of-line) (point))))))))

(defun w3m-bitmap-image-delete (pos width)
  "Delete bitmap-image with a WIDTH on POS."
  (let ((ovr (w3m-bitmap-image-get-overlay pos))
	cnt)
    (when ovr
      (setq cnt (1- (overlay-get ovr 'w3m-bitmap-image-count)))
      (overlay-put ovr 'w3m-bitmap-image-count cnt)
      (w3m-bitmap-image-delete-internal pos width)
      (when (zerop cnt)
	(save-excursion
	  (goto-char (min (point) (overlay-start ovr)))
	  (forward-line)
	  (when (< (point) (overlay-end ovr))
	    (delete-region (point) (1+ (overlay-end ovr))))
	  (delete-overlay ovr))))))

(defun w3m-bitmap-image-delete-string (pos width)
  "Delete string with a WIDTH on POS same as bitmap-image."
  (w3m-bitmap-image-delete-internal pos width))

(defun w3m-bitmap-image-add-text-properties (pos width &rest properties)
  "add PROPERTIES to bitmap-image with a WIDTH on POS."
  (save-excursion
    (goto-char pos)
    (let ((ovr (w3m-bitmap-image-get-overlay (point))))
      (if ovr
	  (let ((col (current-column)))
	    (while (< (point) (overlay-end ovr))
	      (move-to-column-force col)
	      (w3m-add-text-properties (point)
				       (min (+ (point) width)
					    (save-excursion
					      (end-of-line) (point)))
				       properties)
	      (forward-line)))
	(w3m-add-text-properties (point)
				 (min (+ (point) width)
				      (save-excursion
					(end-of-line) (point)))
				 properties)))))
    

;;; Handle images:

;; Function which returns non-nil when the current display device can
;; show images inline.
(defun w3m-display-graphic-p () t)

(defun w3m-display-inline-images-p ()
  "Returns non-nil when images can be displayed under the present
circumstances."
  (and w3m-display-inline-images (w3m-display-graphic-p)))

(defun w3m-create-image (url &optional no-cache referer size handler)
  "Retrieve data from URL and create an image object.
If optional argument NO-CACHE is non-nil, cache is not used.
If second optional argument REFERER is non-nil, it is used as Referer: field."
  (if (not handler)
      (w3m-process-with-wait-handler
	(w3m-create-image url no-cache referer handler))
    (if (and w3m-bitmap-image-use-cache
	     (assoc url w3m-bitmap-image-cache-alist))	      

	(cdr (assoc url w3m-bitmap-image-cache-alist))
      (w3m-process-do-with-temp-buffer
	  (type (w3m-retrieve url nil no-cache nil referer))
	(ignore-errors
	  (when (and (stringp type) (string-match "^image/" type))
	    (setq type (replace-match "" nil nil type))
	    (when (w3m-imagick-convert-buffer type "xbm")
	      (let ((str (buffer-string)))
		(with-temp-buffer
		  (insert str)
		  (let ((image (w3m-bitmap-image-buffer (current-buffer))))
		    (push (cons url image) w3m-bitmap-image-cache-alist)
		    image))))))))))

(defun w3m-insert-image (beg end image)
  "Display image on the current buffer.
Buffer string between BEG and END are replaced with IMAGE."
  (when image
    (save-excursion
      (let ((properties (text-properties-at beg))
	    (name (buffer-substring beg end))
	    (len (length (car image))))
	(w3m-bitmap-image-delete-string beg (- end beg))
	(goto-char beg)
	(w3m-bitmap-image-insert (point) image)
	(apply 'w3m-bitmap-image-add-text-properties
	       (point) len
	       'w3m-image-status 'on
	       'face 'w3m-image-face
	       'w3m-bitmap-image t
	       'w3m-bitmap-image-width len
	       'w3m-image-name name
	       properties)))))

(defun w3m-remove-image (beg end)
  "Remove an image which is inserted between BEG and END."
  (let ((width (get-text-property beg 'w3m-bitmap-image-width))
	(name (get-text-property beg 'w3m-image-name)))
    (when width
      (w3m-bitmap-image-delete beg width)
      (w3m-bitmap-image-insert-string beg name)
      (+ beg (length name)))))

(defun w3m-image-type-available-p (image-type)
  (w3m-static-if (featurep 'bitmap) t nil))

;;; Coding system.

(defun w3m-make-ccl-coding-system
  (coding-system mnemonic docstring decoder encoder)
  "Define a new CODING-SYSTEM by CCL programs DECODER and ENCODER.
CODING-SYSTEM, DECODER and ENCODER must be symbol."
  (if (memq coding-system coding-system-list)
      coding-system
    (make-coding-system coding-system 4 mnemonic docstring
			(cons decoder encoder))))

(provide 'w3m-e20)

;;; w3m-e20.el ends here

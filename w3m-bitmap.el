;;; w3m-bitmap.el --- Display bitmap image functions for w3m

;; Copyright (C) 2001, 2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: Taiki SUGAWARA <taiki.s@cityfujisawa.ne.jp>
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

;; This module requires BITMAP-MULE package.  It can be downloaded from:
;;
;;    http://www.jpl.org/elips/bitmap/

;;; Code:

(require 'w3m-util)
(require 'w3m-proc)
(require 'w3m-image)
(require 'bitmap)

;; Functions and variables which should be defined in the other module
;; at run-time.
(eval-when-compile
  (defvar w3m-display-inline-images)
  (defvar w3m-work-buffer-name)
  (autoload 'w3m-retrieve "w3m"))

(defface w3m-bitmap-image-face
  '((((background light))
     (:foreground "Black"))
    (t
     (:background "White") (:foreground "Black")))
  "Face used to highlight bitmap images."
  :group 'w3m-face)

;;; Bitmap image functions.
(defvar w3m-bitmap-image-cache-alist nil)
(defvar w3m-bitmap-image-use-cache t
  "*If non-nil, bitmap-image is cached to this alist")

(eval-and-compile
  (if (or (>= emacs-major-version 21)
	  (and (= emacs-major-version 20)
	       (>= emacs-minor-version 3)))
      (progn
	(defalias 'w3m-bitmap-current-column 'current-column)
	(defalias 'w3m-bitmap-move-to-column-force 'move-to-column-force))

    (defun w3m-bitmap-current-column ()
      "Like `current-column', except that works with byte-indexed bitmap
chars as well."
      (let ((home (point))
	    (cols 0))
	(while (not (bolp))
	  (forward-char -1)
	  (setq cols (+ cols (char-width (following-char)))))
	(goto-char home)
	cols))

    (defun w3m-bitmap-move-to-column-force (column)
      "Like `move-to-column-force', except that works with byte-indexed
bitmap chars as well."
      (beginning-of-line)
      (let ((cols 0)
	    width)
	(if (wholenump column)
	    (progn
	      (while (and (not (eolp))
			  (< cols column))
		(setq width (char-width (following-char))
		      cols (+ cols width))
		(forward-char 1))
	      (cond ((> cols column)
		     (delete-backward-char 1)
		     (insert-char ?\  width)
		     (forward-char (- column cols)))
		    ((< cols column)
		     (insert-char ?\  (- column cols))))
	      column)
	  (signal 'wrong-type-argument (list 'wholenump column)))))))

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
    (catch  'loop
      (dolist (o (overlays-in (line-beginning-position)
			      (line-end-position)))
	(when (overlay-get o 'w3m-bitmap-image-line)
	  (throw 'loop o))))))

(defun w3m-bitmap-image-insert-internal (pos image &optional props)
  (save-excursion
    (goto-char pos)
    (let* ((ovrbeg (line-beginning-position))
	   (ovr (w3m-bitmap-image-get-overlay ovrbeg))
	   (col (current-column))
	   indent-tabs-mode end-col ovrend)
      (unless ovr
	(setq ovr (make-overlay ovrbeg ovrbeg))
	(overlay-put ovr 'w3m-bitmap-image-line t))
      (insert (car image))
      (w3m-add-text-properties pos (point) props)
      (setq end-col (current-column)
	    ovrend (overlay-end ovr)
	    image (cdr image))
      (forward-line)
      (while (or image (< (point) ovrend))
	(when (>= (point) ovrend)
	  (beginning-of-line)
	  (insert "\n")
	  (forward-line -1))
	(move-to-column-force col)
	(if image
	    (progn
	      (setq pos (point))
	      (insert (car image))
	      (w3m-add-text-properties pos (point) props))
	  (indent-to-column end-col))
	(setq image (cdr image))
	(forward-line))
      (move-overlay ovr (min ovrbeg (overlay-start ovr))
		    (1- (point))))))

(defun w3m-bitmap-image-insert (pos image props)
  "Insert IMAGE to POS."
  (w3m-bitmap-image-insert-internal pos image props)
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
			     (min (+ (point) width) (line-end-position)))
	      (forward-line)))
	(delete-region (point)
		       (min (+ (point) width)
			    (line-end-position)))))))

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

;;; Handle images:

;; Function which returns non-nil when the current display device can
;; show images inline.
(defun w3m-display-graphic-p ()
  window-system)

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
	(w3m-bitmap-image-insert (point) image
				 (append (list 'w3m-image-status 'on
					       'face 'w3m-bitmap-image-face
					       'w3m-bitmap-image t
					       'w3m-bitmap-image-width len
					       'w3m-image-name name)
					 properties))))))

(defun w3m-remove-image (beg end)
  "Remove an image which is inserted between BEG and END."
  (let ((width (get-text-property beg 'w3m-bitmap-image-width))
	(name (get-text-property beg 'w3m-image-name)))
    (when width
      (w3m-bitmap-image-delete beg width)
      (w3m-bitmap-image-insert-string beg name)
      (+ beg (length name)))))

(defun w3m-image-type-available-p (image-type)
  w3m-imagick-convert-program)

(provide 'w3m-bitmap)

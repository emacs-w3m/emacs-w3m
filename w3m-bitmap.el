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
  (defvar w3m-mode-map)
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

(eval-when-compile
  (defmacro w3m-bitmap-byte-indexed-characters-p ()
    (or (not (boundp 'emacs-major-version))
	(<= emacs-major-version 19)
	(and (= emacs-major-version 20)
	     (<= emacs-minor-version 2)))))

(eval-and-compile
  (unless (w3m-bitmap-byte-indexed-characters-p)
    (defalias 'w3m-bitmap-current-column 'current-column)
    (defalias 'w3m-bitmap-move-to-column-force 'move-to-column-force))

  (when (w3m-bitmap-byte-indexed-characters-p)
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

    (defun w3m-bitmap-move-to-column (column &optional force)
      "Like `move-to-column', except that works with byte-indexed bitmap
chars as well."
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
	      (if force
		  (progn
		    (cond ((> cols column)
			   (delete-backward-char 1)
			   (insert-char ?\  width)
			   (forward-char (- column cols)))
			  ((< cols column)
			   (insert-char ?\  (- column cols))))
		    column)
		cols))
	  (signal 'wrong-type-argument (list 'wholenump column)))))

    (defun w3m-bitmap-move-to-column-force (column)
      "Like `move-to-column-force', except that works with byte-indexed
bitmap chars as well."
      (inline (w3m-bitmap-move-to-column column t)))

    (defun w3m-bitmap-next-line (arg)
      "Simple emulation to `next-line' to work with byte-indexed bitmap chars."
      (interactive "p")
      (let ((column (w3m-bitmap-current-column)))
	(forward-line arg)
	(w3m-bitmap-move-to-column column)))

    (defun w3m-bitmap-previous-line (arg)
      "Simple emulation to `previous-line' to work with byte-indexed bitmap
chars."
      (interactive "p")
      (w3m-bitmap-next-line (- arg)))

    (defun w3m-bitmap-substitute-key-definitions ()
      "Substitute some key defs to work with byte-indexed bitmap chars.  Note
that it is intended to be used to `w3m-mode-hook' exclusively."
      (remove-hook 'w3m-mode-hook 'w3m-bitmap-substitute-key-definitions)
      (substitute-key-definition 'next-line 'w3m-bitmap-next-line
				 w3m-mode-map global-map)
      (substitute-key-definition 'previous-line 'w3m-bitmap-previous-line
				 w3m-mode-map global-map))

    (add-hook 'w3m-mode-hook 'w3m-bitmap-substitute-key-definitions)))

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

(defun w3m-bitmap-image-get-overlay (pos)
  "Return an overlay in and around POS, which has the `bitmap-image'
property ."
  (let ((home (point))
	ovrs ovr)
    (goto-char pos)
    (setq ovrs (overlays-in (line-beginning-position) (line-end-position)))
    (while (and (not ovr)
		ovrs)
      (if (overlay-get (car ovrs) 'w3m-bitmap-image-line)
	  (setq ovr (car ovrs))
	(setq ovrs (cdr ovrs))))
    (goto-char home)
    ovr))

(defun w3m-bitmap-image-insert (pos image &optional props ovr)
  "Insert IMAGE to POS.  IMAGE should be a list of bitmap image lines or
a non-list text.  PROPS specifies properties for bitmap images.  OVR
is an overlay which covers the area for bitmap images.  If OVR is nil,
a new overlay will be created and returned."
  (save-excursion
    (when (markerp pos)
      (setq pos (marker-position pos)))
    (goto-char pos)
    (let ((ovrbeg (line-beginning-position))
	  (col (w3m-bitmap-current-column))
	  indent-tabs-mode end-col face-ovrs)
      (unless ovr
	(setq ovr (make-overlay ovrbeg ovrbeg))
	(overlay-put ovr 'w3m-bitmap-image-line t)
	(overlay-put ovr 'w3m-bitmap-image-count 0))
      (if (consp image)
	  (progn
	    (insert-before-markers (car image))
	    (setq image (cdr image))
	    (overlay-put ovr 'w3m-bitmap-image-count
			 (1+ (overlay-get ovr 'w3m-bitmap-image-count))))
	(insert-before-markers image)
	(setq image nil))
      (when props
	(w3m-add-text-properties pos (point) props)
	(push (make-overlay pos (point)) face-ovrs))
      (setq end-col (w3m-bitmap-current-column))
      (forward-line)
      (while (or image (< (point) (overlay-end ovr)))
	(when (>= (point) (overlay-end ovr))
	  (beginning-of-line)
	  (insert-before-markers "\n")
	  (forward-line -1))
	(w3m-bitmap-move-to-column-force col)
	(if image
	    (progn
	      (setq pos (point))
	      (insert-before-markers (car image))
	      (when props
		(w3m-add-text-properties pos (point) props)
		(push (make-overlay pos (point)) face-ovrs)))
	  (indent-to-column end-col))
	(setq image (cdr image))
	(forward-line))
      (move-overlay ovr (min ovrbeg (overlay-start ovr))
		    (1- (point)))
      (overlay-put ovr 'evaporate t)
      ;; Since Emacs 20 has a bug(?) that an overlay hides the face
      ;; text properties, we should also use overlays to highlight
      ;; bitmap images.
      (while face-ovrs
	(overlay-put (car face-ovrs) 'face 'w3m-bitmap-image-face)
	(overlay-put (pop face-ovrs) 'evaporate t))
      ovr)))

(defun w3m-bitmap-image-delete-internal (pos ovr &optional width)
  (save-excursion
    (goto-char pos)
    (let (col eol)
      (if ovr
	  (progn
	    (overlay-put ovr 'evaporate nil)
	    (setq col (w3m-bitmap-current-column))
	    (while (< (point) (overlay-end ovr))
	      (setq eol (line-end-position))
	      (w3m-bitmap-move-to-column-force col)
	      (delete-region (point)
			     (if width
				 (min (+ (point) width) eol)
			       (or (text-property-not-all (point) eol
							  'w3m-bitmap-image t)
				   eol)))
	      (if (and (bolp)
		       (eolp)
		       (> (point) pos))
		  (delete-char 1)
		(forward-line))))
	(setq eol (line-end-position))
	(delete-region pos (if width
			       (min (+ pos width) eol)
			     (or (text-property-not-all pos eol
							'w3m-bitmap-image t)
				 eol)))))))

(defun w3m-bitmap-image-delete (pos ovr)
  "Delete bitmap-image on POS."
  (when ovr
    (let ((cnt (1- (overlay-get ovr 'w3m-bitmap-image-count))))
      (overlay-put ovr 'w3m-bitmap-image-count cnt)
      (w3m-bitmap-image-delete-internal pos ovr)
      (when (zerop cnt)
	(save-excursion
	  (goto-char (min (point) (overlay-start ovr)))
	  (forward-line)
	  (when (< (point) (overlay-end ovr))
	    (delete-region (point) (1+ (overlay-end ovr)))))))))

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
	     (assoc (if (and w3m-resize-images
			     (and (consp size)(car size)(cdr size)))
			(list url size)
		      url)
		    w3m-bitmap-image-cache-alist))
	(cdr (assoc (if (and w3m-resize-images
			     (consp size)(car size)(cdr size))
			(list url size)
		      url)
		    w3m-bitmap-image-cache-alist))
      (w3m-process-do-with-temp-buffer
	  (type (w3m-retrieve url nil no-cache nil referer))
	(ignore-errors
	  (when (and (stringp type) (string-match "^image/" type))
	    (setq type (replace-match "" nil nil type))
	    (lexical-let ((url url)
			  (size size)
			  (data (buffer-string))
			  set-size)
	      (if (and w3m-resize-images
		       (consp size)(car size)(cdr size))
		  (setq set-size t))
	      (w3m-process-do-with-temp-buffer
		  (success (progn
			     (unless (boundp 'MULE)
			       (set-buffer-multibyte nil))
			     (insert data)
			     (apply 'w3m-imagick-start-convert-buffer
				    handler type "xbm"
				    (if set-size
					(list "-geometry"
					      (concat (number-to-string
						       (car size))
						      "x"
						      (number-to-string
						       (cdr size)) "!"))))))
		(when success
		  (let ((image (w3m-bitmap-image-buffer (current-buffer))))
		    (push (cons (if set-size (list url size) url)
				image) w3m-bitmap-image-cache-alist)
		    image))))))))))

(defun w3m-insert-image (beg end image)
  "Display image on the current buffer.
Buffer string between BEG and END are replaced with IMAGE."
  (let ((properties (text-properties-at beg))
	(name (buffer-substring beg end))
	(ovr (w3m-bitmap-image-get-overlay beg)))
    (w3m-bitmap-image-delete-internal beg ovr (- end beg))
    (w3m-bitmap-image-insert beg image
			     (w3m-modify-plist properties
					       'w3m-image-status 'on
					       'w3m-bitmap-image t
					       'w3m-image-name name)
			     ovr)))

(defun w3m-remove-image (beg end)
  "Remove an image which is inserted between BEG and END.
\(Note: END will be ignored in this version of `w3m-remove-image'.)"
  (let ((name (get-text-property beg 'w3m-image-name))
	(ovr (w3m-bitmap-image-get-overlay beg)))
    (when name
      (w3m-bitmap-image-delete beg ovr)
      (w3m-bitmap-image-insert beg name nil ovr)
      (+ beg (length name)))))

(defun w3m-image-type-available-p (image-type)
  w3m-imagick-convert-program)

(provide 'w3m-bitmap)

;; w3m-bitmap.el ends here

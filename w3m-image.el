;;; w3m-image.el --- Image conversion routines.

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: Yuuichi Teranishi  <teranisi@gohome.org>
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

;; This file contains the stuffs to convert images for emacs-w3m.
;; For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/
;;
;; Routines in this file requires ImageMagick's convert.
;; For more detail about ImageMagick, see:
;;
;;    http://www.imagemagick.org/

;;; Code:

(require 'w3m-util)
(require 'w3m-proc)

(defcustom w3m-imagick-convert-program (w3m-which-command "convert")
  "*Program name of ImageMagick's `convert'."
  :group 'w3m
  :type 'string)

;;; Image handling functions.
(defcustom w3m-resize-images (and w3m-imagick-convert-program t)
  "*If non-nil, resize images to the specified width and height."
  :group 'w3m
  :type 'boolean)

;;; Synchronous image conversion.
(defun w3m-imagick-convert-buffer (from-type to-type &rest args)
  (when w3m-imagick-convert-program
    (let* ((coding-system-for-read 'binary)
	   (coding-system-for-write 'binary)
	   (default-process-coding-system (cons 'binary 'binary))
	   (return (apply 'call-process-region
			  (point-min) (point-max)
			  w3m-imagick-convert-program
			  t t nil (append args (list
						(concat
						 (if from-type
						     (concat from-type ":"))
						 "-")
						(concat
						 (if to-type
						     (concat to-type ":"))
						 "-"))))))
      (if (and (numberp return)
	       (zerop return))
	  t
	(message "Image conversion failed (code `%s')"
		 (if (stringp return)
		     (string-as-multibyte return)
		   return))
	nil))))

(defun w3m-imagick-convert-data (data from-type to-type &rest args)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert data)
    (and (apply 'w3m-imagick-convert-buffer from-type to-type args)
	 (buffer-string))))

;;; Asynchronous image conversion.
(defun w3m-imagick-start-convert-data (handler
				       data from-type to-type &rest args)
  (w3m-process-do-with-temp-buffer
      (success (progn
		 (set-buffer-multibyte nil)
		 (insert data)
		 (apply 'w3m-imagick-start-convert-buffer
			handler from-type to-type args)))
    (if success (buffer-string))))

(defun w3m-imagick-start-convert-buffer (handler from-type to-type &rest args)
  (lexical-let ((in-file (make-temp-name
			  (expand-file-name "w3mel" w3m-profile-directory)))
		(out-file (make-temp-name
			   (expand-file-name "w3mel" w3m-profile-directory)))
		(out-buffer (current-buffer)))
    (setq w3m-current-url "non-existent")
    (let ((file-coding-system 'binary)
	  (coding-system-for-write 'binary)
	  (buffer-file-coding-system 'binary)
	  jka-compr-compression-info-list
	  jam-zcat-filename-list
	  format-alist)
      (write-region (point-min) (point-max) in-file nil 'nomsg))
    (w3m-process-do
	(success (progn
		   (apply
		    'w3m-imagick-start handler
		    (append args
			    (list
			     (concat
			      (if from-type
				  (concat from-type ":"))
			      in-file)
			     (concat
			      (if to-type
				  (concat to-type ":"))
			      out-file))))))
      (with-current-buffer out-buffer
	(erase-buffer)
	(ignore-errors
	  (insert-file-contents-literally out-file)))
      (when (file-exists-p in-file)
	(delete-file in-file))
      (when (file-exists-p out-file)
	(delete-file out-file))
      success)))

(defun w3m-imagick-start (handler &rest arguments)
  "Run ImageMagick's convert as an asynchronous process."
  (let ((w3m-command w3m-imagick-convert-program))
    (if w3m-async-exec
	(w3m-process-do
	    (exit-status (w3m-process-push handler arguments))
	  (w3m-process-start-after exit-status))
      (w3m-process-start-after
       (apply 'call-process w3m-command nil t nil arguments)))))

(defun w3m-resize-image (data width height handler)
  "Resize image DATA to WIDTH and HEIGHT asynchronously.
HANDLER is called after conversion with resized data as an argument."
  (w3m-process-do
      (result (w3m-imagick-start-convert-data
	       handler
	       data nil nil "-geometry"
	       (concat (number-to-string width)
		       "x"
		       (number-to-string height)
		       "!")))
    result))

(provide 'w3m-image)

;;; w3m-image.el ends here

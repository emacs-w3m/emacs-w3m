;;; w3m-favicon.el --- utilities for handling favicon in emacs-w3m

;; Copyright (C) 2001, 2002, 2003 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: Yuuichi Teranishi  <teranisi@gohome.org>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>
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

;;; Code:

(eval-when-compile
  (require 'cl))

;;(require 'w3m-util)
;;(require 'w3m-proc)
(require 'w3m-image)

(eval-when-compile
  (autoload 'w3m-retrieve "w3m")
  (autoload 'w3m-save-list "w3m")
  (autoload 'w3m-load-list "w3m"))

(defcustom w3m-favicon-size nil
  "*Size of favicon. This value is used as geometry argument for `convert'."
  :group 'w3m
  :get (lambda (symbol)
	 (let ((value (default-value symbol)))
	   (if (and (stringp value)
		    (string-match "\
\\`[\t\n ]*\\([0-9]+\\)[\t\n ]*[Xx][\t\n ]*\\([0-9]+\\)[\t\n ]*\\'"
				  value))
	       (cons (string-to-number (match-string 1 value))
		     (string-to-number (match-string 2 value))))))
  :set (lambda (symbol value)
	 (custom-set-default symbol
			     (if (consp value)
				 (format "%dx%d" (car value) (cdr value)))))
  :type '(radio (const :tag "Not specified" nil)
		(cons :format "%v"
		      (integer :format "Width: %v " :size 0 :value 16)
		      (integer :format "Height: %v " :size 0 :value 16))))

(defconst w3m-favicon-name "favicon.ico"
  "The favicon name.")

(defvar w3m-current-favicon-data nil
  "A cons cell of (IMAGE-DATA . IMAGE-TYPE).")
(defvar w3m-current-favicon-image nil)
(defvar w3m-favicon-converted nil)
(make-variable-buffer-local 'w3m-current-favicon-data)
(make-variable-buffer-local 'w3m-current-favicon-image)
(make-variable-buffer-local 'w3m-favicon-converted)
(add-hook 'w3m-display-functions 'w3m-setup-favicon)

(defcustom w3m-favicon-use-cache-file nil
  "*If non-nil, use favicon cache file."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-favicon-cache-file nil
  "Filename of saving favicon cache."
  :group 'w3m
  :type '(radio (const :format "Not specified\n")
		(file :format "%t: %v\n" :size 0)))

(defcustom w3m-favicon-cache-expire-wait (* 30 24 60 60)
  "*The cache will be expired after specified seconds passed since retrieval.
If this variable is nil, never expired."
  :group 'w3m
  :type '(integer :size 0))

(defcustom w3m-favicon-type
  (let ((types '(pbm png gif xpm bmp))
	type)
    (catch 'det
      (while types
	(setq type (car types)
	      types (cdr types))
	(if (if (featurep 'xemacs)
		(featurep type)
	      (image-type-available-p type))
	    (throw 'det type)))))
  "*Image type of display favicon."
  :group 'w3m
  :type (cons 'radio
	      (let ((types (if (featurep 'xemacs)
			       (delq nil
				     (mapcar (lambda (type)
					       (if (featurep type) type))
					     '(gif jpeg png tiff xpm)))
			     (delq 'postscript (copy-sequence image-types)))))
		(nconc (mapcar (lambda (x)
				 `(const :format "%v  " ,x))
			       (butlast types))
		       `((const ,(car (last types))))))))

(defvar w3m-favicon-type-alist '((pbm . ppm))
  "A list of a difference type of image between Emacs and ImageMagick.
 0. Type of Emacs
 1. Type of ImageMagick")

(defvar w3m-favicon-cache-data nil
  "A list of favicon cache (internal variable).
Each information is a list whose elements are:
 0. URL
 1. Favicon
 2. Retrieved date")

(defmacro w3m-favicon-cache-p (url)
  `(assoc ,url w3m-favicon-cache-data))

(defmacro w3m-favicon-cache-favicon (url)
  `(let ((data (nth 1 (assoc ,url w3m-favicon-cache-data))))
     (if (stringp data) (cons data 'ico) data)))

(defmacro w3m-favicon-cache-retrieved (url)
  `(nth 2 (assoc ,url w3m-favicon-cache-data)))

(defun w3m-setup-favicon (url)
  (setq w3m-current-favicon-data nil
	w3m-current-favicon-image nil
	w3m-favicon-converted nil)
  (when (and w3m-use-favicon
	     w3m-current-url
	     (w3m-static-if (featurep 'xemacs)
		 (and (device-on-window-system-p)
		      (featurep w3m-favicon-type))
	       (display-images-p)
	       (image-type-available-p w3m-favicon-type)))
    (cond
     ((string-match "\\`about://\\([^/]+\\)/" url)
      (let ((icon (intern-soft (concat "w3m-about-" (match-string 1 url)
				       "-favicon"))))
	(if (and (fboundp 'base64-decode-string)
		 icon)
	    (with-current-buffer w3m-current-buffer
	      (setq w3m-current-favicon-data
		    (cons (eval (list 'base64-decode-string
				      (symbol-value icon)))
			  'ico))))))
     ((string-match "\\`https?://" url)
      (w3m-retrieve-favicon
       (or (symbol-value 'w3m-icon-data)
	   (cons (concat url
			 (if (string-match "/\\'" url) "" "/")
			 w3m-favicon-name)
		 'ico))
       w3m-current-buffer)))))

(defun w3m-buffer-favicon (buffer)
  (with-current-buffer buffer
    (when (and w3m-current-favicon-data (car w3m-current-favicon-data))
      (or w3m-current-favicon-image
	  w3m-favicon-converted
	  (lexical-let ((height (number-to-string
				 (w3m-static-if (featurep 'xemacs)
				     (face-height 'default)
				   (frame-char-height))))
			(buffer buffer))
	    (w3m-process-with-null-handler
	      (setq w3m-favicon-converted t)
	      (w3m-process-do
		  (img (w3m-imagick-start-convert-data
			handler
			(car w3m-current-favicon-data)
			(symbol-name (cdr w3m-current-favicon-data))
			(symbol-name (or (cdr (assq w3m-favicon-type
						    w3m-favicon-type-alist))
					 w3m-favicon-type))
			"-geometry" (or w3m-favicon-size
					(concat height "x" height))))
		(with-current-buffer buffer
		  (if img
		      (setq w3m-current-favicon-image
			    (w3m-static-if (featurep 'xemacs)
				(make-glyph
				 (make-image-instance
				  (vector w3m-favicon-type :data img)))
			      (create-image img
					    w3m-favicon-type
					    t
					    :ascent 'center)))
		    (setq w3m-current-favicon-data nil))))))))))

(defun w3m-retrieve-favicon (pair target &optional handler)
  (if (and (w3m-favicon-cache-p (car pair))
	   (or (null w3m-favicon-cache-expire-wait)
	       (< (- (w3m-float-time)
		     (w3m-float-time (w3m-favicon-cache-retrieved
				      (car pair))))
		  w3m-favicon-cache-expire-wait)))
      (setq w3m-current-favicon-data
	    (w3m-favicon-cache-favicon (car pair)))
    (lexical-let ((pair pair)
		  (target target))
      (w3m-process-do-with-temp-buffer
	  (ok (w3m-retrieve (car pair) 'raw nil nil nil handler))
	(let (idata)
	  (when ok
	    (setq idata (buffer-string))
	    (with-current-buffer target
	      (setq w3m-current-favicon-data (cons idata (cdr pair)))))
	  (push (list (car pair) (and idata (cons idata (cdr pair)))
		      (current-time))
		w3m-favicon-cache-data))))))

(defun w3m-favicon-save-cache-file ()
  (when w3m-favicon-use-cache-file
    (w3m-save-list (or w3m-favicon-cache-file
		       (expand-file-name ".favicon" w3m-profile-directory))
		   w3m-favicon-cache-data 'binary)))

(defun w3m-favicon-load-cache-file ()
  (when (and w3m-favicon-use-cache-file
	     (null w3m-favicon-cache-data))
    (setq w3m-favicon-cache-data
	  (w3m-load-list
	   (or w3m-favicon-cache-file
	       (expand-file-name ".favicon" w3m-profile-directory))
	   'binary))))

(provide 'w3m-favicon)

;;; w3m-favicon.el ends here

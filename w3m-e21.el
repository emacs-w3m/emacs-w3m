;;; w3m-e21.el --- The stuffs to use emacs-w3m on Emacs-21.

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

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

;; This file contains the stuffs to use emacs-w3m on Emacs-21.  For
;; more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/


;;; Code:

(require 'w3m-macro)
(require 'w3m-fsf)

;; Functions and variables which should be defined in the other module
;; at run-time.
(eval-when-compile
  (defvar w3m-display-inline-image)
  (defvar w3m-icon-directory)
  (defvar w3m-mode-map)
  (defvar w3m-toolbar)
  (defvar w3m-toolbar-buttons)
  (defvar w3m-use-header-line)
  (defvar w3m-work-buffer-name)
  (defvar w3m-history)
  (defvar w3m-history-flat)
  (defvar w3m-form-use-fancy-faces)
  (autoload 'w3m-retrieve "w3m")
  (autoload 'w3m-image-type "w3m"))

;; Image handling functions.

;; `display-images-p' has not been available prior to Emacs 21.0.105.
(unless (fboundp 'display-images-p)
  (defalias 'display-images-p 'display-graphic-p))

;; Function which returns non-nil when the current display device can
;; show images inline.
(defalias 'w3m-display-graphic-p 'display-images-p)

(defun w3m-display-inline-image-p ()
  "Returns non-nil when images can be displayed under the present
circumstances."
  (and w3m-display-inline-image (display-images-p)))

(defun w3m-create-image (url &optional no-cache referer)
  "Retrieve data from URL and create an image object.
If optional argument NO-CACHE is non-nil, cache is not used.
If second optional argument REFERER is non-nil, it is used as Referer: field."
  (condition-case err
      (let ((type (w3m-retrieve url 'raw no-cache nil referer)))
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
  (add-text-properties beg end (list 'display image
				     'intangible image
				     'invisible nil))
  ;; Hide underlines behind inline images.

  ;; Gerd Moellmann <gerd@gnu.org>, the maintainer of Emacs 21, wrote in
  ;; the article <86heyi7vks.fsf@gerd.segv.de> in the list emacs-pretest-
  ;; bug@gnu.org on 18 May 2001 that to show an underline of a text even
  ;; if it has an image as a text property is the feature of Emacs 21.
  ;; However, that behavior is not welcome to the w3m buffers, so we do
  ;; to fix it with the following stuffs.
  (let ((face (get-text-property beg 'face)))
    (when (and face
	       (face-underline-p face))
      (put-text-property beg end 'face nil)
      (put-text-property beg end 'w3m-underline-face face))))

(defun w3m-remove-image (beg end)
  "Remove an image which is inserted between BEG and END."
  (remove-text-properties beg end '(display nil intangible nil))
  (let ((underline (get-text-property beg 'w3m-underline-face)))
    (when underline
      (put-text-property beg end 'face underline))))

(defun w3m-image-type-available-p (image-type)
  "Return non-nil if an image with IMAGE-TYPE can be displayed inline."
  (and (display-images-p)
       (image-type-available-p image-type)))

;;; Form buttons
(defface w3m-form-button-face
  '((((type x w32 mac) (class color))
     (:box (:line-width 1 :style released-button)
	   :background "lightgrey" :foreground "black"))
    (((class color) (background light)) (:foreground "cyan" :underline t))
    (((class color) (background dark)) (:foreground "red" :underline t))
    (t (:underline t)))
  "*Face to fontify buttons in forms."
  :group 'w3m-face)

(defface w3m-form-button-pressed-face
  '((((type x w32 mac) (class color))
     (:box (:line-width 1 :style pressed-button)
	   :background "lightgrey" :foreground "black"))
    (((class color) (background light)) (:foreground "cyan" :underline t))
    (((class color) (background dark)) (:foreground "red" :underline t))
    (t (:underline t)))
  "*Face to fontify pressed buttons in forms."
  :group 'w3m-face)

(defun w3m-form-make-button (start end properties)
  "Make button on the region from START to END."
  (if w3m-form-use-fancy-faces
      (or (and (face-attribute 'w3m-form-button-face :box)
	       (eq ?\[ (char-after start))
	       (eq ?\] (char-before end))
	       (save-excursion
		 (goto-char start)
		 (delete-char 1)
		 (insert " ")
		 (goto-char end)
		 (delete-char -1)
		 (insert " ")
		 (add-text-properties
		  (1+ start) (1- end)
		  (append '(face
			    w3m-form-button-face
			    mouse-face
			    w3m-form-button-pressed-face)
			  properties))))
	  (add-text-properties
	   start end
	   (append '(face
		     w3m-form-button-face
		     mouse-face
		     w3m-form-button-pressed-face)
		   properties)))
    (add-text-properties start end
			 (append '(face w3m-form-face)
				 properties))))

;;; Toolbar
(defcustom w3m-use-toolbar (w3m-image-type-available-p 'xpm)
  "Non-nil activates toolbar of w3m."
  :group 'w3m
  :type 'boolean)

(defvar w3m-e21-toolbar-configurations
  '((auto-resize-tool-bar        . t)
    (auto-raise-tool-bar-buttons . t)
    (tool-bar-button-margin      . 0)
    (tool-bar-button-relief      . 2)))

(defun w3m-e21-setup-toolbar (keymap defs)
  (let ((configs w3m-e21-toolbar-configurations)
	config)
    (while (setq config (pop configs))
      (set (make-local-variable (car config)) (cdr config))))
  ;; Invalidate the default bindings.
  (let ((keys (cdr (key-binding [tool-bar] t)))
	item)
    (while (setq item (pop keys))
      (when (setq item (car-safe item))
	(define-key keymap (vector 'tool-bar item) 'undefined))))
  (let ((n (length defs))
	def)
    (while (>= n 0)
      (setq n (1- n)
	    def (nth n defs))
      (define-key keymap (vector 'tool-bar (aref def 1))
	(list 'menu-item (aref def 3) (aref def 1)
	      :enable (aref def 2)
	      :image (symbol-value (aref def 0)))))))

(defun w3m-e21-make-toolbar-buttons (buttons)
  (dolist (button buttons)
    (let ((up (expand-file-name (concat button "-up.xpm")
				w3m-icon-directory))
	  (down (expand-file-name (concat button "-down.xpm")
				  w3m-icon-directory))
	  (disabled (expand-file-name (concat button "-disabled.xpm")
				      w3m-icon-directory))
	  (icon (intern (concat "w3m-toolbar-" button "-icon")))
	  (props '(:ascent
		   center
		   :color-symbols (("backgroundToolBarColor" . "None")))))
      (unless (boundp icon)
	(if (file-exists-p up)
	    (progn
	      (setq up (apply 'create-image up 'xpm nil props))
	      (if (file-exists-p down)
		  (setq down (apply 'create-image down 'xpm nil props))
		(setq down nil))
	      (if (file-exists-p disabled)
		  (setq disabled (apply 'create-image disabled 'xpm nil props))
		(setq disabled nil))
	      (set icon (vector down up disabled disabled)))
	  (error "Icon file %s not found" up))))))

(defun w3m-setup-toolbar ()
  (when w3m-use-toolbar
    (w3m-e21-make-toolbar-buttons w3m-toolbar-buttons)
    (w3m-e21-setup-toolbar w3m-mode-map w3m-toolbar)))

(defalias 'w3m-update-toolbar 'ignore)

;;; Header line
(defface w3m-header-line-location-title-face
  '((((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "Cyan")))
  "*Face for header-line location title."
  :group 'w3m-face)

(defface w3m-header-line-location-content-face
  '((((class color) (background light)) (:foreground "DarkGoldenrod"))
    (((class color) (background dark)) (:foreground "LightGoldenrod")))
  "*Face for header-line location content."
  :group 'w3m-face)

(defun w3m-setup-header-line ()
  "Setup header line."
  (if w3m-use-header-line
      (setq header-line-format (list
				(propertize
				 "Location: "
				 'face
				 'w3m-header-line-location-title-face)
				'(:eval
				  (propertize
				   (replace-regexp-in-string "%" "%%"
							     w3m-current-url)
				   'face
				   'w3m-header-line-location-content-face
				   'local-map
				   (let ((map (make-sparse-keymap)))
				     (define-key map [header-line mouse-2]
				       'w3m-goto-url)
				     map)
				   'help-echo
				   "mouse-2 prompts to input URL"))))))

(add-hook 'w3m-mode-hook 'w3m-setup-header-line)

(provide 'w3m-e21)
;;; w3m-e21.el ends here.

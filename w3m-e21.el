;;; w3m-e21.el --- The stuffs to use w3m.el on Emacs-21.

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

(provide 'w3m-e21);; It is needed to avoid circular dependencies.
(require 'w3m)

(defun w3m-create-image (url &optional no-cache)
  "Retrieve data from URL and create an image object.
If optional argument NO-CACHE is non-nil, cache is not used."
  (condition-case err
      (let ((type (w3m-retrieve url 'raw no-cache)))
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
  (unless (car w3m-cache-underline-faces)
    ;; Detach an underlined face if it exists.
    (let ((face (get-text-property beg 'face)))
      (when (and face
		 (face-underline-p face))
	(put-text-property beg end 'face nil)
	(push (list (set-marker (make-marker) beg)
		    (set-marker (make-marker) end)
		    face)
	      (cdr w3m-cache-underline-faces))))))

(defun w3m-remove-image (beg end)
  "Remove an image which is inserted between BEG and END."
  (remove-text-properties beg end '(display nil intangible nil)))

(defun w3m-image-type-available-p (image-type)
  "Return non-nil if an image with IMAGE-TYPE can be displayed inline."
  (and (display-graphic-p)
       (image-type-available-p image-type)))

;;; Toolbar
(defcustom w3m-use-toolbar (and (display-graphic-p)
				(image-type-available-p 'xpm))
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
				   w3m-current-url
				   'face
				   'w3m-header-line-location-content-face
				   'local-map
				   (let ((map (make-sparse-keymap)))
				     (define-key map [header-line mouse-2]
				       'w3m-goto-url)
				     map)
				   'help-echo
				   "mouse-2 prompts to input URL."))))))

(add-hook 'w3m-mode-hook 'w3m-setup-header-line)
;;; w3m-e21.el ends here.

;;; w3m-e21.el --- The stuffs to use emacs-w3m on Emacs-21

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

(require 'w3m-util)
(require 'w3m-proc)
(require 'w3m-fsf)
(require 'wid-edit)

;; Functions and variables which should be defined in the other module
;; at run-time.
(eval-when-compile
  (defvar w3m-current-url)
  (defvar w3m-current-title)
  (defvar w3m-current-process)
  (defvar w3m-current-buffer)
  (defvar w3m-display-inline-images)
  (defvar w3m-icon-directory)
  (defvar w3m-mode-map)
  (defvar w3m-toolbar)
  (defvar w3m-toolbar-buttons)
  (defvar w3m-use-tab)
  (defvar w3m-use-header-line)
  (defvar w3m-work-buffer-name)
  (defvar w3m-history)
  (defvar w3m-history-flat)
  (defvar w3m-form-use-fancy-faces)
  (autoload 'w3m-expand-url "w3m")
  (autoload 'w3m-retrieve "w3m")
  (autoload 'w3m-image-type "w3m"))

;;; Coding system.

(defun w3m-make-ccl-coding-system
  (coding-system mnemonic docstring decoder encoder)
  "Define a new CODING-SYSTEM by CCL programs DECODER and ENCODER.
CODING-SYSTEM, DECODER and ENCODER must be symbol."
  (make-coding-system coding-system 4 mnemonic docstring
		      (cons decoder encoder)))

;;; Image handling functions.

;; `display-images-p' has not been available prior to Emacs 21.0.105.
(unless (fboundp 'display-images-p)
  (defalias 'display-images-p 'display-graphic-p))

;; Function which returns non-nil when the current display device can
;; show images inline.
(defalias 'w3m-display-graphic-p 'display-images-p)

(defun w3m-display-inline-images-p ()
  "Returns non-nil when images can be displayed under the present
circumstances."
  (and w3m-display-inline-images (display-images-p)))

(defun w3m-create-image (url &optional no-cache referer handler)
  "Retrieve data from URL and create an image object.
If optional argument NO-CACHE is non-nil, cache is not used.
If second optional argument REFERER is non-nil, it is used as Referer: field."
  (if (not handler)
      (w3m-process-with-wait-handler
	(w3m-create-image url no-cache referer handler))
    (w3m-process-do-with-temp-buffer
	(type (w3m-retrieve url 'raw no-cache nil referer handler))
      (ignore-errors
	(when (w3m-image-type-available-p (setq type (w3m-image-type type)))
	  (create-image (buffer-string)
			type
			t
			:ascent 'center))))))

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
     :background "lightgrey" :foreground "black"
     :box (:line-width 2 :style released-button))
    (((class color) (background light)) (:foreground "cyan" :underline t))
    (((class color) (background dark)) (:foreground "red" :underline t))
    (t (:underline t)))
  "*Face to fontify buttons in forms."
  :group 'w3m-face)

(defface w3m-form-button-mouse-face
  '((((type x w32 mac) (class color))
     :background "DarkSeaGreen1" :foreground "black"
     :box (:line-width 2 :style released-button))
    (((class color) (background light)) (:foreground "cyan" :underline t))
    (((class color) (background dark)) (:foreground "red" :underline t))
    (t (:underline t)))
  "*Face to fontify focused buttons in forms."
  :group 'w3m-face)

(defface w3m-form-button-pressed-face
  '((((type x w32 mac) (class color))
     :background "lightgrey" :foreground "black"
     :box (:line-width 2 :style pressed-button))
    (((class color) (background light)) (:foreground "cyan" :underline t))
    (((class color) (background dark)) (:foreground "red" :underline t))
    (t (:underline t)))
  "*Face to fontify pressed buttons in forms."
  :group 'w3m-face)

(defvar w3m-form-button-keymap
  (let ((map (copy-keymap widget-keymap)))
    (substitute-key-definition 'widget-forward nil map)
    (substitute-key-definition 'widget-backward nil map)
    map))

(define-widget 'w3m-form-button 'push-button
  "Widget for w3m form button."
  :keymap w3m-form-button-keymap
  :action (function (lambda (widget &optional e)
		      (eval (widget-get widget :w3m-form-action)))))

(defun w3m-form-make-button (start end properties)
  "Make button on the region from START to END."
  (if w3m-form-use-fancy-faces
      (progn
	(unless (memq (face-attribute 'w3m-form-button-face :box)
		      '(nil unspecified))
	  (and (eq ?\[ (char-after start))
	       (eq ?\] (char-before end))
	       (save-excursion
		 (goto-char start)
		 (delete-char 1)
		 (insert " ")
		 (goto-char end)
		 (delete-char -1)
		 (insert " ")
		 (setq start (1+ start)
		       end (1- end)))))
	(widget-convert-button
	 'w3m-form-button start end
	 :w3m-form-action (plist-get properties 'w3m-action))
	(add-text-properties start end properties))
    (add-text-properties start end (append '(face w3m-form-face)
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

;;; favicon
(defcustom w3m-imagick-convert-program "convert"
  "*Program name of ImageMagick's `convert'."
  :group 'w3m
  :type 'string)

(defcustom w3m-favicon-size "16x16"
  "*Size of favicon. This value is used as geometry argument for `convert'."
  :group 'w3m
  :type 'string)

(defcustom w3m-use-favicon nil
  "*Use favicon."
  :group 'w3m
  :type 'boolean)

(defconst w3m-favicon-name "favicon.ico"
  "The favicon name.")

(defvar w3m-current-favicon-data nil)
(defvar w3m-current-favicon-image nil)
(make-variable-buffer-local 'w3m-current-favicon-data)
(make-variable-buffer-local 'w3m-current-favicon-image)
(add-hook 'w3m-display-hook 'w3m-setup-favicon)

(defun w3m-imagick-convert-buffer (from-type to-type &rest args)
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary)
	(default-process-coding-system (cons 'binary 'binary)))
    (zerop (apply 'call-process-region
		  (point-min) (point-max)
		  w3m-imagick-convert-program
		  t t nil (append args (list (concat from-type ":-")
					     (concat to-type ":-")))))))

(defun w3m-imagick-convert-data (data from-type to-type &rest args)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert data)
    (and (apply 'w3m-imagick-convert-buffer from-type to-type args)
	 (buffer-string))))

(defun w3m-setup-favicon (url)
  (setq w3m-current-favicon-data nil
	w3m-current-favicon-image nil)
  (when (and w3m-use-favicon
	     (w3m-image-type-available-p 'xpm))
    (if (string-match "\\`about://\\([^/]+\\)/" url)
	(let ((icon (intern-soft (concat "w3m-about-" (match-string 1 url)
					 "-favicon"))))
	  (if (and (fboundp 'base64-decode-string)
		   (boundp icon))
	      (with-current-buffer w3m-current-buffer
		(setq w3m-current-favicon-data
		      (eval (list 'base64-decode-string
				  (symbol-value icon)))))))
      (w3m-retrieve-favicon
       (w3m-expand-url (concat "/" w3m-favicon-name) url)
       w3m-current-buffer))))

(defun w3m-buffer-favicon (buffer)
  (with-current-buffer buffer
    (when w3m-current-favicon-data
      (or w3m-current-favicon-image
	  (let ((xpm (w3m-imagick-convert-data
		      w3m-current-favicon-data
		      "ico" "xpm" "-geometry" w3m-favicon-size)))
	    (and xpm
		 (setq w3m-current-favicon-image
		       (create-image xpm
				     'xpm
				     t
				     :ascent 'center))))))))

(defun w3m-retrieve-favicon (url target &optional handler)
  (lexical-let ((url url)
		(target target))
    (w3m-process-do-with-temp-buffer
	(ok (w3m-retrieve url 'raw nil nil nil handler))
      (when ok
	(let ((data (buffer-string)))
	  (with-current-buffer target
	    (setq w3m-current-favicon-data data)))))))

;;; Header line & Tabs
(defface w3m-header-line-location-title-face
    '((((class color) (background light))
       (:foreground "Blue" :background "Gray90"))
      (((class color) (background dark))
       (:foreground "Cyan" :background "Gray20")))
  "*Face for header-line location title."
  :group 'w3m-face)

(defface w3m-header-line-location-content-face
  '((((class color) (background light))
     (:foreground "DarkGoldenrod" :background "Gray90"))
    (((class color) (background dark))
     (:foreground "LightGoldenrod" :background "Gray20")))
  "*Face for header-line location content."
  :group 'w3m-face)

(defcustom w3m-tab-width 11
  "w3m tab width."
  :group 'w3m
  :type 'integer)

(defface w3m-tab-unselected-face
  '((((type x w32 mac) (class color))
     :background "Gray50" :foreground "Gray20"
     :underline "Gray85" :box (:line-width -1 :style released-button))
    (((class color))
     (:background "cyan" :foreground "black" :underline "blue")))
  "*Face to fontify unselected tabs."
  :group 'w3m-face)

(defface w3m-tab-unselected-retrieving-face
  '((((type x w32 mac) (class color))
     :background "Gray50" :foreground "OrangeRed"
     :underline "Gray85" :box (:line-width -1 :style released-button))
    (((class color))
     (:background "cyan" :foreground "OrangeRed" :underline "blue")))
  "*Face to fontify unselected tabs which are retrieving their pages."
  :group 'w3m-face)

(defface w3m-tab-selected-face
  '((((type x w32 mac) (class color))
     :background "Gray85" :foreground "black"
     :underline "Gray85" :box (:line-width -1 :style released-button))
    (((class color))
     (:background "blue" :foreground "black" :underline "blue"))
    (t (:underline t)))
  "*Face to fontify selected tab."
  :group 'w3m-face)

(defface w3m-tab-selected-retrieving-face
  '((((type x w32 mac) (class color))
     :background "Gray85" :foreground "red"
     :underline "Gray85" :box (:line-width -1 :style released-button))
    (((class color))
     (:background "blue" :foreground "red" :underline "blue"))
    (t (:underline t)))
  "*Face to fontify selected tab which is retrieving its page."
  :group 'w3m-face)

(defface w3m-tab-background-face
  '((((type x w32 mac) (class color))
     :background "LightSteelBlue" :foreground "black"
     :underline "Gray85")
    (((class color))
     (:background "white" :foreground "black" :underline "blue")))
  "*Face to fontify background of tab line."
  :group 'w3m-face)

(defvar w3m-header-line-map (make-sparse-keymap))
(define-key w3m-header-line-map [mouse-2] 'w3m-goto-url)

(defun w3m-setup-header-line ()
  (if w3m-use-tab
      (setq header-line-format (list '(:eval (w3m-tab-line))))
    (if w3m-use-header-line
	(setq header-line-format
	      (list
	       (propertize
		"Location: "
		'face
		'w3m-header-line-location-title-face)
	       '(:eval
		 (propertize
		  (if (stringp w3m-current-url)
		      (replace-regexp-in-string "%" "%%" w3m-current-url)
		    "")
		  'face
		  'w3m-header-line-location-content-face
		  'local-map
		  (let ((map (make-sparse-keymap)))
		    (define-key map [header-line mouse-2]
		      'w3m-goto-url)
		    map)
		  'help-echo
		  "mouse-2 prompts to input URL")))))))

(defun w3m-insert-header-line ()
  (when (and w3m-use-tab
	     w3m-use-header-line
	     (stringp w3m-current-url)
	     (eq 'w3m-mode major-mode))
    (goto-char (point-min))
    (insert "Location: ")
    (put-text-property (point-min) (point)
		       'face 'w3m-header-line-location-title-face)
    (let ((start (point))
	  (help "button2 prompts to input URL"))
      (insert w3m-current-url)
      (add-text-properties start (point)
			   (list 'face
				 'w3m-header-line-location-content-face
				 'mouse-face 'highlight
				 'keymap w3m-header-line-map
				 'help-echo help
				 'balloon-help help))
      (setq start (point))
      (insert-char ?\  (max 0 (- (window-width) (current-column) 1)))
      (put-text-property start (point)
			 'face 'w3m-header-line-location-content-face)
      (insert "\n"))))

(defun w3m-setup-widget-faces ()
  (make-local-variable 'widget-button-face)
  (make-local-variable 'widget-mouse-face)
  (make-local-variable 'widget-button-pressed-face)
  (setq widget-button-face 'w3m-form-button-face)
  (setq widget-mouse-face 'w3m-form-button-mouse-face)
  (setq widget-button-pressed-face 'w3m-form-button-pressed-face))

(defun w3m-tab-make-keymap (buffer)
  (let ((map (make-sparse-keymap))
	(action `(lambda (e) (interactive "e")
		   (switch-to-buffer ,(buffer-name buffer)))))
    (define-key map [header-line down-mouse-1] action)
    (define-key map [header-line down-mouse-2] action)
    map))

(defun w3m-tab-line ()
  (let ((current (current-buffer)))
    (concat
     (save-current-buffer
       (mapconcat
	(lambda (buffer)
	  (let ((title (w3m-buffer-title buffer))
		(favicon (if w3m-use-favicon (w3m-buffer-favicon buffer))))
	    (propertize
	     (concat (if favicon
			 (propertize " " 'display favicon)
		       " ")
		     (if (and (> w3m-tab-width 0)
			      (> (string-width title) w3m-tab-width))
			 (concat (truncate-string-to-width
				  title
				  (max 0 (- w3m-tab-width 3)))
				 "...")
		       title)
		     " ")
	     'mouse-face 'highlight
	     'face (if (progn (set-buffer buffer) w3m-current-process)
		       (if (eq buffer current)
			   'w3m-tab-selected-retrieving-face
			 'w3m-tab-unselected-retrieving-face)
		     (if (eq buffer current)
			 'w3m-tab-selected-face
		       'w3m-tab-unselected-face))
	     'local-map (w3m-tab-make-keymap buffer)
	     'help-echo title)))
	(w3m-list-buffers)
	(propertize " " 'face 'w3m-tab-background-face)))
     (propertize (make-string (window-width) ?\ )
		 'face 'w3m-tab-background-face))))

(add-hook 'w3m-mode-hook 'w3m-setup-header-line)
(add-hook 'w3m-mode-hook 'w3m-setup-widget-faces)
(add-hook 'w3m-fontify-after-hook 'w3m-insert-header-line)

(provide 'w3m-e21)

;;; w3m-e21.el ends here

;;; w3m-xmas.el --- The stuffs to use emacs-w3m on XEmacs.

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: Yuuichi Teranishi  <teranisi@gohome.org>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
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
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.


;;; Commentary:

;; This file contains the stuffs to use emacs-w3m on XEmacs.  For more
;; detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/


;;; Code:

(require 'w3m-macro)

;; Functions and variables which should be defined in the other module
;; at run-time.
(eval-when-compile
  (defvar w3m-current-image-status)
  (defvar w3m-current-url)
  (defvar w3m-default-coding-system)
  (defvar w3m-history)
  (defvar w3m-history-flat)
  (defvar w3m-icon-directory)
  (defvar w3m-menubar)
  (defvar w3m-toolbar)
  (defvar w3m-toolbar-buttons)
  (defvar w3m-use-header-line)
  (defvar w3m-work-buffer-name)
  (autoload 'update-tab-in-gutter "gutter-items")
  (autoload 'w3m-image-type "w3m")
  (autoload 'w3m-retrieve "w3m"))

(require 'path-util)
(require 'poe)
(require 'poem)
(require 'pccl)

;;; Handle coding system:
(defalias 'w3m-find-coding-system 'find-coding-system)
(defalias 'w3m-make-ccl-coding-system 'make-ccl-coding-system)

(defun w3m-detect-coding-region (start end &optional priority-list)
  "Detect coding system of the text in the region between START and END.
Return the first possible coding system.

PRIORITY-LIST is a list of coding systems ordered by priority."
  (let (category categories codesys)
    (dolist (codesys priority-list)
      (setq category (coding-system-category codesys))
      (unless (assq category categories)
	(push (cons category codesys) categories)))
    (if (consp (setq codesys (detect-coding-with-priority
			      start end (nreverse categories))))
	(car codesys)
      codesys)))

;;; Handle images:

;; Function which returns non-nil when the current display device can
;; show images inline.
(defalias 'w3m-display-graphic-p 'device-on-window-system-p)

(defun w3m-display-inline-image-p ()
  "Returns non-nil when images can be displayed under the present
circumstances."
  (and w3m-current-image-status (device-on-window-system-p)))

(defvar w3m-should-unoptimize-animated-gifs
  (and (= emacs-major-version 21)
       (or (>= emacs-minor-version 4)
	   (and (= emacs-minor-version 2)
		(>= (or emacs-patch-level emacs-beta-version) 20))))
  "Specify whether w3m should unoptimize animated gif images for showing.
It is applicable to XEmacs 21.2.20 or later, since which only support
to show unoptimized animated gif images.")

(defvar w3m-should-convert-interlaced-gifs t
  "Specify whether w3m should convert interlaced gif images to be non-
interlaced.  There is a known bug in the whole version of XEmacs so
far that it may crash when some kind of an interlaced gif image is
displayed.")

(defvar w3m-gifsicle-program (when (exec-installed-p "gifsicle")
			       "gifsicle")
  "*Name of the gifsicle program used to unoptimize animated gif images
or to convert interlaced gif images to be non-interlaced.")

(defvar w3m-cache-fixed-gif-images nil
  "Cache used to keep fixed gif images.  Fixed gif means that an
unoptimized animated gif or a non-interlaced gif which is converted
from an interlaced gif.  The value is an alist of a URL and a glyph.
Glyph will be nil if there is no need to be unoptimized nor to convert
to be non-interlaced.  Each element should be updated when a URL is
newly retrieved.")

(defvar w3m-cache-fixed-gif-images-max-length 32
  "*Number to limit the length of `w3m-cache-fixed-gif-images'.")

(defvar w3m-animated-gif-maximum-size 1048579
  "*Maximum size (width * height * frames) of animated gif images.  If a
size of an image is larger than this (it might be a bomb!), only the
first frame will be shown.  You can make it to be unlimited with the
value nil if your computer has TerrrrrrraBytes of memories.")

(defun w3m-fix-gif (url data no-cache)
  "Return a glyph image of a gif DATA corresponding to a URL which is
suitable for XEmacs.  It will unoptimize an animated gif or convert
an interlaced gif to be non-interlaced.  Otherwise it returns nil when
there is no need to be unoptimized nor to convert to be non-interlaced
\(or, unfortunately, the unoptimization or the conversion is failed).
It manages the cache `w3m-cache-fixed-gif-images'.  If NO-CACHE is
non-nil, a cached data will not be used and it will be updated by a
new glyph image.  See also the documentation for the variable
`w3m-animated-gif-maximum-size'."
  (let ((cache (assoc url w3m-cache-fixed-gif-images)))
    ;; Move the element which is associated with `url' to the
    ;; top of the cache.
    (when (and cache
	       (not (eq cache (car w3m-cache-fixed-gif-images))))
      (setq w3m-cache-fixed-gif-images
	    (cons cache (delq cache w3m-cache-fixed-gif-images))))
    (if (or no-cache
	    (not cache))
	(with-temp-buffer
	  (let ((coding-system-for-read 'binary)
		(coding-system-for-write 'binary)
		should-be-non-interlaced should-unoptimize size1 size2 glyph)
	    (insert data)
	    (goto-char (point-min))
	    (when (looking-at "GIF8[79]")
	      (call-process-region (point-min) (point-max)
				   w3m-gifsicle-program
				   t t nil "--info")
	      (goto-char (point-min))
	      ;; Check whether a `data' is interlaced.
	      (setq should-be-non-interlaced
		    (re-search-forward
		     "  \\+ image #[0-9]+ \\([0-9]+x[0-9]+\\).* interlaced"
		     nil t))
	      (goto-char (point-min))
	      ;; Check whether a `data' is optimized or larger than
	      ;; the value of `w3m-animated-gif-maximum-size'.
	      (when (and w3m-animated-gif-maximum-size
			 (looking-at ".+ \\([0-9]+\\) images\r?$"))
		(setq size1 (string-to-number (match-string 1)))
		(forward-line 1)
		(unless (and (looking-at ".+ \\([0-9]+\\)x\\([0-9]+\\)\r?$")
			     (natnump (setq size1 (* size1
						     (string-to-number
						      (match-string 1)))))
			     (<= size1 w3m-animated-gif-maximum-size)
			     (natnump (setq size1 (* size1
						     (string-to-number
						      (match-string 2)))))
			     (<= size1 w3m-animated-gif-maximum-size))
		  ;; It should be truncated to be only one frame.
		  (setq should-unoptimize "#0"))
		(setq size1 nil)
		(while (and (not should-unoptimize)
			    (re-search-forward
			     "  \\+ image #[0-9]+ \\([0-9]+x[0-9]+\\)"
			     nil t))
		  (if size1
		      (if (string-equal size1 (setq size2 (match-string 1)))
			  (setq size1 size2)
			(setq should-unoptimize "--unoptimize"))
		    (setq size1 (match-string 1)))))
	      (when (or should-unoptimize should-be-non-interlaced)
		(erase-buffer)
		(insert data)
		;; Unoptimize anyway.
		(if should-unoptimize
		    (call-process-region (point-min) (point-max)
					 w3m-gifsicle-program
					 t t nil should-unoptimize
					 "--no-interlace")
		  (call-process-region (point-min) (point-max)
				       w3m-gifsicle-program
				       t t nil "--no-interlace"))
		(goto-char (point-min))
		(when (or (looking-at "GIF8[79]")
			  ;; Unoptimization is failed. :-<
			  ;; Attempt to extract the first frame.
			  (progn
			    (erase-buffer)
			    (insert data)
			    (call-process-region (point-min) (point-max)
						 w3m-gifsicle-program
						 t t nil "#0" "--no-interlace")
			    (goto-char (point-min))
			    (looking-at "GIF8[79]")))
		  ;; Perhaps the unoptimization is succeeded.
		  (setq glyph
			(make-glyph (vector 'gif :data (buffer-string)))))))
	    ;; Update a cache.
	    (if cache
		(setcdr cache glyph)
	      (push (cons url glyph) w3m-cache-fixed-gif-images)
	      (let ((maxlen w3m-cache-fixed-gif-images-max-length))
		(when (and (integerp maxlen)
			   (>= maxlen 1)
			   (> (length w3m-cache-fixed-gif-images) maxlen))
		  (setcdr (nthcdr (1- maxlen) w3m-cache-fixed-gif-images)
			  nil))))
	    glyph))
      ;; Use a cached glyph.
      (cdr cache))))

(defun w3m-create-image (url &optional no-cache referer)
  "Retrieve data from URL and create an image object.
If optional argument NO-CACHE is non-nil, cache is not used.
If second optional argument REFERER is non-nil, it is used as Referer: field."
  (let ((type (condition-case err
		  (w3m-retrieve url 'raw no-cache nil referer)
		(error
		 (message "While retrieving %s: %s" url err)
		 nil))))
    (when (w3m-image-type-available-p (setq type (w3m-image-type type)))
      (let ((data (w3m-with-work-buffer (buffer-string))))
	(or (and (eq type 'gif)
		 (or w3m-should-unoptimize-animated-gifs
		     w3m-should-convert-interlaced-gifs)
		 w3m-gifsicle-program
		 (w3m-fix-gif url data no-cache))
	    (make-glyph (vector type :data data)))))))

(defun w3m-insert-image (beg end image)
  "Display image on the current buffer.
Buffer string between BEG and END are replaced with IMAGE."
  (let (extent glyphs)
    (while (setq extent (extent-at beg nil 'w3m-xmas-icon extent 'at))
      (push (extent-end-glyph extent) glyphs))
    (set-extent-properties (make-extent beg end)
			   (list 'invisible t 'w3m-xmas-icon t
				 'end-glyph image))
    (while glyphs
      (set-extent-properties (make-extent end end)
			     (list 'w3m-xmas-icon t
				   'end-glyph (pop glyphs))))))

(defun w3m-remove-image (beg end)
  "Remove an image which is inserted between BEG and END."
  (let (extent)
    (while (setq extent (extent-at beg nil 'w3m-xmas-icon extent 'at))
      (if (extent-end-glyph extent)
	  (set-extent-end-glyph extent nil))
      (set-extent-property extent 'invisible nil))
    (while (setq extent (extent-at end nil 'w3m-xmas-icon extent 'at))
      (if (extent-end-glyph extent)
	  (set-extent-end-glyph extent nil))
      (set-extent-property extent 'invisible nil))))

(defun w3m-image-type-available-p (image-type)
  "Return non-nil if an image with IMAGE-TYPE can be displayed inline."
  (and (device-on-window-system-p)
       (featurep image-type)))

;;; Toolbar:
(defcustom w3m-use-toolbar (and (featurep 'toolbar) t)
  "Non-nil activates toolbar of w3m."
  :group 'w3m
  :type 'boolean)

(defun w3m-xmas-make-toolbar-buttons (buttons)
  (dolist (button buttons)
    (let ((up (expand-file-name (concat button "-up.xpm")
				w3m-icon-directory))
	  (down (expand-file-name (concat button "-down.xpm")
				  w3m-icon-directory))
	  (disabled (expand-file-name (concat button "-disabled.xpm")
				      w3m-icon-directory))
	  (icon (intern (concat "w3m-toolbar-" button "-icon"))))
      (if (file-exists-p up)
	  (set icon
	       (toolbar-make-button-list
		up
		(and (file-exists-p down) down)
		(and (file-exists-p disabled) disabled)))
	(error "Icon file %s not found" up)))))

(defun w3m-setup-toolbar ()
  "Setup toolbar."
  (when w3m-use-toolbar
    (w3m-xmas-make-toolbar-buttons w3m-toolbar-buttons)
    (set-specifier default-toolbar
		   (cons (current-buffer) w3m-toolbar))))

(defun w3m-update-toolbar ()
  "Update toolbar."
  (when w3m-use-toolbar
    (set-specifier default-toolbar
		   (cons (current-buffer) w3m-toolbar))))

;;; Menu:
(defun w3m-setup-menu ()
  "Define menubar buttons for XEmacs."
  (when (and (featurep 'menubar)
	     current-menubar
	     (not (assoc (car w3m-menubar) current-menubar)))
    (set-buffer-menubar (cons w3m-menubar current-menubar))))

;;; Widget:
(eval-when-compile (require 'wid-edit))

(defun w3m-xmas-define-missing-widgets ()
  "Define some missing widgets."
  (unless (get 'coding-system 'widget-type)
    ;; The following codes are imported from wid-edit.el of Emacs 20.7.

    (defvar widget-coding-system-prompt-value-history nil
      "History of input to `widget-coding-system-prompt-value'.")

    (defun widget-coding-system-prompt-value (widget prompt value unbound)
      ;; Read coding-system from minibuffer.
      (intern
       (completing-read (format "%s (default %s) " prompt value)
			(mapcar (lambda (sym)
				  (list (symbol-name sym)))
				(coding-system-list)))))

    (defun widget-coding-system-action (widget &optional event)
      ;; Read a file name from the minibuffer.
      (let ((answer
	     (eval (list 'widget-coding-system-prompt-value
			 widget
			 (widget-apply widget :menu-tag-get)
			 (widget-value widget)
			 t))))
	(widget-value-set widget answer)
	(widget-apply widget :notify widget event)
	(widget-setup))))

    (define-widget 'coding-system 'symbol
      "A MULE coding-system."
      :format "%{%t%}: %v"
      :tag "Coding system"
      :prompt-history 'widget-coding-system-prompt-value-history
      :prompt-value 'widget-coding-system-prompt-value
      :action 'widget-coding-system-action)

  (unless (get 'other 'widget-type)
    ;; The following definition is imported from wid-edit.el of Emacs 20.7.
    (define-widget 'other 'sexp
      "Matches any value, but doesn't let the user edit the value.
This is useful as last item in a `choice' widget.
You should use this widget type with a default value,
as in (other DEFAULT) or (other :tag \"NAME\" DEFAULT).
If the user selects this alternative, that specifies DEFAULT
as the value."
      :tag "Other"
      :format "%t%n"
      :value 'other)))

(eval-after-load "wid-edit" '(w3m-xmas-define-missing-widgets))

;;; Header line (emulating Emacs 21):
(defvar w3m-header-line-map (make-sparse-keymap))
(define-key w3m-header-line-map 'button2 'w3m-goto-url)

(defun w3m-setup-header-line ()
  "Setup header line (emulating Emacs 21)."
  (when (and w3m-use-header-line w3m-current-url
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
      (unless (eolp)
	(insert "\n")))))

;;; Gutter:
(defcustom w3m-xmas-show-current-title-in-buffer-tab
  (and (boundp 'gutter-buffers-tab-enabled)
       (symbol-value 'gutter-buffers-tab-enabled))
  "If non-nil, show the title string in the buffer tab.  It has no effect
if your XEmacs does not support the gutter items.  If you turn on this
option, it is recommended a bit that setting both the option
`w3m-pop-up-windows' and the option `w3m-pop-up-frames' to nil."
  :group 'w3m
  :type 'boolean
  :get (lambda (symbol)
	 (if (boundp 'gutter-buffers-tab-enabled)
	     (default-value symbol)))
  :set (lambda (symbol value)
	 (prog2
	     (or (boundp 'gutter-buffers-tab-enabled)
		 (setq value nil))
	     (set-default symbol value)
	   (if value
	       (add-hook 'w3m-display-hook 'w3m-xmas-update-tab-in-gutter)
	     (remove-hook 'w3m-display-hook 'w3m-xmas-update-tab-in-gutter))
	   (condition-case nil
	       (progn
		 (if value
		     (ad-enable-advice
		      'format-buffers-tab-line 'around
		      'w3m-xmas-show-current-title-in-buffer-tab)
		   (ad-disable-advice
		    'format-buffers-tab-line 'around
		    'w3m-xmas-show-current-title-in-buffer-tab))
		 (if (boundp 'gutter-buffers-tab-enabled)
		     (mapc #'update-tab-in-gutter (frame-list))))
	     (error)))))

(when (boundp 'gutter-buffers-tab-enabled)
  (defadvice format-buffers-tab-line
    (around w3m-xmas-show-current-title-in-buffer-tab (buffer) activate)
    "Advised by Emacs-W3M.
Show the current title string in the buffer tab.  Unfortunately,
existing XEmacs does not support showing non-ascii characters.  When a
title contains non-ascii characters, show a url name by default."
    (with-current-buffer buffer
      (if (and w3m-xmas-show-current-title-in-buffer-tab
	       (symbol-value 'gutter-buffers-tab-enabled)
	       (eq 'w3m-mode major-mode))
	  (let* ((len (specifier-instance
		       (symbol-value 'buffers-tab-default-buffer-line-length)))
		 (name (if (string-match "^[ -~]+$"
					 (symbol-value 'w3m-current-title))
			   (symbol-value 'w3m-current-title)
			 (directory-file-name
			  (if (string-match "^[^/:]+:/+"
					    (symbol-value 'w3m-current-url))
			      (substring (symbol-value 'w3m-current-url)
					 (match-end 0))
			    (symbol-value 'w3m-current-url)))))
		 (num (if (string-match ".*<\\(.+\\)>$" (buffer-name buffer))
			  (match-string 1 (buffer-name buffer))))
		 (lnum (length num)))
	    (setq ad-return-value
		  (if num
		      (if (and (> len 0)
			       (> (+ (length name) lnum) len))
			  (concat "[" num "]"
				  (substring name 0 (max 0 (- len lnum 5 )))
				  "...")
			(concat "[" num "]" name))
		    (if (and (> len 0)
			     (> (length name) len))
			(concat (substring name 0 (max 0 (- len 3))) "...")
		      name))))
	ad-do-it)))

  (if w3m-xmas-show-current-title-in-buffer-tab
      (ad-enable-advice 'format-buffers-tab-line 'around
			'w3m-xmas-show-current-title-in-buffer-tab)
    (ad-disable-advice 'format-buffers-tab-line 'around
		       'w3m-xmas-show-current-title-in-buffer-tab))

  (defun w3m-xmas-update-tab-in-gutter (&rest args)
    "Update the tab control in the gutter area."
    (update-tab-in-gutter (selected-frame)))

  (when (symbol-value 'gutter-buffers-tab-enabled)
    (add-hook 'w3m-display-hook 'w3m-xmas-update-tab-in-gutter)))

(provide 'w3m-xmas)

;;; w3m-xmas.el ends here

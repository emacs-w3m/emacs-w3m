;;; w3m-lnum.el --- Operations using link numbers

;; Copyright (C) 2004, 2005, 2006, 2007, 2009, 2010
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Modified by Andrey Kotlarski <m00naticus@gmail.com>
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file provides a minor mode to enable operations using link
;; numbers.

;;; Usage:

;; Install this file to an appropriate directory, and add these
;; expressions to your ~/.emacs-w3m.

;; (autoload 'w3m-linknum-follow "w3m-lnum"
;;   "Turn on link numbers, ask for one and execute appropriate action on it." t)
;; (autoload 'w3m-go-to-linknum "w3m-lnum"
;;   "Turn on link and form numbers and ask for one to go to." t)
;; (autoload 'w3m-linknum-toggle-image "w3m-lnum"
;;   "Turn on link numbers and toggle an image." t)
;; (autoload 'w3m-linknum-read-url "w3m-lnum"
;;   "Turn on link numbers and return PROMPT selected url.")
;; (add-hook 'w3m-mode-hook 'w3m-link-numbering-mode)

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'w3m)

(defface w3m-link-numbering
  '((((class color) (background light)) (:foreground "gray60"))
    (((class color) (background dark)) (:foreground "gray50")))
  "Face used to highlight link numbers."
  :group 'w3m-face)

(defface w3m-linknum-minibuffer-prompt
  '((((background dark)) :foreground "cyan")
    (((type pc)) :foreground "magenta")
    (t :foreground "medium blue"))
  "Face for w3m linknum minibuffer prompt."
  :group 'w3m-face)

;; backward-compatibility alias
(put 'w3m-link-numbering-face 'face-alias 'w3m-link-numbering)

(defcustom w3m-link-numbering-mode-hook nil
  "*Hook run after `w3m-link-numbering-mode' initialization."
  :group 'w3m
  :type 'hook)

(defvar w3m-link-numbering-mode-map
  (let ((keymap (make-sparse-keymap)))
    (substitute-key-definition 'w3m-view-this-url
			       'w3m-move-numbered-anchor
			       keymap w3m-mode-map)
    keymap)
  "Keymap used when `w3m-link-numbering-mode' is active.")

(defvar w3m-link-numbering-mode nil
  "Non-nil if w3m operations using link numbers are enabled.")
(make-variable-buffer-local 'w3m-link-numbering-mode)
(unless (assq 'w3m-link-numbering-mode minor-mode-map-alist)
  (push (cons 'w3m-link-numbering-mode w3m-link-numbering-mode-map)
	minor-mode-map-alist))

(defun w3m-linknum-remove-overlays (&optional arg)
  "Remove numbering and match overlays.
With ARG remove only temporary match"
  (if arg
      (catch 'done
	(dolist (overlay (overlays-in (point-min) (point-max)))
	  (when (overlay-get overlay 'w3m-linknum-match)
	    (delete-overlay overlay)
	    (throw 'done nil))))
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (if (or (overlay-get overlay 'w3m-link-numbering-overlay)
	      (overlay-get overlay 'w3m-linknum-match))
	  (delete-overlay overlay)))))

(defun w3m-link-numbering-mode (&optional arg)
  "Minor mode to enable operations using link numbers.
With prefix ARG 0 disable mode, with prefix ARG 4 index forms as well.
With prefix ARG 2 index only images."
  (add-hook 'w3m-display-functions 'w3m-link-numbering)
  (let (diff)
    (setq w3m-link-numbering-mode
	  (if arg
	      (unless (= (setq arg (prefix-numeric-value arg)) 0)
		(setq diff (not (eq arg w3m-link-numbering-mode)))
		arg)
	    (if (not w3m-link-numbering-mode) 1)))
    (if w3m-link-numbering-mode
	(progn
	  (if diff (w3m-linknum-remove-overlays))
	  (w3m-link-numbering w3m-link-numbering-mode)
	  (run-hooks 'w3m-link-numbering-mode-hook))
      (w3m-linknum-remove-overlays))))

(defun w3m-link-numbering (&rest args)
  "Make overlays that display link numbers.
With 2 as first ARGS argument index only images.
With 4 as first ARGS argument index form fields and buttons along links."
  (let ((arg (cond ((not args) 1)
		   ((numberp (car args)) (car args))
		   (t 1))))
    (if (= arg 0)
	(w3m-linknum-remove-overlays)
      (save-excursion
	(goto-char (point-min))
	(let ((i 0)
	      (next-func 'w3m-goto-next-anchor)
	      pos overlay num)
	  (if (eq arg 2)
	      (setq next-func (lambda () (if (w3m-goto-next-image)
					(point)))))
	  (catch 'already-numbered
	    (while (setq pos (funcall next-func))
	      (when (or (> arg 1)
			(get-char-property pos 'w3m-href-anchor))
		(when (get-char-property pos
					 'w3m-link-numbering-overlay)
		  (throw 'already-numbered nil))
		(setq overlay (make-overlay pos (1+ pos))
		      num (format "[%d]" (incf i)))
		(w3m-static-if (featurep 'xemacs)
		    (progn
		      (overlay-put overlay 'before-string num)
		      (set-glyph-face (extent-begin-glyph overlay)
				      'w3m-link-numbering))
		  (w3m-add-face-property 0 (length num)
					 'w3m-link-numbering num)
		  (overlay-put overlay 'before-string num)
		  (overlay-put overlay 'evaporate t))
		(overlay-put overlay
			     'w3m-link-numbering-overlay i)))))))))

(defun w3m-move-numbered-anchor (&optional arg)
  "Move the point to the specified anchor.
When no prefix argument is specified, call `w3m-view-this-url' instead
of moving cursor."
  (interactive "P")
  (if (and arg
	   (> (setq arg (prefix-numeric-value arg)) 0))
      (catch 'found
	(dolist (overlay (overlays-in (point-min) (point-max)))
	  (when (eq arg (overlay-get overlay 'w3m-link-numbering-overlay))
	    (goto-char (overlay-start overlay))
	    (push (w3m-anchor-sequence) w3m-goto-anchor-hist)
	    (w3m-horizontal-on-screen)
	    (throw 'found (w3m-print-this-url))))
	(error "Cannot find specified link: %d" arg))
    (w3m-view-this-url)))

(defun w3m-read-int-interactive (prompt fun &optional default)
  "Interactively read a valid integer from minubuffer with PROMPT.
Execute a one argument function FUN with every current valid integer.
Initial value is DEFAULT if specified or 0.
Use <return> to submit current value and <backspace> for correction."
  (let ((prompt (propertize prompt 'face
			    'w3m-linknum-minibuffer-prompt))
	(num (or default 0))
	(min-len (length prompt))
	ch)
    (let ((temp-prompt (format "%s%d" prompt num)))
      (while (not (eq (setq ch (read-event temp-prompt)) 'return))
	(cond ((and (eq ch 'backspace)
		    (> (length temp-prompt) min-len))
	       (setq num (/ num 10)
		     temp-prompt (format "%s%d" prompt num))
	       (funcall fun num))
	      ((and (numberp ch) (> ch 47) (< ch 58))
	       (setq num (+ (* num 10) (- ch 48))
		     temp-prompt (format "%s%d" prompt num))
	       (funcall fun num))))
      num)))

(defmacro w3m-with-linknum (type &rest body)
  "Within TYPE anchor numbering execute BODY.
Types are: 0 no numbering, 1 links, 2 images,
4 links, form fields and buttons.
Then restore previous numbering condition."
  `(let ((ty ,type)
	 (active (or w3m-link-numbering-mode 0)))
     (let ((diff (not (= active ty))))
       (when diff
	 (or (= ty 0) (w3m-linknum-remove-overlays))
	 (w3m-link-numbering ty))
       (unwind-protect (progn ,@body)
	 (if diff
	     (progn (or (= active 0) (w3m-linknum-remove-overlays))
		    (w3m-link-numbering active))
	   (w3m-linknum-remove-overlays t))))))

(defun w3m-highlight-numbered-anchor (arg)
  "Highlight specified by ARG number anchor."
  (catch 'done
    (let (found-prev marked-new)
      (dolist (overlay (overlays-in (point-min) (point-max)))
	(cond
	 ((overlay-get overlay 'w3m-linknum-match)
	  (delete-overlay overlay)
	  (setq found-prev t))
	 ((eq arg (overlay-get overlay 'w3m-link-numbering-overlay))
	  (let* ((start (overlay-start overlay))
		 (match-overlay
		  (make-overlay
		   start
		   (next-single-property-change
		    start
		    (cond ((get-text-property start 'w3m-href-anchor)
			   'w3m-href-anchor)
			  ((get-text-property start 'w3m-image)
			   'w3m-image)
			  (t 'w3m-action))))))
	    (overlay-put match-overlay 'w3m-linknum-match t)
	    (overlay-put match-overlay 'face 'match))
	  (setq marked-new t)))
	(and found-prev marked-new (throw 'done nil))))))

(defun w3m-get-anchor-info (&optional num)
  "Get info (url/action position [image]) of anchor numbered as NUM.
If NUM is not specified, use currently highlighted anchor."
  (macrolet
      ((get-match-info
	(condition)
	`(dolist (overlay (overlays-in (point-min) (point-max)))
	   (if ,condition
	       (let* ((pos (overlay-start overlay))
		      (href (get-text-property pos 'w3m-href-anchor)))
		 (throw
		  'found
		  (if href (list href pos
				 (get-text-property pos 'w3m-image))
		    (list (get-text-property pos 'w3m-action)
			  pos))))))))
    (catch 'found
      (if num (get-match-info
	       (eq num (overlay-get
			overlay 'w3m-link-numbering-overlay)))
	(get-match-info (overlay-get overlay 'w3m-linknum-match))))))

;;;###autoload
(defun w3m-go-to-linknum (arg)
  "Turn on link and form numbers and ask for one to go to.
With prefix ARG don't highlight current link."
  (interactive "P")
  (w3m-with-linknum
   4
   (let (info)
     (if arg
	 (setq info (w3m-get-anchor-info
		     (w3m-read-number "Anchor number: ")))
       (w3m-read-int-interactive "Anchor number: "
				 'w3m-highlight-numbered-anchor)
       (setq info (w3m-get-anchor-info)))
     (if info
	 (goto-char (cadr info))
       (error "No valid anchor selected")))))

(defun w3m-linknum-get-action (&optional prompt type)
  "Turn on link numbers and return list of url or action, position
and image url if such of  PROMPT selected anchor.
TYPE sets types of anchors to be numbered, if nil or 4, number urls,
form fields and buttons. 1 - only links, 2 - only images.
Highlight every intermediate result anchor."
  (w3m-with-linknum
   (or type 4) (w3m-read-int-interactive (or prompt "Anchor number: ")
					 'w3m-highlight-numbered-anchor)
   (w3m-get-anchor-info)))

;;;###autoload
(defun w3m-linknum-follow (arg)
  "Turn on link numbers, ask for one and execute appropriate action on it.
When link - visit it, when button - press, when input - activate it.
With prefix ARG visit link in new session or move over field/button
before activate/press."
  (interactive "P")
  (let ((info (w3m-linknum-get-action
	       (concat "Follow " (if arg "in new session ")
		       "(select link): "))))
    (if info
	(let ((action (car info)))
	  (cond ((stringp action)	; url
		 (if arg (w3m-goto-url-new-session action)
		   (goto-char (cadr info))
		   (w3m-goto-url action)))
		((eq (car action) 'w3m-form-submit) ; button
		 (if arg (goto-char (cadr info)))
		 (widget-button-press (cadr info) action))
		(t (if arg		; form field
		       (progn
			 (goto-char (cadr info))
			 (let ((w3m-form-new-session t)
			       (w3m-form-download nil))
			   (eval action)))
		     (save-excursion
		       (goto-char (cadr info))
		       (let ((w3m-form-new-session nil)
			     (w3m-form-download nil))
			 (eval action)))))))
      (error "No valid link selected"))))

;;;###autoload
(defun w3m-linknum-read-url (&optional prompt)
  "Turn on link numbers and return PROMPT selected url.
Highlight each intermediate result anchor."
  (let ((link (w3m-linknum-get-action (or prompt "Link number: ") 1)))
    (and link (stringp (setq link (car link)))
	 link)))

;;;###autoload
(defun w3m-linknum-toggle-image (&optional arg)
  "Turn on link numbers and toggle an image.
With prefix ARG open in new session url behind image if such."
  (interactive "P")
  (let ((im (w3m-linknum-get-action
	     (if arg
		 "Open image url in new session: "
	       "Toggle image: ")
	     2)))
    (if im
	(if (and arg (car im))
	    (w3m-goto-url-new-session (car im))
	  (save-excursion (goto-char (cadr im))
			  (w3m-toggle-inline-image)))
      (error "No image selected"))))

(provide 'w3m-lnum)

;;; w3m-lnum.el ends here

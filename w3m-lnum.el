;;; w3m-lnum.el --- Operations using link numbers

;; Copyright (C) 2004, 2005, 2006, 2007, 2009, 2010
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
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

;; (autoload 'w3m-link-numbering-mode "w3m-lnum" nil t)
;; (autoload 'w3m-linknum-follow "w3m-lnum" nil t)
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
      (dolist (overlay (overlays-in (point-min) (point-max)))
	(if (overlay-get overlay 'w3m-linknum-match)
	    (delete-overlay overlay)))
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (if (or (overlay-get overlay 'w3m-link-numbering-overlay)
	      (overlay-get overlay 'w3m-linknum-match))
	  (delete-overlay overlay)))))

;;;###autoload
(defun w3m-link-numbering-mode (&optional arg)
  "Minor mode to enable operations using link numbers.
With prefix ARG 0 disable mode, with prefix ARG 4 index forms as well."
  (interactive "P")
  (add-hook 'w3m-display-functions 'w3m-link-numbering)
  (let (diff)
    (setq w3m-link-numbering-mode
	  (if arg
	      (when (not (= (setq arg (prefix-numeric-value arg)) 0))
		(setq diff (not (eq arg w3m-link-numbering-mode)))
		arg)
	    (if (not w3m-link-numbering-mode) 1)))
    (if w3m-link-numbering-mode
	(progn
	  (if diff (w3m-linknum-remove-overlays))
	  (if (= w3m-link-numbering-mode 4)
	      (w3m-link-numbering t)
	    (w3m-link-numbering))
	  (run-hooks 'w3m-link-numbering-mode-hook))
      (w3m-linknum-remove-overlays))))

(defun w3m-link-numbering (&rest args)
  "Make overlays that display link numbers.
With ARGS index forms as well."
  (when w3m-link-numbering-mode
    (save-excursion
      (goto-char (point-min))
      (let ((i 0)
	    pos overlay num)
	(catch 'already-numbered
	  (while (setq pos (w3m-goto-next-anchor))
	    (when (or args (get-char-property pos 'w3m-href-anchor))
	      (when (get-char-property pos 'w3m-link-numbering-overlay)
		(throw 'already-numbered nil))
	      (setq overlay (make-overlay pos (1+ pos))
		    num (format "[%d]" (incf i)))
	      (w3m-static-if (featurep 'xemacs)
		  (progn
		    (overlay-put overlay 'before-string num)
		    (set-glyph-face (extent-begin-glyph overlay)
				    'w3m-link-numbering))
		(w3m-add-face-property 0 (length num) 'w3m-link-numbering num)
		(overlay-put overlay 'before-string num)
		(overlay-put overlay 'evaporate t))
	      (overlay-put overlay 'w3m-link-numbering-overlay i))))))))

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
  (let ((prompt (propertize prompt 'face 'minibuffer-prompt))
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
Then restore previous numbering condition."
  `(let ((ty ,type)
	 (active (or w3m-link-numbering-mode 0)))
     (let ((diff (not (= active ty))))
       (when diff
	 (or (= ty 0) (w3m-link-numbering-mode 0))
	 (w3m-link-numbering-mode ty))
       (unwind-protect
	   (progn ,@body)
	 (if diff
	     (progn
	       (or (= active 0) (w3m-link-numbering-mode 0))
	       (w3m-link-numbering-mode active))
	   (w3m-linknum-remove-overlays t))))))

(defun w3m-highlight-numbered-anchor (arg)
  "Highlight specified by ARG number anchor."
  (catch 'done
    (let (found-prev marked-new)
      (dolist (overlay (overlays-in (point-min) (point-max)))
	(cond
	 ((and found-prev marked-new)
	  (throw 'done nil))
	 ((overlay-get overlay 'w3m-linknum-match)
	  (delete-overlay overlay)
	  (setq found-prev t))
	 ((eq arg (overlay-get overlay 'w3m-link-numbering-overlay))
	  (let* ((start (overlay-start overlay))
		 (end (1+ start)))
	    (let ((prop 'w3m-href-anchor)
		  (anchor (get-text-property start 'w3m-href-anchor))
		  (comparator 'equal))
	      (or anchor
		  (setq prop 'w3m-action
			anchor (get-text-property start 'w3m-action)
			comparator 'eq))
	      (while (funcall comparator anchor
			      (get-text-property end prop))
		(setq end (1+ end))))
	    (let ((match-overlay (make-overlay start end)))
	      (overlay-put match-overlay 'w3m-linknum-match t)
	      (overlay-put match-overlay 'face 'match)))
	  (setq marked-new t)))))))

(defun w3m-get-numbered-url (&optional num)
  "Get url of anchor numbered as NUM.
If NUM is not specified, find currently highlighted anchor."
  (catch 'found
    (if num
	(dolist (overlay (overlays-in (point-min) (point-max)))
	  (if (eq num (overlay-get overlay
				   'w3m-link-numbering-overlay))
	      (let ((result (get-text-property (overlay-start overlay)
					       'w3m-href-anchor)))
		(throw
		 'found
		 (let ((pos (overlay-start overlay)))
		   (cons (or result
			     (get-text-property pos 'w3m-action))
			 pos))))))
      (dolist (overlay (overlays-in (point-min) (point-max)))
	(if (overlay-get overlay 'w3m-linknum-match)
	    (let ((result (get-text-property (overlay-start overlay)
					     'w3m-href-anchor)))
	      (throw
	       'found
	       (let ((pos (overlay-start overlay)))
		 (cons (or result
			   (get-text-property pos 'w3m-action))
		       pos)))))))))

;;;###autoload
(defun w3m-go-to-linknum (arg)
  "Turn on link and form numbers and ask for one to go to.
With prefix ARG don't highlight current link."
  (interactive "P")
  (w3m-with-linknum
   4
   (w3m-move-numbered-anchor
    (if arg
	(w3m-read-number "Anchor number: ")
      (w3m-read-int-interactive "Anchor number: "
				'w3m-highlight-numbered-anchor)))))

(defun w3m-linknum-get-action (&optional prompt)
  "Turn on link numbers and return cons of url or action and position
of  PROMPT selected anchor.
Highlight every intermediate result anchor."
  (w3m-with-linknum
   4 (w3m-read-int-interactive (or prompt "Anchor number: ")
			       'w3m-highlight-numbered-anchor)
   (w3m-get-numbered-url)))

;;;###autoload
(defun w3m-linknum-follow (arg)
  "Turn on link numbers, ask for one and execute appropriate action on it.
When link - visit it, when button - press, when input - activate it.
With prefix ARG visit link in new session."
  (interactive "P")
  (let ((link (w3m-linknum-get-action
	       (concat "Follow " (if arg "in new session ")
		       "(select link): "))))
    (if (consp link)
	(cond ((stringp (car link))
	       (if arg (w3m-goto-url-new-session (car link))
		 (goto-char (cdr link))
		 (w3m-goto-url (car link))))
	      ((eq (caar link) 'w3m-form-submit)
	       (widget-button-press (cdr link) (car link)))
	      (t (goto-char (cdr link))
		 (let ((w3m-form-new-session arg)
		       (w3m-form-download nil))
		   (eval (car link)))))
      (error "No valid link selected"))))

;;;###autoload
(defun w3m-linknum-read-url (&optional prompt)
  "Turn on link numbers and return PROMPT selected url.
Highlight each intermediate result anchor."
  (w3m-with-linknum
   1 (w3m-read-int-interactive (or prompt "Anchor number: ")
			       'w3m-highlight-numbered-anchor)
   (let ((link (w3m-get-numbered-url)))
     (and (consp link) (stringp (car link))
	  (car link)))))

(provide 'w3m-lnum)

;;; w3m-lnum.el ends here

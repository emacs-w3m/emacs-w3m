;;; w3m-link-numbering.el --- Operations using link numbers

;; Copyright (C) 2004 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

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
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file provides a minor mode to enable operations using link
;; numbers.

;;; Usage:

;; Install this file to an appropriate directory, and add these
;; expressions to your ~/.emacs-w3m.

;;      (autoload 'w3m-link-numbering "w3m-link-numbering" nil t)
;;	(add-hook 'w3m-mode-hook 'w3m-link-numbering-mode)

;;; Code:

(require 'w3m)

(defcustom w3m-link-numbering-mode-hook nil
  "*Hook run after `w3m-link-numbering-mode' initialization."
  :group 'w3m
  :type 'hook)

(defvar w3m-link-numbering-mode-map
  (let ((keymap (make-sparse-keymap)))
    (substitute-key-definition 'w3m-view-this-url
			       'w3m-view-numbered-link
			       keymap w3m-mode-map)
    keymap)
  "Keymap used when `w3m-link-numbering-mode' is active.")

(defvar w3m-link-numbering-mode nil
  "Non-nil if w3m link numberring mode is enabled.")
(make-variable-buffer-local 'w3m-link-numbering-mode)
(unless (assq 'w3m-link-numbering-mode minor-mode-map-alist)
  (push (cons 'w3m-link-numbering-mode w3m-link-numbering-mode-map)
	minor-mode-map-alist))

;;;###autoload
(defun w3m-link-numbering-mode (&optional arg)
  "Minor mode to enable operations using link numbers."
  (interactive "P")
  (add-hook 'w3m-display-functions 'w3m-link-numbering)
  (when (prog1 (setq w3m-link-numbering-mode
		     (if arg
			 (> (prefix-numeric-value arg) 0)
		       (not w3m-link-numbering-mode)))
	  (w3m-link-numbering))
    (run-hooks 'w3m-link-numbering-mode-hook)))

(defun w3m-link-numbering (&rest args)
  "Make overlays that display link numbers."
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (overlay-get overlay 'w3m-link-numbering-overlay)
      (delete-overlay overlay)))
  (when w3m-link-numbering-mode
    (save-excursion
      (goto-char (point-min))
      (let ((i 0))
	(while (w3m-goto-next-anchor)
	  (when (w3m-anchor)
	    (let ((overlay (make-overlay (point) (1+ (point)))))
	      (overlay-put overlay 'w3m-link-numbering-overlay (incf i))
	      (overlay-put overlay 'before-string (format "[%d]" i)))))))))

(defun w3m-view-numbered-link (&optional arg)
  "Display the page pointed to by the specified link."
  (interactive "P")
  (if (and arg
	   (> (setq arg (prefix-numeric-value arg)) 0))
      (catch 'found
	(dolist (overlay (overlays-in (point-min) (point-max)))
	  (when (eq arg (overlay-get overlay 'w3m-link-numbering-overlay))
	    (goto-char (overlay-start overlay))
	    (throw 'found (w3m-view-this-url))))
	(error "Cannot found your specified link: %d" arg))
    (w3m-view-this-url)))

(provide 'w3m-link-numbering)

;;; w3m-link-numbering.el ends here

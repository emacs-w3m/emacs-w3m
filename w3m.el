;;; -*- mode: Emacs-Lisp; coding: euc-japan -*-
;; $Id$

;; Copyright (C) 2000 TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>

;; Author: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;; Keywords: w3m, WWW, hypermedia

;; w3m.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; w3m.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with w3m.el; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


;;; Commentary:

;; w3m.el is the interface program of w3m on Emacs.  For more detail
;; about w3m, see:
;;
;;    http://ei5nazha.yz.yamagata-u.ac.jp/~aito/w3m/


;;; Code:

(defgroup w3m nil
  "w3m - the web browser of choice."
  :group 'hypermedia)

(defgroup w3m-face nil
  "Faces for w3m."
  :group 'w3m
  :prefix "w3m-")

(defcustom w3m-command "w3m"
  "*Name of the executable file of w3m."
  :group 'w3m
  :type 'string)

(defcustom w3m-command-arguments '("-e" "-halfdump" "-cols" col url)
  "*Arguments of w3m."
  :group 'w3m
  :type '(repeat (restricted-sexp :match-alternatives (stringp 'col 'url))))

(defcustom w3m-viewer-command "xv"
  "*Name of the viewer."
  :group 'w3m
  :type 'string)

(defcustom w3m-viewer-command-arguments '(file)
  "Arguments of viewer."
  :group 'w3m
  :type '(repeat (restricted-sexp :match-alternatives (stringp 'file))))

(defcustom w3m-browser-command "netscape"
  "*Name of the browser."
  :group 'w3m
  :type 'string)

(defcustom w3m-browser-command-arguments '(url)
  "Arguments of browser."
  :group 'w3m
  :type '(repeat (restricted-sexp :match-alternatives (stringp 'url))))

(defcustom w3m-coding-system (if (boundp 'MULE) '*euc-japan* 'euc-japan)
  "*Coding system for w3m."
  :group 'w3m
  :type 'symbol)

(defcustom w3m-bookmark-file (expand-file-name "~/.w3m/bookmark.html")
  "*Bookmark file of w3m."
  :group 'w3m
  :type 'file)

(defcustom w3m-fill-column (- (frame-width) 4)
  "*Fill column of w3m."
  :group 'w3m
  :type 'integer)

(defface w3m-anchor-face
  '((((class color) (background light)) (:foreground "red" :underline t))
    (((class color) (background dark)) (:foreground "blue" :underline t))
    (t (:underline t)))
  "*Face to fontify anchors."
  :group 'w3m-face)

(defface w3m-image-face
  '((((class color) (background light)) (:foreground "ForestGreen"))
    (((class color) (background dark)) (:foreground "PaleGreen"))
    (t (:underline t)))
  "*Face to fontify image alternate strings."
  :group 'w3m-face)

(defcustom w3m-hook nil
  "*Hook run before w3m called."
  :group 'w3m
  :type 'hook)

(defcustom w3m-mode-hook nil
  "*Hook run before w3m-mode called."
  :group 'w3m
  :type 'hook)

(defcustom w3m-fontify-before-hook nil
  "*Hook run before w3m-fontify called."
  :group 'w3m
  :type 'hook)

(defcustom w3m-fontify-after-hook nil
  "*Hook run after w3m-fontify called."
  :group 'w3m
  :type 'hook)


(defun w3m-fontify ()
  "Fontify this buffer."
  (let ((case-fold-search t))
    (run-hooks 'w3m-fontify-before-hook)
    ;; Decode escaped characters.
    (goto-char (point-min))
    (while (search-forward "&nbsp;" nil t)
      (delete-region (match-beginning 0) (match-end 0))
      (insert " "))
    ;; Fontify anchors.
    (goto-char (point-min))
    (while (re-search-forward
	    "<a\\( hseq=\"[-0-9]+\"\\)?\\( href=\"\\([^\"]*\\)\"\\)?\\( name=\"\\([^\"]*\\)\"\\)?>" nil t)
      (let ((url (match-string 3))
	    (tag (match-string 5))
	    (start (match-beginning 0))
	    (end))
	(delete-region start (match-end 0))
	(when (search-forward "</a>" nil t)
	  (delete-region (setq end (match-beginning 0)) (match-end 0))
	  (when url
	    (put-text-property start end 'face 'w3m-anchor-face)
	    (put-text-property start end 'w3m-href-anchor url))
	  (when tag
	    (put-text-property start end 'w3m-name-anchor tag)))))
    ;; Fontify image alternate strings.
    (goto-char (point-min))
    (while (re-search-forward "<img_alt src=\"\\([^\"]*\\)\">" nil t)
      (let ((src (match-string 1))
	    (start (match-beginning 0))
	    (end))
	(delete-region start (match-end 0))
	(when (search-forward "</img_alt>" nil t)
	  (delete-region (setq end (match-beginning 0)) (match-end 0))
	  (put-text-property start end 'face 'w3m-image-face)
	  (put-text-property start end 'w3m-image src))))
    ;; Fontify bold characters.
    (goto-char (point-min))
    (while (search-forward "<b>" nil t)
      (let ((start (match-beginning 0)))
	(delete-region start (match-end 0))
	(when (search-forward "</b>" nil t)
	  (delete-region (match-beginning 0) (match-end 0))
	  (put-text-property start (match-beginning 0) 'face 'bold))))
    ;; Remove other markups.
    (goto-char (point-min))
    (while (re-search-forward "<[^>]*>" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (run-hooks 'w3m-fontify-after-hook)))


(defvar w3m-input-url-history nil)

(defun w3m-input-url (&optional prompt default)
  "Read a URL from the minibuffer, prompting with string PROMPT."
  (read-from-minibuffer (or prompt "URL: ") nil nil nil 'w3m-input-url-history default))


(defvar w3m-url nil "URL of this buffer.")
(defvar w3m-title nil "Title of this buffer.")

(defun w3m-exec (url &optional buffer)
  (save-excursion
    (if buffer (set-buffer buffer))
    (delete-region (point-min) (point-max))
    (let ((coding-system-for-read w3m-coding-system)
	  (default-process-coding-system (cons w3m-coding-system w3m-coding-system)))
      (apply 'call-process w3m-command nil t nil
	     (mapcar (lambda (arg)
		       (if (eq arg 'col)
			   (format "%d" w3m-fill-column)
			 (eval arg)))
		     w3m-command-arguments)))
    ;; Setting buffer local variables.
    (set (make-local-variable 'w3m-url) url)
    (goto-char (point-min))
    (let (start title)
      (and (search-forward "<title>" nil t)
	   (prog1 (setq start (match-beginning 0))
	     (delete-region start (match-end 0)))
	   (search-forward "</title>" nil t)
	   (setq title (buffer-substring start (match-beginning 0)))
	   (delete-region start (match-end 0)))
      (set (make-local-variable 'w3m-title) title))))


(defun w3m-expand-url (url base)
  "Convert URL to absolute, and canonicalize it."
  (cond
   ;; URL has absolute spec.
   ((string-match "^[^:]+://" url)
    url)
   ((string-match "^/" url)
    (if (string-match "^\\([^:]+://[^/]*\\)/" base)
	(concat (match-string 1 base) url)
      url))
   (t ;; URL is relative on BASE.
    (let ((server ""))
      (if (string-match "^\\([^:]+://[^/]*\\)/" base)
	  (setq server (match-string 1 base)
		base (substring base (match-end 1))))
      (if (string-match "[^/]*$" base)
	  (setq base (substring base 0 (match-beginning 0))))
      (concat server (expand-file-name url base))))))
    

(defun w3m-view-this-url ()
  "*View the URL of the link under point."
  (interactive)
  (let ((url (get-text-property (point) 'w3m-href-anchor)))
    (if url (w3m (w3m-expand-url url w3m-url)))))


(defun w3m-view-image ()
  "*View the image under point."
  (interactive)
  (let ((file (get-text-property (point) 'w3m-image)))
    (if file
	(let ((buffer (get-buffer-create " *w3m-view*")))
	  (apply 'start-process
		 "w3m-view"
		 buffer
		 w3m-viewer-command
		 (mapcar 'eval w3m-viewer-command-arguments))))))


(defun w3m-save-image ()
  "*Save the image under point to a file."
  (interactive)
  (let ((file (get-text-property (point) 'w3m-image)))
    (message "Please implement this function !!")))


(defun w3m-view-current-url-with-external-browser ()
  "*View this URL."
  (interactive)
  (let ((buffer (get-buffer-create " *w3m-view*")))
    (apply 'start-process
	   "w3m-external-browser"
	   buffer
	   w3m-viewer-command
	   (mapcar (lambda (x)
		     (if (eq x 'url) w3m-url x))
		   w3m-viewer-command-arguments))))


(defun w3m-download-this-url ()
  "*Download the URL of the link under point to a file."
  (interactive)
  (message "Please implement this function !!"))


(defun w3m-print-this-url ()
  "*Print the URL of the link under point."
  (interactive)
  (message (or (get-text-property (point) 'w3m-href-anchor) "Not found.")))


(defun w3m-next-anchor (&optional arg)
  "*Move cursor to the next anchor."
  (interactive "P")
  (unless arg (setq arg 1))
  (if (< arg 0)
      ;; If ARG is negative.
      (w3m-previous-anchor (- arg))
    (when (get-text-property (point) 'w3m-href-anchor)
      (goto-char (next-single-property-change (point) 'w3m-href-anchor)))
    (while (and
	    (> arg 0)
	    (setq pos (next-single-property-change (point) 'w3m-href-anchor)))
      (goto-char pos)
      (unless (zerop (setq arg (1- arg)))
	(goto-char (next-single-property-change (point) 'w3m-href-anchor))))))


(defun w3m-previous-anchor (&optional arg)
  "Move cursor to the previous anchor."
  (interactive "P")
  (unless arg (setq arg 1))
  (if (< arg 0)
      ;; If ARG is negative.
      (w3m-next-anchor (- arg))
    (when (get-text-property (point) 'w3m-href-anchor)
      (goto-char (previous-single-property-change (1+ (point)) 'w3m-href-anchor)))
    (while (and
	    (> arg 0)
	    (setq pos (previous-single-property-change (point) 'w3m-href-anchor)))
      (goto-char (previous-single-property-change pos 'w3m-href-anchor))
      (setq arg (1- arg)))))


(defun w3m-view-bookmark ()
  (interactive)
  (if (file-readable-p w3m-bookmark-file)
      (w3m w3m-bookmark-file)))


(defvar w3m-mode-map nil)
(unless w3m-mode-map
  (setq w3m-mode-map (make-keymap))
  (define-key w3m-mode-map " " 'scroll-up)
  (define-key w3m-mode-map "b" 'scroll-down)
  (define-key w3m-mode-map [backspace] 'scroll-down)
  (define-key w3m-mode-map [delete] 'scroll-down)
  (define-key w3m-mode-map "h" 'backward-char)
  (define-key w3m-mode-map "j" 'next-line)
  (define-key w3m-mode-map "k" 'previous-line)
  (define-key w3m-mode-map "l" 'forward-char)
  (define-key w3m-mode-map "J" (lambda () (interactive) (scroll-up 1)))
  (define-key w3m-mode-map "K" (lambda () (interactive) (scroll-up -1)))
  (define-key w3m-mode-map "G" 'goto-line)
  (define-key w3m-mode-map "\C-?" 'scroll-down)
  (define-key w3m-mode-map "\t" 'w3m-next-anchor)
  (define-key w3m-mode-map "\M-\t" 'w3m-previous-anchor)
  (define-key w3m-mode-map "\C-m" 'w3m-view-this-url)
  (define-key w3m-mode-map "d" 'w3m-download-this-url)
  (define-key w3m-mode-map "u" 'w3m-print-this-url)
  (define-key w3m-mode-map "I" 'w3m-view-image)
  (define-key w3m-mode-map "\M-I" 'w3m-save-image)
  (define-key w3m-mode-map "c" (lambda () (interactive) (message w3m-url)))
  (define-key w3m-mode-map "M" 'w3m-view-current-url-with-external-browser)
  (define-key w3m-mode-map "g" 'w3m)
  (define-key w3m-mode-map "U" 'w3m)
  (define-key w3m-mode-map "V" 'w3m)
  (define-key w3m-mode-map "v" 'w3m-view-bookmark)
  (define-key w3m-mode-map "q" (lambda ()
				 (interactive)
				 (if (y-or-n-p "Do you want to exit w3m? ")
				     (kill-buffer (current-buffer)))))
  (define-key w3m-mode-map "Q" (lambda ()
				 (interactive)
				 (kill-buffer (current-buffer))))
  )


(defun w3m-mode ()
  "Major mode to browsing w3m buffer."
  (kill-all-local-variables)
  (setq major-mode 'w3m-mode
	mode-name   "w3m")
  (use-local-map w3m-mode-map)
  (run-hooks 'w3m-mode-hook))


(defun w3m (url)
  (interactive (list (w3m-input-url)))
  (save-current-buffer
    (set-buffer (get-buffer-create "*w3m*"))
    (w3m-mode)
    (let ((buffer-read-only nil))
      (w3m-exec url (current-buffer))
      (w3m-fontify))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))
    (run-hooks 'w3m-hook)))


(provide 'w3m)
;;; w3m.el ends here.

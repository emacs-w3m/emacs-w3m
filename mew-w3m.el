;; mew-w3m.el --- View Text/Html content with w3m in Mew

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: Shun-ichi GOTO  <gotoh@taiyo.co.jp>,
;;         Hideyuki SHIRAI <shirai@meadowy.org>
;; Created: Wed Feb 28 03:31:00 2001
;; Version: $Revision$
;; Keywords: Mew, mail, w3m, WWW, hypermedia

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package is for viewing formatted (rendered) Text/Html content
;; in Mew's message buffer.

;;; Instalation:

;; (1) Simply load this file and add followings in your ~/.mew file.
;;
;; (require 'mew-w3m)
;; (setq mew-prog-html '(mew-mime-text/html-w3m nil nil))
;;  or
;; (require 'mew-w3m)
;; (setq mew-prog-text/html 'mew-mime-text/html-w3m)
;;;; (setq mew-prog-text/html-ext 'mew-mime-text/html-w3m)
;;
;; (2) And you can use keymap of w3m-mode as mew-w3m-minor-mode.
;; To activate this feaeture, add followings also:
;;
;; (setq mew-use-w3m-minor-mode t)
;; (add-hook 'mew-message-hook 'mew-w3m-minor-mode-setter)
;;
;; (3) If you use mew-1.95b118 or later on which Emacs-21 or XEmacs,
;; can display the images in the Text/Html message.
;; To activate this feaeture, add following in your ~/.mew file.
;;
;; (define-key mew-summary-mode-map "T" 'mew-w3m-view-inline-image)
;;
;; Press "T": Display the images included its message only.
;; Press "C-uT": Display the all images included its Text/Html part."

;;; Usage:

;; There's nothing special. Browse messages in usual way.
;; On viewing Text/Html file, rendered text is appeared in message
;; buffer instead of usual "HTML" banner.
;; C-c C-e operation is also allowed to view with external browser.
;;
;; If mew-use-w3m-minor-mode is t, key operations of w3m-mode is
;; allowed (as minor-mode-map) and jump links in message buffer.
;; NOTE: This feature is not complete. You may confuse.
;;
;;

;;; Code:

(require 'mew)
(require 'w3m)
(eval-when-compile (require 'cl))

;;; initializer for mew
(defgroup mew-w3m nil
  "mew-w3m - Inline HTML rendering extension of Mew"
  :group 'w3m)

(defcustom mew-use-w3m-minor-mode nil
  "*Use w3m minor mode in message buffer.
When viewing Text/Html contents rendering with w3m, use `w3m-minor-mode'
and its keymap in message buffer."
  :group 'mew-w3m
  :type 'boolean)

(defcustom mew-w3m-auto-insert-image nil
  "*If non-nil, the images inserts automatic in Multipart/Related message.
This variable effected only XEmacs or Emacs 21."
  :group 'mew-w3m
  :type 'boolean)

;; these are defined here.
;; It's not reasonable to merge into w3m.el, I think
(defvar mew-w3m-minor-mode nil)
(defconst mew-w3m-safe-url-regexp "\\`cid:")

(make-variable-buffer-local 'mew-w3m-minor-mode)
(add-to-list 'minor-mode-alist '(mew-w3m-minor-mode " w3m"))
(add-to-list 'minor-mode-map-alist (cons 'mew-w3m-minor-mode w3m-mode-map))

(defun mew-w3m-minor-mode-setter ()
  "Check message buffer and activate mew-w3m-minor-mode."
  (setq mew-w3m-minor-mode (and (get-text-property (point-min) 'w3m)
				mew-use-w3m-minor-mode)))


(defun mew-w3m-view-inline-image (&optional allimage)
  "Display the images of Text/Html part.
\\<mew-summary-mode-map>
'\\[mew-w3m-view-inline-image]'	Display the images included its message only.
'\\[universal-argument]\\[mew-w3m-view-inline-image]'	Display the all images included its Text/Html part."
  (interactive "P")
  (mew-summary-msg-or-part
   (if allimage
       (let ((mew-w3m-auto-insert-image t)
	     (mew-w3m-safe-url-regexp nil))
	 (mew-summary-display 'force))
     (let ((mew-w3m-auto-insert-image t))
       (mew-summary-display 'force)))))


;; processing Text/Html contents with w3m.
(defun mew-mime-text/html-w3m (&rest args)
  "View Text/Html contents with w3m rendering output."
  (let ((w3m-current-image-status mew-w3m-auto-insert-image)
	(w3m-safe-url-regexp mew-w3m-safe-url-regexp)
	w3m-force-redisplay	;; don't redraw
	charset wcs xref
	cache begin end params execute)
    (if (= (length args) 2)
	;; Mew-2
	(setq begin (nth 0 args) end (nth 1 args))
      ;; Old Mew
      (setq cache (nth 0 args))
      (setq begin (nth 1 args))
      (setq end (nth 2 args))
      (setq params (nth 3 args))
      (setq execute (nth 4 args)))
    (if (and cache (or execute (<= end begin)))
	;; 'C-cC-e' + Old Mew
	(apply 'mew-mime-text/html (list cache begin end params execute))
      (save-excursion
	;; search Xref: Header in SHIMBUN article
	(when cache (set-buffer cache))
	(goto-char (point-min))
	(when (re-search-forward mew-eoh nil t)
	  (let ((eoh (point))
		(case-fold-search t))
	    (goto-char (point-min))
	    (when (and (re-search-forward "^X-Shimbun-Id: " eoh t)
		       (goto-char (point-min))
		       (re-search-forward "^Xref: \\(.+\\)\n" eoh t))
	      (setq xref (mew-match 1))))))
      (mew-elet
       (cond
	((and (null cache) (eq w3m-type 'w3m-m17n))
	 ;; Mew-2 + w3m-m17n.
	 ;; Coding-system and charset are decided by Mew.
	 (let ((w3m-input-coding-system w3m-input-coding-system)
	       (w3m-output-coding-system w3m-output-coding-system)
	       (w3m-halfdump-command-arguments w3m-halfdump-command-arguments))
	   (when (setq charset (mew-charset-guess-region begin end))
	     (setq wcs (mew-charset-to-cs charset)))
	   (when (and charset wcs (mew-coding-system-p wcs))
	     ;; guess correctly and not us-ascii
	     (setq w3m-input-coding-system wcs)
	     (setq w3m-output-coding-system wcs)
	     (setq w3m-halfdump-command-arguments
		   (list "-halfdump"
			 "-I" charset "-O" charset
			 "-o" "ext_halfdump=1"
			 "-o" "pre_conv=1"
			 "-o" "strict_iso2022=0")))
	   (w3m-region begin end xref)))
	((null cache)	;; Mew-2 + w3m, w3mmee
	 (w3m-region begin end xref))
	(t		;; Old Mew
	 (setq charset (or (mew-syntax-get-param params "charset")
			   (save-excursion
			     (set-buffer cache)
			     (mew-charset-guess-region begin end))))
	 (if charset
	     (setq wcs (mew-charset-to-cs charset))
	   (setq wcs mew-cs-text-for-write))
	 (mew-frwlet
	  mew-cs-dummy wcs
	  (w3m-region (point)
		      (progn (insert-buffer-substring cache begin end)
			     (point))
		      xref))))
       (put-text-property (point-min) (1+ (point-min)) 'w3m t)))))

(defvar w3m-mew-support-cid (fboundp 'mew-syntax-get-entry-by-cid))

(defun mew-w3m-cid-retrieve (url &rest args)
  (save-excursion
    (when (and w3m-mew-support-cid
	       (string-match "^cid:\\(.+\\)" url))
      (setq url (match-string 1 url))
      (let ((fld (mew-current-get-fld (mew-frame-id))))
	(set-buffer fld)
	(let* ((msg (mew-current-get-msg (mew-frame-id)))
	       (cache (mew-cache-hit fld msg 'must-hit))
	       (syntax (mew-cache-decode-syntax cache))
	       cidstx beg end)
	  (setq cidstx (mew-syntax-get-entry-by-cid syntax url))
	  (when cidstx
	    (setq beg (mew-syntax-get-begin cidstx))
	    (setq end (mew-syntax-get-end cidstx))
	    (w3m-with-work-buffer
	      (delete-region (point-min) (point-max))
	      (set-buffer-multibyte nil)
	      (insert-buffer-substring cache beg end))
	    (car (mew-syntax-get-ct cidstx))))))))

(push (cons 'mew-message-mode 'mew-w3m-cid-retrieve)
      w3m-cid-retrieve-function-alist)

;;;
(provide 'mew-w3m)

;; mew-w3m.el ends here

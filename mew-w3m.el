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

;;; Installation:

;; (1) Simply load this file and add followings in your ~/.mew file.
;;
;; (require 'mew-w3m)
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
;;
;; (4) You can use emacs-w3m to fetch and/or browse
;; `external-body with URL access'. To activate this feaeture,
;; add followings also:
;;
;; (setq mew-ext-url-alist
;;      '(("^application/" "Fetch by emacs-w3m" mew-w3m-ext-url-fetch nil)
;;        (t "Browse by emacs-w3m" mew-w3m-ext-url-show nil)))
;;  or
;; (setq mew-ext-url-alist
;;      '((t "Browse by emacs-w3m" mew-w3m-ext-url-show nil)))
;;

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
Non-nil means that the minor mode whose keymap contains keys binded to
some emacs-w3m commands are activated in message buffer, when viewing
Text/Html contents."
  :group 'mew-w3m
  :type 'boolean)

(defcustom mew-w3m-auto-insert-image nil
  "*If non-nil, the images inserts automatic in Multipart/Related message.
This variable effected only XEmacs or Emacs 21."
  :group 'mew-w3m
  :type 'boolean)

(defcustom mew-w3m-cid-retrieve-hook nil
  "*Hook run after cid retrieved"
  :group 'mew-w3m
  :type 'hook)

(defconst mew-w3m-safe-url-regexp "\\`cid:")

;; Avoid bytecompile error and warnings.
(eval-when-compile
  (unless (fboundp 'mew-current-get-fld)
    (autoload 'mew-current-get-fld "mew")
    (autoload 'mew-current-get-msg "mew")
    (autoload 'mew-syntax-get-entry-by-cid "mew")
    (defun mew-cache-hit (&rest args) ())))

(defun mew-w3m-minor-mode-setter ()
  "Check message buffer and activate w3m-minor-mode."
  (w3m-minor-mode (or (and (get-text-property (point-min) 'w3m)
			   mew-use-w3m-minor-mode)
		      0)))

(defvar mew-w3m-use-safe-url-regexp t)

(defun mew-w3m-view-inline-image (&optional allimage)
  "Display the images of Text/Html part.
\\<mew-summary-mode-map>
'\\[mew-w3m-view-inline-image]'	Toggle display the images included its message only.
'\\[universal-argument]\\[mew-w3m-view-inline-image]'	Display the all images included its Text/Html part."
  (interactive "P")
  (mew-summary-msg-or-part
   (if allimage
       (let ((mew-w3m-auto-insert-image t)
	     (mew-w3m-use-safe-url-regexp nil))
	 (mew-summary-display 'force))
     (let ((mew-w3m-auto-insert-image (not mew-w3m-auto-insert-image)))
       (mew-summary-display 'force)))))

;; processing Text/Html contents with w3m.
(defun mew-mime-text/html-w3m (&rest args)
  "View Text/Html contents with w3m rendering output."
  (let ((w3m-display-inline-images mew-w3m-auto-insert-image)
	(w3m-safe-url-regexp (when mew-w3m-use-safe-url-regexp
			       mew-w3m-safe-url-regexp))
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
	      (setq xref (match-string 1))
	      (w3m-static-if (fboundp 'match-string-no-properties)
		  (setq xref (match-string-no-properties 1))
		(setq xref (match-string 1))
		(set-text-properties 0 (length xref) nil xref))))))
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
	 (w3m-region begin end xref (mew-charset-guess-region begin end)))
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

(defvar w3m-mew-support-cid (and (boundp 'mew-version-number)
				 (fboundp 'mew-syntax-get-entry-by-cid)))

(defun mew-w3m-cid-retrieve (url &rest args)
  (let ((output-buffer (current-buffer)))
    (with-current-buffer w3m-current-buffer
      (when (and w3m-mew-support-cid
		 (string-match "^cid:\\(.+\\)" url))
	(setq url (match-string 1 url))
	(let ((fld (mew-current-get-fld (mew-frame-id))))
	  (set-buffer fld)
	  (let* ((msg (mew-current-get-msg (mew-frame-id)))
		 (cache (mew-cache-hit fld msg 'must-hit))
		 (syntax (mew-cache-decode-syntax cache))
		 cidstx beg end)
	    (if (string< "4.0.53" mew-version-number)
		(setq cidstx (mew-syntax-get-entry-by-cid syntax (concat "<" url ">")))
 	      (setq cidstx (mew-syntax-get-entry-by-cid syntax url)))
	    (when cidstx
	      (setq beg (mew-syntax-get-begin cidstx))
	      (setq end (mew-syntax-get-end cidstx))
	      (prog1
		  (with-current-buffer output-buffer
		    (set-buffer-multibyte t)
		    (insert-buffer-substring cache beg end)
		    (set-buffer-multibyte nil)
		    (downcase (car (mew-syntax-get-ct cidstx))))
		(run-hooks 'mew-w3m-cid-retrieve-hook)))))))))

(when w3m-mew-support-cid
  (push (cons 'mew-message-mode 'mew-w3m-cid-retrieve)
	w3m-cid-retrieve-function-alist))

(defun mew-w3m-ext-url-show (dummy url)
  (pop-to-buffer (mew-buffer-message))
  (w3m url))

(defun mew-w3m-ext-url-fetch (dummy url)
  (lexical-let ((url url)
		(name (file-name-nondirectory url))
		handler)
    (w3m-process-do
	(success (prog1
		     (w3m-download url nil nil handler)
		   (message "Download: %s..." name)))
      (if success
	  (message "Download: %s...done" name)
	(message "Download: %s...failed" name))
      (sit-for 1))))

;;;
(provide 'mew-w3m)

;; mew-w3m.el ends here

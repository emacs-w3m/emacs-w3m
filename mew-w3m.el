;;; mew-w3m.el --- View Text/Html content with w3m in Mew -*- lexical-binding: t -*-

;; Copyright (C) 2001-2006, 2008-2010, 2019
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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
;; (3) To display images in a Text/Html message, add the following snippet
;; to the ~/.mew file.
;;
;; (define-key mew-summary-mode-map "T" 'mew-w3m-view-inline-image)
;;
;; Press "T":    Toggle the visibility of images in only the message.
;; Press "C-uT": Display images in Text/Html parts in any message.
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

;;; initializer for mew
(defgroup mew-w3m nil
  "mew-w3m - Inline HTML rendering extension of Mew"
  :group 'w3m)

(defcustom mew-use-w3m-minor-mode nil
  "Use w3m minor mode in message buffer.
Non-nil means that the minor mode whose keymap contains keys binded to
some emacs-w3m commands are activated in message buffer, when viewing
Text/Html contents."
  :group 'mew-w3m
  :type 'boolean)

(defcustom mew-w3m-auto-insert-image nil
  "Non-nil means insert images inline in an html message automatically."
  :group 'mew-w3m
  :type 'boolean)

(defcustom mew-w3m-cid-retrieve-hook nil
  "Hook run after cid retrieved"
  :group 'mew-w3m
  :type 'hook)

(defcustom mew-w3m-region-cite-mark "&gt;&nbsp;"
  "Method of converting `blockquote'."
  :group 'mew-w3m
  :type '(choice (const :tag "Use Indent" nil)
		 (const :tag "Use Cite Mark \"> \"" "&gt;&nbsp;")
		 (string :tag "Use Other Mark")))

(defcustom mew-w3m-safe-url-regexp "\\`\\(cid\\|data\\):"
  "Regexp that matches safe url names.
Some HTML mails might have the trick of spammers using <img> tags.  It
is likely to be intended to verify whether you have read the mail.
You can prevent your personal informations from leaking by setting
this to the regexp which matches the safe url names.  The value of the
variable `w3m-safe-url-regexp' will be bound with this value.  You may
set this value to nil if you consider all the urls to be safe."
  :group 'mew-w3m
  :type '(choice (regexp :format "%t: %v")
		 (const :tag "All URLs are safe" nil)))

;; Avoid bytecompile errors and warnings.
(defvar mew-use-text/html)
(declare-function mew-cache-hit "mew-cache" (fld msg &optional must-hit))
(declare-function mew-coding-system-p "mew-mule3" (cs))
(declare-function mew-current-get-fld "mew" (arg))
(declare-function mew-current-get-msg "mew" (arg))
(declare-function mew-syntax-get-entry-by-cid "mew-syntax" (syntax cid))

(defmacro mew-w3m-add-text-properties (props)
  `(add-text-properties (point-min)
			(min (1+ (point-min)) (point-max))
			,props))

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
       (let ((mew-use-text/html t)
	     (mew-w3m-auto-insert-image t)
	     (mew-w3m-use-safe-url-regexp nil))
	 (mew-summary-display 'force))
     (with-current-buffer (mew-buffer-message)
       (let* ((image (get-text-property (point-min) 'w3m-images))
	      (w3m-display-inline-images image)
	      (w3m-safe-url-regexp (when mew-w3m-use-safe-url-regexp
				     mew-w3m-safe-url-regexp)))
	 (w3m-toggle-inline-images)
	 (mew-elet
	  (mew-w3m-add-text-properties `(w3m-images ,(not image)))
	  (set-buffer-modified-p nil)))))))

(defun mew-w3m-cite-blockquote (&optional inside-blockquote)
  "Quote paragraphs in <blockquote>...</blockquote> with the citation mark.
The variable `mew-w3m-region-cite-mark' specifies the citation mark."
  (let ((case-fold-search t))
    (while (and (re-search-forward "\
[\t\n ]*<[\t\n ]*blockquote\\(?:[\t\n ]*>\\|[\t\n ]+[^>]+>\\)" nil t)
		(w3m-end-of-tag "blockquote" t))
      (save-restriction
	(narrow-to-region (match-beginning 0) (match-end 0))
	(delete-region (goto-char (match-end 3)) (match-end 0))
	(insert "\n")
	(delete-region (goto-char (point-min)) (match-beginning 3))
	(insert "\n")
	(while (and (re-search-forward
		     "<[\t\n ]*pre\\(?:[\t\n ]*>\\|[\t\n ]+[^>]+>\\)" nil t)
		    (w3m-end-of-tag "pre" t))
	  (delete-region (goto-char (match-end 2)) (match-end 0))
	  (if (bolp)
	      (when (looking-at "\n+") (replace-match ""))
	    (insert "\n"))
	  (delete-region (goto-char (match-beginning 0)) (match-beginning 2))
	  (if (bolp)
	      (when (looking-at "\n+") (replace-match ""))
	    (insert "\n")))
	(goto-char (point-min))
	(mew-w3m-cite-blockquote 'inside-blockquote)
	(goto-char (point-min))
	(while (re-search-forward
		"[\t\n ]*<br\\(?:[\t\n ]*>\\|[\t\n ]+[^>]+>\\)" nil t)
	  (replace-match "\n"))
	(goto-char (point-min))
	(while (and (re-search-forward
		     "[\t\n ]*<div\\(?:[\t\n ]*>\\|[\t\n ]+[^>]+>\\)" nil t)
		    (w3m-end-of-tag "div"))
	  (goto-char (match-beginning 0))
	  (insert "\n")
	  (goto-char (1+ (match-end 0)))
	  (insert "\n"))
	(goto-char (point-min))
	(while (re-search-forward "^[\t <>]+$" nil t)
	  (replace-match ""))
	(goto-char (point-min))
	(while (re-search-forward "\n\n\n+" nil t)
	  (replace-match "\n\n"))
	(goto-char (point-min))
	(when mew-w3m-region-cite-mark
	  (goto-char (point-min))
	  (while (re-search-forward "[^\t\n ]" nil t)
	    (beginning-of-line)
	    (if (looking-at "[\t ]+")
		(replace-match mew-w3m-region-cite-mark)
	      (insert mew-w3m-region-cite-mark))
	    (end-of-line)))
	(unless inside-blockquote
	  ;; "> > > " --> ">>> "
	  (when (and mew-w3m-region-cite-mark
		     (string-match "&nbsp;\\'" mew-w3m-region-cite-mark))
	    (let ((base (substring mew-w3m-region-cite-mark
				   0 (match-beginning 0)))
		  (regexp (regexp-quote mew-w3m-region-cite-mark)))
	      (setq regexp (concat "^" regexp "\\(?:" regexp "\\)+"))
	      (goto-char (point-min))
	      (while (re-search-forward regexp nil t)
		(dotimes (_i (prog1
				 (/ (- (match-end 0) (match-beginning 0))
				    (length mew-w3m-region-cite-mark))
			       (delete-region (match-beginning 0)
					      (match-end 0))))
		  (insert base))
		(insert "&nbsp;"))))
	  (goto-char (point-min))
	  (insert "<pre>")
	  (goto-char (point-max))
	  (insert "</pre>\n"))))))

(defun mew-w3m-region (start end &optional url charset)
  "w3m-region with inserting the cite mark."
  (if (null mew-w3m-region-cite-mark)
      (w3m-region start end url charset)
    (save-restriction
      (narrow-to-region (goto-char start) end)
      (mew-w3m-cite-blockquote)
      (w3m-region (point-min) (point-max) url charset)
      (goto-char (point-min))
      (while (re-search-forward "^[\t ]+$" nil t)
	(replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "\n\n\n+" nil t)
	(replace-match "\n\n"))
      (goto-char (point-min))
      (skip-chars-forward "\n")
      (delete-region (point-min) (point))
      (goto-char (point-max))
      (skip-chars-backward "\n")
      (delete-region (point) (point-max))
      (insert "\n"))))

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
	      (setq xref (match-string-no-properties 1))))))
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
	   (mew-w3m-region begin end xref)))
	((null cache)	;; Mew-2 + w3m, w3mmee
	 (mew-w3m-region begin end xref (mew-charset-guess-region begin end)))
	(t		;; Old Mew
	 (setq charset (or (mew-syntax-get-param params "charset")
			   (with-current-buffer cache
			     (mew-charset-guess-region begin end))))
	 (if charset
	     (setq wcs (mew-charset-to-cs charset))
	   (setq wcs mew-cs-text-for-write))
	 (mew-frwlet
	     mew-cs-dummy wcs
	   (mew-w3m-region (point)
			   (progn (insert-buffer-substring cache begin end)
				  (point))
			   xref))))
       (mew-w3m-add-text-properties `(w3m t w3m-images ,mew-w3m-auto-insert-image))))))

(defun mew-w3m-cid-retrieve (url &rest _args)
  (let ((output-buffer (current-buffer)))
    (with-current-buffer w3m-current-buffer
      (when (string-match "\\`cid:\\(.+\\)" url)
	(setq url (match-string 1 url))
	(let* ((fld (mew-current-get-fld (mew-frame-id)))
	       (msg (mew-current-get-msg (mew-frame-id)))
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
	      (run-hooks 'mew-w3m-cid-retrieve-hook))))))))

(push (cons 'mew-message-mode 'mew-w3m-cid-retrieve)
      w3m-cid-retrieve-function-alist)

(defun mew-w3m-ext-url-show (_dummy url)
  (pop-to-buffer (mew-buffer-message))
  (w3m url))

(defun mew-w3m-ext-url-fetch (_dummy url)
  (let ((name (file-name-nondirectory url))
	handler)
    (w3m-process-do
	(success (prog1
		     (w3m-download url nil nil handler)
		   (message "Download: %s..." name)))
      (if success
	  (message "Download: %s...done" name)
	(message "Download: %s...failed" name))
      (sit-for 1))))

(defun w3m-mail-compose-with-mew (source url charset content-type
					 to subject other-headers)
  "Compose a mail using Mew."
  (when (one-window-p)
    (split-window))
  (select-window (next-window))
  (condition-case nil
      (unless (and mew-init-p
		   (progn
		     (mew-summary-jump-to-draft-buffer)
		     (and (eq major-mode 'mew-draft-mode)
			  (y-or-n-p "Attatch this draft? "))))
	(mew-user-agent-compose to subject other-headers))
    (quit
     (if (y-or-n-p "Create new draft? ")
	 (mew-user-agent-compose to subject other-headers)
       (delete-window)
       (error "Abort mail composing"))))
  (let* ((basename (file-name-nondirectory (w3m-url-strip-query url)))
	 (ct (downcase  content-type))
	 (mew-attach-move-next-after-copy nil)
	 (i 1)
	 (pos -1)
	 (csorig (mew-charset-to-cs (symbol-name charset)))
	 last filename cs)
    (unless (mew-attach-p)
      (mew-draft-prepare-attachments))
    ;; goto last attachment
    (setq last (catch 'last
		 (while (not (= pos (point)))
		   (setq i (1+ i))
		   (mew-attach-goto-number 'here `(,i))
		   (when (mew-attach-line-lastp)
		     (throw 'last t)))))
    (when (eq csorig mew-cs-unknown)
      (setq csorig nil))
    (if (or (not last) (not (mew-attach-not-line012-1)))
	(message "Can not attach from emacs-w3m here!")
      ;; Application/.*xml is not inline view with Mew.
      (cond
       ((string= "application/xhtml+xml" ct)
	(setq ct "text/html"))
       ((string-match "\\`application/.*xml\\'" ct)
	(setq ct "text/xml")))
      (setq filename
	    (expand-file-name (cond
			       ((and (string-match "\\`[\t ]*\\'" basename)
				     (string= ct "text/html"))
				"index.html")
			       ((and (string-match "\\`[\t ]*\\'" basename)
				     (string= ct "text/xml"))
				"index.xml")
			       ((string-match "\\`[\t ]*\\'" basename)
				"dummy")
			       (t
				basename))
			      mew-temp-dir))
      (with-temp-buffer
	(cond
	 ((string= "text/html" ct)
	  (insert source)
	  (setq cs (mew-text/html-detect-cs (point-min) (point-max)))
	  (when (or (eq cs mew-cs-unknown) (not cs))
	    (cond
	     (csorig
	      (setq cs csorig))
	     (t
	      (setq cs mew-cs-autoconv)))))
	 ((string= "text/xml" ct)
	  (insert source)
	  (setq cs (mew-text/html-detect-cs (point-min) (point-max)))
	  (when (or (eq cs mew-cs-unknown) (not cs))
	    (cond
	     (csorig
	      (setq cs csorig))
	     ((mew-coding-system-p 'utf-8)
	      (setq cs 'utf-8))
	     (t
	      (setq cs mew-cs-autoconv)))))
	 ((string-match "\\`text/" ct)
	  (insert source)
	  (setq cs mew-cs-autoconv))
	 (t
	  (mew-set-buffer-multibyte nil)
	  (insert source)
	  (setq cs mew-cs-binary)))
	(setq charset (cond
		       ((eq cs mew-cs-autoconv)
			(mew-charset-guess-region (point-min) (point-max)))
		       ((eq cs mew-cs-binary)
			nil)
		       (t
			(mew-cs-to-charset cs))))
	(mew-frwlet
	    mew-cs-text-for-read cs
	  (write-region (point-min) (point-max) filename nil 'nomsg)))
      (when ct
	(setq ct (mew-capitalize ct)))
      (mew-attach-copy filename (file-name-nondirectory filename))
      ;; content-type check & set
      (let* ((nums (mew-syntax-nums))
	     (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	     (file (mew-syntax-get-file syntax))
	     (ctl (mew-syntax-get-ct syntax))
	     (ct-orig (mew-syntax-get-value ctl 'cap))
	     cte)
	(unless (string= ct ct-orig)
	  (setq ctl (list ct))
	  (mew-syntax-set-ct syntax ctl)
	  (setq cte (mew-ctdb-cte (mew-ctdb-by-ct ct)))
	  (mew-syntax-set-cte syntax cte)
	  (mew-syntax-set-cdp syntax (mew-syntax-cdp-format ct file))
	  (mew-encode-syntax-print mew-encode-syntax)))
      ;; charset set
      (let* ((nums (mew-syntax-nums))
	     (syntax (mew-syntax-get-entry mew-encode-syntax nums))
	     (ctl (mew-syntax-get-ct syntax))
	     (ct (mew-syntax-get-value ctl 'cap))
	     (params (mew-syntax-get-params ctl))
	     (ocharset "charset"))
	(when (and (string-match "\\`Text" ct) charset)
	  (setq params (mew-delete ocharset params))
	  (setq ctl (cons ct (cons (list ocharset charset) params)))
	  (mew-syntax-set-ct syntax ctl))
	(mew-syntax-set-cd syntax url)
	(mew-encode-syntax-print mew-encode-syntax))
      (message "Compose a mail using Mew with %s...done" url)
      (when (and (file-exists-p filename) (file-writable-p filename))
	(delete-file filename)))))

(provide 'mew-w3m)

;; mew-w3m.el ends here

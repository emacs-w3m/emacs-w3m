;; -*- mode: emacs-lisp -*-
;; mew-shimbun.el --- View shimbun contents with Mew

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;;         Hideyuki SHIRAI <shirai@meadowy.org>
;; Keywords: Mew, shimbun, w3m, WWW, hypermedia

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
;; This package is SHIMBUN interface for Mew version 2 or later.

;;; Instalation:
;; Simply load this file and add followings in your ~/.mew file.
;; If you can understand Japanese, please read 'README.shimbun.ja'.
;;
;;; Comment out below one line, if you use 'Mew Shimbun unseen mark'.
;;
;;; (setq mew-shimbun-use-unseen t)
;;;; (setq mew-shimbun-use-unseen-cache-save t)
;;
;; (setq mew-shimbun-folder-groups .... ;; as you please
;;
;; (require 'mew-shimbun)
;; (define-key mew-summary-mode-map "G"  (make-sparse-keymap))
;; (define-key mew-summary-mode-map "Gg" 'mew-shimbun-goto-folder)
;; (define-key mew-summary-mode-map "Gi" 'mew-shimbun-retrieve)
;; (define-key mew-summary-mode-map "GI" 'mew-shimbun-retrieve-all)
;; (define-key mew-summary-mode-map "Gr" 'mew-shimbun-re-retrieve)
;; (define-key mew-summary-mode-map "GR" 'mew-shimbun-re-retrieve-all)
;; (when mew-shimbun-use-unseen
;;   (define-key mew-summary-mode-map "Gu" 'mew-shimbun-remove-unseen)
;;   (define-key mew-summary-mode-map "GU" 'mew-shimbun-remove-unseen-all))
;;

;;; Code:
;; disable runtime cl
(eval-when-compile
  (require 'cl))

(eval-when-compile
  (unless (dolist (var nil t))
    (load "cl-macs" nil t)))

(eval-and-compile
  (require 'shimbun)
  (require 'mew))

;; countermeasure for byte-compile warnings
(eval-when-compile
  (unless (fboundp 'MEW-FLD)
    (defun MEW-FLD () ()))
  (unless (fboundp 'MEW-ID)
    (defun MEW-ID () ()))
  (unless (fboundp 'MEW-TO)
    (defun MEW-TO () ())
  (unless (fboundp 'MEW-SHIMBUN-STS)
    (defun MEW-SHIMBUN-STS () ()))))

(defcustom mew-shimbun-folder "+shimbun"
  "*The folder where SHIMBUN are contained."
  :group 'shimbun
  :group 'mew
  :type 'string)

(defcustom mew-shimbun-folder-groups nil
  "*Alist of `shimbun folder name (exclude `mew-shimbun-folder')'
and included `shimbun server.groups' and its `range parameters',
show below example,
  '((\"yomiuri\"		;; \"shimbun folder\"
     (\"yomiuri.shakai\" . 2)	;; (\"server.group\" . range)
     (\"yomiuri.sports\". 2)
     (\"yomiuri.seiji\" . 2)
     (\"yomiuri.keizai\" . 1)
     (\"yomiuri.kokusai\". 1))
    (\"comp\"
     (\"cnet.comp\" . last)
     (\"zdnet.comp\" . last))
    (\"mew/mgp\"
     (\"mew.mgp-users\" . last)
     (\"mew.mgp-users-jp\" . last))
    (\"mew/mew-int\"
     (\"mew.mew-int\" . last)))
"
  :group 'shimbun
  :group 'mew
  :type '(repeat
	  (cons (string :tag "Folder")
		(repeat
		 (cons (string :tag "Server.Group")
		       (choice :tag "Index checking range"
			       (const all)
			       (const last)
			       (integer :tag "pages")))))))

(defcustom mew-shimbun-db-file ".mew-shimbun-db"
  "*File name of mew-shimbun database."
  :group 'shimbun
  :group 'mew
  :type 'file)

(defcustom mew-shimbun-groups-db-length nil
  "*Max length of mew-shimbun database.
If nil, same 'mew-lisp-max-length'.
If integer, all group limit 'integer'.
If alist, each cell has shimbun folder names and their max length,
show below example,

  '((\"mew/mgp\" . 1000)
    (\"tcup/meadow\" . 20)
    (\"asahi\" . 100)
    (\"slashdot-jp/story\" . 3000)
    (t . 2000))
"
  :group 'shimbun
  :group 'mew
  :type '(choice
	  (const :tag "same 'mew-lisp-max-length'" nil)
	  (integer :tag "limit for all group" :value 2000)
	  (repeat :tag "alist group and length"
		  (cons (choice :tag "Group"
				(string :tag "server/group or server")
				(const :tag "other" t))
			(choice :tag "Max length of database"
				(integer :tag "length" :value 2000))))))

(defcustom mew-shimbun-unknown-from "foo@bar.baz"
  "*Shimbun mail address when From header is strange."
  :group 'shimbun
  :group 'mew
  :type 'string)

(defcustom mew-shimbun-mark-re-retrieve mew-mark-multi
  "*Shimbun re-retrieve mark."
  :group 'shimbun
  :group 'mew
  :type 'character)

(defcustom mew-shimbun-mark-unseen mew-mark-review
  "*Shimbun unseen mark."
  :group 'shimbun
  :group 'mew
  :type 'character)

(defcustom mew-shimbun-use-unseen nil
  "*If non-nil, SHIMBUN folder support 'unseen' mark."
  :group 'shimbun
  :group 'mew
  :type 'boolean)

(defcustom mew-shimbun-use-unseen-cache-save nil
  "*If non-nin, save '.mew-cache' whenever remove 'unseen' mark."
  :group 'shimbun
  :group 'mew
  :type 'boolean)

(defcustom mew-shimbun-mark-unseen mew-mark-review
  "*Shimbun unseen mark."
  :group 'shimbun
  :group 'mew
  :type 'character)

(defcustom mew-shimbun-before-retrieve-hook nil
  "*Hook run after mew-shimbun-retrieve called."
  :group 'shimbun
  :group 'mew
  :type 'hook)

(defcustom mew-shimbun-retrieve-hook nil
  "*Hook run after mew-shimbun-retrieve called."
  :group 'shimbun
  :group 'mew
  :type 'hook)

(defconst mew-shimbun-id-format "%s+%s:%s")
(defconst mew-shimbun-db-buffer-name " *mew-shimbun-overview*")
(defconst mew-shimbun-article-buffer-name " *mew-shimbun-article*")

(defvar mew-shimbun-unseen-regex
  (concat mew-regex-msg "\\("
	  (regexp-quote (string mew-shimbun-mark-unseen))
	  "\\)"))
(defvar mew-shimbun-folder-regex
  (mew-folder-regex (concat mew-shimbun-folder "/")))
(defvar mew-shimbun-db nil)
(defvar mew-shimbun-db2 nil)
(defvar mew-shimbun-input-hist nil)

;;; Macro:
(defmacro mew-shimbun-db-search-id (id)
  `(assoc ,id mew-shimbun-db))

(defmacro mew-shimbun-db-search-id2 (id)
  `(assoc ,id mew-shimbun-db2))

(defsubst mew-shimbun-folder-p (fld)
  (if (string-match mew-shimbun-folder-regex fld) t nil))

(defmacro mew-shimbun-element-body (sgr &rest body)
  `(when (string-match "\\([^.]+\\)\\.\\(.+\\)" (car ,sgr))
     (let ((server (mew-match 1 (car ,sgr)))
	   (group (mew-match 2 (car ,sgr)))
	   (range (cdr ,sgr)))
       ,@body)))
(put 'mew-shimbun-element-body 'lisp-indent-function 1)

;;; Main:
;;;###autoload
(defun mew-shimbun-goto-folder (&optional args)
  "Goto folder for SHIMBUN.
If called with '\\[universal-argument]', goto folder to have few new messages."
  (interactive "P")
  (let ((flds mew-folder-list)
	sbflds alst fld cfile)
    (save-excursion
      (dolist (fld flds)
	(when (and (mew-shimbun-folder-p fld)
		   (file-exists-p
		    (expand-file-name mew-shimbun-db-file
				      (mew-expand-folder fld))))
	  (if (null args)
	      (setq sbflds (cons fld sbflds))
	    (if (mew-shimbun-folder-new-p fld)
		(setq sbflds (cons (concat fld "*") sbflds))
	      (when mew-shimbun-use-unseen
		(if (get-buffer fld)
		    (with-current-buffer fld
		      (goto-char (point-min))
		      (when (re-search-forward mew-shimbun-unseen-regex nil t)
			(setq sbflds (cons (concat fld "%") sbflds))))
		  (setq cfile (mew-expand-folder fld mew-summary-cache-file))
		  (when (file-readable-p cfile)
		    (with-temp-buffer
		      (mew-frwlet
		       mew-cs-text-for-read mew-cs-dummy
		       (insert-file-contents cfile nil)
		       (goto-char (point-min))
		       (when (re-search-forward mew-shimbun-unseen-regex nil t)
			 (setq sbflds (cons (concat fld "%") sbflds)))))))))))))
    (mapcar (lambda (x)
	      (setq alst (cons (list x) alst)))
	    sbflds)
    (let ((completion-ignore-case mew-complete-folder-ignore-case))
      (setq fld (completing-read
		 (if args
		     "Shimbun unread folder: "
		   "Shimbun folder: ")
		 alst
		 nil t (concat mew-shimbun-folder "/")
		 'mew-shimbun-input-hist)))
    (when (string-match "[*%]$" fld)
      (setq fld (substring fld 0 (match-beginning 0)))
      (setcar mew-shimbun-input-hist fld))
    (setq mew-input-folder-hist (cons fld mew-input-folder-hist))
    (mew-summary-ls (mew-summary-switch-to-folder fld))))

;;;###autoload
(defun mew-shimbun-retrieve ()
  "Retrieve articles via SHIMBUN on this folder."
  (interactive)
  (when (mew-summary-exclusive-p)
    (mew-summary-only
     (let ((fld (mew-summary-folder-name))
	   (mua (luna-make-entity 'shimbun-mew-mua))
	   (count 0)
	   alst server group range)
       (if (not (mew-shimbun-folder-p fld))
	   (message "This command can not execute here")
	 (setq alst (assoc (substring fld (match-end 0)) mew-shimbun-folder-groups))
	 (if (null alst)
	     (message "%s is not include 'mew-shimbun-folder-groups'" fld)
	   (run-hooks 'mew-shimbun-before-retrieve-hook)
	   (mew-window-configure 'summary)
	   (mew-current-set nil nil nil)
	   (mew-decode-syntax-delete)
	   (unless (mew-sinfo-get-scan-form)
	     (mew-sinfo-set-scan-form (mew-summary-scan-form fld)))
	   (mew-scan (mew-scan-mewls-src fld (mew-input-range fld nil)))
	   (mew-rendezvous mew-summary-buffer-process)
	   (save-excursion
	     (dolist (sgr (cdr alst))
	       (mew-shimbun-element-body sgr
		 (setq count
		       (+ (mew-shimbun-retrieve-article
			   mua server group range fld 'scan)
			  count)))))
	   (when (> count 0) (mew-summary-folder-cache-save))
	   (run-hooks 'mew-shimbun-retrieve-hook)
	   (message "Getting %s %s in '%s' done"
		    (if (= count 0) "no" (number-to-string count))
		    (if (> count 1) "messages" "message")
		    fld)))))))

;;;###autoload
(defun mew-shimbun-retrieve-all ()
  "Retrieve all articles via SHIMBUN."
  (interactive)
  (mew-summary-only
   (let ((mua (luna-make-entity 'shimbun-mew-mua))
	 (cfld (mew-summary-folder-name))
	 (count 0)
	 (cfldcount 0)
	 fld server group range)
     (run-hooks 'mew-shimbun-before-retrieve-hook)
     (mew-window-configure 'summary)
     (mew-current-set nil nil nil)
     (mew-decode-syntax-delete)
     (when (mew-shimbun-folder-p cfld)
       (unless (mew-sinfo-get-scan-form)
	 (mew-sinfo-set-scan-form (mew-summary-scan-form cfld)))
       (mew-scan (mew-scan-mewls-src cfld (mew-input-range cfld nil)))
       (mew-rendezvous mew-summary-buffer-process))
     (save-excursion
       (dolist (fldgrp mew-shimbun-folder-groups)
	 (setq fld (concat mew-shimbun-folder "/" (car fldgrp)))
	 (dolist (sgr (cdr fldgrp))
	   (mew-shimbun-element-body sgr
	     (if (not (string= fld cfld))
		 (setq count
		       (+ (mew-shimbun-retrieve-article
			   mua server group range fld nil)
			  count))
	       (setq cfldcount
		     (mew-shimbun-retrieve-article
		      mua server group range fld 'scan))
	       (setq count (+ cfldcount count)))))))
     (when (> cfldcount 0) (mew-summary-folder-cache-save))
     (run-hooks 'mew-shimbun-retrieve-hook)
     (message "Getting %s %s done"
	      (if (= count 0) "no" (number-to-string count))
	      (if (> count 1) "articles" "article")))))

(defun mew-shimbun-retrieve-article (mua server group range fld scan)
  "Retrieve articles via SHIMBUN."
  (luna-define-method shimbun-mua-search-id ((mua shimbun-mew-mua) id)
    (let ((shimbun (shimbun-mua-shimbun-internal mua)))
      (mew-shimbun-db-search-id
       (format mew-shimbun-id-format
	       (shimbun-server-internal shimbun)
	       (shimbun-current-group-internal shimbun)
	       id))))
  (let ((shimbun (shimbun-open server mua))
	(count 0)
	(buf (get-buffer-create mew-shimbun-article-buffer-name))
	msg file)
    (shimbun-open-group shimbun group)
    (unless (file-exists-p (mew-expand-folder fld))
      (mew-make-directory (mew-expand-folder fld)))
    (mew-shimbun-db-setup fld)
    (unwind-protect
	(dolist (head (shimbun-headers shimbun range))
	  (let ((id (format mew-shimbun-id-format
			    server group
			    (shimbun-header-id head)))
		md5)
	    (unless (mew-shimbun-db-search-id id)
	      (with-current-buffer buf
		(mew-erase-buffer)
		(shimbun-article shimbun head)
		(setq md5 (mew-shimbun-md5))
		(when (and (> (buffer-size) 0)
			   (mew-shimbun-db-add-id id md5))
		  (setq count (1+ count))
		  (goto-char (point-min))
		  (when mew-shimbun-use-unseen
		    (insert "X-Shimbun-Status: unseen\n"))
		  (insert (format "X-Shimbun-Id: %s\n" id))
		  (mew-shimbun-sanity-convert)
		  (setq msg (mew-folder-new-message fld 'numonly))
		  (setq file (mew-expand-folder fld msg))
		  (mew-frwlet
		   mew-cs-dummy mew-cs-text-for-write
		   (write-region (point-min) (point-max) file nil 'nomsg))
		  (set-file-modes file mew-file-mode)
		  (when scan
		    (mew-shimbun-scan fld msg)))))))
      (kill-buffer buf)
      (mew-folder-insert fld)
      (shimbun-close-group shimbun)
      (shimbun-close shimbun)
      (mew-shimbun-db-shutdown fld count))
    count))

;;;###autoload
(defun mew-shimbun-re-retrieve (&optional args)
  "Re-retrieve this message.
If called with '\\[universal-argument]', re-retrieve messages marked with '@'."
  (interactive "P")
  (when (mew-summary-exclusive-p)
    (mew-summary-only
     (let* ((fld (mew-summary-folder-name))
	    (msgs (list (progn
			  (mew-summary-goto-message)
			  (mew-summary-message-number))))
	    (mua (luna-make-entity 'shimbun-mew-mua))
	    (newcount 0)
	    (rplcount 0)
	    (same 0)
	    countlst id-msgs alst server group range)
       (if (not (mew-shimbun-folder-p fld))
	   (message "This command can not execute here")
	 (setq alst (assoc (substring fld (match-end 0)) mew-shimbun-folder-groups))
	 (if (null alst)
	     (message "%s is not include 'mew-shimbun-folder-groups'" fld)
	   (run-hooks 'mew-shimbun-before-retrieve-hook)
	   (mew-window-configure 'summary)
	   (mew-current-set nil nil nil)
	   (mew-decode-syntax-delete)
	   (unless (mew-sinfo-get-scan-form)
	     (mew-sinfo-set-scan-form (mew-summary-scan-form fld)))
	   (mew-scan (mew-scan-mewls-src fld (mew-input-range fld nil)))
	   (mew-rendezvous mew-summary-buffer-process)
	   (when args
	     (setq msgs (mew-summary-mark-collect mew-shimbun-mark-re-retrieve)))
	   (if (null msgs)
	       (message "No message re-retrieve.")
	     (setq id-msgs (mew-shimbun-get-id-msgs 'list fld msgs))
	     (if id-msgs
		 (save-excursion
		   (dolist (sgr (cdr alst))
		     (mew-shimbun-element-body sgr
		       (setq countlst
			     (mew-shimbun-re-retrieve-article
			      mua server group range fld id-msgs))
		       (setq rplcount (+ rplcount (nth 0 countlst)))
		       (setq newcount (+ newcount (nth 1 countlst)))
		       (setq same (+ same (nth 2 countlst)))))
		   (message "Replace %s, new %s, same %s messages in '%s' done"
			    rplcount newcount same fld)
		   (when (> (+ newcount rplcount) 0) (mew-summary-folder-cache-save)))
	       (message "No detect 'X-Shimbun-Id:'"))
	     (run-hooks 'mew-shimbun-retrieve-hook))))))))

;;;###autoload
(defun mew-shimbun-re-retrieve-all (&optional arg)
  "Re-retrieve all messages in this folder.
If called with '\\[universal-argument]', re-retrieve messages in the region."
  (interactive "P")
  (when (mew-summary-exclusive-p)
    (mew-summary-only
     (let* ((fld (mew-summary-folder-name))
	    (mua (luna-make-entity 'shimbun-mew-mua))
	    (begend (cons (point-min) (point-max)))
	    (newcount 0)
	    (rplcount 0)
	    (same 0)
	    countlst id-msgs begmsg endmsg alst server group range)
       (if (not (mew-shimbun-folder-p fld))
	   (message "This command can not execute here")
	 (setq alst (assoc (substring fld (match-end 0)) mew-shimbun-folder-groups))
	 (if (null alst)
	     (message "%s is not include 'mew-shimbun-folder-groups'" fld)
	   (when arg
	     (setq begend (mew-summary-get-region)))
	   (save-excursion
	     (save-restriction
	       (narrow-to-region (car begend) (cdr begend))
	       (goto-char (point-min))
	       (mew-summary-goto-message)
	       (setq begmsg (mew-summary-message-number))
	       (goto-char (point-max))
	       (mew-summary-goto-message)
	       (setq endmsg (mew-summary-message-number))))
	   (setq id-msgs (mew-shimbun-get-id-msgs 'range fld begmsg endmsg))
	   (unless (mew-sinfo-get-scan-form)
	     (mew-sinfo-set-scan-form (mew-summary-scan-form fld)))
	   (mew-window-configure 'summary)
	   (mew-current-set nil nil nil)
	   (mew-decode-syntax-delete)
	   (mew-scan (mew-scan-mewls-src fld (mew-input-range fld nil)))
	   (mew-rendezvous mew-summary-buffer-process)
	   (run-hooks 'mew-shimbun-before-retrieve-hook)
	   (if id-msgs
	       (save-excursion
		 (dolist (sgr (cdr alst))
		   (mew-shimbun-element-body sgr
		     (setq countlst
			   (mew-shimbun-re-retrieve-article
			    mua server group range fld id-msgs))
		     (setq rplcount (+ rplcount (nth 0 countlst)))
		     (setq newcount (+ newcount (nth 1 countlst)))
		     (setq same (+ same (nth 2 countlst)))))
		 (message "Replace %s, new %s, same %s messages in '%s' done"
			  rplcount newcount same fld)
		 (when (> (+ newcount rplcount) 0) (mew-summary-folder-cache-save)))
	     (message "No detect 'X-Shimbun-Id:'"))
	   (run-hooks 'mew-shimbun-retrieve-hook)))))))

(defun mew-shimbun-get-id-msgs (type &rest args)
  (let (id-msgs)
    (cond
     ((eq type 'list)
      ;; folder msgs
      (with-temp-buffer
	(dolist (msg (car (cdr args)))
	  (erase-buffer)
	  (mew-insert-message (car args) msg mew-cs-text-for-read 1024)
	  (goto-char (point-min))
	  (when (re-search-forward "^X-Shimbun-Id: \\(.+\\)\n" nil t)
	    (setq id-msgs (cons (cons (mew-match 1) msg) id-msgs)))))
      (nreverse id-msgs))
     ((eq type 'range)
      ;; folder begin-message end-message
      (with-temp-buffer
	(mew-piolet
	 mew-cs-text-for-read mew-cs-text-for-write
	 (call-process mew-prog-mewls nil t nil
		       "-b" mew-mail-path
		       "-d" "X-Shimbun-Id:"
		       "-s" (format "%s %s-%s" (nth 0 args) (nth 1 args) (nth 2 args))))
	(goto-char (point-min))
	(while (re-search-forward "^\\([1-9][0-9]*\\): \\([^\n]+\\)" nil t)
	  (setq id-msgs (cons (cons (mew-match 2) (mew-match 1)) id-msgs))))
      (nreverse id-msgs))
     (t nil))))

(defun mew-shimbun-re-retrieve-article (mua server group range fld id-msgs)
  "Re-retrieve articles via SHIMBUN."
  (luna-define-method shimbun-mua-search-id ((mua shimbun-mew-mua) id)
    (let ((shimbun (shimbun-mua-shimbun-internal mua)))
      (mew-shimbun-db-search-id2
       (format mew-shimbun-id-format
	       (shimbun-server-internal shimbun)
	       (shimbun-current-group-internal shimbun)
	       id))))
  (let ((shimbun (shimbun-open server mua))
	(buf (get-buffer-create mew-shimbun-article-buffer-name))
	(newcount 0)
	(rplcount 0)
	(same 0))
    (shimbun-open-group shimbun group)
    (mew-shimbun-db-setup2 fld id-msgs)
    (unwind-protect
	(dolist (head (shimbun-headers shimbun range))
	  (let ((newid (format mew-shimbun-id-format
			       server group
			       (shimbun-header-id head)))
		newmd5 oldmd5
		alst msg file)
	    (unless (mew-shimbun-db-search-id2 newid)
	      (if (setq alst (assoc newid id-msgs))
		  ;; message replace?
		  (progn
		    (setq rplcount (1+ rplcount))
		    (setq msg (cdr alst))
		    (setq oldmd5 (cdr (mew-shimbun-db-search-id newid))))
		;; new message
		(setq newcount (1+ newcount))
		(setq msg (mew-folder-new-message fld 'numonly))
		(setq oldmd5 nil))
	      (setq file (mew-expand-folder fld msg))
	      (with-current-buffer buf
		(mew-erase-buffer)
		(shimbun-article shimbun head)
		(when (> (buffer-size) 0)
		  (setq newmd5 (mew-shimbun-md5))
		  (if (and (stringp oldmd5) (string= oldmd5 newmd5))
		      ;; same message
		      (setq rplcount (1- rplcount) same (1+ same))
		    (mew-shimbun-db-add-id newid newmd5 (stringp oldmd5))
		    (goto-char (point-min))
		    (when mew-shimbun-use-unseen
		      (insert "X-Shimbun-Status: unseen\n"))
		    (insert (format "X-Shimbun-Id: %s\n" newid))
		    (mew-shimbun-sanity-convert)
		    (mew-frwlet
		     mew-cs-dummy mew-cs-text-for-write
		     (write-region (point-min) (point-max) file nil 'nomsg))
		    (set-file-modes file mew-file-mode)
		    (mew-shimbun-scan fld msg)))))))
      (kill-buffer buf)
      (shimbun-close-group shimbun)
      (shimbun-close shimbun)
      (mew-shimbun-db-shutdown2 fld (+ newcount rplcount)))
    (list rplcount newcount same)))

;;; Mew interface funcitions:
(defun mew-shimbun-scan (fld msg)
  (let ((width (1- (mew-scan-width)))
	(vec (mew-pop-scan-header)))
    (mew-scan-set-folder vec fld)
    (mew-scan-set-message vec msg)
    (mew-scan-insert-line fld vec width msg nil)
    (sit-for 0.05)))	;; for summary redraw

(defun mew-shimbun-sanity-convert ()
  (if (re-search-forward mew-eoh nil t)
      (beginning-of-line)
    (goto-char (point-max))
    (insert "\n"))
  (save-restriction
    (let ((case-fold-search t)
	  (unknown-from mew-shimbun-unknown-from)
	  beg end from from13)
      (narrow-to-region (point-min) (point))
      (goto-char (point-min))
      (if (not (re-search-forward mew-from: nil t))
	  ;; No From:
	  (progn
	    (goto-char (point-max))
	    (insert (concat mew-from: " " unknown-from "\n")))
	(setq beg (match-end 0))
	(forward-line)
	(mew-header-goto-next)
	(setq end (1- (point)))
	(setq from (or (buffer-substring beg end) ""))
	(setq from (or (mew-addrstr-parse-address from) ""))
	(unless (string-match
		 "^[-A-Za-z0-9._!%]+@[A-Za-z0-9][-A-Za-z0-9._!]+[A-Za-z0-9]$"
		 from)
	  ;; strange From:
	  (goto-char (point-min))
	  (when (re-search-forward "^From-R13:" nil t)
	    ;; From-R13:
	    (setq beg (match-end 0))
	    (forward-line)
	    (mew-header-goto-next)
	    (setq from13 (buffer-substring beg (1- (point))))
	    (when (setq from13 (mew-shimbun-sanity-convert-rot13 from13))
	      (setq unknown-from from13)))
	  (goto-char end)
	  (insert " <" unknown-from ">"))))))

(defun mew-shimbun-sanity-convert-rot13 (from13)
  (with-temp-buffer
    (insert from13)
    ;; from13 is binary
    (mew-cs-decode-region (point-min) (point-max) mew-cs-autoconv)
    (goto-char (point-min))
    ;; Extent rot14(@,A-Z,[) + rot13(a-z)
    (while (< (point) (point-max))
      (let* ((chr (char-after (point))))
	(cond
	 ((and (<= ?@ chr) (<= chr ?\[))
	  (setq chr (+ chr 14))
	  (when (> chr ?\[) (setq chr (- chr 28)))
	  (delete-char 1)
	  (insert chr))
	 ((and (<= ?a chr) (<= chr ?z))
	  (setq chr (+ chr 13))
	  (when (> chr ?z) (setq chr (- chr 26)))
	  (delete-char 1)
	  (insert chr))
	 (t (forward-char)))))
    (setq from13 (buffer-substring (point-min) (point-max)))
    (mew-addrstr-parse-address from13)))

(defun mew-shimbun-remove-unseen ()
  "Remove 'unseen' mark and 'X-Shimbun-Status:'."
  (interactive)
  (mew-summary-or-thread
   (let* ((all (when (interactive-p)
		 (prog1 'all
		   (mew-decode-syntax-delete))))
	  (fld (mew-summary-folder-name))
	  (vfld (mew-summary-folder-name 'ext))
	  (msg (mew-summary-message-number))
	  (part (mew-syntax-nums))
	  (file (mew-expand-folder fld msg)))
     (when (and fld msg (null part)
		(mew-shimbun-folder-p fld)
		(file-readable-p file))
       (mew-shimbun-remove-unseen-one fld vfld msg file all)))))

(defun mew-shimbun-remove-unseen-all (&optional arg)
  "Remove 'unseen' mark and 'X-Shimbun-Status:' of all messages in folder.
If called with '\\[universal-argument]', remove 'unseen' mark in the region."
  (interactive "P")
  (mew-summary-or-thread
   (let* ((fld (mew-summary-folder-name))
	  (vfld (mew-summary-folder-name 'ext))
	  (begend (cons (point-min) (point-max)))
	  msg file)
     (if (not (mew-shimbun-folder-p fld))
	 (message "Can not exec here")
       (when arg (setq begend (mew-summary-get-region)))
       (mew-decode-syntax-delete)
       (save-excursion
	 (goto-char (car begend))
	 (while (re-search-forward mew-shimbun-unseen-regex (cdr begend) t)
	   (setq msg (mew-summary-message-number))
	   (setq file (mew-expand-folder fld msg))
	   (message "Shimbun seen...%s/%s" fld msg)
	   (when (file-readable-p file)
	     (mew-shimbun-remove-unseen-one fld vfld msg file 'all)))
	 (message "Shimbun seen...done")
	 (if (string= fld vfld)
	     ;; normal shimbun folder
	     (unless mew-summary-buffer-process
	       (mew-summary-folder-cache-save)
	       (set-buffer-modified-p nil))
	   ;; in thread folder
	   (when (get-buffer fld)
	     ;; normal shimbun folder
	     (set-buffer fld)
	     (condition-case nil
		 (mew-summary-folder-cache-save)
	       (error nil))
	     (set-buffer-modified-p nil))))))))

(defun mew-shimbun-remove-unseen-advice ()
  "Remove 'unseen' mark and 'X-Shimbun-Status:' for advice."
   (let* ((fld (mew-summary-folder-name))
	  (vfld (mew-summary-folder-name 'ext))
	  (msg (mew-summary-message-number))
	  (part (mew-syntax-nums))
	  (file (mew-expand-folder fld msg)))
     (when (and fld msg (null part)
		(mew-shimbun-folder-p fld)
		(file-readable-p file))
       (mew-shimbun-remove-unseen-one fld vfld msg file nil))))

(defun mew-shimbun-remove-unseen-one (fld vfld msg file all)
  (let ((msgbuf (mew-buffer-message))
	(det nil) cbuf)
    (unless all
      (when (and msgbuf (get-buffer msgbuf))
	(save-excursion
	  (set-buffer msgbuf)
	  ;; message buffer
	  (goto-char (point-min))
	  (when (search-forward "X-Shimbun-Status: unseen\n" (mew-header-end) t)
	    (setq det t)))))
    (when (or all det)
      (with-temp-buffer
	(mew-insert-message fld msg mew-cs-text-for-read nil)
	(goto-char (point-min))
	(when (re-search-forward "^X-Shimbun-Status: unseen\n" nil t)
	  (delete-region (match-beginning 0) (match-end 0))
	  (mew-frwlet
	   mew-cs-dummy mew-cs-text-for-write
	   (write-region (point-min) (point-max) file nil 'nomsg)
	   ;; cache fake
	   (unless all
	     (when (setq cbuf (mew-cache-buffer-get (mew-cache-get fld msg)))
	       (set-buffer cbuf)
	       (mew-cinfo-set
		fld msg (mew-file-get-time file) (mew-file-get-size file))))))))
    ;; summary buffer
    (save-excursion
      (beginning-of-line)
      (when (looking-at mew-shimbun-unseen-regex)
	;; in normal or thread folder
	(mew-mark-unmark)
	(set-buffer-modified-p nil)
	(when (and (not (string= fld vfld)) (get-buffer fld))
	  ;; thread => normal shimbun folder
	  (mew-summary-unmark-in-physical fld msg))))))

;;; Message-ID database:
(defun mew-shimbun-db-setup (fld)
  (setq mew-shimbun-db
	(mew-lisp-load
	 (expand-file-name mew-shimbun-db-file
			   (mew-expand-folder fld)))))

(defun mew-shimbun-db-setup2 (fld id-msgs)
  (mew-shimbun-db-setup fld)
  (setq mew-shimbun-db2 (copy-sequence mew-shimbun-db))
  (dolist (x id-msgs)
    (setq mew-shimbun-db2
	  (delq (assoc (car x) mew-shimbun-db2)
		mew-shimbun-db2))))

(defun mew-shimbun-db-shutdown (fld count)
  (when (> count 0)
    (let ((mew-lisp-max-length (mew-shimbun-db-length fld)))
      (mew-lisp-save
       (expand-file-name mew-shimbun-db-file (mew-expand-folder fld))
       mew-shimbun-db)
      (mew-touch-folder fld)))
  (setq mew-shimbun-db nil))

(defun mew-shimbun-db-shutdown2 (fld count)
  (mew-shimbun-db-shutdown fld count)
  (setq mew-shimbun-db2 nil))

(defun mew-shimbun-db-add-id (id md5 &optional replace)
  (let ((alist (mew-shimbun-db-search-id id)))
    (if (null alist)
	;; new
	(setq mew-shimbun-db (cons (cons id md5) mew-shimbun-db))
      (when replace
	;; replace
	(setq mew-shimbun-db
	      (cons (cons id md5) (delq alist mew-shimbun-db)))))))

(defun mew-shimbun-db-length (fld)
  (cond
   ((null mew-shimbun-groups-db-length)
    mew-lisp-max-length)
   ((numberp mew-shimbun-groups-db-length)
    mew-shimbun-groups-db-length)
   (t
    (catch 'det
      (dolist (x mew-shimbun-groups-db-length)
	(when (and (stringp (car x))
		   (string-match
		    (concat "^" (regexp-quote
				 (concat mew-shimbun-folder "/" (car x))))
		    fld))
	  (throw 'det (cdr x))))
      (or (cdr (assq t mew-shimbun-groups-db-length))
	  mew-lisp-max-length)))))

(luna-define-class shimbun-mew-mua (shimbun-mua) ())

;;; Misc
(defun mew-shimbun-md5 ()
  "Calculate MD5 with boundary remove."
  (let ((str (mew-buffer-substring
	      (point-min)
	      (min (point-max) (+ (point-min) 6144)))) ;; (* 4096 1.5)
	(case-fold-search nil))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward "===shimbun_[0-9]+_[0-9]+_[0-9]+===" nil t)
	(replace-match ""))
      (mew-md5
       (string-as-unibyte
	(mew-buffer-substring (point-min)
			      (min (point-max) (+ (point-min) 4096))))))))

(defun mew-shimbun-folder-new-p (fld)
  (let* ((dir (file-chase-links (mew-expand-folder fld)))
	 (tdir (if mew-touch-folder-p
		   (mew-file-get-time
		    (expand-file-name mew-summary-touch-file
				      (mew-expand-folder dir)))
		 (mew-file-get-time dir)))
	 (cache (expand-file-name mew-summary-cache-file dir))
	 (tcache (mew-file-get-time cache)))
    (cond
     ((null tdir) nil)
     ((null tcache) t) ;; do update
     ((> (nth 0 tdir) (nth 0 tcache)) t)
     ((= (nth 0 tdir) (nth 0 tcache))
      (if (> (nth 1 tdir) (nth 1 tcache)) t nil))
     (t nil))))

;;; for debug
(defun mew-shimbun-all-unseen ()
  (interactive)
  (when (mew-summary-exclusive-p)
    (mew-summary-only
     (let ((fld (mew-summary-folder-name)))
       (if (not (mew-shimbun-folder-p fld))
	   (message "Can not exec here")
	 (mew-decode-syntax-delete)
	 (save-excursion
	   (dolist (file
		    (directory-files (mew-expand-folder fld) t "^[1-9][0-9]*"))
	     (with-temp-buffer
	       (mew-frwlet
		mew-cs-text-for-read mew-cs-text-for-write
		(insert-file-contents file nil)
		(goto-char (point-min))
		(unless (search-forward "X-Shimbun-Status: unseen\n" nil t)
		  (insert "X-Shimbun-Status: unseen\n")
		  (write-region (point-min) (point-max) file nil 'nomsg)
		  (message "%s done" file)))))
	   (goto-char (point-min))
	   (while (re-search-forward mew-regex-msg nil t)
	     (mew-mark-put-here mew-shimbun-mark-unseen))))))))

;;; Unseen enable
(defun mew-shimbun-unseen-setup ()
  "`Shimbun unseen mark' support function."
  (interactive)
  (when mew-shimbun-use-unseen
    (unless (member "X-Shimbun-Status:" mew-scan-fields)
      (setq mew-scan-fields (append mew-scan-fields (list "X-Shimbun-Status:")))
      (setq mew-scan-fields-alias (append mew-scan-fields-alias (list "SHIMBUN-STS")))

      (mew-scan-setup)

      (defun mew-scan-form-mark ()
	"A function to return a mark.
'mew-scan-form-mark-delete', 'mew-scan-form-mark-review'
and 'X-Shimbun-Status:' effect to this function."
	(let ((id (MEW-ID))
	      (unseen (MEW-SHIMBUN-STS))
	      duplicated review)
	  (when mew-scan-form-mark-delete
	    (when (string-match mew-regex-id id)
	      (setq id (mew-match 1 id))
	      (if (member id (mew-sinfo-get-scan-id)) ;; in Summary mode
		  (setq duplicated t)
		(mew-sinfo-set-scan-id (cons id (mew-sinfo-get-scan-id))))))
	  (when mew-scan-form-mark-review
	    (let* ((mew-header-max-depth nil)
		   (to (mew-addrstr-parse-address-list (MEW-TO))))
	      (catch 'loop
		(dolist (x to)
		  (if (mew-is-my-address mew-regex-my-address-list x)
		      (throw 'loop (setq review t)))))))
	  (cond
	   ((and (mew-shimbun-folder-p (MEW-FLD)) (string= unseen "unseen"))
	    (string mew-shimbun-mark-unseen))
	   (duplicated (string mew-mark-delete))
	   (review     (string mew-mark-review))
	   (t " "))))

      (defadvice mew-summary-cursor-postscript (before shimbun-unseen activate)
	(mew-shimbun-remove-unseen-advice))

      (when mew-shimbun-use-unseen-cache-save
	;; "C-cC-q" 
	(defadvice mew-kill-buffer (before shimbun-cache-save activate)
	  (let* ((buf (or buf (current-buffer)))
		 (fld (if (bufferp buf) (buffer-name buf) buf)))
	    (when (and (get-buffer buf) (mew-shimbun-folder-p fld))
	      (save-excursion
		(set-buffer buf)
		(unless (mew-summary-folder-dir-newp)
		  (mew-summary-folder-cache-save))))))

	;; "Q" or exit Emacs
	(defadvice mew-mark-clean-up (before shimbun-cache-save activate)
	  (save-excursion
	    (dolist (fld mew-buffers)
	      (when (and (get-buffer fld) (mew-shimbun-folder-p fld))
		(set-buffer fld)
		(unless (mew-summary-folder-dir-newp)
		  (mew-summary-folder-cache-save))))))
	))))

;;; unseen setup
(when mew-shimbun-use-unseen
  (mew-shimbun-unseen-setup))

(provide 'mew-shimbun)
;;; mew-shimbun.el ends here.

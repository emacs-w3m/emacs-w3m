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
;; This package is SHIMBUN interface for Mew ver.2.

;;; Instalation:
;; Simply load this file and add followings in your ~/.mew file.
;;
;; (require 'mew-shimbun)
;; (define-key mew-summary-mode-map "G"  (make-sparse-keymap))
;; (define-key mew-summary-mode-map "Gg" 'mew-shimbun-goto-folder)
;; (define-key mew-summary-mode-map "Gi" 'mew-shimbun-retrieve)
;; (define-key mew-summary-mode-map "GI" 'mew-shimbun-retrieve-all)))
;;

;;; Code:
(eval-when-compile
  (require 'cl))

(eval-when-compile
  (unless (dolist (var nil t))
    (load "cl-macs" nil t)))

(eval-and-compile
  (require 'shimbun)
  (require 'mew))

(defcustom mew-shimbun-groups
  '(("mew/mew-dist" . last)
    ("tcup/meadow" . last)
    ("asahi/international" . 2)
    ("asahi-html/sports" . 2))
  "*List of shimbun group names and their parameters."
  :group 'shimbun
  :group 'mew
  :type '(repeat
	  (cons (string :tag "Group")
		(choice :tag "Index checking range"
			(const all)
			(const last)
			(integer :tag "pages")))))

(defcustom mew-shimbun-folder "+shimbun"
  "*The folder where 'shimbun' are contained."
  :group 'shimbun
  :group 'mew
  :type 'string)

(defcustom mew-shimbun-db-file ".mew-shimbun-db"
  "*File name of mew-shimbun database."
  :group 'shimbun
  :group 'mew
  :type 'file)

(defcustom mew-shimbun-before-retrieve-all-hook nil
  "*Hook run before mew-shimbun-retrieve-all called."
  :group 'shimbun
  :group 'mew
  :type 'hook)

(defcustom mew-shimbun-retrieve-all-hook nil
  "*Hook run after mew-shimbun-retrieve-all called."
  :group 'shimbun
  :group 'mew
  :type 'hook)

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
(defvar mew-shimbun-db nil)
(defvar mew-shimbun-input-hist nil)


;;; Macro

(defmacro mew-shimbun-db-search-id (id)
  `(if (member ,id mew-shimbun-db) t nil))

;;; Main:

;;;###autoload
(defun mew-shimbun-goto-folder (&optional args)
  "Goto folder for 'shimbun'.
If executed with '\\[universal-argument]', goto folder to have few new messages."
  (interactive "P")
  (let ((flds mew-folder-list)
	(regex (mew-folder-regex (concat mew-shimbun-folder "/")))
	sbflds alst fld)
    (while flds
      (when (and (string-match regex (car flds))
		 (file-exists-p
		  (expand-file-name mew-shimbun-db-file
				    (mew-expand-folder (car flds)))))
	(if (mew-shimbun-folder-new-p (car flds))
	    (setq sbflds (cons (concat (car flds) "*") sbflds))
	  (if (null args)
	      (setq sbflds (cons (car flds) sbflds)))))
      (setq flds (cdr flds)))
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
    (when (string-match "\\*$" fld)
      (setq fld (substring fld 0 (match-beginning 0)))
      (setcar mew-shimbun-input-hist fld))
    (setq mew-input-folder-hist (cons fld mew-input-folder-hist))
    (mew-summary-ls (mew-summary-switch-to-folder fld))))

;;;###autoload
(defun mew-shimbun-retrieve ()
  "Retrieve articles via 'shimbun' on this folder."
  (interactive)
  (when (mew-summary-exclusive-p)
    (mew-summary-only
     (let ((fld (mew-summary-folder-name))
	   lst shimbun server group range)
       (if (not (string-match (mew-folder-regex (concat mew-shimbun-folder "/")) fld))
	   (message "This command can not execute here")
	 (setq lst (assoc (substring fld (match-end 0)) mew-shimbun-groups))
	 (if (or (not lst) (not (string-match "\\([^/]+\\)\\/" (car lst))))
	     (message "%s is not include 'mew-shimbun-groups'" fld)
	   (run-hooks 'mew-shimbun-before-retrieve-hook)
	   (setq server (mew-match 1 (car lst)))
	   (setq group (substring (car lst) (match-end 0)))
	   (setq range (cdr lst))
	   ;;
	   (mew-window-configure 'summary)
	   (mew-current-set nil nil nil)
	   (mew-decode-syntax-delete)
	   (unless (mew-sinfo-get-scan-form)
	     (mew-sinfo-set-scan-form (mew-summary-scan-form fld)))
	   (mew-summary-goto-folder nil fld)
	   (mew-rendezvous mew-summary-buffer-process)
	   ;; msgs
	   (when (> (mew-shimbun-retrieve-article
		     (luna-make-entity 'shimbun-mew-mua)
		     server group range fld 'scan) 0)
	     (mew-summary-folder-cache-save))
	   (run-hooks 'mew-shimbun-retrieve-hook)))))))

;;;###autoload
(defun mew-shimbun-retrieve-all ()
  "Retrieve all articles via 'shimbun'."
  (interactive)
  (let ((mua (luna-make-entity 'shimbun-mew-mua))
	alist)
    (run-hooks 'mew-shimbun-before-retrieve-all-hook)
    (dolist (elem (reverse mew-shimbun-groups))
      (when (string-match "\\`\\([^/]+\\)\\/" (car elem))
	(let* ((server (mew-match 1 (car elem)))
	       (group (substring (car elem) (match-end 0)))
	       (range (cdr elem))
	       (x (assoc server alist)))
	  (if x
	      (unless (assoc group (cdr x))
		(setcdr x (cons (cons group range) (cdr x))))
	    (push (list server (cons group range)) alist)))))
    (dolist (elem alist)
      (dolist (pair (cdr elem))
	(mew-shimbun-retrieve-article
	 mua (car elem) (car pair) (cdr pair))))
    (run-hooks 'mew-shimbun-retrieve-all-hook)
    (message "Getting all shimbun articles done")))
  
(defun mew-shimbun-retrieve-article (mua server group range &optional folder scan)
  "Retrieve articles via 'shimbun'."
  (let ((shimbun (shimbun-open server mua))
	(newmsgs 0)
	(buf (get-buffer-create mew-shimbun-article-buffer-name))
	msg file)
    (shimbun-open-group shimbun group)
    (unless folder
      (setq folder (concat mew-shimbun-folder "/" server "/" group)))
    (unless (file-exists-p (mew-expand-folder folder))
      (mew-make-directory (mew-expand-folder folder)))
    (mew-shimbun-db-setup folder)
    (unwind-protect
	(dolist (head (shimbun-headers shimbun range))
	  (let ((id (format mew-shimbun-id-format
			    server group
			    (shimbun-header-id head))))
	    (unless (mew-shimbun-db-search-id id)
	      (with-current-buffer buf
		(mew-erase-buffer)
		(shimbun-article shimbun head)
		(when (and (> (buffer-size) 0)
			   (mew-shimbun-db-add-id id))
		  (setq newmsgs (1+ newmsgs))
		  (goto-char (point-min))
		  (insert (format "X-Shimbun-Id: %s\n" id))
		  ;; (mew-shimbun-sanity-convert)
		  (setq msg (mew-folder-new-message folder 'numonly))
		  (setq file (mew-expand-folder folder msg))
		  (mew-frwlet 
		   mew-cs-dummy mew-cs-text-for-write
		   (write-region (point-min) (point-max) file nil 'nomsg))
		  (set-file-modes file mew-file-mode)
		  (when scan (mew-shimbun-scan folder msg)))))))
      (kill-buffer buf)
      (mew-folder-insert folder)
      (shimbun-close-group shimbun)
      (shimbun-close shimbun)
      (mew-shimbun-db-shutdown folder newmsgs))
    (message "Getting %s %s in '%s' done"
	     (if (= newmsgs 0) "no" (number-to-string newmsgs))
	     (if (> newmsgs 1) "messages" "message")
	     folder)
    newmsgs))

;; XXXXX Not implement yet
;; (defun mew-shimbun-sanity-convert ()
;;   ())

(defun mew-shimbun-scan (folder msg)
  (let ((width (1- (mew-scan-width)))
	(vec (mew-pop-scan-header)))
    (mew-scan-set-folder vec folder)
    (mew-scan-set-message vec msg)
    (mew-scan-insert-line folder vec width msg nil)))


;;; Message-ID database:

(defun mew-shimbun-db-setup (folder)
  (setq mew-shimbun-db
	(mew-lisp-load
	 (expand-file-name mew-shimbun-db-file
			   (mew-expand-folder folder)))))

(defun mew-shimbun-db-shutdown (folder newmsgs)
  (when (> newmsgs 0)
    (mew-lisp-save
     (expand-file-name mew-shimbun-db-file
		       (mew-expand-folder folder))
     mew-shimbun-db)
    (mew-touch-folder folder))
  (setq mew-shimbun-db nil))

(defun mew-shimbun-db-add-id (id)
  (unless (mew-shimbun-db-search-id id)
    (setq mew-shimbun-db (cons id mew-shimbun-db))))

(luna-define-class shimbun-mew-mua (shimbun-mua) ())

(luna-define-method shimbun-mua-search-id ((mua shimbun-mew-mua) id)
  (let ((shimbun (shimbun-mua-shimbun-internal mua)))
    (mew-shimbun-db-search-id
     (format mew-shimbun-id-format
	     (shimbun-server-internal shimbun)
	     (shimbun-current-group-internal shimbun)
	     id))))

;;; Misc

(defun mew-shimbun-folder-new-p (folder)
  (let* ((dir (file-chase-links (mew-expand-folder folder)))
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

(provide 'mew-shimbun)
;;; mew-shimbun.el ends here.

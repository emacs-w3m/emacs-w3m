;;; w3m-namazu.el --- The add-on program to search files with Namazu.

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: w3m, WWW, hypermedia, namazu

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

;; w3m-namazu.el is the add-on program of emacs-w3m to search files
;; with Namazu.  For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/


;;; History:

;; Original program was posted by
;; Takayuki Arakawa <takayu@pop02.odn.ne.jp> in [emacs-w3m:01340] at
;; Jul 31, 2001.

;; Many codes are imported from namazu.el written by
;; Yukihiro Matsumoto <matz@netlab.co.jp> et al.

;; All stuffs are rewritten by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org> at Aug 2, 2001.


;;; Code:
(require 'w3m)

(defgroup w3m-namazu nil
  "w3m-namazu front-end for Emacs."
  :group 'w3m
  :prefix "w3m-namazu-")

(defcustom w3m-namazu-command "namazu"
  "*Name of the executable file of Namazu."
  :group 'w3m-namazu
  :type 'string)

(defcustom w3m-namazu-arguments
  '("-h"			; print in HTML format.
    "-H"			; print further result links.
    "-n" w3m-namazu-page-max	; set number of documents shown to NUM.
    "-w" whence)		; set first number of documents shown to NUM.
  "*Arguments of Namazu."
  :group 'w3m-namazu
  :type '(repeat
	  (restricted-sexp :tag "Argument"
			   :match-alternatives
			   (stringp 'w3m-namazu-page-max 'whence))))

(defcustom w3m-namazu-page-max
  (if (boundp 'namazu-search-num)
      (symbol-value 'namazu-search-num)
    30)
  "*A maximum number of documents which are retrieved by one-time search."
  :group 'w3m-namazu
  :type 'integer)

(defcustom w3m-namazu-index-alist
  (when (boundp 'namazu-dir-alist)
    (symbol-value 'namazu-dir-alist))
  "*Alist of alias and index directories."
  :group 'w3m-namazu
  :type '(repeat
	  (cons :format "%v"
		(string :tag "Alias")
		(string :tag "Index path"))))

(defcustom w3m-namazu-default-index
  (unless (and (boundp 'namazu-always-query-index-directory)
	       (symbol-value 'namazu-always-query-index-directory))
    (when (boundp 'namazu-default-dir)
      (symbol-value 'namazu-default-dir)))
  "*Alias or directory of the default index.
If this variable equals nil, it is required to input an index path
whenever `w3m-namazu' is called interactively without prefix
argument."
  :group 'w3m-namazu
  :type '(choice
	  (const :tag "No default index" nil)
	  (string :tag "Default index path")))

(defcustom w3m-namazu-output-coding-system
  (if (boundp 'namazu-cs-write)
      (symbol-value 'namazu-cs-write)
    (if (memq system-type '(OS/2 emx windows-nt))
	'shift_jis-dos
      'euc-japan-unix))
  "*Coding system for namazu process."
  :group 'w3m-namazu
  :type 'coding-system)

(defcustom w3m-namazu-input-coding-system
  (if (boundp 'namazu-cs-read)
      (symbol-value 'namazu-cs-read)
    'undecided)
  "*Coding system for namazu process."
  :group 'w3m-namazu
  :type 'coding-system)


(defsubst w3m-namazu-call-process (index query &optional whence)
  (when (assoc index w3m-namazu-index-alist)
    (setq index (cdr (assoc index w3m-namazu-index-alist))))
  (setq index (mapcar 'expand-file-name (split-string index)))
  (unless whence
    (setq whence "0"))
  (let ((file-name-coding-system w3m-file-name-coding-system)
	(coding-system-for-read w3m-namazu-input-coding-system)
	(coding-system-for-write w3m-namazu-output-coding-system)
	(default-process-coding-system
	  (cons w3m-namazu-input-coding-system
		w3m-namazu-output-coding-system)))
    (apply 'call-process w3m-namazu-command nil t nil
	   (let ((w3m-namazu-page-max
		  (number-to-string w3m-namazu-page-max)))
	     (nconc (mapcar 'eval w3m-namazu-arguments)
		    (list query)
		    index)))))

;;;###autoload
(defun w3m-about-namazu (url &optional no-decode no-cache &rest args)
  (let (index query whence)
    (when (string-match "^about://namazu/\\?" url)
      (dolist (s (split-string (substring url (match-end 0)) "&"))
	(when (string-match "^\\(index\\|\\(query\\)\\|\\(whence\\)\\)=" s)
	  (set (cond
		((match-beginning 2) 'query)
		((match-beginning 3) 'whence)
		(t 'index))
	       (substring s (match-end 0)))))
      (w3m-with-work-buffer
	(delete-region (point-min) (point-max))
	(set-buffer-multibyte t)
	(when (zerop (w3m-namazu-call-process index query whence))
	  (let ((case-fold-search t))
	    (goto-char (point-min))
	    (while (search-forward "<a href=\"/" nil t)
	      (forward-char -1)
	      (insert "file://"))
	    (goto-char (point-min))
	    (while (search-forward "<a href=\"?&whence=" nil t)
	      (forward-char -8)
	      (delete-char -1)
	      (insert (format "about://namazu/?index=%s&query=%s" index query))))
	  "text/html")))))

(defun w3m-namazu-complete-index (index predicate flag)
  "Function to complete index name"
  (if (eq flag 'lambda)
      (or (and (assoc index w3m-namazu-index-alist) t)
	  (file-directory-p index))
    (let ((alist
	   (if (string-match "^[~/\\.]" index)
	       (delq nil
		     (mapcar
		      (lambda (f)
			(and (not (string-match "/\\.\\.?$" f))
			     (file-directory-p f)
			     (cons (file-name-as-directory f) nil)))
		      (directory-files (file-name-directory
					(setq index (expand-file-name index)))
				       t)))
	     (mapcar
	      (lambda (x) (cons x nil))
	      (all-completions index w3m-namazu-index-alist)))))
      (cond
       ((not flag) (try-completion index alist predicate))
       ((eq flag t) (all-completions index alist predicate))))))

(defvar w3m-namazu-index-history nil)
(defvar w3m-namazu-query-history nil)

;;;###autoload
(defun w3m-namazu (index query)
  "Search indexed files with Namazu."
  (interactive
   (list
    (if (if w3m-namazu-default-index
	    current-prefix-arg
	  (not (and current-prefix-arg
		    w3m-namazu-index-history)))
	(let* ((default (or (car w3m-namazu-index-history)
			    w3m-namazu-default-index))
	       (s (completing-read
		   (if default
		       (format "Index (default %s): " default)
		     "Index: ")
		   'w3m-namazu-complete-index nil t nil
		   'w3m-namazu-index-history)))
	  (if (string= s "") default s))
      (or w3m-namazu-default-index
	  (car w3m-namazu-index-history)))
    (read-string "Query: " nil 'w3m-namazu-query-history)))
  (w3m-goto-url (format "about://namazu/?index=%s&query=%s&whence=0"
			index query)))

(provide 'w3m-namazu)
;;; w3m-namazu.el ends here

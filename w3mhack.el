;;; w3mhack.el --- a hack to setup the environment for building w3m

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: w3m, WWW, hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'cl)
(unless (dolist (var nil t))
  ;; Override the macro `dolist' which may have been defined in egg.el.
  (load "cl-macs" nil t))

;; Add supplementary directories to `load-path'.
(let ((addpath (or (pop command-line-args-left) "NONE"))
      path paths)
  (while (string-match "\\([^\0-\37:]+\\)[\0-\37:]*" addpath)
    (setq path (file-name-as-directory
		(expand-file-name (substring addpath
					     (match-beginning 1)
					     (match-end 1))))
	  addpath (substring addpath (match-end 0)))
    (when (file-directory-p path)
      (push path paths)))
  (unless (null paths)
    (setq load-path (nconc (nreverse paths) load-path))))

;; Needed for interdependencies between w3m modules.
(push default-directory load-path)

(defun w3mhack-examine-modules ()
  "Examine w3m modules should be byte-compile'd."
  (let ((modules (directory-files default-directory nil "\\.el$"))
	(deletes (cond ((featurep 'xemacs)
			'("w3m-e21.el" "w3m-om.el"))
		       ((and (boundp 'emacs-major-version)
			     (>= emacs-major-version 21))
			'("w3m-om.el" "w3m-xmas.el"))
		       ((and (boundp 'emacs-major-version)
			     (= emacs-major-version 20))
			'("w3m-e21.el" "w3m-om.el" "w3m-xmas.el"))
		       ((boundp 'MULE)
			'("w3m-e21.el" "w3m-xmas.el"))))
	(mime (locate-library "mime")))
    (push "w3mhack.el" deletes)
    (unless (and mime
		 ;; It is very unlikely that Umerin's mime exists.
		 (string-match "/flim/$" (file-name-directory mime)))
      (push "mime-w3m.el" deletes))
    (unless (locate-library "mew")
      (push "mew-w3m.el" deletes))
    (dolist (module modules)
      (unless (member module deletes)
	(princ (format "%sc " module))))))

;; Byte optimizers.
(require 'bytecomp)

(put 'truncate-string 'byte-optimizer
     (lambda (form)
       (if (fboundp 'truncate-string-to-width)
	   (cons 'truncate-string-to-width (cdr form))
	 form)))

(cond
 ((featurep 'xemacs)
  (setq byte-compile-warnings
	(delq 'unused-vars (copy-sequence byte-compile-default-warnings))))
 ((boundp 'MULE)
  ;; Bind defcustom'ed variables.
  (put 'custom-declare-variable 'byte-hunk-handler
       (lambda (form)
	 (if (memq 'free-vars byte-compile-warnings)
	     (setq byte-compile-bound-variables
		   (cons (nth 1 (nth 1 form)) byte-compile-bound-variables)))
	 form))

  ;; Make `locate-library' run quietly at run-time.
  (put 'locate-library 'byte-optimizer
       (lambda (form)
	 (` (let ((fn (function locate-library))
		  (msg (symbol-function 'message)))
	      (fset 'message (function ignore))
	      (unwind-protect
		  (, (append '(funcall fn) (cdr form)))
		(fset 'message msg))))))
  (let (current-load-list)
    ;; Mainly for the compile-time.
    (defun locate-library (library &optional nosuffix)
      "Show the full path name of Emacs library LIBRARY.
This command searches the directories in `load-path' like `M-x load-library'
to find the file that `M-x load-library RET LIBRARY RET' would load.
Optional second arg NOSUFFIX non-nil means don't add suffixes `.elc' or `.el'
to the specified name LIBRARY (a la calling `load' instead of `load-library')."
      (interactive "sLocate library: ")
      (catch 'answer
	(mapcar
	 '(lambda (dir)
	    (mapcar
	     '(lambda (suf)
		(let ((try (expand-file-name (concat library suf) dir)))
		  (and (file-readable-p try)
		       (null (file-directory-p try))
		       (progn
			 (or noninteractive
			     (message "Library is file %s" try))
			 (throw 'answer try)))))
	     (if nosuffix '("") '(".elc" ".el" ""))))
	 load-path)
	(or noninteractive
	    (message "No library %s in search path" library))
	nil))
    (byte-compile 'locate-library))))

(require 'w3m)

(defun w3mhack-version ()
  "Print version of w3m.el."
  (princ emacs-w3m-version))

;;; w3mhack.el ends here

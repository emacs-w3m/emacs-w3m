;;; w3mhack.el --- a hack to setup the environment for building w3m

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
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

(defconst shimbun-module-directory "shimbun")

(defconst w3mhack-colon-keywords-file "w3m-kwds.el")

;; Needed for interdependencies among w3m and shimbun modules.
(push default-directory load-path)
(push (expand-file-name shimbun-module-directory default-directory) load-path)

(defun w3mhack-examine-modules ()
  "Examine w3m modules should be byte-compile'd."
  (let* ((modules (directory-files default-directory nil "^[^#]+\\.el$"))
	 (version-specific-modules '("w3m-e20.el" "w3m-e21.el"
				     "w3m-om.el" "w3m-xmas.el"))
	 (ignores (delete (cond
			   ((featurep 'xemacs)
			    (push "w3m-fsf.el" version-specific-modules)
			    "w3m-xmas.el")
			   ((boundp 'MULE)
			    (push "w3m-fsf.el" version-specific-modules)
			    "w3m-om.el")
			   ((boundp 'emacs-major-version)
			    (if (>= emacs-major-version 21)
				"w3m-e21.el"
			      "w3m-e20.el")))
			  (append version-specific-modules
				  (list "w3mhack.el"
					w3mhack-colon-keywords-file))))
	 (shimbun-dir (file-name-as-directory shimbun-module-directory))
	 print-level print-length)
    (unless (locate-library "mew")
      (push "mew-w3m.el" ignores))
    (unless (and (boundp 'emacs-major-version)
		 (featurep 'mule)
		 (if (featurep 'xemacs)
		     ;; Mule-UCS does not support XEmacs versions prior
		     ;; to 21.2.37.
		     (and (>= emacs-major-version 21)
			  (or (> emacs-minor-version 2)
			      (and (= emacs-major-version 2)
				   (>= emacs-beta-version 37))))
		   (>= emacs-major-version 20))
		 (locate-library "un-define"))
      (push "w3m-ucs.el" ignores))
    (if (and (featurep 'mule)
	     (locate-library "mime-def"))
	(progn
	  ;; Add shimbun modules.
	  (dolist (file (directory-files (expand-file-name shimbun-dir)
					 nil "^[^#]+\\.el$"))
	    (setq modules (nconc modules (list (concat shimbun-dir file)))))
	  ;; mew-shimbun check
	  (unless (locate-library "mew")
	    (push (concat shimbun-dir "mew-shimbun.el") ignores)))
      (push "mime-w3m.el" ignores)
      (push "octet.el" ignores))
    (unless (featurep 'mule)
      (push "w3m-weather.el" ignores))
    (if (and (boundp 'emacs-major-version)
	     (not (featurep 'xemacs))
	     (<= emacs-major-version 20)
	     (locate-library "bitmap"))
	;; Against the error "Already defined charset: 242".
	(when (locate-library "un-define")
	  (setq bitmap-alterable-charset 'tibetan-1-column)
	  (setq bitmap-use-alterable-charset-anyway t)
	  (require 'bitmap)
	  (require 'un-define))
      (push "w3m-bitmap.el" ignores))
    ;; To byte-compile w3m-util.el and a version specific module first.
    (princ "w3m-util.elc ")
    (setq modules (delete "w3m-util.el" modules))
    (princ "w3m-proc.elc ")
    (setq modules (delete "w3m-proc.el" modules))
    (dolist (module version-specific-modules)
      (when (and (not (member module ignores))
		 (member module modules))
	(setq modules (delete module modules))
	(princ (format "%sc " module))))
    (dolist (module modules)
      (unless (member module ignores)
	(princ (format "%sc " module))))))

(require 'bytecomp)

(defun w3mhack-compile ()
  "Byte-compile the w3m modules."
  (let (modules)
    (let* ((buffer (generate-new-buffer " *modules*"))
	   (standard-output buffer)
	   elc el)
      (w3mhack-examine-modules)
      (save-excursion
	(set-buffer buffer)
	(goto-char (point-min))
	(while (re-search-forward "\\([^ ]+\\.el\\)c" nil t)
	  (setq elc (buffer-substring (match-beginning 0) (match-end 0))
		el (buffer-substring (match-beginning 1) (match-end 1)))
	  (if (file-exists-p elc)
	      (if (file-newer-than-file-p elc el)
		  (message " `%s' is up to date" elc)
		(delete-file elc)
		(setq modules (cons el modules)))
	    (setq modules (cons el modules)))))
      (kill-buffer buffer))
    (setq modules (nreverse modules))
    (while modules
      (condition-case nil
	  (byte-compile-file (car modules))
	(error))
      (setq modules (cdr modules)))))

;; Byte optimizers and version specific functions.
(put 'truncate-string 'byte-optimizer
     (lambda (form)
       (if (fboundp 'truncate-string-to-width)
	   (cons 'truncate-string-to-width (cdr form))
	 form)))

(put 'match-string-no-properties 'byte-optimizer
     (lambda (form)
       (let ((num (nth 1 form))
	     (string (nth 2 form)))
	 (cond ((and string (featurep 'xemacs))
		(` (let ((num (, num)))
		     (if (match-beginning num)
			 (let ((string (substring (, string)
						  (match-beginning num)
						  (match-end num))))
			   (map-extents (lambda (extent maparg)
					  (delete-extent extent))
					string 0 (lenght string))
			   string)))))
	       (string
		(` (let ((num (, num)))
		     (if (match-beginning num)
			 (let ((string (substring (, string)
						  (match-beginning num)
						  (match-end num))))
			   (set-text-properties 0 (length string) nil string)
			   string)))))
	       (t
		(` (let ((num (, num)))
		     (if (match-beginning num)
			 (buffer-substring-no-properties
			  (match-beginning num) (match-end num))))))))))

(cond
 ((featurep 'xemacs)
  ;; Don't warn for the unused non-global variables.
  (setq byte-compile-warnings
	(delq 'unused-vars (copy-sequence byte-compile-default-warnings)))

  (defun w3mhack-byte-optimize-letX (form)
    "Byte optimize `let' or `let*' FORM in the source level
to remove some obsolete variables in the first argument VARLIST.

Examples of the optimization:

;;From
  (let ((coding-system-for-read 'binary)
	(file-coding-system-for-read *noconv*))
    (insert-file-contents FILE))
;;To
  (let ((coding-system-for-read 'binary))
    (insert-file-contents FILE))

;;From
  (let* ((codesys 'utf-8)
	 (file-coding-system codesys)
	 (coding-system-for-write file-coding-system))
    (save-buffer))
;;To
  (let* ((codesys 'utf-8)
	 (coding-system-for-write codesys))
    (save-buffer))
"
    (let ((obsoletes '(file-coding-system
		       file-coding-system-for-read
		       pop-up-frame-alist))
	  (varlist (copy-sequence (cadr form)))
	  obsolete elements element value)
      (while (setq obsolete (pop obsoletes))
	(setq elements varlist
	      varlist nil)
	(while (setq element (pop elements))
	  (if (or (prog1
		      (eq obsolete element)
		    (setq value nil))
		  (when (eq obsolete (car-safe element))
		    (setq value (unless (eq obsolete (cadr element))
				  (cadr element)))
		    t))
	      (when (eq 'let* (car form))
		(while (setq element (rassoc (list obsolete) elements))
		  (setcdr element (list value))))
	    (push element varlist)))
	(setq varlist (nreverse varlist)))
      (setcar (cdr form) varlist))
    form)

  (require 'byte-optimize)
  (defadvice byte-optimize-form-code-walker
    (before w3mhack-byte-optimize-letX activate compile)
    "Byte optimize `let' or `let*' FORM in the source level
to remove some obsolete variables in the first argument VARLIST."
    (when (memq (car-safe (ad-get-arg 0)) '(let let*))
      (ad-set-arg 0 (w3mhack-byte-optimize-letX (ad-get-arg 0)))))

  ;; We dare to do it even though it might be a futile work, since the
  ;; doc-string says "You should NEVER use this function".  If the
  ;; function `set-text-properties' is used for a whole string, it
  ;; will make the program run a little bit faster.
  (put 'set-text-properties 'byte-optimizer
       (lambda (form)
	 (let ((start (nth 1 form))
	       (end (nth 2 form))
	       (props (nth 3 form))
	       (string (nth 4 form)))
	   (if (and string
		    (zerop start)
		    (eq 'length (car-safe end))
		    (eq string (car-safe (cdr-safe end))))
	       (if props
		   (` (let ((end (, end)))
			(map-extents (lambda (extent maparg)
				       (delete-extent extent))
				     (, string) 0 end)
			(add-text-properties 0 end (, props) (, string))))
		 (` (map-extents (lambda (extent maparg)
				   (delete-extent extent))
				 (, string) 0 (, end))))
	     form))))

  (defun w3mhack-make-package ()
    "Make some files in the XEmacs package directory."
    (let* ((package-dir (pop command-line-args-left))
	   (lisp-dir (expand-file-name "lisp/w3m/" package-dir))
	   (custom-load (expand-file-name "custom-load.el" lisp-dir))
	   (generated-autoload-file (expand-file-name "auto-autoloads.el"
						      lisp-dir))
	   (els (nconc (directory-files default-directory nil "^[^#]+\\.el$")
		       (directory-files (expand-file-name
					 shimbun-module-directory)
					nil "^[^#]+\\.el$")))
	   (elcs (with-temp-buffer
		   (let ((standard-output (current-buffer)))
		     (w3mhack-examine-modules)
		     (split-string (buffer-string) " \\(shimbun/\\)?"))))
	   (icons (directory-files (expand-file-name "icons/") nil
				   "^[^#]+\\.xpm$"))
	   (si:message (symbol-function 'message))
	   manifest make-backup-files noninteractive)
      (when (file-exists-p custom-load)
	(delete-file custom-load))
      (when (file-exists-p (concat custom-load "c"))
	(delete-file (concat custom-load "c")))
      (with-temp-buffer
	(let ((standard-output (current-buffer)))
	  (Custom-make-dependencies lisp-dir))
	;; Print messages into stderr.
	(message "%s" (buffer-string)))
      (when (file-exists-p custom-load)
	(require 'cus-load)
	(byte-compile-file custom-load)
	(push "custom-load.el" els)
	(push "custom-load.elc" elcs))
      (message "Updating autoloads for the directory %s..." lisp-dir)
      (when (file-exists-p generated-autoload-file)
	(delete-file generated-autoload-file))
      (when (file-exists-p (concat generated-autoload-file "c"))
	(delete-file (concat generated-autoload-file "c")))
      (defun message (fmt &rest args)
	"Ignore useless messages while generating autoloads."
	(cond ((and (string-equal "Generating autoloads for %s..." fmt)
		    (file-exists-p (file-name-nondirectory (car args))))
	       (funcall si:message
			fmt (file-name-nondirectory (car args))))
	      ((string-equal "No autoloads found in %s" fmt))
	      ((string-equal "Generating autoloads for %s...done" fmt))
	      (t (apply si:message fmt args))))
      (unwind-protect
	  (update-autoloads-from-directory lisp-dir)
	(fset 'message si:message))
      (when (file-exists-p generated-autoload-file)
	(byte-compile-file generated-autoload-file)
	(push "auto-autoloads.el" els)
	(push "auto-autoloads.elc" elcs))
      (when (file-directory-p (expand-file-name "pkginfo/" package-dir))
	(setq manifest (expand-file-name "pkginfo/MANIFEST.w3m" package-dir))
	(message "Generating %s..." manifest)
	(with-temp-file manifest
	  (insert "pkginfo/MANIFEST.w3m\n")
	  (dolist (log (directory-files lisp-dir nil
					"^ChangeLog\\(\\.[0-9]+\\)?$"))
	    (insert "lisp/w3m/" log "\n"))
	  (dolist (el els)
	    (insert "lisp/w3m/" el "\n")
	    (when (member (concat el "c") elcs)
	      (insert "lisp/w3m/" el "c\n")))
	  (dolist (icon icons)
	    (insert "etc/w3m/" icon "\n")))
	(message "Generating %s...done" manifest)))))

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

(defun w3mhack-generate-colon-keywords-file ()
  "Generate a file which contains a list of colon keywords to be bound at
run-time.  The file name is specified by `w3mhack-colon-keywords-file'."
  (let* ((srcdir default-directory)
	 (kwds-file (expand-file-name w3mhack-colon-keywords-file srcdir))
	 (makefile (expand-file-name "Makefile" srcdir))
	 (buffer (get-buffer-create " *colon keywords*"))
	 (dirs '("./" "./shimbun/"))
	 keywords ignores files file directories dir
	 form elem make-backup-files)
    (save-excursion
      (set-buffer buffer)
      (let (buffer-file-format
	    format-alist
	    insert-file-contents-post-hook insert-file-contents-pre-hook
	    jam-zcat-filename-list jka-compr-compression-info-list)
	(unless (and (file-exists-p kwds-file)
		     (file-exists-p makefile)
		     (file-newer-than-file-p kwds-file makefile))
	  (setq
	   ignores
	   '(:symbol-for-testing-whether-colon-keyword-is-available-or-not
	     ;; The following keywords will be bound by CUSTOM.
	     :get :group :initialize :link :load :options :prefix
	     :require :set :tag :type))
	  ;; Add el(c) files in the following list if necessary.
	  ;; Each file should be representative file of a package
	  ;; which will be used together with emacs-w3m.
	  (setq files (list (locate-library "mailcap")
			    (locate-library "mime-def")
			    (locate-library "path-util")
			    (locate-library "poem")))
	  (while files
	    (when (setq file (pop files))
	      (setq dir (file-name-directory file))
	      (unless (member dir dirs)
		(push dir dirs))))
	  (setq directories dirs)
	  (message "Searching for all the colon keywords in:")
	  (while dirs
	    (setq dir (pop dirs))
	    (message " %s" dir)
	    (setq files (directory-files dir t "\\.el\\(\\.gz\\|\\.bz2\\)?$"))
	    (while files
	      (setq file (pop files))
	      (if (string-match "\\(\\.gz$\\)\\|\\.bz2$" file)
		  (let ((temp (expand-file-name "w3mtemp.el" srcdir)))
		    (when
			(let* ((binary (if (boundp 'MULE)
					   '*noconv*
					 'binary))
			       (coding-system-for-read binary)
			       (coding-system-for-write binary)
			       (input-coding-system binary)
			       (output-coding-system binary)
			       (default-process-coding-system
				 (cons binary binary))
			       call-process-hook)
			  (insert-file-contents file nil nil nil t)
			  (when
			      (condition-case code
				  (progn
				    (if (match-beginning 1)
					(call-process-region (point-min)
							     (point-max)
							     "gzip" t buffer
							     nil "-cd")
				      (call-process-region (point-min)
							   (point-max)
							   "bzip2" t buffer
							   nil "-d"))
				    t)
				(error
				 (erase-buffer)
				 (message "In file %s: %s" file code)
				 nil))
			    (write-region (point-min) (point-max) temp nil
					  'silent)
			    t))
		      (unwind-protect
			  (insert-file-contents temp nil nil nil t)
			(delete-file temp))))
		(insert-file-contents file nil nil nil t))
	      (while (setq form (condition-case nil
				    (read buffer)
				  (error nil)))
		(while form
		  (setq elem (pop form))
		  (unless (memq (car-safe elem)
				'(\` backquote defcustom defface defgroup
				  define-widget quote))
		    (while (consp elem)
		      (push (car elem) form)
		      (setq elem (cdr elem)))
		    (when (and elem
			       (symbolp elem)
			       (not (eq ': elem))
			       (eq ?: (aref (symbol-name elem) 0))
			       (not (memq elem ignores))
			       (not (memq elem keywords)))
		      (push elem keywords)))))))
	  (setq keywords (sort keywords
			       (lambda (a b)
				 (string-lessp (symbol-name a)
					       (symbol-name b)))))
	  (erase-buffer)
	  (insert ";;; " w3mhack-colon-keywords-file "\
 --- List of colon keywords which will be bound at run-time

;; This file should be generated by make in emacs-w3m source directory.
;; There are some colon keywords which were found in the directories
;; listed below:
;;
;; "
		  (mapconcat 'identity directories "\n;; ")
		  "\n
\(defvar w3m-colon-keywords)
\(setq w3m-colon-keywords
      '("
		  (mapconcat 'symbol-name keywords "\n\t")
		  "))\n")
	  (write-region (point-min) (point) kwds-file))))
    (kill-buffer buffer)))

(condition-case nil
    (let ((kwds-file (expand-file-name w3mhack-colon-keywords-file)))
      :symbol-for-testing-whether-colon-keyword-is-available-or-not
      (when (file-exists-p kwds-file)
	(delete-file (expand-file-name w3mhack-colon-keywords-file))))
  (void-variable
   (byte-compile 'w3mhack-generate-colon-keywords-file)
   (w3mhack-generate-colon-keywords-file)))

(defun w3mhack-load-path ()
  "Print default value of additional load paths for w3m.el."
  (let (paths x)
    (and (or (featurep 'xemacs)
	     (boundp 'MULE)
	     (locate-library "mime-def"))
	 (setq x (locate-library "poe"))
	 (progn
	   (setq x (file-name-directory x))
	   (if (string-match "/emu/\\'" x)
	       (push (substring x 0 (1+ (match-beginning 0))) paths))
	   (push x paths)))
    (if (setq x (locate-library "mime-def"))
	(push (file-name-directory x) paths))
    (and (boundp 'MULE)
	 (setq x (locate-library "custom"))
	 (push (file-name-directory x) paths))
    (and (boundp 'MULE)
	 (setq x (locate-library "regexp-opt"))
	 (push (file-name-directory x) paths))
    (if (setq x (locate-library "mew"))
	(push (file-name-directory x) paths))
    (and (boundp 'emacs-major-version)
	 (if (featurep 'xemacs)
	     ;; Mule-UCS does not support XEmacs versions prior to 21.2.37.
	     (and (>= emacs-major-version 21)
		  (or (> emacs-minor-version 2)
		      (and (= emacs-major-version 2)
			   (>= emacs-beta-version 37))))
	   (>= emacs-major-version 20))
	 (setq x (locate-library "un-define"))
	 (push (file-name-directory x) paths))
    (and (boundp 'emacs-major-version)
	 (= emacs-major-version 20)
	 (setq x (locate-library "bitmap"))
	 (push (file-name-directory x) paths))
    (let (print-level print-length)
      (princ (mapconcat
	      (function directory-file-name)
	      (nreverse paths) ":")))))

(defun w3mhack-version ()
  "Print version of w3m.el."
  (require 'w3m)
  (let (print-level print-length)
    (princ emacs-w3m-version)))

;;; w3mhack.el ends here

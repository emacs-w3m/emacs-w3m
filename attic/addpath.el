;; This file is used for the make rule `very-slow' to add user
;; specific additional directories and the current source directories
;; to `load-path'.
(let ((addpath (prog1
		   (or (car command-line-args-left)
		       "NONE")
		 (setq command-line-args-left (cdr command-line-args-left))))
      path paths)
  (while (string-match "\\([^\0-\37:]+\\)[\0-\37:]*" addpath)
    (setq path (expand-file-name (substring addpath
					    (match-beginning 1)
					    (match-end 1)))
	  addpath (substring addpath (match-end 0)))
    (if (file-directory-p path)
	(setq paths (cons path paths))))
  (or (null paths)
      (setq load-path (append (nreverse paths) load-path))))
(setq load-path (append (list default-directory
			      (expand-file-name "shimbun")) load-path))

(if (and (boundp 'emacs-major-version)
	 (>= emacs-major-version 21))
    (defadvice load (before nomessage activate)
      "Shut up `Loading...' message."
      (ad-set-arg 2 t)))

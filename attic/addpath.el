;; This file is used for the make rule `very-slow' which adds the user
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

;; Check whether the shell command can be used.
(let ((test (lambda nil
	      (let ((buffer (generate-new-buffer " *temp*"))
		    (msg "Hello World"))
		(save-excursion
		  (set-buffer buffer)
		  (condition-case nil
		      (call-process shell-file-name
				    nil t nil "-c"
				    (concat "MESSAGE=\"" msg "\"&&"
					    "echo \"${MESSAGE}\""))
		    (error))
		  (prog2
		      (goto-char (point-min))
		      (search-forward msg nil t)
		    (kill-buffer buffer)))))))
  (or (funcall test)
      (progn
	(require 'executable)
	(setq shell-file-name (executable-find "cmdproxy"))
	(funcall test))
      (progn
	(setq shell-file-name (executable-find "sh"))
	(funcall test))
      (progn
	(setq shell-file-name (executable-find "bash"))
	(funcall test))
      (error "\
There is no shell command which is equivalent to /bin/sh.  Try
``make SHELL=foo [option...]'', where `foo' is the absolute path name
for the proper shell command in your system.")))

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
(load-file (expand-file-name "w3m.el" default-directory))

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
	(princ (format "%sc\n" module))))))

(defun w3mhack-version ()
  "Print version of w3m.el."
  (princ emacs-w3m-version))

;;; w3mhack.el ends here

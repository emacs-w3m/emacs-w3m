;;; w3m-bug.el --- command to report emacs-w3m bugs -*- coding: euc-japan -*-

;; Copyright (C) 2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

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
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; `M-x report-emacs-w3m-bug' starts an email note to the emacs-w3m
;; developers describing a problem.

;;; Code:

(defvar report-emacs-w3m-bug-address "emacs-w3m@namazu.org"
  "*Address of mailing list for emacs-w3m bugs.")

(defvar report-emacs-w3m-bug-no-explanations nil
  "*If non-nil, suppress the explanations given for the sake of novice users.")

(defconst report-emacs-w3m-bug-system-informations
  '(emacs-w3m-version
    emacs-version
    mule-version
    Meadow-version
    system-type
    w3m-version
    w3m-type
    w3m-compile-options
    w3m-language
    w3m-command-arguments
    w3m-command-arguments-alist
    w3m-command-environment
    w3m-use-mule-ucs)
  "List of the system informations.  Users should NEVER modify the value."
  ;; For the developers:
  ;; It is possible that it would be a security hole.  To prevent those
  ;; rogue attacks, this constant should be reloaded for each time to
  ;; send a bug report.  Each element can be the symbol of a variable,
  ;; a Lisp function with no argument or any Lisp form to be evaluated.
  )

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (when (boundp 'MULE)
    (require 'w3m-om)))

(defun report-emacs-w3m-bug (topic)
  "Report a bug in emacs-w3m.
Prompts for bug subject.  Leaves you in a mail buffer."
  (interactive
   (if (or (eq major-mode 'w3m-mode)
	   (let ((keymap (or (get-text-property (point) 'keymap)
			     (get-text-property (point) 'local-map))))
	     (and (keymapp keymap)
		  (where-is-internal 'w3m-print-current-url keymap))))
       (list (read-string "Bug Subject: "))
     (error "`%s' must be invoked from the `w3m-mode' buffer" this-command)))
  (let (after-load-alist)
    ;; See the comment for `report-emacs-w3m-bug-system-informations'.
    (load "w3m-bug"))
  (let ((w3m-buffer (current-buffer)))
    (compose-mail report-emacs-w3m-bug-address topic)
    (goto-char (point-min))
    (re-search-forward (concat "^" (regexp-quote mail-header-separator) "$"))
    (forward-line 1)
    (unless report-emacs-w3m-bug-no-explanations
      ;; Insert warnings for novice users.
      (if (string-equal (symbol-value 'w3m-language) "Japanese")
	  (progn
	    (insert "このバグリポートは emacs-w3m 開発チームに送られます。\n")
	    (put-text-property (point)
			       (progn
				 (insert "\
あなたのローカルサイトの管理者宛てではありません!!")
				 (point))
			       'face 'underline)
	    (insert "\n\nできるだけ簡潔に述べて下さい:
\t- 何が起きましたか?
\t- 本当はどうなるべきだったと思いますか?
\t- そのとき何をしましたか? (正確に)

もし Lisp のバックトレースがあれば添付して下さい。\n"))
	(insert "\
This bug report will be sent to the emacs-w3m development team,\n")
	(put-text-property (point)
			   (progn
			     (insert " not to your local site managers!!")
			     (point))
			   'face 'italic)
	(insert "\nPlease write in ")
	(put-text-property (point) (progn
				     (insert "simple")
				     (point))
			   'face 'italic)
	(insert " English, because the emacs-w3m developers
aren't good at English reading. ;-)

Please describe as succinctly as possible:
\t- What happened.
\t- What you thought should have happened.
\t- Precisely what you were doing at the time.

Please also include any Lisp back-traces that you may have.\n"))
      (insert "\
================================================================\n"))
    (insert "Dear Bug Team!\n\n")
    (let ((user-point (point))
	  (print-escape-newlines t)
	  infos)
      (insert "\n
================================================================

System Info to help track down your bug:
---------------------------------------\n")
      (with-current-buffer w3m-buffer
	(dolist (info report-emacs-w3m-bug-system-informations)
	  (push (prin1-to-string info) infos)
	  (push "\n => " infos)
	  (push (cond ((functionp info)
		       (prin1-to-string (condition-case code
					    (funcall info)
					  (error
					   code))))
		      ((symbolp info)
		       (if (boundp info)
			   (prin1-to-string (symbol-value info))
			 "(not bound)"))
		      ((consp info)
		       (prin1-to-string (condition-case code
					    (eval info)
					  (error
					   code)))))
		infos)
	  (push "\n" infos)))
      (apply 'insert (nreverse infos))
      (goto-char user-point))))

;;; w3m-bug.el ends here

;;; w3m-filter.el --- filtering utility of advertisements on WEB sites.

;; Copyright (C) 2001, 2002, 2003, 2004, 2005
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
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

;; w3m-filter.el is the add-on utility to filter advertisements on WEB
;; sites.


;;; Code:

(provide 'w3m-filter)

(eval-when-compile
  (require 'cl))

(require 'w3m)

(defcustom w3m-filter-rules
  `(("\\`http://www\\.geocities\\.co\\.jp/"
     w3m-filter-delete-regions
     "<DIV ALIGN=CENTER>\n<!--*/GeoGuide/*-->" "<!--*/GeoGuide/*-->\n</DIV>")
    ("\\`http://[a-z]+\\.hp\\.infoseek\\.co\\.jp/"
     w3m-filter-delete-regions
     "<!-- start AD -->" "<!-- end AD -->")
    ("\\`http://linux\\.ascii24\\.com/linux/"
     w3m-filter-delete-regions
     "<!-- DAC CHANNEL AD START -->" "<!-- DAC CHANNEL AD END -->")
    ("\\`http://www\\.asahi\\.com/" w3m-filter-asahi-shimbun))
  "Rules to filter advertisements on WEB sites."
  :group 'w3m
  :type '(repeat
	  (cons :format "%v" :indent 4
		(regexp :format "Regexp: %v\n" :size 0)
		(choice
		 :tag "Filtering Rule"
		 (list :tag "Delete regions surrounded with these patterns"
		       (function-item :format "" w3m-filter-delete-region)
		       (regexp :tag "Start")
		       (regexp :tag "End"))
		 (list :tag "Filter with a user defined function"
		       function
		       (repeat :tag "Arguments" sexp))))))

;;;###autoload
(defun w3m-filter (url)
  "Apply filtering rule of URL against a content in this buffer."
  (save-match-data
    (catch 'apply-filtering-rule
      (dolist (elem w3m-filter-rules)
	(when (string-match (car elem) url)
	  (throw 'apply-filtering-rule
		 (apply (cadr elem) url (cddr elem))))))))

(defun w3m-filter-delete-regions (url start end)
  "Delete regions surrounded with a START pattern and an END pattern."
  (goto-char (point-min))
  (let (p (i 0))
    (while (and (search-forward start nil t)
		(setq p (match-beginning 0))
		(search-forward end nil t))
      (delete-region p (match-end 0))
      (incf i))
    (> i 0)))

;; Filter functions:
(defun w3m-filter-asahi-shimbun (url)
  "Convert entity reference of UCS."
  (when w3m-use-mule-ucs
    (goto-char (point-min))
    (let ((case-fold-search t)
	  end ucs)
      (while (re-search-forward "alt=\"\\([^\"]+\\)" nil t)
	(goto-char (match-beginning 1))
	(setq end (set-marker (make-marker) (match-end 1)))
	(while (re-search-forward "&#\\([0-9]+\\);" (max end (point)) t)
	  (setq ucs (string-to-number (match-string 1)))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert-char (w3m-ucs-to-char ucs) 1))))))

;;; w3m-filter.el ends here

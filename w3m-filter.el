;;; w3m-filter.el --- filtering utility of advertisements on WEB sites.

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006
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
;; Inc.; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.


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
    ("\\`http://\\(www\\|images\\|news\\|maps\\|groups\\)\\.google\\."
     w3m-filter-google)
    ("\\`https?://\\(?:www\\.\\)?amazon\\.\\(?:com\\|co\\.\\(?:jp\\|uk\\)\\|fr\\|de\\)/"
     w3m-filter-amazon)
    ("\\`http://mixi\\.jp" w3m-filter-mixi)
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

(defcustom w3m-filter-google-use-utf8
  (or (featurep 'un-define) (fboundp 'utf-translate-cjk-mode)
      (and (not (equal "Japanese" w3m-language))
	   (w3m-find-coding-system 'utf-8)))
  "*Use the converting rule to UTF-8 on the site of Google."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-filter-google-use-ruled-line  t
  "*Use the ruled line on the site of Google."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-filter-amazon-regxp (concat
				    "\\`\\(https?://\\(?:www\\.\\)?amazon\\."
				    "\\(?:com\\|co\\.\\(?:jp\\|uk\\)\\|fr\\|de\\)"
				    ;; "Joyo.com" 
				    "\\)/"
				    "\\(?:"
				    "\\(?:exec/obidos\\|o\\)/ASIN"
				    "\\|"
				    "gp/product"
				    "\\|"
				    "\\(?:[^/]+/\\)?dp"
				    "\\)"
				    "/\\([0-9]+\\)")
  "*Regexp to extract ASIN number for Amazon."
  :group 'w3m
  :type '(string :size 0))

(defcustom w3m-filter-amazon-short-url-bottom nil
  "*Amazon short URLs insert bottom position."
  :group 'w3m
  :type 'boolean)

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

(defun w3m-filter-google (url)
  "Insert separator within items."
  (goto-char (point-min))
  (let ((endm (make-marker))
	(case-fold-search t)
	pos beg end)
    (when (and w3m-filter-google-use-utf8
	       (re-search-forward
     		"<a class=. href=\"http://\\(www\\|images\\|news\\|maps\\|groups\\)\\.google\\."
     		nil t)
     	       (setq pos (match-beginning 0))
     	       (search-backward "<table" nil t)
     	       (setq beg (match-beginning 0))
     	       (search-forward "</table" nil t)
     	       (set-marker endm (match-end 0))
     	       (< pos (marker-position endm)))
      (goto-char beg)
      (while (re-search-forward "[?&][io]e=\\([^&]+\\)&" endm t)
     	(replace-match "UTF-8" nil nil nil 1))
      (setq end (marker-position endm)))
    (when (string-match "\\`http://www\\.google\\.[^/]+/search\\?" url)
      (goto-char (point-max))
      (when (and w3m-filter-google-use-ruled-line
		 (search-backward "<div class=" end t)
		 (search-forward "</div>" nil t))
	(insert "<hr>"))
      (if w3m-filter-google-use-ruled-line
	  (while (search-backward "<div class=" end t)
	    (insert "<hr>"))
	(while (search-backward "<div class=" end t)
	  (insert "<p>"))))))

(defun w3m-filter-amazon (url)
  "Insert Amazon short URIs."
  (when (string-match w3m-filter-amazon-regxp url)
    (let* ((base (match-string 1 url))
	   (asin (match-string 2 url))
	   (shorturls `(,(concat base "/dp/" asin "/")
			,(concat base "/o/ASIN/" asin "/")
			,(concat base "/gp/product/" asin "/")))
	   (case-fold-search t)
	   shorturl)
      (goto-char (point-min))
      (setq url (file-name-as-directory url))
      (when (or (and (not w3m-filter-amazon-short-url-bottom)
		     (search-forward "<body>" nil t))
		(and w3m-filter-amazon-short-url-bottom
		     (search-forward "</body>" nil t)
		     (goto-char (match-beginning 0))))
	(insert "\n")
	(while (setq shorturl (car shorturls))
	  (setq shorturls (cdr shorturls))
	  (unless (string= url shorturl)
	    (insert (format "Amazon Short URL: <a href=\"%s\">%s</a><br>\n"
			    shorturl shorturl))))
	(insert "\n")))))

(defun w3m-filter-mixi (url)
  "Direct jump to the external diary."
  (goto-char (point-min))
  (let (newurl)
    (while (re-search-forward "<a href=view_diary\\.pl\\?url=\\([^>]+\\)>" nil t)
      (setq newurl (match-string 1))
      (when newurl
	(delete-region (match-beginning 0) (match-end 0))
	(when (string-match "&owner_id=[0-9]+\\'" newurl)
	  (setq newurl (substring newurl 0 (match-beginning 0))))
	(insert (format "<a href=\"%s\">" (w3m-url-readable-string newurl)))))))

;;; w3m-filter.el ends here

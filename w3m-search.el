;;; -*- mode: Emacs-Lisp; coding: euc-japan -*-

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>

;; Authors: Keisuke Nishida    <kxn30@po.cwru.edu>,
;;          Shun-ichi GOTO     <gotoh@taiyo.co.jp>,
;;          TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;; Keywords: w3m, WWW, hypermedia

;; w3m-search.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; w3m-search.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with w3m.el; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


;;; Commentary:

;; w3m-search.el is the add-on program of w3m.el to access WEB search
;; engines.  For more detail about w3m.el, see:
;;
;;    http://namazu.org/~tsuchiya/emacs-w3m/


;;; How to install:

;; Please put this file to appropriate directory, and if you want
;; byte-compile it.  And add following lisp expressions to your
;; ~/.emacs.
;;
;;     (autoload 'w3m-search "w3m-search" "*Search QUERY using SEARCH-ENGINE." t)


;;; Code:
(require 'w3m)

(defcustom w3m-search-default-coding-system 'euc-japan
  "*Coding system to encode search query string.
This value is default and used only when spec defined by
`w3m-search-engine-alist' does not have encoding information."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-search-engine-alist
  '(("yahoo" "http://search.yahoo.com/bin/search?p=%s" nil)
    ("yahoo-ja" "http://search.yahoo.co.jp/bin/search?p=%s" euc-japan)
    ("google" "http://www.google.com/search?q=%s" nil)
    ("google-ja" "http://www.google.com/search?q=%s&hl=ja&lr=" shift_jis)
    ("goo-ja" "http://www.goo.ne.jp/default.asp?MT=%s" euc-japan)
    ("rpmfind" "http://rpmfind.net/linux/rpm2html/search.php?query=%s" nil))
  "*An alist of search engines.
Each elemnt looks like (ENGINE ACTION CODING)
ENGINE is a string, the name of the search engine.
ACTION is a string, the URL that performs a search.
ACTION must contain a \"%s\", which is substituted by a query string.
CODING is optional value which is coding system for query string.
If omitted, `w3m-search-default-coding-system' is used.
"
  :group 'w3m
  :type '(repeat
	  (list
	   (string :tag "Engine")
	   (string :tag "Action")
	   (coding-system))))

(defcustom w3m-search-default-engine "yahoo"
  "*Default search engine name.
See also `w3m-search-engine-alist'."
  :group 'w3m
  :type 'string)

(defun w3m-search-escape-query-string (str &optional coding)
  (mapconcat
   (lambda (s)
     (w3m-url-encode-string s (or coding w3m-search-default-coding-system)))
   (split-string str)
   "+"))

(defun w3m-search (search-engine query)
  "*Search QUERY using SEARCH-ENGINE.
When called interactively with prefix argument, you can choose search
engine deinfed in `w3m-search-engine-alist'.  Otherwise use
`w3m-search-default-engine'."
  (interactive
   (let* ((engine
	  (if current-prefix-arg
	      (completing-read
	       (format "Which Engine? (%s): " w3m-search-default-engine)
	       w3m-search-engine-alist nil t)
	    w3m-search-default-engine))
	  (default (thing-at-point 'word))
	  (prompt (if default
		      (format "%s search (default %s): " engine default)
		    (format "%s search: " engine)))
	  (query (read-string prompt nil nil default)))
     (list engine query)))
  (unless (string= query "")
    (let ((info (assoc search-engine w3m-search-engine-alist)))
      (if info
	  (w3m (format (cadr info)
		       (w3m-search-escape-query-string query (caddr info))))
	(error "Unknown search engine: %s" search-engine)))))


(provide 'w3m-search)
;;; w3m-search.el ends here.

;;; w3m-search.el --- The add-on program to access WEB search engines.

;; Copyright (C) 2001, 2002 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: Keisuke Nishida    <kxn30@po.cwru.edu>,
;;          Shun-ichi GOTO     <gotoh@taiyo.co.jp>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>
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

;; w3m-search.el is the add-on program of emacs-w3m to access WEB
;; search engines.  For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/


;;; How to install:

;; Put this file in the appropriate directory, and if you want,
;; byte-compile it.  Then add the following lisp expression to your
;; ~/.emacs.
;;
;;     (autoload 'w3m-search "w3m-search" "Search QUERY using SEARCH-ENGINE." t)


;;; Code:

(eval-when-compile
  (require 'cl))

(require 'w3m)

(defcustom w3m-search-engine-alist
  (` (("yahoo" "http://search.yahoo.com/bin/search?p=%s" nil)
      ("yahoo-ja" "http://search.yahoo.co.jp/bin/search?p=%s" euc-japan)
      (, (if (equal "Japanese" w3m-language)
	     '("google" "http://www.google.com/search?q=%s&hl=ja" shift_jis)
	   '("google" "http://www.google.com/search?q=%s" nil)))
      ("google groups" "http://groups.google.com/groups?q=%s" nil)
      ("google-ja" "http://www.google.com/search?q=%s&hl=ja&lr=lang_ja" shift_jis)
      ("goo-ja" "http://www.goo.ne.jp/default.asp?MT=%s" euc-japan)
      ("excite-ja" "http://www.excite.co.jp/search.gw?target=combined&look=excite_jp&lang=jp&tsug=-1&csug=-1&search=%s" shift_jis)
      ("lycos-ja" "http://www.lycos.co.jp/cgi-bin/pursuit?query=\"%s\"&cat=jp&encoding=shift-jis" shift_jis)
      ("altavista" "http://altavista.com/sites/search/web?q=\"%s\"&kl=ja&search=Search"  nil)
      ("rpmfind" "http://rpmfind.net/linux/rpm2html/search.php?query=%s" nil)
      ("debian-pkg" "http://packages.debian.org/cgi-bin/search_contents.pl?directories=yes&arch=i386&version=unstable&case=insensitive&word=%s" nil)
      ("debian-bts" "http://bugs.debian.org/cgi-bin/pkgreport.cgi?archive=yes&pkg=%s" nil)
      ("freebsd-users-jp" "http://home.jp.FreeBSD.org/cgi-bin/namazu.cgi?key=\"%s\"&whence=0&max=50&format=long&sort=score&dbname=FreeBSD-users-jp" euc-japan)
      ("iij-archie" "http://www.iij.ad.jp/cgi-bin/archieplexform?query=%s&type=Case+Insensitive+Substring+Match&order=host&server=archie1.iij.ad.jp&hits=95&nice=Nice")))
  "*An alist of search engines.
Each element looks like (ENGINE ACTION CODING)
ENGINE is a string, the name of the search engine.
ACTION is a string, the URL that performs a search.
ACTION must contain a \"%s\", which is substituted by a query string.
CODING is optional value which is coding system for query string.
If omitted, `w3m-default-coding-system' is used.
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

(defcustom w3m-search-quick-search-engine-alist
  '(("gg"   . "google")
    ("ggg"  . "google groups")
    ("ya"   . "yahoo")
    ("al"   . "altavista")
    ("bts"  . "debian-bts")
    ("dpkg" . "debian-pkg"))
  "*An alist of short names for defined search engine names.
Each element follows the scheme (NAME . ENGINE)
NAME if the short name you want to use for this engine.
ENGINE is the name of the engine as defined in `w3m-search-engine-alist'.
Be careful: the names you use here must match exactly the names of the
engines defined in `w3m-search-engine-alist'."
  :group 'w3m)

(defcustom w3m-search-word-at-point t
  "*Non-nil means that the word at point is used as initial string."
  :group 'w3m
  :type 'boolean)

(defun w3m-search-escape-query-string (str &optional coding)
  (mapconcat
   (lambda (s)
     (w3m-url-encode-string s (or coding w3m-default-coding-system)))
   (split-string str)
   "+"))

;;;###autoload
(defun w3m-search (search-engine query)
  "Search QUERY using SEARCH-ENGINE.
When called interactively with a prefix argument, you can choose one of
the search engines defined in `w3m-search-engine-alist'.  Otherwise use
`w3m-search-default-engine'."
  (interactive
   (let ((engine
	  (if current-prefix-arg
	      (completing-read
	       (format "Which Engine? (%s): " w3m-search-default-engine)
	       w3m-search-engine-alist nil t)
	    w3m-search-default-engine))
	 (default (unless (eq (get-text-property (line-beginning-position)
						 'face)
			      'w3m-header-line-location-title-face)
		    (thing-at-point 'word)))
	 prompt query)
     (when default
       (set-text-properties 0 (length default) nil default))
     (setq prompt (if (and default (not w3m-search-word-at-point))
		      (format "%s search (default %s): " engine default)
		    (format "%s search: " engine)))
     (setq query (if w3m-search-word-at-point
		     (read-string prompt default)
		   (read-string prompt nil nil default)))
     (list (if (string= engine "")
	       w3m-search-default-engine
	     engine)
	   query)))
  (unless (string= query "")
    (let ((info (assoc search-engine w3m-search-engine-alist)))
      (if info
	  (w3m (format (cadr info)
		       (w3m-search-escape-query-string query (caddr info))))
	(error "Unknown search engine: %s" search-engine)))))

;;;###autoload
(defun w3m-search-quick-search-handler (url)
  "Check if the url is a quicksearch url.
If URL is a quicksearch url, replace it with the real url needed to access
the search engine.  If not, leave it alone."
  (let ((ret url))
    (dolist (quick-search-engine w3m-search-quick-search-engine-alist ret)
      (if (string-match (concat "\\`" (car quick-search-engine) ":") url)
	  (let ((query (substring url (match-end 0))))
	    (unless (string= query "")
	      (let ((info (assoc (cdr quick-search-engine)
				 w3m-search-engine-alist)))
		(if info
		    (setq ret (format (cadr info)
				      (w3m-search-escape-query-string
				       query (caddr info))))))))))))

(provide 'w3m-search)

;;; w3m-search.el ends here

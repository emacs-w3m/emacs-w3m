;;; w3m-search.el --- functions convenient to access web search engines

;; Copyright (C) 2001, 2002, 2003, 2004
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: Keisuke Nishida    <kxn30@po.cwru.edu>,
;;          Shun-ichi GOTO     <gotoh@taiyo.co.jp>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;          Romain FRANCOISE   <romain@orebokech.com>
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

;; This module contains the `w3m-search' command and some utilities
;; to improve your cyberlife.  For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'w3m)

(defcustom w3m-search-engine-alist
  (let ((ja (equal "Japanese" w3m-language)))
    `(,@(if ja
	    '(("yahoo"
	       "http://search.yahoo.co.jp/bin/search?p=%s"
	       euc-japan)
	      ("yahoo-en"
	       "http://search.yahoo.com/bin/search?p=%s"))
	  '(("yahoo"
	     "http://search.yahoo.com/bin/search?p=%s")
	    ("yahoo-ja"
	     "http://search.yahoo.co.jp/bin/search?p=%s"
	     euc-japan)))
	,@(if ja
	      '(("google"
		 "http://www.google.com/search?q=%s&hl=ja&lr=lang_ja&ie=Shift_JIS"
		 shift_jis)
		("google-en"
		 "http://www.google.com/search?q=%s"))
	    '(("google"
	       "http://www.google.com/search?q=%s")
	      ("google-ja"
	       "http://www.google.com/search?q=%s&hl=ja&lr=lang_ja&ie=Shift_JIS"
	       shift_jis)))
	("google groups"
	 "http://groups.google.com/groups?q=%s")
	,@(if ja
	      '(("All the Web"
		 "http://www.alltheweb.com/search?web&_sb_lang=ja&cs=euc-jp\
&q=%s"
		 euc-japan)
		("All the Web-en"
		 "http://www.alltheweb.com/search?web&_sb_lang=en&q=%s"))
	    '(("All the Web"
	       "http://www.alltheweb.com/search?web&_sb_lang=en&q=%s")
	      ("All the Web-ja"
	       "http://www.alltheweb.com/search?web&_sb_lang=ja&cs=euc-jp&q=%s"
	       euc-japan)))
	("goo-ja"
	 "http://www.goo.ne.jp/default.asp?MT=%s"
	 euc-japan)
	("excite-ja"
	 "http://www.excite.co.jp/search.gw?target=combined&look=excite_jp\
&lang=jp&tsug=-1&csug=-1&search=%s"
	 shift_jis)
	("lycos-ja"
	 "http://www.lycos.co.jp/cgi-bin/pursuit?query=\"%s\"&cat=jp&\
encoding=shift-jis"
	 shift_jis)
	("altavista"
	 "http://altavista.com/sites/search/web?q=\"%s\"&kl=ja&search=Search")
	("rpmfind"
	 "http://rpmfind.net/linux/rpm2html/search.php?query=%s"
	 nil)
	("debian-pkg"
	 "http://packages.debian.org/cgi-bin/search_contents.pl\
?directories=yes&arch=i386&version=unstable&case=insensitive&word=%s")
	("debian-bts"
	 "http://bugs.debian.org/cgi-bin/pkgreport.cgi?archive=yes&pkg=%s")
	("freebsd-users-jp"
	 "http://home.jp.FreeBSD.org/cgi-bin/namazu.cgi?key=\"%s\"&whence=0\
&max=50&format=long&sort=score&dbname=FreeBSD-users-jp"
	 euc-japan)
	("iij-archie"
	 "http://www.iij.ad.jp/cgi-bin/archieplexform?query=%s\
&type=Case+Insensitive+Substring+Match&order=host&server=archie1.iij.ad.jp\
&hits=95&nice=Nice")
	("waei"
	 "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=1"
	 euc-japan)
	("eiwa"
	 "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=0")
	("kokugo"
	 "http://dictionary.goo.ne.jp/cgi-bin/dict_search.cgi?MT=%s&sw=2"
	 euc-japan)
	("eiei"
	 "http://www.dictionary.com/cgi-bin/dict.pl?term=%s&r=67")
	,@(if ja
		'(("amazon"
		   "http://www.amazon.co.jp/exec/obidos/search-handle-form/\
250-7496892-7797857"
		   shift_jis
		   "url=index=blended&search-type=quick-search&\
field-keywords=%s")
		  ("amazon-en"
		   "http://www.amazon.com/exec/obidos/search-handle-form/\
250-7496892-7797857"
		   nil
		   "url=index=blended&field-keywords=%s"))
	      '(("amazon"
		 "http://www.amazon.com/exec/obidos/search-handle-form/\
250-7496892-7797857"
		 nil
		 "url=index=blended&field-keywords=%s")
		("amazon-ja"
		 "http://www.amazon.co.jp/exec/obidos/search-handle-form/\
250-7496892-7797857"
		 shift_jis
		 "url=index=blended&search-type=quick-search&\
field-keywords=%s")))))
  "*An alist of search engines.
Each element looks like (ENGINE ACTION CODING POST-DATA)
ENGINE is a string, the name of the search engine.
ACTION is a string, the URL that performs a search.
ACTION must contain a \"%s\", which is substituted by a query string.
CODING is optional value which is coding system for query string.
POST-DATA is optional value which is a string for POST method search engine.
If CODING is omitted, it defaults to `w3m-default-coding-system'."
  :group 'w3m
  :type `(repeat
	  (group :indent 2
		 (string :format "Engine: %v\n" :size 0)
		 (string :format "       Action: %v\n" :size 0)
		 (coding-system :format "%t: %v\n" :size 0)
		 (checklist :inline t
			    :entry-format ,(if (if (featurep 'xemacs)
						   (device-on-window-system-p)
						 window-system)
					       "%b   %v"
					     "%b  %v")
			    (string :format "PostData: %v\n" :size 0)))))

(defcustom w3m-search-default-engine "google"
  "*Name of the default search engine.
See also `w3m-search-engine-alist'."
  :group 'w3m
  :type '(string :size 0))

(defcustom w3m-search-word-at-point t
  "*Non-nil means that the word at point is used as an initial string.
If Transient Mark mode, this option is ignored and the region is used
as an initial string."
  :group 'w3m
  :type 'boolean)

(defun w3m-search-escape-query-string (str &optional coding)
  (mapconcat
   (lambda (s)
     (w3m-url-encode-string s (or coding w3m-default-coding-system)))
   (split-string str)
   "+"))

(defun w3m-search-read-query (prompt prompt-with-default &optional history)
  "Read a query from the minibuffer, prompting with string PROMPT.
When a default value for the query is discovered, prompt with string
PROMPT-WITH-DEFAULT instead of string PROMPT."
  (let ((default
	  (if (w3m-region-active-p)
	      (buffer-substring (region-beginning) (region-end))
	    (unless (eq (get-text-property (line-beginning-position) 'face)
			'w3m-header-line-location-title-face)
	      (thing-at-point 'word))))
	(initial))
    (when default
      (set-text-properties 0 (length default) nil default)
      (when (or w3m-search-word-at-point (w3m-region-active-p))
	(setq initial default
	      default nil))
      (when (w3m-region-active-p)
	(w3m-deactivate-region)))
    (read-string (if default
		     (format prompt-with-default default)
		   prompt)
		 initial history default)))

;;;###autoload
(defun w3m-search (search-engine query)
  "Search QUERY using SEARCH-ENGINE.
When called interactively with a prefix argument, you can choose one of
the search engines defined in `w3m-search-engine-alist'.  Otherwise use
`w3m-search-default-engine'.
If Transient Mark mode, use the region as an initial string of query
and deactivate the mark."
  (interactive
   (let ((engine
	  (if current-prefix-arg
	      (let ((completion-ignore-case t))
		(completing-read (format "Which engine? (default %s): "
					 w3m-search-default-engine)
				 w3m-search-engine-alist nil t))
	    w3m-search-default-engine)))
     (when (string= engine "")
       (setq engine w3m-search-default-engine))
     (list engine
	   (w3m-search-read-query
	    (format "%s search: " engine)
	    (format "%s search (default %%s): " engine)))))
  (unless (string= query "")
    (let ((info (assoc search-engine w3m-search-engine-alist)))
      (if info
	  (let ((query-string (w3m-search-escape-query-string query
							      (caddr info)))
		(post-data (cadddr info)))
	    (w3m-goto-url
	     (format (cadr info) query-string)
	     post-data
	     nil
	     (and post-data (format post-data query-string))))
	(error "Unknown search engine: %s" search-engine)))))

;;;###autoload
(defun w3m-search-uri-replace (uri engine)
  "Generate query string for ENGINE from URI matched by last search."
  (let ((query (substring uri (match-end 0)))
	(info (assoc engine w3m-search-engine-alist)))
    (when info
      (format (cadr info)
	      (w3m-search-escape-query-string query (caddr info))))))

(provide 'w3m-search)

;;; w3m-search.el ends here

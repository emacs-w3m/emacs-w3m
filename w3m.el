;;; -*- mode: Emacs-Lisp; coding: euc-japan -*-

;; Copyright (C) 2000,2001 TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>,
;;          Shun-ichi GOTO     <gotoh@taiyo.co.jp>,
;;          Satoru Takabayashi <satoru-t@is.aist-nara.ac.jp>,
;;          Hideyuki SHIRAI    <shirai@meadowy.org>,
;;          Keisuke Nishida    <kxn30@po.cwru.edu>
;; Keywords: w3m, WWW, hypermedia

;; w3m.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; w3m.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with w3m.el; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


;;; Commentary:

;; w3m.el is the interface program of w3m on Emacs.  For more detail
;; about w3m, see:
;;
;;    http://ei5nazha.yz.yamagata-u.ac.jp/~aito/w3m/


;;; How to install:

;; Please put this file to appropriate directory, and if you want
;; byte-compile it.  And add following lisp expressions to your
;; ~/.emacs.
;;
;;     (autoload 'w3m "w3m" "Interface for w3m on Emacs." t)


;;; Code:

(or (and (boundp 'emacs-major-version)
	 (>= emacs-major-version 20))
    (progn
      (require 'poe)
      (require 'pcustom)))

(if (featurep 'xemacs)
    (require 'poem))

(require 'thingatpt)

(unless (fboundp 'find-coding-system)
  (if (fboundp 'coding-system-p)
      (defsubst find-coding-system (obj)
	"Return OBJ if it is a coding-system."
	(if (coding-system-p obj) obj))
    (require 'pces)))

;; this package using a few CL macros
(eval-when-compile (require 'cl))

(put 'w3m-static-if 'lisp-indent-function 2)
(defmacro w3m-static-if (cond then &rest else)
  (if (eval cond) then (` (progn  (,@ else)))))

(defgroup w3m nil
  "w3m - the web browser of choice."
  :group 'hypermedia)

(defgroup w3m-face nil
  "Faces for w3m."
  :group 'w3m
  :prefix "w3m-")

(defcustom w3m-command "w3m"
  "*Name of the executable file of w3m."
  :group 'w3m
  :type 'string)

(defcustom w3m-fill-column -1
  "*Fill column of w3m.
Value is integer.
Positive value is for fixed column rendering.
Zero or negative value is for fitting w3m output with current frame
width using expression (+ (frame-width) VALUE)."
  :group 'w3m
  :type 'integer)

(defcustom w3m-mailto-url-function nil
  "*Mailto handling Function."
  :group 'w3m
  :type 'function)

(defcustom w3m-coding-system
  (w3m-static-if (boundp 'MULE) '*euc-japan* 'euc-japan)
  "*Coding system for w3m."
  :group 'w3m
  :type 'symbol)

(defcustom w3m-input-coding-system
  (w3m-static-if (boundp 'MULE) '*iso-2022-jp* 'iso-2022-jp)
  "*Coding system for w3m."
  :group 'w3m
  :type 'symbol)

(defcustom w3m-output-coding-system
  (w3m-static-if (boundp 'MULE) '*euc-japan* 'euc-japan)
  "*Coding system for w3m."
  :group 'w3m
  :type 'symbol)

(defcustom w3m-default-url-coding-system
  (w3m-static-if (boundp 'MULE) '*euc-japan* 'euc-japan)
  "*Coding system to encode search query string.
This value is default and used only when spec defined by
`w3m-search-engine-alist' does not have encoding information."
  :group 'w3m
;  :type 'string)
  :type '(restricted-sexp :match-alternatives (coding-system-p)))

(defcustom w3m-use-cygdrive t
  "*If non-nil, use /cygdrive/ rule when expand-file-name."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-profile-directory "~/.w3m"
  "*Directory of w3m profiles."
  :group 'w3m
  :type 'directory)

(defcustom w3m-default-save-directory "~/.w3m"
  "*Default directory for save file."
  :group 'w3m
  :type 'directory)

(defcustom w3m-delete-duplicated-empty-lines t
  "*Compactize page by deleting duplicated empty lines."
  :group 'w3m
  :type 'boolean)


(defun w3m-url-to-file-name (url)
  (if (string-match "^file:" url)
      (setq url (substring url (match-end 0))))
  (if (string-match "^\\(//\\|/cygdrive/\\)\\(.\\)/\\(.*\\)" url)
      (setq url (concat (match-string 2 url) ":/" (match-string 3 url))))
  url)

(defun w3m-expand-file-name-as-url (file &optional directory)
  ;; if filename is cygwin format,
  ;; then remove cygdrive prefix before expand-file-name
  (if directory
      (setq file (w3m-url-to-file-name file)))
  ;; expand to file scheme url considering Win32 environment
  (setq file (expand-file-name file directory))
  (if (string-match "^\\(.\\):\\(.*\\)" file)
      (if w3m-use-cygdrive
	  (concat "/cygdrive/" (match-string 1 file) (match-string 2 file))
	(concat "file://" (match-string 1 file) (match-string 2 file)))
    file))

(defcustom w3m-bookmark-file
  (expand-file-name "bookmark.html" w3m-profile-directory)
  "*Bookmark file of w3m."
  :group 'w3m
  :type 'file)

(defcustom w3m-bookmark-file-coding-system
  (w3m-static-if (boundp 'MULE) '*euc-japan* 'euc-japan)
  "*Coding system for bookmark file."
  :group 'w3m
  :type 'symbol)

(defcustom w3m-home-page
  (or (getenv "HTTP_HOME")
      (getenv "WWW_HOME")
      (if (file-readable-p w3m-bookmark-file)
	  (w3m-expand-file-name-as-url w3m-bookmark-file)
	"http://namazu.org/~tsuchiya/emacs-w3m/"))
  "*Home page of w3m.el."
  :group 'w3m
  :type 'string)

(defcustom w3m-arrived-urls-file
  (expand-file-name ".arrived" w3m-profile-directory)
  "*Arrived URL file of w3m."
  :group 'w3m
  :type 'file)

(defcustom w3m-arrived-file-coding-system
  (w3m-static-if (boundp 'MULE) '*euc-japan*unix 'euc-japan-unix)
  "*Coding system for arrived file."
  :group 'w3m
  :type 'symbol)

(defcustom w3m-keep-arrived-urls 500
  "*Arrived keep count of w3m."
  :group 'w3m
  :type 'integer)

(defcustom w3m-keep-backlog 300
  "*Back log size of w3m."
  :group 'w3m
  :type 'integer)

(defface w3m-anchor-face
  '((((class color) (background light)) (:foreground "blue" :underline t))
    (((class color) (background dark)) (:foreground "cyan" :underline t))
    (t (:underline t)))
  "*Face to fontify anchors."
  :group 'w3m-face)

(defface w3m-arrived-anchor-face
  '((((class color) (background light)) (:foreground "navy" :underline t))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :underline t))
    (t (:underline t)))
  "*Face to fontify anchors, if arrived."
  :group 'w3m-face)

(defface w3m-image-face
  '((((class color) (background light)) (:foreground "ForestGreen"))
    (((class color) (background dark)) (:foreground "PaleGreen"))
    (t (:underline t)))
  "*Face to fontify image alternate strings."
  :group 'w3m-face)

(defface w3m-form-face
  '((((class color) (background light)) (:foreground "cyan" :underline t))
    (((class color) (background dark)) (:foreground "red" :underline t))
    (t (:underline t)))
  "*Face to fontify forms."
  :group 'w3m-face)

(defcustom w3m-hook nil
  "*Hook run before w3m called."
  :group 'w3m
  :type 'hook)

(defcustom w3m-mode-hook nil
  "*Hook run before w3m-mode called."
  :group 'w3m
  :type 'hook)

(defcustom w3m-fontify-before-hook nil
  "*Hook run before w3m-fontify called."
  :group 'w3m
  :type 'hook)

(defcustom w3m-fontify-after-hook nil
  "*Hook run after w3m-fontify called."
  :group 'w3m
  :type 'hook)

(defcustom w3m-async-exec nil
  "*If non-nil, w3m is executed an asynchronously process."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-process-connection-type t
  "*Process connection type for w3m execution."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-executable-type
  (if (memq window-system '(w32 win32))
      'cygwin ; xxx, cygwin on win32 by default
    'native)
  "*Executable binary type of w3m program.
Value is 'native or 'cygwin.
This value is maily used for win32 environment.
In other environment, use 'native."
  :group 'w3m
  :type '(choice (const cygwin) (const native)))

;; FIXME: ËÜÅö¤Ï mailcap ¤òÅ¬ÀÚ¤ËÆÉ¤ß¹þ¤ó¤ÇÀßÄê¤¹¤ëÉ¬Í×¤¬¤¢¤ë
(defcustom w3m-content-type-alist
  '(("text/plain" "\\.\\(txt\\|tex\\|el\\)" nil)
    ("text/html" "\\.s?html?$" ("netscape" url))
    ("image/jpeg" "\\.jpe?g$" ("xv" file))
    ("image/png" "\\.png$" ("xv" file))
    ("image/gif" "\\gif$" ("xv" file))
    ("image/tiff" "\\tif?f$" ("xv" file))
    ("image/x-xwd" "\\.xwd$" ("xv" file))
    ("image/x-xbm" "\\.xbm$" ("xv" file))
    ("image/x-xpm" "\\.xpm$" ("xv" file))
    ("image/x-bmp" "\\.bmp$" ("xv" file))
    ("video/mpeg" "\\.mpe?g$" ("mpeg_play" file))
    ("video/quicktime" "\\.mov$" ("mpeg_play" file))
    ("application/postscript" "\\.\\(ps\\|eps\\)$" ("gv" file))
    ("application/pdf" "\\.pdf$" ("acroread" file)))
  "Alist of file suffixes vs. content type."
  :group 'w3m
  :type '(repeat
	  (list
	   (string :tag "Type")
	   (string :tag "Regexp")
	   (choice
	    (const :tag "None" nil)
	    (cons :tag "Externai viewer"
		  (string :tag "Command")
		  (repeat :tag "Arguments"
			  (restricted-sexp :match-alternatives
					   (stringp 'file 'url))))
	    (function :tag "Function")))))

(eval-and-compile
  (and (not (fboundp 'find-coding-system))
       (fboundp 'coding-system-p)
       (defun find-coding-system (obj)
	 "Return t if OBJ if it is a coding-system."
	 (if (coding-system-p obj) obj))))

(defcustom w3m-charset-coding-system-alist
  (let ((rest
	 '((us-ascii      . raw-text)
	   (gb2312	  . cn-gb-2312)
	   (cn-gb	  . cn-gb-2312)
	   (iso-2022-jp-2 . iso-2022-7bit-ss2)
	   (iso-2022-jp-3 . iso-2022-7bit-ss2)
	   (tis-620	  . tis620)
	   (windows-874	  . tis-620)
	   (cp874	  . tis-620)
	   (x-ctext       . ctext)
	   (unknown       . undecided)
	   (x-unknown     . undecided)
	   (euc-jp        . euc-japan)
	   (shift-jis     . shift_jis)
	   (shift_jis     . shift_jis)
	   (sjis          . shift_jis)
	   (x-euc-jp      . euc-japan)
	   (x-shift-jis   . shift_jis)
	   (x-shift_jis   . shift_jis)
	   (x-sjis        . shift_jis)))
	dest)
    (while rest
      (or (find-coding-system (car (car rest)))
	  (setq dest (cons (car rest) dest)))
      (setq rest (cdr rest)))
    dest)
  "Alist MIME CHARSET vs CODING-SYSTEM.
MIME CHARSET and CODING-SYSTEM must be symbol."
  :group 'w3m
  :type '(repeat (cons symbol coding-system)))

(defcustom w3m-search-engine-alist
  '(("yahoo" "http://search.yahoo.com/bin/search?p=%s" nil)
    ("yahoo-ja" "http://search.yahoo.co.jp/bin/search?p=%s" euc-japan)
    ("google" "http://www.google.com/search?q=%s" nil)
    ("google-ja" "http://www.google.com/search?q=%s&hl=ja&lr=" shift_jis)
    ("goo-ja" "http://www.goo.ne.jp/default.asp?MT=%s" euc-japan))
  "*An alist of search engines.
Each elemnt looks like (ENGINE ACTION CODING)
ENGINE is a string, the name of the search engine.
ACTION is a string, the URL that performs a search.
ACTION must contain a \"%s\", which is substituted by a query string.
CODING is optional value which is coding system for query string.
If omitted, `w3m-default-url-coding-system' is used.
"
  :group 'w3m
  :type '(repeat
	  (list
	   (string :tag "Engine")
	   (string :tag "Action")
	   (restricted-sexp :match-alternatives (coding-system-p nil)
			    :tag "Coding"))))

(defcustom w3m-default-search-engine "yahoo"
  "*Default search engine name.
See also `w3m-search-engine-alist'."
  :group 'w3m
  :type 'string)

(defcustom w3m-horizontal-scroll-columns 10
  "*Column size to scroll horizontaly."
  :group 'w3m
  :type 'integer)

(defcustom w3m-use-form nil
  "*Non-nil activates form extension. (EXPERIMENTAL)"
  :group 'w3m
  :type 'boolean)

(defconst w3m-weather-url-alist
  (eval-when-compile
    (let ((format "http://channel.goo.ne.jp/weather/area/%s.html")
	  (alist
	   '(("ËÌ³¤Æ»¡¦½¡Ã«ÃÏÊý" . "011")
	     ("ËÌ³¤Æ»¡¦ÌÖÁöÃÏÊý" . "021")
	     ("ËÌ³¤Æ»¡¦ËÌ¸«ÃÏÊý" . "022")
	     ("ËÌ³¤Æ»¡¦ÌæÊÌÃÏÊý" . "023")
	     ("ËÌ³¤Æ»¡¦¾åÀîÃÏÊý" . "031")
	     ("ËÌ³¤Æ»¡¦Î±Ë¨ÃÏÊý" . "032")
	     ("ËÌ³¤Æ»¡¦¶üÏ©ÃÏÊý" . "041")
	     ("ËÌ³¤Æ»¡¦º¬¼¼ÃÏÊý" . "042")
	     ("ËÌ³¤Æ»¡¦½½¾¡ÃÏÊý" . "043")
	     ("ËÌ³¤Æ»¡¦ÃÀ¿¶ÃÏÊý" . "051")
	     ("ËÌ³¤Æ»¡¦Æü¹âÃÏÊý" . "052")
	     ("ËÌ³¤Æ»¡¦ÀÐ¼íÃÏÊý" . "061")
	     ("ËÌ³¤Æ»¡¦¶õÃÎÃÏÊý" . "062")
	     ("ËÌ³¤Æ»¡¦¸å»ÖÃÏÊý" . "063")
	     ("ËÌ³¤Æ»¡¦ÅÏÅçÃÏÊý" . "071")
	     ("ËÌ³¤Æ»¡¦ÛØ»³ÃÏÊý" . "072")
	     ("ÀÄ¿¹¸©¡¦ÄÅ·ÚÃÏÊý" . "081")
	     ("ÀÄ¿¹¸©¡¦²¼ËÌÃÏÊý" . "082")
	     ("ÀÄ¿¹¸©¡¦»°È¬¾åËÌÃÏÊý" . "083")
	     ("½©ÅÄ¸©¡¦±è´ßÉô" . "091")
	     ("½©ÅÄ¸©¡¦ÆâÎ¦Éô" . "092")
	     ("´ä¼ê¸©¡¦ÆâÎ¦Éô" . "101")
	     ("´ä¼ê¸©¡¦±è´ßËÌÉô" . "102")
	     ("´ä¼ê¸©¡¦±è´ßÆîÉô" . "103")
	     ("»³·Á¸©¡¦Â¼»³ÃÏÊý" . "111")
	     ("»³·Á¸©¡¦ÃÖ»òÃÏÊý" . "112")
	     ("»³·Á¸©¡¦¾±ÆâÃÏÊý" . "113")
	     ("»³·Á¸©¡¦ºÇ¾åÃÏÊý" . "114")
	     ("µÜ¾ë¸©¡¦Ê¿ÌîÉô" . "121")
	     ("µÜ¾ë¸©¡¦»³±è¤¤" . "122")
	     ("Ê¡Åç¸©¡¦ÃæÄÌ¤ê" . "131")
	     ("Ê¡Åç¸©¡¦ÉÍÄÌ¤ê" . "132")
	     ("Ê¡Åç¸©¡¦²ñÄÅÃÏÊý" . "133")
	     ("¿·³ã¸©¡¦²¼±ÛÃÏÊý" . "141")
	     ("¿·³ã¸©¡¦Ãæ±ÛÃÏÊý" . "142")
	     ("¿·³ã¸©¡¦¾å±ÛÃÏÊý" . "143")
	     ("¿·³ã¸©¡¦º´ÅÏÅç" . "144")
	     ("ÉÙ»³¸©¡¦ÅìÉô" . "151")
	     ("ÉÙ»³¸©¡¦À¾Éô" . "152")
	     ("ÀÐÀî¸©¡¦²Ã²ìÃÏÊý" . "161")
	     ("ÀÐÀî¸©¡¦Ç½ÅÐÃÏÊý" . "162")
	     ("Ê¡°æ¸©¡¦ÎæËÌ" . "171")
	     ("Ê¡°æ¸©¡¦ÎæÆî" . "172")
	     ("ÆÊÌÚ¸©¡¦ÆîÉô" . "181")
	     ("ÆÊÌÚ¸©¡¦ËÌÉô" . "182")
	     ("·²ÇÏ¸©¡¦ÆîÉô" . "191")
	     ("·²ÇÏ¸©¡¦ËÌÉô" . "192")
	     ("ºë¶Ì¸©¡¦ÆîÉô" . "201")
	     ("ºë¶Ì¸©¡¦ËÌÉô" . "202")
	     ("ºë¶Ì¸©¡¦ÃáÉãÃÏÊý" . "203")
	     ("°ñ¾ë¸©¡¦ËÌÉô" . "211")
	     ("°ñ¾ë¸©¡¦ÆîÉô" . "212")
	     ("ÀéÍÕ¸©¡¦ËÌÀ¾Éô" . "221")
	     ("ÀéÍÕ¸©¡¦ËÌÅìÉô" . "222")
	     ("ÀéÍÕ¸©¡¦ÆîÉô" . "223")
	     ("ÅìµþÅÔ" . "231")
	     ("ÅìµþÅÔ¡¦°ËÆ¦½ôÅçËÌÉô" . "232")
	     ("ÅìµþÅÔ¡¦°ËÆ¦½ôÅçÆîÉô" . "233")
	     ("ÅìµþÅÔ¡¦¾®³Þ¸¶" . "234")
	     ("¿ÀÆàÀî¸©¡¦ÅìÉô" . "261")
	     ("¿ÀÆàÀî¸©¡¦À¾Éô" . "262")
	     ("Ä¹Ìî¸©¡¦ËÌÉô" . "271")
	     ("Ä¹Ìî¸©¡¦ÃæÉô" . "272")
	     ("Ä¹Ìî¸©¡¦ÆîÉô" . "273")
	     ("»³Íü¸©¡¦ÃæÀ¾Éô" . "281")
	     ("»³Íü¸©¡¦ÅìÉôÉÙ»Î¸Þ¸Ð" . "282")
	     ("ÀÅ²¬¸©¡¦ÃæÉô" . "291")
	     ("ÀÅ²¬¸©¡¦À¾Éô" . "292")
	     ("ÀÅ²¬¸©¡¦ÅìÉô" . "293")
	     ("ÀÅ²¬¸©¡¦°ËÆ¦ÃÏÊý" . "294")
	     ("´ôÉì¸©¡¦ÈþÇ»ÃÏÊý" . "301")
	     ("´ôÉì¸©¡¦ÈôÂÍÃÏÊý" . "302")
	     ("»°½Å¸©¡¦ËÌÃæÉô" . "311")
	     ("»°½Å¸©¡¦ÆîÉô" . "312")
	     ("°¦ÃÎ¸©¡¦À¾Éô" . "321")
	     ("°¦ÃÎ¸©¡¦ÅìÉô" . "322")
	     ("µþÅÔÉÜ¡¦ÆîÉô" . "331")
	     ("µþÅÔÉÜ¡¦ËÌÉô" . "332")
	     ("Ê¼¸Ë¸©¡¦ÆîÉô" . "341")
	     ("Ê¼¸Ë¸©¡¦ËÌÉô" . "342")
	     ("ÆàÎÉ¸©¡¦ËÌÉô" . "351")
	     ("ÆàÎÉ¸©¡¦ÆîÉô" . "352")
	     ("¼¢²ì¸©¡¦ÆîÉô" . "361")
	     ("¼¢²ì¸©¡¦ËÌÉô" . "362")
	     ("ÏÂ²Î»³¸©¡¦ËÌÉô" . "371")
	     ("ÏÂ²Î»³¸©¡¦ÆîÉô" . "372")
	     ("ÂçºåÉÜ" . "381")
	     ("Ä»¼è¸©¡¦ÅìÉô" . "391")
	     ("Ä»¼è¸©¡¦À¾Éô" . "392")
	     ("Åçº¬¸©¡¦ÅìÉô" . "401")
	     ("Åçº¬¸©¡¦À¾Éô" . "402")
	     ("Åçº¬¸©¡¦±£´ô½ôÅç" . "403")
	     ("²¬»³¸©¡¦ÆîÉô" . "411")
	     ("²¬»³¸©¡¦ËÌÉô" . "412")
	     ("¹­Åç¸©¡¦ÆîÉô" . "421")
	     ("¹­Åç¸©¡¦ËÌÉô" . "422")
	     ("»³¸ý¸©¡¦À¾Éô" . "431")
	     ("»³¸ý¸©¡¦ÃæÉô" . "432")
	     ("»³¸ý¸©¡¦ÅìÉô" . "433")
	     ("»³¸ý¸©¡¦ËÌÉô" . "434")
	     ("¹áÀî¸©" . "441")
	     ("°¦É²¸©¡¦ÃæÍ½ÃÏÊý" . "451")
	     ("°¦É²¸©¡¦ÅìÍ½ÃÏÊý" . "452")
	     ("°¦É²¸©¡¦ÆîÍ½ÃÏÊý" . "453")
	     ("ÆÁÅç¸©¡¦ËÌÉô" . "461")
	     ("ÆÁÅç¸©¡¦ÆîÉô" . "462")
	     ("¹âÃÎ¸©¡¦ÃæÉô" . "471")
	     ("¹âÃÎ¸©¡¦ÅìÉô" . "472")
	     ("¹âÃÎ¸©¡¦À¾Éô" . "473")
	     ("Ê¡²¬¸©¡¦Ê¡²¬ÃÏÊý" . "481")
	     ("Ê¡²¬¸©¡¦ËÌ¶å½£ÃÏÊý" . "482")
	     ("Ê¡²¬¸©¡¦ÃÞË­ÃÏÊý" . "483")
	     ("Ê¡²¬¸©¡¦ÃÞ¸åÃÏÊý" . "484")
	     ("ÂçÊ¬¸©¡¦ÃæÉô" . "491")
	     ("ÂçÊ¬¸©¡¦ËÌÉô" . "492")
	     ("ÂçÊ¬¸©¡¦À¾Éô" . "493")
	     ("ÂçÊ¬¸©¡¦ÆîÉô" . "494")
	     ("º´²ì¸©¡¦ÆîÉô" . "501")
	     ("º´²ì¸©¡¦ËÌÉô" . "502")
	     ("·§ËÜ¸©¡¦·§ËÜÃÏÊý" . "511")
	     ("·§ËÜ¸©¡¦°¤ÁÉÃÏÊý" . "512")
	     ("·§ËÜ¸©¡¦Å·Áð°²ËÌÃÏÊý" . "513")
	     ("·§ËÜ¸©¡¦µåËáÃÏÊý" . "514")
	     ("µÜºê¸©¡¦ÆîÉôÊ¿Ìî" . "521")
	     ("µÜºê¸©¡¦ÆîÉô»³±è¤¤" . "522")
	     ("µÜºê¸©¡¦ËÌÉôÊ¿Ìî" . "523")
	     ("µÜºê¸©¡¦ËÌÉô»³±è¤¤" . "524")
	     ("Ä¹ºê¸©¡¦ÆîÉô" . "531")
	     ("Ä¹ºê¸©¡¦ËÌÉô" . "532")
	     ("Ä¹ºê¸©¡¦°í´ôÂÐÇÏÃÏÊý" . "533")
	     ("Ä¹ºê¸©¡¦¸ÞÅçÃÏÊý" . "534")
	     ("¼¯»ùÅç¸©¡¦»§ËàÃÏÊý" . "561")
	     ("¼¯»ùÅç¸©¡¦Âç¶ùÃÏÊý" . "562")
	     ("¼¯»ùÅç¸©¡¦¼ï»ÒÅç" . "563")
	     ("¼¯»ùÅç¸©¡¦²°µ×Åç" . "563")
	     ("±âÈþ½ôÅç" . "564")
	     ("²­Æì¸©¡¦ÃæÆîÉô" . "591")
	     ("²­Æì¸©¡¦ËÌÉô" . "592")
	     ("²­Æì¸©¡¦µ×ÊÆÅç" . "593")
	     ("²­Æì¸©¡¦ÂçÅìÅç" . "594")
	     ("²­Æì¸©¡¦µÜ¸ÅÅç" . "595")
	     ("²­Æì¸©¡¦ÀÐ³ÀÅç" . "596")
	     ("²­Æì¸©¡¦Í¿Æá¹ñÅç" . "597"))))
      (mapcar (lambda (area)
		(cons (car area) (format format (cdr area))))
	      alist)))
  "Associative list of regions and urls.")

(defcustom w3m-weather-default-area
  "µþÅÔÉÜ¡¦ÆîÉô"
  "Default region to check weateher."
  :group 'w3m
  :type (cons 'radio
	      (mapcar (lambda (area) (list 'const (car area)))
		      w3m-weather-url-alist)))

(defcustom w3m-weather-filter-functions
  '(w3m-weather-remove-headers
    w3m-weather-remove-footers
    w3m-weather-remove-weather-images
    w3m-weather-remove-washing-images
    w3m-weather-remove-futon-images
    w3m-weather-remove-week-weather-images
    w3m-weather-insert-title)
  "Filter functions to remove useless tags."
  :group 'w3m
  :type 'hook)

(defconst w3m-extended-charcters-table
  '(("\xa0" . " ")))

(defconst w3m-entity-alist		; html character entities and values
  '(("nbsp" . " ")
    ("gt" . ">")
    ("lt" . "<")
    ("amp" . "&")
    ("quot" . "\"")
    ("apos" . "'")))
(defvar w3m-entity-db nil)		; nil means un-initialized
(defconst w3m-entity-db-size 13)	; size of obarray

(defvar w3m-current-url nil "URL of this buffer.")
(defvar w3m-current-title nil "Title of this buffer.")
(defvar w3m-current-forms nil "Forms of this buffer.")
(defvar w3m-url-history nil "History of URL.")
(make-variable-buffer-local 'w3m-current-url)
(make-variable-buffer-local 'w3m-current-title)
(make-variable-buffer-local 'w3m-url-history)

(defvar w3m-verbose t "Flag variable to control messages.")

(defvar w3m-backlog-buffer nil)
(defvar w3m-backlog-articles nil)
(defvar w3m-backlog-hashtb nil)
(defvar w3m-input-url-history nil)

(defvar w3m-arrived-anchor-list nil)
(defvar w3m-arrived-user-list nil)

(defvar w3m-process-message nil "Function to message status.")
(defvar w3m-process-user nil)
(defvar w3m-process-passwd nil)
(defvar w3m-process-user-counter 0)
(defvar w3m-process-temp-file nil)
(make-variable-buffer-local 'w3m-process-temp-file)

(defvar w3m-bookmark-data nil)
(defvar w3m-bookmark-file-time-stamp nil)
(defvar w3m-bookmark-section-history nil)
(defvar w3m-bookmark-title-history nil)

(defconst w3m-work-buffer-name " *w3m-work*")

(defconst w3m-meta-content-type-charset-regexp
  (eval-when-compile
    (concat "<meta[ \t]+http-equiv=\"?Content-type\"?[ \t]+content=\"\\([^;]+\\)"
	    ";[ \t]*charset=\"?\\([^\"]+\\)\"?"
	    ">"))
  "Regexp used in parsing `<META HTTP-EQUIV=\"Content-Type\" content=\"...;charset=...\">
for a charset indication")

(defconst w3m-meta-charset-content-type-regexp
  (eval-when-compile
    (concat "<meta[ \t]+content=\"\\([^;]+\\)"
	    ";[ \t]*charset=\"?\\([^\"]+\\)\"?"
	    "[ \t]+http-equiv=\"?Content-type\"?>"))
  "Regexp used in parsing `<META content=\"...;charset=...\" HTTP-EQUIV=\"Content-Type\">
for a charset indication")

(eval-and-compile
  (defconst w3m-form-string-regexp
    "\\(\"\\(\\([^\"\\\\]+\\|\\\\.\\)+\\)\"\\|[^\"<> \t\r\f\n]*\\)"
    "Regexp used in parsing to detect string."))

(defconst w3m-command-arguments
  '("-T" "text/html" "-t" tab-width "-halfdump"
    "-cols" (if (< 0 w3m-fill-column)
		w3m-fill-column		; fixed columns
	      (+ (frame-width) (or w3m-fill-column -1)))) ; fit for frame
  "Arguments for execution of w3m.")

(defun w3m-message (&rest args)
  "Alternative function of `message' for w3m.el."
  (if w3m-verbose
      (apply (function message) args)
    (apply (function format) args)))

(defun w3m-sub-list (list n)
  "Make new list from LIST with top most N items.
If N is negative, last N items of LIST is returned."
  (if (< n 0)
      ;; N is negative, get items from tail of list
      (if (>= (- n) (length list))
	  (copy-sequence list)
	(nthcdr (+ (length list) n) (copy-sequence list)))
    ;; N is non-negative, get items from top of list
    (if (>= n (length list))
	(copy-sequence list)
      (nreverse (nthcdr (- (length list) n) (reverse list))))))

(defun w3m-load-list (file coding)
  "Load list from FILE with CODING and return list."
  (when (file-readable-p file)
    (with-temp-buffer
      (let ((file-coding-system-for-read coding)
	    (coding-system-for-read coding))
	(insert-file-contents file)
	(condition-case nil
	    (read (current-buffer))	; return value
	  (error nil))))))

(defun w3m-save-list (file coding list)
  "Save LIST into file with CODING."
  (when (and list (file-writable-p file))
    (with-temp-buffer
      (let ((file-coding-system coding)
	    (coding-system-for-write coding))
	(print list (current-buffer))
	(write-region (point-min) (point-max)
		      file nil 'nomsg)))))

(defun w3m-arrived-list-load ()
  "Load arrived url list from 'w3m-arrived-urls-file'."
  (setq w3m-arrived-anchor-list
	(w3m-load-list w3m-arrived-urls-file w3m-arrived-file-coding-system)))

(defun w3m-arrived-list-save ()
  "Save arrived url list to 'w3m-arrived-urls-file'."
  (w3m-save-list w3m-arrived-urls-file w3m-arrived-file-coding-system
		 (w3m-sub-list w3m-arrived-anchor-list w3m-keep-arrived-urls)))

(defun w3m-arrived-list-add (&optional url)
  "Cons url to 'w3m-arrived-anchor-list'. CAR is newest."
  (setq url (or url w3m-current-url))
  (when (> (length url) 5) ;; ignore short
    (set-text-properties 0 (length url) nil url)
    (setq w3m-arrived-anchor-list
	  (cons url (delete url w3m-arrived-anchor-list)))))


;;; Form:
(defun w3m-form-new (method action &optional baseurl)
  "Return new form object."
  (vector 'w3m-form-object
	  (if (stringp method)
	      (intern method)
	    method)
	  (if baseurl
	      (w3m-expand-url action baseurl)
	    action)
	  nil))

(defsubst w3m-form-p (obj)
  "Return t if OBJ is a form object."
  (and (vectorp obj)
       (symbolp (aref 0 obj))
       (eq (aref 0 obj) 'w3m-form-object)))

(defmacro w3m-form-method (form)
  `(aref ,form 1))
(defmacro w3m-form-action (form)
  `(aref ,form 2))
(defmacro w3m-form-plist (form)
  `(aref ,form 3))
(defmacro w3m-form-put (form name value)
  (let ((tempvar (make-symbol "formobj")))
    `(let ((,tempvar ,form))
       (aset ,tempvar 3 (plist-put (w3m-form-plist ,tempvar) (intern ,name) ,value)))))
(defmacro w3m-form-get (form name)
  `(plist-get (w3m-form-plist ,form) (intern ,name)))

(defun w3m-url-encode-string (str &optional coding)
  (apply (function concat)
	 (mapcar
	  (lambda (ch)
	    (cond
	     ((string-match "[-a-zA-Z0-9_:/]" (char-to-string ch)) ; xxx?
	      (char-to-string ch))	; printable
	     (t
	      (format "%%%02X" ch))))	; escape
	  (string-to-list
	   (encode-coding-string
	    str
	    (or coding
		(w3m-static-if (boundp 'MULE)
		    '*iso-2022-jp*
		  'iso-2022-7bit)))))))

(defun w3m-form-make-get-string (form)
  (when (eq 'get (w3m-form-method form))
    (let ((plist (w3m-form-plist form))
	  (buf))
      (while plist
	(setq buf (cons
		   (format "%s=%s"
			   (w3m-url-encode-string (symbol-name (car plist)))
			   (w3m-url-encode-string (nth 1 plist)))
		   buf)
	      plist (nthcdr 2 plist)))
      (if buf
	  (format "%s?%s"
		  (w3m-form-action form)
		  (mapconcat (function identity) buf "&"))
	(w3m-form-action form)))))

(put 'w3m-parse-attributes 'lisp-indent-function '1)
(put 'w3m-parse-attributes 'edebug-form-spec '(&rest form))
(defmacro w3m-parse-attributes (attributes &rest form)
  (` (let ((,@ (mapcar
		(lambda (attr)
		  (if (listp attr) (car attr) attr))
		attributes)))
       (while
	   (cond
	    (,@ (mapcar
		 (lambda (attr)
		   (or (symbolp attr)
		       (and (listp attr)
			    (<= (length attr) 2)
			    (symbolp (car attr)))
		       (error "Internal error, type mismatch."))
		   (let ((sexp (quote
				(or (match-string 2)
				    (match-string 1)))))
		     (when (listp attr)
		       (cond
			((eq (nth 1 attr) :case-ignore)
			 (setq sexp
			       (quote
				(downcase
				 (or (match-string 2)
				     (match-string 1))))))
			((eq (nth 1 attr) :integer)
			 (setq sexp
			       (quote
				(string-to-number
				 (or (match-string 2)
				     (match-string 1))))))
			((nth 1 attr)
			 (error "Internal error, unknown modifier.")))
		       (setq attr (car attr)))
		     (` ((looking-at
			  (, (format "%s=%s"
				     (symbol-name attr)
				     w3m-form-string-regexp)))
			 (setq (, attr) (, sexp))))))
		 attributes))
	    ((looking-at
	      (, (concat "[A-z]*=" w3m-form-string-regexp))))
	    ((looking-at "[^<> \t\r\f\n]+")))
	 (goto-char (match-end 0))
	 (skip-chars-forward " \t\r\f\n"))
       (skip-chars-forward "^>")
       (,@ form))))

(defun w3m-form-parse-region (start end)
  "Parse HTML data in this buffer and return form objects."
  (save-restriction
    (narrow-to-region start end)
    (let ((case-fold-search t)
	  forms)
      (goto-char (point-min))
      (while (re-search-forward "<\\(\\(form\\)\\|\\(input\\)\\|select\\)[ \t\r\f\n]+" nil t)
	(cond
	 ((match-string 2)
	  ;; When <FORM> is found.
	  (w3m-parse-attributes (action (method :case-ignore))
	    (setq forms
		  (cons (w3m-form-new (or method "get")
				      (or action w3m-current-url)
				      w3m-current-url)
			forms))))
	 ((match-string 3)
	  ;; When <INPUT> is found.
	  (w3m-parse-attributes (name value (type :case-ignore))
	    (when name
	      (w3m-form-put (car forms)
			    name
			    (cons value (w3m-form-get (car forms) name))))))
	 ;; When <SELECT> is found.
	 (t
	  ;; FIXME: ¤³¤ÎÉôÊ¬¤Ç¤Ï¡¢¹¹¤Ë <OPTION> ¥¿¥°¤ò½èÍý¤·¤Æ¡¢¸å¤«¤é
	  ;; ÍøÍÑ¤Ç¤­¤ë¤è¤¦¤ËÃÍ¤Î¥ê¥¹¥È¤òºîÀ®¤·¡¢ÊÝÂ¸¤·¤Æ¤ª¤¯É¬Í×¤¬¤¢
	  ;; ¤ë¡£¤·¤«¤·¡¢¤³¤ì¤ò¼ÂÁõ¤¹¤ë¤Î¤Ï¡¢¤Þ¤Ã¤È¤¦¤Ê HTML parser ¤ò
	  ;; ¼ÂÁõ¤¹¤ë¤Î¤ËÅù¤·¤¤Ï«ÎÏ¤¬É¬Í×¤Ç¤¢¤ë¤Î¤Ç¡¢º£²ó¤Ï¼êÈ´¤­¤·¤Æ
	  ;; ¤ª¤¯¡£
	  )))
      (set (make-local-variable 'w3m-current-forms) (nreverse forms)))))

(defun w3m-fontify-forms ()
  "Process half-dumped data in this buffer and fontify <input_alt> tags."
  (goto-char (point-min))
  (while (search-forward "<input_alt " nil t)
    (let (start)
      (setq start (match-beginning 0))
      (goto-char (match-end 0))
      (w3m-parse-attributes ((fid :integer)
			     (type :case-ignore)
			     (width :integer)
			     (maxlength :integer)
			     name value)
	(search-forward "</input_alt>")
	(goto-char (match-end 0))
	(let ((form (nth fid w3m-current-forms)))
	  (when form
	    (cond
	     ((string= type "submit")
	      (put-text-property start (point)
				 'w3m-action
				 `(w3m-form-submit ,form)))
	     ((string= type "reset")
	      (put-text-property start (point)
				 'w3m-action
				 `(w3m-form-reset ,form)))
	     (t ;; input button.
	      (put-text-property start (point)
				 'w3m-action
				 `(w3m-form-input ,form
						  ,name
						  ,type
						  ,width
						  ,maxlength
						  ,value))
	      (w3m-form-put form name value)))
	    (put-text-property start (point) 'face 'w3m-form-face))
	  )))))

(defun w3m-form-replace (string)
  (let* ((start (text-property-any
		 (point-min)
		 (point-max)
		 'w3m-action
		 (get-text-property (point) 'w3m-action)))
	 (width (string-width
		 (buffer-substring
		  start
		  (next-single-property-change start 'w3m-action))))
	 (prop (text-properties-at start))
	 (buffer-read-only))
    (goto-char start)
    (insert (setq string (truncate-string string width))
	    (make-string (- width (string-width string)) ?\ ))
    (delete-region (point)
		   (next-single-property-change (point) 'w3m-action))
    (add-text-properties start (point) prop)
    (point)))

;;; FIXME: ËÜÅö¤Ï type ¤ÎÃÍ¤Ë¹ç¤ï¤»¤Æ¡¢Å¬ÀÚ¤ÊÃÍ¤Î¤ß¤ò¼õ¤±ÉÕ¤±¤ë¤è¤¦¤Ë
;;; ¥Á¥§¥Ã¥¯¤·¤¿¤ê¡¢ÆþÎÏÊýË¡¤òÊÑ¤¨¤¿¤ê¤¹¤ë¤è¤¦¤Ê¼ÂÁõ¤¬É¬Í×¡£
(defun w3m-form-input (form name type width maxlength value)
  (save-excursion
    (let ((input (read-from-minibuffer
		  (concat (upcase type) ":")
		  (w3m-form-get form name))))
      (w3m-form-put form name input)
      (w3m-form-replace input))))

(defun w3m-form-submit (form)
  (let ((url (w3m-form-make-get-string form)))
    (if url
	(w3m-goto-url url)
      (w3m-message "This form's method has not been supported: %s"
		   (prin1-to-string (w3m-form-method form))))))

(defsubst w3m-form-real-reset (form sexp)
  (and (eq 'w3m-form-input (car sexp))
       (eq form (nth 1 sexp))
       (w3m-form-put form (nth 2 sexp) (nth 6 sexp))
       (w3m-form-replace (nth 6 sexp))))

(defun w3m-form-reset (form)
  (save-excursion
    (let (pos prop)
      (when (setq prop (get-text-property
			(goto-char (point-min))
			'w3m-action))
	(goto-char (or (w3m-form-real-reset form prop)
		       (next-single-property-change pos 'w3m-action))))
      (while (setq pos (next-single-property-change (point) 'w3m-action))
	(goto-char pos)
	(goto-char (or (w3m-form-real-reset form (get-text-property pos 'w3m-action))
		       (next-single-property-change pos 'w3m-action)))))))


;;; HTML character entity handling:

(defun w3m-entity-db-setup ()
  ;; initialize entity database (obarray)
  (setq w3m-entity-db (make-vector w3m-entity-db-size 0))
  (dolist (elem w3m-entity-alist)
    (set (intern (car elem) w3m-entity-db)
	 (cdr elem))))

(defun w3m-entity-value (name)
  ;; initialize if need
  (if (null w3m-entity-db)
      (w3m-entity-db-setup))
    ;; return value of specified entity, or empty string for unknown entity.
    (or (symbol-value (intern-soft (match-string 1) w3m-entity-db))
	(if (not (char-equal (string-to-char name) ?#))
	    (concat "&" name)		; unknown entity
	  ;; case of immediate character (accept only 0x20 .. 0x7e)
	  (let ((char (string-to-int (substring name 1)))
		sym)
	    ;; make character's representation with learning
	    (set (setq sym (intern name w3m-entity-db))
		 (if (or (< char 32) (< 127 char))
		     "~"		; un-supported character
		   (char-to-string char)))))))

(defun w3m-fontify-bold ()
  "Fontify bold characters in this buffer which contains half-dumped data."
  (goto-char (point-min))
  (while (search-forward "<b>" nil t)
    (let ((start (match-beginning 0)))
      (delete-region start (match-end 0))
      (when (search-forward "</b>" nil t)
	(delete-region (match-beginning 0) (match-end 0))
	(put-text-property start (match-beginning 0) 'face 'bold)))))

(defun w3m-fontify-underline ()
  "Fontify underline characters in this buffer which contains half-dumped data."
  (goto-char (point-min))
  (while (search-forward "<u>" nil t)
    (let ((start (match-beginning 0)))
      (delete-region start (match-end 0))
      (when (search-forward "</u>" nil t)
	(delete-region (match-beginning 0) (match-end 0))
	(put-text-property start (match-beginning 0) 'face 'underline)))))

(defun w3m-fontify-anchors ()
  "Fontify anchor tags in this buffer which contains half-dumped data."
  ;; Delete excessive `hseq' elements of anchor tags.
  (goto-char (point-min))
  (while (re-search-forward "<a\\( hseq=\"[-0-9]+\"\\)" nil t)
    (delete-region (match-beginning 1) (match-end 1)))
  ;; Re-ordering anchor elements.
  (goto-char (point-min))
  (let (href)
    (while (re-search-forward "<a\\([ \t\n]\\)[^>]+[ \t\n]href=\\(\"?[^\" >]*\"?\\)" nil t)
      (setq href (buffer-substring (match-beginning 2) (match-end 2)))
      (delete-region (match-beginning 2) (match-end 2))
      (goto-char (match-beginning 1))
      (insert " href=" href)))
  ;; Fontify anchor tags.
  (goto-char (point-min))
  (while (re-search-forward
	  "<a\\([ \t\n]+href=\"?\\([^\" >]*\\)\"?\\)?\\([ \t\n]+name=\"?\\([^\" >]*\\)\"?\\)?[^>]*>"
	  nil t)
    (let ((url (match-string 2))
	  (tag (match-string 4))
	  (start (match-beginning 0))
	  (end))
      (delete-region start (match-end 0))
      (cond (url
	     (when (search-forward "</a>" nil t)
	       (setq url (w3m-expand-url url w3m-current-url))
	       (delete-region (setq end (match-beginning 0)) (match-end 0))
	       (if (member url w3m-arrived-anchor-list)
		   (put-text-property start end 'face 'w3m-arrived-anchor-face)
		 (put-text-property start end 'face 'w3m-anchor-face))
	       (put-text-property start end 'w3m-href-anchor url)
	       (put-text-property start end 'mouse-face 'highlight))
	     (when tag
	       (put-text-property start end 'w3m-name-anchor tag)))
	    (tag
	     (when (re-search-forward "<\\|\n" nil t)
	       (setq end (match-beginning 0))
	       (put-text-property start end 'w3m-name-anchor tag)))))))

(defun w3m-fontify-images ()
  "Fontify image alternate strings in this buffer which contains half-dumped data."
  (goto-char (point-min))
  (while (re-search-forward "<img_alt src=\"\\([^\"]*\\)\">" nil t)
    (let ((src (match-string 1))
	  (start (match-beginning 0))
	  (end))
      (delete-region start (match-end 0))
      (when (search-forward "</img_alt>" nil t)
	(delete-region (setq end (match-beginning 0)) (match-end 0))
	(setq src (w3m-expand-url src w3m-current-url))
	(put-text-property start end 'face 'w3m-image-face)
	(put-text-property start end 'w3m-image src)
	(put-text-property start end 'mouse-face 'highlight)))))

(defun w3m-fontify ()
  "Fontify this buffer."
  (let ((case-fold-search t)
	(buffer-read-only))
    (run-hooks 'w3m-fontify-before-hook)
    ;; Delete <?xml ... ?> tag
    (goto-char (point-min))
    (if (search-forward "<?xml" nil t)
	(let ((start (match-beginning 0)))
	  (search-forward "?>" nil t)
	  (delete-region start (match-end 0))))
    ;; Delete extra title tag.
    (goto-char (point-min))
    (let (start)
      (and (search-forward "<title>" nil t)
	   (setq start (match-beginning 0))
	   (search-forward "</title>" nil t)
	   (delete-region start (match-end 0))))
    (w3m-fontify-bold)
    (w3m-fontify-underline)
    (w3m-fontify-anchors)
    (if w3m-use-form
	(w3m-fontify-forms))
    (w3m-fontify-images)
    ;; Remove other markups.
    (goto-char (point-min))
    (while (re-search-forward "</?[A-z][^>]*>" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    ;; Decode escaped characters (entities).
    (goto-char (point-min))
    (let (prop)
      (while (re-search-forward "&\\([a-z]+\\|#[0-9]+\\);?" nil t)
	(setq prop (text-properties-at (match-beginning 0)))
	(replace-match (w3m-entity-value (match-string 1)) nil t)
	(if prop (add-text-properties (match-beginning 0) (point) prop))))
    ;; Decode w3m-specific extended charcters.
    (let ((x (w3m-static-if (boundp 'MULE)
		 mc-flag
	       enable-multibyte-characters)))
      (set-buffer-multibyte nil)
      (dolist (elem w3m-extended-charcters-table)
	(goto-char (point-min))
	(while (search-forward (car elem) nil t)
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert (cdr elem))))
      (set-buffer-multibyte x))
    (goto-char (point-min))
    (if w3m-delete-duplicated-empty-lines
	(while (re-search-forward "^[ \t]*\n\\([ \t]*\n\\)+" nil t)
	  (replace-match "\n" nil t)))
    (run-hooks 'w3m-fontify-after-hook)))

;;

(defun w3m-refontify-anchor (&optional buff)
  "Change face 'w3m-anchor-face to 'w3m-arrived-anchor-face."
  (save-excursion
    (and buff (set-buffer buff))
    (when (and (eq major-mode 'w3m-mode)
	       (eq (get-text-property (point) 'face) 'w3m-anchor-face))
      (let* ((start)
	     (end (next-single-property-change (point) 'face))
	     (buffer-read-only))
	(when (and end
		   (setq start (previous-single-property-change end 'face)))
	  (put-text-property start end 'face 'w3m-arrived-anchor-face))
	(set-buffer-modified-p nil)))))

(defun w3m-input-url (&optional prompt default)
  "Read a URL from the minibuffer, prompting with string PROMPT."
  (let (url candidates)
    (w3m-backlog-setup)
    (or w3m-input-url-history
	(setq w3m-input-url-history (or w3m-arrived-anchor-list
					(w3m-arrived-list-load))))
    (mapatoms (lambda (x)
		(setq candidates (cons (cons (symbol-name x) x) candidates)))
	      w3m-backlog-hashtb)
    (setq default (or default (thing-at-point 'url)))
    (setq prompt (or prompt
		     (if default "URL: "
		       (format "URL (default %s): " w3m-home-page))))
    (setq url (completing-read prompt candidates nil nil default
			       'w3m-input-url-history w3m-home-page))
    ;; remove duplication
    (setq w3m-input-url-history (cons url (delete url w3m-input-url-history)))
    ;; return value
    url))


;;; Backlog:
(defun w3m-backlog-setup ()
  "Initialize backlog variables."
  (unless (and (bufferp w3m-backlog-buffer)
	       (buffer-live-p w3m-backlog-buffer))
    (save-excursion
      (set-buffer (get-buffer-create " *w3m backlog*"))
      (buffer-disable-undo)
      (set-buffer-multibyte nil)
      (setq buffer-read-only t
	    w3m-backlog-buffer (current-buffer))))
  (unless w3m-backlog-hashtb
    (setq w3m-backlog-hashtb (make-vector 1021 0))))

(defun w3m-backlog-shutdown ()
  "Clear all backlog variables and buffers."
  (when (get-buffer w3m-backlog-buffer)
    (kill-buffer w3m-backlog-buffer))
  (setq w3m-backlog-hashtb nil
	w3m-backlog-articles nil))

(defun w3m-backlog-enter (url type charset buffer)
  "Store URL's data which is placed in the BUFFER.
Return symbol to identify its backlog data."
  (w3m-backlog-setup)
  (let ((ident (intern url w3m-backlog-hashtb)))
    (w3m-backlog-remove url)
    ;; Remove the oldest article, if necessary.
    (and (numberp w3m-keep-backlog)
	 (>= (length w3m-backlog-articles) w3m-keep-backlog)
	 (w3m-backlog-remove-oldest))
    ;; Insert the new article.
    (save-excursion
      (set-buffer w3m-backlog-buffer)
      (let (buffer-read-only)
	(goto-char (point-max))
	(unless (bolp) (insert "\n"))
	(let ((b (point)))
	  (insert-buffer-substring buffer)
	  ;; Tag the beginning of the article with the ident.
	  (when (> (point-max) b)
	    (put-text-property b (1+ b) 'w3m-backlog ident)
	    (put-text-property b (1+ b) 'w3m-backlog-type type)
	    (put-text-property b (1+ b) 'w3m-backlog-charset charset)
	    (setq w3m-backlog-articles (cons ident w3m-backlog-articles)))
	  )))))

(defun w3m-backlog-remove-oldest ()
  (save-excursion
    (set-buffer w3m-backlog-buffer)
    (goto-char (point-min))
    (if (zerop (buffer-size))
	()				; The buffer is empty.
      (let ((ident (get-text-property (point) 'w3m-backlog))
	    buffer-read-only)
	;; Remove the ident from the list of articles.
	(when ident
	  (setq w3m-backlog-articles (delq ident w3m-backlog-articles)))
	;; Delete the article itself.
	(delete-region (point)
		       (next-single-property-change
			(1+ (point)) 'w3m-backlog nil (point-max)))))))

(defun w3m-backlog-remove (url)
  "Remove URL's data from the backlog."
  (w3m-backlog-setup)
  (let ((ident (intern url w3m-backlog-hashtb))
	beg end)
    (when (memq ident w3m-backlog-articles)
      ;; It was in the backlog.
      (save-excursion
	(set-buffer w3m-backlog-buffer)
	(let (buffer-read-only)
	  (when (setq beg (text-property-any
			   (point-min) (point-max) 'w3m-backlog ident))
	    ;; Find the end (i. e., the beginning of the next article).
	    (setq end (next-single-property-change
		       (1+ beg) 'w3m-backlog (current-buffer) (point-max)))
	    (delete-region beg end)))
	(setq w3m-backlog-articles (delq ident w3m-backlog-articles))))))

(defun w3m-backlog-request (url &optional buffer)
  "Insert URL's data to the BUFFER.
If URL's data is found in the backlog, return a list of URL's content
type and URL's charcter set name.  Otherwise return nil.  When BUFFER
is nil, all data will be inserted in the current buffer."
  (w3m-backlog-setup)
  (let ((ident (intern url w3m-backlog-hashtb)))
    (when (memq ident w3m-backlog-articles)
      ;; It was in the backlog.
      (let (beg end type charset)
	(save-excursion
	  (set-buffer w3m-backlog-buffer)
	  (if (setq beg (text-property-any
			 (point-min) (point-max) 'w3m-backlog ident))
	      ;; Find the end (i. e., the beginning of the next article).
	      (setq type (get-text-property beg 'w3m-backlog-type)
		    charset (get-text-property beg 'w3m-backlog-charset)
		    end (next-single-property-change
			 (1+ beg) 'w3m-backlog (current-buffer) (point-max)))
	    ;; It wasn't in the backlog after all.
	    (setq w3m-backlog-articles (delq ident w3m-backlog-articles))))
	(and beg
	     end
	     (save-excursion
	       (and buffer (set-buffer buffer))
	       (let (buffer-read-only)
		 (insert-buffer-substring w3m-backlog-buffer beg end))
	       (list type charset)))))))


;;; Handle process:
(defun w3m-exec-process (&rest args)
  (save-excursion
    (let ((coding-system-for-read
	   (w3m-static-if (boundp 'MULE) '*noconv* 'binary))
	  (coding-system-for-write w3m-coding-system)
	  (default-process-coding-system
	    (cons (w3m-static-if (boundp 'MULE) '*noconv* 'binary)
		  w3m-coding-system))
	  (process-connection-type w3m-process-connection-type))
      (if w3m-async-exec
	  ;; start-process
	  (let ((w3m-process-user)
		(w3m-process-passwd)
		(w3m-process-user-counter 2)
		(proc (apply 'start-process w3m-command (current-buffer) w3m-command args)))
	    (set-process-filter proc 'w3m-exec-filter)
	    (set-process-sentinel proc (lambda (proc event) nil))
	    (process-kill-without-query proc)
	    (while (eq (process-status proc) 'run)
	      (if (functionp w3m-process-message)
		  (funcall w3m-process-message))
	      (sit-for 0.2)
	      (discard-input))
	    (and w3m-current-url
		 w3m-process-user
		 (setq w3m-arrived-user-list
		       (cons
			(cons w3m-current-url
			      (list w3m-process-user w3m-process-passwd))
			(delete (assoc w3m-current-url w3m-arrived-user-list)
				w3m-arrived-user-list)))))
	;; call-process
	(apply 'call-process w3m-command nil t nil args)))))

(defun w3m-exec-get-user (url)
  (if (= w3m-process-user-counter 0)
      nil
    (catch 'get
      (dolist (elem w3m-arrived-user-list nil)
	(when (string-match (concat "^" (regexp-quote (car elem))) url)
	  (setq w3m-process-user-counter (1- w3m-process-user-counter))
	  (throw 'get (cdr elem)))))))

(defun w3m-read-file-name (&optional prompt dir default existing initial)
  (let* ((default (and default (file-name-nondirectory default)))
	 (prompt (or prompt
		     (if default (format "Save to (%s): " default) "Save to: ")))
	 (initial (or initial default))
	 (dir (file-name-as-directory (or dir w3m-default-save-directory)))
	 (default-directory dir)
	 (file (read-file-name prompt dir default existing initial)))
    (if (not (file-directory-p file))
	(setq w3m-default-save-directory
	      (or (file-name-directory file) w3m-default-save-directory))
      (setq w3m-default-save-directory file)
      (if default
	  (setq file (expand-file-name default file))))
    (expand-file-name file)))

(defun w3m-read-passwd (prompt)
  (let ((inhibit-input-event-recording t))
    (if (fboundp 'read-passwd)
	(condition-case nil
	    (read-passwd prompt)
	  (error ""))
      (let ((pass "")
	    (c 0)
	    (echo-keystrokes 0)
	    (ociea cursor-in-echo-area))
	(condition-case nil
	    (progn
	      (setq cursor-in-echo-area 1)
	      (while (and (/= c ?\r) (/= c ?\n) (/= c ?\e) (/= c 7)) ;; ^G
		(message "%s%s"
			 prompt
			 (make-string (length pass) ?.))
		(setq c (read-char-exclusive))
		(cond
		 ((char-equal c ?\C-u)
		  (setq pass ""))
		 ((or (char-equal c ?\b) (char-equal c ?\177))  ;; BS DELL
		  ;; delete one character in the end
		  (if (not (equal pass ""))
		      (setq pass (substring pass 0 -1))))
		 ((< c 32) ()) ;; control, just ignore
		 (t
		  (setq pass (concat pass (char-to-string c))))))
	      (setq cursor-in-echo-area -1))
	  (quit
	   (setq cursor-in-echo-area ociea)
	   (signal 'quit nil))
	  (error
	   ;; Probably not happen. Just align to the code above.
	   (setq pass "")))
	(setq cursor-in-echo-area ociea)
	(message "")
	(sit-for 0)
	pass))))

(defun w3m-exec-filter (process string)
  (if (buffer-name (process-buffer process))
      (with-current-buffer (process-buffer process)
	(let ((buffer-read-only nil)
	      (case-fold-search nil)
	      (str))
	  (goto-char (process-mark process))
	  (insert string)
	  (set-marker (process-mark process) (point))
	  (forward-line 0)
	  (cond
	   ((looking-at "Username: Password: ")
	    (setq w3m-process-passwd
		  (or (nth 1 (w3m-exec-get-user w3m-current-url))
		      (w3m-read-passwd "Password: "))
		  str w3m-process-passwd))
	   ((looking-at "Username: ")
	    (setq w3m-process-user
		  (or (nth 0 (w3m-exec-get-user w3m-current-url))
		      (read-from-minibuffer "Username: "))
		  str w3m-process-user)))
	  (if str
	      (process-send-string process (concat str "\n")))))))


;;; Handle character sets:
(defun w3m-charset-to-coding-system (charset)
  "Return coding-system corresponding with CHARSET.
CHARSET is a symbol whose name is MIME charset.
This function is imported from mcharset.el."
  (if (stringp charset)
      (setq charset (intern (downcase charset))))
  (let ((cs (assq charset w3m-charset-coding-system-alist)))
    (setq cs (if cs (cdr cs) charset))
    (if (find-coding-system cs)
	cs)))

(defun w3m-decode-buffer (type charset)
  (if (and (not charset) (string= type "text/html"))
      (setq charset
	    (let ((case-fold-search t))
	      (goto-char (point-min))
	      (and (or (re-search-forward
			w3m-meta-content-type-charset-regexp nil t)
		       (re-search-forward
			w3m-meta-charset-content-type-regexp nil t))
		   (buffer-substring-no-properties (match-beginning 2)
						   (match-end 2))))))
  (decode-coding-region
   (point-min) (point-max)
   (if charset
       (w3m-charset-to-coding-system charset)
     (let ((default (condition-case nil
			(coding-system-category w3m-coding-system)
		      (error nil)))
	   (candidate (detect-coding-region (point-min) (point-max))))
       (unless (listp candidate)
	 (setq candidate (list candidate)))
       (catch 'coding
	 (dolist (coding candidate)
	   (if (eq default (coding-system-category coding))
	       (throw 'coding coding)))
	 (if (eq (coding-system-category 'binary)
		 (coding-system-category (car candidate)))
	     w3m-coding-system
	   (car candidate))))))
  (set-buffer-multibyte t))


;;; Retrieve local data:
(defun w3m-local-content-type (url)
  (catch 'type-detected
    (dolist (elem w3m-content-type-alist "unknown")
      (if (string-match (nth 1 elem) url)
	  (throw 'type-detected (car elem))))))

(defun w3m-local-retrieve (url &optional no-decode accept-type-regexp)
  "Retrieve content of local URL and insert it to the working buffer.
This function will return content-type of URL as string when retrieval
succeed.  If NO-DECODE, set the multibyte flag of the working buffer
to nil.  Only contents whose content-type matches ACCEPT-TYPE-REGEXP
are retrieved."
  (let ((type (w3m-local-content-type url))
	(file))
    (when (or (not accept-type-regexp)
	      (string-match accept-type-regexp type))
      (setq file (w3m-url-to-file-name url))
      (with-current-buffer (get-buffer-create w3m-work-buffer-name)
	(delete-region (point-min) (point-max))
	(if (and (string-match "^text/" type)
		 (not no-decode))
	    (progn
	      (set-buffer-multibyte t)
	      (insert-file-contents file))
	  (set-buffer-multibyte nil)
	  (let ((coding-system-for-read
		 (w3m-static-if (boundp 'MULE) '*noconv* 'binary))
		(file-coding-system-for-read
		 (w3m-static-if (boundp 'MULE) '*noconv* 'binary)))
	    (insert-file-contents file)))))
    type))


;;; Retrieve data via HTTP:
(defun w3m-remove-redundant-spaces (str)
  "Remove spaces/tabs at the front of a string and at the end of a string"
  (save-match-data
    (if (string-match "^[ \t\r\f\n]+" str)
	(setq str (substring str (match-end 0))))
    (if (string-match "[ \t\r\f\n]+$" str)
	(setq str (substring str 0 (match-beginning 0)))))
  str)

(defun w3m-w3m-check-header (url)
  "Ask the header of the URL to HTTP server."
  (with-current-buffer (get-buffer-create w3m-work-buffer-name)
    (delete-region (point-min) (point-max))
    (let ((w3m-current-url url)
	  (case-fold-search t)
	  length type charset)
      (w3m-message "Request sent, waiting for response...")
      (w3m-exec-process "-dump_head" url)
      (w3m-message "Request sent, waiting for response... done")
      (goto-char (point-min))
      (if (re-search-forward "^content-type:\\([^\r\n]+\\)\r*$" nil t)
	  (progn
	    (setq type (match-string 1))
	    (if (string-match ";[ \t]*charset=" type)
		(setq charset (w3m-remove-redundant-spaces
			       (substring type (match-end 0)))
		      type (w3m-remove-redundant-spaces
			    (substring type 0 (match-beginning 0))))
	      (setq type (w3m-remove-redundant-spaces type)))))
      (goto-char (point-min))
      (if (re-search-forward "^content-length:\\([^\r\n]+\\)\r*$" nil t)
	  (setq length (string-to-number (match-string 1))))
      (list (or type (w3m-local-content-type url) "unknown")
	    charset
	    length))))

(defun w3m-pretty-length (n)
  ;; This function imported from url.el.
  (cond
   ((< n 1024)
    (format "%d bytes" n))
   ((< n (* 1024 1024))
    (format "%dk" (/ n 1024.0)))
   (t
    (format "%2.2fM" (/ n (* 1024 1024.0))))))

(defun w3m-w3m-retrieve (url &optional no-decode accept-type-regexp no-cache)
  "Retrieve content of URL with w3m and insert it to the working buffer.
This function will return content-type of URL as string when retrieval
succeed.  If NO-DECODE, set the multibyte flag of the working buffer
to nil.  Only contents whose content-type matches ACCEPT-TYPE-REGEXP
are retrieved."
  (with-current-buffer (get-buffer-create w3m-work-buffer-name)
    (delete-region (point-min) (point-max))
    (set-buffer-multibyte nil)
    (or
     (unless no-cache
       (let ((headers (w3m-backlog-request url)))
	 (if headers
	     (let ((type (car headers))
		   (charset (nth 1 headers)))
	       (when (or (not accept-type-regexp)
			 (string-match accept-type-regexp type))
		 (and (string-match "^text/" type)
		      (not no-decode)
		      (w3m-decode-buffer type charset)))
	       type))))
     (let ((headers (w3m-w3m-check-header url)))
       (if headers
	   (let ((type    (car headers))
		 (charset (nth 1 headers))
		 (length  (nth 2 headers)))
	     (when (or (not accept-type-regexp)
		       (string-match accept-type-regexp type))
	       (let* ((w3m-current-url url)
		      (w3m-w3m-retrieve-length length)
		      (w3m-process-message
		       (lambda ()
			 (if w3m-w3m-retrieve-length
			     (w3m-message
			      "Reading... %s of %s (%d%%)"
			      (w3m-pretty-length (buffer-size))
			      (w3m-pretty-length w3m-w3m-retrieve-length)
			      (/ (* (buffer-size) 100) w3m-w3m-retrieve-length))
			   (w3m-message "Reading... %s"
					(w3m-pretty-length (buffer-size)))))))
		 (w3m-message "Reading...")
		 (delete-region (point-min) (point-max))
		 (w3m-exec-process "-dump_source" url)
		 (w3m-message "Reading... done"))
	       (if length
		   (if (eq w3m-executable-type 'cygwin)
		       (cond
			;; No authentication and no bugs in output.
			((= (buffer-size) length))
			;; No authentication but new-line character is replaced to CRLF.
			((= (buffer-size)
			    (+ length (count-lines (point-min) (point-max))))
			 (while (search-forward "\r\n" nil t)
			   (delete-region (- (point) 2) (1- (point)))))
			(t;; Authentication
			 (while (and
				 (re-search-forward "^Username: Password: \n" nil t)
				 (cond
				  ((= (- (point-max) (point)) length)
				   (delete-region (point-min) (point))
				   nil)
				  ((= (- (point-max) (point))
				      (+ length (count-lines (point) (point-max))))
				   (delete-region (point-min) (point))
				   (while (search-forward "\r\n" nil t)
				     (delete-region (- (point) 2) (1- (point))))))))))
		     (delete-region (point-min) (- (point-max) length)))
		 (when (string= "text/html" type)
		   ;; Remove cookies.
		   (goto-char (point-min))
		   (while (and (not (eobp))
			       (looking-at "Received cookie: "))
		     (forward-line 1))
		   (skip-chars-forward " \t\r\f\n")
		   (if (or (looking-at "<!DOCTYPE")
			   (looking-at "<HTML>")) ; for eGroups.
		       (delete-region (point-min) (point)))))
	       (w3m-backlog-enter url type charset (current-buffer))
	       (and (string-match "^text/" type)
		    (not no-decode)
		    (w3m-decode-buffer type charset)))
	     type))))))

(defun w3m-retrieve (url &optional no-decode accept-type-regexp no-cache)
  "Retrieve content of URL and insert it to the working buffer.
This function will return content-type of URL as string when retrieval
succeed.  If NO-DECODE, set the multibyte flag of the working buffer
to nil.  Only contents whose content-type matches ACCEPT-TYPE-REGEXP
are retrieved."
  (cond
   ((string-match "^about:" url)
    (let (func)
      (if (and (string-match "^about://\\([^/]+\\)/" url)
	       (setq func (intern-soft
			   (concat "w3m-about-" (match-string 1 url))))
	       (fboundp func))
	  (funcall func url no-decode accept-type-regexp no-cache)
	(w3m-about url no-decode accept-type-regexp no-cache))))
   ((string-match "^\\(file:\\|/\\)" url)
    (w3m-local-retrieve url no-decode accept-type-regexp))
   (t
    (w3m-w3m-retrieve url no-decode accept-type-regexp no-cache))))

(defun w3m-download (url &optional filename no-cache)
  (unless filename
    (setq filename (w3m-read-file-name nil nil url)))
  (w3m-retrieve url t nil no-cache)
  (with-current-buffer (get-buffer w3m-work-buffer-name)
    (let ((buffer-file-coding-system
	   (w3m-static-if (boundp 'MULE) '*noconv* 'binary))
	  (coding-system-for-write
	   (w3m-static-if (boundp 'MULE) '*noconv* 'binary)))
      (if (or (not (file-exists-p filename))
	      (y-or-n-p (format "File(%s) is aleready exists. Overwrite? " filename)))
	  (write-region (point-min) (point-max) filename)))))

(defun w3m-content-type (url)
  (cond
   ((string-match "^about:" url) "text/html")
   ((string-match "^\\(file:\\|/\\)" url)
    (w3m-local-content-type url))
   (t (car (w3m-w3m-check-header url)))))


;;; Retrieve data via FTP:
(defun w3m-exec-ftp (url)
  (let ((ftp (w3m-convert-ftp-to-emacsen url))
	(file (file-name-nondirectory url)))
    (if (string-match "\\(\\.gz\\|\\.bz2\\|\\.zip\\|\\.lzh\\)$" file)
	(copy-file ftp (w3m-read-file-name nil nil file))
      (dired-other-window ftp))))

(defun w3m-convert-ftp-to-emacsen (url)
  (or (and (string-match "^ftp://?\\([^/@]+@\\)?\\([^/]+\\)\\(/~/\\)?" url)
	   (concat "/"
		   (if (match-beginning 1)
		       (substring url (match-beginning 1) (match-end 1))
		     "anonymous@")
		   (substring url (match-beginning 2) (match-end 2))
		   ":"
		   (substring url (match-end 2))))
      (error "URL is strange.")))

(defun w3m-rendering-region (start end)
  "Rendering data in current buffer as HTML."
  (let ((coding-system-for-read w3m-output-coding-system)
	(coding-system-for-write w3m-input-coding-system)
	(default-process-coding-system
	  (cons w3m-output-coding-system w3m-input-coding-system)))
    (w3m-message "Rendering...")
    (if w3m-use-form
	(w3m-form-parse-region start end))
    (apply 'call-process-region
	   start end w3m-command t t nil
	   (mapcar (lambda (x)
		     (if (stringp x)
			 x
		       (prin1-to-string (eval x))))
		   w3m-command-arguments))
    (goto-char (point-min))
    (w3m-message "Rendering... done")
    (let (title)
      (mapcar (lambda (regexp)
		(goto-char 1)
		(when (re-search-forward regexp nil t)
		  (setq title (match-string 1))
		  (delete-region (match-beginning 0) (match-end 0))))
	      '("<title_alt[ \t\n]+title=\"\\([^\"]+\\)\">"
		"<title>\\([^<]\\)</title>"))
      (if (and (null title)
	       (stringp w3m-current-url)
	       (< 0 (length (file-name-nondirectory w3m-current-url))))
	  (setq title (file-name-nondirectory w3m-current-url)))
      (setq w3m-current-title (or title "<no-title>")))))

(defun w3m-exec (url &optional buffer no-cache)
  "Download URL with w3m to the BUFFER.
If BUFFER is nil, all data is placed to the current buffer.  When new
content is retrieved and hald-dumped data is placed in the BUFFER,
this function returns t.  Otherwise, returns nil."
  (save-excursion
    (if buffer (set-buffer buffer))
    (if (and (string-match "^ftp://" url)
	     (not (string= "text/html" (w3m-local-content-type url))))
	(progn (w3m-exec-ftp url) nil)
      (let ((type (w3m-retrieve url nil "^text/" no-cache)))
	(if (string-match "^text/" type)
	    (let (buffer-read-only)
	      (setq w3m-current-url url)
	      (setq w3m-url-history (cons url w3m-url-history))
	      (setq-default w3m-url-history
			    (cons url (default-value 'w3m-url-history)))
	      (delete-region (point-min) (point-max))
	      (insert-buffer w3m-work-buffer-name)
	      (if (string= "text/html" type)
		  (progn (w3m-rendering-region (point-min) (point-max)) t)
		(setq w3m-current-title (file-name-nondirectory url))
		nil))
	  (w3m-message "Requested URL has an unsuitable content type: %s" type)
	  nil)))))


(defun w3m-search-name-anchor (name &optional quiet)
  (interactive "sName: ")
  (let ((pos (point-min)))
    (catch 'found
      (while (setq pos (next-single-property-change pos 'w3m-name-anchor))
	(when (equal name (get-text-property pos 'w3m-name-anchor))
	  (goto-char pos)
	  (throw 'found t))
	(setq pos (next-single-property-change pos 'w3m-name-anchor)))
      (unless quiet
	(message "Not found such name anchor."))
      nil)))


(defun w3m-save-position (url)
  (if url
      (let ((ident (intern-soft url w3m-backlog-hashtb)))
	(when ident
	  (set ident (cons (window-start) (point)))))))

(defun w3m-restore-position (url)
  (let ((ident (intern-soft url w3m-backlog-hashtb)))
    (when (and ident (boundp ident))
      (set-window-start nil (car (symbol-value ident)))
      (goto-char (cdr (symbol-value ident))))))


(defun w3m-view-previous-page (&optional arg)
  (interactive "p")
  (unless arg (setq arg 1))
  (let ((url (nth arg w3m-url-history)))
    (when url
      (let (w3m-url-history) (w3m-goto-url url))
      ;; restore last position
      (w3m-restore-position url)
      (setq w3m-url-history
	    (nthcdr arg w3m-url-history)))))

(defun w3m-view-previous-point ()
  (interactive)
  (w3m-restore-position w3m-current-url))

(defun w3m-expand-url (url base)
  "Convert URL to absolute, and canonicalize it."
  (save-match-data
    (if (not base) (setq base ""))
    (if (string-match "^[^:/]+://[^/]*$" base)
	(setq base (concat base "/")))
    (cond
     ;; URL is relative on BASE.
     ((string-match "^#" url)
      (concat base url))
     ;; URL has absolute spec.
     ((string-match "^[^:/]+:" url)
      url)
     ((string-match "^/" url)
      (if (string-match "^\\([^:/]+://[^/]*\\)/" base)
	  (concat (match-string 1 base) url)
	url))
     (t
      (let ((server "") path)
	(if (string-match "^\\([^:]+://[^/]*\\)/" base)
	    (setq server (match-string 1 base)
		  base (substring base (match-end 1))))
	(setq path (expand-file-name url (file-name-directory base)))
	;; remove drive (for Win32 platform)
	(if (string-match "^.:" path)
	    (setq path (substring path (match-end 0))))
	(concat server path))))))

(defsubst w3m-anchor (&optional point)
  (get-text-property (or point (point)) 'w3m-href-anchor))

(defsubst w3m-image (&optional point)
  (get-text-property (or point (point)) 'w3m-image))
 
(defsubst w3m-action (&optional point)
  (get-text-property (or point (point)) 'w3m-action))
 
(defun w3m-view-this-url (&optional arg)
  "*View the URL of the link under point."
  (interactive "P")
  (let ((url (w3m-anchor)) (act (w3m-action)))
    (cond
     (url (w3m-goto-url url arg))
     (act (eval act)))))

(defun w3m-mouse-view-this-url (event)
  (interactive "e")
  (mouse-set-point event)
  (let ((url (w3m-anchor)) (img (w3m-image)))
    (cond
     (url (w3m-view-this-url))
     (img (w3m-view-image))
     (t (message "No URL at point.")))))

(defun w3m-external-view (url)
  (let ((method (nth 2 (assoc (w3m-content-type url) w3m-content-type-alist))))
    (cond
     ((not method)
      (error "Unknown content type: %s" (w3m-content-type url)))
     ((functionp method)
      (funcall method url))
     ((consp method)
      (let ((command (car method))
	    (arguments (cdr method))
	    (file (make-temp-name
		   (expand-file-name "w3mel" w3m-profile-directory)))
	    (proc))
	(unwind-protect
	    (with-current-buffer
		(generate-new-buffer " *w3m-external-view*")
	      (if (memq 'file arguments) (w3m-download url file))
	      (setq proc
		    (apply 'start-process
			   "w3m-external-view"
			   (current-buffer)
			   command
			   (mapcar (function eval) arguments)))
	      (setq w3m-process-temp-file file)
	      (set-process-sentinel
	       proc
	       (lambda (proc event)
		 (and (string-match "^\\(finished\\|exited\\)" event)
		      (buffer-name (process-buffer proc))
		      (save-excursion
			(set-buffer (process-buffer proc))
			(if (file-exists-p w3m-process-temp-file)
			    (delete-file w3m-process-temp-file)))
		      (kill-buffer (process-buffer proc))))))
	  (if (file-exists-p file)
	      (unless (and (processp proc)
			   (memq (process-status proc) '(run stop)))
		(delete-file file)))))))))

(defun w3m-view-image ()
  "*View the image under point."
  (interactive)
  (let ((url (w3m-image)))
    (if url
	(w3m-external-view url)
      (message "No file at point."))))

(defun w3m-save-image ()
  "*Save the image under point to a file."
  (interactive)
  (let ((url (w3m-image)))
    (if url
	(w3m-download url)
      (message "No file at point."))))

(defun w3m-view-current-url-with-external-browser ()
  "*View this URL."
  (interactive)
  (let ((url (w3m-anchor)))
    (or url
	(y-or-n-p (format "Browse <%s> ? " w3m-current-url))
	(setq url w3m-current-url))
    (when url
      (message "Browse <%s>" url)
      (w3m-external-view url))))

(defun w3m-download-this-url ()
  "*Download the URL of the link under point to a file."
  (interactive)
  (let ((url (w3m-anchor)))
    (if url
	(progn
	  (w3m-download url)
	  (w3m-refontify-anchor (current-buffer)))
      (message "No URL at point."))))

(defun w3m-print-current-url ()
  "*Print the URL of current page and push it into kill-ring."
  (interactive)
  (kill-new w3m-current-url)
  (message "%s" w3m-current-url))

(defun w3m-print-this-url ()
  "*Print the URL of the link under point."
  (interactive)
  (let ((url (w3m-anchor)))
    (message "%s" (or url "Not found"))))

(defun w3m-save-this-url ()
  (interactive)
  (let ((url (w3m-anchor)))
    (if url (kill-new url))))

(defun w3m-goto-next-anchor ()
  ;; move to the end of the current anchor
  (when (w3m-anchor)
    (goto-char (next-single-property-change (point) 'w3m-href-anchor)))
  ;; find the next anchor
  (or (w3m-anchor)
      (let ((pos (next-single-property-change (point) 'w3m-href-anchor)))
	(if pos (progn (goto-char pos) t) nil))))

(defun w3m-next-anchor (&optional arg)
  "*Move cursor to the next anchor."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (w3m-previous-anchor (- arg))
    (while (> arg 0)
      (unless (w3m-goto-next-anchor)
	;; search from the beginning of the buffer
	(goto-char (point-min))
	(w3m-goto-next-anchor))
      (setq arg (1- arg)))
    (w3m-print-this-url)))

(defun w3m-goto-previous-anchor ()
  ;; move to the beginning of the current anchor
  (when (w3m-anchor)
    (goto-char (previous-single-property-change (1+ (point))
						'w3m-href-anchor)))
  ;; find the previous anchor
  (let ((pos (previous-single-property-change (point) 'w3m-href-anchor)))
    (if pos (goto-char
	     (if (w3m-anchor pos) pos
	       (previous-single-property-change pos 'w3m-href-anchor))))))

(defun w3m-previous-anchor (&optional arg)
  "Move cursor to the previous anchor."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      (w3m-next-anchor (- arg))
    (while (> arg 0)
      (unless (w3m-goto-previous-anchor)
	;; search from the end of the buffer
	(goto-char (point-max))
	(w3m-goto-previous-anchor))
      (setq arg (1- arg)))
    (w3m-print-this-url)))


(defun w3m-view-bookmark ()
  (interactive)
  (if (file-readable-p w3m-bookmark-file)
      (w3m (w3m-expand-file-name-as-url w3m-bookmark-file))))


(defun w3m-copy-buffer (buf &optional newname and-pop) "\
Create a twin copy of the current buffer.
if NEWNAME is nil, it defaults to the current buffer's name.
if AND-POP is non-nil, the new buffer is shown with `pop-to-buffer'."
  (interactive (list (current-buffer)
		     (if current-prefix-arg (read-string "Name: "))
		     t))
  (setq newname (or newname (buffer-name)))
  (if (string-match "<[0-9]+>\\'" newname)
      (setq newname (substring newname 0 (match-beginning 0))))
  (with-current-buffer buf
    (let ((ptmin (point-min))
	  (ptmax (point-max))
	  (content (save-restriction (widen) (buffer-string)))
	  (mode major-mode)
	  (lvars (buffer-local-variables))
	  (new (generate-new-buffer (or newname (buffer-name)))))
      (with-current-buffer new
	;;(erase-buffer)
	(insert content)
	(narrow-to-region ptmin ptmax)
	(funcall mode)			;still needed??  -sm
	(mapcar (lambda (v)
		  (if (not (consp v)) (makunbound v)
		    (condition-case ()	;in case var is read-only
			(set (make-local-variable (car v)) (cdr v))
		      (error nil))))
		lvars)
	(when and-pop (pop-to-buffer new))
	new))))


(defvar w3m-mode-map nil)
(unless w3m-mode-map
  (let ((map (make-keymap)))
    (define-key map " " 'scroll-up)
    (define-key map "b" 'scroll-down)
    (define-key map [backspace] 'scroll-down)
    (define-key map [delete] 'scroll-down)
    (define-key map "h" 'backward-char)
    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)
    (define-key map "l" 'forward-char)
    (define-key map "J" (lambda () (interactive) (scroll-up 1)))
    (define-key map "K" (lambda () (interactive) (scroll-up -1)))
    (define-key map "G" 'goto-line)
    (define-key map "\C-?" 'scroll-down)
    (define-key map "\t" 'w3m-next-anchor)
    (define-key map [(shift tab)] 'w3m-previous-anchor)
    (define-key map [down] 'w3m-next-anchor)
    (define-key map "\M-\t" 'w3m-previous-anchor)
    (define-key map [up] 'w3m-previous-anchor)
    (define-key map "\C-m" 'w3m-view-this-url)
    (define-key map [right] 'w3m-view-this-url)
    (if (featurep 'xemacs)
	(define-key map [(button2)] 'w3m-mouse-view-this-url)
      (define-key map [mouse-2] 'w3m-mouse-view-this-url))
    (define-key map "\C-c\C-b" 'w3m-view-previous-point)
    (define-key map [left] 'w3m-view-previous-page)
    (define-key map "B" 'w3m-view-previous-page)
    (define-key map "d" 'w3m-download-this-url)
    (define-key map "u" 'w3m-print-this-url)
    (define-key map "I" 'w3m-view-image)
    (define-key map "\M-I" 'w3m-save-image)
    (define-key map "c" 'w3m-print-current-url)
    (define-key map "M" 'w3m-view-current-url-with-external-browser)
    (define-key map "g" 'w3m)
    (define-key map "U" 'w3m)
    (define-key map "V" 'w3m)
    (define-key map "v" 'w3m-view-bookmark)
    (define-key map "q" 'w3m-quit)
    (define-key map "Q" (lambda () (interactive) (w3m-quit t)))
    (define-key map "\M-n" 'w3m-copy-buffer)
    (define-key map "R" 'w3m-reload-this-page)
    (define-key map "?" 'describe-mode)
    (define-key map "\M-a" 'w3m-bookmark-add-this-url)
    (define-key map "a" 'w3m-bookmark-add-current-url)
    (define-key map ">" 'w3m-scroll-left)
    (define-key map "<" 'w3m-scroll-right)
    (setq w3m-mode-map map)))


(defun w3m-quit (&optional force)
  (interactive "P")
  (when (or force
	    (y-or-n-p "Do you want to exit w3m? "))
    (kill-buffer (current-buffer))
    (w3m-arrived-list-save)
    (or (save-excursion
	  ;; Check existing w3m buffers.
	  (delq nil (mapcar (lambda (b)
			      (set-buffer b)
			      (eq major-mode 'w3m-mode))
			    (buffer-list))))
	;; If no w3m buffer exists, then destruct all cache.
	(w3m-backlog-shutdown))))


(defun w3m-mode ()
  "\\<w3m-mode-map>
   Major mode to browsing w3m buffer.

\\[w3m-view-this-url]	View this url.
\\[w3m-mouse-view-this-url]	View this url.
\\[w3m-reload-this-page]	Reload this page.
\\[w3m-next-anchor]	Jump to next anchor.
\\[w3m-previous-anchor]	Jump to previous anchor.
\\[w3m-view-previous-page]	Back to previous page.

\\[w3m-download-this-url]	Download this url.
\\[w3m-print-this-url]	Print this url.
\\[w3m-view-image]	View image.
\\[w3m-save-image]	Save image.

\\[w3m-print-current-url]	Print current url.
\\[w3m-view-current-url-with-external-browser]	View current url with external browser.

\\[scroll-up]	Scroll up.
\\[scroll-down]	Scroll down.
\\[w3m-scroll-left]	Scroll to left.
\\[w3m-scroll-right]	Scroll to right.

\\[next-line]	Next line.
\\[previous-line]	Previous line.

\\[forward-char]	Forward char.
\\[backward-char]	Backward char.

\\[goto-line]	Jump to line.
\\[w3m-view-previous-point]	w3m-view-previous-point.

\\[w3m]	w3m.
\\[w3m-view-bookmark]	w3m-view-bookmark.
\\[w3m-copy-buffer]	w3m-copy-buffer.

\\[w3m-quit]	w3m-quit.
\\[describe-mode]	describe-mode.
"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'w3m-mode)
  (setq mode-name "w3m")
  (use-local-map w3m-mode-map)
  (setq truncate-lines t)
  (run-hooks 'w3m-mode-hook))

(defun w3m-scroll-left (arg)
  "Scroll to left.
Scroll size is `w3m-horizontal-scroll-size' columns
or prefix ARG columns."
  (interactive "P")
  (scroll-left (if arg
		   (prefix-numeric-value arg)
		 w3m-horizontal-scroll-columns)))

(defun w3m-scroll-right (arg)
  "Scroll to right.
Scroll size is `w3m-horizontal-scroll-size' columns
or prefix ARG columns."
  (interactive "P")
  (scroll-right (if arg
		    (prefix-numeric-value arg)
		  w3m-horizontal-scroll-columns)))

(defun w3m-mailto-url (url)
  (if (and (symbolp w3m-mailto-url-function)
	   (fboundp w3m-mailto-url-function))
      (funcall w3m-mailto-url-function url)
    (let (comp)
      ;; Require `mail-user-agent' setting
      (if (not (and (boundp 'mail-user-agent)
		    mail-user-agent
		    (setq comp (intern-soft (concat (symbol-name mail-user-agent)
						    "-compose")))
		    (fboundp comp)))
	  (error "You must specify valid `mail-user-agent'."))
      ;; Use rfc2368.el if exist.
      ;; rfc2368.el is written by Sen Nagata.
      ;; You can find it in "contrib" directory of Mew package
      ;; or in "utils" directory of Wanderlust package.
      (if (or (featurep 'rfc2368)
	      (condition-case nil (require 'rfc2368) (error nil)))
	  (let ((info (rfc2368-parse-mailto-url url)))
	    (apply comp (mapcar (lambda (x)
				  (cdr (assoc x info)))
				'("To" "Subject"))))
	;; without rfc2368.el.
	(funcall comp (match-string 1 url))))))


(defun w3m-goto-url (url &optional reload)
  "Retrieve URL and display it in this buffer."
  (let (name)
    (cond
     ;; process mailto: protocol
     ((string-match "^mailto:\\(.*\\)" url)
      (w3m-mailto-url url))
     (t
      (when (string-match "#\\([^#]+\\)$" url)
	(setq name (match-string 1 url)
	      url (substring url 0 (match-beginning 0))))
      (w3m-save-position w3m-current-url)
      (or w3m-arrived-anchor-list (w3m-arrived-list-load))
      (w3m-arrived-list-add url)
      (if (not (w3m-exec url nil reload))
	  (w3m-refontify-anchor)
	(w3m-fontify)
	(setq buffer-read-only t)
	(set-buffer-modified-p nil)
	(or (and name (w3m-search-name-anchor name))
	    (goto-char (point-min))))))))


(defun w3m-reload-this-page ()
  "Reload current page without cache."
  (interactive)
  (setq w3m-url-history (cdr w3m-url-history))
  (w3m-goto-url w3m-current-url 'reload))


(defun w3m (url &optional args)
  "Interface for w3m on Emacs."
  (interactive (list (w3m-input-url)))
  (set-buffer (get-buffer-create "*w3m*"))
  (or (eq major-mode 'w3m-mode)
      (w3m-mode))
  (setq mode-line-buffer-identification
	(list "%b" " / " 'w3m-current-title))
  (w3m-goto-url url)
  (switch-to-buffer (current-buffer))
  (run-hooks 'w3m-hook))


(defun w3m-browse-url (url &optional new-window)
  "w3m interface function for browse-url.el."
  (interactive
   (progn
     (require 'browse-url)
     (browse-url-interactive-arg "w3m URL: ")))
  (if new-window (split-window))
  (w3m url))

(defun w3m-find-file (file)
  "w3m Interface function for local file."
  (interactive "fFilename: ")
  (w3m (w3m-expand-file-name-as-url file)))

;; bookmark operations

(defun w3m-bookmark-file-modified-p ()
  "Predicate for FILE is something modified."
  (and (file-exists-p w3m-bookmark-file)
       w3m-bookmark-file-time-stamp
       (not (equal (elt (file-attributes w3m-bookmark-file) 5)
		   w3m-bookmark-file-time-stamp))))

;; bookmark data format
;; bookmark has some section.
;; section has some entries.
;; entry is pair of link name (title) and url
;; for example:
;; bookmark := ( ("My Favorites"                    ; section
;;                ( "title1" . "http://foo.com/" )  ; entry
;;                ( "title2" . "http://bar.org/" )) ; entry
;;               ("For Study"                       ; section
;;                ( "Title3" . "http://baz.net/" )) ; entry
;;
;; In w3m, section is h2 level contents. (HTML/BODY/H2)
;; entry is A item (represented as UL/LI)
;; `w3m-bookmark-parse' assumes above
;;  because this is easy implementation. :-)

(defun w3m-bookmark-parse ()
  "Parse current buffer and returns bookmark data alist for internal."
  (let (bookmark tag url str)
    (goto-char 1)
    (while (re-search-forward
	    "<\\(h2\\|a\\)\\( *href=\"\\([^\"]+\\)\"[^>]*\\)?>" nil t)
      (setq tag (match-string 1))
      (cond
       ((string= tag "h2")
	;; make new section (in top)
	(setq bookmark (cons (list (buffer-substring
				    (match-end 0)
				    (progn (re-search-forward "</h2>")
					   (match-beginning 0))))
			     bookmark)))
       ((string= tag "a")
	(if (null (match-beginning 2))
	    (error "parse error, href attribute is expected."))
	(setq url (match-string 3)
	      str (buffer-substring (match-end 0)
				    (progn
				      (re-search-forward "</a>")
				      (match-beginning 0))))
	(setcar bookmark (cons (cons str url) (car bookmark))))
       (t (error "parse error, unknown tag is matched."))))
    ;; reverse entries and sections
    (nreverse (mapcar 'nreverse bookmark))))



(defun w3m-bookmark-load (&optional file)
  "Load bookmark from FILE.
Parsed bookmark data is hold in `w3m-bookmark-data'."
  (or file
      (setq file w3m-bookmark-file))	; default name
  (message "Loading bookmarks...")
  (with-temp-buffer
    (if (file-exists-p w3m-bookmark-file)
	(insert-file-contents w3m-bookmark-file))
    ;; parse
    (setq w3m-bookmark-data (w3m-bookmark-parse)
	  w3m-bookmark-file-time-stamp (elt (file-attributes file) 5)))
  (message "Loading bookmarks...done"))


(defun w3m-bookmark-save (&optional file)
  "Save internal bookmark data into bookmark file as w3m format."
  (or file
      (setq file w3m-bookmark-file))	; default name
  ;; check output directory
  (if (not (file-writable-p file))
      (message "Can't write! Bookmark file is not saved.")
    ;;
    (with-temp-buffer
      ;; print beginning of html
      (insert "<html><head><title>Bookmarks</title></head>\n"
	      "<body>\n"
	      "<h1>Bookmarks</h1>\n")
      (dolist (entries w3m-bookmark-data)
	(insert "<h2>" (car entries) "</h2>\n"
		"<ul>\n")
	(dolist (ent (cdr entries))
	  (insert "<li><a href=\"" (cdr ent) "\">" (car ent) "</a>\n"))
	(insert "<!--End of section (do not delete this comment)-->\n"
		"</ul>\n"))
      ;; print end of html
      (insert "</body>\n"
	      "</html>\n")
      ;; write to file!
      (let ((coding-system-for-write w3m-bookmark-file-coding-system))
	(write-region (point-min) (point-max) file))
      (setq w3m-bookmark-file-time-stamp (elt (file-attributes file) 5))
      (message "Saved to '%s'" file))))

(defun w3m-bookmark-data-prepare ()
  "Prepare for bookmark operation.
If bookmark data is not loaded, load it.
If bookmark file is modified since last load, ask reloading."
  (if (and (null w3m-bookmark-file-time-stamp)
	   (null w3m-bookmark-data)
	   (file-exists-p w3m-bookmark-file))
      (w3m-bookmark-load w3m-bookmark-file)))


(defun w3m-bookmark-add (url &optional title section)
  "Add URL to bookmark data and save it to file.
Optional argument TITLE is title of link.
SECTION is category name in bookmark."
  (let (sec ent)
    (w3m-bookmark-data-prepare)
    ;; check time stamp of bookmark file (localy modified?)
    (if (and (w3m-bookmark-file-modified-p)
	     (y-or-n-p "Bookmark file is modified. Reload it? (y/n): "))
	(w3m-bookmark-load))
    ;; go on ...
    ;; ask section (with completion).
    (setq sec (completing-read
	       "Section: "
	       w3m-bookmark-data nil nil nil
	       'w3m-bookmark-section-history ))
    (if (string-match sec "^ *$")
	(error "You must specify section name."))
    ;; ask title (with default)
    (setq title (read-string "Title: " title 'w3m-bookmark-title-history))
    (if (string-match sec "^ *$")
	(error "You must specify title."))
    ;; add it to internal bookmark data.
    (if (setq ent (assoc sec w3m-bookmark-data))
	;; add to existing section
	(nconc ent (list (cons title url))) ; add to tail
      ;; add as new section
      (setq w3m-bookmark-data
	    (nconc w3m-bookmark-data
		   (list (list sec (cons title url))))))
    ;; then save to file. (xxx, force saving, should we ask?)
    (w3m-bookmark-save)))


(defun w3m-bookmark-add-this-url ()
  "Add link under cursor to bookmark."
  (interactive)
  (if (null (w3m-anchor))
      (message "No anchor.")		; nothing to do
    (let ((url (w3m-anchor))
	  (title (buffer-substring-no-properties
		  (previous-single-property-change (1+ (point))
						   'w3m-href-anchor)
		  (next-single-property-change (point) 'w3m-href-anchor))))
      (w3m-bookmark-add url title))
    (message "Added.")))


(defun w3m-bookmark-add-current-url (&optional arg)
  "Add link of current page to bookmark.
With prefix, ask new url to add instead of current page."
  (interactive "P")
  (w3m-bookmark-add (if (null arg) w3m-current-url (w3m-input-url))
		    w3m-current-title)
  (message "Added."))


(defun w3m-cygwin-path (path)
  "Convert win32 path into cygwin format.
ex.) c:/dir/file => //c/dir/file"
  (if (string-match "^\\([A-Za-z]\\):" path)
      (replace-match "//\\1" nil nil path)
    path))


(defun w3m-region (start end)
  "Render region in current buffer and replace with result."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (w3m-rendering-region start end)
    (w3m-fontify)))

(defun w3m-escape-query-string (str &optional coding)
  (mapconcat
   (lambda (s)
     (w3m-url-encode-string s coding))
   (split-string str)
   "+"))

(defun w3m-search (search-engine query)
  "Search QUERY using SEARCH-ENGINE.
When called interactively wich prefix argument, you can choose search
engine deinfed in `w3m-search-engine-alist'.  Otherwise use
`w3m-default-search-engine'."
  (interactive
   (let ((engine
	  (if current-prefix-arg
	      (completing-read
	       (format "Which Engine? (%s): " w3m-default-search-engine)
	       w3m-search-engine-alist nil t)
	    w3m-default-search-engine)))
     (list engine
	   (read-string (format "%s search: " engine)))))
  (unless (string= query "")
    (let ((info (assoc search-engine w3m-search-engine-alist)))
      (if info
	  (w3m (format (cadr info)
		       (w3m-escape-query-string query (caddr info))))
	(error "Unknown search engine: %s" search-engine)))))


;;; About:
(defun w3m-about (url &rest args)
  (save-excursion
    (set-buffer (get-buffer-create w3m-work-buffer-name))
    (delete-region (point-min) (point-max)))
  "text/html")


;;; Weather:
(defun w3m-weather (area)
  "*Display weather report."
  (interactive
   (list (if current-prefix-arg
	     (completing-read "Input area: " w3m-weather-url-alist nil t)
	   w3m-weather-default-area)))
  (w3m (format "about://weather/%s" area)))

(defun w3m-about-weather (url &rest args)
  (let (area furl)
    (if (and (string-match "^about://weather/" url)
	     (setq area (substring url (match-end 0))
		   furl (cdr (assoc area w3m-weather-url-alist))))
	(save-excursion
	  (w3m-retrieve furl)
	  (set-buffer w3m-work-buffer-name)
	  (run-hook-with-args 'w3m-weather-filter-functions area)
	  "text/html")
      (w3m-message "Unknown URL: %s" url)
      nil)))

(defun w3m-weather-remove-headers (&rest args)
  "Remove header of the weather forecast page."
  (goto-char (point-min))
  (when (search-forward "<!-- area_s_title -->" nil t)
    (delete-region (point-min) (point))
    (when (search-forward "<img src=\"/common/clear.gif\"")
      (let ((start))
	(and (search-backward "<tr>" nil t)
	     (setq start (point))
	     (search-forward "</tr>" nil t)
	     (delete-region start (point)))))))

(defun w3m-weather-remove-footers (&rest args)
  "Remove footer of the weather forecast page."
  (goto-char (point-max))
  (when (search-backward "<!-- /area_7days -->" nil t)
    (delete-region (point) (point-max))
    (forward-line -2)
    (when (looking-at "<div")
      (delete-region (point) (point-max)))))

(defun w3m-weather-remove-weather-images (&rest args)
  "Remove images which stand for weather forecasts."
  (let ((case-fold-search t) start end)
    (goto-char (point-min))
    (and (re-search-forward
	  "\\(<td[^>]*>Å·µ¤</td>\\)[ \t\r\f\n]*<td[^>]*><img src=\"/weather/images/"
	  nil t)
	 (setq start (match-beginning 1)
	       end (match-end 1))
	 (search-forward
	  "<tr bgcolor=\"#FFFFFF\">"
	  (prog2 (forward-line 5) (point) (goto-char (match-end 0)))
	  t)
	 (progn
	   (delete-region end (point))
	   (goto-char start)
	   (when (re-search-forward "\\([ \t\r\f\n]rowspan=\"[0-9]+\"\\)[> \t\r\f\n]" end t)
	     (delete-region (match-beginning 1) (match-end 1)))))))

(defun w3m-weather-remove-washing-images (&rest args)
  "Remove images which stand for washing index."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (while (re-search-forward
	    "<td[^>]*>\\(<img src=\"/weather/images/wash[-0-9]*.gif\"[^>]*><br>\\)"
	    nil t)
      (delete-region (match-beginning 1) (match-end 1)))))

(defun w3m-weather-remove-futon-images (&rest args)
  "Remove images which stand for futon index."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (while (re-search-forward
	    "<td[^>]*>\\(<img src=\"/weather/images/bed[-0-9]*.gif\"[^>]*><br>\\)"
	    nil t)
      (delete-region (match-beginning 1) (match-end 1)))))

(defun w3m-weather-remove-week-weather-images (&rest args)
  "Remove images which stand for the weather forecast for the week."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (while (re-search-forward
	    "<td[^>]*>\\(<img src=\"/weather/images/tk[0-9]*.gif\"[^>]*><br>\\)"
	    nil t)
      (delete-region (match-beginning 1) (match-end 1)))))

(defun w3m-weather-insert-title (area &rest args)
  "Insert title."
  (goto-char (point-min))
  (insert "<head><title>Weather forecast of " area "</title></head><body>")
  (goto-char (point-max))
  (insert "</body>"))


(provide 'w3m)
;;; w3m.el ends here.

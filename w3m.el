;;; w3m.el --- Interface program of w3m on Emacs

;; Copyright (C) 2000,2001 TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>,
;;          Shun-ichi GOTO     <gotoh@taiyo.co.jp>,
;;          Satoru Takabayashi <satoru-t@is.aist-nara.ac.jp>,
;;          Hideyuki SHIRAI    <shirai@meadowy.org>,
;;          Keisuke Nishida    <kxn30@po.cwru.edu>,
;;          Yuuichi Teranishi  <teranisi@gohome.org>,
;;          Akihiro Arisawa    <ari@mbf.sphere.ne.jp>,
;;          Katsumi Yamaoka    <yamaoka@jpl.org>
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

;; In the top level directory of the w3m distribution, run the program
;; `configure' and then type `make install'.  See README file for more
;; information.


;;; Code:

(eval-and-compile
  (cond
   ((featurep 'xemacs)
    (require 'w3m-xmas))
   ((and (boundp 'emacs-major-version)
	 (>= emacs-major-version 21))
    (require 'w3m-e21))
   ((boundp 'MULE)
    (require 'w3m-om))))

(require 'w3m-hist)
(require 'thingatpt)
(require 'timezone)

;; this package using a few CL macros
(eval-when-compile
  (require 'cl))

;; Override the macro `dolist' which may have been defined in egg.el.
(eval-when-compile
  (unless (dolist (var nil t))
    (load "cl-macs" nil t)))

(put 'w3m-static-if 'lisp-indent-function 2)
(eval-and-compile
  (defmacro w3m-static-if (cond then &rest else)
    (if (eval cond) then (` (progn  (,@ else)))))
  (defmacro w3m-static-cond (&rest clauses)
    (while (and clauses
		(not (eval (car (car clauses)))))
      (setq clauses (cdr clauses)))
    (if clauses
	(cons 'progn (cdr (car clauses))))))

(w3m-static-cond ((fboundp 'find-coding-system))
		 ((fboundp 'coding-system-p)
		  (defsubst find-coding-system (obj)
		    "Return OBJ if it is a coding-system."
		    (if (coding-system-p obj) obj)))
		 (t
		  (require 'pces)))

(unless (fboundp 'w3m-read-coding-system)
  (defalias 'w3m-read-coding-system 'read-coding-system))

;; Add-on programs:
(eval-and-compile
  (autoload 'w3m-bookmark-view "w3m-bookmark" nil t)
  (autoload 'w3m-bookmark-add-this-url "w3m-bookmark"
    "*Add link under cursor to bookmark." t)
  (autoload 'w3m-bookmark-add-current-url "w3m-bookmark"
    "*Add link of current page to bookmark." t)
  (autoload 'w3m-search "w3m-search"
    "*Search QUERY using SEARCH-ENGINE." t)
  (autoload 'w3m-weather "w3m-weather"
    "*Display weather report." t)
  (autoload 'w3m-about-weather "w3m-weather")
  (autoload 'w3m-antenna "w3m-antenna"
    "*Display antenna report." t)
  (autoload 'w3m-about-antenna "w3m-antenna")
  (autoload 'w3m-fontify-forms "w3m-form")
  (autoload 'w3m-form-parse-region "w3m-form")
  (autoload 'w3m-filter "w3m-filter"))

;; Avoid byte-compile warnings.
(eval-when-compile
  (autoload 'rfc2368-parse-mailto-url "rfc2368")
  (autoload 'w3m-remove-image (if (featurep 'xemacs)
				  "w3m-xmas"
				"w3m-e21")))

(defconst emacs-w3m-version
  (eval-when-compile
    (let ((rev "$Revision$"))
      (and (string-match "\\.\\([0-9]+\\) \$$" rev)
	   (format "0.2.%d"
		   (- (string-to-number (match-string 1 rev)) 28)))))
  "Version number of this package.")

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

(defcustom w3m-command-arguments '()
  "*Arguments for execution of w3m."
  :group 'w3m
  :type '(repeat string))

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

(defcustom w3m-coding-system 'euc-japan
  "*Coding system for w3m."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-input-coding-system 'iso-2022-jp
  "*Coding system for w3m."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-output-coding-system 'euc-japan
  "*Coding system for w3m."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-key-binding
  nil
  "*This variable decides default key mapping used in w3m-mode buffers."
  :group 'w3m
  :type '(choice
	  (const :tag "Use Info-like key mapping." info)
	  (other :tag "Use Lynx-like key mapping." nil)))

(defcustom w3m-use-cygdrive (eq system-type 'windows-nt)
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

(defcustom w3m-display-inline-image nil
  "*Display inline images."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-icon-directory
  (if (fboundp 'locate-data-directory)
      (locate-data-directory "w3m")
    (let ((icons (expand-file-name "w3m/icons/"
				   data-directory)))
      (if (file-directory-p icons)
	  icons)))
  "*Icon directory for w3m (XEmacs or Emacs 21)."
  :group 'w3m
  :type 'directory)


;; Generic functions:
(defun w3m-url-to-file-name (url)
  "Return the file name which is pointed by URL."
  ;; Remove scheme part and net_loc part.  NOTE: This function accepts
  ;; only urls whose net_loc part is empty or NULL string.
  (when (string-match "^\\(file:\\(//\\)?\\)/" url)
    (setq url (substring url (match-end 1))))
  ;; Process abs_path part in Windows.
  (when (string-match "^/\\(\\([A-z]\\)[|:]?\\|cygdrive/\\([A-z]\\)\\)/" url)
    (setq url (concat
	       (or (match-string 2 url)
		   (match-string 3 url))
	       ":/"
	       (substring url (match-end 0)))))
  url)

(defun w3m-expand-file-name-as-url (file &optional directory)
  "Return URL which points the FILE."
  ;; if filename is cygwin format,
  ;; then remove cygdrive prefix before expand-file-name
  (if directory
      (setq file (w3m-url-to-file-name file)))
  ;; expand to file scheme url considering Win32 environment
  (setq file (expand-file-name file directory))
  (if (string-match "^\\(.\\):\\(.*\\)" file)
      (if w3m-use-cygdrive
	  (concat "file:///cygdrive/"
		  (match-string 1 file) (match-string 2 file))
	(concat "file:///" (match-string 1 file) "|" (match-string 2 file)))
    (concat "file://" file)))

(defcustom w3m-home-page
  (or (getenv "HTTP_HOME")
      (getenv "WWW_HOME")
      "about:")
  "*Home page of w3m.el."
  :group 'w3m
  :type 'string)

(defcustom w3m-arrived-file
  (expand-file-name ".arrived" w3m-profile-directory)
  "*File which has list of arrived URLs."
  :group 'w3m
  :type 'file)

(defcustom w3m-arrived-file-coding-system 'euc-japan
  "*Coding system for arrived file."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-keep-arrived-urls 500
  "*Arrived keep count of w3m."
  :group 'w3m
  :type 'integer)

(defcustom w3m-keep-cache-size 300
  "*Cache size of w3m."
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

;; FIXME: 本当は mailcap を適切に読み込んで設定する必要がある
(defcustom w3m-content-type-alist
  (if (eq system-type 'windows-nt)
      '(("text/plain" "\\.\\(txt\\|tex\\|el\\)" nil)
	("text/html" "\\.s?html?$" w3m-w32-browser-with-fiber)
	("image/jpeg" "\\.jpe?g$" ("fiber.exe" file))
	("image/png" "\\.png$" ("fiber.exe" file))
	("image/gif" "\\gif$" ("fiber.exe" file))
	("image/tiff" "\\tif?f$" ("fiber.exe" file))
	("image/x-xwd" "\\.xwd$" ("fiber.exe" file))
	("image/x-xbm" "\\.xbm$" ("fiber.exe" file))
	("image/x-xpm" "\\.xpm$" ("fiber.exe" file))
	("image/x-bmp" "\\.bmp$" ("fiber.exe" file))
	("video/mpeg" "\\.mpe?g$" ("fiber.exe" file))
	("video/quicktime" "\\.mov$" ("fiber.exe" file))
	("application/postscript" "\\.\\(ps\\|eps\\)$" ("fiber.exe" file))
	("application/pdf" "\\.pdf$" ("fiber.exe" file)))
    (cons
     (list "text/html" "\\.s?html?$"
	   (if (and (condition-case nil (require 'browse-url) (error nil))
		    (fboundp 'browse-url-netscape))
	       'browse-url-netscape
	     '("netscape" url)))
     '(("text/plain" "\\.\\(txt\\|tex\\|el\\)" nil)
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
       ("application/pdf" "\\.pdf$" ("acroread" file)))))
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

(defcustom w3m-decoder-alist
  '((gzip "gunzip" nil)
    (bzip "bunzip2" nil)
    (deflate "inflate" nil))
  "Associative list of DECODER."
  :group 'w3m
  :type '(repeat
	  (list (choice :tag "Encoding"
			(const gzip)
			(const bzip)
			(const deflate))
		(string :tag "Command")
		(repeat :tag "Arguments" string))))

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
	(fn (if (fboundp 'find-coding-system)
		;; It might be unbound at run-time.
		'find-coding-system
	      'coding-system-p))
	dest)
    (while rest
      (or (funcall fn (car (car rest)))
	  (setq dest (cons (car rest) dest)))
      (setq rest (cdr rest)))
    dest)
  "Alist MIME CHARSET vs CODING-SYSTEM.
MIME CHARSET and CODING-SYSTEM must be symbol."
  :group 'w3m
  :type '(repeat (cons symbol coding-system)))

(defcustom w3m-horizontal-scroll-columns 10
  "*Column size to scroll horizontally."
  :group 'w3m
  :type 'integer)

(defcustom w3m-use-form nil
  "*Non-nil means form extension is activated. (EXPERIMENTAL)"
  :group 'w3m
  :type 'boolean
  :require 'w3m-form)

(defcustom w3m-use-filter nil
  "*Non nil means filtering of WEB is used."
  :group 'w3m
  :type 'boolean
  :require 'w3m-filter)

(defcustom w3m-mnc nil
  "*When using w3m with M.N.C. patch, set non-nil value."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-linefeed-type
  (and (null w3m-mnc)
       (memq system-type '(windows-nt OS/2 emx))
       'crlf)
  "*Linefeed type of w3m program output.
Value is 'crlf or nil.
'crlf is mainly used for win32 or OS/2 environment if 'w3m-mnc' is nil."
  :group 'w3m
  :type '(choice (const :tag "CRLF" crlf) (const :tag "native" nil)))

(defcustom w3m-track-mouse t
  "Whether to track the mouse and message the url under the mouse.
This feature does not work under Emacs or XEmacs versions prior to 21.
See also the documentation for the variable `show-help-function' if
you are using Emacs 21.  You can also use the `balloon-help' feature
under XEmacs by the command M-x balloon-help-mode with arg 1.  If the
window manager decorates the balloon-help frame, and that is not to
your taste, you may strip it off with the following directives.

For ol[v]wm use this in .Xdefaults:
   olvwm.NoDecor: balloon-help
     or
   olwm.MinimalDecor: balloon-help

For fvwm version 1 use this in your .fvwmrc:
   NoTitle balloon-help
or
   Style \"balloon-help\" NoTitle, NoHandles, BorderWidth 0

For twm use this in your .twmrc:
   NoTitle { \"balloon-help\" }

See the file balloon-help.el for more information."
  :group 'w3m
  :type 'boolean)

(defconst w3m-extended-characters-table
  '(("\xa0" . " ")
    ("\x80" . " ")))

(eval-and-compile
  (defconst w3m-entity-alist		; html character entities and values
    (eval-when-compile
      (let ((basic-entity-alist
	     '(("nbsp" . " ")
	       ("gt" . ">")
	       ("lt" . "<")
	       ("amp" . "&")
	       ("quot" . "\"")
	       ("apos" . "'")))
	    (latin1-entity
	     '(				;("nbsp" . 160)
	       ("iexcl" . 161) ("cent" . 162) ("pound" . 163)
	       ("curren" . 164) ("yen" . 165) ("brvbar" . 166) ("sect" . 167)
	       ("uml" . 168) ("copy" . 169) ("ordf" . 170) ("laquo" . 171)
	       ("not" . 172)  ("shy" . 173) ("reg" . 174) ("macr" . 175)
	       ("deg" . 176) ("plusmn" . 177) ("sup2" . 178) ("sup3" . 179)
	       ("acute" . 180) ("micro" . 181) ("para" . 182) ("middot" . 183)
	       ("cedil" . 184) ("sup1" . 185) ("ordm" . 186) ("raquo" . 187)
	       ("frac14" . 188) ("frac12" . 189) ("frac34" . 190) ("iquest" . 191)
	       ("Agrave" . 192) ("Aacute" . 193) ("Acirc" . 194) ("Atilde" . 195)
	       ("Auml" . 196) ("Aring" . 197) ("AElig" . 198) ("Ccedil" . 199)
	       ("Egrave" . 200) ("Eacute" . 201) ("Ecirc" . 202) ("Euml" . 203)
	       ("Igrave" . 204) ("Iacute" . 205) ("Icirc" . 206) ("Iuml" . 207)
	       ("ETH"  . 208) ("Ntilde" . 209) ("Ograve" . 210) ("Oacute" . 211)
	       ("Ocirc" . 212) ("Otilde" . 213) ("Ouml" . 214) ("times" . 215)
	       ("Oslash" . 216) ("Ugrave" . 217) ("Uacute" . 218) ("Ucirc" . 219)
	       ("Uuml" . 220) ("Yacute" . 221) ("THORN" . 222) ("szlig" . 223)
	       ("agrave" . 224) ("aacute" . 225) ("acirc" . 226) ("atilde" . 227)
	       ("auml" . 228) ("aring" . 229) ("aelig" . 230) ("ccedil" . 231)
	       ("egrave" . 232) ("eacute" . 233) ("ecirc" . 234) ("euml" . 235)
	       ("igrave" . 236) ("iacute" . 237) ("icirc" . 238) ("iuml" . 239)
	       ("eth" . 240) ("ntilde" . 241) ("ograve" . 242) ("oacute" . 243)
	       ("ocirc" . 244) ("otilde" . 245) ("ouml" . 246) ("divide" . 247)
	       ("oslash" . 248) ("ugrave" . 249) ("uacute" . 250) ("ucirc" . 251)
	       ("uuml" . 252) ("yacute" . 253) ("thorn" . 254) ("yuml" . 255))))
	(append basic-entity-alist
		(mapcar
		 (function
		  (lambda (entity)
		    (cons (car entity)
			  (char-to-string
			   (make-char
			    (w3m-static-if (boundp 'MULE) lc-ltn1 'latin-iso8859-1)
			    (cdr entity))))))
		 latin1-entity))))))
(defconst w3m-entity-regexp
  (eval-when-compile
    (format "&\\(%s\\|#[0-9]+\\);?"
	    (if (fboundp 'regexp-opt)
		(let ((fn (function regexp-opt)))
		  ;; Don't funcall directly for avoiding compile warning.
		  (funcall fn (mapcar (function car)
				      w3m-entity-alist)))
	      (mapconcat (lambda (s)
			   (regexp-quote (car s)))
			 w3m-entity-alist
			 "\\|")))))
(defvar w3m-entity-db nil)		; nil means un-initialized
(defconst w3m-entity-db-size 13)	; size of obarray

(defconst w3m-encoding-alist
  (eval-when-compile
    (apply 'nconc
	   (mapcar (lambda (elem)
	       (mapcar (lambda (x) (cons x (car elem)))
		       (cdr elem)))
		   '((gzip . ("gzip" "x-gzip" "compress" "x-compress"))
		     (bzip . ("x-bzip" "bzip" "bzip2"))
		     (deflate . ("x-deflate" "deflate")))))))

(defvar w3m-current-url nil "URL of this buffer.")
(defvar w3m-current-title nil "Title of this buffer.")
(make-variable-buffer-local 'w3m-current-url)
(make-variable-buffer-local 'w3m-current-title)

(defvar w3m-verbose t "Flag variable to control messages.")

(defvar w3m-cache-buffer nil)
(defvar w3m-cache-articles nil)
(defvar w3m-cache-hashtb nil)
(defvar w3m-input-url-history nil)

(defconst w3m-arrived-db-size 1023)
(defvar w3m-arrived-db nil)		; nil means un-initialized.
(defvar w3m-arrived-user-list nil)

(defvar w3m-process-user nil)
(defvar w3m-process-passwd nil)
(defvar w3m-process-user-counter 0)
(defvar w3m-process-temp-file nil)
(make-variable-buffer-local 'w3m-process-temp-file)

(defvar w3m-display-inline-image-status nil) ; 'on means image is displayed
(make-variable-buffer-local 'w3m-display-inline-image-status)

(defconst w3m-image-type-alist
  '(("image/jpeg" . jpeg)
    ("image/gif" . gif)
    ("image/png" . png)
    ("image/x-xbm" . xbm)
    ("image/x-xpm" . xpm))
  "An alist of CONTENT-TYPE and IMAGE-TYPE.")

(defconst w3m-toolbar-buttons
  '("back" "parent" "forward" "reload" "open" "home" "search" "image"
    "weather" "antenna" "history" "db-history")
  "Toolbar button list for w3m.")

(defconst w3m-toolbar
  '([w3m-toolbar-back-icon w3m-view-previous-page
			   (w3m-history-previous-link-available-p)
			   "前のページに戻る"]
    [w3m-toolbar-parent-icon w3m-view-parent-page
			     (w3m-parent-page-available-p)
			     "上のディレクトリへ移動する"]
    [w3m-toolbar-forward-icon w3m-view-next-page
			      (w3m-history-next-link-available-p)
			      "次のページに進む"]
    [w3m-toolbar-reload-icon w3m-reload-this-page t "サーバからページをもう一度読み込む"]
    [w3m-toolbar-open-icon w3m-goto-url t "URL を入力してページを開く"]
    [w3m-toolbar-home-icon
     (lambda () (interactive) (w3m-goto-url w3m-home-page))
     w3m-home-page "ホームページへジャンプ"]
    [w3m-toolbar-search-icon w3m-search t "インターネット上を検索"]
    [w3m-toolbar-image-icon w3m-toggle-inline-images t "画像の表示をトグルする"]
    [w3m-toolbar-weather-icon w3m-weather t "天気予報を見る"]
    [w3m-toolbar-antenna-icon w3m-antenna t "アンテナで受信する"]
    [w3m-toolbar-history-icon w3m-history t "ヒストリー"]
    [w3m-toolbar-db-history-icon w3m-db-history t "ＤＢヒストリー"]
    )
  "Toolbar definition for w3m.")

(defvar w3m-cid-retrieve-function-alist nil)
(defvar w3m-force-redisplay t)

(eval-and-compile
  (condition-case nil
      :symbol-for-testing-whether-colon-keyword-is-available-or-not
    (void-variable
     (eval '(defconst :case-ignore ':case-ignore))
     (eval '(defconst :integer ':integer)))))

(defvar w3m-work-buffer-list nil)
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
  (defconst w3m-html-string-regexp
    "\\(\"\\([^\"]+\\)\"\\|'\\([^\']+\\)'\\|[^\"\'<> \t\r\f\n]*\\)"
    "Regexp used in parsing to detect string."))

(defconst w3m-halfdump-command-arguments
  '("-T" "text/html" "-t" tab-width "-halfdump"
    "-cols" (if (< 0 w3m-fill-column)
		w3m-fill-column		; fixed columns
	      (+ (frame-width) (or w3m-fill-column -1)))) ; fit for frame
  "Arguments for 'halfdump' execution of w3m.")


;; Generic macros and inline functions:
(put 'w3m-with-work-buffer 'lisp-indent-function 0)
(put 'w3m-with-work-buffer 'edebug-form-spec '(body))
(defmacro w3m-with-work-buffer (&rest body)
  "Execute the forms in BODY with working buffer as the current buffer."
  (` (with-current-buffer
	 (w3m-get-buffer-create w3m-work-buffer-name)
       (,@ body))))

(defsubst w3m-attributes (url &optional no-cache)
  "Return a list of attributes of URL.
Value is nil if retirieval of header is failed.  Otherwise, list
elements are:
 0. Type of contents.
 1. Charset of contents.
 2. Size in bytes.
 3. Encoding of contents.
 4. Last modification time.
 5. Real URL.
If optional argument NO-CACHE is non-nil, cache is not used."
  (cond
   ((string-match "^about:" url)
    (list "text/html" nil nil nil nil url))
   ((string-match "^\\(file:\\|/\\)" url)
    (w3m-local-attributes url))
   (t
    (w3m-w3m-attributes url no-cache))))

(defmacro w3m-content-type (url &optional no-cache)
  (` (car (w3m-attributes (, url) (, no-cache)))))
(defmacro w3m-content-charset (url &optional no-cache)
  (` (nth 1 (w3m-attributes (, url) (, no-cache)))))
(defmacro w3m-content-length (url &optional no-cache)
  (` (nth 2 (w3m-attributes (, url) (, no-cache)))))
(defmacro w3m-content-encoding (url &optional no-cache)
  (` (nth 3 (w3m-attributes (, url) (, no-cache)))))
(defmacro w3m-last-modified (url &optional no-cache)
  (` (nth 4 (w3m-attributes (, url) (, no-cache)))))
(defmacro w3m-real-url (url &optional no-cache)
  (` (nth 5 (w3m-attributes (, url) (, no-cache)))))

(defsubst w3m-anchor (&optional point)
  (get-text-property (or point (point)) 'w3m-href-anchor))
(defsubst w3m-image (&optional point)
  (get-text-property (or point (point)) 'w3m-image))
(defsubst w3m-action (&optional point)
  (get-text-property (or point (point)) 'w3m-action))

(defsubst w3m-get-buffer-create (name)
  "Return the buffer named NAME, or create such a buffer and return it."
  (or (get-buffer name)
      (let ((buf (get-buffer-create name)))
	(setq w3m-work-buffer-list (cons buf w3m-work-buffer-list))
	(buffer-disable-undo buf)
	buf)))

(defmacro w3m-make-help-echo (property)
  "Make a function for showing a `help-echo' string."
  (if (featurep 'xemacs)
      (` (if (and (boundp 'emacs-major-version)
		  (>= emacs-major-version 21))
	     (function (lambda (extent)
			 (if w3m-track-mouse
			     (get-text-property (extent-start-position extent)
						(quote (, property))))))))
    (` (if (and (boundp 'emacs-major-version)
		(>= emacs-major-version 21))
	   (function (lambda (window object pos)
		       (if w3m-track-mouse
			   (get-text-property pos (quote (, property))))))))))

(defmacro w3m-make-balloon-help (property)
  "Make a function for showing a `balloon-help' under XEmacs."
  (when (featurep 'xemacs)
    (` (let ((fn (intern (format "w3m-balloon-help-for-%s"
				 (quote (, property))))))
	 (prog1
	     fn
	   (unless (fboundp fn)
	     (defalias fn
	       (lambda (extent)
		 (if w3m-track-mouse
		     (get-text-property (extent-start-position extent)
					(quote (, property)))))))
	   (when (and (featurep 'bytecomp)
		      (not (compiled-function-p (symbol-function fn))))
	     (byte-compile fn)))))))

(defun w3m-message (&rest args)
  "Alternative function of `message' for w3m.el."
  (if w3m-verbose
      (apply (function message) args)
    (apply (function format) args)))

(defun w3m-time-parse-string (string)
  "Parse the time-string STRING and return its time as Emacs style."
  (ignore-errors
    (let ((x (timezone-fix-time string nil nil)))
      (encode-time (aref x 5) (aref x 4) (aref x 3)
		   (aref x 2) (aref x 1) (aref x 0)
		   (aref x 6)))))

;; When buggy timezone.el is loaded, parse-time.el will be used
;; instead of timezone.el.
(unless (let* ((x (current-time))
	       (y (w3m-time-parse-string
		   (format-time-string "%A, %d-%b-%y %T %Z" x))))
	  (and (eq (car x) (car y)) (eq (nth 1 x) (nth 1 y))))
  (ignore-errors
    (require 'parse-time))
  (defun w3m-time-parse-string (string)
    "Parse the time-string STRING and return its time as Emacs style."
    (let ((fn (when (fboundp 'parse-time-string)
		'parse-time-string)))
      (when fn
	(apply (function encode-time) (funcall fn string))))))

(defsubst w3m-time-newer-p (a b)
  "Return t, if A is newer than B.  Otherwise return nil.
A and B are lists which represent time in Emacs-style.  If value is
nil, it is regarded as the oldest time."
  (and a
       (or (not b)
	   (or (> (car a) (car b))
	       (and (= (car a) (car b))
		    (> (nth 1 a) (nth 1 b)))))))

(defun w3m-sub-list (list n)
  "Make new list from LIST with top most N items.
If N is negative, last N items of LIST is returned."
  (if (integerp n)
      (if (< n 0)
	  ;; N is negative, get items from tail of list
	  (if (>= (- n) (length list))
	      (copy-sequence list)
	    (nthcdr (+ (length list) n) (copy-sequence list)))
	;; N is non-negative, get items from top of list
	(if (>= n (length list))
	    (copy-sequence list)
	  (nreverse (nthcdr (- (length list) n) (reverse list)))))
    (copy-sequence list)))

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
	    (coding-system-for-write coding)
	    (standard-output (current-buffer))
	    element print-length print-level)
	(insert "(")
	(while list
	  (setq element (car list)
		list (cdr list))
	  (if (consp element)
	      (progn
		(insert "(")
		(prin1 (car element))
		(insert "\n")
		(while (setq element (cdr element))
		  (insert "  ")
		  (prin1 (car element))
		  (insert "\n"))
		(backward-delete-char 1)
		(insert ")\n "))
	    (prin1 element)
	    (insert "\n")))
	(skip-chars-backward "\n ")
	(delete-region (point) (point-max))
	(insert ")\n")
	(let ((mode (and (file-exists-p file)
			 (file-modes file))))
	  (write-region (point-min) (point-max) file nil 'nomsg)
	  (when mode (set-file-modes file mode)))))))

(eval-when-compile
  (defmacro w3m-arrived-add-1 (ident title modified-time
				     arrived-time coding-system)
    (` (progn
	 (put (, ident) 'title (, title))
	 (put (, ident) 'last-modified (, modified-time))
	 (put (, ident) 'coding-system (, coding-system))
	 (set (, ident) (, arrived-time))))))

(defun w3m-arrived-add (url &optional title modified-time
			    arrived-time coding-system)
  "Add URL to hash database of arrived URLs."
  (when (> (length url) 5);; ignore trifles.
    (let ((parent (when (string-match "#\\([^#]+\\)$" url)
		    (substring url 0 (match-beginning 0))))
	  ident)
      (unless (and modified-time arrived-time)
	(let ((ct (current-time)))
	  (unless modified-time
	    (setq modified-time (or (w3m-last-modified (or parent url))
				    ct)))
	  (unless arrived-time
	    (setq arrived-time ct))))
      (prog1
	  (setq ident (intern url w3m-arrived-db))
	(w3m-arrived-add-1 ident title modified-time
			   arrived-time coding-system)
	(when parent
	  (setq ident (intern parent w3m-arrived-db))
	  (w3m-arrived-add-1 ident title modified-time
			     arrived-time coding-system))))))

(defsubst w3m-arrived-p (url)
  "If URL has been arrived, return non-nil value.  Otherwise return nil."
  (intern-soft url w3m-arrived-db))

(defun w3m-arrived-time (url)
  "If URL has been arrived, return its arrived time.  Otherwise return nil."
  (let ((v (intern-soft url w3m-arrived-db)))
    (and v (boundp v) (symbol-value v))))

(defun w3m-arrived-title (url)
  "Return the stored title of the page, which is pointed by URL."
  (let ((v (intern-soft url w3m-arrived-db)))
    (and v (get v 'title))))

(defun w3m-arrived-last-modified (url)
  "If URL has been arrived, return its last modified time.  Otherwise return nil."
  (let ((v (intern-soft url w3m-arrived-db)))
    (and v (get v 'last-modified))))

(defun w3m-arrived-coding-system (url)
  "If URL has been specified coding-system, return its coding-system.
  Otherwise return nil."
  (let ((v (intern-soft url w3m-arrived-db)))
    (and v (get v 'coding-system))))

(defun w3m-arrived-setup ()
  "Load arrived url list from `w3m-arrived-file' and setup hash database."
  (unless w3m-arrived-db
    (setq w3m-arrived-db (make-vector w3m-arrived-db-size nil))
    (let ((list (w3m-load-list w3m-arrived-file
			       w3m-arrived-file-coding-system)))
      (dolist (elem list)
	(if (or (not (nth 1 elem)) (stringp (nth 1 elem)))
	    ;; Process new format of arrived URL database.
	    (w3m-arrived-add (if (string-match "^/" (car elem))
				 (w3m-expand-file-name-as-url (car elem))
			       (car elem))
			     (nth 1 elem)
			     (nth 2 elem)
			     (or (nth 3 elem) (nth 2 elem))
			     (nth 4 elem))
	  ;; Process old format of arrived URL database, is used
	  ;; before revision 1.135.
	  (w3m-arrived-add (car elem) nil (cdr elem) (cdr elem) nil)))
      (unless w3m-input-url-history
	(setq w3m-input-url-history (mapcar (function car) list))))))

(defun w3m-arrived-shutdown ()
  "Save hash database of arrived URLs to `w3m-arrived-file'."
  (when w3m-arrived-db
    ;; Re-read arrived DB file, and check sites which are arrived on
    ;; the other emacs process.
    (dolist (elem (w3m-load-list w3m-arrived-file
				 w3m-arrived-file-coding-system))
      (and
       ;; Check format of arrived DB file.
       (or (not (nth 1 elem)) (stringp (nth 1 elem)))
       (w3m-time-newer-p (nth 3 elem) (w3m-arrived-time (car elem)))
       (w3m-arrived-add (if (string-match "^/" (car elem))
			    (w3m-expand-file-name-as-url (car elem))
			  (car elem))
			(nth 1 elem)
			(nth 2 elem)
			(nth 3 elem)
			(nth 4 elem))))
    ;; Convert current arrived DB to a list.
    (let (list)
      (mapatoms
       (lambda (sym)
	 (and sym
	      (boundp sym)
	      (push (list (symbol-name sym)
			  (get sym 'title)
			  (get sym 'last-modified)
			  (symbol-value sym)
			  (get sym 'coding-system))
		    list)))
       w3m-arrived-db)
      (w3m-save-list w3m-arrived-file
		     w3m-arrived-file-coding-system
		     (w3m-sub-list
		      (sort list
			    (lambda (a b)
			      (w3m-time-newer-p (nth 3 a) (nth 3 b))))
		      w3m-keep-arrived-urls)))
    (setq w3m-arrived-db nil)))

(add-hook 'kill-emacs-hook 'w3m-arrived-shutdown)


;;; Working buffers:
(defun w3m-kill-all-buffer ()
  "Kill all working buffer."
  (dolist (buf w3m-work-buffer-list)
    (when (buffer-live-p buf)
      (kill-buffer buf)))
  (setq w3m-work-buffer-list nil))

(defun w3m-url-encode-string (str &optional coding)
  (apply (function concat)
	 (mapcar
	  (lambda (ch)
	    (cond
	     ((string-match "[-a-zA-Z0-9_:/]" (char-to-string ch)) ; xxx?
	      (char-to-string ch))	; printable
	     (t
	      (format "%%%02X" ch))))	; escape
	  ;; Coerce a string to a list of chars.
	  (append (encode-coding-string str (or coding 'iso-2022-jp))
		  nil))))

(put 'w3m-parse-attributes 'lisp-indent-function '1)
(put 'w3m-parse-attributes 'edebug-form-spec '(form))
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
				(w3m-remove-redundant-spaces
				 (or (match-string-no-properties 2)
				     (match-string-no-properties 3)
				     (match-string-no-properties 1))))))
		     (when (listp attr)
		       (cond
			((eq (nth 1 attr) :case-ignore)
			 (setq sexp (list 'downcase sexp)))
			((eq (nth 1 attr) :integer)
			 (setq sexp (list 'string-to-number sexp)))
			((nth 1 attr)
			 (error "Internal error, unknown modifier.")))
		       (setq attr (car attr)))
		     (` ((looking-at
			  (, (format "%s[ \t\r\f\n]*=[ \t\r\f\n]*%s"
				     (symbol-name attr)
				     w3m-html-string-regexp)))
			 (setq (, attr) (, sexp))))))
		 attributes))
	    ((looking-at
	      (, (concat "[A-z]*[ \t\r\f\n]*=[ \t\r\f\n]*" w3m-html-string-regexp))))
	    ((looking-at "[^<> \t\r\f\n]+")))
	 (goto-char (match-end 0))
	 (skip-chars-forward " \t\r\f\n"))
       (skip-chars-forward "^>")
       (forward-char)
       (,@ form))))


;;; HTML character entity handling:
(defun w3m-entity-db-setup ()
  ;; initialise entity database (obarray)
  (setq w3m-entity-db (make-vector w3m-entity-db-size 0))
  (dolist (elem w3m-entity-alist)
    (set (intern (car elem) w3m-entity-db)
	 (cdr elem))))

(defsubst w3m-entity-value (name)
  ;; initialise if need
  (if (null w3m-entity-db)
      (w3m-entity-db-setup))
  ;; return value of specified entity, or empty string for unknown entity.
  (or (symbol-value (intern-soft name w3m-entity-db))
      (if (not (char-equal (string-to-char name) ?#))
	  (concat "&" name)		; unknown entity
	;; case of immediate character (accept only 0x20 .. 0x7e)
	(let ((char (string-to-int (substring name 1))))
	  ;; make character's representation with learning
	  (set (intern name w3m-entity-db)
	       (if (or (< char 32) (< 127 char))
		   "~"			; un-supported character
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

(defsubst w3m-decode-anchor-string (str)
  ;; FIXME: This is a quite ad-hoc function to process encoded URL
  ;;        string.  More discussion about timing &-sequence decode is
  ;;        required.  See [emacs-w3m:00150] for detail.
  (let ((start 0) (buf))
    (while (string-match "&amp;" str start)
      (setq buf (cons "&" (cons (substring str start (match-beginning 0)) buf))
	    start (match-end 0)))
    (apply (function concat)
	   (nreverse (cons (substring str start) buf)))))

(defun w3m-fontify-anchors ()
  "Fontify anchor tags in this buffer which contains half-dumped data."
  (goto-char (point-min))
  (let ((help (w3m-make-help-echo w3m-href-anchor))
	(balloon (w3m-make-balloon-help w3m-href-anchor))
	start end)
    (while (re-search-forward "<a[ \t\r\f\n]+" nil t)
      (setq start (match-beginning 0))
      (w3m-parse-attributes (href name)
	(delete-region start (point))
	(cond
	 (href
	  (when (search-forward "</a>" nil t)
	    (delete-region (setq end (match-beginning 0)) (match-end 0))
	    (setq href (w3m-expand-url (w3m-decode-anchor-string href)
				       w3m-current-url))
	    (add-text-properties start end
				 (list 'face (if (w3m-arrived-p href)
						 'w3m-arrived-anchor-face
					       'w3m-anchor-face)
				       'w3m-href-anchor href
				       'mouse-face 'highlight
				       'w3m-name-anchor name
				       'help-echo help
				       'balloon-help balloon))))
	 (name
	  (when (re-search-forward "<\\|\n" nil t)
	    (setq end (match-beginning 0))
	    (when (= start end)
	      (setq end (min (1+ end) (point-max))))
	    (put-text-property start end 'w3m-name-anchor name))))))))

(defun w3m-image-type (content-type)
  "Return image type which corresponds to CONTENT-TYPE."
  (cdr (assoc content-type w3m-image-type-alist)))

(eval-and-compile
  (unless (fboundp 'w3m-create-image)
    (defun w3m-create-image (url &optional no-cache)))
  (unless (fboundp 'w3m-insert-image)
    (defun w3m-insert-image (beg end image)))
  (unless (fboundp 'w3m-image-type-available-p)
    (defun w3m-image-type-available-p (image-type)
      "Return non-nil if an image with IMAGE-TYPE can be displayed inline."
      nil))
  (unless (fboundp 'w3m-setup-toolbar)
    (defun w3m-setup-toolbar ()))
  (unless (fboundp 'w3m-update-toolbar)
    (defun w3m-update-toolbar ())))

(defun w3m-fontify-images ()
  "Fontify image alternate strings in this buffer which contains
half-dumped data."
  (goto-char (point-min))
  (let ((help (w3m-make-help-echo w3m-image))
	(balloon (w3m-make-balloon-help w3m-image))
	src upper start end)
    (while (re-search-forward "<\\(img_alt\\) src=\"\\([^\"]*\\)\">" nil t)
      (setq src (match-string-no-properties 2)
	    upper (string= (match-string 1) "IMG_ALT")
	    start (match-beginning 0))
      (delete-region start (match-end 0))
      (setq src (w3m-expand-url src w3m-current-url))
      (when (search-forward "</img_alt>" nil t)
	(delete-region (setq end (match-beginning 0)) (match-end 0))
	(add-text-properties start end (list 'face 'w3m-image-face
					     'w3m-image src
					     'mouse-face 'highlight
					     'w3m-image-redundant upper
					     'help-echo help
					     'balloon-help balloon))))))

(defun w3m-toggle-inline-images (&optional force no-cache)
  "Toggle displaying of inline images on current buffer.
If optional argument FORCE is non-nil, displaying is forced.
If second optional argument NO-CACHE is non-nil, cache is not used."
  (interactive "P")
  (unless (and force (eq w3m-display-inline-image-status 'on))
    (let ((cur-point (point))
	  (buffer-read-only)
	  point end url image)
      (if (or force (eq w3m-display-inline-image-status 'off))
	  (save-excursion
	    (goto-char (point-min))
	    (while (if (get-text-property (point) 'w3m-image)
		       (setq point (point))
		     (setq point (next-single-property-change (point)
							      'w3m-image)))
	      (setq end (or (next-single-property-change point 'w3m-image)
			    (point-max)))
	      (goto-char end)
	      (if (setq url (w3m-image point))
		  (setq url (w3m-expand-url url w3m-current-url)))
	      (if (get-text-property point 'w3m-image-redundant)
		  (progn
		    ;; Insert dummy string instead of redundant image.
		    (setq image
			  (make-string
			   (string-width (buffer-substring point end))
			   ? ))
		    (put-text-property point end 'invisible t)
		    (setq point (point))
		    (insert image)
		    (put-text-property point (point) 'w3m-image-dummy t)
		    (put-text-property point (point) 'w3m-image "dummy"))
		(save-excursion
		  (goto-char cur-point)
		  (when (and url
			     (setq image (w3m-create-image url no-cache)))
		    (w3m-insert-image point end image)
		    ;; Redisplay
		    (and w3m-force-redisplay (sit-for 0))))))
	    (setq w3m-display-inline-image-status 'on))
	(save-excursion
	  (goto-char (point-min))
	  (while (if (get-text-property (point) 'w3m-image)
		     (setq point (point))
		   (setq point (next-single-property-change (point)
							    'w3m-image)))
	    (setq end (or (next-single-property-change point 'w3m-image)
			  (point-max)))
	    (goto-char end)
	    ;; IMAGE-ALT-STRING DUMMY-STRING
	    ;; <--------w3m-image---------->
	    ;; <---redundant--><---dummy--->
	    ;; <---invisible-->
	    (cond
	     ((get-text-property point 'w3m-image-redundant)
	      ;; Remove invisible property.
	      (remove-text-properties point end '(invisible)))
	     ((get-text-property point 'w3m-image-dummy)
	      ;; Remove dummy string.
	      (delete-region point end))
	     (t (w3m-remove-image point end))))
	  (setq w3m-display-inline-image-status 'off))))))

(defun w3m-decode-entities (&optional reserve-prop)
  "Decode entities in the current buffer.
If optional RESERVE-PROP is non-nil, text property is reserved."
  (save-excursion
    (goto-char (point-min))
    (let (prop)
      (while (re-search-forward w3m-entity-regexp nil t)
	(if reserve-prop
	    (setq prop (text-properties-at (match-beginning 0))))
	(replace-match (w3m-entity-value (match-string 1)) nil t)
	(if (and reserve-prop prop)
	    (add-text-properties (match-beginning 0) (point) prop))))))

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
    (while (re-search-forward "</?[A-z_][^>]*>" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    ;; Decode escaped characters (entities).
    (w3m-decode-entities 'reserve-prop)
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
    (w3m-arrived-setup)
    (mapatoms (lambda (x)
		(when x (push (cons (symbol-name x) x) candidates)))
	      w3m-arrived-db)
    (setq default (or default (thing-at-point 'url)))
    (setq url (let ((minibuffer-setup-hook (append minibuffer-setup-hook
						   '(beginning-of-line))))
		(completing-read (or prompt
				     (if default
					 "URL: "
				       (format "URL (default %s): " w3m-home-page)))
				 candidates nil nil default
				 'w3m-input-url-history)))
    (if (string= "" url) (setq url w3m-home-page))
    ;; remove duplication
    (setq w3m-input-url-history (cons url (delete url w3m-input-url-history)))
    ;; return value
    url))


;;; Cache:
(defun w3m-cache-setup ()
  "Initialise cache variables."
  (unless (and (bufferp w3m-cache-buffer)
	       (buffer-live-p w3m-cache-buffer))
    (save-excursion
      (set-buffer (w3m-get-buffer-create " *w3m cache*"))
      (buffer-disable-undo)
      (set-buffer-multibyte nil)
      (setq buffer-read-only t
	    w3m-cache-buffer (current-buffer)
	    w3m-cache-hashtb (make-vector 1021 0)))))

(defun w3m-cache-shutdown ()
  "Clear all cache variables and buffers."
  (when (buffer-live-p w3m-cache-buffer)
    (kill-buffer w3m-cache-buffer))
  (setq w3m-cache-hashtb nil
	w3m-cache-articles nil))

(defun w3m-cache-header (url header)
  "Store up URL's HEADER in cache."
  (w3m-cache-setup)
  (set (intern url w3m-cache-hashtb) header))

(defun w3m-cache-request-header (url)
  "Return the URL's header string, when it is stored in cache."
  (w3m-cache-setup)
  (let ((ident (intern url w3m-cache-hashtb)))
    (and (boundp ident)
	 (symbol-value ident))))

(defun w3m-cache-remove-oldest ()
  (save-excursion
    (set-buffer w3m-cache-buffer)
    (goto-char (point-min))
    (unless (zerop (buffer-size))
      (let ((ident (get-text-property (point) 'w3m-cache))
	    buffer-read-only)
	;; Remove the ident from the list of articles.
	(when ident
	  (setq w3m-cache-articles (delq ident w3m-cache-articles)))
	;; Delete the article itself.
	(delete-region (point)
		       (next-single-property-change
			(1+ (point)) 'w3m-cache nil (point-max)))))))

(defun w3m-cache-remove (url)
  "Remove URL's data from the cache."
  (w3m-cache-setup)
  (let ((ident (intern url w3m-cache-hashtb))
	beg end)
    (when (memq ident w3m-cache-articles)
      ;; It was in the cache.
      (save-excursion
	(set-buffer w3m-cache-buffer)
	(let (buffer-read-only)
	  (when (setq beg (text-property-any
			   (point-min) (point-max) 'w3m-cache ident))
	    ;; Find the end (i. e., the beginning of the next article).
	    (setq end (next-single-property-change
		       (1+ beg) 'w3m-cache (current-buffer) (point-max)))
	    (delete-region beg end)))
	(setq w3m-cache-articles (delq ident w3m-cache-articles))))))

(defun w3m-cache-contents (url buffer)
  "Store URL's contents which is placed in the BUFFER.
Return symbol to identify its cache data."
  (w3m-cache-setup)
  (let ((ident (intern url w3m-cache-hashtb)))
    (w3m-cache-remove url)
    ;; Remove the oldest article, if necessary.
    (and (numberp w3m-keep-cache-size)
	 (>= (length w3m-cache-articles) w3m-keep-cache-size)
	 (w3m-cache-remove-oldest))
    ;; Insert the new article.
    (save-excursion
      (set-buffer w3m-cache-buffer)
      (let (buffer-read-only)
	(goto-char (point-max))
	(unless (bolp) (insert "\n"))
	(let ((b (point)))
	  (insert-buffer-substring buffer)
	  ;; Tag the beginning of the article with the ident.
	  (when (> (point-max) b)
	    (put-text-property b (1+ b) 'w3m-cache ident)
	    (setq w3m-cache-articles (cons ident w3m-cache-articles))
	    ident))))))

(defun w3m-cache-request-contents (url &optional buffer)
  "Insert URL's data to the BUFFER.
If URL's data is found in the cache, return t.  Otherwise return nil.
When BUFFER is nil, all data will be inserted in the current buffer."
  (w3m-cache-setup)
  (let ((ident (intern url w3m-cache-hashtb)))
    (when (memq ident w3m-cache-articles)
      ;; It was in the cache.
      (let (beg end)
	(save-excursion
	  (set-buffer w3m-cache-buffer)
	  (if (setq beg (text-property-any
			 (point-min) (point-max) 'w3m-cache ident))
	      ;; Find the end (i. e., the beginning of the next article).
	      (setq end (next-single-property-change
			 (1+ beg) 'w3m-cache (current-buffer) (point-max)))
	    ;; It wasn't in the cache after all.
	    (setq w3m-cache-articles (delq ident w3m-cache-articles))))
	(and beg
	     end
	     (save-excursion
	       (when buffer
		 (set-buffer buffer))
	       (let (buffer-read-only)
		 (insert-buffer-substring w3m-cache-buffer beg end))
	       t))))))


;;; Handle process:
(defun w3m-exec-process (&rest args)
  "Run w3m-command and return process exit status."
  (save-excursion
    (let ((coding-system-for-read 'binary)
	  (coding-system-for-write w3m-coding-system)
	  (default-process-coding-system (cons 'binary w3m-coding-system))
	  (process-connection-type w3m-process-connection-type))
      (setq args (append w3m-command-arguments args))
      (if w3m-async-exec
	  ;; start-process
	  (let ((w3m-process-user)
		(w3m-process-passwd)
		(w3m-process-user-counter 2)
		(proc (apply 'start-process w3m-command (current-buffer) w3m-command args)))
	    (set-process-filter proc 'w3m-exec-filter)
	    (set-process-sentinel proc 'ignore)
	    (process-kill-without-query proc)
            (unwind-protect
                (prog2
		    (while (eq (process-status proc) 'run)
		      (accept-process-output nil 0 200))
		    (process-exit-status proc)
		  (and w3m-current-url
		       w3m-process-user
		       (setq w3m-arrived-user-list
			     (cons
			      (cons w3m-current-url
				    (list w3m-process-user w3m-process-passwd))
			      (delete (assoc w3m-current-url w3m-arrived-user-list)
				      w3m-arrived-user-list)))))
              (delete-process proc)));; Clean up resources of process.
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

(defun w3m-exec-filter (process string)
  (if (buffer-name (process-buffer process))
      (with-current-buffer (process-buffer process)
	(let ((buffer-read-only nil)
	      (case-fold-search nil))
	  (goto-char (process-mark process))
	  (insert string)
	  (set-marker (process-mark process) (point))
	  (unless (string= "" string)
	    (goto-char (point-min))
	    (cond
	     ((and (looking-at
		    "\\(\nWrong username or password\n\\)?Username: Password: ")
		   (= (match-end 0) (point-max)))
	      (setq w3m-process-passwd
		    (or (nth 1 (w3m-exec-get-user w3m-current-url))
			(read-passwd "Password: ")))
	      (condition-case nil
		  (progn
		    (process-send-string process
					 (concat w3m-process-passwd "\n"))
		    (delete-region (point-min) (point-max)))
		(error nil)))
	     ((and (looking-at
		    "\\(\nWrong username or password\n\\)?Username: ")
		   (= (match-end 0) (point-max)))
	      (setq w3m-process-user
		    (or (nth 0 (w3m-exec-get-user w3m-current-url))
			(read-from-minibuffer "Username: ")))
	      (condition-case nil
		  (process-send-string process
				       (concat w3m-process-user "\n"))
		(error nil)))))))))


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


;;; Handle encoding of contents:
(defsubst w3m-which-command (command)
  (catch 'found-command
    (let (bin)
      (dolist (dir exec-path)
	(when (or (file-executable-p
		   (setq bin (expand-file-name command dir)))
		  (file-executable-p
		   (setq bin (expand-file-name (concat command ".exe") dir))))
	  (throw 'found-command bin))))))

(defun w3m-decode-encoded-buffer (encoding)
  (let ((x (and (stringp encoding)
		(assoc encoding w3m-encoding-alist))))
    (or (not (and x (setq x (cdr (assq (cdr x) w3m-decoder-alist)))))
	(let ((coding-system-for-write 'binary)
	      (coding-system-for-read 'binary)
	      (default-process-coding-system (cons 'binary 'binary)))
	  (zerop (apply 'call-process-region
			(point-min) (point-max)
			(w3m-which-command (car x))
			t '(t nil) nil (nth 1 x)))))))

(defun w3m-decode-buffer (url &optional cs)
  (let ((type (w3m-content-type url))
	(charset (w3m-content-charset url))
	(encoding (w3m-content-encoding url)))
    (unless (w3m-decode-encoded-buffer encoding)
      (error "Can't decode encoded contents: %s" url))
    (if (and (not charset) (string= type "text/html"))
	(setq charset
	      (let ((case-fold-search t))
		(goto-char (point-min))
		(and (or (re-search-forward
			  w3m-meta-content-type-charset-regexp nil t)
			 (re-search-forward
			  w3m-meta-charset-content-type-regexp nil t))
		     (match-string-no-properties 2)))))
    (decode-coding-region
     (point-min) (point-max)
     (cond
      (cs cs)
      (charset
       (w3m-charset-to-coding-system charset))
      (t
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
	     (car candidate)))))))
    (set-buffer-multibyte t)))


;;; Retrieve local data:
(defun w3m-local-content-type (url)
  (catch 'type-detected
    (dolist (elem w3m-content-type-alist "unknown")
      (if (string-match (nth 1 elem) url)
	  (throw 'type-detected (car elem))))))

(defun w3m-local-attributes (url &rest args)
  "Return a list of attributes of URL.
Value is nil if retirieval of header is failed.  Otherwise, list
elements are:
 0. Type of contents.
 1. Charset of contents.
 2. Size in bytes.
 3. Encoding of contents.
 4. Last modification time.
 5. Real URL.
"
  (let* ((file (w3m-url-to-file-name url))
	 (attr (when (file-exists-p file)
		 (file-attributes file))))
    (list (w3m-local-content-type url)
	  nil
	  (nth 7 attr)
	  nil
	  (nth 5 attr)
	  (w3m-expand-file-name-as-url (file-truename file)))))

(defun w3m-local-retrieve (url &optional no-decode &rest args)
  "Retrieve content of local URL and insert it to the working buffer.
This function will return content-type of URL as string when retrieval
succeed.  If NO-DECODE, set the multibyte flag of the working buffer
to nil."
  (let ((type (w3m-local-content-type url))
	(file))
    (setq file (w3m-url-to-file-name url))
    (w3m-with-work-buffer
      (delete-region (point-min) (point-max))
      (if (and (string-match "^text/" type)
	       (not no-decode))
	  (progn
	    (set-buffer-multibyte t)
	    (insert-file-contents file))
	(set-buffer-multibyte nil)
	(let ((coding-system-for-read 'binary)
	      (file-coding-system-for-read 'binary)
	      jka-compr-compression-info-list
	      jam-zcat-filename-list
	      format-alist)
	  (insert-file-contents file))))
    type))


;;; Retrieve data via HTTP:
(defun w3m-remove-redundant-spaces (str)
  "Remove spaces/tabs at the front of a string and at the end of a string"
  (save-match-data
    (substring str
	       (if (string-match "^[ \t\r\f\n]+" str) (match-end 0) 0)
	       (and (string-match "[ \t\r\f\n]+$" str) (match-beginning 0)))))

(defun w3m-w3m-get-header (url &optional no-cache)
  "Return the header string of the URL.
If optional argument NO-CACHE is non-nil, cache is not used."
  (or (unless no-cache
	(w3m-cache-request-header url))
      (with-temp-buffer
	(let ((w3m-current-url url))
	  (w3m-message "Request sent, waiting for response...")
	  (when (zerop (prog1 (w3m-exec-process "-dump_head" url)
			 (w3m-message "Request sent, waiting for response... done")))
	    (w3m-cache-header url (buffer-string)))))))

(defun w3m-w3m-attributes (url &optional no-cache)
  "Return a list of attributes of URL.
Value is nil if retirieval of header is failed.  Otherwise, list
elements are:
 0. Type of contents.
 1. Charset of contents.
 2. Size in bytes.
 3. Encoding of contents.
 4. Last modification time.
 5. Real URL.
If optional argument NO-CACHE is non-nil, cache is not used."
  (let ((header (w3m-w3m-get-header url no-cache)))
    (cond
     ((and header (string-match "HTTP/1\\.[0-9] 200 " header))
      (let (alist type charset)
	(dolist (line (split-string header "\n"))
	  (when (string-match "^\\([^:]+\\):[ \t]*" line)
	    (push (cons (downcase (match-string 1 line))
			(substring line (match-end 0)))
		  alist)))
	(when (setq type (cdr (assoc "content-type" alist)))
	  (if (string-match ";[ \t]*charset=" type)
	      (setq charset (w3m-remove-redundant-spaces
			     (substring type (match-end 0)))
		    type (w3m-remove-redundant-spaces
			  (substring type 0 (match-beginning 0))))
	    (setq type (w3m-remove-redundant-spaces type))
	    (when (string-match ";$" type)
	      (setq type (substring type 0 (match-beginning 0))))))
	(list (or type (w3m-local-content-type url))
	      charset
	      (let ((v (cdr (assoc "content-length" alist))))
		(and v (string-to-number v)))
	      (cdr (assoc "content-encoding" alist))
	      (let ((v (cdr (assoc "last-modified" alist))))
		(and v (w3m-time-parse-string v)))
	      (or (cdr (assoc "w3m-current-url" alist))
		  url))))
     ;; FIXME: adhoc implementation
     ;; HTTP/1.1 500 Server Error on Netscape-Enterprise/3.6
     ;; HTTP/1.0 501 Method Not Implemented
     ((and header (string-match "HTTP/1\\.[0-9] 50[0-9]" header))
      (list "text/html" nil nil nil nil url)))))

(defun w3m-pretty-length (n)
  ;; This function imported from url.el.
  (cond
   ((< n 1024)
    (format "%d bytes" n))
   ((< n (* 1024 1024))
    (format "%dk" (/ n 1024.0)))
   (t
    (format "%2.2fM" (/ n (* 1024 1024.0))))))

(defun w3m-crlf-to-lf ()
  (when (eq w3m-linefeed-type 'crlf)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\r\n" nil t)
	(delete-region (- (point) 2) (1- (point)))))))

(defun w3m-w3m-dump-head-source (url)
  (and (let ((w3m-current-url url))
	 (w3m-message "Reading...")
	 (prog1 (zerop (w3m-exec-process "-dump_extra" url))
	   (w3m-message "Reading... done")
	   (w3m-crlf-to-lf)))
       (goto-char (point-min))
       (let ((case-fold-search t))
	 (re-search-forward "^w3m-current-url:" nil t))
       (search-forward "\n\n" nil t)
       (progn
	 (w3m-cache-header url (buffer-substring (point-min) (point)))
	 (delete-region (point-min) (point))
	 (w3m-cache-contents url (current-buffer))
	 (w3m-w3m-attributes url))))

(defun w3m-w3m-dump-source (url)
  (let ((headers (w3m-w3m-attributes url t)))
    (when headers
      (let ((type   (car headers))
	    (length (nth 2 headers)))
	(when (let ((w3m-current-url url))
		(w3m-message "Reading...")
		(prog1 (zerop (w3m-exec-process "-dump_source" url))
		  (w3m-message "Reading... done")))
	  (w3m-crlf-to-lf)
	  (cond
	   ((and length (> (buffer-size) length))
	    (delete-region (point-min) (- (point-max) length)))
	   ((string= "text/html" type)
	    ;; Remove cookies.
	    (goto-char (point-min))
	    (while (and (not (eobp))
			(looking-at "Received cookie: "))
	      (forward-line 1))
	    (skip-chars-forward " \t\r\f\n")
	    (if (or (looking-at "<!DOCTYPE")
		    (looking-at "<HTML>")) ; for eGroups.
		(delete-region (point-min) (point)))))
	  (w3m-cache-contents url (current-buffer))
	  headers)))))

(defun w3m-w3m-retrieve (url &optional no-decode no-cache cs)
  "Retrieve content of URL with w3m and insert it to the working buffer.
This function will return content-type of URL as string when retrieval
succeed.  If NO-DECODE, set the multibyte flag of the working buffer
to nil."
  (w3m-with-work-buffer
    (delete-region (point-min) (point-max))
    (set-buffer-multibyte nil)
    (or (unless no-cache
	  (when (w3m-cache-request-contents url)
	    (let ((type (w3m-content-type url)))
	      (and (string-match "^text/" type)
		   (unless no-decode
		     (w3m-decode-buffer url cs)))
	      type)))
	(let ((type (car (if w3m-mnc
			     (w3m-w3m-dump-head-source url)
			   (w3m-w3m-dump-source url)))))
	  (when type
	    (and (string-match "^text/" type)
		 (not no-decode)
		 (w3m-decode-buffer url cs))
	    type)))))

(defsubst w3m-url-local-p (url)
  "If URL points a file on the local system, return non-nil value.  Otherwise return nil."
  (string-match "^\\(file:\\|/\\)" url))

(defun w3m-retrieve (url &optional no-decode no-cache cs)
  "Retrieve content of URL and insert it to the working buffer.
This function will return content-type of URL as string when retrieval
succeed.  If NO-DECODE, set the multibyte flag of the working buffer
to nil."
  (let ((v (cond
	    ((string-match "^about:" url)
	     (let (func)
	       (if (and (string-match "^about://\\([^/]+\\)/" url)
			(setq func (intern-soft
				    (concat "w3m-about-" (match-string 1 url))))
			(fboundp func))
		   (funcall func url no-decode no-cache)
		 (w3m-about url no-decode no-cache))))
	    ((w3m-url-local-p url)
	     (w3m-local-retrieve url no-decode))
	    ((string-match "^cid:" url)
	     (let ((func (cdr (assq major-mode w3m-cid-retrieve-function-alist))))
	       (when func
		 (funcall func url no-decode no-cache))))
	    (t
	     (w3m-w3m-retrieve url no-decode no-cache cs)))))
    (and v
	 (not no-decode)
	 w3m-use-filter
	 (w3m-filter url))
    v))

(defun w3m-download (url &optional filename no-cache)
  (unless filename
    (setq filename (w3m-read-file-name nil nil url)))
  (if (w3m-retrieve url t no-cache)
      (with-current-buffer (get-buffer w3m-work-buffer-name)
	(let ((buffer-file-coding-system 'binary)
	      (file-coding-system 'binary)
	      (coding-system-for-write 'binary)
	      jka-compr-compression-info-list
	      jam-zcat-filename-list
	      format-alist)
	  (if (or (not (file-exists-p filename))
		  (y-or-n-p (format "File(%s) is already exists. Overwrite? " filename)))
	      (write-region (point-min) (point-max) filename))))
    (error "Unknown URL: %s" url)))


;;; Retrieve data:
(defsubst w3m-decode-extended-characters ()
  "Decode w3m-specific extended characters in this buffer."
  (dolist (elem w3m-extended-characters-table)
    (goto-char (point-min))
    (while (search-forward (car elem) nil t)
      (delete-region (match-beginning 0) (match-end 0))
      (insert (cdr elem)))))

(defun w3m-rendering-region (start end)
  "Do rendering of contents in this buffer as HTML and return title."
  (save-restriction
    (narrow-to-region start end)
    (let ((buf (w3m-get-buffer-create " *w3m-rendering-region*"))
	  (coding-system-for-write w3m-input-coding-system)
	  (coding-system-for-read 'binary)
	  (default-process-coding-system
	    (cons 'binary w3m-input-coding-system)))
      (with-current-buffer buf
	(delete-region (point-min) (point-max))
	(set-buffer-multibyte nil))
      (if w3m-use-form
	  (w3m-form-parse-region start end))
      (w3m-message "Rendering...")
      (apply 'call-process-region
	     start end w3m-command t buf nil
	     (mapcar (lambda (x)
		       (if (stringp x)
			   x
			 (prin1-to-string (eval x))))
		     w3m-halfdump-command-arguments))
      (w3m-message "Rendering... done")
      (goto-char (point-min))
      (insert
       (with-current-buffer buf
	 (goto-char (point-min))
	 ;; FIXME: Adhoc support for w3m with patch in [w3m-dev 01876].
	 (and (looking-at "<!DOCTYPE w3mhalfdump public")
	      (search-forward "\n<pre>\n" nil t)
	      (delete-region (point-min) (1+ (match-beginning 0))))
	 (w3m-decode-extended-characters)
	 (decode-coding-region (point-min) (point-max) w3m-output-coding-system)
	 (set-buffer-multibyte t)
	 (buffer-string)))
      (goto-char (point-min))
      (let (title)
	(dolist (regexp '("<title_alt[ \t\n]+title=\"\\([^\"]+\\)\">"
			  "<title>\\([^<]\\)</title>"))
	  (goto-char (point-min))
	  (when (re-search-forward regexp nil t)
	    (setq title (match-string 1))
	    (delete-region (match-beginning 0) (match-end 0))))
	(if (and (null title)
		 (stringp w3m-current-url)
		 (< 0 (length (file-name-nondirectory w3m-current-url))))
	    (setq title (file-name-nondirectory w3m-current-url)))
	(setq w3m-current-title (or title "<no-title>"))))))

(defun w3m-exec (url &optional buffer no-cache cs)
  "Download URL with w3m to the BUFFER.
If BUFFER is nil, all data is placed to the current buffer.  When new
content is retrieved and half-dumped data is placed in the BUFFER,
this function returns t.  Otherwise, returns nil."
  (save-excursion
    (if buffer (set-buffer buffer))
    (let ((type (w3m-retrieve url nil no-cache cs)))
      (if type
	  (cond
	   ((string-match "^text/" type)
	    (let (buffer-read-only)
	      (setq w3m-current-url (w3m-real-url url))
	      (delete-region (point-min) (point-max))
	      (insert-buffer w3m-work-buffer-name)
	      (prog1
		  (if (string= "text/html" type)
		      (progn
			(w3m-rendering-region (point-min) (point-max))
			t)
		    (setq w3m-current-title (file-name-nondirectory url))
		    nil)
		(w3m-history-push w3m-current-url
				  (list ':title w3m-current-title)))))
	   ((and (w3m-image-type-available-p (w3m-image-type type))
		 (string-match "^image/" type))
	    (let (buffer-read-only)
	      (setq w3m-current-url (w3m-real-url url)
		    w3m-current-title (file-name-nondirectory url))
	      (delete-region (point-min) (point-max))
	      (insert w3m-current-title)
	      (add-text-properties (point-min) (point-max)
				   (list 'face 'w3m-image-face
					 'w3m-image url
					 'mouse-face 'highlight))
	      (w3m-history-push w3m-current-url
				(list ':title w3m-current-title))
	      t))
	   (t (w3m-external-view url)
	      nil))
	(error "Unknown URL: %s" url)))))


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

(defun w3m-parent-page-available-p ()
  (if (null w3m-current-url)
      nil
    (save-match-data
      (string-match "[a-z]+://?[^/]+/." w3m-current-url))))

(defun w3m-view-parent-page ()
  (interactive)
  (if (null w3m-current-url)
      (error "w3m-current-url is not set"))
  (let (parent-url)
    ;; Check whether http://foo/bar/ or http://foo/bar
    (if (string-match "/$" w3m-current-url)
	(if (string-match "\\(.*\\)/[^/]+/$" w3m-current-url)
	    ;; http://foo/bar/ -> http://foo/
	    (setq parent-url (concat (match-string 1 w3m-current-url) "/")))
      (if (string-match "\\(.*\\)/.+$" w3m-current-url)
	  ;; http://foo/bar -> http://foo/
	  (setq parent-url (concat (match-string 1 w3m-current-url) "/"))))
    ;; Ignore "http:/"
    (if (and parent-url
	     (string-match "^[a-z]+:/+$" parent-url))
	(setq parent-url nil))
    (if parent-url
	(w3m-goto-url parent-url)
      (error "No parent page for: %s" w3m-current-url))))

(defun w3m-view-previous-page (&optional arg)
  (interactive "p")
  (let ((url (car (w3m-history-backward arg))))
    (when url
      (w3m-goto-url url)
      ;; restore last position.
      (w3m-history-restore-position))))

(defun w3m-view-next-page (&optional arg)
  (interactive "p")
  (let ((url (car (w3m-history-forward arg))))
    (when url
      (w3m-goto-url url)
      ;; restore last position
      (w3m-history-restore-position))))

(defun w3m-expand-url (url base)
  "Convert URL to absolute, and canonicalise it."
  (save-match-data
    (unless base
      (setq base ""))
    (when (string-match "^[^:/]+://[^/]*$" base)
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
  (let* ((type (w3m-content-type url))
	 (method (nth 2 (assoc type w3m-content-type-alist))))
    (cond
     ((not method)
      (if (w3m-url-local-p url)
	  (error "No method to view `%s' is registered."
		 (w3m-url-to-file-name url))
	(w3m-download url)))
     ((functionp method)
      (funcall method url))
     ((consp method)
      (let ((command (w3m-which-command (car method)))
	    (arguments (cdr method))
	    (file (make-temp-name
		   (expand-file-name "w3mel" w3m-profile-directory)))
	    (proc))
	(if command
	    (unwind-protect
		(with-current-buffer
		    (generate-new-buffer " *w3m-external-view*")
		  (when (memq 'file arguments)
		    (w3m-download url file))
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
		    (delete-file file))))
	  (w3m-download url)))))))

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
	(and (y-or-n-p (format "Browse <%s> ? " w3m-current-url))
	     (setq url w3m-current-url)))
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

(defun w3m-print-this-url (&optional add-kill-ring)
  "*Print the URL of the link under point."
  (interactive (list t))
  (let ((url (w3m-anchor)))
    (and add-kill-ring url (kill-new url))
    (message "%s" (or url "Not found"))))

(defun w3m-edit-current-url ()
  "*Edit the local file pointed by the URL of current page"
  (interactive)
  (if (w3m-url-local-p w3m-current-url)
      (find-file (w3m-url-to-file-name w3m-current-url))
    (error "The URL of current page is not local.")))

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
	  (new (generate-new-buffer (or newname (buffer-name))))
          (pt (point)))
      (with-current-buffer new
	;;(erase-buffer)
	(insert content)
	(narrow-to-region ptmin ptmax)
	(funcall mode)			;still needed??  -sm
	(dolist (v lvars)
	  (cond ((not (consp v))
		 (makunbound v))
		((memq (car v) '(w3m-history w3m-history-flat)))
		(t
		 (condition-case ()	;in case var is read-only
		     (set (make-local-variable (car v))
			  (if (consp (cdr v))
			      (copy-sequence (cdr v))
			    (cdr v)))
		   (error nil)))))
	;; Make copies of `w3m-history' and `w3m-history-flat'.
	(w3m-history-copy buf)
	(goto-char pt)
	(when and-pop (pop-to-buffer new))
	new))))


(defvar w3m-lynx-like-map nil
  "Lynx-like keymap used in w3m-mode buffers.")
(unless w3m-lynx-like-map
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
    (define-key map [(shift iso-lefttab)] 'w3m-previous-anchor)
    (define-key map [down] 'w3m-next-anchor)
    (define-key map "\M-\t" 'w3m-previous-anchor)
    (define-key map [up] 'w3m-previous-anchor)
    (define-key map "\C-m" 'w3m-view-this-url)
    (define-key map [right] 'w3m-view-this-url)
    (if (featurep 'xemacs)
	(define-key map [(button2)] 'w3m-mouse-view-this-url)
      (define-key map [mouse-2] 'w3m-mouse-view-this-url))
    (define-key map "\C-c\C-@" 'w3m-history-store-position)
    (define-key map [?\C-c?\C- ] 'w3m-history-store-position)
    (define-key map "\C-c\C-b" 'w3m-history-restore-position)
    (define-key map [left] 'w3m-view-previous-page)
    (define-key map "B" 'w3m-view-previous-page)
    (define-key map "N" 'w3m-view-next-page)
    (define-key map "^" 'w3m-view-parent-page)
    (define-key map "d" 'w3m-download-this-url)
    (define-key map "u" 'w3m-print-this-url)
    (define-key map "I" 'w3m-view-image)
    (define-key map "\M-I" 'w3m-save-image)
    (define-key map "c" 'w3m-print-current-url)
    (define-key map "M" 'w3m-view-current-url-with-external-browser)
    (define-key map "g" 'w3m-goto-url)
    (define-key map "t" 'w3m-toggle-inline-images)
    (define-key map "U" 'w3m-goto-url)
    (define-key map "V" 'w3m-goto-url)
    (define-key map "v" 'w3m-bookmark-view)
    (define-key map "q" 'w3m-close-window)
    (define-key map "Q" 'w3m-quit)
    (define-key map "\M-n" 'w3m-copy-buffer)
    (define-key map "R" 'w3m-reload-this-page)
    (define-key map "C" 'w3m-redisplay-with-coding-system)
    (define-key map "?" 'describe-mode)
    (define-key map "\M-a" 'w3m-bookmark-add-this-url)
    (define-key map "a" 'w3m-bookmark-add-current-url)
    (define-key map ">" 'w3m-scroll-left)
    (define-key map "<" 'w3m-scroll-right)
    (define-key map "\\" 'w3m-view-source)
    (define-key map "=" 'w3m-view-header)
    (define-key map "s" 'w3m-history)
    (define-key map "E" 'w3m-edit-current-url)
    (setq w3m-lynx-like-map map)))

(defvar w3m-info-like-map nil
  "Info-like keymap used in w3m-mode buffers.")
(unless w3m-info-like-map
  (let ((map (make-keymap)))
    (define-key map [backspace] 'scroll-down)
    (define-key map [delete] 'scroll-down)
    (define-key map "\C-?" 'scroll-down)
    (define-key map "\t" 'w3m-next-anchor)
    (define-key map [(shift tab)] 'w3m-previous-anchor)
    (define-key map [(shift iso-lefttab)] 'w3m-previous-anchor)
    (define-key map "\M-\t" 'w3m-previous-anchor)
    (define-key map "\C-m" 'w3m-view-this-url)
    (if (featurep 'xemacs)
	(define-key map [(button2)] 'w3m-mouse-view-this-url)
      (define-key map [mouse-2] 'w3m-mouse-view-this-url))
    (define-key map " " 'scroll-up)
    (define-key map "a" 'w3m-bookmark-add-current-url)
    (define-key map "\M-a" 'w3m-bookmark-add-this-url)
    (define-key map "b" 'scroll-down)
    (define-key map "C" 'w3m-redisplay-with-coding-system)
    (define-key map "d" 'w3m-bookmark-view)
    (define-key map "D" 'w3m-download-this-url)
    (define-key map "e" 'w3m-edit-current-url)
    (define-key map "f" 'undefined) ;; reserved.
    (define-key map "g" 'w3m-goto-url)
    (define-key map "h" 'describe-mode)
    (define-key map "I" 'w3m-view-image)
    (define-key map "\M-I" 'w3m-save-image)
    (define-key map "l" 'w3m-view-previous-page)
    (define-key map [(control L)] 'w3m-reload-this-page)
    (define-key map "\C-l" 'recenter)
    (define-key map "M" 'w3m-view-current-url-with-external-browser)
    (define-key map "n" 'w3m-view-next-page)
    (define-key map "\M-n" 'w3m-copy-buffer)
    (define-key map "o" 'w3m-history)
    (define-key map "p" 'w3m-view-previous-page)
    (define-key map "q" 'w3m-close-window)
    (define-key map "Q" 'w3m-quit)
    (define-key map "R" 'w3m-reload-this-page)
    (define-key map "s" 'w3m-search)
    (define-key map "t" (lambda () (interactive) (w3m-goto-url w3m-home-page)))
    (define-key map "u" 'w3m-view-parent-page)
    (define-key map "v" 'w3m-bookmark-view)
    (define-key map "y" 'w3m-print-this-url)
    (define-key map "=" 'w3m-view-header)
    (define-key map "\\" 'w3m-view-source)
    (define-key map "?" 'describe-mode)
    (define-key map ">" 'w3m-scroll-left)
    (define-key map "<" 'w3m-scroll-right)
    (define-key map "." 'beginning-of-buffer)
    (setq w3m-info-like-map map)))

(defun w3m-alive-p ()
  "When w3m is running, return that buffer.  Otherwise return nil."
  (catch 'alive
    (save-current-buffer
      (dolist (buf (buffer-list))
	(set-buffer buf)
	(when (eq major-mode 'w3m-mode)
	  (throw 'alive buf))))
    nil))

(defun w3m-quit (&optional force)
  "Quit browsing WWW after updating arrived URLs list."
  (interactive "P")
  (when (or force
	    (prog1
		(y-or-n-p "Do you want to exit w3m? ")
	      (message "")))
    (kill-buffer (current-buffer))
    (unless (w3m-alive-p)
      ;; If no w3m is running, then destruct all data.
      (w3m-cache-shutdown)
      (w3m-arrived-shutdown)
      (remove-hook 'kill-emacs-hook 'w3m-arrived-shutdown)
      (w3m-kill-all-buffer))))

(defun w3m-close-window ()
  "Close this window and make the other buffer current."
  (interactive)
  (bury-buffer (current-buffer))
  (set-window-buffer (selected-window) (other-buffer)))


(defvar w3m-mode-map nil "Keymap used in w3m-mode buffers.")
(unless w3m-mode-map
  (setq w3m-mode-map
	(if (eq w3m-key-binding 'info)
	    w3m-info-like-map
	  w3m-lynx-like-map)))

(defun w3m-mode ()
  "\\<w3m-mode-map>
   Major mode to browsing w3m buffer.

\\[w3m-view-this-url]	View this url.
\\[w3m-mouse-view-this-url]	View this url.
\\[w3m-reload-this-page]	Reload this page.
\\[w3m-redisplay-with-coding-system]	Redisplay this page with specified coding-system.
\\[w3m-next-anchor]	Jump to next anchor.
\\[w3m-previous-anchor]	Jump to previous anchor.
\\[w3m-view-previous-page]	Back to previous page.
\\[w3m-view-next-page]	Forward to next page.
\\[w3m-view-parent-page]	Upward to parent page.

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
\\[w3m-history-store-position]	Mark the current position point.
\\[w3m-history-restore-position]	Goto the last position point.

\\[w3m]	w3m.
\\[w3m-bookmark-view]	w3m-bookmark-view.
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
  (w3m-setup-toolbar)
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

(defun w3m-goto-mailto-url (url)
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

(defun w3m-convert-ftp-url-for-emacsen (url)
  (or (and (string-match "^ftp://?\\([^/@]+@\\)?\\([^/]+\\)\\(/~/\\)?" url)
	   (concat "/"
		   (if (match-beginning 1)
		       (substring url (match-beginning 1) (match-end 1))
		     "anonymous@")
		   (substring url (match-beginning 2) (match-end 2))
		   ":"
		   (substring url (match-end 2))))
      (error "URL is strange.")))

(defun w3m-goto-ftp-url (url)
  (let ((ftp (w3m-convert-ftp-url-for-emacsen url))
	(file (file-name-nondirectory url)))
    (if (string-match "\\(\\.gz\\|\\.bz2\\|\\.zip\\|\\.lzh\\)$" file)
	(copy-file ftp (w3m-read-file-name nil nil file))
      (dired-other-window ftp))))

(defun w3m-goto-url (url &optional reload cs)
  "*Retrieve contents of URL."
  (interactive
   (list
    (w3m-input-url nil
		   (when (stringp w3m-current-url)
		     (if (string-match "about://\\(header\\|source\\)/"
				       w3m-current-url)
			 (substring w3m-current-url (match-end 0))
		       w3m-current-url)))
    current-prefix-arg))
  (set-text-properties 0 (length url) nil url)
  (cond
   ;; process mailto: protocol
   ((string-match "^mailto:\\(.*\\)" url)
    (w3m-goto-mailto-url url))
   ;; process ftp: protocol
   ((and (string-match "^ftp://" url)
	 (not (string= "text/html" (w3m-local-content-type url))))
    (w3m-goto-ftp-url url))
   (t
    ;; When this buffer's major mode is not w3m-mode, generate an
    ;; appropriate buffer and select it.
    (unless (eq major-mode 'w3m-mode)
      (set-buffer (get-buffer-create "*w3m*"))
      (unless (eq major-mode 'w3m-mode)
	(w3m-mode)
	(setq mode-line-buffer-identification
	      (list "%b" " / " 'w3m-current-title))))
    ;; Setup arrived database.
    (w3m-arrived-setup)
    ;; Store the current position point in the history structure.
    (w3m-history-store-position)
    ;; Retrieve.
    (let ((orig url)
	  (name))
      (when (string-match "#\\([^#]+\\)$" url)
	(setq name (match-string 1 url)
	      url (substring url 0 (match-beginning 0))))
      (setq cs (and (not (eq cs 'reset))
		    (or cs
			(and (find-coding-system (w3m-arrived-coding-system url))
			     (w3m-arrived-coding-system url)))))
      (if (not (w3m-exec url nil reload cs))
	  (w3m-refontify-anchor)
	(w3m-fontify)
	(or (and name (w3m-search-name-anchor name))
	    (goto-char (point-min)))
	(setq w3m-display-inline-image-status 'off)
	(when w3m-display-inline-image
	  (and w3m-force-redisplay (sit-for 0))
	  (w3m-toggle-inline-images 'force reload))
	(setq buffer-read-only t)
	(set-buffer-modified-p nil))
      (setq default-directory
	    (file-name-as-directory
	     (if (w3m-url-local-p url)
		 (file-name-directory (w3m-url-to-file-name url))
	       w3m-profile-directory)))
      (w3m-arrived-add orig w3m-current-title nil nil cs)
      (w3m-update-toolbar)
      (switch-to-buffer (current-buffer))))))

(defun w3m-reload-this-page (&optional arg)
  "Reload current page without cache."
  (interactive "P")
  (let ((w3m-display-inline-image (if arg t w3m-display-inline-image)))
    (w3m-goto-url w3m-current-url 'reload)))

(defun w3m-redisplay-with-coding-system (&optional arg)
  "Redisplay current page with specified coding-system.
If input is nil, use default coding-system on w3m."
  (interactive "P")
  (let ((w3m-display-inline-image (if arg t w3m-display-inline-image))
	(cs (w3m-read-coding-system "Decode coding-system: ")))
    (unless (and cs (find-coding-system cs)) (setq cs 'reset))
    (w3m-goto-url w3m-current-url nil cs)))


;;;###autoload
(defun w3m (url &optional args)
  "Interface for w3m on Emacs."
  (interactive
   (list (or (w3m-alive-p)
	     (w3m-input-url))))
  (if (bufferp url)
      (progn
	(set-buffer url)
	(switch-to-buffer (current-buffer)))
    (w3m-goto-url url)))

(eval-when-compile
  (autoload 'browse-url-interactive-arg "browse-url"))

(defun w3m-browse-url (url &optional new-window)
  "w3m interface function for browse-url.el."
  (interactive
   (browse-url-interactive-arg "w3m URL: "))
  (when (stringp url)
    (if new-window (split-window))
    (w3m-goto-url url)))

(defun w3m-find-file (file)
  "w3m Interface function for local file."
  (interactive "fFilename: ")
  (w3m-goto-url (w3m-expand-file-name-as-url file)))


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
    (w3m-fontify)
    (setq w3m-display-inline-image-status 'off)
    (when w3m-display-inline-image
      (and w3m-force-redisplay (sit-for 0))
      (w3m-toggle-inline-images 'force))))


;;; About:
(defun w3m-about (url &rest args)
  (w3m-with-work-buffer
    (delete-region (point-min) (point-max))
    (insert "<!doctype html public \"-//W3C//DTD HTML 3.2//EN\">
<html>
<head><title>About emacs-w3m</title></head>
<body>
<center>
Welcome to <a href=\"http://namazu.org/~tsuchiya/emacs-w3m/\">emacs-w3m</a>!
<br><br>
emacs-w3m is an interface program of
<a href=\"http://ei5nazha.yz.yamagata-u.ac.jp/~aito/w3m/\">w3m</a>,
works on Emacs.
</center>
</body>
</html>"))
  "text/html")

(defun w3m-about-source (url &optional no-decode no-cache)
  (when (string-match "^about://source/" url)
    (let ((type (w3m-retrieve (substring url (match-end 0))
			      no-decode no-cache)))
      (and type "text/plain"))))

(defun w3m-view-source ()
  "*Display source of this current buffer."
  (interactive)
  (if (string-match "^about://header/" w3m-current-url)
      (message "Can't load source %s." w3m-current-url)
    (w3m-goto-url (if (string-match "^about://source/" w3m-current-url)
		      (substring w3m-current-url (match-end 0))
		    (concat "about://source/" w3m-current-url)))))

(defun w3m-about-header (url &optional no-decode no-cache)
  (when (string-match "^about://header/" url)
    (w3m-with-work-buffer
      (delete-region (point-min) (point-max))
      (let ((header (w3m-w3m-get-header (substring url (match-end 0)) no-cache)))
	(when header
	  (insert header)
	  "text/plain")))))

(defun w3m-view-header ()
  "*Display header of this current buffer."
  (interactive)
  (if (or (and (string-match "^about:" w3m-current-url)
	       (not (string-match "^about://header/" w3m-current-url)))
	  (string-match "^file://" w3m-current-url))
      (message "Can't load header %s." w3m-current-url)
    (w3m-goto-url (if (string-match "^about://header/" w3m-current-url)
		      (substring w3m-current-url (match-end 0))
		    (concat "about://header/" w3m-current-url)))))

(defconst w3m-about-history-except-regex
  "^about:\\(//\\(header\\|source\\|history\\|db-history\\|antenna\\)/.*\\)?$"
  "Regexp for not show history.")

(defun w3m-about-history-1 (history source depth)
  "Internal function used to `w3m-about-history' for recursive funcall."
  (let (element url title about children)
    (while history
      (setq element (car history)
	    history (cdr history)
	    url (car element)
	    title (plist-get (cadr element) ':title)
	    about (string-match w3m-about-history-except-regex url)
	    source (concat source
			   (if (zerop depth)
			       ""
			     (make-string depth ?│))
			   (if history
			       "├"
			     "└")
			   "<a href=\"" url "\">"
			   (when about
			     "&lt;")
			   (if (or (not title)
				   (string-equal "<no-title>" title)
				   (string-match "^[\t 　]*$" title))
			       url
			     title)
			   (when about
			     "&gt;")
			   "</a>\n")
	    children (cddr element))
      (when children
	(setq source (w3m-about-history-1 (apply 'append children)
					  source (1+ depth))))))
  source)

(defun w3m-about-history (&rest args)
  "Show a tree-structured history."
  (let ((source (w3m-about-history-1 (cdr w3m-history)
				     "\
<head><title>URL history</title></head><body>
<h1>List of all the links you have visited in this session.</h1><pre>\n"
				     0)))
    (w3m-with-work-buffer
      (erase-buffer)
      (set-buffer-multibyte t)
      (insert source)
      (goto-char (point-min))
      (when (re-search-forward "\\(└\\)\\|\\(├\\)" nil t)
	(replace-match (if (match-beginning 1)
			   "─"
			 "┬")))
      (goto-char (point-max))
      (insert "</pre></body>")))
  "text/html")

(defun w3m-about-db-history (&rest args)
  (let ((width (- (if (< 0 w3m-fill-column)
		      w3m-fill-column
		    (+ (frame-width) (or w3m-fill-column -1)))
		  26))
	(today (format-time-string "%Y-%m-%d" (current-time)))
	url title time alist date)
    (when w3m-arrived-db
      (mapatoms
       (lambda (sym)
	 (when (and sym
		    (setq url (symbol-name sym))
		    (not (string-match w3m-about-history-except-regex url)))
	   (setq time (w3m-arrived-time url))
	   (push (cons url time) alist)))
       w3m-arrived-db)
      (setq alist (sort alist (lambda (a b) (w3m-time-newer-p (cdr a) (cdr b))))))
    (w3m-with-work-buffer
      (delete-region (point-min) (point-max))
      (set-buffer-multibyte t)
      (insert "<html><head><title>URL history in DataBase</title></head><body>\n")
      (insert "<h1>arrived URL history in DataBase</h1>\n")
      (if (null alist)
	  (insert "<h2>Nothing in DataBase.</h2>\n")
	(insert "<table cellpadding=0>\n")
	(insert "<tr><td><h2> Titile/URL </h2></td><td><h2>Time/Date</h2></td></tr>\n")
	(while alist
	  (setq url (car (car alist)))
	  (setq title (w3m-arrived-title url))
	  (when (or (null title)
		    (string= "<no-title>" title))
	    (setq title (if (<= (length url) width)
			    url
			  (substring url 0 width))));; only ASCII characters.
	  (insert (format "<tr><td><a href=\"%s\">%s</a></td>" url title))
	  (when (cdr (car alist))
	    (setq date (format-time-string "%Y-%m-%d" (cdr (car alist))))
	    (if (and today (string= today date))
		(setq date (format-time-string "%H:%M:%S" (cdr (car alist))))
	      (setq today nil))
	    (insert "<td>" date "</td>"))
	  (insert "</tr>\n")
	  (setq alist (cdr alist)))
	(insert "</table>"))
      (insert "</body></html>\n")))
  "text/html")

(defun w3m-history (&optional arg)
  (interactive "P")
  (if (null arg)
      (w3m-goto-url "about://history/")
    (w3m-goto-url "about://db-history/")))

(defun w3m-db-history ()
  (interactive)
  (w3m-goto-url "about://db-history/"))

(defun w3m-w32-browser-with-fiber (url)
  (start-process "w3m-w32-browser-with-fiber"
		 (current-buffer)
		 "fiber.exe"
		 (w3m-url-to-file-name url)))


;;; Header line (emulating Emacs 21).
(defcustom w3m-use-header-line t
  "Non-nil activates header-line of w3m."
  :group 'w3m
  :type 'boolean)

(when (or (featurep 'xemacs)
	  (not (boundp 'emacs-major-version))
	  (<= emacs-major-version 20))
  ;; Faces for Emacs 21 will be declared in w3m-e21.el.
  (defface w3m-header-line-location-title-face
    '((((class color) (background light))
       (:foreground "Blue" :background "Gray90"))
      (((class color) (background dark))
       (:foreground "Cyan" :background "Gray20")))
    "Face for header-line location title."
    :group 'w3m-face)

  (defface w3m-header-line-location-content-face
    '((((class color) (background light))
       (:foreground "DarkGoldenrod" :background "Gray90"))
      (((class color) (background dark))
       (:foreground "LightGoldenrod" :background "Gray20")))
    "Face for header-line location content."
    :group 'w3m-face)

  (add-hook 'w3m-fontify-after-hook 'w3m-setup-header-line))

(unless (or (featurep 'xemacs)
	    (and (boundp 'emacs-major-version)
		 (>= emacs-major-version 21)))
  ;; The following definitions are for Emacs 19 and 20, otherwise, the
  ;; analogs for Emacs 21 or XEmacs will be defined in w3m-e21.el or
  ;; w3m-xmas.el.
  (defvar w3m-header-line-map (make-sparse-keymap))
  (define-key w3m-header-line-map [mouse-2] 'w3m-goto-url)

  (defun w3m-setup-header-line ()
    "Setup header line (emulating Emacs 21)."
    (when (and w3m-use-header-line
	       w3m-current-url
	       (eq 'w3m-mode major-mode))
      (goto-char (point-min))
      (insert "Location: ")
      (put-text-property (point-min) (point)
			 'face 'w3m-header-line-location-title-face)
      (let ((start (point)))
	(insert w3m-current-url)
	(add-text-properties start (point)
			     (list 'face
				   'w3m-header-line-location-content-face
				   'mouse-face 'highlight
				   'local-map w3m-header-line-map))
	(setq start (point))
	(insert-char ?\  (max 0 (- (window-width) (current-column) 1)))
	(put-text-property start (point)
			   'face 'w3m-header-line-location-content-face)
	(unless (eolp)
	  (insert "\n"))))))

(provide 'w3m)
;;; w3m.el ends here.

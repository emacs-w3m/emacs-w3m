;;; w3m.el --- Interface program of w3m on Emacs

;; Copyright (C) 2000,2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;          Shun-ichi GOTO     <gotoh@taiyo.co.jp>,
;;          Satoru Takabayashi <satoru-t@is.aist-nara.ac.jp>,
;;          Hideyuki SHIRAI    <shirai@meadowy.org>,
;;          Keisuke Nishida    <kxn30@po.cwru.edu>,
;;          Yuuichi Teranishi  <teranisi@gohome.org>,
;;          Akihiro Arisawa    <ari@mbf.sphere.ne.jp>,
;;          Katsumi Yamaoka    <yamaoka@jpl.org>
;; Keywords: w3m, WWW, hypermedia

;; This file is the main part of emacs-w3m.

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

;; emacs-w3m is the interface program of w3m on Emacs.  For more
;; detail about w3m, see:
;;
;;    http://ei5nazha.yz.yamagata-u.ac.jp/~aito/w3m/


;;; How to install:

;; See README and verify that the latest version of w3m is available.
;;
;; In the top level directory of the emacs-w3m distribution, run the
;; program `configure' and then type `make install'.  See README file
;; for more information.


;;; Code:

;; Developers, you must not use the cl functions (e.g. `butlast',
;; `coerce', `merge', etc.) in any emacs-w3m or shimbun modules.  To
;; exclude a run-time cl is the policy of emacs-w3m.  However, XEmacs
;; employs a cl package for all the time, or that functions were
;; possibly provided in the other modules as such as APEL, so you may
;; use them only in w3m-xmas.el or w3m-om.el.
(eval-when-compile
  (require 'cl))

;; Override the macro `dolist' which may have been defined in egg.el.
(eval-when-compile
  (unless (dolist (var nil t))
    (load "cl-macs" nil t)))

(require 'w3m-macro)

(eval-and-compile
  (cond
   ((featurep 'xemacs)
    (require 'w3m-xmas))
   ((and (boundp 'emacs-major-version)
	 (>= emacs-major-version 21))
    (require 'w3m-e21))
   ((boundp 'MULE)
    (require 'w3m-om))
   (t
    (require 'w3m-e20))))

(require 'w3m-hist)
(require 'thingatpt)
(require 'timezone)

;; Add-on programs:
(eval-and-compile
  (autoload 'w3m-bookmark-view "w3m-bookmark" nil t)
  (autoload 'w3m-bookmark-add-this-url "w3m-bookmark"
    "Add link under cursor to bookmark." t)
  (autoload 'w3m-bookmark-add-current-url "w3m-bookmark"
    "Add link of current page to bookmark." t)
  (autoload 'w3m-search "w3m-search"
    "Search QUERY using SEARCH-ENGINE." t)
  (autoload 'w3m-weather "w3m-weather"
    "Display weather report." t)
  (autoload 'w3m-about-weather "w3m-weather")
  (autoload 'w3m-antenna "w3m-antenna"
    "Display antenna report." t)
  (autoload 'w3m-about-antenna "w3m-antenna")
  (autoload 'w3m-dtree "w3m-dtree"
    "Display directory tree." t)
  (autoload 'w3m-about-dtree "w3m-dtree")
  (autoload 'w3m-namazu "w3m-namazu"
    "Search files with Namazu." t)
  (autoload 'w3m-about-namazu "w3m-namazu")
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
	   (format "1.1.%d"
		   (- (string-to-number (match-string 1 rev)) 233)))))
  "Version number of this package.")

(defgroup w3m nil
  "w3m - the web browser of choice."
  :group 'hypermedia)

(defgroup w3m-face nil
  "Faces for w3m."
  :group 'w3m
  :prefix "w3m-")

(defcustom w3m-type
  (if (and (boundp 'w3m-mnc) (symbol-value 'w3m-mnc)) 'w3m-mnc 'w3m)
  "*Type of w3m."
  :group 'w3m
  :type '(choice (const :tag "w3m" 'w3m)
		 (const :tag "w3m with MNC patch" 'w3m-mnc)
		 (const :tag "w3mmee" 'w3mmee)
		 (const :tag "w3m-m17n" 'w3m-m17n)
		 (symbol :tag "other" nil)))

(defcustom w3m-command
  (cond ((eq w3m-type 'w3mmee) "w3mmee")
	((eq w3m-type 'w3m-m17n) "w3m-m17n")
	(t "w3m"))
  "*Name of the executable file of w3m."
  :group 'w3m
  :type 'string)

(defcustom w3m-command-arguments
  (if (eq w3m-type 'w3mmee) '("-o" "concurrent=0" "-F") nil)
  "*Arguments for execution of w3m."
  :group 'w3m
  :type '(repeat string))

(defcustom w3m-process-environment
  (delq nil
	(list
	 (if (eq w3m-type 'w3mmee)
	     (cons "W3MLANG" "ja_JP.kterm"))
	 (if (eq system-type 'windows-nt)
	     (cons "CYGWIN" "binmode tty"))))
  "*Alist of environment variables for subprocesses to inherit."
  :group 'w3m
  :type '(repeat
	  (cons
	   (string :tag "Name")
	   (string :tag "Value"))))

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

(defcustom w3m-coding-system 'iso-2022-7bit
  "*Basic coding system for `w3m'."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-terminal-coding-system 'euc-japan
  "*Coding system for keyboard input to `w3m'."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-input-coding-system
  (if (memq w3m-type '(w3mmee w3m-m17n)) 'binary 'iso-2022-7bit)
  "*Coding system for write operations to `w3m'."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-output-coding-system
  (cond ((eq w3m-type 'w3mmee) 'ctext)
	((eq w3m-type 'w3m-m17n) 'iso-2022-7bit-ss2)
	(t 'w3m-euc-japan))
  "*Coding system for read operations of `w3m'."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-file-name-coding-system
  (if (memq system-type '(windows-nt OS/2 emx))
      'shift_jis 'euc-japan)
  "*Coding system for encoding file names of `w3m'."
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

(defcustom w3m-profile-directory
  (concat "~/." (file-name-nondirectory w3m-command))
  "*Directory of w3m profiles."
  :group 'w3m
  :type 'directory)

(defcustom w3m-default-save-directory
  (concat "~/." (file-name-nondirectory w3m-command))
  "*Default directory for save file."
  :group 'w3m
  :type 'directory)

(defcustom w3m-delete-duplicated-empty-lines t
  "*Compactize page by deleting duplicated empty lines."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-display-inline-image nil
  "Whether to display images inline.  It will be ignored when there is no
condition to display images."
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

(defconst w3m-emacs-w3m-icon "\
R0lGODlhQgAOAPIAAEFp4f+MAJkyzC6LV////yCyqv9FAP8AACH5BAQhAP8ALAAAAABCAA4A
AAOjSLrc/jDKSau9OOt6+tlgSHwLKZ6VwZgR4AqKIMvLDCtDPixFX0QBRVDV+Hg6DQBDCbs1
GbDdYvfjQYKEIJZg6CqM36WYGYPSbg0pz9fLCt0NFXi0dNkJZILTqYcMqhA9AYNbCypyYQtK
SXhlaH2PDmoPVViFh1yJCoubjYt5ek81BJOTDVWVDEOZdHUunZ8xZ7I0ODoSqAuElqxEKL/A
wcIKCQAh+QQFIQAAACwAAAAAAQABAAACAkQBACH5BAUhAAAALAAAAAABAAEAAAICRAEAIfkE
BSEAAAAsAgACAD4ACgAAAxhIutz+MMpJq7046827/2AojmRpnmiqhgkAIfkEBSEAAQAsAgAF
AA0ABwAAAhMMHqkK25teNPDYJmelemJveVEBACH5BAUhAAAALAYABQAXAAcAAAIdFCCZh8r/
jEGUTQjnbRLHo2mV920diV2LiYKruxQAIfkEBSEAAAAsFwAFAA0ABwAAAg/EPqCruc4ig3LW
Su27KBUAIfkEBSEAAAAsHgAFABMABwAAAxYItQX+ELY3o72MXVdx39YHStwYTUUCACH5BAUh
AAAALCwABQAPAAcAAAIUDGCni2fJnITRhVtjolkqzlwYUgAAIfkEBSEAAAAsMQAEAA8ACAAA
AxQIYNre7Elp6rzxOpY1XxoEhuKSAAA7"
  "A small icon image for the url about://emacs-w3m.gif.  It is currently
encoded in the optimized animated gif format and base64.")

(defcustom w3m-broken-proxy-cache nil
  "*If non nil, cache on proxy server is not used.
This feature should be enabled only if the caching configuration of
your proxy server is broken.  In order to use this feature, you must
apply the patch posted in [emacs-w3m:01119]."
  :group 'w3m
  :type 'boolean)


;; Generic functions:
(defun w3m-url-to-file-name (url)
  "Return the file name which is pointed by URL."
  ;; Remove scheme part and net_loc part.  NOTE: This function accepts
  ;; only urls whose net_loc part is empty or NULL string.
  (if (string-match "^\\(file:\\(//\\)?\\)/" url)
      (setq url (substring url (match-end 1)))
    (if (string-match "^\\(about://dtree\\)/" url)
	(setq url (substring url (match-end 1)))))
  ;; Process abs_path part in Windows.
  (when (string-match "^/\\(\\([A-Za-z]\\)[|:]?\\|cygdrive/\\([A-Za-z]\\)\\)/" url)
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
  "*Home page of emacs-w3m."
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

(defvar w3m-show-inline-images-before-hook nil
  "Hook run before showing inline images in `w3m-toggle-inline-images'.
It will be used for the w3m system internal for Emacs 21.")

(defvar w3m-show-inline-images-after-hook nil
  "Hook run after showing inline images in `w3m-toggle-inline-images'.
It will be used for the w3m system internal for Emacs 21.")

(defvar w3m-remove-inline-images-before-hook nil
  "Hook run before removing inline images in `w3m-toggle-inline-images'.")

(defvar w3m-remove-inline-images-after-hook nil
  "Hook run after removing inline images in `w3m-toggle-inline-images'.
It will be used for the w3m system internal for Emacs 21.")

(defcustom w3m-async-exec nil
  "*If non-nil, w3m is executed an asynchronously process."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-process-connection-type t
  "*Process connection type for w3m execution."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-default-content-type "text/html"
  "*Default content type of local files."
  :group 'w3m
  :type 'string)

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
  "*Alist of file suffixes vs. content type."
  :group 'w3m
  :type '(repeat
	  (list
	   (string :tag "Type")
	   (string :tag "Regexp")
	   (choice
	    (const :tag "None" nil)
	    (cons :tag "External viewer"
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
	 '((us_ascii      . raw-text)
	   (us-ascii      . raw-text)
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
      (or (w3m-find-coding-system (car (car rest)))
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

(defcustom w3m-edit-function 'find-file
  "*Function of editing local file."
  :group 'w3m
  :type '(choice
	  (const :tag "Edit" find-file)
	  (const :tag "Edit with other window" find-file-other-window)
	  (const :tag "Edit with other frame" find-file-other-frame)
	  (const :tag "View with other window" view-file-other-window)
	  (function :tag "Other" view-file)))

(defcustom w3m-track-mouse t
  "*Whether to track the mouse and message the url under the mouse.
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

(defcustom w3m-pop-up-frames nil
  "Like `pop-up-frames', except that it only affects the `w3m' commands."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-pop-up-frame-parameters nil
  "Alist of frame parameters used when creating a new w3m frame.  It
allows a kludge that it can also be a plist of frame properties."
  :group 'w3m
  :type '(choice (repeat (cons :format "%v"
			       (symbol :tag "Parameter")
			       (sexp :tag "Value")))
		 plist))

(defcustom w3m-mbconv-command "mbconv"
  "*Command name for \"mbconv\" be supplied with \"libmoe\"."
  :group 'w3m
  :type 'string)

(defcustom w3m-local-directory-view-method 'w3m-cgi
  "*View method in local directory.
If 'w3m-cgi, display directory tree by the use of w3m's dirlist.cgi.
If 'w3m-dtree, display directory tree by the use of w3m-dtree."
  :group 'w3m
  :type '(choice (const :tag "Dirlist CGI" w3m-cgi)
		 (const :tag "Directory tree" w3m-dtree)))

(defcustom w3m-dirlist-cgi-program
  (cond ((eq system-type 'windows-nt)
	 "c:/usr/local/lib/w3m/dirlist.cgi")
	((memq system-type '(OS/2 emx))
	 (expand-file-name "dirlist.cmd" (getenv "W3M_LIB_DIR")))
	(t nil))
  "*Name of the directory list CGI Program.
If nil, use an internal CGI of w3m."
  :group 'w3m
  :type '(choice (const :tag "w3m internal CGI" nil)
		 (file :tag "path of 'dirlist.cgi'"
		  "/usr/local/lib/w3m/dirlist.cgi")))

(defcustom w3m-add-referer t
  "*If non-nil, add refererence information when sending request to HTTP server."
  :group 'w3m
  :type 'boolean)

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
(defvar w3m-initial-frame nil "Initial frame of this session.")
(make-variable-buffer-local 'w3m-current-url)
(make-variable-buffer-local 'w3m-current-title)
(make-variable-buffer-local 'w3m-initial-frame)

(defvar w3m-current-buffer nil "The current w3m buffer.")

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
(defvar w3m-process-exit-status nil "The last exit status of a process.")

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
    "copy" "weather" "antenna" "history" "db-history")
  "Toolbar button list for w3m.")

(defconst w3m-toolbar
  (if (and (boundp 'current-language-environment)
	   (string-equal "Japanese"
			 (symbol-value 'current-language-environment)))
      '([w3m-toolbar-back-icon w3m-view-previous-page
			       (w3m-history-previous-link-available-p)
			       "前のページに戻る"]
	[w3m-toolbar-parent-icon w3m-view-parent-page
				 (w3m-parent-page-available-p)
				 "上のディレクトリへ移動する"]
	[w3m-toolbar-forward-icon w3m-view-next-page
				  (w3m-history-next-link-available-p)
				  "次のページに進む"]
	[w3m-toolbar-reload-icon w3m-reload-this-page
				 w3m-current-url
				 "サーバからページをもう一度読み込む"]
	[w3m-toolbar-open-icon w3m-goto-url t "URL を入力してページを開く"]
	[w3m-toolbar-home-icon w3m-gohome w3m-home-page
			       "ホームページへジャンプ"]
	[w3m-toolbar-search-icon w3m-search t "インターネット上を検索"]
	[w3m-toolbar-image-icon w3m-toggle-inline-images t
				"画像の表示をトグルする"]
	[w3m-toolbar-copy-icon w3m-copy-buffer t
			       "このセッションのコピーを作る"]
	[w3m-toolbar-weather-icon w3m-weather t "天気予報を見る"]
	[w3m-toolbar-antenna-icon w3m-antenna t "アンテナで受信する"]
	[w3m-toolbar-history-icon w3m-history t "ヒストリー"]
	[w3m-toolbar-db-history-icon w3m-db-history t "ＤＢヒストリー"])
    '([w3m-toolbar-back-icon w3m-view-previous-page
			     (w3m-history-previous-link-available-p)
			     "Back to Previous Page"]
      [w3m-toolbar-parent-icon w3m-view-parent-page
			       (w3m-parent-page-available-p)
			       "Upward to Parent Page"]
      [w3m-toolbar-forward-icon w3m-view-next-page
				(w3m-history-next-link-available-p)
				"Forward to Next Page"]
      [w3m-toolbar-reload-icon w3m-reload-this-page
			       w3m-current-url
			       "Reload This Page"]
      [w3m-toolbar-open-icon w3m-goto-url t "Go to..."]
      [w3m-toolbar-home-icon w3m-gohome w3m-home-page "Go to Home Page"]
      [w3m-toolbar-search-icon w3m-search t "Search the Internet"]
      [w3m-toolbar-image-icon w3m-toggle-inline-images t "Toggle Images"]
      [w3m-toolbar-copy-icon w3m-copy-buffer t "Make a Copy of This Session"]
      [w3m-toolbar-weather-icon w3m-weather t "Weather Forecast"]
      [w3m-toolbar-antenna-icon w3m-antenna t "Investigate with Antenna"]
      [w3m-toolbar-history-icon w3m-history t "Show a History"]
      [w3m-toolbar-db-history-icon w3m-db-history t "Show a DB History"]))
  "Toolbar definition for w3m.")

(defconst w3m-menubar
  '("W3M"
    ["Back to Previous Page" w3m-view-previous-page
     (w3m-history-previous-link-available-p)]
    ["Forward to Next Page" w3m-view-next-page
     (w3m-history-next-link-available-p)]
    ["Upward to Parent Page" w3m-view-parent-page
     (w3m-parent-page-available-p)]
    ["Reload This Page" w3m-reload-this-page w3m-current-url]
    ["Go to..." w3m-goto-url t]
    ["Go to Home Page" w3m-gohome w3m-home-page]
    ["Search the Internet" w3m-search t]
    ["Toggle Images" w3m-toggle-inline-images (w3m-display-graphic-p)]
    ["Make a Copy of This Session" w3m-copy-buffer t]
    ["Weather Forecast" w3m-weather t]
    ["Investigate with Antenna" w3m-antenna t]
    ["Show a History" w3m-history t]
    ["Show a DB History" w3m-db-history t]
    ["Download This URL" w3m-download-this-url t]
    ["Print Current URL" w3m-print-current-url t]
    ["View Bookmark" w3m-bookmark-view t]
    )
  "Menubar definition for w3m.")

(defvar w3m-cid-retrieve-function-alist nil)
(defvar w3m-force-redisplay t)

(eval-and-compile
  (condition-case nil
      :symbol-for-testing-whether-colon-keyword-is-available-or-not
    (void-variable
     (eval '(defconst :bool ':bool))
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

(defconst w3m-dump-head-source-command-argument
  (if (eq w3m-type 'w3mmee) "-dump=extra,head,source" "-dump_extra")
  "Arguments for 'dump_extra' execution of w3m.")

(defvar w3m-halfdump-command nil
  "Name of the executable file of w3m. If nil use 'w3m-command'.")

(defmacro w3m-tag-regexp-of (&rest names)
  (concat "<\\("
	  (mapconcat 'identity names "\\|")
	  "\\)\\([ \t\r\f\n]+[^>]*\\)?/?>"))

(defconst w3m-halfdump-command-arguments
  (cond ((eq w3m-type 'w3mmee)
	 (list "-dump=half-buffer"
	       '(if charset "-I")
	       'charset
	       "-o" "concurrent=0"))
	((eq w3m-type 'w3m-m17n)
	 (list "-halfdump"
	       "-o" "ext_halfdump=1"
	       '(if charset "-I") 'charset
	       "-O" "ISO-2022-JP-2" "-o" "strict_iso2022=0"))
	(t (list "-halfdump")))
  "Arguments for 'halfdump' execution of w3m.")

(defconst w3m-halfdump-command-common-arguments
  '("-T" "text/html" "-t" tab-width
    "-cols" (if (< 0 w3m-fill-column)
		w3m-fill-column		; fixed columns
	      (+ (frame-width) (or w3m-fill-column -1)))) ; fit for frame
  "Common arguments for 'halfdump' execution of all w3m variants.")

(defconst w3m-arrived-ignored-regexp
  "^about:\\(//\\(header\\|source\\|history\\|db-history\\|antenna\\)/.*\\)?$"
  "Regexp of urls to be ignored in an arrived-db.")

(defconst w3m-history-ignored-regexp
  "^about:\\(//\\(header\\|source\\|history\\|db-history\\|antenna\\|dtree\\)/.*\\)?$"
  "Regexp of urls to be ignored in a history.")

(defvar w3m-mode-map nil "Keymap used in w3m-mode buffers.")

;; Generic macros and inline functions:
(defsubst w3m-attributes (url &optional no-cache)
  "Return a list of attributes of URL.
Value is nil if retrieval of header is failed.  Otherwise, list
elements are:
 0. Type of contents.
 1. Charset of contents.
 2. Size in bytes.
 3. Encoding of contents.
 4. Last modification time.
 5. Real URL.
If optional argument NO-CACHE is non-nil, cache is not used."
  (when (string-match "#[^#]+$" url)
    (setq url (substring url 0 (match-beginning 0))))
  (cond
   ((string-equal "about://emacs-w3m.gif" url)
    (list "image/gif" nil nil nil nil url))
   ((string-match "^about:" url)
    (list "text/html" w3m-coding-system nil nil nil url))
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

(defmacro w3m-get-text-property-around (prop &optional position)
  "Search for the text property PROP in the POSITION and return a value
or nil.  If POSITION is omitted, searching is performed in the current
cursor position and around there."
  (if position
      (` (get-text-property (, position) (, prop)))
    (` (let ((position (point)))
	 (or (get-text-property position (, prop))
	     (and (not (bolp))
		  (get-text-property (1- position) (, prop)))
	     (and (not (eolp))
		  (get-text-property (1+ position) (, prop))))))))

(defmacro w3m-anchor (&optional position)
  (` (w3m-get-text-property-around 'w3m-href-anchor (, position))))
(defmacro w3m-image (&optional position)
  (` (w3m-get-text-property-around 'w3m-image (, position))))
(defmacro w3m-action (&optional position)
  (` (w3m-get-text-property-around 'w3m-action (, position))))
(defmacro w3m-submit (&optional position)
  (` (w3m-get-text-property-around 'w3m-submit (, position))))

(defmacro w3m-cursor-anchor (&optional position)
  (if position
      (` (get-text-property (, position) 'w3m-cursor-anchor))
    (` (get-text-property (point) 'w3m-cursor-anchor))))

(defmacro w3m-make-help-echo (property)
  "Make a function for showing a `help-echo' string."
  (if (featurep 'xemacs)
      (` (if (and (boundp 'emacs-major-version)
		  (>= emacs-major-version 21))
	     (function
	      (lambda (extent)
		(if (and w3m-track-mouse
			 (eq (extent-object extent) (current-buffer)))
		    (get-text-property (extent-start-position extent)
				       (quote (, property))))))))
    (` (if (and (boundp 'emacs-major-version)
		(>= emacs-major-version 21))
	   (function
	    (lambda (window object pos)
	      (if w3m-track-mouse
		  (get-text-property pos (quote (, property))
				     (window-buffer window)))))))))

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
		 (if (and w3m-track-mouse
			  (eq (extent-object extent) (current-buffer)))
		     (get-text-property (extent-start-position extent)
					(quote (, property)))))))
	   (when (and (featurep 'bytecomp)
		      (not (compiled-function-p (symbol-function fn))))
	     (byte-compile fn)))))))

(defmacro w3m-pop-up-frame-parameters ()
  "Return a pop-up frame plist if this file is compiled for XEmacs,
otherwise return an alist."
  (if (featurep 'xemacs)
      '(let ((params (or w3m-pop-up-frame-parameters pop-up-frame-plist)))
	 (if (consp (car-safe params))
	     (alist-to-plist params)
	   params))
    '(let ((params (or w3m-pop-up-frame-parameters pop-up-frame-alist))
	   alist)
       (if (consp (car-safe params))
	   params
	 (while params
	   (push (cons (car params) (cdr params)) alist)
	   (setq params (cddr params)))
	 (nreverse alist)))))

(defmacro w3m-popup-frame-p ()
  "Return non-nil if the command `w3m' should popup a new frame."
  (list 'and 'w3m-pop-up-frames '(interactive-p)
	(if (featurep 'xemacs)
	    '(device-on-window-system-p)
	  'window-system)))

(defun w3m-message (&rest args)
  "Alternative function of `message' for emacs-w3m."
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
    (ignore-errors
      (let ((fn (when (fboundp 'parse-time-string)
		  'parse-time-string)))
	(when fn
	  (apply (function encode-time) (funcall fn string)))))))

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
  (defmacro w3m-arrived-add-1 (ident title modified-time arrived-time
				     content-charset content-type)
    (` (progn
	 (put (, ident) 'title (, title))
	 (put (, ident) 'last-modified (, modified-time))
	 (put (, ident) 'content-charset (, content-charset))
	 (put (, ident) 'content-type (, content-type))
	 (set (, ident) (, arrived-time))))))

(defun w3m-arrived-add (url &optional title modified-time
			    arrived-time content-charset content-type)
  "Add URL to hash database of arrived URLs."
  (unless (or (<= (length url) 5);; ignore trifles or about:*.
	      (string-match w3m-arrived-ignored-regexp url))
    (let ((parent (when (string-match "#[^#]+$" url)
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
	(w3m-arrived-add-1 ident title modified-time arrived-time
			   content-charset content-type)
	(when parent
	  (setq ident (intern parent w3m-arrived-db))
	  (w3m-arrived-add-1 ident title modified-time arrived-time
			     content-charset content-type))))))

(defsubst w3m-arrived-p (url)
  "If URL has been arrived, return non-nil value.  Otherwise return nil."
  (or (string-match w3m-arrived-ignored-regexp url)
      (intern-soft url w3m-arrived-db)))

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

(defun w3m-arrived-content-charset (url)
  "If URL has been specified content-charset, return its content-charset.
  Otherwise return nil."
  (let ((v (intern-soft url w3m-arrived-db)))
    (and v (get v 'content-charset))))

(defun w3m-arrived-content-type (url)
  "If URL has been specified content-type, return its content-type.
  Otherwise return nil."
  (let ((v (intern-soft url w3m-arrived-db)))
    (and v (get v 'content-type))))

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
			     (when (stringp (nth 4 elem)) (nth 4 elem))
			     (nth 5 elem))
	  ;; Process old format of arrived URL database, is used
	  ;; before revision 1.135.
	  (w3m-arrived-add (car elem) nil (cdr elem) (cdr elem))))
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
			(when (stringp (nth 4 elem)) (nth 4 elem))
			(nth 5 elem))))
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
			  (get sym 'content-charset)
			  (get sym 'content-type))
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
	     ((eq ch ?\n) ; newline
	      "%0D%0A")
	     ((string-match "[-a-zA-Z0-9_:/]" (char-to-string ch)) ; xxx?
	      (char-to-string ch))	; printable
	     ((char-equal ch ?\x20)	; space
	      "+")
	     (t
	      (format "%%%02X" ch))))	; escape
	  ;; Coerce a string to a list of chars.
	  (append (encode-coding-string (or str "") (or coding 'iso-2022-jp))
		  nil))))

(put 'w3m-parse-attributes 'lisp-indent-function '1)
(def-edebug-spec w3m-parse-attributes
  ((&rest &or (symbolp &optional symbolp) symbolp) body))
(defmacro w3m-parse-attributes (attributes &rest form)
  (` (let ((,@ (mapcar
		(lambda (attr)
		  (if (listp attr) (car attr) attr))
		attributes)))
       (skip-chars-forward " \t\r\f\n")
       (while
	   (cond
	    (,@ (mapcar
		 (lambda (attr)
		   (or (symbolp attr)
		       (and (listp attr)
			    (<= (length attr) 2)
			    (symbolp (car attr)))
		       (error "Internal error, type mismatch"))
		   (let ((sexp (quote
				(w3m-remove-redundant-spaces
				 (or (match-string-no-properties 2)
				     (match-string-no-properties 3)
				     (match-string-no-properties 1)))))
			 type)
		     (when (listp attr)
		       (setq type (nth 1 attr))
		       (cond
			((eq type :case-ignore)
			 (setq sexp (list 'downcase sexp)))
			((eq type :integer)
			 (setq sexp (list 'string-to-number sexp)))
			((eq type :bool)
			 (setq sexp t))
			((nth 1 attr)
			 (error "Internal error, unknown modifier")))
		       (setq attr (car attr)))
		     (` ((looking-at
			  (, (if (eq type :bool)
				 (symbol-name attr)
			       (format "%s[ \t\r\f\n]*=[ \t\r\f\n]*%s"
				       (symbol-name attr)
				       w3m-html-string-regexp))))
			 (setq (, attr) (, sexp))))))
		 attributes))
	    ((looking-at
	      (, (concat "[A-Za-z]*[ \t\r\f\n]*=[ \t\r\f\n]*" w3m-html-string-regexp))))
	    ((looking-at "[^<> \t\r\f\n]+")))
	 (goto-char (match-end 0))
	 (skip-chars-forward " \t\r\f\n"))
       (skip-chars-forward "^>")
       (forward-char)
       (,@ form))))


;;; HTML character entity handling:
(defun w3m-entity-db-setup ()
  ;; initialize entity database (obarray)
  (setq w3m-entity-db (make-vector w3m-entity-db-size 0))
  (dolist (elem w3m-entity-alist)
    (set (intern (car elem) w3m-entity-db)
	 (cdr elem))))

(defsubst w3m-entity-value (name)
  ;; initialize if need
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
	(w3m-add-text-properties start (match-beginning 0) '(face bold))))))

(defun w3m-fontify-underline ()
  "Fontify underline characters in this buffer which contains half-dumped data."
  (goto-char (point-min))
  (while (search-forward "<u>" nil t)
    (let ((start (match-beginning 0)))
      (delete-region start (match-end 0))
      (when (search-forward "</u>" nil t)
	(delete-region (match-beginning 0) (match-end 0))
	(w3m-add-text-properties start (match-beginning 0)
				 '(face underline))))))

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
  (let ((help (w3m-make-help-echo w3m-href-anchor))
	(balloon (w3m-make-balloon-help w3m-href-anchor))
	start end)
    (goto-char (point-min))
    (while (re-search-forward "<_id[ \t\r\f\n]+" nil t)
      (setq start (match-beginning 0))
      (w3m-parse-attributes (id)
	(delete-region start (point))
	(when (re-search-forward "<\\|\n" nil t)
	  (setq end (match-beginning 0))
	  (when (= start end)
	    (setq end (min (1+ end) (point-max))))
	  (w3m-add-text-properties start end
				   (list 'w3m-name-anchor id)))))
    (goto-char (point-min))
    (while (re-search-forward "<a[ \t\r\f\n]+" nil t)
      (setq start (match-beginning 0))
      (w3m-parse-attributes (href name)
	(delete-region start (point))
	(cond
	 (href
	  (when (re-search-forward "[ \t\r\f\n]*\\(</a>\\)" nil t)
	    (setq end (match-beginning 0))
	    (delete-region (match-beginning 1) (match-end 1))
	    (setq href (w3m-expand-url (w3m-decode-anchor-string href)
				       w3m-current-url))
	    (w3m-add-text-properties start end
				     (list 'face (if (w3m-arrived-p href)
						     'w3m-arrived-anchor-face
						   'w3m-anchor-face)
					   'w3m-href-anchor href
					   'w3m-cursor-anchor href
					   'mouse-face 'highlight
					   'w3m-name-anchor name
					   'help-echo help
					   'balloon-help balloon))))
	 (name
	  (when (re-search-forward "<\\|\n" nil t)
	    (setq end (match-beginning 0))
	    (when (= start end)
	      (setq end (min (1+ end) (point-max))))
	    (w3m-add-text-properties start end
				     (list 'w3m-name-anchor name)))))))))

(defun w3m-image-type (content-type)
  "Return image type which corresponds to CONTENT-TYPE."
  (cdr (assoc content-type w3m-image-type-alist)))

(eval-and-compile
  (unless (featurep 'xemacs)
    (defun w3m-setup-menu ()
      "Define menubar buttons for FSF Emacsen."
      (let ((items (mapcar 'car (cdr (lookup-key global-map [menu-bar])))))
	(when items
	  ;; Locate W3M menu in the forefront of the menubar.
	  (set (make-local-variable 'menu-bar-final-items)
	       (delq 'w3m items))))
      (unless (keymapp (lookup-key w3m-mode-map [menu-bar w3m]))
	(let ((map (make-sparse-keymap (car w3m-menubar))))
	  (define-key w3m-mode-map [menu-bar] (make-sparse-keymap))
	  (define-key w3m-mode-map [menu-bar w3m] (cons (car w3m-menubar) map))
	  (dolist (def (reverse (cdr w3m-menubar)))
	    (define-key map (vector (aref def 1)) (cons (aref def 0)
							(aref def 1)))
	    (put (aref def 1) 'menu-enable (aref def 2)))
	  ;; (define-key map [separator-eval] '("--"))
	  )))))

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
	(w3m-add-text-properties start end (list 'w3m-image src
						 'w3m-image-redundant upper))
	(unless (get-text-property start 'w3m-href-anchor)
	  ;; No need to use `w3m-add-text-properties' here.
	  (add-text-properties start end (list 'face 'w3m-image-face
					       'mouse-face 'highlight
					       'help-echo help
					       'balloon-help balloon)))))))

(defun w3m-toggle-inline-images (&optional force no-cache)
  "Toggle displaying of inline images on current buffer.
If optional argument FORCE is non-nil, displaying is forced.
If second optional argument NO-CACHE is non-nil, cache is not used."
  (interactive "P")
  (unless (w3m-display-graphic-p)
    (error "Can't display images in this environment"))
  (unless (and force (eq w3m-display-inline-image-status 'on))
    (let ((cur-point (point))
	  (buffer-read-only)
	  point end url image)
      (if (or force (eq w3m-display-inline-image-status 'off))
	  (save-excursion
	    (message "Showing images...")
	    (run-hooks 'w3m-show-inline-images-before-hook)
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
		    (w3m-add-text-properties point end '(invisible t))
		    (setq point (point))
		    (insert image)
		    (w3m-add-text-properties point (point)
					     '(w3m-image-dummy
					       t
					       w3m-image "dummy")))
		(save-excursion
		  (goto-char cur-point)
		  (when (and url
			     (setq image (w3m-create-image url no-cache
							   w3m-current-url)))
		    (w3m-insert-image point end image)
		    ;; Redisplay
		    (and w3m-force-redisplay (sit-for 0))))))
	    (run-hooks 'w3m-show-inline-images-after-hook)
	    (message "Showing images...done")
	    (setq w3m-display-inline-image-status 'on))
	(save-excursion
	  (run-hooks 'w3m-remove-inline-images-before-hook)
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
	      (remove-text-properties point end '(invisible nil)))
	     ((get-text-property point 'w3m-image-dummy)
	      ;; Remove dummy string.
	      (delete-region point end))
	     (t (w3m-remove-image point end))))
	  (run-hooks 'w3m-remove-inline-images-after-hook)
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
	    (w3m-add-text-properties (match-beginning 0) (point) prop))))))

(defun w3m-fontify ()
  "Fontify this buffer."
  (let ((case-fold-search t)
	(buffer-read-only))
    (run-hooks 'w3m-fontify-before-hook)
    (w3m-message "Fontifying...")
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
    (while (re-search-forward "</?[A-Za-z_][^>]*>" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    ;; Decode escaped characters (entities).
    (w3m-decode-entities 'reserve-prop)
    (goto-char (point-min))
    (if w3m-delete-duplicated-empty-lines
	(while (re-search-forward "^[ \t]*\n\\([ \t]*\n\\)+" nil t)
	  (replace-match "\n" nil t)))
    (w3m-message "Fontifying...done")
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
	  (w3m-add-text-properties start end '(face w3m-arrived-anchor-face)))
	(set-buffer-modified-p nil)))))

(defun w3m-url-completion (url predicate flag)
  "Completion function for URL."
  (if (string-match "^\\(file:\\|/\\|~\\|[a-zA-Z]:\\)" url)
      (if (eq flag 'lambda)
	  (file-exists-p (w3m-url-to-file-name url))
	(let* ((partial
		(expand-file-name
		 (cond
		  ((string-match "^file:[^/]" url)
		   (substring url 5))
		  ((string-match "/\\(~\\)" url)
		   (substring url (match-beginning 1)))
		  (t (w3m-url-to-file-name url)))))
	       (collection
		(let ((dir (file-name-directory partial)))
		  (mapcar
		   (lambda (f)
		     (list (w3m-expand-file-name-as-url f dir)))
		   (file-name-all-completions (file-name-nondirectory partial)
					      dir)))))
	  (setq partial (w3m-expand-file-name-as-url partial))
	  (cond
	   ((not flag)
	    (try-completion partial collection predicate))
	   ((eq flag t)
	    (all-completions partial collection predicate)))))
    (cond
     ((not flag)
      (try-completion url w3m-arrived-db))
     ((eq flag t)
      (all-completions url w3m-arrived-db))
     ((eq flag 'lambda)
      (if (w3m-arrived-p url) t nil)))))

(defun w3m-input-url (&optional prompt default)
  "Read a URL from the minibuffer, prompting with string PROMPT."
  (let (url candidates)
    (w3m-arrived-setup)
    (unless default
      (setq default (thing-at-point 'url)))
    (setq url (let ((minibuffer-setup-hook
		     (append minibuffer-setup-hook '(beginning-of-line))))
		(completing-read
		 (or prompt
		     (if default
			 "URL: "
		       (format "URL (default %s): " w3m-home-page)))
		 'w3m-url-completion nil nil default
		 'w3m-input-url-history)))
    (if (string= "" url) (setq url w3m-home-page))
    ;; remove duplication
    (setq w3m-input-url-history
	  (cons url (delete url w3m-input-url-history)))
    ;; return value
    url))


;;; Cache:
(defun w3m-cache-setup ()
  "Initialize cache variables."
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
	(let ((b (point)))
	  (insert-buffer-substring buffer)
	  ;; Tag the beginning of the article with the ident.
	  (when (> (point-max) b)
	    (w3m-add-text-properties b (1+ b) (list 'w3m-cache ident))
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
  "Run w3m-command and return t if succeeded otherwise nil."
  (save-excursion
    (let ((coding-system-for-read 'binary)
	  (coding-system-for-write w3m-terminal-coding-system)
	  (default-process-coding-system
	    (cons 'binary w3m-terminal-coding-system))
	  (process-connection-type w3m-process-connection-type)
	  (process-environment process-environment)
	  status)
      (dolist (elem w3m-process-environment)
	(setenv (car elem) (cdr elem)))
      (setq args (append w3m-command-arguments args)
	    w3m-process-exit-status nil)
      (if w3m-async-exec
	  ;; start-process
	  (let ((w3m-process-user)
		(w3m-process-passwd)
		(w3m-process-user-counter 2)
		(proc (apply 'start-process w3m-command (current-buffer)
			     w3m-command args)))
	    (set-process-filter proc 'w3m-exec-filter)
	    (set-process-sentinel proc 'ignore)
	    (process-kill-without-query proc)
            (unwind-protect
                (progn
		  (while (eq (process-status proc) 'run)
		    (accept-process-output nil 0 200))
		  (setq status (process-exit-status proc))
		  (and w3m-current-url
		       w3m-process-user
		       (setq w3m-arrived-user-list
			     (cons
			      (cons w3m-current-url
				    (list w3m-process-user w3m-process-passwd))
			      (delq (assoc w3m-current-url
					   w3m-arrived-user-list)
				    w3m-arrived-user-list)))))
              (delete-process proc)));; Clean up resources of process.
	;; call-process
	(setq status (apply 'call-process w3m-command nil t nil args)))
      (cond ((numberp status)
	     (zerop (setq w3m-process-exit-status status)))
	    ((not status)
	     nil)
	    (t
	     (setq w3m-process-exit-status
		   (string-as-multibyte (format "%s" status)))
	     nil)))))

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

(defvar w3m-proxy-user nil)
(defvar w3m-proxy-passwd nil)

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
		    "\\(\nWrong username or password\n\\)?Proxy Username for \\(.*\\): Proxy Password: ")
		   (= (match-end 0) (point-max)))
	      (unless w3m-proxy-passwd
		(setq w3m-proxy-passwd
		      (read-passwd "Proxy Password: ")))
	      (condition-case nil
		  (progn
		    (process-send-string process
					 (concat w3m-proxy-passwd "\n"))
		    (delete-region (point-min) (point-max)))
		(error nil)))
	     ((and (looking-at
		    "\\(\nWrong username or password\n\\)?Proxy Username for \\(.*\\): ")
		   (= (match-end 0) (point-max)))
	      (unless w3m-proxy-user
		(setq w3m-proxy-user
		      (read-from-minibuffer (concat 
					     "Proxy Username for "
					     (match-string 2) ": "))))
	      (condition-case nil
		  (process-send-string process
				       (concat w3m-proxy-user "\n"))
		(error nil)))
	     ((and (looking-at
		    "\\(\nWrong username or password\n\\)?Username for \\(.*\\): Password: ")
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
		    "\\(\nWrong username or password\n\\)?Username for \\(.*\\): ")
		   (= (match-end 0) (point-max)))
	      (setq w3m-process-user
		    (or (nth 0 (w3m-exec-get-user w3m-current-url))
			(read-from-minibuffer (concat
					       "Username for " (match-string 2)
					       ": "))))
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
    (w3m-find-coding-system (if cs (cdr cs) charset))))

;; FIXME: 現実に有り得る Content-charset: の調査が必要
(defun w3m-read-content-charset (prompt &optional default)
  "Read a content charset from the minibuffer, prompting with string PROMPT.
If the user enters null input, return second argument DEFAULT."
  (let ((charset (completing-read
		  prompt
		  (nconc
		   (mapcar (lambda (c) (cons (symbol-name c) c))
			   (coding-system-list))
		   (mapcar (lambda (c) (cons (symbol-name (car c)) c))
			   w3m-charset-coding-system-alist))
		  nil t)))
    (if (string= "" charset)
	default
      charset)))


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

(defun w3m-decode-encoded-contents (encoding)
  "Decode encoded (gzipped, bzipped, deflated, etc) contents in this buffer."
  (let ((x (and (stringp encoding)
		(assoc encoding w3m-encoding-alist))))
    (or (not (and x (setq x (cdr (assq (cdr x) w3m-decoder-alist)))))
	(let ((coding-system-for-write 'binary)
	      (coding-system-for-read 'binary)
	      (default-process-coding-system (cons 'binary 'binary))
	      (process-environment process-environment))
	  (dolist (elem w3m-process-environment)
	    (setenv (car elem) (cdr elem)))
	  (zerop (apply 'call-process-region
			(point-min) (point-max)
			(w3m-which-command (car x))
			t '(t nil) nil (nth 1 x)))))))

(defun w3m-decode-buffer (url &optional content-charset content-type)
  (unless content-charset
    (setq content-charset
	  (or (w3m-content-charset url)
	      (when (string= "text/html"
			     (or content-type (w3m-content-type url)))
		(let ((case-fold-search t))
		  (goto-char (point-min))
		  (when (or (re-search-forward
			     w3m-meta-content-type-charset-regexp nil t)
			    (re-search-forward
			     w3m-meta-charset-content-type-regexp nil t))
		    (match-string-no-properties 2)))))))
  (when (and (eq w3m-type 'w3mmee)
	     (or (and (stringp content-charset)
		      (string= "x-moe-internal" (downcase content-charset)))
		 (eq content-charset 'x-moe-internal)))
    (setq content-charset (w3m-x-moe-decode-buffer)))
  (decode-coding-region
   (point-min) (point-max)
   (if content-charset
       (w3m-charset-to-coding-system content-charset)
     (let ((codesys (detect-coding-region (point-min) (point-max))))
       (if (consp codesys)
	   (car codesys)
	 codesys))))
  (set-buffer-multibyte t))

(defun w3m-x-moe-decode-buffer ()
  (let ((args '("-i" "-cs" "x-moe-internal"))
	(coding-system-for-read 'binary)
	(coding-system-for-write 'binary)
	(default-process-coding-system (cons 'binary 'binary))
	(process-environment process-environment)
	charset)
    (dolist (elem w3m-process-environment)
      (setenv (car elem) (cdr elem)))
    (if (w3m-find-coding-system 'utf-8)
	(setq args (append args '("-o" "-cs" "utf-8"))
	      charset 'utf-8)
      (setq args
	    (append args (list "-o" "-cs" (symbol-name w3m-coding-system))))
      (setq charset w3m-coding-system))
    (apply 'call-process-region (point-min) (point-max)
	   w3m-mbconv-command t t nil args)
    charset))

;;; Retrieve local data:
(defun w3m-local-content-type (url)
  (if (file-directory-p (w3m-url-to-file-name url))
      "text/html"
    (catch 'type-detected
      (dolist (elem w3m-content-type-alist "unknown")
	(if (string-match (nth 1 elem) url)
	    (throw 'type-detected (car elem)))))))

(defun w3m-local-attributes (url &rest args)
  "Return a list of attributes of URL.
Value is nil if retrieval of header is failed.  Otherwise, list
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
  (let ((file (w3m-url-to-file-name url)))
    (w3m-with-work-buffer
      (delete-region (point-min) (point-max))
      (set-buffer-multibyte nil)
      (when (file-readable-p file)
	(if (not (file-directory-p file))
	    (let ((coding-system-for-read 'binary)
		  (file-coding-system-for-read 'binary))
	      (if no-decode
		  (let (jka-compr-compression-info-list
			jam-zcat-filename-list
			format-alist)
		    (insert-file-contents file)))
	      (insert-file-contents file))
	  (w3m-local-dirlist-cgi url))
	(w3m-local-content-type url)))))

(defun w3m-local-dirlist-cgi (url)
  (w3m-message "Reading %s..." url)
  (if w3m-dirlist-cgi-program
      (if (file-executable-p w3m-dirlist-cgi-program)
	  (let ((coding-system-for-read 'binary)
		(default-process-coding-system
		  (cons 'binary 'binary))
		(process-environment process-environment)
		file beg end)
	    (setenv "QUERY_STRING"
		    (encode-coding-string (w3m-url-to-file-name url)
					  w3m-file-name-coding-system))
	    (call-process w3m-dirlist-cgi-program nil t nil)
	    (goto-char (point-min))
	    (when (re-search-forward "^<html>" nil t)
	      (delete-region (point-min) (match-beginning 0))
	      (while (re-search-forward
		      "<a href=\"\\([^\"]+\\)\"\\(>\\| \\)" nil t)
		(setq file (match-string 1))
		(setq beg (match-beginning 1))
		(setq end (match-end 1))
		(if (file-directory-p file)
		    (setq file (w3m-expand-file-name-as-url
				(file-name-as-directory file)))
		  (setq file (w3m-expand-file-name-as-url file)))
		(delete-region beg end)
		(goto-char beg)
		(insert file))))
	(error "Can't execute: %s" w3m-dirlist-cgi-program))
    ;; execute w3m internal CGI
    (w3m-exec-process "-dump_source" url))
  ;; bind charset to w3m-file-name-coding-system
  (let ((charset (or (car (rassq w3m-file-name-coding-system
				 w3m-charset-coding-system-alist))
		     w3m-file-name-coding-system)))
    (goto-char (point-min))
    (when (re-search-forward "<head>" nil t)
      (insert "\n<meta http-equiv=\"CONTENT-TYPE\" "
	      "content=\"text/html; charset="
	      (symbol-name charset)
	      "\">")))
  (w3m-message "Reading %s...done" url))

;;; Retrieve data via HTTP:
(defun w3m-remove-redundant-spaces (str)
  "Remove spaces/tabs at the front of a string and at the end of a string"
  (save-match-data
    (setq str
	  (substring str
		     (if (string-match "^[ \t\r\f\n]+" str) (match-end 0) 0)))
    (substring str 0
	       (and (string-match "[ \t\r\f\n]+$" str) (match-beginning 0)))))

(defun w3m-w3m-get-header (url &optional no-cache)
  "Return the header string of the URL.
If optional argument NO-CACHE is non-nil, cache is not used."
  (or (unless no-cache
	(w3m-cache-request-header url))
      (with-temp-buffer
	(let ((w3m-current-url url))
	  (w3m-message "Request sent, waiting for response...")
	  (when (prog1
		    (w3m-exec-process "-dump_head" url)
		  (w3m-message "Request sent, waiting for response...done"))
	    (w3m-cache-header url (buffer-string)))))))

(defun w3m-w3m-attributes (url &optional no-cache)
  "Return a list of attributes of URL.
Value is nil if retrieval of header is failed.  Otherwise, list
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
	  (if (string-match ";[ \t]*charset=\"?\\([^\"]+\\)\"?" type)
	      (setq charset (w3m-remove-redundant-spaces
			     (match-string 1 type))
		    type (w3m-remove-redundant-spaces
			  (substring type 0 (match-beginning 0))))
	    (setq type (w3m-remove-redundant-spaces type))
	    (when (string-match ";$" type)
	      (setq type (substring type 0 (match-beginning 0))))))
	(list (or type (w3m-local-content-type url))
	      (or charset
		  (when (setq charset (cdr (assoc "w3m-document-charset"
						  alist)))
		    (car (split-string charset))))
	      (let ((v (cdr (assoc "content-length" alist))))
		(and v (setq v (string-to-number v)) (> v 0) v))
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

(defun w3m-w3m-dump-head-source (url)
  (and (let ((w3m-current-url url))
	 (w3m-message "Reading %s..." url)
	 (prog1
	     (w3m-exec-process w3m-dump-head-source-command-argument url)
	   (w3m-message "Reading %s...done" url)))
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
		(w3m-message "Reading %s..." url)
		(prog1
		    (w3m-exec-process "-dump_source" url)
		  (w3m-message "Reading %s...done" url)))
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

(defun w3m-w3m-retrieve (url &optional no-decode no-cache post-data referer)
  "Retrieve content of URL with w3m and insert it to the working buffer.
This function will return content-type of URL as string when retrieval
succeed.  If NO-DECODE, set the multibyte flag of the working buffer
to nil."
  (w3m-with-work-buffer
    (delete-region (point-min) (point-max))
    (set-buffer-multibyte nil)
    (let ((w3m-command-arguments w3m-command-arguments)
	  (coding-system-for-write 'binary)
	  type file modes)
      (and no-cache
	   w3m-broken-proxy-cache
	   (setq w3m-command-arguments
		 (append w3m-command-arguments '("-o" "no_cache=1"))))
      (when post-data
	(setq file (make-temp-name
		    (expand-file-name "w3mel" w3m-profile-directory)))
	(setq modes (default-file-modes))
	(with-temp-buffer
	  (insert (if (consp post-data) (cdr post-data) post-data))
	  (unwind-protect
	      (progn
		(set-default-file-modes (* 64 6))
		(write-region (point-min) (point-max) file nil 'silent))
	    (set-default-file-modes modes)))
	(setq w3m-command-arguments
	      (append w3m-command-arguments
		      (if (consp post-data)
			  (list "-header" (concat "Content-Type: "
						  (car post-data))))
		      (list "-post" file))))
      (when referer
	(setq w3m-command-arguments
	      (append w3m-command-arguments
		      (list "-header" (concat "Referer: " referer)))))
      (unwind-protect
	  (setq type
		(or (unless no-cache
		      (and (w3m-cache-request-contents url)
			   (w3m-content-type url)))
		    (car (if (memq w3m-type '(w3m-mnc w3mmee))
			     (w3m-w3m-dump-head-source url)
			   (w3m-w3m-dump-source url)))))
	(if file (delete-file file)))
      (when type
	(or no-decode
	    (w3m-decode-encoded-contents (w3m-content-encoding url))
	    (error "Can't decode encoded contents: %s" url))
	type))))

(defsubst w3m-url-local-p (url)
  "If URL points a file on the local system, return non-nil value.  Otherwise return nil."
  (string-match "^\\(file:\\|/\\)" url))

(defsubst w3m-url-dtree-p (url)
  "If URL points a 'w3m-dtree', return non-nil value.  Otherwise return nil."
  (string-match "^about://dtree/" url))

(defsubst w3m-about-retrieve (url &optional no-decode no-cache)
  (if (string= "about://emacs-w3m.gif" url)
      (when (fboundp 'base64-decode-string)
	(w3m-with-work-buffer
	  (delete-region (point-min) (point-max))
	  (set-buffer-multibyte nil)
	  (insert (eval (list 'base64-decode-string
			      w3m-emacs-w3m-icon))))
	"image/gif")
    (w3m-with-work-buffer
      (delete-region (point-min) (point-max))
      (set-buffer-multibyte t)
      (prog1
	  (save-current-buffer
	    (let (func)
	      (if (and (string-match "^about://\\([^/]+\\)/" url)
		       (setq func
			     (intern-soft
			      (concat "w3m-about-" (match-string 1 url))))
		       (fboundp func))
		  (funcall func url no-decode no-cache)
		(w3m-about url no-decode no-cache))))
	(encode-coding-region (point-min)
			      (point-max)
			      w3m-coding-system)
	(set-buffer-multibyte nil)))))

(defsubst w3m-cid-retrieve (url &optional no-decode no-cache)
  (let ((func
	 (cdr (assq major-mode w3m-cid-retrieve-function-alist))))
    (when func
      (funcall func url no-decode no-cache))))

(defun w3m-retrieve (url &optional no-decode no-cache post-data referer)
  "Retrieve content of URL and insert it to the working buffer.
This function will return content-type of URL as string when retrieval
succeed.  If NO-DECODE, set the multibyte flag of the working buffer
to nil.
"
  (when (string-match "#[^#]+$" url)
    (setq url (substring url 0 (match-beginning 0))))
  (let ((v (cond
	    ((string-match "^about:" url)
	     (w3m-about-retrieve url no-decode no-cache))
	    ((string-match "^cid:" url)
	     (w3m-cid-retrieve url no-decode no-cache))
	    ((w3m-url-local-p url)
	     (w3m-local-retrieve url no-decode))
	    (t
	     (w3m-w3m-retrieve url no-decode no-cache post-data referer)))))
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
		  (y-or-n-p (format "File(%s) is already exists. Overwrite? "
				    filename)))
	      (write-region (point-min) (point-max) filename))))
    (error "Cannot retrieve URL: %s%s"
	   url
	   (if w3m-process-exit-status
	       (format " (exit status: %s)" w3m-process-exit-status)
	     ""))))


;;; Retrieve data:
(unless (get 'w3m-euc-japan-decoder 'ccl-program-idx)
  (define-ccl-program w3m-euc-japan-decoder
    `(2
      (loop
       (read r0)
       ;; Process normal EUC characters.
       (if (r0 < ?\x80)
	   (write-repeat r0))
       (if (r0 > ?\xa0)
	   ((read r1)
	    (write ,(w3m-static-if (boundp 'MULE)
			lc-jp
		      (charset-id 'japanese-jisx0208)))
	    (write r0)
	    (write-repeat r1)))
       (if (r0 == ?\x8e)
	   ((read r0)
	    (write ,(w3m-static-if (boundp 'MULE)
			lc-kana
		      (charset-id 'katakana-jisx0201)))
	    (write-repeat r0)))
       (if (r0 == ?\x8f)
	   ((read r0)
	    (read r1)
	    (write ,(w3m-static-if (boundp 'MULE)
			lc-jp2
		      (charset-id 'japanese-jisx0212)))
	    (write r0)
	    (write-repeat r1)))
       ;; Process internal characters used in w3m.
       (if (r0 == ?\x80)	     ; Old ANSP (w3m-0.1.11pre+kokb23)
	   (write-repeat 32))
       (if (r0 == ?\x90)		; ANSP (use for empty anchor)
	   (write-repeat 32))
       (if (r0 == ?\x91)		; IMSP (blank around image)
	   (write-repeat 32))
       (if (r0 == ?\xa0)		; NBSP (non breakble space)
	   (write-repeat 32))
       (write-repeat r0)))))

(define-ccl-program w3m-euc-japan-encoder
  `(1 (loop (read r0) (write-repeat r0))))

(unless (w3m-static-if (featurep 'xemacs)
	    (find-coding-system 'w3m-euc-japan)
	  (coding-system-p 'w3m-euc-japan))
  (let ((fn
	 (if (and (not (featurep 'xemacs))
		  (>= emacs-major-version 20))
	     (progn
	       (require 'ccl)
	       (lambda (cs ch doc dec enc)
		 (make-coding-system cs 4 ch doc (cons dec enc))))
	   (require 'pccl)
	   'make-ccl-coding-system)))
    (funcall fn 'w3m-euc-japan ?E
	     "ISO 2022 based EUC encoding for Japanese with w3m internal characters.
  (generated by `w3m')"
	     'w3m-euc-japan-decoder
	     'w3m-euc-japan-encoder)))

(defun w3m-remove-comments ()
  "Remove HTML comments in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (beg)
      (while (search-forward "<!--" nil t)
	(setq beg (match-beginning 0))
	(if (search-forward "-->" nil t)
	    (delete-region beg (point)))))))

(defun w3m-rendering-region (start end &optional charset)
  "Do rendering of contents in this buffer as HTML and return title."
  (save-restriction
    (narrow-to-region start end)
    (set-buffer-multibyte t)
    (w3m-remove-comments)
    (when w3m-use-form
      (w3m-form-parse-region (point-min) (point-max) charset))
    (w3m-message "Rendering...")
    (let ((coding-system-for-read w3m-output-coding-system)
	  (coding-system-for-write w3m-input-coding-system)
	  (default-process-coding-system
	    (cons w3m-output-coding-system w3m-input-coding-system))
	  (process-environment process-environment))
      (dolist (elem w3m-process-environment)
	(setenv (car elem) (cdr elem)))
      (apply 'call-process-region
	     (point-min)
	     (point-max)
	     (or w3m-halfdump-command w3m-command)
	     t t nil
	     (delq nil
		   (mapcar
		    (lambda (x)
		      (cond
		       ((stringp x) x)
		       ((setq x (eval x))
			(if (stringp x)
			    x
			  (let (print-level print-length)
			    (prin1-to-string x))))))
		    (append w3m-halfdump-command-arguments
			    w3m-halfdump-command-common-arguments)))))
    (w3m-message "Rendering...done")
    (goto-char (point-min))
    ;; FIXME: Adhoc support for w3m with patch in [w3m-dev 01876].
    (and (looking-at "<!DOCTYPE w3mhalfdump public")
	 (search-forward "\n<pre>\n" nil t)
	 (delete-region (point-min) (1+ (match-beginning 0))))
    (goto-char (point-min))
    (let (title)
      (dolist (regexp '("<title_alt[ \t\n]+title=\"\\([^\"]+\\)\">"
			"<title>\\([^<]\\)</title>"))
	(goto-char (point-min))
	(when (re-search-forward regexp nil t)
	  (setq title (match-string 1))
	  (delete-region (match-beginning 0) (match-end 0))
	  (with-temp-buffer
	      (insert title)
	      (w3m-decode-entities)
	      (setq title (buffer-string)))))
      (if (and (null title)
	       (stringp w3m-current-url)
	       (< 0 (length (file-name-nondirectory w3m-current-url))))
	  (setq title (file-name-nondirectory w3m-current-url)))
      (or title "<no-title>"))))

(defun w3m-exec (url &optional buffer no-cache content-charset
		     content-type post-data referer)
  "Download URL with w3m to the BUFFER.
If BUFFER is nil, all data is placed to the current buffer.  When new
content is retrieved and half-dumped data is placed in the BUFFER,
this function returns t.  Otherwise, returns nil."
  (save-excursion
    (when buffer (set-buffer buffer))
    (let ((type (w3m-retrieve url nil no-cache post-data referer)))
      (if type
	  (progn
	    (when content-type (setq type content-type))
	    (cond
	     ((string-match "^text/" type)
	      (let (buffer-read-only)
		(setq w3m-current-url (w3m-real-url url)
		      w3m-current-title
		      (w3m-with-work-buffer
			(if (string= "text/html" type)
			    (progn
			      (unless (memq w3m-type '(w3mmee w3m-m17n))
				(w3m-decode-buffer url content-charset type))
			      (w3m-rendering-region (point-min) (point-max)
						    content-charset))
			  (w3m-decode-buffer url content-charset type)
			  (file-name-nondirectory url))))
		(delete-region (point-min) (point-max))
		(insert-buffer w3m-work-buffer-name)
		(when (string= "text/html" type) (w3m-fontify))
		t))
	     ((and (w3m-image-type-available-p (w3m-image-type type))
		   (string-match "^image/" type))
	      (let (buffer-read-only)
		(setq w3m-current-url (w3m-real-url url)
		      w3m-current-title (file-name-nondirectory url))
		(delete-region (point-min) (point-max))
		(insert w3m-current-title)
		(w3m-add-text-properties (point-min) (point-max)
					 (list 'face 'w3m-image-face
					       'w3m-image url
					       'mouse-face 'highlight))
		t))
	     (t (w3m-external-view url no-cache)
		nil)))
	(error "Cannot retrieve URL: %s%s"
	       url
	       (if w3m-process-exit-status
		   (format " (exit status: %s)" w3m-process-exit-status)
		 ""))))))


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
	(message "No such anchor: %s" name))
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
  "Convert URL to absolute, and canonicalize it."
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
  "View the URL of the link under point."
  (interactive "P")
  (let ((url (w3m-anchor)) (act (w3m-action)))
    (cond
     (url (w3m-goto-url url arg nil nil w3m-current-url))
     (act (eval act))
     (t (message "No URL at point")))))

(defun w3m-mouse-view-this-url (event)
  (interactive "e")
  (mouse-set-point event)
  (let ((url (w3m-anchor))
	(act (w3m-action))
	(img (w3m-image)))
    (cond
     (url (w3m-view-this-url))
     (act (eval act))
     (img (w3m-view-image))
     (t (message "No URL at point")))))

(defun w3m-submit-form ()
  "Submit form at point."
  (interactive)
  (let ((submit (w3m-submit)))
    (if submit
	(eval submit)
      (message "Can't Submit at this point"))))

(defun w3m-external-view (url &optional no-cache)
  (let* ((type (w3m-content-type url))
	 (method (nth 2 (assoc type w3m-content-type-alist))))
    (cond
     ((not method)
      (if (w3m-url-local-p url)
	  (error "No method to view `%s' is registered. Use `w3m-edit-this-url'"
		 (file-name-nondirectory (w3m-url-to-file-name url)))
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
  "View the image under point."
  (interactive)
  (let ((url (w3m-image)))
    (if url
	(w3m-external-view url)
      (message "No image at point"))))

(defun w3m-save-image ()
  "Save the image under point to a file."
  (interactive)
  (let ((url (w3m-image)))
    (if url
	(w3m-download url)
      (message "No image at point"))))

(defun w3m-view-current-url-with-external-browser ()
  "View this URL."
  (interactive)
  (let ((url (w3m-anchor)))
    (or url
	(and (y-or-n-p (format "Browse <%s> ? " w3m-current-url))
	     (setq url w3m-current-url)))
    (when url
      (message "Browsing <%s>..." url)
      (w3m-external-view url))))

(defun w3m-download-this-url ()
  "Download the URL of the link under point to a file."
  (interactive)
  (let ((url (w3m-anchor)))
    (if url
	(progn
	  (w3m-download url)
	  (w3m-refontify-anchor (current-buffer)))
      (message "No URL at point"))))

(defun w3m-print-current-url ()
  "Print the URL of current page and push it into kill-ring."
  (interactive)
  (kill-new w3m-current-url)
  (message "%s" w3m-current-url))

(defun w3m-print-this-url (&optional add-kill-ring)
  "Print the URL of the link under point."
  (interactive (list t))
  (let ((url (w3m-anchor)))
    (and add-kill-ring url (kill-new url))
    (message "%s" (or url
		      (and (w3m-action) "There's a form")
		      "There's no url"))))

(defun w3m-edit-current-url ()
  "Edit the local file pointed by the URL of current page"
  (interactive)
  (if (or (w3m-url-local-p w3m-current-url)
	  (w3m-url-dtree-p w3m-current-url))
      (funcall w3m-edit-function (w3m-url-to-file-name w3m-current-url))
    (error "The URL of current page is not local")))

(defun w3m-edit-this-url (&optional url)
  "Edit the local file by the under point."
  (interactive)
  (setq url (or url (w3m-anchor)))
  (if (null url)
      (message "No URL at point")
    (if (or (w3m-url-local-p url)
	    (w3m-url-dtree-p url))
	(funcall w3m-edit-function (w3m-url-to-file-name url))
      (error "URL:%s is not a local file" url))))

(defvar w3m-goto-anchor-hist nil)
(make-variable-buffer-local 'w3m-goto-anchor-hist)

(defun w3m-goto-next-anchor ()
  ;; move to the end of the current anchor
  (when (w3m-cursor-anchor)
    (goto-char (next-single-property-change (point) 'w3m-cursor-anchor)))
  ;; find the next anchor
  (or (w3m-cursor-anchor)
      (let ((pos (next-single-property-change (point) 'w3m-cursor-anchor)))
	(when pos
	  (goto-char pos)
	  t))))

(defun w3m-next-anchor (&optional arg)
  "Move cursor to the next anchor."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (null (memq last-command '(w3m-next-anchor w3m-previous-anchor)))
      (setq w3m-goto-anchor-hist
	    (list (get-text-property (point) 'w3m-cursor-anchor)))
    (if (eq last-command 'w3m-previous-anchor)
	(setq w3m-goto-anchor-hist (list (car w3m-goto-anchor-hist)))))
  (if (< arg 0)
      (w3m-previous-anchor (- arg))
    (while (> arg 0)
      (unless (w3m-goto-next-anchor)
	;; search from the beginning of the buffer
	(setq w3m-goto-anchor-hist nil)
	(goto-char (point-min))
	(w3m-goto-next-anchor))
      (setq arg (1- arg))
      (if (member (w3m-cursor-anchor) w3m-goto-anchor-hist)
	  (setq arg (1+ arg))
	(setq w3m-goto-anchor-hist
	      (cons (get-text-property (point) 'w3m-cursor-anchor)
		    w3m-goto-anchor-hist))))
    (w3m-print-this-url)))

(defun w3m-goto-previous-anchor ()
  ;; move to the beginning of the current anchor
  (when (w3m-cursor-anchor)
    (goto-char (previous-single-property-change (1+ (point))
						'w3m-cursor-anchor)))
  ;; find the previous anchor
  (let ((pos (previous-single-property-change (point) 'w3m-cursor-anchor)))
    (if pos
	(goto-char
	 (if (w3m-cursor-anchor pos) pos
	   (previous-single-property-change pos 'w3m-cursor-anchor))))))

(defun w3m-previous-anchor (&optional arg)
  "Move cursor to the previous anchor."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (null (memq last-command '(w3m-next-anchor w3m-previous-anchor)))
      (setq w3m-goto-anchor-hist
	    (list (get-text-property (point) 'w3m-cursor-anchor)))
    (if (eq last-command 'w3m-next-anchor)
	(setq w3m-goto-anchor-hist (list (car w3m-goto-anchor-hist)))))
  (if (< arg 0)
      (w3m-next-anchor (- arg))
    (while (> arg 0)
      (unless (w3m-goto-previous-anchor)
	;; search from the end of the buffer
	(setq w3m-goto-anchor-hist nil)
	(goto-char (point-max))
	(w3m-goto-previous-anchor))
      (setq arg (1- arg))
      (if (member (w3m-cursor-anchor) w3m-goto-anchor-hist)
	  (setq arg (1+ arg))
	(setq w3m-goto-anchor-hist
	      (cons (get-text-property (point) 'w3m-cursor-anchor)
		    w3m-goto-anchor-hist))))
    (w3m-print-this-url)))


(defun w3m-copy-buffer (buf &optional newname and-pop) "\
Create a twin copy of the current buffer.
If NEWNAME is nil, it defaults to the current buffer's name.
If AND-POP is non-nil, the new buffer is shown with `pop-to-buffer',
that is affected by `w3m-pop-up-frames'."
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
	(when and-pop
	  (let* ((pop-up-frames w3m-pop-up-frames)
		 (pop-up-frame-alist (w3m-pop-up-frame-parameters))
		 (pop-up-frame-plist pop-up-frame-alist)
		 (oframe (selected-frame))
		 nframe)
	    (pop-to-buffer new)
	    (unless (eq oframe (setq nframe (selected-frame)))
	      (setq w3m-initial-frame (selected-frame)))))
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
    (define-key map "C" 'w3m-redisplay-with-charset)
    (define-key map "?" 'describe-mode)
    (define-key map "\M-a" 'w3m-bookmark-add-this-url)
    (define-key map "a" 'w3m-bookmark-add-current-url)
    (define-key map "H" 'w3m-gohome)
    (define-key map "A" 'w3m-antenna)
    (define-key map "W" 'w3m-weather)
    (define-key map "S" 'w3m-search)
    (define-key map "D" 'w3m-dtree)
    (define-key map "N" 'w3m-namazu)
    (define-key map ">" 'w3m-scroll-left)
    (define-key map "<" 'w3m-scroll-right)
    (define-key map "\\" 'w3m-view-source)
    (define-key map "=" 'w3m-view-header)
    (define-key map "s" 'w3m-history)
    (define-key map "E" 'w3m-edit-current-url)
    (define-key map "e" 'w3m-edit-this-url)
    (define-key map "\C-c\C-c" 'w3m-submit-form)
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
    (define-key map "A" 'w3m-antenna)
    (define-key map "b" 'scroll-down)
    (define-key map "C" 'w3m-redisplay-with-charset)
    (define-key map "d" 'w3m-bookmark-view)
    (define-key map "D" 'w3m-download-this-url)
    (define-key map "e" 'w3m-edit-current-url)
    (define-key map "E" 'w3m-edit-this-url)
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
    (define-key map "T" 'w3m-dtree)
    (define-key map "N" 'w3m-namazu)
    (define-key map "p" 'w3m-view-previous-page)
    (define-key map "q" 'w3m-close-window)
    (define-key map "Q" 'w3m-quit)
    (define-key map "R" 'w3m-reload-this-page)
    (define-key map "s" 'w3m-search)
    (define-key map "t" 'w3m-gohome)
    (define-key map "u" 'w3m-view-parent-page)
    (define-key map "v" 'w3m-bookmark-view)
    (define-key map "W" 'w3m-weather)
    (define-key map "y" 'w3m-print-this-url)
    (define-key map "=" 'w3m-view-header)
    (define-key map "\\" 'w3m-view-source)
    (define-key map "?" 'describe-mode)
    (define-key map ">" 'w3m-scroll-left)
    (define-key map "<" 'w3m-scroll-right)
    (define-key map "." 'beginning-of-buffer)
    (define-key map "\C-c\C-c" 'w3m-submit-form)
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

(defun w3m-delete-frame-maybe ()
  "Delete this frame if it has popped up as w3m frame in the beginning.
Even so, if there are other windows, it won't delete the frame.
Return t if deleting current frame or window is succeeded."
  (let ((frame (selected-frame))
	(window (selected-window)))
    (cond ((eq w3m-initial-frame frame)
	   (if (eq (next-window) window)
	       (delete-frame frame)
	     (delete-window window))
	   t)
	  ((and (frame-live-p w3m-initial-frame)
		(eq (window-buffer
		     (setq window (frame-first-window w3m-initial-frame)))
		    (current-buffer)))
	   (if (eq (next-window window) window)
	       (delete-frame w3m-initial-frame)
	     (delete-window window))
	   nil))))

(defun w3m-quit (&optional force)
  "Quit browsing WWW after updating arrived URLs list."
  (interactive "P")
  (when (or force
	    (prog1
		(y-or-n-p "Do you want to exit w3m? ")
	      (message "")))
    (let ((buffer (current-buffer)))
      (w3m-delete-frame-maybe)
      (kill-buffer buffer))
    (unless (w3m-alive-p)
      ;; If no w3m is running, then destruct all data.
      (w3m-cache-shutdown)
      (w3m-arrived-shutdown)
      (remove-hook 'kill-emacs-hook 'w3m-arrived-shutdown)
      (w3m-kill-all-buffer))))

(defun w3m-close-window ()
  "Close this window and make the other buffer current."
  (interactive)
  (unless (let ((buffer (current-buffer)))
	    (prog1
		(w3m-delete-frame-maybe)
	      (bury-buffer (current-buffer))))
    (set-window-buffer (selected-window) (other-buffer))))

(unless w3m-mode-map
  (setq w3m-mode-map
	(if (eq w3m-key-binding 'info)
	    w3m-info-like-map
	  w3m-lynx-like-map)))

(defun w3m-mode ()
  "\\<w3m-mode-map>
   Major mode to browsing w3m buffer.

\\[w3m-view-this-url]	View this url.
\\[w3m-mouse-view-this-url]	View this url use mouse.
	If w3m-use-form is t, \\[w3m-view-this-url] and \\[w3m-mouse-view-this-url] action a form input.
\\[w3m-submit-form]	Submit the form at point.

\\[w3m-reload-this-page]	Reload this page.
\\[w3m-redisplay-with-charset]	Redisplay this page with specified coding-system.

\\[w3m-next-anchor]	Jump to next anchor.
\\[w3m-previous-anchor]	Jump to previous anchor.
\\[w3m-view-previous-page]	Back to previous page.
\\[w3m-view-next-page]	Forward to next page.
\\[w3m-view-parent-page]	Upward to parent page.

\\[w3m-goto-url]	Goto URL.
\\[w3m-gohome]	Goto home page.

\\[w3m-download-this-url]	Download this url.
\\[w3m-print-this-url]	Print this url.
\\[w3m-view-image]	View image.
\\[w3m-save-image]	Save image.

\\[w3m-print-current-url]	Print current url.
\\[w3m-view-current-url-with-external-browser]	View current url with external browser.
\\[w3m-view-source]	Display source of this current buffer.
\\[w3m-view-header]	Display header of this current buffer.
\\[w3m-edit-current-url]	Edit the local file pointed by the URL of current page.
\\[w3m-edit-this-url]	Edit the local file by the under the point.

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

\\[w3m-history]	Display arrived history.
	If called with '\\[universal-argument]', this command displays arrived-DB history.
\\[w3m-antenna]	Display the report change of WEB sites.
	If called with '\\[universal-argument]', this command reloads report.
\\[w3m-search]	Search query.
	If called with '\\[universal-argument]', you can choose search engine.
\\[w3m-weather]	Display weather report.
	If called with '\\[universal-argument]', you can choose local area.
\\[w3m-dtree]	Display directory tree.
	If called with '\\[universal-argument]', view all directories and files.
\\[w3m-namazu]	Search files with Namazu.
	If called with '\\[universal-argument]', you can choose index.

\\[w3m-bookmark-view]	w3m-bookmark-view.
\\[w3m-bookmark-add-current-url]	Add link of current page to bookmark.
	If called with  '\\[universal-argument]', ask new url to add instead of current page.
\\[w3m-bookmark-add-this-url]	Add link under cursor to bookmark.

\\[w3m-copy-buffer]	Create a twin copy of the current buffer.

\\[w3m]	w3m.
\\[w3m-close-window]	Close this window and make the other buffer current.
\\[w3m-quit]	Quit browsing WWW after updating arrived URLs list.

\\[describe-mode]	describe-mode.
"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'w3m-mode)
  (setq mode-name "w3m")
  (use-local-map w3m-mode-map)
  (setq truncate-lines t)
  (w3m-setup-toolbar)
  (w3m-setup-menu)
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
	  (error "You must specify valid `mail-user-agent'"))
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
      (error "URL is strange")))

(defun w3m-goto-ftp-url (url)
  (let* ((ftp (w3m-convert-ftp-url-for-emacsen url))
	 (file (file-name-nondirectory ftp)))
    (if (file-directory-p ftp)
	(dired-other-window ftp)
      (copy-file ftp (w3m-read-file-name nil nil file)))))

;;;###autoload
(defun w3m-goto-url (url &optional reload charset post-data referer)
  "Retrieve contents of URL.
If second argument RELOAD is non-nil, content is re-loaded.
Third argument CHARSET specifies content charset.
Fourth argument POST-DATA should be a string or a cons cell.
If string is specified, it is used as request body and content-type is
set as \"x-www-form-urlencoded\".
If cons cell is specified, car of the cell is used as content-type and
cdr of the cell is used as body.
If fifth argument REFERER is specified, it is used as Referer: field of
the request."
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
    (setq w3m-current-buffer (current-buffer))
    ;; Setup arrived database.
    (w3m-arrived-setup)
    ;; Store the current position point in the history structure.
    (w3m-history-store-position)
    ;; Retrieve.
    (let ((orig url) name localpath localcgi)
      ;; local directory URL check
      (if (and (w3m-url-local-p url)
	       (file-directory-p (w3m-url-to-file-name url))
	       (setq url (file-name-as-directory url)))
	  (if (and (eq w3m-local-directory-view-method 'w3m-dtree)
		   (string-match "^file:///" url))
	      (setq url (replace-match "about://dtree/" nil nil url))
	    (setq localcgi t)))
      (when (string-match "#\\([^#]+\\)$" url)
	(setq name (match-string 1 url)
	      url (substring url 0 (match-beginning 0))))
      (let ((ct (w3m-arrived-content-type url))
	    (cs (or charset (w3m-arrived-content-charset url))))
	(if ct
	    (when reload
	      (let* ((minibuffer-setup-hook
		      (append minibuffer-setup-hook '(beginning-of-line)))
		     (s (completing-read
			 (format "Input %s's content type: "
				 (file-name-nondirectory url))
			 w3m-content-type-alist nil t ct)))
		(setq ct (if (string= "" s) nil s))))
	  (and (w3m-url-local-p url)
	       (string= "unknown" (w3m-local-content-type url))
	       (let ((s (completing-read
			 (format "Input %s's content type (default %s): "
				 (file-name-nondirectory url)
				 w3m-default-content-type)
			 w3m-content-type-alist nil t)))
		 (setq ct (if (string= "" s) w3m-default-content-type s)))))
	(if (not (w3m-exec url nil reload cs ct post-data
			   (if w3m-add-referer referer nil)))
	    (w3m-refontify-anchor)
	  (w3m-history-push w3m-current-url
			    (list ':title w3m-current-title))
	  (or (and name (w3m-search-name-anchor name))
	      (goto-char (point-min)))
	  (setq w3m-display-inline-image-status 'off)
	  (when (w3m-display-inline-image-p)
	    (and w3m-force-redisplay (sit-for 0))
	    (w3m-toggle-inline-images 'force reload))
	  (setq buffer-read-only t)
	  (set-buffer-modified-p nil))
	(w3m-arrived-add orig w3m-current-title nil nil cs ct))
      (setq localpath (and (or (w3m-url-local-p url)
			       (w3m-url-dtree-p url))
			   (w3m-url-to-file-name url)))
      (setq default-directory
	    (file-name-as-directory
	     (if (and localpath (file-exists-p localpath))
		 (if (file-directory-p localpath)
		     localpath
		   (file-name-directory localpath))
	       w3m-profile-directory)))
      (w3m-update-toolbar)
      (switch-to-buffer (current-buffer))
      (when localcgi (w3m-goto-url-localcgi-movepoint))))))

(defun w3m-goto-url-localcgi-movepoint ()
  (let ((height (/ (window-height) 5))
	(pos (point-min)))
    (when (= (point-min) (point))
      (goto-char
       (catch 'detect
	 (while (and (not (eobp))
		     (setq pos (next-single-property-change
				pos 'w3m-name-anchor)))
	   (when (equal (get-text-property pos 'w3m-name-anchor) "current")
	     (throw 'detect pos)))
	 (point-min)))
      (recenter height))))

(defun w3m-gohome ()
  "Go to the Home page."
  (interactive)
  (unless w3m-home-page
    (error "You have to specify the value of `w3m-home-page'"))
  (w3m-goto-url w3m-home-page))

(defun w3m-reload-this-page (&optional arg)
  "Reload current page without cache."
  (interactive "P")
  (let ((w3m-display-inline-image (if arg t w3m-display-inline-image)))
    (w3m-goto-url w3m-current-url 'reload)))

;; FIXME: 現在はどの文字コードとして解釈されているのか表示して欲しい
(defun w3m-redisplay-with-charset (&optional arg)
  "Redisplay current page with specified coding-system.
If input is nil, use default coding-system on w3m."
  (interactive "P")
  (let ((w3m-display-inline-image (if arg t w3m-display-inline-image))
	(default (w3m-content-charset w3m-current-url)))
    (w3m-goto-url w3m-current-url nil
		  (w3m-read-content-charset
		   (if default
		       (format "Content-charset (default %s): " default)
		     "Content-charset: ")
		   default))))


;;;###autoload
(defun w3m (&optional url)
  "Visit the World Wide Web page using the external command w3m or w3mmee.
If you invoke this command interactively for the first time, it will
prompt you for the URL where you wish to go, otherwise it will pop up
an existing window or frame.  In addition, you can run this command in
the batch mode like \"emacs -f w3m URL&\".  URL should be a string
which defaults to the value of `w3m-home-page' or \"about:\".  The
value of `w3m-pop-up-frames' specifies whether to pop up a new frame,
however, it will be ignored (treated as nil) when this command is
called non-interactively."
  (interactive (list (or (w3m-alive-p)
			 (w3m-input-url))))
  (unless url
    ;; It may be called non-interactively.
    (setq url (or (when (= 1 (length command-line-args-left))
		    (pop command-line-args-left))
		  w3m-home-page
		  "about:")))
  (let ((focusing-function
	 (append '(lambda (frame)
		    (raise-frame frame)
		    (select-frame frame))
		 ;; `focus-frame' might not work on some environments.
		 (if (fboundp 'x-focus-frame)
		     '((x-focus-frame frame))
		   '((focus-frame frame)))))
	(params (w3m-pop-up-frame-parameters))
	(popup-frame-p (w3m-popup-frame-p))
	window frame)
    (if (bufferp url)
	(progn
	  (when (setq window (get-buffer-window url t))
	    (setq frame (window-frame window)))
	  (cond (frame
		 (funcall focusing-function frame)
		 (select-window window)
		 (setq frame nil))
		(window
		 (select-window window))
		(t
		 (when popup-frame-p
		   (funcall focusing-function
			    (setq frame (make-frame params))))
		 (switch-to-buffer url))))
      (when popup-frame-p
	(funcall focusing-function (setq frame (make-frame params))))
      (w3m-goto-url url))
    (when frame
      (setq w3m-initial-frame frame))))

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
    (when (memq w3m-type '(w3mmee w3m-m17n))
      (encode-coding-region (point-min) (point-max) w3m-coding-system))
    (setq w3m-current-buffer (current-buffer)
	  w3m-current-title
	  (w3m-rendering-region (point-min) (point-max)))
    (w3m-fontify)
    (setq w3m-display-inline-image-status 'off)
    (when (w3m-display-inline-image-p)
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
Welcome to <a href=\"http://emacs-w3m.namazu.org/\">")
    (insert "<img src=\"about://emacs-w3m.gif\" alt=\"emacs-w3m\">")
    (insert "</a>!
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
    (let ((type (w3m-retrieve (setq url (substring url (match-end 0)))
			      no-decode no-cache)))
      (w3m-decode-buffer url)
      (and type "text/plain"))))

(defun w3m-view-source ()
  "Display source of this current buffer."
  (interactive)
  (if (string-match "^about://header/" w3m-current-url)
      (message "Can't load a source for %s" w3m-current-url)
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
  "Display header of this current buffer."
  (interactive)
  (if (or (and (string-match "^about:" w3m-current-url)
	       (not (string-match "^about://header/" w3m-current-url)))
	  (string-match "^file://" w3m-current-url))
      (message "Can't load a header for %s" w3m-current-url)
    (w3m-goto-url (if (string-match "^about://header/" w3m-current-url)
		      (substring w3m-current-url (match-end 0))
		    (concat "about://header/" w3m-current-url)))))

(defvar w3m-about-history-max-indentation '(/ (* (window-width) 2) 3)
  "*Number to limit the indentation when showing a tree-structured
history by the command `w3m-about-history'.  This value is evaluated
for each run-time, so it can be any `s' expressions.")

(defvar w3m-about-history-indent-level 4
  "*Number that says how much each sub-tree should be indented when
showing a tree-structured history by the command `w3m-about-history'.")

(defun w3m-about-history (&rest args)
  "Show a tree-structured history."
  (let (history current start)
    (with-current-buffer w3m-current-buffer
      (setq history w3m-history-flat
	    current w3m-current-url))
    (insert "\
<head><title>URL history</title></head><body>
<h1>List of all the links you have visited in this session.</h1><pre>\n")
    (setq start (point))
    (when history
      (let ((form
	     (format
	      "%%0%dd"
	      (length
	       (number-to-string
		(apply 'max
		       (apply 'append
			      (mapcar
			       (function
			     ;; Don't use `caddr' here, since it won't
			      ;; be substituted by the compiler macro.
				(lambda (e)
				  (car (cdr (cdr e)))))
			       history)))))))
	    (cur (current-buffer))
	    (margin (if (> w3m-about-history-indent-level 1)
			1
		      0))
	    (max-indent (condition-case nil
			    ;; Force the value to be a number or nil.
			    (+ 0 (eval w3m-about-history-max-indentation))
			  (error nil)))
	    (last-indent -1)
	    (sub-indent 0)
	    element url about title position bol indent)
	(while history
	  (setq element (pop history)
		url (car element)
		about (string-match w3m-history-ignored-regexp url)
		title (plist-get (cadr element) ':title)
		position (caddr element))
	  (insert (format "h%s %d <a href=\"%s\">%s%s%s</a>\n"
			  (mapconcat (function (lambda (d) (format form d)))
				     position
				     "-")
			  (/ (1- (length position)) 2)
			  url
			  (if about "&lt;" "")
			  (if (or (not title)
				  (string-equal "<no-title>" title)
				  (string-match "^[\t 　]*$" title))
			      url
			    title)
			  (if about "&gt;" ""))))
	(sort-fields 0 start (point-max))
	(goto-char start)
	(while (not (eobp))
	  (setq bol (point))
	  (skip-chars-forward "^ ")
	  (setq indent (read cur)
		sub-indent (if (= indent last-indent)
			       (1+ sub-indent)
			     0)
		last-indent indent
		indent (+ (* w3m-about-history-indent-level indent)
			  sub-indent))
	  (delete-region bol (point))
	  (insert-char ?\  (+ margin (if max-indent
					 (min max-indent indent)
				       indent)))
	  (forward-line 1))))
    (insert "</pre></body>")
    (goto-char start)
    (when (and current
	       (re-search-forward (concat "\\(<a href=\""
					  (regexp-quote current)
					  "\">.+</a>\\)")
				  nil t))
      (beginning-of-line)
      (delete-char 1)
      (insert "&gt;")))
  "text/html")

(defun w3m-about-db-history (&rest args)
  (let ((width (- (if (< 0 w3m-fill-column)
		      w3m-fill-column
		    (+ (frame-width) (or w3m-fill-column -1)))
		  18))
	(today (format-time-string "%Y-%m-%d" (current-time)))
	url title time alist date)
    (when w3m-arrived-db
      (mapatoms
       (lambda (sym)
	 (when (and sym
		    (setq url (symbol-name sym))
		    (not (string-match w3m-history-ignored-regexp url)))
	   (setq time (w3m-arrived-time url))
	   (push (cons url time) alist)))
       w3m-arrived-db)
      (setq alist (sort alist
			(lambda (a b)
			  (w3m-time-newer-p (cdr a) (cdr b))))))
    (insert "<html><head><title>URL history in DataBase</title></head><body>\n")
    (insert "<h1>arrived URL history in DataBase</h1>\n")
    (if (null alist)
	(insert "<h2>Nothing in DataBase.</h2>\n")
      (insert "<table cellpadding=0>\n")
      (insert "<tr><td><h2> Title/URL </h2></td><td><h2>Time/Date</h2></td></tr>\n")
      (while alist
	(setq url (car (car alist)))
	(setq title (w3m-arrived-title url))
	(if (or (null title)
		(string= "<no-title>" title))
	    (setq title (concat
			 "&lt;"
			 (if (<= (length url) width)
			     url
			   (substring url 0 width)) ;; only ASCII characters.
			 "&gt;"))
	  (when (>= (string-width title) width)
	    (setq title
		  (concat
		   (with-temp-buffer
		     (insert title)
		     (move-to-column width)
		     (buffer-substring (point-min) (point)))
		   "..."))))
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
    (insert "</body></html>\n"))
  "text/html")

(defun w3m-history (&optional arg)
  "Display w3m history.
If called with 'prefix argument', display arrived-DB history."
  (interactive "P")
  (if (null arg)
      (w3m-goto-url "about://history/")
    (w3m-goto-url "about://db-history/")))

(defun w3m-db-history ()
  (interactive)
  (w3m-goto-url "about://db-history/"))

(defun w3m-w32-browser-with-fiber (url)
  (let ((proc (start-process "w3m-w32-browser-with-fiber"
			     (current-buffer)
			     "fiber.exe"
			     (w3m-url-to-file-name url))))
    (set-process-filter proc 'ignore)
    (set-process-sentinel proc 'ignore)))


;;; Header line (emulating Emacs 21).
(defcustom w3m-use-header-line t
  "*Non-nil activates header-line of w3m."
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
      (w3m-add-text-properties (point-min) (point)
			       '(face w3m-header-line-location-title-face))
      (let ((start (point)))
	(insert w3m-current-url)
	(w3m-add-text-properties start (point)
				 (list 'face
				       'w3m-header-line-location-content-face
				       'mouse-face 'highlight
				       'local-map w3m-header-line-map))
	(setq start (point))
	(insert-char ?\  (max 0 (- (window-width) (current-column) 1)))
	(w3m-add-text-properties start (point)
				 '(face w3m-header-line-location-content-face))
	(unless (eolp)
	  (insert "\n"))))))

(provide 'w3m)
;;; w3m.el ends here.

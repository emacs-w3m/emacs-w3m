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
;;    http://w3m.sourceforge.net/


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

(require 'w3m-util)
(require 'w3m-proc)

;; The following variables will be referred by the version specific
;; modules which bind such variables only when compiling themselves.
;; And some module(s) use `defadvice' which will do byte-compile at
;; the run-time.
(defvar w3m-current-title nil "Title of this buffer.")
(defvar w3m-current-url nil "URL of this buffer.")

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
(require 'timezone)
(require 'ccl)

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
  (autoload 'w3m-antenna-add-current-url "w3m-antenna"
    "Add link of current page to antenna." t)
  (autoload 'w3m-about-antenna "w3m-antenna")
  (autoload 'w3m-dtree "w3m-dtree"
    "Display directory tree." t)
  (autoload 'w3m-about-dtree "w3m-dtree")
  (autoload 'w3m-namazu "w3m-namazu"
    "Search files with Namazu." t)
  (autoload 'w3m-about-namazu "w3m-namazu")
  (autoload 'w3m-fontify-forms "w3m-form")
  (autoload 'w3m-form-parse-buffer "w3m-form")
  (autoload 'w3m-filter "w3m-filter")
  (autoload 'w3m-setup-tab-menu "w3m-tabmenu")
  (autoload 'w3m-switch-buffer "w3m-tabmenu"))

;; Avoid byte-compile warnings.
(eval-when-compile
  (autoload 'rfc2368-parse-mailto-url "rfc2368"))

(defconst emacs-w3m-version
  (eval-when-compile
    (let ((rev "$Revision$"))
      (and (string-match "\\.\\([0-9]+\\) \$$" rev)
	   (format "1.2.%d"
		   (- (string-to-number (match-string 1 rev)) 426)))))
  "Version number of this package.")

(defgroup w3m nil
  "w3m - the web browser of choice."
  :group 'hypermedia)

(defgroup w3m-face nil
  "Faces for w3m."
  :group 'w3m
  :prefix "w3m-")

(defcustom w3m-type
  (let ((command (if (boundp 'w3m-command)
		     (symbol-value 'w3m-command)
		   (or (w3m-which-command "w3m")
		       (w3m-which-command "w3mmee")
		       (w3m-which-command "w3m-m17n")))))
    (when command
      (setq w3m-command command)
      (with-temp-buffer
	(call-process command nil t nil "-version")
	(goto-char (point-min))
	(when (re-search-forward "version w3m/0\\.\\(2\\.1\\|\
\\(2\\.[2-9]\\(\\.[0-9]\\)*\\|3\\(\\.[0-9\\]\\)*\\)\\)\\(-inu\
\\|\\(-m17n\\|\\(\\+mee\\)\\)\\)?" nil t)
	  (cond
	   ((match-beginning 7) 'w3mmee)
	   ((match-beginning 6) 'w3m-m17n)
	   ((or (match-beginning 5) (match-beginning 2)) 'w3m))))))
  "*Type of w3m."
  :group 'w3m
  :type '(choice (const :tag "w3m" 'w3m)
		 (const :tag "w3mmee" 'w3mmee)
		 (const :tag "w3m-m17n" 'w3m-m17n)
		 (symbol :tag "other" nil)))

(defcustom w3m-language
  (if (or (and (boundp 'current-language-environment)
	       (string= "Japanese"
			(symbol-value 'current-language-environment)))
	  (boundp 'MULE))
      "Japanese")
  "*Language of w3m."
  :group 'w3m
  :type 'string)

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

(defcustom w3m-command-environment
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

(defcustom w3m-use-mule-ucs
  (and (eq w3m-type 'w3m) (featurep 'un-define))
  "*Non nil means using multi-script support with Mule-UCS."
  :group 'w3m
  :type 'boolean
  :require 'w3m-ucs)

(when w3m-use-mule-ucs
  (condition-case nil
      (require 'w3m-ucs)
    (error (setq w3m-use-mule-ucs nil))))

(defvar w3m-accept-japanese-characters
  (or (memq w3m-type '(w3mmee w3m-m17n))
      ;; Detect that the internal character set of `w3m-command' is EUC-JP.
      (let ((str
	     (eval-when-compile
	       (format
		(concat
		 "<!doctype html public \"-//W3C//DTD HTML 3.2//EN\">"
		 "<html><head><meta http-equiv=\"Content-Type\" "
		 "content=\"text/html; charset=ISO-2022-JP\">"
		 "</head><body>%s</body>\n")
		(string 27 36 66 52 65 59 122 27 40 66)))))
	(with-temp-buffer
	  (set-buffer-multibyte nil)
	  (insert str)
	  (let ((coding-system-for-write 'binary)
		(coding-system-for-read 'binary)
		(default-process-coding-system (cons 'binary 'binary)))
	    (call-process-region (point-min) (point-max) w3m-command
				 t t nil "-T" "text/html" "-halfdump")
	    (goto-char (point-min))
	    (skip-chars-forward "a-zA-Z<>/_ \n")
	    (string= (buffer-substring (point) (min (+ 4 (point)) (point-max)))
		     (string ?\264 ?\301 ?\273 ?\372))))))
  "Non-nil means that `w3m-command' accepts Japanese characters.")

(defcustom w3m-coding-system
  'iso-2022-7bit
  "*Basic coding system for `w3m'."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-terminal-coding-system
  (if w3m-accept-japanese-characters
      'euc-japan 'iso-8859-1)
  "*Coding system for keyboard input to `w3m'."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-input-coding-system
  (if (memq w3m-type '(w3mmee w3m-m17n))
      'binary
    (if w3m-use-mule-ucs
	(if w3m-accept-japanese-characters
	    'w3m-euc-japan 'w3m-iso-latin-1)
      (if w3m-accept-japanese-characters
	  'iso-2022-7bit 'iso-8859-1)))
  "*Coding system for write operations to `w3m'."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-output-coding-system
  (cond
   ((eq w3m-type 'w3mmee) 'ctext)
   ((eq w3m-type 'w3m-m17n) 'iso-2022-7bit-ss2)
   (w3m-accept-japanese-characters 'w3m-euc-japan)
   (t 'w3m-iso-latin-1))
  "*Coding system for read operations of `w3m'."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-file-coding-system
  'iso-2022-7bit
  "*Coding system for writing configuration files in `w3m'.
The value will be referred by the function `w3m-save-list'."
  :group 'w3m
  :type 'coding-system)

(defvar w3m-file-coding-system-for-read nil
  "*Coding system for reading configuration files in `w3m'.  It is strongly
recommended that you do not set this variable if there is no particular
reason.  The value will be referred by the function `w3m-load-list'.")

(defcustom w3m-file-name-coding-system
  (if (memq system-type '(windows-nt OS/2 emx))
      'shift_jis 'euc-japan)
  "*Coding system for encoding file names of `w3m'."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-default-coding-system
  (if (equal "Japanese" w3m-language) 'shift_jis 'iso-8859-1)
  "*Default coding system to encode URL strings and post-data."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-coding-system-priority-list
  (if (equal "Japanese" w3m-language) '(shift_jis))
  "*Priority for detect coding-system."
  :group 'w3m
  :type '(repeat coding-system))

(defcustom w3m-key-binding nil
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
  (concat "~/." (file-name-sans-extension
		 (file-name-nondirectory w3m-command)))
  "*Directory of w3m profiles."
  :group 'w3m
  :type 'directory)

(defcustom w3m-default-save-directory
  (concat "~/." (file-name-sans-extension
		 (file-name-nondirectory w3m-command)))
  "*Default directory for save file."
  :group 'w3m
  :type 'directory)

(defcustom w3m-accept-languages
  (let ((file (expand-file-name "config" w3m-profile-directory)))
    (or (when (file-readable-p file)
	  (with-temp-buffer
	    (insert-file-contents file)
	    (goto-char (point-min))
	    (when (re-search-forward "^accept_language[\t ]+\\(.+\\)$" nil t)
	      (delete "" (split-string (match-string 1))))))
	(when (string= w3m-language "Japanese")
	  '("ja" "en"))))
  "*Prioirity for acceptable languages."
  :group 'w3m
  :type '(repeat string))

(defcustom w3m-delete-duplicated-empty-lines t
  "*Compactize page by deleting duplicated empty lines."
  :group 'w3m
  :type 'boolean)

(defvar w3m-display-inline-images nil
  "Non-nil means images are displayed inline in the w3m buffer.
This variable is buffer-local which defaults to the value of
`w3m-default-display-inline-images'.
You should not set it directly; You can toggle the value of this variable by
using the command \\<w3m-mode-map>`\\[w3m-toggle-inline-images]'.")
(make-variable-buffer-local 'w3m-display-inline-images)

(defcustom w3m-default-display-inline-images nil
  "Default value of `w3m-display-inline-images' for buffers not overriding it."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-toggle-inline-images-permanently t
  "*If nil, apply the value of `w3m-default-display-inline-images' to
`w3m-display-inline-images' in the current w3m buffer when you visit a
new page for each time.  Otherwise, the value of
`w3m-display-inline-images' won't be changed."
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

(defcustom w3m-broken-proxy-cache nil
  "*If non nil, cache on proxy server is not used.
This feature should be enabled only if the caching configuration of
your proxy server is broken.  In order to use this feature, you must
apply the patch posted in [emacs-w3m:01119]."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-quick-start t
  "*This switch controls the action when `w3m' is interactively called.
When it is equal to the value other than nil, `w3m' displays no prompt
to input URL when URL-like string is not detected under the cursor."
  :group 'w3m
  :type 'boolean)

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
  "Face used to fontify anchors."
  :group 'w3m-face)

(defface w3m-arrived-anchor-face
  '((((class color) (background light)) (:foreground "navy" :underline t))
    (((class color) (background dark)) (:foreground "LightSkyBlue" :underline t))
    (t (:underline t)))
  "Face used to fontify anchors, if arrived."
  :group 'w3m-face)

(defface w3m-image-face
  '((((class color) (background light)) (:foreground "ForestGreen"))
    (((class color) (background dark)) (:foreground "PaleGreen"))
    (t (:underline t)))
  "Face used to fontify image alternate strings."
  :group 'w3m-face)

(defface w3m-history-current-url-face
  ;; Merge the face attributes of `base' into `w3m-arrived-anchor-face'.
  (let ((base 'secondary-selection)
	(fn (if (featurep 'xemacs)
		'face-custom-attributes-get
	      'custom-face-attributes-get));; What a perverseness it is.
	base-attributes attributes attribute value)
    (require 'wid-edit);; Needed for only Emacs 20.
    (setq base-attributes (funcall fn base nil)
	  attributes (funcall fn 'w3m-arrived-anchor-face nil))
    (while base-attributes
      (setq attribute (car base-attributes))
      (unless (memq attribute '(:foreground :underline))
	(setq attributes (plist-put attributes attribute
				    (cadr base-attributes))))
      (setq base-attributes (cddr base-attributes)))
    (list (list t attributes)))
  "Face used to highlight the current url in \"about://history/\"."
  :group 'w3m-face)

(defcustom w3m-mode-hook nil
  "*Hook run before `w3m-mode' called."
  :group 'w3m
  :type 'hook)

(defcustom w3m-fontify-before-hook nil
  "*Hook run before `w3m-fontify' called."
  :group 'w3m
  :type 'hook)

(defcustom w3m-fontify-after-hook nil
  "*Hook run after `w3m-fontify' called."
  :group 'w3m
  :type 'hook)

(defcustom w3m-display-hook
  '(w3m-history-highlight-current-url
    w3m-move-point-for-localcgi
    w3m-select-buffer-update)
  "*Hook run at the end of `w3m-goto-url'."
  :group 'w3m
  :type 'hook)

(defcustom w3m-async-exec t
  "*If non-nil, w3m is executed as an asynchronous process."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-process-connection-type
  (not (and (featurep 'xemacs)
	    (string-match "solaris" system-configuration)))
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
	("image/jpeg" "\\.jpe?g$" ("fiber.exe" "-s" file))
	("image/png" "\\.png$" ("fiber.exe" "-s" file))
	("image/gif" "\\.gif$" ("fiber.exe" "-s" file))
	("image/tiff" "\\.tif?f$" ("fiber.exe" "-s" file))
	("image/x-xwd" "\\.xwd$" ("fiber.exe" "-s" file))
	("image/x-xbm" "\\.xbm$" ("fiber.exe" "-s" file))
	("image/x-xpm" "\\.xpm$" ("fiber.exe" "-s" file))
	("image/x-bmp" "\\.bmp$" ("fiber.exe" "-s" file))
	("video/mpeg" "\\.mpe?g$" ("fiber.exe" "-s" file))
	("video/quicktime" "\\.mov$" ("fiber.exe" "-s" file))
	("application/postscript" "\\.\\(ps\\|eps\\)$" ("fiber.exe" "-s" file))
	("application/pdf" "\\.pdf$" ("fiber.exe" "-s" file)))
    (cons
     (list "text/html" "\\.s?html?$"
	   (if (and (condition-case nil (require 'browse-url) (error nil))
		    (fboundp 'browse-url-netscape))
	       'browse-url-netscape
	     '("netscape" url)))
     '(("text/plain" "\\.\\(txt\\|tex\\|el\\)" nil)
       ("image/jpeg" "\\.jpe?g$" ("xv" file))
       ("image/png" "\\.png$" ("xv" file))
       ("image/gif" "\\.gif$" ("xv" file))
       ("image/tiff" "\\.tif?f$" ("xv" file))
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
  (` ((gzip "gunzip" nil)
      (bzip "bunzip2" nil)
      (deflate
	(, (let ((file
		  (expand-file-name
		   (concat "../lib/"
			   (file-name-nondirectory w3m-command)
			   "/"
			   (if (memq system-type '(windows-nt OS/2 emx))
			       "inflate.exe"
			     "inflate"))
		   (file-name-directory
		    (w3m-which-command w3m-command)))))
	     (if (file-executable-p file)
		 file
	       "inflate")))
	nil)))
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

(defcustom w3m-use-form t
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

(defcustom w3m-url-local-directory-alist
  (when (boundp 'yahtml-path-url-alist)
    (mapcar
     (lambda (pair)
       (cons (cdr pair) (car pair)))
     (symbol-value 'yahtml-path-url-alist)))
  "*Alist of URLs and local directories."
  :type '(repeat
	  (cons
	   (string :tag "URL")
	   (directory :tag "Directory")))
  :group 'w3m)

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

(defcustom w3m-use-tab t
  "Use w3m as a tab browser."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-use-tab-menubar t
  "Use 'TAB' menubar."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-pop-up-windows
  (if (or (featurep 'xemacs)
	  (and (boundp 'emacs-major-version)
	       (>= emacs-major-version 21)))
      (not w3m-use-tab)
    t)
  "Like `pop-up-windows', except that it only affects the command
`w3m-copy-buffer'.  If this value is non-nil and the value of the
option `w3m-pop-up-frames' is nil, split the windows when a new
session is created.  If you are using XEmacs with the buffers tab in
the gutter area, it is recommended a bit that setting both this option
and the option `w3m-pop-up-frames' to nil and you turn on the option
`w3m-xmas-show-current-title-in-buffer-tab'."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-pop-up-frames nil
  "Like `pop-up-frames', except that it only affects the `w3m' commands.
If you are using XEmacs with the buffers tab in the gutter area, it is
recommended a bit that setting both this option and the option
`w3m-pop-up-windows' to nil and you turn on the option
`w3m-xmas-show-current-title-in-buffer-tab'."
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

(defcustom w3m-use-refresh t
  "*If non-nil, support REFRESH attribute in META tags."
  :group 'w3m
  :type 'boolean)

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
  :type (` (choice (const :tag "w3m internal CGI" nil)
		   (file :tag "path of 'dirlist.cgi'"
			 (, (expand-file-name
			     (concat "../lib/"
				     (file-name-nondirectory w3m-command)
				     "/dirlist.cgi")
			     (file-name-directory
			      (w3m-which-command w3m-command))))))))

(defcustom w3m-add-referer-regexps
  (when (or (not (boundp 'w3m-add-referer))
	    (symbol-value 'w3m-add-referer))
    (cons "\\`http:"
	  "\\`http://\\(localhost\\|127\\.0\\.0\\.1\\)/"))
  "*Cons of two regexps to allow and not to allow sending a reference
information to HTTP servers.  If a reference matches the car of this
value and it does not match the cdr of this value, it will be sent.
You may set the cdr of this value to inhibit sending references which
will disclose your private informations, for example:

\(setq w3m-add-referer-regexps
      '(\"^http:\"
	. \"^http://\\\\([^./]+\\\\.\\\\)*your-company\\\\.com/\"))
"
  :group 'w3m
  :type '(cons (list :inline t :format "%v"
		     (radio :tag "Allow"
			    regexp
			    (const :tag "Don't allow all" nil))
		     (radio :tag "Don't allow"
			    regexp
			    (const :tag "Allow all" nil)))))

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
	     '(;("nbsp" . 160)
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
    (format "&\\(%s\\|#[0-9]+\\|#x[0-9a-f]+\\);?"
	    (regexp-opt (mapcar (function car) w3m-entity-alist)))))

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

(defconst w3m-emacs-w3m-icon "\
R0lGODlhQgAOAPIAAEFp4f+MAJkyzC6LV////yCyqv9FAP8AACH/C05FVFNDQVBFMi4wAwEA
AAAh+QQEIQD/ACwAAAAAQgAOAEADoki63P4wyqmAvQQQofj2nOcMhVQUQRo4BtEeCkzN9Nwu
cq3vfO/7h2AuolloQp0NgzNgNEsLqGNFWFFdhhtMNixWMmCwSIBcMJ0RKPRadbkJw598Tq8P
448LiEw28xcDgU0KJycRVCs3DFtCcV5gR0l+ZQ2DUYWGiG0NL3AxDI9fRZRKIg+WD2sKbC2t
n6AYkF8fZXwigqgOagsqmjeKdcE0CQAh+QQFIQAAACwAAAAAAQABAAADAggJACH5BAUhAAAA
LAAAAAABAAEAAAMCCAkAIfkEBSEAAAAsAgACAD4ACgAAAxhIutz+MMpJq7046827/2AojmRp
nmiqhgkAIfkEBSEAAQAsAgAFAA0ABwAAAxQYsNHu4MXXZlV06Whvxt7FiZpYJQAh+QQFIQAA
ACwGAAUAFwAHAAADHygA0r3MwTmVYhheSu+OFlctnpeJ4xei3PaoLPnKTwIAIfkEBSEAAAAs
FwAFAA0ABwAAAxEIswP+rj0JK6T25oz13EyTAAAh+QQFIQAAACweAAUAEwAHAAADFgi1Bf4Q
tjejvYxdV3Hf1gdK3BhNRQIAIfkEBSEAAAAsLAAFAA8ABwAAAxUYALr+bKkGpaVVhp1rw53l
gNDGMQkAIfkEBSEAAAAsMQAEAA8ACAAAAxQIYNre7Elp6rzxOpY1XxoEhuKSAAA7"
  "A small icon image for the url about://emacs-w3m.gif.  It is encoded
in the optimized interlaced endlessly animated gif format and base64.")

(defconst w3m-modeline-process-status-on "<PRC>"
  "Modeline string which is displayed when the process is runnning now.")

(defconst w3m-modeline-image-status-on "[IMG]"
  "Modeline string which is displayed when inline image is on.")

(defconst w3m-modeline-status-off "[ - ]"
  "Modeline string which is displayed when default status.")

(defvar w3m-initial-frame nil "Initial frame of this session.")
(make-variable-buffer-local 'w3m-initial-frame)

(defvar w3m-image-only-page nil "Non-nil if image only page.")
(make-variable-buffer-local 'w3m-image-only-page)

(defvar w3m-current-process nil "Current retrieving process of this buffer.")
(make-variable-buffer-local 'w3m-current-process)

(defvar w3m-refresh-timer nil "Timer of refresh process,")
(make-variable-buffer-local 'w3m-refresh-timer)

(defvar w3m-current-base-url nil "Base URL of this buffer.")
(defvar w3m-current-forms nil "Forms of this buffer.")
(defvar w3m-current-coding-system nil "Current coding-system of this buffer.")
(defvar w3m-next-url nil "Next URL of this buffer.")
(defvar w3m-previous-url nil "Previous URL of this buffer.")
(defvar w3m-start-url nil "Start URL of this buffer.")
(defvar w3m-contents-url nil "Table of Contents URL of this buffer.")
(defvar w3m-max-anchor-sequence nil "Maximum number of anchor sequence on this buffer.")
(defvar w3m-current-refresh nil "Cons pair of refresh attribute, '(sec . url).")

(make-variable-buffer-local 'w3m-current-url)
(make-variable-buffer-local 'w3m-current-base-url)
(make-variable-buffer-local 'w3m-current-title)
(make-variable-buffer-local 'w3m-current-forms)
(make-variable-buffer-local 'w3m-current-coding-system)
(make-variable-buffer-local 'w3m-next-url)
(make-variable-buffer-local 'w3m-previous-url)
(make-variable-buffer-local 'w3m-start-url)
(make-variable-buffer-local 'w3m-contents-url)
(make-variable-buffer-local 'w3m-max-anchor-sequence)
(make-variable-buffer-local 'w3m-current-refresh)

(defsubst w3m-clear-local-variables ()
  (setq w3m-current-url nil
	w3m-current-base-url nil
	w3m-current-title nil
	w3m-current-forms nil
	w3m-current-coding-system nil
	w3m-next-url nil
	w3m-previous-url nil
	w3m-start-url nil
	w3m-contents-url nil
	w3m-max-anchor-sequence nil
	w3m-current-refresh nil))

(defsubst w3m-copy-local-variables (from-buffer)
  (let (url base title forms cs next prev start toc hseq refresh)
    (with-current-buffer from-buffer
      (setq url w3m-current-url
	    base w3m-current-base-url
	    title w3m-current-title
	    forms w3m-current-forms
	    cs w3m-current-coding-system
	    next w3m-next-url
	    prev w3m-previous-url
	    start w3m-start-url
	    toc w3m-contents-url
	    hseq w3m-max-anchor-sequence
	    refresh w3m-current-refresh))
    (setq w3m-current-url url
	  w3m-current-base-url base
	  w3m-current-title title
	  w3m-current-forms forms
	  w3m-current-coding-system cs
	  w3m-next-url next
	  w3m-previous-url prev
	  w3m-start-url start
	  w3m-contents-url toc
	  w3m-max-anchor-sequence hseq
	  w3m-current-refresh refresh)))

(defvar w3m-verbose t "Flag variable to control messages.")

(defvar w3m-safe-url-regexp nil "Regexp of URLs which point safe contents.")

(defvar w3m-current-buffer nil)
(defvar w3m-cache-buffer nil)
(defvar w3m-cache-articles nil)
(defvar w3m-cache-hashtb nil)
(defvar w3m-input-url-history nil)

(defconst w3m-arrived-db-size 1023)
(defvar w3m-arrived-db nil)		; nil means un-initialized.

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
  (if (equal "Japanese" w3m-language)
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

(defvar w3m-work-buffer-list nil)
(defconst w3m-work-buffer-name " *w3m-work*")
(defconst w3m-select-buffer-name " *w3m buffers*")

(defconst w3m-meta-content-type-charset-regexp
  (eval-when-compile
    (concat "<meta[ \t]+http-equiv=\"?Content-type\"?[ \t]+content=\"\\([^;]+\\)"
	    ";[ \t]*charset=\"?\\([^\"]+\\)\"?"
	    "[ \t]*/?>"))
  "Regexp used in parsing `<META HTTP-EQUIV=\"Content-Type\" content=\"...;charset=...\">
for a charset indication")

(defconst w3m-meta-charset-content-type-regexp
  (eval-when-compile
    (concat "<meta[ \t]+content=\"\\([^;]+\\)"
	    ";[ \t]*charset=\"?\\([^\"]+\\)\"?"
	    "[ \t]+http-equiv=\"?Content-type\"?[ \t]*/?>"))
  "Regexp used in parsing `<META content=\"...;charset=...\" HTTP-EQUIV=\"Content-Type\">
for a charset indication")

(defconst w3m-meta-refresh-content-regexp
  (eval-when-compile
    (concat "<meta[ \t]+http-equiv=\"?refresh\"?[ \t]+"
	    "content=\"\\([^;]+\\)"
	    ";[ \t]*url=\"?\\([^\"]+\\)\"?"
	    "[ \t]*/?>"))
  "Regexp used in parsing `<META HTTP-EQUIV=\"Refresh\" content=\"n;url=...\">
for a refresh indication")

(defconst w3m-meta-content-refresh-regexp
  (eval-when-compile
    (concat "<meta[ \t]+content=\"\\([^;]+\\)"
	    ";[ \t]*url=\"?\\([^\"]+\\)\"?"
	    "[ \t]+http-equiv=\"?refresh\"?[ \t]*/?>"))
  "Regexp used in parsing `<META content=\"n;url=...\" HTTP-EQUIV=\"Refresh\">
for a refresh indication")

(eval-and-compile
  (defconst w3m-html-string-regexp
    "\\(\"\\([^\"]+\\)\"\\|'\\([^\']+\\)'\\|[^\"\'<> \t\r\f\n]*\\)"
    "Regexp used in parsing to detect string."))

(defconst w3m-dump-head-source-command-arguments
  (cond ((eq w3m-type 'w3mmee)
	 (list
	  '(if w3m-accept-languages "-o")
	  '(if w3m-accept-languages
	       (concat "request_header Accept-Language: "
		       (mapconcat 'identity w3m-accept-languages " ")))
	  "-dump=extra,head,source"))
	(t
	 (list
	  '(if w3m-accept-languages "-o")
	  '(if w3m-accept-languages
	       (concat "accept_language="
		       (mapconcat 'identity w3m-accept-languages " ")))
	  "-dump_extra")))
  "Arguments for 'dump_extra' execution of w3m.")

(defvar w3m-halfdump-command nil
  "Name of the executable file of w3m.  If nil use `w3m-command'.")

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
  "^about:\\(//\\(header\\|source\\|history\\|db-history\\|antenna\\|namazu\\|dtree\\)/.*\\)?$"
  "Regexp of urls to be ignored in an arrived-db.")

(defconst w3m-history-ignored-regexp
  "^about:\\(//\\(header\\|source\\|history\\|db-history\\|antenna\\|namazu\\|dtree\\)/.*\\)?$"
  "Regexp of urls to be ignored in a history.")

(defvar w3m-mode-map nil "Keymap used in w3m-mode buffers.")


;; Generic functions:
(defun w3m-url-to-file-name (url)
  "Return the file name which is pointed by URL.
When URL does not point any local files, it returns nil."
  ;; Remove scheme part and net_loc part.  NOTE: This function accepts
  ;; only urls whose net_loc part is empty or NULL string.
  (cond
   ((string-match "\\`\\(file:\\(//\\)?\\|about://dtree\\)/" url)
    (setq url (substring url (match-end 1)))
    ;; Process abs_path part in Windows.
    (if (string-match
	 "\\`/\\(\\([a-zA-Z]\\)[|:]?\\|cygdrive/\\([a-zA-Z]\\)\\)/" url)
	(concat (or (match-string 2 url) (match-string 3 url))
		":/"
		(substring url (match-end 0)))
      url))
   ((string-match "\\`\\([~/]\\|[a-zA-Z]:/\\|\\.\\.?/\\)" url) url)
   (t
    (catch 'found-file
      (dolist (pair w3m-url-local-directory-alist)
	(and (string-match (concat "\\`"
				   (regexp-quote
				    (file-name-as-directory (car pair))))
			   url)
	     (let ((file (expand-file-name (substring url (match-end 0))
					   (cdr pair))))
	       (when (file-exists-p file)
		 (throw 'found-file file)))))))))

(defun w3m-expand-file-name-as-url (file &optional directory)
  "Return URL which points the FILE."
  (setq file (expand-file-name file directory))
  (concat "file://"
	  (if (string-match "\\`\\([a-zA-Z]\\):" file)
	      (format (if w3m-use-cygdrive "/cygdrive/%s%s" "/%s|%s")
		      (match-string 1 file)
		      (substring file (match-end 0)))
	    file)))

;; Generic macros and inline functions:
(defun w3m-attributes (url &optional no-cache handler)
  "Return a list of attributes of URL.
Value is nil if retrieval of header is failed.  Otherwise, list
elements are:
 0. Type of contents.
 1. Charset of contents.
 2. Size in bytes.
 3. Encoding of contents.
 4. Last modification time.
 5. Real URL.
 6. Base URL.
If optional argument NO-CACHE is non-nil, cache is not used."
  (if (not handler)
      (condition-case nil
	  (w3m-process-with-wait-handler
	    (w3m-attributes url no-cache handler))
	(w3m-process-timeout nil))
    (when (string-match "\\`\\([^#]*\\)#" url)
      (setq url (substring url 0 (match-end 1))))
    (cond
     ((string= "about://emacs-w3m.gif" url)
      (list "image/gif" nil nil nil nil url url))
     ((string-match "\\`about://source/" url)
      (lexical-let ((src (substring url (match-end 0))))
	(w3m-process-do
	    (attrs (w3m-attributes src no-cache handler))
	  (list "text/plain"
		(or (w3m-arrived-content-charset src) (cadr attrs))
		(nth 2 attrs)
		(nth 3 attrs)
		(nth 4 attrs)
		(concat "about://source/" (nth 5 attrs))
		(nth 6 attrs)))))
     ((string-match "\\`about:" url)
      (list "text/html" w3m-coding-system nil nil nil url url))
     ((w3m-url-local-p url)
      (w3m-local-attributes url))
     (t
      (w3m-w3m-attributes url no-cache handler)))))

(defmacro w3m-content-type (url &optional no-cache handler)
  (if handler
      `(let ((handler ,handler))
	 (w3m-process-do
	     (attrs (w3m-attributes ,url ,no-cache handler))
	   (car attrs)))
    `(car (w3m-attributes ,url ,no-cache))))
(defmacro w3m-content-charset (url &optional no-cache handler)
  (if handler
      `(let ((handler ,handler))
	 (w3m-process-do
	     (attrs (w3m-attributes ,url ,no-cache handler))
	   (nth 1 attrs)))
    `(nth 1 (w3m-attributes ,url ,no-cache))))
(defmacro w3m-content-length (url &optional no-cache handler)
  (if handler
      `(let ((handler ,handler))
	 (w3m-process-do
	     (attrs (w3m-attributes ,url ,no-cache handler))
	   (nth 2 attrs)))
    `(nth 2 (w3m-attributes ,url ,no-cache))))
(defmacro w3m-content-encoding (url &optional no-cache handler)
  (if handler
      `(let ((handler ,handler))
	 (w3m-process-do
	     (attrs (w3m-attributes ,url ,no-cache handler))
	   (nth 3 attrs)))
    `(nth 3 (w3m-attributes ,url ,no-cache))))
(defmacro w3m-last-modified (url &optional no-cache handler)
  (if handler
      `(let ((handler ,handler))
	 (w3m-process-do
	     (attrs (w3m-attributes ,url ,no-cache handler))
	   (nth 4 attrs)))
    `(nth 4 (w3m-attributes ,url ,no-cache))))
(defmacro w3m-real-url (url &optional no-cache handler)
  (if handler
      `(let ((handler ,handler))
	 (w3m-process-do
	     (attrs (w3m-attributes ,url ,no-cache handler))
	   (nth 5 attrs)))
    `(nth 5 (w3m-attributes ,url ,no-cache))))
(defmacro w3m-base-url (url &optional no-cache handler)
  (if handler
      `(let ((handler ,handler))
	 (w3m-process-do
	     (attrs (w3m-attributes ,url ,no-cache handler))
	   (nth 6 attrs)))
    `(nth 6 (w3m-attributes ,url ,no-cache))))

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

(defmacro w3m-popup-frame-p (&optional force)
  "Return non-nil if the command `w3m' should popup a new frame.  If
optional FORCE is non-nil, treat as if the last command is called
interactively."
  (list 'and 'w3m-pop-up-frames
	(list 'or force '(interactive-p))
	(if (featurep 'xemacs)
	    '(device-on-window-system-p)
	  'window-system)))

(defun w3m-message (&rest args)
  "Alternative function of `message' for emacs-w3m."
  (if (and w3m-verbose
	   (or (not (bufferp w3m-current-buffer))
	       (get-buffer-window w3m-current-buffer)))
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

(defun w3m-load-list (file &optional coding-system)
  "Return contents of FILE as a list.  CODING-SYSTEM is used to read FILE
which defaults to the value of `w3m-file-coding-system-for-read'."
  (when (and (file-readable-p file)
	     ;; XEmacs 21.4 might crash when inserting a directory.
	     (not (file-directory-p file)))
    (with-temp-buffer
      (when (condition-case nil
		(let ((coding-system-for-read
		       (or coding-system
			   w3m-file-coding-system-for-read))
		      (file-coding-system-for-read
		       (or coding-system
			   w3m-file-coding-system-for-read
			   (if (boundp 'file-coding-system-for-read)
			       (symbol-value 'file-coding-system-for-read)))))
		  (insert-file-contents file))
	      (error
	       (message "Error while loading %s" file)
	       nil))
	;; point is not always moved to the beginning of the buffer
	;; after `insert-file-contents' is done.
	(goto-char (point-min))
	(condition-case err
	    (read (current-buffer))
	  (error
	   (message "Error while reading %s; %s"
		    file (error-message-string err))
	   nil))))))

(defun w3m-save-list (file list &optional coding-system)
  "Save LIST into file with CODING."
  (when (and list (file-writable-p file))
    (with-temp-buffer
      (let ((file-coding-system (or coding-system w3m-file-coding-system))
	    (coding-system-for-write (or coding-system w3m-file-coding-system))
	    (standard-output (current-buffer))
	    element print-length print-level)
	(insert (format "\
;;; %s  -*- mode: emacs-lisp%s -*-
;; This file is generated automatically by Emacs-W3M v%s.

"
			(file-name-nondirectory file)
			(if coding-system-for-write
			    (format "; coding: %s" coding-system-for-write)
			  "")
			emacs-w3m-version))
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
    (let ((parent (when (string-match "\\`\\([^#]*\\)#" url)
		    (substring url 0 (match-end 1))))
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
    (let ((list (w3m-load-list w3m-arrived-file)))
      (when (stringp (car list))
	;; When arrived URL database is too old, its data is ignored.
	(setq list nil)
	(delete-file w3m-arrived-file))
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
    (dolist (elem (w3m-load-list w3m-arrived-file))
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
		     (w3m-sub-list
		      (sort list
			    (lambda (a b)
			      (w3m-time-newer-p (nth 3 a) (nth 3 b))))
		      w3m-keep-arrived-urls)))
    (setq w3m-arrived-db nil)))

(add-hook 'kill-emacs-hook 'w3m-arrived-shutdown)


(defun w3m-url-encode-string (str &optional coding)
  (apply (function concat)
	 (mapcar
	  (lambda (ch)
	    (cond
	     ((eq ch ?\n)		; newline
	      "%0D%0A")
	     ((string-match "[-a-zA-Z0-9_:/.]" (char-to-string ch)) ; xxx?
	      (char-to-string ch))	; printable
	     ((char-equal ch ?\x20)	; space
	      "+")
	     (t
	      (format "%%%02X" ch))))	; escape
	  ;; Coerce a string to a list of chars.
	  (append (encode-coding-string (or str "")
					(or coding
					    w3m-default-coding-system
					    w3m-coding-system
					    'iso-2022-7bit))
		  nil))))

(defun w3m-url-decode-string (str &optional coding)
  (let ((start 0)
	(buf))
    (while (string-match "+\\|%\\(0D%0A\\|\\([0-9a-fA-F][0-9a-fA-F]\\)\\)" str start)
      (push (substring str start (match-beginning 0)) buf)
      (push (cond
	     ((match-beginning 2)
	      (string (string-to-number (match-string 2 str) 16)))
	     ((match-beginning 1) "\n")
	     (t " "))
	    buf)
      (setq start (match-end 0)))
    (decode-coding-string
     (apply 'concat (nreverse (cons (substring str start) buf)))
     (or coding
	 w3m-default-coding-system
	 w3m-coding-system
	 'iso-2022-7bit))))


;;; HTML character entity handling:
(defun w3m-entity-db-setup ()
  ;; initialize entity database (obarray)
  (setq w3m-entity-db (make-vector w3m-entity-db-size 0))
  (dolist (elem w3m-entity-alist)
    (set (intern (car elem) w3m-entity-db)
	 (cdr elem))))

(eval-and-compile
  (unless (fboundp 'w3m-ucs-to-char)
    (defun w3m-ucs-to-char (codepoint)
      ;; case of immediate character (accept only 0x20 .. 0x7e)
      (if (or (< codepoint 32) (< 127 codepoint))
	  ?~				; un-supported character
	codepoint))))

(defsubst w3m-entity-value (name)
  ;; initialize if need
  (unless w3m-entity-db
    (w3m-entity-db-setup))
  ;; return value of specified entity, or empty string for unknown entity.
  (or (symbol-value (intern-soft name w3m-entity-db))
      (if (not (char-equal (string-to-char name) ?#))
	  (concat "&" name)		; unknown entity
	(setq name (substring name 1))
	(let ((codepoint (if (char-equal (string-to-char name) ?x)
			     (string-to-number (substring name 1) 16)
			   (string-to-number name))))
	  (char-to-string (w3m-ucs-to-char codepoint))))))

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
    (setq w3m-max-anchor-sequence 0)	;; reset max-hseq
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
      (w3m-parse-attributes (href name (rel :case-ignore) (hseq :integer))
	(when rel
	  (setq rel (split-string rel))
	  (cond
	   ((member "next" rel) (setq w3m-next-url href))
	   ((or (member "prev" rel) (member "previous" rel))
	    (setq w3m-previous-url href))
	   ((member "start" rel) (setq w3m-start-url href))
	   ((member "contents" rel) (setq w3m-contents-url href))))
	(delete-region start (point))
	(cond
	 (href
	  (when (re-search-forward "[ \t\r\f\n]*\\(</a>\\)" nil t)
	    (setq end (match-beginning 0))
	    (delete-region (match-beginning 1) (match-end 1))
	    (setq href (w3m-expand-url (w3m-decode-anchor-string href)))
	    (setq hseq (or (and (null hseq) 0) (abs hseq)))
	    (setq w3m-max-anchor-sequence (max hseq w3m-max-anchor-sequence))
	    (w3m-add-text-properties start end
				     (list 'face (if (w3m-arrived-p href)
						     'w3m-arrived-anchor-face
						   'w3m-anchor-face)
					   'w3m-href-anchor href
					   'mouse-face 'highlight
					   'w3m-name-anchor name
					   'w3m-anchor-sequence hseq
					   'help-echo help
					   'balloon-help balloon))))
	 (name
	  (when (re-search-forward "[<\n]" nil t)
	    (goto-char (setq end (match-beginning 0)))
	    (when (= start end)
	      (setq end (min (if (looking-at "\\(<[^>]*>\\)+")
				 (1+ (match-end 0))
			       (1+ end))
			     (point-max))))
	    (w3m-add-text-properties start end
				     (list 'w3m-name-anchor name)))))))
    (when w3m-next-url
      (setq w3m-next-url (w3m-expand-url w3m-next-url)))
    (when w3m-previous-url
      (setq w3m-previous-url (w3m-expand-url w3m-previous-url)))
    (when w3m-start-url
      (setq w3m-start-url (w3m-expand-url w3m-start-url)))
    (when w3m-contents-url
      (setq w3m-contents-url (w3m-expand-url w3m-contents-url)))))

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
	  (when w3m-use-tab-menubar (w3m-setup-tab-menu))
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
      (setq src (w3m-expand-url (w3m-decode-anchor-string src)))
      (when (search-forward "</img_alt>" nil t)
	(delete-region (setq end (match-beginning 0)) (match-end 0))
	(w3m-add-text-properties start end (list 'w3m-image src
						 'w3m-image-status 'off
						 'w3m-image-redundant upper))
	(unless (or (get-text-property start 'w3m-href-anchor)
		    (get-text-property start 'w3m-action))
	  ;; No need to use `w3m-add-text-properties' here.
	  (add-text-properties start end (list 'face 'w3m-image-face
					       'mouse-face 'highlight
					       'help-echo help
					       'balloon-help balloon)))))))

(defsubst w3m-toggle-inline-images-internal (status no-cache url)
  "Toggle displaying of inline images on current buffer.
STATUS is current image status.
If NO-CACHE is non-nil, cache is not used.
If URL is specified, only the image with URL is toggled."
  (interactive "P")
  (let ((cur-point (point))
	(buffer-read-only)
	point beg end iurl image)
    (if (equal status 'off)
	(save-excursion
	  (goto-char (point-min))
	  (while (if (get-text-property (point) 'w3m-image)
		     (setq point (point))
		   (setq point (next-single-property-change (point)
							    'w3m-image)))
	    (setq beg point
		  end (or (next-single-property-change point 'w3m-image)
			  (point-max)))
	    (goto-char end)
	    (setq iurl (w3m-image point))
	    (when (and (or (null url)
			   ;; URL is specified and is same as the image URL.
			   (and url (string= url iurl)))
		       (not (eq (get-text-property beg 'w3m-image-status)
				'on)))
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
		  (when iurl
		    (w3m-process-with-null-handler
		      (lexical-let ((start (set-marker (make-marker) point))
				    (end (set-marker (make-marker) end))
				    (url w3m-current-url))
			(w3m-process-do
			    (image (let ((w3m-current-buffer (current-buffer)))
				     (w3m-create-image
				      iurl no-cache w3m-current-url handler)))
			  (when (and image
				     (buffer-live-p (marker-buffer start)))
			    (with-current-buffer (marker-buffer start)
			      (when (equal url w3m-current-url)
				(let (buffer-read-only)
				  (w3m-insert-image start end image))
				;; Redisplay
				(when w3m-force-redisplay
				  (sit-for 0))))
			    (set-marker start nil)
			    (set-marker end nil))))))))
	      (w3m-add-text-properties beg end '(w3m-image-status on)))))
      ;; Remove.
      (save-excursion
	(goto-char (point-min))
	(while (if (get-text-property (point) 'w3m-image)
		   (setq point (point))
		 (setq point (next-single-property-change (point)
							  'w3m-image)))
	  (setq beg point
		end (or (next-single-property-change point 'w3m-image)
			(point-max)))
	  (goto-char end)
	  (setq iurl (w3m-image point))
	  ;; IMAGE-ALT-STRING DUMMY-STRING
	  ;; <--------w3m-image---------->
	  ;; <---redundant--><---dummy--->
	  ;; <---invisible-->
	  (when (and (or (null url)
			 ;; URL is specified and is not same as the image URL.
			 (and url (string= url iurl)))
		     (not (eq (get-text-property beg 'w3m-image-status)
			      'off)))
	    (cond
	     ((get-text-property point 'w3m-image-redundant)
	      ;; Remove invisible property.
	      (remove-text-properties point end '(invisible nil)))
	     ((get-text-property point 'w3m-image-dummy)
	      ;; Remove dummy string.
	      (delete-region point end))
	     (t (w3m-remove-image point end)))
	    (w3m-add-text-properties beg end '(w3m-image-status off))))))))

(defun w3m-toggle-inline-image (&optional force no-cache)
  "Toggle displaying of inline image on cursor point.
If FORCE is non-nil, image displaying is forced.
If NO-CACHE is non-nil, cache is not used."
  (interactive "P")
  (unless (w3m-display-graphic-p)
    (error "Can't display images in this environment"))
  (let ((url (w3m-image (point)))
	(status (get-text-property (point) 'w3m-image-status)))
    (if url
	(progn
	  (if force (setq status 'off))
	  (unwind-protect
	      (w3m-toggle-inline-images-internal status no-cache url)
	    (set-buffer-modified-p nil)))
      (message "No image at point"))))

(defun w3m-toggle-inline-images (&optional force no-cache)
  "Toggle displaying of inline images on current buffer.
If FORCE is non-nil, image displaying is forced.
If NO-CACHE is non-nil, cache is not used."
  (interactive "P")
  (let ((status w3m-display-inline-images))
    (unless (w3m-display-graphic-p)
      (error "Can't display images in this environment"))
    (if force (setq w3m-display-inline-images nil
		    status nil))
    (unwind-protect
	(w3m-toggle-inline-images-internal (if w3m-display-inline-images
					       'on 'off)
					   no-cache nil)
      (unless (setq w3m-display-inline-images (not status))
	(w3m-process-stop (current-buffer)))
      (set-buffer-modified-p nil)
      (force-mode-line-update))))

(defun w3m-decode-entities (&optional reserve-prop)
  "Decode entities in the current buffer.
If optional RESERVE-PROP is non-nil, text property is reserved."
  (save-excursion
    (goto-char (point-min))
    (let (prop)
      (while (re-search-forward w3m-entity-regexp nil t)
	(if reserve-prop
	    (setq prop (text-properties-at (match-beginning 0))))
	(replace-match (save-match-data
			 (w3m-entity-value (match-string 1)))
		       nil t)
	(if (and reserve-prop prop)
	    (w3m-add-text-properties (match-beginning 0) (point) prop))))))

(defun w3m-decode-entities-string (string)
  "Decode entities in the STRING."
  (with-temp-buffer
    (insert string)
    (save-match-data (w3m-decode-entities))
    (buffer-string)))

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
    (when w3m-use-form
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
  (if (string-match "\\`\\(file:\\|[/~]\\|\\.\\.?/\\|[a-zA-Z]:\\)" url)
      (if (eq flag 'lambda)
	  (file-exists-p (w3m-url-to-file-name url))
	(let* ((partial
		(expand-file-name
		 (cond
		  ((string-match "\\`file:[^/]" url)
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
	  (setq partial
		(if (string-match "/\\.\\'" url)
		    (concat (file-name-as-directory
			     (w3m-expand-file-name-as-url partial))
			    ".")
		  (w3m-expand-file-name-as-url partial)))
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

(cond
 ((locate-library "ffap")
  (autoload 'ffap-url-at-point "ffap")
  (defalias 'w3m-url-at-point 'ffap-url-at-point))
 ((locate-library "thingatpt")
  (autoload 'thing-at-point "thingatpt")
  (defun w3m-url-at-point ()
    "Return url from around point if it exists, or nil."
    (thing-at-point 'url)))
 (t
  (defalias 'w3m-url-at-point 'ignore)))

(defun w3m-input-url (&optional prompt initial default quick-start)
  "Read a URL from the minibuffer, prompting with string PROMPT."
  (let (url)
    (w3m-arrived-setup)
    (unless default
      (setq default w3m-home-page))
    (unless initial
      (setq initial (w3m-url-at-point)))
    (if (and quick-start
	     default
	     (not initial))
	default
      (setq url (let ((minibuffer-setup-hook
		       (append minibuffer-setup-hook '(beginning-of-line))))
		  (completing-read
		   (or prompt
		       (if default
			   (format "URL (default %s): "
				   (if (stringp default)
				       (if (eq default w3m-home-page)
					   "HOME" default)
				     (prin1-to-string default)))
			 "URL: "))
		   'w3m-url-completion nil nil initial
		   'w3m-input-url-history)))
      (if (string= "" url) (setq url default))
      ;; remove duplication
      (when (stringp url)
	(setq w3m-input-url-history
	      (cons url (delete url w3m-input-url-history))))
      ;; return value
      url)))


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

(defsubst w3m-cache-header-delete-variable-part (header)
  (let (buf)
    (dolist (line (split-string header "\n+"))
      (unless (string-match "\\`\\(Date\\|Server\\|W3m-[^:]+\\):" line)
	(push line buf)))
    (mapconcat (function identity) (nreverse buf) "\n")))

(defun w3m-cache-header (url header &optional overwrite)
  "Store up URL's HEADER in cache."
  (w3m-cache-setup)
  (let ((ident (intern url w3m-cache-hashtb)))
    (if (boundp ident)
	(if (and (not overwrite)
		 (string=
		  (w3m-cache-header-delete-variable-part header)
		  (w3m-cache-header-delete-variable-part (symbol-value ident))))
	    (symbol-value ident)
	  (w3m-cache-remove url)
	  (set ident header))
      (set ident header))))

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

;; FIXME: リモートで更新されていないか、チェックする機構が必要。
(defun w3m-cache-available-p (url)
  "Return non-nil, if URL's data is cached."
  (w3m-cache-setup)
  (and (stringp url)
       (let ((ident (intern url w3m-cache-hashtb)))
	 (and (memq ident w3m-cache-articles) ident))))

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
(defun w3m-decode-encoded-contents (encoding)
  "Decode encoded (gzipped, bzipped, deflated, etc) contents in this buffer."
  (let ((x (and (stringp encoding)
		(assoc encoding w3m-encoding-alist))))
    (or (not (and x (setq x (cdr (assq (cdr x) w3m-decoder-alist)))))
	(let ((coding-system-for-write 'binary)
	      (coding-system-for-read 'binary)
	      (default-process-coding-system (cons 'binary 'binary)))
	  (w3m-process-with-environment w3m-command-environment
	    (zerop (apply 'call-process-region
			  (point-min) (point-max)
			  (w3m-which-command (car x))
			  t '(t nil) nil (cadr x))))))))

(defun w3m-decode-buffer (url &optional content-charset content-type)
  (w3m-decode-get-refresh url)
  (let (cs)
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
    (when content-charset
      (setq cs (w3m-charset-to-coding-system content-charset)))
    (when (and (eq w3m-type 'w3mmee)
	       (or (and (stringp content-charset)
			(string= "x-moe-internal" (downcase content-charset)))
		   (eq content-charset 'x-moe-internal)))
      (setq cs (w3m-x-moe-decode-buffer)))
    (decode-coding-region
     (point-min) (point-max)
     (setq w3m-current-coding-system
	   (or cs
	       (w3m-detect-coding-region
		(point-min) (point-max)
		(if (w3m-url-local-p url)
		    nil
		  w3m-coding-system-priority-list)))))
    (set-buffer-multibyte t)))

(defun w3m-decode-get-refresh (url)
  "Get REFRESH attribute in META tags."
  (setq w3m-current-refresh nil)
  (when (and w3m-use-refresh
	     (not (string-match "\\`about://" url)))
    (goto-char (point-min))
    (let ((case-fold-search t)
	  sec refurl)
      (goto-char (point-min))
      (when (or (re-search-forward w3m-meta-refresh-content-regexp nil t)
		(re-search-forward w3m-meta-content-refresh-regexp nil t))
	(setq sec (match-string-no-properties 1))
	(setq refurl (match-string-no-properties 2))
	(when (string-match "^[0-9]+$" sec)
	  (setq w3m-current-refresh (cons (string-to-number sec)
					  (w3m-expand-url refurl url))))))))

(defun w3m-x-moe-decode-buffer ()
  (let ((args '("-i" "-cs" "x-moe-internal"))
	(coding-system-for-read 'binary)
	(coding-system-for-write 'binary)
	(default-process-coding-system (cons 'binary 'binary))
	charset)
    (if (w3m-find-coding-system 'utf-8)
	(setq args (append args '("-o" "-cs" "utf-8"))
	      charset 'utf-8)
      (setq args
	    (append args (list "-o" "-cs" (symbol-name w3m-coding-system))))
      (setq charset w3m-coding-system))
    (w3m-process-with-environment w3m-command-environment
      (apply 'call-process-region (point-min) (point-max)
	     w3m-mbconv-command t t nil args))
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
 6. Base URL.
"
  (let ((file (w3m-url-to-file-name url))
	(attr))
    (setq attr
	  (if (file-exists-p file)
	      (file-attributes file)
	    (when (file-exists-p
		   (setq attr (w3m-url-decode-string
			       file w3m-file-name-coding-system)))
	      (setq file attr)
	      (file-attributes file))))
    (list (w3m-local-content-type url)
	  nil
	  (nth 7 attr)
	  nil
	  (nth 5 attr)
	  (w3m-url-encode-string
	   (w3m-expand-file-name-as-url (file-truename file))
	   w3m-file-name-coding-system)
	  ;; FIXME: ファイルに含まれている <base> タグの指定を解釈する
	  ;; 必要がある。
	  (w3m-url-encode-string
	   (w3m-expand-file-name-as-url (file-truename file))
	   w3m-file-name-coding-system))))

(defun w3m-local-retrieve (url &optional no-decode &rest args)
  "Retrieve content of local URL and insert it to this buffer.
This function will return content-type of URL as string when retrieval
succeed."
  (let ((file (w3m-url-to-file-name url)))
    (when (or (file-readable-p file)
	      (file-readable-p
	       (setq file (w3m-url-decode-string
			   file w3m-file-name-coding-system))))
      (if (file-directory-p file)
	  (w3m-local-dirlist-cgi url)
	(let ((coding-system-for-read 'binary)
	      (file-coding-system-for-read 'binary))
	  (if no-decode
	      (let (jka-compr-compression-info-list
		    jam-zcat-filename-list
		    format-alist)
		(insert-file-contents file))
	    (insert-file-contents file))))
      (w3m-local-content-type file))))

(defun w3m-local-dirlist-cgi (url)
  (w3m-message "Reading %s..." url)
  (if w3m-dirlist-cgi-program
      (if (file-executable-p w3m-dirlist-cgi-program)
	  (let ((coding-system-for-read 'binary)
		(default-process-coding-system
		  (cons 'binary 'binary))
		file beg end)
	    (w3m-process-with-environment
		(list
		 (cons "QUERY_STRING"
		       (encode-coding-string (w3m-url-to-file-name url)
					     w3m-file-name-coding-system)))
	      (call-process w3m-dirlist-cgi-program nil t nil))
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
    (w3m-process-with-wait-handler
      (w3m-process-start handler "-dump_source" url)))
  ;; bind charset to w3m-file-name-coding-system
  (let ((charset (or (car (rassq w3m-file-name-coding-system
				 w3m-charset-coding-system-alist))
		     w3m-file-name-coding-system))
	beg)
    (goto-char (point-min))
    (when (search-forward "<head>" nil t)
      (insert "\n<meta http-equiv=\"CONTENT-TYPE\" "
	      "content=\"text/html; charset="
	      (symbol-name charset)
	      "\">"))
    (goto-char (point-min))
    ;; Remove <form>...</form>
    (when (search-forward "<form " nil t)
      (setq beg (match-beginning 0))
      (when (search-forward "</form>" nil t)
	(delete-region beg (match-end 0)))))
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

(defun w3m-w3m-get-header (url no-cache handler)
  "Return the header string of the URL.
If optional argument NO-CACHE is non-nil, cache is not used."
  (or (unless no-cache
	(w3m-cache-request-header url))
      (lexical-let ((url url))
	(w3m-message "Request sent, waiting for response...")
	(w3m-process-do-with-temp-buffer
	    (success (progn
		       (setq w3m-current-url url)
		       (w3m-process-start handler "-dump_head" url)))
	  (w3m-message "Request sent, waiting for response...done")
	  (when success
	    (w3m-cache-header url (buffer-string)))))))

(defun w3m-w3m-attributes (url no-cache handler)
  "Return a list of attributes of URL.
Value is nil if retrieval of header is failed.  Otherwise, list
elements are:
 0. Type of contents.
 1. Charset of contents.
 2. Size in bytes.
 3. Encoding of contents.
 4. Last modification time.
 5. Real URL.
 6. Base URL.
If optional argument NO-CACHE is non-nil, cache is not used."
  (lexical-let ((url url))
    (w3m-process-do
	(header (w3m-w3m-get-header url no-cache handler))
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
		    (and (memq w3m-type '(w3mmee w3m-m17n))
			 (setq charset
			       (cdr (assoc "w3m-document-charset" alist)))
			 (car (split-string charset))))
		(let ((v (cdr (assoc "content-length" alist))))
		  (and v (setq v (string-to-number v)) (> v 0) v))
		(cdr (assoc "content-encoding" alist))
		(let ((v (cdr (assoc "last-modified" alist))))
		  (and v (w3m-time-parse-string v)))
		(or (cdr (assoc "w3m-current-url" alist))
		    url)
		(or (cdr (assoc "w3m-base-url" alist))
		    (cdr (assoc "w3m-current-url" alist))
		    url))))
       ;; FIXME: adhoc implementation
       ;; HTTP/1.1 500 Server Error on Netscape-Enterprise/3.6
       ;; HTTP/1.0 501 Method Not Implemented
       ((and header (string-match "HTTP/1\\.[0-9] 50[0-9]" header))
	(list "text/html" nil nil nil nil url url))))))

(defmacro w3m-w3m-expand-arguments (arguments)
  (` (delq nil
	   (mapcar
	    (lambda (x)
	      (cond
	       ((stringp x) x)
	       ((setq x (eval x))
		(if (stringp x)
		    x
		  (let (print-level print-length)
		    (prin1-to-string x))))))
	    (, arguments)))))

(defun w3m-w3m-dump-head-source (url handler)
  "Retrive headers and content pointed by URL, and call the HANDLER
function with attributes of the retrieved content when retrieval is
complete."
  (lexical-let ((url url))
    (w3m-message "Reading %s..." url)
    (w3m-process-do
	(result
	 (apply 'w3m-process-start
		handler
		(w3m-w3m-expand-arguments
		 (append w3m-dump-head-source-command-arguments (list url)))))
      (w3m-message "Reading %s...done" url)
      (when result
	(goto-char (point-min))
	(when (let ((case-fold-search t))
		(re-search-forward "^w3m-current-url:" nil t))
	  (delete-region (point-min) (match-beginning 0))
	  (when (search-forward "\n\n" nil t)
	    (w3m-cache-header url (buffer-substring (point-min) (point)) t)
	    (delete-region (point-min) (point))
	    (w3m-cache-contents url (current-buffer))
	    (w3m-w3m-attributes url nil handler)))))))

(defun w3m-w3m-retrieve (url no-decode no-cache post-data referer handler)
  "Retrieve content pointed by URL with w3m, insert it to this buffer,
and call the HANDLER function with its content type as a string
argument, when retrieve is complete."
  (let ((w3m-command-arguments w3m-command-arguments)
	(temp-file))
    (and no-cache
	 w3m-broken-proxy-cache
	 (setq w3m-command-arguments
	       (append w3m-command-arguments '("-o" "no_cache=1"))))
    (when post-data
      (setq temp-file (make-temp-name
		       (expand-file-name "w3mel" w3m-profile-directory)))
      (with-temp-buffer
	(insert (if (consp post-data) (cdr post-data) post-data))
	(let ((modes (default-file-modes)))
	  (unwind-protect
	      (let ((coding-system-for-write 'binary))
		(set-default-file-modes (* 64 6))
		(write-region (point-min) (point-max) temp-file nil 'silent))
	    (set-default-file-modes modes))))
      (setq w3m-command-arguments
	    (append w3m-command-arguments
		    (if (consp post-data)
			(list "-header" (concat "Content-Type: "
						(car post-data))))
		    (list "-post" temp-file))))
    (when (and (stringp referer)
	       (not (and (cdr w3m-add-referer-regexps)
			 (string-match (cdr w3m-add-referer-regexps)
				       referer)))
	       (car w3m-add-referer-regexps)
	       (string-match (car w3m-add-referer-regexps) referer))
      (setq w3m-command-arguments
	    (append w3m-command-arguments
		    (list "-header" (concat "Referer: " referer)))))
    (lexical-let ((url url)
		  (no-decode no-decode)
		  (temp-file temp-file))
      (w3m-process-do
	  (attributes
	   (or (unless no-cache
		 (and (w3m-cache-request-contents url)
		      (w3m-w3m-attributes url nil handler)))
	       (w3m-w3m-dump-head-source url handler)))
	(when temp-file
	  (delete-file temp-file))
	(when attributes
	  (or no-decode
	      (w3m-decode-encoded-contents (nth 3 attributes))
	      (error "Can't decode encoded contents: %s" url))
	  (car attributes))))))

(defsubst w3m-about-retrieve (url &optional no-decode no-cache
				  post-data referer handler)
  "Retrieve content pointed by URL which has about: scheme, insert it
to this buffer."
  (cond
   ((string= "about://emacs-w3m.gif" url)
    (when (fboundp 'base64-decode-string)
      (insert (eval (list 'base64-decode-string
			  w3m-emacs-w3m-icon)))
      "image/gif"))
   ((string-match "\\`about://source/" url)
    (w3m-process-do
	(type (w3m-retrieve (substring url (match-end 0))
			    no-decode no-cache post-data referer handler))
      (when type "text/plain")))
   (t
    (lexical-let ((output-buffer (current-buffer)))
      (w3m-process-do-with-temp-buffer
	  (type (let (func)
		  (setq w3m-current-url url)
		  (set-buffer-multibyte t)
		  (if (and (string-match "\\`about://\\([^/]+\\)/" url)
			   (setq func
				 (intern-soft
				  (concat "w3m-about-" (match-string 1 url))))
			   (fboundp func))
		      (funcall func url no-decode no-cache
			       post-data referer handler)
		    (w3m-about url no-decode no-cache))))
	(when type
	  (encode-coding-region (point-min) (point-max) w3m-coding-system)
	  (set-buffer-multibyte nil)
	  (when (buffer-name output-buffer)
	    (let ((temp-buffer (current-buffer)))
	      (with-current-buffer output-buffer
		(insert-buffer temp-buffer))))
	  type))))))

(defsubst w3m-cid-retrieve (url &optional no-decode no-cache)
  "Retrieve content pointed by URL which has cid: scheme, insert it to
this buffer."
  (let ((func (cdr (assq (with-current-buffer w3m-current-buffer major-mode)
			 w3m-cid-retrieve-function-alist))))
    (when func (funcall func url no-decode no-cache))))

(defun w3m-retrieve (url &optional no-decode no-cache
			 post-data referer handler)
  "Retrieve content pointed by URL, insert it to this buffer, and
returns its content type.
If the argument HANDLER is equal to the other value than nil, this
function returns nil immediately and the specified content is retrived
asynchronously.  The HANDLER function will be called with its content
type as a string argument, when retrieve is complete."
  (if (not handler)
      (condition-case nil
	  (w3m-process-with-wait-handler
	    (w3m-retrieve url no-decode no-cache post-data referer handler))
	(w3m-process-timeout nil))
    (unless (and w3m-safe-url-regexp
		 (not (string-match w3m-safe-url-regexp url)))
      (when (string-match "\\`\\([^#]*\\)#" url)
	(setq url (substring url 0 (match-end 1))))
      (set-buffer-multibyte nil)
      (cond
       ((string-match "\\`about:" url)
	(w3m-about-retrieve url no-decode no-cache post-data referer handler))
       ((string-match "\\`cid:" url)
	(w3m-cid-retrieve url no-decode no-cache))
       ((w3m-url-local-p url)
	(w3m-local-retrieve url no-decode))
       (t
	(w3m-w3m-retrieve url no-decode no-cache post-data referer handler))))))


;;;###autoload
(defun w3m-download (url &optional filename no-cache handler)
  (interactive
   (let* ((url (w3m-input-url
		nil
		(when (stringp w3m-current-url)
		  (if (string-match "\\`about://\\(header\\|source\\)/"
				    w3m-current-url)
		      (substring w3m-current-url (match-end 0))
		    w3m-current-url))))
	  (basename (file-name-nondirectory url)))
     (if (string-match "^[\t ]*$" basename)
	 (error "You should specify the existing file name")
       (list url
	     (w3m-read-file-name (format "Download %s to: " basename)
				 w3m-default-save-directory basename)
	     current-prefix-arg))))
  (if (string-match "\\`ftp://" url)
      (w3m-goto-ftp-url url filename)
    (lexical-let ((filename (or filename (w3m-read-file-name nil nil url))))
      (w3m-process-do-with-temp-buffer
	  (type (progn
		  (w3m-clear-local-variables)
		  (setq w3m-current-url url)
		  (w3m-retrieve url t no-cache nil nil handler)))
	(if type
	    (let ((buffer-file-coding-system 'binary)
		  (file-coding-system 'binary)
		  (coding-system-for-write 'binary)
		  jka-compr-compression-info-list
		  jam-zcat-filename-list
		  format-alist)
	      (when (or (not (file-exists-p filename))
			(y-or-n-p (format "File(%s) already exists. Overwrite? "
					  filename)))
		(write-region (point-min) (point-max) filename)
		t))
	  (ding)
	  (w3m-message "Cannot retrieve URL: %s%s"
		       url
		       (if w3m-process-exit-status
			   (format " (exit status: %s)" w3m-process-exit-status)
			 ""))
	  nil)))))

;;; Retrieve data:
(eval-and-compile
  (defconst w3m-internal-characters-alist
    '((?\x90 . ? )			; ANSP (use for empty anchor)
      (?\x91 . ? )			; IMSP (blank around image)
      (?\xa0 . ? ))			; NBSP (non breakble space)
    "Alist of internal characters v.s. ASCII characters."))

(eval-and-compile
  (defun w3m-ccl-write-repeat (charset &optional r0 r1)
    (unless r0
      (setq r0 'r0))
    (unless r1
      (setq r1 (if (eq r0 'r1) 'r0 'r1)))
    (let* ((spec (cdr
		  (assq charset
			'((latin-iso8859-1 .   (nil . lc-ltn1))
			  (japanese-jisx0208 . (t   . lc-jp))
			  (japanese-jisx0212 . (t   . lc-jp2))
			  (katakana-jisx0201 . (nil . lc-kana))))))
	   (id (eval (if (boundp 'MULE)
			 (cdr spec)
		       '(charset-id charset)))))
      (if (fboundp 'ccl-compile-write-multibyte-character)
	  (` (((, r1) &= ?\x7f)
	      (,@ (when (car spec)
		    (` (((, r1) |= (((, r0) & ?\x7f) << 7))))))
	      ((, r0) = (, id))
	      (write-multibyte-character (, r0) (, r1))
	      (repeat)))
	(` ((write (, id))
	    (,@ (when (car spec)
		  (` ((write (, r0))))))
	    (write-repeat (, r1))))))))

(define-ccl-program w3m-euc-japan-decoder
  (` (2
      (loop
       (read r0)
       ;; Process normal EUC characters.
       (if (r0 < ?\x80)
	   (write-repeat r0))
       (if (r0 > ?\xa0)
	   ((read r1)
	    (,@ (w3m-ccl-write-repeat 'japanese-jisx0208))))
       (if (r0 == ?\x8e)
	   ((read r1)
	    (,@ (w3m-ccl-write-repeat 'katakana-jisx0201))))
       (if (r0 == ?\x8f)
	   ((read r0)
	    (read r1)
	    (,@ (w3m-ccl-write-repeat 'japanese-jisx0212))))
       ;; Process internal characters used in w3m.
       (,@ (mapcar (lambda (pair)
		     (` (if (r0 == (, (car pair)))
			    (write-repeat (, (cdr pair))))))
		   w3m-internal-characters-alist))
       (write-repeat r0)))))

(unless (get 'w3m-euc-japan-encoder 'ccl-program-idx)
  (define-ccl-program w3m-euc-japan-encoder
    (` (1 (loop (read r0) (write-repeat r0))))))

(w3m-make-ccl-coding-system
 'w3m-euc-japan ?E
 "ISO 2022 based EUC encoding for Japanese with w3m internal characters.
  (generated by `w3m')"
 'w3m-euc-japan-decoder
 'w3m-euc-japan-encoder)

(define-ccl-program w3m-iso-latin-1-decoder
  (` (2
      (loop
       (read r0)
       ;; Process ASCII characters.
       (if (r0 < ?\x80)
	   (write-repeat r0))
       ;; Process Latin-1 characters.
       (if (r0 > ?\xa0)
	   ((,@ (w3m-ccl-write-repeat 'latin-iso8859-1 'r1))))
       ;; Process internal characters used in w3m.
       (,@ (mapcar (lambda (pair)
		     (` (if (r0 == (, (car pair)))
			    (write-repeat (, (cdr pair))))))
		   w3m-internal-characters-alist))
       (write-repeat r0)))))

(unless (get 'w3m-iso-latin-1-encoder 'ccl-program-idx)
  (define-ccl-program w3m-iso-latin-1-encoder
    (` (1 (loop (read r0) (write-repeat r0))))))

(w3m-make-ccl-coding-system
 'w3m-iso-latin-1 ?1
 "ISO 2022 based 8-bit encoding for Latin-1 with w3m internal characters.
  (generated by `w3m')"
 'w3m-iso-latin-1-decoder
 'w3m-iso-latin-1-encoder)

(defun w3m-remove-comments ()
  "Remove HTML comments in the current buffer."
  (goto-char (point-min))
  (let (beg)
    (while (search-forward "<!--" nil t)
      (setq beg (match-beginning 0))
      (if (search-forward "-->" nil t)
	  (delete-region beg (point))))))

(defun w3m-check-link-tags ()
  "Process <LINK> tags in the current buffer."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (when (search-forward "</head>" nil t)
      (save-restriction
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(while (re-search-forward "<link[ \t\r\f\n]+" nil t)
	  (w3m-parse-attributes ((rel :case-ignore) href)
	    (when rel
	      (setq rel (split-string rel))
	      (cond
	       ((member "next" rel) (setq w3m-next-url href))
	       ((or (member "prev" rel) (member "previous" rel))
		(setq w3m-previous-url href))
	       ((member "start" rel) (setq w3m-start-url href))
	       ((member "contents" rel) (setq w3m-contents-url href))))))))))

(defun w3m-remove-meta-charset-tags ()
  (let ((case-fold-search t))
    (goto-char (point-min))
    (when (or (re-search-forward
	       w3m-meta-content-type-charset-regexp nil t)
	      (re-search-forward
	       w3m-meta-charset-content-type-regexp nil t))
      (delete-region (match-beginning 0) (match-end 0)))))

(defsubst w3m-rendering-extract-title ()
  "Extract the title from the half-dumped data in this buffer."
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
    (or title "<no-title>")))

(defsubst w3m-rendering-half-dump (&optional charset)
  (let ((coding-system-for-read w3m-output-coding-system)
	(coding-system-for-write w3m-input-coding-system)
	(default-process-coding-system
	  (cons w3m-output-coding-system w3m-input-coding-system)))
    (w3m-process-with-environment w3m-command-environment
      (apply 'call-process-region
	     (point-min)
	     (point-max)
	     (or w3m-halfdump-command w3m-command)
	     t t nil
	     (w3m-w3m-expand-arguments
	      (append w3m-halfdump-command-arguments
		      w3m-halfdump-command-common-arguments))))))

(defun w3m-rendering-buffer-1 (&optional content-charset binary-buffer)
  (w3m-message "Rendering...")
  (when w3m-use-filter (w3m-filter w3m-current-url))
  (w3m-remove-comments)
  (w3m-check-link-tags)
  (when w3m-use-form (setq w3m-current-forms (w3m-form-parse-buffer)))
  (w3m-remove-meta-charset-tags)
  (if binary-buffer
      (progn
	(delete-region (point-min) (point-max))
	(insert-buffer
	 (with-current-buffer binary-buffer
	   (w3m-rendering-half-dump content-charset)
	   (current-buffer)))
	(w3m-kill-buffer binary-buffer))
    (w3m-rendering-half-dump))
  (w3m-message "Rendering...done")
  (w3m-rendering-extract-title))

(defun w3m-rendering-unibyte-buffer (url &optional content-charset)
  "Do rendering of contents in this buffer as HTML and return title."
  (let (binary-buffer)
    (when (memq w3m-type '(w3mmee w3m-m17n))
      (let ((original-buffer (current-buffer)))
	(with-current-buffer
	    (setq binary-buffer
		  (w3m-get-buffer-create
		   (generate-new-buffer-name w3m-work-buffer-name)))
	  (set-buffer-multibyte nil)
	  (insert-buffer original-buffer)
	  (set-buffer-multibyte t)
	  (w3m-copy-local-variables original-buffer))))
    (w3m-decode-buffer url content-charset "text/html")
    (w3m-rendering-buffer-1 content-charset binary-buffer)))

(defun w3m-rendering-multibyte-buffer ()
  "Do rendering of contents in this buffer as HTML and return title."
  (let (binary-buffer)
    (when (memq w3m-type '(w3mmee w3m-m17n))
      (let ((original-buffer (current-buffer)))
	(with-current-buffer
	    (setq binary-buffer
		  (w3m-get-buffer-create
		   (generate-new-buffer-name w3m-work-buffer-name)))
	  (set-buffer-multibyte t)
	  (insert-buffer original-buffer)
	  (encode-coding-region (point-min) (point-max) w3m-coding-system)
	  (w3m-copy-local-variables original-buffer))))
    (w3m-rendering-buffer-1 w3m-coding-system binary-buffer)))

(defun w3m-retrieve-and-render (url &optional no-cache content-charset
				    content-type post-data referer handler)
  "Insert content pointed by URL to this buffer, render it, and return
a `w3m-process' object immediately.  The HANDLER function will be
called when rendering is complete.  When new content is retrieved in
this buffer, the HANDLER function will be called with t as an
argument.  Otherwise, it will be called with nil."
  (lexical-let ((url url)
		(content-type content-type)
		(content-charset content-charset)
		(output-buffer (current-buffer)))
    (w3m-process-do-with-temp-buffer
	(type (progn
		(w3m-clear-local-variables)
		(setq w3m-current-url url)
		(w3m-retrieve url nil no-cache post-data referer handler)))
      (when (buffer-live-p output-buffer)
	(if type
	    (prog1 (w3m-prepare-content url (or content-type type)
					output-buffer content-charset)
	      (and w3m-verbose
		   (not (get-buffer-window output-buffer))
		   (message "The content (%s) has been retrieved in %s"
			    url (buffer-name output-buffer))))
	  (ding)
	  (w3m-message "Cannot retrieve URL: %s%s"
		       url
		       (if w3m-process-exit-status
			   (format " (exit status: %s)"
				   w3m-process-exit-status)
			 ""))
	  nil)))))

(defun w3m-prepare-content (url type output-buffer &optional content-charset)
  (cond
   ((string-match "\\`text/" type)
    (setq w3m-current-url (w3m-real-url url)
	  w3m-current-base-url (w3m-base-url url)
	  w3m-current-title
	  (if (string= "text/html" type)
	      (w3m-rendering-unibyte-buffer url content-charset)
	    (w3m-decode-buffer url content-charset type)
	    (or (when (string-match "\\`about://\\(source\\|header\\)/" url)
		  (w3m-arrived-title (substring url (match-end 0))))
		(file-name-nondirectory url))))
    (let ((result-buffer (current-buffer)))
      (with-current-buffer output-buffer
	(let (buffer-read-only)
	  (widen)
	  (delete-region (point-min) (point-max))
	  (insert-buffer result-buffer)
	  (w3m-copy-local-variables result-buffer)
	  (set-buffer-file-coding-system w3m-current-coding-system)
	  (when (string= "text/html" type) (w3m-fontify))
	  t))))
   ((and (w3m-image-type-available-p (w3m-image-type type))
	 (string-match "\\`image/" type))
    (with-current-buffer output-buffer
      (let (buffer-read-only)
	(w3m-clear-local-variables)
	(setq w3m-current-url (w3m-real-url url)
	      w3m-current-title (file-name-nondirectory url)
	      w3m-image-only-page t)
	(widen)
	(delete-region (point-min) (point-max))
	(insert w3m-current-title)
	(w3m-add-text-properties (point-min) (point-max)
				 (list 'face 'w3m-image-face
				       'w3m-image url
				       'mouse-face 'highlight))
	t)))
   (t (with-current-buffer output-buffer
	(w3m-external-view url))
      nil)))

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

(defun w3m-view-previous-page (&optional count)
  "View previous page.  If COUNT is a positive integer, move backward
COUNT times in the history.  If COUNT is a negative integer, moving
forward is performed.  Otherwise, COUNT is treated as 1 by default."
  (interactive "p")
  (let ((url (if (and (integerp count)
		      (not (zerop count)))
		 (car (w3m-history-backward count))
	       (w3m-history-backward)))
	(w3m-use-refresh nil))
    (when url
      (w3m-goto-url url nil nil
		    (w3m-history-plist-get :post-data nil nil t)
		    (w3m-history-plist-get :referer nil nil t))
      ;; restore last position
      (w3m-history-restore-position url))))

(defun w3m-view-next-page (&optional count)
  "View next page.  If COUNT is a positive integer, move forward COUNT
times in the history.  If COUNT is a negative integer, moving backward
is performed.  Otherwise, COUNT is treated as 1 by default."
  (interactive "p")
  (w3m-view-previous-page (if (integerp count) (- count) -1)))

(unless (fboundp 'w3m-expand-path-name)
  (if (string-match "\\`.:" (expand-file-name "/"))
      ;; Avoid incompatibility of drive letter.
      (defun w3m-expand-path-name (name &optional base)
	"Convert path string NAME to the canonicalized one."
	(let ((x (expand-file-name name base)))
	  (if (string-match "\\`.:" x)
	      (substring x (match-end 0))
	    x)))
    (defalias 'w3m-expand-path-name 'expand-file-name)))

(defconst w3m-url-components-regexp
  "\\`\\(\\([^:/?#]+\\):\\)?\\(//\\([^/?#]*\\)\\)?\\([^?#]*\\)\\(\\?\\([^#]*\\)\\)?\\(#\\(.*\\)\\)?\\'"
  "Regular expression for parsing the potential four components and
fragment identifier of a URI reference.  For more detail, see Appendix
B of RFC2396 <URL:http://www.ietf.org/rfc/rfc2396.txt>.")

(defconst w3m-url-hierarchical-schemes
  '("http" "https" "ftp" "file")
  "List of schemes which may have hierarchical parts.  This list is
refered in `w3m-expand-url' to keep backward compatibility which is
described in Section 5.2 of RFC 2396.")

(defconst w3m-url-fallback-base "http:///")

(defun w3m-expand-url (url &optional base)
  "Convert URL to absolute, and canonicalize it."
  (save-match-data
    (if base
	(if (and (string-match w3m-url-components-regexp base)
		 (match-beginning 1))
	    (and (not (match-beginning 3))
		 (member (match-string 2 base) w3m-url-hierarchical-schemes)
		 (setq base (concat
			     (substring base 0 (match-end 1))
			     "//"
			     (substring base (match-beginning 5)))))
	  (error "BASE must have a scheme part: %s" base))
      (setq base (or w3m-current-base-url
		     w3m-current-url
		     w3m-url-fallback-base)))
    (string-match w3m-url-components-regexp url)
    ;; Remove an empty fragment part.
    (when (and (match-beginning 8)
	       (= (match-beginning 9) (length url)))
      (setq url (substring url 0 (match-beginning 8)))
      (string-match w3m-url-components-regexp url))
    ;; Remove an empty query part.
    (when (and (match-beginning 6)
	       (= (match-beginning 7) (length url)))
      (setq url (substring url 0 (match-beginning 6)))
      (string-match w3m-url-components-regexp url))
    (cond
     ((match-beginning 1)
      ;; URL has a scheme part. => URL may have an absolute spec.
      (if (or (match-beginning 3)
	      (and (< (match-beginning 5) (length url))
		   (eq ?/ (aref url (match-beginning 5)))))
	  ;; URL has a net-location part or a absolute hierarchical
	  ;; part. => URL has an absolute spec.
	  url
	(let ((scheme (match-string 2 url)))
	  (if (and (member scheme w3m-url-hierarchical-schemes)
		   (string-match w3m-url-components-regexp base)
		   (equal scheme (match-string 2 base)))
	      (w3m-expand-url (substring url (match-end 1)) base)
	    url))))
     ((match-beginning 3)
      ;; URL has a net-location part. => The hierarchical part of URL
      ;; has an absolute spec.
      (string-match w3m-url-components-regexp base)
      (concat (substring base 0 (match-end 1)) url))
     ((> (match-end 5) (match-beginning 5))
      ;; URL has a hierarchical part.
      (if (eq ?/ (aref url (match-beginning 5)))
	  ;; Its first character is the slash "/". => The hierarchical
	  ;; part of URL has an absolute spec.
	  (progn
	    (string-match w3m-url-components-regexp base)
	    (concat (substring base 0 (or (match-end 3) (match-end 1)))
		    url))
	;; The hierarchical part of URL has a relative spec.
	(let ((path-end (match-end 5)))
	  (string-match w3m-url-components-regexp base)
	  (concat
	   (substring base 0 (match-beginning 5))
	   (if (member (match-string 2 base) w3m-url-hierarchical-schemes)
	       (w3m-expand-path-name
		(substring url 0 path-end)
		(file-name-directory (match-string 5 base)))
	     (substring url 0 path-end))
	   (substring url path-end)))))
     ((match-beginning 6)
      ;; URL has a query part.
      (string-match w3m-url-components-regexp base)
      (concat (file-name-directory (substring base 0 (match-end 5)))
	      url))
     (t
      ;; URL has only fragment part.
      (string-match w3m-url-components-regexp base)
      (concat (substring base 0 (match-beginning 8))
	      url)))))

(defun w3m-view-this-url (&optional arg new-session)
  "View the URL of the link under point.  If ARG is the number 2 or the
list of the number 16 (you may produce this by typing `C-u' twice) or
NEW-SESSION is non-nil, and the link is an anchor, make a copy of the
current session in advance.  Otherwise, if ARG is non-nil, force
reload the url at point.  If the option `w3m-pop-up-frames' is non-nil,
also make a new frame for the copied session."
  (interactive (if (member current-prefix-arg '(2 (16)))
		   (list nil t)
		 (list current-prefix-arg nil)))
  (let ((url (w3m-anchor))
	(act (w3m-action)))
    (cond
     (url
      (lexical-let (pos)
	(when new-session
	  (setq pos (point-marker))
	  (switch-to-buffer (w3m-copy-buffer nil nil t 'empty))
	  ;; When new URL has `name' portion, we have to goto the base url
	  ;; because generated buffer has no content at this moment.
	  (when (and (string-match w3m-url-components-regexp url)
		     (match-beginning 8))
	    (let ((name (match-string 9 url))
		  (url (substring url 0 (match-beginning 8))))
	      (w3m-goto-url url arg nil nil w3m-current-url))))
	(let (handler)
	  (w3m-process-do
	      (success (w3m-goto-url url arg nil nil w3m-current-url handler))
	    ;; FIXME: 本当は w3m-goto-url() が適当な返り値を返すように
	    ;; 変更して、その値を検査するべきだ
	    (when (and pos (buffer-name (marker-buffer pos)))
	      (save-excursion
		(set-buffer (marker-buffer pos))
		(goto-char pos)
		(w3m-refontify-anchor)))))))
     (act (eval act))
     ((w3m-image)
      (if (w3m-display-graphic-p)
	  (w3m-toggle-inline-image)
	(w3m-view-image)))
     (t (message "No URL at point")))))

(defun w3m-mouse-view-this-url (event &optional arg)
  "Perform the command `w3m-view-this-url' by the mouse event."
  (interactive "e\nP")
  (mouse-set-point event)
  (w3m-view-this-url arg))

(defun w3m-view-this-url-new-session (&optional arg)
  "Perform the command `w3m-view-this-url' in the new session."
  (interactive "P")
  (w3m-view-this-url arg t))

(defun w3m-mouse-view-this-url-new-session (event &optional arg)
  "Perform the command `w3m-view-this-url' by the mouse event in the new
session."
  (interactive "e\nP")
  (mouse-set-point event)
  (w3m-view-this-url arg t))

(defun w3m-submit-form ()
  "Submit form at point."
  (interactive)
  (let ((submit (w3m-submit)))
    (if submit
	(eval submit)
      (message "Can't Submit at this point"))))

(defun w3m-external-view (url &optional no-cache handler)
  (lexical-let ((url url)
		(no-cache no-cache))
    (w3m-process-do
	(type (w3m-content-type url no-cache handler))
      (when type
	(lexical-let ((method (nth 2 (assoc type w3m-content-type-alist))))
	  (cond
	   ((not method)
	    (if (w3m-url-local-p url)
		(error "No method to view `%s' is registered. Use `w3m-edit-this-url'"
		       (file-name-nondirectory (w3m-url-to-file-name url)))
	      (w3m-download url nil no-cache handler)))
	   ((functionp method)
	    (funcall method url))
	   ((consp method)
	    (lexical-let
		((command (w3m-which-command (car method)))
		 (arguments (cdr method))
		 (file (make-temp-name
			(expand-file-name "w3mel" w3m-profile-directory)))
		 suffix)
	      (setq suffix (file-name-nondirectory url))
	      (when (string-match "\\.[a-zA-Z0-9]+$" suffix)
		(setq suffix (match-string 0 suffix))
		(when (< (length suffix) 5)
		  (setq file (concat file suffix))))
	      (cond
	       ((and command (memq 'file arguments))
		(w3m-process-do
		    (success (w3m-download url file no-cache handler))
		  (when success
		    (w3m-external-view-file command file url arguments))))
	       (command
		(w3m-external-view-file command nil url arguments))
	       (t
		(w3m-download url nil no-cache handler)))))))))))

(defun w3m-external-view-file (command file url arguments)
  ;; The 3rd argument `url' is necessary to handle the constant `url'
  ;; included in the 4th argument `arguments' which is provided by
  ;; `w3m-content-type-alist'.
  (lexical-let ((file file))
    (let (proc)
      (unwind-protect
	  (with-current-buffer
	      (generate-new-buffer " *w3m-external-view*")
	    (setq proc
		  (apply 'start-process
			 "w3m-external-view"
			 (current-buffer)
			 command
			 (mapcar (function eval) arguments)))
	    (w3m-message "Start %s..." (file-name-nondirectory command))
	    (set-process-sentinel
	     proc
	     (lambda (proc event)
	       (and (string-match "^\\(finished\\|exited\\)" event)
		    (buffer-name (process-buffer proc))
		    (with-current-buffer (process-buffer proc)
		      (and (stringp file)
			   (file-exists-p file)
			   (delete-file file)))
		    (kill-buffer (process-buffer proc))))))
	(and (stringp file)
	     (file-exists-p file)
	     (unless (and (processp proc)
			  (memq (process-status proc) '(run stop)))
	       (delete-file file)))))))

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

(defun w3m-view-url-with-external-browser ()
  "View this URL."
  (interactive)
  (let ((url (or (w3m-anchor)
		 (unless w3m-display-inline-images
		   (w3m-image))
		 (when (y-or-n-p (format "Browse <%s> ? " w3m-current-url))
		   w3m-current-url))))
    (when url
      (message "Browsing <%s>..." url)
      (w3m-external-view url))))

(defun w3m-download-this-url ()
  "Download the file or the image which pointed by the link under cursor."
  (interactive)
  (let ((url (or (w3m-anchor) (w3m-image))))
    (if url
	(lexical-let ((pos (point-marker))
		      (curl w3m-current-url))
	  (w3m-process-with-null-handler
	    (w3m-process-do
		(success (w3m-download url nil nil handler))
	      (and success
		   (buffer-name (marker-buffer pos))
		   (save-excursion
		     (set-buffer (marker-buffer pos))
		     (when (equal curl w3m-current-url)
		       (goto-char pos)
		       (w3m-refontify-anchor)))))))
      (message "No URL at point"))))

(defun w3m-print-current-url ()
  "Print the URL of current page and push it into kill-ring."
  (interactive)
  (kill-new w3m-current-url)
  (message "%s" w3m-current-url))

(defun w3m-print-this-url (&optional arg)
  "Print the URL of the link under point."
  (interactive (list t))
  (let ((url (or (w3m-anchor) (w3m-image))))
    (when (or url arg)
      (and url arg (kill-new url))
      (message "%s"
	       (or url
		   (and (w3m-action) "There is a form")
		   "There is no url")))))

(defun w3m-edit-url (url)
  "Edit the local file pointed by URL."
  (when (string-match "\\`about://\\(header\\|source\\)/" url)
    (setq url (substring url (match-end 0))))
  (funcall w3m-edit-function
	   (or (w3m-url-to-file-name url)
	       (error "URL:%s is not a local file" url))))

(defun w3m-edit-current-url ()
  "Edit the local file pointed by URL of current page."
  (interactive)
  (w3m-edit-url w3m-current-url))

(defun w3m-edit-this-url (&optional url)
  "Edit the local file pointed by URL under point."
  (interactive)
  (unless url
    (setq url (w3m-anchor)))
  (if url
      (w3m-edit-url url)
    (message "No URL at point")))

(defvar w3m-goto-anchor-hist nil)
(make-variable-buffer-local 'w3m-goto-anchor-hist)

(defun w3m-goto-next-anchor ()
  (let ((hseq (w3m-anchor-sequence))
	(pos (next-single-property-change (point) 'w3m-anchor-sequence)))
    (if (or (not hseq) (< hseq 1))
	(and pos (goto-char pos))
      (setq pos
	    ;; hseq is not sequence in form.
	    (catch 'loop
	      (setq hseq (1+ hseq))
	      (while (<= hseq w3m-max-anchor-sequence)
		(setq pos (text-property-any
			   (point-min) (point-max) 'w3m-anchor-sequence hseq))
		(when pos (throw 'loop pos))
		(setq hseq (1+ hseq)))))
      (and pos (goto-char pos)))))

(defun w3m-next-anchor (&optional arg)
  "Move cursor to the next anchor."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (null (memq last-command '(w3m-next-anchor w3m-previous-anchor)))
      (setq w3m-goto-anchor-hist (list (w3m-anchor-sequence)))
    (if (and (eq last-command 'w3m-previous-anchor) w3m-goto-anchor-hist)
	(setcdr w3m-goto-anchor-hist nil)))
  (if (< arg 0)
      (w3m-previous-anchor (- arg))
    (while (> arg 0)
      (unless (w3m-goto-next-anchor)
	;; search from the beginning of the buffer
	(setq w3m-goto-anchor-hist nil)
	(goto-char (point-min))
	(w3m-goto-next-anchor))
      (setq arg (1- arg))
      (if (member (w3m-anchor-sequence) w3m-goto-anchor-hist)
	  (setq arg (1+ arg))
	(setq w3m-goto-anchor-hist
	      (cons (w3m-anchor-sequence) w3m-goto-anchor-hist))))
    (w3m-print-this-url)))

(defun w3m-goto-previous-anchor ()
  (let ((hseq (w3m-anchor-sequence))
	(pos (previous-single-property-change (point) 'w3m-anchor-sequence)))
    (cond
     ((and (not hseq) pos)
      (if (w3m-anchor-sequence pos)
	  (goto-char pos)
	(setq pos (previous-single-property-change pos 'w3m-anchor-sequence))
	(and pos (goto-char pos))))
     ((or (not pos) (< hseq 2)) nil)
     (t
      (setq pos
	    ;; hseq is not sequence in form.
	    (catch 'loop
	      (setq hseq (1- hseq))
	      (while (> hseq 0)
		(setq pos (text-property-any
			   (point-min) (point-max) 'w3m-anchor-sequence hseq))
		(when pos (throw 'loop pos))
		(setq hseq (1- hseq)))))
      (and pos (goto-char pos))))))

(defun w3m-previous-anchor (&optional arg)
  "Move cursor to the previous anchor."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (null (memq last-command '(w3m-next-anchor w3m-previous-anchor)))
      (setq w3m-goto-anchor-hist (list (w3m-anchor-sequence)))
    (if (and (eq last-command 'w3m-next-anchor) w3m-goto-anchor-hist)
	(setcdr w3m-goto-anchor-hist nil)))
  (if (< arg 0)
      (w3m-next-anchor (- arg))
    (while (> arg 0)
      (unless (w3m-goto-previous-anchor)
	;; search from the end of the buffer
	(setq w3m-goto-anchor-hist nil)
	(goto-char (point-max))
	(w3m-goto-previous-anchor))
      (setq arg (1- arg))
      (if (member (w3m-anchor-sequence) w3m-goto-anchor-hist)
	  (setq arg (1+ arg))
	(setq w3m-goto-anchor-hist
	      (cons (w3m-anchor-sequence) w3m-goto-anchor-hist))))
    (w3m-print-this-url)))

(defun w3m-goto-next-form ()
  ;; move to the end of the current form
  (when (w3m-action (point))
    (goto-char (next-single-property-change (point) 'w3m-action)))
  ;; find the next form
  (or (w3m-action (point))
      (let ((pos (next-single-property-change (point) 'w3m-action)))
	(when pos
	  (goto-char pos)
	  t))))

(defun w3m-next-form (&optional arg)
  "Move cursor to the next form."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (null (memq last-command '(w3m-next-form w3m-previous-form)))
      (setq w3m-goto-anchor-hist
	    (list (get-text-property (point) 'w3m-action)))
    (if (and (eq last-command 'w3m-previous-form) w3m-goto-anchor-hist)
	(setcdr w3m-goto-anchor-hist nil)))
  (if (< arg 0)
      (w3m-previous-form (- arg))
    (while (> arg 0)
      (unless (w3m-goto-next-form)
	;; search from the beginning of the buffer
	(setq w3m-goto-anchor-hist nil)
	(goto-char (point-min))
	(w3m-goto-next-form))
      (setq arg (1- arg))
      (if (member (w3m-action) w3m-goto-anchor-hist)
	  (setq arg (1+ arg))
	(setq w3m-goto-anchor-hist
	      (cons (get-text-property (point) 'w3m-action)
		    w3m-goto-anchor-hist))))
    (w3m-print-this-url)))

(defun w3m-goto-previous-form ()
  ;; move to the beginning of the current form
  (when (w3m-action (point))
    (goto-char (previous-single-property-change (1+ (point))
						'w3m-action)))
  ;; find the previous form
  (let ((pos (previous-single-property-change (point) 'w3m-action)))
    (if pos
	(goto-char
	 (if (w3m-action pos) pos
	   (previous-single-property-change pos 'w3m-action))))))

(defun w3m-previous-form (&optional arg)
  "Move cursor to the previous form."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (null (memq last-command '(w3m-next-form w3m-previous-form)))
      (setq w3m-goto-anchor-hist
	    (list (get-text-property (point) 'w3m-action)))
    (if (and (eq last-command 'w3m-next-form) w3m-goto-anchor-hist)
	(setcdr w3m-goto-anchor-hist nil)))
  (if (< arg 0)
      (w3m-next-form (- arg))
    (while (> arg 0)
      (unless (w3m-goto-previous-form)
	;; search from the end of the buffer
	(setq w3m-goto-anchor-hist nil)
	(goto-char (point-max))
	(w3m-goto-previous-form))
      (setq arg (1- arg))
      (if (member (w3m-action) w3m-goto-anchor-hist)
	  (setq arg (1+ arg))
	(setq w3m-goto-anchor-hist
	      (cons (get-text-property (point) 'w3m-action)
		    w3m-goto-anchor-hist))))
    (w3m-print-this-url)))

(defun w3m-goto-next-image ()
  ;; move to the end of the current image
  (when (w3m-image (point))
    (goto-char (next-single-property-change (point) 'w3m-image)))
  ;; find the next form or image
  (or (w3m-image (point))
      (let ((pos (next-single-property-change (point) 'w3m-image)))
	(when pos
	  (goto-char pos)
	  t))))

(defun w3m-next-image (&optional arg)
  "Move cursor to the next image."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (null (memq last-command
		  '(w3m-next-image w3m-previous-image)))
      (setq w3m-goto-anchor-hist
	    (list (get-text-property (point) 'w3m-image)))
    (if (and (eq last-command 'w3m-previous-image) w3m-goto-anchor-hist)
	(setcdr w3m-goto-anchor-hist nil)))
  (if (< arg 0)
      (w3m-previous-image (- arg))
    (while (> arg 0)
      (unless (w3m-goto-next-image)
	;; search from or image the beginning of the buffer
	(setq w3m-goto-anchor-hist nil)
	(goto-char (point-min))
	(w3m-goto-next-image))
      (setq arg (1- arg))
      (if (member (w3m-image) w3m-goto-anchor-hist)
	  (setq arg (1+ arg))
	(setq w3m-goto-anchor-hist
	      (cons (get-text-property (point) 'w3m-image)
		    w3m-goto-anchor-hist))))
    (w3m-print-this-url)))

(defun w3m-goto-previous-image ()
  ;; move to the beginning of the current image
  (when (w3m-image (point))
    (goto-char (previous-single-property-change (1+ (point))
						'w3m-image)))
  ;; find the previous form or image
  (let ((pos (previous-single-property-change (point) 'w3m-image)))
    (if pos
	(goto-char
	 (if (w3m-image pos) pos
	   (previous-single-property-change pos 'w3m-image))))))

(defun w3m-previous-image (&optional arg)
  "Move cursor to the previous image."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (null (memq last-command
		  '(w3m-next-image w3m-previous-image)))
      (setq w3m-goto-anchor-hist
	    (list (get-text-property (point) 'w3m-image)))
    (if (and (eq last-command 'w3m-next-image) w3m-goto-anchor-hist)
	(setcdr w3m-goto-anchor-hist nil)))
  (if (< arg 0)
      (w3m-next-image (- arg))
    (while (> arg 0)
      (unless (w3m-goto-previous-image)
	;; search from the end of the buffer
	(setq w3m-goto-anchor-hist nil)
	(goto-char (point-max))
	(w3m-goto-previous-image))
      (setq arg (1- arg))
      (if (member (w3m-image) w3m-goto-anchor-hist)
	  (setq arg (1+ arg))
	(setq w3m-goto-anchor-hist
	      (cons (get-text-property (point) 'w3m-image)
		    w3m-goto-anchor-hist))))
    (w3m-print-this-url)))

(defun w3m-copy-buffer (&optional buf newname and-pop empty)
  "Create a copy of the buffer BUF which defaults to the current buffer.
If NEWNAME is nil, it defaults to the current buffer's name.
If AND-POP is non-nil, the new buffer is shown with `pop-to-buffer',
that is affected by `w3m-pop-up-frames'.
If EMPTY is non-nil, the created buffer has empty content."
  (interactive (list (current-buffer)
		     (if current-prefix-arg (read-string "Name: "))
		     t))
  (when (and w3m-pop-up-windows
	     (not (string= (buffer-name) w3m-select-buffer-name))
	     (get-buffer-window w3m-select-buffer-name))
    ;; w3m-select-buffer too thin
    (delete-windows-on (get-buffer w3m-select-buffer-name)))
  (unless buf
    (setq buf (current-buffer)))
  (unless newname
    (setq newname (buffer-name buf)))
  (when (string-match "<[0-9]+>\\'" newname)
    (setq newname (substring newname 0 (match-beginning 0))))
  (with-current-buffer buf
    (let ((url w3m-current-url)
	  (images w3m-display-inline-images)
	  (mode major-mode)
	  (new (generate-new-buffer newname)))
      (with-current-buffer new
	(w3m-mode)
	(if w3m-toggle-inline-images-permanently
	    (setq w3m-display-inline-images images)
	  (setq w3m-display-inline-images w3m-default-display-inline-images))
	(unless empty
	  (w3m-goto-url url))
	;; Make copies of `w3m-history' and `w3m-history-flat'.
	(w3m-history-copy buf)
	(when empty
	  (w3m-clear-local-variables))
	(when and-pop
	  (let* ((pop-up-windows w3m-pop-up-windows)
		 (pop-up-frames w3m-pop-up-frames)
		 (pop-up-frame-alist (w3m-pop-up-frame-parameters))
		 (pop-up-frame-plist pop-up-frame-alist)
		 (oframe (selected-frame)))
	    (if pop-up-windows
		(pop-to-buffer new)
	      (switch-to-buffer new))
	    (unless (eq oframe (selected-frame))
	      (setq w3m-initial-frame (selected-frame)))))
	new))))

(defun w3m-next-buffer ()
  "Switch to next w3m buffer."
  (interactive)
  (let ((buffers (w3m-list-buffers)))
    (switch-to-buffer
     (or (cadr (memq (current-buffer) buffers))
	 (car buffers)))))

(defun w3m-previous-buffer ()
  "Switch to previous w3m buffer."
  (interactive)
  (let ((buffers (nreverse (w3m-list-buffers))))
    (switch-to-buffer
     (or (cadr (memq (current-buffer) buffers))
	 (car buffers)))))

(defun w3m-delete-buffer (&optional force)
  "Delete w3m buffer and switch to previous w3m buffer if exists."
  (interactive "P")
  (if (= 1 (length (w3m-list-buffers t)))
      (w3m-quit force)
    (let ((buffer (current-buffer)))
      (w3m-previous-buffer)
      (kill-buffer buffer))
    (w3m-select-buffer-update)))

(defvar w3m-lynx-like-map nil
  "Lynx-like keymap used in w3m-mode buffers.")
(unless w3m-lynx-like-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map " " 'w3m-scroll-up-or-next-url)
    (define-key map "b" 'w3m-scroll-down-or-previous-url)
    (define-key map [backspace] 'w3m-scroll-down-or-previous-url)
    (define-key map [delete] 'w3m-scroll-down-or-previous-url)
    (if (featurep 'xemacs)
	(define-key map [(shift space)] 'w3m-scroll-down-or-previous-url)
      ;; Note: It does not have an effect on Emacs 19.
      (define-key map [?\S-\ ] 'w3m-scroll-down-or-previous-url))
    (define-key map "h" 'backward-char)
    (define-key map "j" 'next-line)
    (define-key map "k" 'previous-line)
    (define-key map "l" 'forward-char)
    (define-key map "J" (lambda () (interactive) (scroll-down 1)))
    (define-key map "K" (lambda () (interactive) (scroll-up 1)))
    (define-key map "\M-g" 'goto-line)
    (define-key map "\C-?" 'w3m-scroll-down-or-previous-url)
    (define-key map "\t" 'w3m-next-anchor)
    (define-key map [(shift tab)] 'w3m-previous-anchor)
    (define-key map [(shift iso-lefttab)] 'w3m-previous-anchor)
    (define-key map [down] 'w3m-next-anchor)
    (define-key map "\M-\t" 'w3m-previous-anchor)
    (define-key map [up] 'w3m-previous-anchor)
    (define-key map "\C-m" 'w3m-view-this-url)
    (define-key map [(shift return)] 'w3m-view-this-url-new-session)
    (define-key map [(shift kp-enter)] 'w3m-view-this-url-new-session)
    (define-key map [right] 'w3m-view-this-url)
    (if (featurep 'xemacs)
	(progn
	  (define-key map [(button2)] 'w3m-mouse-view-this-url)
	  (define-key map [(shift button2)]
	    'w3m-mouse-view-this-url-new-session))
      (define-key map [mouse-2] 'w3m-mouse-view-this-url)
      (define-key map [S-mouse-2] 'w3m-mouse-view-this-url-new-session))
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
    (define-key map "\M-i" 'w3m-save-image)
    (define-key map "c" 'w3m-print-current-url)
    (define-key map "M" 'w3m-view-url-with-external-browser)
    (define-key map "G" 'w3m-goto-url-new-session)
    (define-key map "g" 'w3m-goto-url)
    (define-key map "T" 'w3m-toggle-inline-images)
    (define-key map "t" 'w3m-toggle-inline-image)
    (define-key map "U" 'w3m-goto-url)
    (define-key map "V" 'w3m-goto-url)
    (define-key map "v" 'w3m-bookmark-view)
    (define-key map "q" 'w3m-close-window)
    (define-key map "Q" 'w3m-quit)
    (define-key map "\M-n" 'w3m-copy-buffer)
    (define-key map "\C-c\C-t" 'w3m-copy-buffer)
    (define-key map "\C-c\C-p" 'w3m-previous-buffer)
    (define-key map "\C-c\C-n" 'w3m-next-buffer)
    (define-key map "\C-c\C-w" 'w3m-delete-buffer)
    (define-key map "\C-c\C-s" 'w3m-select-buffer)
    (define-key map "\C-c\C-a" 'w3m-switch-buffer)
    (define-key map "R" 'w3m-reload-this-page)
    (define-key map "C" 'w3m-redisplay-with-charset)
    (define-key map "?" 'describe-mode)
    (define-key map "\M-a" 'w3m-bookmark-add-this-url)
    (define-key map "a" 'w3m-bookmark-add-current-url)
    (define-key map "+" 'w3m-antenna-add-current-url)
    (define-key map "]" 'w3m-next-form)
    (define-key map "[" 'w3m-previous-form)
    (define-key map "}" 'w3m-next-image)
    (define-key map "{" 'w3m-previous-image)
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
    (define-key map "\C-c\C-g" 'w3m-process-stop)
    (setq w3m-lynx-like-map map)))

(defvar w3m-info-like-map nil
  "Info-like keymap used in w3m-mode buffers.")
(unless w3m-info-like-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map [backspace] 'w3m-scroll-down-or-previous-url)
    (define-key map [delete] 'w3m-scroll-down-or-previous-url)
    (define-key map "\C-?" 'w3m-scroll-down-or-previous-url)
    (if (featurep 'xemacs)
	(define-key map [(shift space)] 'w3m-scroll-down-or-previous-url)
      ;; Note: It does not have an effect on Emacs 19.
      (define-key map [?\S-\ ] 'w3m-scroll-down-or-previous-url))
    (define-key map "\t" 'w3m-next-anchor)
    (define-key map [(shift tab)] 'w3m-previous-anchor)
    (define-key map [(shift iso-lefttab)] 'w3m-previous-anchor)
    (define-key map "\M-\t" 'w3m-previous-anchor)
    (define-key map "\C-m" 'w3m-view-this-url)
    (define-key map [(shift return)] 'w3m-view-this-url-new-session)
    (define-key map [(shift kp-enter)] 'w3m-view-this-url-new-session)
    (if (featurep 'xemacs)
	(progn
	  (define-key map [(button2)] 'w3m-mouse-view-this-url)
	  (define-key map [(shift button2)]
	    'w3m-mouse-view-this-url-new-session))
      (define-key map [mouse-2] 'w3m-mouse-view-this-url)
      (define-key map [S-mouse-2] 'w3m-mouse-view-this-url-new-session))
    (define-key map "\C-c\C-@" 'w3m-history-store-position)
    (define-key map [?\C-c?\C- ] 'w3m-history-store-position)
    (define-key map "\C-c\C-b" 'w3m-history-restore-position)
    (define-key map " " 'w3m-scroll-up-or-next-url)
    (define-key map "a" 'w3m-bookmark-add-current-url)
    (define-key map "\M-a" 'w3m-bookmark-add-this-url)
    (define-key map "+" 'w3m-antenna-add-current-url)
    (define-key map "A" 'w3m-antenna)
    (define-key map "b" 'w3m-scroll-down-or-previous-url)
    (define-key map "c" 'w3m-print-this-url)
    (define-key map "C" 'w3m-redisplay-with-charset)
    (define-key map "d" 'w3m-download)
    (define-key map "D" 'w3m-download-this-url)
    (define-key map "e" 'w3m-edit-current-url)
    (define-key map "E" 'w3m-edit-this-url)
    (define-key map "f" 'undefined) ;; reserved.
    (define-key map "g" 'w3m-goto-url)
    (define-key map "\M-g" 'goto-line)
    (define-key map "G" 'w3m-goto-url-new-session)
    (define-key map "h" 'describe-mode)
    (define-key map "H" 'w3m-gohome)
    (define-key map "i" (if (w3m-display-graphic-p)
			    'w3m-toggle-inline-image
			  'w3m-view-image))
    (define-key map "I" 'w3m-toggle-inline-images)
    (define-key map "\M-i" 'w3m-save-image)
    (define-key map "l" 'w3m-view-previous-page)
    (define-key map "\C-l" 'recenter)
    (define-key map [(control L)] 'w3m-reload-this-page)
    (define-key map "M" 'w3m-view-url-with-external-browser)
    (define-key map "n" 'w3m-view-next-page)
    (define-key map "N" 'w3m-namazu)
    (define-key map "\M-n" 'w3m-copy-buffer)
    (define-key map "\C-c\C-t" 'w3m-copy-buffer)
    (define-key map "\C-c\C-p" 'w3m-previous-buffer)
    (define-key map "\C-c\C-n" 'w3m-next-buffer)
    (define-key map "\C-c\C-w" 'w3m-delete-buffer)
    (define-key map "\C-c\C-s" 'w3m-select-buffer)
    (define-key map "\C-c\C-a" 'w3m-switch-buffer)
    (define-key map "o" 'w3m-history)
    (define-key map "O" 'w3m-db-history)
    (define-key map "p" 'w3m-view-previous-page)
    (define-key map "P" 'undecided) ;; reserved for print-this-buffer.
    (define-key map "q" 'w3m-close-window)
    (define-key map "Q" 'w3m-quit)
    (define-key map "R" 'w3m-reload-this-page)
    (define-key map "s" 'w3m-search)
    (define-key map "S" (lambda ()
			  (interactive)
			  (let ((current-prefix-arg t))
			    (call-interactively 'w3m-search))))
    (define-key map "T" 'w3m-dtree)
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
    (define-key map "^" 'w3m-view-parent-page)
    (define-key map "]" 'w3m-next-form)
    (define-key map "[" 'w3m-previous-form)
    (define-key map "}" 'w3m-next-image)
    (define-key map "{" 'w3m-previous-image)
    (define-key map "\C-c\C-c" 'w3m-submit-form)
    (define-key map "\C-c\C-g" 'w3m-process-stop)
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
	    (prog1 (y-or-n-p "Do you want to exit w3m? ")
	      (message "")))
    (w3m-delete-frame-maybe)
    (dolist (buffer (w3m-list-buffers t))
      (w3m-cancel-refresh-timer buffer)
      (kill-buffer buffer))
    (w3m-select-buffer-close-window)
    (w3m-cache-shutdown)
    (w3m-arrived-shutdown)
    (remove-hook 'kill-emacs-hook 'w3m-arrived-shutdown)
    (w3m-process-shutdown)
    (w3m-kill-all-buffer)))

(defun w3m-close-window ()
  "Close this window and make the other buffer current."
  (interactive)
  (unless (prog1
	      (w3m-delete-frame-maybe)
	    (let ((cur) (buffers (list (current-buffer))))
	      (bury-buffer (current-buffer))
	      (w3m-cancel-refresh-timer (current-buffer))
	      (while (with-current-buffer (setq cur (other-buffer))
		       (and (not (memq (current-buffer) buffers))
			    (eq major-mode 'w3m-mode)))
		(w3m-cancel-refresh-timer cur)
		(bury-buffer cur)
		(push cur buffers))))
    (set-window-buffer (selected-window) (other-buffer))
    (w3m-select-buffer-close-window)))

(unless w3m-mode-map
  (setq w3m-mode-map
	(if (eq w3m-key-binding 'info)
	    w3m-info-like-map
	  w3m-lynx-like-map)))

(defun w3m-mode ()
  "\\<w3m-mode-map>
   Major mode to browsing w3m buffer.

\\[w3m-view-this-url]	View this url.
	You may use the prefix arg `2' or `\\[universal-argument] \\<universal-argument-map>\\[universal-argument-more]\\<w3m-mode-map>' to make a new session.
\\[w3m-mouse-view-this-url]	View this url use mouse.
	If w3m-use-form is t, \\[w3m-view-this-url] and \\[w3m-mouse-view-this-url] action a form input.
	You may use the prefix arg `2' or `\\[universal-argument] \\<universal-argument-map>\\[universal-argument-more]\\<w3m-mode-map>' to make a new session.
\\[w3m-view-this-url-new-session]	View this url in a new session.
\\[w3m-mouse-view-this-url-new-session]	View this url in a new session use mouse.
\\[w3m-submit-form]	Submit the form at point.

\\[w3m-reload-this-page]	Reload this page.
\\[w3m-redisplay-with-charset]	Redisplay this page with specified coding-system.

\\[w3m-next-anchor]	Jump to next anchor.
\\[w3m-previous-anchor]	Jump to previous anchor.
\\[w3m-next-form]	Jump to next form.
\\[w3m-previous-form]	Jump to previous form.
\\[w3m-next-image]	Jump to next image.
\\[w3m-previous-image]	Jump to previous image.

\\[w3m-view-previous-page]	Back to previous page.
\\[w3m-view-next-page]	Forward to next page.
\\[w3m-view-parent-page]	Upward to parent page.

\\[w3m-goto-url]	Goto URL.
\\[w3m-goto-url-new-session]	Goto URL in the new session.
\\[w3m-gohome]	Goto home page.

\\[w3m-download-this-url]	Download this url.
\\[w3m-print-this-url]	Print this url.
\\[w3m-view-image]	View image.
\\[w3m-save-image]	Save image.
\\[w3m-toggle-inline-images]	Toggle displaying of inline images on current buffer.

\\[w3m-print-current-url]	Print current url.
\\[w3m-view-url-with-external-browser]	View current url with external browser.
\\[w3m-view-source]	Display source of this current buffer.
\\[w3m-view-header]	Display header of this current buffer.
\\[w3m-edit-current-url]	Edit the local file pointed by the URL of current page.
\\[w3m-edit-this-url]	Edit the local file by the under the point.

\\[w3m-scroll-up-or-next-url]	Scroll up or go to next url.
\\[w3m-scroll-down-or-previous-url]	Scroll down or go to previous url.
\\[w3m-scroll-left]	Scroll to left.
\\[w3m-scroll-right]	Scroll to right.

\\[next-line]	Next line.
\\[previous-line]	Previous line.

\\[forward-char]	Forward char.
\\[backward-char]	Backward char.

\\[goto-line]	Jump to line.
\\[w3m-history-store-position]	Mark the current position.
\\[w3m-history-restore-position]	Goto the last position.

\\[w3m-history]	Display arrived history.
	If called with '\\[universal-argument]', this command displays arrived-DB history.
\\[w3m-antenna]	Display the report change of WEB sites.
	If called with '\\[universal-argument]', this command reloads report.
\\[w3m-antenna-add-this-url]	Add link under cursor to antenna.
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
\\[w3m-next-buffer]	Switch to next w3m buffer.
\\[w3m-previous-buffer]	Switch to previous w3m buffer.
\\[w3m-select-buffer]	Select one buffer of all w3m buffers.
\\[w3m-switch-buffer]	Switch one buffer of all w3m buffers.
\\[w3m-delete-buffer]	Kill current w3m buffer.

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
  (setq truncate-lines t
	w3m-display-inline-images w3m-default-display-inline-images)
  (w3m-setup-toolbar)
  (w3m-setup-menu)
  (run-hooks 'w3m-mode-hook))

(defun w3m-scroll-up-or-next-url (arg)
  "Scroll text of current window upward ARG lines; or go to next url."
  (interactive "P")
  (if (pos-visible-in-window-p (point-max))
      (if w3m-next-url
	  (w3m-goto-url w3m-next-url)
	(signal 'end-of-buffer nil))
    (scroll-up arg)))

(defun w3m-scroll-down-or-previous-url (arg)
  (interactive "P")
  (if (pos-visible-in-window-p (point-min))
      (if w3m-previous-url
	  (w3m-goto-url w3m-previous-url)
	(signal 'beginning-of-buffer nil))
    (scroll-down arg)))

(defun w3m-scroll-left (arg)
  "Scroll to left.
Scroll size is `w3m-horizontal-scroll-columns' columns
or prefix ARG columns."
  (interactive "P")
  ;; When `scroll-left' is called non-interactively in Emacs21, it
  ;; doesn't work correctly.
  (let ((current-prefix-arg
	 (if arg
	     (prefix-numeric-value arg)
	   w3m-horizontal-scroll-columns)))
    (call-interactively 'scroll-left)))

(defun w3m-scroll-right (arg)
  "Scroll to right.
Scroll size is `w3m-horizontal-scroll-columns' columns
or prefix ARG columns."
  (interactive "P")
  ;; When `scroll-right' is called non-interactively in Emacs21, it
  ;; doesn't work correctly.
  (let ((current-prefix-arg
	 (if arg
	     (prefix-numeric-value arg)
	   w3m-horizontal-scroll-columns)))
    (call-interactively 'scroll-right)))

(defun w3m-goto-mailto-url (url)
  (if (and (symbolp w3m-mailto-url-function)
	   (fboundp w3m-mailto-url-function))
      (funcall w3m-mailto-url-function url)
    (let (comp)
      ;; Require `mail-user-agent' setting
      (unless (and (boundp 'mail-user-agent)
		   (symbol-value 'mail-user-agent))
	(error "You must specify the valid value to `mail-user-agent'"))
      (unless (and (setq comp (get (symbol-value 'mail-user-agent)
				   'composefunc))
		   (fboundp comp))
	(error "No function to compose a mail in `%s'"
	       (symbol-value 'mail-user-agent)))
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

(defun w3m-goto-ftp-url (url &optional filename)
  "Copy a remote file to a local file if URL looks like a file, otherwise
run `dired-other-window' for URL using `ange-ftp' or `efs'.  Optional
FILENAME specifies the name of a local file.  If FILENAME is omitted,
it will prompt user where to save a file."
  (let ((ftp (w3m-convert-ftp-url-for-emacsen url))
	file)
    (if (or (string-equal "/" (substring ftp -1))
	    ;; `file-directory-p' takes a long time for remote files.
	    (file-directory-p ftp))
	(dired-other-window ftp)
      (setq file (file-name-nondirectory ftp))
      (unless filename
	(setq filename (w3m-read-file-name nil nil file)))
      (unless (file-writable-p (file-name-directory filename))
	(error "Permission denied, %s" (file-name-directory filename)))
      (when (or (not (file-exists-p filename))
		(if (file-writable-p filename)
		    (and (prog1
			     (y-or-n-p
			      (format "File(%s) already exists. Overwrite? "
				      filename))
			   (message ""))
			 (progn
			   (delete-file filename)
			   t))
		  (error "Permission denied, %s" filename)))
	(copy-file ftp filename)
	(message "Wrote %s" filename)))))

(defun w3m-add-local-hook (hook function &optional append)
  "Add to the buffer-local value of HOOK the function FUNCTION."
  (w3m-static-when (or (featurep 'xemacs) (< emacs-major-version 21))
    (make-local-hook hook))
  (add-hook hook function append t))

(defvar w3m-current-position -1)
(make-variable-buffer-local 'w3m-current-position)

(defun w3m-store-current-position ()
  "Store the current position to `w3m-current-position' before every
commands.  This function is designed as the hook function which is
registered to `pre-command-hook' by `w3m-buffer-setup'."
  (setq w3m-current-position (point)))

(defun w3m-print-this-url-after-command ()
  "Print the URL pointed by the anchor under the cursor after every
commands.  This function is designed as the hook function which is
registered to `post-command-hook' by `w3m-buffer-setup'."
  (when (/= (point) w3m-current-position)
    (w3m-print-this-url)))

(defsubst w3m-buffer-setup ()
  "When this buffer's major mode is not w3m-mode, generate an
appropriate buffer and select it."
  (unless (eq major-mode 'w3m-mode)
    (set-buffer (get-buffer-create "*w3m*"))
    (unless (eq major-mode 'w3m-mode)
      (w3m-mode)))
  (w3m-add-local-hook 'pre-command-hook 'w3m-store-current-position)
  (w3m-add-local-hook 'post-command-hook 'w3m-print-this-url-after-command)
  (setq mode-line-buffer-identification
	(list "%b "
	      '((w3m-current-process
		 w3m-modeline-process-status-on
		 (w3m-display-inline-images
		  w3m-modeline-image-status-on
		  w3m-modeline-status-off)))
	      " / "
	      'w3m-current-title)))

;;;###autoload
(defun w3m-goto-url (url &optional reload charset post-data referer handler)
  "Retrieve contents of URL.
If the second argument RELOAD is non-nil, reload a content of URL.
The third argument CHARSET specifies a charset to be used for decoding
a content.  Except that if it is t, a charset entry in the arrived DB
will be cleared.
The fourth argument POST-DATA should be a string or a cons cell.  If
it is a string, it makes this function request a body as if the
content-type is \"x-www-form-urlencoded\".  If it is a cons cell, the
car of a cell is used as the content-type and the cdr of a cell is
used as the body.
If the fifth argument REFERER is specified, it is used for a Referer:
field for this request."
  (interactive
   (list
    (w3m-input-url nil
		   (when (stringp w3m-current-url)
		     (if (string-match "\\`about://\\(header\\|source\\)/"
				       w3m-current-url)
			 (substring w3m-current-url (match-end 0))
		       w3m-current-url)))
    current-prefix-arg
    (w3m-static-if (fboundp 'universal-coding-system-argument)
	coding-system-for-read)))
  (set-text-properties 0 (length url) nil url)
  (cond
   ;; process mailto: protocol
   ((string-match "\\`mailto:\\(.*\\)" url)
    (w3m-goto-mailto-url url))
   ;; process ftp: protocol
   ((and (string-match "\\`ftp://" url)
	 (not (string= "text/html" (w3m-local-content-type url))))
    (w3m-goto-ftp-url url))
   (t
    (w3m-buffer-setup)			; Setup buffer.
    (w3m-arrived-setup)			; Setup arrived database.
    (switch-to-buffer (current-buffer))
    (w3m-cancel-refresh-timer (current-buffer))
    (when w3m-current-process
      (error "%s" "Can not run two w3m processes simultaneously"))
    (w3m-process-stop (current-buffer))	; Stop all processes retrieving images.
    ;; Store the current position in the history structure.
    (w3m-history-store-position)
    (when w3m-current-forms
      ;; Store the current forms in the history structure.
      (w3m-history-plist-put :forms w3m-current-forms nil nil t))
    (when (and post-data (w3m-history-assoc url))
      ;; Remove processing url's forms from the history structure.
      (w3m-history-remove-properties '(:forms) url nil t))
    ;; Retrieve.
    (lexical-let ((url url)
		  (reload reload)
		  (charset charset)
		  (post-data post-data)
		  (referer referer)
		  (orig url)
		  (name))
      ;; local directory URL check
      (if (and (w3m-url-local-p url)
	       (file-directory-p (w3m-url-to-file-name url))
	       (setq url (file-name-as-directory url))
	       (eq w3m-local-directory-view-method 'w3m-dtree)
	       (string-match "^file:///" url))
	  (setq url (replace-match "about://dtree/" nil nil url)))
      (and (string-match w3m-url-components-regexp url)
	   (match-beginning 8)
	   (setq name (match-string 9 url)
		 url (substring url 0 (match-beginning 8))))
      (lexical-let ((ct (w3m-arrived-content-type url))
		    (cs (unless (eq t charset)
			  (or charset (w3m-arrived-content-charset url)))))
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
	(w3m-process-do
	    (action
	     (if (and (not reload)
		      (not charset)
		      (stringp w3m-current-url)
		      (string= url w3m-current-url))
		 (progn
		   (w3m-refontify-anchor)
		   (or (when name (w3m-search-name-anchor name))
		       (goto-char (point-min)))
		   'cursor-moved)
	       (setq w3m-image-only-page nil
		     w3m-current-buffer (current-buffer)
		     w3m-current-process
		     (w3m-retrieve-and-render url reload cs ct post-data
					      referer handler))))
	  (with-current-buffer w3m-current-buffer
	    (setq w3m-current-process nil)
	    (cond
	     ((not action)
	      (w3m-history-push (w3m-real-url url)
				(list :title (file-name-nondirectory url)))
	      (w3m-history-push w3m-current-url)
	      (w3m-refontify-anchor))
	     ((not (eq action 'cursor-moved))
	      (w3m-history-push w3m-current-url (list :title w3m-current-title))
	      (w3m-history-add-properties (list :referer referer
						:post-data post-data)
					  nil nil t)
	      (or (and name (w3m-search-name-anchor name))
		  (goto-char (point-min)))
	      (unless w3m-toggle-inline-images-permanently
		(setq w3m-display-inline-images w3m-default-display-inline-images))
	      (cond ((w3m-display-inline-images-p)
		     (and w3m-force-redisplay (sit-for 0))
		     (w3m-toggle-inline-images 'force reload))
		    ((and (w3m-display-graphic-p)
			  w3m-image-only-page)
		     (and w3m-force-redisplay (sit-for 0))
		     (w3m-toggle-inline-image 'force reload)))
	      (setq buffer-read-only t)
	      (set-buffer-modified-p nil)))
	    (w3m-arrived-add orig w3m-current-title nil nil cs ct)
	    (setq default-directory
		  (file-name-as-directory
		   (let ((file (w3m-url-to-file-name url)))
		     (if (and file (file-exists-p file))
			 (if (file-directory-p file)
			     file
			   (file-name-directory file))
		       w3m-profile-directory))))
	    (w3m-update-toolbar)
	    (run-hook-with-args 'w3m-display-hook url)
	    (w3m-refresh-at-time))))))))

(defun w3m-refresh-at-time ()
  (when (and w3m-use-refresh w3m-current-refresh)
    (if (= (car w3m-current-refresh) 0)
	(w3m-goto-url-with-timer (cdr w3m-current-refresh) (current-buffer))
      (setq w3m-refresh-timer
	    (run-at-time (car w3m-current-refresh)
			 nil
			 'w3m-goto-url-with-timer
			 (cdr w3m-current-refresh)
			 (current-buffer))))))

(defun w3m-goto-url-with-timer (url buffer)
  "Run the command `w3m-goto-url' from the timer of refresh."
  (when (and url buffer (get-buffer buffer))
    (if (get-buffer-window buffer)
	(save-selected-window
	  (pop-to-buffer buffer)
	  (with-current-buffer buffer
	    (w3m-cancel-refresh-timer buffer)
	    (w3m-goto-url url)))
      (with-current-buffer buffer
	(w3m-cancel-refresh-timer buffer)))))

;;;###autoload
(defun w3m-goto-url-new-session (url
				 &optional reload charset post-data referer)
  "Run the command `w3m-goto-url' in the new session.  If you invoke this
command in the w3m buffer, the new session will be created by copying
the current session.  Otherwise, the new session will start afresh."
  (interactive
   (list
    (w3m-input-url nil
		   (when (stringp w3m-current-url)
		     (if (string-match "\\`about://\\(header\\|source\\)/"
				       w3m-current-url)
			 (substring w3m-current-url (match-end 0))
		       w3m-current-url)))
    current-prefix-arg
    (w3m-static-if (fboundp 'universal-coding-system-argument)
	coding-system-for-read)))
  (if (eq 'w3m-mode major-mode)
      (progn
	(switch-to-buffer (w3m-copy-buffer nil nil (interactive-p) 'empty))
	;; When new URL has `name' portion, we have to goto the base url
	;; because generated buffer has no content at this moment.
	(when (and (string-match w3m-url-components-regexp url)
		   (match-beginning 8))
	  (let ((name (match-string 9 url))
		(url (substring url 0 (match-beginning 8))))
	    (w3m-goto-url url reload charset post-data referer)))
	(w3m-goto-url url reload charset post-data referer))
    (w3m url t)))

(defun w3m-move-point-for-localcgi (url)
  (when (and (w3m-url-local-p url)
	     (file-directory-p (w3m-url-to-file-name url))
	     (not (eq w3m-local-directory-view-method 'w3m-dtree)))
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
	(recenter height)))))

;;;###autoload
(defun w3m-gohome ()
  "Go to the Home page."
  (interactive)
  (unless w3m-home-page
    (error "You have to specify the value of `w3m-home-page'"))
  (w3m-goto-url w3m-home-page))

(defun w3m-reload-this-page (&optional arg)
  "Reload current page without cache.
If called with '\\[universal-argument]', clear form and post datas"
  (interactive "P")
  (let ((post-data (w3m-history-plist-get :post-data nil nil t))
	(form-data (w3m-history-plist-get :forms nil nil t))
	(referer (w3m-history-plist-get :referer nil nil t)))
    (when arg
      (when form-data
	(w3m-history-remove-properties '(:forms) nil nil t))
      (when post-data
	(setq post-data nil)
	(w3m-history-remove-properties '(:post-data) nil nil t))
      (setq w3m-current-forms nil))
    (if (and post-data (y-or-n-p "Repost form data? "))
	(w3m-goto-url w3m-current-url 'reload nil post-data referer)
      (w3m-goto-url w3m-current-url 'reload nil nil referer))))

(defun w3m-redisplay-with-charset (&optional arg)
  "Redisplay current page with specified coding-system.
If input is nil, use default coding-system on w3m."
  (interactive "P")
  (let ((charset
	 (w3m-read-content-charset
	  (format "Content-charset (current %s, default reset): "
		  w3m-current-coding-system)
	  t ;; Default action is reseting charset entry in arrived DB.
	  )))
    (when arg
      (setq w3m-display-inline-images (not w3m-display-inline-images)))
    (w3m-goto-url w3m-current-url nil charset)))


;;;###autoload
(defun w3m (&optional url new-session)
  "Visit the World Wide Web page using the external command w3m, w3mmee
or w3m-m17n.  When you invoke this command interactively, it will
prompt you for a URL where you wish to go.  Except that if the prefix
argument is given or you enter the empty string, and the buffer for
w3m exists, it will just pop up the buffer.  URL should be a string
which defaults to the value of `w3m-home-page' or \"about:\".
Otherwise, you can run this command in the batch mode like:

  emacs -f w3m http://emacs-w3m.namazu.org/ &

The value of `w3m-pop-up-frames' specifies whether to pop up a new
frame, however, it will be ignored (treated as nil) when this command
is called non-interactively.  Optional NEW-SESSION is supposed to be
used by the command `w3m-goto-url-new-session' to create a new session,
for neither the interactive use nor the batch mode."
  (interactive
   (let ((default (if (w3m-alive-p) 'popup w3m-home-page)))
     (list
      (if current-prefix-arg
	  default
	(w3m-input-url nil nil default w3m-quick-start)))))
  (let ((nofetch (eq url 'popup))
	(buffer (unless new-session
		  (w3m-alive-p)))
	(focusing-function
	 (if (fboundp 'select-frame-set-input-focus)
	     'select-frame-set-input-focus
	   (lambda (frame)
	     (raise-frame frame)
	     (select-frame frame)
	     (focus-frame frame))))
	(params (w3m-pop-up-frame-parameters))
	(popup-frame-p (w3m-popup-frame-p new-session))
	window frame)
    (unless (and (stringp url)
		 (> (length url) 0))
      (if buffer
	  (setq nofetch t)
	;; It may be called non-interactively.
	(setq url (or (when (= 1 (length command-line-args-left))
			(pop command-line-args-left))
		      w3m-home-page
		      "about:")
	      nofetch nil)))
    (if (bufferp buffer)
	(progn
	  (when (setq window (get-buffer-window buffer t))
	    (setq frame (window-frame window)))
	  (cond (frame
		 (funcall focusing-function frame)
		 (select-window window)
		 (setq frame nil))
		(window
		 (select-window window))
		(popup-frame-p
		 (funcall focusing-function (setq frame (make-frame params)))
		 (switch-to-buffer buffer))
		(t
		 (switch-to-buffer buffer))))
      (setq buffer (generate-new-buffer "*w3m*"))
      (if popup-frame-p
	  (progn
	    (funcall focusing-function (setq frame (make-frame params)))
	    (switch-to-buffer buffer))
	(switch-to-buffer buffer))
      (insert (make-string (max 0 (/ (1- (window-height)) 2)) ?\n)
	      "Reading " url "...")
      (beginning-of-line)
      (let ((fill-column (window-width)))
	(center-region (point) (point-max)))
      (goto-char (point-min))
      (sit-for 0)
      (w3m-mode))
    (unwind-protect
	(unless nofetch
	  (w3m-goto-url url))
      (when frame
	(setq w3m-initial-frame frame)))))

(eval-when-compile
  (autoload 'browse-url-interactive-arg "browse-url"))

;;;###autoload
(defun w3m-browse-url (url &optional new-window)
  "w3m interface function for browse-url.el."
  (interactive
   (browse-url-interactive-arg "w3m URL: "))
  (when (stringp url)
    (if new-window (split-window))
    (w3m-goto-url url)))

;;;###autoload
(defun w3m-find-file (file)
  "w3m Interface function for local file."
  (interactive "fFilename: ")
  (w3m-goto-url (w3m-expand-file-name-as-url file)
		nil
		(w3m-static-if (fboundp 'universal-coding-system-argument)
		    coding-system-for-read)))


(defun w3m-cygwin-path (path)
  "Convert win32 path into cygwin format.
ex.) c:/dir/file => //c/dir/file"
  (if (string-match "^\\([A-Za-z]\\):" path)
      (replace-match "//\\1" nil nil path)
    path))


;;;###autoload
(defun w3m-region (start end &optional url)
  "Render region in current buffer and replace with result."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (w3m-clear-local-variables)
    (setq w3m-current-buffer (current-buffer)
	  w3m-current-url url
	  w3m-current-base-url url
	  w3m-current-title (w3m-rendering-multibyte-buffer))
    (w3m-fontify)
    (when (w3m-display-inline-images-p)
      (and w3m-force-redisplay (sit-for 0))
      (w3m-toggle-inline-images 'force))))


;;; About:
(defun w3m-about (url &rest args)
  (insert "<!doctype html public \"-//W3C//DTD HTML 3.2//EN\">
<html>
<head><title>About emacs-w3m</title></head>
<body>
<center>
Welcome to <a href=\"http://emacs-w3m.namazu.org/\">\
<img src=\"about://emacs-w3m.gif\" alt=\"emacs-w3m\"></a>!
<br><br>
emacs-w3m is an interface program of
<a href=\"http://w3m.sourceforge.net/\">w3m</a>,
works on Emacs.
</center>
</body>
</html>")
  "text/html")

(defun w3m-view-source ()
  "Display source of this current buffer."
  (interactive)
  (w3m-goto-url
   (cond
    ((string-match "\\`about://source/" w3m-current-url)
     (substring w3m-current-url (match-end 0)))
    ((string-match "\\`about://header/" w3m-current-url)
     (concat "about://source/" (substring w3m-current-url (match-end 0))))
    (t (concat "about://source/" w3m-current-url)))))

(defun w3m-about-header (url &optional no-decode no-cache &rest args)
  (when (string-match "\\`about://header/" url)
    (setq url (substring url (match-end 0)))
    (insert "Page Information\n"
	    "\nTitle:          " (or (w3m-arrived-title url) "")
	    "\nURL:            " url
	    "\nDocument Type:  " (w3m-content-type url)
	    "\nLast Modified:  "
	    (let ((time (w3m-last-modified url)))
	      (if time (current-time-string time) "")))
    (let ((charset (w3m-arrived-content-charset url)))
      (when charset
	(insert "\nDocument Code:  " charset)))
    (let (header)
      (and (not (w3m-url-local-p url))
	   (setq header (condition-case nil
			    (w3m-process-with-wait-handler
			      (w3m-w3m-get-header url no-cache handler))
			  (w3m-process-timeout nil)))
	   (insert
	    (if (string= w3m-language "Japanese")
		"\n\n━━━━━━━━━━━━━━━━━━━\n\nHeader information\n\n"
	      "\n\n--------------------------------------\n\nHeader information\n\n")
	    header)))
    "text/plain"))

(defun w3m-view-header ()
  "Display header of this current buffer."
  (interactive)
  (w3m-goto-url
   (cond
    ((string-match "\\`about://header/" w3m-current-url)
     (substring w3m-current-url (match-end 0)))
    ((string-match "\\`about://source/" w3m-current-url)
     (concat "about://header/" (substring w3m-current-url (match-end 0))))
    ((string-match "\\`about:" w3m-current-url)
     (error "Can't load a header for %s" w3m-current-url))
    (t (concat "about://header/" w3m-current-url)))))

(defvar w3m-about-history-max-indentation '(/ (* (window-width) 2) 3)
  "*Number to limit the indentation when showing a tree-structured
history by the command `w3m-about-history'.  This value is evaluated
for each run-time, so it can be any `s' expressions.")

(defvar w3m-about-history-indent-level 4
  "*Number that says how much each sub-tree should be indented when
showing a tree-structured history by the command `w3m-about-history'.")

(defun w3m-about-history (&rest args)
  "Show a tree-structured history."
  (let ((start)
	(history (with-current-buffer w3m-current-buffer
		   w3m-history-flat)))
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
		;; FIXME: Ad-hoc workaround to avoid illegal-type error.
		about (or (not (stringp url))
			  (string-match w3m-history-ignored-regexp url))
		title (plist-get (cadr element) :title)
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
    (let ((current (with-current-buffer w3m-current-buffer
		     (car (w3m-history-current)))))
      (and current
	   (search-forward (concat "<a href=\"" current "\">") nil t)
	   (progn
	     (forward-line 0)
	     (delete-char 1)
	     (insert "&gt;"))))
    "text/html"))

(defun w3m-about-db-history (&rest args)
  (let ((width (- (if (< 0 w3m-fill-column)
		      w3m-fill-column
		    (+ (frame-width) (or w3m-fill-column -1)))
		  18))
	(now (current-time))
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
	  (if (<= (w3m-time-lapse-seconds (cdr (car alist)) now)
		  64800) ;; = (* 60 60 18) 18hours ago.
	      (setq date (format-time-string "%H:%M:%S" (cdr (car alist))))
	    (setq date (format-time-string "%Y-%m-%d" (cdr (car alist)))))
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

(defun w3m-history-highlight-current-url (url)
  ;; Highlight the current url if it is a page for the history.
  (when (string-equal "about://history/" url)
    (goto-char (point-min))
    (when (search-forward "\n>" nil t)
      (w3m-next-anchor)
      (let ((start (point))
	    (inhibit-read-only t))
	(end-of-line)
	(put-text-property start (point) 'face 'w3m-history-current-url-face)
	(goto-char start)
	(set-buffer-modified-p nil)))))

(defun w3m-db-history ()
  (interactive)
  (w3m-goto-url "about://db-history/"))

(defun w3m-w32-browser-with-fiber (url)
  (let ((proc (start-process "w3m-w32-browser-with-fiber"
			     (current-buffer)
			     "fiber.exe" "-s"
			     (if (w3m-url-local-p url)
				 (w3m-url-to-file-name url)
			       url))))
    (set-process-filter proc 'ignore)
    (set-process-sentinel proc 'ignore)))

;;; Interactive select buffer.
(defcustom w3m-select-buffer-horizontal-window t
  "*Non-nil means that `w3m-select-buffer' generates its window horizontally."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-select-buffer-window-ratio `(18 . 12)
  "*A pair of the parcent of `w3m-select-buffer' window size for the frame size."
  :group 'w3m
  :type '(cons (integer :tag "horizontally ratio (n/100)")
	       (integer :tag "vertically ratio (m/100)")))

(defvar w3m-select-buffer-window nil)
(defconst w3m-select-buffer-message
  "n: next buffer, p: previous buffer, q: quit.")

(defun w3m-select-buffer (&optional toggle nomsg)
  "Display a new buffer to select a buffer among the set of w3m-mode
buffers.  User can type following keys:

\\{w3m-select-buffer-mode-map}"
  (interactive "P")
  (when toggle
    (setq w3m-select-buffer-horizontal-window
	  (not w3m-select-buffer-horizontal-window))
    (when (get-buffer-window w3m-select-buffer-name)
      (delete-windows-on (get-buffer w3m-select-buffer-name))))
  (let ((selected-window (selected-window))
	(current-buffer (current-buffer)))
    (set-buffer (w3m-get-buffer-create w3m-select-buffer-name))
    (unless (eq nomsg 'update)
      (setq w3m-select-buffer-window selected-window))
    (let ((w (or (get-buffer-window w3m-select-buffer-name)
		 (split-window selected-window
			       (w3m-select-buffer-window-size)
			       w3m-select-buffer-horizontal-window))))
      (set-window-buffer w (current-buffer))
      (select-window w))
    (w3m-select-buffer-generate-contents current-buffer))
  (w3m-select-buffer-mode)
  (or nomsg (message w3m-select-buffer-message)))

(defun w3m-select-buffer-update (&rest args)
  (when (get-buffer-window w3m-select-buffer-name)
    (save-selected-window
      (w3m-select-buffer nil 'update))))

(defun w3m-select-buffer-generate-contents (current-buffer)
  (let (buffer-read-only)
    (delete-region (point-min) (point-max))
    (dolist (buffer (w3m-list-buffers))
      (put-text-property (point)
			 (progn
			   (insert (w3m-buffer-title buffer) "\n")
			   (point))
			 'w3m-select-buffer buffer))
    (skip-chars-backward " \t\r\f\n")
    (delete-region (point) (point-max))
    (set-buffer-modified-p nil)
    (goto-char (or (text-property-any (point-min) (point-max)
				      'w3m-select-buffer current-buffer)
		   (point-min)))))

(defvar w3m-select-buffer-mode-map nil)
(unless w3m-select-buffer-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (substitute-key-definition
     'next-line 'w3m-select-buffer-next-line map global-map)
    (substitute-key-definition
     'previous-line 'w3m-select-buffer-previous-line map global-map)
    (substitute-key-definition
     'w3m-copy-buffer 'w3m-select-buffer-copy-buffer map w3m-mode-map)
    (substitute-key-definition
     'w3m-next-buffer 'w3m-select-buffer-next-line map w3m-mode-map)
    (substitute-key-definition
     'w3m-previous-buffer 'w3m-select-buffer-previous-line map w3m-mode-map)
    (substitute-key-definition
     'w3m-delete-buffer 'w3m-select-buffer-delete-buffer map w3m-mode-map)
    (substitute-key-definition
     'w3m-scroll-up-or-next-url
     'w3m-select-buffer-show-this-line map w3m-mode-map)
    (substitute-key-definition
     'w3m-scroll-down-or-previous-url
     'w3m-select-buffer-show-this-line-and-down map w3m-mode-map)
    (substitute-key-definition
     'w3m-select-buffer 'w3m-select-buffer-toggle-style map w3m-mode-map)
    (define-key map " " 'w3m-select-buffer-show-this-line)
    (define-key map "g" 'w3m-select-buffer-recheck)
    (define-key map "j" 'w3m-select-buffer-next-line)
    (define-key map "k" 'w3m-select-buffer-previous-line)
    (define-key map "n" 'w3m-select-buffer-next-line)
    (define-key map "p" 'w3m-select-buffer-previous-line)
    (define-key map "q" 'w3m-select-buffer-quit)
    (define-key map "h" 'w3m-select-buffer-show-this-line-and-switch)
    (define-key map "w" 'w3m-select-buffer-show-this-line-and-switch)
    (define-key map "\C-m" 'w3m-select-buffer-show-this-line-and-quit)
    (define-key map "\C-c\C-c" 'w3m-select-buffer-show-this-line-and-quit)
    (define-key map "\C-c\C-k" 'w3m-select-buffer-quit)
    (define-key map "\C-c\C-q" 'w3m-select-buffer-quit)
    (define-key map "?" 'describe-mode)
    (setq w3m-select-buffer-mode-map map)))

(defun w3m-select-buffer-mode ()
  "\\<w3m-select-buffer-mode-map>
Major mode to select a buffer from the set of w3m-mode buffers.

\\[w3m-select-buffer-next-line]	Next line.
\\[w3m-select-buffer-previous-line]	Previous line.

\\[w3m-select-buffer-show-this-line]	Show the current buffer or scroll up.
\\[w3m-select-buffer-show-this-line-and-down]	Show the current buffer or scroll down.
\\[w3m-select-buffer-show-this-line-and-switch]	Show the current buffer and set cusor to w3m buffer.
\\[w3m-select-buffer-show-this-line-and-quit]	Show the current buffer and quit menu.

\\[w3m-select-buffer-toggle-style]	Toggle the type of split which horizon or vertical.
\\[w3m-select-buffer-recheck]	Recheck buffers.
\\[w3m-select-buffer-quit]	Quit menu.
"
  (setq major-mode 'w3m-select-buffer-mode
	mode-name "w3m buffers"
	truncate-lines t
	buffer-read-only t)
  (use-local-map w3m-select-buffer-mode-map)
  (run-hooks 'w3m-select-buffer-mode-hook))

(defun w3m-select-buffer-recheck ()
  "Recheck all w3m-mode buffers and regenerate the menu content to
select them."
  (interactive)
  (let ((buffer-read-only nil))
    (erase-buffer))
  (w3m-select-buffer-generate-contents
   (window-buffer w3m-select-buffer-window))
  (w3m-select-buffer-show-this-line))

(defmacro w3m-select-buffer-current-buffer ()
  `(get-text-property (point) 'w3m-select-buffer))

(defun w3m-select-buffer-show-this-line ()
  "Show the current buffer on this menu line or scroll up its."
  (interactive)
  (forward-line 0)
  (let ((obuffer (and (window-live-p w3m-select-buffer-window)
		      (window-buffer w3m-select-buffer-window)))
	(buffer (w3m-select-buffer-current-buffer)))
    (unless buffer
      (error "No buffer at point"))
    (cond
     ((get-buffer-window buffer)
      (setq w3m-select-buffer-window (get-buffer-window buffer)))
     ((window-live-p w3m-select-buffer-window)
      ())
     ((one-window-p)
      (setq w3m-select-buffer-window (selected-window))
      (select-window
       (split-window nil
		     (w3m-select-buffer-window-size)
		     w3m-select-buffer-horizontal-window)))
     (t (setq w3m-select-buffer-window (get-largest-window))))
    (set-window-buffer w3m-select-buffer-window buffer)
    (when (and (interactive-p) (eq obuffer buffer))
      (save-selected-window
	(pop-to-buffer buffer)
	(w3m-scroll-up-or-next-url nil)))
    (message w3m-select-buffer-message)
    buffer))

(defun w3m-select-buffer-show-this-line-and-down ()
  "Show the current buffer on this menu line or scroll down its."
  (interactive)
  (let ((obuffer (and (window-live-p w3m-select-buffer-window)
		      (window-buffer w3m-select-buffer-window)))
	(buffer (w3m-select-buffer-show-this-line)))
    (when (eq obuffer buffer)
      (save-selected-window
	(pop-to-buffer buffer)
	(w3m-scroll-down-or-previous-url nil)))))

(defun w3m-select-buffer-next-line (&optional n)
  "Move cursor vertically down ARG lines and show the buffer on the
new menu line."
  (interactive "p")
  (forward-line n)
  (w3m-select-buffer-show-this-line))

(defun w3m-select-buffer-previous-line (&optional n)
  "Move cursor vertically up ARG lines and show the buffer on the new
menu line."
  (interactive "p")
  (w3m-select-buffer-next-line (- n)))

(defun w3m-select-buffer-copy-buffer ()
  "Create a copy of the buffer on the current menu line, and show it."
  (interactive)
  (w3m-select-buffer-show-this-line)
  (let ((selected-window (selected-window))
	w3m-pop-up-frames w3m-pop-up-windows
	pop-up-windows buf)
    (pop-to-buffer (w3m-select-buffer-current-buffer))
    (setq buf (w3m-copy-buffer (current-buffer)))
    (select-window selected-window)
    ;; w3m-select-buffer was updated automatically.
    (w3m-select-buffer-show-this-line)))

(defun w3m-select-buffer-delete-buffer ()
  "Delete the buffer on the current menu line."
  (interactive)
  (let ((buffer (w3m-select-buffer-current-buffer)))
    (forward-line -1)
    (unless (and (eq buffer (w3m-select-buffer-current-buffer))
		 (progn (forward-line 1) (eobp)))
      (kill-buffer buffer)
      (w3m-select-buffer-generate-contents
       (w3m-select-buffer-current-buffer))
      (w3m-select-buffer-show-this-line))))

(defun w3m-select-buffer-quit ()
  "Quit the menu to select a buffer from w3m-mode buffers."
  (interactive)
  (if (one-window-p)
      (set-window-buffer (selected-window)
			 (or (w3m-select-buffer-current-buffer)
			     (w3m-alive-p)))
    (let ((buf (or (w3m-select-buffer-current-buffer)
		   (w3m-alive-p))))
      (pop-to-buffer buf)
      (and (get-buffer-window w3m-select-buffer-name)
	   (delete-windows-on (get-buffer w3m-select-buffer-name))))))

(defun w3m-select-buffer-show-this-line-and-switch ()
  "Show the current buffer, and quit select a buffer from w3m-mode buffers."
  (interactive)
  (pop-to-buffer (w3m-select-buffer-show-this-line))
  (message ""))

(defun w3m-select-buffer-show-this-line-and-quit ()
  "Show the current buffer, and quit the menu to select a buffer from
w3m-mode buffers."
  (interactive)
  (w3m-select-buffer-show-this-line-and-switch)
  (and (get-buffer-window w3m-select-buffer-name)
       (delete-windows-on (get-buffer w3m-select-buffer-name))))

(defun w3m-select-buffer-close-window ()
  "Close the window which displays the menu to select w3m-mode buffers."
  (let ((window (get-buffer-window w3m-select-buffer-name)))
    (when window
      (if (one-window-p)
	  (set-window-buffer window (other-buffer))
	(delete-window window)))))

(defun w3m-select-buffer-toggle-style()
  "Toggle the style of select-buffer."
  (interactive)
  (w3m-select-buffer t))

(defun w3m-select-buffer-window-size ()
  (if w3m-select-buffer-horizontal-window
      (- (window-width)
	 (/ (* (frame-width) (car w3m-select-buffer-window-ratio)) 100))
    (- (window-height)
       (/ (* (frame-height) (cdr w3m-select-buffer-window-ratio)) 100))))


;;; Header line
(defcustom w3m-use-header-line t
  "*Non-nil activates header-line of w3m."
  :group 'w3m
  :type 'boolean)

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

(defvar w3m-header-line-map nil)
(unless w3m-header-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'w3m-goto-url)
    (setq w3m-header-line-map map)))

(defun w3m-header-line-insert ()
  "Insert the header line to this buffer."
  (when (and (or (fboundp 'xemacs)
		 (< emacs-major-version 21)
		 w3m-use-tab)
	     w3m-use-header-line
	     w3m-current-url
	     (eq 'w3m-mode major-mode))
    (goto-char (point-min))
    (insert "Location: ")
    (w3m-add-text-properties (point-min) (point)
			     `(face w3m-header-line-location-title-face))
    (let ((start (point)))
      (insert w3m-current-url)
      (w3m-add-text-properties start (point)
			       `(face w3m-header-line-location-content-face
				 mouse-face highlight
				 local-map ,w3m-header-line-map))
      (setq start (point))
      (insert-char ?\  (max 0 (- (window-width) (current-column) 1)))
      (w3m-add-text-properties start (point)
			       `(face w3m-header-line-location-content-face))
      (unless (eolp)
	(insert "\n")))))

(add-hook 'w3m-fontify-after-hook 'w3m-header-line-insert)

(provide 'w3m)

;;; w3m.el ends here

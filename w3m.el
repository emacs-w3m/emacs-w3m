;;; w3m.el --- Interface program of w3m on Emacs

;; Copyright (C) 2000, 2001, 2002, 2003
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

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

;; The following variables will be referred by the external modules
;; which bind such variables only when compiling themselves.  And some
;; module(s) use `defadvice' which will do byte-compile at run-time.
(eval-and-compile
  (defvar w3m-current-title nil "Title of this buffer.")
  (defvar w3m-current-url nil "URL of this buffer."))

(require 'w3m-util)
(require 'w3m-proc)

(eval-and-compile
  (cond
   ((featurep 'xemacs)
    (require 'w3m-xmas))
   ((and (boundp 'emacs-major-version)
	 (>= emacs-major-version 21))
    (require 'w3m-e21))
   ((boundp 'MULE)
    (require 'w3m-om))
   ((and (boundp 'emacs-major-version)
	 (= emacs-major-version 20))
    (require 'w3m-e20))
   (t
    (require 'w3m-e19))))

(require 'w3m-hist)
(require 'timezone)
(eval-and-compile
  (when (featurep 'mule)
    (require 'ccl)))

;; Add-on programs:
(eval-and-compile
  (autoload 'w3m-bookmark-view "w3m-bookmark" nil t)
  (autoload 'w3m-bookmark-add-this-url "w3m-bookmark"
    "Add link under cursor to bookmark." t)
  (autoload 'w3m-bookmark-add-current-url "w3m-bookmark"
    "Add link of current page to bookmark." t)
  (autoload 'w3m-search "w3m-search"
    "Search QUERY using SEARCH-ENGINE." t)
  (autoload 'w3m-search-uri-replace "w3m-search")
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
  (autoload 'w3m-perldoc "w3m-perldoc"
    "View Perl documents" t)
  (autoload 'w3m-about-perldoc "w3m-perldoc")
  (autoload 'w3m-fontify-forms "w3m-form")
  (autoload 'w3m-setup-tab-menu "w3m-tabmenu")
  (autoload 'w3m-switch-buffer "w3m-tabmenu")
  (autoload 'w3m-cookie-set "w3m-cookie")
  (autoload 'w3m-cookie-get "w3m-cookie")
  (autoload 'w3m-cookie "w3m-cookie")
  (autoload 'w3m-about-cookie "w3m-cookie")
  (autoload 'w3m-cookie-shutdown "w3m-cookie" nil t)
  (autoload 'report-emacs-w3m-bug "w3m-bug" nil t)
  (autoload 'widget-forward "wid-edit" nil t))

;; Avoid byte-compile warnings.
(eval-when-compile
  (autoload 'rfc2368-parse-mailto-url "rfc2368")
  (autoload 'widget-convert-button "wid-edit"))

(defconst emacs-w3m-version "1.3.5"
  "Version number of this package.")

(defconst w3m-treat-drive-letter (memq system-type '(windows-nt OS/2 emx))
  "Operating system has a drive letter.")

(defgroup w3m nil
  "w3m - the web browser of choice."
  :group 'hypermedia)

(defgroup w3m-face nil
  "Faces for w3m."
  :group 'w3m
  :prefix "w3m-")

(defcustom w3m-command nil
  "*Name of the executable file of w3m."
  :group 'w3m
  :type 'string)

(defvar w3m-type nil "Type of w3m.
These values are acceptable: w3m, w3mmee, w3m-m17n.")
(defvar w3m-compile-options nil "Compile options of w3m.")
(defvar w3m-version nil "Version string of w3m command.")

;; Set w3m-command, w3m-type, w3m-version and w3m-compile-options
(if noninteractive
    ;; Don't call the external command when compiling.
    (unless w3m-command
      (setq w3m-command "w3m"))
  (when (or (null w3m-command)
	    (null w3m-type)
	    (null w3m-version)
	    (null w3m-compile-options))
    (let ((command (or w3m-command
		       (w3m-which-command "w3m")
		       (w3m-which-command "w3mmee")
		       (w3m-which-command "w3m-m17n"))))
      (when command
	(setq w3m-command command)
	(with-temp-buffer
	  (call-process command nil t nil "-version")
	  (goto-char (point-min))
	  (when (re-search-forward "version \\(w3m/0\\.[3-9]\
\\(\\.[0-9\\]\\)*\\(rc[0-9]+\\)?\
\\(-stable\\|\\(\\+cvs\\(-[0-9]+\\.[0-9]+\\)?\\)\\)?\
\\(-inu\\|\\(-m17n\\|\\(\\+mee\\)\\)\\)?[^,]*\\)" nil t)
	    (setq w3m-version (match-string 1))
	    (setq w3m-type
		  (cond
		   ((match-beginning 9) 'w3mmee)
		   ((match-beginning 8) 'w3m-m17n)
		   ((match-beginning 1) 'w3m)
		   (t 'other))))
	  (when (re-search-forward "options +" nil t)
	    (setq w3m-compile-options
		  (or (split-string (buffer-substring
				     (match-end 0)
				     (save-excursion (end-of-line)
						     (point)))
				    ",")
		      (list nil)))))))))

(defcustom w3m-user-agent (concat "Emacs-w3m/" emacs-w3m-version
				  " " w3m-version)
  "User agent string of this package."
  :group 'w3m
  :type 'string)

(defcustom w3m-add-user-agent t
  "Add User-Agent field to the request header.
The value of `w3m-user-agent' is used for the field body."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-language
  (if (or (and (boundp 'current-language-environment)
	       (string= "Japanese"
			(symbol-value 'current-language-environment)))
	  (boundp 'MULE))
      "Japanese")
  "*Language of w3m."
  :group 'w3m
  :type '(choice
	  (const "Japanese")
	  (const nil tag: "Other")))

(defcustom w3m-command-arguments
  (if (eq w3m-type 'w3mmee) '("-o" "concurrent=0" "-F") nil)
  "*List of the default arguments passed to the w3m command.  See also
the documentation for the option `w3m-command-arguments-alist'."
  :group 'w3m
  :type '(repeat string))

(defcustom w3m-command-arguments-alist nil
  "*Alist of a regexp matching urls and additional arguments passed to
the w3m command.  This lets you, for instance, use or not use proxy
server for the particular hosts.  The first match made will be used.
Here is an example of how to set this option:

\(setq w3m-command-arguments-alist
      '(;; Don't use any additional options to visit local web pages.
	(\"^http://\\\\([^/]*\\\\.\\\\)*your-company\\\\.com\\\\(/\\\\|$\\\\)\"
	 \"-no-proxy\")
	;; Use the proxy server to visit any foreign urls.
	(\"\"
	 \"-o\" \"http_proxy=http://proxy.your-company.com:8080/\")))

Where the first element matches the url that the scheme is \"http\" and
the hostname is either \"your-company.com\" or a name ended with
\".your-company.com\".  If you are a novice on the regexps, you can use
the option `w3m-no-proxy-domains' instead."
  :group 'w3m
  :type '(repeat (cons :format "%v"
		       regexp
		       (repeat :tag "Arguments passed to w3m command"
			       (string :tag "Arg")))))

(defcustom w3m-no-proxy-domains nil
  "*List of domain names that emacs-w3m will not use a proxy server to
connect to.  Each element should be exactly a domain name which means
the latter common part of the host names, not a regexp."
  :group 'w3m
  :type '(repeat (string :tag "Domain name")))

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
width using expression (+ (window-width) VALUE)."
  :group 'w3m
  :type 'integer)

(defcustom w3m-mailto-url-function nil
  "*Mailto handling Function."
  :group 'w3m
  :type 'function)

(defcustom w3m-mailto-url-popup-function-alist
  '((cmail-mail-mode . pop-to-buffer)
    (mail-mode . pop-to-buffer)
    (message-mode . pop-to-buffer)
    (mew-draft-mode . pop-to-buffer)
    (mh-letter-mode . pop-to-buffer)
    (wl-draft-mode . pop-to-buffer))
  "*Alist of (MAJOR-MODE . FUNCTION) pairs used to popup a mail buffer.
If a user clicks on a `mailto' url and a mail buffer is composed by
`mail-user-agent' with the MAJOR-MODE, FUNCTION will be called with a
mail buffer as an argument.  Note that the variables
`special-display-buffer-names', `special-display-regexps',
`same-window-buffer-names' and `same-window-regexps' will be bound to
nil while popping up a buffer."
  :group 'w3m
  :type '(repeat (cons :format "%v" (symbol :tag "major-mode") function)))

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

(defcustom w3m-use-ange-ftp nil
  "*Non-nil means that `ange-ftp' or `efs' is used to access FTP servers."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-imitate-widget-button '(eq major-mode 'gnus-article-mode)
  "*If non-nil, imitate the widget button on link (anchor) buttons.
It is useful for moving about in a Gnus article buffer using TAB key.
It can also be any Lisp form that should return a boolean value."
  :group 'w3m
  :type 'sexp)

(defcustom w3m-treat-image-size (and (member "image" w3m-compile-options) t)
  "*Non-nil means to let the w3m HTML rendering be conscious of image size.
`w3m-pixels-per-character' is used for the `-ppc' argument of the w3m command.
`w3m-pixels-per-line' is used for the `-ppl' argument of the w3m command."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-pixels-per-line 64
  "*This value is used for the `-ppl' argument of the w3m command.
If nil, the height of the default face font is used.
It is valid only when `w3m-treat-image-size' is non-nil."
  :group 'w3m
  :type '(choice (const :tag "Auto Detect" nil)
		 (integer :tag "Specify Pixels")))

(defcustom w3m-pixels-per-character nil
  "*This value is used for the `-ppc' argument of the w3m command.
If nil, the width of the default face font is used.
It is valid only when `w3m-treat-image-size' is non-nil."
  :group 'w3m
  :type '(choice (const :tag "Auto Detect" nil)
		 (integer :tag "Specify Pixels")))

(defvar w3m-accept-japanese-characters
  (and (not noninteractive)
       (featurep 'mule)
       (or (memq w3m-type '(w3mmee w3m-m17n))
	   ;; Detect that the internal character set of `w3m-command'
	   ;; is EUC-JP.
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
		 (string= (buffer-substring (point) (min (+ 4 (point))
							 (point-max)))
			  (string ?\264 ?\301 ?\273 ?\372)))))))
  "Non-nil means that `w3m-command' accepts Japanese characters.")

(defcustom w3m-coding-system (if (featurep 'mule)
				 (if (eq w3m-type 'w3mmee)
				     'iso-2022-7bit-ss2
				   'iso-2022-7bit)
			       'iso-8859-1)
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
   ((not (featurep 'mule)) 'iso-8859-1)
   ((eq w3m-type 'w3mmee) 'ctext)
   ((and (eq w3m-type 'w3m-m17n) w3m-use-mule-ucs) 'utf-8)
   ((and (eq w3m-type 'w3m-m17n)) 'iso-2022-7bit-ss2)
   (w3m-accept-japanese-characters 'w3m-euc-japan)
   (t 'w3m-iso-latin-1))
  "*Coding system for read operations of `w3m'."
  :group 'w3m
  :type 'coding-system)

(defcustom w3m-file-coding-system (if (featurep 'mule)
				      'iso-2022-7bit
				    'iso-8859-1)
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
	  (const :tag "Use Lynx-like key mapping." nil))
  ;; Since the following form won't be byte-compiled, you developers
  ;; should never use CL macros like `caaaar', `when', `unless' ...
  :set (lambda (symbol value)
	 (prog1
	     (if (fboundp 'custom-set-default)
		 (custom-set-default symbol value)
	       ;; XEmacs or Emacs 19 does not have `custom-set-default'.
	       (set-default symbol value))
	   (if (or noninteractive
		   ;; Loading w3m.elc is just in progress...
		   (not (featurep 'w3m)))
	       nil
	     (if (and;; Gnus binds `w3m-mode-map' for compiling.
		  (boundp 'w3m-mode-map)
		  (boundp 'w3m-info-like-map)
		  (boundp 'w3m-lynx-like-map))
		 ;; It won't be bound at the first time.
		 (setq w3m-mode-map (if (eq value 'info)
					w3m-info-like-map
				      w3m-lynx-like-map)
		       w3m-minor-mode-map (w3m-make-minor-mode-keymap)))
	     (let ((buffers (buffer-list)))
	       (save-excursion
		 (while buffers
		   (set-buffer (car buffers))
		   (if (eq major-mode 'w3m-mode)
		       (condition-case nil
			   (progn
			     (use-local-map w3m-mode-map)
			     (w3m-setup-toolbar)
			     (w3m-setup-menu))
			 (error)))
		   (setq buffers (cdr buffers)))))))))

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

(defcustom w3m-init-file "~/.emacs-w3m"
  "*Your emacs-w3m startup file name.
If a file with the `.el' or `.elc' suffixes exists, it will be read
instead.

Note: The file pointed by this variable is used as the startup file
for emacs-w3m, but is *NOT* used as a startup file for w3m which works
on terminal.  In order to modify configurations of w3m which works on
terminal, you must edit the startup file for itself, whose name is
~/.w3m/config typically."
  :group 'w3m
  :type 'file)

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
  "*File which keep the list of arrived URLs."
  :group 'w3m
  :type 'file)

(defcustom w3m-keep-arrived-urls 500
  "*Maximum number of arrived URLs."
  :group 'w3m
  :type 'integer)

(defcustom w3m-keep-cache-size 300
  "*Cache size of w3m."
  :group 'w3m
  :type 'integer)

(defcustom w3m-follow-redirection 9
  "*Follow this number of redirections.
If this value is nil, w3m command itself follows redirection.
If you want to use cookies, (i.e. set `w3m-use-cookies' as non-nil),
you should not set this value as nil because many cookie enabled pages set
cookies between redirections."
  :group 'w3m
  :type 'integer)

(defcustom w3m-resize-image-scale 50
  "*A number of percent used to resize inline images."
  :group 'w3m
  :type 'integer)

(defcustom w3m-redirect-with-get t
  "*If non-nil, use GET method after redirection by 301/302.
RFC 1945 and RFC 2068 specify that the client is not allowed
to change the method on the redirected request.  However, most
existing user agent implementations treat 302 as if it were a 303
response, performing a GET on the Location field-value regardless
of the original request method. -- RFC2616"
  :group 'w3m
  :type 'boolean)

(defface w3m-anchor-face
  '((((class color) (background light)) (:foreground "blue"))
    (((class color) (background dark)) (:foreground "cyan"))
    (t (:underline t)))
  "Face used to fontify anchors."
  :group 'w3m-face)

(defface w3m-arrived-anchor-face
  '((((class color) (background light)) (:foreground "navy"))
    (((class color) (background dark)) (:foreground "LightSkyBlue"))
    (t (:underline t)))
  "Face used to fontify anchors, if arrived."
  :group 'w3m-face)

(defface w3m-current-anchor-face
  `((t (:underline t :bold t)))
  "Face used to highlight current anchor."
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
	;; Both `face-custom-attributes-get' in XEmacs and
	;; `custom-face-attributes-get' in CUSTOM 1.9962 attempt to
	;; require `font' in Emacs/w3 and `cl' unconditionally. :-(
	(features (cons 'font features))
	base-attributes attributes attribute)
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

(defface w3m-bold-face `((t (,@w3m-default-face-colors :bold t)))
  "Face used to fontify bold characters."
  :group 'w3m-face)

(defface w3m-underline-face `((t (,@w3m-default-face-colors :underline t)))
  "Face used to fontify underlined part."
  :group 'w3m-face)

(defcustom w3m-mode-hook nil
  "*Hook run after `w3m-mode' called."
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
  '(w3m-move-point-for-localcgi
    w3m-history-highlight-current-url)
  "*Hook run at the end of `w3m-goto-url'."
  :group 'w3m
  :type 'hook
  :initialize 'w3m-custom-hook-initialize)

(defcustom w3m-after-cursor-move-hook
  '(w3m-highlight-current-anchor
    w3m-print-this-url
    w3m-auto-show)
  "*Hook run after cursor moved in w3m buffers."
  :group 'w3m
  :type 'hook
  :initialize 'w3m-custom-hook-initialize)

(defcustom w3m-delete-buffer-hook
  '(w3m-pack-buffer-numbers)
  "*Hook run when any w3m buffers are deleted."
  :group 'w3m
  :type 'hook
  :initialize 'w3m-custom-hook-initialize)

(defcustom w3m-select-buffer-hook nil
  "*Hook run when a different w3m buffer is selected."
  :group 'w3m
  :type 'hook)

(defcustom w3m-async-exec t
  "*If non-nil, w3m is executed as an asynchronous process."
  :group 'w3m
  :type 'boolean)

;; As far as we know, Emacs 20/21 under MacOS X[1] and XEmacs under
;; Solaris[2] won't run the asynchronous operations correctly when
;; both `w3m-async-exec' and `w3m-process-connection-type' are non-nil;
;; [1] the final kilobyte or so might get lost from raw data downloaded
;; from a web site; [2] XEmacs hangs up.

(defcustom w3m-process-connection-type
  (not (or (memq system-type '(macos))
	   (and (featurep 'xemacs)
		(string-match "solaris" system-configuration))))
  "*Process connection type for w3m execution."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-default-content-type "text/html"
  "*Default content type of local files."
  :group 'w3m
  :type 'string)

(defvar w3m-image-viewer
  (or (w3m-which-command "display")
      (w3m-which-command "eeyes")
      (w3m-which-command "xloadimage")
      (w3m-which-command "xv"))
  "*Command to view image files.
Note: this option is installed temporally.  It will be abolished by
the implement of the mailcap parser to set `w3m-content-type-alist'.")

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
    `(("text/html" "\\.s?html?$"
       ,(if (and (condition-case nil (require 'browse-url) (error nil))
		 (fboundp 'browse-url-netscape))
	    'browse-url-netscape
	  '("netscape" url)))
      ("text/plain" "\\.\\(txt\\|tex\\|el\\)" nil)
      ("image/jpeg" "\\.jpe?g$" (,w3m-image-viewer file))
      ("image/png" "\\.png$" (,w3m-image-viewer file))
      ("image/gif" "\\.gif$" (,w3m-image-viewer file))
      ("image/tiff" "\\.tif?f$" (,w3m-image-viewer file))
      ("image/x-xwd" "\\.xwd$" (,w3m-image-viewer file))
      ("image/x-xbm" "\\.xbm$" (,w3m-image-viewer file))
      ("image/x-xpm" "\\.xpm$" (,w3m-image-viewer file))
      ("image/x-bmp" "\\.bmp$" (,w3m-image-viewer file))
      ("video/mpeg" "\\.mpe?g$" ("mpeg_play" file))
      ("video/quicktime" "\\.mov$" ("mpeg_play" file))
      ("application/postscript" "\\.\\(ps\\|eps\\)$" ("gv" file))
      ("application/pdf" "\\.pdf$" ("acroread" file))))
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

;; FIXME: w3m-encoding-type-alist / w3m-decoder-alist / w3m-encoding-alist
;; の相互の関係が複雑かつ冗長なので整理の必要あり．
(defcustom w3m-encoding-type-alist
  '(("\\.gz\\'" . "gzip")
    ("\\.bz2?\\'" . "bzip"))
  "*Alist of file suffixes vs. content encoding types."
  :group 'w3m
  :type '(repeat
	  (cons
	   (string :tag "Regexp of Suffixes")
	   (string :tag "Encoding Type"))))

(defcustom w3m-decoder-alist
  `((gzip "gzip" ("-d"))	;; Don't use "gunzip" and "bunzip2"
    (bzip "bzip2" ("-d"))	;; for broken OS & environment
    (deflate
      ,(if (not noninteractive)
	   (let ((exec-path
		  (let ((prefix (file-name-directory
				 (directory-file-name
				  (file-name-directory
				   (w3m-which-command w3m-command))))))
		    (list (expand-file-name "libexec/w3m" prefix)
			  (expand-file-name "lib/w3m" prefix)))))
	     (w3m-which-command "inflate")))
      nil))
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
	   (windows-1250  . cp1250)
	   (windows-1251  . cp1251)
	   (windows-1252  . cp1252)
	   (windows-1253  . cp1253)
	   (windows-1254  . cp1254)
	   (windows-1255  . cp1255)
	   (windows-1256  . cp1256)
	   (windows-1257  . cp1257)
	   (windows-1258  . cp1258)
	   (euc-jp	  . euc-japan)
	   (shift-jis     . shift_jis)
	   (shift_jis     . shift_jis)
	   (sjis	  . shift_jis)
	   (x-euc-jp      . euc-japan)
	   (x-shift-jis   . shift_jis)
	   (x-shift_jis   . shift_jis)
	   (x-sjis	  . shift_jis)))
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

(defcustom w3m-horizontal-shift-columns 2
  "*Column size to shift horizontally."
  :group 'w3m
  :type 'integer)

(defcustom w3m-use-form t
  "*Non-nil means form extension is activated. (EXPERIMENTAL)"
  :group 'w3m
  :type 'boolean
  :require 'w3m-form)

(defcustom w3m-use-cookies nil
  "Non-nil means using cookies. (EXPERIMENTAL)"
  :group 'w3m
  :type 'boolean)

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

(defcustom w3m-use-favicon (featurep 'w3m-image)
  "*If non-nil, use favicon.
It will be set to nil automatically if ImageMagick's
`convert' does not support a ico format.  You can inhibit the
use of ImageMagick absolutely by setting this option to nil."
  :get (lambda (symbol)
	 (and (not noninteractive)
	      (default-value symbol)
	      (featurep 'w3m-image)
	      (w3m-favicon-usable-p)))
  :set (lambda (symbol value)
	 (funcall (if (fboundp 'custom-set-default)
		      'custom-set-default
		    'set-default)
		  symbol
		  (and (not noninteractive)
		       value
		       (featurep 'w3m-image)
		       (w3m-favicon-usable-p))))
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

(defcustom w3m-view-this-url-new-session-in-background nil
  "Execute `w3m-view-this-url' without switching to the newly created buffer."
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

(defcustom w3m-auto-show t
  "*Use internal auto-show method."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-horizontal-scroll-division 4
  "*Division number of horizontal scroll."
  :group 'w3m
  :type 'integer)

(defcustom w3m-use-refresh t
  "*If non-nil, support REFRESH attribute in META tags."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-mbconv-command "mbconv"
  "*Command name for \"mbconv\" be supplied with \"libmoe\"."
  :group 'w3m
  :type 'string)

(defcustom w3m-local-find-file-regexps '(nil . "\\.html?\\'")
  "*Cons of two regexps matching and not matching local file names which
will be opened by the function specified by the
`w3m-local-find-file-function' variable.  Nil matches any file names,
for instance, the value `(nil . \"\\\\.html?\\\\'\")' does not match
\"file:///any/where/index.html\" but \"file:///some/where/w3m.el\".  It
only affects when the `w3m-local-find-file-function' variable is set
properly (see also the documentation for that variable)."
  :group 'w3m
  :type '(cons (radio :tag "Match"
		      (const :format "All " nil) regexp)
	       (radio :tag "Nomatch"
		      (const :format "All " nil) regexp)))

(defcustom w3m-local-find-file-function
  '(if (or (and (featurep 'xemacs)
		(device-on-window-system-p))
	   window-system)
       'find-file-other-frame
     'find-file-other-window)
  "*Function used to open local files whose name matches the
`w3m-local-find-file-regexps' variable.  Function should take one
argument, the string naming the local file.  It can also be any Lisp
form that should return a function.  Set this to nil if you want to
always use emacs-w3m to see local files."
  :group 'w3m
  :type 'sexp)

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
			 (, (if (not noninteractive)
				(expand-file-name
				 (concat "../lib/"
					 (file-name-nondirectory w3m-command)
					 "/dirlist.cgi")
				 (file-name-directory
				  (w3m-which-command w3m-command)))))))))

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

(defcustom w3m-touch-command
  (w3m-which-command "touch")
  "*Name of the executable file of touch utilty."
  :group 'w3m
  :type 'string)

(defcustom w3m-uri-replace-alist
  '(("\\`gg:" w3m-search-uri-replace "google")
    ("\\`ggg:" w3m-search-uri-replace "google groups")
    ("\\`ya:" w3m-search-uri-replace "yahoo")
    ("\\`al:" w3m-search-uri-replace "altavista")
    ("\\`bts:" w3m-search-uri-replace "debian-bts")
    ("\\`dpkg:" w3m-search-uri-replace "debian-pkg")
    ("\\`archie:" w3m-search-uri-replace "iij-archie")
    ("\\`urn:ietf:rfc:\\([0-9]+\\)" w3m-pattern-uri-replace
     "http://www.ietf.org/rfc/rfc\\1.txt"))
  "*Alist of a regexp matching a uri and its replacement.

Each element of this alist is (REGEXP FUNCTION OPTIONS...).  FUNCTION
should take one or more arguments, a uri and OPTIONS.  When this
FUNCTION is called, sub-strings found in matching REGEXP can be
refered.  Here are some predefined functions meant for use in this
way:

`w3m-pattern-uri-replace'
    Replace a uri with PATTERN.  In PATTERN, `\' is treated as special
    in the same manner of `replace-match'.

`w3m-search-uri-replace'
    Generate a query from a uri for specified engine.
"
  :group 'w3m
  :type '(repeat
	  (list
	   :convert-widget
	   (lambda (widget)
	     (require 'w3m-search)
	     (list
	      'choice :format "%[Value Menu%] %v" :tag "Replacing URI with"
	      :args
	      (append
	       '((list :tag "Replacement Using Pattern"
		       (string :tag "Regexp" :value "")
		       (function-item :format "" w3m-pattern-uri-replace)
		       (string :tag "Pattern" :value "")))
	       '((list :format "Quick Search:\n%v" :tag "Quick Search"
		       (regexp :tag "Prefix URI Regexp")
		       (function-item :format "" w3m-search-uri-replace)
		       (string :tag "Quick Search Engine")))
	       (mapcar
		(lambda (elem)
		  (let ((engine (car elem))
			prefix)
		    (setq prefix (mapconcat 'identity
					    (split-string (downcase engine))
					    "-"))
		    (list 'list
			  :format "Quick Search:\n%v"
			  :tag (concat "Quick Search: " prefix)
			  (list 'regexp
				:tag "Prefix URL Regexp"
				(concat "\\`" (regexp-quote prefix) ":"))
			  '(function-item :format "" w3m-search-uri-replace)
			  (list 'string :tag "Quick Search Engine" engine))))
		w3m-search-engine-alist)
	       '((list :tag "User Defined Function"
		       (string :tag "Regexp" :value "")
		       (function)
		       (repeat :tag "Options" sexp)))))))))

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
			   (make-char (w3m-static-if (boundp 'MULE)
					  lc-ltn1
					'latin-iso8859-1)
				      (cdr entity))))))
		 latin1-entity))))))

(defconst w3m-entity-regexp
  "&\\([a-z][a-z0-9]*\\|#[0-9]+\\|#x[0-9a-f]+\\)\\(;\\)?")

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
R0lGODlhUwAOAPIFAP///38AvwAAv1Uq/AC/AL9/Af8AAAAAACH/C05FVFNDQVBFMi4wAwEA
AAAh+QQEIQD/ACwAAAAAUwAOAEADwgi63P4wykldCBYDIRT33RYOQ0ScU1EAKrs2hqHEswzQ
ynUte78vnA9DQGKQihIUQNliNGlQXGVKrVqv2Oy06Zr+vpqfcDgqAZDHCvPVVHFxcJsOAAbZ
RWdkfn9GKSVtbC83NnE1WoiIblyJjY4KXIwSdXQ+GEKYIXtoZnoPa5CDoTUyMaVyOpRimkFC
ngtHZksntAQKBCstkiyFvYeVGXeZCpzEsgt/EYGjv4aEPHPCIWNEJXppyLQVy6RSzlKP4eIJ
ACH5BAQhAP8ALAAAAAABAAEAAAMCCAkAIfkEBCEA/wAsAQABAFEACwAAAx0Iutz+MMpJq704
6827/2AojmRpnmiqrmzrvnBsJgAh+QQEIQD/ACwBAAQAEQAIAAADHAixAb6qMRgfZDbXHaem
nBOKINmIpnSWqIe+TgIAIfkEBCEA/wAsCQAEABcACAAAAykYAcoNUEgBq1ALAxnp9tXzcFVX
OtAITmS4fCYsuzHZnqhsg/geb7hKAgAh+QQEIQD/ACwXAAQAFAAIAAADJSgKzDKjAQjbYrXO
uJ3AnEZZytdJoAZcKuqKIeduchaidjPOQAIAIfkEBCEA/wAsJgAEABUACAAAAyA4OtD+gJAm
GVAQyodz3s5iRVI5RR/ordTJapP6OuacJQAh+QQEIQD/ACw2AAEAEAALAAADJgi6XO4sAqio
nGU5ogjH0VZlYFN8VPoARAZZGviSlzpKdmneF5AAADs="
  "A small icon image for the url about://emacs-w3m.gif.  It is encoded
in the optimized interlaced endlessly animated gif format and base64.")

(defconst w3m-modeline-process-status-on "<PRC>"
  "Modeline string which is displayed when the process is runnning now.")

(defconst w3m-modeline-image-status-on "[IMG]"
  "Modeline string which is displayed when inline image is on.")

(defconst w3m-modeline-status-off "[ - ]"
  "Modeline string which is displayed when default status.")

(defconst w3m-modeline-ssl-image-status-on "[IMG(SSL)]"
  "Modeline string which is displayed when inline image is on and use SSL connection.")

(defconst w3m-modeline-ssl-status-off "[SSL]"
  "Modeline string which is displayed when SSL connection status.")

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
(defvar w3m-current-content-charset nil "Current content-charset of this buffer.")
(defvar w3m-icon-data nil
  "Cons cell of icon URL and its IMAGE-TYPE of this buffer.")
(defvar w3m-next-url nil "Next URL of this buffer.")
(defvar w3m-previous-url nil "Previous URL of this buffer.")
(defvar w3m-start-url nil "Start URL of this buffer.")
(defvar w3m-contents-url nil "Table of Contents URL of this buffer.")
(defvar w3m-max-anchor-sequence nil "Maximum number of anchor sequence on this buffer.")
(defvar w3m-current-refresh nil "Cons pair of refresh attribute, '(sec . url).")
(defvar w3m-current-ssl nil "SSL certification indicator.")
(defvar w3m-current-redirect nil "Redirect location.")

(make-variable-buffer-local 'w3m-current-url)
(make-variable-buffer-local 'w3m-current-base-url)
(make-variable-buffer-local 'w3m-current-title)
(make-variable-buffer-local 'w3m-current-forms)
(make-variable-buffer-local 'w3m-current-coding-system)
(make-variable-buffer-local 'w3m-current-content-charset)
(make-variable-buffer-local 'w3m-icon-data)
(make-variable-buffer-local 'w3m-next-url)
(make-variable-buffer-local 'w3m-previous-url)
(make-variable-buffer-local 'w3m-start-url)
(make-variable-buffer-local 'w3m-contents-url)
(make-variable-buffer-local 'w3m-max-anchor-sequence)
(make-variable-buffer-local 'w3m-current-refresh)
(make-variable-buffer-local 'w3m-current-ssl)
(make-variable-buffer-local 'w3m-current-redirect)

(defsubst w3m-clear-local-variables ()
  (setq w3m-current-url nil
	w3m-current-base-url nil
	w3m-current-title nil
	w3m-current-coding-system nil
	w3m-current-content-charset nil
	w3m-icon-data nil
	w3m-next-url nil
	w3m-previous-url nil
	w3m-start-url nil
	w3m-contents-url nil
	w3m-max-anchor-sequence nil
	w3m-current-refresh nil
	w3m-current-ssl nil
	w3m-current-redirect nil))

(defsubst w3m-copy-local-variables (from-buffer)
  (let (url base title forms cs char icon next prev
	    start toc hseq refresh ssl redirect)
    (with-current-buffer from-buffer
      (setq url w3m-current-url
	    base w3m-current-base-url
	    title w3m-current-title
	    cs w3m-current-coding-system
	    char w3m-current-content-charset
	    icon w3m-icon-data
	    next w3m-next-url
	    prev w3m-previous-url
	    start w3m-start-url
	    toc w3m-contents-url
	    hseq w3m-max-anchor-sequence
	    refresh w3m-current-refresh
	    ssl w3m-current-ssl
	    redirect w3m-current-redirect))
    (setq w3m-current-url url
	  w3m-current-base-url base
	  w3m-current-title title
	  w3m-current-coding-system cs
	  w3m-current-content-charset char
	  w3m-icon-data icon
	  w3m-next-url next
	  w3m-previous-url prev
	  w3m-start-url start
	  w3m-contents-url toc
	  w3m-max-anchor-sequence hseq
	  w3m-current-refresh refresh
	  w3m-current-ssl ssl
	  w3m-current-redirect redirect)))

(defvar w3m-verbose t "Flag variable to control messages.")

(defvar w3m-safe-url-regexp nil "Regexp of URLs which point safe contents.")

(defvar w3m-current-buffer nil)
(defvar w3m-cache-buffer nil)
(defvar w3m-cache-articles nil)
(defvar w3m-cache-hashtb nil)
(defvar w3m-input-url-history nil)

(defconst w3m-arrived-db-size 1023)
(defvar w3m-arrived-db nil)		; nil means un-initialized.
(defvar w3m-arrived-setup-functions nil
  "Internal functions run at the end of `w3m-arrived-setup'.")
(defvar w3m-arrived-shutdown-functions nil
  "Functions run at the end of `w3m-arrived-shutdown'.")

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
      (let ((a (decode-coding-string "\e$B%\"\e(B" 'iso-2022-jp)));;ア
	`([w3m-toolbar-back-icon w3m-view-previous-page
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
	  [w3m-toolbar-antenna-icon w3m-antenna t
				    ,(concat a "ンテナで受信する")]
	  [w3m-toolbar-history-icon w3m-history t "ヒストリー"]
	  [w3m-toolbar-db-history-icon w3m-db-history t
				       "過去に訪問した URL の履歴を見る"]))
    '([w3m-toolbar-back-icon w3m-view-previous-page
			     (w3m-history-previous-link-available-p)
			     "Back to Previous Page"]
      [w3m-toolbar-parent-icon w3m-view-parent-page
			       (w3m-parent-page-available-p)
			       "View the parent page"]
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
      [w3m-toolbar-db-history-icon w3m-db-history t "View Arrived URLs"]))
  "Toolbar definition for w3m.")

(defconst w3m-menubar
  '("W3M"
    ["Go to Home Page" w3m-gohome w3m-home-page]
    ["Back to Previous Page" w3m-view-previous-page
     (w3m-history-previous-link-available-p)]
    ["Forward to Next Page" w3m-view-next-page
     (w3m-history-next-link-available-p)]
    ["View the parent page" w3m-view-parent-page
     (w3m-parent-page-available-p)]
    ;;
    ["Reload This Page" w3m-reload-this-page w3m-current-url]
    ["Cancel Process" w3m-process-stop t]
    ["Toggle Images" w3m-toggle-inline-images (w3m-display-graphic-p)]
    ;;
    ["Redisplay This Page" w3m-redisplay-this-page w3m-current-url]
    ["Redisplay This Page with Charset"
     w3m-redisplay-with-charset w3m-current-url]
    ["Redisplay This Page with Content-type"
     w3m-redisplay-with-content-type w3m-current-url]
    ["Reset Charset and Content-type" w3m-redisplay-and-reset w3m-current-url]
    ;;
    ["Go to..." w3m-goto-url t]
    ["Go to... in the New Session" w3m-goto-url-new-session t]
    ["Make a Copy of This Session" w3m-copy-buffer t]
    ;;
    ["View Bookmark" w3m-bookmark-view t]
    ["Search the Internet" w3m-search t]
    ["Weather Forecast" w3m-weather t]
    ["Investigate with Antenna" w3m-antenna t]
    ;;
    ["View Source" w3m-view-source t]
    ["View Header" w3m-view-header t]
    ["Show a History" w3m-history t]
    ["View Arrived URLs" w3m-db-history t]
    ;;
    ["Download This URL" w3m-download-this-url t]
    ["Print the Current URL" w3m-print-current-url t]
    ;;
    ["Send a Bug Report" report-emacs-w3m-bug t]
    )
  "Menubar definition for w3m.")

(defvar w3m-cid-retrieve-function-alist nil)
(defvar w3m-force-redisplay t)

(defvar w3m-work-buffer-list nil)
(defconst w3m-work-buffer-name " *w3m-work*")
(defconst w3m-select-buffer-name " *w3m buffers*")

(defconst w3m-meta-content-type-charset-regexp
  (eval-when-compile
    (concat "<meta[ \t]+http-equiv=\"?Content-type\"?[ \t]+"
	    "content=\"?\\([^;]+\\);[ \t]*charset=\\([^\"]+\\)\"?"
	    "[ \t]*/?>"))
  "Regexp used in parsing `<META HTTP-EQUIV=\"Content-Type\" content=\"...;charset=...\">
for a charset indication")

(defconst w3m-meta-charset-content-type-regexp
  (eval-when-compile
    (concat "<meta[ \t]+content=\"?\\([^;]+\\);[ \t]*charset=\\([^\"]+\\)\"?"
	    "[ \t]+http-equiv=\"?Content-type\"?[ \t]*/?>"))
  "Regexp used in parsing `<META content=\"...;charset=...\" HTTP-EQUIV=\"Content-Type\">
for a charset indication")

(defconst w3m-meta-refresh-content-regexp
  (eval-when-compile
    (concat "<meta[ \t]+http-equiv=\"?refresh\"?[ \t]+"
	    "content=\"?\\([^;]+\\);[ \t]*url=\\([^\"]+\\)\"?"
	    "[ \t]*/?>"))
  "Regexp used in parsing `<META HTTP-EQUIV=\"Refresh\" content=\"n;url=...\">
for a refresh indication")

(defconst w3m-meta-content-refresh-regexp
  (eval-when-compile
    (concat "<meta[ \t]+content=\"?\\([^;]+\\);[ \t]*url=\\([^\"]+\\)\"?"
	    "[ \t]+http-equiv=\"?refresh\"?[ \t]*/?>"))
  "Regexp used in parsing `<META content=\"n;url=...\" HTTP-EQUIV=\"Refresh\">
for a refresh indication")

(eval-and-compile
  (defconst w3m-html-string-regexp
    "\\(\"\\([^\"]+\\)\"\\|'\\([^\']+\\)'\\|[^\"\'<> \t\r\f\n]*\\)"
    "Regexp used in parsing to detect string."))

(defconst w3m-dump-head-source-command-arguments
  (cond ((eq w3m-type 'w3mmee)
	 (list "-dump=extra,head,source"))
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
	 (list '(if w3m-treat-image-size
		    "-dump=half-buffer,single-row-image"
		  "-dump=half-buffer")
	       '(if charset "-I")
	       'charset
	       "-o" "concurrent=0"))
	((eq w3m-type 'w3m-m17n)
	 (list "-halfdump"
	       "-o" "ext_halfdump=1"
	       "-o" "strict_iso2022=0"
	       '(if charset "-I") 'charset
	       "-O"
	       (if w3m-use-mule-ucs "UTF-8" "ISO-2022-JP-2" )))
	((eq w3m-input-coding-system 'w3m-euc-japan)
	 (list "-halfdump" "-I" "e"))
	(t (list "-halfdump")))
  "Arguments for 'halfdump' execution of w3m.")

(defconst w3m-halfdump-command-common-arguments
  '("-T" "text/html" "-t" tab-width
    "-cols" (if (< 0 w3m-fill-column)
		w3m-fill-column		; fixed columns
	      (+ (window-width) (or w3m-fill-column -1)))) ; fit for frame
  "Common arguments for 'halfdump' execution of all w3m variants.")

(defconst w3m-arrived-ignored-regexp
  "\\`about:\\(//\\(header\\|source\\|history\\|db-history\\|antenna\\|namazu\\|dtree\\)/.*\\)?$"
  "Regexp of urls to be ignored in an arrived-db.")

(defconst w3m-history-ignored-regexp
  "\\`about:\\(//\\(header\\|source\\|history\\|db-history\\|antenna\\|namazu\\|dtree\\)/.*\\)?$"
  "Regexp of urls to be ignored in a history.")

(defconst w3m-url-components-regexp
  "\\`\\(\\([^:/?#]+\\):\\)?\\(//\\([^/?#]*\\)\\)?\\([^?#]*\\)\\(\\?\\([^#]*\\)\\)?\\(#\\(.*\\)\\)?\\'"
  "Regular expression for parsing the potential four components and
fragment identifier of a URI reference.  For more detail, see Appendix
B of RFC2396 <URL:http://www.ietf.org/rfc/rfc2396.txt>.")

(defvar w3m-mode-map nil "Keymap used in w3m-mode buffers.")

(defvar w3m-mode-setup-functions nil
  "Functions run after `w3m-mode' called.")
(defvar w3m-display-functions nil
  "Functions run at the end of `w3m-goto-url'.")

(defvar w3m-load-hook nil
  "*Hook run after loading emacs-w3m.
It is recommended that customize code is put into `w3m-init-file'
instead of this hook.")


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
    (setq url
	  (if (and w3m-treat-drive-letter
		   (string-match
		    "\\`/\\(\\([a-zA-Z]\\)[|:]?\\|cygdrive/\\([a-zA-Z]\\)\\)/" url))
	      (concat (or (match-string 2 url) (match-string 3 url))
		      ":/"
		      (substring url (match-end 0)))
	    url))
    (if (file-exists-p url)
	url
      (let ((x (w3m-url-decode-string url w3m-file-name-coding-system)))
	(if (file-exists-p x) x url))))
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
	       (when (or (file-exists-p file)
			 (file-exists-p
			  (setq file (w3m-url-decode-string
				      file w3m-file-name-coding-system))))
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
    (setq url (w3m-url-strip-fragment url))
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
      `(if (and (boundp 'emacs-major-version)
		(>= emacs-major-version 21))
	   (function
	    (lambda (extent)
	      (if (and w3m-track-mouse
		       (eq (extent-object extent) (current-buffer)))
		  (get-text-property (extent-start-position extent)
				     ',property)))))
    `(if (and (boundp 'emacs-major-version)
	      (>= emacs-major-version 21))
	 (function
	  (lambda (window object pos)
	    (if w3m-track-mouse
		(progn
		  (w3m-display-message "");; Clear the echo area.
		  (get-text-property pos ',property
				     (window-buffer window)))))))))

(defmacro w3m-make-balloon-help (property)
  "Make a function for showing a `balloon-help' under XEmacs."
  (when (featurep 'xemacs)
    `(let ((fn (intern (format "w3m-balloon-help-for-%s"
			       ',property))))
       (prog1
	   fn
	 (unless (fboundp fn)
	   (defalias fn
	     (lambda (extent)
	       (if (and w3m-track-mouse
			(eq (extent-object extent) (current-buffer)))
		   (get-text-property (extent-start-position extent)
				      ',property)))))
	 (when (and (featurep 'bytecomp)
		    (not (compiled-function-p (symbol-function fn))))
	   (byte-compile fn))))))

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
(unless (equal (w3m-time-parse-string "Thursday, 01-Jan-1970 00:00:00 GMT")
	       '(0 0))
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

(defun w3m-save-list (file list &optional coding-system escape-ctl-chars)
  "Save LIST form into FILE.  Contents will be encoded with CODING-SYSTEM
which defaults to the value of `w3m-file-coding-system'.  Optional
ESCAPE-CTL-CHARS if it is non-nil, control chars will be represented
with ^ as `cat -v' does."
  (when (and list (file-writable-p file))
    (with-temp-buffer
      (let ((file-coding-system (or coding-system w3m-file-coding-system))
	    (coding-system-for-write (or coding-system w3m-file-coding-system))
	    (standard-output (current-buffer))
	    (print-fn (if escape-ctl-chars
			  'w3m-prin1
			'prin1))
	    element print-length print-level)
	(insert (format "\
;;; %s  -*- mode: emacs-lisp%s -*-
;; This file is generated automatically by emacs-w3m v%s.

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
		(funcall print-fn (car element))
		(insert "\n")
		(while (setq element (cdr element))
		  (insert "  ")
		  (funcall print-fn (car element))
		  (insert "\n"))
		(backward-delete-char 1)
		(insert ")\n "))
	    (funcall print-fn element)
	    (insert "\n")))
	(skip-chars-backward "\n ")
	(delete-region (point) (point-max))
	(insert ")\n")
	(let ((mode (and (file-exists-p file)
			 (file-modes file))))
	  (write-region (point-min) (point-max) file nil 'nomsg)
	  (when mode (set-file-modes file mode)))))))

(defun w3m-arrived-add (url &optional title modified-time
			    arrived-time content-charset content-type)
  "Add URL to hash database of arrived URLs."
  (unless (string-match w3m-arrived-ignored-regexp url)
    (let ((ident (intern url w3m-arrived-db)))
      (if (string-match "\\`\\([^#]+\\)#" url)
	  (w3m-arrived-add (substring url 0 (match-end 1))
			   title modified-time arrived-time
			   content-charset content-type)
	(put ident 'content-charset content-charset)
	(put ident 'content-type content-type))
      (put ident 'title title)
      (put ident 'last-modified modified-time)
      (set ident arrived-time)
      ident)))

(defsubst w3m-arrived-p (url)
  "If URL has been arrived, return non-nil value.  Otherwise return nil."
  (or (string-match w3m-arrived-ignored-regexp url)
      (intern-soft url w3m-arrived-db)))

(defun w3m-arrived-modify (url &optional title modified-time
			       arrived-time content-charset content-type)
  "Modify hash database of URL. If some optional argument is 'reset, reset its."
  (when (and (w3m-arrived-p url)
	     (or title modified-time arrived-time content-charset content-type))
    (w3m-arrived-add url
		     (cond ((eq title 'reset) nil)
			   (title title)
			   (t (w3m-arrived-title url)))
		     (cond ((eq modified-time 'reset) nil)
			   (modified-time modified-time)
			   (t (w3m-arrived-last-modified url)))
		     (cond ((eq arrived-time 'reset) nil)
			   (arrived-time arrived-time)
			   (t (w3m-arrived-time url)))
		     (cond ((eq content-charset 'reset) nil)
			   (content-charset content-charset)
			   (t (w3m-arrived-content-charset url)))
		     (cond ((eq content-type 'reset) nil)
			   (content-type content-type)
			   (t (w3m-arrived-content-type url))))))

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

(defun w3m-arrived-load-list ()
  (let ((list (w3m-load-list w3m-arrived-file)))
    ;; When arrived URL database is too old, its data is ignored.
    (when (or
	   ;; Before the revision 1.120, every element of the list was
	   ;; a string that represented an arrived URL.
	   (stringp (car list))
	   ;; Before the revision 1.135, every element was a cons
	   ;; cell: its car kept a URL, and its cdr kept a time when
	   ;; the URL was arrived.
	   ;; Before the revision 1.178, every element was a 4-tuple
	   ;; that consisted of a URL, a title, a modified time, and
	   ;; an arrived time.
	   ;; An element of the modern database is a 6-tuple that
	   ;; consisted of a URL, a title, a modified time, an arrived
	   ;; time, a charset, and a content type.
	   ;; Thus, the following condition eliminates the revision
	   ;; 1.177 and olders.
	   (<= (length (car list)) 4))
      (setq list nil)
      (when (file-exists-p w3m-arrived-file)
	(delete-file w3m-arrived-file)))
    list))

(defun w3m-arrived-setup ()
  "Load arrived url list from `w3m-arrived-file' and setup hash database."
  (unless w3m-arrived-db
    (setq w3m-arrived-db (make-vector w3m-arrived-db-size 0))
    (let ((list (w3m-arrived-load-list)))
      (dolist (elem list)
	;; Ignore an element that lacks an arrived time information.
	(when (nth 3 elem)
	  (w3m-arrived-add (if (string-match "\\`/" (car elem))
			       (w3m-expand-file-name-as-url (car elem))
			     (car elem))
			   (nth 1 elem)
			   (nth 2 elem)
			   (nth 3 elem)
			   (when (stringp (nth 4 elem)) (nth 4 elem))
			   (nth 5 elem))))
      (unless w3m-input-url-history
	(setq w3m-input-url-history (mapcar (function car) list))))
    (run-hooks 'w3m-arrived-setup-functions)))

(defun w3m-arrived-shutdown ()
  "Save hash database of arrived URLs to `w3m-arrived-file'."
  (when w3m-arrived-db
    ;; Re-read arrived DB file, and check sites which are arrived on
    ;; the other emacs process.
    (dolist (elem (w3m-arrived-load-list))
      (when (w3m-time-newer-p (nth 3 elem) (w3m-arrived-time (car elem)))
	(w3m-arrived-add (if (string-match "\\`/" (car elem))
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
	      (symbol-value sym) ; Ignore an entry lacks an arrived time.
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
			      (if (equal (nth 3 a) (nth 3 b))
				  (string< (car a) (car b))
				(w3m-time-newer-p (nth 3 a) (nth 3 b)))))
		      w3m-keep-arrived-urls)
		     nil t))
    (setq w3m-arrived-db nil)
    (run-hooks 'w3m-arrived-shutdown-functions)))

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
	      (format "%%%02x" ch))))	; escape
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
	      (vector (string-to-number (match-string 2 str) 16)))
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

(defsubst w3m-url-transfer-encode-string (url &optional coding)
  "Encode all non-ASCII characters included in URL to sequences of
escaped octets in the specified coding system.
This function is designed for conversion for safe transmission of URL.
Therefore, this function handles only non-ASCII characters that can
not be transmitted safely with network streams.  In general, you
should use `w3m-url-encode-string' instead of this."
  (let ((start 0)
	(buf))
    (while (string-match "[^\x21-\x7e]+" url start)
      (setq buf
	    (cons (apply 'concat
			 (mapcar
			  (lambda (c) (format "%%%02x" c))
			  (append (encode-coding-string
				   (match-string 0 url)
				   (or coding
				       w3m-current-coding-system)))))
		  (cons (substring url start (match-beginning 0))
			buf))
	    start (match-end 0)))
    (apply 'concat
	   (nreverse (cons (substring url start) buf)))))


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

(defun w3m-entity-value (name strict)
  ;; initialize if need
  (unless w3m-entity-db
    (w3m-entity-db-setup))
  ;; return value of specified entity, or nil if unknown.
  (if (char-equal (string-to-char name) ?#)
      (progn
	(setq name (substring name 1))
	(let ((codepoint (if (char-equal (string-to-char name) ?x)
			     (string-to-number (substring name 1) 16)
			   (string-to-number name))))
	  (char-to-string (w3m-ucs-to-char codepoint))))
    (let ((val (intern-soft name w3m-entity-db))
	  (pre name)
	  (post ""))
      (if (not strict)
	  (while (and (null val)
		      (< 0 (length pre))
		      (null (setq val (intern-soft pre w3m-entity-db))))
	    (setq post (concat (substring pre -1) post)
		  pre (substring pre 0 -1))))
      (and val (concat (symbol-value val) post)))))

(defun w3m-fontify-bold ()
  "Fontify bold characters in this buffer which contains half-dumped data."
  (goto-char (point-min))
  (while (re-search-forward "<[\t\n ]*b[^>]*>" nil t)
    (let ((start (match-beginning 0)))
      (delete-region start (match-end 0))
      (when (re-search-forward "<[\t\n ]*/b[\t\n ]*>" nil t)
	(delete-region (match-beginning 0) (match-end 0))
	(w3m-add-text-properties start (match-beginning 0)
				 '(face w3m-bold-face))))))

(defun w3m-fontify-underline ()
  "Fontify underline characters in this buffer which contains half-dumped data."
  (goto-char (point-min))
  (while (re-search-forward "<[\t\n ]*u[^>]*>" nil t)
    (let ((start (match-beginning 0)))
      (delete-region start (match-end 0))
      (when (re-search-forward "<[\t\n ]*/u[\t\n ]*>" nil t)
	(delete-region (match-beginning 0) (match-end 0))
	(w3m-add-text-properties start (match-beginning 0)
				 '(face w3m-underline-face))))))

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

(defun w3m-image-type (content-type)
  "Return image type which corresponds to CONTENT-TYPE."
  (cdr (assoc content-type w3m-image-type-alist)))

(defun w3m-imitate-widget-button ()
  "Return a boolean value corresponding to the variable of the same name."
  (if (listp w3m-imitate-widget-button)
      (condition-case nil
	  (eval w3m-imitate-widget-button)
	(error nil))
    (and w3m-imitate-widget-button t)))

(defun w3m-fontify-anchors ()
  "Fontify anchor tags in this buffer which contains half-dumped data."
  (let ((help (w3m-make-help-echo w3m-href-anchor))
	(balloon (w3m-make-balloon-help w3m-href-anchor))
	prenames start end)
    (goto-char (point-min))
    (setq w3m-max-anchor-sequence 0)	;; reset max-hseq
    (while (re-search-forward "<_id[ \t\r\f\n]+" nil t)
      (setq start (match-beginning 0))
      (setq prenames (get-text-property start 'w3m-name-anchor))
      (w3m-parse-attributes (id)
	(delete-region start (point))
	(w3m-add-text-properties start (point-max)
				 (list 'w3m-name-anchor
				       (cons id prenames)))))
    (goto-char (point-min))
    (while (re-search-forward "<a[ \t\r\f\n]+" nil t)
      (setq start (match-beginning 0))
      (setq prenames (get-text-property start 'w3m-name-anchor))
      (w3m-parse-attributes (href name charset
				  (rel :case-ignore) (hseq :integer))
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
	    (setq href (if (and (string-match w3m-url-components-regexp href)
				(match-beginning 8))
			   (concat (w3m-url-transfer-encode-string
				    (substring href 0 (match-beginning 8))
				    (w3m-charset-to-coding-system charset))
				   "#" (match-string 9 href))
			 (w3m-url-transfer-encode-string
			  href
			  (w3m-charset-to-coding-system charset)))
		  hseq (or (and (null hseq) 0) (abs hseq))
		  w3m-max-anchor-sequence (max hseq w3m-max-anchor-sequence))
	    (w3m-add-text-properties start end
				     (list 'face (if (w3m-arrived-p href)
						     'w3m-arrived-anchor-face
						   'w3m-anchor-face)
					   'w3m-href-anchor href
					   'mouse-face 'highlight
					   'w3m-anchor-sequence hseq
					   'help-echo help
					   'balloon-help balloon))
	    (when (w3m-imitate-widget-button)
	      (require 'wid-edit)
	      (let ((widget-button-face (if (w3m-arrived-p href)
					    'w3m-arrived-anchor-face
					  'w3m-anchor-face))
		    (widget-mouse-face 'highlight))
		(widget-convert-button 'default start end
				       :button-keymap nil
				       :help-echo href)))
	    (when name
	      (w3m-add-text-properties start (point-max)
				       (list 'w3m-name-anchor
					     (cons name prenames))))))
	 (name
	  (w3m-add-text-properties start (point-max)
				   (list 'w3m-name-anchor
					 (cons name prenames)))))))
    (when w3m-icon-data
      (setq w3m-icon-data (cons (w3m-expand-url (car w3m-icon-data))
				(or (w3m-image-type (cdr w3m-icon-data))
				    'ico))))
    (when w3m-next-url
      (setq w3m-next-url (w3m-expand-url w3m-next-url)))
    (when w3m-previous-url
      (setq w3m-previous-url (w3m-expand-url w3m-previous-url)))
    (when w3m-start-url
      (setq w3m-start-url (w3m-expand-url w3m-start-url)))
    (when w3m-contents-url
      (setq w3m-contents-url (w3m-expand-url w3m-contents-url)))))

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
	upper start end)
    (while (re-search-forward "<\\(img_alt\\)[^>]+>" nil t)
      (setq upper (string= (match-string 1) "IMG_ALT")
	    start (match-beginning 0)
	    end (match-end 0))
      (goto-char (match-end 1))
      (w3m-parse-attributes (src
			     (width :integer)
			     (height :integer)
			     usemap)
	(delete-region start end)
	(setq src (w3m-expand-url (w3m-decode-anchor-string src)))
	(when (search-forward "</img_alt>" nil t)
	  (delete-region (setq end (match-beginning 0)) (match-end 0))
	  (w3m-add-text-properties start end
				   (list 'w3m-image src
					 'w3m-image-size
					 (when (or width height)
					   (cons width height))
					 'w3m-image-usemap usemap
					 'w3m-image-status 'off
					 'w3m-image-redundant upper))
	  (unless (or (w3m-anchor start)
		      (w3m-action start))
	    ;; No need to use `w3m-add-text-properties' here.
	    (add-text-properties start end (list 'face 'w3m-image-face
						 'mouse-face 'highlight
						 'help-echo help
						 'balloon-help balloon))))))))

(defsubst w3m-toggle-inline-images-internal (status no-cache url)
  "Toggle displaying of inline images on current buffer.
STATUS is current image status.
If NO-CACHE is non-nil, cache is not used.
If URL is specified, only the image with URL is toggled."
  (interactive "P")
  (let ((cur-point (point))
	(buffer-read-only)
	(end (point-min))
	start iurl image size)
    (save-excursion
      (if (equal status 'off)
	  (while (setq start
		       (if (w3m-image end)
			   end
			 (next-single-property-change end 'w3m-image)))
	    (setq end (or (next-single-property-change start 'w3m-image)
			  (point-max))
		  iurl (w3m-image start)
		  size (get-text-property start 'w3m-image-size))
	    (when (and (or (not url)
			   ;; URL is specified and is same as the image URL.
			   (string= url iurl))
		       (not (eq (get-text-property start 'w3m-image-status)
				'on)))
	      (w3m-add-text-properties start end '(w3m-image-status on))
	      (if (get-text-property start 'w3m-image-redundant)
		  (progn
		    ;; Insert dummy string instead of redundant image.
		    (setq image (make-string
				 (string-width (buffer-substring start end))
				 ? ))
		    (w3m-add-text-properties start end '(invisible t))
		    (goto-char end)
		    (w3m-add-text-properties
		     end (progn (insert image) (point))
		     '(w3m-image-dummy t w3m-image "dummy"))
		    (setq end (point)))
		(goto-char cur-point)
		(when (w3m-url-valid iurl)
		  (w3m-process-with-null-handler
		    (lexical-let ((start (set-marker (make-marker) start))
				  (end (set-marker (make-marker) end))
				  (iurl iurl)
				  (url w3m-current-url))
		      (w3m-process-do
			  (image (let ((w3m-current-buffer (current-buffer)))
				   (w3m-create-image
				    iurl no-cache
				    w3m-current-url
				    size handler)))
			(when (buffer-live-p (marker-buffer start))
			  (with-current-buffer (marker-buffer start)
			    (if image
				(when (equal url w3m-current-url)
				  (let (buffer-read-only)
				    (w3m-insert-image start end image iurl))
				  ;; Redisplay
				  (when w3m-force-redisplay
				    (sit-for 0)))
			      (let (buffer-read-only)
				(w3m-add-text-properties
				 start end '(w3m-image-status off))))
			    (set-buffer-modified-p nil))
			  (set-marker start nil)
			  (set-marker end nil)))))))))
	;; Remove.
	(while (setq start (if (w3m-image end)
			       end
			     (next-single-property-change end 'w3m-image)))
	  (setq end (or (next-single-property-change start 'w3m-image)
			(point-max))
		iurl (w3m-image start))
	  ;; IMAGE-ALT-STRING DUMMY-STRING
	  ;; <--------w3m-image---------->
	  ;; <---redundant--><---dummy--->
	  ;; <---invisible-->
	  (when (and (or (not url)
			 ;; URL is specified and is not same as the image URL.
			 (string= url iurl))
		     (not (eq (get-text-property start 'w3m-image-status)
			      'off)))
	    (cond
	     ((get-text-property start 'w3m-image-redundant)
	      ;; Remove invisible property.
	      (put-text-property start end 'invisible nil))
	     ((get-text-property start 'w3m-image-dummy)
	      ;; Remove dummy string.
	      (delete-region start end)
	      (setq end start))
	     ((get-text-property start 'w3m-bitmap-image)
	      (goto-char end)
	      (setq end (w3m-remove-image start end)))
	     (t (w3m-remove-image start end)))
	    (w3m-add-text-properties start end '(w3m-image-status off))))
	(set-buffer-modified-p nil)))))

(defun w3m-toggle-inline-image (&optional force no-cache)
  "Toggle displaying of inline image on cursor point.
If FORCE is non-nil, image displaying is forced.
If NO-CACHE is non-nil, cache is not used."
  (interactive "P")
  (unless (w3m-display-graphic-p)
    (error "Can't display images in this environment"))
  (let ((url (w3m-image))
	(status (get-text-property (point) 'w3m-image-status))
	(scale (get-text-property (point) 'w3m-image-scale)))
    (if (and scale (equal status 'off))
	(w3m-zoom-in-image 0)
      (if (w3m-url-valid url)
	  (progn
	    (if force (setq status 'off))
	    (w3m-toggle-inline-images-internal status no-cache url))
	(w3m-display-message "No image at point")))))

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
      (force-mode-line-update))))

(defsubst w3m-resize-inline-image-internal (url rate)
  "Resize an inline image on cursor point.
URL is the image file's url.
RATE is resize percentage."
  (interactive "P")
  (let ((buffer-read-only)
	start end iurl image size iscale scale)
    (if (or (featurep 'xemacs)
	    (and (boundp 'emacs-major-version)
		 (>= emacs-major-version 21)))
	(progn
	  (setq start (point)
		end (or (next-single-property-change start 'w3m-image)
			(point-max))
		iurl (w3m-image start)))
      ;; The case of using BITMAP-MULE:
      ;; Look for the start and the end positions of the first line of
      ;; a bitmap image.
      (setq end (point-min)
	    iurl "")
      (while (and (not (string-equal url iurl))
		  (setq start
			(if (w3m-image end)
			    end
			  (next-single-property-change end 'w3m-image))))
	(setq end (or (next-single-property-change start 'w3m-image)
		      (point-max))
	      iurl (w3m-image start)))
      (goto-char start)
      ;; Remove an existing bitmap image.
      (let ((inhibit-read-only t))
	(setq end (or (w3m-remove-image start end) end))))
    (setq size (get-text-property start 'w3m-image-size)
	  iscale (or (get-text-property start 'w3m-image-scale) '100))
    (w3m-add-text-properties start end '(w3m-image-status on))
    (setq scale (truncate (* iscale rate 0.01)))
    (w3m-add-text-properties start end (list 'w3m-image-scale scale))
    (if (get-text-property start 'w3m-image-redundant)
	(progn
	  ;; Insert dummy string instead of redundant image.
	  (setq image (make-string
		       (string-width (buffer-substring start end))
		       ? ))
	  (w3m-add-text-properties start end '(invisible t))
	  (w3m-add-text-properties (point)
				   (progn (insert image) (point))
				   '(w3m-image-dummy t
						     w3m-image "dummy")))
      (when iurl
	(w3m-process-with-null-handler
	  (lexical-let ((start (set-marker (make-marker) start))
			(end (set-marker (make-marker) end))
			(iurl iurl)
			(rate scale)
			(url w3m-current-url))
	    (w3m-process-do
		(image (let ((w3m-current-buffer (current-buffer)))
			 (w3m-create-resized-image
			  iurl
			  rate
			  w3m-current-url
			  size handler)))
	      (when (buffer-live-p (marker-buffer start))
		(with-current-buffer (marker-buffer start)
		  (if image
		      (when (equal url w3m-current-url)
			(let (buffer-read-only)
			  (w3m-static-when (featurep 'xemacs)
			    (w3m-remove-image start end))
			  (w3m-insert-image start end image iurl))
			;; Redisplay
			(when w3m-force-redisplay
			  (sit-for 0)))
		    (let (buffer-read-only)
		      (w3m-add-text-properties
		       start end '(w3m-image-status off))))
		  (set-buffer-modified-p nil))
		(set-marker start nil)
		(set-marker end nil)))))))))

(defun w3m-zoom-in-image (&optional rate)
  "Zoom in image on cursor point."
  (interactive "P")
  (unless (w3m-display-graphic-p)
    (error "Can't display images in this environment"))
  (let ((url (w3m-image)))
    (unless rate
      (setq rate w3m-resize-image-scale))
    (if url
	(w3m-resize-inline-image-internal url (+ 100 rate))
      (w3m-display-message "No image at point"))))

(defun w3m-zoom-out-image (&optional rate)
  "Zoom out image on cursor point."
  (interactive "P")
  (unless (w3m-display-graphic-p)
    (error "Can't display images in this environment"))
  (let ((url (w3m-image)))
    (unless rate
      (setq rate w3m-resize-image-scale))
    (if url
	(w3m-resize-inline-image-internal url (- 100 rate))
      (w3m-display-message "No image at point"))))

(defun w3m-decode-entities (&optional reserve-prop)
  "Decode entities in the current buffer.
If optional RESERVE-PROP is non-nil, text property is reserved."
  (save-excursion
    (goto-char (point-min))
    (let (prop)
      (while (re-search-forward w3m-entity-regexp nil t)
	(if reserve-prop
	    (setq prop (text-properties-at (match-beginning 0))))
	(replace-match (or (w3m-entity-value (match-string 1)
					     (match-beginning 2))
			   (match-string 0))
		       nil t)
	(if (and reserve-prop prop)
	    (w3m-add-text-properties (match-beginning 0) (point) prop))))))

(defun w3m-decode-entities-string (str)
  "Decode entities in the string STR."
  (with-temp-buffer
    (insert str)
    (save-match-data (w3m-decode-entities))
    (buffer-string)))

(defun w3m-encode-specials-string (str)
  "Encode special characters in the string STR."
  (let ((pos 0)
	(buf))
    (while (string-match "[<>&]" str pos)
      (setq buf
	    (cons ";"
		  (cons (car (rassoc (match-string 0 str) w3m-entity-alist))
			(cons "&"
			      (cons (substring str pos (match-beginning 0))
				    buf))))
	    pos (match-end 0)))
    (if buf
	(apply 'concat (nreverse (cons (substring str pos) buf)))
      str)))

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
    (when w3m-delete-duplicated-empty-lines
      (while (re-search-forward "^[ \t]*\n\\([ \t]*\n\\)+" nil t)
	(delete-region (match-beginning 0) (1- (match-end 0)))))
    (w3m-message "Fontifying...done")
    (w3m-header-line-insert)
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

(eval-and-compile
  (cond
   ((locate-library "ffap")
    (autoload 'ffap-url-at-point "ffap")
    (if (featurep 'xemacs)
	(defun w3m-url-at-point ()
	  "Like `ffap-url-at-point', except that text props will be stripped."
	  (let (ffap-xemacs)
	    (ffap-url-at-point)))
      (defalias 'w3m-url-at-point 'ffap-url-at-point))
    (eval-after-load "ffap"
      '(progn
	 ;; Under Emacs 19, 20 or XEmacs, `ffap-url-regexp' won't match
	 ;; to https urls by default.
	 (if (and ffap-url-regexp
		  (not (string-match ffap-url-regexp "https://foo"))
		  (string-match "\\((\\|\\\\|\\)\\(http\\)\\(\\\\|\\|\\\\)\\)"
				ffap-url-regexp))
	     (setq ffap-url-regexp (replace-match "\\1\\2s?\\3"
						  nil nil ffap-url-regexp)))
	 ;; Add nntp:.
	 (if (and ffap-url-regexp
		  (not (string-match ffap-url-regexp "nntp://bar"))
		  (string-match "\\(\\\\(news\\\\(post\\\\)\\?:\\)\\(\\\\|\\)"
				ffap-url-regexp))
	     (setq ffap-url-regexp (replace-match "\\1\\\\|nntp:\\2"
						  nil nil ffap-url-regexp))))))
   ((locate-library "thingatpt")
    (autoload 'thing-at-point "thingatpt")
    (defun w3m-url-at-point ()
      "Return url from around point if it exists, or nil."
      (let ((url (thing-at-point 'url)))
	(when url
	  (set-text-properties 0 (length url) nil url)
	  url))))
   (t
    (defalias 'w3m-url-at-point 'ignore))))

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

(defun w3m-read-file-name (&optional prompt dir default existing)
  (when default
    (setq default (file-name-nondirectory (w3m-url-strip-query default))))
  (unless prompt
    (setq prompt (if default
		     (format "Save to (%s): " default)
		   "Save to: ")))
  (setq dir (file-name-as-directory (or dir w3m-default-save-directory)))
  (let ((default-directory dir)
	(file (read-file-name prompt dir nil existing default)))
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
		(assoc (downcase encoding) w3m-encoding-alist))))
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
    (setq w3m-current-content-charset content-charset)
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
(defun w3m-local-file-type (url)
  "Return the content type and the content encoding type."
  (setq url (or (w3m-url-to-file-name url)
		(file-name-nondirectory url)))
  (if (and (file-name-absolute-p url)
	   (file-directory-p url))
      (cons "text/html" nil)
    (let ((encoding
	   (catch 'encoding-detected
	     (dolist (elem w3m-encoding-type-alist)
	       (when (string-match (car elem) url)
		 (setq url (substring url 0 (match-beginning 0)))
		 (throw 'encoding-detected (cdr elem)))))))
      (cons (catch 'type-detected
	      (dolist (elem w3m-content-type-alist)
		(if (string-match (nth 1 elem) url)
		    (throw 'type-detected (car elem))))
	      "unknown")
	    encoding))))

(defmacro w3m-local-content-type (url)
  `(car (w3m-local-file-type ,url)))

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
  (let* ((file (w3m-url-to-file-name url))
	 (attr (when (file-exists-p file)
		 (file-attributes file)))
	 (type (w3m-local-file-type url)))
    (list (or (w3m-arrived-content-type url) (car type))
	  nil
	  (nth 7 attr)
	  (cdr type)
	  (nth 5 attr)
	  (w3m-expand-file-name-as-url (file-truename file))
	  ;; FIXME: ファイルに含まれている <base> タグの指定を解釈する
	  ;; 必要がある。
	  (w3m-expand-file-name-as-url (file-truename file)))))

(defun w3m-local-retrieve (url &optional no-decode &rest args)
  "Retrieve content of local URL and insert it to this buffer.
This function will return content-type of URL as string when retrieval
succeed."
  (let ((file (w3m-url-to-file-name url)))
    (when (file-readable-p file)
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
      (or (w3m-arrived-content-type url)
	  (w3m-local-content-type file)))))

(defun w3m-local-dirlist-cgi (url)
  (w3m-message "Reading %s..." url)
  (if w3m-dirlist-cgi-program
      (if (file-executable-p w3m-dirlist-cgi-program)
	  (let ((coding-system-for-read 'binary)
		(default-process-coding-system
		  (cons 'binary 'binary))
		(lcookie (make-temp-name
			  (format "%s.%d." (user-login-name) (emacs-pid))))
		(cfile (make-temp-name
			(expand-file-name "w3melck" w3m-profile-directory)))
		file beg end)
	    (with-temp-buffer
	      (insert lcookie)
	      (write-region (point-min) (point-max) cfile 'nomsg))
	    (w3m-process-with-environment
		(list
		 (cons "LOCAL_COOKIE" lcookie)
		 (cons "LOCAL_COOKIE_FILE" cfile)
		 (cons "QUERY_STRING"
		       (format "dir=%s&cookie=%s"
			       (encode-coding-string (w3m-url-to-file-name url)
						     w3m-file-name-coding-system)
			       lcookie)))
	      (call-process w3m-dirlist-cgi-program nil t nil))
	    ;; delete local cookie file
	    (when (and (file-exists-p cfile) (file-writable-p cfile))
	      (delete-file cfile))
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
		(insert (encode-coding-string
			 (w3m-url-decode-string file w3m-file-name-coding-system)
			 w3m-file-name-coding-system)))))
	(error "Can't execute: %s" w3m-dirlist-cgi-program))
    ;; execute w3m internal CGI
    (w3m-process-with-wait-handler
      (setq w3m-current-url url)
      (w3m-process-start handler
			 w3m-command
			 (append w3m-command-arguments
				 (list "-dump_source" url)))))
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
    (when (string-match "\\`[ \t\r\f\n]+" str)
      (setq str (substring str (match-end 0))))
    (if (string-match "[ \t\r\f\n]+\\'" str)
	(substring str 0 (match-beginning 0))
      str)))

(defun w3m-w3m-get-header (url no-cache handler)
  "Return the header string of the URL.
If optional argument NO-CACHE is non-nil, cache is not used."
  (or (unless no-cache
	(w3m-cache-request-header (w3m-url-strip-authinfo url)))
      (lexical-let ((url url))
	(w3m-message "Request sent, waiting for response...")
	(w3m-process-do-with-temp-buffer
	    (success (progn
		       (setq w3m-current-url url
			     url (w3m-url-strip-authinfo url))
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
      (when header
	(let ((case-fold-search t)
	      alist type charset moved)
	  (setq w3m-current-redirect nil)
	  (dolist (line (split-string header "[\t ]*\n"))
	    (when (string-match "^\\([^ \t:]+\\):[ \t]*" line)
	      (push (cons (downcase (match-string 1 line))
			  (substring line (match-end 0)))
		    alist)))
	  (cond
	   ((string-match "\\`ftp://" url)
	    (setq url (or (cdr (assoc "w3m-current-url" alist)) url))
	    (if (string-match "/\\'" url)
		(list "text/html" "w3m-euc-japan" nil nil nil url url)
	      (list (w3m-local-content-type url) nil nil nil nil url url)))
	   ((or (string-match "HTTP/1\\.[0-9] 200[ \n]" header)
		(setq moved (and (string-match "HTTP/1\\.[0-9] \\(30[1237]\\)[ \n]"
					       header)
				 (match-string 1 header))))
	    (when (setq type (cdr (assoc "content-type" alist)))
	      (if (string-match ";[ \t]*charset=\"?\\([^\"]+\\)\"?" type)
		  (setq charset (w3m-remove-redundant-spaces
				 (match-string 1 type))
			type (w3m-remove-redundant-spaces
			      (substring type 0 (match-beginning 0))))
		(setq type (w3m-remove-redundant-spaces type))
		(when (string-match ";" type)
		  (setq type (substring type 0 (match-beginning 0)))))
	      (setq type (downcase type)))
	    (when (and moved (assoc "location" alist))
	      (setq w3m-current-redirect
		    (cons (string-to-number moved)
			  (w3m-expand-url (cdr (assoc "location" alist))))))
	    (setq w3m-current-ssl (cdr (assoc "w3m-ssl-certificate" alist)))
	    (list (or type (w3m-local-content-type url))
		  (or charset
		      (and (eq w3m-type 'w3mmee)
			   (setq charset
				 (cdr (assoc "w3m-document-charset" alist)))
			   (car (split-string charset))))
		  (let ((v (cdr (assoc "content-length" alist))))
		    (and v (setq v (string-to-number v)) (> v 0) v))
		  (cdr (or (assoc "content-encoding" alist)
			   (when (eq w3m-type 'w3mmee)
			     (assoc "x-w3m-content-encoding" alist))))
		  (let ((v (cdr (assoc "last-modified" alist))))
		    (and v (w3m-time-parse-string v)))
		  (or (cdr (assoc "w3m-current-url" alist))
		      url)
		  (or (cdr (assoc "w3m-base-url" alist))
		      (cdr (assoc "w3m-current-url" alist))
		      url)))
	   ;; FIXME: adhoc implementation
	   ;; HTTP/1.1 500 Server Error on Netscape-Enterprise/3.6
	   ;; HTTP/1.0 501 Method Not Implemented
	   ((string-match "HTTP/1\\.[0-9] 50[0-9]" header)
	    (list "text/html" nil nil nil nil url url))))))))

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

(defun w3m-w3m-dump-head-source (url orig-url handler)
  "Retrive headers and content pointed by URL, and call the HANDLER
function with attributes of the retrieved content when retrieval is
complete."
  (lexical-let ((url url)
		(orig-url orig-url))
    (setq w3m-current-url url
	  url (w3m-url-strip-authinfo url))
    (w3m-message "Reading %s...%s"
		 url
		 (if (and w3m-async-exec (not w3m-process-waited))
		     (substitute-command-keys "\
 (Type `\\<w3m-mode-map>\\[w3m-process-stop]' to stop asynchronous process)")
		   ""))
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
	    (w3m-cache-header (or orig-url url)
			      (buffer-substring (point-min) (point)) t)
	    (when w3m-use-cookies
	      (w3m-cookie-set url (point-min) (point)))
	    (delete-region (point-min) (point))
	    (prog1 (w3m-w3m-attributes (or orig-url url) nil handler)
	      (unless (and w3m-current-redirect
			   (or (eq (car w3m-current-redirect) 302)
			       (eq (car w3m-current-redirect) 303)
			       (eq (car w3m-current-redirect) 307)))
		(w3m-cache-contents (or orig-url url) (current-buffer))))))))))

(defun w3m-additional-command-arguments (url)
  "Return a list of additional arguments passed to the w3m command.
You may specify additional arguments for the particular urls using the
option `w3m-command-arguments-alist', or using `w3m-no-proxy-domains'
to add the option \"-no-proxy\"."
  (let ((defs w3m-command-arguments-alist)
	def args host)
    (while (and defs
		(null args))
      (setq def (car defs)
	    defs (cdr defs))
      (when (string-match (car def) url)
	(setq args (cdr def))))
    (when (and w3m-no-proxy-domains
	       (not (member "-no-proxy" args))
	       (string-match "^[a-z]+://\\([^/:]+\\)" url)
	       (catch 'domain-match
		 (setq host (match-string 1 url))
		 (dolist (domain w3m-no-proxy-domains)
		   (when (string-match (concat "\\(^\\|\\.\\)"
					       (regexp-quote domain)
					       "$")
				       host)
		     (throw 'domain-match t)))))
      (push "-no-proxy" args))
    args))

;; Currently, -request argument is supported only by w3mmee.
(defun w3m-request-arguments (method url temp-file
				     &optional body referer content-type)
  "Make `-request' or `-header' arguments.
METHOD is an HTTP method name.
TEMP-FILE is a name of temporal file to write request content to.
Optional BODY is the body content string.
Second optional REFERER is the Referer: field content.
Third optional CONTENT-TYPE is the Content-Type: field content."
  (with-temp-buffer
    (let ((modes (default-file-modes))
	  (cookie (and w3m-use-cookies (w3m-cookie-get url))))
      (if (and (null cookie)(null body)
	       (null content-type))
	  (append
	   (when w3m-add-user-agent
	     (list "-header" (concat "User-Agent:" w3m-user-agent)))
	   (when (and (stringp referer)
		      (not (and (cdr w3m-add-referer-regexps)
				(string-match (cdr w3m-add-referer-regexps)
					      referer)))
		      (car w3m-add-referer-regexps)
		      (string-match (car w3m-add-referer-regexps) referer))
	     (list "-header" (concat "Referer: " referer)))
	   (when w3m-accept-languages
	     (list "-header" (concat
			      "Accept-Language: "
			      (mapconcat 'identity w3m-accept-languages
					 " ")))))
	(when w3m-add-user-agent (insert "User-Agent: " w3m-user-agent "\n"))
	(when (and (stringp referer)
		   (not (and (cdr w3m-add-referer-regexps)
			     (string-match (cdr w3m-add-referer-regexps)
					   referer)))
		   (car w3m-add-referer-regexps)
		   (string-match (car w3m-add-referer-regexps) referer))
	  (insert "Referer: " referer "\n"))
	(when w3m-accept-languages
	  (insert "Accept-Language: "
		  (mapconcat 'identity w3m-accept-languages " ") "\n"))
	(when cookie
	  (insert "Cookie: " cookie "\n"))
	(when content-type
	  (insert "Content-Type: " content-type "\n"))
	(insert "\n")
	(when body
	  (insert body))
	(unwind-protect
	    (let ((coding-system-for-write 'binary))
	      (set-default-file-modes (* 64 6))
	      (write-region (point-min) (point-max) temp-file nil 'silent))
	  (set-default-file-modes modes))
	(list "-request" (concat method ":" temp-file))))))

;; Currently, w3m uses this function.
(defun w3m-header-arguments (method url temp-file
				    &optional body referer content-type)
  "Make `-header' arguments.
METHOD is an HTTP method name.
TEMP-FILE is a name of temporal file to write post body to.
Optional BODY is the post body content string.
Second optional REFERER is the Referer: field content.
Third optional CONTENT-TYPE is the Content-Type: field content."
  (let ((modes (default-file-modes))
	(cookie (and w3m-use-cookies (w3m-cookie-get url)))
	args)
    (when w3m-add-user-agent
      (setq args (nconc args
			(list "-o" (concat "user_agent=" w3m-user-agent)))))
    (when cookie
      (setq args (nconc args
			(list "-header" (concat "Cookie: " cookie)))))
    (when (and (string= method "post") temp-file)
      (with-temp-buffer
	(when body (insert body))
	(unwind-protect
	    (let ((coding-system-for-write 'binary))
	      (set-default-file-modes (* 64 6))
	      (write-region (point-min) (point-max) temp-file nil 'silent))
	  (set-default-file-modes modes)))
      (setq args (nconc args
			(when content-type
			  (list "-header" (concat "Content-Type: "
						  content-type)))
			(list "-post" temp-file))))
    (when (and (stringp referer)
	       (not (and (cdr w3m-add-referer-regexps)
			 (string-match (cdr w3m-add-referer-regexps)
				       referer)))
	       (car w3m-add-referer-regexps)
	       (string-match (car w3m-add-referer-regexps) referer))
      (setq args (nconc args (list "-header" (concat "Referer: " referer)))))
    args))

(defun w3m-w3m-retrieve (url no-decode no-cache post-data referer handler)
  "Retrieve content pointed by URL with w3m, insert it to this buffer,
and call the HANDLER function with its content type as a string
argument, when retrieve is complete."
  (lexical-let ((i w3m-follow-redirection)
		(orig-url url)
		(url url)
		(no-decode no-decode)
		(no-cache no-cache)
		(post-data post-data)
		(referer referer)
		(orig-handler handler)
		redirect-handler
		sync return quit)
    (setq redirect-handler
	  (lambda (type)
	    (if (or (null w3m-follow-redirection)
		    (null w3m-current-redirect))
		;; No redirection exists.
		(funcall orig-handler type)
	      ;; Follow the redirection.
	      (if (zerop i)
		  ;; Redirection number exceeds `w3m-follow-redirection'.
		  (funcall orig-handler nil)
		(setq i (1- i))
		(setq url (cdr w3m-current-redirect))
		(erase-buffer)
		(when post-data
		  (cond
		   ((eq (car w3m-current-redirect) 303)
		    ;; Use GET.
		    (setq post-data nil))
		   ((eq (car w3m-current-redirect) 307)
		    ;; Use POST.
		    (setq quit (not
				(y-or-n-p
				 (format "Send POST data to '%s'?" url)))))
		   ((or (eq (car w3m-current-redirect) 302)
			(eq (car w3m-current-redirect) 301))
		    (if w3m-redirect-with-get
			(setq post-data nil)
		      (setq quit
			    (not
			     (y-or-n-p
			      (format "Send POST data to '%s'?" url))))))))
		(if quit
		    (funcall orig-handler nil)
		  (if sync
		      (condition-case nil
			  (w3m-process-with-wait-handler
			    (w3m-w3m-retrieve-1 url orig-url no-decode
						'no-cache
						post-data nil
						redirect-handler))
			(w3m-process-timeout nil))
		    (w3m-w3m-retrieve-1 url orig-url no-decode 'no-cache
					post-data
					nil redirect-handler)))))))
    ;; The first retrieval.
    (prog1 (setq return (w3m-w3m-retrieve-1
			 url nil no-decode no-cache post-data referer
			 redirect-handler))
      (setq sync (w3m-process-p return)))))

(defun w3m-w3m-retrieve-1 (url orig-url no-decode no-cache post-data referer handler)
  "Internal function for w3m-w3m-retrieve.
If this function is called by redirection, ORIG-URL must be set."
  (let ((w3m-command-arguments
	 (append w3m-command-arguments
		 (when (member "cookie" w3m-compile-options)
		   (list "-no-cookie"))
		 ;; Don't follow redirection within w3m command.
		 (when w3m-follow-redirection
		   (list "-o" "follow_redirection=0"))
		 (w3m-additional-command-arguments url)))
	(temp-file))
    (and no-cache
	 w3m-broken-proxy-cache
	 (setq w3m-command-arguments
	       (append w3m-command-arguments '("-o" "no_cache=1"))))
    (setq temp-file
	  (when (or (eq w3m-type 'w3mmee)
		    post-data)
	    (make-temp-name
	     (expand-file-name "w3mel" w3m-profile-directory))))
    (setq w3m-command-arguments
	  (append w3m-command-arguments
		  (apply (if (eq w3m-type 'w3mmee)
			     'w3m-request-arguments
			   'w3m-header-arguments)
			 (list
			  (if post-data "post" "get")
			  url
			  temp-file
			  (if (consp post-data)
			      (cdr post-data)
			    post-data)
			  referer
			  (if (consp post-data) (car post-data))))))
    (lexical-let ((url url)
		  (orig-url orig-url)
		  (no-decode no-decode)
		  (temp-file temp-file))
      (w3m-process-do
	  (attributes
	   (or (unless no-cache
		 (and (w3m-cache-request-contents url)
		      (w3m-w3m-attributes url nil handler)))
	       (w3m-w3m-dump-head-source url orig-url handler)))
	(when (and temp-file (file-exists-p temp-file))
	  (delete-file temp-file))
	(when attributes
	  (if (or no-decode
		  w3m-current-redirect
		  (w3m-decode-encoded-contents (nth 3 attributes)))
	      (car attributes)
	    (ding)
	    (w3m-message "Can't decode encoded contents: %s" url)
	    nil))))))

(defsubst w3m-about-retrieve (url &optional no-decode no-cache
				  post-data referer handler)
  "Retrieve content pointed by URL which has about: scheme, insert it
to this buffer."
  (cond
   ((string= "about://emacs-w3m.gif" url)
    (when (fboundp 'base64-decode-string)
      (let* ((b64d 'base64-decode-string)
	     ;; Avoid compile warn under old Emacsen.
	     (icon (funcall b64d w3m-emacs-w3m-icon)))
	(if (featurep 'xemacs)
	    (insert icon)
	  (set-buffer-multibyte (multibyte-string-p icon))
	  (insert icon)
	  (set-buffer-multibyte nil)))
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
				 (intern-soft (concat "w3m-about-"
						      (match-string 1 url))))
			   (fboundp func))
		      (funcall func url no-decode no-cache
			       post-data referer handler)
		    (w3m-about url no-decode no-cache))))
	(when type
	  (when (string-match "\\`text/" type)
	    (encode-coding-region (point-min) (point-max) w3m-coding-system))
	  (set-buffer-multibyte nil)
	  (when (buffer-name output-buffer)
	    (let ((temp-buffer (current-buffer)))
	      (with-current-buffer output-buffer
		(insert-buffer-substring temp-buffer))))
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
      (setq url (w3m-url-strip-fragment url))
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
   (let* ((url (w3m-input-url "Download URL (default HOME): "
			      (when (stringp w3m-current-url)
				(if (string-match
				     "\\`about://\\(header\\|source\\)/"
				     w3m-current-url)
				    (substring w3m-current-url (match-end 0))
				  w3m-current-url))
			      w3m-home-page))
	  (basename (file-name-nondirectory (w3m-url-strip-query url))))
     (if (string-match "^[\t ]*$" basename)
	 (list url
	       (w3m-read-file-name (format "Download %s to: " url)
				   w3m-default-save-directory "index.html")
	       current-prefix-arg)
       (list url
	     (w3m-read-file-name (format "Download %s to: " basename)
				 w3m-default-save-directory basename)
	     current-prefix-arg))))
  (if (and w3m-use-ange-ftp (string-match "\\`ftp://" url))
      (w3m-goto-ftp-url url filename)
    (lexical-let ((url url)
		  (filename (or filename (w3m-read-file-name nil nil url))))
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
		(w3m-touch-file filename (w3m-last-modified url))
		t))
	  (ding)
	  (message "Cannot retrieve URL: %s%s"
		   url
		   (if w3m-process-exit-status
		       (format " (exit status: %s)" w3m-process-exit-status)
		     ""))
	  nil)))))

(defvar w3m-touch-file-available-p 'undecided)

(defun w3m-touch-file (file time)
  "Change the access and/or modification TIME of the specified FILE."
  ;; Check the validity of `touch' command.
  (when (eq w3m-touch-file-available-p 'undecided)
    (let ((file (make-temp-name
		 (expand-file-name "w3mel" w3m-profile-directory)))
	  time timefile)
      (while (progn
	       (setq time (list (abs (% (random) 8192))
				(abs (% (random) 65536)))
		     timefile (expand-file-name
			       (format-time-string "%Y%m%d%H%M.%S" time)
			       w3m-profile-directory))
	       (file-exists-p timefile)))
      (unwind-protect
	  (setq w3m-touch-file-available-p
		(when (w3m-which-command w3m-touch-command)
		  (with-temp-buffer
		    (insert "touch check")
		    (write-region (point-min) (point-max) file nil 'nomsg))
		  (and (let ((default-directory w3m-profile-directory)
			     (w3m-touch-file-available-p t))
			 (w3m-touch-file file time))
		       (zerop (w3m-time-lapse-seconds
			       time (nth 5 (file-attributes file)))))))
	(when (file-exists-p file)
	  (ignore-errors (delete-file file)))
	(when (file-exists-p timefile)
	  (ignore-errors (delete-file timefile))))))
  (and w3m-touch-file-available-p
       time
       (w3m-which-command w3m-touch-command)
       (file-exists-p file)
       (zerop (let ((default-directory (file-name-directory file)))
		(call-process w3m-touch-command nil nil nil
			      "-t" (format-time-string "%Y%m%d%H%M.%S" time)
			      file)))))

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

(defun w3m-check-header-tags ()
  "Process header tags (<LINK>,<BASE>) in the current buffer."
  (let ((case-fold-search t)
	tag)
    (goto-char (point-min))
    (when (re-search-forward "</head\\([ \t\r\f\n][^>]*\\)?>" nil t)
      (save-restriction
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(while (re-search-forward "<\\(link\\|base\\)[ \t\r\f\n]+" nil t)
	  (setq tag (downcase (match-string 1)))
	  (cond
	   ((string= tag "link")
	    (w3m-parse-attributes ((rel :case-ignore) href type)
	      (when rel
		(setq rel (split-string rel))
		(cond
		 ((member "icon" rel) (setq w3m-icon-data (cons href type)))
		 ((member "next" rel) (setq w3m-next-url href))
		 ((or (member "prev" rel) (member "previous" rel))
		  (setq w3m-previous-url href))
		 ((member "start" rel) (setq w3m-start-url href))
		 ((member "contents" rel) (setq w3m-contents-url href))))))
	   ((string= tag "base")
	    (w3m-parse-attributes (href)
	      (setq w3m-current-base-url href)))))))))

(defun w3m-check-refresh-attribute ()
  "Get REFRESH attribute in META tags."
  (setq w3m-current-refresh nil)
  (when w3m-use-refresh
    (goto-char (point-min))
    (let ((case-fold-search t)
	  sec refurl)
      (goto-char (point-min))
      (when (or (re-search-forward w3m-meta-refresh-content-regexp nil t)
		(re-search-forward w3m-meta-content-refresh-regexp nil t))
	(setq sec (match-string-no-properties 1))
	(setq refurl (match-string-no-properties 2))
	(unless (string-match "[^0-9]" sec)
	  (setq w3m-current-refresh (cons (string-to-number sec)
					  (w3m-expand-url refurl))))))))

(defun w3m-remove-meta-charset-tags ()
  (let ((case-fold-search t))
    (goto-char (point-min))
    (when (or (re-search-forward
	       w3m-meta-content-type-charset-regexp nil t)
	      (re-search-forward
	       w3m-meta-charset-content-type-regexp nil t))
      (delete-region (match-beginning 0) (match-end 0)))))

(defun w3m-rendering-extract-title ()
  "Extract the title from the half-dumped data in this buffer."
  (goto-char (point-min))
  (or (when (re-search-forward "<title_alt[ \t\n]+title=\"\\([^\"]+\\)\">"
			       nil t)
	(prog1 (w3m-decode-entities-string (match-string 1))
	  (delete-region (match-beginning 0) (match-end 0))))
      (when (and (stringp w3m-current-url)
		 (string-match "/\\([^/]+\\)/?\\'" w3m-current-url))
	(match-string 1 w3m-current-url))
      "<no-title>"))

(defun w3m-rendering-half-dump (&optional charset)
  (let ((coding-system-for-read w3m-output-coding-system)
	(coding-system-for-write w3m-input-coding-system)
	(default-process-coding-system
	  (cons w3m-output-coding-system w3m-input-coding-system)))
    (when (and (memq w3m-type '(w3mmee w3m-m17n)) (not charset))
      (setq charset w3m-current-content-charset))
    (w3m-process-with-environment w3m-command-environment
      (apply 'call-process-region
	     (point-min)
	     (point-max)
	     (or w3m-halfdump-command w3m-command)
	     t t nil
	     (w3m-w3m-expand-arguments
	      (append w3m-halfdump-command-arguments
		      w3m-halfdump-command-common-arguments
		      ;; Image size conscious rendering
		      (if (member "image" w3m-compile-options)
			  (if (and w3m-treat-image-size
				   (w3m-display-inline-images-p))
			      (append
			       (list "-o" "display_image=on")
			       (when (w3m-display-graphic-p)
				 (list "-ppl"
				       (number-to-string
					(or w3m-pixels-per-line
					    (w3m-static-if
						(featurep 'xemacs)
						(font-height
						 (face-font 'default))
					      (frame-char-height))))
				       "-ppc"
				       (number-to-string
					(or w3m-pixels-per-character
					    (w3m-static-if
						(featurep 'xemacs)
						(font-width
						 (face-font 'default))
					      (frame-char-width)))))))
			    (list "-o" "display_image=off")))))))))

(defun w3m-rendering-buffer-1 (&optional content-charset binary-buffer)
  (w3m-message "Rendering...")
  (w3m-remove-comments)
  (w3m-check-header-tags)
  (w3m-check-refresh-attribute)
  (w3m-remove-meta-charset-tags)
  (if binary-buffer
      (let ((cbuf (current-buffer)))
	(delete-region (point-min) (point-max))
	(insert-buffer
	 (with-current-buffer binary-buffer
	   (w3m-copy-local-variables cbuf)
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
	  (insert-buffer-substring original-buffer)
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
	  (insert-buffer-substring original-buffer)
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
  (unless (and w3m-current-ssl
	       (not (string-match "\\`\\(ht\\|f\\)tps://" url))
	       (not (y-or-n-p "You are leaving secure page.  Continue? ")))
    (lexical-let ((url url)
		  (content-type content-type)
		  (content-charset content-charset)
		  (output-buffer (current-buffer)))
      (w3m-process-do-with-temp-buffer
	  (type (progn
		  (w3m-clear-local-variables)
		  (w3m-retrieve url nil no-cache post-data referer handler)))
	(when (buffer-live-p output-buffer)
	  (setq url (w3m-url-strip-authinfo url))
	  (if type
	      (prog1 (w3m-prepare-content url (or content-type type)
					  output-buffer content-charset)
		(and w3m-verbose
		     (not (get-buffer-window output-buffer))
		     (message "The content (%s) has been retrieved in %s"
			      url (buffer-name output-buffer))))
	    (ding)
	    (if (eq (car w3m-current-forms) t)
		(setq w3m-current-forms (cdr w3m-current-forms)))
	    (w3m-message "Cannot retrieve URL: %s%s"
			 url
			 (if w3m-process-exit-status
			     (format " (exit status: %s)"
				     w3m-process-exit-status)
			   ""))
	    nil))))))

(defconst w3m-content-prepare-functions
  '(("\\`text/" . w3m-prepare-text-content)
    ("\\`image/" . w3m-prepare-image-content)
    ("\\`application/xhtml\\+xml" . "text/html")))

(defun w3m-prepare-content (url type output-buffer &optional content-charset retry)
  (catch 'content-prepared
    (dolist (elem w3m-content-prepare-functions)
      (and (string-match (car elem) type)
	   (if (functionp (cdr elem))
	       (funcall (cdr elem) url type output-buffer content-charset)
	     (w3m-prepare-content url (cdr elem) output-buffer
				  content-charset))
	   (throw 'content-prepared t)))
    (if (or retry (nth 2 (assoc type w3m-content-type-alist)))
	(with-current-buffer output-buffer
	  (w3m-external-view url)
	  nil)
      ;; select Content-Type
      (let ((cwin (current-window-configuration)) ct)
	(unwind-protect
	    (progn
	      (pop-to-buffer (current-buffer))
	      (delete-other-windows)
	      (ding)
	      (setq ct (completing-read "Content-type (default Download): "
					w3m-content-type-alist nil t)))
	  (set-window-configuration cwin))
	(if (string= ct "")
	    ;; download
	    (with-current-buffer output-buffer
	      (w3m-external-view url)
	      nil)
	  ;; retry with specified Content-type
	  (w3m-prepare-content url ct output-buffer content-charset 'retry))))))

(defun w3m-prepare-text-content (url type output-buffer
				     &optional content-charset)
  (setq w3m-current-url (w3m-real-url url)
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
	(insert-buffer-substring result-buffer)
	(w3m-copy-local-variables result-buffer)
	(set-buffer-file-coding-system w3m-current-coding-system)
	(when (string= "text/html" type) (w3m-fontify))
	t))))

(defun w3m-prepare-image-content (url type output-buffer
				      &optional content-charset)
  (when (w3m-image-type-available-p (w3m-image-type type))
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
	t))))

(defun w3m-search-name-anchor (name &optional quiet)
  (interactive "sName: ")
  (let ((pos (point-min)))
    (catch 'found
      (while (setq pos (next-single-property-change pos 'w3m-name-anchor))
	(when (member name (get-text-property pos 'w3m-name-anchor))
	  (goto-char pos)
	  (when (eolp) (forward-line))
	  (w3m-horizontal-on-screen)
	  (throw 'found t)))
      (unless quiet
	(message "No such anchor: %s" name))
      nil)))

(defun w3m-parent-page-available-p ()
  (if (null w3m-current-url)
      nil
    (save-match-data
      (string-match "[a-z]+://?[^/]+/." w3m-current-url))))

(defun w3m-view-parent-page ()
  "View the parent of the current page, e.g. \"http://foo/\" if you're
currently viewing \"http://foo/bar/\"."
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
  (if w3m-treat-drive-letter
      ;; Avoid incompatibility of drive letter.
      (defun w3m-expand-path-name (name &optional base)
	"Convert path string NAME to the canonicalized one."
	(let ((x (expand-file-name name base)))
	  (if (string-match "\\`.:" x)
	      (substring x (match-end 0))
	    x)))
    (defalias 'w3m-expand-path-name 'expand-file-name)))

(defconst w3m-url-hierarchical-schemes
  '("http" "https" "ftp" "file")
  "List of schemes which may have hierarchical parts.  This list is
refered in `w3m-expand-url' to keep backward compatibility which is
described in Section 5.2 of RFC 2396.")

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

(defsubst w3m-view-this-url-1 (url reload new-session)
  (lexical-let (pos)
    (when new-session
      (setq pos (point-marker))
      (let ((buffer (w3m-copy-buffer
		     nil nil nil
		     ;; If a new url has the #name portion, we simply copy
		     ;; the buffer's contents to the new settion, otherwise
		     ;; creating an empty buffer.
		     (not
		      (and
		       (string-match w3m-url-components-regexp url)
		       (match-beginning 8)
		       (string-equal w3m-current-url
				     (substring url
						0 (match-beginning 8))))))))
	(if w3m-view-this-url-new-session-in-background
	    (set-buffer buffer)
	  (switch-to-buffer buffer))))
    (let (handler)
      (w3m-process-do
	  (success
	   (if (let ((case-fold-search t))
		 (string-match "\\`mailto:" url))
	       ;; Don't save a window configuration to popup a mail buffer.
	       (w3m-goto-url url reload nil nil w3m-current-url handler)
	     (save-window-excursion
	       (w3m-goto-url url reload nil nil w3m-current-url handler))))
	;; FIXME: 本当は w3m-goto-url() が適当な返り値を返すように
	;; 変更して、その値を検査するべきだ
	(when (and pos (buffer-name (marker-buffer pos)))
	  (save-excursion
	    (set-buffer (marker-buffer pos))
	    (save-excursion
	      (goto-char pos)
	      (w3m-refontify-anchor))))))))

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
  (let (act url)
    (cond
     ((setq act (w3m-action))
      (eval act))
     ((setq url (w3m-url-valid (w3m-anchor)))
      (w3m-view-this-url-1 url arg new-session))
     ((w3m-url-valid (w3m-image))
      (if (w3m-display-graphic-p)
	  (w3m-toggle-inline-image)
	(w3m-view-image)))
     ((setq url (w3m-url-at-point))
      (unless (eq 'quit (setq url (w3m-input-url nil url 'quit)))
	(w3m-view-this-url-1 url arg new-session)))
     (t (w3m-display-message "No URL at point")))))

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
    (if (and submit
	     w3m-current-url
	     (w3m-url-valid w3m-current-url))
	(eval submit)
      (w3m-display-message "Can't Submit at this point"))))

(defun w3m-external-view (url &optional no-cache handler)
  (when (w3m-url-valid url)
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
		  (w3m-download url nil no-cache handler))))))))))))

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
  (let ((url (w3m-url-valid (w3m-image))))
    (if url
	(w3m-external-view url)
      (w3m-display-message "No image at point"))))

(defun w3m-save-image ()
  "Save the image under point to a file."
  (interactive)
  (let ((url (w3m-url-valid (w3m-image))))
    (if url
	(w3m-download url)
      (w3m-display-message "No image at point"))))

(defun w3m-view-url-with-external-browser ()
  "View this URL."
  (interactive)
  (let ((url (or (w3m-anchor)
		 (unless w3m-display-inline-images
		   (w3m-image))
		 (when (y-or-n-p (format "Browse <%s> ? " w3m-current-url))
		   w3m-current-url))))
    (if (w3m-url-valid url)
	(progn
	  (message "Browsing <%s>..." url)
	  (w3m-external-view url))
      (w3m-display-message "No URL at point"))))

(defun w3m-download-this-url ()
  "Download the file or the image which pointed by the link under cursor."
  (interactive)
  (let ((url (or (w3m-anchor) (w3m-image))))
    (if (w3m-url-valid url)
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
      (w3m-display-message "No URL at point"))))

(defun w3m-print-current-url ()
  "Print the URL of the current page and push it into the kill-ring."
  (interactive)
  (kill-new w3m-current-url)
  (w3m-display-message "%s" w3m-current-url))

(defun w3m-print-this-url (&optional interactive-p)
  "Print the URL of the link under point and push it into the kill-ring."
  (interactive (list t))
  (let ((url (if interactive-p
		 (or (w3m-anchor) (w3m-image))
	       (or (w3m-anchor (point)) (w3m-image (point))))))
    (when (or url interactive-p)
      (and url interactive-p (kill-new url))
      (w3m-display-message "%s"
			   (or url
			       (and (w3m-action) "There is a form")
			       "There is no url")))))

(defmacro w3m-delete-all-overlays ()
  "Delete all momentary overlays."
  '(dolist (overlay (overlays-in (point-min) (point-max)))
     (if (overlay-get overlay 'w3m-momentary-overlay)
	 (delete-overlay overlay))))

(defun w3m-highlight-current-anchor-1 (seq)
  "Highlight an anchor in the current line if anchor sequence is same as SEQ.
Return t if current line has a same anchor sequence."
  (let ((limit (save-excursion (end-of-line)
			       (point)))
	ov beg pos pseq)
    (save-excursion
      (beginning-of-line)
      (setq pos (point))
      (while (and pos
		  (< pos limit)
		  (not (eq seq (setq pseq (w3m-anchor-sequence pos)))))
	(setq pos (next-single-property-change pos 'w3m-anchor-sequence)))
      (when (and pos (< pos limit) (eq seq pseq))
	(setq beg pos)
	(setq pos (next-single-property-change pos 'w3m-anchor-sequence))
	(setq ov (make-overlay beg pos))
	(overlay-put ov 'face 'w3m-current-anchor-face)
	(overlay-put ov 'w3m-momentary-overlay t)
	t))))

(defun w3m-highlight-current-anchor ()
  "Highlight an anchor under point."
  (when (let ((ovs (overlays-at (point))) ov)
	  ;; If already exists, do nothing.
	  (or (null ovs)
	      (null (progn (while ovs
			     (if (overlay-get (car ovs) 'w3m-momentary-overlay)
				 (setq ov (car ovs)
				       ovs nil))
			     (setq ovs (cdr ovs)))
			   ov))))
    (w3m-delete-all-overlays)
    (save-excursion
      (let ((seq (w3m-anchor-sequence))
	    (pos (point)))
	(when (and seq
		   (w3m-highlight-current-anchor-1 seq)
		   (zerop (forward-line 1)))
	  (while (and (w3m-highlight-current-anchor-1 seq)
		      (zerop (forward-line 1))))
	  (goto-char pos)
	  (while (and (zerop (forward-line -1))
		      (w3m-highlight-current-anchor-1 seq))))))))

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
  (if (w3m-url-valid url)
      (w3m-edit-url url)
    (w3m-display-message "No URL at point")))

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
      (when (setq w3m-goto-anchor-hist (w3m-anchor-sequence))
	(setq w3m-goto-anchor-hist (list w3m-goto-anchor-hist)))
    (when (and (eq last-command 'w3m-previous-anchor) w3m-goto-anchor-hist)
      (setcdr w3m-goto-anchor-hist nil)))
  (if (< arg 0)
      (w3m-previous-anchor (- arg))
    (while (> arg 0)
      (unless (w3m-goto-next-anchor)
	(setq w3m-goto-anchor-hist nil)
	(if (w3m-imitate-widget-button)
	    (widget-forward 1)
	  ;; search from the beginning of the buffer
	  (goto-char (point-min))
	  (w3m-goto-next-anchor)))
      (setq arg (1- arg))
      (if (member (w3m-anchor-sequence) w3m-goto-anchor-hist)
	  (setq arg (1+ arg))
	(push (w3m-anchor-sequence) w3m-goto-anchor-hist)))
    (w3m-horizontal-on-screen)
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
      (when (setq w3m-goto-anchor-hist (w3m-anchor-sequence))
	(setq w3m-goto-anchor-hist (list w3m-goto-anchor-hist)))
    (when (and (eq last-command 'w3m-next-anchor) w3m-goto-anchor-hist)
      (setcdr w3m-goto-anchor-hist nil)))
  (if (< arg 0)
      (w3m-next-anchor (- arg))
    (while (> arg 0)
      (unless (w3m-goto-previous-anchor)
	(setq w3m-goto-anchor-hist nil)
	(if (w3m-imitate-widget-button)
	    (widget-forward -1)
	  ;; search from the end of the buffer
	  (goto-char (point-max))
	  (w3m-goto-previous-anchor)))
      (setq arg (1- arg))
      (if (member (w3m-anchor-sequence) w3m-goto-anchor-hist)
	  (setq arg (1+ arg))
	(push (w3m-anchor-sequence) w3m-goto-anchor-hist)))
    (w3m-horizontal-on-screen)
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
      (when (setq w3m-goto-anchor-hist (w3m-action (point)))
	(setq w3m-goto-anchor-hist (list w3m-goto-anchor-hist)))
    (when (and (eq last-command 'w3m-previous-form) w3m-goto-anchor-hist)
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
      (if (member (w3m-action (point)) w3m-goto-anchor-hist)
	  (setq arg (1+ arg))
	(push (w3m-action (point)) w3m-goto-anchor-hist)))
    (w3m-horizontal-on-screen)
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
	 (if (w3m-action pos)
	     pos
	   (previous-single-property-change pos 'w3m-action))))))

(defun w3m-previous-form (&optional arg)
  "Move cursor to the previous form."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (null (memq last-command '(w3m-next-form w3m-previous-form)))
      (when (setq w3m-goto-anchor-hist (w3m-action (point)))
	(setq w3m-goto-anchor-hist (list w3m-goto-anchor-hist)))
    (when (and (eq last-command 'w3m-next-form) w3m-goto-anchor-hist)
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
      (if (member (w3m-action (point)) w3m-goto-anchor-hist)
	  (setq arg (1+ arg))
	(push (w3m-action (point)) w3m-goto-anchor-hist)))
    (w3m-horizontal-on-screen)
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
      (when (setq w3m-goto-anchor-hist (w3m-image (point)))
	(setq w3m-goto-anchor-hist (list w3m-goto-anchor-hist)))
    (when (and (eq last-command 'w3m-previous-image)
	       w3m-goto-anchor-hist)
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
      (if (member (w3m-image (point)) w3m-goto-anchor-hist)
	  (setq arg (1+ arg))
	(push (w3m-image (point)) w3m-goto-anchor-hist)))
    (w3m-horizontal-on-screen)
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
  (if (null (memq last-command '(w3m-next-image w3m-previous-image)))
      (when (setq w3m-goto-anchor-hist (w3m-image (point)))
	(setq w3m-goto-anchor-hist (list w3m-goto-anchor-hist)))
    (when (and (eq last-command 'w3m-next-image)
	       w3m-goto-anchor-hist)
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
      (if (member (w3m-image (point)) w3m-goto-anchor-hist)
	  (setq arg (1+ arg))
	(push (w3m-image (point)) w3m-goto-anchor-hist)))
    (w3m-horizontal-on-screen)
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
	  (new (generate-new-buffer newname)))
      (with-current-buffer new
	(w3m-mode)
	(if w3m-toggle-inline-images-permanently
	    (setq w3m-display-inline-images images)
	  (setq w3m-display-inline-images w3m-default-display-inline-images))
	(unless empty
	  (w3m-process-with-wait-handler
	    (w3m-goto-url url nil nil nil handler)))
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

(defun w3m-next-buffer (arg)
  "Select the ARG'th different w3m buffer."
  (interactive "p")
  (unless (zerop arg)
    (let ((buffers (if (> arg 0)
		       (w3m-list-buffers)
		     (setq arg (- arg))
		     (nreverse (w3m-list-buffers)))))
      (switch-to-buffer
       (or (nth arg (memq (current-buffer) buffers))
	   (nth (1- arg) buffers))))
    (run-hooks 'w3m-select-buffer-hook)
    (w3m-select-buffer-update)))

(defun w3m-previous-buffer (arg)
  "Select the ARG'th different w3m buffer in the opposite order."
  (interactive "p")
  (w3m-next-buffer (- arg)))

(defun w3m-delete-buffer (&optional force)
  "Delete w3m buffer and switch to previous w3m buffer if exists."
  (interactive "P")
  (if (= 1 (length (w3m-list-buffers t)))
      (w3m-quit force)
    (let ((buffer (current-buffer)))
      (w3m-next-buffer -1)
      (kill-buffer buffer)
      (run-hooks 'w3m-delete-buffer-hook)))
  (w3m-select-buffer-update))

(defun w3m-pack-buffer-numbers ()
  "Pack w3m buffer numbers."
  (interactive)
  (let ((count 1) number)
    (dolist (buffer (w3m-list-buffers))
      (setq number (w3m-buffer-number buffer))
      (when number
	(unless (eq number count)
	  (w3m-buffer-set-number buffer count))
	(incf count)))))

(defun w3m-delete-other-buffers (&optional buffer)
  "Delete w3m buffers except for the current buffer.
The optional argument BUFFER will be used exclusively by the command
`w3m-select-buffer-delete-other-buffers'."
  (interactive)
  (let (window)
    (dolist (buffer (delq (or buffer (current-buffer))
			  (w3m-list-buffers t)))
      ;; Delete all windows and frames related to a buffer.
      (while (setq window (get-buffer-window buffer t))
	(if (eq window (next-window window))
	    ;; If there aren't another windows on a frame, delete it.
	    (delete-frame (window-frame window))
	  ;; Otherwise, delete a window.
	  (delete-window window)))
      ;; Kill a buffer.
      (kill-buffer buffer)))
  (run-hooks 'w3m-delete-buffer-hook)
  (w3m-select-buffer-update))

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
    (define-key map "J" (lambda () (interactive) (scroll-up 1)))
    (define-key map "K" (lambda () (interactive) (scroll-down 1)))
    (define-key map "\M-g" 'goto-line)
    (define-key map "\C-?" 'w3m-scroll-down-or-previous-url)
    (define-key map "\t" 'w3m-next-anchor)
    (define-key map [tab] 'w3m-next-anchor)
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
    (if (featurep 'xemacs)
	(define-key map [?\C-c (control space)] 'w3m-history-store-position)
      ;; `C- ' doesn't mean `C-SPC' in XEmacs.
      (define-key map [?\C-c ?\C-\ ] 'w3m-history-store-position))
    (define-key map "\C-c\C-v" 'w3m-history-restore-position)
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
    (when (w3m-display-graphic-p)
      (define-key map "\M-[" 'w3m-zoom-out-image)
      (define-key map "\M-]" 'w3m-zoom-in-image))
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
    (define-key map "\C-c\M-w" 'w3m-delete-other-buffers)
    (define-key map "\C-c\C-s" 'w3m-select-buffer)
    (define-key map "\C-c\C-a" 'w3m-switch-buffer)
    (define-key map "r" 'w3m-redisplay-this-page)
    (define-key map "R" 'w3m-reload-this-page)
    (define-key map "?" 'describe-mode)
    (define-key map "\M-a" 'w3m-bookmark-add-this-url)
    (define-key map "\M-k" 'w3m-cookie)
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
    (define-key map ">" 'w3m-scroll-left)
    (define-key map "<" 'w3m-scroll-right)
    (define-key map "." 'w3m-shift-left)
    (define-key map "," 'w3m-shift-right)
    (define-key map "\M-l" 'w3m-horizontal-recenter)
    (define-key map "\C-a" 'w3m-beginning-of-line)
    (define-key map "\C-e" 'w3m-end-of-line)
    (define-key map "\\" 'w3m-view-source)
    (define-key map "=" 'w3m-view-header)
    (define-key map "s" 'w3m-history)
    (define-key map "E" 'w3m-edit-current-url)
    (define-key map "e" 'w3m-edit-this-url)
    (define-key map "C" (make-sparse-keymap))
    (define-key map "Ct" 'w3m-redisplay-with-content-type)
    (define-key map "Cc" 'w3m-redisplay-with-charset)
    (define-key map "CC" 'w3m-redisplay-and-reset)
    (define-key map "\C-c\C-b" 'report-emacs-w3m-bug)
    (define-key map "\C-c\C-c" 'w3m-submit-form)
    (define-key map "\C-c\C-k" 'w3m-process-stop)
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
    (define-key map [tab] 'w3m-next-anchor)
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
    (define-key map "\C-c\C-v" 'w3m-history-restore-position)
    (define-key map " " 'w3m-scroll-up-or-next-url)
    (define-key map "a" 'w3m-bookmark-add-current-url)
    (define-key map "\M-a" 'w3m-bookmark-add-this-url)
    (define-key map "+" 'w3m-antenna-add-current-url)
    (define-key map "A" 'w3m-antenna)
    (define-key map "b" 'w3m-scroll-down-or-previous-url)
    (define-key map "c" 'w3m-print-this-url)
    (define-key map "!" 'w3m-redisplay-with-content-type)
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
    (when (w3m-display-graphic-p)
      (define-key map "\M-[" 'w3m-zoom-out-image)
      (define-key map "\M-]" 'w3m-zoom-in-image))
    (define-key map "\M-i" 'w3m-save-image)
    (define-key map "l" 'w3m-view-previous-page)
    (define-key map "\C-l" 'recenter)
    (define-key map [(control L)] 'w3m-reload-this-page)
    (define-key map "M" 'w3m-view-url-with-external-browser)
    (define-key map "n" 'w3m-view-next-page)
    (define-key map "N" 'w3m-namazu)
    (define-key map "\M-n" 'w3m-copy-buffer)
    (define-key map "\M-k" 'w3m-cookie)
    (define-key map "\C-c\C-t" 'w3m-copy-buffer)
    (define-key map "\C-c\C-p" 'w3m-previous-buffer)
    (define-key map "\C-c\C-n" 'w3m-next-buffer)
    (define-key map "\C-c\C-w" 'w3m-delete-buffer)
    (define-key map "\C-c\M-w" 'w3m-delete-other-buffers)
    (define-key map "\C-c\C-s" 'w3m-select-buffer)
    (define-key map "\C-c\C-a" 'w3m-switch-buffer)
    (define-key map "o" 'w3m-history)
    (define-key map "O" 'w3m-db-history)
    (define-key map "p" 'w3m-view-previous-page)
    (define-key map "P" 'undecided) ;; reserved for print-this-buffer.
    (define-key map "q" 'w3m-close-window)
    (define-key map "Q" 'w3m-quit)
    (define-key map "r" 'w3m-redisplay-this-page)
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
    (define-key map [(shift right)] 'w3m-shift-left)
    (define-key map [(shift left)] 'w3m-shift-right)
    (define-key map "\M-l" 'w3m-horizontal-recenter)
    (define-key map "\C-a" 'w3m-beginning-of-line)
    (define-key map "\C-e" 'w3m-end-of-line)
    (define-key map "." 'beginning-of-buffer)
    (define-key map "^" 'w3m-view-parent-page)
    (define-key map "]" 'w3m-next-form)
    (define-key map "[" 'w3m-previous-form)
    (define-key map "}" 'w3m-next-image)
    (define-key map "{" 'w3m-previous-image)
    (define-key map "C" (make-sparse-keymap))
    (define-key map "Ct" 'w3m-redisplay-with-content-type)
    (define-key map "Cc" 'w3m-redisplay-with-charset)
    (define-key map "CC" 'w3m-redisplay-and-reset)
    (define-key map "\C-c\C-b" 'report-emacs-w3m-bug)
    (define-key map "\C-c\C-c" 'w3m-submit-form)
    (define-key map "\C-c\C-k" 'w3m-process-stop)
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
Even so, if there are other windows in the frame or there are no other
visible frames, it won't delete the frame.  Return t if deleting the
frame or a window in the frame is succeeded."
  (let ((frame (selected-frame))
	(window (selected-window)))
    (cond ((eq w3m-initial-frame frame)
	   (if (eq (next-window) window)
	       (if (w3m-static-if (featurep 'xemacs)
		       (filtered-frame-list
			(lambda (f)
			  (not (or (eq f frame)
				   (frame-iconified-p f)))))
		     (filtered-frame-list
		      (lambda (f)
			(unless (eq f frame)
			  (member '(visibility . t) (frame-parameters f))))))
		   (progn
		     (delete-frame frame)
		     t))
	     (delete-window window)
	     t))
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
	      (w3m-display-message "")))
    (w3m-delete-frame-maybe)
    (dolist (buffer (w3m-list-buffers t))
      (w3m-cancel-refresh-timer buffer)
      (kill-buffer buffer))
    (w3m-select-buffer-close-window)
    (w3m-cache-shutdown)
    (w3m-arrived-shutdown)
    (w3m-process-shutdown)
    (when w3m-use-cookies
      (w3m-cookie-shutdown))
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

(defun w3m-clean-hook-options ()
  "Remove old stuffs from hook options to avoid redundant function calls."
  (dolist (elem '((w3m-mode-hook w3m-setup-header-line
				 w3m-setup-widget-faces
				 w3m-update-tab-line)
		  (w3m-fontify-after-hook w3m-header-line-insert
					  w3m-update-tab-line)
		  (w3m-display-hook w3m-select-buffer-update
				    w3m-setup-favicon
				    w3m-xmas-update-tab-in-gutter)
		  (w3m-delete-buffer-hook w3m-select-buffer-update
					  w3m-update-tab-line)))
    (dolist (func (cdr elem))
      (when (memq func (symbol-value (car elem)))
	(message "Remove `%s' from `%s'" func (car elem))
	(remove-hook (car elem) func)))))

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
\\[w3m-redisplay-this-page]	Redisplay this page.
\\[w3m-redisplay-with-content-type]	Redisplay this page with specified content-type.
\\[w3m-redisplay-with-charset]	Redisplay this page with specified charset.
\\[w3m-redisplay-and-reset]	Redisplay this page and reset of specified charset and content-type.

\\[w3m-next-anchor]	Jump to next anchor.
\\[w3m-previous-anchor]	Jump to previous anchor.
\\[w3m-next-form]	Jump to next form.
\\[w3m-previous-form]	Jump to previous form.
\\[w3m-next-image]	Jump to next image.
\\[w3m-previous-image]	Jump to previous image.

\\[w3m-view-previous-page]	Back to previous page.
\\[w3m-view-next-page]	Forward to next page.
\\[w3m-view-parent-page]	View the parent page.

\\[w3m-goto-url]	Goto URL.
\\[w3m-goto-url-new-session]	Goto URL in the new session.
\\[w3m-gohome]	Goto home page.

\\[w3m-download-this-url]	Download this url.
\\[w3m-print-this-url]	Print this url.
\\[w3m-view-image]	View image.
\\[w3m-save-image]	Save image.
\\[w3m-toggle-inline-images]	Toggle displaying of inline images on current buffer.
\\[w3m-zoom-out-image]	Zoom out image on cursor point.
\\[w3m-zoom-in-image]	Zoom in image on cursor point.

\\[w3m-print-current-url]	Print the current url.
\\[w3m-view-url-with-external-browser]	View current url with external browser.
\\[w3m-view-source]	Display source of this current buffer.
\\[w3m-view-header]	Display header of this current buffer.
\\[w3m-edit-current-url]	Edit the local file pointed by the URL of current page.
\\[w3m-edit-this-url]	Edit the local file by the under the point.

\\[w3m-scroll-up-or-next-url]	Scroll up or go to next url.
\\[w3m-scroll-down-or-previous-url]	Scroll down or go to previous url.
\\[w3m-scroll-left]	Scroll to the left.
\\[w3m-scroll-right]	Scroll to the right.
\\[w3m-shift-left]	Shift to the left.
\\[w3m-shift-right]	Shift to the right.
\\[w3m-horizontal-recenter]	Recenter horizontally.
\\[w3m-beginning-of-line]	Go to the entire beginning of line.
\\[w3m-end-of-line]	Go to the entire end of line.

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
\\[w3m-antenna-add-current-url]	Add the current page to antenna.
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
\\[w3m-delete-other-buffers]	Kill other w3m buffers.

\\[w3m]	w3m.
\\[w3m-close-window]	Close this window and make the other buffer current.
\\[w3m-quit]	Quit browsing WWW after updating arrived URLs list.

\\[describe-mode]	describe-mode.

\\[report-emacs-w3m-bug]	Send a bug report.
"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'w3m-mode)
  (setq mode-name "w3m")
  (use-local-map w3m-mode-map)
  (setq truncate-lines t
	w3m-display-inline-images w3m-default-display-inline-images)
  (when w3m-auto-show
    (when (boundp 'auto-hscroll-mode)
      (set (make-local-variable 'auto-hscroll-mode) nil))
    (when (boundp 'automatic-hscrolling)
      (set (make-local-variable 'automatic-hscrolling) nil))
    (when (boundp 'auto-show-mode)
      (set (make-local-variable 'auto-show-mode) nil))
    (when (boundp 'hscroll-mode)
      (set (make-local-variable 'hscroll-mode) nil)))
  (make-local-variable 'list-buffers-directory)
  (w3m-clean-hook-options)
  (w3m-setup-toolbar)
  (w3m-setup-menu)
  (run-hooks 'w3m-mode-setup-functions)
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
  "Scroll text of current window downward ARG lines; or go to previous url."
  (interactive "P")
  (if (pos-visible-in-window-p (point-min))
      (if w3m-previous-url
	  (w3m-goto-url w3m-previous-url)
	(signal 'beginning-of-buffer nil))
    (scroll-down arg)))

(defun w3m-scroll-left (arg)
  "Scroll to the left.
Scroll size is `w3m-horizontal-scroll-columns' columns
or prefix ARG columns."
  (interactive "P")
  (w3m-horizontal-scroll 'left (if arg
				   (prefix-numeric-value arg)
				 w3m-horizontal-scroll-columns)))

(defun w3m-scroll-right (arg)
  "Scroll to the right.
Scroll size is `w3m-horizontal-scroll-columns' columns
or prefix ARG columns."
  (interactive "P")
  (w3m-horizontal-scroll 'right (if arg
				    (prefix-numeric-value arg)
				  w3m-horizontal-scroll-columns)))

(defun w3m-shift-left (arg)
  "Shift to the left.
Shift size is `w3m-horizontal-shift-columns' columns
or prefix ARG columns."
  (interactive "P")
  (w3m-horizontal-scroll 'left (if arg
				   (prefix-numeric-value arg)
				 w3m-horizontal-shift-columns)))

(defun w3m-shift-right (arg)
  "Shift to the right.
Shift size is `w3m-horizontal-shift-columns' columns
or prefix ARG columns."
  (interactive "P")
  (w3m-horizontal-scroll 'right (if arg
				    (prefix-numeric-value arg)
				  w3m-horizontal-shift-columns)))

(defvar w3m-horizontal-scroll-done nil)
(make-variable-buffer-local 'w3m-horizontal-scroll-done)
(defvar w3m-current-position '(-1 0 0))
(make-variable-buffer-local 'w3m-current-position)

(defun w3m-auto-show ()
  "Automatic horizontal scroll."
  (when (and w3m-auto-show
	     (not w3m-horizontal-scroll-done)
	     (not (and (eq last-command this-command)
		       (or (eq (point) (point-min))
			   (eq (point) (point-max)))))
	     (markerp (nth 1 w3m-current-position))
	     (markerp (nth 2 w3m-current-position))
	     (>= (point) (marker-position (nth 1 w3m-current-position)))
	     (<= (point) (marker-position (nth 2 w3m-current-position))))
    (w3m-horizontal-on-screen))
  (setq w3m-horizontal-scroll-done nil))

;; XEmacs bugs ?
(w3m-static-if (and (featurep 'xemacs) (featurep 'mule))
    (progn
      (defun w3m-window-hscroll (&optional window)
	"Simular function of window-hscroll() for XEmacs."
	(let ((hs (window-hscroll window))
	      (spos (point-at-bol))
	      (epos (point-at-eol))
	      (buf (window-buffer window)))
	  (save-selected-window
	    (save-excursion
	      (set-buffer buf)
	      (beginning-of-line)
	      (condition-case nil
		  (forward-char hs)
		(error (goto-char (point-max))))
	      (if (< epos (point))
		  (+ hs (- (string-width (buffer-substring spos epos)) (- epos spos)))
		(string-width (buffer-substring spos (point))))))))

      (defmacro w3m-current-column ()
	"Simular function of current-column() for XEmacs."
	`(- (point) (point-at-bol)))

      (defun w3m-set-window-hscroll (window columns)
	"Simular function of set-window-hscroll() for XEmacs."
	(save-excursion
	  (move-to-column (max columns 0))
	  (if (> columns (current-column))
	      (set-window-hscroll window (+ (- (point-at-eol) (point-at-bol))
					    (- columns (current-column))))
	    (set-window-hscroll window (- (point) (point-at-bol))))))
      )
  (defalias 'w3m-window-hscroll 'window-hscroll)
  (defalias 'w3m-current-column 'current-column)
  (defalias 'w3m-set-window-hscroll 'set-window-hscroll))

(defun w3m-horizontal-scroll (type cols)
  "Horizontal scroll for shift|scroll functions.
TYPE is either 'left or 'right and COLS is columns."
  (setq w3m-horizontal-scroll-done t)
  (let ((inhibit-point-motion-hooks t))
    (w3m-set-window-hscroll (selected-window)
			    (max 0
				 (+ (w3m-window-hscroll)
				    (if (eq type 'left) cols (- cols)))))
    (let ((hs (w3m-window-hscroll))
	  (pos (point)))
      (unless (and (>= (- (current-column) hs) 0)
		   (< (- (current-column) hs) (window-width)))
	(move-to-column (if (eq type 'left) hs
			  (+ hs (window-width)
			     (w3m-static-if (featurep 'xemacs) -3 -2))))))))

(defun w3m-horizontal-on-screen ()
  "Horizontal scroll and show current position in the window."
  (when w3m-auto-show
    (setq w3m-horizontal-scroll-done t)
    (let ((hs (w3m-window-hscroll))
	  (inhibit-point-motion-hooks t))
      (unless (and (>= (- (current-column) hs) 0)
		   (< (+ (- (current-column) hs)
			 (if (eolp) 0
			   (w3m-static-if (featurep 'xemacs)
			       3 2)))	;; '$$'
		      (window-width)))
	(w3m-set-window-hscroll (selected-window)
				(max 0
				     (- (current-column)
					(if (> (window-hscroll) (w3m-current-column))
					    (/ (window-width)
					       w3m-horizontal-scroll-division)
					  (* (/ (window-width)
						w3m-horizontal-scroll-division)
					     (1- w3m-horizontal-scroll-division))))))))))

(defun w3m-horizontal-recenter (&optional arg)
  "Recenter horizontally.  With ARG, put point on column ARG."
  (interactive "P")
  (cond ((< (w3m-current-column) (window-hscroll))
	 (move-to-column (w3m-window-hscroll))
	 (setq arg 0))
	((>= (w3m-current-column) (+ (window-hscroll) (window-width)))
	 (move-to-column (+ (w3m-window-hscroll) (window-width) -2))
	 (setq arg -1))
	((listp arg)
	 (setq arg (car arg))))
  (w3m-set-window-hscroll (selected-window)
			  (if (numberp arg)
			      (if (>= arg 0)
				  (max (- (current-column) arg) 0)
				(let* ((home (point))
				       (inhibit-point-motion-hooks t)
				       (maxcolumn (prog2
						      (end-of-line)
						      (1- (current-column))
						    (goto-char home))))
				  (max (min (- (current-column)
					       (window-width)
					       arg
					       -2)
					    maxcolumn)
				       0)))
			    (max (- (current-column) (/ (window-width) 2) -1)
				 0))))

(defun w3m-beginning-of-line (&optional arg)
  "Like `beginning-of-line', except that set window-hscroll to zero first."
  (interactive "P")
  (when (listp arg)
    (setq arg (car arg)))
  (w3m-set-window-hscroll (selected-window) 0)
  (beginning-of-line arg))

(defun w3m-end-of-line (&optional arg)
  "Like `end-of-line', except that scroll left to make the line end
positions around there (+/-3 lines) visible."
  (interactive "P")
  (when (listp arg)
    (setq arg (car arg)))
  (forward-line (1- (or arg 1)))
  (let ((inhibit-point-motion-hooks t)
	home)
    (end-of-line)
    (setq home (point)
	  arg (current-column))
    (dolist (n '(-3 -2 -1 1 2 3))
      (forward-line n)
      (end-of-line)
      (setq arg (max (current-column) arg))
      (goto-char home)))
  (setq temporary-goal-column arg
	this-command 'next-line)
  (w3m-set-window-hscroll (selected-window)
			  (max (- arg (window-width) -2) 0)))

(defun w3m-pattern-uri-replace (uri format)
  "Create a new uri based on FORMAT from URI matched by last search."
  (replace-match format nil nil uri))

(defun w3m-uri-replace (uri)
  "Rewrite a given URI based on rules of `w3m-uri-replace-alist'."
  (catch 'found-replacement
    (dolist (elem w3m-uri-replace-alist uri)
      (when (string-match (car elem) uri)
	(if (setq uri
		  (cond
		   ((consp (cdr elem))
		    (apply (cadr elem) uri (cddr elem)))
		   ;; Rest conditions are inserted to keep backward
		   ;; compatibility.
		   ((functionp (cdr elem))
		    (funcall (cdr elem) uri))
		   ((stringp (cdr elem))
		    (w3m-pattern-uri-replace uri (cdr elem)))))
	    (throw 'found-replacement uri)
	  (error "Invalid replacement: %s" elem))))))

(defun w3m-goto-mailto-url (url &optional post-data)
  (let ((before (nreverse (buffer-list)))
	comp info buffers buffer function)
    (save-window-excursion
      (if (and (symbolp w3m-mailto-url-function)
	       (fboundp w3m-mailto-url-function))
	  (funcall w3m-mailto-url-function url)
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
	    (progn
	      (setq info (rfc2368-parse-mailto-url url))
	      (apply comp
		     (append (mapcar (lambda (x)
				       (cdr (assoc x info)))
				     '("To" "Subject"))
			     (if post-data
				 (list
				  (list (cons
					 "body"
					 (or (and
					      (consp post-data)
					      (concat (car post-data) "\n"))
					     (concat post-data "\n")))))))))
	  ;; without rfc2368.el.
	  (funcall comp (match-string 1 url)))))
    (setq buffers (nreverse (buffer-list)))
    (save-current-buffer
      (while buffers
	(setq buffer (car buffers)
	      buffers (cdr buffers))
	(unless (memq buffer before)
	  (set-buffer buffer)
	  (when (setq function
		      (cdr (assq major-mode
				 w3m-mailto-url-popup-function-alist)))
	    (setq buffers nil)))))
    (when function
      (let (special-display-buffer-names
	    special-display-regexps
	    same-window-buffer-names
	    same-window-regexps)
	(funcall function buffer)))))

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

(defun w3m-file-directory-p (file)
  "Emulate `file-directory-p' function for remote file."
  (when (file-exists-p file)
    (let (dirp (i 10))
      (catch 'loop
	(while (> i 0)
	  (setq dirp (car (file-attributes file)))
	  (if (stringp dirp)
	      (setq file (expand-file-name
			  dirp
			  (file-name-directory (directory-file-name file)))
		    i (1- i))
	    (throw 'loop dirp)))))))

(defun w3m-goto-ftp-url (url &optional filename)
  "Copy a remote file to a local file if URL looks like a file, otherwise
run `dired-other-window' for URL using `ange-ftp' or `efs'.  Optional
FILENAME specifies the name of a local file.  If FILENAME is omitted,
it will prompt user where to save a file."
  (let ((ftp (w3m-convert-ftp-url-for-emacsen url))
	file)
    (if (or (string-equal "/" (substring ftp -1))
	    ;; `file-directory-p' takes a long time for remote files.
	    ;; `file-directory-p' returns 't' in FSF Emacsen, anytime.
	    (w3m-file-directory-p ftp))
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
			   (w3m-display-message ""))
			 (progn
			   (delete-file filename)
			   t))
		  (error "Permission denied, %s" filename)))
	(copy-file ftp filename)
	(message "Wrote %s" filename)))))

(eval-and-compile
  (unless (fboundp 'w3m-add-local-hook)
    (defun w3m-add-local-hook (hook function &optional append)
      "Add to the buffer-local value of HOOK the function FUNCTION.
Note: This function is designed for the other emacsen than Emacs21."
      (make-local-hook hook)
      (add-hook hook function append t))))

(defun w3m-store-current-position ()
  "Store the current positions to `w3m-current-position' before every
commands.  This function is designed as the hook function which is
registered to `pre-command-hook' by `w3m-buffer-setup'."
  (setq w3m-current-position (list (point)
				   (copy-marker (line-beginning-position))
				   (copy-marker (line-end-position)))))

(defun w3m-check-current-position ()
  "Call functions set to `w3m-after-cursor-move-hook' after cursor is
moved.  This function is designed as the hook function which is
registered to `post-command-hook' by `w3m-buffer-setup'."
  (when (/= (point) (car w3m-current-position))
    (run-hooks 'w3m-after-cursor-move-hook)))

(defsubst w3m-buffer-setup ()
  "When this buffer's major mode is not w3m-mode, generate an
appropriate buffer and select it."
  (unless (eq major-mode 'w3m-mode)
    (set-buffer (get-buffer-create "*w3m*"))
    (unless (eq major-mode 'w3m-mode)
      (w3m-mode)))
  (w3m-add-local-hook 'pre-command-hook 'w3m-store-current-position)
  (w3m-add-local-hook 'post-command-hook 'w3m-check-current-position)
  (setq mode-line-buffer-identification
	(list "%b "
	      '((w3m-current-process
		 w3m-modeline-process-status-on
		 (w3m-current-ssl
		  (w3m-display-inline-images
		   w3m-modeline-ssl-image-status-on
		   w3m-modeline-ssl-status-off)
		  (w3m-display-inline-images
		   w3m-modeline-image-status-on
		   w3m-modeline-status-off))))
	      " / "
	      'w3m-current-title)))

;;;###autoload
(defun w3m-goto-url
  (url &optional reload charset post-data referer handler qsearch)
  "Retrieve contents of URL.
If the second argument RELOAD is non-nil, reload a content of URL.
Except that if it is 'redisplay, re-display the page without reloading.
The third argument CHARSET specifies a charset to be used for decoding
a content.
The fourth argument POST-DATA should be a string or a cons cell.  If
it is a string, it makes this function request a body as if the
content-type is \"x-www-form-urlencoded\".  If it is a cons cell, the
car of a cell is used as the content-type and the cdr of a cell is
used as the body.
If the fifth argument REFERER is specified, it is used for a Referer:
field for this request.
You can also use \"quicksearch\" url schemes such as \"gg:emacs\" which
would search for the term \"emacs\" with the Google search engine.  See
the `w3m-search' function and the variable `w3m-uri-replace-alist'."
  (interactive
   (list
    (w3m-input-url nil
		   (or (w3m-url-at-point)
		       (when (stringp w3m-current-url)
			 (if (string-match "\\`about://\\(header\\|source\\)/"
					   w3m-current-url)
			     (substring w3m-current-url (match-end 0))
			   w3m-current-url))))
    current-prefix-arg
    (w3m-static-if (fboundp 'universal-coding-system-argument)
	coding-system-for-read)
    nil ;; post-data
    nil ;; referer
    nil ;; handler
    t)) ;; qsearch
  (set-text-properties 0 (length url) nil url)
  (setq url (w3m-uri-replace url))
  (unless (or (w3m-url-local-p url)
	      (string-match "\\`about:" url))
    (setq url (w3m-url-transfer-encode-string url w3m-default-coding-system)))
  (cond
   ;; process mailto: protocol
   ((string-match "\\`mailto:" url)
    (w3m-goto-mailto-url url post-data))
   ;; process ftp: protocol
   ((and w3m-use-ange-ftp
	 (string-match "\\`ftps?://" url)
	 (not (string= "text/html" (w3m-local-content-type url))))
    (w3m-goto-ftp-url url))
   ;; find-file directly
   ((condition-case nil
	(and (w3m-url-local-p url)
	     w3m-local-find-file-function
	     (let ((base-url (w3m-url-strip-fragment url))
		   (match (car w3m-local-find-file-regexps))
		   nomatch file)
	       (and (or (not match)
			(string-match match base-url))
		    (not (and (setq nomatch (cdr w3m-local-find-file-regexps))
			      (string-match nomatch base-url)))
		    (setq file (w3m-url-to-file-name base-url))
		    (file-exists-p file)
		    (prog1
			t
		      (unless w3m-current-url
			(erase-buffer)
			(set-buffer-modified-p nil)
			(setq w3m-current-url base-url))
		      (save-excursion
			(funcall (if (functionp w3m-local-find-file-function)
				     w3m-local-find-file-function
				   (eval w3m-local-find-file-function))
				 file))))))
      (error nil)))
   ((w3m-url-valid url)
    (w3m-buffer-setup)			; Setup buffer.
    (w3m-arrived-setup)			; Setup arrived database.
    (switch-to-buffer (current-buffer))
    (w3m-cancel-refresh-timer (current-buffer))
    (when w3m-current-process
      (error "%s"
	     (substitute-command-keys "
Cannot run two w3m processes simultaneously \
\(Type `\\<w3m-mode-map>\\[w3m-process-stop]' to stop asynchronous process)")))
    (w3m-process-stop (current-buffer))	; Stop all processes retrieving images.
    ;; Store the current position in the history structure.
    (w3m-history-store-position)
    ;; Access url group
    (if (string-match "\\`group:" url)
	(let ((urls (mapcar 'w3m-url-decode-string
			    (split-string (substring url (match-end 0)) "&"))))
	  (w3m-process-do
	      (type (prog1
			(w3m-goto-url (car urls) nil nil nil nil nil qsearch)
		      (dolist (url (cdr urls))
			(save-excursion
			  (set-buffer (w3m-copy-buffer nil nil nil 'empty))
			  (save-window-excursion
			    (w3m-goto-url url nil nil nil nil nil qsearch))))))
	    type))
      ;; Retrieve the page.
      (lexical-let ((orig url)
		    (url (w3m-url-strip-authinfo url))
		    (reload (and (not (eq reload 'redisplay)) reload))
		    (redisplay (eq reload 'redisplay))
		    (charset charset)
		    (post-data post-data)
		    (referer referer)
		    (name))
	(when w3m-current-forms
	  ;; Store the current forms in the history structure.
	  (w3m-history-plist-put :forms w3m-current-forms nil nil t))
	;; Set current forms using the history structure.
	(when (setq w3m-current-forms
		    (when (and (null post-data)	; If post, always reload.
			       (w3m-cache-available-p url))
		      (w3m-history-plist-get :forms url nil t)))
	  ;; Mark that the form is from history structure.
	  (setq w3m-current-forms (cons t w3m-current-forms)))
	(when (and post-data (w3m-history-assoc url))
	  ;; Remove processing url's forms from the history structure.
	  (w3m-history-remove-properties '(:forms) url nil t))
	;; local directory URL check
	(when (and (w3m-url-local-p url)
		   (file-directory-p (w3m-url-to-file-name url))
		   (setq url (file-name-as-directory url))
		   (eq w3m-local-directory-view-method 'w3m-dtree)
		   (string-match "\\`file:///" url))
	  (setq url (replace-match "about://dtree/" nil nil url))
	  (setq orig url))
	(and (string-match w3m-url-components-regexp url)
	     (match-beginning 8)
	     (setq name (match-string 9 url)
		   url (substring url 0 (match-beginning 8))))
	(lexical-let ((ct (w3m-arrived-content-type url))
		      (charset (or charset (w3m-arrived-content-charset url)))
		      (real-url))
	  (when (and (not ct)
		     (w3m-url-local-p url)
		     (string= "unknown" (w3m-local-content-type url)))
	    (let ((s (completing-read
		      (format "Input %s's content type (default %s): "
			      (file-name-nondirectory url)
			      w3m-default-content-type)
		      w3m-content-type-alist nil t)))
	      (setq ct (if (string= "" s) w3m-default-content-type s))))
	  (let ((w3m-current-buffer (current-buffer)))
	    (w3m-process-do
	     (action
	      (if (and (not reload)
		       (not redisplay)
		       (stringp w3m-current-url)
		       (string= url w3m-current-url))
		  (progn
		    (w3m-refontify-anchor)
		    'cursor-moved)
		(setq w3m-image-only-page nil
		      w3m-current-process
		      (w3m-retrieve-and-render (w3m-url-strip-fragment orig)
					       reload charset ct post-data
					       referer handler))))
	     (with-current-buffer w3m-current-buffer
	       (setq w3m-current-process nil)
	       (setq real-url (w3m-real-url url))
	       (cond
		((not action)
		 (w3m-history-push real-url
				   (list :title (file-name-nondirectory url)))
		 (w3m-history-push w3m-current-url)
		 (w3m-refontify-anchor))
		((not (eq action 'cursor-moved))
		 (w3m-history-push w3m-current-url
				   (list :title w3m-current-title))
		 (w3m-history-add-properties (list :referer referer
						   :post-data post-data)
					     nil nil t)
		 (unless w3m-toggle-inline-images-permanently
		   (setq w3m-display-inline-images
			 w3m-default-display-inline-images))
		 (cond ((w3m-display-inline-images-p)
			(and w3m-force-redisplay (sit-for 0))
			(w3m-toggle-inline-images 'force reload))
		       ((and (w3m-display-graphic-p)
			     w3m-image-only-page)
			(and w3m-force-redisplay (sit-for 0))
			(w3m-toggle-inline-image 'force reload)))
		 (setq buffer-read-only t)
		 (set-buffer-modified-p nil)))
	       (when action
		 (w3m-arrived-add (setq orig
					(if (when name
					      (w3m-search-name-anchor name))
					    (w3m-url-strip-authinfo orig)
					  (goto-char (point-min))
					  url))
				  w3m-current-title
				  (w3m-last-modified url)
				  (current-time)
				  (or charset
				      (w3m-arrived-content-charset orig))
				  (or ct
				      (w3m-arrived-content-type orig))))
	       (setq list-buffers-directory w3m-current-title)
	       ;; must be `w3m-current-url'
	       (setq default-directory (w3m-current-directory w3m-current-url))
	       (w3m-update-toolbar)
	       (w3m-select-buffer-update)
	       (run-hook-with-args 'w3m-display-functions (or real-url url))
	       (run-hook-with-args 'w3m-display-hook (or real-url url))
	       ;; restore position must call after hooks for localcgi.
	       (when (or reload redisplay)
		 (w3m-history-restore-position))
	       (w3m-refresh-at-time))))))))
   (t (w3m-message "Invalid URL: %s" url))))

(defun w3m-current-directory (url)
  (let (file)
    (file-name-as-directory
     (if (and url (stringp url))
	 (if (string-match "\\`ftp://" url)
	     (progn
	       (setq file (w3m-convert-ftp-url-for-emacsen url))
	       (if (string-match "/\\`" file)
		   file
		 (file-name-directory file)))
	   (setq file (w3m-url-to-file-name url))
	   (if (and file (file-exists-p file))
	       (if (file-directory-p file)
		   file
		 (file-name-directory file))
	     w3m-profile-directory))
       w3m-profile-directory))))

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
  (when (and (w3m-url-valid url) buffer (get-buffer buffer))
    (if (get-buffer-window buffer)
	(save-selected-window
	  (pop-to-buffer buffer)
	  (with-current-buffer buffer
	    (w3m-cancel-refresh-timer buffer)
	    (w3m-goto-url url)))
      (with-current-buffer buffer
	(w3m-cancel-refresh-timer buffer)))))

;;;###autoload
(defun w3m-goto-url-new-session
  (url &optional reload charset post-data referer interactive-p)
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
	coding-system-for-read)
    nil ;; post-data
    nil ;; referer
    t)) ;; interactive-p
  (if (eq 'w3m-mode major-mode)
      (progn
	(switch-to-buffer (w3m-copy-buffer nil nil interactive-p 'empty))
	(w3m-display-progress-message url)
	;; When new URL has `name' portion, we have to goto the base url
	;; because generated buffer has no content at this moment.
	(when (and (string-match w3m-url-components-regexp url)
		   (match-beginning 8))
	  (w3m-goto-url (substring url 0 (match-beginning 8))
			reload charset post-data referer
			nil interactive-p))
	(w3m-goto-url url reload charset post-data referer
		      nil interactive-p))
    (w3m url t)))

(defun w3m-move-point-for-localcgi (url)
  (when (and (w3m-url-local-p url)
	     (file-directory-p (w3m-url-to-file-name url))
	     (not (eq w3m-local-directory-view-method 'w3m-dtree))
	     (= (point-min) (point))
	     (w3m-search-name-anchor "current" 'quiet))
    (recenter (/ (window-height) 5))))

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

(defun w3m-redisplay-this-page (&optional arg)
  "Redisplay current page."
  (interactive "P")
  (when arg
    (setq w3m-display-inline-images (not w3m-display-inline-images)))
  (w3m-goto-url w3m-current-url 'redisplay))

(defun w3m-redisplay-and-reset (&optional arg)
  "Redisplay current page and reset of specified charset and content-type."
  (interactive "P")
  (w3m-arrived-modify w3m-current-url nil nil nil 'reset 'reset)
  (w3m-redisplay-this-page arg))

(defun w3m-redisplay-with-charset (&optional arg)
  "Redisplay current page with specified charset.
If input is nil, use default coding-system on w3m."
  (interactive "P")
  (let ((charset
	 (w3m-read-content-charset
	  (format "Content-charset (current %s, default reset): "
		  w3m-current-coding-system)
	   ;; Default action is reseting charset entry in arrived DB.
	  'reset)))
    (w3m-arrived-modify w3m-current-url nil nil nil charset nil)
    (w3m-redisplay-this-page arg)))

(defun w3m-redisplay-with-content-type (&optional arg)
  "Redisplay current page with specified content-type.
If input is nil, use default content-type on w3m."
  (interactive "P")
  (let ((url w3m-current-url) ct)
    (setq ct (completing-read
	      (format "Content-type (current %s, default reset): "
		      (or (w3m-arrived-content-type url)
			  (if (w3m-url-local-p url)
			      (w3m-local-content-type url)
			    (w3m-content-type url))))
	      w3m-content-type-alist nil t))
    ;; Default action is reseting content-type entry in arrived DB.
    (when (string= ct "")
      (setq ct 'reset))
    (w3m-arrived-modify url nil nil nil nil ct)
    (w3m-redisplay-this-page arg)))

;;;###autoload
(defun w3m (&optional url new-session interactive-p)
  "Visit the World Wide Web page using the external command w3m, w3mmee
or w3m-m17n.

When you invoke this command interactively for the first time, it will
visit the home page which is specified by the option `w3m-home-page',
otherwise if the option `w3m-quick-start' is nil (default t) or the
value of `w3m-home-page' is nil, it will prompt you for a URL where
you wish to go.  The option `w3m-pop-up-frames' controls whether this
command should make a new frame for the session.

When the session for w3m has already been opened, this command will
popup the existing window or frame (it is controlled by the option
`w3m-pop-up-frames'), otherwise if the option `w3m-quick-start' is nil
\(default t), it will prompt you for a URL where you wish to go in the
existing session.

In addition, if the prefix argument is given or you enter the empty
string for the prompt, it will visit the home page which is specified
by the option `w3m-home-page'.

URL should be a string which defaults to the value of `w3m-home-page'
or \"about:\".

You can run this command in the batch mode something like:

  emacs -f w3m http://emacs-w3m.namazu.org/ &

In that case, or if this command is called non-interactively, the
value of `w3m-pop-up-frames' will be ignored (treated as nil) and it
will not popup a frame.

Optional NEW-SESSION is intended to be used by the command
`w3m-goto-url-new-session' to create a new session."
  (interactive
   (let ((default (if (w3m-alive-p) 'popup w3m-home-page)))
     (list
      (if current-prefix-arg
	  default
	(w3m-input-url nil nil default w3m-quick-start))
      nil ;; new-session
      t))) ;; interactive-p
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
      (w3m-display-progress-message url)
      (w3m-mode))
    (unwind-protect
	(unless nofetch
	  (w3m-goto-url url nil nil nil nil nil interactive-p))
      (unless w3m-current-url
	(erase-buffer)
	(set-buffer-modified-p nil))
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
  (interactive
   (list (region-beginning)
	 (region-end)
	 (w3m-expand-file-name-as-url
	  (or (buffer-file-name) default-directory))))
  (save-restriction
    (w3m-process-stop (current-buffer))
    (narrow-to-region start end)
    (w3m-clear-local-variables)
    (let ((w3m-current-buffer (current-buffer)))
      (setq w3m-current-url url
	    w3m-current-base-url url
	    w3m-current-title (let (w3m-use-refresh)
				(w3m-rendering-multibyte-buffer)))
      (w3m-fontify)
      (when (w3m-display-inline-images-p)
	(and w3m-force-redisplay (sit-for 0))
	(w3m-toggle-inline-images 'force)))))


;;; About:
(defun w3m-about (url &rest args)
  (insert "<!doctype html public \"-//W3C//DTD HTML 3.2//EN\">
<html>
<head><title>About emacs-w3m</title></head>
<body>
<center>
Welcome to <a href=\"http://emacs-w3m.namazu.org/\">\
<img src=\"about://emacs-w3m.gif\" alt=\"emacs-w3m\" width=\"83\"
height=\"14\"></a>!<br><br>
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

(defun w3m-make-separator ()
  (if (string= w3m-language "Japanese")
      (w3m-static-if (boundp 'MULE)
	  ;; `make-string' doesn't support Japanese chars
	  ;; and the 1st arg to `make-char' should be int.
	  (let ((default-mc-flag t))
	    (with-temp-buffer
	      (insert-char (make-char (symbol-value 'lc-jp)
				      40 44)
			   (/ (if (< 0 w3m-fill-column)
				  w3m-fill-column
				(+ (window-width) (or w3m-fill-column -1)))
			      2))
	      (buffer-string)))
	(make-string (/ (if (< 0 w3m-fill-column)
			    w3m-fill-column
			  (+ (window-width) (or w3m-fill-column -1)))
			2)
		     (make-char 'japanese-jisx0208 40 44)))
    (make-string (if (< 0 w3m-fill-column)
		     w3m-fill-column
		   (+ (window-width) (or w3m-fill-column -1)))
		 ?-)))

(defun w3m-about-header (url &optional no-decode no-cache &rest args)
  (when (string-match "\\`about://header/" url)
    (setq url (substring url (match-end 0)))
    (insert "Page Information\n"
	    "\nTitle:          " (or (w3m-arrived-title
				      (w3m-url-strip-authinfo url))
				     "")
	    "\nURL:            " url
	    "\nDocument Type:  " (or (w3m-content-type url) "")
	    "\nLast Modified:  "
	    (let ((time (w3m-last-modified url)))
	      (if time (current-time-string time) ""))
	    (let ((anchor (with-current-buffer w3m-current-buffer
			    (and (equal url w3m-current-url) (w3m-anchor)))))
	      (if anchor
		  (concat "\nCurrent Anchor: " anchor)
		"")))
    (let ((ct (w3m-arrived-content-type url))
	  (charset (w3m-arrived-content-charset url))
	  (separator (w3m-make-separator))
	  (case-fold-search t)
	  header ssl beg)
      (when (or ct charset)
	(insert "\n\n" separator "\n\nModifer Information\n")
	(insert "\nDocument Content-Type:  " (or ct ""))
	(insert "\nDocument Charset:       " (or charset "")))
      (when (and (not (w3m-url-local-p url))
		 (setq header (condition-case nil
				  (w3m-process-with-wait-handler
				    (w3m-w3m-get-header url no-cache handler))
				(w3m-process-timeout nil))))
	(insert "\n\n" separator "\n\nHeader Information\n\n" header)
	(goto-char (point-min))
	(when (re-search-forward "^w3m-ssl-certificate: " nil t)
	  (setq beg (match-end 0))
	  (forward-line)
	  (while (and (not (eobp)) (looking-at "^[ \t]"))
	    (forward-line))
	  (setq ssl (buffer-substring beg (point)))
	  (delete-region beg (point))
	  (goto-char beg)
	  (insert "SSL\n")
	  (goto-char (point-max))
	  (insert separator "\n\nSSL Information\n\n")
	  (setq beg (point))
	  (insert ssl)
	  (goto-char beg)
	  (while (re-search-forward "^\t" nil t)
	    (delete-char -1)
	    (when (looking-at "Certificate:")
	      (insert "\n"))))))
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
	  (when url
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
			      (w3m-encode-specials-string title))
			    (if about "&gt;" "")))))
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

(defun w3m-about-db-history (url &rest args)
  (let ((start 0)
	(size nil)
	(width (- (if (< 0 w3m-fill-column)
		      w3m-fill-column
		    (+ (window-width) (or w3m-fill-column -1)))
		  18))
	(now (current-time))
	title time alist date prev next page total)
    (when (string-match "\\`about://db-history/\\?" url)
      (dolist (s (split-string (substring url (match-end 0)) "&"))
	(when (string-match "\\`\\(start\\|\\(size\\)\\)=" s)
	  (set (if (match-beginning 2) 'size 'start)
	       (string-to-number (substring s (match-end 0)))))))
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
    (setq total (length alist))
    (setq alist (nthcdr start alist))
    (when size
      (when (> start 0)
	(setq prev
	      (format "about://db-history/?start=%d&size=%d"
		      (max 0 (- start size)) size)))
      (when (> (length alist) size)
	(setq next
	      (format "about://db-history/?start=%d&size=%d"
		      (+ start size) size)))
      (when (> total 0)
	(setq total (+ (/ total size) (if (> (% total size) 0) 1 0)))
	(setq page (1+ (/ start size)))))
    (insert "<html><head><title>URL history in DataBase</title>"
	    (if prev (format "<link rel=\"prev\" href=\"%s\">\n" prev) "")
	    (if next (format "<link rel=\"next\" href=\"%s\">\n" next) "")
	    (format "</head>\n<body>\n<h1>Arrived URL history in DataBase%s</h1>\n"
		    (if (and page total)
			(format " (%d/%d)" page total) "")))
    (setq prev
	  (if (or prev next)
	      (setq next
		    (concat
		     "<p align=\"left\">"
		     (if prev
			 (format "[<a href=\"%s\">Prev History</a>]" prev)
		       "")
		     (if next
			 (format "[<a href=\"%s\">Next History</a>]" next)
		       "")
		     "</p>\n"))
	    ""))
    (if (null alist)
	(insert "<em>Nothing in DataBase.</em>\n")
      (insert prev "<table cellpadding=0>
<tr><td><h2> Title/URL </h2></td><td><h2>Time/Date</h2></td></tr>\n")
      (while (and alist
		  (or (not size)
		      (>= (decf size) 0)))
	(setq url (car (car alist)))
	(setq title (w3m-arrived-title url))
	(if (or (null title)
		(string= "<no-title>" title))
	    (setq title (concat
			 "<"
			 (if (<= (length url) width)
			     url
			   (substring url 0 width)) ;; only ASCII characters.
			 ">"))
	  (when (>= (string-width title) width)
	    (setq title
		  (concat
		   (with-temp-buffer
		     (insert title)
		     (move-to-column width)
		     (buffer-substring (point-min) (point)))
		   "..."))))
	(insert (format "<tr><td><a href=\"%s\">%s</a></td>"
			url
			(w3m-encode-specials-string title)))
	(when (cdr (car alist))
	  (if (<= (w3m-time-lapse-seconds (cdr (car alist)) now)
		  64800) ;; = (* 60 60 18) 18hours ago.
	      (setq date (format-time-string "%H:%M:%S" (cdr (car alist))))
	    (setq date (format-time-string "%Y-%m-%d" (cdr (car alist)))))
	  (insert "<td>" date "</td>"))
	(insert "</tr>\n")
	(setq alist (cdr alist)))
      (insert "</table>"
	      (if next "\n<br>\n<hr>\n" "")
	      prev))
    (insert "</body></html>\n"))
  "text/html")

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

(defcustom w3m-db-history-display-size
  (and (> w3m-keep-arrived-urls 500) 500)
  "*Number of arrived URLs displayed per page"
  :group 'w3m
  :type '(choice (const :tag "All entries are displayed in single page." nil)
		 integer))

(defun w3m-db-history (&optional start size)
  "Display arrived URLs."
  (interactive
   (list nil w3m-db-history-display-size))
  (w3m-goto-url (concat
		 (format "about://db-history/?start=%d" (or start 0))
		 (if size (format "&size=%d" size) ""))))

(defun w3m-history (&optional arg)
  "Display w3m history.
If called with 'prefix argument', display arrived URLs."
  (interactive "P")
  (if (null arg)
      (w3m-goto-url "about://history/")
    (w3m-db-history nil w3m-db-history-display-size)))

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
  (or nomsg (w3m-display-message w3m-select-buffer-message)))

(unless (fboundp 'w3m-update-tab-line)
  (defalias 'w3m-update-tab-line 'ignore))

(defun w3m-select-buffer-update (&rest args)
  (when (get-buffer-window w3m-select-buffer-name)
    (save-selected-window
      (w3m-select-buffer nil 'update)))
  (w3m-update-tab-line))

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
     'w3m-delete-other-buffers
     'w3m-select-buffer-delete-other-buffers map w3m-mode-map)
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

\\[w3m-select-buffer-next-line]\
	Next line.
\\[w3m-select-buffer-previous-line]\
	Previous line.

\\[w3m-select-buffer-show-this-line]\
	Show the current buffer or scroll up.
\\[w3m-select-buffer-show-this-line-and-down]\
	Show the current buffer on this menu line or scroll\n\t it down.
\\[w3m-select-buffer-show-this-line-and-switch]\
	Show the current buffer and set cusor to w3m buffer.
\\[w3m-select-buffer-show-this-line-and-quit]\
	Show the current buffer and quit menu.

\\[w3m-select-buffer-copy-buffer]\
	Create a copy of the buffer on the current menu line,\n\tand show it.
\\[w3m-select-buffer-delete-buffer]\
	Delete the buffer on the current menu line.
\\[w3m-select-buffer-delete-other-buffers]\
	Delete w3m buffers except for the current menu line.

\\[w3m-select-buffer-toggle-style]\
	Toggle the type of split which horizon or vertical.
\\[w3m-select-buffer-recheck]\
	Recheck buffers.
\\[w3m-select-buffer-quit]\
	Quit menu.
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

(defun w3m-select-buffer-show-this-line (&optional interactive-p)
  "Show the current buffer on this menu line or scroll up its."
  (interactive (list t))
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
    (when (and interactive-p (eq obuffer buffer))
      (save-selected-window
	(pop-to-buffer buffer)
	(w3m-scroll-up-or-next-url nil)))
    (w3m-display-message w3m-select-buffer-message)
    buffer))

(defun w3m-select-buffer-show-this-line-and-down ()
  "Show the current buffer on this menu line or scroll it down."
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
  (prog1
      (w3m-select-buffer-show-this-line)
    (w3m-static-when (featurep 'xemacs)
      (save-window-excursion
	;; Update gutter tabs.
	(select-window w3m-select-buffer-window)))))

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
	pop-up-windows)
    (pop-to-buffer (w3m-select-buffer-current-buffer))
    (w3m-copy-buffer (current-buffer))
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
      (run-hooks 'w3m-delete-buffer-hook)
      (w3m-select-buffer-generate-contents
       (w3m-select-buffer-current-buffer))
      (w3m-select-buffer-show-this-line))))

(defun w3m-select-buffer-delete-other-buffers ()
  "Delete w3m buffers except for the current menu line."
  (interactive)
  (w3m-select-buffer-show-this-line)
  (w3m-delete-other-buffers (w3m-select-buffer-current-buffer)))

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
  (w3m-display-message ""))

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
    (set-keymap-parent map w3m-mode-map)
    (define-key map [mouse-2] 'w3m-goto-url)
    ;; Prevent tool-bar from being doubled under Emacs 21.
    (define-key map [tool-bar] 'undefined)
    (setq w3m-header-line-map map)))

(defun w3m-header-line-insert ()
  "Insert the header line to this buffer."
  (when (and (or (featurep 'xemacs)
		 (< emacs-major-version 21)
		 w3m-use-tab)
	     w3m-use-header-line
	     w3m-current-url
	     (eq 'w3m-mode major-mode))
    (goto-char (point-min))
    (let ((ct (w3m-arrived-content-type w3m-current-url))
	  (charset (w3m-arrived-content-charset w3m-current-url)))
      (insert (format "Location%s: " (cond ((and ct charset) " [TC]")
					   (ct " [T]")
					   (charset " [C]")
					   (t "")))))
    (w3m-add-text-properties (point-min) (point)
			     `(face w3m-header-line-location-title-face))
    (let ((start (point)))
      (insert w3m-current-url)
      (w3m-add-text-properties start (point)
			       `(face
				 w3m-header-line-location-content-face
				 mouse-face
				 highlight
				 ,(if (or (featurep 'xemacs)
					  (>= emacs-major-version 21))
				      'keymap
				    'local-map)
				 ,w3m-header-line-map
				 ,@(if (featurep 'xemacs)
				       '(help-echo
					 "button2 prompts to input URL"
					 balloon-help
					 "button2 prompts to input URL")
				     '(help-echo
				       "mouse-2 prompts to input URL"))))
      (setq start (point))
      (insert-char ?\  (max 0 (- (window-width) (current-column) 1)))
      (w3m-add-text-properties start (point)
			       `(face w3m-header-line-location-content-face))
      (unless (eolp)
	(insert "\n")))))


;;; w3m-minor-mode
(defun w3m-safe-view-this-url ()
  "View the URL of the link under point.
This command is quite similar to `w3m-view-this-url' without three
differences: (1) this command accepts no arguments, (2) this command
does not handle forms, and (3) this command does not consider URL-like
strings under the cursor.  When a unsecure page which may contain
vicious forms is viewed, this command should be used instead of
`w3m-view-this-url'."
  (interactive)
  (let ((url (w3m-url-valid (w3m-anchor))))
    (cond
     (url (w3m url))
     ((w3m-url-valid (w3m-image))
      (if (w3m-display-graphic-p)
	  (w3m-toggle-inline-image)
	(w3m-view-image)))
     (t (w3m-message "No URL at point")))))

(defun w3m-mouse-safe-view-this-url (event)
  "Perform the command `w3m-safe-view-this-url' by the mouse event."
  (interactive "e")
  (mouse-set-point event)
  (w3m-safe-view-this-url))

(defconst w3m-minor-mode-command-alist
  '((w3m-next-anchor)
    (w3m-previous-anchor)
    (w3m-next-image)
    (w3m-previous-image)
    (w3m-toggle-inline-image)
    (w3m-toggle-inline-images)
    (w3m-view-this-url . w3m-safe-view-this-url)
    (w3m-mouse-view-this-url . w3m-mouse-safe-view-this-url))
  "Alist of commands use emacs-w3m in article buffers.
Each element looks like (FROM-COMMAND . TO-COMMAND); Those keys which
are defined as FROM-COMMAND in `w3m-mode-map' are redefined as
TO-COMMAND in `w3m-minor-mode-map'.  When TO-COMMAND is nil,
FROM-COMMAND is defined to `w3m-minor-mode-map' for same keys in
`w3m-mode-map'.")

(defun w3m-make-minor-mode-keymap ()
  "Make keymap for `w3m-minor-mode'."
  (let ((keymap (make-sparse-keymap)))
    (dolist (pair w3m-minor-mode-command-alist)
      (substitute-key-definition (car pair)
				 (or (cdr pair) (car pair))
				 keymap w3m-mode-map))
    (unless (featurep 'xemacs)
      ;; Inhibit the `widget-button-click' command when
      ;; `w3m-imitate-widget-button' is activated.
      (define-key keymap [down-mouse-2] 'undefined))
    keymap))

(defvar w3m-minor-mode-map (w3m-make-minor-mode-keymap)
  "*Keymap for `w3m-minor-mode'.")

(defcustom w3m-minor-mode-hook nil
  "*Hook run at the end of function `w3m-minor-mode'."
  :group 'w3m
  :type 'hook)

(defvar w3m-minor-mode nil "Non-nil if w3m minor mode is enabled.")
(make-variable-buffer-local 'w3m-minor-mode)
(unless (assq 'w3m-minor-mode minor-mode-alist)
  (push (list 'w3m-minor-mode " w3m") minor-mode-alist))
(unless (assq 'w3m-minor-mode minor-mode-map-alist)
  (push (cons 'w3m-minor-mode w3m-minor-mode-map) minor-mode-map-alist))

(defun w3m-minor-mode (&optional arg)
  "Minor mode to view text/html part in articles."
  (interactive)
  (prog1 (setq w3m-minor-mode
	       (if arg
		   (> (prefix-numeric-value arg) 0)
		 (not w3m-minor-mode)))
    (run-hooks 'w3m-minor-mode-hook)))


(provide 'w3m)

(unless noninteractive
  (if (string-match "\\.el\\'" w3m-init-file)
      (or (load (concat w3m-init-file "c") t t t)
	  (load w3m-init-file t t t))
    (load w3m-init-file t t))
  (run-hooks 'w3m-load-hook))

;;; w3m.el ends here

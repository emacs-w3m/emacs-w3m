;;; -*- mode: Emacs-Lisp; coding: euc-japan -*-

;; Copyright (C) 2000 TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>,
;;          Shun-ichi GOTO     <gotoh@taiyo.co.jp>,
;;          Satoru Takabayashi <satoru-t@is.aist-nara.ac.jp>
;;          Hideyuki SHIRAI    <shirai@meadowy.org>
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

(defcustom w3m-command-arguments '("-e" "-halfdump" "-cols" col url)
  "*Arguments of w3m."
  :group 'w3m
  :type '(repeat (restricted-sexp :match-alternatives (stringp 'col 'url))))

(defcustom w3m-viewer-command "xv"
  "*Name of the viewer."
  :group 'w3m
  :type 'string)

(defcustom w3m-viewer-command-arguments '(file)
  "Arguments of viewer."
  :group 'w3m
  :type '(repeat (restricted-sexp :match-alternatives (stringp 'file))))

(defcustom w3m-browser-command "netscape"
  "*Command name or function name of the browser."
  :group 'w3m
  :type '(choice (string :tag "Name of command")
		 (function :tag "Name of function")))

(defcustom w3m-browser-command-arguments '(url)
  "*Arguments of browser."
  :group 'w3m
  :type '(repeat (restricted-sexp :match-alternatives (stringp 'url))))

(defcustom w3m-mailto-url-function nil
  "*Mailto handling Function."
  :group 'w3m
  :type 'function)

(defcustom w3m-use-cygdrive t
  "*If non-nil, use /cygdrive/ rule when expand-file-name."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-default-save-dir "~/.w3m"
  "*Default directory for save file."
  :group 'w3m
  :type 'directory)

(defcustom w3m-coding-system (if (boundp 'MULE) '*euc-japan* 'euc-japan)
  "*Coding system for w3m."
  :group 'w3m
  :type 'symbol)

(defcustom w3m-bookmark-file (expand-file-name "~/.w3m/bookmark.html")
  "*Bookmark file of w3m."
  :group 'w3m
  :type 'file)

(defcustom w3m-arrived-list-file (expand-file-name "~/.w3m/.arrived")
  "*Arrived file of w3m."
  :group 'w3m
  :type 'file)

(defcustom w3m-arrived-list-keep 500
  "*Arrived keep count of w3m."
  :group 'w3m
  :type 'integer)

(defcustom w3m-fill-column (- (frame-width) 4)
  "*Fill column of w3m."
  :group 'w3m
  :type 'integer)

(defface w3m-anchor-face
  '((((class color) (background light)) (:foreground "red" :underline t))
    (((class color) (background dark)) (:foreground "blue" :underline t))
    (t (:underline t)))
  "*Face to fontify anchors."
  :group 'w3m-face)

(defface w3m-arrived-anchor-face
  '((((class color) (background light)) (:foreground "navy" :underline t :bold t))
    (((class color) (background dark)) (:foreground "blue" :underline t :bold t))
    (t (:underline t)))
  "*Face to fontify anchors, if arrived."
  :group 'w3m-face)

(defface w3m-image-face
  '((((class color) (background light)) (:foreground "ForestGreen"))
    (((class color) (background dark)) (:foreground "PaleGreen"))
    (t (:underline t)))
  "*Face to fontify image alternate strings."
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

(defcustom w3m-exec-process-type 'call-process
  "*Function type for w3m execution."
  :group 'w3m
  :type '(choice (symbol :tag "call-process" call-process)
		 (symbol :tag "start-process" start-process)))

(defcustom w3m-process-connection-type t
  "*Process connection type for w3m."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-keep-backlog 300
  "*Back log size of w3m."
  :group 'w3m
  :type 'integer)

(defvar w3m-backlog-buffer nil)
(defvar w3m-backlog-articles nil)
(defvar w3m-backlog-hashtb nil)
(defvar w3m-input-url-history nil)

(defvar w3m-arrived-anchor-list nil)
(defvar w3m-current-url nil "URL of this buffer.")
(defvar w3m-current-title nil "Title of this buffer.")
(defvar w3m-url-history nil "History of URL.")

(defun w3m-arrived-list-add (&optional url)
  "Cons url to 'w3m-arrived-anchor-list'. CAR is newest."
  (setq url (or url w3m-current-url))
  (when (> (length url) 5) ;; ignore short
    (set-text-properties 0 (length url) nil url)
    (setq w3m-arrived-anchor-list
	  (cons url (delete url w3m-arrived-anchor-list)))))
	  
(defun w3m-arrived-list-load ()
  "Load arrived url list from 'w3m-arrived-list-file'."
  (when (file-readable-p w3m-arrived-list-file)
    (with-temp-buffer
      (insert-file-contents w3m-arrived-list-file)
      (condition-case nil
	  (read (current-buffer))
	(error nil)))))

(defun w3m-arrived-list-save ()
  "Save arrived url list to 'w3m-arrived-list-file'."
  (when (> (length w3m-arrived-anchor-list) w3m-arrived-list-keep)
    (setq w3m-arrived-anchor-list
	  (nreverse (nthcdr (- (length w3m-arrived-anchor-list)
			       w3m-arrived-list-keep)
			    (nreverse w3m-arrived-anchor-list)))))
  (if (file-writable-p w3m-arrived-list-file)
      (with-temp-buffer
	(prin1 w3m-arrived-anchor-list (current-buffer))
	(princ "\n" (current-buffer))
	(write-region (point-min) (point-max)
		      w3m-arrived-list-file nil 'nomsg)
	(setq w3m-arrived-anchor-list nil))))

(defun w3m-fontify ()
  "Fontify this buffer."
  (let ((case-fold-search t))
    (run-hooks 'w3m-fontify-before-hook)
    ;; Delete extra title tag.
    (let (start)
      (and (search-forward "<title>" nil t)
	   (setq start (match-beginning 0))
	   (search-forward "</title>" nil t)
	   (delete-region start (match-end 0))))
    ;; Fontify bold characters.
    (goto-char (point-min))
    (while (search-forward "<b>" nil t)
      (let ((start (match-beginning 0)))
	(delete-region start (match-end 0))
	(when (search-forward "</b>" nil t)
	  (delete-region (match-beginning 0) (match-end 0))
	  (put-text-property start (match-beginning 0) 'face 'bold))))
    ;; Delete excessive `hseq' elements of anchor tags.
    (goto-char (point-min))
    (while (re-search-forward "<a\\( hseq=\"[-0-9]+\"\\)" nil t)
      (delete-region (match-beginning 1) (match-end 1)))
    ;; Re-ordering anchor elements.
    (goto-char (point-min))
    (let (href)
      (while (re-search-forward "<a\\([ \t\n]\\)[^>]+[ \t\n]href=\\(\"[^\"]*\"\\)" nil t)
	(setq href (buffer-substring (match-beginning 2) (match-end 2)))
	(delete-region (match-beginning 2) (match-end 2))
	(goto-char (match-beginning 1))
	(insert " href=" href)))
    ;; Fontify anchor tags.
    (goto-char (point-min))
    (while (re-search-forward
	    "<a\\([ \t\n]+href=\"\\([^\"]*\\)\"\\)?\\([ \t\n]+name=\"\\([^\"]*\\)\"\\)?[^>]*>"
	    nil t)
      (let ((url (match-string 2))
	    (tag (match-string 4))
	    (start (match-beginning 0))
	    (end))
	(delete-region start (match-end 0))
	(cond (url
	       (when (search-forward "</a>" nil t)
		 (delete-region (setq end (match-beginning 0)) (match-end 0))
		 (if (member (w3m-expand-url url w3m-current-url)
			     w3m-arrived-anchor-list)
		     (put-text-property start end 'face 'w3m-arrived-anchor-face)
		   (put-text-property start end 'face 'w3m-anchor-face))
		 (put-text-property start end 'w3m-href-anchor url))
	       (when tag
		 (put-text-property start end 'w3m-name-anchor tag)))
	      (tag
	       (when (re-search-forward "<\\|\n" nil t)
		 (setq end (match-beginning 0))
		 (put-text-property start end 'w3m-name-anchor tag))))))
    ;; Fontify image alternate strings.
    (goto-char (point-min))
    (while (re-search-forward "<img_alt src=\"\\([^\"]*\\)\">" nil t)
      (let ((src (match-string 1))
	    (start (match-beginning 0))
	    (end))
	(delete-region start (match-end 0))
	(when (search-forward "</img_alt>" nil t)
	  (delete-region (setq end (match-beginning 0)) (match-end 0))
	  (put-text-property start end 'face 'w3m-image-face)
	  (put-text-property start end 'w3m-image src))))
    ;; Remove other markups.
    (goto-char (point-min))
    (while (re-search-forward "</?[A-z][^>]*>" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    ;; Decode escaped characters.
    (goto-char (point-min))
    (let (prop)
      (while (re-search-forward
	      "&\\(\\(nbsp\\)\\|\\(gt\\)\\|\\(lt\\)\\|\\(amp\\)\\|\\(quot\\)\\|\\(apos\\)\\);"
	      nil t)
	(setq prop (text-properties-at (match-beginning 0)))
	(delete-region (match-beginning 0) (match-end 0))
	(insert (if (match-beginning 2) " "
		  (if (match-beginning 3) ">"
		    (if (match-beginning 4) "<"
		      (if (match-beginning 5) "&"
			(if (match-beginning 6) "\"" "'"))))))
	(if prop (add-text-properties (1- (point)) (point) prop))))
    (run-hooks 'w3m-fontify-after-hook)))


(defun w3m-input-url (&optional prompt default)
  "Read a URL from the minibuffer, prompting with string PROMPT."
  (let (url candidates)
    (w3m-backlog-setup)
    (or w3m-input-url-history
	(setq w3m-input-url-history (w3m-arrived-list-load)))
    (mapatoms (lambda (x)
		(setq candidates (cons (cons (symbol-name x) x) candidates)))
	      w3m-backlog-hashtb)
    (setq url (completing-read (or prompt "URL: ")
			       candidates nil nil
			       default 'w3m-input-url-history default))
    ;; remove duplication
    (setq w3m-input-url-history (cons url (delete url w3m-input-url-history)))
    ;; return value
    url))



(defun w3m-backlog-setup ()
  "Initialize backlog variables."
  (unless (and (bufferp w3m-backlog-buffer)
	       (buffer-live-p w3m-backlog-buffer))
    (save-excursion
      (set-buffer (get-buffer-create " *w3m backlog*"))
      (buffer-disable-undo)
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

(defun w3m-backlog-enter (url buffer)
  (w3m-backlog-setup)
  (let ((ident (intern url w3m-backlog-hashtb)))
    (if (memq ident w3m-backlog-articles)
	()				; It's already kept.
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
	      (setq w3m-backlog-articles (cons ident w3m-backlog-articles)))
	    ))))))

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
  "Remove data of URL from the backlog."
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
  (w3m-backlog-setup)
  (let ((ident (intern url w3m-backlog-hashtb)))
    (when (memq ident w3m-backlog-articles)
      ;; It was in the backlog.
      (let (beg end)
	(save-excursion
	  (set-buffer w3m-backlog-buffer)
	  (if (not (setq beg (text-property-any
			      (point-min) (point-max) 'w3m-backlog ident)))
	      ;; It wasn't in the backlog after all.
	      (setq w3m-backlog-articles (delq ident w3m-backlog-articles))
	    ;; Find the end (i. e., the beginning of the next article).
	    (setq end
		  (next-single-property-change
		   (1+ beg) 'w3m-backlog (current-buffer) (point-max)))))
	(and beg
	     end
	     (save-excursion
	       (and buffer (set-buffer buffer))
	       (let (buffer-read-only)
		 (insert-buffer-substring w3m-backlog-buffer beg end))
	       t))))))

(defvar w3m-process nil)
(defvar w3m-process-string nil)
(defvar w3m-process-url nil)
(make-variable-buffer-local 'w3m-process)
(make-variable-buffer-local 'w3m-process-string)
(make-variable-buffer-local 'w3m-process-url)

(defun w3m-exec (url &optional buffer)
  "Download URL with w3m to the BUFFER.
If BUFFER is nil, all data is placed to the current buffer."
  (save-excursion
    (if buffer (set-buffer buffer))
    (delete-region (point-min) (point-max))
    (or (w3m-backlog-request url) (w3m-exec-process url))
    (w3m-backlog-enter url (current-buffer))
    ;; Setting buffer local variables.
    (set (make-local-variable 'w3m-current-url) url)
    (goto-char (point-min))
    (let (title)
      (mapcar (lambda (regexp)
		(goto-char 1)
		(when (re-search-forward regexp nil t)
		  (setq title (match-string 1))
		  (delete-region (match-beginning 0) (match-end 0))))
	      '("<title_alt[ \t\n]+title=\"\\([^\"]+\\)\">"
		"<title>\\([^<]\\)</title>"))
      (if (and (null title)
	       (< 0 (length (file-name-nondirectory url))))
	  (setq title (file-name-nondirectory url)))
      (set (make-local-variable 'w3m-current-title) (or title "<no-title>")))
    (set (make-local-variable 'w3m-url-history)
	 (cons url w3m-url-history))
    (setq-default w3m-url-history
		  (cons url (default-value 'w3m-url-history)))))

(defun w3m-exec-process (url)
  (save-excursion
    (let ((coding-system-for-read w3m-coding-system)
	  (coding-system-for-write w3m-coding-system)
	  (process-connection-type w3m-process-connection-type)
	  (args (copy-sequence w3m-command-arguments)))
      (cond
       ((string-match "\\.s?html?$\\|/$" url)) ; do nothing
       ((string-match "\\.\\(txt\\|el\\)$" url)
	(setq args (cons "-dump" (delete "-halfdump" args))))
       ;; (t
       ;;  (error "Unsupported suffix to retrieve.")))
       )
      (message "Loading page...")
      (if (eq w3m-exec-process-type 'start-process)
	  ;; start-process
	  (unwind-protect nil
	    (let ()
	      ;; (pop-to-buffer (current-buffer))
	      (setq w3m-process-url url)
	      (setq w3m-process-string nil)
	      (setq buffer-read-only t)
	      (setq w3m-process
		    (apply 'start-process w3m-command (current-buffer) w3m-command
			   (mapcar (lambda (arg)
				     (if (eq arg 'col)
					 (format "%d" w3m-fill-column)
				       (eval arg)))
				   args)))
	      (set-process-coding-system w3m-process w3m-coding-system)
	      (set-process-filter w3m-process 'w3m-exec-filter)
	      (set-process-sentinel w3m-process 'w3m-exec-sentinel)
	      (while w3m-process
		(sit-for 0.5)
		(discard-input)))
	    (setq w3m-process nil)
	    (setq w3m-process-url url)
	    (setq w3m-process-string nil)
	    (setq buffer-read-only nil))
	;; call-process
	(apply 'call-process w3m-command nil t nil
	       (mapcar (lambda (arg)
			 (if (eq arg 'col)
			     (format "%d" w3m-fill-column)
			   (eval arg)))
		       args)))
      (message "Loading page... done."))))

(defun w3m-exec-filter (process string)
  (if (bufferp (process-buffer process))
      (let ((obuf (buffer-name)))
	(unwind-protect
	    (progn
	      (set-buffer (process-buffer process))
	      (let ((buffer-read-only nil)
		    (case-fold-search nil)
		    file input prompt)
		(goto-char (point-max))
		(setq w3m-process-string
		      (concat w3m-process-string string))
		(while (string-match "\n" w3m-process-string)
		  (insert (concat
			   (substring w3m-process-string 0 (match-beginning 0))
			   "\n"))
		  (setq w3m-process-string
			(substring w3m-process-string (match-end 0))))
		(cond
		 ;; username
		 ((string-match "^Username: " w3m-process-string)
		  (setq prompt (match-string 0 w3m-process-string))
		  (setq w3m-process-string "")
		  (setq input (read-from-minibuffer prompt))
		  (process-send-string process (concat input "\n")))
		 ;; passwd
		 ((string-match "^Password: " w3m-process-string)
		  (setq prompt (match-string 0 w3m-process-string))
		  (setq w3m-process-string "")
		  (setq input (w3m-read-passwd prompt))
		  (process-send-string process (concat input "\n")))
		 ;; save file
		 ((string-match "Save file to:" w3m-process-string)
		  (setq w3m-process-string "")
		  (setq file (if w3m-process-url
				 (file-name-nondirectory w3m-process-url)))
		  (setq prompt (if file (format "Save filt to (%s): " file)
				 "Save file to: "))
		  (setq input (w3m-read-file-name prompt nil file))
		  (process-send-string process (concat input "\n"))
		  (insert (format "Save to %s.\n" input)))
		 ;; overwrite
		 ((string-match "File exists. Overwrite? (y or n)" w3m-process-string)
		  (setq w3m-process-string "")
		  (condition-case nil
		      (process-send-string process "y\n")
		    (error nil)))
		 ;; quit
		 ((string-match " *Hit any key to quit w3m:" w3m-process-string)
		  (condition-case nil
		      (quit-process process)
		    (error nil))))))
	  (message "Loading page...")
	  (if (get-buffer obuf)
	      (set-buffer obuf))))))

(defun w3m-exec-sentinel (process event)
  (setq w3m-process-string nil)
  (setq w3m-process nil))

(defun w3m-read-file-name (&optional prompt directory default existing initial)
  (let* ((prompt (or prompt (if default (format "File(%s): " default) "File: ")))
	 (directory (file-name-as-directory (or directory w3m-default-save-dir)))
	 (insert-default-directory t)
	 file)
    (setq file (read-file-name prompt directory default existing initial))
    (if (file-directory-p file)
	(progn
	  (setq w3m-default-save-dir file)
	  (if default
	      (setq file (expand-file-name default file))))
      (setq w3m-default-save-dir
	    (or (file-name-directory file) w3m-default-save-dir)))
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


(defun w3m-expand-url (url base)
  "Convert URL to absolute, and canonicalize it."
  (if (string-match "^[^:]+://[^/]*$" base)
      (setq base (concat base "/")))
  (cond
   ;; URL is relative on BASE.
   ((string-match "^#" url)
    (concat base url))
   ;; URL has absolute spec.
   ((string-match "^[^:]+:" url)
    url)
   ((string-match "^/" url)
    (if (string-match "^\\([^:]+://[^/]*\\)/" base)
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
      (concat server path)))))


(defun w3m-view-this-url (arg)
  "*View the URL of the link under point."
  (interactive "P")
  (let ((url (get-text-property (point) 'w3m-href-anchor)))
    (if url (w3m-goto-url (w3m-expand-url url w3m-current-url) arg))))

(defun w3m-mouse-view-this-url (event)
  (interactive "e")
  (mouse-set-point event)
  (call-interactively (function w3m-view-this-url)))

(defun w3m-view-image ()
  "*View the image under point."
  (interactive)
  (let ((file (get-text-property (point) 'w3m-image)))
    (if file
	(let ((buffer (get-buffer-create " *w3m-view*")))
	  (apply 'start-process
		 "w3m-view"
		 buffer
		 w3m-viewer-command
		 (mapcar 'eval w3m-viewer-command-arguments)))
      (message "NO file."))))


(defun w3m-save-image ()
  "*Save the image under point to a file."
  (interactive)
  (let ((file (get-text-property (point) 'w3m-image)))
    (message "Please implement this function !!")))


(defun w3m-view-current-url-with-external-browser ()
  "*View this URL."
  (interactive)
  (let ((buffer (get-buffer-create " *w3m-view*"))
	(url (w3m-expand-url (get-text-property (point) 'w3m-href-anchor)
			     w3m-current-url)))
    (if (and (symbolp w3m-browser-command)
	     (fboundp w3m-browser-command))
	(funcall w3m-browser-command url)
      (apply 'start-process
	     "w3m-external-browser"
	     buffer
	     w3m-browser-command
	     (mapcar (function eval)
		     w3m-browser-command-arguments)))))


(defun w3m-download-this-url ()
  "*Download the URL of the link under point to a file."
  (interactive)
  (message "Please implement this function !!"))

(defun w3m-print-current-url ()
  "*Print the URL of current page and push it into kill-ring."
  (interactive)
  (kill-new w3m-current-url)
  (message "%s" w3m-current-url))

(defun w3m-print-this-url ()
  "*Print the URL of the link under point."
  (interactive)
  (let ((url (get-text-property (point) 'w3m-href-anchor)))
    (if url
	(kill-new (setq url (w3m-expand-url url w3m-current-url))))
    (message "%s" (or url "Not found."))))


(defun w3m-next-anchor (&optional arg)
  "*Move cursor to the next anchor."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      ;; If ARG is negative.
      (w3m-previous-anchor (- arg))
    (when (get-text-property (point) 'w3m-href-anchor)
      (goto-char (next-single-property-change (point) 'w3m-href-anchor)))
    (while (and
	    (> arg 0)
	    (setq pos (next-single-property-change (point) 'w3m-href-anchor)))
      (goto-char pos)
      (unless (zerop (setq arg (1- arg)))
	(goto-char (next-single-property-change (point) 'w3m-href-anchor))))))


(defun w3m-previous-anchor (&optional arg)
  "Move cursor to the previous anchor."
  (interactive "p")
  (unless arg (setq arg 1))
  (if (< arg 0)
      ;; If ARG is negative.
      (w3m-next-anchor (- arg))
    (when (get-text-property (point) 'w3m-href-anchor)
      (goto-char (previous-single-property-change (1+ (point)) 'w3m-href-anchor)))
    (while (and
	    (> arg 0)
	    (setq pos (previous-single-property-change (point) 'w3m-href-anchor)))
      (goto-char (previous-single-property-change pos 'w3m-href-anchor))
      (setq arg (1- arg)))))


(defun w3m-expand-file-name (file)
  (setq file (expand-file-name file))
  (if (string-match "^\\(.\\):\\(.*\\)" file)
      (if w3m-use-cygdrive
	  (concat "/cygdrive/" (match-string 1 file) (match-string 2 file))
	(concat "file://" (match-string 1 file) (match-string 2 file)))
    file))


(defun w3m-view-bookmark ()
  (interactive)
  (if (file-readable-p w3m-bookmark-file)
      (w3m-goto-url (w3m-expand-file-name w3m-bookmark-file))))


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
  (setq w3m-mode-map (make-keymap))
  (define-key w3m-mode-map " " 'scroll-up)
  (define-key w3m-mode-map "b" 'scroll-down)
  (define-key w3m-mode-map [backspace] 'scroll-down)
  (define-key w3m-mode-map [delete] 'scroll-down)
  (define-key w3m-mode-map "h" 'backward-char)
  (define-key w3m-mode-map "j" 'next-line)
  (define-key w3m-mode-map "k" 'previous-line)
  (define-key w3m-mode-map "l" 'forward-char)
  (define-key w3m-mode-map "J" (lambda () (interactive) (scroll-up 1)))
  (define-key w3m-mode-map "K" (lambda () (interactive) (scroll-up -1)))
  (define-key w3m-mode-map "G" 'goto-line)
  (define-key w3m-mode-map "\C-?" 'scroll-down)
  (define-key w3m-mode-map "\t" 'w3m-next-anchor)
  (define-key w3m-mode-map [down] 'w3m-next-anchor)
  (define-key w3m-mode-map "\M-\t" 'w3m-previous-anchor)
  (define-key w3m-mode-map [up] 'w3m-previous-anchor)
  (define-key w3m-mode-map "\C-m" 'w3m-view-this-url)
  (define-key w3m-mode-map [right] 'w3m-view-this-url)
  (if (featurep 'xemacs)
      (define-key w3m-mode-map [(button2)] 'w3m-mouse-view-this-url)
    (define-key w3m-mode-map [mouse-2] 'w3m-mouse-view-this-url))
  (define-key w3m-mode-map [left] 'w3m-view-previous-page)
  (define-key w3m-mode-map "B" 'w3m-view-previous-page)
  (define-key w3m-mode-map "d" 'w3m-download-this-url)
  (define-key w3m-mode-map "u" 'w3m-print-this-url)
  (define-key w3m-mode-map "I" 'w3m-view-image)
  (define-key w3m-mode-map "\M-I" 'w3m-save-image)
  (define-key w3m-mode-map "c" 'w3m-print-current-url)
  (define-key w3m-mode-map "M" 'w3m-view-current-url-with-external-browser)
  (define-key w3m-mode-map "g" 'w3m)
  (define-key w3m-mode-map "U" 'w3m)
  (define-key w3m-mode-map "V" 'w3m)
  (define-key w3m-mode-map "v" 'w3m-view-bookmark)
  (define-key w3m-mode-map "q" 'w3m-quit)
  (define-key w3m-mode-map "Q" (lambda () (interactive) (w3m-quit t)))
  (define-key w3m-mode-map "\M-n" 'w3m-copy-buffer)
  (define-key w3m-mode-map "R" 'w3m-reload-this-page)
  (define-key w3m-mode-map "?" 'describe-mode)
  )


(defun w3m-quit (&optional force)
  (interactive "P")
  (w3m-arrived-list-save)
  (when (or force
	    (y-or-n-p "Do you want to exit w3m? "))
    (kill-buffer (current-buffer))
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
\\[w3m-next-anchor]	Jump next anchor.
\\[w3m-previous-anchor]	Jump previous anchor.
\\[w3m-view-previous-page]	Back to previous page.

\\[w3m-download-this-url]	Download this url.
\\[w3m-print-this-url]	Print this url.
\\[w3m-view-image]	View image.
\\[w3m-save-image]	Save image.

\\[w3m-print-current-url]	Print current url.
\\[w3m-view-current-url-with-external-browser]	View current url with external browser.

\\[scroll-up]	Scroll up.
\\[scroll-down]	Scroll down.

\\[next-line]	Next line.
\\[previous-line]	Previous line.

\\[forward-char]	Forward char.
\\[backward-char]	Backward char.

\\[goto-line]	Jump to line.

\\[w3m]	w3m.
\\[w3m-view-bookmark]	w3m-view-bookmark.
\\[w3m-copy-buffer]	w3m-copy-buffer.

\\[w3m-quit]	w3m-quit.
\\[describe-mode]	describe-mode.
"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'w3m-mode
	mode-name "w3m")
  (use-local-map w3m-mode-map)
  (run-hooks 'w3m-mode-hook))

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
    (if reload
	(w3m-backlog-remove url))
    (cond
     ;; process mailto: protocol
     ((string-match "^mailto:\\(.*\\)" url)
      (w3m-mailto-url url))
     (t
      (when (string-match "#\\([^#]+\\)$" url)
	(setq name (match-string 1 url)
	      url (substring url 0 (match-beginning 0))))
      (setq buffer-read-only nil)
      (w3m-save-position w3m-current-url)
      (w3m-exec url)
      (or w3m-arrived-anchor-list
	  (setq w3m-arrived-anchor-list (w3m-arrived-list-load)))
      (w3m-arrived-list-add w3m-current-url)
      (w3m-fontify)
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (or (and name (w3m-search-name-anchor name))
	  (goto-char (point-min)))))))


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
	(list "%12b" " / " 'w3m-current-title))
  (if (string= url "")
      (w3m-view-bookmark)
    (w3m-goto-url url))
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
  (w3m (w3m-expand-file-name file)))


(provide 'w3m)
;;; w3m.el ends here.

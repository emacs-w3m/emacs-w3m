;;; w3m-session.el --- Functions to operate session of w3m

;; Copyright (C) 2001, 2002, 2003, 2005, 2006, 2007
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: Hideyuki SHIRAI <shirai@meadowy.org>
;; Keywords: w3m, WWW, hypermedia

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

;; w3m-session.el is the add-on program of emacs-w3m to save and load
;; sessions.   For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/


;;; Code:
(require 'w3m)

(defcustom w3m-session-file
  (expand-file-name ".sessions" w3m-profile-directory)
  "*File name to keep sessions."
  :group 'w3m
  :type '(file :size 0))

(defcustom w3m-session-autosave t
  "*Non-nil means save automatically when w3m quit."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-session-deleted-save t
  "*Non-nil means save deleted sessions."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-session-time-format
  (if (and (equal "Japanese" w3m-language)
	   (not (featurep 'xemacs)))
      "%Y年%m月%d日(%a) %H:%M"
    "%Y-%m-%d (%a) %H:%M")
  "*Format of saved time."
  :group 'w3m
  :type '(string :size 0))

(defcustom w3m-session-automatic-title
  (if (equal "Japanese" w3m-language)
      "自動保存"
    "Automatic saved sessions")
  "*String of title to save session automatically."
  :group 'w3m
  :type '(string :size 0))

(defcustom w3m-session-deleted-title
  (if (equal "Japanese" w3m-language)
      "削除セッション"
    "Removed sessions")
  "*String of title to save session when buffer delete."
  :group 'w3m
  :type '(string :size 0))

(defcustom w3m-session-deleted-keep-number 5
  "*Number to keep sessions when buffers delete."
  :group 'w3m
  :type '(integer :size 0))

(defface w3m-session-select-face
  `((((class color) (type tty))
     (:foreground "black"))
    (((class color) (background light))
     (:foreground "dark blue"))
    (((class color) (background dark))
     (:foreground "white"))
    (t nil))
  "Face of w3m-session."
  :group 'w3m)

(defface w3m-session-selected-face
  `((((class color) (type tty))
     (:foreground "blue"))
    (((class color) (background light))
     (:foreground "dark blue" :bold t :underline t))
    (((class color) (background dark))
     (:foreground "white" :bold t :underline t))
    (t (:bold t :underline t)))
  "Face of selected w3m-session."
  :group 'w3m)

;; format of sessin file.
;; '((title1 time1 (url url url ...) current1)
;;   (title2 time2 (url url url ...) current2)
;;   ...)

;;;###autoload
(defun w3m-session-save ()
  "Save list of displayed session."
  (interactive)
  (let ((sessions (w3m-load-list w3m-session-file))
	(bufs (w3m-list-buffers))
	(prompt "New session title: ")
	(cnum 0)
	(i 0)
	title titles urls len buf cbuf)
    (mapcar (lambda (x)
	      (setq titles (cons (cons (car x) (car x)) titles)))
	    sessions)
    (setq title (or w3m-current-title
		    (save-excursion
		      (set-buffer (car bufs))
		      w3m-current-title)))
    (setq titles (cons (cons title title) titles))
    (catch 'loop
      (while t
	(setq title (completing-read prompt titles nil nil title))
	(if (or (string= title "")
		(and (assoc title sessions)
		     (not (y-or-n-p (format "\"%s\" is exist. Overwrite? "
					    title)))))
	    (setq prompt "Again New session title: ")
	  (throw 'loop t))))
    (setq cbuf (current-buffer))
    (save-excursion
      (while (setq buf (car bufs))
	(setq bufs (cdr bufs))
	(set-buffer buf)
	(when w3m-current-url
	  (when (eq cbuf (current-buffer))
	    (setq cnum i))
	  (setq i (1+ i))
	  (setq urls (cons w3m-current-url urls)))))
    (if (not urls)
	(message "%s: no session save...done" title)
      (setq len (length urls))
      (setq urls (nreverse urls))
      (when (assoc title sessions)
	(setq sessions (delete (assoc title sessions) sessions)))
      (setq sessions (cons (list title (current-time) urls cnum) sessions))
      (w3m-save-list w3m-session-file sessions)
      (if (= len 1)
	  (message "%s: 1 session save...done" title)
	(message "%s: %d sessions save...done" title len)))))

(defun w3m-session-automatic-save ()
  "Save list of displayed session automatically."
  (when w3m-session-autosave
    (let ((sessions (w3m-load-list w3m-session-file))
	  (bufs (w3m-list-buffers))
	  (title w3m-session-automatic-title)
	  (cnum 0)
	  (i 0)
	  urls buf cbuf)
      (when bufs
	(setq cbuf (current-buffer))
	(save-excursion
	  (while (setq buf (car bufs))
	    (setq bufs (cdr bufs))
	    (set-buffer buf)
	    (when w3m-current-url
	      (when (eq cbuf (current-buffer))
		(setq cnum i))
	      (setq i (1+ i))
	      (setq urls (cons w3m-current-url urls)))))
	(when urls
	  (setq urls (nreverse urls))
	  (when (assoc title sessions)
	    (setq sessions (delete (assoc title sessions) sessions)))
	  (setq sessions (cons (list title (current-time) urls cnum) sessions))
	  (w3m-save-list w3m-session-file sessions))))))

(defun w3m-session-deleted-save (buffers)
  "Save list of deleted session."
  (when w3m-session-deleted-save
    (let ((sessions (w3m-load-list w3m-session-file))
	  (title (concat w3m-session-deleted-title "-1"))
	  (titleregex (concat "^"
			      (regexp-quote w3m-session-deleted-title)
			      "-[0-9]+$"))
	  (bufs (copy-sequence buffers))
	  (i 2)
	  urls buf session
	  tmp tmptitle tmptime tmpurls)
      (when bufs
	(setq bufs (sort bufs 'w3m-buffer-name-lessp))
	(save-excursion
	  (while (setq buf (car bufs))
	    (setq bufs (cdr bufs))
	    (set-buffer buf)
	    (when w3m-current-url
	      (setq urls (cons w3m-current-url urls)))))
	(when urls
	  (while (setq session (car sessions))
	    (setq sessions (cdr sessions))
	    (if (string-match titleregex (nth 0 session))
		(when (<= i w3m-session-deleted-keep-number)
		  (setq tmptitle (format (concat w3m-session-deleted-title "-%d") i))
		  (setq tmptime (nth 1 session))
		  (setq tmpurls (nth 2 session))
		  (setq tmp (cons (list tmptitle tmptime tmpurls nil) tmp))
		  (setq i (1+ i)))
	      (setq tmp (cons session tmp))))
	  (setq sessions (nreverse tmp))
	  (setq urls (nreverse urls))
	  (setq sessions (cons (list title (current-time) urls nil) sessions))
	  (w3m-save-list w3m-session-file sessions))))))

;;;###autoload
(defun w3m-session-select ()
  "Select session from session list."
  (interactive)
  (let* ((sessions (w3m-load-list w3m-session-file))
	 (showbuf (get-buffer-create " *w3m-session select*"))
	 (wheight (max (+ (length sessions) 5) window-min-height))
	 (minimsg "Select Session(return), (S)ave, (D)elete or (Q)uit" )
	 (num 0)
	 (max 0)
	 c title titles time times url urls wid
	 window last-window num-or-sym pos)
    (if (not sessions)
	(message "No saved session")
      (mapcar (lambda (x)
		(setq title (format "%s[%d]" (nth 0 x) (length (nth 2 x))))
		(setq wid (string-width title))
		(when (> wid max)
		  (setq max wid))
		(setq titles (cons title titles))
		(setq times (cons (format-time-string w3m-session-time-format
						      (nth 1 x))
				  times))
		(setq urls (cons (mapconcat 'identity (nth 2 x) ", ")
				 urls)))
	      sessions)
      (setq titles (nreverse titles))
      (setq times (nreverse times))
      (setq urls (nreverse urls))
      (setq max (+ max 2))
      (unwind-protect
	  (save-window-excursion
	    (setq last-window (previous-window
			       (w3m-static-if (fboundp 'frame-highest-window)
				   (frame-highest-window)
				 (frame-first-window))))
	    (while (minibuffer-window-active-p last-window)
	      (setq last-window (previous-window last-window)))
	    (while (and
		    (not (one-window-p))
		    (or (< (window-width last-window)
			   (frame-width))
			(< (window-height last-window)
			   (+ wheight window-min-height))))
	      (setq window last-window)
	      (setq last-window (previous-window window))
	      (delete-window window))
	    (select-window (split-window last-window))
	    (condition-case nil
		(shrink-window (- (window-height) wheight))
	      (error nil))
	    (switch-to-buffer showbuf)
	    (setq buffer-read-only nil)
	    (setq truncate-lines t)
	    (erase-buffer)
	    (shrink-window (- (window-height) wheight))
	    (insert "Select session:\n\n")
	    (while (and (setq title (car titles))
			(setq time (car times))
			(setq url (car urls)))
	      (setq titles (cdr titles))
	      (setq times (cdr times))
	      (setq urls (cdr urls))
	      (setq pos (point))
	      (insert title)
	      (add-text-properties pos (point)
				   `(face w3m-session-select-face
					  w3m-session-number ,num))
	      (setq num (1+ num))
	      (insert (make-string (- max (string-width title)) ?\ ))
	      (insert time "  " url "\n"))
	    (goto-char (point-min))
	    (goto-char (next-single-property-change
			(point) 'w3m-session-number))
	    (put-text-property (point)
			       (next-single-property-change
				(point) 'w3m-session-number)
			       'face 'w3m-session-selected-face)
	    (while (null c)
	      (set-buffer-modified-p nil)
	      (setq c (w3m-static-cond
		       ((and (fboundp 'next-command-event)
			     (fboundp 'event-key))
			(event-key (next-command-event nil minimsg)))
		       ((fboundp 'read-event)
			(read-event minimsg))
		       (t
			(message minimsg)
			(read-char-exclusive))))
	      (cond
	       ((memq c '(?q ?Q ?  space))
		(setq num-or-sym 'exit))
	       ((memq c '(?\C-m ?m return))
		(beginning-of-line)
		(setq num-or-sym (get-text-property
				  (point) 'w3m-session-number)))
	       ((and (memq c '(?d ?D))
		     (y-or-n-p "Delete this session? "))
		(beginning-of-line)
		(setq num-or-sym (cons 'delete
				       (get-text-property
					(point) 'w3m-session-number))))
	       ((and (memq c '(?s ?S))
		     (y-or-n-p "Save this sessions? "))
		(setq num-or-sym 'save))
	       ((memq c '(?\C-n ?n ?j down))
		(setq c nil)
		(beginning-of-line)
		(put-text-property (point)
				   (next-single-property-change
				    (point) 'w3m-session-number)
				   'face 'w3m-session-select-face)
		(forward-line)
		(unless (get-text-property (point) 'w3m-session-number)
		  (goto-char (next-single-property-change
			      (point-min) 'w3m-session-number)))
		(put-text-property (point)
				   (next-single-property-change
				    (point) 'w3m-session-number)
				   'face 'w3m-session-selected-face))
	       ((memq c '(?\C-p ?p ?k up))
		(setq c nil)
		(put-text-property (point)
				   (next-single-property-change
				    (point) 'w3m-session-number)
				   'face 'w3m-session-select-face)
		(forward-line -1)
		(beginning-of-line)
		(unless (get-text-property (point) 'w3m-session-number)
		  (goto-char (point-max))
		  (goto-char (previous-single-property-change
			      (point) 'w3m-session-number))
		  (beginning-of-line))
		(put-text-property (point)
				   (next-single-property-change
				    (point) 'w3m-session-number)
				   'face 'w3m-session-selected-face))
	       (t
		(setq c nil)
		(unless (string-match "retry$" minimsg)
		  (setq minimsg (concat minimsg ", retry"))))))
	    (message nil))
	(kill-buffer showbuf))
      (cond
       ((numberp num-or-sym)
	(w3m-session-goto-session (nth num-or-sym sessions)))
       ((eq num-or-sym 'save)
	(w3m-session-save)
	(w3m-session-select))
       ((and (consp num-or-sym)
	     (eq 'delete (car num-or-sym)))
	(w3m-session-delete sessions (cdr num-or-sym))
	(w3m-session-select))))))

(defun w3m-session-goto-session (session)
  "Goto URLs."
  (let ((title (nth 0 session))
	(urls (nth 2 session))
	(cnum (nth 3 session))
	(i 0)
	(w3m-async-exec (and w3m-async-exec-with-many-urls w3m-async-exec))
	url cbuf)
    (message "Session goto(%s)..." title)
    (while (setq url (car urls))
      (setq urls (cdr urls))
      (w3m-goto-url-new-session url)
      (when (and (numberp cnum) (= cnum i))
	(setq cbuf (car (nreverse (w3m-list-buffers)))))
      (setq i (1+ i)))
    (when (and cbuf (eq major-mode 'w3m-mode))
      (set-window-buffer (selected-window) cbuf))
    (message "Session goto(%s)...done" title)))

(defun w3m-session-delete (sessions num)
  (let ((tmp (nth num sessions)))
    (setq sessions (delete tmp sessions))
    (if sessions
	(w3m-save-list w3m-session-file sessions)
      (let ((file (expand-file-name w3m-session-file)))
	(when (and (file-exists-p file)
		   (file-writable-p file))
	  (delete-file file))))))

(provide 'w3m-session)
;;; w3m-session.el ends here

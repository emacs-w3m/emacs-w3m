;;; w3m-tabmenu.el --- Functions for TAB menu browsing

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: Hideyuki SHIRAI    <shirai@meadowy.org>,
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

;; This file contains the functions for TAB browsing.  For more detail
;; about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/



;;; Code:

(defun w3m-setup-tab-menu ()
  "Define TAB menubar buttons for FSF Emacsen."
  (unless (lookup-key w3m-mode-map [menu-bar w3m-tab])
    (define-key-after
      (lookup-key w3m-mode-map [menu-bar])
      [w3m-tab]
      (cons "TAB" (cons 'keymap (w3m-tab-menubar-make-items))) t)
    (add-hook 'menu-bar-update-hook 'w3m-tab-menubar-update)))

(defun w3m-tab-menubar-open-buffer ()
  "Open w3m buffer from tab menubar."
  (interactive)
  (switch-to-buffer last-command-event))

(defun w3m-tab-menubar-update ()
  "Update w3m tab menubar."
  (when (eq major-mode 'w3m-mode)
    (define-key w3m-mode-map [menu-bar w3m-tab]
      (cons "TAB" (cons 'keymap (w3m-tab-menubar-make-items))))))

(defsubst w3m-tab-menubar-pull-bufnum (bufname)
  (cond
   ((string= "*w3m*" bufname) 1)
   ((string-match "\\*w3m\\*<\\([0-9]+\\)>" bufname)
    (string-to-number (match-string 1 bufname)))
   (t 100)))

(defun w3m-tab-menubar-make-items (&optional nomenu)
  "Create w3m tab menu items."
  (let ((cbuf (current-buffer))
	menus bufs title item)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(when (eq major-mode 'w3m-mode)
	  (setq title (cond
		       ((and (stringp w3m-current-title)
			     (not (string= w3m-current-title "<no-title>")))
			w3m-current-title)
		       ((stringp w3m-current-url)
			(directory-file-name
			 (if (string-match "^[^/:]+:/+" w3m-current-url)
			     (substring w3m-current-url (match-end 0))
			   w3m-current-url)))
		       (t "No title")))
	  (setq bufs (cons (list (buffer-name) title (eq cbuf buf)) bufs)))))
    (setq bufs
	  (sort bufs (lambda (x y)
		       (< (w3m-tab-menubar-pull-bufnum (car x))
			  (w3m-tab-menubar-pull-bufnum (car y))))))
    (dolist (elem bufs)
      (setq item (nconc (list (nth 0 elem)
			      (format "%s%s"
				      (if nomenu
					  (if (nth 2 elem) "* " "")
					(if (nth 2 elem) "* " "  "))
				      (nth 1 elem))
			      (cons nil nil))
			'w3m-tab-menubar-open-buffer))
      (setq menus (cons item menus)))
    (nreverse menus)))

(defun w3m-switch-buffer ()
  "Switch `w3m-mode' buffer in the current window."
  (interactive)
  (let ((items (w3m-tab-menubar-make-items 'nomenu))
	(count 1)
	(form "%s [%s]")
	comp hist histlen default buf)
    (dolist (item items)
      (when (string-match "^\\* " (nth 1 item))	;; current-buffer
	(setq default count))
      (setq comp (cons
		  (cons
		   (format form (nth 1 item) (nth 0 item)) (nth 0 item))
		  comp))
      (setq hist (cons (format form (nth 1 item) (nth 0 item)) hist))
      (setq count (1+ count)))
    (setq comp (nreverse comp))
    (setq histlen (length hist))
    (setq hist (append hist hist hist hist hist)) ;; STARTPOS 3rd hist
    (setq buf
	  (completing-read
	   "Switch to w3m buffer: "
	   comp nil t (car (nth (1- default) comp))
	   (cons 'hist (+ (* 3 histlen) (- histlen default -1)))
	   (car (nth (1- default) comp))))
    (setq buf (cdr (assoc buf comp)))
    (when (get-buffer buf)
      (switch-to-buffer buf))))

(provide 'w3m-tabmenu)
;;; w3m-tabmenu.el ends here

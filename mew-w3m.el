;; mew-w3m.el -- View Text/Html content with w3m in Mew

;; Copyright (c) 2001 Shun-ichi Goto.

;; Author: Shun-ichi GOTO  <gotoh@taiyo.co.jp>
;; Created: Wed Feb 28 03:31:00 2001
;; Version: $Revision$
;; Keywords: Mew, mail, w3m, WWW, hypermedia

;;; Commentary:

;; This package is for viewing formatted (rendered) Text/Html content
;; in Mew's message buffer.

;;; Instalation:

;; Simply load this file and add followings in your ~/.mew file.
;;
;; (require 'mew-w3m)
;; (setq mew-prog-html '(mew-mime-text/html-w3m nil nil))
;;
;; And you can use keymap of w3m-mode as mew-w3m-minor-mode.
;; To activate this feaeture, add followings also: 
;;
;; (setq mew-use-w3m-minor-mode t)
;; (add-hook 'mew-message-hook 'mew-w3m-minor-mode-setter)

;;; Usage:

;; There's nothing special. Browse messages in usual way.
;; On viewing Text/Html file, rendered text is appeared in message
;; buffer instead of usual "HTML" banner.
;; C-c C-e operation is also allowed to view with external browser.
;;
;; If mew-use-w3m-minor-mode is t, key operations of w3m-mode is
;; allowed (as minor-mode-map) and jump links in message buffer.
;; NOTE: This feature is not complete. You may confuse.

;;; Code:

(require 'mew)
(require 'w3m)

;;; initializer for mew

(defvar mew-use-w3m-minor-mode nil
  "*Use w3m minor mode in message buffer.
When viewing Text/Html contents rendering with w3m, use `w3m-minor-mode'
and its keymap in message buffer.")

(defvar mew-w3m-auto-insert-image t
  "*If t, an image inserts automatic in Multipart/Related message.
This variable effected only XEmacs or Emacs 21.")
  
;; these are defined here.
;; It's not reasonable to merge into w3m.el, i think
(defvar mew-w3m-minor-mode nil)
(make-variable-buffer-local 'mew-w3m-minor-mode)
(add-to-list 'minor-mode-alist '(mew-w3m-minor-mode " w3m"))
(add-to-list 'minor-mode-map-alist (cons 'mew-w3m-minor-mode w3m-mode-map))

(defun mew-w3m-minor-mode-setter () 
  "Check message buffer and activate mew-w3m-minor-mode."
  (setq mew-w3m-minor-mode (and (get-text-property (point-min) 'w3m)
				mew-use-w3m-minor-mode)))

;; processing Text/Html contents with w3m.
(defun mew-mime-text/html-w3m (cache begin end &optional params execute)
  "View Text/Html contents with w3m rendering output."
  (if (or execute
	  (<= end begin))
      (mew-mime-text/html cache begin end params execute)
    (mew-elet
     (let ((file (format "%s.html" (mew-make-temp-name)))
	   (w3m-display-inline-image mew-w3m-auto-insert-image)
	   charset wcs)
       (setq charset (mew-syntax-get-param params "charset"))
       (if charset 
	   (setq wcs (mew-charset-to-cs charset))
	 (setq wcs mew-cs-text-for-write))
       (mew-frwlet
	mew-cs-dummy wcs
	(w3m-region (point)
		    (progn (insert-buffer-substring cache begin end) 
			   (point)))
	(put-text-property (point-min) (1+ (point-min)) 'w3m t))))))

(defun mew-w3m-cid-retrieve (url &optional no-decode no-cache)
  (save-excursion
    (when (string-match "^cid:\\(.+\\)" url)
      (setq url (match-string 1 url))
      (let ((fld (mew-current-get-fld (mew-frame-id))))
	(set-buffer fld)
	(let* ((msg (mew-current-get-msg (mew-frame-id)))
	       (part (mew-syntax-nums))
	       (cache (mew-cache-hit fld msg 'must-hit))
	       (syntax (mew-cache-decode-syntax cache))
	       (part2 1)
	       len cid cidpart cidstx beg end)
	  (if (< (length part) 2)
	      (setq part nil)
	    (setcdr (nthcdr (- (length part) 2) part) nil))
	  (setq len
		(- (length
		    (mew-syntax-get-part (mew-syntax-get-entry syntax part)))
		   mew-syntax-magic))
	  (setq cidpart
		(catch 'detcid
		  (while (>= len part2)
		    (setq cid (mew-syntax-get-cid
			       (mew-syntax-get-entry syntax (append part (list part2)))))
		    (when (and cid (string= cid url))
		      (throw 'detcid (append part (list part2))))
		    (setq part2 (1+ part2)))))
	  (when cidpart
	    (setq cidstx (mew-syntax-get-entry syntax cidpart))
	    (setq beg (mew-syntax-get-begin cidstx))
	    (setq end (mew-syntax-get-end cidstx))
	    (w3m-with-work-buffer
	      (delete-region (point-min) (point-max))
	      (set-buffer-multibyte nil)
	      (insert-buffer-substring cache beg end))
	    (car (mew-syntax-get-ct cidstx))))))))

(push (cons 'mew-message-mode 'mew-w3m-cid-retrieve)
      w3m-cid-retrieve-function-alist)

;;;
(provide 'mew-w3m)

;; mew-w3m.el ends here

;;; sb-mhonarc.el --- shimbun backend class for mhonarc

;; Author: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;         Akihiro Arisawa    <ari@mbf.sphere.ne.jp>
;;         Yuuichi Teranishi <teranisi@gohome.org>

;; Keywords: news

;;; Copyright:

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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>.

;;; Code:

(require 'shimbun)

(eval-and-compile
  (luna-define-class shimbun-mhonarc (shimbun)
		     (reverse-flag litemplate-regexp))
  (luna-define-internal-accessors 'shimbun-mhonarc))

(defvar shimbun-mhonarc-litemplate-regexp
  "<strong><a name=\"\\([0-9]+\\)\" href=\"\\(msg[0-9]+.html\\)\">\\([^<]+\\)</a></strong>\n<ul><li><em>From</em>: \\([^<]+\\)</li></ul>")

(luna-define-method initialize-instance :after ((shimbun shimbun-mhonarc)
						&rest init-args)
  (shimbun-mhonarc-set-reverse-flag-internal
   shimbun
   (symbol-value
    (intern-soft (concat "shimbun-" (shimbun-server-internal shimbun)
			 "-reverse-flag"))))
  (shimbun-mhonarc-set-litemplate-regexp-internal
   shimbun
   (symbol-value
    (intern-soft (concat "shimbun-" (shimbun-server-internal shimbun)
			 "-litemplate-regexp"))))
  shimbun)
   
(luna-define-method shimbun-get-headers ((shimbun shimbun-mhonarc))
  (catch 'stop
    (shimbun-mhonarc-get-headers shimbun
				 (shimbun-index-url shimbun))))

(defmacro shimbun-mhonarc-extract-header-values (shimbun url headers aux)
  `(let ((id (format "<%s%s%%%s>"
		     (or ,aux "")
		     (match-string 1)
		     (shimbun-current-group-internal ,shimbun)))
	 (url (shimbun-expand-url (match-string 2) ,url))
	 (subject (subst-char-in-string ?\n ?  (match-string 3)))
	 (from (subst-char-in-string ?\n ?  (match-string 4))))
     (if (shimbun-search-id ,shimbun id)
	 (throw 'stop ,headers)
       (push (shimbun-make-header 0
				  (shimbun-mime-encode-string subject)
				  (shimbun-mime-encode-string from)
				  "" id "" 0 0 url)
	     ,headers))))
  
(defun shimbun-mhonarc-get-headers (shimbun url &optional headers aux)
  (let ((case-fold-search t)
	(regexp (or (shimbun-mhonarc-litemplate-regexp-internal shimbun)
		    shimbun-mhonarc-litemplate-regexp)))
    (if (shimbun-mhonarc-reverse-flag-internal shimbun)
	(progn
	  (goto-char (point-min))
	  (while (re-search-forward regexp nil t)
	    (shimbun-mhonarc-extract-header-values shimbun url headers aux)
	    (forward-line 1)))
      (goto-char (point-max))
      (while (re-search-backward regexp nil t)
	(shimbun-mhonarc-extract-header-values shimbun url headers aux)
	(forward-line 0)))
    headers))

(luna-define-method shimbun-make-contents ((shimbun shimbun-mhonarc)
					   header)
  (if (search-forward "<!--X-Head-End-->" nil t)
      (progn
	(forward-line 0)
	;; Processing headers.
	(save-restriction
	  (narrow-to-region (point-min) (point))
	  (shimbun-decode-entities)
	  (goto-char (point-min))
	  (while (search-forward "\n<!--X-" nil t)
	    (replace-match "\n"))
	  (goto-char (point-min))
	  (while (search-forward " -->\n" nil t)
	    (replace-match "\n"))
	  (goto-char (point-min))
	  (while (search-forward "\t" nil t)
	    (replace-match " "))
	  (goto-char (point-min))
	  (let (buf refs reply-to)
	    (while (not (eobp))
	      (cond
	       ((looking-at "<!--")
		(delete-region (point) (progn (forward-line 1) (point))))
	       ((looking-at "Subject: +")
		(shimbun-header-set-subject header
					    (shimbun-header-field-value))
		(delete-region (point) (progn (forward-line 1) (point))))
	       ((looking-at "From: +")
		(shimbun-header-set-from header (shimbun-header-field-value))
		(delete-region (point) (progn (forward-line 1) (point))))
	       ((looking-at "Date: +")
		(shimbun-header-set-date header (shimbun-header-field-value))
		(delete-region (point) (progn (forward-line 1) (point))))
	       ((looking-at "Message-Id: +")
		(shimbun-header-set-id header
		 (concat "<" (shimbun-header-field-value) ">"))
		(delete-region (point) (progn (forward-line 1) (point))))
	       ((looking-at "Reference: +")
		(push (concat "<" (shimbun-header-field-value) ">") refs)
		(delete-region (point) (progn (forward-line 1) (point))))
	       ((looking-at "Content-Type: ")
		(delete-region (point) (progn (forward-line 1) (point))))
	       (t (forward-line 1))))
	    (if (setq reply-to (shimbun-reply-to shimbun))
		(insert "Reply-To: " reply-to "\n"))
	    (insert "MIME-Version: 1.0\n")
	    (insert "Content-Type: text/html; charset=ISO-2022-JP\n")
	    (if refs
		(shimbun-header-set-references header
					       (mapconcat 'identity refs " ")))
	    (insert "\n")
	    (goto-char (point-min))
	    (shimbun-header-insert shimbun header))
	  (goto-char (point-max)))
	;; Processing body.
	(save-restriction
	  (narrow-to-region (point) (point-max))
	  (delete-region
	   (point)
	   (progn
	     (search-forward "\n<!--X-Body-of-Message-->\n" nil t)
	     (point)))
	  (when (search-forward "\n<!--X-Body-of-Message-End-->\n" nil t)
	    (forward-line -1)
	    (delete-region (point) (point-max)))))
    (goto-char (point-min))
    (shimbun-header-insert shimbun header)
    (insert
     "Content-Type: text/html; charset=ISO-2022-JP\nMIME-Version: 1.0\n\n"))
  (encode-coding-string (buffer-string)
			(mime-charset-to-coding-system "ISO-2022-JP")))

(provide 'sb-mhonarc)

;;; sb-mhonarc.el ends here

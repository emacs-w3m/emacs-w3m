;;; sb-muchy.el --- shimbun backend for Muchy's Palmware Review!

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

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

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-muchy (shimbun) ())

(luna-define-method shimbun-index-url ((shimbun shimbun-muchy))
  (shimbun-url-internal shimbun))

(defvar shimbun-muchy-url "http://muchy.com/")
(defvar shimbun-muchy-groups '("review"))
(defvar shimbun-muchy-from-address "webmaster@muchy.com")
(defvar shimbun-muchy-coding-system 'japanese-shift-jis-unix)

(defsubst shimbun-muchy-parse-time (str)
  (save-match-data
    (if (string-match "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)" str)
	(list (string-to-number (match-string 1 str))
	      (string-to-number (match-string 2 str))
	      (string-to-number (match-string 3 str))))))

;;(luna-define-method shimbun-reply-to ((shimbun shimbun-muchy))
;;  "")

(luna-define-method shimbun-get-headers ((shimbun shimbun-muchy)
					 header &optional outbuf)
  (let* ((count 0)
	 (from (shimbun-from-address-internal shimbun))
	 (group (shimbun-current-group-internal shimbun))
	 (baseurl (shimbun-url-internal shimbun))
	 case-fold-search pos regexp-list headers id url subject date)
    (catch 'stop
      (with-temp-buffer
	(shimbun-retrieve-url baseurl 'reload)
	(subst-char-in-region (point-min) (point-max) ?\t ?  t)
	(goto-char (point-min))
	(or (re-search-forward "<tr>.+alt=\"更新履歴\"></td></tr>" nil t nil)
	    (throw 'stop nil))
	(delete-region (point-min) (point))
	(or (search-forward "<a href=\"whatsold.htm\">[これ以前の更新履歴]</a>")
	    (throw 'stop nil))
	(beginning-of-line)
	(setq pos (point))
	(while (re-search-backward
		"<a href=\"#\\([0-9]+/[0-9]+/[0-9]+\\)\">\\[\\1\\]</a>"
		nil t nil)
	  (setq regexp-list (cons (match-string 1) regexp-list))
	  (delete-region pos (point))
	  (setq pos (point)))
	;;(setq regexp-list (nreverse regexp-list))
	(if (not (search-forward
		  (format "<a name=\"%s\">" (car regexp-list))
		  nil t nil))
	    nil
	  (setq pos (point))
	  (while regexp-list
	    ;; getting DATE
	    (setq date (apply 'shimbun-make-date-string
			      (shimbun-muchy-parse-time (car regexp-list))))
	    ;; getting URL and SUBJECT
	    (while (re-search-forward
		    "<a href=\"\\(.+\\.html\\)\"> *<strong>\\(.+\\)</strong></a>"
		    nil t nil)
	      (setq url (match-string 1)
		    subject (match-string 2))
	      ;; building ID
	      (setq id (format "<%s%08d%%%s>" url
			       (string-to-number
				(mapconcat
				 'identity
				 (mapcar
				  'number-to-string
				  (shimbun-muchy-parse-time (car regexp-list)))
				 ""))
			       group))
	      (if (shimbun-search-id shimbun id)
		  (throw 'stop nil))
	      (setq url (concat baseurl "/" url))
	      (push (shimbun-make-header
		     0 (shimbun-mime-encode-string subject)
		     from date id "" 0 0 url)
		    headers)
	      (forward-line 1))
	    (setq regexp-list (cdr regexp-list))
	    (delete-region pos
			   (if (search-forward
				(format "<a href=\"#%s\">" (car regexp-list))
				nil t nil)
			       (point) (point-max)))))))
    headers))

(luna-define-method shimbun-make-contents ((shimbun shimbun-muchy) header)
  ;; cleaning up
  (let (case-fold-search)
    (if (re-search-forward "</table>" nil t nil)
	(progn
	  (beginning-of-line)
	  (delete-region (point-min) (point))))
    (if (search-forward "<a name=\"webclip\">" nil t nil)
	(delete-region (progn (beginning-of-line) (point))
		       (and (re-search-forward "^$" nil t nil)
			    (forward-line 1) (point))))
    (if (re-search-forward "<!-- *VC layer *-->" nil t nil)
	(progn
	  (beginning-of-line)
	  (delete-region
	   (point)
	   (progn (re-search-forward "<!-- *vc layer *-->" nil t nil)
		  (point)))))
    (if (re-search-forward "<!-- *VC active *-->" nil t nil)
	(progn
	  (beginning-of-line)
	  (delete-region
	   (point)
	   (progn (re-search-forward "<!-- *vc active *-->" nil t nil)
		  (point)))))
    (if (search-backward "記事の内容への質問・フォローは" nil t nil)
	(delete-region (progn (beginning-of-line) (point))
		       (point-max))))
  (goto-char (point-min))
  (subst-char-in-region (point-min) (point-max) ?\t ?  t)
  (shimbun-decode-entities)
  (goto-char (point-min))
  (shimbun-header-insert shimbun header)
  (insert "Content-Type: text/html; charset=ISO-2022-JP\nMIME-Version: 1.0\n\n")
  (encode-coding-string
   (buffer-string) (mime-charset-to-coding-system "ISO-2022-JP")))

(provide 'sb-muchy)
;;; sb-muchy.el ends here

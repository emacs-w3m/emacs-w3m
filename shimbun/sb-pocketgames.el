;;; sb-pocketgames.el --- shimbun backend class for www.pocketgames.jp. -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003, 2004 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Keywords: news
;; Version: $Id$
;; Last Modified: $Date$

;; This file is a part of shimbun.

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

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)

(eval-and-compile
  (luna-define-class shimbun-pocketgames (shimbun) (content-hash))
  (luna-define-internal-accessors 'shimbun-pocketgames))

(defvar shimbun-pocketgames-url "http://www.pocketgames.jp")
(defvar shimbun-pocketgames-groups '("news"))
(defvar shimbun-pocketgames-coding-system 'shift_jis)
(defvar shimbun-pocketgames-content-start
  "<a class=\"pn-normal\" href=\"modules.php\\?op=modload\&amp;name=Search\\&amp;file=index\\&amp;action=search\\&amp;overview=1\\&amp;active_stories=[0-9]+\\&amp;stories_topics\[[0-9]+\]=\"><b>Older Articles</b></a>")
(defvar shimbun-pocketgames-content-end
  "</body>")

(luna-define-method shimbun-index-url ((shimbun shimbun-pocketgames))
  shimbun-pocketgames-url)

(luna-define-method shimbun-reply-to ((shimbun shimbun-pocketgames))
  "Return the mailing list address."
  "info@pocketgames.jp")

(defvar shimbun-pocketgames-expiration-days 14)

(luna-define-method shimbun-headers ((shimbun shimbun-pocketgames)
				     &optional range)
  (shimbun-pocketgames-headers shimbun))

(defun shimbun-pocketgames-headers (shimbun)
  (let (case-fold-search)
    (with-temp-buffer
      (shimbun-retrieve-url (shimbun-index-url shimbun) 'reload 'binary)
      (set-buffer-multibyte t)
      (decode-coding-region (point-min) (point-max)
			    (shimbun-coding-system-internal shimbun))
      (goto-char (point-min))
      (shimbun-pocketgames-headers-1 shimbun))))

(defun shimbun-pocketgames-headers-1 (shimbun)
  (let ((regexp "<a class=\"pn-title\" href=\"\\(modules.php\\?op=modload\\&amp;name=News\\&amp;file=article\\&amp;sid=[0-9]+\\&amp;mode=thread\\&amp;order=0\\)\">\\([^<]+\\)</a></font><br>")
	url from year month day time date subject id start end headers)
    (catch 'quit
      (while (re-search-forward regexp nil t nil)
	(setq url (match-string-no-properties 1)
	      subject (match-string-no-properties 2)
	      start (point)
	      end (set-marker
		   (make-marker)
		   (or (and (re-search-forward regexp nil t nil)
			    (match-beginning 0))
		       (point-max))))
	(goto-char start)
	(unless
	    (re-search-forward
	     "Posted by: \\(.+\\) on \\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\) (\\(月\\|火\\|水\\|木\\|金\\|土\\|日\\))  - \\([0-9][0-9]:[0-9][0-9]\\) JST <\/font>"
	     end t nil)
	  (throw 'quit nil))
	(setq from (shimbun-mime-encode-string (match-string 1))
	      year (string-to-number (match-string 2))
	      month (string-to-number (match-string 3))
	      day (string-to-number (match-string 4))
	      time (match-string 6)
	      date (shimbun-make-date-string year month day time)
	      id (format "<%04d%02d%02d%s%%news@pocketgames>"
			 year month day
			 (apply (lambda (x y) (format "%02d%02d" x y))
                                (mapcar 'string-to-number (split-string time ":")))))
	(when (shimbun-search-id shimbun id)
	  (throw 'quit nil))
	(with-temp-buffer
	  (insert subject)
	  (shimbun-remove-markup)
	  (setq subject (buffer-string)))
	(setq url (w3m-expand-url
		   (w3m-decode-anchor-string url)
		   (concat (shimbun-index-url shimbun) "/")))
	(push (shimbun-make-header
	       0 (shimbun-mime-encode-string subject)
	       from date id "" 0 0 url)
	      headers)))
    headers))

(luna-define-method shimbun-make-contents ((shimbun shimbun) header)
  (when (shimbun-clear-contents shimbun header)
    (goto-char (point-min))
    (insert "<html>\n<head>\n<base href=\""
	    (shimbun-header-xref header)
	    "\">\n</head>\n<body>\n")
    (goto-char (point-max))
    (insert (shimbun-footer shimbun header t)
	    "\n</body>\n</html>\n"))
  (shimbun-pocketgames-make-mime-article shimbun header)
  (buffer-string))

(defun shimbun-pocketgames-make-mime-article (shimbun header)
  "Make a MIME article according to SHIMBUN and HEADER.
If article have inline images, generated article have a multipart/related
content-type if `shimbun-encapsulate-images' is non-nil."
  (let ((case-fold-search t)
	(count 0)
	(msg-id (shimbun-header-id header))
	beg end
	url type img imgs boundary charset)
    (when (string-match "^<\\([^>]+\\)>$" msg-id)
      (setq msg-id (match-string 1 msg-id)))
    (setq charset
	  (upcase (symbol-name
		   (detect-mime-charset-region (point-min)(point-max)))))
    (goto-char (point-min))
    (when shimbun-encapsulate-images
      (while (re-search-forward "<img" nil t)
	(setq beg (point))
	(when (search-forward ">" nil t)
	  (setq end (point))
	  (goto-char beg)
	  (when (re-search-forward
		 ;; <img src=pics/palm/clieplanet.jpg>
		 "src[ \t\r\f\n]*=[ \t\r\f\n]*\\([^ >]*\\)" end t)
	    (save-match-data
	      (setq url (match-string 1))
	      (unless (setq img (assoc url imgs))
		(setq imgs (cons
			    (setq img (list
				       url
				       (format "shimbun.%d.%s"
					       (incf count)
					       msg-id)
				       (with-temp-buffer
					 (set-buffer-multibyte nil)
					 (setq
					  type
					  (shimbun-retrieve-url
					   (shimbun-expand-url
					    url
					    (shimbun-header-xref header))
					   'no-cache 'no-decode))
					 (buffer-string))
				       type))
			    imgs))))
	    (replace-match (concat "src=\"cid:" (nth 1 img) "\"")))))
      (setq imgs (nreverse imgs)))
    (goto-char (point-min))
    (shimbun-header-insert shimbun header)
    (if imgs
	(progn
	  (setq boundary (apply 'format "===shimbun_%d_%d_%d==="
				(current-time)))
	  (insert "Content-Type: multipart/related; type=\"text/html\"; "
		  "boundary=\"" boundary "\"; start=\"<shimbun.0." msg-id ">\""
		  "\nMIME-Version: 1.0\n\n"
		  "--" boundary
		  "\nContent-Type: text/html; charset=" charset
		  "\nContent-ID: <shimbun.0." msg-id ">\n\n"))
      (insert "Content-Type: text/html; charset=" charset "\n"
	      "MIME-Version: 1.0\n\n"))
    (encode-coding-region (point-min) (point-max)
			  (mime-charset-to-coding-system charset))
    (goto-char (point-max))
    (dolist (img imgs)
      (unless (eq (char-before) ?\n) (insert "\n"))
      (insert "--" boundary "\n"
	      "Content-Type: " (or (nth 3 img) "application/octed-stream")
	      "\nContent-Disposition: inline"
	      "\nContent-ID: <" (nth 1 img) ">"
	      "\nContent-Transfer-Encoding: base64"
	      "\n\n"
	      (shimbun-base64-encode-string (nth 2 img))))
    (when imgs
      (unless (eq (char-before) ?\n) (insert "\n"))
      (insert "--" boundary "--\n"))))

(provide 'sb-pocketgames)

;;; sb-pocketgames.el ends here

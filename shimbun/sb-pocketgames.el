;;; sb-pocketgames.el --- shimbun backend class for www.pocketgames.jp. -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Keywords: news

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

(defvar shimbun-pocketgames-content-hash-length 31)
(defvar shimbun-pocketgames-url "http://www.pocketgames.jp")
(defvar shimbun-pocketgames-groups '("news"))
(defvar shimbun-pocketgames-coding-system 'shift_jis)

(luna-define-method initialize-instance :after ((shimbun shimbun-pocketgames)
						&rest init-args)
  (shimbun-pocketgames-set-content-hash-internal
   shimbun
   (make-vector shimbun-pocketgames-content-hash-length 0))
  shimbun)

(luna-define-method shimbun-index-url ((shimbun shimbun-pocketgames))
  shimbun-pocketgames-url)

(luna-define-method shimbun-reply-to ((shimbun shimbun-pocketgames))
  "Return the mailing list address."
  "info@pocketgames.jp")

(defvar shimbun-pocketgames-expiration-days 6)

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

(defun shimbun-pocketgames-headers-1 (shimbun &optional regexp)
  (let ((url (shimbun-index-url shimbun))
	from year month day time date
	next point subject id start end body
	headers)
    (unless regexp
      (setq regexp "\">\\([^<>]+\\)</a></font><br>"))
    (while (re-search-forward regexp nil t nil)
      (catch 'next
	(setq subject (match-string 1)
	      start (point)
	      end (and (re-search-forward "^<BR><BR>" nil t nil)
		       (set-marker (make-marker) (point))))
	(goto-char start)
	(unless
	    (re-search-forward
	     "Posted by: \\(.+\\) on \\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\) (\\(月\\|火\\|水\\|木\\|金\\|土\\|日\\))  - \\([0-9][0-9]:[0-9][0-9]\\) JST <\/font>"
	     end t nil)
	  (throw 'next nil))
	(setq from (shimbun-mime-encode-string (match-string 1))
	      year (string-to-number (match-string 2))
	      month (string-to-number (match-string 3))
	      day (string-to-number (match-string 4))
	      time (match-string 6)
	      date (shimbun-make-date-string year month day time)
	      id (format "<%04d%02d%02d%s%%news@pocketgames>"
			 year month day
			 (apply (lambda (x y) (format "%02d%02d" x y))
                                (mapcar 'string-to-number (split-string time ":"))))
	      point (point)
	      next (or (re-search-forward
			"\">\\([^<>]+\\)</a></font><br>" nil t nil)
		       (point-max)))
	(goto-char point)
	(setq headers (shimbun-pocketgames-comment-article
		       shimbun headers url id next))
	(when (shimbun-search-id shimbun id)
	  (throw 'next nil))
	(with-temp-buffer
	  (insert subject)
	  (shimbun-remove-markup)
	  (setq subject (buffer-string)))
	(setq start (re-search-forward
		     "^<TD ALIGN=\"left\" VALIGN=\"top\" CLASS=\"pn-normal\">"
		     end t nil))
	;;<a class="pn-normal" href="modules.php?op=modload&amp;name=Search&amp;file=index&amp;action=search&amp;overview=1&amp;active_stories=1&amp;stories_topics[0]=7">
	(while (re-search-forward "<br />" end t nil)
	  (replace-match "<br>"))
	(goto-char start)
	(when (re-search-forward
	       "<a class=\"pn-normal\" href=\"modules.php[^\"]+\">"
	       end t nil)
	  (delete-region (match-beginning 0) (match-end 0))
	  (when (re-search-forward
		 ;;<img src="images/topics/palmsoft_icon.gif" border="0" Alt="Palmソフトウェア" align="right" hspace="5" vspace="5"></a>
		 "<img src=\"images/topics/.+\\.\\(gif\\|jpg\\)[^/]+</a>"
		 end t nil)
	    (delete-region (match-beginning 0) (match-end 0))))
	(setq body (buffer-substring-no-properties start end))
	(set (intern id (shimbun-pocketgames-content-hash-internal shimbun))
	     body)
	(push (shimbun-make-header
	       0 (shimbun-mime-encode-string subject)
	       from date id "" 0 0 (concat url "/"))
	      headers)))
    headers))

(defun shimbun-pocketgames-comment-article (shimbun headers baseurl ref-id end)
  (let (url from year month day date time
	subject body id start end)
    (save-excursion
      (when (re-search-forward
	     "<a class=\"pn-normal\" href=\"\\(modules.php\?.+\\)\">[0-9]+ コメント<\/a>"
	     end t nil)
	(setq url (concat baseurl "/" (w3m-decode-anchor-string (match-string 1))))
	(with-temp-buffer
	  (shimbun-retrieve-url url 'reload 'binary)
	  (set-buffer-multibyte t)
	  (decode-coding-region (point-min) (point-max)
				(shimbun-coding-system-internal shimbun))
	  (goto-char (point-min))
	  (re-search-forward "<!-- *COMMENTS NAVIGATION BAR END *-->" nil t nil)
	  (while (re-search-forward "<font class=\"pn-title\">\\(Re: [^<]+\\)" nil t nil)
	    (catch 'next
	      (setq subject (match-string-no-properties 1))
	      (unless (re-search-forward
		       ;; <br>by <a class="pn-normal" href="mailto:-">aoshimak</a> <b>(-)</b></font><font class="pn-sub"> on 2003年3月08日 - 06:15 PM<br><font class="pn-normal">
		       "<br>by \\(.+\\) on \\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日 - \\([0-9][0-9]+:[0-9][0-9]+\\) \\(AM\\|PM\\)\\(</font></td></tr><tr><td>\\|<br>\\)<font class=\"pn-normal\">"
		       nil t nil)
		(throw 'next nil))
	      (setq from (match-string-no-properties 1)
		    year (string-to-number (match-string-no-properties 2))
		    month (string-to-number (match-string-no-properties 3))
		    day (string-to-number (match-string-no-properties 4))
		    time (match-string-no-properties 5)
		    start (point)
		    end (and (search-forward
			      "</font></td></tr></table><br><br><font class=\"pn-normal\">"
			      nil t nil)
			     (set-marker (make-marker) (match-beginning 0)))
		    date (shimbun-make-date-string year month day time)
		    id (format "<%04d%02d%02d%s%%comment@pocketgames>"
			year month day
			(apply (lambda (x y) (format "%02d%02d" x y))
			       (mapcar 'string-to-number (split-string time ":")))))
	      (when (shimbun-search-id shimbun id)
		(throw 'next nil))
	      (goto-char start)
	      (when (re-search-forward "(<a class=\"pn-normal\" href=\"user.php[^>]+>ユーザーインフォメーション<\/a> | <a href=\"modules.php[^>]+\">メッセージを送信<\/a>)" end t nil)
		(delete-region (match-beginning 0) (match-end 0)))
	      (setq body (buffer-substring start end))
	      (with-temp-buffer
		(insert from)
		(shimbun-remove-markup)
		(setq from (shimbun-mime-encode-string (buffer-string)))
		(erase-buffer)
		(insert subject)
		(shimbun-remove-markup)
		(setq subject (shimbun-mime-encode-string (buffer-string))))
	      (set (intern id (shimbun-pocketgames-content-hash-internal shimbun))
		   body)
	      (push (shimbun-make-header 0 subject from date id ref-id 0 0 url)
		    headers)))))
      headers)))

(luna-define-method shimbun-article
  ((shimbun shimbun-pocketgames) header &optional outbuf)
  (shimbun-pocketgames-article shimbun header outbuf))

(defun shimbun-pocketgames-article (shimbun header outbuf)
  (let (string)
    (with-current-buffer (or outbuf (current-buffer))
      (with-temp-buffer
	(let ((sym (intern-soft (shimbun-header-id header)
				(shimbun-pocketgames-content-hash-internal
				 shimbun))))
	  (if (not (and (boundp sym) (symbol-value sym)))
	      (shimbun-pocketgames-headers-1
	       shimbun
	       (concat
		"\">\\("
		(regexp-quote (shimbun-header-subject header))
		"\\)</a></font><br>")))
	  (setq string (shimbun-pocketgames-article-1
			shimbun header sym))))
      (when string
	(w3m-insert-string string)))))

(defun shimbun-pocketgames-article-1 (shimbun header sym)
  (message "shimbun: Make contents...")
  (insert (symbol-value sym))
  (goto-char (point-min))
  (insert "<html>\n<head>\n<base href=\""
	  (shimbun-header-xref header) "\">\n</head>\n<body>\n")
  (goto-char (point-max))
  (insert "\n</body>\n</html>\n")
  (encode-coding-string
   (buffer-string)
   (mime-charset-to-coding-system "ISO-2022-JP"))
  (shimbun-pocketgames-make-mime-article shimbun header)
  (message "shimbun: Make contents...done")
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

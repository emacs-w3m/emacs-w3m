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
  "info@pocketgames.jp")

(defvar shimbun-pocketgames-expiration-days 6)

(luna-define-method shimbun-headers ((shimbun shimbun-pocketgames)
				     &optional range)
  (let ((case-fold-search t)
	(url (shimbun-index-url shimbun))
	(count -1)
	from year month day date lastyear lastmonth lastday
	next point subject id start end body headers)
    (with-temp-buffer
      (shimbun-retrieve-url url 'reload 'binary)
      (set-buffer-multibyte t)
      (decode-coding-region (point-min) (point-max)
			    (shimbun-coding-system-internal shimbun))
      (goto-char (point-min))
      (while (re-search-forward "\">\\([^<>]+\\)</a></font><br>" nil t nil)
	(catch 'next
	  (setq subject (match-string 1)
		start (point)
		end (re-search-forward "^<BR><BR>" nil t nil))
	  (goto-char start)
	  (unless
	      (re-search-forward
	       "Posted by: \\(.+\\) on \\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\) (\\(月\\|火\\|水\\|木\\|金\\|土\\|日\\))"
	       end t nil)
	    (throw 'next nil))
	  (setq from (shimbun-mime-encode-string (match-string 1))
		year (string-to-number (match-string 2))
		month (string-to-number (match-string 3))
		day (string-to-number (match-string 4))
		date (shimbun-make-date-string year month day))
	  (if (and lastyear lastmonth lastday
		   (= lastday day) (= lastmonth month) (= lastyear year))
	      (setq count (1+ count))
	    (setq count 0))
	  (setq id (format "<%02d%04d%02d%02d.news.pocketgames>"
			   count year month day)
		lastyear year
		lastmonth month
		lastday day
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
	  (setq body (buffer-substring-no-properties start end))
	  (set (intern id (shimbun-pocketgames-content-hash-internal shimbun))
	       body)
	  (push (shimbun-make-header
		 0 (shimbun-mime-encode-string subject)
		 from date id "" 0 0 (concat url "/"))
		headers))))
      headers))

(defun shimbun-pocketgames-comment-article (shimbun headers baseurl ref-id end)
  (let ((count 0)
	url from year month day date
	subject body id	lastyear lastmonth lastday)
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
	  (while (re-search-forward
		  "<br>by \\(.+\\) on \\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日.*</font>.+<font class=\"pn-normal\">\\([^\n\r]+\\)"
		  nil t nil)
	    (catch 'next
	      (setq from (match-string-no-properties 1)
		    year (string-to-number (match-string-no-properties 2))
		    month (string-to-number (match-string-no-properties 3))
		    day (string-to-number (match-string-no-properties 4))
		    subject (match-string-no-properties 5)
		    body (buffer-substring
			  (point)
			  (search-forward 
			   "</font></td></tr></table><br><br><font class=\"pn-normal\">"
			   nil t nil))
		    date (shimbun-make-date-string year month day))
	      (if (and lastyear lastmonth lastday
		       (= lastday day) (= lastmonth month) (= lastyear year))
		  (setq count (1+ count))
		(setq count 0))
	      (setq id (format "<%02d%04d%02d%02d.comment.pocketgames>"
			       count year month day)
		    lastyear year
		    lastmonth month
		    lastday day)
	      (when (shimbun-search-id shimbun id)
		(throw 'next nil))
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
  (let (string)
    (with-current-buffer (or outbuf (current-buffer))
      (with-temp-buffer
	(let ((sym (intern-soft (shimbun-header-id header)
				(shimbun-pocketgames-content-hash-internal
				 shimbun))))
	  (when (and (boundp sym) (symbol-value sym))
	    (insert (symbol-value sym))
	    (goto-char (point-min))
	    (insert "<html>\n<head>\n<base href=\""
		    (shimbun-header-xref header) "\">\n</head>\n<body>\n")
	    (goto-char (point-max))
	    (insert "\n</body>\n</html>\n")
	    (encode-coding-string
	     (buffer-string)
	     (mime-charset-to-coding-system "ISO-2022-JP"))
	    (shimbun-make-mime-article shimbun header)
	    (setq string (buffer-string)))))
      (when string
	(w3m-insert-string string)))))

(provide 'sb-pocketgames)

;;; sb-pocketgames.el ends here

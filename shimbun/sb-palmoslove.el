;;; sb-palmoslove.el --- shimbun backend class for palmoslove web site. -*- coding: iso-2022-7bit; -*-

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
  (luna-define-class shimbun-palmoslove (shimbun) (content-hash))
  (luna-define-internal-accessors 'shimbun-palmoslove))

(defvar shimbun-palmoslove-content-hash-length 31)
(defvar shimbun-palmoslove-url "http://palmoslove.com")
(defvar shimbun-palmoslove-coding-system 'japanese-shift-jis)

(defvar shimbun-palmoslove-groups
  '("webnews" "pdanews" "sitenews"))

(defvar shimbun-palmoslove-expiration-days 14)

(luna-define-method initialize-instance :after ((shimbun shimbun-palmoslove)
						&rest init-args)
  (shimbun-palmoslove-set-content-hash-internal
   shimbun
   (make-vector shimbun-palmoslove-content-hash-length 0))
  shimbun)

(luna-define-method shimbun-index-url ((shimbun shimbun-palmoslove))
  (concat (shimbun-url-internal shimbun)
	  "/"
	  (shimbun-current-group-internal shimbun)
	  ".html"))

(luna-define-method shimbun-headers ((shimbun shimbun-palmoslove)
				     &optional range)
  (let ((case-fold-search t)
	(url (shimbun-index-url shimbun))
	(group (shimbun-current-group-internal shimbun))
	(idbase "palmoslove")
	(from "webmaster@palmoslove.com")
	(year (string-to-number (substring (current-time-string) 20)))
	headers)
    (with-temp-buffer
      (shimbun-retrieve-url url 'no-cache 'no-decode)
      (decode-coding-region
       (point-min) (point-max)
       (shimbun-coding-system-internal shimbun))
      (set-buffer-multibyte t)
      (subst-char-in-region (point-min) (point-max) ?\t ?  t)
      (goto-char (point-min))
      (catch 'stop
	(unless (re-search-forward "<!-- *WebNewsWeekly Start *-->" nil t nil)
	  (throw 'stop nil))
	(delete-region (point-min) (point))
	(when (re-search-forward "<!-- footer start -->" nil t nil)
	  (delete-region (point-max) (point)))
	(goto-char (point-min))
	(while (re-search-forward
		"<B>\\([0-9][0-9]\\)月\\([0-9][0-9]\\)日<\/B><\/TD>"
		nil t nil)
	  (let* ((start (point))
		 (month (string-to-number
			 (match-string-no-properties 1)))
		 (day (string-to-number
		       (match-string-no-properties 2)))
		 (end (if (re-search-forward
			   "<B>\\([0-9][0-9]\\)月\\([0-9][0-9]\\)日<\/B><\/TD>"
			   nil t nil)
			  (progn
			    (goto-char (match-beginning 0))
			    (forward-char -1)
			    (point))
			(point-max)))
		 date link subject hour minutes ampm time id body)
	    (goto-char start)
	    (while (re-search-forward
		    "<TD VALIGN=\"top\">\\(<A HREF=\"[^<>]+\">\\)"
		    end t nil)
	      (catch 'next
		(setq link (match-string-no-properties 1)
		      subject (buffer-substring-no-properties
			       (point)
			       (progn
				 (search-forward "</A>" end t nil)
				 (match-beginning 0))))
		(re-search-forward
		 "(\\([0-9]+\\):\\([0-9]+\\)\&nbsp;\\(AM\\|PM\\))"
		 end t nil)
		(setq hour (string-to-number (match-string-no-properties 1))
		      minutes (string-to-number (match-string-no-properties 2))
		      ampm (match-string-no-properties 3))
		(when (string= ampm "PM")
		  (setq hour (+ 12 hour)))
		(setq time (format "%02d:%02d" hour minutes)
		      date (shimbun-make-date-string year month day time))
		(unless (re-search-forward
			 "<FONT size=\"-1\">\\(.+\\)<\/FONT><\/TD>"
			 end t nil)
		  (throw 'next nil))
		(setq body (match-string-no-properties 1)
		      body (concat
			    (format "%s%s</A><BR>" link subject)
			    body)
		      id (format "<%04d%04d%02d%02d%s@%s.%s>"
				 (length body)
				 year month day time group idbase))
		(if (shimbun-search-id shimbun id)
		    (throw 'stop nil))
		(set (intern id (shimbun-palmoslove-content-hash-internal shimbun))
		     body)
		(while (string-match "[\n\r]+" subject)
		  (setq subject (concat
				 (substring subject 0 (match-beginning 0))
				 (substring subject (match-end 0)))))
		(push (shimbun-make-header
		       0 (shimbun-mime-encode-string subject)
		       from date id "" 0 0 url)
		      headers)))))))
    headers))

(luna-define-method shimbun-article
  ((shimbun shimbun-palmoslove) header &optional outbuf)
  (let (string)
    (with-current-buffer (or outbuf (current-buffer))
      (with-temp-buffer
	(let ((sym (intern-soft (shimbun-header-id header)
				(shimbun-palmoslove-content-hash-internal
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

(provide 'sb-palmoslove)

;;; sb-palmoslove.el ends here

;;; sb-palmfan.el --- shimbun backend class for palmfan web site.

;; Copyright (C) 2002 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
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
  (luna-define-class shimbun-palmfan (shimbun) (content-hash))
  (luna-define-internal-accessors 'shimbun-palmfan))

(defvar shimbun-palmfan-content-hash-length 31)
(defvar shimbun-palmfan-url "http://www.palmfan.com")
;;(defvar shimbun-palmfan-coding-system 'japanese-shift-jis-mac)
(defconst shimbun-palmfan-group-path-alist
  '(("news" . "")
    ;; not yet
    ;;("nm502i" . "/cgi/tnote.cgi?book=book2")
    ;;("hotsync" . "/cgi/tnote.cgi?book=book3")
    ))

(defvar shimbun-palmfan-groups
  (mapcar 'car shimbun-palmfan-group-path-alist))

(defconst shimbun-palmfan-date-regexp
  ;;<P><A name="Apr,13.2002"></A><B>　Apr,13.2002</B><A href="#Apr,12.2002">▼</A>
  "^<P><A name=\"\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\|\\),\\([0-9]+\\)\\.\\([0-9]+\\)\"></A><B>.*▼</A>$")

(defconst shimbun-palmfan-month-alist
  '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4)
    ("May" . 5) ("Jun" . 6) ("Jul" . 7) ("Aug" . 8)
    ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))

(luna-define-method initialize-instance :after ((shimbun shimbun-palmfan)
						&rest init-args)
  (shimbun-palmfan-set-content-hash-internal
   shimbun
   (make-vector shimbun-palmfan-content-hash-length 0))
  shimbun)

(luna-define-method shimbun-reply-to ((shimbun shimbun-palmfan))
  "hirose@palmfan.com")

(luna-define-method shimbun-index-url ((shimbun shimbun-palmfan))
  (concat (shimbun-url-internal shimbun)
	  "/"
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-palmfan-group-path-alist))))

(luna-define-method shimbun-headers ((shimbun shimbun-palmfan)
				     &optional range)
  (let ((group (shimbun-current-group-internal shimbun)))
    (if (string= group "news")
	(shimbun-palmfan-news-headers shimbun range)
      (shimbun-palmfan-bbs-headers shimbun range))))

(defun shimbun-palmfan-bbs-headers (shimbun &optional range)
  ;; not yet
  )

(defun shimbun-palmfan-news-headers (shimbun &optional range)
  (let* ((case-fold-search t)
	 (url (shimbun-index-url shimbun))
	 (idbase (if (string-match "^http://\\([^/]+\\)/" url)
		     (match-string 1 url)
		   url))
	 (from "hirose@palmfan.com")
	headers)
    (with-temp-buffer
      (shimbun-retrieve-url url 'no-cache 'no-decode)
      (decode-coding-region
       (point-min) (point-max) 'japanese-shift-jis-mac)
      (set-buffer-multibyte t)
      (subst-char-in-region (point-min) (point-max) ?\t ?  t)
      (goto-char (point-min))
      (when (re-search-forward "^<!--スポンサー・バナーここまで-->$" nil t nil)
	(forward-line 1)
	(beginning-of-line 1)
	(delete-region (point-min) (point)))
      (when (re-search-forward "■過去記事一覧■<BR>$" nil t nil)
	(beginning-of-line 1)
	(delete-region (point) (point-max)))
      (goto-char (point-min))
      (catch 'stop
	(while (re-search-forward shimbun-palmfan-date-regexp nil t nil)
	  (let* ((month (match-string 1))
		 (day (string-to-number (match-string 2)))
		 (year (string-to-number (match-string 3)))
		 (date (format "%02d %s %04d 00:00 +0900" day month year))
		 (start (point-marker))
		 (end (progn
			(if (re-search-forward shimbun-palmfan-date-regexp nil t nil)
			    (progn
			      (beginning-of-line)
			      (forward-char -1))
			  (point-max))
			(point-marker)))
		 (count -1))
	    (goto-char start)
	    (while (or (re-search-forward
			"^<!-- \\(トピック\\|ソフト\\)タイトル -->$" end t nil)
		       ;; <FONT color="#0000AF">●</FONT><B>ひとりごと</B>
		       ;; <FONT color="#0000AF">●</FONT><B>DCF・Exif・JPEGについて</B>
		       (re-search-forward
			"^<FONT color=\"#0000AF\">●</FONT><B>\\(.+\\)</B>" end t nil))
	      (let (subject id others body)
		(if (not (member (match-string 1) '("トピック" "ソフト")))
		    (progn
		      (setq subject (match-string 1))
		      (unless (string= others "ひとりごと")
			;;<FONT color="#0000AF">●</FONT><B>DCF・Exif・JPEGについて</B>
			(setq others t)))
		  (setq subject (buffer-substring-no-properties
				 (progn (forward-char 1) (point))
				 (progn (re-search-forward "<BLOCKQUOTE>" end t nil)
					(beginning-of-line 1) (point)))))
		(when (or others
			  (re-search-forward "^<!--\\(本文\\|コメント\\|ひとりごと本文\\)-->$" end t nil))
		  (setq body (buffer-substring-no-properties
			      (point) (search-forward "</BLOCKQUOTE>" end))
			count (1+ count)
			id (format "<%02d%04d%02d%02d@%s>" count year 
				   (cdr (assoc month shimbun-palmfan-month-alist))
				   day idbase))
		  (if (shimbun-search-id shimbun id)
		      (throw 'stop nil))
		  (when (string-match "^[\n\t ]*\\(.*\\)[\n\t ]*$" subject)
		    (setq subject (match-string 1 subject)))
		  (let ((case-fold-search t))
		    (when (string-match "<A href=.*</A>" subject)
		      (setq body (concat "<P>" subject "</P>" body))))
		  (with-temp-buffer
		    (insert subject)
		    (shimbun-remove-markup)
		    (setq subject (buffer-string)))
		  (set (intern id (shimbun-palmfan-content-hash-internal shimbun))
		       body)
		  (push (shimbun-make-header
			 0 (shimbun-mime-encode-string subject)
			 from date id "" 0 0 id)
			headers)))))))
      (nreverse headers))))

(luna-define-method shimbun-article ((shimbun shimbun-palmfan) header
				     &optional outbuf)
  (when (shimbun-current-group-internal shimbun)
    (with-current-buffer (or outbuf (current-buffer))
      (insert
       (with-temp-buffer
	 (let ((sym (intern-soft (shimbun-header-xref header)
				 (shimbun-palmfan-content-hash-internal
				  shimbun))))
	   (if (boundp sym)
	       (insert (symbol-value sym)))
	   (goto-char (point-min))
	   (shimbun-header-insert shimbun header)
	   (insert "Content-Type: " "text/html"
		   "; charset=ISO-2022-JP\n"
		   "MIME-Version: 1.0\n")
	   (insert "\n")
	   (encode-coding-string
	    (buffer-string)
	    (mime-charset-to-coding-system "ISO-2022-JP"))))))))

(provide 'sb-palmfan)

;;; sb-palmfan.el ends here

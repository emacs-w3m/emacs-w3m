;;; sb-wincefan.el --- shimbun backend for Muchy's Palmware Review! -*- coding: iso-2022-7bit; -*-

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

;;; Commentary:

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-wincefan (shimbun) ())

(luna-define-method shimbun-index-url ((shimbun shimbun-wincefan))
  (shimbun-url-internal shimbun))

(defconst shimbun-wincefan-url "http://www.wince.ne.jp/")
(defvar shimbun-wincefan-groups '("news"))
(defvar shimbun-wincefan-from-address "webmaster@wincefan.ne.jp")
(defvar shimbun-wincefan-coding-system 'japanese-shift-jis)
(defvar shimbun-wincefan-content-start "\n<BLOCKQUOTE><B>■トピックスの内容</B>")
(defvar shimbun-wincefan-content-end "</BLOCKQUOTE>\n")
(defvar shimbun-wincefan-expiration-days 14)

(defsubst shimbun-wincefan-parse-time (str)
  (save-match-data
    (if (string-match "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)" str)
	(list (string-to-number (match-string 1 str))
	      (string-to-number (match-string 2 str))
	      (string-to-number (match-string 3 str))))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-wincefan)
					 &optional outbuf)
  (let* ((from (shimbun-from-address-internal shimbun))
	 (group (shimbun-current-group-internal shimbun))
	 (baseurl (when (string-match "^http://\\([^/]+\\)/*$"
				      shimbun-wincefan-url)
		    (match-string 1 shimbun-wincefan-url)))
	 (case-fold-search t)
	 url headers)
    (catch 'stop
      (if (not (re-search-forward "^[\t ]+<!-+ＷＨＡＴ’Ｓ　ＮＥＷ-+>"
				  nil t nil))
	  (throw 'stop nil)
	(delete-region (point-min) (point)))
      (if (not (re-search-forward "^[\t ]+<!-+ＨＯＴ　ＴＯＰＩＣＳ-+>"
				  nil t nil))
	  (throw 'stop nil)
	(delete-region (point-max) (point)))
      (goto-char (point-min))
      (while (re-search-forward 
	      "<TR><TD VALIGN=TOP NOWRAP><FONT COLOR=\"[#A-Z0-9]+\" SIZE=-1>\\([0-9]+\\)月\\([0-9]+\\)日(\\(月\\|火\\|水\\|木\\|金\\|土\\|日\\))</FONT></TD>"
	      nil t nil)
	(let* ((month (match-string 1))
	       (day (match-string 2))
	       (year (substring (current-time-string) 20))
	       (date (shimbun-make-date-string 
		      (string-to-number year)
		      (string-to-number month)
		      (string-to-number day)))
	       (rawdate 
		(string-to-number 
		 (concat year
			 (format "%02d" (string-to-number month))
			 (format "%02d" (string-to-number day)))))
	       (end (save-excursion
		      (or (re-search-forward 
			   "<TR><TD VALIGN=TOP NOWRAP><FONT COLOR=\"[#A-Z0-9]+\" SIZE=-1>\\([0-9]+\\)月\\([0-9]+\\)日(\\(月\\|火\\|水\\|木\\|金\\|土\\|日\\))</FONT></TD>"
			   nil t nil)
			  (point-max))))
	       (count -1)
	       subject id)
	  (while (re-search-forward
		  "\\[<A HREF=\"[^<>]+\"><FONT COLOR=\"[#A-Z0-9]+\" SIZE=-1>\\([^<>]+\\)<\\/FONT><\\/A>\\]"
		  end t nil)
	    (setq subject (match-string 1))
	    (setq count (1+ count))
	    (when (re-search-forward "<TD><A HREF=\"\\([^<>]+\\)\">" end t nil)
	      (setq url (match-string 1)))
	    (when (string-match "frame.asp\\?/" url)
	      (setq url (concat (substring url 0 (match-beginning 0))
				(substring url (match-end 0)))))
	    (when (re-search-forward
		   "<FONT SIZE=-1>\\([^<>]+\\)<\/FONT><\/A><\/TD>"
		   end t nil)
	      (setq subject (concat (if subject (concat subject "/")) (match-string 1))))
	    ;; building ID
	    (setq id (format "<%02d%08d%%%s@%s>" count rawdate group baseurl))
	    (if (shimbun-search-id shimbun id)
		(throw 'stop nil))
	    (push (shimbun-make-header
		   0 (shimbun-mime-encode-string subject)
		   from date id "" 0 0 url)
		  headers)))))
    headers))

(luna-define-method shimbun-make-contents :before
  ((shimbun shimbun-wincefan) header)
  (let ((case-fold-search t))
    (save-excursion
      (when (re-search-forward "^<BLOCKQUOTE>" nil t nil)
	(delete-region (point) (point-min)))
      (when (re-search-forward  "^<\/BLOCKQUOTE>" nil t nil)
	(delete-region (match-beginning 0) (point-max))))))

(provide 'sb-wincefan)

;;; sb-wincefan.el ends here

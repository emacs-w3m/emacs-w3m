;;; sb-makanai.el --- shimbun backend for www.makanai.com -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005
;; MIYOSHI Masanori <miyoshi@meadowy.org>

;; Author: MIYOSHI Masanori <miyoshi@meadowy.org>
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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>.

;;; Code:

(require 'shimbun)
(require 'sb-text)

(eval-and-compile
  (luna-define-class shimbun-makanai (shimbun shimbun-text) (content-hash))
  (luna-define-internal-accessors 'shimbun-makanai))

(defvar shimbun-makanai-url "http://www.makanai.com/")
(defvar shimbun-makanai-server-name "makanai")
(defvar shimbun-makanai-groups '("f1news"))
(defvar shimbun-makanai-group-alist
  '(("f1news" . "http://www1.wisnet.ne.jp/~matunaga/")))
(defvar shimbun-makanai-from-address  "matunaga@mail1.wisnet.ne.jp")
(defvar shimbun-makanai-content-hash-length 31)

(luna-define-method initialize-instance :after ((shimbun shimbun-makanai)
						&rest init-args)
  (shimbun-makanai-set-content-hash-internal
   shimbun
   (make-vector shimbun-makanai-content-hash-length 0))
  shimbun)

(luna-define-method shimbun-index-url ((shimbun shimbun-makanai))
  (concat (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-makanai-group-alist))
	  "news/001.html"))

(eval-and-compile
  (unless (and (fboundp 'md5)
	       (subrp (symbol-function 'md5)))
    ;; The lisp function might be provided by FLIM.
    (autoload 'md5 "md5")))

(defun shimbun-makanai-scan-articles (shimbun &optional force-rescan
					      skip-this-page skip-next-page)
  (let (headers
	(case-fold-search t)
	(search-next-page t))
    (catch 'stop
      (while (re-search-forward "<title>F1gpnews</title>" nil t)
	(while (and (not skip-this-page)
		    (re-search-forward "<font color=#[0-9a-f]+>\\([0-9]+\\)年\\([0-9]+\\)月\\([0-9]+\\)日(\\(\\w+\\)) \\([^>]+\\) ?</font>" nil t))
	  (let* ((year (match-string 1))
		 (month (match-string 2))
		 (day (match-string 3))
		 ;;(day-of-week (match-string 4))
		 (subject (match-string 5))
		 (article-number (md5 subject))
		 (article (buffer-substring
			   (match-end 0)
			   (progn
			     (search-forward "</blockquote>" nil t)
			     (point))))
		 (id (format "<%s.%s.%s.%s.%s@www.makanai.com>"
			     year month day
			     article-number
			     (shimbun-current-group-internal shimbun)))
		 (date (shimbun-make-date-string (string-to-number year)
						 (string-to-number month)
						 (string-to-number day)))
		 (xref (format "%s#%s"
			       (shimbun-index-url shimbun) article-number)))
	    (if (and (not force-rescan)
		     (shimbun-search-id shimbun id))
		(setq search-next-page nil))
	    (set (intern xref
			 (shimbun-makanai-content-hash-internal shimbun))
		 article)

	    (push (shimbun-create-header
		   0
		   subject
		   (shimbun-from-address shimbun)
		   date id "" 0 0 xref)
		  headers)))
	(when (and search-next-page  (not skip-next-page)
		   (re-search-forward
		    "<a href=\"\\([^\"]+\\)\">これ以前のニュース</a>" nil t))
	  (let ((url (concat (shimbun-index-url shimbun)
			     (match-string 1))))
	    (erase-buffer)
	    (shimbun-retrieve-url url t)
	    (setq skip-this-page nil)
	    (goto-char (point-min))))))
    headers))

(luna-define-method shimbun-get-headers ((shimbun shimbun-makanai)
					 &optional range)
  (shimbun-makanai-scan-articles shimbun))

(defun shimbun-makanai-retrieve-article (shimbun header)
  (with-temp-buffer
    (shimbun-retrieve-url (shimbun-index-url shimbun) t)
    (goto-char (point-min))
    (shimbun-makanai-scan-articles shimbun t nil t)
    ;; if not found, try next page
    (let ((sym (shimbun-makanai-search-article-in-hash shimbun header)))
      (unless (and sym (boundp sym))
	(goto-char (point-min))
	(shimbun-makanai-scan-articles shimbun t t)))))

(defun shimbun-makanai-search-article-in-hash (shimbun header)
  (intern-soft (shimbun-header-xref header)
	       (shimbun-makanai-content-hash-internal shimbun)))

(luna-define-method shimbun-article ((shimbun shimbun-makanai) header
				     &optional outbuf)
  (when (shimbun-current-group-internal shimbun)
    (with-current-buffer (or outbuf (current-buffer))
      (insert
       (with-temp-buffer
	 (let ((sym (shimbun-makanai-search-article-in-hash shimbun header)))
	   (unless (and sym (boundp sym))
	     (shimbun-makanai-retrieve-article shimbun header)
	     (setq sym
		   (shimbun-makanai-search-article-in-hash shimbun header)))
	   (if (and sym (boundp sym))
	       (insert (symbol-value sym)))
	   (shimbun-shallow-rendering)
	   (goto-char (point-min))
	   (shimbun-header-insert shimbun header)
	   (insert "Content-Type: " "text/html"
		   "; charset=ISO-2022-JP\n"
		   "MIME-Version: 1.0\n\n")
	   (encode-coding-string
	    (buffer-string)
	    (mime-charset-to-coding-system "ISO-2022-JP"))))))))

(luna-define-method shimbun-close :after ((shimbun shimbun-makanai))
  (shimbun-makanai-set-content-hash-internal shimbun nil))

(provide 'sb-makanai)

;;; sb-makanai.el ends here

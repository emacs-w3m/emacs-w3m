;;; sb-slashdot-jp.el --- shimbun backend for slashdot.jp -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Keywords: news
;; Created: Jun 14, 2003

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

;;; History:

;; This backend was created by Yuuichi Teranishi <teranisi@gohome.org>
;; at July 5th, 2001.

;; Because the site desgin had been changed, this backend was
;; completly rewritten by TSUCHIYA Masatoshi <tsuchiya@namazu.org> at
;; February 28th, 2002.

;; NAKAJIMA Mikio <minakaji@namazu.org> created a new backend,
;; sb-slashdot-jp-rss.el, at July 15th, 2003.  It was an alternative
;; backend of slashdot.jp based on RSS.

;; In order to reduce the cost to maintain both backends, the backend
;; based on the traditional approach was succeeded by the backend
;; based on RSS at January 18th, 2004.

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(defcustom shimbun-slashdot-jp-comment-arguments
  '((threshold . 1)
    (mode . nested)
    (commentsort . 0))
  "*Arguments to view comment pages."
  :group 'shimbun
  :type '(list
	  (cons :tag "Score threshold" :format "%t: %v"
		(const :tag "" threshold) integer)
	  (cons :tag "Threading mode" :format "%t: %v"
		(const :tag "" mode)
		(choice (const flat)
			(const nested)
			(const nocomment)
			(const thread)))
	  (cons :tag "Sorting order" :format "%t: %v"
		(const :tag "" commentsort)
		(choice (const :tag "Oldest first" 0)
			(const :tag "Newest first" 1)
			(const :tag "Highest scores first" 3)
			(const :tag "Oldest first (Ignore threads)" 4)
			(const :tag "Newest first (Ignore threads)" 5)))))

(luna-define-class shimbun-slashdot-jp (shimbun-rss) ())

(defvar shimbun-slashdot-jp-url "http://slashdot.jp/slashdot.rdf")
(defvar shimbun-slashdot-jp-groups '("story"))
(defvar shimbun-slashdot-jp-from-address  "webmaster@slashdot.jp")
(defvar shimbun-slashdot-jp-coding-system 'euc-japan)
(defvar shimbun-slashdot-jp-content-start
  "\n<!-- start template: ID [0-9]+,.*dispStory.* -->\n")
(defvar shimbun-slashdot-jp-content-end
  "\n<!-- end template: ID [0-9]+,.*dispStory.*-->\n")

(luna-define-method shimbun-index-url ((shimbun shimbun-slashdot-jp))
  shimbun-slashdot-jp-url)

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-slashdot-jp) url date)
  (unless (string-match
	   "\\`http://slashdot.jp/article.pl\\?sid=\\([/0-9]+\\)&" url)
    (error "Cannot find message-id base"))
  (concat "<" (match-string-no-properties 1 url) "@slashdot.jp>"))

(luna-define-method shimbun-get-headers :around
  ((shimbun shimbun-slashdot-jp) &optional range)
  (let ((headers (luna-call-next-method)))
    (dolist (head headers)
      (shimbun-header-set-xref head
			       (concat (shimbun-header-xref head)
				       "&mode=nocomment")))
    headers))

(defun shimbun-slashdot-jp-comment-url (url)
  (mapconcat 'identity
	     (cons (if (string-match "&mode=nocomment\\'" url)
		       (substring url 0 (match-beginning 0))
		     url)
		   (mapcar (lambda (x)
			     (format "%s=%s" (car x) (cdr x)))
			   shimbun-slashdot-jp-comment-arguments))
	     "&"))

(luna-define-method shimbun-clear-contents :around
  ((shimbun shimbun-slashdot-jp) header)
  (when (luna-call-next-method)
    (shimbun-remove-tags "<!-- begin ad code -->" "<!-- end ad code -->")
    (goto-char (point-max))
    (insert "\n<p align=left>[<a href=\""
	    (shimbun-slashdot-jp-comment-url (shimbun-header-xref header))
	    "\">もっと読む…</a>]</p>")
    t))

(provide 'sb-slashdot-jp)

;;; sb-slashdot-jp.el ends here

;;; sb-slashdot-jp-rss.el --- shimbun backend for slashdot-jp-rss -*- coding: iso-2022-7bit; -*-

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

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(defcustom shimbun-slashdot-jp-rss-comment-arguments
  '((threshold . 1)
    (mode . nested)
    (commentsort . 0))
  "*Arguments to view comment pages."
  :group 'shimbun
  :type '(repeat
	  (choice
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
			 (const :tag "Newest first (Ignore threads)" 5)))
	   (string :tag "User defined argument"))))

(luna-define-class shimbun-slashdot-jp-rss (shimbun-rss) ())

(defvar shimbun-slashdot-jp-rss-url "http://slashdot.jp/slashdot.rdf")
(defvar shimbun-slashdot-jp-rss-groups '("story"))
(defvar shimbun-slashdot-jp-rss-from-address  "webmaster@slashdot.jp")
(defvar shimbun-slashdot-jp-rss-coding-system 'euc-japan)
(defvar shimbun-slashdot-jp-rss-content-start
  "\n<!-- start template: ID [0-9]+,.*dispStory.* -->\n")
(defvar shimbun-slashdot-jp-rss-content-end
  "\n<!-- end template: ID [0-9]+,.*dispStory.*-->\n")

(luna-define-method shimbun-index-url ((shimbun shimbun-slashdot-jp-rss))
  shimbun-slashdot-jp-rss-url)

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-slashdot-jp-rss) url date)
  (unless (string-match
	   "http://slashdot.jp/article.pl\\?sid=[\/0-9]+\/\\([0-9]+\\)\\&topic=[0-9]+"
	   url)
    (error "Cannot find message-id base"))
  (concat "<" (match-string-no-properties 1 url) "%%rss@slashdot.jp>"))

(luna-define-method shimbun-get-headers :around
  ((shimbun shimbun-slashdot-jp-rss) &optional range)
  (let ((headers (luna-call-next-method)))
    (dolist (head headers)
      (shimbun-header-set-xref head
			       (concat (shimbun-header-xref head)
				       "&mode=nocomment")))
    headers))

(defun shimbun-slashdot-jp-rss-comment-url (url)
  (mapconcat (lambda (x)
	       (if (stringp x)
		   x
		 (format "%s=%s" (car x) (cdr x))))
	     (cons (if (string-match "&mode=nocomment\\'" url)
		       (substring url 0 (match-beginning 0))
		     url)
		   shimbun-slashdot-jp-rss-comment-arguments)
	     "&"))

(luna-define-method shimbun-clear-contents :around
  ((shimbun shimbun-slashdot-jp-rss) header)
  (when (luna-call-next-method)
    (shimbun-remove-tags "<!-- begin ad code -->" "<!-- end ad code -->")
    (goto-char (point-max))
    (insert "\n<p align=left>[<a href=\""
	    (shimbun-slashdot-jp-rss-comment-url (shimbun-header-xref header))
	    "\">もっと読む…</a>]</p>")
    t))

(provide 'sb-slashdot-jp-rss)

;;; sb-slashdot-jp-rss.el ends here

;;; sb-slashdot-jp.el --- shimbun backend for slashdot.jp -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi  <teranisi@gohome.org>,
;;         TSUCHIYA Masatoshi <tsuchiya@namazu.org>
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
(eval-when-compile (require 'cl))

(luna-define-class shimbun-slashdot-jp (shimbun) ())

(defconst shimbun-slashdot-jp-groups '("story"))

(eval-and-compile
  (defconst shimbun-slashdot-jp-domain "slashdot.jp"))

(defconst shimbun-slashdot-jp-url
  (eval-when-compile (format "http://%s/" shimbun-slashdot-jp-domain)))

(defvar shimbun-slashdot-jp-threshold -1
  "*Lower threshold of accept comments.")

(defmacro shimbun-slashdot-jp-article-url (shimbun)
  `(shimbun-expand-url "article.pl" (shimbun-url-internal ,shimbun)))

(defmacro shimbun-slashdot-jp-sid-url (shimbun sid)
  `(format "%s?sid=%s&threshold=%d&mode=flat&commentsort=0"
	   (shimbun-slashdot-jp-article-url shimbun)
	   ,sid
	   shimbun-slashdot-jp-threshold))

(defsubst shimbun-slashdot-jp-extract-sid-and-cid (id)
  (when (string-match "\\`<\\([/0-9]+\\)\\(#\\([0-9]+\\)\\)?@[^>]+>\\'" id)
    (list (match-string 1 id)
	  (match-string 3 id))))

(defmacro shimbun-slashdot-jp-make-message-id (sid &optional cid)
  (if cid
      `(concat "<" ,sid "#" ,cid "@" shimbun-slashdot-jp-domain ">")
    `(concat "<" ,sid "@" shimbun-slashdot-jp-domain ">")))

(luna-define-method shimbun-make-contents ((shimbun shimbun-slashdot-jp) head)
  (multiple-value-bind (sid cid)
      (shimbun-slashdot-jp-extract-sid-and-cid (shimbun-header-id head))
    (if cid
	(shimbun-slashdot-jp-make-comment-article shimbun head sid cid)
      (shimbun-slashdot-jp-make-story-article shimbun head sid))))

(defsubst shimbun-slashdot-jp-make-article-after (shimbun head)
  (goto-char (point-min))
  (insert "<html>
<head><base href=\"" (shimbun-header-xref head) "\"></head>
<body>\n")
  (goto-char (point-max))
  (insert "\n</body>\n</html>\n")
  (shimbun-make-mime-article shimbun head)
  (buffer-string))

(defconst shimbun-slashdot-jp-story-article-start-pattern
  "<!-- start template: ID 88, dispStory;misc;default -->")
(defconst shimbun-slashdot-jp-story-article-end-pattern
  "<!-- end template: ID 88, dispStory;misc;default -->")

(defun shimbun-slashdot-jp-make-story-article (shimbun head sid)
  (with-temp-buffer
    (let (begin)
      (when (and (shimbun-retrieve-url
		  (shimbun-slashdot-jp-sid-url shimbun sid))
		 (search-forward
		  shimbun-slashdot-jp-story-article-start-pattern nil t)
		 (setq begin (point))
		 (search-forward
		  shimbun-slashdot-jp-story-article-end-pattern nil t))
	(delete-region (match-beginning 0) (point-max))
	(delete-region (point-min) begin)
	(shimbun-slashdot-jp-make-article-after shimbun head)))))

(defun shimbun-slashdot-jp-make-comment-article (shimbun head sid cid)
  (with-temp-buffer
    (let (begin end)
      (when (and (shimbun-retrieve-url
		  (shimbun-slashdot-jp-sid-url shimbun sid))
		 (shimbun-slashdot-jp-search-comment-head shimbun sid cid)
		 (re-search-forward "<table[^>]*><tr><td[^>]*>" nil t))
	(delete-region (point-min) (match-beginning 0))
	(when (re-search-forward "</td></tr></table>" nil t)
	  (delete-region (point) (point-max))
	  (shimbun-slashdot-jp-make-article-after shimbun head))))))

(luna-define-method shimbun-headers ((shimbun shimbun-slashdot-jp)
				     &optional range)
  (cond
   ((eq range 'all) (setq range nil))
   ((eq range 'last) (setq range 1)))
  (let (headers)
    (catch 'range-check
      (dolist (head (shimbun-slashdot-jp-make-story-headers shimbun))
	(unless (shimbun-search-id shimbun (shimbun-header-id head))
	  (push head headers))
	(setq headers
	      (nconc
	       (shimbun-slashdot-jp-make-comment-headers
		shimbun
		(car (shimbun-slashdot-jp-extract-sid-and-cid
		      (shimbun-header-id head)))
		head)
	       headers))
	(and range
	     (= 0 (decf range))
	     (throw 'range-check headers)))
      headers)))

(defconst shimbun-slashdot-jp-month-alist
  '(("January" . 1)
    ("February" . 2)
    ("March" . 3)
    ("April" . 4)
    ("May" . 5)
    ("June" . 6)
    ("July" . 7)
    ("August" . 8)
    ("September" . 9)
    ("October" . 10)
    ("November" . 11)
    ("December" . 12)))

(defsubst shimbun-slashdot-jp-parse-date-string (time)
  (setq time (decode-time time))
  (when (looking-at
	 (eval-when-compile
	   (concat
	    " *[A-z]+ \\([A-z]+\\) \\([0-9]+\\), "
	    "@\\([0-9]+\\):\\([0-9]+\\)\\(AM\\|\\(PM\\)\\)")))
    (let ((month (cdr (assoc (match-string 1)
			     shimbun-slashdot-jp-month-alist)))
	  (day   (string-to-number (match-string 2)))
	  (hour  (string-to-number (match-string 3)))
	  (min   (string-to-number (match-string 4))))
      (when (match-beginning 6) ;; PM id found
	(setq hour (+ 12 hour)))
      (shimbun-make-date-string (if (< (nth 4 time) month)
				    (1+ (nth 5 time))
				  (nth 5 time))
				month day
				(format "%02d:%02d" hour min)))))

(defconst shimbun-slashdot-jp-story-head-start-pattern
  "<!-- start template: ID 45, storysearch;search;default -->")
(defconst shimbun-slashdot-jp-story-head-end-pattern
  "<!-- end template: ID 45, storysearch;search;default -->")

(luna-define-method shimbun-index-url ((shimbun shimbun-slashdot-jp))
  (shimbun-expand-url "search.pl" (shimbun-url-internal shimbun)))

(defun shimbun-slashdot-jp-make-story-headers (shimbun &optional sid)
  (let (headers begin)
    (with-temp-buffer
      (when (and (shimbun-retrieve-url (shimbun-index-url shimbun)
				       (not sid))
		 (search-forward
		  shimbun-slashdot-jp-story-head-start-pattern nil t)
		 (setq begin (point))
		 (search-forward
		  shimbun-slashdot-jp-story-head-end-pattern nil t))
	(delete-region (point) (point-max))
	(delete-region (point-min) begin)
	(goto-char (point-min))
	(while (re-search-forward
		(format "<a href=\"%s\\?sid=\\(%s\\)[^>]*>"
			(regexp-quote
			 (shimbun-slashdot-jp-article-url shimbun))
			(or sid "[/0-9]+"))
		nil t)
	  (let ((x (match-string 1))
		(head (shimbun-make-header)))
	    (shimbun-header-set-xref head
				     (shimbun-slashdot-jp-sid-url shimbun x))
	    (shimbun-header-set-id head
				   (shimbun-slashdot-jp-make-message-id x))
	    (when (looking-at "\\([^<]+\\)</a>")
	      (shimbun-header-set-subject head
					  (shimbun-mime-encode-string
					   (match-string 1))))
	    (forward-line 1)
	    (when (looking-at
		   "[ \t]+by \\([^ \t\r\f\n]+\\) with [0-9]+ comments <[^>]+>on ")
	      (shimbun-header-set-from head
				       (shimbun-mime-encode-string
					(match-string 1)))
	      (goto-char (match-end 0))
	      (when (setq x (shimbun-slashdot-jp-parse-date-string
			     (current-time)))
		(shimbun-header-set-date head x)
		(push head headers)))
	    (forward-line 1)))))
    (if sid (car headers) (nreverse headers))))

(defun shimbun-slashdot-jp-make-comment-headers (shimbun sid parent)
  (let (head headers)
    (with-temp-buffer
      (when (shimbun-retrieve-url
	     (shimbun-slashdot-jp-sid-url shimbun sid) t)
	(while (setq head
		     (shimbun-slashdot-jp-search-comment-head shimbun sid
							      nil parent))
	  (unless (shimbun-search-id shimbun (shimbun-header-id head))
	    (push head headers)))))
    headers))

(defun shimbun-slashdot-jp-search-comment-head (shimbun sid &optional
							cid parent)
  (when (re-search-forward
	 (format "<a name=\"%s\">" (or cid "\\([0-9]+\\)")) nil t)
    (let ((head (shimbun-make-header)))
      (unless cid
	(setq cid (match-string 1)))
      (shimbun-header-set-id head
			     (shimbun-slashdot-jp-make-message-id sid cid))
      (shimbun-header-set-xref head
			       (concat
				(shimbun-slashdot-jp-sid-url shimbun sid)
				"#" cid))
      (when (looking-at "<b>\\([^>]*\\)</b></a>")
	(shimbun-header-set-subject head
				    (shimbun-mime-encode-string
				     (match-string 1))))
      (when (search-forward "<font size=" nil t)
	(forward-line 1)
	(shimbun-header-set-from
	 head
	 (shimbun-mime-encode-string
	  (cond
	   ((looking-at "<a href=\"mailto:\\([^\"]*\\)\">\\([^<]*\\)")
	    (concat (match-string 2) " <" (match-string 1) ">"))
	   ((re-search-forward " *\\(.*\\) のコメント:" nil t)
	    (match-string 1))))))
      (forward-line 1)
      (when parent
	(shimbun-header-set-date head
				 (shimbun-slashdot-jp-parse-date-string
				  (w3m-time-parse-string
				   (shimbun-header-date parent)))))
      (forward-line 1)
      (let ((pos (point)))
	(when (re-search-forward
	       (format (eval-when-compile
			 (concat
			  "<a href=\"%scomments\\.pl\\?sid=[0-9]+&[^>]+"
			  "&cid=\\([0-9]+\\)\">親コメント</A>"))
		       (regexp-quote (shimbun-url-internal shimbun)))
	       nil t)
	  (shimbun-header-set-references
	   head
	   (if (equal "0" (setq cid (match-string 1)))
	       (shimbun-slashdot-jp-make-message-id sid)
	     (concat (shimbun-slashdot-jp-make-message-id sid)
		     " "
		     (shimbun-slashdot-jp-make-message-id sid cid)))))
	(goto-char pos))
      head)))

(provide 'sb-slashdot-jp)

;;; sb-slashdot-jp.el ends here

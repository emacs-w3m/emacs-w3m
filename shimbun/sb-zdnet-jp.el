;;; sb-zdnet-jp.el --- shimbun backend for ZDNet Japan -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2005 Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>

;; Author: Tsuyoshi CHO       <tsuyoshi_cho@ybb.ne.jp>
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

;; This code is based on sb-cnet-jp.el@ 2005-04-07.

;; Thanks.
;;  NAKAJIMA Mikio     <minakaji@namazu.org>,
;;  TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;  Katsumi Yamaoka    <yamaoka@jpl.org>

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)
(require 'sb-rss)
(require 'md5)

(luna-define-class shimbun-zdnet-jp (shimbun-japanese-newspaper shimbun-rss) ())

(defvar shimbun-zdnet-jp-group-alist
  '( ;; news
    ("news"          . "http://japan.zdnet.com/rss/news/index.rdf")
    ("news.network"  . "http://japan.zdnet.com/rss/news/nw/index.rdf")
    ("news.hardware" . "http://japan.zdnet.com/rss/news/hardware/index.rdf")
    ("news.software" . "http://japan.zdnet.com/rss/news/software/index.rdf")
    ("news.manage"   . "http://japan.zdnet.com/rss/news/itm/index.rdf")
    ("news.security" . "http://japan.zdnet.com/rss/news/sec/index.rdf")
    ("news.internet" . "http://japan.zdnet.com/rss/news/internet/index.rdf")
    ("news.os"       . "http://japan.zdnet.com/rss/news/os/index.rdf")
    ("news.db"       . "http://japan.zdnet.com/rss/news/db/index.rdf")
    ("news.system"   . "http://japan.zdnet.com/rss/news/devsys/index.rdf")

    ;; column
    ("column"        . "http://japan.zdnet.com/rss/column/index.rdf")

    ;; channel
    ("channel.security" . "http://japan.zdnet.com/rss/channel/sec/index.rdf")
    ))

(defvar shimbun-zdnet-jp-orphaned-group-list
  '())

(defvar shimbun-zdnet-jp-server-name "ZDNet Japan")
(defvar shimbun-zdnet-jp-content-start "<div class=\"leaf_body\">")
(defvar shimbun-zdnet-jp-content-end "<!-- *\\(/leaf_foot\\|/leaf_body\\|/main_left\\) *-->")
;; (defvar shimbun-zdnet-jp-x-face-alist
;;   '(("default" . "X-Face: 0p7.+XId>z%:!$ahe?x%+AEm37Abvn]n\
;; *GGh+>v=;[3`a{1lqO[$,~3C3xU_ri>[JwJ!9l0\n ~Y`b*eXAQ:*q=bBI\
;; _=ro*?]4:|n>]ZiLZ2LEo^2nr('C<+`lO~/!R[lH'N'4X&%\\I}8T!wt")))

(luna-define-method shimbun-groups ((shimbun shimbun-zdnet-jp))
  (nconc (mapcar 'car shimbun-zdnet-jp-group-alist)
	 shimbun-zdnet-jp-orphaned-group-list))

(luna-define-method shimbun-headers :around ((shimbun shimbun-zdnet-jp)
					     &optional range)
  (unless (member (shimbun-current-group shimbun)
		  shimbun-zdnet-jp-orphaned-group-list)
    (luna-call-next-method)))

(luna-define-method shimbun-index-url ((shimbun shimbun-zdnet-jp))
  (cdr (assoc (shimbun-current-group shimbun) shimbun-zdnet-jp-group-alist)))

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-zdnet-jp) url date)
  (if (string-match "http://japan\\.zdnet\\.com/\\([^/]+\\)/\
\\([^/]+\\)/\\([^/]+\\)/\\([,0-9]+\\)\\.htm" url) ;; \\?ref=rss do not need
      (shimbun-replace-in-string
       (concat "<"
	       (match-string-no-properties 4 url)
	       "%" (concat (match-string-no-properties 2 url)
			   "."
			   (match-string-no-properties 1 url))
	       "@japan.zdnet.com>")
       "," ".")
    (or (concat "<" (md5 url) "@japan.zdnet.com>")
	(error "Cannot find message-id base"))))

(defun shimbun-zdnet-jp-retrieve-next-pages (shimbun header base-cid url
						     &optional images)
  (let ((case-fold-search t)
	(next nil))
    (goto-char (point-min))
    ;; check next
    (when (re-search-forward "<a +href=\"\\([^\"]*\\)\"[^>]*>次のページ" nil t)
      (setq next (shimbun-expand-url (match-string 1) url)))
    (shimbun-clear-contents shimbun header)
    ;; remove page footer (last page is ignored)
    (goto-char (point-min))
    (when (and
	   (re-search-forward "<a [^>]+>\\(前\\|次\\)のページ</a" nil t)
	   (re-search-backward "<div class=\"leaf_body_page\">[ \t\r\n]*<ul>" nil t))
      (let ((start (match-beginning 0))
	    end)
	(if next
	    ;; isn't last
	    (delete-region start (point-max))
	  ;; last page
	  (when (re-search-forward "<div class=\"leaf_foot\">" nil t)
	    (setq end (match-end 0))
	    (delete-region start end))))
      (goto-char (point-min)))
    (insert "<html>\n<head>\n<base href=\"" url "\">\n</head>\n<body>\n")
    (goto-char (point-max))
    (unless next
      (insert (shimbun-footer shimbun header t)))
    (insert "\n</body>\n</html>\n")
    (when shimbun-encapsulate-images
      (setq images (shimbun-mime-replace-image-tags base-cid url images)))
    (let ((body (shimbun-make-text-entity "text/html" (buffer-string)))
	  (result (when next
		    (with-temp-buffer
		      (shimbun-fetch-url shimbun next)
		      (shimbun-zdnet-jp-retrieve-next-pages
		       shimbun header base-cid next images)))))
      (list (cons body (car result))
	    (or (nth 1 result) images)))))

(luna-define-method shimbun-make-contents ((shimbun shimbun-zdnet-jp) header)
  (let ((case-fold-search t)
	(base-cid (shimbun-header-id header)))
    ;; check author in leaf_head
    (goto-char (point-min))
    (if (and (re-search-forward "<div class=\"leaf_head\">" nil t)
	     (re-search-forward "<li>\\([^<]+\\)\\(<br[^>]*>\\([^<]+\\)\\)?\
</li>[ \n\r\t]*<li class=\"date\">" nil t))
	(shimbun-header-set-from header
				 (shimbun-mime-encode-string
				  (concat
				   (match-string-no-properties 1)
				   (if (match-string-no-properties 2)
				       " "
				     "")
				   (or (match-string-no-properties 3)
				       ""))))
      (shimbun-header-set-from header
			       (shimbun-mime-encode-string
				(concat shimbun-zdnet-jp-server-name
					" (" (shimbun-current-group-name shimbun) ")"))))
    (goto-char (point-min))
    (when (string-match "\\`<\\([^>]+\\)>\\'" base-cid)
      (setq base-cid (match-string 1 base-cid)))
    (let (body)
      (multiple-value-bind (texts images)
	  (shimbun-zdnet-jp-retrieve-next-pages shimbun header base-cid
						(shimbun-header-xref header))
	(erase-buffer)
	(if (= (length texts) 1)
	    (setq body (car texts))
	  (setq body (shimbun-make-multipart-entity))
	  (let ((i 0))
	    (dolist (text texts)
	      (setf (shimbun-entity-cid text)
		    (format "shimbun.%d.%s" (incf i) base-cid))))
	  (apply 'shimbun-entity-add-child body texts))
	(when images
	  (setf (shimbun-entity-cid body) (concat "shimbun.0." base-cid))
	  (let ((new (shimbun-make-multipart-entity)))
	    (shimbun-entity-add-child new body)
	    (apply 'shimbun-entity-add-child new
		   (mapcar 'cdr (nreverse images)))
	    (setq body new))))
      (shimbun-header-insert shimbun header)
      (insert "MIME-Version: 1.0\n")
      (shimbun-entity-insert body)))
  (buffer-string))

(luna-define-method shimbun-clear-contents :before
  ((shimbun shimbun-zdnet-jp) header)
  (shimbun-strip-cr)
  ;; remove advertisement <div class="ad.*"> - </div>
  (shimbun-remove-tags "<div +class=\"?ad" "</div>")
  ;; remove column <div class="pall5( bd1)"> - </div>
  (shimbun-remove-tags "<div +class=\"?pall5" "</div>")
  (shimbun-remove-tags "<script" "</script>")
  (shimbun-remove-tags "<noscript" "</noscript>"))

(luna-define-method shimbun-footer :around ((shimbun shimbun-zdnet-jp) header
					    &optional html)
  (if (string-match "news" (shimbun-current-group shimbun))
      (luna-call-next-method)
    ""))

(provide 'sb-zdnet-jp)

;;; sb-zdnet-jp.el ends here

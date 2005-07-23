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
;; Inc.; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; This code is based on sb-cnet-jp.el@ 2005-04-07.

;; Thanks.
;;  NAKAJIMA Mikio     <minakaji@namazu.org>,
;;  TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;  Katsumi Yamaoka    <yamaoka@jpl.org>

;;; Code:

(require 'shimbun)
(require 'sb-cnetnetworks)

(luna-define-class shimbun-zdnet-jp (shimbun-cnetnetworks) ())

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
    ("column"       . "http://japan.zdnet.com/rss/column/index.rdf")
    ("column.sp1" . "http://japan.zdnet.com/rss/column/sp1/index.rdf")
    ("column.netsecurity1"
     . "http://japan.zdnet.com/rss/column/netsecurity1/index.rdf")
    ("column.ea1" . "http://japan.zdnet.com/rss/column/ea1/index.rdf")
    ("column.btl" . "http://japan.zdnet.com/rss/column/btl/index.rdf")
    ("column.solutionIT"
     . "http://japan.zdnet.com/rss/column/solutionIT/index.rdf")

    ;; channel
    ("channel.security" . "http://japan.zdnet.com/rss/channel/sec/index.rdf")
    ("channel.ilm" . "http://japan.zdnet.com/rss/channel/ilm/index.rdf")

    ;; blog
    ("blog.iida" . "http://blog.japan.zdnet.com/iida/index.rdf")
    ("blog.mhatta" . "http://blog.japan.zdnet.com/mhatta/index.rdf")
    ("blog.kurei" . "http://blog.japan.zdnet.com/kurei/index.rdf")
    ("blog.opensource" . "http://blog.japan.zdnet.com/opensource/index.rdf")
    ("blog.soa"  . "http://blog.japan.zdnet.com/soa/index.rdf")
    ("blog.dp" . "http://blog.japan.zdnet.com/dp/index.rdf")))

(defvar shimbun-zdnet-jp-orphaned-group-list
  '())

(defvar shimbun-zdnet-jp-content-start
  "\\(<div class=\"leaf_body\">\\|<div class=\"leaf_body\">\\)")
(defvar shimbun-zdnet-jp-content-end
  "\\(<!-- *\\(/leaf_foot\\|/leaf_body\\|/main_left\\|\
NEWS LETTER SUB\\|ZD CAMPAIGN SUB\\) *-->\\)")

(defvar shimbun-zdnet-jp-server-name "CNET Networks,Inc.")
(defvar shimbun-zdnet-jp-x-face-alist
  '(("default" . "X-Face: 0p7.+XId>z%:!$ahe?x%+AEm37Abvn]n\
*GGh+>v=;[3`a{1lqO[$,~3C3xU_ri>[JwJ!9l0\n ~Y`b*eXAQ:*q=bBI\
_=ro*?]4:|n>]ZiLZ2LEo^2nr('C<+`lO~/!R[lH'N'4X&%\\I}8T!wt")))

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

(luna-define-method shimbun-rss-build-message-id :around
  ((shimbun shimbun-zdnet-jp) url date)
  (cond
   ((string-match "http://japan\\.zdnet\\.com/\\([^/]+\\)/\
\\([^/]+\\)/\\([^/]+\\)/\\([,0-9]+\\)\\.htm" url) ;; \\?ref=rss do not need
    (shimbun-replace-in-string
     (concat "<" (match-string-no-properties 4 url)
	     "%" (match-string-no-properties 2 url)
	     "." (match-string-no-properties 1 url)
	     "@japan.zdnet.com>")
     "," "."))
   (t
    (luna-call-next-method))))

(luna-define-method shimbun-cnetnetworks-clear-footer
  ((shimbun shimbun-zdnet-jp) header has-next)
  (goto-char (point-min))
  (when (and
	 (re-search-forward
	  "<a [^>]+>\\(前\\|次\\)のページ</a" nil t)
	 (re-search-backward
	  "<div class=\"leaf_body_page\">[ \t\r\n]*<ul>" nil t))
    (let ((start (match-beginning 0))
	  end)
      (if has-next
	  ;; isn't last
	  (delete-region start (point-max))
	;; last page
	(when (re-search-forward "<div class=\"leaf_foot\">" nil t)
	  (setq end (match-end 0))
	  (delete-region start end))))))

(luna-define-method shimbun-cnetnetworks-header-reconfig :around
  ((shimbun shimbun-zdnet-jp) header)
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
    (luna-call-next-method)))

(luna-define-method shimbun-footer :around ((shimbun shimbun-zdnet-jp)
					    header &optional html)
  (if (string-match "news" (shimbun-current-group shimbun))
      (luna-call-next-method)
    ""))

(provide 'sb-zdnet-jp)

;;; sb-zdnet-jp.el ends here

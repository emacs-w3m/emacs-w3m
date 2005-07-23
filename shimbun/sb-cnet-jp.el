;;; sb-cnet-jp.el --- shimbun backend for CNET Japan -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2003, 2004, 2005 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio     <minakaji@namazu.org>,
;;         TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Katsumi Yamaoka    <yamaoka@jpl.org>,
;;         Tsuyoshi CHO       <tsuyoshi_cho@ybb.ne.jp>
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

;;; Code:

(require 'shimbun)
(require 'sb-cnetnetworks)

(luna-define-class shimbun-cnet-jp (shimbun-cnetnetworks) ())

(defvar shimbun-cnet-jp-group-alist
  '(("news"	     . "http://japan.cnet.com/rss/index.rdf")
    ("blog.kenn"     . "http://blog.japan.cnet.com/kenn/index.rdf")
    ("blog.sakamoto" . "http://blog.japan.cnet.com/sakamoto/index.rdf")
    ("blog.lessig"   . "http://blog.japan.cnet.com/lessig/index.rdf")
    ("blog.watanabe" . "http://blog.japan.cnet.com/watanabe/index.rdf")
    ("blog.editors"  . "http://blog.japan.cnet.com/editors/index.rdf")))

(defvar shimbun-cnet-jp-orphaned-group-list
  '("blog.inoue"
    "blog.mori"
    "blog.umeda"))

(defvar shimbun-cnet-jp-content-start "<div class=\"leaf_body\">")
(defvar shimbun-cnet-jp-content-end
  "<!-- *\\(NEWS LETTER SUB\\|ZD CAMPAIGN SUB\\) *-->")

(defvar shimbun-cnet-jp-server-name "CNET Networks,Inc.")
(defvar shimbun-cnet-jp-x-face-alist
  '(("default" . "X-Face: 0p7.+XId>z%:!$ahe?x%+AEm37Abvn]n\
*GGh+>v=;[3`a{1lqO[$,~3C3xU_ri>[JwJ!9l0\n ~Y`b*eXAQ:*q=bBI\
_=ro*?]4:|n>]ZiLZ2LEo^2nr('C<+`lO~/!R[lH'N'4X&%\\I}8T!wt")))

(luna-define-method shimbun-groups ((shimbun shimbun-cnet-jp))
  (nconc (mapcar 'car shimbun-cnet-jp-group-alist)
	 shimbun-cnet-jp-orphaned-group-list))

(luna-define-method shimbun-headers :around ((shimbun shimbun-cnet-jp)
					     &optional range)
  (unless (member (shimbun-current-group shimbun)
		  shimbun-cnet-jp-orphaned-group-list)
    (luna-call-next-method)))

(luna-define-method shimbun-index-url ((shimbun shimbun-cnet-jp))
  (cdr (assoc (shimbun-current-group shimbun) shimbun-cnet-jp-group-alist)))

(luna-define-method shimbun-rss-build-message-id :around
  ((shimbun shimbun-cnet-jp) url date)
  (if (or
       ;; For news group
       (string-match "http://japan\\.cnet\\.com/\
\\(.+\\)/\\([,0-9]+\\)\\.htm\\(\\?ref=rss\\)?" url)
       ;; For blog group
       (string-match "http://blog\\.japan\\.cnet\\.com/\
\\([^/]+\\)/archives/\\([0-9]+\\)\\.html" url))
      (concat "<"
	      (shimbun-replace-in-string
	       (match-string-no-properties 2 url) "," ".")
	      "%" (shimbun-current-group shimbun) "@japan.cnet.com>")
    (luna-call-next-method)))

(luna-define-method shimbun-cnetnetworks-clear-footer
  ((shimbun shimbun-cnet-jp) header has-next)
  ;; remove page footer (last page is ignored)
  (goto-char (point-min))
  (when (re-search-forward "| [0-9]+ / [0-9]+ |" nil t)
    (if has-next
	;; isn't last
	(progn
	  (goto-char (match-end 0))
	  ;; "<a href...>前の..." or "| 1 "
	  (re-search-backward "\\(<a\\|| 1 \\)" nil t)
	  (delete-region (match-beginning 0) (point-max)))
      ;; last page
      (let ((end (match-end 0)))
	(goto-char end)
	(re-search-backward "<a" nil t) ;; "<a href...>前の..."
	(delete-region (match-beginning 0) end)))))

(luna-define-method shimbun-footer :around ((shimbun shimbun-cnet-jp)
					    header &optional html)
  (if (string-match "news" (shimbun-current-group shimbun))
      (luna-call-next-method)
    ""))

(provide 'sb-cnet-jp)

;;; sb-cnet-jp.el ends here

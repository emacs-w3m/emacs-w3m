;;; sb-cnet-jp.el --- shimbun backend for CNET Japan -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2003, 2004, 2005 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio     <minakaji@namazu.org>,
;;         TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Katsumi Yamaoka    <yamaoka@jpl.org>,
;;         Tsuyoshi CHO       <mfalcon_sky@emailuser.net>
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

(eval-when-compile
  (require 'cl))

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-cnet-jp (shimbun-japanese-newspaper shimbun-rss) ())

(defvar shimbun-cnet-jp-group-alist
  '(("news"	     . "http://japan.cnet.com/rss/index.rdf")
    ("blog.kenn"     . "http://blog.japan.cnet.com/kenn/index.rdf")
    ("blog.lessig"   . "http://blog.japan.cnet.com/lessig/index.rdf")
    ("blog.watanabe" . "http://blog.japan.cnet.com/watanabe/index.rdf")
    ("blog.editors"  . "http://blog.japan.cnet.com/editors/index.rdf")))

(defvar shimbun-cnet-jp-orphaned-group-list
  '("blog.inoue"
    "blog.mori"
    "blog.umeda"))

(defvar shimbun-cnet-jp-server-name "CNET Japan")
(defvar shimbun-cnet-jp-from-address  "webmaster@japan.cnet.com")
(defvar shimbun-cnet-jp-content-start "<div class=\"leaf_body\">")
(defvar shimbun-cnet-jp-content-end "<!--NEWS LETTER SUB-->")
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

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-cnet-jp) url date)
  (if (or
       ;; For news group
       (string-match "http://japan\\.cnet\\.com/\
\\(.+\\)/\\([,0-9]+\\)\\.htm\\?ref=rss" url)
       ;; For blog group
       (string-match "http://blog\\.japan\\.cnet\\.com/\
\\([^/]+\\)/archives/\\([0-9]+\\)\\.html" url))
      (concat "<"
	      (shimbun-replace-in-string
	       (match-string-no-properties 2 url) "," ".")
	      "%" (shimbun-current-group shimbun) "@japan.cnet.com>")
    (error "Cannot find message-id base")))

(defun shimbun-cnet-jp-clean-text-page ()
  (let ((case-fold-search t) (start))
    (goto-char (point-min))
    (when (and (search-forward shimbun-cnet-jp-content-start nil t)
	       (setq start (match-end 0))
	       (re-search-forward shimbun-cnet-jp-content-end nil t))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start)
      (goto-char (point-min))
      )))

(defun shimbun-cnet-jp-retrieve-next-pages (shimbun header base-cid url
						    &optional images)
  (let ((case-fold-search t) (next))
    (goto-char (point-min))
    (when (re-search-forward
	   "<a +href=\"\\([^\"]*\\)\"[^>]*>次のページ" nil t)
      (setq next (shimbun-expand-url (match-string 1) url)))
    (shimbun-cnet-jp-clean-text-page)
    (goto-char (point-min))
    ;; remove page footer (last page is ignored)
    (when (re-search-forward "| [0-9]+ / [0-9]+ |" nil t)
      (if next
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
	  (delete-region (match-beginning 0) end))))
    (goto-char (point-min))
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
		      (shimbun-clear-contents shimbun header)
		      (shimbun-cnet-jp-retrieve-next-pages
		       shimbun header base-cid next images)))))
      (list (cons body (car result))
	    (or (nth 1 result) images)))))

(luna-define-method shimbun-make-contents ((shimbun shimbun-cnet-jp) header)
  (let ((case-fold-search t))
    (shimbun-clear-contents shimbun header)
    (let ((base-cid (shimbun-header-id header)))
      (when (string-match "\\`<\\([^>]+\\)>\\'" base-cid)
	(setq base-cid (match-string 1 base-cid)))
      (let (body)
	(multiple-value-bind (texts images)
	    (shimbun-cnet-jp-retrieve-next-pages shimbun header base-cid
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
    (buffer-string)))

(luna-define-method shimbun-clear-contents :before
  ((shimbun shimbun-cnet-jp) header)
  (shimbun-strip-cr)
  ;; remove advertisement <div class="ad.*"> - </div>
  (shimbun-remove-tags "<div +class=\"?ad" "</div>")
  ;; remove column <div class="pall5( bd1)"> - </div>
  (shimbun-remove-tags "<div +class=\"?pall5" "</div>")
  (shimbun-remove-tags "<script" "</script>")
  (shimbun-remove-tags "<noscript" "</noscript>"))

(luna-define-method shimbun-footer :around ((shimbun shimbun-cnet-jp) header
					    &optional html)
  (if (string= "news" (shimbun-current-group shimbun))
      (luna-call-next-method)
    ""))

(provide 'sb-cnet-jp)

;;; sb-cnet-jp.el ends here

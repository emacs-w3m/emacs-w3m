;;; sb-cnetnetworks.el --- shimbun superclass for CNET Networks,Inc. Japan -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2005 Tsuyoshi CHO  <tsuyoshi_cho@ybb.ne.jp>

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

;;; Based on sb-cnet-jp.el

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-cnetnetworks
		   (shimbun-japanese-newspaper shimbun-rss) ())

(defvar shimbun-cnetnetworks-server-name "CNET Networks,Inc.")

(luna-define-generic shimbun-cnetnetworks-clear-footer
  (shimbun header has-next)
  "Remove footer by page.")

(luna-define-generic shimbun-cnetnetworks-header-reconfig (shimbun header)
  "HEADER reconfig by ARTICLE page.")

(luna-define-method shimbun-cnetnetworks-header-reconfig
  ((shimbun shimbun-cnetnetworks) header)
  (shimbun-header-set-from header
			   (shimbun-mime-encode-string
			    (format "%s (%s@%s)"
				    shimbun-cnetnetworks-server-name
				    (shimbun-current-group shimbun)
				    (shimbun-server shimbun)))))

(defun shimbun-cnetnetworks-retrieve-next-pages (shimbun header base-cid url
							 &optional images)
  (let ((case-fold-search t) (next))
    (goto-char (point-min))
    ;; check next page
    (when (re-search-forward
	   "<a +href=\"\\([^\"]*\\)\"[^>]*>次のページ" nil t)
      (setq next (shimbun-expand-url (match-string 1) url)))
    ;; clear contents
    (shimbun-clear-contents shimbun header)
    ;; clear footer(ex.relative link list and other)
    (shimbun-cnetnetworks-clear-footer shimbun header next)
    (goto-char (point-min))
    (insert "<html>\n<head>\n<base href=\"" url "\">\n</head>\n<body>\n")
    (goto-char (point-max))
    ;; when last page, insert shimbun-newspaper-jp footer
    (unless next
      (insert (shimbun-footer shimbun header t)))
    (insert "\n</body>\n</html>\n")
    (when shimbun-encapsulate-images
      (setq images (shimbun-mime-replace-image-tags base-cid url images)))
    (let ((body (shimbun-make-text-entity "text/html" (buffer-string)))
	  (result (when next
		    (with-temp-buffer
		      (shimbun-fetch-url shimbun next)
		      (shimbun-cnetnetworks-retrieve-next-pages
		       shimbun header base-cid next images)))))
      (list (cons body (car result))
	    (or (nth 1 result) images)))))

(luna-define-method shimbun-make-contents
  ((shimbun shimbun-cnetnetworks) header)
  (let ((case-fold-search t)
	(base-cid (shimbun-header-id header)))
    (shimbun-cnetnetworks-header-reconfig shimbun header)
    (goto-char (point-min))
    (when (string-match "\\`<\\([^>]+\\)>\\'" base-cid)
      (setq base-cid (match-string 1 base-cid)))
    (let (body)
      (multiple-value-bind (texts images)
	  (shimbun-cnetnetworks-retrieve-next-pages
	   shimbun header base-cid (shimbun-header-xref header))
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
  ((shimbun shimbun-cnetnetworks) header)
  (shimbun-strip-cr)
  ;; remove advertisement <div class="ad.*"> - </div>
  (shimbun-remove-tags "<div +class=\"?ad" "</div>")
  ;; remove column <div class="pall5( bd1)"> - </div>
  (shimbun-remove-tags "<div +class=\"?pall5" "</div>")
  (shimbun-remove-tags "<script" "</script>")
  (shimbun-remove-tags "<noscript" "</noscript>"))

(provide 'sb-cnetnetworks)

;;; sb-cnet-jp.el ends here

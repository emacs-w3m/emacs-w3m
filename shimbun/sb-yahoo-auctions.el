;;; sb-yahoo-auctions.el --- shimbun backend for Yahoo! AUCTIONS

;; Copyright (C) 2005  ARISAWA Akihiro <ari@mbf.sphere.ne.jp>

;; Author: ARISAWA Akihiro <ari@mbf.sphere.ne.jp>
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

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-yahoo-auctions (shimbun-rss) ())

(defcustom shimbun-yahoo-auctions-group-alist nil
  "*An alist of Yahoo! AUCTIONS group definition.
Each element looks like (NAME URL).
NAME is a shimbun group name.
URL is the URL for category or search result."
  :group 'shimbun
  :type '(repeat (cons :format "%v"
		       (string :tag "Group name")
		       (string :tag "URL"))))

(defvar shimbun-yahoo-auctions-content-start "<hr size=1 noshade>")
(defvar shimbun-yahoo-auctions-content-end
  "<table CELLPADDING=\"2\" CELLSPACING=\"0\" BGCOLOR=\"#666666\" BORDER=\"0\">")

(luna-define-method shimbun-groups ((shimbun shimbun-yahoo-auctions))
  (mapcar 'car shimbun-yahoo-auctions-group-alist))

(luna-define-method shimbun-group-p ((shimbun shimbun-yahoo-auctions) group)
  t)

(luna-define-method shimbun-from-address ((shimbun shimbun-yahoo-auctions))
  (format "Yahoo!オークション (%s)" (shimbun-current-group shimbun)))

(luna-define-method shimbun-index-url ((shimbun shimbun-yahoo-auctions))
  (let* ((group (shimbun-current-group shimbun))
	 (elem (assoc group shimbun-yahoo-auctions-group-alist)))
    (if elem
	(cdr elem)
      (concat "http://search.auctions.yahoo.co.jp/search_rss?p=" 
	      (shimbun-url-encode-string group 'euc-japan)))))

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-yahoo-auctions) url date)
  (unless (string-match "\\([^/]+\\)$" url)
    (error "Cannot find message-id base"))
  (format "<%s@auctions.yahoo.co.jp>" (match-string 1 url)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-yahoo-auctions)
					 &optional range)
  (static-when (featurep 'xemacs)
    ;; It's one of many bugs in XEmacs that the coding systems *-dos
    ;; provided by Mule-UCS don't convert CRLF to LF when decoding.
    (shimbun-strip-cr))
  (let ((xml (condition-case err
		 (xml-parse-region (point-min) (point-max))
	       (error
		(message "Error while parsing %s: %s"
			 (shimbun-index-url shimbun)
			 (error-message-string err))
		nil)))
	rss-ns headers)
    (when xml
      (setq rss-ns (shimbun-rss-get-namespace-prefix
		    xml "http://purl.org/rss/1.0/"))
      (dolist (item (shimbun-rss-find-el (intern (concat rss-ns "item")) xml))
	(let ((url (and (listp item)
			(eq (intern (concat rss-ns "item")) (car item))
			(shimbun-rss-node-text rss-ns 'link (cddr item)))))
	  (when url
	    (let* ((date (shimbun-rss-node-text rss-ns 'pubDate item))
		   (id (shimbun-rss-build-message-id shimbun url date))
		   (description
		    (shimbun-rss-node-text rss-ns 'description item)))
	      (push (shimbun-create-header
		     0
		     (shimbun-rss-node-text rss-ns 'title item)
		     (shimbun-from-address shimbun)
		     (shimbun-rss-process-date shimbun date)
		     id "" 0 0 url
		     (when description
		       (list (cons 'description description))))
		    headers)))))
      (nreverse headers))))

(provide 'sb-yahoo-auctions)
;;; sb-yahoo-auctions.el ends here

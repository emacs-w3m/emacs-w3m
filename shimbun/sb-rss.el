;;; sb-rss.el --- shimbun backend for RSS (Rich Site Summary).

;; Copyright (C) 2003 Koichiro Ohba <koichiro@meadowy.org>
;;               2003 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: Koichiro Ohba <koichiro@meadowy.org>
;;         NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
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
  (require 'cl)
  (require 'static))

(require 'shimbun)
(eval-when-compile
  (ignore-errors
    (require 'xml)))
(eval '(require 'xml))

(luna-define-class shimbun-rss (shimbun) ())

(luna-define-generic shimbun-rss-process-date (shimbun-rss date)
  "Process DATE string and return proper Date string to show it in MUA.")

(luna-define-method shimbun-rss-process-date ((shimbun shimbun-rss) date)
  ;; make Date string from ISO 8601 date format.  See
  ;; http://www.w3.org/TR/NOTE-datetime.
  ;;    Year:
  ;;       YYYY (eg 1997)
  ;;    Year and month:
  ;;       YYYY-MM (eg 1997-07)
  ;;    Complete date:
  ;;       YYYY-MM-DD (eg 1997-07-16)
  ;;    Complete date plus hours and minutes:
  ;;       YYYY-MM-DDThh:mmTZD (eg 1997-07-16T19:20+01:00)
  ;;    Complete date plus hours, minutes and seconds:
  ;;       YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30+01:00)
  ;;    Complete date plus hours, minutes, seconds and a decimal fraction of a
  ;; second
  ;;       YYYY-MM-DDThh:mm:ss.sTZD (eg 1997-07-16T19:20:30.45+01:00)
  ;; where:
  ;;      YYYY = four-digit year
  ;;      MM   = two-digit month (01=January, etc.)
  ;;      DD   = two-digit day of month (01 through 31)
  ;;      hh   = two digits of hour (00 through 23) (am/pm NOT allowed)
  ;;      mm   = two digits of minute (00 through 59)
  ;;      ss   = two digits of second (00 through 59)
  ;;      s    = one or more digits representing a decimal fraction of a second
  ;;      TZD  = time zone designator (Z or +hh:mm or -hh:mm)
  ;;
  ;;      YYYY = four-digit year
  ;;      MM   = two-digit month (01=January, etc.)
  ;;      DD   = two-digit day of month (01 through 31)
  ;;      hh   = two digits of hour (00 through 23) (am/pm NOT allowed)
  (if (or (null date)
	  (not (string-match
		"\\([0-9][0-9][0-9][0-9]\\)\\(-[0-9][0-9]\\)?\\(-[0-9][0-9]\\)?T?\\([0-9][0-9]:[0-9][0-9]\\(:[.0-9]+\\)?\\)?\\(\\+[0-9][0-9]:?[0-9][0-9]\\|Z\\)?"
		date)))
      ""
    (let ((year  (match-string-no-properties 1 date))
	  (month (match-string-no-properties 2 date))
	  (day   (match-string-no-properties 3 date))
	  (time  (match-string-no-properties 4 date))
	  (zone  (match-string-no-properties 6 date)))
      (shimbun-make-date-string
       (string-to-number year)
       (if month (string-to-number (substring month 1)) 1)
       (if day (string-to-number (substring day 1)) 1)
       (or time "00:00")
       (when zone
	 (if (string-equal zone "Z")
	     "+0000"
	   (if (string-match ":" zone)
	       (concat (substring zone 0 (match-beginning 0))
		       (substring zone (match-end 0)))
	     zone)))))))

(luna-define-generic shimbun-rss-get-date (shimbun-rss url)
  "Process URL and return a Date string for an article of the URL.
When a RSS file does not contain any date information for each article,
but you can identify it from the URL, define this method in a backend.")

(luna-define-method shimbun-rss-get-date ((shimbun shimbun-rss) url)
  nil)

(luna-define-generic shimbun-rss-build-message-id (shimbun-rss url date)
  "Build unique message-id from URL and DATE and return it.")

(luna-define-method shimbun-headers ((shimbun shimbun-rss) &optional range)
  (with-temp-buffer
    (let ((case-fold-search t))
      (shimbun-retrieve-url
       (shimbun-index-url shimbun) 'no-cache 'no-decode)
      (goto-char (point-min))
      (decode-coding-region (point-min) (point-max) (shimbun-rss-get-encoding))
      (set-buffer-multibyte t)
      (shimbun-get-headers shimbun range))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-rss)
					 &optional range)
  (static-when (featurep 'xemacs)
    ;; It's one of many bugs in XEmacs that the coding systems *-dos
    ;; provided by Mule-UCS don't convert CRLF to LF when decoding.
    (shimbun-strip-cr))
  (let* ((xml (xml-parse-region (point-min) (point-max)))
	 (dc-ns (shimbun-rss-get-namespace-prefix
		 xml "http://purl.org/dc/elements/1.1/"))
	 (rss-ns (shimbun-rss-get-namespace-prefix
		  xml "http://purl.org/rss/1.0/"))
	 (author)
	 (headers))
    (setq author
	  (catch 'found-author
	    (dolist (channel
		     (shimbun-rss-find-el (intern (concat rss-ns "channel"))
					  xml))
	      (throw 'found-author
		     (or
		      (shimbun-rss-node-text rss-ns 'author channel)
		      (shimbun-rss-node-text dc-ns 'creator channel)
		      (shimbun-rss-node-text dc-ns 'contributor channel))))))
    (dolist (item (shimbun-rss-find-el (intern (concat rss-ns "item")) xml))
      (let ((url (and (listp item)
		      (eq (intern (concat rss-ns "item")) (car item))
		      (shimbun-rss-node-text rss-ns 'link (cddr item)))))
	(when url
	  (let* ((date (or (shimbun-rss-get-date shimbun url)
			   (shimbun-rss-node-text dc-ns 'date item)
			   (shimbun-rss-node-text rss-ns 'pubDate item)))
		 (id (shimbun-rss-build-message-id shimbun url date)))
	    (unless (shimbun-search-id shimbun id)
	      (push (shimbun-create-header
		     0
		     (shimbun-rss-node-text rss-ns 'title item)
		     (or (shimbun-rss-node-text rss-ns 'author item)
			 (shimbun-rss-node-text dc-ns 'creator item)
			 (shimbun-rss-node-text dc-ns 'contributor item)
			 author
			 (shimbun-from-address shimbun))
		     (shimbun-rss-process-date shimbun date)
		     id "" 0 0 url)
		    headers))))))
    headers))

;;; Internal functions

;;; XML functions

(defun shimbun-rss-get-encoding ()
  (let (end encoding)
    (cond
     ((search-forward "<?" nil t nil)
      (let ((pos (point)))
	(setq end (search-forward "?>"))
	(goto-char pos))
      (setq encoding
	    (if (re-search-forward "encoding=\"\\([^ ]+\\)\"" end t)
		(downcase (match-string-no-properties 1))
	      "utf-8")))
     (t ;; XML Default encoding.
      (setq encoding "utf-8")))
    (intern-soft (concat encoding "-dos"))))

(defun shimbun-rss-node-text (namespace local-name element)
  (let* ((node (assq (intern (concat namespace (symbol-name local-name)))
		     element))
	 (text (if (and node (listp node))
		   (shimbun-rss-node-just-text node)
		 node))
	 (cleaned-text (if text (shimbun-replace-in-string
				 text "^[\000-\037\177]+\\|^ +\\| +$" ""))))
    (if (string-equal "" cleaned-text)
	nil
      cleaned-text)))

(defun shimbun-rss-node-just-text (node)
  (if (and node (listp node))
      (mapconcat 'shimbun-rss-node-just-text (cddr node) " ")
    node))

(defun shimbun-rss-find-el (tag data &optional found-list)
  "Find the all matching elements in the data.  Careful with this on
large documents!"
  (if (listp data)
      (mapcar (lambda (bit)
		(if (car-safe bit)
		    (progn (if (equal tag (car bit))
			       (setq found-list
				     (append found-list
					     (list bit))))
			   (if (and (listp (car-safe (caddr bit)))
				    (not (stringp (caddr bit))))
			       (setq found-list
				     (append found-list
					     (shimbun-rss-find-el
					      tag (caddr bit))))
			     (setq found-list
				   (append found-list
					   (shimbun-rss-find-el
					    tag (cddr bit))))))))
	      data))
  found-list)

(defun shimbun-rss-get-namespace-prefix (el uri)
  "Given EL (containing a parsed element) and URI (containing a string
that gives the URI for which you want to retrieve the namespace
prefix), return the prefix.
See http://feeds.archive.org/validator/docs/howto/declare_namespaces.html
for more RSS namespaces."
  (let* ((prefix (car (rassoc uri (cadar el))))
	 (nslist (if prefix
		     (split-string (symbol-name prefix) ":")))
	 (ns (cond ((eq (length nslist) 1) ; no prefix given
		    "")
		   ((eq (length nslist) 2) ; extract prefix
		    (cadr nslist)))))
    (if (and ns (not (equal ns "")))
	(concat ns ":")
      ns)))

(provide 'sb-rss)

;; end of sb-rss.el

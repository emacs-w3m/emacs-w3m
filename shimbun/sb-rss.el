;;; sb-rss.el --- shimbun backend for RSS (Rich Site Summary).

;; Copyright (C) 2003, 2004, 2005 Koichiro Ohba <koichiro@meadowy.org>
;; Copyright (C) 2003, 2004, 2005 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

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

(autoload 'timezone-parse-date "timezone")

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

  ;; In addition to the above, it also supports the date format in the
  ;; RFC822 style which RSS 2.0 allows.
;;;<DEBUG>
;;  (shimbun-rss-process-date-1 date))
;;
;;(defun shimbun-rss-process-date-1 (date)
;;;</DEBUG>
  (let (vector year month day time zone)
    (cond ((null date))
	  ((string-match " [0-9]+ " date)
	   (setq vector (timezone-parse-date date)
		 year (string-to-number (aref vector 0)))
	   (when (>= year 1970)
	     (setq month (string-to-number (aref vector 1))
		   day   (string-to-number (aref vector 2))
		   time  (aref vector 3))
	     (when (setq zone (aref vector 4))
	       (unless (string-match "\\`[A-Z+-]" zone)
		 (setq zone nil)))))
	  ((string-match
	    "\\([0-9][0-9][0-9][0-9]\\)\\(-[0-9][0-9]\\)?\\(-[0-9][0-9]\\)?T?\\([0-9][0-9]:[0-9][0-9]\\(:[.0-9]+\\)?\\)?\\([-+][0-9][0-9]:?[0-9][0-9]\\|Z\\)?"
	    date)
	   (setq year  (string-to-number (match-string 1 date))
		 month (if (match-beginning 2)
			   (string-to-number (substring (match-string 2 date)
							1))
			 1)
		 day   (if (match-beginning 3)
			   (string-to-number (substring (match-string 3 date)
							1))
			 1)
		 time  (or (match-string-no-properties 4 date) "00:00")
		 zone  (match-string-no-properties 6 date))
	   (when zone
	     (cond ((null zone))
		   ((string-equal zone "Z")
		    (setq zone "+0000"))
		   ((string-match ":" zone)
		    (setq zone (concat (substring zone 0 (match-beginning 0))
				       (substring zone (match-end 0)))))))))
    (if month
	(shimbun-make-date-string year month day time zone)
      "")))

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
  (shimbun-rss-get-headers shimbun range t))

(defun shimbun-rss-get-headers (shimbun &optional range
					need-descriptions need-all-items)
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
	dc-ns rss-ns author headers)
    (when xml
      (setq dc-ns (shimbun-rss-get-namespace-prefix
		   xml "http://purl.org/dc/elements/1.1/")
	    rss-ns (shimbun-rss-get-namespace-prefix
		    xml "http://purl.org/rss/1.0/")
	    author
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
	      (when (or need-all-items
			(not (shimbun-search-id shimbun id)))
		(push (shimbun-create-header
		       0
		       (shimbun-rss-node-text rss-ns 'title item)
		       (or (shimbun-rss-node-text rss-ns 'author item)
			   (shimbun-rss-node-text dc-ns 'creator item)
			   (shimbun-rss-node-text dc-ns 'contributor item)
			   author
			   (shimbun-from-address shimbun))
		       (shimbun-rss-process-date shimbun date)
		       id "" 0 0 url
		       (when need-descriptions
			 (let ((description (shimbun-rss-node-text
					     rss-ns 'description item)))
			   (when description
			     (list (cons 'description description))))))
		      headers))))))
      headers)))

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
				 text "^[ \000-\037\177]+\\|[ \000-\037\177]+$" ""))))
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

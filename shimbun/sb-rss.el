;;; sb-rss.el --- shimbun backend for RSS (Rich Site Summary).

;; Copyright (C) 2003 Koichiro Ohba <koichiro@meadowy.org>
;;               2003 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: Koichiro Ohba <koichiro@meadowy.org>
;;         NAKAJIMA Mikio <minakaji@namazu.org>
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

(eval-when-compile (require 'cl))

(require 'shimbun)
(eval-when-compile
  (ignore-errors
    (require 'xml)))
(eval '(require 'xml))

(luna-define-class shimbun-rss (shimbun) ())

(luna-define-generic shimbun-rss-process-date (shimbun-rss date)
  "Process DATE string and return proper Date string for showing it in MUA.")

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
  (let (year month day minutes timezone)
    (if (or (null date) (not (string-match
	      "\\([0-9][0-9][0-9][0-9]\\)\\(-[0-9][0-9]\\)?\\(-[0-9][0-9]\\)?T?\\([0-9][0-9]:[0-9][0-9]\\(:[.0-9]+\\)?\\)?\\(\\+[0-9][0-9]:[0-9][0-9]\\|Z\\)?"
	      date)))
	""
      (setq year (match-string-no-properties 1 date)
	    month (match-string-no-properties 2 date)
	    day (match-string-no-properties 3 date)
	    minutes (match-string-no-properties 4 date)
	    timezone (match-string-no-properties 6 date))
      (unless month (setq month "01"))
      (unless day (setq day "01"))
      (when (and month (string-match "^-" month))
	(setq month (substring month 1)))
      (when (and day (string-match "^-" day))
	(setq day (substring day 1)))
      (when timezone
	(if (string-equal timezone "Z")
	    (setq timezone "+0000")
	  (if (string-match ":" timezone)
	      (setq timezone (concat (substring timezone 0 (match-beginning 0))
				     (substring timezone (match-end 0)))))))
      (setq year (string-to-number year)
	    month (string-to-number month)
	    day (string-to-number day))
      (shimbun-make-date-string year month day minutes timezone))))

(luna-define-generic shimbun-rss-build-message-id (shimbun-rss url)
  "Building unique message-id from URL and return it.")

(luna-define-method shimbun-headers ((shimbun shimbun-rss) &optional range)
  (with-temp-buffer
    (let ((case-fold-search t)
	  encoding)
      (shimbun-retrieve-url
       (shimbun-index-url shimbun) 'no-cache 'no-decode)
      (goto-char (point-min))
      (setq encoding (intern-soft (concat
				   (downcase (shimbun-rss-get-encoding)) "-dos")))
      (decode-coding-region (point-min) (point-max) encoding)
      (set-buffer-multibyte t)
      (shimbun-get-headers shimbun range))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-rss)
					 &optional range)
  (let* ((xml (xml-parse-region (point-min) (point-max)))
	 (dc-ns (shimbun-rss-get-namespace-prefix
		 xml "http://purl.org/dc/elements/1.1/"))
	 (rss-ns (shimbun-rss-get-namespace-prefix
		  xml "http://purl.org/rss/1.0/"))
         ;;(rdf-ns (shimbun-rss-get-namespace-prefix
         ;;         xml "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
         ;;(content-ns (shimbun-rss-get-namespace-prefix
         ;;             xml "http://purl.org/rss/1.0/modules/content/"))
	 headers from subject date id url ; extra
	 )
    (dolist (item (nreverse (shimbun-rss-find-el (intern (concat rss-ns "item")) xml)))
      (catch 'next
	(when (and (listp item)
		   (eq (intern (concat rss-ns "item")) (car item))
		   (setq url (shimbun-rss-node-text rss-ns 'link (cddr item))))
	  (setq id (shimbun-rss-build-message-id shimbun url))
	  (when (shimbun-search-id shimbun id)
	    (throw 'next nil))
	  (setq subject (shimbun-rss-node-text rss-ns 'title item))
          ;;(setq extra (or (shimbun-rss-node-text content-ns 'encoded item)
          ;;                (shimbun-rss-node-text rss-ns 'description item)))
	  (setq from (or (shimbun-rss-node-text rss-ns 'author item)
			 (shimbun-rss-node-text dc-ns 'creator item)
			 (shimbun-from-address shimbun)))
	  (setq date (or (shimbun-rss-node-text dc-ns 'date item)
			 (shimbun-rss-node-text rss-ns 'pubDate item)))
	  (setq date (shimbun-rss-process-date shimbun date))
	  (push (shimbun-make-header
		 0
		 (shimbun-mime-encode-string subject)
		 (shimbun-mime-encode-string from)
		 date id "" 0 0 url)
		headers))))
    (nreverse headers)))

;;; Internal functions

;;; XML functions

(defun shimbun-rss-get-encoding ()
  (let (encoding end)
    (cond
     ((search-forward "<?")
      (let (pos)
	(setq pos (point))
	(setq end (search-forward "?>"))
	(goto-char pos))
      (if (re-search-forward "encoding=\"\\(.+\\)\"" end t)
	  (setq encoding (match-string-no-properties 1))
	(setq encoding "utf-8")))
     (t	;; XML Default encoding.
      (setq encoding "utf-8")
      ))
    encoding))

(defun shimbun-rss-node-text (namespace local-name element)
  (let* ((node (assq (intern (concat namespace (symbol-name local-name)))
		     element))
	 (text (if (and node (listp node))
		   (shimbun-rss-node-just-text node)
		 node))
	 (cleaned-text (if text (shimbun-rss-replace-in-string
				 text "^[\000-\037\177]+\\|^ +\\| +$" ""))))
    (if (string-equal "" cleaned-text)
	nil
      cleaned-text)))

(defun shimbun-rss-node-just-text (node)
  (if (and node (listp node))
      (mapconcat 'shimbun-rss-node-just-text (cddr node) " ")
    node))

(defun shimbun-rss-replace-in-string (string regexp newtext &optional literal)
  (let ((start 0) tail)
    (while (string-match regexp string start)
      (setq tail (- (length string) (match-end 0)))
      (setq string (replace-match newtext nil literal string))
      (setq start (- (length string) tail)))
    string))

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

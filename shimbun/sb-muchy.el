;;; sb-muchy.el --- shimbun backend for Muchy's Palmware Review! -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
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

(luna-define-class shimbun-muchy (shimbun) ())

(defvar shimbun-muchy-url "http://muchy.com")
(defvar shimbun-muchy-groups '("review"))
(defvar shimbun-muchy-from-address "webmaster@muchy.com")
(defvar shimbun-muchy-coding-system 'japanese-shift-jis-unix)
(defvar shimbun-muchy-content-start "\n<table border=0 width=100% cellspacing=0 cellpadding=0>")
(defvar shimbun-muchy-content-end
  "<\/td><\/tr><\/table><div align=center><p><a href=\"/index.html\">\\[HOME\\]<\/a>")

(defvar shimbun-muchy-expiration-days 31)

(defsubst shimbun-muchy-parse-time (str)
  (save-match-data
    (if (string-match "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)" str)
	(list (string-to-number (match-string 1 str))
	      (string-to-number (match-string 2 str))
	      (string-to-number (match-string 3 str))))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-muchy)
					 &optional range)
  (let ((url (concat (shimbun-url-internal shimbun) "/" "whatsold.html"))
	case-fold-search date-list headers)
    (catch 'stop
      (subst-char-in-region (point-min) (point-max) ?\t ?  t)
      (goto-char (point-min))
      (when (re-search-forward
	     "<table width=\"100%\" border=\"0\">"
	     nil t 2)
	(delete-region (point-min) (point)))
      (unless (search-forward
	       "<a href=\"\/history\/whatsold.html\">[これより前の更新履歴]</a>"
	       nil t nil)
	(throw 'stop nil))
      (beginning-of-line)
      (while (re-search-backward
	      "<font color=white>▲</font></a> <font color=white><span class=quotation>\\([0-9][0-9][0-9][0-9]/[0-9]+/[0-9]+\\)</span></font> <a href=\"#1\"><font color=white>▼</font>"
	      nil t nil)
	(setq date-list (cons (match-string 1) date-list)))
      (setq headers (shimbun-muchy-get-headers shimbun headers date-list)
	    date-list nil)
      (erase-buffer)
      (shimbun-retrieve-url url 'reload)
      (subst-char-in-region (point-min) (point-max) ?\t ?  t)
      (goto-char (point-min))
      (save-excursion
	(while (re-search-forward
		"<p><a name=\"\\([0-9][0-9][0-9][0-9]/[0-9][0-9]*/[0-9][0-9]*\\)\"></a></p>"
		nil t nil)
	  (setq date-list (cons (match-string 1) date-list))))
      (setq date-list (nreverse date-list))
      (setq headers (shimbun-muchy-get-headers shimbun headers date-list)))
  headers))

(defun shimbun-muchy-get-headers (shimbun headers date-list)
  (let* ((from (shimbun-from-address shimbun))
	 (group (shimbun-current-group-internal shimbun))
	 (baseurl (shimbun-url-internal shimbun))
	 case-fold-search date)
    (catch 'stop
      (while (and date-list
		  (search-forward
		   (format
		    "<font color=white><span class=quotation>%s</span></font>"
		    (car date-list))
		   nil t nil))
	;; getting DATE
	(setq date (apply 'shimbun-make-date-string
			  (shimbun-muchy-parse-time (car date-list))))
	(let (
	      ;; defining region to work
	      (beg (point))
	      (end (save-excursion
		     (or (and (nth 1 date-list)
			      (search-forward
			       (format
				"<font color=white><span class=quotation>%s</span></font>"
				(nth 1 date-list)) nil t nil)
			      (progn (beginning-of-line) (point)))
			 (point-max))))
	      innerend star id url subject)
	  ;; getting URL and SUBJECT
	  (while (re-search-forward
		  "<img src=\"/\\(new\\|revise\\|update\\)\\.png\""
		  end t nil)
	    (catch 'next
	      (setq subject (upcase (match-string 1))
		    innerend (save-excursion
			       (or
				(and
				 (re-search-forward
				  "<img src=\"/\\(new\\|revise\\|update\\)\\.png\""
				  end t nil)
				 (goto-char (match-beginning 0))
				 (point))
				end)))
	      (unless (re-search-forward
		       "<strong><a href=\"\\(/.+\\.html.*\\)\">\\(.+\\)</a></strong>"
		       innerend t nil)
		(throw 'next nil))
	      (setq url (match-string 1)
		    subject (concat (match-string 2) "/" subject))
	      (if (re-search-forward
		   "<img src=\"/hoshi\\([0-9]\\)\\.png\""
		   innerend t nil)
		  (setq star (string-to-number (match-string 1)))
		(setq star nil))
	      ;; adding license fee to subject
	      (if (re-search-forward
		   "価格; \\(<a href=\"[^<>]+\">\\)*<font color=\"#[0-9A-Z]+\">\\(標準添付\\|標準搭載\\|プレゼント\\|.*ウェア.*\\|[$\\\\][,.0-9]+\\).*</font>"
		   innerend t nil)
		  (setq subject (concat subject "/" (match-string 2))))
	      (when (and star (> star 0))
		(setq subject (concat
			       subject
			       "/"
			       (make-string star (string-to-char "★")))))
	      ;; building ID
	      (setq id (format "<%08d@%s.%s%%muchy.com>"
			       (string-to-number
				(mapconcat
				 'number-to-string
				 (shimbun-muchy-parse-time (car date-list))
				 ""))
			       (if (string-match "\\([^\/]+\\)\\.html" url)
				   (match-string 1 url)
				 url)
			       group))
	      (if (shimbun-search-id shimbun id)
		  (throw 'stop nil))
	      (setq url (concat baseurl url))
	      (push (shimbun-make-header
		     0 (shimbun-mime-encode-string subject)
		     from date id "" 0 0 url)
		    headers)
	      (goto-char innerend)
	      (beginning-of-line)))
	  (setq date-list (cdr date-list))
	  (delete-region beg end)
	  (goto-char end))))
    headers))

(provide 'sb-muchy)

;;; sb-muchy.el ends here

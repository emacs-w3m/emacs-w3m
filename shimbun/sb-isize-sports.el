;;; sb-isize-sports.el --- shimbun backend for www.isize.com/sports  -*- coding: iso-2022-7bit -*-

;; Author: MIYOSHI Masanori <miyoshi@boreas.dti.ne.jp>

;; Keywords: news

;;; Copyright:

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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(require 'shimbun)
(require 'sb-text)

(luna-define-class shimbun-isize-sports (shimbun shimbun-text) ())

(defvar shimbun-isize-sports-url "http://www.isize.com/sports/")
(defvar shimbun-isize-sports-group-alist
  '(("football" "football"
     "<TD[^>]*>&nbsp;&nbsp;<A href=\"\\(news_vivid_N\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9][0-9]\\)\\.html\\)\"><FONT[^>]*>\\([^<]+\\)</FONT></A></TD>"
     "[\\s ]*<FONT face=\"Osaka,ＭＳ Ｐゴシック\" size=\"3\">" "</FONT>\n")
;    ("baseball" "bb" "" "" "") ; NG
    ("f1" "f1"
     "<TD[^>]*>&nbsp;&nbsp;<A href=\"\\(news_detail_N\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9][0-9]\\)\\.html\\)\"[^>]*><FONT[^>]*>\\([^<]*\\)</FONT></A></TD>"
     "<P class=\"news\">" "</P>")
    ("mlb" "mlb"
     "<TD[^>]*>[^<]*<A href=\"\\(news_detail_N\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9][0-9]\\)\\.html\\)\"[^>]*><FONT[^>]*>\\([^<]+\\)</FONT></A></TD>"
     "<FONT face=\"ＭＳ Ｐゴシック,Osaka\" class=\"newsM\">"
     "</FONT>")
    ("nba" "nba"
     "<TD[^>]*>&nbsp;&nbsp;<A href=\"\\(news_detail_N\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9][0-9]\\)\\.html\\)\"[^>]*><FONT[^>]*>\\([^<]+\\)</FONT></A></TD>"
     "<P class=\"news\">" "</P>")
    ("nfl" "american/nfl"
     "<TD[^>]*><A href=\"\\(N\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9][0-9]\\)\\.html\\)\"[^>]*><FONT[^>]*>\\([^<]+\\)</FONT></A></TD>"
     "<!--# NEWS_DETAIL_MESSAGE START #-->"
     "<!--# NEWS_DETAIL_MESSAGE END #-->")
    ("x-league" "american/x"
     "<TD[^>]*><A href=\"\\(N\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9][0-9]\\)\\.html\\)\"[^>]*><FONT[^>]*>\\([^<]+\\)</FONT></A></TD>"
     "<!--# NEWS_DETAIL_MESSAGE START #-->"
     "<!--# NEWS_DETAIL_MESSAGE END #-->")
    ("keiba" "dabipara"
     "<TD[^>]*>&nbsp;&nbsp;<A href=\"\\(news_\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9][0-9]\\)\\.html\\)\"[^>]*>\\([^<]+\\)</A></TD>"
     "<P class=\"news\">" "</P>")
    ("golf" "golf"
     "<TD[^>]*><A href=\"\\(news_\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9][0-9]\\)\\.html\\)\"[^>]*>\\([^<]+\\)</A></TD>"
     "<P class=\"news\">" "</P>")
;    ("fishing" "fishing" "" "" "") ; NG
;    ("ski" "ski" nil nil nil) ; NG (not available now!)
    ("table-tennis" "tt"
     "<TD[^>]*>&nbsp;&nbsp;<A href=\"[^<]+/\\(news_\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9][0-9]\\)\\.html\\)\"[^>]+><FONT[^>]+>\\([^<]+\\)</FONT></A></TD>"
     "<P class=\"news\"><BR><FONT face=\"ＭＳ Ｐゴシック,Osaka\">" "</FONT>")))
(defvar shimbun-isize-sports-groups
  (mapcar 'car shimbun-isize-sports-group-alist))
(defvar shimbun-isize-sports-from-address "http://www.isize.com/sports/")
(defvar shimbun-isize-sports-max-pages 2)

(luna-define-method shimbun-index-url ((shimbun shimbun-isize-sports))
  (concat (shimbun-url-internal shimbun)
	  (nth 1 (assoc (shimbun-current-group-internal shimbun)
			shimbun-isize-sports-group-alist))
	  "/news/news.html"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-isize-sports)
					 &optional range)
  (let ((case-fold-search t)
	(pages (shimbun-header-index-pages range))
	(count 1)
	(regexp (nth 2 (assoc (shimbun-current-group-internal shimbun)
			      shimbun-isize-sports-group-alist)))
	headers)
    (goto-char (point-min))
    (catch 'stop
      (while t
	(while (re-search-forward regexp nil t)
	  (let ((url (match-string 1))
		(year (match-string 2))
		(month (match-string 3))
		(day (match-string 4))
		(no (match-string 5))
		(subject (match-string 6))
		date id)
	    (setq year (if (eq (length year) 2)
			   (concat "20" year)
			 year))
	    (setq id (format "<%s%s%s%s.%s.sports@www.isize.ne.jp>"
			     year month day no
			     (shimbun-current-group-internal shimbun)))
	    (if (shimbun-search-id shimbun id)
		(throw 'stop nil))
	    (setq date (shimbun-make-date-string
			(string-to-number year)
			(string-to-number month)
			(string-to-number day)))
	    (push (shimbun-make-header
		   0
		   (shimbun-mime-encode-string subject)
		   (shimbun-from-address-internal shimbun)
		   date id "" 0 0
		   (concat
		    (shimbun-url-internal shimbun)
		    (nth 1 (assoc (shimbun-current-group-internal shimbun)
				  shimbun-isize-sports-group-alist))
		    "/news/"
		    url))
		  headers)))

	(setq count (1+ count))
	(unless (and (if pages (<= count pages) t)
		     (<= count shimbun-isize-sports-max-pages))
	  (throw 'stop nil))
	(if (re-search-forward "<A href=\"\\([^\"]+\\)\"[^>]*>\\(<B>\\)?前の[0-9]+件\\(</B>\\)?</a>" nil t)
	    (let ((url (concat
			(shimbun-url-internal shimbun)
			(nth 1 (assoc (shimbun-current-group-internal shimbun)
				      shimbun-isize-sports-group-alist))
			"/news/"
			(match-string 1))))
	      (erase-buffer)
	      (shimbun-retrieve-url url t)
	      (goto-char (point-min)))
	  (throw 'stop nil))))
    headers))

(luna-define-method shimbun-make-contents :around ((shimbun shimbun-isize-sports)
						   &optional header)
  (let ((entry (assoc (shimbun-current-group-internal shimbun)
		      shimbun-isize-sports-group-alist)))
    (shimbun-set-content-start-internal shimbun (nth 3 entry))
    (shimbun-set-content-end-internal shimbun (nth 4 entry))
    (luna-call-next-method)))

(provide 'sb-isize-sports)

;;; sb-isize-sports.el ends here

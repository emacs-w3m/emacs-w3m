;;; sb-x51.el --- shimbun backend for x51.org -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2004 Tsuyoshi CHO <mfalcon21@hotmail.com>

;; Author: Tsuyoshi CHO <mfalcon21@hotmail.com>
;; Keywords: news blog
;; Created: Feb 21, 2004

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
(require 'sb-rss)

(luna-define-class shimbun-x51 (shimbun-rss) ())

(defvar shimbun-x51-url "http://x51.org/")
(defvar shimbun-x51-group-alist
  '(("top"        . "index.rdf") ;; Top-RDF
    ("edge"       . "x/edge.php")
    ("oparts"     . "x/oparts.php")
    ("love"       . "x/love.php")
    ("blow"       . "x/blow.php")
    ("ufo"        . "x/ufo.php")
    ("media"      . "x/media.php")
    ("crime"      . "x/crime.php")
    ("disaster"   . "x/disaster.php")
    ("life"       . "x/life.php")
    ("homme"      . "x/homme.php")
    ("science"    . "x/science.php")
    ("medical"    . "x/medical.php")
    ("uma"        . "x/uma.php")
    ("auction"    . "x/auction.php")
    ("xfiles"     . "x/xfiles.php")
    ("phallic"    . "x/phallic.php")
    ("art"        . "x/art.php")
    ("military"   . "x/military.php")
    ("religion"   . "x/religion.php")
    ("psychics"   . "x/psychics.php")
    ("info"       . "x/info.php")
    ("cabal"      . "x/cabal.php")
    ("northkorea" . "x/northkorea.php")))
(defvar shimbun-x51-server-name "x51.org")
(defvar shimbun-x51-from-address "webmaster@x51.org")
(defvar shimbun-x51-auther "X51")
(defvar shimbun-x51-coding-system 'utf-8)
(defvar shimbun-x51-content-start
  "<div class=\"individualbody\">")
(defvar shimbun-x51-content-end
  "<span class=\"posted\">")

;; X-Face create from banner
(defvar shimbun-x51-x-face-alist
  '(("default" ."X-Face: \"15Ng%Hp0)P[AP!e1^W3KhKMVHC*AcXANx^CaW]\
!dzRSN]tO68A5{`1RzK`g+0Yo$0q2RFM\n 7m?9-o[R6ou-[9X$JI1HYc>A-a[+DGgI")))

(luna-define-method shimbun-groups ((shimbun shimbun-x51))
  (mapcar 'car shimbun-x51-group-alist))

(defmacro shimbun-x51-concat-url (shimbun url)
  (` (concat (shimbun-url-internal (, shimbun))
	     (cdr (assoc (shimbun-current-group-internal (, shimbun))
			 shimbun-x51-group-alist))
	     (, url))))

(luna-define-method shimbun-index-url ((shimbun shimbun-x51))
  (shimbun-x51-concat-url shimbun ""))

(luna-define-method shimbun-rss-build-message-id ((shimbun shimbun-x51)
						  url date)
		    (unless (string-match "http://[^\/]+/x/\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\).php" url)
		      (error "Cannot find message-id base"))
		    (format "<%s%s%s@x51.org>"
			    (match-string-no-properties 1 url)
			    (match-string-no-properties 2 url)
			    (match-string-no-properties 3 url)))

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-x51)
						 &optional range)
  (with-temp-buffer
    (shimbun-retrieve-url (shimbun-index-url shimbun))
    (let ((case-fold-search nil) url headers)
      ;(message "group %s" (shimbun-groups shimbun)) ;; debug
      (if (string-match "top" (car (assoc (shimbun-current-group-internal shimbun)
					  shimbun-x51-group-alist)))
	  (progn
	    (luna-call-next-method)) ;; call parent method
	(progn
	  (let (beg end)
	    ;; buffer narrowing
	    ;; <!--CategoryArcivesList--> (start)
	    ;; <div class="pages"> (end)
	    (goto-char (point-min))
	    (search-forward "<!--CategoryArcivesList-->")
	    (setq beg (point))
	    (search-forward "<div class=\"pages\">")
	    (setq end (point))
	    ;(message "begin %d / end %d" beg end)
	    (goto-char beg)
	    (save-excursion
	      (narrow-to-region beg end)
	      ;; get header source
	      (goto-char (point-min))
	      (let (title url date id)
		(while (re-search-forward
			"<a ?href=\"http://x51\\.org/x/\\([0-9]*\\)/\\([0-9]*\\)/\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\.php\">\\([^<]*\\)</a>"
			nil t)
		  (setq url (shimbun-expand-url (concat
						 "http://x51.org/x/"
						 (match-string 1) "/"
						 (match-string 2) "/"
						 (match-string 3) (match-string 4)
						 ".php")
						(shimbun-index-url shimbun))) ; url
		  (setq title (match-string 5)) ; title
		  (setq date  (shimbun-make-date-string
			       (string-to-int (match-string 1))
			       (string-to-int (match-string 2))
			       (string-to-int (match-string 3)))) ; date
		  (setq id (shimbun-rss-build-message-id shimbun url date)) ; id
		  ;; create header & push header
		  ;; from sb-rss
		  (push (shimbun-create-header
			 0
			 title
			 (shimbun-from-address shimbun)
			 date
			 id "" 0 0 url)
			headers))))
	    headers))))))

;; date normalize
(defun shimbun-x51-prepare-article (shimbun header)
  "Prepare an article: adjusting a date header if there is a correct
information available."
  (let* ((case-fold-search t)
	 (start (re-search-forward shimbun-x51-content-start nil t))
	 (end (and start
		   (re-search-forward shimbun-x51-content-end nil t)
		   (prog1
		       (match-beginning 0)
		     (goto-char start)))))
    (when (re-search-forward
	   "Posted by[^|]*| ?\\([0-9]*\\)\\(&#24180;\\|年\\)\\([0-9]*\\)\\(&#26376;\\|月\\)\\([0-9]*\\)\\(&#26085;\\|日\\) ?\\([012][0-9]:[0-5][0-9]\\)"
	   ;; Posted by : X51 | 2004&#24180;05&#26376;22&#26085; 23:15 年月日
	   nil t)
      (shimbun-header-set-date
       header
       (shimbun-make-date-string
	(string-to-number (match-string 1))
	(string-to-number (match-string 3))
	(string-to-number (match-string 5))
	(match-string 7)))
      (goto-char (point-min)))))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-x51)
						   header)
  (shimbun-x51-prepare-article shimbun header))

(provide 'sb-x51)

;;; sb-x51.el ends here

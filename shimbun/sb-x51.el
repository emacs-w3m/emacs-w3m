;;; sb-x51.el --- shimbun backend for x51.org -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2004, 2005 Tsuyoshi CHO <mfalcon21@hotmail.com>

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

(eval-when-compile (require 'cl))
(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-x51 (shimbun-rss) ())

(defvar shimbun-x51-url "http://x51.org/")
(defvar shimbun-x51-group-alist
  '(("top"        . "index.rdf") ;; Top-RDF
    ("art"        . "x/art.php")
    ("auction"    . "x/auction.php")
    ("blow"       . "x/blow.php")
    ("cabal"      . "x/cabal.php")
    ("crime"      . "x/crime.php")
    ("disaster"   . "x/disaster.php")
    ("edge"       . "x/edge.php")
    ("homme"      . "x/homme.php")
    ("info"       . "x/info.php")
    ("life"       . "x/life.php")
    ("love"       . "x/love.php")
    ("media"      . "x/media.php")
    ("medical"    . "x/medical.php")
    ("military"   . "x/military.php")
    ("news"       . "x/news.php")
    ("northkorea" . "x/northkorea.php")
    ("oparts"     . "x/oparts.php")
    ("phallic"    . "x/phallic.php")
    ("psychic"    . "x/psychic.php")
    ("religion"   . "x/religion.php")
    ("science"    . "x/science.php")
    ("story"      . "x/story.php")
    ("ufo"        . "x/ufo.php")
    ("uma"        . "x/uma.php")
    ("xfiles"     . "x/xfiles.php")))

(defvar shimbun-x51-server-name "x51.org")
(defvar shimbun-x51-from-address "webmaster@x51.org")
(defvar shimbun-x51-auther "X51")
(defvar shimbun-x51-coding-system 'utf-8)
(defvar shimbun-x51-content-start "<!-- Article -->")
(defvar shimbun-x51-content-end "<!---/ Article --->")

;; X-Face create from banner
(defvar shimbun-x51-x-face-alist
  '(("default" ."X-Face: \"15Ng%Hp0)P[AP!e1^W3KhKMVHC*AcXANx^CaW]\
!dzRSN]tO68A5{`1RzK`g+0Yo$0q2RFM\n 7m?9-o[R6ou-[9X$JI1HYc>A-a[+DGgI")))

(luna-define-method shimbun-groups ((shimbun shimbun-x51))
  (mapcar 'car shimbun-x51-group-alist))

(defmacro shimbun-x51-concat-url (shimbun url)
  `(concat (shimbun-url-internal ,shimbun)
	   (cdr (assoc (shimbun-current-group-internal ,shimbun)
		       shimbun-x51-group-alist))
	   ,url))

(luna-define-method shimbun-index-url ((shimbun shimbun-x51))
  (shimbun-x51-concat-url shimbun ""))

(luna-define-method shimbun-rss-build-message-id ((shimbun shimbun-x51)
						  url date)
  (unless (string-match
	   "http://[^\/]+/x/\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\).php"
	   url)
    (error "Cannot find message-id base"))
  (format "<%s%s%s@x51.org>"
	  (match-string-no-properties 1 url)
	  (match-string-no-properties 2 url)
	  (match-string-no-properties 3 url)))

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-x51)
						 &optional range)
  (let* ((case-fold-search t)
	 (url (shimbun-index-url shimbun))
	 headers)
    (if (string-match "top" (car (assoc (shimbun-current-group-internal
					 shimbun)
					shimbun-x51-group-alist)))
	(luna-call-next-method)	;; call parent method
      (let* ((pages (shimbun-header-index-pages range))
	     (beg (point-min))
	     (end (point-max))
	     indexes)
	(push (concat url "?page=1") indexes) ;; push page 1
	(when (if pages (< 1 pages) t)
	  (goto-char (point-min))
	  (when (search-forward "<div class=\"middlebar\">" nil t)
	    (setq beg (point)))
	  (when (search-forward "</div>" nil t)
	    (setq end (point)))
	  (save-excursion
	    (narrow-to-region beg end)
	    (goto-char (point-min))
	    (let* ((count 2))
	      (while (and (if pages (<= count pages) t)
			  (re-search-forward
			   (format "<a href=\"[^?]*\\?page=%d\"" count)
			   nil t))
		;; push linked for page 2-end
		(push (format "%s%s%d" url "?page=" count) indexes)
		(incf count)))
	    (widen)))
	(setq indexes (nreverse indexes))
	(catch 'stop
	  (dolist (index indexes)
	    (erase-buffer)
	    (shimbun-retrieve-url index t) ;; retrieve target page
	    (goto-char (point-min))
	    (setq beg (point-min)
		  end (point-max))
	    (when (re-search-forward "<!-- *top article *-->" nil t)
	      (setq beg (match-end 0)))
	    (when (re-search-forward "<!--/ *middle bar *-->" nil t)
	      (setq end (match-beginning 0)))
	    (save-excursion
	      (narrow-to-region beg end)
	      ;; get header source
	      (goto-char (point-min))
	      (let (title url date id)
		(while (re-search-forward
			"<a +href=\"http://x51\\.org/x/\
\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\.php\"\
 +class=\"title[^\"]*\">\\([^<]*\\)</a>"
			nil t)
		  (setq url (shimbun-expand-url
			     (concat
			      "http://x51.org/x/"
			      (match-string 1) "/"
			      (match-string 2) "/"
			      (match-string 3) (match-string 4)
			      ".php")
			     (shimbun-index-url shimbun)))
		  (setq title (match-string 5))
		  (setq date  (shimbun-make-date-string
			       (string-to-int (match-string 1))
			       (string-to-int (match-string 2))
			       (string-to-int (match-string 3))))
		  (setq id (shimbun-rss-build-message-id shimbun url date))
		  ;; check old id
		  (when (shimbun-search-id shimbun id)
		    (throw 'stop nil))
		  ;; create header & push header
		  (push (shimbun-create-header
			 0
			 title
			 (shimbun-from-address shimbun)
			 date
			 id "" 0 0 url)
			headers))
		(widen)))))
	headers))))

;; normalize date
(defun shimbun-x51-prepare-article (shimbun header)
  "Adjust a date header if there's a correct information available."
  (let* ((case-fold-search t)
	 (start (re-search-forward shimbun-x51-content-start nil t))
	 (end (and start
		   (re-search-forward shimbun-x51-content-end nil t)
		   (prog1
		       (match-beginning 0)
		     (goto-char start)))))
    ;; Posted by : X51 | 2004&#24180;05&#26376;22&#26085; 23:15 年月日
    (when (re-search-forward
	   "Posted by[^|]*\|\
 ?\\([0-9]*\\)\\(&#24180;\\|年\\)\
\\([0-9]*\\)\\(&#26376;\\|月\\)\
\\([0-9]*\\)\\(&#26085;\\|日\\)\
 ?\\([012][0-9]:[0-5][0-9]\\)"
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

(luna-define-method shimbun-clear-contents :before ((shimbun shimbun-x51)
						    header)
  (shimbun-strip-cr)
  (shimbun-remove-tags "<script" "</script>")
  (shimbun-remove-tags "<noscript" "</noscript>")
  (shimbun-remove-tags "<div class=\"notes\"" "</div>")
  (shimbun-remove-tags "<div class=\"line\"" "</div>")
  (shimbun-remove-tags "<div class=\"middlebar\"" "</div>"))

(provide 'sb-x51)

;;; sb-x51.el ends here

;;; sb-slashdot-jp.el --- shimbun backend for slashdot.jp

;; Copyright (C) 2001 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
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
(luna-define-class shimbun-slashdot-jp (shimbun) ())

(defvar shimbun-slashdot-jp-groups '("story"))
(defvar shimbun-slashdot-jp-url "http://slashdot.jp/")

(defvar shimbun-slashdot-jp-content-start
  "cellpadding=\"5\" cellspacing=\"0\">\n?<tr><td bgcolor=\"#FFFFFF\"")
(defvar shimbun-slashdot-jp-content-end "</td>")

(defvar shimbun-slashdot-jp-months
  '("December" "November" "October" "September" "August" "July" "June" "May"
    "April" "March" "February" "January"))

(defvar shimbun-slashdot-jp-comments-per-story 3)

(defvar shimbun-slashdot-jp-story-max-pages 10
  "*Max page number of slashdot.jp's story pages.
One page contains 30 stories.")

(defvar shimbun-slashdot-jp-comment-max-pages 30
  "*Max page number of slashdot.jp's comment pages.
One page contains 30 comments.")

(luna-define-method shimbun-headers ((shimbun shimbun-slashdot-jp)
				     &optional range)
  (let* ((case-fold-search t)
	 (pages (min (or (shimbun-header-index-pages range)
			 shimbun-slashdot-jp-story-max-pages)
		     shimbun-slashdot-jp-story-max-pages))
	 year month mday uniq subject from pos hour min refs count
	 id cid pid headers)
    (catch 'stop
      (setq count 0)
      ;; comments
      (while (< count (* pages shimbun-slashdot-jp-comments-per-story))
	(with-current-buffer (shimbun-retrieve-url-buffer
			      (concat shimbun-slashdot-jp-url
				      (format
				       "search.pl?op=comments&start=%d"
				       (* 30 count)))
			      'reload)
	  (goto-char (point-min))
	  (while (re-search-forward "<A HREF=\".*/comments.pl\\?sid=\\(\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)/[^\"]+\\)" nil t)
	    (setq year (string-to-number (match-string 2))
		  uniq (match-string 1) refs nil)
	    (when (string-match "\\([0-9]+\\)#\\([0-9]+\\)$" uniq)
	      (setq pid (substring uniq (match-beginning 1) (match-end 1))
				cid (substring uniq (match-beginning 2) (match-end 2)))
	      (when (string-match "^[^&]+" uniq)
			(setq refs (concat (substring uniq 0 (match-end 0))
						(unless (string= "0" pid) (concat "&cid=" pid))))
			(setq uniq (concat (substring uniq 0 (match-end 0))
						"&cid=" cid))))
	    (setq id (format "<%s@slashdot.jp>" uniq))
	    (if (shimbun-search-id shimbun id)
		(throw 'stop nil))
	    (when (search-forward ">" nil t)
	      (setq pos (match-end 0))
	      (search-forward "<")
	      (setq subject (buffer-substring pos (match-beginning 0))))
	    (when (re-search-forward
		   "<a href=\"mailto:\\([^>]*\\)\">\\([^<]*\\)</a>" nil t)
	      (setq from
		    (if (zerop (length (match-string 1)))
			(concat (match-string 2))
		      (concat (match-string 2) " <" (match-string 1) ">"))))
	    (when (re-search-forward
		   "[A-z][a-z]+day \\([A-Z][a-z]+\\) \\([0-9]+\\)" nil t)
	      (setq month (length
			   (member (match-string 1) shimbun-slashdot-jp-months)))
	      (setq mday (string-to-number (match-string 2))))
	    (when (re-search-forward "@\\([0-9]+\\):\\([0-9]+\\)\\(AM\\|PM\\)"
				     nil t)
	      (setq hour (string-to-number (match-string 1)))
	      (if (string= (match-string 3) "PM")
		  (setq hour (+ 12 hour)))
	      (setq min (string-to-number (match-string 2))))
		(unless refs
		  (when (re-search-forward
			 "<A HREF=\".*/article.pl\\?sid=\\([0-9]+/[0-9]+/[0-9]+/[0-9]+\\)"
			   nil t)
			(setq refs (match-string 1))))
	    (push (shimbun-make-header
		   0
		   (shimbun-mime-encode-string subject)
		   (shimbun-mime-encode-string from)
		   (shimbun-make-date-string year month mday
					     (format "%02d:%02d" hour min))
		   id
		   (format "<%s@slashdot.jp>" refs)
		   0 0 (concat
			(shimbun-url-internal shimbun)
			"comments.pl?sid=" uniq))
		  headers)))
	(setq count (1+ count))))
    (catch 'stop
      ;; main strories
      (setq count 0)
      (while (< count pages)
	(with-current-buffer (shimbun-retrieve-url-buffer
			      (concat shimbun-slashdot-jp-url
				      (format "search.pl?start=%d"
					      (* 30 count)))
			      'reload)
	  (goto-char (point-min))
	  (while (re-search-forward "<A HREF=\".*/article.pl\\?sid=\\(\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)/[0-9]+\\)" nil t)
	    (setq year (string-to-number (match-string 2))
		  uniq (match-string 1))
	    (setq id (format "<%s@slashdot.jp>" uniq))
	    (if (shimbun-search-id shimbun id)
		(throw 'stop nil))
	    (when (search-forward ">" nil t)
	      (setq pos (match-end 0))
	      (search-forward "<")
	      (setq subject (buffer-substring pos (match-beginning 0))))
	    (when (re-search-forward "- \\([A-Za-z-]+\\)，" nil t)
	      (setq from (match-string 1)))
	    (when (re-search-forward
		   "[A-z][a-z]+day \\([A-Z][a-z]+\\) \\([0-9]+\\)" nil t)
	      (setq month (length
			   (member (match-string 1) shimbun-slashdot-jp-months)))
	      (setq mday (string-to-number (match-string 2))))
	    (when (re-search-forward "@\\([0-9]+\\):\\([0-9]+\\)\\(AM\\|PM\\)" nil t)
	      (setq hour (string-to-number (match-string 1)))
	      (if (string= (match-string 3) "PM")
		  (setq hour (+ 12 hour)))
	      (setq min (string-to-number (match-string 2))))
	    (push (shimbun-make-header
		   0
		   (shimbun-mime-encode-string subject)
		   from
		   (shimbun-make-date-string year month mday
					     (format "%02d:%02d" hour min))
		   id
		   "" 0 0 (concat
			   (shimbun-url-internal shimbun)
			   "article.pl?sid=" uniq))
		  headers)))
	(setq count (1+ count))))
    headers))

(defsubst shimbun-slashdot-jp-make-contents (shimbun header)
  (let ((case-fold-search t)
	start num charset)
    (when (progn
	    ;; for comments.
	    (and (string-match "cid=\\([0-9]+\\)$"
			       (shimbun-header-xref header))
		 (setq num (substring
			    (shimbun-header-xref header)
			    (match-beginning 0)))
		 (re-search-forward (concat ">#" num "</A>")
				    nil t))
	    ;; content beginning.
	    (and (re-search-forward (shimbun-content-start-internal shimbun)
				    nil t)
		 (search-forward ">" nil t)
		 (setq start (match-end 0))
		 (re-search-forward (shimbun-content-end-internal shimbun)
				    nil t)))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start)
      ;; eol conversion.
      (goto-char (point-min))
      (while (re-search-forward "\r<BR>" nil t) (replace-match "<BR>"))
      (goto-char (point-min))
      (while (re-search-forward "<BR>\r" nil t) (replace-match "<BR>"))
      (goto-char (point-min))
      (while (re-search-forward "\r" nil t) (replace-match "<BR>")))
    (shimbun-make-mime-article shimbun header)
    (buffer-string)))

(luna-define-method shimbun-article ((shimbun shimbun-slashdot-jp)
				     header &optional outbuf)
  (when (shimbun-current-group-internal shimbun)
    (with-current-buffer (or outbuf (current-buffer))
      (insert
       (or (with-temp-buffer
	     (shimbun-retrieve-url (shimbun-article-url shimbun header)
				   'no-cache)
	     (message "shimbun: Make contents...")
	     (goto-char (point-min))
	     (prog1 (shimbun-slashdot-jp-make-contents shimbun header)
	       (message "shimbun: Make contents...done")))
	   "")))))

(provide 'sb-slashdot-jp)

;;; sb-slashdot-jp.el ends here

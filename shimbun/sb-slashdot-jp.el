;;; sb-slashdot-jp.el --- shimbun backend for slashdot.ne.jp

;; Author: Yuuichi Teranishi <teranisi@gohome.org>

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

;;; Code:

(require 'shimbun)
(luna-define-class shimbun-slashdot-jp (shimbun) ())

(defvar shimbun-slashdot-jp-groups '("story"))
(defvar shimbun-slashdot-jp-url "http://slashdot.ne.jp/")

(defvar shimbun-slashdot-jp-content-start 
  "CELLSPACING=\"0\"><TR><TD BGCOLOR=\"#FFFFFF\">")
(defvar shimbun-slashdot-jp-content-end "</td>")

(defvar shimbun-slashdot-jp-months
  '("December" "November" "October" "August" "July" "June" "May"
    "April" "March" "February" "January"))

(defvar shimbun-slashdot-jp-story-pages 2
  "*Page number of slashdot.jp's story pages.
One page contains 30 stories.")

(defvar shimbun-slashdot-jp-comment-pages 6
  "*Page number of slashdot.jp's comment pages.
One page contains 30 comments.")

(luna-define-method shimbun-headers ((shimbun shimbun-slashdot-jp))
  (let ((case-fold-search t)
	year month mday uniq subject from pos hour min refs count
	id headers)
    (catch 'stop
      ;; main strories
      (setq count 0)
      (while (< count shimbun-slashdot-jp-story-pages)
	(with-current-buffer (shimbun-retrieve-url-buffer
			      (concat shimbun-slashdot-jp-url
				      (format "search.pl?start=%d"
					      (* 30 count)))
			      'reload)
	  (goto-char (point-min))
	  (while (re-search-forward "<A HREF=\".*/article.pl\\?sid=\\(\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)/[0-9]+\\)" nil t)
	    (setq year (string-to-number (match-string 2))
		  uniq (match-string 1))
	    (setq id (format "<%s@slashdot.ne.jp>" uniq))
	    (if (shimbun-search-id shimbun id)
		(throw 'stop nil))
	    (when (search-forward ">" nil t)
	      (setq pos (match-end 0))
	      (search-forward "<")
	      (setq subject (buffer-substring pos (match-beginning 0))))
	    (when (re-search-forward "（\\([A-Za-z-]+\\)，" nil t)
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
    (catch 'stop
      (setq count 0)
      ;; comments
      (while (< count shimbun-slashdot-jp-comment-pages)
	(with-current-buffer (shimbun-retrieve-url-buffer
			      (concat shimbun-slashdot-jp-url
				      (format 
				       "search.pl?op=comments&start=%d"
				       (* 30 count)))
			      'reload)
	  (goto-char (point-min))
	  (while (re-search-forward "<A HREF=\".*/comments.pl\\?sid=\\(\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)/[^\"]+\\)" nil t)
	    (setq year (string-to-number (match-string 2))
		  uniq (match-string 1))
	    (setq id (format "<%s@slashdot.ne.jp>" uniq))
	    (if (shimbun-search-id shimbun id)
		(throw 'stop nil))
	    (when (search-forward ">" nil t)
	      (setq pos (match-end 0))
	      (search-forward "<")
	      (setq subject (buffer-substring pos (match-beginning 0))))
	    (when (re-search-forward
		   "（<a href=\"mailto:\\([^>]*\\)\">\\([^<]*\\)</a>，" nil t)
	      (setq from 
		    (if (zerop (length (match-string 1)))
			(concat (match-string 2))
		      (concat (match-string 2) "<" (match-string 1) ">"))))
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
	    (when
		(re-search-forward
		 "<A HREF=\".*/article.pl\\?sid=\\([0-9]+/[0-9]+/[0-9]+/[0-9]+\\)"
		 nil t)
	      (setq refs (match-string 1)))
	    (push (shimbun-make-header
		   0
		   (shimbun-mime-encode-string subject)
		   from
		   (shimbun-make-date-string year month mday
					     (format "%02d:%02d" hour min))
		   id
		   (format "<%s@slashdot.ne.jp>" refs)
		   0 0 (concat 
			(shimbun-url-internal shimbun)
			"comments.pl?sid=" uniq))
		  headers)))
	(setq count (1+ count))))
    headers))

(luna-define-method shimbun-make-contents ((shimbun shimbun-slashdot-jp)
					   header)
  (let ((case-fold-search t)
	start num)
    (when (progn
	    ;; for comments.
	    (and (string-match "\\(#[0-9]+\\)$"
			       (shimbun-header-xref header))
		 (setq num (substring 
			    (shimbun-header-xref header)
			    (match-beginning 0)))
		 (re-search-forward (concat ">" num "</A>")
				    nil t))
	    ;; content beginning.
	    (and (re-search-forward (shimbun-content-start-internal shimbun)
				    nil t)
		 (setq start (match-end 0))
		 (re-search-forward (shimbun-content-end-internal shimbun)
				    nil t)))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start))
    (goto-char (point-min))
    (shimbun-header-insert shimbun header)
    (insert "Content-Type: text/html; charset=ISO-2022-JP\n"
	    "MIME-Version: 1.0\n\n")
    (encode-coding-string (buffer-string)
			  (mime-charset-to-coding-system "ISO-2022-JP"))))


(provide 'sb-slashdot-jp)

;;; sb-slashdot-jp.el ends here

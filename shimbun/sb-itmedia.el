;;; sb-itmedia.el --- shimbun backend for ITmedia -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2004, 2005 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>
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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)

(luna-define-class shimbun-itmedia (shimbun) ())

(defvar shimbun-itmedia-url "http://www.itmedia.co.jp/")

(defvar shimbun-itmedia-group-alist
  (let ((template (concat "<a href=\""
			  "\\(" (regexp-quote shimbun-itmedia-url) "\\|/\\)"
			  "\\(\\(%s\\)/\\([0-9][0-9]\\)\\([0-9][0-9]\\)\
/\\([0-9][0-9]\\)/\\([^\\.\">]+\\)\\.html\\)[^>]*>")))
    `(("anchordesk" "anchordesk"
       ,(format template "anchordesk/articles")
       "［[^ ]* \\([0-9]+:[0-9]+\\)］")
      ("bursts" "news/bursts"
       ,(format template "enterprise/articles\\|news/articles")
       "［[^ ]* \\([0-9]+:[0-9]+\\)］")
      ("business" "news/business"
       ,(format template "news/articles")
       nil)
      ("enterprise" "enterprise"
       ,(format template "enterprise/articles")
       "［[^ ]* \\([0-9]+:[0-9]+\\)］")
      ("games" "games/news"
       ,(format template "games/articles")
       nil)
      ("lifestyle" "lifestyle"
       ,(format template "lifestyle/articles")
       nil)
      ("mobile" "mobile/news"
       ,(format template "mobile/articles")
       nil)
      ("news" "news/past"
       ,(format template "news/articles")
       "(\\([0-9]+:[0-9]+\\))")
      ("pcupdate" "pcupdate/news"
       ,(format template "pcupdate/articles")
       nil)
      ("technology" "news/technology"
       ,(format template "news/articles")
       nil))))

(defvar shimbun-itmedia-server-name "ITmedia")
(defvar shimbun-itmedia-from-address "webmaster@itmedia.co.jp")
(defvar shimbun-itmedia-x-face-alist
  '(("default" . "X-Face: %JzFW&0lP]xKGl{Bk3\\`yC0zZp|!;\\KT9'rqE2AIk\
R[TQ[*i0d##D=I3|g`2yr@sc<pK1SB
 j`}1YEnKc;U0:^#LQB*})Q}y=45<lIE4q<gZ88e2qS8a@Tys6S")))

(luna-define-method shimbun-groups ((shimbun shimbun-itmedia))
  (mapcar 'car shimbun-itmedia-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-itmedia))
  (concat
   (shimbun-url-internal shimbun)
   (nth 1 (assoc (shimbun-current-group-internal shimbun)
		 shimbun-itmedia-group-alist))
   "/"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-itmedia)
					 &optional range)
;;;<DEBUG>
;;  (shimbun-itmedia-get-headers shimbun))
;;
;;(defun shimbun-itmedia-get-headers (shimbun)
;;;</DEBUG>
  (let* ((case-fold-search t)
	 (group (shimbun-current-group-internal shimbun))
	 (url-regexp (nth 2 (assoc group shimbun-itmedia-group-alist)))
	 (time-regexp (nth 3 (assoc group shimbun-itmedia-group-alist)))
	 (table '(("：" ":") ("［" "[") ("］" "]")))
	 next headers)
    (while (search-forward "\r" nil t)
      (replace-match "\n"))
    (goto-char (point-min))
    (while (if next
	       (progn
		 (set-match-data next)
		 (setq next nil)
		 (goto-char (match-end 0)))
	     (re-search-forward url-regexp nil t))
      (unless (string= "index" (match-string 7))
	(let ((url (match-string 2))
	      (year (+ 2000 (string-to-number (match-string 4))))
	      (month (string-to-number (match-string 5)))
	      (day (string-to-number (match-string 6)))
	      (id (format "<%s%s%s%s%%%s>"
			  (match-string 4)
			  (match-string 5)
			  (match-string 6)
			  (match-string 7)
			  group))
	      (subject (mapconcat
			(lambda (s)
			  (dolist (e table s)
			    (setq s (apply 'shimbun-replace-in-string s e))))
			(delete
			 ""
			 (split-string
			  (buffer-substring (match-end 0)
					    (progn
					      (search-forward "</A>" nil t)
					      (point)))
			  "[\t\n ]*<[^>]+>[\t\n ]*\\|[\t\n 　]+"))
			" "))
	      time)
	  (unless (zerop (length subject))
	    (setq subject (shimbun-replace-in-string
			   subject " ?\\([“”（）「」]\\) ?" "\\1"))
	    (when time-regexp
	      (setq next (point)
		    next (when (re-search-forward url-regexp nil t)
			   (goto-char next)
			   (match-data))
		    time (when (re-search-forward time-regexp (car next) t)
			   (match-string 1))))
	    (push (shimbun-create-header
		   0 subject
		   (shimbun-from-address shimbun)
		   (shimbun-make-date-string year month day time)
		   id  "" 0 0
		   (concat shimbun-itmedia-url url))
		  headers)))))
    (shimbun-sort-headers headers)))

(defun shimbun-itmedia-clean-text-page ()
  (let ((case-fold-search t) (start))
    (goto-char (point-min))
    (when (and (search-forward "<!--BODY-->" nil t)
	       (setq start (match-end 0))
	       (re-search-forward "<!--BODY ?END-->" nil t))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start)
      ;; Remove anchors to both the next page and the previous page.
      ;; These anchors are inserted into the head and the tail of the
      ;; article body.
      (skip-chars-backward " \t\r\f\n")
      (forward-line 0)
      (when (looking-at "<P ALIGN=\"CENTER\"><[AB]")
	(delete-region (point) (point-max)))
      (goto-char (point-min))
      (skip-chars-forward " \t\r\f\n")
      (when (looking-at "<P ALIGN=\"CENTER\"><[AB]")
	(delete-region (point-min) (line-end-position))))
    (shimbun-remove-tags "<!-- AD START -->" "<!-- AD END -->")
    (shimbun-remove-tags "\
<IMG [^>]*SRC=\"http:/[^\"]*/\\(ad\\.itmedia\\.co\\.jp\\|\
a1100\\.g\\.akamai\\.net\\)/[^>]+>")
    (shimbun-remove-tags "\
<A [^>]*HREF=\"http:/[^\"]*/\\(ad\\.itmedia\\.co\\.jp\\|\
a1100\\.g\\.akamai\\.net\\)/[^>]+>[^<]*</A>")))

(defun shimbun-itmedia-retrieve-next-pages (shimbun base-cid url
						    &optional images)
  (let ((case-fold-search t) (next))
    (goto-char (point-min))
    (when (re-search-forward
	   "<b><a href=\"\\([^\"]+\\)\">次のページ</a></b>" nil t)
      (setq next (shimbun-expand-url (match-string 1) url)))
    (shimbun-itmedia-clean-text-page)
    (goto-char (point-min))
    (insert "<html>\n<head>\n<base href=\"" url "\">\n</head>\n<body>\n")
    (goto-char (point-max))
    (insert "\n</body>\n</html>\n")
    (when shimbun-encapsulate-images
      (setq images (shimbun-mime-replace-image-tags base-cid url images)))
    (let ((body (shimbun-make-text-entity "text/html" (buffer-string)))
	  (result (when next
		    (with-temp-buffer
		      (shimbun-fetch-url shimbun next)
		      (shimbun-itmedia-retrieve-next-pages shimbun base-cid
							   next images)))))
      (list (cons body (car result))
	    (or (nth 1 result) images)))))

(luna-define-method shimbun-make-contents ((shimbun shimbun-itmedia) header)
  (let ((case-fold-search t))
    (when (re-search-forward "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\) \
\\([0-9]+:[0-9]+\\) 更新" nil t)
      (shimbun-header-set-date
       header
       (shimbun-make-date-string
	(string-to-number (match-string 1))
	(string-to-number (match-string 2))
	(string-to-number (match-string 3))
	(match-string 4))))
    (let ((base-cid (shimbun-header-id header)))
      (when (string-match "\\`<\\([^>]+\\)>\\'" base-cid)
	(setq base-cid (match-string 1 base-cid)))
      (let (body)
	(multiple-value-bind (texts images)
	    (shimbun-itmedia-retrieve-next-pages shimbun base-cid
						 (shimbun-header-xref header))
	  (erase-buffer)
	  (if (= (length texts) 1)
	      (setq body (car texts))
	    (setq body (shimbun-make-multipart-entity))
	    (let ((i 0))
	      (dolist (text texts)
		(setf (shimbun-entity-cid text)
		      (format "shimbun.%d.%s" (incf i) base-cid))))
	    (apply 'shimbun-entity-add-child body texts))
	  (when images
	    (setf (shimbun-entity-cid body) (concat "shimbun.0." base-cid))
	    (let ((new (shimbun-make-multipart-entity)))
	      (shimbun-entity-add-child new body)
	      (apply 'shimbun-entity-add-child new
		     (mapcar 'cdr (nreverse images)))
	      (setq body new))))
	(shimbun-header-insert shimbun header)
	(insert "MIME-Version: 1.0\n")
	(shimbun-entity-insert body)))
    (buffer-string)))

(provide 'sb-itmedia)

;;; sb-itmedia.el ends here

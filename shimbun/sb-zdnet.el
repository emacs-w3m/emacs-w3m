;;; sb-zdnet.el --- shimbun backend for Zdnet Japan -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2001, 2002 Yuuichi Teranishi <teranisi@gohome.org>

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

(require 'shimbun)
(luna-define-class shimbun-zdnet (shimbun) ())

(defvar shimbun-zdnet-url "http://www.zdnet.co.jp/")

(defvar shimbun-zdnet-group-alist
  (let ((template1 "<A HREF=\"/\\(\\(%s\\)/\\([0-9][0-9]\\)\\([0-9][0-9]\\)\
/\\([0-9][0-9]\\)/\\([^\\.\">]+\\)\\.html\\)[^>]*>")
	(template2 "<A HREF=\"\\(\\(%s\\)/\\([0-9][0-9]\\)\\([0-9][0-9]\\)\
/\\([0-9][0-9]\\)/\\([^\\.\">]+\\)\\.html\\)[^>]*>"))
    `(("comp" "news/past" ,(format template1 "news"))
      ("biztrends" "news/biztrends" ,(format template1 "news"))
      ("gamespot" "games" ,(format template2 "gsnews"))
      ("enterprise" "enterprise" ,(format template1 "enterprise"))
      ("broadband" "broadband/news" ,(format template1 "broadband"))
      ("macwire" "macwire/news" ,(format template1 "macwire"))
      ("mobile" "mobile/news" ,(format template1 "mobile")))))

(defvar shimbun-zdnet-server-name "ZDNet Japan")
(defvar shimbun-zdnet-from-address "zdnn@softbank.co.jp")
(defvar shimbun-zdnet-content-start "\\(<!--BODY-->\\|<!--DATE-->\\)")
(defvar shimbun-zdnet-content-end
  "\\(<!--BODY ?END-->\\|<!--BYLINE ?END-->\\)")
(defvar shimbun-zdnet-x-face-alist
  '(("default" . "X-Face: &w=7iU!o,u[!-faU#$3},zAI2\"E-a%!PFv%V5\"(XO>a8PqU>\
e2!`gk'sks9B5W'f-'p3APL\n L--L8Y@%_e*hagUPo#-ewB4.bTxjM}+*(4\\9@E1irAXc/iW\"\
d8xA's/bf}5B/e$!0:aO5^x\"eHwHi$\n |!EX8'`SIP3q-Y=|<}\\02|s_pK3850`Q$5l1y5ImB\
x|3Z|D*vbQ%UY!38ikbc/EnUU_tbHVH\"9Sfk{\n w>zvk!?===x`]c5_-+<@ooVVV#D~F`e0")))

(defvar shimbun-zdnet-expiration-days 31)

(luna-define-method shimbun-groups ((shimbun shimbun-zdnet))
  (mapcar 'car shimbun-zdnet-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-zdnet))
  (concat
   (shimbun-url-internal shimbun)
   (nth 1 (assoc (shimbun-current-group-internal shimbun)
		 shimbun-zdnet-group-alist))
   "/"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-zdnet)
					 &optional range)
  (let* ((case-fold-search t)
	 (group (shimbun-current-group-internal shimbun))
	 (url-regexp (nth 2 (assoc group shimbun-zdnet-group-alist)))
	 headers)
    (while (re-search-forward "\r" nil t)
      (replace-match "\n"))
    (goto-char (point-min))
    (while (re-search-forward url-regexp nil t)
      (unless (string= "index" (match-string 6))
	(let* ((url (match-string 1))
	       (year (+ 2000 (string-to-number (match-string 3))))
	       (month (string-to-number (match-string 4)))
	       (day (string-to-number (match-string 5)))
	       (id (format "<%s%s%s%s%%%s>"
			   (match-string 3)
			   (match-string 4)
			   (match-string 5)
			   (match-string 6)
			   group))
	       (subject (mapconcat 'identity
				   (split-string
				    (buffer-substring
				     (match-end 0)
				     (progn (search-forward "</A>" nil t) (point)))
				    "<[^>]+>")
				   "")))
	  (while (string-match "<[^>]+>" subject)
	    (setq subject (concat (substring subject 0 (match-beginning 0))
				  (substring subject (match-end 0)))))
	  (push (shimbun-make-header
		 0
		 (shimbun-mime-encode-string subject)
		 (shimbun-from-address shimbun)
		 (shimbun-make-date-string year month day)
		 id  "" 0 0
		 (cond ((equal group "gamespot") (concat (shimbun-index-url shimbun) url))
		       (t (concat shimbun-zdnet-url url))))
		headers))))
    headers))

(defun shimbun-zdnet-clean-text-page ()
  (let ((case-fold-search t) (start))
    (goto-char (point-min))
    (when (and (search-forward "<!--BODY-->" nil t)
	       (setq start (match-beginning 0))
	       (search-forward "<!--BODYEND-->" nil t))
      (delete-region (point) (point-max))
      (delete-region (point-min) start))
    (goto-char (point-min))
    (while (and (search-forward "<!-- AD START -->" nil t)
		(setq start (match-beginning 0))
		(search-forward "<!-- AD END -->" nil t))
      (delete-region start (point)))
    (while (re-search-forward
	    "<IMG [^>]*SRC=\"http:/[^\"]*/\\(ad\\.zdnet\\.co\\.jp\\|\
a1100\\.g\\.akamai\\.net\\)/[^>]+>"
	    nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (while (re-search-forward
	    "<A [^>]*HREF=\"http:/[^\"]*/\\(ad\\.zdnet\\.co\\.jp\\|\
a1100\\.g\\.akamai\\.net\\)/[^>]+>[^<]*</A>"
	    nil t)
      (delete-region (match-beginning 0) (match-end 0)))))

(defun shimbun-zdnet-retrieve-next-pages (shimbun base-cid url
						  &optional images)
  (let ((case-fold-search t) (next))
    (goto-char (point-min))
    (when (re-search-forward
	   "<a href=\"\\([^\"]+\\)\"><b>次のページ</b></a>" nil t)
      (setq next (shimbun-expand-url (match-string 1) url)))
    (shimbun-zdnet-clean-text-page)
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
		      (shimbun-zdnet-retrieve-next-pages shimbun base-cid
							 next images)))))
      (list (cons body (car result))
	    (or (nth 1 result) images)))))

(luna-define-method shimbun-make-contents ((shimbun shimbun-zdnet) header)
  (let ((case-fold-search t))
    (when (and (search-forward "<!--DATE-->" nil t)
	       (looking-at "[ 　]+\\([0-9]+\\)年\\(1[012]\\|[2-9]\\)月\
\\([12][0-9]?\\|3[01]?\\|[4-9]\\)日[ 　]+\
\\(0[0-9]\\|1[0-2]\\):\\([0-5][0-9]\\)[ 　]+\\([AP]M\\)"))
      (shimbun-header-set-date
       header
       (shimbun-make-date-string
	(string-to-number (match-string 1))
	(string-to-number (match-string 2))
	(string-to-number (match-string 3))
	(if (string= "PM" (match-string 6))
	    (format "%02d:%s"
		    (+ 12 (string-to-number (match-string 4)))
		    (match-string 5))
	  (buffer-substring (match-beginning 4) (match-end 5))))))
    (let ((base-cid (shimbun-header-id header)))
      (when (string-match "\\`<\\([^>]+\\)>\\'" base-cid)
	(setq base-cid (match-string 1 base-cid)))
      (let (body)
	(multiple-value-bind (texts images)
	    (shimbun-zdnet-retrieve-next-pages shimbun base-cid
					       (shimbun-header-xref header))
	  (erase-buffer)
	  (if (= (length texts) 1)
	      (setq body (car texts))
	    (setq body (shimbun-make-multipart-entity nil
						      (concat "shimbun.0."
							      base-cid)))
	    (let ((i 0))
	      (dolist (text texts)
		(setf (shimbun-entity-cid text)
		      (format "shimbun.%d.%s" (incf i) base-cid))))
	    (apply 'shimbun-entity-add-child body texts))
	  (when images
	    (let ((new (shimbun-make-multipart-entity nil)))
	      (shimbun-entity-add-child new body)
	      (apply 'shimbun-entity-add-child new
		     (mapcar 'cdr (nreverse images)))
	      (setq body new))))
	(shimbun-header-insert shimbun header)
	(insert "MIME-Version: 1.0\n")
	(shimbun-entity-insert body)))
    (buffer-string)))

(provide 'sb-zdnet)

;;; sb-zdnet.el ends here

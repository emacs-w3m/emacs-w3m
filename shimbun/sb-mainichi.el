;;; sb-mainichi.el --- shimbun backend for MSN-Mainichi -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004 Koichiro Ohba <koichiro@meadowy.org>

;; Author: Koichiro Ohba <koichiro@meadowy.org>
;;         Katsumi Yamaoka <yamaoka@jpl.org>
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

;; Original code was sb-yomiuri.el which is written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org> and
;; Yuuichi Teranishi <teranisi@gohome.org>

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-mainichi (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-mainichi-top-level-domain "mainichi-msn.co.jp"
  "Name of the top level domain for the MSN-Mainichi INTERACTIVE.")

(defvar shimbun-mainichi-url
  (concat "http://www." shimbun-mainichi-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-mainichi-group-table
  '(("shakai" "社会")
    ("shakai.edu" "教育")
    ("sports" "スポーツ")
    ("geinou" "芸能")
    ("kurashi" "暮らし")
    ("it" "IT")
    ("kagaku.science" "科学")
    ("kagaku.env" "環境")
    ("kagaku.medical" "医学")
    ("seiji" "政治")
    ("keizai" "経済")
    ("kokusai" "国際"))
  "List of supported groups and Japanese translations.")

(defvar shimbun-mainichi-server-name "毎日新聞")
(defvar shimbun-mainichi-from-address
  (concat "webmaster@" shimbun-mainichi-top-level-domain))
(defvar shimbun-mainichi-content-start
  "<!--[\t\n ]+START[\t\n ]+Article[\t\n ]+-->[\t\n ]*")
(defvar shimbun-mainichi-content-end
  "\\([\t\n ]*<[^>]+>\\)*[\t\n ]*<!--[\t\n ]+END[\t\n ]+Article[\t\n ]+-->")

(defvar shimbun-mainichi-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAABwAAAAcBAMAAACAI8KnAAAABGdBTUEAALGPC/xhBQAAABh
 QTFRFC2zfDGzfEnDgJn3iU5fnj7vvzuH4+vz++nlsOQAAAC90RVh0U29mdHdhcmUAWFYgVmVyc2l
 vbiAzLjEwYStGTG1hc2sgIFJldjogMTIvMjkvOTQbx6p8AAABA0lEQVR4nGWRPW+DMBiED5vuqK0
 60yppVwQ0rFTYZo348poSA3uDzd/vCxNNT14en1/5zgZYEoCF69rk2zhWPR6GADwCl864tsaHFUJ
 dweTJuMcQZZ0kRQxkqnaHik2DbBwdVuPgtPG7WTcuhPdshdM5f7lp4SpyXUPoazu1i6HZpbY6R3a
 ZhAW8ztmZsDxPqf0Cb6zsVzQjJQA/2GNE2OWHbqaQvEggI7wFfOmxk1esLUL2GrJg2yBkrTSDqvB
 eJKmhqtNpttk3sllICskmdbXlGdkPNcd/TIuuvOxcM65IsxvSa2Q79w7V8AfL2u1nY9ZquuiWfK7
 1BSVNQzxF9B+40y/ui1KdNxt0ugAAAAd0SU1FB9QEDQAjJMA7GTQAAAAASUVORK5CYII=")))

(defvar shimbun-mainichi-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-mainichi))
  (mapcar 'car shimbun-mainichi-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-mainichi))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-mainichi-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-mainichi))
  (concat (shimbun-url-internal shimbun)
	  (shimbun-subst-char-in-string
	   ?. ?/ (shimbun-current-group-internal shimbun))
	  "/"))

(defun shimbun-mainichi-make-date-string (&rest args)
  "Run `shimbun-make-date-string' with ARGS and fix a day if needed.

\(shimbun-mainichi-make-date-string YEAR MONTH DAY &optional TIME TIMEZONE)"
  (save-match-data
    (let* ((ctime (current-time))
	   (date (apply 'shimbun-make-date-string args))
	   (time (shimbun-time-parse-string date))
	   (ms (car time))
	   (ls (cadr time))
	   (system-time-locale "C"))
      (if (or (> ms (car ctime))
	      (and (= ms (car ctime))
		   (> ls (cadr ctime))))
	  ;; It should be yesterday's same time.
	  (progn
	    (setq ms (1- ms))
	    (when (< (setq ls (- ls (eval-when-compile (- (* 60 60 24) 65536))))
		     0)
	      (setq ms (1- ms)
		    ls (+ ls 65536)))
	    (format-time-string "%a, %d %b %Y %R +0900" (list ms ls)))
	date))))

(defun shimbun-mainichi-get-headers (shimbun)
  (let* ((group (shimbun-current-group-internal shimbun))
	 (hierarchy (when (string-match "\\." group)
		      (mapconcat 'identity
				 (nreverse (split-string group "\\."))
				 ".")))
	 (from (shimbun-from-address shimbun))
	 (case-fold-search t)
	 url urls subgroup header topnews headers date)
    (while (search-forward "\r" nil t)
      (delete-backward-char 1))
    (goto-char (point-min))
    (while (re-search-forward "\
<!--[\t\n ]*MEROS[\t\n ]+START[\t\n ]+Module=[^>]+>"
			      nil t)
      (narrow-to-region (point)
			(or (re-search-forward "\
<!--[\t\n ]*MEROS[\t\n ]+END[\t\n ]+Module[^>]+>"
					       nil t)
			    (point-max)))
      (goto-char (point-min))
      (while (re-search-forward
	      (eval-when-compile
		(let ((s0 "[\t\n ]*")
		      (s1 "[\t\n ]+"))
		  (concat "<a" s1 "href=\"/"
			  ;; 1. url
			  "\\([^/]+/"
			  "\\("
			  ;; 3. subgroup
			  "\\([^/]+\\)"
			  "/\\)?news/"
			  ;; 4. serial number
			  "\\("
			  ;; 5. year
			  "\\(20[0-9][0-9]\\)"
			  ;; 6. month
			  "\\([01][0-9]\\)"
			  ;; 7. day
			  "\\([0-3][0-9]\\)"
			  "[0-9a-z]+\\)"
			  "\\.html\\)"
			  "[^>]*>\\(" s0 "<[^>]+>\\)*" s0
			  ;; 9+11. subject
			  "\\([^<]+\\)\\(<br[^>]+>\\)?\\([^<]+\\)"
			  "\\(\\(" s0 "<[^>]+>\\)+" s0
			  ;; 14. hour
			  "\\([012]?[0-9]\\)"
			  ":"
			  ;; 15. minute
			  "\\([0-5]?[0-9]\\)"
			  "\\)?")))
	      nil t)
	(unless ;; Exclude duplications.
	    (member (setq url
			  (file-name-nondirectory (match-string 1)))
		    urls)
	  (push url urls)
	  (setq subgroup (when (match-beginning 3)
			   (match-string 3)))
	  (setq header
		(shimbun-create-header
		 0
		 (concat (match-string 9) (match-string 11))
		 from
		 (shimbun-mainichi-make-date-string
		  (string-to-number (match-string 5))
		  (string-to-number (match-string 6))
		  (string-to-number (match-string 7))
		  (if (match-beginning 15)
		      (format "%02d:%02d"
			      (string-to-number (match-string 14))
			      (string-to-number (match-string 15)))
		    (unless subgroup
		      ;; It may be a top news.
		      "23:59:59")))
		 (concat "<" (match-string 4) "%"
			 (or hierarchy
			     (concat (or subgroup "top") "." group))
			 "." shimbun-mainichi-top-level-domain ">")
		 "" 0 0
		 (concat shimbun-mainichi-url (match-string 1))))
	  (unless subgroup
	    (push header topnews))
	  (push header headers)))
      (goto-char (point-max))
      (widen))
    (prog1
	(shimbun-sort-headers headers)
      ;; Fix date headers for top news.
      (while topnews
	(setq header (pop topnews)
	      date (shimbun-header-date header))
	(when (string-match "23:59:59" date)
	  (shimbun-header-set-date header
				   (replace-match "00:00" nil nil date)))))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-mainichi)
					 &optional range)
  (shimbun-mainichi-get-headers shimbun))

(defun shimbun-mainichi-prepare-article (shimbun header)
  (let ((case-fold-search t))
    (while (search-forward "\r" nil t)
      (delete-backward-char 1))
    (goto-char (point-min))
    ;; Remove the subject lines.
    (when (re-search-forward "<\\([0-9a-z]+\\)[\t\n ]+class=\"m-title" nil t)
      (let ((start (match-beginning 0)))
	(when (search-forward (concat "</" (match-string 1) ">") nil t)
	  (delete-region start (point)))))
    ;; Fix the Date header.
    (when (re-search-forward
	   (eval-when-compile
	     (let ((s0 "[\t\n 　]*")
		   (s1 "[\t\n 　]+"))
	       (concat "<span" s1 "class=\"m-txt[^>]+>" s0 "毎日新聞" s1
		       ;; 1. year
		       "\\(20[0-9][0-9]\\)"
		       s0 "年" s0
		       ;; 2. month
		       "\\([01]?[0-9]\\)"
		       s0 "月" s0
		       ;; 3. day
		       "\\([0-3]?[0-9]\\)"
		       s0 "日" s0
		       ;; 4. hour
		       "\\([012]?[0-9]\\)"
		       s0 "時" s0
		       ;; 5. minute
		       "\\([0-5]?[0-9]\\)"
		       s0 "分" s0 "</span>")))
	   nil t)
      (shimbun-header-set-date
       header
       (shimbun-make-date-string
	(string-to-number (match-string 1))
	(string-to-number (match-string 2))
	(string-to-number (match-string 3))
	(format "%02d:%02d"
		(string-to-number (match-string 4))
		(string-to-number (match-string 5))))))
    ;; Break continuous lines.
    (when (string-match "\\`余録：" (shimbun-header-subject header 'no-encode))
      (goto-char (point-min))
      (while (search-forward "▲" nil t)
	(replace-match "。<br>\\&<br>")))
    ;; Break long lines.
    (shimbun-break-long-japanese-lines shimbun)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-mainichi)
						   header)
  (shimbun-mainichi-prepare-article shimbun header))

(provide 'sb-mainichi)

;;; sb-mainichi.el ends here

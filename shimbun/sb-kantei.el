;;; sb-kantei.el --- shimbun backend for kantei mail magazine backnumber -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2003, 2003, 2004, 2005
;; Yuuichi Teranishi <teranisi@gohome.org>

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
;; Inc.; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-kantei (shimbun) ())

(defvar shimbun-kantei-url "http://www.kantei.go.jp/")
(defvar shimbun-kantei-from-address "koizumi@mmz.kantei.go.jp")

(defvar shimbun-kantei-groups '("m-magazine-en" "m-magazine-ja" "m-magazine")
  "List of groups of the Koizumi cabinet email magazine.
Note that the `m-magazine-ja' is the same as `m-magazine' which is for
the backward compatibility.")

(defvar shimbun-kantei-content-start
  "<!-- CONTENT -->\\|<PRE>")
(defvar shimbun-kantei-content-end
  "\\(<!-- /CONTENT -->\\)\\|\\(</PRE>\\)\n</FONT>\n</TD></TR></TABLE>")
(defvar shimbun-kantei-x-face-alist
  '(("default" . "X-Face: .bsmj'!8A`wI\\o+KF!)#0.a0,f1MA~PH/5T0\
fu$Mg+)_5G~NSk4.0t]&|f@^c3l8-Fuz8'|\n kr;td_Jn7|GwREbDs'H9$Iy#y\
M#*J2c'L},(m8K:8?$vTPC%D}YJ[bV#7xw|{\"DJ:_?`V1m_4^+;7+\n JOf6v&\
x6?mU-q=0}mTK5@\"-bFGuD}2Y/(lR/V#'?HRc2Jh2UrR,oIR~NL!})|^%kw")))

(luna-define-method shimbun-index-url ((shimbun shimbun-kantei))
  (let ((group (shimbun-current-group-internal shimbun)))
    (concat (shimbun-url-internal shimbun)
	    (cond ((string-equal group "m-magazine-en")
		   "foreign/m-magazine")
		  ((string-equal group "m-magazine") ;; Backward compatibility.
		   "jp/m-magazine")
		  (t
		   "jp/m-magazine"))
	    "/backnumber/")))

(luna-define-method shimbun-from-address ((shimbun shimbun-kantei))
  (format "%s <%s>"
	  (shimbun-mime-encode-string
	   (if (string-equal (shimbun-current-group-internal shimbun)
			     "m-magazine-en")
	       "Official Residence"
	     "首相官邸"))
	  (shimbun-from-address-internal shimbun)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-kantei)
					 &optional range)
;;;<DEBUG>
;;  (shimbun-kantei-get-headers shimbun range))
;;
;;(defun shimbun-kantei-get-headers (shimbun range)
;;;</DEBUG>
  (let* ((group (shimbun-current-group-internal shimbun))
	 (enp (equal group "m-magazine-en"))
	 (regexp (if enp
		     (eval-when-compile
		       (concat "\\["
			       ;; 1. month
			       "\\([01][0-9]\\)"
			       "/"
			       ;; 2. day of month
			       "\\([0-3][0-9]\\)"
			       "/"
			       ;; 3. year
			       "\\(20[0-9][0-9]\\)"
			       "\\][\t\n ]*"
			       ;; 4. serial number
			       "\\(No\\. [0-9]+:\\)?"
			       "\\(?:[\t\n ]*<[^A][^>]+>\\)+[\t\n ]*"
			       "<A[\t\n ]+HREF=\""
			       ;; 5. url
			       "\\([^\"]+\\)"
			       "\"[^>]*>[\t\n ]*"
			       ;; 6. subject
			       "\\([^<]+\\)"))
		   (eval-when-compile
		     (concat "<A[\t\n ]+HREF=\""
			     ;; 1. url
			     "\\("
			     ;; 2. year
			     "\\(20[0-9][0-9]\\)"
			     "/"
			     ;; 3. month
			     "\\([01][0-9]\\)"
			     ;; 4. day of month
			     "\\([0-3][0-9]\\)"
			     ;; 5. revision e.g., 2005/0602b.html
			     "\\([^.]+\\)?"
			     "\\.html\\)"
			     "\"[^>]*>[\t\n ]*【[^】]+】[\t\n ]*"
			     ;; 6. subject
			     "\\([^<]+\\)"))))
	 (parent (shimbun-index-url shimbun))
	 (from (shimbun-from-address shimbun))
	 year month mday url subject id headers)
    (while (re-search-forward regexp nil t)
      (if enp
	  (progn
	    (setq year (match-string 3)
		  month (match-string 1)
		  mday (match-string 2)
		  url (match-string 5)
		  subject (if (match-beginning 4)
			      (concat (match-string 4) " " (match-string 6))
			    (match-string 6)))
	    (if (string-match "\
\\`\\(20[0-9][0-9]\\)/\\(\\([01][0-9]\\)\\([0-3][0-9]\\)[^.]*\\)\\.html\\'"
			      url)
		(setq year (string-to-number (match-string 1 url))
		      month (string-to-number (match-string 3 url))
		      mday (string-to-number (match-string 4 url))
		      id (format "<%d%s%s@www.kantei.or.jp>"
				 year (match-string 2 url) group))
	      (setq year (string-to-number year)
		    month (string-to-number month)
		    mday (string-to-number mday)
		    id (format "<%d%02d%02d%s@www.kantei.or.jp>"
			       year month mday group)))
	    (when (string-match "[\t ]+\\'" subject)
	      (setq subject (substring subject 0 (match-beginning 0)))))
	(setq year (string-to-number (match-string 2))
	      month (string-to-number (match-string 3))
	      mday (string-to-number (match-string 4))
	      url (match-string 1)
	      subject (match-string 6)
	      id (format "<%d%02d%02d%s%s@www.kantei.or.jp>"
			 year month mday
			 (or (match-string 5) "")
			 group)))
      (push (shimbun-create-header
	     0 subject from
	     (shimbun-make-date-string year month mday)
	     id "" 0 0
	     (if (string-match "\\`http:" url)
		 url
	       (shimbun-expand-url url parent)))
	    headers))
    headers))

(luna-define-method shimbun-clear-contents ((shimbun shimbun-kantei) header)
  (let ((case-fold-search t) start)
    (when (and (re-search-forward (shimbun-content-start shimbun) nil t)
	       (setq start (match-beginning 0))
	       (re-search-forward (shimbun-content-end shimbun) nil t))
      (delete-region (or (match-end 1) (match-end 2)) (point-max))
      (delete-region (point-min) start)
      t)))

(provide 'sb-kantei)

;;; sb-kantei.el ends here

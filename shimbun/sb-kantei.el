;;; sb-kantei.el --- shimbun backend for kantei mail magazine backnumber -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2003, 2003, 2004 Yuuichi Teranishi <teranisi@gohome.org>

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
  (let (year month mday id url subject headers)
    (while (re-search-forward
	    ;; 1. url
	    ;; 3. year (ja)
	    ;; 4. month (ja)
	    ;; 5. mday (ja)
	    ;; 6. month (en)
	    ;; 7. mday (en)
	    ;; 8. year (en)
	    ;; 9. subject
	    "\
<A HREF=\"\\([^\">]+\\)\">\
\\(\
【\\(20[0-9][0-9]\\)/\\([01][0-9]\\)/\\([0-3][0-9]\\)】\
\\|\
\\[\\([01][0-9]\\)/\\([0-3][0-9]\\)/\\(20[0-9][0-9]\\)\\]\
\\)\
\[\t ]*\\([^<]+\\)"
	    nil t)
      (setq year (string-to-number (or (match-string 3) (match-string 8)))
	    month (string-to-number (or (match-string 4) (match-string 6)))
	    mday (string-to-number (or (match-string 5) (match-string 7)))
	    url (match-string 1)
	    id (format "<%d%02d%02d%s@www.kantei.or.jp>"
		       year month mday
		       (shimbun-current-group-internal shimbun))
	    subject (match-string 9))
      (push (shimbun-create-header
	     0
	     (or subject "")
	     (shimbun-from-address shimbun)
	     (shimbun-make-date-string year month mday)
	     id "" 0 0
	     (if (string-match "\\`http:" url)
		 url
	       (concat (shimbun-index-url shimbun) url)))
	    headers))
    headers))

(luna-define-method shimbun-clear-contents ((shimbun shimbun-kantei) header)
  (let ((case-fold-search t) start)
    (when (and (re-search-forward (shimbun-content-start-internal shimbun)
				  nil t)
	       (setq start (match-beginning 0))
	       (re-search-forward (shimbun-content-end-internal shimbun)
				  nil t))
      (delete-region (or (match-end 1) (match-end 2)) (point-max))
      (delete-region (point-min) start)
      t)))

(provide 'sb-kantei)

;;; sb-kantei.el ends here

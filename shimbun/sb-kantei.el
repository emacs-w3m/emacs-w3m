;;; sb-kantei.el --- shimbun backend for kantei mail magazine backnumber.

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

(luna-define-class shimbun-kantei (shimbun) ())

(defvar shimbun-kantei-url "http://www.kantei.go.jp/jp/")
(defvar shimbun-kantei-from-address "首相官邸 <koizumi@mmz.kantei.go.jp>")
(defvar shimbun-kantei-groups '("m-magazine"))
(defvar shimbun-kantei-content-start "<PRE>")
(defvar shimbun-kantei-content-end "\\(</PRE>\\)\n</FONT>\n</TD></TR></TABLE>")
(defvar shimbun-kantei-x-face-alist
  '(("default" . "X-Face: .bsmj'!8A`wI\\o+KF!)#0.a0,f1MA~PH/5T0\
fu$Mg+)_5G~NSk4.0t]&|f@^c3l8-Fuz8'|\n kr;td_Jn7|GwREbDs'H9$Iy#y\
M#*J2c'L},(m8K:8?$vTPC%D}YJ[bV#7xw|{\"DJ:_?`V1m_4^+;7+\n JOf6v&\
x6?mU-q=0}mTK5@\"-bFGuD}2Y/(lR/V#'?HRc2Jh2UrR,oIR~NL!})|^%kw")))

(luna-define-method shimbun-index-url ((shimbun shimbun-kantei))
  (concat (shimbun-url-internal shimbun)
	  (shimbun-current-group-internal shimbun)
	  "/backnumber/"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-kantei)
					 &optional range)
  (let (year month mday id url subject headers)
    (while (re-search-forward "<A HREF=\"\\(\\([0-9]+\\)/\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\.html\\)\">" nil t)
      (setq year (string-to-number (match-string 2))
	    month (string-to-number (match-string 3))
	    mday (string-to-number (match-string 4))
	    url (match-string 1)
	    id (format "<%s%s%s%s@www.kantei.or.jp>"
		       (match-string 2)
		       (match-string 3)
		       (match-string 4)
		       (shimbun-current-group-internal shimbun)))
      (if (re-search-forward "】\\([^<]+\\)<" nil t)
	  (setq subject (match-string 1)))
      (push (shimbun-make-header
	     0
	     (shimbun-mime-encode-string (or subject ""))
	     (shimbun-mime-encode-string
	      (shimbun-from-address-internal shimbun))
	     (shimbun-make-date-string year month mday)
	     id "" 0 0 (concat (shimbun-index-url shimbun) url))
	    headers))
    headers))

(luna-define-method shimbun-make-contents ((shimbun shimbun-kantei) header)
  (let ((case-fold-search t)
	start)
    (when (and (re-search-forward (shimbun-content-start-internal shimbun)
				  nil t)
	       (setq start (match-beginning 0))
	       (re-search-forward (shimbun-content-end-internal shimbun)
				  nil t))
      (delete-region (match-end 1) (point-max))
      (delete-region (point-min) start)
      (goto-char (point-min))
      (insert "<html>\n<head>\n<base href=\""
	      (shimbun-header-xref header) "\">\n</head>\n<body>\n")
      (goto-char (point-max))
      (insert "\n</body>\n</html>\n"))
    (shimbun-make-mime-article shimbun header)
    (buffer-string)))

(provide 'sb-kantei)

;;; sb-kantei.el ends here

;;; sb-kantei.el --- shimbun backend for kantei mail magazine backnumber -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2003, 2003, 2004, 2005, 2006, 2007
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

(defvar shimbun-kantei-groups '("m-magazine-en" "m-magazine-ja"
				;;"m-magazine-en.koizumi"
				"m-magazine-ja.koizumi"
				"m-magazine")
  "List of the groups subscribing to the email magazin of Japan's Cabinet.
Note that the `m-magazine-ja.koizumi' is the same as `m-magazine' which
is for the backward compatibility.")

(defvar shimbun-kantei-content-start
  "<!-- CONTENT -->\\|<PRE>")

(defvar shimbun-kantei-content-end
  "\\(<!-- /CONTENT -->\\)\\|\\(</PRE>\\)\n</FONT>\n</TD></TR></TABLE>")

(defvar shimbun-kantei-x-face-alist
  '(("default" . "X-Face: 2lqMN=orK#d]Xl-K5P`=ApJHMB3[faCtca;G(i=qL^3\
qh<kEoLHF\"L\"x/a:|xD>x=IKEqN%\n 3EL93@D{*BW-{GE88b7{d^m-%v9}=-7=^M#$\
?zJm$]Yy07J^}:#V?9t_<{fhavZVZQ1^1=SLQf3X=<\n z|Af_njD},U!m}4V}$]L_7a!\
b>X!RW$['xZs$r=G?o|=M^O)IJoOurt|UKUu[UuQFT/r&vygySYUmf\n <G6B:zwx0@$x\
HbD#Hr3`J,C!5rN5t7oI)ng/'e40?>Jm;kjj")
    ("\\.koizumi\\'\\|\\`m-magazine\\'" . "X-Face: .bsmj'!8A`wI\\o+KF\
!)#0.a0,f1MA~PH/5T0fu$Mg+)_5G~NSk4.0t]&|f@^c3l8-Fuz8'|\n kr;td_Jn7|Gw\
REbDs'H9$Iy#yM#*J2c'L},(m8K:8?$vTPC%D}YJ[bV#7xw|{\"DJ:_?`V1m_4^+;7+\n\
 JOf6v&x6?mU-q=0}mTK5@\"-bFGuD}2Y/(lR/V#'?HRc2Jh2UrR,oIR~NL!})|^%kw")))

(luna-define-method shimbun-index-url ((shimbun shimbun-kantei))
  (let ((group (shimbun-current-group-internal shimbun)))
    (concat (shimbun-url-internal shimbun)
	    (cond ((string-equal group "m-magazine-en")
		   "foreign/m-magazine/backnumber/")
		  ;;((string-equal group "m-magazine-en.koizumi")
		  ;; "???")
		  ((string-equal group "m-magazine-ja.koizumi")
		   "jp/m-magazine/backnumber/koizumi.html")
		  ((string-equal group "m-magazine") ;; Backward compatibility.
		   "jp/m-magazine/backnumber/koizumi.html")
		  (t
		   "jp/m-magazine/backnumber/")))))

(luna-define-method shimbun-from-address ((shimbun shimbun-kantei))
  (let ((group (shimbun-current-group-internal shimbun)))
    (cond ((string-equal group "m-magazine-en")
	   "Shinzo Abe")
	  ;;((string-equal group "m-magazine-en.koizumi")
	  ;; "Junichiro Koizumi")
	  ((string-equal group "m-magazine-ja.koizumi")
	   (shimbun-mime-encode-string "小泉純一郎"))
	  ((string-equal group "m-magazine") ;; Backward compatibility.
	   (shimbun-mime-encode-string "小泉純一郎"))
	  (t
	   (shimbun-mime-encode-string "阿部晋三")))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-kantei)
					 &optional range)
;;;<DEBUG>
;;  (shimbun-kantei-get-headers shimbun range))
;;
;;(defun shimbun-kantei-get-headers (shimbun range)
;;;</DEBUG>
  (let* ((group (shimbun-current-group-internal shimbun))
	 (enp (member group '("m-magazine-en")))
	 (regexp (if enp
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
			       "\\.html\\)\">[\t\n ]*"
			       ;; 5. subject
			       "\\(\\(?:[^\t\n <]+[\t\n ]+\\)*[^\t\n <]+\\)"
			       "[\t\n ]*</A>[\t\n ]*</TD>[\t\n ]*</TR>"))
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
	  (setq year (string-to-number (match-string 2))
		month (string-to-number (match-string 3))
		mday (string-to-number (match-string 4))
		url (match-string 1)
		subject (match-string 5)
		id (format "<%d%02d%02d.%s%%kantei.go.jp>"
			   year month mday group))
	(setq year (string-to-number (match-string 2))
	      month (string-to-number (match-string 3))
	      mday (string-to-number (match-string 4))
	      url (match-string 1)
	      subject (shimbun-replace-in-string (match-string 6)
						 "[\t\n 　]+" " ")
	      id (format "<%d%02d%02d%s.%s%%kantei.go.jp>"
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
  (let ((case-fold-search t)
	start hankaku)
    (when (and (re-search-forward (shimbun-content-start shimbun) nil t)
	       (setq start (match-beginning 0))
	       (re-search-forward (shimbun-content-end shimbun) nil t))
      (delete-region (or (match-end 1) (match-end 2)) (point-max))
      (delete-region (point-min) start)
      (when (member (shimbun-current-group-internal shimbun)
		    '("m-magazine-en"))
	(goto-char (point-min))
	(while (re-search-forward "</?center>" nil t)
	  (delete-region (match-beginning 0) (match-end 0))))
      (setq hankaku (shimbun-japanese-hankaku shimbun))
      (when (and hankaku (not (memq hankaku '(header subject))))
	(shimbun-japanese-hankaku-buffer t))
      t)))

(provide 'sb-kantei)

;;; sb-kantei.el ends here

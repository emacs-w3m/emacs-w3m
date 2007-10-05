;;; sb-ohmynews-jp.el --- shimbun backend for OhmyNews Japan -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2006, 2007 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
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
(require 'sb-rss)

(luna-define-class shimbun-ohmynews-jp (shimbun-rss) ())

(defvar shimbun-ohmynews-jp-url "http://www.ohmynews.co.jp/"
  "Name of the parent url.")

(defvar shimbun-ohmynews-jp-server-name "$B%*!<%^%$%K%e!<%9(B")

(defvar shimbun-ohmynews-jp-group-table
  '(("news" "$B:G?75-;v(B")
    ("watashi" "$B$o$?$7(B" 100)
    ("life" "$B@83h!&0eNE(B" 110)
    ("shakai" "$B<R2q(B" 120)
    ("seiji" "$B@/<#(B" 130)
    ("keizai" "$B7P:Q(B" 140)
    ("kaigai" "$B3$30(B" 150)
    ("culture" "$B%+%k%A%c!<(B" 160)
    ("style" "$B%9%?%$%k(B" 170)
    ("sports" "$B%9%]!<%D(B" 180)
    ("it" "IT" 190)
    ("local" "$BCO0hHG(B" 200)))

(defvar shimbun-ohmynews-jp-x-face-alist
  '(("default" . "X-Face: o|vpDA-})w*TrtnFk9lZ\",j\"y_kn<xZy+LC\\zH(wC$\
Q^ur1c4B3)t\\tK\\yi-Qku8$*\\d<m]\n x;<6rdcYugs1o1w2dObSQ.INk`9f1x!hNe\\\
v*[xW.y6Tt/r=U{a?+nH20N{)a/w145kJxfhqf}Jd<p\n `bP:u\\Awi^xGQ3pUOrsPL.';\
|}zKE@+4GE4!+rd4[>dSxnHe#Z4#\\hy*R&}uSO=(,5UM)-jERou2]H\n ,5\"$Ka&<hoeL")))

(defvar shimbun-ohmynews-jp-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-ohmynews-jp))
  (mapcar 'car shimbun-ohmynews-jp-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-ohmynews-jp))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-ohmynews-jp-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-ohmynews-jp))
  (let ((index (nth 2 (assoc (shimbun-current-group-internal shimbun)
			     shimbun-ohmynews-jp-group-table))))
    (shimbun-expand-url (if (numberp index)
			    (format "rss/news_c%03d.xml" index)
			  "rss/news.xml")
			(shimbun-url-internal shimbun))))

(luna-define-method shimbun-clear-contents :around ((shimbun
						     shimbun-ohmynews-jp)
						    header)
  (shimbun-strip-cr)
  (goto-char (point-min))
  (when (and (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"news_home_box_border\""
				nil t)
	     (shimbun-end-of-tag "div" t))
    (delete-region (match-end 3) (point-max))
    (insert "\n")
    (delete-region (goto-char (point-min)) (match-beginning 3))
    ;; Remove title that's needless in the body.
    (goto-char (point-min))
    (let ((need (static-if (= (length "$,2"f(B") 1)
		    "$B!y!z!{!|!}!~"!"""#"$"%"&"'$,2"f"e"+"/"."'"&!a!`!s!r!}!|(B"
		  "$B!y!z!{!|!}!~"!"""#"$"%"&"'(B"))
	  st nd)
      (when (and (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"title\""
				    nil t)
		 (shimbun-end-of-tag "div" t)
		 (progn
		   (setq st (match-end 3)
			 nd (match-end 0))
		   (goto-char (match-beginning 3))
		   (looking-at (concat "<\\(h[0-9]+\\)>\\(?:[\t\n ]*"
				       "\\([" need "]\\)\\)?")))
		 (progn
		   (setq need (match-beginning 2))
		   (shimbun-end-of-tag (match-string 1) t)))
	(delete-region (goto-char st) nd)
	(insert "\n")
	(delete-region (point-min) (if need
				       (match-beginning 0)
				     (match-end 0)))))
    ;; Remove javascripts.
    (while (and (re-search-forward "\
<script[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*type=\"text/javascript\""
				   nil t)
		(or (shimbun-end-of-tag "script" t)
		    (shimbun-end-of-tag nil t)))
      (replace-match "\n"))
    ;; Remove useless buttons.
    (goto-char (point-min))
    (when (and (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"news_btn\""
				  nil t)
	       (shimbun-end-of-tag "div" t))
      (replace-match "\n"))
    ;; Remove trailing garbage.
    (goto-char (point-min))
    (when (re-search-forward
	   "\\(?:</p>\\)?\\(\\(?:[\t\n $B!!(B]*<[^>]+>\\)+[\t\n $B!!(B]*\\'\\)"
	   nil t)
      (delete-region (match-beginning 1) (point-max))
      (insert "\n"))
    ;; Convert Japanese zenkaku ASCII chars into hankaku.
    (let ((hankaku (shimbun-japanese-hankaku shimbun)))
      (when (and hankaku (not (memq hankaku '(header subject))))
	(shimbun-japanese-hankaku-buffer t)))
    (if (shimbun-prefer-text-plain-internal shimbun)
	;; Replace image tags with text.
	(progn
	  (goto-char (point-min))
	  (while (and (re-search-forward "<img[\t\n ]+" nil t)
		      (shimbun-end-of-tag))
	    (replace-match "&lt;$B<L??(B&gt;")))
      ;; Break long lines.
      (shimbun-break-long-japanese-lines))
    t))

(luna-define-method shimbun-footer :around ((shimbun shimbun-ohmynews-jp)
					    header &optional html)
  (if html
      (concat "<div align=\"left\">\n--&nbsp;<br>\n\
$B$3$N5-;v$NCx:n8"$O%*!<%^%$%K%e!<%9!";TL15-<T!"$^$?$OEj9F<T$K5"B0$7$^$9!#(B<br>
$B86J*$O(B<a href=\""
	      (shimbun-article-base-url shimbun header)
	      "\"><u>$B$3$3(B</u></a>$B$G8x3+$5$l$F$$$^$9!#(B\n</div>\n")
    (concat "-- \n\
$B$3$N5-;v$NCx:n8"$O%*!<%^%$%K%e!<%9!";TL15-<T!"$^$?$OEj9F<T$K5"B0$7$^$9!#(B\n\
$B86J*$O0J2<$N>l=j$G8x3+$5$l$F$$$^$9(B:\n"
	    (shimbun-article-base-url shimbun header) "\n")))

(provide 'sb-ohmynews-jp)

;;; sb-ohmynews-jp.el ends here

;;; sb-impress.el --- shimbun backend for www.watch.impress.co.jp

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
(luna-define-class shimbun-impress (shimbun) ())

(defvar shimbun-impress-url "http://www.watch.impress.co.jp/")

(defvar shimbun-impress-groups-alist
  '(("internet" "<a href=\"\\(www/article/\\([0-9]+\\)/\\([0-9][0-9]\\)\\([0-9][0-9]\\)/\\([^>]*\\)\\)\">" "<!-- 本文開始 -->" "<!-- 本文終了 -->")
    ("pc" "<a href=\"\\(docs/article/\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)/\\([^>]*\\)\\)\">" "\\(<hr>\\|<!-- 本文開始 -->\\)" "<!-- 本文終了 -->")
    ("akiba" "<a href=\"\\(hotline/\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)/\\([^>]*\\)\\)\">" "\\(<hr>\\|<!-- 本文開始 -->\\)" "<!-- 本文終了 -->")
    ("game" "<a href=\"\\(docs/\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)/\\([^>]*\\)\\)\">" "<!-- 本文開始 -->" "<!-- 本文終了 -->")
    ("av" "<a href=\"\\(docs/\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)/\\([^>]*\\)\\)\">" "\\(<!-- 本文開始 -->\\|<!-- title -->\\)" "<!-- 本文終了 -->")
    ("jijinews" "<a href=\"\\.\\./\\(news/\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)[0-9]+/\\([^>]*\\)\\)\">" "<!--■■本文■■-->" "<br>" "/main/main.htm")
    ("sports" "<a href=\"\\.\\./\\(news/\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)[0-9]+/\\([^>]*\\)\\)\">" "<!--■■本文■■-->" "<br>" "/main/main.htm")))
    
(defvar shimbun-impress-groups (mapcar 'car shimbun-impress-groups-alist))
(defvar shimbun-impress-coding-system 'shift_jis)
(defvar shimbun-impress-from-address "www-admin@impress.co.jp")
(defvar shimbun-impress-x-face-alist
  '(("default" . 
     "X-Face: F3zvh@X{;Lw`hU&~@uiX9J0dwTeROiIzoSoe'Y.gU#(EqHA5K}v}2ah,QlHa[S^}5ZuTefR
 ZA[pF1_ZNlDB5D_DJzTbXTM!V{ecn<+l,RDM&H3CKdu8tWENJlbRm)a|Hk+limu}hMtR\\E!%r9wC\"6
 ebr5rj1[UJ5zDEDsfo`N7~s%;P`\\JK'#y.w^>K]E~{`wZru")))
(defvar shimbun-impress-expiration-days 7)

(luna-define-method shimbun-index-url ((shimbun shimbun-impress))
  (let ((index (or (nth 4 (assoc (shimbun-current-group-internal shimbun)
				 shimbun-impress-groups-alist))
		   "/index.htm")))
    (concat (shimbun-url-internal shimbun) "/"
	    (shimbun-current-group-internal shimbun) index)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-impress))
  (let ((case-fold-search t)
	(regexp (nth 1 (assoc (shimbun-current-group-internal shimbun)
			      shimbun-impress-groups-alist)))
	ids
	headers)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((apath (match-string 1))
	    (year  (string-to-number (match-string 2)))
	    (month (string-to-number (match-string 3)))
	    (mday  (string-to-number (match-string 4)))
	    (uniq  (match-string 5))
	    (pos (point))
	    subject
	    id)
	(when (re-search-forward "</a>" nil t)
	  (setq subject (buffer-substring pos (match-beginning 0))
		subject (with-temp-buffer
			  (insert subject)
			  (goto-char (point-min))
			  (while (re-search-forward "[\r\n]" nil t)
			    (replace-match ""))
			  (shimbun-remove-markup)
			  (buffer-string))))
	(setq id (format "<%d%d%d%s%%%s@www.watch.impress.co.jp>"
			 year month mday uniq (shimbun-current-group-internal
					       shimbun)))
	(unless (member id ids)
	  (setq ids (cons id ids))
	  (push (shimbun-make-header
		 0
		 (shimbun-mime-encode-string (or subject ""))
		 (shimbun-from-address-internal shimbun)
		 (shimbun-make-date-string year month mday)
		 id
		 "" 0 0 (concat
			 (shimbun-url-internal shimbun)
			 (shimbun-current-group-internal shimbun)
			 "/" apath))
		headers))))
    headers))

(luna-define-method shimbun-make-contents :around ((shimbun shimbun-impress)
						   &optional header)
  (let ((entry (assoc (shimbun-current-group-internal shimbun)
		      shimbun-impress-groups-alist)))
    (shimbun-set-content-start-internal shimbun (nth 2 entry))
    (shimbun-set-content-end-internal shimbun (nth 3 entry))
    (luna-call-next-method)))

(provide 'sb-impress)

;;; sb-impress.el ends here

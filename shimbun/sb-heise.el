;;; sb-heise.el --- heise online shimbun backend

;; Copyright (C) 2004 David Hansen

;; Author: David Hansen <david.hansen@physik.fu-berlin.de>
;; Keywords: news

;; This file is a part of shimbun.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-heise (shimbun) ())

(defvar shimbun-heise-url "http://www.heise.de")

(defvar shimbun-heise-group-path-alist
  '(("news" . "/newsticker/")
    ("telepolis" . "/tp/")))


(defvar shimbun-heise-content-start
  "\\(<!-- Meldung -->\\|<!-- INHALT -->\\)")
(defvar shimbun-heise-content-end
  "\\(<!-- untere News-Navigation -->\\|<!-- INHALT -->\\)")

(defvar shimbun-heise-x-face-alist
  '(("default" . "X-Face: #RVD(kjrS;RY\"2yH]w.1U,ZC_DbR,9{tQnhyYe|,\\J)\"\
C*o1{%`*]WwtAuo;reeq_koTr=oIKXFB4#bS'tSdz.Mc%t~-@873uYV>SMjL7D6K$M4L0Up{D\
_rBgD*Xj,t;iPKWh:!B}ijDOoCxs!}rs&(r-TLwU8=>@[w^H(>^u$wM*}\":9LANQs)1\"cZP\
6ftp?9>b&|rkGR+VWIlD:%?,Fvi8h?q2H+pVqq5#Z9*k2q7.P)0$x!A)T")))


(defvar shimbun-heise-groups
  (mapcar 'car shimbun-heise-group-path-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-heise))
  (concat shimbun-heise-url
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-heise-group-path-alist))))


(defun shimbun-heise-get-newsticker-headers (shimbun)
  (let ((regexp "<a href=\"meldung/\\([0-9]+\\)\">\\([^<]+\\)</a>")
	(from "Heise Online News <invalid@heise.de>")
	(date "") (id) (url) (subject) (headers))
    (catch 'stop
      (while (re-search-forward regexp nil t nil)
	(setq id (match-string 1))
	(setq url (w3m-expand-url (concat "meldung/" id)
				  (shimbun-index-url shimbun)))
	(setq subject (match-string 2))
	(setq id (concat "<newsticker" id "@heise.de>"))
	(when (shimbun-search-id shimbun id)
	  (throw 'stop nil))
	(message id)
	(push (shimbun-make-header
	       0 (shimbun-mime-encode-string subject)
	       (shimbun-mime-encode-string from)
	       date id "" 0 0 url)
	      headers)))
    headers))


(defun shimbun-heise-get-telepolis-headers (shimbun)
  (let ((regexp-begin "<!-- NEWS-ENTRY -->")
	(regexp-article "<a href=\"\\([^\"]+\\)\">\\([^<]+\\)</a>")
	(from "Heise Telepolis <invalid@heise.de>")
	(date "") (id) (url) (subject) (headers))
    (catch 'stop
      (while (re-search-forward regexp-begin nil t nil)
	(when (re-search-forward regexp-article nil t nil)
	  (setq url (w3m-expand-url (match-string 1) shimbun-heise-url))
	  (setq subject (match-string 2))
	  (string-match "/[0-9]+/" url)
	  (setq id (concat "<telepolis" (match-string 0 url) "@heise.de>"))
	  (when (shimbun-search-id shimbun id)
	    (throw 'stop nil))
	  (push (shimbun-make-header
		 0 (shimbun-mime-encode-string subject)
		 (shimbun-mime-encode-string from)
		 date id "" 0 0 url)
		headers))))
    headers))


(luna-define-method shimbun-get-headers
  ((shimbun shimbun-heise) &optional range)
  (if (equal (shimbun-current-group-internal shimbun) "news")
      (shimbun-heise-get-newsticker-headers shimbun)
    (shimbun-heise-get-telepolis-headers shimbun)))


(defun shimbun-heise-wash-newsticker-article (header)
  (save-excursion

    ;; get the real date
    (let ((regexp-date-begin "<!-- \\*\\*\\* tmpl \\*\\*\\* -->")
	  (regexp-date-end "<!-- obere News-Navigation -->")
	  (regexp-date (concat "\\([0-9]+\\)\\.\\([0-9]+\\)\\."
			       "\\([0-9]+\\)[ \t]+\\([0-9]+\\:[0-9]+\\)"))
	  (tmp-point) (bound-point))
      (when (setq tmp-point (re-search-forward regexp-date-begin nil t nil))
	(when (setq bound-point (re-search-forward regexp-date-end nil t nil))
	  (goto-char tmp-point)
	  (when (re-search-forward regexp-date bound-point t nil)
	    (shimbun-header-set-date
	     header
	     (shimbun-make-date-string
	      (string-to-number (match-string 3)) ; year
	      (string-to-number (match-string 2)) ; month
	      (string-to-number (match-string 1)) ; day
	      (match-string 4)			  ; time
	      ;; FIXME: timezone is always wrong, slightly better than the
	      ;; default "+0900"
	      "+0000"))))))

    ;; get the real from
    (let ((regexp-from-begin "<!-- Meldung -->")
	  (regexp-from-end "<!-- untere News-Navigation -->")
	  (regexp-from (concat "(<a href=\"mailto:\\([^@]+@ct.heise.de\\)\""
			       "[^>]*>\\([^<]+\\)</a>"))
	  (tmp-point) (bound-point))
      (when (setq tmp-point (re-search-forward regexp-from-begin nil t nil))
	(when (setq bound-point (re-search-forward regexp-from-end nil t nil))
	  (goto-char tmp-point)
	  (when (re-search-forward regexp-from bound-point t nil)
	    (shimbun-header-set-from
	     header
	     (shimbun-mime-encode-string
	      (concat "Heise Online News, "
		      (match-string 2)
		      " <"
		      (match-string 1)
		      ">")))))))

    ;; strip ads
    (goto-char (point-min))
    (let ((regexp-ad-begin "<!-- Meldung -->")
	  (regexp-ad-end "<!-- untere News-Navigation -->")
	  (regexp-ad "<!--OAS AD=\"Middle[0-9]*\"-->")
	  (tmp-point) (bound-min) (bound-max))
      (when (setq bound-min (re-search-forward regexp-ad-begin nil t nil))
	(when (setq bound-max (re-search-forward regexp-ad-end nil t nil))
	  (goto-char bound-min)
	  (while (re-search-forward regexp-ad bound-max t nil)
	    (let ((begin-region (re-search-backward "<table" bound-min t nil))
		  (end-region (re-search-forward "</table>" bound-max t nil)))
	      (when (and begin-region end-region)
		(delete-region begin-region end-region)))))))))


(defun shimbun-heise-wash-telepolis-article (header)
  (save-excursion

    ;; get real from and date
    (let ((regexp-from "<meta name=\"Author\" content=\"\\([^\"]+\\)\">")
	  (regexp-date "<meta name=\"Date\" content=\"\\([0-9]+\\)\">")
	  (date-string))
      (when (re-search-forward regexp-from nil t nil)
	(shimbun-header-set-from
	 header
	 (shimbun-mime-encode-string
	  (concat (match-string 1) " <webmaster@heise.de>"))))
      (goto-char (point-min))
      (when (re-search-forward regexp-date nil t nil)
	(setq date-string (match-string 1))
	(shimbun-header-set-date
	 header
	 (shimbun-make-date-string
	  (string-to-number (substring date-string 0 4)) ; year
	  (string-to-number (substring date-string 4 6)) ; month
	  (string-to-number (substring date-string 6 8)) ; day
	  "00:00"                                        ; time
	  ;; FIXME: timezone is always wrong, slightly better than the
	  ;; default "+0900"
	  "+0000"))))

    ;; strip nasty "download" images
    (goto-char (point-min))
    (while (re-search-forward "<!-- DL\\+CUT -->" nil t nil)
      (delete-region (point) (re-search-forward "<!-- DL\\-CUT -->"
						nil t nil)))

    ;; strip ads
    (goto-char (point-min))
    (when (search-forward "<blockquote>" nil t)
      (let ((beg (match-beginning 0))
	    (bound (search-forward "</blockquote>" nil t))
	    end)
	(when bound
	  (setq end (match-end 0))
	  (goto-char beg)
	  (when (re-search-forward
		 "<td><font size=\"-1\">Anzeige[ \t]*</font><br>" bound t)
	    (delete-region beg end)))))))


(luna-define-method shimbun-make-contents
  :before ((shimbun shimbun-heise) header)
  (if (equal (shimbun-current-group-internal shimbun) "news")
      (shimbun-heise-wash-newsticker-article header)
    (shimbun-heise-wash-telepolis-article header)))


(provide 'sb-heise)

;;; sb-heise.el ends here

;;; sb-fml.el --- shimbun backend class for fml archiver.

;; Copyright (C) 2001 Akihiro Arisawa   <ari@mbf.sphere.ne.jp>
;; Copyright (C) 2001 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Akihiro Arisawa    <ari@mbf.sphere.ne.jp>,
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

(luna-define-class shimbun-fml (shimbun) ())

(luna-define-method shimbun-index-url ((shimbun shimbun-fml))
  (shimbun-url-internal shimbun))

(defsubst shimbun-fml-parse-time (str)
  (save-match-data
    (if (string-match
	 "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\) \\([0-9]+:[0-9]+:[0-9]+\\)"
	 str)
	(shimbun-make-date-string (string-to-number (match-string 1 str))
				  (string-to-number (match-string 2 str))
				  (string-to-number (match-string 3 str))
				  (match-string 4 str))
      str)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-fml)
					 &optional range)
  (let ((case-fold-search t)
	(pages (shimbun-header-index-pages range))
	(count 0)
	headers auxs aux)
    (while (and (if pages (<= (incf count) pages) t)
		(re-search-forward "<a href=\"\\([0-9]+\\(\\.week\\|\\.month\\)?\\)/index.html\">" nil t))
      (setq auxs (append auxs (list (match-string 1)))))
    (catch 'stop
      (while auxs
	(with-temp-buffer
	  (shimbun-retrieve-url
	   (concat (shimbun-url-internal shimbun) (setq aux (car auxs)) "/")
	   'reload)
	  (subst-char-in-region (point-min) (point-max) ?\t ?  t)
	  (let ((case-fold-search t)
		id url date subject from)
	    (goto-char (point-min))
	    (while (re-search-forward
		    "<LI><A HREF=\"\\([0-9]+\\.html\\)\">Article .*</A> <DIV><SPAN CLASS=article>Article <SPAN CLASS=article-value>\\([0-9]+\\)</SPAN></SPAN> at <SPAN CLASS=Date-value>\\([^<]*\\)</SPAN> <SPAN CLASS=Subject>Subject: <SPAN CLASS=Subject-value>\\([^<]*\\)</SPAN></SPAN></DIV><DIV><SPAN CLASS=From>From: <SPAN CLASS=From-value>\\([^<]*\\)</SPAN></SPAN></DIV>"
		    nil t)
	      (setq url (concat (shimbun-url-internal shimbun)
				aux "/" (match-string 1))
		    id (format "<%s%05d%%%s>"
			       aux
			       (string-to-number (match-string 2))
			       (shimbun-current-group-internal shimbun))
		    date (shimbun-fml-parse-time (match-string 3))
		    subject (match-string 4)
		    from (match-string 5))
	      (setq subject (with-temp-buffer
			      (insert subject)
			      (shimbun-decode-entities)
			      (shimbun-remove-markup)
			      (buffer-string)))
	      (if (shimbun-search-id shimbun id)
		  (throw 'stop nil))
	      (forward-line 1)
	      (push (shimbun-make-header
		     0
		     (shimbun-mime-encode-string subject)
		     from date id "" 0 0 url)
		    headers)))
	  (setq auxs (cdr auxs)))))
    headers))

(luna-define-method shimbun-make-contents ((shimbun shimbun-fml) header)
  (catch 'stop
    (if (search-forward "<SPAN CLASS=mailheaders>" nil t)
	(delete-region (point-min) (point))
      (throw 'stop nil))
    (if (search-forward "</PRE>")
	(progn
	  (beginning-of-line)
	  (delete-region (point) (point-max)))
      (throw 'stop nil))
    (if (search-backward "</SPAN>")
	(progn
	  (beginning-of-line)
	  (delete-region (point) (save-excursion (end-of-line) (point))))
      (throw 'stop nil))
    (save-restriction
      (narrow-to-region (point-min) (point))
      (subst-char-in-region (point-min) (point-max) ?\t ?  t)
      (shimbun-decode-entities)
      (goto-char (point-min))
      (let (field value start value-beg end)
	(while (and (setq start (point))
		    (re-search-forward "<SPAN CLASS=\\(.*\\)>\\(.*\\)</SPAN>:"
				       nil t)
		    (setq field (match-string 2))
		    (re-search-forward
		     (concat "<SPAN CLASS=" (match-string 1) "-value>") nil t)
		    (setq value-beg (point))
		    (search-forward "</SPAN>" nil t)
		    (setq end (point)))
	  (setq value (shimbun-mime-encode-string
		       (buffer-substring value-beg
					 (progn (search-backward "</SPAN>")
						(point)))))
	  (delete-region start end)
	  (cond ((string= field "Date")
		 (shimbun-header-set-date header value))
		((string= field "From")
		 (shimbun-header-set-from header value))
		((string= field "Subject")
		 (shimbun-header-set-subject header value))
		((string= field "Message-Id")
		 (shimbun-header-set-id header value))
		((string= field "References")
		 (shimbun-header-set-references header value))
		(t
		 (insert (concat field ": " value "\n")))))
	(goto-char (point-min))
	(shimbun-header-insert shimbun header)
	(insert
	 "Content-Type: text/html; charset=ISO-2022-JP\nMIME-Version: 1.0\n"))
      (goto-char (point-max))))
  (insert "<PRE>\n")
  (goto-char (point-max))
  (insert "</PRE>")
  (encode-coding-string (buffer-string)
			(mime-charset-to-coding-system "ISO-2022-JP")))

(provide 'sb-fml)

;;; sb-fml.el ends here

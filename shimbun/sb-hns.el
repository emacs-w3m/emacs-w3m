;;; sb-hns.el --- shimbun backend for Hyper Nikki System.

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

(eval-and-compile
  (luna-define-class shimbun-hns (shimbun) (content-hash))
  (luna-define-internal-accessors 'shimbun-hns))

(defvar shimbun-hns-group-alist nil
  "An alist of HNS shimbun group definition.
Each element looks like (NAME URL ADDRESS).
NAME is a shimbun group name.
URL is the URL for HNS access point for the group.
ADDRESS is the e-mail address for the diary owner.")

(defvar shimbun-hns-content-hash-length 31)

(luna-define-method initialize-instance :after ((shimbun shimbun-hns)
						&rest init-args)
  (shimbun-hns-set-content-hash-internal
   shimbun
   (make-vector shimbun-hns-content-hash-length 0))
  shimbun)

(luna-define-method shimbun-groups ((shimbun shimbun-hns))
  (mapcar 'car shimbun-hns-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-hns))
  (concat (cadr (assoc (shimbun-current-group-internal shimbun)
		       shimbun-hns-group-alist))
	  "title.cgi"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-hns))
  (let ((case-fold-search t)
	id year month mday sect uniq pos subject
	headers)
    (goto-char (point-min))
    (while (re-search-forward "<a href=\"[^\\?]*\\?\\([^\\#]*#\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9]+\\)\\)\">[^<]+</a>:" nil t)
      (setq year  (string-to-number (match-string 2))
	    month (string-to-number (match-string 3))
	    mday  (string-to-number (match-string 4))
	    sect  (string-to-number (match-string 5))
	    uniq  (match-string 1)
	    pos (point))
      (when (re-search-forward "<br>" nil t)
	(setq subject (buffer-substring pos (match-beginning 0))
	      subject (with-temp-buffer
			(insert subject)
			(goto-char (point-min))
			(if (re-search-forward "[ \t\n]*" nil t)
			    (delete-region (point-min) (point)))
			(shimbun-remove-markup)
			(buffer-string))))
      (setq id (format "<%s%%%s.hns>" uniq 
		       ;; Sometimes include kanji.
		       (eword-encode-string
			(shimbun-current-group-internal
			 shimbun))))
      (push (shimbun-make-header
	     0
	     (shimbun-mime-encode-string (or subject ""))
	     (nth 2 (assoc (shimbun-current-group-internal shimbun)
			   shimbun-hns-group-alist))
	     (shimbun-make-date-string year month mday
				       (format "00:%02d" sect))
	     id "" 0 0 uniq)
	    headers))
    headers))
;    ;; sort by xref
;    (sort headers (lambda (a b) (string< (shimbun-header-xref a)
;					 (shimbun-header-xref b))))))

(defun shimbun-hns-article (shimbun xref)
  "Return article string which corresponds to SHIMBUN and XREF."
  (let (uniq start sym prefix)
    (when (string-match "#" xref)
      (setq prefix (substring xref 0 (match-end 0)))
      (if (boundp (setq sym (intern xref
				    (shimbun-hns-content-hash-internal
				     shimbun))))
	  (symbol-value sym)
	(with-current-buffer (shimbun-retrieve-url-buffer
			      (concat
			       (cadr (assoc
				      (shimbun-current-group-internal shimbun)
				      shimbun-hns-group-alist))
			       "?" xref))
	  ;; Add articles to the content hash.
	  (goto-char (point-min))
	  (while (re-search-forward 
		  "<h3 class=\"new\"><a [^<]*name=\"\\([0-9]+\\)\"" nil t)
	    (setq uniq (concat prefix (match-string 1)))
	    (when (re-search-forward "</h3>" nil t)
	      (setq start (point))
	      (when (re-search-forward "<!-- end of L?NEW -->" nil t)
		(set (intern uniq (shimbun-hns-content-hash-internal shimbun))
		     (buffer-substring start (point))))))
	  (if (boundp (setq sym (intern-soft xref
					     (shimbun-hns-content-hash-internal
					      shimbun))))
	      (symbol-value sym)))))))

(luna-define-method shimbun-article ((shimbun shimbun-hns) header
				     &optional outbuf)
  (when (shimbun-current-group-internal shimbun)
    (with-current-buffer (or outbuf (current-buffer))
      (shimbun-header-insert shimbun header)
      (insert "Content-Type: " "text/html" "; charset=ISO-2022-JP\n"
	      "MIME-Version: 1.0\n"
	      "\n"
	      (encode-coding-string
	       (shimbun-hns-article shimbun (shimbun-header-xref header))
	       (mime-charset-to-coding-system "ISO-2022-JP"))))))

(provide 'sb-hns)

;;; sb-hns.el ends here

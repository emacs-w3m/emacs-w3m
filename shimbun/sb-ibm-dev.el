;;; sb-ibm-dev.el --- shimbun backend for www-6.ibm.com/ja/developerworks

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

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

(luna-define-class shimbun-ibm-dev (shimbun) ())

(luna-define-method shimbun-index-url ((shimbun shimbun-ibm-dev))
  (shimbun-url-internal shimbun))

(defvar shimbun-ibm-dev-url "http://www-6.ibm.com/jp/developerworks/")
(defvar shimbun-ibm-dev-groups
  '("components" "java" "linux" "opensource" "security" "unicode"
    "usability" "web" "webservices" "xml"))
(defvar shimbun-ibm-dev-from-address "webmaster@www-6.ibm.com")
(defvar shimbun-ibm-dev-coding-system 'japanese-shift-jis-unix)

;;(luna-define-method shimbun-reply-to ((shimbun shimbun-ibm-dev))
;;  "")

(luna-define-method shimbun-get-headers ((shimbun shimbun-ibm-dev)
					 header &optional outbuf)
  (let* ((case-fold-search t)
	 (count 0)
	 (from (shimbun-from-address-internal shimbun))
	 (group (shimbun-current-group-internal shimbun))
	 (baseurl (shimbun-url-internal shimbun))
	 aux headers id url subject date datelist)
    (catch 'stop
      (with-temp-buffer
	(shimbun-retrieve-url (concat baseurl group "/library.html") 'reload)
	(subst-char-in-region (point-min) (point-max) ?\t ?  t)
	(goto-char (point-min))
	(while (re-search-forward
		;; getting URL and SUBJECT
		"<td width=\"100%\"><a href=\"\\(.*\\.html\\)\">\\(.*\\)</a>"
		nil t)
	  (setq url (match-string 1)
		subject (match-string 2))
	  ;; getting DATE
	  (if (re-search-forward
	       "(\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\))<br>"
	       nil t nil)
	      (setq datelist (list (string-to-number (match-string 1))
				   (string-to-number (match-string 2))
				   (string-to-number (match-string 3)))
		    date (apply 'shimbun-make-date-string datelist)))
	  ;; adjusting URL
	  (setq url (cond ((string-match
			    ;; same group
			    ;;<td width="100%"><a href="/jp/developerworks/linux/010511/j_l-p560.html">洗練されたPerl:
			    (concat "^/jp/developerworks/" group "/") url)
			   (concat baseurl group "/" url))
			  ;; other group
			  ((string-match "^/jp/developerworks/" url)
			   (concat baseurl (substring url (match-end 0))))
			  ;; relative url
			  (t (concat baseurl group "/" url))))
	  ;; building ID
	  (setq aux (if (string-match "\\([^/]+\\)\\.html" url)
			(match-string 1 url)
		      url))
	  (setq id (format "<%s%08d%%%s>" aux
			   (string-to-number
			    (mapconcat
			     'identity
			     (mapcar 'number-to-string datelist) ""))
			   group))
	  (if (shimbun-search-id shimbun id)
	      (throw 'stop nil))
	  (push (shimbun-make-header
		 0 (shimbun-mime-encode-string subject)
		 from date id "" 0 0 url)
		headers)
	  (forward-line 1))))
    headers))

(luna-define-method shimbun-article ((shimbun shimbun-ibm-dev)
				     header &optional outbuf)
  (when (shimbun-current-group-internal shimbun)
    (with-current-buffer (or outbuf (current-buffer))
      (insert
       (or (with-temp-buffer
	     (shimbun-retrieve-url (shimbun-article-url shimbun header))
	     (message "shimbun: Make contents...")
	     (goto-char (point-min))
	     (if (re-search-forward
		  "<meta http-equiv=\"refresh\" +content=\"0;URL=\\(.+\\)\">"
		  nil t nil)
		 (let ((url (match-string 1)))
		   (message "shimbun: Redirecting...")
		   ;;(shimbun-set-url-internal shimbun url)
		   (erase-buffer)
		   (shimbun-retrieve-url url 'no-cache)
		   (message "shimbun: Redirecting...done")
		   (message "shimbun: Make contents...")))
	     (goto-char (point-min))
	     (prog1 (shimbun-make-contents shimbun header)
	       (message "shimbun: Make contents...done")))
	   "")))))

(luna-define-method shimbun-make-contents ((shimbun shimbun-ibm-dev) header)
  (catch 'stop
    ;; cleaning up
    (let (beg end buffer)
      (if (re-search-forward "<!--[ 　]*Title[ 　]*-->" nil t nil)
	  (delete-region (point-min) (point))
	(throw 'stop nil))
      (while (re-search-forward "<!--[ 　]*PDF Mail[ 　]*-->" nil t nil)
	(setq beg (progn (beginning-of-line) (point))
	      end (progn (re-search-forward "</table>" nil t nil) (point)))
	(or buffer
	    (let (case-fold-search)
	      (goto-char beg)
	      (when (re-search-forward
		     "<a href=\"\\(.*\\.pdf\\)\">.+alt=\"\\(PDF *- *[0-9]+[A-Z]+\\)\".*>"
		     end t nil)
		(setq buffer (format "<a href=\"%s\">%s</a>"
				     (match-string 1) (match-string 2))))))
	(delete-region beg end))
      (goto-char (point-min))
      (if (re-search-forward "<!--[ 　]*Contents[ 　]*-->" nil t nil)
	  (delete-region (progn (beginning-of-line) (point))
			 (progn (re-search-forward "</table>" nil t nil))))
      (if (re-search-forward "<!--[ 　]*End of Contents[ 　]*-->" nil t nil)
	  (progn
	    (beginning-of-line)
	    (delete-region (point) (point-max)))
	(throw 'stop nil))
      (when buffer
	(goto-char (point-max))
	(insert buffer)))
    (goto-char (point-min))
    ;; getting SUBJECT field infomation (really necessary?  already have it)
    (if (re-search-forward "<h1>\\(.*\\)</h1>" nil t nil)
	(let (subject)
	  (setq subject (match-string 1))
	  (while (string-match "\\(<font class=\".+\">\\|</font>\\)" subject)
	    (setq subject (concat (substring subject 0 (match-beginning 0))
				  (substring subject (match-end 0)))))
	  (shimbun-header-set-subject
	   header (shimbun-mime-encode-string subject))))
    ;; getting FROM field information
    (let (author address)
      (if (re-search-forward
	   "<a href=\"#author.*\">\\(.*\\)</a> (<a href=\"mailto:\\(.*\\)\">\\2</a>) *\n*"
	   nil t nil)
	  (progn
	    (setq author (match-string 1)
		  address (match-string 2))
	    (delete-region (match-beginning 0) (match-end 0)))
	(if (re-search-forward "<a href=\"#author.*\">\\(.+\\)</a>" nil t nil)
	    (progn
	      (setq author (match-string 1))
	      (goto-char (point-min))))
	(if (re-search-forward "<a href=\"mailto:\\(.+\\)\">\\1</a>" nil t nil)
	    (setq address (match-string 1))))
      (if address
	  (shimbun-header-set-from
	   header (shimbun-mime-encode-string
		   (if author (format "%s <%s>" author address) address))))))
  (goto-char (point-min))
  (subst-char-in-region (point-min) (point-max) ?\t ?  t)
  (shimbun-decode-entities)
  (goto-char (point-min))
  (shimbun-header-insert shimbun header)
  (insert "Content-Type: text/html; charset=ISO-2022-JP\nMIME-Version: 1.0\n\n")
  (encode-coding-string
   (buffer-string) (mime-charset-to-coding-system "ISO-2022-JP")))

(provide 'sb-ibm-dev)
;;; sb-ibm-dev.el ends here

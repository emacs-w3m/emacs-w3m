;;; sb-mailman.el --- shimbun backend class for mailman archiver.

;; Copyright (C) 2002 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
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
;; Mailman is the GNU Mailing List Manager.
;; See http://www.gnu.org/software/mailman/index.html for its detail.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)

(luna-define-class shimbun-mailman (shimbun) ())

(luna-define-method shimbun-make-contents ((shimbun shimbun-mailman) header)
  (subst-char-in-region (point-min) (point-max) ?\t ?  t)
  (shimbun-decode-entities)
  (goto-char (point-min))
  (let ((end (search-forward "<!--beginarticle-->"))
	value)
    (goto-char (point-min))
    (search-forward "</HEAD>")
    (when (re-search-forward "<H1>\\([^\n]+\\)\n +</H1>" end t nil)
      (shimbun-header-set-subject
       header
       (shimbun-mime-encode-string (match-string 1))))
    (when (re-search-forward
	   "<B>\\([^\n]+\\)\n +</B> *\n +<A HREF=\"[^\n]+\n +TITLE=\"[^\n]+\">\\([^\n]+\\)"
	   end t nil)
      ;;(list (match-string 1) (match-string 2)))
      ;; ("Travis Casey" "efindel@earthlink.net")
      (shimbun-header-set-from
       header
       (shimbun-mime-encode-string (concat (match-string 1) " <" (match-string 2) ">")))
      (when (re-search-forward "<I>\\([^\n]+\\)</I>" end t nil)
	(shimbun-header-set-date header (match-string 1)))
      (delete-region (point-min) end)
      (delete-region (search-forward "<!--endarticle-->") (point-max))
      (goto-char (point-min))
      (shimbun-header-insert shimbun header)
      (insert
       "Content-Type: text/html; charset=ISO-2022-JP\nMIME-Version: 1.0\n"))
    (encode-coding-string (buffer-string)
			  (mime-charset-to-coding-system "ISO-2022-JP"))))

(luna-define-method shimbun-headers ((shimbun shimbun-mailman) &optional range)
  (with-temp-buffer
    (shimbun-retrieve-url
     (concat (shimbun-index-url shimbun) "/index.html") 'reload)
    (let ((case-fold-search t)
	  (pages (shimbun-header-index-pages range))
	  (count 0)
	  headers auxs aux)
      (while (and (if pages (<= (incf count) pages) t)
		  ;;<A href="2002-April/date.html">[ Date ]</a>
		  (re-search-forward "<a href=\"\\([0-9][0-9][0-9][0-9]-\\(January\\|February\\|March\\|April\\|May\\|June\\|July\\|August\\|September\\|October\\|November\\|December\\|\\)\\)/date.html\">" nil t))
	;; (list (match-string 1) (match-string 2))
	;;("2002-April" "April")
	(setq auxs (append auxs (list (match-string 1)))))
      (catch 'stop
	(while auxs
	  (with-temp-buffer
	    (shimbun-retrieve-url
	     (concat (shimbun-index-url shimbun)
		     "/" (setq aux (car auxs)) "/date.html")
	     'reload)
	    (subst-char-in-region (point-min) (point-max) ?\t ?  t)
	    (let ((case-fold-search t)
		  (suffix (save-match-data
			    (if (string-match "^http://\\([^/]+\\)/"
					      (shimbun-index-url shimbun))
				(match-string 1 (shimbun-index-url shimbun))
			      (shimbun-index-url shimbun)))))
	      (goto-char (point-min))
	      (while (re-search-forward
		      "<LI><A HREF=\"\\(\\([0-9]+\\)\\.html\\)\">\\([^\n]+\\)\n</A><A NAME=\"[0-9]+\">&nbsp;</A>\n<I>\\([^\n]+\\)\n</I>"
		      nil t nil)
		;;(list (match-string 1)(match-string 2)(match-string 3)(match-string 4))
		;;("000128.html" "000128" "[newbie] Palm m505 usb sync: Eureka! success" "Dan Butler")
		;;
		;;http://www.pilot-link.org/pipermail/pilot-link-general/2002-April/000122.html
		(let* ((url (concat (shimbun-index-url shimbun)
				    "/" aux "/" (match-string 1)))
		       (id (format "<%06d.%s@%s>"
				   (string-to-number (match-string 2))
				   (shimbun-current-group-internal shimbun)
				   suffix))
		       (subject (match-string 3))
		       (from (match-string 4)))
		  (if (shimbun-search-id shimbun id)
		      (throw 'stop nil))
		  (setq subject (with-temp-buffer
				  (insert subject)
				  (shimbun-decode-entities)
				  (shimbun-remove-markup)
				  (buffer-string)))
		  (push (shimbun-make-header
			 0
			 (shimbun-mime-encode-string subject)
			 from "" id "" 0 0 url)
			headers))
		(setq auxs (cdr auxs)))))))
      (nreverse headers))))

(provide 'sb-mailman)

;;; sb-mailman.el ends here

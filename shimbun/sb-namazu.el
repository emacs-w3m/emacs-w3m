;;; sb-namazu.el --- shimbun backend for namazu.org

;; Author: Akihiro Arisawa <ari@mbf.sphere.ne.jp>

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

;;; Code:

(require 'shimbun)
(require 'sb-mhonarc)

(luna-define-class shimbun-namazu (shimbun-mhonarc) ())

(defvar shimbun-namazu-url "http://www.namazu.org/ml/")
(defvar shimbun-namazu-groups '("namazu-users-ja" "namazu-win32-users-ja"
				"namazu-users-en" "namazu-devel-ja"
				"namazu-devel-en"))
(defvar shimbun-namazu-coding-system 'euc-jp)
(defvar shimbun-namazu-use-entire-index nil)

(luna-define-method shimbun-index-url ((shimbun shimbun-namazu))
  (concat (shimbun-url-internal shimbun)
	  (shimbun-current-group-internal shimbun)
	  "/"))

(luna-define-method shimbun-get-headers ((shimbun shimbun-namazu))
  (let ((headers (shimbun-namazu-get-headers-1 shimbun))
	aux)
    (if (shimbun-use-entire-index-internal shimbun)
	(while (and (re-search-forward "<A href=\"\\(mail[0-9]+.html\\)\">Next Index</A>"
				       nil t)
		    (not (string-equal (match-string 1) aux)))
	  (setq aux (match-string 1))
	  (erase-buffer)
	  (shimbun-retrieve-url (concat (shimbun-index-url shimbun) aux))
	  (setq headers (shimbun-namazu-get-headers-1 shimbun headers))))
    headers))

(defun shimbun-namazu-get-headers-1 (shimbun &optional headers)
  (while (re-search-forward
	  "<A NAME=\"\\([0-9]+\\)\" HREF=\"\\(msg[0-9]+.html\\)\"> ?\\(.*\\)</A>.*<EM>\\(.*\\)</EM>"
	  nil t)
    (let ((id (format "<%s%%%s>"
		      (match-string 1) (shimbun-current-group-internal shimbun)))
	  (url (match-string 2))
	  (subject (match-string 3))
	  (from (match-string 4)))
      (push (shimbun-make-header
	     0
	     (shimbun-mime-encode-string subject)
	     (shimbun-mime-encode-string from)
	     "" id "" 0 0 (concat (shimbun-index-url shimbun) url))
	    headers)
      (forward-line 1)))
  headers)

(provide 'sb-namazu)
;;; sb-namazu.el ends here

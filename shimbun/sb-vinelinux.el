;;; sb-vinelinux.el --- shimbun backend class for vinelinux web site.

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Keywords: news

;; Copyright:

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

(luna-define-class shimbun-vinelinux (shimbun) ())

(defconst shimbun-vinelinux-url "http://www.vinelinux.com")
(defconst shimbun-vinelinux-group-path-alist
  '(("errata.2x.i386" . "errata/2x/i386.html")
    ("errata.2x.ppc" . "errata/2x/ppc.html")
    ("errata.2x.sparc" . "errata/2x/sparc.html")
    ("errata.2x.alpha" . "errata/2x/alpha.html")
    ("errata.1x" . "errata/1x/index.html")))

(defconst shimbun-vinelinux-group-path-alist1
  '(("errata.2x.i386" . "errata/2x/")
    ("errata.2x.ppc" . "errata/2x/")
    ("errata.2x.sparc" . "errata/2x/")
    ("errata.2x.alpha" . "errata/2x/")
    ("errata.1x" . "errata/1x/")))

(defconst shimbun-vinelinux-groups
  (mapcar 'car shimbun-vinelinux-group-path-alist))
(defconst shimbun-vinelinux-coding-system 'iso-2022-jp)
(defconst shimbun-vinelinux-from-address "webmaster@www.vinelinux.com")

(defsubst shimbun-vinelinux-parse-time (str)
  (shimbun-make-date-string
   (string-to-number (substring str 0 4))
   (string-to-number (substring str 4 6))
   (string-to-number (substring str 6))))

(luna-define-method shimbun-index-url ((shimbun shimbun-vinelinux))
  (concat (shimbun-url-internal shimbun)
	  "/"
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-vinelinux-group-path-alist))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-vinelinux)
					 &optional range)
  (let ((case-fold-search t)
	(start (progn (search-forward "</font>の更新/障害情報です</h4>" nil t nil)
		      (search-forward "<table>" nil t nil)
		      (point)))
	(end (progn (search-forward "</table>" nil t nil)
		    (point)))
	;;(pages (shimbun-header-index-pages range))
	(count 0)	
	headers aux url id date subject)
    (goto-char start)
    ;; Use entire archive.
    (catch 'stop
      (while ;;(and (if pages (<= (incf count) pages) t)
	  (re-search-forward
	   ;; <td><a href="20010501.html">quota の更新</a></td>
	   ;; <td><a href="20010501-3.html">Vine Linux 2.1.5 にアップグレードした時の xdvi や tgif 等の日本語表示の不具合</a></td>
	   "^<td><a href=\"\\(\\([0-9]+\\)\\(-[0-9]+\\)*\\.html\\)\">\\(.+\\)</a></td>"
	   end t)
	;;)
	(setq url (concat shimbun-vinelinux-url
			  "/"
			  (cdr (assoc (shimbun-current-group-internal shimbun)
				      shimbun-vinelinux-group-path-alist1))
			  (match-string-no-properties 1))
	      id (format "<%s%05d%%%s>"
			 (match-string-no-properties 1)
			 (string-to-number (match-string-no-properties 2))
			 (shimbun-current-group-internal shimbun))
	      date (shimbun-vinelinux-parse-time (match-string-no-properties 2))
	      ;; from (match-string 4)
	      subject (match-string 4))
	(if (shimbun-search-id shimbun id)
	    (throw 'stop nil))
	(forward-line 1)
	(push (shimbun-make-header
	       0 (shimbun-mime-encode-string subject)
	       shimbun-vinelinux-from-address date id "" 0 0 url)
	      headers)))
    headers))

(luna-define-method shimbun-make-contents ((shimbun shimbun-vinelinux) header)
  (if (not (re-search-forward "^<h4>●[,0-9]+●.*</h4>" nil t nil))
      nil
    (delete-region (progn (forward-line 1) (point)) (point-min))
    (shimbun-header-insert shimbun header)
    (insert
     "Content-Type: text/html; charset=ISO-2022-JP\nMIME-Version: 1.0\n\n")
    (goto-char (point-min))
    (while (re-search-forward "</*blockquote>\n" nil t nil)
      (delete-region (match-beginning 0) (point)))
    (goto-char (point-max))
    (encode-coding-string (buffer-string)
			  (mime-charset-to-coding-system "ISO-2022-JP"))))

(provide 'sb-vinelinux)

;;; sb-vinelinux.el ends here

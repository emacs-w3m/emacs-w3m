;;; sb-nikkei.el --- shimbun backend for nikkei.co.jp

;; Author: Kazuyoshi KOREEDA <Kazuyoshi.Koreeda@rdmg.mgcs.mei.co.jp>

;; Keywords: news

;;; Copyright:

;; This program is free software; you can redistribute it a>nd/or modify
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

;; Original code was sb-asahi.el which is written by
;; TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp> and 
;; Yuuichi Teranishi <teranisi@gohome.org>.

;;; Code:

(require 'shimbun)
(require 'sb-text)

(luna-define-class shimbun-nikkei (shimbun shimbun-text) ())

(defvar shimbun-nikkei-url "http://www.nikkei.co.jp/news/")

(defvar shimbun-nikkei-groups '("seikei" "kaigai" "market" "sangyo" "tento" "shakai" "retto" "shasetsu" "zinzi" "okuyami"))
 
(defvar shimbun-nikkei-coding-system 'shift_jis)
(defvar shimbun-nikkei-from-address "webmaster@nikkei.co.jp")
(defvar shimbun-nikkei-content-start "<!--FJZONE START NAME=\"HONBUN\" -->")
(defvar shimbun-nikkei-content-end   "<!--FJZONE END NAME=\"HONBUN\" -->")
(defvar shimbun-nikkei-x-face-alist
  '(("default" .   "X-Face: Ygq$6P.,%Xt$U)DS)cRY@k$VkW!7(X'X'?U{{osjjFG\"E]hND;SPJ-J?O?R|a?L
        g2$0rVng=O3Lt}?~IId8Jj&vP^3*o=LKUyk(`t%0c!;t6REk=JbpsEn9MrN7gZ%")))

(luna-define-method shimbun-index-url ((shimbun shimbun-nikkei))
  (format "%s%s/index.html"
	  (shimbun-url-internal shimbun)
	  (shimbun-current-group-internal shimbun)))

(luna-define-method shimbun-article-expiration-days ((shimbun shimbun-nikkei))
  7)

(luna-define-method shimbun-get-headers ((shimbun shimbun-nikkei))
  (let ((case-fold-search t)
	p headers begin end str)
    (goto-char (point-min))
    (while (re-search-forward "<!-- aLink -->\\(<\\|<!-- \\)a href=\"\\(\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)[0-9A-Z]+\\).html\"\\(>\\| -->\\)\\([0-9][0-9]/[0-9][0-9] \\([0-9][0-9]:[0-9][0-9]\\) - \\)?<!-- headline -->\\([^<]+\\)<!-- /headline -->" nil t)
      (let* ((id (format "<%s%s>" (shimbun-current-group-internal shimbun) 
			 (match-string 2)))
	     (file (format "%s.html" (match-string 2)))
	     (year  (string-to-number (match-string 3)))
	     (month (string-to-number (match-string 4)))
	     (day   (string-to-number (match-string 5)))
	     (time  (or (match-string 8) "00:00"))
	     (group (shimbun-current-group-internal shimbun))
	     (url (format "%s/%s/%s" (shimbun-url-internal shimbun) group file))
	     (title (match-string 9)))
	(push (shimbun-make-header
	       0
	       (shimbun-mime-encode-string title)
	       (shimbun-from-address-internal shimbun)
	       (shimbun-make-date-string year month day time)
	       id "" 0 0  url)
	      headers)))
    headers))

(provide 'sb-nikkei)

;;; sb-nikkei.el ends here.

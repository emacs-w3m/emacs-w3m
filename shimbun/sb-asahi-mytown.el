;;; sb-asahi-mytown.el --- mytown.asahi.com -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003, 2004 Katsumi Yamaoka

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
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-asahi-mytown
		   (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-asahi-mytown-group-table
  '(("hokkaido" "北海道")
    ("aomori" "青森")
    ("iwate" "岩手")
    ("miyagi" "宮城")
    ("akita" "秋田")
    ("yamagata" "山形")
    ("fukushima" "福島")
    ("ibaraki" "茨城")
    ("tochigi" "栃木")
    ("gunma" "群馬")
    ("saitama" "埼玉")
    ("chiba" "千葉")
    ("tokyo" "東京")
    ("tama" "多摩")
    ("kanagawa" "神奈川")
    ("niigata" "新潟")
    ("toyama" "富山")
    ("ishikawa" "石川")
    ("fukui" "福井")
    ("yamanashi" "山梨")
    ("nagano" "長野")
    ("gifu" "岐阜")
    ("shizuoka" "静岡")
    ("aichi" "愛知")
    ("mie" "三重")
    ("shiga" "滋賀")
    ("kyoto" "京都")
    ("osaka" "大阪")
    ("hyogo" "兵庫")
    ("nara" "奈良")
    ("wakayama" "和歌山")
    ("tottori" "鳥取")
    ("shimane" "島根")
    ("okayama" "岡山")
    ("hiroshima" "広島")
    ("yamaguchi" "山口")
    ("tokushima" "徳島")
    ("kagawa" "香川")
    ("ehime" "愛媛")
    ("kochi" "高知")
    ("fukuoka" "福岡・北九州")
    ("saga" "佐賀")
    ("nagasaki" "長崎")
    ("kumamoto" "熊本")
    ("oita" "大分")
    ("miyazaki" "宮崎")
    ("kagoshima" "鹿児島")
    ("okinawa" "沖縄")
    ("usa" "ＵＳＡ"))
  "Alist of group names and their Japanese translations.")

(defvar shimbun-asahi-mytown-server-name "朝日新聞")

(defvar shimbun-asahi-mytown-top-level-domain "mytown.asahi.com"
  "Name of the top level domain for the Mytown Asahi Shimbun.")

(defvar shimbun-asahi-mytown-url
  (concat "http://" shimbun-asahi-mytown-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-asahi-mytown-from-address
  (concat "webmaster@" shimbun-asahi-mytown-top-level-domain))

(defvar shimbun-asahi-mytown-expiration-days 6)

(defvar shimbun-asahi-mytown-content-start
  "<!--[\t\n ]*FJZONE START NAME=\"\\(HONBUN\\|MIDASHI\\)\"[\t\n ]*-->")

(defvar shimbun-asahi-mytown-content-end
  "<!--[\t\n ]*FJZONE END NAME=\"HONBUN\"[\t\n ]*-->")

(defvar shimbun-asahi-mytown-x-face-alist
  '(("default" . "X-Face: +Oh!C!EFfmR$+Zw{dwWW]1e_>S0rnNCA*CX|\
bIy3rr^<Q#lf&~ADU:X!t5t>gW5)Q]N{Mmn\n L]suPpL|gFjV{S|]a-:)\\FR\
7GRf9uL:ue5_=;h{V%@()={uTd@l?eXBppF%`6W%;h`#]2q+f*81n$B\n h|t")))

(luna-define-method shimbun-groups ((shimbun shimbun-asahi-mytown))
  (mapcar 'car shimbun-asahi-mytown-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-asahi-mytown))
  (concat "マイタウン"
	  (nth 1 (assoc (shimbun-current-group-internal shimbun)
			shimbun-asahi-mytown-group-table))))

(luna-define-method shimbun-index-url ((shimbun shimbun-asahi-mytown))
  (concat shimbun-asahi-mytown-url
	  (shimbun-current-group-internal shimbun)
	  "/news_itiran.asp"))

(defun shimbun-asahi-mytown-get-headers (shimbun)
  "Return a list of headers."
  (let ((group (shimbun-current-group-internal shimbun))
	(from (shimbun-from-address shimbun))
	(case-fold-search t)
	cyear cmonth start href artnum limit month day subject year headers)
    (setq cyear (decode-time)
	  cmonth (nth 4 cyear)
	  cyear (nth 5 cyear))
    (while (and (re-search-forward "<[\t\n\r ]*a[\t\n\r ]+href=\"\
\\(news\\.asp\\?kiji=\\([0-9]+\\)\\)\"" nil t)
		(setq start (match-end 0)
		      href (match-string 1)
		      artnum (match-string 2))
		(re-search-forward "<[\t\n\r ]*/a[\t\n\r ]*>[\t\n\r ]*（\
\[\t\n\r ]*\\([0-9]+\\)[\t\n\r ]*/[\t\n\r ]*\\([0-9]+\\)" nil t)
		(setq limit (match-beginning 0)
		      month (string-to-number (match-string 1))
		      day (string-to-number (match-string 2)))
		(goto-char start)
		(re-search-forward ">\\([\t\n\r ]*<[^<>]+>\\)*[\t\n\r ]*\
\\([^<>]+\\)[\t\n\r ]*<?" limit t)
		(setq subject (match-string 2)))
      (setq year (cond ((>= (- month cmonth) 2)
			(1- cyear))
		       ((and (= 1 month) (= 12 cmonth))
			(1+ cyear))
		       (t
			cyear)))
      (push (shimbun-create-header
	     ;; number
	     0
	     ;; subject
	     subject
	     ;; from
	     from
	     ;; date
	     (shimbun-make-date-string year month day)
	     ;; id
	     (format "<%d%02d%02d.%s%%%s.%s>"
		     year month day artnum group
		     shimbun-asahi-mytown-top-level-domain)
	     ;; references, chars, lines
	     "" 0 0
	     ;; xref
	     (shimbun-expand-url (concat group "/" href)
				 shimbun-asahi-mytown-url))
	    headers))
    headers))

(luna-define-method shimbun-get-headers ((shimbun shimbun-asahi-mytown)
					 &optional range)
  (shimbun-asahi-mytown-get-headers shimbun))

(defun shimbun-asahi-mytown-prepare-article ()
  "Remove a subject line."
  (let ((case-fold-search t)
	start)
    (when (and (re-search-forward "\
<!--[\t\n\r ]*FJZONE START NAME=\"MIDASHI\"[\t\n\r ]*-->"
				  nil t)
	       (setq start (point))
	       (re-search-forward "\
<!--[\t\n\r ]*FJZONE END NAME=\"MIDASHI\"[\t\n\r ]*-->[\t\n\r ]*\
<hr[^>]*>[\t\n\r ]*"
				  nil t))
      (delete-region start (point)))))

(luna-define-method shimbun-make-contents
  :before ((shimbun shimbun-asahi-mytown) header)
  (shimbun-asahi-mytown-prepare-article))

(provide 'sb-asahi-mytown)

;;; sb-asahi-mytown.el ends here

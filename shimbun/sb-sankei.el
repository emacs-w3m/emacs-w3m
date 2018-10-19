;;; sb-sankei.el --- shimbun backend for the Sankei News -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003-2011, 2013-2018 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: news

;; This file is a part of shimbun.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'sb-multi)

(luna-define-class shimbun-sankei
		   (shimbun-japanese-newspaper shimbun-multi shimbun)
		   ())

(defvar shimbun-sankei-url "https://www.sankei.com/")

(defvar shimbun-sankei-top-level-domain "www.sankei.com")

(defvar shimbun-sankei-server-name "産経ニュース")

(defvar shimbun-sankei-group-table
  '(("top" "ニュース"
     "https://www.sankei.com/")
    ("flash" "速報"
     "https://www.sankei.com/flash/newslist/flash-n1.html")
    ("affairs" "事件"
     "https://www.sankei.com/affairs/newslist/affairs-n1.html")
    ("politics" "政治"
     "https://www.sankei.com/politics/newslist/politics-n1.html")
    ("world" "国際"
     "https://www.sankei.com/world/newslist/world-n1.html")
    ("economy" "経済"
     "https://www.sankei.com/economy/newslist/economy-n1.html")
    ("column" "コラム"
     "https://www.sankei.com/column/newslist/column-n1.html")
    ("column.sankeisyo" "産経抄"
     "https://www.sankei.com/column/newslist/sankeisyo-n1.html")
    ("column.editorial" "主張"
     "https://www.sankei.com/column/newslist/editorial-n1.html")
    ("column.seiron" "正論"
     "https://www.sankei.com/column/newslist/seiron-n1.html")
    ("sports" "スポーツ"
     "https://www.sankei.com/sports/newslist/sports-n1.html")
    ("entertainments" "エンタメ"
     "https://www.sankei.com/entertainments/newslist/entertainments-n1.html")
    ("life" "ライフ"
     "https://www.sankei.com/life/newslist/life-n1.html")
    ("region.hokkaido-tohoku" "北海道東北"
     "https://www.sankei.com/region/newslist/tohoku-n1.html")
    ("region.kanto" "関東"
     "https://www.sankei.com/region/newslist/kanto-n1.html")
    ("region.chubu" "中部"
     "https://www.sankei.com/region/newslist/chubu-n1.html")
    ("region.kinki" "近畿"
     "https://www.sankei.com/region/newslist/kinki-n1.html")
    ("region.chugoku-shikoku" "中国四国"
     "https://www.sankei.com/region/newslist/chushikoku-n1.html")
    ("region.kyushu-okinawa" "九州沖縄"
     "https://www.sankei.com/region/newslist/kyushu-n1.html")
    ("west.flash" "関西速報"
     "https://www.sankei.com/west/newslist/west-n1.html")
    ("west.affairs" "関西できごと"
     "https://www.sankei.com/west/newslist/west_affairs-n1.html")
    ("west.sports" "関西スポーツ"
     "https://www.sankei.com/west/newslist/west_sports-n1.html")
    ("west.life" "関西ライフ"
     "https://www.sankei.com/west/newslist/west_life-n1.html")
    ("west.economy" "関西経済"
     "https://www.sankei.com/west/newslist/west_economy-n1.html")))

(defvar shimbun-sankei-category-name-alist
  '(("afr" . "事件") ("clm" . "コラム") ("ecn" . "経済") ("ent" . "エンタメ")
    ("etc" . "その他") ("gqj" . "GQ JAPAN") ("lif" . "ライフ") ("plt" . "政治")
    ("prm" . "プレミアム") ("spo" . "スポーツ") ("wor" . "国際")
    ("wst" . "関西"))
  "Alist used to convert author's name in the top and the flash groups.")

(defvar shimbun-sankei-x-face-alist
  ;; Faces used for the light background display.
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAABsAAAAbBAMAAAB/+ulmAAAAD1BMVEX8/PwAAAD///+G
 d3j/AADv136FAAAAAXRSTlMAQObYZgAAAKtJREFUGNNNkN0VAiAIhfW4QMgEXhpAtA1y/5kS0
 IoHD59w+UvJLD/StdlT0n4p834o/NJTMeRA6hHhXwXTBbrEsmnOcOyzKBFxP92UWIcon26Z0S
 qgtzxBBtCiea4KMyVP1uEEEi9F232tIQwXMyBrvWVCfQoLrmVqwxpotfsvetC0zz/UwKP1vh4
 ExVTNh4ScBVvM1e6C1UjO/fZKxnpvXYjnJP5efucf+gA+DB8q52OUwwAAAABJRU5ErkJggg=="))
;;  ;; Faces used for the dark background display.
;;  '(("default" . "\
;;Face: iVBORw0KGgoAAAANSUhEUgAAABsAAAAbAgMAAADwuhzGAAAADFBMVEUAAAD///95
;; iIf/AACrdmo+AAAAAXRSTlMAQObYZgAAAJ9JREFUCNcdjzEOwjAMRb8jJZJhYEGcoKoQC
;; 0cgjGyASNWxI9zC4g5FnKGX4FqdgO9ESp4s5/9vA5AMnoWpOTeIfLdokcieVapf1Ei2wh
;; AnNoHrWO5rcnw8X28XStaBLMHkTMkJ0J6XrrFx6XJuaZGw/+4UtPj8QDakidXamXCoVNL
;; 7aqvZc+UYrOZeaFhzMwJzEG9Q3yB0U+cLQAvH+AOYXSEFLdF2GAAAAABJRU5ErkJggg=="))
  )

(defvar shimbun-sankei-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-sankei))
  (mapcar 'car shimbun-sankei-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-sankei))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-sankei-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-sankei))
  (nth 2 (assoc (shimbun-current-group-internal shimbun)
		shimbun-sankei-group-table)))

(defvar shimbun-sankei-retry-fetching 1)

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-sankei)
						 &optional range)
  (shimbun-sankei-get-headers shimbun range
			      (shimbun-current-group-internal shimbun)))

(defun shimbun-sankei-get-headers (shimbun range group)
  "Get headers for a categorized group."
  (let ((regexp
	 (eval-when-compile
	   (concat
	    "<article[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"entry[^>]+>"
	    "\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*"
	    "\\(?:<div[\t\n ]+class=\"entry_content\">[\t\n ]*\\)?"
	    "\\(?:<[^\t\n >a][^>]+>[\t\n ]*\\)*<a[\t\n ]+href=\""
	    ;; 1. url
	    "\\([^\">]+/"
	    ;; 2. year (lower 2 digits)
	    "\\([1-9][0-9]\\)"
	    ;; 3. month
	    "\\([01][0-9]\\)"
	    ;; 4. day
	    "\\([0-3][0-9]\\)"
	    "/\\(?:[^\"/]+/\\)*"
	    ;; 5. category
	    "\\([a-z]+\\)"
	    ;; 6. serial number
	    "\\([^\"/]+\\)"
	    "-n[0-9]+\\.html\\)" ;; 1. url
	    "\">[\t\n ]*"
	    ;; 7. subject
	    "\\(\\(?:[^\t\n <]+[\t\n ]\\)*[^\t\n <]+\\)")))
	(maxyear (1+ (nth 5 (decode-time))))
	(rgrp (mapconcat 'identity (nreverse (split-string group "\\.")) "."))
	(index (shimbun-index-url shimbun))
	st nd url year month day category id subj old time from headers)
    (shimbun-strip-cr)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (setq st (match-beginning 0)
	    nd (match-end 0)
	    url (match-string 1)
	    year (match-string 2)
	    month (match-string 3)
	    day (match-string 4)
	    category (match-string 5)
	    id (concat "<" category (match-string 6) "." rgrp "%"
		       shimbun-sankei-top-level-domain ">")
	    subj (match-string 7))
      (if (shimbun-search-id shimbun id)
	  (setq old t)
	(goto-char st)
	(setq time nil)
	(if (shimbun-end-of-tag "article")
	    (progn
	      (goto-char nd)
	      (if (re-search-forward "<time[\t\n ]+\
\\(?:[^\t\n >]+[\t\n ]+\\)*\\(?:datetime=\"20[1-9][0-9]-[01][0-9]-[0-3][0-9]T\
\\([0-2][0-9]:[0-5][0-9]\\)\
\\|class=\"time\"[^>]*>[\t\n ]*\\([0-2][0-9]:[0-5][0-9]\\)\\)" (match-end 2) t)
		  (setq time (or (match-string 1) (match-string 2)))
		(goto-char (match-end 0)))
	      (goto-char nd)))
	(setq from
	      (concat
	       (shimbun-server-name shimbun)
	       " ("
	       (or (and (member group '("top" "flash"))
			(cdr (assoc category
				    shimbun-sankei-category-name-alist)))
		   (shimbun-current-group-name shimbun))
	       ")"))
	(push (shimbun-create-header
	       0 subj from
	       (shimbun-make-date-string
		(min (+ 2000 (string-to-number year)) maxyear)
		(string-to-number month)
		(string-to-number day)
		time)
	       id "" 0 0
	       (shimbun-expand-url url index))
	      headers)))
    headers))

(luna-define-method shimbun-multi-next-url ((shimbun shimbun-sankei)
					    header url)
  (shimbun-sankei-multi-next-url shimbun header url))

(defun shimbun-sankei-multi-next-url (shimbun header url)
  (let (next)
    (when (and (re-search-forward "<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*\
class=\"pagination\"" nil t)
	       (shimbun-end-of-tag "div" t))
      (save-restriction
	(narrow-to-region (goto-char (match-beginning 0)) (match-end 0))
	(when (re-search-forward "<a[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*\
href=\"\\([^\"]+\\)[^>]+>[\t\n ]*Next[\t\n ]*</a>" nil t)
	  (if (> (length (setq next (match-string 1))) 1)
	      (setq next (shimbun-expand-url (match-string 1) url))
	    (setq next nil)))
	(delete-region (point-min) (point-max))
	(insert "\n")))
    (goto-char (point-min))
    next))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-sankei)
						    header)
  (shimbun-sankei-clear-contents shimbun header))

(defun shimbun-sankei-clear-contents (shimbun header)
  ;; Delete things other than the article.
  (when (and (re-search-forward "<article[\t\n >]" nil t)
	     (shimbun-end-of-tag "article"))
    (delete-region (match-end 2) (point-max))
    (delete-region (goto-char (point-min)) (match-beginning 2)))
  ;; Update Date header.
  (when (re-search-forward "<time[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*\
datetime=\"\\(20[1-9][0-9]\\)-\\([01][0-9]\\)-\\([0-3][0-9]\\)T\
\\([0-5][0-9]:[0-5][0-9]\\)" nil t)
    (shimbun-header-set-date header
			     (shimbun-make-date-string
			      (string-to-number (match-string 1))
			      (string-to-number (match-string 2))
			      (string-to-number (match-string 3))
			      (match-string 4))))
  ;; Delete `PR's
  (let (case-fold-search)
    (goto-char (point-min))
    (while (re-search-forward "<\\([a-z]+\\)[^>]*>[\t\n ]*<[^>]+>PR<" nil t)
      (goto-char (match-beginning 0))
      (if (shimbun-end-of-tag (match-string 1) t)
	  (replace-match "\n")
	(goto-char (match-end 0)))))
  ;; Collect images.
  (let (img images)
    (goto-char (point-min))
    (while (and (search-forward "<figure>" nil t)
		(shimbun-end-of-tag "figure" t))
      (save-restriction
	(narrow-to-region (goto-char (match-beginning 0)) (match-end 0))
	(when (re-search-forward "<img[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*\
src=\"\\([^\"]+\\)" nil t)
	  (setq img (match-string 1))
	  (unless (assoc img images)
	    (push (cons img (buffer-string)) images)))
	(delete-region (point-min) (goto-char (point-max)))
	(insert "\n")))
    (goto-char (point-min))
    (while (re-search-forward "\
[\t\n ]*\\(<img[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*\
src=\"\\([^\"]+\\)[^>]+>\\)[\t\n ]*" nil t)
      (setq img (match-string 2))
      (unless (assoc img images)
	(push (cons img (match-string 1)) images))
      (delete-region (match-beginning 0) (goto-char (match-end 0)))
      (insert "\n"))
    ;; Delete garbage.
    (dolist (class '("post_header" "sns" "post_footer"))
      (goto-char (point-min))
      (when (and (re-search-forward
		  (concat "<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\""
			  class "\"")
		  nil t)
		 (shimbun-end-of-tag "div" t))
	(replace-match "")
	(unless (bolp) (insert "\n"))))
    (goto-char (point-min))
    (when (and (re-search-forward "<p[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*\
class=\"pageNextsubhead\"" nil t)
	       (shimbun-end-of-tag "p" t))
      (replace-match "\n"))
    ;; Restore images.
    (when images
      (goto-char (point-min))
      (narrow-to-region (point) (point))
      (mapc (lambda (img)
	      (insert (replace-regexp-in-string
		       "[\t\n ]*<figcaption>" "<br>\n<figcaption>"
		       (replace-regexp-in-string
			"[\t\n ]*alt=\"[^\"]+[\t\n ]*" ""
			(cdr img)))
		      "<br>\n"))
	    (nreverse images))
      (widen)))

  (unless (memq (shimbun-japanese-hankaku shimbun) '(header subject nil))
    (shimbun-japanese-hankaku-buffer t))
  t)

(luna-define-method shimbun-footer :around ((shimbun shimbun-sankei)
					    header &optional html)
  (concat "<div align=\"left\">\n--&nbsp;<br>\n\
この記事の著作権は産経新聞社に帰属します。オリジナルはこちら：<br>\n\
<a href=\""
	  (shimbun-article-base-url shimbun header) "\">&lt;"
	  (shimbun-article-base-url shimbun header) "&gt;</a>\n</div>\n"))

(provide 'sb-sankei)

;;; sb-sankei.el ends here

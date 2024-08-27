;;; sb-sankei.el --- shimbun backend for the Sankei News -*- lexical-binding: nil -*-

;; Copyright (C) 2003-2011, 2013-2019, 2021-2024 Katsumi Yamaoka

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

(require 'shimbun)
(require 'sb-multi)

(luna-define-class shimbun-sankei (shimbun-japanese-newspaper shimbun-multi
							      shimbun) ())

(defvar shimbun-sankei-url "https://www.sankei.com/")

(defvar shimbun-sankei-top-level-domain "www.sankei.com")

(defvar shimbun-sankei-server-name "産経ニュース")

(defvar shimbun-sankei-group-table
  '(("top" "ニュース"
     "https://www.sankei.com/")
    ("flash" "速報"
     "https://www.sankei.com/flash/")
    ("affairs" "社会"
     "https://www.sankei.com/affairs/")
    ("politics" "政治"
     "https://www.sankei.com/politics/")
    ("world" "国際"
     "https://www.sankei.com/world/")
    ("economy" "経済"
     "https://www.sankei.com/economy/")
    ("sports" "スポーツ"
     "https://www.sankei.com/sports/")
    ("entertainments" "エンタメ"
     "https://www.sankei.com/entertainments/")
    ("life" "ライフ"
     "https://www.sankei.com/life/")
    ("column" "コラム"
     "https://www.sankei.com/column/")
    ("column.editorial" "主張"
     "https://www.sankei.com/column/editorial/")
    ("column.seiron" "正論"
     "https://www.sankei.com/column/seiron/")
    ("column.sankeisyo" "産経抄"
     "https://www.sankei.com/column/sankeisyo/")
    ("column.naniwa" "浪速風"
     "https://www.sankei.com/column/naniwa/")
    ("west" "産経WEST"
     "https://www.sankei.com/west/")
    ("west.essay" "朝晴れエッセー"
     "https://www.sankei.com/tag/series/etc_21/")))

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

(defvar shimbun-sankei-login-url "https://special.sankei.com/login"
  "*Url to login to special.sankei.com.")

(defvar shimbun-sankei-logout-url "https://special.sankei.com/logout"
  "*Url to logout from special.sankei.com.")

(defcustom shimbun-sankei-login-name nil
  "Login name used to login to special.sankei.com.
To use this, set both `w3m-use-cookies' and `w3m-use-form' to t."
  :group 'shimbun
  :type '(choice (const :tag "None" nil) (string :tag "User name")))

(defcustom shimbun-sankei-login-password nil
  "Password used to login to special.sankei.com.
To use this, set both `w3m-use-cookies' and `w3m-use-form' to t."
  :group 'shimbun
  :type '(choice (const :tag "None" nil) (string :tag "Password")))

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
  (shimbun-sankei-get-headers shimbun range))

(defun shimbun-sankei-get-headers (shimbun range)
  "Get headers for the group that SHIMBUN specifies in RANGE."
  (let ((group (shimbun-current-group-internal shimbun))
	regexp)
    (if (member group '("column.sankeisyo"))
	(shimbun-sankei-get-headers-ranking shimbun range group)
      ;; Ignore articles of which the title does not match this regexp.
      (let ((subj-re (cdr (assoc group
				 '(("column.editorial" . "主張")
				   ("column.seiron" . "正論")
				   ("column.naniwa" . "浪速風")
				   ("west.essay" . "朝晴れエッセー")))))
	    url id ids nd subject date names headers)
	(when subj-re
	  (setq subj-re (format
			 "\\`\\(?:[〈＜]\\|&lt;\\)%s\\(?:[〉＞]\\|&gt;\\)"
			 subj-re)))
	(goto-char (point-min))
	(while (re-search-forward "<div[^>]* class=[^>]*[ \"]order-1[ \"]"
				  nil t)
	  (when (shimbun-end-of-tag "div")
	    (narrow-to-region (goto-char (match-beginning 2)) (match-end 2))
	    (when (re-search-forward "<a[^>]* href=\"\\(/article/[0-9]\\{8\\}-\
\\([0-9A-Z]\\{26\\}\\)/\\)" nil t)
	      (setq url (match-string 1)
		    id (concat "<" (match-string 2) "."
			       (mapconcat #'identity
					  (nreverse (split-string group "\\."))
					  ".")
			       "%" shimbun-sankei-top-level-domain ">"))
	      (unless (or (member id ids)
			  (progn (push id ids)
				 (shimbun-search-id shimbun id)))
		(when (shimbun-end-of-tag "a")
		  (goto-char (match-beginning 2))
		  (setq nd (match-end 2))
		  (when (and (re-search-forward
			      "\\(?:[^>]*<[^>]*>\\)*[\t\n ]*\\([^<]+\\)" nd t)
			     (progn
			       (setq subject (replace-regexp-in-string
					      "[\t\n 　]+\\'" ""
					      (match-string 1)))
			       (or (not subj-re)
				   (string-match subj-re subject))))
		    ;; Add the author's name to the subject string if possible.
		    (goto-char (point-min))
		    (when (re-search-forward "[\t\n ]class=\"subheadline\"\
[^>]*>[\t\n ]*\\([^<]+\\)</" nil t)
		      (setq subject (concat subject " " (match-string 1))))
		    (goto-char (point-min))
		    (when (and (re-search-forward "<time[^>]* dateTime=\"\
\\(20[2-9][0-9]-[01][0-9]-[0-3][0-9]T\
[012][0-9]:[0-5][0-9]:[0-5][0-9]\\(?:\\.[0-9]+\\)?Z\\)" nil t)
			       (ignore-errors
				 (setq date (decode-time (date-to-time
							  (match-string 1))))))
		      (setq names nil)
		      (while (re-search-forward
			      "<a[\t\n ]href=\"/[^\"]+/\"[^>]+>\\([^<]+\\)</a>"
			      nil t)
			(setq names
			      (nconc names
				     (split-string (match-string 1)
						   "[ \f\t\n\r\v・]+" t))))
		      (and names
			   (setq names
				 (replace-regexp-in-string
				  "\\`[\t 　]+" ""
				  (mapconcat #'identity
					     ;;(last names 2)
					     names
					     " ")))
			   ;; Ignore too long attributes.
			   (when (string-match "[、。]" names)
			     (setq names nil)))
		      (unless names
			(setq names
			      (if (equal group "top")
				  "トップ"
				(cadr (assoc group
					     shimbun-sankei-group-table)))))
		      (push (shimbun-create-header
			     0 subject
			     (concat shimbun-sankei-server-name
				     (if names
					 (concat " (" names ")")
				       ""))
			     (shimbun-make-date-string
			      (nth 5 date) (nth 4 date) (nth 3 date)
			      (apply #'format "%02d:%02d:%02d"
				     (last (nreverse date) 3)))
			     id "" 0 0
			     (shimbun-expand-url url shimbun-sankei-url))
			    headers)))))))
	  (goto-char (point-max))
	  (widen))
	(shimbun-sort-headers headers)))))

(defun shimbun-sankei-get-headers-ranking (shimbun range group)
  "Get headers for the GROUP that SHIMBUN specifies in RANGE.
This function looks for the articles in only the ranking block."
  ;; Ignore articles of which the title does not match this regexp.
  (let ((regexp (cdr (assoc group '(("column.sankeisyo" . "\\`＜産経抄＞")))))
	subject url date id ids name headers)
    (goto-char (point-min))
    (while (re-search-forward "<li[^>]* class=[^>]*[ \"]ranking__item[ \"]"
			      nil t)
      (when (shimbun-end-of-tag "li")
	(narrow-to-region (goto-char (match-beginning 2)) (match-end 2))
	(when (and (re-search-forward "<a[^>]* href=\
\"\\(\\(?:https://www\\.sankei\\.com\\)?/article/\
\\(20[2-9][0-9]\\)\\([01][0-9]\\)\\([0-3][0-9]\\)-\
\\([0-9A-Z]\\{26\\}\\)/\\)" nil t)
		   (progn
		     (goto-char (1+ (match-beginning 0)))
		     (save-match-data
		       (w3m-parse-attributes (title)
			 (string-match regexp (setq subject title))))))
	  (setq url (match-string 1)
		date (list (match-string 2) (match-string 3) (match-string 4))
		id (concat "<" (match-string 5) "."
			   (mapconcat #'identity
				      (nreverse (split-string group "\\."))
				      ".")
			   "%" shimbun-sankei-top-level-domain ">"))
	  (unless (or (member id ids)
		      (progn (push id ids)
			     (shimbun-search-id shimbun id)))
	    (setq name (cadr (assoc group shimbun-sankei-group-table)))
	    (push (shimbun-create-header
		   0 subject
		   (concat shimbun-sankei-server-name " (" name ")")
		   (shimbun-make-date-string
		    (string-to-number (car date))
		    (string-to-number (cadr date))
		    (string-to-number (caddr date)))
		   id "" 0 0
		   (shimbun-expand-url url shimbun-sankei-url))
		  headers)))
	(goto-char (point-max))
	(widen)))
    (shimbun-sort-headers headers)))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-sankei)
						    header)
  (shimbun-sankei-clear-contents shimbun header))

(defun shimbun-sankei-clear-contents (shimbun header)
  "Collect contents and create an html page in the current buffer."
  (if (zerop (buffer-size))
      (insert "お探しのページは見つかりませんでした。<br>\n"
	      "ページが削除されたか移動した可能性があります。\n")
    (let (headline author restrictions st nd tem ids simgs id caption img
		 contents eimgs maxwidth fn)
      (goto-char (point-min))
      (when (re-search-forward
	     "[\t\n ]class=\"article-subheadline\"[^>]*>[\t\n ]*\\([^<]+\\)"
	     nil t)
	(setq headline (match-string 1)))
      (goto-char (point-min))
      (when (or (and (re-search-forward "<a[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*\
\\(?:class=\"gtm-click author-name\"\\|href=\"/author/\
\\|data-gtm-action=\"move to author page\"\
\\|data-gtm-label=\"article header author link\\)" nil t)
		     (shimbun-end-of-tag "a")
		     (setq author (match-string 2)))
		(and (progn
		       (goto-char (point-min))
		       (re-search-forward "\
{[^{}]*\"original\":{[^{}]*\"byline\":\"\\([^\"}]+\\)\"" nil t))
		     (setq author (match-string 1))))
	(setq author
	      (replace-regexp-in-string
	       "\\`[\t 　]+\\|\\(\\cj\\)[\t 　]+\\(\\cj\\)\\|[\t 　]+\\'"
	       "\\1\\2" author)))
      (goto-char (point-min))
      (when (and (re-search-forward "<span[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*\
class=\"restrictions\"" nil t)
		 (shimbun-end-of-tag "span")
		 (progn
		   (goto-char (match-beginning 1))
		   (re-search-forward "<span[\t\n >]" (match-end 1) t)
		   (shimbun-end-of-tag "span")))
	(setq restrictions (match-string 2)))
      (goto-char (point-min))
      (when (and (re-search-forward ";Fusion.globalContent=\\({\\)" nil t)
		 (setq st (match-beginning 1)
		       nd (ignore-errors (copy-marker (scan-sexps st 1)))))
	(when (re-search-forward ",\"promo_items\":\\({\\)" nd t)
	  (ignore-errors ;; The other headlines are there.
	    (delete-region (match-beginning 0)
			   (scan-sexps (match-beginning 1) 1))))
	(setq nd (prog1 (marker-position nd) (set-marker nd nil)))
	(unless headline
	  (goto-char nd)
	  (when (and (re-search-backward ",\"headlines\":{\"basic\":\\(\"\\)"
					 st t)
		     (progn
		       (setq tem (ignore-errors
				   (replace-regexp-in-string
				    "\\`[\t 　]+\\|[\t 　]+\\'" ""
				    (read (nth 2 (match-data))))))
		       (not (zerop (length tem)))))
	    (setq headline tem)))
	(goto-char st)
	(when (re-search-forward ",\"content_elements\":\\(\\[\\)" nd t)
	  (ignore-errors
	    (setq st (match-end 0))
	    (setq nd (1- (scan-sexps (match-beginning 1) 1)))))
	(goto-char (point-min))
	(setq ids (shimbun-sankei-extract-images st nil)
	      simgs (car ids)
	      ids (cadr ids))
	(save-restriction
	  (narrow-to-region (goto-char st) nd)
	  (while (re-search-forward "{\"_id\":\"\\([^\"]\\{26\\}\\)\"" nil t)
	    (setq st (goto-char (match-beginning 0))
		  nd (match-end 0)
		  id (match-string 1))
	    (if (ignore-errors (setq nd (scan-sexps st 1)))
		(cond
		 ((search-forward "\"type\":\"image\"" nd t)
		  (goto-char st)
		  (setq caption
			(and (re-search-forward "\"caption\":\\(\"\\)" nd t)
			     (setq tem (ignore-errors
					 (replace-regexp-in-string
					  "\\`[\t 　]+\\|[\t 　]+\\'" ""
					  (read (nth 2 (match-data))))))
			     (not (zerop (length tem)))
			     tem))
		  (if (member id ids)
		      (goto-char nd)
		    (goto-char st)
		    (setq img (and (re-search-forward
				    "\"type\":\"image\",\"url\":\\(\"\\)"
				    nd t)
				   (ignore-errors
				     (read (nth 2 (match-data))))))
		    (goto-char st)
		    (and (or (re-search-forward "\"articleLarge\":\\(\"\\)"
						nd t)
			     (re-search-forward "\"articleSmall\":\\(\"\\)"
						nd t)
			     ;; very large
			     (and (not img)
				  (re-search-forward
				   "\"type\":\"image\",\"url\":\\(\"\\)" nd t))
			     ;; portrait is trimmed?
			     (re-search-forward
			      "\"articleSnsShareImage\":\\(\"\\)" nd t))
			 (setq tem (ignore-errors
				     (read (nth 2 (match-data)))))
			 (progn
			   (push id ids)
			   (push (concat (if img
					     (concat "<a href=\"" img "\">")
					   "")
					 "<img src=\"" tem
					 "\" alt=\"[写真]\">"
					 (if img "</a>" "")
					 (if caption
					     (concat "<br>\n" caption)
					   ""))
				 contents)))))
		 ((search-forward "\"type\":\"raw_html\"" nd t)
		  (goto-char st)
		  (if (and (re-search-forward "\"content\":\\(\"\\)" nd t)
			   (setq tem (ignore-errors
				       (read (nth 2 (match-data))))))
		      (with-temp-buffer
			(insert tem)
			(shimbun-strip-cr)
			(goto-char (point-min))
			(while (and (re-search-forward "\
<div[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*class=\"sankei_netshop\"" nil t)
				    (shimbun-end-of-tag "div" t))
			  (delete-region (match-beginning 0) (match-end 0))
			  (insert "\n"))
			(goto-char (point-min))
			(while (re-search-forward "\
[\t\n 　]*\\(?:<\\(?:br\\|/?p\\)>[\t\n 　]*\\)+" nil t)
			  (replace-match "\n\n"))
			(goto-char (point-min))
			(while (re-search-forward "^[\t 　]+\\|[\t 　]+$"
						  nil t)
			  (delete-region (match-beginning 0) (match-end 0)))
			(goto-char (point-min))
			(while (and (re-search-forward "<img[\t ]" nil t)
				    (shimbun-end-of-tag))
			  (goto-char (match-beginning 0))
			  (unless (save-match-data
				    (re-search-forward "[\t ]alt=\""
						       (match-end 0) 'move))
			    (forward-char -1)
			    (insert " alt=\"[写真]\"")))
			(goto-char (point-min))
			(while (re-search-forward ">$" nil t)
			  (or (looking-at "\n\n")
			      (looking-back "<br>" nil)
			      (insert "<br>")))
			(when (setq tem (split-string (buffer-string) "\n\n+"
						      t))
			  (setq contents (nconc (nreverse tem) contents)))))
		  (goto-char nd))
		 ((search-forward "\"type\":\"interstitial_link\"" nd t)
		  (goto-char st)
		  (and (setq caption (and (re-search-forward
					   "\"content\":\\(\"\\)" nd t)
					  (ignore-errors
					    (replace-regexp-in-string
					     "\\`[\t 　]+\\|[\t 　]+\\'" ""
					     (read (nth 2 (match-data)))))))
		       (not (zerop (length caption)))
		       (progn
			 (goto-char st)
			 (setq tem (and (re-search-forward "\"url\":\\(\"\\)"
							   nd t)
					(ignore-errors
					  (read (nth 2 (match-data)))))))
		       (push (concat "<a href=\"" tem "\">" caption "</a>")
			     contents)))
		 ((re-search-forward
		   "\"raw_oembed\":{\"html\":\\(\"<iframe[\t\n ]+\\)" nd t)
		  (when (setq tem (ignore-errors (read (nth 2 (match-data)))))
		    (setq tem (replace-regexp-in-string
			       ">[^<]*</iframe" ">[動画]</iframe" tem))
		    (push (if (string-match "[\t\n ]src=\"\\([^ \">?]+\\)" tem)
			      (concat "<a href=\"" (match-string 1 tem) "\">"
				      tem "</a>")
			    tem)
			  contents)))
		 (t
		  (if (and (re-search-forward "\"content\":\\(\"\\)" nd t)
			   (setq tem (ignore-errors
				       (read (nth 2 (match-data))))))
		      (progn
			(setq tem (replace-regexp-in-string
				   "\\`[\t 　]+\\|[\t 　]+\\'" "" tem))
			(unless (or (zerop (length tem))
				    ;; <br/> only case
				    (string-match "\\`<[^>]*>\\'" tem))
			  (push tem contents)))
		    (goto-char nd))))
	      (goto-char nd))))
	(goto-char nd)
	(setq eimgs (car (shimbun-sankei-extract-images nil ids)))
	(erase-buffer)
	(when (and author headline
		   (string-match (regexp-quote author) headline))
	  (setq author nil))
	(if headline
	    (insert "<p>" headline
		    (if restrictions
			(concat "<br>\n--- " restrictions " ---"
				(if author
				    (concat " (" author ")") ""))
		      (if author
			  (concat "<br>\n(" author ")") ""))
		    "</p>\n")
	  (if restrictions
	      (insert "<p>--- " restrictions " ---"
		      (if author
			  (concat " (" author ")") "")
		      "</p>\n")
	    (when author
	      (insert "<p>(" author ")</p>\n"))))
	(when simgs
	  (insert "<p>" (mapconcat #'identity (nreverse simgs) "</p>\n<p>")
		  "</p>\n"))
	(when contents
	  (setq maxwidth (max (- (window-width) 10) 10))
	  (if (string-match "産経抄\\|浪速風"
			    (shimbun-header-from-internal header))
	      (setq fn (lambda (str &optional last)
			 (when (eq (aref str 0) ?▼)
			   (setq str (substring str 1)))
			 (if (or (not (eq (char-syntax (aref str 0)) ?w))
				 (eq (aref str (1- (length str))) ?>))
			     (concat str "<br>" (if last "" "<br>"))
			   (concat (if last "" "<p>")
				   (if (and (string-match "[,.、。]" str)
					    (>= (string-width str) maxwidth))
				       "　" "")
				   str (if last "" "</p>")))))
	    (setq fn (lambda (str &optional last)
		       (if (or (not (eq (char-syntax (aref str 0)) ?w))
			       (eq (aref str (1- (length str))) ?>))
			   (concat str "<br>" (if last "" "<br>"))
			 (concat (if last "" "<p>")
				 (if (and (string-match "[,.、。]" str)
					  (>= (string-width str) maxwidth))
				     "　" "")
				 str (if last "" "</p>"))))))
	  (if eimgs
	      (insert (mapconcat fn (nreverse contents) "\n") "\n")
	    (when (cdr contents)
	      (insert (mapconcat fn (reverse (cdr contents)) "\n") "\n"))
	    (insert (funcall fn (car contents) t) "\n")))
	(when eimgs
	  (when (cdr eimgs)
	    (insert "<p>" (mapconcat #'identity (reverse (cdr eimgs))
				     "</p>\n<p>")
		    "</p>\n"))
	  (insert (car eimgs) "\n"))
	(unless (memq (shimbun-japanese-hankaku shimbun) '(header subject nil))
	  (shimbun-japanese-hankaku-buffer t))
	t))))

(defun shimbun-sankei-extract-images (end ids)
  "Extract images existing in the area from the current position to END.
END defaults to (point-max).  Image of which ID is in IDS is ignored.
Return a list of images and IDS."
  (let (img nd id to images)
    (while (and (re-search-forward
		 "<figure[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*\
class=\"\\(?:[^\t\n \"]+[\t\n ]+\\)*article-image[\t\n >]+" end t)
		(shimbun-end-of-tag "figure"))
      (setq img (match-string 0)
	    nd (match-end 0))
      (goto-char (match-beginning 0))
      (when (or (re-search-forward "<a[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*\
href=\"[^\"]+/photo/\\([0-9A-Z]\\{26\\}\\)/" nd t)
		(re-search-forward "<img[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*\
src=\"[^\"]+/\\([0-9A-Z]\\{26\\}\\)\\.[^\"]+\"" nd t))
	(unless (member (setq id (match-string 1)) ids)
	  (push id ids)
	  (with-temp-buffer
	    (insert img)
	    (goto-char (point-min))
	    (when (and (re-search-forward "<img[\t\n ]+" nil t)
		       (shimbun-end-of-tag))
	      (setq to (match-end 0))
	      (goto-char (match-beginning 0))
	      (if (re-search-forward "alt=\"\\([^\">]*\\)\"" to t)
		  (replace-match "[写真]" nil nil nil 1)
		(goto-char (1- to))
		(insert " alt=\"[写真]\""))
	      (push (buffer-string) images)))))
      (goto-char nd))
    (list images ids)))

(luna-define-method shimbun-footer :around ((shimbun shimbun-sankei)
					    header &optional html)
  (concat "<div align=\"left\">\n--&nbsp;<br>\n\
この記事の著作権は産経新聞社に帰属します。オリジナルはこちら：<br>\n\
<a href=\""
	  (shimbun-article-base-url shimbun header) "\">&lt;"
	  (shimbun-article-base-url shimbun header) "&gt;</a>\n</div>\n"))

(luna-define-method shimbun-multi-next-url ((shimbun shimbun-sankei)
					    header url)
  (shimbun-sankei-multi-next-url shimbun header url))

(defun shimbun-sankei-multi-next-url (shimbun header url)
  (goto-char (point-min))
  (when (and (re-search-forward "<a\\(?:[\t\n ]+[^\t\n >]+\\)*\
\\(?:[\t\n ]+data-gtm-action=\"move to next article page\"\
\\|>[\t\n ]*続きを見る[\t\n ]*<\\)" nil t)
	     (progn
	       (goto-char (match-beginning 0))
	       (re-search-forward "href=\"\\([^\"]+\\)" nil t)))
    (shimbun-expand-url (match-string 1) url)))

(luna-define-method shimbun-multi-clear-contents :around ((shimbun
							   shimbun-sankei)
							  header
							  has-previous-page
							  has-next-page)
  (shimbun-sankei-multi-clear-contents shimbun header
				       has-previous-page has-next-page))

(defun shimbun-sankei-multi-clear-contents (shimbun header
						    has-previous-page
						    has-next-page)
  (when (luna-call-next-method)
    (when has-previous-page
      (goto-char (point-min))
      (insert "&#012;\n"))
    t))

(eval-when-compile
  (require 'w3m-cookie)
  (require 'w3m-form))

(declare-function w3m-cookie-save "w3m-cookie" (&optional domain))
(autoload 'w3m-cookie-setup "w3m-cookie")

(autoload 'password-cache-add "password-cache")
(autoload 'password-read-from-cache "password-cache")
(autoload 'timezone-make-date-arpa-standard "timezone")

(defun shimbun-sankei-login (&optional name password interactive-p)
  "Login to special.sankei.com with NAME and PASSWORD.
NAME and PASSWORD default to `shimbun-sankei-login-name' and
`shimbun-sankei-login-password' respectively.  `password-data', if
cached, overrides `shimbun-sankei-login-password'.  If the prefix
argument is given, you will be prompted for new NAME and PASSWORD."
  (interactive (let ((pass shimbun-sankei-login-password)
		     name default password)
		 (unless (and w3m-use-cookies w3m-use-form)
		   (error "\
You should set `w3m-use-cookies' and `w3m-use-form' to non-nil"))
		 (setq name (if current-prefix-arg
				(completing-read
				 "Login name: "
				 (cons shimbun-sankei-login-name nil)
				 nil nil shimbun-sankei-login-name)
			      shimbun-sankei-login-name))
		 (when (and name (string-match "\\`[\t ]*\\'" name))
		   (setq name nil))
		 (setq default (and name (or (password-read-from-cache name)
					     shimbun-sankei-login-password))
		       password (and name
				     (if current-prefix-arg
					 (read-passwd
					  (concat "Password"
						  (when default
						    (concat " (default "
							    (make-string
							     (length default)
							     ?*)
							    ")"))
						  ": ")
					  nil default)
				       default)))
		 (when (and password (string-match "\\`[\t ]*\\'" password))
		   (setq name nil
			 password nil))
		 (list name password t)))
  (unless interactive-p
    (if (or name (setq name shimbun-sankei-login-name))
	(or password
	    (setq password (or (password-read-from-cache name)
			       shimbun-sankei-login-password))
	    (setq name nil))
      (setq password nil)))
  (if (not (and w3m-use-cookies w3m-use-form name password))
      (when interactive-p (message "Quit"))
    (when interactive-p (message "Logging in to special.sankei.com..."))
    (require 'w3m-cookie)
    ;; Delete old login/out cookies.
    (w3m-cookie-setup)
    (let ((case-fold-search t))
      (dolist (cookie w3m-cookies)
	(when (or (string-match
		   "\\.sankei\\.\\(?:com\\|.+log\\(?:in\\|out\\)\\)"
		   (w3m-cookie-url cookie))
		  (and (equal "AKA_A2" (w3m-cookie-name cookie))
		       (equal "sankei.com" (w3m-cookie-domain cookie))))
	  (setq w3m-cookies (delq cookie w3m-cookies)))))
    (require 'w3m-form)
    (w3m-arrived-setup)
    (let ((cache (buffer-live-p w3m-cache-buffer))
	  (w3m-message-silent t)
	  w3m-clear-display-while-reading next forms form action handler)
      (condition-case err
	  (with-temp-buffer
	    (w3m-process-with-wait-handler
	      (w3m-retrieve-and-render shimbun-sankei-login-url
				       t nil nil nil handler))
	    (goto-char (point-min))
	    (when (re-search-forward "^Location:[\t\n\r ]+\\(http[^\n]+\\)"
				     nil t)
	      (setq next (match-string-no-properties 1))
	      (w3m-process-with-wait-handler
		(w3m-retrieve-and-render next t nil nil nil handler))
	      (goto-char (point-min))
	      (when (re-search-forward
		     "^You were redirected to:[\t\n\r ]+\\(http[^\n]+\\)"
		     nil t)
		(setq next (match-string-no-properties 1))
		(w3m-process-with-wait-handler
		  (w3m-retrieve-and-render next t nil nil nil handler))))
	    (setq forms w3m-current-forms)
	    (while forms
	      (setq form (car forms)
		    forms (cdr forms)
		    action (w3m-form-action form))
	      (unless (and action (string-match "login\\.php\\'" action))
		(setq form nil)))
	    (if (not form)
		(when interactive-p (message "Failed to login"))
	      (setq form (w3m-form-make-form-data form))
	      (while (string-match "\
&\\(?:LOGIN_ID\\|LOGIN_PASSWORD\\|STAY_LOGGED_IN\\)=[^&]*" form)
		(setq form (replace-match "" nil nil form)))
	      (setq form (concat form
				 "&LOGIN=&LOGIN_ID="
				 (shimbun-url-encode-string name)
				 "&LOGIN_PASSWORD="
				 (shimbun-url-encode-string password)
				 "&STAY_LOGGED_IN=1"))
	      (w3m-process-with-wait-handler
		(w3m-retrieve-and-render action t nil form nil handler))
	      (setq form (car w3m-current-forms))
	      (if (not (string-match "login\\'"
				     (setq action (w3m-form-action form))))
		  (when interactive-p (message "Failed to login"))
		(setq form (w3m-form-make-form-data form))
		(w3m-process-with-wait-handler
		  (w3m-retrieve-and-render action t nil form nil handler)))
	      (if (not (and w3m-current-url
			    (string-match
			     "\\`https://www.sankei.com/\\?[0-9]+\\'"
			     w3m-current-url)))
		  (when interactive-p (message "Failed to login"))
		(when interactive-p (message "Logged in"))
		;; Use a copy of the password so not to be expired by C-@s.
		(password-cache-add name (copy-sequence password))
		(when w3m-cookie-save-cookies (w3m-cookie-save))))
	    (when (get-buffer " *w3m-cookie-parse-temp*")
	      (kill-buffer (get-buffer " *w3m-cookie-parse-temp*")))
	    (unless cache (w3m-cache-shutdown)))
	(error (if (or interactive-p debug-on-error)
		   (signal (car err) (cdr err))
		 (message "Error while logging in to special.sankei.com:\n %s"
			  (error-message-string err))))))))

(defun shimbun-sankei-logout (&optional interactive-p)
  "Logout from special.sankei.com."
  (interactive (list t))
  (require 'w3m-cookie)
  ;; Delete old login/out cookies.
  (w3m-cookie-setup)
  (let ((case-fold-search t))
    (dolist (cookie w3m-cookies)
      (when (or (string-match "\\.sankei\\.\\(?:com\\|.+log\\(?:in\\|out\\)\\)"
			      (w3m-cookie-url cookie))
		(and (equal "AKA_A2" (w3m-cookie-name cookie))
		     (equal "sankei.com" (w3m-cookie-domain cookie))))
	(setq w3m-cookies (delq cookie w3m-cookies)))))
  (require 'w3m-form)
  (w3m-arrived-setup)
  (let ((cache (buffer-live-p w3m-cache-buffer))
	(w3m-message-silent t)
	(next shimbun-sankei-logout-url)
	w3m-clear-display-while-reading done handler)
    (when interactive-p (message "Logging out from special.sankei.com..."))
    (condition-case err
	(with-temp-buffer
	  (while (not done)
	    (w3m-process-with-wait-handler
	      (w3m-retrieve-and-render next t nil nil nil handler))
	    (goto-char (point-min))
	    (if (re-search-forward "\
^\\(?:Location\\|You were redirected to\\):[\t\n\r ]+\\(http[^\n]+\\)" nil t)
		(setq next (match-string-no-properties 1))
	      (w3m-process-with-wait-handler
		(w3m-retrieve-and-render next t nil nil nil handler))
	      (when w3m-cookie-save-cookies (w3m-cookie-save))
	      (when interactive-p (message "Logged out"))
	      (setq done t)))
	  (when (get-buffer " *w3m-cookie-parse-temp*")
	    (kill-buffer (get-buffer " *w3m-cookie-parse-temp*")))
	  (unless cache (w3m-cache-shutdown)))
      (error (if (or interactive-p debug-on-error)
		 (signal (car err) (cdr err))
	       (message "Error while logging out from special.sankei.com:\n %s"
			(error-message-string err)))))))

(defun shimbun-sankei-keep-login (&optional force)
  "Keep logging in in Sankei."
  (interactive (list t))
  (when (and w3m-use-cookies (progn (w3m-cookie-setup) t)
	     w3m-use-form
	     shimbun-sankei-login-name shimbun-sankei-login-password
	     (or force
		 (let ((cookies w3m-cookies) cookie expiry)
		   (while cookies
		     (setq cookie (pop cookies))
		     (and (equal "AKA_A2" (w3m-cookie-name cookie))
			  (equal "sankei.com" (w3m-cookie-domain cookie))
			  (setq cookies nil
				expiry (w3m-cookie-expires cookie))))
		   (or (not expiry)
		       (not (setq expiry
				  (ignore-errors
				    (date-to-time
				     (timezone-make-date-arpa-standard
				      expiry)))))
		       (> (time-to-seconds (time-since expiry)) -60)))))
    (shimbun-sankei-login shimbun-sankei-login-name
			  shimbun-sankei-login-password
			  t)))

(luna-define-method shimbun-headers :before ((shimbun shimbun-sankei)
					     &optional range)
  (shimbun-sankei-keep-login))

(luna-define-method shimbun-article :before ((shimbun shimbun-sankei)
					     header &optional outbuf)
  (shimbun-sankei-keep-login))

(provide 'sb-sankei)

;;; sb-sankei.el ends here

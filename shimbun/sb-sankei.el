;;; sb-sankei.el --- shimbun backend for the Sankei News

;; Copyright (C) 2003-2011, 2013-2019, 2021 Katsumi Yamaoka

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

(autoload 'timezone-parse-date "timezone")

(defun shimbun-sankei-get-headers (shimbun range)
  "Get headers for the group that SHIMBUN specifies in RANGE."
  (let ((group (shimbun-current-group-internal shimbun))
	nd url id st ids date tem subject names headers)
    (goto-char (point-min))
    (while (re-search-forward
	    "\"website_url\":\"\\([^\"]+-\\([0-9A-Z]\\{26\\}\\)[^\"]*\\)"
	    nil t)
      (setq nd (match-end 0)
	    url (match-string 1)
	    id (match-string 2))
      (when (and (search-backward (concat "{\"_id\":\"" id "\"") nil t)
		 (progn
		   (setq st (match-beginning 0))
		   (or (ignore-errors (setq nd (scan-sexps st 1)))
		       (progn (goto-char nd) nil))))
	(setq id (concat "<" id "."
			 (mapconcat #'identity
				    (nreverse (split-string group "\\."))
				    ".")
			 "%" shimbun-sankei-top-level-domain ">"))
	(if (or (member id ids)
		(progn (push id ids)
		       (shimbun-search-id shimbun id)))
	    (goto-char nd)
	  (save-restriction
	    (narrow-to-region (goto-char st) nd)

	    ;; The version that works on Emacs 28 and elders.
	    ;;(setq date (decode-time ;; Default to the current time.
	    ;;            (and (re-search-forward "\"display_date\":\"\
;;\\(20[2-9][0-9]-[01][0-9]-[0-3][0-9]T[0-5][0-9]:[0-5][0-9]:[^\"]+\\)" nil t)
	    ;;                 (ignore-errors
	    ;;                   (encode-time
	    ;;                    (parse-time-string (match-string 1)))))))

	    ;; On Emacs 27 and earliers `parse-time-string' doesn't support
	    ;; ISO8601 date.
	    ;; On Emacs 26 and earliers `encode-time' doesn't accept the 1st
	    ;; argument that is a list style.

	    ;; The version that supports Emacs 27 and 26.
	    (setq date
		  (decode-time ;; Default to the current time.
		   (and (re-search-forward "\"display_date\":\"\
\\(20[2-9][0-9]-[01][0-9]-[0-3][0-9]T[0-5][0-9]:[0-5][0-9]:[^\"]+\\)" nil t)
			(ignore-errors
			  (setq tem (match-string 1)
				date (parse-time-string tem))
			  (if (car date) ;; true on Emacs 28
			      (encode-time date)
			    (setq date (timezone-parse-date tem)
				  tem (split-string (aref date 3) ":")
				  date (list
					(string-to-number (caddr tem))
					(string-to-number (cadr tem))
					(string-to-number (car tem))
					(string-to-number (aref date 2))
					(string-to-number (aref date 1))
					(string-to-number (aref date 0))
					nil nil nil))
			    (condition-case nil
				(encode-time date) ;; works on Emacs 27
			      (error ;; Emacs 26
			       (apply #'encode-time date))))))))
	    (goto-char st)
	    (when (re-search-forward "\"headlines\":{\"basic\":\\(\"\\)" nil t)
	      (setq subject (condition-case nil
				(replace-regexp-in-string
				 "\\`[\t 　]+\\|[\t 　]+\\'" ""
				 (read (nth 2 (match-data))))
			      (error "(failed to fetch subject)")))
	      (goto-char st)
	      (setq names nil)
	      (when (and (re-search-forward "\"taxonomy\":\\({\\)" nil t)
			 (setq tem (ignore-errors
				     (scan-sexps (match-beginning 1) 1))))
		(save-restriction
		  (narrow-to-region (match-beginning 1) tem)
		  (while (re-search-forward "\"name\":\"\\([^\"]+\\)\"" nil t)
		    (push (match-string 1) names))))
	      (when (or (not names)
			(not (setq
			      tem
			      (cdr (assoc
				    group
				    '(("column.editorial" . "主張")
				      ("column.seiron" . "正論")
				      ("column.sankeisyo" . "産経抄")
				      ("column.naniwa" . "浪速風")
				      ("west.essay" . "朝晴れエッセー"))))))
			(member tem names))
		(push (shimbun-create-header
		       0 subject
		       (concat shimbun-sankei-server-name
			       (if names
				   (concat " (" (mapconcat #'identity
							   (last names 2)
							   " ")
					   ")")
				 ""))
		       (shimbun-make-date-string
			(nth 5 date) (nth 4 date) (nth 3 date)
			(format "%02d:%02d:%02d"
				(nth 2 date) (nth 1 date) (nth 0 date)))
		       id "" 0 0
		       (shimbun-expand-url url shimbun-sankei-url))
		      headers)))
	    (goto-char nd)))))
    (shimbun-sort-headers headers)))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-sankei)
						    header)
  (shimbun-sankei-clear-contents shimbun header))

(defun shimbun-sankei-clear-contents (shimbun header)
  "Collect contents and create an html page in the current buffer."
  (let (author st nd tem headline ids simgs id caption img contents eimgs
	       maxwidth fn)
    (goto-char (point-min))
    (when (or (and (re-search-forward "<a[\t\n ]+\\(?:[^\t\n >]+[\t\n ]+\\)*\
\\(?:class=\"gtm-click author-name\"\\|href=\"/author/\
\\|data-gtm-action=\"move to author page\"\
\\|data-gtm-label=\"article header author link\\)" nil t)
		   (shimbun-end-of-tag "a"))
	      (re-search-forward "{\"byline\":\"\\(\\([^\"}]+\\)\\)\"" nil t))
      (setq author (replace-regexp-in-string
		    "\\`[\t 　]+\\|\\(\\cj\\)[\t 　]+\\(\\cj\\)\\|[\t 　]+\\'"
		    "\\1\\2" (match-string 2))))
    (goto-char (point-min))
    (when (and (re-search-forward ";Fusion.globalContent=\\({\\)" nil t)
	       (setq st (match-beginning 1)
		     nd (ignore-errors (copy-marker (scan-sexps st 1)))))
      (when (re-search-forward ",\"promo_items\":\\({\\)" nd t)
	(ignore-errors ;; The other headlines are there.
	  (delete-region (match-beginning 0)
			 (scan-sexps (match-beginning 1) 1))))
      (setq nd (prog1 (marker-position nd) (set-marker nd nil)))
      (goto-char nd)
      (when (and (re-search-backward ",\"headlines\":{\"basic\":\\(\"\\)" st t)
		 (progn
		   (setq tem (ignore-errors
			       (replace-regexp-in-string
				"\\`[\t 　]+\\|[\t 　]+\\'" ""
				(read (nth 2 (match-data))))))
		   (not (zerop (length tem)))))
	(setq headline tem))
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
		  (and (or (re-search-forward "\"articleLarge\":\\(\"\\)" nd t)
			   (re-search-forward "\"articleSmall\":\\(\"\\)" nd t)
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
					   (concat "<br>\n" caption
						   ;;"<br><br>")
						   )
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
		      (while (re-search-forward
			      "[\t\n 　]*\\(?:<\\(?:br\\|/?p\\)>[\t\n 　]*\\)+"
			      nil t)
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
		      (when (setq tem (split-string (buffer-string) "\n\n+" t))
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
		(condition-case nil
		    (setq tem (replace-regexp-in-string
			       ">[^<]*</iframe"	">[動画]</iframe"
			       (read (nth 2 (match-data)))))
		  (error (setq tem "[動画]")))
		(when (string-match "[\t\n ]src=\"\\([^ \">?]+\\)" tem)
		  (push (concat "<a href=\"" (match-string 1 tem) "\">"
				tem "</a>")
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
      (if headline
	  (progn
	    (insert "<p>" headline)
	    (when (and author
		       (not (string-match (regexp-quote author) headline)))
	      (insert "<br>\n<b>" author "</b>"))
	    (insert "</p>\n"))
	(when author
	  (insert "<p><b>" author "</b></p>\n")))
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
	  (insert "<p>" (mapconcat #'identity (reverse (cdr eimgs)) "</p>\n<p>")
		  "</p>\n"))
	(insert (car eimgs) "\n"))
      (unless (memq (shimbun-japanese-hankaku shimbun) '(header subject nil))
	(shimbun-japanese-hankaku-buffer t))
      t)))

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
(declare-function w3m-cookie-setup "w3m-cookie")

(autoload 'password-cache-add "password-cache")
(autoload 'password-read-from-cache "password-cache")

(defun shimbun-sankei-login (&optional name password interactive-p)
  "Login to special.sankei.com with NAME and PASSWORD.
NAME and PASSWORD default to `shimbun-sankei-login-name' and
`shimbun-sankei-login-password' respectively.  `password-data', if
cached, overrides `shimbun-sankei-login-password'.  If the prefix
argument is given, you will be prompted for new NAME and PASSWORD."
  (interactive (let ((pass (copy-sequence shimbun-sankei-login-password))
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
		 (setq default (and name
				    (or (password-read-from-cache name)
					;; `password-cache' will expire
					;; the password by filling it with
					;; C-@'s, so we use a copy of
					;; the original.
					(copy-sequence
					 shimbun-sankei-login-password)))
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
	    (setq password
		  (or (password-read-from-cache name)
		      (copy-sequence shimbun-sankei-login-password)))
	    (setq name nil))
      (setq password nil)))
  (if (not (and w3m-use-cookies w3m-use-form name password))
      (when interactive-p (message "Quit"))
    (when interactive-p (message "Logging in to special.sankei.com..."))
    (require 'w3m-cookie)
    ;; Delete old login cookies.
    (w3m-cookie-setup)
    (dolist (cookie w3m-cookies)
      (when (string-match "\\.sankei\\..+login" (w3m-cookie-url cookie))
	(setq w3m-cookies (delq cookie w3m-cookies))))
    (require 'w3m-form)
    (w3m-arrived-setup)
    (let ((cache (buffer-live-p w3m-cache-buffer))
	  (w3m-message-silent t)
	  w3m-clear-display-while-reading next form action handler)
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
	    (setq form (car w3m-current-forms))
	    (if (not (string-match "login\\.php\\'"
				   (setq action (w3m-form-action form))))
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
		(password-cache-add name password)
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
	      (when interactive-p (message "Logged out"))
	      (setq done t)))
	  (when (get-buffer " *w3m-cookie-parse-temp*")
	    (kill-buffer (get-buffer " *w3m-cookie-parse-temp*")))
	  (unless cache (w3m-cache-shutdown)))
      (error (if (or interactive-p debug-on-error)
		 (signal (car err) (cdr err))
	       (message "Error while logging out from special.sankei.com:\n %s"
			(error-message-string err)))))))

;;(shimbun-sankei-login)

(provide 'sb-sankei)

;;; sb-sankei.el ends here

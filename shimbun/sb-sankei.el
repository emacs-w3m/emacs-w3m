;;; sb-sankei.el --- shimbun backend for the Sankei News

;; Copyright (C) 2003-2011, 2013-2019 Katsumi Yamaoka

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
     "https://special.sankei.com/sankeisyo/")
    ("column.editorial" "主張"
     "https://www.sankei.com/column/newslist/editorial-n1.html")
    ("column.seiron" "正論"
     "https://special.sankei.com/seiron/")
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
     "https://www.sankei.com/west/newslist/west_economy-n1.html")
    ("west.essay" "朝晴れエッセー"
     "https://www.sankei.com/column/topics/column-36217-t1.html")))

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
  (let ((group (shimbun-current-group-internal shimbun)))
    (cond ((string-match "\\`https://special\\.sankei\\.com/"
			 (shimbun-index-url shimbun))
	   (shimbun-sankei-get-headers-special shimbun range group))
	  (t
	   (shimbun-sankei-get-headers shimbun range group)))))

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

(defun shimbun-sankei-get-headers-special (shimbun range group)
  "Get headers for the groups in special.sankei.com."
  (let ((regexp
	 (eval-when-compile
	   (concat
	    "<div\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+class=\"block_list_head\">"
	    "\\(?:[\t\n ]*<[^a>][^>]+>\\)*[\t\n ]*<a[\t\n ]+href=\""
	    ;; 1. url
	    "\\(\\(?:[^/>]+/\\)+"
	    ;; 2. seriai
	    "\\([0-9]+\\)" "\\.html\\)"
	    "\">[\t\n ]*"
	    ;; 3. subject
	    "\\([^<]+\\)"
	    "[\t\n ]*\\(?:[^<]*<\\(?:[^/>]\\|/[^a>]\\)[^>]*>\\)+[\t\n ]*</a>"
	    "\\(?:[^<]*<\\(?:/\\|[^t>]\\|t[^i>]\\|ti[^m>]\\)[^>]*>\\)"
	    "[^<]*<time[\t\n ]+datetime=\""
	    ;; 4. year
	    "\\(20[1-9][0-9]\\)" "-"
	    ;; 5. month
	    "\\([01][0-9]\\)" "-"
	    ;; 6. day
	    "\\([0-3][0-9]\\)" "T"
	    ;; 7. hh:mm
	    "\\([012][0-8]:[0-5][0-9]\\)" "\"")))
	(rgrp (mapconcat 'identity (nreverse (split-string group "\\.")) "."))
	(index (shimbun-index-url shimbun))
	url subj year month day hour-min id old from headers)
    (shimbun-strip-cr)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (setq url (match-string 1)
	    subj (match-string 3)
	    year (match-string 4)
	    month (match-string 5)
	    day (match-string 6)
	    hour-min (match-string 7)
	    id (concat "<" year month day "." (match-string 2) "." rgrp
		       "%special.sankei.com>"))
      (unless (shimbun-search-id shimbun id)
	(setq from
	      (concat
	       (shimbun-server-name shimbun)
	       " (" (shimbun-current-group-name shimbun) ")"))
	(push (shimbun-create-header
	       0 subj from
	       (shimbun-make-date-string
		(string-to-number year)
		(string-to-number month)
		(string-to-number day)
		hour-min)
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
    (goto-char (point-min))
    (while (and (re-search-forward "<button[\t\n ]+" nil t)
		(shimbun-end-of-tag "button" t))
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
			    (string-match "\\`https://special.sankei.com/?\\'"
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

(shimbun-sankei-login)

(provide 'sb-sankei)

;;; sb-sankei.el ends here

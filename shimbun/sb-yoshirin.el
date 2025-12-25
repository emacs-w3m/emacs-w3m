;;; sb-yoshirin.el --- shimbun backend for Yoshinori Kobayashi Official Site -*- lexical-binding: nil -*-

;; Copyright (C) 2015, 2016, 2021, 2022, 2025 Katsumi Yamaoka

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

(luna-define-class shimbun-yoshirin (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-yoshirin-top-level-domain "yoshinori-kobayashi.com")

(defvar shimbun-yoshirin-url
  (concat "https://" shimbun-yoshirin-top-level-domain "/"))

(defvar shimbun-yoshirin-server-name "小林よしのり全宇宙")

(defvar shimbun-yoshirin-group-table
  '(("all" "すべての著者"
     "https://yoshinori-kobayashi.com/blog?author=all")
    ("kobayashi" "小林よしのり"
     "https://yoshinori-kobayashi.com/blog?author=kobayashi")
    ("takamori" "高森明勅"
     "https://yoshinori-kobayashi.com/blog?author=takamori")
    ("sasa" "笹幸恵"
     "https://yoshinori-kobayashi.com/blog?author=sasa")
    ("mokuren" "泉美木蘭"
     "https://yoshinori-kobayashi.com/blog?author=mokuren")
    ("kuramochi" "倉持麟太郎"
     "https://yoshinori-kobayashi.com/blog?author=kuramochi")
    ("oosuga" "大須賀淳"
     "https://yoshinori-kobayashi.com/blog?author=oosuga")
    ("chinone" "茅根豪"
     "https://yoshinori-kobayashi.com/blog?author=chinone")
    ("cheb" "ちぇぶ"
     "https://yoshinori-kobayashi.com/blog?author=cheb")
    ("mah" "まー"
     "https://yoshinori-kobayashi.com/blog?author=mah")
    ("kishibata" "岸端みな"
     "https://yoshinori-kobayashi.com/blog?author=kishibata")
    ("gosenjournalist" "ゴー宣ジャーナリスト"
     "https://yoshinori-kobayashi.com/blog?author=gosenjournalist")
    ("kiridoshi" "切通理作"
     "https://yoshinori-kobayashi.com/blog?author=kiridoshi")
    ("tokiura" "時浦兼(トッキー)"
     "https://yoshinori-kobayashi.com/blog?author=tokiura")
    ;; for the backward compatibility (reading archived articles)
    ("blog") ("topics")))

(defvar shimbun-yoshirin-index-range 20
  "*The number of indices that should be checked to detect new articles.
`all' or nil is for all indices, `last' is for the last index, and an
integer N is for the last N pages of indices.  This overrides any other
index-range setting if it is non-nil.

If you want to increase this value or change the value to nil or `all',
you will need to re-create the group(s) (delete the group(s) completely
in advance).  If you don't do so, you will never get old articles.")

(defvar shimbun-yoshirin-x-face-alist
  '(("default" . "X-Face: SX1=HV&[cI##/uM,,hed]\"a1.(e<M21Jl\
2dAy1-JnWwb'yT9t|fq*~ZpiUV%bx\\@&sdy`Vb\n c5[lT$}jl2|X~V97O'\
I31<&o-eCYZDs~WZVK{m,T}x>b3T9PCilX3;\"*8oF;QS\"GCHWit%'u!of`\
\\p\n &ute*s]IuWa5co-wMr4X1dQqqx/PB3y,@P3~Cdc<:$9.Jp^X$-*DPC")
    ("\\`kobayashi\\'" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAMAAABg3Am1AAAAtFBMVEX////8uAT8vAH8vQD
 8wAT8wgD9uAD9uQC1k45cPydcQjj+twD+uQH+uwD+vQD+vgD+vwD+wAD+wh7+wwD+xABcVFU5Gyj
 /uAD/uQCCPlL/uwD/vwH/wQDJmGb/wgAoDSPuxGWDWFfpwq4YDR/v07/wuxvfniDv6+vNopSFcVQ
 sIC3x2cirflSIaClANjJTNUmIfYLRvKx5SDD15q7kxZX269QgHS34vAOxgSdGHSlZHzXVyMrkNCq
 HAAAAAXRSTlMAQObYZgAABDBJREFUSMeNlot6ojoUhaVgG1GGoo1VO6jhZogQIIACff/3OjsBrDN
 1Zs7yq1pZf/aFbeJk8l11Oaie/Ft1eVy+3bQ8lv+wH42337Ss6r/Y31ZK+9XqbXgLMqo/+Mshl/1
 qb4RhaBjT6RRgGaV8vHxv3++nhhsTnEbhdAr/yQ8fBOn9+/1q6hiGm2fswngaXVcyOUkdv/n3vVZ
 h1CRxzhjjnGcpEG/9heMj/0/HiJIcZxmGAIxlmDcGVPGA6P0O+AnGPONYBQDCDK/7n4PuKi8/1Cf
 ONMKMy+wxifOYmACmzdVxeuarV7Wh7I4TEvBzjnNEq4omicA+40BMHUXckjoOMR0IwHCMCi/wvIq
 6CTF9qCV6HYAxqXKwO06DuYm8IDiPAPcvF9YYzgAs+yk5Or1eDQAIDc6gwKM3wJREr7Kv4LWXHpo
 Zj6kniUARsWCSEKE9WJYqI3vQNYFWJhVkdJZZUYqIMJnvM3HVX3uLIXM62vbHh8SvKdwrVCm/5xU
 xJuZMcN+/zELd1nUbwtiyUcsPAECvTw30lFYFQoh60NQ4FbOZDMEj3f5hy3VtyKkeM7L1CHMMibj
 IVYAwZyY3+aVtQv3H4DGgBP2ma5xh13WTVAhhEhgq08QmZ+lVt0YLFFHp1qgQM1NQCr1F21wkCSE
 Cyubp9ebQP8pJBa8/lKxrw2LTTWQNRUGgCGAkoIFDmSzdVsCuB3ZWaMIYIQqPs5cIQmIJZDLCsKS
 uS2B3A7QmphRWL+jZcwmC9wlUIyM8gWu3g2QA2N1kvWjQH1ABgyRE4kGviBCp9mQtFrvNbrPZLMp
 J+QUsrKdQTYbnIZTHCVV+ABZWb9jsfgE2mxfr6eipO61mCcZPEs0AQIidVk/qxeZOFgCBmqaCoiR
 xk1iIaGG9jNfl9C3n8/dR8+dlBTUEQYHIVpgxjSXw/HIzyFk6Pt/4zctcq5SdekUSq/sgxPX5/X0
 0yC9EeQds5nOZUwUVQENjl4gZAJDCmJH6yi3vgM1c5gQxYPhgrAQAs0abz4er/TZQzb/0PuQkS0Y
 ohvEWfN0TcFXrd4FauyOew6quJUCTGGZva5prv4205/kCLo77zH0IrTrX9TlQXY0TCIGZf1inkQa
 MdtvJljd7FAWFd67VfYA2zVrp933/MGsi7Wt3LVVSWhil7QGmFWLUZw9uRdti7PdA23Xk7uyqNA1
 2+e22a9dZntBaETThOZZuCXRdV9xv3xTcWwnIjSMOFFEg2O/7ADIC+uWAOKEekKcC2iKZVJFdLtJ
 6OKgn9NsRpIhszQ6Mwyvyzjm7AADOdduu1x39fioWqgRQt93iAkv/4aAAqPfz0bH7mbdtB4+22+Y
 Y7Bc2Avnp8UF9KnDXZRn8qfUZxJPA4+VHJJelZFytv1Zd/jz9/efGucgJyRiDYjKSF6f/8YNmcjp
 9Kp0euf8D68DXEs6Uw/EAAAAASUVORK5CYII=")
    ("\\`tokiura\\'" . "\
Face: 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
 yAAAAAElFTkSuQmCC")))

(defvar shimbun-yoshirin-expiration-days 1000)

(luna-define-method shimbun-groups ((shimbun shimbun-yoshirin))
  (mapcar 'car shimbun-yoshirin-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-yoshirin))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-yoshirin-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-yoshirin))
  (nth 2 (assoc (shimbun-current-group-internal shimbun)
		shimbun-yoshirin-group-table)))

(luna-define-method shimbun-headers :around ((shimbun shimbun-yoshirin)
					     &optional range)
  ;; backward compat; see `shimbun-yoshirin-group-table'.
  (when (shimbun-index-url shimbun)
    (luna-call-next-method)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-yoshirin)
					 &optional range)
  (shimbun-yoshirin-get-headers shimbun
				(or shimbun-yoshirin-index-range range)))

(luna-define-method shimbun-make-contents :around ((shimbun shimbun-yoshirin)
						   header)
  (shimbun-yoshirin-make-contents shimbun header)
  (let ((group (cdr (assq 'group (shimbun-header-extra header))))
	x-face ofn)
    (when (and group (assoc group shimbun-yoshirin-group-table))
      (setq x-face (or (cdr (assoc group shimbun-yoshirin-x-face-alist))
		       (catch 'face
			 (dolist (elem shimbun-yoshirin-x-face-alist)
			   (when (and (string-match "[]$*+\\^[]" (car elem))
				      (string-match (car elem) group))
			     (throw 'face (cdr elem)))))
		       shimbun-x-face))
      (setq ofn (symbol-function #'shimbun-x-face))
      (fset #'shimbun-x-face
	    `(lambda (shimbun)
	       (shimbun-set-x-face-internal shimbun ,x-face))))
    (unwind-protect
	(luna-call-next-method)
      (when ofn
	(fset #'shimbun-x-face ofn)))))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-yoshirin)
						    header)
  t) ;; Force inserting footer.

(luna-define-method shimbun-footer :around ((shimbun shimbun-yoshirin)
					    header &optional html)
  (shimbun-yoshirin-footer shimbun header))

(defun shimbun-yoshirin-footer (shimbun header)
  (concat "<div align=\"left\">\n--&nbsp;<br>\n\
原物は<a href=\"" (shimbun-article-base-url shimbun header)
	  "\"><u>ここ</u></a>で公開されています。\n</div>\n"))

(defun shimbun-yoshirin-get-headers (shimbun range)
  "Get headers for articles in the group of SHIMBUN in RANGE."
  (let ((count (cond ((memq range '(0 nil all)) nil)
		     ((natnump range) range)
		     (t 1)))
	headers)
    (when (or (not count) (> count 0))
      (let (group from st nd id url subject datetime)
	(catch 'stop
	  (while t
	    (when (and (re-search-forward "\
<div\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+class=\"main-content" nil t)
		       (shimbun-end-of-tag "div"))
	      (save-restriction
		(narrow-to-region (goto-char (match-beginning 2))
				  (match-end 2))
		(while (re-search-forward "\
<div\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+class=\"swiper-slide\"[^>]*" nil t)
		  (goto-char (match-beginning 0))
		  (setq group (or (and (re-search-forward "\
[\t\n ]data-author=\"\\([^\"]+\\)" (match-end 0) t)
				       (match-string 1))
				  (shimbun-current-group-internal shimbun))
			from (or (and (nth 1 (assoc
					      group
					      shimbun-yoshirin-group-table)))
				 (shimbun-current-group-name shimbun)))
		  (when (shimbun-end-of-tag "div")
		    (goto-char (setq st (match-beginning 2)))
		    (setq nd (match-end 2))
		    (when (and (re-search-forward "\
<span\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+class=\"title\"" nd t)
			       (shimbun-end-of-tag "span")
			       (progn
				 (goto-char (match-beginning 2))
				 (re-search-forward "\
<a\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+href=\"\\([^\"]+/\\([0-9]+\\)\\)\
[^>]+>\\([^<]+\\)" (match-end 2) t)))
		      (setq id (concat "<" (match-string 2) "." group "%"
				       shimbun-yoshirin-top-level-domain ">"))
		      (when (save-match-data (shimbun-search-id shimbun id))
			(throw 'stop nil))
		      (setq url (match-string 1)
			    subject (match-string 3))
		      (goto-char st)
		      (setq datetime
			    (or (and (re-search-forward "\
<time\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+datetime=\"\\([^\"]+\\)" nd t)
				     (condition-case nil
					 (parse-time-string (match-string 1))
				       (error nil)))
				(decode-time (current-time) 32400)))
		      (push (shimbun-create-header
			     0 subject from
			     (shimbun-make-date-string
			      (nth 5 datetime) (nth 4 datetime)
			      (nth 3 datetime)
			      (format "%02d:%02d:%02d"
				      (nth 2 datetime) (nth 1 datetime)
				      (nth 0 datetime)))
			     id "" 0 0 url (list (cons 'group group)))
			    headers)
		      (goto-char nd))))))
	    (when (and count (<= (setq count (1- count)) 0))
	      (throw 'stop nil))
	    (goto-char (point-min))
	    (if (and (re-search-forward "\
<a\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+class=\"next page-numbers\"[^>]*>" nil t)
		     (progn
		       (goto-char (match-beginning 0))
		       (re-search-forward "[\t\n ]href=\"\\([^\"]+\\)"
					  (match-end 0) t)))
		(shimbun-retrieve-url (prog1
					  (match-string 1)
					(erase-buffer)))
	      (throw 'stop nil))))))
    headers))

(defun shimbun-yoshirin-make-contents (shimbun header)
  "Make an html article."
  (when (and (or (re-search-forward "\
<div\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+class=\"content-wrapper\"" nil t)
		 (re-search-forward "\
<div\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+class=\"entry-content" nil t))
	     (shimbun-end-of-tag "div"))
    (delete-region (goto-char (match-end 0)) (point-max))
    (insert "\n")
    (delete-region (goto-char (point-min)) (match-beginning 0))))

(provide 'sb-yoshirin)

;; sb-yoshirin.el ends here

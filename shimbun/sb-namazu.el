;;; sb-namazu.el --- shimbun backend for namazu.org -*- coding: iso-2022-jp; -*-

;; Copyright (C) 2001, 2002, 2003, 2004 Akihiro Arisawa  <ari@mbf.sphere.ne.jp>

;; Author: Akihiro Arisawa <ari@mbf.sphere.ne.jp>
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

;;; Code:

(require 'shimbun)
(require 'sb-mailman)

(luna-define-class shimbun-namazu (shimbun-mailman) ())

(defvar shimbun-namazu-url "http://www.namazu.org/pipermail/")

(defvar shimbun-namazu-groups
  '("kakasi-commits" "kakasi-dev" "migemo" "namazu-devel-en" "namazu-devel-ja"
    "namazu-users-en" "namazu-users-ja" "namazu-win32-users-ja" "sary"))

(luna-define-method shimbun-index-url ((shimbun shimbun-namazu))
  (shimbun-expand-url
   (concat (shimbun-current-group-internal shimbun) "/")
   (shimbun-url-internal shimbun)))

(luna-define-method shimbun-reply-to ((shimbun shimbun-namazu))
  (concat (shimbun-current-group-internal shimbun)
	  "@namazu.org"))

(luna-define-method shimbun-make-contents
  ((shimbun shimbun-namazu) header)
  (shimbun-namazu-make-contents shimbun header))

(defun shimbun-namazu-make-contents (shimbun header)
  ;; copy from shimbun-squeak-ja-make-contents
  (subst-char-in-region (point-min) (point-max) ?\t ?\  t)
  (shimbun-decode-entities)
  (goto-char (point-min))
  (let ((end (search-forward "<!--beginarticle-->"))
	name address date)
    (goto-char (point-min))
    (search-forward "</HEAD>")
    (when (re-search-forward "<H1>\\([^\n]+\\)\\(\n +\\)?</H1>" end t nil)
      (shimbun-header-set-subject
       header
       (shimbun-mime-encode-string (match-string 1))))
    (when (re-search-forward "<B>\\([^\n]+\\)\\(\n +\\)?</B> *\n +\
<A HREF=\"[^\n]+\n +TITLE=\"[^\n]+\">\\([^\n]+\\)"
			     end t nil)
      (setq name (match-string 1)
	    address (match-string 3))
      ;; Yoshiki.Ohshima ＠ acm.org
      (when (string-match " ＠ " name)
	(setq name (concat (substring name 0 (match-beginning 0))
			   "@"
			   (substring name (match-end 0)))))
      (when (string-match " ＠ " address)
	(setq address (concat (substring address 0 (match-beginning 0))
			      "@"
			      (substring address (match-end 0)))))
      (shimbun-header-set-from
       header
       (shimbun-mime-encode-string (concat name " <" address ">")))

      (when (re-search-forward "<I>\\([0-9][0-9][0-9][0-9]\\)年 *\\([0-9][0-9]*\\)月 *\\([0-9][0-9]*\\)日 (\\(月\\|火\\|水\\|木\\|金\\|土\\|日\\)) \\([:0-9]+\\) \\([A-Z]+\\)</I>" end t nil)
	;; <I>Sat, 12 Apr 2003 17:29:51 +0900 (JST)</I> ;; mailman original
	;; <I>2003年 4月 11日 (金) 02:43:25 CEST</I> ;; squeak-ja
        (setq date (shimbun-make-date-string
		    (string-to-number (match-string-no-properties 1))
		    (string-to-number (match-string-no-properties 2))
		    (string-to-number (match-string-no-properties 3))
		    (match-string-no-properties 5)
		    (match-string-no-properties 6)))
	(shimbun-header-set-date header date))
      (delete-region (point-min) end)
      (delete-region (search-forward "<!--endarticle-->") (point-max))
      (shimbun-header-insert-and-buffer-string shimbun header nil t))))

(provide 'sb-namazu)

;;; sb-namazu.el ends here

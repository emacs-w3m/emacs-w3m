;;; sb-squeak-ja.el --- shimbun backend for Squeak-ja's ML archive -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2003 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
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
(require 'sb-mailman)

(luna-define-class shimbun-squeak-ja (shimbun-mailman) ())

(defvar shimbun-squeak-ja-url "http://lists.squeakfoundation.org/pipermail/squeak-ja")

(defvar shimbun-squeak-ja-groups '("main"))

(luna-define-method shimbun-index-url ((shimbun shimbun-squeak-ja))
  shimbun-squeak-ja-url)

;;(luna-define-method shimbun-reply-to ((shimbun shimbun-squeak-ja))
;;  "squeak-ja@lists.squeakfoundation.org")

(luna-define-method shimbun-make-contents
  ((shimbun shimbun-squeak-ja) header)
  (shimbun-squeak-ja-make-contents shimbun header))

(defun shimbun-squeak-ja-make-contents (shimbun header)
  (subst-char-in-region (point-min) (point-max) ?\t ?\  t)
  (shimbun-decode-entities)
  (goto-char (point-min))
  (let ((end (search-forward "<!--beginarticle-->")))
    (goto-char (point-min))
    (search-forward "</HEAD>")
    (when (re-search-forward "<H1>\\([^\n]+\\)\\(\n +\\)?</H1>" end t nil)
      (shimbun-header-set-subject
       header
       (shimbun-mime-encode-string (match-string 1))))
    (when (re-search-forward "<B>\\([^\n]+\\)\\(\n +\\)?</B> *\n +\
<A HREF=\"[^\n]+\n +TITLE=\"[^\n]+\">\\([^\n]+\\)"
			     end t nil)
      (shimbun-header-set-from
       header
       (shimbun-mime-encode-string (concat (match-string 1)
					   " <" (match-string 3) ">")))

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

(provide 'sb-squeak-ja)
;;; sb-squeak-ja.el ends here

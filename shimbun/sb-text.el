;;; sb-text.el -- shimbun backend class for text content -*- coding: junet; -*-
;; Author: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;;         Akihiro Arisawa    <ari@atesoft.advantest.co.jp>
;;         Yuuichi Teranishi <teranisi@gohome.org>

;; Keywords: news

;;; Copyright:

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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>.

;;; Code:

(require 'shimbun)
(luna-define-class shimbun-text (shimbun) ())

;; Fast fill-region function

(defvar shimbun-fill-column (min 80 (- (frame-width) 4)))

(defconst shimbun-kinsoku-bol-list
  (append "!)-_~}]:;',.?、。，．・：；？！゛゜´｀¨＾￣＿ヽヾゝゞ〃\
仝々〆〇ー―‐／＼〜‖｜…‥’”）〕］｝〉》」』】°′″℃ぁぃぅぇぉ\
っゃゅょゎァィゥェォッャュョヮヵヶ" nil))

(defconst shimbun-kinsoku-eol-list
  (append "({[`‘“（〔［｛〈《「『【°′″§" nil))

(defun shimbun-fill-line ()
  (forward-line 0)
  (let ((top (point)) chr)
    (while (if (>= (move-to-column shimbun-fill-column)
		   shimbun-fill-column)
	       (not (progn
		      (if (memq (preceding-char) shimbun-kinsoku-eol-list)
			  (progn
			    (backward-char)
			    (while (memq (preceding-char) shimbun-kinsoku-eol-list)
			      (backward-char))
			    (insert "\n"))
			(while (memq (setq chr (following-char)) shimbun-kinsoku-bol-list)
			  (forward-char))
			(if (looking-at "\\s-+")
			    (or (eolp) (delete-region (point) (match-end 0)))
			  (or (> (char-width chr) 1)
			      (re-search-backward "\\<" top t)
			      (end-of-line)))
			(or (eolp) (insert "\n"))))))
      (setq top (point))))
  (forward-line 1)
  (not (eobp)))

(defsubst shimbun-shallow-rendering ()
  (goto-char (point-min))
  (while (search-forward "<p>" nil t)
    (insert "\n\n"))
  (goto-char (point-min))
  (while (search-forward "<br>" nil t)
    (insert "\n"))
  (shimbun-remove-markup)
  (shimbun-decode-entities)
  (goto-char (point-min))
  (while (shimbun-fill-line))
  (goto-char (point-min))
  (when (skip-chars-forward "\n")
    (delete-region (point-min) (point)))
  (while (search-forward "\n\n" nil t)
    (let ((p (point)))
      (when (skip-chars-forward "\n")
	(delete-region p (point)))))
  (goto-char (point-max))
  (when (skip-chars-backward "\n")
    (delete-region (point) (point-max)))
  (insert "\n"))

(luna-define-method shimbun-make-contents ((shimbun shimbun-text)
					   header)
  (let ((case-fold-search t) (html t) (start))
    (when (and (re-search-forward (shimbun-content-start-internal shimbun)
				  nil t)
	       (setq start (point))
	       (re-search-forward (shimbun-content-end-internal shimbun)
				  nil t))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start)
      (shimbun-shallow-rendering)
      (setq html nil))
    (goto-char (point-min))
    (shimbun-header-insert header)
    (insert "Content-Type: " (if html "text/html" "text/plain")
	    "; charset=ISO-2022-JP\nMIME-Version: 1.0\n")
    (when (shimbun-x-face-internal shimbun)
      (insert (shimbun-x-face-internal shimbun))
      (unless (bolp)
	(insert "\n")))
    (insert "\n")
    (encode-coding-string (buffer-string)
			  (mime-charset-to-coding-system "ISO-2022-JP"))))

(provide 'sb-text)

;;; sb-text.el ends here

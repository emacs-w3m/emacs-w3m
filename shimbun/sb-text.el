;;; sb-text.el -- shimbun backend class for text content -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

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
  (shimbun-header-insert-and-buffer-string
   shimbun header nil
   ;; When cleaning has been succeeded, this article is treated as a
   ;; text/plain message.  Otherwise, it is treated as a text/html
   ;; message.
   (if (shimbun-clear-contents shimbun header)
       (shimbun-shallow-rendering)
     t)))

(provide 'sb-text)

;;; sb-text.el ends here

;;; sb-jpilot.el --- shimbun backend for www.jpilot.org

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
(require 'sendmail)

(luna-define-class shimbun-jpilot (shimbun-mailman) ())

(defvar shimbun-jpilot-url "http://www.jpilot.org/pipermail/jpilot")

(defvar shimbun-jpilot-groups '("main"))

(luna-define-method shimbun-index-url ((shimbun shimbun-jpilot))
  shimbun-jpilot-url)

(luna-define-method shimbun-make-contents :after
  ((shimbun shimbun-jpilot) header)
  (save-excursion
    (let ((end (and (mail-position-on-field "From") (point)))
	  (begin (progn (beginning-of-line) (point)))
	  (marker (make-marker)))
      (when end
	(narrow-to-region begin end)
	(goto-char (point-min))
	(when (re-search-forward " at " nil t nil)
	  (set-marker marker (match-beginning 0))
	  (delete-region (match-beginning 0) (match-end 0))
	  (goto-char marker)
	  (insert "@"))
	(widen))))
  (buffer-string))

;;(luna-define-method shimbun-reply-to ((shimbun shimbun-jpilot))
;;  "jpilot@jpilot.org")

(provide 'sb-jpilot)
;;; sb-jpilot.el ends here

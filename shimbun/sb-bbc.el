;;; sb-bbc.el --- shimbun backend for BBC UK

;; Copyright (C) 2003, 2004 Koichiro Ohba <koichiro@meadowy.org>

;; Author: Koichiro Ohba <koichiro@meadowy.org>
;; Keywords: news
;; Created: Jun 18, 2003

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
(require 'sb-rss)

(luna-define-class shimbun-bbc (shimbun-rss) ())

(defvar shimbun-bbc-url
  "http://news.bbc.co.uk/rss/newsonline_uk_edition/world/rss091.xml")
(defvar shimbun-bbc-groups '("news"))
(defvar shimbun-bbc-from-address  "newsonline@bbc.co.uk")
(defvar shimbun-bbc-content-start
  "\\(<!-- \\(E I\\(BYL\\|IMA\\)\\|S I\\(BOX\\|IMA\\)\\) -->\\)")
(defvar shimbun-bbc-content-end "<!-- E BO -->")

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-bbc) url date)
;;;<DEBUG>
;;  (shimbun-bbc-build-message-id url))
;;
;;(defun shimbun-bbc-build-message-id (url)
;;;</DEBUG>
  (string-match "http://news.bbc.co.uk/go/click/rss/0.91/\
public/-/\\(.+\\)/hi/\\(.+\\)/\\([0-9]+\\)\\(.+\\)?\\.stm" url)
  (cond ((match-beginning 4)
	 ;; FIXME: Although it solves the problem at which fetching of
	 ;; headers stops on the way, the article in question may contain
	 ;; garbages.
	 (concat "<"
		 (shimbun-subst-char-in-string
		  ?/ ?.
		  (substring url (match-beginning 3) (match-end 4)))
		 "@bbc.co.uk>"))
	((match-beginning 3)
	 (concat "<" (match-string-no-properties 3 url) "@bbc.co.uk>"))
	(t
	 (error "Cannot find message-id base"))))

(provide 'sb-bbc)

;;; sb-bbc.el ends here

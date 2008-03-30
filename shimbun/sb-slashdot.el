;;; sb-slashdot.el --- slashdot.org shimbun backend

;; Copyright (C) 2008 David Engster

;; Author: David Engster <dengste@eml.cc>
;; Keywords: news

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-slashdot (shimbun) ())

(defvar shimbun-slashdot-group-url
  '(("frontpage" "http://www.slashdot.org")
    ("apple" "http://apple.slashdot.org")
    ("askslashdot" "http://ask.slashdot.org")
    ("books" "http://books.slashdot.org")
    ("developers" "http://developers.slashdot.org")
    ("games" "http://games.slashdot.org")
    ("hardware" "http://hardware.slashdot.org")
    ("interviews" "http://interviews.slashdot.org")
    ("IT" "http://it.slashdot.org")
    ("linux" "http://linux.slashdot.org")
    ("mobile" "http://mobile.slashdot.org")
    ("politics" "http://politics.slashdot.org")
    ("science" "http://science.slashdot.org")
    ("YRO" "http://yro.slashdot.org")))

(defvar shimbun-slashdot-url "http://www.slashdot.org")

(defvar shimbun-slashdot-content-start
  "<div class=\"intro\".*?>")

;; This also strips the comments - change this accordingly if you want
;; to see them.
(defvar shimbun-slashdot-content-end
  "<div class=\"commentBox\".*?>")

(defvar shimbun-slashdot-regexp-section-id-subject
  "<div class=\"\\(generaltitle\\|briefarticles\\)\"[^\0]*?\
<a href=\".*slashdot.org/\\(.*?\\)/\\(.*?\\).shtml\">\\(.*?\\)</a>")

(defvar shimbun-slashdot-regexp-author-time
  "Posted by[^a-zA-Z]*\\(.*\\)[^\0]*?@\\([0-9]+\\):\\([0-9]+\\)\\(AM\\|PM\\)")

(defvar shimbun-slashdot-groups
  (mapcar 'car shimbun-slashdot-group-url))

(defvar shimbun-slashdot-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAABAAAAAQAQMAAAAlPW0iAAAABlBMVEUAgID////5Zpl0AAA
 AKElEQVQI12P4/58BiP7Zg9CfehD68R+EPgLRcYbHzSB0HIiOM4BVAgB9+xqjH78TVQAAAABJRU5
 ErkJggg==")))

(defvar shimbun-slashdot-retry-fetching 1)

(luna-define-method shimbun-index-url ((shimbun shimbun-slashdot))
  (let ((group (shimbun-current-group-internal shimbun)))
    (cadr (assoc group shimbun-slashdot-group-url))))

(luna-define-method shimbun-get-headers
  ((shimbun shimbun-slashdot) &optional range)
  (shimbun-slashdot-get-headers shimbun))

(defun shimbun-slashdot-get-headers (shimbun)
  (let ((from "Slashdot <invalid@slashdot.org>")
	hour minute date ampm id url subject headers section)
    (catch 'stop
      (while (re-search-forward shimbun-slashdot-regexp-section-id-subject
				nil t)
	(setq section (match-string 2))
	(setq id (match-string 3))
	(setq url (concat "http://www.slashdot.org/" section "/" id ".shtml"))
	;; Make section prettier
	(setq subject (concat
		       (if (< (length section) 4)
			   (upcase section)
			 (capitalize section))
		       ": " (match-string 4)))
	(if (string= (match-string 1) "briefarticles")
	    (progn
	      (setq hour "00")
	      (setq minute "00")
	      (setq from "Slashdot")
	      (setq subject (concat "(brief article) " subject)))
	  (when (re-search-forward shimbun-slashdot-regexp-author-time
				   nil t)
	    (setq from (match-string 1))
	    (setq hour (match-string 2))
	    (setq minute (match-string 3))
	    ;; US->European time conversion
	    (cond
	     ((and (string= (match-string 4) "PM")
		   (not (string= hour "12")))
	      (setq hour
		    (number-to-string (+ (string-to-number hour) 12))))
	     ((and (string= (match-string 4) "AM")
		   (string= hour "12"))
	      (setq hour "00")))
	    ;; remove link from author name if necessary
	    (when (string-match ">\\(.*\\)</a>" from)
	      (setq from (match-string 1 from))))
	  (while (string-match "/" id)
	    (setq id (replace-match "" t t id)))
	  (setq date (shimbun-make-date-string
		      ;; Hey, my first year 2100 bug!
		      (string-to-number (concat "20" (substring id 0 2)))
		      (string-to-number (substring id 2 4))
		      (string-to-number (substring id 4 6))
		      (format "%s:%s" hour minute)
		      ;; Maybe we should derive this from current-time-zone?
		      "+0000")))
	(setq id (concat "<" section id "@slashdot.org>"))
	(when (shimbun-search-id shimbun id)
	  (throw 'stop nil))
	(push (shimbun-make-header
	       0 (shimbun-mime-encode-string subject)
	       (shimbun-mime-encode-string from)
	       date id "" 0 0 url)
	      headers)))
    headers))

(provide 'sb-slashdot)

;;; sb-slashdot.el ends here

;;; sb-aljazeera.el --- Al Jazeera shimbun backend

;; Copyright (C) 2007 David Hansen

;; Author: David Hansen <david.hansen@physik.fu-berlin.de>
;; Keywords: news

;; This file is a part of shimbun.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-aljazeera (shimbun-rss) ())

(defvar shimbun-aljazeera-url "http://english.aljazeera.net/NR/exeres/\
4D6139CD-6BB5-438A-8F33-96A7F25F40AF.htm?ArticleGuid=")

(defvar shimbun-aljazeera-path-alist
  '(("news" . "55ABE840-AC30-41D2-BDC9-06BBE2A36665")
    ("africa" . "16ED2681-04F1-4E20-987D-441249318394")
    ("america" . "BE36B87D-5D66-4F3E-9BB6-7A0166FD8086")
    ("asia-pacific" . "73B34486-9703-4A5B-8536-9630AF60A8AB")
    ("central-asia" . "4F328D41-F0FF-434F-B392-BC3F16BAEBB0")
    ("europe" . "F7B82066-4245-4746-AE8E-6DA403DDB128")
    ("middle-east" . "736515E4-37CE-4242-8A5F-854381D9DFEE")
    ;; rss feed not working?
    ("focus" . "E61E22A6-6776-4E73-BDE0-644778A336E3")
    ("business" . "6710D253-4D19-4C4E-BF56-14C94349545C")
    ("sport" . "F182B1C7-D945-43CA-888B-B24D86973A13")
    ;; rss feed not working?
    ("programmes" . "3D7AB564-6F62-4899-B982-63B520D409F1")))

(defvar shimbun-aljazeera-from-address  "press.int@aljazeera.net")

(defvar shimbun-aljazeera-content-start "<td[^>]*DetaildTitle[^>]*>")

(defvar shimbun-aljazeera-content-end "<TD id=\"tdRightColumn\"")

(defvar shimbun-aljazeera-groups (mapcar 'car shimbun-aljazeera-path-alist))

(defvar shimbun-aljazeera-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAABgAAAAgAgMAAAB1MFCrAAAADFBMVEXIiyP27tr9/v7ixH8
 SFb1+AAAAzUlEQVQI12NYtWrlq1WrGFatWp0FppZAqBVQQSsIxQemwg+DqJU8Aa+A1HIGESsgtca
 Ylw1ILWFmPgik5jDLfMhiWPnhw3OZWwzLec+8NLdimHCTIfY4G4NztHHtgXMMZ698/MpQyGAp45f
 BcJnBNN49xeYNg0GgT21ADgODocPBw2sYDtsdrmdcwODMxzvHvoDBwdbmw4XDDPV/TZjjGRn2zpc
 /wMDHMDnA4ACDFMMahstnvwLtO1zMGwW0ffpn1lsgBx5gywI5twDoeABy7mBlftzySgAAAABJRU5
 ErkJggg==")))

(luna-define-method shimbun-index-url ((shimbun shimbun-aljazeera))
  (concat shimbun-aljazeera-url
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-aljazeera-path-alist))))

(defconst shimbun-aljazeera-date-re
  (eval-when-compile
    (concat "\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\) " ; M/D/Y
	    "\\([0-9]+\\):\\([0-9]+:[0-9]+\\) "       ; h:m:s
	    "\\(AM\\|PM\\)")))

(luna-define-method shimbun-rss-process-date
  :around ((shimbun shimbun-aljazeera) date)
  ;; M/D/Y h:m:s (A|P)M (violates RSS 2.0 spec)
  (if (string-match shimbun-aljazeera-date-re date)
      (let ((year (string-to-number (match-string 3 date)))
	    (month (string-to-number (match-string 1 date)))
	    (day (string-to-number (match-string 2 date)))
	    (hours (string-to-number (match-string 4 date)))
	    (minutes (match-string 5 date)))
	(when (string= "PM" (match-string 6 date))
	  (setq hours (+ hours 12)))
	(shimbun-make-date-string
	 year month day (format "%d:%s" hours minutes) "+0000"))
    (luna-call-next-method)))

(defun sb-aljazeera-wash-article ()
  ;; html coded by drunken monkeys
  (goto-char (point-min))
  (while (re-search-forward "<input[^>]*value=\"Remove Format\"[^>]*>" nil t)
    (delete-region (match-beginning 0) (match-end 0)))
  (goto-char (point-min))
  (while (re-search-forward "<[^>]*\\(align=\"right\"\\)" nil t)
    (delete-region (match-beginning 1) (match-end 1)))
  (goto-char (point-min))
  (while (re-search-forward "<tr[^>]*id=\"trMainImages" nil t)
    (let ((beg (match-beginning 0)))
      (when (search-forward "</tr>" nil t)
	(delete-region beg (point))))))

(luna-define-method shimbun-make-contents
  :before ((shimbun shimbun-aljazeera) header)
  (sb-aljazeera-wash-article))

(provide 'sb-aljazeera)

;;; sb-aljazeera.el ends here

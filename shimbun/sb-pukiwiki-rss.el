;;; sb-pukiwiki-rss.el --- shimbun backend for pukiwiki-rss

;; Copyright (C) 2003 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Keywords: news
;; Created: Jun 19, 2003

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

(luna-define-class shimbun-pukiwiki-rss (shimbun-rss) ())

(defvar shimbun-pukiwiki-rss-content-start "\n</ul></span>\t+</td>")
(defvar shimbun-pukiwiki-rss-content-end "</address>")

(defvar shimbun-pukiwiki-group-alist
  '(("pukiwiki" "http://pukiwiki.org/index.php" "webmaster@pukiwiki.org"))
  "An alist of PukiWiki shimbun group definition.
Each element looks like \(NAME URL ADDRESS X-FACE\).
NAME is a shimbun group name.
URL is the URL for PukiWiki access point of the group.
ADDRESS is the e-mail address for the diary owner.
Optional X-FACE is a string for X-Face field.")

(luna-define-method initialize-instance :after ((shimbun shimbun-pukiwiki-rss)
						 &rest init-args)
  (shimbun-set-from-address-internal
   shimbun
   (nth 2 (assoc (shimbun-current-group-internal shimbun)
		 shimbun-pukiwiki-group-alist)))
  shimbun)

(luna-define-method shimbun-x-face ((shimbun shimbun-pukiwiki-rss))
  (or (shimbun-x-face-internal shimbun)
      (shimbun-set-x-face-internal
       shimbun
       (or
 	(nth 3 (assoc (shimbun-current-group-internal shimbun)
 		      shimbun-pukiwiki-group-alist))
 	(cdr (assoc "default" (shimbun-x-face-alist-internal shimbun)))
 	shimbun-x-face))))

(luna-define-method shimbun-groups ((shimbun shimbun-pukiwiki-rss))
  (mapcar 'car shimbun-pukiwiki-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-pukiwiki-rss))
  (concat (cadr (assoc (shimbun-current-group-internal shimbun)
		       shimbun-pukiwiki-group-alist)) "?cmd=rss10"))

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-pukiwiki-rss) url date)
  (let (page host datedesc)
    (unless (string-match "http:\\/\\/\\([^\/]+\\)\\/.+\\.php\\?\\(.+\\)" url)
      (error "Cannot find message-id base"))
    (setq host (match-string-no-properties 1 url)
	  page (match-string-no-properties 2 url))
    (unless (string-match "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)T\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)" date)
      (error "Cannot find message-id base"))
    (setq datedesc (concat (match-string-no-properties 1 date)
			   (match-string-no-properties 2 date)
			   (match-string-no-properties 3 date)
			   (match-string-no-properties 4 date)
			   (match-string-no-properties 5 date)))
  (format "%s%s%%rss@%s" datedesc page host)))

(provide 'sb-pukiwiki-rss)

;;; sb-pukiwiki-rss.el ends here

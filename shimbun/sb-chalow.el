;;; sb-chalow.el --- shimbun backend for chalow

;; Copyright (C) 2003, 2004, 2005 OHASHI Akira <bg66@koka-in.org>

;; Author: OHASHI Akira <bg66@koka-in.org>
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
;; Inc.; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)

(luna-define-class shimbun-chalow (shimbun) ())

(defvar shimbun-chalow-content-start "</span>")
(defvar shimbun-chalow-content-end "</div>")

(defcustom shimbun-chalow-group-alist nil
  "*An alist of CHALOW shimbun group definition.
Each element looks like \(NAME URL ADDRESS\).
NAME is a shimbun group name.
URL is the URL for CHALOW access point of the group.
ADDRESS is the e-mail address for the diary owner."
  :group 'shimbun
  :type '(repeat
	  (group (string :tag "Group name")
		 (string :tag "URL")
		 (string :tag "Mail address"))))

(luna-define-method shimbun-groups ((shimbun shimbun-chalow))
  (mapcar 'car shimbun-chalow-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-chalow))
  (cadr (assoc (shimbun-current-group-internal shimbun)
	       shimbun-chalow-group-alist)))

(defmacro shimbun-chalow-get-headers (shimbun url headers &optional aux)
  (` (let ((case-fold-search t))
       (goto-char (point-max))
       (while (re-search-backward "<a name=\"\\(\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)-[0-9]+\\)\" href=\"\\([-0-9]+\.html#[-0-9]+\\)\"><span class=\"[ps]anchor\">.+</span></a> <span class=\"clitemheader\">\\(.+\\)</span>" nil t)
	 (let ((id (match-string 1))
	       (year (match-string 2))
	       (month (match-string 3))
	       (day (match-string 4))
	       (url (match-string 5))
	       (subject (match-string 6))
	       date)
	   (setq date (shimbun-make-date-string (string-to-number year)
						(string-to-number month)
						(string-to-number day)))
	   (setq id (format "<%s.%s@chalow>"
			    id
			    (shimbun-current-group-internal (, shimbun))))
	   (push (shimbun-make-header
		  0
		  (shimbun-mime-encode-string subject)
		  (nth 2 (assoc (shimbun-current-group-internal shimbun)
				shimbun-chalow-group-alist))
		  date id "" 0 0 (concat
				  (shimbun-index-url (, shimbun))
				  url))
		 (, headers)))))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-chalow)
					 &optional range)
  (let ((case-fold-search t)
	(pages (shimbun-header-index-pages range))
	(count 0)
	headers months)
    (goto-char (point-min))
    (while (re-search-forward "<a href=\"\\([0-9][0-9][0-9][0-9]-[0-9][0-9]\\.html\\)\"" nil t)
      (push (match-string 1) months))
    (setq months (nreverse (sort months 'string<)))
    (catch 'stop
      (dolist (month months)
	(when (and pages (> (incf count) pages))
	  (throw 'stop nil))
	(let ((url (concat (shimbun-index-url shimbun) month)))
	  (erase-buffer)
	  (shimbun-retrieve-url url t)
	  (shimbun-chalow-get-headers shimbun url headers month))))
    headers))

(luna-define-method shimbun-make-contents ((shimbun shimbun-chalow)
					   header)
  (let ((case-fold-search t)
	(id (shimbun-header-id header))
	(start))
    (when (string-match "\\([-0-9]+\\)\." id)
      (setq id (substring id (match-beginning 1) (match-end 1)))
      (re-search-forward (concat "<a name=\"" id) nil t))
    (when (and (re-search-forward (shimbun-content-start shimbun) nil t)
	       (setq start (point))
	       (re-search-forward (shimbun-content-end shimbun) nil t))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start))
    (goto-char (point-min))
    (insert "<pre>")
    (shimbun-header-insert-and-buffer-string shimbun header nil t)))

(provide 'sb-chalow)

;;; sb-chalow.el ends here

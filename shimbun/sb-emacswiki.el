;;; sb-emacswiki.el --- emacswiki shimbun backend

;; Copyright (C) 2004 David Hansen

;; Author: David Hansen <david.hansen@physik.fu-berlin.de>
;; Version: 0.3
;; URL: http://www.emacswiki.org/cgi-bin/wiki/WThreeMShimbun
;; Keywords: emacs-w3m, gnus, hypermedia

;; This file is not part of GNU Emacs.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Change Log:
;;
;; 0.3:
;;  - new X-Face:
;; 0.2:
;;  - copied Markus Knittig's X-Face:

;;; Commentary:
;;
;; Read the fine emacs-w3m manual to find out more about the shimbun module.
;;
;; Install:
;;
;;   Put this file in e.g. ~/.elisp/ and add
;;
;;     (setq shimbun-server-additional-path '("~/.elisp"))
;;
;;   to your ~/.emacs.

;;; Code:

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-emacswiki (shimbun-rss) ())

(defvar shimbun-emacswiki-url
  "http://www.emacswiki.org/cgi-bin/wiki.pl?action=rss")
(defvar shimbun-emacswiki-groups '("diff"))
(defvar shimbun-emacswiki-from-address  "invalid@emacswiki.org")
(defvar shimbun-emacswiki-content-start "<h1>")
(defvar shimbun-emacswiki-content-end "<div class=\"footer\">")

(defvar shimbun-emacswiki-x-face-alist
  '(("default" . "X-Face: 'Is?R.u_yTmkkPe(`Zyec$CF<xHX/m-bK|ROSqoD|DDW6;z&\
/T$@b=k:F#n>ri1KJ)/XVXzJ~!dA'H{,F+;f-IaJ$2~S9ZU6U@_\"%*YzLz8kAxsX3(q`>a&zos\
\\9.[2/gpE76Fim]r7o7hz&@@O#d{`BXdD)i]DQBW,Z]#$5YWYNT}@Y{cm}O}ev`l`QAeZI*NN<\
e2ibWOZWTFz8j~/m")))

(luna-define-method shimbun-index-url ((shimbun shimbun-emacswiki))
  shimbun-emacswiki-url)

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-emacswiki)
						 &optional range)
  (mapcar
   (lambda (header)
     (let ((url (shimbun-header-xref header)))
       (when (string-match "id=.*?;\\(revision=[0-9]+\\)" url)
	 (shimbun-header-set-xref
          header
          (replace-match "diff=1" t nil url 1)))
       header))
     (luna-call-next-method)))

(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-emacswiki) url date)
  (unless (string-match "id=\\(.*?\\);revision=\\([0-9]+\\)" url)
    (error "Cannot find message-id base"))
  (concat "<" (match-string 1 url) (match-string 2 url) "@emacswiki.org>"))

(provide 'sb-emacswiki)

;;; sb-emacswiki.el ends here

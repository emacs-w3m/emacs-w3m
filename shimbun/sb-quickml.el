;;; sb-quickml.el --- shimbun backend for QuickML

;; Copyright (C) 2002  Free Software Foundation, Inc.

;; Author: ARISAWA Akihiro <ari@mbf.sphere.ne.jp>
;; Keywords: news

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'shimbun)
(require 'sb-mailarc)

(luna-define-class shimbun-quickml (shimbun-mailarc) ())

(defvar shimbun-quickml-groups '("quickml-users" "quickml-server"))
(defvar shimbun-quickml-url "http://namazu.org/~satoru/archives/")

(luna-define-method shimbun-index-url ((shimbun shimbun-mailarc))
  (shimbun-expand-url (concat (shimbun-current-group-internal shimbun) "/")
		      shimbun-quickml-url))

(luna-define-method shimbun-reply-to ((shimbun shimbun-mailarc))
  (concat (shimbun-current-group-internal shimbun) "@quickml.com"))

(provide 'sb-quickml)
;;; sb-quickml.el ends here

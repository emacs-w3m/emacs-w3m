;;; sb-emacs-w3m.el --- shimbun backend for emacs-w3m mailing list

;; Author: Akihiro Arisawa <ari@mbf.sphere.ne.jp>

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

;;; Code:

(require 'shimbun)
(require 'sb-namazu)
(luna-define-class shimbun-emacs-w3m (shimbun-namazu) ())

(defvar shimbun-emacs-w3m-url "http://www.namazu.org/~tsuchiya/emacs-w3m/ml/")
(defvar shimbun-emacs-w3m-groups '("emacs-w3m"))
(defvar shimbun-emacs-w3m-coding-system 'euc-jp)
(defvar shimbun-emacs-w3m-use-entire-index nil)

(luna-define-method shimbun-index-url ((shimbun shimbun-namazu))
  (shimbun-url-internal shimbun))

(provide 'sb-emacs-w3m)
;;; sb-emacs-w3m.el ends here

;;; sb-savannah.el --- shimbun backend for gnu list archives on savannah

;; Copyright (C) 2002 Yoichi NAKAYAMA <yoichi@FreeBSD.org>

;; Author: Yoichi NAKAYAMA <yoichi@FreeBSD.org>
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

(luna-define-class shimbun-savannah (shimbun-mailman) ())

(defvar shimbun-savannah-url "http://mail.gnu.org/pipermail")

(defvar shimbun-savannah-group-path-alist
  '(("emacs-announce" . "emacs-commit")
    ("emacs-devel" . "emacs-devel")
    ("emacs-bidi" . "emacs-bidi")
    ("emacs-diffs" . "emacs-diffs")))

(defvar shimbun-savannah-groups
  (mapcar 'car shimbun-savannah-group-path-alist))

(defvar shimbun-savannah-x-face-alist
  '(("default" . "X-Face: \"ve{jUI{XipH\"!p<$d]|*@,jKlwo,H{bJL~s@_/L\
<v\\(w1Fl@;[>_e)S*v59YRxw.-Xo`/z\n SJ3XL'Em*Qn#@<CbHA]{(nLVMcWhP{iRz\
eg#>v@W)6Ei!X,[sn+a=B0Tzj~QLWT+QLO'leRXxBKh-9\n )!C/L|Al):n[Cb!eSD]b\
xIUL{1DS0vSlMbcV0&W(;bg\"/)^j&L+VMB7<L*Zf@w:O!r!$n)(t\\qsxX~\n TXKI")))

(luna-define-method shimbun-index-url ((shimbun shimbun-mailman))
  (concat (shimbun-url-internal shimbun) "/"
	  (cdr (assoc (shimbun-current-group-internal shimbun)
		      shimbun-savannah-group-path-alist))))

(provide 'sb-savannah)

;;; sb-savannah.el ends here

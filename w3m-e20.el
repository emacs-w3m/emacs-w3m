;;; w3m-e20.el --- Emacs 20 specific functions for w3m.

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;          Shun-ichi GOTO     <gotoh@taiyo.co.jp>,
;;          Satoru Takabayashi <satoru-t@is.aist-nara.ac.jp>,
;;          Hideyuki SHIRAI    <shirai@meadowy.org>,
;;          Keisuke Nishida    <kxn30@po.cwru.edu>,
;;          Yuuichi Teranishi  <teranisi@gohome.org>,
;;          Akihiro Arisawa    <ari@mbf.sphere.ne.jp>,
;;          Katsumi Yamaoka    <yamaoka@jpl.org>
;; Keywords: w3m, WWW, hypermedia

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This module provides Emacs 20 specific functions.  Visit
;; <URL:http://emacs-w3m.namazu.org/> for more details of emacs-w3m.

;;; Code:

;; Dummy functions.
(defalias 'w3m-create-image 'ignore)
(defalias 'w3m-insert-image 'ignore)
(defalias 'w3m-image-type-available-p 'ignore)
(defalias 'w3m-setup-toolbar 'ignore)
(defalias 'w3m-update-toolbar 'ignore)
(defalias 'w3m-display-graphic-p 'ignore)
(defalias 'w3m-display-inline-image-p 'ignore)

(defsubst w3m-find-coding-system (obj)
  "Return OBJ if it is a coding-system."
  (if (coding-system-p obj) obj))

(defalias 'w3m-expand-path-name 'expand-file-name)

(provide 'w3m-e20)

;;; w3m-e20.el ends here.

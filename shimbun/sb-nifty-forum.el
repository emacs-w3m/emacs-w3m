;;; sb-nifty-forum.el --- shimbun backend for nifty forum

;; Copyright (C) 2005, 2006 YOSIDA Kozi

;; Author: YOSIDA Kozi
;; Keywords: news
;; Created: Mar 27, 2005

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

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-nifty-forum (shimbun-rss) ())

(defvar shimbun-nifty-forum-group-alist
  '(("fphys24" . "http://bbs.com.nifty.com/mes/FPHYS_B024/index.rdf")
    ("fphys12" . "http://bbs.com.nifty.com/mes/FPHYS_B012/index.rdf")
    ("fphys13" . "http://bbs.com.nifty.com/mes/FPHYS_B013/index.rdf")
    ("fphys14" . "http://bbs.com.nifty.com/mes/FPHYS_B014/index.rdf")
    ("fphys15" . "http://bbs.com.nifty.com/mes/FPHYS_B015/index.rdf")
    ("fphys16" . "http://bbs.com.nifty.com/mes/FPHYS_B016/index.rdf")
    ("fphys17" . "http://bbs.com.nifty.com/mes/FPHYS_B017/index.rdf")
    ("fphys18" . "http://bbs.com.nifty.com/mes/FPHYS_B018/index.rdf")
    ("fphys25" . "http://bbs.com.nifty.com/mes/FPHYS_B025/index.rdf")
    ("fsci01" . "http://bbs.com.nifty.com/mes/FSCI_B001/index.rdf")
    ("fsci04" . "http://bbs.com.nifty.com/mes/FSCI_B004/index.rdf")
    ("fsci09" . "http://bbs.com.nifty.com/mes/FSCI_B009/index.rdf")
    ("fsci10" . "http://bbs.com.nifty.com/mes/FSCI_B010/index.rdf")
    ("fsci11" . "http://bbs.com.nifty.com/mes/FSCI_B011/index.rdf")
    ("fsci17" . "http://bbs.com.nifty.com/mes/FSCI_B017/index.rdf")
    ("fsci18" . "http://bbs.com.nifty.com/mes/FSCI_B018/index.rdf")
    ("fsci20" . "http://bbs.com.nifty.com/mes/FSCI_B020/index.rdf")
    ("fsci21" . "http://bbs.com.nifty.com/mes/FSCI_B021/index.rdf")
    ("fsci23" . "http://bbs.com.nifty.com/mes/FSCI_B023/index.rdf")
    ("fsci26" . "http://bbs.com.nifty.com/mes/FSCI_B026/index.rdf")
    ("fsci28" . "http://bbs.com.nifty.com/mes/FSCI_B028/index.rdf")
    ("fsci29" . "http://bbs.com.nifty.com/mes/FSCI_B029/index.rdf")
    ("fsci30" . "http://bbs.com.nifty.com/mes/FSCI_B030/index.rdf")
    ("fsci31" . "http://bbs.com.nifty.com/mes/FSCI_B031/index.rdf")
    ("fsci32" . "http://bbs.com.nifty.com/mes/FSCI_B032/index.rdf")
    ("fsci34" . "http://bbs.com.nifty.com/mes/FSCI_B034/index.rdf")
    ("fsci35" . "http://bbs.com.nifty.com/mes/FSCI_B035/index.rdf")
    ("fsci36" . "http://bbs.com.nifty.com/mes/FSCI_B036/index.rdf")
    ("fsci37" . "http://bbs.com.nifty.com/mes/FSCI_B037/index.rdf")
    ("fsci27" . "http://bbs.com.nifty.com/mes/FSCI_B027/index.rdf")))
(defvar shimbun-nifty-forum-from-address  "forum@nifty.com")
(defvar shimbun-nifty-forum-content-start
  "<div class=\"main-top\">")
(defvar shimbun-nifty-forum-content-end "</table></div>")

(defvar shimbun-nifty-forum-groups
  (mapcar 'car shimbun-nifty-forum-group-alist))


(luna-define-method shimbun-index-url ((shimbun shimbun-nifty-forum))
  (cdr (assoc (shimbun-current-group shimbun)
	      shimbun-nifty-forum-group-alist)))

(provide 'sb-nifty-forum)

;;; sb-nifty-forum.el ends here

;;; sb-rediff.el --- shimbun backend for rediff.com

;; Copyright (C) 2004 S V N Vishwanathan <vishketan@yahoo.com>

;; Author: S V N Vishwanathan <vishketan@yahoo.com>
;; Keywords: news
;; Created: Nov 22, 2004

;; This file is not a part of shimbun.

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

(luna-define-class shimbun-rediff (shimbun-rss) ())

(defvar shimbun-rediff-url "http://www.rediff.com/rss/newsrss.xml")
(defvar shimbun-rediff-groups '("news"))
(defvar shimbun-rediff-from-address  "news@rediff.com")
(defvar shimbun-rediff-content-start "<BR></FONT>")
(defvar shimbun-rediff-content-end "</P></FONT></TD></TR><TR><TD>")

;; Print version has less ads

(luna-define-method shimbun-article :before 
  ((shimbun shimbun-rediff) header &optional outbuf)
  (let ((url (shimbun-article-url shimbun header)))
    (when (string-match "http://www.rediff.com/rss/redirect.php\\?\
url=http://www.rediff.com/\\(.+\\.htm\\)" url)
      (shimbun-header-set-xref header (concat "http://in.rediff.com/cms/print.jsp?docpath=" (match-string 1 url))))))

;; Three kinds of tags to strip from the print version

;; Type 1: 
;; <P align=center><B>
;; <A class="" target=new href="blah"> blah blah </A>
;; </B></P>

;; Type 2:
;;<TABLE cellSpacing=0 cellPadding=0 width=200 align=left border=0>
;; blah blah!
;; </TABLE></TD></TR></TABLE>

;; Type 3:
;;<UL><LI><STRONG>
;; <A class="" href="blah" target=new> blah blah </A>
;; </STRONG></LI></UL><P>


(luna-define-method shimbun-clear-contents :before
  ((shimbun shimbun-rediff) header)
  (when (luna-call-next-method)
    (shimbun-remove-tags "<P align=center><B>" "</B></P>")
    (shimbun-remove-tags "<TABLE cellSpacing=0 cellPadding=0 width=200 align=left border=0>" "</TABLE></TD></TR></TABLE>"  ) 
    (shimbun-remove-tags "<UL><LI><STRONG>" "</STRONG></LI></UL>")
    ) )

;; Build unique ID for the message
(luna-define-method shimbun-rss-build-message-id
  ((shimbun shimbun-rediff) url date)
  (string-match "http://www.rediff.com/rss/redirect.php\\?\
url=http://www.rediff.com/[A-Za-z]+/\\([0-9]+\\)/\\([^/]+\\)/\\(.+\\)\\.htm" url)
  (concat "<" (match-string 1 url) (match-string 2 url) (match-string 3 url) "@rediff.com>" )
  )

(provide 'sb-rediff)

;;; sb-rediff.el ends here

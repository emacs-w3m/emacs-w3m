;;; sb-cgi-board.el --- Shimbun backend for CGI_Board bulletin board systems

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Keywords: shimbun

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

;; This is a shimbun backend to browse CGI_Board bulletin board
;; systems, developed by KUROKI Gen <kuroki@math.tohoku.ac.jp>.

;;; Code:

(require 'shimbun)

(defcustom shimbun-cgi-board-group-alist
  '(("support" .
     "http://www.math.tohoku.ac.jp/~kuroki/support/BBS.cgi?b=cgi_board")
    ("kuroki.a" .
     "http://www.math.tohoku.ac.jp/~kuroki/keijiban/BBS.cgi?b=a")
    ("kuroki.b" .
     "http://www.math.tohoku.ac.jp/~kuroki/keijiban/BBS.cgi?b=b")
    ("kuroki.c" .
     "http://www.math.tohoku.ac.jp/~kuroki/keijiban/BBS.cgi?b=c")
    ("kuroki.e" .
     "http://www.math.tohoku.ac.jp/~kuroki/keijiban/BBS.cgi?b=e")
    ("nojiri" .
     "http://njb.virtualave.net/BBS.cgi?b=nmain")
    ("yamagata" .
     "http://ruitomo.com/~hiroo/bbs/BBS.cgi?b=kohobu"))
  "*An alist of CGI_Board bulletin systems and their URLs."
  :group 'shimbun
  :type '(repeat
	  (cons :format "%v" :indent 4
		(string :tag "Name")
		(string :tag "      URL"))))

(luna-define-class shimbun-cgi-board (shimbun) ())

(luna-define-method shimbun-groups ((shimbun shimbun-cgi-board))
  (mapcar 'car shimbun-cgi-board-group-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-cgi-board))
  (cdr (assoc (shimbun-current-group-internal shimbun)
	      shimbun-cgi-board-group-alist)))

(luna-define-method shimbun-x-face ((shimbun shimbun-cgi-board))
  nil)

(luna-define-method shimbun-headers ((shimbun shimbun-cgi-board)
				     &optional range)
  (catch 'found
    (let ((headers)
	  (base (shimbun-index-url shimbun)))
      (dolist (page (shimbun-cgi-board-get-pages shimbun range))
	(with-temp-buffer
	  (when (shimbun-fetch-url shimbun (concat base "&index&_f=" page))
	    (let (references)
	      (goto-char (point-min))
	      (while (re-search-forward "^<li>\\[\\([^]]+\\)\\] *\
<a name=\"[^\"]+\" href=\"\\([^\"]+\\)\" target=\"article\">\\([^<]*\\)</a> *\
<small>(\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\) \\([0-9:]+\\))</small>" nil t)
		(let* ((author (match-string 1))
		       (url (shimbun-expand-url (match-string 2) base))
		       (subject (match-string 3))
		       (date (shimbun-make-date-string
			      (string-to-number (match-string 4))
			      (string-to-number (match-string 5))
			      (string-to-number (match-string 6))
			      (match-string 7)))
		       (id (shimbun-cgi-board-make-message-id url)))
		  (when (shimbun-search-id shimbun id)
		    (throw 'found headers))
		  ;; Make alist of articles and their parent articles after
		  ;; new articles are found, in order to avoid waste access.
		  (unless references
		    (setq references
			  (shimbun-cgi-board-get-references shimbun page)))
		  ;; When the current subject is equal to the fragment,
		  ;; this fact represents that the original subject is empty.
		  (when (string-match
			 (concat "\\`[^#]*#" (regexp-quote subject) "\\'")
			 url)
		    (setq subject ""))
		  (push (shimbun-create-header
			 0 subject author date id
			 (when (setq id (cdr (assoc url references)))
			   (shimbun-cgi-board-make-message-id id))
			 nil nil
			 (if (string-match "\\`\\([^#]+\\)#" url)
			     (match-string 1 url)
			   url))
			headers)))))))
      headers)))

(defun shimbun-cgi-board-get-pages (shimbun &optional range)
  "Return a list of splited index pages."
  (with-temp-buffer
    (when (shimbun-fetch-url shimbun
			     (concat (shimbun-index-url shimbun) "&old"))
      (let ((pages)
	    (count 0)
	    (limit (shimbun-header-index-pages range)))
	(goto-char (point-min))
	(while (and (or (not limit) (<= (incf count) limit))
		    (re-search-forward
		     "<a href=\"\\./\\([^.]+\\.html\\)\" target=\"article\">"
		     nil t))
	  (push (match-string 1) pages))
	(nreverse pages)))))

(defun shimbun-cgi-board-get-references (shimbun page)
  "Return alist of articles and their parents."
  (let ((alist)
	(base (shimbun-index-url shimbun)))
    (with-temp-buffer
      (when (shimbun-fetch-url shimbun (concat base "&thread&_f=" page))
	(goto-char (point-max))
	(let (pair stack)
	  (while (re-search-backward "<a name=\"[^\"]+\" \
href=\"\\([^\"]+\\)\" target=\"article\">" nil t)
	    (setq pair (cons (shimbun-expand-url (match-string 1) base)
			     (progn
			       (forward-line 0)
			       (skip-chars-forward " "))))
	    (while (and stack (>= (cdar stack) (cdr pair)))
	      (pop stack))
	    (push (cons (car pair) (caar stack)) alist)
	    (push pair stack)))))
    alist))

(defun shimbun-cgi-board-make-message-id (url)
  (save-match-data
    (when (string-match "\\`http://\\([^/#]+\\)/[^#]*#\\(.*\\)\\'" url)
      (concat "<"
	      (match-string 2 url)
	      "@"
	      (match-string 1 url)
	      ">"))))

(luna-define-method shimbun-clear-contents ((shimbun shimbun-cgi-board) header)
  (let ((id (shimbun-header-id header)))
    (when (string-match "\\`<\\([^@]+\\)@" id)
      (goto-char (point-min))
      (let (start)
	(when (and (search-forward
		    (concat "\n<!--" (match-string 1 id) ":-->") nil t)
		   (setq start (match-end 0))
		   (re-search-forward "<!--[^-]*-->\n" nil t))
	  (delete-region (match-beginning 0) (point-max))
	  (delete-region (point-min) start)
	  (goto-char (point-min))
	  (when (looking-at "<hr[^>]*>")
	    (delete-region (match-beginning 0) (match-end 0)))
	  t)))))

(provide 'sb-cgi-board)

;;; sb-cgi-board.el ends here

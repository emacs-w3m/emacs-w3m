;;; w3m-hist.el --- a history management system for w3m
;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: w3m, WWW, hypermedia

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

;; w3m keeps history in the buffer-local variable `w3m-url-history'.
;; See the documentation of that variable for details.

;;; Code:

(eval-when-compile (require 'cl))

(defvar w3m-url-history nil
  "This variable contains a tree-structured complex list of all the links
you have visited, which is buffer-local.  For instance, it looks like
the following:

\[Branch-1.0.0.0]:                 +--> U1.0.0.0.0 --> U1.0.0.0.1
                                  |
    [Branch-1.0]:         +--> U1.0.0 --> U1.0.1 --> U1.0.2
                          |
         [Trunk]: U0 --> U1 --> U2 --> U3 --> U4 --> U5 --> U6
                                 |
    [Branch-2.0]:                +--> U2.0.0 --> U2.0.1 --> U2.0.2
                                 |
    [Branch-2.1]:                +--> U2.1.0 --> U2.1.1
                                                    |
\[Branch-2.1.1.0]:                                   +--> U2.1.1.0.0

In this case, the history element U1.0.0.0.0 represents the first link
of the first branch which is sprouted from the history element U1.0.0.

The trunk or each branch is a simple list which will have some history
elements.  History elements in the trunk or each branch should be
arranged in order of increasing precedence (the newest history element
should be the last element of the list).

Each history element represents a link which consists of the following
records:

     (URL ATTRIBUTES BRANCH BRANCH ...)

URL should be a string of an address of a link.  ATTRIBUTES is a plist
of any kind of data to supplement the URL as such as follows:

     (title \"The title string of the page\"
            date \"Thursday, 22-Mar-01 11:54:48 GMT\"
            last-modified \"Wednesday, 31-Jan-01 09:36:30 GMT\"
            content-type \"text/html\")

The rest BRANCHes are branches of the history element.  Branches
should also be arranged in order of increasing precedence (the newest
one should be located in the rightmost).  Each BRANCH will also be a
tree-structured complex list.  Thus the history structure will grow up
infinitely.  Do you have enough memories for it? :-p

There are special rules on the w3m history management system.  As you
might expect, the operation BACK on U2.0.0 goes to U2, one more BACK
goes to U1.  Well, where should we go next when the operation FORWARD
is performed on U1?  The rule is, to select the newest link you have
visited.  So, that operation should go to U1.0.0.

One more rule.  If you visit to the link U4 from U2.0.2 directly, jump
is occurred instead of to sprout the new branch from U2.0.2.

In addition, the variable `w3m-url-history' has a pointer in its car
cell to designate the current position in the history.  It is a list
of integers.  (0) points U0, (2 0 1) points U2.0.1, etc.  Finally the
value of `w3m-url-history' will be as such as follows:

\((2 0 1)
 (\"http://www.U0.org/\" (title \"U0\" content-type \"text/html\"))
 (\"http://www.U1.org/\" (title \"U1\" content-type \"text/html\")
  ((\"http://www.U100.org/\" (title \"U100\" content-type \"text/html\")
    ((\"http://www.U10000.org/\"
      (title \"U10000\" content-type \"text/html\"))
     (\"http://www.U10001.org/\"
      (title \"U10001\" content-type \"text/html\"))))
   (\"http://www.U101.org/\" (title \"U101\" content-type \"text/html\"))
   (\"http://www.U102.org/\" (title \"U102\" content-type \"text/html\"))))
 (\"http://www.U2.org/\" (title \"U2\" content-type \"text/html\")
  ((\"http://www.U200.org/\" (title \"U200\" content-type \"text/html\"))
   (\"http://www.U201.org/\" (title \"U201\" content-type \"text/html\"))
   (\"http://www.U202.org/\" (title \"U202\" content-type \"text/html\")))
  ((\"http://www.U210.org/\" (title \"U210\" content-type \"text/html\"))
   (\"http://www.U211.org/\" (title \"U211\" content-type \"text/html\")
    ((\"http://www.U21100.org/\"
      (title \"U21100\" content-type \"text/html\"))))))
 (\"http://www.U3.org/\" (title \"U3\" content-type \"text/html\"))
 (\"http://www.U4.org/\" (title \"U4\" content-type \"text/html\"))
 (\"http://www.U5.org/\" (title \"U5\" content-type \"text/html\"))
 (\"http://www.U6.org/\" (title \"U6\" content-type \"text/html\")))
")

(make-variable-buffer-local 'w3m-url-history)

(defun w3m-history-current ()
  "Return a history element of the current position."
  (when w3m-url-history
    (let ((position (car w3m-url-history))
	  element)
      (setq element (nth (pop position) (cdr w3m-url-history)))
      (while (> (length position) 0)
	(setq element (nth (pop position) (cddr element))
	      element (nth (pop position) element)))
      element)))

(defun w3m-history-forward ()
  "Move forward in the history and return a history element of the
position.  The position pointer of `w3m-url-history' will go forward.
If the next element does not exist in the history, it returns a
history element of the current position."
  (when w3m-url-history
    (let ((position (car w3m-url-history))
	  number element branch branches)
      (setq branch (cdr w3m-url-history)
	    number (pop position)
	    element (nth number branch))
      (while (> (length position) 0)
	(setq branch (nth (pop position) (cddr element))
	      number (pop position)
	      element (nth number branch)))
      (setq position (car w3m-url-history)
	    branches (cddr element))
      (cond (branches
	     ;; This element has branch(es).
	     (setq number (1- (length branches)))
	     (setcdr (nthcdr (1- (length position)) position)
		     (list number 0))
	     (car (nth number branches)))
	    ((> (length branch) (setq number (1+ number)))
	     ;; Next element exists in the branch.
	     (setcar (nthcdr (1- (length position)) position) number)
	     (nth number branch))
	    (t
	     ;; No next element.
	     element)))))

(defun w3m-history-backward ()
  "Move backward in the history and return a history element of the
position.  The position pointer of `w3m-url-history' will go backward.
If the previous element does not exist in the history, it returns a
history element of the current position."
  (when w3m-url-history
    (let* ((position (car w3m-url-history))
	   (class (1- (length position)))
	   (number (nth class position))
	   element)
      (if (zerop number)
	  ;; This element is the first element of the branch.
	  (if (zerop class)
	      ;; No previous element.
	      nil
	    ;; This element has a parent.
	    (setcdr (nthcdr (- class 2) position) nil))
	;; Previous element exists in the branch.
	(setcar (nthcdr class position) (1- number)))
      (setq element (nth (pop position) (cdr w3m-url-history)))
      (while (> (length position) 0)
	(setq element (nth (pop position) (cddr element))
	      element (nth (pop position) element)))
      element)))

(defun w3m-history-flat (&optional history position alist)
  "Return a flattened alist of `w3m-url-history'.  Each element will have
the following records:

\(URL ATTRIBUTES POSITION)

Where URL is a string of an address of a link, ATTRIBUTES is a plist
to supplement the URL, POSITION is a list of integers to designate the
current position in the history.  See the doc-string for the variable
`w3m-url-history' for more information.

Don't specify the optional arguments in normal use, they will be used
to recursive funcall itself internally."
  (when (or history
	    (setq history (cdr w3m-url-history)))
    (unless position
      (setq position '(t)))
    (let ((i 0)
	  element branches j)
      (while (setq element (pop history))
	(setcar (nthcdr (1- (length position)) position) i)
	(setq i (1+ i))
	(push (list (car element) (cadr element) (copy-sequence position))
	      alist)
	(when (setq branches (nthcdr 2 element))
	  (setq j 0)
	  (while branches
	    (setq alist (w3m-history-flat (pop branches)
					  (append position (list j t))
					  alist)
		  j (1+ j)))))
      (if (cdr position)
	  alist
	(nreverse alist)))))

(defun w3m-history-assoc (url &optional set-current)
  "Return a history element if URL is `equal' to the car of an element of
`w3m-url-history'.  The value is actually the element of the history
structure whose car equals URL.  If the optional argument SET-CURRENT
is non-nil, the position pointer of the history will come to designate
the element whose car equals URL."
  (let ((element (assoc url (w3m-history-flat)))
	position)
    (when element
      (setq position (nth 2 element))
      (when set-current
	(setcar w3m-url-history position))
      (setq element (nth (pop position) (cdr w3m-url-history)))
      (while (> (length position) 0)
	(setq element (nth (pop position) (cddr element))
	      element (nth (pop position) element)))
      element)))

;;(not-provided-yet 'w3m-hist)

;;; w3m-hist.el ends here

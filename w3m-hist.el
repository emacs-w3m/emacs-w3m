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

;; w3m keeps history in the buffer-local variables `w3m-history' and
;; `w3m-history-flat'.  Each variable contains a list of all the links
;; you have visited.  See the documentations for them for details.

;;; Code:

(eval-when-compile (require 'cl))

(defvar w3m-history nil
  "A buffer-local variable which contains a tree-structured complex list
of all the links you have visited.  For instance, it looks like the
following:

\[Branch-1.0.0.0]:                 +--> U1.0.0.0.0 --> U1.0.0.0.1
                                  |
    [Branch-1.0]:         +--> U1.0.0 --> U1.0.1 --> U1.0.2
                          |
         [Trunk]: U0 --> U1 --> U2 --> U3 --> U4 --> U5 --> U6
                                 |
    [Branch-2.0]:                +--> U2.0.0 --> U2.0.1
                                 |
    [Branch-2.1]:                +--> U2.1.0 --> U2.1.1 --> U2.1.2
                                                    |
\[Branch-2.1.1.0]:                                   +--> U2.1.1.0.0

In this case, the history element U1.0.0.0.0 represents the first link
of the first branch which is sprouted from the history element U1.0.0.

The trunk or each branch is a simple list which will have some history
elements.  History elements in the trunk or each branch should be
arranged in order of increasing precedence (the newest history element
should be the last element of the list).  Each history element
represents a link which consists of the following records:

	(URL PROPERTIES BRANCH BRANCH ...)

Where URL should be a string of an address of a link.  PROPERTIES is a
plist which contains any kind of data to propertize the URL as follows:

	(KEYWORD VALUE KEYWORD VALUE ...)

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

One more rule.  If you visit to the link U4 from U1.0.1 directly, jump
is occurred instead of to sprout the new branch from U1.0.1.

In addition, the variable `w3m-history' has a list of pointers in its
`car' cell which looks like the following:

	(PREV CURRENT NEXT)

Where the list PREV points the previous history element, the list
CURRENT points the current one, the list NEXT points the next one.
Each list contains an odd number of integers, e.g., (0) points U0,
\(2 0 1) points U2.0.1, etc.  Finally the value of `w3m-history' will
come to be as such as follows:

\(((1) (2) (2 1 0))
 (\"http://www.U0.org/\" (:title \"U0\" :foo \"bar\"))
 (\"http://www.U1.org/\" (:title \"U1\" :foo \"bar\")
  ((\"http://www.U100.org/\" (:title \"U100\" :foo \"bar\")
    ((\"http://www.U10000.org/\" (:title \"U10000\" :foo \"bar\"))
     (\"http://www.U10001.org/\" (:title \"U10001\" :foo \"bar\"))))
   (\"http://www.U101.org/\" (:title \"U101\" :foo \"bar\"))
   (\"http://www.U102.org/\" (:title \"U102\" :foo \"bar\"))))
 (\"http://www.U2.org/\" (:title \"U2\" :foo \"bar\")
  ((\"http://www.U200.org/\" (:title \"U200\" :foo \"bar\"))
   (\"http://www.U201.org/\" (:title \"U201\" :foo \"bar\")))
  ((\"http://www.U210.org/\" (:title \"U210\" :foo \"bar\"))
   (\"http://www.U211.org/\" (:title \"U211\" :foo \"bar\")
    ((\"http://www.U21100.org/\" (:title \"U21100\" :foo \"bar\"))))
   (\"http://www.U212.org/\" (:title \"U212\" :foo \"bar\"))))
 (\"http://www.U3.org/\" (:title \"U3\" :foo \"bar\"))
 (\"http://www.U4.org/\" (:title \"U4\" :foo \"bar\"))
 (\"http://www.U5.org/\" (:title \"U5\" :foo \"bar\"))
 (\"http://www.U6.org/\" (:title \"U6\" :foo \"bar\")))")

(defvar w3m-history-flat nil
  "A buffer-local variable to keep a flattened alist of `w3m-history'.
Each element will have the following records:

	(URL PROPERTIES POSITION)

Where URL is a string of an address of a link, PROPERTIES is a plist
to propertize the URL.  The sequence PROPERTIES is exactly the same
with the corresponding contents of `w3m-history'.  POSITION is a list
of integers to designate the current position in the history.  See the
documentation for the variable `w3m-history' for more information.")

(make-variable-buffer-local 'w3m-history)
(make-variable-buffer-local 'w3m-history-flat)

(defun w3m-history-previous-link-available-p ()
  "Return non-nil if the previous history element is available."
  (caar w3m-history))

(defun w3m-history-next-link-available-p ()
  "Return non-nil if the next history element is available."
  (caddar w3m-history))

(eval-when-compile
  (defmacro w3m-history-current-1 (position)
    (` (let* ((position (, position))
	      (element (nth (pop position) (cdr w3m-history))))
	 (while position
	   (setq element (nth (pop position) (cddr element))
		 element (nth (pop position) element)))
	 element))))

(defun w3m-history-current ()
  "Return a history element of the current position.  The value looks
like the following:

     (URL PROPERTIES BRANCH BRANCH ...)

See the documentation for the variable `w3m-history' for more
information."
  (when w3m-history
    (w3m-history-current-1 (cadar w3m-history))))

(defun w3m-history-forward ()
  "Move forward in the history and return a history element of the
position.  The position pointers of `w3m-history' will go forward.
If the next element does not exist in the history, it returns a
history element of the current position."
  (when w3m-history
    (let ((next (caddar w3m-history)))
      (prog1
	  (w3m-history-current-1 (or next (cadar w3m-history)))
	(when next
	  ;; Examine the next of the next history.
	  (let (branch number element branches)
	    (setq branch (cdr w3m-history)
		  number (pop next)
		  element (nth number branch))
	    (while next
	      (setq branch (nth (pop next) (cddr element))
		    number (pop next)
		    element (nth number branch)))
	    ;; (The value of `next' is nil.)
	    (cond ((setq branches (cddr element))
		   ;; The next element has branch(es).
		   (setq number (1- (length branches))
			 next (copy-sequence (caddar w3m-history)))
		   (setcdr (nthcdr (1- (length next)) next) (list number 0)))
		  ((> (length branch) (setq number (1+ number)))
		   ;; The next of the next element exists in the branch.
		   (setq next (copy-sequence (caddar w3m-history)))
		   (setcar (nthcdr (1- (length next)) next) number))))
	  ;; Shift left the position pointers.
	  (setcar w3m-history
		  (nconc (cdar w3m-history) (list next))))))))

(defun w3m-history-backward ()
  "Move backward in the history and return a history element of the
position.  The position pointers of `w3m-history' will go backward.
If the previous element does not exist in the history, it returns a
history element of the current position."
  (when w3m-history
    (let ((previous (copy-sequence (caar w3m-history))))
      (prog1
	  (w3m-history-current-1 (or previous (cadar w3m-history)))
	(when previous
	  ;; Examine the previous of the previous history.
	  (let* ((class (1- (length previous)))
		 (number (nth class previous)))
	    (if (zerop number)
		;; The previous element is the first of the branch.
		(if (zerop class)
		    ;; There is no previous of the previous element.
		    (setq previous nil)
		  ;; The previous element has a parent.
		  (setcdr (nthcdr (- class 2) previous) nil))
	      ;; The previous of the previous element exists in the branch.
	      (setcar (nthcdr class previous) (1- number))))
	  ;; Shift right the position pointers.
	  (setcdr (cdar w3m-history) nil)
	  (setcar w3m-history (cons previous (car w3m-history))))))))

(defun w3m-history-flat (&optional history position alist)
  "Set the buffer-local variable `w3m-history-flat' with the value of a
flattened alist of `w3m-history'.  See the documentation for the
variable `w3m-history-flat' for details.  Note that the optional
arguments should only be used to recursive funcall itself internally,
so don't specify them for the normal use."
  (if (or history
	  (setq history (cdr w3m-history)))
      (progn
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
	    (setq w3m-history-flat (nreverse alist)))))
    (setq w3m-history-flat nil)))

(defun w3m-history-assoc (url &optional set-current properties)
  "Return a history element if URL is `equal' to the `car' of an element
of `w3m-history-flat'.  Elements of the return value is actually the
contents of the history structure.  If the optional first argument
SET-CURRENT is non-nil, the position pointer will come to point the
element in the history structure and properties of the element will
be replaced with the second argument PROPERTIES if it is not t."
  (let ((element (assoc url w3m-history-flat))
	position)
    (prog1
	element
      (when (and element set-current)
	(setq position (nth 2 element))
	;; Set the current position.
	(setcar (cdar w3m-history) position)
	(unless (eq t properties)
	  ;; Replace properties in the current history element of both
	  ;; `w3m-history' and `w3m-history-flat' with the specified value.
	  (setcar (cdr element) properties))
	(let* ((class (1- (length position)))
	       (number (nth class position))
	       branch branches)
	  ;; Examine the previous history.
	  (if (zerop number)
	      ;; This element is the first of the branch.
	      (if (zerop class)
		  ;; There is no previous element.
		  (setq position nil)
		;; This element has a parent.
		(setq position (copy-sequence position))
		(setcdr (nthcdr (- class 2) position) nil))
	    ;; The previous element exists in the branch.
	    (setq position (copy-sequence position))
	    (setcar (nthcdr class position) (1- number)))
	  ;; Set the previous position pointer.
	  (setcar (car w3m-history) position)
	  ;; Examine the next history.
	  (setq position (cadar w3m-history)
		branch (cdr w3m-history)
		number (pop position)
		element (nth number branch))
	  (while position
	    (setq branch (nth (pop position) (cddr element))
		  number (pop position)
		  element (nth number branch)))
	  ;; (The value of `position' is nil.)
	  (cond ((setq branches (cddr element))
		 ;; This element has branch(es).
		 (setq number (1- (length branches))
		       position (copy-sequence (cadar w3m-history)))
		 (setcdr (nthcdr (1- (length position)) position)
			 (list number 0)))
		((> (length branch) (setq number (1+ number)))
		 ;; The next element exists in the branch.
		 (setq position (copy-sequence (cadar w3m-history)))
		 (setcar (nthcdr (1- (length position)) position) number)))
	  ;; Set the next position pointer.
	  (setcar (cddar w3m-history) position))))))

(defun w3m-history-push (url &optional properties)
  "Push URL and PROPERTIES onto both `w3m-history' and `w3m-history-flat'
as a new current history element.  URL should be a string of an
address of a link.  PROPERTIES is a plist to propertize the URL.
If the history element has already been registered in the history
structure, only position pointers of the history will be modified.
Even so, properties in the history element will be replaced with
PROPERTIES.  However, if PROPERTIES is t, properties in the history
won't be modified.  See the documentation for the variables
`w3m-history' and `w3m-history-flat' for more information."
  (let (element position)
    (cond ((null w3m-history)
	   ;; The dawn of the history.
	   (setq element (list url (and (not (eq t properties))
					properties))
		 position '(nil (0) nil)
		 w3m-history (list position element)
		 w3m-history-flat (list (append element '((0)))))
	   position)
	  ((w3m-history-assoc url t properties)
	   ;; URL has been registered in the history.
	   (car w3m-history))
	  (t
	   (when (eq t properties)
	     (setq properties nil))
	   (let* ((position (copy-sequence (cadar w3m-history)))
		  (class (1- (length position)))
		  (number 0))
	     (setq element (nthcdr (car position) (cdr w3m-history)))
	     (while (> class number)
	       (setq number (1+ number)
		     element (nth (nth number position) (cddar element))
		     number (1+ number)
		     element (nthcdr (nth number position) element)))
	     (if (cdr element)
		 ;; We should sprout a new branch.
		 (progn
		   (setq number (1- (length (car element))))
		   (setcdr (nthcdr class position) (list (1- number) 0))
		   (setcdr (nthcdr number (car element))
			   (list (list (list url properties)))))
	       ;; The current position is the last of the branch.
	       (setcar (nthcdr class position)
		       (1+ (car (nthcdr class position))))
	       (setcdr element (list (list url properties))))
	     (setq w3m-history-flat
		   (nconc w3m-history-flat
			  (list (list url properties position))))
	     (setcar w3m-history
		     (list (cadar w3m-history) position nil)))))))

(defun w3m-history-copy-1 (sequence)
  "Internal function used to `w3m-history-copy' for recursive funcall."
  (let ((index (length sequence))
	element rest)
    (while (> index 0)
      (setq index (1- index)
	    element (nth index sequence)
	    rest (cons (if (consp element)
			   (w3m-history-copy-1 element)
			 element)
		       rest)))
    rest))

(defun w3m-history-copy (buffer)
  "Copy the buffer-local variables `w3m-history' and `w3m-history-flat'
from BUFFER to the current buffer."
  (let (tree flat)
    (save-excursion
      (set-buffer buffer)
      (setq tree (w3m-history-copy-1 w3m-history)
	    flat (w3m-history-copy-1 w3m-history-flat)))
    (setq w3m-history tree
	  w3m-history-flat flat)))

(defun w3m-history-plist-get (keyword &optional url set-current)
  "Extract a value from the properties of a history element.  KEYWORD is
usually a symbol.  This function returns the value corresponding to
the KEYWORD, or nil if KEYWORD is not one of the keyword on the
properties.  If the optional first argument URL is omitted, it is
performed on the current history element.  If the optional second
argument SET-CURRENT is non-nil, the position pointer will come to
point the element in the history structure."
  (let ((element (if url
		     (w3m-history-assoc url set-current t)
		   (w3m-history-current))))
    (if element
	(plist-get (cadr element) keyword)
      (error "No history element found to be extracted."))))

(defun w3m-history-plist-put (keyword value &optional url set-current)
  "Change value in the properties of a history element of KEYWORD to
VALUE, and return the properties.  KEYWORD is usually a symbol and
VALUE is any object.  If the optional first argument URL is omitted,
it is performed on the current history element.  If the optional
second argument SET-CURRENT is non-nil, the position pointer will come
to point the element in the history structure."
  (let (flat tree)
    (if (if url
	    (when (setq flat (w3m-history-assoc url set-current t))
	      (setq tree (w3m-history-current-1 (caddr flat))))
	  (when (setq tree (w3m-history-current))
	    (setq flat (w3m-history-assoc (car tree)))))
	(let ((plist (plist-put (cadr tree) keyword value))
	      keyword value rest)
	  ;; Remove keyword-value pairs whose value is nil.
	  (while plist
	    (setq keyword (car plist)
		  value (cadr plist)
		  plist (cddr plist))
	    (when value
	      (setq rest (cons value (cons keyword rest)))))
	  (setq rest (nreverse rest))
	  (setcar (cdr tree) rest)
	  (setcar (cdr flat) rest))
      (error "No history element found to be modified."))))

(defun w3m-history-add-properties (properties &optional url set-current)
  "Add each keyword-value pair of PROPERTIES to the properties of a
history element.  Returns t if any property was changed, nil
otherwise.  If the optional first argument URL is omitted, it is
performed on the current history element.  If the optional second
argument SET-CURRENT is non-nil, the position pointer will come to
point the element in the history structure."
  (let (flat tree)
    (if (if url
	    (when (setq flat (w3m-history-assoc url set-current t))
	      (setq tree (w3m-history-current-1 (caddr flat))))
	  (when (setq tree (w3m-history-current))
	    (setq flat (w3m-history-assoc (car tree)))))
	(let ((plist (cadr tree))
	      keyword value changed rest)
	  (while properties
	    (setq keyword (car properties)
		  value (cadr properties)
		  properties (cddr properties))
	    (unless (equal (plist-get plist keyword) value)
	      (setq plist (plist-put plist keyword value)
		    changed t)))
	  ;; Remove keyword-value pairs whose value is nil.
	  (while plist
	    (setq keyword (car plist)
		  value (cadr plist)
		  plist (cddr plist))
	    (if value
		(setq rest (cons value (cons keyword rest)))
	      (setq changed t)))
	  (when changed
	    (setq rest (nreverse rest))
	    (setcar (cdr tree) rest)
	    (setcar (cdr flat) rest)
	    t))
      (error "No history element found to add properties."))))

(defun w3m-history-remove-properties (properties &optional url set-current)
  "Remove each keyword of the keyword-value pair of PROPERTIES from the
properties of a history element.  The values in PROPERTIES are ignored
\(treated as nil).  Returns t if any property was changed, nil
otherwise.  If the optional first argument URL is omitted, it is
performed on the current history element.  If the optional second
argument SET-CURRENT is non-nil, the position pointer will come to
point the element in the history structure."
  (let (flat tree)
    (if (if url
	    (when (setq flat (w3m-history-assoc url set-current t))
	      (setq tree (w3m-history-current-1 (caddr flat))))
	  (when (setq tree (w3m-history-current))
	    (setq flat (w3m-history-assoc (car tree)))))
	(let ((plist (cadr tree)))
	  (when plist
	    (let (keywords keyword value changed rest)
	      (while properties
		(setq keywords (cons (car properties) keywords)
		      properties (cddr properties)))
	      ;; Remove keyword-value pairs whose value is nil.
	      (while plist
		(setq keyword (car plist)
		      value (cadr plist)
		      plist (cddr plist))
		(if (or (memq keyword keywords)
			(not value))
		    (setq changed t)
		  (setq rest (cons value (cons keyword rest)))))
	      (when changed
		(setq rest (nreverse rest))
		(setcar (cdr tree) rest)
		(setcar (cdr flat) rest)
		t))))
      (error "No history element found to remove properties."))))

(defun w3m-history-rename-url (new-url &optional old-url set-current)
  "Rename the name of the url in a history element with NEW-URL.  If the
optional first argument OLD-URL is omitted, renaming is performed on
the current history element.  If the optional second argument
SET-CURRENT is non-nil, the position pointer will come to point the
element in the history structure."
  (let ((tree (w3m-history-current))
	url flat)
    (if (and tree
	     (setq url (car tree))
	     (if old-url
		 (when (setq flat (w3m-history-assoc old-url t t))
		   (setq tree (w3m-history-current)))
	       (setq flat (w3m-history-assoc url))))
	(prog1
	    (setcar tree new-url)
	  (setcar flat new-url)
	  (when (and old-url (not set-current))
	    (w3m-history-assoc url t t)))
      (error "No history element found to be renamed."))))

(provide 'w3m-hist)

;;; w3m-hist.el ends here

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

The history management system keeps URL string and PROPERTIES of each
history element to be shared by all the w3m buffers.  Note that if
there is need to use buffer-local properties, use the variable
`w3m-history-flat' instead.

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
be as such as follows:

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

	(URL PROPERTIES POSITION [LOCAL-PROPERTIES])

Where URL is a string of an address of a link, PROPERTIES is a plist
to propertize the URL.  The sequence PROPERTIES is exactly the same
with the corresponding contents of `w3m-history'.  POSITION is a list
of integers to designate the current position in the history.  The
sequence LOCAL-PROPERTIES is similar to PROPERTIES, but it is buffer-
local.  You can use the functions `w3m-history-plist-get',
`w3m-history-plist-put', `w3m-history-add-properties' and
`w3m-history-remove-properties' to manipulate the buffer-local
properties.  See the documentation for the variable `w3m-history' for
more information.")

(make-variable-buffer-local 'w3m-history)
(make-variable-buffer-local 'w3m-history-flat)

(eval-when-compile
  ;; Internal macros.  They are not serviceable in the other modules.

  (defmacro w3m-history-current-1 (position)
    "Return a history element located in the POSITION of `w3m-history'."
    (` (let* ((position (, position))
	      (element (nth (pop position) (cdr w3m-history))))
	 (while position
	   (setq element (nth (pop position) (cddr element))
		 element (nth (pop position) element)))
	 element)))

  (defmacro w3m-history-save-position (not-save &rest forms)
    "Save the position pointers of `w3m-history' when NOT-SAVE is nil and
evaluate FORMS like `progn'."
    (` (let ((oposition (unless (, not-save)
			  (let ((position (car w3m-history)))
			    (list (copy-sequence (car position))
				  (copy-sequence (cadr position))
				  (copy-sequence (caddr position)))))))
	 (prog1
	     (progn
	       (,@ forms))
	   (when oposition
	     (setcar w3m-history oposition))))))
  (put 'w3m-history-save-position 'lisp-indent-function 1)

  (defmacro w3m-history-forward-1 ()
    "Move one element forward in the history and return a position of the
element.  The position pointers of `w3m-history' will have new values."
    '(let ((next (caddar w3m-history)))
       (prog1
	   next
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
		   (nconc (cdar w3m-history) (list next)))))))

  (defmacro w3m-history-backward-1 ()
    "Move one element backward in the history and return a position of the
element.  The position pointers of `w3m-history' will have new values."
    '(let ((previous (caar w3m-history)))
       (prog1
	   previous
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
		   (setq previous (copy-sequence previous))
		   (setcdr (nthcdr (- class 2) previous) nil))
	       ;; The previous of the previous element exists in the branch.
	       (setq previous (copy-sequence previous))
	       (setcar (nthcdr class previous) (1- number))))
	   ;; Shift right the position pointers.
	   (setcdr (cdar w3m-history) nil)
	   (setcar w3m-history (cons previous (car w3m-history)))))))

  (defmacro w3m-history-modify-properties (old new &optional replace)
    "Merge NEW plist into OLD plist and return a modified plist.  If
REPLACE is non-nil, OLD will be replaced by NEW.  The return value
will not contain keyword-value pairs whose value is nil."
    (` (let ((properties (, new))
	     (rest (unless (, replace)
		     (, old))))
	 (if rest
	     (progn
	       (while properties
		 (setq rest (plist-put rest
				       (car properties) (cadr properties))
		       properties (cddr properties)))
	       (while rest
		 (when (cadr rest)
		   (setq properties (cons (cadr rest)
					  (cons (car rest) properties))
			 rest (cddr rest))))
	       (nreverse properties))
	   (while properties
	     (when (cadr properties)
	       (setq rest (cons (cadr properties)
				(cons (car properties) rest))
		     properties (cddr properties))))
	   (nreverse rest)))))

  (defmacro w3m-history-with-element (url set-current
					  error-message-if-no-elements
					  &rest forms)
    "Bind the variable `element' and evaluate FORMS like `progn'.  The
variable `element' will have a history element which comes from
`w3m-history-flat'.  URL specifies the position in the history
structure, and defaults to the current position.  If SET-CURRENT is
non-nil, the history element corresponding to URL will be set as the
current history element.  If history element is not available, signal
an error with ERROR-MESSAGE-IF-NO-ELEMENTS."
    (` (let (element)
	 (if (let ((url (, url))
		   (set-current (, set-current)))
	       (if url
		   (setq element (w3m-history-assoc url set-current))
		 (and (setq element (w3m-history-current))
		      (setq element (w3m-history-assoc (car element)
						       set-current)))))
	     (progn
	       (,@ forms))
	   (error "%s" (, error-message-if-no-elements))))))
  (put 'w3m-history-with-element 'lisp-indent-function 'defun))

;; Functions for internal use.
(defun w3m-history-seek-properties (url)
  "Seek properties corresponding to URL in all the w3m buffers except for
the current-buffer."
  (let* ((current (current-buffer))
	 (buffers (delq current (buffer-list)))
	 properties)
    (while (and (not properties)
		buffers)
      (set-buffer (pop buffers))
      (when (eq major-mode 'w3m-mode)
	(setq properties (cadr (w3m-history-assoc url)))))
    (set-buffer current)
    properties))

(defun w3m-history-share-properties (url properties)
  "Function used to keep properties of each history element to be shared
by all the w3m buffers."
  (let ((buffers (buffer-list))
	(current (current-buffer))
	flat)
    (while buffers
      (set-buffer (pop buffers))
      (when (and (eq major-mode 'w3m-mode)
		 (setq flat (w3m-history-assoc url)))
	(setcar (cdr flat) properties)
	(setcar (cdr (w3m-history-current-1 (caddr flat))) properties)))
    (set-buffer current)))

;; Generic functions.
(defun w3m-history-previous-link-available-p ()
  "Return non-nil if the previous history element is available."
  (caar w3m-history))

(defun w3m-history-next-link-available-p ()
  "Return non-nil if the next history element is available."
  (caddar w3m-history))

(defun w3m-history-current ()
  "Return a history element of the current position.  The value looks
like the following:

     (URL PROPERTIES BRANCH BRANCH ...)

See the documentation for the variable `w3m-history' for more
information."
  (when w3m-history
    (w3m-history-current-1 (cadar w3m-history))))

(defun w3m-history-forward (&optional count set-current)
  "Move forward COUNT times in the history structure and return a history
element of the position.  If COUNT is omitted, it defaults to number
one.  If COUNT is negative, moving backward is performed.  If there is
no room in the history, move as far as possible.  The position
pointers of `w3m-history' will have new values when SET-CURRENT is
non-nil."
  (when w3m-history
    (w3m-history-save-position set-current
      (cond ((or (unless count
		   (setq count 1))
		 (> count 0))
	     (while (and (w3m-history-forward-1)
			 (> (setq count (1- count)) 0))))
	    ((< count 0)
	     (while (and (w3m-history-backward-1)
			 (< (setq count (1+ count)) 0)))))
      (w3m-history-current-1 (cadar w3m-history)))))

(defun w3m-history-backward (&optional count set-current)
  "Move backward COUNT times in the history structure and return a history
element of the position.  If COUNT is omitted, it defaults to number
one.  If COUNT is negative, moving forward is performed.  If there is
no room in the history, move as far as possible.  The position
pointers of `w3m-history' will have new values when SET-CURRENT is
non-nil."
  (when w3m-history
    (w3m-history-save-position set-current
      (cond ((or (unless count
		   (setq count 1))
		 (> count 0))
	     (while (and (w3m-history-backward-1)
			 (> (setq count (1- count)) 0))))
	    ((< count 0)
	     (while (and (w3m-history-forward-1)
			 (< (setq count (1+ count)) 0)))))
      (w3m-history-current-1 (cadar w3m-history)))))

(defun w3m-history-flat ()
  "Set the buffer-local variable `w3m-history-flat' with the value of a
flattened alist of `w3m-history'.  See the documentation for the
variable `w3m-history-flat' for details."
  (setq w3m-history-flat nil)
  (when w3m-history
    (let ((history (cdr w3m-history))
	  (position (list 0))
	  element branches flag children)
      (while (setq element (pop history))
	(if (stringp (car element))
	    (progn
	      (push (list (car element) (cadr element) (reverse position))
		    w3m-history-flat)
	      (if (setq element (cddr element))
		  (progn
		    (setq history (append element history)
			  position (append (list 0 0) position))
		    (push (length element) branches))
		(setcar position (1+ (car position)))
		(setq flag t)
		(while (and flag
			    children
			    (zerop (setcar children (1- (car children)))))
		  (setq children (cdr children))
		  (if (zerop (setcar branches (1- (car branches))))
		      (progn
			(setq branches (cdr branches)
			      position (cddr position))
			(setcar position (1+ (car position))))
		    (setcar position 0)
		    (setcar (cdr position) (1+ (cadr position)))
		    (setq flag nil)))))
	  (setq history (append element history))
	  (push (length element) children))))
    (setq w3m-history-flat (nreverse w3m-history-flat))))

(defun w3m-history-tree (&optional newpos)
  "Make a tree-structured history in the variable `w3m-history' from the
value of `w3m-history-flat'.  The optional NEWPOS should be a list
of pointers which will be the `car' of the new value of `w3m-history'
if it is specified.  It defaults to the beginning of a history."
  (if w3m-history-flat
      (let ((flat w3m-history-flat)
	    element positions rest position)
	(setq w3m-history (list (list nil nil)))
	(while (setq element (pop flat))
	  (setq positions (caddr element)
		rest w3m-history)
	  (while positions
	    (setq position (pop positions))
	    (unless (> (length rest) position)
	      (setcdr (nthcdr (1- (length rest)) rest)
		      (make-list (- position (length rest) -1)
				 (list nil nil))))
	    (setq rest (nth position rest))
	    (when positions
	      (setq position (pop positions))
	      (unless (> (- (length rest) 2) position)
		(setcdr (nthcdr (1- (length rest)) rest)
			(make-list (- position (length rest) -3)
				   (list (list nil nil)))))
	      (setq rest (nth (+ position 2) rest))))
	  (setcar rest (car element))
	  (setcar (cdr rest) (cadr element)))
	(if newpos
	    (setq w3m-history (cons newpos w3m-history))
	  (setq w3m-history (cons (list nil nil (list 0)) w3m-history))
	  (w3m-history-forward-1))
	w3m-history)
    (setq w3m-history nil)))

(defun w3m-history-assoc (url &optional set-current properties replace-props)
  "Return a history element if URL is `equal' to the `car' of an element
of `w3m-history-flat'.  Elements of the return value is actually the
contents of the history structure.  If SET-CURRENT is non-nil, the
history element corresponding to URL will be set as the current
history element.  If REPLACE-PROPS is non-nil, existing properties
will be completely replaced with PROPERTIES, otherwise PROPERTIES will
be merged into existing properties."
  (let ((element (assoc url w3m-history-flat))
	position)
    (prog1
	element
      (when element
	;; Modify the value of shared properties.
	(let ((shared-plist (cadr element)))
	  (setq properties (w3m-history-modify-properties
			    shared-plist properties replace-props))
	  (cond ((and shared-plist properties)
		 (setcar shared-plist (car properties))
		 (setcdr shared-plist (cdr properties)))
		((or shared-plist properties)
		 (setcar (cdr element) properties)
		 (w3m-history-share-properties url properties))))
	(when set-current
	  (setq position (nth 2 element))
	  ;; Set the current position.
	  (setcar (cdar w3m-history) position)
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
	    (setcar (cddar w3m-history) position)))))))

(defun w3m-history-push (url &optional properties replace-props)
  "Push URL and PROPERTIES onto both `w3m-history' and `w3m-history-flat'
as a new current history element.  URL should be a string of an
address of a link.  PROPERTIES is a plist to propertize the URL.  If
the history element for URL has already been registered in the history
structure, URL will be set as the current history element.  If REPLACE-
PROPS is non-nil, existing properties will be completely replaced with
PROPERTIES, otherwise PROPERTIES will be merged into existing
properties.  See the documentation for the variables `w3m-history' and
`w3m-history-flat' for more information."
  (let (element position)
    (cond ((null w3m-history)
	   ;; The dawn of the history.
	   (let ((shared-plist (w3m-history-seek-properties url)))
	     (when shared-plist
	       (setq properties (w3m-history-modify-properties
				 shared-plist properties replace-props))
	       (w3m-history-share-properties url properties)))
	   (setq element (list url properties)
		 position '(nil (0) nil)
		 w3m-history (list position element)
		 w3m-history-flat (list (append element '((0)))))
	   position)
	  ((w3m-history-assoc url t properties)
	   ;; URL has been registered in the history.
	   (car w3m-history))
	  (t
	   (let ((shared-plist (w3m-history-seek-properties url)))
	     (when shared-plist
	       (setq properties (w3m-history-modify-properties
				 shared-plist properties replace-props))
	       (w3m-history-share-properties url properties)))
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

(defun w3m-history-copy (buffer)
  "Copy the buffer-local variables `w3m-history' and `w3m-history-flat'
from BUFFER to the current buffer.  This function keeps properties of
each history element to be shared between BUFFER and the current
buffer."
  (let ((current (current-buffer))
	position flat)
    (set-buffer buffer)
    (when w3m-history
      (setq position (list (copy-sequence (caar w3m-history))
			   (copy-sequence (cadar w3m-history))
			   (copy-sequence (caddar w3m-history)))
	    flat w3m-history-flat))
    (set-buffer current)
    (when position
      ;; Remove buffer-local properties from the new `w3m-history-flat'.
      (let (element rest)
	(while flat
	  (setq element (copy-sequence (car flat))
		flat (cdr flat))
	  (setcdr (cddr element) nil)
	  (push element rest))
	(setq w3m-history-flat (nreverse rest)))
      (w3m-history-tree position))))

(defun w3m-history-plist-get (keyword &optional url set-current local)
  "Extract a value from the properties of a history element.  KEYWORD is
usually a symbol.  This function returns the value corresponding to
the KEYWORD, or nil if KEYWORD is not one of the keyword on the
properties.  If URL is omitted, it is performed on the current history
element.  If SET-CURRENT is non-nil, the history element corresponding
to URL will be set as the current history element.  LOCAL specifies
whether to access the buffer-local properties."
  (unless url
    (setq url (car (w3m-history-current))))
  (let (element)
    (if (and url
	     (setq element (w3m-history-assoc url set-current)))
	(plist-get (if local
		       (cadddr element)
		     (cadr element))
		   keyword)
      (error "No history element found to be extracted."))))

(defun w3m-history-plist-put (keyword value &optional url set-current local)
  "Change value in the properties of a history element of KEYWORD to
VALUE, and return the new properties.  KEYWORD is usually a symbol and
VALUE is any object.  If URL is omitted, it is performed on the
current history element.  If SET-CURRENT is non-nil, the history
element corresponding to URL will be set as the current history
element.  LOCAL specifies whether to access the buffer-local
properties."
  (w3m-history-with-element url set-current
    "No history element found to be modified."
    (if local
	(let ((properties (w3m-history-modify-properties
			   nil
			   (plist-put (cadddr element) keyword value)
			   t)))
	  (setcdr (cddr element) (when properties
				   (list properties))))
      (let* ((shared-plist (cadr element))
	     (properties (w3m-history-modify-properties
			  nil (plist-put shared-plist keyword value) t)))
	(if (and shared-plist properties)
	    (progn
	      (setcar shared-plist (car properties))
	      (setcdr shared-plist (cdr properties))
	      shared-plist)
	  (w3m-history-share-properties (or url (car (w3m-history-current)))
					properties)
	  properties)))))

(defun w3m-history-add-properties (properties &optional url set-current local)
  "Add each keyword-value pair of PROPERTIES to the properties of a
history element.  Returns t if any property was changed, nil
otherwise.  If URL is omitted, it is performed on the current history
element.  If SET-CURRENT is non-nil, the history element corresponding
to URL will be set as the current history element.  LOCAL specifies
whether to access the buffer-local properties."
  (w3m-history-with-element url set-current
    "No history element found to add properties."
    (let* ((former (if local
		       (cadddr element)
		     (cadr element)))
	   (plist former)
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
	(if local
	    (setcdr (cddr element) (when rest
				     (list rest)))
	  (if (and former rest)
	      (progn
		(setcar former (car rest))
		(setcdr former (cdr rest)))
	    (w3m-history-share-properties (or url (car (w3m-history-current)))
					  rest)))
	t))))

(defun w3m-history-remove-properties (properties &optional url set-current
						 local)
  "Remove each keyword of the keyword-value pair of PROPERTIES from the
properties of a history element.  The values in PROPERTIES are ignored
\(treated as nil).  Returns t if any property was changed, nil
otherwise.  If URL is omitted, it is performed on the current history
element.  If SET-CURRENT is non-nil, the history element corresponding
to URL will be set as the current history element.  LOCAL specifies
whether to access the buffer-local properties."
  (w3m-history-with-element url set-current
    "No history element found to remove properties."
    (let* ((former (if local
		       (cadddr element)
		     (cadr element)))
	   (plist former))
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
	    (if local
		(setcdr (cddr element) (when rest
					 (list (nreverse rest))))
	      (if rest
		  (progn
		    (setq rest (nreverse rest))
		    (setcar former (car rest))
		    (setcdr former (cdr rest)))
		(w3m-history-share-properties (or url
						  (car (w3m-history-current)))
					      nil)))
	    t))))))

(defun w3m-history-rename-url (new-url
			       &optional old-url set-current this-buffer)
  "Rename the name of the url in a history element with NEW-URL.  If OLD-
URL is omitted, renaming is performed on the current history element.
If SET-CURRENT is non-nil, the history element corresponding to URL
will be set as the current history element.  If THIS-BUFFER is nil,
renaming will be done for all the w3m buffers."
  (w3m-history-with-element old-url set-current
    "No history element found to be renamed."
    (setcar element new-url)
    (setcar (w3m-history-current-1 (caddr element)) new-url))
  (unless this-buffer
    (let* ((current (current-buffer))
	   (buffers (delq current (buffer-list)))
	   element)
      (while buffers
	(set-buffer (pop buffers))
	(when (and (eq major-mode 'w3m-mode)
		   (setq element (w3m-history-assoc old-url)))
	  (setcar element new-url)
	  (setcar (w3m-history-current-1 (caddr element)) new-url)))
      (set-buffer current))))

(defun w3m-history-store-position ()
  "Store the current position point in the history structure."
  (interactive)
  (when (cadar w3m-history)
    (w3m-history-add-properties (list ':window-start (window-start)
				      ':position (point))
				nil nil t)
    (when (interactive-p)
      (message "The position point has registered"))))

(defun w3m-history-restore-position ()
  "Restore the last position point for the current url."
  (interactive)
  (when (cadar w3m-history)
    (let ((start (w3m-history-plist-get ':window-start nil nil t))
	  position)
      (cond ((and start
		  (setq position (w3m-history-plist-get ':position
							nil nil t)))
	     (set-window-start nil start)
	     (goto-char position))
	    ((interactive-p)
	     (message "No last position registered"))))))

(eval-when-compile
  (defvar w3m-arrived-db)
  (autoload 'w3m-goto-url "w3m"))

(defun w3m-history-add-arrived-db (&optional clear-history)
  "Add the arrived database to the history structure unreasonably.
If arg CLEAR-HISTORY is given, the current history will be cleared
in advance.  It's only a joke, you should NEVER use it."
  (interactive "P")
  (unless (eq 'w3m-mode major-mode)
    (error "`%s' must be invoked from a w3m buffer." this-command))
  (when (and w3m-arrived-db
	     (prog1
		 (yes-or-no-p
		  "Are you sure you really want to destroy the history? ")
	       (message "")))
    (when clear-history
      (setq w3m-history nil
	    w3m-history-flat nil))
    (let (url-title title)
      (mapatoms (function
		 (lambda (symbol)
		   (when symbol
		     (if (setq title (get symbol 'title))
			 (push (list (symbol-name symbol)
				     (list ':title title))
			       url-title)
		       (push (list (symbol-name symbol)) url-title)))))
		w3m-arrived-db)
      (apply 'w3m-history-push (nth (random (length url-title)) url-title))
      (while url-title
	(w3m-history-push (car (nth (random (length w3m-history-flat))
				    w3m-history-flat)))
	(apply 'w3m-history-push (pop url-title))))
    (w3m-goto-url "about://history/")))

(provide 'w3m-hist)

;;; w3m-hist.el ends here

;;; w3m-hist.el --- the history management system for emacs-w3m

;; Copyright (C) 2001, 2002, 2003 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
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
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Emacs-w3m keeps history in the buffer-local variables `w3m-history'
;; and `w3m-history-flat'.  Each variable contains a list of all the
;; links you have visited.  The behavior tracing history backward or
;; forward is controlled by the `w3m-history-reuse-history-elements'
;; variable.  See the documentations for those variables for details.

;;; Code:

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (dont-compile
    (condition-case nil
	:symbol-for-testing-whether-colon-keyword-is-available-or-not
      (void-variable
       (let (w3m-colon-keywords)
	 (load "w3m-kwds.el" nil t t)
	 (while w3m-colon-keywords
	   (set (car w3m-colon-keywords) (car w3m-colon-keywords))
	   (setq w3m-colon-keywords (cdr w3m-colon-keywords))))))))

(defcustom w3m-history-reuse-history-elements nil
  "Non-nil means reuse the history element when re-visiting the page.
Otherwise, a new history element will be created even if there are
elements for the same url in the history.

Emacs-w3m used to do as if it is non-nil, however it sometimes brought
about users' dissatisfaction.  For example, if a user visits the pages
A -> B -> C -> B in order, the operation BACK on the second B brings a
user to A.  ``That's weird!''  The reason why it occurred is that the
`w3m-history' variable only had the list `(A B C)' as a history and B
is the current position for that time.

The default value for this variable is `nil', and the `w3m-history'
variable can have the list `(A B C B)'.  Where contents of two B's are
the identical Lisp objects, so the Lisp resources won't be much wasted.

See the documentation for the variables `w3m-history' and
`w3m-history-flat' for more information."
  :group 'w3m
  :type '(boolean :format "%{%t%}: %[%v%]" :on "On" :off "Off"))

(defcustom w3m-history-minimize-in-new-session nil
  "Non-nil means minimize copied history so that there's only current page.
This variable only affects creating of the new session by copying."
  :group 'w3m
  :type '(boolean :format "%{%t%}: %[%v%]" :on "On" :off "Off"))

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

In this case, the U1.0.0.0.0 history element represents the first link
of the first branch which is sprouted from the U1.0.0 history element.

The trunk or each branch is a simple list which will contain some
history elements.  History elements in the trunk or each branch should
be arranged in order of increasing precedence (the newest history
element should be the last element of the list).  Each history element
represents a link which consists of the following records:

	(URL PROPERTIES BRANCH BRANCH ...)

Where URL should be a string of an address of a link.  PROPERTIES is a
plist which contains any kind of data to propertize the URL as follows:

	(KEYWORD VALUE KEYWORD VALUE ...)

PROPERTIES should always be a non-nil value in order to make it easy
to share the value in all history elements in every emacs-w3m buffers.

The rest BRANCHes are branches of the history element.  Branches
should also be arranged in order of increasing precedence (the newest
one should be located in the rightmost).  Each BRANCH will also be a
tree-structured complex list.  Therefore, the history structure will
grow up infinitely.  Do you have enough memories for it? :-p

In order to save the Lisp resources, URL strings and PROPERTIES in the
`w3m-history' variables are shared in all emacs-w3m buffers.  It means
that each element in two `w3m-history' variables can be compared by
`eq' rather than `equal'.  If there is a need to make some properties
buffer-local, use the `w3m-history-flat' variable instead.

There are special rules on the emacs-w3m history management system.
As you may expect, the operation BACK on U2.0.0 brings you to U2, and
one more BACK brings you to U1.  Well, where should we go next when
the operation FORWARD is performed on U1?  The rule is to select the
newest link you have visited.  So, that operation should go to U1.0.0.

Another rule is that if you visit the U4 link from U1.0.1 directly,
the new U4 link will be sprouted from U1.0.1 if the value of the
`w3m-history-reuse-history-elements' variable is `nil'.  Otherwise if
it is non-nil, jumping to the existing U4 link is performed rather
than to sprout the new branch from U1.0.1.

In addition, the `w3m-history' variable contains a list of pointers in
its `car' cell which looks like the following:

	(PREV CURRENT NEXT)

Where the list PREV points the previous history element, the list
CURRENT points the current one, the list NEXT points the next one.
Each list contains an odd number of integers, e.g., (0) points U0,
\(2 0 1) points U2.0.1, etc.  Finally, the value of the `w3m-history'
variable will be constructed as follows:

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
  "A buffer-local variable having a flattened alist of `w3m-history'.
Each element will contain the following records:

    (URL PROPERTIES POSITION [KEYWORD VALUE [KEYWORD VALUE ...]])

Where URL is a string of an address of a link, PROPERTIES is a plist
to propertize the URL.  The sequence PROPERTIES is the identical
object of the corresponding contents of `w3m-history'.  POSITION is a
list of integers to designate the current position in the history.
The rest [KEYWORD VALUE [KEYWORD VALUE ...]] is a property list
similar to PROPERTIES, but it is buffer-local.  You can use the
functions `w3m-history-plist-get', `w3m-history-plist-put',
`w3m-history-add-properties' and `w3m-history-remove-properties' to
manipulate the buffer-local properties.  See the documentation for the
`w3m-history' variable for more information.")

(make-variable-buffer-local 'w3m-history)
(make-variable-buffer-local 'w3m-history-flat)

;; Inline functions.
(defsubst w3m-history-assoc (url)
  "Return a history element if URL is `equal' to the `car' of an element
of `w3m-history-flat'."
  (assoc url w3m-history-flat))

(defsubst w3m-history-set-current (position)
  "Set POSITION in the `w3m-history' variable so that it is the current
history position, and return a list of new position pointers."
  (setcar w3m-history (w3m-history-regenerate-pointers position)))

;; Functions for internal use.
(defun w3m-history-element (position &optional flat)
  "Return a history element located in the POSITION of the history.  If
FLAT is omitted or nil, the value will be extracted from `w3m-history'
and represented with the `(URL PROPERTIES BRANCH BRANCH ...)' form.
Otherwise, the value will be extracted from `w3m-history-flat' and
represented with the `(URL PROPERTIES POSITION [KEYWORD VALUE ...])'
form.  The current position can be obtained by `(cadar w3m-history)'."
  (when position
    (if flat
	(let ((flat w3m-history-flat)
	      element)
	  (while flat
	    (if (equal (caddr (setq element (pop flat))) position)
		(setq flat nil)
	      (setq element nil)))
	  element)
      (let ((element (nth (pop position) (cdr w3m-history))))
	(while position
	  (setq element (nth (pop position) (cddr element))
		element (nth (pop position) element)))
	element))))

(defun w3m-history-previous-position (position)
  "Return a position pointer pointing a previous one of a history element
to whom POSITION points.  POSITION is a list of integers mentioned in
the `w3m-history' variable documentation.  There is no side effect to
modify the value of a given argument."
  (let (class number previous)
    (when position
      (setq class (1- (length position))
	    number (nth class position))
      (if (zerop number)
	  ;; This POSITION is the beginning of the branch.
	  (unless (zerop class)
	    ;; There's a parent.
	    (setq previous (copy-sequence position))
	    (setcdr (nthcdr (- class 2) previous) nil))
	;; This POSITION is not the beginning of the branch.
	(setq previous (copy-sequence position))
	(setcar (nthcdr class previous) (1- number))))
    previous))

(defun w3m-history-next-position (position)
  "Return a position pointer pointing a next one of a history element to
whom POSITION points.  POSITION is a list of integers mentioned in the
`w3m-history' variable documentation.  There is no side effect to
modify the value of a given argument."
  (let (next branch element number)
    (when position
      (setq next position
	    branch (cdr w3m-history)
	    element (nth (pop next) branch))
      (while next
	(setq branch (nth (pop next) (cddr element))
	      element (nth (pop next) branch)))
      (cond ((nth 2 element)
	     ;; There're branches sprouted from the POSITION.
	     (setq next (copy-sequence position))
	     (setcdr (nthcdr (1- (length next)) next)
		     (list (- (length element) 3) 0)))
	    ((> (length branch)
		(setq number (1+ (nth (1- (length position)) position))))
	     ;; This POSITION is not the end of the branch.
	     (setq next (copy-sequence position))
	     (setcar (nthcdr (1- (length next)) next) number))))
    next))

(defun w3m-history-set-plist (plist property value)
  "Like `plist-put', except that PLIST is modified so that PROPERTY's
value is VALUE (Emacs actually does so but XEmacs doesn't).  If VALUE
is nil, the PROPERTY-VALUE pair will be removed from PLIST.  This
function guarantees the return value will not be nil.  The value of
PLIST will be `(nil nil)' if there is no PROPERTY-VALUE pair."
  (let ((pair (memq property plist)))
    (if pair
	(if value
	    (setcar (cdr pair) value)
	  (if (eq (car plist) property)
	      (progn
		(setcar plist (nth 2 plist))
		(setcar (cdr plist) (nth 3 plist))
		(setcdr (cdr plist) (nthcdr 4 plist)))
	    (setcdr (nthcdr (- (length plist) (length pair) 1) plist)
		    (nthcdr 2 pair))))
      (when value
	(setcdr (nthcdr (1- (length plist)) plist) (list property value)))))
  plist)

(defun w3m-history-modify-properties (old new &optional replace)
  "Merge NEW plist into OLD plist and return a modified plist.
If REPLACE is non-nil, OLD will be replaced by NEW.  OLD plist is
modified; i.e., all the history elements that contain OLD plist as
properties will have a new value.  The return value will not contain
keyword-value pairs whose value is nil.  If there is no keyword-value
pairs, the value will be made into `(nil nil)'."
  (prog1
      old
    (if replace
	(progn
	  (setcar old (car new))
	  (setcdr old (or (cdr new) (list nil))))
      (while new
	(w3m-history-set-plist old (car new) (cadr new))
	(setq new (cddr new))))
    (setq new (copy-sequence old))
    (while new
      (w3m-history-set-plist old (car new) (cadr new))
      (setq new (cddr new)))))

(defun w3m-history-seek-element (url &optional newprops replace)
  "Search all the emacs-w3m buffers for a history element corresponding
to URL and return a copy of an history element found first.  NEPROPS
will be merged into properties of an element if REPLACE is nil,
otherwise properties of an element will be replaced with NEWPROPS."
  (let* ((current (current-buffer))
	 (buffers (cons current (delq current (buffer-list))))
	 element)
    (while buffers
      (set-buffer (pop buffers))
      (when (and (eq major-mode 'w3m-mode)
		 (setq element (w3m-history-assoc url)))
	(setq buffers nil)))
    (set-buffer current)
    (prog1
	(copy-sequence element)
      (when element
	(w3m-history-modify-properties (cadr element) newprops replace)))))

;; Generic functions.
(defun w3m-history-previous-link-available-p ()
  "Return non-nil if the previous history element is available."
  (caar w3m-history))

(defun w3m-history-next-link-available-p ()
  "Return non-nil if the next history element is available."
  (caddar w3m-history))

(defun w3m-history-backward (&optional count)
  "Move backward COUNT times in the history structure and return a cons
of a new history element and new position pointers of the history.
The position pointers of `w3m-history' will not change.  If COUNT is
omitted, it defaults to the number one.  If COUNT is negative, moving
forward is performed.  Return nil if there is no previous element."
  (when w3m-history
    (let ((oposition (copy-sequence (car w3m-history)))
	  position last)
      (cond ((or (unless count
		   (setq count 1))
		 (> count 0))
	     (while (and (> count 0)
			 (setq position (caar w3m-history)))
	       (w3m-history-set-current (setq last position))
	       (decf count)))
	    ((< count 0)
	     (while (and (< count 0)
			 (setq position (caddar w3m-history)))
	       (w3m-history-set-current (setq last position))
	       (incf count)))
	    (t ;; Don't move.
	     (setq last (cadar w3m-history))))
      (prog1
	  (when last
	    (cons (w3m-history-element (cadar w3m-history))
		  (car w3m-history)))
	(setcar w3m-history oposition)))))

(defun w3m-history-forward (&optional count)
  "Move forward COUNT times in the history structure and return a cons of
a new history element and new position pointers of the history.  The
position pointers of `w3m-history' will not change.  If COUNT is
omitted, it defaults to the number one.  If COUNT is negative, moving
backward is performed.  If there is no room in the history, move as
far as possible."
  (w3m-history-backward (- (or count 1))))

(defun w3m-history-regenerate-pointers (position)
  "Regenerate the `(PREV CURRENT NEXT)' style position pointers only by
the current POSITION."
  (list (w3m-history-previous-position position)
	position
	(w3m-history-next-position position)))

(defun w3m-history-flat ()
  "Set the buffer-local variable `w3m-history-flat' with the value of a
flattened alist of `w3m-history'.  See the documentation for the
`w3m-history-flat' variable for details."
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
  "Make a tree-structured history in the `w3m-history' variable by the
value of `w3m-history-flat'.  If the optional NEWPOS which should be a
position pointer (i.e., a list of integers) is specified, it will be
the current position of the new history.  Otherwise, it will be set to
the beginning position of the history."
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
	(push 'dummy w3m-history)
	(w3m-history-set-current (or newpos (list 0)))
	w3m-history)
    (setq w3m-history nil)))

(defun w3m-history-push (url &optional newprops replace)
  "Push URL and NEWPROPS onto both `w3m-history' and `w3m-history-flat'
as a new current history element, and return a list of new position
pointers.  URL should be a string of an address of a link.  NEWPROPS
is a plist to propertize the URL.  If the history element for URL has
already been registered in the history structure and the
`w3m-history-reuse-history-elements' variable is non-nil, that element
will be assigned as the current history element.  Otherwise, a new
history element will be created for given URL and set as the current
history element.  NEWPROPS will be merged into existing properties if
REPLACE is nil, otherwise properties will be replaced with NEWPROPS.
See the documentation for the variables `w3m-history' and
`w3m-history-flat' for more information."
  (let ((element (w3m-history-seek-element url newprops replace))
	position class number branch)
    (if element
	(setcdr (cdr element) nil)
      (setq element (list url (w3m-history-modify-properties newprops nil))))
    (cond
     ((null w3m-history)
      ;; The dawn of the history.
      (setq position (list nil (list 0) nil)
	    w3m-history (list position element)
	    w3m-history-flat (list (append element (list (list 0)))))
      position)

     ((and w3m-history-reuse-history-elements
	   (setq position (caddr (w3m-history-assoc url))))
      ;; Reuse the existing history element assigned to the current one.
      ;; The position pointers will be fixed with correct values after
      ;; visiting a page when moving back, moving forward or jumping from
      ;; the about://history/ page.
      (w3m-history-set-current position))

     (t
      ;; Sprout a new history element.
      (setq position (copy-sequence (cadar w3m-history))
	    class (1- (length position))
	    number 0
	    branch (nthcdr (car position) (cdr w3m-history)))
      (while (> class number)
	(setq number (1+ number)
	      branch (nth (nth number position) (cddar branch))
	      number (1+ number)
	      branch (nthcdr (nth number position) branch)))
      (if (cdr branch)
	  ;; We should sprout a new branch.
	  (progn
	    (setq number (1- (length (car branch))))
	    (setcdr (nthcdr class position) (list (1- number) 0))
	    (setcdr (nthcdr number (car branch)) (list (list element))))
	;; The current position is the last of the branch.
	(setcar (nthcdr class position)
		(1+ (car (nthcdr class position))))
	(setcdr branch (list element)))
      (setq w3m-history-flat (nconc w3m-history-flat
				    (list (append element (list position)))))
      (setcar w3m-history (list (cadar w3m-history) position nil))))))

(defun w3m-history-copy (buffer)
  "Copy the buffer-local variables `w3m-history' and `w3m-history-flat'
from BUFFER to the current buffer.  This function is used to share
properties of each history element between BUFFER and the current
buffer."
  (let ((current (current-buffer))
	position flat element rest)
    (set-buffer buffer)
    (when w3m-history
      (setq position (copy-sequence (cadar w3m-history))
	    flat w3m-history-flat))
    (set-buffer current)
    (when position
      (if w3m-history-minimize-in-new-session
	  (progn
	    (setq w3m-history-flat flat
		  element (copy-sequence (w3m-history-element position t)))
	    (setcdr (cdr element) nil)
	    (setq w3m-history (list (list nil (list 0) nil) element)
		  w3m-history-flat (list (append element (list (list 0))))))
	;; Remove buffer-local properties from the new `w3m-history-flat'.
	(while flat
	  (setq element (copy-sequence (car flat))
		flat (cdr flat))
	  (setcdr (cddr element) nil)
	  (push element rest))
	(setq w3m-history-flat (nreverse rest))
	(w3m-history-tree position)))))

(defun w3m-history-plist-get (keyword &optional global)
  "Extract a value from the properties of the current history element.
KEYWORD is usually a symbol.  This function returns the value
corresponding to the KEYWORD, or nil if KEYWORD is not one of the
keyword on the properties.  If GLOBAL is omitted or nil, this function
accesses global properties, otherwise accesses the buffer-local
properties."
  (let ((element (w3m-history-element (cadar w3m-history) t)))
    (plist-get (if global
		   (cadr element)
		 (cdddr element))
	       keyword)))

(defun w3m-history-add-properties (newprops &optional global)
  "Add each keyword-value pair of NEWPROPS to the properties of the
current history element, and return new properties.  If GLOBAL is
omitted or nil, this function accesses global properties, otherwise
accesses the buffer-local properties."
  (if global
      (cadr (w3m-history-seek-element
	     (car (w3m-history-element (cadar w3m-history)))
	     newprops))
    (let ((element (w3m-history-element (cadar w3m-history) t))
	  properties)
      (if element
	  (progn
	    (setq properties (cdddr element)
		  properties
		  (if properties
		      (w3m-history-modify-properties properties newprops)
		    ;; Use `w3m-history-modify-properties' to remove
		    ;; keyword-value pairs whose value is nil.
		    (w3m-history-modify-properties newprops nil)))
	    (unless (car properties) ;; check whether it is `(nil nil)'.
	      (setq properties nil))
	    (setcdr (cddr element) properties))
	(message "\
Warning: the history database may be something corrupted in this session.")
	(sit-for 1)
	nil))))

(defun w3m-history-plist-put (keyword value &optional global)
  "Change value in the properties of the current history element of
KEYWORD to VALUE, and return new properties.  KEYWORD is usually a
symbol and VALUE is any object.  If GLOBAL is omitted or nil, this
function accesses global properties, otherwise accesses the
buffer-local properties."
  (inline (w3m-history-add-properties (list keyword value) global)))

(defun w3m-history-remove-properties (properties &optional global)
  "Remove each keyword of the keyword-value pair of PROPERTIES from the
properties of the current history element.  The values in PROPERTIES
are ignored (treated as nil).  Returns new properties.  If GLOBAL is
omitted or nil, this function accesses global properties, otherwise
accesses the buffer-local properties."
  (let (rest)
    (while properties
      (setq rest (cons nil (cons (car properties) rest))
	    properties (cddr properties)))
    (inline (w3m-history-add-properties (nreverse rest) global))))

(defun w3m-history-store-position ()
  "Store the current cursor position in the history structure."
  (interactive)
  (when (cadar w3m-history)
    (w3m-history-add-properties (list :window-start (window-start)
				      :position (point)))
    (when (interactive-p)
      (message "The current cursor position has stored"))))

(defun w3m-history-restore-position ()
  "Restore the saved cursor position for the page."
  (interactive)
  (when (cadar w3m-history)
    (let ((start (w3m-history-plist-get :window-start))
	  position)
      (cond ((and start
		  (setq position (w3m-history-plist-get :position)))
	     (set-window-start nil start)
	     (goto-char position))
	    ((interactive-p)
	     (message "No cursor position registered"))))))

(defun w3m-history-minimize ()
  "Minimize the history so that there is only the current page."
  (interactive)
  (let ((position (cadar w3m-history))
	element)
    (when position
      (setq element (w3m-history-element position t))
      (setcar (cddr element) (list 0))
      (setq w3m-history-flat (list element)
	    w3m-history (list (list nil (list 0) nil)
			      (list (car element) (cadr element)))))))

(eval-when-compile
  (defvar w3m-arrived-db)
  (autoload 'w3m-goto-url "w3m"))

(defun w3m-history-add-arrived-db ()
  "Add the arrived database to the history structure unreasonably.
It's only a joke, you should NEVER use it."
  (interactive)
  (unless (eq 'w3m-mode major-mode)
    (error "`%s' must be invoked from an emacs-w3m buffer" this-command))
  (when (and w3m-arrived-db
	     (prog1
		 (yes-or-no-p
		  "Are you sure you really want to destroy the history? ")
	       (message "")))
    (setq w3m-history nil
	  w3m-history-flat nil)
    (let ((w3m-history-reuse-history-elements t)
	  url-title title)
      (mapatoms (lambda (symbol)
		  (when symbol
		    (if (setq title (get symbol 'title))
			(push (list (symbol-name symbol)
				    (list :title title))
			      url-title)
		      (push (list (symbol-name symbol)) url-title))))
		w3m-arrived-db)
      (apply 'w3m-history-push (nth (random (length url-title)) url-title))
      (while url-title
	(w3m-history-push (car (nth (random (length w3m-history-flat))
				    w3m-history-flat)))
	(apply 'w3m-history-push (pop url-title))))
    (w3m-goto-url "about://history/" t)))

(provide 'w3m-hist)

;;; w3m-hist.el ends here

;;; w3m-form.el --- Stuffs to handle <form> tag

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;          Yuuichi Teranishi  <teranisi@gohome.org>,
;;          Hideyuki SHIRAI    <shirai@meadowy.org>,
;;          Shun-ichi GOTO     <gotoh@taiyo.co.jp>,
;;          Akihiro Arisawa    <ari@mbf.sphere.ne.jp>
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

;; This file contains the stuffs to handle <form> tag on emacs-w3m.
;; For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/


;;; Code:

(require 'w3m)

(eval-when-compile
  (defvar w3m-current-forms))

(defcustom w3m-form-use-fancy-faces t
  "*Use fancy faces to fontify <form> tags."
  :group 'w3m
  :type 'boolean)

(defface w3m-form-face
  '((((class color) (background light)) (:foreground "cyan" :underline t))
    (((class color) (background dark)) (:foreground "red" :underline t))
    (t (:underline t)))
  "*Face to fontify forms."
  :group 'w3m-face)

;;; w3m-form structure:

(defun w3m-form-new (method action &optional baseurl charlst)
  "Return new form object."
  (vector 'w3m-form-object
	  (if (stringp method)
	      (intern method)
	    method)
	  (w3m-expand-url action baseurl)
	  charlst
	  nil))

(defsubst w3m-form-p (obj)
  "Return t if OBJ is a form object."
  (and (vectorp obj)
       (symbolp (aref 0 obj))
       (eq (aref 0 obj) 'w3m-form-object)))

(defmacro w3m-form-method (form)
  (` (aref (, form) 1)))
(defmacro w3m-form-action (form)
  (` (aref (, form) 2)))
(defmacro w3m-form-charlst (form)
  (` (aref (, form) 3)))
(defmacro w3m-form-plist (form)
  (` (aref (, form) 4)))
(defmacro w3m-form-put (form name value)
  (let ((tempvar (make-symbol "formobj")))
    (` (let (((, tempvar) (, form)))
	 (aset (, tempvar) 4
	       (plist-put (w3m-form-plist (, tempvar))
			  (intern (, name)) (, value)))))))
(defmacro w3m-form-get (form name)
  (` (plist-get (w3m-form-plist (, form)) (intern (, name)))))

(defun w3m-form-goto-next-field ()
  "Move to next form field and return the point.
If no field in forward, return nil without moving."
  (let* ((id (get-text-property (point) 'w3m-form-field-id))
	 (beg (if id
		  (next-single-property-change (point) 'w3m-form-field-id)
		(point)))
	 (next (next-single-property-change beg 'w3m-form-field-id)))
    (if next
	(goto-char next)
      nil)))

(defun w3m-form-make-form-data (form &optional urlencode)
  (let ((plist (w3m-form-plist form))
	(coding (w3m-form-charlst form))
	buf multipart)
    (setq coding
	  (or (catch 'det
		(while coding
		  (if (w3m-charset-to-coding-system (car coding))
		      (throw 'det (w3m-charset-to-coding-system (car coding)))
		    (setq coding (cdr coding)))))
	      w3m-current-coding-system
	      (w3m-charset-to-coding-system
	       (w3m-content-charset w3m-current-url))
	      w3m-default-coding-system))
    (while plist
      (let ((name (symbol-name (car plist)))
	    (value (cadr plist)))
	(cond
	 ((and (consp value)
	       (eq (car value) 'file))
	  (setq multipart t)
	  (setq buf (cons (cons name value) buf)))
	 ((and (consp value)
	       (consp (cdr value))
	       (consp (cadr value)))	; select.
	  (setq buf (cons (cons name (car value)) buf)))
	 ((consp value)			; checkbox
	  (setq buf (append (mapcar (lambda (x) (cons name x)) value)
			    buf)))
	 (value
	  (setq buf (cons (cons name value) buf))))
	(setq plist (cddr plist))))
    (when buf
      (if (and multipart
	       (not urlencode))
	  (let ((boundary (apply 'format "--_%d_%d_%d" (current-time)))
		file type)
	    (setq buf (nreverse buf))
	    (cons
	     (concat "multipart/form-data; boundary=" boundary)
	     (with-temp-buffer
	       (while buf
		 (if (and (consp (cdr (car buf)))
			  (eq (car (cdr (car buf))) 'file))
		     (progn
		       (setq file (expand-file-name (cdr (cdr (car buf)))))
		       (if (string= (setq type (w3m-local-content-type file))
				    "unknown")
			   (setq type "application/octet-stream"))
		       (insert boundary "\r\n"
			       "Content-Disposition: form-data; name=\""
			       (car (car buf))
			       "\"; filename=\"" file "\"\r\n"
			       "Content-Type: " type "\r\n"
			       "Content-Transfer-Encoding: binary\r\n\r\n")
		       (when (file-exists-p file)
			 (insert-file-contents-literally file)
			 (goto-char (point-max)))
		       (insert "\r\n"))
		   (insert boundary "\r\n"
			   "Content-Disposition: form-data; name=\""
			   (car (car buf))
			   "\"\r\n\r\n"
			   (cdr (car buf))
			   "\r\n"))
		 (setq buf (cdr buf)))
	       (insert boundary "--\r\n")
	       (buffer-string))))
	(mapconcat (lambda (elem)
		     (format "%s=%s"
			     (w3m-url-encode-string (car elem) coding)
			     (w3m-url-encode-string (if (stringp (cdr elem))
							(cdr elem)
						      "")
						    coding)))
		   buf "&")))))

;;;###autoload
(defun w3m-form-parse-buffer ()
  "Parse HTML data in this buffer and return form/map objects.
Check the cached form/map objects are available, and if available
return them with the flag."
  (or (when (w3m-cache-available-p w3m-current-url)
	(let ((forms (w3m-history-plist-get :forms w3m-current-url nil t)))
	  ;; Mark that `w3m-current-forms' is resumed from history.
	  (and forms (cons t forms))))
      (nreverse
       (w3m-form-parse-forms))))

(defun w3m-form-parse-forms ()
  "Parse Form/usemap objects in this buffer."
  (let ((case-fold-search t)
	forms tag)
    (goto-char (point-min))
    (while (re-search-forward (w3m-tag-regexp-of "form" "img") nil t)
      (setq tag (downcase (match-string 1)))
      (goto-char (match-end 1))
      (cond
       ((string= tag "img")
	;; Parse USEMAP property of IMG tag
	(w3m-parse-attributes (usemap)
	  (when usemap
	    (if (not (string-match "^#" usemap))
		(setq forms (cons nil forms)) ;; Sure ?
	      (setq usemap (substring usemap 1))
	      (setq forms
		    (cons (w3m-form-new "map" usemap)
			  forms))
	      (save-excursion
		(goto-char (point-min))
		(let (candidates)
		  (when (re-search-forward
			 (concat "<map +name=\"" usemap "\"[^>]*>") nil t)
		    (while (and (re-search-forward
				 (w3m-tag-regexp-of "area" "/map") nil t)
				(not (char-equal
				      (char-after (match-beginning 1))
				      ?/)))
		      (goto-char (match-end 1))
		      (w3m-parse-attributes (href alt)
			(when href
			  (setq candidates (cons (cons href (or alt href))
						 candidates)))))
		    (when candidates
		      (w3m-form-put (car forms)
				    "link"
				    (nreverse candidates))))))))))
       (t
	;; Parse attribute of FORM tag
	;; accept-charset <= charset,charset,...
	;; charset <= valid only w3mmee with frame
	(w3m-parse-attributes (action (method :case-ignore)
				      (accept-charset :case-ignore)
				      (charset :case-ignore))
	  (if accept-charset
	      (setq accept-charset (split-string accept-charset ","))
	    (when (and charset (eq w3m-type 'w3mmee))
	      (cond
	       ((string= charset "e")	;; w3mee without libmoe
		(setq accept-charset (list "euc-jp")))
	       ((string= charset "s")	;; w3mee without libmoe
		(setq accept-charset (list "shift-jis")))
	       ((string= charset "n")	;; w3mee without libmoe
		(setq accept-charset (list "iso-2022-7bit")))
	       (t				;; w3mee with libmoe
		(setq accept-charset (list charset))))))
	  (setq forms
		(cons (w3m-form-new
		       (or method "get")
		       (or action
			   (progn
			     (string-match w3m-url-components-regexp w3m-current-url)
			     (substring w3m-current-url 0
					(or (match-beginning 6) (match-beginning 8)))))
		       nil
		       accept-charset)
		      forms)))
	;; Parse form fields until </FORM>
	(while (and (re-search-forward
		     (w3m-tag-regexp-of "input" "textarea" "select" "/form")
		     nil t)
		    (not (char-equal (char-after (match-beginning 1)) ?/)))
	  (setq tag (downcase (match-string 1)))
	  (goto-char (match-end 1))	; go to end of tag name
	  (cond
	   ((string= tag "input")
	    ;; When <INPUT> is found.
	    (w3m-parse-attributes (name (value :decode-entity)
					(type :case-ignore) (checked :bool))
	      (when name
		(cond
		 ((string= type "submit")
		  ;; Submit button input, not set name and value here.
		  ;; They are set in `w3m-form-submit'.
		  nil)
		 ((string= type "checkbox")
		  ;; Check box input, one name has multiple values
		  ;; Value is list of item VALUE which has same NAME.
		  (let ((cvalue (w3m-form-get (car forms) name)))
		    (w3m-form-put (car forms) name
				  (if checked
				      (cons value cvalue)
				    cvalue))))
		 ((string= type "radio")
		  ;; Radio button input, one name has one value
		  (w3m-form-put (car forms) name
				(if checked value
				  (w3m-form-get (car forms) name))))
		 (t
		  ;; ordinaly text input
		  (w3m-form-put (car forms)
				name
				(or value (w3m-form-get (car forms)
							name))))))))
	   ((string= tag "textarea")
	    ;; When <TEXTAREA> is found.
	    (w3m-parse-attributes (name)
	      (let ((start (point))
		    value)
		(skip-chars-forward "^<")
		(setq value (buffer-substring start (point)))
		(when name
		  (w3m-form-put (car forms)
				name
				(or value (w3m-form-get (car forms) name)))))))
	   ;; When <SELECT> is found.
	   ((string= tag "select")
	    (let (vbeg svalue cvalue candidates)
	      (goto-char (match-end 1))
	      (w3m-parse-attributes (name)
		;; Parse FORM SELECT fields until </SELECT> (or </FORM>)
		(while (and (re-search-forward
			     (w3m-tag-regexp-of "option" "/select" "/form")
			     nil t)
			    (not (char-equal (char-after (match-beginning 1))
					     ?/)))
		  ;; <OPTION> is found
		  (goto-char (match-end 1)) ; goto very after "<xxxx"
		  (w3m-parse-attributes (value (selected :bool))
		    (setq vbeg (point))
		    (skip-chars-forward "^<")
		    (setq svalue
			  (mapconcat 'identity
				     (split-string
				      (buffer-substring vbeg (point)) "\n")
				     ""))
		    (unless value
		      (setq value svalue))
		    (when selected
		      (setq cvalue value))
		    (push (cons value svalue) candidates)))
		(when name
		  (w3m-form-put (car forms) name (cons
						  cvalue ; current value
						  (nreverse
						   candidates))))))))))))
    forms))

(defun w3m-form-resume (forms)
  "Resume content of all forms in the current buffer using FORMS."
  (save-excursion
    (goto-char (point-min))
    (let (fid type name form textareas)
      (while (w3m-form-goto-next-field)
	(setq fid (get-text-property (point) 'w3m-form-field-id))
	(when (and fid
		   (string-match
		    "fid=\\([^/]+\\)/type=\\([^/]+\\)/name=\\(.*\\)$"
		    fid))
	  (setq form (nth (string-to-number (match-string 1 fid))
			  forms)
		type (match-string 2 fid)
		name (match-string 3 fid))
	  (cond
	   ((string= type "submit")
	    ;; Remove status to support forms containing multiple
	    ;; submit buttons.
	    (w3m-form-put form name nil))
	   ((or (string= type "reset")
		(string= type "hidden")
		;; Do nothing.
		))
	   ((string= type "password")
	    (w3m-form-replace (w3m-form-get form name)
			      'invisible))
	   ((or (string= type "checkbox")
		(string= type "radio"))
	    (when (stringp (w3m-form-get form name))
	      (w3m-form-replace
	       (if (string= (w3m-form-get form name)
			    (nth 3 (w3m-action)))
		   "*" " "))))
	   ((string= type "select")
	    (let ((selects (w3m-form-get form name)))
	      (when (car selects)
		(w3m-form-replace (cdr (assoc (car selects) (cdr selects)))))))
	   ((string= type "textarea")
	    (let ((hseq (nth 2 (w3m-action))))
	      (when (> hseq 0)
		(setq textareas
		      (cons (cons hseq (w3m-form-get form name))
			    textareas)))))
	   ((string= type "file")
	    (let ((value (w3m-form-get form name)))
	      (when (and value
			 (consp value))
		(w3m-form-replace (cdr value)))))
	   (t
	    (let ((value (w3m-form-get form name)))
	      (when value
		(w3m-form-replace value)))))))
      (dolist (textarea textareas)
	(when (cdr textarea)
	  (w3m-form-textarea-replace (car textarea) (cdr textarea)))))))

;;;###autoload
(defun w3m-fontify-forms ()
  "Process half-dumped data in this buffer and fontify <input_alt> tags."
  ;; Check whether `w3m-current-forms' is resumed from history.
  (if (eq t (car w3m-current-forms))
      (progn
	(setq w3m-current-forms (cdr w3m-current-forms))
	(w3m-form-fontify w3m-current-forms)
	(w3m-form-resume w3m-current-forms))
    (w3m-form-fontify w3m-current-forms)))

(eval-and-compile
  (unless (fboundp 'w3m-form-make-button)
    (defun w3m-form-make-button (start end properties)
      "Make button on the region from START to END."
      (add-text-properties start end (append '(face w3m-form-face) properties)))))

(defun w3m-form-fontify (forms)
  "Process half-dumped data in this buffer and fontify <input_alt> tags using FORMS."
  (goto-char (point-min))
  (while (search-forward "<input_alt " nil t)
    (let (start)
      (setq start (match-beginning 0))
      (goto-char (match-end 0))
      (w3m-parse-attributes ((fid :integer)
			     (type :case-ignore)
			     (width :integer)
			     (maxlength :integer)
			     (hseq :integer)
			     name value)
	(delete-region start (point))
	(search-forward "</input_alt>")
	(goto-char (match-beginning 0))
	(delete-region (match-beginning 0) (match-end 0))
	(let ((form (nth fid forms)))
	  (when form
	    (cond
	     ((and (string= type "hidden")
		   (string= name "link"))
	      (add-text-properties
	       start (point)
	       `(face
		 w3m-form-face
		 w3m-action (w3m-form-input-map ,form ,name)
		 w3m-cursor-anchor (w3m-form-input-map ,form ,name))))
	     ((string= type "submit")
	      (w3m-form-make-button
	       start (point)
	       `(w3m-action (w3m-form-submit ,form ,name ,value)
		 w3m-submit (w3m-form-submit ,form ,name
					     (w3m-form-get ,form ,name))
		 w3m-cursor-anchor (w3m-form-submit ,form))))
	     ((string= type "reset")
	      (w3m-form-make-button
	       start (point)
	       `(w3m-action (w3m-form-reset ,form)
		 w3m-cursor-anchor (w3m-form-reset ,form))))
	     ((string= type "textarea")
	      (add-text-properties
	       start (point)
	       `(face
		 w3m-form-face
		 w3m-action (w3m-form-input-textarea ,form ,hseq)
		 w3m-submit (w3m-form-submit ,form ,name
					     (w3m-form-get ,form ,name))
		 w3m-form-hseq ,hseq))
	      (when (> hseq 0)
		(add-text-properties
		 start (point)
		 `(w3m-cursor-anchor
		   (w3m-form-input-textarea ,form ,hseq)
		   w3m-form-name ,name))))
	     ((string= type "select")
	      (add-text-properties
	       start (point)
	       `(face
		 w3m-form-face
		 w3m-action (w3m-form-input-select ,form ,name)
		 w3m-submit (w3m-form-submit ,form ,name
					     (w3m-form-get ,form ,name))
		 w3m-cursor-anchor (w3m-form-input-select ,form ,name))))
	     ((string= type "password")
	      (add-text-properties
	       start (point)
	       `(face
		 w3m-form-face
		 w3m-action (w3m-form-input-password ,form ,name)
		 w3m-submit (w3m-form-submit ,form ,name
					     (w3m-form-get ,form ,name))
		 w3m-cursor-anchor (w3m-form-input-password ,form ,name))))
	     ((string= type "checkbox")
	      (add-text-properties
	       start (point)
	       `(face
		 w3m-form-face
		 w3m-action (w3m-form-input-checkbox ,form ,name ,value)
		 w3m-submit (w3m-form-submit ,form ,name
					     (w3m-form-get ,form ,name))
		 w3m-cursor-anchor (w3m-form-input-checkbox ,form ,name
							    ,value))))
	     ((string= type "radio")
	      (add-text-properties
	       start (point)
	       `(face
		 w3m-form-face
		 w3m-action (w3m-form-input-radio ,form ,name ,value)
		 w3m-submit (w3m-form-submit ,form ,name
					     (w3m-form-get ,form ,name))
		 w3m-cursor-anchor (w3m-form-input-radio ,form ,name ,value))))
	     ((string= type "file")
	      (add-text-properties
	       start (point)
	       `(face
		 w3m-form-face
		 w3m-action (w3m-form-input-file ,form ,name ,value)
		 w3m-submit (w3m-form-submit ,form ,name
					     (w3m-form-get ,form ,name))
		 w3m-cursor-anchor (w3m-form-input-file ,form ,name ,value))))
	     (t ;; input button.
	      (add-text-properties
	       start (point)
	       `(face
		 w3m-form-face
		 w3m-action (w3m-form-input ,form ,name ,type
					    ,width ,maxlength ,value)
		 w3m-submit (w3m-form-submit ,form ,name
					     (w3m-form-get ,form ,name))
		 w3m-cursor-anchor (w3m-form-input ,form ,name ,type
						   ,width ,maxlength
						   ,value)))))))
	(put-text-property start (point)
			   'w3m-form-field-id
			   (format "fid=%d/type=%s/name=%s" fid type name))))))


(defun w3m-form-replace (string &optional invisible)
  (let* ((start (text-property-any
		 (point-min)
		 (point-max)
		 'w3m-action
		 (w3m-get-text-property-around 'w3m-action)))
	 (width (string-width
		 (buffer-substring
		  start
		  (next-single-property-change start 'w3m-action))))
	 (prop (text-properties-at start))
	 (p (point))
	 (buffer-read-only))
    (goto-char start)
    (insert (setq string
		  (if invisible
		      (make-string (length string) ?.)
		    (mapconcat 'identity
			       (split-string
				(truncate-string string width) "\n")
			       "")))
	    (make-string (- width (string-width string)) ?\ ))
    (delete-region (point)
		   (next-single-property-change (point) 'w3m-action))
    (add-text-properties start (point) prop)
    (prog1 (point)
      (goto-char p))))

(defun w3m-form-input (form name type width maxlength value)
  (save-excursion
    (let* ((fvalue (w3m-form-get form name))
	   (input (read-from-minibuffer (concat (upcase type) ": ") fvalue)))
      (w3m-form-put form name input)
      (w3m-form-replace input))))

(defun w3m-form-input-password (form name)
  (let* ((fvalue (w3m-form-get form name))
	 (input (read-passwd (concat "PASSWORD"
				     (if fvalue
					 " (default is no change)")
				     ": ")
			     nil
			     fvalue)))
    (w3m-form-put form name input)
    (w3m-form-replace input 'invisible)))

(defun w3m-form-input-checkbox (form name value)
  (let ((fvalue (w3m-form-get form name)))
    (if (member value fvalue)		; already checked
	(progn
	  (w3m-form-put form name (delete value fvalue))
	  (w3m-form-replace " "))
      (w3m-form-put form name (cons value fvalue))
      (w3m-form-replace "*"))))

(defun w3m-form-input-radio (form name value)
  ;; Uncheck all RADIO input having same NAME
  (save-excursion
    (let ((id (get-text-property (point) 'w3m-form-field-id)))
      (goto-char 1)
      (while (w3m-form-goto-next-field)
	(if (string= id (get-text-property (point) 'w3m-form-field-id))
	    (w3m-form-replace " "))))) ; erase check
  ;; Then set this field as checked.
  (w3m-form-put form name value)
  (w3m-form-replace "*"))

(defun w3m-form-input-file (form name value)
  (let ((input (read-file-name "File name: "
			       (or (cdr (w3m-form-get form name))
				   "~/"))))
    (w3m-form-put form name (cons 'file input))
    (w3m-form-replace input)))

;;; TEXTAREA

(defcustom w3m-form-input-textarea-buffer-lines 10
  "*Buffer lines for form textarea buffer."
  :group 'w3m
  :type 'integer)

(defcustom w3m-form-input-textarea-mode-hook nil
  "*A hook called after w3m-form-input-textarea-mode."
  :group 'w3m
  :type 'hook)

(defcustom w3m-form-input-textarea-set-hook nil
  "*A Hook called before w3m-form-input-textarea-set."
  :group 'w3m
  :type 'hook)

(defun w3m-form-text-chop (text)
  "Return a list of substrings of TEXT which are separated by newline
character."
  (let ((start 0) parts)
    (while (string-match "\n" text start)
      (setq parts (cons (substring text start (match-beginning 0)) parts)
	    start (match-end 0)))
    (nreverse (cons (substring text start) parts))))

(defun w3m-form-textarea-replace (hseq string)
  (let ((s (get-text-property (point) 'w3m-form-hseq))
	(hseq (abs hseq))
	(chopped (w3m-form-text-chop string))
	(p (point))
	cs next)
    (unless (and s (eq s hseq))
      (goto-char (point-min))
      (while (and (not (eobp))
		  (not (eq hseq (get-text-property (point) 'w3m-form-hseq))))
	(goto-char (next-single-property-change (point)
						'w3m-form-hseq))))
    (while chopped
      (w3m-form-replace (car chopped))
      (goto-char (next-single-property-change (point) 'w3m-form-hseq)) ; end
      (when (setq next (next-single-property-change (point) 'w3m-form-hseq))
	(goto-char next))
      (setq cs (get-text-property (point) 'w3m-form-hseq))
      (setq chopped
	    (if (and cs (eq (abs cs) hseq))
		(cdr chopped))))
    (goto-char p)))

(defun w3m-form-textarea-info ()
  "Return a cons cell of (NAME . LINE) for current text area."
  (let ((s (w3m-get-text-property-around 'w3m-form-hseq))
	(lines 0)
	next)
    (save-excursion
      (when (and s (not (> s 0)))
	(while (and (not (bobp))
		    (not (eq (abs s) (get-text-property (point)
							'w3m-form-hseq))))
	  (goto-char (previous-single-property-change
		      (point) 'w3m-form-hseq))
	  (when (and (get-text-property (point) 'w3m-form-hseq)
		     (setq next (previous-single-property-change
				 (point)
				 'w3m-form-hseq))
		     (goto-char next)))
	  (incf lines)))
      (cons (w3m-get-text-property-around 'w3m-form-name) lines))))

(defvar w3m-form-input-textarea-keymap nil)
(unless w3m-form-input-textarea-keymap
  (setq w3m-form-input-textarea-keymap (make-sparse-keymap))
  (define-key w3m-form-input-textarea-keymap "\C-c\C-c"
    'w3m-form-input-textarea-set)
  (define-key w3m-form-input-textarea-keymap "\C-c\C-q"
    'w3m-form-input-textarea-exit)
  (define-key w3m-form-input-textarea-keymap "\C-c\C-k"
    'w3m-form-input-textarea-exit))

(defvar w3m-form-input-textarea-buffer nil)
(defvar w3m-form-input-textarea-form nil)
(defvar w3m-form-input-textarea-hseq nil)
(defvar w3m-form-input-textarea-point nil)
(defvar w3m-form-input-textarea-wincfg nil)
(make-variable-buffer-local 'w3m-form-input-textarea-buffer)
(make-variable-buffer-local 'w3m-form-input-textarea-form)
(make-variable-buffer-local 'w3m-form-input-textarea-hseq)
(make-variable-buffer-local 'w3m-form-input-textarea-point)
(make-variable-buffer-local 'w3m-form-input-textarea-wincfg)

(defun w3m-form-input-textarea-set ()
  "Save and exit from w3m form textarea mode."
  (interactive)
  (run-hooks 'w3m-form-input-textarea-set-hook)
  (let ((input (buffer-string))
	(buffer (current-buffer))
	(hseq w3m-form-input-textarea-hseq)
	(form w3m-form-input-textarea-form)
	(point w3m-form-input-textarea-point)
	(w3mbuffer w3m-form-input-textarea-buffer)
	(wincfg w3m-form-input-textarea-wincfg))
    (when (buffer-live-p w3mbuffer)
      (or (one-window-p) (delete-window))
      (kill-buffer buffer)
      (pop-to-buffer w3mbuffer)
      (set-window-configuration wincfg)
      (when (and form point)
	(goto-char point)
	(w3m-form-put form (car (w3m-form-textarea-info)) input)
	(w3m-form-textarea-replace hseq input)))))

(defun w3m-form-input-textarea-exit ()
  "Exit from w3m form textarea mode."
  (interactive)
  (let ((buffer (current-buffer))
	(point w3m-form-input-textarea-point)
	(w3mbuffer w3m-form-input-textarea-buffer)
	(wincfg w3m-form-input-textarea-wincfg))
    (when (buffer-live-p w3mbuffer)
      (or (one-window-p) (delete-window))
      (kill-buffer buffer)
      (pop-to-buffer w3mbuffer)
      (set-window-configuration wincfg)
      (when point (goto-char point)))))

(defun w3m-form-input-textarea-mode ()
  "\\<w3m-form-input-textarea-keymap>
   Major mode for w3m form textarea.

\\[w3m-form-input-textarea-set]	Save and exit from w3m form textarea mode.
\\[w3m-form-input-textarea-exit]	Exit from w3m form textarea mode.
"
  (setq mode-name "w3m form textarea"
	major-mode 'w3m-form-input-textarea-mode)
  (use-local-map w3m-form-input-textarea-keymap)
  (run-hooks 'w3m-form-input-textarea-mode-hook))

(defun w3m-form-input-textarea (form hseq)
  (let* ((info  (w3m-form-textarea-info))
	 (value (w3m-form-get form (car info)))
	 (cur-win (selected-window))
	 (wincfg (current-window-configuration))
	 (w3mbuffer (current-buffer))
	 (point (point))
	 (size (min
		(- (window-height cur-win)
		   window-min-height 1)
		(- (window-height cur-win)
		   (max window-min-height
			(1+ w3m-form-input-textarea-buffer-lines)))))
	 (buffer (generate-new-buffer "*w3m form textarea*")))
    (condition-case nil
	(split-window cur-win (if (> size 0) size window-min-height))
      (error
       (delete-other-windows)
       (split-window cur-win (- (window-height cur-win)
				w3m-form-input-textarea-buffer-lines))))
    (select-window (next-window))
    (let ((pop-up-windows nil))
      (switch-to-buffer buffer)
      (set-buffer buffer)
      (setq w3m-form-input-textarea-form form)
      (setq w3m-form-input-textarea-hseq hseq)
      (setq w3m-form-input-textarea-buffer w3mbuffer)
      (setq w3m-form-input-textarea-point point)
      (setq w3m-form-input-textarea-wincfg wincfg)
      (if value (insert value))
      (goto-char (point-min))
      (forward-line (1- (cdr info)))
      (w3m-form-input-textarea-mode))))

;;; SELECT

(defcustom w3m-form-input-select-buffer-lines 10
  "*Buffer lines for form select buffer."
  :group 'w3m
  :type 'integer)

(defcustom w3m-form-input-select-mode-hook nil
  "*A hook called after w3m-form-input-select-mode."
  :group 'w3m
  :type 'hook)

(defcustom w3m-form-input-select-set-hook nil
  "*A Hook called before w3m-form-input-select-set."
  :group 'w3m
  :type 'hook)

(defcustom w3m-form-mouse-face 'highlight
  "*Mouse face to highlight selected value."
  :group 'w3m
  :type 'face)

(defvar w3m-form-input-select-keymap nil)
(unless w3m-form-input-select-keymap
  (setq w3m-form-input-select-keymap (make-sparse-keymap))
  (define-key w3m-form-input-select-keymap "\C-c\C-c"
    'w3m-form-input-select-set)
  (define-key w3m-form-input-select-keymap "\r"
    'w3m-form-input-select-set)
  (define-key w3m-form-input-select-keymap "\C-m"
    'w3m-form-input-select-set)
  (define-key w3m-form-input-select-keymap "\C-c\C-q"
    'w3m-form-input-select-exit)
  (define-key w3m-form-input-select-keymap "\C-c\C-k"
    'w3m-form-input-select-exit)
  (define-key w3m-form-input-select-keymap "q"
    'w3m-form-input-select-exit)
  (if (featurep 'xemacs)
      (define-key w3m-form-input-select-keymap [(button2)]
	'w3m-form-input-select-set-mouse)
    (define-key w3m-form-input-select-keymap [mouse-2]
      'w3m-form-input-select-set-mouse)))
(defvar w3m-form-input-select-buffer nil)
(defvar w3m-form-input-select-form nil)
(defvar w3m-form-input-select-name nil)
(defvar w3m-form-input-select-point nil)
(defvar w3m-form-input-select-candidates nil)
(defvar w3m-form-input-select-wincfg nil)
(make-variable-buffer-local 'w3m-form-input-select-buffer)
(make-variable-buffer-local 'w3m-form-input-select-form)
(make-variable-buffer-local 'w3m-form-input-select-name)
(make-variable-buffer-local 'w3m-form-input-select-point)
(make-variable-buffer-local 'w3m-form-input-select-candidates)
(make-variable-buffer-local 'w3m-form-input-select-wincfg)

(defun w3m-form-input-select-set-mouse (event)
  "Save and exit from w3m form select mode with mouse."
  (interactive "e")
  (mouse-set-point event)
  (w3m-form-input-select-set))

(defun w3m-form-input-select-set ()
  "Save and exit from w3m form select mode."
  (interactive)
  (run-hooks 'w3m-form-input-select-set-hook)
  (let* ((cur (get-text-property (point)
				 'w3m-form-select-value))
	 (buffer (current-buffer))
	 (name w3m-form-input-select-name)
	 (form w3m-form-input-select-form)
	 (point w3m-form-input-select-point)
	 (w3mbuffer w3m-form-input-select-buffer)
	 (wincfg w3m-form-input-select-wincfg)
	 input)
    (setcar w3m-form-input-select-candidates cur)
    (setq input w3m-form-input-select-candidates)
    (when (buffer-live-p w3mbuffer)
      (or (one-window-p) (delete-window))
      (kill-buffer buffer)
      (pop-to-buffer w3mbuffer)
      (set-window-configuration wincfg)
      (when (and form point)
	(goto-char point)
	(w3m-form-put form name input)
	(w3m-form-replace (cdr (assoc cur (cdr input))))))))

(defun w3m-form-input-select-exit ()
  "Exit from w3m form select mode."
  (interactive)
  (let* ((buffer (current-buffer))
	 (point w3m-form-input-select-point)
	 (w3mbuffer w3m-form-input-select-buffer)
	 (wincfg w3m-form-input-select-wincfg))
    (when (buffer-live-p w3mbuffer)
      (or (one-window-p) (delete-window))
      (kill-buffer buffer)
      (pop-to-buffer w3mbuffer)
      (set-window-configuration wincfg)
      (when point (goto-char point)))))

(defun w3m-form-input-select-mode ()
  "\\<w3m-form-input-select-keymap>
   Major mode for w3m form select.

\\[w3m-form-input-select-set]	Save and exit from w3m form select mode.
\\[w3m-form-input-select-exit]	Exit from w3m form select mode.
\\[w3m-form-input-select-set-mouse]	Save and exit from w3m form select mode with mouse.
"
  (setq mode-name "w3m form select"
	major-mode 'w3m-form-input-select-mode)
  (setq buffer-read-only t)
  (use-local-map w3m-form-input-select-keymap)
  (run-hooks 'w3m-form-input-select-mode-hook))

(defun w3m-form-input-select (form name)
  (let* ((value (w3m-form-get form name))
	 (cur-win (selected-window))
	 (wincfg (current-window-configuration))
	 (w3mbuffer (current-buffer))
	 (point (point))
	 (size (min
		(- (window-height cur-win)
		   window-min-height 1)
		(- (window-height cur-win)
		   (max window-min-height
			(1+ w3m-form-input-select-buffer-lines)))))
	 (buffer (generate-new-buffer "*w3m form select*"))
	 cur pos)
    (condition-case nil
	(split-window cur-win (if (> size 0) size window-min-height))
      (error
       (delete-other-windows)
       (split-window cur-win (- (window-height cur-win)
				w3m-form-input-select-buffer-lines))))
    (select-window (next-window))
    (let ((pop-up-windows nil))
      (switch-to-buffer buffer)
      (set-buffer buffer)
      (setq w3m-form-input-select-form form)
      (setq w3m-form-input-select-name name)
      (setq w3m-form-input-select-buffer w3mbuffer)
      (setq w3m-form-input-select-point point)
      (setq w3m-form-input-select-candidates value)
      (setq w3m-form-input-select-wincfg wincfg)
      (when value
	(setq cur (car value))
	(setq value (cdr value))
	(dolist (candidate value)
	  (setq pos (point))
	  (insert (cdr candidate))
	  (add-text-properties pos (point)
			       (list 'w3m-form-select-value (car candidate)
				     'mouse-face w3m-form-mouse-face))
	  (insert "\n")))
      (goto-char (point-min))
      (while (and (not (eobp))
		  (not (equal cur (get-text-property (point)
						     'w3m-form-select-value))))
	(goto-char (next-single-property-change (point)
						'w3m-form-select-value)))
      (set-buffer-modified-p nil)
      (beginning-of-line)
      (w3m-form-input-select-mode))))


;;; MAP

(defcustom w3m-form-input-map-buffer-lines 10
  "*Buffer lines for form select map buffer."
  :group 'w3m
  :type 'integer)

(defcustom w3m-form-input-map-mode-hook nil
  "*A hook called after w3m-form-input-map-mode."
  :group 'w3m
  :type 'hook)

(defcustom w3m-form-input-map-set-hook nil
  "*A Hook called before w3m-form-input-map-set."
  :group 'w3m
  :type 'hook)

(defvar w3m-form-input-map-keymap nil)
(unless w3m-form-input-map-keymap
  (setq w3m-form-input-map-keymap (make-sparse-keymap))
  (define-key w3m-form-input-map-keymap "\C-c\C-c"
    'w3m-form-input-map-set)
  (define-key w3m-form-input-map-keymap "\r"
    'w3m-form-input-map-set)
  (define-key w3m-form-input-map-keymap "\C-m"
    'w3m-form-input-map-set)
  (define-key w3m-form-input-map-keymap "\C-c\C-q"
    'w3m-form-input-map-exit)
  (define-key w3m-form-input-map-keymap "\C-c\C-k"
    'w3m-form-input-map-exit)
  (define-key w3m-form-input-map-keymap "q"
    'w3m-form-input-map-exit)
  (if (featurep 'xemacs)
      (define-key w3m-form-input-map-keymap [(button2)]
	'w3m-form-input-map-set-mouse)
    (define-key w3m-form-input-map-keymap [mouse-2]
      'w3m-form-input-map-set-mouse)))
(defvar w3m-form-input-map-buffer nil)
(defvar w3m-form-input-map-wincfg nil)
(defvar w3m-form-input-map-point nil)
(make-variable-buffer-local 'w3m-form-input-map-buffer)
(make-variable-buffer-local 'w3m-form-input-map-wincfg)
(make-variable-buffer-local 'w3m-form-input-map-point)

(defun w3m-form-input-map-set-mouse (event)
  "Save and exit from w3m form select map mode with mouse."
  (interactive "e")
  (mouse-set-point event)
  (w3m-form-input-map-set))

(defun w3m-form-input-map-set ()
  "Save and exit from w3m form select map mode."
  (interactive)
  (run-hooks 'w3m-form-input-map-set-hook)
  (let* ((map (get-text-property (point) 'w3m-form-map-value))
	 (buffer (current-buffer))
	 (w3mbuffer w3m-form-input-map-buffer)
	 (wincfg w3m-form-input-map-wincfg)
	 (point w3m-form-input-map-point))
    (when (buffer-live-p w3mbuffer)
      (or (one-window-p) (delete-window))
      (kill-buffer buffer)
      (pop-to-buffer w3mbuffer)
      (set-window-configuration wincfg)
      (when point (goto-char point))
      (w3m-goto-url (w3m-expand-url map)))))

(defun w3m-form-input-map-exit ()
  "Exit from w3m form select map mode."
  (interactive)
  (let* ((buffer (current-buffer))
	 (w3mbuffer w3m-form-input-map-buffer)
	 (wincfg w3m-form-input-map-wincfg)
	 (point w3m-form-input-map-point))
    (when (buffer-live-p w3mbuffer)
      (or (one-window-p) (delete-window))
      (kill-buffer buffer)
      (pop-to-buffer w3mbuffer)
      (set-window-configuration wincfg)
      (when point (goto-char point)))))

(defun w3m-form-input-map-mode ()
  "\\<w3m-form-input-map-keymap>
   Major mode for w3m map select.

\\[w3m-form-input-map-set]	Save and exit from w3m form select map mode.
\\[w3m-form-input-map-exit]	Exit from w3m form select map mode.
\\[w3m-form-input-map-set-mouse]	Save and exit from w3m form select map mode with mouse.
"
  (setq mode-name "w3m map select"
	major-mode 'w3m-form-input-map-mode)
  (setq buffer-read-only t)
  (use-local-map w3m-form-input-map-keymap)
  (run-hooks 'w3m-form-input-map-mode-hook))

(defun w3m-form-input-map (form name)
  (let* ((value (w3m-form-get form name))
	 (cur-win (selected-window))
	 (wincfg (current-window-configuration))
	 (w3mbuffer (current-buffer))
	 (point (point))
	 (size (min
		(- (window-height cur-win)
		   window-min-height 1)
		(- (window-height cur-win)
		   (max window-min-height
			(1+ w3m-form-input-map-buffer-lines)))))
	 (buffer (generate-new-buffer "*w3m map select*"))
	 pos)
    (condition-case nil
	(split-window cur-win (if (> size 0) size window-min-height))
      (error
       (delete-other-windows)
       (split-window cur-win (- (window-height cur-win)
				w3m-form-input-map-buffer-lines))))
    (select-window (next-window))
    (let ((pop-up-windows nil))
      (switch-to-buffer buffer)
      (set-buffer buffer)
      (setq w3m-form-input-map-buffer w3mbuffer)
      (setq w3m-form-input-map-wincfg wincfg)
      (setq w3m-form-input-map-point point)
      (when value
	(dolist (candidate value)
	  (setq pos (point))
	  (insert (cdr candidate))
	  (add-text-properties pos (point)
			       (list 'w3m-form-map-value (car candidate)
				     'mouse-face w3m-form-mouse-face))
	  (insert "\n")))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (beginning-of-line)
      (w3m-form-input-map-mode))))

;;;

(defun w3m-form-submit (form &optional name value)
  (when (and name (not (zerop (length name))))
    (w3m-form-put form name value))
  (let ((url (or (w3m-form-action form)
		 (if (string-match "\\?" w3m-current-url)
		     (substring w3m-current-url 0 (match-beginning 0))
		   w3m-current-url))))
    (cond ((eq 'get (w3m-form-method form))
	   (w3m-goto-url
	    (concat url "?" (w3m-form-make-form-data form 'urlencode))))
	  ((eq 'post (w3m-form-method form))
	   (w3m-goto-url url 'reload nil
			 (w3m-form-make-form-data form)
			 w3m-current-url))
	  (t
	   (w3m-message "This form's method has not been supported: %s"
			(let (print-level print-length)
			  (prin1-to-string (w3m-form-method form))))))))

(defsubst w3m-form-real-reset (form sexp)
  (and (eq 'w3m-form-input (car sexp))
       (eq form (nth 1 sexp))
       (w3m-form-put form (nth 2 sexp) (nth 6 sexp))
       (w3m-form-replace (nth 6 sexp))))

(defun w3m-form-reset (form)
  (save-excursion
    (let (pos prop)
      (when (setq prop (get-text-property
			(goto-char (point-min))
			'w3m-action))
	(goto-char (or (w3m-form-real-reset form prop)
		       (next-single-property-change pos 'w3m-action))))
      (while (setq pos (next-single-property-change (point) 'w3m-action))
	(goto-char pos)
	(goto-char (or (w3m-form-real-reset form
					    (get-text-property pos
							       'w3m-action))
		       (next-single-property-change pos 'w3m-action)))))))


(provide 'w3m-form)

;;; w3m-form.el ends here

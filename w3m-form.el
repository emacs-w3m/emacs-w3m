;;; w3m-form.el --- Stuffs to handle <form> tag

;; Copyright (C) 2001, 2002, 2003 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

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

(require 'w3m-util)
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

(defun w3m-form-new (method action &optional baseurl charlst enctype)
  "Return new form object."
  (vector 'w3m-form-object
	  (if (stringp method)
	      (intern method)
	    method)
	  (and action
	       (w3m-expand-url action baseurl))
	  charlst
	  (or enctype 'urlencoded)
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
(defmacro w3m-form-enctype (form)
  (` (aref (, form) 4)))
(defmacro w3m-form-plist (form)
  (` (aref (, form) 5)))
(defsubst w3m-form-put-property (form name property value)
  (aset form 5
	(plist-put (w3m-form-plist form)
		   (setq name (intern name))
		   (plist-put (plist-get (w3m-form-plist form) name)
			      property value)))
  value)
(defmacro w3m-form-get-property (form name property)
  (` (plist-get (plist-get (w3m-form-plist (, form))
			   (intern (, name)))
		(, property))))
(defmacro w3m-form-put (form name value)
  (` (w3m-form-put-property (, form) (, name) :value (, value))))
(defmacro w3m-form-get (form name)
  (` (w3m-form-get-property (, form) (, name) :value)))

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

(defun w3m-form-make-form-data (form)
  (let ((plist (w3m-form-plist form))
	(coding (w3m-form-charlst form))
	buf)
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
	    (value (plist-get (cadr plist) :value)))
	(cond
	 ((and (consp value)
	       (eq (car value) 'file))
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
      (if (eq (w3m-form-enctype form) 'multipart)
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

(defun w3m-form-resume (forms)
  "Resume content of all forms in the current buffer using FORMS."
  (if (eq (car forms) t)
      (setq forms (cdr forms)))
  (save-excursion
    (goto-char (point-min))
    (let (fid type name form cform textareas)
      (while (w3m-form-goto-next-field)
	(setq fid (get-text-property (point) 'w3m-form-field-id))
	(when (and fid
		   (string-match
		    "fid=\\([^/]+\\)/type=\\([^/]+\\)/name=\\(.*\\)$"
		    fid))
	  (setq form (nth (string-to-number (match-string 1 fid))
			  forms)
		cform (nth (string-to-number (match-string 1 fid))
			   w3m-current-forms)
		type (match-string 2 fid)
		name (match-string 3 fid))
	  (cond
	   ((or (string= type "submit")
		(string= type "image"))
	    ;; Remove status to support forms containing multiple
	    ;; submit buttons.
	    (w3m-form-put cform name nil))
	   ((or (string= type "reset")
		(string= type "hidden")
		;; Do nothing.
		))
	   ((string= type "password")
	    (w3m-form-replace (w3m-form-get form name)
			      'invisible)
	    (unless (eq form cform)
	      (w3m-form-put cform name (w3m-form-get form name))))
	   ((string= type "radio")
	    (let ((value (w3m-form-get form name)))
	      (when value
		(w3m-form-replace
		 (if (string= value (nth 3 (w3m-action (point))))
		     "*" " ")))
	      (unless (eq form cform)
		(w3m-form-put cform name value))))
	   ((string= type "checkbox")
	    (let ((value (w3m-form-get form name)))
	      (when value
		(w3m-form-replace
		 (if (member (nth 3 (w3m-action (point))) value)
		     "*" " ")))
	      (unless (eq form cform)
		(w3m-form-put cform name value))))
	   ((string= type "select")
	    (let ((selects (w3m-form-get form name)))
	      (when (car selects)
		(w3m-form-replace (cdr (assoc (car selects) (cdr selects)))))
	      (unless (eq form cform)
		(w3m-form-put cform name selects))))
	   ((string= type "textarea")
	    (let ((hseq (nth 2 (w3m-action (point))))
		  (value (w3m-form-get form name)))
	      (when (> hseq 0)
		(setq textareas (cons (cons hseq value) textareas)))
	      (unless (eq form cform)
		(w3m-form-put cform name value))))
	   ((string= type "file")
	    (let ((value (w3m-form-get form name)))
	      (when (and value
			 (consp value))
		(w3m-form-replace (cdr value)))
	      (unless (eq form cform)
		(w3m-form-put cform name value))))
	   (t
	    (let ((value (w3m-form-get form name)))
	      (when (stringp value)
		(w3m-form-replace value))
	      (unless (eq form cform)
		(w3m-form-put cform name value)))))))
      (dolist (textarea textareas)
	(when (cdr textarea)
	  (w3m-form-textarea-replace (car textarea) (cdr textarea)))))))

;;;###autoload
(defun w3m-fontify-forms ()
  "Process half-dumped data and fontify forms in this buffer."
  ;; If `w3m-current-forms' is resumed from history, reuse it.
  (w3m-form-parse-and-fontify
   (when (eq t (car w3m-current-forms))
     (setq w3m-current-forms (cdr w3m-current-forms)))))

(eval-and-compile
  (unless (fboundp 'w3m-form-make-button)
    (defun w3m-form-make-button (start end properties)
      "Make button on the region from START to END."
      (add-text-properties start end (append '(face w3m-form-face)
					     properties)))))

;;; w3mmee
;;
(if (fboundp 'char-to-int)
    (defalias 'w3m-char-to-int 'char-to-int)
  (defalias 'w3m-char-to-int 'identity))

(defmacro w3m-form-mee-attr-unquote (x)
  "Unquote form attribute of w3mmee."
  '(let (attr)
     (when (eq (car x) ?T)
       (setq x (cdr x))
       (while (and x (not (eq (w3m-char-to-int (car x)) 0)))
	 (setq attr (concat attr (char-to-string (car x))))
	 (setq x (cdr x))))
     attr))

(if (fboundp 'string-to-list)
    (defalias 'w3m-string-to-char-list 'string-to-list)
  (defun w3m-string-to-char-list (str)
    (mapcar 'identity str)))

(if (fboundp 'int-to-char)
    (defalias 'w3m-int-to-char 'int-to-char)
  (defalias 'w3m-int-to-char 'identity))

(defun w3m-form-mee-new (x)
  "Decode form information of w3mmee."
  (setq x (w3m-string-to-char-list (w3m-url-decode-string x)))
  (let (method enctype action charset target name)
    (setq method (case (/ (w3m-char-to-int (car x)) 16)
		   (0 "get")
		   (1 "post")
		   (2 "internal")
		   (3 "head"))
	  enctype (case (% (w3m-char-to-int (car x)) 16)
		    (0 'urlencoded)
		    (1 'multipart)))
    (setq x (cdr x))
    (setq action (w3m-form-mee-attr-unquote x))
    (setq x (cdr x))
    (if (member "lang=many" w3m-compile-options)
	(setq charset (w3m-form-mee-attr-unquote x))
      (setq charset (case (car x)
		      (?e "euc-jp")
		      (?s "shift-jis")
		      (?n "iso-2022-7bit"))))
    (setq x (cdr x))
    (setq target (w3m-form-mee-attr-unquote x)) ; not used.
    (setq x (cdr x))
    (setq name (w3m-form-mee-attr-unquote x))   ; not used.
    (w3m-form-new method action nil (and charset (list charset)) enctype)))

(defun w3m-form-mee-select-value (value)
  "Decode select form information of w3mmee."
  (let ((clist (w3m-string-to-char-list (w3m-url-decode-string value)))
	label val s selected candidates)
    (while clist
      (setq s (eq (car clist) (w3m-int-to-char 1))
	    label nil
	    val nil)
      (setq clist (cdr clist))
      (while (not (eq (car clist) (w3m-int-to-char 0)))
	(setq label (concat label (char-to-string (car clist))))
	(setq clist (cdr clist)))
      (if label
	  (setq label (decode-coding-string label w3m-output-coding-system)))
      (setq clist (cdr clist))
      (while (not (eq (car clist) (w3m-int-to-char 0)))
	(setq val (concat val (char-to-string (car clist))))
	(setq clist (cdr clist)))
      (if val
	  (setq val (decode-coding-string val w3m-output-coding-system)))
      (if s (setq selected val))
      (push (cons val label) candidates)
      (setq clist (cdr clist)))
    (cons selected (nreverse candidates))))

(defun w3m-form-parse-and-fontify (&optional reuse-forms)
  "Parse forms of the half-dumped data in this buffer and fontify them.
Result form structure is saved to the local variable `w3m-current-forms'.
If optional REUSE-FORMS is non-nil, reuse it as `w3m-current-form'."
  (let ((case-fold-search t)
	tag start end internal-start textareas selects forms maps mapval
	form)
    (goto-char (point-min))
    (while (re-search-forward (if (eq w3m-type 'w3mmee)
				  (w3m-tag-regexp-of
				   "_f" "map" "img_alt" "input_alt"
				   "/input_alt")
				(w3m-tag-regexp-of
				   "form_int" "map" "img_alt" "input_alt"
				   "/input_alt"))
			      nil t)
      (setq tag (downcase (match-string 1)))
      (goto-char (match-end 1))
      (setq start (match-end 0))
      (cond
       ((string= tag (if (eq w3m-type 'w3mmee) "_f" "form_int"))
	(if (eq w3m-type 'w3mmee)
	    (w3m-parse-attributes (_x)
	      (setq forms (nconc forms (list (w3m-form-mee-new _x)))))
	  (w3m-parse-attributes (action (method :case-ignore)
					(fid :integer)
					(accept-charset :case-ignore)
					(enctype :case-ignore)
					(charset :case-ignore))
	    (setq forms
		  (cons
		   (cons
		    fid
		    (w3m-form-new
		     (or method "get")
		     (or action (and w3m-current-url
				     (string-match w3m-url-components-regexp
						   w3m-current-url)
				     (substring w3m-current-url 0
						(or (match-beginning 6)
						    (match-beginning 8)))))
		     nil
		     (if accept-charset
			 (setq accept-charset
			       (split-string accept-charset ",")))
		     (if enctype
			 (intern enctype)
		       'urlencoded)))
		   forms)))))
       ((string= tag "map")
	(let (candidates)
	  (w3m-parse-attributes (name)
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
	    (unless maps (setq maps (w3m-form-new "map" ".")))
	    (when candidates
	      (w3m-form-put maps
			    name
			    (nreverse candidates))))))
       ((string= tag "img_alt")
	(w3m-parse-attributes (usemap)
	  (re-search-forward (w3m-tag-regexp-of "/img_alt") nil t)
	  (when (or usemap mapval)
	    (unless maps (setq maps (w3m-form-new "map" ".")))
	    (unless usemap (setq usemap mapval))
	    (when mapval (setq mapval nil))
	    (add-text-properties
	     start (match-beginning 0)
	     `(face w3m-form-face
		    w3m-action (w3m-form-input-map ,maps ,usemap))))))
       ((string= tag "/input_alt")
	(replace-match ""))
       ((string= tag "input_alt")
	(w3m-parse-attributes ((fid :integer)
			       (type :case-ignore)
			       (width :integer)
			       (maxlength :integer)
			       (hseq :integer)
			       (selectnumber :integer) ; select
			       (textareanumber :integer) ; textarea
			       (size :integer) ; textarea
			       (rows :integer) ; textarea
			       (top_mergin :integer) ; textarea
			       (checked :bool) ; checkbox, radio
			       no_effect ; map
			       name value)
	  (save-excursion
	    (search-forward "</input_alt>")
	    (setq end (match-beginning 0)))
	  (let ((abs-hseq (or (and (null hseq) 0) (abs hseq))))
	    (setq w3m-max-anchor-sequence
		  (max abs-hseq w3m-max-anchor-sequence))
	    (if (eq w3m-type 'w3mmee)
		(setq form (nth fid forms))
	      (setq form (cdr (assq fid forms))))
	    (when form
	      (cond
	       ((and (string= type "hidden")
		     (string= name "link"))
		(setq mapval value))
	       ((or (string= type "submit")
		    (string= type "image"))
		(unless (string= no_effect "true")
		  (w3m-form-make-button
		   start end
		   `(w3m-form-field-id
		     ,(format "fid=%d/type=%s/name=%s" fid type name)
		     w3m-action (w3m-form-submit ,form ,name ,value)
		     w3m-submit (w3m-form-submit ,form ,name
						 (w3m-form-get ,form ,name))
		     w3m-anchor-sequence ,abs-hseq))))
	       ((string= type "reset")
		(w3m-form-make-button
		 start end
		 `(w3m-form-field-id
		   ,(format "fid=%d/type=%s/name=%s" fid type name)
		   w3m-action (w3m-form-reset ,form)
		   w3m-anchor-sequence ,abs-hseq)))
	       ((string= type "textarea")
		(if (eq w3m-type 'w3mmee)
		    (w3m-form-put form name
				  (decode-coding-string
				   (w3m-url-decode-string value)
				   w3m-output-coding-system))
		  (setq textareas (cons (list textareanumber form name)
					textareas)))
		(add-text-properties
		 start end
		 `(w3m-form-field-id
		   ,(format "fid=%d/type=%s/name=%s" fid type name)
		   face w3m-form-face
		   w3m-action (w3m-form-input-textarea ,form ,hseq)
		   w3m-submit (w3m-form-submit ,form ,name
					       (w3m-form-get ,form ,name))
		   w3m-form-hseq ,hseq
		   w3m-anchor-sequence ,abs-hseq))
		(when (> hseq 0)
		  (add-text-properties start end `(w3m-form-name ,name))))
	       ((string= type "select")
		(if (eq w3m-type 'w3mmee)
		    (w3m-form-put form name
				  (w3m-form-mee-select-value value))
		  (setq selects (cons (list selectnumber form name)
				      selects)))
		(add-text-properties
		 start end
		 `(w3m-form-field-id
		   ,(format "fid=%d/type=%s/name=%s" fid type name)
		   face w3m-form-face
		   w3m-action (w3m-form-input-select ,form ,name)
		   w3m-submit (w3m-form-submit ,form ,name
					       (w3m-form-get ,form ,name))
		   w3m-anchor-sequence ,abs-hseq)))
	       ((string= type "password")
		(add-text-properties
		 start end
		 `(w3m-form-field-id
		   ,(format "fid=%d/type=%s/name=%s" fid type name)
		   face w3m-form-face
		   w3m-action (w3m-form-input-password ,form ,name)
		   w3m-submit (w3m-form-submit ,form ,name
					       (w3m-form-get ,form ,name))
		   w3m-anchor-sequence ,abs-hseq)))
	       ((string= type "checkbox")
		(let ((cvalue (w3m-form-get form name)))
		  (w3m-form-put form name
				(if checked
				    (cons value cvalue)
				  cvalue)))
		(add-text-properties
		 start end
		 `(w3m-form-field-id
		   ,(format "fid=%d/type=%s/name=%s" fid type name)
		   face w3m-form-face
		   w3m-action (w3m-form-input-checkbox ,form ,name ,value)
		   w3m-submit (w3m-form-submit ,form ,name
					       (w3m-form-get ,form ,name))
		   w3m-anchor-sequence ,abs-hseq)))
	       ((string= type "radio")
		;; Radio button input, one name has one value
		(w3m-form-put form name
			      (if checked value
				(w3m-form-get form name)))
		(add-text-properties
		 start end
		 `(w3m-form-field-id
		   ,(format "fid=%d/type=%s/name=%s" fid type name)
		   face w3m-form-face
		   w3m-action (w3m-form-input-radio ,form ,name ,value)
		   w3m-submit (w3m-form-submit ,form ,name
					       (w3m-form-get ,form ,name))
		   w3m-anchor-sequence ,abs-hseq)))
	       ((string= type "file")
		(add-text-properties
		 start end
		 `(w3m-form-field-id
		   ,(format "fid=%d/type=%s/name=%s" fid type name)
		   face w3m-form-face
		   w3m-action (w3m-form-input-file ,form ,name ,value)
		   w3m-submit (w3m-form-submit ,form ,name
					       (w3m-form-get ,form ,name))
		   w3m-anchor-sequence ,abs-hseq)))
	       (t
		(w3m-form-put form
			      name
			      (or value (w3m-form-get form name)))
		(add-text-properties
		 start end
		 `(w3m-form-field-id
		   ,(format "fid=%d/type=%s/name=%s" fid type name)
		   face w3m-form-face
		   w3m-action (w3m-form-input ,form ,name ,type
					      ,width ,maxlength ,value)
		   w3m-submit (w3m-form-submit ,form ,name
					       (w3m-form-get ,form ,name))
		   w3m-anchor-sequence ,abs-hseq))))))))))
    ;; Process <internal> tag.
    (when (search-forward "<internal>" nil t)
      (setq internal-start (match-beginning 0))
      (while (and (null reuse-forms)
		  (re-search-forward "<\\([a-z]+\\)_int" nil t))
	(cond
	 ((string= (match-string 1) "select")
	  (w3m-parse-attributes ((selectnumber :integer))
	    (let ((selectinfo (cdr (assq selectnumber selects)))
		  current candidates)
	      (when selectinfo
		;; Parse FORM SELECT fields until </SELECT> (or </FORM>)
		(while (and (re-search-forward
			     (w3m-tag-regexp-of "option_int" "/select_int")
			     nil t)
			    (not (char-equal (char-after (match-beginning 1))
					     ?/)))
		  ;; <option_int> is found
		  (goto-char (match-end 1))
		  (w3m-parse-attributes ((value :decode-entity)
					 (label :decode-entity)
					 (selected :bool))
		    (push (cons value label) candidates)
		    (if selected (setq current value))
		    (skip-chars-forward ">\n")))
		(setq candidates (nreverse candidates))
		(w3m-form-put (nth 0 selectinfo)
			      (nth 1 selectinfo)
			      (cons (or current ; current value
					(caar candidates))
				    candidates))))))
	 ((string= (match-string 1) "textarea")
	  (w3m-parse-attributes ((textareanumber :integer))
	    (forward-char 1) ; skip newline character.
	    (let ((textareainfo (cdr (assq textareanumber textareas)))
		  end)
	      (when textareainfo
		(setq start (point))
		(skip-chars-forward "^<")
		(w3m-form-put (nth 0 textareainfo)
			      (nth 1 textareainfo)
			      (w3m-decode-entities-string
			       (buffer-substring start (point))))))))))
      (when (search-forward "</internal>" nil t)
	(delete-region internal-start (match-end 0))))
    (setq w3m-current-forms (if (eq w3m-type 'w3mmee)
				forms
			      (mapcar 'cdr
				      (sort forms (lambda (x y)
						    (< (car x)(car y)))))))
    (w3m-form-resume (or reuse-forms w3m-current-forms))))

(defun w3m-form-replace (string &optional invisible)
  (let* ((start (text-property-any (point-min) (point-max)
				   'w3m-action (w3m-action (point))))
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
				(w3m-truncate-string (or string "")
						     width) "\n")
			       "")))
	    (make-string (max (- width (string-width string)) 0) ?\ ))
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
  (let ((s (get-text-property (point) 'w3m-form-hseq))
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
      (cons (get-text-property (point) 'w3m-form-name) lines))))

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
  (define-key w3m-form-input-select-keymap "h" 'backward-char)
  (define-key w3m-form-input-select-keymap "j" 'next-line)
  (define-key w3m-form-input-select-keymap "k" 'previous-line)
  (define-key w3m-form-input-select-keymap "l" 'forward-char)
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

\\[w3m-form-input-select-set]\
	Save and exit from w3m form select mode.
\\[w3m-form-input-select-exit]\
	Exit from w3m form select mode.
\\[w3m-form-input-select-set-mouse]\
	Save and exit from w3m form select mode with mouse.
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
	  (insert (if (zerop (length (cdr candidate)))
		      " " ; "" -> " "
		    (cdr candidate)))
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
  (define-key w3m-form-input-map-keymap "h" 'backward-char)
  (define-key w3m-form-input-map-keymap "j" 'next-line)
  (define-key w3m-form-input-map-keymap "k" 'previous-line)
  (define-key w3m-form-input-map-keymap "l" 'forward-char)
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

\\[w3m-form-input-map-set]\
	Save and exit from w3m form select map mode.
\\[w3m-form-input-map-exit]\
	Exit from w3m form select map mode.
\\[w3m-form-input-map-set-mouse]\
	Save and exit from w3m form select map mode with mouse.
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
	  (insert (if (zerop (length (cdr candidate)))
		      (car candidate)
		    (cdr candidate)))
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
  (let* ((orig-url w3m-current-url)
	 (url (or (w3m-form-action form)
		  (if (string-match "\\?" w3m-current-url)
		      (substring w3m-current-url 0 (match-beginning 0))
		    w3m-current-url))))
    (cond ((and (not (string= url orig-url))
		(string-match "^https://" orig-url)
		(string-match "^http://" url)
		(not (y-or-n-p (format "Send POST data to '%s'?" url))))
	   (ding))
	  ((eq 'get (w3m-form-method form))
	   (w3m-goto-url
	    (concat url "?" (w3m-form-make-form-data form))))
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
      (when (setq prop (w3m-action (goto-char (point-min))))
	(goto-char (or (w3m-form-real-reset form prop)
		       (next-single-property-change pos 'w3m-action))))
      (while (setq pos (next-single-property-change (point) 'w3m-action))
	(goto-char pos)
	(goto-char (or (w3m-form-real-reset form (w3m-action pos))
		       (next-single-property-change pos 'w3m-action)))))))


(provide 'w3m-form)

;;; w3m-form.el ends here

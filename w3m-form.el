;;; -*- mode: Emacs-Lisp; coding: iso-2022-7bit -*-

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;; Keywords: w3m, WWW, hypermedia

;; w3m-form.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2 of the License,
;; or (at your option) any later version.

;; w3m-form.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with w3m.el; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA


;;; Commentary:

;; This file contains the stuffs to add ability of processing <form>
;; tag with w3m.el.  For more detail about w3m.el, see:
;;
;;    http://namazu.org/~tsuchiya/emacs-w3m/


;;; Code:

(require 'w3m)

(defface w3m-form-face
  '((((class color) (background light)) (:foreground "cyan" :underline t))
    (((class color) (background dark)) (:foreground "red" :underline t))
    (t (:underline t)))
  "*Face to fontify forms."
  :group 'w3m-face)

(defvar w3m-current-forms nil "Forms of this buffer.")
(make-variable-buffer-local 'w3m-current-forms)


;;; w3m-form structure:

(defun w3m-form-new (method action &optional baseurl)
  "Return new form object."
  (vector 'w3m-form-object
	  (if (stringp method)
	      (intern method)
	    method)
	  (if baseurl
	      (w3m-expand-url action baseurl)
	    action)
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
(defmacro w3m-form-plist (form)
  (` (aref (, form) 3)))
(defmacro w3m-form-put (form name value)
  (let ((tempvar (make-symbol "formobj")))
    (` (let (((, tempvar) (, form)))
	 (aset (, tempvar) 3
	       (plist-put (w3m-form-plist (, tempvar))
			  (intern (, name)) (, value)))))))
(defmacro w3m-form-get (form name)
  (` (plist-get (w3m-form-plist (, form)) (intern (, name)))))

(defun w3m-form-make-get-string (form)
  (when (eq 'get (w3m-form-method form))
    (let ((plist (w3m-form-plist form))
	  (buf) str)
      (while plist
	(setq buf (cons
		   (format "%s=%s"
			   (w3m-url-encode-string (symbol-name (car plist)))
			   (if (listp (setq str (nth 1 plist)))
			       (w3m-url-encode-string (car str))
			     (w3m-url-encode-string str)))
		   buf)
	      plist (nthcdr 2 plist)))
      (if buf
	  (format "%s?%s"
		  (w3m-form-action form)
		  (mapconcat (function identity) buf "&"))
	(w3m-form-action form)))))

;;;###autoload
(defun w3m-form-parse-region (start end)
  "Parse HTML data in this buffer and return form objects."
  (save-restriction
    (narrow-to-region start end)
    (let ((case-fold-search t)
	  forms)
      (goto-char (point-min))
      (while (re-search-forward "<\\(\\(form\\)\\|\\(input\\)\\|select\\)[ \t\r\f\n]+" nil t)
	(cond
	 ((match-string 2)
	  ;; When <FORM> is found.
	  (w3m-parse-attributes (action (method :case-ignore))
	    (setq forms
		  (cons (w3m-form-new (or method "get")
				      (or action w3m-current-url)
				      w3m-current-url)
			forms))))
	 ((match-string 3)
	  ;; When <INPUT> is found.
	  (w3m-parse-attributes (name value (type :case-ignore))
	    (when name
	      (w3m-form-put (car forms)
			    name
			    (cons value (w3m-form-get (car forms) name))))))
	 ;; When <SELECT> is found.
	 (t
	  ;; FIXME: この部分では、更に <OPTION> タグを処理して、後から
	  ;; 利用できるように値のリストを作成し、保存しておく必要があ
	  ;; る。しかし、これを実装するのは、まっとうな HTML parser を
	  ;; 実装するのに等しい労力が必要であるので、今回は手抜きして
	  ;; おく。
	  )))
      (set (make-local-variable 'w3m-current-forms) (nreverse forms)))))

;;;###autoload
(defun w3m-fontify-forms ()
  "Process half-dumped data in this buffer and fontify <input_alt> tags."
  (goto-char (point-min))
  (while (search-forward "<input_alt " nil t)
    (let (start)
      (setq start (match-beginning 0))
      (goto-char (match-end 0))
      (w3m-parse-attributes ((fid :integer)
			     (type :case-ignore)
			     (width :integer)
			     (maxlength :integer)
			     name value)
	(search-forward "</input_alt>")
	(goto-char (match-end 0))
	(let ((form (nth fid w3m-current-forms)))
	  (when form
	    (cond
	     ((string= type "submit")
	      (add-text-properties start (point)
				   (list 'face 'w3m-form-face
					 'w3m-action
					 `(w3m-form-submit ,form)
					 'w3m-cursor-anchor
					 `(w3m-form-submit ,form))))
	     ((string= type "reset")
	      (add-text-properties start (point)
				   (list 'face 'w3m-form-face
					 'w3m-action
					 `(w3m-form-reset ,form)
					 'w3m-cursor-anchor
					 `(w3m-form-submit ,form))))
	     (t ;; input button.
	      (add-text-properties start (point)
				   (list 'face 'w3m-form-face
					 'w3m-action
					 `(w3m-form-input ,form
							  ,name
							  ,type
							  ,width
							  ,maxlength
							  ,value)
					 'w3m-cursor-anchor
					 `(w3m-form-input ,form
							  ,name
							  ,type
							  ,width
							  ,maxlength
							  ,value))))
	     (w3m-form-put form name value))))))))

(defun w3m-form-replace (string)
  (let* ((start (text-property-any
		 (point-min)
		 (point-max)
		 'w3m-action
		 (get-text-property (point) 'w3m-action)))
	 (width (string-width
		 (buffer-substring
		  start
		  (next-single-property-change start 'w3m-action))))
	 (prop (text-properties-at start))
	 (buffer-read-only))
    (goto-char start)
    (insert (setq string (truncate-string string width))
	    (make-string (- width (string-width string)) ?\ ))
    (delete-region (point)
		   (next-single-property-change (point) 'w3m-action))
    (add-text-properties start (point) prop)
    (point)))

;;; FIXME: 本当は type の値に合わせて、適切な値のみを受け付けるように
;;; チェックしたり、入力方法を変えたりするような実装が必要。
(defun w3m-form-input (form name type width maxlength value)
  (save-excursion
    (let ((input (read-from-minibuffer
		  (concat (upcase type) ":")
		  (w3m-form-get form name))))
      (w3m-form-put form name input)
      (w3m-form-replace input))))

(defun w3m-form-submit (form)
  (let ((url (w3m-form-make-get-string form)))
    (if url
	(w3m-goto-url url)
      (w3m-message "This form's method has not been supported: %s"
		   (prin1-to-string (w3m-form-method form))))))

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
	(goto-char (or (w3m-form-real-reset form (get-text-property pos 'w3m-action))
		       (next-single-property-change pos 'w3m-action)))))))


(provide 'w3m-form)
;;; w3m-form.el ends here.

;;; octet.el --- An octet stream viewer.

;; Copyright (C) 2000-2002 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Created: 2000/05/19
;; Keywords: octet-stream, broken document

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Display application/octet-stream inline on the emacs buffer.
;;
;; This program requires:
;;
;; emacs-w3m for HTML rendereing.
;; (http://emacs-w3m.namazu.org/)
;; Mule-UCS  for UTF-8 decoding.
;; (ftp://ftp.m17n.org/pub/mule/Mule-UCS/)
;; wvHtml for MS Word document.
;; (http://www.wvware.com/)
;; xlHtml for MS Excel document.
;; (http://www.xlhtml.org/)
;; pptHtml for MS PowerPoint document.
;; (http://www.xlhtml.org/)
;; gunzip for decoding gzipped file.

;; Put follwing line in your setting file:
;;
;; (require 'octet)
;;
;; To display octet data file, execute following command.
;; 
;; M-x octet-find-file
;;
;; If you use SEMI, put following lines in your setting file:
;;
;; (require 'octet)
;; (octet-mime-setup)
;;
;; Then you can toggle displaying application/octet-stream messages.

;;; History:
;; 
;; This file is created in 2000/05/19.
;; All part was rewrote in 2002/01/28.
;; Added to emacs-w3m repository in 2002/01/29.

;;; Code:

(require 'pces)    ; as-binary-process
(require 'mime)    ; SEMI

(defvar octet-temp-directory "/tmp"
  "A directory to create temporal files.")

(defvar octet-html-render-function 'w3m-region
  "A function for HTML rendering.")

(defvar octet-suffix-type-alist
  '(("xls"  . msexcel)
    ("ppt"  . msppt)
    ("doc"  . msword)
    ("gz"   . gzip)
    ("html" . html)
    ("txt"  . text))
  "Alist of suffix-to-octet-type.")

(defvar octet-type-filter-alist
  `((msexcel octet-filter-call1       "xlhtml" ("-te")  html-u8)
    (msppt   octet-filter-call1       "ppthtml" nil     html-u8)
    (msword  octet-filter-call2       "wvHtml"  nil     html-u8)
    (html    octet-render-html        nil       nil     nil)
    (html-u8 octet-decode-u8-text     nil       nil     html)
    (gzip    octet-filter-call1       "gunzip"  ("-c")  text) ; should guess.
    (text    octet-decode-text        nil       nil     nil))
  "Alist of type-to-filter-program.
Each element should have the form like:
\(TYPE FUNCTION FILTER_PROGRAM ARGUMENT_LIST NEW-TYPE\)")

(defun octet-render-html (&rest args)
  (funcall octet-html-render-function (point-min) (point-max))
  0)

(defun octet-decode-text (&rest args)
  (let ((string (buffer-string)))
    (erase-buffer)
    (set-buffer-multibyte t)
    (insert (decode-coding-string string 'undecided)))
  0)

(defun octet-decode-u8-text (&rest args)
  (let ((string (buffer-string)))
    (erase-buffer)
    (set-buffer-multibyte t)
    (insert (decode-coding-string string 'utf-8)))
  0)

(defun octet-filter-call2 (filter &optional args)
  "Call external octed filter with two arguments (infile, outfile).
Current buffer content is replaced.
Returns t if succeed."
  (let ((infile (make-temp-name "octet"))
	(outfile (make-temp-name "octet"))
	(last-dir default-directory)
	result)
    (cd octet-temp-directory)
    (write-region-as-binary (point-min) (point-max) infile nil 'no-msg)
    (unwind-protect
	(progn
	  (setq result (apply 'call-process filter nil nil nil
			      (append args (list infile outfile))))
	  (when (and (numberp result)
		     (zerop result))
	    (erase-buffer)
	    (insert-file-contents-as-binary outfile))
	  0)
      (if (file-exists-p infile) (delete-file infile))
      (if (file-exists-p outfile) (delete-file outfile))
      (cd last-dir))))

(defun octet-filter-call1 (filter &optional args)
  "Call external octed filter with two arguments (infile) and obtain stdout.
Current buffer content is replaced.
Returns t if succeed."
  (let ((infile (make-temp-name "octet"))
	(last-dir default-directory)
	result)
    (cd octet-temp-directory)
    (write-region-as-binary (point-min) (point-max) infile nil 'no-msg)
    (unwind-protect
	(progn
	  (erase-buffer)
	  (setq result (apply 'call-process filter nil t nil
			      (append args (list infile))))
	  (if (numberp result) result 1))
      (if (file-exists-p infile) (delete-file infile))
      (cd last-dir))))

(defun octet-guess-type-from-name (name)
  (when (string-match "\\.\\([a-z]+\\)$" name)
    (or (cdr (assoc (match-string 1 name)
		    octet-suffix-type-alist))
	'text)))

(defun octet-filter-buffer (type)
  "Call a filter function in `octet-type-filter-alist'.
TYPE is the symbol of type.
Returns NEW-TYPE."
  (let ((elem (assq type octet-type-filter-alist)))
    (if (zerop (apply (nth 1 elem) (list (nth 2 elem) (nth 3 elem))))
	(nth 4 elem))))

;;;###autoload
(defun octet-buffer (&optional name)
  "View octet-stream content according to `octet-type-filter-alist'.
NAME is the filename."
  (interactive "r")
  (let (type result)
    (setq type (if (or name buffer-file-name)
		   (octet-guess-type-from-name (or name buffer-file-name))
		 (cdr (assoc (completing-read "Suffix Type:"
					      (mapcar
					       (lambda (pair)
						 (list (car pair)))
					       octet-suffix-type-alist)
					      nil 'require-match)
			     octet-suffix-type-alist))))
    (while (setq type (octet-filter-buffer type)))))

;;;###autoload
(defun octet-find-file (file)
  "Find FILE with octet-stream decoding."
  (interactive "fFilename: ")
  (as-binary-input-file	(find-file file))
  (unwind-protect
      (octet-buffer)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (auto-save-mode -1)
    (setq buffer-read-only t
	  truncate-lines t)))

;;;
;; Functions for SEMI.
;;
(defvar mime-view-octet-hook nil)

;; From EMIKO.
(defvar mime-magic-type-alist
  '(("^\377\330\377[\340\356]..JFIF"	image jpeg)
    ("^\211PNG"				image png)
    ("^GIF8[79]"			image gif)
    ("^II\\*\000"			image tiff)
    ("^MM\000\\*"			image tiff)
    ("^MThd"				audio midi)
    ("^\000\000\001\263"		video mpeg))
  "*Alist of regexp about magic-number vs. corresponding media-types.
Each element looks like (REGEXP TYPE SUBTYPE).
REGEXP is a regular expression to match against the beginning of the
content of entity.
TYPE is symbol to indicate primary type of media-type.
SUBTYPE is symbol to indicate subtype of media-type.")

;;;###autoload
(defun mime-preview-octet (entity situation)
  "A method for mime-view to preview octet message."
  (goto-char (point-max))
  (let ((p (point))
	(name (mime-entity-filename entity)))
    (insert "\n")
    (goto-char p)
    (save-restriction
      (narrow-to-region p p)
      (insert (with-temp-buffer
		(set-buffer-multibyte nil)
		(insert (mime-entity-content entity))
		(octet-buffer name)
		(set-buffer-multibyte t)
		(buffer-string))))))

;;;###autoload
(defun mime-view-octet (entity situation)
  "A method for mime-view to display octet message."
  (let (type subtype)
    ;; Guess by magic stolen from EMIKO.
    (let ((mdata (mime-entity-content entity))
	  (rest mime-magic-type-alist))
      (while (not (let ((cell (car rest)))
		    (if cell
			(if (string-match (car cell) mdata)
			    (setq type (nth 1 cell)
				  subtype (nth 2 cell)))
		      t)))
	(setq rest (cdr rest))))
    (if type
	(progn
	  (setq situation (del-alist 'method (copy-alist situation)))
	  (funcall (symbol-function 'mime-play-entity)
		   entity
		   (put-alist 'type type
			      (put-alist 'subtype subtype
					 situation))
		   'mime-view-octet))
      ;; Guess by filename suffix.
      (let ((buf (get-buffer-create
		  (format "%s-%s" (buffer-name) (mime-entity-number entity))))
	    (name (mime-entity-filename entity)))
	(with-current-buffer buf
	  (set-buffer-multibyte nil)
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (insert (mime-entity-content entity))
	  (octet-buffer name)
	  (setq buffer-read-only t
		truncate-lines t)
	  (set-buffer-multibyte t)
	  (set-buffer-modified-p nil))
	(let ((win (get-buffer-window (current-buffer))))
	  (or (eq (selected-window) win)
	      (select-window (or win (get-largest-window)))))
	(view-buffer buf)
	(run-hooks 'mime-view-octet-hook)
	(goto-char (point-min))))))

;;;###autoload
(defun octet-mime-setup ()
  "Octet setting for MIME module."
  (eval-after-load "mime-view"
    '(progn
       (ctree-set-calist-strictly
	'mime-acting-condition
	'((mode . "play")
	  (type . application)(subtype . msword)
	  (method . mime-view-octet)))

       (ctree-set-calist-strictly
	'mime-acting-condition
	'((mode . "play")
	  (type . application)(subtype . excel)
	  (method . mime-view-octet)))
       
       (ctree-set-calist-strictly
	'mime-acting-condition
	'((mode . "play")
	  (type . application)(subtype . x-msexcel)
	  (method . mime-view-octet)))
       
       (ctree-set-calist-strictly
	'mime-acting-condition
	'((mode . "play")
	  (type . application)(subtype . vnd.ms-excel)
	  (method . mime-view-octet)))

       (ctree-set-calist-strictly
	'mime-acting-condition
	'((mode . "play")
	  (type . application)(subtype . octet-stream)
	  (method . mime-view-octet)))
       
       (ctree-set-calist-strictly
	'mime-preview-condition
	'((type . application)(subtype . t)
	  (encoding . t)
	  (body . invisible)
	  (body-presentation-method . mime-preview-octet)))
       ;; another condition?
       )))

(provide 'octet)

;;; octet.el ends here

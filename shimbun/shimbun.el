;;; shimbun.el --- interfacing with web newspapers -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005
;; Yuuichi Teranishi <teranisi@gohome.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Akihiro Arisawa    <ari@mbf.sphere.ne.jp>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>
;; Keywords: news

;; This file is the main part of shimbun.

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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;; Shimbun API:
;;
;; shimbun-open
;; shimbun-server
;; shimbun-groups
;; shimbun-current-group
;; shimbun-open-group
;; shimbun-close-group
;; shimbun-headers
;; shimbun-reply-to
;; shimbun-x-face
;; shimbun-header-insert
;; shimbun-search-id
;; shimbun-article-expiration-days
;; shimbun-article
;; shimbun-close

;; Shimbun Header API:
;;
;; shimbun-header-subject
;; shimbun-header-set-subject
;; shimbun-header-from
;; shimbun-header-set-from
;; shimbun-header-date
;; shimbun-header-set-date
;; shimbun-header-id
;; shimbun-header-set-id
;; shimbun-header-references
;; shimbun-header-set-references
;; shimbun-header-chars
;; shimbun-header-set-chars
;; shimbun-header-lines
;; shimbun-header-set-lines
;; shimbun-header-xref
;; shimbun-header-set-xref
;; shimbun-header-extra
;; shimbun-header-set-extra

;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (require 'static))

(require 'mcharset)
(require 'eword-encode)
(require 'luna)
(require 'std11)

(eval-and-compile
  (luna-define-class shimbun ()
		     (mua server current-group groups
			  x-face x-face-alist
			  url coding-system from-address
			  content-start content-end
			  expiration-days server-name))
  (luna-define-internal-accessors 'shimbun))

(defgroup shimbun nil
  "shimbun - the backend library to read web newspapers."
  :group 'w3m
  :group 'hypermedia)

(defcustom shimbun-x-face
  "X-Face: @Q+y!*#)K`rvKfSQnCK.Q\\{T0qs@?plqxVu<=@H-y\
22NlKSprlFiND7{\"{]&Ddg1=P6{Ze|\n xbW2L1p5ofS\\&u~28A\
dJrT4Cd<Ls?U!G4}0S%FA~KegR;YZWieoc%`|$4M\\\"i*2avWm?"
  "*Default X-Face field for shimbun."
  :group 'shimbun
  :type '(string :format "%{%t%}:\n%v" :size 0))

(defcustom shimbun-server-additional-path nil
  "*List of additional directories to search for shimbun servers."
  :group 'shimbun
  :type '(repeat (directory :format "%t: %v\n" :size 0)))

(defcustom shimbun-checking-new-news-format "Checking new news on #S for #g"
  "*Format string used to show a progress message while chacking new news.
See `shimbun-message' for the special format specifiers."
  :group 'shimbun
  :type '(string :format "%{%t%}:\n%v" :size 0))

(defcustom shimbun-verbose t
  "*Flag controls whether shimbun should be verbose.
If it is non-nil, the `w3m-verbose' variable will be bound to nil
while shimbun is waiting for a server's response."
  :group 'shimbun
  :type 'boolean)

(defcustom shimbun-message-enable-logging nil
  "*Non-nil means preserve echo messages in the *Message* buffer."
  :group 'shimbun
  :type 'boolean)

(defun shimbun-servers-list ()
  "Return a list of shimbun servers."
  (let (servers)
    (dolist (dir (cons (file-name-directory (locate-library "shimbun"))
		       shimbun-server-additional-path))
      (when (file-directory-p dir)
	(dolist (file (directory-files dir nil nil t))
	  (and (string-match "\\`sb-\\(.*\\)\\.elc?\\'" file)
	       (not (member (setq file (match-string 1 file))
			    '("fml" "glimpse" "lump" "mailarc"
			      "mailman" "mhonarc" "rss" "text")))
	       (not (member file servers))
	       (push file servers)))))
    (sort servers 'string-lessp)))

(defun shimbun-servers-alist ()
  "Return an associative list of shimbun servers."
  (mapcar 'list (shimbun-servers-list)))

;;; Shimbun MUA
(eval-and-compile
  (luna-define-class shimbun-mua () (shimbun))
  (luna-define-internal-accessors 'shimbun-mua))

(luna-define-generic shimbun-mua-search-id (mua id)
  "Return non-nil when MUA found a message structure which corresponds to ID.")

(defun shimbun-mua-shimbun (mua)
  "Return the shimbun object created by MUA."
  (shimbun-mua-shimbun-internal mua))

;;; BASE 64
(require 'mel)
(eval-and-compile
  (fset 'shimbun-base64-encode-string
	(mel-find-function 'mime-encode-string "base64")))

;;; emacs-w3m implementation of url retrieval and entity decoding.
(require 'w3m)
(defun shimbun-retrieve-url (url &optional no-cache no-decode referer)
  "Rertrieve URL contents and insert to current buffer.
Return content-type of URL as string when retrieval succeeded."
  (let (type)
    (when (and url (setq type (w3m-retrieve url nil no-cache nil referer)))
      (unless no-decode
	(w3m-decode-buffer url)
	(goto-char (point-min)))
      type)))

(defun shimbun-fetch-url (shimbun url &optional no-cache no-decode referer)
  "Retrieve contents specified by URL for SHIMBUN.
This function is exacly similar to `shimbun-retrieve-url', but
considers the `coding-system' slot of SHIMBUN when estimating a coding
system of retrieved contents."
  (if (shimbun-coding-system-internal shimbun)
      (let ((w3m-coding-system-priority-list
	     (cons (shimbun-coding-system-internal shimbun)
		   w3m-coding-system-priority-list)))
	(inline
	  (shimbun-retrieve-url url no-cache no-decode referer)))
    (inline
      (shimbun-retrieve-url url no-cache no-decode referer))))

(defun shimbun-real-url (url &optional no-cache)
  "Return a real URL."
  (w3m-real-url url no-cache))

(defalias 'shimbun-decode-entities 'w3m-decode-entities)
(defalias 'shimbun-expand-url 'w3m-expand-url)
(defalias 'shimbun-find-coding-system 'w3m-find-coding-system)
(defalias 'shimbun-url-encode-string 'w3m-url-encode-string)

;;; Implementation of Header API.
(eval-and-compile
  (luna-define-class shimbun-header ()
		     (number subject from date id references
			     chars lines xref extra))
  (luna-define-internal-accessors 'shimbun-header))

(defun shimbun-header-number (header)
  (shimbun-header-number-internal header))

(defun shimbun-header-set-number (header number)
  (shimbun-header-set-number-internal header number))

(defun shimbun-header-subject (header &optional no-encode)
  (if no-encode
      (shimbun-header-subject-internal header)
    (shimbun-mime-encode-string
     (shimbun-header-subject-internal header))))

(defsubst shimbun-header-normalize (string &optional keep-angle-brackets)
  (when string
    (save-match-data
      ;; This is a trick to keep backward compatibility for
      ;; `shimbun-header-set-subject' and `shimbun-header-set-from'.
      (if (string-match eword-encoded-word-regexp string)
	  (eword-decode-string string)
	(with-temp-buffer
	  (insert string)
	  (unless keep-angle-brackets
	    (shimbun-remove-markup))
	  (shimbun-decode-entities)
	  (subst-char-in-region (point-min) (point-max) ?\t ?\  t)
	  (subst-char-in-region (point-min) (point-max) ?\r ?\  t)
	  (subst-char-in-region (point-min) (point-max) ?\f ?\  t)
	  (subst-char-in-region (point-min) (point-max) ?\n ?\  t)
	  (goto-char (point-min))
	  (skip-chars-forward " ")
	  (buffer-substring (point)
			    (progn
			      (goto-char (point-max))
			      (skip-chars-backward " ")
			      (point))))))))

(defun shimbun-header-set-subject (header subject &optional asis)
  (shimbun-header-set-subject-internal header
				       (if asis
					   subject
					 (shimbun-header-normalize subject))))

(defun shimbun-header-from (header &optional no-encode)
  (if no-encode
      (shimbun-header-from-internal header)
    (shimbun-mime-encode-string
     (shimbun-header-from-internal header))))

(defun shimbun-header-set-from (header from &optional asis)
  (shimbun-header-set-from-internal header
				    (if asis
					from
				      (shimbun-header-normalize from t))))

(defun shimbun-header-date (header)
  (shimbun-header-date-internal header))

(defun shimbun-header-set-date (header date &optional asis)
  (shimbun-header-set-date-internal header (if asis
					       date
					     (shimbun-header-normalize date))))

(defun shimbun-header-id (header)
  (shimbun-header-id-internal header))

(defun shimbun-header-set-id (header id &optional asis)
  (shimbun-header-set-id-internal header
				  (if asis
				      id
				    (shimbun-header-normalize id t))))

(defun shimbun-header-references (header)
  (shimbun-header-references-internal header))

(defun shimbun-header-set-references (header references &optional asis)
  (shimbun-header-set-references-internal
   header
   (if asis
       references
     (shimbun-header-normalize references t))))

(defun shimbun-header-chars (header)
  (shimbun-header-chars-internal header))

(defun shimbun-header-set-chars (header chars)
  (shimbun-header-set-chars-internal header chars))

(defun shimbun-header-lines (header)
  (shimbun-header-lines-internal header))

(defun shimbun-header-set-lines (header lines)
  (shimbun-header-set-lines-internal header lines))

(defun shimbun-header-xref (header)
  (shimbun-header-xref-internal header))

(defun shimbun-header-set-xref (header xref)
  (shimbun-header-set-xref-internal header xref))

(defun shimbun-header-extra (header)
  (shimbun-header-extra-internal header))

(defun shimbun-header-set-extra (header extra)
  (shimbun-header-set-extra-internal header extra))

(defun shimbun-create-header (&optional number subject from date id
					references chars lines xref
					extra asis)
  "Return a new header for a shimbun article.
Because `shimbun-create-header' normalizes arguments with
`shimbun-header-normalize' before creating new header object,
following operations are unnecessary:

  * MIME-encoding of subjects and from addresses.
  * Removal of HTML tags.
  * Decode of HTML entities.
  * Replacement of space characteres, such as tab, newline, and
    linefeed.

If optional 11th argument ASIS is non-nil, normalization of header
values is suppressed."
  (let ((new (luna-make-entity 'shimbun-header :number number)))
    (inline
      (shimbun-header-set-subject new subject asis)
      (shimbun-header-set-from new from asis)
      (shimbun-header-set-date new date asis)
      (shimbun-header-set-id new id asis)
      (shimbun-header-set-references new references asis)
      (shimbun-header-set-chars new chars)
      (shimbun-header-set-lines new lines)
      (shimbun-header-set-xref new xref)
      (shimbun-header-set-extra new extra))
    new))

(defun shimbun-make-header (&optional number subject from date id
				      references chars lines xref
				      extra)
  "Return a new header for a shimbun article.
This function is obsolete.  You should use `shimbun-create-header'
instead of this function."
  (shimbun-create-header number
			 (and subject (eword-decode-string subject))
			 (and from (eword-decode-string from))
			 date id references chars lines xref extra t))

;; Inline functions for the internal use.
(defsubst shimbun-article-base-url (shimbun header)
  "Return URL which points the original page specified by HEADER for SHIMBUN."
  (let ((xref (shimbun-header-xref header)))
    (if (and xref (eq (aref xref 0) ?/))
	(concat (shimbun-url-internal shimbun) xref)
      xref)))

(luna-define-generic shimbun-article-url (shimbun header)
  "Return URL which points the printable page specified by HEADER for SHIMBUN.")
(luna-define-method shimbun-article-url ((shimbun shimbun) header)
  (shimbun-article-base-url shimbun header))

(defcustom shimbun-encapsulate-images t
  "*If non-nil, inline images will be encapsulated in the articles.
Generated article have a multipart/related content-type."
  :group 'shimbun
  :type 'boolean)

;;; Base class for shimbun message entities:
(eval-and-compile
  (luna-define-class shimbun-entity () (type cid data))
  (luna-define-internal-accessors 'shimbun-entity))

(luna-define-generic shimbun-entity-type (entity) "Return TYPE of ENTITY.")
(luna-define-method shimbun-entity-type ((entity shimbun-entity))
  (shimbun-entity-type-internal entity))

(luna-define-generic shimbun-entity-cid (entity) "Return CID of ENTITY.")
(luna-define-method shimbun-entity-cid ((entity shimbun-entity))
  (shimbun-entity-cid-internal entity))

(defun shimbun-entity-set-cid (entity cid)
  (shimbun-entity-set-cid-internal entity cid))
(defsetf shimbun-entity-cid shimbun-entity-set-cid)

(luna-define-generic shimbun-entity-insert (entity)
  "Insert ENTITY as a MIME part.")
(luna-define-method shimbun-entity-insert ((entity shimbun-entity))
  (insert "Content-Type: " (shimbun-entity-type entity) "\n")
  (when (shimbun-entity-cid entity)
    (insert "Content-ID: <" (shimbun-entity-cid entity) ">\n"))
  (insert "\n"))

;;; Class for multipart entities:
(eval-and-compile
  (luna-define-class shimbun-multipart-entity (shimbun-entity) (boundary))
  (luna-define-internal-accessors 'shimbun-multipart-entity))

(defvar shimbun-multipart-entity-counter 0)

(luna-define-method initialize-instance :before ((entity
						  shimbun-multipart-entity)
						 &rest init-args)
  (shimbun-multipart-entity-set-boundary-internal
   entity
   (apply 'format "===shimbun_%d_%d_%d_%d==="
	  (incf shimbun-multipart-entity-counter)
	  (current-time))))

(defun shimbun-make-multipart-entity (&optional type cid)
  (luna-make-entity 'shimbun-multipart-entity :type type :cid cid))

(luna-define-method shimbun-entity-type ((entity shimbun-multipart-entity))
  (concat
   (or (shimbun-entity-type-internal entity)
       (shimbun-entity-set-type-internal
	entity
	(catch 'type
	  (dolist (child (shimbun-entity-data-internal entity))
	    (unless (string-match "\\`text/" (shimbun-entity-type child))
	      (throw 'type "multipart/related")))
	  "multipart/mixed")))
   "; boundary=\"" (shimbun-multipart-entity-boundary-internal entity) "\""
   (when (string= "multipart/related" (shimbun-entity-type-internal entity))
     (catch 'start
       (dolist (child (shimbun-entity-data-internal entity))
	 (when (string-match "\\`\\(text/\\|multipart/mixed\\)"
			     (shimbun-entity-type child))
	   (throw 'start
		  (concat "; type=\""
			  (shimbun-entity-type-internal child)
			  "\"; start=\"<"
			  (shimbun-entity-cid child)
			  ">\""))))))))

(luna-define-method shimbun-entity-insert :after ((entity
						   shimbun-multipart-entity))
  (let ((boundary (shimbun-multipart-entity-boundary-internal entity)))
    (dolist (child (shimbun-entity-data-internal entity))
      (insert "--" boundary "\n")
      (shimbun-entity-insert child)
      (insert (if (bolp)
		  "\n"
		"\n\n")))
    (insert "--" boundary "--\n")))

(defun shimbun-entity-add-child (entity &rest children)
  (shimbun-entity-set-data-internal entity
				    (nconc
				     (shimbun-entity-data-internal entity)
				     children)))

;;; Class of text entities:
(eval-and-compile
  (luna-define-class shimbun-text-entity (shimbun-entity) (charset))
  (luna-define-internal-accessors 'shimbun-text-entity))

(defun shimbun-make-text-entity (type data &optional cid)
  (luna-make-entity 'shimbun-text-entity :type type :data data :cid cid))

(luna-define-generic shimbun-text-entity-charset (entity &optional begin end)
  "Return MIME charset of ENTITY.")
(luna-define-method shimbun-text-entity-charset ((entity shimbun-text-entity)
						 &optional begin end)
  (or (shimbun-text-entity-charset-internal entity)
      (shimbun-text-entity-set-charset-internal
       entity
       (upcase
	(symbol-name
	 (if (and begin end)
	     (detect-mime-charset-region begin end)
	   (with-temp-buffer
	     (insert (shimbun-entity-data-internal entity))
	     (detect-mime-charset-region (point-min) (point-max)))))))))

(luna-define-method shimbun-entity-type ((entity shimbun-text-entity))
  (concat (shimbun-entity-type-internal entity)
	  "; charset=" (shimbun-text-entity-charset entity)))

(luna-define-method shimbun-entity-insert :around ((entity
						    shimbun-text-entity))
  (save-restriction
    (narrow-to-region (point) (point))
    (insert (shimbun-entity-data-internal entity))
    (encode-coding-region (point-min) (point-max)
			  (mime-charset-to-coding-system
			   (shimbun-text-entity-charset entity
							(point-min)
							(point-max))))
    (goto-char (point-min))
    (luna-call-next-method)
    (goto-char (point-max))))

;;; Class for image entities:
(eval-and-compile
  (luna-define-class shimbun-image-entity (shimbun-entity) (disposition))
  (luna-define-internal-accessors 'shimbun-image-entity))

(luna-define-method initialize-instance :before ((entity shimbun-image-entity)
						 &rest init-args)
  (shimbun-image-entity-set-disposition-internal entity "inline"))

(defun shimbun-make-image-entity (type data &optional cid)
  (luna-make-entity 'shimbun-image-entity :type type :data data :cid cid))

(luna-define-method shimbun-entity-insert :around ((entity
						    shimbun-image-entity))
  (insert "Content-Transfer-Encoding: base64\n"
	  "Content-Disposition: "
	  (shimbun-image-entity-disposition-internal entity) "\n")
  (luna-call-next-method)
  (insert
   (shimbun-base64-encode-string (shimbun-entity-data-internal entity))))

(defun shimbun-mime-replace-image-tags (base-cid &optional base-url images)
  "Replace all IMG tags with references to inlined image parts.
This function takes a BASE-CID as a base string for CIDs of inlined
image parts, and returns an alist of URLs and image entities."
  (goto-char (point-min))
  (let ((case-fold-search t)
	start end url img type)
    (while (and (re-search-forward "\\(<[\t\n\f\r ]*img\\)[\t\n\f\r ]" nil t)
		(progn
		  (setq start (match-end 1))
		  (search-forward ">" nil 'move))
		(progn
		  (setq end (match-beginning 0))
		  (goto-char start)
		  (re-search-forward
		   (eval-when-compile
		     (let ((spc "\t\n\f\r "))
		       (concat "[" spc "]+"
			       ;; 1. replaceable part
			       "\\(src[" spc "]*=[" spc "]*"
			       "\\(\""
			       ;; 3. url quoted with \"
			       "\\([^\"]+\\)"
			       "\"\\|'"
			       ;; 4. url quoted with '
			       "\\([^']+\\)"
			       "'\\|"
			       ;; 5. url unquoted
			       "\\([^" spc "\"']+\\)"
			       "\\)\\)")))
		   end t)))
      (setq start (match-beginning 1)
	    end (match-end 1)
	    url (shimbun-expand-url (or (match-string 3)
					(match-string 4)
					(match-string 5))
				    base-url))
      (unless (setq img (assoc url images))
	(with-temp-buffer
	  (set-buffer-multibyte nil)
	  (setq type (shimbun-retrieve-url url nil t base-url))
	  (when (and type (string-match "\\`image/" type))
	    (push (setq img (cons url
				  (shimbun-make-image-entity
				   type
				   (buffer-string)
				   (format "shimbun.inline.%d.%s"
					   (length images) base-cid))))
		  images))))
      (when img
	;; Only when an image is successfully retrieved, its
	;; source URI should be rewritten.
	(goto-char start)
	(delete-region start end)
	(insert "src=\"cid:" (shimbun-entity-cid (cdr img)) "\""))))
  images)

(defun shimbun-make-mime-article (shimbun header)
  "Make a MIME article according to SHIMBUN and HEADER.
If article have inline images, generated article have a multipart/related
content-type if `shimbun-encapsulate-images' is non-nil."
  (let ((base-cid (shimbun-header-id header)) images)
    (when (string-match "\\`<\\([^>]+\\)>\\'" base-cid)
      (setq base-cid (match-string 1 base-cid)))
    (when shimbun-encapsulate-images
      (setq images
	    (shimbun-mime-replace-image-tags base-cid
					     (shimbun-article-url shimbun
								  header))))
    (let ((body (shimbun-make-text-entity "text/html" (buffer-string))))
      (erase-buffer)
      (when images
	(let ((new (shimbun-make-multipart-entity)))
	  (setf (shimbun-entity-cid body) (concat "shimbun.0." base-cid))
	  (shimbun-entity-add-child new body)
	  (apply 'shimbun-entity-add-child new (mapcar 'cdr (nreverse images)))
	  (setq body new)))
      (shimbun-header-insert shimbun header)
      (insert "MIME-Version: 1.0\n")
      (shimbun-entity-insert body))))

(defcustom shimbun-x-face-database-function
  (if (boundp 'shimbun-use-bbdb-for-x-face)
      (cdr (assq (symbol-value 'shimbun-use-bbdb-for-x-face)
		 '((t . shimbun-bbdb-get-x-face)
		   (never . never)))))
  "*Function to get faces from a favorite database.
When its initial value is nil and BBDB or LSDB is loaded, it will be
set to an appropriate default value.  You can set this to `never' if
you want to use no database."
  :group 'shimbun
  :type '(radio
	  (const :tag "Default" nil)
	  (const :tag "Use no database" never)
	  (const :tag "Use BBDB" shimbun-bbdb-get-x-face)
	  (const :tag "Use LSDB" shimbun-lsdb-get-x-face)
	  (function :format "User defined function: %v\n" :size 0)))

(defun shimbun-header-insert (shimbun header)
  (let ((from (shimbun-header-from header))
	(refs (shimbun-header-references header))
	(reply-to (shimbun-reply-to shimbun))
	x-face
	;; Make sure the temp buffer's multibyteness is true.  It is
	;; needed to make `encode-mime-charset-string' (which is
	;; employed by `eword-encode-string') encode non-ascii text.
	(default-enable-multibyte-characters t))
    (insert
     (with-temp-buffer
       (insert "Subject: " (or (eword-encode-string
				(shimbun-header-subject header t))
			       "(none)")
	       "\nFrom: " (or (eword-encode-string
			       (shimbun-header-from header t))
			      "(nobody)")
	       "\nDate: " (or (shimbun-header-date header) "")
	       "\nMessage-ID: " (shimbun-header-id header) "\n")
       (when reply-to
	 (insert "Reply-To: " reply-to "\n"))
       (when (and refs
		  (string< "" refs))
	 (insert "References: " refs "\n"))
       (insert "Lines: " (number-to-string (or (shimbun-header-lines header)
					       0))
	       "\n"
	       "Xref: " (or (shimbun-article-base-url shimbun header) "") "\n")
       (unless shimbun-x-face-database-function
	 (when (and (fboundp 'bbdb-get-field)
		    (not (eq 'autoload
			     (car-safe (symbol-function 'bbdb-get-field))))
		    (boundp 'bbdb-file)
		    (stringp (symbol-value 'bbdb-file))
		    (file-exists-p (symbol-value 'bbdb-file)))
	   (setq shimbun-x-face-database-function 'shimbun-bbdb-get-x-face)))
       (unless shimbun-x-face-database-function
	 (when (and
		(fboundp 'lsdb-lookup-records)
		(not (eq 'autoload
			 (car-safe (symbol-function 'lsdb-lookup-records)))))
	   (setq shimbun-x-face-database-function 'shimbun-lsdb-get-x-face)))
       (when (setq x-face
		   (or (and from
			    (functionp shimbun-x-face-database-function)
			    (funcall shimbun-x-face-database-function from))
		       (shimbun-x-face shimbun)))
	 (insert x-face)
	 (unless (bolp)
	   (insert "\n")))
       (buffer-string)))))

(eval-when-compile
  ;; Attempt to pick up the inline function `bbdb-search-simple'.
  (condition-case nil
      (require 'bbdb)
    (error
     (autoload 'bbdb-search-simple "bbdb")
     (autoload 'bbdb-get-field "bbdb"))))

(defun shimbun-bbdb-get-x-face (person)
  "Search a face of a PERSON from BBDB.  When missing it, return nil."
  (let (x)
    (and (setq x (cadr (mail-extract-address-components person)))
	 (setq x (bbdb-search-simple nil x))
	 (setq x (bbdb-get-field x 'face))
	 (not (zerop (length x)))
	 (concat "X-Face: "
		 (mapconcat 'identity
			    (split-string x)
			    "\nX-Face: ")))))

(eval-when-compile
  (condition-case nil
      (require 'lsdb)
    (error
     (autoload 'lsdb-maybe-load-hash-tables "lsdb")
     (autoload 'lsdb-lookup-records "lsdb"))))

(defun shimbun-lsdb-get-x-face (person)
  "Return a face of a PERSON from LSDB.  When missing it, return nil."
  (lsdb-maybe-load-hash-tables)
  (let (x)
    (and (setq x (car (mail-extract-address-components person)))
	 (setq x (car (lsdb-lookup-records x)))
	 (setq x (cdr (assq 'x-face x)))
	 (not (zerop (length x)))
	 (mapconcat (lambda (x-face)
		      (concat "X-Face: "
			      (mapconcat 'identity
					 (split-string x-face)
					 "\n ")))
		    x
		    "\n"))))

;;; Implementation of Shimbun API.

(defconst shimbun-attributes
  '(url groups coding-system server-name from-address
	content-start content-end x-face-alist expiration-days))

(defun shimbun-open (server &optional mua)
  "Open a shimbun for SERVER.
Optional MUA is a `shimbun-mua' instance."
  (let ((load-path (append shimbun-server-additional-path load-path)))
    (require (intern (concat "sb-" server))))
  (let (url groups coding-system server-name from-address
	    content-start content-end x-face-alist shimbun expiration-days)
    (dolist (attr shimbun-attributes)
      (set attr
	   (symbol-value (intern-soft
			  (concat "shimbun-" server "-" (symbol-name attr))))))
    (setq shimbun (luna-make-entity (intern (concat "shimbun-" server))
				    :mua mua
				    :server server
				    :server-name server-name
				    :url url
				    :groups groups
				    :coding-system coding-system
				    :from-address from-address
				    :content-start content-start
				    :content-end content-end
				    :expiration-days expiration-days
				    :x-face-alist x-face-alist))
    (when mua
      (shimbun-mua-set-shimbun-internal mua shimbun))
    shimbun))

(defun shimbun-server (shimbun)
  "Return the server name of SHIMBUN."
  (shimbun-server-internal shimbun))

(luna-define-generic shimbun-server-name (shimbun)
  "Return the server name of SHIMBUN in human-readable style.")

(luna-define-method shimbun-server-name ((shimbun shimbun))
  (or (shimbun-server-name-internal shimbun)
      (shimbun-server-internal shimbun)))

(luna-define-generic shimbun-groups (shimbun)
  "Return a list of groups which are available in the SHIMBUN.")

(luna-define-method shimbun-groups ((shimbun shimbun))
  (shimbun-groups-internal shimbun))

(luna-define-generic shimbun-group-p (shimbun group)
  "Return non-nil if group is available in the SHIMBUN.")

(luna-define-method shimbun-group-p ((shimbun shimbun) group)
  (member group (shimbun-groups shimbun)))

(defun shimbun-current-group (shimbun)
  "Return the current group of SHIMBUN."
  (shimbun-current-group-internal shimbun))

(luna-define-generic shimbun-current-group-name (shimbun)
  "Return the current group name of SHIMBUN in human-readable style.")

(luna-define-method shimbun-current-group-name ((shimbun shimbun))
  (shimbun-current-group-internal shimbun))

(defun shimbun-open-group (shimbun group)
  "Open a SHIMBUN GROUP."
  (if (shimbun-group-p shimbun group)
      (shimbun-set-current-group-internal shimbun group)
    (error "No such group %s" group)))

(defun shimbun-close-group (shimbun)
  "Close opened group of SHIMBUN."
  (when (shimbun-current-group-internal shimbun)
    (shimbun-set-current-group-internal shimbun nil)))

(luna-define-generic shimbun-headers (shimbun &optional range)
  "Return a SHIMBUN header list.
Optional argument RANGE is one of following:
nil or `all': Retrieve all header indices.
`last':       Retrieve the last header index.
integer n:    Retrieve n pages of header indices.")

(defmacro shimbun-header-index-pages (range)
  "Return number of pages to retrieve according to RANGE.
Return nil if all pages should be retrieved."
  (` (if (eq 'last (, range)) 1
       (if (eq 'all (, range)) nil
	 (, range)))))

(luna-define-method shimbun-headers ((shimbun shimbun) &optional range)
  (shimbun-message shimbun (concat shimbun-checking-new-news-format "..."))
  (prog1
      (with-temp-buffer
	(let ((w3m-verbose (if shimbun-verbose nil w3m-verbose)))
	  (shimbun-fetch-url shimbun (shimbun-index-url shimbun) 'reload)
	  (shimbun-get-headers shimbun range)))
    (shimbun-message shimbun (concat shimbun-checking-new-news-format
				     "...done"))))

(luna-define-generic shimbun-reply-to (shimbun)
  "Return a reply-to field body for SHIMBUN.")

(luna-define-method shimbun-reply-to ((shimbun shimbun))
  nil)

(luna-define-generic shimbun-x-face (shimbun)
  "Return a X-Face field string for SHIMBUN.")

(luna-define-method shimbun-x-face ((shimbun shimbun))
  (shimbun-set-x-face-internal
   shimbun
   (or (cdr (assoc (shimbun-current-group-internal shimbun)
		   (shimbun-x-face-alist-internal shimbun)))
       (cdr (assoc "default" (shimbun-x-face-alist-internal shimbun)))
       shimbun-x-face)))

(defun shimbun-search-id (shimbun id)
  "Return non-nil when MUA found a message structure which corresponds to ID."
  (when (shimbun-mua-internal shimbun)
    (shimbun-mua-search-id (shimbun-mua-internal shimbun) id)))

(defun shimbun-article-expiration-days (shimbun)
  "Return an expiration day number of SHIMBUN.
Return nil when articles are not expired."
  (shimbun-expiration-days-internal shimbun))

(luna-define-generic shimbun-from-address (shimbun)
  "Make a From address like \"SERVER (GROUP) <ADDRESS>\".")

(luna-define-method shimbun-from-address ((shimbun shimbun))
  (format "%s (%s) <%s>"
	  (shimbun-server-name shimbun)
	  (shimbun-current-group-name shimbun)
	  (or (shimbun-from-address-internal shimbun)
	      (shimbun-reply-to shimbun))))

(luna-define-generic shimbun-article (shimbun header &optional outbuf)
  "Retrieve a SHIMBUN article which corresponds to HEADER to the OUTBUF.
HEADER is a shimbun-header which is obtained by `shimbun-headers'.
If OUTBUF is not specified, article is retrieved to the current buffer.")

(luna-define-method shimbun-article ((shimbun shimbun) header &optional outbuf)
  (when (shimbun-current-group-internal shimbun)
    (with-current-buffer (or outbuf (current-buffer))
      (w3m-insert-string
       (or (with-temp-buffer
	     (shimbun-fetch-url shimbun
				(shimbun-article-url shimbun header)
				nil nil
				(shimbun-article-base-url shimbun header))
	     (shimbun-message shimbun "shimbun: Make contents...")
	     (goto-char (point-min))
	     (prog1 (shimbun-make-contents shimbun header)
	       (shimbun-message shimbun "shimbun: Make contents...done")))
	   "")))))

(luna-define-generic shimbun-make-contents (shimbun header)
  "Return a content string of SHIMBUN article using current buffer content.
HEADER is a header structure obtained via `shimbun-headers'.")

(defsubst shimbun-make-html-contents (shimbun header)
  (when (shimbun-clear-contents shimbun header)
    (goto-char (point-min))
    (insert "<html>\n<head>\n<base href=\""
	    (shimbun-article-url shimbun header)
	    "\">\n</head>\n<body>\n")
    (goto-char (point-max))
    (insert (shimbun-footer shimbun header t)
	    "\n</body>\n</html>\n"))
  (shimbun-make-mime-article shimbun header)
  (buffer-string))

(luna-define-method shimbun-make-contents ((shimbun shimbun) header)
  (shimbun-make-html-contents shimbun header))

(luna-define-generic shimbun-clear-contents (shimbun header)
  "Clear a content in this current buffer for an article of SHIMBUN.
Return nil, unless a content is cleared successfully.")

(luna-define-method shimbun-clear-contents ((shimbun shimbun) header)
  (let ((case-fold-search t) start)
    (goto-char (point-min))
    (when (and (stringp (shimbun-content-start-internal shimbun))
	       (re-search-forward (shimbun-content-start-internal shimbun)
				  nil t)
	       (setq start (point))
	       (stringp (shimbun-content-end-internal shimbun))
	       (re-search-forward (shimbun-content-end-internal shimbun)
				  nil t))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start)
      t)))

(luna-define-generic shimbun-footer (shimbun header &optional html)
  "Make a footer string for SHIMBUN and HEADER.")

(luna-define-method shimbun-footer ((shimbun shimbun) header &optional html)
  "Return a null string for servers that have no footer."
  "")

(luna-define-generic shimbun-index-url (shimbun)
  "Return a index URL of SHIMBUN.")

(luna-define-method shimbun-index-url ((shimbun shimbun))
  (shimbun-url-internal shimbun))

(luna-define-generic shimbun-get-headers (shimbun &optional range)
  "Return a shimbun header list of SHIMBUN.
Optional argument RANGE is one of following:
nil or `all': Retrieve all header indices.
`last':       Retrieve the last header index.
integer n:    Retrieve n pages of header indices.")

(luna-define-generic shimbun-close (shimbun)
  "Close a SHIMBUN.")

(luna-define-method shimbun-close ((shimbun shimbun))
  (shimbun-close-group shimbun))

;;; Virtual class for Japanese Newspapers:
(luna-define-class shimbun-japanese-newspaper () ())
(luna-define-method shimbun-footer ((shimbun shimbun-japanese-newspaper) header
				    &optional html)
  (if html
      (concat "\n<p align=\"left\">\n-- <br>\nこの記事の著作権は、"
	      (shimbun-server-name shimbun)
	      "社に帰属します。<br>\n原物は <a href=\""
	      (shimbun-article-base-url shimbun header) "\">"
	      (shimbun-article-base-url shimbun header)
	      "</a> で公開されています。\n</p>\n")
    (concat "\n-- \nこの記事の著作権は、"
	    (shimbun-server-name shimbun)
	    "社に帰属します。\n原物は "
	    (shimbun-article-base-url shimbun header)
	    " で公開されています。\n")))

;;; Misc Functions
(static-cond
 ((fboundp 'point-at-bol)
  (defalias 'shimbun-point-at-bol 'point-at-bol))
 ((fboundp 'line-beginning-position)
  (defalias 'shimbun-point-at-bol 'line-beginning-position))
 (t
  (defun shimbun-point-at-bol ()
    "Return point at the beginning of the line."
    (let ((p (point)))
      (beginning-of-line)
      (prog1 (point)
	(goto-char p))))))

(static-cond
 ((fboundp 'point-at-eol)
  (defalias 'shimbun-point-at-eol 'point-at-eol))
 ((fboundp 'line-end-position)
  (defalias 'shimbun-point-at-eol 'line-end-position))
 (t
  (defun shimbun-point-at-eol ()
    "Return point at the end of the line."
    (let ((p (point)))
      (end-of-line)
      (prog1 (point)
	(goto-char p))))))

(defun shimbun-header-insert-and-buffer-string (shimbun header
							&optional charset html)
  "Insert headers which are generated from SHIMBUN and HEADER, and
return the contents of this buffer as an encoded string."
  (unless charset
    (setq charset "ISO-2022-JP"))
  (goto-char (point-min))
  (shimbun-header-insert shimbun header)
  (insert "Content-Type: text/" (if html "html" "plain") "; charset=" charset
	  "\nMIME-Version: 1.0\n\n")
  (if html
      (progn
	(insert "<html><head><base href=\""
		(shimbun-article-url shimbun header)
		"\"></head><body>")
	(goto-char (point-max))
	(insert (shimbun-footer shimbun header html)
		"\n</body></html>"))
    (goto-char (point-max))
    (insert (shimbun-footer shimbun header html)))
  (encode-coding-string (buffer-string)
			(mime-charset-to-coding-system charset)))

(defun shimbun-mime-encode-string (string)
  (condition-case nil
      (save-match-data
	;; Make sure the temp buffer's multibyteness is true.  It is
	;; needed to make `encode-mime-charset-string' (which is
	;; employed by `eword-encode-string') encode non-ascii text.
	(let ((default-enable-multibyte-characters t))
	  (with-temp-buffer
	    (mapconcat
	     #'identity
	     (split-string (or (eword-encode-string
				(shimbun-decode-entities-string string)) ""))
	     " "))))
    (error string)))

(defun shimbun-make-date-string (year month day &optional time timezone)
  "Make a date string which will be used as shimbun headers.

YEAR is a 4-digit number, and MONTH and DAY are also numbers.  TIME is
a string in the \"HH:MM\" form, where HH is a number of hours and MM is
a number of minutes.  It defaults to \"00:00\".

TIMEZONE defaults to \"+0900\" by the historical reason.  You should
specify this if a time is represented based on other than the \"+0900\"
zone."
  (setq year (cond ((< year 69)
		    (+ year 2000))
		   ((< year 100)
		    (+ year 1900))
		   ((< year 1000)	; possible 3-digit years.
		    (+ year 1900))	; why isn't it 1000?
		   (t year)))
  (let ((cts (current-time-string (encode-time 0 0 0 day month year))))
    (format "%s, %02d %s %04d %s %s"
	    (substring cts 0 3)
	    day
	    (substring cts 4 7)
	    year
	    (or time "00:00")
	    (or timezone "+0900"))))

(autoload 'timezone-fix-time "timezone")

(defun shimbun-time-parse-string (string)
  "Parse the time-string STRING into the Emacs style (HIGH LOW) time."
  (let ((x (nreverse (append (timezone-fix-time string nil nil) nil))))
    (apply 'encode-time (nconc (cdr x) (list (car x))))))

(defun shimbun-sort-headers (headers)
  "Return a list of sorted HEADERS by date in increasing order."
  (sort headers
	(lambda (a b)
	  (setq a (shimbun-time-parse-string (shimbun-header-date a))
		b (shimbun-time-parse-string (shimbun-header-date b)))
	  (or (< (car a) (car b))
	      (and (= (car a) (car b))
		   (< (cadr a) (cadr b)))))))

(if (fboundp 'regexp-opt)
    (defalias 'shimbun-regexp-opt 'regexp-opt)
  (defun shimbun-regexp-opt (strings &optional paren)
    "Return a regexp to match a string in STRINGS.
Each string should be unique in STRINGS and should not contain any regexps,
quoted or not.  If optional PAREN is non-nil, ensure that the returned regexp
is enclosed by at least one regexp grouping construct."
    (let ((open-paren (if paren "\\(" "")) (close-paren (if paren "\\)" "")))
      (concat open-paren (mapconcat 'regexp-quote strings "\\|") close-paren))))

(defun shimbun-decode-entities-string (string)
  "Decode entities in the STRING."
  (with-temp-buffer
    (insert string)
    (shimbun-decode-entities)
    (buffer-string)))

(defun shimbun-remove-tags (begin-tag &optional end-tag)
  "Remove all occurrences of regions surrounded by BEGIN-TAG and END-TAG."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (if end-tag
	(let (pos)
	  (while (and (re-search-forward begin-tag nil t)
		      (setq pos (match-beginning 0))
		      (re-search-forward end-tag nil t))
	    (delete-region pos (point))))
      (while (re-search-forward begin-tag nil t)
	(delete-region (match-beginning 0) (match-end 0))))))

(defun shimbun-remove-markup ()
  "Remove all HTML markup, leaving just plain text."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "<!--" nil t)
      (delete-region (match-beginning 0)
		     (or (search-forward "-->" nil t)
			 (point-max))))
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t)
      (replace-match "" t t))))

(defsubst shimbun-strip-cr ()
  "Strip ^M from the end of all lines."
  (goto-char (point-max))
  (while (search-backward "\r\n" nil t)
    (delete-char 1)))

(eval-and-compile
  (cond
   ((fboundp 'replace-in-string)
    (defalias 'shimbun-replace-in-string 'replace-in-string))
   ((fboundp 'replace-regexp-in-string)
    (defun shimbun-replace-in-string  (string regexp newtext &optional literal)
      ;;(replace-regexp-in-string regexp newtext string nil literal)))
      ;;
      ;; Don't call the symbol function `replace-regexp-in-string' directly
      ;; in order to silence the byte-compiler when an Emacs which doesn't
      ;; provide it is used.  The following form generates exactly the same
      ;; byte-code.
      (funcall (symbol-function 'replace-regexp-in-string)
	       regexp newtext string nil literal)))
   (t
    (defun shimbun-replace-in-string (string regexp newtext &optional literal)
      (let ((start 0) tail)
	(while (string-match regexp string start)
	  (setq tail (- (length string) (match-end 0)))
	  (setq string (replace-match newtext nil literal string))
	  (setq start (- (length string) tail))))
      string))))

(if (fboundp 'subst-char-in-string)
    (defalias 'shimbun-subst-char-in-string 'subst-char-in-string)
  (defun shimbun-subst-char-in-string (fromchar tochar string
						&optional inplace)
    "Replace characters in STRING from FROMCHAR to TOCHAR.
Unless optional argument INPLACE is non-nil, return a new string."
    (let ((string (if inplace string (copy-sequence string)))
	  (len (length string))
	  (idx 0))
      ;; Replace all occurrences of FROMCHAR with TOCHAR.
      (while (< idx len)
	(when (= (aref string idx) fromchar)
	  (aset string idx tochar))
	(setq idx (1+ idx)))
      string)))

(defun shimbun-message (shimbun fmt &rest args)
  "Function equivalent to `message' enabling to handle special formats.
SHIMBUN is a shimbun entity object.  FMT and ARGS are the same as the
arguments of `message'.  This function allows the following special
format specifiers:

#g means print a group name.
#s means print a server name.
#S means print a human-readable server name.

Use ## to put a single # into the output.  If `shimbun-verbose' is nil,
it will run silently.  The `shimbun-message-enable-logging' variable
controls whether this function should preserve a message in the
*Messages* buffer."
  (let ((default-enable-multibyte-characters t)
	specifier)
    (with-temp-buffer
      (insert fmt)
      (goto-char (point-min))
      (while (search-forward "#" nil t)
	(setq specifier (char-after))
	(delete-region (1- (point)) (1+ (point)))
	(cond ((eq specifier ?#)
	       (insert "#"))
	      ((eq specifier ?g)
	       (insert (shimbun-current-group-internal shimbun)))
	      ((eq specifier ?s)
	       (insert (shimbun-server-internal shimbun)))
	      ((eq specifier ?S)
	       (insert (or (shimbun-server-name-internal shimbun)
			   (shimbun-server-internal shimbun))))))
      (setq fmt (buffer-string))))
  (if shimbun-verbose
      (static-if (featurep 'xemacs)
	  (let ((string (apply 'format fmt args)))
	    (if shimbun-message-enable-logging
		(display-message 'message string)
	      (display-message 'no-log string))
	    string)
	(if shimbun-message-enable-logging
	    (apply 'message fmt args)
	  (let (message-log-max)
	    (apply 'message fmt args))))
    (apply 'format fmt args)))

(defun shimbun-break-long-japanese-lines (&optional shimbun)
  "Break long Japanese lines in an article.
Article should be charset decoded html data.  If SHIMBUN is given,
this function will narrow the buffer to just an article using the
shimbun class variables `content-start' and `content-end'.  Otherwise,
it considers the buffer has already been narrowed to an article."
  (save-restriction
    (when shimbun
      (goto-char (point-min))
      (let ((case-fold-search t)
	    start)
	(when (and (re-search-forward (shimbun-content-start-internal shimbun)
				      nil t)
		   (setq start (point))
		   (re-search-forward (shimbun-content-end-internal shimbun)
				      nil t))
	  (narrow-to-region start (match-beginning 0)))))
    (goto-char (point-min))
    (while (re-search-forward "<p[^>]*>\\|</p>\\|[、。）」]+" nil t)
      (unless (eolp)
	(insert "\n"))))
  (goto-char (point-min)))

(provide 'shimbun)

;;; shimbun.el ends here

;;; mime-w3m.el --- mime-view content filter for text

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>

;; Author: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>,
;;         Akihiro Arisawa <ari@mbf.sphere.ne.jp>
;; Keywords: HTML, MIME, multimedia, mail, news

;; This file is *NOT* yet part of SEMI (Suite of Emacs MIME Interfaces).

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


;;; Install:

;; (1) Install SEMI.
;; (2) Put this file to appropriate directory.
;; (3) Write these following code to your ~/.emacs or ~/.gnus.
;;
;;    (setq mime-setup-enable-inline-html nil)
;;    (eval-after-load "mime-view"
;;      '(progn
;;         (autoload 'mime-w3m-preview-text/html "mime-w3m")
;;         (ctree-set-calist-strictly
;;          'mime-preview-condition
;;          '((type . text)
;;            (subtype . html)
;;            (body . visible)
;;            (body-presentation-method . mime-w3m-preview-text/html)))
;;         (set-alist 'mime-view-type-subtype-score-alist
;;                    '(text . html) 3)))


;;; Code:

(require 'w3m)
(require 'mime)
(eval-when-compile (require 'cl))

(defvar mime-w3m-mode-map nil)

(defmacro mime-put-keymap-region (start end keymap)
  `(put-text-property ,start ,end
		      ',(if (featurep 'xemacs)
			    'keymap
			  'local-map)
		      ,keymap))

(defmacro mime-save-background-color (&rest body)
  (if (featurep 'xemacs)
      `(let ((color (color-name (face-background 'default))))
	 (prog1
	     (progn ,@body)
	   (font-set-face-background 'default color (current-buffer))
	   ))
    (cons 'progn body)))

(defvar mime-w3m-message-structure nil)

(defun mime-w3m-preview-text/html (entity situation)
  (setq mime-w3m-message-structure (mime-find-root-entity entity))
  (let ((p (point))
	(xref (mime-entity-fetch-field entity "xref")))
    ;; For nnshimbun.el.
    (and (stringp xref)
	 (string-match "^http://" xref)
	 (setq w3m-current-url xref))
    (goto-char p)
    (insert "\n")
    (goto-char p)
    (mime-save-background-color
     (save-restriction
       (narrow-to-region p p)
       (mime-insert-text-content entity)
       (run-hooks 'mime-text-decode-hook)
       (condition-case err
	   (w3m-region p (point-max))
	 (error (message (format "%s" err))))
       (mime-put-keymap-region p (point-max) mime-w3m-mode-map)
       ))))

(defun mime-w3m-cid-retrieve (url &optional no-decode accept-type-regexp
				  no-cache)
  (let ((entity (mime-find-entity-from-content-id (mime-uri-parse-cid url)
						  mime-w3m-message-structure)))
    (when entity
      (w3m-with-work-buffer
       (delete-region (point-min) (point-max))
       (set-buffer-multibyte nil)
       (mime-insert-entity-content entity))
      (mime-entity-type/subtype entity))))

(push (cons 'mime-view-mode 'mime-w3m-cid-retrieve)
      w3m-cid-retrieve-function-alist)

(unless mime-w3m-mode-map
  (let ((map (copy-keymap w3m-mode-map)))
    (substitute-key-definition 'w3m-view-this-url 'mime-w3m-view-this-url map)
    (substitute-key-definition 'w3m-mouse-view-this-url 'mime-w3m-view-this-url map)
    (substitute-key-definition 'w3m-quit 'mime-preview-quit map)
    (substitute-key-definition 'w3m-view-previous-page nil map)
    (substitute-key-definition 'w3m-reload-this-page nil map)
    (setq mime-w3m-mode-map map)))

(defun mime-w3m-view-this-url ()
  "*View the URL of the link under point."
  (interactive)
  (let ((url (w3m-anchor)))
    (when url (w3m url))))

(defun mime-w3m-mouse-view-this-url (event)
  (interactive "e")
  (mouse-set-point event)
  (let ((url (w3m-anchor)) (img (w3m-image)))
    (cond
     (url (w3m url))
     (img (w3m-view-image))
     (t (message "No URL at point.")))))

(provide 'mime-w3m)
;;; mime-w3m.el ends here

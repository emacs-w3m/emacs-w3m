;;; mime-w3m.el --- mime-view content filter for text

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>

;; Author: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
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
;;         (autoload 'w3m-mime-preview-text/html "mime-w3m")
;;         (ctree-set-calist-strictly
;;          'mime-preview-condition
;;          '((type . text)
;;            (subtype . html)
;;            (body . visible)
;;            (body-presentation-method . w3m-mime-preview-text/html)))
;;         (set-alist 'mime-view-type-subtype-score-alist
;;                    '(text . html) 3)))


;;; Code:

(condition-case nil
    (require 'w3m)
  (error nil))
(require 'mime)

(defmacro-maybe mime-put-keymap-region (start end keymap)
  `(put-text-property ,start ,end
		      ',(if (featurep 'xemacs)
			    'keymap
			  'local-map)
		      ,keymap))

(defmacro-maybe mime-save-background-color (&rest body)
  (if (featurep 'xemacs)
      `(let ((color (color-name (face-background 'default))))
	 (prog1
	     (progn ,@body)
	   (font-set-face-background 'default color (current-buffer))
	   ))
    (cons 'progn body)))

(defun w3m-mime-preview-text/html (entity situation)
  (goto-char (point-max))
  (let ((p (point)))
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
       (mime-put-keymap-region p (point-max) w3m-mode-map)
       ))))

(provide 'mime-w3m)
;;; mime-w3m.el ends here

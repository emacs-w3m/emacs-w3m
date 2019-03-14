;;; w3m-download.el --- use wget, show progress, save metadata -*- coding: utf-8 ; lexical-binding: t -*-

;; Copyright © 2019 Boruch Baum <boruch_baum@gmx.com>

;; Authors:    Boruch Baum <boruch_baum@gmx.com>
;; Keywords:   w3m, WWW, hypermedia
;; Homepage:   http://emacs-w3m.namazu.org/
;; Repository: https://github.com/emacs-w3m/emacs-w3m

;; This file is part of `emacs-w3m'.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.



;;; Commentary:

;; This file provides download features for the `emacs-w3m' project.
;; Although the project is meant to be a front-end to the `w3m'
;; browser, this code uses elisp and, when available, external
;; back-end programs (currently just `wget') in order to offer
;; additional download features not native to `w3m':
;;
;;   1) Individual detailed download progress logs in dedicated
;;      buffers, automatically deleted upon successful completion.
;;
;;   2) Partial downloads are stored on disk, not in memory (yes...).
;;
;;   3) Resumption of partial downloads that were interrupted due to
;;      aborts or other failures.
;;
;;   4) Optional appending of meta-data to a download, currently
;;      offered for png and jpeg files. The image link's "alt" caption
;;      is stored as meta-data element "Exif.Image.ImageDescription".
;;      Note that enabling this option will modify the file's
;;      checksum. This option currently uses `exif' and `exiv2' as
;;      external back-end programs.
;;
;;      4.1) Some ways to view from the command-line an embedded
;;           caption are:
;;             exiv2 -g "Exif.Image.ImageDescription" foo.png
;;             exif --ifd=0 -t0x010e  foo.jpg |grep value
;;
;; This file also absorbed most of the legacy download functions,
;; those which were basically wrappers for `w3m-download', to keep all
;; (most) functionality in one place. The functions and defcustom not
;; moved here are:
;;
;;   --FILE--
;;   mew-w3m.el    mew-w3m-ext-url-fetch (dummy url)
;;   w3m.el        w3m-external-view (url &optional no-cache handler)
;;   w3m-lnum.el   w3m-lnum-actions-image-alist
;;   w3m-lnum.el   w3m-lnum-save-image ()



;;; TODO
;; + danger of 'feature-creep': None of these-items are necessary, and
;;   at some point the project should consider turning to a en external
;;   program which specializes in downlaoding, but anyway ...
;;   + an alist of extensions and metadata commands
;;   + an alist of download commands (eg. aria2, curl)
;;   + offloading to a dedicated downloader (eg. uget)
;;   + persistence across crashes (ie. maintain a disk file)
;;   + warn if more than 'n' concurrent downloads in progress
;;   + queue when more than 'n' concurrent downloads in progress
;;   + renice download processes
;;   + pause a download
;;   + delete a partially downloaded file when manually aborted
;;     (eg. when killing the progress buffer)
;;   + replace individual progress buffers with a single buffer, and
;;     use the process-fiter function to maintain a dedicated section
;;     for each download. This would be similar to the 'look' of the
;;     download windows of many other browsers.
;;     + summary line at top of buffer.
;;     + download states could be color-coded.
;;     + click option to resume a download
;;     + click option to open a download



;;; Code

;;  Dependencies
(require 'w3m-util)
(require 'w3m)



;;; Temporary compatability operation(s)

;; My development git fork has a different messaging standard, based
;; upon a pending pull request from my branch `bb_messaging', so
;; support this git branch being merged into a branch lacking that
;; other merge.
(eval-when-compile
  (when (not (fboundp 'w3m--message))
    (defun w3m--message (timeout face &rest args)
      (w3m-message args))))



;;; Global variables

;;Global list of all running `w3m-download' processes.
(setq w3m--download-processes-list nil)



;;; User-options

(defcustom w3m-download-save-metadata t
  "Whether image downloads should save their descriptions.

This sets a feature for function `w3m-download' that operates on
jpeg and png images, and saves their link 'alt' caption strings
as the file's \"Exif.Image.ImageDescription\" metadata element.
From within emas-w3m you can see what the value would be by
moving point to the link (the string will appear in the
mini-buffer).

This operation is performed by calls to external programs; if
those programs are not executable on your system, a message will
be logged. Currently, the selected programs are hard-coded,
`exif' for jpeg files, and `exiv2' for png files.

Note that enabling this option will modify the file's checksum."
  :group 'w3m
  :type 'boolean)



;;; Buffer-local variables

(defvar-local w3m--download-metadata-operation nil
  "Text string of the complete shell command to be used to tag a
  download. It is set buffer-local in the download's progress
  buffer.")

(defvar-local w3m--download-local-proc nil
  "The process id of the download associated with the current
  download progress buffer.")

(defvar-local w3m--download-save-path nil
  "The full path-name of the downloaded file.")


;;; Hook functions
;;
;; Maybe (see 'feature creep' discussion above) there should be hook
;; functions to optionally kill running downloads, for
;; `kill-emacs-hook' (without user interaction),
;; `kill-emacs-query-functions' (with user interaction), and the mode
;; exit hook for `emacs-w3m' (look it up, what is it?). Maybe there
;; should also be download recovery/resume/re-attach hooks for either
;; `after-init-hook', `emacs-startup-hook' or the startup hook for
;; `emacs-w3m' (look it up, what is it?)

(defun w3m--download-kill-all-asociated-processes ()
;; NOTE: This function can be used directly in development to clean up
;; any stray processes.
;;
;; TODO: Consider calling this function upon exiting emacs-w3m. The
;; argument against doing so is that it allows operating downloads to
;; continue. The argument for is that the progress of such processes
;; will no longer be trackable. The argument against would propose to
;; maintain a record on disk of `w3m--download-processes-list', and
;; offer the user to either re-create the tracking buffers, or to kill
;; the processes.
;;
;; TODO: Double check that if emacs crashes during a download, the
;; wget sub-processes also die. If for some reason not, we could
;; programmatically find the processes and possibly re-attach their
;; STDOUT/STDERR to a new tracking buffer, or we could kill them.
  (let (x)
    (while (setq x (pop w3m--download-processes-list))
      (when (processp (car x))
        (delete-process (car x)))
      (when (bufferp (cdr x))
        (kill-buffer (cdr x))))))



;;; Internal functions

(defun w3m--download-apply-metadata-tags ()
  "Run a shell command to apply metadata tags to a saved file.
The literal text of the shell command to run to tag the file
would be in the download buffer's buffer-local variable
`w3m--download-metadata-operation'.

When a file is saved from the cache, this function is not called;
the operation is perfomed directly by
`w3m--download-check-and-use-cache'."
  (when w3m--download-metadata-operation
    (goto-char (point-max))
    (insert (format "\nAdding meta-data to file... \n  %s\n"
                    w3m--download-metadata-operation))
    (shell-command w3m--download-metadata-operation t)))

(defun w3m--download-check-and-use-cache (url save-path metadata)
  (when (bufferp w3m-cache-buffer)
    (let* (beg end
          (ident (intern (w3m-w3m-canonicalize-url url) w3m-cache-hashtb)))
      (with-current-buffer w3m-cache-buffer
        (cond
         ((not (setq beg (text-property-any
                           (point-min) (point-max) 'w3m-cache ident)))
          ;; It wasn't in the cache after all.
          (setq w3m-cache-articles (delq ident w3m-cache-articles))
          nil)
         (t
          ;; Find the end (i.e., the beginning of the next article).
          (when (setq end (next-single-property-change
                     (1+ beg) 'w3m-cache w3m-cache-buffer (point-max)))
            (write-region beg end save-path))
          (when metadata
            (shell-command metadata))
          (w3m--message t t "Saved from cache %s to %s"
            (if metadata "(with metadata)" "")
            save-path)
          t))))))

(defun w3m--download-kill-associated-process ()
  "Hook function for `Kill-buffer-hook' for w3m download buffers.
PROC should have been set as a local variable at buffer creation."
  (if (not (processp w3m--download-local-proc))
    (w3m--message t 'w3m-warning
                  "Warning: no process found to kill (w3m-download).")
   (delete-process w3m--download-local-proc)
   (setq w3m--download-processes-list
     (assq-delete-all w3m--download-local-proc w3m--download-processes-list))))

(defun w3m--download-sentinel (proc event)
  "This function is called when a download process 'changes state'.
Reference `set-process-sentinel'."
  (let ((buf (process-buffer proc)))
   (with-current-buffer buf
     (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (concat "\n" event))
      (cond
       ((string-match "^open" event) t)
       ((string-match "^finished" event)
         ;; TODO: Maybe keep buffer open if there was an error in
         ;; performing the metadata tagging?
         (shell-command (concat "mv " w3m--download-save-path "{.PART,}"))
         (w3m--download-apply-metadata-tags)
         (w3m--message t t "Download completed successfully.")
         (setq w3m--download-processes-list
           (assq-delete-all proc w3m--download-processes-list))
         (kill-buffer buf))
       ((string-match
           "\\(deleted\\)\\|\\(terminated\\)\\|\\(interrupt\\)\\|\\(killed\\)"
           event)
         (insert (format "\nDownload encountered '%s' event."
                         (substring event 0 -1)))
         (setq w3m--download-processes-list
           (assq-delete-all proc w3m--download-processes-list)))
       (t
         (w3m--message t 'w3m-error
            "Possible failed download. Check buffer %s for details." buf)
         (insert (format "\nPossible failed download. Encountered event '%s'.\n
 IMPORTANT: wget ocassionally reports errors even though it
 it really has successfully performed its download, so it's
 always a good idea to check the file itself. Note that if
 the file exists, it likely has an extension \".PART\" added
 to it, indicating a possible incomplete download.\n\n"
                   (substring event 0 -1)))
         (w3m--download-apply-metadata-tags)
         (setq w3m--download-processes-list
           (assq-delete-all proc w3m--download-processes-list))))))))

(defun w3m--download-process-filter (proc input-string)
  "Download buffers should handle 'carriage return' characters.
`wget' sends a \r at the beginning of every progress message in
order to over-write its prior message. "
  (let ((proc-buf (process-buffer proc)))
   (when (buffer-live-p proc-buf)
     (with-current-buffer proc-buf
       (let ((inhibit-read-only t))
        ;; FIXME: Selecting and copying text from this buffer doesn't work
        ;; while the process is active. What is copied to the kill ring is
        ;; always the value of `input-string'. `save-excursion' only helps
        ;; us by giving us the ability to move `point'.
        (save-excursion
          (goto-char (point-max))
          (if (not (string= "\r" (substring input-string 0 1)))
            (insert input-string)
           (forward-line 0)
           (delete-region (point) (point-max))
           (insert (substring input-string 1)))))))))

(defun w3m--download-using-wget (url save-path resume no-cache metadata)
  (when (not (when (not no-cache)
               (w3m--download-check-and-use-cache url save-path metadata)))
    (let ((buf (generate-new-buffer "*w3m-download*")))
     (with-current-buffer buf
       (insert (format "emacs-w3m download log\n
    Killing this buffer will abort the download!\n
Time: %s\nURL : %s\nExec: %s%s %s %s %s\n\n"
                 (current-time-string) url
                 "wget" (if resume " -c" " ") "-O" save-path url))
       (setq buffer-read-only t)
       (setq w3m--download-save-path save-path)
       (setq w3m--download-local-proc
         (apply 'start-process "w3m-download" buf
           (delq nil (list "wget" (if resume "-c")
                           "-O" (concat save-path ".PART") url))))
       (set-process-filter w3m--download-local-proc
                           'w3m--download-process-filter)
       (setq w3m--download-metadata-operation metadata)
       (push (cons w3m--download-local-proc buf) w3m--download-processes-list)
       (add-hook 'kill-buffer-hook 'w3m--download-kill-associated-process nil t)
       (set-process-sentinel w3m--download-local-proc 'w3m--download-sentinel)
       (goto-char (point-max)))
     (w3m--message t t "Requesting download."))))

(defun w3m--download-validate-basename (url &optional verbose)
  "Returns a valid basename."
  ;; TODO: Check the project codebase to see if this is duplicated anywhere
  (let ((basename (file-name-nondirectory (w3m-url-strip-query url))))
    (when (string-match "^[\t ]*$" basename)
      (when (string-match
              "^[\t ]*$"
              (setq basename (file-name-nondirectory url)))
        (when verbose
          (w3m--message t 'w3m-warning
            "Undefined file-name. Saving as \'index.html\'")
          (sit-for 2))
        (setq basename "index.html")))
    basename))



;;; Interactive and user-facing functions

(defun w3m-download-this-url ()
  "Download the file or the page pointed to by the link under point."
  (interactive)
  (let ((url (or (w3m-anchor) (w3m-image))) act)
    (cond
     ((w3m-url-valid url)
      (let ((pos (point-marker))
            (curl w3m-current-url))
        (w3m-process-with-null-handler
          (w3m-process-do
              (success (w3m-download url nil nil handler))
            (and success
                 (buffer-name (marker-buffer pos))
                 (with-current-buffer (marker-buffer pos)
                   (when (equal curl w3m-current-url)
                     (goto-char pos)
                     (w3m-refontify-anchor))))))))
     ((setq act (w3m-action))
      (let ((w3m-form-download t))
        (eval act)))
     (t
      (w3m--message t 'w3m-error "No URL at point")))))

(defun w3m-download-this-image ()
  "Download the image under point."
  (interactive)
  (let ((url (w3m-image)) act)
    (cond
     ((w3m-url-valid url)
      (let ((pos (point-marker))
            (curl w3m-current-url))
        (w3m-process-with-null-handler
          (w3m-process-do
              (success (w3m-download url nil nil handler))
            (and success
                 (buffer-name (marker-buffer pos))
                 (with-current-buffer (marker-buffer pos)
                   (when (equal curl w3m-current-url)
                     (goto-char pos)
                     (w3m-refontify-anchor))))))))
     ((setq act (w3m-action))
      (let ((w3m-form-download t))
        (eval act)))
     (t
      (w3m--message t 'w3m-error "No image at point")))))

(defun w3m-save-image ()
  "Save the image under point to a file.
The default name will be the original name of the image."
  (interactive)
  (let ((url (w3m-url-valid (w3m-image))))
    (if url
        (w3m-download url)
      (w3m--message t 'w3m-error "No image at point"))))

(defun w3m-download-delete-all-download-buffers ()
  "Delete all `w3m-download' buffers.

Be careful when using this function. It will kill any active `wget'
downloads in progress."
  (interactive)
  (let ((bufs (buffer-list))
         buf)
    (while (setq buf (pop bufs))
      (when (string-match "^\\*w3m-download" (buffer-name buf))
        (kill-buffer buf)))))

(defalias 'w3m-download-kill-all-wget-processes
  'w3m-download-delete-all-download-buffers
  "Aborts all current downloads using `wget'.

This is an alias for `w3m-download-delete-all-download-buffers',
so expect the buffers to be deleted also.")



;;;###autoload
(defun w3m-download-using-wget (url &optional save-path no-cache interactive)
  "Download URL to `w3m-default-save-directory'.
With prefix argument, prompt for an alternate SAVE-PATH,
including alternate file name.

This function uses the external download program `wget', and
indicates the download progress in a dedicated buffer, which is
deleted upon success.

Additionally, for certain downloads, if variable
`w3m-download-save-metadata' is non-nil, then certain metadata
will be attached to the file."
  (interactive (list (w3m-active-region-or-url-at-point) nil nil t))
  (let* (basename extension metadata caption
        (num-in-progress (length w3m--download-processes-list))
        (others-in-progress-prompt
          (if (zerop num-in-progress) ""
           (format "(%d other download%s in progress)"
                   num-in-progress
                   (if (= 1 num-in-progress) "" "s"))))
        (download-prompt
          (concat "Download URL"
                  (if (zerop num-in-progress) ""
                   (concat "(" others-in-progress-prompt ")"))
                  ": "))
        (resume-prompt
          (format "%s%s"
            (if (zerop num-in-progress) ""
             (concat others-in-progress-prompt "\n"))
            "File(%s) already exists.
Are you trying to resume an aborted partial download? ")))
    (unless url
      (while (string-equal ""
               (setq url (w3m-input-url download-prompt nil
                           "" nil nil 'no-initial)))
        (w3m--message t 'w3m-error "A url is required")
        (sit-for 1)))
    (setq url (w3m-url-decode-string url))
    (when current-prefix-arg
      (setq basename
        (w3m--download-validate-basename url))
      (setq save-path
        (w3m-read-file-name
          (format "Download %s to: " url)
          w3m-default-save-directory
          basename)))
    (setq save-path
      (expand-file-name
        (w3m--download-validate-basename (or save-path url) t)
        (if save-path
          (file-name-directory save-path)
         w3m-default-save-directory)))
    (when (and w3m-download-save-metadata
              (setq caption (w3m-image-alt))
              (setq extension (downcase (file-name-extension save-path))))
      (setq metadata
        (cond
         ((and (string= "png" extension) (executable-find "exiv2"))
          (format "exiv2 -M\"add Exif.Image.ImageDescription %s\" %s"
                  caption save-path))
         ((and (string-match "^jpe?g$" extension) (executable-find "exif"))
          (format "exif --create-exif --ifd=0 -t0x10e \
                   --set-value=\"%s\" --output=\"%s\" %s"
                  caption save-path save-path))
         (t nil))))
    (if (and (not (file-exists-p save-path))
             (not (file-exists-p (concat save-path ".PART"))))
      (w3m--download-using-wget url save-path nil no-cache metadata)
     (cond
      ((or (not interactive)
           (y-or-n-p (format resume-prompt save-path)))
       (w3m--download-using-wget url save-path t no-cache metadata))
      ((y-or-n-p (format "Overwrite? (%s)" save-path))
       (w3m--download-using-wget url save-path nil no-cache metadata))))))

;;;###autoload
(defun w3m-download-using-w3m (url
                               &optional filename no-cache handler post-data)
  "Download contents of URL to a file named FILENAME.
NO-CACHE (which the prefix argument gives when called interactively)
specifies not using the cached data."
  (interactive (list nil nil current-prefix-arg))
  (unless url
    (while (string-equal ""
             (setq url (w3m-input-url
                         "Download URL: " nil
                         (or (w3m-active-region-or-url-at-point) "")
                         nil nil 'no-initial)))
      (w3m--message t 'w3m-error "A url is required")
      (sit-for 1)))
  (setq url (w3m-url-decode-string url))
  (unless filename
    (let ((basename (w3m--download-validate-basename url)))
      (setq filename
            (w3m-read-file-name (format "Download %s to: " url)
                                w3m-default-save-directory basename))))
  (if (and w3m-use-ange-ftp (string-match "\\`ftp://" url))
      (w3m-goto-ftp-url url filename)
    (let ((page-buffer (current-buffer)))
      (w3m-process-do-with-temp-buffer
          (type (progn
                  (w3m-clear-local-variables)
                  (setq w3m-current-url url)
                  (w3m-retrieve url t no-cache post-data nil handler)))
        (if type
            (let ((buffer-file-coding-system 'binary)
                  (coding-system-for-write 'binary)
                  jka-compr-compression-info-list
                  format-alist)
              (when (or (not (file-exists-p filename))
                        (prog1 (y-or-n-p
                                (format "File(%s) already exists. Overwrite? "
                                        filename))
                          (message nil)))
                (write-region (point-min) (point-max) filename)
                (w3m-touch-file filename (w3m-last-modified url))
                t))
          (ding)
          (with-current-buffer page-buffer
            (w3m--message t 'w3m-error "Cannot retrieve URL: %s%s" url
                     (cond ((and w3m-process-exit-status
                                 (not (equal w3m-process-exit-status 0)))
                            (format " (exit status: %s)"
                                    w3m-process-exit-status))
                           (w3m-http-status
                            (format " (http status: %s)" w3m-http-status))
                           (t ""))))
          nil)))))

;;;###autoload
(defun w3m-download (url
                     &optional filename no-cache handler post-data interactive)
  (interactive
    (cond
     ((executable-find "wget")
      (list (w3m-active-region-or-url-at-point)
            nil current-prefix-arg nil nil t))
     (t
      (list nil nil current-prefix-arg))))
  (if (executable-find "wget")
    (w3m-download-using-wget url filename no-cache interactive)
   (w3m-download-using-w3m url filename no-cache handler post-data)))



;;; Provide this feature
(provide 'w3m-download)

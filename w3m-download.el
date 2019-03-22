;;; w3m-download.el --- download using emacs-w3m -*- coding: utf-8 ; lexical-binding: t -*-

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
;;   4) Bulk downloading of a buffer's links or region's links,
;;      selectable by regex and a presentation buffer. Throughout
;;      the code, this feature is referred to as `download-select'.
;;
;;   5) The number of simultaneous downloads may be controlled, and
;;      can be dynamically changed.
;;
;;   6) Downloads are queued, and the queue can be modified in
;;      real-time.
;;
;;   7) Optional appending of meta-data to a download.
;;
;;      7.1) Defcustom `w3m-download-save-metadata' controls
;;           whether downloaded `png' and `jpeg' files should
;;           have their link's "alt" caption is stored as
;;           meta-data element "Exif.Image.ImageDescription".
;;           Note that enabling this option will modify the
;;           file's checksum. This option currently uses `exif'
;;           and `exiv2' as external back-end programs.
;;
;;           Some ways to view from the command-line an embedded
;;           caption are:
;;             exiv2 -g "Exif.Image.ImageDescription" foo.png
;;             exif --ifd=0 -t0x010e  foo.jpg |grep value
;;
;;      7.2) Defcustom `w3m-download-enable-xattr' controls
;;           whether to save a file's original URL and Referer
;;           HTTP header value. This feature uses the `wget'
;;           `--xattr' argument and thus requires that the
;;           save-path be on a file-system that supports extended
;;           attributes.
;;
;;           Be aware that the URL might contain private
;;           information like access tokens or credentials.
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



;;; Usage:

;; For downloading single items at point, there exist multiple
;; functions that seem redundant:
;;
;;     `w3m-download-this-url'
;;     `w3m-download-this-image'
;;     `w3m-save-image'
;;     `w3m-download-using-wget'
;;     `w3m-download-using-w3m'   (not recommended)
;;     `w3m-download'
;;
;; For downloading more than one link in a buffer or region:
;;
;;     `w3m-download-select'
;;
;; At any time, you can change the number of allowable
;; simultaneous downloads:
;;
;;     `w3m-download-increase-simultaneous'
;;     `w3m-download-decrease-simultaneous'
;;
;;     This is easiest done from a w3m-download-select buffer,
;;     where the functions are bound to the +/- keys.
;;
;; The current recommended way to kill a download in progress is
;; to kill its progress buffer. To kill all current downloads:
;;
;;     `w3m-download-delete-all-download-buffers'
;;     `w3m-download-kill-all-wget-processes'      (just an alias)
;;
;;     There also exists for now a slightly different function
;;     that was originally intended as a possible hook function:
;;
;;         `w3m-download-kill-all-asociated-processes'
;;
;; The download queue can be examined in a special buffer that allows
;; one to delete or change the order of items:
;;
;;     `w3m-download-view-queue'



;;; TODO:

;; + Easier way to find a particular download to kill
;; + function: w3m-download-select-exec
;;   + pre-screen the list of downloads for possible name conflicts
;;     with existing files, and duplicates, and prompt the user
;;     accordingly.
;;     + maybe do this instead when creating the select buffer, ie.
;;       gray out entries that already exist on the queue
;; + pause download items
;; + `w3m--download-check-and-use-cache'
;;   + does not emulate `w3m-download-enable-xattr'
;; + danger of 'feature-creep': None of these-items are necessary, and
;;   at some point the project should consider turning to a en external
;;   program which specializes in downlaoding, but anyway ...
;;   + an alist of extensions and metadata commands
;;   + an alist of download commands (eg. aria2, curl, axel)
;;   + offloading to a dedicated downloader (eg. uget)
;;   + persistence across crashes (ie. maintain a disk file)
;;   + warn if more than 'n' concurrent downloads in progress
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
;;   + torrent and magnet support
;;     + maybe do this using the type-alist for external helper
;;       programs?
;;   + Think about the need / desirability of hook functions to
;;     optionally kill running downloads, for `kill-emacs-hook'
;;     (without user interaction), `kill-emacs-query-functions'
;;     (with user interaction), and the mode exit hook for
;;     `emacs-w3m' (look it up, what is it?). Maybe there should
;;     also be download recovery/resume/re-attach hooks for either
;;     `after-init-hook', `emacs-startup-hook' or the startup hook
;;     for `emacs-w3m' (look it up, what is it?)



;;; Code:

;;; Dependencies:
(require 'w3m-util)



;;; Temporary compatability operation(s):

;; My development git fork has a different messaging standard, based
;; upon a pending pull request from my branch `bb_messaging', so
;; support this git branch being merged into a branch lacking that
;; other merge.
(eval-when-compile
  (when (not (fboundp 'w3m--message))
    (defun w3m--message (timeout face &rest args)
      (apply 'w3m-message args))))



;;; Global constants:

(defconst w3m--download-mutex (make-mutex "w3m-download")
  "Control manipulation of `w3m--download-queue'.")



;;; Global variables:

(defvar w3m-download-select-mode-map nil
  "Major mode for bulk download of links in a buffer or region.")
(unless w3m-download-select-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map [? ]        'w3m-download-select-toggle-line); [? ] = <SPC>
    (define-key map "\C-k"      'w3m-download-delete-line)
    (define-key map "q"         'w3m-download-buffer-quit)
    (define-key map "Q"         'w3m-download-buffer-quit)
    (define-key map "\C-c\C-k"  'w3m-download-buffer-quit)
    (define-key map "\C-c\C-c"  'w3m-download-select-exec)
    (define-key map "+"         'w3m-download-increase-simultaneous)
    (define-key map "-"         'w3m-download-decrease-simultaneous)
    (setq w3m-download-select-mode-map map)))

(defvar w3m-download-queue-mode-map nil
  "Major mode for viewing and editing the w3m-download queue.")
(unless w3m-download-queue-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "\C-k"      'w3m-download-delete-line)
    (define-key map "q"         'w3m-download-buffer-quit)
    (define-key map "Q"         'w3m-download-buffer-quit)
    (define-key map "\C-c\C-k"  'w3m-download-buffer-quit)
    (define-key map "\C-c\C-n"  'w3m-download-queue-drop)
    (define-key map "\C-c\C-p"  'w3m-download-queue-raise)
    (define-key map "+"         'w3m-download-increase-simultaneous)
    (define-key map "-"         'w3m-download-decrease-simultaneous)
    (setq w3m-download-queue-mode-map map)))

(defvar w3m--download-queue nil
  "List of pending downloads.
Each element is a list of four elements:

1. URL - The link to download, as a string.

2. WGET-CMD-LIST - the complete wget command line, as list of
   strings suitable for function `start-process'.

3. SAVE-PATH - the fully qualified path to place the download.

4. METADATA - the complete command line for tagging the download
   with metadata, as a string sutiable for function
   `shell-command'.")

(defvar w3m--download-processes-list nil
  "Global list of all running `w3m-download' processes.")

(defvar w3m--download-select-filter-history nil
  "Record of past download-select filter use.
Needed for function `completing-read' to be able to display the
most recently used.")


;;; User-options:

(defcustom w3m-download-max-simultaneous 4
  "Maximum number of simultaneous downloads."
  :group 'w3m
  :type 'integer)

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

(defcustom w3m-download-enable-xattr nil
"Save extended attributes when using wget for downloading.

This uses the `wget' `--xattr' argument, to enable use of file a
system's extended attributes to save the original URL and the
Referer HTTP header value if used. Be aware that the URL might
contain private information like access tokens or credentials."
  :group 'w3m
  :type 'boolean)

(defcustom w3m-download-select-filter-list
  '("all_links: .*$"
    "e-books:   \\.\\(pdf\\)\\|\\(epub\\)\\|\\(mobi\\)$"
    "text:      \\.txt$"
    "images:    \\.\\(png\\)\\|\\(jpe?g\\)\\|\\(gif\\)$"
    "audio:     \\.\\(og[ag]\\)\\|\\(mp3\\)$"
    "video:     \\.\\(ogv\\)\\|\\(mp4\\)$")
  "Regex patterns for filtering downloads.
Each regex string may begin with an optional single descriptive word
ending with a colon `:', followed by the regex for the filter."
  :group 'w3m
  :type '(repeat (string :format "%v\n")))



;;; Faces:

(defface w3m-download-selected
  '((t :inherit 'w3m-session-select))
  "Face of URL selected for download in w3m-download-select
buffer."
  :group 'w3m)

(defface w3m-download-current-line
  '((t :inherit 'w3m-bold))
  "Face of current line in w3m-download-select buffer."
  :group 'w3m)

(defface w3m-download-selected-current-line
  '((t :weight bold :inherit 'w3m-session-select))
  "Face of current line in w3m-download-select
buffer when that URL is selected for download."
  :group 'w3m)



;;; Buffer-local variables:

(defvar-local w3m--download-metadata-operation nil
  "Text string of the complete shell command to be used to tag a
  download. It is set buffer-local in the download's progress
  buffer.")

(defvar-local w3m--download-local-proc nil
  "The process id of the download associated with the current
  download progress buffer.")

(defvar-local w3m--download-save-path nil
  "The full path-name of the downloaded file.")


;;; Hook functions:

(defun w3m--download-kill-associated-process ()
  "Hook function for `kill-buffer-hook' for w3m-download buffers.
`w3m--download-local-proc' should have been set as a local
variable at buffer creation."
  (if (not (processp w3m--download-local-proc))
    (w3m--message t 'w3m-warning
                  "Warning: no process found to kill (w3m-download).")
   (delete-process w3m--download-local-proc)
   (setq w3m--download-processes-list
     (assq-delete-all w3m--download-local-proc w3m--download-processes-list)))
   (when (and w3m--download-queue
              (>= w3m-download-max-simultaneous
                  (length w3m--download-processes-list)))
     (w3m--download-from-queue)))

(defun w3m--download-update-statistics ()
  "A hook function for `w3m-download-select' and `w3m-download-queue' buffers.
Meant for use with `post-command-hook'."
  (save-excursion
    (let ((mode (if (eq major-mode 'w3m-download-select-mode)
                  "Selected" "Queued"))
          (num-selected 0)
          (inhibit-read-only t))
      (goto-char (point-min))
      (while (re-search-forward
                (if (eq major-mode 'w3m-download-select-mode)
                  "^\\[X" "^Q")
                nil t)
        (setq num-selected (1+ num-selected)))
      (goto-char (point-min))
      (when (re-search-forward "^>.*$" nil t)
        (replace-match
          (format "> %d %s; %d maximum simultaneously"
                  num-selected
                  mode
                  w3m-download-max-simultaneous))
        (put-text-property (point-min) (+ 3 (point)) 'cursor-intangible t)))))

(defun w3m--download-update-faces ()
  "A hook function for `w3m-download-select' and `w3m-download-queue' buffers.
Meant for use with `pre-command-hook' and `post-command-hook'."
  (let* ((beg (line-beginning-position))
         (end (line-beginning-position 2))
         (this-face (get-text-property beg 'face))
         (not-done t)
         (inhibit-read-only t))
   (put-text-property beg end 'face
     (cond
      ((equal this-face 'w3m-download-selected-current-line)
        'w3m-download-selected)
      ((equal this-face 'w3m-download-current-line)
        'default)
      ((equal this-face 'w3m-download-selected)
      'w3m-download-selected-current-line)
      ((equal this-face 'default)
       (if (or (= (char-after beg) 91)
               ;; for download-select buffer,
               ;; char 91 = "[". I'm not using ?[
               ;; because it messes up check-parens etc.
               (= (char-after beg) ?Q))
               ;; for download-queue buffer.
         'w3m-download-current-line
        'default))
      (t 'default)))))



;;; Internal functions:

(defun w3m-download-select-mode ()
  "Major mode for bulk download of links in a buffer or region.

\\<w3m-download-select-mode-map>
\\[w3m-download-select-toggle-line]\tToggle current line's status.
\\[w3m-download-delete-line]\tDelete current line.
\\[w3m-download-increase-simultaneous]\tIncrease numner of parallel downloads.
\\[w3m-download-decrease-simultaneous]\tDecrease number of parralel downloads.
\\[w3m-download-buffer-quit]\tQuit.
\\[w3m-download-select-exec]\tPerform the downloading.\n\n"
     (setq
       mode-name "w3m download select"
  	   truncate-lines t
	     major-mode 'w3m-download-select-mode
	     buffer-read-only t)
     (cursor-intangible-mode)
     (use-local-map w3m-download-select-mode-map)
     (add-hook 'pre-command-hook  'w3m--download-update-faces t t)
     (add-hook 'post-command-hook 'w3m--download-update-faces t t)
     (add-hook 'post-command-hook 'w3m--download-update-statistics t t))

(defun w3m-download-queue-mode ()
  "Major mode for viewing and editing the w3m-download queue.

\\<w3m-download-queue-mode-map>
\\[w3m-download-queue-drop]\tMove current line down the queue.
\\[w3m-download-queue-raise]\tMove current line up the queue.
\\[w3m-download-delete-line]\tDelete current line.
\\[w3m-download-increase-simultaneous]\tIncrease numner of parallel downloads.
\\[w3m-download-decrease-simultaneous]\tDecrease number of parralel downloads.
\\[w3m-download-buffer-quit]\tQuit.\n\n"
  ; Note that this mode shares functions with `w3m-download-select-mode'.
     (setq
       mode-name "w3m download queue"
       truncate-lines t
       major-mode 'w3m-download-queue-mode
       buffer-read-only t)
     (cursor-intangible-mode)
     (use-local-map w3m-download-queue-mode-map)
     (add-hook 'pre-command-hook  'w3m--download-update-faces t t)
     (add-hook 'post-command-hook 'w3m--download-update-faces t t)
     (add-hook 'post-command-hook 'w3m--download-update-statistics t t))

(defun w3m--download-queue-adjust (direction)
  "Change the current entry's position in the w3m-download queue.
DIRECTION is either 'raise or 'drop. This function is called by
the interactive functions `w3m-download-queue-raise' and
`w3m-download-queue-raise'."
  (if (not (eq major-mode 'w3m-download-queue-mode))
    (w3m--message t 'w3m-error
      "This command is available only in w3m-download-queue buffers.")
   (let ((inhibit-read-only t)
         (current-column (current-column))
         (url (buffer-substring-no-properties
                (+ 2 (line-beginning-position))
                (1- (line-beginning-position 2))))
         queue elem prior result)
    ; modify the queue
    (with-mutex w3m--download-mutex
      (setq queue w3m--download-queue)
      (cond
       ((eq direction 'raise)
             (while queue
               (cond
                ((equal url (car (setq elem (pop queue))))
                 (push elem result))
                (t
                 (push prior result)
                 (setq prior elem)))))
       ((eq direction 'drop)
             (while queue
               (cond
                ((equal url (car (setq elem (pop queue))))
                 (push prior result)
                 (setq prior elem))
                (t
                 (push elem result)
                 (when prior
                   (push prior result)
                   (setq prior nil)))))))
      (push prior result)
      (setq w3m--download-queue (reverse (delq nil result))))
    ; redisplay the queue elements
    (w3m--download-display-queue-list url current-column nil))))

(defun w3m--download-display-queue-list (url current-column current-line)
  "Display the download queue.
Exactly one of URL or CURRENT-LINE must be NIL. A CURRENT-COLUMN
NIL is interpreted as a 0. This function is meant to be called by
`w3m-download-delete-line' and `w3m--download-queue-adjust'."
  (goto-char (point-min))
  (re-search-forward "^>" nil t)
  (forward-line 2)
  (delete-region (point) (point-max))
  (dolist (entry w3m--download-queue)
    (setq pos (point))
    (insert "\nQ " (nth 0 entry)) ; url
    (put-text-property pos (point) 'face 'default))
  (w3m--download-update-statistics)
  (goto-char (point-min))
  (cond
   (url
    (re-search-forward url)
    (forward-line 0))
   (current-line
    (forward-line current-line)))
  (forward-char (or current-column 0))
; (w3m--download-update-faces)
  )

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
  "If URL exists in the cache, use that copy.

This function saves the copy to the fully-qualified path
SAVE-PATH, and optionally adds the metadata using the command
string found in METADATA, without any need for `wget' or creating
tracking buffers, etc."
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

(defun w3m--download-sentinel (proc event)
  "Called by Emacs when `w3m-download' process PROC 'changes state' to EVENT.
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
  "Parse output from `wget'.
This function is called by Emacs whenever `w3m-download' process
PROC sends INPUT-STRING to its STDOUT. It translates 'carriage
return' characters. \r at the beginning of every progress message
into the equivalent of a real carriage return in order to
over-write the prior progress message."
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

(defun w3m--download--cache-or-queue (url save-path resume no-cache metadata)
  "Either prepare a `wget' download or save URL from the `emacs-w3m' cache.

With NO-CACHE nil, if URL exists in the cache it is saved from there,
avoiding any further download action.

Otherwise, use URL, SAVE-PATH, RESUME, and METADATA to add the
download's information to the end of `w3m--download-queue' and
try to perform a download from the head of the queue.

RESUME is a boolean used to tell `wget' to continue an aborted partial
download.

If METADATA is non-nil, it should be a shell command to be used
to add metadata to SAVE-PATH."
  (when (not (when (not no-cache)
               (w3m--download-check-and-use-cache url save-path metadata)))
    (with-mutex w3m--download-mutex
      (add-to-list 'w3m--download-queue
        (list
          url
          (delq nil (list "wget"
                    (if resume "-c")
                    (if w3m-download-enable-xattr "--xattr")
                    "-O" (concat save-path ".PART") url))
          save-path
          metadata)
       t)) ; t = append to end of list
    (w3m--download-from-queue)))

(defun w3m--download-from-queue ()
  "Set-up a download progress buffer and begin a download process."
   (while (and w3m--download-queue
              (>  w3m-download-max-simultaneous
                  (length w3m--download-processes-list)))
    (let* ((buf (generate-new-buffer "*w3m-download*"))
           (job (with-mutex w3m--download-mutex
                  (pop w3m--download-queue)))
           (url       (nth 0 job))
           (wget-cmd  (nth 1 job))
           (save-path (nth 2 job))
           (metadata  (nth 3 job)))
     (with-current-buffer buf
       (insert (format "emacs-w3m download log\n
    Killing this buffer will abort the download!\n
Time: %s\nURL : %s\nExec: %s\n\n"
                 (current-time-string) url wget-cmd))
       (setq buffer-read-only t)
       (setq w3m--download-save-path save-path)
       (setq w3m--download-local-proc
         (apply 'start-process "w3m-download" buf wget-cmd))
       (set-process-filter w3m--download-local-proc
                           'w3m--download-process-filter)
       (setq w3m--download-metadata-operation metadata)
       (push (cons w3m--download-local-proc buf) w3m--download-processes-list)
       (add-hook 'kill-buffer-hook 'w3m--download-kill-associated-process nil t)
       (set-process-sentinel w3m--download-local-proc 'w3m--download-sentinel)
       (goto-char (point-max)))
     (w3m--message t t "Requesting download."))))

(defun w3m--download-validate-basename (url &optional verbose)
  "Return a valid basename, based upon URL.
With VERBOSE non-nil, send warning messages to the user."
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



;;; Interactive and user-facing functions:

(defun w3m-download-kill-all-asociated-processes (&optional arg)
  "Kill all `w3m-download' processes and progress buffers.

With the optional prefix-argument ARG, try a hackish alternative
method.

This also empties the download queue.

This function was originally envisioned as a hook function to
possibly call when exiting `emacs-w3m'. It is thus quite similar
to function `w3m-download-delete-all-download-buffers'."
;; TODO: Double check that if emacs crashes during a download, the
;; wget sub-processes also die. If for some reason not, we could
;; programmatically find the processes and possibly re-attach their
;; STDOUT/STDERR to a new tracking buffer, or we could kill them.
  (interactive "p")
  (setq w3m--download-queue nil) ; without using mutex
  (cond
   ((not arg)
    (let ((kill-buffer-query-functions nil)
          x)
      (while (setq x (pop w3m--download-processes-list))
        (when (processp (car x))
          (delete-process (car x)))
        (when (bufferp (cdr x))
          (kill-buffer (cdr x))))))
   (t
     (let ((kill-buffer-query-functions nil)
           (procs (process-list))
           (bufs  (buffer-list)))
       (dolist (proc procs)
         (when (string-match "w3m-download" (process-name proc))
           (delete-process proc)))
       (dolist (buf bufs)
         (when (string-match "w3m-download" (buffer-name buf))
           (kill-buffer buf)))))))

(defun w3m-download-queue-drop ()
  "Move the curent entry down one position in the queue.
This means it will be downloaded later."
  (interactive)
  (w3m--download-queue-adjust 'drop))

(defun w3m-download-queue-raise ()
  "Move the curent entry up one position in the queue.
This means it will be downloaded sooner."
  (interactive)
  (w3m--download-queue-adjust 'raise))

(defun w3m-download-view-queue ()
  "View the w3m-download queue and allow editing it."
  (interactive)
  (if (not w3m--download-queue)
    (w3m--message t 'w3m-error "Download queue is empty. Nothing to display.")
   (with-current-buffer
     (setq buf (get-buffer-create "*w3m-download-queue*"))
     (w3m-download-queue-mode)
     (let ((inhibit-read-only t) pos)
       (insert "  w3m-download-queue buffer\n
This buffer displays only the downloads not yet started.
  +/-      Adjust maximum number of simultaneous downloads.
  C-C C-n  Move an entry line down the queue (later download)
  C-c C-p  Move an entry line up the queue (sooner download)
  C-k      Delete an entry line
  q        Close this buffer (or just kill the buffer)\n\n\n>\n\n")
       (put-text-property (point-min) (point) 'cursor-intangible t)
       (dolist (entry w3m--download-queue)
         (setq pos (point))
         (insert "\nQ " (nth 0 entry)) ; url
         (put-text-property pos (point) 'face 'default))
       (w3m--download-update-statistics)))
   (switch-to-buffer buf)
   (goto-char (point-min))
   (re-search-forward "^\\Q")
   (w3m--download-update-faces)))

(defun w3m-download-select-exec ()
  "Initiate a bulk download from the download-select buffer."
  (interactive)
  (if (not (eq major-mode 'w3m-download-select-mode))
    (w3m--message t 'w3m-error
      "This command is available only in w3m-download-select buffers.")
   (let (urls)
     (goto-char (point-min))
     (while (re-search-forward "^\\[X\\] \\(.*\\)$" nil t)
       (push (match-string 1) urls))
     (dolist (url urls)
       (w3m-download-using-wget url)))
   (kill-buffer (current-buffer))))

(defun w3m-download-increase-simultaneous (&optional inc)
  "Adjust variable `w3m-download-max-simultaneous'.

Increment by one, or by the predix argument INC, the maximum
number of simultaneous w3m-downloads. A negative prefix argument
would decrement the value, but you could just use the other
function `w3m-download-decrease-simultaneous'. This variable and
functiononly applies to downloads using `wget', not those using
`w3m'."
  (interactive "p")
  (setq w3m-download-max-simultaneous
    (max 1 (+ w3m-download-max-simultaneous
              (or inc 1))))
  (w3m--message t t "Maximum simultaneous downloads now set to %s."
                    w3m-download-max-simultaneous))

(defun w3m-download-decrease-simultaneous (&optional dec)
  "Adjust variable `w3m-download-max-simultaneous'.

Decrement by one, or by the predix argument DEC, the maximum
number of simultaneous w3m-downloads. A negative prefix argument
would increment the value, but you could just use the other
function `w3m-download-increase-simultaneous'. This variable and
functiononly applies to downloads using `wget', not those using
`w3m'."
  (interactive "p")
  (setq w3m-download-max-simultaneous
    (max 1 (- w3m-download-max-simultaneous
              (or dec 1))))
  (w3m--message t t "Maximum simultaneous downloads now set to %s."
                    w3m-download-max-simultaneous))

(defun w3m-download-buffer-quit ()
  "Exit from w3m download select mode."
  (interactive)
  (if (and (not (eq major-mode 'w3m-download-select-mode))
           (not (eq major-mode 'w3m-download-queue-mode)))
    (w3m--message t 'w3m-error
      "This command is available only in w3m-download buffers.")
   (kill-buffer (current-buffer))))

(defun w3m-download-delete-line ()
  "Delete the current URL entry.
Used in `w3m-download-select' and `w3m-download-queue' buffers."
  (interactive)
  (cond
   ((eq major-mode 'w3m-download-queue-mode)
    (let ((inhibit-read-only t)
          (current-column (current-column))
          (current-line (1- (string-to-number (format-mode-line "%l"))))
          (url (buffer-substring-no-properties
                 (+ 2 (line-beginning-position))
                 (1- (line-beginning-position 2)))))
     (with-mutex w3m--download-mutex
       (setq w3m--download-queue
         (delq (assoc url w3m--download-queue) w3m--download-queue)))
     (w3m--download-display-queue-list nil current-column current-line)))
   ((eq major-mode 'w3m-download-select-mode)
    (let ((inhibit-read-only t))
     (delete-region
       (line-beginning-position)
       (line-beginning-position 2))))
   (t
    (w3m--message t 'w3m-error
      "This command is available only in w3m-download buffers."))))

(defun w3m-download-select-toggle-line ()
  "Change selection status of the current URL entry.
Used in w3m download select mode buffers."
  (interactive)
  (if (not (eq major-mode 'w3m-download-select-mode))
    (w3m--message t 'w3m-error
      "This command is available only in w3m-download-select buffers.")
   (save-excursion
     (let* ((beg (line-beginning-position))
            (end (line-beginning-position 2))
            (inhibit-read-only t))
       (when (= (char-after beg) 91)
                ;; char 91 = "[". I'm not using ?[
                ;; because it messes up check-parens etc.
         (goto-char (1+ beg))
         (insert (if (= (char-after) ?X) " " "X"))
         (delete-char 1)
         (put-text-property beg end 'face
            (if (= (char-after (1+ beg)) ?X)
              ;; These face assignments only seem wrong until you
              ;; remember that the pre- and post- command hooks will
              ;; immediately change them
              'w3m-download-selected
             'default)))))))

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
downloads in progress, and empty the download queue."
  (interactive)
  (setq w3m--download-queue nil) ; without using mutex
  (let ((bufs (buffer-list))
        (kill-buffer-query-functions nil))
    (dolist (buf bufs)
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
will be attached to the file.

NO-CACHE is a boolean value for using the emacs-w3m cache.

INTERACTIVE is a boolean value for whether the calling function
was itself interactive and thus whether the user may be prompted for
further information."
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
      (w3m--download--cache-or-queue url save-path nil no-cache metadata)
     (cond
      ((or (not interactive)
           (y-or-n-p (format resume-prompt save-path)))
       (w3m--download--cache-or-queue url save-path t no-cache metadata))
      ((y-or-n-p (format "Overwrite(%s)? " save-path))
       (w3m--download--cache-or-queueg url save-path nil no-cache metadata))))))

;;;###autoload
(defun w3m-download-using-w3m (url
                               &optional filename no-cache handler post-data)
  "Download contents of URL to a file named FILENAME.
NO-CACHE (which the prefix argument gives when called interactively)
specifies not using the cached data.

HANDLER and POST-DATA are the standard emacs-w3m args for its
internal asynchronous process control and handling POST
requests."
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
(defun w3m-download-select (start end &optional filter no-prompt)
  "Download multiple links, by REGEX, and manual selection.

When called interactively, if a region is selected, act on that
region as START and END; otherwise act on the entire buffer.

Without the prefix argument, The user will be prompted for a
regular expression FILTER to limit the links to be downloaded
from within the scope, and can select from a collection of common
regexes, as provided by `w3m-download-select-filter-list'. A
selection buffer is then presented to allow modifying the
selection list.

With the prefix argument, FILTER is presumed to be \".*\",
NO-PROMPT is set non-nil, and all possible links are
downloaded without user interaction.

If any selected URL has been partially downloaded prior, it will
resume instead of restarting from scratch."
  (interactive
    (list
      (if (region-active-p) (region-beginning) (point-min))
      (if (region-active-p) (region-end) (point-max))
      (if current-prefix-arg ".*"
       (completing-read
         "Download filter regex: "
         (if w3m--download-select-filter-history
           (cons (car w3m--download-select-filter-history) w3m-download-select-filter-list)
          w3m-download-select-filter-list)
         nil nil nil
         'w3m--download-select-filter-history))
      current-prefix-arg))
  (let ((pos start)
        (not-done t)
        (regex (if (string-match "^[^ ]*: +" filter)
                  (substring filter (match-end 0))
                 filter))
        anchor anchor-list buf)
   (save-mark-and-excursion
     (while not-done
       (when (setq anchor (or (get-text-property pos 'w3m-href-anchor)
                              (get-text-property pos 'w3m-image)))
         ;; Remove URI fragment part, if any (.ie '#foo')
         (w3m-string-match-url-components anchor)
         (when (match-beginning 8)
           (setq anchor (substring anchor 0 (match-beginning 8))))
         (when (not (member anchor anchor-list))
           (push anchor anchor-list)))
       (if (= pos end)
         (setq not-done nil)
        (setq pos ; to next anchor or end
          ; this snippet is based upon `w3m-goto-next-anchor-or-image'
          (progn
            (cond ; currently on anchor or image
             ((w3m-anchor-sequence pos)
              (setq pos (next-single-property-change pos 'w3m-href-anchor nil end)))
             ((w3m-image pos)
              (setq pos (next-single-property-change pos 'w3m-image nil end))))
            (or (w3m-anchor-sequence pos)
                (w3m-image pos)
                (let ((image-pos (next-single-property-change pos 'w3m-image nil end)))
                  (setq pos (next-single-property-change pos 'w3m-href-anchor nil end))
                  (and image-pos
                       (or (not pos) (> pos image-pos))
                       (setq pos image-pos))))
            pos)))))
   (if (not anchor-list)
     (w3m--message t 'w3m-error "No links found in region.")
    (setq anchor-list (reverse anchor-list))
    (cond
     (no-prompt
      (dolist (anchor anchor-list)
        (when (string-match regex anchor)
          (w3m-download-using-wget anchor))))
     (t
      (with-current-buffer
        (setq buf (get-buffer-create "*w3m-download-select*"))
        (w3m-download-select-mode)
        (let ((inhibit-read-only t) pos)
          (insert "  w3m-download-select buffer\n
Review the links selected [X] for downloading.\n
  C-c C-c  Begin downloading
  <SPACE>  Toggle a link's status
  +/-      Adjust maximum number of simultaneous downloads.
  C-k      Delete an entry line
  q        Abort (or just kill the buffer)\n\n\n>\n\n")
          (put-text-property (point-min) (point) 'cursor-intangible t)
          ;; feature-creep:
          ;; + handle persistence and retries
          (dolist (anchor anchor-list)
            (setq pos (point))
            (cond
             ((string-match regex anchor)
              (insert (format "\n[X] %s" anchor))
              (put-text-property pos (point) 'face 'w3m-download-selected))
             (t
              (insert (format "\n[ ] %s" anchor))
              (put-text-property pos (point) 'face 'default))))
          (w3m--download-update-statistics)))
      (switch-to-buffer buf)
      (goto-char (point-min))
      (re-search-forward "^\\[")
      (w3m--download-update-faces))))))

;;;###autoload
(defun w3m-download (url
                     &optional filename no-cache handler post-data interactive)
  "Download a URL.

The external program `wget' is preferred if it is available. If
not, `w3m' is used.

When called interactively, URL at point is presumed. Otherwise,
the user is prompted for it. FILENAME defaults to the basename of
URL. With the prefix argument NO-CACHE is set non-nil.

HANDLER and POST-DATA are the standard emacs-w3m args for its
internal asynchronous process control and handling POST
requests.

INTERACTIVE is a boolean value for whether the calling function
was itself interactive and thus whether the user may be prompted for
further information."
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
;;; w3m-download.el ends here

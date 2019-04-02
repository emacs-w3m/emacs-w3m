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
;;   + Partial downloads are stored on disk, not in memory (yes...).
;;
;;   + Resumption of partial downloads that were interrupted due to
;;     aborts or other failures.
;;
;;   + Bulk downloading of a buffer's links or region's links,
;;     selectable by regex and a presentation buffer. Throughout the
;;     code, this feature is referred to as `download-select'.
;;
;;   + The number of simultaneous downloads may be controlled, and can
;;     be dynamically changed.
;;
;;   + Downloads are queued, and the queue can be modified in
;;     real-time.
;;
;;   + Video downloads do not need actual download links, but can be
;;     scraped from web pages. This extension requires the external
;;     program `youtube-dl' (as is not limited to youtube).
;;
;;   + Optional appending of meta-data to a download.
;;
;;     + Defcustom `w3m-download-save-metadata' controls whether
;;       downloaded `png' and `jpeg' files should have their link's
;;       "alt" caption is stored as meta-data element
;;       "Exif.Image.ImageDescription". Note that enabling this option
;;       will modify the file's checksum. This option currently uses
;;       `exif' and `exiv2' as external back-end programs.
;;
;;       Some ways to view from the command-line an embedded caption
;;       are:
;;
;;           exiv2 -g "Exif.Image.ImageDescription" foo.png
;;             exif --ifd=0 -t0x010e  foo.jpg |grep value
;;
;;     + Defcustom `w3m-download-enable-xattr' controls whether to
;;       save a file's original URL and Referer HTTP header value.
;;       This feature uses the `wget' `--xattr' argument and thus
;;       requires that the save-path be on a file-system that supports
;;       extended attributes.
;;
;;       Be aware that the URL might contain private information like
;;      access tokens or credentials.
;;
;;   + Individual detailed download progress logs in dedicated
;;     buffers, automatically deleted upon successful completion.
;;
;;   + A single central buffer to view all downloads in all states
;;     (queued, running, paused, failed, and completed), act on them,
;;     and see their details. The buffer includes a summary statistics
;;     line.
;;
;;     + Downloads can be paused, resumed, and removed.
;;
;;     + The category order of the display can be easily customized.
;;
;;     + Dedicated faces for each download category.
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
;;     `w3m-download-this-url'       (legacy)
;;     `w3m-download-this-image'     (legacy)
;;     `w3m-save-image'              (legacy)
;;     `w3m-download-video'          (uses `youtube-dl')
;;     `w3m-download-video-at-point' (uses `youtube-dl')
;;     `w3m-download-using-wget'
;;     `w3m-download-using-w3m'      (not recommended)
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
;;
;;     This function is intended to be bound to keybinding `S-C-y' in
;;     a nod to what seems standard in many modern browsers. You can
;;     manually bind the function by evaluating:
;;
;;     (define-key w3m-mode-map [(shift ctrl y)] 'w3m-download-view-queue)
;;     (define-key w3m-mode-map "Y"              'w3m-download-view-queue)
;;
;;
;;     The buffer auto-refreshes every `w3m-download-refresh-interval'
;;     seconds (default 16), but you can always force a refresh using
;;     `w3m-download-refresh-buffer' (`C-g'), and you can change the
;;     value using `w3m-download-set-refresh-interval' (`C-u C-g').
;;
;; The download resumption behavior is controlled by variable
;; `w3m-download-resume' to either auto-restart them, pause them, or
;; start with a clean empty slate.



;;; TODO:

;; + branch w3m-history-scrub needs now to shred
;;   `w3m-download-save-file'.
;;
;; + Easier way to find a particular download to kill
;;
;; + function: w3m-download-select-exec
;;
;;   + pre-screen the list of downloads for possible name conflicts
;;     with existing files, and duplicates, and prompt the user
;;     accordingly.
;;
;;     + maybe do this instead when creating the select buffer, ie.
;;       gray out entries that already exist on the queue
;;
;;   + this should be done for downloads outside of select-exec also
;;
;;
;; + `w3m--download-check-and-use-cache'
;;
;;   + does not emulate `w3m-download-enable-xattr'
;;
;; + add hooks for quitting emacs-w3m
;;
;;   + this might not be necessary. the current method using the save
;;     and load list may be sufficient.
;;
;; + feature creep: None of these-items are necessary. At some point
;;   the project could consider turning to an external program which
;;   specializes in downloading, but anyway ...
;;
;;   + add an annotation field in lists, to be a visible comment that
;;     a user can make on a download record.
;;
;;   + apply certain command upon a region, ie. more than one download
;;     at a time.
;;
;;      + move position in queue
;;
;;      + toggle details
;;
;;      + toggle pause
;;
;;      + delete lines / entries
;;
;;   + an alist of extensions and metadata commands
;;
;;   + an alist of download commands (eg. aria2, curl, axel)
;;
;;   + offloading to a dedicated downloader (eg. uget)
;;
;;   + persistence across crashes (ie. maintain a disk file)
;;
;;   + renice download processes
;;
;;   + delete a partially downloaded file when manually aborted
;;     (eg. when killing the progress buffer)
;;
;;   + use the process-fiter function to add detail to each download.
;;
;;   + torrent and magnet support
;;
;;     + maybe do this using the type-alist for external helper
;;       programs?
;;
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
  "Control manipulation of download state lists.
These are:`w3m--download-queued', `w3m--download-running',
`w3m--download-paused', `w3m--download-failed', and
`w3m--download-completed'.")

(defconst w3m--download-progress-regex
  "\\([0-9]+%\\)\\[[^]]+] +\\([^ ]+\\) +\\([^ ]+\\) +\\(.*\\)$"
  "Parses four values from wget progress message.")

(defconst w3m--download-queue-buffer-header-string "
 This buffer displays running, queued, paused, failed, and completed downloads.\n
   +/-      Adjust maximum number of simultaneous downloads.
   C-o      Toggle a line's details (C-c C-o for all lines)
   p        Pause or re-queue an entry
   C-C C-n  Move an entry line down the queue (later download)
   C-c C-p  Move an entry line up the queue (sooner download)
   C-C C-a  Move an entry line to top of queue (first to download)
   C-C C-e  Move an entry line to bottom of queue (last to download)
   C-k      Delete an entry line (file or fragment will remain on disk)
   C-g      Refresh this buffer now (C-u C-g to set auto-refresh rate)
   q        Close this buffer (or just kill the buffer)\n\n>\n\n")



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
  "Major mode for viewing and editing the `w3m-download' queue.")
(unless w3m-download-queue-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "\C-o"      'w3m-download-toggle-details)
    (define-key map "\C-c\C-o"  'w3m-download-toggle-all-details)
    (define-key map "p"         'w3m-download-toggle-pause)
    (define-key map "\C-k"      'w3m-download-delete-line)
    (define-key map "q"         'w3m-download-buffer-quit)
    (define-key map "Q"         'w3m-download-buffer-quit)
    (define-key map "\C-c\C-k"  'w3m-download-buffer-quit)
    (define-key map "\C-c\C-n"  'w3m-download-queue-drop)
    (define-key map "\C-c\C-p"  'w3m-download-queue-raise)
    (define-key map "\C-c\C-a"  'w3m-download-queue-top)
    (define-key map "\C-c\C-e"  'w3m-download-queue-bottom)
    (define-key map "\C-g"      'w3m-download-refresh-buffer)
    (define-key map "+"         'w3m-download-increase-simultaneous)
    (define-key map "-"         'w3m-download-decrease-simultaneous)
    (setq w3m-download-queue-mode-map map)))

(defvar w3m--download-queued nil
  "List of pending downloads.
Manipulation of this variable should only be done when holding
`w3m--download-mutex'. Each element is a list of five fields:

1. URL - the link to download, as a string.

2. WGET-CMD-LIST - the complete wget command line, as a list of
   strings suitable for function `start-process'.

3. SAVE-PATH - the fully qualified path to place the download.

4. METADATA - the complete command line for tagging the download
   with metadata, as a string sutiable for function
   `shell-command'.

5. SHOW-STATE - the boolean whether its details are being shown
   or hidden.

Additionally, if the element had once been a running download,
ie. it had been paused and then re-queued:

6. START_TIMESTAMP - a string.

7. WGET_PROGRESS - a string, expected to include % completed and
   bytes downloaded")

(defvar w3m--download-running nil
  "List of running downloads.
This is a reference for use by the display buffer and the resume
operation. It is different from `w3m--download-processes-list'.
Manipulation of this variable should only be done when holding
`w3m--download-mutex'. Each element here is inherited from
`w3m--download-queued', and adds the following fields:

6. START_TIMESTAMP - a string.

7. WGET_PROGRESS - a string, expected to include % completed,
   bytes downloaded, rate, and eta.

8. PROC-ID - the Emacs process id for the wget operation.

9. BUFFER - the process buffer, where STDOUT/STDERR is sent.")

(defvar w3m--download-paused nil
  "List of paused downloads.
Manipulation of this variable should only be done when holding
`w3m--download-mutex'. Each entry has the seven elements of
`w3m--download-queued', as inherited from either
`w3m--download-queued' or `w3m--download-running', with the
following modifications:

8. ERROR_CODE - the 'event' string from the Emacs process sentinel.

9. Removed.")

(defvar w3m--download-failed nil
  "List of failed downloads.
Manipulation of this variable should only be done when holding
`w3m--download-mutex'. Each element here is inherited from
`w3m--download-running', with the following modifications:

6. STOP_TIMESTAMP - a string.

7. WGET_PROGRESS - a string, expected to include % completed,
   bytes downloaded, rate, and eta.

8. ERROR_CODE - the 'event' string from the Emacs process sentinel.

9. Removed.")

(defvar w3m--download-completed nil
  "List of completed downloads.
Manipulation of this variable should only be done when holding
`w3m--download-mutex'. Each element here is inherited from
`w3m--download-running', with the following modifications:

8. Removed.

9. Removed.")

(defvar w3m--download-processes-list nil
  "Global list of all running `w3m-download' processes.")

(defvar w3m--download-select-filter-history nil
  "Record of past download-select filter use.
Needed for function `completing-read' to be able to display the
most recently used.")

(defvar w3m--download-refresh-buffer-timer nil
  "Timer object for function `w3m--download-refresh-buffer'.")

(defvar w3m--download-buffer-sequence '(
  (w3m--download-running "R " w3m-download-running)
  (w3m--download-queued  "Q " w3m-download-queued)
  (w3m--download-paused  "P " w3m-download-paused)
  (w3m--download-failed  "F " w3m-download-failed)
  (w3m--download-completed  "C " w3m-download-completed))
  "Sort order details for the `w3m-download-queue' display buffer.
This is just a bit too tricky and error-prone to become a
defcustom. There must be a list element for each category of
items to be displayed. Each element is itself a list of three
elements:

1. The queue list symbol name.

2. A two character string to identify members of the list. For
the first character, you should be able to go to town with the
unicode emoji of your choice, as long as it isn't \">\", but your
choice needs to also be put into `w3m--download-category-regex',
and the second character must be a space.'

3. The face symbol name.")

(defvar w3m--download-category-regex "^[RQPFC]"
  "A regex collection download category identifiers.
These are five single characters that appear at the beginning of
lines denoting 'running', 'queued', 'paused', 'failed', and
'completed' downloads. These need to match elements of
`w3m--download-buffer-sequence'.")



;;; User-options:

(defcustom w3m-download-max-simultaneous 4
  "Maximum number of simultaneous downloads."
  :group 'w3m
  :type 'integer)

(defcustom w3m-download-resume 'pause
  "What to do with unfinished downloads when beginning a new `emacs-w3m' session.

NOTHING means ignore any past downloads. The record of past
downloads will be deleted.

PAUSE means to reload the record of past downloads, but
re-categorize all running or queued ones as paused.

AUTO-RESTART means to reload the record of past downloads and
immediately restart them."
  :group 'w3m
  :type '(radio  (const :tag "Pause" pause)
                 (const :tag "Nothing" nil)
                 (const :tag "Auto-restart" auto-restart)))

(defcustom w3m-download-refresh-interval 16
  "How often (in seconds) to refresh the download display buffer.
This is also the interval for saving the download lists to
`w3m-download-save-file'."
  :group 'w3m
  :type 'integer)

(defcustom w3m-download-save-file
  (concat (file-name-as-directory w3m-profile-directory)
          ".emacs-w3m-downloads")
"Where to save download information.
This will include information about running, queued, paused,
failed, and completed downloads, so that upon resuming emacs-w3m
you can continue where you left off and remember what you
recently did."
  :group 'w3m
  :type 'file)

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

(defcustom w3m-download-video-alist
  '(("\\.youtube\\." . "-f 18 --newline"))
  "*Specific arguments to use for downloading from specific urls.

This data is used for function `w3m-download-video-at-point' and
`w3m-download-video-at-point', which use the external program
`youtube-dl'.

Each element should be of the form (URL . ARGS),
for example (\"\\.youtube\\.\" . \"-f 18\").

Refer to \"man(1) `youtube-dl'\" for information on available
arguments."
  :group 'w3m
  :type '(repeat
      (cons :format "%v" :indent 12
        (string :format "url base:            %v")
        (string :format "args for youtube-dl: %v"))))



;;; Faces:

(defface w3m-download-selected
  '((t :inherit 'w3m-session-select))
  "Face of URL selected for download in w3m-download-select
buffer."
  :group 'w3m)

(defface w3m-download-current-line
  ;; TODO: deprecate this in favor of
  ;;  (add-face-text-property beg end '(:weight bold))))
  ;;  and then removing it using (cdr this-face) [example see below]
  '((t :inherit 'w3m-bold))
  "Face of current line in w3m-download-select buffer."
  :group 'w3m)

(defface w3m-download-selected-current-line
  ;; TODO: deprecate this in favor of
  ;;  (add-face-text-property beg end '(:weight bold))))
  ;;  and then removing it using (cdr this-face) [example see below]
  '((t :weight bold :inherit 'w3m-session-select))
  "Face of current line in w3m-download-select
buffer when that URL is selected for download."
  :group 'w3m)

(defface w3m-download-queued
  '((t :weight normal :inherit 'default))
  "Face of running downloads in w3m-download buffer."
; TODO: Make this more sophisticated to be consistent with other
; project faces, to account for light/dark themes and window-display-p
  :group 'w3m)

(defface w3m-download-running
  '((t :weight normal :foreground "green"))
  "Face of running downloads in w3m-download buffer."
; TODO: Make this more sophisticated to be consistent with other
; project faces, to account for light/dark themes and window-display-p
  :group 'w3m)

(defface w3m-download-paused
  '((t :weight normal :foreground "yellow"))
  "Face of failed downloads in w3m-download buffer."
; TODO: Make this more sophisticated to be consistent with other
; project faces, to account for light/dark themes and window-display-p
  :group 'w3m)

(defface w3m-download-failed
  '((t :weight normal  :inherit 'w3m-error))
  "Face of failed downloads in w3m-download buffer."
; TODO: Make this more sophisticated to be consistent with other
; project faces, to account for light/dark themes and window-display-p
  :group 'w3m)

(defface w3m-download-completed
  '((t :weight normal :foreground "blue"))
  "Face of failed downloads in w3m-download buffer."
; TODO: Make this more sophisticated to be consistent with other
; project faces, to account for light/dark themes and window-display-p
  :group 'w3m)



;;; Buffer-local variables:

(defvar-local w3m--download-url nil
  "The URL to be downloaded.
 This variable is set buffer-local in the download's progress
 buffer.")

(defvar-local w3m--download-save-path nil
  "The full path-name of the downloaded file.
This variable is set buffer-local in the download's progress
buffer.")

(defvar-local w3m--download-local-proc nil
  "The download's process id.
This variable is set buffer-local in the download's progress
buffer.")

(defvar-local w3m--download-metadata-operation nil
  "The complete shell command to be used to tag a download.
A text string. This variable is set buffer-local in the
  download's progress buffer.")



;;; Hook functions:

(defun w3m--download-kill-associated-process ()
  "Hook function for `kill-buffer-hook' for `w3m-download' buffers.
`w3m--download-local-proc' should have been set as a local
variable at buffer creation."
  (if (not (processp w3m--download-local-proc))
    (w3m--message t 'w3m-warning
                  "Warning: no process found to kill (w3m-download).")
   (delete-process w3m--download-local-proc)
   (setq w3m--download-processes-list
     (assq-delete-all w3m--download-local-proc w3m--download-processes-list)))
   (when (and w3m--download-queued
              (>= w3m-download-max-simultaneous
                  (length w3m--download-processes-list)))
     (w3m--download-from-queue)))

(defun w3m--download-update-statistics ()
  "Hook function for `w3m-download-select' and `w3m-download-queue' buffers.
Meant for use with `post-command-hook'."
  (let ((num-selected 0)
        (inhibit-read-only t)
        (pos (point)))
    (when (eq major-mode 'w3m-download-select-mode)
      (goto-char (point-min))
      (while (re-search-forward "^\\[X" nil t)
        (setq num-selected (1+ num-selected))))
    (goto-char (point-min))
    (when (re-search-forward "^>.*$" nil t)
      (replace-match (concat
        (format "> %s"
          (if (eq major-mode 'w3m-download-select-mode)
            (format "%d selected; " num-selected)
           ""))
        (propertize (format "%d/%d running/max;"
                      (length w3m--download-running)
                      w3m-download-max-simultaneous)
          'face 'w3m-download-running)
        (propertize (format " %d queued;" (length w3m--download-queued))
          'face 'w3m-download-queued)
        (propertize (format " %d paused;" (length w3m--download-paused))
          'face 'w3m-download-paused)
        (propertize (format " %d failed;" (length w3m--download-failed))
          'face 'w3m-download-failed)
        (propertize (format " %d completed;" (length w3m--download-completed))
          'face 'w3m-download-completed)))
      (put-text-property (point-min) (min (point-max) (+ 3 (point))) 'field t)
      (put-text-property (point-min) (min (point-max) (+ 3 (point))) 'front-sticky t)
      (put-text-property (point-min) (min (point-max) (+ 3 (point))) 'cursor-intangible t))
  (goto-char (min pos (point-max)))))

(defun w3m--download-update-faces-pre-command ()
  "Hook function for `w3m-download-select' and `w3m-download-queue' buffers.
Meant for use with `pre-command-hook'."
  (let ((pos (point))
        (beg (if (eq major-mode 'w3m-download-select-mode)
               (line-beginning-position)
              (previous-single-property-change
                (min (1+ (point)) (point-max)) 'url nil 767))) ; hard-coded point-min!
        (end (if (eq major-mode 'w3m-download-select-mode)
               (line-end-position)
              (next-single-property-change (point) 'url nil (point-max))))
        (this-face (get-text-property (point) 'face))
        (inhibit-read-only t))
    (unless  (= (point) (point-max))
      (put-text-property beg end 'face (or (cdr this-face) this-face)))
    (goto-char pos)))

(defun w3m--download-update-faces-post-command ()
  "Hook function for `w3m-download-select' and `w3m-download-queue' buffers.
Meant for use with `post-command-hook'."
  (let ((pos (point))
        (beg (if (eq major-mode 'w3m-download-select-mode)
               (line-beginning-position)
              (previous-single-property-change
                (min (1+ (point)) (point-max)) 'url nil 767))) ; hard-coded point-min!
        (end (if (eq major-mode 'w3m-download-select-mode)
               (line-end-position)
              (next-single-property-change (point) 'url nil (point-max))))
        (inhibit-read-only t))
    (unless (= (point) (point-max))
      (add-face-text-property beg end '(:weight bold)))
    (goto-char pos)))

(defun w3m--download-queue-buffer-kill ()
  "Hook function to run when killing a w3m-download-queue buffer."
  ; intentionally redundant and unnecessary code, for safety
  (when (timerp w3m--download-refresh-buffer-timer)
    (cancel-timer w3m--download-refresh-buffer-timer))
  (cancel-function-timers 'w3m--download-refresh-buffer)
  (setq w3m--download-refresh-buffer-timer nil))



;;; Internal functions:

(defun w3m-download-select-mode ()
  "Major mode for bulk download of links in a buffer or region.

\\[w3m-download-select-toggle-line]\tToggle current line's status.
\\[w3m-download-delete-line]\tDelete current line.
\\[w3m-download-increase-simultaneous]\tIncrease numner of parallel downloads.
\\[w3m-download-decrease-simultaneous]\tDecrease number of parralel downloads.
\\[w3m-download-buffer-quit]\tQuit.
\\[w3m-download-select-exec]\tPerform the downloading.\n\n
\\{w3m-download-select-mode-map}\n\n"
  (setq
    mode-name "w3m download select"
    truncate-lines t
    major-mode 'w3m-download-select-mode
    buffer-read-only t)
  (cursor-intangible-mode)
  (use-local-map w3m-download-select-mode-map)
  (add-hook 'pre-command-hook  'w3m--download-update-faces-pre-command t t)
  (add-hook 'post-command-hook 'w3m--download-update-faces-post-command t t)
  (add-hook 'post-command-hook 'w3m--download-update-statistics t t))

(defun w3m-download-queue-mode ()
  "Major mode for viewing and editing the `w3m-download' queue.

\\[w3m-download-toggle-pause]    Toggle pause state of the download.
\\[w3m-download-queue-drop]    Move current line down the queue.
\\[w3m-download-queue-raise]    Move current line up the queue.
\\[w3m-download-queue-top]    Move current line to the top of the queue.
\\[w3m-download-queue-bottom]    Move current to the bottom of the queue.
\\[w3m-download-delete-line]        Delete current line.
\\[w3m-download-increase-simultaneous] / \\[w3m-download-decrease-simultaneous]  \
    Increase /Decrease numner of parallel downloads.
\\[w3m-download-refresh-buffer]        Refresh (update) the buffer contents.
\\[universal-argument] \\[w3m-download-refresh-buffer]\
    Change the buffer auto-refresh interval.
\\[w3m-download-buffer-quit]          Quit.\n\n
\\{w3m-download-queue-mode-map}\n\n"
  ; Note that this mode shares functions with `w3m-download-select-mode'.
  (setq
    mode-name "w3m download queue"
    truncate-lines t
    major-mode 'w3m-download-queue-mode
    buffer-read-only t
    buffer-invisibility-spec (list 'yes))
  (cursor-intangible-mode)
  (use-local-map w3m-download-queue-mode-map)
  (setq w3m--download-refresh-buffer-timer
    (run-with-timer
      w3m-download-refresh-interval
      w3m-download-refresh-interval
      'w3m--download-refresh-buffer))
  (add-hook 'pre-command-hook  'w3m--download-update-faces-pre-command t t)
  (add-hook 'post-command-hook 'w3m--download-update-faces-post-command t t)
  (add-hook 'post-command-hook 'w3m--download-update-statistics t t)
  (add-hook 'kill-buffer-hook  'w3m--download-queue-buffer-kill t t))

(defun w3m--download-load-lists (&optional file)
;; FIXME !!
  "Retrieve the lists of downloads from `w3m-download-save-file' or FILE.
The saved lists are `w3m--download-queued',
 `w3m--download-running', `w3m--download-paused',
 `w3m--download-failed', and `w3m--download-completed'."
  (let ((all-lists (w3m-load-list (or file w3m-download-save-file)))
        one-list)
    (when all-lists
      (dolist (this-list '(w3m--download-queued
                         ; w3m--download-running (merged with queued)
                           w3m--download-paused
                           w3m--download-failed
                           w3m--download-completed))
        (setq one-list (pop all-lists))
        (when (and (not (eval this-list)) one-list)
          (eval `(setq ,this-list (quote ,one-list))))))))

(defun w3m--download-save-lists (&optional file)
  "Save the lists of downloads to `w3m-download-save-file' or FILE.
The saved lists are `w3m--download-queued',
 `w3m--download-running', `w3m--download-paused',
 `w3m--download-failed', and `w3m--download-completed'."
  (with-mutex w3m--download-mutex
    (w3m-save-list
      (or file w3m-download-save-file)
      (list
        ; merge 'running' into 'queued'
        `( ,@(mapcar
              (lambda (x) `( ,@(butlast x 2)))
              w3m--download-running)
           ,@w3m--download-queued)
        w3m--download-paused
        w3m--download-failed
        w3m--download-completed))))

(defun w3m--download-update-progress ()
  "Collect latest progress information from all running downloads."
  (with-mutex w3m--download-mutex
    (dolist (entry w3m--download-running)
      (when (and (< 8 (length entry))
                 (buffer-live-p (nth 8 entry)))
        (with-current-buffer (nth 8 entry)
          (let ((txt (buffer-substring-no-properties
                       (- (point-max) 100) (point-max)))
                (url (nth 0 entry)))
            (when (string-match w3m--download-progress-regex
                    txt)
              (setq txt
                (format "%s, %s,  %s, %s"
                  (match-string 1 txt) (match-string 2 txt)
                  (match-string 3 txt) (match-string 4 txt)))
              (setf (nth 6 entry) txt))))))))

(defun w3m--download-display-queue-list ()
  "Display the download queue.
This is a primitve. It expects the current buffer and point to be
prepared! It is meant to be called by `w3m-download-view-queue'
for the initial buffer creation, and by
`w3m--download-update-display-queue-list' for updates."
  ; TODO: Should I seize the mutex for this?
  (let (result)
    (dolist (state-list w3m--download-buffer-sequence)
      (dolist (entry (eval (nth 0 state-list)))
        (insert
          (propertize
            (concat
              (nth 1 state-list) (nth 0 entry) "\n"
              (propertize
                (dolist (field
                         (let ((len (length entry)))
                           (nconc (butlast (cdr entry) (- len 4))
                                  (last (cdr entry) (- len 5))))
                         result)
                  (when (and field
                             (or (not (sequencep field))
                                 (not (zerop (length field)))))
                    (setq result (concat result
                                         (format "    %s\n" field)))))
                'invisible (if (nth 4 entry) 'yes 'no)))
           'state (nth 0 state-list)
           'url (nth 0 entry)
           'face (nth 2 state-list)))
        (goto-char (point-max))
        (setq result nil))))
  (goto-char (point-max))
; (delete-backward-char 1)
  (w3m--download-update-statistics))

(defun w3m--download-restore-point-sensibly (from cur-col)
"Move point to entry we expect the user will want to use next.
Usually this means keeping it within the same list category.
FROM is the entry's current list, as a symbol.
CUR-COL is the current column of point.
 This function is meant to be called by
`w3m-download-toggle-pause' and `w3m-download-delete-line'."
  (if (and (= (point) (point-max))
           (= 0 (current-column)))
    (forward-line -5)
   (when (not (eq from (get-text-property (point) 'state)))
     (let ((prior (1+ (previous-single-property-change
                        (previous-single-property-change (point)
                          'url nil (point-min))
                      'url nil (point-min))))
           (pos (point)))
       (if (and (get-text-property prior 'url)
                (eq from (get-text-property prior 'state)))
         (goto-char (+ prior cur-col -1))
        (while (and (setq pos (next-single-property-change pos 'state))
                    (not (eq from (get-text-property pos 'state)))))
        (when (and pos (/= pos (point-max)))
          (goto-char (+ pos cur-col))))))))

(defun w3m--download-update-display-queue-list (url current-column current-line)
  "Update the download queue display.
Use URL as a primary indicator of where to restore point, and
CURRENT-LINE as a secondary indicator. A CURRENT-COLUMN NIL is
interpreted as a 0. This function is meant to be called by
`w3m-download-delete-line', `w3m--download-queue-adjust',
`w3m--download-refresh-buffer', `w3m-download-toggle-details',
`w3m-download-toggle-all-details', and
`w3m-download-toggle-pause'."
  (goto-char (point-min))
  (re-search-forward "^>" nil t)
  (forward-line 2)
  (delete-region (point) (point-max))
  (w3m--download-display-queue-list)
  (goto-char (point-min))
  (cond
   (url
    (if (re-search-forward url nil t)
      (forward-line 0)
     (if current-line
       (forward-line current-line)
      (re-search-forward w3m--download-category-regex nil t))))
   (current-line
    (forward-line current-line))
   (t
    (re-search-forward w3m--download-category-regex nil t)))
  (when (/= (point) (point-max))
    (forward-char (or current-column 0))))

(defun w3m--download-refresh-buffer ()
  "Refresh the `w3m-download-queue' buffer.
Invoked by `w3m-download-queue-mode' using `run-with-timer'. It
can be manually invoked via the `w3m-download-refresh-buffer'.
This function also saves the download lists to
`w3m-download-save-file'."
  (let ((buf (get-buffer "*w3m-download-queue*"))
        (inhibit-read-only t))
   (if (not buf)
     (w3m--download-queue-buffer-kill)
    (w3m--download-update-progress)
    (w3m--download-save-lists)
    (with-current-buffer buf
      (w3m--download-update-display-queue-list
        (get-text-property (point) 'url)
        (current-column)
        (1- (string-to-number (format-mode-line "%l"))))
      (w3m--download-update-faces-post-command)))))

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

(defun w3m--download-xfer-entry (url from to)
  "Move or delete a download URL's entry amongst state lists.
FROM and TO are download lists, as symbols.

Used only by `w3m-download-delete-line'.

Similar operations are performed in `w3m-download-toggle-pause',
`w3m--download-from-queue' (xfer from queued to running) and
`w3m--download-sentinel' (xfer from running to completed/failed).

Others?"
  (with-mutex w3m--download-mutex
    (let ((elem (assoc url (eval from)))
          temp)
      (if (not elem) nil ; Entry changed state since last display refresh
       (funcall 'set from (delq elem (eval from)))
       (when to
         (add-to-list to elem t)
         (when (eq to 'w3m--download-queued)
           (w3m--download-from-queue)))
       t))))

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
         (url (get-text-property (point) 'url))
         urls pos pos-end queue elem prior result)

; TODO: Although `transient-mark-mode' is non-nil, `region-active-p'
;       always returns nil, so I can't get region commands to work...
; (when (region-active-p)
;   (setq pos (region-beginning))
;   (setq pos-end (region-end))
;   (while (and pos
;               (setq pos (next-single-property-change pos 'url nil pos-end)))
;     (push (get-text-property (point) 'url) urls)
;     (when (= pos pos-end)
;       (setq pos nil))))
; (message "debug: urls: %s; beg: %s; end: %s" urls (region-beginning)(region-end))

    ; modify the queue
    (with-mutex w3m--download-mutex
      (setq queue w3m--download-queued)
      (cond
       ((eq direction 'top)
        (setq elem (assoc url queue))
        (when elem
          (setq w3m--download-queued (delq elem w3m--download-queued)))
          (push elem w3m--download-queued))
       ((eq direction 'bottom)
        (setq elem (assoc url queue))
        (when elem
          (setq w3m--download-queued (delq elem w3m--download-queued)))
          (add-to-list 'w3m--download-queued elem t))
       ((eq direction 'raise)
        (while queue
          (cond
           ((equal url (car (setq elem (pop queue))))
            (push elem result))
           (t
            (push prior result)
            (setq prior elem))))
        (push prior result)
        (setq w3m--download-queued (reverse (delq nil result))))
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
              (setq prior nil)))))
        (push prior result)
        (setq w3m--download-queued (reverse (delq nil result))))))
    ; redisplay the queue elements
    (w3m--download-update-display-queue-list url current-column nil))))

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
  "Called by Emacs when `w3m-download' process PROC 'change state' to EVENT.
Reference `set-process-sentinel'."
  (let ((buf (process-buffer proc)))
   (with-current-buffer buf
     (let ((inhibit-read-only t)
           (kill-buffer-query-functions nil))
      (goto-char (point-max))
      (insert (concat "\n" event))
      (cond
       ((string-match "^open" event) t)
       ((string-match "^finished" event)
         ;; TODO: Maybe keep buffer open if there was an error in
         ;; performing the metadata tagging?
         (shell-command (concat "mv " w3m--download-save-path "{.PART,}"))
         (w3m--message t t "Download completed successfully.")
         (condition-case err
           (w3m--download-apply-metadata-tags)
          (err (w3m--message t 'w3m-error
                 "Error %s applying metadata to %s" err w3m--download-url)))
         (setq w3m--download-processes-list
           (assq-delete-all proc w3m--download-processes-list))
         (let ((elem (assoc w3m--download-url w3m--download-running))
               txt index)
           (if (not elem)
             nil ; an error, but I'm undecided what to do about it
            (when (re-search-backward
                    " 100%\\[=+> *\\] +\\([^ ]+\\) +\\([^ ]+\\) +\\(.*\\)$" nil t)
              (setq txt (format "%s, %s, %s"
                          (match-string 1)
                          (match-string 2)
                          (replace-regexp-in-string "  +" " " (match-string 3)))))
            (goto-char (point-max))
            (when (re-search-backward "saved \\[\\([0-9/]+\\)\\]$" nil t)
              (setq txt (concat txt (format ", %s bytes." (match-string 1)))))
            (with-mutex w3m--download-mutex t
              (setq w3m--download-running (delq elem w3m--download-running))
              (push `(,@(butlast elem 3)
                      ,(concat "Completed: " (current-time-string))
                      ,txt)
                     w3m--download-completed))))
          (kill-buffer buf))
        ((string-match
            "\\(deleted\\)\\|\\(terminated\\)\\|\\(interrupt\\)\\|\\(killed\\)"
            event) ; treat these as 'pause' events
         (setq w3m--download-processes-list
           (assq-delete-all proc w3m--download-processes-list))
         (with-mutex w3m--download-mutex
           (let ((elem (assoc w3m--download-url w3m--download-running))
                 txt index)
             (if (not elem)
               nil ; an error, but I'm undecided what to do about it
              (setq w3m--download-running (delq elem w3m--download-running))
              (setq txt (nth 6 elem))
              (when (setq index (string-match ",  " txt))
                (setq txt (substring txt 0 index)))
              (setq elem `(,@(butlast elem 4)
                           ,(concat (nth 5 elem) "\n    Paused:    " (current-time-string))
                           ,txt
                           ,event))
              (push elem w3m--download-paused))))
         (kill-buffer buf))
        (t
         (w3m--message t 'w3m-error "Error (%s) downloading %s"
           (substring event 0 -1) w3m--download-url)
         (condition-case err
           (w3m--download-apply-metadata-tags)
          (err (w3m--message t 'w3m-error
                 "Error %s applying metadata to %s" err w3m--download-url)))
         (setq w3m--download-processes-list
           (assq-delete-all proc w3m--download-processes-list))
         (with-mutex w3m--download-mutex
           (let ((elem (assoc w3m--download-url w3m--download-running))
                 txt index)
             (if (not elem)
               nil ; an error, but I'm undecided what to do about it
              (setq w3m--download-running (delq elem w3m--download-running))
              (setq txt (nth 6 elem))
              (when (setq index (string-match ",  " txt))
                (setq txt (substring txt 0 index)))
              (setq elem `(,@(butlast elem 4)
                           ,(concat (nth 5 elem) "\n    Failed:  " (current-time-string))
                           ,txt
                           ,event))
              (push elem w3m--download-failed))))
         (kill-buffer buf)))))))

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
        (save-excursion
          (goto-char (point-max))
          (if (not (string= "\r" (substring input-string 0 1)))
            (insert input-string)
           (forward-line 0)
           (delete-region (point) (point-max))
           (insert (substring input-string 1)))))))))

(defun w3m--download--cache-or-queue (url save-path no-cache metadata)
  "Either prepare a `wget' download or save URL from the `emacs-w3m' cache.

With NO-CACHE nil, if URL exists in the cache it is saved from there,
avoiding any further download action.

Otherwise, use URL, SAVE-PATH, RESUME, and METADATA to add the
download's information to the end of `w3m--download-queued' and
try to perform a download from the head of the queue.

If METADATA is non-nil, it should be a shell command to be used
to add metadata to SAVE-PATH."
  (when (not (when (not no-cache)
               (w3m--download-check-and-use-cache url save-path metadata)))
    (with-mutex w3m--download-mutex
      (add-to-list 'w3m--download-queued
        (list
          url
          (delq nil (list "wget" "-c"
                    (if w3m-download-enable-xattr "--xattr")
                    "-O" (concat save-path ".PART") url))
          save-path
          metadata
          t) ; SHOW-STATE = details begin as invisible/hidden
       t)) ; t = append to end of list
    (w3m--download-from-queue)))

(defun w3m--download-from-queue ()
  "Set-up a download progress buffer and begin a download process."
   (while (and w3m--download-queued
              (>  w3m-download-max-simultaneous
                  (length w3m--download-processes-list)))
    (let* ((buf (generate-new-buffer "*w3m-download*"))
           (job (with-mutex w3m--download-mutex
                  (pop w3m--download-queued)))
           (len (length job))
           (url       (nth 0 job))
           (wget-cmd  (nth 1 job))
           (save-path (nth 2 job))
           (metadata  (nth 3 job))
           (time-stamp (if (> len 5)
                        (nth 5 job)
                       (concat "Started:   " (current-time-string))))
           (progress (if (> len 6)
                        (nth 6 job)
                       "")))
     (with-current-buffer buf
       (insert (format "emacs-w3m download log\n
    Killing this buffer will abort the download!\n
Time: %s\nURL : %s\nExec: %s\n\n"
                 time-stamp url wget-cmd))
       (setq w3m--download-url url)
       (setq w3m--download-save-path save-path)
       (setq w3m--download-local-proc
         (apply 'start-process "w3m-download" buf wget-cmd))
       (set-process-filter w3m--download-local-proc
                           'w3m--download-process-filter)
       (setq w3m--download-metadata-operation metadata)
       (push (cons w3m--download-local-proc buf) w3m--download-processes-list)
       (add-hook 'kill-buffer-hook 'w3m--download-kill-associated-process nil t)
       (set-process-sentinel w3m--download-local-proc 'w3m--download-sentinel)
       (setq job `(,@(butlast job (- len 5))
                   ,time-stamp  ,progress ,w3m--download-local-proc ,buf))
       (setq buffer-read-only t)
       (goto-char (point-max)))
     (with-mutex w3m--download-mutex
       (add-to-list 'w3m--download-running job t))
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

(defun w3m--download-init ()
  "Begin and/or resume an emacs-w3m download session.
See `w3m-download-resume' for options."
  (if (not w3m-download-resume)
    (delete-file w3m-download-save-file)
   (w3m--download-load-lists)
   (cond
    ((eq w3m-download-resume 'pause)
     (setq w3m--download-paused
       (delq nil `(,@w3m--download-queued
                   ,@w3m--download-paused)))
     (setq w3m--download-running nil)
     (setq w3m--download-queued nil))
    ((eq w3m-download-resume 'auto-restart)
     (when w3m--download-queued
       (w3m--download-from-queue))))))



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
  (setq w3m--download-queued nil) ; without using mutex
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

(defun w3m-download-queue-top ()
  "Move the curent entry to the top of the queue.
This means it will be downloaded next."
  (interactive)
  (w3m--download-queue-adjust 'top))

(defun w3m-download-queue-bottom ()
  "Move the curent entry to the bottom of the queue.
This means it will be downloaded last."
  (interactive)
  (w3m--download-queue-adjust 'bottom))

(defun w3m-download-set-refresh-interval (&optional arg)
  "Change value `w3m-download-refresh-interval'.
ARG must be a positive integer, in seconds. The default is 16."
  (interactive)
  (let
    ((default 16)
     (prompt
       "refresh interval for download queue buffer (integer seconds)? "))
   (setq w3m-download-refresh-interval
     (if (> 1 (if (integerp arg)
                arg
               (setq arg (read-number prompt default))
               (if (integerp arg) arg -1)))
       default
      arg))))

(defun w3m-download-toggle-all-details ()
  "View or hide the details of all downloads.
Use the state of the current entry as a basis for the decision
which to do."
  (interactive)
  (if (not (eq major-mode  'w3m-download-queue-mode))
    (w3m--message t 'w3m-error
      "This command is available only in w3m-download buffers")
   (let ((inhibit-read-only t)
         (state (or (get-text-property (point) 'invisible)
                    (get-text-property
                      (next-single-property-change (point) 'invisible)
                      'invisible))))
    (setq state (if (eq state 'no) t nil))
    (with-mutex w3m--download-mutex
      (dolist (state-list w3m--download-buffer-sequence)
        (dolist (elem (eval (car state-list)))
          (setf (nth 4 elem) state))))
    (w3m--download-update-display-queue-list
      (get-text-property (point) 'url)
      (current-column)
      ; FIXME now with option to view detailsd, this technique for
      ; using the current-line needs to be changed!
      ; (1- (string-to-number (format-mode-line "%l")))))))
      nil ))))

(defun w3m-download-toggle-details ()
  "View or hide the details of the download at point."
  (interactive)
  (if (not (eq major-mode 'w3m-download-queue-mode))
    (w3m--message t 'w3m-error
      "This command is available only in w3m-download buffers")
   (let* ((inhibit-read-only t)
          (state-list (get-text-property (point) 'state))
          (url (get-text-property (point) 'url)))
     (with-mutex w3m--download-mutex
       (let* ((elem (eval `(assoc ,url ,state-list)))
              (new-state (when elem
                           (not (nth 4 elem)))))
         (when elem
           ; setf can be tricky. be careful with it...
           (setf (nth 4 (assoc url (eval state-list))) new-state))))
     (w3m--download-update-display-queue-list
       url (current-column) nil))))

(defun w3m-download-toggle-pause ()
  "Change the pause state of the download at point.
If the download is running or queued, pause it. If it is paused
or failed, restart or continue it."
  (interactive)
  (if (not (eq major-mode 'w3m-download-queue-mode))
    (w3m--message t 'w3m-error
      "This command is available only in w3m-download buffers")
   (let* ((inhibit-read-only t)
          (kill-buffer-query-functions nil)
          (update-err-msg
            "Entry has changed state since last display refresh")
          (state (get-text-property (point) 'state))
          (url  (get-text-property (point) 'url))
          (cur-col (current-column))
          elem timestamp txt)
    (if (eq state 'w3m--download-completed)
      (w3m--message t 'w3m-error "Can not pause a completed download")
     (with-mutex w3m--download-mutex
       (setq elem (assoc url (eval state)))
       (if (not elem)
         (w3m--message t 'w3m-error update-err-msg)
        (cond
         ((eq state 'w3m--download-paused)
          (setq w3m--download-paused (delq elem w3m--download-paused))
          (setq timestamp (nth 5 elem))
          (when (and timestamp (string-match "\n" timestamp))
            (setq timestamp (substring timestamp 0 (match-beginning 0))))
          (setq txt (nth 6 elem))
          (setq elem `(,@(butlast elem 3) ,timestamp ,txt))
          (add-to-list 'w3m--download-queued elem t)
          (w3m--download-from-queue))
         (t
          (eval `(setq ,state (delq (quote ,elem) ,state)))
          (when (buffer-live-p (nth 8 elem))
            (kill-buffer (nth 8 elem)))
          (setq txt (nth 6 elem))
          (when (and txt (string-match ",  " txt))
            (setq txt (substring txt 0 (match-beginning 0))))
          (setq base (butlast elem (if (equal state "w3m--download-queued") 2 4)))
          (setq elem `(,@(butlast elem (if (eq state 'w3m--download-queued) 2 4))
                       ,(concat (nth 5 elem) "\n    Paused:    " (current-time-string))
                       ,txt))
          (add-to-list 'w3m--download-paused elem t)))))
    (w3m--download-update-display-queue-list nil cur-col
      (1- (string-to-number (format-mode-line "%l"))))
    (w3m--download-restore-point-sensibly state cur-col)))))

(defun w3m-download-refresh-buffer (&optional arg)
  "Refresh the `w3m-download' buffer.
If ARG is non-nil, change the auto-refresh interval, using
function `w3m-download-set-refresh-interval'."
  (interactive "P")
   (if (not arg)
     (w3m--download-refresh-buffer)
    (w3m-download-set-refresh-interval arg)))

;;;###autoload
(defun w3m-download-view-queue ()
  "View the `w3m-download' queue and allow editing it."
  (interactive)
  (if (and (not w3m--download-queued)
           (not w3m--download-running)
           (not w3m--download-paused)
           (not w3m--download-failed)
           (not w3m--download-completed))
    (w3m--message t 'w3m-error "Download lists are empty. Nothing to display.")
   (with-current-buffer
     (setq buf (get-buffer-create "*w3m-download-queue*"))
     (w3m-download-queue-mode)
     (let ((inhibit-read-only t)
           pos)
       (erase-buffer)
       (insert (propertize w3m--download-queue-buffer-header-string
                  'cursor-intangible t 'field t 'front-sticky t))
       (w3m--download-display-queue-list))
     (goto-char (point-min))
     (re-search-forward w3m--download-category-regex))
   (switch-to-buffer buf)))

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
  "Exit from a `w3m-download' buffer."
  (interactive)
  (if (not (or (eq major-mode 'w3m-download-select-mode)
               (eq major-mode 'w3m-download-queue-mode)))
    (w3m--message t 'w3m-error
      "This command is available only in w3m-download buffers")
   (let ((kill-buffer-query-functions nil))
     (kill-buffer (current-buffer)))))

(defun w3m-download-delete-line ()
  "Delete the current URL entry.
Used in `w3m-download-select' and `w3m-download-queue' buffers."
  (interactive)
  (cond
   ((eq major-mode 'w3m-download-queue-mode)
    (let ((inhibit-read-only t)
          (kill-buffer-query-functions nil)
          (cur-col (current-column))
          (cur-lin (1- (string-to-number (format-mode-line "%l"))))
          (url  (get-text-property (point) 'url))
          (from (get-text-property (point) 'state)))
     ; BEGIN - code duplication from `w3m-download-toggle-pause'
     (when (eq from 'w3m--download-running)
       (let ((elem (assoc url w3m--download-running)))
         (when (and elem
                    (< 7 (length elem))
                    (buffer-live-p (nth 7 elem)))
           (kill-buffer (nth 7 elem)))))
     ; END - code duplication from `w3m-download-toggle-pause'
     (w3m--download-xfer-entry url from nil)
     (w3m--download-update-display-queue-list nil cur-col cur-lin)
     (w3m--download-restore-point-sensibly from cur-col)))
   ((eq major-mode 'w3m-download-select-mode)
    (let ((inhibit-read-only t))
     (delete-region
       (line-beginning-position)
       (line-beginning-position 2))))
   (t
    (w3m--message t 'w3m-error
      "This command is available only in w3m-download buffers"))))

(defun w3m-download-select-toggle-line ()
  "Change selection status of the current URL entry.
Used in w3m download select mode buffers."
  (interactive)
  (if (not (eq major-mode 'w3m-download-select-mode))
    (w3m--message t 'w3m-error
      "This command is available only in w3m-download-select buffers.")
   (let ((beg (line-beginning-position))
         (end (line-beginning-position 2))
         (cur-col (current-column))
         (inhibit-read-only t))
     (save-excursion
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
             'default))))
       (goto-char (+ end cur-col))
       (when (/= cur-col (current-column))
         (forward-line 0)
         (forward-char cur-col)))))

;;;###autoload
(defun w3m-download-this-url ()
  "Download the file or the page pointed to by the link under point."
  ;; This is not a native w3m-download.el function. It is a legacy
  ;; function imported from w3m.el
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

;;;###autoload
(defun w3m-download-this-image ()
  "Download the image under point."
  ;; This is not a native w3m-download.el function. It is a legacy
  ;; function imported from w3m.el
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

;;;###autoload
(defun w3m-save-image ()
  "Save the image under point to a file.
The default name will be the original name of the image."
  (interactive)
  ;; This is not a native w3m-download.el function. It is a legacy
  ;; function imported from w3m.el
  (let ((url (w3m-url-valid (w3m-image))))
    (if url
        (w3m-download url)
      (w3m--message t 'w3m-error "No image at point"))))

(defun w3m-download-delete-all-download-buffers ()
  "Delete all `w3m-download' buffers.

Be careful when using this function. It will kill any active `wget'
downloads in progress, and empty the download queue."
  (interactive)
  (setq w3m--download-queued nil) ; without using mutex
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
(defun w3m-download-video (url)
  "Invoke `youtube-dl' in a sub-process to download a video from URL."
  ;; This is not a native w3m-download.el function. It is a legacy
  ;; function imported from the emacs-w3m mailing list. [#12802]
  (interactive)
  (if (not (w3m-url-valid url))
    (error "Invalid URL")
  (when (string-match "\\(\\&list=[^&]+\\)" url)
    (setq url
      (concat
        (substring-no-properties url nil (match-beginning 0))
        (substring-no-properties url (match-end 0)))))
  (let* (proc
         (title (w3m-anchor-title))
         (buf (generate-new-buffer "*w3m-download-video*"))
         (base (and (string-match "//\\([^/]+\\)/" url)
           (substring-no-properties url
             (match-beginning 1) (match-end 1))))
         (args (catch 'found-replacement
           (dolist (elem w3m-download-video-alist "--")
             (when (string-match (car elem) base)
               (throw 'found-replacement (cdr elem)))))))
    (with-current-buffer buf
      (insert (current-time-string) "\n\n  " (or title " ")
        "\n\n  youtube-dl " args " " url "\n\n")
      ; NOTE: There does exist a function `w3m-process-do' to evaluate
      ;       lisp code asyncronously.
      (setq proc
        (apply 'start-process "w3m-download-video" buf "youtube-dl"
          (split-string (concat args " " url)))))
    (w3m--message nil t "Requesting download.")
    (set-process-sentinel proc
      (lambda (proc event)
        (let ((buf (process-buffer proc)))
         (with-current-buffer buf (insert event))
         (cond
          ((string-match "^finished" event)
             (w3m--message t t
               "Download complete. Check buffer %s for details." buf))
          ((string-match "^open" event) t)
          (t (w3m--message t 'w3m-error
               "Download error. Check buffer %s for details." buf)))))))))

;;;###autoload
(defun w3m-download-video-at-point ()
  "Invoke `youtube-dl' in a sub-process to download the video at point."
  ;; This is not a native w3m-download.el function. It is a legacy
  ;; function imported from the emacs-w3m mailing list [#12802].
  (interactive)
  (let ((url (w3m-anchor)))
   (if (not url)
     (user-error "No url found at point")
    (w3m-download-video url))))

;;;###autoload
(defun w3m-download-using-wget (url &optional save-path no-cache interactively)
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

INTERACTIVELY is a boolean value for whether the calling function
was itself interactively and thus whether the user may be prompted for
further information."
  (interactive (list (w3m-active-region-or-url-at-point) nil nil t))
  (let* (basename extension metadata caption found-file
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
        (overwrite-prompt
          (format "%s%s"
            (if (zerop num-in-progress) ""
             (concat others-in-progress-prompt "\n"))
            "File(%s) already exists. Overwrite? "))
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
    (when interactively
      (when (file-exists-p save-path)
        (if (y-or-n-p (format overwrite-prompt save-path))
          (delete-file save-path)
         (setq save-path nil)))
      (when (and (file-exists-p (setq found-file (concat save-path ".PART")))
                 (not (y-or-n-p (format resume-prompt found-file))))
        (setq save-path nil)))
    (when save-path
      (w3m--download--cache-or-queue url save-path no-cache metadata))))

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
          (erase-buffer)
          (insert (propertize
                    "  w3m-download-select buffer\n
Review the links selected [X] for downloading.\n
  C-c C-c  Begin downloading
  <SPACE>  Toggle a link's status
  +/-      Adjust maximum number of simultaneous downloads.
  C-k      Delete an entry line
  q        Abort (or just kill the buffer)\n\n\n>\n\n"
                     'cursor-intangible t 'field t 'front-sticky t))
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
      (w3m--download-update-faces-post-command))))))

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
(w3m--download-init)
(provide 'w3m-download)
;;; w3m-download.el ends here

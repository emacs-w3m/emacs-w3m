= emacs-w3m

[![Build Status](https://travis-ci.com/Boruch-Baum/emacs-w3m.svg?branch=bb_travis)](https://travis-ci.com/boruch-baum/emacs-w3m)


[//]: # License badges: https://gist.github.com/lukas-h/2a5d00690736b4c3a7ba

This is a fork of the
[emacs-w3m](https://github.com/emacs-w3m/emacs-w3m) git repository.
The `master` branch here provides my tested modifications to the
original, while the `bb-upstream`branch here _should_ track the
official repository's `master` branch. Additionally, this fork will
have unique branches for development experiments, projects, and
pending pull requests.

See [below](#project-readme-documentation) for the project's [README documentation](#project-readme-documentation).

## Unique branches here

* `bb_download` <sub>[_pr #27_](https://github.com/emacs-w3m/emacs-w3m/pull/27)</sub>
  * New feature: Use `wget` for downloads, when available.
  * New feature: Allow resumption of aborted downloads.
  * Downloads are queued, and the queue can be examined and
    re-arranged from a special buffer.
  * New feature: Detailed individual progress buffers for each
    download.
  * New feature: Download status display buffer to view, resequence,
    pause, resume, or kill any download.
  * New feature: Ability to abort a download just by killing its
    progress buffer.
  * New feature: The number of simultaneous downloads may be
    controlled, and can be dynamically changed.
  * New feature: Option to save an image's caption as metadata (this
    requires external programs `exif` for png files and `exiv2` for
    jpg files).
  * New feature: For file-systems that support extended attributes, it
    is possible to have `wget` save URL and HTTP header information as
    metadata.
  * New feature: Use `youtube-dl` when available, for downloading videos.
  * New file `w3m-download.el` collects most downloaded-related functions.
  * New feature: Optionally deep-search links from within HTML "SCRIPT"
    elements and other 'hidden' parts of HTML source code.
  * New feature: Smart auto-rename ambiguous and duplicate download names.

[![License: GPL
v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)

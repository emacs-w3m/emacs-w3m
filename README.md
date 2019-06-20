# emacs-w3m: an Emacs interface to w3m

[![Build Status](https://travis-ci.com/emacs-w3m/emacs-w3m.svg)](https://travis-ci.com/emacs-w3m/emacs-w3m)
[![License: GPL v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)

## 1. Introduction

w3m is a pager with WWW capability, developed by Akinori ITO.
Although it is a pager, it can be used as a text-mode WWW browser.
Visit the official w3m page for details.

    http://w3m.sourceforge.net/

Emacs-w3m is a simple Emacs interface to w3m.  Its official web page
is available at:

    http://emacs-w3m.namazu.org/

You can find more detailed version of the following explanations
in the form of HTML'ized info:

    http://emacs-w3m.namazu.org/info/


## 2. Requirements

Check whether your system meets the following requirements before
installing emacs-w3m.

Emacs-w3m requires the latest version of w3m (version 0.3.1 and
later).  Since this program is much sensitive to the version of w3m,
you should confirm it if you already have w3m installed.  And we
recommend you visit the official w3m web page to check whether a
newer version of w3m has been released:

    http://prdownloads.sourceforge.net/w3m/

If you want to use the shimbun library which is included in the
emacs-w3m distribution, you have to install FLIM package.  For more
detail about the shimbun library, see "Shimbun Library" section in
Info.


<dl>
  <dt>a) Emacs 21.x</dt>
  <dd>No additional packages are required.</dd>
  <dt>b) XEmacs 21.x</dt>
  <dd>First of all, you should note that emacs-w3m supports only XEmacs
      21.4.17 and later and XEmacs 21.5-b19 and later.  In addition, you
      need to have installed the latest xemacs-base package including
      the timer-funcs.el module.

      APEL package is required.  Use the latest one available in:

        http://kanji.zinbun.kyoto-u.ac.jp/~tomo/lemi/dist/apel/

      Note: You must not use the APEL XEmacs package (which is contained
       in SUMO) of the versions older than 1.32.  If you have already
       installed such a version, you should upgrade it or replace it
       with APEL which is linked above (you can also use the same
       directives in order to newly install APEL):

          % rm -fr /usr/local/lib/xemacs/xemacs-packages/lisp/apel
          % cd apel-10.7
          % make install-package XEMACS=xemacs-21.4.x\
            PACKAGEDIR=/usr/local/lib/xemacs/xemacs-packages

      If you are using XEmacs 21.x, you should install the "gifsicle"
      program.  There is a known bug in all XEmacs 21.x series that
      it won't let it display optimized animated gifs correctly or may
      make it crash when some kind of an interlaced gif image is
      displayed.  Emacs-w3m uses the "gifsicle" program to convert gif
      data in order to make it possible to be handled by XEmacs 21.x.
      It is available at:

          http://www.lcdf.org/gifsicle/
  </dd>
  <dt>c) Emacs 20.x, Emacs 19.34 (including Mule 2.3)</dt>
  <dd>Emacs-w3m no longer supports those Emacs versions.</dd>
</dl>


## 3. Installation

### Installing emacs-w3m on UNIX-like systems

At the first, run the `configure' script.

```
% ./configure
```

If you can't find the `configure' script, rus the `autoconf'
command. It create this script.

```
% autoconf
```

> Important notice to the Gnus users:
> ===================================
> If the various versions of Gnusae are installed in your system (it
> is likely that there are the released version and the development
> version of Gnusae), make sure that priority is given to the
> directory where the gnus.elc file which you use is installed in the
> load-path.  To do that, use the --with-addpath option as follows:
>
>     % ./configure --with-addpath=/usr/local/share/emacs/site-lisp/gnus
>
> If you've installed APEL, FLIM or something in non-standard
> directories other than the default `load-path', you must specify
> them using the --with-addpath option as follows (you may also
> include the Gnus directory being separated with `:' in it):
>
 >   % ./configure --with-addpath=/opt/share/apel:/opt/share/flim

Next, execute the following commands to install emacs-w3m to an
appropriate directory.

```
% make
% make install
```

If you are using Emacs 21 or XEmacs, you had better install icon
image files.  To do this:

```
% make install-icons
```

or

```
% make install-icons30
```

The latter will install the slightly larger icons with characters.

You can also install emacs-w3m as an XEmacs package using
`make install-package' instead of `make install'.

```
% make
% make install-package
```

In this case, you don't have to execute `make install-icons' nor
`make install-icons30'.

The info files will also be installed by `make install' or
`make install-package'.

### Installing on non-UNIX-like systems

If you cannot execute the `configure' script on your system, or if
no `make' command is available, execute the following command:

```
% emacs -batch -q -no-site-file -l w3mhack.el NONE -f w3mhack-nonunix-install
```

If APEL, FLIM (or any other library) aren't installed in the
ordinary places, the installer will leave them out.  In such a
case, it is necessary to tell those places to the installer as
shown below:

```
% emacs -batch -q -no-site-file -l w3mhack.el //c/share/apel://c/share/flim -f w3mhack-nonunix-install
```


## 4. Configuration

We recommend using the ~/.emacs-w3m file (which is the default
value of `w3m-init-file') when you twiddle some variables of
emacs-w3m.  This file is similar to ~/.emacs, but is read when
emacs-w3m starts.  However, note that there are options which
shouldn't be put there, for example, `w3m-command'.

### Essential Configuration

Put this line into your ~/.emacs file:

```emacs-lisp
(require 'w3m-load)
```

You have nothing to do if you have emacs-w3m installed as an XEmacs
package.

### mime-w3m.el

In order to handle text/html part with emacs-w3m under SEMI MUAs
such as T-gnus and Wanderlust, you have to put the following line
in your ~/.emacs file:

    (require 'mime-w3m)

### Proxy Gateway

There are some ways to do this, one is to set the "http_proxy"
environment variable globally in the shell something like:

    setenv http_proxy http://proxy.hogege.com:8000/

Another way is to customize the `w3m-command-arguments' variable to
add the options "-o" and "http_proxy=http://PROXY_SERVER_NAME:PORT/".
This can also be done in your ~/.emacs-w3m file as shown below:

   (setq w3m-command-arguments
         (nconc w3m-command-arguments
                '("-o" "http_proxy=http://proxy.hogege.com:8000/")))

To specify `no-proxy' hosts, which shouldn't be connected to with
proxy gateways, you can set the "no_proxy" environment variable
with the comma separated host names, or set the
`w3m-no-proxy-domains' variable with a list of domain names (not
host names) as follows:

    (setq w3m-no-proxy-domains '("local.com" "neighbor.com"))

See also the documentation for the `w3m-command-arguments-alist'
variable to use regexps to specify the `no-proxy' hosts.


## 5. Contact the emacs-w3m community

To contact the emacs-w3m community for reporting bugs, contributing
improvements, making a suggestion or asking us for help, send a mail
to the open list <emacs-w3m@namazu.org>.  You can also send a bug
report using the `report-emacs-w3m-bug' command or the `C-c C-b' key
if you have set the `mail-user-agent' variable that will work
properly.


## 6. Acknowledgments

w3m, which is an essential part of this package, was written by
Akinori ITO.  We'd like to address our thanks to him for his nice
work.


## 7. Related Information

<dl>
  <dt>APEL</dt>
  <dd>It can be downloaded from: http://kanji.zinbun.kyoto-u.ac.jp/~tomo/lemi/dist/apel/<dd>
  <dt>FLIM</dt>
  <dd>It can be downloaded from: http://kanji.zinbun.kyoto-u.ac.jp/~tomo/lemi/dist/flim/flim-1.14/  Before installing it, it is necessary to install APEL.</dd>
  <dt>gifsicle</dt>
  <dd>It can be downloaded from: http://www.lcdf.org/gifsicle/</dd>
</dl>

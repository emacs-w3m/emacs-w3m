AC_DEFUN(AC_EMACS_LISP, [
elisp="$2"
if test -z "$3"; then
	AC_MSG_CHECKING(for $1)
fi
AC_CACHE_VAL(EMACS_cv_SYS_$1,[
	OUTPUT=./conftest-$$
	echo ${EMACS}' -batch -eval '\''(let ((x '"${elisp}"')) (write-region (if (stringp x) (princ x) (prin1-to-string x)) nil "'${OUTPUT}'" nil 5))'\' >& AC_FD_CC 2>&1
	eval ${EMACS}' -batch -eval '\''(let ((x '"${elisp}"')) (write-region (if (stringp x) (princ x) (prin1-to-string x)) nil "'${OUTPUT}'" nil 5))'\' >& AC_FD_CC 2>&1
	retval="`cat ${OUTPUT}`"
	echo "=> ${retval}" >& AC_FD_CC 2>&1
	rm -f ${OUTPUT}
	EMACS_cv_SYS_$1="${retval}"
])
$1="${EMACS_cv_SYS_$1}"
if test -z "$3"; then
	AC_MSG_RESULT($$1)
fi
])

AC_DEFUN(AC_CHECK_EMACS,
 [dnl Check for Emacsen.

  dnl Apparently, if you run a shell window in Emacs, it sets the EMACS
  dnl environment variable to 't'.  Lets undo the damage.
  test x${EMACS} = xt && EMACS=

  dnl Ignore cache.
  unset ac_cv_prog_EMACS; unset EMACS_cv_SYS_flavor;

  AC_ARG_WITH(emacs,
   [  --with-emacs=EMACS      compile with EMACS [EMACS=emacs, xemacs, mule...]],
   [if test "${withval}" = yes -o -z "${withval}"; then
      AC_CHECK_PROGS(EMACS, emacs xemacs mule, emacs)
    else
      AC_CHECK_PROG(EMACS, ${withval}, ${withval}, emacs)
    fi])
  AC_ARG_WITH(xemacs,
   [  --with-xemacs=XEMACS    compile with XEMACS [XEMACS=xemacs]],
   [if test x$withval = xyes -o x$withval = x; then
      AC_CHECK_PROG(EMACS, xemacs, xemacs, xemacs)
    else
      AC_CHECK_PROG(EMACS, $withval, $withval, xemacs)
    fi])
  test -z "${EMACS}" && AC_CHECK_PROGS(EMACS, emacs xemacs mule, emacs)
  AC_SUBST(EMACS)
  
  AC_MSG_CHECKING([what a flavor does ${EMACS} have])
  AC_EMACS_LISP(flavor,
    (cond ((featurep (quote xemacs)) \"XEmacs\")\
          ((and (boundp (quote emacs-major-version))\
                (>= emacs-major-version 21))\
           \"FSF Emacs 21\")\
          ((boundp (quote MULE)) \"MULE\")\
          (t \"FSF Emacs\")),
    noecho)
  case "${flavor}" in
  XEmacs)
    EMACS_FLAVOR=xemacs;;
  MULE)
    EMACS_FLAVOR=mule;;
  "FSF Emacs 21")
    EMACS_FLAVOR=emacs21;;
  *)
    EMACS_FLAVOR=emacs;;
  esac
  AC_MSG_RESULT(${flavor})])

AC_DEFUN(AC_EXAMINE_PACKAGEDIR,
 [dnl Examine PACKAGEDIR.
  AC_EMACS_LISP(PACKAGEDIR,
    (let ((prefix \"${prefix}\")\
	  package-dir)\
      (if (boundp (quote early-packages))\
	  (let ((dirs (append (if early-package-load-path early-packages)\
			      (if late-package-load-path late-packages)\
			      (if last-package-load-path last-packages))))\
	    (while (and dirs (not package-dir))\
	      (if (file-directory-p (car dirs))\
		  (setq package-dir (car dirs)\
			dirs (cdr dirs))))))\
      (if package-dir\
	  (progn\
	    (if (string-match \"/\$\" package-dir)\
		(setq package-dir (substring package-dir 0\
					     (match-beginning 0))))\
	    (if (and prefix\
		     (progn\
		       (setq prefix (file-name-as-directory prefix))\
		       (zerop (string-match (regexp-quote prefix)\
					    package-dir))))\
		(replace-match \"\$(prefix)/\" nil nil package-dir)\
	      package-dir))\
	\"NONE\")),
    noecho)])

AC_DEFUN(AC_PATH_PACKAGEDIR,
 [dnl Check for PACKAGEDIR.
  if test ${EMACS_FLAVOR} = xemacs; then
    AC_MSG_CHECKING([where the XEmacs package is])
    AC_ARG_WITH(packagedir,
      [  --with-packagedir=DIR   package DIR for XEmacs],
      [if test "${withval}" = yes -o -z "${withval}"; then
	AC_EXAMINE_PACKAGEDIR
      else
	PACKAGEDIR="${withval}"
      fi],
      AC_EXAMINE_PACKAGEDIR)
    if test -z "${PACKAGEDIR}"; then
      AC_MSG_RESULT(not found)
    else
      AC_MSG_RESULT(${PACKAGEDIR})
    fi
  else
    PACKAGEDIR=NONE
  fi
  AC_SUBST(PACKAGEDIR)])

AC_DEFUN(AC_PATH_LISPDIR, [
  if test ${EMACS_FLAVOR} = emacs21; then
	tribe=emacs
  else
	tribe=${EMACS_FLAVOR}
  fi
  if test ${prefix} = NONE; then
	AC_MSG_CHECKING([prefix for ${EMACS}])
	AC_EMACS_LISP(prefix,(expand-file-name \"..\" invocation-directory),noecho)
	prefix=${EMACS_cv_SYS_prefix}
	AC_MSG_RESULT(${prefix})
  fi
  AC_ARG_WITH(lispdir,
    [  --with-lispdir=DIR      where lisp files should go
                          (use --with-packagedir for XEmacs package)],
    lispdir="${withval}")
  AC_MSG_CHECKING([where lisp files should go])
  if test -z "${lispdir}"; then
    dnl Set the default value.
    theprefix=${prefix}
    if test x${theprefix} = xNONE; then
	theprefix=${ac_default_prefix}
    fi
    lispdir="\$(datadir)/${tribe}/site-lisp/w3m"
    for thedir in share lib; do
	potential=
	if test -d ${theprefix}/${thedir}/${tribe}/site-lisp; then
	   lispdir="\$(prefix)/${thedir}/${tribe}/site-lisp/w3m"
	   break
	fi
    done
  fi
  if test ${EMACS_FLAVOR} = xemacs; then
    AC_MSG_RESULT(${lispdir}/
         (it will be ignored when \"make install-package\" is done))
  else
    AC_MSG_RESULT(${lispdir}/)
  fi
  AC_SUBST(lispdir)])

AC_DEFUN(AC_PATH_ICONDIR,
 [dnl Examin icon directory.

  dnl Ignore cache.
  unset EMACS_cv_SYS_icondir;

  if test ${EMACS_FLAVOR} = xemacs -o ${EMACS_FLAVOR} = emacs21; then
    AC_ARG_WITH(icondir,
     [  --with-icondir=ICONDIR  directory for icons [$(data-directory)/w3m/icons]],
      ICONDIR="${withval}")
    AC_MSG_CHECKING([where icon files should go])
    if test -z "${ICONDIR}"; then
      dnl Set the default value.
      AC_EMACS_LISP(icondir,
        (let ((prefix \"${prefix}\")\
	      (default (expand-file-name \"w3m/icons\" data-directory)))\
	  (if (and prefix\
		   (progn\
		     (setq prefix (file-name-as-directory prefix))\
		     (zerop (string-match (regexp-quote prefix) default))))\
	      (replace-match \"\$(prefix)/\" nil nil default)\
	    default)),
	${prefix},noecho)
      ICONDIR=${EMACS_cv_SYS_icondir}
    fi
    if test ${EMACS_FLAVOR} = xemacs; then
      AC_MSG_RESULT(${ICONDIR}/
         (it will be ignored when \"make install-package\" is done))
    else
      AC_MSG_RESULT(${ICONDIR})
    fi
  else
    ICONDIR=NONE
  fi
  AC_SUBST(ICONDIR)])

AC_DEFUN(AC_ADD_LOAD_PATH,
 [dnl Check for additional load path.
  AC_ARG_WITH(addpath,
   [  --with-addpath=PATHs    specify additional PATHs for load-path
                          use colons to separate directory names],
   [AC_MSG_CHECKING([where to find the additional elisp libraries])
      if test "x${withval}" != xyes -a "x${withval}" != x; then
	ADDITIONAL_LOAD_PATH="${withval}"
      else
	ADDITIONAL_LOAD_PATH=NONE
      fi
      AC_MSG_RESULT(${ADDITIONAL_LOAD_PATH})],
    ADDITIONAL_LOAD_PATH=NONE)
  AC_SUBST(ADDITIONAL_LOAD_PATH)])

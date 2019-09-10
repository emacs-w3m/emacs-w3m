AC_DEFUN(AC_SET_VANILLA_FLAG,
 [dnl Determine arguments to run Emacs as vanilla.
  VANILLA_FLAG="-q -no-site-file"
  AC_SUBST(VANILLA_FLAG)])

AC_DEFUN(AC_EMACS_LISP, [
elisp="$2"
if test -z "$3"; then
	AC_MSG_CHECKING(for $1)
fi
AC_CACHE_VAL(EMACS_cv_SYS_$1,[
	OUTPUT=./conftest-$$
	EL=./conftest-$$.el
	echo "(let ((x ${elisp})) (write-region (format \"%s\" x) nil \"${OUTPUT}\") (delete-file \"${EL}\"))" > ${EL}
	eval "'${EMACS}' ${VANILLA_FLAG} -batch -l ${EL}" >& AC_FD_CC 2>&1
	if test -f ${OUTPUT}; then
		retval="`cat ${OUTPUT}`"
		echo "=> ${retval}" >& AC_FD_CC 2>&1
		rm -f ${OUTPUT}
		EMACS_cv_SYS_$1="${retval}"
	fi])
$1="${EMACS_cv_SYS_$1}"
if test -z "$3"; then
	AC_MSG_RESULT($$1)
fi
])

AC_DEFUN(AC_PATH_CYGWIN,
 [dnl Do `cygpath -u' for the given argument when running on Cygwin.
  $1=$2
  if test x"${CYGPATH}" != xno -a -n "`echo $$1| ${EGREP} '^[[A-Za-z]]:'`"; then
	$1=`"${CYGPATH}" -u "$$1"`
  fi])

AC_DEFUN(AC_PATH_EMACS,
 [dnl Check for Emacsen.

  dnl Apparently, if you run a shell window or a term window in Emacs,
  dnl it sets the EMACS environment variable to 't' or a version number
  dnl of Emacs.  Lets undo the damage.
  test "${EMACS}" = "t" -o -n "${INSIDE_EMACS}" && EMACS=

  dnl Ignore cache.
  unset ac_cv_prog_EMACS; unset EMACS_cv_SYS_flavor;

  AC_ARG_WITH(emacs,
   [  --with-emacs=EMACS      compile with EMACS [EMACS=emacs]],
   [if test "${withval}" = yes -o -z "${withval}"; then
      AC_PATH_PROGS(EMACS, emacs, emacs)
    else
      AC_PATH_PROG(EMACS, ${withval}, ${withval})
    fi])
  test -z "${EMACS}" && AC_PATH_PROGS(EMACS, emacs, emacs)
  AC_SUBST(EMACS)
  AC_SET_VANILLA_FLAG

  AC_MSG_CHECKING([what a flavor does ${EMACS} have])
  AC_EMACS_LISP(flavor,
    (let ((vers (split-string emacs-version (concat (vector 92 46)))))
      (concat \"Emacs \" (car vers) \".\" (nth 1 vers))),
    noecho)
  case "${flavor}" in
  "")
    flavor=unknown;;
  Emacs\ 2[[5-9]]\.*)
    EMACS_FLAVOR=emacs;;
  *)
    EMACS_FLAVOR=unsupported;;
  esac
  AC_MSG_RESULT(${flavor})
  if test "${flavor}" = unknown; then
    AC_MSG_ERROR(Cannot examine Emacs flavor.)
    exit 1
  else
    if test ${EMACS_FLAVOR} = unsupported; then
      AC_MSG_ERROR(${flavor} is not supported.)
      exit 1
    fi
  fi])

AC_DEFUN(AC_PATH_LISPDIR, [
  if test ${EMACS_FLAVOR} = emacs; then
	tribe=emacs
  else
	tribe=${EMACS_FLAVOR}
  fi
  AC_MSG_CHECKING([prefix for ${EMACS}])
  if test "${prefix}" = NONE; then
	AC_EMACS_LISP(prefix,(expand-file-name \"..\" invocation-directory),noecho)
	AC_PATH_CYGWIN(prefix,"${EMACS_cv_SYS_prefix}")
  fi
  AC_MSG_RESULT(${prefix})
  AC_ARG_WITH(lispdir,
    [  --with-lispdir=DIR      where lisp files should go],
    lispdir="${withval}")
  AC_MSG_CHECKING([where lisp files should go])
  if test -z "${lispdir}"; then
    dnl Set the default value.
    theprefix="${prefix}"
    if test "${theprefix}" = NONE; then
	theprefix=${ac_default_prefix}
    fi
    lispdir="\$(datadir)/${tribe}/site-lisp/w3m"
    for thedir in share lib; do
	potential=
	dnl The directory name should be quoted because it might contain spaces.
	if test -d "${theprefix}/${thedir}/${tribe}/site-lisp"; then
	   lispdir="\$(prefix)/${thedir}/${tribe}/site-lisp/w3m"
	   break
	fi
    done
  fi
  AC_MSG_RESULT(${lispdir})
  AC_SUBST(lispdir)])

AC_DEFUN(AC_PATH_ICONDIR,
 [dnl Examin icon directory.

  dnl Ignore cache.
  unset EMACS_cv_SYS_icondir;

  AC_ARG_WITH(icondir,
   [  --with-icondir=ICONDIR  directory for icons [\$(data-directory)/images/w3m]],
    ICONDIR="${withval}")
  AC_MSG_CHECKING([where icon files should go])
  if test -z "${ICONDIR}"; then
    dnl Set the default value.
    AC_EMACS_LISP(icondir,
      (let ((prefix \"${prefix}\")\
	    (default (expand-file-name \"images/w3m\" data-directory)))\
	    (if (and prefix\
		     (progn\
		      (setq prefix (file-name-as-directory prefix))\
		      (eq 0 (string-match (regexp-quote prefix) default))))\
	      (replace-match \"\$(prefix)/\" nil nil default)\
	      default)),
	noecho)
    AC_PATH_CYGWIN(ICONDIR,"${EMACS_cv_SYS_icondir}")
  fi
  AC_MSG_RESULT(${ICONDIR})
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
	if test x"$USER" != xroot -a x"$HOME" != x -a -f "$HOME"/.emacs; then
          ADDITIONAL_LOAD_PATH=`\'${EMACS}\' -batch -l \'$HOME/.emacs\' -l w3mhack.el NONE -f w3mhack-load-path 2>/dev/null | \'${EGREP}\' -v \'^$\'`
        else
          ADDITIONAL_LOAD_PATH=`\'${EMACS}\' -batch -l w3mhack.el NONE -f w3mhack-load-path 2>/dev/null | \'${EGREP}\' -v \'^$\'`
        fi
      fi
      AC_MSG_RESULT(${ADDITIONAL_LOAD_PATH})],
    ADDITIONAL_LOAD_PATH=NONE)
  AC_ARG_WITH(attic,
   [  --with-attic            use attic libraries for compiling [default: no]
                          (it does not mean installing attic libraries)],
   [if test "x${withval}" = xyes; then
      if test x"$ADDITIONAL_LOAD_PATH" = xNONE; then
        ADDITIONAL_LOAD_PATH="`pwd`/attic"
      else
        ADDITIONAL_LOAD_PATH="${ADDITIONAL_LOAD_PATH}:`pwd`/attic"
      fi
    fi])
  retval=`eval "'${EMACS}' ${VANILLA_FLAG} -batch -l w3mhack.el '${ADDITIONAL_LOAD_PATH}' -f w3mhack-print-status 2>/dev/null | '${EGREP}' -v '^$'"`
  if test x"$retval" != xOK; then
    AC_MSG_ERROR([Process couldn't proceed.  See the above messages.])
  fi
  AC_SUBST(ADDITIONAL_LOAD_PATH)])

AC_DEFUN(AC_COMPRESS_INSTALL,
 [dnl Check for the `--with(out)-compress-install' option.
  AC_PATH_PROG(GZIP_PROG, gzip)
  AC_ARG_WITH(compress-install,
	[  --without-compress-install
                          do not compress .el and .info files when installing],
	[if test "${withval}" = no; then
		COMPRESS_INSTALL=no;
	  else
		if test -n "${GZIP_PROG}"; then
			COMPRESS_INSTALL=yes;
		else
			COMPRESS_INSTALL=no;
		fi;
	  fi],
	[if test -n "${GZIP_PROG}"; then
		COMPRESS_INSTALL=yes;
	  else
		COMPRESS_INSTALL=no;
	  fi])
  AC_SUBST(COMPRESS_INSTALL)])

AC_DEFUN(AC_CHECK_TEXINFO,
 [dnl Check the existence and the version of a texinfo command.
  command=$1
  if test "${command}" != no; then
    AC_MSG_CHECKING([if ${command} version >= 6.3])
    version=`${command} --version\
      | awk 'BEGIN {zero=0}\
        tolower($zero)~/gnu +texinfo/&&match($zero,/[[0-9]]+\.[[0-9]]+/)\
        {print substr($zero,RSTART,RLENGTH); exit}' 2>/dev/null`
    case "${version}" in
    6\.[[3-9]]|[[7-9]]\.[[0-9]])
      AC_MSG_RESULT([ok (${version})]);;
    *)
      if test -z "${version}"; then
        AC_MSG_RESULT(no)
      else
        AC_MSG_RESULT([no (${version})])
      fi;;
    esac
  fi])

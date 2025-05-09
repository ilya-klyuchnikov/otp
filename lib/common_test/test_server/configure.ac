dnl Process this file with autoconf to produce a configure script for Erlang.
dnl
dnl %CopyrightBegin%
dnl
dnl SPDX-License-Identifier: Apache-2.0
dnl
dnl Copyright Ericsson AB 1997-2025. All Rights Reserved.
dnl
dnl Licensed under the Apache License, Version 2.0 (the "License");
dnl you may not use this file except in compliance with the License.
dnl You may obtain a copy of the License at
dnl
dnl     http://www.apache.org/licenses/LICENSE-2.0
dnl
dnl Unless required by applicable law or agreed to in writing, software
dnl distributed under the License is distributed on an "AS IS" BASIS,
dnl WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
dnl See the License for the specific language governing permissions and
dnl limitations under the License.
dnl
dnl %CopyrightEnd%
dnl 

AC_INIT
AC_CONFIG_SRCDIR([conf_vars.in])
AC_PREREQ([2.72])

AC_CANONICAL_HOST

dnl Checks for programs.
AC_PROG_CC

DEBUG_FLAGS="-g -DDEBUG"
if test "$GCC" = yes; then
	DEBUG_FLAGS="$DEBUG_FLAGS -Wall $CFLAGS"
fi
AC_SUBST(DEBUG_FLAGS)

AC_ARG_ENABLE(debug-mode,
[  --enable-debug-mode           enable debug mode],
[ case "$enableval" in
    no) ;;
    *)  CFLAGS=$DEBUG_FLAGS ;;
  esac ], )

AC_ARG_ENABLE(m64-build,
AS_HELP_STRING([--enable-m64-build],
               [build 64-bit binaries using the -m64 flag to (g)cc]),
[ case "$enableval" in
    no) enable_m64_build=no ;;
    *)  enable_m64_build=yes ;;
  esac
],enable_m64_build=no)

AC_ARG_ENABLE(m32-build,
AS_HELP_STRING([--enable-m32-build],
               [build 32-bit binaries using the -m32 flag to (g)cc]),
[ case "$enableval" in
    no) enable_m32_build=no ;;
    *)  enable_m32_build=yes ;;
  esac
],enable_m32_build=no)

no_mXX_LDFLAGS="$LDFLAGS"

if test X${enable_m64_build} = Xyes; then
	CFLAGS="-m64 $CFLAGS"
	LDFLAGS="-m64 $LDFLAGS"
fi
if test X${enable_m32_build} = Xyes; then
	CFLAGS="-m32 $CFLAGS"
	LDFLAGS="-m32 $LDFLAGS"
fi

# Large file support and 8-byte time_t by default
AC_SYS_YEAR2038_RECOMMENDED

AC_CHECK_LIB(m, sin)

#--------------------------------------------------------------------
#	Interactive UNIX requires -linet instead of -lsocket, plus it
#	needs net/errno.h to define the socket-related error codes.
#--------------------------------------------------------------------

AC_CHECK_LIB(inet, main, [LIBS="$LIBS -linet"])
AC_CHECK_HEADER(net/errno.h, AC_DEFINE(HAVE_NET_ERRNO_H))

#--------------------------------------------------------------------
# Linux/tcp.h may be needed for sockopt test in kernel
#--------------------------------------------------------------------

AC_CHECK_HEADER(linux/tcp.h, AC_DEFINE(HAVE_LINUX_TCP_H))
AC_MSG_CHECKING(for sane linux/tcp.h)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <stdio.h>
		#include <stdlib.h>
		#include <string.h>
		#include <unistd.h>
		#include <stdarg.h>
		#include <sys/types.h>
		#include <sys/socket.h>
		#include <sys/wait.h>
		#include <linux/tcp.h>
		#include <netinet/in.h>
		#include <netdb.h>]], [[return 0;]])],[have_sane_linux_tcp_h=yes],[have_sane_linux_tcp_h=no])

if test $have_sane_linux_tcp_h = yes; then
	AC_DEFINE(HAVE_SANE_LINUX_TCP_H,[1],
		  [Define if we have sane linux/tcp.h])
	AC_MSG_RESULT(yes)
else
	AC_MSG_RESULT(no)
fi



#--------------------------------------------------------------------
#	Linux requires sys/socketio.h instead of sys/sockio.h
#--------------------------------------------------------------------
AC_CHECK_HEADER(sys/socketio.h, AC_DEFINE(HAVE_SOCKETIO_H))


#--------------------------------------------------------------------
#	Misc
#--------------------------------------------------------------------
AC_CHECK_HEADER(poll.h, AC_DEFINE(HAVE_POLL_H))

#--------------------------------------------------------------------
#	The statements below define a collection of symbols related to
#	dynamic loading and shared libraries:
#
#	SHLIB_CFLAGS -	Flags to pass to cc when compiling the components
#			of a shared library (may request position-independent
#			code, among other things).
#	SHLIB_LD -	Base command to use for combining object files
#			into a shared library.
#	SHLIB_SUFFIX -	Suffix to use for the names of dynamically loadable
#			extensions.  An empty string means we don't know how
#			to use shared libraries on this platform.
#--------------------------------------------------------------------

# Step 1: set the variable "system" to hold the name and version number
# for the system.

AC_MSG_CHECKING([system version (for dynamic loading)])
system=`./config.sub $host`
AC_MSG_RESULT($system)

# Step 2: check for existence of -ldl library.  This is needed because
# Linux can use either -ldl or -ldld for dynamic loading.

AC_CHECK_LIB(dl, dlopen, have_dl=yes, have_dl=no)

# Step 3: set configuration options based on system name and version.

SHLIB_LDLIBS=
fullSrcDir=`cd $srcdir; pwd`
AS_CASE([$system],

    [*-linux-*],
    [
	SHLIB_CFLAGS="-fPIC"
	SHLIB_SUFFIX=".so"
	if test "$have_dl" = yes; then
	    SHLIB_LD="${CC}"
	    SHLIB_LDFLAGS="$LDFLAGS -shared"
	    LD_FLAGS="-rdynamic"
	else
	    AC_CHECK_HEADER(dld.h, [
		SHLIB_LD="ld"
		SHLIB_LDFLAGS="-shared"])
	    if test X${enable_m64_build} = Xyes; then
	        AC_MSG_ERROR(don't know how to link 64-bit dynamic drivers)
	    fi
	    if test X${enable_m32_build} = Xyes; then
	        AC_MSG_ERROR(don't know how to link 32-bit dynamic drivers)
	    fi
	fi
    ],

    [*-openbsd*|*-netbsd*|*-freebsd*|*-dragonfly*],
    [
	# Not available on all versions:  check for include file.
	AC_CHECK_HEADER(dlfcn.h, [
	    SHLIB_CFLAGS="-fpic"
	    SHLIB_LD="${CC}"
	    SHLIB_LDFLAGS="$LDFLAGS -shared"
	    SHLIB_SUFFIX=".so"
	    if test X${enable_m64_build} = Xyes; then
	        AC_MSG_ERROR(don't know how to link 64-bit dynamic drivers)
	    fi
	    if test X${enable_m32_build} = Xyes; then
	        AC_MSG_ERROR(don't know how to link 32-bit dynamic drivers)
	    fi
	], [
	    # No dynamic loading.
	    SHLIB_CFLAGS=""
	    SHLIB_LD="ld"
	    SHLIB_LDFLAGS=""
	    SHLIB_SUFFIX=""
	    AC_MSG_ERROR(don't know how to compile and link dynamic drivers)
	])
    ],

    [*-solaris2*|*-sysv4*],
    [
	SHLIB_CFLAGS="-KPIC"
	SHLIB_LD="${CC}"
	SHLIB_LDFLAGS="$no_mXX_LDFLAGS -G -Wl,-z,text"
	if test X${enable_m64_build} = Xyes; then
	   SHLIB_LDFLAGS="-64 $SHLIB_LDFLAGS"
	fi
	if test X${enable_m32_build} = Xyes; then
	   AC_MSG_ERROR(don't know how to link 32-bit dynamic drivers)
	fi
	SHLIB_SUFFIX=".so"
    ],

    [*darwin*],
    [
	SHLIB_CFLAGS="-fno-common"
	SHLIB_LD="cc"
	SHLIB_LDFLAGS="$LDFLAGS -bundle -flat_namespace -undefined suppress"
	SHLIB_SUFFIX=".so"
    ],

    [*osf1*],
    [
	SHLIB_CFLAGS="-fPIC"
	SHLIB_LD="ld"
	SHLIB_LDFLAGS="$LDFLAGS -shared"
	if test X${enable_m64_build} = Xyes; then
	   AC_MSG_ERROR(don't know how to link 64-bit dynamic drivers)
	fi
	if test X${enable_m32_build} = Xyes; then
	   AC_MSG_ERROR(don't know how to link 32-bit dynamic drivers)
	fi
	SHLIB_SUFFIX=".so"
    ],

    [*osf5*],
    [
	SHLIB_CFLAGS="-fPIC"
	SHLIB_LD="${CC} -shared"
	SHLIB_LDFLAGS="$LDFLAGS"
	SHLIB_SUFFIX=".so"
    ],

    [
	# No dynamic loading.
	SHLIB_CFLAGS=""
	SHLIB_LD="ld"
	SHLIB_LDFLAGS=""
	SHLIB_LDLIBS=""
	SHLIB_SUFFIX=""
	AC_MSG_ERROR(don't know how to compile and link dynamic drivers)
    ])

# If we're running gcc, then change the C flags for compiling shared
# libraries to the right flags for gcc, instead of those for the
# standard manufacturer compiler.

if test "$CC" = "gcc" -o `$CC -v 2>&1 | grep -c gcc` != "0" ; then
	case $system in
	    *-aix)
		;;
	    *-bsd*)
		;;
	    *-irix)
		;;
	    *-netbsd|*-freebsd|*-openbsd)
		;;
	    *-riscos)
		;;
	    *ultrix4.*)
		;;
	    *darwin*)
		;;
	    *)
		SHLIB_CFLAGS="-fPIC"
		;;
	esac
fi

# Make it possible for erl_interface to use it's own compiler options
EI_CFLAGS="$CFLAGS"

# Add thread-safety flags if requested
AC_ARG_ENABLE(shlib-thread-safety,
[  --enable-shlib-thread-safety  enable thread safety for build shared libraries],
[ case "$enableval" in
    no) ;;
    *)  SHLIB_CFLAGS="$SHLIB_CFLAGS -D_THREAD_SAFE -D_REENTRANT"
	CFLAGS="$CFLAGS -D_THREAD_SAFE -D_REENTRANT"
	;;
  esac ], )

SHLIB_CFLAGS="$SHLIB_CFLAGS $CFLAGS"


AC_SUBST(CFLAGS)
AC_SUBST(SHLIB_LD)
AC_SUBST(SHLIB_LDFLAGS)
AC_SUBST(SHLIB_LDLIBS)
AC_SUBST(SHLIB_CFLAGS)
AC_SUBST(SHLIB_SUFFIX)
AC_SUBST(EI_CFLAGS)

#--------------------------------------------------------------------
#	Check for the existence of the -lsocket and -lnsl libraries.
#	The order here is important, so that they end up in the right
#	order in the command line generated by make.  Here are some
#	special considerations:
#	1. Use "connect" and "accept" to check for -lsocket, and
#	   "gethostbyname" to check for -lnsl.
#	2. Use each function name only once:  can't redo a check because
#	   autoconf caches the results of the last check and won't redo it.
#	3. Use -lnsl and -lsocket only if they supply procedures that
#	   aren't already present in the normal libraries.  This is because
#	   IRIX 5.2 has libraries, but they aren't needed and they're
#	   bogus:  they goof up name resolution if used.
#	4. On some SVR4 systems, can't use -lsocket without -lnsl too.
#	   To get around this problem, check for both libraries together
#	   if -lsocket doesn't work by itself.
#--------------------------------------------------------------------

erl_checkBoth=0
AC_CHECK_FUNC(connect, erl_checkSocket=0, erl_checkSocket=1)
AS_IF([test "$erl_checkSocket" = 1],
  [
    AC_CHECK_LIB(socket, main, LIBS="$LIBS -lsocket", erl_checkBoth=1)
  ])
AS_IF([test "$erl_checkBoth" = 1],
  [
    tk_oldLibs=$LIBS
    LIBS="$LIBS -lsocket -lnsl"
    AC_CHECK_FUNC(accept, erl_checkNsl=0, [LIBS=$tk_oldLibs])
  ])
AC_CHECK_FUNC(gethostbyname, , AC_CHECK_LIB(nsl, main, [LIBS="$LIBS -lnsl"]))

dnl Checks for library functions.
AC_CHECK_FUNCS(strerror)
AC_CHECK_FUNCS(vsnprintf)
AC_CHECK_FUNCS(usleep)

# First check if the library is available, then if we can choose between
# two versions of gethostbyname
AC_CHECK_LIB([resolv],[main],[],[],[])ac_cv_lib_resolv=ac_cv_lib_resolv_main

AC_CHECK_LIB(resolv, res_gethostbyname,[AC_DEFINE(HAVE_RES_GETHOSTBYNAME,1)])

#--------------------------------------------------------------------
# Check for isfinite
#--------------------------------------------------------------------

AC_MSG_CHECKING([for isfinite])
AC_LINK_IFELSE([AC_LANG_PROGRAM([[#include <math.h>]], [[isfinite(0);]])],[have_isfinite=yes],[have_isfinite=no])

if test $have_isfinite = yes; then
    AC_DEFINE(HAVE_ISFINITE,1)
    AC_MSG_RESULT(yes)
else
    AC_DEFINE(HAVE_FINITE,1)
    AC_MSG_RESULT(no)
fi

#--------------------------------------------------------------------
# Emulator compatible flags (for drivers)
#--------------------------------------------------------------------

ERTS_CFLAGS=$CFLAGS
AC_SUBST(ERTS_CFLAGS)

ERTS_LIBS=$LIBS
AC_SUBST(ERTS_LIBS)

#--------------------------------------------------------------------
# Special compiler macro to handle cross compiling
# (HCC) is used to compile tools run in the HOST environment
#--------------------------------------------------------------------
HCC='$(CC)'
AC_SUBST(HCC)

#--------------------------------------------------------------------
# ld
#--------------------------------------------------------------------
LD='$(CC) $(CFLAGS)'
AC_SUBST(LD)

#--------------------------------------------------------------------
# object file suffix
#--------------------------------------------------------------------
obj='.o'
AC_SUBST(obj)

#--------------------------------------------------------------------
# executable file suffix
#--------------------------------------------------------------------
exe=''
AC_SUBST(exe)

#--------------------------------------------------------------------
# wsl command (if on windows using wsl)
#--------------------------------------------------------------------
wsl=''
AC_SUBST(wsl)

#--------------------------------------------------------------------
# flags when linking for cross platform targets (yet 'tis useful with
# native builds)
#--------------------------------------------------------------------
CROSSLDFLAGS=''
AC_SUBST(CROSSLDFLAGS)

dnl
dnl SSL and CRYPTO needs the library openSSL/ssleay
dnl
dnl Check flags --with-ssl, --without-ssl --with-ssl=PATH.
dnl If no option is given or --with-ssl is set without a path then we
dnl search for SSL libraries and header files in the standard locations. 
dnl If set to --without-ssl we disable the use of SSL
dnl If set to --with-ssl=PATH we use that path as the prefix, i.e. we
dnl use "PATH/include" and "PATH/lib".

AC_SUBST(SSLEAY_ROOT)
TARGET=$host

# We search for SSL. First in the OTP team ClearCase standard location,
# then in the common OS standard locations
# No we do not.  
SSL_APP=ssl
CRYPTO_APP=crypto
SSLEAY_ROOT=$TARGET
#for dir in /usr /usr/pkg /usr/local /usr/local/ssl /usr/lib/ssl /usr/ssl; do
#         AC_CHECK_HEADER($dir/include/openssl/opensslv.h,
#		ac_cv_openssl=yes, ac_cv_openssl=no)
#	 if test $ac_cv_openssl = yes ; then
#                SSLEAY_ROOT="$dir"
#		ssl_found=yes
#		break
#	 fi
#done

# Find a usable java compiler
#
# WARNING this code is copied from ERTS configure.in, and should be
# updated if that code changes. I hate duplicating code, but what
# can I do.
#
dnl ERL_TRY_LINK_JAVA(CLASSES, FUNCTION-BODY
dnl                   [ACTION_IF_FOUND [, ACTION-IF-NOT-FOUND]])
dnl Freely inspired by AC_LINK_IFELSE([AC_LANG_PROGRAM([[]], [[]])],[],[]). (Maybe better to create a 
dnl AC_LANG_JAVA instead...)
AC_DEFUN(ERL_TRY_LINK_JAVA,
[java_link='$JAVAC conftest.java 1>&AS_MESSAGE_LOG_FD'
changequote(, )dnl
cat > conftest.java <<EOF
$1
class conftest { public static void main(String[] args) {
   $2
   ; return; }}
EOF
changequote([, ])dnl
AS_IF([AC_TRY_EVAL(java_link) && test -s conftest.class],
 [
   ifelse([$3], , :, [rm -rf conftest*
   $3])
 ],
 [
   echo "configure: failed program was:" 1>&AS_MESSAGE_LOG_FD
   cat conftest.java 1>&AS_MESSAGE_LOG_FD
   echo "configure: PATH was $PATH" 1>&AS_MESSAGE_LOG_FD
ifelse([$4], , , [  rm -rf conftest*
  $4
])dnl
 ])
rm -f conftest*])
dnl
AC_CHECK_PROGS(JAVAC, javac guavac gcj jikes bock)
AS_IF([test -n "$JAVAC"],
 [
  dnl Make sure it's at least JDK 1.5
  AC_CACHE_CHECK(for JDK version 1.5, 
     ac_cv_prog_javac_ver_1_5,
     [ERL_TRY_LINK_JAVA([], [for (String i : args);],
        ac_cv_prog_javac_ver_1_5=yes, ac_cv_prog_javac_ver_1_5=no)])
  if test $ac_cv_prog_javac_ver_1_5 = no; then
    unset -v JAVAC
  fi
 ])
if test -n "$JAVAC"; then
  AC_SUBST(JAVAC)
  :
fi

AC_CHECK_PROGS([make_command], [gmake make], [false])
AC_SUBST(make_command)

if test "$GCC" = yes; then
  test_c_compiler="{gnuc, undefined}"
else
  test_c_compiler="undefined"
fi
AC_SUBST(test_c_compiler)

AC_CONFIG_FILES([conf_vars])
AC_OUTPUT

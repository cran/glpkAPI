AC_INIT([glpkAPI], [1.1.5], [geliudie@uni-duesseldorf.de])

dnl # --------------------------------------------------------------------
dnl # R compiler and flags
dnl # --------------------------------------------------------------------

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
    echo "could not determine R_HOME"
    exit 1
fi

dnl # GNU R default settings
CC=`"${R_HOME}/bin/R" CMD config CC`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`
AC_PROG_CC


dnl # --------------------------------------------------------------------
dnl # get arguments
dnl # --------------------------------------------------------------------

AC_ARG_WITH(glpk-include,
            AC_HELP_STRING([--with-glpk-include=PATH],
                           [location of glpk include directory [[/usr/local/include]]]
                          ),
            [GLPK_INCLUDE=$withval],
            [GLPK_INCLUDE=""])

AC_ARG_WITH(glpk-lib,
            AC_HELP_STRING([--with-glpk-lib=PATH],
                           [location of GLPK callable library [[/usr/local/lib]]]
                          ),
            [GLPK_LIB=$withval],
            [GLPK_LIB=""])

AC_ARG_ENABLE(gmp,
              AC_HELP_STRING([--enable-gmp[=yes|no]],
                             [link to GNU bignum library libgmp [[yes]]]
                            ),
              [GMP=$enableval], [GMP="yes"])


dnl # --------------------------------------------------------------------
dnl # test arguments
dnl # --------------------------------------------------------------------

dnl # include and library directories
if test [ -n "$GLPK_INCLUDE" -a -n "$GLPK_LIB" ] ; then

    if test [ ! -d "$GLPK_INCLUDE" ] ; then
        AC_MSG_ERROR([directory $GLPK_INCLUDE does not exist])
    else
        GLPK_CPPFLAGS="-I${GLPK_INCLUDE}"
    fi

    if test [ ! -d "$GLPK_LIB" ] ; then
        AC_MSG_ERROR([directory $GLPK_LIB does not exist])
    else
        LIBS="-L${GLPK_LIB}"
        # LIBS="-L${GLPK_LIB} -Wl,-rpath ${GLPK_LIB}"
    fi

else

dnl # if no arguments are given, try to find the glpsol executable. If is not
dnl # found, use /usr/local

    AC_PREFIX_PROGRAM(glpsol)

    if test [ "$prefix" = "NONE" ] ; then

        AC_MSG_NOTICE([No prefix given, trying -I/usr/local/include and -L/usr/local/lib])

        if test [ -d "/usr/local/include" -a -d "/usr/local/lib" ] ; then
            GLPK_CPPFLAGS="-I/usr/local/include"
            LIBS="-L/usr/local/lib"
            # LIBS="-L/usr/local/lib -Wl,-rpath /usr/local/lib"
        else
            AC_MSG_ERROR([directories /usr/local/include and /usr/local/lib do not exist])
        fi

    else

        if test [ -d "$prefix/include" -a -d "$prefix/lib" ] ; then
            GLPK_CPPFLAGS="-I$prefix/include"
            LIBS="-L$prefix/lib"
            # LIBS="-L$prefix/lib -Wl,-rpath $prefix/lib"
        else
            AC_MSG_ERROR([directories $prefix/include and $prefix/lib do not exist])
        fi

    fi

fi

CPPFLAGS="${CPPFLAGS} ${GLPK_CPPFLAGS}"

INCL="-lglpk"

if test [ "${GMP}" != "no" ] ; then
    INCL="$INCL -lgmp"
fi

LIBS="$LIBS $INCL"


dnl # --------------------------------------------------------------------
dnl # CFLAGS
dnl # --------------------------------------------------------------------

OS_NAME=`uname -s`
MACHINE_NAME=`uname -m`

GLPK_CFLAGS="-Wall"

dnl # OS dependent
if test [ "${OS_NAME}" = "Darwin" ] ; then
    GLPK_CFLAGS="${GLPK_CFLAGS} -ansi"
else
    GLPK_CFLAGS="${GLPK_CFLAGS} -std=c99"
    if test [ "${MACHINE_NAME}" = "x86_64" ] ; then
        GLPK_CFLAGS="${GLPK_CFLAGS} -m64"
    else
        GLPK_CFLAGS="${GLPK_CFLAGS} -m32"
    fi
fi


dnl # compiler dependent
if test [ "${CC:0:3}" = "icc" ] ; then

dnl # for icc: disable remark #981: operands are evaluated in unspecified order
    GLPK_CFLAGS="-wd981"
else
    if test [ "${CC:0:3}" = "gcc" ] ; then
        GLPK_CFLAGS="${GLPK_CFLAGS} -Wextra -Wunknown-pragmas"
        GLPK_CFLAGS="${GLPK_CFLAGS} -Wconversion -Wwrite-strings"
        GLPK_CFLAGS="${GLPK_CFLAGS} -Wcast-qual"
        GLPK_CFLAGS="${GLPK_CFLAGS} -pedantic"
    fi
fi


dnl # --------------------------------------------------------------------
dnl # check header and library
dnl # --------------------------------------------------------------------

AC_CHECK_HEADER([glpk.h],,
    AC_MSG_ERROR([Could not find glpk.h:
      glpkAPI requires GLPK from http://www.gnu.org/software/glpk/
      use --with-glpk-include or GLPK_INCLUDE to specify the include path.]))


AC_SEARCH_LIBS([glp_create_prob], [glpk], ,
    AC_MSG_ERROR([Could not link to GLPK:
      use --with-glpk-lib or GLPK_LIB to specify the lib path.]))

dnl # GLPK version
AC_CHECK_FUNC([glp_print_ranges], ,
    AC_MSG_ERROR([GLPK version must be >= 4.42.]))


dnl # --------------------------------------------------------------------
dnl # substitute src/Makevars
dnl # --------------------------------------------------------------------

AC_SUBST(GLPK_CFLAGS)
AC_SUBST(GLPK_CPPFLAGS)
AC_SUBST(LIBS)
AC_OUTPUT(src/Makevars)

exit 0

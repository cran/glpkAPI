AC_INIT([glpkAPI], [1.1.7], [geliudie@uni-duesseldorf.de])

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

    if test [ -d "$GLPK_INCLUDE" ] ; then
        GLPK_CPPFLAGS="-I${GLPK_INCLUDE}"
    else
        AC_MSG_ERROR([directory $GLPK_INCLUDE does not exist])
    fi

    if test [ -d "$GLPK_LIB" ] ; then
        LIBS="-L${GLPK_LIB}"
        # LIBS="-L${GLPK_LIB} -Wl,-rpath ${GLPK_LIB}"
    else
        AC_MSG_ERROR([directory $GLPK_LIB does not exist])
    fi

else

dnl # if no arguments are given, try to find the glpsol executable. If is not
dnl # found, use /usr/local and /usr

    AC_PREFIX_PROGRAM(glpsol)

    if test [ "$prefix" = "NONE" ] ; then

        AC_MSG_NOTICE([No prefix given, trying /usr and /usr/local])

        if test [ -d "/usr/include" -o -d "/usr/local/include" ] ; then
            GLPK_CPPFLAGS="-I/usr/include -I/usr/local/include"
        else
            AC_MSG_ERROR([directories /usr/include and /usr/local/include do not exist])
        fi

        if test [ -d "/usr/local/lib" -o -d "/usr/local/lib64" -o -d "/usr/lib" -o -d "/usr/lib64"] ; then
            LIBS=""
            if test [ -d "/usr/local/lib" ] ; then
                LIBS="$LIBS -L/usr/local/lib"
            fi
            if test [ -d "/usr/local/lib64" ] ; then
                LIBS="$LIBS -L/usr/local/lib64"
            fi
            if test [ -d "/usr/lib" ] ; then
                LIBS="$LIBS -L/usr/lib"
            fi
            if test [ -d "/usr/lib64" ] ; then
                LIBS="$LIBS -L/usr/lib64"
            fi
            # LIBS="-L/usr/local/lib -Wl,-rpath /usr/local/lib"
        else
            AC_MSG_ERROR([none of the directories /usr/local/lib, /usr/local/lib64, /usr/lib of /usr/lib64 exists])
        fi

    else

        AC_MSG_NOTICE([found executable glpsol in $prefix])
        
        if test [ -d "$prefix/include" ] ; then
            GLPK_CPPFLAGS="-I$prefix/include"
        else
            AC_MSG_ERROR([directory $prefix/include does not exist])
        fi

        if test [ -d "$prefix/lib" -o -d "$prefix/lib64" ] ; then
            LIBS=""
            if test [ -d "$prefix/lib" ] ; then
                LIBS="$LIBS -L$prefix/lib"
            fi
            if test [ -d "$prefix/lib64" ] ; then
                LIBS="$LIBS -L$prefix/lib64"
            fi
        else
            AC_MSG_ERROR([neither $prefix/lib nor $prefix/lib64 exist])
        fi

dnl #        if test [ -d "$prefix/include" -a -d "$prefix/lib" ] ; then
dnl #            GLPK_CPPFLAGS="-I$prefix/include"
dnl #            LIBS="-L$prefix/lib"
dnl #            # LIBS="-L$prefix/lib -Wl,-rpath $prefix/lib"
dnl #        else
dnl #            AC_MSG_ERROR([directories $prefix/include and $prefix/lib do not exist])
dnl #        fi

    fi

fi

CPPFLAGS="${CPPFLAGS} ${GLPK_CPPFLAGS}"

if test [ "${GMP}" != "no" ] ; then
    dnl # AC_CHECK_HEADER([gmp.h], INCL="$INCL -lgmp", AC_MSG_NOTICE([Could not link to gmp.]))
    INCL="-lglpk -lgmp"
else
    INCL="-lglpk"
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
      use --with-glpk-lib or GLPK_LIB to specify the lib path.
      Maybe you want to set --enable-glpk=no]))

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

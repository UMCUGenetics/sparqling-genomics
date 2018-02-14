dnl Copyright © 2017, 2018 Roel Janssen <roel@gnu.org>
dnl
dnl This program is free software: you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or
dnl (at your option) any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program. If not, see <http://www.gnu.org/licenses/>.
dnl -*- Autoconf -*-

AC_INIT(sparqling-svs, 0.0.1)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

AC_PROG_CC
AM_PROG_CC_C_O
AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h stdio.h])

GUILE_PKG([2.2 2.0])
GUILE_PROGS

dnl Installation directories for .scm and .go files.
moddir="${prefix}/share/guile/site/$GUILE_EFFECTIVE_VERSION"
godir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([moddir])
AC_SUBST([godir])

if $GUILE_TOOLS | grep -q compile; then
   true
else
   AC_MSG_ERROR([Guile 2.2 required.])
fi


AM_PATH_LIBGCRYPT(1.5.3)

if test "x${LIBGCRYPT_LIBS}" = "x"; then
	AC_MSG_ERROR([unable to find libgcrypt])
fi

AC_CONFIG_FILES([env], [chmod +x env])
AC_CONFIG_FILES([web/run], [chmod +x web/run])
AC_CONFIG_FILES([
  Makefile
  doc/Makefile
  tools/vcf2turtle/Makefile
  tools/vcf2rdf/Makefile
  web/Makefile
])

PKG_PROG_PKG_CONFIG
PKG_CHECK_MODULES([htslib], [htslib])
PKG_CHECK_MODULES([redland], [redland])
PKG_CHECK_MODULES([raptor2], [raptor2])
PKG_CHECK_MODULES([rasqal], [rasqal])

dnl Check for presence of 'pdflatex'
AC_CHECK_PROG(PDFLATEX, pdflatex, pdflatex)

if test "x$PDFLATEX" = "x"; then
  AC_MSG_WARN([Unable to create PDF version of the user manual.])
fi

AM_CONDITIONAL([HAVE_PDFLATEX], test -n "$PDFLATEX")

AC_OUTPUT

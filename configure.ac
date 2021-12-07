#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#
# Maintainer: SAITO Fuyuki
# Created: Jun 5 2020
# Time-stamp: <2021/11/28 13:24:13 fuyuki configure.ac>

AC_PREREQ([2.71])

MT_INIT()
AC_INIT([touza],MT_VERSION,[saitofuyuki@jamstec.go.jp])

AC_COPYRIGHT([dnl
Copyright (C) 2020, 2021 Japan Agency for Marine-Earth Science and Technology

This is TOUZA, a collection of MIROC base infrastructure.

Licensed under the Apache License, Version 2.0 (http://www.apache.org/licenses/LICENSE-2.0)
])

AC_CONFIG_SRCDIR([touza.F90])
AC_LANG_PUSH([Fortran])
MT_PROG_FC_MPI
dnl AC_PROG_FC
AC_PROG_F77

AC_FC_PP_SRCEXT([F90])
AC_FC_PP_DEFINE
AC_FC_FREEFORM

AC_FC_MODULE_EXTENSION
AC_FC_MODULE_FLAG
AC_FC_MODULE_OUTPUT_FLAG([], [AC_MSG_WARN])
AX_COMPILER_VENDOR
AC_LANG_POP([Fortran])

AC_ARG_ENABLE([warn-max],
[AS_HELP_STRING([--enable-warn-max], [compile with maximum warning option if found @<:@default=no@:>@])],
 [use_warn_max=$enableval],
 [use_warn_max=no])
AS_IF([test x"$use_warn_max" = xyes], [AX_FCFLAGS_WARN_ALL([AM_FCFLAGS])])
AC_SUBST([AM_FCFLAGS])

dnl common procedures
MT_PACKAGE_INIT()
MT_AM_INCLUDE()

AC_ARG_PROGRAM
AC_CONFIG_MACRO_DIRS([m4c])
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([-Wall foreign])
AM_MAINTAINER_MODE
AM_CONDITIONAL([NEVER], [false])

AC_PROG_MAKE_SET

MT_FORTRAN_DEFINE([PACKAGE_TAG], m4_quote(m4_toupper(AC_PACKAGE_NAME)), [package name])

AC_MSG_CHECKING([if transform library name])
AC_ARG_WITH([touza-name],
[AS_HELP_STRING([--with-touza-name=STRING],
                [replace "lib]AC_PACKAGE_NAME[" with "libSTRING" @<:@default=]AC_PACKAGE_NAME[@:>@])])
AS_IF([test "$with_touza_name" = "no"],
[AC_MSG_RESULT([Error])
 AC_MSG_ERROR([Invalid to specify --without-touza-name])])

AS_IF([test "$with_touza_name" = "" || test "$with_touza_name" = "yes"],
      [with_touza_name="$PACKAGE_NAME"])
AC_MSG_RESULT([$with_touza_name])
AC_SUBST([TOUZA_NAME], $with_touza_name)

AM_PROG_AR
LT_PREREQ([2.4.2])
LT_INIT([disable-shared])

m4_define([TOUZA_config],
          [m4_join([_], AC_PACKAGE_NAME, [config.h])])

AC_CONFIG_HEADERS(m4_expand(TOUZA_config))
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([m4c/Makefile])
AC_CONFIG_FILES([makeshift], [chmod +x makeshift])

AC_MSG_CHECKING([if enable elemental function])
AC_ARG_ENABLE([elemental],
[AS_HELP_STRING([--disable-elemental], [disable elemental functions])],
[], [enableval=yes])
AS_IF([test x"$enableval" = x"no"],
      [enable_elemental=0
       AC_MSG_RESULT([disable])],
      [enable_elemental=1
       AC_MSG_RESULT([enable])])
AC_DEFINE_UNQUOTED([OPT_ENABLE_FORTRAN_ELEMENTAL], [$enable_elemental], [prefer elemental function])

MT_SUB_PACKAGE([std], [always])
MT_SUB_PACKAGE([cal], [],     [std])
MT_SUB_PACKAGE([trp], [],     [std])
MT_SUB_PACKAGE([nng], [],     [std trp])
dnl MT_SUB_PACKAGE([emu], [],     [std])
dnl MT_SUB_PACKAGE([div], [],     [std])

AC_MSG_CHECKING([if also build jmz])
AC_ARG_WITH([build-jmz],
   [AS_HELP_STRING([--with-build-jmz=(yes|no)],
                   [whether to build jmz programs.  @<:@default=no@:>@])],
   [], [with_build_jmz=no])
AS_IF([test "x$with_build_jmz" != xno],
      [AC_MSG_RESULT([yes])
       enable_sub_all=yes],
      [AC_MSG_RESULT([no])])

MT_PARSE_PACKAGES()

AS_IF([test "x$with_build_jmz" != xno],
[AX_SUBDIRS_CONFIGURE([jmz],
    [],
    [[TOUZA_CFLAGS=-I${ac_top_build_prefix}/ -I${ac_top_srcdir} ${FC_MODINC}${ac_top_build_prefix}nng ${FC_MODINC}${ac_top_build_prefix}trp ${FC_MODINC}${ac_top_build_prefix}std],
     [TOUZA_LIBS=${ac_top_build_prefix}lib${with_touza_name}.la]],
    [],
    [--with-build-jmz])])

PKG_PROG_PKG_CONFIG()
PKG_PREREQ([0.29])

dnl AX_CREATE_PKGCONFIG_INFO()
AC_SUBST([PACKAGE_REQUIRES], [])
AC_CONFIG_FILES([touza.pc])


AC_OUTPUT

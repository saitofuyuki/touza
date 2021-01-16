#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#
# Maintainer: SAITO Fuyuki
# Created: Jun 5 2020
# Time-stamp: <2021/01/15 21:56:15 fuyuki configure.ac>

AC_PREREQ([2.69])

AT_INIT()
AC_INIT([touza], AT_VERSION, [saitofuyuki@jamstec.go.jp])

AC_COPYRIGHT([dnl
Copyright (C) 2020, 2021 Japan Agency for Marine-Earth Science and Technology

This is TOUZA, a collection of MIROC base infrastructure.

Licensed under the Apache License, Version 2.0 (http://www.apache.org/licenses/LICENSE-2.0)
])

AC_CONFIG_SRCDIR([touza.F90])
AT_PROG_FC_MPI
dnl AC_PROG_FC
AC_PROG_F77

AC_FC_PP_SRCEXT([F90])
AC_FC_PP_DEFINE
AC_FC_FREEFORM

AC_FC_MODULE_EXTENSION
AC_FC_MODULE_FLAG
AC_FC_MODULE_OUTPUT_FLAG([], [AC_MSG_WARN])

AC_ARG_ENABLE([warn-max],
[AS_HELP_STRING([--enable-warn-max], [compile with maximum warning option if found @<:@default=no@:>@])],
 [use_warn_max=$enableval],
 [use_warn_max=no])
AS_IF([test x"$use_warn_max" = xyes], [AX_FCFLAGS_WARN_ALL])

dnl common procedures
AT_PACKAGE_INIT()
AT_AM_INCLUDE()

AC_ARG_PROGRAM
AC_CONFIG_MACRO_DIRS([m4c])
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([-Wall])
AM_MAINTAINER_MODE
AC_PROG_MAKE_SET

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

AC_CONFIG_HEADER(m4_expand([TOUZA_config]))
AC_CONFIG_FILES([Makefile])
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

AT_SUB_PACKAGE([std], [always])
AT_SUB_PACKAGE([cal], [],     [std])
dnl AT_SUB_PACKAGE([emu], [],     [std])
dnl AT_SUB_PACKAGE([div], [],     [std])

AT_PARSE_PACKAGES()

AC_OUTPUT

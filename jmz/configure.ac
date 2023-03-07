#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#
# Maintainer: SAITO Fuyuki
# Created: Nov 24 2021
# Time-stamp: <2023/01/10 13:42:13 fuyuki configure.ac>

AC_PREREQ([2.71])

AC_INIT([touza-jmz],[1.00],[saitofuyuki@jamstec.go.jp])

AC_COPYRIGHT([dnl
Copyright (C) 2022 Japan Agency for Marine-Earth Science and Technology

This is TOUZA/Jmz (Ji-Mo-Tsu), a collection of sample programs using TOUZA.

Licensed under the Apache License, Version 2.0 (http://www.apache.org/licenses/LICENSE-2.0)
])

AC_CONFIG_SRCDIR([jmz.h])
AC_LANG_PUSH([Fortran])
AX_PROG_FC_MPI
AC_PROG_FC
AC_PROG_F77

AC_FC_PP_SRCEXT([F90])
AC_FC_PP_DEFINE
AC_FC_FREEFORM

AC_FC_MODULE_EXTENSION
AC_FC_MODULE_FLAG
AC_FC_MODULE_OUTPUT_FLAG([], [AC_MSG_WARN])
AX_COMPILER_VENDOR

MT_FORTRAN_BATCH_CHECK_STATEMENT([error], [stop],[
error stop 1])
MT_FORTRAN_BATCH_CHECK_SUBROUTINE([exit], [1], [])

AC_LANG_POP([Fortran])

AC_ARG_ENABLE([warn-max],
[AS_HELP_STRING([--enable-warn-max], [compile with maximum warning option if found @<:@default=no@:>@])],
 [use_warn_max=$enableval],
 [use_warn_max=no])
AS_IF([test x"$use_warn_max" = xyes], [AX_FCFLAGS_WARN_ALL([AM_FCFLAGS])])
AC_SUBST([AM_FCFLAGS])

dnl common procedures
AC_ARG_PROGRAM

AM_INIT_AUTOMAKE([-Wall foreign])
AM_MAINTAINER_MODE
AM_CONDITIONAL([NEVER], [false])

AC_PROG_MAKE_SET

AM_PROG_AR
LT_PREREQ([2.4.2])
LT_INIT([disable-shared])

AC_CONFIG_HEADERS([jmz_config.h])
AC_CONFIG_FILES([Makefile])

PKG_PROG_PKG_CONFIG()
PKG_PREREQ([0.29])
PKG_CHECK_MODULES_STATIC([TOUZA], [touza])

AC_ARG_VAR([TOUZA_NAME], [touza package name])
AC_ARG_VAR([TOUZA_VERSION], [touza package version])
PKG_CHECK_VAR([TOUZA_NAME], [touza], [touza_name])
_PKG_CONFIG([TOUZA_VERSION], [modversion], [touza])

AC_SUBST([TOUZA_LDFLAGS])
AC_SUBST([TOUZA_top_builddir])

dnl AX_CREATE_PKGCONFIG_INFO()
AC_SUBST([PACKAGE_REQUIRES], [])
AC_DEFINE_UNQUOTED([TOUZA_NAME], ['${TOUZA_NAME}'], [touza package name])
AC_DEFINE_UNQUOTED([TOUZA_VERSION], ['${TOUZA_VERSION}'], [touza package version])

AC_ARG_ENABLE([nctcdf],
[AS_HELP_STRING([--enable-nctcdf], [enable nio/n*c*tcdf features@<:@default=no@:>@])],
 [use_nctcdf=$enableval],
 [use_nctcdf=no])
AS_IF(
[test x"[$]use_nctcdf" = xforce],
[use_nctcdf=yes
AC_MSG_WARN([nctcdf force enabled (expected if built-in)])],
[test x"[$]use_nctcdf" != xno],
[AC_LANG_PUSH([Fortran])
AX_VAR_PUSHVALUE([LIBS], [[$]LIBS [$]TOUZA_LIBS])
AX_VAR_PUSHVALUE([LDFLAGS], [[$]LDFLAGS [$]TOUZA_LDFLAGS])
AX_VAR_PUSHVALUE([FCFLAGS], [[$]FCFLAGS [$]TOUZA_FCFLAGS [$]TOUZA_CFLAGS])
AC_LINK_IFELSE([
program actest
  use TOUZA_Nio,only: nct_open_write
end program actest
], [use_nctcdf=yes], [use_nctcdf=no])
AX_VAR_POPVALUE([LIBS])
AX_VAR_POPVALUE([LDFLAGS])
AX_VAR_POPVALUE([FCFLAGS])
AC_LANG_POP([Fortran])
AS_IF([test x"[$]use_nctcdf" = xno],
      [AC_MSG_ERROR([nctcdf disabled.])])
use_nctcdf=yes])

AM_CONDITIONAL([with_nctcdf], [test x"[$]use_nctcdf" = xyes])
AS_IF([test x"[$]use_nctcdf" = xyes],
      [AC_DEFINE([OPT_WITH_NCTCDF], [1], [build with TOUZA/Nio/nctcdf])])


AM_PATH_PYTHON([3.9], , [:])
AM_CONDITIONAL([HAVE_PYTHON], [test "$PYTHON" != :])
AX_PYTHON()
AX_PYTHON_MODULE([pyparsing], , [python3])

AC_PATH_PROGS([ZSH], [zsh], [:])

AC_OUTPUT

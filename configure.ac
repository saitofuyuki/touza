#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#
# Maintainer: SAITO Fuyuki
# Created: Jun 5 2020
# Time-stamp: <2021/01/07 15:25:42 fuyuki configure.ac>

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

dnl common procedures
AT_PACKAGE_INIT()
AT_AM_INCLUDE()

AC_ARG_PROGRAM
AC_CONFIG_MACRO_DIRS([m4c])
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([-Wall])
AM_MAINTAINER_MODE
AC_PROG_MAKE_SET

AM_PROG_AR
LT_PREREQ([2.4.2])
LT_INIT([disable-shared])

m4_define([TOUZA_config],
          [m4_join([_], AC_PACKAGE_NAME, [config.h])])

AC_CONFIG_HEADER(m4_expand([TOUZA_config]))
AC_CONFIG_FILES([Makefile])

AT_SUB_PACKAGE([std])
AT_SUB_PACKAGE([cal], [std])
dnl AT_SUB_PACKAGE([emu], [std])
dnl AT_SUB_PACKAGE([div], [std])

AT_PARSE_PACKAGES()

AC_DEFINE([TOUZA_PREFIX], [AS_TR_CPP(AC_PACKAGE_NAME)_], [module prefix])

AC_OUTPUT

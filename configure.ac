#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#
# Maintainer: SAITO Fuyuki
# Created: Jun 5 2020
# Time-stamp: <2023/03/24 17:22:02 fuyuki configure.ac>

AC_PREREQ([2.71])

MT_INIT()
AC_INIT([touza],MT_VERSION,[saitofuyuki@jamstec.go.jp])

AC_COPYRIGHT([dnl
Copyright (C) 2020-2023 Japan Agency for Marine-Earth Science and Technology

This is TOUZA, a collection of MIROC base infrastructure.

Licensed under the Apache License, Version 2.0 (http://www.apache.org/licenses/LICENSE-2.0)
])

AC_CONFIG_SRCDIR([touza.F90])

AC_CONFIG_MACRO_DIRS([m4c])
AC_CONFIG_AUX_DIR([build-aux])

AC_CANONICAL_HOST
AC_CANONICAL_BUILD
AC_CANONICAL_TARGET

AC_LANG_PUSH([Fortran])
MT_PROG_FC_MPI
AX_PROG_F77_MPI([test x"$with_mpi" != xno], [use_f77_mpi=yes], [use_f77_mpi=no])

AM_CONDITIONAL([BUILD_WITH_MPI], [test x"[$]with_mpi" != xno])

dnl AC_PROG_FC

AC_FC_PP_SRCEXT([F90])
AC_FC_PP_DEFINE
AC_FC_FREEFORM

AC_FC_MODULE_EXTENSION
AC_FC_MODULE_FLAG
AC_FC_MODULE_OUTPUT_FLAG([], [AC_MSG_WARN])
AX_COMPILER_VENDOR
AC_LANG_POP([Fortran])

AC_PROG_F77

AC_ARG_ENABLE([warn-max],
[AS_HELP_STRING([--enable-warn-max], [compile with maximum warning option if found @<:@default=no@:>@])],
 [use_warn_max=$enableval],
 [use_warn_max=no])
AS_IF([test x"$use_warn_max" = xyes], [AX_FCFLAGS_WARN_ALL([AM_FCFLAGS])])
AC_SUBST([AM_FCFLAGS])

dnl common procedures
MT_PACKAGE_INIT()
MT_AM_INCLUDE([lib@TOUZA_NAME@_la_SOURCES])
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

AC_ARG_PROGRAM

AM_INIT_AUTOMAKE([foreign -Wall])
AM_MAINTAINER_MODE
AM_CONDITIONAL([NEVER], [false])

AC_PROG_MAKE_SET

MT_FORTRAN_DEFINE([PACKAGE_TAG], m4_quote(m4_toupper(AC_PACKAGE_NAME)), [package name])

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

AS_IF([test x"$use_warn_max" = xyes], [AX_FCFLAGS_WARN_ALL([AM_FCFLAGS])])

AS_IF([test x"$with_mpi" != xno], [enable_sub_div_default=yes], [enable_sub_div_default=no])

MT_SUB_PACKAGE([std], [always])
MT_SUB_PACKAGE([emu], [],     [std])
MT_SUB_PACKAGE([cal], [],     [std])
MT_SUB_PACKAGE([trp], [],     [std])
MT_SUB_PACKAGE([ppp], [],     [std emu])
dnl MT_SUB_PACKAGE([div], [[$]enable_sub_div_default],     [std])
AM_CONDITIONAL([BUILD_div], [false])
dnl MT_SUB_PACKAGE([nio], [],     [std emu trp div])
MT_SUB_PACKAGE([nio], [],     [std emu trp])
MT_SUB_PACKAGE([ami], [],     [std nio])

AC_ARG_ENABLE([nctcdf],
[AS_HELP_STRING([--enable-nctcdf], [enable nio/n*c*tcdf features@<:@default=no@:>@])],
 [use_nctcdf=$enableval],
 [use_nctcdf=no])

AS_IF([test x"[$]use_nctcdf" != xno],
[AX_LIB_NETCDF4([serial])
AS_IF([test x"[$]with_netcdf4_fortran" != xyes],
[AS_IF([test x"[$]use_nctcdf" = xforce],
       [AC_MSG_WARN([netcdf4 fortran library disabled (ignored).])],
       [AC_MSG_ERROR([netcdf4 fortran library disabled.])])])
use_nctcdf=yes])

AM_CONDITIONAL([with_nctcdf], [test x"[$]use_nctcdf" = xyes])
AS_IF([test x"[$]use_nctcdf" = xyes],
      [AC_DEFINE([OPT_WITH_NCTCDF], [1], [build with TOUZA/Nio/nctcdf])])

AC_MSG_CHECKING([if also build jmz])
AC_ARG_WITH([build-jmz],
   [AS_HELP_STRING([--with-build-jmz=(yes|no)],
                   [whether to build jmz programs.  @<:@default=no@:>@])],
   [], [with_build_jmz=no])
AS_IF([test "x$with_build_jmz" != xno],
      [AC_MSG_RESULT([yes])
       enable_sub_nio=yes],
      [AC_MSG_RESULT([no])])

AS_IF([test "x$with_build_jmz" != xno],
[
AS_IF([test x"[$]use_nctcdf" = xno],
      [jmz_nctcdf=no],
      [jmz_nctcdf=force])
AX_SUBDIRS_CONFIGURE([jmz],
    [],
    [[TOUZA_top_builddir=${ac_top_build_prefix}],
     [TOUZA_VERSION="${PACKAGE_VERSION}"],
     [TOUZA_NAME="${PACKAGE_NAME}"],
     [TOUZA_CFLAGS=-I${ac_top_build_prefix}/ -I${ac_top_srcdir} ${FC_MODINC}${ac_top_build_prefix}nio ${FC_MODINC}${ac_top_build_prefix}trp ${FC_MODINC}${ac_top_build_prefix}std ${NETCDF4_FFLAGS}],
     [TOUZA_LIBRARY=${ac_top_build_prefix}lib${with_touza_name}.la],
     [TOUZA_LIBS=${NETCDF4_FLIBS} ${NETCDF4_LIBS}],
     [TOUZA_LDFLAGS=${NETCDF4_LDFLAGS}]],
    [[--enable-nctcdf="[$]jmz_nctcdf"]],
    [--with-build-jmz])])

AC_ARG_ENABLE([install-switches],
              [AS_HELP_STRING([--enable-install-switches],
                              [install switcher sources if applicable @<:@default='${datadir}/PACKAGE'@:>@])],
              [switchdir="$enableval"],
              [switchdir='yes'])
AS_IF([test x"[$]switchdir" = xyes],
      [switchdir='[$]{datadir}/[$]{PACKAGE}'])
AC_SUBST([switchdir])

AX_ADD_AM_MACRO_STATIC([
install-data-hook: install-switchDATA
switchdir     = @switchdir@
switch_DATA   =
])

AC_ARG_ENABLE([follow-revision],
[AS_HELP_STRING([--enable-follow-revision], [follow git revision updates @<:@default=no@:>@])],
 [use_follow_revision=$enableval],
 [use_follow_revision=def])
AS_IF([test x"$use_follow_revision" = xdef],
      [use_follow_revision=$enable_maintainer_mode])
AM_CONDITIONAL([autorevision],
[test x"$use_follow_revision" != xno])

AC_PATH_PROGS([AUTOREVISION], [autorevision], [:])
AC_PATH_PROGS([REALPATH], [realpath], [false])
AC_PATH_PROGS([ZSH], [zsh], [:])

dnl dependency hack
dnl _AM_DEPENDENCIES(FC)
AC_ARG_ENABLE([diag-dependencies],
[AS_HELP_STRING([--enable-diag-dependencies], [diagnose dependencies using gcc features@<:@default=no@:>@])],
 [use_gcc_dependencies=$enableval],
 [use_gcc_dependencies=no])
AM_CONDITIONAL([gcc_dependencies], [
  test "x$enable_dependency_tracking" != xno \
  && test "$use_gcc_dependencies" = yes])

AC_ARG_ENABLE([mod-dependencies],
[AS_HELP_STRING([--enable-mod-dependencies], [enable generated dependencies @<:@default=yes@:>@])],
 [use_mod_dependencies=$enableval],
 [use_mod_dependencies=yes])
AM_CONDITIONAL([enable_module_dependencies], [
  test "x$use_mod_dependencies" != xno \
  && test x"$ax_f90_modext" != xunknown])

dnl AM_CONDITIONAL([am__fastdepFC], [false])

dnl AX_CREATE_PKGCONFIG_INFO()
MT_PARSE_PACKAGES()

PKG_PROG_PKG_CONFIG()
PKG_PREREQ([0.29])
AC_SUBST([PACKAGE_REQUIRES], [])
AC_SUBST([pkgconfig_CPPFLAGS])
AC_CONFIG_FILES([touza.pc])

AC_OUTPUT

dnl Filename:   touza/m4c/mt_package.m4
dnl Maintainer: SAITO Fuyuki
dnl Created:    Jun 7 2020
dnl Time-stamp: <2025/05/21 14:33:41 fuyuki mt_package.m4>

dnl Copyright: 2020-2025 JAMSTEC
dnl Licensed under the Apache License, Version 2.0
dnl   (https://www.apache.org/licenses/LICENSE-2.0)

# MT_PACKAGE_INIT()
# ------------
# subpackages management initialization
AC_DEFUN([MT_PACKAGE_INIT],
[AC_REQUIRE([MT_ONCE])])# MT_PACKAGE_INIT

# MT_ONCE
# -------
# common definition called once
AC_DEFUN([MT_ONCE],
[MT_ENV_PP()
MT_ENV_MODULES()
])# MT_ONCE

# MT_ENV_PP
# ---------
# preprocessor environment
AC_DEFUN([MT_ENV_PP],
[AC_LANG_PUSH([Fortran])
MT_FORTRAN_PP_CONCAT([HAVE_PP_CONCAT])
MT_FORTRAN_PP_COMMENT([HAVE_PP_COMMENT_KEPT])
AC_LANG_POP([Fortran])
])# MT_ENV_PP

# MT_ENV_MODULES
# --------------
# autoconf batch for fortran module treatment.
AC_DEFUN([MT_ENV_MODULES],
[AC_REQUIRE([AC_FC_MODULE_EXTENSION])
AC_REQUIRE([AC_FC_MODULE_FLAG])
AC_REQUIRE([AC_FC_MODULE_OUTPUT_FLAG])

AM_CONDITIONAL([CLEAN_FCMOD], [test x"$FC_MODEXT" != "x"])

AC_ARG_ENABLE([install-modules],
              [AS_HELP_STRING([--enable-install-modules],
                              [install module files if applicable @<:@default='${exec_prefix}/include/PACKAGE'@:>@])],
              [moddir="$enableval"],
              [moddir='yes'])

AM_CONDITIONAL([INSTALL_MODULES],
               [test x"[$]moddir" != "xno" -a x"$FC_MODEXT" != "x"])
AS_IF([test x"[$]moddir" = xyes],
      [moddir='[$]{exec_prefix}/include/[$]{PACKAGE}'])

AC_SUBST([moddir])
])# MT_ENV_MODULES

# MT_ALL_SUB_PACKAGES
# -------------------
# placeholder
m4_define([MT_ALL_SUB_PACKAGES], [])

# MT_SUB_PACKAGE(NAME, [ENABLE], [DEP], [DIRECTORY])
# --------------------------------------------------
# Declare NAME subpackage under DIRECTORY (or NAME if not set).
# ENABLE: always yes no
AC_DEFUN([MT_SUB_PACKAGE],
[_$0([$1],
     [m4_default([$2], [yes])],
     [$3],
     m4_quote(m4_default([$4], [$1])))])# MT_SUB_PACKAGE

# _MT_SUB_PACKAGE(NAME, ENABLE, DEP, DIRECTORY)
# ---------------------------------------------
AC_DEFUN([_MT_SUB_PACKAGE],
[m4_do([m4_append_uniq([MT_ALL_SUB_PACKAGES], [$1], [ ])],
       [MT_PACKAGE_DEPS([$1], m4_quote($3))],
       [m4_define([MT_DEFAULT_SW($1)], [$2])],
       [m4_define([MT_DIRECTORY($1)], [$4])],
       [AC_REQUIRE([MT_SUB_DEFAULT])],
       [AC_ARG_ENABLE([sub-$1],
                      [AS_HELP_STRING([--enable-sub-$1=(yes|no)],
                                      [whether to build $1 subpackage.  @<:@default=yes@:>@])],
                      [], [MT_VAR_ENABLE([$1])=$2])],
       )])# _MT_SUB_PACKAGE

# MT_VAR_ENABLE(SUB)
# -------------------
# shell variable to hold build condition of SUB subpackage.
AC_DEFUN([MT_VAR_ENABLE], [AS_TR_SH([enable-sub-$1])])

# MT_SUB_DEFAULT
# --------------
# set-up default build policy on subpackages.
AC_DEFUN([MT_SUB_DEFAULT],
[AC_ARG_ENABLE([sub-all],
               [AS_HELP_STRING([--disable-sub-all],
                               [disable to build all subpackages])],
               [],
               [enable_sub_all=])])# MT_DEFAULT_SUB

# MT_REQUIRE(SUB)
# ----------------
# Return dependencies of package SUB
AC_DEFUN([MT_REQUIRE], [m4_indir([$0($1)])])# MT_REQUIRE

# MT_DIRECTORY(SUB)
# ----------------
# Return directory of SUB
AC_DEFUN([MT_DIRECTORY], [m4_indir([$0($1)])])# MT_DIRECTORY

# MT_DEFAULT_SW(SUB)
# ------------------
# Return default enable-switch of SUB
AC_DEFUN([MT_DEFAULT_SW], [m4_indir([$0($1)])])# MT_DEFAULT_SW

# MT_PACKAGE_DEPS(PACKAGE, LIST)
# ------------------------------
AC_DEFUN([MT_PACKAGE_DEPS],
[m4_define([MT_REQUIRE($1)], [$2])
m4_map_args_w([$2], [_$0([$1],], [)])
])# MT_PACKAGE_DEPS

# _MT_PACKAGE_DEPS(PACKAGE, DEP)
# ------------------------------
AC_DEFUN([_MT_PACKAGE_DEPS],
[m4_ifndef([MT_REQUIRE($2)],
           [m4_fatal([subpackage dependencies failure for $1::$2.  check the order])])
m4_append_uniq_w([MT_REQUIRE($1)], m4_quote(MT_REQUIRE($2)))
])# _MT_PACKAGE_DEPS

# MT_PARSE_PACKAGES()
# -----------------------
# Finalize all the subpackage procedures
AC_DEFUN([MT_PARSE_PACKAGES],
[m4_do([m4_if([MT_PACKAGE_DEPS_DBG()])],
       [MT_MAP_ALL_SUBS([MT_PACKAGE_CHECKS])],
       [MT_MAP_ALL_SUBS([MT_PACKAGE_LOAD])],
       [MT_MAP_ALL_SUBS([MT_PACKAGE_CONFIG])],
)])# MT_PARSE_PACKAGES

# MT_MAP_ALL_SUBS(MACRO)
# ----------------------
# expand MACRO(SUB) for all packages in MT_ALL_SUB_PACKAGES
AC_DEFUN([MT_MAP_ALL_SUBS],
[m4_map_args_w(m4_quote(MT_ALL_SUB_PACKAGES),
               [$1(],
               [)])])# MT_MAP_ALL_SUBS

# MT_PACKAGE_CHECKS(SUB)
# ----------------------
# wrap _MT_PACKAGE_CHECKS to check build condition of SUB subpackge.
dnl AC_DEFUN([MT_PACKAGE_CHECKS],
dnl [_$0([$1], [AS_TR_SH([enable_sub-$1])])])# _MT_PACKAGE_CHECKS
AC_DEFUN([MT_PACKAGE_CHECKS],
[_$0([$1],
     [MT_VAR_ENABLE([$1])],
     [MT_VAR_ENABLE([all])],
     [MT_DEFAULT_SW([$1])])])# _MT_PACKAGE_CHECKS

# _MT_PACKAGE_CHECKS(SUB, VARIABLE, VAR-DEFAULT, SW-DEFAULT)
# ---------------------------------
AC_DEFUN([_MT_PACKAGE_CHECKS],
[dnl
m4_if([$4], [always],
      [eval $2="$4"],
      [AS_IF([test x"@S|@$2" = x],
             [eval $2="@S|@$3"])
       AS_IF([test x"@S|@$2" = x],
             [eval $2="$4"])])
AS_IF([test x"@S|@$2" != xno],
[m4_map_args_w(m4_quote(MT_REQUIRE($1)),
               [MT_ENABLE_SUB(], [)])])
])# _MT_PACKAGE_CHECKS

# MT_ENABLE_SUB(SUB)
# ------------------
AC_DEFUN([MT_ENABLE_SUB],
[_$0([$1], [MT_VAR_ENABLE([$1])])])# MT_ENABLE_SUB

# _MT_ENABLE_SUB(SUB, VARIABLE)
# -----------------------------
AC_DEFUN([_MT_ENABLE_SUB],
[AS_CASE(["@S|@$2"],
         [no],  [eval $2=dep],
         [""],  [eval $2=yes])
])

# MT_PACKAGE_LOAD(SUB)
# --------------------
# load subpackage m4 macros (conditionally)
AC_DEFUN([MT_PACKAGE_LOAD],
[_$0([$1],
     [MT_VAR_ENABLE([$1])],
     m4_quote(MT_DIRECTORY($1)))])# MT_PACKAGE_LOAD

# _MT_PACKAGE_LOAD(SUB, VARIABLE, DIRECTORY)
# -------------------------------
AC_DEFUN([_MT_PACKAGE_LOAD],
[AS_CASE(["@S|@$2"],
         [always], [AC_MSG_NOTICE([load subpackage $1 (mandatory)])],
         [yes],    [AC_MSG_NOTICE([load subpackage $1])],
         [dep],    [AC_MSG_NOTICE([load subpackage $1 (dependency)])],
         [no],     [AC_MSG_NOTICE([skip subpackage $1])],
         [build],  [AC_MSG_NOTICE([(if possible) build-only subpackage $1])],
         [AC_MSG_FAILURE([invalid switch for subpackage $1 @S|@$2."])])
AS_IF([test x"@S|@$2" != xno],
      [MT_LOAD([$3], [$1])])
])# _MT_PACKAGE_LOAD

# MT_LOAD(SUB, DIRECTORY)
# -----------------------
AC_DEFUN([MT_LOAD], [_$0([$2]/MT_LOCAL_FILE)])# MT_LOAD

# _MT_LOAD(SUB, DIRECTORY)
# ------------------------
AC_DEFUN([_MT_LOAD], [mt_sinclude([$1])])# _MT_LOAD

# MT_PACKAGE_CONFIG(SUB, [PACKAGE])
# --------------------
# autoconf/automake macros for subpackage
AC_DEFUN([MT_PACKAGE_CONFIG],
[_$0([$1],
     [MT_VAR_ENABLE([$1])],
     m4_quote(MT_DIRECTORY($1)),
     m4_quote(m4_default([$2], [AC_PACKAGE_NAME])))])# MT_PACKAGE_CONFIG

# _MT_PACKAGE_CONFIG(SUB, VARIABLE, DIRECTORY, PACKAGE)
# --------------------------------------------
AC_DEFUN([_MT_PACKAGE_CONFIG],
[AS_IF([test "x$enable_sub_$1" != xno],
[AC_CONFIG_FILES([$3/Makefile])])
AM_CONDITIONAL([BUILD_$1], [test x"@S|@$2" != xno])
AS_IF([test x"@S|@$2" != xno],
      [AC_DEFINE(AS_TR_CPP(ENABLE_$4_$1), [1], [$1 is enabled])])
srcdir_$4_$1='[$](top_srcdir)/$3'
builddir_$4_$1='[$](top_builddir)/$3'
FC_INCLUDE_$4_$1='-I[$](srcdir_$4_$1)'
FC_MODULE_$4_$1='[$](FC_MODINC)[$](builddir_$4_$1)'
FC_LDADD_$4_$1='[$](builddir_$4_$1)/lib$4_local.la'
AC_SUBST([srcdir_$4_$1])
AC_SUBST([builddir_$4_$1])
AC_SUBST([FC_INCLUDE_$4_$1])
AC_SUBST([FC_MODULE_$4_$1])
AC_SUBST([FC_LDADD_$4_$1])
])# _MT_PACKAGE_CONFIG


# MT_PACKAGE_DEPS_DBG
# -------------------
AC_DEFUN([MT_PACKAGE_DEPS_DBG],
[@%:@ :MT@&t@_ {MT_ALL_SUB_PACKAGES}
m4_foreach_w([_mt_pkg],
             m4_quote(MT_ALL_SUB_PACKAGES),
[@%:@ :MT@&t@_ _m4_pkg:: {MT_REQUIRE(_mt_pkg)}
])])

dnl AS_VAR_SET([mt_cv_enable_$3_$1], [0])
dnl AS_VAR_SET([mt_cv_enable_$3_$1], [1])

dnl Local Variables:
dnl mode: autoconf
dnl end:

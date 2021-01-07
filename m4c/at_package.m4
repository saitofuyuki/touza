dnl Filename:   at_package.m4
dnl Maintainer: SAITO Fuyuki
dnl Created:    Jun 7 2020
dnl Time-stamp: <2021/01/06 10:46:55 fuyuki at_package.m4>

dnl Copyright: 2020, 2021 JAMSTEC
dnl Licensed under the Apache License, Version 2.0
dnl   (https://www.apache.org/licenses/LICENSE-2.0)

# AT_PACKAGE_INIT()
# ------------
# subpackages management initialization
AC_DEFUN([AT_PACKAGE_INIT],
[AC_REQUIRE([AT_ONCE])])# AT_PACKAGE_INIT

# AT_ONCE
# -------
# common definition called once
AC_DEFUN([AT_ONCE],
[AT_ENV_PP()
AT_ENV_MODULES()
])# AT_ONCE

# AT_ENV_PP
# ---------
# preprocessor environment
AC_DEFUN([AT_ENV_PP],
[AC_LANG_PUSH([Fortran])
AT_FORTRAN_PP_CONCAT([HAVE_PP_CONCAT])
AC_LANG_POP([Fortran])
])# AT_ENV_PP

# AT_ENV_MODULES
# --------------
# autoconf batch for fortran module treatment.
AC_DEFUN([AT_ENV_MODULES],
[AC_REQUIRE([AC_FC_MODULE_EXTENSION])
AC_REQUIRE([AC_FC_MODULE_FLAG])
AC_REQUIRE([AC_FC_MODULE_OUTPUT_FLAG])

AM_CONDITIONAL([CLEAN_FCMOD], [test x"$FC_MODEXT" != "x"])

AC_ARG_ENABLE([install-modules],
              [AS_HELP_STRING([--enable-install-modules],
                              [install module files if applicable. Default: '$(pkgincludedir)'])],
              [pkgmoddir="$enableval"],
              [pkgmoddir='$(pkgincludedir)'])

AM_CONDITIONAL([INSTALL_MODULES],
               [test x"[$]pkgmoddir" != "xno" -a x"$FC_MODEXT" != "x"])
AS_IF([test x"[$]pkgmoddir" = xyes],
      [pkgmoddir='[$](pkgincludedir)'])

AC_SUBST([pkgmoddir])
])# AT_ENV_MODULES

# AT_ALL_SUB_PACKAGES
# -------------------
# placeholder
m4_define([AT_ALL_SUB_PACKAGES], [])

# AT_SUB_PACKAGE(NAME, [DEP], [DIRECTORY])
# ----------------------------------------
# Declare NAME subpackage under DIRECTORY (or NAME if not set).
AC_DEFUN([AT_SUB_PACKAGE],
[_$0([$1], [$2], m4_quote(m4_default([$3], [$1])))])# AT_SUB_PACKAGE

# _AT_SUB_PACKAGE(NAME, DEP, DIRECTORY)
# -------------------------------------
AC_DEFUN([_AT_SUB_PACKAGE],
[m4_do([m4_append_uniq([AT_ALL_SUB_PACKAGES], [$1], [ ])],
       [AT_PACKAGE_DEPS([$1], m4_quote($2))],
       [m4_define([AT_DIRECTORY($1)], [$3])],
       [AC_REQUIRE([AT_SUB_DEFAULT])],
       [AC_ARG_ENABLE([sub-$1],
                      [AS_HELP_STRING([--enable-sub-$1=(yes|no)],
                                      [whether to build $1 subpackage.  @<:@default=yes@:>@])],
                      [], [])],
       )])# _AT_SUB_PACKAGE

# AT_VAR_ENABLE(SUB)
# -------------------
# shell variable to hold build condition of SUB subpackage.
AC_DEFUN([AT_VAR_ENABLE], [AS_TR_SH([enable-sub-$1])])

# AT_SUB_DEFAULT
# --------------
# set-up default build policy on subpackages.
AC_DEFUN([AT_SUB_DEFAULT],
[AC_ARG_ENABLE([sub-all],
               [AS_HELP_STRING([--disable-sub-all],
                               [disable to build all subpackages])],
               [],
               [enable_sub_all=yes])])# AT_DEFAULT_SUB

# AT_REQUIRE(SUB)
# ----------------
# Return dependencies of package SUB
AC_DEFUN([AT_REQUIRE], [m4_indir([$0($1)])])# AT_REQUIRE

# AT_DIRECTORY(SUB)
# ----------------
# Return directory of SUB
AC_DEFUN([AT_DIRECTORY], [m4_indir([$0($1)])])# AT_DIRECTORY

# AT_PACKAGE_DEPS(PACKAGE, LIST)
# ------------------------------
AC_DEFUN([AT_PACKAGE_DEPS],
[m4_define([AT_REQUIRE($1)], [$2])
m4_map_args_w([$2], [_$0([$1],], [)])
])# AT_PACKAGE_DEPS

# _AT_PACKAGE_DEPS(PACKAGE, DEP)
# ------------------------------
AC_DEFUN([_AT_PACKAGE_DEPS],
[m4_ifndef([AT_REQUIRE($2)],
           [m4_fatal([subpackage dependencies failure for $1::$2.  check the order])])
m4_append_uniq_w([AT_REQUIRE($1)], m4_quote(AT_REQUIRE($2)))
])# _AT_PACKAGE_DEPS

# AT_PARSE_PACKAGES()
# -----------------------
# Finalize all the subpackage procedures
AC_DEFUN([AT_PARSE_PACKAGES],
[m4_do([m4_if([AT_PACKAGE_DEPS_DBG()])],
       [AT_MAP_ALL_SUBS([AT_PACKAGE_CHECKS])],
       [AT_MAP_ALL_SUBS([AT_PACKAGE_LOAD])],
       [AT_MAP_ALL_SUBS([AT_PACKAGE_CONFIG])],
)])# AT_PARSE_PACKAGES

# AT_MAP_ALL_SUBS(MACRO)
# ----------------------
# expand MACRO(SUB) for all packages in AT_ALL_SUB_PACKAGES
AC_DEFUN([AT_MAP_ALL_SUBS],
[m4_map_args_w(m4_quote(AT_ALL_SUB_PACKAGES),
               [$1(],
               [)])])# AT_MAP_ALL_SUBS

# AT_PACKAGE_CHECKS(SUB)
# ----------------------
# wrap _AT_PACKAGE_CHECKS to check build condition of SUB subpackge.
dnl AC_DEFUN([AT_PACKAGE_CHECKS],
dnl [_$0([$1], [AS_TR_SH([enable_sub-$1])])])# _AT_PACKAGE_CHECKS
AC_DEFUN([AT_PACKAGE_CHECKS],
[_$0([$1],
     [AT_VAR_ENABLE([$1])],
     [AT_VAR_ENABLE([all])])])# _AT_PACKAGE_CHECKS

# _AT_PACKAGE_CHECKS(SUB, VARIABLE, VAR-DEFAULT)
# ---------------------------------
AC_DEFUN([_AT_PACKAGE_CHECKS],
[AS_IF([test x"@S|@$2" = x],
       [eval $2="@S|@$3"])
AS_IF([test x"@S|@$2" = xyes],
[m4_map_args_w(m4_quote(AT_REQUIRE($1)),
               [AT_ENABLE_SUB(], [)])])
])# _AT_PACKAGE_CHECKS

# AT_ENABLE_SUB(SUB)
# ------------------
AC_DEFUN([AT_ENABLE_SUB],
[_$0([$1], [AT_VAR_ENABLE([$1])])])# AT_ENABLE_SUB

# _AT_ENABLE_SUB(SUB, VARIABLE)
# -----------------------------
AC_DEFUN([_AT_ENABLE_SUB],
[AS_CASE(["@S|@$2"],
         [no],  [eval $2=dep],
         [""],  [eval $2=yes])
])

# AT_PACKAGE_LOAD(SUB)
# --------------------
# load subpackage m4 macros (conditionally)
AC_DEFUN([AT_PACKAGE_LOAD],
[_$0([$1],
     [AT_VAR_ENABLE([$1])],
     m4_quote(AT_DIRECTORY($1)))])# AT_PACKAGE_LOAD

# _AT_PACKAGE_LOAD(SUB, VARIABLE, DIRECTORY)
# -------------------------------
AC_DEFUN([_AT_PACKAGE_LOAD],
[AS_CASE(["@S|@$2"],
         [yes], [AC_MSG_NOTICE([load subpackage $1])],
         [dep], [AC_MSG_NOTICE([load subpackage $1 (dependency)])],
         [no],  [AC_MSG_NOTICE([skip subpackage $1])],
         [AC_MSG_FAILURE([invalid switch for subpackage $1 @S|@$2."])])
AS_IF([test x"@S|@$2" != xno],
      [AT_LOAD([$3], [$1])])
])# _AT_PACKAGE_LOAD

# AT_LOAD(SUB, DIRECTORY)
# -----------------------
AC_DEFUN([AT_LOAD], [_$0([$2]/AT_LOCAL_FILE)])# AT_LOAD

# _AT_LOAD(SUB, DIRECTORY)
# ------------------------
AC_DEFUN([_AT_LOAD], [at_sinclude([$1])])# _AT_LOAD

# AT_PACKAGE_CONFIG(SUB, [PACKAGE])
# --------------------
# autoconf/automake macros for subpackage
AC_DEFUN([AT_PACKAGE_CONFIG],
[_$0([$1],
     [AT_VAR_ENABLE([$1])],
     m4_quote(AT_DIRECTORY($1)),
     m4_quote(m4_default([$2], [AC_PACKAGE_NAME])))])# AT_PACKAGE_CONFIG

# _AT_PACKAGE_CONFIG(SUB, VARIABLE, DIRECTORY, PACKAGE)
# --------------------------------------------
AC_DEFUN([_AT_PACKAGE_CONFIG],
[AC_CONFIG_FILES([$3/Makefile])
AM_CONDITIONAL([BUILD_$1], [test x"@S|@$2" != xno])
AS_IF([test x"@S|@$2" != xno],
      [AC_DEFINE(AS_TR_CPP(ENABLE_$4_$1), [1], [$1 is enabled])])
FC_MODULE_$4_$1='[$](FC_MODINC)[$](top_builddir)/$3'
FC_LDADD_$4_$1='[$](top_builddir)/$3/lib$4_local.la'
AC_SUBST([FC_MODULE_$4_$1])
AC_SUBST([FC_LDADD_$4_$1])
])# _AT_PACKAGE_CONFIG


# AT_PACKAGE_DEPS_DBG
# -------------------
AC_DEFUN([AT_PACKAGE_DEPS_DBG],
[@%:@ :AT@&t@_ {AT_ALL_SUB_PACKAGES}
m4_foreach_w([_m4_pkg],
             m4_quote(AT_ALL_SUB_PACKAGES),
[@%:@ :AT@&t@_ _m4_pkg:: {AT_REQUIRE(_m4_pkg)}
])])

dnl AS_VAR_SET([at_cv_enable_$3_$1], [0])
dnl AS_VAR_SET([at_cv_enable_$3_$1], [1])

dnl Local Variables:
dnl mode: autoconf
dnl end:

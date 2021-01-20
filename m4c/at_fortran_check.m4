dnl Filename:  at_fortran_check.m4
dnl Author:    SAITO Fuyuki
dnl Created:   Jun 3 2020
dnl Time-stamp: <2021/01/20 09:06:31 fuyuki at_fortran_check.m4>

dnl Copyright: 2020, 2021 JAMSTEC
dnl Licensed under the Apache License, Version 2.0
dnl   (https://www.apache.org/licenses/LICENSE-2.0)

dnl
dnl User level
dnl   AT_FORTRAN_BATCH_CHECK_FUNCTION
dnl   AT_FORTRAN_BATCH_CHECK_SUBROUTINE
dnl   AT_FORTRAN_BATCH_CHECK_MODULE
dnl   AT_FORTRAN_PP_CONCAT
dnl   AT_FORTRAN_MODULE_FLAG
dnl   AT_FORTRAN_BATCH_LDADD

# AT_FORTRAN_CHECK
# ----------------
# Dummy macro
AC_DEFUN([AT_FORTRAN_CHECK])

# ======================================================================
# AT_FORTRAN_BATCH_CHECK_FUNCTION(FUNCTION, [ARGUMENTS])
# ------------------------------------------------------
AC_DEFUN([AT_FORTRAN_BATCH_CHECK_FUNCTION],
[m4_indir([AT_FORTRAN_BATCH_CHECK],
          [whether function $1 works],
          [AT_FORTRAN_CACHE_ID($1)],
          [AT_FORTRAN_CPP_ID([HAVE_$1])],
          [AT_FORTRAN_CHECK_FUNCTION],
          $@)])# AT_FORTRAN_BATCH_CHECK_FUNCTION

# AT_FORTRAN_BATCH_CHECK_SUBROUTINE(SUBROUTINE, [ARGUMENTS])
# ------------------------------------------------------
AC_DEFUN([AT_FORTRAN_BATCH_CHECK_SUBROUTINE],
[m4_indir([AT_FORTRAN_BATCH_CHECK],
          [whether subroutine $1 works],
          [AT_FORTRAN_CACHE_ID($1)],
          [AT_FORTRAN_CPP_ID([HAVE_$1])],
          [AT_FORTRAN_CHECK_SUBROUTINE],
          $@)])# AT_FORTRAN_BATCH_CHECK_SUBROUTINE

# AT_FORTRAN_BATCH_CHECK_MODULE(MODULE, [MEMBER])
# ------------------------------------------------------
AC_DEFUN([AT_FORTRAN_BATCH_CHECK_MODULE],
[m4_ifblank([$2],
            [m4_indir([AT_FORTRAN_BATCH_CHECK],
                      [whether module $1 works],
                      [AT_FORTRAN_CACHE_ID($1)],
                      [AT_FORTRAN_CPP_ID([HAVE_$1])],
                      [AT_FORTRAN_CHECK_MODULE],
                      $@)],
            [m4_indir([AT_FORTRAN_BATCH_CHECK],
                      [whether module $1 works with $2],
                      [AT_FORTRAN_CACHE_ID($1_$2)],
                      [AT_FORTRAN_CPP_ID([HAVE_$1_$2])],
                      [AT_FORTRAN_CHECK_MODULE_MEMBER],
                      $@)])])# AT_FORTRAN_BATCH_CHECK_MODULE

# AT_FORTRAN_BATCH_CHECK_STATEMENT(STATEMENT, SPEC, CODE)
# ------------------------------------------------------
AC_DEFUN([AT_FORTRAN_BATCH_CHECK_STATEMENT],
[m4_indir([AT_FORTRAN_BATCH_CHECK],
          [whether statement $1 works with $2 specifier],
          [AT_FORTRAN_CACHE_ID($1_$2)],
          [AT_FORTRAN_CPP_ID([HAVE_$1_$2])],
          [AT_FORTRAN_CHECK_STATEMENT_SPEC],
          $@)])# AT_FORTRAN_BATCH_CHECK_STATEMENT


# AT_FORTRAN_BATCH_CHECK(MESSAGE, CACHE, MACRO, CHECKER, ARGUMENTS...)
# -------------------------------------------------
AC_DEFUN([AT_FORTRAN_BATCH_CHECK],
[AC_CACHE_CHECK([$1], [$2],
[m4_indir(m4_shift3($@),
          [AS_VAR_SET([at_fortran_check], [yes])],
          [AS_VAR_SET([at_fortran_check], [no])])
AS_VAR_SET([$2], [$[]at_fortran_check])])
##
AS_IF([test "x$[]$2" = xyes],
      [AC_DEFINE(m4_quote($3), [1], [$1])])
])# AT_FORTRAN_BATCH_CHECK

# ======================================================================

# AT_FORTRAN_CHECK_FUNCTION(FUNCTION, [ARGUMENTS], [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# -------------------------------------------------------------------------------------
# Check whether FUNCTION can be used.
AC_DEFUN([AT_FORTRAN_CHECK_FUNCTION],
[AT_FORTRAN_CHECK_DEFINE([$1],
      [write(*,*) $1($2)], [$3], [$4])])# AT_FORTRAN_CHECK_FUNCTION

# AT_FORTRAN_CHECK_SUBROUTINE(SUBROUTINE, [ARGUMENTS], [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# -----------------------------------------------------------------------------------------
# Check whether SUBROUTINE can be used.
AC_DEFUN([AT_FORTRAN_CHECK_SUBROUTINE],
[AT_FORTRAN_CHECK_DEFINE([$1],
      [call $1($2)], [$3], [$4])])# AT_FORTRAN_CHECK_SUBROUTINE

# AT_FORTRAN_CHECK_MODULE(MODULE, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# --------------------------------------------------------------------
# Check whether MODULE can be used.
AC_DEFUN([AT_FORTRAN_CHECK_MODULE],
[AT_FORTRAN_CHECK_DEFINE([$1],
      [use $1], [$2], [$3])])# AT_FORTRAN_CHECK_MODULE

# AT_FORTRAN_CHECK_MODULE_MEMBER(MODULE, MEMBER, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# -----------------------------------------------------------------------------------
# Check whether MODULE contains MEMBER.
AC_DEFUN([AT_FORTRAN_CHECK_MODULE_MEMBER],
[AT_FORTRAN_CHECK_DEFINE([$1_$2],
     [use $1,only: $2], [$3], [$4])])# AT_FORTRAN_CHECK_MODULE_MEMBER

# AT_FORTRAN_CHECK_STATEMENT_SPEC(STATEMENT, SPEC, CODE, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# -----------------------------------------------------------------------------------------------
# Check whether STATMENT contains SPEC specifier.
AC_DEFUN([AT_FORTRAN_CHECK_STATEMENT_SPEC],
[AT_FORTRAN_CHECK_DEFINE([$1_$2],
     [$3], [$4], [$5])])# AT_FORTRAN_CHECK_STATEMENT_SPEC

# AT_FORTRAN_CHECK_DEFINE(NAME, BODY, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
AC_DEFUN([AT_FORTRAN_CHECK_DEFINE],
[AC_LINK_IFELSE([AC_LANG_PROGRAM([], [$2])], [$3], [$4])])# AT_FORTRAN_CHECK_DEFINE

dnl AC_DEFUN([AT_FORTRAN_CHECK_DEFINE],
dnl [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [$2])], [$3], [$4])])# AT_FORTRAN_CHECK_DEFINE
# ==================================================

# AT_FORTRAN_PP_CONCAT(MACRO, [CACHE])
# ------------------------------------
# Check concatenation works with the preprocessor.
# Result set as MACRO, where 0 if disabled, 1 for '##',
# 2 for traditional mode ('/**/').
AC_DEFUN([AT_FORTRAN_PP_CONCAT],
[_$0([$1], [m4_default([$2], [at_cv_fortran_pp_concat])])])

AC_DEFUN([_AT_FORTRAN_PP_CONCAT],
[AC_CACHE_CHECK([preprocessor concatenation type],
[$2],
[dnl
# check concatenation
AS_VAR_SET([$2], 0)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
[@%:@define CONT(A) CONT @%:@@%:@ A
 CONT(INUE)])],
# then result
[AS_VAR_SET([$2], 1)],
# else second check
[AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
[@%:@define CONT(A) CONT/**/A
 CONT(INUE)])],
[AS_VAR_SET([$2], 2)])])])
# end check concatenation
AS_IF([test x"$[]$2" != x],
      [AC_DEFINE_UNQUOTED([$1], [$[]$2],
                          [preprocessor concatenation type])])
dnl AH_BOTTOM([@%:@if $1 == 1
dnl @%:@define _CONCAT(A,B) A@%:@@%:@B
dnl @%:@elif $1 == 2
dnl @%:@define _CONCAT(A,B) A/**/B
dnl @%:@endif])
])# AT_FORTRAN_PP_CONCAT

# ======================================================================

# AT_FORTRAN_CACHE_ID(NAME)
# -------------------------
AC_DEFUN([AT_FORTRAN_CACHE_ID], [AS_TR_SH([at_cv_fortran_$1])])

# AT_FORTRAN_CPP_ID(NAME)
# -----------------------
AC_DEFUN([AT_FORTRAN_CPP_ID], [AS_TR_CPP($@)])

# AT_FORTRAN_VAR_ID(NAME)
# -----------------------
AC_DEFUN([AT_FORTRAN_VAR_ID], [AS_TR_SH($@)])

# ======================================================================
# Header/Module/Library search (ref. AX_F90_LIBRARY_SETUP)

# AT_FORTRAN_MODULE_FLAG(MODULE, PATH, [NAME])
# --------------------------------------------
# Search MODULE in PATH and store result using NAME or MODULE
AC_DEFUN([AT_FORTRAN_MODULE_FLAG], [_$0($@, m4_default([$3], [$1]))])

# _AT_FORTRAN_MODULE_FLAG(MODULE, PATH, NAME)
AC_DEFUN([_AT_FORTRAN_MODULE_FLAG],
[AC_REQUIRE([AC_FC_MODULE_FLAG])dnl
 m4_indir([AT_FORTRAN_BATCH_SEARCH_MODULE],
          [flag to use module $1],
          [AT_FORTRAN_CACHE_ID([compile_$3])],
          [AT_FORTRAN_VAR_ID([FC_MODULE_$3])],
          ["[$]FC_MODINC"],
          [$2],
          [use $1])])# _AT_FORTRAN_MODULE_FLAG

# AT_FORTRAN_BATCH_SEARCH_MODULE(MESSAGE, CACHE, RESULT, FLAG, PATH, BODY)
# ------------------------------------------------------------------------
# AT_FORTRAN_SEARCH_COMPILE wrapper.
# Check CACHE and store to RESULT.
AC_DEFUN([AT_FORTRAN_BATCH_SEARCH_MODULE],
[AC_CACHE_CHECK([$1], [$2],
[m4_indir([AT_FORTRAN_SEARCH_COMPILE],m4_shift2($@))
 AS_IF([test "x[$]at_result" = x"not found"],
       [AS_VAR_SET([$2], [no])],
       [AS_VAR_SET([$2], [[$]at_result])])])
##
AS_IF([test "x[$]$2" = xno],
      [AS_VAR_SET([$3])],
      [AS_VAR_SET([$3], [[$]$2])])
AC_SUBST($3)])# AT_FORTRAN_BATCH_SEARCH_MODULE


# AT_FORTRAN_SEARCH_COMPILE(ID, FLAG, PATH, BODY, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# ------------------------------------------------------------------------------------
# Try compile with source BODY with searching module in PATH using
# compiler option FLAG.
AC_DEFUN([AT_FORTRAN_SEARCH_COMPILE],
[AS_IF([test x"$3" = x],
       [at_search="[$]prefix:[$]ac_default_prefix"],
       [at_search="$3"])
at_result="not found"
at_save_FCFLAGS="[$]FCFLAGS"
for at_dir in `AS_ECHO([[$]at_search]) | tr ':' '\012'`; do
 AS_IF([test "x[$]at_result" = x"not found"],
    [at_tmp="$2[$]at_dir"
     FCFLAGS="[$]at_save_FCFLAGS [$]at_tmp"
     AT_FORTRAN_CHECK_DEFINE([$1],[$4],[at_result="[$]at_tmp"])])
done
FCFLAGS="[$]at_save_FCFLAGS"
AS_IF([test "x[$]at_result" = x"not found"],
      [$6],
      [$5])
])# AT_FORTRAN_SEARCH_COMPILE

# AT_FORTRAN_BATCH_LDADD(BASE, PATH, [EXTS], [NAME],
#                        [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# --------------------------------------------------
AC_DEFUN([AT_FORTRAN_BATCH_LDADD],
[_$0([$1], [$2], [$3], m4_default([$4], [$1]), [$5], [$6])])

# _AT_FORTRAN_BATCH_LDADD(BASE, PATH, EXTS, NAME, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# -------------------------------------------------
# Search BASE library in PATH and store results in
# cache(NAME) and variable FC_LADDD_NAME.
AC_DEFUN([_AT_FORTRAN_BATCH_LDADD],
[m4_indir([AT_FORTRAN_BATCH_SEARCH_LTLIBRARY],
          [arguments to link $1],
          [AT_FORTRAN_CACHE_ID([ldadd_$4])],
          [AT_FORTRAN_VAR_ID([FC_LDADD_$4])],
          [$1], [$2], [$3], [$5], [$6])])# _AT_FORTRAN_BATCH_LDADD

# AT_FORTRAN_BATCH_SEARCH_LTLIBRARY(MESSAGE, CACHE, RESULT, BASE, PATH, EXTS,
#                                   [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# ---------------------------------------------------------------------------
# AT_FORTRAN_SEARCH_LTLIBRARY wrapper.
# Check CACHE variable and store to RESULT.
AC_DEFUN([AT_FORTRAN_BATCH_SEARCH_LTLIBRARY],
[AC_CACHE_CHECK([$1], [$2],
[m4_indir([AT_FORTRAN_SEARCH_LTLIBRARY],m4_shift3($@))
 AS_IF([test "x[$]at_result" = x"not found"],
       [AS_VAR_SET([$2], [no])],
       [AS_VAR_SET([$2], [[$]at_result])])])
##
AS_IF([test "x[$]$2" = xno],
      [AS_VAR_SET([$3])],
      [AS_VAR_SET([$3], [[$]$2])])
AC_SUBST($3)])# AT_FORTRAN_BATCH_SEARCH_LTLIBRARY

# AT_FORTRAN_SEARCH_LTLIBRARY(BASE, PATH, [EXTS], [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# ------------------------------------------------------------------------------------
#  Search libtool library with BASE in PATH, appending an extension from list EXTS.
AC_DEFUN([AT_FORTRAN_SEARCH_LTLIBRARY],
[AS_IF([test x"$2" = x],
       [at_search="[$]prefix:[$]ac_default_prefix"],
       [at_search="$2"])
 AS_IF([test x"$3" = x],
       [at_exts=".a .la"],
       [at_exts="$3"])
at_result="not found"
for at_dir in `AS_ECHO([[$]at_search]) | tr ':' '\012'`; do
 for at_ext in [$]at_exts; do
 AS_IF([test "x[$]at_result" = x"not found"],
       [at_tmp="[$]at_dir/$1[$]at_ext"
        AS_IF([test -f "[$]at_tmp"], [at_result=[$]at_tmp])])
done; done
AS_IF([test "x[$]at_result" = x"not found"],
      [$5],
      [$4])
])# AT_FORTRAN_SEARCH_LTLIBRARY

# ======================================================================
# fortran standard specific features

# AT_FC_F2003_ALLOCATABLE_DUMMY()
# -------------------------------
# define HAVE_F2003_ALLOCATABLE_DUMMY if fortran understand allocatable
# property for dummy arguments.
AC_DEFUN([AT_FC_F2003_ALLOCATABLE_DUMMY],
[AC_CACHE_CHECK([whether fortran accepts allocatable dummy arguments],
  [at_cv_f2003_allocatable_dummy],
  [AC_LANG_PUSH([Fortran])
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
[      contains
       integer function f(x)
       integer,intent(in),allocatable :: x
       end function f])],
  [at_cv_f2003_allocatable_dummy=yes],
  [at_cv_f2003_allocatable_dummy=no])
  AC_LANG_POP([Fortran])])
AS_IF([test x"$at_cv_f2003_allocatable_dummy" = xyes],
      [AC_DEFINE([HAVE_F2003_ALLOCATABLE_DUMMY], [1], [allocatable dummy argument])])
])# AT_FC_F2003_ALLOCATABLE_DUMMY

# AT_FC_F2003_ALLOCATABLE_MEMBER()
# -------------------------------
# define HAVE_F2003_ALLOCATABLE_MEMBER if fortran understand allocatable
# member for user-define types
AC_DEFUN([AT_FC_F2003_ALLOCATABLE_MEMBER],
[AC_CACHE_CHECK([whether fortran accepts allocatable type members],
  [at_cv_f2003_allocatable_member],
  [AC_LANG_PUSH([Fortran])
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
[      type at_type
         integer,allocatable :: x
       end type at_type])],
  [at_cv_f2003_allocatable_member=yes],
  [at_cv_f2003_allocatable_member=no])
  AC_LANG_POP([Fortran])])
AS_IF([test x"$at_cv_f2003_allocatable_member" = xyes],
      [AC_DEFINE([HAVE_F2003_ALLOCATABLE_MEMBER], [1], [allocatable type member])])
])# AT_FC_F2003_ALLOCATABLE_MEMBER

dnl Local Variables:
dnl mode: autoconf
dnl End:

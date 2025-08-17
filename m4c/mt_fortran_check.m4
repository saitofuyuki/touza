dnl Filename:  touza/m4c/mt_fortran_check.m4
dnl Maintainer: SAITO Fuyuki
dnl Created:   Jun 3 2020
dnl Time-stamp: <2025/07/02 11:55:45 fuyuki mt_fortran_check.m4>

dnl Copyright (C) 2020-2025
dnl           Japan Agency for Marine-Earth Science and Technology
dnl Licensed under the Apache License, Version 2.0
dnl   (https://www.apache.org/licenses/LICENSE-2.0)

dnl
dnl User level
dnl   MT_FORTRAN_BATCH_CHECK_FUNCTION
dnl   MT_FORTRAN_BATCH_CHECK_SUBROUTINE
dnl   MT_FORTRAN_BATCH_CHECK_MODULE
dnl   MT_FORTRAN_PP_CONCAT
dnl   MT_FORTRAN_MODULE_FLAG
dnl   MT_FORTRAN_BATCH_LDADD

# MT_FORTRAN_CHECK
# ----------------
# Dummy macro
AC_DEFUN([MT_FORTRAN_CHECK])

# ======================================================================
# MT_FORTRAN_BATCH_CHECK_FUNCTION(FUNCTION, ARGUMENTS)
# ------------------------------------------------------
AC_DEFUN([MT_FORTRAN_BATCH_CHECK_FUNCTION],
[m4_indir([MT_FORTRAN_BATCH_CHECK],
          [whether function $1 works],
          [MT_FORTRAN_CACHE_ID($1)],
          [MT_FORTRAN_CPP_HAVE([$1])],
          [MT_FORTRAN_CHECK_FUNCTION],
          $@)])# MT_FORTRAN_BATCH_CHECK_FUNCTION

# MT_FORTRAN_BATCH_CHECK_SUBROUTINE(SUBROUTINE, ARGUMENTS, PROLOGUE[, SYMBOL])
# ------------------------------------------------------
AC_DEFUN([MT_FORTRAN_BATCH_CHECK_SUBROUTINE],
[m4_indir([MT_FORTRAN_BATCH_CHECK],
          [whether subroutine $1 works],
          [MT_FORTRAN_CACHE_ID(m4_default([$4], [$1]))],
          [MT_FORTRAN_CPP_HAVE(m4_default([$4], [$1]))],
          [MT_FORTRAN_CHECK_SUBROUTINE],
          [$1], [$2], [$3])])# MT_FORTRAN_BATCH_CHECK_SUBROUTINE

# MT_FORTRAN_BATCH_CHECK_MODULE(MODULE, [MEMBER])
# ------------------------------------------------------
AC_DEFUN([MT_FORTRAN_BATCH_CHECK_MODULE],
[m4_ifblank([$2],
            [m4_indir([MT_FORTRAN_BATCH_CHECK],
                      [whether module $1 works],
                      [MT_FORTRAN_CACHE_ID($1)],
                      [MT_FORTRAN_CPP_HAVE([$1])],
                      [MT_FORTRAN_CHECK_MODULE],
                      $@)],
            [m4_indir([MT_FORTRAN_BATCH_CHECK],
                      [whether module $1 works with $2],
                      [MT_FORTRAN_CACHE_ID($1_$2)],
                      [MT_FORTRAN_CPP_HAVE([$1_$2])],
                      [MT_FORTRAN_CHECK_MODULE_MEMBER],
                      $@)])])# MT_FORTRAN_BATCH_CHECK_MODULE

# MT_FORTRAN_BATCH_CHECK_STATEMENT(STATEMENT, [SPEC], CODE)
# ------------------------------------------------------
AC_DEFUN([MT_FORTRAN_BATCH_CHECK_STATEMENT],
[m4_ifblank([$2],
 [m4_indir([MT_FORTRAN_BATCH_CHECK],
          [whether statement $1 works],
          [MT_FORTRAN_CACHE_ID($1)],
          [MT_FORTRAN_CPP_HAVE([$1])],
          [MT_FORTRAN_CHECK_STATEMENT_SPEC],
          $@)],
 [m4_indir([MT_FORTRAN_BATCH_CHECK],
          [whether statement $1 works with $2 specifier],
          [MT_FORTRAN_CACHE_ID($1_$2)],
          [MT_FORTRAN_CPP_HAVE([$1_$2])],
          [MT_FORTRAN_CHECK_STATEMENT_SPEC],
          $@)])])# MT_FORTRAN_BATCH_CHECK_STATEMENT

# MT_FORTRAN_BATCH_CHECK(MESSAGE, CACHE, MACRO, CHECKER, ARGUMENTS...)
# -------------------------------------------------
AC_DEFUN([MT_FORTRAN_BATCH_CHECK],
[AC_CACHE_CHECK([$1], [$2],
[m4_indir(m4_shift3($@),
          [AS_VAR_SET([mt_fortran_check], [yes])],
          [AS_VAR_SET([mt_fortran_check], [no])])
AS_VAR_SET([$2], [$[]mt_fortran_check])])
##
AS_IF([test "x$[]$2" = xyes],
      [AC_DEFINE(m4_quote($3), [1], [$1])])
])# MT_FORTRAN_BATCH_CHECK

# ======================================================================

# MT_FORTRAN_CHECK_FUNCTION(FUNCTION, [ARGUMENTS], [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# -------------------------------------------------------------------------------------
# Check whether FUNCTION can be used.
AC_DEFUN([MT_FORTRAN_CHECK_FUNCTION],
[MT_FORTRAN_CHECK_DEFINE([$1],
      [
       write(*,*) $1($2)], [$3], [$4])])# MT_FORTRAN_CHECK_FUNCTION

# MT_FORTRAN_CHECK_SUBROUTINE(SUBROUTINE, [ARGUMENTS], [PROLOGUE], [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# -----------------------------------------------------------------------------------------
# Check whether SUBROUTINE can be used.
AC_DEFUN([MT_FORTRAN_CHECK_SUBROUTINE],
[MT_FORTRAN_CHECK_DEFINE([$1],
      [
       $3
       call $1($2)], [$4], [$5])])# MT_FORTRAN_CHECK_SUBROUTINE

# MT_FORTRAN_CHECK_MODULE(MODULE, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# --------------------------------------------------------------------
# Check whether MODULE can be used.
AC_DEFUN([MT_FORTRAN_CHECK_MODULE],
[MT_FORTRAN_CHECK_DEFINE([$1],
      [
       use $1], [$2], [$3])])# MT_FORTRAN_CHECK_MODULE

# MT_FORTRAN_CHECK_MODULE_MEMBER(MODULE, MEMBER, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# -----------------------------------------------------------------------------------
# Check whether MODULE contains MEMBER.
AC_DEFUN([MT_FORTRAN_CHECK_MODULE_MEMBER],
[MT_FORTRAN_CHECK_DEFINE([$1_$2],
      [
       use $1,only: $2], [$3], [$4])])# MT_FORTRAN_CHECK_MODULE_MEMBER

# MT_FORTRAN_CHECK_STATEMENT_SPEC(STATEMENT, SPEC, CODE, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# -----------------------------------------------------------------------------------------------
# Check whether STATMENT contains SPEC specifier.
AC_DEFUN([MT_FORTRAN_CHECK_STATEMENT_SPEC],
[MT_FORTRAN_CHECK_DEFINE([$1_$2],
     [$3], [$4], [$5])])# MT_FORTRAN_CHECK_STATEMENT_SPEC

# MT_FORTRAN_CHECK_DEFINE(NAME, BODY, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
AC_DEFUN([MT_FORTRAN_CHECK_DEFINE],
[AC_LINK_IFELSE([AC_LANG_PROGRAM([], [$2])], [$3], [$4])])# MT_FORTRAN_CHECK_DEFINE

dnl AC_DEFUN([MT_FORTRAN_CHECK_DEFINE],
dnl [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [$2])], [$3], [$4])])# MT_FORTRAN_CHECK_DEFINE
# ==================================================

# MT_FORTRAN_PP_CONCAT(MACRO, [CACHE])
# ------------------------------------
# Check concatenation works with the preprocessor.
# Result set as MACRO, where 0 if disabled, 1 for '##',
# 2 for traditional mode ('/**/').
AC_DEFUN([MT_FORTRAN_PP_CONCAT],
[_$0([$1], [m4_default([$2], [mt_cv_fortran_pp_concat])])])

AC_DEFUN([_MT_FORTRAN_PP_CONCAT],
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
])# MT_FORTRAN_PP_CONCAT

# ======================================================================
# MT_FORTRAN_PP_COMMENT(MACRO, [DIALECT, [CACHE]])
# ------------------------------------
# Check whether comment character ! is left as it is after preprocessing.
# Result set as MACRO, where 1 if left, 0 if removed.
AC_DEFUN([MT_FORTRAN_PP_COMMENT],
[_$0([$1],
     m4_default([$2], [Fortran]),
     [m4_default([$3], [mt_cv_fortran_pp_comment])])])

AC_DEFUN([_MT_FORTRAN_PP_COMMENT],
[AC_LANG_PUSH([$2])dnl
AC_CACHE_CHECK([preprocessor comment treatment],
[$3],
[# check comment
AS_VAR_SET([$3], 0)
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
[@%:@define IGNORE !
         IGNORE do 100
])],
[AS_VAR_SET([$3], 1)],
[AS_VAR_SET([$3], 0)])])
AS_IF([test x"$[]$3" != x],
      [AC_DEFINE_UNQUOTED([$1], [$[]$3],
                          [preprocessor comment])])
AC_LANG_POP([$2])])# _MT_FORTRAN_PP_COMMENT

# ======================================================================

# MT_FORTRAN_CACHE_ID(NAME)
# -------------------------
AC_DEFUN([MT_FORTRAN_CACHE_ID], [AS_TR_SH([mt_cv_fortran_$1])])

# MT_FORTRAN_CPP_HAVE(NAME)
# -----------------------
AC_DEFUN([MT_FORTRAN_CPP_HAVE], [AS_TR_CPP([HAVE_FORTRAN_$@])])

# MT_FORTRAN_CPP_ID(NAME)
# -----------------------
AC_DEFUN([MT_FORTRAN_CPP_ID], [AS_TR_CPP($@)])

# MT_FORTRAN_VAR_ID(NAME)
# -----------------------
AC_DEFUN([MT_FORTRAN_VAR_ID], [AS_TR_SH($@)])

# ======================================================================
# Header/Module/Library search (ref. AX_F90_LIBRARY_SETUP)

# MT_FORTRAN_MODULE_FLAG(MODULE, PATH, [NAME])
# --------------------------------------------
# Search MODULE in PATH and store result using NAME or MODULE
AC_DEFUN([MT_FORTRAN_MODULE_FLAG], [_$0($@, m4_default([$3], [$1]))])

# _MT_FORTRAN_MODULE_FLAG(MODULE, PATH, NAME)
AC_DEFUN([_MT_FORTRAN_MODULE_FLAG],
[AC_REQUIRE([AC_FC_MODULE_FLAG])dnl
 m4_indir([MT_FORTRAN_BATCH_SEARCH_MODULE],
          [flag to use module $1],
          [MT_FORTRAN_CACHE_ID([compile_$3])],
          [MT_FORTRAN_VAR_ID([FC_MODULE_$3])],
          ["[$]FC_MODINC"],
          [$2],
          [use $1])])# _MT_FORTRAN_MODULE_FLAG

# MT_FORTRAN_BATCH_SEARCH_MODULE(MESSAGE, CACHE, RESULT, FLAG, PATH, BODY)
# ------------------------------------------------------------------------
# MT_FORTRAN_SEARCH_COMPILE wrapper.
# Check CACHE and store to RESULT.
AC_DEFUN([MT_FORTRAN_BATCH_SEARCH_MODULE],
[AC_CACHE_CHECK([$1], [$2],
[m4_indir([MT_FORTRAN_SEARCH_COMPILE],m4_shift2($@))
 AS_IF([test "x[$]mt_result" = x"not found"],
       [AS_VAR_SET([$2], [no])],
       [AS_VAR_SET([$2], [[$]mt_result])])])
##
AS_IF([test "x[$]$2" = xno],
      [AS_VAR_SET([$3])],
      [AS_VAR_SET([$3], [[$]$2])])
AC_SUBST($3)])# MT_FORTRAN_BATCH_SEARCH_MODULE


# MT_FORTRAN_SEARCH_COMPILE(ID, FLAG, PATH, BODY, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# ------------------------------------------------------------------------------------
# Try compile with source BODY with searching module in PATH using
# compiler option FLAG.
AC_DEFUN([MT_FORTRAN_SEARCH_COMPILE],
[AS_IF([test x"$3" = x],
       [mt_search="[$]prefix:[$]ac_default_prefix"],
       [mt_search="$3"])
mt_result="not found"
mt_save_FCFLAGS="[$]FCFLAGS"
for mt_dir in `AS_ECHO([[$]mt_search]) | tr ':' '\012'`; do
 AS_IF([test "x[$]mt_result" = x"not found"],
    [mt_tmp="$2[$]mt_dir"
     FCFLAGS="[$]mt_save_FCFLAGS [$]mt_tmp"
     MT_FORTRAN_CHECK_DEFINE([$1],[$4],[mt_result="[$]mt_tmp"])])
done
FCFLAGS="[$]mt_save_FCFLAGS"
AS_IF([test "x[$]mt_result" = x"not found"],
      [$6],
      [$5])
])# MT_FORTRAN_SEARCH_COMPILE

# MT_FORTRAN_BATCH_LDADD(BASE, PATH, [EXTS], [NAME],
#                        [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# --------------------------------------------------
AC_DEFUN([MT_FORTRAN_BATCH_LDADD],
[_$0([$1], [$2], [$3], m4_default([$4], [$1]), [$5], [$6])])

# _MT_FORTRAN_BATCH_LDADD(BASE, PATH, EXTS, NAME, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# -------------------------------------------------
# Search BASE library in PATH and store results in
# cache(NAME) and variable FC_LADDD_NAME.
AC_DEFUN([_MT_FORTRAN_BATCH_LDADD],
[m4_indir([MT_FORTRAN_BATCH_SEARCH_LTLIBRARY],
          [arguments to link $1],
          [MT_FORTRAN_CACHE_ID([ldadd_$4])],
          [MT_FORTRAN_VAR_ID([FC_LDADD_$4])],
          [$1], [$2], [$3], [$5], [$6])])# _MT_FORTRAN_BATCH_LDADD

# MT_FORTRAN_BATCH_SEARCH_LTLIBRARY(MESSAGE, CACHE, RESULT, BASE, PATH, EXTS,
#                                   [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# ---------------------------------------------------------------------------
# MT_FORTRAN_SEARCH_LTLIBRARY wrapper.
# Check CACHE variable and store to RESULT.
AC_DEFUN([MT_FORTRAN_BATCH_SEARCH_LTLIBRARY],
[AC_CACHE_CHECK([$1], [$2],
[m4_indir([MT_FORTRAN_SEARCH_LTLIBRARY],m4_shift3($@))
 AS_IF([test "x[$]mt_result" = x"not found"],
       [AS_VAR_SET([$2], [no])],
       [AS_VAR_SET([$2], [[$]mt_result])])])
##
AS_IF([test "x[$]$2" = xno],
      [AS_VAR_SET([$3])],
      [AS_VAR_SET([$3], [[$]$2])])
AC_SUBST($3)])# MT_FORTRAN_BATCH_SEARCH_LTLIBRARY

# MT_FORTRAN_SEARCH_LTLIBRARY(BASE, PATH, [EXTS], [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# ------------------------------------------------------------------------------------
#  Search libtool library with BASE in PATH, appending an extension from list EXTS.
AC_DEFUN([MT_FORTRAN_SEARCH_LTLIBRARY],
[AS_IF([test x"$2" = x],
       [mt_search="[$]prefix:[$]ac_default_prefix"],
       [mt_search="$2"])
 AS_IF([test x"$3" = x],
       [mt_exts=".a .la"],
       [mt_exts="$3"])
mt_result="not found"
for mt_dir in `AS_ECHO([[$]mt_search]) | tr ':' '\012'`; do
 for mt_ext in [$]mt_exts; do
 AS_IF([test "x[$]mt_result" = x"not found"],
       [mt_tmp="[$]mt_dir/$1[$]mt_ext"
        AS_IF([test -f "[$]mt_tmp"], [mt_result=[$]mt_tmp])])
done; done
AS_IF([test "x[$]mt_result" = x"not found"],
      [$5],
      [$4])
])# MT_FORTRAN_SEARCH_LTLIBRARY

# ======================================================================
# fortran standard specific features

# MT_FC_F2003_ALLOCATABLE_DUMMY()
# -------------------------------
# define HAVE_F2003_ALLOCATABLE_DUMMY if fortran understand allocatable
# property for dummy arguments.
AC_DEFUN([MT_FC_F2003_ALLOCATABLE_DUMMY],
[AC_CACHE_CHECK([whether fortran accepts allocatable dummy arguments],
  [mt_cv_f2003_allocatable_dummy],
  [AC_LANG_PUSH([Fortran])
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
[      contains
       integer function f(x)
       integer,intent(in),allocatable :: x
       end function f])],
  [mt_cv_f2003_allocatable_dummy=yes],
  [mt_cv_f2003_allocatable_dummy=no])
  AC_LANG_POP([Fortran])])
AS_IF([test x"$mt_cv_f2003_allocatable_dummy" = xyes],
      [AC_DEFINE([HAVE_F2003_ALLOCATABLE_DUMMY], [1], [allocatable dummy argument])])
])# MT_FC_F2003_ALLOCATABLE_DUMMY

# MT_FC_F2003_ALLOCATABLE_MEMBER()
# -------------------------------
# define HAVE_F2003_ALLOCATABLE_MEMBER if fortran understand allocatable
# member for user-define types
AC_DEFUN([MT_FC_F2003_ALLOCATABLE_MEMBER],
[AC_CACHE_CHECK([whether fortran accepts allocatable type members],
  [mt_cv_f2003_allocatable_member],
  [AC_LANG_PUSH([Fortran])
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
[      type mt_type
         integer,allocatable :: x
       end type mt_type])],
  [mt_cv_f2003_allocatable_member=yes],
  [mt_cv_f2003_allocatable_member=no])
  AC_LANG_POP([Fortran])])
AS_IF([test x"$mt_cv_f2003_allocatable_member" = xyes],
      [AC_DEFINE([HAVE_F2003_ALLOCATABLE_MEMBER], [1], [allocatable type member])])
])# MT_FC_F2003_ALLOCATABLE_MEMBER

# MT_FC_F2003_DEFERRED_TYPE()
# -------------------------------
# define HAVE_F2003_DEFERRED_TYPE if fortran understand allocatable
# (deferred) length character
AC_DEFUN([MT_FC_F2003_DEFERRED_TYPE],
[AC_CACHE_CHECK([whether fortran accepts deferred-type parameter],
  [mt_cv_f2003_deferred_type],
  [AC_LANG_PUSH([Fortran])
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
[      character(len=:),allocatable :: T])],
  [mt_cv_f2003_deferred_type=yes],
  [mt_cv_f2003_deferred_type=no])
  AC_LANG_POP([Fortran])])
AS_IF([test x"$mt_cv_f2003_deferred_type" = xyes],
      [AC_DEFINE([HAVE_F2003_DEFERRED_TYPE], [1], [deferred type parameter])])
])# MT_FC_F2003_DEFERRED_TYPE

# MT_FC_CONCATENATION()
# ------------------------------
# define HAVE_FC_CONCATENATION if // works, i.e., not interpreted as
# C preprocessor comment.
AC_DEFUN([MT_FC_CONCATENATION],
[AC_CACHE_CHECK([whether concatenation // works (not preprocessor comment)],
  [mt_cv_concatenation],
  [AC_LANG_PUSH([Fortran])
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([],
[      write(*,*) 'xyz' // 1])],
  [mt_cv_concatenation=no],
  [mt_cv_concatenation=yes])
  AC_LANG_POP([Fortran])])
AS_IF([test x"$mt_cv_concatenation" = xyes],
      [AC_DEFINE([HAVE_FC_CONCATENATION], [1], [// works])])
])# MT_FC_CONCATENATION

dnl Local Variables:
dnl mode: autoconf
dnl End:

dnl Filename:  std/atpackage.m4
dnl Author:    SAITO Fuyuki
dnl Created:   Jun 8 2020
dnl Time-stamp: <2021/01/05 16:38:51 fuyuki atpackage.m4>

dnl Copyright: 2020, 2021 JAMSTEC
dnl Licensed under the Apache License, Version 2.0
dnl   (https://www.apache.org/licenses/LICENSE-2.0)

AC_LANG_PUSH([Fortran])

AT_FORTRAN_BATCH_CHECK_SUBROUTINE([get_command_argument], [1])
AT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env])
AT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [INT32])
AT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [INT64])
AT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [REAL32])
AT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [REAL64])

AT_FORTRAN_BATCH_CHECK_STATEMENT([open], [iomsg],[
character T*(30)
open(UNIT=1,IOMSG=T)])

AC_ARG_VAR([OPT_STDIN_UNIT],    [fortran i/o unit for stdin])
AC_ARG_VAR([OPT_STDOUT_UNIT],   [fortran i/o unit for stdout])
AC_ARG_VAR([OPT_STD_UNITS_TRY], [try limit for brute-force std units check])

AS_IF([test "x[$]AT_FORTRAN_CACHE_ID([iso_fortran_env])" != xyes],
      [at_stdin_def=5;  at_stdout_def=6],
      [at_stdin_def=-1; at_stdout_def=-1])

AT_VAR_SET_DEFAULT([OPT_STDIN_UNIT],  [[$]at_stdin_def])
AT_VAR_SET_DEFAULT([OPT_STDOUT_UNIT], [[$]at_stdout_def])

AC_DEFINE_UNQUOTED([OPT_STDIN_UNIT],  [$OPT_STDIN_UNIT],  [fortran i/o unit for stdin])
AC_DEFINE_UNQUOTED([OPT_STDOUT_UNIT], [$OPT_STDOUT_UNIT], [fortran i/o unit for stdout])

AC_LANG_POP([Fortran])

dnl Local Variables:
dnl mode: autoconf
dnl End:
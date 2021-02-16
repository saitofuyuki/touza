dnl Filename:  std/mt_local.m4
dnl Author:    SAITO Fuyuki
dnl Created:   Jun 8 2020
dnl Time-stamp: <2021/02/13 10:33:03 fuyuki mt_local.m4>

dnl Copyright: 2020, 2021 JAMSTEC
dnl Licensed under the Apache License, Version 2.0
dnl   (https://www.apache.org/licenses/LICENSE-2.0)

AC_LANG_PUSH([Fortran])

MT_FORTRAN_BATCH_CHECK_SUBROUTINE([get_command_argument], [1])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [INT32])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [INT64])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [REAL32])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [REAL64])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [INTEGER_KINDS])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [REAL_KINDS])

MT_FORTRAN_BATCH_CHECK_STATEMENT([open], [iomsg],[
character T*(30)
open(UNIT=1,IOMSG=T)])

MT_FORTRAN_BATCH_CHECK_STATEMENT([inquire], [iolength],[
integer L
inquire(IOLENGTH=L) int(0)])

AC_ARG_VAR([OPT_STDIN_UNIT],    [fortran i/o unit for stdin])
AC_ARG_VAR([OPT_STDOUT_UNIT],   [fortran i/o unit for stdout])
AC_ARG_VAR([OPT_STDERR_UNIT],   [fortran i/o unit for stderr])
AC_ARG_VAR([OPT_STD_UNITS_TRY], [try limit for brute-force std units check])

AS_IF([test "x[$]MT_FORTRAN_CACHE_ID([iso_fortran_env])" != xyes],
      [mt_stdin_def=5;  mt_stdout_def=6;  mt_stderr_def=0],
      [mt_stdin_def=-1; mt_stdout_def=-1; mt_stderr_def=-1])

MT_VAR_SET_DEFAULT([OPT_STDIN_UNIT],  [[$]mt_stdin_def])
MT_VAR_SET_DEFAULT([OPT_STDOUT_UNIT], [[$]mt_stdout_def])
MT_VAR_SET_DEFAULT([OPT_STDERR_UNIT], [[$]mt_stderr_def])

AC_DEFINE_UNQUOTED([OPT_STDIN_UNIT],  [$OPT_STDIN_UNIT],  [fortran i/o unit for stdin])
AC_DEFINE_UNQUOTED([OPT_STDOUT_UNIT], [$OPT_STDOUT_UNIT], [fortran i/o unit for stdout])
AC_DEFINE_UNQUOTED([OPT_STDERR_UNIT], [$OPT_STDERR_UNIT], [fortran i/o unit for stderr])

MT_FC_CONCATENATION()

AC_LANG_POP([Fortran])

dnl Local Variables:
dnl mode: autoconf
dnl End:

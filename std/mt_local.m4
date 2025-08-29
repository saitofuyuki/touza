dnl Filename:  std/mt_local.m4
dnl Author:    SAITO Fuyuki
dnl Created:   Jun 8 2020
dnl Time-stamp: <2025/08/28 15:11:32 fuyuki mt_local.m4>

dnl Copyright: 2020-2023 JAMSTEC
dnl Licensed under the Apache License, Version 2.0
dnl   (https://www.apache.org/licenses/LICENSE-2.0)

AC_LANG_PUSH([Fortran])

MT_FORTRAN_BATCH_CHECK_SUBROUTINE([get_command_argument], [1], [])
MT_FORTRAN_BATCH_CHECK_SUBROUTINE([getarg], [1, T], [
     character T*30])
MT_FORTRAN_BATCH_CHECK_FUNCTION([command_argument_count], [])

MT_FORTRAN_BATCH_CHECK_FUNCTION([new_line], ['a'])

dnl Fortran 2008
MT_FORTRAN_BATCH_CHECK_FUNCTION([hypot], [0.0, 0.0])
MT_FORTRAN_BATCH_CHECK_FUNCTION([atanh], [0.0])
MT_FORTRAN_BATCH_CHECK_FUNCTION([acosh], [1.0])
MT_FORTRAN_BATCH_CHECK_FUNCTION([asinh], [0.0])

dnl SX
MT_FORTRAN_BATCH_CHECK_FUNCTION([iargc], [])
MT_FORTRAN_BATCH_CHECK_FUNCTION([fseek],  [0,0,0])
MT_FORTRAN_BATCH_CHECK_FUNCTION([ftell],  [0])
MT_FORTRAN_BATCH_CHECK_FUNCTION([ftelli8], [0])

MT_FORTRAN_BATCH_CHECK_MODULE([mpi_f08])
MT_FORTRAN_BATCH_CHECK_MODULE([mpi])
dnl MT_FORTRAN_BATCH_CHECK_MODULE([mpi], [mpi_bcast])
dnl MT_FORTRAN_BATCH_CHECK_MODULE([mpi], [mpi_group_translate_ranks])

MT_FORTRAN_BATCH_CHECK_MODULE_SUBROUTINE([mpi],
      [mpi_group_translate_ranks],
      [GROUP1, N, RANKS1, GROUP2, RANKS2, IERROR],
      [INTEGER GROUP1, N, RANKS1(16), GROUP2, RANKS2(16), IERROR])

MT_FORTRAN_BATCH_CHECK_MODULE_SUBROUTINE([mpi],
      [mpi_bcast],
      [BUFFER, COUNT, DATATYPE, ROOT, COMM, IERROR],
      [integer BUFFER(1)
       integer COUNT, DATATYPE, ROOT, COMM, IERROR])

MT_FORTRAN_BATCH_CHECK_MODULE_SUBROUTINE([mpi],
      [mpi_reduce],
      [SENDBUF, RECVBUF, COUNT, DATATYPE, OP, ROOT, COMM, IERROR],
      [integer SENDBUF(1), RECVBUF(1)
       integer COUNT, DATATYPE, OP, ROOT, COMM, IERROR])

MT_FORTRAN_BATCH_CHECK_MODULE_SUBROUTINE([mpi],
      [mpi_isend],
      [BUF, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR],
      [integer  BUF(1)
       integer COUNT, DATATYPE, DEST, TAG, COMM, REQUEST, IERROR])

MT_FORTRAN_BATCH_CHECK_MODULE_SUBROUTINE([mpi],
      [mpi_irecv],
      [BUF, COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST, IERROR],
      [integer BUF(1)
       integer COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST, IERROR])

MT_FORTRAN_BATCH_CHECK_MODULE_SUBROUTINE([mpi],
      [mpi_send],
      [BUF, COUNT, DATATYPE, DEST, TAG, COMM, IERROR],
      [integer BUF(1)
       integer COUNT, DATATYPE, DEST, TAG, COMM, IERROR])

MT_FORTRAN_BATCH_CHECK_MODULE_SUBROUTINE([mpi],
      [mpi_recv],
      [BUF, COUNT, DATATYPE, SOURCE, TAG, COMM, STATUS, IERROR],
      [integer BUF(1)
       integer COUNT, DATATYPE, SOURCE, TAG, COMM
       integer STATUS(16), IERROR])

MT_FORTRAN_BATCH_CHECK_MODULE_SUBROUTINE([mpi],
      [mpi_gather],
      [SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT, RECVTYPE, ROOT, COMM, IERROR],
      [integer SENDBUF(1), RECVBUF(1)
       integer SENDCOUNT, SENDTYPE, RECVCOUNT, RECVTYPE, ROOT
       integer COMM, IERROR])

MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [INT8])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [INT16])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [INT32])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [INT64])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [REAL32])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [REAL64])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [REAL128])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [CHARACTER_KINDS])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [INTEGER_KINDS])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [REAL_KINDS])

MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [CHARACTER_STORAGE_SIZE])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [NUMERIC_STORAGE_SIZE])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [FILE_STORAGE_SIZE])

MT_FORTRAN_BATCH_CHECK_MODULE([iso_fortran_env], [IOSTAT_END])

MT_FORTRAN_BATCH_CHECK_MODULE([iso_c_binding])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_c_binding], [C_SIZE_T])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_c_binding], [C_INT8_T])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_c_binding], [C_INT16_T])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_c_binding], [C_INT32_T])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_c_binding], [C_INT64_T])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_c_binding], [C_FLOAT])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_c_binding], [C_DOUBLE])
MT_FORTRAN_BATCH_CHECK_MODULE([iso_c_binding], [C_LONG_DOUBLE])

AM_CONDITIONAL([have_iso_c_binding],
[test x"$mt_cv_fortran_iso_c_binding" = xyes])

MT_FORTRAN_BATCH_CHECK_MODULE([ieee_arithmetic])

MT_FORTRAN_BATCH_CHECK_STATEMENT([open], [iomsg],[
      character T*(30)
      open(UNIT=1,IOMSG=T)])

MT_FORTRAN_BATCH_CHECK_STATEMENT([open], [convert],[
      open(UNIT=1,CONVERT='BIG_ENDIAN')])

MT_FORTRAN_BATCH_CHECK_STATEMENT([open], [stream],[
      open(UNIT=1,ACCESS='STREAM')])

MT_FORTRAN_BATCH_CHECK_STATEMENT([inquire], [iolength],[
      integer L
      inquire(IOLENGTH=L) int(0)])

MT_FORTRAN_BATCH_CHECK_STATEMENT([inquire], [pos],[
      integer L
      inquire(10, POS=L)])

MT_FORTRAN_BATCH_CHECK_STATEMENT([inquire], [convert],[
      character T*(30)
      inquire(10, CONVERT=T)])

MT_FORTRAN_BATCH_CHECK_STATEMENT([flush], [unit],[
      flush(unit=1)])

MT_FORTRAN_BATCH_CHECK_STATEMENT([format], [zero_width_real],[
  102 format(E0.3)
      write(*, 102) 0.0])

MT_FORTRAN_BATCH_CHECK_STATEMENT([backslash_noquote], [],[
      character(len=1) :: BS = '\'
      write(*, *) BS
])

dnl Fortran 2008
MT_FORTRAN_BATCH_CHECK_STATEMENT([error_stop], [],[
      error stop 1
])
dnl Fortran 2018
MT_FORTRAN_BATCH_CHECK_STATEMENT([error_stop], [quiet],[
      error stop 1, quiet=.TRUE.
])
MT_FORTRAN_BATCH_CHECK_STATEMENT([stop], [quiet],[
      stop 1, quiet=.TRUE.
])

dnl system procedures
MT_FORTRAN_BATCH_CHECK_SUBROUTINE([get_environment_variable], ['A'])
dnl GNU extensions
MT_FORTRAN_BATCH_CHECK_SUBROUTINE([getlog], [T], [
      character T*30])
MT_FORTRAN_BATCH_CHECK_SUBROUTINE([getenv], ['A', T], [
      character T*30])
MT_FORTRAN_BATCH_CHECK_SUBROUTINE([hostnm], [T], [
      character T*30])

MT_FORTRAN_BATCH_CHECK_SUBROUTINE([exit], [1], [])


dnl GNU, nfort
dnl    GNU provides both function and subroutine forms,
dnl    while nfort provides the latter form by
dnl    F90_UNIX_DIR module.
MT_FORTRAN_BATCH_CHECK_FUNCTION([chdir], ['.'])
dnl MT_FORTRAN_BATCH_CHECK_SUBROUTINE([getcwd], [T], [
dnl       character T*30])

MT_FORTRAN_BATCH_CHECK_STATEMENT([getcwd], [],[
      character(len=10) :: P
      write(*, *) getcwd(P)
])

dnl etime
dnl ETIME in GNU extension requires real(4) arguments
MT_FORTRAN_BATCH_CHECK_SUBROUTINE([etime], [V, T], [
      real(8) V(2), T], [ETIME_8])
MT_FORTRAN_BATCH_CHECK_SUBROUTINE([etime], [V, T], [
      real(4) V(2), T], [ETIME_4])

dnl intel IFPORT module
MT_FORTRAN_BATCH_CHECK_MODULE([ifport], [getenv])
MT_FORTRAN_BATCH_CHECK_MODULE([ifport], [getlog])
MT_FORTRAN_BATCH_CHECK_MODULE([ifport], [hostnam])
MT_FORTRAN_BATCH_CHECK_MODULE([ifport], [chdir])
MT_FORTRAN_BATCH_CHECK_MODULE([ifport], [getcwd])

dnl SX F90_UNIX_ENV module
MT_FORTRAN_BATCH_CHECK_MODULE([f90_unix_env], [getenv])
MT_FORTRAN_BATCH_CHECK_MODULE([f90_unix_env], [getlogin])
MT_FORTRAN_BATCH_CHECK_MODULE([f90_unix_env], [gethostname])

dnl SX F90_UNIX_DIR module
MT_FORTRAN_BATCH_CHECK_MODULE([f90_unix_dir], [chdir])
MT_FORTRAN_BATCH_CHECK_MODULE([f90_unix_dir], [getcwd])

dnl visual fortran DFPORT module
MT_FORTRAN_BATCH_CHECK_MODULE([dfport], [chdir])
MT_FORTRAN_BATCH_CHECK_MODULE([dfport], [getcwd])
MT_FORTRAN_BATCH_CHECK_MODULE([dfport], [getenv])
MT_FORTRAN_BATCH_CHECK_MODULE([dfport], [getlog])
MT_FORTRAN_BATCH_CHECK_MODULE([dfport], [hostnam])

MT_FC_F2003_DEFERRED_TYPE()
MT_FC_F2003_ALLOCATABLE_DUMMY()
MT_FC_F2003_ALLOCATABLE_MEMBER()

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

dnl Filename:   touza/m4c/mt_prog_fc_mpi.m4
dnl Maintainer: SAITO Fuyuki
dnl Created:    Nov 30 2020
dnl Time-stamp: <2022/09/16 14:52:52 fuyuki mt_prog_fc_mpi.m4>

dnl Copied from example in AX_PROG_FC_MPI description,
dnl with little modification.
dnl Original license: Copyright (C) 2010,2011 Olaf Lenz

# MT_PROG_FC_MPI
# --------------
# wrap AX_PROG_FC_MPI() in autoconf archive.
AC_DEFUN([MT_PROG_FC_MPI],
[AC_ARG_WITH([mpi],
  [AS_HELP_STRING([--with-mpi],
                  [compile with MPI support @<:@default=auto@:>@])],
  [], [with_mpi=auto])
AX_PROG_FC_MPI([test x"$with_mpi" != xno],
[use_mpi=yes],
[use_mpi=no
AS_IF([test x"$with_mpi" = xyes],
      [AC_MSG_FAILURE([MPI compiler requested, but couldn't use MPI.])],
      [AC_MSG_WARN([No MPI compiler found, won't use MPI.])])])
AS_IF([test "x$use_mpi" = xyes],
      [AC_DEFINE([OPT_USE_MPI], [1], [Define if you use mpi])])
AM_CONDITIONAL([with_mpi], [test "x$use_mpi" = xyes])
])# MT_PROG_FC_MPI
dnl Local Variables:
dnl mode: autoconf
dnl end:

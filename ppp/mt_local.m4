dnl Filename:  ppp/mt_local.m4
dnl Author:    SAITO Fuyuki
dnl Created:   Jan 26 2022
dnl Time-stamp: <2022/05/23 10:22:59 c0210 mt_local.m4>

dnl Copyright (C) 2022
dnl           Japan Agency for Marine-Earth Science and Technology
dnl Licensed under the Apache License, Version 2.0
dnl   (https://www.apache.org/licenses/LICENSE-2.0)

AC_LANG_PUSH([Fortran])
MT_FORTRAN_BATCH_CHECK_SUBROUTINE([ftrace_region_begin], ['a'], [])
MT_FORTRAN_BATCH_CHECK_SUBROUTINE([ftrace_region_end], ['a'], [])

MT_FORTRAN_BATCH_CHECK_MODULE([mpi], [mpi_group_translate_ranks])
MT_FORTRAN_BATCH_CHECK_MODULE([mpi], [mpi_isend])
MT_FORTRAN_BATCH_CHECK_MODULE([mpi], [mpi_irecv])

AC_LANG_POP([Fortran])

dnl Local Variables:
dnl mode: autoconf
dnl End:

dnl Filename:  trp/mt_local.m4
dnl Author:    SAITO Fuyuki
dnl Created:   Feb 26 2021
dnl Time-stamp: <2022/05/23 10:24:08 c0210 mt_local.m4>

dnl Copyright (C) 2021,2022
dnl           Japan Agency for Marine-Earth Science and Technology
dnl Licensed under the Apache License, Version 2.0
dnl   (https://www.apache.org/licenses/LICENSE-2.0)

AC_LANG_PUSH([Fortran])

MT_FORTRAN_BATCH_CHECK_MODULE([ieee_arithmetic])
MT_FORTRAN_BATCH_CHECK_FUNCTION([amt],  [0.0])dnl   (SX) an alternate for FRACTION()
MT_FORTRAN_BATCH_CHECK_FUNCTION([ire],  [0.0])dnl   (SX) an alternate for EXPONENT()
MT_FORTRAN_BATCH_CHECK_FUNCTION([exp2], [0.0])dnl   (SX) an alternate for SET_EXPONENT()

MT_FORTRAN_BATCH_CHECK_MODULE([mpi], [mpi_isend])
MT_FORTRAN_BATCH_CHECK_MODULE([mpi], [mpi_irecv])

AC_LANG_POP([Fortran])
dnl Local Variables:
dnl mode: autoconf
dnl End:

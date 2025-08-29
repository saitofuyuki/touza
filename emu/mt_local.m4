dnl Filename:  emu/mt_local.m4
dnl Author:    SAITO Fuyuki
dnl Created:   Jun 14 2020
dnl Time-stamp: <2025/08/27 08:49:28 fuyuki mt_local.m4>

dnl Copyright (C) 2020, 2025
dnl           Japan Agency for Marine-Earth Science and Technology
dnl Licensed under the Apache License, Version 2.0
dnl   (https://www.apache.org/licenses/LICENSE-2.0)

AC_LANG_PUSH([Fortran])

MT_FORTRAN_BATCH_CHECK_STATEMENT([constant_atan2], [],[
      implicit none
      integer,parameter :: KTGT = SELECTED_REAL_KIND(15, 307)
      real(kind=KTGT),parameter :: p = ATAN2(0.0_KTGT, 1.0_KTGT)
])

AC_LANG_POP([Fortran])

dnl Local Variables:
dnl mode: autoconf
dnl End:

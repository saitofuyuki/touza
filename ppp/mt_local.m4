dnl Filename:  ppp/mt_local.m4
dnl Author:    SAITO Fuyuki
dnl Created:   Jan 26 2022
dnl Time-stamp: <2025/08/28 14:48:43 fuyuki mt_local.m4>

dnl Copyright (C) 2025
dnl           Japan Agency for Marine-Earth Science and Technology
dnl Licensed under the Apache License, Version 2.0
dnl   (https://www.apache.org/licenses/LICENSE-2.0)

AC_LANG_PUSH([Fortran])
MT_FORTRAN_BATCH_CHECK_SUBROUTINE([ftrace_region_begin], ['a'], [])
MT_FORTRAN_BATCH_CHECK_SUBROUTINE([ftrace_region_end], ['a'], [])
AC_LANG_POP([Fortran])

dnl Local Variables:
dnl mode: autoconf
dnl End:

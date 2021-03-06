dnl Filename:  cal/mt_local.m4
dnl Author:    SAITO Fuyuki
dnl Created:   Jun 8 2020
dnl Time-stamp: <2021/01/22 08:55:53 fuyuki mt_local.m4>

dnl Copyright (C) 2020, 2021
dnl           Japan Agency for Marine-Earth Science and Technology
dnl Licensed under the Apache License, Version 2.0
dnl   (https://www.apache.org/licenses/LICENSE-2.0)

AC_REQUIRE_AUX_FILE([tap-driver.sh])
AC_LANG_PUSH([Fortran])
MT_FORTRAN_BATCH_CHECK_FUNCTION([huge], [0])
AC_LANG_POP([Fortran])

dnl Local Variables:
dnl mode: autoconf
dnl End:

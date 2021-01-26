dnl Filename:   touza/m4c/mt_define.m4
dnl Maintainer: SAITO Fuyuki
dnl Created:    Jun 22 2020
dnl Time-stamp: <2021/01/22 11:20:59 fuyuki mt_define.m4>

dnl Copyright: 2021 JAMSTEC
dnl Licensed under the Apache License, Version 2.0
dnl   (https://www.apache.org/licenses/LICENSE-2.0)

# MT_FORTRAN_DEFINE(VARIABLE, VALUE, [DESCRIPTION])
# -------------------------------------------------
# AC_DEFINE wrapper to define VARIABLE with quotation.
AC_DEFUN([MT_FORTRAN_DEFINE],
[AC_DEFINE([$1], ['$2'], [$3])])# MT_FORTRAN_DEFINE

dnl Local Variables:
dnl mode: autoconf
dnl end:

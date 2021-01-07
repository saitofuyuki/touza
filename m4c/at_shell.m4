dnl Filename:   at_shell.m4
dnl Maintainer: SAITO Fuyuki
dnl Created:    Jun 4 2020
dnl Time-stamp: <2021/01/07 13:55:23 fuyuki at_shell.m4>

dnl Copyright: 2020, 2021 JAMSTEC
dnl Licensed under the Apache License, Version 2.0
dnl   (https://www.apache.org/licenses/LICENSE-2.0)

# AT_SHELL
# --------
# Dummy macro
AC_DEFUN([AT_SHELL])

# AT_VAR_SET_DEFAULT(VAR, [VALUE])
# ------------------
AC_DEFUN([AT_VAR_SET_DEFAULT],
[AS_VAR_SET_IF([$1], [], [AS_VAR_SET([$1], [$2])])])# AT_VAR_SET_DEFAULT

dnl Local Variables:
dnl mode: autoconf
dnl end:

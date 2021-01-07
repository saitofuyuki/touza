dnl Filename:   at_init.m4
dnl Maintainer: SAITO Fuyuki
dnl Created:    Jun 4 2020
dnl Time-stamp: <2021/01/05 22:56:10 fuyuki at_init.m4>

dnl Copyright: 2020, 2021 JAMSTEC
dnl Licensed under the Apache License, Version 2.0
dnl   (https://www.apache.org/licenses/LICENSE-2.0)

# AT_INIT([LOCAL-FILE])
# ---------------------
# AT system initialization dispatcher.
# Also load LOCAL-FILE (or atpackage.m4 default) to define
# AT_VERSION and package specific macros for later expansion.
# Usage:
#   AT_INIT()
#   AC_INIT(<PACKAGE>, AT_VERSION, ....)
AC_DEFUN([AT_INIT],
[m4_pattern_forbid([^[_]?AT_])
_$0(m4_quote(m4_default([$2], atpackage.m4)))dnl
])# AT_INIT

# _AT_INIT(LOCAL-FILE)
# --------------------
AC_DEFUN([_AT_INIT],
[dnl m4_define([_AT_SAVED_CONTENTS])
m4_errprintn(m4_location[: load $1])
at_sinclude([$1])
m4_define([AT_LOCAL_FILE], [$1])
m4_define([AT_VERSION()], m4_quote(_AT_PACKAGE_VERSION))dnl
AT_DEBUG([$3], [AT_VERSION], [AT_VERSION($1)])dnl
])# _AT_INIT

# AT_VERSION([NAME])
# ------------------
m4_define([AT_VERSION],
[m4_ifdef([$0($1)], [m4_indir([$0($1)])], [0.0.0.0])])

# AT_PACKAGE_VERSION(VERSION)
# ---------------------------
# Set package version string.  Called in atpackage.m4.
m4_define([AT_PACKAGE_VERSION], [m4_define([_$0], [$1])])

# _AT_PACKAGE_VERSION
# -------------------
m4_define([_AT_PACKAGE_VERSION], [0.0.0,0])

# at_sinclude(FILE)
# at_include(FILE)
# -----------------
# m4_sinclude m4_include wrappers
m4_define([at_sinclude], [m4_sinclude][([$1])])
m4_define([at_include],  [m4_include][([$1])])

# AT_INIT__(DUMMY)
# ----------------
# helper macro to predefine at-system macros for aclocal.
# DO NOT USE.
AC_DEFUN([AT_INIT__(DUMMY)],
[AT_AM_INCLUDE()
 AT_FORTRAN_CHECK()
 dnl AT_PACKAGE()
 AT_SHELL()])# AT_INIT__(DUMMY)

# AT_DEBUG(MACRO, ...)
# -------------------
AC_DEFUN([AT_DEBUG], [m4_map_args([_$0], $@)])
AC_DEFUN([_AT_DEBUG],[_$0([$1], m4_quote($1))])
AC_DEFUN([__AT_DEBUG], [])

dnl Local Variables:
dnl mode: autoconf
dnl end:

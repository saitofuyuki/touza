dnl Filename:   touza/m4c/mt_init.m4
dnl Maintainer: SAITO Fuyuki
dnl Created:    Jun 4 2020
dnl Time-stamp: <2021/01/22 08:48:40 fuyuki mt_init.m4>

dnl Copyright: 2020, 2021 JAMSTEC
dnl Licensed under the Apache License, Version 2.0
dnl   (https://www.apache.org/licenses/LICENSE-2.0)

# MT_INIT([LOCAL-FILE])
# ---------------------
# MT system initialization dispatcher.
# Also load LOCAL-FILE (or mt_local.m4 default) to define
# MT_VERSION and package specific macros for later expansion.
# Usage:
#   MT_INIT()
#   AC_INIT(<PACKAGE>, MT_VERSION, ....)
AC_DEFUN([MT_INIT],
[m4_pattern_forbid([^[_]?MT_])
_$0(m4_quote(m4_default([$2], mt_local.m4)))dnl
])# MT_INIT

# _MT_INIT(LOCAL-FILE)
# --------------------
AC_DEFUN([_MT_INIT],
[dnl m4_define([_MT_SAVED_CONTENTS])
m4_errprintn(m4_location[: load $1])
mt_sinclude([$1])
m4_define([MT_LOCAL_FILE], [$1])
m4_define([MT_VERSION()], m4_quote(_MT_PACKAGE_VERSION))dnl
MT_DEBUG([$3], [MT_VERSION], [MT_VERSION($1)])dnl
])# _MT_INIT

# MT_VERSION([NAME])
# ------------------
m4_define([MT_VERSION],
[m4_ifdef([$0($1)], [m4_indir([$0($1)])], [0.0.0.0])])

# MT_PACKAGE_VERSION(VERSION)
# ---------------------------
# Set package version string.  Typically called in mt_local.m4.
m4_define([MT_PACKAGE_VERSION], [m4_define([_$0], [$1])])

# _MT_PACKAGE_VERSION
# -------------------
m4_define([_MT_PACKAGE_VERSION], [0.0.0,0])

# mt_sinclude(FILE)
# mt_include(FILE)
# -----------------
# m4_sinclude m4_include wrappers
m4_define([mt_sinclude], [m4_sinclude][([$1])])
m4_define([mt_include],  [m4_include][([$1])])

# MT_INIT__(DUMMY)
# ----------------
# helper macro to predefine MT-system macros for aclocal.
# DO NOT USE.
AC_DEFUN([MT_INIT__(DUMMY)],
[MT_AM_INCLUDE()
 MT_FORTRAN_CHECK()
 dnl MT_PACKAGE()
 MT_SHELL()])# MT_INIT__(DUMMY)

# MT_DEBUG(MACRO, ...)
# -------------------
AC_DEFUN([MT_DEBUG], [m4_map_args([_$0], $@)])
AC_DEFUN([_MT_DEBUG],[_$0([$1], m4_quote($1))])
AC_DEFUN([__MT_DEBUG], [])

dnl Local Variables:
dnl mode: autoconf
dnl end:

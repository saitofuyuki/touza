dnl Filename:   at_am_include.m4
dnl Maintainer: SAITO Fuyuki
dnl Created:    Jun 16 2020
dnl Time-stamp: <2021/01/07 13:53:50 fuyuki at_am_include.m4>

dnl Copyright: 2020, 2021 JAMSTEC
dnl Licensed under the Apache License, Version 2.0
dnl   (https://www.apache.org/licenses/LICENSE-2.0)

# AT_AM_INCLUDE
# -------------
# generate common include file for automake
AC_DEFUN([AT_AM_INCLUDE],
[
AX_ADD_AM_MACRO_STATIC([BUILD_AUX  =    ${AX_DOLLAR}(top_srcdir)/build-aux])

AX_ADD_AM_MACRO_STATIC([
if CLEAN_FCMOD
MOSTLYCLEANFILES += *.${AX_DOLLAR}(FC_MODEXT)
endif

install-exec-hook: install-mod
])

AT_ADD_RECURSIVE_AM_MACRO_STATIC([install-mod],
[if INSTALL_MODULES
	if test -z '${AX_DOLLAR}(pkgmoddir)'; then \\
		false; \\
	else \\
		${AX_DOLLAR}(MKDIR_P) ${AX_DOLLAR}(pkgmoddir); \\
		${AX_DOLLAR}(install_sh_DATA) -t ${AX_DOLLAR}(pkgmoddir) *.${AX_DOLLAR}(FC_MODEXT); \\
	fi
endif

])
AT_ADD_RECURSIVE_AM_MACRO_STATIC([check-bin],[], [${AX_DOLLAR}(check_PROGRAMS)])

])

# AT_ADD_RECURSIVE_AM_MACRO_STATIC(TARGET, RULE, [DEP])
# ----------------------------------------------
AC_DEFUN([AT_ADD_RECURSIVE_AM_MACRO_STATIC],
[AX_ADD_RECURSIVE_AM_MACRO_STATIC([$1],
[$1: $1-recursive

$1-am:$3
$2])])

dnl Local Variables:
dnl mode: autoconf
dnl end:

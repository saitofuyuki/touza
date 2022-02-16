dnl Filename:   touza/m4c/mt_am_include.m4
dnl Maintainer: SAITO Fuyuki
dnl Created:    Jun 16 2020
dnl Time-stamp: <2022/02/15 09:29:07 fuyuki mt_am_include.m4>

dnl Copyright: 2020, 2021 JAMSTEC
dnl Licensed under the Apache License, Version 2.0
dnl   (https://www.apache.org/licenses/LICENSE-2.0)

# MT_AM_INCLUDE
# -------------
# generate common include file for automake
AC_DEFUN([MT_AM_INCLUDE],
[
AX_ADD_AM_MACRO_STATIC([BUILD_AUX  =    ${AX_DOLLAR}(top_srcdir)/build-aux])

AX_ADD_AM_MACRO_STATIC([
if CLEAN_FCMOD
MOSTLYCLEANFILES += *.${AX_DOLLAR}(FC_MODEXT)
endif

DEBUG   =
AM_FCFLAGS  = @AM_FCFLAGS@ ${AX_DOLLAR}(DEBUG)
AM_CPPFLAGS = -I${AX_DOLLAR}(top_srcdir)

moddir      = @moddir@

install-exec-hook: install-mod

])

AX_ADD_AM_MACRO_STATIC([
COPY_SOURCE_TARGETS = ${AX_DOLLAR}(lib@TOUZA_NAME@_la_SOURCES:%=copy-%)
DIFF_SOURCE_TARGETS = ${AX_DOLLAR}(lib@TOUZA_NAME@_la_SOURCES:%=diff-%)
${AX_DOLLAR}(COPY_SOURCE_TARGETS):
	@target=\`echo ${AX_DQ}${AX_DOLLAR}@${AX_DQ} | sed -e 's/^copy-//'\`;\\
	if test -e ${AX_DOLLAR}(builddir)/${AX_DOLLAR}${AX_DOLLAR}{target}; then \\
	  echo ${AX_DQ}exists ${AX_DOLLAR}${AX_DOLLAR}{target}.${AX_DQ}; \\
	else \\
	  echo ${AX_DQ}clone ${AX_DOLLAR}(srcdir)/${AX_DOLLAR}${AX_DOLLAR}{target}${AX_DQ}; \\
	  cp ${AX_DOLLAR}(srcdir)/${AX_DOLLAR}${AX_DOLLAR}{target} ${AX_DOLLAR}(builddir)/${AX_DOLLAR}${AX_DOLLAR}{target} || exit ${AX_DOLLAR}${AX_DOLLAR}?; \\
	fi
${AX_DOLLAR}(DIFF_SOURCE_TARGETS):
	@target=\`echo ${AX_DQ}${AX_DOLLAR}@${AX_DQ} | sed -e 's/^diff-//'\`;\\
	diff ${AX_DOLLAR}(srcdir)/${AX_DOLLAR}${AX_DOLLAR}{target} ${AX_DOLLAR}(builddir)/${AX_DOLLAR}${AX_DOLLAR}{target}
])

MT_ADD_RECURSIVE_AM_MACRO_STATIC([install-mod],
[if INSTALL_MODULES
	if test -z '${AX_DOLLAR}(moddir)'; then \\
		false; \\
	else \\
		${AX_DOLLAR}(MKDIR_P) ${AX_DOLLAR}(moddir); \\
		${AX_DOLLAR}(install_sh_DATA) -t ${AX_DOLLAR}(moddir) *.${AX_DOLLAR}(FC_MODEXT); \\
	fi
endif

])
MT_ADD_RECURSIVE_AM_MACRO_STATIC([check-bin],[], [${AX_DOLLAR}(check_PROGRAMS)])

])

# MT_ADD_RECURSIVE_AM_MACRO_STATIC(TARGET, RULE, [DEP])
# ----------------------------------------------
AC_DEFUN([MT_ADD_RECURSIVE_AM_MACRO_STATIC],
[AX_ADD_RECURSIVE_AM_MACRO_STATIC([$1],
[$1: $1-recursive

$1-am:$3
$2])])

dnl Local Variables:
dnl mode: autoconf
dnl end:

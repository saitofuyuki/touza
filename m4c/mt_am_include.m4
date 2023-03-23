dnl Filename:   touza/m4c/mt_am_include.m4
dnl Maintainer: SAITO Fuyuki
dnl Created:    Jun 16 2020
dnl Time-stamp: <2023/03/13 11:28:15 fuyuki mt_am_include.m4>

dnl Copyright: 2020,2021,2022 JAMSTEC
dnl Licensed under the Apache License, Version 2.0
dnl   (https://www.apache.org/licenses/LICENSE-2.0)

# MT_AM_INCLUDE(SOURCE)
# ---------------------
# generate common include file for automake
# SOURCE is used to define COPY_SOURCE_TARGETS and DIFF_SOURCE_TARGETS.
AC_DEFUN([MT_AM_INCLUDE],
[
AX_ADD_AM_MACRO_STATIC([BUILD_AUX  =    ${AX_DOLLAR}(top_srcdir)/build-aux])

AX_NULL=''
AX_ADD_AM_MACRO_STATIC([
CLEANFILES =
MOSTLYCLEANFILES =

LIST_MODFILES = .modfiles

if CLEAN_FCMOD
MOSTLYCLEANFILES += *.${AX_DOLLAR}(FC_MODEXT) ${AX_DOLLAR}(LIST_MODFILES)
endif

DEBUG   =
${AX_NULL}AM_FCFLAGS=@AM_FCFLAGS@ ${AX_DOLLAR}(DEBUG)
${AX_NULL}AM_CPPFLAGS=-I${AX_DOLLAR}(top_srcdir)

# no blank between -MT and NAME
${AX_NULL}AM_FCFLAGS_DEP='-MT${AX_DOLLAR}@' -MD -MP -MF ${AX_DOLLAR}*.dep
# AM_FCFLAGS_DEP = -MD -MP -MF ${AX_DOLLAR}*.dep
if gcc_dependencies
${AX_NULL}AM_FCFLAGS+=${AX_DOLLAR}(AM_FCFLAGS_DEP)
MOSTLYCLEANFILES += *.dep
endif

${AX_NULL}AM_FCFLAGS_MODULE=
${AX_NULL}AM_FCFLAGS+=${AX_DOLLAR}(AM_FCFLAGS_MODULE)

moddir      = @moddir@
install-exec-hook: install-mod
uninstall-hook: uninstall-mod

echo-srcdir:
	@echo ${AX_DOLLAR}(srcdir)
echo-builddir:
	@echo ${AX_DOLLAR}(builddir)
echo-top_srcdir:
	@echo ${AX_DOLLAR}(top_srcdir)
echo-top_builddir:
	@echo ${AX_DOLLAR}(top_builddir)
echo-build_aux:
	@echo ${AX_DOLLAR}(BUILD_AUX)

lib:
	@if test ${AX_DQ}${AX_DOLLAR}(top_builddir)${AX_DQ} = ${AX_DQ}${AX_DOLLAR}(builddir)${AX_DQ}; then :; \\
	else cd ${AX_DOLLAR}(top_builddir) && ${AX_DOLLAR}(MAKE); fi
rebuild-lib:
	@if test ${AX_DQ}${AX_DOLLAR}(top_builddir)${AX_DQ} = ${AX_DQ}${AX_DOLLAR}(builddir)${AX_DQ}; then :; \\
	else cd ${AX_DOLLAR}(top_builddir) && ${AX_DOLLAR}(MAKE) clean all; fi
])

AX_ADD_AM_MACRO_STATIC([
COPY_SOURCE_TARGETS = ${AX_DOLLAR}($1:%=copy-%)
DIFF_SOURCE_TARGETS = ${AX_DOLLAR}($1:%=diff-%)
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
	@${AX_DOLLAR}(NORMAL_INSTALL)
	if test -z '${AX_DOLLAR}(moddir)'; then false; \\
	else \\
		rm -f ${AX_DOLLAR}(LIST_MODFILES); touch ${AX_DOLLAR}(LIST_MODFILES);\\
		for dir in ${AX_DOLLAR}(DESTDIR)${AX_DOLLAR}(moddir); do \\
		test -z ${AX_DOLLAR}${AX_DOLLAR}dir || ${AX_DOLLAR}(MKDIR_P) ${AX_DOLLAR}${AX_DOLLAR}dir; \\
		done; \\
		for modfile in *.${AX_DOLLAR}(FC_MODEXT); \\
		do test -e ${AX_DOLLAR}${AX_DOLLAR}modfile || continue; \\
		${AX_DOLLAR}(install_sh_DATA) -t ${AX_DOLLAR}(DESTDIR)${AX_DOLLAR}(moddir) ${AX_DOLLAR}${AX_DOLLAR}modfile; \\
		echo "${AX_DOLLAR}${AX_DOLLAR}modfile" >> ${AX_DOLLAR}(LIST_MODFILES); \\
		done; \\
	fi
endif

])

MT_ADD_RECURSIVE_AM_MACRO_STATIC([uninstall-mod],
[if INSTALL_MODULES
	if test -z '${AX_DOLLAR}(moddir)'; then false; \\
	else \\
		list=\`test -e ${AX_DOLLAR}(LIST_MODFILES) && cat ${AX_DOLLAR}(LIST_MODFILES)\`; \\
		files=\`for p in ${AX_DOLLAR}${AX_DOLLAR}list; do echo ${AX_DOLLAR}${AX_DOLLAR}p; done | sed -e 's|^.*/||'\`; \\
		dir='${AX_DOLLAR}(DESTDIR)${AX_DOLLAR}(moddir)'; ${AX_DOLLAR}(am__uninstall_files_from_dir); \\
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

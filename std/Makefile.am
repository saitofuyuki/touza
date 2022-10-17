# touza/std Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: May 27 2020
# Time-stamp: <2022/06/27 11:21:30 fuyuki Makefile.am>
#
# Copyright (C) 2020, 2021, 2022
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)
#

EXTRA_DIST	=	README-std.org
SUBDIRS	=	.

include $(top_srcdir)/aminclude_static.am

include_HEADERS	=	touza_std.h

noinst_LTLIBRARIES	=	libtouza_local.la

libtouza_local_la_SOURCES = std_prc.F90 std_utl.F90 std_log.F90 std_wsh.F90 \
							std_bld.F90 std_mwe.F90 std_fun.F90 std_env.F90 \
							std_sus.F90 std_arg.F90 std_htb.F90 \
							std.F90 $(include_HEADERS)

MOSTLYCLEANFILES	+=	fort.*

TEST_COMMON_LIBS		=	$(noinst_LTLIBRARIES)

check_PROGRAMS			=	test_std_prc test_std_utl test_std_log \
							test_std_bld test_std_mwe test_std_fun test_std_env \
							test_std_sus test_std_arg test_std_htb test_std

test_std_prc_SOURCES	=	std_prc.F90
test_std_prc_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_STD_PRC=1
test_std_prc_LDADD		=

test_std_utl_SOURCES	=	std_utl.F90
test_std_utl_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_STD_UTL=1
test_std_utl_LDADD		=	$(TEST_COMMON_LIBS)

test_std_log_SOURCES	=	std_log.F90
test_std_log_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_STD_LOG=1
test_std_log_LDADD		=	$(TEST_COMMON_LIBS)

test_std_fun_SOURCES	=	std_fun.F90
test_std_fun_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_STD_FUN=1
test_std_fun_LDADD		=	$(TEST_COMMON_LIBS)

test_std_env_SOURCES	=	std_env.F90
test_std_env_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_STD_ENV=1
test_std_env_LDADD		=	$(TEST_COMMON_LIBS)

test_std_sus_SOURCES	=	std_sus.F90
test_std_sus_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_STD_SUS=1
test_std_sus_LDADD		=	$(TEST_COMMON_LIBS)

test_std_arg_SOURCES	=	std_arg.F90
test_std_arg_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_STD_ARG=1
test_std_arg_LDADD		=	$(TEST_COMMON_LIBS)

test_std_bld_SOURCES	=	std_bld.F90
test_std_bld_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_STD_BLD=1
test_std_bld_LDADD		=	$(TEST_COMMON_LIBS)

test_std_mwe_SOURCES	=	std_mwe.F90
test_std_mwe_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_STD_MWE=1
test_std_mwe_LDADD		=	$(TEST_COMMON_LIBS)

test_std_htb_SOURCES	=	std_htb.F90
test_std_htb_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_STD_HTB=1
test_std_htb_LDADD		=	$(TEST_COMMON_LIBS)

test_std_SOURCES	=	std.F90
test_std_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_STD=1
test_std_LDADD		=	$(TEST_COMMON_LIBS)

include Makedep.inc

### Local Variables:
### mode: makefile-automake
### end:

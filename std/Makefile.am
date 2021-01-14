# touza/std Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: May 27 2020
# Time-stamp: <2021/01/13 17:39:53 fuyuki Makefile.am>
#
# Copyright (C) 2020, 2021
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)
#

EXTRA_DIST	=	README-std.org
MOSTLYCLEANFILES	=
SUBDIRS	=	.

include $(top_srcdir)/aminclude_static.am

include_HEADERS	=	touza_std.h

noinst_LTLIBRARIES	=	libtouza_local.la

libtouza_local_la_SOURCES = std_prc.F90 std_utl.F90 \
							std_env.F90 std_log.F90 \
							std_fun.F90 std_arg.F90 \
							std_mwe.F90 \
							std.F90 $(include_HEADERS)

MOSTLYCLEANFILES	+=	fort.*

TEST_COMMON_LIBS		=	$(noinst_LTLIBRARIES)

check_PROGRAMS			=	test_std_prc test_std_env test_std_log test_std_fun test_std_arg test_std_mwe test_std

test_std_env_SOURCES	=	std_env.F90
test_std_env_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_STD_ENV=1
test_std_env_LDADD		=	$(TEST_COMMON_LIBS)

test_std_log_SOURCES	=	std_log.F90
test_std_log_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_STD_LOG=1
test_std_log_LDADD		=	$(TEST_COMMON_LIBS)

test_std_fun_SOURCES	=	std_fun.F90
test_std_fun_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_STD_FUN=1
test_std_fun_LDADD		=	$(TEST_COMMON_LIBS)

test_std_prc_SOURCES	=	std_prc.F90
test_std_prc_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_STD_PRC=1
test_std_prc_LDADD		=	$(TEST_COMMON_LIBS)

test_std_arg_SOURCES	=	std_arg.F90
test_std_arg_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_STD_ARG=1
test_std_arg_LDADD		=	$(TEST_COMMON_LIBS)

test_std_mwe_SOURCES	=	std_mwe.F90
test_std_mwe_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_STD_MWE=1
test_std_mwe_LDADD		=	$(TEST_COMMON_LIBS)

test_std_SOURCES	=	std.F90
test_std_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_STD=1
test_std_LDADD		=	$(TEST_COMMON_LIBS)

### Local Variables:
### mode: makefile-automake
### end:

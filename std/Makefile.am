# touza/std Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: May 27 2020
# Time-stamp: <2023/03/19 22:47:32 fuyuki Makefile.am>
#
# Copyright (C) 2020-2023
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)
#

EXTRA_DIST = README-std.org
SUBDIRS = .

include $(top_srcdir)/aminclude_static.am

include_HEADERS = touza_std.h

noinst_LTLIBRARIES = libtouza_local.la

libtouza_local_la_SOURCES =
libtouza_local_la_SOURCES += std_prc.F90 std_utl.F90 std_log.F90 std_wsh.F90
libtouza_local_la_SOURCES += std_bld.F90 std_mwe.F90 std_fun.F90 std_env.F90
libtouza_local_la_SOURCES += std_ipc.F90 std_sus.F90 std_arg.F90 std_htb.F90
libtouza_local_la_SOURCES += std.F90 $(include_HEADERS)

MOSTLYCLEANFILES += fort.*

TEST_COMMON_LIBS = $(noinst_LTLIBRARIES)

check_PROGRAMS =

check_PROGRAMS += test_std_prc
test_std_prc_SOURCES = std_prc.F90
test_std_prc_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_STD_PRC=1
test_std_prc_LDADD =
EXTRA_test_std_prc_DEPENDENCIES = $(test_std_prc_LDADD)

check_PROGRAMS += test_std_utl
test_std_utl_SOURCES = std_utl.F90
test_std_utl_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_STD_UTL=1
test_std_utl_LDADD = $(TEST_COMMON_LIBS)
EXTRA_test_std_utl_DEPENDENCIES = $(test_std_utl_LDADD)

check_PROGRAMS += test_std_log
test_std_log_SOURCES = std_log.F90
test_std_log_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_STD_LOG=1
test_std_log_LDADD = $(TEST_COMMON_LIBS)
EXTRA_test_std_log_DEPENDENCIES = $(test_std_log_LDADD)

check_PROGRAMS += test_std_fun
test_std_fun_SOURCES = std_fun.F90
test_std_fun_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_STD_FUN=1
test_std_fun_LDADD = $(TEST_COMMON_LIBS)
EXTRA_test_std_fun_DEPENDENCIES = $(test_std_fun_LDADD)

check_PROGRAMS += test_std_env
test_std_env_SOURCES = std_env.F90
test_std_env_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_STD_ENV=1
test_std_env_LDADD = $(TEST_COMMON_LIBS)
EXTRA_test_std_env_DEPENDENCIES = $(test_std_env_LDADD)

check_PROGRAMS += test_std_sus
test_std_sus_SOURCES = std_sus.F90
test_std_sus_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_STD_SUS=9
test_std_sus_LDADD = $(TEST_COMMON_LIBS)
EXTRA_test_std_sus_DEPENDENCIES = $(test_std_sus_LDADD)

check_PROGRAMS += test_std_sus1
test_std_sus1_SOURCES = std_sus.F90
test_std_sus1_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_STD_SUS=1
test_std_sus1_LDADD = $(TEST_COMMON_LIBS)
EXTRA_test_std_sus1_DEPENDENCIES = $(test_std_sus1_LDADD)

check_PROGRAMS += test_std_sus2
test_std_sus2_SOURCES = std_sus.F90
test_std_sus2_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_STD_SUS=2
test_std_sus2_LDADD = $(TEST_COMMON_LIBS)
EXTRA_test_std_sus2_DEPENDENCIES = $(test_std_sus2_LDADD)

check_PROGRAMS += test_std_arg
test_std_arg_SOURCES = std_arg.F90
test_std_arg_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_STD_ARG=1
test_std_arg_LDADD = $(TEST_COMMON_LIBS)
EXTRA_test_std_arg_DEPENDENCIES = $(test_std_arg_LDADD)

check_PROGRAMS += test_std_bld
test_std_bld_SOURCES = std_bld.F90
test_std_bld_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_STD_BLD=1
test_std_bld_LDADD = $(TEST_COMMON_LIBS)
EXTRA_test_std_bld_DEPENDENCIES = $(test_std_bld_LDADD)

check_PROGRAMS += test_std_mwe
test_std_mwe_SOURCES = std_mwe.F90
test_std_mwe_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_STD_MWE=1
test_std_mwe_LDADD = $(TEST_COMMON_LIBS)
EXTRA_test_std_mwe_DEPENDENCIES = $(test_std_mwe_LDADD)

check_PROGRAMS += test_std_htb1
test_std_htb1_SOURCES = std_htb.F90
test_std_htb1_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_STD_HTB=1
test_std_htb1_LDADD = $(TEST_COMMON_LIBS)
EXTRA_test_std_htb1_DEPENDENCIES = $(test_std_htb1_LDADD)

check_PROGRAMS += test_std_htb2
test_std_htb2_SOURCES = $(test_std_htb1_SOURCES)
test_std_htb2_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_STD_HTB=2
test_std_htb2_LDADD = $(test_std_htb1_LDADD)
EXTRA_test_std_htb2_DEPENDENCIES = $(test_std_htb2_LDADD)

check_PROGRAMS += test_std_htb3
test_std_htb3_SOURCES = $(test_std_htb1_SOURCES)
test_std_htb3_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_STD_HTB=3
test_std_htb3_LDADD = $(test_std_htb1_LDADD)
EXTRA_test_std_htb3_DEPENDENCIES = $(test_std_htb3_LDADD)

check_PROGRAMS += test_std_ipc1
test_std_ipc1_SOURCES = std_ipc.F90
test_std_ipc1_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_STD_IPC=1
test_std_ipc1_LDADD = $(TEST_COMMON_LIBS)
EXTRA_test_std_ipc1_DEPENDENCIES = $(test_std_ipc1_LDADD)

check_PROGRAMS += test_std_ipc2
test_std_ipc2_SOURCES = $(test_std_ipc1_SOURCES)
test_std_ipc2_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_STD_IPC=2
test_std_ipc2_LDADD = $(test_std_ipc1_LDADD)
EXTRA_test_std_ipc2_DEPENDENCIES = $(test_std_ipc2_LDADD)

check_PROGRAMS += test_std
test_std_SOURCES = std.F90
test_std_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_STD=1
test_std_LDADD = $(TEST_COMMON_LIBS)
EXTRA_test_std_DEPENDENCIES = $(test_std_LDADD)

include Makedep.inc

### Local Variables:
### mode: makefile-automake
### end:

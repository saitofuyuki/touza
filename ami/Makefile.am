# touza/ami Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: May 2 2022
# Time-stamp: <2023/03/19 17:27:10 fuyuki Makefile.am>

# Copyright (C) 2022,2023
#           Japan Agency for Marine-Earth Science and Technology
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

EXTRA_DIST = README-ami.org
SUBDIRS = .

include $(top_srcdir)/aminclude_static.am

include_HEADERS = touza_ami.h

noinst_LTLIBRARIES = libtouza_local.la
libtouza_local_la_SOURCES =  ami_std.F90
libtouza_local_la_SOURCES += ami_table.F90
libtouza_local_la_SOURCES += ami_utils.F90
libtouza_local_la_SOURCES += ami_legacy.F90
libtouza_local_la_SOURCES += ami_nio.F90
libtouza_local_la_SOURCES += $(include_HEADERS)

# switch_DATA   += ami_miroc.F90
# EXTRA_libtouza_local_la_SOURCES   = ami_miroc.F90
# libtouza_local_la_SOURCES +=  $(libtouza_APPENDS)
# libtouza_APPENDS =

TEST_COMMON_LIBS =  $(noinst_LTLIBRARIES)
TEST_COMMON_LIBS += $(FC_LDADD_touza_nio) $(FC_LDADD_touza_trp)
TEST_COMMON_LIBS += $(FC_LDADD_touza_std)
# TEST_COMMON_LIBS  += $(FC_LDADD_touza_emu)

if with_nctcdf
TEST_COMMON_LIBS += $(NETCDF4_FLIBS) $(NETCDF4_LIBS)
endif

AM_FCFLAGS_MODULE += $(FC_MODULE_touza_nio)
AM_FCFLAGS_MODULE += $(FC_MODULE_touza_std)
# AM_FCFLAGS += $(FC_MODULE_touza_emu)

check_PROGRAMS = test_ami_table
test_ami_table_SOURCES  = $(TEST_COMMON_SOURCES) ami_table.F90
test_ami_table_LDADD    = $(TEST_COMMON_LIBS)
test_ami_table_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_AMI_TABLE=1
EXTRA_test_ami_table_DEPENDENCIES    = $(test_ami_table_LDADD)

check_PROGRAMS += test_ami_utils
test_ami_utils_SOURCES  = $(TEST_COMMON_SOURCES) ami_utils.F90
test_ami_utils_LDADD    = $(TEST_COMMON_LIBS)
test_ami_utils_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_AMI_UTILS=1
EXTRA_test_ami_utils_DEPENDENCIES    = $(test_ami_utils_LDADD)

check_PROGRAMS += test_ami_legacy
test_ami_legacy_SOURCES  = $(TEST_COMMON_SOURCES) ami_legacy.F90
test_ami_legacy_LDADD    = $(TEST_COMMON_LIBS)
test_ami_legacy_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_AMI_LEGACY=1
EXTRA_test_ami_legacy_DEPENDENCIES    = $(test_ami_legacy_LDADD)

check_PROGRAMS += test_ami_nio
test_ami_nio_SOURCES  = $(TEST_COMMON_SOURCES) ami_nio.F90
test_ami_nio_LDADD    = $(TEST_COMMON_LIBS)
test_ami_nio_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_AMI_NIO=1
EXTRA_test_ami_nio_DEPENDENCIES    = $(test_ami_nio_LDADD)

include Makedep.inc

### Local Variables:
### mode: makefile-automake
### end:

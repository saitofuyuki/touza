# TOUZA/Nio Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Oct 11 2021
# Time-stamp: <2025/05/23 10:58:20 fuyuki Makefile.am>

# Copyright (C) 2021-2025
#           Japan Agency for Marine-Earth Science and Technology
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

EXTRA_DIST = README-nio.org
SUBDIRS	= .

include $(top_srcdir)/aminclude_static.am

include_HEADERS = touza_nio.h touza_nio_interface.h touza_nio_param.h

noinst_LTLIBRARIES = libtouza_local.la
libtouza_local_la_SOURCES = nio_std.F90 nio_header.F90
libtouza_local_la_SOURCES += nio_record.F90 nio_sparse.F90

libtouza_local_la_SOURCES += nio_axis.F90
libtouza_local_la_SOURCES += nio_cache.F90

if BUILD_div
if with_mpi
# libtouza_local_la_SOURCES += nio_division.F90
endif
endif
if with_nctcdf
libtouza_local_la_SOURCES += nio_nctcdf.F90
endif
libtouza_local_la_SOURCES += nio_control.F90

if have_iso_c_binding
libtouza_local_la_SOURCES += nio_bindc.F90
endif

libtouza_local_la_SOURCES += nio.F90 $(include_HEADERS)
libtouza_local_la_LDFLAGS =

switch_DATA	+= nio_miroc.F90
EXTRA_libtouza_local_la_SOURCES	= nio_miroc.F90
libtouza_local_la_SOURCES +=$(libtouza_APPENDS)
libtouza_APPENDS =

if BUILD_div
AM_FCFLAGS_MODULE += $(FC_MODULE_touza_div)
endif
AM_FCFLAGS_MODULE += $(FC_MODULE_touza_std) $(FC_MODULE_touza_trp)
if BUILD_div
AM_CPPFLAGS += $(FC_INCLUDE_touza_div)
endif
AM_CPPFLAGS += $(FC_INCLUDE_touza_std) $(FC_INCLUDE_touza_trp)

if with_nctcdf
# AM_LDFLAGS = $(NETCDF4_LDFLAGS)
endif

if with_nctcdf
AM_FCFLAGS_MODULE += $(NETCDF4_FFLAGS)
endif

CLEANFILES += $(noinst_SCRIPTS)
noinst_SCRIPTS =
TESTS=

TEST_COMMON_LIBS = $(noinst_LTLIBRARIES)
if BUILD_div
TEST_COMMON_LIBS += $(FC_LDADD_touza_div)
endif
TEST_COMMON_LIBS += $(FC_LDADD_touza_trp) $(FC_LDADD_touza_std)

check_PROGRAMS = test_nio_header test_nio_record test_nio_cache
check_PROGRAMS += test_nio_sparse
if BUILD_div
check_PROGRAMS += test_nio_division
endif
check_PROGRAMS += test_nio_control
check_PROGRAMS += test_nio test_nio_miroc
if with_nctcdf
check_PROGRAMS+= test_nio_nctcdf
TEST_COMMON_LIBS += $(NETCDF4_FLIBS) $(NETCDF4_LIBS)
endif

if have_iso_c_binding
check_PROGRAMS += test_nio_bindc
endif

test_nio_header_SOURCES = $(TEST_COMMON_SOURCES) nio_header.F90
test_nio_header_LDADD = $(TEST_COMMON_LIBS)
test_nio_header_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_NIO_HEADER=1
EXTRA_test_nio_header_DEPENDENCIES = $(test_nio_header_LDADD)

test_nio_record_SOURCES = $(TEST_COMMON_SOURCES) nio_record.F90
test_nio_record_LDADD = $(TEST_COMMON_LIBS)
test_nio_record_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_NIO_RECORD=1
EXTRA_test_nio_record_DEPENDENCIES = $(test_nio_record_LDADD)

test_nio_sparse_SOURCES = $(TEST_COMMON_SOURCES) nio_sparse.F90
test_nio_sparse_LDADD = $(TEST_COMMON_LIBS)
test_nio_sparse_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_NIO_SPARSE=1
EXTRA_test_nio_sparse_DEPENDENCIES = $(test_nio_sparse_LDADD)

WTEST_LOGDIR = log
WTEST = wtest.sh
WTESTin = $(WTEST).in
noinst_SCRIPTS += $(WTEST)
EXTRA_DIST += $(WTESTin)

LOG_DRIVER = env AM_TAP_AWK='$(AWK)' $(SHELL) $(BUILD_AUX)/tap-driver.sh
LOG_DRIVER_FLAGS='--comments'

LOG_COMPILER = $(builddir)/$(WTEST)
TEST_NUM = 0
AM_LOG_FLAGS = -l $(WTEST_LOGDIR) -t $(TEST_NUM)
TESTS += record

$(TESTS):

check_targets_all = check-record

distclean-local: distclean-logdir

distclean-logdir:
	@if test -d $(WTEST_LOGDIR); then rm -rf $(WTEST_LOGDIR); else :; fi

$(check_targets_all):
	echo TESTS=`echo "$@" | sed -e 's/^check-//'` >&2
	$(MAKE) check TESTS=`echo "$@" | sed -e 's/^check-//'`

$(WTEST): $(srcdir)/$(WTESTin)
	@if sed -e 's!@srcdir@!$(srcdir)!g' \
		$(srcdir)/$(WTESTin) > $@; \
	then chmod +x $@; \
	else rm -f $@; fi

test_nio_cache_SOURCES = $(TEST_COMMON_SOURCES) nio_cache.F90
test_nio_cache_LDADD = $(TEST_COMMON_LIBS)
test_nio_cache_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_NIO_CACHE=1
EXTRA_test_nio_cache_DEPENDENCIES = $(test_nio_cache_LDADD)

test_nio_control_SOURCES = $(TEST_COMMON_SOURCES) nio_control.F90
test_nio_control_LDADD = $(TEST_COMMON_LIBS)
test_nio_control_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_NIO_CONTROL=1
EXTRA_test_nio_control_DEPENDENCIES = $(test_nio_control_LDADD)

if BUILD_div
test_nio_division_SOURCES = $(TEST_COMMON_SOURCES) nio_division.F90
test_nio_division_LDADD = $(TEST_COMMON_LIBS)
test_nio_division_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_NIO_DIVISION=1
EXTRA_test_nio_division_DEPENDENCIES = $(test_nio_division_LDADD)
endif

test_nio_SOURCES = $(TEST_COMMON_SOURCES) nio.F90
test_nio_LDADD = $(TEST_COMMON_LIBS)
test_nio_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_NIO=1
EXTRA_test_nio_LDEPENDENCIES = $(test_nio_LDADD)

CAL_SWITCH = calendar_miroc.F90
CLEANFILES += $(CAL_SWITCH)

test_nio_miroc_SOURCES = $(TEST_COMMON_SOURCES) nio_miroc.F90
nodist_test_nio_miroc_SOURCES = $(CAL_SWITCH)
test_nio_miroc_LDADD = $(FC_LDADD_touza_cal) $(TEST_COMMON_LIBS)
test_nio_miroc_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_NIO_MIROC=1 $(FC_INCLUDE_touza_cal)
test_nio_miroc_FCFLAGS = $(AM_FCFLAGS) $(FC_MODULE_touza_cal) $(FC_MODULE_touza_ppp)

test_nio_nctcdf_SOURCES = $(TEST_COMMON_SOURCES) nio_nctcdf.F90
test_nio_nctcdf_LDADD = $(TEST_COMMON_LIBS)
test_nio_nctcdf_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_NIO_NCTCDF=1
test_nio_nctcdf_FCFLAGS = $(AM_FCFLAGS)

test_nio_bindc_SOURCES = test_nio_bindc.c touza_nio_interface.h touza_nio_param.h
test_nio_bindc_LDADD = $(TEST_COMMON_LIBS) $(FCLIBS)

check_PROGRAMS += test_nio_std
test_nio_std_SOURCES = $(TEST_COMMON_SOURCES) nio_std.F90
test_nio_std_LDADD = $(TEST_COMMON_LIBS)
test_nio_std_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_NIO_STD=1

$(CAL_SWITCH):
	@cp $(srcdir)/../cal/$@ .

test_helpd	=	bin

distclean-local:
	if test -h $(test_helpd); then rm -f $(test_helpd); else true; fi

link-bin:
	@sub=$(test_helpd); bin=$(srcdir)/$${sub}; \
	if test -f $${sub}; then :; \
	elif test -e $${bin}; then \
	  ln -sf $${bin} .; \
	fi

include Makedep.inc

### Local Variables:
### mode: makefile-automake
### end:

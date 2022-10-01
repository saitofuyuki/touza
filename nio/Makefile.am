# TOUZA/Nio Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Oct 11 2021
# Time-stamp: <2022/06/27 11:20:55 fuyuki Makefile.am>

# Copyright (C) 2021, 2022
#           Japan Agency for Marine-Earth Science and Technology
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

EXTRA_DIST		=	README-nio.org
SUBDIRS	=	.

include $(top_srcdir)/aminclude_static.am

include_HEADERS	=	touza_nio.h

noinst_LTLIBRARIES = libtouza_local.la
libtouza_local_la_SOURCES = nio_std.F90 nio_header.F90 \
							nio_record.F90 nio.F90 \
							$(include_HEADERS)
switch_DATA	+= nio_miroc.F90
EXTRA_libtouza_local_la_SOURCES	= nio_miroc.F90
libtouza_local_la_SOURCES	+=	$(libtouza_APPENDS)
libtouza_APPENDS =

AM_FCFLAGS_MODULE += $(FC_MODULE_touza_std) $(FC_MODULE_touza_trp)
AM_CPPFLAGS += $(FC_INCLUDE_touza_std) $(FC_INCLUDE_touza_trp)

TEST_COMMON_LIBS	= $(noinst_LTLIBRARIES)
TEST_COMMON_LIBS	+= $(FC_LDADD_touza_std) $(FC_LDADD_touza_trp)

check_PROGRAMS	=	test_nio_header test_nio_record test_nio test_nio_miroc

test_nio_header_SOURCES		= $(TEST_COMMON_SOURCES) nio_header.F90
test_nio_header_LDADD		= $(TEST_COMMON_LIBS)
test_nio_header_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_NIO_HEADER=1

test_nio_record_SOURCES		= $(TEST_COMMON_SOURCES) nio_record.F90
test_nio_record_LDADD		= $(TEST_COMMON_LIBS)
test_nio_record_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_NIO_RECORD=1

test_nio_SOURCES	= $(TEST_COMMON_SOURCES) nio.F90
test_nio_LDADD		= $(TEST_COMMON_LIBS)
test_nio_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_NIO=1

CAL_SWITCH	=	calendar_miroc.F90
CLEANFILES	+=	$(CAL_SWITCH)

test_nio_miroc_SOURCES			= $(TEST_COMMON_SOURCES) nio_miroc.F90
nodist_test_nio_miroc_SOURCES	= $(CAL_SWITCH)
test_nio_miroc_LDADD	= $(FC_LDADD_touza_cal) $(TEST_COMMON_LIBS)
test_nio_miroc_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_NIO_MIROC=1 $(FC_INCLUDE_touza_cal)
test_nio_miroc_FCFLAGS	= $(AM_FCFLAGS) $(FC_MODULE_touza_cal)

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

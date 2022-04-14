# TOUZA/Nng Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Oct 11 2021
# Time-stamp: <2022/04/06 12:35:49 fuyuki Makefile.am>

# Copyright (C) 2021, 2022
#           Japan Agency for Marine-Earth Science and Technology
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

EXTRA_DIST		=	README-nng.org
MOSTLYCLEANFILES=
SUBDIRS	=	.

include $(top_srcdir)/aminclude_static.am

include_HEADERS	=	touza_nng.h

noinst_LTLIBRARIES = libtouza_local.la
libtouza_local_la_SOURCES = nng_std.F90 nng_header.F90 \
							nng_record.F90 nng.F90 \
							$(include_HEADERS)
switch_DATA	+= nng_miroc.F90
EXTRA_libtouza_local_la_SOURCES	= nng_miroc.F90
libtouza_local_la_SOURCES	+=	$(libtouza_APPENDS)
libtouza_APPENDS =

AM_FCFLAGS += $(FC_MODULE_touza_std) $(FC_MODULE_touza_trp)
AM_CPPFLAGS += $(FC_INCLUDE_touza_std) $(FC_INCLUDE_touza_trp)

TEST_COMMON_LIBS	= $(noinst_LTLIBRARIES)
TEST_COMMON_LIBS	+= $(FC_LDADD_touza_std) $(FC_LDADD_touza_trp)

check_PROGRAMS	=	test_nng_header test_nng_record test_nng test_nng_miroc

test_nng_header_SOURCES		= $(TEST_COMMON_SOURCES) nng_header.F90
test_nng_header_LDADD		= $(TEST_COMMON_LIBS)
test_nng_header_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_NNG_HEADER=1

test_nng_record_SOURCES		= $(TEST_COMMON_SOURCES) nng_record.F90
test_nng_record_LDADD		= $(TEST_COMMON_LIBS)
test_nng_record_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_NNG_RECORD=1

test_nng_SOURCES	= $(TEST_COMMON_SOURCES) nng.F90
test_nng_LDADD		= $(TEST_COMMON_LIBS)
test_nng_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_NNG=1

CAL_SWITCH	=	calendar_miroc.F90
CLEANFILES	=	$(CAL_SWITCH)

test_nng_miroc_SOURCES			= $(TEST_COMMON_SOURCES) nng_miroc.F90
nodist_test_nng_miroc_SOURCES	= $(CAL_SWITCH)
test_nng_miroc_LDADD	= $(FC_LDADD_touza_cal) $(TEST_COMMON_LIBS)
test_nng_miroc_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_NNG_MIROC=1 $(FC_INCLUDE_touza_cal)
test_nng_miroc_FCFLAGS	= $(AM_FCFLAGS) $(FC_MODULE_touza_cal)

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

### Local Variables:
### mode: makefile-automake
### end:

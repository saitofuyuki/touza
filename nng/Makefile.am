# TOUZA/Nng Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Oct 11 2021
# Time-stamp: <2021/11/18 14:36:50 fuyuki Makefile.am>

# Copyright (C) 2021
#           Japan Agency for Marine-Earth Science and Technology
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

EXTRA_DIST		=	README-nng.org
MOSTLYCLEANFILES=
SUBDIRS	=	.

include $(top_srcdir)/aminclude_static.am

include_HEADERS	=	touza_nng.h

noinst_LTLIBRARIES = libtouza_local.la
libtouza_local_la_SOURCES = nng_std.F90 nng_io.F90 nng_header.F90 \
							nng_record.F90 nng.F90 \
							$(include_HEADERS)
EXTRA_libtouza_local_la_SOURCES	= nng_miroc.F90
libtouza_local_la_SOURCES	+=	$(libtouza_APPENDS)
libtouza_APPENDS = nng_miroc.F90

AM_FCFLAGS = $(FC_MODULE_touza_std) $(FC_MODULE_touza_trp)
AM_CPPFLAGS += $(FC_INCLUDE_touza_std) $(FC_INCLUDE_touza_trp)

TEST_COMMON_LIBS	= $(noinst_LTLIBRARIES)
TEST_COMMON_LIBS	+= $(FC_LDADD_touza_std) $(FC_LDADD_touza_trp)

check_PROGRAMS	=	test_nng_header test_nng_io test_nng_record test_nng

test_nng_header_SOURCES		= $(TEST_COMMON_SOURCES) nng_header.F90
test_nng_header_LDADD		= $(TEST_COMMON_LIBS)
test_nng_header_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_NNG_HEADER=1

test_nng_io_SOURCES		= $(TEST_COMMON_SOURCES) nng_io.F90
test_nng_io_LDADD		= $(TEST_COMMON_LIBS)
test_nng_io_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_NNG_IO=1

test_nng_record_SOURCES		= $(TEST_COMMON_SOURCES) nng_record.F90
test_nng_record_LDADD		= $(TEST_COMMON_LIBS)
test_nng_record_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_NNG_RECORD=1

test_nng_SOURCES	= $(TEST_COMMON_SOURCES) nng.F90
test_nng_LDADD		= $(TEST_COMMON_LIBS)
test_nng_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_NNG=1

test_helpd	=	bin

distclean-local:
	@test -h $(test_helpd) && rm -f $(test_helpd)

link-bin:
	@sub=$(test_helpd); bin=$(srcdir)/$${sub}; \
	if test -f $${sub}; then :; \
	elif test -e $${bin}; then \
	  ln -sf $${bin} .; \
	fi

### Local Variables:
### mode: makefile-automake
### end:

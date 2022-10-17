# touza/ppp Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Jan 26 2022
# Time-stamp: <2022/06/27 11:20:26 fuyuki Makefile.am>

# Copyright (C) 2022
#           Japan Agency for Marine-Earth Science and Technology
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

EXTRA_DIST		=	README-ppp.org
SUBDIRS	= .

include $(top_srcdir)/aminclude_static.am

include_HEADERS	=	touza_ppp.h

noinst_LTLIBRARIES = libtouza_local.la
libtouza_local_la_SOURCES = ppp_std.F90 ppp_amng.F90 ppp_king.F90 \
                            ppp_comm.F90 ppp.F90 $(include_HEADERS)

switch_DATA	+= ppp_miroc.F90
EXTRA_libtouza_local_la_SOURCES	= ppp_miroc.F90
libtouza_local_la_SOURCES	+=	$(libtouza_APPENDS)
libtouza_APPENDS =

TEST_COMMON_LIBS	= $(noinst_LTLIBRARIES)
TEST_COMMON_LIBS	+= $(FC_LDADD_touza_emu) $(FC_LDADD_touza_std)

AM_FCFLAGS_MODULE += $(FC_MODULE_touza_std)
AM_FCFLAGS_MODULE += $(FC_MODULE_touza_emu)

check_PROGRAMS		= test_ppp test_ppp_std test_ppp_amng test_ppp_king \
                      test_ppp_comm \
                      test_ppp_miroc_a test_ppp_miroc_o test_ppp_miroc_s

test_ppp_SOURCES	= $(TEST_COMMON_SOURCES) ppp.F90
test_ppp_LDADD		= $(TEST_COMMON_LIBS)
test_ppp_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_PPP=1

test_ppp_std_SOURCES	= $(TEST_COMMON_SOURCES) ppp_std.F90
test_ppp_std_LDADD		= $(FC_LDADD_touza_std)
test_ppp_std_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_PPP_STD=1

test_ppp_amng_SOURCES	= $(TEST_COMMON_SOURCES) ppp_amng.F90
test_ppp_amng_LDADD		= $(TEST_COMMON_LIBS)
test_ppp_amng_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_PPP_AMNG=1

test_ppp_king_SOURCES	= $(TEST_COMMON_SOURCES) ppp_king.F90
test_ppp_king_LDADD		= $(TEST_COMMON_LIBS)
test_ppp_king_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_PPP_KING=1

test_ppp_comm_SOURCES	= $(TEST_COMMON_SOURCES) ppp_comm.F90
test_ppp_comm_LDADD		= $(TEST_COMMON_LIBS)
test_ppp_comm_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_PPP_COMM=1

test_ppp_miroc_a_SOURCES	= $(TEST_COMMON_SOURCES) ppp_miroc.F90
test_ppp_miroc_a_LDADD		= $(TEST_COMMON_LIBS)
test_ppp_miroc_a_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_PPP_MIROC=1

test_ppp_miroc_o_SOURCES	= $(test_ppp_miroc_a_SOURCES)
test_ppp_miroc_o_LDADD		= $(test_ppp_miroc_a_LDADD)
test_ppp_miroc_o_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_PPP_MIROC=2

test_ppp_miroc_s_SOURCES	= $(test_ppp_miroc_a_SOURCES)
test_ppp_miroc_s_LDADD		= $(test_ppp_miroc_a_LDADD)
test_ppp_miroc_s_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_PPP_MIROC=3

include Makedep.inc

### Local Variables:
### mode: makefile-automake
### end:

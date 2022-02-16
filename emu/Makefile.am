# touza/emu Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Jun 15 2020
# Time-stamp: <2022/02/12 10:22:43 fuyuki Makefile.am>

# Copyright (C) 2020, 2021, 2022
#           Japan Agency for Marine-Earth Science and Technology
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

EXTRA_DIST		=	README-emu.org
MOSTLYCLEANFILES=
SUBDIRS	=	.

include $(top_srcdir)/aminclude_static.am

include_HEADERS	=	touza_emu.h

noinst_LTLIBRARIES = libtouza_local.la
libtouza_local_la_SOURCES = emu_usi.F90 emu_rpx.F90 emu.F90 $(include_HEADERS)
EXTRA_libtouza_local_la_SOURCES = emu_msg.F90

AM_FCFLAGS += $(FC_MODULE_touza_std)
AM_CPPFLAGS += $(FC_INCLUDE_touza_std)

TEST_COMMON_LIBS	=  $(noinst_LTLIBRARIES)
TEST_COMMON_LIBS	+= $(FC_LDADD_touza_std)

check_PROGRAMS			=	 test_emu_msg test_emu_usi

test_emu_usi_SOURCES	=	emu_usi.F90
test_emu_usi_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_EMU_USI=1
test_emu_usi_LDADD		=	$(TEST_COMMON_LIBS)

test_emu_msg_SOURCES	=	emu_msg.F90
test_emu_msg_CPPFLAGS	=	$(AM_CPPFLAGS) -DTEST_EMU_MSG=1
test_emu_msg_LDADD		=	$(TEST_COMMON_LIBS)

### Local Variables:
### mode: makefile-automake
### end:
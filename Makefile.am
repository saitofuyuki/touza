# touza Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Jun 5 2020
# Time-stamp: <2021/01/07 15:25:51 fuyuki Makefile.am>

# Copyright (C) 2020, 2021
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

ACLOCAL_AMFLAGS	=	-I m4c
MOSTLYCLEANFILES	=
EXTRA_DIST	=	autogen.sh README-touza.org

include $(top_srcdir)/aminclude_static.am

SUBDIRS 			= std
libtouza_la_LIBADD	= std/$(local_name)
AM_FCFLAGS			= $(FC_MODULE_touza_std)

if BUILD_cal
SUBDIRS 			+= cal
libtouza_la_LIBADD	+= cal/$(local_name)
AM_FCFLAGS			+= $(FC_MODULE_touza_cal)
endif

# if BUILD_emu
# SUBDIRS 			+= emu
# libtouza_la_LIBADD	+= emu/$(local_name)
# AM_FCFLAGS			+= $(FC_MODULE_touza_emu)
# endif

# if BUILD_div
# SUBDIRS 			+= div
# libtouza_la_LIBADD	+= div/$(local_name)
# AM_FCFLAGS			+= $(FC_MODULE_touza_div)
# endif

include_HEADERS		= touza.h $(CONFIG_HEADER)
lib_LTLIBRARIES 	= libtouza.la
libtouza_la_SOURCES = touza.F90 touza.h $(CONFIG_HEADER)

local_name			= libtouza_local.la

check_PROGRAMS		= test_touza
test_touza_SOURCES	= touza.F90
test_touza_LDADD	= $(lib_LTLIBRARIES)
test_touza_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_TOUZA=1

### Local Variables:
### mode: makefile-automake
### end:
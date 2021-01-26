# touza Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Jun 5 2020
# Time-stamp: <2021/01/22 21:03:08 fuyuki Makefile.am>

# Copyright (C) 2020, 2021
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

ACLOCAL_AMFLAGS	=	-I m4c
MOSTLYCLEANFILES	=
EXTRA_DIST	=	autogen.sh README-touza.org
include $(top_srcdir)/aminclude_static.am

nodist_bin_SCRIPTS 	= makeshift
CLEANFILES			= $(nodist_bin_SCRIPTS)

SUBDIRS 					= std
lib@TOUZA_NAME@_la_LIBADD	= std/$(local_name)
AM_FCFLAGS					= $(FC_MODULE_touza_std)

if BUILD_cal
SUBDIRS 					+= cal
lib@TOUZA_NAME@_la_LIBADD	+= cal/$(local_name)
AM_FCFLAGS					+= $(FC_MODULE_touza_cal)
endif

# if BUILD_emu
# SUBDIRS 					+= emu
# lib@TOUZA_NAME@_la_LIBADD	+= emu/$(local_name)
# AM_FCFLAGS					+= $(FC_MODULE_touza_emu)
# endif

# if BUILD_div
# SUBDIRS 					+= div
# lib@TOUZA_NAME@_la_LIBADD	+= div/$(local_name)
# AM_FCFLAGS					+= $(FC_MODULE_touza_div)
# endif

SUBDIRS	+=	m4c

include_HEADERS		= touza.h $(CONFIG_HEADER)
lib_LTLIBRARIES 	= lib@TOUZA_NAME@.la
lib@TOUZA_NAME@_la_SOURCES = touza.F90 touza.h $(CONFIG_HEADER)

local_name			= libtouza_local.la

check_PROGRAMS		= test_touza
test_touza_SOURCES	= touza.F90
test_touza_LDADD	= $(lib_LTLIBRARIES)
test_touza_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_TOUZA=1

if NEVER
EXTRA_PROGRAMS = dummy_never_built
nodist_dummy_never_built_SOURCES = dummy_never_built.c
endif

### Local Variables:
### mode: makefile-automake
### end:

# touza Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Jun 5 2020
# Time-stamp: <2024/07/24 15:47:34 fuyuki Makefile.am>

# Copyright (C) 2020-2024
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

ACLOCAL_AMFLAGS	=	-I m4c
EXTRA_DIST	=	autogen.sh README-touza.org LICENSE NOTICE
include $(top_srcdir)/aminclude_static.am

include_HEADERS		= touza.h $(CONFIG_HEADER)
lib_LTLIBRARIES 	= lib@TOUZA_NAME@.la
lib@TOUZA_NAME@_la_SOURCES = touza.F90 touza.h $(CONFIG_HEADER)
lib@TOUZA_NAME@_la_LDFLAGS =
lib@TOUZA_NAME@_la_LDFLAGS += $(NETCDF4_FLIBS) $(NETCDF4_LIBS)

noinst_SCRIPTS 	= makeshift
CLEANFILES += $(noinst_bin_SCRIPTS)

SUBDIRS 				  = std
lib@TOUZA_NAME@_la_LIBADD = std/$(local_name)
AM_FCFLAGS_MODULE		  += $(FC_MODULE_touza_std)

if BUILD_emu
SUBDIRS 				  += emu
lib@TOUZA_NAME@_la_LIBADD += emu/$(local_name)
AM_FCFLAGS_MODULE		  += $(FC_MODULE_touza_emu)
endif

if BUILD_cal
SUBDIRS 				  += cal
lib@TOUZA_NAME@_la_LIBADD += cal/$(local_name)
AM_FCFLAGS_MODULE		  += $(FC_MODULE_touza_cal)
endif

# if BUILD_div
# if BUILD_WITH_MPI
# SUBDIRS 				  += div
# lib@TOUZA_NAME@_la_LIBADD += div/$(local_name)
# AM_FCFLAGS_MODULE		  += $(FC_MODULE_touza_div)
# endif
# endif

if BUILD_trp
SUBDIRS 				  += trp
lib@TOUZA_NAME@_la_LIBADD += trp/$(local_name)
AM_FCFLAGS_MODULE		  += $(FC_MODULE_touza_trp)
endif

if BUILD_nio
SUBDIRS 				  += nio
lib@TOUZA_NAME@_la_LIBADD += nio/$(local_name)
AM_FCFLAGS_MODULE		  += $(FC_MODULE_touza_nio)
endif

if BUILD_ppp
SUBDIRS 				  += ppp
lib@TOUZA_NAME@_la_LIBADD += ppp/$(local_name)
AM_FCFLAGS_MODULE         += $(FC_MODULE_touza_ppp)
endif

if BUILD_ami
SUBDIRS 				  += ami
lib@TOUZA_NAME@_la_LIBADD += ami/$(local_name)
AM_FCFLAGS_MODULE         += $(FC_MODULE_touza_ami)
endif

if BUILD_zbt
SUBDIRS += zbt
endif

pkgconfigdir	=	$(libdir)/pkgconfig
pkgconfig_DATA	=	@TOUZA_NAME@.pc

SUBDIRS	+=	m4c

SUBDIRS	+=	. $(subdirs_extra)

local_name			= libtouza_local.la

check_PROGRAMS		= test_touza
test_touza_SOURCES	= touza.F90
test_touza_LDADD	= $(lib_LTLIBRARIES)
test_touza_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_TOUZA=1

if NEVER
EXTRA_PROGRAMS = dummy_never_built
nodist_dummy_never_built_SOURCES = dummy_never_built.c
endif

EXTRA_DIST += $(AUTOREVISION_cache) $(AUTOREVISION_h)

AUTOREVISION_h = autorevision.h
AUTOREVISION_cache = autorevision.cache

AM_DISTCHECK_CONFIGURE_FLAGS = --with-build-jmz --enable-shared

BUILT_SOURCES = autorevision-cache $(AUTOREVISION_h)

autorevision: $(BUILT_SOURCES)

$(AUTOREVISION_h): $(top_srcdir)/$(AUTOREVISION_cache)
	@if test x"$(AUTOREVISION)" = x:; then \
		echo "skip to run autorevision" >&2; touch $@; \
	else \
		$(AUTOREVISION) -f -t h -o $(top_srcdir)/$(AUTOREVISION_cache) | sed -e "s/\"/'/g" -e "s/''$$/' '/" > $@; \
	fi

# only update when top_srcdir is the top-level directory in git work-tree
autorevision-cache:
if autorevision
	@if cd $(top_srcdir); then \
		top=`git rev-parse --show-cdup` || exit 0; \
		if test x$${top} != x; then :; else \
			$(AUTOREVISION) -t h -o $(AUTOREVISION_cache) > /dev/null; \
		fi; fi
else
	@:
endif

EXPORT=

export: $(AUTOREVISION_h)
	@if test x"$(EXPORT)" = x; then \
		echo "EXPORT must be set as destination" >&2; exit 1; \
	elif test -d "$(EXPORT)"; then \
		echo "$(EXPORT) exists, aborts." >&2; exit 1; \
	else \
		export=`$(REALPATH) --relative-to=$(top_builddir) $(EXPORT)` || exit $$?; \
		$(MAKE) distdir=$${export} distdir; \
	fi

include Makedep.inc

### Local Variables:
### mode: makefile-automake
### end:

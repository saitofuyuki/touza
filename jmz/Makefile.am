# touza/jmz Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Nov 21 2021
# Time-stamp: <2022/08/20 17:47:13 fuyuki Makefile.am>

# Copyright (C) 2021,2022
#           Japan Agency for Marine-Earth Science and Technology
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

ACLOCAL_AMFLAGS = -I ../m4c

AM_FCFLAGS += $(AM_FCFLAGS_MODULE)
AM_FCFLAGS_MODULE = $(DEBUG) $(TOUZA_CFLAGS)
# AM_LDFLAGS = $(TOUZA_LDFLAGS)
LDADD = $(TOUZA_LIBS) libjmz_local.la

bin_PROGRAMS = jmzconv jmzcmp jmzmath
bin_SCRIPTS = jmzdepf90
noinst_LTLIBRARIES	=	libjmz_local.la

libjmz_local_la_SOURCES = jmzlib.F90


jmzconv_SOURCES = jmzconv.F90 jmz.h
# jmzconv_DEPENDENCIES  =   $(TOUZA_LIBS)
jmzcmp_SOURCES = jmzcmp.F90 jmz.h
# jmzcmp_DEPENDENCIES   =   $(TOUZA_LIBS)
jmzmath_SOURCES = jmzmath_oprs.F90 jmzmath.F90 jmz.h

python_PYTHON = jmznml.py

jmzdepf90: jmzdepf90.sh.in
	sed -e 's,[@]ZSH[@],$(ZSH),g' < $< >$@
	chmod +x $@

EXTRA_DIST = jmzdepf90.sh.in
MOSTLYCLEANFILES =
CLEANFILES = $(bin_SCRIPTS)
SUBDIRS = .

install-mod:
	:

### Local Variables:
### mode: makefile-automake
### end:

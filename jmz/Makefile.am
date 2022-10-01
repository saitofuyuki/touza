# touza/jmz Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Nov 21 2021
# Time-stamp: <2022/09/21 15:17:19 fuyuki Makefile.am>

# Copyright (C) 2021,2022
#           Japan Agency for Marine-Earth Science and Technology
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

ACLOCAL_AMFLAGS = -I ../m4c

AM_FCFLAGS += $(AM_FCFLAGS_MODULE)
AM_FCFLAGS_MODULE = $(DEBUG) $(TOUZA_CFLAGS)
# AM_LDFLAGS = $(TOUZA_LDFLAGS)
LDADD = $(TOUZA_LIBS) libjmz_local.la

bin_PROGRAMS = chak jmzconv jmzcmp
bin_SCRIPTS = jmzdepf90
noinst_PROGRAMS = jmzex
noinst_SCRIPTS = genx.sh

noinst_LTLIBRARIES	=	libjmz_local.la

libjmz_local_la_SOURCES = jmzlib.F90

jmzex_SOURCES = jmzex.F90 jmz.h

jmzconv_SOURCES = jmzconv.F90 jmz.h
# jmzconv_DEPENDENCIES  =   $(TOUZA_LIBS)
jmzcmp_SOURCES = jmzcmp.F90 jmz.h
# jmzcmp_DEPENDENCIES   =   $(TOUZA_LIBS)
chak_SOURCES = chak.F90 jmz.h

python_PYTHON = jmznml.py

# Following dependencies are necesary, as workaround
$(jmzconv_OBJECTS): $(noinst_LTLIBRARIES)
$(jmzcmp_OBJECTS): $(noinst_LTLIBRARIES)
$(chak_OBJECTS): $(noinst_LTLIBRARIES)

jmzdepf90: jmzdepf90.sh.in
	sed -e 's,[@]ZSH[@],$(ZSH),g' < $< >$@
	chmod +x $@

genx.sh: genx.sh.in
	sed -e 's,[@]ZSH[@],$(ZSH),g' < $< >$@
	chmod +x $@

EXTRA_DIST = jmzdepf90.sh.in genx.sh.in
MOSTLYCLEANFILES =
CLEANFILES = $(bin_SCRIPTS) $(noinst_SCRIPTS)
SUBDIRS = .

install-mod:
	:

### Local Variables:
### mode: makefile-automake
### end:

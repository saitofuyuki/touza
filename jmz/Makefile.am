# touza/jmz Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Nov 21 2021
# Time-stamp: <2022/10/17 10:20:52 fuyuki Makefile.am>

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
noinst_SCRIPTS = genx.sh genx2.sh

noinst_LTLIBRARIES	=	libjmz_local.la

libjmz_local_la_SOURCES = jmzlib.F90

jmzex_SOURCES = jmzex.F90 jmz.h

jmzconv_SOURCES = jmzconv.F90 jmz.h
# jmzconv_DEPENDENCIES  =   $(TOUZA_LIBS)
jmzcmp_SOURCES = jmzcmp.F90 jmz.h
# jmzcmp_DEPENDENCIES   =   $(TOUZA_LIBS)
chak_SOURCES = chak_lib.F90 chak.F90 jmz.h
chak_to_include = chak_decl.F90 chak_reg.F90

python_PYTHON = jmznml.py

# Following dependencies are necesary, as workaround
$(jmzconv_OBJECTS): $(noinst_LTLIBRARIES)
$(jmzcmp_OBJECTS): $(noinst_LTLIBRARIES)
$(chak_OBJECTS): $(noinst_LTLIBRARIES) $(chak_to_include)
$(chak_lib_OBJECTS): $(noinst_LTLIBRARIES)

jmzdepf90: jmzdepf90.sh.in
	sed -e 's,[@]ZSH[@],$(ZSH),g' < $< >$@
	chmod +x $@

genx.sh: genx.sh.in
	sed -e 's,[@]ZSH[@],$(ZSH),g' < $< >$@
	chmod +x $@

genx2.sh: genx2.sh.in
	sed -e 's,[@]ZSH[@],$(ZSH),g' < $< >$@
	chmod +x $@

EXTRA_DIST = jmzdepf90.sh.in genx.sh.in genx2.sh.in $(chak_to_include)

MOSTLYCLEANFILES =
CLEANFILES = $(bin_SCRIPTS) $(noinst_SCRIPTS)
SUBDIRS = .

install-mod:
	:

### Local Variables:
### mode: makefile-automake
### end:

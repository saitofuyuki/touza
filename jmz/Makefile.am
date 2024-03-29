# touza/jmz Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Nov 21 2021
# Time-stamp: <2023/06/30 07:19:20 fuyuki Makefile.am>

# Copyright (C) 2021,2022,2023
#           Japan Agency for Marine-Earth Science and Technology
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

ACLOCAL_AMFLAGS = -I ../m4c
EXTRA_DIST =

AM_FCFLAGS += $(AM_FCFLAGS_MODULE)
AM_FCFLAGS_MODULE = $(DEBUG) $(TOUZA_CFLAGS)
AM_LDFLAGS = $(TOUZA_LDFLAGS)
LDADD = libjmz_local.la $(TOUZA_LIBS)

noinst_SCRIPTS = jmzdepf90
noinst_PROGRAMS = $(JMZ_EXTRA)
EXTRA_PROGRAMS = jmzcmp

noinst_LTLIBRARIES	=	libjmz_local.la
libjmz_local_la_SOURCES = jmzlib.F90 chak_lib.F90 chak_opr.F90 chak_file.F90

jmzcmp_SOURCES = jmzcmp.F90 jmz.h

TOUZA_LIBS	=	$(TOUZA_LIBRARY) @TOUZA_LIBS@
TOUZA_LIBRARY = @TOUZA_LIBRARY@

bin_PROGRAMS = chak
chak_SOURCES = chak.F90 jmz.h
chak_to_include = chak_decl.F90 chak_reg.F90
EXTRA_chak_DEPENDENCIES = $(TOUZA_LIBRARY)

noinst_SCRIPTS += demo-chak.sh
noinst_SCRIPTS += ocheck.sh
EXTRA_DIST += ocheck.sh.in

noinst_PYTHON = jmznml.py

TEST_COMMON_LIBS = $(noinst_LTLIBRARIES)
check_PROGRAMS = test_chak_file

test_chak_file_SOURCES = chak_file.F90
test_chak_file_CPPFLAGS	= $(AM_CPPFLAGS) -DTEST_CHAK_FILE=1
test_chak_file_LDADD = $(TEST_COMMON_LIBS) $(LDADD)

# Following dependencies are necesary, as workaround
$(jmzcmp_OBJECTS): $(noinst_LTLIBRARIES)
$(chak_OBJECTS): $(noinst_LTLIBRARIES)
# $(chak_lib_OBJECTS): $(noinst_LTLIBRARIES)
chak_opr.lo $(chak_opr_OBJECTS): $(chak_lib_OBJECTS) $(chak_to_include)

jmzdepf90: jmzdepf90.sh.in
	sed -e 's,[@]ZSH[@],$(ZSH),g' < $< >$@
	chmod +x $@

demo-chak.sh: demo-chak.sh.in
	@sed -e 's,[@]ZSH[@],$(ZSH),g' \
         -e 's,[@]srcdir[@],$(srcdir),g' \
		$< >$@; chmod +x $@

ocheck.sh: ocheck.sh.in
	@sed -e 's,[@]CHAK[@],$(abs_builddir)/chak,g' \
		 -e 's,[@]CHAK_OPTS[@],$(CHAK_OPTS),g' < $< >$@; chmod +x $@

genx2.sh: genx2.sh.in
	if test -e $<; then \
		sed -e 's,[@]ZSH[@],$(ZSH),g' < $< >$@; \
		chmod +x $@; \
	fi

EXTRA_DIST += jmzdepf90.sh.in demo-chak.sh.in $(chak_to_include)
EXTRA_DIST += README-jmz.org

MOSTLYCLEANFILES =
CLEANFILES = $(bin_SCRIPTS) $(noinst_SCRIPTS)
SUBDIRS = .
MOSTLYCLEANFILES += *.$(FC_MODEXT)

uninstall-mod:
	@:
install-mod:
	@:

lib:
	@if test "$(TOUZA_top_builddir)"; \
	then cd $(TOUZA_top_builddir) && $(MAKE); \
	else echo "Need to set TOUZA_top_builddir" >&2; exit 1; fi

rebuild-lib:
	@if test "$(TOUZA_top_builddir)"; \
	then cd $(TOUZA_top_builddir) && $(MAKE) clean all; \
	else echo "Need to set TOUZA_top_builddir" >&2; exit 1; fi

### Local Variables:
### mode: makefile-automake
### end:

# TOUZA/Trp Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Feb 26 2021
# Time-stamp: <2023/03/18 17:48:48 fuyuki Makefile.am>

# Copyright (C) 2021,2022,2023
#           Japan Agency for Marine-Earth Science and Technology
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

EXTRA_DIST = README-trp.org
SUBDIRS = .

include $(top_srcdir)/aminclude_static.am

include_HEADERS = touza_trp.h

noinst_LTLIBRARIES = libtouza_local.la
libtouza_local_la_SOURCES = trapiche_std.F90
libtouza_local_la_SOURCES += trapiche_pack.F90 trapiche_float.F90 trapiche_ctl.F90
if BUILD_WITH_MPI
libtouza_local_la_SOURCES += trapiche_transfer.F90
endif
libtouza_local_la_SOURCES += trapiche.F90 $(include_HEADERS)

AM_FCFLAGS_MODULE += $(FC_MODULE_touza_std)
AM_CPPFLAGS += $(FC_INCLUDE_touza_std)

TEST_COMMON_LIBS = $(noinst_LTLIBRARIES)
TEST_COMMON_LIBS += $(FC_LDADD_touza_std)
# TEST_COMMON_SOURCES = div_test.F90

test_helpd = bin

UINTPACK = uintpack.F
MIROCH = miroc.h
CLEANFILES += $(MIROCH)

$(UINTPACK): $(MIROCH)
	@if test -f $(builddir)/$(UINTPACK); then :; else \
		orgsrc=$${GCMDIR}/src/util/$(UINTPACK); \
		if test -f "$${orgsrc}"; then \
			echo "copy $${orgsrc}"; cp $${orgsrc} $(builddir)/$@; \
		else \
			echo "Please manually install MIROC/src/util/$(UINTPACK) in $(abs_builddir)." >&2; exit 1; \
		fi; fi

$(MIROCH):
	touch $@

check_PROGRAMS =
check_PROGRAMS += test_trapiche_std

test_trapiche_std_SOURCES = $(TEST_COMMON_SOURCES) trapiche_std.F90
test_trapiche_std_LDADD = $(FC_LDADD_touza_std)
test_trapiche_std_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_TRAPICHE_STD=1
EXTRA_test_trapiche_std_DEPENDENCIES = $(test_trapiche_std_LDADD)

check_PROGRAMS += test_trapiche_pack_32 test_trapiche_pack_64
test_trapiche_pack_32_SOURCES = $(TEST_COMMON_SOURCES) trapiche_pack.F90
test_trapiche_pack_32_LDADD = $(TEST_COMMON_LIBS)
test_trapiche_pack_32_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_TRAPICHE_PACK=32
EXTRA_test_trapiche_pack_32_aDEPENDENCIES = $(test_trapiche_pack_32_LDADD)

test_trapiche_pack_64_SOURCES = $(test_trapiche_pack_32_SOURCES)
test_trapiche_pack_64_LDADD = $(test_trapiche_pack_32_LDADD)
test_trapiche_pack_64_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_TRAPICHE_PACK=64
EXTRA_test_trapiche_pack_64_DEPENDENCIES = $(test_trapiche_pack_64_LDADD)

check_PROGRAMS += test_trapiche_float test_trapiche_ctl

test_trapiche_ctl_SOURCES = $(TEST_COMMON_SOURCES) trapiche_ctl.F90
test_trapiche_ctl_LDADD = $(TEST_COMMON_LIBS)
test_trapiche_ctl_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_TRAPICHE_CTL=1
EXTRA_test_trapiche_ctl_DEPENDENCIES = $(test_trapiche_ctl_LDADD)

test_trapiche_float_SOURCES = $(TEST_COMMON_SOURCES) trapiche_float.F90
test_trapiche_float_LDADD = $(TEST_COMMON_LIBS)
test_trapiche_float_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_TRAPICHE_FLOAT=1
EXTRA_test_trapiche_float_DEPENDENCIES = $(test_trapiche_float_LDADD)

if MAINTAINER_MODE
check_PROGRAMS += test_trapiche_pack_uip
endif
test_trapiche_pack_uip_SOURCES = $(TEST_COMMON_SOURCES) trapiche_pack.F90
test_trapiche_pack_uip_LDADD = $(TEST_COMMON_LIBS)
test_trapiche_pack_uip_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_TRAPICHE_PACK=32 -DTEST_UINTPACK=1
EXTRA_test_trapiche_pack_uip_DEPENDENCIES = $(test_trapiche_pack_uip_LDADD)
nodist_test_trapiche_pack_uip_SOURCES = $(UINTPACK)

if BUILD_WITH_MPI
check_PROGRAMS += test_trapiche_transfer
endif
test_trapiche_transfer_SOURCES = $(TEST_COMMON_SOURCES) trapiche_transfer.F90
test_trapiche_transfer_LDADD = $(TEST_COMMON_LIBS)
test_trapiche_transfer_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_TRAPICHE_TRANSFER=1
EXTRA_test_trapiche_transfer_DEPENDENCIES = $(test_trapiche_transfer_LDADD)

check_PROGRAMS += test_trapiche
test_trapiche_SOURCES = $(TEST_COMMON_SOURCES) trapiche.F90
test_trapiche_LDADD = $(TEST_COMMON_LIBS)
test_trapiche_CPPFLAGS = $(AM_CPPFLAGS) -DTEST_TRAPICHE=1
EXTRA_test_trapiche_DEPENDENCIES = $(test_trapiche_LDADD)

distclean-local:
	if test -h $(test_helpd); then rm -f $(test_helpd); else true; fi

link-bin:
	@sub=$(test_helpd); bin=$(srcdir)/$${sub}; \
	if test -f $${sub}; then :; \
	elif test -e $${bin}; then \
	  ln -sf $${bin} .; \
	fi

include Makedep.inc

### Local Variables:
### mode: makefile-automake
### end:

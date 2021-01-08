# touza/calendar Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: May 27 2020
# Time-stamp: <2021/01/07 12:17:13 fuyuki Makefile.am>

# Copyright (C) 2020, 2021
#           Japan Agency for Marine-Earth Science and Technology
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

EXTRA_DIST		=	README-cal.org
MOSTLYCLEANFILES=
SUBDIRS	=	.

include $(top_srcdir)/aminclude_static.am

include_HEADERS	=	touza_cal.h

noinst_LTLIBRARIES = libtouza_local.la
libtouza_local_la_SOURCES = calendar_primitive.F90 calendar_core.F90 \
	calendar.F90 calendar_miroc.F90 \
	calendar_matsiro.F90 calendar_ils.F90 $(include_HEADERS)

AM_FCFLAGS = $(FC_MODULE_touza_std)

LOG_DRIVER = env AM_TAP_AWK='$(AWK)' $(SHELL) $(BUILD_AUX)/tap-driver.sh
LOG_DRIVER_FLAGS='--comments'

noinst_SCRIPTS	=	$(WTC)
CLEANFILES		=	$(noinst_SCRIPTS)

WTC_LOGDIR	=	log
WTC		=	wtc.sh
WTCin 	=	$(WTC).in

EXTRA_DIST+=$(WTCin)

$(WTC): $(srcdir)/$(WTCin)
	if sed -e 's!@XNEW@!$(builddir)/test_calendar_comp!g' \
			-e 's!@XOLD@!$(builddir)/test_calendar_trad!g' \
			-e 's!@XMAT@!$(builddir)/test_calendar_matsiro!g' \
			-e 's!@XILS@!$(builddir)/test_calendar_ils!g' $(srcdir)/$(WTCin) > $@; \
		then chmod +x $@; \
		else rm -f $@; fi

LOG_COMPILER	=	$(builddir)/$(WTC)
TEST_NUM		=	0
AM_LOG_FLAGS	=	-l $(WTC_LOGDIR) -t $(TEST_NUM)

TESTS_miroc		=	auto auto.p grego_i grego_i.p grego_l grego_l.p ideal ideal.p
TESTS_matsiro	=	matsiro.grego matsiro.365
TESTS_ils		=	ils.grego     ils.noleap  ils.30360
TESTS			=	$(TESTS_miroc) $(TESTS_matsiro) $(TESTS_ils)
$(TESTS):

check_targets_miroc	=	check-auto check-auto.p check-grego_i check-grego_i.p \
						check-grego_l check-grego_l.p check-ideal check-ideal.p

check_targets_matsiro	=	check-matsiro.grego check-matsiro.365

check_targets_ils		=	check-ils.grego check-ils.noleap check-ils.30360

check_targets_all	=	$(check_targets_miroc) $(check_targets_matsiro) $(check_targets_ils)

check-miroc:
	$(MAKE) check TESTS="$(TESTS_miroc)"

check-matsiro:
	$(MAKE) check TESTS="$(TESTS_matsiro)"

check-ils:
	$(MAKE) check TESTS="$(TESTS_ils)"

$(check_targets_all):
	$(MAKE) check TESTS=`echo "$@" | sed -e 's/^check-//'`

TEST_COMMON_SRCS	= test_calendar.F90 $(include_HEADERS)
TEST_COMMON_LIBS	= $(noinst_LTLIBRARIES)
TEST_COMMON_LIBS	+= $(FC_LDADD_touza_std)
UCALN	=	ucaln.F

$(UCALN):
	@echo "Please manually install MIROC/src/util/$(UCALN) in $(abs_builddir)." >&2; exit 1

check_PROGRAMS	=	test_calendar_comp test_calendar_trad \
					test_calendar_matsiro test_calendar_ils

test_calendar_comp_SOURCES	= $(TEST_COMMON_SRCS)
test_calendar_comp_LDADD	= $(TEST_COMMON_LIBS)

test_calendar_trad_SOURCES			= $(TEST_COMMON_SRCS)
nodist_test_calendar_trad_SOURCES	= $(UCALN)
test_calendar_trad_LDADD			= $(TEST_COMMON_LIBS)
test_calendar_trad_CPPFLAGS			= -DOPT_USE_BASE_UCALN=1

test_calendar_matsiro_SOURCES	= calendar_matsiro.F90
test_calendar_matsiro_LDADD		= $(TEST_COMMON_LIBS)
test_calendar_matsiro_CPPFLAGS	= -DTEST_CALENDAR_MATSIRO=1

test_calendar_ils_SOURCES	= calendar_ils.F90
test_calendar_ils_LDADD		= $(TEST_COMMON_LIBS)
test_calendar_ils_CPPFLAGS	= -DTEST_CALENDAR_ILS=1

### Local Variables:
### mode: makefile-automake
### end:

# touza/zbt/tests Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Jul 24 2024
# Time-stamp: <2025/01/22 18:06:20 fuyuki Makefile.am>

# Copyright (C) 2024, 2025
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

EXTRA_DIST = gen_demo.sh 

noinst_PYTHON =
noinst_PYTHON += test_calendar.py test_curve.py

copy_sources = $(noinst_PYTHON)
COPY_SOURCES_TARGETS = $(copy_sources:%=copy-%)

# dummy
all:

install-mod:

uninstall-mod:

copy-sources: $(COPY_SOURCES_TARGETS)

$(COPY_SOURCES_TARGETS):
	@target=`echo "$@" | sed -e 's/^copy-//'`;\
	if test -e $(builddir)/$${target}; then \
		echo "exists $${target}."; \
	else \
		echo "clone $(srcdir)/$${target}"; \
		cp $(srcdir)/$${target} $(builddir)/$${target} || exit $$?; \
	fi

link-sources: link-sources-am

clean-link-sources: clean-link-sources-am

link-sources-am:
	@for target in $(COPY_SOURCES_TARGETS); do \
		target=`echo "$$target" | sed -e 's/^copy-//'`; \
		if test -e $(builddir)/$${target}; then \
			echo "exists $${target}."; \
		else \
			echo "symbolic link $(srcdir)/$${target}"; \
			$(LN_S) $(srcdir)/$${target} $(builddir)/$${target} || exit $$?; \
		fi; \
	done

clean-link-sources-am:
	@for target in $(COPY_SOURCES_TARGETS); do \
		target=`echo "$$target" | sed -e 's/^copy-//'`; \
		if test -h $(builddir)/$${target}; then \
			echo "remove symbolic link $${target}."; \
			rm $(builddir)/$${target} || exit $$?; \
		fi; \
	done

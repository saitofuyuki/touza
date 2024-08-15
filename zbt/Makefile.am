# touza/zbt Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Jul 24 2024
# Time-stamp: <2024/08/15 11:09:34 fuyuki Makefile.am>

# Copyright (C) 2024
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

SUBDIRS = zbt tests

EXTRA_DIST =
EXTRA_DIST += README-zbt.org MANIFEST.in pyproject.toml setup.py
EXTRA_DIST += param.sh

copy_sources = $(EXTRA_DIST)
# dummy
all:

install-mod:

uninstall-mod:

python-build-prepare: copy-sources
	@for d in $(SUBDIRS); \
		do ( cd $$d; $(MAKE) copy-sources ); done

python-build: python-build-prepare
	$(PYTHON) -m build

copy-sources: $(COPY_SOURCES_TARGETS)

COPY_SOURCES_TARGETS = $(copy_sources:%=copy-%)

$(COPY_SOURCES_TARGETS):
	@target=`echo "$@" | sed -e 's/^copy-//'`;\
	if test -e $(builddir)/$${target}; then \
		echo "exists $${target}."; \
	else \
		echo "clone $(srcdir)/$${target}"; \
		cp $(srcdir)/$${target} $(builddir)/$${target} || exit $$?; \
	fi

### Local Variables:
### mode: makefile-automake
### end:

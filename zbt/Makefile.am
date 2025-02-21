# touza/zbt Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Jul 24 2024
# Time-stamp: <2025/01/22 12:07:28 fuyuki Makefile.am>

# Copyright (C) 2024, 2025
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

SUBDIRS = zbt tests

extra_sources =
extra_sources += README-zbt.org MANIFEST.in pyproject.in
extra_sources += param.sh

copy_sources = $(extra_sources)
copy_sources += doc/demo01.png doc/demo02.png

EXTRA_DIST = $(copy_sources)

link_sources = $(extra_sources)
link_sources += doc
# dummy
all:

install-mod:

uninstall-mod:

python-build-prepare: copy-sources
	@for d in $(SUBDIRS); \
		do ( cd $$d; $(MAKE) copy-sources ); done

python-build: python-build-prepare
	$(PYTHON) -m build

link-sources: link-sources-am
	@for d in $(SUBDIRS); \
		do ( cd $$d; $(MAKE) link-sources ); done

clean-link-sources: clean-link-sources-am
	@for d in $(SUBDIRS); \
		do ( cd $$d; $(MAKE) clean-link-sources ); done

link-sources-am:
	@for target in $(link_sources); do \
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

copy-sources: $(COPY_SOURCES_TARGETS)

COPY_SOURCES_TARGETS = $(copy_sources:%=copy-%)

$(COPY_SOURCES_TARGETS):
	@target=`echo "$@" | sed -e 's/^copy-//'`;\
	if test -e $(builddir)/$${target}; then \
		echo "exists $${target}."; \
	else \
		echo "clone $(srcdir)/$${target}"; \
		dir=`dirname $(builddir)/$${target}`; \
		$(MKDIR_P) $$dir; \
		cp $(srcdir)/$${target} $(builddir)/$${target} || exit $$?; \
	fi

### Local Variables:
### mode: makefile-automake
### end:

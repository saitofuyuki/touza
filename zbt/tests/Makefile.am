# touza/zbt/tests Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Jul 24 2024
# Time-stamp: <2024/08/15 11:07:23 fuyuki Makefile.am>

# Copyright (C) 2024
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

EXTRA_DIST = gen_demo.sh 

noinst_PYTHON = 
noinst_PYTHON += test_libtouza.py test_xrnio.py test_dsnio.py

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

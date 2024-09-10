# touza/zbt/zbt Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Jul 24 2024
# Time-stamp: <2024/10/09 16:50:35 fuyuki Makefile.am>

# Copyright (C) 2024
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

if ZBT_BUILD_ONLY
else
pkgpython_PYTHON = __init__.py
pkgpython_PYTHON += util.py libtouza.py dsnio.py
pkgpython_PYTHON += xrnio.py
pkgpython_PYTHON += layout.py plot.py control.py
pkgpython_PYTHON += zbcont.py

# pkgpython_PYTHON += param.py
nodist_pkgpython_PYTHON = config.py param.py
pkgpython_DATA = zbtrc.toml

endif

copy_sources = __init__.py
copy_sources += util.py libtouza.py dsnio.py
copy_sources += xrnio.py zbcont.py
copy_sources += layout.py plot.py control.py
copy_sources += zbtrc.toml

EXTRA_PYTHON =
EXTRA_PYTHON += $(copy_sources)
EXTRA_PYTHON += config.py param.py

pkgpythondir = $(pythondir)/zbt

BUILT_SOURCE = param.py config.py

CLEANFILES = $(BUILT_SOURCE)

GENPARAM = $(srcdir)/../param.sh

# dummy
all: $(BUILT_SOURCE)

install-mod:

uninstall-mod:

param.py: $(top_srcdir)/nio/touza_nio_param.h
	$(SHELL) $(GENPARAM) -o $@ ParamTouzaNio $(top_srcdir)/nio/touza_nio_param.h || rm -f $@

config.py:
	echo "# pythondir=$(pythondir)" > $@
	echo "TOUZA_NAME = '$(TOUZA_NAME)'" >> $@
	echo "LIBRARY_DIR = '$(libdir)'" >> $@

copy-sources: $(BUILT_SOURCE) copy-sources-prepare $(COPY_SOURCES_TARGETS)

COPY_SOURCES_TARGETS = $(copy_sources:%=copy-%)

copy-sources-prepare:

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

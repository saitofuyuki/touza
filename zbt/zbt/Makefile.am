# touza/zbt/zbt Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Jul 24 2024
# Time-stamp: <2024/10/31 16:50:27 fuyuki Makefile.am>

# Copyright (C) 2024
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

BUILT_SOURCE =
EXTRA_DIST =

if ZBT_BUILD_ONLY
else
pkgpython_PYTHON = __init__.py
pkgpython_PYTHON += util.py libtouza.py dsnio.py
pkgpython_PYTHON += xrnio.py config.py
pkgpython_PYTHON += plot.py control.py
pkgpython_PYTHON += zbcont.py

# pkgpython_PYTHON += param.py
nodist_pkgpython_PYTHON = env.py param.py
pkgpython_DATA = zbtrc.toml

bin_SCRIPTS = zbcont
BUILT_SOURCE += $(bin_SCRIPTS)
endif
EXTRA_DIST += zbcont.in

copy_sources = __init__.py
copy_sources += util.py libtouza.py dsnio.py
copy_sources += xrnio.py config.py
copy_sources += plot.py control.py
copy_sources += zbcont.py zbtrc.toml

EXTRA_PYTHON =
EXTRA_PYTHON += $(copy_sources)
EXTRA_PYTHON += env.py param.py

pkgpythondir = $(pythondir)/zbt

BUILT_SOURCE += env.py param.py

CLEANFILES = $(BUILT_SOURCE)

GENPARAM = $(srcdir)/../param.sh

if zbt_embed_libdir
zbt_libtouza_dir = $(libdir)
else
zbt_libtouza_dir =
endif

# dummy
all: $(BUILT_SOURCE)

install-mod:

uninstall-mod:

param.py: $(top_srcdir)/nio/touza_nio_param.h
	$(SHELL) $(GENPARAM) -o $@ ParamTouzaNio $(top_srcdir)/nio/touza_nio_param.h || rm -f $@

env.py:
	echo "TOUZA_NAME = '$(TOUZA_NAME)'" >> $@
	echo "LIBRARY_DIR = '$(zbt_libtouza_dir)'" >> $@

zbcont: zbcont.in
	sed -e 's![@]PYTHON[@]!$(PYTHON)!' \
	    -e 's![@]PYTHON_BIN[@]!$(PYTHON_BIN)!' \
		-e 's![@]pythondir[@]!$(pythondir)!' \
		-e 's![@]pkgpythondir[@]!$(pkgpythondir)!' \
		$< > $@

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

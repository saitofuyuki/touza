# touza/zbt/zbt Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Jul 24 2024
# Time-stamp: <2024/08/11 16:47:30 fuyuki Makefile.am>

# Copyright (C) 2024
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

pkgpython_PYTHON = __init__.py
pkgpython_PYTHON += util.py libtouza.py dsnio.py
pkgpython_PYTHON += xrnio.py

pkgpython_PYTHON += param.py

pkgpythondir = $(pythondir)/zbt

# BUILT_SOURCE = param.py

GENPARAM = $(srcdir)/../param.sh

# dummy
all: $(BUILT_SOURCE)

install-mod:

uninstall-mod:

param.py: $(top_srcdir)/nio/touza_nio_param.h
	$(SHELL) $(GENPARAM) -o $@ ParamTouzaNio $(top_srcdir)/nio/touza_nio_param.h || rm -f $@

# touza/zbt/zbt Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Jul 24 2024
# Time-stamp: <2024/07/27 17:49:29 fuyuki Makefile.am>

# Copyright (C) 2024
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

python_PYTHON = __init__.py
python_PYTHON += util.py libtouza.py dsnio.py
python_PYTHON += xrnio.py

python_PYTHON += param.py

BUILT_SOURCE = param.py

GENPARAM = $(srcdir)/../param.sh

# dummy
all: $(BUILT_SOURCE)

install-mod:

uninstall-mod:

param.py: $(top_srcdir)/nio/touza_nio_param.h
	$(SHELL) $(GENPARAM) -o $@ ParamTouzaNio $(top_srcdir)/nio/touza_nio_param.h || rm -f $@

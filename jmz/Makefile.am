# touza/jmz Makefile.am
# Maintainer:  SAITO Fuyuki
# Created: Nov 21 2021
# Time-stamp: <2021/12/24 11:24:27 fuyuki Makefile.am>

# Copyright (C) 2021
#           Japan Agency for Marine-Earth Science and Technology
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

ACLOCAL_AMFLAGS	=	-I ../m4c

AM_FCFLAGS +=	$(DEBUG) $(TOUZA_CFLAGS)
AM_LDFLAGS =	$(TOUZA_LIBS)

bin_PROGRAMS	=	jmzconv jmzcmp
jmzconv_SOURCES =	jmzconv.F90 jmz.h
jmzconv_DEPENDENCIES	=	$(TOUZA_LIBS)

jmzcmp_SOURCES =	jmzcmp.F90 jmz.h
jmzcmp_DEPENDENCIES	=	$(TOUZA_LIBS)

EXTRA_DIST		=	
MOSTLYCLEANFILES=
SUBDIRS	=	.

install-mod:
	:

### Local Variables:
### mode: makefile-automake
### end:

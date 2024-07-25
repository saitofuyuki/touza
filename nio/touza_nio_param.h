/* touza_nio_param.h - TOUZA/Nio parameter macros */
/* Maintainer: SAITO Fuyuki */
/* Created: Jul 25 2024 */
/* Time-stamp: <2024/07/25 15:58:51 fuyuki touza_nio_param.h> */
/* Copyright (C) 2024 */
/*           Japan Agency for Marine-Earth Science and Technology */
/* Licensed under the Apache License, Version 2.0 */
/*   (https://www.apache.org/licenses/LICENSE-2.0) */

/* This file is provided for a simple solution */
/* to share various parameters among Fortran, C and python */

#ifndef    _TOUZA_NIO_PARAM_H
#  define  _TOUZA_NIO_PARAM_H 1

#  define NIO_HEADER_ITEMS        64
#  define NIO_HEADER_LEN_ITEM     16

#  define NIO_CACHE_COLL_DEFAULT   0
#  define NIO_CACHE_COLL_STRICT   -1
#  define NIO_CACHE_COLL_STD       1
#  define NIO_CACHE_COLL_BASIC     2
#  define NIO_CACHE_COLL_NOSIGN    3
#  define NIO_CACHE_COLL_NONUM     4
#  define NIO_CACHE_COLL_NOSPECIAL 8

#  define NIO_CACHE_ALLOW_VAR_DUP  16

#  define NIO_CACHE_VAR_SUITE     -99

#endif /* not _TOUZA_NIO_PARAM_H */

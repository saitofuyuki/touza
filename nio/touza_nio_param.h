/* touza_nio_param.h - TOUZA/Nio parameter macros */
/* Maintainer: SAITO Fuyuki */
/* Created: Jul 25 2024 */
/* Time-stamp: <2024/07/26 18:08:22 fuyuki touza_nio_param.h> */
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

#  define NIO_CACHE_COLL_MASK_STD       1
#  define NIO_CACHE_COLL_MASK_BASIC     2
#  define NIO_CACHE_COLL_MASK_NOSIGN    4
#  define NIO_CACHE_COLL_MASK_NONUM     8

#  define NIO_CACHE_COLL_STD       1
#  define NIO_CACHE_COLL_BASIC     (1+2)
#  define NIO_CACHE_COLL_NOSIGN    (1+2+4)
#  define NIO_CACHE_COLL_NONUM     (1+2+4+8)

#  define NIO_CACHE_COLL_NOSPECIAL 16

#  define NIO_CACHE_ALLOW_VAR_DUP  32
#  define NIO_CACHE_ALLOW_GRP_DUP  64
#  define NIO_CACHE_ALLOW_COOR_DUP  128

#  define NIO_CONTROL_ENABLE_CACHE      256
#  define NIO_CONTROL_ENABLE_SEQUENTIAL 512
#  define NIO_CONTROL_ENABLE_AUTO      1024

#  define NIO_CACHE_VAR_SUITE     -99

#endif /* not _TOUZA_NIO_PARAM_H */

/* touza_nio.h - TOUZA/Nio common definition */
/* Maintainer: SAITO Fuyuki */
/* Created: Oct 11 2021 */
/* Time-stamp: <2023/02/16 20:32:50 fuyuki touza_nio.h> */
/* Copyright (C) 2021, 2022 */
/*           Japan Agency for Marine-Earth Science and Technology */
/* Licensed under the Apache License, Version 2.0 */
/*   (https://www.apache.org/licenses/LICENSE-2.0) */

#include "touza.h"
#ifndef    _TOUZA_NIO_H
#  define  _TOUZA_NIO_H 1

#  ifdef TOUZA_TRANSFORM
#    define TOUZA_Nio       TOUZA_TRANSFORM(Nio)
#  endif
#  ifdef TOUZA_TRANSFORM_MDL
#  endif

#  ifndef GROUP_TAG
#  define GROUP_TAG 'Nio'
#  endif
#  ifndef __GRP__
#  define __GRP__ GROUP_TAG
#  endif

#ifndef    NIO_MSG_LEVEL
#  define  NIO_MSG_LEVEL 0
#endif

#define ERR_HITEM_INVALID               (ERR_MASK_SPECIFIC-1)   /* invalid header item */
#define ERR_HITEM_INVALID_RANGE         (ERR_MASK_SPECIFIC-2)   /* invalid header item range */
#define ERR_HITEM_TYPE_MISMATCH         (ERR_MASK_SPECIFIC-3)   /* header item type mismatch to default */

#define ERR_UNKNOWN_FORMAT              (ERR_MASK_SPECIFIC-11)  /* cannot parse as a gtool format */
#define ERR_NOT_GTOOL_FORMAT            (ERR_MASK_SPECIFIC-12)  /* cannot parse as a gtool record */
#define ERR_DEPRECATED_FORMAT           (ERR_MASK_SPECIFIC-13)  /* depreacted gtool format */

#define ERR_SIZE_MISMATCH               (ERR_MASK_SPECIFIC-14)  /* data length mismatch */

#ifndef   OPT_PATH_LEN
#  define OPT_PATH_LEN 1024 /* file path limit length */
#endif

#endif /* not _TOUZA_NIO_H */

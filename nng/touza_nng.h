/* touza_nng.h - TOUZA/Nng common definition */
/* Maintainer: SAITO Fuyuki */
/* Created: Oct 11 2021 */
/* Time-stamp: <2021/11/08 09:22:22 fuyuki touza_nng.h> */
/* Copyright (C) 2021 */
/*           Japan Agency for Marine-Earth Science and Technology */
/* Licensed under the Apache License, Version 2.0 */
/*   (https://www.apache.org/licenses/LICENSE-2.0) */

#include "touza.h"
#ifndef    _TOUZA_NNG_H
#  define  _TOUZA_NNG_H 1

#  ifdef TOUZA_TRANSFORM
#    define TOUZA_Nng       TOUZA_TRANSFORM(Nng)
#  endif
#  ifdef TOUZA_TRANSFORM_MDL
#  endif

#  ifndef GROUP_TAG
#  define GROUP_TAG 'nng'
#  endif
#  ifndef __GRP__
#  define __GRP__ GROUP_TAG
#  endif

#ifndef    NNG_MSG_LEVEL
#  define  NNG_MSG_LEVEL 0
#endif

#define ERR_HITEM_INVALID               (ERR_MASK_SPECIFIC-1)   /* invalid header item */
#define ERR_HITEM_INVALID_RANGE         (ERR_MASK_SPECIFIC-2)   /* invalid header item range */
#define ERR_HITEM_TYPE_MISMATCH         (ERR_MASK_SPECIFIC-3)   /* header item type mismatch to default */

#define ERR_INVALID_RECORD_SIZE         (ERR_MASK_SPECIFIC-9)   /* insufficient sequential record length */
#define ERR_INCONSISTENT_RECORD_MARKERS (ERR_MASK_SPECIFIC-10)  /* different head/foot markers */
#define ERR_UNKNOWN_FORMAT              (ERR_MASK_SPECIFIC-11)  /* cannot parse as a gtool format */
#define ERR_NOT_GTOOL_FORMAT            (ERR_MASK_SPECIFIC-12)  /* cannot parse as a gtool record */

#endif /* not _TOUZA_NNG_H */

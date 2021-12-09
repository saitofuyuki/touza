/* touza_trp.h - TOUZA/Trp common definition */
/* Maintainer: SAITO Fuyuki */
/* Created: Feb 26 2021 */
/* Time-stamp: <2021/11/08 21:38:41 fuyuki touza_trp.h> */
/* Copyright (C) 2021 */
/*           Japan Agency for Marine-Earth Science and Technology */
/* Licensed under the Apache License, Version 2.0 */
/*   (https://www.apache.org/licenses/LICENSE-2.0) */

#include "touza.h"

#ifndef    _TOUZA_TRP_H
#  define  _TOUZA_TRP_H 1

#  ifdef TOUZA_TRANSFORM
#    define TOUZA_Trp       TOUZA_TRANSFORM(Trp)
#  endif
#  ifdef TOUZA_TRANSFORM_MDL
#    define TOUZA_Trp_std    TOUZA_TRANSFORM_MDL(Trp,std)
#    define TOUZA_Trp_pack   TOUZA_TRANSFORM_MDL(Trp,pack)
#    define TOUZA_Trp_float  TOUZA_TRANSFORM_MDL(Trp,float)
#  endif

#  ifndef GROUP_TAG
#  define GROUP_TAG 'trp'
#  endif
#  ifndef __GRP__
#  define __GRP__ GROUP_TAG
#  endif

#ifndef    TRP_MSG_LEVEL
#  define  TRP_MSG_LEVEL 0
#endif

#endif /* not _TOUZA_TRP_H */

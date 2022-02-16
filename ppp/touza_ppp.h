/* touza_ppp.h - touza/ppp common definition */
/* Maintainer: SAITO Fuyuki */
/* Created: Jan 26 2022 */
/* Time-stamp: <2022/01/26 15:28:13 fuyuki touza_ppp.h> */
/* Copyright (C) 2022 */
/*           Japan Agency for Marine-Earth Science and Technology */
/* Licensed under the Apache License, Version 2.0 */
/*   (https://www.apache.org/licenses/LICENSE-2.0) */

#include "touza.h"
#ifndef    _TOUZA_PPP_H
#  define  _TOUZA_PPP_H 1

#  ifdef TOUZA_TRANSFORM
#    define TOUZA_Ppp      TOUZA_TRANSFORM(Ppp)
#  endif
#  ifdef TOUZA_TRANSFORM_MDL
#    define TOUZA_Ppp_std   TOUZA_TRANSFORM_MDL(Ppp,std)
#    define TOUZA_Ppp_comm  TOUZA_TRANSFORM_MDL(Ppp,comm)
#  endif

#  ifndef GROUP_TAG
#  define GROUP_TAG 'ppp'
#  endif
#  ifndef __GRP__
#  define __GRP__ GROUP_TAG
#  endif

#ifndef    PPP_MSG_LEVEL
#  define  PPP_MSG_LEVEL 0
#endif

#endif /* not _TOUZA_PPP_H */

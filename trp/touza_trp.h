/* touza_trp.h - TOUZA/Trp common definition */
/* Maintainer: SAITO Fuyuki */
/* Created: Feb 26 2021 */
/* Time-stamp: <2023/02/25 22:19:35 fuyuki touza_trp.h> */
/* Copyright (C) 2021-2023 */
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

/* issue in GCC Fortran */
#if __GFORTRAN__
#  ifndef OPT_USE_IPC_IBITS
#  define OPT_USE_IPC_IBITS 1
#  endif
#endif
#ifndef   OPT_USE_IPC_IBITS
#  define OPT_USE_IPC_IBITS 0
#endif
#if OPT_USE_IPC_IBITS
#  warning "Disable (some) intrinsic ibits() calls."
#endif

#if OPT_USE_IPC_IBITS
# define _IBITS(I,P,L) ipc_IBITS(I,P,L)
#else
# define _IBITS(I,P,L) IBITS(I,P,L)
#endif

#endif /* not _TOUZA_TRP_H */

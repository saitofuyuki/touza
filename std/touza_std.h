/* touza_std.h - touza/std common definition */
/* Maintainer: SAITO Fuyuki */
/* Created: May 30 2020 */
/* Time-stamp: <2022/01/28 08:50:29 fuyuki touza_std.h> */
/* Copyright (C) 2020, 2021 */
/*           Japan Agency for Marine-Earth Science and Technology */
/* Licensed under the Apache License, Version 2.0 */
/*   (https://www.apache.org/licenses/LICENSE-2.0) */

#include "touza.h"
#ifndef    _TOUZA_STD_H
#  define  _TOUZA_STD_H 1

#  ifdef TOUZA_TRANSFORM
#    define TOUZA_Std      TOUZA_TRANSFORM(Std)
#  endif
#  ifdef TOUZA_TRANSFORM_MDL
#    define TOUZA_Std_arg  TOUZA_TRANSFORM_MDL(Std,arg)
#    define TOUZA_Std_env  TOUZA_TRANSFORM_MDL(Std,env)
#    define TOUZA_Std_fun  TOUZA_TRANSFORM_MDL(Std,fun)
#    define TOUZA_Std_log  TOUZA_TRANSFORM_MDL(Std,log)
#    define TOUZA_Std_mwe  TOUZA_TRANSFORM_MDL(Std,mwe)
#    define TOUZA_Std_prc  TOUZA_TRANSFORM_MDL(Std,prc)
#    define TOUZA_Std_utl  TOUZA_TRANSFORM_MDL(Std,utl)
#    define TOUZA_Std_htb  TOUZA_TRANSFORM_MDL(Std,htb)
#  endif

/* message formats (general) */

#ifndef    TOUZA_TAG_SEP
#  define  TOUZA_TAG_SEP '/'
#endif
#ifndef    TOUZA_FUN_SEP
#  define  TOUZA_FUN_SEP ':'
#endif

/* macros used in modules deeper than Std_log */

#  define _TOUZA_FORMAT_TAG(T)  '[', T, '] '

#  define _TOUZA_TAG_P(P)       (P)
#  define _TOUZA_TAG_PG(P,G)    (P, TOUZA_TAG_SEP, G)
#  define _TOUZA_TAG_PGM(P,G,M) (P, TOUZA_TAG_SEP, G, TOUZA_TAG_SEP, M)
#  define _TOUZA_TAG_F(F)       (TOUZA_FUN_SEP, F)

#  define _TOUZA_FORMAT_FUN(P,G,M,F) _TOUZA_FORMAT_TAG((_TOUZA_TAG_PGM(P,G,M), _TOUZA_TAG_F(F)))
#  define _TOUZA_FORMAT_MDL(P,G,M)   _TOUZA_FORMAT_TAG((_TOUZA_TAG_PGM(P,G,M)))
#  define _TOUZA_FORMAT_GRP(P,G)     _TOUZA_FORMAT_TAG((_TOUZA_TAG_PG(P,G)))

# define TOUZA_FORMAT_FUN(G,M,F) _TOUZA_FORMAT_FUN(PACKAGE_TAG,G,M,F)
# define TOUZA_FORMAT_MDL(G,M)   _TOUZA_FORMAT_MDL(PACKAGE_TAG,G,M)
# define TOUZA_FORMAT_GRP(G)     _TOUZA_FORMAT_GRP(PACKAGE_TAG,G)

/* Std group */

#  ifndef GROUP_TAG
#  define GROUP_TAG 'Std'
#  endif

#  ifndef __GRP__
#  define __GRP__ GROUP_TAG
#  endif

#  define STD_FORMAT          TOUZA_FORMAT_GRP(__GRP__)
#  define STD_FORMAT_MDL(M)   TOUZA_FORMAT_MDL(__GRP__,M)
#  define STD_FORMAT_FUN(M,F) TOUZA_FORMAT_FUN(__GRP__,M,F)

/* message verbose level chech by macro */

#  define MSG_LEVEL_PANIC     16
#  define MSG_LEVEL_FATAL     8
#  define MSG_LEVEL_CRITICAL  4
#  define MSG_LEVEL_SEVERE    2
#  define MSG_LEVEL_WARNING   1
#  define MSG_LEVEL_NORMAL    0
#  define MSG_LEVEL_INFO     -1
#  define MSG_LEVEL_DETAIL   -2
#  define MSG_LEVEL_DEBUG    -4

#  define VCHECK(L,C) ((L)+(C)).ge.0

#  define VCHECK_PANIC(L)    VCHECK(L,MSG_LEVEL_PANIC)
#  define VCHECK_FATAL(L)    VCHECK(L,MSG_LEVEL_FATAL)
#  define VCHECK_CRITICAL(L) VCHECK(L,MSG_LEVEL_CRITICAL)
#  define VCHECK_SEVERE(L)   VCHECK(L,MSG_LEVEL_SEVERE)
#  define VCHECK_WARNING(L)  VCHECK(L,MSG_LEVEL_WARNING)
#  define VCHECK_NORMAL(L)   VCHECK(L,MSG_LEVEL_NORMAL)
#  define VCHECK_INFO(L)     VCHECK(L,MSG_LEVEL_INFO)
#  define VCHECK_DETAIL(L)   VCHECK(L,MSG_LEVEL_DETAIL)
#  define VCHECK_DEBUG(L)    VCHECK(L,MSG_LEVEL_DEBUG)

#ifndef   STD_MSG_LEVEL
#  define STD_MSG_LEVEL MSG_LEVEL_NORMAL
#endif

#  ifndef PACKAGE_TAG
#  define PACKAGE_TAG GROUP_TAG
#  endif

#define ERR_INVALID_RECORD_SIZE         (ERR_MASK_SPECIFIC-1)  /* insufficient sequential record length */
#define ERR_INCONSISTENT_RECORD_MARKERS (ERR_MASK_SPECIFIC-2)  /* different head/foot markers */

#endif /* not _TOUZA_STD_H */

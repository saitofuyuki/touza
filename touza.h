!!!_! touza.h - touza definitions
! Maintainer: SAITO Fuyuki
! Created: Jun 5 2020
! Time-stamp: <2021/01/25 17:59:58 fuyuki touza.h>

/* Copyright (C) 2020, 2021 */
/*           Japan Agency for Marine-Earth Science and Technology */
/* Licensed under the Apache License, Version 2.0 */
/*   (https://www.apache.org/licenses/LICENSE-2.0) */

#ifndef    _TOUZA_H
#  define  _TOUZA_H

#if   HAVE_PP_CONCAT == 1
#  define _CONCAT(A,B) A ## B
#  define CONCAT(A,B) _CONCAT(A,B)
#elif HAVE_PP_CONCAT == 2
#  define _CONCAT(A)  A
#  define CONCAT(A,B) _CONCAT(A)/**/B
#else
#  define CONCAT(A,B) B
#endif

/* init mode */
#define INIT_SKIP    -1   /* skip */
#define INIT_DEFAULT 0    /* default init mode */
#define INIT_SHALLOW +1   /* shallow initialization (not call deeper) */
#define INIT_DEEP    +2   /* deep initialization (call deeper) */

/* diag mode */
#define DIAG_SKIP          -1   /* shallow diagnosis */
#define DIAG_DEFAULT        0   /* default diagnosis */
#define DIAG_SHALLOW       +1   /* shallow diagnosis */
#define DIAG_DEEP          +2   /* deep diagnosis */
#define DIAG_FORCE         +4   /* force bit */
#define DIAG_SHALLOW_FORCE +5   /* 1+4 shallow diagnosis (force) */
#define DIAG_DEEP_FORCE    +6   /* 2+4 deep diagnosis (force) */

/* errors */

#define ERR_MULTIPLE_INIT -16
#define ERR_FAILURE_INIT  -17

#endif  /* not _TOUZA_H */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! mode: f90
! End:

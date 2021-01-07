!!!_! touza.h - touza definitions
! Maintainer: SAITO Fuyuki
! Created: Jun 5 2020
! Time-stamp: <2021/01/07 11:54:21 fuyuki touza.h>

/* Copyright (C) 2020, 2021 */
/*           Japan Agency for Marine-Earth Science and Technology */
/* Licensed under the Apache License, Version 2.0 */
/*   (https://www.apache.org/licenses/LICENSE-2.0) */

#ifndef    _TOUZA_H
#  define  _TOUZA_H

/* preprocessor concatenation */
#ifndef   HAVE_PP_CONCAT
#  define HAVE_PP_CONCAT 0
#endif

#if   HAVE_PP_CONCAT == 1
#  define _CONCAT(A,B) A ## B
#  define CONCAT(A,B) _CONCAT(A,B)
#elif HAVE_PP_CONCAT == 2
#  define _CONCAT(A)  A
#  define CONCAT(A,B) _CONCAT(A)/**/B
#else
#  define CONCAT(A,B) B
#endif

#ifdef TOUZA_PREFIX
#  define STD_NAME(T) CONCAT(TOUZA_PREFIX,T)
#  define CAL_NAME(T) CONCAT(TOUZA_PREFIX,T)
#else /* not TOUZA_PREFIX */
#  define STD_NAME(T) T
#  define CAL_NAME(T) T
#endif /* not TOUZA_PREFIX */

#endif  /* not _TOUZA_H */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! mode: f90
! End:

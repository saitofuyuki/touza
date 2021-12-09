!!!_! touza.h - touza definitions
! Maintainer: SAITO Fuyuki
! Created: Jun 5 2020
! Time-stamp: <2021/12/09 12:32:14 fuyuki touza.h>

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

/* init/diag/finalize mode */
#define MODE_SKIP      -1   /* skip */
#define MODE_DEFAULT    0   /* default init mode */
#define MODE_SURFACE    1   /* surface initialization (module) */
#define MODE_SHALLOW    2   /* shallow initialization (group) */
#define MODE_BIT_DEEP   4   /* deep initialization    (group+module) */
#define MODE_DEEP     (MODE_BIT_DEEP+MODE_SURFACE) /* +4+1 deep initialization    (group+module) */
#define MODE_DEEPER   (MODE_BIT_DEEP+MODE_SHALLOW) /* +4+2 deeper initialization  (group+group)  */
#define MODE_DEEPEST  (MODE_DEEPER+1)              /* +4+3 deepest initialization  (full)  */
#define MODE_FORCE      8   /* force initialization */
#define MODE_LOOSE     16  /* allow non-initialized */

/* errors */

/* common errors */
#define ERR_SUCCESS   0

#define ERR_UNDEFINED -1   /* undefined error */
#define ERR_NO_INIT   -2   /* module is not initialized */
#define ERR_MULTIPLE_INIT -16
#define ERR_FAILURE_INIT  -17
#define ERR_NOT_IMPLEMENTED -251
#define ERR_SEVERE    -252
#define ERR_CRITICAL  -253
#define ERR_FATAL     -254
#define ERR_PANIC     -255

#define ERR_ALLOCATION    -32 /* allocation error */
#define ERR_NO_IO_UNIT    -33 /* io unit allocation error */
#define ERR_EOF           -34 /* end of file */
#define ERR_BROKEN_RECORD -35 /* broken record (sequential) */

/* module specific errors */

#define ERR_MASK_SPECIFIC -256 /* module specific error offset */

/* masks */
#define ERR_MASK_MODULE  512
#define ERR_MASK_GROUP   8192

#define ERR_MASK_STD     (ERR_MASK_GROUP * 1)
#define ERR_MASK_STD_PRC (ERR_MASK_STD + ERR_MASK_MODULE * 1)
#define ERR_MASK_STD_UTL (ERR_MASK_STD + ERR_MASK_MODULE * 2)
#define ERR_MASK_STD_LOG (ERR_MASK_STD + ERR_MASK_MODULE * 3)
#define ERR_MASK_STD_FUN (ERR_MASK_STD + ERR_MASK_MODULE * 4)
#define ERR_MASK_STD_ENV (ERR_MASK_STD + ERR_MASK_MODULE * 5)
#define ERR_MASK_STD_BLD (ERR_MASK_STD + ERR_MASK_MODULE * 6)
#define ERR_MASK_STD_ARG (ERR_MASK_STD + ERR_MASK_MODULE * 7)
#define ERR_MASK_STD_MWE (ERR_MASK_STD + ERR_MASK_MODULE * 8)
#define ERR_MASK_STD_WSH (ERR_MASK_STD + ERR_MASK_MODULE * 9)

#define ERR_MASK_CAL     (ERR_MASK_GROUP * 2)
#define ERR_MASK_EMU     (ERR_MASK_GROUP * 3)
#define ERR_MASK_DIV     (ERR_MASK_GROUP * 4)
#define ERR_MASK_TRP     (ERR_MASK_GROUP * 5)
#define ERR_MASK_NNG     (ERR_MASK_GROUP * 6)

#  ifndef __PKG__
#  define __PKG__ PACKAGE_TAG
#  endif

#endif  /* not _TOUZA_H */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! mode: f90
! End:

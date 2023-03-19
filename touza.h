/* touza.h - touza definitions */
/*    Maintainer: SAITO Fuyuki */
/*    Created: Jun 5 2020 */
/*     Time-stamp: <2023/03/20 21:29:29 fuyuki touza.h> */

/* Copyright (C) 2020-2023 */
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

/* mpi module */
#ifndef _MPI_
#  if    HAVE_FORTRAN_MPI_MPI_BCAST
#    define _MPI_ MPI
#  elif  HAVE_FORTRAN_MPI_F08
#    define _MPI_ MPI_F08
#  else
#    define _MPI_ MPI
#  endif
#endif /* _MPI_ */
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

#define ERR_ALLOCATION          -32 /* allocation error */
#define ERR_NO_IO_UNIT          -33 /* io unit allocation error */
#define ERR_EOF                 -34 /* end of file */
#define ERR_BROKEN_RECORD       -35 /* broken record (sequential) */
#define ERR_OUT_OF_RANGE        -36
#define ERR_DUPLICATE_SET       -37 /* item is already set */
#define ERR_OPR_DISABLE         -38 /* disabled operation */
#define ERR_INVALID_PARAMETER   -39 /* invalid content for argument(s) */
#define ERR_INVALID_SWITCH      -40 /* invalid switch for argument */
#define ERR_SECOND_INVOCATION   -41 /* operation allowed only once  */
#define ERR_INSUFFICIENT_BUFFER -42 /* buffer is not enough to store */
#define ERR_NEED_ARGUMENT       -43 /* require (may be optional) argument */
#define ERR_FRESH               -44 /* something is not initilaized */
#define ERR_STALE               -45 /* something is already initilaized */
#define ERR_INVALID_ITEM        -46 /* wrong item */
#define ERR_NOT_FOUND           -47 /* search failed */
#define ERR_FEW_ARGUMENTS       -48 /* insufficient arguments  */
#define ERR_IO_GENERAL          -49 /* general error at io operation */
#define ERR_FILE_EXISTS         -50 /* file exists */
#define ERR_FILE_NOT_EXISTS     -51 /* file not exists */

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
#define ERR_MASK_STD_SUS (ERR_MASK_STD + ERR_MASK_MODULE *10)
#define ERR_MASK_STD_HTB (ERR_MASK_STD + ERR_MASK_MODULE *11)
#define ERR_MASK_STD_IPC (ERR_MASK_STD + ERR_MASK_MODULE *12)

#define ERR_MASK_DIV        (ERR_MASK_GROUP * 4)
#define ERR_MASK_DIV_STD    (ERR_MASK_DIV + ERR_MASK_MODULE * 1)
#define ERR_MASK_DIV_TILE   (ERR_MASK_DIV + ERR_MASK_MODULE * 2)
#define ERR_MASK_DIV_LAYOUT (ERR_MASK_DIV + ERR_MASK_MODULE * 3)
#define ERR_MASK_DIV_ZONE   (ERR_MASK_DIV + ERR_MASK_MODULE * 4)
#define ERR_MASK_DIV_SCAN   (ERR_MASK_DIV + ERR_MASK_MODULE * 5)
#define ERR_MASK_DIV_CTRL   (ERR_MASK_DIV + ERR_MASK_MODULE * 6)

#define ERR_MASK_PPP      (ERR_MASK_GROUP * 7)
#define ERR_MASK_PPP_STD  (ERR_MASK_PPP + ERR_MASK_MODULE * 1)
#define ERR_MASK_PPP_AMNG (ERR_MASK_PPP + ERR_MASK_MODULE * 2)
#define ERR_MASK_PPP_KING (ERR_MASK_PPP + ERR_MASK_MODULE * 3)
#define ERR_MASK_PPP_COMM (ERR_MASK_PPP + ERR_MASK_MODULE * 4)

#define ERR_MASK_NIO          (ERR_MASK_GROUP * 6)
#define ERR_MASK_NIO_STD      (ERR_MASK_NIO + ERR_MASK_MODULE * 1)
#define ERR_MASK_NIO_HEADER   (ERR_MASK_NIO + ERR_MASK_MODULE * 2)
#define ERR_MASK_NIO_RECORD   (ERR_MASK_NIO + ERR_MASK_MODULE * 3)
#define ERR_MASK_NIO_DIVISION (ERR_MASK_NIO + ERR_MASK_MODULE * 4)
#define ERR_MASK_NIO_NCTCDF   (ERR_MASK_NIO + ERR_MASK_MODULE * 5)
#define ERR_MASK_NIO_BINDC    (ERR_MASK_NIO + ERR_MASK_MODULE * 6)
#define ERR_MASK_NIO_CACHE    (ERR_MASK_NIO + ERR_MASK_MODULE * 7)
#define ERR_MASK_NIO_CTRL     (ERR_MASK_NIO + ERR_MASK_MODULE * 8)

#define ERR_MASK_TRP          (ERR_MASK_GROUP * 5)
#define ERR_MASK_TRP_STD      (ERR_MASK_TRP + ERR_MASK_MODULE * 1)
#define ERR_MASK_TRP_PACK     (ERR_MASK_TRP + ERR_MASK_MODULE * 2)
#define ERR_MASK_TRP_FLOAT    (ERR_MASK_TRP + ERR_MASK_MODULE * 3)

#define ERR_MASK_CAL     (ERR_MASK_GROUP * 2)
#define ERR_MASK_EMU     (ERR_MASK_GROUP * 3)

#define DEBUG_TRACE(E,T) call trace_debug(E, T, __FILE__, __LINE__)

#  ifndef __PKG__
#  define __PKG__ PACKAGE_TAG
#  endif

#endif  /* not _TOUZA_H */

/* Local Variables: */
/* mode: f90 */
/* End: */

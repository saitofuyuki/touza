/* touza_std.h - touza/std common definition */
/* Maintainer: SAITO Fuyuki */
/* Created: May 30 2020 */
/* Time-stamp: <2021/01/07 09:23:35 fuyuki std.h> */
/* Copyright (C) 2020, 2021 */
/*           Japan Agency for Marine-Earth Science and Technology */
/* Licensed under the Apache License, Version 2.0 */
/*   (https://www.apache.org/licenses/LICENSE-2.0) */

#ifndef    _TOUZA_STD_H
#  define  _TOUZA_STD_H 1

#  ifdef TOUZA_TRANSFORM
#    define TOUZA_Std      TOUZA_TRANSFORM(Std)
#  endif
#  ifdef TOUZA_TRANSFORM_MEM
#    define TOUZA_Std_arg  TOUZA_TRANSFORM_MEM(Std, arg)
#    define TOUZA_Std_env  TOUZA_TRANSFORM_MEM(Std, env)
#    define TOUZA_Std_fun  TOUZA_TRANSFORM_MEM(Std, fun)
#    define TOUZA_Std_log  TOUZA_TRANSFORM_MEM(Std, log)
#    define TOUZA_Std_mwe  TOUZA_TRANSFORM_MEM(Std, mwe)
#    define TOUZA_Std_prc  TOUZA_TRANSFORM_MEM(Std, prc)
#    define TOUZA_Std_utl  TOUZA_TRANSFORM_MEM(Std, utl)
#  endif

#endif /* not _TOUZA_STD_H */

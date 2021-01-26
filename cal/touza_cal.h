/* touza_cal.h - touza/calendar definitions */
/* Maintainer: SAITO Fuyuki */
/* Created: Jul 31 2011 */
/* Time-stamp: <2021/01/26 11:06:55 fuyuki touza_cal.h> */
/* Copyright (C) 2011-2021 */
/*           Japan Agency for Marine-Earth Science and Technology */
/* Licensed under the Apache License, Version 2.0 */
/*   (https://www.apache.org/licenses/LICENSE-2.0) */

#ifndef    _TOUZA_CAL_H
#  define  _TOUZA_CAL_H 1

#ifndef   OPT_CALENDAR_MAX_MONTH /* maximum number of month in a year */
#  define OPT_CALENDAR_MAX_MONTH 12
#endif
#ifndef   OPT_CALENDAR_MAX_LEAP  /* maximum number of leap-year kinds */
#  define OPT_CALENDAR_MAX_LEAP  2
#endif

#ifndef   OPT_KIND_REAL  /* target real kind (default: TOUZA_Std::KDBL) */
#  define OPT_KIND_REAL KDBL
#endif

#  ifndef GROUP_TAG
#  define GROUP_TAG 'Cal'
#  endif
#  ifndef __GRP__
#  define __GRP__ GROUP_TAG
#  endif

#  define CAL_FORMAT          TOUZA_FORMAT_GRP(GROUP_TAG)
#  define CAL_FORMAT_MDL(M)   TOUZA_FORMAT_MDL(GROUP_TAG,M)
#  define CAL_FORMAT_PRC(M,F) TOUZA_FORMAT_PRC(GROUP_TAG,M,F)

#  ifdef TOUZA_TRANSFORM
#    define TOUZA_Cal           TOUZA_TRANSFORM(Cal)
#  endif
#  ifdef TOUZA_TRANSFORM_MDL
#    define TOUZA_Cal_core      TOUZA_TRANSFORM_MDL(Cal,core)
#    define TOUZA_Cal_ils       TOUZA_TRANSFORM_MDL(Cal,ils)
#    define TOUZA_Cal_matsiro   TOUZA_TRANSFORM_MDL(Cal,matsiro)
#    define TOUZA_Cal_miroc     TOUZA_TRANSFORM_MDL(Cal,miroc)
#    define TOUZA_Cal_primitive TOUZA_TRANSFORM_MDL(Cal,primitive)
#  endif

#ifndef    CAL_MSG_LEVEL
#  define  CAL_MSG_LEVEL 0
#endif

#endif  /* not _TOUZA_CAL_H */

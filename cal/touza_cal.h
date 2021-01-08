/* touza_cal.h - touza/calendar definitions */
/* Maintainer: SAITO Fuyuki */
/* Created: Jul 31 2011 */
/* Time-stamp: <2021/01/07 11:42:20 fuyuki touza_cal.h> */
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

#  ifdef TOUZA_TRANSFORM
#    define TOUZA_Cal           TOUZA_TRANSFORM(Cal)
#  endif
#  ifdef TOUZA_TRANSFORM_MEM
#    define TOUZA_Cal_core      TOUZA_TRANSFORM_MEM(Cal, core)
#    define TOUZA_Cal_ils       TOUZA_TRANSFORM_MEM(Cal, ils)
#    define TOUZA_Cal_matsiro   TOUZA_TRANSFORM_MEM(Cal, matsiro)
#    define TOUZA_Cal_miroc     TOUZA_TRANSFORM_MEM(Cal, miroc)
#    define TOUZA_Cal_primitive TOUZA_TRANSFORM_MEM(Cal, primitive)
#  endif

#endif  /* not _TOUZA_CAL_H */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! mode: f90
! End:

!!!_! touza.F90 - touza administration
! Maintainer: SAITO Fuyuki
! Created: Jun 6 2020
#define TIME_STAMP 'Time-stamp: <2021/01/13 16:55:27 fuyuki touza.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2020, 2021
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!!!_* include
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
!!!_* Health check
#if ENABLE_TOUZA_STD
#else
#  error 'TOUZA/std package must be enabled.'
#endif
!!!_* Macros
!!!_@ TOUZA - touza interfaces
module TOUZA
  use TOUZA_Std,&
       & std_init=>init, std_diag=>diag, std_finalize=>finalize
#if ENABLE_TOUZA_CAL
  use TOUZA_Cal, &
       & cal_init=>init, cal_diag=>diag, cal_finalize=>finalize
#endif /* ENABLE_TOUZA_CAL */
!!!_  - default
  implicit none
  private
!!!_  - public
!!!_   . self
  public :: init, diag, finalize
!!!_   . std
  public :: KFLT,     KDBL
  public :: std_init, std_diag, std_finalize
  public :: choice,   choice_a, set_if_present
  public :: uin, uout, uerr
  public :: msg
  public :: unit_star, unit_none, unit_global
  public :: decl_pos_arg, parse, get_option, get_param
!!!_   . calendar
#if ENABLE_TOUZA_CAL
  public :: cal_init, cal_diag, cal_finalize
  public :: alloc, new_calendar
  public :: set_perpetual_date,     set_perpetual_adate
  public :: get_perpetual_date,     set_perpetual_switch
  public :: inq_nday_month,         inq_nday_year,      inq_nmonth_year
  public :: inq_nsec_day,           inq_nsec_minute,    inq_nsec_hour
  public :: conv_cdaysec_csec,      conv_time_tsec,     conv_date_cday
  public :: conv_date_dayy,         conv_date_dayy_compat
  public :: conv_calendar_csec,     conv_duration_sec,  conv_csec_string_ppt_off
  public :: advance_csec,           conv_calendar_string
  public :: is_passed,              is_passed_compat
  public :: conv_csec_cdaysec,      conv_csec_adaysec
  public :: conv_tsec_time,         conv_tsec_atime
  public :: conv_cday_date,         conv_cday_adate
  public :: conv_adate_cday
  public :: conv_csec_calendar,     conv_csec_acalendar
  public :: conv_cday_cydayy,       conv_cday_aydayy
  public :: conv_csec_cydayy,       conv_csec_aydayy
  public :: conv_csec_date,         conv_csec_adate
  public :: conv_string_calendar,   conv_string_acalendar
  public :: KRC,     XREAL
  public :: p_error, p_ideal, p_grego_i, p_grego_l, p_user
#endif /* ENABLE_TOUZA_CAL */
contains
!!!_ + common interfaces
!!!_  & init - touza system initialization batch
  subroutine init(ierr, logu, loglev)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: loglev
    integer,intent(in),optional :: logu
    ierr = 0
    if (ierr.eq.0) call std_init(ierr, logu, loglev)
#ifndef   PACKAGE_STRING
#  define PACKAGE_STRING 'touza 0.000'
#endif
    if (ierr.eq.0) call msg(PACKAGE_STRING, 'TOUZA', 0, logu)
    if (ierr.eq.0) call msg(TIME_STAMP, 'TOUZA', 0, logu)
#if ENABLE_TOUZA_CAL
    if (ierr.eq.0) call cal_init(ierr, logu)
#endif /* ENABLE_TOUZA_CAL */
    return
  end subroutine init
!!!_  & diag - touza system diagnosis batch
  subroutine diag(ierr, u)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    ierr = 0
    if (ierr.eq.0) call std_diag(ierr, u)
#if ENABLE_TOUZA_CAL
    if (ierr.eq.0) call cal_diag(ierr, u)
#else  /* not ENABLE_TOUZA_CAL */
    if (ierr.eq.0) call msg('cal disabled', 'TOUZA', 0, u)
#endif /* not ENABLE_TOUZA_CAL */
    return
  end subroutine diag
!!!_  & finalize - touza system finalization batch
  subroutine finalize(ierr, u)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    ierr = 0
#if ENABLE_TOUZA_CAL
    if (ierr.eq.0) call cal_finalize(ierr, u)
#endif /* ENABLE_TOUZA_CAL */
    if (ierr.eq.0) call std_finalize(ierr, u)
    return
  end subroutine finalize
end module TOUZA
!!!_@ test_touza - test program
#ifdef TEST_TOUZA
program test_touza
  use TOUZA
  implicit none
  integer ierr

  call init(ierr)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call finalize(ierr)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_touza

#endif /* TEST_TOUZA */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

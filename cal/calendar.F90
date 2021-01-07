!!!_! calendar.F90 - touza/calendar: manager
! Maintainer: SAITO Fuyuki
! Created: May 31 2020
#define TIME_STAMP 'Time-stamp: <2021/01/07 12:07:03 fuyuki calendar.F90>'

!!!_! MANIFESTO
!
! Copyright (C) 2020, 2021
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!

!!!_* Includes
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_cal.h"
!!!_* Macros
#ifndef    OPT_CALENDAR_HANDLES
#  define  OPT_CALENDAR_HANDLES 1
#endif
!!!_& TOUZA_Cal - calendar manager
module TOUZA_Cal
  use TOUZA_Cal_core,only: &
       & KRC,         XREAL, &
       & cal_attr_t,  cal_daysec_t, cal_date_t, cal_time_t, &
       & cal_ynday_t, calendar_t, &
       & p_error, p_ideal, p_grego_i, p_grego_l, p_user, &
       & set_perpetual_date_core        => set_perpetual_date, &
       & get_perpetual_date_core        => get_perpetual_date, &
       & conv_string_calendar,          &
       & conv_calendar_string_core      => conv_calendar_string
!!!_ = declaration
  implicit none
  private
!!!_  - interfaces
  interface set_perpetual_date
     module procedure set_perpetual_date_t, set_perpetual_adate
  end interface set_perpetual_date

  interface get_perpetual_date
     module procedure get_perpetual_date_t, get_perpetual_adate
  end interface get_perpetual_date

  interface inq_nday_month
     module procedure inq_nday_month_t, inq_nday_amonth
  end interface inq_nday_month

  interface inq_nday_year
     module procedure inq_nday_year_t, inq_nday_ayear
  end interface inq_nday_year

  interface inq_nmonth_year
     module procedure inq_nmonth_year_t, inq_nmonth_ayear
  end interface inq_nmonth_year

  interface inq_nsec_day
     module procedure inq_nsec_day_t, inq_nsec_aday
  end interface inq_nsec_day

  interface inq_nsec_minute
     module procedure inq_nsec_minute_t, inq_nsec_aminute
  end interface inq_nsec_minute

  interface inq_nsec_hour
     module procedure inq_nsec_hour_t, inq_nsec_ahour
  end interface inq_nsec_hour

  interface conv_cdaysec_csec
     module procedure conv_cdaysec_csec_c, conv_cdaysec_csec_i
     module procedure conv_adaysec_csec_c, conv_adaysec_csec_i
  end interface conv_cdaysec_csec

  interface conv_time_tsec
     module procedure conv_time_tsec_t, conv_atime_tsec
  end interface conv_time_tsec

  interface conv_tsec_time
     module procedure conv_tsec_time_c, conv_tsec_time_i
  end interface conv_tsec_time

  interface conv_tsec_atime
     module procedure conv_tsec_atime_c, conv_tsec_atime_i
  end interface conv_tsec_atime

  interface conv_date_cday
     module procedure conv_date_cday_i,  conv_date_cday_c
     module procedure conv_adate_cday_i, conv_adate_cday_c
  end interface conv_date_cday

  interface conv_adate_cday
     module procedure conv_adate_cday_i, conv_adate_cday_c
  end interface conv_adate_cday

  interface conv_cday_date
     module procedure conv_cday_date_i,  conv_cday_date_c
  end interface conv_cday_date

  interface conv_cday_adate
     module procedure conv_cday_adate_i, conv_cday_adate_c
  end interface conv_cday_adate

  interface conv_date_dayy
     module procedure conv_date_dayy, conv_adate_dayy
  end interface conv_date_dayy

  interface conv_date_dayy_compat
     module procedure conv_date_dayy_compat, conv_adate_dayy_compat
  end interface conv_date_dayy_compat

  interface conv_calendar_csec
     module procedure conv_calendar_csec_c, conv_acalendar_csec_c
  end interface conv_calendar_csec

  interface conv_calendar_string
     module procedure conv_calendar_string_core, conv_acalendar_string
  end interface conv_calendar_string

  interface advance_csec
     module procedure advance_csec_c
  end interface advance_csec

!!!_  - public
  public :: init, diag, finalize
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
!!!_  - static
  integer,save :: lcals = -1, mcals = -1
  type(cal_attr_t),allocatable :: CALH(:)

  integer,parameter,private :: jglobal = 0
!!!_ + common procedures
contains
!!!_  & init - initialization and (optional) global calendar config
  subroutine init &
       & (ierr,  ulog, &
       &  ncals, mode, auto)
#   define __PROC__ 'init'
    use TOUZA_Cal_primitive,only: msg
    use TOUZA_Cal_core,only: core_init => init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: ulog
    integer,intent(in),optional :: ncals
    integer,intent(in),optional :: mode
    logical,intent(in),optional :: auto
    integer jdummy

    ierr = 0
    if (ierr.eq.0) call core_init(ierr, ulog)
    if (lcals.gt.0) then
       ierr = -1
       call logging(ierr, __PROC__, 'reinit', ulog)
       return
    endif
#ifdef PACKAGE_CAL_STRING
    if (ierr.eq.0) call msg(PACKAGE_CAL_STRING, 'CAL', 0, ulog)
#endif
#ifndef   PACKAGE_STRING
#  define PACKAGE_STRING 'cal 0.000'
#endif
    if (ierr.eq.0) call msg(PACKAGE_STRING, 'CAL', 0, ulog)
    if (ierr.eq.0) call msg(TIME_STAMP, 'CAL', 0, ulog)

    lcals = 0 ! to mark initialized

    ! number of calendars
    if (present(ncals)) then
       if (ierr.eq.0) call alloc(ierr, ncals)
    else if (present(mode).or.present(auto)) then
       if (ierr.eq.0) call alloc(ierr, 0)
    endif
    ! global calendar declaration
    if (present(mode).or.present(auto)) then
       if (ierr.eq.0) then
          call new_calendar(ierr, mode, auto, ulog, jdummy)
          if (jdummy.ne.jglobal) ierr = -1
       endif
    endif
    return
#   undef __PROC__
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    ierr = 0
    if (present(u)) continue    ! dummy
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    ierr = 0
    if (present(u)) continue    ! dummy
    return
  end subroutine finalize

!!!_   & logging
  subroutine logging &
       & (istat, proc, txt, ulog)
    use TOUZA_Cal_primitive,only: msg
    integer,         intent(in)          :: istat
    character(len=*),intent(in)          :: proc
    character(len=*),intent(in)          :: txt
    integer,         intent(in),optional :: ulog
    call msg(txt, proc, istat, ulog)
    return
  end subroutine logging

!!!_   & alloc
  subroutine alloc &
       & (ierr, ncals)
#   define __PROC__ 'alloc'
    use TOUZA_Cal_core,only: core_init => init
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: ncals
    ierr = 0
    if (lcals.lt.0) then
       ierr = -1
       call logging(ierr, __PROC__, 'Not initialized.')
       return
    endif
    if (lcals.gt.0) then
       ierr = -2
       call logging(ierr, __PROC__, 'Already allocated.')
       return
    endif
    lcals = max(ncals, OPT_CALENDAR_HANDLES, 1)
    mcals = 0

    allocate(CALH(0:lcals-1), STAT=ierr)
    if (ierr.ne.0) then
       call logging(ierr, __PROC__, 'While allocation')
       ierr = -3
    endif
    return
#   undef __PROC__
  end subroutine alloc

!!!_   & new_calendar - decleare new calendar and assign id
  subroutine new_calendar &
       & (ierr, mode, auto, ulog, jcalh)
#   define __PROC__ 'new_calendar'
    use TOUZA_Cal_core,only: decl_cal
    implicit none
    integer,intent(out)          :: ierr
    integer,intent(in), optional :: mode
    logical,intent(in), optional :: auto
    integer,intent(in), optional :: ulog
    integer,intent(out),optional :: jcalh
    integer jc

    ierr = 0
    jc = mcals
    mcals = mcals + 1
    if (mcals.gt.lcals) then
       ierr = -1
       call logging(ierr, __PROC__, 'overflow')
    endif

    call decl_cal(CALH(jc), mode=mode, auto=auto, ulog=ulog)

    if (present(jcalh)) then
       if (ierr.eq.0) then
          jcalh = jc
       else
          jcalh = -1
       endif
    endif
    return
#   undef __PROC__
  end subroutine new_calendar

!!!_   & set_perpetual_date
  subroutine set_perpetual_date_t &
       & (cd, p_set, jcalh)
    implicit none
    type(cal_date_t),intent(in)          :: cd
    logical,         intent(in),optional :: p_set
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    call set_perpetual_date_core(CALH(jc), cd, p_set)
    return
  end subroutine set_perpetual_date_t

!!!_   & set_perpetual_adate
  subroutine set_perpetual_adate &
       & (y, m, d, p_set, jcalh)
    use TOUZA_Cal_core,only: cal_date_t
    implicit none
    integer,intent(in)          :: y, m, d
    logical,intent(in),optional :: p_set
    integer,intent(in),optional :: jcalh
    call set_perpetual_date_t(cal_date_t(y,m,d), p_set, jcalh)
    return
  end subroutine set_perpetual_adate

!!!_ + user procedures
!!!_  & get_perpetual_date
  subroutine get_perpetual_date_t &
       & (cd, p_set, jcalh)
    implicit none
    type(cal_date_t),intent(out)          :: cd
    logical,         intent(out)          :: p_set
    integer,         intent(in), optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    call get_perpetual_date_core(CALH(jc), cd, p_set)
    return
  end subroutine get_perpetual_date_t

!!!_  & get_perpetual_adate
  subroutine get_perpetual_adate &
       & (y, m, d, p_set, jcalh)
    implicit none
    integer,intent(out)          :: y, m, d
    logical,intent(out)          :: p_set
    integer,intent(in), optional :: jcalh
    type(cal_date_t)  :: p_date
    call get_perpetual_date_t(p_date, p_set, jcalh)
    y = p_date%y
    m = p_date%m
    d = p_date%d
    return
  end subroutine get_perpetual_adate

!!!_  & set_perpetual_switch
  subroutine set_perpetual_switch &
       & (sw, jcalh)
    use TOUZA_Cal_core,only: set_perpetual_switch_core => set_perpetual_switch
    implicit none
    logical,intent(in)          :: sw
    integer,intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    call set_perpetual_switch_core(CALH(jc), sw)
    return
  end subroutine set_perpetual_switch

!!!_  & inq_nday_month ()
  integer function inq_nday_month_t &
       & (cd, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: inq_nday_month_core => inq_nday_month
    implicit none
    type(cal_date_t),intent(in)          :: cd
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = inq_nday_month_core(CALH(jc), cd)
    return
  end function inq_nday_month_t

  integer function inq_nday_amonth &
       & (ym, jcalh) &
       & result (r)
    implicit none
    integer,intent(in)          :: ym(2)
    integer,intent(in),optional :: jcalh
    r = inq_nday_month_t(cal_date_t(ym(1), ym(2), 0), jcalh)
    return
  end function inq_nday_amonth

!!!_  & inq_nday_year ()
  integer function inq_nday_year_t &
       & (cd, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: inq_nday_year_core => inq_nday_year
    implicit none
    type(cal_date_t),intent(in)          :: cd
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = inq_nday_year_core(CALH(jc), cd)
    return
  end function inq_nday_year_t

  integer function inq_nday_ayear &
       & (y, jcalh) &
       & result (r)
    implicit none
    integer,intent(in)          :: y
    integer,intent(in),optional :: jcalh
    r = inq_nday_year_t(cal_date_t(y, 0, 0), jcalh)
    return
  end function inq_nday_ayear

!!!_  & inq_nmonth_year ()
  integer function inq_nmonth_year_t &
       & (cd, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: inq_nmonth_year_core => inq_nmonth_year
    implicit none
    type(cal_date_t),intent(in)          :: cd
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = inq_nmonth_year_core(CALH(jc), cd)
    return
  end function inq_nmonth_year_t

  integer function inq_nmonth_ayear &
       & (y, jcalh) &
       & result (r)
    implicit none
    integer,intent(in)          :: y
    integer,intent(in),optional :: jcalh
    r = inq_nmonth_year_t(cal_date_t(y, 0, 0), jcalh)
    return
  end function inq_nmonth_ayear

!!!_  & inq_nsec_day ()
  integer function inq_nsec_day_t &
       & (cd, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: inq_nsec_day_core => inq_nsec_day
    implicit none
    type(cal_date_t),intent(in)          :: cd
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = inq_nsec_day_core (CALH(jc), cd)
    return
  end function inq_nsec_day_t

  integer function inq_nsec_aday &
       & (jcalh) &
       & result (r)
    implicit none
    integer, intent(in),optional :: jcalh
    r = inq_nsec_day_t (cal_date_t(0, 0, 0), jcalh)
    return
  end function inq_nsec_aday

!!!_  & inq_nsec_minute ()
  integer function inq_nsec_minute_t &
       & (cd, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: inq_nsec_minute_core => inq_nsec_minute
    implicit none
    type(cal_date_t),intent(in)          :: cd
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = inq_nsec_minute_core(CALH(jc), cd)
    return
  end function inq_nsec_minute_t

  integer function inq_nsec_aminute &
       & (jcalh) &
       & result (r)
    implicit none
    integer,intent(in),optional :: jcalh
    r = inq_nsec_minute_t(cal_date_t(0, 0, 0), jcalh)
    return
  end function inq_nsec_aminute

!!!_  & inq_nsec_hour ()
  integer function inq_nsec_hour_t &
       & (cd, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: inq_nsec_hour_core => inq_nsec_hour
    implicit none
    type(cal_date_t),intent(in)          :: cd
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = inq_nsec_hour_core(CALH(jc), cd)
    return
  end function inq_nsec_hour_t

  integer function inq_nsec_ahour &
       & (jcalh) &
       & result (r)
    implicit none
    integer,intent(in),optional :: jcalh
    r = inq_nsec_hour_t(cal_date_t(0, 0, 0), jcalh)
    return
  end function inq_nsec_ahour

!!!_  & conv_csec_cdaysec() - calendar[sec] to calendar[day+sec]
  type(cal_daysec_t) function conv_csec_cdaysec &
       &  (csec, cd, jcalh) &
       result (r)
    use TOUZA_Cal_core,only: &
         & conv_csec_cdaysec_core => conv_csec_cdaysec
    implicit none
    real(kind=KRC),  intent(in)          :: csec
    type(cal_date_t),intent(in),optional :: cd
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_csec_cdaysec_core (CALH(jc), csec, cd)
    return
  end function conv_csec_cdaysec

!!!_  & conv_csec_adaysec - calendar[sec] to calendar[day+sec] array
  subroutine conv_csec_adaysec &
       &  (cday, rsec, csec, jcalh)
    use TOUZA_Cal_core,only: &
         & conv_csec_cdaysec_core => conv_csec_cdaysec
    implicit none
    integer,       intent(out)         :: cday
    real(kind=KRC),intent(out)         :: rsec
    real(kind=KRC),intent(in)          :: csec
    integer,       intent(in),optional :: jcalh
    type(cal_daysec_t) :: ds
    integer jc
    jc = check_id(jcalh)
    ds = conv_csec_cdaysec_core (CALH(jc), csec, cal_date_t(0, 0, 0))
    cday = ds%d
    rsec = ds%s
    return
  end subroutine conv_csec_adaysec

!!!_  & conv_cdaysec_csec()
  real(kind=KRC) function conv_cdaysec_csec_c &
       & (ds, xk, cd, jcalh) &
       result (r)
    use TOUZA_Cal_core,only: conv_cdaysec_csec_core => conv_cdaysec_csec
    implicit none
    type(cal_daysec_t),intent(in)          :: ds
    real(kind=KRC),    intent(in)          :: xk ! dummy
    type(cal_date_t),  intent(in),optional :: cd
    integer,           intent(in),optional :: jcalh

    integer jc
    jc = check_id(jcalh)
    r = conv_cdaysec_csec_core(CALH(jc), ds, cd, xk)
    return
  end function conv_cdaysec_csec_c

  integer function conv_cdaysec_csec_i &
       & (ds, xk, cd, jcalh) &
       result (r)
    use TOUZA_Cal_core,only: conv_cdaysec_csec_core => conv_cdaysec_csec
    implicit none
    type(cal_daysec_t),intent(in)          :: ds
    integer,           intent(in)          :: xk ! dummy
    type(cal_date_t),  intent(in),optional :: cd
    integer,           intent(in),optional :: jcalh

    integer jc
    jc = check_id(jcalh)
    r = conv_cdaysec_csec_core(CALH(jc), ds, cd, xk)
    return
  end function conv_cdaysec_csec_i

!!!_  & conv_adaysec_csec() - calendar[day+sec] array to calendar[sec]
  real(kind=KRC) function conv_adaysec_csec_c &
       & (cday, rsec, xk, jcalh) &
       result (r)
    use TOUZA_Cal_core,only: conv_cdaysec_csec_core => conv_cdaysec_csec
    implicit none
    integer,       intent(in)          :: cday
    real(kind=KRC),intent(in)          :: rsec
    real(kind=KRC),intent(in)          :: xk ! dummy
    integer,       intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_cdaysec_csec_core &
         & (CALH(jc), cal_daysec_t(cday, rsec), cal_date_t(0, 0, 0), xk)
    return
  end function conv_adaysec_csec_c

  integer function conv_adaysec_csec_i &
       & (cday, rsec, xk, jcalh) &
       result (r)
    use TOUZA_Cal_core,only: conv_cdaysec_csec_core => conv_cdaysec_csec
    implicit none
    integer,       intent(in)          :: cday
    real(kind=KRC),intent(in)          :: rsec
    integer,       intent(in)          :: xk ! dummy
    integer,       intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_cdaysec_csec_core &
         & (CALH(jc), cal_daysec_t(cday, rsec), cal_date_t(0, 0, 0), xk)
    return
  end function conv_adaysec_csec_i

!!!_  & conv_tsec_time() - time[sec] to time[hour min sec]
  type(cal_time_t) function conv_tsec_time_c &
       & (tsec, cd, jcalh) &
       result (r)
    use TOUZA_Cal_core,only: &
         & conv_tsec_time_core => conv_tsec_time
    implicit none
    real(kind=KRC),  intent(in)          :: tsec
    type(cal_date_t),intent(in),optional :: cd
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_tsec_time_core(CALH(jc), tsec, cd)
    return
  end function conv_tsec_time_c

  type(cal_time_t) function conv_tsec_time_i &
       & (tsec, cd, jcalh) &
       result (r)
    use TOUZA_Cal_core,only: &
         & conv_tsec_time_core => conv_tsec_time
    implicit none
    integer,         intent(in)          :: tsec
    type(cal_date_t),intent(in),optional :: cd
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_tsec_time_core(CALH(jc), tsec, cd)
    return
  end function conv_tsec_time_i

!!!_  & conv_tsec_atime() - time[sec] to time[hour min sec] array
  function conv_tsec_atime_c &
       & (tsec, jcalh) &
       & result(at)
    use TOUZA_Cal_core,only: &
         & conv_tsec_time_core => conv_tsec_time
    implicit none
    integer at(3)
    real(kind=KRC),intent(in)          :: tsec
    integer,       intent(in),optional :: jcalh
    type(cal_time_t) :: ct
    integer jc
    jc = check_id(jcalh)
    ct = conv_tsec_time_core(CALH(jc), tsec, cal_date_t(0, 0, 0))
    at(1:3) = (/ct%h, ct%m, ct%s/)
    return
  end function conv_tsec_atime_c

  function conv_tsec_atime_i &
       & (tsec, jcalh) &
       & result(at)
    use TOUZA_Cal_core,only: &
         & conv_tsec_time_core => conv_tsec_time
    implicit none
    integer at(3)
    integer,intent(in)          :: tsec
    integer,intent(in),optional :: jcalh
    type(cal_time_t) :: ct
    integer jc
    jc = check_id(jcalh)
    ct = conv_tsec_time_core(CALH(jc), tsec, cal_date_t(0, 0, 0))
    at(1:3) = (/ct%h, ct%m, ct%s/)
    return
  end function conv_tsec_atime_i

!!!_  & conv_time_tsec() - time[hour min sec] to time[sec]
  integer function conv_time_tsec_t &
       & (t, cd, jcalh) &
       result (r)
    use TOUZA_Cal_core,only: conv_time_tsec_core => conv_time_tsec
    implicit none
    type(cal_time_t),intent(in)          :: t
    type(cal_date_t),intent(in),optional :: cd
    integer,         intent(in),optional :: jcalh
    integer jc
    integer xk
    jc = check_id(jcalh)
    r = conv_time_tsec_core (CALH(jc), t, cd, xk)
    return
  end function conv_time_tsec_t

  integer function conv_atime_tsec &
       & (hms, jcalh) &
       result (r)
    implicit none
    integer,intent(in)          :: hms(3)
    integer,intent(in),optional :: jcalh
    r = conv_time_tsec_t &
         & (cal_time_t(hms(1), hms(2), hms(3)), cal_date_t(0, 0, 0), jcalh)
    return
  end function conv_atime_tsec

!!!_  & conv_cday_date() - calendar[day] to calendar[D]
  type(cal_date_t) function conv_cday_date_i &
       & (cday, jcalh) &
       result (r)
    use TOUZA_Cal_core,only: &
         & conv_cday_date_core => conv_cday_date
    implicit none
    integer,intent(in)          :: cday
    integer,intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_cday_date_core(CALH(jc), cday)
    return
  end function conv_cday_date_i

  type(cal_date_t) function conv_cday_date_c &
       & (cday, jcalh) &
       result (r)
    use TOUZA_Cal_core,only: &
         & conv_cday_date_core => conv_cday_date
    implicit none
    real(kind=KRC),intent(in)          :: cday
    integer,       intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_cday_date_core(CALH(jc), cday)
    return
  end function conv_cday_date_c

!!!_  & conv_cday_adate() - calendar[day] to calendar[D] array
  function conv_cday_adate_i &
       & (cday, jcalh) result (ad)
    implicit none
    integer ad(3)
    integer,intent(in)          :: cday
    integer,intent(in),optional :: jcalh
    type(cal_date_t) :: cd
    cd = conv_cday_date(cday, jcalh)
    ad(1:3) = (/cd%y, cd%m, cd%d/)
    return
  end function conv_cday_adate_i

  function conv_cday_adate_c &
       & (cday, jcalh) result (ad)
    implicit none
    integer ad(3)
    real(kind=KRC),intent(in)          :: cday
    integer,       intent(in),optional :: jcalh
    type(cal_date_t) :: cd
    cd = conv_cday_date(cday, jcalh)
    ad(1:3) = (/cd%y, cd%m, cd%d/)
    return
  end function conv_cday_adate_c

!!!_  & conv_date_cday() - calendar[D] to calendar[day]
  integer function conv_date_cday_i &
       & (cd, xk, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: conv_date_cday_core => conv_date_cday
    implicit none
    type(cal_date_t),intent(in)          :: cd
    integer,         intent(in)          :: xk ! dummy
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_date_cday_core(CALH(jc), cd, xk)
    return
  end function conv_date_cday_i

  real(kind=KRC) function conv_date_cday_c &
       & (cd, xk, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: conv_date_cday_core => conv_date_cday
    implicit none
    type(cal_date_t),intent(in)          :: cd
    real(kind=KRC),  intent(in)          :: xk
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_date_cday_core(CALH(jc), cd, xk)
    return
  end function conv_date_cday_c

!!!_  & conv_adate_cday() - calendar[D] array to calendar[day]
  integer function conv_adate_cday_i &
       & (ymd, xk, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: conv_date_cday_core => conv_date_cday
    implicit none
    integer,intent(in)          :: ymd(3)
    integer,intent(in)          :: xk ! dummy
    integer,intent(in),optional :: jcalh

    integer jc
    jc = check_id(jcalh)
    r = conv_date_cday_core &
         & (CALH(jc), cal_date_t(ymd(1), ymd(2), ymd(3)), xk)
    return
  end function conv_adate_cday_i

  real(kind=KRC) function conv_adate_cday_c &
       & (ymd, xk, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: conv_date_cday_core => conv_date_cday
    implicit none
    integer,       intent(in)          :: ymd(3)
    real(kind=KRC),intent(in)          :: xk
    integer,       intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_date_cday_core &
         & (CALH(jc), cal_date_t(ymd(1), ymd(2), ymd(3)), xk)
    return
  end function conv_adate_cday_c

!!!_  & conv_date_dayy() - calendar[D] to calendar[day in year]
  integer function conv_date_dayy &
       & (cd, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: conv_date_dayy_core => conv_date_dayy
    implicit none
    type(cal_date_t),intent(in)          :: cd
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_date_dayy_core(CALH(jc), cd)
    return
  end function conv_date_dayy

  integer function conv_adate_dayy &
       & (ymd, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: conv_date_dayy_core => conv_date_dayy
    implicit none
    integer,intent(in)          :: ymd(3)
    integer,intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_date_dayy_core &
         & (CALH(jc), cal_date_t(ymd(1), ymd(2), ymd(3)))
    return
  end function conv_adate_dayy

!!!_  & conv_date_dayy_compat()
  integer function conv_date_dayy_compat &
       & (cd, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: conv_date_dayy_compat_core => conv_date_dayy_compat
    implicit none
    type(cal_date_t),intent(in)          :: cd
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_date_dayy_compat_core(CALH(jc), cd)
    return
  end function conv_date_dayy_compat

  integer function conv_adate_dayy_compat &
       & (ymd, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: conv_date_dayy_compat_core => conv_date_dayy_compat
    implicit none
    integer,intent(in)          :: ymd(3)
    integer,intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_date_dayy_compat_core &
         & (CALH(jc), cal_date_t(ymd(1), ymd(2), ymd(3)))
    return
  end function conv_adate_dayy_compat

!!!_  & conv_csec_calendar() - calendar[sec] to calendar[D,T]
  type(calendar_t) function conv_csec_calendar &
       & (csec, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: &
         & conv_csec_calendar_core => conv_csec_calendar
    implicit none
    real(kind=KRC),intent(in)          :: csec
    integer,       intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_csec_calendar_core(CALH(jc), csec)
    return
  end function conv_csec_calendar

!!!_  & conv_csec_acalendar() - calendar[sec] to calendar[D,T] array
  function conv_csec_acalendar &
       & (csec, jcalh) &
       & result(ac)
    use TOUZA_Cal_core,only: &
         & conv_csec_calendar_core => conv_csec_calendar
    implicit none
    integer ac(6)
    real(kind=KRC),intent(in)          :: csec
    integer,       intent(in),optional :: jcalh
    type(calendar_t) :: cc
    integer jc
    jc = check_id(jcalh)
    cc = conv_csec_calendar_core(CALH(jc), csec)
    ac(1:3) = (/cc%d%y, cc%d%m, cc%d%d/)
    ac(4:6) = (/cc%t%h, cc%t%m, cc%t%s/)
    return
  end function conv_csec_acalendar

!!!_  & conv_calendar_csec() - calendar[D,T] to calendar[sec]
  real(kind=KRC) function conv_calendar_csec_c &
       & (cc, xk, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: conv_calendar_csec_core => conv_calendar_csec
    implicit none
    type(calendar_t),intent(in)          :: cc
    real(kind=KRC),  intent(in)          :: xk ! dummy
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_calendar_csec_core(CALH(jc), cc, xk)
    return
  end function conv_calendar_csec_c

!!!_  & conv_acalendar_csec() - calendar[D,T] array to calendar[sec]
  real(kind=KRC) function conv_acalendar_csec_c &
       & (y,mo,d,h,mi,s, xk, jcalh) &
       & result (r)
    implicit none
    integer,       intent(in)          :: y, mo, d
    integer,       intent(in)          :: h, mi, s
    real(kind=KRC),intent(in)          :: xk ! dummy
    integer,       intent(in),optional :: jcalh

    type(calendar_t) :: cc
    cc%d%y = y
    cc%d%m = mo
    cc%d%d = d
    cc%t%h = h
    cc%t%m = mi
    cc%t%s = s
    r = conv_calendar_csec_c(cc, xk, jcalh)
    return
  end function conv_acalendar_csec_c

!!!_  & conv_cday_cydayy() - calendar[day] to calendar[Y+day]
  type(cal_ynday_t) function conv_cday_cydayy &
       & (cday, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: &
         & conv_cday_cydayy_core => conv_cday_cydayy
    implicit none
    integer,intent(in)          :: cday
    integer,intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_cday_cydayy_core(CALH(jc), cday)
    return
  end function conv_cday_cydayy

!!!_  & conv_cday_aydayy() - calendar[day] to calendar[year+day] array
  function conv_cday_aydayy &
       & (cday, jcalh) &
       & result(ayd)
    use TOUZA_Cal_core,only: &
         & conv_cday_cydayy_core => conv_cday_cydayy
    implicit none
    integer ayd(2)
    integer,intent(in)          :: cday
    integer,intent(in),optional :: jcalh
    type(cal_ynday_t) :: cy
    integer jc
    jc = check_id(jcalh)
    cy = conv_cday_cydayy_core(CALH(jc), cday)
    ayd(1:2) = (/cy%y, cy%d/)
    return
  end function conv_cday_aydayy

!!!_  & conv_csec_date() - calendar[sec] to calendar[D]
  type(cal_date_t) function conv_csec_date &
       & (dsec, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: &
         & conv_csec_date_core => conv_csec_date
    implicit none
    real(kind=KRC),intent(in)          :: dsec
    integer,       intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_csec_date_core(CALH(jc), dsec)
    return
  end function conv_csec_date

!!!_  & conv_csec_adate() - calendar[sec] to calendar[D] array
  function conv_csec_adate &
       & (dsec, jcalh) &
       & result(ad)
    use TOUZA_Cal_core,only: &
         & conv_csec_date_core => conv_csec_date
    implicit none
    integer :: ad(3)
    real(kind=KRC),intent(in)          :: dsec
    integer,       intent(in),optional :: jcalh
    type(cal_date_t) :: cd
    integer jc
    jc = check_id(jcalh)
    cd = conv_csec_date_core(CALH(jc), dsec)
    ad(1:3) = (/cd%y, cd%m, cd%d/)
    return
  end function conv_csec_adate

!!!_  & conv_csec_cydayy() - calendar[sec] to calendar[year+day]
  type(cal_ynday_t) function conv_csec_cydayy &
       & (csec, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: &
         & conv_csec_cydayy_core => conv_csec_cydayy
    implicit none
    real(kind=KRC),intent(in)          :: csec
    integer,       intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_csec_cydayy_core(CALH(jc), csec)
    return
  end function conv_csec_cydayy

!!!_  & conv_csec_aydayy() - calendar[sec] to calendar[year+day] array
  function conv_csec_aydayy &
       & (csec, jcalh) &
       & result(ayd)
    use TOUZA_Cal_core,only: &
         & conv_csec_cydayy_core => conv_csec_cydayy
    implicit none
    integer ayd(2)
    real(kind=KRC),intent(in)          :: csec
    integer,       intent(in),optional :: jcalh
    type(cal_ynday_t) :: cy
    integer jc
    jc = check_id(jcalh)
    cy = conv_csec_cydayy_core(CALH(jc), csec)
    ayd(1:2) = (/cy%y, cy%d/)
    return
  end function conv_csec_aydayy

!!!_  & conv_duration_sec() - unit-conversion from duration[unit] to seconds
  real(kind=KRC) function conv_duration_sec &
       & (dur, unit, refsec, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: &
         & conv_duration_sec_core => conv_duration_sec
    implicit none
    real(kind=KRC),  intent(in)          :: dur
    character(len=*),intent(in)          :: unit
    real(kind=KRC),  intent(in)          :: refsec
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = conv_duration_sec_core(CALH(jc), dur, unit, refsec)
    return
  end function conv_duration_sec

!!!_  & conv_csec_string_ppt_off()
  subroutine conv_csec_string_ppt_off &
       (str, csec, jcalh)
    use TOUZA_Cal_core,only: &
         & conv_csec_string_ppt_off_core => conv_csec_string_ppt_off
    implicit none
    character(len=*),intent(out)         :: str
    real(kind=KRC),  intent(in)          :: csec
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    call conv_csec_string_ppt_off_core(CALH(jc), str, csec)
    return
  end subroutine conv_csec_string_ppt_off

!!!_   & conv_string_acalendar ()
  function conv_string_acalendar &
       & (str) &
       & result(ac)
    implicit none
    integer  ac(6)
    character(len=*),intent(in)  :: str
    type(calendar_t) :: cc
    cc = conv_string_calendar(str)
    ac(1:3) = (/cc%d%y, cc%d%m, cc%d%d/)
    ac(4:6) = (/cc%t%h, cc%t%m, cc%t%s/)
  end function conv_string_acalendar

!!!_   & conv_acalendar_string ()
  subroutine conv_acalendar_string &
       & (str, y,mo,d,h,mi,s)
    implicit none
    character(len=*),intent(out) :: str
    integer,         intent(in)  :: y,mo,d,h,mi,s
    type(calendar_t) :: cc
    cc%d%y = y
    cc%d%m = mo
    cc%d%d = d
    cc%t%h = h
    cc%t%m = mi
    cc%t%s = s
    call conv_calendar_string_core(str, cc)
  end subroutine conv_acalendar_string

!!!_  & advance_csec() - calendar[sec] advance by duration[unit]
  real(kind=KRC) function advance_csec_c &
       & (dur, unit, refsec, xk, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: &
         & advance_csec_core => advance_csec
    implicit none
    real(kind=KRC),  intent(in)          :: dur
    real(kind=KRC),  intent(in)          :: refsec
    character(len=*),intent(in)          :: unit
    real(kind=KRC),  intent(in)          :: xk ! dummy
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = advance_csec_core(CALH(jc), dur, unit, refsec, xk)
    return
  end function advance_csec_c

!!!_  & is_passed() - check if pass
  logical function is_passed &
       & (csec, refsec, orgsec, dur, unit, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: &
         & is_passed_core => is_passed
    implicit none
    real(kind=KRC),  intent(in)          :: csec, refsec, orgsec, dur
    character(len=*),intent(in)          :: unit
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = is_passed_core(CALH(jc), csec, refsec, orgsec, dur, unit)
    return
  end function is_passed

!!!_  & is_passed_compat() - check if pass deprecated implementation (different with is_passed())
  logical function is_passed_compat &
       & (csec, refsec, orgsec, dur, unit, jcalh) &
       & result (r)
    use TOUZA_Cal_core,only: &
         & is_passed_compat_core => is_passed_compat
    implicit none
    real(kind=KRC),  intent(in)          :: csec, refsec, orgsec, dur
    character(len=*),intent(in)          :: unit
    integer,         intent(in),optional :: jcalh
    integer jc
    jc = check_id(jcalh)
    r = is_passed_compat_core(CALH(jc), csec, refsec, orgsec, dur, unit)
    return
  end function is_passed_compat

!!!_ + internal procedures
!!!_  & check_id()
  integer function check_id (jcalh) result(r)
    implicit none
    integer,intent(in),optional :: jcalh
    if (present(jcalh)) then
       r = jcalh
    else
       r = jglobal
    endif
    if (r.lt.jglobal.or.r.gt.mcals) r = -1
    return
  end function check_id


!!!_ + end module
end module TOUZA_Cal

!!!_@ test_calendar - test program
#ifdef TEST_CALENDAR
program test_calendar
  use TOUZA_Cal
  implicit none
  stop
end program test_calendar
#endif /* TEST_CALENDAR */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

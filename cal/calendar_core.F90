!!!_! calendar_core.F90 - TOUZA/Cal core
! Maintainer: SAITO Fuyuki
! Created: Fri Jul 25 2011
#define TIME_STAMP 'Time-stamp: <2023/03/25 13:27:04 fuyuki calendar_core.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2011-2023
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
#include "touza.h"
!!!_* Macros
#ifdef TEST_CALENDAR_CORE
#  define DEBUG *
#endif
#define OPT_USE_DGAUS_PRIVATE
#define DEFAULT_LOG_UNIT -1
#ifndef   OPT_CALENDAR_FIXED_YEAR_DIGITS
#  define OPT_CALENDAR_FIXED_YEAR_DIGITS 0  /* 4-digits year as legacy */
#endif
!!!_@ calendar_core - calendar core
module TOUZA_Cal_core
!!!_ = declaration
!!!_  - modules
  use TOUZA_Cal_primitive,only: &
       & KRC,        XREAL,      &
       & cal_date_t, cal_time_t, cal_daysec_t, &
       & p_error,    p_ideal,    p_grego_i,    p_grego_l, p_user, &
       & is_leap_year, &
       & trace_control, trace_fine, &
       & control_mode,  control_deep, is_first_force
!!!_  - private
  implicit none

  private
!!!_  - public
  public :: init, diag, finalize
  public :: decl_cal
  public :: set_perpetual_date,   get_perpetual_date,   set_perpetual_switch
  public :: inq_nday_month,       inq_nday_year,        inq_nmonth_year
  public :: inq_nsec_day,         inq_nsec_minute,      inq_nsec_hour
  public :: conv_cdaysec_csec,    conv_csec_cdaysec
  public :: conv_time_tsec,       conv_tsec_time
  public :: conv_cday_date,       conv_date_cday
  public :: conv_date_dayy,       conv_date_dayy_compat
  public :: conv_csec_calendar,   conv_calendar_csec
  public :: conv_cday_cydayy,     conv_csec_date,       conv_csec_cydayy
  public :: advance_csec,         conv_duration_sec
  public :: conv_string_calendar, conv_calendar_string, conv_csec_string_ppt_off
  public :: is_passed,            is_passed_compat

  public :: cal_attr_t,           cal_perpetual_t

!!!_   . calendar_primitive
  public :: KRC,        XREAL
  public :: cal_date_t, cal_time_t, cal_daysec_t
  public :: p_error,    p_ideal,    p_grego_i,   p_grego_l, p_user
  public :: is_leap_year
!!!_  * parameters
  character(len=*),parameter :: fmt_calendar_w = '(I0.4,''/'',I2.2,''/'',I2.2,''-'',I2.2,'':'',I2.2,'':'',I2.2)'
#if OPT_CALENDAR_FIXED_YEAR_DIGITS
  character(len=*),parameter :: fmt_calendar_r = '(I4.4,1X,   I2.2,1X,   I2.2,1X,   I2.2,1X,   I2.2,1X,   I2.2)'
#endif

  integer,parameter :: auto_cyear_grego_l = 1900
  integer,parameter :: auto_cyear_grego_i = 1000
  integer,parameter :: auto_cday_grego_l  = auto_cyear_grego_l * 365 + auto_cyear_grego_l / 4 - 19 + 5
  integer,parameter :: auto_cday_grego_i  = auto_cyear_grego_i * 365

  integer,parameter,public :: iunit_invalid = 0
  integer,parameter,public :: iunit_YR = 1
  integer,parameter,public :: iunit_MO = 2
  integer,parameter,public :: iunit_DY = 3
  integer,parameter,public :: iunit_HR = 4
  integer,parameter,public :: iunit_MI = 5
  integer,parameter,public :: iunit_SC = 6

  character(len=*),parameter,public :: unit_YR = 'Y'
  character(len=*),parameter,public :: unit_MO = 'MO'
  character(len=*),parameter,public :: unit_DY = 'D'
  character(len=*),parameter,public :: unit_HR = 'H'
  character(len=*),parameter,public :: unit_MI = 'MI'
  character(len=*),parameter,public :: unit_SC = 'S'

  integer,parameter,public :: auto_false = 0   ! no-auto mode
  integer,parameter,public :: auto_once  = 1   ! one-time auto-mode
  integer,parameter,public :: auto_every = -1  ! legacy every step auto-mode

!!!_  - static
  integer,save :: global_id = -1
  integer,save :: ulog = DEFAULT_LOG_UNIT
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = CAL_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT

#define __MDL__ 'c'
!!!_  - misc
    character(len=1024) tmsg
!!!_  * cal_perpetual_t - perpetual mode attributes
  type cal_perpetual_t
     ! logical    :: toggle = .false.
     logical          :: toggle = .true.
     logical          :: set    = .false.
     type(cal_date_t) :: date
  end type cal_perpetual_t

!!!_  * cal_attr_t - calendar attributes
  type cal_attr_t
     integer :: id    = -1
     integer :: stt   = -1
     integer :: mode  = p_error
     integer :: auto  = auto_false
     integer :: ulog  = DEFAULT_LOG_UNIT
     logical :: angular = .false.

     type(cal_perpetual_t) :: pt
  end type cal_attr_t

!!!_  * cal_ynday_t
  type cal_ynday_t
     integer :: y
     integer :: d
  end type cal_ynday_t
  public :: cal_ynday_t

!!!_  * calendar_t
  type calendar_t
     type(cal_date_t) :: d
     type(cal_time_t) :: t
  end type calendar_t
  public :: calendar_t

!!!_  - interfaces
#ifdef OPT_USE_DGAUS_PRIVATE
  interface dgaus
     module procedure dgaus_private
  end interface dgaus
#endif

  interface conv_tsec_time
     module procedure conv_tsec_time_i, conv_tsec_time_c
  end interface conv_tsec_time

  interface conv_time_tsec
     module procedure conv_time_tsec_i, conv_time_tsec_c
  end interface conv_time_tsec

  interface conv_date_cday
     module procedure conv_date_cday_i, conv_date_cday_c
  end interface conv_date_cday

  interface conv_cday_date
     module procedure conv_cday_date_i, conv_cday_date_c
  end interface conv_cday_date

  interface check_mode_cday
     module procedure check_mode_cday_i, check_mode_cday_c
  end interface check_mode_cday

  interface conv_cdaysec_csec
     module procedure conv_cdaysec_csec_i, conv_cdaysec_csec_c
  end interface conv_cdaysec_csec

  interface conv_csec_cdaysec
     module procedure conv_csec_cdaysec_i, conv_csec_cdaysec_c
  end interface conv_csec_cdaysec

  interface conv_calendar_csec
     module procedure conv_calendar_csec_i, conv_calendar_csec_c
  end interface conv_calendar_csec

  interface conv_csec_calendar
     module procedure conv_csec_calendar_i, conv_csec_calendar_c
  end interface conv_csec_calendar

  interface conv_duration_sec
     module procedure conv_uduration_sec
     module procedure conv_jduration_sec
  end interface conv_duration_sec

  interface advance_csec
     module procedure advancej_csec_c
     module procedure advanceu_csec_c
  end interface advance_csec

  interface is_passed
     module procedure is_jpassed
     module procedure is_upassed
  end interface is_passed

contains
!!!_ + common interfaces
!!!_  & init - initialization
  subroutine init (ierr, u, levv, mode, stdv)
    use TOUZA_Cal_primitive,only: primitive_init=>init, msg, choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    integer lv, md, lmd

    ierr = 0

    md = control_mode(mode, MODE_SHALLOW)
    init_mode = md

    if (md.ge.MODE_SURFACE) then
       err_default = ERR_SUCCESS
       lv = choice(lev_verbose, levv)
       if (is_first_force(init_counts, mode)) then
          ulog = choice(ulog, u)
          lev_verbose = lv
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call primitive_init(ierr, ulog, levv, mode=lmd, stdv=stdv)
       endif
       if (is_first_force(init_counts, mode)) then
          global_id = 0
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif

    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Cal_primitive,only: primitive_diag=>diag, &
         & choice, msg, msglev_normal, msglev_warning, &
         & control_mode, control_deep
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp
    integer lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) call msg(msglev_normal, TIME_STAMP, __MDL__, utmp)
          if (ierr.eq.0) then
101          format('auto year = ', I0, 1x, I0)
102          format('auto day = ', I0, 1x, I0)
             write(tmsg, 101) auto_cyear_grego_l, auto_cyear_grego_i
             call msg(msglev_normal, tmsg, __MDL__, utmp)
             write(tmsg, 102) auto_cday_grego_l, auto_cday_grego_i
             call msg(msglev_normal, tmsg, __MDL__, utmp)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call primitive_diag(ierr, utmp, levv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Cal_primitive,only: primitive_finalize=>finalize, choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call primitive_finalize (ierr, utmp, lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + calendar property interfaces
!!!_  & decl_cal - declaration of calendar type
  subroutine decl_cal &
       & (self, mode, auto, u)
    use TOUZA_Cal_primitive,only: msg, msglev_normal, msglev_warning
    implicit none
    type(cal_attr_t),intent(inout)      :: self
    integer,         intent(in),optional:: mode
    integer,         intent(in),optional:: auto
    integer,         intent(in),optional:: u

    integer :: old_id
    integer jerr
!!!_   . initialization
    if (global_id .lt. 0) call init(jerr)
!!!_   . self initialization
    old_id    = self%id
    self%stt  = self%stt + 1

    self%id   = global_id
    global_id = global_id + 1
    if (present(u)) then
       self%ulog = u
    else
       self%ulog = ulog
    endif

    if (present (mode)) then
       self%mode = mode
    else
       self%mode = p_ideal
    endif

    if (present (auto)) then
       self%auto = auto
       if (auto.ne.auto_false) self%mode = p_error
    else
       self%auto = auto_false
    endif

    if (old_id.ge.0 .and. self%stt.ge.0) then
101    format('reinit:', I0, 1x, I0, 1x, I0)
       write(tmsg, 101) old_id, self%id, self%stt
       call msg(msglev_warning, tmsg, __MDL__, self%ulog)
    endif

    if (present (u)) then
201    format('decl:', I0, ' = ', I0, 1x, I0, 1x, I0, 1x, I0)
       write(tmsg, 201) self%id, self%stt, self%mode, self%auto, self%ulog
       call msg(msglev_normal, tmsg, __MDL__, self%ulog)
    endif

    return
  end subroutine decl_cal

!!!_  & set_perpetual_date - set perpetual mode and date
  subroutine set_perpetual_date &
       & (self, p_date, p_set)
    implicit none
    type(cal_attr_t),intent(inout)       :: self
    type(cal_date_t),intent(in),optional :: p_date
    logical,         intent(in),optional :: p_set

    if (present (p_date)) then
       self % pt % date = p_date
       self % pt % set  = .true.
    else
       self % pt % set  = .false.
    endif
    if (present (p_set)) then
       self % pt % set = p_set
    endif

    return
  end subroutine set_perpetual_date

!!!_  & get_perpetual_date - get perpetual mode and date
  subroutine get_perpetual_date &
       & (self, p_date, p_set)
    implicit none
    type(cal_attr_t),intent(in)  :: self
    type(cal_date_t),intent(out) :: p_date
    logical,         intent(out) :: p_set

    p_date = self % pt % date
    p_set  = self % pt % set

    return
  end subroutine get_perpetual_date

!!!_  & set_perpetual_switch - set perpetual mode switch to activate
  subroutine set_perpetual_switch &
       & (self, sw)
    implicit none
    type(cal_attr_t),intent(inout) :: self
    logical,         intent(in)    :: sw
    self % pt % toggle = sw
    return
  end subroutine set_perpetual_switch

!!!_ + primitive procedures
!!!_  & inq_nday_month()
  integer function inq_nday_month &
       & (self, cd) &
       & result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_inq_nday_month => inq_nday_month
    implicit none
    type(cal_attr_t),intent(inout) :: self
    type(cal_date_t),intent(in)    :: cd
    integer :: mode
    mode = check_mode_year (self, cd % y)
    r = primitive_inq_nday_month (mode, cd)
  end function inq_nday_month

!!!_  & inq_nday_year()
  integer function inq_nday_year &
       & (self, cd) &
       & result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_inq_nday_year => inq_nday_year
    implicit none
    type(cal_attr_t),intent(inout) :: self
    type(cal_date_t),intent(in)    :: cd
    integer :: mode
    mode = check_mode_year (self, cd % y)
    r = primitive_inq_nday_year (mode, cd)
  end function inq_nday_year

!!!_  & inq_nmonth_year()
  integer function inq_nmonth_year &
       & (self, cd) &
       & result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_inq_nmonth_year => inq_nmonth_year
    implicit none
    type(cal_attr_t),intent(in) :: self
    type(cal_date_t),intent(in) :: cd
    r = primitive_inq_nmonth_year (self % mode, cd)
    return
  end function inq_nmonth_year

!!!_  & inq_nsec_day()
  integer function inq_nsec_day &
       & (self, cd) &
       & result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_inq_nsec_day => inq_nsec_day
    implicit none
    type(cal_attr_t),intent(in)          :: self
    type(cal_date_t),intent(in),optional :: cd
    r = primitive_inq_nsec_day (self % mode, cd)
    return
  end function inq_nsec_day

!!!_  & inq_nsec_minute()
  integer function inq_nsec_minute &
       & (self, cd) &
       & result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_inq_nsec_minute => inq_nsec_minute
    implicit none
    type(cal_attr_t),intent(in)          :: self
    type(cal_date_t),intent(in),optional :: cd
    r = primitive_inq_nsec_minute (self % mode, cd)
    return
  end function inq_nsec_minute

!!!_  & inq_nsec_hour()
  integer function inq_nsec_hour &
       & (self, cd) &
       & result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_inq_nsec_hour => inq_nsec_hour
    implicit none
    type(cal_attr_t),intent(in)          :: self
    type(cal_date_t),intent(in),optional :: cd
    r = primitive_inq_nsec_hour (self % mode, cd)
    return
  end function inq_nsec_hour

!!!_  & inq_nminute_hour()
  integer function inq_nminute_hour &
       & (self, cd) &
       & result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_inq_nminute_hour => inq_nminute_hour
    implicit none
    type(cal_attr_t),intent(in)          :: self
    type(cal_date_t),intent(in),optional :: cd
    r = primitive_inq_nminute_hour (self % mode, cd)
    return
  end function inq_nminute_hour

!!!_  & conv_csec_cdaysec()
  type(cal_daysec_t) function conv_csec_cdaysec_c &
       & (self, csec, cd) &
       result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_conv_csec_cdaysec_c => conv_csec_cdaysec_c
    implicit none
    type(cal_attr_t),intent(in)          :: self
    real(kind=KRC),  intent(in)          :: csec
    type(cal_date_t),intent(in),optional :: cd
    r = primitive_conv_csec_cdaysec_c (self % mode, csec, cd)
    return
  end function conv_csec_cdaysec_c

  type(cal_daysec_t) function conv_csec_cdaysec_i &
       & (self, csec, cd) &
       result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_conv_csec_cdaysec_i => conv_csec_cdaysec_i
    implicit none
    type(cal_attr_t),intent(in)          :: self
    integer,         intent(in)          :: csec
    type(cal_date_t),intent(in),optional :: cd
    r = primitive_conv_csec_cdaysec_i(self % mode, csec, cd)
    return
  end function conv_csec_cdaysec_i

!!!_  & conv_cdaysec_csec()
  real(kind=KRC) function conv_cdaysec_csec_c &
       & (self, daysec, cd, xk) &
       result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_conv_cdaysec_csec_c => conv_cdaysec_csec_c
    implicit none
    type(cal_attr_t),  intent(in) :: self
    type(cal_daysec_t),intent(in) :: daysec
    type(cal_date_t),  intent(in) :: cd
    real(kind=KRC),    intent(in) :: xk ! dummy

    r = primitive_conv_cdaysec_csec_c(self % mode, daysec, cd)
    return
  end function conv_cdaysec_csec_c

  integer function conv_cdaysec_csec_i &
       & (self, daysec, cd, xk) &
       result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_conv_cdaysec_csec_i => conv_cdaysec_csec_i
    implicit none
    type(cal_attr_t),  intent(in) :: self
    type(cal_daysec_t),intent(in) :: daysec
    type(cal_date_t),  intent(in) :: cd
    integer,           intent(in) :: xk ! dummy

    r = primitive_conv_cdaysec_csec_i(self % mode, daysec, cd)
    return
  end function conv_cdaysec_csec_i

!!!_  & conv_tsec_time()
  type(cal_time_t) function conv_tsec_time_c &
       & (self, tsec, cd) &
       result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_conv_tsec_time_c => conv_tsec_time_c
    implicit none
    type(cal_attr_t),intent(in)          :: self
    real(kind=KRC),  intent(in)          :: tsec
    type(cal_date_t),intent(in),optional :: cd

    r = primitive_conv_tsec_time_c (self % mode, tsec, cd)
    return
  end function conv_tsec_time_c

  type(cal_time_t) function conv_tsec_time_i &
       & (self, tsec, cd) &
       result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_conv_tsec_time_i => conv_tsec_time_i
    implicit none
    type(cal_attr_t),intent(in)          :: self
    integer,         intent(in)          :: tsec
    type(cal_date_t),intent(in),optional :: cd

    r = primitive_conv_tsec_time_i (self % mode, tsec, cd)
    return
  end function conv_tsec_time_i

!!!_  & conv_time_tsec()
  integer function conv_time_tsec_i &
       & (self, t, cd, xk) &
       result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_conv_time_tsec_i => conv_time_tsec_i
    implicit none
    type(cal_attr_t),intent(in) :: self
    type(cal_time_t),intent(in) :: t
    type(cal_date_t),intent(in) :: cd
    integer,         intent(in) :: xk ! dummy

    r = primitive_conv_time_tsec_i(self % mode, t, cd)
    return
  end function conv_time_tsec_i

  real(kind=KRC) function conv_time_tsec_c &
       & (self, t, cd, xk) &
       result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_conv_time_tsec_c => conv_time_tsec_c
    implicit none
    type(cal_attr_t),intent(in) :: self
    type(cal_time_t),intent(in) :: t
    type(cal_date_t),intent(in) :: cd
    real(kind=KRC),  intent(in) :: xk ! dummy

    r = primitive_conv_time_tsec_c(self % mode, t, cd)
    return
  end function conv_time_tsec_c

!!!_  & conv_cday_date()
  type(cal_date_t) function conv_cday_date_i &
       & (self, &
       &  cday) &
       result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_conv_cday_date_i => conv_cday_date_i
    implicit none
    type(cal_attr_t),intent(inout) :: self
    integer,         intent(in)    :: cday
    integer :: mode

    if (self % pt % set .and. self % pt % toggle) then
       r = self % pt % date
    else
       mode = check_mode_cday (self, cday)
       r = primitive_conv_cday_date_i(mode, cday)
    endif
    return
  end function conv_cday_date_i

  type(cal_date_t) function conv_cday_date_c &
       & (self, &
       &  cday) &
       result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_conv_cday_date_c => conv_cday_date_c
    implicit none
    type(cal_attr_t),intent(inout) :: self
    real(kind=KRC),  intent(in)    :: cday
    integer :: mode

    if (self % pt % set .and. self % pt % toggle) then
       r = self % pt % date
    else
       mode = check_mode_cday (self, cday)
       r = primitive_conv_cday_date_c (mode, cday)
    endif
    return
  end function conv_cday_date_c

!!!_  & conv_date_cday()
  integer function conv_date_cday_i &
       & (self, cd, xk) &
       & result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_conv_date_cday_i => conv_date_cday_i
    implicit none
    type(cal_attr_t),intent(inout) :: self
    type(cal_date_t),intent(in)    :: cd
    integer,         intent(in)    :: xk ! dummy
    integer :: mode
    mode = check_mode_year (self, cd % y)
    r = primitive_conv_date_cday_i(mode, cd)
  end function conv_date_cday_i

  real(kind=KRC) function conv_date_cday_c &
       & (self, cd, xk) &
       & result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_conv_date_cday_c => conv_date_cday_c
    implicit none
    type(cal_attr_t),intent(inout) :: self
    type(cal_date_t),intent(in)    :: cd
    real(kind=KRC),  intent(in)    :: xk
    integer :: mode
    mode = check_mode_year (self, cd % y)
    r = primitive_conv_date_cday_c(mode, cd)
  end function conv_date_cday_c

!!!_  & conv_date_dayy()
  integer function conv_date_dayy &
       & (self, cd) &
       & result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_conv_date_dayy => conv_date_dayy
    implicit none
    type(cal_attr_t),intent(inout) :: self
    type(cal_date_t),intent(in)    :: cd
    integer :: mode
    mode = check_mode_year (self, cd % y)
    r = primitive_conv_date_dayy (mode, cd)
  end function conv_date_dayy

!!!_  & conv_date_dayy_compat()
  integer function conv_date_dayy_compat &
       & (self, cd) &
       & result (r)
    use TOUZA_Cal_primitive,only: &
         & primitive_conv_date_dayy_compat => conv_date_dayy_compat
    implicit none
    type(cal_attr_t),intent(inout) :: self
    type(cal_date_t),intent(in)    :: cd
    integer :: mode
    mode = check_mode_year (self, cd % y)
    r = primitive_conv_date_dayy_compat(mode, cd)
  end function conv_date_dayy_compat

!!!_ + core procedures
!!!_  & conv_csec_calendar () - get calendar[D,T] from calendar[sec]
  type(calendar_t) function conv_csec_calendar_c &
       & (self, csec) &
       & result (r)
    implicit none
    type(cal_attr_t),intent(inout) :: self
    real(kind=KRC),  intent(in)    :: csec
    type(cal_daysec_t) :: ds

    ds = conv_csec_cdaysec (self, csec)

    r % d = conv_cday_date (self, ds % d)
    r % t = conv_tsec_time (self, ds % s)

  end function conv_csec_calendar_c

  type(calendar_t) function conv_csec_calendar_i &
       & (self, csec) &
       & result (r)
    implicit none
    type(cal_attr_t),intent(inout) :: self
    integer,         intent(in)    :: csec
    type(cal_daysec_t) :: ds

    ds = conv_csec_cdaysec (self, csec)

    r % d = conv_cday_date (self, ds % d)
    r % t = conv_tsec_time (self, ds % s)

  end function conv_csec_calendar_i

!!!_  & conv_calendar_csec () - get calendar[sec] from calendar[D,T]
  real(kind=KRC) function conv_calendar_csec_c &
       & (self, cal, xk) &
       & result (r)
    implicit none
    type(cal_attr_t),intent(inout) :: self
    type(calendar_t),intent(in)    :: cal
    real(kind=KRC),  intent(in)    :: xk ! dummy

    type(cal_daysec_t) :: ds

    ds%d = conv_date_cday(self, cal%d, ds%d)
    ds%s = conv_time_tsec_c(self, cal % t, cal % d, xk)

    r = conv_cdaysec_csec(self, ds, cal % d, xk)

  end function conv_calendar_csec_c

  integer function conv_calendar_csec_i &
       & (self, cal, xk) &
       & result (r)
    implicit none
    type(cal_attr_t),intent(inout) :: self
    type(calendar_t),intent(in)    :: cal
    integer,         intent(in)    :: xk ! dummy

    type(cal_daysec_t) :: ds

    ds%d = conv_date_cday(self, cal%d, ds%d)
    ds%s = xreal(conv_time_tsec_i(self, cal % t, cal % d, xk))

    r = conv_cdaysec_csec_i(self, ds, cal % d, xk)

  end function conv_calendar_csec_i

!!!_  & conv_cday_cydayy () - get calendar[year day] from calendar[day]
  type(cal_ynday_t) function conv_cday_cydayy &
       & (self, cday) &
       & result (r)
    implicit none
    type(cal_attr_t),intent(inout) :: self
    integer,         intent(in)    :: cday
    type(cal_date_t) :: dt

    dt = conv_cday_date (self, cday)
    r % y = dt % y
    r % d = conv_date_dayy (self, dt)
  end function conv_cday_cydayy

!!!_  & conv_csec_cydayy () - get calendar[year day] from calendar[sec]
  type(cal_ynday_t) function conv_csec_cydayy &
       & (self, csec) &
       & result (r)
    implicit none
    type(cal_attr_t),intent(inout) :: self
    real(kind=KRC),  intent(in)    :: csec
    type(cal_date_t) :: dt

    dt = conv_csec_date (self, csec)
    r % y = dt % y
    r % d = conv_date_dayy (self, dt)
  end function conv_csec_cydayy

!!!_  & conv_csec_date () - get calendar(D) from calendar[sec] (truncated)
  type(cal_date_t) function conv_csec_date &
       & (self, csec) &
       & result (r)
    implicit none
    type(cal_attr_t),intent(inout) :: self
    real(kind=KRC),  intent(in)    :: csec
    type(cal_daysec_t) :: ds
    ds = conv_csec_cdaysec (self, csec)
    r = conv_cday_date (self, ds % d)
  end function conv_csec_date

!!!_  & conv_duration_sec () - get duration[sec] from duration[unit] since calendar[sec]
  real(kind=KRC) function conv_uduration_sec &
       & (self, dur, unit, refsec) &
       & result (r)
    implicit none
    type(cal_attr_t),intent(inout) :: self
    real(kind=KRC),  intent(in)    :: dur
    character(len=*),intent(in)    :: unit
    real(kind=KRC),  intent(in)    :: refsec

    character (len=10) :: unit_buf
    integer ju

    unit_buf = unit
    ju = parse_unit(unit_buf)
    r = conv_jduration_sec(self, dur, ju, refsec)

    return
  end function conv_uduration_sec

  real(kind=KRC) function conv_jduration_sec &
       & (self, dur, junit, refsec) &
       & result (r)
    implicit none
    type(cal_attr_t),intent(inout) :: self
    real(kind=KRC),  intent(in)    :: dur
    integer,         intent(in)    :: junit
    real(kind=KRC),  intent(in)    :: refsec

    type(cal_date_t) :: cd

    if      (junit .eq. iunit_SC)  then
       r = dur
    else if (junit .eq. iunit_MI) then
       r = dur * xreal(inq_nsec_minute (self))
    else if (junit .eq. iunit_HR)  then
       r = dur * xreal(inq_nsec_hour (self))
    else if (junit .eq. iunit_DY)  then
       r = dur * xreal(inq_nsec_day (self))
    else if (junit .eq. iunit_MO)  then
       cd = conv_csec_date (self, refsec)
       r = dur * xreal(inq_nday_month (self, cd)) * xreal(inq_nsec_day (self))
    else if (junit .eq. iunit_YR)  then
       cd = conv_csec_date (self, refsec)
       r = dur * xreal(inq_nday_year (self, cd)) * xreal(inq_nsec_day (self))
    else
       r = dur
    endif
    return
  end function conv_jduration_sec

!!!_  & conv_string_calendar () - parse calendar[D,T] from string
  type(calendar_t) function conv_string_calendar &
       & (str) &
       & result (r)
    implicit none
    character(len=*),intent(in) :: str
#if OPT_CALENDAR_FIXED_YEAR_DIGITS
    integer y, m, d, h, mm, s

    read(str, fmt_calendar_r) y, m, d, h, mm, s
    r % d % y = y
    r % d % m = m
    r % d % d = d
    r % t % h = h
    r % t % m = mm
    r % t % s = s
#else /* not OPT_CALENDAR_FIXED_YEAR_DIGITS */
    integer nc(6), jc
    integer jf, jt, ls

    jc = 1
    nc(:) = 0
    ls = len_trim(str)
    jf = 1
    do jc = 1, 6
       if (jf.gt.ls) exit
       jt = verify(str(jf:ls), '0123456789')
       ! write(*, *) jc, jf, jt, str(jf:ls)
       if (jt.eq.0) then
          jt = ls
       else
          jt = jf + jt - 2
       endif
       if (jt.ge.jf) then
          read(str(jf:jt), *) nc(jc)
       endif
       jf = jt + 2
    enddo
    r % d % y = nc(1)
    r % d % m = nc(2)
    r % d % d = nc(3)
    r % t % h = nc(4)
    r % t % m = nc(5)
    r % t % s = nc(6)

#endif /* not OPT_CALENDAR_FIXED_YEAR_DIGITS */
  end function conv_string_calendar

!!!_  & conv_calendar_string - format string from calendar[D,T]
  subroutine conv_calendar_string &
       (str, cal)
    implicit none
    character(len=*),intent(out) :: str
    type(calendar_t),intent(in)  :: cal
    write (str, fmt_calendar_w) cal % d % y, cal % d % m, cal % d % d, &
         & cal % t % h, cal % t % m, cal % t % s
  end subroutine conv_calendar_string

!!!_  & conv_csec_string_ppt_off - format string from calendar[sec] with disabling perptural mode
  subroutine conv_csec_string_ppt_off &
       (self, str, csec)
    implicit none
    type(cal_attr_t),intent(inout) :: self
    character(len=*),intent(out)   :: str
    real(kind=KRC),  intent(in)    :: csec
    type(calendar_t) :: cal

    call set_perpetual_switch (self, .false.)
    cal = conv_csec_calendar (self, csec)
    call conv_calendar_string (str, cal)
    call set_perpetual_switch (self, .true.)
  end subroutine conv_csec_string_ppt_off

!!!_  & advance_csec() - get calendar[sec] advanced by duration[unit] since calendar[sec]
  real(kind=KRC) function advanceu_csec_c &
       & (self, dur, unit, refsec, xk) &
       & result (r)
    implicit none
    type(cal_attr_t),intent(inout) :: self
    real(kind=KRC),  intent(in)    :: dur
    real(kind=KRC),  intent(in)    :: refsec
    character(len=*),intent(in)    :: unit
    real(kind=KRC),  intent(in)    :: xk ! dummy

    character (len=10) :: unit_buf
    integer ju
    unit_buf = unit
    ju = parse_unit(unit_buf)
    r = advancej_csec_c(self, dur, ju, refsec, xk)
    return
  end function advanceu_csec_c

  real(kind=KRC) function advancej_csec_c &
       & (self, dur, junit, refsec, xk) &
       & result (r)
    implicit none
    type(cal_attr_t),intent(inout) :: self
    real(kind=KRC),  intent(in)    :: dur
    real(kind=KRC),  intent(in)    :: refsec
    integer,         intent(in)    :: junit
    real(kind=KRC),  intent(in)    :: xk ! dummy

    real(kind=KRC) :: step
    type(cal_daysec_t) :: ds
    type(cal_date_t) :: cd

    if (junit.eq.iunit_YR .or. junit.eq.iunit_MO) then
       ds = conv_csec_cdaysec (self, refsec)
       cd = conv_cday_date (self, ds % d)
       if (junit.eq.iunit_YR) then
          cd % y = cd % y + int (dur)
       else
          cd % m = cd % m + int (dur)
       endif
       ds % d = conv_date_cday(self, cd, ds%d)
       r = conv_cdaysec_csec(self, ds, cd, xk)
    else
       step = conv_duration_sec (self, dur, junit, refsec)
       r = refsec + step
    endif
  end function advancej_csec_c

!!!_  & is_passed_compat() - check if duration[unit] passed or equal since ref[sec] at calendar[sec]
  logical function is_passed_compat &
       & (self, csec, refsec, orgsec, dur, unit) &
       & result (r)
    implicit none
    type(cal_attr_t),intent(inout) :: self
    real(kind=KRC),  intent(in)    :: csec, refsec, orgsec, dur
    character(len=*),intent(in)    :: unit

    r = (csec .eq. refsec)
    if (r) return

    r = is_passed(self, csec, refsec, orgsec, dur, unit)
    return
  end function is_passed_compat

!!!_  & is_passed () - check if duration[unit] passes since ref[sec] at calendar[sec]
  logical function is_upassed &
       & (self, csec, refsec, orgsec, dur, unit) &
       & result (r)
    implicit none
    type(cal_attr_t),intent(inout) :: self
    real(kind=KRC),  intent(in)    :: csec, refsec, orgsec, dur
    character(len=*),intent(in)    :: unit

    character(len=5) :: utmp
    integer ju

    r = (csec .eq. refsec)
    if (r) return

    utmp = unit
    ju = parse_unit(utmp)

    r = is_jpassed(self, csec, refsec, orgsec, dur, ju)

    return
  end function is_upassed

  logical function is_jpassed &
       & (self, csec, refsec, orgsec, dur, junit) &
       & result (r)
    implicit none
    type(cal_attr_t),intent(inout) :: self
    real(kind=KRC),  intent(in)    :: csec, refsec, orgsec, dur
    integer,         intent(in)    :: junit

    type(cal_date_t) :: cdt, cdtp
    real(kind=KRC)   :: dursec
    real(kind=KRC)   :: ry, rmo

    integer nmy, ndy
    integer imo, iy, ndm

    r = (csec .eq. refsec)
    if (r) return

    cdt  = conv_csec_date(self, csec)
    cdtp = conv_csec_date(self, refsec)

    dursec = conv_duration_sec(self, dur, junit, csec)

    if (csec .ge. orgsec) then
       if      (junit.eq.iunit_YR)  then
          nmy = inq_nmonth_year(self, cdt)
          ndy = inq_nday_year(self, cdt)
          ry = XREAL(cdt%y - cdtp%y) &
               & + XREAL(cdt%m - cdtp%m) / XREAL(nmy) &
               & + XREAL(cdt%d - cdtp%d) / XREAL(ndy)
          if (ry .ge. dur) r = .true.
       else if (junit.eq.iunit_MO) then
          imo = 0
          do iy = cdtp%y, cdt%y - 1
             imo = imo + inq_nmonth_year(self, cal_date_t(iy, 0, 0))
          enddo
          ndm = inq_nday_month(self, cdt)
          rmo = XREAL(cdt%m - cdtp%m + imo) &
               & + XREAL(cdt%d - cdtp%d) / XREAL(ndm)
          if (rmo .ge. dur) r = .true.
       else if (DGAUS((csec - orgsec) / dursec) &
            &   .gt. DGAUS((refsec - orgsec) / dursec)) then
          r = .true.
       endif
    endif
    return
  end function is_jpassed

!!!_  & check_mode_year () - auto-mode check by year
  integer function check_mode_year &
       & (self, cyear) &
       & result (r)
!!!_   = declaration
    implicit none
    type(cal_attr_t),intent(inout) :: self
    integer,         intent(in)    :: cyear
!!!_   . body
    if (self%auto .ne. auto_false) then
       if (cyear .ge. auto_cyear_grego_l) then
          r = p_grego_l
       else if (cyear .ge. auto_cyear_grego_i) then
          r = p_grego_i
       else
          r = p_ideal
       endif
       if (self%auto.eq.auto_once) then
          self%auto = auto_false
          self%mode = r
       endif
    else
       r = self % mode
    endif
    return
  end function check_mode_year

!!!_  & check_mode_cday () - auto-mode check by calendar-day
  integer function check_mode_cday_i &
       & (self, cday) &
       & result (r)
    implicit none
    type(cal_attr_t),intent(inout) :: self
    integer,         intent(in)    :: cday

    if (self%auto .ne. auto_false) then
       if (cday .ge. auto_cday_grego_l) then       !! 1900*365+1900/4-19+5
          r = p_grego_l
       else if (cday .ge. auto_cday_grego_i) then
          r = p_grego_i
       else
          r = p_ideal
       endif
       if (self%auto.eq.auto_once) then
          self%auto = auto_false
          self%mode = r
       endif
    else
       r = self % mode
    endif
    return
  end function check_mode_cday_i

  integer function check_mode_cday_c &
       & (self, cday) &
       & result (r)
    implicit none
    type(cal_attr_t),intent(inout) :: self
    real(kind=KRC),  intent(in)    :: cday

    if (self%auto .ne. auto_false) then
       if (cday .ge. xreal(auto_cday_grego_l)) then
          r = p_grego_l
       else if (cday .ge. xreal(auto_cday_grego_i)) then
          r = p_grego_i
       else
          r = p_ideal
       endif
       if (self%auto.eq.auto_once) then
          self%auto = auto_false
          self%mode = r
       endif
    else
       r = self % mode
    endif
    return
  end function check_mode_cday_c

!!!_ & parse_unit ()
  integer function parse_unit &
       & (unit) result (r)
    implicit none
    character(len=*),intent(in) :: unit
    character(len=10)           :: unit_buf

    unit_buf = unit
    if      (conv_str_bgn(unit_buf, unit_SC)) then
       r = iunit_SC
    else if (conv_str_bgn(unit_buf, unit_MI)) then
       r = iunit_MI
    else if (conv_str_bgn(unit_buf, unit_HR)) then
       r = iunit_HR
    else if (conv_str_bgn(unit_buf, unit_DY)) then
       r = iunit_DY
    else if (conv_str_bgn(unit_buf, unit_MO)) then
       r = iunit_MO
    else if (conv_str_bgn(unit_buf, unit_YR)) then
       r = iunit_YR
    else
       r = iunit_invalid
    endif
    return
  end function parse_unit

  logical function conv_str_bgn(V, T) result(r)
    implicit none
    character(len=*),intent(in) :: V, T
    integer l
    l = len(T)
    r = V(1:l) .eq. T(1:l)
  end function conv_str_bgn

!!!_ & dgaus_private()
  real(kind=KRC) function dgaus_private &
       & (DX) &
       result (r)
    real(kind=KRC),intent(in) :: dx
    r = AINT (dx) + AINT (dx - AINT (dx) + 1.D0) - 1.D0
    return
  end function dgaus_private

!!!_ + end
end module TOUZA_Cal_core
!!!_@ test_calendar_core - test program
#ifdef TEST_CALENDAR_CORE
program test_calendar_core
  use TOUZA_Cal_core
  implicit none
  integer ierr

  call init (ierr, stdv=-9)
  call diag (ierr)

  call chk_conv_string('1234/56/78-12:34:56')
  call chk_conv_string('1234/56/78 12:34:56')
  call chk_conv_string('1234 56 78 12 34 56')
  call chk_conv_string('12343 56 78 12 34 56')
  call chk_conv_string('12343/56,78 12 34 56')
  call chk_conv_string('123/56 78 12 34 56')
  call chk_conv_string('1234//78-::56')

  call finalize(ierr)

  stop
contains
  subroutine chk_conv_string (str)
    implicit none
    character(len=*),intent(in) :: str
    type(calendar_t) :: cal

    cal = conv_string_calendar(str)

101 format('<', A, '> == ', 6(1x, I0))
    write(*, 101) trim(str), cal

  end subroutine chk_conv_string

end program test_calendar_core
#endif /* TEST_CALENDAR_CORE */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

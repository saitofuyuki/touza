!!!_! calendar_primitive.F90 - TOUZA/Cal primitives
! Maintainer: SAITO Fuyuki
! Created: Fri Jul 22 2011
#define TIME_STAMP 'Time-stamp: <2025/08/26 22:20:39 fuyuki calendar_primitive.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2011-2025
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
#ifndef   TEST_CALENDAR_PRIMITIVE
#  define TEST_CALENDAR_PRIMITIVE 0
#endif
#if TEST_CALENDAR_PRIMITIVE
#  define DEBUG *
#endif
!!!_@ TOUZA_Cal_primitive - calendar primitive attributes definition
module TOUZA_Cal_primitive
!!!_ = declaration
!!!_  - modules
  use TOUZA_Std,only: KFLT, KDBL
  use TOUZA_Std,only: is_msglev,      is_msglev_normal
  use TOUZA_Std,only: msglev_panic
  use TOUZA_Std,only: msglev_fatal,   msglev_critical, msglev_severe
  use TOUZA_Std,only: msglev_warning, msglev_normal,   msglev_info
  use TOUZA_Std,only: msglev_detail,  msglev_debug
  use TOUZA_Std,only: choice,         control_mode,    control_deep,  is_first_force
  use TOUZA_Std,only: trace_control,  trace_fine
!!!_  - default
  implicit none
  private
!!!_  - parameters
  integer,parameter,public  :: KRC = OPT_KIND_REAL

  integer,parameter,private :: max_month = OPT_CALENDAR_MAX_MONTH
  integer,parameter,private :: max_leap  = OPT_CALENDAR_MAX_LEAP
!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = CAL_MSG_LEVEL
  integer,save :: lev_stdv = CAL_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = -1

# define __MDL__ 'p'
!!!_  - cal_date_t
  type cal_date_t
     integer y, m, d
  end type cal_date_t
!!!_  - cal_time_t
  type cal_time_t
     integer h, m, s
  end type cal_time_t
!!!_  - cal_daysec_t - day number plus second
  type cal_daysec_t
     integer        :: d
     real(KIND=KRC) :: s
  end type cal_daysec_t
!!!_  - cal_prop_t - calendar properties
  type cal_prop_t
     integer :: nmon_year = 0
     integer :: nday_year(max_leap)                  = 0
     integer :: nday_mon(0:max_month, max_leap)      = 0
     integer :: cum_day_mon(0:max_month, max_leap)   = 0
     integer :: ang_first_day(0:max_month, max_leap) = 0  !! cumulative first day (for angular calendar)
     integer :: nsec_day  = 0
     integer :: nsec_hour = 0
     integer :: nmin_hour = 0
     integer :: nsec_min  = 0
     integer :: nyear_period = 0    !!   number of years within a leap cycle (400 years for Gregorian)
     integer :: nday_period = 0     !!   number of days within a leap cycle  (146097 days for Gregorian)
  end type cal_prop_t
!!!_   + notes
!!       nday_mon (0) > 0 if constant else 0
!!!_  - predefined properties
  integer,parameter,public :: p_error   = 0
  integer,parameter,public :: p_ideal   = 1  ! idealized even-day month calendar
  integer,parameter,public :: p_grego_i = 2  ! idealized gregorian calendar
  integer,parameter,public :: p_grego_l = 3  ! gregorian calendar with leap years
  integer,parameter,public :: p_user    = 4  ! user-defined calendar

  type(cal_prop_t),private,save :: props(0:4)

!!!_  - interfaces
  interface xreal
     module procedure xreal_i
     module procedure xreal_c
  end interface xreal

!!!_  - public
  public :: cal_date_t, cal_time_t, cal_daysec_t

  public :: init, diag, finalize, msg

  public :: inq_nday_month,    inq_nday_year,   inq_nmonth_year
  public :: inq_nsec_day,      inq_nsec_minute, inq_nsec_hour, inq_nminute_hour
  public :: inq_nday_period,   inq_nyear_period
  public :: get_nday_months
  public :: conv_cdaysec_csec_i, conv_cdaysec_csec_c
  public :: conv_csec_cdaysec_i, conv_csec_cdaysec_c
  public :: conv_tsec_time_i,    conv_tsec_time_c
  public :: conv_time_tsec_i,    conv_time_tsec_c
  public :: conv_cday_date_i,    conv_cday_date_c
  public :: conv_date_cday_i,    conv_date_cday_c
  public :: conv_date_dayy,      conv_date_dayy_compat
  public :: is_leap_year
  public :: xreal

!!!_   . inheritance from TOUZA_Std
  public :: is_msglev,      is_msglev_normal
  public :: msglev_panic
  public :: msglev_fatal,   msglev_critical, msglev_severe
  public :: msglev_warning, msglev_normal,   msglev_info
  public :: msglev_detail,  msglev_debug
  public :: choice,         control_mode,    control_deep,  is_first_force
  public :: trace_control,  trace_fine

contains
!!!_ & init - initialization
  subroutine init(ierr, u, levv, mode, stdv)
!!!_  = declaration
    use TOUZA_Std,only: std_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode ! initialization mode
    integer,intent(in),optional :: stdv ! verbose level of TOUZA_Std
    integer im
    integer md, lv, lmd
!!!_  - body
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
       if (md.ge.MODE_DEEP) then
          lev_stdv = choice(lev_stdv, stdv)
          if (ierr.eq.0) call std_init(ierr, u=ulog, levv=lev_stdv, mode=lmd)
       endif
       if (is_first_force(init_counts, mode)) then
          do im = p_error, p_user
             call init_prop(props(im), im)
          enddo
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_ & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std,only: std_diag=>diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer lv, md, utmp, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             call msg(msglev_normal, TIME_STAMP, __MDL__, utmp)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call std_diag(ierr, utmp, levv=lev_stdv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_ & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std,only: std_finalize=>finalize
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
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call std_finalize(ierr, utmp, levv=lev_stdv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_  & msg - message dispatcher
  subroutine msg &
       & (levm, txt, mdl, u)
    use TOUZA_Std,only: gen_tag, std_msg=>msg
    implicit none
    integer,         intent(in)          :: levm
    character(len=*),intent(in)          :: txt
    character(len=*),intent(in),optional :: mdl
    integer,         intent(in),optional :: u
    character(len=128) tag
    integer utmp

    if (is_msglev(levm, lev_verbose)) then
       if (init_mode.ge.MODE_DEEP) then
          call gen_tag(tag, PACKAGE_TAG, __GRP__, mdl)
          call std_msg(txt, tag, u)
       else
          utmp = choice(-1, u)
101       format(A)
          if (utmp.ge.0) then
             write(utmp, 101) trim(txt)
          else if (utmp.eq.-1) then
             write(*,    101) trim(txt)
          endif
       endif
    endif
    return
  end subroutine msg

!!!_ & init_prop - standard properties
  subroutine init_prop &
       & (prop, &
       &  mode)
    implicit none
    type(cal_prop_t),intent(out) :: prop
    integer,         intent(in)  :: mode
    integer m, l
    integer nt

    prop%nsec_min  = 60
    prop%nmin_hour = 60
    prop%nsec_hour = prop%nsec_min  * prop%nmin_hour
    prop%nsec_day  = prop%nsec_hour * 24

    if (mode .eq. p_error) then
       prop%nday_mon(:,:)  = 0
       ! nmon_year is valid
       prop%nmon_year = 12
       prop%nday_year = (prop%nday_mon(0, 1) * prop%nmon_year)
       prop%nyear_period = 1
       prop%nday_period = prop%nday_year(1)
    else if (mode .eq. p_ideal) then
       prop%nday_mon(:,:)  = 30
       prop%nmon_year = 12
       prop%nday_year = (prop%nday_mon(0, 1) * prop%nmon_year)
       prop%nyear_period = 1
       prop%nday_period = prop%nday_year(1)
    else if ((mode .eq. p_grego_i) .or. (mode .eq. p_grego_l)) then
       prop%nday_mon(0:12, 1) = (/0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
       prop%nday_mon(:,    2) = prop%nday_mon(:, 1)
       if (mode .eq. p_grego_l) then
          prop%nday_mon(2, 2) = prop%nday_mon(2, 2) + 1
       endif
       prop%nmon_year = 12
       prop%nday_year(1) = sum(prop%nday_mon(1:12, 1))
       prop%nday_year(2) = sum(prop%nday_mon(1:12, 2))

       if (mode .eq. p_grego_l) then
          prop%nyear_period = 400
          nt = (prop%nyear_period / 4) - (prop%nyear_period / 100) + (prop%nyear_period / 400)
          prop%nday_period = prop%nday_year(1) * (prop%nyear_period - nt) &
               &           + prop%nday_year(2) * nt
       else
          prop%nyear_period = 1
          prop%nday_period = prop%nday_year(1)
       endif
    else
!     insert here user definition of calendar
    endif
!
    do l = 1, max_leap
       prop%cum_day_mon(0, l) = 0
       do m = 1, max_month
          prop%cum_day_mon(m, l) = prop%cum_day_mon(m - 1, l) + prop%nday_mon(m, l)
       enddo
    enddo

#   ifdef DEBUG
      write(DEBUG, '(A, I1)') 'property:', mode
      write(DEBUG, '(4I6)')   prop%nsec_min, prop%nsec_hour, prop%nmin_hour, prop%nsec_day
      write(DEBUG, '(3I5)')   prop%nmon_year, prop%nday_year
      write(DEBUG, '(I4, 2x, 12I4)')  prop%nday_mon (:, 1)
      write(DEBUG, '(I4, 2x, 12I4)')  prop%nday_mon (:, 2)
      write(DEBUG, '(I4, 2x, 12I4)')  prop%cum_day_mon (:, 1)
      write(DEBUG, '(I4, 2x, 12I4)')  prop%cum_day_mon (:, 2)
      write(DEBUG, '(I0, 1x, I0)')    prop%nday_period, prop%nyear_period
#   endif /* DEBUG */
  end subroutine init_prop

!!!_ & get_nday_months - get table of number of days in the months
  subroutine get_nday_months &
       & (ierr, nmon, nday_mon, mode, lp)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: nmon
    integer,intent(out) :: nday_mon(*)
    integer,intent(in)  :: mode
    integer,intent(in)  :: lp

    ierr = err_default
    nmon = props(mode) % nmon_year
    nday_mon(1:nmon) = props(mode)%nday_mon(1:nmon, lp)
    return
  end subroutine get_nday_months

! !!!_ & set_angular_months - [reserved] set cumulative day in terms of angular months
!   subroutine set_angular_months &
!        & (ierr, joff, nday_mon, mode, lp)
!     implicit none
!     integer,intent(out) :: ierr
!     integer,intent(in)  :: joff
!     integer,intent(in)  :: nday_mon(*)
!     integer,intent(in)  :: mode
!     integer,intent(in)  :: lp

!     integer jm
!     ierr = 0

!     ! props(mode)%ang_day_mon(0, lp) = joff
!     ! do jm = 1, props(mode)%nmon_year
!     !    props(mode)%ang_day_mon(jm, lp) = props(mode)%ang_day_mon(jm - 1, lp) + nday_mon (jm)
!     ! enddo

!   end subroutine set_angular_months

!!!_ & inq_nday_period () - return number of days in a leap-cycle period
  integer function inq_nday_period &
       & (mode) &
       & result (r)
    implicit none
    integer,intent(in) :: mode
    r  = props(mode) % nday_period
    return
  end function inq_nday_period

!!!_ & inq_nyear_period () - return number of years in a leap-cycle period
  integer function inq_nyear_period &
       & (mode) &
       & result (r)
    implicit none
    integer,intent(in) :: mode
    r  = props(mode) % nyear_period
    return
  end function inq_nyear_period

!!!_ & inq_nday_month () - return number of days in the month of the date
  integer function inq_nday_month &
       & (mode, cd) &
       & result (r)
    implicit none
    integer,         intent(in) :: mode
    type(cal_date_t),intent(in) :: cd
    integer lp
    lp = get_ndm_index(mode, cd%y)
    r  = props(mode) % nday_mon(cd%m, lp)
    return
  end function inq_nday_month

!!!_ & inq_nday_year () - return number of days in the year of the date
  integer function inq_nday_year &
       & (mode, cd) &
       & result (r)
    implicit none
    integer,         intent(in) :: mode
    type(cal_date_t),intent(in) :: cd
    integer lp
    lp = get_ndm_index(mode, cd%y)
    r  = props(mode) % nday_year(lp)
    return
  end function inq_nday_year

!!!_ & inq_nmonth_year () - return number of months in the year of the date
  integer function inq_nmonth_year &
       & (mode, cd) &
       & result (r)
    implicit none
    integer,         intent(in)          :: mode
    type(cal_date_t),intent(in),optional :: cd ! for consistency
    if (present(cd)) continue                    ! dummy
    r  = props(mode) % nmon_year
    return
  end function inq_nmonth_year

!!!_ & inq_nsec_day () - return number of seconds in the day of the date
  integer function inq_nsec_day &
       & (mode, cd) &
       & result (r)
    implicit none
    integer,         intent(in)          :: mode
    type(cal_date_t),intent(in),optional :: cd ! for consistency
    if (present(cd)) continue                    ! dummy
    r  = props(mode) % nsec_day
    return
  end function inq_nsec_day

!!!_ & inq_nsec_minute () - return number of seconds in the minute of the date
  integer function inq_nsec_minute &
       & (mode, cd) &
       & result (r)
    implicit none
    integer,         intent(in)          :: mode
    type(cal_date_t),intent(in),optional :: cd ! for consistency
    if (present(cd)) continue                    ! dummy
    r  = props(mode) % nsec_min
    return
  end function inq_nsec_minute

!!!_ & inq_nsec_hour () - return number of seconds in the hour of the date
  integer function inq_nsec_hour &
       & (mode, cd) &
       & result (r)
    implicit none
    integer,         intent(in)          :: mode
    type(cal_date_t),intent(in),optional :: cd ! for consistency
    if (present(cd)) continue                  ! dummy
    r  = props(mode) % nsec_hour
    return
  end function inq_nsec_hour

!!!_ & inq_nminute_hour () - return number of minutes in the hour of the date
  integer function inq_nminute_hour &
       & (mode, cd) &
       & result (r)
    implicit none
    integer,         intent(in)          :: mode
    type(cal_date_t),intent(in),optional :: cd   ! for consistency
    if (present(cd)) continue                    ! dummy
    r  = props(mode) % nmin_hour
    return
  end function inq_nminute_hour

!!!_ & conv_csec_cdaysec () - get calendar[day:sec] from calendar[sec]
  type(cal_daysec_t) function conv_csec_cdaysec_c &
       & (mode, csec, cd) &
       result (r)
    implicit none
    integer,         intent(in)          :: mode
    real(kind=KRC),  intent(in)          :: csec
    type(cal_date_t),intent(in),optional :: cd
    integer  nsd
    integer  idays
    real(kind=KRC) :: rsec

    nsd = inq_nsec_day(mode, cd)

    idays  = int(csec / xreal(nsd)) + 1
    rsec   = csec - xreal(idays - 1) * xreal(nsd)
    if (nint(rsec) .GE. nsd) then
       idays = idays + 1
       rsec  = rsec - xreal(nsd)
    endif
    r % d = idays
    r % s = rsec
    return
  end function conv_csec_cdaysec_c

  type(cal_daysec_t) function conv_csec_cdaysec_i &
       & (mode, csec, cd) &
       result (r)
    implicit none
    integer,         intent(in)          :: mode
    integer,         intent(in)          :: csec
    type(cal_date_t),intent(in),optional :: cd
    integer  nsd
    integer  idays
    integer  rsec

    nsd = inq_nsec_day(mode, cd)

    idays  = csec / nsd + 1
    rsec   = mod(csec, nsd)
    r % d = idays
    r % s = xreal(rsec)
    return
  end function conv_csec_cdaysec_i

!!!_ & conv_cdaysec_csec () - get calendar[sec] from calendar[day:sec]
  real(kind=KRC) function conv_cdaysec_csec_c &
       & (mode, daysec, cd) &
       result (r)
    implicit none
    integer,           intent(in) :: mode
    type(cal_daysec_t),intent(in) :: daysec
    type(cal_date_t),  intent(in) :: cd
    integer  nsd

    nsd = inq_nsec_day(mode, cd)
    r   = xreal(daysec%d - 1) * xreal(nsd) + xreal(daysec%s)
    return
  end function conv_cdaysec_csec_c

  integer function conv_cdaysec_csec_i &
       & (mode, daysec, cd) &
       result (r)
    implicit none
    integer,           intent(in) :: mode
    type(cal_daysec_t),intent(in) :: daysec
    type(cal_date_t),  intent(in) :: cd
    integer  nsd

    nsd = inq_nsec_day(mode, cd)
    r   = (daysec%d - 1) * nsd + int(daysec%s)
    return
  end function conv_cdaysec_csec_i

!!!_ & conv_tsec_time () - get time[hour:min:sec] from time[sec]
  type(cal_time_t) function conv_tsec_time_c &
       & (mode, tsec, cd) &
       result (r)
!!!_  - Note
  ! TSEC is expected to be zero or positive and less than one day,
  ! which should be asserted.
!!!_  - Body
    implicit none
    integer,         intent(in)          :: mode
    real(kind=KRC),  intent(in)          :: tsec
    type(cal_date_t),intent(in),optional :: cd

    integer nsh, nsm, nmh
    nsh = inq_nsec_hour(mode, cd)
    nsm = inq_nsec_minute(mode, cd)
    nmh = inq_nminute_hour(mode, cd)
    r%h = int(tsec / xreal(nsh))
    r%m = int((tsec - xreal(r%h * nsh)) / xreal(nsm))
    r%s = nint(tsec - xreal(r%h * nsh) - xreal(r%m * nsm))
    if (r%s .ge. nsm) then
       r%m = r%m + 1
       r%s = r%s - nsm
    endif
!!!_  ? Condition [.EQ.] follows ucaln:CRS2HM
    if (r%m .eq. nmh) then
       r%h = r%h + 1
       r%m = r%m - nmh
    endif
    return
  end function conv_tsec_time_c

  type(cal_time_t) function conv_tsec_time_i &
       & (mode, tsec, cd) &
       result (r)
!!!_  - Note
  ! TSEC is expected to be zero or positive and less than one day,
  ! which should be asserted.
!!!_  - Body
    implicit none
    integer,         intent(in)          :: mode
    integer,         intent(in)          :: tsec
    type(cal_date_t),intent(in),optional :: cd

    integer nsh, nsm, nmh
    nsh = inq_nsec_hour(mode, cd)
    nsm = inq_nsec_minute(mode, cd)
    nmh = inq_nminute_hour(mode, cd)
    r%h = tsec / nsh
    r%m = mod(tsec / nsm, nmh)
    r%s = mod(tsec, nsm)
    return
  end function conv_tsec_time_i

!!!_ & conv_time_tsec () - get time[sec] from time[hour:min:sec]
  integer function conv_time_tsec_i &
       & (mode, t, cd) &
       result (r)
!!!_  - Note
  ! Negative time should be avoided.
!!!_  - Body
    implicit none
    integer,         intent(in) :: mode
    type(cal_time_t),intent(in) :: t
    type(cal_date_t),intent(in) :: cd
    integer nsh, nsm
    nsh = inq_nsec_hour(mode, cd)
    nsm = inq_nsec_minute(mode, cd)
    r = t%h * nsh + t%m * nsm + t%s
    return
  end function conv_time_tsec_i

  real(kind=KRC) function conv_time_tsec_c &
       & (mode, t, cd) &
       result (r)
!!!_  - Body
    implicit none
    integer,         intent(in) :: mode
    type(cal_time_t),intent(in) :: t
    type(cal_date_t),intent(in) :: cd
    r = xreal(conv_time_tsec_i(mode, t, cd))
    return
  end function conv_time_tsec_c

!!!_ & conv_cday_date () - get date[year/month/day] from calendar[day]
  type(cal_date_t) function conv_cday_date_i &
       & (mode, &
       &  cday) &
       result (r)
    implicit none
    integer,intent(in) :: mode
    integer,intent(in) :: cday

    integer :: lp

    integer ndm, ndy
    integer jy,  jy4, jcent, jcent4, idays0
    integer idy, id,  m
!
    lp = 1
    ndm = props(mode) % nday_mon(0, lp)

    if (mode .eq. p_grego_l) then
       jy = int(xreal(cday) / 365.24)
       do
          jy4    = (jy + 3)   / 4
          jcent  = (jy + 99)  / 100
          jcent4 = (jy + 399) / 400
          idays0 = jy * 365 + jy4 - jcent + jcent4
          if (cday .le. idays0) then
             jy = jy - 1
             if (jy .ge. 0) cycle
          endif
          exit
       enddo
       r%y = JY
       idy = cday - idays0
       if (is_leap_year(r%y)) then
          lp = 2
       else
          lp = 1
       endif
    else if (ndm.eq.0) then
       jy     = cday / props(mode) % nday_year(lp)
       idays0 = jy * props(mode) % nday_year(lp)
       if (cday .le. idays0) then
          jy     = jy - 1
          idays0 = jy * props(mode) % nday_year(lp)
       endif
       r%y   = JY
       idy   = cday - idays0
    else
!!!_  * just for /not initialized/ warning
       idy = 0
    endif
!!!_   + case non-uniform month
    if (ndm.eq.0) then
       id = 0
       do m = 1, props(mode) % nmon_year
          id = props(mode) % cum_day_mon(m, lp)
          if (idy .le. id) then
             r%m = m
             r%d = idy - id + props(mode) % nday_mon(m, lp)
             exit
          endif
       enddo
!!!_   + case uniform month
    else
       ndy = props(mode) % nday_year(lp)
       r%y = (cday - 1) / ndy
       r%m = (cday - 1 - r%y * ndy) / ndm + 1
       r%d = (cday - r%y * ndy - (r%m - 1) * ndm)
    endif
    return
  end function conv_cday_date_i

  type(cal_date_t) function conv_cday_date_c &
       & (mode, &
       &  cday) &
       result (r)
    implicit none
    integer,       intent(in) :: mode
    real(kind=KRC),intent(in) :: cday

    integer :: lp

    integer ndm, ndy
    integer jy,  jy4, jcent, jcent4
    integer idy, id,  m
    real(kind=KRC) :: days0c
!
    lp = 1
    ndm = props(mode) % nday_mon(0, lp)

    if (mode .eq. p_grego_l) then
       jy = int(cday / 365.24)
       do
          jy4    = (jy + 3)   / 4
          jcent  = (jy + 99)  / 100
          jcent4 = (jy + 399) / 400
          days0c = xreal(jy) * 365_KRC + jy4 - jcent + jcent4
          if (cday .le. days0c) then
             jy = jy - 1
             if (jy .ge. 0) cycle
          endif
          exit
       enddo
       r%y = JY
       idy = int(cday - days0c)
       if (is_leap_year(r%y)) then
          lp = 2
       else
          lp = 1
       endif
    else if (ndm.eq.0) then
       jy     = int(cday / xreal(props(mode) % nday_year(lp)))
       days0c = xreal(jy) * xreal(props(mode) % nday_year(lp))
       if (cday .le. days0c) then
          jy     = jy - 1
          days0c = xreal(jy) * xreal(props(mode) % nday_year(lp))
       endif
       r%y   = JY
       idy   = int(cday - days0c)
    else
!!!_  - just for /not initialized/ warning
       idy = 0
    endif
!!!_   + case non-uniform month
    if (ndm.eq.0) then
       id = 0
       do m = 1, props(mode) % nmon_year
          id = props(mode) % cum_day_mon(m, lp)
          if (idy .le. id) then
             r%m = m
             r%d = idy - id + props(mode) % nday_mon(m, lp)
             exit
          endif
       enddo
!!!_   + case uniform month
    else
       ndy = props(mode) % nday_year(lp)
       r%y = int((cday - 1.0_KRC) / xreal(ndy))
       r%m = int(cday - 1.0_KRC - xreal(r%y) * xreal(ndy)) / ndm + 1
       r%d = int(cday - xreal(r%y) * xreal(ndy) - xreal((r%m - 1) * ndm))
    endif
    return
  end function conv_cday_date_c

!!!_ & conv_date_cday () - get calendar[day] from date[year/month/day]
  integer function conv_date_cday_i &
       & (mode, cd) &
       & result (r)
    implicit none
    integer,         intent(in) :: mode
    type(cal_date_t),intent(in) :: cd
    integer lp
    type(cal_date_t) :: ncd
    integer jy4, jcent, jcent4
    integer idays0
    integer id
    ncd = date_normalize(mode, cd)
!!!_  * get idays0
    if (mode .eq. p_grego_l) then
       jy4    = (ncd%y + 3)   / 4
       jcent  = (ncd%y + 99)  / 100
       jcent4 = (ncd%y + 399) / 400
       idays0 = ncd%y * 365 + jy4 - jcent + jcent4
       if (is_leap_year(ncd%y)) then
          lp = 2
       else
          lp = 1
       endif
    else
       lp = 1
       idays0 = ncd%y * props(mode) % nday_year(lp)
    endif
    id = props(mode) % cum_day_mon(ncd%m - 1, lp)
    r = idays0 + id + ncd%d
    return
  end function conv_date_cday_i

  real(kind=KRC) function conv_date_cday_c &
       & (mode, cd) &
       & result (r)
    implicit none
    integer,         intent(in) :: mode
    type(cal_date_t),intent(in) :: cd
    integer lp
    type(cal_date_t) :: ncd
    integer jy4, jcent, jcent4
    real(kind=KRC) :: days0c
    integer id
    ncd = date_normalize(mode, cd)
!!!_  - get idays0
    if (mode .eq. p_grego_l) then
       jy4    = (ncd%y + 3)   / 4
       jcent  = (ncd%y + 99)  / 100
       jcent4 = (ncd%y + 399) / 400
       days0c = xreal(ncd%y) * 365_KRC + xreal(jy4 - jcent + jcent4)
       if (is_leap_year(ncd%y)) then
          lp = 2
       else
          lp = 1
       endif
    else
       lp = 1
       days0c = xreal(ncd%y) * xreal(props(mode) % nday_year(lp))
    endif
    id = props(mode) % cum_day_mon(ncd%m - 1, lp)
    r = days0c + xreal(id + ncd%d)
    return
  end function conv_date_cday_c

!!!_ & conv_date_dayy () # (reserved) get serial-day number from date[year/month/day]
  integer function conv_date_dayy &
       & (mode, cd) &
       & result(r)
    implicit none
    integer,         intent(in) :: mode
    type(cal_date_t),intent(in) :: cd
    integer lp
    type(cal_date_t) :: ncd
    integer id
    ncd = date_normalize(mode, cd)
    if (mode .eq. p_grego_l) then
       if (is_leap_year(ncd%y)) then
          lp = 2
       else
          lp = 1
       endif
    else
       lp = 1
    endif
    id = props(mode) % cum_day_mon(ncd%m - 1, lp)
    r  = id + ncd%d
    return
  end function conv_date_dayy

!!!_ & conv_date_dayy_compat () # get serial-day number from date[year/month/day] compatible
  integer function conv_date_dayy_compat &
       & (mode, cd) &
       & result (r)
    implicit none
    integer,         intent(in) :: mode
    type(cal_date_t),intent(in) :: cd
    integer lp
    type(cal_date_t) :: ncd
    integer id
    ncd = date_normalize(mode, cd)
    if (mode .eq. p_grego_l) then
       if (is_leap_year(ncd%y)) then
          lp = 2
       else
          lp = 1
       endif
    else
       lp = 1
    endif
    id = props(mode)% cum_day_mon(ncd%m - 1, lp)
    r  = id + ncd%d
    if (mode .ne. p_grego_l .and. mode .ne. p_grego_i) then
       r = r + (ncd%y - cd%y) * props(mode) % nday_year(lp)
    endif
    return
  end function conv_date_dayy_compat

!!!_ & date_normalize () # date normalization
  type(cal_date_t) function date_normalize &
       & (mode, cd) &
       & result (r)
!!!_  = declaration
    implicit none
    integer,         intent(in) :: mode
    type(cal_date_t),intent(in) :: cd
    integer nmy
!!!_  * body
!    if ((mode .eq. p_grego_l)  .or. (mode .eq. p_grego_i)) then
       nmy = props(mode) % nmon_year
       if (cd%m .gt. 0) then
          r%y = cd%y + (cd%m - 1) / nmy
          r%m = mod(cd%m - 1, nmy) + 1
       else
          r%y = cd%y - (- cd%m) / nmy - 1
          r%m = nmy - mod(- cd%m, nmy)
       endif
       r%d = cd%d
!    else
!       r = cd
!    endif
    return
  end function date_normalize

!!!_ & get_ndm_index () - get nday-month (leap-year) index
  integer function get_ndm_index &
       & (mode, iyear) &
       & result (r)
!!!_  = declaration
    implicit none
    integer,intent(in) :: mode
    integer,intent(in) :: iyear
!!!_  - body
    r = 1
    if (mode .eq. p_grego_l) then
       if (is_leap_year(iyear)) then
          r = 2
       endif
    endif
    return
  end function get_ndm_index

!!!_ & is_leap_year () - if or not leap year
  logical function is_leap_year &
    & (iyear) &
    & result (l)
    implicit none
    integer,intent(in) :: iyear
    integer  iy, iycen, icent

    iy    = mod(iyear, 4)
    iycen = mod(iyear, 100)
    icent = mod(iyear / 100, 4)

    l = ((iy .eq. 0) &
         .and.  ((iycen .ne. 0) .or. (icent .eq. 0)))
    return
  end function is_leap_year

!!!_ & xreal () - type conversion wrapper
  ELEMENTAL &
  real(kind=KRC) function xreal_i (IV) &
       & result(r)
    integer,intent(in) :: IV
    r = real(IV, kind=KRC)
    return
  end function xreal_i

  ELEMENTAL &
  real(kind=KRC) function xreal_c (RV) &
       & result(r)
    real(kind=KRC),intent(in) :: RV
    r = RV
    return
  end function xreal_c

!!!_ + end
end module TOUZA_Cal_primitive
!!!_@ test_calendar_primitive - test program
#if TEST_CALENDAR_PRIMITIVE
program test_calendar_primitive
  use TOUZA_Cal_primitive
  implicit none
  integer ierr, levv, stdv

  ierr = 0

  levv = +9
  stdv = +9

  if (ierr.eq.0) call init(ierr, levv=levv, stdv=stdv, mode=MODE_DEEPEST)
  if (ierr.eq.0) call diag(ierr, levv=levv)
  if (ierr.eq.0) call finalize(ierr, levv=levv)
  stop
end program test_calendar_primitive

#endif /* TEST_CALENDAR_PRIMITIVE */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

!!!_! calendar_miroc.F90 - touza/calendar: miroc compatible interfaces
! Maintainer: SAITO Fuyuki
! Created: Fri Jul 25 2011
#define TIME_STAMP 'Time-stamp: <2021/01/26 21:47:19 fuyuki calendar_miroc.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2011-2021
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
!!!_@ calendar_miroc - calendar/miroc compatible procedures
module TOUZA_Cal_miroc
!!!_ = declaration
  implicit none
  public
!!!_  * private
  logical,save,private :: ofirst = .true.
# define __MDL__ 'miroc'
!!!_  * interfaces (external)
  interface
     ! subroutine CALNDR(IFPAR, JFPAR)
     !   implicit none
     !   integer,intent(in),optional :: IFPAR
     !   integer,intent(in),optional :: JFPAR
     ! end subroutine CALNDR
     subroutine CPERPT(IYEAR, IMONTH, IDAY)
       implicit none
       integer,intent(in) :: IYEAR, IMONTH, IDAY
     end subroutine CPERPT
     subroutine CPERPR (IYEAR, IMONTH, IDAY, OOPERP)
       implicit none
       integer,intent(out) :: IYEAR, IMONTH, IDAY
       logical,intent(out) :: OOPERP
     end subroutine CPERPR
     subroutine CPERPO (OOPERP)
       implicit none
       logical,intent(in) :: OOPERP
     end subroutine CPERPO
     subroutine CDAYMO (NDAYMO, IYEAR,  IMONTH)
       implicit none
       integer,intent(out) :: NDAYMO
       integer,intent(in)  :: IYEAR, IMONTH
     end subroutine CDAYMO
     subroutine CDAYYR (NDAYYR, IYEAR)
       implicit none
       integer,intent(out) :: NDAYYR
       integer,intent(in)  :: IYEAR
     end subroutine CDAYYR
     subroutine CMONYR (NMONYR, IYEAR)
       implicit none
       integer,intent(out) :: NMONYR
       integer,intent(in)  :: IYEAR
     end subroutine CMONYR
     subroutine CSECDY (NSECDY)
       implicit none
       integer,intent(out) :: NSECDY
     end subroutine CSECDY
     subroutine CSECMI (NSECMI)
       implicit none
       integer,intent(out) :: NSECMI
     end subroutine CSECMI
     subroutine CSECHR (NSECHR)
       implicit none
       integer,intent(out) :: NSECHR
     end subroutine CSECHR
     subroutine CSS2DS (IDAYS, RSEC, DSEC)
       use TOUZA_Cal,only: KRC
       implicit none
       integer,       intent(out) :: IDAYS
       real(kind=KRC),intent(out) :: RSEC
       real(kind=KRC),intent(in)  :: DSEC
     end subroutine CSS2DS
     subroutine CDS2SS (DSEC, IDAYS, RSEC)
       use TOUZA_Cal,only: KRC
       implicit none
       integer,       intent(in)  :: IDAYS
       real(kind=KRC),intent(in)  :: RSEC
       real(kind=KRC),intent(out) :: DSEC
     end subroutine CDS2SS
     subroutine CRS2HM (IHOUR, IMIN, ISEC, RSEC)
       use TOUZA_Cal,only: KRC
       implicit none
       integer,       intent(out) :: IHOUR, IMIN, ISEC
       real(kind=KRC),intent(in)  :: RSEC
     end subroutine CRS2HM
     subroutine CHM2RS (RSEC, IHOUR, IMIN, ISEC)
       use TOUZA_Cal,only: KRC
       implicit none
       real(kind=KRC),intent(out) :: RSEC
       integer,       intent(in)  :: IHOUR, IMIN, ISEC
     end subroutine CHM2RS
     subroutine CDD2YM (IYEAR, IMONTH, IDAY, IDAYS)
       implicit none
       integer,intent(out) :: IYEAR, IMONTH, IDAY
       integer,intent(in)  :: IDAYS
     end subroutine CDD2YM
     subroutine CYM2DD (IDAYS, IYEAR, IMONTH, IDAY)
       implicit none
       integer,intent(out) :: IDAYS
       integer,intent(in)  :: IYEAR, IMONTH, IDAY
     end subroutine CYM2DD
     subroutine CYM2YD (IDAYSY, IYEAR, IMONTH, IDAY)
       implicit none
       integer,intent(out) :: IDAYSY
       integer,intent(in)  :: IYEAR, IMONTH, IDAY
     end subroutine CYM2YD
     subroutine CSS2YH (IDATE, DSEC)
       use TOUZA_Cal,only: KRC
       implicit none
       integer,       intent(out) :: IDATE (6)
       real(kind=KRC),intent(in)  :: DSEC
     end subroutine CSS2YH
     subroutine CYH2SS (DSEC, IDATE)
       use TOUZA_Cal,only: KRC
       implicit none
       integer,       intent(in)  :: IDATE (6)
       real(kind=KRC),intent(out) :: DSEC
     end subroutine CYH2SS
     subroutine CDD2YD (IYEAR, IDAYSY, IDAYS)
       implicit none
       integer,intent(out) :: IYEAR, IDAYSY
       integer,intent(in)  :: IDAYS
     end subroutine CDD2YD
     subroutine CSS2YD (IYEAR, IDAYSY, DSEC)
       use TOUZA_Cal,only: KRC
       implicit none
       integer,       intent(out) :: IYEAR, IDAYSY
       real(kind=KRC),intent(in)  :: DSEC
     end subroutine CSS2YD
     subroutine CSS2YM (IYEAR, IMONTH, IDAY, DSEC)
       use TOUZA_Cal,only: KRC
       implicit none
       integer,       intent(out) :: IYEAR, IMONTH, IDAY
       real(kind=KRC),intent(in)  :: DSEC
     end subroutine CSS2YM
     subroutine CXX2SS (DDSEC, RTDUR, HUNIT, DSEC)
       use TOUZA_Cal,only: KRC
       implicit none
       real(kind=KRC),  intent(out) :: DDSEC
       real(kind=KRC),  intent(in)  :: RTDUR, DSEC
       character(len=*),intent(in)  :: HUNIT
     end subroutine CXX2SS
     subroutine CCC2YH (ITIME, HTIME)
       implicit none
       integer,         intent(out) :: ITIME (6)
       character(len=*),intent(in)  :: HTIME
     end subroutine CCC2YH
     subroutine CYH2CC (HTIME, ITIME)
       implicit none
       integer,         intent(in)  :: ITIME (6)
       character(len=*),intent(out) :: HTIME
     end subroutine CYH2CC
     subroutine CSS2CC (HTIME, DSEC)
       use TOUZA_Cal,only: KRC
       implicit none
       character(len=*),intent(out) :: HTIME
       real(kind=KRC),  intent(in)  :: DSEC
     end subroutine CSS2CC
     logical function OCLEAP (IYEAR)
       implicit none
       integer,intent(in) :: IYEAR
     end function OCLEAP
     subroutine CSSAFT (DSECA, DSEC,  RAFTR, HUNIT)
       use TOUZA_Cal,only: KRC
       implicit none
       real(kind=KRC),  intent(out) :: DSECA
       real(kind=KRC),  intent(in)  :: DSEC, RAFTR
       character(len=*),intent(in)  :: HUNIT
     end subroutine CSSAFT
     logical function OINTVL (DTIME, DTPREV, DTORGN, RINTV, HTUNIT)
       use TOUZA_Cal,only: KRC
       implicit none
       real(kind=KRC),  intent(in) :: DTIME, DTPREV, DTORGN, RINTV
       character(len=*),intent(in) :: HTUNIT
     end function OINTVL
  end interface
!!!_  * public
  public init, diag, finalize
contains
!!!_ & init - calendar init
  subroutine init (ifpar, jfpar, levv)
    use TOUZA_Cal_primitive,only: msg, msglev_normal, msglev_warning
    use TOUZA_Cal,only: cal_init=>init, set_perpetual_date, &
         & auto_once, auto_false
    implicit none
    integer,intent(in)          :: ifpar, jfpar
    integer,intent(in),optional :: levv
    integer :: mode
    logical :: is_auto
    integer :: auto
    logical :: perpetual
    integer :: idatpp(3)
    integer jerr
    if (ofirst) then
       ofirst = .false.
       call config_by_namelist (mode, is_auto, ifpar, jfpar, perpetual, idatpp)
       if (is_auto) then
          auto = auto_once
       else
          auto = auto_false
       endif
       call cal_init (jerr, jfpar, 0, mode, auto, levv)
       call msg(msglev_normal, TIME_STAMP, __MDL__, jfpar)
       if (perpetual) then
          call set_perpetual_date &
               & (idatpp(1), idatpp(2), idatpp(3), perpetual)
       endif
    else
       call msg(msglev_warning, 'calendar_miroc::init skipped', __MDL__, jfpar)
    endif
    return
  end subroutine init

!!!_ & diag
  subroutine diag(ierr, u)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    ierr = 0
    if (present(u)) continue    ! dummy
    return
  end subroutine diag

!!!_ & finalize
  subroutine finalize(ierr, u)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    ierr = 0
    if (present(u)) continue    ! dummy
    return
  end subroutine finalize

!!!_ & config_by_namelist
  subroutine config_by_namelist &
       & (mode,      auto, &
       &  ifpar,     jfpar, &
       &  perpetual, idatpp)
    use TOUZA_Cal,only: &
         & p_error, p_ideal, p_grego_i, p_grego_l, p_user
    implicit none
    integer,intent(out) :: mode
    logical,intent(out) :: auto
    integer,intent(in)  :: ifpar, jfpar
    integer,intent(out) :: idatpp(*)
    logical,intent(out) :: perpetual

    LOGICAL ::  OAUTO  = .false.   !! YR=0-999     : 360day
                                   !! YR=1000-1899 : 365day
                                   !! YR=1900-     : Gregorian
    LOGICAL ::  OGREGO = .false.   !! the Gregorian calendar ?
    LOGICAL ::  OIDEAL = .true.    !! ideal calender (n day per month)
    LOGICAL ::  OANGLC = .false.   !! use angular calendar

    LOGICAL :: OPERPT = .false.    !! perpetual ?
    INTEGER :: IYRPP  = 0          !! perpetual date(year)
    INTEGER :: IMONPP = 3          !! perpetual date(month)
    INTEGER :: IDAYPP = 21         !! perpetual date(day)

    !!  Changing the following properties is not supported
!!$    INTEGER :: IDAYMO = 30         !! 1 month = X days
!!$    INTEGER :: IMONYR = 12         !! 1 year = X months
!!$    INTEGER :: ISECMN = 60         !! 1 minute = X sec.
!!$    INTEGER :: IMINHR = 60         !! 1 hour = X minutes
!!$    INTEGER :: IHRDAY = 24         !! 1 day = X hours

    INTEGER :: IDAYMO = -1
    INTEGER :: IMONYR = -1
    INTEGER :: ISECMN = -1
    INTEGER :: IMINHR = -1
    INTEGER :: IHRDAY = -1

    NAMELIST  /NMCALN/ &
         &           OAUTO , OGREGO, OIDEAL, OPERPT, OANGLC, &
         &           IDAYMO, IMONYR, IYRPP,  IMONPP, IDAYPP, &
         &           ISECMN, IMINHR, IHRDAY

    integer ios

    rewind(ifpar, IOSTAT=ios)
    read  (ifpar, NMCALN, IOSTAT=ios)
    write (jfpar, NMCALN, IOSTAT=ios)

    auto = OAUTO

!!  ANY attempts to change these properties fail.
    if (IDAYMO.ge.0 .or. IMONYR.ge.0 .or. ISECMN.ge.0 &
         & .or. IMINHR.ge.0 .or. IHRDAY.ge.0) then
       mode = p_error
    else if (OIDEAL) then
       if (OGREGO) then
          mode = p_grego_i
       else
          mode = p_ideal
       endif
    else
       mode = p_grego_l
    endif
!      OGREGO    t    f    *
!      OIDEAL    t    t    f
!      dys/yr   365  360  366
    perpetual = OPERPT
    idatpp(1) = IYRPP
    idatpp(2) = IMONPP
    idatpp(3) = IDAYPP

  end subroutine config_by_namelist
!!!_ + end
end module TOUZA_Cal_miroc

!!!_* /nonmodule/ interfaces
!!!_ + initalization
!!!_  & UCALN - calendar
subroutine UCALN()
  implicit none
  stop
  return
end subroutine UCALN

!!!_ & CALNDR - initialize calendar (compatible)
subroutine CALNDR(IFPAR, JFPAR)
  use TOUZA_Cal_miroc,only: init
  implicit none
  integer,intent(in),optional :: IFPAR
  integer,intent(in),optional :: JFPAR
  if (present(IFPAR).and.present(JFPAR)) then
     call init(IFPAR, JFPAR)
  else
     write(*,*) 'ABORTS.  UCALN/CALNDR SUBROUTINE REQUIRES ARGUMENTS'
     stop
  endif
  return
end subroutine CALNDR

!!!_ + perpetual mode subroutines
!!!_  & CPERPT - calendar, fixed date
subroutine CPERPT &
     & (IYEAR, IMONTH, IDAY)
  use TOUZA_Cal,only: set_perpetual_date
  implicit none
  integer,intent(in) :: IYEAR, IMONTH, IDAY
  call set_perpetual_date(IYEAR, IMONTH, IDAY)
  return
end subroutine CPERPT

!!!_  & CPERPR - calendar, refer to fixed date
subroutine CPERPR &
     & (IYEAR, IMONTH, IDAY, OOPERP)
  use TOUZA_Cal,only: get_perpetual_date
  implicit none
  integer,intent(out) :: IYEAR, IMONTH, IDAY
  logical,intent(out) :: OOPERP
  call get_perpetual_date (IYEAR, IMONTH, IDAY, OOPERP)
  return
end subroutine CPERPR

!!!_  & CPERPO - calendar, fixed date(on/off)
subroutine CPERPO &
     & (OOPERP)
  use TOUZA_Cal,only: set_perpetual_switch
  implicit none
  logical,intent(in) :: OOPERP
  call set_perpetual_switch (OOPERP)
  return
end subroutine CPERPO

!!!_ + attributes inquiry subroutines
!!!_  & CDAYMO - calendar, No.of day in a month
subroutine CDAYMO &
     & (NDAYMO, &
     &  IYEAR,  IMONTH)
  use TOUZA_Cal,only: inq_nday_month
  implicit none
  integer,intent(out) :: NDAYMO
  integer,intent(in)  :: IYEAR, IMONTH
  NDAYMO = inq_nday_month((/IYEAR, IMONTH/))
  return
end subroutine CDAYMO

!!!_  & CDAYYR - calendar, No.of day in an year
subroutine CDAYYR &
     & (NDAYYR, &
     &  IYEAR)
  use TOUZA_Cal,only: inq_nday_year
  implicit none
  integer,intent(out) :: NDAYYR
  integer,intent(in)  :: IYEAR
  NDAYYR = inq_nday_year (IYEAR)
  return
end subroutine CDAYYR

!!!_  & CMONYR - calendar, No.of month in an year
subroutine CMONYR &
     & (NMONYR, &
     &  IYEAR)
  use TOUZA_Cal,only: inq_nmonth_year
  implicit none
  integer,intent(out) :: NMONYR
  integer,intent(in)  :: IYEAR

  NMONYR = inq_nmonth_year (IYEAR)
  return
end subroutine CMONYR

!!!_  & CSECDY - calendar, No.of sec. in a day
subroutine CSECDY &
     & (NSECDY)
  use TOUZA_Cal,only: inq_nsec_day
  implicit none
  integer,intent(out) :: NSECDY
  NSECDY = inq_nsec_day ()
  return
end subroutine CSECDY

!!!_  & CSECMI - calendar, No of sec. in a minute
subroutine CSECMI &
     & (NSECMI)
  use TOUZA_Cal,only: inq_nsec_minute
  implicit none
  integer,intent(out) :: NSECMI
  NSECMI = inq_nsec_minute ()
  return
end subroutine CSECMI

!!!_  & CSECHR - calendar, No.of sec. in an hour
subroutine CSECHR &
     & (NSECHR)
  use TOUZA_Cal,only: inq_nsec_hour
  implicit none
  integer,intent(out) :: NSECHR
  NSECHR = inq_nsec_hour ()
  return
end subroutine CSECHR

!!!_ + conversion subroutines
!!!_  & CSS2DS - calendar, sec. -> ddss
subroutine CSS2DS &
     & (IDAYS, RSEC, &
     &  DSEC)
  use TOUZA_Cal,only: KRC, conv_csec_adaysec
  implicit none
  integer,       intent(out) :: IDAYS
  real(kind=KRC),intent(out) :: RSEC
  real(kind=KRC),intent(in)  :: DSEC
  call conv_csec_adaysec(IDAYS, RSEC, DSEC)
  return
end subroutine CSS2DS

!!!_  & CDS2SS - calendar, ddss -> sec.
subroutine CDS2SS &
     & (DSEC, &
     &  IDAYS, RSEC)
  use TOUZA_Cal,only: KRC, conv_cdaysec_csec
  implicit none
  integer,       intent(in)  :: IDAYS
  real(kind=KRC),intent(in)  :: RSEC
  real(kind=KRC),intent(out) :: DSEC
  DSEC = conv_cdaysec_csec (IDAYS, RSEC, DSEC)
  return
end subroutine CDS2SS

!!!_  & CRS2HM - calendar, sec. -> hhmmss
subroutine CRS2HM &
     & (IHOUR, IMIN, ISEC, &
     &  RSEC)
  use TOUZA_Cal,only: KRC, conv_tsec_atime
  implicit none
  integer,       intent(out) :: IHOUR, IMIN, ISEC
  real(kind=KRC),intent(in)  :: RSEC
  integer IHMS(3)
  IHMS(1:3) = conv_tsec_atime (RSEC)
  IHOUR = IHMS(1)
  IMIN  = IHMS(2)
  ISEC  = IHMS(3)
  return
end subroutine CRS2HM

!!!_  & CHM2RS - calendar, hhmmss -> sec.
subroutine CHM2RS &
     & (RSEC, &
     &  IHOUR, IMIN, ISEC)
  use TOUZA_Cal,only: KRC, conv_time_tsec, XREAL
  implicit none
  real(kind=KRC),intent(out) :: RSEC
  integer,       intent(in)  :: IHOUR, IMIN, ISEC
  RSEC = XREAL(conv_time_tsec((/IHOUR, IMIN, ISEC/)))
  return
end subroutine CHM2RS

!!!_  & CDD2YM - calendar, day -> yymmdd
subroutine CDD2YM &
     & (IYEAR, IMONTH, IDAY, &
     &  IDAYS)
  use TOUZA_Cal,only: conv_cday_adate
  implicit none
  integer,intent(out) :: IYEAR, IMONTH, IDAY
  integer,intent(in)  :: IDAYS
  integer IYMD(3)
  IYMD(:) = conv_cday_adate(IDAYS)
  IYEAR  = IYMD(1)
  IMONTH = IYMD(2)
  IDAY   = IYMD(3)
  return
end subroutine CDD2YM

!!!_  & CYM2DD - calendar, yymmdd -> day
subroutine CYM2DD &
     & (IDAYS, &
     &  IYEAR, IMONTH, IDAY)
  use TOUZA_Cal,only: conv_adate_cday
  implicit none
  integer,intent(out) :: IDAYS
  integer,intent(in)  :: IYEAR, IMONTH, IDAY
  IDAYS = conv_adate_cday((/IYEAR, IMONTH, IDAY/), XK=IDAYS)
  return
end subroutine CYM2DD

!!!_  & CYM2YD - calendar, yymmdd -> yydd
subroutine CYM2YD &
     & (IDAYSY, &
     &  IYEAR, IMONTH, IDAY)
  use TOUZA_Cal,only: conv_date_dayy_compat
  implicit none
  integer,intent(out) :: IDAYSY
  integer,intent(in)  :: IYEAR, IMONTH, IDAY
  !! IDAYSY = conv_date_dayy (cd)
  IDAYSY = conv_date_dayy_compat((/IYEAR, IMONTH, IDAY/))
  return
end subroutine CYM2YD

!!!_  & CSS2YH - calendar, sec. -> date
subroutine CSS2YH &
     & (IDATE, &
     &  DSEC)
  use TOUZA_Cal,only: KRC,conv_csec_acalendar
  implicit none
  integer,       intent(out) :: IDATE (6)
  real(kind=KRC),intent(in)  :: DSEC
  !
  IDATE(1:6) = conv_csec_acalendar(DSEC)
  return
end subroutine CSS2YH

!!!_  & CYH2SS - calendar, date -> sec.
subroutine CYH2SS &
     & (DSEC, &
     &  IDATE)
  use TOUZA_Cal,only: KRC, conv_calendar_csec
  implicit none
  integer,       intent(in)  :: IDATE (6)
  real(kind=KRC),intent(out) :: DSEC
  dsec = conv_calendar_csec &
       & (IDATE(1), IDATE(2), IDATE(3), IDATE(4), IDATE(5), IDATE(6), dsec)
  return
end subroutine CYH2SS

!!!_  & CDD2YD - calendar, day -> yydd
subroutine CDD2YD &
     & (IYEAR, IDAYSY, &
     &  IDAYS)
  use TOUZA_Cal,only: conv_cday_aydayy
  implicit none
  integer,intent(out) :: IYEAR, IDAYSY
  integer,intent(in)  :: IDAYS
  integer IYD(2)
  IYD(1:2) = conv_cday_aydayy(IDAYS)
  IYEAR  = IYD(1)
  IDAYSY = IYD(2)
  return
end subroutine CDD2YD

!!!_  & CSS2YD - calendar, sec. -> yydd
subroutine CSS2YD &
     & (IYEAR, IDAYSY, &
     &  DSEC)
  use TOUZA_Cal,only: KRC, conv_csec_aydayy
  implicit none
  integer,       intent(out) :: IYEAR, IDAYSY
  real(kind=KRC),intent(in)  :: DSEC
  integer IYD(2)
  IYD(1:2) = conv_csec_aydayy(DSEC)
  IYEAR  = IYD(1)
  IDAYSY = IYD(2)
  return
end subroutine CSS2YD

!!!_  & CSS2YM - calendar, sec. -> yymmdd
subroutine CSS2YM &
     & (IYEAR, IMONTH, IDAY, &
     &  DSEC)
  use TOUZA_Cal,only: KRC, conv_csec_adate
  implicit none
  integer,       intent(out) :: IYEAR, IMONTH, IDAY
  real(kind=KRC),intent(in)  :: DSEC
  integer IYMD(3)
  IYMD(1:3) = conv_csec_adate(DSEC)
  IYEAR  = IYMD(1)
  IMONTH = IYMD(2)
  IDAY   = IYMD(3)
  return
end subroutine CSS2YM

!!!_  & CXX2SS - calendar, hour ->sec.
subroutine CXX2SS &
     & (DDSEC, &
     &  RTDUR, HUNIT, DSEC)
  use TOUZA_Cal,only: KRC, conv_duration_sec
  implicit none
  real(kind=KRC),  intent(out) :: DDSEC
  real(kind=KRC),  intent(in)  :: RTDUR, DSEC
  character(len=*),intent(in)  :: HUNIT

  DDSEC = conv_duration_sec (RTDUR, HUNIT, DSEC)
  return
end subroutine CXX2SS

!!!_  & CCC2YH - calendar, character -> date
subroutine CCC2YH &
     & (ITIME, &
     &  HTIME)
  use TOUZA_Cal,only: conv_string_acalendar
  implicit none
  integer,         intent(out) :: ITIME (6)
  character(len=*),intent(in)  :: HTIME
  ITIME(1:6) = conv_string_acalendar(HTIME)
  return
end subroutine CCC2YH

!!!_  & CYH2CC - calendar, date -> character
subroutine CYH2CC &
     & (HTIME, &
     &  ITIME)
  use TOUZA_Cal,only: conv_calendar_string
  implicit none
  integer,         intent(in)  :: ITIME (6)
  character(len=*),intent(out) :: HTIME
  call conv_calendar_string &
       & (HTIME, ITIME(1), ITIME(2), ITIME(3), ITIME(4), ITIME(5), ITIME(6))
  return
end subroutine CYH2CC

!!!_  & CSS2CC - calendar, sec. -> character (NO PERPET.)
subroutine CSS2CC &
     & (HTIME, &
     &  DSEC)
  use TOUZA_Cal,only: KRC, conv_csec_string_ppt_off
  implicit none
  character(len=*),intent(out) :: HTIME
  real(kind=KRC),  intent(in)  :: DSEC
  call conv_csec_string_ppt_off (HTIME, DSEC)
  return
end subroutine CSS2CC

!!!_  & OCLEAP () - calendar : leap year or not
logical function OCLEAP &
     & (IYEAR) &
     result (r)
  use TOUZA_Cal_core,only: is_leap_year
  implicit none
  integer,intent(in) :: IYEAR
  r = is_leap_year (IYEAR)
  return
end function OCLEAP

!!!_ + calendar advance subroutines
!!!_  & CSSAFT - calendar, time advancing
subroutine CSSAFT &
     & (DSECA, &
     &  DSEC,  RAFTR, HUNIT)
  use TOUZA_Cal,only: KRC, advance_csec
  implicit none
  real(kind=KRC),  intent(out) :: DSECA
  real(kind=KRC),  intent(in)  :: DSEC, RAFTR
  character(len=*),intent(in)  :: HUNIT
  DSECA = advance_csec(RAFTR, HUNIT, DSEC, DSECA)
  return
end subroutine CSSAFT

!!!_  & OINTVL () - time step passed (or equal)?
logical function OINTVL &
     & (DTIME, DTPREV, DTORGN, RINTV, HTUNIT) &
     result (r)
  use TOUZA_Cal,only: KRC, is_passed_compat
  implicit none
  real(kind=KRC),  intent(in) :: DTIME, DTPREV, DTORGN, RINTV
  character(len=*),intent(in) :: HTUNIT
  r = is_passed_compat(DTIME, DTPREV, DTORGN, RINTV, HTUNIT)
  return
end function OINTVL

!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

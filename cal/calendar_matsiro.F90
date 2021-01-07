!!!_! calendar_matsiro.F90 - touza/calendar: (sample) matsiro interfaces
! Maintainer: SAITO Fuyuki
! Created: Jun 8 2020
#define TIME_STAMP 'Time-stamp: <2021/01/07 12:07:55 fuyuki calendar_matsiro.F90>'

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
#include "touza_std.h"
!!!_@ calendar_matsiro - calendar/matsiro compatible procedures
module TOUZA_Cal_matsiro
  use TOUZA_Cal,only: &
       & KRC, &
       & p_grego_i, p_grego_l, p_ideal
!!!_ = declaration
  implicit none
!!!_  * private
  private
  logical,save :: ofirst = .true.
  integer,save :: nsecd = 0
!!!_  * public
  public init, diag, finalize
  public get_interval
  public calendar_advance
!!!_   . calendar modes
  integer,parameter,public :: CALENDAR_NORMAL     = p_grego_l
  integer,parameter,public :: CALENDAR_NOLEAPYEAR = p_grego_i
  integer,parameter,public :: CALENDAR_30360      = p_ideal

contains
!!!_ & init - calendar init
  subroutine init (ierr, cmode)
    use TOUZA_Cal,only: &
         & init_mng => init, &
         & inq_nsec_day
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: cmode
    if (ofirst) then
       ofirst = .false.
       call init_mng (ierr, -1, 0, cmode)

       nsecd = inq_nsec_day()
    endif
    return
  end subroutine init

!!!_ & diag
  subroutine diag(ierr, u)
    use TOUZA_Cal, only: diag_mng => diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    call diag_mng(ierr, u)
    return
  end subroutine diag

!!!_ & finalize
  subroutine finalize(ierr, u)
    use TOUZA_Cal, only: finalize_mng => finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    call finalize_mng(ierr, u)
    return
  end subroutine finalize

!!!_ & get_interval() - interval in seconds between two date-time
  ! return cal_b - cal_a
  integer function get_interval &
       & (cal_a, cal_b) &
       &  result(dsec)
    use TOUZA_Cal, only: &
         & conv_date_cday, conv_time_tsec
    implicit none
    integer,intent(in) :: cal_a(6)
    integer,intent(in) :: cal_b(6)
    integer cdaya, cdayb
    integer tseca, tsecb
    cdaya = conv_date_cday(cal_a(1:3), xk=cdaya)
    cdayb = conv_date_cday(cal_b(1:3), xk=cdayb)
    tseca = conv_time_tsec(cal_a(4:6))
    tsecb = conv_time_tsec(cal_b(4:6))

    dsec = (cdayb - cdaya) * nsecd + tsecb - tseca
    return
  end function get_interval

!!!_ & calendar_advance - advance time in seconds since reference date
  ! cal_d = cal_r + dsec
  subroutine calendar_advance &
       & (cal_d, cal_r, dsec)
    use TOUZA_Cal, only: &
         & conv_date_cday, conv_time_tsec, &
         & conv_cday_adate, conv_tsec_atime
    implicit none
    integer,intent(out) :: cal_d(6)
    integer,intent(in)  :: cal_r(6)
    integer,intent(in)  :: dsec

    integer cday
    integer tsec

    cday = conv_date_cday(cal_r(1:3), xk=cday)
    tsec = conv_time_tsec(cal_r(4:6))
    tsec = tsec + dsec

    if (tsec.lt.0) then
       cday = cday + tsec / nsecd - 1
       tsec = mod(tsec, nsecd) + nsecd
    else
       cday = cday + tsec / nsecd
       tsec = mod(tsec, nsecd)
    endif

    cal_d(1:3) = conv_cday_adate(cday)
    cal_d(4:6) = conv_tsec_atime(tsec)

    return
  end subroutine calendar_advance
!!!_ + end
end module TOUZA_Cal_matsiro
!!!_@ test_calendar_matsiro - test program
#ifdef TEST_CALENDAR_MATSIRO
program test_calendar_matsiro
  use TOUZA_Std, only: uin
  use TOUZA_Cal_matsiro
  use TOUZA_Cal, only: conv_tsec_atime
  implicit none

  integer ierr
  integer cmode
  integer cal_o(6), cal_n(6)
  integer tim_d(3)
  integer dstep,    dchk

  integer l
  integer nerr

  namelist /NMTEST/ cmode, dstep

  ierr = 0
  cmode = CALENDAR_NORMAL
  dstep = 86000
  rewind(uin, IOSTAT=ierr)
  if (ierr.ne.0) then
     write(*,*) 'Need valid configuration.', ierr
     write(*,*) 'Try input following from stdin.'
     write(*,NMTEST)
  else
     read(uin, NMTEST, IOSTAT=ierr)
  endif
  if (ierr.eq.0) then
     call init(ierr, cmode)
  endif
  if (ierr.eq.0) then
109  format('tests = ', I0, 1x, I0, 1x, 3(1x, I0))
101  format(I6, 1x, I4.4, I2.2, I2.2, 1x, I2.2, I2.2, I2.2, &
          & ' -- ', I4.4, I2.2, I2.2, 1x, I2.2, I2.2, I2.2, &
          & 1x, L1)
901  format('invalid count = ', I0)

     nerr = 0
     cal_o = (/1973, 1, 30, 0, 0, 10/)

     tim_d(1:3) = conv_tsec_atime(dstep)
     write(*, 109) cmode, dstep, tim_d(:)

     do l = 0, 10000
        call calendar_advance(cal_n, cal_o, dstep)
        dchk = get_interval(cal_o, cal_n)
        write(*, 101) l, cal_o(:), cal_n(:), dchk.eq.dstep
        if (dchk.ne.dstep) nerr = nerr + 1
        cal_o(:) = cal_n(:)
     enddo

     write (*, 901) nerr
  endif

  call finalize(nerr)
  stop
end program test_calendar_matsiro

#endif /* TEST_CALENDAR_CORE */

!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

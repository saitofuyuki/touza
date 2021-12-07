!!!_! calendar_ils.F90 - touza/calendar: (sample) ILS interfaces
! Maintainer: SAITO Fuyuki
! Created: Jun 8 2020
#define TIME_STAMP 'Time-stamp: <2021/11/15 13:19:31 fuyuki calendar_ils.F90>'
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
!!!_* Macros
#ifndef    OPT_KIND_INT4
#  define  OPT_KIND_INT4 4
#endif
#ifndef    OPT_KIND_INT8
#  define  OPT_KIND_INT8 8
#endif
!!!_@ calendar_ils - calendar/ILS compatible procedures
module TOUZA_Cal_ils
  use TOUZA_Cal,only: &
       & KRC, &
       & p_grego_i, p_grego_l, p_ideal
!!!_ = declaration
  implicit none
!!!_  * private
  private
  logical,save :: ofirst = .true.
  integer,save :: nsecd = 0
  REAL(kind=KRC),save :: nsecd_c = 0.0_KRC

!!!_  * public
!!!_   . integer kinds
  integer,parameter,public :: KI4 = OPT_KIND_INT4
  integer,parameter,public :: KI8 = OPT_KIND_INT8
!!!_   . calendar modes
  integer,parameter,public :: CALENDAR_NORMAL     = p_grego_l
  integer,parameter,public :: CALENDAR_NOLEAPYEAR = p_grego_i
  integer,parameter,public :: CALENDAR_30360      = p_ideal

!!!_  - interfaces
  public :: init, diag, finalize
  public :: cal_date_diff4, inc_calendar4, dec_calendar4
  public :: cal_date_diff8, inc_calendar8, dec_calendar8
  public :: inc_month,     dec_month
  public :: GetMonthDate,  GetMonthNumDays
contains
!!!_ & init - calendar init
  subroutine init (ierr, cmode, levv)
    use TOUZA_Cal,only: cal_init=>init, inq_nsec_day, xreal, &
         &              auto_false
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: cmode
    integer,intent(in),optional :: levv
    integer auto
    integer ulog
    integer inim, stdv

    if (ofirst) then
       ofirst = .false.
       auto = auto_false
       ulog = -99               ! no logging unit
       inim = MODE_SHALLOW      ! skip TOUZA_Std initialization
       stdv = -99               ! verbose level TOUZA_Std (quiet)
       call cal_init &
            & (ierr, u=ulog, levv=levv, mode=inim, stdv=stdv, &
            &  ncals=0, global=cmode, auto=auto)
       nsecd = inq_nsec_day()
       nsecd_c = xreal(nsecd)
    endif
    return
  end subroutine init

!!!_ & diag
  subroutine diag(ierr, u)
    use TOUZA_Cal, only: cal_diag=>diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    call cal_diag(ierr, u=u)
    return
  end subroutine diag

!!!_ & finalize
  subroutine finalize(ierr, u)
    use TOUZA_Cal, only: cal_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    call cal_finalize(ierr, u=u)
    return
  end subroutine finalize

!!!_ & cal_date_diff4() - interval in seconds between two date-time
  ! return date2 - date1
  integer(kind=KI4) function cal_date_diff4 &
       & (date1, date2) &
       &  result(dsec)
    use TOUZA_Cal, only: &
         & conv_date_cday, conv_time_tsec
    implicit none
    integer,intent(in) :: date1(6)
    integer,intent(in) :: date2(6)
    integer cday1, cday2
    integer tsec1, tsec2
    cday1 = conv_date_cday(date1(1:3), xk=cday1)
    cday2 = conv_date_cday(date2(1:3), xk=cday2)
    tsec1 = conv_time_tsec(date1(4:6))
    tsec2 = conv_time_tsec(date2(4:6))

    dsec = (cday2 - cday1) * nsecd + tsec2 - tsec1
    return
  end function cal_date_diff4

!!!_ & cal_date_diff8() - interval in seconds between two date-time
  ! return date2 - date1
  integer(kind=KI8) function cal_date_diff8 &
       & (date1, date2) &
       &  result(dsec)
    use TOUZA_Cal, only: &
         & conv_date_cday, conv_time_tsec
    implicit none
    integer,intent(in) :: date1(6)
    integer,intent(in) :: date2(6)
    real(kind=KRC) :: cday1, cday2
    integer tsec1, tsec2
    cday1 = conv_date_cday(date1(1:3), xk=cday1)
    cday2 = conv_date_cday(date2(1:3), xk=cday2)
    tsec1 = conv_time_tsec(date1(4:6))
    tsec2 = conv_time_tsec(date2(4:6))

    dsec = int((cday2 - cday1), kind=KI8) * nsecd + tsec2 - tsec1
    return
  end function cal_date_diff8

!!!_ & inc_calendar4 - advance time in seconds since reference date
  subroutine inc_calendar4 &
       & (date, delta_t)
    use TOUZA_Cal, only: &
         & conv_date_cday, conv_time_tsec, &
         & conv_cday_adate, conv_tsec_atime
    implicit none
    integer,          intent(inout) :: date(6)
    integer(kind=KI4),intent(in)    :: delta_t

    integer cday
    integer tsec

    cday = conv_date_cday(date(1:3), xk=cday)
    tsec = conv_time_tsec(date(4:6))
    tsec = tsec + delta_t

    if (tsec.lt.0) then
       cday = cday + tsec / nsecd - 1
       tsec = mod(tsec, nsecd) + nsecd
    else
       cday = cday + tsec / nsecd
       tsec = mod(tsec, nsecd)
    endif

    date(1:3) = conv_cday_adate(cday)
    date(4:6) = conv_tsec_atime(tsec)

    return
  end subroutine inc_calendar4

!!!_ & inc_calendar8 - advance time in seconds since reference date
  subroutine inc_calendar8 &
       & (date, delta_t)
    use TOUZA_Cal, only: &
         & conv_date_cday, conv_time_tsec, &
         & conv_cday_adate, conv_tsec_atime, &
         & xreal
    implicit none
    integer,          intent(inout) :: date(6)
    integer(kind=KI8),intent(in)    :: delta_t

    real(kind=KRC) :: cday
    real(kind=KRC) :: tsec

    cday = conv_date_cday(date(1:3), xk=cday)
    tsec = xreal(conv_time_tsec(date(4:6)))
    tsec = tsec + delta_t

    if (tsec.lt.0.0_KRC) then
       cday = cday + AINT(tsec / nsecd_c) - 1.0_KRC
       tsec = AINT(mod(tsec, nsecd_c)) + nsecd_c
    else
       cday = cday + AINT(tsec / nsecd_c)
       tsec = AINT(mod(tsec, nsecd_c))
    endif

    date(1:3) = conv_cday_adate(cday)
    date(4:6) = conv_tsec_atime(tsec)

    return
  end subroutine inc_calendar8

!!!_ & dec_calendar4 - retreat time in seconds since reference date
  subroutine dec_calendar4 &
       & (date, delta_t)
    implicit none
    integer,          intent(inout) :: date(6)
    integer(kind=KI4),intent(in)    :: delta_t
    call inc_calendar4(date, -delta_t)
  end subroutine dec_calendar4

!!!_ & dec_calendar4 - retreat time in seconds since reference date
  subroutine dec_calendar8 &
       & (date, delta_t)
    implicit none
    integer,          intent(inout) :: date(6)
    integer(kind=KI8),intent(in)    :: delta_t
    call inc_calendar8(date, -delta_t)
  end subroutine dec_calendar8

!!!_ & inc_month - advance time in months since reference date
  subroutine inc_month &
       & (date, month)
    use TOUZA_Cal, only: &
         & conv_date_cday, conv_cday_adate
    implicit none
    integer,intent(inout) :: date(6)
    integer,intent(in)    :: month

    real(kind=KRC) :: cday

    date(2) = date(2) + month
    cday = conv_date_cday(date(1:3), xk=cday)
    date(1:3) = conv_cday_adate(cday)

    return
  end subroutine inc_month

!!!_ & dec_month - retreat time in months since reference date
  subroutine dec_month &
       & (date, month)
    use TOUZA_Cal, only: &
         & conv_date_cday, conv_cday_adate
    implicit none
    integer,intent(inout) :: date(6)
    integer,intent(in)    :: month

    real(kind=KRC) :: cday

    date(2) = date(2) - month
    cday = conv_date_cday(date(1:3), xk=cday)
    date(1:3) = conv_cday_adate(cday)

    return
  end subroutine dec_month

!!!_ & GetMonthDate()
  integer(kind=KI8) function GetMonthDate &
       & (yyyy, mo) &
       & result(iday)
    use TOUZA_Cal,only: conv_date_dayy_compat
    implicit none
    integer,intent(IN) :: yyyy
    integer,intent(IN) :: mo
    iday = INT(conv_date_dayy_compat((/ yyyy, mo, 1 /)), KIND=KI8) - 1
    return
  end function GetMonthDate

!!!_ & GetMonthNumDays()
  integer(kind=KI8) function GetMonthNumDays &
       & (yyyy, mo) &
       & result(iday)
    use TOUZA_Cal, only: inq_nday_month
    implicit none
    integer,intent(IN) :: yyyy
    integer,intent(IN) :: mo
    iday = INT(inq_nday_month((/ yyyy, mo /)), KIND=KI8)
    return
  end function GetMonthNumDays

!!!_ + end
end module TOUZA_Cal_ils
!!!_@ test_calendar_ils - test program
#ifdef TEST_CALENDAR_ILS
#if HAVE_HUGE == 0
#  error 'Require huge() intrinsic of Fortran-95 standards'
#endif
program test_calendar_ils
  use TOUZA_Std, only: uin
  use TOUZA_Cal_ils
  implicit none

  integer ierr
  integer cmode, KTEST
  integer cal_o(6)
  integer lmax
  integer(KIND=KI4) :: dsec4
  integer(KIND=KI8) :: dsec8
  integer dmon

  namelist /NMTEST/ cmode, KTEST

  ierr = 0
  cmode = CALENDAR_NORMAL
  KTEST = 0
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
     cal_o = (/1973, 1, 30, 0, 0, 10/)
     lmax  = 1000
  endif
  dsec4 = 86400 * 17 - 4321
  dsec8 = Huge(dsec4) * 10_KI8
  dmon  = 11
  if (KTEST.eq.0.or.KTEST.eq.1) then
     if (ierr.eq.0) then
        call test_ci_incdecdiff4(ierr, cal_o, lmax, dsec4)
     endif
  endif
  if (KTEST.eq.0.or.KTEST.eq.2) then
     if (ierr.eq.0) then
        call test_ci_incdecdiff8(ierr, cal_o, lmax, dsec8)
     endif
  endif
  if (KTEST.eq.0.or.KTEST.eq.3) then
     if (ierr.eq.0) then
        call test_ci_month(ierr, cal_o, lmax, dmon)
     endif
  endif
901 format('invalid count = ', I0)
  write(*, 901) ierr
  call finalize(ierr)
  stop

contains
  subroutine test_ci_incdecdiff4 &
       & (ierr, cal_o, lmax, dsec4)
    use TOUZA_Cal,only: conv_csec_acalendar, xreal
    implicit none
    integer,          intent(out) :: ierr
    integer,          intent(in)  :: cal_o(6)
    integer,          intent(in)  :: lmax
    integer(kind=KI4),intent(in)  :: dsec4
    integer cal_p(6), cal_n(6), cal_b(6)
    integer l, j
    integer(kind=KI4) :: diff4
    integer dc(6)

    logical bchk0, bchk1(6)

    ierr = 0

    cal_p(:) = cal_o(:)

    dc(:) = conv_csec_acalendar(xreal(dsec4))

109 format('DIFF = ', I0, &
         & 2x, I0, '/', I0, '/', I0, &
         & 1x, I0, ':', I0, ':', I0)
    write(*, 109) dsec4, dc(:)

    do l = 0, lmax - 1
       cal_n(:) = cal_p(:)
       call inc_calendar4(cal_n, dsec4)
       diff4 = cal_date_diff4(cal_p, cal_n)
       cal_b(:) = cal_n(:)
       call dec_calendar4(cal_b, dsec4)

       bchk0    = (dsec4.eq.diff4)
       bchk1(:) = (cal_b(:).eq.cal_p(:))

       if (bchk0) then
          continue
       else
          do j = 1, 6
             if (.not.bchk1(j)) then
                ierr = ierr + 1
                exit
             endif
          enddo
       endif
101    format(7L1, 1x, I0, 1x, &
            & I4.4,I2.2,I2.2,1x,I2.2,I2.2,I2.2, ' -- ', &
            & I4.4,I2.2,I2.2,1x,I2.2,I2.2,I2.2)
       write(*,101) bchk0, bchk1(:), l, cal_p(:), cal_n(:)
       cal_p(:) = cal_n(:)
    enddo
    return
  end subroutine test_ci_incdecdiff4

  subroutine test_ci_incdecdiff8 &
       & (ierr, cal_o, lmax, dsec8)
    use TOUZA_Cal,only: conv_csec_acalendar, KRC
    implicit none
    integer,          intent(out) :: ierr
    integer,          intent(in)  :: cal_o(6)
    integer,          intent(in)  :: lmax
    integer(kind=KI8),intent(in)  :: dsec8
    integer cal_p(6), cal_n(6), cal_b(6)
    integer l, j
    integer(kind=KI8) :: diff8
    integer dc(6)

    logical bchk0, bchk1(6)

    ierr = 0

    cal_p(:) = cal_o(:)

    dc(:) = conv_csec_acalendar(REAL(dsec8, kind=KRC))

109 format('DIFF = ', I0, &
         & 2x, I0, '/', I0, '/', I0, &
         & 1x, I0, ':', I0, ':', I0)
    write(*, 109) dsec8, dc(:)

    do l = 0, lmax - 1
       cal_n(:) = cal_p(:)
       call inc_calendar8(cal_n, dsec8)
       diff8 = cal_date_diff8(cal_p, cal_n)
       cal_b(:) = cal_n(:)
       call dec_calendar8(cal_b, dsec8)

       bchk0    = (dsec8.eq.diff8)
       bchk1(:) = (cal_b(:).eq.cal_p(:))

       if (bchk0) then
          continue
       else
          do j = 1, 6
             if (.not.bchk1(j)) then
                ierr = ierr + 1
                exit
             endif
          enddo
       endif
101    format(7L1, 1x, I0, 1x, &
            & I10.10,1x,I2.2,I2.2,1x,I2.2,I2.2,I2.2, ' -- ', &
            & I10.10,1x,I2.2,I2.2,1x,I2.2,I2.2,I2.2)
       write(*,101) bchk0, bchk1(:), l, cal_p(:), cal_n(:)
       cal_p(:) = cal_n(:)
    enddo
    return
  end subroutine test_ci_incdecdiff8

  subroutine test_ci_month &
       & (ierr, cal_o, lmax, dmon)
    use TOUZA_Cal,only: conv_csec_acalendar, xreal
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: cal_o(6)
    integer,intent(in)  :: lmax
    integer,intent(in)  :: dmon
    integer cal_p(6), cal_n(6), cal_b(6)
    integer l

    logical bchk1(6)

    integer(kind=KI8) mday, nday
    ierr = 0

    cal_p(:) = cal_o(:)

109 format('DMONTH = ', I0)
    write(*, 109) dmon

    do l = 0, lmax - 1
       cal_n(:) = cal_p(:)

       mday = GetMonthDate(cal_p(1), cal_p(2))
       nday = GetMonthNumDays(cal_p(1), cal_p(2))

       call inc_month(cal_n, dmon)
       cal_b(:) = cal_n(:)
       call dec_month(cal_b, dmon)

       bchk1(:) = (cal_b(:).eq.cal_p(:))

       ! if (bchk0) then
       !    continue
       ! else
       !    do j = 1, 6
       !       if (.not.bchk1(j)) then
       !          ierr = ierr + 1
       !          exit
       !       endif
       !    enddo
       ! endif
101    format(6L1, 1x, I0, 1x, &
            & I0, 1x, I0, ' // ', &
            & I4.4,I2.2,I2.2,1x,I2.2,I2.2,I2.2, ' -- ', &
            & I4.4,I2.2,I2.2,1x,I2.2,I2.2,I2.2)
       write(*,101) bchk1(:), l, mday, nday, cal_p(:), cal_n(:)
       cal_p(:) = cal_n(:)
    enddo
    return
  end subroutine test_ci_month

end program test_calendar_ils


#endif /* TEST_CALENDAR_CORE */

!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

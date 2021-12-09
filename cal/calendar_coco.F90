!!!_! calendar_coco.F90 - touza/calendar: coco compatible interfaces
! Maintainer: SAITO Fuyuki
! Created: Feb 16 2021
#define TIME_STAMP 'Time-stamp: <2021/11/15 13:18:40 fuyuki calendar_coco.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021
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
#ifndef    COCO_INTEGER
#  define  COCO_INTEGER 4
#endif
#ifndef    COCO_DOUBLE
#  define  COCO_DOUBLE 8
#endif
#ifndef  TEST_CALENDAR_COCO
# define TEST_CALENDAR_COCO 0
#endif
!!!_@ calendar_coco - calendar/miroc compatible procedures
module TOUZA_Cal_coco
!!!_ = declaration
  implicit none
  public
  integer,parameter :: KCI = COCO_INTEGER
  integer,parameter :: KCD = COCO_DOUBLE
# define __MDL__ 'coco'
!!!_  * interfaces (external)
  interface
     subroutine CALNDR(icaln)
       implicit none
       integer(kind=COCO_INTEGER),intent(in) :: icaln
     end subroutine CALNDR
     subroutine CSS2YH (IDATE, DSEC)
       implicit none
       integer(kind=COCO_INTEGER),intent(out) :: IDATE (6)
       real(kind=COCO_DOUBLE),    intent(in)  :: DSEC
     end subroutine CSS2YH
     subroutine CYH2SS (DSEC, IDATE)
       implicit none
       integer(kind=COCO_INTEGER),intent(in)  :: IDATE (6)
       real(kind=COCO_DOUBLE),    intent(out) :: DSEC
     end subroutine CYH2SS
  end interface
!!!_  * public
  public init, diag, finalize
contains
!!!_ & init - calendar init
  subroutine init (cmode, jfpar)
    use TOUZA_Cal_primitive,only: msg, msglev_normal, msglev_warning, choice
    use TOUZA_Cal,only: cal_init=>init, auto_once, auto_false
    implicit none
    integer,intent(in)          :: cmode
    integer,intent(in),optional :: jfpar
    integer auto
    integer jerr
    integer levv
    integer inim
    integer u

    levv  = -99
    auto  = auto_false
    inim  = MODE_SHALLOW
    u = choice(-99, jfpar)
    call cal_init &
         & (jerr, u=u, levv=levv, mode=inim, ncals=0, global=cmode, auto=auto)

    return
  end subroutine init

!!!_ & diag
  subroutine diag(ierr, u)
    use TOUZA_Cal,only: cal_diag=>diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    ierr = 0
    if (present(u)) continue    ! dummy
    call cal_diag(ierr, u=u)
    return
  end subroutine diag

!!!_ & finalize
  subroutine finalize(ierr, u)
    use TOUZA_Cal,only: cal_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    ierr = 0
    if (present(u)) continue    ! dummy
    call cal_finalize(ierr, u=u)
    return
  end subroutine finalize

!!!_ + end
end module TOUZA_Cal_coco

!!!_* /nonmodule/ interfaces
!!!_ & CALNDR - initialize calendar (compatible)
subroutine CALNDR(icaln)
  use TOUZA_Cal,only: p_grego_i, p_grego_l, p_ideal
  use TOUZA_Cal_coco,only: init, KCI
  implicit none
  integer(kind=KCI),intent(in) :: icaln
  integer mode

  if (icaln.eq.1) then
     mode = p_grego_i
  else if (icaln.eq.2) then
     mode = p_grego_l
  else
     mode = p_ideal
  endif
  call init(int(mode))

  return
end subroutine CALNDR

!!!_  & CSS2YH - calendar, sec. -> date
subroutine CSS2YH &
     & (IDATE, &
     &  DSEC)
  use TOUZA_Cal_coco,only: KCI, KCD
  use TOUZA_Cal,only: KRC,conv_csec_acalendar
  implicit none
  integer(kind=KCI),intent(out) :: IDATE (6)
  real(kind=KCD),   intent(in)  :: DSEC
  !
  IDATE(1:6) = conv_csec_acalendar(DSEC)
  return
end subroutine CSS2YH

!!!_  & CYH2SS - calendar, date -> sec.
subroutine CYH2SS &
     & (DSEC, &
     &  IDATE)
  use TOUZA_Cal_coco,only: KCI, KCD
  use TOUZA_Cal,only: KRC, conv_calendar_csec
  implicit none
  integer(kind=KCI),intent(in)  :: IDATE (6)
  real(kind=KCD),   intent(out) :: DSEC
  dsec = conv_calendar_csec &
       & (IDATE(1), IDATE(2), IDATE(3), IDATE(4), IDATE(5), IDATE(6), dsec)
  return
end subroutine CYH2SS

!!!_@ test_calendar_coco - test program
#if TEST_CALENDAR_COCO
program test_calendar_coco
  use TOUZA_Cal_coco
  implicit none

  integer ierr
  integer icaln
  integer cal_o(6), cal_t(6)
  real(8) dsec
  ierr = 0

  icaln = 2
  call calndr(icaln)
  if (ierr.eq.0) then
     cal_o = (/1973, 1, 30, 0, 0, 10/)
     call CYH2SS(dsec, cal_o)
     call CSS2YH(cal_t, dsec)
  endif
  write(*, *) 'input = ', cal_o
  write(*, *) 'sec = ', dsec
  write(*, *) 'output = ', cal_t

  stop
end program test_calendar_coco

#endif /* TEST_CALENDAR_CORE */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

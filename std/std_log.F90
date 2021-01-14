!!!_! std_log.F90 - touza/std simple logging helper
! Maintainer: SAITO Fuyuki
! Created: Jul 27 2011
#define TIME_STAMP 'Time-stamp: <2021/01/13 09:04:27 fuyuki std_log.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2011-2021
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
!!!_* include
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_std.h"
!!!_@ TOUZA_Std_log - simple logging
module TOUZA_Std_log
!!!_ = declaration
!!!_  - default
  implicit none
  private
!!!_  - parameters
  integer,parameter,public :: unit_star   = -1
  integer,parameter,public :: unit_global = -2
  integer,parameter,public :: unit_none   = -999
!!!_  - static
  integer,save :: global_unit = unit_star
  logical,save :: ofirst = .TRUE.

  character(len=*),parameter :: fmt_ytag = '(''['', A, '':'', I0, '']'', 1x, A)'
  character(len=*),parameter :: fmt_ntag = '(''['', I0, '']'', 1x, A)'
!!!_  - public
  public init, diag, finalize
  public msg
  ! public log_string
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u

    ierr = 0

    global_unit = choice(unit_star, u)
    if (ofirst) then
       call msg(TIME_STAMP, 'STD/LOG', 0, u)
    endif
    if (ofirst) ofirst = .FALSE.
    return
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
    call msg('Fine', 'STD/LOG', 0, u)
    return
  end subroutine finalize
!!!_ + user subroutines
!!!_  & msg - message core
  subroutine msg &
       & (txt, tag, lv, u)
    use TOUZA_Std_utl,only: choice
    implicit none
    character(len=*),intent(in)          :: txt
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: lv
    integer,         intent(in),optional :: u
    integer lt, ut
    lt = choice(0, lv)
    ut = choice(unit_global, u)
    if (ut.eq.unit_global) ut = global_unit
    if (present(tag)) then
       if      (ut.eq.unit_star) then
          write(*,   fmt_ytag) trim(tag), lt, trim(txt)
       else if (ut.ge.0)  then
          write(ut,  fmt_ytag) trim(tag), lt, trim(txt)
       endif
    else
       if      (ut.eq.unit_star) then
          write(*,   fmt_ntag) lt, trim(txt)
       else if (ut.ge.0)  then
          write(ut,  fmt_ntag) lt, trim(txt)
       endif
    endif
    return
  end subroutine msg
!!!_ + end
end module TOUZA_Std_log
!!!_@ test_std_log - test program
#ifdef TEST_STD_LOG
program test_std_log
  use TOUZA_Std_log
  implicit none
  integer ierr

  call init(ierr)
  call msg('discarded',  'SKIP', 0, -99)
  call msg('to unit 10', '10',   0, 10)

  call finalize(ierr)

  stop
end program test_std_log
#endif /* TEST_STD_LOG */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

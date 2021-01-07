!!!_! std_prc.F90 - touza/std precision(kind) manager
! Maintainer: SAITO Fuyuki
! Created: Sep 6 2020
#define TIME_STAMP 'Time-stamp: <2021/01/07 11:55:26 fuyuki std_prc.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2020, 2021
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
!!!_* macros
#ifndef    OPT_REAL_SINGLE_DIGITS
#  define  OPT_REAL_SINGLE_DIGITS  6     /* decimal precision */
#endif
#ifndef    OPT_REAL_DOUBLE_DIGITS
#  define  OPT_REAL_DOUBLE_DIGITS  15    /* decimal precision */
#endif
#ifndef    OPT_REAL_SINGLE_EXP
#  define  OPT_REAL_SINGLE_EXP 37        /* decimal exponent range */
#endif
#ifndef    OPT_REAL_DOUBLE_EXP
#  define  OPT_REAL_DOUBLE_EXP 307       /* decimal exponent range */
#endif
!!!_@ TOUZA_Std_prc - precision
module TOUZA_Std_prc
!!!_ = declaration
  implicit none
  private
!!!_  - real precisions
  integer,parameter :: dflt = OPT_REAL_SINGLE_DIGITS
  integer,parameter :: xflt = OPT_REAL_SINGLE_EXP
  integer,parameter :: ddbl = OPT_REAL_DOUBLE_DIGITS
  integer,parameter :: xdbl = OPT_REAL_DOUBLE_EXP

  integer,parameter,public :: KFLT = SELECTED_REAL_KIND(dflt, xflt)
  integer,parameter,public :: KDBL = SELECTED_REAL_KIND(ddbl, xdbl)
!!!_  - public procedures
  public init, diag, finalize
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    ierr = 0 * ulog(u)          ! dummy
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer ui
    ierr = 0
    ui = ulog(u)
101 format('[STD/PRC] ', A, ' = ', I0, 2x, I0, ',', I0)
    if (ui.ge.0) then
       write(ui, 101) 'single', KFLT, dflt, xflt
       write(ui, 101) 'double', KDBL, ddbl, xdbl
    else
       write(*,  101) 'single', KFLT, dflt, xflt
       write(*,  101) 'double', KDBL, ddbl, xdbl
    endif

    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    ierr = 0 * ulog(u)          ! dummy
    return
  end subroutine finalize

!!!_ + private subroutines
  integer function ulog(u) result(ui)
    implicit none
    integer,intent(in),optional :: u
    if (present(u)) then
       ui = u
    else
       ui = -1
    endif
    return
  end function ulog
end module TOUZA_Std_prc

!!!_@ test_std_prc - test program
#ifdef TEST_STD_PRC
program test_std_prc
  use TOUZA_Std_prc
  implicit none
  integer ierr

  call init(ierr)
  if (ierr.eq.0) then
     call diag(ierr)
  endif
  if (ierr.eq.0) then
     call finalize(ierr)
  endif
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_std_prc

#endif /* TEST_STD_PRC */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

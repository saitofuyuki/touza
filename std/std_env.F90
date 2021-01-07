!!!_! std_env.F90 - touza/std standard environments
! Maintainer: SAITO Fuyuki
! Created: May 30 2020
#define TIME_STAMP 'Time-stamp: <2021/01/07 11:55:17 fuyuki std_env.F90>'
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
!!!_@ TOUZA_Std_env - standard environments
module TOUZA_Std_env
!!!_ = declaration
#ifndef   OPT_STDIN_UNIT
#  define OPT_STDIN_UNIT -1 /* default standard input unit */
#endif
#ifndef   OPT_STDOUT_UNIT
#  define OPT_STDOUT_UNIT -1 /* default standard output unit */
#endif
#ifndef   OPT_STD_UNITS_TRY
#  define OPT_STD_UNITS_TRY -1  /* default try limit for brute-force std units check */
#endif
!!!_  - fortran standard condition
#if HAVE_ISO_FORTRAN_ENV
  use ISO_FORTRAN_ENV,only: &
       &  OUTPUT_UNIT, INPUT_UNIT
#  if OPT_STDOUT_UNIT < 0
#    undef  OPT_STDOUT_UNIT
#    define OPT_STDOUT_UNIT OUTPUT_UNIT
#  else
#    warning "Force to use OPT_STDOUT_UNIT"
#  endif
#  if OPT_STDIN_UNIT < 0
#    undef  OPT_STDIN_UNIT
#    define OPT_STDIN_UNIT  INPUT_UNIT
#  else
#    warning "Force to use OPT_STDIN_UNIT"
#  endif
#endif
!!!_  - default
  implicit none
  private
!!!_  - parameters
  integer,save,public :: uin  = OPT_STDIN_UNIT
  integer,save,public :: uout = OPT_STDOUT_UNIT
!!!_  - static
  logical,save :: ofirst = .TRUE.
!!!_  - public
  public init, diag, finalize
  public brute_force_std_units
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, levtry)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: levtry
    integer ltry
    integer ui, uo, ue

    ierr = 0
111 format('[STD/ENV] ISO_FORTRAN_ENV ', A, 1x, I0, 1x, I0)
    if (ofirst) then
#     if HAVE_ISO_FORTRAN_ENV
       write(OUTPUT_UNIT, 111) 'ENABLED', INPUT_UNIT, OUTPUT_UNIT
#     else
       write(*, 111) 'DISABLED'
#     endif
    endif

201 format('[STD/ENV] Try brute-force finder: ', I0)
    if (uin.lt.0.or.uout.lt.0) then
       ltry = choice(OPT_STD_UNITS_TRY, levtry)
       if (ltry.ge.0) then
          write(*, 201) ltry
          call brute_force_std_units(ierr, ui, uo, ue, 0, ltry)
          if (ierr.eq.0) then
             if (uin.lt.0) uin = ui
             if (uout.lt.0) uout = uo
          endif
       endif
    endif

101 format('[STD/ENV] Aborts. ', A, ' IS NOT SET')
    if (uin.lt.0) then
       write(*, 101) 'OPT_INPUT_UNIT'
       ierr = ierr - 1
    endif
    if (uout.lt.0) then
       write(*, 101) 'OPT_OUTPUT_UNIT'
       ierr = ierr - 1
    endif
    if (ofirst) ofirst = .false.
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer ut
    ierr = 0
    ut = choice(uout, u)
101 format('[STD/ENV] UNITS = ', I0, 1x, I0)
    if (ut.ge.0) then
       write(ut, 101) uin, uout
    else
       write(*, 101) uin, uout
    endif
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
!!!_ + user subroutines
!!!_  & brute_force_std_units - lazy trial to find standard units
  subroutine brute_force_std_units &
       & (ierr, ustdi, ustdo, ustde, ubgn, uend)
    ! CAUTION: results are not guaranteed.
    ! Search standard io units between UBGN and UEND
    ! Set USTDI, USTDO, USTDE as input, output, error respectively, if found,
    ! otherwise -1.
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: ustdi, ustdo, ustde
    integer,intent(in),optional :: ubgn,  uend
    integer ub, ue
    integer jchk
    logical OPND
    character(len=16) :: TA

    ierr = 0

    ustdi = -1
    ustdo = -1
    ustde = -1

    ub  = max(0, choice(0, ubgn))
    ue  = choice(-1, uend)
    if (ue.lt.0) ue = ub + 10
    do jchk = ub, ue
       if (ierr.eq.0) inquire(UNIT=jchk, IOSTAT=ierr, OPENED=opnd)
       if (ierr.eq.0.and.OPND) then
          inquire(unit=jchk, IOSTAT=ierr, ACTION=TA)
          if (ierr.eq.0) then
             if (TA.eq.'READ') then
                if (ustdi.lt.0) ustdi = jchk
             else if (TA.eq.'WRITE') then
                if (ustde.lt.0) then
                   ustde = jchk
                else if (ustdo.lt.0) then
                   ustdo = jchk
                endif
             endif
          endif
       endif
    enddo
    if (ustdo.lt.0.and.ustde.ge.0) then
       ustdo = ustde
       ustde = -1
    endif
    return
  end subroutine brute_force_std_units
end module TOUZA_Std_env

!!!_@ test_std_env - test program
#ifdef TEST_STD_ENV
program test_std_env
  use TOUZA_Std_env
  implicit none
  integer ierr
  integer ui, uo, ue

  call init(ierr)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) then
     call brute_force_std_units(ierr, ui, uo, ue)
     write(*, *) 'STD = ', ui, uo, ue
  endif
  if (ierr.eq.0) call finalize(ierr)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_std_env

#endif /* TEST_STD_ENV */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

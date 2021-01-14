!!!_! std_env.F90 - touza/std standard environments
! Maintainer: SAITO Fuyuki
! Created: May 30 2020
#define TIME_STAMP 'Time-stamp: <2021/01/13 16:51:54 fuyuki std_env.F90>'
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
#ifndef   OPT_STDERR_UNIT
#  define OPT_STDERR_UNIT -1 /* default standard error unit */
#endif
#ifndef   OPT_STD_UNITS_TRY
#  define OPT_STD_UNITS_TRY -1  /* default try limit for brute-force std units check */
#endif
#ifndef   OPT_FILE_STORAGE_BITS
#  define OPT_FILE_STORAGE_BITS -1  /* file storage unit in BITS */
#endif
#ifndef   OPT_CHAR_STORAGE_BITS
#  define OPT_CHAR_STORAGE_BITS -1  /* character storage unit in BITS */
#endif
!!!_  - fortran standard condition
#if HAVE_ISO_FORTRAN_ENV
  use ISO_FORTRAN_ENV,only: &
       &  OUTPUT_UNIT, INPUT_UNIT, ERROR_UNIT, &
       &  FILE_STORAGE_SIZE, CHARACTER_STORAGE_SIZE
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
#  if OPT_STDERR_UNIT < 0
#    undef  OPT_STDERR_UNIT
#    define OPT_STDERR_UNIT  ERROR_UNIT
#  else
#    warning "Force to use OPT_STDERR_UNIT"
#  endif
#  if OPT_FILE_STORAGE_BITS < 0
#    undef  OPT_FILE_STORAGE_BITS
#    define OPT_FILE_STORAGE_BITS FILE_STORAGE_SIZE
#  else
#    warning "Force to use OPT_FILE_STORAGE_BITS"
#  endif
#  if OPT_CHAR_STORAGE_BITS < 0
#    undef  OPT_CHAR_STORAGE_BITS
#    define OPT_CHAR_STORAGE_BITS CHARACTER_STORAGE_SIZE
#  else
#    warning "Force to use OPT_CHAR_STORAGE_BITS"
#  endif
#endif /* HAVE_ISO_FORTRAN_ENV */
#if HAVE_ISO_C_BINDING
#endif /* HAVE_ISO_C_BINDING */
!!!_  - default
  implicit none
  private
!!!_  - parameters
  integer,save,public :: uin  = OPT_STDIN_UNIT
  integer,save,public :: uout = OPT_STDOUT_UNIT
  integer,save,public :: uerr = OPT_STDERR_UNIT
  integer,save,public :: lfileu = OPT_FILE_STORAGE_BITS
  integer,save,public :: lcharu = OPT_CHAR_STORAGE_BITS
!!!_  - static
  logical,save :: ofirst = .TRUE.
  integer,save :: lrecb = 0
  integer,save :: lreci = 0, lrecf = 0, lrecd = 0
!!!_  - public
  public init, diag, finalize
  public brute_force_std_units
  public brute_force_storage_unit
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
111 format('[STD/ENV] ISO_FORTRAN_ENV ', A, 1x, I0, 1x, I0, 1x, I0)
112 format('[STD/ENV] INQUIRE(IOLENGTH) ', A)
    if (ofirst) then
#     if HAVE_ISO_FORTRAN_ENV
       write(OUTPUT_UNIT, 111) 'ENABLED', INPUT_UNIT, OUTPUT_UNIT, ERROR_UNIT
#     else
       write(*, 111) 'DISABLED'
#     endif
#     if HAVE_INQUIRE_IOLENGTH
       write(OUTPUT_UNIT, 112) 'ENABLED'
#     else  /* not HAVE_INQUIRE_IOLENGTH */
       write(OUTPUT_UNIT, 112) 'DISABLED'
#     endif /* not HAVE_INQUIRE_IOLENGTH */
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
             if (uerr.lt.0) uerr = ue
             if (uerr.lt.0) uerr = uout
          endif
       endif
    endif

101 format('[STD/ENV] Aborts. ', A, ' IS NOT SET')
    if (uin.lt.0) then
       write(*, 101) 'OPT_STDIN_UNIT'
       ierr = ierr - 1
    endif
    if (uout.lt.0) then
       write(*, 101) 'OPT_STDOUT_UNIT'
       ierr = ierr - 1
    endif
    if (uerr.lt.0) then
       write(*, 101) 'OPT_STDERR_UNIT'
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
101 format('[STD/ENV] UNITS = ', I0, 1x, I0, 1x, I0)
102 format('[STD/ENV] BITS = ', I0, 1x, I0)
    if (ut.ge.0) then
       write(ut, 101) uin, uout, uerr
       write(ut, 102) lfileu, lcharu
    else
       write(*, 101) uin, uout, uerr
       write(*, 102) lfileu, lcharu
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

!!!_ + storage size/record length detection
!!!_  & brute_force_storage_unit - lazy trial to find file storage unit
  subroutine brute_force_storage_unit &
       & (ierr, lunit, utest, ulog)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: lunit
    integer,intent(in)          :: utest
    integer,intent(in),optional :: ulog
    integer ul
    integer lrec
    character(len=*),parameter :: teststr = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=1) :: C

    ierr = 0
    lunit = -1
    ul = choice(-1, ulog)
    open(UNIT=utest, STATUS='NEW', IOSTAT=ierr)
    if (ierr.ne.0) then
101    format ('ERROR: not a new file = ', I0, 1x, I0)
       if (ul.ge.0) then
          write(ul, 101) utest, ierr
       else if (ul.eq.-1) then
          write(*,  101) utest, ierr
       endif
       return
    endif

    close(UNIT=utest, IOSTAT=ierr)
    if (ierr.eq.0) then
       lrec = len(teststr) + 10
       open(UNIT=utest, STATUS='UNKNOWN', ACCESS='DIRECT', &
            & FORM='UNFORMATTED', ACTION='WRITE', RECL=lrec, IOSTAT=ierr)
    endif
    if (ierr.eq.0) write(UNIT=utest, REC=1, IOSTAT=ierr) teststr
    if (ierr.eq.0) close(UNIT=utest, IOSTAT=ierr)
    if (ierr.eq.0) then
       lrec = 1
       open(UNIT=utest, STATUS='UNKNOWN', ACCESS='DIRECT', &
            & FORM='UNFORMATTED', ACTION='READWRITE', RECL=lrec, IOSTAT=ierr)
    endif
    if (ierr.eq.0) read(UNIT=utest, REC=2, IOSTAT=ierr) C
    if (ierr.eq.0) close(UNIT=utest, STATUS='delete', IOSTAT=ierr)
    if (ierr.eq.0) then
       lunit = INDEX(teststr, C) - 1
    endif

    return
  end subroutine brute_force_storage_unit
end module TOUZA_Std_env

!!!_@ test_std_env - test program
#ifdef TEST_STD_ENV
program test_std_env
  use TOUZA_Std_env
  implicit none
  integer ierr
  integer ui, uo, ue
  integer ut, lu

  call init(ierr)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) then
     call brute_force_std_units(ierr, ui, uo, ue)
     write(*, *) 'STD = ', ui, uo, ue
  endif
  if (ierr.eq.0) then
     do ut = 10, 10
     ! do ut = 10, 13
        call brute_force_storage_unit(ierr, lu, ut)
        write(*, *) 'STORAGE = ', lu
     enddo
     ierr = 0
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

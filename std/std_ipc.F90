!!!_! std_ipc.F90 - touza/std intrinsic procedures compatible gallery
! Maintainer: SAITO Fuyuki
! Created: Feb 25 2023
#define TIME_STAMP 'Time-stamp: <2023/11/26 11:20:51 fuyuki std_ipc.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2023
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
#ifndef    OPT_IPC_HYPOT_ITER
#  define  OPT_IPC_HYPOT_ITER 3
#endif
!!!_ + debug
#ifndef   TEST_STD_IPC
#  define TEST_STD_IPC 0
#endif
!!!_@ TOUZA_Std_env - standard environments
module TOUZA_Std_ipc
  use TOUZA_Std_prc,only: KI32, KI64
  use TOUZA_Std_log,only: unit_global,  trace_fine,   trace_control
!!!_ = declaration
!!!_  - default
  implicit none
  private
# define __MDL__ 'ipc'
# define _ERROR(E) (E - ERR_MASK_STD_IPC)
!!!_  - parameters
  integer,parameter :: iter_hypot = OPT_IPC_HYPOT_ITER
!!!_  - public constants
!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL
  integer,save :: ulog = unit_global

  integer,save :: err_default = ERR_NO_INIT - ERR_MASK_STD_IPC
!!!_  - interfaces
  interface ipc_IBITS
     module procedure ipc_IBITS_i, ipc_IBITS_l
  end interface ipc_IBITS
  interface exam_IBITS
     module procedure exam_IBITS_i, exam_IBITS_l
  end interface exam_IBITS

  interface ipc_HYPOT
     module procedure ipc_HYPOT_d, ipc_HYPOT_f
  end interface ipc_HYPOT

  interface ipc_ASINH
     module procedure ipc_ASINH_d, ipc_ASINH_f
  end interface ipc_ASINH
  interface ipc_ACOSH
     module procedure ipc_ACOSH_d, ipc_ACOSH_f
  end interface ipc_ACOSH
  interface ipc_ATANH
     module procedure ipc_ATANH_d, ipc_ATANH_f
  end interface ipc_ATANH

!!!_  - public
  public init, diag, finalize
  public ipc_IBITS,  exam_IBITS
  public ipc_HYPOT
  public ipc_ASINH,  ipc_ACOSH, ipc_ATANH
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: utl_init=>init, choice
    use TOUZA_Std_log,only: log_init=>init
    use TOUZA_Std_prc,only: prc_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u      ! log unit
    integer,intent(in),optional :: levv   ! verbose level
    integer,intent(in),optional :: mode   ! initialization flag

    integer md, lv, lmd

    ierr = 0

    md = control_mode(mode, MODE_DEEPEST)
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
          if (ierr.eq.0) call prc_init(ierr, ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call utl_init(ierr, ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call log_init(ierr, ulog, levv=lv, mode=lmd)
       endif
       if (is_first_force(init_counts, mode)) then

       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = _ERROR(ERR_FAILURE_INIT)
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: utl_diag=>diag, choice
    use TOUZA_Std_log,only: log_diag=>diag, msg_mdl
    use TOUZA_Std_prc,only: prc_diag=>diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer lv, md, utmp, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (VCHECK_NORMAL(lv)) then
                call msg_mdl(TIME_STAMP, __MDL__, utmp)
             endif
             if (VCHECK_DEBUG(lv)) then
                call msg_mdl('(''init = '', I0)', (/init_counts/), __MDL__, utmp)
             endif
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call prc_diag(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call utl_diag(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call log_diag(ierr, utmp, lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: utl_finalize=>finalize, choice
    use TOUZA_Std_log,only: log_finalize=>finalize
    use TOUZA_Std_prc,only: prc_finalize=>finalize
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
          if (ierr.eq.0) call prc_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call utl_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call log_finalize(ierr, utmp, lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + IBITS - Bit extraction
!!!_  - notes about IBITS()
  ! (CAUTION)
  ! Since the bitwise operator for negative value is processor dependent
  ! according to Fortran standards, IBITS(-1,0,32) may be undefined in principle.

  ! As far as the maintainer tried, GCC (gfortran) does not return
  ! the expected answer by IBITS(-1,0,32) (32-bit case).
  ! The return value of IBITS(-1,0,32) = 0, while -1 (untouched) is expected.
  ! This feature originates from the design of IBITS() in GCC.
!!!_  - ipc_IBITS(I, POS, LEN)
  ELEMENTAL &
  integer(kind=KTGT) function ipc_IBITS_i(I, POS, LEN) result(n)
    use TOUZA_Std_prc,only: KTGT=>KI32
    implicit none
    integer(kind=KTGT),intent(in) :: I
    integer,           intent(in) :: POS, LEN
    integer(kind=KTGT),parameter  :: FB = -1_KTGT
    n = IAND(ISHFT(I, - POS), NOT(ISHFT(FB, LEN)))
  end function ipc_IBITS_i
  ELEMENTAL &
  integer(kind=KTGT) function ipc_IBITS_l(I, POS, LEN) result(n)
    use TOUZA_Std_prc,only: KTGT=>KI64
    implicit none
    integer(kind=KTGT),intent(in) :: I
    integer,           intent(in) :: POS, LEN
    integer(kind=KTGT),parameter  :: FB = -1_KTGT
    n = IAND(ISHFT(I, - POS), NOT(ISHFT(FB, LEN)))
  end function ipc_IBITS_l
!!!_  - exam_IBITS
  subroutine exam_IBITS_i(ierr, mold, u, levv)
    use TOUZA_Std_prc,only: KTGT=>KI32
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: is_msglev_DEBUG, msg_mdl
    implicit none
    integer,           intent(out) :: ierr
    integer(kind=KTGT),intent(in)  :: mold
    integer,optional,  intent(in)  :: u, levv

    integer(kind=KTGT),parameter :: isrc = -1_KTGT
    integer,parameter :: lbits = BIT_SIZE(mold)
    character(len=64) :: fmt
    character(len=lbits) :: aref, aans
    character(len=lbits*2+128) :: txt
    integer jp, ll
    integer utmp, lv
    integer(kind=KTGT) :: iref, ians
    integer :: score
    intrinsic :: IBITS

    ierr = 0
    score = 0

    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)
    fmt = ' '
111 format('(B', I0, '.', I0, ')')
    write(fmt, 111, IOSTAT=ierr) lbits, lbits

    do jp = 0, lbits
       do ll = 0, lbits - jp
          iref = IBITS(isrc, jp, ll)
          ians = ipc_IBITS(isrc, jp, ll)
101       format('IBITS(', I0, ',', I0, ',', I0, ') ', &
               & L1, 1x, A, 1x, A)
          if (iref.ne.ians .or. is_msglev_DEBUG(lv)) then
             write(aref, fmt) iref
             write(aans, fmt) ians
             write(txt, 101) isrc, jp, ll, &
                  & iref.eq.ians, &
                  & aref, aans
             call msg_mdl(txt, __MDL__, utmp)
          endif
          if (iref.ne.ians) score = score + 1
       enddo
    enddo
    if (score.gt.0 .or. is_msglev_DEBUG(lv)) then
102    format('score of IBITS() = ', I0)
       write(txt, 102) score
       call msg_mdl(txt, __MDL__, utmp)
    endif
    if (score.gt.0) ierr = _ERROR(ERR_PANIC)
  end subroutine exam_IBITS_i
  subroutine exam_IBITS_l(ierr, mold, u, levv)
    use TOUZA_Std_prc,only: KTGT=>KI64
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: is_msglev_DEBUG, msg_mdl
    implicit none
    integer,           intent(out) :: ierr
    integer(kind=KTGT),intent(in)  :: mold
    integer,optional,  intent(in)  :: u, levv

    integer(kind=KTGT),parameter :: isrc = -1_KTGT
    integer,parameter :: lbits = BIT_SIZE(mold)
    character(len=64) :: fmt
    character(len=lbits) :: aref, aans
    character(len=lbits*2+128) :: txt
    integer jp, ll
    integer utmp, lv
    integer(kind=KTGT) :: iref, ians
    integer :: score
    intrinsic :: IBITS

    ierr = 0
    score = 0

    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)
    fmt = ' '
111 format('(B', I0, '.', I0, ')')
    write(fmt, 111, IOSTAT=ierr) lbits, lbits

    do jp = 0, lbits + 1
       do ll = 0, lbits + 2 - jp
          iref = IBITS(isrc, jp, ll)
          ians = ipc_IBITS(isrc, jp, ll)
101       format('IBITS(', I0, ',', I0, ',', I0, ') ', &
               & L1, 1x, A, 1x, A)
          if (iref.ne.ians .or. is_msglev_DEBUG(lv)) then
             write(aref, fmt) iref
             write(aans, fmt) ians
             write(txt, 101) isrc, jp, ll, &
                  & iref.eq.ians, &
                  & aref, aans
             call msg_mdl(txt, __MDL__, utmp)
          endif
          if (iref.ne.ians) score = score + 1
       enddo
    enddo
    if (score.gt.0 .or. is_msglev_DEBUG(lv)) then
102    format('score of IBITS() = ', I0)
       write(txt, 102) score
       call msg_mdl(txt, __MDL__, utmp)
    endif
    if (score.gt.0) ierr = _ERROR(ERR_PANIC)
  end subroutine exam_IBITS_l
!!!_ + HYPOT - Euclidean distance
!!!_  - notes about HYPOT
  ! Moler and Morrison algorithm
!!!_  - ipc_HYPOT
  ELEMENTAL &
  real(kind=KTGT) function ipc_HYPOT_f(X, Y) result(r)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    real(kind=KTGT),intent(in) :: X, Y
    real(kind=KTGT) :: a, b
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    integer j
    if (x.eq.ZERO) then
       r = abs(y)
       return
    endif
    if (y.eq.ZERO) then
       r = abs(x)
       return
    endif
    a = max(abs(x), abs(y))
    b = min(abs(x), abs(y))
    do j = 1, iter_hypot
       r = (b / a) ** 2
       r = r / (4.0_KTGT + r)
       a = a + 2.0_KTGT * (a * r)
       b = b * r
    enddo
    r = a
    return
  end function ipc_HYPOT_f
  ELEMENTAL &
  real(kind=KTGT) function ipc_HYPOT_d(X, Y) result(r)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: X, Y
    real(kind=KTGT) :: a, b
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    integer j
    if (x.eq.ZERO) then
       r = abs(y)
       return
    endif
    if (y.eq.ZERO) then
       r = abs(x)
       return
    endif
    a = max(abs(x), abs(y))
    b = min(abs(x), abs(y))
    do j = 1, iter_hypot
       r = (b / a) ** 2
       r = r / (4.0_KTGT + r)
       a = a + 2.0_KTGT * (a * r)
       b = b * r
    enddo
    r = a
    return
  end function ipc_HYPOT_d
!!!_ + Inverse hyperbolic functions (Fortran 2008)
!!!_  - ipc_ASINH
  ELEMENTAL &
  real(kind=KTGT) function ipc_ASINH_f(X) result(r)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    real(kind=KTGT),intent(in) :: X
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT

    r = LOG(X + ipc_HYPOT(ONE, X))
    return
  end function ipc_ASINH_f
  real(kind=KTGT) function ipc_ASINH_d(X) result(r)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: X
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT

    r = LOG(X + ipc_HYPOT(ONE, X))
    return
  end function ipc_ASINH_d

!!!_  - ipc_ACOSH
  ELEMENTAL &
  real(kind=KTGT) function ipc_ACOSH_f(X) result(r)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    real(kind=KTGT),intent(in) :: X
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT

    r = LOG(X + SQRT((X - ONE) * (X + ONE)))
    return
  end function ipc_ACOSH_f
  real(kind=KTGT) function ipc_ACOSH_d(X) result(r)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: X
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT

    r = LOG(X + SQRT((X - ONE) * (X + ONE)))
    return
  end function ipc_ACOSH_d

!!!_  - ipc_ATANH
  ELEMENTAL &
  real(kind=KTGT) function ipc_ATANH_f(X) result(r)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    real(kind=KTGT),intent(in) :: X
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT

    r = LOG((ONE + X) / (ONE - X)) / TWO
    return
  end function ipc_ATANH_f
  real(kind=KTGT) function ipc_ATANH_d(X) result(r)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: X
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT

    r = LOG((ONE + X) / (ONE - X)) / TWO
    return
  end function ipc_ATANH_d

!!!_ + end TOUZA_Std_ipc
end module TOUZA_Std_ipc

!!!_@ test_std_ipc - test program
#if TEST_STD_IPC
#  if TEST_STD_IPC == 1
#    define TEST_IPC_IKIND KI32
#    define TEST_IPC_RKIND KFLT
#  else
#    define TEST_IPC_IKIND KI64
#    define TEST_IPC_RKIND KDBL
#  endif
program test_std_ipc
  use TOUZA_Std_prc,only: KI32, KI64
  use TOUZA_Std_prc,only: KFLT, KDBL
  use TOUZA_Std_bld,only: bld_init=>init, bld_diag=>diag, bld_finalize=>finalize
  use TOUZA_Std_ipc
  implicit none
  integer,parameter :: KITGT = TEST_IPC_IKIND
  integer,parameter :: KRTGT = TEST_IPC_RKIND

  real(kind=KRTGT),parameter :: H = HUGE(0.0_KRTGT)
  real(kind=KRTGT) :: F

  integer ierr

  ierr = 0
  call init(ierr)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call finalize(ierr)
  if (ierr.eq.0) call bld_init(ierr)
  if (ierr.eq.0) call bld_diag(ierr, levv=+9)
  if (ierr.eq.0) call bld_finalize(ierr)

  if (ierr.eq.0) call exam_IBITS(ierr, 0_KITGT, levv=0)

101 format('FINAL = ', I0)
  write(*, 101) ierr

  ierr = 0
  call test_hypot(ierr, 3.0_KRTGT, 4.0_KRTGT)
  F = (H / 8.0_KRTGT)
  call test_hypot(ierr, 3.0_KRTGT * F, 4.0_KRTGT * F)

  call test_invht(ierr, 2.0_KRTGT)
  call test_invht(ierr, 1.0_KRTGT)
  call test_invht(ierr, 0.5_KRTGT)
  call test_invht(ierr, 0.0_KRTGT)

  write(*, 101) ierr
  stop
contains
  subroutine test_hypot(ierr, x, y)
    integer,intent(out) :: ierr
    real(kind=KRTGT),intent(in) :: x, y

    real(kind=KRTGT) :: zt, zr, zi

    ierr = 0
    zt = ipc_HYPOT(x, y)
    zi = HYPOT(x, y)
    zr = sqrt(x**2 + y**2)

101 format('hypot:', A, 1x, 2E16.8, 1x E24.16)
    write(*, 101) 'ipc',       x, y, zt
    write(*, 101) 'intrinsic', x, y, zi
    write(*, 101) 'raw',       x, y, zr

  end subroutine test_hypot

  subroutine test_invht(ierr, x)
    integer,intent(out) :: ierr
    real(kind=KRTGT),intent(in) :: x

    real(kind=KRTGT) :: zs, zc, zt
    real(kind=KRTGT) :: ys, yc, yt

    ierr = 0
    zt = ipc_ATANH(x)
    zc = ipc_ACOSH(x)
    zs = ipc_ASINH(x)
#if HAVE_FORTRAN_ACOSH
    yc = ACOSH(x)
#else
    yc = - HUGE(0.0_KRTGT)
#endif
#if HAVE_FORTRAN_ASINH
    ys = ASINH(x)
#else
    ys = - HUGE(0.0_KRTGT)
#endif
#if HAVE_FORTRAN_ATANH
    yt = ATANH(x)
#else
    yt = - HUGE(0.0_KRTGT)
#endif

101 format('invht:', A, 1x, 2ES24.16, 1x, ES24.16)
    write(*, 101) 'asinh', zs, ys, zs-ys
    write(*, 101) 'acosh', zc, yc, zc-yc
    write(*, 101) 'atanh', zt, yt, zt-yt
  end subroutine test_invht

end program test_std_ipc

#endif /* TEST_STD_IPC */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

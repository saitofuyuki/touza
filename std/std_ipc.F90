!!!_! std_ipc.F90 - touza/std intrinsic procedures compatible gallery
! Maintainer: SAITO Fuyuki
! Created: Feb 25 2023
#define TIME_STAMP 'Time-stamp: <2025/07/17 08:19:07 fuyuki std_ipc.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2023-2025
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
!!!_ + ETIME
#define HAVE_FORTRAN_ETIME 0   /* RESERVED */
#ifndef   HAVE_FORTRAN_ETIME
#  if HAVE_FORTRAN_ETIME_8
#    define HAVE_FORTRAN_ETIME 8
#  elif HAVE_FORTRAN_ETIME_4
#    define HAVE_FORTRAN_ETIME 4
#  else
#    define HAVE_FORTRAN_ETIME 0
#  endif
#endif
!!!_ + debug
#ifndef   TEST_STD_IPC
#  define TEST_STD_IPC 0
#endif
!!!_ + switches (_PCASE)
#define _NAVL 0  /* not available */
#define _SUBR 1  /* subroutine */
#define _MFUN 2  /* module function */
#define _OFUN 3  /* other function */
#define _SUBR_SX 4  /* subroutine (special for getcwd) */
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

  interface ipc_ETIME
     module procedure ipc_ETIME_d, ipc_ETIME_f
  end interface ipc_ETIME

!!!_  - public
  public init, diag, finalize
  public ipc_IBITS,  exam_IBITS
  public ipc_HYPOT
  public ipc_ASINH,  ipc_ACOSH, ipc_ATANH
  public ipc_ETIME
  public ipc_GETCWD, ipc_CHDIR
  public ipc_EXIT
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: choice
    ! use TOUZA_Std_prc,only: prc_init=>init  ! included by TOUZA_Std_utl
    ! use TOUZA_Std_utl,only: utl_init=>init  ! included by TOUZA_Std_log
    use TOUZA_Std_log,only: log_init=>init
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
          ! if (ierr.eq.0) call prc_init(ierr, ulog, levv=lv, mode=lmd)
          ! if (ierr.eq.0) call utl_init(ierr, ulog, levv=lv, mode=lmd)
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
    use TOUZA_Std_utl,only: choice
    ! use TOUZA_Std_prc,only: prc_diag=>diag
    ! use TOUZA_Std_utl,only: utl_diag=>diag
    use TOUZA_Std_log,only: log_diag=>diag, msg_mdl
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
          ! if (ierr.eq.0) call prc_diag(ierr, utmp, lv, mode=lmd)
          ! if (ierr.eq.0) call utl_diag(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call log_diag(ierr, utmp, lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: choice
    ! use TOUZA_Std_prc,only: prc_finalize=>finalize
    ! use TOUZA_Std_utl,only: utl_finalize=>finalize
    use TOUZA_Std_log,only: log_finalize=>finalize
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
          ! if (ierr.eq.0) call prc_finalize(ierr, utmp, lv, mode=lmd)
          ! if (ierr.eq.0) call utl_finalize(ierr, utmp, lv, mode=lmd)
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
  function ipc_IBITS_i(I, POS, LEN) result(n)
    use TOUZA_Std_prc,only: KTGT=>KI32
    implicit none
    integer(kind=KTGT) :: n
    integer(kind=KTGT),intent(in) :: I
    integer,           intent(in) :: POS, LEN
    integer(kind=KTGT),parameter  :: FB = -1_KTGT
    n = IAND(ISHFT(I, - POS), NOT(ISHFT(FB, LEN)))
  end function ipc_IBITS_i
  ELEMENTAL &
  function ipc_IBITS_l(I, POS, LEN) result(n)
    use TOUZA_Std_prc,only: KTGT=>KI64
    implicit none
    integer(kind=KTGT) :: n
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
  function ipc_HYPOT_f(X, Y) result(r)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    real(kind=KTGT) :: r
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
  function ipc_HYPOT_d(X, Y) result(r)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: r
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
  function ipc_ASINH_f(X) result(r)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    real(kind=KTGT) :: r
    real(kind=KTGT),intent(in) :: X
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT

    r = LOG(X + ipc_HYPOT(ONE, X))
    return
  end function ipc_ASINH_f
  ELEMENTAL &
  function ipc_ASINH_d(X) result(r)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: r
    real(kind=KTGT),intent(in) :: X
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT

    r = LOG(X + ipc_HYPOT(ONE, X))
    return
  end function ipc_ASINH_d

!!!_  - ipc_ACOSH
  ELEMENTAL &
  function ipc_ACOSH_f(X) result(r)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    real(kind=KTGT) :: r
    real(kind=KTGT),intent(in) :: X
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT

    r = LOG(X + SQRT((X - ONE) * (X + ONE)))
    return
  end function ipc_ACOSH_f
  ELEMENTAL &
  function ipc_ACOSH_d(X) result(r)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: r
    real(kind=KTGT),intent(in) :: X
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT

    r = LOG(X + SQRT((X - ONE) * (X + ONE)))
    return
  end function ipc_ACOSH_d

!!!_  - ipc_ATANH
  ELEMENTAL &
  function ipc_ATANH_f(X) result(r)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    real(kind=KTGT) :: r
    real(kind=KTGT),intent(in) :: X
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT

    r = LOG((ONE + X) / (ONE - X)) / TWO
    return
  end function ipc_ATANH_f
  ELEMENTAL &
  function ipc_ATANH_d(X) result(r)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: r
    real(kind=KTGT),intent(in) :: X
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT

    r = LOG((ONE + X) / (ONE - X)) / TWO
    return
  end function ipc_ATANH_d

!!!_ + ETIME - Execution time
  subroutine ipc_ETIME_d(VALUES, TIME)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out) :: VALUES(2)
    real(kind=KTGT),intent(out) :: TIME
#if HAVE_FORTRAN_ETIME
    integer,parameter :: karg = HAVE_FORTRAN_ETIME
    real(kind=karg) :: v(2), t
    call ETIME(v, t)
    VALUES(1:2) = real(v(1:2), kind=KTGT)
    TIME = real(TIME, kind=KTGT)
#else
    VALUES(1:2) = 0.0_KTGT
    TIME = 0.0_KTGT
#endif
  end subroutine ipc_ETIME_d
  subroutine ipc_ETIME_f(VALUES, TIME)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    real(kind=KTGT),intent(out) :: VALUES(2)
    real(kind=KTGT),intent(out) :: TIME
#if HAVE_FORTRAN_ETIME
    integer,parameter :: karg = HAVE_FORTRAN_ETIME
    real(kind=karg) :: v(2), t
    call ETIME(v, t)
    VALUES(1:2) = real(v(1:2), kind=KTGT)
    TIME = real(TIME, kind=KTGT)
#else
    VALUES(1:2) = 0.0_KTGT
    TIME = 0.0_KTGT
#endif
  end subroutine ipc_ETIME_f
!!!_ + chdir
#ifdef _PCASE
#undef _PCASE
#endif
  subroutine ipc_CHDIR(PATH, status, flag)
#if   HAVE_FORTRAN_F90_UNIX_DIR_CHDIR
#   define _PCASE _SUBR
    use F90_UNIX_DIR,only: CHDIR
#elif HAVE_FORTRAN_IFPORT_CHDIR
#   define _PCASE _MFUN
    use IFPORT,only: CHDIR
#elif HAVE_FORTRAN_CHDIR
#   define _PCASE _OFUN
#else
#   define _PCASE _NAVL
#endif
    use TOUZA_Std_utl,only: choice
    implicit none
    character(len=*),intent(in)  :: PATH
    integer,         intent(out) :: status
    integer,optional,intent(in)  :: flag    ! if nonzero return status as is
    integer f
#if _PCASE == _OFUN
    integer :: CHDIR
#endif
#if _PCASE == _SUBR
    call CHDIR(PATH, status)
#elif _PCASE == _MFUN || _PCASE == _OFUN
    status = CHDIR(PATH)
#else
    status = ERR_NOT_IMPLEMENTED
#endif
    f = choice(0, flag)
    if (f.eq.0) then
       if (status.ne.0) status = ERR_INVALID_PARAMETER
    endif
  end subroutine ipc_CHDIR
!!!_ + getcwd
!!!_  - note
  !! F90_UNIX_DIR:GETCWD   (subroutine)   GETCWD(PATH[o], LENPATH[o], ERRNO[o])
  !! gcc/GETCWD            (subroutine)   GETCWD(C, STATUS[o])
  !!                       (function)     integer GETCWD(C)
  !! nv/GETCWD             (subroutine)   GETCWD(C)
  !! IFPORT:GETCWD         (function)     integer GETCWD(DIRNAME)
!!!_  - code
#ifdef _PCASE
#undef _PCASE
#endif
  subroutine ipc_GETCWD(PATH, status, flag)
#if   HAVE_FORTRAN_F90_UNIX_DIR_GETCWD
#   define _PCASE _SUBR_SX
    use F90_UNIX_DIR,only: GETCWD
#elif HAVE_FORTRAN_IFPORT_GETCWD
#   define _PCASE _MFUN
    use IFPORT,only: GETCWD
#elif HAVE_FORTRAN_GETCWD
#   define _PCASE _OFUN
#else
#   define _PCASE 0
#endif
    use TOUZA_Std_utl,only: choice
    implicit none
    character(len=*),intent(out) :: PATH
    integer,         intent(out) :: status
    integer,optional,intent(in)  :: flag
    integer f
#if _PCASE == _OFUN
    integer GETCWD
#endif
#if _PCASE == _SUBR
#  if __NVCOMPILER
    call GETCWD(PATH)
    status = 0
#  else
    call GETCWD(PATH, status)
#  endif
#elif _PCASE == _SUBR_SX
    call GETCWD(PATH, errno=status)
#elif _PCASE == _MFUN || _PCASE == _OFUN
    status = GETCWD(PATH)
#else
    status = ERR_NOT_IMPLEMENTED
#endif
    f = choice(0, flag)
    if (f.eq.0) then
       if (status.ne.0) status = ERR_INVALID_PARAMETER
    endif
  end subroutine ipc_GETCWD
!!!_ & ipc_exit
  subroutine ipc_EXIT(ierr)
    implicit none
    integer,intent(in) :: ierr
    integer jerr
    jerr = max(-255, min(255, ierr))
#if HAVE_FORTRAN_EXIT
    call exit(jerr)
#else /* default */
    stop jerr
#endif /* default */
  end subroutine ipc_EXIT
!!!_ + end TOUZA_Std_ipc
end module TOUZA_Std_ipc

!!!_@ test_std_ipc - test program
#if TEST_STD_IPC
#if TEST_STD_IPC <= 2
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
  if (ierr.eq.0) call bld_init(ierr, mode=MODE_SURFACE)
  if (ierr.eq.0) call bld_diag(ierr, levv=+9)
  if (ierr.eq.0) call bld_finalize(ierr, levv=+9)

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

  call test_etime(ierr)

  call test_dir(ierr)

  call finalize(ierr, levv=+9)
  if (ierr.ne.0) call ipc_EXIT(ierr)
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

  subroutine test_etime(ierr)
    integer,intent(out) :: ierr
    integer j, k
    integer,parameter :: n = 65536 * 4

    real(kind=KRTGT) :: V(2), T

    ierr = 0

101 format('etime: ', I0, 2(1x, F0.4), 1x, F0.4)

    call ipc_ETIME(V, T)
    write(*, 101) n, V, T

    k = 0
    do j = 0, n - 1
       k = k + 1 ! dummy
    enddo

    call ipc_ETIME(V, T)
    write(*, 101) k, V, T

    return
  end subroutine test_etime

  subroutine test_dir(ierr)
    implicit none
    integer,intent(out) :: ierr

    character(len=1024) :: path0, path1

101 format('pwd[', I0, '] ', A)
102 format('chdir[', I0, '] ', A)

    call ipc_GETCWD(path0, ierr)
    write(*, 101) ierr, trim(path0)

    path1 = ' non exist'
    call ipc_CHDIR(path1, ierr)
    write(*, 102) ierr, trim(path1)
    call ipc_GETCWD(path1, ierr)
    write(*, 101) ierr, trim(path1)

    path1 = '..'
    call ipc_CHDIR(path1, ierr)
    write(*, 102) ierr, trim(path1)
    call ipc_GETCWD(path1, ierr)
    write(*, 101) ierr, trim(path1)

    path1 = path0
    call ipc_CHDIR(path1, ierr)
    write(*, 102) ierr, trim(path1)
    call ipc_GETCWD(path1, ierr)
    write(*, 101) ierr, trim(path1)

  end subroutine test_dir

end program test_std_ipc
#else /* TEST_STD_IPC == 3 (chdir) */
program test_std_ipc
  use TOUZA_Std_bld,only: bld_init=>init, bld_diag=>diag, bld_finalize=>finalize
  use TOUZA_Std_ipc
  implicit none
  integer ierr
  character(len=*),parameter :: test_file_a = 'chdir-a.dat'
  character(len=*),parameter :: test_file_b = 'chdir-b.dat'
  character(len=*),parameter :: subd = 'sub00'
  integer,parameter :: uchk = 10, upar = 20, usub = 30, udup = 40, ucmp = 50

  ierr = 0
  call init(ierr, levv=-9)
  if (ierr.eq.0) call diag(ierr, levv=-9)
  if (ierr.eq.0) call bld_init(ierr, mode=MODE_SURFACE, levv=-9)
  if (ierr.eq.0) call bld_diag(ierr, levv=-9)
  if (ierr.eq.0) call bld_finalize(ierr, levv=-9)

  if (ierr.eq.0) call test_chdir_prepare(ierr, ' ',  test_file_a)
  if (ierr.eq.0) call test_chdir_prepare(ierr, ' ',  test_file_b)
  if (ierr.eq.0) call test_chdir_prepare(ierr, subd, test_file_a)
  if (ierr.eq.0) call test_chdir_prepare(ierr, subd, test_file_b)

  if (ierr.eq.0) then
     call test_chdir_sub(ierr, '1', subd, test_file_a, usub, desc='sub only')
  endif
  if (ierr.eq.0) close(usub, iostat=ierr)

  if (ierr.eq.0) then
     call test_chdir_sub(ierr, '2', ' ',  test_file_a, upar, desc='parent')
  endif

  if (ierr.eq.0) then
     call test_chdir_sub(ierr, '3', subd, test_file_a, usub, desc='keep parent')
  endif
  if (ierr.eq.0) close(usub, iostat=ierr)

  if (ierr.eq.0) then
     call test_chdir_sub(ierr, '4', subd, test_file_b, usub, desc='no parent open')
  endif
  if (ierr.eq.0) close(usub, iostat=ierr)

  if (ierr.eq.0) then
     call test_chdir_sub(ierr, '5', subd, test_file_a, usub, abs=.TRUE., desc='absolute')
  endif
  if (ierr.eq.0) close(usub, iostat=ierr)

  ! close parent
  if (ierr.eq.0) close(upar, iostat=ierr)
  if (ierr.eq.0) then
     call test_chdir_sub(ierr, '6', subd, test_file_a, usub, desc='after close parent')
  endif
  ! if (ierr.eq.0) close(usub, iostat=ierr)

  ! keep sub open
  if (ierr.eq.0) then
     call test_chdir_sub(ierr, '7', subd, test_file_a, udup, abs=.TRUE., desc='keep sub, absolute')
  endif
  if (ierr.eq.0) close(udup, iostat=ierr)

  ! keep sub open, and open again
  if (ierr.eq.0) then
     call test_chdir_sub(ierr, '8', subd, test_file_a, udup, desc='keep sub, same')
  endif

  call finalize(ierr, levv=-9)
  if (ierr.ne.0) call ipc_EXIT(ierr)
  stop
contains
  subroutine test_chdir_prepare(ierr, sub, file)
    implicit none
    integer,intent(out) :: ierr
    character(len=*),intent(in) :: sub, file
    integer u
    character(len=1024) :: path
    character(len=1024) :: txt
    logical bx

    ierr = 0

    if (sub.eq.' ') then
       path = trim(file)
    else
       path = trim(sub) // '/' // trim(file)
    endif
    inquire(file=path, exist=bx, iostat=ierr)
    u = uchk
    if (ierr.eq.0) then
       if (bx) then
          call test_chdir_compare(ierr, path)
       else
          open(u, file=path, iostat=ierr, &
               & status='new', form='formatted', action='write', &
               & access='sequential', iomsg=txt)
          if (ierr.eq.0) write(u, '(A)', iostat=ierr) trim(path)
          if (ierr.eq.0) close(u, iostat=ierr)
          if (ierr.ne.0) then
119          format('Failed to create ', A)
             write(*, 119) trim(path)
          endif
       endif
    endif
  end subroutine test_chdir_prepare

  subroutine test_chdir_sub(ierr, test, sub, file, u, abs, desc)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: test
    character(len=*),intent(in)          :: sub, file
    integer,         intent(in)          :: u
    logical,         intent(in),optional :: abs
    character(len=*),intent(in),optional :: desc
    character(len=1024) :: oldpwd, path, ftmp
    character(len=1024) :: txt, ptxt
    integer utmp
    logical bo

    ierr = 0
301 format('[', A, '] ', A)
    if (sub.eq.' ') then
       path = trim(file)
       write(ptxt, 301) '.', trim(file)
    else
       path = trim(sub) // '/' // trim(file)
       write(ptxt, 301) trim(sub), trim(file)
    endif

    if (sub.ne.' ') then
       call ipc_GETCWD(oldpwd, ierr)
       if (ierr.eq.0) call ipc_CHDIR(sub, ierr)
    endif
    bo = .FALSE.
    if (ierr.eq.0) then
       utmp = u
       if (choice(.FALSE., abs)) then
          call ipc_getcwd(ftmp, ierr)
          if (ierr.eq.0) ftmp = trim(ftmp) // '/' // trim(file)
          if (ierr.eq.0) inquire(opened=bo, file=ftmp, iostat=ierr, iomsg=txt)
          if (ierr.eq.0) then
             open(utmp, file=ftmp, iostat=ierr, &
                  & status='old', form='formatted', action='read', &
                  & access='sequential', iomsg=txt)
          endif
       else
          inquire(opened=bo, file=ftmp, iostat=ierr, iomsg=txt)
          if (ierr.eq.0) then
             open(utmp, file=file, iostat=ierr, &
                  & status='old', form='formatted', action='read', &
                  & access='sequential', iomsg=txt)
          endif
       endif
    endif
101 format('# ', A, ' failed at open: ', A)
102 format('# ', A, ' inquire unit  ', I0, ': ', A)
103 format('# ', A, ' inquire name  ', I0, ': ', A)
104 format('# ', A, ' already opened ', A)
    if (ierr.eq.0) then
       if (bo) then
          write(*, 104) trim(test), trim(file)
       endif
    endif
    if (ierr.ne.0) then
       utmp = -1
       write(*, 101) trim(test), trim(txt)
       if (ierr.eq.0) then
          if (bo) then
             inquire(number=utmp, file=file, iostat=ierr, iomsg=txt)
             write(*, 102) trim(test), utmp, trim(file)
             inquire(unit=utmp, name=ftmp, iostat=ierr, iomsg=txt)
             write(*, 103) trim(test), utmp, trim(ftmp)
          endif
       endif
    endif
201 format('ok ',     A, 1x, '- ', A)
202 format('not ok ', A, 1x, '- ', A)
211 format('ok ',     A, 1x, '- ', A, 1x, '(', A, ')')
212 format('not ok ', A, 1x, '- ', A, 1x, '(', A, ')')
    if (ierr.eq.0) then
       if (utmp.ge.0) then
          call test_chdir_compare(ierr, path, utmp)
          if (ierr.eq.0) then
             if (present(desc)) then
                write(*, 211) trim(test), trim(desc), trim(ptxt)
             else
                write(*, 201) trim(test), trim(ptxt)
             endif
          else
             if (present(desc)) then
                write(*, 212) trim(test), trim(desc), trim(ptxt)
             else
                write(*, 202) trim(test), trim(ptxt)
             endif
          endif
          ierr = 0
       endif
    endif

    if (sub.ne.' ') then
       if (ierr.eq.0) call ipc_chdir(oldpwd, ierr)
    endif
  end subroutine test_chdir_sub

  subroutine test_chdir_compare(ierr, path, u)
    integer,intent(out) :: ierr
    character(len=*),intent(in) :: path
    integer,optional,intent(in) :: u
    integer utmp
    character(len=1024) :: txt

    ierr = 0
    if (present(u)) then
       utmp = u
       rewind(utmp, iostat=ierr)
    else
       utmp = ucmp
       open(utmp, file=path, iostat=ierr, &
            & status='old', form='formatted', action='read', &
            & access='sequential', iomsg=txt)
    endif
    ! write(*, *) ierr, trim(path), '//'

    if (ierr.eq.0) read(utmp, '(A)', iostat=ierr) txt
    if (ierr.eq.0) then
       if (txt.ne.path) then
109       format('Invalid content for test in ', A)
108       format(1x, A)
          write(*, 109) trim(path)
          write(*, 108) trim(txt)
          ierr = -1
       endif
    endif
    if (present(u)) then
       continue
    else
       if (ierr.eq.0) close(utmp, iostat=ierr)
    endif
  end subroutine test_chdir_compare
end program test_std_ipc

#endif /* TEST_STD_IPC == 3 (chdir) */
#endif /* TEST_STD_IPC */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

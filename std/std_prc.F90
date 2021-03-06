!!!_! std_prc.F90 - touza/std precision(kind) manager
! Maintainer: SAITO Fuyuki
! Created: Sep 6 2020
#define TIME_STAMP 'Time-stamp: <2021/03/05 08:35:20 fuyuki std_prc.F90>'
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
!!!_ + Fortran 2003 and later intrinsic modules
#ifndef    OPT_INTEGER_32_KIND
#  define  OPT_INTEGER_32_KIND 0
#endif
#if OPT_INTEGER_32_KIND == 0
#   undef OPT_INTEGER_32_KIND
#   if HAVE_ISO_FORTRAN_ENV_INT32
#      define OPT_INTEGER_32_KIND  INT32
#   elif HAVE_ISO_C_BINDING_C_INT32_T
#      define OPT_INTEGER_32_KIND C_INT32_T
#   else
#      define OPT_INTEGER_32_KIND SELECTED_INT_KIND(9)
#   endif
#endif /* OPT_INTEGER_32_KIND == 0 */

#ifndef    OPT_INTEGER_64_KIND
#  define  OPT_INTEGER_64_KIND 0
#endif
#if OPT_INTEGER_64_KIND == 0
#   undef OPT_INTEGER_64_KIND
#   if HAVE_ISO_FORTRAN_ENV_INT64
#      define OPT_INTEGER_64_KIND  INT64
#   elif HAVE_ISO_C_BINDING_C_INT64_T
#      define OPT_INTEGER_64_KIND C_INT64_T
#   else
#      define OPT_INTEGER_64_KIND SELECTED_INT_KIND(18)
#   endif
#endif /* OPT_INTEGER_64_KIND == 0 */

#ifndef    OPT_INTEGER_8_KIND
#  define  OPT_INTEGER_8_KIND 0
#endif
#if OPT_INTEGER_8_KIND == 0
#   undef OPT_INTEGER_8_KIND
#   if HAVE_ISO_FORTRAN_ENV_INT8
#      define OPT_INTEGER_8_KIND  INT8
#   elif HAVE_ISO_C_BINDING_C_INT64_T
#      define OPT_INTEGER_8_KIND C_INT8_T
#   else
#      define OPT_INTEGER_8_KIND SELECTED_INT_KIND(2)
#   endif
#endif /* OPT_INTEGER_8_KIND == 0 */

!!!_@ TOUZA_Std_prc - precision
module TOUZA_Std_prc
!!!_ = declaration
#if HAVE_ISO_FORTRAN_ENV
  use ISO_FORTRAN_ENV
#endif
#if HAVE_IEEE_ARITMETIC
  use IEEE_ARITHMETIC
#endif
  implicit none
  private
# define __TAG__ STD_FORMAT_MDL('prc')
!!!_  - real precisions
  integer,parameter :: dflt = OPT_REAL_SINGLE_DIGITS
  integer,parameter :: xflt = OPT_REAL_SINGLE_EXP
  integer,parameter :: ddbl = OPT_REAL_DOUBLE_DIGITS
  integer,parameter :: xdbl = OPT_REAL_DOUBLE_EXP

  integer,parameter,public :: KFLT = SELECTED_REAL_KIND(dflt, xflt)
  integer,parameter,public :: KDBL = SELECTED_REAL_KIND(ddbl, xdbl)
!!!_  - integer precisions
  integer,parameter,public :: KI8  = OPT_INTEGER_8_KIND
  integer,parameter,public :: KI32 = OPT_INTEGER_32_KIND
  integer,parameter,public :: KI64 = OPT_INTEGER_64_KIND
!!!_  - interfaces
  interface check_real_zero
     module procedure check_real_zero_d
  end interface check_real_zero
  interface check_real_one
     module procedure check_real_one_d
  end interface check_real_one
  interface check_real_inf
     module procedure check_real_inf_d
  end interface check_real_inf
  interface check_real_dnm
     module procedure check_real_dnm_d
  end interface check_real_dnm

  interface diag_real_props
     module procedure diag_real_props_d, diag_real_props_f
  end interface diag_real_props
!!!_  - public procedures
  public init, diag, finalize
  public diag_real_kinds, diag_real_props
  public diag_int_kinds
  public check_real_zero, check_real_one, check_real_inf, check_real_dnm
!!!_  - static
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, levv, mode)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: levv, mode
    integer md

    ierr = 0

    md = idef(mode, INIT_DEFAULT)
    if (md.eq.INIT_DEFAULT) md = INIT_DEEP

    if (md.gt.INIT_DEFAULT) then
       if (init_counts.eq.0) then
          lev_verbose = idef(levv, lev_verbose)
       endif
       init_counts = init_counts + 1
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md

    ierr = 0
    utmp = idef(u)
    lv = idef(levv, lev_verbose)
    md = idef(mode, DIAG_DEFAULT)
    if (md.eq.DIAG_DEFAULT) md = DIAG_DEEP

    if (md.gt.DIAG_DEFAULT) then
       if (diag_counts.eq.0.or.IAND(md,DIAG_FORCE).gt.0) then
101       format(__TAG__, A)
          if (VCHECK_NORMAL(lv)) then
             if (utmp.ge.0) then
                write(utmp, 101) TIME_STAMP
             else
                write(*,  101) TIME_STAMP
             endif
          endif
201       format(__TAG__, A, ' = ', I0, 2x, I0, ',', I0)
202       format(__TAG__, A, ' = ', I0, 2x, I0)
          if (VCHECK_NORMAL(lv)) then
             if (utmp.ge.0) then
                write(utmp, 201) 'single', KFLT, dflt, xflt
                write(utmp, 201) 'double', KDBL, ddbl, xdbl
                write(utmp, 202) 'int8',   KI8,  BIT_SIZE(INT(0, KIND=KI8))
                write(utmp, 202) 'int32',  KI32, BIT_SIZE(INT(0, KIND=KI32))
                write(utmp, 202) 'int64',  KI64, BIT_SIZE(INT(0, KIND=KI64))
             else
                write(*, 201) 'single', KFLT, dflt, xflt
                write(*, 201) 'double', KDBL, ddbl, xdbl
                write(*, 202) 'int8',   KI8,  BIT_SIZE(INT(0, KIND=KI8))
                write(*, 202) 'int32',  KI32, BIT_SIZE(INT(0, KIND=KI32))
                write(*, 202) 'int64',  KI64, BIT_SIZE(INT(0, KIND=KI64))
             endif
          endif
          if (VCHECK_DETAIL(lv)) then
             if (ierr.eq.0) call diag_int_kinds(ierr, u)
             if (ierr.eq.0) call diag_real_kinds(ierr, u)
             if (ierr.eq.0) call diag_real_props(ierr, real(0, KIND=KFLT), u)
             if (ierr.eq.0) call diag_real_props(ierr, real(0, KIND=KDBL), u)
          endif
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & diag_real_props - real properties diagnosis
  subroutine diag_real_props_d &
       & (ierr, v, u)
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KDBL),intent(in)          :: v
    integer,        intent(in),optional :: u
    integer utmp, k
    ierr = 0
    utmp = idef(u)
    k = KIND(v)
101 format(__TAG__, 'real:', I0, ' fraction = ', I0, 1x, I0)
102 format(__TAG__, 'real:', I0, ' exponent = ', I0, 1x, I0)
103 format(__TAG__, 'real:', I0, ' precision = ', I0)
    if (utmp.ge.0) then
       write(utmp, 101) k, RADIX(v), DIGITS(v)
       write(utmp, 102) k, MINEXPONENT(v), MAXEXPONENT(v)
       write(utmp, 103) k, PRECISION(v)
    else
       write(*, 101) k, RADIX(v), DIGITS(v)
       write(*, 102) k, MINEXPONENT(v), MAXEXPONENT(v)
       write(*, 103) k, PRECISION(v)
    endif
    return
  end subroutine diag_real_props_d

  subroutine diag_real_props_f &
       & (ierr, v, u)
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KFLT),intent(in)          :: v
    integer,        intent(in),optional :: u
    integer utmp, k
    ierr = 0
    utmp = idef(u)
    k = KIND(v)
101 format(__TAG__, 'real:', I0, ' fraction = ', I0, 1x, I0)
102 format(__TAG__, 'real:', I0, ' exponent = ', I0, 1x, I0)
103 format(__TAG__, 'real:', I0, ' precision = ', I0)
    if (utmp.ge.0) then
       write(utmp, 101) k, RADIX(v), DIGITS(v)
       write(utmp, 102) k, MINEXPONENT(v), MAXEXPONENT(v)
       write(utmp, 103) k, PRECISION(v)
    else
       write(*, 101) k, RADIX(v), DIGITS(v)
       write(*, 102) k, MINEXPONENT(v), MAXEXPONENT(v)
       write(*, 103) k, PRECISION(v)
    endif
    return
  end subroutine diag_real_props_f

!!!_  & diag_int_kinds - brute-force checker of integer kinds
  subroutine diag_int_kinds &
       & (ierr, u, maxr, minr)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: maxr, minr
    integer rbgn, rend, jr
    integer utmp
    integer k
    integer kprv, knxt

    ierr = 0
    utmp = idef(u)
    rbgn  = idef(minr, 1)
    rend = max(rbgn+1, idef(maxr, 128))

    kprv = selected_int_kind(rbgn)

101 format(__TAG__, 'integer:', I0, ' = ', I0)
    do jr = rbgn, rend
       k = selected_int_kind(jr)
       if (kprv.ne.k) then
          knxt = k
          if (utmp.ge.0) then
             write(utmp, 101) kprv, jr - 1
          else
             write(*, 101) kprv, jr - 1
          endif
          kprv = knxt
       endif
       if (knxt.lt.0) exit
    enddo

  end subroutine diag_int_kinds

!!!_  & diag_real_kinds - brute-force checker of real kinds
  subroutine diag_real_kinds &
       & (ierr, u, maxp, minp)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: maxp, minp
    integer jp, pend
    integer jx
    integer utmp
    integer k
    integer kprv, knxt

    ierr = 0
    utmp = idef(u)
    jp   = idef(minp, 1)
    pend = max(jp, idef(maxp, 128))

    kprv = selected_real_kind(jp)

101 format(__TAG__, 'real:', I0, ' = ', I0, ',', I0)
    do jp = jp, pend
       k = selected_real_kind(jp)
       if (kprv.ne.k) then
          knxt = k
          jx = 1
          do
             k = selected_real_kind(jp - 1, jx)
             if (kprv.ne.k) exit
             jx = jx + 1
          enddo
          if (utmp.ge.0) then
             write(utmp, 101) kprv, jp - 1, jx - 1
          else
             write(*, 101) kprv, jp - 1, jx - 1
          endif
          kprv = knxt
       endif
       if (knxt.lt.0) exit
    enddo

  end subroutine diag_real_kinds

!!!_   & finalize
  subroutine finalize(ierr, u, levv, mode)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    ierr = 0 * idef(u) * idef(levv) * idef(mode) ! dummy
    return
  end subroutine finalize

!!!_ + real properties checker
!!!_  & check_real_zero - check real(0) properties, return exponent
  subroutine check_real_zero_d &
       & (kx, v, u, levv)
    implicit none
    integer,parameter :: KTGT = KDBL
    integer,        intent(out)         :: kx
    real(kind=KTGT),intent(in)          :: v
    integer,        intent(in),optional :: u
    integer,        intent(in),optional :: levv
    real(kind=KTGT) :: fr, t
    integer utmp, lv
    ! SHOULD RETURN 0 under fortran 95 standard (0 = 0 * 2^0)

    utmp = idef(u)
    lv = idef(levv, lev_verbose)

    t = real(0.0, kind=KIND(v))
    fr = FRACTION(t)
    kx = EXPONENT(t)

    if (fr.ne.t) kx = - HUGE(kx)

101 format(__TAG__, 'real:', I0, ' zero = ', I0, ' (', F5.1, ')')
    if (VCHECK_DEBUG(lv) &
         & .or. (kx.ne.0 .and. VCHECK_NORMAL(lv))) then
       if (utmp.ge.0) then
          write(utmp, 101) KIND(v), kx, fr
       else
          write(*,    101) KIND(v), kx, fr
       endif
    endif

    return
  end subroutine check_real_zero_d

!!!_  & check_real_one - check real(1) properties, return exponent
  subroutine check_real_one_d &
       & (kx, v, u, levv)
    implicit none
    integer,parameter :: KTGT = KDBL
    integer,        intent(out)         :: kx
    real(kind=KTGT),intent(in)          :: v
    integer,        intent(in),optional :: u
    integer,        intent(in),optional :: levv
    real(kind=KTGT) :: fr, t
    integer utmp, lv
    ! SHOULD RETURN 1 under fortran 95 standard (1 = 0.5 * 2^1)

    utmp = idef(u)
    lv = idef(levv, lev_verbose)

    t = real(1.0, kind=KIND(v))
    fr = FRACTION(t)
    kx = EXPONENT(t)

    if (fr.eq.real(0.0, kind=KIND(v))) kx = - HUGE(kx)

101 format(__TAG__, 'real:', I0, ' one = ', I0, ' (', F5.1, ')')
    if (VCHECK_DETAIL(lv) &
         & .or. (kx.ne.1 .and. VCHECK_NORMAL(lv))) then
       if (utmp.ge.0) then
          write(utmp, 101) KIND(v), kx, fr
       else
          write(*,    101) KIND(v), kx, fr
       endif
    endif

    return
  end subroutine check_real_one_d

!!!_  & check_real_inf - check infinity properties
  subroutine check_real_inf_d &
       & (istt, v, u, levv)
    implicit none
    integer,parameter :: KTGT = KDBL
    integer,        intent(out)         :: istt
    real(kind=KTGT),intent(in)          :: v
    integer,        intent(in),optional :: u
    integer,        intent(in),optional :: levv
    real(kind=KTGT) :: fh, th, ti
    integer kh, kx, km
    integer utmp, lv
    ! SHOULD RETURN 0 under fortran 95 standard
    ! check if exponent(huge) ==  maxexponent
    !       if exponent(huge*b) > maxexponent
    !       if exponent(fr*(b**(me+1))) > maxexponent

    istt = 0

    utmp = idef(u)
    lv = idef(levv, lev_verbose)

    th = HUGE(v)
    km = MAXEXPONENT(v)
    fh = FRACTION(th)
    kh = EXPONENT(th)

    if (km.ne.kh) istt = -1

    ti = th * REAL(RADIX(v), kind=KIND(v))
    kx = EXPONENT(ti)
    if (kx.le.kh) istt = -1

    ti = set_exponent(fh, kh+1)
    kx = EXPONENT(ti)
    if (kx.le.kh) istt = -1

101 format(__TAG__, 'real:', I0, ' inf = ', I0, ' (', I0, 1x, I0, 1x, I0, ')')
    if (VCHECK_DETAIL(lv) &
         & .or. (kx.ne.1 .and. VCHECK_NORMAL(lv))) then
       if (utmp.ge.0) then
          write(utmp, 101) KIND(v), istt, kx, kh, km
       else
          write(*,    101) KIND(v), istt, kx, kh, km
       endif
    endif

    return
  end subroutine check_real_inf_d

!!!_  & check_real_dnm - check denormal properties
  subroutine check_real_dnm_d &
       & (kx, v, u, levv)
    implicit none
    integer,parameter :: KTGT = KDBL
    integer,        intent(out)         :: kx
    real(kind=KTGT),intent(in)          :: v
    integer,        intent(in),optional :: u
    integer,        intent(in),optional :: levv
    integer utmp, lv
    real(kind=KTGT) :: ti, fi, t, z
    integer ki, kr
    integer j
    ! RETURN 0 no denormalized
    ! RETURN negative if with denormalized (exponents to be expressed),
    !        should be -(DIGITS-1).

    kx = 0
    utmp = idef(u)
    lv = idef(levv, lev_verbose)

    ti = +TINY(V)
    z  = REAL(0, kind=KIND(v))

    t = ti
    kr = 0
201 format(__TAG__, 'real:', I0, 1x, I0, 1x, I0)
    do j = 1, DIGITS(v) + 1
       t = t / REAL(RADIX(v), kind=KIND(v))
       if (VCHECK_DEBUG(lv)) then
          if (utmp.ge.0) then
             write(utmp, 201) kind(v), j, exponent(t)
          else
             write(*,    201) kind(v), j, exponent(t)
          endif
       endif
       if (t.eq.z) then
          kr = j
          exit
       endif
    enddo
    if (kr.eq.0) then
       kx = HUGE(kx)
    else
       kx = 1 - kr
    endif
    if (kx.lt.0) then
       ki = EXPONENT(ti)
       fi = FRACTION(ti)
       ! kr SHOULD BE (ki - 1)
       kr = EXPONENT(ti / REAL(RADIX(v), kind=KIND(v)))
       if (kr.ne.ki-1) kx = 1
       kr = EXPONENT(SET_EXPONENT(fi, ki-1))
       if (kr.ne.ki-1) kx = 2
    endif

101 format(__TAG__, 'real:', I0, ' dnm = ', I0, 1x, 2F4.1)
    if (VCHECK_DETAIL(lv) &
         & .or. (kx.ne.0 .and. VCHECK_NORMAL(lv))) then
       if (kx.le.0) then
          t  = SET_EXPONENT(TINY(v), MINEXPONENT(v) + kx)
          z  = SET_EXPONENT(TINY(v), MINEXPONENT(v) + kx - 1)
       else
          t  = real(1, kind=KIND(v))
          z  = real(1, kind=KIND(v))
       endif
       if (utmp.ge.0) then
          write(utmp, 101) KIND(v), kx, FRACTION(t), FRACTION(z)
       else
          write(*,    101) KIND(v), kx, FRACTION(t), FRACTION(z)
       endif
    endif

    return
  end subroutine check_real_dnm_d

!!!_ + private subroutines
!!!_  & idef - choice() stand-alone
  integer function idef(i0, i1) result(iv)
    implicit none
    integer,intent(in),optional :: i0, i1
    if (present(i0)) then
       iv = i0
    else if (present(i1)) then
       iv = i1
    else
       iv = -1
    endif
    return
  end function idef
end module TOUZA_Std_prc

!!!_@ test_std_prc - test program
#ifdef TEST_STD_PRC
program test_std_prc
  use TOUZA_Std_prc
  implicit none
  integer ierr
  integer istt
  real(kind=KDBL) :: VSH = 0.0

  call init(ierr)
  if (ierr.eq.0) call diag(ierr, levv=-1)
  if (ierr.eq.0) call diag(ierr, levv=10, mode=DIAG_DEEP_FORCE)
  if (ierr.eq.0) call finalize(ierr)

  if (ierr.eq.0) then
     call diag_real_props(istt, real(0, kind=KFLT))
     call diag_real_props(istt, real(0, kind=KDBL))
  endif
  if (ierr.eq.0) then
     call check_real_zero(istt, VSH, levv=10)
     call check_real_one(istt, VSH, levv=10)
     call check_real_inf(istt, VSH, levv=10)
     call check_real_dnm(istt, VSH, levv=10)
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

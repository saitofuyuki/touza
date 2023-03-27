!!!_! std_prc.F90 - TOUZA/Std precision(kind) manager
! Maintainer: SAITO Fuyuki
! Created: Sep 6 2020
#define TIME_STAMP 'Time-stamp: <2023/03/26 12:04:57 fuyuki std_prc.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2020-2023
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
!!!_ + decimal precision for floats
#ifndef    OPT_REAL_SINGLE_DIGITS
#  define  OPT_REAL_SINGLE_DIGITS  6
#endif
#ifndef    OPT_REAL_DOUBLE_DIGITS
#  define  OPT_REAL_DOUBLE_DIGITS  15
#endif
#ifndef    OPT_REAL_QUADRUPLE_DIGITS
#  define  OPT_REAL_QUADRUPLE_DIGITS -1        /* disabled default */
!! #  define  OPT_REAL_QUADRUPLE_DIGITS 33
#endif
!!!_ + decimal exponent range for floats
#ifndef    OPT_REAL_SINGLE_EXP
#  define  OPT_REAL_SINGLE_EXP 37
#endif
#ifndef    OPT_REAL_DOUBLE_EXP
#  define  OPT_REAL_DOUBLE_EXP 307
#endif
#ifndef    OPT_REAL_QUADRUPLE_EXP
#  define  OPT_REAL_QUADRUPLE_EXP 4931
#endif
!!!_ + Fortran 2003 and later intrinsic modules
#ifndef    OPT_INTEGER_32_KIND
#  define  OPT_INTEGER_32_KIND 0
#endif
#if OPT_INTEGER_32_KIND == 0
#   undef OPT_INTEGER_32_KIND
#   if HAVE_FORTRAN_ISO_FORTRAN_ENV_INT32
#      define OPT_INTEGER_32_KIND  INT32
#   elif HAVE_FORTRAN_ISO_C_BINDING_C_INT32_T
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
#   if HAVE_FORTRAN_ISO_FORTRAN_ENV_INT64
#      define OPT_INTEGER_64_KIND  INT64
#   elif HAVE_FORTRAN_ISO_C_BINDING_C_INT64_T
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
#   if HAVE_FORTRAN_ISO_FORTRAN_ENV_INT8
#      define OPT_INTEGER_8_KIND  INT8
#   elif HAVE_FORTRAN_ISO_C_BINDING_C_INT8_T
#      define OPT_INTEGER_8_KIND C_INT8_T
#   else
#      define OPT_INTEGER_8_KIND SELECTED_INT_KIND(2)
#   endif
#endif /* OPT_INTEGER_8_KIND == 0 */

#ifndef    OPT_INTEGER_16_KIND
#  define  OPT_INTEGER_16_KIND 0
#endif
#if OPT_INTEGER_16_KIND == 0
#   undef OPT_INTEGER_16_KIND
#   if HAVE_FORTRAN_ISO_FORTRAN_ENV_INT16
#      define OPT_INTEGER_16_KIND  INT16
#   elif HAVE_FORTRAN_ISO_C_BINDING_C_INT16_T
#      define OPT_INTEGER_16_KIND C_INT16_T
#   else
#      define OPT_INTEGER_16_KIND SELECTED_INT_KIND(4)
#   endif
#endif /* OPT_INTEGER_16_KIND == 0 */

!!!_@ TOUZA_Std_prc - precision
module TOUZA_Std_prc
!!!_ = declaration
#if HAVE_FORTRAN_ISO_FORTRAN_ENV
  use ISO_FORTRAN_ENV
#endif
#if HAVE_FORTRAN_IEEE_ARITMETIC
  use IEEE_ARITHMETIC
#endif
#if HAVE_FORTRAN_ISO_C_BINDING
  use ISO_C_BINDING
#endif
  implicit none
  private
# define __MDL__ 'prc'
# define __TAG__ STD_FORMAT_MDL(__MDL__)
# define _ERROR(E) (E - ERR_MASK_STD_PRC)
!!!_  - real precisions
  integer,parameter :: dflt = OPT_REAL_SINGLE_DIGITS
  integer,parameter :: xflt = OPT_REAL_SINGLE_EXP
  integer,parameter :: ddbl = OPT_REAL_DOUBLE_DIGITS
  integer,parameter :: xdbl = OPT_REAL_DOUBLE_EXP
  integer,parameter :: dqpl = OPT_REAL_QUADRUPLE_DIGITS
  integer,parameter :: xqpl = OPT_REAL_QUADRUPLE_EXP

  integer,parameter,public :: KFLT = SELECTED_REAL_KIND(dflt, xflt)
  integer,parameter,public :: KDBL = SELECTED_REAL_KIND(ddbl, xdbl)
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  integer,parameter,public :: KQPL = SELECTED_REAL_KIND(dqpl, xqpl)
#else
  integer,parameter,public :: KQPL = -1
#endif
!!!_  - integer precisions
  integer,parameter,public :: KI8  = OPT_INTEGER_8_KIND
  integer,parameter,public :: KI16 = OPT_INTEGER_16_KIND
  integer,parameter,public :: KI32 = OPT_INTEGER_32_KIND
  integer,parameter,public :: KI64 = OPT_INTEGER_64_KIND
!!!_  - interfaces
  interface check_real_props
     module procedure check_real_props_d, check_real_props_f
#if OPT_REAL_QUADRUPLE_DIGITS > 0
     module procedure check_real_props_q
#endif
  end interface check_real_props
  interface check_real_zero
     module procedure check_real_zero_d, check_real_zero_f
#if OPT_REAL_QUADRUPLE_DIGITS > 0
     module procedure check_real_zero_q
#endif
  end interface check_real_zero
  interface check_real_one
     module procedure check_real_one_d,  check_real_one_f
#if OPT_REAL_QUADRUPLE_DIGITS > 0
     module procedure check_real_one_q
#endif
  end interface check_real_one
  interface check_real_inf
     module procedure check_real_inf_d,  check_real_inf_f
#if OPT_REAL_QUADRUPLE_DIGITS > 0
     module procedure check_real_inf_q
#endif
  end interface check_real_inf
  interface check_real_dnm
     module procedure check_real_dnm_d,  check_real_dnm_f
#if OPT_REAL_QUADRUPLE_DIGITS > 0
     module procedure check_real_dnm_q
#endif
  end interface check_real_dnm
  interface check_real_mantissa
     module procedure check_real_mantissa_d, check_real_mantissa_f
#if OPT_REAL_QUADRUPLE_DIGITS > 0
     module procedure check_real_mantissa_q
#endif
  end interface check_real_mantissa
  interface diag_real_props
     module procedure diag_real_props_d, diag_real_props_f
#if OPT_REAL_QUADRUPLE_DIGITS > 0
     module procedure diag_real_props_q
#endif
  end interface diag_real_props
!!!_  - public procedures
  public init, diag, finalize
  public init_set_switches
  public diag_abstract
  public diag_real_kinds, diag_real_props
  public diag_int_kinds
  public check_real_props
  public check_real_zero, check_real_one, check_real_inf, check_real_dnm
  public check_real_mantissa
  public set_defu
!!!_  - static
  integer,save :: init_mode   = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL
  integer,save :: err_default = _ERROR(ERR_NO_INIT)
  integer,save :: ulog = -1

  logical,save :: force_check_dnm = .FALSE.
  logical,save :: force_check_inf = .FALSE.

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, inf, dnm)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    logical,intent(in),optional :: inf,  dnm    ! boolean to report inifinity, denormalized properties
    integer md

    ierr = ERR_SUCCESS

    md = idef(mode, MODE_DEFAULT)
    if (md.eq.MODE_DEFAULT) md = MODE_DEEP
    init_mode = md

    if (md.ge.MODE_SURFACE) then
       err_default = ERR_SUCCESS
       if (init_counts.eq.0.or.IAND(md, MODE_FORCE).gt.0) then
          lev_verbose = idef(levv, lev_verbose)
          ulog = idef(u, ulog)
          call init_set_switches(ierr, inf, dnm)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = _ERROR(ERR_FAILURE_INIT)
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

    ierr = err_default

    md = idef(mode, MODE_DEFAULT)
    if (md.eq.MODE_DEFAULT) md = init_mode
    utmp = idef(u, ulog)
    lv = idef(levv, lev_verbose)

    if (md.ge.MODE_SURFACE) then
       ! loose mode (reset error at initialization)
       if (ierr.ne.0 .and. IAND(md, MODE_LOOSE).gt.0) then
          if (VCHECK_NORMAL(lv)) then
301          format(STD_FORMAT_FUN(__MDL__, 'diag'), ' loose: ', I0)
             if (utmp.ge.0) then
                write(utmp, 301) ierr
             else
                write(*,    301) ierr
             endif
          endif
          ierr = ERR_SUCCESS
       endif
       if (diag_counts.eq.0.or.IAND(md,MODE_FORCE).gt.0) then
101       format(__TAG__, A)
          if (VCHECK_NORMAL(lv)) then
             if (utmp.ge.0) then
                write(utmp, 101) TIME_STAMP
             else
                write(*,    101) TIME_STAMP
             endif
          endif
          if (VCHECK_INFO(lv)) then
111          format(__TAG__, 'OPT_REAL_SINGLE_DIGITS', ' = ', I0)
112          format(__TAG__, 'OPT_REAL_SINGLE_EXP',    ' = ', I0)
121          format(__TAG__, 'OPT_REAL_DOUBLE_DIGITS', ' = ', I0)
122          format(__TAG__, 'OPT_REAL_DOUBLE_EXP',    ' = ', I0)
131          format(__TAG__, 'OPT_REAL_QUADRUPLE_DIGITS', ' = ', I0)
132          format(__TAG__, 'OPT_REAL_QUADRUPLE_EXP',    ' = ', I0)
             if (utmp.ge.0) then
                write(utmp, 111) OPT_REAL_SINGLE_DIGITS
                write(utmp, 112) OPT_REAL_SINGLE_EXP
                write(utmp, 121) OPT_REAL_DOUBLE_DIGITS
                write(utmp, 122) OPT_REAL_DOUBLE_EXP
                write(utmp, 131) OPT_REAL_QUADRUPLE_DIGITS
                write(utmp, 132) OPT_REAL_QUADRUPLE_EXP
             else
                write(*, 111) OPT_REAL_SINGLE_DIGITS
                write(*, 112) OPT_REAL_SINGLE_EXP
                write(*, 121) OPT_REAL_DOUBLE_DIGITS
                write(*, 122) OPT_REAL_DOUBLE_EXP
                write(*, 131) OPT_REAL_QUADRUPLE_DIGITS
                write(*, 132) OPT_REAL_QUADRUPLE_EXP
             endif
          endif
          if (VCHECK_NORMAL(lv)) then
             if (ierr.eq.0) call diag_abstract(ierr, u)
          endif
          if (VCHECK_DETAIL(lv)) then
             if (ierr.eq.0) call diag_int_kinds(ierr, u)
             if (ierr.eq.0) call diag_real_kinds(ierr, u)
             if (ierr.eq.0) call diag_real_props(ierr, real(0, KIND=KFLT), u)
             if (ierr.eq.0) call diag_real_props(ierr, real(0, KIND=KDBL), u)
#if OPT_REAL_QUADRUPLE_DIGITS > 0
             if (ierr.eq.0) call diag_real_props(ierr, real(0, KIND=KQPL), u)
#endif
          endif
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md

    ierr = err_default

    md = idef(mode, MODE_DEFAULT)
    if (md.eq.MODE_DEFAULT) md = init_mode
    utmp = idef(u, ulog)
    lv = idef(levv, lev_verbose)

    if (md.ge.MODE_SURFACE) then
       if (fine_counts.eq.0.or.IAND(md,MODE_FORCE).gt.0) then
          if (VCHECK_DEBUG(lv)) then
311          format(STD_FORMAT_FUN(__MDL__, 'finalize'), 'fine: ', I0, 1x, I0, 1x, I0, 1x, I0)
             if (utmp.ge.0) then
                write(utmp, 311) ierr, init_counts, diag_counts, fine_counts
             else
                write(*,    311) ierr, init_counts, diag_counts, fine_counts
             endif
          endif
       endif
       if (ierr.ne.0 .and. IAND(md, MODE_LOOSE).gt.0) then
          if (VCHECK_NORMAL(lv)) then
301          format(STD_FORMAT_FUN(__MDL__, 'finalize'), 'loose: ', I0)
             if (utmp.ge.0) then
                write(utmp, 301) ierr
             else
                write(*,    301) ierr
             endif
          endif
          ierr = ERR_SUCCESS
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + init subcontracts
  subroutine init_set_switches &
       & (ierr, inf, dnm)
    implicit none
    integer,intent(out) :: ierr
    logical,intent(in),optional :: inf
    logical,intent(in),optional :: dnm

    ierr = ERR_SUCCESS
    if (present(inf)) then
       force_check_inf = inf
    endif
    if (present(dnm)) then
       force_check_dnm = dnm
    endif
  end subroutine init_set_switches

!!!_ + diag subcontracts
!!!_  & diag_abstract - kind abstract
  subroutine diag_abstract &
       & (ierr, u)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer utmp
    ierr = ERR_SUCCESS
    utmp = idef(u, ulog)

201 format(__TAG__, A, ' = ', I0, 2x, I0, ',', I0)
202 format(__TAG__, A, ' = ', I0, 2x, I0)
    if (utmp.ge.0) then
       write(utmp, 201) 'single',    KFLT, dflt, xflt
       write(utmp, 201) 'double',    KDBL, ddbl, xdbl
       write(utmp, 201) 'quadruple', KQPL, dqpl, xqpl
#if HAVE_FORTRAN_ISO_C_BINDING_C_FLOAT
       write(utmp, 201) 'c_float',   C_FLOAT, DIGITS(0.0_C_FLOAT), RANGE(0.0_C_FLOAT)
#endif
#if HAVE_FORTRAN_ISO_C_BINDING_C_DOUBLE
       write(utmp, 201) 'c_double',  C_DOUBLE, DIGITS(0.0_C_DOUBLE), RANGE(0.0_C_DOUBLE)
#endif
#if HAVE_FORTRAN_ISO_C_BINDING_C_LONG_DOUBLE
       write(utmp, 201) 'c_long_double',  C_LONG_DOUBLE, DIGITS(0.0_C_LONG_DOUBLE), RANGE(0.0_C_LONG_DOUBLE)
#endif
       write(utmp, 202) 'int8',   KI8,  BIT_SIZE(INT(0, KIND=KI8))
       write(utmp, 202) 'int16',  KI16, BIT_SIZE(INT(0, KIND=KI16))
       write(utmp, 202) 'int32',  KI32, BIT_SIZE(INT(0, KIND=KI32))
       write(utmp, 202) 'int64',  KI64, BIT_SIZE(INT(0, KIND=KI64))
#if HAVE_FORTRAN_ISO_C_BINDING_C_SIZE_T
       write(utmp, 202) 'c_size_t', C_SIZE_T,  BIT_SIZE(INT(0, KIND=C_SIZE_T))
#endif
#if HAVE_FORTRAN_ISO_C_BINDING_C_INT8_T
       write(utmp, 202) 'c_int8_t', C_INT8_T,  BIT_SIZE(INT(0, KIND=C_INT8_T))
#endif
#if HAVE_FORTRAN_ISO_C_BINDING_C_INT16_T
       write(utmp, 202) 'c_int16_t', C_INT16_T,  BIT_SIZE(INT(0, KIND=C_INT16_T))
#endif
#if HAVE_FORTRAN_ISO_C_BINDING_C_INT32_T
       write(utmp, 202) 'c_int32_t', C_INT32_T,  BIT_SIZE(INT(0, KIND=C_INT32_T))
#endif
#if HAVE_FORTRAN_ISO_C_BINDING_C_INT64_T
       write(utmp, 202) 'c_int64_t', C_INT64_T,  BIT_SIZE(INT(0, KIND=C_INT64_T))
#endif
    else
       write(*,    201) 'single',    KFLT, dflt, xflt
       write(*,    201) 'double',    KDBL, ddbl, xdbl
       write(*,    201) 'quadruple', KQPL, dqpl, xqpl
#if HAVE_FORTRAN_ISO_C_BINDING_C_FLOAT
       write(*,    201) 'c_float',   C_FLOAT, DIGITS(0.0_C_FLOAT), RANGE(0.0_C_FLOAT)
#endif
#if HAVE_FORTRAN_ISO_C_BINDING_C_DOUBLE
       write(*,    201) 'c_double',  C_DOUBLE, DIGITS(0.0_C_DOUBLE), RANGE(0.0_C_DOUBLE)
#endif
#if HAVE_FORTRAN_ISO_C_BINDING_C_LONG_DOUBLE
       write(*,    201) 'c_long_double',  C_LONG_DOUBLE, DIGITS(0.0_C_LONG_DOUBLE), RANGE(0.0_C_LONG_DOUBLE)
#endif
       write(*,    202) 'int8',   KI8,  BIT_SIZE(INT(0, KIND=KI8))
       write(*,    202) 'int16',  KI16, BIT_SIZE(INT(0, KIND=KI16))
       write(*,    202) 'int32',  KI32, BIT_SIZE(INT(0, KIND=KI32))
       write(*,    202) 'int64',  KI64, BIT_SIZE(INT(0, KIND=KI64))
#if HAVE_FORTRAN_ISO_C_BINDING_C_SIZE_T
       write(*,    202) 'c_size_t', C_SIZE_T,  BIT_SIZE(INT(0, KIND=C_SIZE_T))
#endif
#if HAVE_FORTRAN_ISO_C_BINDING_C_INT8_T
       write(*,    202) 'c_int8_t', C_INT8_T,  BIT_SIZE(INT(0, KIND=C_INT8_T))
#endif
#if HAVE_FORTRAN_ISO_C_BINDING_C_INT16_T
       write(*,    202) 'c_int16_t', C_INT16_T,  BIT_SIZE(INT(0, KIND=C_INT16_T))
#endif
#if HAVE_FORTRAN_ISO_C_BINDING_C_INT32_T
       write(*,    202) 'c_int32_t', C_INT32_T,  BIT_SIZE(INT(0, KIND=C_INT32_T))
#endif
#if HAVE_FORTRAN_ISO_C_BINDING_C_INT64_T
       write(*,    202) 'c_int64_t', C_INT64_T,  BIT_SIZE(INT(0, KIND=C_INT64_T))
#endif
    endif
  end subroutine diag_abstract
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

    ierr = ERR_SUCCESS
    utmp = idef(u, ulog)
    rbgn = idef(minr, 1)
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

    ierr = ERR_SUCCESS
    utmp = idef(u, ulog)
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
             write(*,    101) kprv, jp - 1, jx - 1
          endif
          kprv = knxt
       endif
       if (knxt.lt.0) exit
    enddo

  end subroutine diag_real_kinds

!!!_  & diag_real_props - real properties diagnosis
  subroutine diag_real_props_d &
       & (ierr, mold, u)
    implicit none
    integer,parameter :: KTGT = KDBL
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(in)          :: mold
    integer,        intent(in),optional :: u
    ierr = ERR_SUCCESS
    call diag_real_props_core &
         & (ierr,  &
         &  KIND(mold),        RADIX(mold),                  DIGITS(mold),            &
         &  MINEXPONENT(mold), MAXEXPONENT(mold),            PRECISION(mold),         &
         &  RANGE(mold),       EXPONENT(real(1, kind=KTGT)), EXPONENT(EPSILON(mold)), &
         &  u)
    return
  end subroutine diag_real_props_d
  subroutine diag_real_props_f &
       & (ierr, mold, u)
    implicit none
    integer,parameter :: KTGT = KFLT
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(in)          :: mold
    integer,        intent(in),optional :: u
    ierr = ERR_SUCCESS
    call diag_real_props_core &
         & (ierr,  &
         &  KIND(mold),        RADIX(mold),                  DIGITS(mold),            &
         &  MINEXPONENT(mold), MAXEXPONENT(mold),            PRECISION(mold),         &
         &  RANGE(mold),       EXPONENT(real(1, kind=KTGT)), EXPONENT(EPSILON(mold)), &
         &  u)
    return
  end subroutine diag_real_props_f
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  subroutine diag_real_props_q &
       & (ierr, mold, u)
    implicit none
    integer,parameter :: KTGT = KQPL
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(in)          :: mold
    integer,        intent(in),optional :: u
    ierr = ERR_SUCCESS
    call diag_real_props_core &
         & (ierr,  &
         &  KIND(mold),        RADIX(mold),                  DIGITS(mold),            &
         &  MINEXPONENT(mold), MAXEXPONENT(mold),            PRECISION(mold),         &
         &  RANGE(mold),       EXPONENT(real(1, kind=KTGT)), EXPONENT(EPSILON(mold)), &
         &  u)
    return
  end subroutine diag_real_props_q
#endif

  subroutine diag_real_props_core &
       & (ierr, &
       &  k,    rx, d, mine, maxe, pr, rg, e1, ee, u)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: k
    integer,intent(in)          :: rx, d,  mine, maxe
    integer,intent(in)          :: pr, rg, e1,   ee
    integer,intent(in),optional :: u
    integer utmp
    ierr = ERR_SUCCESS
    utmp = idef(u, ulog)
101 format(__TAG__, 'real:', I0, ' fraction = ', I0, 1x, I0)
102 format(__TAG__, 'real:', I0, ' exponent = ', I0, 1x, I0)
103 format(__TAG__, 'real:', I0, ' precision = ', I0, 1x, I0)
104 format(__TAG__, 'real:', I0, ' exponent(1 epsilon) = ', I0, 1x, I0)
    if (utmp.ge.0) then
       write(utmp, 101) k, rx,   d
       write(utmp, 102) k, mine, maxe
       write(utmp, 103) k, pr,   rg
       write(utmp, 104) k, e1,   ee
    else
       write(*,    101) k, rx,   d
       write(*,    102) k, mine, maxe
       write(*,    103) k, pr,   rg
       write(*,    104) k, e1,   ee
    endif
  end subroutine diag_real_props_core

!!!_ + misc
!!!_  & set_defu - set global logging unit
  subroutine set_defu(u)
    implicit none
    integer,intent(in) :: u
    ulog = u
    return
  end subroutine set_defu

!!!_ + real properties checker
!!!_  & check_real_props - batch property checker
  subroutine check_real_props_d &
       & (kx, mold, u, levv)
    implicit none
    integer,parameter :: KTGT = KDBL
    integer,        intent(out)         :: kx
    real(kind=KTGT),intent(in)          :: mold
    integer,        intent(in),optional :: u
    integer,        intent(in),optional :: levv

    integer utmp, lv
    utmp = idef(u, ulog)
    lv = idef(levv, lev_verbose)

    call check_real_zero(kx, mold, u, levv)
    call check_real_one(kx, mold, u, levv)
101 format(__TAG__, 'real:', I0, ' skip to check ', A)
    if (force_check_inf) then
       call check_real_inf(kx, mold, u, levv)
    else if (VCHECK_DETAIL(lv)) then
       if (utmp.ge.0) then
          write(utmp, 101) KTGT, 'infinity'
       else if (utmp.eq.-1) then
          write(*,    101) KTGT, 'infinity'
       endif
    endif
    if (force_check_dnm) then
       call check_real_dnm(kx, mold, u, levv)
    else if (VCHECK_DETAIL(lv)) then
       if (utmp.ge.0) then
          write(utmp, 101) KTGT, 'denormalized'
       else if (utmp.eq.-1) then
          write(*,    101) KTGT, 'denormalized'
       endif
    endif
    return
  end subroutine check_real_props_d
  subroutine check_real_props_f &
       & (kx, mold, u, levv)
    implicit none
    integer,parameter :: KTGT = KFLT
    integer,        intent(out)         :: kx
    real(kind=KTGT),intent(in)          :: mold
    integer,        intent(in),optional :: u
    integer,        intent(in),optional :: levv

    integer utmp, lv
    utmp = idef(u, ulog)
    lv = idef(levv, lev_verbose)

    call check_real_zero(kx, mold, u, levv)
    call check_real_one(kx, mold, u, levv)
101 format(__TAG__, 'real:', I0, ' skip to check ', A)
    if (force_check_inf) then
       call check_real_inf(kx, mold, u, levv)
    else if (VCHECK_DETAIL(lv)) then
       if (utmp.ge.0) then
          write(utmp, 101) KTGT, 'infinity'
       else if (utmp.eq.-1) then
          write(*,    101) KTGT, 'infinity'
       endif
    endif
    if (force_check_dnm) then
       call check_real_dnm(kx, mold, u, levv)
    else if (VCHECK_DETAIL(lv)) then
       if (utmp.ge.0) then
          write(utmp, 101) KTGT, 'denormalized'
       else if (utmp.eq.-1) then
          write(*,    101) KTGT, 'denormalized'
       endif
    endif
    return
  end subroutine check_real_props_f
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  subroutine check_real_props_q &
       & (kx, mold, u, levv)
    implicit none
    integer,parameter :: KTGT = KQPL
    integer,        intent(out)         :: kx
    real(kind=KTGT),intent(in)          :: mold
    integer,        intent(in),optional :: u
    integer,        intent(in),optional :: levv

    integer utmp, lv
    utmp = idef(u, ulog)
    lv = idef(levv, lev_verbose)

    call check_real_zero(kx, mold, u, levv)
    call check_real_one(kx, mold, u, levv)
101 format(__TAG__, 'real:', I0, ' skip to check ', A)
    if (force_check_inf) then
       call check_real_inf(kx, mold, u, levv)
    else if (VCHECK_DETAIL(lv)) then
       if (utmp.ge.0) then
          write(utmp, 101) KTGT, 'infinity'
       else if (utmp.eq.-1) then
          write(*,    101) KTGT, 'infinity'
       endif
    endif
    if (force_check_dnm) then
       call check_real_dnm(kx, mold, u, levv)
    else if (VCHECK_DETAIL(lv)) then
       if (utmp.ge.0) then
          write(utmp, 101) KTGT, 'denormalized'
       else if (utmp.eq.-1) then
          write(*,    101) KTGT, 'denormalized'
       endif
    endif
    return
  end subroutine check_real_props_q
#endif

!!!_  & check_real_mantissa() - number of (effective) mantissa bits
  !! return number of significant mantissa bits, i.e., ignoring
  !! the first implicit 1 bit.
  !! Should be identical with DIGITS(x) - 1.
  integer function check_real_mantissa_d (mold) result (m)
    integer,parameter :: KTGT = KDBL
    real(kind=KTGT),intent(in) :: mold    ! placeholder
    m = EXPONENT(1.0_KTGT) -  EXPONENT(EPSILON(mold))
    return
  end function check_real_mantissa_d
  integer function check_real_mantissa_f (mold) result (m)
    integer,parameter :: KTGT = KFLT
    real(kind=KTGT),intent(in) :: mold    ! placeholder
    m = EXPONENT(1.0_KTGT) -  EXPONENT(EPSILON(mold))
    return
  end function check_real_mantissa_f
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  integer function check_real_mantissa_q (mold) result (m)
    integer,parameter :: KTGT = KQPL
    real(kind=KTGT),intent(in) :: mold    ! placeholder
    m = EXPONENT(1.0_KTGT) -  EXPONENT(EPSILON(mold))
    return
  end function check_real_mantissa_q
#endif
!!!_  & check_real_zero - check real(0) properties, return exponent
  subroutine check_real_zero_d &
       & (kx, mold, u, levv)
    implicit none
    integer,parameter :: KTGT = KDBL
    integer,        intent(out)         :: kx
    real(kind=KTGT),intent(in)          :: mold
    integer,        intent(in),optional :: u
    integer,        intent(in),optional :: levv
    real(kind=KTGT) :: fr, t
    integer utmp, lv
    ! SHOULD RETURN 0 under fortran 95 standard (0 = 0 * 2^0)

    utmp = idef(u, ulog)
    lv = idef(levv, lev_verbose)

    t = real(0.0, kind=KIND(mold))
    fr = FRACTION(t)
    kx = EXPONENT(t)

    if (fr.ne.t) kx = - HUGE(kx)

101 format(__TAG__, 'real:', I0, ' zero = ', I0, ' (', F5.1, ')')
    if (VCHECK_DEBUG(lv) &
         & .or. (kx.ne.0 .and. VCHECK_NORMAL(lv))) then
       if (utmp.ge.0) then
          write(utmp, 101) KIND(mold), kx, fr
       else
          write(*,    101) KIND(mold), kx, fr
       endif
    endif

    return
  end subroutine check_real_zero_d
  subroutine check_real_zero_f &
       & (kx, mold, u, levv)
    implicit none
    integer,parameter :: KTGT = KFLT
    integer,        intent(out)         :: kx
    real(kind=KTGT),intent(in)          :: mold
    integer,        intent(in),optional :: u
    integer,        intent(in),optional :: levv
    real(kind=KTGT) :: fr, t
    integer utmp, lv
    ! SHOULD RETURN 0 under fortran 95 standard (0 = 0 * 2^0)

    utmp = idef(u, ulog)
    lv = idef(levv, lev_verbose)

    t = real(0.0, kind=KIND(mold))
    fr = FRACTION(t)
    kx = EXPONENT(t)

    if (fr.ne.t) kx = - HUGE(kx)

101 format(__TAG__, 'real:', I0, ' zero = ', I0, ' (', F5.1, ')')
    if (VCHECK_DEBUG(lv) &
         & .or. (kx.ne.0 .and. VCHECK_NORMAL(lv))) then
       if (utmp.ge.0) then
          write(utmp, 101) KIND(mold), kx, fr
       else
          write(*,    101) KIND(mold), kx, fr
       endif
    endif

    return
  end subroutine check_real_zero_f
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  subroutine check_real_zero_q &
       & (kx, mold, u, levv)
    implicit none
    integer,parameter :: KTGT = KQPL
    integer,        intent(out)         :: kx
    real(kind=KTGT),intent(in)          :: mold
    integer,        intent(in),optional :: u
    integer,        intent(in),optional :: levv
    real(kind=KTGT) :: fr, t
    integer utmp, lv
    ! SHOULD RETURN 0 under fortran 95 standard (0 = 0 * 2^0)

    utmp = idef(u, ulog)
    lv = idef(levv, lev_verbose)

    t = real(0.0, kind=KIND(mold))
    fr = FRACTION(t)
    kx = EXPONENT(t)

    if (fr.ne.t) kx = - HUGE(kx)

101 format(__TAG__, 'real:', I0, ' zero = ', I0, ' (', F5.1, ')')
    if (VCHECK_DEBUG(lv) &
         & .or. (kx.ne.0 .and. VCHECK_NORMAL(lv))) then
       if (utmp.ge.0) then
          write(utmp, 101) KIND(mold), kx, fr
       else
          write(*,    101) KIND(mold), kx, fr
       endif
    endif

    return
  end subroutine check_real_zero_q
#endif

!!!_  & check_real_one - check real(1) properties, return exponent
  subroutine check_real_one_d &
       & (kx, mold, u, levv)
    implicit none
    integer,parameter :: KTGT = KDBL
    integer,        intent(out)         :: kx
    real(kind=KTGT),intent(in)          :: mold
    integer,        intent(in),optional :: u
    integer,        intent(in),optional :: levv
    real(kind=KTGT) :: fr, t
    integer utmp, lv
    ! SHOULD RETURN 1 under fortran 95 standard (1 = 0.5 * 2^1)

    utmp = idef(u, ulog)
    lv = idef(levv, lev_verbose)

    t = real(1.0, kind=KIND(mold))
    fr = FRACTION(t)
    kx = EXPONENT(t)

    if (fr.eq.real(0.0, kind=KIND(mold))) kx = - HUGE(kx)

101 format(__TAG__, 'real:', I0, ' one = ', I0, ' (', F5.1, ')')
    if (VCHECK_DETAIL(lv) &
         & .or. (kx.ne.1 .and. VCHECK_NORMAL(lv))) then
       if (utmp.ge.0) then
          write(utmp, 101) KIND(mold), kx, fr
       else
          write(*,    101) KIND(mold), kx, fr
       endif
    endif

    return
  end subroutine check_real_one_d
  subroutine check_real_one_f &
       & (kx, mold, u, levv)
    implicit none
    integer,parameter :: KTGT = KFLT
    integer,        intent(out)         :: kx
    real(kind=KTGT),intent(in)          :: mold
    integer,        intent(in),optional :: u
    integer,        intent(in),optional :: levv
    real(kind=KTGT) :: fr, t
    integer utmp, lv
    ! SHOULD RETURN 1 under fortran 95 standard (1 = 0.5 * 2^1)

    utmp = idef(u, ulog)
    lv = idef(levv, lev_verbose)

    t = real(1.0, kind=KIND(mold))
    fr = FRACTION(t)
    kx = EXPONENT(t)

    if (fr.eq.real(0.0, kind=KIND(mold))) kx = - HUGE(kx)

101 format(__TAG__, 'real:', I0, ' one = ', I0, ' (', F5.1, ')')
    if (VCHECK_DETAIL(lv) &
         & .or. (kx.ne.1 .and. VCHECK_NORMAL(lv))) then
       if (utmp.ge.0) then
          write(utmp, 101) KIND(mold), kx, fr
       else
          write(*,    101) KIND(mold), kx, fr
       endif
    endif

    return
  end subroutine check_real_one_f
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  subroutine check_real_one_q &
       & (kx, mold, u, levv)
    implicit none
    integer,parameter :: KTGT = KQPL
    integer,        intent(out)         :: kx
    real(kind=KTGT),intent(in)          :: mold
    integer,        intent(in),optional :: u
    integer,        intent(in),optional :: levv
    real(kind=KTGT) :: fr, t
    integer utmp, lv
    ! SHOULD RETURN 1 under fortran 95 standard (1 = 0.5 * 2^1)

    utmp = idef(u, ulog)
    lv = idef(levv, lev_verbose)

    t = real(1.0, kind=KIND(mold))
    fr = FRACTION(t)
    kx = EXPONENT(t)

    if (fr.eq.real(0.0, kind=KIND(mold))) kx = - HUGE(kx)

101 format(__TAG__, 'real:', I0, ' one = ', I0, ' (', F5.1, ')')
    if (VCHECK_DETAIL(lv) &
         & .or. (kx.ne.1 .and. VCHECK_NORMAL(lv))) then
       if (utmp.ge.0) then
          write(utmp, 101) KIND(mold), kx, fr
       else
          write(*,    101) KIND(mold), kx, fr
       endif
    endif

    return
  end subroutine check_real_one_q
#endif

!!!_  & check_real_inf - check infinity properties
  subroutine check_real_inf_d &
       & (istt, mold, u, levv)
    implicit none
    integer,parameter :: KTGT = KDBL
    integer,        intent(out)         :: istt
    real(kind=KTGT),intent(in)          :: mold
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

    utmp = idef(u, ulog)
    lv = idef(levv, lev_verbose)

    th = HUGE(mold)
    km = MAXEXPONENT(mold)
    fh = FRACTION(th)
    kh = EXPONENT(th)

    if (km.ne.kh) istt = -1

    ti = th * REAL(RADIX(mold), kind=KIND(mold))
    kx = EXPONENT(ti)
    if (kx.le.kh) istt = -1

    ti = set_exponent(fh, kh+1)
    kx = EXPONENT(ti)
    if (kx.le.kh) istt = -1

101 format(__TAG__, 'real:', I0, ' inf = ', I0, ' (', I0, 1x, I0, 1x, I0, ')')
    if (VCHECK_DETAIL(lv) &
         & .or. (kx.ne.1 .and. VCHECK_NORMAL(lv))) then
       if (utmp.ge.0) then
          write(utmp, 101) KIND(mold), istt, kx, kh, km
       else
          write(*,    101) KIND(mold), istt, kx, kh, km
       endif
    endif

    return
  end subroutine check_real_inf_d
  subroutine check_real_inf_f &
       & (istt, mold, u, levv)
    implicit none
    integer,parameter :: KTGT = KFLT
    integer,        intent(out)         :: istt
    real(kind=KTGT),intent(in)          :: mold
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

    utmp = idef(u, ulog)
    lv = idef(levv, lev_verbose)

    th = HUGE(mold)
    km = MAXEXPONENT(mold)
    fh = FRACTION(th)
    kh = EXPONENT(th)

    if (km.ne.kh) istt = -1

    ti = th * REAL(RADIX(mold), kind=KIND(mold))
    kx = EXPONENT(ti)
    if (kx.le.kh) istt = -1

    ti = set_exponent(fh, kh+1)
    kx = EXPONENT(ti)
    if (kx.le.kh) istt = -1

101 format(__TAG__, 'real:', I0, ' inf = ', I0, ' (', I0, 1x, I0, 1x, I0, ')')
    if (VCHECK_DETAIL(lv) &
         & .or. (kx.ne.1 .and. VCHECK_NORMAL(lv))) then
       if (utmp.ge.0) then
          write(utmp, 101) KIND(mold), istt, kx, kh, km
       else
          write(*,    101) KIND(mold), istt, kx, kh, km
       endif
    endif

    return
  end subroutine check_real_inf_f
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  subroutine check_real_inf_q &
       & (istt, mold, u, levv)
    implicit none
    integer,parameter :: KTGT = KQPL
    integer,        intent(out)         :: istt
    real(kind=KTGT),intent(in)          :: mold
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

    utmp = idef(u, ulog)
    lv = idef(levv, lev_verbose)

    th = HUGE(mold)
    km = MAXEXPONENT(mold)
    fh = FRACTION(th)
    kh = EXPONENT(th)

    if (km.ne.kh) istt = -1

    ti = th * REAL(RADIX(mold), kind=KIND(mold))
    kx = EXPONENT(ti)
    if (kx.le.kh) istt = -1

    ti = set_exponent(fh, kh+1)
    kx = EXPONENT(ti)
    if (kx.le.kh) istt = -1

101 format(__TAG__, 'real:', I0, ' inf = ', I0, ' (', I0, 1x, I0, 1x, I0, ')')
    if (VCHECK_DETAIL(lv) &
         & .or. (kx.ne.1 .and. VCHECK_NORMAL(lv))) then
       if (utmp.ge.0) then
          write(utmp, 101) KIND(mold), istt, kx, kh, km
       else
          write(*,    101) KIND(mold), istt, kx, kh, km
       endif
    endif

    return
  end subroutine check_real_inf_q
#endif

!!!_  & check_real_dnm - check denormal properties
  subroutine check_real_dnm_d &
       & (kx, mold, u, levv)
    implicit none
    integer,parameter :: KTGT = KDBL
    integer,        intent(out)         :: kx
    real(kind=KTGT),intent(in)          :: mold
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
    utmp = idef(u, ulog)
    lv = idef(levv, lev_verbose)

    ti = +TINY(MOLD)
    z  = REAL(0, kind=KIND(mold))

    t = ti
    kr = 0
201 format(__TAG__, 'real:', I0, 1x, I0, 1x, I0)
    do j = 1, DIGITS(mold) + 1
       t = t / REAL(RADIX(mold), kind=KIND(mold))
       if (VCHECK_DEBUG(lv)) then
          if (utmp.ge.0) then
             write(utmp, 201) kind(mold), j, exponent(t)
          else
             write(*,    201) kind(mold), j, exponent(t)
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
       kr = EXPONENT(ti / REAL(RADIX(mold), kind=KIND(mold)))
       if (kr.ne.ki-1) kx = 1
       kr = EXPONENT(SET_EXPONENT(fi, ki-1))
       if (kr.ne.ki-1) kx = 2
    endif

101 format(__TAG__, 'real:', I0, ' dnm = ', I0, 1x, 2F4.1)
    if (VCHECK_DETAIL(lv) &
         & .or. (kx.ne.0 .and. VCHECK_NORMAL(lv))) then
       if (kx.le.0) then
          t  = SET_EXPONENT(TINY(mold), MINEXPONENT(mold) + kx)
          z  = SET_EXPONENT(TINY(mold), MINEXPONENT(mold) + kx - 1)
       else
          t  = real(1, kind=KIND(mold))
          z  = real(1, kind=KIND(mold))
       endif
       if (utmp.ge.0) then
          write(utmp, 101) KIND(mold), kx, FRACTION(t), FRACTION(z)
       else
          write(*,    101) KIND(mold), kx, FRACTION(t), FRACTION(z)
       endif
    endif

    return
  end subroutine check_real_dnm_d
  subroutine check_real_dnm_f &
       & (kx, mold, u, levv)
    implicit none
    integer,parameter :: KTGT = KFLT
    integer,        intent(out)         :: kx
    real(kind=KTGT),intent(in)          :: mold
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
    utmp = idef(u, ulog)
    lv = idef(levv, lev_verbose)

    ti = +TINY(MOLD)
    z  = REAL(0, kind=KIND(mold))

    t = ti
    kr = 0
201 format(__TAG__, 'real:', I0, 1x, I0, 1x, I0)
    do j = 1, DIGITS(mold) + 1
       t = t / REAL(RADIX(mold), kind=KIND(mold))
       if (VCHECK_DEBUG(lv)) then
          if (utmp.ge.0) then
             write(utmp, 201) kind(mold), j, exponent(t)
          else
             write(*,    201) kind(mold), j, exponent(t)
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
       kr = EXPONENT(ti / REAL(RADIX(mold), kind=KIND(mold)))
       if (kr.ne.ki-1) kx = 1
       kr = EXPONENT(SET_EXPONENT(fi, ki-1))
       if (kr.ne.ki-1) kx = 2
    endif

101 format(__TAG__, 'real:', I0, ' dnm = ', I0, 1x, 2F4.1)
    if (VCHECK_DETAIL(lv) &
         & .or. (kx.ne.0 .and. VCHECK_NORMAL(lv))) then
       if (kx.le.0) then
          t  = SET_EXPONENT(TINY(mold), MINEXPONENT(mold) + kx)
          z  = SET_EXPONENT(TINY(mold), MINEXPONENT(mold) + kx - 1)
       else
          t  = real(1, kind=KIND(mold))
          z  = real(1, kind=KIND(mold))
       endif
       if (utmp.ge.0) then
          write(utmp, 101) KIND(mold), kx, FRACTION(t), FRACTION(z)
       else
          write(*,    101) KIND(mold), kx, FRACTION(t), FRACTION(z)
       endif
    endif

    return
  end subroutine check_real_dnm_f
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  subroutine check_real_dnm_q &
       & (kx, mold, u, levv)
    implicit none
    integer,parameter :: KTGT = KQPL
    integer,        intent(out)         :: kx
    real(kind=KTGT),intent(in)          :: mold
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
    utmp = idef(u, ulog)
    lv = idef(levv, lev_verbose)

    ti = +TINY(MOLD)
    z  = REAL(0, kind=KIND(mold))

    t = ti
    kr = 0
201 format(__TAG__, 'real:', I0, 1x, I0, 1x, I0)
    do j = 1, DIGITS(mold) + 1
       t = t / REAL(RADIX(mold), kind=KIND(mold))
       if (VCHECK_DEBUG(lv)) then
          if (utmp.ge.0) then
             write(utmp, 201) kind(mold), j, exponent(t)
          else
             write(*,    201) kind(mold), j, exponent(t)
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
       kr = EXPONENT(ti / REAL(RADIX(mold), kind=KIND(mold)))
       if (kr.ne.ki-1) kx = 1
       kr = EXPONENT(SET_EXPONENT(fi, ki-1))
       if (kr.ne.ki-1) kx = 2
    endif

101 format(__TAG__, 'real:', I0, ' dnm = ', I0, 1x, 2F4.1)
    if (VCHECK_DETAIL(lv) &
         & .or. (kx.ne.0 .and. VCHECK_NORMAL(lv))) then
       if (kx.le.0) then
          t  = SET_EXPONENT(TINY(mold), MINEXPONENT(mold) + kx)
          z  = SET_EXPONENT(TINY(mold), MINEXPONENT(mold) + kx - 1)
       else
          t  = real(1, kind=KIND(mold))
          z  = real(1, kind=KIND(mold))
       endif
       if (utmp.ge.0) then
          write(utmp, 101) KIND(mold), kx, FRACTION(t), FRACTION(z)
       else
          write(*,    101) KIND(mold), kx, FRACTION(t), FRACTION(z)
       endif
    endif

    return
  end subroutine check_real_dnm_q
#endif

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
!!!_ + end of TOUZA_Std_prc
end module TOUZA_Std_prc

!!!_@ test_std_prc - test program
#ifdef TEST_STD_PRC
program test_std_prc
  use TOUZA_Std_prc
  implicit none
  integer ierr
  integer istt
  real(kind=KDBL) :: VDBL = 0.0
  real(kind=KFLT) :: VFLT = 0.0
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  real(kind=KQPL) :: VQPL = 0.0
#endif

  call init(ierr)
  if (ierr.eq.0) call diag(ierr, levv=-1)
  if (ierr.eq.0) call diag(ierr, levv=10, mode=MODE_DEEP + MODE_FORCE)
  if (ierr.eq.0) call finalize(ierr, levv=10)

  if (ierr.eq.0) then
     call diag_real_props(istt, real(0, kind=KFLT))
     call diag_real_props(istt, real(0, kind=KDBL))
#if OPT_REAL_QUADRUPLE_DIGITS > 0
     call diag_real_props(istt, real(0, kind=KQPL))
#endif
  endif

  if (ierr.eq.0) then
     call check_real_props(istt, VDBL, levv=10)
     call check_real_props(istt, VFLT, levv=10)
#if OPT_REAL_QUADRUPLE_DIGITS > 0
     call check_real_props(istt, VQPL, levv=10)
#endif
  endif

  if (ierr.eq.0) call init_set_switches(ierr, .TRUE., .TRUE.)
  if (ierr.eq.0) then
     call check_real_props(istt, VDBL, levv=10)
     call check_real_props(istt, VFLT, levv=10)
#if OPT_REAL_QUADRUPLE_DIGITS > 0
     call check_real_props(istt, VQPL, levv=10)
#endif
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

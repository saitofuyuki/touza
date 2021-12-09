!!!_! trapiche_float.F90 - TOUZA/Trapiche(trapiche) floating-point (dis)assembler
! Maintainer: SAITO Fuyuki
! Created: Mar 1 2021
#define TIME_STAMP 'Time-stamp: <2021/12/07 07:23:45 fuyuki trapiche_float.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_trp.h"
!!!_* macros
!!!_ + system dependent workarounds
#ifndef    OPT_PROHIBIT_AUTO_WORKAROUND
#  define  OPT_PROHIBIT_AUTO_WORKAROUND 0
#endif
#ifndef    OPT_TRAPICHE_PROHIBIT_WORKAROUND
#  define  OPT_TRAPICHE_PROHIBIT_WORKAROUND OPT_PROHIBIT_AUTO_WORKAROUND
#endif
#if OPT_TRAPICHE_PROHIBIT_WORKAROUND
#else /* not OPT_TRAPICHE_PROHIBIT_WORKAROUND */
#  if __NEC__
#     ifndef OPT_TRAPICHE_SX_SPECIALS
#     define OPT_TRAPICHE_SX_SPECIALS 1
#     endif
#  endif
#endif /* not OPT_TRAPICHE_PROHIBIT_WORKAROUND */
#ifndef   OPT_TRAPICHE_SX_SPECIALS
#  define OPT_TRAPICHE_SX_SPECIALS 0
#endif
!!!_  - where or do-if
#ifndef    OPT_PREFER_WHERE
#  define  OPT_PREFER_WHERE 0    /* WHERE statement instead of DO+IF */
#endif
#define  _PREFER_WHERE OPT_PREFER_WHERE.ne.0
#define  _PREFER_DOIF  OPT_PREFER_WHERE.eq.0
!!!_ + ieee
#ifndef   HAVE_IEEE_ARITMETIC
#  define HAVE_IEEE_ARITMETIC 0
#endif
#define OPT_TRAPICHE_IEEE 0
#ifndef   OPT_TRAPICHE_IEEE
#  define OPT_TRAPICHE_IEEE HAVE_IEEE_ARITMETIC
#endif
!!!_@ TOUZA_Trp_float - trapiche floating-point manager
module TOUZA_Trp_float
!!!_ = declaration
  use TOUZA_Trp_std,only: KI32, KDBL, KFLT, &
       & control_mode, control_deep, is_first_force, &
       & unit_global,  trace_fine,   trace_control
  implicit none
  private
!!!_  - public parameters (switches)
!!!_   . exponent range
  integer,parameter,public :: XnoTop = + HUGE(0)    ! no upper limit for exponent storing
  integer,parameter,public :: XnoBtm = - HUGE(0) -1 ! no lower limit for exponent storing
!!!_   . functions
  integer,parameter,public :: KCODE_DEFAULT     = 0
  integer,parameter,public :: KCODE_TRANSPOSE   = 1   ! (T) enable transpose filling
  integer,parameter,public :: KCODE_SEQUENTIAL  = 2   ! (S) enable sequential filling
  integer,parameter,public :: KCODE_INCREMENTAL = 4   ! (I) enable incremental variation on sequential filling
  integer,parameter,public :: KCODE_MANUAL      = 8   ! (M) enable manual variation on filling
  integer,parameter,public :: KCODE_CLIPPING    = 16  ! (C) enable trimming(clipping)
  integer,parameter,public :: KCODE_SIGN_ZERO   = 32  ! (Z) enable sign preservation on zero
  integer,parameter,public :: KCODE_ROUND       = 64  ! (R) enable round-up variation

!!!_  - parameters
  integer,parameter,public :: TRAPICHE_ID = 1416785920 ! Trp[00]
!!!_   . special values
  integer,parameter,public :: KX_ZERO  = 0   !  0    00    000    zero
  integer,parameter,public :: KX_MISS  = 1   !  1    01    001    missing
  integer,parameter,public :: KX_UNFL  = 2   ! (0)   10    010    underflow
  integer,parameter,public :: KX_OVFL  = 3   ! (1)   11    011    overflow
  integer,parameter,public :: KX_DNM   = 4   ! (0)  (00)   100    denormalized
  integer,parameter,public :: KX_NAN   = 5   ! (1)  (01)   101    nan
  !                                          !                    reserved
  integer,parameter,public :: KX_INF   = 7   ! (1)  (11)   111    infinity

  integer,parameter :: MAX_SPECIAL   = 8

!!!_   . storage properties
  integer,parameter :: KB_ID   = 0   ! id
  integer,parameter :: KB_NCNZ = 1   ! total items (source)
  integer,parameter :: KB_NBGZ = 2   ! total items (packed)
  integer,parameter :: KB_XBIT = 3   ! exponent bit info         (packing|target|(stored))
  integer,parameter :: KB_HBIT = 4   ! mantissa(higher) bit info (packing|target|stored|sign)
  integer,parameter :: KB_LBIT = 5   ! mantissa(lower) bit info  (packing|target|stored)
  integer,parameter :: KB_XBGN = 6   ! exponent corresponding to index 0 (rel. to exponent(1))
  integer,parameter :: KB_XDNM = 6   ! obsolete
  integer,parameter :: KB_XLBD = 7   ! exponent lower bound to clip
  integer,parameter :: KB_XUBD = 8   ! exponent upper bound to clip
  integer,parameter :: KB_MSKH = 9   ! mantissa (higher) bit mask
  integer,parameter :: KB_OFSH = 10  ! mantissa (higher) bit offset
  integer,parameter :: KB_MSKL = 11  ! mantissa (lower) bit mask
  integer,parameter :: KB_OFSL = 12  ! mantissa (lower) bit offset
  integer,parameter :: KB_XTR0 = 13  ! extra properties
  integer,parameter :: KB_XTR1 = 14
  integer,parameter :: KB_XTR2 = 15
  integer,parameter :: KB_XTRZ = KB_XTR2

  integer,parameter,public :: KB_HEAD = 1 + KB_XTRZ  ! storage start position

  !!    |-------base------|
  !!    000.....XXXXXXXXXX| target
  !!            |--stored-|
  !!       mmmmmmmmmmmmmmm| offset
  !!       000yy0000000000| mask
!!!_   . sign flag (reserved)
  integer,parameter :: KF_SIGN_INFLATE = 64
  integer,parameter :: KF_ALL_POSITIVE = +1
  integer,parameter :: KF_ALL_NEGATIVE = -1
  integer,parameter :: KF_MIXED        = 0
  integer,parameter :: KF_POS_MZERO    = +2  ! positive except for -0
  integer,parameter :: KF_NEG_PZERO    = -2  ! negative except for +0
  integer,parameter :: KF_POS_ZERO     = +3  ! positive or zero
  integer,parameter :: KF_NEG_ZERO     = -3  ! negative or zero

!!!_   . catalogo flags
  integer,parameter :: KC_ZERO  = 1  ! contains 0
  integer,parameter :: KC_LARGE = 2  ! contains undef,nan,inifinity
  integer,parameter :: KC_SMALL = 4  ! contains denormalized
!!!_   . restore schemes
  integer,parameter,public :: KS_ZERO    = 0
  integer,parameter,public :: KS_NAN     = -1
  integer,parameter,public :: KS_INF     = -2
  integer,parameter,public :: KS_DNM     = -3
  integer,parameter,public :: KS_MISS    = +1
  integer,parameter,public :: KS_TINY    = +2    ! TINY()
  integer,parameter,public :: KS_HUGE    = +3    ! HUGE()
  integer,parameter,public :: KS_FLOOR   = +4    ! Lower limit
  integer,parameter,public :: KS_CEILING = +5    ! Upper limit
  integer,parameter,public :: KS_UNDER   = +6    ! Under floor
  integer,parameter,public :: KS_ROOF    = +7    ! Over ceiling

!!!_  - work-area shared
  integer(KIND=KI32),allocatable,save :: wibuf(:)
!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = TRP_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global

  integer,save :: min_bits_store = 0

  logical,save :: force_health_check = .FALSE.
!!!_   . schemes for special
  integer,save :: KSCHM_DNM  = KS_ZERO
  integer,save :: KSCHM_NAN  = KS_MISS
  integer,save :: KSCHM_INF  = KS_MISS
  integer,save :: KSCHM_OVFL = KS_ROOF
  integer,save :: KSCHM_UNFL = KS_UNDER

!!!_   . sign flag alias
  integer,save :: ksign_table(0:15) = &                                        !  -+np 0123
       & (/ KF_ALL_POSITIVE, KF_ALL_NEGATIVE, KF_ALL_POSITIVE, KF_POS_ZERO, &  !  FFFF TFFF FTFF TTFF
       &    KF_ALL_NEGATIVE, KF_ALL_NEGATIVE, KF_NEG_PZERO,    KF_NEG_ZERO, &  !  FFTF TFTF FTTF TTTF
       &    KF_ALL_POSITIVE, KF_POS_MZERO,    KF_ALL_POSITIVE, KF_POS_ZERO, &  !  FFFT TFFT FTFT TTFT
       &    KF_MIXED,        KF_MIXED,        KF_MIXED,        KF_MIXED /)     !  FFTT TFTT FTTT TTTT
!!!_   . preset options in show_bagazo_props
  integer,          save :: idx_sbp  = 0    ! index
  character(len=64),save :: atag_sbp = ' '  ! tag
  integer,          save :: itag_sbp = -1   ! index

# define __MDL__ 'f'
!!!_  - common
  character(len=256) :: tmsg
!!!_  - interfaces
! #if HAVE_IEEE_ARITHMETIC
!   interface catalogar_ieee
!      module procedure catalogar_ieee_d
!   end interface catalogar_ieee
! #endif /* HAVE_IEEE_ARITHMETIC */

  interface helper_props
     module procedure helper_props_d, helper_props_f
  end interface helper_props

  interface encode_alloc
     module procedure encode_alloc_d, encode_alloc_f
  end interface encode_alloc
  interface encode_stack
     module procedure encode_stack_d, encode_stack_f
  end interface encode_stack
  interface encode_trig
     module procedure encode_trig_di, encode_trig_fi
  end interface encode_trig

  interface decode_alloc
     module procedure decode_alloc_d, decode_alloc_f
  end interface decode_alloc
  interface decode_stack
     module procedure decode_stack_d, decode_stack_f
  end interface decode_stack
  interface decode_work
     module procedure decode_work_d, decode_work_f
  end interface decode_work

  interface health_check
     module procedure health_check_d, health_check_f
  end interface health_check

  interface guardar_extra
     module procedure guardar_extra_is, guardar_extra_ia
  end interface guardar_extra

  interface ajustar_full
     module procedure ajustar_full_i
  end interface ajustar_full
  interface ajustar_high
     module procedure ajustar_high_i
  end interface ajustar_high

  interface diluir
     module procedure diluir_di, diluir_fi
  end interface diluir

  interface recortar
     module procedure recortar_i
  end interface recortar

  interface show_bagazo_patterns
     module procedure show_bagazo_patterns_di
  end interface show_bagazo_patterns

  interface show_work_patterns
     module procedure show_work_patterns_di
  end interface show_work_patterns
  interface batch_specials
     module procedure batch_specials_d, batch_specials_f
  end interface batch_specials

  interface set_special
     module procedure set_special_d, set_special_f
  end interface set_special
  interface xsign
     module procedure xsign_d, xsign_f
  end interface xsign
  interface which_sp
     module procedure which_sp_d, which_sp_f
  end interface which_sp
  interface xspecial
     module procedure xspecial_d, xspecial_f
  end interface xspecial
  interface zspecial
     module procedure zspecial_d, zspecial_f
  end interface zspecial

  interface xuint
     module procedure xuint_d, xuint_f
  end interface xuint
  interface xureal
     module procedure xureal_d, xureal_f
  end interface xureal

  interface show_pattern_float
     module procedure show_pattern_float_d, show_pattern_float_f
  end interface show_pattern_float
  interface binstr_float
     module procedure binstr_float_d, binstr_float_f
  end interface binstr_float
  interface compare_report
     module procedure compare_report_d
  end interface compare_report
  interface compare_element
     module procedure compare_element_d
  end interface compare_element

#if OPT_TRAPICHE_SX_SPECIALS
  interface XSETSX
     module procedure XSETSX_d, XSETSX_f
end interface XSETSX
#endif
!!!_  - public procedures
  public init, diag, finalize
  ! public trapiche_disassemble_exp
  public helper_props
  public encode_alloc, encode_stack, encode_trig
  public decode_alloc, decode_stack, decode_work
  public health_check
  public asignar
  public guardar_extra
  public diluir
  public retrieve_ncnz, retrieve_nbgz, retrieve_extra
  public suggest_filling
  public push_show_tags
  public show_bagazo_props, show_bagazo_patterns, show_pattern_float, binstr_float
  public compare_report,    compare_element
  public parse_codes, unparse_codes
!!!_   . TOUZA_Trp_std
  public KI32, KDBL, KFLT
contains
!!!_ + intrincic replacement by macro
#if OPT_TRAPICHE_SX_SPECIALS
#  define _EXPONENT(A)  IRE(A)
#  define _SET_EXPONENT XSETSX
#else
#  define _EXPONENT(A)  EXPONENT(A)
#  define _SET_EXPONENT SET_EXPONENT
#endif
!!!_ + common interfaces
!!!_  & init
  subroutine init &
       & (ierr, &
       &  u,     levv,  mode,  stdv, &
       &  minbs, mwork, hch)
    use TOUZA_Trp_std, only: choice
    use TOUZA_Trp_pack,only: tp_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u, levv, mode, stdv
    integer,intent(in),optional :: minbs
    integer,intent(in),optional :: mwork  ! initial work-area size to allocate
    logical,intent(in),optional :: hch    ! force health check (may cause fpe)

    integer lv, md, lmd

    ierr = 0

    md = control_mode(mode, MODE_DEEPEST)
    init_mode = md

    if (md.ge.MODE_SURFACE) then
       err_default = ERR_SUCCESS
       lv = choice(lev_verbose, levv)
       if (is_first_force(init_counts, md)) then
          ulog = choice(ulog, u)
          lev_verbose = lv
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call tp_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv)
       endif
       if (is_first_force(init_counts, md)) then
          if (ierr.eq.0) call init_batch(ierr, minbs, mwork, ulog, lv)
       endif
       if (is_first_force(init_counts, md)) then
          if (present(hch)) then
             force_health_check = hch
          endif
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif

    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Trp_std, only: choice, msg, is_msglev_normal
    use TOUZA_Trp_pack,only: tp_diag=>diag
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
       call trace_control &
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, md)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) then
                call msg(TIME_STAMP, __MDL__, utmp)
                call msg('(''where instead of do-if '', I0)', OPT_PREFER_WHERE, __MDL__, utmp)
             endif
             if (is_msglev_normal(lv)) then
                call msg('(''minimum bit storage = '', I0)', min_bits_store, __MDL__, utmp)
                call diag_system(ierr, utmp)
                call diag_special(ierr, KSCHM_DNM,  'D', utmp)
                call diag_special(ierr, KSCHM_NAN,  'N', utmp)
                call diag_special(ierr, KSCHM_INF,  'I', utmp)
                call diag_special(ierr, KSCHM_OVFL, 'O', utmp)
                call diag_special(ierr, KSCHM_UNFL, 'U', utmp)
             endif
             if (is_msglev_normal(lv)) then
                call msg('(''work:w = '', I0)', size(wibuf), __MDL__, utmp)
             endif
          endif
          if (force_health_check) then
             if (ierr.eq.0) call health_check(ierr, 0.0_KDBL, utmp, levv=lv)
             if (ierr.eq.0) call health_check(ierr, 0.0_KFLT, utmp, levv=lv)
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call tp_diag(ierr, utmp, lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Trp_std,only: choice
    use TOUZA_Trp_pack,only: tp_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, md)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       if (ierr.eq.0) then
          if (allocated(wibuf)) deallocate(wibuf, STAT=ierr)
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call tp_finalize (ierr, utmp, levv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + init subcontracts
!!!_  & init_batch
  subroutine init_batch &
       & (ierr, &
       &  minbs, mwork, &
       &  u,      levv)
    use TOUZA_Trp_std, only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: minbs
    integer,intent(in),optional :: mwork ! initial work-area size to allocate
    integer,intent(in),optional :: u, levv

    integer mw

    ierr = 0

    if (present(minbs)) then
       min_bits_store = minbs
    endif
    mw = max(1, choice(0, mwork)) * 3
    if (ierr.eq.0) allocate(wibuf(0:mw-1), STAT=ierr)

    return
  end subroutine init_batch

!!!_ + diag subcontracts
!!!_  & diag_special
  subroutine diag_special(ierr, ks, tag, u)
    use TOUZA_Trp_std,only: msg
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: ks
    character(len=*),intent(in)          :: tag
    integer,         intent(in),optional :: u

    character(len=128) :: t

    ierr = 0
101 format('scheme/', A, ': ', I0)
    write(t, 101) trim(tag), ks
    call msg(t, __MDL__, u)
    return
  end subroutine diag_special

!!!_  & diag_system
  subroutine diag_system(ierr, u)
    use TOUZA_Trp_std,only: choice, msg, KRTGT=>KDBL
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u

#   if OPT_TRAPICHE_SX_SPECIALS
    integer utmp
    integer j, jx
    real(KIND=KRTGT),parameter :: one = 1.0_KRTGT
    integer,parameter :: ML = MINEXPONENT(one)
    integer,parameter :: MH = MAXEXPONENT(one)
    real(KIND=KRTGT) :: vx, vy, vr
#   endif

    ierr = 0 * choice(0, u)
#   if OPT_TRAPICHE_SX_SPECIALS
    call msg('(''sx special procedures enabled '', I0)', &
         & OPT_TRAPICHE_SX_SPECIALS, __MDL__, u)
    if (HAVE_AMT.eq.0) then
       ierr = -1
       call msg('not found amt()', __MDL__, u)
    endif
    if (HAVE_IRE.eq.0) then
       ierr = -1
       call msg('not found ire()', __MDL__, u)
    endif
    if (HAVE_EXP2.eq.0) then
       ierr = -1
       call msg('not found exp2()', __MDL__, u)
    endif
    utmp = choice(-1, u)
101 format('exp2:', I0, ': ', L1, 1x, E24.16, 1x, E24.16)
    do j = +2, -2, -1
       jx = ML + j
       vx = AMT(one) * EXP2(REAL(jx, KIND=KRTGT))
       vr = SET_EXPONENT(one, jx)
       if (utmp.ge.0) then
          write(utmp, 101) jx, vx.eq.vr, vx, vr
       else if (utmp.eq.-1) then
          write(*,    101) jx, vx.eq.vr, vx, vr
       endif
    enddo
102 format('exp2:', A, ':', I0, ': ', L1, 1x, E24.16, 1x, E24.16)
    do j = -2, +2
       jx = MH + j
       vx = AMT(one) * EXP2(REAL(jx, KIND=KRTGT))
       vy = AMT(one) * EXP2(REAL(1, KIND=KRTGT)) * EXP2(REAL(jx-1, KIND=KRTGT))
       vr = SET_EXPONENT(one, jx)
       if (utmp.ge.0) then
          write(utmp, 102) 'R', jx, vx.eq.vr, vx, vr
          write(utmp, 102) 'A', jx, vy.eq.vr, vy, vr
       else if (utmp.eq.-1) then
          write(*, 102) 'R', jx, vx.eq.vr, vx, vr
          write(*, 102) 'A', jx, vy.eq.vr, vy, vr
       endif
    enddo
#   endif /* OPT_TRAPICHE_SX_SPECIALS */
  end subroutine diag_system

!!!_ + user subroutines (batch)
!!!_  & helper_props - simple helper to suggest trapiche properties
  subroutine helper_props_d &
       & (mbits,  xbits,  xbtm, &
       &  refmax, refmin, res)
    use TOUZA_Trp_std,only: choice, first_bit
    implicit none
    integer,parameter :: KRFLD=KDBL
    integer,         intent(out)         :: mbits,  xbits, xbtm
    real(kind=KRFLD),intent(in)          :: refmax, refmin
    real(kind=KRFLD),intent(in),optional :: res
    integer,parameter :: ixone = exponent(1.0_KRFLD)

    integer ixh, ixl, ixr

    ixh = exponent(refmax) - ixone
    ixl = exponent(refmin) - ixone
    ! write(*,*) 'helper input', ixh, ixl, ixone
    xbits = first_bit(ixh - ixl + 1) + 1
    if (present(res)) then
       if (res.gt.0.0) then
          ixr = exponent(res) - ixone
       else
          ixr = ixl
       endif
    else
       ixr = ixl
    endif
    mbits = ixh - ixr + 1
    xbtm = ixl
    return
  end subroutine helper_props_d
  subroutine helper_props_f &
       & (mbits,  xbits,  xbtm, &
       &  refmax, refmin, res)
    use TOUZA_Trp_std,only: choice, first_bit
    implicit none
    integer,parameter :: KRFLD=KFLT
    integer,         intent(out)         :: mbits,  xbits, xbtm
    real(kind=KRFLD),intent(in)          :: refmax, refmin
    real(kind=KRFLD),intent(in),optional :: res
    integer,parameter :: ixone = exponent(1.0_KRFLD)

    integer ixh, ixl, ixr

    ixh = exponent(refmax) - ixone
    ixl = exponent(refmin) - ixone

    xbits = first_bit(ixh - ixl + 1) + 1
    if (present(res)) then
       if (res.gt.0.0) then
          ixr = exponent(res) - ixone
       else
          ixr = ixl
       endif
    else
       ixr = ixl
    endif
    mbits = ixh - ixr + 1
    xbtm = ixl
    return
  end subroutine helper_props_f
!!!_  & encode_alloc - trapiche encoder (internal allocation)
  subroutine encode_alloc_d &
       & (ierr,   &
       &  ibagaz, &
       &  vsrc,   mem,   vmiss, &
       &  mbits,  xbits, ixtop, ixbtm, kcode)
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KDBL
    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)  ! integer representaion of float source
    real   (kind=KRFLD),intent(in)  :: vsrc(0:*)
    real   (kind=KRFLD),intent(in)  :: vmiss
    integer,            intent(in)  :: mbits, xbits ! mantissa/exponent bits
    integer,            intent(in)  :: ixtop, ixbtm  ! exponent upper/lower limit (relative to exp(1))
    integer,            intent(in)  :: mem
    integer,            intent(in)  :: kcode        ! switches

    ierr = err_default
    if (ierr.eq.0) call alloc_works(ierr, mem * 3)
    if (ierr.eq.0) then
       call encode_trig_di &
            & (ierr,  ibagaz, wibuf, &
            &  vsrc,  mem,    vmiss, &
            &  mbits, xbits,  ixtop, ixbtm, kcode)
    endif
    return
  end subroutine encode_alloc_d
  subroutine encode_alloc_f &
       & (ierr,   &
       &  ibagaz, &
       &  vsrc,   mem,   vmiss, &
       &  mbits,  xbits, ixtop, ixbtm, kcode)
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KFLT
    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)  ! integer representaion of float source
    real   (kind=KRFLD),intent(in)  :: vsrc(0:*)
    real   (kind=KRFLD),intent(in)  :: vmiss
    integer,            intent(in)  :: mbits, xbits ! mantissa/exponent bits
    integer,            intent(in)  :: ixtop, ixbtm  ! exponent upper/lower limit (relative to exp(1))
    integer,            intent(in)  :: mem
    integer,            intent(in)  :: kcode        ! switches

    ierr = err_default
    if (ierr.eq.0) call alloc_works(ierr, mem * 3)
    if (ierr.eq.0) then
       call encode_trig_fi &
            & (ierr,  ibagaz, wibuf, &
            &  vsrc,  mem,    vmiss, &
            &  mbits, xbits,  ixtop, ixbtm, kcode)
    endif
    return
  end subroutine encode_alloc_f
!!!_  & encode_stack - trapiche encoder (stack)
  subroutine encode_stack_d &
       & (ierr,   &
       &  ibagaz, &
       &  vsrc,   mem,   vmiss, &
       &  mbits,  xbits, ixtop, ixbtm, kcode)
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KDBL
    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)
    real   (kind=KRFLD),intent(in)  :: vsrc(0:*)
    real   (kind=KRFLD),intent(in)  :: vmiss
    integer,            intent(in)  :: mbits, xbits ! mantissa/exponent bits
    integer,            intent(in)  :: ixtop, ixbtm  ! exponent upper/lower limit (relative to exp(1))
    integer,            intent(in)  :: mem
    integer,            intent(in)  :: kcode        ! switches

    integer iwork(0:mem*3)

    ierr = err_default
    if (ierr.eq.0) then
       call encode_trig_di &
            & (ierr,  ibagaz, iwork, &
            &  vsrc,  mem,    vmiss, &
            &  mbits, xbits,  ixtop, ixbtm, kcode)
    endif
    return
  end subroutine encode_stack_d
  subroutine encode_stack_f &
       & (ierr,   &
       &  ibagaz, &
       &  vsrc,   mem,   vmiss, &
       &  mbits,  xbits, ixtop, ixbtm, kcode)
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KFLT
    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)
    real   (kind=KRFLD),intent(in)  :: vsrc(0:*)
    real   (kind=KRFLD),intent(in)  :: vmiss
    integer,            intent(in)  :: mbits, xbits ! mantissa/exponent bits
    integer,            intent(in)  :: ixtop, ixbtm  ! exponent upper/lower limit (relative to exp(1))
    integer,            intent(in)  :: mem
    integer,            intent(in)  :: kcode        ! switches

    integer iwork(0:mem*3)

    ierr = err_default
    if (ierr.eq.0) then
       call encode_trig_fi &
            & (ierr,  ibagaz, iwork, &
            &  vsrc,  mem,    vmiss, &
            &  mbits, xbits,  ixtop, ixbtm, kcode)
    endif
    return
  end subroutine encode_stack_f
!!!_  & encode_trig - encoder core
  subroutine encode_trig_di &
       & (ierr,  ibagaz, iwork, &
       &  vsrc,  mem,    vmiss, &
       &  mbits, xbits,  ixtop, ixbtm, kcode)
    use TOUZA_Trp_std, only: first_bit
    use TOUZA_Trp_pack,only: count_packed, pack_store
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KDBL

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)
    integer(kind=KIBGZ),intent(out) :: iwork(0:*)   ! work array
    real   (kind=KRFLD),intent(in)  :: vsrc(0:*)
    integer,            intent(in)  :: mem
    real   (kind=KRFLD),intent(in)  :: vmiss
    integer,            intent(in)  :: mbits, xbits
    integer,            intent(in)  :: ixtop, ixbtm  ! exponent upper/lower limit (relative to exp(1))
    integer,            intent(in)  :: kcode        ! encoding scheme

    integer(kind=KIBGZ),parameter :: kzero = 0_KIBGZ

    real(kind=KRFLD),   parameter :: vone  = 1.0_KRFLD
    real(kind=KRFLD),   parameter :: vzero = 0.0_KRFLD

    integer,parameter :: kxone = EXPONENT(vone)
    integer,parameter :: kxmin = MINEXPONENT(vone)
    integer,parameter :: kxmax = MAXEXPONENT(vone)
    integer,parameter :: kxspc = kxmax + 1

    integer,parameter :: lfrd = digits(vone) - 1
    integer,parameter :: lbgz = bit_size(kzero)

    integer jwx, jwh, jwl, jwe       ! work array offset position
    integer nbitsh, nbitsl
    integer ebitsh, ebitsl, nbitsa
    integer jbgz,   jbend,  nbgz

    integer :: kxbgn, kxdnm, kxlbd, kxubd, kx0sp  ! native exponent
    integer xbi
    logical bzero, bmiss, binf, bdnm
    integer nbspc
    integer ksignp_org, ksignb
    integer(kind=KIBGZ) :: moffsh, mmskh
    integer(kind=KIBGZ) :: moffsl, mmskl
    integer kpackx, kpackh, kpackl
    integer(kind=KIBGZ) :: mhpmax, mhpmin, mhnmax, mhnmin
    integer(kind=KIBGZ) :: mlpmax, mlpmin, mlnmax, mlnmin

    ierr = 0

    xbi = xbits
    jwx = 0
    jwh = jwx + mem
    jwl = jwh + mem
    jwe = jwl + mem

    mlpmax = 0
    mlpmin = 0
    mlnmax = 0
    mlnmin = 0
    mhpmax = 0
    mhpmin = 0
    mhnmax = 0
    mhnmin = 0

    if (IAND(kcode, KCODE_ROUND).gt.0) then
       if (mbits.ge.lbgz) then
          call desmontar_full_round_di &
               & (ierr,   &
               &  iwork(jwx:jwh-1), iwork(jwh:jwl-1), iwork(jwl:jwe-1), &
               &  kxdnm,      kxlbd,  kxubd,  bzero, bmiss, binf, bdnm, &
               &  ksignp_org, nbitsh, nbitsl, &
               &  vsrc,       mem,    vmiss,  mbits, kxspc)
       else
          call desmontar_high_round_di &
               & (ierr,   &
               &  iwork(jwx:jwh-1), iwork(jwh:jwl-1), &
               &  kxdnm,      kxlbd,  kxubd,  bzero, bmiss, binf, bdnm, &
               &  ksignp_org, nbitsh, nbitsl, &
               &  vsrc,       mem,    vmiss,  mbits, kxspc)
       endif
    else
       if (mbits.ge.lbgz) then
          call desmontar_full_di &
               & (ierr,   &
               &  iwork(jwx:jwh-1), iwork(jwh:jwl-1), iwork(jwl:jwe-1), &
               &  kxdnm,      kxlbd,  kxubd,  bzero, bmiss, binf, bdnm, &
               &  ksignp_org, nbitsh, nbitsl, &
               &  vsrc,       mem,    vmiss,  mbits, kxspc)
       else
          call desmontar_high_di &
               & (ierr,   &
               &  iwork(jwx:jwh-1), iwork(jwh:jwl-1), &
               &  kxdnm,      kxlbd,  kxubd,  bzero, bmiss, binf, bdnm, &
               &  ksignp_org, nbitsh, nbitsl, &
               &  vsrc,       mem,    vmiss,  mbits, kxspc)
       endif
    endif

    ! if (.TRUE.) then
    !    nbitsa = count_msbits(nbitsh, ksignp)
    !    call show_work_patterns(ierr, iwork, vsrc, mem, nbitsa=nbitsa, nbitsl=nbitsl)
    ! endif

    call abarcar &
         & (kxbgn, kxdnm, kxlbd, kxubd, kx0sp, xbi,   nbspc, &
         &  mbits, bzero, bmiss, binf,  bdnm,  &
         &  ixtop, ixbtm, kxmin, kxmax, kxone, lfrd)

    ksignb = ksignp_org
    if (IAND(kcode, KCODE_SIGN_ZERO).gt.0) then
       if (ksignb.eq.KF_NEG_ZERO .or. ksignb.eq.KF_POS_ZERO) ksignb = KF_MIXED
    endif

    if (mbits.ge.lbgz) then
       if (ksignb.ne.0) then
          call reajustar_full_i &
               & (ierr,   ibagaz, &
               &  iwork(jwx:jwh-1), iwork(jwh:jwl-1), iwork(jwl:jwe-1), &
               &  mem,    &
               &  nbitsh, nbitsl, xbi,    &
               &  kxbgn,  kxdnm,  kxlbd,  kxubd,  kx0sp, kxspc, &
               &  kcode)
          nbitsh = nbitsh + 1
          nbitsl = nbitsl - 1
       else
          call ajustar_full_i &
               & (ierr,   ibagaz, &
               &  iwork(jwx:jwh-1), iwork(jwh:jwl-1), iwork(jwl:jwe-1), &
               &  mem,    &
               &  ksignb, nbitsh, nbitsl, xbi,   &
               &  kxbgn,  kxdnm,  kxlbd,  kxubd,  kx0sp, kxspc, &
               &  kcode)
       endif
    else
       call ajustar_high_i &
            & (ierr,   ibagaz, &
            &  iwork(jwx:jwh-1), iwork(jwh:jwl-1), &
            &  mem,    &
            &  ksignb, nbitsh, xbi,   &
            &  kxbgn,  kxdnm,  kxlbd,  kxubd, kx0sp, kxspc, &
            &  kcode)
    endif

    ebitsh = nbitsh
    moffsh = 0
    mmskh = 0
    ebitsh = max(ebitsh, min(nbspc, nbitsh))
    ebitsl = nbitsl
    moffsl = 0
    mmskl = 0

    jbgz = KB_HEAD
    jbend = jbgz + count_packed(xbi, mem, kzero)
    kpackx = suggest_filling(xbi, mem, kcode)
    if (xbi.gt.0) then
       call pack_store &
            & (ierr,  ibagaz(jbgz:jbend-1), iwork(jwx:jwh-1), mem, xbi,  kpackx)
    endif
    nbitsa = count_msbits(ebitsh, ksignb)
    jbgz = jbend
    jbend = jbgz + count_packed(nbitsa, mem, kzero)
    kpackh = suggest_filling(nbitsa, mem, kcode)
    if (nbitsa.gt.0) then
       call pack_store &
            & (ierr,  ibagaz(jbgz:jbend-1), iwork(jwh:jwl-1), mem, nbitsa,  kpackh)
    endif
    jbgz = jbend
    jbend = jbgz + count_packed(ebitsl, mem, kzero)
    kpackl = suggest_filling(ebitsl, mem, kcode)
    if (ebitsl.gt.0) then
       call pack_store &
            & (ierr,  ibagaz(jbgz:jbend-1), iwork(jwl:jwe-1), mem, ebitsl,  kpackl)
    endif
    if (ierr.eq.0) ibagaz(0:KB_HEAD-1) = 0
    if (ierr.eq.0) then
       nbgz = jbend - KB_HEAD
       ! call save_basics(ierr, ibagaz, ixdnm, ixlbd, ixubd, kxone, mem, nbgz)
       ! a quick hack.  need improvement
       if (kxbgn.eq.kxlbd) kxdnm = kxlbd
       call save_basics(ierr, ibagaz, kxdnm, kxlbd, kxubd, kxone, mem, nbgz)
    endif
    if (ierr.eq.0) call save_bitprops(ierr, ibagaz, KB_XBIT, xbi,    xbi,    kpackx)
    if (ierr.eq.0) call save_bitprops(ierr, ibagaz, KB_HBIT, nbitsh, ebitsh, kpackh, ksignp_org)
    if (ierr.eq.0) call save_bitprops(ierr, ibagaz, KB_LBIT, nbitsl, ebitsl, kpackl)
    if (ierr.eq.0) call save_masks(ierr, ibagaz, moffsh, mmskh, moffsl, mmskl)

    ! if (.TRUE.) then
    !    call show_work_patterns(ierr, iwork, vsrc, mem, nbitsa=nbitsa, nbitsl=nbitsl)
    ! endif

  end subroutine encode_trig_di

  subroutine encode_trig_fi &
       & (ierr,  ibagaz, iwork, &
       &  vsrc,  mem,    vmiss, &
       &  mbits, xbits,  ixtop, ixbtm, kcode)
    use TOUZA_Trp_std, only: first_bit
    use TOUZA_Trp_pack,only: count_packed, pack_store
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KFLT

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)
    integer(kind=KIBGZ),intent(out) :: iwork(0:*)   ! work array
    real   (kind=KRFLD),intent(in)  :: vsrc(0:*)
    integer,            intent(in)  :: mem
    real   (kind=KRFLD),intent(in)  :: vmiss
    integer,            intent(in)  :: mbits, xbits
    integer,            intent(in)  :: ixtop, ixbtm  ! exponent upper/lower limit (relative to exp(1))
    integer,            intent(in)  :: kcode        ! encoding scheme

    integer(kind=KIBGZ),parameter :: kzero = 0_KIBGZ

    real(kind=KRFLD),   parameter :: vone  = 1.0_KRFLD
    real(kind=KRFLD),   parameter :: vzero = 0.0_KRFLD

    integer,parameter :: kxone = EXPONENT(vone)
    integer,parameter :: kxmin = MINEXPONENT(vone)
    integer,parameter :: kxmax = MAXEXPONENT(vone)
    integer,parameter :: kxspc = kxmax + 1

    integer,parameter :: lfrd = digits(vone) - 1
    integer,parameter :: lbgz = bit_size(kzero)

    integer jwx, jwh, jwl       ! work array offset position
    integer nbitsh, nbitsl
    integer ebitsh, ebitsl, nbitsa
    integer jbgz,   jbend,  nbgz

    integer :: kxbgn, kxdnm, kxlbd, kxubd, kx0sp  ! native exponent
    integer xbi
    logical bzero, bmiss, binf, bdnm
    integer nbspc
    integer ksignp_org, ksignb
    integer(kind=KIBGZ) :: moffsh, mmskh
    integer(kind=KIBGZ) :: moffsl, mmskl
    integer kpackx, kpackh, kpackl

    ierr = 0

    xbi = xbits
    jwx = 0
    jwh = jwx + mem
    jwl = jwh + mem

    if (mbits.ge.lbgz) then
       ierr = -1
       stop
    else
       if (IAND(kcode, KCODE_ROUND).gt.0) then
          call desmontar_high_round_fi &
               & (ierr,   &
               &  iwork(jwx:jwh-1), iwork(jwh:jwl-1), &
               &  kxdnm,      kxlbd,  kxubd,  bzero, bmiss, binf, bdnm, &
               &  ksignp_org, nbitsh, nbitsl, &
               &  vsrc,       mem,    vmiss,  mbits, kxspc)
       else
          call desmontar_high_fi &
               & (ierr,   &
               &  iwork(jwx:jwh-1), iwork(jwh:jwl-1), &
               &  kxdnm,      kxlbd,  kxubd,  bzero, bmiss, binf, bdnm, &
               &  ksignp_org, nbitsh, nbitsl, &
               &  vsrc,       mem,    vmiss,  mbits, kxspc)
       endif
    endif

    ! if (.TRUE.) then
    !    nbitsa = count_msbits(nbitsh, ksignp)
    !    call show_work_patterns(ierr, iwork, vsrc, mem, nbitsa=nbitsa, nbitsl=nbitsl)
    ! endif

    call abarcar &
         & (kxbgn, kxdnm, kxlbd, kxubd, kx0sp, xbi,   nbspc, &
         &  mbits, bzero, bmiss, binf,  bdnm,  &
         &  ixtop, ixbtm, kxmin, kxmax, kxone, lfrd)

    ksignb = ksignp_org
    if (IAND(kcode, KCODE_SIGN_ZERO).gt.0) then
       if (ksignb.eq.KF_NEG_ZERO .or. ksignb.eq.KF_POS_ZERO) ksignb = KF_MIXED
    endif

    call ajustar_high_i &
         & (ierr,   ibagaz, &
         &  iwork(jwx:jwh-1), iwork(jwh:jwl-1), &
         &  mem,    &
         &  ksignb, nbitsh, xbi,   &
         &  kxbgn,  kxdnm,  kxlbd,  kxubd, kx0sp, kxspc, &
         &  kcode)

    ebitsh = nbitsh
    moffsh = 0
    mmskh = 0
    ebitsh = max(ebitsh, min(nbspc, nbitsh))
    ebitsl = nbitsl
    moffsl = 0
    mmskl = 0

    jbgz = KB_HEAD
    jbend = jbgz + count_packed(xbi, mem, kzero)
    kpackx = suggest_filling(xbi, mem, kcode)
    if (xbi.gt.0) then
       call pack_store &
            & (ierr,  ibagaz(jbgz:jbend-1), iwork(jwx:jwh-1), mem, xbi,  kpackx)
    endif
    nbitsa = count_msbits(ebitsh, ksignb)
    jbgz = jbend
    jbend = jbgz + count_packed(nbitsa, mem, kzero)
    kpackh = suggest_filling(nbitsa, mem, kcode)
    if (nbitsa.gt.0) then
       call pack_store &
            & (ierr,  ibagaz(jbgz:jbend-1), iwork(jwh:jwl-1), mem, nbitsa,  kpackh)
    endif
    jbgz = jbend
    jbend = jbgz + count_packed(ebitsl, mem, kzero)
    kpackl = suggest_filling(ebitsl, mem, kcode)

    if (ierr.eq.0) ibagaz(0:KB_HEAD-1) = 0
    if (ierr.eq.0) then
       nbgz = jbend - KB_HEAD
       ! call save_basics(ierr, ibagaz, ixdnm, ixlbd, ixubd, kxone, mem, nbgz)
       ! a quick hack.  need improvement
       if (kxbgn.eq.kxlbd) kxdnm = kxlbd
       call save_basics(ierr, ibagaz, kxdnm, kxlbd, kxubd, kxone, mem, nbgz)
    endif
    if (ierr.eq.0) call save_bitprops(ierr, ibagaz, KB_XBIT, xbi,    xbi,    kpackx)
    if (ierr.eq.0) call save_bitprops(ierr, ibagaz, KB_HBIT, nbitsh, ebitsh, kpackh, ksignp_org)
    if (ierr.eq.0) call save_bitprops(ierr, ibagaz, KB_LBIT, nbitsl, ebitsl, kpackl)
    if (ierr.eq.0) call save_masks(ierr, ibagaz, moffsh, mmskh, moffsl, mmskl)

    ! if (.TRUE.) then
    !    call show_work_patterns(ierr, iwork, vsrc, mem, nbitsa=nbitsa, nbitsl=nbitsl)
    ! endif

  end subroutine encode_trig_fi

!!!_  & decode_alloc - trapiche decoder (internal allocation)
  subroutine decode_alloc_d &
       & (ierr,   &
       &  d,      &
       &  ibagaz, mem,   vmiss, kcode)
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KDBL
    integer,            intent(out)         :: ierr
    real   (kind=KRFLD),intent(out)         :: d(0:*)
    integer(kind=KIBGZ),intent(in)          :: ibagaz(0:*)
    real   (kind=KRFLD),intent(in)          :: vmiss
    integer,            intent(in)          :: mem
    integer,            intent(in),optional :: kcode

    ierr = err_default
    if (ierr.eq.0) call alloc_works(ierr, mem * 2)
    if (ierr.eq.0) then
       call diluir &
            & (ierr, d, wibuf(0:mem-1), wibuf(mem:), ibagaz, mem, vmiss, kcode)
    endif
    return
  end subroutine decode_alloc_d
  subroutine decode_alloc_f &
       & (ierr,   &
       &  d,      &
       &  ibagaz, mem,   vmiss, kcode)
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KFLT
    integer,            intent(out)         :: ierr
    real   (kind=KRFLD),intent(out)         :: d(0:*)
    integer(kind=KIBGZ),intent(in)          :: ibagaz(0:*)
    real   (kind=KRFLD),intent(in)          :: vmiss
    integer,            intent(in)          :: mem
    integer,            intent(in),optional :: kcode

    ierr = err_default
    if (ierr.eq.0) call alloc_works(ierr, mem * 2)
    if (ierr.eq.0) then
       call diluir &
            & (ierr, d, wibuf(0:mem-1), wibuf(mem:), ibagaz, mem, vmiss, kcode)
    endif
    return
  end subroutine decode_alloc_f
!!!_  & decode_stack - trapiche decoder (stack)
  subroutine decode_stack_d &
       & (ierr,   &
       &  d,      &
       &  ibagaz, mem,   vmiss,  kcode)
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KDBL
    integer,            intent(out)         :: ierr
    real   (kind=KRFLD),intent(out)         :: d(0:*)
    integer(kind=KIBGZ),intent(in)          :: ibagaz(0:*)
    real   (kind=KRFLD),intent(in)          :: vmiss
    integer,            intent(in)          :: mem
    integer,            intent(in),optional :: kcode

    integer ictlg(0:mem-1)
    integer iwork(0:mem-1)

    ierr = err_default
    if (ierr.eq.0) then
       call diluir &
            & (ierr, d, ictlg, iwork, ibagaz, mem, vmiss, kcode)
    endif
    return
  end subroutine decode_stack_d
  subroutine decode_stack_f &
       & (ierr,   &
       &  d,      &
       &  ibagaz, mem,   vmiss, kcode)
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KFLT
    integer,            intent(out)         :: ierr
    real   (kind=KRFLD),intent(out)         :: d(0:*)
    integer(kind=KIBGZ),intent(in)          :: ibagaz(0:*)
    real   (kind=KRFLD),intent(in)          :: vmiss
    integer,            intent(in)          :: mem
    integer,            intent(in),optional :: kcode

    integer ictlg(0:mem-1)
    integer iwork(0:mem-1)

    ierr = err_default
    if (ierr.eq.0) then
       call diluir &
            & (ierr, d, ictlg, iwork, ibagaz, mem, vmiss, kcode)
    endif
    return
  end subroutine decode_stack_f
!!!_  & decode_work - trapiche decoder (work-area arguments)
  subroutine decode_work_d &
       & (ierr,   &
       &  d,      ictlg, iwork, &
       &  ibagaz, mem,   vmiss, kcode)
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KDBL
    integer,            intent(out)         :: ierr
    real   (kind=KRFLD),intent(out)         :: d(0:*)
    integer,            intent(out)         :: ictlg(0:mem-1)
    integer,            intent(out)         :: iwork(0:mem-1)
    integer(kind=KIBGZ),intent(in)          :: ibagaz(0:*)
    real   (kind=KRFLD),intent(in)          :: vmiss
    integer,            intent(in)          :: mem
    integer,            intent(in),optional :: kcode

    ierr = err_default
    if (ierr.eq.0) then
       call diluir &
            & (ierr, d, ictlg, iwork, ibagaz, mem, vmiss, kcode)
    endif
    return
  end subroutine decode_work_d
  subroutine decode_work_f &
       & (ierr,   &
       &  d,      ictlg, iwork, &
       &  ibagaz, mem,   vmiss, kcode)
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KFLT
    integer,            intent(out)         :: ierr
    real   (kind=KRFLD),intent(out)         :: d(0:*)
    integer,            intent(out)         :: ictlg(0:mem-1)
    integer,            intent(out)         :: iwork(0:mem-1)
    integer(kind=KIBGZ),intent(in)          :: ibagaz(0:*)
    real   (kind=KRFLD),intent(in)          :: vmiss
    integer,            intent(in)          :: mem
    integer,            intent(in),optional :: kcode

    ierr = err_default
    if (ierr.eq.0) then
       call diluir &
            & (ierr, d, ictlg, iwork, ibagaz, mem, vmiss, kcode)
    endif
    return
  end subroutine decode_work_f

!!!_ + user subroutines
!!!_  & health_check
  subroutine health_check_d &
       & (ierr, vhld, u, levv)
    use TOUZA_Trp_std,only: choice, &
         & msg, is_msglev, is_msglev_detail, &
         & check_real_dnm
    implicit none
    integer,parameter :: KRFLD=KDBL
    integer,         intent(out)         :: ierr
    real(kind=KRFLD),intent(in)          :: vhld
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer ixdnm
    integer lv

    ierr = 0
    lv = choice(lev_verbose, levv)

    if (ierr.eq.0) call check_real_dnm(ixdnm, vhld, levv=-99) ! force silent
    if (is_msglev_detail(lv)) then
       call msg('(''denormalized = '', I0)', ixdnm, __MDL__, u)
    endif
    return
  end subroutine health_check_d
  subroutine health_check_f &
       & (ierr, vhld, u, levv)
    use TOUZA_Trp_std,only: choice, &
         & msg, is_msglev, is_msglev_detail, &
         & check_real_dnm
    implicit none
    integer,parameter :: KRFLD=KFLT
    integer,         intent(out)         :: ierr
    real(kind=KRFLD),intent(in)          :: vhld
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer ixdnm
    integer lv

    ierr = 0
    lv = choice(lev_verbose, levv)

    if (ierr.eq.0) call check_real_dnm(ixdnm, vhld, levv=-99) ! force silent
    if (is_msglev_detail(lv)) then
       call msg('(''denormalized = '', I0)', ixdnm, __MDL__, u)
    endif
    return
  end subroutine health_check_f

!!!_  & asignar - assign how to decode special values
  subroutine asignar &
       & (ierr, tag, kscheme)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: tag
    integer,         intent(in)  :: kscheme

    ierr = 0
    select case (tag(1:1))
    case ('d', 'D')
       KSCHM_DNM = kscheme
    case ('i', 'I')
       KSCHM_INF = kscheme
    case ('n', 'N')
       KSCHM_NAN = kscheme
    case ('o', 'O')
       KSCHM_OVFL = kscheme
    case ('u', 'U')
       KSCHM_UNFL = kscheme
    case default
       ierr = -1
    end select
    return
  end subroutine asignar

!!!_  & desmontar_full - scan/disassemble source to exponent/mantissa high/low
  subroutine desmontar_full_di &
       & (ierr,   &
       &  iwx,    iwh,    iwl,    &
       &  kxdnm,  kxlbd,  kxubd,  &
       &  bzero,  bmiss,  binf,   bdnm, &
       &  ksignp, nbitsh, nbitsl, &
       &  vsrc,   mem,    vmiss,  mbits, kxspc)
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KDBL

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: iwx(0:*)
    integer(kind=KIBGZ),intent(out) :: iwh(0:*)
    integer(kind=KIBGZ),intent(out) :: iwl(0:*)
    integer,            intent(out) :: kxdnm,  kxlbd, kxubd
    logical,            intent(out) :: bzero,  bmiss, binf,  bdnm
    integer,            intent(out) :: ksignp
    integer,            intent(out) :: nbitsh, nbitsl
    ! integer(kind=KIBGZ),intent(out) :: maxhu, minhu, maxhl, minhl
    ! integer(kind=KIBGZ),intent(out) :: maxlu, minlu, maxll, minll
    real   (kind=KRFLD),intent(in)  :: vsrc(0:*)
    real   (kind=KRFLD),intent(in)  :: vmiss
    integer,            intent(in)  :: mem
    integer,            intent(in)  :: mbits
    integer,            intent(in)  :: kxspc

    integer(kind=KIBGZ),parameter :: kzero = 0_KIBGZ

    real(kind=KRFLD),   parameter :: vone  = 1.0_KRFLD
    real(kind=KRFLD),   parameter :: vzero = 0.0_KRFLD

    integer,parameter :: kxone = EXPONENT(vone)
    integer,parameter :: kxmin = MINEXPONENT(vone)
    integer,parameter :: kxmax = MAXEXPONENT(vone)

    integer,parameter :: lfrd = digits(vone) - 1
    integer,parameter :: lbgz = bit_size(kzero)

    integer kx
    integer j

    integer ixshh, ixshl, ixshm

    real(kind=KRFLD) :: vmskl, vmskh, vh, vl, vsign
    logical bzerom, bzerop, bneg, bpos

    ierr = 0

    nbitsh = lbgz - 1
    nbitsl = max(0, mbits - nbitsh)

    ixshh = nbitsh + kxone
    ixshl = nbitsl + kxone
    ixshm = mbits  + kxone

    vmskh = _SET_EXPONENT(vone, ixshh)     ! first (skipped) bit
    vmskl = _SET_EXPONENT(vone, nbitsl + kxone)

    ! exponent(0)   = 0       float(0)    = 0
    ! exponent(inf) = HUGE    float(inf)  = infinity
    ! exponent(nan) = HUGE    float(nan)  = nan

    kxlbd = + HUGE(kzero)
    kxubd = - HUGE(kzero) - 1
    kxdnm = - HUGE(kzero) - 1

    ! decomposition and scaning
    ! exponent/sign+high/low
    bzerom = .FALSE.
    bzerop = .FALSE.
    bneg   = .FALSE.
    bpos   = .FALSE.
    bmiss  = .FALSE.
    binf   = .FALSE.
    bdnm   = .FALSE.
    do j = 0, mem - 1
       if      (vsrc(j).eq.vzero) then
          iwx(j) = kxspc
          iwh(j) = KX_ZERO
          iwl(j) = 0
          vsign = sign(vone, vsrc(j))
          if (vsign.lt.vzero) then
             iwh(j) = IBSET(iwh(j), nbitsh)
             bzerom = .TRUE.
          else
             bzerop = .TRUE.
          endif
       else if (vsrc(j).eq.vmiss) then
          iwx(j) = kxspc
          iwh(j) = KX_MISS
          iwl(j) = 0
          bmiss = .TRUE.
       else
          kx = _EXPONENT(vsrc(j))
          if      (kx.gt.kxmax) then
             ! nan or infinity
             iwx(j) = kxspc
             iwh(j) = KX_INF
             iwl(j) = 0
             binf = .TRUE.
          ! else if (kx.lt.kxmin) then
          !    iwork(jwx+j) = kx
          else
             if (kx.lt.kxmin) then
                bdnm = .TRUE.
                kxdnm = max(kxdnm, kx)
             else
                kxlbd = min(kxlbd, kx)
                kxubd = max(kxubd, kx)
             endif
             iwx(j) = kx
             vh = _SET_EXPONENT(abs(vsrc(j)), ixshh)
             iwh(j) = int(vh - vmskh)
             ! ! The following implementation should be identical,
             ! ! but, for some environment (tested on es4 intel)
             ! ! the lower mantissa may be rounded to 1-bit neighbor.
             ! ! (For a worst case, 111111 becomes 000000.)
             ! ! Need -fp-model=source option at compile time
             ! ! to avoid such situations.
             ! vl = _SET_EXPONENT((vh - aint(vh)) + vone, ixshl)
             ! iwl(j) = int(vl - vmskl)
             ! ! The following implementation gives expected
             ! ! lower mantissa, as far as I checked.
             vl = mod(_SET_EXPONENT(abs(vsrc(j)), ixshm), vmskl)
             iwl(j) = int(vl)
          endif
          vsign = sign(vone, vsrc(j))
          if (vsign.lt.vzero) then
             iwh(j) = IBSET(iwh(j), nbitsh)
             bneg = .TRUE.
          else
             bpos = .TRUE.
          endif
       endif
    enddo
    ksignp = sign_flag(bzerom, bzerop, bneg, bpos)
    bzero = (bzerom.or.bzerop)
    ! write(*,*) ksignp, bzerom, bzerop, bneg, bpos, binf, bdnm, bmiss
    return
  end subroutine desmontar_full_di
!!!_  & desmontar_high - scan/disassemble source to exponent/mantissa high
  subroutine desmontar_high_di &
       & (ierr,   &
       &  iwx,    iwh,    &
       &  kxdnm,  kxlbd,  kxubd,  &
       &  bzero,  bmiss,  binf,   bdnm, &
       &  ksignp, nbitsh, nbitsl, &
       &  vsrc,   mem,    vmiss,  mbits, kxspc)
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KDBL

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: iwx(0:*)
    integer(kind=KIBGZ),intent(out) :: iwh(0:*)
    integer,            intent(out) :: kxdnm,  kxlbd,  kxubd
    logical,            intent(out) :: bzero,  bmiss,  binf,   bdnm
    integer,            intent(out) :: ksignp
    integer,            intent(out) :: nbitsh, nbitsl
    real   (kind=KRFLD),intent(in)  :: vsrc(0:*)
    real   (kind=KRFLD),intent(in)  :: vmiss
    integer,            intent(in)  :: mem
    integer,            intent(in)  :: mbits
    integer,            intent(in)  :: kxspc

    integer(kind=KIBGZ),parameter :: kzero = 0_KIBGZ

    real(kind=KRFLD),   parameter :: vone  = 1.0_KRFLD
    real(kind=KRFLD),   parameter :: vzero = 0.0_KRFLD

    integer,parameter :: kxone = EXPONENT(vone)
    integer,parameter :: kxmin = MINEXPONENT(vone)
    integer,parameter :: kxmax = MAXEXPONENT(vone)

    integer,parameter :: lfrd = digits(vone) - 1
    integer,parameter :: lbgz = bit_size(kzero)

    integer kx
    integer j

    integer ixshh

    real(kind=KRFLD) :: vmskh, vh, vsign
    logical bzerom, bzerop, bneg, bpos
    integer mhalf

    ierr = 0

    nbitsh = mbits
    nbitsl = 0

    ixshh = nbitsh + kxone

    vmskh = _SET_EXPONENT(vone, ixshh)     ! first (skipped) bit

    kxlbd = + HUGE(kzero)
    kxubd = - HUGE(kzero) - 1
    kxdnm = - HUGE(kzero) - 1

    bzerom = .FALSE.
    bzerop = .FALSE.
    bneg   = .FALSE.
    bpos   = .FALSE.
    bmiss  = .FALSE.
    binf   = .FALSE.
    bdnm   = .FALSE.

    do j = 0, mem - 1
       if      (vsrc(j).eq.vzero) then
          iwx(j) = kxspc
          iwh(j) = KX_ZERO
          vsign = sign(vone, vsrc(j))
          if (vsign.lt.vzero) then
             iwh(j) = IBSET(iwh(j), nbitsh)
             bzerom = .TRUE.
          else
             bzerop = .TRUE.
          endif
       else if (vsrc(j).eq.vmiss) then
          iwx(j) = kxspc
          iwh(j) = KX_MISS
          bmiss = .TRUE.
       else
          kx = _EXPONENT(vsrc(j))
          if      (kx.gt.kxmax) then
             ! nan or infinity
             iwx(j) = kxspc
             iwh(j) = KX_INF
             binf = .TRUE.
          else
             if (kx.lt.kxmin) then
                bdnm = .TRUE.
                kxdnm = max(kxdnm, kx)
             else
                kxlbd = min(kxlbd, kx)
                kxubd = max(kxubd, kx)
             endif
             iwx(j) = kx
             vh = _SET_EXPONENT(abs(vsrc(j)), ixshh)
             iwh(j) = int(vh - vmskh)
          endif
          vsign = sign(vone, vsrc(j))
          if (vsign.lt.vzero) then
             iwh(j) = IBSET(iwh(j), nbitsh)
             bneg = .TRUE.
          else
             bpos = .TRUE.
          endif
       endif
    enddo
    ksignp = sign_flag(bzerom, bzerop, bneg, bpos)
    bzero = (bzerom.or.bzerop)
    return
  end subroutine desmontar_high_di
  subroutine desmontar_high_fi &
       & (ierr,   &
       &  iwx,    iwh,    &
       &  kxdnm,  kxlbd,  kxubd,  &
       &  bzero,  bmiss,  binf,   bdnm, &
       &  ksignp, nbitsh, nbitsl, &
       &  vsrc,   mem,    vmiss,  mbits, kxspc)
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KFLT

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: iwx(0:*)
    integer(kind=KIBGZ),intent(out) :: iwh(0:*)
    integer,            intent(out) :: kxdnm,  kxlbd, kxubd
    logical,            intent(out) :: bzero, bmiss, binf, bdnm
    integer,            intent(out) :: ksignp
    integer,            intent(out) :: nbitsh, nbitsl
    real   (kind=KRFLD),intent(in)  :: vsrc(0:*)
    real   (kind=KRFLD),intent(in)  :: vmiss
    integer,            intent(in)  :: mem
    integer,            intent(in)  :: mbits
    integer,            intent(in)  :: kxspc

    integer(kind=KIBGZ),parameter :: kzero = 0_KIBGZ

    real(kind=KRFLD),   parameter :: vone  = 1.0_KRFLD
    real(kind=KRFLD),   parameter :: vzero = 0.0_KRFLD

    integer,parameter :: kxone = EXPONENT(vone)
    integer,parameter :: kxmin = MINEXPONENT(vone)
    integer,parameter :: kxmax = MAXEXPONENT(vone)

    integer,parameter :: lfrd = digits(vone) - 1
    integer,parameter :: lbgz = bit_size(kzero)

    integer kx
    integer j

    integer ixshh

    real(kind=KRFLD) :: vmskh, vh, vsign
    logical bzerom, bzerop, bneg, bpos

    ierr = 0

    nbitsh = mbits
    nbitsl = 0

    ixshh = nbitsh + kxone

    vmskh = _SET_EXPONENT(vone, ixshh)     ! first (skipped) bit

    kxlbd = + HUGE(kzero)
    kxubd = - HUGE(kzero) - 1
    kxdnm = - HUGE(kzero) - 1

    ! decomposition and scaning
    ! exponent/sign+high/low
    bzerom = .FALSE.
    bzerop = .FALSE.
    bneg   = .FALSE.
    bpos   = .FALSE.
    bmiss  = .FALSE.
    binf   = .FALSE.
    bdnm   = .FALSE.
    do j = 0, mem - 1
       if      (vsrc(j).eq.vzero) then
          iwx(j) = kxspc
          iwh(j) = KX_ZERO
          vsign = sign(vone, vsrc(j))
          if (vsign.lt.vzero) then
             iwh(j) = IBSET(iwh(j), nbitsh)
             bzerom = .TRUE.
          else
             bzerop = .TRUE.
          endif
       else if (vsrc(j).eq.vmiss) then
          iwx(j) = kxspc
          iwh(j) = KX_MISS
          bmiss = .TRUE.
       else
          kx = _EXPONENT(vsrc(j))
          if      (kx.gt.kxmax) then
             ! nan or infinity
             iwx(j) = kxspc
             iwh(j) = KX_INF
             binf = .TRUE.
          ! else if (kx.lt.kxmin) then
          !    iwork(jwx+j) = kx
          else
             if (kx.lt.kxmin) then
                bdnm = .TRUE.
                kxdnm = max(kxdnm, kx)
             else
                kxlbd = min(kxlbd, kx)
                kxubd = max(kxubd, kx)
             endif
             iwx(j) = kx
             vh = _SET_EXPONENT(abs(vsrc(j)), ixshh)
             iwh(j) = int(vh - vmskh)
          endif
          vsign = sign(vone, vsrc(j))
          if (vsign.lt.vzero) then
             iwh(j) = IBSET(iwh(j), nbitsh)
             bneg = .TRUE.
          else
             bpos = .TRUE.
          endif
       endif
    enddo
    ksignp = sign_flag(bzerom, bzerop, bneg, bpos)
    bzero = (bzerom.or.bzerop)
    ! write(*,*) ksignp, bzerom, bzerop, bneg, bpos, binf, bdnm, bmiss
    return
  end subroutine desmontar_high_fi

!!!_  & desmontar_full_round - scan/disassemble source to exponent/mantissa high/low (round)
  subroutine desmontar_full_round_di &
       & (ierr,   &
       &  iwx,    iwh,    iwl,    &
       &  kxdnm,  kxlbd,  kxubd,  &
       &  bzero,  bmiss,  binf,   bdnm, &
       &  ksignp, nbitsh, nbitsl, &
       &  vsrc,   mem,    vmiss,  mbits, kxspc)
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KDBL

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: iwx(0:*)
    integer(kind=KIBGZ),intent(out) :: iwh(0:*)
    integer(kind=KIBGZ),intent(out) :: iwl(0:*)
    integer,            intent(out) :: kxdnm,  kxlbd, kxubd
    logical,            intent(out) :: bzero,  bmiss, binf,  bdnm
    integer,            intent(out) :: ksignp
    integer,            intent(out) :: nbitsh, nbitsl
    real   (kind=KRFLD),intent(in)  :: vsrc(0:*)
    real   (kind=KRFLD),intent(in)  :: vmiss
    integer,            intent(in)  :: mem
    integer,            intent(in)  :: mbits
    integer,            intent(in)  :: kxspc

    integer(kind=KIBGZ),parameter :: kzero = 0_KIBGZ

    real(kind=KRFLD),   parameter :: vone  = 1.0_KRFLD
    real(kind=KRFLD),   parameter :: vzero = 0.0_KRFLD

    integer,parameter :: kxone = EXPONENT(vone)
    integer,parameter :: kxmin = MINEXPONENT(vone)
    integer,parameter :: kxmax = MAXEXPONENT(vone)

    integer,parameter :: lfrd = digits(vone) - 1
    integer,parameter :: lbgz = bit_size(kzero)

    integer kx
    integer j

    integer ixshh, ixshl, ixshm

    real(kind=KRFLD) :: vmskl, vmskh, vh, vl, vsign, vt
    logical bzerom, bzerop, bneg, bpos

    ierr = 0

    nbitsh = lbgz - 1
    nbitsl = max(0, mbits - nbitsh)

    ixshh = nbitsh + kxone
    ixshl = nbitsl + kxone
    ixshm = mbits  + kxone

    vmskh = _SET_EXPONENT(vone, ixshh)     ! first (skipped) bit
    vmskl = _SET_EXPONENT(vone, nbitsl + kxone)

    kxlbd = + HUGE(kzero)
    kxubd = - HUGE(kzero) - 1
    kxdnm = - HUGE(kzero) - 1

    bzerom = .FALSE.
    bzerop = .FALSE.
    bneg   = .FALSE.
    bpos   = .FALSE.
    bmiss  = .FALSE.
    binf   = .FALSE.
    bdnm   = .FALSE.
    do j = 0, mem - 1
       if      (vsrc(j).eq.vzero) then
          iwx(j) = kxspc
          iwh(j) = KX_ZERO
          iwl(j) = 0
          vsign = sign(vone, vsrc(j))
          if (vsign.lt.vzero) then
             iwh(j) = IBSET(iwh(j), nbitsh)
             bzerom = .TRUE.
          else
             bzerop = .TRUE.
          endif
       else if (vsrc(j).eq.vmiss) then
          iwx(j) = kxspc
          iwh(j) = KX_MISS
          iwl(j) = 0
          bmiss = .TRUE.
       else
          vt = ANINT(_SET_EXPONENT(abs(vsrc(j)), mbits+kxone))
          kx = _EXPONENT(vsrc(j)) + (_EXPONENT(vt) - (mbits+kxone))
          if      (kx.gt.kxmax) then
             ! nan or infinity
             iwx(j) = kxspc
             iwh(j) = KX_INF
             iwl(j) = 0
             binf = .TRUE.
          else
             if (kx.lt.kxmin) then
                bdnm = .TRUE.
                kxdnm = max(kxdnm, kx)
             else
                kxlbd = min(kxlbd, kx)
                kxubd = max(kxubd, kx)
             endif
             iwx(j) = kx
             vh = _SET_EXPONENT(vt, ixshh)
             iwh(j) = int(vh - vmskh)
             vl = mod(vt, vmskl)
             iwl(j) = int(vl)
          endif
          vsign = sign(vone, vsrc(j))
          if (vsign.lt.vzero) then
             iwh(j) = IBSET(iwh(j), nbitsh)
             bneg = .TRUE.
          else
             bpos = .TRUE.
          endif
       endif
    enddo
    ksignp = sign_flag(bzerom, bzerop, bneg, bpos)
    bzero = (bzerom.or.bzerop)
    ! write(*,*) ksignp, bzerom, bzerop, bneg, bpos, binf, bdnm, bmiss
    return
  end subroutine desmontar_full_round_di
!!!_  & desmontar_high_round - scan/disassemble source to exponent/mantissa high (round)
  subroutine desmontar_high_round_di &
       & (ierr,   &
       &  iwx,    iwh,    &
       &  kxdnm,  kxlbd,  kxubd,  &
       &  bzero,  bmiss,  binf,   bdnm, &
       &  ksignp, nbitsh, nbitsl, &
       &  vsrc,   mem,    vmiss,  mbits, kxspc)
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KDBL

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: iwx(0:*)
    integer(kind=KIBGZ),intent(out) :: iwh(0:*)
    integer,            intent(out) :: kxdnm,  kxlbd,  kxubd
    logical,            intent(out) :: bzero,  bmiss,  binf,   bdnm
    integer,            intent(out) :: ksignp
    integer,            intent(out) :: nbitsh, nbitsl
    real   (kind=KRFLD),intent(in)  :: vsrc(0:*)
    real   (kind=KRFLD),intent(in)  :: vmiss
    integer,            intent(in)  :: mem
    integer,            intent(in)  :: mbits
    integer,            intent(in)  :: kxspc

    integer(kind=KIBGZ),parameter :: kzero = 0_KIBGZ

    real(kind=KRFLD),   parameter :: vone  = 1.0_KRFLD
    real(kind=KRFLD),   parameter :: vzero = 0.0_KRFLD

    integer,parameter :: kxone = EXPONENT(vone)
    integer,parameter :: kxmin = MINEXPONENT(vone)
    integer,parameter :: kxmax = MAXEXPONENT(vone)

    integer,parameter :: lfrd = digits(vone) - 1
    integer,parameter :: lbgz = bit_size(kzero)

    integer kx
    integer j

    integer ixshh

    real(kind=KRFLD) :: vmskh, vh, vsign
    logical bzerom, bzerop, bneg, bpos
    integer mhalf

    ierr = 0

    nbitsh = mbits
    nbitsl = 0

    ixshh = nbitsh + kxone

    vmskh = _SET_EXPONENT(vone, ixshh)     ! first (skipped) bit

    kxlbd = + HUGE(kzero)
    kxubd = - HUGE(kzero) - 1
    kxdnm = - HUGE(kzero) - 1

    bzerom = .FALSE.
    bzerop = .FALSE.
    bneg   = .FALSE.
    bpos   = .FALSE.
    bmiss  = .FALSE.
    binf   = .FALSE.
    bdnm   = .FALSE.

    do j = 0, mem - 1
       if      (vsrc(j).eq.vzero) then
          iwx(j) = kxspc
          iwh(j) = KX_ZERO
          vsign = sign(vone, vsrc(j))
          if (vsign.lt.vzero) then
             iwh(j) = IBSET(iwh(j), nbitsh)
             bzerom = .TRUE.
          else
             bzerop = .TRUE.
          endif
       else if (vsrc(j).eq.vmiss) then
          iwx(j) = kxspc
          iwh(j) = KX_MISS
          bmiss = .TRUE.
       else
          kx = _EXPONENT(vsrc(j))
          if      (kx.gt.kxmax) then
             ! nan or infinity
             iwx(j) = kxspc
             iwh(j) = KX_INF
             binf = .TRUE.
          else
             vh = ANINT(_SET_EXPONENT(abs(vsrc(j)), mbits+kxone))
             kx = _EXPONENT(vsrc(j)) + (_EXPONENT(vh) - (mbits+kxone))
             vh = _SET_EXPONENT(vh, ixshh)
             iwh(j) = int(vh - vmskh)
             if (kx.lt.kxmin) then
                bdnm = .TRUE.
                kxdnm = max(kxdnm, kx)
             else
                kxlbd = min(kxlbd, kx)
                kxubd = max(kxubd, kx)
             endif
             iwx(j) = kx
          endif
          vsign = sign(vone, vsrc(j))
          if (vsign.lt.vzero) then
             iwh(j) = IBSET(iwh(j), nbitsh)
             bneg = .TRUE.
          else
             bpos = .TRUE.
          endif
       endif
    enddo
    ksignp = sign_flag(bzerom, bzerop, bneg, bpos)
    bzero = (bzerom.or.bzerop)
    return
  end subroutine desmontar_high_round_di
  subroutine desmontar_high_round_fi &
       & (ierr,   &
       &  iwx,    iwh,    &
       &  kxdnm,  kxlbd,  kxubd,  &
       &  bzero,  bmiss,  binf,   bdnm, &
       &  ksignp, nbitsh, nbitsl, &
       &  vsrc,   mem,    vmiss,  mbits, kxspc)
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KFLT

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: iwx(0:*)
    integer(kind=KIBGZ),intent(out) :: iwh(0:*)
    integer,            intent(out) :: kxdnm,  kxlbd,  kxubd
    logical,            intent(out) :: bzero,  bmiss,  binf,   bdnm
    integer,            intent(out) :: ksignp
    integer,            intent(out) :: nbitsh, nbitsl
    real   (kind=KRFLD),intent(in)  :: vsrc(0:*)
    real   (kind=KRFLD),intent(in)  :: vmiss
    integer,            intent(in)  :: mem
    integer,            intent(in)  :: mbits
    integer,            intent(in)  :: kxspc

    integer(kind=KIBGZ),parameter :: kzero = 0_KIBGZ

    real(kind=KRFLD),   parameter :: vone  = 1.0_KRFLD
    real(kind=KRFLD),   parameter :: vzero = 0.0_KRFLD

    integer,parameter :: kxone = EXPONENT(vone)
    integer,parameter :: kxmin = MINEXPONENT(vone)
    integer,parameter :: kxmax = MAXEXPONENT(vone)

    integer,parameter :: lfrd = digits(vone) - 1
    integer,parameter :: lbgz = bit_size(kzero)

    integer kx
    integer j

    integer ixshh

    real(kind=KRFLD) :: vmskh, vh, vsign
    logical bzerom, bzerop, bneg, bpos
    integer mhalf

    ierr = 0

    nbitsh = mbits
    nbitsl = 0

    ixshh = nbitsh + kxone

    vmskh = _SET_EXPONENT(vone, ixshh)     ! first (skipped) bit

    kxlbd = + HUGE(kzero)
    kxubd = - HUGE(kzero) - 1
    kxdnm = - HUGE(kzero) - 1

    bzerom = .FALSE.
    bzerop = .FALSE.
    bneg   = .FALSE.
    bpos   = .FALSE.
    bmiss  = .FALSE.
    binf   = .FALSE.
    bdnm   = .FALSE.

    do j = 0, mem - 1
       if      (vsrc(j).eq.vzero) then
          iwx(j) = kxspc
          iwh(j) = KX_ZERO
          vsign = sign(vone, vsrc(j))
          if (vsign.lt.vzero) then
             iwh(j) = IBSET(iwh(j), nbitsh)
             bzerom = .TRUE.
          else
             bzerop = .TRUE.
          endif
       else if (vsrc(j).eq.vmiss) then
          iwx(j) = kxspc
          iwh(j) = KX_MISS
          bmiss = .TRUE.
       else
          kx = _EXPONENT(vsrc(j))
          if      (kx.gt.kxmax) then
             ! nan or infinity
             iwx(j) = kxspc
             iwh(j) = KX_INF
             binf = .TRUE.
          else
             vh = ANINT(_SET_EXPONENT(abs(vsrc(j)), mbits+kxone))
             kx = _EXPONENT(vsrc(j)) + (_EXPONENT(vh) - (mbits+kxone))
             vh = _SET_EXPONENT(vh, ixshh)
             iwh(j) = int(vh - vmskh)
             if (kx.lt.kxmin) then
                bdnm = .TRUE.
                kxdnm = max(kxdnm, kx)
             else
                kxlbd = min(kxlbd, kx)
                kxubd = max(kxubd, kx)
             endif
             iwx(j) = kx
          endif
          vsign = sign(vone, vsrc(j))
          if (vsign.lt.vzero) then
             iwh(j) = IBSET(iwh(j), nbitsh)
             bneg = .TRUE.
          else
             bpos = .TRUE.
          endif
       endif
    enddo
    ksignp = sign_flag(bzerom, bzerop, bneg, bpos)
    bzero = (bzerom.or.bzerop)
    return
  end subroutine desmontar_high_round_fi

!!!_  - sign_flag - detect sign distribution
  integer function sign_flag &
       & (bzerom, bzerop, bneg, bpos) &
       & result(n)
    implicit none
    logical,intent(in) :: bzerom, bzerop, bneg, bpos
    n = 0
    if (bzerom) n = n + 1
    if (bzerop) n = n + 2
    if (bneg)   n = n + 4
    if (bpos)   n = n + 8
    n = ksign_table(n)
    return
  end function sign_flag
!!!_  & abarcar
  subroutine abarcar &
       & (kxbgn, kxdnm, kxlbd, kxubd, kx0sp, xbits, nbspc, &
       &  mbits, bzero, bmiss, binf,  bdnm,  &
       &  ixtop, ixbtm, kxmin, kxmax, kxone, lfrd)
    use TOUZA_Trp_std,only: first_bit, condop
    implicit none
    integer,intent(inout) :: kxbgn, kxdnm, kxlbd, kxubd, kx0sp ! exponents native
    integer,intent(inout) :: xbits
    integer,intent(out)   :: nbspc         ! number of bits required for specials
    integer,intent(in)    :: mbits
    logical,intent(in)    :: bzero, bmiss, binf, bdnm
    integer,intent(in)    :: ixtop, ixbtm  ! exponents upper/lower limit (relative to exp(1))
    integer,intent(in)    :: kxmin, kxmax, kxone  ! exponents minimum/maximum/1 native
    integer,intent(in)    :: lfrd
    ! integer j
    ! integer nreq, ntop, nbtm
    ! integer itmp
    ! integer xrtop, xrbtm            ! xtop, xbtm in terms of set_exponent value

    integer,parameter :: khuge = HUGE(ixtop)
    integer kxtop, kxbtm, kxd, kxl, kxu, kxend
    integer nbx
    integer nxf,   nxw
    integer nrem,  naddt, naddb

    kxtop = ixtop
    kxbtm = ixbtm
    if (kxtop.lt.+khuge) kxtop = kxtop + kxone
    if (kxbtm.ge.-khuge) kxbtm = kxbtm + kxone

    kxu = min(max(kxtop, kxlbd), kxubd, kxmax)
    kxl = min(max(kxbtm, kxlbd), kxu)
    kxd = kxdnm

    ! write(*, *) 'abarcar2 sw ', bzero, bmiss, binf,  bdnm
    ! write(*, *) 'abarcar2 cfg', kxone, kxbtm, kxtop, mbits
    ! write(*, *) 'abarcar2 adj', kxd,   kxl,   kxu
    ! write(*, *) 'abarcar2 in ', kxdnm, kxlbd, kxubd, xbits

       ! null        full range/keep top
       ! ixtop       keep top
       ! ixbtm       keep bottom
       ! ixbtm ixtop full range
    if (xbits.eq.0) then
       naddt = 0
       naddb = 0
       kxend = min(kxu, kxubd)
       kxbgn = max(kxl, kxlbd)
    else
       naddt = condop((binf.or.bmiss), 1, 0)
       naddb = condop(bdnm, 1, 0)

       kxend = min(kxu + 1, kxubd + naddt)
       kxbgn = max(kxl - 1, kxlbd - naddb)
       naddt = kxend - kxu
       naddb = kxl - kxbgn
    endif
    nxf = kxend - kxbgn + 1
    nbx = first_bit(nxf - 1) + 1
    nxw = 2 ** nbx
    nrem = (nxw - nxf)
    if (xbits.lt.0) then
       ! automatic exponent range
       if (bzero) then
          if (naddt.gt.0) then
             continue
          else if (nrem.gt.0) then
             kxend = kxend + 1
             nrem = nrem - 1
          else if (naddb.gt.0) then
             continue
          else
             kxend = kxend + 1
             nbx = nbx + 1
             nxf = nxf + 1
             nxw = nxw * 2
             nrem = (nxw - nxf)
          endif
       endif
       xbits = nbx
       kxl   = kxl - nrem
       kxbgn = kxbgn - nrem
       kx0sp = condop((kxend.gt.kxu), kxend, kxbgn)
       kxd   = condop((kxl.le.kxlbd), min(kxdnm, kxbgn), kxl - 1)
    else
       ! write(*,*) 'abarcar2 ini', kxbgn, kxend, kxl, kxu
       if (bzero) then
          if (naddt+naddb.gt.0) then
             ! 0 special already reserved
             continue
          else if (xbits.gt.nbx &
               & .or. (xbits.eq.nbx.and.nrem.gt.0)) then
             kxend = kxend + 1
             nxf = nxf + 1
             nbx = first_bit(nxf - 1) + 1
             nxw = 2 ** nbx
             nrem = (nxw - nxf)
          endif
       endif
       nbx = min(xbits, nbx)
       kxbgn = kxend - (2 ** nbx - 1)
       if (kxbgn.gt.kxlbd) then
          kxl = kxbgn + 1
       else if (bdnm) then
          kxl   = kxbgn + 1
       else if (bzero.and.(kxu.eq.kxend)) then
          kxl   = kxbgn + 1
       else
          kxl   = kxbgn
       endif
       kxd   = condop((kxl.le.kxlbd), min(kxdnm, kxbgn), kxl - 1)
       xbits = nbx
       kx0sp = condop((kxend.gt.kxu), kxend, kxbgn)
    endif

! 101 format('abarcar2 out: ', &
!          & I0, 1x, I0, ':', I0, '  /  ', &
!          & I0, 1x, I0, ':', I0, 1x, I0, ':', I0, 2x, I0, 1x, '[', I0, ']')
!     write(*, 101) kxdnm, kxlbd, kxubd, &
!          & kxd, kxl, kxu, kxbgn, kxend, kx0sp, xbits
    nbspc = -1
    if (bzero) nbspc = max(nbspc, KX_ZERO)
    if (bmiss) nbspc = max(nbspc, KX_MISS)
    if (binf)  nbspc = max(nbspc, KX_INF)
    if (kxubd.lt.kxu) nbspc = max(nbspc, KX_OVFL)
    ! todo: check underflow
    if (nbspc.lt.0) then
       nbspc = 0
    else
       nbspc = max(0, first_bit(nbspc) + 1)
    endif

    kxlbd = kxl
    kxubd = kxu
    kxdnm = max(kxd, kxmin - lfrd * 4)

    return
  end subroutine abarcar

!!!_  & reajustar_full - compressor (adjust again, no sign bit)
  subroutine reajustar_full_i &
       & (ierr,   ibagaz, &
       &  iwx,    iwh,    iwl,    &
       &  mem,    &
       &  nbitsh, nbitsl, xbits, &
       &  kxbgn,  kxdnm,  kxlbd,  kxubd,  kx0sp, kxspc, &
       &  kcode)
    use TOUZA_Trp_std,only: condop
    implicit none
    integer,parameter :: KIBGZ=KI32

    integer,            intent(out)   :: ierr
    integer(kind=KIBGZ),intent(out)   :: ibagaz(0:*)
    integer(kind=KIBGZ),intent(inout) :: iwx(0:*)
    integer(kind=KIBGZ),intent(inout) :: iwh(0:*)
    integer(kind=KIBGZ),intent(inout) :: iwl(0:*)
    integer,            intent(in)    :: mem
    integer,            intent(in)    :: nbitsh, nbitsl, xbits    ! nbits[hl]: original width
    integer,            intent(in)    :: kxbgn,  kxdnm,  kxlbd,  kxubd,  kx0sp, kxspc
    integer,            intent(in)    :: kcode        ! encoding scheme

    integer(kind=KIBGZ),parameter :: kzero = 0_KIBGZ

    integer j
    integer kxufl, kxdnh
    integer jx0sp, jxspc, jxoff, jxlbd, jxdnm
    integer nsh
    integer mbits
    integer mh_ovf, mh_unf, mh_miss, mh_inf
    integer ml_ovf, ml_unf, ml_miss, ml_inf
    integer(kind=KIBGZ) :: msp

    integer maskl, maskh
    integer,parameter :: nbmv = 1   ! other parameter never works

    ierr = 0

    mbits = nbitsh + nbitsl
    maskl = IBSET(0, nbitsl - nbmv) - 1
    maskh = IBITS(NOT(kzero), 0, nbitsh)

    kxufl = kxdnm - mbits
    kxdnh = kxdnm - nbitsh
    if (xbits.eq.0) then
       kxufl = -HUGE(kzero)
       kxdnh = -HUGE(kzero)
    endif
    jxlbd = 0
    ! if (kxlbd.ne.kxdnm) jxlbd = 1
    if (kxbgn.ne.kxlbd) jxlbd = 1
    jxoff = - kxlbd + jxlbd
    jx0sp = jxoff + kx0sp
    jxspc = jxoff + kxubd + 1
    jxdnm = 0

    if (xbits.eq.0) then
       mh_unf  = 0
       mh_ovf  = IBITS(NOT(kzero), 0, nbitsh+nbmv)
       mh_miss = mh_ovf
       mh_inf  = mh_ovf
       ml_unf  = 0
       ml_ovf  = IBITS(NOT(kzero), 0, nbitsl-nbmv)
       ml_miss = ml_ovf
       ml_inf  = ml_ovf
    else
       mh_unf  = IBITS(KX_UNFL, 0, nbitsh)
       mh_ovf  = IBITS(KX_OVFL, 0, nbitsh)
       mh_miss = IBITS(KX_MISS, 0, nbitsh)
       mh_inf  = IBITS(KX_INF,  0, nbitsh)
       ml_unf  = 0
       ml_ovf  = 0
       ml_miss = 0
       ml_inf  = 0
    endif

    do j = 0, mem - 1
       if (iwx(j).eq.kxspc) then
          msp = IAND(iwh(j), maskh)
          if (msp.eq.KX_ZERO) then
             iwh(j) = KX_ZERO
             iwl(j) = 0
             iwx(j) = jx0sp
          else if (msp.eq.KX_INF) then
             iwh(j) = mh_inf
             iwl(j) = ml_inf
             iwx(j) = jxspc
          else
             iwh(j) = mh_miss
             iwl(j) = ml_miss
             iwx(j) = jxspc
          endif
       else if (iwx(j).le.kxufl) then
          ! if underflow
          iwh(j) = mh_unf
          iwl(j) = ml_unf
          iwx(j) = jxspc
       else if (iwx(j).gt.kxubd) then
          ! if overflow
          iwh(j) = mh_ovf
          iwl(j) = ml_ovf
          iwx(j) = jxspc
       else if (iwx(j).le.kxdnh-1) then
          ! if denormalized (top bit on l)
          nsh = kxdnm - iwx(j) + 1
          iwl(j) = ISHFT(IBSET(iwh(j), nbitsh), nbitsl - nsh)
          iwh(j) = 0
          iwx(j) = jxdnm
       else if (iwx(j).lt.kxlbd) then
          ! if denormalized (top bit on h)
          nsh = kxdnm - iwx(j) + 1
          iwl(j) = IAND(maskl, IOR(ISHFT(iwl(j), -nsh), ISHFT(iwh(j), nbitsl - nsh)))
          iwh(j) = ISHFT(IBSET(iwh(j), nbitsh), -nsh + nbmv)
          iwx(j) = jxdnm
       else
          ! if normalized (hard-coded 1 bit transfer)
          iwh(j) = IOR(ISHFT(iwh(j), nbmv), IBITS(iwl(j), nbitsl-nbmv, nbmv))
          iwl(j) = IBCLR(iwl(j), nbitsl-nbmv)
          iwx(j) = iwx(j) + jxoff
       endif
    enddo

  end subroutine reajustar_full_i

!!!_  & ajustar_full - compressor
  subroutine ajustar_full_i &
       & (ierr,   ibagaz, &
       &  iwx,    iwh,    iwl,    &
       &  mem,    &
       &  ksignp, nbitsh, nbitsl, xbits, &
       &  kxbgn,  kxdnm,  kxlbd,  kxubd,  kx0sp, kxspc, &
       &  kcode)
    use TOUZA_Trp_std,only: condop
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KDBL

    integer,            intent(out)   :: ierr
    integer(kind=KIBGZ),intent(out)   :: ibagaz(0:*)
    integer(kind=KIBGZ),intent(inout) :: iwx(0:*)
    integer(kind=KIBGZ),intent(inout) :: iwh(0:*)
    integer(kind=KIBGZ),intent(inout) :: iwl(0:*)
    integer,            intent(in)    :: mem
    integer,            intent(in)    :: ksignp, nbitsh, nbitsl, xbits
    integer,            intent(in)    :: kxbgn,  kxdnm,  kxlbd,  kxubd,  kx0sp, kxspc
    integer,            intent(in)    :: kcode        ! encoding scheme

    integer(kind=KIBGZ),parameter :: kzero = 0_KIBGZ
    integer,parameter :: lbgz  = bit_size(kzero)

    integer j
    integer kxufl, kxdnh
    integer jx0sp, jxspc, jxoff, jxlbd, jxdnm
    integer nsh
    integer mbits
    integer mh_ovf, mh_unf, mh_miss, mh_inf
    integer ml_ovf, ml_unf, ml_miss, ml_inf

    integer mask_sign, maskl, maskh, maska
    integer(kind=KIBGZ) :: msp

    ierr = 0

    mbits = nbitsh + nbitsl
    mask_sign = condop((ksignp.eq.0), IBSET(0, nbitsh), 0)
    maskl = IBSET(0, nbitsl) - 1
    maskh = IBITS(NOT(kzero), 0, nbitsh)
    maska = IOR(maskh, mask_sign)

    kxufl = kxdnm - mbits
    kxdnh = kxdnm - nbitsh
    if (xbits.eq.0) then
       kxufl = -HUGE(kzero)
       kxdnh = -HUGE(kzero)
    endif
    jxlbd = 0
    if (kxbgn.ne.kxlbd) jxlbd = 1
    ! if (kxlbd.ne.kxdnm) jxlbd = 1
    jxoff = - kxlbd + jxlbd
    jx0sp = jxoff + kx0sp
    jxspc = jxoff + kxubd + 1
    jxdnm = 0

    if (xbits.eq.0) then
       mh_unf  = 0
       mh_ovf  = IBITS(NOT(kzero), 0, nbitsh)
       mh_ovf  = -1
       mh_miss = mh_ovf
       mh_inf  = mh_ovf
       ml_unf  = 0
       ml_ovf  = IBITS(NOT(kzero), 0, nbitsl)
       ml_miss = ml_ovf
       ml_inf  = ml_ovf
    else
       mh_unf  = IBITS(KX_UNFL, 0, nbitsh)
       mh_ovf  = IBITS(KX_OVFL, 0, nbitsh)
       mh_miss = IBITS(KX_MISS, 0, nbitsh)
       mh_inf  = IBITS(KX_INF,  0, nbitsh)
       ml_unf  = 0
       ml_ovf  = 0
       ml_miss = 0
       ml_inf  = 0
    endif

    do j = 0, mem - 1
       if (iwx(j).eq.kxspc) then
          msp = IAND(iwh(j), maskh)
          if (msp.eq.KX_ZERO) then
             iwh(j) = IOR(IAND(iwh(j), mask_sign), KX_ZERO)
             iwl(j) = 0
             iwx(j) = jx0sp
          else if (msp.eq.KX_INF) then
             iwh(j) = IOR(IAND(iwh(j), mask_sign), mh_inf)
             iwl(j) = ml_inf
             iwx(j) = jxspc
          else
             iwh(j) = IOR(IAND(iwh(j), mask_sign), mh_miss)
             iwl(j) = ml_miss
             iwx(j) = jxspc
          endif
       else if (iwx(j).le.kxufl) then
          ! if underflow
          iwh(j) = IOR(IAND(iwh(j), mask_sign), mh_unf)
          iwl(j) = ml_unf
          iwx(j) = jxspc
       else if (iwx(j).gt.kxubd) then
          ! if overflow
          iwh(j) = IOR(IAND(iwh(j), mask_sign), mh_ovf)
          iwl(j) = ml_ovf
          iwx(j) = jxspc
       else if (iwx(j).le.kxdnh) then
          ! if denormalized (top bit on l)
          nsh = kxdnm - iwx(j) + 1
          iwl(j) = ISHFT(IBSET(iwh(j), nbitsh), nbitsl - nsh)
          iwh(j) = IAND(iwh(j), mask_sign)
          iwx(j) = jxdnm
       else if (iwx(j).lt.kxlbd) then
          ! if denormalized (top bit on h)
          nsh = kxdnm - iwx(j) + 1
          iwl(j) = IAND(maskl, IOR(ISHFT(iwl(j), -nsh), ISHFT(iwh(j), nbitsl - nsh)))
          iwh(j) = IOR(IAND(iwh(j), mask_sign), ISHFT(IBSET(iwh(j), nbitsh), - nsh))
          iwx(j) = jxdnm
       else
          ! if normalized
          iwh(j) = IAND(iwh(j), maska)
          iwx(j) = iwx(j) + jxoff
       endif
    enddo

  end subroutine ajustar_full_i

!!!_  & ajustar_high - compressor
  subroutine ajustar_high_i &
       & (ierr,   ibagaz, &
       &  iwx,    iwh,    &
       &  mem,    &
       &  ksignp, nbitsh, xbits, &
       &  kxbgn,  kxdnm,  kxlbd,  kxubd, kx0sp, kxspc, &
       &  kcode)
    use TOUZA_Trp_std,only: condop
    implicit none
    integer,parameter :: KIBGZ=KI32

    integer,            intent(out)   :: ierr
    integer(kind=KIBGZ),intent(out)   :: ibagaz(0:*)
    integer(kind=KIBGZ),intent(inout) :: iwx(0:*)
    integer(kind=KIBGZ),intent(inout) :: iwh(0:*)
    integer,            intent(in)    :: mem
    integer,            intent(in)    :: ksignp, nbitsh, xbits
    integer,            intent(in)    :: kxbgn, kxdnm,   kxlbd,  kxubd,  kx0sp, kxspc
    integer,            intent(in)    :: kcode        ! encoding scheme

    integer(kind=KIBGZ),parameter :: kzero = 0_KIBGZ
    integer,parameter :: lbgz  = bit_size(kzero)

    integer j
    integer kxufl, kxdnh
    integer jx0sp, jxspc, jxoff, jxlbd, jxdnm
    integer nsh
    integer mbits
    integer mask_sign, maskh, maska
    integer mh_ovf, mh_unf, mh_miss, mh_inf
    integer(kind=KIBGZ) :: msp

    ierr = 0

    mbits = nbitsh + 0
    mask_sign = condop((ksignp.eq.0), IBSET(0, nbitsh), 0)
    maskh = IBITS(NOT(kzero), 0, nbitsh)
    maska = IOR(maskh, mask_sign)

    kxufl = kxdnm - mbits
    kxdnh = kxdnm - nbitsh
    if (xbits.eq.0) then
       kxufl = -HUGE(kzero)
       kxdnh = -HUGE(kzero)
    endif
    jxlbd = 0
    if (kxbgn.ne.kxlbd) jxlbd = 1
    ! if (kxlbd.ne.kxdnm) jxlbd = 1
    jxoff = - kxlbd + jxlbd
    jx0sp = jxoff + kx0sp
    jxspc = jxoff + kxubd + 1
    jxdnm = 0

    if (xbits.eq.0) then
       mh_unf  = 0
       mh_ovf  = IBITS(NOT(kzero), 0, nbitsh)
       mh_ovf  = -1
       mh_miss = mh_ovf
       mh_inf  = mh_ovf
    else
       mh_unf  = IBITS(KX_UNFL, 0, nbitsh)
       mh_ovf  = IBITS(KX_OVFL, 0, nbitsh)
       mh_miss = IBITS(KX_MISS, 0, nbitsh)
       mh_inf  = IBITS(KX_INF,  0, nbitsh)
    endif

    do j = 0, mem - 1
       if (iwx(j).eq.kxspc) then
          msp = IAND(iwh(j), maskh)
          if (msp.eq.KX_ZERO) then
             iwh(j) = IOR(IAND(iwh(j), mask_sign), KX_ZERO)
             iwx(j) = jx0sp
          else if (msp.eq.KX_INF) then
             iwh(j) = IOR(IAND(iwh(j), mask_sign), mh_inf)
             iwx(j) = jxspc
          else
             iwh(j) = IOR(IAND(iwh(j), mask_sign), mh_miss)
             iwx(j) = jxspc
          endif
       else if (iwx(j).le.kxufl) then
          ! if underflow
          iwh(j) = IOR(IAND(iwh(j), mask_sign), mh_unf)
          iwx(j) = jxspc
       else if (iwx(j).gt.kxubd) then
          ! if overflow
          iwh(j) = IOR(IAND(iwh(j), mask_sign), mh_ovf)
          iwx(j) = jxspc
       else if (iwx(j).le.kxdnh) then
          ! if denormalized (top bit on l)
          iwh(j) = IOR(IAND(iwh(j), mask_sign), mh_unf)
          iwx(j) = jxspc
       else if (iwx(j).lt.kxlbd) then
          ! if denormalized
          nsh = kxdnm - iwx(j) + 1
          iwh(j) = IOR(IAND(iwh(j), mask_sign), ISHFT(IBSET(iwh(j), nbitsh), - nsh))
          iwx(j) = jxdnm
       else
          ! if normalized
          iwh(j) = IAND(iwh(j), maska)
          iwx(j) = iwx(j) + jxoff
       endif
    enddo

  end subroutine ajustar_high_i

!!!_   & recortar - trimming(cliping)
  subroutine recortar_i &
       & (ierr,  moffs, nbeff, iwork, &
       &  ictlg, mem,   ixubd, ixufl, nbits)
    use TOUZA_Trp_std,only: first_bit, show_pattern
    implicit none
    integer,parameter :: KIBGZ=KI32
    integer,            intent(out)   :: ierr
    integer,            intent(out)   :: moffs, nbeff
    integer(kind=KIBGZ),intent(inout) :: iwork(0:*)   ! work array
    integer,            intent(in)    :: ictlg(0:*)
    integer,            intent(in)    :: mem
    integer,            intent(in)    :: ixubd, ixufl
    integer,            intent(in)    :: nbits

    integer(kind=KIBGZ) :: mpmax, mpmin, mnmax, mnmin
    integer,parameter :: lbgz = bit_size(0_KIBGZ)

    integer j
    integer(kind=KIBGZ) :: mhalf, mulim, mllim, mrange
    integer(kind=KIBGZ) :: mrh,   mrl

    ierr = 0
    moffs = 0
    mrange = -1
    nbeff = nbits
    ! return

    mllim = -HUGE(0_KIBGZ) - 1
    if (nbits.eq.lbgz) then
       mpmax = -1
       mulim = HUGE(0_KIBGZ)
       mpmin = mulim
       mnmax = mllim
       mnmin = 0
       do j = 0, mem - 1
          if (ictlg(j).le.ixubd.and.ictlg(j).ge.ixufl) then
             if (iwork(j).ge.0) then
                mpmax = MAX(mpmax, iwork(j))
                mpmin = MIN(mpmin, iwork(j))
             else
                mnmax = MAX(mnmax, iwork(j))
                mnmin = MIN(mnmin, iwork(j))
             endif
          endif
       enddo
       if (mpmax.lt.0) then
          if (mnmin.ge.0) then
             moffs = 0
             mrange = -1
          else
             moffs = mnmin
             mrange = mnmax - mnmin
          endif
       else if (mnmin.ge.0) then
          moffs = mpmin
          mrange = mpmax - mpmin
       else if (mpmin.eq.0.or.mnmax.eq.-1) then
          ! (mpmax - mnmin) + 1 <= mulim + 1
          ! - mnmin <= mulim - mpmax
          if (mnmin .ge. mpmax - mulim) then
             moffs = mnmin
             mrange = mpmax - mnmin
          else
             moffs = 0
             mrange = -1
          endif
       else
          ! (mpmin - 0 + 1) + (-1 - mnmax + 1) >= mulim + 1
          !  mpmin + (-mnmax) >= mulim
          if (mpmin.ge.mnmax+mulim) then
             moffs = mpmin
             mrange = (mulim - mpmin + 1) + (mnmax - mllim + 1) - 1
          else if (mpmax.le.mnmin+mulim) then
          ! (mpmax - mnmin + 1) <= mulim + 1
             moffs = mnmin
             mrange = mpmax - mnmin
          else
             moffs = 0
             mrange = -1
          endif
       endif
       if (moffs.gt.0) then
          if (_PREFER_DOIF) then
             do j = 0, mem - 1
                if (ictlg(j).le.ixubd.and.ictlg(j).ge.ixufl) then
                   if (iwork(j).ge.0) then
                      iwork(j) = iwork(j) - moffs
                   else
                      iwork(j) = (iwork(j) - mllim + 1) + mulim - moffs
                   endif
                endif
             enddo
          else
             where(ictlg(0:mem-1).le.ixubd.and.ictlg(0:mem-1).ge.ixufl)
                where(iwork(0:mem-1).ge.0)
                   iwork(0:mem-1) = iwork(0:mem-1) - moffs
                elsewhere
                   iwork(0:mem-1) = (iwork(0:mem-1) - mllim + 1) + mulim - moffs
                end where
             end where
          endif
       else if (moffs.lt.0) then
          if (_PREFER_DOIF) then
             do j = 0, mem - 1
                if (ictlg(j).le.ixubd.and.ictlg(j).ge.ixufl) then
                   iwork(j) = iwork(j) - moffs
                endif
             enddo
          else
             where(ictlg(0:mem-1).le.ixubd.and.ictlg(0:mem-1).ge.ixufl)
                iwork(0:mem-1) = iwork(0:mem-1) - moffs
             end where
          endif
       endif
    else
       mulim = 2_KIBGZ ** nbits
       mhalf = 2_KIBGZ ** (nbits - 1)
       mpmax = -1
       mpmin = +HUGE(0_KIBGZ)
       mnmax = -1
       mnmin = +HUGE(0_KIBGZ)
       do j = 0, mem - 1
          if (ictlg(j).le.ixubd.and.ictlg(j).ge.ixufl) then
             if (iwork(j).lt.mhalf) then
                mpmax = MAX(mpmax, iwork(j))
                mpmin = MIN(mpmin, iwork(j))
             else
                mnmax = MAX(mnmax, iwork(j))
                mnmin = MIN(mnmin, iwork(j))
             endif
          endif
       enddo
       if (mpmax.lt.0) then
          if (mnmax.lt.0) then
             moffs = 0
             mrange = -1
          else
             moffs = mnmin
             mrange = mnmax - mnmin
          endif
       else if (mnmax.lt.0) then
          moffs = mpmin
          mrange = mpmax - mpmin
       else
          mrh = mulim + mpmax - mnmin
          mrl = mnmax - mpmin
          if (mrh.lt.mrl) then
             moffs = mnmin
             mrange = mrh
          else
             moffs = mpmin
             mrange = mrl
          endif
       endif
       if (_PREFER_DOIF) then
          do j = 0, mem - 1
             if (ictlg(j).le.ixubd.and.ictlg(j).ge.ixufl) then
                iwork(j) = iwork(j) - moffs
             endif
          enddo
       else
          where(ictlg(0:mem-1).le.ixubd.and.ictlg(0:mem-1).ge.ixufl)
             iwork(0:mem-1) = iwork(0:mem-1) - moffs
          end where
       endif
    endif
    if (mrange.lt.0) then
       nbeff = nbits
    else if (mrange.eq.0) then
       nbeff = 0
    else
       nbeff = max(first_bit(mrange) + 1, 1)
    endif
    ! write(*, *) 'TRIM:', nbits, mrange, moffs, nbeff
    ! call show_pattern(ierr, 'TRIM/PH', mpmax, W=nbits)
    ! call show_pattern(ierr, 'TRIM/PL', mpmin, W=nbits)
    ! call show_pattern(ierr, 'TRIM/NH', mnmax, W=nbits)
    ! call show_pattern(ierr, 'TRIM/NL', mnmin, W=nbits)
    ! ierr = 0
    return
  end subroutine recortar_i

!!!_  & diluir_di - diluter(decompressor) using fp manipulation functions
  subroutine diluir_di &
       & (ierr,   vdst,  ictlg, iwork, &
       &  ibagaz, mem,   vskp,  kcode)
    use TOUZA_Trp_std, only: choice, show_pattern
    use TOUZA_Trp_pack,only: pack_restore, count_packed
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KDBL

    integer,            intent(out)         :: ierr
    real   (kind=KRFLD),intent(out)         :: vdst(0:*)
    integer(kind=KIBGZ),intent(out)         :: ictlg(0:*)
    integer(kind=KIBGZ),intent(out)         :: iwork(0:*)
    integer(kind=KIBGZ),intent(in)          :: ibagaz(0:*)
    integer,            intent(in)          :: mem
    real(kind=KRFLD),   intent(in),optional :: vskp
    integer,            intent(in),optional :: kcode

    real(kind=KRFLD),   parameter :: one   = 1.0_KRFLD
    real(kind=KRFLD),   parameter :: zero  = 0.0_KRFLD
    integer(kind=KIBGZ),parameter :: kstkh = 0_KIBGZ
    integer,            parameter :: ixone = EXPONENT(one)

    integer,parameter :: lbgz = BIT_SIZE(kstkh)
    integer,parameter :: lfrc = DIGITS(zero) - 1

    integer ebitsx, kpackx
    integer nbitsh, ebitsh, kpackh, ksign, kzsign
    integer nbitsl, ebitsl, kpackl

    integer(kind=KIBGZ) :: moffsh, mmskh
    integer(kind=KIBGZ) :: moffsl, mmskl

    integer ixbase
    integer nbitsa
    integer nbgz,   ncnz

    integer mbufx,  mbufh,  mbufl
    integer xbits,  mbits

    integer(kind=KIBGZ),parameter :: mplim = HUGE(kstkh)
    integer(kind=KIBGZ),parameter :: mllim = - HUGE(kstkh) - 1

    integer ixdnm, ixlbd,  ixubd
    integer kxspc, kxdnm
    integer jbbgn, jbend
    real(kind=KRFLD) :: vmsk, vrst, vsign
    real(kind=KRFLD) :: vu, vnan(0:1), vinf(0:1), vovf(0:1), vudf(0:1), vdnm(0:1)

    real(kind=KRFLD) :: zz, zm, zu, zo, zi, zd

    integer j

    ierr = 0
    vu = choice(-HUGE(vu), vskp)

    if (ierr.eq.0) call retrieve_basics(ierr, ibagaz, ixdnm, ixlbd, ixubd, ncnz, nbgz)
    if (ierr.eq.0) then
       if (ncnz.gt.mem) ierr = -1
    endif
    if (ierr.eq.0) call retrieve_bitprops(ierr, ibagaz, KB_XBIT, xbits,  ebitsx, kpackx)
    if (ierr.eq.0) call retrieve_bitprops(ierr, ibagaz, KB_HBIT, nbitsh, ebitsh, kpackh, ksign)
    if (ierr.eq.0) call retrieve_bitprops(ierr, ibagaz, KB_LBIT, nbitsl, ebitsl, kpackl)

    if (ierr.eq.0) call retrieve_masks(ierr, ibagaz, moffsh, mmskh, moffsl, mmskl)

    ! if (ierr.eq.0) then
    !    write (*, *) 'basic', ixdnm, ixlbd, ixubd, ncnz, nbgz
    !    write (*, *) 'bit/x', xbits,  ebitsx, kpackx
    !    write (*, *) 'bit/h', nbitsh, ebitsh, kpackh, ksign
    !    write (*, *) 'bit/l', nbitsl, ebitsl, kpackl
    !    write (*, *) 'mask',  moffsh, mmskh, moffsl, mmskl
    ! endif

    if (ierr.eq.0) then
       mbits  = nbitsh + nbitsl
       nbitsa = count_msbits(ebitsh, ksign)

       mbufx  = count_packed(xbits,  ncnz, kstkh)
       mbufh  = count_packed(nbitsa, ncnz, kstkh)
       mbufl  = count_packed(ebitsl, ncnz, kstkh)

       call batch_specials &
            & (vnan,  vinf,  vdnm,  vovf,  vudf,  &
            &  mbits, ixdnm, ixlbd, ixubd, vu)

       ixdnm  = ixdnm + ixone
       ixlbd  = ixlbd + ixone
       ixubd  = ixubd + ixone

       kxdnm  = -1
       ixbase = ixlbd
       if (ixlbd.ne.ixdnm) then
          ixbase = ixbase - 1
          kxdnm = 0
       endif
       kxspc = ixubd - ixbase + 1
       ! special case when xbits == 0
       ! if (xbits.eq.0) then
       !    kxspc = kxdnm + 1
       !    ixdnm = ixdnm - 1
       ! endif
       ! write(*, *) 'XSPC', kxspc, kxdnm, xbits

       ! restore exponents
       jbbgn = KB_HEAD
       jbend = jbbgn + mbufx
       kpackx = suggest_filling(xbits, ncnz, kcode, kpackx)
       if (xbits.gt.0) then
          call pack_restore &
               & (ierr, ictlg, ibagaz(jbbgn:jbend-1), ncnz, xbits, kpackx)
       else
          ictlg(0:ncnz-1) = 0
       endif

       ! set special values
       zz = zspecial(KX_ZERO, nbitsh, moffsh, mmskh, nbitsl, moffsl, mmskl, one, ixone)
       zm = zspecial(KX_MISS, nbitsh, moffsh, mmskh, nbitsl, moffsl, mmskl, one, ixone)
       zu = zspecial(KX_UNFL, nbitsh, moffsh, mmskh, nbitsl, moffsl, mmskl, one, ixone)
       zo = zspecial(KX_OVFL, nbitsh, moffsh, mmskh, nbitsl, moffsl, mmskl, one, ixone)
       zi = zspecial(KX_INF,  nbitsh, moffsh, mmskh, nbitsl, moffsl, mmskl, one, ixone)
       zd = zspecial(KX_DNM,  nbitsh, moffsh, mmskh, nbitsl, moffsl, mmskl, one, ixone)

       ! restore higher mantissa
       jbbgn = KB_HEAD + mbufx
       jbend = jbbgn + mbufh
       kpackh = suggest_filling(nbitsa, ncnz, kcode, kpackh)
       call pack_restore &
            & (ierr, iwork, ibagaz(jbbgn:jbend-1), ncnz, nbitsa, kpackh)
       vmsk = _SET_EXPONENT(one, nbitsh + ixone)
       if (ksign.eq.0) then
          iwork(0:ncnz-1) = IOR(ISHFT(IBITS(iwork(0:ncnz-1), ebitsh, 1), nbitsh), &
               &               IOR(IBITS(IBITS(iwork(0:ncnz-1), 0, ebitsh) + moffsh, 0, nbitsh), mmskh))
          vdst(0:ncnz-1) = &
               & _SET_EXPONENT(vmsk + real(ibits(iwork(0:ncnz-1), 0, nbitsh), kind=KRFLD), &
               &              mbits + ixone)
          vsign = - one
          if (_PREFER_DOIF) then
             do j = 0, ncnz - 1
                if (btest(iwork(j), nbitsh)) then
                   vdst(j) = sign(vdst(j), vsign)
                endif
             enddo
          else
             where (btest(iwork(0:ncnz-1), nbitsh))
                vdst(0:ncnz-1) = sign(vdst(0:ncnz-1), vsign)
             end where
          endif
       else if (nbitsh.eq.lbgz) then
          vsign = real(ksign, kind=KRFLD)
          if (moffsh.lt.0) then
             iwork(0:ncnz-1) = IOR(iwork(0:ncnz-1) + moffsh, mmskh)
          else if (moffsh.gt.0) then
             if (_PREFER_DOIF) then
                do j = 0, ncnz - 1
                   if (iwork(j).le.mplim-moffsh) then
                      iwork(j) = iwork(j) + moffsh
                   else
                      iwork(j) = (iwork(j) - (mplim - moffsh) - 1 + mllim)
                   endif
                enddo
             else
                where(iwork(0:ncnz-1).le.mplim-moffsh)
                   iwork(0:ncnz-1) = iwork(0:ncnz-1) + moffsh
                elsewhere
                   iwork(0:ncnz-1) = (iwork(0:ncnz-1) - (mplim - moffsh) - 1 + mllim)
                end where
             endif
          endif
          vdst(0:ncnz-1) = &
               & _SET_EXPONENT(vmsk + xureal(iwork(0:ncnz-1), one), &
               &               mbits + ixone)
          vdst(0:ncnz-1) = sign(vdst(0:ncnz-1), vsign)
       else
          iwork(0:ncnz-1) = IOR(IBITS(iwork(0:ncnz-1) + moffsh, 0, nbitsh), mmskh)
          vsign = real(ksign, kind=KRFLD)
          vdst(0:ncnz-1) = &
               & _SET_EXPONENT(vmsk + real(iwork(0:ncnz-1), kind=KRFLD), &
               &               mbits + ixone)
          vdst(0:ncnz-1) = sign(vdst(0:ncnz-1), vsign)
       endif
       ! restore lower mantissa
       if (nbitsl.gt.0) then
          jbbgn = KB_HEAD + mbufx + mbufh
          jbend = jbbgn + mbufl
          kpackl = suggest_filling(ebitsl, ncnz, kcode, kpackl)
          call pack_restore &
               & (ierr, iwork, ibagaz(jbbgn:jbend-1), ncnz, ebitsl, kpackl)
          iwork(0:ncnz-1) = IOR(IBITS(iwork(0:ncnz-1) + moffsl, 0, nbitsl), mmskl)
          vdst(0:ncnz-1) = sign(abs(vdst(0:ncnz-1)) + real(iwork(0:ncnz-1), kind=KRFLD), &
               &               vdst(0:ncnz-1))
       endif

       ! call show_pattern_float(ierr, 'dilute/zz', zz)
       ! call show_pattern_float(ierr, 'dilute/zm', zm)
       ! call show_pattern_float(ierr, 'dilute/zu', zu)
       ! call show_pattern_float(ierr, 'dilute/zo', zo)
       ! call show_pattern_float(ierr, 'dilute/zi', zi)
       ! call show_pattern_float(ierr, 'dilute/zd', zd)
       ! do j = 0, ncnz - 1
       !    if (ictlg(j).ge.kxspc.or.ictlg(j).eq.kxdnm) then
       !       call show_pattern_float(ierr, 'dilute/ds', vdst(j))
       !    endif
       ! enddo

       kzsign = +one
       if (ksign.eq.KF_NEG_PZERO.or.ksign.eq.KF_POS_MZERO) kzsign = -one

       vmsk = _SET_EXPONENT(one, mbits + ixone)
       vrst = _SET_EXPONENT(one, ixdnm + 1)
       if (_PREFER_DOIF) then
          do j = 0, ncnz - 1
             if (ictlg(j).ge.kxspc) then
                if     (abs(vdst(j)).eq.zz) then
                   vdst(j) = sign(zero, sign(one, vdst(j)) * kzsign)
                else if (abs(vdst(j)).eq.zm) then
                   vdst(j) = vu
                else if (abs(vdst(j)).eq.zu) then
                   vdst(j) = vudf(which_sp(vdst(j)))
                else if (abs(vdst(j)).eq.zo) then
                   vdst(j) = vovf(which_sp(vdst(j)))
                else if (abs(vdst(j)).eq.zi) then
                   vdst(j) = vinf(which_sp(vdst(j)))
                else if (abs(vdst(j)).eq.zd) then
                   vdst(j) = vdnm(which_sp(vdst(j)))
                else
                   vdst(j) = vnan(which_sp(vdst(j)))
                endif
             else if (ictlg(j).eq.kxdnm) then
                if     (abs(vdst(j)).eq.zz) then
                   vdst(j) = sign(zero, sign(one, vdst(j)) * kzsign)
                else
                   vdst(j) = &
                        & sign(_SET_EXPONENT(abs(vdst(j)), ixdnm + 1) - vrst, &
                        &      vdst(j))
                endif
             else
                vdst(j) = _SET_EXPONENT(vdst(j), ixbase + ictlg(j))
             endif
          enddo
       else
          where (ictlg(0:ncnz-1).ge.kxspc)
             where     (abs(vdst(0:ncnz-1)).eq.zz)
                vdst(0:ncnz-1) = sign(zero, sign(one, vdst(0:ncnz-1)) * kzsign)
             elsewhere (abs(vdst(0:ncnz-1)).eq.zm)
                vdst(0:ncnz-1) = vu
             elsewhere (abs(vdst(0:ncnz-1)).eq.zu)
                vdst(0:ncnz-1) = vudf(which_sp(vdst(0:ncnz-1)))
             elsewhere (abs(vdst(0:ncnz-1)).eq.zo)
                vdst(0:ncnz-1) = vovf(which_sp(vdst(0:ncnz-1)))
             elsewhere (abs(vdst(0:ncnz-1)).eq.zi)
                vdst(0:ncnz-1) = vinf(which_sp(vdst(0:ncnz-1)))
             elsewhere (abs(vdst(0:ncnz-1)).eq.zd)
                vdst(0:ncnz-1) = vdnm(which_sp(vdst(0:ncnz-1)))
             elsewhere
                vdst(0:ncnz-1) = vnan(which_sp(vdst(0:ncnz-1)))
             end where
          elsewhere (ictlg(0:ncnz-1).eq.kxdnm)
             where     (abs(vdst(0:ncnz-1)).eq.zz)
                vdst(0:ncnz-1) = sign(zero, sign(one, vdst(0:ncnz-1)) * kzsign)
             elsewhere
                vdst(0:ncnz-1) = &
                     & sign(_SET_EXPONENT(abs(vdst(0:ncnz-1)), ixdnm + 1) - vrst, &
                     &      vdst(0:ncnz-1))
             end where
          elsewhere
             vdst(0:ncnz-1) = _SET_EXPONENT(vdst(0:ncnz-1), ixbase + ictlg(0:ncnz-1))
          end where
       endif
    endif
    return
  end subroutine diluir_di
!!!_  & diluir_fi - diluter(decompressor) using fp manipulation functions
  subroutine diluir_fi &
       & (ierr,   vdst,  ictlg, iwork, &
       &  ibagaz, mem,   vskp,  kcode)
    use TOUZA_Trp_std, only: choice, show_pattern
    use TOUZA_Trp_pack,only: pack_restore, count_packed
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KFLT

    integer,            intent(out)         :: ierr
    real   (kind=KRFLD),intent(out)         :: vdst(0:*)
    integer(kind=KIBGZ),intent(out)         :: ictlg(0:*)
    integer(kind=KIBGZ),intent(out)         :: iwork(0:*)
    integer(kind=KIBGZ),intent(in)          :: ibagaz(0:*)
    integer,            intent(in)          :: mem
    real(kind=KRFLD),   intent(in),optional :: vskp
    integer,            intent(in),optional :: kcode

    real(kind=KRFLD),   parameter :: one   = 1.0_KRFLD
    real(kind=KRFLD),   parameter :: zero  = 0.0_KRFLD
    integer(kind=KIBGZ),parameter :: kstkh = 0_KIBGZ
    integer,            parameter :: ixone = EXPONENT(one)

    integer,parameter :: lbgz = BIT_SIZE(kstkh)
    integer,parameter :: lfrc = DIGITS(zero) - 1

    integer ebitsx, kpackx
    integer nbitsh, ebitsh, kpackh, ksign, kzsign
    integer nbitsl, ebitsl, kpackl

    integer(kind=KIBGZ) :: moffsh, mmskh
    integer(kind=KIBGZ) :: moffsl, mmskl

    integer ixbase
    integer nbitsa
    integer nbgz,   ncnz

    integer mbufx,  mbufh,  mbufl
    integer xbits,  mbits

    integer(kind=KIBGZ),parameter :: mplim = HUGE(kstkh)
    integer(kind=KIBGZ),parameter :: mllim = - HUGE(kstkh) - 1

    integer ixdnm, ixlbd,  ixubd
    integer kxspc, kxdnm
    integer jbbgn, jbend
    real(kind=KRFLD) :: vmsk, vrst, vsign
    real(kind=KRFLD) :: vu, vnan(0:1), vinf(0:1), vovf(0:1), vudf(0:1), vdnm(0:1)

    real(kind=KRFLD) :: zz, zm, zu, zo, zi, zd

    integer j

    ierr = 0
    vu = choice(-HUGE(vu), vskp)

    if (ierr.eq.0) call retrieve_basics(ierr, ibagaz, ixdnm, ixlbd, ixubd, ncnz, nbgz)
    if (ierr.eq.0) then
       if (ncnz.gt.mem) ierr = -1
    endif
    if (ierr.eq.0) call retrieve_bitprops(ierr, ibagaz, KB_XBIT, xbits,  ebitsx, kpackx)
    if (ierr.eq.0) call retrieve_bitprops(ierr, ibagaz, KB_HBIT, nbitsh, ebitsh, kpackh, ksign)
    if (ierr.eq.0) call retrieve_bitprops(ierr, ibagaz, KB_LBIT, nbitsl, ebitsl, kpackl)

    if (ierr.eq.0) call retrieve_masks(ierr, ibagaz, moffsh, mmskh, moffsl, mmskl)

    ! if (ierr.eq.0) then
    !    write (*, *) 'basic', ixdnm, ixlbd, ixubd, ncnz, nbgz
    !    write (*, *) 'bit/x', xbits,  ebitsx, kpackx
    !    write (*, *) 'bit/h', nbitsh, ebitsh, kpackh, ksign
    !    write (*, *) 'bit/l', nbitsl, ebitsl, kpackl
    !    write (*, *) 'mask',  moffsh, mmskh, moffsl, mmskl
    ! endif

    if (ierr.eq.0) then
       mbits  = nbitsh + nbitsl
       nbitsa = count_msbits(ebitsh, ksign)

       mbufx  = count_packed(xbits,  ncnz, kstkh)
       mbufh  = count_packed(nbitsa, ncnz, kstkh)
       mbufl  = count_packed(ebitsl, ncnz, kstkh)

       call batch_specials &
            & (vnan,  vinf,  vdnm,  vovf,  vudf,  &
            &  mbits, ixdnm, ixlbd, ixubd, vu)

       ixdnm  = ixdnm + ixone
       ixlbd  = ixlbd + ixone
       ixubd  = ixubd + ixone

       kxdnm  = -1
       ixbase = ixlbd
       if (ixlbd.ne.ixdnm) then
          ixbase = ixbase - 1
          kxdnm = 0
       endif
       kxspc = ixubd - ixbase + 1
       ! special case when xbits == 0
       ! if (xbits.eq.0) then
       !    kxspc = kxdnm + 1
       !    ixdnm = ixdnm - 1
       ! endif
       ! write(*, *) 'XSPC', kxspc, kxdnm, xbits

       ! restore exponents
       jbbgn = KB_HEAD
       jbend = jbbgn + mbufx
       kpackx = suggest_filling(xbits, ncnz, kcode, kpackx)
       if (xbits.gt.0) then
          call pack_restore &
               & (ierr, ictlg, ibagaz(jbbgn:jbend-1), ncnz, xbits, kpackx)
       else
          ictlg(0:ncnz-1) = 0
       endif

       ! set special values
       zz = zspecial(KX_ZERO, nbitsh, moffsh, mmskh, nbitsl, moffsl, mmskl, one, ixone)
       zm = zspecial(KX_MISS, nbitsh, moffsh, mmskh, nbitsl, moffsl, mmskl, one, ixone)
       zu = zspecial(KX_UNFL, nbitsh, moffsh, mmskh, nbitsl, moffsl, mmskl, one, ixone)
       zo = zspecial(KX_OVFL, nbitsh, moffsh, mmskh, nbitsl, moffsl, mmskl, one, ixone)
       zi = zspecial(KX_INF,  nbitsh, moffsh, mmskh, nbitsl, moffsl, mmskl, one, ixone)
       zd = zspecial(KX_DNM,  nbitsh, moffsh, mmskh, nbitsl, moffsl, mmskl, one, ixone)

       ! restore higher mantissa
       jbbgn = KB_HEAD + mbufx
       jbend = jbbgn + mbufh
       kpackh = suggest_filling(nbitsa, ncnz, kcode, kpackh)
       call pack_restore &
            & (ierr, iwork, ibagaz(jbbgn:jbend-1), ncnz, nbitsa, kpackh)
       vmsk = _SET_EXPONENT(one, nbitsh + ixone)
       if (ksign.eq.0) then
          iwork(0:ncnz-1) = IOR(ISHFT(IBITS(iwork(0:ncnz-1), ebitsh, 1), nbitsh), &
               &               IOR(IBITS(IBITS(iwork(0:ncnz-1), 0, ebitsh) + moffsh, 0, nbitsh), mmskh))
          vdst(0:ncnz-1) = &
               & _SET_EXPONENT(vmsk + real(ibits(iwork(0:ncnz-1), 0, nbitsh), kind=KRFLD), &
               &              mbits + ixone)
          vsign = - one
          if (_PREFER_DOIF) then
             do j = 0, ncnz - 1
                if (btest(iwork(j), nbitsh)) then
                   vdst(j) = sign(vdst(j), vsign)
                endif
             enddo
          else
             where (btest(iwork(0:ncnz-1), nbitsh))
                vdst(0:ncnz-1) = sign(vdst(0:ncnz-1), vsign)
             end where
          endif
       else if (nbitsh.eq.lbgz) then
          vsign = real(ksign, kind=KRFLD)
          if (moffsh.lt.0) then
             iwork(0:ncnz-1) = IOR(iwork(0:ncnz-1) + moffsh, mmskh)
          else if (moffsh.gt.0) then
             if (_PREFER_DOIF) then
                do j = 0, ncnz - 1
                   if (iwork(j).le.mplim-moffsh) then
                      iwork(j) = iwork(j) + moffsh
                   else
                      iwork(j) = (iwork(j) - (mplim - moffsh) - 1 + mllim)
                   endif
                enddo
             else
                where(iwork(0:ncnz-1).le.mplim-moffsh)
                   iwork(0:ncnz-1) = iwork(0:ncnz-1) + moffsh
                elsewhere
                   iwork(0:ncnz-1) = (iwork(0:ncnz-1) - (mplim - moffsh) - 1 + mllim)
                end where
             endif
          endif
          vdst(0:ncnz-1) = &
               & _SET_EXPONENT(vmsk + xureal(iwork(0:ncnz-1), one), &
               &               mbits + ixone)
          vdst(0:ncnz-1) = sign(vdst(0:ncnz-1), vsign)
       else
          iwork(0:ncnz-1) = IOR(IBITS(iwork(0:ncnz-1) + moffsh, 0, nbitsh), mmskh)
          vsign = real(ksign, kind=KRFLD)
          vdst(0:ncnz-1) = &
               & _SET_EXPONENT(vmsk + real(iwork(0:ncnz-1), kind=KRFLD), &
               &               mbits + ixone)
          vdst(0:ncnz-1) = sign(vdst(0:ncnz-1), vsign)
       endif
       ! restore lower mantissa
       if (nbitsl.gt.0) then
          jbbgn = KB_HEAD + mbufx + mbufh
          jbend = jbbgn + mbufl
          kpackl = suggest_filling(ebitsl, ncnz, kcode, kpackl)
          call pack_restore &
               & (ierr, iwork, ibagaz(jbbgn:jbend-1), ncnz, ebitsl, kpackl)
          iwork(0:ncnz-1) = IOR(IBITS(iwork(0:ncnz-1) + moffsl, 0, nbitsl), mmskl)
          vdst(0:ncnz-1) = sign(abs(vdst(0:ncnz-1)) + real(iwork(0:ncnz-1), kind=KRFLD), &
               &               vdst(0:ncnz-1))
       endif

       ! call show_pattern_float(ierr, 'dilute/zz', zz)
       ! call show_pattern_float(ierr, 'dilute/zm', zm)
       ! call show_pattern_float(ierr, 'dilute/zu', zu)
       ! call show_pattern_float(ierr, 'dilute/zo', zo)
       ! call show_pattern_float(ierr, 'dilute/zi', zi)
       ! call show_pattern_float(ierr, 'dilute/zd', zd)
       ! do j = 0, ncnz - 1
       !    if (ictlg(j).ge.kxspc.or.ictlg(j).eq.kxdnm) then
       !       call show_pattern_float(ierr, 'dilute/ds', vdst(j))
       !    endif
       ! enddo

       kzsign = +one
       if (ksign.eq.KF_NEG_PZERO.or.ksign.eq.KF_POS_MZERO) kzsign = -one

       vmsk = _SET_EXPONENT(one, mbits + ixone)
       vrst = _SET_EXPONENT(one, ixdnm + 1)
       if (_PREFER_DOIF) then
          do j = 0, ncnz - 1
             if (ictlg(j).ge.kxspc) then
                if     (abs(vdst(j)).eq.zz) then
                   vdst(j) = sign(zero, sign(one, vdst(j)) * kzsign)
                else if (abs(vdst(j)).eq.zm) then
                   vdst(j) = vu
                else if (abs(vdst(j)).eq.zu) then
                   vdst(j) = vudf(which_sp(vdst(j)))
                else if (abs(vdst(j)).eq.zo) then
                   vdst(j) = vovf(which_sp(vdst(j)))
                else if (abs(vdst(j)).eq.zi) then
                   vdst(j) = vinf(which_sp(vdst(j)))
                else if (abs(vdst(j)).eq.zd) then
                   vdst(j) = vdnm(which_sp(vdst(j)))
                else
                   vdst(j) = vnan(which_sp(vdst(j)))
                endif
             else if (ictlg(j).eq.kxdnm) then
                if     (abs(vdst(j)).eq.zz) then
                   vdst(j) = sign(zero, sign(one, vdst(j)) * kzsign)
                else
                   vdst(j) = &
                        & sign(_SET_EXPONENT(abs(vdst(j)), ixdnm + 1) - vrst, &
                        &      vdst(j))
                endif
             else
                vdst(j) = _SET_EXPONENT(vdst(j), ixbase + ictlg(j))
             endif
          enddo
       else
          where (ictlg(0:ncnz-1).ge.kxspc)
             where     (abs(vdst(0:ncnz-1)).eq.zz)
                vdst(0:ncnz-1) = sign(zero, sign(one, vdst(0:ncnz-1)) * kzsign)
             elsewhere (abs(vdst(0:ncnz-1)).eq.zm)
                vdst(0:ncnz-1) = vu
             elsewhere (abs(vdst(0:ncnz-1)).eq.zu)
                vdst(0:ncnz-1) = vudf(which_sp(vdst(0:ncnz-1)))
             elsewhere (abs(vdst(0:ncnz-1)).eq.zo)
                vdst(0:ncnz-1) = vovf(which_sp(vdst(0:ncnz-1)))
             elsewhere (abs(vdst(0:ncnz-1)).eq.zi)
                vdst(0:ncnz-1) = vinf(which_sp(vdst(0:ncnz-1)))
             elsewhere (abs(vdst(0:ncnz-1)).eq.zd)
                vdst(0:ncnz-1) = vdnm(which_sp(vdst(0:ncnz-1)))
             elsewhere
                vdst(0:ncnz-1) = vnan(which_sp(vdst(0:ncnz-1)))
             end where
          elsewhere (ictlg(0:ncnz-1).eq.kxdnm)
             where     (abs(vdst(0:ncnz-1)).eq.zz)
                vdst(0:ncnz-1) = sign(zero, sign(one, vdst(0:ncnz-1)) * kzsign)
             elsewhere
                vdst(0:ncnz-1) = &
                     & sign(_SET_EXPONENT(abs(vdst(0:ncnz-1)), ixdnm + 1) - vrst, &
                     &      vdst(0:ncnz-1))
             end where
          elsewhere
             vdst(0:ncnz-1) = _SET_EXPONENT(vdst(0:ncnz-1), ixbase + ictlg(0:ncnz-1))
          end where
       endif
    endif
    return
  end subroutine diluir_fi

!!!_   & batch_specials
  subroutine batch_specials_d &
       & (vnan,  vinf,  vdnm,  vovf, vudf, &
       &  mbits, ixdnm, ixlbd, ixubd, vmiss)
    implicit none
    integer,parameter :: KRFLD=KDBL
    real(kind=KRFLD),intent(out) :: vnan(0:1), vinf(0:1), vdnm(0:1)
    real(kind=KRFLD),intent(out) :: vovf(0:1), vudf(0:1)
    integer,         intent(in)  :: mbits, ixdnm, ixlbd, ixubd
    real(kind=KRFLD),intent(in)  :: vmiss

    vnan(:) = set_special(KSCHM_NAN,  mbits, ixdnm, ixlbd, ixubd, vmiss)
    vinf(:) = set_special(KSCHM_INF,  mbits, ixdnm, ixlbd, ixubd, vmiss)
    vdnm(:) = set_special(KSCHM_DNM,  mbits, ixdnm, ixlbd, ixubd, vmiss)
    vovf(:) = set_special(KSCHM_OVFL, mbits, ixdnm, ixlbd, ixubd, vmiss)
    vudf(:) = set_special(KSCHM_UNFL, mbits, ixdnm, ixlbd, ixubd, vmiss)

    return
  end subroutine batch_specials_d
  subroutine batch_specials_f &
       & (vnan,  vinf,  vdnm,  vovf, vudf, &
       &  mbits, ixdnm, ixlbd, ixubd, vmiss)
    implicit none
    integer,parameter :: KRFLD=KFLT
    real(kind=KRFLD),intent(out) :: vnan(0:1), vinf(0:1), vdnm(0:1)
    real(kind=KRFLD),intent(out) :: vovf(0:1), vudf(0:1)
    integer,         intent(in)  :: mbits, ixdnm, ixlbd, ixubd
    real(kind=KRFLD),intent(in)  :: vmiss

    vnan(:) = set_special(KSCHM_NAN,  mbits, ixdnm, ixlbd, ixubd, vmiss)
    vinf(:) = set_special(KSCHM_INF,  mbits, ixdnm, ixlbd, ixubd, vmiss)
    vdnm(:) = set_special(KSCHM_DNM,  mbits, ixdnm, ixlbd, ixubd, vmiss)
    vovf(:) = set_special(KSCHM_OVFL, mbits, ixdnm, ixlbd, ixubd, vmiss)
    vudf(:) = set_special(KSCHM_UNFL, mbits, ixdnm, ixlbd, ixubd, vmiss)

    return
  end subroutine batch_specials_f

!!!_   & set_special
  PURE &
  function set_special_d &
       & (kschm, mbits, ixdnm, ixlbd, ixubd, vmiss) &
       & result (r)
#if HAVE_IEEE_ARITHMETIC
    use IEEE_ARITHMETIC
#endif
    implicit none
    integer,parameter :: KRFLD=KDBL
    real(kind=KRFLD)             :: r(0:1)
    integer,         intent(in)  :: kschm
    integer,         intent(in)  :: mbits, ixdnm, ixlbd, ixubd
    real(kind=KRFLD),intent(in)  :: vmiss

    real(kind=KRFLD),parameter :: one =  1.0_KRFLD
    real(kind=KRFLD),parameter :: sn  = -1.0_KRFLD
    real(kind=KRFLD) :: vpos
    integer,parameter :: ixone = exponent(one)
    integer jw
    integer ixd

    select case (kschm)
#   if HAVE_IEEE_ARITHMETIC
    case (KS_MISS)
       r(0:1) = vmiss
#   else /* not HAVE_IEEE_ARITHMETIC */
    case (KS_MISS,KS_NAN,KS_INF)
       r(0:1) = vmiss
#   endif
    case default
       select case (kschm)
       case (KS_ZERO)
          vpos = 0.0_KRFLD
#      if HAVE_IEEE_ARITHMETIC
       case (KS_NAN)
          vpos = IEEE_VALUE(vpos, IEEE_QUIET_NAN)
       case (KS_INF)
          vpos = IEEE_VALUE(vpos, IEEE_POSITIVE_INF)
       case (KS_DNM)
          vpos = IEEE_VALUE(vpos, IEEE_POSITIVE_DENORMAL)
#      endif
       case (KS_TINY)
          vpos = TINY(vmiss)
       case (KS_HUGE)
          vpos = HUGE(vmiss)
       case (KS_FLOOR)
          vpos = _SET_EXPONENT(1.0_KRFLD, ixlbd + ixone)
       case (KS_CEILING)
          vpos = _SET_EXPONENT(HUGE(vmiss), ixubd + ixone) ! (to do) mbits mask
       case (KS_UNDER)
          ixd = min(ixdnm, ixlbd - 1) - mbits + ixone
          if (ixd.ge.MINEXPONENT(vmiss)) then
             vpos = _SET_EXPONENT(HUGE(vmiss), ixd) ! (to do) mbits mask
          else
             jw = digits(vmiss) - 1 - (MINEXPONENT(vmiss) - ixd - 1)
             vpos = _SET_EXPONENT(AINT(_SET_EXPONENT(HUGE(vmiss), jw)), ixd)
          endif
       case (KS_ROOF)
          vpos = _SET_EXPONENT(1.0_KRFLD, ixubd+1 + ixone)
       case default
          vpos = 0.0_KRFLD
       end select
       r(0) = vpos
       r(1) = sign(vpos, sn)
    end select
    return
  end function set_special_d
  PURE &
  function set_special_f &
       & (kschm, mbits, ixdnm, ixlbd, ixubd, vmiss) &
       & result (r)
#if HAVE_IEEE_ARITHMETIC
    use IEEE_ARITHMETIC
#endif
    implicit none
    integer,parameter :: KRFLD=KFLT
    real(kind=KRFLD)             :: r(0:1)
    integer,         intent(in)  :: kschm
    integer,         intent(in)  :: mbits, ixdnm, ixlbd, ixubd
    real(kind=KRFLD),intent(in)  :: vmiss

    real(kind=KRFLD),parameter :: one =  1.0_KRFLD
    real(kind=KRFLD),parameter :: sn  = -1.0_KRFLD
    real(kind=KRFLD) :: vpos
    integer,parameter :: ixone = exponent(one)
    integer jw
    integer ixd

    select case (kschm)
#   if HAVE_IEEE_ARITHMETIC
    case (KS_MISS)
       r(0:1) = vmiss
#   else /* not HAVE_IEEE_ARITHMETIC */
    case (KS_MISS,KS_NAN,KS_INF)
       r(0:1) = vmiss
#   endif
    case default
       select case (kschm)
       case (KS_ZERO)
          vpos = 0.0_KRFLD
#      if HAVE_IEEE_ARITHMETIC
       case (KS_NAN)
          vpos = IEEE_VALUE(vpos, IEEE_QUIET_NAN)
       case (KS_INF)
          vpos = IEEE_VALUE(vpos, IEEE_POSITIVE_INF)
       case (KS_DNM)
          vpos = IEEE_VALUE(vpos, IEEE_POSITIVE_DENORMAL)
#      endif
       case (KS_TINY)
          vpos = TINY(vmiss)
       case (KS_HUGE)
          vpos = HUGE(vmiss)
       case (KS_FLOOR)
          vpos = _SET_EXPONENT(1.0_KRFLD, ixlbd + ixone)
       case (KS_CEILING)
          vpos = _SET_EXPONENT(HUGE(vmiss), ixubd + ixone) ! (to do) mbits mask
       case (KS_UNDER)
          ixd = min(ixdnm, ixlbd - 1) - mbits + ixone
          if (ixd.ge.MINEXPONENT(vmiss)) then
             vpos = _SET_EXPONENT(HUGE(vmiss), ixd) ! (to do) mbits mask
          else
             jw = digits(vmiss) - 1 - (MINEXPONENT(vmiss) - ixd - 1)
             vpos = _SET_EXPONENT(AINT(_SET_EXPONENT(HUGE(vmiss), jw)), ixd)
          endif
       case (KS_ROOF)
          vpos = _SET_EXPONENT(1.0_KRFLD, ixubd+1 + ixone)
       case default
          vpos = 0.0_KRFLD
       end select
       r(0) = vpos
       r(1) = sign(vpos, sn)
    end select
    return
  end function set_special_f

!!!_ + internal procedures
!!!_  & alloc_works
  subroutine alloc_works &
       & (ierr, n)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: n
    integer nc

    ierr = 0
    nc = size(wibuf)
    if (n.gt.nc) then
       if (ierr.eq.0) deallocate(wibuf, STAT=ierr)
       if (ierr.eq.0) allocate(wibuf(0:n-1), STAT=ierr)
    endif
    return
  end subroutine alloc_works
!!!_  & xuint - unsigned integer conversion emulation
  ELEMENTAL integer function xuint_d(v) result(n)
    implicit none
    integer,parameter :: KRTGT=KDBL
    real(kind=KRTGT),intent(in) :: v
    real(kind=KRTGT),parameter  :: o = REAL(HUGE(n), kind=KRTGT) + 1.0_KRTGT
    real(kind=KRTGT),parameter  :: d = o * 2.0_KRTGT

    n = int(mod(aint(abs(v)) + o, d) - o)
    return
  end function xuint_d
  ELEMENTAL integer function xuint_f(v) result(n)
    implicit none
    integer,parameter :: KRTGT=KFLT
    real(kind=KRTGT),intent(in) :: v
    real(kind=KRTGT),parameter  :: o = REAL(HUGE(n), kind=KRTGT) + 1.0_KRTGT
    real(kind=KRTGT),parameter  :: d = o * 2.0_KRTGT

    n = int(mod(aint(abs(v)) + o, d) - o)
    return
  end function xuint_f

!!!_  & xureal - unsigned integer conversion emulation
  ELEMENTAL real(kind=KDBL) function xureal_d(n, v) result(r)
    implicit none
    integer,parameter :: KRTGT=KDBL
    integer,         intent(in) :: n
    real(kind=KRTGT),intent(in) :: v
    real(kind=KRTGT),parameter  :: o = REAL(HUGE(n), kind=KRTGT) + 1.0_KRTGT
    real(kind=KRTGT),parameter  :: d = o * 2.0_KRTGT

    r = mod(real(n, kind=KIND(v)) + d, d)
    return
  end function xureal_d
  ELEMENTAL real(kind=KFLT) function xureal_f(n, v) result(r)
    implicit none
    integer,parameter :: KRTGT=KFLT
    integer,         intent(in) :: n
    real(kind=KRTGT),intent(in) :: v
    real(kind=KRTGT),parameter  :: o = REAL(HUGE(n), kind=KRTGT) + 1.0_KRTGT
    real(kind=KRTGT),parameter  :: d = o * 2.0_KRTGT

    r = mod(real(n, kind=KIND(v)) + d, d)
    return
  end function xureal_f

!!!_  & xsign_d() - return +1 if not sign bit, otherwise -1
  ELEMENTAL integer function xsign_d(v) result(n)
!NEC$ always_inline
    implicit none
    integer,parameter :: KRTGT=KDBL
    real(kind=KRTGT),intent(in) :: v
    real(kind=KRTGT),parameter :: one  = 1.0_KRTGT
    n = int(sign(one, v))
  end function xsign_d
  ELEMENTAL integer function xsign_f(v) result(n)
!NEC$ always_inline
    implicit none
    integer,parameter :: KRTGT=KFLT
    real(kind=KRTGT),intent(in) :: v
    real(kind=KRTGT),parameter :: one  = 1.0_KRTGT
    n = int(sign(one, v))
  end function xsign_f

!!!_  & xspecial ()
  ELEMENTAL integer function xspecial_d(v) result(n)
    implicit none
    integer,parameter :: KRTGT=KDBL
    real(kind=KRTGT),intent(in) :: v
    real(kind=KRTGT),parameter :: xmd = MAX_SPECIAL
    n = int(mod(abs(v), xmd))
    return
  end function xspecial_d
  ELEMENTAL integer function xspecial_f(v) result(n)
    implicit none
    integer,parameter :: KRTGT=KFLT
    real(kind=KRTGT),intent(in) :: v
    real(kind=KRTGT),parameter :: xmd = MAX_SPECIAL
    n = int(mod(abs(v), xmd))
    return
  end function xspecial_f

!!!_  & zspecial ()
  ELEMENTAL &
       real(kind=KRTGT) function zspecial_d &
       & (i, nh, kh, mh, nl, kl, ml, one, ixone) &
       & result(v)
    use TOUZA_Trp_std,only: KRTGT=>KDBL
    implicit none
    integer,parameter :: KIBGZ=KI32
    integer(kind=KIBGZ),intent(in) :: i
    integer(kind=KIBGZ),intent(in) :: nh, kh, mh
    integer(kind=KIBGZ),intent(in) :: nl, kl, ml
    real(kind=KRTGT),   intent(in) :: one
    integer,            intent(in) :: ixone
    integer,parameter :: mplim = +HUGE(KIBGZ)
    integer,parameter :: mnlim = -HUGE(KIBGZ) -1
    real(kind=KRTGT)    :: vmsk
    integer(kind=KIBGZ) :: jh, jl
    integer mbits
    if (kh.gt.mplim - i) then
       jh = i - (mplim - kh) - 1 + mnlim
    else
       jh = i + kh
    endif
    ! why not work for maximum bits?
    if (nh.lt.BIT_SIZE(0_KIBGZ)) then
       jh = IBITS(IOR(jh, mh), 0, nh)
    endif
    mbits  = nh + nl
    vmsk = _SET_EXPONENT(one, nh + ixone)
    v = _SET_EXPONENT(vmsk + xureal(jh, one), mbits + ixone)

    if (nl.gt.0) then
       jl = 0 + kl
       jl = IBITS(IOR(jl, ml), 0, nl)
       v  = v + real(jl, kind=KRTGT)
    endif
    return
  end function zspecial_d
  ELEMENTAL &
       real(kind=KRTGT) function zspecial_f &
       & (i, nh, kh, mh, nl, kl, ml, one, ixone) &
       & result(v)
    use TOUZA_Trp_std,only: KRTGT=>KFLT
    implicit none
    integer,parameter :: KIBGZ=KI32
    integer(kind=KIBGZ),intent(in) :: i
    integer(kind=KIBGZ),intent(in) :: nh, kh, mh
    integer(kind=KIBGZ),intent(in) :: nl, kl, ml
    real(kind=KRTGT),   intent(in) :: one
    integer,            intent(in) :: ixone
    integer,parameter :: mplim = +HUGE(KIBGZ)
    integer,parameter :: mnlim = -HUGE(KIBGZ) -1
    real(kind=KRTGT)    :: vmsk
    integer(kind=KIBGZ) :: jh, jl
    integer mbits
    if (kh.gt.mplim - i) then
       jh = i - (mplim - kh) - 1 + mnlim
    else
       jh = i + kh
    endif
    ! why not work for maximum bits?
    if (nh.lt.BIT_SIZE(0_KIBGZ)) then
       jh = IBITS(IOR(jh, mh), 0, nh)
    endif
    mbits  = nh + nl
    vmsk = _SET_EXPONENT(one, nh + ixone)
    v = _SET_EXPONENT(vmsk + xureal(jh, one), mbits + ixone)

    if (nl.gt.0) then
       jl = 0 + kl
       jl = IBITS(IOR(jl, ml), 0, nl)
       v  = v + real(jl, kind=KRTGT)
    endif
    return
  end function zspecial_f

!!!_  & which_sp () - return 0 if positive sign, +1 otherwise
  ELEMENTAL integer function which_sp_d(v) result(n)
    integer,parameter :: KRTGT=KDBL
    real(kind=KRTGT),intent(in) :: v
    real(kind=KRTGT),parameter  :: one = 1.0_KRTGT
    n = (1 - int(sign(one, v))) / 2
    return
  end function which_sp_d
  ELEMENTAL integer function which_sp_f(v) result(n)
    integer,parameter :: KRTGT=KFLT
    real(kind=KRTGT),intent(in) :: v
    real(kind=KRTGT),parameter  :: one = 1.0_KRTGT
    n = (1 - int(sign(one, v))) / 2
    return
  end function which_sp_f

!!!_  & count_msbits () - count sign + mantissa bits
  PURE &
  integer function count_msbits(m, s) &
    & result(n)
    implicit none
    integer,intent(in) :: m     ! mantissa bits
    integer,intent(in) :: s     ! sign flag

    n = m + (1 - min(1, abs(s)))
    return
  end function count_msbits

!!!_  & push_show_tags
  subroutine push_show_tags(atag, itag)
    implicit none
    character(len=*),intent(in),optional :: atag
    integer,         intent(in),optional :: itag
    if (present(atag)) atag_sbp = atag
    if (present(itag)) itag_sbp = itag
  end subroutine push_show_tags
!!!_  & show_bagazo_props
  subroutine show_bagazo_props &
       & (ierr, ibagaz, u)
    use TOUZA_Trp_std,only: choice
    use TOUZA_Trp_pack,only: count_packed, unparse_relleno
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KDBL
    integer,            intent(out)         :: ierr
    integer(kind=KIBGZ),intent(in)          :: ibagaz(0:*)
    integer,            intent(in),optional :: u

    integer(kind=KIBGZ),parameter :: kstkh = 0_KIBGZ

    integer utmp

    integer kid
    integer ixdnm,  ixubd,  ixlbd
    integer mbgzx,  mbgzh,  mbgzl
    integer xbits,  ebitsx, kpackx
    integer nbitsh, ebitsh, kpackh, ksign
    integer nbitsl, ebitsl, kpackl

    integer mmskh,  mmskl
    integer mofsh,  mofsl
    integer ncnz,   nbgz
    integer nbitsa, mbits
    integer sbits

    real(kind=KRFLD) :: vh, vl, dh, dl
    real(kind=KRFLD),parameter :: vone = 1.0_KRFLD
    real(kind=KRFLD),parameter :: vfil = +HUGE(vone)

    integer,parameter :: ixone = exponent(vone)
    integer,parameter :: ml = minexponent(vone)
    integer,parameter :: mh = maxexponent(vone)
    character(len=2) :: tpackx, tpackh, tpackl

    character(len=2) :: tsign(KF_NEG_ZERO:KF_POS_ZERO) &
         & = (/'*-', '0-', '--', '+-', '++', '0+', '*+'/)
    character(len=64) :: tag
    integer lt

    ierr = 0
    utmp = choice(-1, u)
101 format('bgz:', I0)
102 format('bgz/', I0)
111 format(A, ':', I0)
112 format(A, '/', I0)
    if (atag_sbp.eq.' ') then
       if (itag_sbp.ge.0) then
          write(tag, 101) itag_sbp
       else
          write(tag, 102) idx_sbp
       endif
    else
       if (itag_sbp.ge.0) then
          write(tag, 111) trim(atag_sbp), itag_sbp
       else
          write(tag, 112) trim(atag_sbp), itag_sbp
       endif
    endif
    idx_sbp = idx_sbp + 1
    lt = len_trim(tag)

    if (ierr.eq.0) call retrieve_basics(ierr, ibagaz, ixdnm, ixlbd, ixubd, ncnz, nbgz)
    if (ierr.eq.0) call retrieve_bitprops(ierr, ibagaz, KB_XBIT, xbits,  ebitsx, kpackx)
    if (ierr.eq.0) call retrieve_bitprops(ierr, ibagaz, KB_HBIT, nbitsh, ebitsh, kpackh, ksign)
    if (ierr.eq.0) call retrieve_bitprops(ierr, ibagaz, KB_LBIT, nbitsl, ebitsl, kpackl)
    if (ierr.eq.0) call retrieve_masks(ierr, ibagaz, mofsh, mmskh, mofsl, mmskl)
    if (ierr.eq.0) call unparse_relleno(ierr, tpackx, kpackx)
    if (ierr.eq.0) call unparse_relleno(ierr, tpackh, kpackh)
    if (ierr.eq.0) call unparse_relleno(ierr, tpackl, kpackl)

    if (ierr.eq.0) then
       kid = ibagaz(KB_ID)
       nbitsa = count_msbits(ebitsh, ksign)
       mbgzx = count_packed(xbits,  ncnz, kstkh)
       mbgzh = count_packed(nbitsa, ncnz, kstkh)
       mbgzl = count_packed(ebitsl, ncnz, kstkh)
       sbits = count_msbits(0, ksign)

       mbits = nbitsh + nbitsl

       vl = set_exponent(vone, max(ml, ixlbd + ixone))
       vh = set_exponent(vfil, min(mh, ixubd + ixone))

       dl = set_exponent(vone, max(ml, ixlbd - mbits + ixone))
       dh = set_exponent(vone, min(mh, ixubd - mbits + ixone))

211    format(A, ':X: ', I2, 1x, 2x, 1x, I12, 1x, A2, 2x, I0, ':', I0, ' / ', I0)
202    format(A, ':H: ', I2, 1x, A2, 1x, I12, 1x, A2, 2x, I0, 1x, '+', I0, ':', I0)
203    format(A, ':L: ', I2, 1x, 2x, 1x, I12, 1x, A2, 2x, I0, 1x, '+', I0, ':', I0)
251    format(A, ':T: ', I2, 1x, 2x, 1x, I12, ' / ', I0)
252    format(A, ':A: ', E16.9, ' - ', E16.9)
253    format(A, ':D: ', E16.9, ' - ', E16.9, 1x, I0)
254    format(A, ':E: ', 16(1x, I0))
       if (utmp.ge.0) then
          write(utmp, 211) tag(1:lt), &
               &  xbits, mbgzx, tpackx, ixlbd, ixubd, ixdnm
          write(utmp, 202) tag(1:lt), &
               &  ebitsh+sbits, tsign(ksign), mbgzh, tpackh, nbitsh, mofsh, mmskh
          write(utmp, 203) tag(1:lt), &
               &  ebitsl, mbgzl, tpackl, nbitsl, mofsl, mmskl
          write(utmp, 251) tag(1:lt), &
               &  (xbits + sbits + ebitsh + ebitsl), nbgz, ncnz
          write(utmp, 252) tag(1:lt), vl, vh
          write(utmp, 253) tag(1:lt), dl, dh, mbits
          write(utmp, 254) tag(1:lt), ibagaz(KB_XTR0:KB_XTRZ)
       else if (utmp.eq.-1) then
          write(*,    211) tag(1:lt), &
               & xbits, mbgzx, tpackx, ixlbd, ixubd, ixdnm
          write(*,    202) tag(1:lt), &
               & ebitsh+sbits, tsign(ksign), mbgzh, tpackh, nbitsh, mofsh, mmskh
          write(*,    203) tag(1:lt), &
               & ebitsl, mbgzl, tpackl, nbitsl, mofsl, mmskl
          write(*,    251) tag(1:lt), &
               &  (xbits + sbits + ebitsh + ebitsl), nbgz, ncnz
          write(*,    252) tag(1:lt), vl, vh
          write(*,    253) tag(1:lt), dl, dh, mbits
          write(*,    254) tag(1:lt), ibagaz(KB_XTR0:KB_XTRZ)
       endif
    endif
    return
  end subroutine show_bagazo_props

!!!_  & show_bagazo_patterns
  subroutine show_bagazo_patterns_di &
       & (ierr, ibagaz, vsrc, mem, u)
    use TOUZA_Trp_std,only: choice
    use TOUZA_Trp_pack,only: count_packed, pack_store, show_packed
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KDBL
    integer,            intent(out)         :: ierr
    integer(kind=KIBGZ),intent(in)          :: ibagaz(0:*)
    real   (kind=KRFLD),intent(in)          :: vsrc(0:*)
    integer,            intent(in)          :: mem
    integer,            intent(in),optional :: u

    integer(kind=KIBGZ),parameter :: kstkh = 0_KIBGZ

    integer utmp

    integer ncnz
    integer mbgzx,  mbgzh,  mbgzl
    integer xbits,  ebitsx, kpackx
    integer nbitsh, ebitsh, kpackh, ksign
    integer nbitsl, ebitsl, kpackl
    integer jpbgn,  jpend
    integer bitsm

    ierr = 0
    utmp = choice(-1, u)

    if (ierr.eq.0) call retrieve_bitprops(ierr, ibagaz, KB_XBIT, xbits,  ebitsx, kpackx)
    if (ierr.eq.0) call retrieve_bitprops(ierr, ibagaz, KB_HBIT, nbitsh, ebitsh, kpackh, ksign)
    if (ierr.eq.0) call retrieve_bitprops(ierr, ibagaz, KB_LBIT, nbitsl, ebitsl, kpackl)

    ncnz  = ibagaz(KB_NCNZ)
    bitsm = count_msbits(ebitsh, ksign)

    mbgzx = count_packed(xbits,  ncnz, kstkh)
    mbgzh = count_packed(bitsm,  ncnz, kstkh)
    mbgzl = count_packed(ebitsl, ncnz, kstkh)

    jpbgn = KB_HEAD
    jpend = jpbgn + mbgzx
    if (xbits.gt.0) then
       call show_packed(ierr, ibagaz(jpbgn:jpend-1), mem, xbits, kpackx, 'x', utmp)
    endif
    jpbgn = jpend
    jpend = jpbgn + mbgzh
    call show_packed(ierr, ibagaz(jpbgn:jpend-1), mem, bitsm, kpackh, 'h', utmp)
    if (mbgzl.gt.0) then
       jpbgn = jpend
       jpend = jpbgn + mbgzl
       call show_packed(ierr, ibagaz(jpbgn:jpend-1), mem, ebitsl, kpackl, 'l', utmp)
    endif
  end subroutine show_bagazo_patterns_di

!!!_  & show_work_patterns
  subroutine show_work_patterns_di &
       & (ierr, iwork, vsrc, mem, u, nbitsa, nbitsl)
    use TOUZA_Trp_std, only: choice, binstr
    implicit none
    integer,parameter :: KIBGZ=KI32, KRFLD=KDBL
    integer,            intent(out)         :: ierr
    integer(kind=KIBGZ),intent(in)          :: iwork(0:*)
    real   (kind=KRFLD),intent(in)          :: vsrc(0:*)
    integer,            intent(in)          :: mem
    integer,            intent(in),optional :: u
    integer,            intent(in),optional :: nbitsa, nbitsl

    integer(kind=KIBGZ),parameter :: kstkh = 0_KIBGZ

    integer j
    integer utmp
    character(len=128) :: bh, bl

    ierr = 0
    utmp = choice(-1, u)

901 format(I3.3, 1x, SP, I5.4, SS, 1x, A, 1x, A, 1x, E16.9, 1x, I0)
    do j = 0, mem - 1
       if (nbitsa.gt.0) then
          call binstr(bh, iwork(j+mem),   W=nbitsa)
       else
          bh = '-'
       endif
       if (nbitsl.gt.0) then
          call binstr(bl, iwork(j+mem*2), W=nbitsl)
       else
          bl = '-'
       endif
       if (utmp.ge.0) then
          write(utmp, 901) j, iwork(j), trim(bh), trim(bl), vsrc(j), exponent(vsrc(j))
       else if (utmp.eq.-1) then
          write(*,    901) j, iwork(j), trim(bh), trim(bl), vsrc(j), exponent(vsrc(j))
       endif
    enddo

  end subroutine show_work_patterns_di


!!!_  & show_pattern_float
  subroutine show_pattern_float_d (ierr, tag, V, u)
    use TOUZA_Trp_std,only: choice
    implicit none
    integer,parameter :: KRTGT=KDBL
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: tag
    real(kind=KRTGT),intent(in)          :: v
    integer,         intent(in),optional :: u

    integer,parameter :: lstr = 64
    character(len=lstr) :: buf
    integer utmp

    ierr = 0
    utmp = choice(-1, u)
    call binstr_float(buf, V)

101 format(A, ': ', A, 1x, E24.16)
    if (utmp.ge.0) then
       write(utmp, 101) trim(tag), trim(buf), v
    else if (utmp.eq.-1) then
       write(*,    101) trim(tag), trim(buf), v
    endif
    return
  end subroutine show_pattern_float_d
  subroutine show_pattern_float_f (ierr, tag, V, u)
    use TOUZA_Trp_std,only: choice
    implicit none
    integer,parameter :: KRTGT=KFLT
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: tag
    real(kind=KRTGT),intent(in)          :: v
    integer,         intent(in),optional :: u

    integer,parameter :: lstr = 32
    character(len=lstr) :: buf
    integer utmp

    ierr = 0
    utmp = choice(-1, u)
    call binstr_float(buf, V)

101 format(A, ': ', A, 1x, E24.16)
    if (utmp.ge.0) then
       write(utmp, 101) trim(tag), trim(buf), v
    else if (utmp.eq.-1) then
       write(*,    101) trim(tag), trim(buf), v
    endif
    return
  end subroutine show_pattern_float_f
!!!_  & binstr_float
  subroutine binstr_float_d(T, V)
    use TOUZA_Trp_std,only: KI64, binstr
    implicit none
    integer,parameter :: KRTGT=KDBL
    character(len=*),intent(out) :: T
    real(kind=KRTGT),intent(in)  :: V
    integer(kind=KI64) :: L
    character(len=128) :: B
    real(kind=KRTGT),parameter :: one  = 1.0_KRTGT
    real(kind=KRTGT),parameter :: zero = 0.0_KRTGT
    integer,parameter :: MSK = DIGITS(V) - 1
    integer E, S

    L = TRANSFER(V, L)
    call binstr(B, L, M=MSK)
    S = int(sign(one, V))
    E = max(0, min(exponent(V), maxexponent(V) + 1) - minexponent(V) + 1)
    if (V.eq.zero) E = 0
101 format(I4.4, A1, A)
    if (S.gt.0) then
       write(T, 101) E, '+', trim(B)
    else
       write(T, 101) E, '-', trim(B)
    endif
    return
  end subroutine binstr_float_d
  subroutine binstr_float_f(T, V)
    use TOUZA_Trp_std,only: KI32, binstr
    implicit none
    integer,parameter :: KRTGT=KFLT
    character(len=*),intent(out) :: T
    real(kind=KRTGT),intent(in)  :: V
    integer(kind=KI32) :: L
    character(len=128) :: B
    real(kind=KRTGT),parameter :: one  = 1.0_KRTGT
    real(kind=KRTGT),parameter :: zero = 0.0_KRTGT
    integer,parameter :: MSK = DIGITS(V) - 1
    integer E, S

    L = TRANSFER(V, L)
    call binstr(B, L, M=MSK)
    S = int(sign(one, V))
    E = max(0, min(exponent(V), maxexponent(V) + 1) - minexponent(V) + 1)
    if (V.eq.zero) E = 0
101 format(I4.4, A1, A)
    if (S.gt.0) then
       write(T, 101) E, '+', trim(B)
    else
       write(T, 101) E, '-', trim(B)
    endif
    return
  end subroutine binstr_float_f

!!!_  & guardar_extra - save extra properties
  subroutine guardar_extra_is &
       & (ierr, ibagaz, &
       &  idx,  v)
    implicit none
    integer,parameter :: KIBGZ=KI32
    integer,            intent(out)   :: ierr
    integer(kind=KIBGZ),intent(inout) :: ibagaz(0:*)
    integer,            intent(in)    :: idx
    integer,            intent(in)    :: v
    integer jx

    if (idx.lt.0) then
       ibagaz(KB_XTR0:KB_XTRZ) = v
    else
       jx = idx + KB_XTR0
       if (jx.ge.KB_XTR0.and.jx.le.KB_XTRZ) then
          ibagaz(jx) = v
       else
          ierr = -1
       endif
    endif
    return
  end subroutine guardar_extra_is
  subroutine guardar_extra_ia &
       & (ierr, ibagaz, &
       &  idx,  v)
    implicit none
    integer,parameter :: KIBGZ=KI32
    integer,            intent(out)   :: ierr
    integer(kind=KIBGZ),intent(inout) :: ibagaz(0:*)
    integer,            intent(in)    :: idx
    integer,            intent(in)    :: v(:)
    integer jxb, jxe

    jxb = idx + KB_XTR0
    if (jxb.ge.KB_XTR0.and.jxb.le.KB_XTRZ) then
       jxe = jxb + size(v) - 1
       if (jxe.ge.KB_XTR0.and.jxe.le.KB_XTRZ) then
          ibagaz(jxb:jxe) = v(:)
       else
          ierr = -1
       endif
    else
       ierr = -1
    endif
    return
  end subroutine guardar_extra_ia

!!!_  & save_basics - save basic properties in bagazo
  subroutine save_basics &
       & (ierr,  ibagaz, &
       &  ixdnm, ixlbd, ixubd, ixone, ncnz, nbgz)
    implicit none
    integer,parameter :: KIBGZ=KI32
    integer,            intent(out)   :: ierr
    integer(kind=KIBGZ),intent(inout) :: ibagaz(0:*)  ! integer representaion of float source
    integer,            intent(in)    :: ixdnm, ixlbd, ixubd  ! exponent ranges
    integer,            intent(in)    :: ixone                ! reference exponent
    integer,            intent(in)    :: ncnz,  nbgz          ! number of items compressed/original

    ierr = 0

    ibagaz(KB_ID)   = TRAPICHE_ID
    ibagaz(KB_NCNZ) = ncnz
    ibagaz(KB_NBGZ) = nbgz

    ibagaz(KB_XDNM) = ixdnm - ixone
    ibagaz(KB_XLBD) = ixlbd - ixone
    ibagaz(KB_XUBD) = ixubd - ixone

  end subroutine save_basics

!!!_  & retrieve_basics - save basic properties in bagazo
  subroutine retrieve_basics &
       & (ierr,  ibagaz, &
       &  ixdnm, ixlbd, ixubd, ncnz, nbgz)
    implicit none
    integer,parameter :: KIBGZ=KI32
    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! integer representaion of float source
    integer,            intent(out) :: ixdnm, ixlbd, ixubd  ! exponent ranges
    integer,            intent(out) :: ncnz,  nbgz          ! number of items compressed/original

    ierr = 0
    if (ibagaz(KB_ID).eq.TRAPICHE_ID) then
       ncnz  = ibagaz(KB_NCNZ)
       nbgz  = ibagaz(KB_NBGZ)
       ixdnm = ibagaz(KB_XDNM)
       ixlbd = ibagaz(KB_XLBD)
       ixubd = ibagaz(KB_XUBD)
    else
       ierr = -1
    endif
    return
  end subroutine retrieve_basics

!!!_  & save_masks - save mask and offset properties in bagazo
  subroutine save_masks &
       & (ierr,  ibagaz, &
       &  mofsh, mmskh,  mofsl, mmskl)
    implicit none
    integer,parameter :: KIBGZ=KI32
    integer,            intent(out)   :: ierr
    integer(kind=KIBGZ),intent(inout) :: ibagaz(0:*)  ! integer representaion of float source
    integer,            intent(in)    :: mofsh, mmskh
    integer,            intent(in)    :: mofsl, mmskl
    ierr = 0
    ibagaz(KB_MSKH) = mmskh
    ibagaz(KB_MSKL) = mmskl
    ibagaz(KB_OFSH) = mofsh
    ibagaz(KB_OFSL) = mofsL
  end subroutine save_masks
!!!_  & retrieve_masks - retrieve mask and offset properties from bagazo
  subroutine retrieve_masks &
       & (ierr,  ibagaz, &
       &  mofsh, mmskh,  mofsl, mmskl)
    implicit none
    integer,parameter :: KIBGZ=KI32
    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! integer representaion of float source
    integer,            intent(out) :: mofsh, mmskh
    integer,            intent(out) :: mofsl, mmskl
    ierr = 0
    mmskh = ibagaz(KB_MSKH)
    mmskl = ibagaz(KB_MSKL)
    mofsh = ibagaz(KB_OFSH)
    mofsL = ibagaz(KB_OFSL)
  end subroutine retrieve_masks

!!!_  & save_bitprops - save bit properties in bagazo
  subroutine save_bitprops &
       & (ierr, ibagaz, &
       &  idx,  nbits,  ebits,  kpack, ksignp)
    implicit none
    integer,parameter :: KIBGZ=KI32
    integer,            intent(out)         :: ierr
    integer(kind=KIBGZ),intent(inout)       :: ibagaz(0:*)  ! integer representaion of float source
    integer,            intent(in)          :: idx
    integer,            intent(in)          :: nbits, ebits
    integer,            intent(in)          :: kpack
    integer,            intent(in),optional :: ksignp

    integer(kind=KIBGZ) m

    ierr = 0
    m = kpack
    m = IOR(m, ISHFT(nbits, 8))
    m = IOR(m, ISHFT(ebits, 16))
    if (present(ksignp)) then
       m = IOR(M, ISHFT((ksignp + KF_SIGN_INFLATE), 24))
    endif
    ibagaz(idx) = m
  end subroutine save_bitprops

!!!_  & retrieve_bitprops - retrieve bit properties from bagazo
  subroutine retrieve_bitprops &
       & (ierr, ibagaz, &
       &  idx,  nbits,  ebits,  kpack, ksignp)
    implicit none
    integer,parameter :: KIBGZ=KI32
    integer,            intent(out)          :: ierr
    integer(kind=KIBGZ),intent(in)           :: ibagaz(0:*)  ! integer representaion of float source
    integer,            intent(in)           :: idx
    integer,            intent(out)          :: nbits, ebits
    integer,            intent(out)          :: kpack
    integer,            intent(out),optional :: ksignp

    integer(kind=KIBGZ) m

    ierr = 0
    m = ibagaz(idx)

    kpack = IBITS(m, 0,  8)
    nbits = IBITS(m, 8,  8)
    ebits = IBITS(m, 16, 8)
    if (present(ksignp)) then
       ksignp = IBITS(m, 24, 8) - KF_SIGN_INFLATE
    endif

  end subroutine retrieve_bitprops

!!!_  & retrieve_pack () - retrieve pack method
  ! integer function retrieve_pack(ibagaz) result (n)
  !   implicit none
  !   integer,intent(in) :: ibagaz(0:*)
  !   n = mod(ibagaz(KB_PACK), 256)
  ! end function retrieve_pack

!!!_  & retrieve_nbgz () - retrieve total items (packed)
  integer function retrieve_nbgz(ibagaz) result (n)
    implicit none
    integer,intent(in) :: ibagaz(0:*)
    n = ibagaz(KB_NBGZ)
  end function retrieve_nbgz

!!!_  & retrieve_ncnz () - retrieve total items (source)
  integer function retrieve_ncnz(ibagaz) result (n)
    implicit none
    integer,intent(in) :: ibagaz(0:*)
    n = ibagaz(KB_NCNZ)
  end function retrieve_ncnz

!!!_  & retrieve_extra () - retrieve extra item (source)
  integer function retrieve_extra(ibagaz, idx) result (n)
    implicit none
    integer,intent(in) :: ibagaz(0:*)
    integer,intent(in) :: idx
    !! to check the index range
    n = ibagaz(KB_XTR0 + idx)
  end function retrieve_extra

!!!_  & is_recortar () - check if recortar switch enabled
  logical function is_recortar(f, ixdnm, ixlbd) result(b)
    implicit none
    integer,intent(in) :: f
    integer,intent(in) :: ixdnm, ixlbd
    ! simply skip clipping if denormalized exists
    ! b = (IAND(f, KCODE_CLIPPING).ne.0) .and. (ixdnm.eq.ixlbd)
    b = (IAND(f, KCODE_CLIPPING).ne.0)
  end function is_recortar

!!!_  & parse_codes - retrieve code flag from string
  subroutine parse_codes(ierr, kcode, str)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: kcode
    character(len=*),intent(in)  :: str
    ierr = 0
    kcode = 0
    if (scan(str, 'Tt').gt.0)   kcode = kcode + KCODE_TRANSPOSE
    if (scan(str, 'Ss').gt.0)   kcode = kcode + KCODE_SEQUENTIAL
    if (scan(str, 'Ii').gt.0)   kcode = kcode + KCODE_INCREMENTAL
    if (scan(str, 'Mm').gt.0)   kcode = kcode + KCODE_MANUAL
    if (scan(str, 'Cc').gt.0)   kcode = kcode + KCODE_CLIPPING
    if (scan(str, 'Zz').gt.0)   kcode = kcode + KCODE_SIGN_ZERO
    if (scan(str, 'Rr').gt.0)   kcode = kcode + KCODE_ROUND
    return
  end subroutine parse_codes
!!!_  & unparse_codes - retrieve code flag from string
  subroutine unparse_codes(ierr, kcode, str)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: kcode
    character(len=*),intent(out) :: str
    ierr = 0
    str = ' '
    if (IAND(kcode, KCODE_TRANSPOSE).gt.0)   str = str // 'T'
    if (IAND(kcode, KCODE_SEQUENTIAL).gt.0)  str = str // 'S'
    if (IAND(kcode, KCODE_INCREMENTAL).gt.0) str = str // 'I'
    if (IAND(kcode, KCODE_MANUAL).gt.0)      str = str // 'M'
    if (IAND(kcode, KCODE_CLIPPING).gt.0)    str = str // 'C'
    if (IAND(kcode, KCODE_SIGN_ZERO).gt.0)   str = str // 'Z'
    if (IAND(kcode, KCODE_ROUND).gt.0)       str = str // 'R'

    str = ADJUSTL(str)
    return
  end subroutine unparse_codes
!!!_  & suggest_filling () - suggest packing(filling) option
  integer function suggest_filling &
       & (nbits, nmem, kcode, kfill) &
       & result(m)
    use TOUZA_Trp_std, only: choice
    use TOUZA_Trp_pack,only: &
         & RELLENO_TRANSPOSE, RELLENO_SEQUENTIAL, &
         & RELLENO_STRIDE,    RELLENO_MANUAL
    implicit none
    integer,intent(in)          :: nbits
    integer,intent(in)          :: nmem
    integer,intent(in),optional :: kcode
    integer,intent(in),optional :: kfill    ! need reference filling if decoding
    integer,parameter :: mask_filling = KCODE_TRANSPOSE + KCODE_SEQUENTIAL + KCODE_INCREMENTAL
    integer kc, kf
    m = (0 * nbits) * nmem ! dummy
    kc = max(0, choice(0, kcode))
    if (.not.present(kfill)) then
       ! encoding
       if (IAND(kc, KCODE_TRANSPOSE).ne.0) then
          m = m + RELLENO_TRANSPOSE
       else if (IAND(kc, KCODE_SEQUENTIAL).ne.0) then
          if (IAND(kc, KCODE_INCREMENTAL).ne.0) then
             m = m + RELLENO_SEQUENTIAL
          else
             m = m + RELLENO_STRIDE
          endif
       else
          m = m + RELLENO_TRANSPOSE
       endif
       if (IAND(kc, KCODE_MANUAL).ne.0) m = m + RELLENO_MANUAL
    else
       ! decoding
       kf = IAND(kfill, RELLENO_SEQUENTIAL)
       if (kf.eq.0) then
          ! transpose
          m = m + RELLENO_TRANSPOSE
       else
          ! sequential
          if (IAND(kc, KCODE_INCREMENTAL).ne.0) then
             m = m + RELLENO_SEQUENTIAL
          else
             m = m + RELLENO_STRIDE
          endif
       endif
       if (IAND(kc, KCODE_MANUAL).ne.0) m = m + RELLENO_MANUAL
    endif
  end function suggest_filling

!!!_  & XSETSX() - set_exponent alternates for SX system
#if OPT_TRAPICHE_SX_SPECIALS
  elemental real(kind=KRTGT) function XSETSX_d(X, I) &
       & result(Y)
    use TOUZA_Trp_std,only: KRTGT=>KDBL
    implicit none
    real(kind=KRTGT),intent(in) :: X
    integer,         intent(in) :: I
    integer,parameter :: MH = MAXEXPONENT(0.0_KRTGT)
    ! more simpler design can be adopted, but, it seem not
    ! work on higher optimization case than -O1.
    ! D1(0:N-1) = (FM(0:N-1) * EXP2(REAL(MAX(0, ME(0:N-1)-(MH-1)), KIND=KIND(F0)))) &
    !      & * EXP2(REAL(MIN(MH-1, ME(0:N-1)),KIND=KIND(F0)))
    ! V(:) = (F(:) * EXP2(REAL(MAX(0, E(:) - (MH-1))))) * EXP2(REAL(MIN(MH-1, E(:))))
    Y = AMT(X) &
         & * EXP2(REAL(MAX(0, I - (MH - 1)), kind=KRTGT)) &
         & * EXP2(REAL(MIN(I, MH - 1),       kind=KRTGT))
    ! Y = AMT(X) * EXP2(REAL(I, KIND=KRTGT))
    return
  end function XSETSX_d
  elemental real(kind=KRTGT) function XSETSX_f(X, I) &
       & result(Y)
    use TOUZA_Trp_std,only: KRTGT=>KFLT
    implicit none
    real(kind=KRTGT),intent(in) :: X
    integer,         intent(in) :: I
    integer,parameter :: MH = MAXEXPONENT(0.0_KRTGT)
    ! more simpler design can be adopted, but, it seem not
    ! work on higher optimization case than -O1.
    ! D1(0:N-1) = (FM(0:N-1) * EXP2(REAL(MAX(0, ME(0:N-1)-(MH-1)), KIND=KIND(F0)))) &
    !      & * EXP2(REAL(MIN(MH-1, ME(0:N-1)),KIND=KIND(F0)))
    ! V(:) = (F(:) * EXP2(REAL(MAX(0, E(:) - (MH-1))))) * EXP2(REAL(MIN(MH-1, E(:))))
    Y = AMT(X) &
         & * EXP2(REAL(MAX(0, I - (MH - 1)), kind=KRTGT)) &
         & * EXP2(REAL(MIN(I, MH - 1),       kind=KRTGT))
    ! Y = AMT(X) * EXP2(REAL(I, KIND=KRTGT))
    return
  end function XSETSX_f
#endif /* OPT_TRAPICHE_SX_SPECIALS */
!!!_ + compare_report - check results (array)
  subroutine compare_report_d &
       & (ierr, va, vb, n, fmt, tag, u)
    use TOUZA_Trp_std,only: choice, choice_a, KDBL
    implicit none
    integer,parameter :: KRTGT=KDBL
    integer,         intent(out)         :: ierr
    real(kind=KRTGT),intent(in)          :: va(0:*), vb(0:*)
    integer,         intent(in)          :: n
    character(len=*),intent(in),optional :: fmt
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u

    integer,         parameter :: kxone = exponent(1.0_KRTGT)
    real(kind=KRTGT),parameter :: zero  = 0.0_KRTGT

    integer nt
    character(len=128) :: ti, tt
    character(len=128) :: BA, BB, BC
    real(kind=KRTGT) :: ea, er
    integer j
    integer ja, jb, jc, jx, jo, lb
    integer utmp

    ierr = 0
    utmp = choice(-1, u)
    call choice_a(ti, 'cmp', tag)

101 format(A, ':', I0)
    do j = 0, n - 1
       write(tt, 101) trim(ti), j
       call compare_element_d(ierr, va(j), vb(j), fmt, tt, u)
    enddo
1002 format(A, ': count = ', I0, ' / ', I0)
    nt = COUNT(va(0:n-1).eq.vb(0:n-1))
    if (utmp.ge.0) then
       write(utmp, 1002) trim(ti), nt, n
    else if (utmp.eq.-1) then
       write(*, 1002) trim(ti), nt, n
    endif
    return
  end subroutine compare_report_d

!!!_ + compare_element - check results (element)
  subroutine compare_element_d &
       & (ierr, va, vb, fmt, tag, u)
    use TOUZA_Trp_std,only: choice, choice_a, KDBL
    implicit none
    integer,parameter :: KRTGT=KDBL
    integer,         intent(out)         :: ierr
    real(kind=KRTGT),intent(in)          :: va, vb
    character(len=*),intent(in),optional :: fmt
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u

    integer,         parameter :: kxone = exponent(1.0_KRTGT)
    real(kind=KRTGT),parameter :: zero  = 0.0_KRTGT

    integer nt
    character(len=128) :: fi
    character(len=128) :: ti
    character(len=128) :: BA, BB, BC
    character(len=128) :: sa, sb, sc
    real(kind=KRTGT) :: ea, er
    integer ja, jb, jc, jx, jo, lb
    integer utmp

    ierr = 0
    utmp = choice(-1, u)
    call choice_a(ti, ' ', tag)
    call choice_a(fi, ' ', fmt)
    if (ti.eq.' ') ti = 'cmp'
    if (fi.eq.' ') fi = '(E26.18)'

1013 format(A, ': ', 4x,           A, 2x, A)
1014 format(A, ': ', 4x,           A, 2x, A)
1015 format(A, ': ', 1x, L1, I2.2, A, 2x, A)
    nt = 0
    call binstr_float(BA, va)
    call binstr_float(BB, vb)
    ea = abs(va - vb)
    if (va.eq.zero) then
       er = ea
    else
       er = ea / abs(va)
    endif

    ja = SCAN(BA, '+-') - 1
    jb = SCAN(BB, '+-') - 1
    BC = BB
    do jc = 1, jb
       BC(jc:jc) = ' '
    enddo
    jx = 0
    jo = -1 ! skip sign part
    lb = len_trim(BB)
    do jc = 1, lb - jb + 1
       if (BA(ja+jc:ja+jc).ne.'|') jo = jo + 1
       if (BA(ja+jc:ja+jc).eq.BB(jb+jc:jb+jc)) then
          BC(jb+jc:jb+jc) = ' '
       else
          BC(jb+jc:jb+jc) = 'X'
          if (jx.eq.0) jx = jo
       endif
    enddo
    write(sa, fi) va
    write(sb, fi) vb
    write(sc, fi) abs(va-vb)
    if (utmp.ge.0) then
       write(utmp, 1015) trim(ti), (va .eq. vb), jx, &
            &                      BC(1:lb), trim(sc)
       write(utmp, 1013) trim(ti), trim(BA), trim(sa)
       write(utmp, 1014) trim(ti), trim(BB), trim(sb)
    else
       write(*, 1015) trim(ti), (va .eq. vb), jx, &
            &                   BC(1:lb), trim(sc)
       write(*, 1013) trim(ti), trim(BA), trim(sa)
       write(*, 1014) trim(ti), trim(BB), trim(sb)
    endif
    return
  end subroutine compare_element_d

!!!_ + end of TOUZA_Trp_float
end module TOUZA_Trp_float

!!!_@ test_trapiche_float - test program
#ifdef TEST_TRAPICHE_FLOAT
program test_trapiche_float
  use TOUZA_Std_arg, only: parse, get_option, arg_init=>init, arg_diag=>diag
  use TOUZA_Std_prc, only: diag_real_props
  use TOUZA_Trp_pack,only: RELLENO_TRANSPOSE
  use TOUZA_Trp_std, only: binstr
  use TOUZA_Trp_float
  implicit none
  integer ierr
  integer,         parameter :: KRFLD = KDBL
  real(kind=KRFLD),parameter :: vone = 1.0_KRFLD ! placeholder
  real(kind=KRFLD),parameter :: vhld = vone

  integer :: lbgz, lcnz, lwrk
  real(kind=KRFLD),POINTER :: vsrc(:)

  real(kind=KRFLD),allocatable :: vrstu(:)
  integer,         allocatable :: idstu(:)
  integer,         allocatable :: iwork(:)

  real(kind=KRFLD) :: vmiss

  integer xran(3), xtop,  xbtm
  integer mbits,   xbits, xbits0
  integer mem
  integer kcode
  character(len=16) ccode
  integer jx


  integer,parameter :: kxone = exponent(vone)
  integer,parameter :: kxmin = minexponent(vone)
  integer,parameter :: kxmax = maxexponent(vone)

  ierr = 0

101 format(A, ' = ', I0)

  call init(ierr, levv=-1)
  ! write(*, 101) 'init', ierr
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call diag_real_props(ierr, vhld)
  ! write(*, 101) 'diag', ierr

  if (ierr.eq.0) call test_helper(ierr, 300.0_KRFLD, 1.0_KRFLD)
  if (ierr.eq.0) call test_helper(ierr, 256.0_KRFLD, 1.0_KRFLD)
  if (ierr.eq.0) call test_helper(ierr, 255.0_KRFLD, 1.0_KRFLD)
  do jx = 16, 14, -1
     if (ierr.eq.0) call test_helper(ierr, real(jx, kind=KRFLD), 1.0_KRFLD)
     if (ierr.eq.0) call test_helper(ierr, real(jx, kind=KRFLD), 1.0_KRFLD, 0.25_KRFLD)
  enddo
  do jx = 9, 2, -1
     if (ierr.eq.0) call test_helper(ierr, real(jx, kind=KRFLD), 1.0_KRFLD)
  enddo

!!!_ + arguments
  mbits = 0
  xran(1:3) = (/-HUGE(0)-1, +HUGE(0), 0/)

  if (ierr.eq.0) call arg_init(ierr)
  if (ierr.eq.0) call parse(ierr)
  if (ierr.eq.0) call arg_diag(ierr)
  if (ierr.eq.0) call get_option(ierr, xbits0,    'x', -HUGE(0))            ! x=EXPONENT-BITS
  if (ierr.eq.0) call get_option(ierr, xran(1:3), 'e', unset=.TRUE.)        ! e=LOWER,UPPER-EXPONENTS
  if (ierr.eq.0) call get_option(ierr, mbits, 'm', 0)                       ! m=MANTISSA-BITS
  if (ierr.eq.0) call get_option(ierr, ccode, 'c', ' ')                     ! c=CODES
  if (ierr.eq.0) call init(ierr)

  if (ierr.eq.0) call parse_codes(ierr, kcode, ccode)

  if (ierr.eq.0) call adj_xrange (xran, kxone, kxmax, kxmin, HUGE(0))

  if (ierr.eq.0) xbtm = xran(1) - kxone
  if (ierr.eq.0) xtop = xran(2) - kxone

  if (mbits.le.0) mbits = DIGITS(vsrc(1)) - 1

  if (ierr.eq.0) call asignar(ierr, 'I', KS_INF)
!!!_ + set-up
  if (ierr.eq.0) then
     call set_array_more(ierr, vsrc, vmiss, mem)
  endif

  if (ierr.eq.0) then
     lcnz = mem
     lbgz = lcnz * 3 + 16
     lwrk = mem
     allocate(vrstu(0:lcnz-1), idstu(0:lbgz-1), iwork(0:lwrk*3-1),  STAT=ierr)
  endif
!!!_ + compressor/diluter with float
  if (ierr.eq.0) then
     xbits = xbits0
     call encode_trig &
          & (ierr,  idstu, iwork, &
          &  vsrc,  mem,   vmiss, mbits, xbits, xtop, xbtm, kcode)
     call show_bagazo_props(ierr, idstu)
     ! call show_bagazo_patterns (ierr, idstu, vsrc, mem)
     call decode_alloc(ierr, vrstu, idstu, mem, vmiss)
     if (ierr.eq.0) call compare_report(ierr, vsrc, vrstu, mem)
  endif
  if (ierr.eq.0) then
     call finalize(ierr)
  endif
  write(*, 101) 'Fine', ierr
  stop
contains
  subroutine test_helper &
       & (ierr, refmax, refmin, erel)
    use TOUZA_Trp_std,only: choice
    implicit none
    integer,parameter :: KRFLD=KDBL
    integer,         intent(out)         :: ierr
    real(kind=KRFLD),intent(in)          :: refmax, refmin
    real(kind=KRFLD),intent(in),optional :: erel

    real(kind=KRFLD) :: e

    ierr = 0
    e = choice(0.0_KRFLD, erel)
    call helper_props(mbits, xbits, xbtm, refmax, refmin, erel)

101 format('helper', 3(1x, E9.3), ' >> ', I0, 1x, I0, 1x, I0)
    write(*, 101) refmax, refmin, e, mbits, xbits, xbtm
    return
  end subroutine test_helper

!!!_ + set_array - set test inputs
  subroutine set_array_more &
       & (ierr, v, vmiss, mem)
    implicit none
    integer,parameter :: KRFLD=KDBL
    integer,         intent(out)         :: ierr
    real(kind=KRFLD),intent(out),POINTER :: v(:)
    real(kind=KRFLD),intent(out)         :: vmiss
    integer,         intent(out)         :: mem

    real(kind=KRFLD),parameter :: vzero = 0.0_KRFLD
    real(kind=KRFLD),parameter :: vone = 1.0_KRFLD
    integer,parameter :: kxone = exponent(vone)
    integer,parameter :: kxmin = minexponent(vone)
    integer,parameter :: kxmax = maxexponent(vone)
    integer,parameter :: lfrd = DIGITS(vone) - 1

    integer ssw,   nfr
    integer nml(3), dnm(2), rfr(3)
    real(kind=KRFLD) :: sa(0:1), s
    integer kipos, kineg
    integer kzpos, kzneg
    integer kmiss
    character(len=256) :: csw
    integer kxs

    real(kind=KRFLD) :: FR110(lfrd)    ! 1.1111 1.1110 1.1100 1.1000
    real(kind=KRFLD) :: FR011(lfrd)    ! 1.1111 1.0111 1.0011 1.0001
    real(kind=KRFLD) :: FR010(lfrd)    ! 1.1000 1.0100 1.0010 1.0000
    real(kind=KRFLD) :: ftmp

    integer :: jv, lv
    integer :: j,  jj

    ierr = 0

    do j = 1, lfrd
       FR010(j) = vone + set_exponent(vone, kxone-j)
       FR011(j) = vone
       FR110(j) = vone
       do jj = j, lfrd
          FR011(j) = FR011(j) + set_exponent(vone, kxone-jj)
       enddo
       do jj = 1, lfrd - j + 1
          FR110(j) = FR110(j) + set_exponent(vone, kxone-jj)
       enddo
    enddo
    FR010(lfrd) = vone           ! set manually

    if (ierr.eq.0) call get_option(ierr, nml(:), 'n', 0)  ! n=BEGIN[,END[,REF]]   exponent range (normalized)
    if (ierr.eq.0) call get_option(ierr, dnm(:), 'd', 0)  ! d=BEGIN[,END]         exponent range (denormalized)
    if (ierr.eq.0) call get_option(ierr, rfr(:), 'f', 0)  ! f=BEGIN[,END,[MODE]]  fraction range
    if (ierr.eq.0) call get_option(ierr, ssw,    's', 0)  ! sign switch
    if (ierr.eq.0) call get_option(ierr, vmiss,  'u',  -999.0_KRFLD)
    if (ierr.eq.0) call get_option(ierr, csw,    'o',  ' ')

    kzpos = 0
    kzneg = 0
    kipos = 0
    kineg = 0
    kmiss = 0
    if (ierr.eq.0) then
       if (index(csw, '+0').gt.0) kzpos = 1
       if (index(csw, '-0').gt.0) kzneg = 1
       if (index(csw, '+i').gt.0) kipos = 1
       if (index(csw, '-i').gt.0) kineg = 1
       if (index(csw, 'u').gt.0)  kmiss = 1
    endif
    ! rfr(:)
    if (ierr.eq.0) then
       if (rfr(1).eq.0) rfr(1) = 1
       if (rfr(2).eq.0) rfr(2) = lfrd
       if (rfr(3).eq.0) rfr(3) = 1 + 2 + 4
       nfr = 0
       do jj = 1, 3
          if (BTEST(rfr(3), jj-1)) nfr = nfr + 1
       enddo
    endif
    ! nml
    if (ierr.eq.0) call adj_xrange (nml, kxone, kxmax, kxmin)
    ! dnm
    if (ierr.eq.0) then
       kxs = 0
       if (dnm(1).eq.0.and.dnm(2).eq.0) then ! 0,0  no denormalized
          dnm(1) = 0
          dnm(2) = -1
       else
          if (dnm(1).lt.0) dnm(1) = lfrd + 1 + dnm(1)
          if (dnm(2).lt.0) dnm(2) = lfrd + 1 + dnm(2)
          dnm(1) = max(1, dnm(1))
          dnm(2) = max(dnm(1), dnm(2))
       endif
    endif
    ! sign
    if (ierr.eq.0) then
       if (ssw.gt.0) then
          sa(0:1) = +vone
       else if (ssw.lt.0) then
          sa(0:1) = -vone
       else
          sa(0) = +vone
          sa(1) = -vone
       endif
    endif


    if (ierr.eq.0) then
       lv = kzpos + kzneg + kipos + kineg + kmiss
       lv = lv + (abs(nml(2)-nml(1)) + 1)    * (abs(rfr(2) - rfr(1)) + 1) * nfr ! normalized
       lv = lv + (max(dnm(2)-dnm(1) + 1, 0)) * (abs(rfr(2) - rfr(1)) + 1) * nfr ! denormalized
       allocate(vsrc(0:lv-1), STAT=ierr)
    endif

    jv = 0
    if (ierr.eq.0) then
       do jx = nml(2), nml(1), -sign(1, nml(2)-nml(1))
          if (IAND(rfr(3),4).ne.0) then
             do j = rfr(1), rfr(2)
                s = sa(modulo(j, 2))
                v(jv) = set_exponent(FR110(j), jx) * s
                jv = jv + 1
             enddo
          endif
          s = sa(modulo(jx, 2))
          do j = rfr(1), rfr(2)
             if (IAND(rfr(3),2).ne.0) then
                v(jv) = set_exponent(FR011(j), jx) * s
                jv = jv + 1
             endif
             if (IAND(rfr(3),1).ne.0) then
                v(jv) = set_exponent(FR010(j), jx) * s
                jv = jv + 1
             endif
          enddo
       enddo
       do jx = dnm(1), min(dnm(2), lfrd - 1)
          if (IAND(rfr(3),4).ne.0) then
             do j = rfr(1), min(lfrd - jx, rfr(2))
                s = sa(modulo(j, 2))
                ftmp = AINT(set_exponent(FR110(j+1), kxone + lfrd - jx))
                v(jv) = set_exponent(ftmp, kxmin - jx) * s
                jv = jv + 1
             enddo
          endif
          s = sa(modulo(jx, 2))
          do j = rfr(1), min(lfrd - jx - 1, rfr(2) - 1)
             if (IAND(rfr(3),2).ne.0) then
                ftmp = AINT(set_exponent(FR011(j), kxone + lfrd - jx))
                v(jv) = set_exponent(ftmp, kxmin - jx) * s
                jv = jv + 1
             endif
             if (IAND(rfr(3),1).ne.0) then
                ftmp = AINT(set_exponent(FR010(j), kxone + lfrd - jx))
                v(jv)   = set_exponent(ftmp, kxmin - jx) * s
                jv = jv + 1
             endif
          enddo
          ! need special pattern for last
          j = min(lfrd - jx, rfr(2))
          if (j.ge.rfr(1)) then
             if (IAND(rfr(3),2).ne.0) then
                ftmp = AINT(set_exponent(FR011(j), kxone + lfrd - jx))
                v(jv) = set_exponent(ftmp, kxmin - jx) * s
                jv = jv + 1
             endif
             if (IAND(rfr(3),1).ne.0) then
                ftmp = AINT(set_exponent(vone, kxone + lfrd - jx))
                v(jv) = set_exponent(ftmp, kxmin - jx) * s
                jv = jv + 1
             endif
          endif
       enddo
       jx = dnm(2)
       if (jx.eq.lfrd.and.rfr(1).le.1) then
          s = sa(modulo(jx, 2))
          j = min(lfrd - jx, rfr(2))
          ftmp = AINT(set_exponent(FR010(j), kxone + lfrd - jx))
          v(jv) = set_exponent(ftmp, kxmin - jx) * s
          jv = jv + 1
       endif
    endif
    if (ierr.eq.0) then
       if (kzpos.gt.0) then
          v(jv) = sign(vzero, +vone)
          jv = jv + 1
       endif
       if (kzneg.gt.0) then
          v(jv) = sign(vzero, -vone)
          jv = jv + 1
       endif
       if (kmiss.gt.0) then
          v(jv) = vmiss
          jv = jv + 1
       endif
       if (kipos.gt.0) then
          v(jv) = +HUGE(vone)
          v(jv) = v(jv) * REAL(RADIX(vone), KRFLD)
          jv = jv + 1
       endif
       if (kineg.gt.0) then
          v(jv) = -HUGE(vone)
          v(jv) = v(jv) * REAL(RADIX(vone), KRFLD)
          jv = jv + 1
       endif
    endif

    mem = jv
  end subroutine set_array_more

!!!_ + adj_xrange - adjust exponent range to natives
  subroutine adj_xrange (xe, kxone, kxmax, kxmin, kxundef)
    implicit none
    integer,intent(inout)       :: xe(1:3)
    integer,intent(in)          :: kxone, kxmax, kxmin
    integer,intent(in),optional :: kxundef
    integer kxs, kxbase
    integer j
    kxs = 0
    if (xe(3).eq.0) then ! native
       kxbase = 0
    else if (xe(3).eq.1) then ! relative to exp(1)
       kxbase = kxone
    else if (xe(3).gt.1) then ! relative to maxexponent
       kxbase = kxmax
       kxs = -1
    else                      ! relative to minexponent
       kxbase = kxmin
       kxs = +1
    endif
    if (present(kxundef)) then
       do j = 1, 2
          if (xe(j).ge.kxundef.or.xe(j).le.-kxundef) then
             continue
          else
             if (kxs.ne.0) xe(j) = sign(xe(j), kxs)
             xe(j) = xe(j) + kxbase
          endif
       enddo
    else
       if (kxs.ne.0) xe(1:2) = sign(xe(1:2), kxs)
       xe(1:2) = xe(1:2) + kxbase
    endif
  end subroutine adj_xrange

end program test_trapiche_float

#endif /* TEST_TRAPICHE_FLOAT */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

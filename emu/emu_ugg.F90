!!!_! emu_ugg.F90 - touza/emu geography geometry geodesy
! Maintainer: SAITO Fuyuki
! Created: Dec 23 2022
#define TIME_STAMP 'Time-stamp: <2023/11/27 15:09:01 fuyuki emu_ugg.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022, 2023
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
!!!_* Macros
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_emu.h"
#include "touza_std.h"
#ifndef TEST_EMU_UGG
#define TEST_EMU_UGG 0
#endif  TEST_EMU_UGG 0
!!!_ + hypot
#ifndef   OPT_UGG_HYPOT_INTRINSIC
#  define OPT_UGG_HYPOT_INTRINSIC 1
#endif
#if   OPT_UGG_HYPOT_INTRINSIC
#  if HAVE_FORTRAN_HYPOT
#  else
#    warning "intrinsic hypot not avaiable"
#    undef  OPT_UGG_HYPOT_INTRINSIC
#    define OPT_UGG_HYPOT_INTRINSIC 0
#  endif
#endif
#if   OPT_UGG_HYPOT_INTRINSIC
#  define _hypot HYPOT
#else
#  define _hypot ipc_HYPOT
#endif
#ifndef   OPT_PSGINV_ITER_LIMIT
#  define OPT_PSGINV_ITER_LIMIT 30
#endif

#define _SIN(TERM) TERM(1)
#define _COS(TERM) TERM(2)
#define _ANGLE(TERM) TERM(3)
#define _SIN2(TERM) (_SIN(TERM)**2)
#define _COS2(TERM) (_COS(TERM)**2)

!!!_@ TOUZA_Emu_ugg - Geometry procedures
module TOUZA_Emu_ugg
!!!_ = declaration
!!!_  - modules
  use TOUZA_Std,only: get_logu,     unit_global,  trace_fine,   trace_control
  use TOUZA_Std,only: ipc_HYPOT
!!!_  - default
  implicit none
  private
!!!_ + public parameter
  integer,parameter,public :: LAT_GAUSS_LEGACY = 0        !! gaussian latitude (MIROC fully compatible)
  integer,parameter,public :: LAT_GAUSS  = 1              !! gaussian latitude (MIROC symbolically equivalent)
  integer,parameter,public :: LAT_LINEAR = 2              !! linear latitude
  integer,parameter,public :: LAT_LINEAR_COMPATIBLE = 3   !! linear latitude (deprecated)

  integer,parameter,public :: DIV_EACH_EDGE  = 0          !! divide each cells, accumulate from both edges
  integer,parameter,public :: DIV_EACH_INCR  = 1          !! divide each cells, accumulate from lower edge
  integer,parameter,public :: DIV_ACCUMULATE = 2          !! legacy, accumulate from the origin

!!!_ + cache size and index
  integer,parameter,public :: ncache_psgp_lo = 2
  integer,parameter,public :: ncache_psgp_la = 2
  integer,parameter,public :: ncache_psgp_co = 9

  integer,parameter :: icache_psgp_sindlo = 1
  integer,parameter :: icache_psgp_cosdlo = 2

  integer,parameter :: icache_psgp_xrho   = 1
  integer,parameter :: icache_psgp_yrho   = 2

  integer,parameter :: icache_psgp_xco  = 1
  integer,parameter :: icache_psgp_yco  = 2
  integer,parameter :: icache_psgp_sign = 3
  integer,parameter :: icache_psgp_ecc  = 4
  integer,parameter :: icache_psgp_olon = 5
  integer,parameter :: icache_psgp_tcf  = 6  ! rho factor
  integer,parameter :: icache_psgp_tol  = 7  !
  integer,parameter :: icache_psgp_maj  = 8  ! major
  integer,parameter :: icache_psgp_psf  = 9  ! scale factor at pole

  integer,parameter :: lim_psginv = OPT_PSGINV_ITER_LIMIT
!!!_  - static
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = EMU_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global

# define __MDL__ 'ugg'
!!!_  - interfaces
  interface get_longitude
     module procedure get_longitude_d
  end interface get_longitude
  interface mid_longitude
     module procedure mid_longitude_d
  end interface mid_longitude
  interface div_longitude
     module procedure div_longitude_d
  end interface div_longitude
  interface check_longitude
     module procedure check_longitude_d
  end interface check_longitude
  interface check_div_longitude
     module procedure check_div_longitude_d
  end interface check_div_longitude

  interface get_latitude
     module procedure get_latitude_d
  end interface get_latitude
  interface gauss_latitude
     module procedure gauss_latitude_d
  end interface gauss_latitude
  interface mid_latitude
     module procedure mid_latitude_d
  end interface mid_latitude
  interface div_latitude
     module procedure div_latitude_d
  end interface div_latitude

  interface array_reverse
     module procedure array_reverse_d
  end interface array_reverse

  interface check_precision
     module procedure check_precision_d, check_precision_f
  end interface check_precision

  interface is_equidistant
     module procedure is_equidistant_d
  end interface is_equidistant

  interface span_longitude
     module procedure span_longitude_d
  end interface span_longitude
  interface span_latitude
     module procedure span_latitude_d
  end interface span_latitude

  interface conformal_latitude
     module procedure conformal_latitude_d
  end interface conformal_latitude
  interface flatten_to_ecc
     module procedure flatten_to_ecc_d
  end interface flatten_to_ecc

  interface psgp_set
     module procedure psgp_set_d
  end interface psgp_set
  interface psgp_fwd
     module procedure psgp_fwd_core_d, psgp_fwd_once_d
  end interface psgp_fwd
  interface psgp_bwd_ll
     module procedure psgp_bwd_ll_d
  end interface psgp_bwd_ll
  interface psgp_bwd_tr
     module procedure psgp_bwd_tr_d
  end interface psgp_bwd_tr
  interface psgp_bwd_sf
     module procedure psgp_bwd_sf_d
  end interface psgp_bwd_sf
  interface psgp_bwd_isf
     module procedure psgp_bwd_isf_d
  end interface psgp_bwd_isf
  interface psgp_bwd_iaf
     module procedure psgp_bwd_iaf_d
  end interface psgp_bwd_iaf
  interface psgp_bwd_core
     module procedure psgp_bwd_core_d
  end interface psgp_bwd_core
  interface psgp_fwd_core
     module procedure psgp_fwd_core_d
  end interface psgp_fwd_core
  interface psgp_cachela
     module procedure psgp_cachela_d
  end interface psgp_cachela
  interface psgp_cachelo
     module procedure psgp_cachelo_d
  end interface psgp_cachelo

  interface pi_
     module procedure pi_d
  end interface pi_

  interface div_coordinate
     module procedure div_coordinate_d
  end interface div_coordinate
  interface deg2rad
     module procedure deg2rad_d
  end interface deg2rad
  interface rad2deg
     module procedure rad2deg_d
  end interface rad2deg

  interface reduced_lat_tan
     module procedure reduced_lat_tan_d
  end interface reduced_lat_tan
  interface reduced_lat
     module procedure reduced_lat_d
  end interface reduced_lat
  interface aux_dspherical_lon_cos
     module procedure aux_dspherical_lon_cos_d
  end interface aux_dspherical_lon_cos
  interface aux_dspherical_lon
     module procedure aux_dspherical_lon_d
  end interface aux_dspherical_lon
  interface aux_arcl_eetri_trig
     module procedure aux_arcl_eetri_trig_d
  end interface aux_arcl_eetri_trig
  interface aux_arcl_eetri
     module procedure aux_arcl_eetri_d
  end interface aux_arcl_eetri
  interface aux_azimuth1_etri_trig
     module procedure aux_azimuth1_etri_trig_d
  end interface aux_azimuth1_etri_trig
  interface aux_azimuth1_etri
     module procedure aux_azimuth1_etri_d
  end interface aux_azimuth1_etri

  interface azimuth0_eetri_trig
     module procedure azimuth0_eetri_trig_d
  end interface azimuth0_eetri_trig
  interface azimuth0_eetri
     module procedure azimuth0_eetri_d
  end interface azimuth0_eetri
  interface arcl_eetri_trig
     module procedure arcl_eetri_trig_d
  end interface arcl_eetri_trig
  interface arcl_eetri
     module procedure arcl_eetri_d
  end interface arcl_eetri
  interface sphlon_eetri_trig
     module procedure sphlon_eetri_trig_d
  end interface sphlon_eetri_trig
  interface sphlon_eetri
     module procedure sphlon_eetri_d
  end interface sphlon_eetri
  interface azimuth2cos_eetri_trig
     module procedure azimuth2cos_eetri_trig_d
  end interface azimuth2cos_eetri_trig
  interface azimuth2cos_eetri
     module procedure azimuth2cos_eetri_d
  end interface azimuth2cos_eetri
  interface expparam_trig
     module procedure expparam_trig_d
  end interface expparam_trig
  interface expparam
     module procedure expparam_d
  end interface expparam

  interface comp_elongi
     module procedure comp_elongi_d
  end interface comp_elongi
  interface gen_ctable_elongi
     module procedure gen_ctable_elongi_d
  end interface gen_ctable_elongi
  interface gen_vtable_elongi
     module procedure gen_vtable_elongi_d
  end interface gen_vtable_elongi
  interface gen_ctable_I1
     module procedure gen_ctable_I1_d
  end interface gen_ctable_I1
  interface gen_ctable_I2b
     module procedure gen_ctable_I2b_d
  end interface gen_ctable_I2b
  interface gen_ctable_I2a
     module procedure gen_ctable_I2a_d
  end interface gen_ctable_I2a

  interface geodesic_inverse_core
     module procedure geodesic_inverse_core_d
  end interface geodesic_inverse_core
  interface geodesic_inverse_canonical
     module procedure geodesic_inverse_canonical_d
  end interface geodesic_inverse_canonical
  interface geodesic_inverse_guess
     module procedure geodesic_inverse_guess_d
  end interface geodesic_inverse_guess
  interface comp_diff_xinteg
     module procedure comp_diff_xinteg_d
  end interface comp_diff_xinteg
  interface arcl_auxsph
     module procedure arcl_auxsph_d
  end interface arcl_auxsph

  interface nml_sincos
     module procedure nml_sincos_d
  end interface nml_sincos
  interface set_sincos
     module procedure set_sincos_d
  end interface set_sincos
  interface setd_sincos
     module procedure setd_sincos_d
  end interface setd_sincos

  interface sin_canonical
     module procedure sin_canonical_d
  end interface sin_canonical
  interface cos_canonical
     module procedure cos_canonical_d
  end interface cos_canonical

  interface diag_sc
     module procedure diag_sc_d
  end interface diag_sc
  interface diag_ph
     module procedure diag_ph_d
  end interface diag_ph

!!!_  - public
  public init, diag, finalize
  public get_longitude, mid_longitude,  div_longitude
  public check_longitude, check_div_longitude
  public get_latitude,  gauss_latitude, mid_latitude, div_latitude
  public check_precision, is_equidistant
  public deg2rad, rad2deg

  public flatten_to_ecc

  public psgp_set, psgp_cachela, psgp_cachelo
  public psgp_fwd
  public psgp_bwd_ll, psgp_bwd_tr
  public psgp_bwd_sf, psgp_bwd_isf, psgp_bwd_iaf

  public reduced_lat, reduced_lat_tan
  public aux_dspherical_lon, aux_dspherical_lon_cos
  public aux_arcl_eetri, aux_arcl_eetri_trig
  public aux_azimuth1_etri,  aux_azimuth1_etri_trig
  public azimuth0_eetri, azimuth0_eetri_trig
  public arcl_eetri, arcl_eetri_trig
  public sphlon_eetri, sphlon_eetri_trig
  public azimuth2cos_eetri, azimuth2cos_eetri_trig
  public expparam, expparam_trig
  public comp_elongi
  public gen_ctable_elongi, gen_vtable_elongi
  public geodesic_inverse_canonical, geodesic_inverse_guess
  public geodesic_inverse_core
  public gen_ctable_I1, gen_ctable_I2b, gen_ctable_I2a

  public set_sincos, setd_sincos
  public sin_canonical, cos_canonical

  public diag_sc, diag_ph

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv)
    use TOUZA_Std,only: control_mode, control_deep, is_first_force
    use TOUZA_Std,only: prc_init
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer,intent(in),optional :: stdv
    integer lv, md, lmd

    ierr = 0

    md = control_mode(mode, MODE_DEEPER)
    init_mode = md
    if (md.ge.MODE_SURFACE) then
       err_default = ERR_SUCCESS
       lv = choice(lev_verbose, levv)

       if (is_first_force(init_counts, mode)) then
          ulog = choice(ulog, u)
          lev_verbose = lv
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call prc_init(ierr, u=ulog, levv=stdv, mode=lmd)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init
!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std,only: control_mode, control_deep, is_first_force
    use TOUZA_Std,only: prc_diag
    use TOUZA_Std,only: choice
    use TOUZA_Std,only: is_msglev_NORMAL, is_msglev_DETAIL, msg_grp
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (is_msglev_NORMAL(lv)) then
             if (ierr.eq.0) call msg_grp(TIME_STAMP, __GRP__, __MDL__, utmp)
          endif
          if (is_msglev_DETAIL(lv)) then
             if (ierr.eq.0) call diag_pi(ierr, utmp)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call prc_diag(ierr, u=utmp, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std,only: control_mode, control_deep, is_first_force
    use TOUZA_Std,only: prc_finalize
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call prc_finalize(ierr, u=utmp, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize
!!!_  & diag_pi
  subroutine diag_pi(ierr, u)
    use TOUZA_Std,only: KDBL
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    call diag_pi_d(ierr, 0.0_KDBL, u)
  end subroutine diag_pi

  subroutine diag_pi_d(ierr, mold, u)
    use TOUZA_Std,only: KTGT=>KDBL
    use TOUZA_Std,only: choice
    use TOUZA_Std,only: is_msglev_NORMAL, msg_grp
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(in)          :: mold
    integer,        intent(in),optional :: u

    real(kind=KTGT) :: p0
    real(kind=KTGT) :: pp(3)
    integer j
    integer utmp
    character(len=128) :: txt

    real(kind=KTGT),parameter :: TWO   = 2.0_KTGT
    real(kind=KTGT),parameter :: ONE   = 1.0_KTGT
    real(kind=KTGT),parameter :: ZERO  = 0.0_KTGT
    real(kind=KTGT),parameter :: HALF  = 0.5_KTGT
    real(kind=KTGT),parameter :: QUAD  = 0.25_KTGT
    real(kind=KTGT),parameter :: OHALF = 1.5_KTGT
    real(kind=KTGT),parameter :: THREE = 3.0_KTGT
    real(kind=KTGT),parameter :: SIX   = 6.0_KTGT

    ierr = 0
    utmp = get_logu(u, ulog)

    p0 = pi_(mold)
    pp(1) = ATAN(1.0_KTGT) * 4.0_KTGT
    pp(2) = ATAN2(1.0_KTGT,  0.0_KTGT) * 2.0_KTGT
    pp(3) = ATAN2(0.0_KTGT, -1.0_KTGT)

    do j = 1, size(pp)
101    format('pi check/', I0, 1x, 2E24.16, 1x, E9.3)
102    format('pi check/', I0, ' passed')
       if (p0.eq.pp(j)) then
          write(txt, 102) j
       else
          write(txt, 101) j, p0, pp(j), p0 - pp(j)
       endif
       call msg_grp(txt, __GRP__, __MDL__, utmp)
    enddo
    call diag_trig_d(ierr, 'sin',  wsin_d, p0, HALF,  +ONE,  utmp)
    call diag_trig_d(ierr, 'sin',  wsin_d, p0, ONE,   ZERO, utmp)
    call diag_trig_d(ierr, 'sin',  wsin_d, p0, OHALF, -ONE, utmp)
    call diag_trig_d(ierr, 'sin',  wsin_d, p0, TWO,   ZERO, utmp)
    call diag_trig_d(ierr, 'sin',  wsin_d, p0, ONE/SIX,       +HALF, utmp)
    call diag_trig_d(ierr, 'sin',  wsin_d, p0, 5.0_KTGT/SIX,  +HALF, utmp)
    call diag_trig_d(ierr, 'sin',  wsin_d, p0, 7.0_KTGT/SIX,  -HALF, utmp)
    call diag_trig_d(ierr, 'sin',  wsin_d, p0, 11.0_KTGT/SIX, -HALF, utmp)

    call diag_trig_d(ierr, 'cos',  wcos_d, p0, HALF,  ZERO, utmp)
    call diag_trig_d(ierr, 'cos',  wcos_d, p0, ONE,   -ONE, utmp)
    call diag_trig_d(ierr, 'cos',  wcos_d, p0, OHALF, ZERO, utmp)
    call diag_trig_d(ierr, 'cos',  wcos_d, p0, TWO,   +ONE, utmp)
    call diag_trig_d(ierr, 'cos',  wcos_d, p0, ONE/THREE,      +HALF, utmp)
    call diag_trig_d(ierr, 'cos',  wcos_d, p0, TWO/THREE,      -HALF, utmp)
    call diag_trig_d(ierr, 'cos',  wcos_d, p0, 4.0_KTGT/THREE, -HALF, utmp)
    call diag_trig_d(ierr, 'cos',  wcos_d, p0, 5.0_KTGT/THREE, +HALF, utmp)

    call diag_trig_d(ierr, 'tan',  wtan_d, p0, QUAD,           +ONE,  utmp)
    call diag_trig_d(ierr, 'tan',  wtan_d, p0, QUAD*3.0_KTGT,  -ONE,  utmp)
    call diag_trig_d(ierr, 'tan',  wtan_d, p0, QUAD*5.0_KTGT,  +ONE,  utmp)
    call diag_trig_d(ierr, 'tan',  wtan_d, p0, QUAD*7.0_KTGT,  -ONE,  utmp)
    call diag_trig_d(ierr, 'tan',  wtan_d, p0, ONE,            ZERO,  utmp)
    call diag_trig_d(ierr, 'tan',  wtan_d, p0, TWO,            ZERO,  utmp)

  end subroutine diag_pi_d

  subroutine diag_trig_d(ierr, tag, f, p, c, z, u)
    use TOUZA_Std,only: KTGT=>KDBL
    use TOUZA_Std,only: choice
    use TOUZA_Std,only: msg_grp
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: tag
    real(kind=KTGT), intent(in)          :: p
    real(kind=KTGT), intent(in)          :: c
    real(kind=KTGT), intent(in)          :: z
    integer,         intent(in),optional :: u
    interface
       real(kind=KTGT) function f(x)
         use TOUZA_Std,only: KTGT=>KDBL
         implicit none
         real(kind=KTGT),intent(in) :: x
       end function f
    end interface

    integer utmp
    character(len=128) :: txt
    real(kind=KTGT) :: x, v, y
    integer,parameter :: n = 2
    real(kind=KTGT) :: vv(-n:n), yy(-n:n)
    integer j, jm

    ierr = 0
    utmp = get_logu(u, ulog)

! 101 format('check/', A, '(', F4.1, 'pi)', 1x, ES24.16, 1x, ES10.3, 1x, ES10.3)
102 format('check/', A, '(', F4.2, 'pi)', 1x, ' passed')
103 format('check/', A, '(', F4.2, 'pi)', 1x, SP, I0, 1x, ES24.16, 1x, ES24.16, 1x, ES10.3, 1x, '!!')
104 format('check/', A, '(', F4.2, 'pi)', 1x, SP, I0, 1x, ES24.16, 1x, ES24.16, 1x, ES10.3, 1x, '!')
105 format('check/', A, '(', F4.2, 'pi)', 1x, SP, I0, 1x, ES24.16, 1x, ES24.16, 1x, ES10.3)
    x = p * c
    y = x
    v = f(y)
    if (v.eq.z) then
       write(txt, 102) trim(tag), c
       call msg_grp(txt, __GRP__, __MDL__, utmp)
    else
       ! write(txt, 101) trim(tag), v, z, v - z
       ! call msg_grp(txt, __GRP__, __MDL__, utmp)
       y = x
       do j = -1, -n, -1
          y = NEAREST(y, -1.0_KTGT)
          yy(j) = y
          vv(j) = f(y)
       enddo
       y = x
       do j = 0, n
          yy(j) = y
          vv(j) = f(y)
          y = NEAREST(y, +1.0_KTGT)
       enddo
       j = 0
       write(txt, 105) trim(tag), c, j, yy(j), vv(j), vv(j) - z
       call msg_grp(txt, __GRP__, __MDL__, utmp)
       jm = MINLOC(abs(vv(-n:n)-z), 1) - n - 1
       if (jm.ne.0) then
          if (vv(jm).eq.z) then
             ! if exact answer found
             write(txt, 103) trim(tag), c, jm, yy(jm), vv(jm), vv(jm) - z
          else
             ! if minimum error found
             write(txt, 104) trim(tag), c, jm, yy(jm), vv(jm), vv(jm) - z
          endif
          call msg_grp(txt, __GRP__, __MDL__, utmp)
       endif
    endif
  end subroutine diag_trig_d
!!!_   . wsin()
  real(kind=KTGT) function wsin_d(x) result(z)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: x
    z = sin(x)
  end function wsin_d
!!!_   . wcos()
  real(kind=KTGT) function wcos_d(x) result(z)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: x
    z = cos(x)
  end function wcos_d
!!!_   . wtan()
  real(kind=KTGT) function wtan_d(x) result(z)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: x
    z = tan(x)
  end function wtan_d
!!!_   . deg2rad
  ELEMENTAL &
  real(kind=KTGT) function deg2rad_d(deg) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: deg
    real(kind=KTGT),parameter :: w = 360.0_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT) :: s
    s = sign(ONE, deg)
    ! r = s * (modulo(abs(deg), w) / (w / 2.0_KTGT)) * pi_(deg)
    r = s * (modulo(abs(deg), w) * (pi_(deg) / (w / 2.0_KTGT)))
  end function deg2rad_d
!!!_   . rad2deg
  ELEMENTAL &
  real(kind=KTGT) function rad2deg_d(rad) result(d)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: rad
    real(kind=KTGT),parameter :: w = 360.0_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT) :: p2, s
    p2 = 2.0_KTGT * pi_(rad)
    s = sign(ONE, rad)
    d = s * ((modulo(abs(rad), p2)) / p2) * w
  end function rad2deg_d

!!!_ + user subroutines (longitude)
!!!_  & get_longitude
  subroutine get_longitude_d &
       & (ierr,  longi, weight, &
       &  n,     div,   span,   wnml, org, acc)
!!!_   . note for MIROC compatibility
    !  Need set wnml = 1.0
    !  Need set div  = n * MINT
    !  Need set org  = 0.5 if OMIDGD
!!!_   . body
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(out)         :: longi(0:*)
    real(kind=KTGT),intent(out)         :: weight(0:*)
    integer,        intent(in)          :: n          ! array size
    integer,        intent(in),optional :: div        ! logical domain width (default n)
    real(kind=KTGT),intent(in),optional :: span       ! physical domain width (default: 2 pi)
    real(kind=KTGT),intent(in),optional :: wnml       ! normalization factor on weights, i.e, wnml == sum(weight) (default: span)
    real(kind=KTGT),intent(in),optional :: org        ! origin (in dx unit default, direct value if acc)
    logical,        intent(in),optional :: acc        ! accumulate switch (legacy)

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE  = 1.0_KTGT

    real(kind=KTGT) :: PI
    real(kind=KTGT) :: width, o, sp, wf
    integer j, lw

    ierr = 0

    PI = pi_(ONE)
    o = choice(ZERO, org)
    sp = span_longitude(span)
    wf = choice(ZERO, wnml)
    lw = choice(0, div)

    if (wf.eq.ZERO) wf = sp
    if (lw.le.0) lw = n

    width = real(lw, kind=KTGT)
    if (choice(.FALSE., acc)) then
       j = 0
       longi(j) = o
       do j = 1, n - 1
          longi(j) = longi(j - 1) + (sp / width)
       enddo
    else
       do j = 0, n - 1
          longi(j) = ((real(j, kind=KTGT) + o) / width) * sp
       enddo
    endif
    weight(0:n - 1) = wf / width

  end subroutine get_longitude_d
!!!_  & mid_longitude - (conditionaly) emulate MKLONM1
  subroutine mid_longitude_d &
       & (ierr,  longm,  &
       &  longi, weight, n,  wconv)
!!!_   . note
    !  Need set wconv = 2 PI or 0 to fully emulate MKLONM1
!!!_   . body
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(out)         :: longm(0:*)
    real(kind=KTGT),intent(in)          :: longi(0:*)
    real(kind=KTGT),intent(in)          :: weight(0:)
    integer,        intent(in)          :: n          ! array size
    real(kind=KTGT),intent(in),optional :: wconv      ! conversion factor on weights (default: 1)

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE  = 1.0_KTGT
    real(kind=KTGT),parameter :: HALF = 0.5_KTGT

    real(kind=KTGT) :: PI
    real(kind=KTGT) :: wc

    PI = pi_(ONE)

    ierr = 0
    wc = choice(ONE, wconv)   ! default == 1
    wc = span_longitude(wc)
    wc = wc * HALF

    longm(0:n-1) = longi(0:n-1) - weight(0:n-1) * wc
    longm(n)     = longi(n-1)   + weight(n-1)   * wc

  end subroutine mid_longitude_d

!!!_  & div_longitude
  subroutine div_longitude_d &
       & (ierr, longi, div, boundary, longi_c, longi_b, base, method, span)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(out)         :: longi(0:*)
    integer,        intent(in)          :: div           ! division number
    logical,        intent(in)          :: boundary      ! boundary or center
    real(kind=KTGT),intent(in)          :: longi_c(0:*)  ! source coordinate (center)     o o
    real(kind=KTGT),intent(in)          :: longi_b(0:*)  ! source coordinate (boundary)  x x x
    integer,        intent(in)          :: base          ! source array size
    integer,        intent(in),optional :: method
    real(kind=KTGT),intent(in),optional :: span          ! domain width
    real(kind=KTGT) :: sp
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT

    if (present(span)) then
       sp = span_longitude(span)
    else
       sp = ZERO
    endif
    call div_coordinate &
         & (ierr, longi, div, boundary, longi_c, longi_b, base, sp, method)
  end subroutine div_longitude_d

!!!_  - check_longitude
  subroutine check_longitude_d &
       & (ierr, longi, n, div, span, tag, u, tol)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    use TOUZA_Std,only: msg_grp
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KTGT), intent(out)         :: longi(0:*)
    integer,         intent(in)          :: n          ! array size
    integer,         intent(in),optional :: div        ! logical domain width (default n)
    real(kind=KTGT), intent(in),optional :: span       ! physical domain width (default: 2 pi)
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    real(kind=KTGT), intent(in),optional :: tol
    real(kind=KTGT) :: sp, refd, e
    integer utmp
    logical b
    character(len=128) :: ti, txt

    ierr = 0
    utmp = get_logu(u, ulog)
    ti = ' '
    if (present(tag)) then
       ti = tag
    endif
    if (ti.eq.' ') write(ti, '(I0)') n

101 format('check/longitude:', A, ': ', E10.3, ' / ', E10.3, 1x, L1)
102 format('check/longitude:', A, ': ', E10.3, 1x, L1)
    if (present(span)) then
       sp = span_longitude(span)
       refd = sp / real(choice(n, div), kind=KTGT)
       b = is_equidistant(longi, n, tol, e, r=refd)
       write(txt, 101) trim(ti), e, refd, b
    else
       b = is_equidistant(longi, n, tol, e)
       write(txt, 102) trim(ti), e, b
    endif
    call msg_grp(txt, __GRP__, __MDL__, utmp)
  end subroutine check_longitude_d
!!!_  - check_div_longitude
  subroutine check_div_longitude_d &
       & (ierr, longi, div, boundary, longi_c, longi_b, base, span, &
       &  tag,  u,     tol)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    use TOUZA_Std,only: msg_grp
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KTGT), intent(out)         :: longi(0:*)
    integer,         intent(in)          :: div           ! division number
    logical,         intent(in)          :: boundary      ! boundary or center
    real(kind=KTGT), intent(in)          :: longi_c(0:*)  ! source coordinate (center)     o o
    real(kind=KTGT), intent(in)          :: longi_b(0:*)  ! source coordinate (boundary)  x x x
    integer,         intent(in)          :: base          ! source array size
    real(kind=KTGT), intent(in),optional :: span          ! domain width
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    real(kind=KTGT), intent(in),optional :: tol

    integer utmp
    character(len=128) :: ti, txt
    integer mem, mwid
    real(kind=KTGT) :: x(0:div * base)

    ierr = 0
    utmp = get_logu(u, ulog)
    mwid = base * div
    mem = mwid
    if (boundary) mem = mem + 1
    ti = ' '
    if (present(tag)) then
       ti = tag
    endif
    if (ti.eq.' ') write(ti, '(I0, ''*'', I0)') base, div
    if (ierr.eq.0) then
       call check_longitude(ierr, longi, mem, mwid, span, ti, utmp, tol)
    endif
    if (ierr.eq.0) then
101    format('check/longitude/b:', A, ': ', E10.3)
102    format('check/longitude/c:', A, ': ', E10.3)
       if (boundary) then
          x(0:base) = longi_b(0:base) - longi(0:mwid:div)
          write(txt, 101) trim(ti), maxval(x(0:base)) - minval(x(0:base))
          call msg_grp(txt, __GRP__, __MDL__, utmp)

          if (mod(div, 2).eq.0) then
             x(0:base-1) = longi_c(0:base-1) - longi(div/2:mwid:div)
             write(txt, 102) trim(ti), maxval(x(0:base-1)) - minval(x(0:base-1))
             call msg_grp(txt, __GRP__, __MDL__, utmp)
          endif
       else
          if (mod(div, 2).eq.1) then
             x(0:base-1) = longi_c(0:base-1) - longi(div/2:mwid:div)
             write(txt, 102) trim(ti), maxval(x(0:base-1)) - minval(x(0:base-1))
             call msg_grp(txt, __GRP__, __MDL__, utmp)
          endif
       endif
    endif

  end subroutine check_div_longitude_d
!!!_ + user subroutines (latitude)
!!!_  & get_latitude - latitude computation with poles as boundaries
  subroutine get_latitude_d &
       & (ierr, lati,  weight, &
       &  n,    span,  wnml,   method)
    use TOUZA_Std,only: KTGT=>KDBL
    use TOUZA_Std,only: choice
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(out)         :: lati(*)
    real(kind=KTGT),intent(out)         :: weight(*)
    integer,        intent(in)          :: n
    real(kind=KTGT),intent(in),optional :: span       ! physical domain width (default: pi)
    real(kind=KTGT),intent(in),optional :: wnml       ! normalization factor on weights, i.e, wnml == sum(weight) (default: span)
    integer,        intent(in),optional :: method

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE  = 1.0_KTGT
    real(kind=KTGT),parameter :: HALF = 0.5_KTGT

    real(kind=KTGT) :: sp, wf
    real(kind=KTGT) :: PI
    real(kind=KTGT) :: width
    real(kind=KTGT) :: latn, lats
    integer j
    integer m

    ierr = ERR_PANIC
    m = choice(LAT_GAUSS_LEGACY, method)
    sp = span_latitude(span)
    wf = choice(ZERO, wnml)
    if (wf.eq.ZERO) wf = sp

    PI = pi_(ONE)

    select case(m)
    case(LAT_GAUSS,LAT_GAUSS_LEGACY)
       call gauss_latitude(ierr, lati, weight, n, sp, wf, method)
       if (ierr.eq.0) lati(1:n) = asin(lati(1:n))
       if (sp.ne.PI) lati(1:n) = (lati(1:n) / PI) * sp
    case(LAT_LINEAR)
       width = real(2 * n, kind=KTGT)
       do j = 1, (n - 1) / 2 + 1
          lati(j) = (real((n + 1 - 2 * j), KIND=KTGT) / width) * sp
          latn    = (real((n + 2 - 2 * j), KIND=KTGT) / width) * sp
          lats    = (real((n + 0 - 2 * j), KIND=KTGT) / width) * sp
          weight(j) = ((sin(latn) - sin(lats)) * HALF) * wf
       enddo
       do j = (n + 3) / 2, n
          lati(j)   = - lati(n - j + 1)
          weight(j) =   weight(n - j + 1)
       enddo
    case(LAT_LINEAR_COMPATIBLE)
       latn = HALF * PI
       ! to do: gurantee symmetric coordinate
       do j = 1, n
          lati(j)   = (HALF - (real(j, KIND=KTGT) - HALF) / real(n, kind=KTGT)) * sp
          lats      = latn - ONE / real(n, kind=KTGT) * sp
          weight(j) = ((sin(latn) - sin(lats)) * HALF) * wf
          latn = lats
       enddo
    case default
       ierr = ERR_INVALID_SWITCH
    end select
  end subroutine get_latitude_d

!!!_   & gauss_latitude - compute gauss latitudes (inherit gauss())
  subroutine gauss_latitude_d &
       & (ierr, CTHETA,  GW, NLATS, span, wnml, method, &
       &  prec, max_iter)
    use TOUZA_Std,only: KTGT=>KDBL
    use TOUZA_Std,only: choice
    use TOUZA_Std,only: msg_grp
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(out)         :: CTHETA(*)
    real(kind=KTGT),intent(out)         :: GW(*)
    integer,        intent(in)          :: NLATS
    real(kind=KTGT),intent(in),optional :: span       ! physical domain width (default: pi)
    real(kind=KTGT),intent(in),optional :: wnml       ! normalization factor on weights, i.e, wnml == sum(weight) (default: span)
    integer,        intent(in),optional :: method     ! reserved
    real(kind=KTGT),intent(in),optional :: PREC
    integer,        intent(in),optional :: max_iter

    real(kind=KTGT),allocatable :: QPN(:) !! Pn
    real(kind=KTGT),allocatable :: EPS(:)
    real(kind=KTGT) DELTP
    real(kind=KTGT) PI,  X0,   DELTX
    real(kind=KTGT) QDPN
    integer         N,   J,    ITER
    integer         ITRMAX
    character(len=1024) :: TMSG
    real(kind=KTGT),parameter :: ONE  = 1.0_KTGT
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT) :: sp, wf
    integer m

    ierr = 0

    m = choice(LAT_GAUSS_LEGACY, method)
    sp = span_latitude(span)
    wf = choice(ZERO, wnml)
    if (wf.eq.ZERO) wf = sp

    DELTP = choice(100.0_KTGT, prec)
    if (DELTP.ge.ONE) then
       DELTP = DELTP * check_precision()
    else if (DELTP.le.ZERO) then
       DELTP = check_precision(base=DELTP)
    endif
    ITRMAX = choice(0, max_iter)
    if (ITRMAX.le.0) ITRMAX = 50

    allocate(QPN(0:NLATS+1), EPS(0:NLATS+1), STAT=ierr)
    if (ierr.ne.0) then
       ierr = ERR_ALLOCATION
       CTHETA(1:NLATS) = ZERO
       GW(1:NLATS) = ZERO
    endif

    if (ierr.eq.0) then
       PI = pi_(ONE)
       if (m.eq.LAT_GAUSS_LEGACY) then
          do N = 1, NLATS + 1
             EPS(N) = SQRT((REAL(N, KIND=KTGT)**2) / (4.0_KTGT * REAL(N, KIND=KTGT)**2 - ONE))
          enddo
       else
          do N = 1, NLATS + 1
             EPS(N) = REAL(N, KIND=KTGT) / SQRT(4.0_KTGT * REAL(N, KIND=KTGT)**2 - ONE)
          enddo
       endif
       QPN(0) = ONE

       do J = 1, (NLATS + 1) / 2
          ! < 1. initial guess >
          X0 = COS((REAL(J, KTGT) - 0.5_KTGT) / REAL(NLATS, KTGT) * PI)
          do ITER = 1, ITRMAX
             ! < 2. calc. Pn >
             QPN(1) = SQRT(3.0_KTGT) * X0
             do N = 2, NLATS + 1
                QPN(N) = (QPN(N - 1) * X0 - QPN(N - 2) * EPS(N - 1)) / EPS(N)
             enddo
             ! < 3. calc. d/dmu Pn >
             N = NLATS
             QDPN = QPN(N + 1) * REAL(N, KTGT) * EPS(N + 1) - QPN(N - 1) * REAL(N + 1, KTGT) * EPS(N)
             ! < 4. solve by Newton method >
             DELTX = QPN(N) / QDPN * (ONE - X0 ** 2)
             X0    = X0 + DELTX
             IF (ABS(DELTX) .LT. DELTP) exit
          enddo
          if (ITER.GT.ITRMAX) then
101          format('no conversion = ', 2E24.16, 1x, I0, '/', I0)
             write(TMSG, 101) X0, DELTX, J, NLATS
             call msg_grp(TMSG, __GRP__, __MDL__, ulog)
          endif
          ! special treatment at equator when odd NLATS
          if (J.eq.(NLATS / 2 + 1)) X0 = 0.0_KTGT

          CTHETA(J) = X0
          ! < 5. Gaussian weight >
          GW(J) =   (REAL(2*NLATS, KTGT) - ONE) &
               &  * (ONE - X0 ** 2) &
               &  / (REAL(NLATS, KTGT) * QPN(NLATS-1))**2
       enddo
       do J = 1, NLATS / 2
          GW    (NLATS - J + 1) =   GW    ( J )
          CTHETA(NLATS - J + 1) = - CTHETA( J )
       enddo
    endif

    if (ierr.eq.0) GW(1:nlats) = wf * GW(1:nlats)

    if (ierr.eq.0) deallocate(QPN, EPS, STAT=ierr)
    return
  end subroutine gauss_latitude_d

!!!_  & mid_latitude - emulate mklatm1
  subroutine mid_latitude_d &
       & (ierr,   latm, &
       &  weight, n,    ini)
    use TOUZA_Std,only: KTGT=>KDBL
    use TOUZA_Std,only: choice
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(out)         :: latm(*)
    real(kind=KTGT),intent(in)          :: weight(*)
    integer,        intent(in)          :: n
    real(kind=KTGT),intent(in),optional :: ini

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE  = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO  = 2.0_KTGT

    real(kind=KTGT) :: L0
    integer j

    ierr = 0
    L0 = choice(ONE, ini)
    if (L0.eq.ONE) then
       latm(1) = L0
       do j = 1, (n + 1) / 2
          latm(j + 1) = latm(j) - weight(j) * TWO
       enddo
    else if (L0.eq.-ONE) then
       latm(1) = L0
       do j = 1, (n + 1) / 2
          latm(j + 1) = latm(j) + weight(j) * TWO
       enddo
    else
       ierr = ERR_PANIC
    endif
    if (ierr.eq.0) then
       if (MOD(n, 2).eq.0) latm(n / 2 + 1) = ZERO
       do j = 1, (n + 1) / 2
          latm(n - j + 2) = - latm(j)
       enddo
       if (ABS(L0).eq.ONE) then
          latm(1:n+1) = ASIN(latm(1:n+1))
       else
          ierr = ERR_PANIC
       endif
   endif

  end subroutine mid_latitude_d
!!!_  & div_latitude
  subroutine div_latitude_d &
       & (ierr, lati, div, boundary, lati_c, lati_b, base, method, span)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(out)         :: lati(0:*)
    integer,        intent(in)          :: div           ! division number
    logical,        intent(in)          :: boundary      ! boundary or center
    real(kind=KTGT),intent(in)          :: lati_c(0:*)   ! source coordinate (center)     o o
    real(kind=KTGT),intent(in)          :: lati_b(0:*)   ! source coordinate (boundary)  x x x
    integer,        intent(in)          :: base          ! source array size
    integer,        intent(in),optional :: method
    real(kind=KTGT),intent(in),optional :: span          ! domain width

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT) :: sp

    if (present(span)) then
       sp = span_latitude(span)
    else
       sp = ZERO
    endif
    call div_coordinate &
         & (ierr, lati, div, boundary, lati_c, lati_b, base, sp, method)

  end subroutine div_latitude_d

!!!_  & div_coordinate
  subroutine div_coordinate_d &
       & (ierr, co, div, boundary, cc, cb, base, span, method)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(out)         :: co(0:*)
    integer,        intent(in)          :: div           ! division number
    logical,        intent(in)          :: boundary      ! boundary or center
    real(kind=KTGT),intent(in)          :: cc(0:*)       ! source coordinate (center)     o o
    real(kind=KTGT),intent(in)          :: cb(0:*)       ! source coordinate (boundary)  x x x
    integer,        intent(in)          :: base          ! source array size
    real(kind=KTGT),intent(in),optional :: span          ! physical domain width (default: pi)
    integer,        intent(in),optional :: method

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE  = 1.0_KTGT
    real(kind=KTGT),parameter :: HALF = 0.5_KTGT
    integer m
    logical beq
    real(kind=KTGT) :: dxs, d
    real(kind=KTGT),allocatable :: dxd(:)
    integer jd, js, ji
    integer no
    real(kind=KTGT) :: o

    ierr = 0
    beq = .FALSE.

    if (ierr.eq.0) allocate(dxd(0:base-1), STAT=ierr)
    if (ierr.ne.0) ierr = ERR_ALLOCATION

    if (ierr.eq.0) then
       m = choice(DIV_EACH_EDGE, method)
       dxs = span / real(base, kind=KTGT)
       if (dxs.eq.ZERO) then
          beq = .FALSE.
       else if (is_equidistant(cb, base + 1, r=dxs)) then
          beq = .TRUE.
       else
          ierr = ERR_INVALID_PARAMETER
       endif
    endif

    if (ierr.eq.0) then
       if (beq) then
          dxd(0:base-1) = span / real(base * div, kind=KTGT)
       else
          do js = 0, base - 1
             dxs = cb(js + 1) - cb(js)
             dxd(js) = dxs / real(div, kind=KTGT)
          enddo
       endif

       select case(m)
       case(DIV_EACH_EDGE)
          if (boundary) then
             o = ZERO
             no = 0
             js = base
             jd = js * div + 0
             co(jd) = cb(js)
          else
             o = HALF
             no = 1
          endif
          do js = 0, base - 1
             do ji = 0, div / 2 - no
                jd = js * div + ji
                co(jd) = cb(js) + (real(ji, kind=KTGT) + o) * dxd(js)
             enddo
             do ji = div / 2 - no + 1, div - 1
                jd = js * div + ji
                co(jd) = cb(js + 1) - (real((div - ji), kind=KTGT) - o) * dxd(js)
             enddo
          enddo
          if (mod(div, 2).eq.no) then
             do js = 0, base - 1
                jd = js * div + div / 2
                co(jd) = cc(js)
             enddo
          endif
       case(DIV_EACH_INCR)
          if (boundary) then
             o = ZERO
             js = base
             jd = js * div + 0
             co(jd) = cb(js)
          else
             o = HALF
          endif
          do js = 0, base - 1
             do ji = 0, div - 1
                jd = js * div + ji
                co(jd) = cb(js) + (real(ji, kind=KTGT) + o) * dxd(js)
             enddo
          enddo
       case(DIV_ACCUMULATE)
          if (span.ne.ZERO) then
             ! compute again for compatibility
             d = dxs / real(div, kind=KTGT)
             if (boundary) then
                o = ZERO
                no = base * div + 1
             else
                o = HALF
                no = base * div
             endif
             jd = 0
             js = 0
             co(jd) = cb(js) + d * o
             do jd = 1, no - 1
                co(jd) = co(jd - 1) + d
             enddo
          else
             ierr = ERR_INVALID_PARAMETER
          endif
       case default
          ierr = ERR_INVALID_SWITCH
       end select
    endif
    if (ierr.eq.0) deallocate(dxd, STAT=ierr)
  end subroutine div_coordinate_d

!!!_  & array_reverse
  subroutine array_reverse_d &
       & (v, n)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(inout) :: v(*)
    integer,        intent(in)    :: n
    real(kind=KTGT) :: b(n)
    integer j
    do j = 1, n
       b (n - j + 1) = v(j)
    enddo
    v(1:n) = b(1:n)
  end subroutine array_reverse_d

!!!_ + polar stereographic projection
!!!_  - note
  ! Conformal latitude computation using Gudermannian function.
  ! See Karney(2011).
  subroutine psgp_set_d &
       & (cco, ecc, a, latts, lonorg, pole, xs, ys, tol)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    real(kind=KTGT),intent(out)         :: cco(*)
    real(kind=KTGT),intent(in)          :: ecc, a
    real(kind=KTGT),intent(in)          :: latts
    real(kind=KTGT),intent(in)          :: lonorg
    integer,        intent(in)          :: pole    ! north pole == +1, south pole == -1
    real(kind=KTGT),intent(in),optional :: xs, ys  ! scale for x,y (default == 1)
    real(kind=KTGT),intent(in),optional :: tol

    real(kind=KTGT),parameter :: ONE=1.0_KTGT, ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: TWO=2.0_KTGT
    real(kind=KTGT) :: s, d
    real(kind=KTGT) :: tcf, lt
    real(kind=KTGT) :: tglat, tclat, cc, e2c
    real(kind=KTGT) :: fc, c

    if (pole.lt.0) then
       cco(icache_psgp_sign) = - ONE
       lt = - latts
    else
       cco(icache_psgp_sign) = + ONE
       lt = latts
    endif

    ! 1 - f = sqrt(1-e^2)
    ! c = (1-f) * exp[e atanh (e)]
    fc = SQRT((ONE + ecc) * (ONE - ecc))
    c = fc * EXP(ecc * ATANH (ecc))

    tglat = TAN(lt)
    e2c = (ONE - ecc) * (ONE + ecc)
    d = SQRT(ONE + e2c * (tglat * tglat))
    tcf = ONE / d
    ! psf = c / TWO / d

    tclat = conformal_latitude(tglat, ecc)
    cc = _hypot(ONE, tclat) + ABS(tclat)
    if (tclat.lt.ZERO) then
       tcf = tcf / cc
       ! psf = psf / cc
    else
       tcf = tcf * cc
       ! psf = psf * cc
    endif

    cco(icache_psgp_tcf)  = tcf * a
    cco(icache_psgp_psf)  = tcf * (c / TWO)
    cco(icache_psgp_maj)  = a
    cco(icache_psgp_ecc)  = ecc
    cco(icache_psgp_olon) = lonorg

    s = choice(ONE, xs)
    if (s.le.ZERO) s = ONE
    cco(icache_psgp_xco)  = s

    s = choice(ONE, ys)
    if (s.le.ZERO) s = ONE
    cco(icache_psgp_yco)  = s

    cco(icache_psgp_tol) = choice(ONE, tol)

  end subroutine psgp_set_d

!!!_  & psgp_cachela - lat cache for sterographic projection (pole)
  subroutine psgp_cachela_d &
    & (cla, lat, cco)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out) :: cla(*)
    real(kind=KTGT),intent(in)  :: lat
    real(kind=KTGT),intent(in)  :: cco(*)

    real(kind=KTGT),parameter :: ONE=1.0_KTGT
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT) :: ecc
    real(kind=KTGT) :: tglat
    real(kind=KTGT) :: tclat, cc,     tcf

    ecc = cco(icache_psgp_ecc)
    tcf = cco(icache_psgp_tcf)

    if (abs(lat).eq.pi_(ZERO)/2.0_KTGT) then
       cla(icache_psgp_xrho) = ZERO
       cla(icache_psgp_yrho) = ZERO
    else
       tglat = TAN(lat) * cco(icache_psgp_sign)
       tclat = conformal_latitude(tglat, ecc)

       cc = _hypot(ONE, tclat) + ABS(tclat)
       if (tclat.lt.ZERO) then
          tcf = tcf * cc
       else
          tcf = tcf / cc
       endif
       tcf = tcf * cco(icache_psgp_sign)
       ! write(*, *) 'psgp:l', tglat, tclat, tcf, cco(icache_psgp_xco)

       cla(icache_psgp_xrho) = tcf / cco(icache_psgp_xco)
       cla(icache_psgp_yrho) = tcf / cco(icache_psgp_yco)
    endif
  end subroutine psgp_cachela_d

!!!_  & psgp_cachelo - lon cache for sterographic projection (pole)
  subroutine psgp_cachelo_d &
    & (clo, lon, cco)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out) :: clo(*)
    real(kind=KTGT),intent(in)  :: lon
    real(kind=KTGT),intent(in)  :: cco(*)

    real(kind=KTGT) :: lonorg
    real(kind=KTGT) :: dlo
    lonorg = cco(icache_psgp_olon)
    dlo = (lon - lonorg) * cco(icache_psgp_sign)
    clo(icache_psgp_sindlo) = sin_canonical(dlo)
    clo(icache_psgp_cosdlo) = cos_canonical(dlo)
  end subroutine psgp_cachelo_d

!!!_  & psgp_fwd_once()
  function psgp_fwd_once_d &
       & (lon, lat, cco) result (xy)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: xy(2)
    real(kind=KTGT),intent(in) :: lon, lat
    real(kind=KTGT),intent(in) :: cco(*)
    real(kind=KTGT) :: tlo(ncache_psgp_la)
    real(kind=KTGT) :: tla(ncache_psgp_lo)

    call psgp_cachela(tla, lat, cco)
    call psgp_cachelo(tlo, lon, cco)

    xy(:) = psgp_fwd_core_d(tlo, tla)
  end function psgp_fwd_once_d

!!!_  & psgp_fwd_core()
  function psgp_fwd_core_d &
       & (clo, cla) result (xy)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: xy(2)
    real(kind=KTGT),intent(in) :: clo(*)
    real(kind=KTGT),intent(in) :: cla(*)

    ! write(*, *) 'psgp:x', cla(icache_psgp_xrho), clo(icache_psgp_sindlo)
    xy(1) = + cla(icache_psgp_xrho) * clo(icache_psgp_sindlo)
    xy(2) = - cla(icache_psgp_yrho) * clo(icache_psgp_cosdlo)
  end function psgp_fwd_core_d

!!!_  & psgp_bwd_tr()
  subroutine psgp_bwd_tr_d &
       & (gla, dlo, x, y, cco)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out):: gla(*)
    real(kind=KTGT),intent(out):: dlo(*)
    real(kind=KTGT),intent(in) :: x, y
    real(kind=KTGT),intent(in) :: cco(*)

    real(kind=KTGT) :: s, d
    real(kind=KTGT) :: rho
    real(kind=KTGT) :: tglat
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: ONE=1.0_KTGT

    s   = cco(icache_psgp_sign)
    rho = _hypot(x, y)
    if (rho.eq.ZERO) then
       _SIN(gla) = s * ONE
       _COS(gla) = ZERO
       _SIN(dlo) = ZERO
       _COS(dlo) = ONE
    else
       tglat = psgp_bwd_core(rho, cco)
       d = _hypot(ONE, tglat)
       _SIN(gla) = s * tglat / d
       _COS(gla) = ONE / d

       _SIN(dlo) =  (x * s) / rho * s
       _COS(dlo) = -(y * s) / rho
    endif
  end subroutine psgp_bwd_tr_d

!!!_  & psgp_bwd_ll()
  function psgp_bwd_ll_d &
       & (x, y, cco) result(ll)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: ll(2)
    real(kind=KTGT),intent(in) :: x, y
    real(kind=KTGT),intent(in) :: cco(*)

    real(kind=KTGT) :: s, lo
    real(kind=KTGT) :: rho
    real(kind=KTGT) :: tglat
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT

    s   = cco(icache_psgp_sign)
    rho = _hypot(x, y)
    if (rho.eq.ZERO) then
       ll(2) = s * (pi_(ZERO) / 2.0_KTGT)
       ll(1) = cco(icache_psgp_olon)
    else
       tglat = psgp_bwd_core(rho, cco)

       ll(2) = s * ATAN(tglat)

       lo = s * cco(icache_psgp_olon) + ATAN2(x * s, - y * s)
       ll(1) = lo * s
    endif
  end function psgp_bwd_ll_d

!!!_  & psgp_bwd_sf() - scale factor
  real(kind=KTGT) function psgp_bwd_sf_d &
       & (x, y, cco) result(sf)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: x, y
    real(kind=KTGT),intent(in) :: cco(*)

    real(kind=KTGT) :: ecc, e2c
    real(kind=KTGT) :: rho
    real(kind=KTGT) :: tglat
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: ONE=1.0_KTGT

    rho = _hypot(x, y)
    if (rho.eq.ZERO) then
       sf = cco(icache_psgp_psf)
    else
       ecc = cco(icache_psgp_ecc)
       e2c = (ONE - ecc) * (ONE + ecc)
       tglat = psgp_bwd_core(rho, cco)
       sf = (rho / cco(icache_psgp_maj)) * SQRT(ONE + e2c * (tglat * tglat))
    endif
  end function psgp_bwd_sf_d

!!!_  & psgp_bwd_isf() - inverse scale factor
  real(kind=KTGT) function psgp_bwd_isf_d &
       & (x, y, cco) result(sf)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: x, y
    real(kind=KTGT),intent(in) :: cco(*)

    real(kind=KTGT) :: ecc, e2c
    real(kind=KTGT) :: rho
    real(kind=KTGT) :: tglat
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: ONE=1.0_KTGT

    rho = _hypot(x, y)
    if (rho.eq.ZERO) then
       sf = ONE / cco(icache_psgp_psf)
    else
       ecc = cco(icache_psgp_ecc)
       e2c = (ONE - ecc) * (ONE + ecc)
       tglat = psgp_bwd_core(rho, cco)
       sf = (cco(icache_psgp_maj) / rho) / SQRT(ONE + e2c * (tglat * tglat))
    endif
  end function psgp_bwd_isf_d

!!!_  & psgp_bwd_iaf() - inverse area factor
  real(kind=KTGT) function psgp_bwd_iaf_d &
       & (x, y, cco) result(sf)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: x, y
    real(kind=KTGT),intent(in) :: cco(*)

    real(kind=KTGT) :: ecc, e2c
    real(kind=KTGT) :: rho
    real(kind=KTGT) :: tglat
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: ONE=1.0_KTGT

    rho = _hypot(x, y)
    if (rho.eq.ZERO) then
       sf = ONE / cco(icache_psgp_psf)
    else
       ecc = cco(icache_psgp_ecc)
       e2c = (ONE - ecc) * (ONE + ecc)
       tglat = psgp_bwd_core(rho, cco)
       sf = (cco(icache_psgp_maj) / rho) ** 2 / (ONE + e2c * (tglat * tglat))
    endif
  end function psgp_bwd_iaf_d

!!!_  & psgp_bwd_core
  real(kind=KTGT) function psgp_bwd_core_d &
       & (rho, cco) result(tlat)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: rho
    real(kind=KTGT),intent(in) :: cco(*)

    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: ONE=1.0_KTGT
    real(kind=KTGT),parameter :: TWO=2.0_KTGT
    real(kind=KTGT) :: ecc, tol, e2c
    real(kind=KTGT) :: t
    real(kind=KTGT) :: tclat, tglat
    real(kind=KTGT) :: z, dz
    ! real(kind=KTGT) :: scf
    integer j

    if (rho.eq.ZERO) then
       tlat = cco(icache_psgp_sign) * HUGE(ZERO)
    else
       tol = cco(icache_psgp_tol)
       ecc = cco(icache_psgp_ecc)
       e2c = (ONE - ecc) * (ONE + ecc)

       t = rho / cco(icache_psgp_tcf)
       tclat = (ONE / t - t) / TWO
       ! tglat = tclat
       ! Taylor expansion of conformal latitude with e is
       !   tclat = tglat - e^2 tglat + ....,
       ! thus use tglat = tclat/ (1-e^2) as initial guess
       tglat = tclat / e2c
       do j = 0, lim_psginv
          z = conformal_latitude(tglat, ecc)
          dz = (tclat - z) * (ONE + e2c * (tglat * tglat)) &
               & / (e2c * _hypot(ONE, z) * _hypot(ONE, tglat))
          tglat = tglat + dz
          if (abs(dz).le.spacing(tglat) * tol) exit
       enddo
       tlat = tglat
       ! scf = (rho / cco(icache_psgp_maj)) * SQRT(ONE + e2c * (tglat * tglat))
       ! write(*, *) 'scale =', scf, tglat, tclat, rho, cco(icache_psgp_maj)
    endif
  end function psgp_bwd_core_d

!!!_  & flatten_to_ecc ()
  ELEMENTAL &
  real(kind=KTGT) function flatten_to_ecc_d (f) result (e)
    use TOUZA_Std,only: KTGT=>KDBL
    real(kind=KTGT),intent(in) :: f
    e = sqrt(2.0_KTGT * f - f * f)
  end function flatten_to_ecc_d

!!!_  & conformal_latitude() - tan(conf. lat)
  ELEMENTAL &
  real(kind=KTGT) function conformal_latitude_d (tglat, ecc) result (x)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: tglat ! tan(geod.lat)
    real(kind=KTGT),intent(in) :: ecc
    real(kind=KTGT) :: seglat, gd
    real(kind=KTGT),parameter :: ONE  = 1.0_KTGT

    seglat = _hypot(ONE, tglat)
    gd = SINH(ecc * ATANH(ecc * tglat / seglat))
    x = tglat * _hypot(ONE, gd) - gd * seglat

    return
  end function conformal_latitude_d

!!!_ + geodesy
!!!_  !  notes
  !   algorithms based on Karney (2013, 2022)

  !   glat:   (phi)     geographic latitude
  !   glon:   (lambda)  longitude
  !   azim:   (alpha)   azimuth of the geodesic
  !   eazim:  (alpha_0) azimuth at the node
  !   gdis:   (s)       distance along the geodesic
  !   plat:   (beta)    parametric latitude
  !   alon:   (omega)   longitude on the auxiliary sphere
  !   aarc:   (sigma)   arc length on the auxiliary sphere
  !   earea:  (S)       the area between the geodesic and the equator

  !   node == the point where the geodesic crosses the equator

!!!_  & geodesic_inverse
  subroutine geodesic_inverse_core_d &
       & (ierr,  &
       &  gdis,  &
       &  glat1, glat2, dglon, &
       &  inia1, f,     a,     rtol,  atol, liter)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    use TOUZA_Std,only: msg_grp
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(out)         :: gdis
    real(kind=KTGT),intent(in)          :: glat1(2)      ! sin, cos
    real(kind=KTGT),intent(in)          :: glat2(2)      ! sin, cos
    real(kind=KTGT),intent(in)          :: dglon(3)      ! sin, cos, rad
    real(kind=KTGT),intent(in)          :: inia1(2)      ! initial guess of azimuth[1]
    real(kind=KTGT),intent(in)          :: f, a
    real(kind=KTGT),intent(in),optional :: rtol, atol    ! tolerances
    integer,        intent(in),optional :: liter

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT

    real(kind=KTGT) :: eazim(2)                  ! alpha_0
    real(kind=KTGT) :: azim1(2), azim2(2)        ! alpha
    real(kind=KTGT) :: plat1(2), plat2(2)        ! beta
    real(kind=KTGT) :: aarc1(2), aarc2(2)        ! sigma
    real(kind=KTGT) :: alon1_ph(2), alon2_ph(2)  ! omega

    real(kind=KTGT) :: dpx, dpy, dcx, dcy
    real(kind=KTGT) :: daarc
    real(kind=KTGT) :: ds(2), ss(2)

    real(kind=KTGT) :: ee, ep2, eps, kk, fiii
    real(kind=KTGT) :: nazim(2)

    integer jo
    integer,parameter :: lodr = 9
    integer,parameter :: i3odr = 5
    integer,parameter :: i1odr = 6, i2odr = 6
    integer,parameter :: jjodr = min(i1odr, i2odr)
    real(kind=KTGT) :: C3C(0:lodr, 0:lodr)
    real(kind=KTGT) :: C3(0:lodr)
    real(kind=KTGT) :: C2(0:lodr), C1(0:lodr)
    real(kind=KTGT) :: JJ(0:lodr)

    real(kind=KTGT) :: dI3, dJJ
    real(kind=KTGT) :: AA(2,2)
    real(kind=KTGT) :: F1(2)

    real(kind=KTGT) :: ddx, ddy, dsol
    real(kind=KTGT) :: bf1, bf2
    real(kind=KTGT) :: mob

    real(kind=KTGT) :: etol, dtol, echk
    integer miter

    integer iter

    ierr = 0

    miter = choice(0, liter)
    if (miter.le.0) miter = 16

    etol = choice(ONE, rtol)
    if (etol.le.ZERO) etol = 1.0_KTGT
    if (etol.ge.ONE) etol = (etol * epsilon(ZERO)) * ATAN2(_SIN(dglon), _COS(dglon))

    dtol = choice(ZERO, atol)
    if (dtol.eq.ZERO) dtol = -4.0_KTGT    ! 0.1mm
    if (dtol.lt.ZERO) dtol = (10.0_KTGT ** dtol) / a

    echk = epsilon(ZERO)

    fiii = f / (TWO - f)
    ee = f * (TWO - f)
    ep2 = ee / (ONE - ee)
    call gen_ctable_elongi(ierr, C3C, i3odr, fiii)

    azim1(1:2) = inia1(1:2)
    plat1(1:2) = nml_sincos(_SIN(glat1) * (ONE - f), _COS(glat1))
    plat2(1:2) = nml_sincos(_SIN(glat2) * (ONE - f), _COS(glat2))

    if (_SIN(glat1).eq.ZERO) then
       ! equatorial
       if (abs(_ANGLE(dglon)).gt.(ONE - f) * pi_(ZERO)) then
          ierr = ERR_NOT_IMPLEMENTED
          call msg_grp('Cannot handle equatorial antipodal case', __GRP__, __MDL__)
          return
       endif
       gdis = a * abs(_ANGLE(dglon))
    else
       do iter = 0, miter - 1
          ! NEA
          if (_COS(azim1).eq.ZERO.and._SIN(plat1).eq.ZERO) then
             _SIN(eazim) = ONE
             _COS(eazim) = ZERO
          else
             _SIN(eazim) = _SIN(azim1) * _COS(plat1)
             _COS(eazim) = _hypot(_COS(azim1), _SIN(azim1) * _SIN(plat1))
          endif
          aarc1(1:2) = arcl_auxsph(azim1, plat1)
          alon1_ph(1) = _SIN(plat1) * _SIN(eazim)
          alon1_ph(2) = _COS(azim1) * _COS(plat1)
          ! NEB
          ! to check when _COS(plat2)==0 (pole)
          _SIN(azim2) = _SIN(eazim) / _COS(plat2)
          _COS(azim2) = SQRT((_COS(azim1) * _COS(plat1))**2 &
               &             +(_COS(plat2) - _COS(plat1)) * (_COS(plat2) + _COS(plat1))) &
               &         / _COS(plat2)
          aarc2(1:2) = arcl_auxsph(azim2, plat2)
          alon2_ph(1) = _SIN(plat2) * _SIN(eazim)
          alon2_ph(2) = _COS(azim2) * _COS(plat2)

          ! omega2 - omega1 phase
          dpy = alon1_ph(2) * alon2_ph(1) - alon1_ph(1) * alon2_ph(2)
          dpx = alon1_ph(2) * alon2_ph(2) + alon1_ph(1) * alon2_ph(1)
          ! omega2 - omega1 - (lambda2 - lambda1) phase
          dcy = dpy * _COS(dglon) - dpx * _SIN(dglon)
          dcx = dpx * _COS(dglon) + dpy * _SIN(dglon)
          ddx = atan2(dcy, dcx)

          kk = ep2 * (_COS2(eazim))
          eps = kk / ((ONE + sqrt(ONE + kk)) * TWO + kk)

          call gen_vtable_elongi(ierr, C3, C3C, i3odr, eps)

          _SIN(ds) = _COS(aarc1) * _SIN(aarc2) - _SIN(aarc1) * _COS(aarc2)
          _COS(ds) = _COS(aarc1) * _COS(aarc2) + _SIN(aarc1) * _SIN(aarc2)
          daarc = ATAN2(_SIN(ds), _COS(ds))

          _SIN(ss) = _COS(aarc1) * _SIN(aarc2) + _SIN(aarc1) * _COS(aarc2)
          _COS(ss) = _COS(aarc1) * _COS(aarc2) - _SIN(aarc1) * _SIN(aarc2)

          AA(1,1) = + (_COS(ds) * _COS(ss)) * TWO
          AA(2,2) = AA(1,1)
          AA(2,1) = - ((_SIN(ds) * _SIN(ss)) * TWO)
          AA(1,2) = AA(2,1)
          F1(1) = _COS(ds) * _SIN(ss)
          F1(2) = _SIN(ds) * _COS(ss)

          call comp_diff_xinteg(dI3, C3, i3odr, AA, F1)
          dI3 = f * C3(0) * _SIN(eazim) * (dI3 + daarc)

          ddx = ddx - dI3
          ! iteration
          call gen_ctable_I1(ierr, C1, i1odr, eps)
          call gen_ctable_I2b(ierr, C2, i2odr, eps)
          do jo = 1, jjodr
             JJ(jo) = (C1(0) + ONE) * C1(jo) - (C2(0) + ONE) * C2(jo)
          enddo
          JJ(0) = C1(0) - C2(0)

          call comp_diff_xinteg(dJJ, JJ, jjodr, AA, F1)
          dJJ = dJJ + daarc * JJ(0)

          bf1 = sqrt(ONE + ep2 * (_COS(eazim) * _SIN(aarc1)) ** 2)
          bf2 = sqrt(ONE + ep2 * (_COS(eazim) * _SIN(aarc2)) ** 2)

          mob = bf2 * _COS(aarc1) * _SIN(aarc2) - bf1 * _SIN(aarc1) * _COS(aarc2) &
               & - (_COS(aarc1) * _COS(aarc2)) * dJJ

          ddy = (mob * (ONE - f)) / (_COS(azim2) * _COS(plat2))

          dsol = - ddx / ddy

          if (abs(ddx).le.etol) exit
          if (abs(ddx).le.echk.and.abs(dsol).le.dtol) exit

          dcx = cos(dsol)
          dcy = sin(dsol)

          _SIN(nazim) = _COS(azim1) * dcy + _SIN(azim1) * dcx
          _COS(nazim) = _COS(azim1) * dcx - _SIN(azim1) * dcy

          azim1(1:2) = nml_sincos(_SIN(nazim), _COS(nazim))
       enddo
       call comp_diff_xinteg(gdis, C1, i1odr, AA, F1)
       gdis = (gdis + daarc) * (C1(0) + ONE)
       gdis = gdis * (a * (ONE - f))
    endif
  end subroutine geodesic_inverse_core_d

!!!_  & geodesic_inverse_canonical
  subroutine geodesic_inverse_canonical_d &
       & (ierr,  &
       &  glat1, glat2, dglon)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    integer,        intent(out)   :: ierr
    real(kind=KTGT),intent(inout) :: glat1(*)      ! sin, cos
    real(kind=KTGT),intent(inout) :: glat2(*)      ! sin, cos
    real(kind=KTGT),intent(inout) :: dglon(*)      ! sin, cos

    real(kind=KTGT),parameter :: ONE = 1.0_KTGT

    real(kind=KTGT) :: xlat1(2), xlat2(2)

    ierr = 0

    if (ABS(_SIN(glat1)).gt.ABS(_SIN(glat2))) then
       xlat1(1:2) = glat1(1:2)
       xlat2(1:2) = glat2(1:2)
    else
       xlat2(1:2) = glat1(1:2)
       xlat1(1:2) = glat2(1:2)
    endif
    _SIN(xlat2) = _SIN(xlat2) * (- SIGN(ONE, _SIN(xlat1)))
    _SIN(xlat1) = - ABS(_SIN(xlat1))

    glat1(1:2) = xlat1(1:2)
    glat2(1:2) = xlat2(1:2)

    _SIN(dglon) = ABS(_SIN(dglon))
  end subroutine geodesic_inverse_canonical_d

!!!_  & geodesic_inverse_guess
  subroutine geodesic_inverse_guess_d &
       & (ierr,  inia1, &
       &  glat1, glat2, dglon, &
       &  f)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: inia1(2)      ! initial guess of azimuth[1]
    real(kind=KTGT),intent(in)  :: glat1(2)      ! sin, cos
    real(kind=KTGT),intent(in)  :: glat2(2)      ! sin, cos
    real(kind=KTGT),intent(in)  :: dglon
    real(kind=KTGT),intent(in)  :: f

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT

    real(kind=KTGT) :: plat1(2), plat2(2)        ! beta
    real(kind=KTGT) :: e2
    real(kind=KTGT) :: wd
    real(kind=KTGT) :: dalon(2) ! omega
    real(kind=KTGT) :: zx, zy, zz

    ierr = 0

    plat1(1:2) = nml_sincos(_SIN(glat1) * (ONE - f), _COS(glat1))
    plat2(1:2) = nml_sincos(_SIN(glat2) * (ONE - f), _COS(glat2))

    e2 = f * (TWO - f)
    wd = sqrt(ONE - e2 * ((_COS(plat1) + _COS(plat2)) / TWO)**2)
    ! write(*, *) 'guess: ', wd, rad2deg(dglon / wd)

    _SIN(dalon) = sin(dglon / wd)
    _COS(dalon) = cos(dglon / wd)

    zx = _COS(plat1) * _SIN(plat2) - _SIN(plat1) * _COS(plat2) * _COS(dalon)
    zy = _COS(plat2) * _SIN(dalon)
    zz = _hypot(zx, zy)

    _SIN(inia1) = zy / zz
    _COS(inia1) = zx / zz

    ! write(*, *) _SIN(glat1), dglon, (ONE - f) * pi_(ZERO)
  end subroutine geodesic_inverse_guess_d

!!!_  - comp_diff_xinteg
  subroutine comp_diff_xinteg_d &
       & (z, C, modr, AA, F1)
    use TOUZA_Std,only: KTGT=>KDBL
    real(kind=KTGT),intent(out) :: z
    integer,        intent(in)  :: modr
    real(kind=KTGT),intent(in)  :: C(0:*)
    real(kind=KTGT),intent(in)  :: AA(2, 2)
    real(kind=KTGT),intent(in)  :: F1(2)

    real(kind=KTGT) :: B0(2,2), B1(2,2), B2(2,2)
    integer jo
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT

    B1(1:2, 1:2) = ZERO
    B2(1:2, 1:2) = ZERO
    do jo = modr, 1, -1
       B0(1,1) = C(jo) + ((AA(1,1) * B1(1,1) + AA(2,1) * B1(1,2)) - B2(1,1))
       B0(2,2) = C(jo) + ((AA(1,2) * B1(2,1) + AA(2,2) * B1(2,2)) - B2(2,2))
       B0(2,1) =         ((AA(1,1) * B1(2,1) + AA(2,1) * B1(2,2)) - B2(2,1))
       B0(1,2) =         ((AA(1,2) * B1(1,1) + AA(2,2) * B1(1,2)) - B2(1,2))
       B2(1:2,1:2) = B1(1:2,1:2)
       B1(1:2,1:2) = B0(1:2,1:2)
    enddo
    z = (B1(1,2) * F1(1) + B1(2,2) * F1(2)) * TWO
  end subroutine comp_diff_xinteg_d

!!!_  & arcl_auxsph()
  function arcl_auxsph_d &
       & (azim, plat) &
       & result (aarc)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: azim(*)
    real(kind=KTGT),intent(in) :: plat(*)
    real(kind=KTGT) :: aarc(2)
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT

    if (_COS(azim).eq.ZERO.and._SIN(plat).eq.ZERO) then
       _SIN(aarc) = ZERO  ! from _SIN(plat1) == 0
       _COS(aarc) = ONE
    else
       aarc(1:2)  = nml_sincos(_SIN(plat), _COS(azim) * _COS(plat))
    endif
  end function arcl_auxsph_d

!!!_  & diag_sc
  subroutine diag_sc_d(tag, sc)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    character(len=*),intent(in) :: tag
    real(kind=KTGT), intent(in) :: sc(*)

    real(kind=KTGT) :: arad, adeg

    arad = atan2(_SIN(sc), _COS(sc))
    adeg = rad2deg(arad)

101 format('sincos:', A, ': ', ES24.16, 1x, E10.3, ' (', ES24.16, 1x, ES24.16, ')')
    write(*, 101) trim(tag), adeg, arad, sc(1), sc(2)

  end subroutine diag_sc_d

!!!_  & diag_ph
  subroutine diag_ph_d(tag, ph)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    character(len=*),intent(in) :: tag
    real(kind=KTGT), intent(in) :: ph(*)

    real(kind=KTGT) :: sc(2)
    real(kind=KTGT) :: arad, adeg

    sc = nml_sincos(ph(1), ph(2))
    arad = atan2(sc(1), sc(2))
    adeg = rad2deg(arad)

101 format('phase:', A, ': ', ES24.16, 1x, E10.3, ' (', E10.3, 1x, E10.3, ')')
    write(*, 101) trim(tag), adeg, arad, sc(1), sc(2)

  end subroutine diag_ph_d

!!!_  & set_sincos() - set sine and cosine array
  function set_sincos_d(angl) &
       & result(sc)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: angl
    real(kind=KTGT) :: sc(2)
    real(kind=KTGT) r, a, q, s, c
    integer n

    q = pi_(angl) / 2.0_KTGT
    a = ANINT(angl / q)
    r = angl - q * a
    n = modulo(int(a), 4)
    s = sin(r)
    c = cos(r)
    select case(n)
    case(0)
       _SIN(sc) = +s
       _COS(sc) = +c
    case(1)
       _SIN(sc) = +c
       _COS(sc) = -s
    case(2)
       _SIN(sc) = -s
       _COS(sc) = -c
    case default
       _SIN(sc) = -c
       _COS(sc) = +s
    end select
  end function set_sincos_d

!!!_  & sin_canonical()
  real(kind=KTGT) function sin_canonical_d (angl) result(v)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: angl

    real(kind=KTGT) r, a, q
    integer n

    q = pi_(angl) / 2.0_KTGT
    a = ANINT(angl / q)
    r = angl - q * a
    n = modulo(int(a), 4)
    select case(n)
    case(0)
       v = + sin(r)
    case(1)
       v = + cos(r)
    case(2)
       v = - sin(r)
    case default
       v = - cos(r)
    end select
  end function sin_canonical_d

!!!_  & cos_canonical()
  real(kind=KTGT) function cos_canonical_d (angl) result(v)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: angl

    real(kind=KTGT) r, a, q
    integer n

    q = pi_(angl) / 2.0_KTGT
    a = ANINT(angl / q)
    r = angl - q * a
    n = modulo(int(a), 4)
    select case(n)
    case(0)
       v = + cos(r)
    case(1)
       v = - sin(r)
    case(2)
       v = - cos(r)
    case default
       v = + sin(r)
    end select
  end function cos_canonical_d

!!!_  & setd_sincos() - set sine and cosine array (degree)
  function setd_sincos_d(angl) &
       & result(sc)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: angl
    real(kind=KTGT) :: sc(2)
    real(kind=KTGT) r, a, q, s, c
    integer n

    q = 90.0_KTGT
    a = ANINT(angl / q)
    r = angl - q * a
    n = modulo(int(a), 4)
    s = sin(deg2rad(r))
    c = cos(deg2rad(r))
    write(*, *) 'setd: ', r, deg2rad(r), n, s, c
    select case(n)
    case(0)
       _SIN(sc) = s
       _COS(sc) = c
    case(1)
       _SIN(sc) = c
       _COS(sc) = -s
    case(2)
       _SIN(sc) = -s
       _COS(sc) = -c
    case default
       _SIN(sc) = -c
       _COS(sc) = s
    end select
  end function setd_sincos_d

!!!_  & nml_sincos() - normalize to compute sine and cosine
  function nml_sincos_d(py, px) &
       & result(sc)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: py, px
    real(kind=KTGT) :: sc(2)
    real(kind=KTGT) :: d

    d = _hypot(px, py)

    _SIN(sc) = py / d
    _COS(sc) = px / d
  end function nml_sincos_d

!!!_  & reduced_lat_tan() -  tan(beta)
  ELEMENTAL &
  real(kind=KTGT) function reduced_lat_tan_d(lat, f) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: lat ! in radian
    real(kind=KTGT),intent(in) :: f   ! flattening
    r = (1.0_KTGT - f) * tan(lat)
  end function reduced_lat_tan_d
!!!_  & reduced_lat() -  beta
  ELEMENTAL &
  real(kind=KTGT) function reduced_lat_d(lat, f) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: lat ! in radian
    real(kind=KTGT),intent(in) :: f   ! flattening
    r = atan(reduced_lat_tan(lat, f))
  end function reduced_lat_d

!!!_  & aux_dspherical_lon_cos - omega_12
  ELEMENTAL &
  real(kind=KTGT) function aux_dspherical_lon_cos_d(dlon, cb1, cb2, e2) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: dlon ! lambda_12 in radian
    real(kind=KTGT),intent(in) :: cb1  ! cos beta_1
    real(kind=KTGT),intent(in) :: cb2  ! cos beta_2
    real(kind=KTGT),intent(in) :: e2   ! e^2
    real(kind=KTGT) :: wt ! tilde omega
    wt = sqrt(1.0_KTGT - (((cb1 + cb2) * 0.5_KTGT)**2) * e2)
    r  = dlon / wt
  end function aux_dspherical_lon_cos_d
!!!_  & aux_dspherical_lon - omega_12
  ELEMENTAL &
  real(kind=KTGT) function aux_dspherical_lon_d(dlon, b1, b2, e2) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: dlon ! lambda_12 in radian
    real(kind=KTGT),intent(in) :: b1   ! beta_1 in radian
    real(kind=KTGT),intent(in) :: b2   ! beta_2
    real(kind=KTGT),intent(in) :: e2   ! e^2
    r = aux_dspherical_lon_cos(dlon, cos(b1), cos(b2), e2)
  end function aux_dspherical_lon_d

!!!_  & aux_arcl_eetri_trig - sigma_12, spher. arc length of the third side of the elem. ellips. triangle [NEP]
  ELEMENTAL &
  real(kind=KTGT) function aux_arcl_eetri_trig_d &
       & (sb1, cb1, sb2, cb2, sw12, cw12) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: sb1,  cb1      ! sin,cos beta_1
    real(kind=KTGT),intent(in) :: sb2,  cb2      ! sin,cos beta_2
    real(kind=KTGT),intent(in) :: sw12, cw12     ! sin,cos omega_12

    real(kind=KTGT) :: z1r, z1i
    real(kind=KTGT) :: y,   x

    z1r = cb1 * sb2 - sb1 * cb2 * cw12
    z1i = cb2 * sw12
    y = _hypot(z1r, z1i)
    x = sb1 * sb2 + cb1 * cb2 * cw12
    r = atan2(y, x)
  end function aux_arcl_eetri_trig_d

!!!_  & aux_arcl_eetri - sigma_12, spher. arc length of the third side of the elem. ellips. triangle [NEP]
  ELEMENTAL &
  real(kind=KTGT) function aux_arcl_eetri_d &
       & (b1, b2, w12) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: b1  ! in radian
    real(kind=KTGT),intent(in) :: b2
    real(kind=KTGT),intent(in) :: w12

    r = aux_arcl_eetri_trig(sin(b1), cos(b1), sin(b2), cos(b2), sin(w12), cos(w12))
  end function aux_arcl_eetri_d

!!!_  & aux_azimuth1_etri_trig - alpha_1, azimuth of ellips. triangle [NAB]
  ELEMENTAL &
  real(kind=KTGT) function aux_azimuth1_etri_trig_d &
       & (sb1, cb1, sb2, cb2, sw12, cw12) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: sb1,  cb1      ! sin,cos beta_1
    real(kind=KTGT),intent(in) :: sb2,  cb2      ! sin,cos beta_2
    real(kind=KTGT),intent(in) :: sw12, cw12     ! sin,cos omega_12

    real(kind=KTGT) :: z1r, z1i

    z1r = cb1 * sb2 - sb1 * cb2 * cw12
    z1i = cb2 * sw12
    r = atan2(z1i, z1r)
  end function aux_azimuth1_etri_trig_d

!!!_  & aux_azimuth1_etri - alpha_1, azimth of ellips. triangle [NAB]
  ELEMENTAL &
  real(kind=KTGT) function aux_azimuth1_etri_d &
       & (b1, b2, w12) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: b1
    real(kind=KTGT),intent(in) :: b2
    real(kind=KTGT),intent(in) :: w12
    r = aux_azimuth1_etri_trig(sin(b1), cos(b1), sin(b2), cos(b2), sin(w12), cos(w12))
  end function aux_azimuth1_etri_d

!!!_  & azimuth0_eetri_trig - alpha_0, azimuth of elem. ellips. triangle [NEP]
  ELEMENTAL &
  real(kind=KTGT) function azimuth0_eetri_trig_d &
       & (sb, cb, sa, ca) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: sb,  cb      ! sin,cos beta
    real(kind=KTGT),intent(in) :: sa,  ca      ! sin,cos alpha

    real(kind=KTGT) :: y,   x

    x = _hypot(ca, sa * sb)
    y = sa * cb
    r = atan2(y, x)
  end function azimuth0_eetri_trig_d

!!!_  & azimuth0_eetri - alpha_0, azimuth of elem. ellips. triangle [NEP]
  ELEMENTAL &
  real(kind=KTGT) function azimuth0_eetri_d &
       & (b, a) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: b
    real(kind=KTGT),intent(in) :: a

    r = azimuth0_eetri_trig(sin(b), cos(b), sin(a), cos(a))
  end function azimuth0_eetri_d

!!!_  & azimuth2cos_eetri_trig - cos(alpha_2), azimuth of elem. ellips. triangle [NEP]
  ELEMENTAL &
  real(kind=KTGT) function azimuth2cos_eetri_trig_d &
       & (cb1, cb2, ca1) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: cb1, cb2     ! cos beta_1, beta_2
    real(kind=KTGT),intent(in) :: ca1          ! cos alpha_1

    r = sqrt((ca1 * cb1) ** 2 + (cb2*cb2 - cb1*cb1)) / cb2
  end function azimuth2cos_eetri_trig_d

!!!_  & azimuth2cos_eetri - cos(alpha_2), azimuth of elem. ellips. triangle [NEP]
  ELEMENTAL &
  real(kind=KTGT) function azimuth2cos_eetri_d &
       & (b1, b2, a1) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: b1, b2     ! beta_1, beta_2
    real(kind=KTGT),intent(in) :: a1         ! alpha_1

    r = azimuth2cos_eetri_trig(cos(b1), cos(b2), cos(a1))
  end function azimuth2cos_eetri_d

!!!_  & arcl_eetri_trig - sigma, spher. arc length of the third side of the elem. ellips. triangle [NEP]
  ELEMENTAL &
  real(kind=KTGT) function arcl_eetri_trig_d &
       & (sb, cb, ca) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: sb, cb   ! sin,cos beta
    real(kind=KTGT),intent(in) :: ca       ! cos alpha

    r = atan2(sb, ca * cb)
  end function arcl_eetri_trig_d
!!!_  & arcl_eetri - sigma, spher. arc length of the third side of the elem. ellips. triangle [NEP]
  ELEMENTAL &
  real(kind=KTGT) function arcl_eetri_d &
       & (b, a) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: b
    real(kind=KTGT),intent(in) :: a

    r = arcl_eetri_trig(sin(b), cos(b), cos(a))
  end function arcl_eetri_d

!!!_  & sphlon_eetri_trig - omega
  ELEMENTAL &
  real(kind=KTGT) function sphlon_eetri_trig_d &
       & (ss, cs, sa) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: ss, cs   ! sin,cos sigma
    real(kind=KTGT),intent(in) :: sa       ! sin alpha_0

    r = atan2(sa * ss, cs)
  end function sphlon_eetri_trig_d

!!!_  & sphlon_eetri - omega
  ELEMENTAL &
  real(kind=KTGT) function sphlon_eetri_d &
       & (s, a) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: s
    real(kind=KTGT),intent(in) :: a

    r = sphlon_eetri_trig(sin(s), cos(s), sin(a))
  end function sphlon_eetri_d

!!!_  & expparam_trig - epsilon, expansion parameter
  ELEMENTAL &
  real(kind=KTGT) function expparam_trig_d &
       & (ca, ep2) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: ca       ! cos alpha_0
    real(kind=KTGT),intent(in) :: ep2      ! e prime ** 2

    real(kind=KTGT) kk, t

    kk = ep2 * (ca ** 2)

    t = sqrt(1.0_KTGT + kk)
    r = (t - 1.0_KTGT) / (t + 1.0_KTGT)
  end function expparam_trig_d

!!!_  & expparam - epsilon, expansion parameter
  ELEMENTAL &
  real(kind=KTGT) function expparam_d &
       & (a, ep2) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: a        ! alpha_0
    real(kind=KTGT),intent(in) :: ep2      ! e prime ** 2

    r = expparam_trig(cos(a), ep2)
  end function expparam_d

!!!_  - comp_elongi - ellipsoidal longitude
  real(kind=KTGT) function comp_elongi_d &
       & (CT, no, f, w, sa0, s, c2s, s2s) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in)  :: CT(0:*)
    integer,        intent(in)  :: no            ! limit order
    real(kind=KTGT),intent(in)  :: f, w
    real(kind=KTGT),intent(in)  :: sa0           ! sin (alpha_0)
    real(kind=KTGT),intent(in)  :: s             ! sigma
    real(kind=KTGT),intent(in)  :: c2s, s2s      ! cos(2 sigma), sin(2 sigma)

    integer jo
    real(kind=KTGT) :: b0, b1, b2

    b1 = 0.0_KTGT
    b2 = 0.0_KTGT

    do jo = no, 1, -1
       b0 = CT(jo) + (2.0_KTGT * c2s) * b1 - b2
       b2 = b1
       b1 = b0
    enddo

    r  = w - (f * sa0) * ((s + b1 * s2s) * CT(0))

  end function comp_elongi_d

!!!_  - comp_delta_elongi - ellipsoidal longitude difference


!!!_  - gen_vtable_elongi - (variable) coefficient table for ellipsoidal longitude (I3)
  subroutine gen_vtable_elongi_d &
       & (ierr, CT, CC, no, eps)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: CT(0:*)
    real(kind=KTGT),intent(in)  :: CC(0:, 0:)
    integer,        intent(in)  :: no            ! limit order
    real(kind=KTGT),intent(in)  :: eps

    real(kind=KTGT) :: ep
    integer jc, jo

    ierr = 0

    ep = 1.0_KTGT
    do jc = 0, no
       CT(jc) = 0.0_KTGT
       do jo = no, jc, -1
          CT(jc) = CT(jc) * eps + CC(jo, jc)
       enddo
       CT(jc) = CT(jc) * ep
       ep = ep * eps
    enddo
  end subroutine gen_vtable_elongi_d

!!!_  - gen_ctable_elongi - (constant) coefficient table for ellipsoidal longitude (I3)
  subroutine gen_ctable_elongi_d &
       & (ierr, C3, no, fiii)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: C3(0:, 0:)
    integer,        intent(in)  :: no            ! limit order
    real(kind=KTGT),intent(in)  :: fiii          ! third flattening (n)
    ierr = 0
    if (no.eq.9) then
#      include "ggf/coeffs_i3-o9.F90"
    else if (no.eq.7) then
#      include "ggf/coeffs_i3-o7.F90"
    else if (no.eq.6) then
#      include "ggf/coeffs_i3-o6.F90"
    else if (no.eq.5) then
#      include "ggf/coeffs_i3-o5.F90"
    else
       ierr = ERR_NOT_IMPLEMENTED
    endif
  end subroutine gen_ctable_elongi_d

!!!_  - gen_ctable_I1
  subroutine gen_ctable_I1_d &
       & (ierr, C1, no, eps)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: C1(0:*)
    integer,        intent(in)  :: no            ! limit order
    real(kind=KTGT),intent(in)  :: eps
    real(kind=KTGT)  :: epsc, eps2
    ierr = 0
    eps2 = eps ** 2
    if (no.eq.9) then
#      include "ggf/coeffs_i1-o9.F90"
    else if (no.eq.7) then
#      include "ggf/coeffs_i1-o7.F90"
    else if (no.eq.6) then
#      include "ggf/coeffs_i1-o6.F90"
    else if (no.eq.5) then
#      include "ggf/coeffs_i1-o5.F90"
    else
       ierr = ERR_NOT_IMPLEMENTED
    endif
    !! need special treatment for C1(0),
    !! which currentrly hold t = (1-e)A1 - 1,
    !! and return A1 - 1 = (t + e)/(1 - e)
    !! to keep precision.
    C1(0) = (C1(0) + eps) / (1.0_KTGT - eps)
  end subroutine gen_ctable_I1_d

!!!_  - gen_ctable_I2b
  subroutine gen_ctable_I2b_d &
       & (ierr, C2b, no, eps)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: C2b(0:*)
    integer,        intent(in)  :: no            ! limit order
    real(kind=KTGT),intent(in)  :: eps
    real(kind=KTGT)  :: epsc, eps2
    ierr = 0
    eps2 = eps ** 2
    if (no.eq.9) then
#      include "ggf/coeffs_i2b-o9.F90"
    else if (no.eq.7) then
#      include "ggf/coeffs_i2b-o7.F90"
    else if (no.eq.6) then
#      include "ggf/coeffs_i2b-o6.F90"
    else if (no.eq.5) then
#      include "ggf/coeffs_i2b-o5.F90"
    else
       ierr = ERR_NOT_IMPLEMENTED
    endif
    !! need special treatment for C2(0),
    !! which currentrly hold t = (1+e)A2 - 1,
    !! and return A2 - 1 = (t - e)/(1 + e)
    !! to keep precision.
    C2b(0) = (C2b(0) - eps) / (1.0_KTGT + eps)
  end subroutine gen_ctable_I2b_d

!!!_  - gen_ctable_I2a
  subroutine gen_ctable_I2a_d &
       & (ierr, C2a, no, eps)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: C2a(0:*)
    integer,        intent(in)  :: no            ! limit order
    real(kind=KTGT),intent(in)  :: eps
    real(kind=KTGT)  :: epsc, eps2
    ierr = 0
    eps2 = eps ** 2
    if (no.eq.9) then
#      include "ggf/coeffs_i2a-o9.F90"
    else if (no.eq.7) then
#      include "ggf/coeffs_i2a-o7.F90"
    else if (no.eq.6) then
#      include "ggf/coeffs_i2a-o6.F90"
    else if (no.eq.5) then
#      include "ggf/coeffs_i2a-o5.F90"
    else
       ierr = ERR_NOT_IMPLEMENTED
    endif
    !! need special treatment for C2(0),
    !! which currentrly hold t = A2/(1-e) - 1,
    !! and return A2 - 1 = t * (1 - e) - e
    !! to keep precision.
    C2a(0) = C2a(0) * (1.0_KTGT - eps) - eps
  end subroutine gen_ctable_I2a_d

!!!_ + private procedures
!!!_  - is_equidistant - check if the array is equidistant under tolerance
  logical function is_equidistant_d(x, n, tol, e, r) result(b)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    real(kind=KTGT),intent(in)           :: x(0:*)
    integer,        intent(in)           :: n
    real(kind=KTGT),intent(in), optional :: tol   ! default: max(x) * 2 eps
    real(kind=KTGT),intent(out),optional :: e     ! error
    real(kind=KTGT),intent(in), optional :: r     ! reference
    real(kind=KTGT) :: dmin, dmax, d, amax, t
    integer j
    if (n.le.1) then
       b = .TRUE.
       if (present(e)) then
          e = 0.0_KTGT
       endif
    else
       j = 0
       d = x(j+1) - x(j)
       dmin = d
       dmax = d
       amax = abs(x(j))
       do j = 1, n - 2
          d = x(j+1) - x(j)
          amax = max(abs(x(j)), amax)
          dmin = min(d, dmin)
          dmax = max(d, dmax)
          ! write(*, *) j, dmin, dmax
       enddo
       t = choice(-1.0_KTGT, tol)
       if (t.lt.0.0_KTGT) then
          t = amax * (epsilon(0.0_KTGT) * 2.0_KTGT)
          ! t = (amax * epsilon(0.0_KTGT))
       endif
       ! write(*, *) dmax, dmin, (dmax - dmin), t
       if (present(r)) then
          d = (dmax - r) - (dmin - r)
       else
          d = dmax - dmin
       endif
       b = d .le. t
       if (present(e)) then
          e = d
       endif
    endif
  end function is_equidistant_d

!!!_  - check_precision - epsilon check (inherit gauss())
!!!_   . NOTE
  !  Actualy the return value is NOT epsilon(), but the first
  !  value to be ONE eq ONE + E.
  !  This implementation originates from MIROC legacy.
  real(kind=KTGT) function check_precision_d &
       & (u, max_iter, base, levv, mold) &
       & result(E)
    use TOUZA_Std,only: KTGT=>KDBL
    use TOUZA_Std,only: choice
    use TOUZA_Std,only: msg_grp
    implicit none
    integer,        intent(in),optional :: u
    integer,        intent(in),optional :: max_iter
    real(kind=KTGT),intent(in),optional :: base
    integer,        intent(in),optional :: levv
    real(kind=KTGT),intent(in),optional :: mold

    integer lv
    integer j, n
    real(kind=KTGT) b, R, RP
    character(len=1024) :: TMSG
    real(kind=KTGT),parameter :: ONE =  1.0_KTGT
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: def = 10.0_KTGT

    b = choice(ZERO, base)
    if (b.lt.ZERO) then
       E = HUGE(mold)
       E = SET_EXPONENT(FRACTION(E), EXPONENT(EPSILON(mold)) - 2)
    else
       lv = choice(0, levv)
       if (b.eq.ZERO) b = def

       n = choice(0, max_iter)
       if (n.le.0) then
          n = INT(ABS(LOG(EPSILON(mold))/LOG(B))) * 2
       endif

       E = ONE
       do j = 1, n
          E  = E / b
          R  = ONE
          RP = R + E
101       format('precision check .. ', F20.18, 1x, E9.3)
          if (lv.ge.0) then
             write(TMSG, 101) RP, E
             call msg_grp(TMSG, __GRP__, __MDL__, u)
          endif
          IF (RP .LE. R) exit
       enddo
    endif
  end function check_precision_d
  real(kind=KTGT) function check_precision_f &
       & (u, max_iter, base, levv, mold) &
       & result(E)
    use TOUZA_Std,only: KTGT=>KFLT
    use TOUZA_Std,only: choice
    use TOUZA_Std,only: msg_grp
    implicit none
    integer,        intent(in),optional :: u
    integer,        intent(in),optional :: max_iter
    real(kind=KTGT),intent(in),optional :: base
    integer,        intent(in),optional :: levv
    real(kind=KTGT),intent(in)          :: mold

    integer lv
    integer j, n
    real(kind=KTGT) b, R, RP
    character(len=1024) :: TMSG
    real(kind=KTGT),parameter :: ONE =  1.0_KTGT
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: def = 10.0_KTGT

    b = choice(ZERO, base)
    if (b.lt.ZERO) then
       E = HUGE(mold)
       E = SET_EXPONENT(FRACTION(E), EXPONENT(EPSILON(mold)) - 2)
       return
    else
       lv = choice(0, levv)
       if (b.eq.ZERO) b = def

       n = choice(0, max_iter)
       if (n.le.0) then
          n = INT(ABS(LOG(EPSILON(mold))/LOG(B))) * 2
       endif

       E = ONE
       do j = 1, n
          E  = E / b
          R  = ONE
          RP = R + E
101       format('precision check .. ', F20.18, 1x, E9.3)
          if (lv.ge.0) then
             write(TMSG, 101) RP, E
             call msg_grp(TMSG, __GRP__, __MDL__, u)
          endif
          IF (RP .LE. R) exit
       enddo
    endif
  end function check_precision_f
!!!_   . span_longitude() - return default span if not present or zero
  PURE real(kind=KTGT) function span_longitude_d(span) result(x)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    real(kind=KTGT),intent(in),optional :: span
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    x = choice(ZERO, span)
    if (x.eq.ZERO) x = 2.0_KTGT * pi_(ZERO)
  end function span_longitude_d

!!!_   . span_latitude() - return default span if not present or zero
  PURE real(kind=KTGT) function span_latitude_d(span) result(x)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    real(kind=KTGT),intent(in),optional :: span
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    x = choice(ZERO, span)
    if (x.eq.ZERO) x = pi_(ZERO)
  end function span_latitude_d

!!!_   . pi_()
  PURE real(kind=KTGT) function pi_d(mold) result(x)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: mold
    real(kind=KTGT),parameter  :: ONE =  real(1, kind=kind(mold))
    real(kind=KTGT),parameter  :: FOUR = real(4, kind=kind(mold))
    real(kind=KTGT),parameter  :: p = ATAN(one) * FOUR
    x = p
  end function pi_d

!!!_ + end Emu/ugg
end module TOUZA_Emu_ugg
!!!_* non-module Procedures

!!!_@ test_emu_usi - test program
#if TEST_EMU_UGG
program test_emu_ugg
  use TOUZA_Emu_ugg
  use TOUZA_Std,only: KFLT, KDBL
  use TOUZA_Std,only: KTGT=>KDBL
  use TOUZA_Std,only: arg_init, arg_diag, parse, get_option
  use TOUZA_Std,only: condop
  implicit none
  integer ierr
  integer nlat(2)
  integer nlon(2)
  integer stereo,  geod, prec
  integer levv

  real(kind=KDBL) :: PI = ATAN2(0.0_KDBL, -1.0_KDBL)

  ierr = 0
101 format(A, ' = ', I0)

  if (ierr.eq.0) call arg_init(ierr)
  if (ierr.eq.0) call parse(ierr)
  if (ierr.eq.0) call arg_diag(ierr)

  ! test arguments
  !   lat=NUMBER[,DIV]  simple latitude test.   skipped if NUMBER==0.  default=128,1
  !   lon=NUMBER[,DIV]  simple longitude test.  skipped if NUMBER==0.  default=128,1
  !   stereo=FLAG       simple north/south polar stereographic projection test,

  if (ierr.eq.0) call get_option(ierr, levv, 'v', 0)
  if (ierr.eq.0) call init(ierr, u=-1, levv=levv, stdv=-1)
  if (ierr.eq.0) call diag(ierr)

  if (ierr.eq.0) call get_option(ierr, nlat(:), 'lat', -1)
  if (ierr.eq.0) nlat(1) = condop((nlat(1).lt.0), 128, nlat(1))

  if (ierr.eq.0) call get_option(ierr, nlon(:), 'lon', -1)
  if (ierr.eq.0) nlon(1) = condop((nlon(1).lt.0), 128, nlon(1))

  if (ierr.eq.0) call get_option(ierr, stereo, 'stereo', 0)
  if (ierr.eq.0) call get_option(ierr, geod,   'geod', 0)
  if (ierr.eq.0) call get_option(ierr, prec,   'prec', 0)

  if (ierr.eq.0) then
     if (prec.ge.0) call test_ugg_prec(ierr)
  endif

  if (ierr.eq.0) then
     if (nlat(1).gt.0) call test_ugg_lat(ierr, nlat(1), nlat(2))
  endif
  if (ierr.eq.0) then
     if (nlon(1).gt.0) call test_ugg_lon(ierr, nlon(1), nlon(2))
  endif

  if (ierr.eq.0) then
     if (stereo.ge.0) call batch_test_stereog(ierr, stereo)
  endif
  if (ierr.eq.0) then
     if (geod.eq.0) then
        call batch_test_geod(ierr, geod)
     else if (geod.eq.1) then
        call batch_test_geod_filter(ierr)
     endif
  endif
  call batch_test_section(ierr)
  call batch_test_length(ierr)

  call finalize(ierr)
  write(*, 101) 'fine', ierr
  stop
contains
!!!_ + test_ugg_prec
  subroutine test_ugg_prec(ierr)
    integer,intent(out) :: ierr
    real(kind=KFLT) :: epsf
    real(kind=KDBL) :: epsd

    ierr = 0

102 format('result = ', E16.9, E16.9)
    epsf = check_precision(mold=epsf)
    epsd = check_precision(mold=epsd)
    write(*, 102) epsf, epsd

    epsf = check_precision(base=2.0_KFLT, mold=epsf)
    epsd = check_precision(base=2.0_KDBL, mold=epsd)
    write(*, 102) epsf, epsd

    epsf = check_precision(base=-1.0_KFLT, mold=epsf)
    epsd = check_precision(base=-1.0_KDBL, mold=epsd)
    write(*, 102) epsf, epsd
    return
  end subroutine test_ugg_prec
!!!_ + test_ugg_lat
  subroutine test_ugg_lat(ierr, nlat, mdiv)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: nlat
    integer,intent(in)  :: mdiv
    real(kind=KTGT) :: glat(nlat),  wlat(nlat)
    real(kind=KTGT) :: glat2(nlat), wlat2(nlat)
    real(kind=KTGT) :: glatm(nlat+1)

    real(kind=KTGT) :: ylatc(nlat*mdiv)
    real(kind=KTGT) :: ylatb(nlat*mdiv+1)

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE =  1.0_KTGT
    real(kind=KTGT),parameter :: HALF = 0.5_KTGT

    real(kind=KTGT) :: span = ZERO
    real(kind=KTGT) :: wnml = ONE
    integer deq

    integer j
    integer method

    ierr = 0

    if (ierr.eq.0) call get_option(ierr, span,    'ly', ZERO)
    if (ierr.eq.0) call get_option(ierr, wnml,    'wy', ONE)
    if (ierr.eq.0) call get_option(ierr, method,  'my', LAT_GAUSS_LEGACY)

    if (ierr.eq.0) call get_latitude(ierr, glat,  wlat, nlat, span=span, wnml=wnml, method=method)
    if (ierr.eq.0) call mid_latitude(ierr, glatm, wlat, nlat)

    if (ierr.eq.0) call gauss_latitude(ierr, glat2, wlat2, nlat, span, wnml, prec=-1.0_KTGT)
    if (ierr.eq.0) glat2(1:nlat) = asin(glat2(1:nlat))

201 format('lat:', I0, 1x, F9.3, 1x, 2E10.3, 2x, 2E10.3, 1x, L1, 1x, E10.3, 1x, 2E10.3)
202 format('latm:', I0, 1x, F9.3, 1x, E16.8)
    if (ierr.eq.0) then
       do j = 1, nlat
          write(*, 201) j, glat(j) * 180.0_KTGT / PI, &
               & glat(j), wlat(j), glat2(j), wlat2(j), &
               & glat(j).eq.glat2(j), glat(j)-glat2(j), &
               & sin(glat(j)), wlat(j) * 2.0_KTGT
       enddo
       do j = 1, nlat + 1
          write(*, 202) j, glatm(j) * 180.0_KTGT / PI, glatm(j)
       enddo
    endif

    if (mdiv.gt.0) then
       deq = 0
       if (ANY(method.eq.(/LAT_LINEAR, LAT_LINEAR_COMPATIBLE/))) deq = 1
       if (deq.eq.0) then
          if (ierr.eq.0)then
             call div_latitude(ierr, ylatc, mdiv, .FALSE., glat, glatm, nlat, method=method)
          endif
          if (ierr.eq.0)then
             call div_latitude(ierr, ylatb, mdiv, .TRUE., glat, glatm, nlat, method=method)
          endif
       else
          if (ierr.eq.0)then
             call div_latitude(ierr, ylatc, mdiv, .FALSE., glat, glatm, nlat, span=span, method=method)
          endif
          if (ierr.eq.0)then
             call div_latitude(ierr, ylatb, mdiv, .TRUE., glat, glatm, nlat, span=span, method=method)
          endif
       endif
       if (ierr.eq.0) then
203 format('laty:', I0, 1x, F9.3, 1x, F9.3)
          do j = 1, nlat * mdiv
             write(*, 203) j, rad2deg(ylatc(j)), rad2deg(ylatb(j))
          enddo
       endif
    endif

  end subroutine test_ugg_lat
!!!_ + test_ugg_lon
  subroutine test_ugg_lon(ierr, nlon, mdiv)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: nlon
    integer,intent(in)  :: mdiv
    real(kind=KTGT) :: glonc(0:nlon-1), wlonc(0:nlon-1)
    real(kind=KTGT) :: glonb(0:nlon),   wlonb(0:nlon)

    real(kind=KTGT) :: ylonc(0:nlon*mdiv-1)
    real(kind=KTGT) :: ylonb(0:nlon*mdiv)

    real(kind=KTGT) :: x(0:nlon)

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE =  1.0_KTGT
    real(kind=KTGT),parameter :: HALF = 0.5_KTGT

    integer bsw
    integer :: method, deq

    real(kind=KTGT) :: span = ZERO
    real(kind=KTGT) :: wnml = ONE
    integer ny

    ierr = 0

    if (ierr.eq.0) call get_option(ierr, span,    'lx', ZERO)
    if (ierr.eq.0) call get_option(ierr, wnml,    'wx', ONE)
    if (ierr.eq.0) call get_option(ierr, bsw,     'bx', 0)    ! boundary (mid_longitude) switch
    if (ierr.eq.0) call get_option(ierr, method,  'mx', DIV_EACH_EDGE)
    if (ierr.eq.0) call get_option(ierr, deq,     'ex', 0)

    if (ierr.eq.0) call get_longitude(ierr, glonc, wlonc, nlon,             span=span, wnml=wnml)
    if (ierr.eq.0) then
       if (bsw.eq.0) then
          if (ierr.eq.0) call get_longitude(ierr, glonb, wlonb, nlon+1, div=nlon, span=span, wnml=wnml, org=-HALF)
       else
          if (ierr.eq.0) call mid_longitude(ierr, glonb, glonc, wlonc, nlon, span / wnml)
       endif
    endif

    if (ierr.eq.0) then
       call test_equidistant(ierr, glonc, nlon,   'lonc', -1.0_KTGT)
       call test_equidistant(ierr, glonb, nlon+1, 'lonb', -1.0_KTGT)
    endif
    if (mdiv.gt.0) then
       if (method.eq.DIV_ACCUMULATE) deq = 1
       if (deq.eq.0) then
          if (ierr.eq.0)then
             call div_longitude(ierr, ylonc, mdiv, .FALSE., glonc, glonb, nlon, method=method)
          endif
          if (ierr.eq.0)then
             call div_longitude(ierr, ylonb, mdiv, .TRUE.,  glonc, glonb, nlon, method=method)
          endif
       else
          if (ierr.eq.0)then
             call div_longitude(ierr, ylonc, mdiv, .FALSE., glonc, glonb, nlon, span=span, method=method)
          endif
          if (ierr.eq.0)then
             call div_longitude(ierr, ylonb, mdiv, .TRUE.,  glonc, glonb, nlon, span=span, method=method)
          endif
       endif
       ny = nlon * mdiv
       call test_equidistant(ierr, ylonc, ny,     'ylonc', -1.0_KTGT)
       call test_equidistant(ierr, ylonb, ny + 1, 'ylonb', -1.0_KTGT)

201    format('consistency/', A, ': ', E9.3)
       ! check b vs b
       x(0:nlon) = glonb(0:nlon) - ylonb(0:ny:mdiv)
       write(*, 201) 'b', maxval(x(0:nlon)) - minval(x(0:nlon))

       if (mod(mdiv, 2).eq.1) then
          ! check c vs c
          x(0:nlon-1) = glonc(0:nlon-1) - ylonc(mdiv/2:ny-1:mdiv)
       else
          ! check c vs b
          x(0:nlon-1) = glonc(0:nlon-1) - ylonb(mdiv/2:ny-1:mdiv)
       endif
       write(*, 201) 'c', maxval(x(0:nlon-1)) - minval(x(0:nlon-1))
    endif
    if (ierr.eq.0) call check_longitude(ierr, glonc, nlon,             span=span, tag='lonc')
    if (ierr.eq.0) call check_longitude(ierr, glonb, nlon+1, div=nlon, span=span, tag='lonb')
    if (mdiv.gt.0) then
       if (ierr.eq.0) then
          call check_div_longitude(ierr, ylonc, mdiv, .FALSE., glonc, glonb, nlon, span=span, tag='ylonc')
       endif
       if (ierr.eq.0) then
          call check_div_longitude(ierr, ylonb, mdiv, .TRUE.,  glonc, glonb, nlon, span=span, tag='ylonb')
       endif
    endif

  end subroutine test_ugg_lon

  subroutine test_equidistant(ierr, x, n, tag, tol)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: n
    real(kind=KTGT), intent(in)  :: x(*)
    character(len=*),intent(in)  :: tag
    real(kind=KTGT), intent(in)  :: tol
    logical b
    real(kind=KTGT) :: e
    ierr = 0
    b = is_equidistant(x, n, tol, e)
101 format('equidistant:', A, ': ', L1, 1x, E10.3)
    write(*, 101) trim(tag), b, e
  end subroutine test_equidistant

!!!_ + batch_test_stereog
  subroutine batch_test_stereog(ierr, test)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr
    integer,intent(in)  :: test
    real(kind=KTGT) :: a, e, latts, lon0

    ierr = 0
    if (test.lt.0) return ! dummy procedure

    ! snyder example
    a = 6378388.0_KTGT
    e = 0.0819919_KTGT

    latts = -71.0_KTGT
    lon0  = -100.0_KTGT
    call test_stereog_single(ierr, a, e, latts, lon0, (/150.0_KTGT, -75.0_KTGT/))
    !  (x, y) = (-1540033.6, -560526.4)
    !       k = 0.9896255
    call test_stereog_single(ierr, a, e, latts, lon0, (/150.0_KTGT, +75.0_KTGT/))
    latts = +71.0_KTGT
    lon0  = +100.0_KTGT
    call test_stereog_single(ierr, a, e, latts, lon0, (/-150.0_KTGT, +75.0_KTGT/))
    call test_stereog_single(ierr, a, e, latts, lon0, (/-150.0_KTGT, -75.0_KTGT/))

    ! pole
    call test_stereog_single(ierr, a, e, latts, lon0, (/0.0_KTGT, 90.0_KTGT/))
    call test_stereog_single(ierr, a, e, latts, lon0, (/0.0_KTGT, 89.9_KTGT/))

    ! k0 = 0
    latts = 90.0_KTGT
    lon0  = 0.0_KTGT
    call test_stereog_single(ierr, a, e, latts, lon0, (/0.0_KTGT, 90.0_KTGT/))
    call test_stereog_single(ierr, a, e, latts, lon0, (/0.0_KTGT, 0.0_KTGT/))

    ! scale factor as 1, e = 0
    latts = +90.0_KTGT
    lon0  = +100.0_KTGT
    e = 0.0_KTGT
    call test_stereog_single(ierr, a, e, latts, lon0, (/-150.0_KTGT, +75.0_KTGT/))

    a = 6378137.0_KTGT
    e = flatten_to_ecc(1.0_KTGT/298.257223563_KTGT)
    latts = +70.0_KTGT
    lon0  = -45.0_KTGT
    call test_stereog_single(ierr, a, e, latts, lon0, (/-55.7362542291_KTGT, 59.0479222286_KTGT/))

  end subroutine batch_test_stereog

  subroutine test_stereog_single(ierr, a, e, latts, lon0, ll)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr
    real(kind=KTGT),intent(in) :: a, e, latts, lon0
    real(kind=KTGT),intent(in) :: ll(2)
    real(kind=KTGT) :: sf
    real(kind=KTGT) :: rr(2), rts, llorg(2)
    real(kind=KTGT) :: xxo(2), xxc(2), lli(2)
    real(kind=KTGT) :: cco2(ncache_psgp_co), clo2(ncache_psgp_lo), cla2(ncache_psgp_la)
    real(kind=KTGT) :: gla(2), dlo(2)
    integer pole

    ierr = 0

    rts = deg2rad(latts)
    rr(1:2) = deg2rad(ll(1:2))
    if (latts.ge.0.0_KTGT) then
       pole = +1
    else
       pole = -1
    endif
    llorg(1) = deg2rad(lon0)
    llorg(2) = deg2rad(90.0_KTGT) * real(pole, kind=KTGT)

101 format('## stereog[', 2F9.1, '] ', '(', 2ES16.8, ')', ' [', 2ES16.8, ']')
    write(*, 101) lon0, latts, ll, a, e

112 format('  psgp[', A, '] ', 1x, ' >> (', 2ES16.8, ')')
113 format('  inverse', 2ES16.8, 1x, 2ES16.8, 2ES16.8, 1x, F16.13)

    call psgp_set(cco2, e, a, rts, llorg(1), pole)

    xxo = psgp_fwd(rr(1), rr(2), cco2)
    write(*, 112) 'once', xxo
    call psgp_cachela(cla2, rr(2), cco2)
    call psgp_cachelo(clo2, rr(1), cco2)
    xxc = psgp_fwd(clo2, cla2)
    write(*, 112) 'cache', xxc

    lli = psgp_bwd_ll(xxc(1), xxc(2), cco2)
    sf = psgp_bwd_sf(xxc(1), xxc(2), cco2)
    write(*, 113) rad2deg(lli(:)), rad2deg(rr(:)), rad2deg(rr(:) - lli(:)), sf

    call psgp_bwd_tr(gla, dlo, xxc(1), xxc(2), cco2)
121 format(2x, A, ':', A, 1x, 2ES16.8)
    write(*, 121) 'sin', 'lat', _SIN(gla), sin(rr(2))
    write(*, 121) 'cos', 'lat', _COS(gla), cos(rr(2))
    write(*, 121) 'sin', 'dlon', _SIN(dlo), sin(rr(1) - llorg(1))
    write(*, 121) 'cos', 'dlon', _COS(dlo), cos(rr(1) - llorg(1))

  end subroutine test_stereog_single

!!!_ + batch_test_length
  subroutine batch_test_length &
       & (ierr)
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr
    real(kind=KTGT) :: xl, xh, dx
    real(kind=KTGT) :: yl, yh, dy

    real(kind=KTGT) :: a, e, rf
    real(kind=KTGT) :: lonorg, tslat
    integer pole
    integer ji, jj, n

    real(kind=KTGT) :: x0, y0, x1, y1, xo, yo

    a = 6378137.0_KTGT
    rf = 298.257223563_KTGT
    e = flatten_to_ecc(1.0_KTGT/rf)
    tslat = deg2rad(70.0_KTGT)
    lonorg = deg2rad(-45.0_KTGT)
    pole = +1

    xl = -641150.0_KTGT - 1500.0_KTGT
    xh = +867850.0_KTGT + 1500.0_KTGT
    yl = -3375050.0_KTGT - 1500.0_KTGT
    yh = -642050.0_KTGT  + 1500.0_KTGT
    dx = 6000.0_KTGT
    dy = 6000.0_KTGT

    x0 = xl + 0.0_KTGT * dx
    y0 = yl + 0.0_KTGT * dy
    x1 = x0 + dx
    y1 = y0
    call test_length(ierr, x0, y0, x1, y1, a, e, tslat, lonorg, pole)

    x1 = xh + 0.0_KTGT * dx
    y1 = yh + 0.0_KTGT * dy
    x0 = x1 - dx
    y0 = y1
    call test_length(ierr, x0, y0, x1, y1, a, e, tslat, lonorg, pole)

    x1 = xh + 0.0_KTGT * dx
    y1 = yh + 0.0_KTGT * dy
    x0 = x1
    y0 = y1 - dy
    call test_length(ierr, x0, y0, x1, y1, a, e, tslat, lonorg, pole)

    x1 = 0.0_KTGT
    y1 = yh + 0.0_KTGT * dy
    x0 = x1
    y0 = y1 - dy
    call test_length(ierr, x0, y0, x1, y1, a, e, tslat, lonorg, pole)

    x1 = 0.0_KTGT
    y1 = yh + 0.0_KTGT * dy
    x0 = x1 - dx
    y0 = y1
    call test_length(ierr, x0, y0, x1, y1, a, e, tslat, lonorg, pole)

    y0 = y1 - dy
    call test_area(ierr, x0, y0, x1, y1, a, e, tslat, lonorg, pole)
    return

    e = 0.0_KTGT
    tslat = deg2rad(90.0_KTGT)
    lonorg = 0.0_KTGT
    x0 = - dx / 2
    y0 = - dy / 2
    x1 = + dx / 2
    y1 = + dy / 2
    call test_area(ierr, x0, y0, x1, y1, a, e, tslat, lonorg, pole)

    ! x0 = x1 - dx / 4.0_KTGT
    ! y0 = y1 - dy / 4.0_KTGT
    ! call test_area(ierr, x0, y0, x1, y1, a, e, tslat, lonorg, pole)

    ! x0 = x1 - dx / 16.0_KTGT
    ! y0 = y1 - dy / 16.0_KTGT
    ! call test_area(ierr, x0, y0, x1, y1, a, e, tslat, lonorg, pole)

    ! x0 = x1 - dx / 64.0_KTGT
    ! y0 = y1 - dy / 64.0_KTGT
    ! call test_area(ierr, x0, y0, x1, y1, a, e, tslat, lonorg, pole)
    ! return

    n = 6
    xo = 0.0_KTGT
    yo = yh
    do jj = 0, n - 1
       do ji = 0, n - 1
          x0 = xo + (ji - 1) * (dx / n)
          y1 = yo - (jj - 1) * (dy / n)
          x1 = x0 + (dx / n)
          y0 = y1 - (dy / n)
          write(*, *) ji, jj
          call test_area(ierr, x0, y0, x1, y1, a, e, tslat, lonorg, pole)
       enddo
    enddo
  end subroutine batch_test_length

!!!_ + test_length
  subroutine test_length &
       & (ierr, x0, y0, x1, y1, a, e, tslat, lonorg, pole)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(in) :: x0, y0
    real(kind=KTGT),intent(in) :: x1, y1
    real(kind=KTGT),intent(in) :: a, e
    real(kind=KTGT),intent(in) :: tslat, lonorg
    integer,        intent(in) :: pole

    real(kind=KTGT),parameter :: HALF = 0.5_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT

    real(kind=KTGT) :: xp,  yp
    real(kind=KTGT) :: ddx, ddy
    real(kind=KTGT) :: sc,  k,  scp
    real(kind=KTGT) :: ll(2)
    real(kind=KTGT) :: tol
    real(kind=KTGT) :: re, rt, rc

    integer,parameter :: llev = 16
    ! integer,parameter :: llev = 10

    integer jlev, jini
    integer jj,   nd
    real(kind=KTGT) :: cco(ncache_psgp_co)
    real(kind=KTGT) :: buf(0:llev), bp
    integer bpos, bj

    ierr = 0
    tol = 2.0_KTGT

    call psgp_set(cco, e, a, tslat, lonorg, pole)

    ddx = x1 - x0
    ddy = y1 - y0

    xp = (x0 + x1) / TWO
    yp = (y0 + y1) / TWO

    ll = psgp_bwd_ll(xp, yp, cco)
    sc = psgp_bwd_isf(xp, yp, cco)

111 format('length:input ','(', 2ES16.8, ')--', &
         & '(', 2ES16.8, ')', &
         & '  [', 2F12.8 ,']', 1x, ES16.8)

    write(*, 111) x0, y0, x1, y1, rad2deg(ll(1)), rad2deg(ll(2)), sc

101 format('length:level ', I0, ' ', ES16.9)
102 format('length:level ', I0, ' ', ES16.9, 1x, ES16.9)
103 format('length:result ', ES16.9, 1x, 2ES16.8)
    scp = 0.0_KTGT
    bp = 0.0_KTGT
    jini = min(6, llev - 1)

    do jlev = jini, llev - 1
       nd = 2 ** jlev
       sc = 0.0_KTGT
       rc = 0.0_KTGT
       bpos = 0
       do jj = 0, nd - 1
          xp = x0 + (ddx / real(nd, kind=KTGT)) * (real(jj, kind=KTGT) + HALF)
          yp = y0 + (ddy / real(nd, kind=KTGT)) * (real(jj, kind=KTGT) + HALF)
          ! write(*, *) jlev, jj, xp, yp
          k = psgp_bwd_isf(xp, yp, cco)
          re = k - rc
          rt = sc + re
          rc = (rt - sc) - re
          sc = rt
          buf(bpos) = k
          bj = jj
          do
             if (mod(bj, 2).eq.0) exit
             buf(bpos - 1) = buf(bpos - 1) + buf(bpos)
             bj = bj / 2
             bpos = bpos - 1
          enddo
          bpos = bpos + 1
          ! write(*, *) jj, bpos, buf(0:bpos-1)
       enddo
       sc = sc / real(nd, kind=KTGT)
       buf(0) = buf(0) / real(nd, kind=KTGT)
       if (jlev.gt.jini) then
          write(*, 102) jlev, sc, sc - scp
          write(*, 102) jlev, buf(0), buf(0) - bp
       else
          write(*, 101) jlev, sc
          write(*, 101) jlev, buf(0)
       endif
       if (abs(sc-scp).le.spacing(scp) * tol) exit
       if (abs(buf(0)-bp).le.spacing(bp) * tol) exit
       scp = sc
       bp = buf(0)
    enddo
    write(*, 103) max(ddx, ddy) * scp, scp, spacing(scp)

  end subroutine test_length

!!!_ + test_area
  subroutine test_area &
       & (ierr, x0, y0, x1, y1, a, e, tslat, lonorg, pole)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(in) :: x0, y0
    real(kind=KTGT),intent(in) :: x1, y1
    real(kind=KTGT),intent(in) :: a, e
    real(kind=KTGT),intent(in) :: tslat, lonorg
    integer,        intent(in) :: pole

    real(kind=KTGT),parameter :: HALF = 0.5_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT

    real(kind=KTGT) :: xp,  yp
    real(kind=KTGT) :: ddx, ddy
    real(kind=KTGT) :: k
    real(kind=KTGT) :: ll(2)
    real(kind=KTGT) :: tol

    real(kind=KTGT) :: aref

    integer,parameter :: llev = 16
    ! integer,parameter :: llev = 12
    ! integer,parameter :: llev = 12

    integer jlev, jini
    integer ji,   jj,   nd
    real(kind=KTGT) :: cco(ncache_psgp_co)
    real(kind=KTGT) :: buf(0:llev*2), bp, be
    integer bpos, bj

    ierr = 0
    tol = 512.0_KTGT
    ! tol = 128.0_KTGT
    ! tol = 32.0_KTGT
    jini = min(8, llev - 2)

    call psgp_set(cco, e, a, tslat, lonorg, pole)

    ddx = x1 - x0
    ddy = y1 - y0
    aref = (ddx * ddy)

    xp = (x0 + x1) / TWO
    yp = (y0 + y1) / TWO

    ll = psgp_bwd_ll(xp, yp, cco)
    k  = psgp_bwd_iaf(xp, yp, cco)

111 format('area:input ','(', 2ES16.8, ')--', &
         & '(', 2ES16.8, ')', &
         & '  [', 2F12.8 ,']', 1x, ES16.8)

    write(*, 111) x0, y0, x1, y1, rad2deg(ll(1)), rad2deg(ll(2)), k

102 format('area:level ', I0, ' ', ES16.9, 1x, ES16.9, 1x, ES16.9)
103 format('area:result ', ES16.9, 1x, ES16.8, 1x, 2ES16.9)

    bp = 0.0_KTGT
    do jlev = jini, llev - 1
       nd = 2 ** jlev
       bpos = 0
       do jj = 0, nd - 1
          yp = y0 + (ddy / real(nd, kind=KTGT)) * (real(jj, kind=KTGT) + HALF)
          do ji = 0, nd - 1
             xp = x0 + (ddx / real(nd, kind=KTGT)) * (real(ji, kind=KTGT) + HALF)
             ! write(*, *) jlev, jj, xp, yp
             k = psgp_bwd_iaf(xp, yp, cco)
             buf(bpos) = k
             bj = jj * nd + ji
             do
                if (mod(bj, 2).eq.0) exit
                buf(bpos - 1) = buf(bpos - 1) + buf(bpos)
                bj = bj / 2
                bpos = bpos - 1
             enddo
             bpos = bpos + 1
          enddo
       enddo
       buf(0) = buf(0) / real(nd * nd, kind=KTGT)
       be = buf(0) - bp
       write(*, 102) jlev, buf(0), be / buf(0), aref * be
       if (abs(be).le.spacing(bp) * tol) exit
       bp = buf(0)
    enddo
    write(*, 103) buf(0), aref * buf(0), aref * be, spacing(buf(0))

  end subroutine test_area

!!!_ + batch_test_section
  subroutine batch_test_section &
       & (ierr)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr
    real(kind=KTGT) :: a, e, rf
    real(kind=KTGT) :: x
    real(kind=KTGT) :: y1, y2, y3

    ! WGS84 a=6378137.0 rf=298.257223563

    ierr = 0
    a = 6378137.0_KTGT
    rf = 298.257223563_KTGT
    e = flatten_to_ecc(1.0_KTGT/rf)

    x = 1000.0e3_KTGT
    y1= 2000.0e3_KTGT
    y2= -y1
    y3 = 0.0_KTGT
    call test_section(ierr, a, e, x, y1, y2, y3)

    e = 0.0_KTGT
    call test_section(ierr, a, e, x, y1, y2, y3)

    e = 0.5_KTGT
    call test_section(ierr, a, e, x, y1, y2, y3)

  end subroutine batch_test_section
!!!_ + test_section
  subroutine test_section &
       & (ierr, a, e, x, y1, y2, y3)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(in) :: a, e
    real(kind=KTGT),intent(in) :: x
    real(kind=KTGT),intent(in) :: y1, y2, y3

    integer j
    integer,parameter :: npos = 4
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE  = 1.0_KTGT

    real(kind=KTGT) :: b, e2
    real(kind=KTGT) :: ll(2, 1:npos-1)
    real(kind=KTGT) :: xy(2, 1:npos-1)

    real(kind=KTGT) :: pp(3, 0:npos-1)
    real(kind=KTGT) :: uu(3, 1:npos-1)
    real(kind=KTGT) :: un

    real(kind=KTGT) :: nv(3, 1:npos-1)

    real(kind=KTGT) :: cco(ncache_psgp_co)

    real(kind=KTGT) :: lonorg, tslat
    integer pole

    real(kind=KTGT) :: rn, clat, slat, clon, slon

    ! (a-b)/a = 1 - sqrt(1-e2)
    ! 1-  b/a = 1 - sqrt(1-e2)
    ! b = a * sqrt(1-e2)

    ierr = 0

    e2 = e * e
    b = a * sqrt((ONE - e) * (ONE + e))

    xy(1:2, 1) = (/x, y1/)
    xy(1:2, 2) = (/x, y2/)
    xy(1:2, 3) = (/x, y3/)

    pole = +1
    lonorg = ZERO
    tslat = deg2rad(90.0_KTGT)

    call psgp_set(cco, e, a, tslat, lonorg, pole)

101 format('section:input[', I0, '] ', 2ES17.9, 1x, 2F14.9)
102 format('section:pos[', I0, '] ', 3ES17.9)
103 format('section:u[', I0, '] ', 3ES17.9)
104 format('section:normal[', I0, '] ', 3ES17.9)
    do j = 1, npos - 1
       ll(:, j) = psgp_bwd_ll(xy(1,j), xy(2,j), cco)
       write(*, 101) j, xy(:,j), rad2deg(ll(:,j))
    enddo
    do j = 1, npos - 1
       clat = cos(ll(2, j))
       slat = sin(ll(2, j))
       if (abs(slat).eq.ONE) clat = ZERO
       clon = cos(ll(1,j))
       slon = sin(ll(1,j))
       if (abs(slon).eq.ONE) clon = ZERO

       rn = a / sqrt(ONE - e2 * (slat * slat))
       pp(1, j) = rn * clat * clon
       pp(2, j) = rn * clat * slon
       pp(3, j) = rn * slat * ((ONE - e) * (ONE - e))
    enddo
    pp(:, 0) = (/ZERO, ZERO, -b/)

    do j = 0, npos - 1
       write(*, 102) j, pp(:,j)
    enddo
    do j = 1, npos - 1
       uu(:, j) = pp(:, j) - pp(:, 0)
       un = HYPOT(HYPOT(uu(1,j), uu(2,j)), uu(3,j))
       uu(:, j) = uu(:, j) / un
       write(*, 103) j, uu(:,j)
    enddo
    do j = 2, npos - 1
       nv(1, j) = uu(2, 1) * uu(3, j) - uu(3, 1) * uu(2, j)
       nv(2, j) = uu(3, 1) * uu(1, j) - uu(1, 1) * uu(3, j)
       nv(3, j) = uu(1, 1) * uu(2, j) - uu(2, 1) * uu(1, j)

       un = HYPOT(HYPOT(nv(1,j), nv(2,j)), nv(3,j))
       nv(:, j) = nv(:, j) / un
       write(*, 104) j, nv(:,j)
    enddo

  end subroutine test_section

!!!_ + batch_test_geod_filter
  subroutine batch_test_geod_filter(ierr)
    use TOUZA_Std,only: split_list
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr

    real(kind=KTGT) :: a, f
    integer,parameter :: lline = 1024
    integer,parameter :: lcols = 16
    character(len=lline) :: text
    integer :: nc
    real(kind=KTGT) :: v(0:lcols)
    real(kind=KTGT) :: lat1d, lon1d, lat2d, lon2d
    real(kind=KTGT) :: dlon
    real(kind=KTGT) :: gdis
    real(kind=KTGT) :: glat1(2), glat2(2), dglon(3)
    real(kind=KTGT) :: inia1(2)

    a = 6378137.0_KTGT
    f = 1.0_KTGT /298.257223563_KTGT

    ierr = 0
    do
       read(*, '(A)', IOSTAT=ierr) text
       if (ierr.ne.0) then
          ierr = 0
          exit
       endif
       call split_list(nc, v, text, ' ', lcols)
       if (nc.lt.0) then
          write(*, *) 'error in split: ', trim(text)
          ierr = nc
          exit
       endif
       if (nc.lt.4) then
          write(*, *) 'too few items: ', trim(text)
          ierr = -1
          exit
       endif
       lat1d = v(0)
       lon1d = v(1)
       lat2d = v(2)
       lon2d = v(3)
       dlon = lon2d - lon1d

       glat1(1:2) = setd_sincos(lat1d)
       glat2(1:2) = setd_sincos(lat2d)
       dglon(1:2) = setd_sincos(dlon)
       _ANGLE(dglon) = deg2rad(dlon)
       if (nc.eq.6) then
          inia1(1) = v(4)
          inia1(2) = v(5)
       else
          call geodesic_inverse_canonical(ierr, glat1, glat2, dglon)
          call geodesic_inverse_guess(ierr, inia1, glat1, glat2, deg2rad(dlon), f)
       endif
       if (ierr.eq.0) then
101       format('guess: ', 2ES24.16, 1x, F16.11)
          write(*, 101) inia1, rad2deg(atan2(_SIN(inia1), _COS(inia1)))
          call geodesic_inverse_core &
               (ierr, gdis, glat1, glat2, dglon, inia1, f, a)
111       format('geod: ', 4ES24.16, 1x, ES24.16)
          write(*, 111) v(0:3), gdis
       else
          ierr = 0
       endif
    enddo
  end subroutine batch_test_geod_filter

!!!_ + batch_test_geod
  subroutine batch_test_geod(ierr, test)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr
    integer,intent(in)  :: test
    real(kind=KTGT) :: a, e2, ep2, f
    ierr = 0
    if (test.lt.0) return ! dummy procedure

    a = 6378137.0_KTGT
    f = 1.0_KTGT /298.257223563_KTGT
    ! f = 0.0_KTGT
    e2 = 0.00669437999014132_KTGT
    ep2 = 0.00673949674227643_KTGT

    call test_geod_inv &
         & (ierr, -30.12345_KTGT, -30.12344_KTGT, 0.00005_KTGT, &
         &  -30.03999083821_KTGT, -30.03998085491_KTGT, &
         &  0.00005012589_KTGT,   0.00004452641_KTGT,   77.04353354237_KTGT, &
         &  a, f, e2)

    call test_geod_inv_ap &
         & (ierr, &
         &  -30.0_KTGT, +29.9_KTGT, 179.8_KTGT, 161.914_KTGT, &
         &  a, f, e2, ep2)

    call test_geod_inv_ap2 &
         & (ierr, &
         &  -30.0_KTGT, +29.9_KTGT, 179.8_KTGT, 161.914_KTGT, &
         &  a, f, e2, ep2)

    call test_geod_inv_ap2 &
         & (ierr, &
         &  -30.12345_KTGT, -30.12344_KTGT, 0.00005_KTGT, 0.0_KTGT, &
         &  a, f, e2, ep2)

  end subroutine batch_test_geod

  subroutine test_geod_inv &
       & (ierr, &
       &  phi1d,  phi2d,  lambda12d, &
       &  beta1d, beta2d, omega12d, sigma12d, alpha1d, &
       &  a,      f,      e2)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr
    real(kind=KTGT),intent(in) :: a, e2, f
    real(kind=KTGT),intent(in) :: phi1d,  phi2d,  lambda12d
    real(kind=KTGT),intent(in) :: beta1d, beta2d, omega12d, sigma12d
    real(kind=KTGT),intent(in) :: alpha1d

    real(kind=KTGT) :: phi1,  phi2,  lambda12
    real(kind=KTGT) :: beta1, beta2, omega12, sigma12
    real(kind=KTGT) :: alpha1

    ierr = 0

    phi1 = deg2rad(phi1d)
    phi2 = deg2rad(phi2d)
    lambda12 = deg2rad(lambda12d)

    beta1 = reduced_lat(phi1, f)
    beta2 = reduced_lat(phi2, f)

    omega12 = aux_dspherical_lon(lambda12, beta1, beta2, e2)
    sigma12 = aux_arcl_eetri(beta1, beta2, omega12)
    alpha1 = aux_azimuth1_etri(beta1, beta2, omega12)

    write(*, *) 'beta_1 = ',   rad2deg(beta1),   beta1d, beta1
    write(*, *) 'beta_2 = ',   rad2deg(beta2),   beta2d, beta2
    write(*, *) 'omega_12 = ', rad2deg(omega12), omega12d, omega12
    write(*, *) 'sigma_12 = ', rad2deg(sigma12), sigma12d, sigma12
    write(*, *) 'alpha_1 = ',  rad2deg(alpha1),  alpha1d, alpha1

  end subroutine test_geod_inv

  subroutine test_geod_inv_ap2 &
       & (ierr, &
       &  phi1d,  phi2d,  lambda12d, alpha1dini, &
       &  a,      f,      e2,        ep2)
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr
    real(kind=KTGT),intent(in) :: a, e2, ep2, f
    real(kind=KTGT),intent(in) :: phi1d,  phi2d,  lambda12d
    real(kind=KTGT),intent(in) :: alpha1dini

    real(kind=KTGT) :: dlon
    real(kind=KTGT) :: gdis
    real(kind=KTGT) :: glat1(2), glat2(2), dglon(3)
    real(kind=KTGT) :: inia1(2)

    ierr = 0

    glat1(1:2) = setd_sincos(phi1d)
    glat2(1:2) = setd_sincos(phi2d)
    dglon(1:2) = setd_sincos(lambda12d)
    ! inia1(1:2) = set_sincos(deg2rad(alpha1dini))
    inia1(1:2) = (/3.1078391672905403e-01_KTGT, -9.5048059270168606e-01_KTGT/)
    write(*, *) 'guess:', inia1
    call geodesic_inverse_core &
         (ierr, gdis, glat1, glat2, dglon, inia1, f, a)
    write(*, *) 'geod: ', gdis

    dlon = deg2rad(lambda12d)
    call geodesic_inverse_guess(ierr, inia1, glat1, glat2, dlon, f)
    write(*, *) 'guess:', inia1
    call geodesic_inverse_core &
         (ierr, gdis, glat1, glat2, dglon, inia1, f, a)
    write(*, *) 'geod: ', gdis

  end subroutine test_geod_inv_ap2

  subroutine test_geod_inv_ap &
       & (ierr, &
       &  phi1d,  phi2d,  lambda12d, alpha1dini, &
       &  a,      f,      e2,        ep2)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr
    real(kind=KTGT),intent(in) :: a, e2, ep2, f
    real(kind=KTGT),intent(in) :: phi1d,  phi2d,  lambda12d
    real(kind=KTGT),intent(in) :: alpha1dini

    real(kind=KTGT) :: phi1, phi2, lambda12

    real(kind=KTGT) :: alpha0
    real(kind=KTGT) :: beta1, alpha1, sigma1, omega1, lambda1
    real(kind=KTGT) :: beta2, alpha2, sigma2, omega2, lambda2
    real(kind=KTGT) :: kk
    real(kind=KTGT) :: xparam
    real(kind=KTGT) :: fiii

    integer,parameter :: lodr = 7
    integer,parameter :: i3odr = 5
    real(kind=KTGT) :: C3C(0:lodr, 0:lodr)
    real(kind=KTGT) :: C3(0:lodr)

    real(kind=KTGT) :: sa0, s2s, c2s
    real(kind=KTGT) :: dl

    ierr = 0

    fiii = f / (2.0_KTGT - f)

    phi1 = deg2rad(phi1d)
    phi2 = deg2rad(phi2d)
    lambda12 = deg2rad(lambda12d)

    alpha1 = deg2rad(alpha1dini)
    ! NEA
    beta1  = reduced_lat(phi1, f)
    alpha0 = azimuth0_eetri(beta1, alpha1)
    sigma1 = arcl_eetri(beta1, alpha1)
    omega1 = sphlon_eetri(sigma1, alpha0)
    write(*, *) 'beta1 = ', rad2deg(beta1), sin(beta1), sin(phi1) * (1.0_KTGT - f)
    write(*, *) 'alpha0 = ', rad2deg(alpha0)
    write(*, *) 'sigma1 = ', rad2deg(sigma1)
    write(*, *) 'omega1 = ', rad2deg(omega1)
    ! NAB
    beta2  = reduced_lat(phi2, f)
    alpha2 = acos(azimuth2cos_eetri(beta1, beta2, alpha1))
    sigma2 = arcl_eetri(beta2, alpha2)
    omega2 = sphlon_eetri(sigma2, alpha0)
    write(*, *) 'beta2 = ', rad2deg(beta2), sin(beta2), sin(phi2) * (1.0_KTGT - f)
    write(*, *) 'alpha2 = ', rad2deg(alpha2)
    write(*, *) 'sigma2 = ', rad2deg(sigma2)
    write(*, *) 'omega2 = ', rad2deg(omega2)

    ! lambda12
    kk = ep2 * (cos(alpha0) ** 2)
    xparam = expparam(alpha0, ep2)

    call gen_ctable_elongi(ierr, C3C, i3odr, fiii)
    call gen_vtable_elongi(ierr, C3, C3C, i3odr, xparam)

    sa0 = sin(alpha0)

    c2s = cos(2.0_KTGT * sigma1)
    s2s = sin(2.0_KTGT * sigma1)

    lambda1 = comp_elongi(C3, i3odr, f, omega1, sa0, sigma1, c2s, s2s)

    c2s = cos(2.0_KTGT * sigma2)
    s2s = sin(2.0_KTGT * sigma2)

    lambda2 = comp_elongi(C3, i3odr, f, omega2, sa0, sigma2, c2s, s2s)
    dl = (lambda2 - lambda1) - lambda12


    write(*, *) 'kk = ', kk
    write(*, *) 'epsilon = ', xparam
    write(*, *) 'lambda1 = ', rad2deg(lambda1)
    write(*, *) 'lambda2 = ', rad2deg(lambda2)
    write(*, *) 'diff lambda = ', rad2deg(lambda2 - lambda1)
    write(*, *) 'diff d lambda = ', rad2deg(dl)

  !   xparam = 0.00143289220416_KTGT
  !   call gen_vtable_elongi(ierr, C3, C3C, i3odr, xparam)

  !   alpha0 = deg2rad(22.55394020262_KTGT)
  !   sigma1 = deg2rad(43.99915364500_KTGT)
  !   sigma2 = deg2rad(133.92164083038_KTGT)

  !   c2s = cos(2.0_KTGT * sigma1)
  !   s2s = sin(2.0_KTGT * sigma1)

  !   write(*, *) 'epsilon = ', xparam
  !   write(*, *) 'sigma1 = ', rad2deg(sigma1)

  !   b2 = 0
  !   b1 = 0
  !   do jo = i3odr, 1, -1
  !      b0 = C3(jo) + (2.0_KTGT * c2s) * b1 - b2
  !      b2 = b1
  !      b1 = b0
  !   enddo
  !   i3 =  (sigma1 + b1 * s2s) * C3(0)
  !   write(*, *) 'I3(1) = ', i3

  !   c2s = cos(2.0_KTGT * sigma2)
  !   s2s = sin(2.0_KTGT * sigma2)

  !   write(*, *) 'sigma2 = ', rad2deg(sigma2)

  !   b2 = 0
  !   b1 = 0
  !   do jo = i3odr, 1, -1
  !      b0 = C3(jo) + (2.0_KTGT * c2s) * b1 - b2
  !      b2 = b1
  !      b1 = b0
  !   enddo
  !   i3 =  (sigma2 + b1 * s2s) * C3(0)
  !   write(*, *) 'I3(2) = ', i3
  end subroutine test_geod_inv_ap

end program test_emu_ugg

#endif /* TEST_EMU_UGG */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

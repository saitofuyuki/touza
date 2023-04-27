!!!_! emu_ugg.F90 - touza/emu geography geometry geodesy
! Maintainer: SAITO Fuyuki
! Created: Dec 23 2022
#define TIME_STAMP 'Time-stamp: <2023/05/19 08:53:04 fuyuki emu_ugg.F90>'
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
  integer,parameter,public :: ncache_stereog_lo = 2
  integer,parameter,public :: ncache_stereog_la = 2
  integer,parameter,public :: ncache_stereog_co = 6

  integer,parameter :: icache_stereog_sindlo = 1
  integer,parameter :: icache_stereog_cosdlo = 2

  integer,parameter :: icache_stereog_xrho   = 1
  integer,parameter :: icache_stereog_yrho   = 2

  integer,parameter :: icache_stereog_xco    = 1
  integer,parameter :: icache_stereog_yco    = 2
  integer,parameter :: icache_stereog_sign   = 3
  integer,parameter :: icache_stereog_ecc    = 4
  integer,parameter :: icache_stereog_olon   = 5
  integer,parameter :: icache_stereog_tcf    = 6
  integer,parameter :: icache_stereog_tol    = 7

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

  interface proj_stereog_npset
     module procedure proj_stereog_npset_d
  end interface proj_stereog_npset
  interface proj_stereog_npfwd
     module procedure proj_stereog_npfwd_d
  end interface proj_stereog_npfwd

  interface proj_stereog_spset
     module procedure proj_stereog_spset_d
  end interface proj_stereog_spset
  interface proj_stereog_spfwd
     module procedure proj_stereog_spfwd_d
  end interface proj_stereog_spfwd

  interface proj_stereog_set
     module procedure proj_stereog_set_d
  end interface proj_stereog_set
  interface proj_stereog_fwd
     module procedure proj_stereog_fwd_d
  end interface proj_stereog_fwd
  interface proj_stereog_xfwd
     module procedure proj_stereog_xfwd_d
  end interface proj_stereog_xfwd
  interface proj_stereog_yfwd
     module procedure proj_stereog_yfwd_d
  end interface proj_stereog_yfwd

  interface conformal_latitude
     module procedure conformal_latitude_d
  end interface conformal_latitude
  interface proj_tsfactor
     module procedure proj_tsfactor_d
  end interface proj_tsfactor
  interface flatten_to_ecc
     module procedure flatten_to_ecc_d
  end interface flatten_to_ecc
  interface local_radius
     module procedure local_radius_d
  end interface local_radius

  interface proj_pstereog
     module procedure proj_pstereog_core_d, proj_pstereog_once_d
  end interface proj_pstereog

  interface proj_pstereog_set
     module procedure proj_pstereog_set_d
  end interface proj_pstereog_set
  interface proj_pstereog_cachelo
     module procedure proj_pstereog_cachelo_d
  end interface proj_pstereog_cachelo
  interface proj_pstereog_cachela
     module procedure proj_pstereog_cachela_d
  end interface proj_pstereog_cachela
  interface proj_pstereog_inv
     module procedure proj_pstereog_inv_d
  end interface proj_pstereog_inv

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

!!!_  - public
  public init, diag, finalize
  public get_longitude, mid_longitude,  div_longitude
  public check_longitude, check_div_longitude
  public get_latitude,  gauss_latitude, mid_latitude, div_latitude
  public check_precision, is_equidistant
  public deg2rad, rad2deg

  public flatten_to_ecc
  public conformal_latitude,  local_radius
  public proj_stereog_set,   proj_stereog_fwd
  public proj_stereog_npset, proj_stereog_npfwd
  public proj_stereog_spset, proj_stereog_spfwd
  public proj_stereog_xfwd,  proj_stereog_yfwd
  public proj_pstereog, proj_pstereog_set, proj_pstereog_cachelo, proj_pstereog_cachela
  public proj_pstereog_inv
  public reduced_lat, reduced_lat_tan
  public aux_dspherical_lon, aux_dspherical_lon_cos
  public aux_arcl_eetri, aux_arcl_eetri_trig
  public aux_azimuth1_etri,  aux_azimuth1_etri_trig
  public azimuth0_eetri, azimuth0_eetri_trig
  public arcl_eetri, arcl_eetri_trig
  public sphlon_eetri, sphlon_eetri_trig
  public azimuth2cos_eetri, azimuth2cos_eetri_trig
  public expparam, expparam_trig

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
    r = s * (modulo(abs(deg), w) / (w / 2.0_KTGT)) * pi_(deg)
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
  ! see Snyder(1987) (https://doi.org/10.3133/pp1395)
!!!_  - proj_pstereog_set - coeff cache for sterographic projection (pole)
  subroutine proj_pstereog_set_d &
    & (cco, ecc, a, latts, lonorg, pole, xs, ys, tol)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    real(kind=KTGT),intent(out)         :: cco(*)
    real(kind=KTGT),intent(in)          :: ecc, a, latts
    real(kind=KTGT),intent(in)          :: lonorg
    integer,        intent(in)          :: pole    ! north pole == +1, south pole == -1
    real(kind=KTGT),intent(in),optional :: xs, ys  ! scale for x,y (default == 1)
    real(kind=KTGT),intent(in),optional :: tol

    real(kind=KTGT) :: mc, tc, lt, c, s
    real(kind=KTGT),parameter :: ONE=1.0_KTGT, ZERO=0.0_KTGT

    if (pole.lt.0) then
       lt = - latts
       cco(icache_stereog_sign) = - ONE
    else
       lt = + latts
       cco(icache_stereog_sign) = + ONE
    endif
    tc = proj_tsfactor(lt, ecc)
    mc = local_radius(lt, ecc)

    c = a * mc / tc
    s = choice(ONE, xs)
    if (s.le.ZERO) s = ONE
    cco(icache_stereog_xco)  = c / s
    s = choice(ONE, ys)
    if (s.le.ZERO) s = ONE
    cco(icache_stereog_yco)  = c / s

    cco(icache_stereog_ecc)  = ecc
    cco(icache_stereog_olon) = lonorg

    ! for inversion
    cco(icache_stereog_tcf) = tc / (a * mc)
    cco(icache_stereog_tol) = choice(1.0_KTGT, tol)
  end subroutine proj_pstereog_set_d
!!!_  - proj_pstereog_cachela - lat cache for sterographic projection (pole)
  subroutine proj_pstereog_cachela_d &
    & (cla, lat, cco)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out) :: cla(*)
    real(kind=KTGT),intent(in)  :: lat
    real(kind=KTGT),intent(in)  :: cco(*)
    real(kind=KTGT) :: ecc, es
    real(kind=KTGT) :: lt

    ecc = cco(icache_stereog_ecc)
    lt = lat * cco(icache_stereog_sign)
    es = proj_tsfactor(lt, ecc) * cco(icache_stereog_sign)
    cla(icache_stereog_xrho) = cco(icache_stereog_xco) * es
    cla(icache_stereog_yrho) = cco(icache_stereog_yco) * es
  end subroutine proj_pstereog_cachela_d
!!!_  - proj_pstereog_cachelo - lon cache for sterographic projection (pole)
  subroutine proj_pstereog_cachelo_d &
    & (clo, lon, cco)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out) :: clo(*)
    real(kind=KTGT),intent(in)  :: lon
    real(kind=KTGT),intent(in)  :: cco(*)

    real(kind=KTGT) :: lonorg
    real(kind=KTGT) :: dlo
    lonorg = cco(icache_stereog_olon)
    dlo = (lon - lonorg) * cco(icache_stereog_sign)
    clo(icache_stereog_sindlo) = sin(dlo)
    clo(icache_stereog_cosdlo) = cos(dlo)
  end subroutine proj_pstereog_cachelo_d

!!!_  & proj_pstereog_once
  function proj_pstereog_once_d &
       & (lon, lat, cco) result (xy)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: xy(2)
    real(kind=KTGT),intent(in) :: lon, lat
    real(kind=KTGT),intent(in) :: cco(*)
    real(kind=KTGT) :: tlo(ncache_stereog_la)
    real(kind=KTGT) :: tla(ncache_stereog_lo)

    call proj_pstereog_cachela(tla, lat, cco)
    call proj_pstereog_cachelo(tlo, lon, cco)

    xy(:) = proj_pstereog_core_d(tlo, tla)
  end function proj_pstereog_once_d

!!!_  & proj_pstereog_core
  function proj_pstereog_core_d &
       & (clo, cla) result (xy)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: xy(2)
    real(kind=KTGT),intent(in) :: clo(*)
    real(kind=KTGT),intent(in) :: cla(*)

    xy(1) = + cla(icache_stereog_xrho) * clo(icache_stereog_sindlo)
    xy(2) = - cla(icache_stereog_yrho) * clo(icache_stereog_cosdlo)
    ! xy(1:2) = xy(1:2) * cco(icache_stereog_sign)
  end function proj_pstereog_core_d

!!!_  & proj_pstereog_inv
  function proj_pstereog_inv_d &
       & (x, y, cco) result(ll)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: ll(2)
    real(kind=KTGT),intent(in) :: x, y
    real(kind=KTGT),intent(in) :: cco(*)
    real(kind=KTGT) :: rho, t, e,   he, lo,  la
    real(kind=KTGT) :: HPI, z, esl, p,  tol, s
    integer j

    HPI = pi_(0.0_KTGT) * 0.5_KTGT
    e   = cco(icache_stereog_ecc)
    tol = cco(icache_stereog_tol)
    s   = cco(icache_stereog_sign)
    he = e * 0.5_KTGT

    rho = _hypot(x, y)
    t = cco(icache_stereog_tcf) * rho
    p = t
    la = hpi - 2.0_KTGT * atan(p)

    do j = 0, lim_psginv
       esl = e * sin(p)
       z = ((1.0_KTGT - esl) / (1.0_KTGT + esl)) ** he
       la = hpi - 2.0_KTGT * atan(t * z)
       if (abs(p-la).le.spacing(la) * tol) exit
       p = la
    enddo
    ll(2) = la * s

    lo = s * cco(icache_stereog_olon) + atan2(x * s, - y * s)
    ll(1) = lo * s
  end function proj_pstereog_inv_d

!!!_ + stereographic projection (reserved)
!!!_  & proj_stereog_npset
  subroutine proj_stereog_npset_d &
       & (cc, ecc, a, latts)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out) :: cc
    real(kind=KTGT),intent(in)  :: ecc, a, latts

    real(kind=KTGT) :: mc, t

    t  = proj_tsfactor(latts, ecc)
    mc = local_radius(latts, ecc)
    cc = a * mc / t
  end subroutine proj_stereog_npset_d

!!!_  & proj_stereog_npfwd
  function proj_stereog_npfwd_d &
       & (ll, ecc, cc, llorg) result (xy)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: xy(2)
    real(kind=KTGT),intent(in) :: ll(2)
    real(kind=KTGT),intent(in) :: ecc
    real(kind=KTGT),intent(in) :: llorg(2)
    real(kind=KTGT),intent(in) :: cc           ! coefficient cache

    real(kind=KTGT) :: lon, lat
    real(kind=KTGT) :: rho
    real(kind=KTGT) :: cdl, sdl

    lon = ll(1)
    lat = ll(2)
    sdl = sin(lon - llorg(1))
    cdl = cos(lon - llorg(1))
    rho = cc * proj_tsfactor(lat, ecc)

    xy(1) = + rho * sdl
    xy(2) = - rho * cdl
  end function proj_stereog_npfwd_d

!!!_  & proj_stereog_spset
  subroutine proj_stereog_spset_d &
       & (cc, ecc, a, latts)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out) :: cc
    real(kind=KTGT),intent(in)  :: ecc, a, latts

    real(kind=KTGT) :: mc, t

    t  = proj_tsfactor(-latts, ecc)
    mc = local_radius(-latts, ecc)
    cc = a * mc / t
  end subroutine proj_stereog_spset_d

!!!_  & proj_stereog_spfwd
  function proj_stereog_spfwd_d &
       & (ll, ecc, cc, llorg) result (xy)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: xy(2)
    real(kind=KTGT),intent(in) :: ll(2)
    real(kind=KTGT),intent(in) :: ecc
    real(kind=KTGT),intent(in) :: llorg(2)
    real(kind=KTGT),intent(in) :: cc           ! coefficient cache

    real(kind=KTGT) :: lon, lat
    real(kind=KTGT) :: rho
    real(kind=KTGT) :: cdl, sdl

    lon = -ll(1)
    lat = -ll(2)
    sdl = sin(lon + llorg(1))
    cdl = cos(lon + llorg(1))
    rho = cc * proj_tsfactor(lat, ecc)

    xy(1) = - rho * sdl
    xy(2) = + rho * cdl
  end function proj_stereog_spfwd_d

!!!_  & proj_stereog_set
  subroutine proj_stereog_set_d &
       & (cc, sinx1, cosx1, llorg, ecc, a, k0)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out) :: cc, sinx1, cosx1
    real(kind=KTGT),intent(in)  :: llorg(2)
    real(kind=KTGT),intent(in)  :: ecc, a, k0

    real(kind=KTGT) :: lc, m1

    lc = conformal_latitude(llorg(2), ecc)
    cosx1 = cos(lc)
    sinx1 = sin(lc)
    m1 = local_radius(llorg(2), ecc)
    cc = 2.0_KTGT * a * k0 * m1
  end subroutine proj_stereog_set_d

!!!_  & proj_stereog_fwd
  function proj_stereog_fwd_d &
       & (ll, ecc, cc, llorg, sinx1, cosx1) result (xy)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: xy(2)
    real(kind=KTGT),intent(in) :: ll(2)
    real(kind=KTGT),intent(in) :: ecc
    real(kind=KTGT),intent(in) :: llorg(2)
    real(kind=KTGT),intent(in) :: cc           ! coefficient cache == (2 a m1 k)
    real(kind=KTGT),intent(in) :: sinx1, cosx1

    real(kind=KTGT) :: lon, lat
    real(kind=KTGT) :: lc, sx, cx, sdl, cdl, AA

    lon = ll(1)
    lat = ll(2)

    lc = conformal_latitude(lat, ecc)
    cx = cos(lc)
    sx = sin(lc)
    sdl = sin(lon - llorg(1))
    cdl = cos(lon - llorg(1))

    AA = cc / (cosx1 * (1.0_KTGT + sinx1 * sx + cosx1 * cx * cdl))

    xy(1) = AA * cx * sdl
    xy(2) = AA * (cosx1 * sx - sinx1 * cx * cdl)
  end function proj_stereog_fwd_d

!!!_  & proj_stereog_xfwd
  ELEMENTAL &
  real(kind=KTGT) function proj_stereog_xfwd_d &
       & (lon, lat, ecc, lon0, cc, sinx1, cosx1) result (x)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: lon, lat
    real(kind=KTGT),intent(in) :: ecc
    real(kind=KTGT),intent(in) :: lon0
    real(kind=KTGT),intent(in) :: cc, sinx1, cosx1

    real(kind=KTGT) :: lc, sx, cx, sdl, cdl, AA
    lc = conformal_latitude(lat, ecc)
    cx = cos(lc)
    sx = sin(lc)
    sdl = sin(lon - lon0)
    cdl = cos(lon - lon0)

    AA = cc / (cosx1 * (1.0_KTGT + sinx1 * sx + cosx1 * cx * cdl))

    x = AA * cx * sdl
  end function proj_stereog_xfwd_d
!!!_  & proj_stereog_yfwd
  ELEMENTAL &
  real(kind=KTGT) function proj_stereog_yfwd_d &
       & (lon, lat, ecc, lon0, cc, sinx1, cosx1) result (y)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: lon, lat
    real(kind=KTGT),intent(in) :: ecc
    real(kind=KTGT),intent(in) :: lon0
    real(kind=KTGT),intent(in) :: cc, sinx1, cosx1

    real(kind=KTGT) :: lc, sx, cx, cdl, AA
    lc = conformal_latitude(lat, ecc)
    cx = cos(lc)
    sx = sin(lc)
    cdl = cos(lon - lon0)

    AA = cc / (cosx1 * (1.0_KTGT + sinx1 * sx + cosx1 * cx * cdl))

    y = AA * (cosx1 * sx - sinx1 * cx * cdl)
  end function proj_stereog_yfwd_d

!!!_  & proj_tsfactor()
  ELEMENTAL &
  real(kind=KTGT) function proj_tsfactor_d (lat, ecc) result (x)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: lat ! in radian
    real(kind=KTGT),intent(in) :: ecc
    ! real(kind=KTGT) :: p
    real(kind=KTGT) :: esl, thl
    real(kind=KTGT),parameter :: ONE  = 1.0_KTGT
    real(kind=KTGT),parameter :: HALF = 0.5_KTGT

    ! tan(a-b)    = (tan(a) - tan(b))/(1 + tan(a)tan(b))
    ! tan(pi/4-b) = (1      - tan(b))/(1 +       tan(b))

    ! p = pi_(lat)
    esl = ecc * sin(lat)
    thl = tan(HALF * lat)
    x = (ONE - thl) / (ONE + thl) &
         & / (((ONE - esl) / (ONE + esl)) ** (HALF * ecc))
    ! x = tan(HALF * (HALF * p - lat)) &
    !      & / (((ONE - esl) / (ONE + esl)) ** (HALF * ecc))
    return
  end function proj_tsfactor_d

!!!_  & flatten_to_ecc ()
  ELEMENTAL &
  real(kind=KTGT) function flatten_to_ecc_d (f) result (e)
    use TOUZA_Std,only: KTGT=>KDBL
    real(kind=KTGT),intent(in) :: f
    e = sqrt(2.0_KTGT * f - f * f)
  end function flatten_to_ecc_d

!!!_  & conformal_latitude()
  ELEMENTAL &
  real(kind=KTGT) function conformal_latitude_d (lat, ecc) result (x)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: lat ! in radian
    real(kind=KTGT),intent(in) :: ecc
    real(kind=KTGT) :: p
    real(kind=KTGT) :: esl
    real(kind=KTGT),parameter :: TWO  = 2.0_KTGT
    real(kind=KTGT),parameter :: ONE  = 1.0_KTGT
    real(kind=KTGT),parameter :: HALF = 0.5_KTGT

    p = pi_(lat)
    esl = ecc * sin(lat)
    x = tan(HALF * (HALF * p + lat)) &
         & * (((ONE - esl) / (ONE + esl)) ** (HALF * ecc))
    x = TWO * atan(x) - HALF * p
    return
  end function conformal_latitude_d

!!!_  & local_radius() - return local radius factor (m)
  ELEMENTAL &
  real(kind=KTGT) function local_radius_d(lat, ecc) result (m)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: lat ! in radian
    real(kind=KTGT),intent(in) :: ecc
    m = cos(lat) / sqrt(1.0_KTGT - (ecc * sin(lat))**2.0_KTGT)
  end function local_radius_d

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
  integer stereo,  geod
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

  if (ierr.eq.0) call test_ugg_prec(ierr)

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
     if (geod.ge.0) call batch_test_geod(ierr, geod)
  endif

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

    a = 6378388.0_KTGT
    e = 0.0819919_KTGT
    latts = -71.0_KTGT
    lon0  = -100.0_KTGT
    call test_stereog_single(ierr, a, e, latts, lon0, (/150.0_KTGT, -75.0_KTGT/))
    latts = +71.0_KTGT
    lon0  = +100.0_KTGT
    call test_stereog_single(ierr, a, e, latts, lon0, (/-150.0_KTGT, +75.0_KTGT/))
    call test_stereog_single(ierr, a, e, latts, lon0, (/-150.0_KTGT, -75.0_KTGT/))

    a = 6378137.0_KTGT
    e = flatten_to_ecc(1.0_KTGT/298.257223563_KTGT)
    ! latts = 0.0_KTGT
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
    real(kind=KTGT) :: cc
    real(kind=KTGT) :: rr(2), rts, llorg(2)
    real(kind=KTGT) :: xx(2), xxo(2), xxc(2), lli(2)
    real(kind=KTGT) :: cco(ncache_stereog_co), clo(ncache_stereog_lo), cla(ncache_stereog_la)
    integer pole
    ierr = 0
    rts = deg2rad(latts)
    rr(1:2) = deg2rad(ll(1:2))
    if (latts.ge.0.0_KTGT) then
       llorg(1:2) = deg2rad((/lon0, 90.0_KTGT/))
       call proj_stereog_npset(cc, e, a,  rts)
       xx = proj_stereog_npfwd(rr, e, cc, llorg)
    else
       llorg(1:2) = deg2rad((/lon0, -90.0_KTGT/))
       call proj_stereog_spset(cc, e, a,  rts)
       xx = proj_stereog_spfwd(rr, e, cc, llorg)
    endif
101 format('stereog[', 2F9.1, '] ', '(', 2E16.8, ') >> (', 2E16.8, ')')
102 format('  pstereog[', A, '] ', L1, 1x, ' >> (', 2E16.8, ')')
103 format('  inverse', 2E16.8, 1x, 2E16.8, 2E16.8)
    write(*, 101) lon0, latts, ll, xx
    ! test cache once
    if (latts.ge.0.0_KTGT) then
       pole = +1
    else
       pole = -1
    endif
    call proj_pstereog_set(cco, e, a, rts, llorg(1), pole)
    ! write(*, *) cc.eq.cco(1), cc, cco
    xxo = proj_pstereog(rr(1), rr(2), cco)
    write(*, 102) 'once', ALL(xxo(:).eq.xx(:)), xxo
    ! test cache core
    call proj_pstereog_cachela(cla, rr(2), cco)
    call proj_pstereog_cachelo(clo, rr(1), cco)
    xxc = proj_pstereog(clo, cla)
    write(*, 102) 'cache', ALL(xxc(:).eq.xx(:)), xxc

    lli = proj_pstereog_inv(xxc(1), xxc(2), cco)

    write(*, 103) rad2deg(lli(:)), rad2deg(rr(:)), rad2deg(rr(:) - lli(:))

  end subroutine test_stereog_single

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
    real(kind=KTGT) :: beta1, alpha1, sigma1, omega1
    real(kind=KTGT) :: beta2, alpha2, sigma2, omega2
    real(kind=KTGT) :: kk
    real(kind=KTGT) :: xparam

    ierr = 0

    phi1 = deg2rad(phi1d)
    phi2 = deg2rad(phi2d)
    lambda12 = deg2rad(lambda12d)

    alpha1 = deg2rad(alpha1dini)
    ! NEA
    beta1  = reduced_lat(phi1, f)
    alpha0 = azimuth0_eetri(beta1, alpha1)
    sigma1 = arcl_eetri(beta1, alpha1)
    omega1 = sphlon_eetri(sigma1, alpha0)
    write(*, *) 'beta1 = ', rad2deg(beta1)
    write(*, *) 'alpha0 = ', rad2deg(alpha0)
    write(*, *) 'sigma1 = ', rad2deg(sigma1)
    write(*, *) 'omega1 = ', rad2deg(omega1)
    ! NAB
    beta2  = reduced_lat(phi2, f)
    alpha2 = acos(azimuth2cos_eetri(beta1, beta2, alpha1))
    sigma2 = arcl_eetri(beta2, alpha2)
    omega2 = sphlon_eetri(sigma2, alpha0)
    write(*, *) 'beta2 = ', rad2deg(beta2)
    write(*, *) 'alpha2 = ', rad2deg(alpha2)
    write(*, *) 'sigma2 = ', rad2deg(sigma2)
    write(*, *) 'omega2 = ', rad2deg(omega2)

    ! lambda12
    kk = ep2 * (cos(alpha0) ** 2)
    xparam = expparam(alpha0, ep2)

    write(*, *) 'kk = ', kk
    write(*, *) 'epsilon = ', xparam
  end subroutine test_geod_inv_ap

end program test_emu_ugg

#endif /* TEST_EMU_UGG */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

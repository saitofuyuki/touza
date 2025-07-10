!!!_! emu_ugg.F90 - touza/emu geography geometry geodesy
! Maintainer: SAITO Fuyuki
! Created: Dec 23 2022
#define TIME_STAMP 'Time-stamp: <2025/07/10 12:37:47 fuyuki emu_ugg.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022, 2023, 2024, 2025
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
#endif
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

#define _SINIDX 1
#define _COSIDX 2

#define _SIN(TERM) TERM(_SINIDX)
#define _COS(TERM) TERM(_COSIDX)
#define _ANGLE(TERM) TERM(3)
#define _SIN2(TERM) (_SIN(TERM)**2)
#define _COS2(TERM) (_COS(TERM)**2)

#define _TRIG(TERM) TERM(_SINIDX:_COSIDX)

#define _LAIDX 1
#define _LOIDX 2

#define _LATI(TERM)  TERM(_LAIDX)
#define _LONGI(TERM) TERM(_LOIDX)

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

  integer,parameter,public :: NTRIG = 2
  integer,parameter,public :: JSIN = _SINIDX, JCOS = _COSIDX

  integer,parameter,public :: NGEOG = 2
  integer,parameter,public :: JLATI = _LAIDX, JLONGI = _LOIDX

  integer,parameter,public :: NATRI = 3
  integer,parameter,public :: JAMP = NATRI   ! != JSIN, JCOS

  integer,parameter,public :: round_2pi    = 0
  integer,parameter,public :: round_degree = 360

  ! symmetric properties
  integer,parameter,public :: symm_error = -1
  integer,parameter,public :: symm_asymm = 0
  integer,parameter,public :: symm_odd   = 1        ! 1: symmetric flag
  integer,parameter,public :: symm_even  = 1 + 2    ! 2: even flag
  integer,parameter,public :: symm_span  = 4

  ! monotonic properties
  integer,parameter,public :: non_monotonic = 0
  integer,parameter,public :: equidistant_strict = 1   ! sign denotes ascending/descending
  integer,parameter,public :: equidistant_enough = 2
  integer,parameter,public :: non_equidistant = 4
!!!_ + polar steregraphic projection: cache size and index
  integer,parameter,public :: ncache_psgp_lo = 2
  integer,parameter,public :: ncache_psgp_la = 5
  integer,parameter,public :: ncache_psgp_co = 15

  integer,parameter :: icache_psgp_sindlo = 1
  integer,parameter :: icache_psgp_cosdlo = 2

  integer,parameter :: icache_psgp_xrho   = 1
  integer,parameter :: icache_psgp_yrho   = 2
  integer,parameter :: icache_psgp_sf     = 3
  integer,parameter :: icache_psgp_sinla  = 4
  integer,parameter :: icache_psgp_cosla  = 5

  integer,parameter :: icache_psgp_xco  = 1  ! major / x-scale
  integer,parameter :: icache_psgp_yco  = 2  ! major / y-scale
  integer,parameter :: icache_psgp_sign = 3
  integer,parameter :: icache_psgp_ecc  = 4
  integer,parameter :: icache_psgp_flat = 5
  integer,parameter :: icache_psgp_olon = 6
  integer,parameter :: icache_psgp_tcf  = 7  ! scale factor parameter
  integer,parameter :: icache_psgp_atcf = 8  ! rho factor
  integer,parameter :: icache_psgp_tol  = 9  !
  integer,parameter :: icache_psgp_maj  = 10 ! major
  integer,parameter :: icache_psgp_psf  = 11 ! scale factor at pole
  integer,parameter :: icache_psgp_e2c  = 12 ! (1 - e^2)
  integer,parameter :: icache_psgp_aco  = 13 ! 1 + e2c (atanh(e) / e)
  integer,parameter :: icache_psgp_tslat= 14 ! true latitude
  integer,parameter :: icache_psgp_loround = 15   ! one cycle in longitude

  integer,parameter :: lim_psginv = OPT_PSGINV_ITER_LIMIT
!!!_ + symmetric tripolar
  integer,parameter :: icache_stp_plon = 1       ! longitude of w-sphere np
  integer,parameter :: icache_stp_plat = 2       !   (sp: lat-np, 180+lon-np)
  integer,parameter :: icache_stp_olat = 3       ! pole latitude of the other side of w-sphere
  integer,parameter :: icache_stp_splat= 4       ! sine(plat)
  integer,parameter :: icache_stp_solat= 5       ! sine(olat). either +1 or -1 (used as sign)
  integer,parameter :: icache_stp_wnpr = 6       ! real-part of w-sphere np projection
  integer,parameter :: icache_stp_loround = 7    ! round angle for longitude
  integer,parameter :: icache_stp_laround = 8    ! round angle for latitude
  integer,parameter,public :: ncache_stp_co = 8

  integer,parameter :: icache_stp_zamp = 1
  integer,parameter :: icache_stp_zclo = 1
  integer,parameter :: icache_stp_zslo = 2

!!!_ + private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = EMU_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global

#define _ERROR(E) (E - ERR_MASK_EMU_UGG)
# define __MDL__ 'ugg'
!!!_ + interfaces
!!!_  - stp
  interface stp_geogr_zproj
     module procedure stp_geogr_zproj_tr_d
  end interface stp_geogr_zproj
  interface stp_wsphere_wproj_tr
     module procedure stp_wsphere_wproj_tr_d
  end interface stp_wsphere_wproj_tr
  interface stp_z2wproj_atr
     module procedure stp_z2wproj_atr_d
  end interface stp_z2wproj_atr
  interface stp_w2zproj_atr
     module procedure stp_w2zproj_atr_d
  end interface stp_w2zproj_atr
  interface stp_wsphere_lat_tr
     module procedure stp_wsphere_lat_tr_d
  end interface stp_wsphere_lat_tr
  interface stp_wsphere_lon_tr
     module procedure stp_wsphere_lon_tr_d
  end interface stp_wsphere_lon_tr
  interface stp_geogr_lat_tr
     module procedure stp_geogr_lat_tr_d
  end interface stp_geogr_lat_tr
  interface stp_geogr_lon_tr
     module procedure stp_geogr_lon_tr_d
  end interface stp_geogr_lon_tr
  interface is_stp_wsphere_tr
     module procedure is_stp_wsphere_tr_d
  end interface is_stp_wsphere_tr
  interface stp_fwd_tr
     module procedure stp_fwd_tr_d
  end interface stp_fwd_tr
  interface stp_bwd_tr
     module procedure stp_bwd_tr_d
  end interface stp_bwd_tr
  interface stp_set
     module procedure stp_set_d
  end interface stp_set
!!!_  - to clean
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

  interface round_choice
     module procedure round_choice_d, round_choice_id
  end interface round_choice
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  interface round_choice
     module procedure round_choice_q, round_choice_iq
  end interface round_choice
#endif

  interface span_longitude
     module procedure span_longitude_d
  end interface span_longitude
  interface span_latitude
     module procedure span_latitude_d
  end interface span_latitude

  interface reduced_latitude
     module procedure reduced_latitude_d
  end interface reduced_latitude
  interface azimuth_node
     module procedure azimuth_node_d
  end interface azimuth_node
  interface arcl_auxsph
     module procedure arcl_auxsph_d
  end interface arcl_auxsph
  interface alon_auxsph_ph
     module procedure alon_auxsph_ph_d
  end interface alon_auxsph_ph

  interface conformal_latitude
     module procedure conformal_latitude_d
  end interface conformal_latitude
  interface flatten_to_ecc
     module procedure flatten_to_ecc_d
  end interface flatten_to_ecc

  interface psgp_set_byf
     module procedure psgp_set_byf_d
  end interface psgp_set_byf
  interface psgp_set
     module procedure psgp_set_d
  end interface psgp_set
  interface psgp_set_step
     module procedure psgp_set_step_d
  end interface psgp_set_step
  interface psgp_inquire
     module procedure psgp_inquire_d
  end interface psgp_inquire

  interface psgp_fwd
     module procedure psgp_fwd_core_d, psgp_fwd_once_d
  end interface psgp_fwd
  interface psgp_fwd_cis
     module procedure psgp_fwd_cis_core_d, psgp_fwd_cis_once_d
  end interface psgp_fwd_cis
  interface psgp_fwd_cis_core
     module procedure psgp_fwd_cis_core_d
  end interface psgp_fwd_cis_core

  interface psgp_fwd_isf
     module procedure psgp_fwd_isf_d
  end interface psgp_fwd_isf
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
  ! interface psgp_bwd_risf
  !    module procedure psgp_bwd_risf_d
  ! end interface psgp_bwd_risf

  interface psgp_bwd_iaf
     module procedure psgp_bwd_iaf_d
  end interface psgp_bwd_iaf
  interface psgp_bwd_rho
     module procedure psgp_bwd_rho_d
  end interface psgp_bwd_rho
  interface psgp_bwd_rhonml
     module procedure psgp_bwd_rhonml_d
  end interface psgp_bwd_rhonml
  interface psgp_bwd_core
     module procedure psgp_bwd_core_d
  end interface psgp_bwd_core
  interface psgp_bwd_length
     module procedure psgp_bwd_length_d
  end interface psgp_bwd_length
  interface psgp_bwd_area
     module procedure psgp_bwd_area_d
  end interface psgp_bwd_area
  interface psgp_bwd_area2
     module procedure psgp_bwd_area2_d
  end interface psgp_bwd_area2
  interface psgp_fwd_core
     module procedure psgp_fwd_core_d
  end interface psgp_fwd_core
  interface psgp_cachela
     module procedure psgp_cachela_d
  end interface psgp_cachela
  interface psgp_cachelo
     module procedure psgp_cachelo_d, psgp_cachelo_tr_d
  end interface psgp_cachelo
  interface psgp_cachelo_px
     module procedure psgp_cachelo_px_d
  end interface psgp_cachelo_px
  interface psgp_bwd_geod
     module procedure psgp_bwd_geod_d
  end interface psgp_bwd_geod

  interface psgp_xlo_tr
     module procedure psgp_xlo_tr_d
  end interface psgp_xlo_tr
  interface psgp_ylo_tr
     module procedure psgp_ylo_tr_d
  end interface psgp_ylo_tr
  interface psgp_xla_tr
     module procedure psgp_xla_tr_d
  end interface psgp_xla_tr
  interface psgp_yla_tr
     module procedure psgp_yla_tr_d
  end interface psgp_yla_tr

  interface psgp_xla_once_tr
     module procedure psgp_xla_once_tr_d
  end interface psgp_xla_once_tr
  interface psgp_yla_once_tr
     module procedure psgp_yla_once_tr_d
  end interface psgp_yla_once_tr
  interface psgp_xlo_once_tr
     module procedure psgp_xlo_once_tr_d
  end interface psgp_xlo_once_tr
  interface psgp_ylo_once_tr
     module procedure psgp_ylo_once_tr_d
  end interface psgp_ylo_once_tr

  interface psgp_dlo_tr
     module procedure psgp_dlo_tr_d
  end interface psgp_dlo_tr
  interface psgp_gla_tr
     module procedure psgp_gla_tr_d
  end interface psgp_gla_tr
  interface psgp_sinlat
     module procedure psgp_sinlat_d
  end interface psgp_sinlat
  interface psgp_comp_dlon
     module procedure psgp_comp_dlon_d
  end interface psgp_comp_dlon

  interface psgp_surf_area
     module procedure psgp_surf_area_d
  end interface psgp_surf_area

  interface pi_
     module procedure pi_d
#if OPT_REAL_QUADRUPLE_DIGITS > 0
     module procedure pi_q
#endif
  end interface pi_


  interface div_coordinate
     module procedure div_coordinate_d
  end interface div_coordinate
  interface ang2rad
     module procedure ang2rad_d
  end interface ang2rad
  interface ang2deg
     module procedure ang2deg_d
  end interface ang2deg
  interface deg2ang
     module procedure deg2ang_d
  end interface deg2ang
  interface rad2ang
     module procedure rad2ang_d
  end interface rad2ang
  interface deg2rad
     module procedure deg2rad_d
  end interface deg2rad
  interface rad2deg
     module procedure rad2deg_d
  end interface rad2deg
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  interface rad2ang
     module procedure rad2ang_q
  end interface rad2ang
  interface ang2rad
     module procedure ang2rad_q
  end interface ang2rad
  interface ang2deg
     module procedure ang2deg_q
  end interface ang2deg
  interface deg2ang
     module procedure deg2ang_q
  end interface deg2ang
  interface deg2rad
     module procedure deg2rad_q
  end interface deg2rad
  interface rad2deg
     module procedure rad2deg_q
  end interface rad2deg
#endif

  interface gen_ctable_elongi
     module procedure gen_ctable_elongi_d
  end interface gen_ctable_elongi
  interface gen_vtable_elongi
     module procedure gen_vtable_elongi_d
  end interface gen_vtable_elongi
  interface gen_ctable_earea
     module procedure gen_ctable_earea_d
  end interface gen_ctable_earea
  interface gen_vtable_earea
     module procedure gen_vtable_earea_d
  end interface gen_vtable_earea
  interface gen_ctable_I1
     module procedure gen_ctable_I1_d
  end interface gen_ctable_I1
  interface gen_ctable_I1p
     module procedure gen_ctable_I1p_d
  end interface gen_ctable_I1p
  interface gen_ctable_I2b
     module procedure gen_ctable_I2b_d
  end interface gen_ctable_I2b
  interface gen_ctable_I2a
     module procedure gen_ctable_I2a_d
  end interface gen_ctable_I2a

  interface geodesic_direct_core
     module procedure geodesic_direct_core_d
  end interface geodesic_direct_core
  interface geodesic_direct
     module procedure geodesic_direct_d
  end interface geodesic_direct
  interface geodesic_target
     module procedure geodesic_target_d
  end interface geodesic_target
  interface geodesic_inverse
     module procedure geodesic_inverse_d
  end interface geodesic_inverse
  interface geodesic_inverse_solve
     module procedure geodesic_inverse_solve_d
  end interface geodesic_inverse_solve
  interface geodesic_inverse_core
     module procedure geodesic_inverse_core_d
  end interface geodesic_inverse_core
  interface geodesic_inverse_canonical
     module procedure geodesic_inverse_canonical_d
  end interface geodesic_inverse_canonical
  interface geodesic_inverse_guess
     module procedure geodesic_inverse_guess_d
  end interface geodesic_inverse_guess
  interface geodesic_area_obs
     module procedure geodesic_area_obs_d
  end interface geodesic_area_obs
  interface geodesic_area_core
     module procedure geodesic_area_core_d
  end interface geodesic_area_core
  interface geodesic_dazim
     module procedure geodesic_dazim_d
#if OPT_REAL_QUADRUPLE_DIGITS > 0
     module procedure geodesic_dazim_q
#endif
  end interface geodesic_dazim

  interface set_diff_xinteg
     module procedure set_diff_xinteg_d
  end interface set_diff_xinteg
  interface comp_diff_xinteg
     module procedure comp_diff_xinteg_d
  end interface comp_diff_xinteg
  interface set_diff_ainteg
     module procedure set_diff_ainteg_d
  end interface set_diff_ainteg
  interface comp_diff_ainteg
     module procedure comp_diff_ainteg_d
  end interface comp_diff_ainteg
  interface comp_series_sine
     module procedure comp_series_sine_d
  end interface comp_series_sine

  interface phase
     module procedure phase_1_d, phase_2_d
#if OPT_REAL_QUADRUPLE_DIGITS > 0
     module procedure phase_1_q, phase_2_q
#endif
  end interface phase

  interface phased
     module procedure phased_1_d, phased_2_d
#if OPT_REAL_QUADRUPLE_DIGITS > 0
     module procedure phased_1_q, phased_2_q
#endif
  end interface phased

  interface nml_sincos
     module procedure nml_sincos_d
  end interface nml_sincos

  interface set_sincos
     module procedure setr_sincos_d
     module procedure sets_sincos_d
  end interface set_sincos
  interface sets_sincos
     module procedure sets_sincos_d
  end interface sets_sincos
  interface setd_sincos
     module procedure setd_sincos_d
  end interface setd_sincos
  interface setr_sincos
     module procedure setr_sincos_d
  end interface setr_sincos
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  interface setd_sincos
     module procedure setd_sincos_q
  end interface setd_sincos
  interface setr_sincos
     module procedure setr_sincos_q
  end interface setr_sincos
  interface sets_sincos
     module procedure sets_sincos_q
  end interface sets_sincos
#endif

  interface sin_canonical
     module procedure sinr_canonical_d
     module procedure sins_canonical_d
  end interface sin_canonical
  interface cos_canonical
     module procedure cosr_canonical_d
     module procedure coss_canonical_d
  end interface cos_canonical

  interface sins_canonical
     module procedure sins_canonical_d
  end interface sins_canonical
  interface coss_canonical
     module procedure coss_canonical_d
  end interface coss_canonical

  interface sinr_canonical
     module procedure sinr_canonical_d
  end interface sinr_canonical
  interface cosr_canonical
     module procedure cosr_canonical_d
  end interface cosr_canonical

  interface sind_canonical
     module procedure sind_canonical_d
  end interface sind_canonical
  interface cosd_canonical
     module procedure cosd_canonical_d
  end interface cosd_canonical

  interface radian_modulo
     module procedure radian_modulo_d
  end interface radian_modulo

  interface degree_modulo
     module procedure degree_modulo_d
  end interface degree_modulo

  interface set_dlongi
     module procedure set_dlongi_d
  end interface set_dlongi

  interface add_angle
     module procedure add_angle_d
  end interface add_angle
  interface sub_angle
     module procedure sub_angle_d
  end interface sub_angle
  interface sub_sin
     module procedure sub_sin_d
  end interface sub_sin

#if OPT_REAL_QUADRUPLE_DIGITS > 0
  interface add_angle
     module procedure add_angle_q
  end interface add_angle
  interface sub_angle
     module procedure sub_angle_q
  end interface sub_angle
#endif

  interface agmpc_gen_table
     module procedure agmpc_gen_table_d
  end interface agmpc_gen_table
  interface agmpc_area_core
     module procedure agmpc_area_core_d
  end interface agmpc_area_core

  interface agmpd_gen_table
     module procedure agmpd_gen_table_d
  end interface agmpd_gen_table
  interface agmpd_area_core
     module procedure agmpd_area_core_d
  end interface agmpd_area_core

  interface agmpe_gen_table
     module procedure agmpe_gen_table_d
  end interface agmpe_gen_table
  interface agmpe_area_core
     module procedure agmpe_area_core_d
  end interface agmpe_area_core

  interface hpsub_sin
     module procedure hpsub_sin_d
  end interface hpsub_sin
  interface hpsub_angle
     module procedure hpsub_angle_d
  end interface hpsub_angle
  interface hpadd_angle
     module procedure hpadd_angle_d
  end interface hpadd_angle

  interface hpInProduct
     module procedure hpInProduct_d
  end interface hpInProduct
  interface hpAdd
     module procedure hpAdd_d
  end interface hpAdd
  interface hpMult
     module procedure hpMult_d
  end interface hpMult
  interface hpSplit
     module procedure hpSplit_d
  end interface hpSplit

  interface check_symmetric
     module procedure check_symmetric_d
  end interface check_symmetric
  interface check_monotonic
     module procedure check_monotonic_d
  end interface check_monotonic
  interface is_equidistant
     module procedure is_equidistant_d
  end interface is_equidistant

  interface area_distortion
     module procedure area_distortion_d
  end interface area_distortion

  interface diag_sc
     module procedure diag_sc_d
  end interface diag_sc
  interface diag_ph
     module procedure diag_ph_d
  end interface diag_ph

!!!_ + public
  public init, diag, finalize
  public get_longitude, mid_longitude,  div_longitude
  public check_longitude, check_div_longitude
  public get_latitude,  gauss_latitude, mid_latitude, div_latitude
  public check_symmetric, check_monotonic
  public check_precision, is_equidistant
  public deg2rad, rad2deg, ang2rad, rad2ang, ang2deg, deg2ang
  public round_choice, span_longitude,   span_latitude

  public :: stp_set
  public :: stp_fwd_tr, stp_bwd_tr
  public :: stp_geogr_zproj, stp_z2wproj_atr, stp_wsphere_lon_tr, stp_wsphere_lat_tr
  public :: is_stp_wsphere_tr

  public flatten_to_ecc

  public psgp_set_byf, psgp_set, psgp_cachela, psgp_cachelo
  public psgp_cachelo_px
  public psgp_set_step
  public psgp_inquire
  public psgp_fwd,    psgp_fwd_isf, psgp_fwd_cis
  public psgp_bwd_ll, psgp_bwd_tr
  public psgp_bwd_sf, psgp_bwd_isf, psgp_bwd_iaf
  public psgp_bwd_length, psgp_bwd_area, psgp_bwd_area2
  public psgp_comp_dlon
  public psgp_bwd_geod
  public psgp_surf_area
  public psgp_xlo_tr, psgp_ylo_tr
  public psgp_xla_tr, psgp_yla_tr, psgp_xla_once_tr, psgp_yla_once_tr
  public psgp_dlo_tr, psgp_gla_tr, psgp_sinlat

  public geodesic_direct, geodesic_target
  public geodesic_inverse
  public geodesic_inverse_canonical, geodesic_inverse_guess
  public geodesic_direct_core,       geodesic_inverse_core
  public geodesic_area_core
  public geodesic_dazim

  public set_dlongi
  public degree_modulo, setd_sincos, sind_canonical, cosd_canonical
  public radian_modulo,  set_sincos,  sin_canonical,  cos_canonical
  public reduced_latitude, azimuth_node, alon_auxsph_ph

  public agmpc_table_size, agmpc_gen_table, agmpc_area_core
  public agmpd_gen_table, agmpd_area_core
  public agmpe_gen_table, agmpe_area_core

  public nml_sincos
  public phase,     phased
  public add_angle, sub_angle, sub_sin

  public hpsub_angle, hpadd_angle, hpsub_sin
  public diag_sc, diag_ph
  public area_distortion

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
       function f(x)
         use TOUZA_Std,only: KTGT=>KDBL
         implicit none
         real(kind=KTGT) :: f
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
  function wsin_d(x) result(z)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: z
    real(kind=KTGT),intent(in) :: x
    z = sin(x)
  end function wsin_d
!!!_   . wcos()
  function wcos_d(x) result(z)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: z
    real(kind=KTGT),intent(in) :: x
    z = cos(x)
  end function wcos_d
!!!_   . wtan()
  function wtan_d(x) result(z)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: z
    real(kind=KTGT),intent(in) :: x
    z = tan(x)
  end function wtan_d
!!!_   . ang2rad
  ELEMENTAL &
  function ang2rad_d(angl, round) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: r
    real(kind=KTGT),intent(in) :: angl
    real(kind=KTGT),intent(in) :: round
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT) :: s
    if (round.eq.real(round_2pi, kind=KTGT)) then
       r = angl
       return
    endif
    s = sign(ONE, angl)
    r = s * (modulo(abs(angl), round) * (pi_(angl) / (round / 2.0_KTGT)))
  end function ang2rad_d
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  ELEMENTAL &
  function ang2rad_q(angl, round) result(r)
    use TOUZA_Std,only: KTGT=>KQPL
    implicit none
    real(kind=KTGT) :: r
    real(kind=KTGT),intent(in) :: angl
    real(kind=KTGT),intent(in) :: round
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT) :: s
    if (round.eq.real(round_2pi, kind=KTGT)) then
       r = angl
       return
    endif
    s = sign(ONE, angl)
    r = s * (modulo(abs(angl), round) * (pi_(angl) / (round / 2.0_KTGT)))
  end function ang2rad_q
#endif
!!!_   . rad2ang
  ELEMENTAL &
  function rad2ang_d(rad, round) result(a)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: a
    real(kind=KTGT),intent(in) :: rad
    real(kind=KTGT),intent(in) :: round
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT) :: p2, s
    if (round.eq.real(round_2pi, kind=KTGT)) then
       a = rad
       return
    endif
    p2 = round_choice(round_2pi, p2)
    ! p2 = 2.0_KTGT * pi_(rad)
    s = sign(ONE, rad)
    a = s * ((modulo(abs(rad), p2)) / p2) * round
  end function rad2ang_d
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  ELEMENTAL &
  function rad2ang_q(rad, round) result(a)
    use TOUZA_Std,only: KTGT=>KQPL
    implicit none
    real(kind=KTGT) :: a
    real(kind=KTGT),intent(in) :: rad
    real(kind=KTGT),intent(in) :: round
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT) :: p2, s
    if (round.eq.real(round_2pi, kind=KTGT)) then
       a = rad
       return
    endif
    p2 = round_choice(round_2pi, p2)
    ! p2 = 2.0_KTGT * pi_(rad)
    s = sign(ONE, rad)
    a = s * ((modulo(abs(rad), p2)) / p2) * round
  end function rad2ang_q
#endif
!!!_   . ang2deg
  ELEMENTAL &
  function ang2deg_d(angl, round) result(d)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: d
    real(kind=KTGT),intent(in) :: angl
    real(kind=KTGT),intent(in) :: round
    real(kind=KTGT),parameter :: w = real(round_degree, kind=KTGT)
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT) :: s, sp
    if (round.eq.w) then
       d = angl
       return
    endif
    sp = round_choice(round, angl)
    ! sp = round
    ! if (sp.eq.0.0_KTGT) sp = pi_(angl) * 2.0_KTGT
    s = sign(ONE, angl)
    d = s * (modulo(abs(angl), sp) / sp) * w
  end function ang2deg_d
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  ELEMENTAL &
  function ang2deg_q(angl, round) result(d)
    use TOUZA_Std,only: KTGT=>KQPL
    implicit none
    real(kind=KTGT) :: d
    real(kind=KTGT),intent(in) :: angl
    real(kind=KTGT),intent(in) :: round
    real(kind=KTGT),parameter :: w = real(round_degree, kind=KTGT)
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT) :: s, sp
    if (round.eq.w) then
       d = angl
       return
    endif
    sp = round_choice(round, angl)
    ! sp = round
    ! if (sp.eq.0.0_KTGT) sp = pi_(angl) * 2.0_KTGT
    s = sign(ONE, angl)
    d = s * (modulo(abs(angl), sp) / sp) * w
  end function ang2deg_q
#endif

!!!_   . deg2ang
  ELEMENTAL &
  function deg2ang_d(deg, round) result(a)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: a
    real(kind=KTGT),intent(in) :: deg
    real(kind=KTGT),intent(in) :: round
    real(kind=KTGT),parameter :: w = real(round_degree, kind=KTGT)
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT) :: s, sp
    if (round.eq.w) then
       a = deg
       return
    endif
    sp = round_choice(round, deg)
    ! sp = round
    ! if (sp.eq.0.0_KTGT) sp = pi_(deg) * 2.0_KTGT
    s = sign(ONE, deg)
    a = s * (modulo(abs(deg), w) / w) * sp
  end function deg2ang_d
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  ELEMENTAL &
  function deg2ang_q(deg, round) result(a)
    use TOUZA_Std,only: KTGT=>KQPL
    implicit none
    real(kind=KTGT) :: a
    real(kind=KTGT),intent(in) :: deg
    real(kind=KTGT),intent(in) :: round
    real(kind=KTGT),parameter :: w = real(round_degree, kind=KTGT)
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT) :: s, sp
    if (round.eq.w) then
       a = deg
       return
    endif
    sp = round_choice(round, deg)
    ! sp = round
    ! if (sp.eq.0.0_KTGT) sp = pi_(deg) * 2.0_KTGT
    s = sign(ONE, deg)
    a = s * (modulo(abs(deg), w) / w) * sp
  end function deg2ang_q
#endif

!!!_   . deg2rad
  ELEMENTAL &
  function deg2rad_d(deg, nml) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    use TOUZA_Std,only: choice
    implicit none
    real(kind=KTGT) :: r
    real(kind=KTGT),intent(in) :: deg
    logical,optional,intent(in) :: nml
    real(kind=KTGT),parameter :: w = real(round_degree, kind=KTGT)
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT) :: s
    s = sign(ONE, deg)
    r = abs(deg)
    if (choice(.TRUE., nml)) r = modulo(r, w)
    r = s * (r * (pi_(deg) / (w / 2.0_KTGT)))
    ! r = s * (modulo(abs(deg), w) / (w / 2.0_KTGT)) * pi_(deg)
  end function deg2rad_d
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  ELEMENTAL &
  function deg2rad_q(deg, nml) result(r)
    use TOUZA_Std,only: KTGT=>KQPL
    use TOUZA_Std,only: choice
    implicit none
    real(kind=KTGT) :: r
    real(kind=KTGT),intent(in) :: deg
    logical,optional,intent(in) :: nml
    real(kind=KTGT),parameter :: w = real(round_degree, kind=KTGT)
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT) :: s
    s = sign(ONE, deg)
    r = abs(deg)
    if (choice(.TRUE., nml)) r = modulo(r, w)
    r = s * (r * (pi_(deg) / (w / 2.0_KTGT)))
    ! r = s * (modulo(abs(deg), w) * (pi_(deg) / (w / 2.0_KTGT)))
  end function deg2rad_q
#endif
!!!_   . rad2deg
  ELEMENTAL &
  function rad2deg_d(rad, nml) result(d)
    use TOUZA_Std,only: KTGT=>KDBL
    use TOUZA_Std,only: choice
    implicit none
    real(kind=KTGT) :: d
    real(kind=KTGT),intent(in) :: rad
    logical,optional,intent(in) :: nml
    real(kind=KTGT),parameter :: w = real(round_degree, kind=KTGT)
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT) :: p2, s
    p2 = 2.0_KTGT * pi_(rad)
    s = sign(ONE, rad)
    d = abs(rad)
    if (choice(.TRUE., nml)) d = modulo(d, p2)
    d = s * (d / p2) * w
    ! d = s * ((modulo(abs(rad), p2)) / p2) * w
  end function rad2deg_d
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  ELEMENTAL &
  function rad2deg_q(rad, nml) result(d)
    use TOUZA_Std,only: KTGT=>KQPL
    use TOUZA_Std,only: choice
    implicit none
    real(kind=KTGT) :: d
    real(kind=KTGT),intent(in) :: rad
    logical,optional,intent(in) :: nml
    real(kind=KTGT),parameter :: w = real(round_degree, kind=KTGT)
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT) :: p2, s
    p2 = 2.0_KTGT * pi_(rad)
    s = sign(ONE, rad)
    d = abs(rad)
    if (choice(.TRUE., nml)) d = modulo(d, p2)
    d = s * (d / p2) * w
    s = sign(ONE, rad)
    ! d = s * ((modulo(abs(rad), p2)) / p2) * w
  end function rad2deg_q
#endif
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

!!!_ + user subroutines (longitude)
!!!_  & get_longitude
  subroutine get_longitude_d &
       & (ierr,  longi, weight, &
       &  n,     div,   round, wnml, org, acc, plain)
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
    integer,        intent(in)          :: n          ! domain size
    integer,        intent(in),optional :: div        ! logical domain width (default n)
    real(kind=KTGT),intent(in),optional :: round      ! round angle (default: 2 pi)
    real(kind=KTGT),intent(in),optional :: wnml       ! normalization factor on weights, i.e, wnml == sum(weight) (default: round)
    real(kind=KTGT),intent(in),optional :: org        ! origin (in dx unit default, direct value if acc)
    logical,        intent(in),optional :: acc        ! accumulate switch (legacy)
    logical,        intent(in),optional :: plain      ! non-cyclic (default .FALSE.)

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE  = 1.0_KTGT

    real(kind=KTGT) :: PI
    real(kind=KTGT) :: width, o, sp, wf
    logical bp
    integer j, lw

    ierr = 0

    pi = pi_(ONE)
    o = choice(ZERO, org)
    sp = span_longitude(round)
    wf = choice(ZERO, wnml)
    lw = choice(0, div)
    bp = choice(.FALSE., plain)

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
       if (MOD(sp, width).eq.ZERO) then
          do j = 0, n - 1
             longi(j) = (real(j, kind=KTGT) + o) * (sp / width)
          enddo
       else
          do j = 0, n - 1
             longi(j) = ((real(j, kind=KTGT) + o) / width) * sp
          enddo
       endif
    endif
    weight(0:n - 1) = wf / width

    if (.not.bp) then
       longi(n)  = longi(0) + sp
       weight(n) = wf / width
    endif

  end subroutine get_longitude_d
!!!_  & mid_longitude - (conditionaly) emulate MKLONM1
  subroutine mid_longitude_d &
       & (ierr,  longm,  &
       &  longi, weight, n,  wconv, plain)
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
    logical,        intent(in),optional :: plain      ! non-cyclic (default .FALSE.)

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE  = 1.0_KTGT
    real(kind=KTGT),parameter :: HALF = 0.5_KTGT

    real(kind=KTGT) :: PI
    real(kind=KTGT) :: wc
    logical bp
    integer m

    PI = pi_(ONE)

    ierr = 0
    bp = choice(.FALSE., plain)

    wc = choice(ONE, wconv)   ! default == 1
    wc = span_longitude(wc)
    wc = wc * HALF

    m = n
    if (.not.bp) m = m + 1

    longm(0:m-1) = longi(0:m-1) - weight(0:m-1) * wc
    longm(m)     = longi(m-1)   + weight(m-1)   * wc

  end subroutine mid_longitude_d
!!!_  & div_longitude
  subroutine div_longitude_d &
       & (ierr,   longi, div, boundary, longi_c, longi_b, base, &
       &  method, round, plain)
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
    real(kind=KTGT),intent(in),optional :: round         ! round-angle
    logical,        intent(in),optional :: plain         ! non-cyclic (default .FALSE.)
    real(kind=KTGT) :: sp
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    logical bp

    bp = choice(.FALSE., plain)

    sp = span_longitude(round)
    ! if (present(span)) then
    !    sp = span_longitude(span)
    ! else
    !    sp = ZERO
    ! endif
    call div_coordinate &
         & (ierr, longi, div, boundary, longi_c, longi_b, base, sp, method, bp)
  end subroutine div_longitude_d
!!!_  & check_longitude
  subroutine check_longitude_d &
       & (ierr, longi, n, div, round, tag, u, tol)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    use TOUZA_Std,only: msg_grp
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KTGT), intent(out)         :: longi(0:*)
    integer,         intent(in)          :: n          ! array size
    integer,         intent(in),optional :: div        ! logical domain width (default n)
    real(kind=KTGT), intent(in),optional :: round      ! round angle (default: 2 pi)
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

    if (present(round)) then
       sp = span_longitude(round)
       refd = sp / real(choice(n, div), kind=KTGT)
       b = is_equidistant(longi, n, tol, e, r=refd)
       write(txt, 101) trim(ti), e, refd, b
    else
       b = is_equidistant(longi, n, tol, e)
       write(txt, 102) trim(ti), e, b
    endif
    call msg_grp(txt, __GRP__, __MDL__, utmp)
  end subroutine check_longitude_d
!!!_  & check_div_longitude
  subroutine check_div_longitude_d &
       & (ierr,  longi, div, boundary, longi_c, longi_b, base, &
       &  round, tag,   u,   tol)
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
    real(kind=KTGT), intent(in),optional :: round         ! round-angle
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
       call check_longitude(ierr, longi, mem, mwid, round, ti, utmp, tol)
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
       &  n,    round, wnml,   method)
    use TOUZA_Std,only: KTGT=>KDBL
    use TOUZA_Std,only: choice
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(out)         :: lati(*)
    real(kind=KTGT),intent(out)         :: weight(*)
    integer,        intent(in)          :: n
    real(kind=KTGT),intent(in),optional :: round      ! round-angle (default 2 pi)
    real(kind=KTGT),intent(in),optional :: wnml       ! normalization factor on weights, i.e, wnml == sum(weight) (default: round/2)
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
    sp = span_latitude(round)
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
!!!_  & gauss_latitude - compute gauss latitudes (inherit gauss())
  subroutine gauss_latitude_d &
       & (ierr, CTHETA,  GW, NLATS, round, wnml, method, &
       &  prec, max_iter)
    use TOUZA_Std,only: KTGT=>KDBL
    use TOUZA_Std,only: choice
    use TOUZA_Std,only: msg_grp
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(out)         :: CTHETA(*)
    real(kind=KTGT),intent(out)         :: GW(*)
    integer,        intent(in)          :: NLATS
    real(kind=KTGT),intent(in),optional :: round      ! round angle
    real(kind=KTGT),intent(in),optional :: wnml       ! normalization factor on weights, i.e, wnml == sum(weight) (default: round/2)
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
    sp = span_latitude(round)
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
       & (ierr, lati, div, boundary, lati_c, lati_b, base, method, round)
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
    real(kind=KTGT),intent(in),optional :: round          ! domain width

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT) :: sp

    sp = span_latitude(round)
    ! if (present(span)) then
    !    sp = span_latitude(span)
    ! else
    !    sp = ZERO
    ! endif
    call div_coordinate &
         & (ierr, lati, div, boundary, lati_c, lati_b, base, sp, method)

  end subroutine div_latitude_d
!!!_  & div_coordinate
  subroutine div_coordinate_d &
       & (ierr, co, div, boundary, cc, cb, base, span, method, plain)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(out)         :: co(0:*)
    integer,        intent(in)          :: div           ! division number
    logical,        intent(in)          :: boundary      ! boundary or center
    real(kind=KTGT),intent(in)          :: cc(0:*)       ! source coordinate (center)     o o
    real(kind=KTGT),intent(in)          :: cb(0:*)       ! source coordinate (boundary)  x x x
    integer,        intent(in)          :: base          ! source array size
    real(kind=KTGT),intent(in)          :: span          ! physical domain width
    integer,        intent(in),optional :: method
    logical,        intent(in),optional :: plain         ! non-cyclic ([CAUTION] default .TRUE.)

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
    logical bp

    ierr = 0
    beq = .FALSE.

    if (ierr.eq.0) allocate(dxd(0:base-1), STAT=ierr)
    if (ierr.ne.0) ierr = ERR_ALLOCATION

    if (ierr.eq.0) then
       bp = choice(.TRUE., plain)
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
          if (.not.bp) co(base) = co(0) + span
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
          if (.not.bp) co(base) = co(0) + span
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
             if (.not.bp) co(base) = co(0) + span
          else
             ierr = ERR_INVALID_PARAMETER
          endif
       case default
          ierr = ERR_INVALID_SWITCH
       end select
    endif

    if (ierr.eq.0) deallocate(dxd, STAT=ierr)
  end subroutine div_coordinate_d

!!!_ + polar stereographic projection
!!!_  - note
  ! Conformal latitude computation using Gudermannian function.
  ! See Karney(2011).
!!!_  & psgp_set_byf - psgp_set wrapper (using flattening and projection center)
  subroutine psgp_set_byf_d &
       & (ierr, cco, f, a, latts, lonorg, pole, xs, ys, tol, lodeg)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(out)         :: cco(*)
    real(kind=KTGT),intent(in)          :: f, a
    real(kind=KTGT),intent(in)          :: latts   ! radian
    real(kind=KTGT),intent(in)          :: lonorg  ! unit is controlled by LODEG
    integer,        intent(in)          :: pole    ! north pole == +1, south pole == -1
    real(kind=KTGT),intent(in),optional :: xs, ys  ! scale for x,y (default == 1)
    real(kind=KTGT),intent(in),optional :: tol
    logical,        intent(in),optional :: lodeg   ! true if unit degree in longitude (default: FALSE)

    real(kind=KTGT) :: e
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    integer p

    ierr = 0
    p = pole

    if (p.gt.0) then
       p = +1
    else if (p.lt.0) then
       p = -1
    else
       ierr = _ERROR(ERR_INVALID_PARAMETER)
    endif

    if (ierr.eq.0) then
       e = flatten_to_ecc(f)
       call psgp_set &
            & (cco, e, a, latts, lonorg, p, xs, ys, tol, f, lodeg)
    endif

  end subroutine psgp_set_byf_d

!!!_  & psgp_set
  subroutine psgp_set_d &
       & (cco, ecc, a, latts, lonorg, pole, xs, ys, tol, f, lodeg)
    ! [caution]
    ! xs ys != 1 are special configuration for amida,
    ! which are not fully tested on consistency.
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    real(kind=KTGT),intent(out)         :: cco(*)
    real(kind=KTGT),intent(in)          :: ecc, a
    real(kind=KTGT),intent(in)          :: latts
    real(kind=KTGT),intent(in)          :: lonorg
    integer,        intent(in)          :: pole    ! north pole == +1, south pole == -1
    real(kind=KTGT),intent(in),optional :: xs, ys  ! scale for x,y (default == 1)
    real(kind=KTGT),intent(in),optional :: tol
    real(kind=KTGT),intent(in),optional :: f
    logical,        intent(in),optional :: lodeg   ! true if unit degree in longitude (default: FALSE)

    real(kind=KTGT),parameter :: ONE=1.0_KTGT, ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: TWO=2.0_KTGT
    real(kind=KTGT) :: d
    real(kind=KTGT) :: tcf, lt
    real(kind=KTGT) :: tglat, tclat, cc, e2c
    real(kind=KTGT) :: fc, c
    real(kind=KTGT) :: loro

    if (pole.lt.0) then
       cco(icache_psgp_sign) = - ONE
    else
       cco(icache_psgp_sign) = + ONE
    endif
    lt = latts * cco(icache_psgp_sign)

    ! 1 - f = sqrt(1-e^2)
    ! c = (1-f) * exp[e atanh (e)]
    e2c = (ONE - ecc) * (ONE + ecc)
    fc = SQRT(e2c)
    c = fc * EXP(ecc * ATANH (ecc))

    tglat = TAN(lt)
    d = SQRT(ONE + e2c * (tglat * tglat))
    tcf = ONE / d

    tclat = conformal_latitude(tglat, ecc)
    cc = _hypot(ONE, tclat) + ABS(tclat)
    if (tclat.lt.ZERO) then
       tcf = tcf / cc
    else
       tcf = tcf * cc
    endif

    cco(icache_psgp_tcf)  = tcf
    cco(icache_psgp_atcf) = tcf * a
    cco(icache_psgp_psf)  = tcf * (c / TWO)
    cco(icache_psgp_maj)  = a
    cco(icache_psgp_ecc)  = ecc
    cco(icache_psgp_e2c)  = e2c
    cco(icache_psgp_tslat) = latts
    if (ecc.eq.ZERO) then
       cco(icache_psgp_aco) = ONE + e2c
    else
       cco(icache_psgp_aco) = ONE + e2c * (ATANH(ecc) / ecc)
    endif

    if (present(f)) then
       cco(icache_psgp_flat) = f
    else
       cco(icache_psgp_flat) = ONE - fc
    endif

    cco(icache_psgp_tol) = choice(ONE, tol)

    loro = round_choice(round_2pi, loro)
    if (choice(.FALSE., lodeg)) loro = real(round_degree, KIND=KTGT)

    call psgp_set_step(cco, xs, ys, lonorg, loround=loro)
    ! s = choice(ONE, xs)
    ! if (s.eq.ZERO) s = ONE
    ! cco(icache_psgp_xco)  = a / s

    ! s = choice(ONE, ys)
    ! if (s.eq.ZERO) s = ONE
    ! cco(icache_psgp_yco)  = a / s
  end subroutine psgp_set_d
!!!_  & psgp_set_step
  subroutine psgp_set_step_d &
       & (cco, xs, ys, lonorg, loround)
    ! [caution]
    ! xs ys != 1 are special configuration for amida,
    ! which are not fully tested on consistency.
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    real(kind=KTGT),intent(inout)       :: cco(*)
    real(kind=KTGT),intent(in),optional :: xs, ys  ! scale for x,y (default == 1)
    real(kind=KTGT),intent(in),optional :: lonorg
    real(kind=KTGT),intent(in),optional :: loround

    real(kind=KTGT),parameter :: ONE=1.0_KTGT, ZERO=0.0_KTGT
    real(kind=KTGT) :: s, a

    a = cco(icache_psgp_maj)

    s = choice(ONE, xs)
    if (s.eq.ZERO) s = ONE
    cco(icache_psgp_xco)  = a / s

    s = choice(ONE, ys)
    if (s.eq.ZERO) s = ONE
    cco(icache_psgp_yco)  = a / s

    if (present(lonorg)) then
       cco(icache_psgp_olon) = lonorg
    endif
    if (present(loround)) then
       cco(icache_psgp_loround) = round_choice(loround, ZERO)
    endif
  end subroutine psgp_set_step_d

!!!_  & psgp_inquire
  subroutine psgp_inquire_d &
       & (ierr, cco, tslat, olat, olon, pxlon, &
       &  loround, psign, e, f)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    integer,        intent(out)          :: ierr
    real(kind=KTGT),intent(in)           :: cco(*)
    real(kind=KTGT),intent(out),optional :: tslat
    real(kind=KTGT),intent(out),optional :: olat
    real(kind=KTGT),intent(out),optional :: olon
    real(kind=KTGT),intent(out),optional :: pxlon       ! +x longitude
    real(kind=KTGT),intent(out),optional :: loround
    real(kind=KTGT),intent(out),optional :: psign
    real(kind=KTGT),intent(out),optional :: e, f

    real(kind=KTGT) :: quad

    ierr = 0
    if (present(tslat)) then
       tslat = cco(icache_psgp_tslat)
    endif
    if (present(olon)) then
       olon = cco(icache_psgp_olon)
    endif
    if (present(pxlon)) then
       quad = round_choice(cco(icache_psgp_loround), mold=quad) / 4.0_KTGT
       ! if (quad.eq.0.0_KTGT) then
       !    quad = pi_(0.0_KTGT) / 2.0_KTGT
       ! else
       !    quad = quad / 4.0_KTGT
       ! endif
       ! pxlon = cco(icache_psgp_olon) + quad * cco(icache_psgp_sign)
       pxlon = cco(icache_psgp_olon) + quad
    endif
    if (present(loround)) then
       loround = round_choice(cco(icache_psgp_loround), mold=loround)
       ! lospan = cco(icache_psgp_lospan)
       ! if (lospan.eq.0.0_KTGT) lospan = pi_(0.0_KTGT) * 2.0_KTGT
    endif
    ! if (present(laround)) then
    !    laround = round_choice(cco(icache_psgp_laround))
    !    ! lospan = cco(icache_psgp_lospan)
    !    ! if (lospan.eq.0.0_KTGT) lospan = pi_(0.0_KTGT) * 2.0_KTGT
    ! endif
    if (present(olat)) then
       olat = cco(icache_psgp_sign)
       olat = olat * (pi_(0.0_KTGT)) / 2.0_KTGT
    endif
    if (present(psign)) then
       psign = cco(icache_psgp_sign)
    endif
    if (present(e)) then
       e = cco(icache_psgp_ecc)
    endif
    if (present(f)) then
       f = cco(icache_psgp_flat)
    endif
  end subroutine psgp_inquire_d

!!!_  & psgp_cachela - lat cache for stereographic projection (pole)
  subroutine psgp_cachela_d &
       & (cla, lat, cco)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out) :: cla(*)
    real(kind=KTGT),intent(in)  :: lat
    real(kind=KTGT),intent(in)  :: cco(*)

    real(kind=KTGT),parameter :: ONE=1.0_KTGT
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: OPPO=HUGE(ZERO)
    real(kind=KTGT) :: ecc, e2c
    real(kind=KTGT) :: tglat
    real(kind=KTGT) :: tclat, cc, tcf
    real(kind=KTGT) :: scl(NTRIG)
    real(kind=KTGT) :: hpi, s

    hpi = pi_(ZERO)/2.0_KTGT
    if (lat.eq.hpi * cco(icache_psgp_sign)) then
       s = cco(icache_psgp_xco) * cco(icache_psgp_sign)
       cla(icache_psgp_xrho) = SIGN(ZERO, s)
       s = cco(icache_psgp_yco) * cco(icache_psgp_sign)
       cla(icache_psgp_yrho) = SIGN(ZERO, s)
       cla(icache_psgp_sf)   = cco(icache_psgp_psf)
    else if (abs(lat).eq.hpi) then
       cla(icache_psgp_xrho) = OPPO * cco(icache_psgp_sign)
       cla(icache_psgp_yrho) = OPPO * cco(icache_psgp_sign)
       cla(icache_psgp_sf)   = OPPO
    else
       ecc = cco(icache_psgp_ecc)
       e2c = cco(icache_psgp_e2c)
       tcf = cco(icache_psgp_tcf)

       tglat = TAN(lat) * cco(icache_psgp_sign)
       tclat = conformal_latitude(tglat, ecc)

       cc = _hypot(ONE, tclat) + ABS(tclat)
       if (tclat.lt.ZERO) then
          tcf = tcf * cc
       else
          tcf = tcf / cc
       endif

       s = cco(icache_psgp_xco) * cco(icache_psgp_sign)
       cla(icache_psgp_xrho) = SIGN(cco(icache_psgp_xco) * tcf, s)
       s = cco(icache_psgp_yco) * cco(icache_psgp_sign)
       cla(icache_psgp_yrho) = SIGN(cco(icache_psgp_yco) * tcf, s)

       cla(icache_psgp_sf)   = tcf * SQRT(ONE + e2c * (tglat * tglat))
    endif
    ! no sign adjustment
    _TRIG(scl) = set_sincos(lat)
    cla(icache_psgp_cosla) = _COS(scl)
    cla(icache_psgp_sinla) = _SIN(scl)
  end subroutine psgp_cachela_d
!!!_  & psgp_cachelo - lon cache for stereographic projection (pole)
  subroutine psgp_cachelo_d &
       & (clo, lon, cco)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out) :: clo(*)
    real(kind=KTGT),intent(in)  :: lon
    real(kind=KTGT),intent(in)  :: cco(*)

    real(kind=KTGT) :: lonorg, loro
    real(kind=KTGT) :: dlo
    lonorg = cco(icache_psgp_olon)
    loro = cco(icache_psgp_loround)
    dlo = (lon - lonorg) * cco(icache_psgp_sign)
    clo(icache_psgp_sindlo) = sin_canonical(dlo, loro)
    clo(icache_psgp_cosdlo) = cos_canonical(dlo, loro)
    ! write(*, *) 'cachelo:', ang2deg(dlo, lospan), sin_canonical(dlo, lospan), cos_canonical(dlo, lospan), lospan
  end subroutine psgp_cachelo_d

!!!_  & psgp_cachelo_px - lon cache for stereographic projection (longitude relative to +x lon)
  subroutine psgp_cachelo_px_d &
       & (clo, rlon, cco)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out) :: clo(*)
    real(kind=KTGT),intent(in)  :: rlon    ! relative longitude from +x longitude
    real(kind=KTGT),intent(in)  :: cco(*)

    real(kind=KTGT) :: loro, rl

    !  sin(a [+-] pi/2) = [+-] cos a
    !  cos(a [+-] pi/2) = [-+] sin a

    loro = cco(icache_psgp_loround)

    ! NP: dlo = + (rlon + pi/2) = +rlon + pi/2
    ! SP: dlo = - (pi/2 - rlon) = +rlon - pi/2
    ! rl = rlon
    ! clo(icache_psgp_sindlo) = + cos_canonical(rl, lospan) * cco(icache_psgp_sign)
    ! clo(icache_psgp_cosdlo) = - sin_canonical(rl, lospan) * cco(icache_psgp_sign)

    ! NP: dlo = + (rlon + pi/2) = + rlon + pi/2
    ! SP: dlo = - (rlon + pi/2) = - rlon - pi/2
    rl = rlon * cco(icache_psgp_sign)
    clo(icache_psgp_sindlo) = + cos_canonical(rl, loro) * cco(icache_psgp_sign)
    clo(icache_psgp_cosdlo) = - sin_canonical(rl, loro) * cco(icache_psgp_sign)

  end subroutine psgp_cachelo_px_d

!!!_  & psgp_cachelo_tr - lon cache for stereographic projection (pole)
  subroutine psgp_cachelo_tr_d &
       & (clo, slon, clon, cco)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out) :: clo(*)
    real(kind=KTGT),intent(in)  :: slon, clon
    real(kind=KTGT),intent(in)  :: cco(*)

    real(kind=KTGT) :: lonorg, loro
    real(kind=KTGT) :: lo0(NTRIG), lo1(NTRIG), dlo(NTRIG)
    lonorg = cco(icache_psgp_olon)
    loro = cco(icache_psgp_loround)
    _TRIG(lo0) = set_sincos(lonorg, loro)
    _SIN(lo1) = slon
    _COS(lo1) = clon
    _TRIG(dlo) = sub_angle(lo1, lo0)
    clo(icache_psgp_sindlo) = _SIN(dlo) * cco(icache_psgp_sign)
    clo(icache_psgp_cosdlo) = _COS(dlo)
  end subroutine psgp_cachelo_tr_d

!!!_  & psgp_comp_dlon - compute longitude properties relative to projection center
  subroutine psgp_comp_dlon_d &
       & (dlon, lon, cco)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out) :: dlon
    real(kind=KTGT),intent(in)  :: lon
    real(kind=KTGT),intent(in)  :: cco(*)
    dlon = lon - cco(icache_psgp_olon)
  end subroutine psgp_comp_dlon_d

!!!_  & psgp_dlo_tr - extract dlon properties from cache
  subroutine psgp_dlo_tr_d &
       & (dlon, clo, cco)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out) :: dlon(*)
    real(kind=KTGT),intent(in)  :: clo(*)
    real(kind=KTGT),intent(in)  :: cco(*)

    _SIN(dlon) = clo(icache_psgp_sindlo) * cco(icache_psgp_sign)
    _COS(dlon) = clo(icache_psgp_cosdlo)
  end subroutine psgp_dlo_tr_d

!!!_  & psgp_gla_tr - extract lat properties from cache
  subroutine psgp_gla_tr_d &
       & (glat, cla, cco)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out) :: glat(*)
    real(kind=KTGT),intent(in)  :: cla(*)
    real(kind=KTGT),intent(in)  :: cco(*)

    _SIN(glat) = cla(icache_psgp_sinla)
    _COS(glat) = cla(icache_psgp_cosla)
  end subroutine psgp_gla_tr_d

!!!_  & psgp_sinlat - extract sine lat properties from cache
  PURE &
  function psgp_sinlat_d (cla, cco) result(v)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in)  :: cla(*)
    real(kind=KTGT),intent(in)  :: cco(*)

    v = cla(icache_psgp_sinla)
  end function psgp_sinlat_d

!!!_  & psgp_fwd_once()
  function psgp_fwd_once_d &
       & (lon, lat, cco) result (xy)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: xy(2)
    real(kind=KTGT),intent(in) :: lon, lat
    real(kind=KTGT),intent(in) :: cco(*)
    real(kind=KTGT) :: tlo(ncache_psgp_lo)
    real(kind=KTGT) :: tla(ncache_psgp_la)

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

    xy(1) = + cla(icache_psgp_xrho) * clo(icache_psgp_sindlo)
    xy(2) = - cla(icache_psgp_yrho) * clo(icache_psgp_cosdlo)
  end function psgp_fwd_core_d

!!!_  & psgp_fwd_cis_once()
  function psgp_fwd_cis_once_d &
       & (lon, lat, cco) result (xyf)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: xyf(3)
    real(kind=KTGT),intent(in) :: lon, lat
    real(kind=KTGT),intent(in) :: cco(*)
    real(kind=KTGT) :: tlo(ncache_psgp_lo)
    real(kind=KTGT) :: tla(ncache_psgp_la)

    call psgp_cachela(tla, lat, cco)
    call psgp_cachelo(tlo, lon, cco)

    xyf(:) = psgp_fwd_cis_core(tlo, tla)
  end function psgp_fwd_cis_once_d
!!!_  & psgp_fwd_cis_core()
  function psgp_fwd_cis_core_d &
       & (clo, cla) result (xyf)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: xyf(3)
    real(kind=KTGT),intent(in) :: clo(*)
    real(kind=KTGT),intent(in) :: cla(*)

    ! write(*, *) 'psgp:x', cla(icache_psgp_xrho), clo(icache_psgp_sindlo)
    ! write(*, *) 'psgp:y', cla(icache_psgp_yrho), clo(icache_psgp_cosdlo)
    xyf(1) = + cla(icache_psgp_xrho) * clo(icache_psgp_sindlo)
    xyf(2) = - cla(icache_psgp_yrho) * clo(icache_psgp_cosdlo)
    xyf(3) = psgp_fwd_isf(cla)
  end function psgp_fwd_cis_core_d

!!!_  & psgp_fwd_isf()
  function psgp_fwd_isf_d &
       & (cla) result (isf)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: isf
    real(kind=KTGT),intent(in) :: cla(*)
    real(kind=KTGT),parameter :: ONE=1.0_KTGT
    isf = ONE / cla(icache_psgp_sf)
  end function psgp_fwd_isf_d
!!!_  & psgp_bwd_tr()
  subroutine psgp_bwd_tr_d &
       & (gla, dlo, x, y, cco)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out):: gla(*)  ! geographic latitude
    real(kind=KTGT),intent(out):: dlo(*)  ! difference in longitude
    real(kind=KTGT),intent(in) :: x, y
    real(kind=KTGT),intent(in) :: cco(*)

    real(kind=KTGT) :: s, d
    real(kind=KTGT) :: rhonml
    real(kind=KTGT) :: tglat
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: ONE=1.0_KTGT

    real(kind=KTGT) :: xx, yy

    s   = cco(icache_psgp_sign)
    xx = x / cco(icache_psgp_xco)
    yy = y / cco(icache_psgp_yco)
    rhonml = _hypot(xx, yy)
    if (rhonml.eq.ZERO) then
       _SIN(gla) = s * ONE
       _COS(gla) = ZERO
       ! undetermined longitude
       _SIN(dlo) = ZERO
       _COS(dlo) = ONE
    else
       tglat = psgp_bwd_core(rhonml, cco)
       d = _hypot(ONE, tglat)
       _SIN(gla) = s * tglat / d
       _COS(gla) = ONE / d

       _SIN(dlo) =   xx / rhonml
       _COS(dlo) = - yy / rhonml * s

       gla(1:NTRIG) = nml_sincos(_SIN(gla), _COS(gla))
       dlo(1:NTRIG) = nml_sincos(_SIN(dlo), _COS(dlo))
    endif
  end subroutine psgp_bwd_tr_d
!!!_  & psgp_bwd_ll()
  function psgp_bwd_ll_d &
       & (x, y, cco) result(ll)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: ll(NGEOG)
    real(kind=KTGT),intent(in) :: x, y
    real(kind=KTGT),intent(in) :: cco(*)

    real(kind=KTGT) :: s, lo, loro
    real(kind=KTGT) :: rhonml
    real(kind=KTGT) :: tglat
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT) :: xx, yy

    loro = cco(icache_psgp_loround)
    s   = cco(icache_psgp_sign)
    xx = x / cco(icache_psgp_xco)
    yy = y / cco(icache_psgp_yco)
    rhonml = _hypot(xx, yy)
    ! rho = psgp_bwd_rho(x, y, cco)
    if (rhonml.eq.ZERO) then
       _LATI(ll)  = s * (pi_(ZERO) / 2.0_KTGT)
       ! undetermined longitude
       _LONGI(ll) = cco(icache_psgp_olon)
    else
       tglat = psgp_bwd_core(rhonml, cco)

       _LATI(ll) = s * ATAN(tglat)

       ! lo = s * cco(icache_psgp_olon) + ATAN2(xx * s, - yy * s)
       ! _LONGI(ll) = lo * s
       lo = rad2ang(s * ATAN2(xx * s, - yy * s), loro)
       lo = cco(icache_psgp_olon) + lo
       _LONGI(ll) = lo
    endif
  end function psgp_bwd_ll_d
!!!_  & psgp_bwd_sf() - scale factor
  function psgp_bwd_sf_d &
       & (x, y, cco) result(sf)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: sf
    real(kind=KTGT),intent(in) :: x, y
    real(kind=KTGT),intent(in) :: cco(*)

    real(kind=KTGT) :: e2c
    real(kind=KTGT) :: rhonml
    real(kind=KTGT) :: tglat
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: ONE=1.0_KTGT

    rhonml = psgp_bwd_rhonml(x, y, cco)
    if (rhonml.eq.ZERO) then
       sf = cco(icache_psgp_psf)
    else
       e2c = cco(icache_psgp_e2c)
       tglat = psgp_bwd_core(rhonml, cco)
       sf = rhonml * SQRT(ONE + e2c * (tglat * tglat))
    endif
  end function psgp_bwd_sf_d
!!!_  & psgp_bwd_isf() - inverse scale factor
  function psgp_bwd_isf_d &
       & (x, y, cco) result(sf)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: sf
    real(kind=KTGT),intent(in) :: x, y
    real(kind=KTGT),intent(in) :: cco(*)

    real(kind=KTGT) :: e2c
    real(kind=KTGT) :: rhonml
    real(kind=KTGT) :: tglat
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: ONE=1.0_KTGT

    rhonml = psgp_bwd_rhonml(x, y, cco)
    if (rhonml.eq.ZERO) then
       sf = ONE / cco(icache_psgp_psf)
    else
       e2c = cco(icache_psgp_e2c)
       tglat = psgp_bwd_core(rhonml, cco)
       sf = (ONE / rhonml) / SQRT(ONE + e2c * (tglat * tglat))
    endif
  end function psgp_bwd_isf_d
!!!_  & psgp_bwd_iaf() - inverse area factor
  function psgp_bwd_iaf_d &
       & (x, y, cco) result(sf)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: sf
    real(kind=KTGT),intent(in) :: x, y
    real(kind=KTGT),intent(in) :: cco(*)

    real(kind=KTGT) :: e2c
    real(kind=KTGT) :: rhonml
    real(kind=KTGT) :: tglat
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: ONE=1.0_KTGT

    rhonml = psgp_bwd_rhonml(x, y, cco)
    if (rhonml.eq.ZERO) then
       sf = ONE / (cco(icache_psgp_psf) ** 2)
    else
       e2c = cco(icache_psgp_e2c)
       tglat = psgp_bwd_core(rhonml, cco)
       sf = (ONE / rhonml) ** 2 / (ONE + e2c * (tglat * tglat))
    endif
  end function psgp_bwd_iaf_d
!!!_  & psgp_bwd_rhonml
  function psgp_bwd_rhonml_d &
       & (x, y, cco) result(rhonml)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: rhonml
    real(kind=KTGT),intent(in) :: x, y
    real(kind=KTGT),intent(in) :: cco(*)
    rhonml = _hypot(x / cco(icache_psgp_xco), y / cco(icache_psgp_yco))
  end function psgp_bwd_rhonml_d
!!!_  & psgp_bwd_rho
  function psgp_bwd_rho_d &
       & (x, y, cco) result(rho)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: rho
    real(kind=KTGT),intent(in) :: x, y
    real(kind=KTGT),intent(in) :: cco(*)
    ! rho = _hypot(x * cco(icache_psgp_xco), y * cco(icache_psgp_yco))
    rho = psgp_bwd_rhonml(x, y, cco) * cco(icache_psgp_maj)
  end function psgp_bwd_rho_d
!!!_  & psgp_bwd_core
  function psgp_bwd_core_d &
       & (rhonml, cco) result(tlat)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: tlat
    real(kind=KTGT),intent(in) :: rhonml   ! normalized rho
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

    if (rhonml.eq.ZERO) then
       tlat = cco(icache_psgp_sign) * HUGE(ZERO)
    else
       tol = cco(icache_psgp_tol)
       ecc = cco(icache_psgp_ecc)
       ! e2c = (ONE - ecc) * (ONE + ecc)
       e2c = cco(icache_psgp_e2c)

       t = rhonml / cco(icache_psgp_tcf)
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
    endif
  end function psgp_bwd_core_d
!!!_  & psgp_bwd_length
  subroutine psgp_bwd_length_d &
       & (dis, x0, y0, x1, y1, cco, levbgn, levend, tol, res)
    ! (caution) return non-adjusted length
    !           input coordinate should be scaled with xco yco parameters,
    !           but return distance is actual value
    use TOUZA_Std,only: KTGT=>KDBL
    use TOUZA_Std,only: choice
    implicit none
    real(kind=KTGT),intent(out)          :: dis
    real(kind=KTGT),intent(in)           :: x0, y0, x1, y1
    real(kind=KTGT),intent(in)           :: cco(*)
    integer,        intent(in)           :: levbgn, levend
    real(kind=KTGT),intent(in)           :: tol
    real(kind=KTGT),intent(out),optional :: res
    real(kind=KTGT),parameter :: TWO=2.0_KTGT
    real(kind=KTGT),parameter :: HALF=0.5_KTGT
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT

    real(kind=KTGT) :: xs, ys, tx, ty
    real(kind=KTGT) :: xp, yp
    real(kind=KTGT) :: k, kprev
    real(kind=KTGT) :: etol
    real(kind=KTGT) :: dref, rsol

    integer,parameter :: llev = BIT_SIZE(0) - 1
    real(kind=KTGT) :: buf(0:llev)
    integer jlev
    integer bpos, bj
    integer jdiv, ndiv

    xs = x1 - x0
    ys = y1 - y0
    xp = (x1 + x0) / TWO
    yp = (y1 + y0) / TWO

    ! dref = _hypot(xs, ys)
    dref = psgp_bwd_rho(xs, ys, cco)
    kprev = psgp_bwd_isf(xp, yp, cco)

    if (tol.lt.ZERO) then
       etol = abs(tol) / dref
    else
       etol = spacing(kprev) * tol
    endif

    rsol = ZERO
    k = ZERO
    do jlev = max(levbgn, 1), min(levend, llev) - 1
       bpos = 0
       ndiv = 2 ** jlev
       tx = xs / real(ndiv, kind=KTGT)
       ty = ys / real(ndiv, kind=KTGT)
       do jdiv = 0, ndiv - 1
          xp = x0 + tx * (real(jdiv, kind=KTGT) + HALF)
          yp = y0 + ty * (real(jdiv, kind=KTGT) + HALF)
          k = psgp_bwd_isf(xp, yp, cco)
          buf(bpos) = k
          bj = jdiv
          do
             if (mod(bj, 2).eq.0) exit
             buf(bpos - 1) = buf(bpos - 1) + buf(bpos)
             bj = bj / 2
             bpos = bpos - 1
          enddo
          bpos = bpos + 1
       enddo
       k = buf(0) / real(ndiv, kind=KTGT)
       rsol = k - kprev
       if (abs(rsol).le.etol) exit
       kprev = k
    enddo
    dis = k * dref

    if (present(res)) then
       res = rsol * dref
    endif
  end subroutine psgp_bwd_length_d
!!!_  & psgp_bwd_area
  subroutine psgp_bwd_area_d &
       & (area, x0, y0, x1, y1, cco, levbgn, levend, tol, res)
    use TOUZA_Std,only: KTGT=>KDBL
    use TOUZA_Std,only: choice
    implicit none
    real(kind=KTGT),intent(out)          :: area
    real(kind=KTGT),intent(in)           :: x0, y0, x1, y1
    real(kind=KTGT),intent(in)           :: cco(*)
    integer,        intent(in)           :: levbgn, levend
    real(kind=KTGT),intent(in)           :: tol
    real(kind=KTGT),intent(out),optional :: res
    real(kind=KTGT),parameter :: TWO=2.0_KTGT
    real(kind=KTGT),parameter :: HALF=0.5_KTGT
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT

    real(kind=KTGT) :: xs, ys, tx, ty
    real(kind=KTGT) :: xp, yp
    real(kind=KTGT) :: k, kprev
    real(kind=KTGT) :: etol
    real(kind=KTGT) :: dref, rsol

    integer,parameter :: llev = BIT_SIZE(0) - 1
    real(kind=KTGT) :: buf(0:llev * 2)
    integer jlev
    integer jdx, jdy
    integer bpos, bj
    integer ndiv

    xs = x1 - x0
    ys = y1 - y0
    xp = (x1 + x0) / TWO
    yp = (y1 + y0) / TWO

    dref = xs * ys
    kprev = psgp_bwd_iaf(xp, yp, cco)

    if (tol.lt.ZERO) then
       etol = abs(tol) / dref
    else
       etol = spacing(kprev) * tol
    endif

    rsol = ZERO
    k = ZERO
    do jlev = max(levbgn, 1), min(levend, llev) - 1
       bpos = 0
       ndiv = 2 ** jlev
       tx = xs / real(ndiv, kind=KTGT)
       ty = ys / real(ndiv, kind=KTGT)
       do jdy = 0, ndiv - 1
          yp = y0 + ty * (real(jdy, kind=KTGT) + HALF)
          do jdx = 0, ndiv - 1
             xp = x0 + tx * (real(jdx, kind=KTGT) + HALF)

             k = psgp_bwd_iaf(xp, yp, cco)
             buf(bpos) = k
             bj = jdy * ndiv + jdx
             do
                if (mod(bj, 2).eq.0) exit
                buf(bpos - 1) = buf(bpos - 1) + buf(bpos)
                bj = bj / 2
                bpos = bpos - 1
             enddo
             bpos = bpos + 1
          enddo
       enddo
       k = buf(0) / real(ndiv * ndiv, kind=KTGT)
       rsol = k - kprev
       if (abs(rsol).le.etol) exit
       kprev = k
    enddo
    area = k * dref

    if (present(res)) then
       res = rsol * dref
    endif
  end subroutine psgp_bwd_area_d

!!!_  & psgp_bwd_area2 - sum using difference
  subroutine psgp_bwd_area2_d &
       & (area, x0, y0, x1, y1, cco, levbgn, levend, tol, res, kref)
    use TOUZA_Std,only: KTGT=>KDBL
    use TOUZA_Std,only: choice
    implicit none
    real(kind=KTGT),intent(out)          :: area
    real(kind=KTGT),intent(in)           :: x0, y0, x1, y1
    real(kind=KTGT),intent(in)           :: cco(*)
    integer,        intent(in)           :: levbgn, levend
    real(kind=KTGT),intent(in)           :: tol
    real(kind=KTGT),intent(out),optional :: res
    real(kind=KTGT),intent(in), optional :: kref
    real(kind=KTGT),parameter :: TWO=2.0_KTGT
    real(kind=KTGT),parameter :: ONE=2.0_KTGT
    real(kind=KTGT),parameter :: HALF=0.5_KTGT
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT

    real(kind=KTGT) :: xs, ys, tx, ty
    real(kind=KTGT) :: xp, yp
    real(kind=KTGT) :: k0, k2prev, dk, k1, k2
    real(kind=KTGT) :: etol
    real(kind=KTGT) :: dref, rsol

    integer,parameter :: llev = BIT_SIZE(0) - 1
    real(kind=KTGT) :: buf1(0:llev * 2)    ! sum (dk)
    real(kind=KTGT) :: buf2(0:llev * 2)    ! sum (dk**2)
    integer jlev
    integer jdx, jdy
    integer bpos, bj
    integer ndiv

    xs = x1 - x0
    ys = y1 - y0
    xp = (x1 + x0) / TWO
    yp = (y1 + y0) / TWO

    dref = xs * ys
    k0 = choice(-ONE, kref)
    if (k0.lt.ZERO) k0 = psgp_bwd_isf(xp, yp, cco)

    if (tol.lt.ZERO) then
       etol = abs(tol) / dref
    else
       k1 = psgp_bwd_iaf(xp, yp, cco)
       etol = spacing(k1) * tol
    endif

    k2prev = ZERO
    rsol = ZERO
    ndiv = 1
    buf2(0) = ZERO
    do jlev = max(levbgn, 1), min(levend, llev) - 1
       bpos = 0
       ndiv = 2 ** jlev
       tx = xs / real(ndiv, kind=KTGT)
       ty = ys / real(ndiv, kind=KTGT)
       do jdy = 0, ndiv - 1
          yp = y0 + ty * (real(jdy, kind=KTGT) + HALF)
          do jdx = 0, ndiv - 1
             xp = x0 + tx * (real(jdx, kind=KTGT) + HALF)

             dk = psgp_bwd_isf(xp, yp, cco) - k0
             buf1(bpos) = dk
             buf2(bpos) = dk ** 2

             bj = jdy * ndiv + jdx
             do
                if (mod(bj, 2).eq.0) exit
                buf1(bpos - 1) = buf1(bpos - 1) + buf1(bpos)
                buf2(bpos - 1) = buf2(bpos - 1) + buf2(bpos)
                bj = bj / 2
                bpos = bpos - 1
             enddo
             bpos = bpos + 1
          enddo
       enddo
       k2 = buf2(0) / real(ndiv * ndiv, kind=KTGT)

       rsol = k2 - k2prev
       ! k1 = buf1(0) / real(ndiv * ndiv, kind=KTGT)
       ! write(*, *) 'area2:', jlev, k0 ** 2, k0 * k1 * TWO, k2, rsol
       if (abs(rsol).le.etol) exit
       k2prev = k2
    enddo

    buf1(0) = buf1(0) / real(ndiv * ndiv, kind=KTGT)
    buf2(0) = buf2(0) / real(ndiv * ndiv, kind=KTGT)
    area = k0 * (k0 + TWO * buf1(0)) + buf2(0)
    ! area = k0 ** 2 + (((TWO * k0 * buf1(0)) + buf2(0)) / real(ndiv * ndiv, kind=KTGT))
    area = area * dref

    if (present(res)) then
       res = rsol * dref
    endif
  end subroutine psgp_bwd_area2_d

!!!_  & psgp_bwd_geod
  subroutine psgp_bwd_geod_d &
       & (garea, gdis, x0, y0, x1, y1, cco, levbgn, levend, tol, ares, dres)
    use TOUZA_Std,only: KTGT=>KDBL
    use TOUZA_Std,only: choice
    implicit none
    real(kind=KTGT),intent(out)          :: garea, gdis
    real(kind=KTGT),intent(in)           :: x0, y0, x1, y1
    real(kind=KTGT),intent(in)           :: cco(*)
    integer,        intent(in)           :: levbgn, levend
    real(kind=KTGT),intent(in)           :: tol
    real(kind=KTGT),intent(out),optional :: ares,   dres
    real(kind=KTGT),parameter :: TWO=2.0_KTGT
    real(kind=KTGT),parameter :: HALF=0.5_KTGT
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT

    real(kind=KTGT) :: xs, ys, tx, ty
    real(kind=KTGT) :: xp, yp, xo, yo

    real(kind=KTGT) :: gla1(NTRIG), glo1(NTRIG+1)
    real(kind=KTGT) :: gla2(NTRIG), glo2(NTRIG+1)
    real(kind=KTGT) :: dglon(NTRIG+1)

    real(kind=KTGT) :: a, f

    real(kind=KTGT) :: aprev, dprev, asol, dsol
    real(kind=KTGT) :: etol
    real(kind=KTGT) :: aerr, derr

    integer,parameter :: llev = BIT_SIZE(0) - 1
    real(kind=KTGT) :: abuf(0:llev), dbuf(0:llev)
    real(kind=KTGT) :: gd, ga
    integer jlev
    integer bpos, bj
    integer jdiv, ndiv
    integer jerr

    xs = x1 - x0
    ys = y1 - y0

    xo = x0
    yo = y0
    xp = x1
    yp = y1

    a = cco(icache_psgp_maj)
    f = cco(icache_psgp_flat)

    call psgp_bwd_tr(gla1, glo1, xo, yo, cco)
    call psgp_bwd_tr(gla2, glo2, xp, yp, cco)
    dglon(:) = set_dlongi(glo1, glo2)
    call geodesic_inverse &
         (jerr, gd, gla1, gla2, dglon, f, a, garea=ga)

    if (tol.lt.ZERO) then
       etol = abs(tol)
    else
       etol = spacing(ga) * tol
    endif

    aprev = ga
    dprev = gd
    asol = ZERO
    dsol = ZERO
    aerr = ZERO
    derr = ZERO
    do jlev = max(levbgn, 1), min(levend, llev) - 1
       bpos = 0
       ndiv = 2 ** jlev
       tx = xs / real(ndiv, kind=KTGT)
       ty = ys / real(ndiv, kind=KTGT)
       xo = x0
       yo = y0
       call psgp_bwd_tr(gla1, glo1, xo, yo, cco)
       do jdiv = 0, ndiv - 1
          xp = x0 + tx * (real(jdiv+1, kind=KTGT))
          yp = y0 + ty * (real(jdiv+1, kind=KTGT))
          call psgp_bwd_tr(gla2, glo2, xp, yp, cco)
          dglon(:) = set_dlongi(glo1, glo2)
          call geodesic_inverse &
               (jerr, gd, gla1, gla2, dglon, f, a, garea=ga)
          abuf(bpos) = ga
          dbuf(bpos) = gd
          bj = jdiv
          do
             if (mod(bj, 2).eq.0) exit
             abuf(bpos - 1) = abuf(bpos - 1) + abuf(bpos)
             dbuf(bpos - 1) = dbuf(bpos - 1) + dbuf(bpos)
             bj = bj / 2
             bpos = bpos - 1
          enddo
          bpos = bpos + 1
          gla1(:) = gla2(:)
          glo1(:) = glo2(:)
       enddo
       dsol = dbuf(0)
       asol = abuf(0)
       aerr = asol - aprev
       derr = dsol - dprev
       ! write(*, *) 'pgeod:', jlev, dsol, derr, asol, aerr
       if (abs(asol).le.etol) exit
       aprev = asol
       dprev = dsol
    enddo
    gdis  = dsol
    garea = asol

    if (present(ares)) then
       ares = aerr
    endif
    if (present(dres)) then
       dres = derr
    endif
  end subroutine psgp_bwd_geod_d

!!!_  & psgp_surf_area ()
  function psgp_surf_area_d(cco) result (v)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in) :: cco(*)
    real(kind=KTGT) :: a, aco
    real(kind=KTGT),parameter :: ONE=1.0_KTGT
    real(kind=KTGT),parameter :: TWO=2.0_KTGT
    a = cco(icache_psgp_maj)
    aco = cco(icache_psgp_aco)

    ! v = (((a * a) * (pi_(ONE))) * (ONE + ((ONE - e) * (ONE + e) * ATANH(e)) / e)) * TWO
    v = (((a * a) * (pi_(ONE))) * aco) * TWO

  end function psgp_surf_area_d
!!!_  & psgp_xlo - solve (x, lon) from (y, lat)
  subroutine psgp_xlo_tr_d &
       & (x, dlo, y, cla, cco, xs, sf)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out)          :: x
    real(kind=KTGT),intent(out)          :: dlo(*)
    real(kind=KTGT),intent(in)           :: y
    real(kind=KTGT),intent(in)           :: cla(*)
    real(kind=KTGT),intent(in)           :: cco(*)
    real(kind=KTGT),intent(in)           :: xs   ! sign of sin(dlo) or x
    real(kind=KTGT),intent(out),optional :: sf

    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: ONE=1.0_KTGT
    real(kind=KTGT),parameter :: TWO=2.0_KTGT
    real(kind=KTGT) :: cdl

    if (cla(icache_psgp_yrho).eq.ZERO) then
       _COS(dlo) = ZERO
       _SIN(dlo) = SIGN(ONE, xs)
       x = SIGN(ZERO, xs)
    else
       cdl = - y / cla(icache_psgp_yrho)
       if (ABS(cdl).gt.ONE) then
          _SIN(dlo) = SIGN(TWO, xs) !  no solution
          x = + SIGN(HUGE(ZERO), xs)
       else
          _COS(dlo) = cdl
          !! caution: sign must be adjusted by the caller
          _SIN(dlo) = SIGN(SQRT((ONE + cdl) * (ONE - cdl)), xs)
          x = + SIGN(cla(icache_psgp_xrho) * _SIN(dlo), xs)
       endif
    endif

    if (present(sf)) then
       sf = cla(icache_psgp_sf)
    endif
  end subroutine psgp_xlo_tr_d

!!!_  & psgp_xlo_once - solve (x, lon) from (y, lat)
  subroutine psgp_xlo_once_tr_d &
       & (x, dlo, y, lat, xs, cco, sf)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out)          :: x
    real(kind=KTGT),intent(out)          :: dlo(*)
    real(kind=KTGT),intent(in)           :: y
    real(kind=KTGT),intent(in)           :: lat
    real(kind=KTGT),intent(in)           :: cco(*)
    real(kind=KTGT),intent(in)           :: xs   ! sign of x
    real(kind=KTGT),intent(out),optional :: sf

    real(kind=KTGT) :: tla(ncache_psgp_la)

    call psgp_cachela(tla, lat, cco)
    call psgp_xlo_tr(x, dlo, y, tla, cco, xs, sf)
  end subroutine psgp_xlo_once_tr_d

!!!_  & psgp_ylo - solve (y,lon) from (x,lat)
  subroutine psgp_ylo_tr_d &
       & (y, dlo, x, cla, cco, ys, sf)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out)          :: y
    real(kind=KTGT),intent(out)          :: dlo(*)
    real(kind=KTGT),intent(in)           :: x
    real(kind=KTGT),intent(in)           :: cla(*)
    real(kind=KTGT),intent(in)           :: cco(*)
    real(kind=KTGT),intent(in)           :: ys   ! sign of y (or s cos(lo))
    real(kind=KTGT),intent(out),optional :: sf

    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: ONE=1.0_KTGT
    real(kind=KTGT),parameter :: TWO=2.0_KTGT
    real(kind=KTGT) :: sdl

    if (cla(icache_psgp_xrho).eq.ZERO) then
       _SIN(dlo) = ZERO
       _COS(dlo) = - SIGN(ONE, ys) * cco(icache_psgp_sign)
       y = SIGN(ZERO, ys)
    else
       sdl = + x / cla(icache_psgp_xrho)
       if (ABS(sdl).gt.ONE) then
          _SIN(dlo) = TWO * cco(icache_psgp_sign) ! no solution
          _COS(dlo) = - SIGN(TWO, ys) * cco(icache_psgp_sign)
          y = + SIGN(HUGE(ZERO), ys)
       else
          _SIN(dlo) = sdl * cco(icache_psgp_sign)
          !! caution: sign must be adjusted by the caller
          _COS(dlo) = SQRT((ONE + sdl) * (ONE - sdl))
          _COS(dlo) = - SIGN(_COS(dlo), ys) * cco(icache_psgp_sign)
          y = + SIGN(cla(icache_psgp_yrho) * _COS(dlo), ys)
       endif
    endif

    if (present(sf)) then
       sf = cla(icache_psgp_sf)
    endif
  end subroutine psgp_ylo_tr_d

!!!_  & psgp_ylo_once - solve (y, lon) from (x, lat)
  subroutine psgp_ylo_once_tr_d &
       & (y, dlo, x, lat, ys, cco, sf)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out)          :: y
    real(kind=KTGT),intent(out)          :: dlo(*)
    real(kind=KTGT),intent(in)           :: x
    real(kind=KTGT),intent(in)           :: lat
    real(kind=KTGT),intent(in)           :: cco(*)
    real(kind=KTGT),intent(in)           :: ys   ! sign of y
    real(kind=KTGT),intent(out),optional :: sf

    real(kind=KTGT) :: tla(ncache_psgp_la)

    call psgp_cachela(tla, lat, cco)
    call psgp_ylo_tr(y, dlo, x, tla, cco, ys, sf)
  end subroutine psgp_ylo_once_tr_d

!!!_  & psgp_xla - solve (x,lat) from (y,lon)
  subroutine psgp_xla_tr_d &
       & (x, gla, y, clo, cco, sf)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out)          :: x
    real(kind=KTGT),intent(out)          :: gla(*)
    real(kind=KTGT),intent(in)           :: y
    real(kind=KTGT),intent(in)           :: clo(*)
    real(kind=KTGT),intent(in)           :: cco(*)
    real(kind=KTGT),intent(out),optional :: sf

    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: ONE=1.0_KTGT

    real(kind=KTGT) :: rhonml, tglat, s, d, e2c

    if (clo(icache_psgp_cosdlo).eq.ZERO) then
       ! undetermined
       ! return invalid values
       _SIN(gla) = ZERO
       _COS(gla) = ZERO
       x = ZERO
       if (present(sf)) then
          sf = ONE
       endif
    else
       s   = cco(icache_psgp_sign)

       rhonml = ABS(y / cco(icache_psgp_yco) / clo(icache_psgp_cosdlo))
       if (rhonml.eq.ZERO) then
          _SIN(gla) = s * ONE
          _COS(gla) = ZERO
          if (present(sf)) then
             sf = cco(icache_psgp_psf)
          endif
       else
          tglat = psgp_bwd_core(rhonml, cco)

          d = _hypot(ONE, tglat)
          _SIN(gla) = s * tglat / d
          _COS(gla) = ONE / d
          x = + (rhonml * cco(icache_psgp_xco)) * clo(icache_psgp_sindlo) * s
          if (present(sf)) then
             e2c = cco(icache_psgp_e2c)
             sf = rhonml * SQRT(ONE + e2c * (tglat * tglat))
          endif
       endif
    endif
  end subroutine psgp_xla_tr_d
!!!_  & psgp_xla_once - solve (x,lat) from (y,lon)
  subroutine psgp_xla_once_tr_d &
       & (x, gla, y, lon, cco, sf)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out)          :: x
    real(kind=KTGT),intent(out)          :: gla(*)
    real(kind=KTGT),intent(in)           :: y
    real(kind=KTGT),intent(in)           :: lon
    real(kind=KTGT),intent(in)           :: cco(*)
    real(kind=KTGT),intent(out),optional :: sf

    real(kind=KTGT) :: tlo(ncache_psgp_lo)

    call psgp_cachelo(tlo, lon, cco)
    call psgp_xla_tr(x, gla, y, tlo, cco, sf)

  end subroutine psgp_xla_once_tr_d
!!!_  & psgp_yla - solve (y,lat) from (x,lon)
  subroutine psgp_yla_tr_d &
       & (y, gla, x, clo, cco, sf)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out)          :: y
    real(kind=KTGT),intent(out)          :: gla(*)
    real(kind=KTGT),intent(in)           :: x
    real(kind=KTGT),intent(in)           :: clo(*)
    real(kind=KTGT),intent(in)           :: cco(*)
    real(kind=KTGT),intent(out),optional :: sf

    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: ONE=1.0_KTGT

    real(kind=KTGT) :: rhonml, tglat, s, d, e2c

    if (clo(icache_psgp_sindlo).eq.ZERO) then
       ! undetermined
       ! return invalid values
       _SIN(gla) = ZERO
       _COS(gla) = ZERO
       y = ZERO
       if (present(sf)) then
          sf = ONE
       endif
    else
       s   = cco(icache_psgp_sign)

       rhonml = ABS(x / cco(icache_psgp_xco) / clo(icache_psgp_sindlo))

       if (rhonml.eq.ZERO) then
          _SIN(gla) = s * ONE
          _COS(gla) = ZERO
          if (present(sf)) then
             sf = cco(icache_psgp_psf)
          endif
       else
          tglat = psgp_bwd_core(rhonml, cco)

          d = _hypot(ONE, tglat)
          _SIN(gla) = s * tglat / d
          _COS(gla) = ONE / d
          y = - (rhonml * cco(icache_psgp_yco)) * clo(icache_psgp_cosdlo) * s
          if (present(sf)) then
             e2c = cco(icache_psgp_e2c)
             sf = rhonml * SQRT(ONE + e2c * (tglat * tglat))
          endif
       endif
    endif
  end subroutine psgp_yla_tr_d
!!!_  & psgp_yla_once - solve (y,lat) from (x,lon)
  subroutine psgp_yla_once_tr_d &
       & (y, gla, x, lon, cco, sf)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out)          :: y
    real(kind=KTGT),intent(out)          :: gla(*)
    real(kind=KTGT),intent(in)           :: x
    real(kind=KTGT),intent(in)           :: lon
    real(kind=KTGT),intent(in)           :: cco(*)
    real(kind=KTGT),intent(out),optional :: sf

    real(kind=KTGT) :: tlo(ncache_psgp_lo)

    call psgp_cachelo(tlo, lon, cco)
    call psgp_yla_tr(y, gla, x, tlo, cco, sf)

  end subroutine psgp_yla_once_tr_d

!!!_  & flatten_to_ecc ()
  ELEMENTAL &
  function flatten_to_ecc_d (f) result (e)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: e
    real(kind=KTGT),intent(in) :: f
    ! e = sqrt(2.0_KTGT * f - f * f)
    e = sqrt((2.0_KTGT - f) * f)
  end function flatten_to_ecc_d
!!!_  & reduced_latitude() - sin,cos(param. lat.)
  function reduced_latitude_d (glat, f) result (plat)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: plat(NTRIG)
    real(kind=KTGT),intent(in) :: glat(NTRIG)
    real(kind=KTGT),intent(in) :: f
    real(kind=KTGT),parameter :: ONE=1.0_KTGT

    plat(1:NTRIG) = nml_sincos(_SIN(glat) * (ONE - f), _COS(glat))
    return
  end function reduced_latitude_d
!!!_  & azimuth_node() - sin,cos(azimuth node)
  function azimuth_node_d (aref, plat) result (eazim)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: eazim(NTRIG)
    real(kind=KTGT),intent(in) :: aref(NTRIG)
    real(kind=KTGT),intent(in) :: plat(NTRIG)
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: ONE=1.0_KTGT

    if (_COS(aref).eq.ZERO.and._SIN(aref).eq.ZERO) then
       _SIN(eazim) = ONE
       _COS(eazim) = ZERO
    else
       _SIN(eazim) = _SIN(aref) * _COS(plat)
       _COS(eazim) = _hypot(_COS(aref), _SIN(aref) * _SIN(plat))
    endif

    return
  end function azimuth_node_d
!!!_  & arcl_auxsph()
  function arcl_auxsph_d &
       & (azim, plat) &
       & result (aarc)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: aarc(NTRIG)
    real(kind=KTGT),intent(in) :: azim(*)
    real(kind=KTGT),intent(in) :: plat(*)
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT

    if (_COS(azim).eq.ZERO.and._SIN(plat).eq.ZERO) then
       _SIN(aarc) = ZERO  ! from _SIN(plat1) == 0
       _COS(aarc) = ONE
    else
       aarc(1:NTRIG)  = nml_sincos(_SIN(plat), _COS(azim) * _COS(plat))
    endif
  end function arcl_auxsph_d
!!!_  & alon_auxsph_ph()
  function alon_auxsph_ph_d &
       & (eazim, aref, plat) &
       & result (alonp)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: alonp(NTRIG)
    real(kind=KTGT),intent(in) :: eazim(NTRIG)
    real(kind=KTGT),intent(in) :: aref(NTRIG)
    real(kind=KTGT),intent(in) :: plat(NTRIG)
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT

    _SIN(alonp) = _SIN(plat) * _SIN(eazim)
    _COS(alonp) = _COS(plat) * _COS(aref)
  end function alon_auxsph_ph_d
!!!_  & conformal_latitude() - tan(conf. lat)
  ELEMENTAL &
  function conformal_latitude_d (tglat, ecc) result (x)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: x
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
!!!_  & geodesic_direct
  subroutine geodesic_direct_d &
       & (ierr,  &
       &  glat2, dglon, &
       &  glat1, azim1, gdis,  &
       &  f,     a)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: glat2(NTRIG)    ! geographic latitude of target
    real(kind=KTGT),intent(out) :: dglon(3)    ! diffence in geographic longitude
    real(kind=KTGT),intent(in)  :: glat1(NTRIG)    ! geographic latitude of origin
    real(kind=KTGT),intent(in)  :: azim1(NTRIG)    ! azimuth of origin
    real(kind=KTGT),intent(in)  :: gdis        ! geodesic distance toward target
    real(kind=KTGT),intent(in)  :: f, a        ! flattening and equatorial radius

    real(kind=KTGT) :: eazim(NTRIG)    ! azimuth of the node [alpha_0]
    real(kind=KTGT) :: plat1(NTRIG)    ! parametric latitude of origin [beta_1]
    real(kind=KTGT) :: aarc1(NTRIG)    ! arc length on auxiliary sphere [sigma_1]
    real(kind=KTGT) :: alon1_ph(NTRIG) ! longitude on the auxiliary sphere [omega_1]  (not sin,cos)

    integer,parameter :: lodr = 9
    integer,parameter :: i1odr = 6
    integer,parameter :: i3odr = 6
    real(kind=KTGT) :: C1(0:lodr)
    real(kind=KTGT) :: C1p(0:lodr)
    real(kind=KTGT) :: C3C(0:lodr, 0:lodr)
    real(kind=KTGT) :: C3(0:lodr)

    real(kind=KTGT) :: ee, ep2, fiii
    real(kind=KTGT) :: kk, eps

    real(kind=KTGT),parameter :: ONE  = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO  = 2.0_KTGT

    ierr = 0

    _TRIG(plat1) = reduced_latitude(glat1, f)
    _TRIG(eazim) = azimuth_node(azim1, plat1)
    _TRIG(aarc1) = arcl_auxsph(azim1, plat1)
    _TRIG(alon1_ph) = alon_auxsph_ph(eazim, azim1, plat1)

    fiii = f / (TWO - f)
    ee = f * (TWO - f)
    ep2 = ee / (ONE - ee)

    kk = ep2 * (_COS2(eazim))
    eps = kk / ((ONE + sqrt(ONE + kk)) * TWO + kk)

    call gen_ctable_I1(ierr, C1, i1odr, eps)
    call gen_ctable_I1p(ierr, C1p, i1odr, eps)
    call gen_ctable_elongi(ierr, C3C, i3odr, fiii)
    call gen_vtable_elongi(ierr, C3, C3C, i3odr, eps)

    call geodesic_target &
         & (ierr,  &
         &  glat2, dglon, &
         &  aarc1, alon1_ph, eazim, &
         &  gdis,  &
         &  C1,    C1p,   C3,    i1odr, i3odr,    &
         &  f,     a)

  end subroutine geodesic_direct_d
!!!_  & geodesic_target
  subroutine geodesic_target_d &
       & (ierr,  &
       &  glat2, dglon, &
       &  aarc1, alon1_ph, eazim, &
       &  gdis,  &
       &  C1,    C1p,   C3,    i1odr, i3odr,    &
       &  f,     a)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: glat2(NTRIG)
    real(kind=KTGT),intent(out) :: dglon(3)
    real(kind=KTGT),intent(in)  :: aarc1(NTRIG)
    real(kind=KTGT),intent(in)  :: alon1_ph(NTRIG)
    real(kind=KTGT),intent(in)  :: eazim(NTRIG)
    real(kind=KTGT),intent(in)  :: gdis
    real(kind=KTGT),intent(in)  :: C1(0:*), C1p(0:*), C3(0:*)
    integer,        intent(in)  :: i1odr,   i3odr
    real(kind=KTGT),intent(in)  :: f, a

    real(kind=KTGT) :: azim2(NTRIG)
    real(kind=KTGT) :: plat2(NTRIG)
    real(kind=KTGT) :: aarc2(NTRIG)
    real(kind=KTGT) :: alon2_ph(NTRIG)
    real(kind=KTGT) :: dalon_ph(NTRIG)

    real(kind=KTGT) :: ee, ep2, fiii
    real(kind=KTGT) :: b

    real(kind=KTGT) :: A1
    real(kind=KTGT) :: B1, B1p, dB3, dI3
    real(kind=KTGT) :: AA(2)
    real(kind=KTGT) :: F1(2)

    real(kind=KTGT) :: rarc1(NTRIG), rarc2(NTRIG)  ! tau_1,2
    real(kind=KTGT) :: daarc(NTRIG)  ! sig_{12}
    real(kind=KTGT) :: saarc(NTRIG)  ! sig_1 + sig_2
    real(kind=KTGT) :: drarc     ! tau_{12}
    real(kind=KTGT) :: dtmp, daa
    real(kind=KTGT) :: sctmp(NTRIG)

    real(kind=KTGT),parameter :: ONE  = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO  = 2.0_KTGT

    ierr = 0

    fiii = f / (TWO - f)
    ee = f * (TWO - f)
    ep2 = ee / (ONE - ee)
    b = a * (ONE - f)

    ! B1: B1(sigma1) == tau1 - sigma1
    B1 = comp_series_sine(C1, i1odr, _SIN(aarc1), _COS(aarc1))
    A1 = ONE + C1(0)
    ! rarc1: tau_1 = sigma_1 + B1(sigma_1)
    _TRIG(sctmp) = set_sincos(B1)
    _TRIG(rarc1) = add_angle(aarc1, sctmp)
    ! drarc: tau_12
    drarc = gdis / (b * A1)
    ! rarc2: tau_2 = tau_1 + tau_12
    _TRIG(sctmp) = set_sincos(drarc)
    _TRIG(rarc2) = add_angle(rarc1, sctmp)

    ! B1p: B1p(tau2) == tau2 - sigma2
    B1p = comp_series_sine(C1p, i1odr, _SIN(rarc2), _COS(rarc2))

    ! daarc: sigma_12 = tau_12 + [B1(sigma1) + B1p(tau2)]
    daa = drarc + (B1 + B1p)
    _TRIG(daarc) = set_sincos(daa)

    _TRIG(aarc2) = add_angle(aarc1, daarc)
    _TRIG(saarc) = add_angle(aarc1, aarc2)

    _TRIG(azim2) = nml_sincos(_SIN(eazim), _COS(eazim) * _COS(aarc2))
    _SIN(plat2) = _COS(eazim) * _SIN(aarc2)
    _COS(plat2) = _hypot(_COS(eazim) * _COS(aarc2), _SIN(eazim))
    _SIN(alon2_ph) = _SIN(aarc2) * _SIN(eazim)
    _COS(alon2_ph) = _COS(aarc2)

    call set_diff_xinteg(AA, F1, daarc, saarc)
    call comp_diff_xinteg(dB3, C3, i3odr, AA, F1)
    dI3 = f * C3(0) * _SIN(eazim) * (dB3 + daa)

    _TRIG(dalon_ph) = sub_angle(alon2_ph, alon1_ph)
    dtmp = phase(dalon_ph)
    _ANGLE(dglon) = dtmp - dI3
    _TRIG(dglon) = set_sincos(_ANGLE(dglon))

    _TRIG(glat2) = nml_sincos(_SIN(plat2), _COS(plat2)  * (ONE - f))

  end subroutine geodesic_target_d
!!!_  & geodesic_direct
  subroutine geodesic_direct_core_d &
       & (ierr,  &
       &  glat2, dglon, &
       &  glat1, azim1, gdis,  &
       &  f,     a)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    use TOUZA_Std,only: msg_grp
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: glat2(NTRIG)
    real(kind=KTGT),intent(out) :: dglon(3)
    real(kind=KTGT),intent(in)  :: glat1(NTRIG)
    real(kind=KTGT),intent(in)  :: azim1(NTRIG)
    real(kind=KTGT),intent(in)  :: gdis
    real(kind=KTGT),intent(in)  :: f, a

    real(kind=KTGT) :: eazim(NTRIG)
    real(kind=KTGT) :: azim2(NTRIG)
    real(kind=KTGT) :: plat1(NTRIG), plat2(NTRIG)
    real(kind=KTGT) :: aarc1(NTRIG), aarc2(NTRIG)
    real(kind=KTGT) :: alon1_ph(NTRIG), alon2_ph(NTRIG)
    real(kind=KTGT) :: dalon_ph(NTRIG), dalon(NTRIG)

    real(kind=KTGT) :: ee, ep2, eps, kk, fiii, b
    real(kind=KTGT) :: dis1, sdis, rdis
    real(kind=KTGT) :: rarc, st, ct
    real(kind=KTGT) :: daarc
    real(kind=KTGT) :: ds(NTRIG), ss(NTRIG)

    integer,parameter :: lodr = 9
    integer,parameter :: i1odr = 6
    integer,parameter :: i3odr = 6
    real(kind=KTGT) :: C1(0:lodr)
    real(kind=KTGT) :: C1p(0:lodr)
    real(kind=KTGT) :: C3C(0:lodr, 0:lodr)
    real(kind=KTGT) :: C3(0:lodr)
    real(kind=KTGT) :: I1, I1p
    real(kind=KTGT) :: dI3
    real(kind=KTGT) :: AA(2)
    real(kind=KTGT) :: F1(2)

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT

    ierr = 0

    fiii = f / (TWO - f)
    ee = f * (TWO - f)
    ep2 = ee / (ONE - ee)
    b = a * (ONE - f)

    call gen_ctable_elongi(ierr, C3C, i3odr, fiii)

    ! NEA
    if (ierr.eq.0) then
       ! parametric latitudes
       _TRIG(plat1) = nml_sincos(_SIN(glat1) * (ONE - f), _COS(glat1))
       ! azimuth at the node
       if (_COS(azim1).eq.ZERO.and._SIN(plat1).eq.ZERO) then
          _SIN(eazim) = ONE
          _COS(eazim) = ZERO
       else
          _SIN(eazim) = _SIN(azim1) * _COS(plat1)
          _COS(eazim) = _hypot(_COS(azim1), _SIN(azim1) * _SIN(plat1))
       endif
       _TRIG(aarc1) = arcl_auxsph(azim1, plat1)
       ! longitudes on the auxiliary sphere
       _SIN(alon1_ph) = _SIN(plat1) * _SIN(eazim)
       _COS(alon1_ph) = _COS(azim1) * _COS(plat1)

       ! call diag_sc('beta1', plat1)
       ! call diag_sc('alpha0', eazim)
       ! call diag_sc('sigma1', aarc1)
       ! call diag_ph('omega1', alon1_ph)

       ! arc length on the auxiliary sphere
       kk = ep2 * (_COS2(eazim))
       eps = kk / ((ONE + sqrt(ONE + kk)) * TWO + kk)
       ! I1 coefficients
       call gen_ctable_I1(ierr, C1, i1odr, eps)
       call gen_ctable_I1p(ierr, C1p, i1odr, eps)
       ! I1 series
       I1 = comp_series_sine(C1, i1odr, _SIN(aarc1), _COS(aarc1))
       ! distance
       rarc = phase(aarc1)
       dis1 = (rarc + I1) * (ONE + C1(0)) * b
       sdis = dis1 + gdis
       rdis = (rarc + I1) + gdis / (b * (ONE + C1(0)))
       ! write(*, *) 'sdis:', kk, eps, sdis, rdis, dis1, ONE + C1(0), dis1 / b
       st = sin(rdis)
       ct = cos(rdis)
       ! call diag_sc('tau', (/st, ct/))
       I1p = comp_series_sine(C1p, i1odr, st, ct)
       ! write(*, *) 'C1p:', C1p(0:i1odr)
       rarc = rdis + I1p
       ! write(*, *) 'rarc:', rarc, rad2deg(rarc), rdis, rad2deg(rdis), I1p
       _SIN(aarc2) = sin(rarc)
       _COS(aarc2) = cos(rarc)
       ! call diag_sc('sigma2', aarc2)
       ! NEB
       _TRIG(azim2) = nml_sincos(_SIN(eazim), _COS(eazim) * _COS(aarc2))
       _SIN(plat2) = _COS(eazim) * _SIN(aarc2)
       _COS(plat2) = _hypot(_COS(eazim) * _COS(aarc2), _SIN(eazim))
       _SIN(alon2_ph) = _SIN(aarc2) * _SIN(eazim)
       _COS(alon2_ph) = _COS(aarc2)
       ! call diag_sc('azim2', azim2)
       ! call diag_sc('plat2', plat2)
       ! call diag_ph('alon2', alon2_ph)
       ! distance
       call gen_vtable_elongi(ierr, C3, C3C, i3odr, eps)

       _SIN(ds) = _COS(aarc1) * _SIN(aarc2) - _SIN(aarc1) * _COS(aarc2)
       _COS(ds) = _COS(aarc1) * _COS(aarc2) + _SIN(aarc1) * _SIN(aarc2)
       daarc = phase(ds)

       _SIN(ss) = _COS(aarc1) * _SIN(aarc2) + _SIN(aarc1) * _COS(aarc2)
       _COS(ss) = _COS(aarc1) * _COS(aarc2) - _SIN(aarc1) * _SIN(aarc2)

       AA(1) = + (_COS(ds) * _COS(ss)) * TWO
       AA(2) = - (_SIN(ds) * _SIN(ss)) * TWO
       F1(1) = _COS(ds) * _SIN(ss)
       F1(2) = _SIN(ds) * _COS(ss)

       call comp_diff_xinteg(dI3, C3, i3odr, AA, F1)
       dI3 = f * C3(0) * _SIN(eazim) * (dI3 + daarc)

       _SIN(dalon_ph) = _COS(alon1_ph) * _SIN(alon2_ph) - _SIN(alon1_ph) * _COS(alon2_ph)
       _COS(dalon_ph) = _COS(alon1_ph) * _COS(alon2_ph) + _SIN(alon1_ph) * _SIN(alon2_ph)

       _TRIG(dalon) = nml_sincos(_SIN(dalon_ph), _COS(dalon_ph))

       _SIN(dglon) = _SIN(dalon)*cos(dI3) - _COS(dalon)*sin(dI3)
       _COS(dglon) = _COS(dalon)*cos(dI3) + _SIN(dalon)*sin(dI3)
       _ANGLE(dglon) = phase(dglon)

       _TRIG(glat2) = nml_sincos(_SIN(plat2), _COS(plat2)  * (ONE - f))
    endif
    ! geographic longitude (relative)
    ! solution
  end subroutine geodesic_direct_core_d
!!!_  & geodesic_inverse
  subroutine geodesic_inverse_d &
       & (ierr,  &
       &  gdis,  &
       &  glat1, glat2, dglon, &
       &  f,     a,     &
       &  liter, rtol,  atol,  garea, azim1)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    use TOUZA_Std,only: msg_grp
    implicit none
    integer,        intent(out)          :: ierr
    real(kind=KTGT),intent(out)          :: gdis        ! geodesic distance toward target
    real(kind=KTGT),intent(in)           :: glat1(NTRIG)    ! geographic latitude of origin
    real(kind=KTGT),intent(in)           :: glat2(NTRIG)    ! geographic latitude of target
    real(kind=KTGT),intent(in)           :: dglon(3)    ! diffence in geographic longitude
    real(kind=KTGT),intent(in)           :: f, a        ! flattening and equatorial radius
    integer,        intent(in),optional  :: liter       ! maximum iteration
    real(kind=KTGT),intent(in),optional  :: rtol, atol  ! tolerances
    real(kind=KTGT),intent(out),optional :: garea       ! geodesic quadrilateral area on equator
    real(kind=KTGT),intent(out),optional :: azim1(NTRIG)    ! azimuth of origin

    real(kind=KTGT) :: xlat1(NTRIG), xlat2(NTRIG) ! canonicalized glat
    real(kind=KTGT) :: xglon(3)           ! canonicalized dglon
    real(kind=KTGT) :: eazim(NTRIG)           ! azimuth at node
    real(kind=KTGT) :: xazi1(NTRIG), xazi2(NTRIG) ! azimuth
    real(kind=KTGT) :: plat1(NTRIG), plat2(NTRIG) ! parametric latitude
    real(kind=KTGT) :: aarc1(NTRIG), aarc2(NTRIG) ! arc length on auxiliary sphere
    real(kind=KTGT) :: dalon_ph(NTRIG)        ! diffence in longitude on the auxiliary sphere (not sin,cos)

    real(kind=KTGT) :: asign, lasign, losign
    logical :: gswap

    real(kind=KTGT) :: ee, ep2, fiii
    integer,parameter :: lodr = 9
    integer,parameter :: i3odr = 6
    real(kind=KTGT) :: C3C(0:lodr, 0:lodr)
    integer,parameter :: i4odr = 6
    real(kind=KTGT) :: C4C(0:lodr, 0:lodr)


    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT


    ierr = 0

    fiii = f / (TWO - f)
    ee = f * (TWO - f)
    ep2 = ee / (ONE - ee)
    call gen_ctable_elongi(ierr, C3C, i3odr, fiii)

    _TRIG(xlat1) = _TRIG(glat1)
    _TRIG(xlat2) = _TRIG(glat2)
    xglon(1:3) = dglon(1:3)

    _TRIG(xazi1) = ZERO

    if (ierr.eq.0) then
       call geodesic_inverse_canonical &
            & (ierr,  &
            &  xlat1, xlat2, xglon, asign, losign, lasign, gswap)
    endif

    if (ierr.eq.0) then
       _TRIG(plat1) = reduced_latitude(xlat1, f)
       _TRIG(plat2) = reduced_latitude(xlat2, f)
    endif
    if (ierr.eq.0) then
       call geodesic_inverse_guess &
            & (ierr,  xazi1, &
            &  xlat1, xlat2, _ANGLE(xglon), &
            &  f)
    endif

    if (ierr.eq.0) then
       call geodesic_inverse_solve &
            & (ierr,  &
            &  gdis,  eazim, dalon_ph, &
            &  plat1, xazi1, aarc1, &
            &  plat2, xazi2, aarc2, &
            &  xglon, &
            &  f,     a,     C3C,   i3odr, &
            &  liter, rtol,  atol)
    endif
    if (present(garea)) then
       if (ierr.eq.0) then
          call gen_ctable_earea(ierr, C4C, i4odr, fiii)

          call geodesic_area_core &
               & (ierr,  garea, &
               &  plat1, plat2, xglon, dalon_ph,     &
               &  eazim, xazi1, xazi2, aarc1, aarc2, &
               &  f,     a,     C4C,   i4odr)
          garea = asign * garea
       endif
    endif
    if (present(azim1)) then
       if (ierr.eq.0) then
          if (gswap) then
             _TRIG(azim1) = _TRIG(xazi2)
          else
             _TRIG(azim1) = _TRIG(xazi1)
          endif
          _SIN(azim1) = _SIN(azim1) * losign
          _COS(azim1) = _COS(azim1) * lasign
       endif
    endif
  end subroutine geodesic_inverse_d
!!!_  & geodesic_inverse_solve
  subroutine geodesic_inverse_solve_d &
       & (ierr,  &
       &  gdis,  eazim, dalon_ph,    &
       &  plat1, azim1, aarc1, &
       &  plat2, azim2, aarc2, &
       &  dglon, &
       &  f,     a,     C3C,   i3odr, &
       &  liter, rtol,  atol)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    use TOUZA_Std,only: msg_grp
    implicit none
    integer,        intent(out)          :: ierr
    real(kind=KTGT),intent(out)          :: gdis
    real(kind=KTGT),intent(out)          :: dalon_ph(NTRIG)
    real(kind=KTGT),intent(out)          :: eazim(NTRIG)
    real(kind=KTGT),intent(in)           :: plat1(NTRIG), plat2(NTRIG)
    real(kind=KTGT),intent(inout)        :: azim1(NTRIG)
    real(kind=KTGT),intent(out)          :: azim2(NTRIG)
    real(kind=KTGT),intent(out)          :: aarc1(NTRIG), aarc2(NTRIG)
    real(kind=KTGT),intent(in)           :: dglon(3)
    real(kind=KTGT),intent(in)           :: f, a
    real(kind=KTGT),intent(in)           :: C3C(0:, 0:)
    integer,        intent(in)           :: i3odr
    integer,        intent(in),optional  :: liter
    real(kind=KTGT),intent(in),optional  :: rtol, atol    ! tolerances

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT

    real(kind=KTGT) :: alon1_ph(NTRIG), alon2_ph(NTRIG)  ! omega
    real(kind=KTGT) :: phtmp(NTRIG)
    real(kind=KTGT) :: dxtmp(NTRIG)
    real(kind=KTGT) :: daa
    real(kind=KTGT) :: daarc(NTRIG)  ! sig_{12}
    real(kind=KTGT) :: saarc(NTRIG)  ! sig_1 + sig_2

    real(kind=KTGT) :: ee, ep2, eps, kk, fiii
    real(kind=KTGT) :: nazim(NTRIG)

    integer jo
    integer,parameter :: lodr = 9
    integer,parameter :: i1odr = 6, i2odr = 6
    integer,parameter :: jjodr = min(i1odr, i2odr)
    real(kind=KTGT) :: C3(0:lodr)
    real(kind=KTGT) :: C2(0:lodr), C1(0:lodr)
    real(kind=KTGT) :: JJ(0:lodr)

    real(kind=KTGT) :: dB3, dI3, dJJ
    real(kind=KTGT) :: AA(2)
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
    if (etol.ge.ONE) etol = (etol * epsilon(ZERO)) * phase(dglon)

    dtol = choice(ZERO, atol)
    if (dtol.eq.ZERO) dtol = -4.0_KTGT    ! 0.1mm
    if (dtol.lt.ZERO) dtol = (10.0_KTGT ** dtol) / a

    echk = epsilon(ZERO)

    fiii = f / (TWO - f)
    ee = f * (TWO - f)
    ep2 = ee / (ONE - ee)

    if (_SIN(plat1).eq.ZERO) then
       ! equatorial
       if (abs(_ANGLE(dglon)).gt.(ONE - f) * pi_(ZERO)) then
          ierr = ERR_NOT_IMPLEMENTED
          call msg_grp('Cannot handle equatorial antipodal case', __GRP__, __MDL__)
          return
       endif
       gdis = a * abs(_ANGLE(dglon))
    else if (_SIN(plat1).le.-ONE .or. _SIN(dglon).eq.ZERO) then
       ! meridian
       ! write(*, *) 'meridian'
       ! Special treatment on azimuth[1], to be delta longitude
       ! Actually, no need to set as follows to geodesic computation,
       ! however, need to allow arbitrary longitude to area computation.
       _TRIG(azim1) = _TRIG(dglon)
       _SIN(azim2)  = ZERO
       _COS(azim2)  = ONE
       _TRIG(aarc1) = arcl_auxsph(azim1, plat1)
       _TRIG(aarc2) = arcl_auxsph(azim2, plat2)

       _TRIG(eazim) = azimuth_node(azim1, plat1)

       kk = ep2 * (_COS2(eazim))
       eps = kk / ((ONE + sqrt(ONE + kk)) * TWO + kk)

       _TRIG(saarc) = add_angle(aarc1, aarc2)
       _TRIG(daarc) = sub_angle(aarc2, aarc1)
       daa = phase(daarc)
       call set_diff_xinteg(AA, F1, daarc, saarc)
       call gen_ctable_I1(ierr, C1, i1odr, eps)
       call comp_diff_xinteg(gdis, C1, i1odr, AA, F1)
       gdis = (gdis + daa) * (C1(0) + ONE)
       gdis = gdis * (a * (ONE - f))

       ! special value to avoid dalon computation on area
       _COS(dalon_ph) = -ONE
       _SIN(dalon_ph) = ZERO
       ! write(*, *) 'alon_ph', alon2_ph, alon1_ph
    else
       ! write(*, *) 'dglon', dglon
       do iter = 0, miter - 1
          ! NEA
          _TRIG(eazim) = azimuth_node(azim1, plat1)
          _TRIG(aarc1) = arcl_auxsph(azim1, plat1)
          ! write(*, *) 'alon1', eazim, azim1, plat1
          _TRIG(alon1_ph) = alon_auxsph_ph(eazim, azim1, plat1)
          ! NEB
          ! to check when _COS(plat2)==0 (pole)
          _SIN(azim2) = _SIN(eazim) / _COS(plat2)
          _COS(azim2) = SQRT((_COS(azim1) * _COS(plat1))**2 &
               &             + (_COS(plat2) - _COS(plat1)) * (_COS(plat2) + _COS(plat1))) &
               &         / _COS(plat2)
          _TRIG(aarc2) = arcl_auxsph(azim2, plat2)
          _TRIG(alon2_ph) = alon_auxsph_ph(eazim, azim2, plat2)
          ! omega2 - omega1 phase
          ! write(*, *) 'alon_ph', alon2_ph, alon1_ph
          _TRIG(dalon_ph) = sub_angle(alon2_ph, alon1_ph)
          ! omega2 - omega1 - (lambda2 - lambda1) phase
          _TRIG(phtmp) = sub_angle(dalon_ph, _TRIG(dglon))
          ddx = phase(phtmp)

          kk = ep2 * (_COS2(eazim))
          eps = kk / ((ONE + sqrt(ONE + kk)) * TWO + kk)

          call gen_vtable_elongi(ierr, C3, C3C, i3odr, eps)
          _TRIG(saarc) = add_angle(aarc1, aarc2)
          _TRIG(daarc) = sub_angle(aarc2, aarc1)
          daa = phase(daarc)

          call set_diff_xinteg(AA, F1, daarc, saarc)
          call comp_diff_xinteg(dB3, C3, i3odr, AA, F1)

          dI3 = f * C3(0) * _SIN(eazim) * (dB3 + daa)

          ddx = ddx - dI3

          ! iteration
          call gen_ctable_I1(ierr, C1, i1odr, eps)
          call gen_ctable_I2b(ierr, C2, i2odr, eps)
          do jo = 1, jjodr
             JJ(jo) = (C1(0) + ONE) * C1(jo) - (C2(0) + ONE) * C2(jo)
          enddo
          JJ(0) = C1(0) - C2(0)
          call comp_diff_xinteg(dJJ, JJ, jjodr, AA, F1)
          dJJ = dJJ + daa * JJ(0)

          bf1 = sqrt(ONE + ep2 * (_COS(eazim) * _SIN(aarc1)) ** 2)
          bf2 = sqrt(ONE + ep2 * (_COS(eazim) * _SIN(aarc2)) ** 2)

          mob = bf2 * _COS(aarc1) * _SIN(aarc2) - bf1 * _SIN(aarc1) * _COS(aarc2) &
               & - (_COS(aarc1) * _COS(aarc2)) * dJJ

          ddy = (mob * (ONE - f)) / (_COS(azim2) * _COS(plat2))

          dsol = - ddx / ddy

          if (abs(ddx).le.etol) exit
          if (abs(ddx).le.echk.and.abs(dsol).le.dtol) exit

          _TRIG(dxtmp) = set_sincos(dsol)

          _TRIG(nazim) = add_angle(azim1, dxtmp)

          _TRIG(azim1) = nml_sincos(_SIN(nazim), _COS(nazim))
       enddo

       call comp_diff_xinteg(gdis, C1, i1odr, AA, F1)
       gdis = (gdis + daa) * (C1(0) + ONE)
       gdis = gdis * (a * (ONE - f))

    endif
    return
  end subroutine geodesic_inverse_solve_d
!!!_  & geodesic_area_core
  subroutine geodesic_area_core_d &
       & (ierr,  garea, &
       &  plat1, plat2, dglon, dalon_ph, &
       &  eazim, azim1, azim2, aarc1, aarc2, &
       &  f,     a,     C4C,   i4odr)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    use TOUZA_Std,only: msg_grp
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: garea
    real(kind=KTGT),intent(in)  :: plat1(NTRIG)      ! sin, cos
    real(kind=KTGT),intent(in)  :: plat2(NTRIG)      ! sin, cos
    real(kind=KTGT),intent(in)  :: dglon(3)
    real(kind=KTGT),intent(in)  :: dalon_ph(NTRIG)      ! phase
    real(kind=KTGT),intent(in)  :: eazim(NTRIG)
    real(kind=KTGT),intent(in)  :: azim1(NTRIG), azim2(NTRIG)
    real(kind=KTGT),intent(in)  :: aarc1(NTRIG), aarc2(NTRIG)
    real(kind=KTGT),intent(in)  :: f, a
    real(kind=KTGT),intent(in)  :: C4C(0:, 0:)
    integer,        intent(in)  :: i4odr

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT

    integer,parameter :: lodr = 9
    real(kind=KTGT) :: C4(0:lodr), A4
    real(kind=KTGT) :: dI4
    real(kind=KTGT) :: AA(2)
    real(kind=KTGT) :: F0(2)
    real(kind=KTGT) :: daarc(NTRIG), saarc(NTRIG)

    real(kind=KTGT) :: ee, ep2, eps, kk, fiii, aco
    real(kind=KTGT) :: da(NTRIG), dazim, cc, b

    real(kind=KTGT) :: dalon(NTRIG)

    ierr = 0

    if (_SIN(plat1).eq.ZERO) then
       ! equatorial
       if (abs(_ANGLE(dglon)).gt.(ONE - f) * pi_(ZERO)) then
          ierr = ERR_NOT_IMPLEMENTED
          call msg_grp('Cannot handle equatorial antipodal case', __GRP__, __MDL__)
          garea = ZERO
          return
       endif
       garea = ZERO
       return
    endif

    fiii = f / (TWO - f)
    ee = f * (TWO - f)
    ep2 = ee / (ONE - ee)

    if (ee.eq.ZERO) then
       aco = ONE
    else
       aco = SQRT(ee)
       aco = ATANH(aco) / aco
    endif

    kk = ep2 * (_COS2(eazim))
    eps = kk / ((ONE + sqrt(ONE + kk)) * TWO + kk)
    call gen_vtable_earea(ierr, C4, C4C, i4odr, eps)

    A4 = (a * a) * (_COS(eazim) * _SIN(eazim)) * ee

    _TRIG(saarc) = add_angle(aarc1, aarc2)
    _TRIG(daarc) = sub_angle(aarc2, aarc1)

    call set_diff_ainteg(AA, F0, daarc, saarc, aarc1, aarc2)
    call comp_diff_ainteg(dI4, C4, i4odr, AA, F0)

    dI4 = A4 * dI4

    ! write(*, *) 'dalon_ph', dalon_ph
    _TRIG(dalon) = nml_sincos(_SIN(dalon_ph), _COS(dalon_ph))

    ! if (.false.) then
    if ((_COS(dalon).gt.-SQRT(TWO)/TWO) &
         & .and. (_SIN(plat2)-_SIN(plat1).lt.1.75_KTGT)) then
       dazim = geodesic_dazim(plat1, plat2, dalon)
    else
       _TRIG(da) = sub_angle(azim2, azim1)
       dazim = phase(da)
    endif

    b = a * (ONE - f)
    ! cc = (a * a + (b * b) * (ATANH(SQRT(ee)) / SQRT(ee))) / TWO
    cc = (a * a + (b * b) * aco) / TWO

    garea = cc * dazim + dI4
  end subroutine geodesic_area_core_d

!!!_  & geodesic_dazim ()
  PURE &
  function geodesic_dazim_d &
       & (plat1, plat2, dalon) &
       & result(v)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in) :: plat1(*), plat2(*), dalon(*)

    real(kind=KTGT) :: cb1d, cb2d
    real(kind=KTGT) :: da(NTRIG)
    real(kind=KTGT),parameter :: ONE=1.0_KTGT
    real(kind=KTGT),parameter :: TWO=2.0_KTGT

    ! identity
    ! tan[(a2-a1)/2] = (sin[(b2+b1)/2] / cos[(b2-b1)/2]) * tan[(w2-w1)/2]
    cb1d = ONE + _COS(plat1)
    cb2d = ONE + _COS(plat2)
    _SIN(da) = _SIN(dalon) * (_SIN(plat1) * cb2d + _SIN(plat2) * cb1d)
    _COS(da) = (ONE + _COS(dalon)) * (_SIN(plat1) * _SIN(plat2) + cb1d * cb2d)
    v = phase(da) * TWO
  end function geodesic_dazim_d
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  PURE &
  function geodesic_dazim_q &
       & (plat1, plat2, dalon) &
       & result(v)
    use TOUZA_Std,only: KTGT=>KQPL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in) :: plat1(*), plat2(*), dalon(*)

    real(kind=KTGT) :: cb1d, cb2d
    real(kind=KTGT) :: da(NTRIG)
    real(kind=KTGT),parameter :: ONE=1.0_KTGT
    real(kind=KTGT),parameter :: TWO=2.0_KTGT

    ! identity
    ! tan[(a2-a1)/2] = (sin[(b2+b1)/2] / cos[(b2-b1)/2]) * tan[(w2-w1)/2]
    cb1d = ONE + _COS(plat1)
    cb2d = ONE + _COS(plat2)
    _SIN(da) = _SIN(dalon) * (_SIN(plat1) * cb2d + _SIN(plat2) * cb1d)
    _COS(da) = (ONE + _COS(dalon)) * (_SIN(plat1) * _SIN(plat2) + cb1d * cb2d)
    ! write(*, *) (_SIN(plat1) * cb2d + _SIN(plat2) * cb1d), (_SIN(plat1) * _SIN(plat2) + cb1d * cb2d)
    ! write(*, *) (_SIN(plat1) * cb2d + _SIN(plat2) * cb1d) / (_SIN(plat1) * _SIN(plat2) + cb1d * cb2d)
    v = phase(da) * TWO
  end function geodesic_dazim_q
#endif
!!!_  & geodesic_inverse
  subroutine geodesic_inverse_core_d &
       & (ierr,  &
       &  gdis,  &
       &  glat1, glat2, dglon, &
       &  inia1, f,     a,     &
       &  rtol,  atol,  liter, garea)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    use TOUZA_Std,only: msg_grp
    implicit none
    integer,        intent(out)          :: ierr
    real(kind=KTGT),intent(out)          :: gdis
    real(kind=KTGT),intent(in)           :: glat1(NTRIG)      ! sin, cos
    real(kind=KTGT),intent(in)           :: glat2(NTRIG)      ! sin, cos
    real(kind=KTGT),intent(in)           :: dglon(3)      ! sin, cos, rad
    real(kind=KTGT),intent(in)           :: inia1(NTRIG)      ! initial guess of azimuth[1]
    real(kind=KTGT),intent(in)           :: f, a
    real(kind=KTGT),intent(in),optional  :: rtol, atol    ! tolerances
    integer,        intent(in),optional  :: liter
    real(kind=KTGT),intent(out),optional :: garea

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT

    real(kind=KTGT) :: eazim(NTRIG)                  ! alpha_0
    real(kind=KTGT) :: azim1(NTRIG), azim2(NTRIG)        ! alpha
    real(kind=KTGT) :: plat1(NTRIG), plat2(NTRIG)        ! beta
    real(kind=KTGT) :: aarc1(NTRIG), aarc2(NTRIG)        ! sigma
    real(kind=KTGT) :: alon1_ph(NTRIG), alon2_ph(NTRIG)  ! omega

    real(kind=KTGT) :: dalon_ph(NTRIG)
    real(kind=KTGT) :: dcx, dcy
    ! real(kind=KTGT) :: dpx, dpy, dcx, dcy
    real(kind=KTGT) :: daarc
    real(kind=KTGT) :: ds(NTRIG), ss(NTRIG)

    real(kind=KTGT) :: ee, ep2, eps, kk, fiii
    real(kind=KTGT) :: nazim(NTRIG)

    integer jo
    integer,parameter :: lodr = 9
    integer,parameter :: i3odr = 6
    integer,parameter :: i1odr = 6, i2odr = 6
    integer,parameter :: jjodr = min(i1odr, i2odr)
    real(kind=KTGT) :: C3C(0:lodr, 0:lodr)
    real(kind=KTGT) :: C3(0:lodr)
    real(kind=KTGT) :: C2(0:lodr), C1(0:lodr)
    real(kind=KTGT) :: JJ(0:lodr)

    real(kind=KTGT) :: dI3, dJJ
    real(kind=KTGT) :: AA(2)
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
    if (etol.ge.ONE) etol = (etol * epsilon(ZERO)) * phase(dglon)

    dtol = choice(ZERO, atol)
    if (dtol.eq.ZERO) dtol = -4.0_KTGT    ! 0.1mm
    if (dtol.lt.ZERO) dtol = (10.0_KTGT ** dtol) / a

    echk = epsilon(ZERO)

    fiii = f / (TWO - f)
    ee = f * (TWO - f)
    ep2 = ee / (ONE - ee)
    call gen_ctable_elongi(ierr, C3C, i3odr, fiii)

    _TRIG(azim1) = _TRIG(inia1)
    _TRIG(plat1) = nml_sincos(_SIN(glat1) * (ONE - f), _COS(glat1))
    _TRIG(plat2) = nml_sincos(_SIN(glat2) * (ONE - f), _COS(glat2))

    if (_SIN(glat1).eq.ZERO) then
       ! equatorial
       if (abs(_ANGLE(dglon)).gt.(ONE - f) * pi_(ZERO)) then
          ierr = ERR_NOT_IMPLEMENTED
          call msg_grp('Cannot handle equatorial antipodal case', __GRP__, __MDL__)
          return
       endif
       gdis = a * abs(_ANGLE(dglon))
       if (present(garea)) then
          garea = ZERO
       endif
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
          _TRIG(eazim) = nml_sincos(_SIN(eazim), _COS(eazim))

          _TRIG(aarc1) = arcl_auxsph(azim1, plat1)
          _SIN(alon1_ph) = _SIN(plat1) * _SIN(eazim)
          _COS(alon1_ph) = _COS(azim1) * _COS(plat1)
          ! NEB
          ! to check when _COS(plat2)==0 (pole)
          _SIN(azim2) = _SIN(eazim) / _COS(plat2)
          _COS(azim2) = SQRT((_COS(azim1) * _COS(plat1))**2 &
               &             +(_COS(plat2) - _COS(plat1)) * (_COS(plat2) + _COS(plat1))) &
               &         / _COS(plat2)
          _TRIG(aarc2) = arcl_auxsph(azim2, plat2)
          _SIN(alon2_ph) = _SIN(plat2) * _SIN(eazim)
          _COS(alon2_ph) = _COS(azim2) * _COS(plat2)

          ! omega2 - omega1 phase
          _SIN(dalon_ph) = _COS(alon1_ph) * _SIN(alon2_ph) - _SIN(alon1_ph) * _COS(alon2_ph)
          _COS(dalon_ph) = _COS(alon1_ph) * _COS(alon2_ph) + _SIN(alon1_ph) * _SIN(alon2_ph)
          ! dpy = alon1_ph(2) * alon2_ph(1) - alon1_ph(1) * alon2_ph(2)
          ! dpx = alon1_ph(2) * alon2_ph(2) + alon1_ph(1) * alon2_ph(1)
          ! omega2 - omega1 - (lambda2 - lambda1) phase
          dcy = _SIN(dalon_ph) * _COS(dglon) - _COS(dalon_ph) * _SIN(dglon)
          dcx = _COS(dalon_ph) * _COS(dglon) + _SIN(dalon_ph) * _SIN(dglon)
          ddx = phase(dcy, dcx)

          kk = ep2 * (_COS2(eazim))
          eps = kk / ((ONE + sqrt(ONE + kk)) * TWO + kk)

          call gen_vtable_elongi(ierr, C3, C3C, i3odr, eps)

          _SIN(ds) = _COS(aarc1) * _SIN(aarc2) - _SIN(aarc1) * _COS(aarc2)
          _COS(ds) = _COS(aarc1) * _COS(aarc2) + _SIN(aarc1) * _SIN(aarc2)
          daarc = phase(ds)

          _SIN(ss) = _COS(aarc1) * _SIN(aarc2) + _SIN(aarc1) * _COS(aarc2)
          _COS(ss) = _COS(aarc1) * _COS(aarc2) - _SIN(aarc1) * _SIN(aarc2)

          AA(1) = + (_COS(ds) * _COS(ss)) * TWO
          AA(2) = - (_SIN(ds) * _SIN(ss)) * TWO
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

          _TRIG(azim1) = nml_sincos(_SIN(nazim), _COS(nazim))
       enddo
       call comp_diff_xinteg(gdis, C1, i1odr, AA, F1)
       gdis = (gdis + daarc) * (C1(0) + ONE)
       gdis = gdis * (a * (ONE - f))

       if (present(garea)) then
          call geodesic_area_obs &
               & (ierr,  garea, &
               &  plat1, plat2, dglon, dalon_ph,    &
               &  eazim, azim1, azim2, aarc1, aarc2, &
               &  f,     a)
       endif
    endif
  end subroutine geodesic_inverse_core_d
!!!_  & geodesic_area_obs - obsolete
  subroutine geodesic_area_obs_d &
       & (ierr,  garea, &
       &  plat1, plat2, dglon, dalon_ph, &
       &  eazim, azim1, azim2, aarc1, aarc2, &
       &  f,     a)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    use TOUZA_Std,only: msg_grp
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: garea
    real(kind=KTGT),intent(in)  :: plat1(NTRIG)      ! sin, cos
    real(kind=KTGT),intent(in)  :: plat2(NTRIG)      ! sin, cos
    real(kind=KTGT),intent(in)  :: dglon(3)
    real(kind=KTGT),intent(in)  :: dalon_ph(NTRIG)      ! phase
    real(kind=KTGT),intent(in)  :: eazim(NTRIG)
    real(kind=KTGT),intent(in)  :: azim1(NTRIG), azim2(NTRIG)
    real(kind=KTGT),intent(in)  :: aarc1(NTRIG), aarc2(NTRIG)
    real(kind=KTGT),intent(in)  :: f, a

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT

    integer,parameter :: lodr = 9
    integer,parameter :: i4odr = 5
    real(kind=KTGT) :: C4C(0:lodr, 0:lodr)
    real(kind=KTGT) :: C4(0:lodr), A4
    real(kind=KTGT) :: dI4
    real(kind=KTGT) :: AA(2)
    real(kind=KTGT) :: F0(2)
    real(kind=KTGT) :: ds(NTRIG), ss(NTRIG)

    real(kind=KTGT) :: ee, ep2, eps, kk, fiii
    real(kind=KTGT) :: da(NTRIG), dazim, cc, b

    real(kind=KTGT) :: cb1d, cb2d, dalon(NTRIG)

    ierr = 0

    if (_SIN(plat1).eq.ZERO) then
       ! equatorial
       if (abs(_ANGLE(dglon)).gt.(ONE - f) * pi_(ZERO)) then
          ierr = ERR_NOT_IMPLEMENTED
          call msg_grp('Cannot handle equatorial antipodal case', __GRP__, __MDL__)
          garea = ZERO
          return
       endif
       garea = ZERO
       return
    endif

    fiii = f / (TWO - f)
    ee = f * (TWO - f)
    ep2 = ee / (ONE - ee)
    call gen_ctable_earea(ierr, C4C, i4odr, fiii)

    kk = ep2 * (_COS2(eazim))
    eps = kk / ((ONE + sqrt(ONE + kk)) * TWO + kk)
    call gen_vtable_earea(ierr, C4, C4C, i4odr, eps)

    A4 = (a * a) * (_COS(eazim) * _SIN(eazim)) * ee

    _SIN(ds) = _COS(aarc1) * _SIN(aarc2) - _SIN(aarc1) * _COS(aarc2)
    _COS(ds) = _COS(aarc1) * _COS(aarc2) + _SIN(aarc1) * _SIN(aarc2)
    _SIN(ss) = _COS(aarc1) * _SIN(aarc2) + _SIN(aarc1) * _COS(aarc2)
    _COS(ss) = _COS(aarc1) * _COS(aarc2) - _SIN(aarc1) * _SIN(aarc2)


    AA(1) = + (_COS(ds) * _COS(ss)) * TWO
    AA(2) = + (_SIN(ds) * _SIN(ss)) * TWO

    F0(1) = - (_COS(aarc2) - _COS(aarc1)) / TWO
    F0(2) = + (_COS(aarc2) + _COS(aarc1)) / TWO

    ! F1(1) = (AA(1) - ONE) * F0(1) + AA(2) * F0(2)
    ! F1(2) = AA(2) * F0(1) + (AA(1) - ONE) * F0(2)

    call comp_diff_ainteg(dI4, C4, i4odr, AA, F0)

    dI4 = A4 * dI4

    _TRIG(dalon) = nml_sincos(dalon_ph(1), dalon_ph(2))

    ! if (.false.) then
    if ((_COS(dalon).gt.-SQRT(TWO)/TWO) &
         & .and. (_SIN(plat2)-_SIN(plat1).lt.1.75_KTGT)) then
       cb1d = ONE + _COS(plat1)
       cb2d = ONE + _COS(plat2)
       _SIN(da) = _SIN(dalon) * (_SIN(plat1) * cb2d + _SIN(plat2) * cb1d)
       _COS(da) = (ONE + _COS(dalon)) * (_SIN(plat1) * _SIN(plat2) + cb1d * cb2d)
       dazim = phase(da) * TWO
    else
       _SIN(da) = _SIN(azim2) * _COS(azim1) - _COS(azim2) * _SIN(azim1)
       _COS(da) = _COS(azim2) * _COS(azim1) + _SIN(azim2) * _SIN(azim1)
       dazim = phase(da)
    endif

    b = a * (ONE - f)
    cc = (a * a + (b * b) * (ATANH(SQRT(ee)) / SQRT(ee))) / TWO

    garea = cc * dazim + dI4
  end subroutine geodesic_area_obs_d
!!!_  & geodesic_inverse_canonical
  subroutine geodesic_inverse_canonical_d &
       & (ierr,  &
       &  glat1, glat2, dglon, asign, losign, lasign, gswap)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    integer,        intent(out)   :: ierr
    real(kind=KTGT),intent(inout) :: glat1(*)      ! sin, cos
    real(kind=KTGT),intent(inout) :: glat2(*)      ! sin, cos
    real(kind=KTGT),intent(inout) :: dglon(*)      ! sin, cos
    real(kind=KTGT),intent(out),optional :: asign  ! sign factor for area
    real(kind=KTGT),intent(out),optional :: losign  ! sign factor for longitude
    real(kind=KTGT),intent(out),optional :: lasign  ! sign factor for latitude
    logical,        intent(out),optional :: gswap

    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT

    real(kind=KTGT) :: xlat1(NTRIG), xlat2(NTRIG)
    real(kind=KTGT) :: as, los, las
    logical bkeep

    ierr = 0

    as = ONE
    los = ONE
    las = ONE

    if (_SIN(dglon).lt.ZERO) then
       as = - as
       los = - los
    endif

    bkeep = ABS(_SIN(glat1)).gt.ABS(_SIN(glat2))

    if (bkeep) then
       _TRIG(xlat1) = _TRIG(glat1)
       _TRIG(xlat2) = _TRIG(glat2)
    else
       _TRIG(xlat2) = _TRIG(glat1)
       _TRIG(xlat1) = _TRIG(glat2)
    endif

    if (_SIN(xlat1).gt.ZERO) then
       as = - as
       las = - las
    endif

    _SIN(xlat2) = _SIN(xlat2) * (- SIGN(ONE, _SIN(xlat1)))
    _SIN(xlat1) = - ABS(_SIN(xlat1))

    _TRIG(glat1) = _TRIG(xlat1)
    _TRIG(glat2) = _TRIG(xlat2)

    _SIN(dglon) = ABS(_SIN(dglon))
    _ANGLE(dglon) = ABS(_ANGLE(dglon))

    if (present(asign)) then
       asign = as
    endif
    if (present(lasign)) then
       if (.not.bkeep) las = - las
       lasign = las
    endif
    if (present(losign)) then
       losign = los
    endif
    if (present(gswap)) then
       gswap = .not. bkeep
    endif

    ! note
    !    if swap        pi - azim2
    !    if swap -lat   azim2
    !    if -lat        pi - azim1

  end subroutine geodesic_inverse_canonical_d
!!!_  & geodesic_inverse_guess
  subroutine geodesic_inverse_guess_d &
       & (ierr,  inia1, &
       &  glat1, glat2, dglon, &
       &  f)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: inia1(NTRIG)      ! initial guess of azimuth[1]
    real(kind=KTGT),intent(in)  :: glat1(NTRIG)      ! sin, cos
    real(kind=KTGT),intent(in)  :: glat2(NTRIG)      ! sin, cos
    real(kind=KTGT),intent(in)  :: dglon
    real(kind=KTGT),intent(in)  :: f

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT

    real(kind=KTGT) :: plat1(NTRIG), plat2(NTRIG)        ! beta
    real(kind=KTGT) :: e2
    real(kind=KTGT) :: wd
    real(kind=KTGT) :: dalon(NTRIG) ! omega
    real(kind=KTGT) :: zx, zy, zz

    ierr = 0

    _TRIG(plat1) = nml_sincos(_SIN(glat1) * (ONE - f), _COS(glat1))
    _TRIG(plat2) = nml_sincos(_SIN(glat2) * (ONE - f), _COS(glat2))

    e2 = f * (TWO - f)
    wd = sqrt(ONE - e2 * ((_COS(plat1) + _COS(plat2)) / TWO)**2)
    ! write(*, *) 'guess: ', wd, rad2deg(dglon / wd)

    _TRIG(dalon) = set_sincos(dglon / wd)

    zx = _COS(plat1) * _SIN(plat2) - _SIN(plat1) * _COS(plat2) * _COS(dalon)
    zy = _COS(plat2) * _SIN(dalon)
    zz = _hypot(zx, zy)

    _SIN(inia1) = zy / zz
    _COS(inia1) = zx / zz

    ! write(*, *) _SIN(glat1), dglon, (ONE - f) * pi_(ZERO)
  end subroutine geodesic_inverse_guess_d
!!!_  & set_diff_xinteg
  subroutine set_diff_xinteg_d &
       & (AA, F1, dsc, ssc)
    use TOUZA_Std,only: KTGT=>KDBL
    real(kind=KTGT),intent(out) :: AA(2)
    real(kind=KTGT),intent(out) :: F1(2)
    real(kind=KTGT),intent(in)  :: dsc(2)   ! sin,cos(A-B)
    real(kind=KTGT),intent(in)  :: ssc(2)   ! sin,cos(A+B)

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT

    AA(1) = + (_COS(dsc) * _COS(ssc)) * TWO
    AA(2) = - (_SIN(dsc) * _SIN(ssc)) * TWO
    F1(1) = _COS(dsc) * _SIN(ssc)
    F1(2) = _SIN(dsc) * _COS(ssc)

  end subroutine set_diff_xinteg_d
!!!_  & comp_diff_xinteg
  subroutine comp_diff_xinteg_d &
       & (z, C, modr, AA, F1)
    use TOUZA_Std,only: KTGT=>KDBL
    real(kind=KTGT),intent(out) :: z
    integer,        intent(in)  :: modr
    real(kind=KTGT),intent(in)  :: C(0:*)
    real(kind=KTGT),intent(in)  :: AA(2)
    real(kind=KTGT),intent(in)  :: F1(2)

    real(kind=KTGT) :: B1(2), B2(2)
    integer jo, mo
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT

    B2(1:2) = ZERO
    B1(2) = ZERO
    if (mod(modr, 2).eq.1) then
       B1(1) = C(modr)
       mo = modr - 1
    else
       B1(1) = ZERO
       mo = modr
    endif

    do jo = mo, 2, -2
       B2(1:2) = (/C(jo), ZERO/)   + ((AA(1:2) * B1(1) + AA(2:1:-1) * B1(2)) - B2(1:2))
       B1(1:2) = (/C(jo-1), ZERO/) + ((AA(1:2) * B2(1) + AA(2:1:-1) * B2(2)) - B1(1:2))
    enddo
    z = (B1(2) * F1(1) + B1(1) * F1(2)) * TWO
  end subroutine comp_diff_xinteg_d
!!!_  & set_diff_ainteg
  subroutine set_diff_ainteg_d &
       & (AA, F0, dsc, ssc, a1, a2)
    use TOUZA_Std,only: KTGT=>KDBL
    real(kind=KTGT),intent(out) :: AA(2)
    real(kind=KTGT),intent(out) :: F0(2)
    real(kind=KTGT),intent(in)  :: dsc(NTRIG)   ! sin,cos(A-B)
    real(kind=KTGT),intent(in)  :: ssc(NTRIG)   ! sin,cos(A+B)
    real(kind=KTGT),intent(in)  :: a1(NTRIG), a2(NTRIG)

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT

    AA(1) = + (_COS(dsc) * _COS(ssc)) * TWO
    AA(2) = + (_SIN(dsc) * _SIN(ssc)) * TWO

    !! Need to check accuracy
    F0(1) = - (_COS(a2) - _COS(a1)) / TWO
    F0(2) = + (_COS(a2) + _COS(a1)) / TWO

    ! F1(1) = (AA(1) - ONE) * F0(1) + AA(2) * F0(2)
    ! F1(2) = AA(2) * F0(1) + (AA(1) - ONE) * F0(2)

  end subroutine set_diff_ainteg_d
!!!_  & comp_diff_ainteg
  subroutine comp_diff_ainteg_d &
       & (z, C, modr, AA, F0)
    use TOUZA_Std,only: KTGT=>KDBL
    real(kind=KTGT),intent(out) :: z
    integer,        intent(in)  :: modr
    real(kind=KTGT),intent(in)  :: C(0:*)
    real(kind=KTGT),intent(in)  :: AA(2)
    real(kind=KTGT),intent(in)  :: F0(2)

    real(kind=KTGT) :: B0(2), B1(2)
    integer jo, mo
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT

    B1(1:2) = ZERO
    B0(2) = ZERO
    if (mod(modr, 2).eq.0) then
       B0(1) = C(modr)
       mo = modr - 1
    else
       B0(1) = ZERO
       mo = modr
    endif

    do jo = mo, 1, -2
       B1(1:2) = (/C(jo),   ZERO/) + ((AA(1:2) * B0(1) + AA(2:1:-1) * B0(2)) - B1(1:2))
       B0(1:2) = (/C(jo-1), ZERO/) + ((AA(1:2) * B1(1) + AA(2:1:-1) * B1(2)) - B0(1:2))
    enddo
    B0(1:2) = B0(1:2) - B1(1:2)
    z = B0(1) * F0(1) + B0(2) * F0(2)
    z = - z * TWO
  end subroutine comp_diff_ainteg_d
!!!_  & set_dlongi()
  function set_dlongi_d &
       & (lon0, lon1) &
       & result(sca)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: lon0(NTRIG), lon1(NTRIG)
    real(kind=KTGT)            :: sca(3)

    _TRIG(sca) = sub_angle(lon1, lon0)
    ! _SIN(sca) = _SIN(lon1) * _COS(lon0) -  _COS(lon1) * _SIN(lon0)
    ! _COS(sca) = _COS(lon1) * _COS(lon0) +  _SIN(lon1) * _SIN(lon0)
    _ANGLE(sca) = phase(sca)

    return
  end function set_dlongi_d
!!!_  & comp_series_sine
  function comp_series_sine_d &
       & (CT, no, ss, cs) result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: r
    real(kind=KTGT),intent(in)  :: CT(0:*)
    integer,        intent(in)  :: no          ! limit order
    real(kind=KTGT),intent(in)  :: ss, cs      ! sine cosine

    integer jo, mo
    real(kind=KTGT) :: c2s, s2s
    real(kind=KTGT) :: b1, b2

    c2s = (cs - ss) * (cs + ss)
    s2s = (ss * cs) * 2.0_KTGT

    ! b[j]   = a b[j+1] - b[j+2] + c :  b2 = a b1 - b2 + c
    ! b[j-1] = a b[j]   - b[j+1] + c :  b1 = a b2 - b1 + c

    b2 = 0.0_KTGT
    if (mod(no, 2).eq.1) then
       b1 = CT(no)
       mo = no - 1
    else
       b1 = 0.0_KTGT
       mo = no
    endif
    do jo = mo, 1, -2
       b2 = (2.0_KTGT * c2s) * b1 - b2 + CT(jo)
       b1 = (2.0_KTGT * c2s) * b2 - b1 + CT(jo-1)
    enddo

    r  = b1 * s2s
  end function comp_series_sine_d
!!!_  & gen_vtable_elongi - (variable) coefficient table for ellipsoidal longitude (I3)
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
!!!_  & gen_ctable_elongi - (constant) coefficient table for ellipsoidal longitude (I3)
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
!!!_  & gen_vtable_earea - (variable) coefficient table for geodesic-equator area (I4)
  subroutine gen_vtable_earea_d &
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
  end subroutine gen_vtable_earea_d
!!!_  & gen_ctable_earea - (constant) coefficient table for geodesic-equator area (I4)
  subroutine gen_ctable_earea_d &
       & (ierr, C4b, no, fiii)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: C4b(0:, 0:)
    integer,        intent(in)  :: no            ! limit order
    real(kind=KTGT),intent(in)  :: fiii          ! third flattening (n)
    ierr = 0
    if (no.eq.9) then
#      include "ggf/coeffs_i4b-o9.F90"
    else if (no.eq.7) then
#      include "ggf/coeffs_i4b-o7.F90"
    else if (no.eq.6) then
#      include "ggf/coeffs_i4b-o6.F90"
    else if (no.eq.5) then
#      include "ggf/coeffs_i4b-o5.F90"
    else
       ierr = ERR_NOT_IMPLEMENTED
    endif
  end subroutine gen_ctable_earea_d
!!!_  & gen_ctable_I1
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
!!!_  & gen_ctable_I1p
  subroutine gen_ctable_I1p_d &
       & (ierr, C1p, no, eps)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: C1p(0:*)      ! 0 as dummy
    integer,        intent(in)  :: no            ! limit order
    real(kind=KTGT),intent(in)  :: eps
    real(kind=KTGT)  :: epsc, eps2
    ierr = 0
    eps2 = eps ** 2
    if (no.eq.9) then
#      include "ggf/coeffs_i1p-o9.F90"
    else if (no.eq.7) then
#      include "ggf/coeffs_i1p-o7.F90"
    else if (no.eq.6) then
#      include "ggf/coeffs_i1p-o6.F90"
    else if (no.eq.5) then
#      include "ggf/coeffs_i1p-o5.F90"
    else
       ierr = ERR_NOT_IMPLEMENTED
    endif
    C1p(0) = 0.0_KTGT
  end subroutine gen_ctable_I1p_d
!!!_  & gen_ctable_I2b
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
!!!_  & gen_ctable_I2a
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

!!!_ + symmetric tripolar coordinate
!!!_  & stp_set
  subroutine stp_set_d &
       & (ierr, csco, latp, lonp, pole, loround, laround)
    ! [caution]
    ! xs ys != 1 are special configuration for amida,
    ! which are not fully tested on consistency.
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(out)         :: csco(*)
    real(kind=KTGT),intent(in)          :: latp, lonp
    integer,        intent(in)          :: pole    ! either +1 or -1, target sphere
    real(kind=KTGT),intent(in),optional :: loround
    real(kind=KTGT),intent(in),optional :: laround

    real(kind=KTGT),parameter :: ONE=1.0_KTGT, ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: TWO=2.0_KTGT
    real(kind=KTGT) :: loro, laro

    real(kind=KTGT) :: ca(NATRI), plo(NTRIG), pla(NTRIG)

    ierr = 0

    loro = round_choice(loround, ZERO)
    laro = round_choice(laround, ZERO)

    if (abs(pole).ne.1) then
       ierr = _ERROR(ERR_INVALID_PARAMETER)
    else
       csco(icache_stp_solat) = - real(pole, kind=KTGT)  ! negate
       csco(icache_stp_olat) = - deg2ang(90.0_KTGT, laro) * real(pole, kind=KTGT)
    endif

    if (ierr.eq.0) then
       csco(icache_stp_loround) = loro
       csco(icache_stp_laround) = laro
       csco(icache_stp_plon) = lonp
       csco(icache_stp_plat) = latp

       csco(icache_stp_splat) = sin_canonical(latp, laro)
    endif
    if (ierr.eq.0) then

       plo(:) = set_sincos(ZERO, loro)
       pla(:) = set_sincos(latp, laro)

       ca = stp_geogr_zproj(plo, pla, csco)

       csco(icache_stp_wnpr) = ca(JAMP) * ca(JCOS)
    endif

  end subroutine stp_set_d

!!!_  & stp_fwd() - transform from geographic to w-sphere coordinate
  PURE &
  function stp_fwd_tr_d &
       & (lon, lat, csco) &
       & result (wg)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: lon(NTRIG)   ! geological coordinates
    real(kind=KTGT),intent(in) :: lat(NTRIG)
    real(kind=KTGT),intent(in) :: csco(*)
    real(kind=KTGT) :: wg(NGEOG)

    real(kind=KTGT) :: zacs(NATRI), wacs(NATRI)
    real(kind=KTGT) :: wlo(NTRIG), wla(NTRIG)

    real(kind=KTGT) :: loro, laro

    zacs = stp_geogr_zproj(lon, lat, csco)
    wacs = stp_z2wproj_atr(zacs, csco)
    wlo = stp_wsphere_lon_tr(wacs)
    wla = stp_wsphere_lat_tr(wacs)
    wla(JSIN) = wla(JSIN) * SIGN(1.0_KTGT, - lon(JCOS))
    if (lon(JCOS).eq.0.0_KTGT) wla(JSIN) = 0.0_KTGT
    wlo(JSIN) = wlo(JSIN) * SIGN(1.0_KTGT, lon(JSIN))

    loro = csco(icache_stp_loround)
    laro = csco(icache_stp_laround)
    wg(JLONGI) = rad2ang(phase(wlo), loro)
    wg(JLATI) = rad2ang(phase(wla), laro)
  end function stp_fwd_tr_d

!!!_  & stp_bwd() - transform from w-sphere to geographic coordinate
  ! PURE &
  function stp_bwd_tr_d &
       & (lon, lat, csco) &
       & result (zg)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: lon(NTRIG)   ! geological coordinates
    real(kind=KTGT),intent(in) :: lat(NTRIG)
    real(kind=KTGT),intent(in) :: csco(*)
    real(kind=KTGT) :: zg(NGEOG)

    real(kind=KTGT) :: zacs(NATRI), wacs(NATRI)
    real(kind=KTGT) :: zlo(NTRIG), zla(NTRIG)

    real(kind=KTGT) :: loro, laro

    wacs = stp_wsphere_wproj_tr(lon, lat, csco)
    zacs = stp_w2zproj_atr(wacs, csco)

    if (zacs(JAMP).eq.0.0_KTGT) then
       zlo(JCOS) = 1.0_KTGT
       zlo(JSIN) = 0.0_KTGT
       zla(JCOS) = 0.0_KTGT
       zla(JSIN) = - csco(icache_stp_solat)
    else
       zlo = stp_geogr_lon_tr(zacs)
       zla = stp_geogr_lat_tr(zacs)
       zlo(JCOS) = zlo(JCOS) * SIGN(1.0_KTGT, -lat(JSIN))
       zlo(JSIN) = zlo(JSIN) * SIGN(1.0_KTGT, +lon(JSIN))
       zla(JSIN) = zla(JSIN) * (- csco(icache_stp_solat))
    endif
    loro = csco(icache_stp_loround)
    laro = csco(icache_stp_laround)
    zg(JLONGI) = rad2ang(phase(zlo), loro)
    zg(JLATI) = rad2ang(phase(zla), laro)
  end function stp_bwd_tr_d

!!!_  & is_stp_wsphere() - check if on the w-sphere
  PURE &
  logical function is_stp_wsphere_tr_d &
       & (lat, csco) &
       & result(b)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: lat(NTRIG)  ! geological latitude
    real(kind=KTGT),intent(in) :: csco(*)

    b = (csco(icache_stp_solat).le.lat(JSIN) .and. lat(JSIN).lt.csco(icache_stp_splat)) &
         & .or. &
         & (csco(icache_stp_solat).ge.lat(JSIN) .and. lat(JSIN).gt.csco(icache_stp_splat))
    b = .not. b
  end function is_stp_wsphere_tr_d

!!!_  & stp_geogr_zproj() - transform from geographic to z-plane projection
  PURE &
  function stp_geogr_zproj_tr_d &
       & (lon, lat, csco) &
       & result (zacs)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: lon(NTRIG)   ! geological coordinates
    real(kind=KTGT),intent(in) :: lat(NTRIG)
    real(kind=KTGT),intent(in) :: csco(*)
    real(kind=KTGT) :: zacs(NATRI)

    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: ZERO = 1.0_KTGT
    zacs(JCOS) = ABS(lon(JCOS))
    zacs(JSIN) = ABS(lon(JSIN))
    zacs(JAMP) = lat(JCOS) / (ONE - lat(JSIN) * csco(icache_stp_solat))
  end function stp_geogr_zproj_tr_d

!!!_  & stp_wsphere_wproj() - transform from w-sphere to w-plane projection
  PURE &
  function stp_wsphere_wproj_tr_d &
       & (lon, lat, csco) &
       & result (wacs)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: lon(NTRIG)   ! geological coordinates
    real(kind=KTGT),intent(in) :: lat(NTRIG)
    real(kind=KTGT),intent(in) :: csco(*)
    real(kind=KTGT) :: wacs(NATRI)

    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: ZERO = 1.0_KTGT
    wacs(JCOS) = lon(JCOS)
    wacs(JSIN) = ABS(lon(JSIN))
    wacs(JAMP) = lat(JCOS) / (ONE + ABS(lat(JSIN)))
  end function stp_wsphere_wproj_tr_d

!!!_  & stp_z2wproj() - transform from z-plane to w-plane projection
  PURE &
  function stp_z2wproj_atr_d &
       & (zacs, csco) &
       & result (wacs)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: zacs(NATRI)
    real(kind=KTGT),intent(in) :: csco(*)
    real(kind=KTGT) :: wacs(NATRI)

    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: ZERO = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT

    real(kind=KTGT) :: wp
    real(kind=KTGT) :: wd, wr, wi, wh

    ! zacs(JCOS) = lon(JCOS)
    ! zacs(JSIN) = lon(JSIN)
    ! zacs(JAMP) = lat(JCOS) / (ONE + lat(JSIN))

    wp = csco(icache_stp_wnpr)

    wd = - ((zacs(JAMP) ** 2 + wp ** 2) &
         &  + (TWO * wp * zacs(JAMP) * zacs(JCOS)))

    wr = (zacs(JAMP) + wp) * (zacs(JAMP) - wp)
    wi = TWO * wp * zacs(JAMP) * zacs(JSIN)
    wh = HYPOT(wr, wi)

    wacs(JCOS) = wr / wp * SIGN(ONE, wd)
    wacs(JSIN) = wi / wp * SIGN(ONE, wd)
    wacs(JAMP) = ABS(wh / wd)

  end function stp_z2wproj_atr_d

!!!_  & stp_w2zproj() - transform from w-plane to z-plane projection
  PURE &
  function stp_w2zproj_atr_d &
       & (wacs, csco) &
       & result (zacs)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: wacs(NATRI)
    real(kind=KTGT),intent(in) :: csco(*)
    real(kind=KTGT) :: zacs(NATRI)

    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: ZERO = 1.0_KTGT
    real(kind=KTGT),parameter :: TWO = 2.0_KTGT

    real(kind=KTGT) :: wp
    real(kind=KTGT) :: wd, wr, wi, wh

    ! wacs(JCOS) = lon(JCOS)
    ! wacs(JSIN) = lon(JSIN)
    ! wacs(JAMP) = lat(JCOS) / (ONE + lat(JSIN))

    wp = csco(icache_stp_wnpr)

    wd = - ((wacs(JAMP) ** 2 + ONE) &
         &  + (TWO * wacs(JAMP) * wacs(JCOS)))

    wr = (wacs(JAMP) + ONE) * (wacs(JAMP) - ONE)
    wi = TWO * wacs(JAMP) * wacs(JSIN)
    wh = HYPOT(wr, wi)

    zacs(JCOS) = wr * SIGN(ONE, wd)
    zacs(JSIN) = wi * SIGN(ONE, wd)
    zacs(JAMP) = ABS(wh * wp / wd)

  end function stp_w2zproj_atr_d

!!!_  & stp_wsphere_lat() - transform w-plane projection to latitude (on w sphere)
  PURE &
  function stp_wsphere_lat_tr_d &
       & (wacs) &
       & result (lat)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: wacs(NATRI)
    real(kind=KTGT) :: lat(NTRIG)

    real(kind=KTGT),parameter :: TWO = 2.0_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT

    real(kind=KTGT) :: s, c

    c = TWO * wacs(JAMP)
    s = (ONE + wacs(JAMP)) * (ONE - wacs(JAMP))

    lat(1:NTRIG) = nml_sincos(s, c)
  end function stp_wsphere_lat_tr_d

!!!_  & stp_wsphere_lon() - transform w-plane projection to longitude (on w sphere)
  PURE &
  function stp_wsphere_lon_tr_d &
       & (wacs) &
       & result (lon)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: wacs(NATRI)
    real(kind=KTGT) :: lon(NTRIG)

    real(kind=KTGT) :: s, c

    s = wacs(JSIN)
    c = wacs(JCOS)

    lon(1:NTRIG) = nml_sincos(s, c)
  end function stp_wsphere_lon_tr_d

!!!_  & stp_geogr_lat() - transform z-plane projection to latitude
  PURE &
  function stp_geogr_lat_tr_d &
       & (zacs) &
       & result (lat)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: zacs(NATRI)
    real(kind=KTGT) :: lat(NTRIG)

    real(kind=KTGT),parameter :: TWO = 2.0_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT

    real(kind=KTGT) :: s, c

    c = TWO * zacs(JAMP)
    s = (ONE + zacs(JAMP)) * (ONE - zacs(JAMP))

    lat(1:NTRIG) = nml_sincos(s, c)
  end function stp_geogr_lat_tr_d

!!!_  & stp_geogr_lon() - transform z-plane projection to longitude
  PURE &
  function stp_geogr_lon_tr_d &
       & (zacs) &
       & result (lon)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: zacs(NATRI)
    real(kind=KTGT) :: lon(NTRIG)

    real(kind=KTGT) :: s, c

    s = zacs(JSIN)
    c = zacs(JCOS)

    lon(1:NTRIG) = nml_sincos(s, c)
  end function stp_geogr_lon_tr_d

!!!_ + trigonometric function
!!!_  & phase() - (atan2() wrapper)
  PURE &
  function phase_1_d(terms) result(v)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in) :: terms(*)
    v = ATAN2(_SIN(terms), _COS(terms))
  end function phase_1_d

  PURE &
  function phase_2_d(s, c) result(v)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in) :: s, c
    v = ATAN2(s, c)
  end function phase_2_d

#if OPT_REAL_QUADRUPLE_DIGITS > 0
  PURE &
  function phase_1_q(terms) result(v)
    use TOUZA_Std,only: KTGT=>KQPL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in) :: terms(*)
    v = ATAN2(_SIN(terms), _COS(terms))
  end function phase_1_q

  PURE &
  function phase_2_q(s, c) result(v)
    use TOUZA_Std,only: KTGT=>KQPL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in) :: s, c
    v = ATAN2(s, c)
  end function phase_2_q
#endif

!!!_  & phased - (atan2() wrapper) in degree
  PURE &
  function phased_1_d(terms) result(v)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in) :: terms(*)
    v = rad2deg(phase(terms))
  end function phased_1_d

  PURE &
  function phased_2_d(s, c) result(v)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in) :: s, c
    v = rad2deg(phase(s, c))
  end function phased_2_d

#if OPT_REAL_QUADRUPLE_DIGITS > 0
  PURE &
  function phased_1_q(terms) result(v)
    use TOUZA_Std,only: KTGT=>KQPL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in) :: terms(*)
    v = rad2deg(phase(terms))
  end function phased_1_q

  PURE &
  function phased_2_q(s, c) result(v)
    use TOUZA_Std,only: KTGT=>KQPL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in) :: s, c
    v = rad2deg(phase(s, c))
  end function phased_2_q
#endif

!!!_  & radian_modulo()
  ELEMENTAL &
  function radian_modulo_d (angl) result(v)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in) :: angl
    real(kind=KTGT) c
    c = pi_(angl) * 2.0_KTGT
    v = modulo(angl, c)
  end function radian_modulo_d

!!!_  & degree_modulo()
  ELEMENTAL &
  function degree_modulo_d (angl) result(v)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in) :: angl
    real(kind=KTGT),parameter :: c = 360.0_KTGT
    v = modulo(angl, c)
  end function degree_modulo_d

!!!_  & sets_sincos() - set sine and cosine array (with round angle)
  PURE &
  function sets_sincos_d(angl, round) &
       & result(sc)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: angl
    real(kind=KTGT),intent(in) :: round
    real(kind=KTGT) :: sc(NTRIG)
    real(kind=KTGT) r, a, s, c, q
    integer n

    if (round.eq.0.0_KTGT) then
       sc(1:NTRIG) = setr_sincos(angl)
    else
       q = round / 4.0_KTGT
       a = ANINT(angl / q)
       r = angl - q * a
       n = modulo(int(a), 4)
       if (ABS(r).eq.(round / 8.0_KTGT)) then
          c = SQRT(2.0_KTGT) / 2.0_KTGT
          s = sign(c, r)
       else
          r = pi_(angl) * (r / (round / 2.0_KTGT))
          s = sin(r)
          c = cos(r)
       endif
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
    endif
  end function sets_sincos_d
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  PURE &
  function sets_sincos_q(angl, round) &
       & result(sc)
    use TOUZA_Std,only: KTGT=>KQPL
    implicit none
    real(kind=KTGT),intent(in) :: angl
    real(kind=KTGT),intent(in) :: round
    real(kind=KTGT) :: sc(NTRIG)
    real(kind=KTGT) r, a, s, c, q
    integer n

    if (round.eq.0.0_KTGT) then
       sc(1:NTRIG) = setr_sincos(angl)
    else
       q = round / 4.0_KTGT
       a = ANINT(angl / q)
       r = angl - q * a
       n = modulo(int(a), 4)
       if (ABS(r).eq.(round / 8.0_KTGT)) then
          c = SQRT(2.0_KTGT) / 2.0_KTGT
          s = sign(c, r)
       else
          r = pi_(angl) * (r / (round / 2.0_KTGT))
          s = sin(r)
          c = cos(r)
       endif
       ! write(*, *) 'setd: ', r, deg2rad(r), n, s, c
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
    endif
  end function sets_sincos_q
#endif
!!!_  & setr_sincos() - set sine and cosine array (radian)
  PURE &
  function setr_sincos_d(angl) &
       & result(sc)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: angl
    real(kind=KTGT) :: sc(NTRIG)
    real(kind=KTGT) r, a, q, s, c
    integer n

    q = pi_(angl) / 2.0_KTGT
    a = ANINT(angl / q)
    r = angl - q * a
    n = modulo(int(a), 4)
    if (ABS(r).eq.(q / 2.0_KTGT)) then
       c = SQRT(2.0_KTGT) / 2.0_KTGT
       s = sign(c, r)
    else
       s = sin(r)
       c = cos(r)
    endif
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
  end function setr_sincos_d
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  PURE &
  function setr_sincos_q(angl) &
       & result(sc)
    use TOUZA_Std,only: KTGT=>KQPL
    implicit none
    real(kind=KTGT),intent(in) :: angl
    real(kind=KTGT) :: sc(NTRIG)
    real(kind=KTGT) r, a, q, s, c
    integer n

    q = pi_(angl) / 2.0_KTGT
    a = ANINT(angl / q)
    r = angl - q * a
    n = modulo(int(a), 4)
    if (ABS(r).eq.(q / 2.0_KTGT)) then
       c = SQRT(2.0_KTGT) / 2.0_KTGT
       s = sign(c, r)
    else
       s = sin(r)
       c = cos(r)
    endif
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
  end function setr_sincos_q
#endif
!!!_  & setd_sincos() - set sine and cosine array (degree)
  PURE &
  function setd_sincos_d(angl) &
       & result(sc)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: angl
    real(kind=KTGT) :: sc(NTRIG)
    real(kind=KTGT),parameter :: c = 360.0_KTGT
    SC(1:NTRIG) = sets_sincos(angl, c)
  end function setd_sincos_d
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  PURE &
  function setd_sincos_q(angl) &
       & result(sc)
    use TOUZA_Std,only: KTGT=>KQPL
    implicit none
    real(kind=KTGT),intent(in) :: angl
    real(kind=KTGT) :: sc(NTRIG)
    real(kind=KTGT),parameter :: c = 360.0_KTGT
    SC(1:NTRIG) = sets_sincos(angl, c)
  end function setd_sincos_q
#endif

!!!_  & sins_canonical() - canonical sine (with round angle)
  ELEMENTAL &
  function sins_canonical_d (angl, round) result(v)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in) :: angl
    real(kind=KTGT),intent(in) :: round

    real(kind=KTGT) r, a, q
    integer n

    if (round.eq.0.0_KTGT) then
       v = sinr_canonical(angl)
    else
       q = round / 4.0_KTGT
       a = ANINT(angl / q)
       r = angl - q * a
       n = modulo(int(a), 4)
       if (ABS(r).eq.(round / 8.0_KTGT)) then
          v = SQRT(2.0_KTGT) / 2.0_KTGT
          select case(n)
          case(0)
             v = + sign(v, r)
          case(1)
             v = + v
          case(2)
             v = - sign(v, r)
          case default
             v = - v
          end select
       else
          r = pi_(angl) * (r / (round / 2.0_KTGT))
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
       endif
    endif
  end function sins_canonical_d
!!!_  & coss_canonical() - canonical cosine (with round angle)
  ELEMENTAL &
  function coss_canonical_d (angl, round) result(v)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in) :: angl
    real(kind=KTGT),intent(in) :: round
    real(kind=KTGT) r, a, q
    integer n

    if (round.eq.0.0_KTGT) then
       v = cosr_canonical(angl)
    else
       q = round / 4.0_KTGT
       a = ANINT(angl / q)
       r = angl - q * a
       n = modulo(int(a), 4)
       if (ABS(r).eq.(round / 8.0_KTGT)) then
          v = SQRT(2.0_KTGT) / 2.0_KTGT
          select case(n)
          case(0)
             v = + v
          case(1)
             v = - sign(v, r)
          case(2)
             v = - v
          case default
             v = + sign(v, r)
          end select
       else
          r = pi_(angl) * (r / (round / 2.0_KTGT))
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
       endif
    endif
  end function coss_canonical_d
!!!_  & sinr_canonical() - canonical sine (radian)
  ELEMENTAL &
  function sinr_canonical_d (angl) result(v)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in) :: angl

    real(kind=KTGT) r, a, q
    integer n

    q = pi_(angl) / 2.0_KTGT
    a = ANINT(angl / q)
    n = modulo(int(a), 4)
    r = angl - q * a
    if (ABS(r).eq.(q / 2.0_KTGT)) then
       v = SQRT(2.0_KTGT) / 2.0_KTGT
       select case(n)
       case(0)
          v = + sign(v, r)
       case(1)
          v = + v
       case(2)
          v = - sign(v, r)
       case default
          v = - v
       end select
    else
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
    endif
  end function sinr_canonical_d
!!!_  & cosr_canonical() - canonical cosine (radian)
  ELEMENTAL &
  function cosr_canonical_d (angl) result(v)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in) :: angl

    real(kind=KTGT) r, a, q
    integer n

    q = pi_(angl) / 2.0_KTGT
    a = ANINT(angl / q)
    n = modulo(int(a), 4)
    r = angl - q * a
    if (ABS(r).eq.(q / 2.0_KTGT)) then
       v = SQRT(2.0_KTGT) / 2.0_KTGT
       select case(n)
       case(0)
          v = + v
       case(1)
          v = - sign(v, r)
       case(2)
          v = - v
       case default
          v = + sign(v, r)
       end select
    else
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
    endif
  end function cosr_canonical_d
!!!_  & sind_canonical() - canonical sine (degree)
  ELEMENTAL &
  function sind_canonical_d (angl) result(v)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in) :: angl
    real(kind=KTGT),parameter :: c = 360.0_KTGT
    v = sins_canonical(angl, c)
  end function sind_canonical_d
!!!_  & cosd_canonical() - canonical sine (degree)
  ELEMENTAL &
  function cosd_canonical_d (angl) result(v)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: v
    real(kind=KTGT),intent(in) :: angl
    real(kind=KTGT),parameter :: c = 360.0_KTGT
    v = coss_canonical(angl, c)
  end function cosd_canonical_d

!!!_  & nml_sincos() - normalize to compute sine and cosine
  PURE &
  function nml_sincos_d(py, px) &
       & result(sc)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: sc(NTRIG)
    real(kind=KTGT),intent(in) :: py, px
    real(kind=KTGT) :: d

    d = _hypot(px, py)

    _SIN(sc) = py / d
    _COS(sc) = px / d
  end function nml_sincos_d
!!!_  & add_angle
  PURE &
  function add_angle_d &
       & (scx, scy) &
       & result(scz)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: scz(NTRIG)
    real(kind=KTGT),intent(in) :: scx(NTRIG)
    real(kind=KTGT),intent(in) :: scy(NTRIG)
    ! sin(x+y) = sin(x)cos(y) + cos(x)sin(y)
    ! cos(x+y) = cos(x)cos(y) - sin(x)sin(y)
    _SIN(scz) = _SIN(scx)*_COS(scy) + _COS(scx)*_SIN(scy)
    _COS(scz) = _COS(scx)*_COS(scy) - _SIN(scx)*_SIN(scy)
  end function add_angle_d

#if OPT_REAL_QUADRUPLE_DIGITS > 0
  PURE &
  function add_angle_q &
       & (scx, scy) &
       & result(scz)
    use TOUZA_Std,only: KTGT=>KQPL
    implicit none
    real(kind=KTGT),intent(in) :: scx(NTRIG)
    real(kind=KTGT),intent(in) :: scy(NTRIG)
    real(kind=KTGT) :: scz(NTRIG)
    ! sin(x+y) = sin(x)cos(y) + cos(x)sin(y)
    ! cos(x+y) = cos(x)cos(y) - sin(x)sin(y)
    _SIN(scz) = _SIN(scx)*_COS(scy) + _COS(scx)*_SIN(scy)
    _COS(scz) = _COS(scx)*_COS(scy) - _SIN(scx)*_SIN(scy)
  end function add_angle_q
#endif
!!!_  & sub_sine_d
  PURE &
  function sub_sin_d(scx, scy) result(s)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: s
    real(kind=KTGT),intent(in) :: scx(NTRIG)
    real(kind=KTGT),intent(in) :: scy(NTRIG)
    ! sin(x-y) = sin(x)cos(y) - cos(x)sin(y)
    s = _SIN(scx)*_COS(scy) - _COS(scx)*_SIN(scy)
  end function sub_sin_d

!!!_  & sub_angle
  function sub_angle_d &
       & (scx, scy) &
       & result(scz)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: scx(NTRIG)
    real(kind=KTGT),intent(in) :: scy(NTRIG)
    real(kind=KTGT) :: scz(NTRIG)
    ! sin(x-y) = sin(x)cos(y) - cos(x)sin(y)
    ! cos(x-y) = cos(x)cos(y) + sin(x)sin(y)
    _SIN(scz) = _SIN(scx)*_COS(scy) - _COS(scx)*_SIN(scy)
    _COS(scz) = _COS(scx)*_COS(scy) + _SIN(scx)*_SIN(scy)
  end function sub_angle_d
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  function sub_angle_q &
       & (scx, scy) &
       & result(scz)
    use TOUZA_Std,only: KTGT=>KQPL
    implicit none
    real(kind=KTGT),intent(in) :: scx(NTRIG)
    real(kind=KTGT),intent(in) :: scy(NTRIG)
    real(kind=KTGT) :: scz(NTRIG)
    ! sin(x-y) = sin(x)cos(y) - cos(x)sin(y)
    ! cos(x-y) = cos(x)cos(y) + sin(x)sin(y)
    _SIN(scz) = _SIN(scx)*_COS(scy) - _COS(scx)*_SIN(scy)
    _COS(scz) = _COS(scx)*_COS(scy) + _SIN(scx)*_SIN(scy)
  end function sub_angle_q
#endif

!!!_  & hpsub_sin - high precision subtraction
  PURE &
  function hpsub_sin_d(scx, scy) result(s)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: s
    real(kind=KTGT),intent(in) :: scx(NTRIG)
    real(kind=KTGT),intent(in) :: scy(NTRIG)

    real(kind=KTGT) :: t1(2), t2(2)

    t1(1:2) = (/+ _SIN(scx), - _COS(scx)/)
    t2(1:2) = (/+ _COS(scy), + _SIN(scy)/)

    s = hpInProduct(t1(1:2), t2(1:2))
  end function hpsub_sin_d

!!!_  & hpsub_angle - high precision subtraction
  function hpsub_angle_d &
       & (scx, scy) &
       & result(scz)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: scx(NTRIG)
    real(kind=KTGT),intent(in) :: scy(NTRIG)
    real(kind=KTGT) :: scz(NTRIG)

    real(kind=KTGT) :: t1(2), t2(2)

    t1(1:2) = (/+ _SIN(scx), - _COS(scx)/)
    t2(1:2) = (/+ _COS(scy), + _SIN(scy)/)

    _SIN(scz) = hpInProduct(t1(1:2), t2(1:2))

    t1(1:2) = (/+ _COS(scx), + _SIN(scx)/)

    _COS(scz) = hpInProduct(t1(1:2), t2(1:2))
    ! sin(x-y) = sin(x)cos(y) - cos(x)sin(y)
    ! cos(x-y) = cos(x)cos(y) + sin(x)sin(y)
  end function hpsub_angle_d

!!!_  & hpadd_angle - high precision addition
  function hpadd_angle_d &
       & (scx, scy) &
       & result(scz)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: scx(NTRIG)
    real(kind=KTGT),intent(in) :: scy(NTRIG)
    real(kind=KTGT) :: scz(NTRIG)

    real(kind=KTGT) :: t1(2), t2(2)

    t1(1:2) = (/+ _SIN(scx), + _COS(scx)/)
    t2(1:2) = (/+ _COS(scy), + _SIN(scy)/)

    _SIN(scz) = hpInProduct(t1(1:2), t2(1:2))

    t1(1:2) = (/+ _COS(scx), - _SIN(scx)/)

    _COS(scz) = hpInProduct(t1(1:2), t2(1:2))
    ! sin(x+y) = sin(x)cos(y) + cos(x)sin(y)
    ! cos(x+y) = cos(x)cos(y) - sin(x)sin(y)
  end function hpadd_angle_d

!!!_  & hpInProduct - high precision inner product
  PURE &
  function hpInProduct_d(x, y) result (r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: r
    real(kind=KTGT),intent(in) :: x(:), y(:)
    real(kind=KTGT) :: p(2), h(2), s
    integer j
    p(:) = hpMult(x(1), y(1))
    ! write(*, *) p(:)
    s = p(2)
    do j = 2, size(x)
       h(:) = hpMult(x(j), y(j))
       ! write(*,*) h(:), s
       p(:) = hpAdd(p(1), h(1))
       s = (s + (h(2) + p(2)))
    enddo
    ! write(*,*) p(1), s
    r = p(1) + s
  end function hpInProduct_d

!!!_  - hpAdd
  PURE &
  function hpAdd_d(a, b) result (hl)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: hl(2)
    real(kind=KTGT),intent(in):: a, b

    real(kind=KTGT) :: z
    hl(1) = a + b
    z = hl(1) - a
    hl(2) = (a - (hl(1) - z)) + (b - z)
  end function hpAdd_d

!!!_  - hpMult
  PURE &
  function hpMult_d(a, b) result (hl)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: hl(2)
    real(kind=KTGT),intent(in):: a, b

    real(kind=KTGT) :: aa(2), bb(2)
    hl(1) = a * b
    aa(:) = hpSplit(a)
    bb(:) = hpSplit(b)
    hl(2) = aa(2) * bb(2) &
         & - (((hl(1) - aa(1) * bb(1)) &
         &      - aa(2) * bb(1)) &
         &    - aa(1) * bb(2))
  end function hpMult_d

!!!_  - hpSplit
  PURE &
  function hpSplit_d(a) result (hl)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: hl(2)
    real(kind=KTGT),intent(in):: a

    real(kind=KTGT) :: c
    integer,parameter :: ss = DIGITS(0.0_KTGT)
    integer,parameter :: s = ss / 2 + mod(ss, 2)

    real(kind=KTGT),parameter :: f = 2.0_KTGT ** s + 1.0_KTGT

    c = f * a
    hl(1) = c - (c - a)
    hl(2) = a - hl(1)

  end function hpSplit_d

!!!_ + geodesic-meridian-parallel segment area
!!!_  - agmpe_table_size - design [e]
!!!_  - agmpe_gen_table
  subroutine agmpe_gen_table_d &
       & (ierr, C, no, dlh, tdhsq, rel)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: C(0:*)
    integer,        intent(in)  :: no         ! order
    real(kind=KTGT),intent(in)  :: dlh        ! (dlon/2)
    real(kind=KTGT),intent(in)  :: tdhsq      ! [tan(dlat/2)]^2
    logical,        intent(in)  :: rel        ! set true to subtract pmem area

    real(kind=KTGT) :: Ce(0:no * (no + 3))
    real(kind=KTGT) :: dlhf, dlhsq

    ierr = 0
    dlhsq = dlh ** 2

    if (no.eq.2) then
#      include "agmp/agmp-e2.F90"
    else if (no.eq.3) then
#      include "agmp/agmp-e3.F90"
    else if (no.eq.4) then
#      include "agmp/agmp-e4.F90"
    else if (no.eq.5) then
#      include "agmp/agmp-e5.F90"
    else
       ierr = ERR_NOT_IMPLEMENTED
    endif
  end subroutine agmpe_gen_table_d
!!!_  - agmpe_area_core
  function agmpe_area_core_d &
       & (slat, clat, tdlath, &
       &  C,    no) &
       &  result(a)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: a
    real(kind=KTGT),intent(in)  :: slat       ! sine latitude
    real(kind=KTGT),intent(in)  :: clat       ! cosine latitude
    real(kind=KTGT),intent(in)  :: tdlath     ! tangent[(d latitude)/2]
    real(kind=KTGT),intent(in)  :: C(0:*)
    integer,        intent(in)  :: no         ! order

    real(kind=KTGT),parameter  :: ZERO=0.0_KTGT
    real(kind=KTGT) :: c2

    real(kind=KTGT) :: b2s, b1s
    real(kind=KTGT) :: b2c, b1c
    integer j

    c2  = 2.0_KTGT * clat
    b2s = ZERO
    b1s = ZERO
    b2c = ZERO
    b1c = ZERO
    do j = no - 1, 1, -1
       b2s = C(j*2)   + c2 * b1s - b2s
       b1s =            c2 * b2s - b1s
       b2c = C(j*2+1) + c2 * b1c - b2c
       b1c =            c2 * b2c - b1c
    enddo
    j = 0
    b2s = C(j*2)   + c2 * b1s - b2s
    b2c = C(j*2+1) + c2 * b1c - b2c
    ! write(*,*) 'e:sc:', b2s, b2c, b2s * slat, b2c * clat, b2c * clat * tdlath

    a = b2s * slat + b2c * clat * tdlath
  end function agmpe_area_core_d
!!!_  - agmpc_table_size - design [c]
  PURE &
  integer function agmpc_table_size(no) result(m)
    implicit none
    integer,intent(in) :: no
    if (mod(no,2).eq.0) then
       m = (no / 4 + 2) * ((no + 2) / 4) * 2
    else
       m = (no / 2 + 4) * (no / 2 + 1) / 2
    endif
  end function agmpc_table_size
!!!_  - agmpc_gen_table
  subroutine agmpc_gen_table_d &
       & (ierr, Cc, no, s, rel)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: Cc(0:*)
    integer,        intent(in)  :: no         ! order
    real(kind=KTGT),intent(in)  :: s          ! sine latitude
    logical,        intent(in)  :: rel        ! set true to subtract pmem area
    real(kind=KTGT)  :: s2
    ierr = 0
    s2 = s ** 2
    if (no.eq.4) then
#      include "agmp/agmp-c4.F90"
    else if (no.eq.5) then
#      include "agmp/agmp-c5.F90"
    else if (no.eq.6) then
#      include "agmp/agmp-c6.F90"
    else if (no.eq.7) then
#      include "agmp/agmp-c7.F90"
    else if (no.eq.8) then
#      include "agmp/agmp-c8.F90"
    else if (no.eq.9) then
#      include "agmp/agmp-c9.F90"
    else if (no.eq.10) then
#      include "agmp/agmp-c10.F90"
    else if (no.eq.11) then
#      include "agmp/agmp-c11.F90"
    else if (no.eq.12) then
#      include "agmp/agmp-c12.F90"
    else
       ierr = ERR_NOT_IMPLEMENTED
    endif
  end subroutine agmpc_gen_table_d

!!!_  - agmpc_area_core
  function agmpc_area_core_d &
       & (clat, tdlath, dlonh, &
       &  Cc,   no) &
       &  result(a)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: a
    real(kind=KTGT),intent(in)  :: clat       ! cosine latitude
    real(kind=KTGT),intent(in)  :: tdlath     ! tangent[(d latitude)/2]
    real(kind=KTGT),intent(in)  :: dlonh      ! (d longitude)/2
    real(kind=KTGT),intent(in)  :: Cc(0:*)
    integer,        intent(in)  :: no         ! order

    integer jp, mp
    integer jcb
    integer jt, jtb, jte
    real(kind=KTGT) :: buf(0:no)
    real(kind=KTGT) :: dlhs, dlht, ct

    jcb = 0

    dlht = dlonh
    dlhs = dlonh ** 2
    if (mod(no, 2).eq.0) then
       mp = ((no + 2) / 4) * 2 - 1
    else
       mp = (no / 2)
    endif
    buf(mp) = 0.0_KTGT
    ! 4  2      ((no + 2) / 4) * 2
    ! 6  4
    ! 8  4
    ! 10  6
    ! 12  6

    ! 5  3      (no / 2) + 1
    ! 7  4
    ! 9  5
    ! 11  6
    do jp = 0, mp
       jtb = jp / 2
       jte = (no - jp + 1) / 2
       ! write(*, *) 'area', no, jp, jcb, jtb, jte
       buf(jp) = Cc(jcb + 1)
       do jt = jtb + 1, jte - 1
          buf(jp) = buf(jp) * dlhs + Cc(jcb + 1 + jt - jtb)
       enddo
       buf(jp) = (buf(jp) * dlht) / Cc(jcb)
       jcb = jcb + (jte - jtb) + 1
       dlht = dlht * (dlhs ** mod(jp, 2))
    enddo
    ct = clat * tdlath
    a = buf(mp)
    do jp = mp - 1, 0, -1
       a = a * ct + buf(jp)
    enddo
    ! a = a * 2.0_KTGT
  end function agmpc_area_core_d

!!!_  - agmpd_gen_table
  subroutine agmpd_gen_table_d &
       & (ierr, Cd, no, s, rel)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: Cd(0:*)
    integer,        intent(in)  :: no         ! order
    real(kind=KTGT),intent(in)  :: s          ! sine latitude
    logical,        intent(in)  :: rel        ! set true to subtract pmem area
    real(kind=KTGT)  :: s2
    ierr = 0
    s2 = s ** 2
    if (no.eq.4) then
#      include "agmp/agmp-d4.F90"
    else if (no.eq.5) then
#      include "agmp/agmp-d5.F90"
    else if (no.eq.6) then
#      include "agmp/agmp-d6.F90"
    else if (no.eq.7) then
#      include "agmp/agmp-d7.F90"
    else if (no.eq.8) then
#      include "agmp/agmp-d8.F90"
    else if (no.eq.9) then
#      include "agmp/agmp-d9.F90"
    else if (no.eq.10) then
#      include "agmp/agmp-d10.F90"
    else
       ierr = ERR_NOT_IMPLEMENTED
    endif
  end subroutine agmpd_gen_table_d

!!!_  - agmpd_area_core
  function agmpd_area_core_d &
       & (clat, tdlath, dlonh, &
       &  Cd,   no) &
       &  result(a)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: a
    real(kind=KTGT),intent(in)  :: clat       ! cosine latitude
    real(kind=KTGT),intent(in)  :: tdlath     ! tangent[(d latitude)/2]
    real(kind=KTGT),intent(in)  :: dlonh      ! (d longitude)/2
    real(kind=KTGT),intent(in)  :: Cd(0:*)
    integer,        intent(in)  :: no         ! order

    integer jp, mp
    integer jcb
    integer jt, jte
    real(kind=KTGT) :: buf(0:no+1)
    real(kind=KTGT) :: dlhs, dlht, ct

    jcb = 0

    dlht = dlonh
    dlhs = dlonh ** 2
    mp = ((no + 1) / 2) * 2
    buf(mp) = 0.0_KTGT

    do jp = 0, mp - 1
       jte = (mp - jp + 1) / 2
       ! write(*, *) 'area:d', no, jp, jcb, jte, Cd(jcb:jcb+jte)
       buf(jp) = Cd(jcb + 1)
       do jt = 2, jte
          buf(jp) = buf(jp) * dlhs + Cd(jcb + jt)
       enddo
       buf(jp) = (buf(jp) * dlht) / Cd(jcb)
       jcb = jcb + jte + 1
       dlht = dlht * (dlhs ** mod(jp, 2))
    enddo
    ct = clat * tdlath
    a = buf(mp - 1)
    ! write(*, *) buf(0:mp - 1)
    do jp = mp - 2, 0, -1
       a = a * ct + buf(jp)
    enddo
  end function agmpd_area_core_d

!!!_ + misc procedures
!!!_  & check_symmetric - check if the array is equidistant under tolerance
  integer function check_symmetric_d &
       & (x, n, tol, e, o) result(k)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    real(kind=KTGT),intent(in)           :: x(0:*)
    integer,        intent(in)           :: n
    real(kind=KTGT),intent(in), optional :: tol   ! default: max(x) * 2 eps
    real(kind=KTGT),intent(out),optional :: e     ! error
    real(kind=KTGT),intent(in), optional :: o     ! origin

    integer jorg
    real(kind=KTGT) :: org, t, r, em
    integer j, nh, nl

    k = symm_error

    org = choice(0.0_KTGT, o)

    t = choice(-1.0_KTGT, tol)
    if (t.lt.0.0_KTGT) then
       t = max(abs(x(0)), abs(x(n-1))) * (epsilon(0.0_KTGT) * 2.0_KTGT)
    endif

    jorg = -1
    if ((x(0) - org).lt.+t) then
       do j = 1, n - 1
          if ((x(j) - org).ge.-t) then
             jorg = j
             exit
          endif
       enddo
    else if ((x(0) - org).gt.-t) then
       do j = 1, n - 1
          if ((x(j) - org).le.+t) then
             jorg = j
             exit
          endif
       enddo
    endif

    if (jorg.lt.0) then
       k = symm_asymm
       em = 0.0_KTGT
    else if (ABS(x(jorg) - org).le.t) then
       ! origin is member
       nh = n - 1 - jorg
       nl = jorg - 0
       em = 0.0_KTGT
       if (nh.eq.0.or.nl.eq.0) then
          k = symm_asymm
       else
          k = symm_even
          if (nh.eq.nl) k = k + symm_span
          do j = 1, min(nh, nl)
             r = abs(x(jorg + j) - x(jorg - j))
             em = max(em, r)
             if (r.gt.t) k = symm_asymm
          enddo
       endif
    else
       ! origin is not member
       nh = n - jorg
       nl = jorg - 0
       em = 0.0_KTGT
       k = symm_odd
       if (nh.eq.nl) k = k + symm_span
       do j = 0, min(nh, nl) - 1
          r = abs(x(jorg + j) - x(jorg - j - 1))
          em = max(em, r)
          if (r.gt.t) k = symm_asymm
       enddo
    endif
    if (present(e)) then
       e = em
    endif
  end function check_symmetric_d
!!!_  - check_monotonic
  integer function check_monotonic_d &
       & (x, n, ddev, jbgn, jend) result(k)
    use TOUZA_Std,only: KTGT=>KDBL, choice, set_if_present
    implicit none
    real(kind=KTGT),intent(in)           :: x(0:*)
    integer,        intent(in)           :: n
    real(kind=KTGT),intent(out),optional :: ddev  ! delta maximum-minimum
    integer,        intent(out),optional :: jbgn, jend

    real(kind=KTGT) :: dmax, dmin
    integer j

    if (n.le.1) then
       k = non_monotonic
       continue
    else if (x(0).lt.x(1)) then
       ! monotonic increase
       k = +1
       do j = 1, n - 1
          if (x(j-1).ge.x(j)) then
             k = non_monotonic
             exit
          endif
       enddo
    else if (x(0).gt.x(1)) then
       ! monotonic decrease
       k = -1
       do j = 1, n - 1
          if (x(j-1).le.x(j)) then
             k = non_monotonic
             exit
          endif
       enddo
    else
       k = non_monotonic
    endif
    if (k.gt.non_monotonic) then
       call set_if_present(jbgn, 0)
       call set_if_present(jend, n)
    else if (k.lt.non_monotonic) then
       call set_if_present(jbgn, n-1)
       call set_if_present(jend, -1)
    else
       call set_if_present(jbgn, 0)
       call set_if_present(jend, 0)
    endif
    if (present(ddev)) then
       if (k.eq.non_monotonic) then
          ddev = +HUGE(0.0_KTGT)
       else
          dmax = MAXVAL(x(1:n-1) - x(0:n-2))
          dmin = MINVAL(x(1:n-1) - x(0:n-2))
          ddev = dmax - dmin
          if (ddev.eq.0.0_KTGT) then
             k = k * equidistant_strict
          else
             dmax = MAXVAL(abs(x(0:n-1)))
             dmin = MINVAL(abs(x(0:n-1)), abs(x(0:n-1)).gt.0.0_KTGT)
             if (abs(ddev * (dmin/dmax)).lt.epsilon(ddev)) then
                k = k * equidistant_enough
             else
                k = k * non_equidistant
             endif
          endif
       endif
    endif
    return
  end function check_monotonic_d

!!!_  & is_equidistant - check if the array is equidistant under tolerance
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

!!!_  & area_distortion() - area distortion factor along latitude
  ELEMENTAL &
  function area_distortion_d &
       & (slat, ecc) &
       & result(r)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: r
    real(kind=KTGT),intent(in) :: slat  ! sine latitude
    real(kind=KTGT),intent(in) :: ecc

    real(kind=KTGT) :: es
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT

    es = ecc * slat

    r = ONE / ((ONE + es) * (ONE - es))**2
  end function area_distortion_d

!!!_ + private procedures
!!!_  & check_precision - epsilon check (inherit gauss())
!!!_   . NOTE
  !  Actualy the return value is NOT epsilon(), but the first
  !  value to be ONE eq ONE + E.
  !  This implementation originates from MIROC legacy.
  function check_precision_d &
       & (u, max_iter, base, levv, mold) &
       & result(E)
    use TOUZA_Std,only: KTGT=>KDBL
    use TOUZA_Std,only: choice
    use TOUZA_Std,only: msg_grp
    implicit none
    real(kind=KTGT) :: E
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
  function check_precision_f &
       & (u, max_iter, base, levv, mold) &
       & result(E)
    use TOUZA_Std,only: KTGT=>KFLT
    use TOUZA_Std,only: choice
    use TOUZA_Std,only: msg_grp
    implicit none
    real(kind=KTGT) :: E
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
!!!_   . round_choice() - return default round-angle if not present or zero
  ELEMENTAL &
  function round_choice_d(round, mold) result(x)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    real(kind=KTGT) :: x
    real(kind=KTGT),intent(in),optional :: round
    real(kind=KTGT),intent(in)          :: mold
    real(kind=KTGT),parameter :: DEF = real(round_2pi, kind=KTGT)
    x = choice(DEF, round)
    if (x.eq.DEF) x = 2.0_KTGT * pi_(mold)
  end function round_choice_d
  ELEMENTAL &
  function round_choice_id(round, mold) result(x)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    real(kind=KTGT) :: x
    integer,        intent(in) :: round
    real(kind=KTGT),intent(in) :: mold
    x = round_choice(real(round, kind=KTGT), mold)
  end function round_choice_id
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  ELEMENTAL &
  function round_choice_q(round, mold) result(x)
    use TOUZA_Std,only: KTGT=>KQPL, choice
    implicit none
    real(kind=KTGT) :: x
    real(kind=KTGT),intent(in),optional :: round
    real(kind=KTGT),intent(in)          :: mold
    real(kind=KTGT),parameter :: DEF = real(round_2pi, kind=KTGT)
    x = choice(DEF, round)
    if (x.eq.DEF) x = 2.0_KTGT * pi_(mold)
  end function round_choice_q
  ELEMENTAL &
  function round_choice_iq(round, mold) result(x)
    use TOUZA_Std,only: KTGT=>KQPL, choice
    implicit none
    real(kind=KTGT) :: x
    integer,        intent(in) :: round
    real(kind=KTGT),intent(in) :: mold
    x = round_choice(real(round, kind=KTGT), mold)
  end function round_choice_iq
#endif

!!!_   . span_longitude() - return default span if not present or zero
  ELEMENTAL &
  function span_longitude_d(round) result(x)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    real(kind=KTGT) :: x
    real(kind=KTGT),intent(in),optional :: round
    x = round_choice(round, mold=0.0_KTGT)
  end function span_longitude_d

!!!_   . span_latitude() - return default span if not present or zero
  ELEMENTAL &
  function span_latitude_d(round) result(x)
    use TOUZA_Std,only: KTGT=>KDBL, choice
    implicit none
    real(kind=KTGT) :: x
    real(kind=KTGT),intent(in),optional :: round
    x = round_choice(round, mold=0.0_KTGT) / 2.0_KTGT
  end function span_latitude_d

!!!_   . pi_()
  PURE &
  function pi_d(mold) result(x)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT) :: x
    real(kind=KTGT),intent(in) :: mold
    real(kind=KTGT),parameter  :: ONE =  real(1, kind=kind(mold))
    real(kind=KTGT),parameter  :: FOUR = real(4, kind=kind(mold))
    real(kind=KTGT),parameter  :: p = ATAN(one) * FOUR
    x = p
  end function pi_d
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  PURE &
  function pi_q(mold) result(x)
    use TOUZA_Std,only: KTGT=>KQPL
    implicit none
    real(kind=KTGT) :: x
    real(kind=KTGT),intent(in) :: mold
    real(kind=KTGT),parameter  :: ONE =  real(1, kind=kind(mold))
    real(kind=KTGT),parameter  :: FOUR = real(4, kind=kind(mold))
    real(kind=KTGT),parameter  :: p = ATAN(one) * FOUR
    x = p
  end function pi_q
#endif
!!!_  & diag_sc
  subroutine diag_sc_d(tag, sc)
    use TOUZA_Std,only: KTGT=>KDBL
    implicit none
    character(len=*),intent(in) :: tag
    real(kind=KTGT), intent(in) :: sc(*)

    real(kind=KTGT) :: arad, adeg

    arad = phase(sc)
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

    real(kind=KTGT) :: sc(NTRIG)
    real(kind=KTGT) :: arad, adeg

    _TRIG(sc) = nml_sincos(_SIN(ph), _COS(ph))
    arad = phase(sc)
    adeg = rad2deg(arad)

101 format('phase:', A, ': ', ES24.16, 1x, E10.3, ' (', E10.3, 1x, E10.3, ')')
    write(*, 101) trim(tag), adeg, arad, sc(1), sc(2)

  end subroutine diag_ph_d

!!!_ + end Emu/ugg
end module TOUZA_Emu_ugg
!!!_* non-module Procedures

!!!_@ test_emu_usi - test program
#if TEST_EMU_UGG
program test_emu_ugg
  use TOUZA_Emu_ugg
  use TOUZA_Std,only: KFLT, KDBL, KQPL
  use TOUZA_Std,only: KTGT=>KDBL
  use TOUZA_Std,only: arg_init, arg_diag, arg_finalize, parse, get_option
  use TOUZA_Std,only: condop, banner
  implicit none
  integer ierr
  integer nlat(2)
  integer nlon(2)
  integer stereo,  geod, prec, section, length, cell, dazim, agmp
  integer stp
  integer hpangle
  integer cano
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
  if (ierr.eq.0) call init(ierr, u=-1, levv=levv, stdv=+9)
  if (ierr.eq.0) call diag(ierr)

  if (ierr.eq.0) call get_option(ierr, prec,   'prec', 0)
  if (ierr.eq.0) call get_option(ierr, cano,   'cano', 0)

  if (ierr.eq.0) call get_option(ierr, nlat(:), 'lat', 0)
  if (ierr.eq.0) nlat(1) = condop((nlat(1).lt.0), 128, nlat(1))

  if (ierr.eq.0) call get_option(ierr, nlon(:), 'lon', 0)
  if (ierr.eq.0) nlon(1) = condop((nlon(1).lt.0), 128, nlon(1))

  if (ierr.eq.0) call get_option(ierr, stereo, 'stereo', 0)
  if (ierr.eq.0) call get_option(ierr, geod,   'geod', 0)
  if (ierr.eq.0) call get_option(ierr, section, 'sect', 0)
  if (ierr.eq.0) call get_option(ierr, length, 'length', 0)
  if (ierr.eq.0) call get_option(ierr, cell, 'cell', 0)
  if (ierr.eq.0) call get_option(ierr, dazim, 'dazim', 0)
  if (ierr.eq.0) call get_option(ierr, agmp, 'agmp', 0)
  if (ierr.eq.0) call get_option(ierr, hpangle, 'hpa', 0)
  if (ierr.eq.0) call get_option(ierr, stp, 'stp', 0)

  if (ierr.eq.0) then
     if (prec.gt.0) call test_ugg_prec(ierr)
  endif
  if (ierr.eq.0) then
     if (cano.gt.0) call test_ugg_cano(ierr, cano)
  endif

  if (ierr.eq.0) then
     if (nlat(1).gt.0) call test_ugg_lat(ierr, nlat(1), nlat(2))
  endif
  if (ierr.eq.0) then
     if (nlon(1).gt.0) call test_ugg_lon(ierr, nlon(1), nlon(2))
  endif

  if (ierr.eq.0) then
     if (stereo.gt.0) call batch_test_stereog(ierr, stereo)
  endif
  if (ierr.eq.0) then
     if (geod.eq.0) then
        continue
     else if (geod.gt.0) then
        call batch_test_geod_filter(ierr)
     else if (geod.lt.0) then
        call batch_test_geod_dfilter(ierr)
     endif
  endif
  if (ierr.eq.0) then
     if (section.gt.0) then
        call batch_test_section(ierr)
     endif
  endif
  if (ierr.eq.0) then
     if (length.gt.0) then
        call batch_test_length(ierr)
     endif
  endif
  if (ierr.eq.0) then
     if (cell.gt.0) then
        call batch_test_pscell_props(ierr)
     endif
  endif

  if (ierr.eq.0) then
     if (dazim.gt.0) call batch_test_ugg_dazim(ierr)
  endif

  if (ierr.eq.0) then
     if (agmp.gt.0) call batch_test_agmp(ierr, agmp)
  endif

  if (ierr.eq.0) then
     if (hpangle.gt.0) call batch_test_hpangle(ierr)
  endif

  if (ierr.eq.0) then
     if (stp.gt.0) call batch_test_stp(ierr, stp)
  endif

  call arg_finalize(ierr, levv=+9)
  call finalize(ierr, levv=+9, mode=MODE_DEEPEST)
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

!!!_ + test_ugg_cano
  subroutine test_ugg_cano(ierr, cano)
    integer,intent(out) :: ierr
    integer,intent(in) :: cano
    ierr = 0
    if (cano.lt.0) return ! dummy

    call test_ugg_cano_core(ierr, 0.0_KTGT)
    call test_ugg_cano_core(ierr, 30.0_KTGT)
    call test_ugg_cano_core(ierr, 45.0_KTGT)
    call test_ugg_cano_core(ierr, 60.0_KTGT)
    call test_ugg_cano_core(ierr, 90.0_KTGT)
    call test_ugg_cano_core(ierr, 135.0_KTGT)
    call test_ugg_cano_core(ierr, 180.0_KTGT)
    call test_ugg_cano_core(ierr, 225.0_KTGT)
    call test_ugg_cano_core(ierr, 270.0_KTGT)
    call test_ugg_cano_core(ierr, 315.0_KTGT)
    call test_ugg_cano_core(ierr, 360.0_KTGT)
    call test_ugg_cano_core(ierr, 540.0_KTGT)

    return
  end subroutine test_ugg_cano

  subroutine test_ugg_cano_core(ierr, deg)
    integer,intent(out) :: ierr
    real(kind=KTGT),intent(in) :: deg
    real(kind=KTGT) :: r,  dr,  rr, dd
    real(kind=KTGT) :: sr, sd, si
    real(kind=KTGT) :: cr, cd, ci

    real(kind=KTGT) :: trr(NTRIG), trd(NTRIG)
    real(kind=KTGT),parameter :: tol = 1.0e-10_KTGT

    ierr = 0
    r  = deg2rad(deg)
    rr = deg2rad(deg, nml=.FALSE.)
    dr = rad2deg(r)
    dd = rad2deg(rr, nml=.FALSE.)
    sd = sind_canonical(deg)
    cd = cosd_canonical(deg)
    sr = sin_canonical(r)
    cr = cos_canonical(r)
    trd = setd_sincos(deg)
    trr = set_sincos(r)

    si = sin(r)
    ci = cos(r)

101 format('canonical trigonometric: ', F10.3)
104 format('  angle: ', L1, 1x, 2(1x, ES16.9))
102 format('  sin: ', L1, 1x, 5(1x, ES16.9))
103 format('  cos: ', L1, 1x, 5(1x, ES16.9))
112 format('  sin check: ', 4L1)
113 format('  cos check: ', 4L1)

    write(*, 101) deg
    write(*, 104) deg.eq.dr, r,  dr
    write(*, 104) deg.eq.dd, rr, dd
    write(*, 102) sd.eq.sr, sd, sr, si, trd(JSIN), trr(JSIN)
    write(*, 103) cd.eq.cr, cd, cr, ci, trd(JCOS), trr(JCOS)

    write(*, 112) ABS((/sd, sr, trd(JSIN), trr(JSIN)/) - si) .lt.tol
    write(*, 113) ABS((/cd, cr, trd(JCOS), trr(JCOS)/) - ci) .lt.tol

    return
  end subroutine test_ugg_cano_core


!!!_ + test_ugg_lat
  subroutine test_ugg_lat(ierr, nlat, mdiv)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: nlat
    integer,intent(in)  :: mdiv
    real(kind=KTGT) :: glat(0:nlat-1),  wlat(0:nlat-1)
    real(kind=KTGT) :: glat2(0:nlat-1), wlat2(0:nlat-1)
    real(kind=KTGT) :: glatm(0:nlat)

    real(kind=KTGT) :: ylatc(0:nlat*mdiv-1)
    real(kind=KTGT) :: ylatb(0:nlat*mdiv)

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE =  1.0_KTGT
    real(kind=KTGT),parameter :: HALF = 0.5_KTGT

    real(kind=KTGT) :: round = ZERO
    real(kind=KTGT) :: wnml = ONE
    integer deq

    integer j
    integer method

    ierr = 0

    if (ierr.eq.0) call get_option(ierr, round,   'ly', ZERO)
    if (ierr.eq.0) call get_option(ierr, wnml,    'wy', ONE)
    if (ierr.eq.0) call get_option(ierr, method,  'my', LAT_GAUSS_LEGACY)

    if (ierr.eq.0) call get_latitude(ierr, glat,  wlat, nlat, round=round, wnml=wnml, method=method)
    if (ierr.eq.0) call mid_latitude(ierr, glatm, wlat, nlat)

    if (ierr.eq.0) call gauss_latitude(ierr, glat2, wlat2, nlat, round, wnml, prec=-1.0_KTGT)
    if (ierr.eq.0) glat2(1:nlat) = asin(glat2(1:nlat))

201 format('lat:', I0, 1x, F9.3, 1x, 2E10.3, 2x, 2E10.3, 1x, L1, 1x, E10.3, 1x, 2E10.3)
202 format('latm:', I0, 1x, F9.3, 1x, E16.8)
    if (ierr.eq.0) then
       do j = 0, nlat - 1
          write(*, 201) j, glat(j) * 180.0_KTGT / PI, &
               & glat(j), wlat(j), glat2(j), wlat2(j), &
               & glat(j).eq.glat2(j), glat(j)-glat2(j), &
               & sin(glat(j)), wlat(j) * 2.0_KTGT
       enddo
       do j = 0, nlat
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
             call div_latitude(ierr, ylatc, mdiv, .FALSE., glat, glatm, nlat, round=round, method=method)
          endif
          if (ierr.eq.0)then
             call div_latitude(ierr, ylatb, mdiv, .TRUE., glat, glatm, nlat, round=round, method=method)
          endif
       endif
       if (ierr.eq.0) then
203 format('laty:', I0, 1x, F9.3, 1x, F9.3)
          do j = 0, nlat * mdiv - 1
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
    real(kind=KTGT) :: glonc(0:nlon), wlonc(0:nlon)
    real(kind=KTGT) :: glonb(0:nlon+1),   wlonb(0:nlon+1)

    real(kind=KTGT) :: ylonc(0:nlon*mdiv)
    real(kind=KTGT) :: ylonb(0:nlon*mdiv+1)

    real(kind=KTGT) :: x(0:nlon+1)

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE =  1.0_KTGT
    real(kind=KTGT),parameter :: HALF = 0.5_KTGT

    integer bsw
    integer :: method, deq

    real(kind=KTGT) :: round = ZERO
    real(kind=KTGT) :: wnml = ONE
    integer j
    integer ny

    ierr = 0

    if (ierr.eq.0) call get_option(ierr, round,   'lx', ZERO)
    if (ierr.eq.0) call get_option(ierr, wnml,    'wx', ONE)
    if (ierr.eq.0) call get_option(ierr, bsw,     'bx', 0)    ! boundary (mid_longitude) switch
    if (ierr.eq.0) call get_option(ierr, method,  'mx', DIV_EACH_EDGE)
    if (ierr.eq.0) call get_option(ierr, deq,     'ex', 0)

    if (ierr.eq.0) call get_longitude(ierr, glonc, wlonc, nlon, round=round, wnml=wnml)
    if (ierr.eq.0) then
       if (bsw.eq.0) then
          if (ierr.eq.0) call get_longitude(ierr, glonb, wlonb, nlon+1, div=nlon, round=round, wnml=wnml, org=-HALF)
       else
          if (ierr.eq.0) call mid_longitude(ierr, glonb, glonc, wlonc, nlon, round / wnml)
       endif
    endif

101 format('lon:',  I0, 1x, F9.3, 1x, 2E10.3)
102 format('lonm:', I0, 1x, F9.3, 1x, E16.8)
    if (ierr.eq.0) then
       do j = 0, nlon - 1
          write(*, 101) j, ang2deg(glonc(j), round),  glonc(j), wlonc(j)
       enddo
       do j = 0, nlon
          write(*, 102) j, ang2deg(glonb(j), round), glonb(j)
       enddo
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
             call div_longitude(ierr, ylonc, mdiv, .FALSE., glonc, glonb, nlon, round=round, method=method)
          endif
          if (ierr.eq.0)then
             call div_longitude(ierr, ylonb, mdiv, .TRUE.,  glonc, glonb, nlon, round=round, method=method)
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
    if (ierr.eq.0) call check_longitude(ierr, glonc, nlon,             round=round, tag='lonc')
    if (ierr.eq.0) call check_longitude(ierr, glonb, nlon+1, div=nlon, round=round, tag='lonb')
    if (mdiv.gt.0) then
       if (ierr.eq.0) then
          call check_div_longitude(ierr, ylonc, mdiv, .FALSE., glonc, glonb, nlon, round=round, tag='ylonc')
       endif
       if (ierr.eq.0) then
          call check_div_longitude(ierr, ylonb, mdiv, .TRUE.,  glonc, glonb, nlon, round=round, tag='ylonb')
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

    real(kind=KTGT) :: zla, zlo
    real(kind=KTGT) :: zx,  zy

    ierr = 0
    if (test.lt.0) return ! dummy procedure

    ! snyder example
    a = 6378388.0_KTGT
    e = 0.0819919_KTGT

    latts = 71.0_KTGT
    lon0  = 100.0_KTGT
    zla = 75.0_KTGT
    zlo = 150.0_KTGT
    zx  = 1540033.6_KTGT
    zy  = 560526.4_KTGT
    call test_stereog_single(ierr, a, e, -latts, -lon0, (/-zla, +zlo/), -zx, -zy)
    !  (x, y) = (-1540033.6, -560526.4)
    !       k = 0.9896255
    call test_stereog_single(ierr, a, e, -latts, -lon0, (/-zla, -zlo/))
    call test_stereog_single(ierr, a, e, -latts, -lon0, (/+zla, -zlo/))
    call test_stereog_single(ierr, a, e, -latts, -lon0, (/+zla, +zlo/))

    call test_stereog_single(ierr, a, e, +latts, +lon0, (/+zla, -zlo/), +zx, +zy)
    call test_stereog_single(ierr, a, e, +latts, +lon0, (/+zla, +zlo/))
    call test_stereog_single(ierr, a, e, +latts, +lon0, (/-zla, +zlo/))
    call test_stereog_single(ierr, a, e, +latts, +lon0, (/-zla, -zlo/))

    call test_stereog_single(ierr, a, e, -latts, +lon0, (/-zla, -zlo/), +zx, -zy)
    call test_stereog_single(ierr, a, e, -latts, +lon0, (/-zla, +zlo/))
    call test_stereog_single(ierr, a, e, -latts, +lon0, (/+zla, +zlo/))
    call test_stereog_single(ierr, a, e, -latts, +lon0, (/+zla, -zlo/))

    call test_stereog_single(ierr, a, e, +latts, -lon0, (/+zla, +zlo/), -zx, +zy)
    call test_stereog_single(ierr, a, e, +latts, -lon0, (/+zla, -zlo/))
    call test_stereog_single(ierr, a, e, +latts, -lon0, (/-zla, -zlo/))
    call test_stereog_single(ierr, a, e, +latts, -lon0, (/-zla, +zlo/))

    latts = +71.0_KTGT
    lon0  = -100.0_KTGT
    ! pole
    call test_stereog_single(ierr, a, e, latts, lon0, (/90.0_KTGT, 0.0_KTGT/))
    call test_stereog_single(ierr, a, e, latts, lon0, (/89.9_KTGT, 0.0_KTGT/))

    ! octant
    call banner(ierr, 'octant')
    latts = +71.0_KTGT
    lon0  = 0.0_KTGT
    zla = 80.0_KTGT
    call test_stereog_single(ierr, a, e, latts, lon0, (/zla, lon0/))
    call test_stereog_single(ierr, a, e, latts, lon0, (/zla, lon0+45.0_KTGT/))
    call test_stereog_single(ierr, a, e, latts, lon0, (/zla, lon0+90.0_KTGT/))
    call test_stereog_single(ierr, a, e, latts, lon0, (/zla, lon0+135.0_KTGT/))
    call test_stereog_single(ierr, a, e, latts, lon0, (/zla, lon0+180.0_KTGT/))
    call test_stereog_single(ierr, a, e, latts, lon0, (/zla, lon0+225.0_KTGT/))
    call test_stereog_single(ierr, a, e, latts, lon0, (/zla, lon0+270.0_KTGT/))
    call test_stereog_single(ierr, a, e, latts, lon0, (/zla, lon0+315.0_KTGT/))

    call test_stereog_single(ierr, a, e, -latts, lon0, (/-zla, lon0/))
    call test_stereog_single(ierr, a, e, -latts, lon0, (/-zla, lon0+45.0_KTGT/))
    call test_stereog_single(ierr, a, e, -latts, lon0, (/-zla, lon0+90.0_KTGT/))
    call test_stereog_single(ierr, a, e, -latts, lon0, (/-zla, lon0+135.0_KTGT/))
    call test_stereog_single(ierr, a, e, -latts, lon0, (/-zla, lon0+180.0_KTGT/))
    call test_stereog_single(ierr, a, e, -latts, lon0, (/-zla, lon0+225.0_KTGT/))
    call test_stereog_single(ierr, a, e, -latts, lon0, (/-zla, lon0+270.0_KTGT/))
    call test_stereog_single(ierr, a, e, -latts, lon0, (/-zla, lon0+315.0_KTGT/))

    ! on pole and axis
    zla = 90.0_KTGT
    call test_stereog_single(ierr, a, e, latts, lon0, (/zla, lon0/))
    call test_stereog_single(ierr, a, e, latts, lon0, (/zla, lon0+90.0_KTGT/))
    call test_stereog_single(ierr, a, e, latts, lon0, (/zla, lon0+180.0_KTGT/))
    call test_stereog_single(ierr, a, e, latts, lon0, (/zla, lon0+270.0_KTGT/))

    call test_stereog_single(ierr, a, e, -latts, lon0, (/-zla, lon0/))
    call test_stereog_single(ierr, a, e, -latts, lon0, (/-zla, lon0+90.0_KTGT/))
    call test_stereog_single(ierr, a, e, -latts, lon0, (/-zla, lon0+180.0_KTGT/))
    call test_stereog_single(ierr, a, e, -latts, lon0, (/-zla, lon0+270.0_KTGT/))

    ! k0 = 0
    latts = 90.0_KTGT
    lon0  = 0.0_KTGT
    call test_stereog_single(ierr, a, e, latts, lon0, (/90.0_KTGT, 0.0_KTGT/))
    call test_stereog_single(ierr, a, e, latts, lon0, (/0.0_KTGT, 0.0_KTGT/))

    ! scale factor as 1, e = 0
    latts = +90.0_KTGT
    lon0  = +100.0_KTGT
    e = 0.0_KTGT
    call test_stereog_single(ierr, a, e, latts, lon0, (/+75.0_KTGT, -150.0_KTGT/))

    a = 6378137.0_KTGT
    e = flatten_to_ecc(1.0_KTGT/298.257223563_KTGT)
    latts = +70.0_KTGT
    lon0  = -45.0_KTGT
    call test_stereog_single(ierr, a, e, latts, lon0, (/59.0479222286_KTGT, -55.7362542291_KTGT/))

  end subroutine batch_test_stereog

  subroutine test_stereog_single(ierr, a, e, latts, lon0, ll, xref, yref)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr
    real(kind=KTGT),intent(in) :: a, e, latts, lon0
    real(kind=KTGT),intent(in) :: ll(2)
    real(kind=KTGT),intent(in),optional :: xref, yref
    real(kind=KTGT) :: sf
    real(kind=KTGT) :: rr(2), rts, llorg(2)
    real(kind=KTGT) :: xxo(3), xxc(3), xxr(3), lli(2), zref(2)
    real(kind=KTGT) :: xxp(3)
    real(kind=KTGT) :: cco2(ncache_psgp_co), clo2(ncache_psgp_lo), cla2(ncache_psgp_la)
    real(kind=KTGT) :: cco3(ncache_psgp_co), clo3(ncache_psgp_lo), cla3(ncache_psgp_la)
    real(kind=KTGT) :: clor(ncache_psgp_lo)
    real(kind=KTGT) :: gla(2), dlo(2)
    real(kind=KTGT) :: laref(2), loref(2)
    real(kind=KTGT),parameter :: ONE=1.0_KTGT
    real(kind=KTGT) :: xt, yt
    real(kind=KTGT),parameter :: ztol = 1.0_KTGT
    real(kind=KTGT) :: pxlon, rxlon
    integer pole
    logical bref
    logical undet

    ierr = 0
    bref = present(xref).and.present(yref)
    if (bref) then
       zref(1:2) = (/xref, yref/)
    endif

    undet = ABS(_LATI(ll)).eq.90.0_KTGT

    rts = deg2rad(latts)
    rr(1:2) = deg2rad(ll(1:2))
    if (latts.ge.0.0_KTGT) then
       pole = +1
    else
       pole = -1
    endif
    _LONGI(llorg) = deg2rad(lon0)
    _LATI(llorg)  = deg2rad(90.0_KTGT) * real(pole, kind=KTGT)

    _TRIG(laref) = setd_sincos(_LATI(ll))
    _TRIG(loref) = setd_sincos(_LONGI(ll) - lon0)

101 format('## stereog[', SP, 2F6.1, SS, 1x, 2ES16.8, '] ', '(', SP, 2F7.1, ')')
    write(*, 101) lon0, latts, a, e, ll

112 format('  psgp[', A, ']:', '--', 1x, ' >> (', 2ES16.8, ')', 1x, ES16.8)
115 format('  psgp[', A, ']:', 2L1,  1x, ' >> (', 2ES16.8, ')', 1x, ES16.8)
113 format('  backward:', 2L1,  1x, 2ES16.8, 1x, 2ES16.8, 1x, F16.13)
114 format('  backward:', '--', 1x, 2ES16.8, 1x, 2ES16.8, 1x, F16.13)
116 format('  rlon:', 1x, 2ES16.8)

    call psgp_set(cco2, e, a, rts, _LONGI(llorg), pole)

    xxo = psgp_fwd_cis(_LONGI(rr), _LATI(rr), cco2)
    if (bref) then
       write(*, 115) 'once', ABS(xxo(1:2)-zref(1:2)).lt.ztol, xxo(1:2), ONE / xxo(3)
    else
       write(*, 112) 'once', xxo(1:2), ONE / xxo(3)
    endif
    call psgp_cachela(cla2, _LATI(rr), cco2)
    call psgp_cachelo(clo2, _LONGI(rr), cco2)
    xxc = psgp_fwd_cis(clo2, cla2)
    if (bref) then
       write(*, 115) 'cache', ABS(xxc(1:2)-zref(1:2)).lt.ztol, xxc(1:2), ONE / xxc(3)
    else
       write(*, 112) 'cache', xxc(1:2), ONE / xxc(3)
    endif

    call psgp_inquire(ierr, cco2, pxlon=pxlon)
    rxlon = _LONGI(rr) - pxlon
    call psgp_cachelo_px(clor, rxlon, cco2)
    xxr = psgp_fwd_cis(clor, cla2)
    if (bref) then
       write(*, 115) 'xlon', ABS(xxr(1:2)-zref(1:2)).lt.ztol, xxr(1:2), ONE / xxr(3)
    else
       write(*, 112) 'xlon', xxr(1:2), ONE / xxc(3)
    endif
    write(*, 116) rad2deg(rxlon), rad2deg(pxlon)


    lli = psgp_bwd_ll(xxc(1), xxc(2), cco2)
    sf = psgp_bwd_sf(xxc(1), xxc(2), cco2)
    if (undet) then
       write(*, 114) rad2deg(lli(:)), rad2deg(rr(:)), sf
    else
       write(*, 113) is_same_angle(rr(:), lli(:)), rad2deg(lli(:)), rad2deg(rr(:)), sf
    endif

    call psgp_bwd_tr(gla, dlo, xxc(1), xxc(2), cco2)
141 format(2x, 'lat:', 2L1, 1x, 4ES16.8)
142 format(2x, 'dlon:', 2L1, 1x, 4ES16.8)
143 format(2x, 'dlon:', '--', 1x, 4ES16.8)
    write(*, 141) is_same_angle(gla(1:2), laref(1:2)), gla(1:2), laref(1:2)
    if (undet) then
       write(*, 143) dlo(1:2), loref(1:2)
    else
       write(*, 142) is_same_angle(dlo(1:2), loref(1:2)), dlo(1:2), loref(1:2)
    endif
! 121 format(2x, A, ':', A, 1x, 2ES16.8)
!     write(*, 121) 'sin', 'lat', _SIN(gla), sin(_LATI(rr))
!     write(*, 121) 'cos', 'lat', _COS(gla), cos(_LATI(rr))
!     write(*, 121) 'sin', 'dlon', _SIN(dlo), sin(_LONGI(rr) - _LONGI(llorg))
!     write(*, 121) 'cos', 'dlon', _COS(dlo), cos(_LONGI(rr) - _LONGI(llorg))

122 format(2x, 'xlo:', 3L1, 1x, ES16.8, 1x, 2ES16.8, 1x, ES10.3)
124 format(2x, 'xlo:', L1, '--', ES16.8, 1x, 2ES16.8, 1x, ES10.3)
123 format(2x, 'ylo:', 3L1, ES16.8, 1x, 2ES16.8, 1x, ES10.3)
125 format(2x, 'ylo:', L1, '--', ES16.8, 1x, 2ES16.8, 1x, ES10.3)
132 format(2x, 'xla:', 3L1, ES16.8, 1x, 2ES16.8)
133 format(2x, 'yla:', 3L1, ES16.8, 1x, 2ES16.8)
    call psgp_xlo_tr(xt, dlo, xxc(2), cla2, cco2, xxc(1))
    if (undet) then
       write(*, 124) is_same_coor(xt, xxc(1)), xt, dlo, xt-xxc(1)
    else
       write(*, 122) is_same_coor(xt, xxc(1)), is_same_angle(dlo(1:2), loref(1:2)), xt, dlo, xt-xxc(1)
    endif
    call psgp_ylo_tr(yt, dlo, xxc(1), cla2, cco2, xxc(2))
    if (undet) then
       write(*, 125) is_same_coor(yt, xxc(2)), yt, dlo, yt-xxc(2)
    else
       write(*, 123) is_same_coor(yt, xxc(2)), is_same_angle(dlo(1:2), loref(1:2)), yt, dlo, yt-xxc(2)
    endif
    call psgp_xla_tr(xt, gla, xxc(2), clo2, cco2)
    write(*, 132) is_same_coor(xt, xxc(1)), is_same_angle(gla(1:2), laref(1:2)), xt, gla
    call psgp_yla_tr(yt, gla, xxc(1), clo2, cco2)
    write(*, 133) is_same_coor(yt, xxc(2)), is_same_angle(gla(1:2), laref(1:2)), yt, gla

    ! xy-scale
    call psgp_set(cco3, e, a, rts, _LONGI(llorg), pole, xs=100.0_KTGT, ys=1000.0_KTGT)

    xxo = psgp_fwd_cis(_LONGI(rr), _LATI(rr), cco3)
    write(*, 112) 'scale', xxo(1:2), ONE / xxo(3)
    call psgp_cachela(cla3, _LATI(rr), cco3)
    call psgp_cachelo(clo3, _LONGI(rr), cco3)
    xxc = psgp_fwd_cis(clo3, cla3)
    write(*, 112) 'scale', xxc(1:2), ONE / xxc(3)

    lli = psgp_bwd_ll(xxc(1), xxc(2), cco3)
    sf = psgp_bwd_sf(xxc(1), xxc(2), cco3)
    if (undet) then
       write(*, 114) rad2deg(rr(:)), sf
    else
       write(*, 113) is_same_angle(rr(:), lli), rad2deg(lli(:)), rad2deg(rr(:)), sf
    endif

    call psgp_bwd_tr(gla, dlo, xxc(1), xxc(2), cco3)
    write(*, 141) is_same_angle(gla(1:2), laref(1:2)), gla(1:2), laref(1:2)
    if (undet) then
       write(*, 143) dlo(1:2), loref(1:2)
    else
       write(*, 142) is_same_angle(dlo(1:2), loref(1:2)), dlo(1:2), loref(1:2)
    endif
    ! write(*, 121) 'sin', 'lat', _SIN(gla), sin(_LATI(rr))
    ! write(*, 121) 'cos', 'lat', _COS(gla), cos(_LATI(rr))
    ! write(*, 121) 'sin', 'dlon', _SIN(dlo), sin(_LONGI(rr) - _LONGI(llorg))
    ! write(*, 121) 'cos', 'dlon', _COS(dlo), cos(_LONGI(rr) - _LONGI(llorg))

    call psgp_xlo_tr(xt, dlo, xxc(2), cla3, cco3, xxc(1))
    if (undet) then
       write(*, 124) is_same_coor(xt, xxc(1)), xt, dlo, xt-xxc(1)
    else
       write(*, 122) is_same_coor(xt, xxc(1)), is_same_angle(dlo(1:2), loref(1:2)), xt, dlo, xt-xxc(1)
    endif
    call psgp_ylo_tr(yt, dlo, xxc(1), cla3, cco3, xxc(2))
    if (undet) then
       write(*, 125) is_same_coor(yt, xxc(2)), yt, dlo, yt-xxc(2)
    else
       write(*, 123) is_same_coor(yt, xxc(2)), is_same_angle(dlo(1:2), loref(1:2)), yt, dlo, yt-xxc(2)
    endif
    call psgp_xla_tr(xt, gla, xxc(2), clo3, cco3)
    write(*, 132) is_same_coor(xt, xxc(1)), is_same_angle(gla(1:2), laref(1:2)), xt, gla
    call psgp_yla_tr(yt, gla, xxc(1), clo3, cco3)
    write(*, 133) is_same_coor(yt, xxc(2)), is_same_angle(gla(1:2), laref(1:2)), yt, gla

152 format(2x, '+xlon:', A, ': ', ES10.2)
    call psgp_inquire(ierr, cco2, pxlon=pxlon)
    write(*, 152) '2', rad2deg(pxlon)
    call psgp_inquire(ierr, cco3, pxlon=pxlon)
    write(*, 152) '3', rad2deg(pxlon)

  end subroutine test_stereog_single

!!!_ + is_same_angle
  ELEMENTAL &
  logical function is_same_angle(a1, a2) result(b)
    implicit none
    integer,parameter :: KTGT=KDBL
    real(kind=KTGT),intent(in) :: a1, a2
    real(kind=KTGT),parameter :: tol = 1.0e-13_KTGT

    b = ABS(radian_modulo(a1) - radian_modulo(a2)) .lt. tol
  end function is_same_angle

!!!_ + is_same_coor
  ELEMENTAL &
  logical function is_same_coor(a1, a2) result(b)
    implicit none
    integer,parameter :: KTGT=KDBL
    real(kind=KTGT),intent(in) :: a1, a2
    real(kind=KTGT),parameter :: tol = 1.0e-13_KTGT

    if (a2.eq.0.0_KTGT) then
       b = ABS(a1 - a2) .lt. tol
    else
       b = ABS((a1 - a2) / a2) .lt. tol
    endif
  end function is_same_coor

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

    write(*, 111) x0, y0, x1, y1, rad2deg(ll(1:2)), sc

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

    write(*, 111) x0, y0, x1, y1, rad2deg(ll(1:2)), k

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
       clat = cos(ll(_LAIDX, j))
       slat = sin(ll(_LAIDX, j))
       if (abs(slat).eq.ONE) clat = ZERO
       clon = cos(ll(_LOIDX,j))
       slon = sin(ll(_LOIDX,j))
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
    real(kind=KTGT) :: gdis, garea, asign
    real(kind=KTGT) :: glat1(2), glat2(2), dglon(3)
    real(kind=KTGT) :: xlat1(2), xlat2(2), xglon(3)
    real(kind=KTGT) :: inia1(2), azim1(2)

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

       _TRIG(glat1) = setd_sincos(lat1d)
       _TRIG(glat2) = setd_sincos(lat2d)
       _TRIG(dglon) = setd_sincos(dlon)
       _ANGLE(dglon) = deg2rad(dlon)

       xlat1(:) = glat1(:)
       xlat2(:) = glat2(:)
       xglon(:) = dglon(:)

       call geodesic_inverse_canonical(ierr, xlat1, xlat2, xglon, asign)
       if (nc.eq.6) then
          inia1(1) = v(4)
          inia1(2) = v(5)
       else
          call geodesic_inverse_guess(ierr, inia1, xlat1, xlat2, _ANGLE(xglon), f)
       endif
       if (ierr.eq.0) then
101       format('guess: ', 2ES24.16, 1x, F16.11)
          write(*, 101) inia1, rad2deg(atan2(_SIN(inia1), _COS(inia1)))
          ! call geodesic_inverse_core &
          !      (ierr, gdis, xlat1, xlat2, xglon, inia1, f, a, garea=garea)
          ! garea = garea * asign
111       format('geod:c ', 4ES24.16, 1x, F24.10, 1x, ES24.16)
          ! write(*, 111) v(0:3), gdis, garea
          gdis = -999
          garea = -999
          call geodesic_inverse &
               (ierr, gdis, glat1, glat2, dglon, f, a, garea=garea, azim1=azim1)
112       format('geod:d ', 4ES24.16, 1x, F24.10, 1x, ES24.16, 1x, F24.14)
          write(*, 112) v(0:3), gdis, garea, rad2deg(atan2(azim1(1), azim1(2)))
       else
          ierr = 0
       endif
    enddo
  end subroutine batch_test_geod_filter

!!!_ + batch_test_geod_dfilter
  subroutine batch_test_geod_dfilter(ierr)
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
    real(kind=KTGT) :: lat1d, azi1d, gdis
    real(kind=KTGT) :: glat1(2), glat2(2), dglon(3)
    real(kind=KTGT) :: azim1(2)

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
       if (nc.lt.3) then
          write(*, *) 'too few items: ', trim(text)
          ierr = -1
          exit
       endif
       lat1d = v(0)
       azi1d = v(1)
       gdis  = v(2)

       _TRIG(glat1) = setd_sincos(lat1d)
       _TRIG(azim1) = setd_sincos(azi1d)
       if (ierr.eq.0) then
          call geodesic_direct_core(ierr, glat2, dglon, glat1, azim1, gdis, f, a)
111       format('geod:c: ', 3ES24.16, 1x, ES24.16, 1x, ES24.16)
          write(*, 111) v(0:2), &
               & rad2deg(ATAN2(_SIN(glat2), _COS(glat2))), &
               & rad2deg(ATAN2(_SIN(dglon), _COS(dglon)))
          glat2(:) = -999
          dglon(:) = -999
          call geodesic_direct(ierr, glat2, dglon, glat1, azim1, gdis, f, a)
112       format('geod:d: ', 3ES24.16, 1x, ES24.16, 1x, ES24.16)
          write(*, 112) v(0:2), &
               & rad2deg(ATAN2(_SIN(glat2), _COS(glat2))), &
               & rad2deg(ATAN2(_SIN(dglon), _COS(dglon)))
       else
          ierr = 0
       endif
    enddo
  end subroutine batch_test_geod_dfilter

!!!_ + batch_test_pscell_props
  subroutine batch_test_pscell_props(ierr)
    use TOUZA_Std,only: split_list
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr

    real(kind=KTGT) :: a, f, rf, e
    integer,parameter :: lline = 1024
    integer,parameter :: lcols = 16
    character(len=lline) :: text, tag
    integer :: nc
    real(kind=KTGT) :: v(0:lcols)
    real(kind=KTGT) :: x0, y0, dx, dy
    real(kind=KTGT) :: tslat, llorg(2)

    ierr = 0

    a = 6378137.0_KTGT
    rf = 298.257223563_KTGT
    f = 1.0_KTGT / rf
    e = flatten_to_ecc(f)

    _LONGI(llorg) = deg2rad(-45.0_KTGT)
    _LATI(llorg)  = deg2rad(90.0_KTGT)
    tslat = deg2rad(+70.0_KTGT)

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
       if (ierr.eq.0) then
          ! geographic coordinates
          if (v(2).lt.0.0_KTGT) then
             v(0) = v(0) + v(2)
             v(2) = - v(2)
          endif
          if (v(3).lt.0.0_KTGT) then
             v(1) = v(1) + v(3)
             v(3) = - v(3)
          endif
          x0 = v(0)
          y0 = v(1)
          dx = v(2)
          dy = v(3)

          tag = ' '
          call test_pscell_props &
               & (ierr, &
               &  x0, y0, dx,  dy,    tag, &
               &  a,  e,  f,   llorg, tslat)
       else
          ierr = 0
       endif
    enddo
  end subroutine batch_test_pscell_props
!!!_ + test_pscell_props - show properties of a cell on polar stereographic domain
  subroutine test_pscell_props &
       & (ierr, &
       &  x0, y0, dx,  dy,    tag, &
       &  a,  e,  f,   llorg, tslat)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr
    real(kind=KTGT),intent(in) :: x0, y0
    real(kind=KTGT),intent(in) :: dx, dy
    character(len=*),intent(in) :: tag
    real(kind=KTGT),intent(in) :: a,  e,  f
    real(kind=KTGT),intent(in) :: llorg(*), tslat  ! radian

    integer jp, jpfrm, jpto
    integer,parameter :: npos = 9
    real(kind=KTGT) :: cpos(2, 0:npos-1)
    real(kind=KTGT) :: gpos(2, 0:npos-1)

    integer,parameter :: POSNW = 7, POSN = 6, POSNE = 5
    integer,parameter :: POSW  = 8, POSO = 0, POSE  = 4
    integer,parameter :: POSSW = 1, POSS = 2, POSSE = 3

    integer pole
    real(kind=KTGT) :: cco(ncache_psgp_co)

    character(len=1) :: dtag(0:npos-1) = (/' ', 'S', 'S', 'E', 'E', 'N', 'N', 'W', 'W' /)

    integer js
    integer,parameter :: nside = 4
    real(kind=KTGT) :: garea(0:nside-1)
    real(kind=KTGT) :: asum
    integer llev
    integer levbgn, levend
    real(kind=KTGT) :: parea, atol, ares

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: HALF = 0.5_KTGT
    integer :: ngdiv = 32
    character(len=16) :: sub

    ierr = 0

    cpos(1:2, POSSW) = (/x0, y0/)
    cpos(1:2, POSSE) = cpos(1:2, POSSW) + (/dx, ZERO/)
    cpos(1:2, POSNE) = cpos(1:2, POSSW) + (/dx, dy/)
    cpos(1:2, POSNW) = cpos(1:2, POSSW) + (/ZERO, dy/)

    cpos(1:2, POSS)  = (/x0 + HALF * dx, y0/)
    cpos(1:2, POSW)  = (/x0,             y0 + HALF * dy/)
    cpos(1:2, POSE)  = (/x0 + dx,        y0 + HALF * dy/)
    cpos(1:2, POSN)  = (/x0 + HALF * dx, y0 + dy/)

    cpos(1:2, POSO)  = (/x0 + HALF * dx, y0 + HALF * dy/)

    if (_LATI(llorg).gt.ZERO) then
       pole = +1
    else
       pole = -1
    endif

    call psgp_set(cco, e, a, tslat, _LONGI(llorg), pole)

101 format(A, ':pos: ', I0, 1x, 2F12.0, 1x, 2F17.12)

    do jp = 0, npos - 1
       gpos(1:2, jp) = psgp_bwd_ll(cpos(1, jp), cpos(2, jp), cco)
       write(*, 101) trim(tag), &
            & jp, cpos(1:2, jp), rad2deg(gpos(2:1:-1, jp))
    enddo

111 format(I1, I1, A1, A1)
    do jpfrm = 1, npos - 1
       jpto = jpfrm + 1
       if (jpto.ge.npos) jpto = jpto - (npos - 1)

       write(sub, 111) jpfrm, jpto, 's', dtag(jpfrm)
       call diag_pscell_side &
            & (ierr, gpos(:,jpfrm), gpos(:,jpto), cpos(:,jpfrm), cpos(:,jpto), &
            &  tag,  sub, f, a, cco, ngdiv)
    enddo

    jpto = 0
    do jpfrm = POSS, npos - 1, 2
       write(sub, 111) jpfrm, jpto, 'c'
       call diag_pscell_side &
            & (ierr, gpos(:,jpfrm), gpos(:,jpto), cpos(:,jpfrm), cpos(:,jpto), &
            &  tag,  sub, f, a, cco, ngdiv)
    enddo

    jpfrm = POSS
    jpto = POSN
    write(sub, 111) jpfrm, jpto, 'v'
    call diag_pscell_side &
         & (ierr, gpos(:,jpfrm), gpos(:,jpto), cpos(:,jpfrm), cpos(:,jpto), &
         &  tag,  sub, f, a, cco, ngdiv)

    jpfrm = POSW
    jpto = POSE
    write(sub, 111) jpfrm, jpto, 'h'
    call diag_pscell_side &
         & (ierr, gpos(:,jpfrm), gpos(:,jpto), cpos(:,jpfrm), cpos(:,jpto), &
         &  tag,  sub, f, a, cco, ngdiv)

    js = 0
    do jpfrm = POSSW, npos - 1, 2
       jpto = jpfrm + 2
       if (jpto.ge.npos) jpto = jpto - (npos - 1)
       write(sub, 111) jpfrm, jpto, 'l', dtag(jpfrm)
       call diag_pscell_side &
            & (ierr, gpos(:,jpfrm), gpos(:,jpto), cpos(:,jpfrm), cpos(:,jpto), &
            &  tag,  sub, f, a, cco, ngdiv, garea(js))
       js = js + 1
    enddo

    asum = - ((garea(0) + garea(2)) + (garea(1) + garea(3)))

123 format(A, ':areag: ', I0, 1x, F24.10)
121 format(A, ':area1: ', I0, 1x, F24.10, 1x, ES8.1)
122 format(A, ':area2: ', I0, 1x, F24.10, 1x, ES8.1)
    write(*, 123) trim(tag), 0, asum

    levbgn = 8
    levend = 12
    do llev = levbgn, levend
       atol = 0.0_KTGT
       call psgp_bwd_area &
            & (parea, &
            &  cpos(1, POSSW), cpos(2, POSSW), cpos(1, POSNE), cpos(2, POSNE), &
            &  cco, llev, llev+1, atol, ares)
       write(*, 121) trim(tag), llev, parea, ares
    enddo

    ! levbgn = 8
    ! levend = 13
    do llev = levbgn, levend
       atol = 0.0_KTGT
       call psgp_bwd_area2 &
            & (parea, &
            &  cpos(1, POSSW), cpos(2, POSSW), cpos(1, POSNE), cpos(2, POSNE), &
            &  cco, llev, llev+1, atol, ares)
       write(*, 122) trim(tag), llev, parea, ares
    enddo

  end subroutine test_pscell_props

!!!_ + diag_pscell_side
  subroutine diag_pscell_side &
       & (ierr, gpos1, gpos2, cpos1, cpos2, tag, sub, f, a, cco, ngdiv, garea)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr
    real(kind=KTGT),intent(in) :: gpos1(*), gpos2(*)
    real(kind=KTGT),intent(in) :: cpos1(*), cpos2(*)
    real(kind=KTGT),intent(in) :: f, a
    character(len=*),intent(in) :: tag, sub
    real(kind=KTGT),intent(in) :: cco(*)
    integer,        intent(in) :: ngdiv
    real(kind=KTGT),intent(out),optional :: garea

    integer j
    real(kind=KTGT) :: glat1(2), glat2(2), dglon(3), glon1(2), glon2(2)
    real(kind=KTGT) :: azim1(2)
    real(kind=KTGT) :: lltmp(2), xytmp(2)
    real(kind=KTGT) :: d, gdis
    real(kind=KTGT) :: pdis,  dtol, dres
    real(kind=KTGT) :: zdis, zarea, zdres, zares, zatol
    integer:: levbgn, levend

    ierr = 0

    _TRIG(glat1) = set_sincos(_LATI(gpos1))
    _TRIG(glat2) = set_sincos(_LATI(gpos2))
    _TRIG(glon1) = set_sincos(_LONGI(gpos1))
    _TRIG(glon2) = set_sincos(_LONGI(gpos2))
    dglon(1:3) = set_dlongi(glon1,  glon2)
    call geodesic_inverse &
         (ierr, gdis, glat1, glat2, dglon, f, a, garea=garea, azim1=azim1)

    levbgn = 10
    levend = 20
    dtol = 0.0_KTGT
    call psgp_bwd_length &
         & (pdis, cpos1(1), cpos1(2), cpos2(1), cpos2(2), cco, levbgn, levend, dtol, dres)

    levbgn = 0
    levend = 10
    zatol = -1.e-2_KTGT
    call psgp_bwd_geod &
         & (zarea, zdis, cpos1(1), cpos1(2), cpos2(1), cpos2(2), &
         &  cco, levbgn, levend, zatol, zares, zdres)

102 format(A, ':side:',  A, 1x, 2F19.14, 1x, 2F19.14, 1x, F16.10, 1x, F16.10, 1x, ES8.1)
103 format(A, ':geod:',  A, 1x, 2F14.6, 1x, 2F19.14, 1x, F16.10)
104 format(A, ':pgeod:',  A, 1x, 2F19.14, 1x, 2F19.14, 1x, F20.7, 1x, ES8.1, 1x, F16.10, 1x, ES8.1)

    write(*, 102) trim(tag), trim(sub), &
         & rad2deg(gpos1(2:1:-1)), rad2deg(gpos2(2:1:-1)), &
         & gdis, pdis, dres
    write(*, 104) trim(tag), trim(sub), &
         & rad2deg(gpos1(2:1:-1)), rad2deg(gpos2(2:1:-1)), &
         & zarea, zares, zdis, zdres

    do j = 0, ngdiv
       d = gdis * real(j, kind=KTGT) / real(ngdiv, kind=KTGT)
       call geodesic_direct(ierr, glat2, dglon, glat1, azim1, d, f, a)
       _TRIG(glon2) = add_angle(glon1, _TRIG(dglon))
       _LATI(lltmp) = ATAN2(glat2(1), glat2(2))
       _LONGI(lltmp) = ATAN2(glon2(1), glon2(2))
       xytmp(1:2) = psgp_fwd(_LONGI(lltmp), _LATI(lltmp), cco)
       write(*, 103) trim(tag), trim(sub), &
            & xytmp(1:2) - cpos1(1:2), rad2deg(lltmp(2:1:-1)), d
    enddo
  end subroutine diag_pscell_side

!!!_ + test_ugg_dazim
  subroutine batch_test_ugg_dazim(ierr)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr
    ierr = 0
    call test_ugg_dazim(ierr, 90.0_KTGT, 90.0_KTGT, 180.0_KTGT)
    call test_ugg_dazim(ierr, 90.0_KTGT, 90.0_KTGT, 170.0_KTGT)
    call test_ugg_dazim(ierr, 90.0_KTGT, 90.0_KTGT, 90.0_KTGT)
    call test_ugg_dazim(ierr, 45.0_KTGT, 45.0_KTGT, 90.0_KTGT)
    call test_ugg_dazim(ierr, 45.0_KTGT, 60.0_KTGT, 90.0_KTGT)
    call test_ugg_dazim(ierr, 15.0_KTGT, 35.0_KTGT, 25.0_KTGT)

    return
  end subroutine batch_test_ugg_dazim

  subroutine test_ugg_dazim(ierr, lat1, lat2, dlon)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr
    real(kind=KTGT),intent(in)  :: lat1, lat2, dlon
    real(kind=KTGT) :: scla1(2), scla2(2), sclo(2)

    real(kind=KTGT) :: dazim

    ierr = 0
    scla1(:) = setd_sincos(lat1)
    scla2(:) = setd_sincos(lat2)
    sclo(:) = setd_sincos(dlon)

    dazim = geodesic_dazim(scla1, scla2, sclo)
101 format('dazim: ', F9.2, F9.2, F9.2, 1x, F9.2, 1x, E16.9)
    write(*, 101) lat1, lat2, dlon, rad2deg(dazim), dazim
    return
  end subroutine test_ugg_dazim

!!!_ + test_agmp
  subroutine batch_test_agmp(ierr, ktest)
    use TOUZA_Std,only: get_nparam, get_param
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr
    integer,intent(in)  :: ktest
    real(kind=KTGT) :: dlat,   dlon
    real(kind=KTGT) :: reflat, reflon
    real(kind=KTGT) :: xbase
    integer :: xrange(2)
    integer narg, jarg
    integer j
    integer xbgn, xend, swap
    logical bswap
    ierr = 0

    xbgn = -1
    xend = -2
    ! call test_agmp(ierr, 90.0_KTGT, -0.1_KTGT, 0.1_KTGT, .FALSE.)
    narg = get_nparam()
    call get_option(ierr, xrange, 'range',  0)
    call get_option(ierr, xbase,  'base',   0.0_KTGT)
    call get_option(ierr, reflon, 'reflon', 0.0_KTGT)
    call get_option(ierr, dlat,   'dlat',   0.0_KTGT)
    call get_option(ierr, dlon,   'dlon',   0.0_KTGT)
    call get_option(ierr, swap,   'swap',   1)

    if (ktest.eq.1) then
       if (ierr.eq.0) then
          xrange(1) = max(0, xrange(1))
          if (xrange(2).eq.0) then
             xrange(2) = xrange(1)
          else if (xrange(2).lt.0) then
             xrange(2) = 7
          endif
          xbgn = xrange(1)
          xend = max(xrange(1), xrange(2))
          if (xbase.le.0.0_KTGT) xbase = 10.0_KTGT
          if (dlat.le.0.0_KTGT) dlat = 1.0_KTGT
       endif
       do jarg = 1, narg
          if (ierr.eq.0) call get_param(ierr, reflat, jarg)
          if (ierr.eq.0) then
             dlon = 1.0_KTGT
             do j = 0, xbgn - 1
                dlon = dlon / xbase
             enddo
             do j = xbgn, xend
                call test_agmp(ierr, reflat, reflon, dlat, -dlon, .TRUE.)
                dlon = dlon / xbase
             enddo
          endif
       enddo
    else
       bswap = swap.eq.1
       reflat = 90.0_KTGT
       reflon = 0.0_KTGT
       jarg = 1
       if (ierr.eq.0) call get_param(ierr, reflat, jarg)
       jarg = jarg + 1
       if (ierr.eq.0) call get_param(ierr, reflon, jarg)
       if (ierr.eq.0) call test_agmp2(ierr, reflat, reflon, dlat, dlon, bswap)
    endif
    return
  end subroutine batch_test_agmp

  subroutine test_agmp2(ierr, reflat, reflon, dlat, dlon, swap)
    implicit none
    integer,parameter :: KTGT=KDBL
#if OPT_REAL_QUADRUPLE_DIGITS > 0
    integer,parameter :: KREF=KQPL
#else
    integer,parameter :: KREF=KTGT
#endif

    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(in)  :: reflat, reflon
    real(kind=KTGT),intent(in)  :: dlat,   dlon
    logical,        intent(in)  :: swap

    real(kind=KTGT) :: xrlat, xdlat
    real(kind=KTGT) :: lat1,  lat2
    real(kind=KTGT) :: lon1,  lon2

    ! y series: target kind
    real(kind=KTGT) :: ylat1(2), ylat2(2), ydlat(2), ytdla(2)
    real(kind=KTGT) :: ylon1(2), ylon2(2), ydlon(2), ytdlo(2)

    real(kind=KTGT) :: vdlonh, vdlath
    real(kind=KTGT) :: vag, vap, va

    real(kind=KTGT) :: udlonh, udlath

    ! z series: higher precision kind
    real(kind=KREF) :: qdlonh, qdlath
    real(kind=KREF) :: zlat1(2), zlat2(2), zdlat(2)
    real(kind=KREF) :: zlon1(2), zlon2(2), zdlon(2)
    real(kind=KREF) :: tqref

    real(kind=KREF) :: qag, qap, qa
    real(kind=KTGT) :: ax
    integer,parameter :: ltbl = 128
    real(kind=KTGT) :: CD(0:ltbl)
    real(kind=KTGT) :: CE(0:ltbl)
    real(kind=KTGT) :: tdhsq, tdh

    integer jo
    integer obgn_d, oend_d
    integer obgn_e, oend_e

    obgn_d = 4
    oend_d = 10
    obgn_e = 2
    oend_e = 5

    if (swap) then
       xrlat = reflat + dlat
       xdlat = -dlat
    else
       xrlat = reflat
       xdlat = +dlat
    endif

    lat1 = xrlat
    lat2 = xrlat + xdlat
    lon1 = reflon
    lon2 = reflon + dlon

    ylat1(:) = setd_sincos(lat1)
    ylat2(:) = setd_sincos(lat2)
    ylon1(:) = setd_sincos(lon1)
    ylon2(:) = setd_sincos(lon2)

    ! ytdla ytdlo: indirect angle subtraction
    ytdla(:) = hpsub_angle(ylat2, ylat1)
    ytdlo(:) = hpsub_angle(ylon2, ylon1)

    vdlonh  = deg2rad(dlon)  / 2.0_KTGT
    vdlath  = deg2rad(xdlat) / 2.0_KTGT

    udlonh = _SIN(ytdlo) / (1.0_KTGT + _COS(ytdlo))   ! tan
    udlath = _SIN(ytdla) / (1.0_KTGT + _COS(ytdla))

    ! high precision
    qdlonh  = deg2rad(real(dlon,  kind=KREF)) / 2.0_KREF
    qdlath  = deg2rad(real(xdlat, kind=KREF)) / 2.0_KREF

    zlat1(:) = setd_sincos(real(lat1, kind=KREF))
    zlat2(:) = setd_sincos(real(lat2, kind=KREF))
    zlon1(:) = setd_sincos(real(lon1, kind=KREF))
    zlon2(:) = setd_sincos(real(lon2, kind=KREF))

301 format('agmp:in ', 4(1x, ES16.9))
    write(*, 301) lat1, lat2, lon1, lon2

101 format('agmp:lat:q ', I0, 1x, ES23.15, 1x, ES23.15)
102 format('agmp:lat:v ', I0, 1x, ES23.15, 1x, ES23.15)

111 format('agmp:tdlath:q ', ES23.15)
121 format('agmp:tdlath:v ', ES23.15, 1x, ES23.15)
131 format('agmp:tdlath:u ', ES23.15, 1x, ES23.15)

112 format('agmp:tdlonh:q ', ES23.15)
122 format('agmp:tdlonh:v ', ES23.15, 1x, ES23.15)
132 format('agmp:tdlonh:u ', ES23.15, 1x, ES23.15)

201 format('agmp:area:q ', ES23.15, 1x, ES23.15, 1x, ES23.15)
211 format('agmp:area:v ', ES23.15, 1x, ES23.15, 1x, ES23.15)
221 format('agmp:area:u ', ES23.15, 1x, ES23.15, 1x, ES23.15)
231 format('agmp:area:dd ', I0, 1x, ES23.15, 1x, ES23.15, 1x, ES23.15)
241 format('agmp:area:ed ', I0, 1x, ES23.15, 1x, ES23.15, 1x, ES23.15)
251 format('agmp:area:di ', I0, 1x, ES23.15, 1x, ES23.15, 1x, ES23.15)
261 format('agmp:area:ei ', I0, 1x, ES23.15, 1x, ES23.15, 1x, ES23.15)

212 format('agmp:err:v  ', ES23.15, 1x, ES23.15)
222 format('agmp:err:u  ', ES23.15, 1x, ES23.15)

    write(*, 101) 1, _SIN(zlat1), _COS(zlat1)
    write(*, 102) 1, _SIN(ylat1), _COS(ylat1)

    write(*, 101) 2, _SIN(zlat2), _COS(zlat2)
    write(*, 102) 2, _SIN(ylat2), _COS(ylat2)

    tqref = tan(qdlath)
    write(*, 111) tqref
    write(*, 121) tan(vdlath), (tan(vdlath) - tqref) / tqref
    write(*, 131) udlath,      (udlath - tqref) / tqref

    tqref = tan(qdlonh)
    write(*, 112) tqref
    write(*, 122) tan(vdlonh), (tan(vdlonh) - tqref) / tqref
    write(*, 132) udlonh,      (udlonh - tqref) / tqref

    ! reference
    qag = ATAN((_SIN(zlat1) + _COS(zlat1) * tan(qdlath)) * tan(qdlonh)) * 2.0_KREF
    qap = _SIN(zlat1) * (qdlonh * 2.0_KREF)
    qa  = qag - qap
    write(*, 201) qa, qag, qap

    ! target direct diff
    vag = ATAN((_SIN(ylat1) + _COS(ylat1) * tan(vdlath)) * tan(vdlonh)) * 2.0_KTGT
    vap = _SIN(ylat1) * (vdlonh * 2.0_KTGT)
    va  = vag - vap
    write(*, 211) va, vag, vap
    write(*, 212) (va - qa) / qa, va - qa

    ! target indirect diff
    vag = ATAN((_SIN(ylat1) + _COS(ylat1) * udlath) * udlonh) * 2.0_KTGT
    vap = _SIN(ylat1) * (ATAN(udlonh) * 2.0_KTGT)
    va  = vag - vap
    write(*, 221) va, vag, vap
    write(*, 222) (va - qa) / qa, va - qa

    ! (d) direct diff by tables
    do jo = obgn_d, oend_d
       call agmpd_gen_table(ierr, Cd, jo, _SIN(ylat1), .TRUE.)
       ax = agmpd_area_core(_COS(ylat1), tan(vdlath), vdlonh, Cd, jo)
       write(*, 231) jo, ax, (ax - qa) / qa, ax - qa
    enddo
    ! (d) indirect diff by tables
    do jo = obgn_d, oend_d
       call agmpd_gen_table(ierr, Cd, jo, _SIN(ylat1), .TRUE.)
       ax = agmpd_area_core(_COS(ylat1), udlath, ATAN(udlonh), Cd, jo)
       write(*, 251) jo, ax, (ax - qa) / qa, ax - qa
    enddo
    ! (e) direct diff by tables
    tdh   = tan(vdlath)
    tdhsq = tdh ** 2
    do jo = obgn_e, oend_e
       call agmpe_gen_table(ierr, Ce, jo, vdlonh, tdhsq, .TRUE.)
       ax = agmpe_area_core(_SIN(ylat1), _COS(ylat1), tdh, Ce, jo)
       write(*, 241) jo, ax, (ax - qa) / qa, ax - qa
    enddo
    ! (e) indirect diff by tables
    tdh   = udlath
    tdhsq = tdh ** 2
    do jo = obgn_e, oend_e
       call agmpe_gen_table(ierr, Ce, jo, ATAN(udlonh), tdhsq, .TRUE.)
       ax = agmpe_area_core(_SIN(ylat1), _COS(ylat1), tdh, Ce, jo)
       write(*, 261) jo, ax, (ax - qa) / qa, ax - qa
    enddo

  end subroutine test_agmp2

  subroutine test_agmp(ierr, reflat, reflon, dlat, dlon, swap)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr
    real(kind=KTGT),intent(in) :: reflat, reflon
    real(kind=KTGT),intent(in) :: dlat,   dlon
    logical,        intent(in) :: swap
    real(kind=KTGT) :: sclat1(2), sclat2(2)
    real(kind=KTGT) :: sclon1(2), sclon2(2)

    real(kind=KTGT) :: scdla(2), scdlo(2)
    real(kind=KTGT) :: dlonh,    tdlath

    integer,parameter :: ltbl = 128
    real(kind=KTGT) :: CC(0:ltbl)
    real(kind=KTGT) :: ax, ag, ap, ad

    real(kind=KTGT) :: CD(0:ltbl)

    real(kind=KTGT) :: CE(0:ltbl)
    real(kind=KTGT) :: tdhsq, xtdhsq

    real(kind=KTGT) :: xrlat, xrlon, xdlat, xdlonh, xtdlath
#if OPT_REAL_QUADRUPLE_DIGITS > 0
    integer,parameter :: KHPR=KQPL
    real(kind=KHPR) :: qlat1(2), qlat2(2)
    real(kind=KHPR) :: qlon1(2), qlon2(2)
    real(kind=KHPR) :: qdla(2),  qdlo(2)
    real(kind=KHPR) :: qdlonh,   qtdlath
    real(kind=KHPR) :: qag, qap, qad
    real(kind=KHPR) :: qaref
#endif

    integer jo, jt, nt

    CC(:) = 0.0_KTGT

    if (swap) then
       xrlat = reflat + dlat
       xdlat = -dlat
    else
       xrlat = reflat
       xdlat = +dlat
    endif

    sclat1(:) = setd_sincos(xrlat)
    sclat2(:) = setd_sincos(xrlat + xdlat)
    sclon1(:) = setd_sincos(reflon)
    sclon2(:) = setd_sincos(reflon + dlon)

    xdlonh = deg2rad(dlon) / 2.0_KTGT
    xtdlath = tan(deg2rad(xdlat) / 2.0_KTGT)

    scdla(:) = hpsub_angle(sclat2, sclat1)
    scdlo(:) = hpsub_angle(sclon2, sclon1)

    dlonh  = ATAN2(_SIN(scdlo), 1.0_KTGT + _COS(scdlo))
    tdlath = _SIN(scdla) / (1.0_KTGT + _COS(scdla))

#if OPT_REAL_QUADRUPLE_DIGITS > 0
    ! write(*, *) 'rad:xrlat', real(xrlat, kind=KHPR), &
    !      & real(xrlat, kind=KHPR) * (atan2(0.0_KHPR, -1.0_KHPR) / 180.0_KHPR), &
    !      & (real(xrlat, kind=KHPR) / 180.0_KHPR) * atan2(0.0_KHPR, -1.0_KHPR), &
    !      & (xrlat / 180.0_KTGT) * atan2(0.0_KTGT, -1.0_KTGT), &
    !      & deg2rad(real(xrlat, kind=KHPR))
    ! write(*, *) sin(deg2rad(real(xrlat, kind=KHPR)))
    ! write(*, *) sin(1.39626322706253841833309025603579_KHPR)
    qlat1(:) = setd_sincos(real(xrlat, kind=KHPR))
    qlat2(:) = setd_sincos(real(xrlat + xdlat, kind=KHPR))
    qdla(:)  = setd_sincos(real(xdlat, kind=KHPR))

    qdlo(:)  = setd_sincos(real(dlon, kind=KHPR))
    ! qref(:) = real(scref(:), kind=KHPR)
    ! qtgt(:) = real(sctgt(:), kind=KHPR)
    ! qdla(:) = real(scdla(:), kind=KHPR)
    ! qdlo(:) = real(scdlo(:), kind=KHPR)

    qdlonh  = deg2rad(real(dlon, kind=KHPR)) / 2.0_KQPL
    qtdlath = _SIN(qdla) / (1.0_KQPL + _COS(qdla))
#endif

101 format('agmp:in ',  F11.7, 1x, ES9.1, 2x, F11.7, 1x, ES9.1)
109 format('agmp:dev ', I0, 2(1x, ES23.15))
111 format('agmp:lon ', I0, 1x, ES23.15, 2(1x, ES23.15))
112 format('agmp:dlat ', I0, 1x, 2(1x, ES23.15))

#if OPT_REAL_QUADRUPLE_DIGITS > 0
102 format('agmp:area ', I0, 1x, ES23.15, 1x, ES16.8, 1x, ES16.8, 1x, ES16.8)
103 format('agmp:eq   ', I0, 1x, ES23.15, 1x, ES16.8, 1x, 2(1x, ES23.15))
122 format('agmp:e:area ', I0, 1x, ES23.15, 1x, ES16.8, 1x, ES16.8, 1x, ES16.8)
132 format('agmp:d:area ', I0, 1x, ES23.15, 1x, ES16.8, 1x, ES16.8, 1x, ES16.8)
#else
102 format('agmp:area ', I0, 1x, ES23.15, 1x, ES16.8, 1x, ES16.8)
103 format('agmp:eq   ', I0, 1x, ES23.15, 1x, 2(1x, ES23.15))
122 format('agmp:e:area ', I0, 1x, ES23.15, 1x, ES16.8, 1x, ES16.8)
132 format('agmp:d:area ', I0, 1x, ES23.15, 1x, ES16.8, 1x, ES16.8)
#endif
    write(*, 101) xrlat, xdlat, reflon, dlon
    write(*, 109) 1, tdlath, dlonh
    write(*, 109) 1, xtdlath, xdlonh
#if OPT_REAL_QUADRUPLE_DIGITS > 0
    write(*, 109) 2, qtdlath, qdlonh
#endif
    write(*, 111) 1, dlonh,  scdlo(1), scdlo(2)
    ! write(*, 111) 1, dlonh,  sub_angle(sclon2, sclon1)
#if OPT_REAL_QUADRUPLE_DIGITS > 0
    write(*, 111) 2, qdlonh, qdlo(1), qdlo(2)
#endif
    write(*, 112) 1, scdla(1), scdla(2)
#if OPT_REAL_QUADRUPLE_DIGITS > 0
    write(*, 112) 2, qdla(1),  qdla(2)
#endif

#if OPT_REAL_QUADRUPLE_DIGITS > 0
    ! qag = geodesic_dazim(qlat1, qlat2, qdlo)
    ! ! qap = _SIN(qref) * real(deg2rad(dlon), kind=KHPR)
    ! qap = _SIN(qlat1) * deg2rad(real(dlon, kind=KHPR))
    ! qad = qag - qap
    ! qaref = qad
    ! write(*, 103) 2, qad, (qad - qaref) / qaref, qag, qap

    qag = ATAN((_SIN(qlat1) + _COS(qlat1) * qtdlath) * tan(qdlonh)) * 2.0_KQPL
    qap = _SIN(qlat1) * deg2rad(real(dlon, kind=KHPR))
    qad = qag - qap

    qaref = qad
    write(*, 103) 2, qad, (qad - qaref) / qaref, qag, qap
    ! write(*, *) _SIN(qref), _COS(qref), _COS(qref) * qtdlath
    ! write(*, *) 'qtdlath', qtdlath, _SIN(qref) + _COS(qref) * qtdlath
    ! write(*, *) qag
    ! write(*, *) _SIN(qref) + _COS(qref) * qtdlath
    ! write(*, *) tan(qdlonh), _SIN(qdlo) / (1.0_KQPL + _COS(qdlo))
    ! write(*, *) qap, _SIN(qref), real(deg2rad(dlon), kind=KHPR), deg2rad(real(dlon, kind=KHPR))
    ! write(*, *) atan2(0.0_KHPR,-1.0_KHPR)
    ! write(*, *) atan2(0.0_KHPR,-1.0_KHPR) * real((dlon/180.0_KTGT), kind=KHPR)
    ! write(*, *) real(dlon, kind=KHPR), real(dlon, kind=KHPR) * atan2(0.0_KHPR,-1.0_KHPR) / 180.0_KHPR
#endif
    ag = geodesic_dazim(sclat1, sclat2, scdlo)
    ap = _SIN(sclat1) * (dlonh * 2.0_KTGT)
    ad = ag - ap
#if OPT_REAL_QUADRUPLE_DIGITS > 0
    write(*, 103) 1, ad, (ad - qaref) / qaref, ag, ap
#else
    write(*, 103) 1, ad, ag, ap
#endif
    ag = ATAN((_SIN(sclat1) + _COS(sclat1) * tdlath) * tan(dlonh)) * 2.0_KTGT
    ap = _SIN(sclat1) * (dlonh * 2.0_KTGT)
    ad = ag - ap
#if OPT_REAL_QUADRUPLE_DIGITS > 0
    write(*, 103) 1, ad, (ad - qaref) / qaref, ag, ap
#else
    write(*, 103) 1, ad, ag, ap
#endif

    ag = ATAN((_SIN(sclat1) + _COS(sclat1) * xtdlath) * tan(xdlonh)) * 2.0_KTGT
    ap = _SIN(sclat1) * (xdlonh * 2.0_KTGT)
    ad = ag - ap
#if OPT_REAL_QUADRUPLE_DIGITS > 0
    write(*, 103) 1, ad, (ad - qaref) / qaref, ag, ap
    ! write(*, *) ap, _SIN(scref), deg2rad(dlon)
    do jo = 8, 12
       nt = agmpc_table_size(jo)
       call agmpc_gen_table(ierr, Cc, jo, _SIN(sclat1), .TRUE.)
       ! do jt = 0, nt - 1
       !    write(*, *) 'CC', jo, jt, Cc(jt)
       ! enddo
       ax = agmpc_area_core(_COS(sclat1), tdlath, dlonh, Cc, jo)
       write(*, 102) jo, ax, (ax - qaref) / qaref, ad - ax, (ad - ax) / ax
       ax = agmpc_area_core(_COS(sclat1), xtdlath, xdlonh, Cc, jo)
       write(*, 102) jo, ax, (ax - qaref) / qaref, ad - ax, (ad - ax) / ax
    enddo
    do jo = 4, 10
       ! nt = agmpd_table_size(jo)
       call agmpd_gen_table(ierr, Cd, jo, _SIN(sclat1), .TRUE.)
       ! do jt = 0, nt - 1
       !    write(*, *) 'CC', jo, jt, Cc(jt)
       ! enddo
       ax = agmpd_area_core(_COS(sclat1), tdlath, dlonh, Cd, jo)
       write(*, 132) jo, ax, (ax - qaref) / qaref, ad - ax, (ad - ax) / ax
       ax = agmpd_area_core(_COS(sclat1), xtdlath, xdlonh, Cd, jo)
       write(*, 132) jo, ax, (ax - qaref) / qaref, ad - ax, (ad - ax) / ax
    enddo

    tdhsq = tdlath ** 2
    xtdhsq = xtdlath ** 2
    ! do jo = 2, 5
    ! write(*, *) dlonh, tdlath, tdhsq
    do jo = 2, 4
       call agmpe_gen_table(ierr, Ce, jo, dlonh, tdhsq, .TRUE.)
       ax = agmpe_area_core(_SIN(sclat1), _COS(sclat1), tdlath, Ce, jo)
       write(*, 122) jo, ax, (ax - qaref) / qaref, ad - ax, (ad - ax) / ax

       call agmpe_gen_table(ierr, Ce, jo, xdlonh, xtdhsq, .TRUE.)
       ax = agmpe_area_core(_SIN(sclat1), _COS(sclat1), xtdlath, Ce, jo)
       write(*, 122) jo, ax, (ax - qaref) / qaref, ad - ax, (ad - ax) / ax
    enddo
#endif /* OPT_REAL_QUADRUPLE_DIGITS > 0 */

  end subroutine test_agmp

!!!_ + test_hpangle
  subroutine batch_test_hpangle(ierr)
    use TOUZA_Std,only: get_nparam, get_param
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr
    real(kind=KTGT) :: dlat, reflat
    real(kind=KTGT) :: xbase
    integer :: xrange(2)
    integer narg, jarg
    integer j
    integer xbgn, xend

    ierr = 0
    narg = get_nparam()
    call get_option(ierr, xrange, 'range', 0)
    call get_option(ierr, xbase,  'base', 0.0_KTGT)
    xbgn = 0
    if (ierr.eq.0) then
       xrange(1) = max(0, xrange(1))
       if (xrange(2).eq.0) then
          xrange(2) = xrange(1)
       else if (xrange(2).lt.0) then
          xrange(2) = 7
       endif
       xbgn = xrange(1)
       xend = max(xrange(1), xrange(2))
       if (xbase.le.0.0_KTGT) xbase = 10.0_KTGT
    endif
    do jarg = 1, narg
       if (ierr.eq.0) call get_param(ierr, reflat, jarg)
       if (ierr.eq.0) then
          dlat = 1.0_KTGT
          do j = 0, xbgn - 1
             dlat = dlat / xbase
          enddo
          do j = xbgn, xend
             call test_hpangle(ierr, reflat, dlat)
             dlat = dlat / xbase
          enddo
       endif
    enddo
    return
  end subroutine batch_test_hpangle

  subroutine test_hpangle(ierr, ref, d)
    implicit none
    integer,parameter :: KTGT=KDBL
#if OPT_REAL_QUADRUPLE_DIGITS > 0
    integer,parameter :: KHPR=KQPL
#else
    integer,parameter :: KHPR=KTGT
#endif
    integer,intent(out) :: ierr
    real(kind=KTGT),intent(in) :: ref, d
    real(kind=KTGT) :: scref(2), sctgt(2)
    real(kind=KTGT) :: scdlp(2), scdhp(2), scd(2)
    real(kind=KTGT) :: scdnl(2), scdnh(2)
    real(kind=KTGT) :: scdn1(2), scdn2(2)
    real(kind=KHPR) :: qscd(2)
    ierr = 0

    scref(:) = setd_sincos(ref)
    sctgt(:) = setd_sincos(ref + d)
    scd(:)   = setd_sincos(d)
    qscd(:)  = setd_sincos(real(d,kind=KHPR))
    scdn1(:) = setd_sincos(nearest(d, +1.0_KTGT))
    scdn2(:) = setd_sincos(nearest(d, -1.0_KTGT))

    scdlp(:) = sub_angle(sctgt, scref)
    scdhp(:) = hpsub_angle(sctgt, scref)

    scdnl(:) = nml_sincos(_SIN(scdlp), _COS(scdlp))
    scdnh(:) = nml_sincos(_SIN(scdhp), _COS(scdhp))

101 format('hpangle:', ES16.9, 1x, ES16.9)
102 format('hpsub:', A, ': ', 2(1x, ES23.15), 2(1x, ES10.3), 2(1x, ES10.3))
    write(*, 101) ref, d
    write(*, 102) 'ref', scref
    write(*, 102) 'tgt', sctgt
    write(*, 102) 'sol', scd
    write(*, 102) 'qsl', qscd
    write(*, 102) 'nd1', scdn1
    write(*, 102) 'nd2', scdn2
    write(*, 102) 'lpr', scdlp, (scdlp-scd)/scd, (scdlp-scd)
    write(*, 102) 'hpr', scdhp, (scdhp-scd)/scd, (scdhp-scd)
    write(*, 102) 'nlp', scdnl, (scdnl-scd)/scd, (scdnl-scd)
    write(*, 102) 'nhp', scdnh, (scdnh-scd)/scd, (scdnh-scd)

  end subroutine test_hpangle

  subroutine batch_test_stp(ierr, stp)
    use TOUZA_Std,only: get_nparam, get_param
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr
    integer,intent(in)  :: stp
    real(kind=KTGT) :: plon, plat, olat
    real(kind=KTGT) :: dlo,  dla,  dxlon
    real(kind=KTGT) :: targ(NGEOG)
    real(kind=KTGT),parameter :: CSPAN = 360.0_KTGT
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: def = -HUGE(ZERO)

    real(kind=KTGT) :: lonp
    real(kind=KTGT) :: latd, lond
    real(kind=KTGT) :: lattr(NTRIG), lontr(NTRIG)

    real(kind=KTGT) :: csco(ncache_stp_co)
    real(kind=KTGT) :: wg(NGEOG), zg(NGEOG)

    integer pole
    integer narg, jarg
    integer j
    integer nla, nlo, jla, jlo
    integer nxlo, nxla

    ierr = 0
    narg = get_nparam()
    if (ierr.eq.0) call get_option(ierr, olat, 'op', def)
    if (ierr.eq.0) then
       if (olat.eq.def) olat = -90.0_KTGT
    endif
    if (ierr.eq.0) call get_option(ierr, targ(1:NGEOG), 'wp', def)
    if (ierr.eq.0) then
       plat = targ(JLATI)
       plon = targ(JLONGI)
       if (plat.eq.def) plat = 62.7328244743782_KTGT
       if (plon.eq.def) plon = 60.0_KTGT
       plat = - SIGN(plat, olat)
    endif
    if (ierr.eq.0) call get_option(ierr, dlo, 'dlo', ZERO)
    if (ierr.eq.0) call get_option(ierr, dla, 'dla', ZERO)
    if (ierr.eq.0) call get_option(ierr, dxlon, 'dxlo', ZERO)
    if (ierr.eq.0) then
       if (dlo.eq.ZERO) dlo = 5.0_KTGT
       if (dla.eq.ZERO) dla = 5.0_KTGT
       if (dxlon.eq.ZERO) dxlon = 10.0_KTGT
       dla = SIGN(dla, olat)
    endif
    write(*, *) olat, plon, plat, dlo, dla, dxlon
    if (ierr.eq.0) then
       if (olat.le.0) then
          pole = +1
       else
          pole = -1
       endif
       call stp_set(ierr, csco, plat, plon, pole, loround=CSPAN, laround=CSPAN)
       write(*, *) 'CSCO = ', csco
    endif

    if (ierr.eq.0) then
       nla = FLOOR(ABS((-olat - plat) / dla))
       nlo = FLOOR(CSPAN / dlo)
       do jla = 0, nla
          latd = - olat + DLA * real(jla, kind=KTGT)
          lattr(:) = setd_sincos(latd)
          do jlo = 0, nlo
             lond = DLO * real(jlo, kind=KTGT)
             lonp = modulo(lond + plon, CSPAN)
             lontr(:) = setd_sincos(lond)
             wg(:) = stp_fwd_tr(lontr, lattr, csco)
             write(*, *) 'ZW:', lonp, latd, wg(JLONGI), wg(JLATI)
          enddo
       enddo
    endif
    if (ierr.eq.0) then
       nxlo = FLOOR(CSPAN / (dxlon * 2.0_KTGT))
       nxla = nlo / 2
       do jlo = 0, nxlo
          lond = -90.0_KTGT + dxlon * (real(jlo, kind=KTGT) + 0.0_KTGT)
          lontr(:) = setd_sincos(lond)
          do jla = 0, nxla
             latd = -90.0_KTGT + dlo * (real(jla, kind=KTGT) + 0.0_KTGT)
             lattr(:) = setd_sincos(latd)
             zg(:) = stp_bwd_tr(lontr, lattr, csco)
             ! lonp = modulo(zg(JLONGI) + plon, CSPAN)
             lonp = zg(JLONGI)
             write(*, *) 'WZ:', lond, latd, lonp, zg(JLATI)
          enddo
       enddo
    endif

    return
  end subroutine batch_test_stp

end program test_emu_ugg

#endif /* TEST_EMU_UGG */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

!!!_! ami_table.F90 - TOUZA/Ami/table amida-coupler table procedures
! Maintainer: SAITO Fuyuki
! Created: May 2 2022
#define TIME_STAMP 'Time-stamp: <2024/07/06 21:26:18 fuyuki ami_table.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022, 2023, 2024
!           Japan Agency for Marine-Earth Science and Technology
!
!!!_* include
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_ami.h"
!!!_* macros
#ifndef   TEST_AMI_TABLE
#  define TEST_AMI_TABLE 0
#endif
#ifndef   OPT_AMI_SUB_ANGLE
#  define OPT_AMI_SUB_ANGLE 1  /* hpsub variation */
#endif
#ifndef   OPT_AMI_AGMP_ORDER
#  define OPT_AMI_AGMP_ORDER 8  /* positive to introduce agmp procedures */
#endif
#ifndef   OPT_AMI_AGMP_METHOD
#  define OPT_AMI_AGMP_METHOD 1 /* design d */
#endif

! #define DEBUG_AMI_SYMM 1
#ifndef DEBUG_AMI_SYMM
#define DEBUG_AMI_SYMM 0
#endif

#if OPT_AMI_SUB_ANGLE
#  define _SUB_ANGLE(A,B) hpsub_angle(A,B)
#  define _SUB_SIN(A,B) hpsub_sin(A,B)
#else
#  define _SUB_ANGLE(A,B) sub_angle(A,B)
#  define _SUB_SIN(A,B) sub_sin(A,B)
#endif
!!!_@ TOUZA_Ami_table - ami-da utilities
module TOUZA_Ami_table
!!!_ + modules
  use TOUZA_Ami_std, as_init=>init, as_diag=>diag, as_finalize=>finalize
  use TOUZA_Emu,only: ncache_psgp_la, ncache_psgp_lo
  use TOUZA_Emu,only: NTRIG, NGEOG, JSIN, JCOS, JLATI, JLONGI
  use TOUZA_Emu,only: phase, rad2deg
!!!_ + default
  implicit none
  private
!!!_ + parameter
  integer,parameter,public :: ps2g_auto_init = -1
  integer,parameter,public :: ps2g_end_switch = -1
  integer,parameter,public :: ps2g_no_switch = +HUGE(0)
  integer,parameter,public :: ps2g_fast_fallback = ps2g_no_switch - 1

  integer,parameter,public :: ps2g_weights = 5
  integer,parameter,public :: ps2g_w0 = 0          ! weights for constant (== first-order weights)
  integer,parameter,public :: ps2g_w1fla = 1       ! weights for first-order derivative (lat, fwd)
  integer,parameter,public :: ps2g_w1flo = 2       !   (== second-order weights)
  integer,parameter,public :: ps2g_w1bla = 3       ! weights for first-order derivative (lat, bwd)
  integer,parameter,public :: ps2g_w1blo = 4
!!!_  - various properties
  integer,parameter :: mcla = ncache_psgp_la
  integer,parameter :: mclo = ncache_psgp_lo

  integer,parameter :: prop_xidx = 1
  integer,parameter :: prop_yidx = 2
  integer,parameter :: prop_lev  = 3
  integer,parameter :: stack_nprop = 3

  integer,parameter :: prop_gtx    = 1   ! geographic tile, x
  integer,parameter :: prop_gty    = 2   ! geographic tile, y
  integer,parameter :: prop_gtlo   = 1   ! geographic tile, lon
  integer,parameter :: prop_gtla   = 2   ! geographic tile, lat
  integer,parameter :: prop_gtlev  = 3   ! geographic tile, level
  integer,parameter :: prop_psg_x1 = 4   ! doubled-index (x) on polar stereographic, geographic node 1
  integer,parameter :: prop_psg_x2 = 5   !
  integer,parameter :: prop_psg_x3 = 6   !
  integer,parameter :: prop_psg_x4 = 7   !
  integer,parameter :: prop_psg_y1 = 8   ! doubled-index (x) on polar stereographic, geographic node 2
  integer,parameter :: prop_psg_y2 = 9   !
  integer,parameter :: prop_psg_y3 = 10  !
  integer,parameter :: prop_psg_y4 = 11  !

  integer,parameter :: prop_psg_size = 11

  integer,parameter :: stt_error      = -1
  integer,parameter :: stt_same       = 0   ! nodes are in the same cell
  integer,parameter :: stt_adjacent_x = 1   ! nodes are in the neighbouring cells (in 2x1, share y-side)
  integer,parameter :: stt_adjacent_y = 2   ! nodes are in the neighbouring cells (in 1x2, share x-side)
  integer,parameter :: stt_adjacent_d = 3   ! nodes are in the neighbouring cells (in 2x2, share corner)
  integer,parameter :: stt_discont    = 4

  integer,parameter :: stt_adjacent_dx = 5
  integer,parameter :: stt_adjacent_dy = 6

  integer,parameter :: lxy    = 2
  integer,parameter :: coor_x = 1
  integer,parameter :: coor_y = 2

  integer,parameter :: lrange = 2             ! active range table
  integer,parameter :: rmin = 1, rmax = 2

  integer,parameter :: side_default  = 0
  integer,parameter :: side_parallel = 1
  integer,parameter :: side_meridian = 2

  integer,parameter :: div_in_two = -1
  integer,parameter :: no_division = -2

  integer,parameter :: node_0   = 0
  integer,parameter :: node_1   = 1, node_2   = 2,  node_3   = 3,  node_4   = 4
  integer,parameter :: node_xi  = 5, node_xo  = 6,  node_yi  = 7,  node_yo  = 8
  integer,parameter :: lnodes   = 8

  integer,parameter :: connect_err = -1
  integer,parameter :: connect_bent = 0
  integer,parameter :: connect_straight = 1

  integer,parameter :: accum_w0   = ps2g_w0
  integer,parameter :: accum_w1la = ps2g_w1fla
  integer,parameter :: accum_w1lo = ps2g_w1flo
  integer,parameter :: accum_num  = 3

  integer,parameter :: latcell_normal = 0
  integer,parameter :: latcell_upb  = 1
  integer,parameter :: latcell_lowb = 2

  !   +c: along y, to the central longitude         (NP: -y  SP: +y)
  !   -c: along y, against the central longitude    (NP: +y  SP: -y)
  !                                       ! origin
  !                                         - axis
  !                                         - perpendicular
  integer,parameter :: oct_px_mc = 0      ! +x -c       90
  integer,parameter :: oct_mc_px = 1      ! -c +x      135
  integer,parameter :: oct_mc_mx = 2      ! -c -x      180
  integer,parameter :: oct_mx_mc = 3      ! -x -c      225
  integer,parameter :: oct_mx_pc = 4      ! -x +c      270
  integer,parameter :: oct_pc_mx = 5      ! +c -x      315
  integer,parameter :: oct_pc_px = 6      ! +c +x        0
  integer,parameter :: oct_px_pc = 7      ! +x +c       45

  integer,parameter :: locts = 8

  integer,parameter :: symsw_negx = 16
  integer,parameter :: symsw_negy = 32
  integer,parameter :: symsw_swap = 64    ! after negx/negy

  integer,parameter :: sym_none = 0
  integer,parameter :: sym_x    = 1    ! symmetric along x=0
  integer,parameter :: sym_y    = 2    ! symmetric along y=0
  integer,parameter :: sym_pd   = 4    ! symmetric along x=+y
  integer,parameter :: sym_md   = 8    ! symmetric along x=-y
  integer,parameter :: sym_xy   = sym_x + sym_y
  integer,parameter :: sym_ro   = sym_x + sym_y + sym_pd + sym_md

  integer,parameter :: zpi_lc = 1 ! convined x range
  integer,parameter :: zpi_eb = 2 ! effective range
  integer,parameter :: zpi_ee = 3 ! effective range
  integer,parameter :: zpi_ff = 4 ! flat index factor
  integer,parameter :: zpi_fo = 5 ! flat index offset

  integer,parameter :: lzpi  = 5

  integer,parameter :: zpr_co = 1
  integer,parameter :: zpr_cd = 2

  integer,parameter :: lzpr  = 2

  integer,parameter :: opi_octant = 1
  integer,parameter :: opi_lobgn  = 2
  integer,parameter :: opi_loend  = 3

  integer,parameter :: lopi  = 3

  integer,parameter :: lpi_octant = 1
  integer,parameter :: lpi_lidx = 2
  integer,parameter :: llpi = 2

!!!_ + static
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = AMI_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global

#define __MDL__ 't'
#define _ERROR(E) (E - ERR_MASK_AMI_TABLE)
!!!_  - debug
  ! logical,save :: debug_side_x = .FALSE.
  ! logical,save :: debug_side_y = .FALSE.
  ! logical,save :: debug_side_d = .FALSE.
!!!_ + type
!!!_ + interfaces
  interface symm_ps2g_map
     module procedure symm_ps2g_map_d
  end interface symm_ps2g_map

  interface div_ps2g_map
     module procedure div_ps2g_map_d
  end interface div_ps2g_map
  interface ps2g_store_fwd
     module procedure ps2g_store_fwd_d
  end interface ps2g_store_fwd
  interface div_ps2g_cell
     module procedure div_ps2g_cell_d
  end interface div_ps2g_cell
  interface div_ps2g_neighbor
     module procedure div_ps2g_neighbor_d
  end interface div_ps2g_neighbor
  interface div_ps2g_advance
     module procedure div_ps2g_advance_d
  end interface div_ps2g_advance
  interface div_ps2g_fallback
     module procedure div_ps2g_fallback_d
  end interface div_ps2g_fallback

  interface div_ps2g_simple_fallback
     module procedure div_ps2g_simple_fallback_d
  end interface div_ps2g_simple_fallback
  interface ps2g_simple_fallback
     module procedure ps2g_simple_fallback_d
  end interface ps2g_simple_fallback

  interface ps2g_parallel_intersection
     module procedure ps2g_parallel_intersection_d
  end interface ps2g_parallel_intersection
  interface ps2g_area_bent
     module procedure ps2g_area_bent_d
  end interface ps2g_area_bent
  interface ps2g_area_straight
     module procedure ps2g_area_straight_d
  end interface ps2g_area_straight
  interface ps2g_neighbor_collect
     module procedure ps2g_neighbor_collect_d
  end interface ps2g_neighbor_collect
  interface ps2g_invert_table
     module procedure ps2g_invert_table_d
  end interface ps2g_invert_table
  interface ps2g_meridian_intersection
     module procedure ps2g_meridian_intersection_d
  end interface ps2g_meridian_intersection

  interface set_tolerance
     module procedure set_tolerance_d
  end interface set_tolerance
  interface set_coor_anchor
     module procedure set_coor_anchor_d
  end interface set_coor_anchor
  interface set_gslat_node
     module procedure set_gslat_node_d
  end interface set_gslat_node
  interface set_grlon_node
     module procedure set_grlon_node_d
  end interface set_grlon_node
  interface is_seg_intersect
     module procedure is_seg_intersect_d
  end interface is_seg_intersect
  interface set_seg_lat_cache
     module procedure set_seg_lat_cache_d
  end interface set_seg_lat_cache
  interface set_seg_rlon_cache
     module procedure set_seg_rlon_cache_d
  end interface set_seg_rlon_cache

  interface coeff2_line_integ
     module procedure coeff2_line_integ_d
  end interface coeff2_line_integ
  interface coeff3_line_integ
     module procedure coeff3_line_integ_d
  end interface coeff3_line_integ

  interface segment_rel_sinlat
     module procedure segment_rel_sinlat_d
  end interface segment_rel_sinlat
  interface segment_rel_lon
     module procedure segment_rel_lon_d
  end interface segment_rel_lon

  interface octant_dist_table
     module procedure octant_dist_table_d
  end interface octant_dist_table
  interface octant_settle_table
     module procedure octant_settle_table_d
  end interface octant_settle_table

  interface octant_gen_table
     module procedure octant_gen_table_d
  end interface octant_gen_table
  interface octant_dec_table
     module procedure octant_dec_table_d
  end interface octant_dec_table
  interface octant_gen_group
     module procedure octant_gen_group_d
  end interface octant_gen_group
  interface octant_wedge_lonseq
     module procedure octant_wedge_lonseq_d
  end interface octant_wedge_lonseq
  interface octant_fill_table
     module procedure octant_fill_table_d
  end interface octant_fill_table
  interface octant_normalize_lon
     module procedure octant_normalize_lon_d
  end interface octant_normalize_lon

  interface get_octant_ndi
     module procedure get_octant_ndi_d
  end interface get_octant_ndi

  interface set_octant_ends
     module procedure set_octant_ends_d
  end interface set_octant_ends
  interface set_zone_props
     module procedure set_zone_props_d
  end interface set_zone_props
  interface set_zone_range
     module procedure set_zone_range_d
  end interface set_zone_range
  interface set_zone_range_core
     module procedure set_zone_range_core_d
  end interface set_zone_range_core
  interface set_zone_coeffs
     module procedure set_zone_coeffs_d
  end interface set_zone_coeffs

  interface adjust_zone_range
     module procedure adjust_zone_range_d
  end interface adjust_zone_range
  interface is_symmetric_plane
     module procedure is_symmetric_plane_d
  end interface is_symmetric_plane
  interface symmetric_coor
     module procedure symmetric_coor_d
  end interface symmetric_coor

  interface nsort4
     module procedure nsort4_d
  end interface nsort4
  interface parallel_dlongi
     module procedure parallel_dlongi_d
  end interface parallel_dlongi

  interface octant_zone
     module procedure octant_zone_d
  end interface octant_zone
  interface octant_lon
     module procedure octant_lon_d
  end interface octant_lon

  interface debug_dump_cell
     module procedure debug_dump_cell_d
  end interface debug_dump_cell
!!!_ + public procedures
  public init, diag, finalize
  public cyclic_interp_table
  public geo_interp_table
  public symm_ps2g_map
  public ps2g_invert_table
  public is_symmetric_plane, set_symmetric_zone

!!!_ + function-like macros
!!!_ + procedures
contains
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    integer,intent(in),optional :: icomm
    integer lv, md, lmd

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
          if (ierr.eq.0) call as_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
       endif
       if (is_first_force(init_counts, mode)) then
          continue
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
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
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __MDL__, utmp)
             if (is_msglev_normal(lv)) then
                call msg('(''sub_angle choice '', I0)', OPT_AMI_SUB_ANGLE, __MDL__, utmp)
                call msg('(''agmp table size '', I0)', OPT_AMI_AGMP_ORDER, __MDL__, utmp)
                call msg('(''agmp method '', I0)', OPT_AMI_AGMP_METHOD, __MDL__, utmp)
             endif
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call as_diag(ierr, utmp, levv=lv, mode=lmd)
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
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call as_finalize(ierr, utmp, levv=lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
  end subroutine finalize
!!!_  - geo_interp_table - create geographic (lon-lat) interpolation table
  ! Notes:
  !   array-shape adjustment (e.g., halo, dummy margin) should be done by caller-side
  subroutine geo_interp_table &
       & (ierr, &
       &  gtbl_factor, gtbl_index,  mtbl, &
       &  lon_dst, lat_dst, nlon_d, nlat_d, &
       &  lon_src, lat_src, nlon_s, nlat_s, &
       &  lgdst,   ltbl)
    use TOUZA_Emu_ugg,only: check_monotonic, non_monotonic
    implicit none
    integer,        intent(out) :: ierr
    integer,        intent(in)  :: lgdst
    real(kind=KDBL),intent(out) :: gtbl_factor(lgdst, *) !! interpolation factor table
    integer,        intent(out) :: gtbl_index (lgdst, *) !! interpolation index table
    integer,        intent(out) :: mtbl
    real(kind=KDBL),intent(in)  :: lon_dst(*), lat_dst(*)  !! cell boundary coordinates, need +1 members
    real(kind=KDBL),intent(in)  :: lon_src(*), lat_src(*)
    integer,        intent(in)  :: nlon_d,     nlat_d
    integer,        intent(in)  :: nlon_s,     nlat_s
    integer,        intent(in)  :: ltbl

    real(kind=KDBL) :: flon(nlon_d, ltbl), flat(nlat_d, ltbl)
    integer         :: ilon(nlon_d, ltbl), ilat(nlat_d, ltbl)

    real(kind=KDBL),parameter :: ZERO = 0.0_KDBL

    real(kind=KDBL) :: lonwd, loned
    real(kind=KDBL) :: lonws, lones

    integer mtx, mty
    integer jxoff
    integer jxd, jt, ji
    integer jxs

    ierr = 0

    gtbl_factor(1:lgdst, 1:ltbl) = ZERO
    gtbl_index (1:lgdst, 1:ltbl) = 0

    return
  end subroutine geo_interp_table

!!!_  - cyclic_interp_table - create 1d interpolation table (cyclic)
  subroutine cyclic_interp_table &
       & (ierr, &
       &  tbl_factor, tbl_index,  mtbl, &
       &  xdst,       ndst, &
       &  xsrc,       nsrc, &
       &  ldim,       ltbl)
    use TOUZA_Emu_ugg,only: check_monotonic, non_monotonic
    implicit none
    integer,        intent(out) :: ierr
    integer,        intent(in)  :: ldim
    real(kind=KDBL),intent(out) :: tbl_factor(ldim, *) !! interpolation factor table
    integer,        intent(out) :: tbl_index (ldim, *) !! interpolation index table
    integer,        intent(out) :: mtbl
    real(kind=KDBL),intent(in)  :: xdst(*) !! cell boundary coordinates, need +1 members
    real(kind=KDBL),intent(in)  :: xsrc(*)
    integer,        intent(in)  :: ndst
    integer,        intent(in)  :: nsrc
    integer,        intent(in)  :: ltbl

    !  boundaries xdst[i] = x(i-1/2)

    real(kind=KDBL),parameter :: ZERO = 0.0_KDBL

    real(kind=KDBL) :: xld, xhd, dx
    real(kind=KDBL) :: xls, xhs
    real(kind=KDBL) :: f

    integer idird, idirs

    integer jxoff, ji
    integer jxd,   jtd
    integer jxs,   jts

    ierr = 0

    mtbl = 0
    tbl_factor(1:ldim, 1:ltbl) = ZERO
    tbl_index (1:ldim, 1:ltbl) = 1

    ! health-check
    if (ierr.eq.0) then
       idird = check_monotonic(xdst, ndst+1)
       idirs = check_monotonic(xsrc, nsrc+1)
       ! TO DO: monotonic decreasing coordinates
       if (idird.le.non_monotonic .or. idirs.le.non_monotonic) ierr = -1
    endif
    if (ierr.eq.0) then
       jxoff = 0
       do jxd = ndst + 1, 1, -1
          if (xdst(jxd).gt.xsrc(1)) jxoff = jxd
       enddo
       if (jxoff.eq.0) ierr = -1 ! panic
    endif
    if (ierr.eq.0) then
       jts = 1
       loop_out: do jtd = 0, ndst - 1
          jxd = jxoff + jtd
          ji = 0
          if (jxd.gt.ndst) then
             jxd = jxd - ndst
             xld = xdst(jxd)   - xdst(1) + xdst(ndst+1)
             xhd = xdst(jxd+1) - xdst(1) + xdst(ndst+1)
          else
             xld = xdst(jxd)
             xhd = xdst(jxd+1)
          endif
          ! dx = xdst(jxd+1) - xsrc(jxd)
          ! for compatibility
          dx = xhd - xld
          do
             if (jts.gt.nsrc) then
                jxs = jts - nsrc
                xls = xsrc(jxs)   - xsrc(1) + xsrc(nsrc+1)
                xhs = xsrc(jxs+1) - xsrc(1) + xsrc(nsrc+1)
             else
                jxs = jts
                xls = xsrc(jxs)
                xhs = xsrc(jxs+1)
             endif
             f = (MIN(xhd, xhs) - MAX(xld, xls)) / dx
             ! write(*,*) jxd, xld, xhd, ji, jxs, xls, xhs, dx, f
             if (f .gt. ZERO) THEN
                ji = ji + 1
                if (ji .gt. ltbl) THEN
                   ierr = -1
                   exit loop_out
                endif
                tbl_index(jxd, ji) = jxs
                tbl_factor(jxd, ji) = f
                mtbl = max(ji, mtbl)
             endif
             IF (xhd .le. xhs) exit
             jts = jts + 1
          enddo
       end do loop_out
    endif
    if (ierr.eq.0) then
       if (TEST_AMI_TABLE.gt.0) then
 101       format('ICYCLE: ', I0, 1x, F9.3, 1x, I0, 1x, 10(1x, I9))
 102       format('FCYCLE: ', I0, 1x, F9.3, 1x, I0, 1x, 10(1x, F9.3))
           do jxd = 1, ndst
              write(*, 101) jxd, xdst(jxd), mtbl, (tbl_index(jxd, ji), ji = 1, mtbl)
              write(*, 102) jxd, xdst(jxd), mtbl, (tbl_factor(jxd, ji), ji = 1, mtbl)
           enddo
        endif
     endif

    return
  end subroutine cyclic_interp_table

!!!_  - [ps2g]
!!!_   & symm_ps2g_map - axi-symmetric ps2g mapping
  subroutine symm_ps2g_map_d &
       & (ierr,   iofs,    iprj,    wprj,  &
       &  nmem,   lmem,    &
       &  latn,   wlat,    jlatb,   jlate, mlat,   &
       &  lonn,   dlon,    jlonb,   jlone, mlon,   &
       &  xl,     dx,      mx,      yl,    dy,     my,   &
       &  cco,    &
       &  lonlev, latlev,  inilev,  swlev, reqlev, tol,  &
       &  deg,    u,       levv,    tag,   udump)
    use TOUZA_Ami_std,only: compact_string
    use TOUZA_Emu_ugg,only: check_monotonic, ncache_psgp_co
    use TOUZA_Emu_ugg,only: psgp_inquire
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: lmem                   ! limit size of iprj wprj  (negative to skip check)
    integer,         intent(out) :: iofs(0:*)              ! [(lo,la)+1] to subscript-vector offset (lv)
    integer,         intent(out) :: iprj(0:*)              ! [lv] to g(xy)-index
    real(kind=KTGT), intent(out) :: wprj(0:ps2g_weights-1, 0:*)    ! [lv] to weights
    integer,         intent(out) :: nmem                   ! wprj active members
    real(kind=KTGT), intent(in)  :: lonn(0:*), latn(0:*)   ! node longitude [0:mlon] and latitude [0:mlat] (deg/rad)
    real(kind=KTGT), intent(in)  :: dlon(0:*), wlat(0:*)   ! d lon(deg/rad); [d sin(lat)]
    integer,         intent(in)  :: jlonb, jlatb, mlat     ! nlon: jlone - jlonb, target span
    integer,         intent(in)  :: jlone, jlate, mlon     ! mlon: full (logical) span
    real(kind=KTGT), intent(in)  :: cco(*)                 ! ps2g cache (common)
    real(kind=KTGT), intent(in)  :: xl, dx
    real(kind=KTGT), intent(in)  :: yl, dy
    integer,         intent(in)  :: mx, my
    integer,         intent(in)  :: lonlev, latlev, inilev, swlev
    integer,         intent(out) :: reqlev                 ! required level limit
    real(kind=KTGT), intent(in)  :: tol
    logical,         intent(in),optional :: deg(:)         ! degree if true, radian otherwise
    integer,         intent(in),optional :: u, levv
    integer,         intent(in),optional :: udump
    character(len=*),intent(in),optional :: tag
    integer nlon, nlat

    integer ksymm
    integer symmz(0:locts-1)
    integer         :: zpropi(lzpi, 0:locts-1, lxy)
    real(kind=KTGT) :: zpropr(lzpr, 0:locts-1, lxy)

    integer :: joct, mocts
    integer :: opropi(lopi, 0:locts)

    integer dir_lon

    integer,parameter :: iniskip = 0
    integer,parameter :: nwedge = 0
    integer :: wpos(0:1)

    real(kind=KTGT) :: yh
    real(kind=KTGT) :: xl_oct, yl_oct
    real(kind=KTGT) :: dx_oct, dy_oct
    integer         :: mx_oct, my_oct
    integer,allocatable :: iofs_oct(:)
    integer,allocatable :: iprj_oct(:)
    real(kind=KTGT),allocatable :: wprj_oct(:, :)
    integer :: itblo(0:locts), wtblo(0:locts)

    integer lolon, mgh
    integer jpws, jpof
    integer nwo,  mwo, lwo

    integer ltbl
    integer lser
    integer lwgt
    integer,allocatable :: dectbl(:)
    integer,allocatable :: doct_dec(:)
    integer,allocatable :: oref_dec(:), jwgt_dec(:), wtbl_dec(:)
    real(kind=KTGT),allocatable :: lonn_dec(:), adjl_dec(:)
    real(kind=KTGT),allocatable :: lonn_ser(:), dlon_ser(:)
    integer :: ooffs(0:locts)
    real(kind=KTGT),allocatable :: lonn_grp(:), dlon_grp(:)
    integer :: ogrps(0:locts)
    integer nlono
    real(kind=KTGT) :: lospan

    character(len=128) :: txt, txt2
    integer jg, jgbgn, jgend
    integer jtbgn, jtend
    integer utmp, lv
    integer mdiag
    integer jerr

    integer kasp
    real(kind=KTGT) :: olat

    integer mdest
#if DEBUG_AMI_SYMM
    integer jdec, jdbgn, jdend
    integer jlo,  jloph
    integer jt
#endif
    ierr = 0

    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)
    mdest = mx * my

    call psgp_inquire(ierr, cco, olat=olat)
    if (olat.gt.0.0_KTGT) then
       kasp = +1
    else
       kasp = -1
    endif
    ! write(*, *) nwedge, wpos(0:nwedge)
    ! write(*, *) lonn(0:mlon)
    ! write(*, *) dlon(0:mlon-1)
    ! write(*, *) cco(1:ncache_psgp_co)
    ! write(*, *) 'aspect:', kasp

    nlat = jlate - jlatb
    nlon = jlone - jlonb
    dir_lon = check_monotonic(lonn(jlonb:jlone), nlon+1)
    if (dir_lon.le.0) then
       call msg('panic: non ascending order in longitude')
       ierr = _ERROR(ERR_PANIC)
       return
    endif

    if (is_deg(deg, JLONGI)) then
       lospan = 360.0_KTGT
    else
       lospan = 0.0_KTGT
    endif

    ! geographic domain side
    lser = nlon + locts * 2
    ltbl = nlon * locts
    lwgt = lser
    if (ierr.eq.0) then
       allocate(lonn_ser(0:lser), dlon_ser(0:lser), &
            &   lonn_grp(0:lser), dlon_grp(0:lser), wtbl_dec(0:nlon), &
            &   doct_dec(0:lser), lonn_dec(0:lser), adjl_dec(0:lser), &
            &   jwgt_dec(0:lwgt), oref_dec(0:lwgt), &
            &   dectbl(0:ltbl),   STAT=ierr)
    endif
    if (ierr.eq.0) then
       ! serialize longitude sequences
       call octant_gen_table &
            & (ierr,     dectbl,   doct_dec, lonn_dec, adjl_dec, &
            &  lonn_ser, dlon_ser, lser,     ooffs, &
            &  lonn,     dlon,     jlonb,    jlone, cco)
    endif

    ! polar stereographic plane side
    ksymm = is_symmetric_plane(xl, dx, mx, yl, dy, my)
    ierr = min(0, ksymm)
    if (ierr.eq.0) call set_symmetric_zone(ierr, symmz, ksymm, kasp)
#if DEBUG_AMI_SYMM
    write(*, *) 'symmz:0:', symmz
#endif /* DEBUG_AMI_SYMM */
    ! symmetric grouping of octant sequence
    if (ierr.eq.0) then
       ! serialize longitude sequences
       call octant_gen_group &
            & (ierr, &
            &  symmz,    &
            &  lonn_grp, dlon_grp, ogrps, &
            &  lonn_ser, dlon_ser, ooffs, &
            &  lser)
    endif
    ! settle symmetric arrangement
    if (ierr.eq.0) then
       call set_zone_props &
            & (ierr,  zpropi, zpropr, &
            &  kasp,  symmz,  xl, dx, mx, yl, dy, my)
    endif
    if (ierr.eq.0) then
       call set_octant_ends &
            & (ierr, mocts, opropi, &
            &  lonn, jlonb, jlone,  cco)
    endif
    if (ierr.eq.0) then
       ! decomposition table
       call octant_dec_table &
            & (ierr, &
            &  jwgt_dec, oref_dec, lwgt,     wtbl_dec, &
            &  doct_dec, lonn_dec, dectbl,   &
            &  lonn_ser, ooffs,    jlonb,    jlone,  symmz)
    endif
#if DEBUG_AMI_SYMM
    if (ierr.eq.0) then
1901   format('symm:grps:     ', I0, 1x, I0, 1x, I0, 1x, I0)
1902   format('symm:grps:lon: ', I0, F9.1)
1911   format('symm:sers:     ', I0, 1x, I0, 1x, I0)
1912   format('symm:sers:lon: ', I0, F9.1)
1921   format('symm:decs:     ', I0, 1x, I0, 1x, I0)
1922   format('symm:decs:lon: ', I0, F9.1, 1x, F9.1, 1x, I0, 1x, I0)
       do joct = 0, locts - 1
          jgbgn = ogrps(joct)
          jgend = ogrps(joct + 1) - 1
          write(*, 1901) joct, symmz(joct), jgbgn, jgend
          do jg = jgbgn, jgend
             write(*, 1902) jg, lonn_grp(jg)
          enddo
          jtbgn = ooffs(joct)
          jtend = ooffs(joct + 1) - 1
          write(*, 1911) joct, jtbgn, jtend
          do jt = jtbgn, jtend
             write(*, 1912) jt, lonn_ser(jt)
          enddo
       enddo
       do joct = 0, locts - 1
          jgbgn = ogrps(joct)
          jgend = ogrps(joct + 1) - 1
          write(*, 1901) joct, symmz(joct), jgbgn, jgend
          do jg = jgbgn, jgend
             write(*, 1902) jg, lonn_grp(jg)
          enddo
          jtbgn = ooffs(joct)
          jtend = ooffs(joct + 1) - 1
          write(*, 1911) joct, jtbgn, jtend
          do jt = jtbgn, jtend
             write(*, 1912) jt, lonn_ser(jt)
          enddo
       enddo
       do jloph = jlonb, jlone - 1
          jlo = jloph - jlonb
          jdbgn = wtbl_dec(jlo)
          jdend = wtbl_dec(jlo + 1)
          write(*, 1921) jlo, jdbgn, jdend
          do jdec = jdbgn, jdend - 1
             write(*, 1922) jdec, lonn_dec(jdec), adjl_dec(jdec), &
                  & doct_dec(jdec), oref_dec(jdec)
          enddo
       enddo
    endif
#endif /* DEBUG_AMI_SYMM */

    if (ierr.eq.0) then
       lolon = 0
       do joct = 0, mocts - 1
          lolon = lolon + (opropi(opi_loend, joct) - opropi(opi_lobgn, joct)) + 1
       enddo
       mgh = lolon * (jlate - jlatb)
       allocate(iofs_oct(0:mgh), STAT=ierr)
    endif
    if (ierr.eq.0) then
       lwo = max(1, lmem)
       allocate(iprj_oct(0:lwo-1), &
            &   wprj_oct(0:ps2g_weights-1, 0:lwo-1), &
            &   STAT=ierr)
       lwo = max(0, lmem)
    endif

    if (ierr.eq.0) then
       wpos(0) = -1
       wpos(1) = jlone + 1
       jpof = 0
       jpws = 0
       mwo = lwo
       nmem = 0
       itblo(0) = jpof
       wtblo(0) = nmem
       do joct = 0, locts - 1
          jgbgn = ogrps(joct)
          jgend = ogrps(joct + 1) - 1
          nlono = jgend - jgbgn

          if (is_msglev_INFO(lv)) then
201          format('symm:octant[', I0, ']:', 1x, A)
202          format('symm:octant[', I0, '=', I0, ']:', 1x, A)
203          format('symm:octant[', I0, '=', I0, ']')
             if (symmz(joct).eq.joct.and.nlono.gt.0) then
                call compact_string(jerr, txt2, lonn_grp(jgbgn), rdelim=':')
                call compact_string(jerr, txt2, lonn_grp(jgend), append=.TRUE.)
                write(txt, 201) joct, trim(txt2)
             else
                jtbgn = ooffs(joct)
                jtend = ooffs(joct + 1) - 1
                if (jtend.gt.jtbgn) then
                   call compact_string(jerr, txt2, lonn_ser(jtbgn), rdelim=':')
                   call compact_string(jerr, txt2, lonn_ser(jtend), append=.TRUE.)
                   write(txt, 202) joct, symmz(joct), trim(txt2)
                else
                   write(txt, 203) joct, symmz(joct)
                endif
             endif
             call msg(txt, u=utmp)
          endif

          if (symmz(joct).eq.joct.and.nlono.gt.0) then
             xl_oct = zpropr(zpr_co, joct, coor_x)
             yl_oct = zpropr(zpr_co, joct, coor_y)
             dx_oct = zpropr(zpr_cd, joct, coor_x)
             dy_oct = zpropr(zpr_cd, joct, coor_y)
             mx_oct = zpropi(zpi_lc, joct, coor_x)
             my_oct = zpropi(zpi_lc, joct, coor_y)
212          format(2x, I0, ': ', A)
213          format(2x, I0, ': ', A)
             if (is_msglev_DETAIL(lv)) then
                mdiag = 8
                do jg = jgbgn, jgend, mdiag
                   call join_list(jerr, txt2, lonn_grp(jg:MIN(jgend,jg+mdiag-1)), fmt=' ')
                   write(txt, 212) jg, trim(txt2)
                   call msg(txt, u=utmp)
                   call join_list(jerr, txt2, dlon_grp(jg:MIN(jgend,jg+mdiag-1)), fmt=' ')
                   write(txt, 213) jg, trim(txt2)
                   call msg(txt, u=utmp)
                enddo
             endif
             if (is_msglev_INFO(lv)) then
211             format(3x, A, ' *', I0)
                call compact_string(jerr, txt2, xl_oct, mag=10)
                call compact_string(jerr, txt2, dx_oct, ldelim=':+', mag=10, append=.TRUE.)
                write(txt, 211) trim(txt2), mx_oct
                call msg(txt, u=utmp)
                call compact_string(jerr, txt2, yl_oct, mag=10)
                call compact_string(jerr, txt2, dy_oct, ldelim=':+', mag=10, append=.TRUE.)
                write(txt, 211) trim(txt2), my_oct
                call msg(txt, u=utmp)
             endif

             mgh = nlono * (jlate - jlatb)
             if (ierr.eq.0) then
                ! if (mx_oct.gt.0.and.my_oct.gt.0) then
                   call div_ps2g_map &
                        & (ierr,   iofs_oct(jpof:), iprj_oct(jpws:), wprj_oct(:, jpws:),  &
                        &  nwo,    mwo,      &
                        &  latn,   wlat,     jlatb,   jlate, mlat,   &
                        &  lonn_grp(jgbgn:jgend), dlon_grp(jgbgn:jgend), 0,     nlono,  nlono,   &
                        &  nwedge, wpos,     iniskip, &
                        &  xl_oct, dx_oct,   mx_oct,  yl_oct, dy_oct, my_oct,   &
                        &  cco,    &
                        &  lonlev, latlev,   inilev,  swlev, reqlev, tol,  &
                        &  deg,    u,        levv,    tag,   udump)
                   mwo = max(0, mwo - nwo)
                   nmem = nmem + nwo
                   jpws = min(nmem, lwo)
                   jpof = jpof + mgh + 1
                ! endif
             endif
          endif
          itblo(joct + 1) = jpof
          wtblo(joct + 1) = nmem
          ! write(*, *) 'tblo:', joct, jpof, nmem
       enddo
    endif
    if (ierr.eq.0) then
       call octant_dist_table &
            & (ierr,     iofs,     iprj,     wprj,  &
            &  nmem,     lmem,     &
            &  iofs_oct, iprj_oct, wprj_oct, &
            &  jwgt_dec, wtbl_dec, doct_dec, adjl_dec, &
            &  symmz,    zpropi,   ooffs,    itblo, wtblo, &
            &  jlatb,    jlate,    &
            &  jlonb,    jlone,    &
            &  mx,       my,       lospan,   &
            &  u,        levv,     tag,      udump)
    endif

    if (ierr.eq.0) then
       if (lmem.lt.0.or.nmem.lt.lmem) then
          call octant_settle_table &
               & (ierr,     iprj,     wprj,  &
               &  iofs,     &
               &  jlatb,    jlate,    jlonb, jlone,  mdest)
       endif
    endif

    if (ierr.eq.0) then
       deallocate(iofs_oct, iprj_oct, wprj_oct, &
            &     lonn_ser, dlon_ser, lonn_grp, dlon_grp, &
            &     doct_dec, lonn_dec, adjl_dec, jwgt_dec, wtbl_dec, dectbl,   &
            &     STAT=ierr)
    endif
    call trace_err(ierr, fun='symm_ps2g_map')
  end subroutine symm_ps2g_map_d

!!!_   & octant_dist_table - distribute normalized octant table
  subroutine octant_dist_table_d &
       & (ierr,     iofs,     iprj,     wprj,  &
       &  nmem,     lmem,     &
       &  iofs_oct, iprj_oct, wprj_oct, &
       &  jwgt_dec, wtbl_dec, doct_dec, adjl_dec, &
       &  symmz,    zpropi,   ooffs,    itblo, wtblo, &
       &  jlatb,    jlate,    &
       &  jlonb,    jlone,    &
       &  mx,       my,       lospan,   &
       &  u,        levv,     tag,      udump)
    use TOUZA_Ami_std,only: compact_string
    use TOUZA_Emu_ugg,only: check_monotonic, ang2rad
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: lmem                   ! limit size of iprj wprj  (negative to skip check)
    integer,         intent(out) :: iofs(0:*)              ! [(lo,la)+1] to subscript-vector offset (lv)
    integer,         intent(out) :: iprj(0:*)              ! [lv] to g(xy)-index
    real(kind=KTGT), intent(out) :: wprj(0:ps2g_weights-1, 0:*)    ! [lv] to weights
    integer,         intent(out) :: nmem                   ! wprj active members
    integer,         intent(in)  :: iofs_oct(0:*)              ! [(lo,la)+1] to subscript-vector offset (lv)
    integer,         intent(in)  :: iprj_oct(0:*)              ! [lv] to g(xy)-index
    real(kind=KTGT), intent(in)  :: wprj_oct(0:ps2g_weights-1, 0:*)    ! [lv] to weights
    integer,         intent(in)  :: jwgt_dec(0:*)
    integer,         intent(in)  :: wtbl_dec(0:*)
    integer,         intent(in)  :: doct_dec(0:*)
    real(kind=KTGT), intent(in)  :: adjl_dec(0:*)
    real(kind=KTGT), intent(in)  :: lospan
    integer,         intent(in)  :: symmz(0:*)
    integer,         intent(in)  :: zpropi(lzpi, 0:locts-1, lxy)
    integer,         intent(in)  :: ooffs(0:*), itblo(0:*), wtblo(0:*)
    integer,         intent(in)  :: jlonb, jlatb
    integer,         intent(in)  :: jlone, jlate
    integer,         intent(in)  :: mx, my
    integer,         intent(in),optional :: u, levv
    integer,         intent(in),optional :: udump
    character(len=*),intent(in),optional :: tag

    integer nlon
    integer koct,  kref
    integer jla,   jlaph
    integer jlo,   jloph, nlono
    integer jg,    mgh
    integer jw
    integer jdec,  jdbgn, jdend
    integer jnml,  jnbgn, jnend
    integer jinml, jjnml, ninml, njnml, jpnml
    integer jistr, jjstr, jpstr
    integer jmem,  mdest
    integer ofso,  ofsp

    real(kind=KTGT) :: cadj_b


    ierr = 0
    jmem = 0
    mdest = mx * my
    nlon = jlone - jlonb

    if (ierr.eq.0) then
       iofs(0) = 0
       do jlaph = jlatb, jlate - 1
          jla = jlaph - jlatb
          do jloph = jlonb, jlone - 1
             jlo = jloph - jlonb
             jg = nlon * jla + jlo
             jdbgn = wtbl_dec(jlo)
             jdend = wtbl_dec(jlo + 1)
             ! write(*, *) 'll', jlaph, jloph, jdbgn, jdend
             do jdec = jdbgn, jdend - 1
                koct = modulo(doct_dec(jdec) / 2, locts)
                kref = symmz(koct)
                ofso = itblo(kref)
                ofsp = wtblo(kref)
                nlono = ooffs(kref + 1) - 1 - ooffs(kref)
                jw = nlono * jla + jwgt_dec(jdec)
                jnbgn = iofs_oct(ofso + jw)
                jnend = iofs_oct(ofso + jw + 1)

                cadj_b = ang2rad(adjl_dec(jdec), lospan)

                ninml = zpropi(zpi_lc, kref, coor_x)
                njnml = zpropi(zpi_lc, kref, coor_y)
                mgh = ninml * njnml
                ! write(*, *) 'zone', jdec, koct, kref, jnbgn, jnend, nlono, mgh
                do jnml = jnbgn, jnend - 1
                   if (lmem.lt.0.or.jmem.lt.lmem) then
                      jpnml = iprj_oct(ofsp + jnml)
                      if (jpnml.ge.mgh) then
                         jistr = -1
                         jjstr = -1
                         jpstr = mdest
                      else
                         jinml = mod(jpnml, ninml)
                         jjnml = jpnml / ninml
                         select case (koct)
                         case(oct_px_mc,oct_mx_mc,oct_mx_pc,oct_px_pc)
                            jistr = zpropi(zpi_fo, koct, coor_x) + zpropi(zpi_ff, koct, coor_x) * jinml
                            jjstr = zpropi(zpi_fo, koct, coor_y) + zpropi(zpi_ff, koct, coor_y) * jjnml
                         case default
                            jistr = zpropi(zpi_fo, koct, coor_y) + zpropi(zpi_ff, koct, coor_y) * jjnml
                            jjstr = zpropi(zpi_fo, koct, coor_x) + zpropi(zpi_ff, koct, coor_x) * jinml
                         end select
                         jpstr = jjstr * mx + jistr
                      endif
                      ! if (jpstr.gt.mdest) write(*, *) 'str', jpstr, jistr, mx, jjstr, my
                      ! if (jpstr.gt.mdest) write(*, *) 'nml', jpnml, jinml, ninml, jjnml, njnml
                      iprj(jmem) = jpstr
                      wprj(:, jmem) = wprj_oct(:, ofsp + jnml)
                      ! write(*, *) jlaph, jloph, &
                      !      & jmem, wprj(ps2g_w1blo, jmem), ang2rad(adjl_dec(jdec), lospan), wprj(ps2g_w0, jmem)
                      select case (koct)
                      case(oct_mc_px,oct_mx_mc,oct_pc_mx,oct_px_pc)
                         wprj(ps2g_w1flo, jmem) = - wprj(ps2g_w1flo, jmem)
                         wprj(ps2g_w1blo, jmem) = - wprj(ps2g_w1blo, jmem)
                      case default
                      end select
                      wprj(ps2g_w1blo, jmem) = &
                           wprj(ps2g_w1blo, jmem) + cadj_b * wprj(ps2g_w0, jmem)
                   endif
                   jmem = jmem + 1
                enddo
             enddo
             iofs(jg + 1) = jmem
          enddo
       enddo
       ! write(*, *) wprj(ps2g_w1flo, 0:nmem-1)
    endif
    nmem = jmem
    return
  end subroutine octant_dist_table_d

!!!_   & octant_settle_table
  subroutine octant_settle_table_d &
       & (ierr, iprj, wprj, iofs, jlatb, jlate, jlonb, jlone, mdest)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,        intent(out)   :: ierr
    integer,        intent(inout) :: iprj(0:*)
    real(kind=KTGT),intent(inout) :: wprj(0:ps2g_weights-1, 0:*)
    integer,        intent(in)    :: iofs(0:*)
    integer,        intent(in)    :: jlatb, jlate
    integer,        intent(in)    :: jlonb, jlone
    integer,        intent(in)    :: mdest
    integer j,  jx, mgg
    integer jp, jpbgn, jpend
    real(kind=KTGT) :: centla, centlo, asum
    real(kind=KTGT) :: sac(0:mdest)
    real(kind=KTGT) :: cla(0:mdest), clo(0:mdest)
    ierr = 0

    sac(0:mdest) = 0.0_KTGT
    cla(0:mdest) = 0.0_KTGT
    clo(0:mdest) = 0.0_KTGT
    mgg = (jlate - jlatb) * (jlone - jlonb)
    do jp = 0, iofs(mgg) - 1
       jx = iprj(jp)
       if (jx.lt.0.or.jx.gt.mdest) then
          write(*, *) 'settle:fail:', jp, jx, mdest
       else
          sac(jx) = sac(jx) + wprj(ps2g_w0, jp)
          cla(jx) = cla(jx) + wprj(ps2g_w1fla, jp)
          clo(jx) = clo(jx) + wprj(ps2g_w1flo, jp)
       endif
    enddo
    do jp = 0, iofs(mgg) - 1
       jx = iprj(jp)
       ! if (jx.le.mdest) then
       if (jx.ge.0.and.jx.ge.mdest) then
          wprj(ps2g_w1fla, jp) = wprj(ps2g_w1fla, jp) - cla(jx) * sac(jx)
          wprj(ps2g_w1flo, jp) = wprj(ps2g_w1flo, jp) - clo(jx) * sac(jx)
       endif
    enddo

    do j = 0, mgg - 1
       jpbgn = iofs(j)
       jpend = iofs(j+1)
       centla = 0.0_KTGT
       centlo = 0.0_KTGT
       asum = 0.0_KTGT
       do jp = jpbgn, jpend - 1
          centla = centla + wprj(ps2g_w1bla, jp)
          centlo = centlo + wprj(ps2g_w1blo, jp)
          asum = asum + wprj(ps2g_w0, jp)
       enddo
       centla = centla / asum
       centlo = centlo / asum
       wprj(ps2g_w1bla, jpbgn:jpend-1) = &
            & wprj(ps2g_w1bla, jpbgn:jpend-1) - centla * wprj(ps2g_w0, jpbgn:jpend-1)
       wprj(ps2g_w1blo, jpbgn:jpend-1) = &
            & wprj(ps2g_w1blo, jpbgn:jpend-1) - centlo * wprj(ps2g_w0, jpbgn:jpend-1)
    enddo

    return
  end subroutine octant_settle_table_d

!!!_    * octant_gen_table - tabulate octant-arrangement
  subroutine octant_gen_table_d &
       & (ierr,     &
       &  dectbl,   doct_dec, lonn_dec, adjl_dec, &
       &  lonn_ser, dlon_ser, lser,     ooffs, &
       &  lonn,     dlon,     jlonb,    jlone, cco)
    implicit none
    integer,parameter     :: KTGT=KDBL
    integer,        intent(out) :: ierr
    integer,        intent(out) :: dectbl(0:*)
    integer,        intent(out) :: doct_dec(0:*)
    real(kind=KTGT),intent(out) :: lonn_dec(0:*)
    real(kind=KTGT),intent(out) :: adjl_dec(0:*)   ! pivot adjustment in longitude
    real(kind=KTGT),intent(out) :: lonn_ser(0:*)
    real(kind=KTGT),intent(out) :: dlon_ser(0:*)
    integer,        intent(in)  :: lser
    integer,        intent(out) :: ooffs(0:*)      ! 0:8
    real(kind=KTGT),intent(in)  :: lonn(0:*)       ! only use lonn(jlonb:jlone)
    real(kind=KTGT),intent(in)  :: dlon(0:*)
    integer,        intent(in)  :: jlonb, jlone
    real(kind=KTGT),intent(in)  :: cco(*)

    integer jo, mo, jobgn
    integer kref, ktmp
    integer js,   jsbgn, jsend, ns

    integer,parameter   :: mdiag = 8

    real(kind=KTGT) :: dlotmp(0:lser)
    integer ofstmp(0:locts+1)
    integer nditmp(0:locts+1)

    ierr = 0

    if (ierr.eq.0) then
       call octant_wedge_lonseq &
            & (ierr,   nditmp,   ofstmp,   mo,     &
            &  dectbl, doct_dec, lonn_dec, dlotmp, adjl_dec, lser, &
            &  lonn,   dlon,     jlonb,    jlone,  cco)
    endif
    ns = ofstmp(mo)
#if DEBUG_AMI_SYMM
    ! write(*, *) 'ofs', ofstmp(0:mo)
    ! write(*, *) 'ndi', nditmp(0:mo)
    ! write(*, *) 'lser', lser, ns, mo
    write(*, *) 'lon:src:', lonn(jlonb:jlone)
    write(*, *) 'doi:dec:', doct_dec(0:ns)
    write(*, *) 'lon:dec:', lonn_dec(0:ns)
    ! write(*, *) 'adj:dec:', adjl_dec(0:ns)
    ! write(*, *) 'dec', dectbl(0:jlone-jlonb)
#endif /* DEBUG_AMI_SYMM */

    if (ierr.eq.0) then
       ! search minimum octant
       jobgn = 0
       kref = locts * 2 + 1
       do jo = 0, mo - 1
          ktmp = modulo(nditmp(jo), locts * 2)
          if (ktmp.lt.kref) then
             jobgn = jo
             kref = ktmp
          endif
       enddo
       ! write(*, *) 'kref', kref, jobgn, nditmp(jobgn)
       ooffs(0:locts) = 0
       js = 0
       do jo = jobgn, mo - 1
          jsbgn = ofstmp(jo)
          jsend = ofstmp(jo+1)
          ns = jsend - jsbgn
          lonn_ser(js:js+ns)   = lonn_dec(jsbgn:jsend)
          dlon_ser(js:js+ns-1) = dlotmp(jsbgn:jsend-1)
          dlon_ser(js+ns) = 0.0_KTGT
          js = js + ns + 1
          ktmp = modulo(nditmp(jo) / 2, locts)
          ! write(*, *) 'right:', jo, ktmp, lonn_dec(jsbgn:jsend)
          ooffs(ktmp+1) = js
       enddo
       do jo = 0, jobgn - 1
          jsbgn = ofstmp(jo)
          jsend = ofstmp(jo+1)
          ns = jsend - jsbgn
          lonn_ser(js:js+ns)   = lonn_dec(jsbgn:jsend)
          dlon_ser(js:js+ns-1) = dlotmp(jsbgn:jsend-1)
          dlon_ser(js+ns) = 0.0_KTGT
          js = js + ns + 1
          ktmp = modulo(nditmp(jo) / 2, locts)
          ! write(*, *) 'left:', jo, ktmp, lonn_dec(jsbgn:jsend)
          ooffs(ktmp+1) = js
       enddo
       ! remnant
       do jo = 0, locts - 1
          ooffs(jo+1) = max(ooffs(jo), ooffs(jo+1))
       enddo
       ! write(*, *) ooffs(0:locts)
       ! write(*, *) lonn_ser(0:js-1)
       ! write(*, *) dlon_ser(0:js-1)

       do jo = 0, locts - 1
          jsbgn = ooffs(jo)
          jsend = ooffs(jo+1)
          select case (jo)
          case(oct_px_mc,oct_mx_pc,oct_mc_mx,oct_pc_px)
          case default
             lonn_ser(jsbgn:jsend-1) = lonn_ser(jsend-1:jsbgn:-1)
             dlon_ser(jsbgn:jsend-2) = dlon_ser(jsend-2:jsbgn:-1)
          end select
       enddo
    endif

    if (ierr.eq.0) call octant_fill_table(ierr, ooffs, lonn_ser, dlon_ser)
    ! diag
    ! do jo = 0, locts - 1
    !    jsbgn = ooffs(jo)
    !    jsend = ooffs(jo+1)
    !    do js = jsbgn, jsend, mdiag
    !       call join_list(ierr, txt, lonn_ser(js:MIN(jsend-1,js+mdiag-1)), fmt=' ')
    !       write(*, *) 'collect:l:', jo, js, trim(txt)
    !       call join_list(ierr, txt, dlon_ser(js:MIN(jsend-1,js+mdiag-1)), fmt=' ')
    !       write(*, *) 'collect:d:', jo, js, trim(txt)
    !    enddo
    ! enddo
  end subroutine octant_gen_table_d

!!!_    * octant_wedge_lonseq
  subroutine octant_wedge_lonseq_d &
       & (ierr,   nditbl, ofs,    mo,  &
       &  dectbl, ndiseq, lonseq, dloseq, adjseq, lseq, &
       &  lonn,   dlon,   jlonb,  jlone,  cco)
    implicit none
    integer,parameter     :: KTGT=KDBL
    integer,        intent(out) :: ierr
    integer,        intent(out) :: nditbl(0:*)
    integer,        intent(out) :: ofs(0:*)
    integer,        intent(out) :: mo
    integer,        intent(out) :: dectbl(0:*)     ! decomposition table
    integer,        intent(out) :: ndiseq(0:*)
    real(kind=KTGT),intent(out) :: lonseq(0:*)
    real(kind=KTGT),intent(out) :: dloseq(0:*)
    real(kind=KTGT),intent(out) :: adjseq(0:*)
    integer,        intent(in)  :: lseq
    real(kind=KTGT),intent(in)  :: lonn(0:*)       ! only use lonn(jlonb:jlone)
    real(kind=KTGT),intent(in)  :: dlon(0:*)
    integer,        intent(in)  :: jlonb, jlone
    real(kind=KTGT),intent(in)  :: cco(*)
    integer js, jlo
    integer ms
    integer kini, kext, kent, knxt, ktmp

    ierr = 0

    mo = 0
    js = 0

    kini = get_octant_ndi(lonn(jlonb), cco, 0)
    kext = 0
    kent = 0
    ofs(mo) = 0
    nditbl(mo) = kini
    dectbl(0) = 0
    do jlo = jlonb, jlone - 1
       kent = get_octant_ndi(lonn(jlo), cco, kext)
       kext = get_octant_ndi(lonn(jlo+1), cco, kent)
       lonseq(js) = lonn(jlo)
       ndiseq(js) = kent
       js = js + 1
       ktmp = kent
       do
          knxt = ((ktmp / 2) + 1) * 2
          if (knxt.ge.kext) exit
          lonseq(js) = octant_lon(knxt, cco, kini, lonn(jlonb))
          ndiseq(js) = knxt
          mo = mo + 1
          ofs(mo) = js
          nditbl(mo) = knxt
          js = js + 1
          ktmp = knxt
       enddo
       if (mod(kext, 2).eq.0) then
          mo = mo + 1
          ofs(mo) = js
          nditbl(mo) = kext
       endif
       dectbl(jlo - jlonb + 1) = js
    enddo
    jlo = jlone - 1
    kext = get_octant_ndi(lonn(jlo+1), cco, kent)
    ! if both ends belong to the same octant
    lonseq(js) = lonn(jlo+1)
    ndiseq(js) = kext
    dectbl(jlo - jlonb + 1) = js

    if (mod(kext, 2).ne.0) mo = mo + 1
    ofs(mo) = js
    nditbl(mo) = kext

    if (js.gt.lseq+1) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)

    ! set or copy dlon
    if (ierr.eq.0) then
       ms = js
       jlo = 0
       do js = 0, ms - 1
          if (lonseq(js).eq.lonn(jlo)) then
             if (lonseq(js+1).lt.lonn(jlo+1)) then
                dloseq(js) = lonseq(js+1) - lonseq(js)
                adjseq(js) = (lonseq(js) - lonn(jlo)) + (dloseq(js) - dlon(jlo)) / 2.0_KTGT
                ! write(*, *) 'A', js, lonseq(js), dloseq(js), jlo, lonn(jlo), dlon(jlo)
                ! [lonseq(js) + dloseq(js)/2] - [lonn(jlo) + dlon(jlo)/2]
             else
                dloseq(js) = dlon(jlo)
                adjseq(js) = 0.0_KTGT
                ! write(*, *) 'C', js, lonseq(js), dloseq(js), jlo, lonn(jlo), dlon(jlo)
             endif
          else
             dloseq(js) = lonseq(js+1) - lonseq(js)
             adjseq(js) = (lonseq(js) - lonn(jlo)) + (dloseq(js) - dlon(jlo)) / 2.0_KTGT
             ! write(*, *) 'B', js, lonseq(js), dloseq(js), jlo, lonn(jlo), dlon(jlo)
          endif
          if (lonseq(js+1).eq.lonn(jlo+1)) jlo = jlo + 1
       enddo
       ! dloseq(ms-1) = 0.0_KTGT
       dloseq(ms) = 0.0_KTGT
       adjseq(ms) = 0.0_KTGT
    endif
#if DEBUG_AMI_SYMM
    do js = 0, ms
       write(*, *) 'lon:dlon:', js, ndiseq(js), lonseq(js), adjseq(js), dloseq(js)
    enddo
#endif /* DEBUG_AMI_SYMM */

    ! normalize
    if (ierr.eq.0) then
       do js = 0, ofs(mo)
          lonseq(js) = octant_normalize_lon(lonseq(js), cco, ndiseq(js))
       enddo
    endif
  end subroutine octant_wedge_lonseq_d

! !!!_   . octant_normalize_lon ()
  real(kind=KTGT) function octant_normalize_lon_d(lon, cco, koct) result(v)
    use TOUZA_Emu_ugg,only: psgp_inquire
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in)  :: lon
    real(kind=KTGT),intent(in)  :: cco(*)
    integer,        intent(in)  :: koct
    real(kind=KTGT) :: span, olon, lref
    integer,parameter :: nquad = 4
    integer ko
    integer jerr
    call psgp_inquire(jerr, cco, olon=olon, lospan=span)
    if (jerr.eq.0) then
       span = abs(span) / real(nquad, kind=KTGT)
       ko = modulo(koct / 2, locts)
       select case(ko)
       case(oct_px_mc,oct_mx_pc,oct_mc_mx,oct_pc_px)
          lref = FLOOR((lon - olon) / span) * span + olon
          v = lon - lref
       case default
          lref = CEILING((lon - olon) / span) * span + olon
          v = lref - lon
       end select
    else
       v = lon
    endif
  end function octant_normalize_lon_d

!!!_    * octant_fill_table
  subroutine octant_fill_table_d &
       & (ierr,  &
       &  ooffs, lonn_ser, dlon_ser)
    implicit none
    integer,parameter     :: KTGT=KDBL
    integer,        intent(out) :: ierr
    integer,        intent(inout) :: ooffs(0:*)
    real(kind=KTGT),intent(inout) :: lonn_ser(0:*)
    real(kind=KTGT),intent(inout) :: dlon_ser(0:*)

    integer jo
    integer js, jsbgn, jsend
    integer mtbl
    ierr = 0
    mtbl = ooffs(locts)

    do jo = locts - 1, 0, -1
       jsbgn = ooffs(jo)
       jsend = ooffs(jo+1)
       do js = jsend - 2, jsbgn, -1
          if (lonn_ser(js).eq.lonn_ser(js+1)) then
             lonn_ser(js:mtbl-2) = lonn_ser(js+1:mtbl-1)
             dlon_ser(js:mtbl-2) = dlon_ser(js+1:mtbl-1)
             mtbl = mtbl - 1
             ooffs(jo+1:locts) = ooffs(jo+1:locts) - 1
          endif
       enddo
    enddo

  end subroutine octant_fill_table_d

!!!_    * octant_gen_group
  subroutine octant_gen_group_d &
       & (ierr,     &
       &  symmz,    &
       &  lonn_grp, dlon_grp, ogrps, &
       &  lonn_ser, dlon_ser, osers, &
       &  lser)
    use TOUZA_Ami_std,only: find_first
    implicit none
    integer,parameter     :: KTGT=KDBL
    integer,        intent(out) :: ierr
    integer,        intent(inout) :: symmz(0:*)
    real(kind=KTGT),intent(out) :: lonn_grp(0:*)
    real(kind=KTGT),intent(out) :: dlon_grp(0:*)
    integer,        intent(out) :: ogrps(0:*)
    real(kind=KTGT),intent(in)  :: lonn_ser(0:*)
    real(kind=KTGT),intent(in)  :: dlon_ser(0:*)
    integer,        intent(in)  :: osers(0:*)
    integer,        intent(in)  :: lser

    integer jo, jt, odup
    integer jsbgn, jsend, jsl, jsh
    integer jdbgn, jdend, jdl, jdh, md, jdins, nd
    integer symm_adj(0:locts-1)
    integer jp, ncp, nsh

    ierr = 0
    ogrps(0) = 0
    do jo = 0, locts - 1
       jsbgn = osers(jo)
       jsend = osers(jo + 1)
       odup = -1
       if (symmz(jo).ne.jo &
            & .and. jsbgn.lt.jsend) then
          jp = -1
          do jt = 0, jo - 1
             if (symmz(jo).ne.symmz(jt)) cycle
             jdbgn = ogrps(jt)
             jdend = ogrps(jt + 1)
             if (jdend.le.jdbgn) cycle

             !  s: b * * l     h * * e
             !  d:     l=B * * E=h
             if (lonn_grp(jdbgn).le.lonn_ser(jsbgn) &
                  & .and. lonn_ser(jsbgn).le.lonn_grp(jdend-1)) then
                ! if dest:0 <= src:0 <= dest:end
                ! s: . . X * * *
                ! d: * * X * * . . .
                jp = find_first(lonn_grp(jdbgn:jdend-1), lonn_ser(jsbgn))
                if (jp.ge.0) then
                   jsl = jsbgn
                   jdl = jdbgn + jp
                endif
             else if (lonn_ser(jsbgn).le.lonn_grp(jdbgn) &
                  & .and. lonn_grp(jdbgn).le.lonn_ser(jsend-1)) then
                ! if src:0 <= dest:0 <= src:end
                ! s: * * X * * *
                ! d: . . X * * . . .
                jp = find_first(lonn_ser(jsbgn:jsend-1), lonn_grp(jdbgn))
                if (jp.ge.0) then
                   jsl = jsbgn + jp
                   jdl = jdbgn
                endif
             else
                ! skip if no overlap (indeed this is insufficient to fully detect overlapping)
                jp = -1
             endif
             if (jp.ge.0) then
                ncp = min(jsend - jsl, jdend - jdl)
                jsh = jsl + ncp
                jdh = jdl + ncp
                if (ANY(lonn_ser(jsl:jsh-1).ne.lonn_grp(jdl:jdh-1))) jp = -1
             endif
             if (jp.lt.0) cycle

             odup = jt
             ! need to fill gap
             where(dlon_grp(jdl:jdh-1).eq.0.0_KTGT)
                dlon_grp(jdl:jdh-1) = dlon_ser(jsl:jsh-1)
             end where

             nsh = max(0, jsl - jsbgn) + max(0, jsend - jsh)
             if (nsh.gt.0) then
                md = ogrps(jo)
                nd = md - jdend
                jdins = jdend + nsh
                lonn_grp(jdins:jdins+nd-1) = lonn_grp(jdend:md-1)
                dlon_grp(jdins:jdins+nd-1) = dlon_grp(jdend:md-1)
                ogrps(jt + 1:jo) = ogrps(jt + 1:jo) + nsh
             endif
             ! insert at left
             nsh = max(0, jsl - jsbgn)
             if (nsh.gt.0) then
                ! s: B * L * ..
                ! d: . . b * e. . .
                nd = jdend - jdbgn
                jdins = jdbgn + nsh
                lonn_grp(jdins:jdins+nd-1) = lonn_grp(jdbgn:jdend-1)
                dlon_grp(jdins:jdins+nd-1) = dlon_grp(jdbgn:jdend-1)
                lonn_grp(jdbgn:jdbgn+nsh-1) = lonn_ser(jsbgn:jsbgn+nsh-1)
                dlon_grp(jdbgn:jdbgn+nsh-1) = dlon_ser(jsbgn:jsbgn+nsh-1)
             endif
             ! append at right
             nsh = max(0, jsend - jsh)
             if (nsh.gt.0) then
                ! s: . . L * H * E
                ! d: * * L * e . . .
                jdins = jdl + (jsend - jsl) - nsh
                lonn_grp(jdins:jdins+nsh-1) = lonn_ser(jsh:jsend-1)
                dlon_grp(jdins:jdins+nsh-1) = dlon_ser(jsh:jsend-1)
                if (jsbgn.lt.jsh) dlon_grp(jdins-1) = dlon_ser(jsh-1)
             endif
             exit
          enddo
       endif
       if (odup.lt.0) then
          jdbgn = ogrps(jo)
          jdend = jdbgn + (jsend - jsbgn)
          lonn_grp(jdbgn:jdend-1) = lonn_ser(jsbgn:jsend-1)
          dlon_grp(jdbgn:jdend-1) = dlon_ser(jsbgn:jsend-1)
          ogrps(jo + 1) = jdend
          symm_adj(jo) = jo
       else
          ogrps(jo + 1) = ogrps(jo)
          symm_adj(jo) = odup
       endif
    enddo
    if (ierr.eq.0) then
       symmz(0:locts-1) = symm_adj(0:locts-1)
    endif
  end subroutine octant_gen_group_d

!!!_    * set_octant_ends
  subroutine set_octant_ends_d &
       & (ierr, nocts, opropi, &
       &  lonn, jlonb, jlone,  cco)
    implicit none
    integer,parameter     :: KTGT=KDBL
    integer,        intent(out)   :: ierr
    integer,        intent(out)   :: nocts
    integer,        intent(out)   :: opropi(lopi, 0:*)
    real(kind=KTGT),intent(in)    :: lonn(0:*)
    integer,        intent(in)    :: jlonb, jlone
    real(kind=KTGT),intent(in)    :: cco(*)

    integer :: jlobgn, jloend
    integer :: kofs
    integer :: koct, kent, kext

    ierr = 0
    nocts = 0
    if (ierr.eq.0) then
       jlobgn = jlonb
       kent = octant_zone(lonn(jlobgn), cco, 0)
       kofs = (kent / 2) * 2   ! minimum zone index
       koct = (kent / 2) * 2 + 1  ! koct always points to the zone interior
       jloend = jlobgn + 1
       ! search upper end
       do
          do
             if (jloend.ge.jlone) exit
             kext = octant_zone(lonn(jloend), cco, kent)
             if (kext .gt. koct) exit
             jloend = jloend + 1
          enddo
          ! search lower end
          kext = kent
          do
             if (jlobgn+1.gt.jloend) exit
             kext = octant_zone(lonn(jlobgn+1), cco, kext)
             if (kext .ge. koct) exit
             jlobgn = jlobgn + 1
          enddo

          if (jlobgn.ge.jloend) exit

          opropi(opi_octant, nocts) = koct
          opropi(opi_lobgn, nocts) = jlobgn
          opropi(opi_loend, nocts) = jloend
          nocts = nocts + 1

          koct = koct + 2
       enddo
    endif

  end subroutine set_octant_ends_d

!!!_    * set_zone_props
  subroutine set_zone_props_d &
       & (ierr,  zpropi, zpropr, &
       &  kasp,  symmz,  xl, dx, mx, yl, dy, my)
    use TOUZA_Ami_std,only: choice
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,        intent(out) :: ierr
    integer,        intent(out) :: zpropi(lzpi,0:locts-1,*)
    real(kind=KTGT),intent(out) :: zpropr(lzpr,0:locts-1,*)
    integer,        intent(in)  :: kasp
    integer,        intent(in)  :: symmz(0:*)
    real(kind=KTGT),intent(in)  :: xl, dx
    real(kind=KTGT),intent(in)  :: yl, dy
    integer,        intent(in)  :: mx, my

    real(kind=KTGT) :: xh, cyh, cyl
    integer jz

    ierr = 0
    !!! left for future improvement
    if (dx.lt.0.0_KTGT.or.dy.lt.0.0_KTGT) then
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       return
    endif

    xh = xl + real(mx, kind=KTGT) * dx
    if (kasp.gt.0) then
       cyl = yl
       cyh = + (yl + real(my, kind=KTGT) * dy)
    else
       cyl = - (yl + real(my, kind=KTGT) * dy)
       cyh = - yl
    endif

    zpropi(:, 0:locts-1, 1:lxy) = -1
    zpropr(:, 0:locts-1, 1:lxy) = 0.0_KTGT

    if (ierr.eq.0) call set_zone_range(ierr, zpropi, zpropr, oct_px_mc, xl,  xh,  +dx, cyl, cyh, +dy)
    if (ierr.eq.0) call set_zone_range(ierr, zpropi, zpropr, oct_mc_px, cyl, cyh, +dy, xl,  xh,  +dx)
    if (ierr.eq.0) call set_zone_range(ierr, zpropi, zpropr, oct_mc_mx, cyl, cyh, +dy, xl,  xh,  -dx)
    if (ierr.eq.0) call set_zone_range(ierr, zpropi, zpropr, oct_mx_mc, xl,  xh,  -dx, cyl, cyh, +dy)
    if (ierr.eq.0) call set_zone_range(ierr, zpropi, zpropr, oct_mx_pc, xl,  xh,  -dx, cyl, cyh, -dy)
    if (ierr.eq.0) call set_zone_range(ierr, zpropi, zpropr, oct_pc_mx, cyl, cyh, -dy, xl,  xh,  -dx)
    if (ierr.eq.0) call set_zone_range(ierr, zpropi, zpropr, oct_pc_px, cyl, cyh, -dy, xl,  xh,  +dx)
    if (ierr.eq.0) call set_zone_range(ierr, zpropi, zpropr, oct_px_pc, xl,  xh,  +dx, cyl, cyh, -dy)

    if (ierr.eq.0) call adjust_zone_range(ierr, zpropi, zpropr, symmz, coor_x)
    if (ierr.eq.0) call adjust_zone_range(ierr, zpropi, zpropr, symmz, coor_y)

    if (ierr.eq.0) call set_zone_coeffs(ierr, zpropi, zpropr, oct_px_mc, xl,  xh,  +dx, cyl, cyh, +dy)
    if (ierr.eq.0) call set_zone_coeffs(ierr, zpropi, zpropr, oct_mc_px, cyl, cyh, +dy, xl,  xh,  +dx)
    if (ierr.eq.0) call set_zone_coeffs(ierr, zpropi, zpropr, oct_mc_mx, cyl, cyh, +dy, xl,  xh,  -dx)
    if (ierr.eq.0) call set_zone_coeffs(ierr, zpropi, zpropr, oct_mx_mc, xl,  xh,  -dx, cyl, cyh, +dy)
    if (ierr.eq.0) call set_zone_coeffs(ierr, zpropi, zpropr, oct_mx_pc, xl,  xh,  -dx, cyl, cyh, -dy)
    if (ierr.eq.0) call set_zone_coeffs(ierr, zpropi, zpropr, oct_pc_mx, cyl, cyh, -dy, xl,  xh,  -dx)
    if (ierr.eq.0) call set_zone_coeffs(ierr, zpropi, zpropr, oct_pc_px, cyl, cyh, -dy, xl,  xh,  +dx)
    if (ierr.eq.0) call set_zone_coeffs(ierr, zpropi, zpropr, oct_px_pc, xl,  xh,  +dx, cyl, cyh, -dy)

    if (ierr.eq.0) then
       if (kasp.lt.0) then
          do jz = 0, locts - 1
             zpropr(zpr_co, jz, coor_y) = - zpropr(zpr_co, jz, coor_y)
             zpropr(zpr_cd, jz, coor_y) = - zpropr(zpr_cd, jz, coor_y)
          enddo
       endif
    endif
#if DEBUG_AMI_SYMM
    if (ierr.eq.0) then
       do jz = 0, locts - 1
          write(*, *) 'zpi:x:', jz, zpropi(:, jz, coor_x), zpropr(:, jz, coor_x)
          write(*, *) 'zpi:y:', jz, zpropi(:, jz, coor_y), zpropr(:, jz, coor_y)
       enddo
    endif
#endif /* DEBUG_AMI_SYMM */

  end subroutine set_zone_props_d

!!!_    * set_zone_range
  subroutine set_zone_range_d &
       & (ierr,  zpropi, zpropr, &
       &  jzset, xl, xh, dx, yl, yh, dy)
    use TOUZA_Ami_std,only: choice
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,        intent(out) :: ierr
    integer,        intent(inout) :: zpropi(lzpi, 0:locts-1, *)
    real(kind=KTGT),intent(inout) :: zpropr(lzpr, 0:locts-1, *)
    integer,        intent(in)  :: jzset
    real(kind=KTGT),intent(in)  :: xl, xh, dx
    real(kind=KTGT),intent(in)  :: yl, yh, dy

    real(kind=KTGT) :: xladj, xhadj, adx
    real(kind=KTGT) :: yladj, yhadj, ady
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT

    ! effective region:
    !   NP aspect: x>=0, y=>0, y<=+x
    !   SP aspect: x>=0, y<=0, y>=-x

    ierr = 0
    adx = abs(dx)
    ady = abs(dy)
    if (dx.gt.0.0_KTGT) then
       xladj = +xl
       xhadj = +xh
       zpropi(zpi_ff, jzset, coor_x) = +1
    else
       xladj = -xh
       xhadj = -xl
       zpropi(zpi_ff, jzset, coor_x) = -1
    endif
    if (dy.gt.0.0_KTGT) then
       yladj = +yl
       yhadj = +yh
       zpropi(zpi_ff, jzset, coor_y) = +1
    else
       yladj = -yh
       yhadj = -yl
       zpropi(zpi_ff, jzset, coor_y) = -1
    endif

    if (xhadj.gt.ZERO.and.yhadj.gt.ZERO) then
       yhadj = min(yhadj, yladj + CEILING((xhadj - yladj) / ady) * ady)
    endif
    xladj = max(xladj, xhadj - CEILING((xhadj - max(yladj, ZERO)) / adx) * adx)

    if (ierr.eq.0) then
       call set_zone_range_core(ierr, zpropi, zpropr, jzset, +xladj, +xhadj, adx, coor_x)
    endif
    if (ierr.eq.0) then
       call set_zone_range_core(ierr, zpropi, zpropr, jzset, +yladj, +yhadj, ady, coor_y)
    endif
    if (ierr.eq.0) then
       ! zpropi(zpi_fo, jzset, coor_x) = int((xladj - xoadj) / dx)
       ! zpropi(zpi_fo, jzset, coor_y) = int((yladj - yoadj) / dy)
    endif
    if (xh.le.yl) then
       if (ierr.eq.0) then
          zpropi(zpi_eb, jzset, coor_x:coor_y) = 0
          zpropi(zpi_ee, jzset, coor_x:coor_y) = -1
          zpropi(zpi_ff, jzset, coor_x:coor_y) = 0
       endif
    endif

  end subroutine set_zone_range_d
!!!_    * set_zone_range_core
  subroutine set_zone_range_core_d &
       & (ierr,  zpropi, zpropr, &
       &  jzset, xl, xh, dx, jco)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,        intent(out) :: ierr
    integer,        intent(inout) :: zpropi(lzpi, 0:locts-1, *)
    real(kind=KTGT),intent(inout) :: zpropr(lzpr, 0:locts-1, *)
    integer,        intent(in)  :: jzset
    real(kind=KTGT),intent(in)  :: xl, xh, dx
    integer,        intent(in)  :: jco

    integer effb, effe
    real(kind=KTGT) :: org
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT

    ierr = 0
    if (xl.ge.ZERO) then
       org = xl
    else if (xh.le.ZERO) then
       org = xh + real(CEILING((- xh) / dx), kind=KTGT) * dx
    else
       org = xl + real(FLOOR((- xl) / dx), kind=KTGT) * dx
    endif
    zpropr(zpr_co, jzset, jco) = org
    zpropr(zpr_cd, jzset, jco) = dx

    effb = max(int((xl - org) / dx), 0)
    effe = int((xh - org) / dx)
    zpropi(zpi_eb, jzset, jco) = effb
    zpropi(zpi_ee, jzset, jco) = effe

! 101 format('R:', I0, ':', I0, ' (', 2F10.1, ') ', 2F9.1, ' // ', 5(1x, I0))
!     write(*, 101) jco, jzset, &
!          & xl, xh, &
!          & zpropr(:, jzset, jco), zpropi(:, jzset, jco)
  end subroutine set_zone_range_core_d

!!!_    * adjust_zone_range
  subroutine adjust_zone_range_d &
       & (ierr,  zpropi, zpropr, symmz, jco)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,        intent(out) :: ierr
    integer,        intent(inout) :: zpropi(lzpi, 0:locts-1, *)
    real(kind=KTGT),intent(inout) :: zpropr(lzpr, 0:locts-1, *)
    integer,        intent(in)  :: symmz(0:*)
    integer,        intent(in)  :: jco
    integer jzref
    integer jzmem
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    integer kadj, leff
    real(kind=KTGT) :: org, vl, dx

    ierr = 0
    do jzref = 0, locts - 1
       if (jzref.ne.symmz(jzref)) cycle
       org = +HUGE(ZERO)
       do jzmem = jzref, locts - 1
          if (jzref.ne.symmz(jzmem)) cycle
          if (ALL(zpropi(zpi_ee, jzmem, 1:lxy).gt.0)) org = min(org, zpropr(zpr_co, jzmem, jco))
       enddo
       if (org.ne.HUGE(ZERO)) then
          leff = -1
          do jzmem = jzref, locts - 1
             if (jzref.ne.symmz(jzmem)) cycle
             vl = zpropr(zpr_co, jzmem, jco)
             dx = zpropr(zpr_cd, jzmem, jco)
             ! write(*, *) jzref, jzmem, zpropi(zpi_ee, jzmem, 1:lxy)
             if (ALL(zpropi(zpi_ee, jzmem, 1:lxy).gt.0)) then
                kadj = int((vl - org) / dx)
                zpropi(zpi_eb, jzmem, jco) = zpropi(zpi_eb, jzmem, jco) + kadj
                zpropi(zpi_ee, jzmem, jco) = zpropi(zpi_ee, jzmem, jco) + kadj
                leff = max(leff, zpropi(zpi_ee, jzmem, jco))
             else
                zpropi(zpi_eb, jzmem, jco) = 0
                zpropi(zpi_ee, jzmem, jco) = -1
             endif
             zpropr(zpr_co, jzmem, jco) = org
          enddo
          zpropi(zpi_lc, jzref, jco) = leff
       else
          do jzmem = jzref, locts - 1
             if (jzref.eq.symmz(jzmem)) then
                zpropi(zpi_eb, jzmem, jco) = 0
                zpropi(zpi_ee, jzmem, jco) = -1
                zpropi(zpi_lc, jzmem, jco) = 0
             endif
          enddo
       endif
    enddo
!     do jzref = 0, locts - 1
!        if (jzref.ne.symmz(jzref)) cycle
! 101    format('A:', I0, 1x, I0, 1x, 2F9.1, ' // ', 5(1x, I0))
! 102    format('A:', I0, 1x, I0, 1x, 2F9.1)
! 111    format('A:', 1x, 1x, I0, 1x, 2F9.1, ' // ', 5(1x, I0))
! 112    format('A:', 1x, 1x, I0)
!        if (zpropi(zpi_ee, jzref, jco).gt.0) then
!           write(*, 101) jco, jzref, zpropr(:, jzref, jco), zpropi(:, jzref, jco)
!        else
!           write(*, 102) jco, jzref, zpropr(:, jzref, jco)
!        endif
!        do jzmem = jzref + 1, locts - 1
!           if (jzref.ne.symmz(jzmem)) cycle
!           if (zpropi(zpi_ee, jzmem, jco).gt.0) then
!              write(*, 111)   jzmem, zpropr(:, jzmem, jco), zpropi(:, jzmem, jco)
!           else
!              write(*, 112)   jzmem
!           endif
!        enddo
!     enddo
  end subroutine adjust_zone_range_d

!!!_    * set_zone_coeffs
  subroutine set_zone_coeffs_d &
       & (ierr,  zpropi, zpropr, &
       &  jzset, xl, xh, dx, yl, yh, dy)
    use TOUZA_Ami_std,only: choice
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,        intent(out) :: ierr
    integer,        intent(inout) :: zpropi(lzpi, 0:locts-1, *)
    real(kind=KTGT),intent(inout) :: zpropr(lzpr, 0:locts-1, *)
    integer,        intent(in)  :: jzset
    real(kind=KTGT),intent(in)  :: xl, xh, dx
    real(kind=KTGT),intent(in)  :: yl, yh, dy

    real(kind=KTGT) :: adx, ady
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT

    ierr = 0
    adx = abs(dx)
    ady = abs(dy)
    ! write(*, *) 'x', jzset, xl, zpropr(:, jzset, coor_x)
    ! write(*, *) 'y', jzset, yl, zpropr(:, jzset, coor_y)

    if (dx.gt.0.0_KTGT) then
       zpropi(zpi_fo, jzset, coor_x) = int((+zpropr(zpr_co, jzset, coor_x) - xl) / adx)
    else
       zpropi(zpi_fo, jzset, coor_x) = int((-zpropr(zpr_co, jzset, coor_x) - xl) / adx) - 1
    endif
    if (dy.gt.0.0_KTGT) then
       zpropi(zpi_fo, jzset, coor_y) = int((+zpropr(zpr_co, jzset, coor_y) - yl) / ady)
    else
       zpropi(zpi_fo, jzset, coor_y) = int((-zpropr(zpr_co, jzset, coor_y) - yl) / ady) - 1
    endif

  end subroutine set_zone_coeffs_d

!!!_    * octant_zone_d ()
  integer function octant_zone_d(lon, cco, ofs) result(k)
    use TOUZA_Emu_ugg,only: deg2rad, psgp_inquire
    implicit none
    integer,parameter :: KTGT=KDBL
    real(kind=KTGT),intent(in) :: lon
    real(kind=KTGT),intent(in) :: cco(*)
    integer,        intent(in) :: ofs
    real(kind=KTGT) :: span, ph, olat, olon
    integer,parameter :: noct = 8
    integer jerr

    ! return  octant * 2 + [1 if not on boundary]

    call psgp_inquire(jerr, cco, olat=olat, olon=olon, lospan=span)
    k = min(0, jerr)
    if (k.eq.0) then
       span = abs(span)
       ph = modulo(lon - olon, span)
       ph = ph / (span / real(noct, kind=KTGT))
       k = FLOOR(ph)

       k = modulo(k + oct_pc_px, noct)

       k = k * 2
       if (ph.ne.AINT(ph)) k = k + 1
       if (k.lt.ofs) k = k + noct * 2
    endif
  end function octant_zone_d

!!!_    * get_octant_ndi () - return normalized doubled octant id
  integer function get_octant_ndi_d(lon, cco, ref) result(k)
    use TOUZA_Emu_ugg,only: deg2rad, psgp_inquire
    implicit none
    integer,parameter :: KTGT=KDBL
    real(kind=KTGT),intent(in) :: lon
    real(kind=KTGT),intent(in) :: cco(*)
    integer,        intent(in) :: ref
    real(kind=KTGT) :: span, ph, olat, olon
    integer,parameter :: noct = 8
    integer,parameter :: noct2 = noct * 2
    integer jerr

    ! return  octant * 2 + [1 if not on boundary]
    !         adjust K such that K >= ref

    call psgp_inquire(jerr, cco, olat=olat, olon=olon, lospan=span)
    k = min(0, jerr)
    if (k.eq.0) then
       span = abs(span)
       ph = modulo(lon - olon, span)
       ph = ph / (span / real(noct, kind=KTGT))
       k = FLOOR(ph)

       k = modulo(k + oct_pc_px, noct)

       k = k * 2
       if (ph.ne.AINT(ph)) k = k + 1
       if (k.lt.ref) k = k + ((ref - k - 1) / noct2 + 1) * noct2
    endif
  end function get_octant_ndi_d

!!!_    * octant_lon_d ()
  real(kind=KTGT) function octant_lon_d(zone, cco, zofs, lorg) result(v)
    use TOUZA_Emu_ugg,only: deg2rad, psgp_inquire
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    integer,        intent(in) :: zone
    real(kind=KTGT),intent(in) :: cco(*)
    integer,        intent(in) :: zofs
    real(kind=KTGT),intent(in) :: lorg
    real(kind=KTGT) :: span, olat, olon
    integer,parameter :: noct = 8
    integer k, k0, kadj
    integer jerr

    call psgp_inquire(jerr, cco, olat=olat, olon=olon, lospan=span)
    if (jerr.eq.0) then
       span = abs(span)
       k0 = zofs / 2
       k = zone / 2 - k0
       kadj = floor((lorg - olon) / span)

       k = modulo(k0 - oct_pc_px, noct) + k

       v = span / real(noct, kind=KTGT) * (k + kadj * noct) + olon
    else
       v = - HUGE(0.0_KTGT)
    endif
  end function octant_lon_d

!!!_    * octant_dec_table - tabulate longitude-decomposition
  subroutine octant_dec_table_d &
       & (ierr,     &
       &  jwgt_dec, oref_dec, lwgt,     wtbl_dec, &
       &  doct_dec, lonn_dec, dectbl,   &
       &  lonn_ser, ooffs,    jlonb,    jlone,  symmz)
    implicit none
    integer,parameter     :: KTGT=KDBL
    integer,        intent(out) :: ierr
    integer,        intent(out) :: jwgt_dec(0:*)
    integer,        intent(out) :: oref_dec(0:*)
    integer,        intent(out) :: wtbl_dec(0:*)
    integer,        intent(in)  :: lwgt
    integer,        intent(in)  :: doct_dec(0:*)
    real(kind=KTGT),intent(in)  :: lonn_dec(0:*)
    integer,        intent(in)  :: dectbl(0:*)
    real(kind=KTGT),intent(in)  :: lonn_ser(0:*)
    integer,        intent(in)  :: ooffs(0:*)      ! 0:8
    integer,        intent(in)  :: jlonb, jlone
    integer,        intent(in)  :: symmz(0:*)

    integer jlo, jt, js
    integer koct, kref
    integer obgn, oend
    integer jp
    real(kind=KTGT) :: lonb, lone

    ierr = 0

    ! write(*, *) 'dec_table'
    jp = 0
    wtbl_dec(0) = 0
    do jlo = 0, (jlone - jlonb) - 1
       ! write(*, *) jlo, dectbl(jlo), dectbl(jlo+1)
       do jt = dectbl(jlo), dectbl(jlo+1) - 1
          koct = modulo(doct_dec(jt) / 2, locts)
          kref = symmz(koct)
          if (mod(koct, 2).eq.0) then
             lonb = lonn_dec(jt)
             lone = lonn_dec(jt+1)
          else
             lonb = lonn_dec(jt+1)
             lone = lonn_dec(jt)
          endif
          ! write(*, *) jlo, jt, koct, kref, lonb, lone
          obgn = ooffs(kref)
          oend = ooffs(kref + 1)
          do js = obgn, oend - 1
             if (lonn_ser(js).le.lonb.and.lone.le.lonn_ser(js+1)) then
                ! write(*, *) '  ', js, lonn_ser(js:js+1)
                if (jp.le.lwgt) then
                   jwgt_dec(jp) = js - obgn
                   oref_dec(jp) = kref
                endif
                jp = jp + 1
             endif
          enddo
       enddo
       wtbl_dec(jlo + 1) = jp
       ! write(*, *)
    enddo
    if (jp.gt.lwgt) then
       ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
  end subroutine octant_dec_table_d


!!!_   & is_symmetric_plane
  integer function is_symmetric_plane_d &
       & (xl, dx, mx, yl, dy, my, xo, yo) &
       & result(k)
    use TOUZA_Ami_std,only: choice
    implicit none
    integer,parameter :: KTGT=KDBL
    real(kind=KTGT),intent(in)  :: xl, dx
    real(kind=KTGT),intent(in)  :: yl, dy
    integer,        intent(in)  :: mx, my
    real(kind=KTGT),intent(in),optional  :: xo, yo

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    ! integer jzl, jzu
    real(kind=KTGT) :: zx, zy

    k = sym_none

    k = k + sym_x * symmetric_coor(xl, dx, mx, xo)
    k = k + sym_y * symmetric_coor(yl, dy, my, yo)

    if (abs(dx).eq.abs(dy)) then
       zx = MODULO((xl - choice(ZERO, xo)), dx)
       zy = MODULO((yl - choice(ZERO, yo)), dy)
       if (abs(zx).eq.abs(zy)) k = k + sym_pd
       if (modulo(abs(zx)+abs(zy), abs(dx)).eq.ZERO) k = k + sym_md
    endif

  end function is_symmetric_plane_d

!!!_    * symmetric_coor
  integer function symmetric_coor_d (o, d, m, a) result(k)
    use TOUZA_Ami_std,only: choice
    implicit none
    integer,parameter :: KTGT=KDBL
    real(kind=KTGT),intent(in)  :: o, d
    integer,        intent(in)  :: m
    real(kind=KTGT),intent(in),optional :: a
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT) :: l, h
    real(kind=KTGT) :: zl, zh
    real(kind=KTGT) :: s

    ! return 1 if symmetric along the origin
    integer,parameter :: asymm = 0, symm = 1

    k = asymm
    s = choice(ZERO, a)

    ! l <= h
    if (d.gt.ZERO) then
       l =  o - s
       h = (o - s) + real(m, kind=KTGT) * d
    else
       l = (o - s) + real(m, kind=KTGT) * d
       h = (o - s)
    endif
    if (d.eq.ZERO) then
       k = symm
    else if (l.ge.ZERO) then
       ! dummy symmetry
       zl = l - FLOOR(l / d) * d
       zh = l - CEILING(l / d) * d
       if (ABS(zl).eq.ABS(zh)) k = symm
    else if (h.le.ZERO) then
       ! dummy symmetry
       zl = h + FLOOR(-h / d) * d
       zh = h + CEILING(-h / d) * d
       if (ABS(zl).eq.ABS(zh)) k = symm
    else
       zl = l + FLOOR((-l) / d) * d
       zh = l + CEILING((-l) / d) * d
       if (ABS(zl).eq.ABS(zh)) k = symm
    endif
  end function symmetric_coor_d

!!!_   & set_symmetric_zone
  subroutine set_symmetric_zone(ierr, symmz, ksymm, kasp)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: symmz(0:*)
    integer,intent(in)  :: ksymm
    integer,intent(in)  :: kasp

    ierr = 0
    if (kasp.gt.0) then
       symmz(oct_px_mc) = symmetric_zone(ksymm, 0, (/sym_none/))
       symmz(oct_mc_px) = symmetric_zone(ksymm, 1, (/sym_pd/))
       symmz(oct_mc_mx) = symmetric_zone(ksymm, 2, (/sym_ro, sym_x/))
       symmz(oct_mx_mc) = symmetric_zone(ksymm, 3, (/sym_x,  sym_ro, sym_md/))
       symmz(oct_mx_pc) = symmetric_zone(ksymm, 4, (/sym_xy, sym_md, sym_ro, sym_y/))
       symmz(oct_pc_mx) = symmetric_zone(ksymm, 5, (/sym_md, sym_xy, sym_y,  sym_ro, sym_pd/))
       symmz(oct_pc_px) = symmetric_zone(ksymm, 6, (/sym_ro, sym_y,  sym_xy, sym_pd, sym_ro, sym_x/))
       symmz(oct_px_pc) = symmetric_zone(ksymm, 7, (/sym_y,  sym_ro, sym_pd, sym_xy, sym_x,  sym_ro, sym_md/))
    else
       symmz(oct_px_mc) = symmetric_zone(ksymm, 0, (/sym_none/))
       symmz(oct_mc_px) = symmetric_zone(ksymm, 1, (/sym_md/))
       symmz(oct_mc_mx) = symmetric_zone(ksymm, 2, (/sym_ro, sym_x/))
       symmz(oct_mx_mc) = symmetric_zone(ksymm, 3, (/sym_x,  sym_ro, sym_pd/))
       symmz(oct_mx_pc) = symmetric_zone(ksymm, 4, (/sym_xy, sym_pd, sym_ro, sym_y/))
       symmz(oct_pc_mx) = symmetric_zone(ksymm, 5, (/sym_pd, sym_xy, sym_y,  sym_ro, sym_md/))
       symmz(oct_pc_px) = symmetric_zone(ksymm, 6, (/sym_ro, sym_y,  sym_xy, sym_md, sym_ro, sym_x/))
       symmz(oct_px_pc) = symmetric_zone(ksymm, 7, (/sym_y,  sym_ro, sym_md, sym_xy, sym_x,  sym_ro, sym_pd/))
    endif

  end subroutine set_symmetric_zone

!!!_   & symmetric_zone
  integer function symmetric_zone &
       & (ksymm, ph, ff) &
       & result(k)
    implicit none
    integer,intent(in) :: ksymm
    integer,intent(in) :: ph
    integer,intent(in) :: ff(0:*)
    integer j
    k = ph
    do j = 0, ph - 1
       if (IAND(ksymm, ff(j)).eq.ff(j)) then
          k = j
          exit
       endif
    enddo
  end function symmetric_zone

!!!_   & div_ps2g_map
  subroutine div_ps2g_map_d &
       & (ierr,   iofs,    iprj,    wprj,  &
       &  nmem,   lmem,    &
       &  latn,   wlat,    jlatb,   jlate, mlat,   &
       &  lonn,   dlon,    jlonb,   jlone, mlon,   &
       &  nwedge, wpos,    iniskip, &
       &  xl,     dx,      mx,      yl,    dy,     my,   &
       &  cco,    &
       &  lonlev, latlev,  inilev,  swlev, reqlev, tol,  &
       &  deg,    u,       levv,    tag,   udump)
    use TOUZA_Emu_ugg,only: ncache_psgp_co
    use TOUZA_Emu_ugg,only: psgp_set_step, psgp_bwd_isf, psgp_inquire, psgp_dlo_tr
    use TOUZA_Emu_ugg,only: deg2rad, sind_canonical, cosd_canonical
    use TOUZA_Emu_ugg,only: sin_canonical, cos_canonical
    use TOUZA_Emu_ugg,only: check_monotonic, ang2deg, ang2rad
    use TOUZA_Ami_Std,only: join_list, choice, is_msglev_WARNING
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: lmem                   ! limit size of iprj wprj  (negative to skip check)
    integer,         intent(out) :: iofs(0:*)              ! [(lo,la)+1] to subscript-vector offset (lv)
    integer,         intent(out) :: iprj(0:*)              ! [lv] to g(xy)-index
    real(kind=KTGT), intent(out) :: wprj(0:ps2g_weights-1, 0:*)    ! [lv] to weights
    integer,         intent(out) :: nmem                   ! wprj active members
    real(kind=KTGT), intent(in)  :: lonn(0:*), latn(0:*)   ! node longitude [0:mlon] and latitude [0:mlat] (deg/rad)
    real(kind=KTGT), intent(in)  :: dlon(0:*), wlat(0:*)   ! d lon(deg/rad); [d sin(lat)]
    ! real(kind=KTGT), intent(in)  :: adjlon(0:*)            ! centroid(pivot) longitude adjustment from source representative
    integer,         intent(in)  :: jlonb, jlatb, mlat     ! nlon: jlone - jlonb, physical size
    integer,         intent(in)  :: jlone, jlate, mlon     ! mlon: logical size
    real(kind=KTGT), intent(in)  :: cco(*)                 ! ps2g cache (common)
    real(kind=KTGT), intent(in)  :: xl, dx
    real(kind=KTGT), intent(in)  :: yl, dy
    integer,         intent(in)  :: mx, my
    integer,         intent(in)  :: iniskip,   nwedge
    integer,         intent(in)  :: wpos(0:*)
    integer,         intent(in)  :: lonlev, latlev, inilev, swlev
    integer,         intent(out) :: reqlev                 ! required level limit
    real(kind=KTGT), intent(in)  :: tol
    logical,         intent(in),optional :: deg(:)         ! degree if true, radian otherwise
    integer,         intent(in),optional :: u, levv
    integer,         intent(in),optional :: udump
    character(len=*),intent(in),optional :: tag

    integer,parameter :: mlimit = HUGE(0)

    integer :: mxd, myd

    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: ONE=1.0_KTGT
    real(kind=KTGT),parameter :: TWO=2.0_KTGT

    real(kind=KTGT) :: cci(ncache_psgp_co)
    integer nlon, nlat, jlon, jlat, jtlon, jtlat
    integer jlonph, jlatph
    integer jalon,  jalat, jagh
    real(kind=KTGT) :: tolx
    integer dir_lon, dir_lat, dir_nat
    real(kind=KTGT) :: destl(lxy), desth(lxy), destd(lxy), destdh(lxy)
    real(kind=KTGT) :: lonorg

    integer,parameter :: sparelev = 2
    integer :: symmlev, xstep
    integer :: locache, lacache
    integer :: lotile,  latile
    integer :: jclon_rep          ! cache index for (sub-)cell representative longitude
    integer :: repfac             ! cache length to unit-cell factor

    integer :: lstack
    integer :: mdest
    integer jtabr(lrange, lxy)    ! possibly active table range

    real(kind=KTGT) :: utseg(0:accum_num-1)
    real(kind=KTGT),allocatable :: gslatn(:), dslatn(:), uslatn(:), cglatn(:, :)
    real(kind=KTGT),allocatable :: glonn(:),  dlonn(:),  ulonn(:),  cglonn(:, :)
    real(kind=KTGT),allocatable :: lintg2(:)

    integer,        allocatable :: levco(:)
    integer,        allocatable :: pstack(:, :)
    real(kind=KTGT),allocatable :: ntiles(:, :), ptiles(:, :)
    real(kind=KTGT),allocatable :: clats(:, :), clons(:, :)
    real(kind=KTGT),allocatable :: aplane(:, :, :), aglat(:, :, :), aglon(:, :, :)

    integer,parameter :: lpw2 = BIT_SIZE(0)
    integer :: cstride(0:lpw2)   ! clats clons stride cache
    integer,parameter :: lpw4 = lpw2 + 2
    real(kind=KTGT) :: afact(0:lpw4)   ! power-4 table

    integer jwedge
    integer latflag
    integer swlevx, inilevx
    logical blatc

    integer lv
    integer utmp, ud
    character(len=128) :: txt
    integer j
    integer jerr

    real(kind=KTGT) :: lospan
    real(kind=KTGT) :: sref, schk, snt, spt
    real(kind=KTGT) :: negmaxa
    real(kind=KTGT) :: rlon(NTRIG)
    ! real(kind=KTGT) :: lo0, lo1, la0, la1

    ierr = 0

    utmp = get_logu(u, ulog)
    ud = choice(-1, udump)
    lv = choice(lev_verbose, levv)

    ! health check
    if (mx.gt.0.and.my.gt.0) then
       if ((mlimit / mx) .lt. my) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       else if (mlimit / (mx * my) .lt. lxy) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
    endif
    if (ierr.ne.0) then
109    format('maximum size reached: ', I0, 1x, I0)
       write(txt, 109) mx, my
       call msg(txt)
       return
    endif

    ! reset
    iofs(0) = 0
    nmem = 0
    reqlev = 0
    negmaxa = ZERO

    nlon = jlone - jlonb
    nlat = jlate - jlatb

    tolx = set_tolerance(tol)

    dir_lon = check_monotonic(lonn(jlonb:jlone), nlon+1)
    dir_lat = check_monotonic(latn(jlatb:jlate), nlat+1)
    dir_nat = - dir_lon * dir_lat
    ! dir_nat is positive if (lon=0,lat=0) -- (+1,0) -- (+1,+1) -- (0,+1)
    ! is clockwise
    ! lon+0  +1
    !     0--1   lat+0
    !        |
    !     3--2   lat+1
    call msg('(''natural direction = '', I0)', (/dir_nat/))
    if (dir_nat.eq.0) then
119    format('direction: ', I0, 1x, I0)
       write(txt, 119) dir_lon, dir_lat
       call msg(txt)
       ierr = _ERROR(ERR_INVALID_PARAMETER)
       return
    endif

    ! normalize psgp parameters
    lonorg = ZERO
    cci(1:ncache_psgp_co) = cco(1:ncache_psgp_co)
    if (is_deg(deg, JLONGI)) then
       lospan = 360.0_KTGT
    else
       lospan = 0.0_KTGT
    endif
#define LON_DEGREE 1
! #define LON_DEGREE 0
#if LON_DEGREE
#else
    lospan = 0.0_KTGT
#endif
    call psgp_set_step(cci, dx, dy, lonorg, lospan=lospan)

    destd(1:2) = ONE
    destdh(1:2) = destd(1:2) / TWO
    destl(1:2) = (/xl, yl/) / (/dx, dy/)
    desth(1:2) = destl(1:2) + real((/mx, my/), kind=KTGT)

    symmlev = min(lonlev, latlev)
    if (symmlev + sparelev.gt.lpw2) then
129    format('panic: symmetric level: ', I0, 1x, I0, 1x, I0)
       write(txt, 129) symmlev, sparelev, lpw2
       call msg(txt)
       ierr = _ERROR(ERR_PANIC)
       return
    endif

    lotile  = 2 ** lonlev
    latile  = 2 ** latlev
    locache = 2 ** (lonlev + sparelev)
    lacache = 2 ** (latlev + sparelev)
    xstep   = 2 ** (symmlev + sparelev)
    repfac  = 2 ** sparelev

    sref = 2.0_KTGT**(lonlev + latlev)

    cstride(0) = xstep
    do j = 1, symmlev + sparelev
       cstride(j) = cstride(j-1) / 2
    enddo
    do j = 0, symmlev + sparelev
       afact(j) = 4.0_KTGT ** (symmlev - j)
    enddo

    lstack = 1 + 3 * symmlev
    mdest  = mx * my
    mxd    = mx * 2
    myd    = my * 2

    if (ierr.eq.0) allocate(levco(0:symmlev), STAT=ierr)
    if (ierr.eq.0) allocate(pstack(prop_psg_size, 0:lstack), STAT=ierr)
    if (ierr.eq.0) allocate(ntiles(0:mdest, 0:accum_num-1), ptiles(0:mdest, 0:accum_num-1), STAT=ierr)
    if (ierr.eq.0) allocate(clats(mcla, 0:lacache), clons(mclo, 0:locache), STAT=ierr)
    if (ierr.eq.0) allocate(aglat(NTRIG, 0:mxd, 0:myd), aglon(NTRIG, 0:mxd, 0:myd), STAT=ierr)
    if (ierr.eq.0) allocate(aplane(lxy, 0:mxd, 0:myd), STAT=ierr)
    if (ierr.eq.0) allocate(gslatn(0:nlat), dslatn(0:nlat-1), uslatn(0:nlat-1), STAT=ierr)
    if (ierr.eq.0) allocate(glonn(0:nlon), dlonn(0:nlon-1), ulonn(0:nlon-1), STAT=ierr)
    if (ierr.eq.0) allocate(lintg2(0:lacache), STAT=ierr)
    if (ierr.eq.0) allocate(cglatn(mcla, 0:nlat), cglonn(mclo, 0:nlon), STAT=ierr)

    ! clon(:, lcache+1) is for longitude origin special
    if (ierr.ne.0) then
       ierr = _ERROR(ERR_ALLOCATION)
       return
    endif

    if (ierr.eq.0) then
       call set_coor_anchor(aplane, aglat, aglon, destdh, destl, mxd, myd, cci)

       call set_gslat_node &
            & (gslatn, dslatn, uslatn, cglatn, &
            &  latn,   wlat,   jlatb,  jlate,  latile, cci, dir_lat, deg)
       call set_grlon_node &
            & (ierr,   &
            &  glonn,  dlonn,  ulonn,  cglonn, &
            &  lonn,   dlon,   jlonb,  jlone,  lotile, cci, dir_lon, deg)
    endif

    ntiles(:,:) = ZERO
    ptiles(:,:) = ZERO

    ! divides

101 format('ps2g:i: ', I0, ',', I0)
102 format('ps2g:o: ', I0, ',', I0)
103 format('ps2g:x: ', I0, ',', I0)
    do jlat = 0, nlat - 1
       blatc = .FALSE.
       jalon = 0
       jwedge = 1
       do jlon = 0, nlon - 1
          jtabr(rmax, :) = - HUGE(0)
          jtabr(rmin, :) = + HUGE(0)

          jlonph = jlon + jlonb

          ! a cell = {wpos-1 wpos wpos+1}

          !  case no subcell         case subcell division
          !     |===cell==|          |========|========|      source cell
          !     l   rep  l+1         l       rep      l+1
          !     o----+----o          o-----o--X--------o
          !                          | rep |    rep    |
          !                          |--x--W-----x-----|      sub cell division at a wedge
          !                         w-1    w          w+1

          ! distance between representatives of the source cell and (sub-)cell
          jclon_rep = locache / 2

          if (dlonn(jlon).eq.0.0_KTGT) then
             if (is_msglev_DETAIL(lv)) then
                write(txt, 103) jlat+jlatb, jlon+jlonb
                call msg(txt, tag, utmp)
             endif
             utseg(accum_w1lo) = abs(lonn(jlon+1) - lonn(jlon))
             utseg(accum_w1la) = utseg(accum_w1lo)
             utseg(accum_w0) = abs(dslatn(jlat)) * utseg(accum_w1lo)
             ptiles(mdest, accum_w0) = ZERO
             ntiles(mdest, accum_w0) = ONE
             ptiles(mdest, accum_w1la) = ZERO
             ntiles(mdest, accum_w1la) = ZERO
             ptiles(mdest, accum_w1lo) = ZERO
             ntiles(mdest, accum_w1lo) = ZERO
          else if (is_seg_intersect(cglatn, cglonn, jlat, jlat+1, jlon, jlon+1, destl, desth)) then
             if (is_msglev_INFO(lv)) then
                write(txt, 101) jlat+jlatb, jlon+jlonb
                call msg(txt, tag, utmp)
             endif
             utseg(accum_w0) = abs(uslatn(jlat) * ulonn(jlon))
             utseg(accum_w1lo) = abs(ulonn(jlon))
             utseg(accum_w1la) = abs(ulonn(jlon))

             if (.not.blatc) then
                jlatph = jlat + jlatb
                call set_seg_lat_cache(clats, lintg2, gslatn(jlat), gslatn(jlat+1), dslatn(jlat), cci, lacache)
                blatc = .TRUE.
             endif
             call set_seg_rlon_cache(clons, glonn(jlon), glonn(jlon+1), dlonn(jlon), cci, locache)

             levco(0:symmlev) = 0
             ! center longitude division may affect switching and initial levels
             inilevx = min(max(0, inilev), symmlev)
             swlevx =  swlev
             if (swlevx.lt.0) swlevx = (symmlev + 1 + swlevx)

             do jtlat = 0, lacache - 1, xstep
                latflag = latcell_normal
                if (jtlat.eq.0) latflag = latflag + latcell_upb
                if (jtlat+xstep.eq.lacache) latflag = latflag + latcell_lowb
                do jtlon = 0, locache - 1, xstep
                   call div_ps2g_cell &
                        & (ntiles,  ptiles,  levco,   jtabr,   pstack,  negmaxa, &
                        &  reqlev,  symmlev, inilevx, swlevx,  &
                        &  clats,   jtlat,   clons,   jtlon,   &
                        &  afact,   cstride, lintg2,  &
                        &  uslatn(jlat), ulonn(jlon), lospan, dir_nat, latflag, jclon_rep, repfac, &
                        &  aplane, mx,  my, mdest, destl,   &
                        &  cci, tolx, lv, tag, ud, jlatph, jlonph)
                enddo
             enddo
#define _FMT_ 1x, F0.3
201          format('ps2g:sum:F: ', I0, 1x, I0, 1x, E10.3, 4(1x, E12.3))
202          format('ps2g:sum:S: ', I0, 1x, I0, 1x, E10.3, 4(_FMT_))
#undef _FMT_
             if (is_msglev_INFO(lv)) then
                snt = SUM(ntiles(0:mdest, accum_w0))
                spt = SUM(ptiles(0:mdest, accum_w0))
                schk = snt + spt
                if (schk.ne.sref) then
                   write(txt, 201) jlatph, jlonph, schk - sref, sref, schk, snt, spt
                   call msg(txt, tag, utmp)
                else if (is_msglev_DETAIL(lv)) then
                   write(txt, 202) jlatph, jlonph, schk - sref, sref, schk, snt, spt
                   call msg(txt, tag, utmp)
                endif
             endif
             if (is_msglev_DETAIL(lv)) then
                call join_list(jerr, txt, levco(0:symmlev))
                call msg(txt, tag, utmp)
             endif
          else
             if (is_msglev_DETAIL(lv)) then
                write(txt, 102) jlat+jlatb, jlon+jlonb
                call msg(txt, tag, utmp)
             endif
             utseg(accum_w0) = abs(dslatn(jlat) * dlonn(jlon))
             utseg(accum_w1lo) = abs(dlonn(jlon))
             utseg(accum_w1la) = abs(dlonn(jlon))
             ptiles(mdest, accum_w0) = ZERO
             ntiles(mdest, accum_w0) = ONE
             ptiles(mdest, accum_w1la) = ZERO
             ntiles(mdest, accum_w1la) = ZERO
             ptiles(mdest, accum_w1lo) = ZERO
             ntiles(mdest, accum_w1lo) = ZERO
          endif

          call psgp_dlo_tr(rlon(1:NTRIG), clons(:, jclon_rep), cco)
          ! store result (clear ntiles ptiles)
          ! write(*, *) 'utseg:', jlat, jlon, deg2rad(utseg(:))
          utseg(:) = ang2rad(utseg(:), lospan)
          ! write(*, *) 'utseg:', jlat, jlon, utseg(:)
          call ps2g_store_fwd &
               & (iprj,   wprj,   nmem,  &
               &  ntiles, ptiles, lmem,  &
               &  aglon,  rlon,   &
               &  mx,     my,     mdest, jtabr,  utseg)

          jalat = jlat
          jagh = jalat * mlon + jalon
          ! a cell = {wpos-1 wpos wpos+1}
          if (jlonph+1.ge.wpos(jwedge)) then
             iofs(jagh+1) = nmem
             jwedge = jwedge + 1
          else
             iofs(jagh+1) = nmem
             jalon = jalon + 1
          endif
       enddo
    enddo
    if (ierr.eq.0) then
       if (negmaxa.lt.ZERO) then
          if (is_msglev_WARNING(lv)) then
141          format('negative area piece detected: ', ES13.3)
             write(txt, 141) negmaxa
             call msg(txt)
          endif
       endif
    endif
    if (ierr.eq.0) deallocate(levco, pstack, ntiles, ptiles, STAT=ierr)
    if (ierr.eq.0) deallocate(clats, clons, STAT=ierr)
    if (ierr.eq.0) deallocate(aglat, aglon, aplane, STAT=ierr)
    if (ierr.eq.0) deallocate(gslatn, dslatn, uslatn, glonn, dlonn, ulonn, lintg2, STAT=ierr)
    if (ierr.eq.0) deallocate(cglatn, cglonn, STAT=ierr)
    return
  end subroutine div_ps2g_map_d

!!!_   & ps2g_store_fwd
  subroutine ps2g_store_fwd_d &
       & (iprj,   wprj,   nmem, &
       &  ntiles, ptiles, lmem,  &
       &  aglon,  rlon,   mx, my, mdest, jtabr,  utseg)
    use TOUZA_Emu_ugg,only: sub_angle, phase, rad2deg
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,        intent(in)    :: lmem
    integer,        intent(inout) :: iprj(0:*)
    real(kind=KTGT),intent(inout) :: wprj(0:ps2g_weights-1, 0:*)
    integer,        intent(inout) :: nmem
    integer,        intent(in)    :: mx, my, mdest
    real(kind=KTGT),intent(inout) :: ntiles(0:mdest,0:*)     ! [0:mx*my]  must be reset before call  (larger)
    real(kind=KTGT),intent(inout) :: ptiles(0:mdest,0:*)     ! [0:mx*my]  must be reset before call  (smaller)
    real(kind=KTGT),intent(in)    :: aglon(:, 0:, 0:)
    real(kind=KTGT),intent(in)    :: rlon(*)
    integer,        intent(in)    :: jtabr(lrange, *)        ! possibly active destination range
    real(kind=KTGT),intent(in)    :: utseg(0:*)

    real(kind=KTGT) :: t0, t1lo, t1la
    integer jdx, jdy, jdh
    integer nmbgn, nmend
    real(kind=KTGT) :: blon(NTRIG), dlon(NTRIG)

    nmbgn = nmem
    do jdy = jtabr(rmin, coor_y), jtabr(rmax, coor_y)
       do jdx = jtabr(rmin, coor_x), jtabr(rmax, coor_x)
          jdh = jdy * mx + jdx
          if (ntiles(jdh,accum_w0).gt.0.0_KTGT.or.ptiles(jdh,accum_w0).gt.0.0_KTGT) then
             t0 = ntiles(jdh,accum_w0) + ptiles(jdh,accum_w0)
             t1la = ntiles(jdh,accum_w1la) + ptiles(jdh,accum_w1la)
             t1lo = ntiles(jdh,accum_w1lo) + ptiles(jdh,accum_w1lo)

             blon(:) = aglon(:, jdx * 2 + 1, jdy * 2 + 1)
             dlon(:) = sub_angle(rlon, blon)
#if DEBUG_AMI_SYMM
             write(*, *) 'dcent:ph:', jdx, jdy, rad2deg(phase(dlon)), rad2deg(phase(blon)), rad2deg(phase(rlon(1:NTRIG)))
             write(*, *) 'dcent:tr:', jdx, jdy, dlon, blon, rlon(1:NTRIG)
#endif /* DEBUG_AMI_SYMM */
             if (lmem.lt.0.or.nmem.lt.lmem) then
                iprj(nmem) = jdh
                wprj(ps2g_w0, nmem) = t0
                wprj(ps2g_w1bla, nmem) = t1la
                wprj(ps2g_w1blo, nmem) = t1lo
                ! temporary store centroid adjustment
                wprj(ps2g_w1fla, nmem) = 0.0_KTGT
                wprj(ps2g_w1flo, nmem) = phase(dlon)
             endif
             nmem = nmem + 1
             ntiles(jdh,0:accum_num-1) = 0.0_KTGT
             ptiles(jdh,0:accum_num-1) = 0.0_KTGT
          endif
       enddo
    enddo

    jdh = mdest
    if (ntiles(jdh,accum_w0).gt.0.0_KTGT.or.ptiles(jdh,accum_w0).gt.0.0_KTGT) then
       t0 = ntiles(jdh,accum_w0) + ptiles(jdh,accum_w0)
       t1la = ntiles(jdh,accum_w1la) + ptiles(jdh,accum_w1la)
       t1lo = ntiles(jdh,accum_w1lo) + ptiles(jdh,accum_w1lo)
       if (lmem.lt.0.or.nmem.lt.lmem) then
          iprj(nmem) = jdh
          wprj(ps2g_w0, nmem) = t0
          wprj(ps2g_w1bla, nmem) = t1la
          wprj(ps2g_w1blo, nmem) = t1lo
          wprj(ps2g_w1fla, nmem) = 0.0_KTGT
          wprj(ps2g_w1flo, nmem) = 0.0_KTGT
       endif
       nmem = nmem + 1
       ntiles(jdh,0:accum_num-1) = 0.0_KTGT
       ptiles(jdh,0:accum_num-1) = 0.0_KTGT
    endif

    nmend = min(nmem, lmem)

    wprj(ps2g_w0, nmbgn:nmend-1) = wprj(ps2g_w0, nmbgn:nmend-1) * utseg(accum_w0)
    ! [NOTE] store fwd weights to bwd array temporally

    wprj(ps2g_w1bla, nmbgn:nmend-1) = wprj(ps2g_w1bla, nmbgn:nmend-1) * utseg(accum_w1la)
    wprj(ps2g_w1blo, nmbgn:nmend-1) = wprj(ps2g_w1blo, nmbgn:nmend-1) * utseg(accum_w1lo) * utseg(accum_w0)

    wprj(ps2g_w1fla, nmbgn:nmend-1) = wprj(ps2g_w1bla, nmbgn:nmend-1)
    wprj(ps2g_w1flo, nmbgn:nmend-1) = wprj(ps2g_w1blo, nmbgn:nmend-1) &
         & + wprj(ps2g_w1flo, nmbgn:nmend-1) * wprj(ps2g_w0, nmbgn:nmend-1)

    ! store unit segment in fwd array temporally
    ! wprj(ps2g_w1fla, nmbgn:nmend-1) = utseg(accum_w1la)
    ! wprj(ps2g_w1flo, nmbgn:nmend-1) = utseg(accum_w1lo) * utseg(accum_w0)

    ! asum = SUM(wprj(ps2g_w0, :)) / utseg(accum_w0)
    ! centloid lat = [SUM(wprj(ps2g_w1fla, :)) / utseg(accum_w1la)]
    !                * utseg(accum_w1la) / asum / utseg(accum_w0)
    !              = SUM(wprj(ps2g_w1fla, :)) / SUM(wprj(ps2g_w0, :))
    ! centloid lon = [SUM(wprj(ps2g_w1flo, :)) / utseg(accum_w1lo) / utseg(accum_w0)]
    !                * utseg(accum_w1lo) / asum + adj_lonsub
    !              = SUM(wprj(ps2g_w1flo, :)) / SUM(wprj(ps2g_w0, :)) + adj_lonsub

  end subroutine ps2g_store_fwd_d

!!!_   & coeff2_line_integ
  real(kind=KTGT) function coeff2_line_integ_d(s) result(v)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: s  ! sin(lat)
    real(kind=KTGT) :: c, la

    c = SQRT((1.0_KTGT + s) * (1.0_KTGT - s))
    la = ASIN(s)

    v = - c - la * s
  end function coeff2_line_integ_d

!!!_   & coeff3_line_integ
  real(kind=KTGT) function coeff3_line_integ_d(s) result(v)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: s  ! sin(lat)
    real(kind=KTGT) :: c, la

    c = SQRT((1.0_KTGT + s) * (1.0_KTGT - s))
    la = ASIN(s)

    v = - (c * s + la) / 2.0_KTGT
  end function coeff3_line_integ_d

!!!_   & div_ps2g_cell
  subroutine div_ps2g_cell_d &
       & (ntiles,  ptiles,  levco,   jtabr,   pstack,  negmaxa, &
       &  reqlev,  symmlev, inilev,  swlev,   &
       &  cachela, jlatc,   cachelo, jlonc,   afact,   cstride, lintg2,  &
       &  uslat,   ulon,    lospan,  dir_nat, latflag, jclon_rep, repfac, &
       &  aplane,  mx,      my,      mdest,   destl,   &
       &  cco,     tol,     levv,    tag,     udump,   jlatph,  jlonph)
    use TOUZA_Emu_ugg,only: psgp_fwd, ang2rad
    use TOUZA_Ami_std,only: inrange, condop
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,parameter :: KDBG=KFLT
    integer,         intent(in)    :: mx, my, mdest
    real(kind=KTGT), intent(inout) :: ntiles(0:mdest,0:*)       ! [0:mx*my]  must be reset before call  (larger)
    real(kind=KTGT), intent(inout) :: ptiles(0:mdest,0:*)       ! [0:mx*my]  must be reset before call  (smaller)
    integer,         intent(inout) :: levco(0:*)                ! need initialize before call
    integer,         intent(inout) :: jtabr(lrange, *)          ! need initialize before call
    integer,         intent(inout) :: pstack(prop_psg_size,0:*) ! work area
    integer,         intent(inout) :: reqlev
    real(kind=KTGT), intent(inout) :: negmaxa
    real(kind=KTGT), intent(in)    :: destl(*)
    real(kind=KTGT), intent(in)    :: cachelo(:, 0:)
    real(kind=KTGT), intent(in)    :: cachela(:, 0:)
    integer,         intent(in)    :: jlatc, jlonc
    real(kind=KTGT), intent(in)    :: afact(0:*)
    integer,         intent(in)    :: cstride(0:*)
    real(kind=KTGT), intent(in)    :: lintg2(0:*)        ! weight2 line intagrand unit
    ! real(kind=KTGT), intent(in)    :: utseg            ! unit area of tiny segment
    real(kind=KTGT), intent(in)    :: ulon, uslat        ! unit longitude,sine(latitude) width
    real(kind=KTGT), intent(in)    :: lospan
    ! real(kind=KTGT), intent(in)    :: bcflat(0:*)        ! integrand for second order coefficients
    integer,         intent(in)    :: dir_nat            ! positive if natural direction is clockwise
    integer,         intent(in)    :: latflag            ! bitwise switch of latitude boundary condition
    integer,         intent(in)    :: jclon_rep, repfac    ! representative longitude index and conversion factor
    real(kind=KTGT), intent(in)    :: aplane(:, 0:, 0:)  ! xy-coordinate cache (:, mxd, myd)
    integer,         intent(in)    :: symmlev
    integer,         intent(in)    :: inilev, swlev
    real(kind=KTGT), intent(in)    :: cco(*)
    real(kind=KTGT), intent(in)    :: tol
    integer,         intent(in)    :: levv
    character(len=*),intent(in)    :: tag
    integer,         intent(in)    :: udump
    integer,         intent(in)    :: jlatph, jlonph

    integer,parameter :: nnode = 4, xnode = 5, nquad = 4
    real(kind=KTGT) :: gpos(lxy, nnode)
    real(kind=KTGT) :: gidx(lxy, nnode)
    integer         :: gdbl(lxy, nnode)
    integer         :: gmin(lxy), gmax(lxy)

    real(kind=KTGT) :: f3, f2, rf
    ! real(kind=KTGT) :: li2u, li2l
    real(kind=KTGT) :: udlon

    integer iwidth
    integer jfla, jflo
    integer jcla, jclo
    integer jtla, jtlo
    integer jpos, jadd
    integer cstep, klev
    integer jdh,   jdext,  jdx, jdy
    integer mxd,   myd
    integer cellstt
    character(len=128) :: txt
    ! logical bucell, blcell

    iwidth = 2 ** inilev
    jdext  = mx * my
    mxd    = mx * 2
    myd    = my * 2
    rf = real(repfac, kind=KIND(rf))

    do jfla = 0, iwidth - 1
       do jflo = 0, iwidth - 1
          jpos = 0
          ! set base cell
          klev = inilev
          pstack((/prop_gtlo, prop_gtla, prop_gtlev/), jpos) = (/jflo, jfla, klev/)
          cstep = cstride(klev)
          jclo = jflo * cstep + jlonc
          jcla = jfla * cstep + jlatc

          ! cell division      node position
          !  2 1                3 4
          !  4 3                1 2
          gpos(:, 1) = psgp_fwd(cachelo(:, jclo),         cachela(:, jcla))
          gpos(:, 2) = psgp_fwd(cachelo(:, jclo + cstep), cachela(:, jcla))
          gpos(:, 3) = psgp_fwd(cachelo(:, jclo),         cachela(:, jcla + cstep))
          gpos(:, 4) = psgp_fwd(cachelo(:, jclo + cstep), cachela(:, jcla + cstep))

          gidx(coor_x, 1:nnode) = (gpos(coor_x, 1:nnode) - destl(coor_x))
          gidx(coor_y, 1:nnode) = (gpos(coor_y, 1:nnode) - destl(coor_y))

          ! write(*, *) 'clo', cachelo(:, jclo)
          ! write(*, *) 'cla', cachela(:, jcla)
          ! write(*, *) 'clo', cachelo(:, jclo + cstep)
          ! write(*, *) 'cla', cachela(:, jcla + cstep)
          ! write(*, *) 'gpos', gpos(:, 1), gidx(:, 1)
          ! write(*, *) 'gpos', gpos(:, 2), gidx(:, 2)
          ! write(*, *) 'gpos', gpos(:, 3), gidx(:, 3)
          ! write(*, *) 'gpos', gpos(:, 4), gidx(:, 4)
          ! notes: floor + ceiling  == even when exactly on the cell boundary
          !                         == odd  when between the cell boundaries (exclusive)
          ! 0   1   2   3  plain index
          ! o---o---o---o
          ! 0 1 2 3 4 5 6  doubled index
          gdbl(:, 1:nnode) = floor(gidx(:, 1:nnode) + tol) + ceiling(gidx(:, 1:nnode) - tol)

          pstack(prop_psg_x1:prop_psg_x4, jpos) = gdbl(coor_x, 1:nnode)
          pstack(prop_psg_y1:prop_psg_y4, jpos) = gdbl(coor_y, 1:nnode)

          ! recursion
          loop_recurse: do
             if (jpos.lt.0) exit loop_recurse

             gmin(coor_x) = minval(pstack(prop_psg_x1:prop_psg_x4, jpos))
             gmax(coor_x) = maxval(pstack(prop_psg_x1:prop_psg_x4, jpos))
             gmin(coor_y) = minval(pstack(prop_psg_y1:prop_psg_y4, jpos))
             gmax(coor_y) = maxval(pstack(prop_psg_y1:prop_psg_y4, jpos))

             klev = pstack(prop_gtlev, jpos)

             cstep = cstride(klev)
             jcla = pstack(prop_gtla, jpos) * cstep + jlatc
             jclo = pstack(prop_gtlo, jpos) * cstep + jlonc

             ! bucell = (jcla.eq.0) .and. (IAND(latflag, latcell_upb).ne.0)
             ! blcell = (jcla+cstep.eq.cstride(0)) .and. (IAND(latflag, latcell_lowb).ne.0)
             ! li2u = bcflat(accum_w1lau) * real(condop(bucell, - dir_nat, 0), KIND=KTGT)
             ! li2l = bcflat(accum_w1lal) * real(condop(blcell, + dir_nat, 0), KIND=KTGT)
             ! dir_bu = condop(bucell, - dir_nat, 0)
             ! dir_bl = condop(blcell, + dir_nat, 0)

             ! out of destination domain
             if (gmax(coor_x).le.0 .or. mxd.le.gmin(coor_x) &
                  & .or. gmax(coor_y).le.0 .or. myd.le.gmin(coor_y)) then
                jdh = jdext
                ! jclo:jclo+cstep
                f2 = real(cstep, kind=KTGT) / rf
                f2 = f2 * (lintg2(jcla + cstep) - lintg2(jcla))
                f3 = real(2 * (jclo - jclon_rep) + cstep, KIND=KTGT) / (2.0_KTGT * rf)
                ntiles(jdh,accum_w0) = ntiles(jdh,accum_w0) + afact(klev)
                ntiles(jdh,accum_w1la) = ntiles(jdh,accum_w1la) + f2
                ntiles(jdh,accum_w1lo) = ntiles(jdh,accum_w1lo) + afact(klev) * f3

                levco(klev) = levco(klev) + 1

                jpos = jpos - 1
                cycle loop_recurse
             endif
             ! negative floor already excluded

             cellstt = is_adjacent_cells(gmin(:), gmax(:))
             if (cellstt.eq.stt_same) then
                ! all nodes in the same destination
                jdx = gmin(coor_x) / 2
                jdy = gmin(coor_y) / 2
                jdh = jdy * mx + jdx

                f2 = real(cstep, kind=KTGT) / rf
                f3 = real(2 * (jclo - jclon_rep) + cstep, KIND=KTGT) / (2.0_KTGT * rf)
                f2 = f2 * (lintg2(jcla + cstep) - lintg2(jcla))
                ntiles(jdh,accum_w0) = ntiles(jdh,accum_w0) + afact(klev)
                ntiles(jdh,accum_w1la) = ntiles(jdh,accum_w1la) + f2
                ntiles(jdh,accum_w1lo) = ntiles(jdh,accum_w1lo) + afact(klev) * f3

                levco(klev) = levco(klev) + 1

                jtabr(rmax, coor_x:coor_y) = max(jtabr(rmax, coor_x:coor_y), (/jdx, jdy/))
                jtabr(rmin, coor_x:coor_y) = min(jtabr(rmin, coor_x:coor_y), (/jdx, jdy/))

                jpos = jpos - 1
                cycle loop_recurse
             else if (cellstt.lt.stt_discont.and.klev.ge.swlev) then
                ! adjacent cell distribution
                udlon = ulon
                if (lospan.ne.0.0_KTGT) udlon = ang2rad(udlon, lospan)
                call div_ps2g_neighbor &
                     & (ntiles,    ptiles,  jtabr,   negmaxa, &
                     &  gmax,      gmin,    &
                     &  afact,     cstride, pstack,  cachelo, jlonc,   cachela, jlatc, cco,     &
                     &  udlon,     uslat,   dir_nat, aplane,  &
                     &  mx,        my,      mdest,   jdext,   jpos,    latflag, &
                     &  jclon_rep, rf,      lintg2,  &
                     &  levv,      jlonph,  jlatph,  tag,     udump)
                levco(klev) = levco(klev) + 1

                jpos = jpos - 1
             else if (cellstt.lt.stt_discont .and. swlev.eq.ps2g_fast_fallback) then
                ! simple fallback
                call div_ps2g_simple_fallback &
                     & (ptiles, jtabr, &
                     &  gmax,   gmin,  afact,     cstride, pstack, &
                     &  jlatc,  jlonc, jclon_rep, rf,      lintg2, &
                     &  mx,     my,    mdest,     jdext,   jpos)
302             format('Reached simple fallback: (', I0, ',', I0, ') [', I0, ',', I0, ']')
                reqlev = max(reqlev, klev)
                if (swlev.le.symmlev) then
                   jtlo = pstack(prop_gtlo, jpos)
                   jtla = pstack(prop_gtla, jpos)
                   write(txt, 302) jlonph, jlatph, jtlo, jtla
                   call msg(txt, __MDL__)
                endif
                jpos = jpos - 1
                cycle loop_recurse
             else if (klev.ge.symmlev) then
                ! fallback
                if (cellstt.lt.stt_discont) then
                   reqlev = max(reqlev, klev)
                else
                   reqlev = max(reqlev, 1 + klev)
                endif
                call div_ps2g_fallback &
                     & (ptiles,  jtabr,   &
                     &  afact,   cstride, pstack,  &
                     &  cachelo, jlonc,   cachela, jlatc,  jclon_rep, rf, lintg2, &
                     &  destl,   mx,      my,      mdest,  jdext,     &
                     &  jpos,    tol,     jlonph,  jlatph, udump)
301             format('Reached fallback: (', I0, ',', I0, ') [', I0, ',', I0, ']')
                if (swlev.le.symmlev) then
                   jtlo = pstack(prop_gtlo, jpos)
                   jtla = pstack(prop_gtla, jpos)
                   write(txt, 301) jlonph, jlatph, jtlo, jtla
                   call msg(txt, __MDL__)
                endif
                jpos = jpos - 1
             else
                ! non adjacent cells
                call div_ps2g_advance &
                     & (pstack,  jadd,    jpos, &
                     &  cstride, cachela, jlatc, cachelo, jlonc, destl, tol)
                jpos = jpos + jadd
                cycle loop_recurse
             endif
          enddo loop_recurse
       enddo
    enddo
  end subroutine div_ps2g_cell_d

!!!_   & div_ps2g_neighbor
  subroutine div_ps2g_neighbor_d &
       & (ntiles,    ptiles,  jtabr,   negmaxa, &
       &  gmax,      gmin,    &
       &  afact,     cstride, pstack,  cachelo, jlonc, cachela, jlatc, cco, &
       &  ulon,      uslat,   dir_nat, aplane,  &
       &  mx,        my,      mdest,   jdext,   jpos,  latflag, &
       &  jclon_rep, repfac,  lintg2,  &
       &  levv,      jlonph,  jlatph,  tag,     udump)
    use TOUZA_Emu_ugg,only: psgp_fwd, psgp_dlo_tr, psgp_gla_tr
    use TOUZA_Emu_ugg,only: psgp_bwd_tr
    use TOUZA_Emu_ugg,only: hpsub_angle, sub_angle
    implicit none
    integer,parameter  :: KTGT=KDBL
    integer,         intent(in)    :: mx, my, mdest
    real(kind=KTGT), intent(inout) :: ntiles(0:mdest, 0:*)
    real(kind=KTGT), intent(inout) :: ptiles(0:mdest, 0:*)
    integer,         intent(inout) :: jtabr(lrange, *)
    real(kind=KTGT), intent(inout) :: negmaxa
    integer,         intent(in)    :: gmax(*),  gmin(*)
    real(kind=KTGT), intent(in)    :: afact(0:*)    ! for unit area
    integer,         intent(in)    :: cstride(0:*)
    integer,         intent(in)    :: pstack(prop_psg_size,0:*) ! work area
    real(kind=KTGT), intent(in)    :: cachelo(:, 0:)
    real(kind=KTGT), intent(in)    :: cachela(:, 0:)
    integer,         intent(in)    :: jlonc, jlatc
    real(kind=KTGT), intent(in)    :: cco(*)
    real(kind=KTGT), intent(in)    :: ulon, uslat
    integer,         intent(in)    :: dir_nat
    real(kind=KTGT), intent(in)    :: aplane(:, 0:, 0:)
    integer,         intent(in)    :: jdext
    integer,         intent(in)    :: jpos
    integer,         intent(in)    :: latflag
    integer,         intent(in)    :: jclon_rep
    real(kind=KTGT), intent(in)    :: repfac
    real(kind=KTGT), intent(in)    :: lintg2(0:*)
    integer,         intent(in)    :: levv
    integer,         intent(in)    :: jlonph, jlatph
    character(len=*),intent(in)    :: tag
    integer,         intent(in)    :: udump

    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: HALF=0.5_KTGT
    real(kind=KTGT),parameter :: ONE=1.0_KTGT
    real(kind=KTGT),parameter :: TWO=2.0_KTGT
    integer,parameter :: nnode = 4, nquad = 4, nqx = 2
    integer klev, cstep
    integer jcla, jcla1, jcla2, jcla3, jcla4, jmla
    integer jclo, jclo1, jclo2, jclo3, jclo4, jmlo

    real(kind=KTGT) :: rlonn(0:nnode)   ! normalized relative longitude from rep
    real(kind=KTGT) :: uone, udlon
    real(kind=KTGT) :: uaseg(0:accum_num-1)
    integer jq
    integer jxa, jya

    integer :: gdbl(lxy, nnode), relpos(nnode)
    integer :: adjstt(nnode)
    real(kind=KTGT) :: xlat(NTRIG), xlon(NTRIG)
    real(kind=KTGT) :: gpos(lxy, nnode), xpos(lxy)
    real(kind=KTGT) :: pmin(lxy),  pmax(lxy)

    real(kind=KTGT) :: gsegco(NGEOG, node_0:lnodes)   ! segment coordinates (geographic)

    real(kind=KTGT) :: aside(0:lnodes, 0:accum_num-1)
    integer ngraph(0:lnodes + 1)
    integer nprops(1:lnodes)
    real(kind=KTGT) :: glat(NTRIG, 0:lnodes), glon(NTRIG, 0:lnodes)
    real(kind=KTGT) :: rlona(NTRIG), xrlon
    real(kind=KTGT) :: dlon(NTRIG)

    integer,parameter :: agmp_order = OPT_AMI_AGMP_ORDER

    integer cflag, nseq
    integer jseq
    integer jxsec, jysec
    integer jf, jt
    integer jc
    integer jdh, jlx, jly, jdx, jdy
    real(kind=KTGT) :: asec(node_xi:node_yo, 0:accum_num-1)
    real(kind=KTGT) :: adiv(nquad, 0:accum_num-1)
    real(kind=KTGT) :: anum, arem, atmp
    real(kind=KTGT) :: f3

    ! logical bucell, blcell

    ! gsegco[node_0] is used for xy-interserctin
    ! glon[node_0] is used for segment center

    klev = pstack(prop_gtlev, jpos)
    cstep = cstride(klev)
    jcla = pstack(prop_gtla, jpos) * cstep + jlatc
    jclo = pstack(prop_gtlo, jpos) * cstep + jlonc
    jmla = jcla + cstep / 2
    jmlo = jclo + cstep / 2

    ! [3] [4]
    !   [0]
    ! [1] [2]
    jcla1 = jcla
    jcla2 = jcla
    jcla3 = jcla + cstep
    jcla4 = jcla + cstep

    jclo1 = jclo
    jclo2 = jclo + cstep
    jclo3 = jclo
    jclo4 = jclo + cstep

    rlonn(node_1) = real(jclo1 - jclon_rep, kind=KTGT) / repfac
    rlonn(node_2) = real(jclo2 - jclon_rep, kind=KTGT) / repfac
    rlonn(node_3) = real(jclo3 - jclon_rep, kind=KTGT) / repfac
    rlonn(node_4) = real(jclo4 - jclon_rep, kind=KTGT) / repfac

    ! factor TWO is relative to uone, hard-coded.
    ! UONE corresponds: ulon * (uslat / 2)
    udlon  = ulon
    uaseg(accum_w0) = TWO / (afact(klev) * uslat * udlon)
    uaseg(accum_w1lo) = - ONE / (udlon * udlon)
    uaseg(accum_w1la) = - ONE / udlon
    uone   = ONE
    if (dir_nat.lt.0) then
       uone = - uone
       uaseg(accum_w0) = - uaseg(accum_w0)
       uaseg(accum_w1lo) = - uaseg(accum_w1lo)
       uaseg(accum_w1la) = - uaseg(accum_w1la)
    endif
    ! write(*, *) 'dir_nat', uone
    ! write(*, *) 'aseg', klev, afact(klev), 2.0_KTGT / uslat, &
    !      & 2.0_KTGT * 3.141592_KTGT / ulon
    gsegco(JLATI:JLONGI, node_1) = (/+UONE * HALF, -UONE * HALF/)    ! segment lat/lon cooordinate
    gsegco(JLATI:JLONGI, node_2) = (/+UONE * HALF, +UONE * HALF/)    ! relative to the segment center
    gsegco(JLATI:JLONGI, node_3) = (/-UONE * HALF, -UONE * HALF/)    ! (dslat/2, dlon/2)
    gsegco(JLATI:JLONGI, node_4) = (/-UONE * HALF, +UONE * HALF/)

    gdbl(coor_x, 1:nnode) = pstack(prop_psg_x1:prop_psg_x4, jpos)
    gdbl(coor_y, 1:nnode) = pstack(prop_psg_y1:prop_psg_y4, jpos)

    relpos(1:nnode) = &
         & ((gdbl(coor_y, 1:nnode) - gmin(coor_y)) / 2) * nqx &
         & + ((gdbl(coor_x, 1:nnode) - gmin(coor_x)) / 2)

    adjstt(node_1) = is_adjacent_cells(gdbl(:, node_1), gdbl(:, node_2))
    adjstt(node_2) = is_adjacent_cells(gdbl(:, node_2), gdbl(:, node_4))
    adjstt(node_4) = is_adjacent_cells(gdbl(:, node_4), gdbl(:, node_3))
    adjstt(node_3) = is_adjacent_cells(gdbl(:, node_3), gdbl(:, node_1))

    call psgp_gla_tr(glat(1:NTRIG, node_0), cachela(:, jmla), cco)      ! segment center
    call psgp_dlo_tr(glon(1:NTRIG, node_0), cachelo(:, jmlo), cco)

    call psgp_gla_tr(glat(1:NTRIG, node_1), cachela(:, jcla1), cco)
    call psgp_gla_tr(glat(1:NTRIG, node_2), cachela(:, jcla2), cco)
    call psgp_gla_tr(glat(1:NTRIG, node_3), cachela(:, jcla3), cco)
    call psgp_gla_tr(glat(1:NTRIG, node_4), cachela(:, jcla4), cco)

    call psgp_dlo_tr(glon(1:NTRIG, node_1), cachelo(:, jclo1), cco)
    call psgp_dlo_tr(glon(1:NTRIG, node_2), cachelo(:, jclo2), cco)
    call psgp_dlo_tr(glon(1:NTRIG, node_3), cachelo(:, jclo3), cco)
    call psgp_dlo_tr(glon(1:NTRIG, node_4), cachelo(:, jclo4), cco)

    ! write(*, *) 'glat:1', rad2deg(phase(glat(1:NTRIG, node_1))), rad2deg(phase(glon(1:NTRIG, node_1)))
    ! write(*, *) 'glat:2', rad2deg(phase(glat(1:NTRIG, node_2))), rad2deg(phase(glon(1:NTRIG, node_2)))
    ! write(*, *) 'glat:3', rad2deg(phase(glat(1:NTRIG, node_3))), rad2deg(phase(glon(1:NTRIG, node_3)))
    ! write(*, *) 'glat:4', rad2deg(phase(glat(1:NTRIG, node_4))), rad2deg(phase(glon(1:NTRIG, node_4)))

    ! write(*, *) 'anchor/m', gmin(coor_x:coor_y), gmax(coor_x:coor_y)
    ! write(*, *) 'anchor/x', anchor_pos(gmin(coor_x), gmax(coor_x)), anchor_pos(gmin(coor_y), gmax(coor_y))

    jxa = anchor_pos(gmin(coor_x), gmax(coor_x))
    jya = anchor_pos(gmin(coor_y), gmax(coor_y))
    xpos(coor_x:coor_y) = aplane(coor_x:coor_y, (jxa / 2) * 2, (jya / 2) * 2)

    call psgp_bwd_tr(xlat, xlon, xpos(coor_x), xpos(coor_y), cco)

    dlon(1:NTRIG) = _SUB_ANGLE(xlon, glon(1:NTRIG, node_0))
    xrlon = ATAN2(dlon(1), dlon(2))
    ! gsegco(JLATI, node_0) = (xlat(JSIN) - glat(JSIN,node_0)) / uslat
    ! gsegco(JLONGI, node_0) = xrlon / ulon
    gsegco(JLATI, node_0) = &
         & segment_rel_sinlat(xlat(JSIN), glat(JSIN,node_0), &
         &                    glat(JSIN,node_1),    glat(JSIN,node_3), &
         &                    gsegco(JLATI,node_1), gsegco(JLATI,node_3), &
         &                    uslat)
    gsegco(JLONGI, node_0) = &
         & segment_rel_lon(xlon(:), glon(:,node_0), &
         &                 glon(:,node_1), glon(:,node_2), &
         &                 gsegco(JLONGI,node_1), gsegco(JLONGI,node_2), &
         &                 ulon)

    call psgp_dlo_tr(rlona(1:NTRIG), cachelo(:, jclon_rep), cco)
    dlon(1:NTRIG) = _SUB_ANGLE(xlon, rlona)
    xrlon = ATAN2(dlon(1), dlon(2))        ! anchor longitude relative to representative

    gpos(:, node_1) = psgp_fwd(cachelo(:, jclo1), cachela(:, jcla1))
    gpos(:, node_2) = psgp_fwd(cachelo(:, jclo2), cachela(:, jcla2))
    gpos(:, node_3) = psgp_fwd(cachelo(:, jclo3), cachela(:, jcla3))
    gpos(:, node_4) = psgp_fwd(cachelo(:, jclo4), cachela(:, jcla4))

    pmin(coor_x:coor_y) = minval(gpos(coor_x:coor_y, node_1:node_4), 2)
    pmax(coor_x:coor_y) = maxval(gpos(coor_x:coor_y, node_1:node_4), 2)

    jseq = 0

    ! unset marks
    nprops(node_xi) = -1
    nprops(node_xo) = -2
    nprops(node_yi) = -1
    nprops(node_yo) = -2

    jxsec = node_xi
    jysec = node_yi
    glon(:, node_xi) = -999.0_KTGT
    glat(:, node_xi) = -999.0_KTGT

    ! 1 - 2 (parallel/upper)
    jf = node_1
    jt = node_2
    jc = jcla1
    call ps2g_parallel_intersection &
         & (aside,  ngraph,   jseq,       &
         &  gsegco, nprops,   glat,       glon,  jxsec, jysec, &
         &  gdbl,   relpos,   adjstt(jf), jf,    jt,    &
         &  xpos,   xlat,     xlon,       jxa,   jya,   &
         &  cachela(:, jc),   cco, uone,  rlonn, lintg2(jc),   ulon)
    ! write(*, *) 'ai', jf, aside(0:jseq-1)
    ! write(*, *) 'intersection', jf, jxsec, glon(:, node_xi), glat(:, node_xi)
    ! 2 - 4 (meridian)
    jf = node_2
    jt = node_4
    jc = jclo2
    call ps2g_meridian_intersection &
         & (aside,  ngraph,    jseq,       &
         &  gsegco, nprops,    glat,       glon,  jxsec, jysec, &
         &  gdbl,   relpos,    adjstt(jf), jf,    jt,    &
         &  xpos,   cachelo(:, jc),        cco,   uslat)
    ! write(*, *) 'ai', jf, aside(0:jseq-1)
    ! write(*, *) 'intersection', jf, jxsec, glon(:, node_xi), glat(:, node_xi)
    ! 4 - 3 (parallel/lower)
    jf = node_4
    jt = node_3
    jc = jcla4
    call ps2g_parallel_intersection &
         & (aside,  ngraph,   jseq,       &
         &  gsegco, nprops,   glat,       glon,  jxsec, jysec, &
         &  gdbl,   relpos,   adjstt(jf), jf,    jt,    &
         &  xpos,   xlat,     xlon,       jxa,   jya,   &
         &  cachela(:, jc),   cco, uone,  rlonn, lintg2(jc),   ulon)
    ! write(*, *) 'ai', jf, aside(0:jseq-1)
    ! write(*, *) 'intersection', jf, jxsec, glon(:, node_xi), glat(:, node_xi)
    ! 3 - 1 (meridian)
    jf = node_3
    jt = node_1
    jc = jclo3
    call ps2g_meridian_intersection &
         & (aside,  ngraph,    jseq,       &
         &  gsegco, nprops,    glat,       glon,  jxsec, jysec, &
         &  gdbl,   relpos,    adjstt(jf), jf,    jt,    &
         &  xpos,   cachelo(:, jc),        cco,   uslat)
    ! write(*, *) 'ai', jf, aside(0:jseq-1)
    ! write(*, *) 'intersection', jf, jxsec, glon(:, node_xi), glat(:, node_xi)

    ! graph connection
    nseq = jseq
    call ps2g_graph_connection(cflag, ngraph, nprops, nseq, jt)

    ! write(*, *) 'aside', aside(0:nseq-1)

    ! write(*, *) 'cflag = ', cflag, jxsec, jysec
    select case(cflag)
    case(connect_bent)
       ! if xy-anchor inside
       call ps2g_area_bent &
            & (asec, glat, glon, gsegco, &
            &  xlat, xlon, ngraph, nprops, uaseg, agmp_order, xrlon, uslat, ulon)
    case(connect_straight)
       ! if xy-anchor outside
       call ps2g_area_straight &
            & (asec, glat, glon, gsegco, node_xi, node_xo, &
            &  ngraph, nprops, uaseg, agmp_order, rlona, uslat, ulon)
       call ps2g_area_straight &
            & (asec, glat, glon, gsegco, node_yi, node_yo, &
            &  ngraph, nprops, uaseg, agmp_order, rlona, uslat, ulon)
    end select
    ! result
    call ps2g_neighbor_collect &
         & (adiv, negmaxa, &
         &  asec, aside,   ngraph, nprops, relpos, nseq, uslat, &
         &  levv, tag, jlonph, jlatph)

    jlx = (gmin(coor_x) + 2) / 2 - 1
    jly = (gmin(coor_y) + 2) / 2 - 1

    ! adiv(:, accum_w1lo) = adiv(:, accum_w1lo) / uslat

    ! write(*, *) 'aside/2', cflag, int(rlonn(node_1:node_4)), aside(0:nseq-1, accum_w1la)
    ! write(*, *) 'asec', asec(:, accum_w1la)
    ! write(*, *) 'adiv', adiv(:, accum_w1la)
    ! write(*, *) 'aside/3', cflag, int(rlonn(node_1:node_4)), aside(0:nseq-1, accum_w1lo)
    ! write(*, *) 'asec', asec(:, accum_w1lo)
    ! write(*, *) 'adiv', adiv(:, accum_w1lo)

    jq = 0
    f3 = afact(klev)
    do jdy = jly, jly + 1
       do jdx = jlx, jlx + 1
          jq = jq + 1
          if (inrange(jdx, 0, mx - 1).and.inrange(jdy, 0, my - 1)) then
             jdh = jdy * mx + jdx
             jtabr(rmax, coor_x:coor_y) = max(jtabr(rmax, coor_x:coor_y), (/jdx, jdy/))
             jtabr(rmin, coor_x:coor_y) = min(jtabr(rmin, coor_x:coor_y), (/jdx, jdy/))
          else
             jdh = jdext
          endif
          jc = accum_w0
          anum = AINT(adiv(jq, jc) / afact(klev))
          arem = MOD(adiv(jq, jc), afact(klev))
          ntiles(jdh,jc) = ntiles(jdh,jc) + (anum * afact(klev))
          ! carry to afact, with keeping ptiles within 1.
          atmp = ptiles(jdh, jc) + arem
          anum = AINT(atmp / afact(klev)) * afact(klev)
          ntiles(jdh,jc) = ntiles(jdh,jc) + anum
          ptiles(jdh,jc) = (ptiles(jdh,jc) - anum) + arem

          jc = accum_w1lo
          anum = AINT(adiv(jq, jc) / f3)
          arem = MOD(adiv(jq, jc), f3)
          ntiles(jdh,jc) = ntiles(jdh,jc) + (anum * f3)
          atmp = ptiles(jdh, jc) + arem
          anum = AINT(atmp / f3) * f3
          ntiles(jdh,jc) = ntiles(jdh,jc) + anum
          ptiles(jdh,jc) = (ptiles(jdh,jc) - anum) + arem

          jc = accum_w1la
          ntiles(jdh,jc) = ntiles(jdh,jc) - adiv(jq, jc)
          ! anum = AINT(-adiv(jq, jc) / f3)
          ! arem = MOD(-adiv(jq, jc), f3)
          ! ntiles(jdh,jc) = ntiles(jdh,jc) + (anum * f3)
          ! atmp = ptiles(jdh, jc) + arem
          ! anum = AINT(atmp / f3) * f3
          ! ntiles(jdh,jc) = ntiles(jdh,jc) + anum
          ! ptiles(jdh,jc) = (ptiles(jdh,jc) - anum) + arem

       enddo
    enddo

  end subroutine div_ps2g_neighbor_d

!!!_   & segment_rel_sinlat() - set relative latitude within tiny rll segment
  PURE &
    real(kind=KTGT) function segment_rel_sinlat_d &
      & (slat, slat0, slat1, slat2, p1, p2, u) &
      &  result(r)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: slat           ! target sine latitude
    real(kind=KTGT),intent(in) :: slat0          ! reference sine latitude
    real(kind=KTGT),intent(in) :: slat1, slat2   ! segment boundary sine latitudes
    real(kind=KTGT),intent(in) :: p1,    p2      ! special return value when slat1 or slat2
    real(kind=KTGT),intent(in) :: u              ! unit sine latitude of the segment

    if (slat.eq.slat1) then
       r = p1
    else if (slat.eq.slat2) then
       r = p2
    else
       r = (slat - slat0) / u
    endif
  end function segment_rel_sinlat_d

!!!_   & segment_rel_lon() - set relative longitude within tiny rll segment
  real(kind=KTGT) function segment_rel_lon_d &
      & (lon, lon0, lon1, lon2, p1, p2, u) &
      &  result(r)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Emu_ugg,only: hpsub_angle, sub_angle
    implicit none
    real(kind=KTGT),intent(in) :: lon(*)             ! target longitude
    real(kind=KTGT),intent(in) :: lon0(*)            ! reference longitude
    real(kind=KTGT),intent(in) :: lon1(*), lon2(*)   ! segment boundary longitude
    real(kind=KTGT),intent(in) :: p1,      p2        ! special return value when lon1 or lon2
    real(kind=KTGT),intent(in) :: u                  ! unit sine latitude of the segment

    real(kind=KTGT) :: dlon(NTRIG)

    if (lon(JSIN)*lon1(JCOS).eq.lon(JCOS)*lon1(JSIN)) then
       r = p1
    else if (lon(JSIN)*lon2(JCOS).eq.lon(JCOS)*lon2(JSIN)) then
       r = p2
    else
       dlon(1:NTRIG) = _SUB_ANGLE(lon(1:NTRIG), lon0(1:NTRIG))
       r = ATAN2(dlon(JSIN), dlon(JCOS))
       r = r / u
    endif

  end function segment_rel_lon_d

!!!_   & ps2g_meridian_intersection
  subroutine ps2g_meridian_intersection_d &
       & (aside,  ngraph,  jseq,   &
       &  gsegco, nprops,  glat,   glon,  jxsec, jysec, &
       &  gdbl,   relpos,  adjstt, jf,    jt,    &
       &  xpos,   cachelo, cco,    uslat)
    use TOUZA_Emu,only: hpsub_sin, sub_sin, phase
    use TOUZA_Emu,only: psgp_yla_tr, psgp_xla_tr
    implicit none
    integer,parameter  :: KTGT=KDBL
    real(kind=KTGT),intent(inout) :: aside(0:lnodes, 0:*)
    integer,        intent(inout) :: ngraph(0:*)
    real(kind=KTGT),intent(inout) :: gsegco(NGEOG, node_0:*)
    integer,        intent(inout) :: jseq
    integer,        intent(inout) :: nprops(*)
    real(kind=KTGT),intent(inout) :: glat(NTRIG, node_0:*)
    real(kind=KTGT),intent(inout) :: glon(NTRIG, node_0:*)
    integer,        intent(inout) :: jxsec, jysec
    integer,        intent(in)    :: gdbl(lxy, *)
    integer,        intent(in)    :: relpos(*)
    integer,        intent(in)    :: adjstt
    integer,        intent(in)    :: jf, jt
    real(kind=KTGT),intent(in)    :: xpos(*)
    real(kind=KTGT),intent(in)    :: cachelo(*)
    real(kind=KTGT),intent(in)    :: cco(*)
    real(kind=KTGT),intent(in)    :: uslat

    integer,parameter :: nqx = 2

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT) :: g(NTRIG), c
    real(kind=KTGT) :: xsin, ysin
    integer jxtgt, jytgt
    integer relf

    jxtgt = -1
    jytgt = -1
    ! write(*, *) '########################################'
    ! if (adjstt.eq.stt_same) then
    if (.FALSE.) then
       if (mod(gdbl(coor_x, jf), 2).eq.0) then
          jytgt = jysec
          glat(1:NTRIG, jytgt) = glat(1:NTRIG, jf)
          glon(1:NTRIG, jytgt) = glon(1:NTRIG, jf)
          jysec = jysec + 1
       else if (mod(gdbl(coor_x, jt), 2).eq.0) then
          jytgt = jysec
          glat(1:NTRIG, jytgt) = glat(1:NTRIG, jt)
          glon(1:NTRIG, jytgt) = glon(1:NTRIG, jt)
          jysec = jysec + 1
       endif
       if (mod(gdbl(coor_y, jf), 2).eq.0) then
          jxtgt = jxsec
          glat(1:NTRIG, jxtgt) = glat(1:NTRIG, jf)
          glon(1:NTRIG, jxtgt) = glon(1:NTRIG, jf)
          jxsec = jxsec + 1
       else if (mod(gdbl(coor_y, jt), 2).eq.0) then
          jxtgt = jxsec
          glat(1:NTRIG, jxtgt) = glat(1:NTRIG, jt)
          glon(1:NTRIG, jxtgt) = glon(1:NTRIG, jt)
          jxsec = jxsec + 1
       endif
    else
       select case(adjstt)
       case(stt_adjacent_x,stt_adjacent_d)
          jytgt = jysec
          call psgp_yla_tr(c, g, xpos(coor_x), cachelo, cco)
          glat(1:NTRIG, jysec) = g(1:NTRIG)
          glon(1:NTRIG, jysec) = glon(1:NTRIG, jf)
          ! gsegco(JLATI, jysec) = (g(JSIN) - glat(JSIN, node_0)) / uslat
          gsegco(JLATI, jysec) = &
               & segment_rel_sinlat(g(JSIN), glat(JSIN,node_0), &
               &                    glat(JSIN,jf),    glat(JSIN,jt), &
               &                    gsegco(JLATI,jf), gsegco(JLATI,jt), &
               &                    uslat)
          gsegco(JLONGI,jysec) = gsegco(JLONGI, jf)
          jysec = jysec + 1
       end select
       select case(adjstt)
       case(stt_adjacent_y,stt_adjacent_d)
          jxtgt = jxsec
          call psgp_xla_tr(c, g, xpos(coor_y), cachelo, cco)
          glat(1:NTRIG, jxsec) = g(1:NTRIG)
          glon(1:NTRIG, jxsec) = glon(1:NTRIG, jf)
          ! gsegco(JLATI, jxsec) = (g(JSIN) - glat(JSIN, node_0)) / uslat
          gsegco(JLATI, jxsec) = &
               & segment_rel_sinlat(g(JSIN), glat(JSIN,node_0), &
               &                    glat(JSIN,jf),    glat(JSIN,jt), &
               &                    gsegco(JLATI,jf), gsegco(JLATI,jt), &
               &                    uslat)
          gsegco(JLONGI,jxsec) = gsegco(JLONGI, jf)
          jxsec = jxsec + 1
       end select
    endif

    ngraph(jseq) = jf

    if (jytgt.ge.0.and.jxtgt.ge.0) then
       relf = relpos(jf)
       ysin = _SUB_SIN(glat(1:NTRIG, jf), glat(1:NTRIG, jytgt))
       xsin = _SUB_SIN(glat(1:NTRIG, jf), glat(1:NTRIG, jxtgt))
       if (ABS(xsin).gt.ABS(ysin)) then
          nprops(jytgt)  = jseq + 1
          nprops(jxtgt)  = jseq + 2
          nprops(jf) = (1 - mod(relf, nqx)) + (relf / nqx) * nqx   ! invert in x
       else
          nprops(jxtgt)  = jseq + 1
          nprops(jytgt)  = jseq + 2
          nprops(jf) = mod(relf, nqx) + ((1 - relf / nqx) * nqx)   ! invert in y
       endif
       aside(jseq:jseq+2, accum_w0)  = ZERO
       aside(jseq:jseq+2, accum_w1la)  = ZERO
       aside(jseq:jseq+2, accum_w1lo)  = ZERO

       ngraph(nprops(jytgt)) = jytgt
       ngraph(nprops(jxtgt)) = jxtgt

       jseq = jseq + 3
    else if (jxtgt.ge.0) then
       ngraph(jseq+1) = jxtgt
       nprops(jxtgt)  = jseq + 1
       aside(jseq:jseq+1, accum_w0)  = ZERO
       aside(jseq:jseq+1, accum_w1la)  = ZERO
       aside(jseq:jseq+1, accum_w1lo)  = ZERO
       nprops(jf) = -1
       jseq = jseq + 2
    else if (jytgt.ge.0) then
       ngraph(jseq+1) = jytgt
       nprops(jytgt)  = jseq + 1
       aside(jseq:jseq+1, accum_w0)  = ZERO
       aside(jseq:jseq+1, accum_w1la)  = ZERO
       aside(jseq:jseq+1, accum_w1lo)  = ZERO
       nprops(jf) = div_in_two
       jseq = jseq + 2
    else
       aside(jseq, accum_w0) = ZERO
       aside(jseq, accum_w1la) = ZERO
       aside(jseq, accum_w1lo) = ZERO
       nprops(jf) = no_division
       jseq = jseq + 1
    endif

  end subroutine ps2g_meridian_intersection_d
!!!_   & ps2g_parallel_intersection
  subroutine ps2g_parallel_intersection_d &
       & (aside,   ngraph, jseq,   &
       &  gsegco,  nprops, glat,   glon,  jxsec, jysec, &
       &  gdbl,    relpos, adjstt, jf,    jt,    &
       &  xpos,    xlat,   xlon,   jxa,   jya,   &
       &  cachela, cco,    uone,   rlonn, li2,   ulon)
    use TOUZA_Ami_std,only: inrange
    use TOUZA_Emu,only: hpsub_sin, sub_sin
    use TOUZA_Emu,only: psgp_xlo_tr, psgp_ylo_tr
    implicit none
    integer,parameter  :: KTGT=KDBL
    real(kind=KTGT),intent(inout) :: aside(0:lnodes, 0:*)
    integer,        intent(inout) :: ngraph(0:*)
    real(kind=KTGT),intent(inout) :: gsegco(NGEOG, node_0:*)
    integer,        intent(inout) :: jseq
    integer,        intent(inout) :: nprops(*)
    real(kind=KTGT),intent(inout) :: glat(NTRIG, node_0:*)
    real(kind=KTGT),intent(inout) :: glon(NTRIG, node_0:*)
    integer,        intent(inout) :: jxsec, jysec
    integer,        intent(in)    :: gdbl(lxy, *)
    integer,        intent(in)    :: relpos(*)
    integer,        intent(in)    :: adjstt
    integer,        intent(in)    :: jf, jt
    real(kind=KTGT),intent(in)    :: xpos(*)
    real(kind=KTGT),intent(in)    :: xlat(*), xlon(*)
    integer,        intent(in)    :: jxa, jya
    real(kind=KTGT),intent(in)    :: cachela(*)
    real(kind=KTGT),intent(in)    :: cco(*)
    real(kind=KTGT),intent(in)    :: uone
    real(kind=KTGT),intent(in)    :: rlonn(0:*)
    real(kind=KTGT),intent(in)    :: li2
    real(kind=KTGT),intent(in)    :: ulon

    integer,parameter :: nqx = 2

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT) :: g(NTRIG), c
    real(kind=KTGT) :: xsin, ysin
    real(kind=KTGT) :: a1, am, a2, as
    real(kind=KTGT) :: w1, wm, w2
    integer jxtgt, jytgt
    integer relf
    real(kind=KTGT) :: f3,   f2
    real(kind=KTGT) :: rstr, runit
    integer jseq0

    jseq0 = jseq

    jxtgt = -1
    jytgt = -1

    ! line integral return: sin(lat)[m2-m1][m2+m1]/2
    ! rstr = m2 - m1

    ! need (ulon * ulon) adjustment to obtain comparable quantity
    ! with direct line integrals (see uaseg(accum_w1lo))

    rstr  = rlonn(jt) - rlonn(jf)
    runit = rstr * (rlonn(jf) + rlonn(jt)) / 2.0_KTGT
    ! write(*, *) 'neighbor', rlonn(jf), rlonn(jt), rstr, runit
    f3 = + uone * glat(JSIN, jf)
    f2 = + uone * li2

    if (adjstt.eq.stt_same) then
    ! if (.FALSE.) then
       ! write(*, *) 'parallel/jxa', jxa, gdbl(coor_x, jf), gdbl(coor_x, jt)
       if (mod(jxa, 2).eq.0) then
          ! if (mod(gdbl(coor_x, jf), 2).eq.0) then
          if (gdbl(coor_x, jf).eq.jxa) then
             jytgt = jysec
             if (gdbl(coor_x, jt).eq.jxa) then
                glat(1:NTRIG, jytgt) = xlat(1:NTRIG)
                glon(1:NTRIG, jytgt) = xlon(1:NTRIG)
                gsegco(1:NGEOG, jytgt) = gsegco(1:NGEOG, node_0)
             else
                glat(1:NTRIG, jytgt) = glat(1:NTRIG, jf)
                glon(1:NTRIG, jytgt) = glon(1:NTRIG, jf)
                gsegco(1:NGEOG, jytgt) = gsegco(1:NGEOG, jf)
             endif
             jysec = jysec + 1
          ! else if (mod(gdbl(coor_x, jt), 2).eq.0) then
          else if (gdbl(coor_x, jt).eq.jxa) then
             jytgt = jysec
             glat(1:NTRIG, jytgt) = glat(1:NTRIG, jt)
             glon(1:NTRIG, jytgt) = glon(1:NTRIG, jt)
             gsegco(1:NGEOG, jytgt) = gsegco(1:NGEOG, jt)
             jysec = jysec + 1
          endif
       endif
       ! write(*, *) 'parallel/jya', jya, gdbl(coor_y, jf), gdbl(coor_y, jt)
       if (mod(jya, 2).eq.0) then
          ! if (mod(gdbl(coor_y, jf), 2).eq.0) then
          if (gdbl(coor_y, jf).eq.jya) then
             jxtgt = jxsec
             if (gdbl(coor_y, jt).eq.jya) then
                glat(1:NTRIG, jxtgt) = xlat(1:NTRIG)
                glon(1:NTRIG, jxtgt) = xlon(1:NTRIG)
                gsegco(1:NGEOG, jxtgt) = gsegco(1:NGEOG, node_0)
             else
                glat(1:NTRIG, jxtgt) = glat(1:NTRIG, jf)
                glon(1:NTRIG, jxtgt) = glon(1:NTRIG, jf)
                gsegco(1:NGEOG, jxtgt) = gsegco(1:NGEOG, jf)
             endif
             jxsec = jxsec + 1
          ! else if (mod(gdbl(coor_y, jt), 2).eq.0) then
          else if (gdbl(coor_y, jt).eq.jya) then
             jxtgt = jxsec
             glat(1:NTRIG, jxtgt) = glat(1:NTRIG, jt)
             glon(1:NTRIG, jxtgt) = glon(1:NTRIG, jt)
             gsegco(1:NGEOG, jxtgt) = gsegco(1:NGEOG, jt)
             jxsec = jxsec + 1
          endif
       endif
    else
       select case(adjstt)
       case(stt_adjacent_x,stt_adjacent_d)
          jytgt = jysec
          call psgp_ylo_tr(c, g, xpos(coor_x), cachela, cco, xpos(coor_y))
          if (inrange(g(JCOS), glon(JCOS, jf), glon(JCOS, jt), .TRUE.)) then
             continue
          else
             g(JCOS) = - g(JCOS)
             c = -c
          endif
          glat(1:NTRIG, jytgt) = glat(1:NTRIG, jf)
          glon(1:NTRIG, jytgt) = g(1:NTRIG)
          gsegco(JLATI, jytgt) = gsegco(JLATI, jf)
          gsegco(JLONGI,jytgt) = &
               & segment_rel_lon(g(:), glon(:, node_0), &
               &                 glon(:,jf), glon(:,jt), &
               &                 gsegco(JLONGI,jf), gsegco(JLONGI, jt), &
               &                 ulon)
          jysec = jysec + 1
       case default
          jytgt = -1
       end select
       select case(adjstt)
       case(stt_adjacent_y,stt_adjacent_d)
          jxtgt = jxsec
          call psgp_xlo_tr(c, g, xpos(coor_y), cachela, cco, xpos(coor_x))
          if (inrange(g(JSIN), glon(JSIN, jf), glon(JSIN, jt), .TRUE.)) then
             continue
          else
             g(JSIN) = - g(JSIN)
             c = -c
          endif
          glat(1:NTRIG, jxtgt) = glat(1:NTRIG, jf)
          glon(1:NTRIG, jxtgt) = g(1:NTRIG)
          gsegco(JLATI, jxtgt) = gsegco(JLATI, jf)
          gsegco(JLONGI,jxtgt) = &
               & segment_rel_lon(g(:), glon(:, node_0), &
               &                 glon(:,jf), glon(:,jt), &
               &                 gsegco(JLONGI,jf), gsegco(JLONGI, jt), &
               &                 ulon)
          jxsec = jxsec + 1
       case default
          jxtgt = -1
       end select
    endif
    ngraph(jseq) = jf

    if (jytgt.ge.0.and.jxtgt.ge.0) then
    ! select case(adjstt)
    ! case(stt_adjacent_d)
       ysin = _SUB_SIN(glon(1:NTRIG, jf), glon(1:NTRIG, jytgt))
       xsin = _SUB_SIN(glon(1:NTRIG, jf), glon(1:NTRIG, jxtgt))
       relf = relpos(jf)
       as = gsegco(JLONGI, jt) - gsegco(JLONGI, jf)

       if (ABS(xsin).gt.ABS(ysin)) then
          nprops(jytgt)  = jseq + 1
          nprops(jxtgt)  = jseq + 2
          nprops(jf) = (1 - mod(relf, nqx)) + (relf / nqx) * nqx   ! invert in x

          ! a1 = parallel_dlongi(glon(:, jf),    glon(:, jytgt))
          ! am = parallel_dlongi(glon(:, jytgt), glon(:, jxtgt))
          ! a2 = parallel_dlongi(glon(:, jxtgt), glon(:, jt))
          a1 = (gsegco(JLONGI, jytgt) - gsegco(JLONGI, jf)) * as
          am = (gsegco(JLONGI, jxtgt) - gsegco(JLONGI, jytgt)) * as
          a2 = (gsegco(JLONGI, jt)    - gsegco(JLONGI, jxtgt)) * as
          ! write(*, *) 'parallel/bx0', a1, am, a2
          ! a1 = parallel_dlongi(glon(:, jf),    glon(:, jytgt))
          ! am = parallel_dlongi(glon(:, jytgt), glon(:, jxtgt))
          ! a2 = parallel_dlongi(glon(:, jxtgt), glon(:, jt))
          ! as = (a1 + a2) + am
          ! write(*, *) 'parallel/bx1', a1 / as, am / as, a2 / as
       else
          nprops(jxtgt)  = jseq + 1
          nprops(jytgt)  = jseq + 2
          nprops(jf) = mod(relf, nqx) + ((1 - relf / nqx) * nqx)   ! invert in y

          a1 = (gsegco(JLONGI, jxtgt) - gsegco(JLONGI, jf)) * as
          am = (gsegco(JLONGI, jytgt) - gsegco(JLONGI, jxtgt)) * as
          a2 = (gsegco(JLONGI, jt)    - gsegco(JLONGI, jytgt)) * as
          ! write(*, *) 'parallel/by0', a1, am, a2
          ! a1 = parallel_dlongi(glon(:, jf),    glon(:, jxtgt))
          ! am = parallel_dlongi(glon(:, jxtgt), glon(:, jytgt))
          ! a2 = parallel_dlongi(glon(:, jytgt), glon(:, jt))
          ! as = (a1 + a2) + am
          ! write(*, *) 'parallel/by1', a1 / as, am / as, a2 / as
       endif
       ! as = (a1 + a2) + am
       ! aside(jseq,   accum_w0) = uone * (a1 / as)
       ! aside(jseq+1, accum_w0) = uone * (am / as)
       ! aside(jseq+2, accum_w0) = uone * (a2 / as)
       aside(jseq,   accum_w0) = uone * a1
       aside(jseq+1, accum_w0) = uone * am
       aside(jseq+2, accum_w0) = uone * a2

       ! w1 = rstr * (a1 / as)
       ! w2 = rstr * (a2 / as)
       w1 = rstr * a1
       w2 = rstr * a2
       wm = rstr - (w1 + w2)
       aside(jseq,   accum_w1la) = f2 * w1
       aside(jseq+1, accum_w1la) = f2 * wm
       aside(jseq+2, accum_w1la) = f2 * w2

       w1 = w1 * (rlonn(jf) + w1 / 2.0_KTGT)
       w2 = w2 * (rlonn(jt) - w2 / 2.0_KTGT)
       wm = runit - (w1 + w2)

       aside(jseq,   accum_w1lo) = f3 * w1
       aside(jseq+1, accum_w1lo) = f3 * wm
       aside(jseq+2, accum_w1lo) = f3 * w2

       ngraph(nprops(jytgt)) = jytgt
       ngraph(nprops(jxtgt)) = jxtgt

       jseq = jseq + 3
    else if (jxtgt.ge.0) then
    ! case(stt_adjacent_y)
       ngraph(jseq+1) = jxtgt
       nprops(jxtgt)  = jseq + 1
       ! a1 = parallel_dlongi(glon(:, jf), glon(:, jxtgt))
       ! a2 = parallel_dlongi(glon(:, jt), glon(:, jxtgt))
       ! as = a1 - a2
       as = gsegco(JLONGI, jt) - gsegco(JLONGI, jf)
       a1 = (gsegco(JLONGI, jxtgt) - gsegco(JLONGI, jf)) * as
       a2 = (gsegco(JLONGI, jxtgt) - gsegco(JLONGI, jt)) * as

       ! w1 = rstr * (+ a1 / as)
       ! w2 = rstr * (- a2 / as)
       ! aside(jseq,   accum_w1la) = + f2 * w1
       ! aside(jseq+1, accum_w1la) = + f2 * w2
       w1 = rstr * (+ a1)
       w2 = rstr * (- a2)
       aside(jseq,   accum_w1la) = + f2 * w1
       aside(jseq+1, accum_w1la) = + f2 * w2

       w1 = w1 * (rlonn(jf) + w1 / 2.0_KTGT)
       w2 = w2 * (rlonn(jt) - w2 / 2.0_KTGT)

       ! a1 = uone * (a1 / as)
       ! a2 = uone * (a2 / as)
       a1 = uone * a1
       a2 = uone * a2
       aside(jseq,   accum_w0) = + a1
       aside(jseq+1, accum_w0) = - a2
       ! as = gsegco(JLONGI, jt) - gsegco(JLONGI, jf)
       ! write(*, *) 'parallel/x', jxtgt, jf, jt, uone, &
       !      & a1, a2, &
       !      & - uone * as * (gsegco(JLONGI, jf)-gsegco(JLONGI, jxtgt)), &
       !      & - uone * as * (gsegco(JLONGI, jt)-gsegco(JLONGI, jxtgt))

       aside(jseq,   accum_w1lo) = f3 * w1
       aside(jseq+1, accum_w1lo) = f3 * w2

       nprops(jf) = div_in_two
       jseq = jseq + 2
    else if (jytgt.ge.0) then
    ! case(stt_adjacent_x)
       ngraph(jseq+1) = jytgt
       nprops(jytgt)  = jseq + 1
       ! a1 = parallel_dlongi(glon(:, jf), glon(:, jytgt))
       ! a2 = parallel_dlongi(glon(:, jt), glon(:, jytgt))
       ! as = a1 - a2
       as = gsegco(JLONGI, jt) - gsegco(JLONGI, jf)
       a1 = (gsegco(JLONGI, jytgt) - gsegco(JLONGI, jf)) * as
       a2 = (gsegco(JLONGI, jytgt) - gsegco(JLONGI, jt)) * as

       ! w1 = rstr * (+ a1 / as)
       ! w2 = rstr * (- a2 / as)
       ! aside(jseq,   accum_w1la) = + f2 * w1
       ! aside(jseq+1, accum_w1la) = + f2 * w2
       w1 = rstr * (+ a1)
       w2 = rstr * (- a2)
       aside(jseq,   accum_w1la) = + f2 * w1
       aside(jseq+1, accum_w1la) = + f2 * w2

       w1 = w1 * (rlonn(jf) + w1 / 2.0_KTGT)
       w2 = w2 * (rlonn(jt) - w2 / 2.0_KTGT)

       ! a1 = uone * (a1 / as)
       ! a2 = uone * (a2 / as)
       a1 = uone * a1
       a2 = uone * a2
       aside(jseq,   accum_w0) = + a1
       aside(jseq+1, accum_w0) = - a2
       as = gsegco(JLONGI, jt) - gsegco(JLONGI, jf)
       ! write(*, *) 'parallel/y', jytgt, jf, jt, uone, &
       !      & a1, a2, &
       !      & - uone * as * (gsegco(JLONGI, jf)-gsegco(JLONGI, jytgt)), &
       !      & - uone * as * (gsegco(JLONGI, jt)-gsegco(JLONGI, jytgt))

       aside(jseq,   accum_w1lo) = f3 * w1
       aside(jseq+1, accum_w1lo) = f3 * w2

       nprops(jf) = div_in_two
       jseq = jseq + 2
    else
    ! case(stt_same)
       aside(jseq, accum_w0) = uone
       aside(jseq, accum_w1la) = f2 * rstr
       aside(jseq, accum_w1lo) = f3 * runit
       nprops(jf) = no_division
       jseq = jseq + 1
    ! end select
    endif

    ! if (jseq.gt.jseq0-1) then
    !    write(*, *) 'para', f3 * runit - SUM(aside(jseq0:jseq-1, accum_w1lo)), &
    !         & aside(jseq0:jseq-1, accum_w1lo)
    ! endif

  end subroutine ps2g_parallel_intersection_d

!!!_   & ps2g_graph_connection
  subroutine ps2g_graph_connection &
       & (cflag, ngraph, nprops, nseq, jt)
    implicit none
    integer,intent(out)   :: cflag
    integer,intent(inout) :: ngraph(0:*)
    integer,intent(inout) :: nprops(*)
    integer,intent(in)    :: nseq, jt

    integer tmp

    cflag = connect_err

    ngraph(nseq) = jt
    ngraph(nseq+1) = -1  ! end mark

    ! write(*, *) 'nprops = ', nprops([node_xi, node_xo, node_yi, node_yo])
    if (nprops(node_xi).lt.nprops(node_yi) &
         & .and.nprops(node_yi).lt.nprops(node_xo) &
         & .and.nprops(node_xo).lt.nprops(node_yo)) then
       ! order [xi yi xo yo]  ::  xi-yo  yi-xi  xo-yi  yo-xo
       tmp = nprops(node_xi)
       nprops(node_xi) = nprops(node_yo)
       nprops(node_yo) = nprops(node_xo)
       nprops(node_xo) = nprops(node_yi)
       nprops(node_yi) = tmp
       cflag = connect_bent
    else if (nprops(node_yi).lt.nprops(node_xi) &
         & .and.nprops(node_xi).lt.nprops(node_yo) &
         & .and.nprops(node_yo).lt.nprops(node_xo)) then
       ! order [yi xi yo xo]  ::  yi-xo  xi-yi  yo-xi  xo-yo
       tmp = nprops(node_yi)
       nprops(node_yi) = nprops(node_xo)
       nprops(node_xo) = nprops(node_yo)
       nprops(node_yo) = nprops(node_xi)
       nprops(node_xi) = tmp
       cflag = connect_bent
    else
       call swap_items(nprops(node_xi), nprops(node_xo))
       call swap_items(nprops(node_yi), nprops(node_yo))
       cflag = connect_straight
    endif
    ! if (cflag.ne.connect_a) then
    !    cflag = 0
    !    if (nprops(node_xo).ge.0) cflag = cflag + connect_x
    !    if (nprops(node_yo).ge.0) cflag = cflag + connect_y
    ! endif
  end subroutine ps2g_graph_connection

!!!_   & ps2g_area_bent
#if OPT_AMI_AGMP_METHOD == 1
#  define _AGMP_TABLE agmpd_gen_table
#  define _AGMP_COMP  agmpd_area_core
#else
#  define _AGMP_TABLE agmp_gen_table
#  define _AGMP_COMP  agmp_area_core
#endif
  subroutine ps2g_area_bent_d &
       & (asec, glat, glon, gsegco, &
       &  xlat, xlon, ngraph, nprops, uaseg, korder, xrlon, uslat, ulon)
    use TOUZA_Emu_ugg,only: psgp_fwd
    use TOUZA_Emu_ugg,only: hpsub_angle, sub_angle
    use TOUZA_Emu_ugg,only: geodesic_dazim
    use TOUZA_Emu_ugg,only: agmpd_gen_table, agmpd_area_core
    implicit none
    integer,parameter  :: KTGT=KDBL
    real(kind=KTGT),intent(out) :: asec(node_xi:node_yo, 0:*)
    real(kind=KTGT),intent(in)  :: glat(NTRIG, node_0:*)
    real(kind=KTGT),intent(in)  :: glon(NTRIG, node_0:*)
    real(kind=KTGT),intent(in)  :: gsegco(NGEOG, node_0:*)
    real(kind=KTGT),intent(in)  :: xlat(*)
    real(kind=KTGT),intent(in)  :: xlon(*)
    integer,        intent(in)  :: ngraph(0:*)
    integer,        intent(in)  :: nprops(*)
    real(kind=KTGT),intent(in)  :: uaseg(0:*)
    integer,        intent(in)  :: korder
    real(kind=KTGT),intent(in)  :: xrlon
    real(kind=KTGT),intent(in)  :: uslat, ulon

    integer jf, jt

    integer,parameter :: ltbl = 128
    real(kind=KTGT) :: CC(0:ltbl)

    real(kind=KTGT) :: dlon(NTRIG), dlat(NTRIG)
    real(kind=KTGT) :: dlonh, tdlath
    real(kind=KTGT) :: ax, bx
    real(kind=KTGT) :: atmp(node_xi:node_yo), btmp(node_xi:node_yo)
    real(kind=KTGT) :: w2tmp(node_xi:node_yo)
    real(kind=KTGT) :: w3tmp(node_xi:node_yo)
    integer jerr

    real(kind=KTGT) :: t3, t2, tt2

    t3 = xrlon * xlat(JSIN)
    t2 = coeff2_line_integ(xlat(JSIN))

    if (korder.gt.0) then
       do jf = node_xi, node_yo
          bx = (gsegco(JLONGI, node_0) - gsegco(JLONGI, jf)) * ulon
          dlonh = bx / 2.0_KTGT
          ! dlon(:) = _SUB_ANGLE(xlon(1:NTRIG), glon(:,jf))  ! to anchor
          dlat(:) = _SUB_ANGLE(xlat(1:NTRIG), glat(:,jf))  ! to anchor
          ! dlonh  = ATAN2(dlon(JSIN), 1.0_KTGT + dlon(JCOS))
          tdlath = dlat(JSIN) / (1.0_KTGT + dlat(JCOS))
          call _AGMP_TABLE(jerr, CC, korder, glat(JSIN, jf), .TRUE.)
          ax = _AGMP_COMP(glat(JCOS, jf), tdlath, dlonh, CC, korder)
          ! bx = ATAN2(dlon(1), dlon(2))
          bx = - bx * (gsegco(JLATI, jf) * uslat)
          ! bx = - bx * (glat(JSIN, jf) - glat(JSIN, node_0))
          atmp(jf) = ax
          btmp(jf) = bx
          w3tmp(jf) = - (t3 + (xrlon - dlonh * 2.0_KTGT) * glat(JSIN, jf)) * dlonh
          tt2 = coeff2_line_integ(glat(JSIN, jf))
          w2tmp(jf) = - (t2 + tt2) * dlonh
       enddo
       do jf = node_xi, node_yo
          jt = ngraph(nprops(jf))
          ax = atmp(jf) - atmp(jt)
          bx = btmp(jf) - btmp(jt)
          asec(jf, accum_w0) = (ax - bx) * uaseg(accum_w0)
          asec(jf, accum_w1lo) = (w3tmp(jf) - w3tmp(jt)) * uaseg(accum_w1lo)
          asec(jf, accum_w1la) = (w2tmp(jf) - w2tmp(jt)) * uaseg(accum_w1la)
       enddo
    else
       do jf = node_xi, node_yo
          dlon(:) = _SUB_ANGLE(xlon(1:NTRIG), glon(:,jf))  ! to anchor
          bx = ATAN2(dlon(1), dlon(2))
          ax = geodesic_dazim(glat(:,jf), xlat, dlon)
          atmp(jf) = ax
          btmp(jf) = bx
          w3tmp(jf) = - (t3 + (xrlon - bx) * glat(JSIN, jf)) * (bx / 2.0_KTGT)
          tt2 = coeff2_line_integ(glat(JSIN, jf))
          w2tmp(jf) = - (t2 + tt2) * (bx / 2.0_KTGT)
       enddo
       do jf = node_xi, node_yo
          jt = ngraph(nprops(jf))
          ax = atmp(jf) - atmp(jt)
          bx = (btmp(jf) - btmp(jt)) * glat(JSIN, node_0)
          asec(jf, accum_w0) = (ax - bx) * uaseg(accum_w0)
          asec(jf, accum_w1lo) = (w3tmp(jf) - w3tmp(jt)) * uaseg(accum_w1lo)
          asec(jf, accum_w1la) = (w2tmp(jf) - w2tmp(jt)) * uaseg(accum_w1la)
       enddo
    endif
  end subroutine ps2g_area_bent_d

!!!_   & ps2g_area_straight
  subroutine ps2g_area_straight_d &
       & (asec, glat, glon, gsegco, jf, jt, &
       &  ngraph, nprops, uaseg, korder, rlona, uslat, ulon)
    use TOUZA_Emu_ugg,only: psgp_fwd
    use TOUZA_Emu_ugg,only: hpsub_angle, sub_angle
    use TOUZA_Emu_ugg,only: geodesic_dazim
    use TOUZA_Emu_ugg,only: agmpd_gen_table, agmpd_area_core
    implicit none
    integer,parameter  :: KTGT=KDBL
    real(kind=KTGT),intent(out) :: asec(node_xi:node_yo, 0:*)
    real(kind=KTGT),intent(in)  :: glat(NTRIG, node_0:*)
    real(kind=KTGT),intent(in)  :: glon(NTRIG, node_0:*)
    real(kind=KTGT),intent(in)  :: gsegco(NGEOG, node_0:*)
    integer,        intent(in)  :: jf, jt
    integer,        intent(in)  :: ngraph(0:*)
    integer,        intent(in)  :: nprops(*)
    real(kind=KTGT),intent(in)  :: uaseg(0:*)
    integer,        intent(in)  :: korder
    real(kind=KTGT),intent(in)  :: rlona(*)
    real(kind=KTGT),intent(in)  :: ulon, uslat

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT

    integer,parameter :: ltbl = 128
    real(kind=KTGT) :: CC(0:ltbl)

    real(kind=KTGT) :: dlon(NTRIG), dlat(NTRIG)
    real(kind=KTGT) :: dlonh, tdlath
    real(kind=KTGT) :: ax, bx
    real(kind=KTGT) :: f31, f32, f3
    real(kind=KTGT) :: f21, f22, f2
    integer jerr

    ! write(*, *) 'straight/g', jf, nprops(jf)
    if (nprops(jf).gt.0) then
       ! write(*, *) 'straight', jf, jt, glon(1:NTRIG,jt),  glon(1:NTRIG,jf)
       ! dlon(1:NTRIG) = _SUB_ANGLE(glon(1:NTRIG,jt),  glon(1:NTRIG,jf))  ! xi to xo
       ! dlonh  = ATAN2(dlon(JSIN), 1.0_KTGT + dlon(JCOS))
       ! bx = ATAN2(dlon(JSIN), dlon(JCOS))
       ! write(*, *) 'straight:bxlo', bx, (gsegco(JLONGI, jt) - gsegco(JLONGI, jf)) * ulon
       if (korder.gt.0) then
          bx = (gsegco(JLONGI, jt) - gsegco(JLONGI, jf)) * ulon
          dlonh = bx / 2.0_KTGT
          dlat(1:NTRIG) = _SUB_ANGLE(glat(1:NTRIG,jt), glat(1:NTRIG,jf))  ! xi to xo
          tdlath = dlat(JSIN) / (1.0_KTGT + dlat(JCOS))
          ! condition for symmetry preserving
          if (ABS(glat(JSIN, jf)).lt.ABS(glat(JSIN, jt))) then
          ! if (.TRUE.) then
             call _AGMP_TABLE(jerr, CC, korder, glat(JSIN, jf), .TRUE.)
             ax = _AGMP_COMP(glat(JCOS, jf), tdlath, dlonh, CC, korder)
             bx = - bx * (gsegco(JLATI, jf) * uslat)
          else
             call _AGMP_TABLE(jerr, CC, korder, glat(JSIN, jt), .TRUE.)
             ax = _AGMP_COMP(glat(JCOS, jt), - tdlath, dlonh, CC, korder)
             bx = - bx * (gsegco(JLATI, jt) * uslat)
          endif
          ! bx = - bx * (glat(JSIN, jf) - glat(JSIN, node_0))
          ! write(*, *) 'straight:bxla', &
          !      & -(glat(JSIN, jf) - glat(JSIN, node_0)), &
          !      & (- gsegco(JLATI, jf)) * uslat
       else
          dlon(1:NTRIG) = _SUB_ANGLE(glon(1:NTRIG,jt),  glon(1:NTRIG,jf))  ! xi to xo
          ax = geodesic_dazim(glat(:,jf), glat(:,jt), dlon)  ! ax: xi to xo
          bx = bx * glat(JSIN, node_0)
       endif
       ! write(*,*) 'straight', jf, jt, ax, bx
       ax = (ax - bx) * uaseg(accum_w0)
       asec(jf, accum_w0) = ax
       asec(jt, accum_w0) = -ax

       dlon(1:NTRIG) = _SUB_ANGLE(glon(1:NTRIG,jf),  rlona)
       f31 = ATAN2(dlon(JSIN), dlon(JCOS)) * glat(JSIN, jf)
       dlon(1:NTRIG) = _SUB_ANGLE(glon(1:NTRIG,jt),  rlona)
       f32 = ATAN2(dlon(JSIN), dlon(JCOS)) * glat(JSIN, jt)
       f3 = - ((f31 + f32) * dlonh) * uaseg(accum_w1lo)
       asec(jf, accum_w1lo) = f3
       asec(jt, accum_w1lo) = -f3

       f21 = coeff2_line_integ(glat(JSIN, jf))
       f22 = coeff2_line_integ(glat(JSIN, jt))
       f2 = - ((f21 + f22) * dlonh) * uaseg(accum_w1la)
       asec(jf, accum_w1la) = f2
       asec(jt, accum_w1la) = -f2
    else
       asec(jf, accum_w0) = ZERO
       asec(jt, accum_w0) = ZERO
       asec(jf, accum_w1la) = ZERO
       asec(jt, accum_w1la) = ZERO
       asec(jf, accum_w1lo) = ZERO
       asec(jt, accum_w1lo) = ZERO
    endif
  end subroutine ps2g_area_straight_d

#undef _AGMP_TABLE
#undef _AGMP_COMP
!!!_   & ps2g_neighbor_collect
  subroutine ps2g_neighbor_collect_d &
         & (adiv, negmaxa, &
         &  asec, aside,   ngraph, nprops, relpos, nseq, uslat, &
         &  levv, tag, jlonph, jlatph)
    use TOUZA_Ami_std,only: join_list
    implicit none
    integer,parameter  :: KTGT=KDBL
    integer,parameter :: nquad = 4

    real(kind=KTGT), intent(out)   :: adiv(0:nquad-1, 0:*)
    real(kind=KTGT), intent(inout) :: negmaxa   ! negative max area
    real(kind=KTGT), intent(in)    :: asec(node_xi:node_yo, 0:*)
    real(kind=KTGT), intent(in)    :: aside(0:lnodes, 0:*)
    integer,         intent(in)    :: ngraph(0:*)
    integer,         intent(in)    :: nseq
    real(kind=KTGT), intent(in)    :: uslat
    integer,         intent(in)    :: relpos(*)
    integer,         intent(in)    :: nprops(*)
    integer,         intent(in)    :: levv
    character(len=*),intent(in)    :: tag
    integer,         intent(in)    :: jlonph, jlatph

    integer jf, jt, jq
    integer jseq, jend
    real(kind=KTGT) :: aquad(0:nquad-1, node_1:node_4, 0:accum_num-1)
    real(kind=KTGT) :: pquad(0:nquad-1, node_1:node_4, 0:accum_num-1)
    real(kind=KTGT) :: asum
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT) :: posmina, posmaxa
    real(kind=KTGT) :: rsp
    character(len=256) :: txt, txt2
    logical bx
    integer jerr
    integer jc

    jseq = 0
    aquad = ZERO
    pquad = ZERO
    ! write(*, *) 'collect', nseq, ngraph(0:nseq+1)
    do
       if (jseq.ge.nseq) exit
       jf = ngraph(jseq)
       select case(nprops(jf))
       case(no_division) ! same
          jend = jseq + 1
          aquad(relpos(jf), jf, :) = aside(jseq, 0:accum_num-1)
       case(div_in_two) ! div in two
          jend = jseq + 2
          jt = ngraph(jend)
          aquad(relpos(jf), jf, :) = aside(jseq, 0:accum_num-1)
          aquad(relpos(jt), jf, :) = aside(jseq+1, 0:accum_num-1)
          pquad(relpos(jf), jf, :) = asec(ngraph(jseq+1), 0:accum_num-1)
       case default
          jend = jseq + 3
          jt = ngraph(jend)
          aquad(relpos(jf), jf, :) = aside(jseq, 0:accum_num-1)
          aquad(relpos(jt), jf, :) = aside(jseq+2, 0:accum_num-1)
          aquad(nprops(jf), jf, :) = aside(jseq+1, 0:accum_num-1)
          pquad(relpos(jf), jf, :) = asec(ngraph(jseq+1), 0:accum_num-1)
          pquad(nprops(jf), jf, :) = asec(ngraph(jseq+2), 0:accum_num-1)
       end select
       jseq = jend
    enddo

    jc = accum_w0
    do jc = accum_w0, accum_w0
       ! aquad(:, :, jc) = aquad(:, :, jc) + pquad(:, :, jc)
       adiv(0:nquad-1, jc) = (aquad(0:nquad-1, node_1, jc) + aquad(0:nquad-1, node_4, jc)) &
            &              + (aquad(0:nquad-1, node_2, jc) + aquad(0:nquad-1, node_3, jc))
       adiv(0:nquad-1, jc) = adiv(0:nquad-1, jc)  &
            &              + ((pquad(0:nquad-1, node_1, jc) + pquad(0:nquad-1, node_4, jc)) &
            &              +  (pquad(0:nquad-1, node_2, jc) + pquad(0:nquad-1, node_3, jc)))
       asum = (adiv(0, jc) + adiv(3, jc)) + (adiv(1, jc) + adiv(2, jc))
       adiv(0:nquad-1, jc) = adiv(0:nquad-1, jc) / asum
    enddo
    jc = accum_w0

    jc = accum_w1la
    do jc = accum_w1la, accum_w1la
       ! aquad(:, :, jc) = aquad(:, :, jc) + pquad(:, :, jc)
       ! adiv(0:nquad-1, jc) = (aquad(0:nquad-1, node_1, jc) + aquad(0:nquad-1, node_4, jc)) &
       !      &              + (aquad(0:nquad-1, node_2, jc) + aquad(0:nquad-1, node_3, jc))
       adiv(0:nquad-1, jc) = (aquad(0:nquad-1, node_1, jc) + aquad(0:nquad-1, node_4, jc)) &
            &              + (aquad(0:nquad-1, node_2, jc) + aquad(0:nquad-1, node_3, jc))
       adiv(0:nquad-1, jc) = adiv(0:nquad-1, jc)  &
            &              + ((pquad(0:nquad-1, node_1, jc) + pquad(0:nquad-1, node_4, jc)) &
            &              +  (pquad(0:nquad-1, node_2, jc) + pquad(0:nquad-1, node_3, jc)))
    enddo
    jc = accum_w1la

    do jc = accum_w1lo, accum_w1lo
       ! aquad(:, :, jc) = aquad(:, :, jc) + pquad(:, :, jc)
       ! adiv(0:nquad-1, jc) = (aquad(0:nquad-1, node_1, jc) + aquad(0:nquad-1, node_4, jc)) &
       !      &              + (aquad(0:nquad-1, node_2, jc) + aquad(0:nquad-1, node_3, jc))
       adiv(0:nquad-1, jc) = (aquad(0:nquad-1, node_1, jc) + aquad(0:nquad-1, node_4, jc)) &
            &              + (aquad(0:nquad-1, node_2, jc) + aquad(0:nquad-1, node_3, jc))
       adiv(0:nquad-1, jc) = adiv(0:nquad-1, jc)  &
            &              + ((pquad(0:nquad-1, node_1, jc) + pquad(0:nquad-1, node_4, jc)) &
            &              +  (pquad(0:nquad-1, node_2, jc) + pquad(0:nquad-1, node_3, jc)))
       adiv(0:nquad-1, jc) = adiv(0:nquad-1, jc) / uslat
    enddo

    if (ANY(adiv(0:nquad-1, accum_w0).lt.0.0_KTGT) &
         & .or.ANY(adiv(0:nquad-1, accum_w0).gt.1.0_KTGT)) then
       posmaxa = maxval(adiv(0:nquad-1, accum_w0))
       posmina = minval(adiv(0:nquad-1, accum_w0), adiv(0:nquad-1, accum_w0).gt.ZERO)
       negmaxa = min(negmaxa, minval(adiv(0:nquad-1, accum_w0)))
       rsp = rrspacing(posmina)
       bx = posmaxa.gt.1.0_KTGT
       if (.not.bx) bx = abs(negmaxa).gt.rsp
       if (.not.bx) bx = is_msglev_DEBUG(levv)

136    format(A, 'bad-area: ', I0, 1x, I0, 1x, A)
137    format(A, '   aquad: ', I0, 1x, A)
138    format(A, '   pquad: ', I0, 1x, A)
139    format(A, '   aside: ', 1x, A)
135    format(A, '    asec: ', 1x, A)
       if (bx) then
          call join_list(jerr, txt, adiv(0:nquad-1, accum_w0), fmt='(ES11.3)')
          write(txt2, 136) trim(tag), jlatph, jlonph, trim(txt)
          call msg(txt2)
          do jq = 0, nquad - 1
             if (adiv(jq, accum_w0).lt.0.0_KTGT .or. adiv(jq, accum_w0).gt.1.0_KTGT) then
                call join_list(jerr, txt, aquad(jq, node_1:node_4, accum_w0), fmt='(ES11.3)')
                write(txt2, 137) trim(tag), jq, trim(txt)
                call msg(txt2)
                call join_list(jerr, txt, pquad(jq, node_1:node_4, accum_w0), fmt='(ES11.3)')
                write(txt2, 138) trim(tag), jq, trim(txt)
                call msg(txt2)
             endif
          enddo
          call join_list(jerr, txt, aside(0:nseq-1, accum_w0), fmt='(ES11.3)')
          write(txt2, 139) trim(tag), trim(txt)
          call msg(txt2)
          call join_list(jerr, txt, asec(:, accum_w0), fmt='(ES11.3)')
          write(txt2, 135) trim(tag), trim(txt)
          call msg(txt2)
       endif
    endif
  end subroutine ps2g_neighbor_collect_d


!!!_   & div_ps2g_advance
  subroutine div_ps2g_advance_d &
       & (pstack,  jadd,    jpos, &
       &  cstride, cachela, jlatc, cachelo, jlonc, destl, tol)
    use TOUZA_Emu_ugg,only: psgp_fwd
    implicit none
    integer,parameter  :: KTGT=KDBL
    integer,        intent(out)   :: jadd
    integer,        intent(inout) :: pstack(prop_psg_size, 0:*) ! work area
    integer,        intent(in)    :: jpos
    integer,        intent(in)    :: cstride(0:*)
    real(kind=KTGT),intent(in)    :: cachelo(:, 0:)
    real(kind=KTGT),intent(in)    :: cachela(:, 0:)
    integer,        intent(in)    :: jlatc, jlonc
    real(kind=KTGT),intent(in)    :: destl(*)
    real(kind=KTGT),intent(in)    :: tol

    integer,parameter :: nnode = 4, xnode = 5
    integer         :: gdbl(lxy, nnode),  xdbl(lxy, xnode)
    real(kind=KTGT) :: xpos(lxy, xnode),  xidx(lxy, xnode)
    integer klev, cstep
    integer jtlo, jtla
    integer jclo, jcla

    gdbl(coor_x, 1:nnode) = pstack(prop_psg_x1:prop_psg_x4, jpos)
    gdbl(coor_y, 1:nnode) = pstack(prop_psg_y1:prop_psg_y4, jpos)
    klev = pstack(prop_gtlev, jpos)

    jtlo = pstack(prop_gtlo, jpos) * 2
    jtla = pstack(prop_gtla, jpos) * 2
    ! 4 3 2 1
    pstack(prop_gtlo,  jpos:jpos+3) = (/jtlo+1, jtlo,   jtlo+1, jtlo/)
    pstack(prop_gtla,  jpos:jpos+3) = (/jtla+1, jtla+1, jtla,   jtla/)
    pstack(prop_gtlev, jpos:jpos+3) = klev + 1

    cstep = cstride(klev + 1)
    jclo = (jtlo + 1) * cstep + jlonc
    jcla = (jtla + 1) * cstep + jlatc

    !  [3](5)[4]  (xpos)
    !  (2)(3)(4)  [gpos]
    !  [1](1)[2]

    xpos(:,1) = psgp_fwd(cachelo(:, jclo),       cachela(:, jcla-cstep))
    xpos(:,2) = psgp_fwd(cachelo(:, jclo-cstep), cachela(:, jcla))
    xpos(:,3) = psgp_fwd(cachelo(:, jclo),       cachela(:, jcla))
    xpos(:,4) = psgp_fwd(cachelo(:, jclo+cstep), cachela(:, jcla))
    xpos(:,5) = psgp_fwd(cachelo(:, jclo),       cachela(:, jcla+cstep))

    xidx(coor_x, 1:xnode) = (xpos(coor_x, 1:xnode) - destl(coor_x))
    xidx(coor_y, 1:xnode) = (xpos(coor_y, 1:xnode) - destl(coor_y))

    xdbl(:, 1:xnode) = floor(xidx(:, 1:xnode) + tol) + ceiling(xidx(:, 1:xnode) - tol)

    pstack(prop_psg_x1:prop_psg_x4, jpos+3) = (/gdbl(coor_x,1), xdbl(coor_x,1), xdbl(coor_x,2), xdbl(coor_x,3)/)
    pstack(prop_psg_y1:prop_psg_y4, jpos+3) = (/gdbl(coor_y,1), xdbl(coor_y,1), xdbl(coor_y,2), xdbl(coor_y,3)/)
    pstack(prop_psg_x1:prop_psg_x4, jpos+2) = (/xdbl(coor_x,1), gdbl(coor_x,2), xdbl(coor_x,3), xdbl(coor_x,4)/)
    pstack(prop_psg_y1:prop_psg_y4, jpos+2) = (/xdbl(coor_y,1), gdbl(coor_y,2), xdbl(coor_y,3), xdbl(coor_y,4)/)
    pstack(prop_psg_x1:prop_psg_x4, jpos+1) = (/xdbl(coor_x,2), xdbl(coor_x,3), gdbl(coor_x,3), xdbl(coor_x,5)/)
    pstack(prop_psg_y1:prop_psg_y4, jpos+1) = (/xdbl(coor_y,2), xdbl(coor_y,3), gdbl(coor_y,3), xdbl(coor_y,5)/)
    pstack(prop_psg_x1:prop_psg_x4, jpos+0) = (/xdbl(coor_x,3), xdbl(coor_x,4), xdbl(coor_x,5), gdbl(coor_x,4)/)
    pstack(prop_psg_y1:prop_psg_y4, jpos+0) = (/xdbl(coor_y,3), xdbl(coor_y,4), xdbl(coor_y,5), gdbl(coor_y,4)/)

    jadd = 3
  end subroutine div_ps2g_advance_d

!!!_   & div_ps2g_simple_fallback
  subroutine div_ps2g_simple_fallback_d &
       & (ptiles, jtabr,   &
       &  gmax,   gmin,    &
       &  afact,  cstride, pstack,    &
       &  jlatc,  jlonc,   jclon_rep, repfac, lintg2, &
       &  mx,     my,      mdest,     jdext,  jpos)
    use TOUZA_Emu_ugg,only: psgp_fwd
    implicit none
    integer,parameter  :: KTGT=KDBL
    integer,parameter  :: KDBG=KFLT
    integer,        intent(in)    :: mx, my, mdest, jdext
    real(kind=KTGT),intent(inout) :: ptiles(0:mdest, 0:*)
    integer,        intent(inout) :: jtabr(lrange, *)
    integer,        intent(in)    :: gmax(*),  gmin(*)
    real(kind=KTGT),intent(in)    :: afact(0:*)    ! for unit area
    real(kind=KTGT),intent(in)    :: lintg2(0:*)   ! for unit area
    integer,        intent(in)    :: cstride(0:*)
    integer,        intent(in)    :: pstack(prop_psg_size, 0:*) ! work area
    integer,        intent(in)    :: jlatc, jlonc
    integer,        intent(in)    :: jclon_rep
    real(kind=KTGT),intent(in)    :: repfac
    integer,        intent(in)    :: jpos

    integer :: klev
    integer jdxl, jdxh, jdyl, jdyh
    integer jcla, jclo, cstep

    real(kind=KTGT) :: f2, f3
    real(kind=KTGT) :: w(accum_w0:accum_num-1)

    klev = pstack(prop_gtlev, jpos)

    cstep = cstride(klev)
    jcla = pstack(prop_gtla, jpos) * cstep + jlatc
    jclo = pstack(prop_gtlo, jpos) * cstep + jlonc

    f2 = real(cstep, kind=KTGT) / repfac
    f3 = real(2 * (jclo - jclon_rep) + cstep, KIND=KTGT) / (2.0_KTGT * repfac)

    w(accum_w0) = afact(klev+1)
    w(accum_w1la) = ((lintg2(jcla + cstep) - lintg2(jcla)) * f2) / 4.0_KTGT
    w(accum_w1lo) = afact(klev+1) * f3

    ! write(*, *) 'w3', w(accum_w1lo)

    jdxl = (gmin(coor_x) + 2) / 2 - 1
    jdyl = (gmin(coor_y) + 2) / 2 - 1
    jdxh = (gmax(coor_x) + 1) / 2 - 1
    jdyh = (gmax(coor_y) + 1) / 2 - 1

    ! [caution] w3 is distributed evenly to simplify
    call ps2g_simple_fallback &
         & (ptiles, jdxl, jdyl, mx, my, jdext, w, mdest)
    call ps2g_simple_fallback &
         & (ptiles, jdxl, jdyh, mx, my, jdext, w, mdest)
    call ps2g_simple_fallback &
         & (ptiles, jdxh, jdyh, mx, my, jdext, w, mdest)
    call ps2g_simple_fallback &
         & (ptiles, jdxh, jdyl, mx, my, jdext, w, mdest)

    if (jdxl.ge.0) jtabr(rmin, coor_x) = min(jdxl, jtabr(rmin, coor_x))
    if (jdyl.ge.0) jtabr(rmin, coor_y) = min(jdyl, jtabr(rmin, coor_y))
    if (jdxh.ge.0) jtabr(rmin, coor_x) = min(jdxh, jtabr(rmin, coor_x))
    if (jdyh.ge.0) jtabr(rmin, coor_y) = min(jdyh, jtabr(rmin, coor_y))

    if (jdxl.lt.mx) jtabr(rmax, coor_x) = max(jdxl, jtabr(rmax, coor_x))
    if (jdyl.lt.my) jtabr(rmax, coor_y) = max(jdyl, jtabr(rmax, coor_y))
    if (jdxh.lt.mx) jtabr(rmax, coor_x) = max(jdxh, jtabr(rmax, coor_x))
    if (jdyh.lt.my) jtabr(rmax, coor_y) = max(jdyh, jtabr(rmax, coor_y))

  end subroutine div_ps2g_simple_fallback_d

!!!_   & ps2g_simple_fallback
  subroutine ps2g_simple_fallback_d &
       & (ptiles, jx, jy, mx, my, jdext, ua, mdest)
    use TOUZA_Ami_std,only: inrange
    implicit none
    integer,parameter  :: KTGT=KDBL
    integer,        intent(in)    :: mdest
    real(kind=KTGT),intent(inout) :: ptiles(0:mdest, accum_w0:*)
    integer,        intent(in)    :: jx, jy
    integer,        intent(in)    :: mx, my
    integer,        intent(in)    :: jdext
    real(kind=KTGT),intent(in)    :: ua(accum_w0:*)
    integer jh

    if (inrange(jx, 0, mx - 1).and.inrange(jy, 0, my - 1)) then
       jh = jy * mx + jx
    else
       jh = jdext
    endif
    ptiles(jh, accum_w0) = ptiles(jh, accum_w0) + ua(accum_w0)
    ptiles(jh, accum_w1la) = ptiles(jh, accum_w1la) + ua(accum_w1la)
    ptiles(jh, accum_w1lo) = ptiles(jh, accum_w1lo) + ua(accum_w1lo)

  end subroutine ps2g_simple_fallback_d

!!!_   & div_ps2g_fallback
  subroutine div_ps2g_fallback_d &
       & (ptiles,  jtabr,   &
       &  afact,   cstride, pstack,  &
       &  cachelo, jlonc,   cachela, jlatc,  jclon_rep, repfac, lintg2, &
       &  destl,   mx,      my,      mdest,  jdext,     &
       &  jpos,    tol,     jlonph,  jlatph, udump)
    use TOUZA_Emu_ugg,only: psgp_fwd
    implicit none
    integer,parameter  :: KTGT=KDBL
    integer,parameter  :: KDBG=KFLT
    integer,        intent(in)    :: mx, my, mdest, jdext
    real(kind=KTGT),intent(inout) :: ptiles(0:mdest, 0:*)
    integer,        intent(inout) :: jtabr(lrange, *)
    real(kind=KTGT),intent(in)    :: afact(0:*)    ! for unit area
    integer,        intent(in)    :: cstride(0:*)
    real(kind=KTGT),intent(in)    :: lintg2(0:*)
    integer,        intent(in)    :: pstack(prop_psg_size, 0:*) ! work area
    real(kind=KTGT),intent(in)    :: cachelo(:, 0:)
    real(kind=KTGT),intent(in)    :: cachela(:, 0:)
    integer,        intent(in)    :: jlonc, jlatc
    integer,        intent(in)    :: jclon_rep
    real(kind=KTGT),intent(in)    :: repfac
    real(kind=KTGT),intent(in)    :: destl(*)
    integer,        intent(in)    :: jpos
    real(kind=KTGT),intent(in)    :: tol
    integer,        intent(in)    :: jlonph, jlatph
    integer,        intent(in)    :: udump

    integer,parameter :: nnode=4
    integer           :: gdbl(lxy, nnode), mdbl(lxy)
    integer           :: gtgt(lxy)
    real(kind=KTGT)   :: f2, f3, uhalf, useg
    real(kind=KTGT)   :: mpos(lxy), midx(lxy)
    integer :: klev, cstep, jclo, jcla
    integer js
    integer jdx, jdy, jdh
    logical xeven, yeven
    logical xmeven, ymeven

    klev = pstack(prop_gtlev, jpos)
    cstep = cstride(klev)
    jclo = pstack(prop_gtlo, jpos) * cstep + jlonc
    jcla = pstack(prop_gtla, jpos) * cstep + jlatc

    f3 = real(2 * (jclo - jclon_rep) + cstep, KIND=KTGT) / (2.0_KTGT * repfac)
    f2 = real(cstep, kind=KTGT) / repfac
    f2 = f2 * (lintg2(jcla + cstep) - lintg2(jcla))
    ! write (*, *) jclo, cstep / 2, f3

    gdbl(coor_x, 1:nnode) = pstack(prop_psg_x1:prop_psg_x4, jpos)
    gdbl(coor_y, 1:nnode) = pstack(prop_psg_y1:prop_psg_y4, jpos)

    mpos(:) = psgp_fwd(cachelo(:, jclo + cstep / 2), cachela(:, jcla + cstep / 2))
    midx(1:lxy) = mpos(1:lxy) - destl(1:lxy)
    mdbl(1:lxy) = floor(midx(1:lxy) + tol) + ceiling(midx(1:lxy) - tol)
    xmeven = mod(mdbl(coor_x), 2).eq.0
    ymeven = mod(mdbl(coor_y), 2).eq.0

    useg = afact(klev+1)
    uhalf = afact(klev+1) / 2.0_KTGT

    ! write(*, *) 'w2', f2

    do js = 1, nnode
       gtgt(:) = gdbl(:, js)
       xeven = mod(gtgt(coor_x), 2).eq.0
       yeven = mod(gtgt(coor_y), 2).eq.0
       if (xeven.and.yeven) then
          ! if node is on the intersects
          xeven = xmeven
          yeven = ymeven
          gtgt(:) = mdbl(:)
       endif
       ! It is possible to have xeven and yeven both true,
       ! however, it should be rare for most practical cases.
       jdx = (gtgt(coor_x) + 2) / 2 - 1
       jdy = (gtgt(coor_y) + 2) / 2 - 1
       if (xeven) then
          if (inrange(jdx, 0, mx - 1).and.inrange(jdy, 0, my - 1)) then
             jdh = jdy * mx + jdx
             jtabr(rmax, coor_x:coor_y) = max(jtabr(rmax, coor_x:coor_y), (/jdx, jdy/))
             jtabr(rmin, coor_x:coor_y) = min(jtabr(rmin, coor_x:coor_y), (/jdx, jdy/))
          else
             jdh = jdext
          endif
          ptiles(jdh,accum_w0) = ptiles(jdh,accum_w0) + uhalf
          ptiles(jdh,accum_w1lo) = ptiles(jdh,accum_w1lo) + uhalf * f3
          ptiles(jdh,accum_w1la) = ptiles(jdh,accum_w1la) + f2 / 8.0_KTGT

          jdx = jdx - 1
          if (inrange(jdx, 0, mx - 1).and.inrange(jdy, 0, my - 1)) then
             jdh = jdy * mx + jdx
             jtabr(rmax, coor_x:coor_y) = max(jtabr(rmax, coor_x:coor_y), (/jdx, jdy/))
             jtabr(rmin, coor_x:coor_y) = min(jtabr(rmin, coor_x:coor_y), (/jdx, jdy/))
          else
             jdh = jdext
          endif
          ptiles(jdh,accum_w0) = ptiles(jdh,accum_w0) + uhalf
          ptiles(jdh,accum_w1lo) = ptiles(jdh,accum_w1lo) + uhalf * f3
          ptiles(jdh,accum_w1la) = ptiles(jdh,accum_w1la) + f2 / 8.0_KTGT
       else if (yeven) then
          if (inrange(jdx, 0, mx - 1).and.inrange(jdy, 0, my - 1)) then
             jdh = jdy * mx + jdx
             jtabr(rmax, coor_x:coor_y) = max(jtabr(rmax, coor_x:coor_y), (/jdx, jdy/))
             jtabr(rmin, coor_x:coor_y) = min(jtabr(rmin, coor_x:coor_y), (/jdx, jdy/))
          else
             jdh = jdext
          endif
          ptiles(jdh,accum_w0) = ptiles(jdh,accum_w0) + uhalf
          ptiles(jdh,accum_w1lo) = ptiles(jdh,accum_w1lo) + uhalf * f3
          ptiles(jdh,accum_w1la) = ptiles(jdh,accum_w1la) + f2 / 8.0_KTGT
          jdy = jdy - 1
          if (inrange(jdx, 0, mx - 1).and.inrange(jdy, 0, my - 1)) then
             jdh = jdy * mx + jdx
             jtabr(rmax, coor_x:coor_y) = max(jtabr(rmax, coor_x:coor_y), (/jdx, jdy/))
             jtabr(rmin, coor_x:coor_y) = min(jtabr(rmin, coor_x:coor_y), (/jdx, jdy/))
          else
             jdh = jdext
          endif
          ptiles(jdh,accum_w0) = ptiles(jdh,accum_w0) + uhalf
          ptiles(jdh,accum_w1lo) = ptiles(jdh,accum_w1lo) + uhalf * f3
          ptiles(jdh,accum_w1la) = ptiles(jdh,accum_w1la) + f2 / 8.0_KTGT
       else
          if (inrange(jdx, 0, mx - 1).and.inrange(jdy, 0, my - 1)) then
             jdh = jdy * mx + jdx
             jtabr(rmax, coor_x:coor_y) = max(jtabr(rmax, coor_x:coor_y), (/jdx, jdy/))
             jtabr(rmin, coor_x:coor_y) = min(jtabr(rmin, coor_x:coor_y), (/jdx, jdy/))
          else
             jdh = jdext
          endif
          ptiles(jdh,accum_w0) = ptiles(jdh,accum_w0) + useg
          ptiles(jdh,accum_w1lo) = ptiles(jdh,accum_w1lo) + useg * f3
          ptiles(jdh,accum_w1la) = ptiles(jdh,accum_w1la) + f2 / 4.0_KTGT
       endif
    enddo

  end subroutine div_ps2g_fallback_d

!!!_   & set_tolerance()
  PURE &
  real(kind=KTGT) function set_tolerance_d(tol) result(r)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: tol

    r = tol
    if (r.gt.+1.0_KTGT) then
       r = 1.0_KTGT / r
    else if (r.lt.-1.0_KTGT) then
       r = 2.0_KTGT ** r
    else
       r = ABS(r)
    endif
  end function set_tolerance_d

!!!_   & set_coor_anchor - set geographic and plane coordinate of anchors(plane gridline intersections)
  subroutine set_coor_anchor_d &
       & (aplane, aglat, aglon, destd, destl, mx, my, cco)
    use TOUZA_Emu,only: psgp_bwd_tr, phase, rad2deg
    implicit none
    integer,parameter :: KTGT=KDBL
    real(kind=KTGT),intent(out) :: aplane(:, 0:, 0:)
    real(kind=KTGT),intent(out) :: aglat(:, 0:, 0:)
    real(kind=KTGT),intent(out) :: aglon(:, 0:, 0:)
    real(kind=KTGT),intent(in)  :: destd(*)
    real(kind=KTGT),intent(in)  :: destl(*)
    integer,        intent(in)  :: mx, my
    real(kind=KTGT),intent(in)  :: cco(*)

    real(kind=KTGT) :: cx, cy
    integer jx, jy

    do jy = 0, my
       cy = real(jy, kind=KTGT) * destd(coor_y) + destl(coor_y)
       do jx = 0, mx
          cx = real(jx, kind=KTGT) * destd(coor_x) + destl(coor_x)
          aplane(coor_x, jx, jy) = cx
          aplane(coor_y, jx, jy) = cy
          call psgp_bwd_tr &
               & (aglat(:, jx, jy), aglon(:, jx, jy), cx, cy, cco)
#if DEBUG_AMI_SYMM
          write(*, *) 'aglon', jx, jy, jx / 2, jy / 2, cx, cy, &
               & rad2deg(phase(aglon(:, jx, jy))), &
               & rad2deg(phase(aglat(:, jx, jy))), aglon(:, jx, jy)
#endif /* DEBUG_AMI_SYMM */
       enddo
    enddo
  end subroutine set_coor_anchor_d

!!!_   & set_gslat_node
  subroutine set_gslat_node_d &
       & (gslatn, dslatn, uslatn, cglatn, &
       &  latn,   wlat,   &
       &  jlatb,  jlate,  latile, cco,    dir, &
       &  deg)
    use TOUZA_Ami_std,only: choice
    use TOUZA_Emu_ugg,only: deg2rad, sind_canonical, sin_canonical
    use TOUZA_Emu_ugg,only: psgp_cachela
    implicit none
    integer,parameter :: KTGT=KDBL
    real(kind=KTGT), intent(out) :: gslatn(0:*)
    real(kind=KTGT), intent(out) :: dslatn(0:*)
    real(kind=KTGT), intent(out) :: uslatn(0:*)
    real(kind=KTGT), intent(out) :: cglatn(mcla, 0:*)
    real(kind=KTGT), intent(in)  :: latn(0:*)
    real(kind=KTGT), intent(in)  :: wlat(0:*)
    real(kind=KTGT), intent(in)  :: cco(*)
    integer,         intent(in)  :: jlatb, jlate
    integer,         intent(in)  :: latile
    integer,         intent(in)  :: dir
    logical,optional,intent(in)  :: deg(:)
    integer nlat, jla


    nlat = jlate - jlatb
    if (is_deg(deg, JLATI)) then
       gslatn(0:nlat) = sind_canonical(latn(jlatb:jlate))
    else
       gslatn(0:nlat) = sin_canonical(latn(jlatb:jlate))
    endif
    if (dir.lt.0) then
       dslatn(0:nlat-1) = - ABS(wlat(jlatb:jlate-1))
    else
       dslatn(0:nlat-1) = + ABS(wlat(jlatb:jlate-1))
    endif
    uslatn(0:nlat-1) = ABS(dslatn(0:nlat-1)) / real(latile, kind=KTGT)

    if (is_deg(deg, JLATI)) then
       do jla = 0, nlat
          call psgp_cachela(cglatn(:, jla), deg2rad(latn(jla+jlatb)), cco)
       enddo
    else
       do jla = 0, nlat
          call psgp_cachela(cglatn(:, jla), latn(jla+jlatb), cco)
       enddo
    endif
  end subroutine set_gslat_node_d

!!!_   & set_grlon_node
  subroutine set_grlon_node_d &
       & (ierr,  &
       &  glonn, dlonn, ulonn,  cglonn, &
       &  lonn,  dlon,  &
       &  jlonb, jlone, lotile, cco,    dir, &
       &  deg)
    use TOUZA_Ami_std,only: choice
    use TOUZA_Emu_ugg,only: deg2rad, sind_canonical, sin_canonical
    use TOUZA_Emu_ugg,only: psgp_cachelo_px, psgp_inquire
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,         intent(out) :: ierr
    real(kind=KTGT), intent(out) :: glonn(0:*)
    real(kind=KTGT), intent(out) :: dlonn(0:*)
    real(kind=KTGT), intent(out) :: ulonn(0:*)
    real(kind=KTGT), intent(out) :: cglonn(mclo, 0:*)
    real(kind=KTGT), intent(in)  :: lonn(0:*)
    real(kind=KTGT), intent(in)  :: dlon(0:*)
    integer,         intent(in)  :: jlonb, jlone
    real(kind=KTGT), intent(in)  :: cco(*)
    integer,         intent(in)  :: lotile
    integer,         intent(in)  :: dir
    logical,optional,intent(in)  :: deg(:)

    integer nlon, jlo
    real(kind=KTGT),parameter :: lonofs = 0.0_KTGT

    ierr = 0
    !! cco::olon must be ZERO
    if (ierr.eq.0) then
       nlon = jlone - jlonb
       if (is_deg(deg, JLONGI)) then
#if LON_DEGREE
          glonn(0:nlon) = lonn(jlonb:jlone) - lonofs
#if DEBUG_AMI_SYMM
          write(*, *) 'lonn', lonn(jlonb:jlone)
          write(*, *) 'glonn', glonn(0:nlon)
#endif /* DEBUG_AMI_SYMM */
          if (dir.lt.0) then
             dlonn(0:nlon-1) = - dlon(jlonb:jlone-1)
          else
             dlonn(0:nlon-1) = + dlon(jlonb:jlone-1)
          endif
          ulonn(0:nlon-1) = ABS(dlon(jlonb:jlone-1) / real(lotile, kind=KTGT))
#else
          glonn(0:nlon) = deg2rad(lonn(jlonb:jlone)) - lonofs
          if (dir.lt.0) then
             dlonn(0:nlon-1) = - ABS(deg2rad(dlon(jlonb:jlone-1)))
          else
             dlonn(0:nlon-1) = + ABS(deg2rad(dlon(jlonb:jlone-1)))
          endif
          ulonn(0:nlon-1) = ABS(deg2rad(dlon(jlonb:jlone-1) / real(lotile, kind=KTGT)))
#endif
       else
          glonn(0:nlon) = lonn(jlonb:jlone) - lonofs
          if (dir.lt.0) then
             dlonn(0:nlon-1) = - ABS(dlon(jlonb:jlone-1))
          else
             dlonn(0:nlon-1) = + ABS(dlon(jlonb:jlone-1))
          endif
          ulonn(0:nlon-1) = ABS(dlon(jlonb:jlone-1) / real(lotile, kind=KTGT))
       endif

       do jlo = 0, nlon
          call psgp_cachelo_px(cglonn(:, jlo), glonn(jlo), cco)
          ! write(*, *) 'cglonn:', jlo, lonn(jlo), sind_canonical(lonn(jlo)), glonn(jlo), cglonn(:, jlo)
       enddo
    endif
  end subroutine set_grlon_node_d

!!!_   & is_deg
  logical function is_deg(deg, dir) result(b)
    implicit none
    logical,intent(in),optional :: deg(*)
    integer,intent(in)          :: dir
    if (present(deg)) then
       b = deg(dir)
    else
       b = .FALSE.
    endif
  end function is_deg

!!!_   & is_seg_intersect()
  logical function is_seg_intersect_d &
       & (clat, clon, jla1, jla2, jlo1, jlo2, destl, desth) &
       & result(b)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Emu_ugg,only: psgp_fwd
    implicit none
    real(kind=KTGT),intent(in) :: clat(mcla, 0:*)
    real(kind=KTGT),intent(in) :: clon(mclo, 0:*)
    integer,        intent(in) :: jla1, jla2, jlo1, jlo2
    real(kind=KTGT),intent(in) :: destl(*), desth(*)

    real(kind=KTGT) :: pos(lxy, 4)
    real(kind=KTGT) :: pmin(lxy), pmax(lxy)

    b = .TRUE.

    ! write(*, *) 'clon0', clon(:,jlo1)
    ! write(*, *) 'clon1', clon(:,jlo2)
    ! write(*, *) 'clat0', clat(:,jla1)
    ! write(*, *) 'clat1', clat(:,jla2)

    pos(:, 1) = psgp_fwd(clon(:, jlo1), clat(:, jla1))
    pos(:, 2) = psgp_fwd(clon(:, jlo2), clat(:, jla1))
    pos(:, 3) = psgp_fwd(clon(:, jlo2), clat(:, jla2))
    pos(:, 4) = psgp_fwd(clon(:, jlo1), clat(:, jla2))

    pmin(1:2) = minval(pos(1:2, :), 2)
    pmax(1:2) = maxval(pos(1:2, :), 2)

    ! write(*, *) 'isi:x:', pos(1, :)
    ! write(*, *) 'isi:y:', pos(2, :)

    ! write(*, *) 'isi:lo:', jlo1, clon(:, jlo1)
    ! write(*, *) 'isi:lo:', jlo2, clon(:, jlo2)
    ! write(*, *) 'isi:la:', jla1, clat(:, jla1)
    ! write(*, *) 'isi:la:', jla2, clat(:, jla2)

    ! write(*, *) 'isi:', jla1, jla2, jlo1, jlo2, pmin(1), pmax(1), pmin(2), pmax(2)
    ! write(*, *) 'isi:d:', destl(1), desth(1), destl(2), desth(2)

    b =  (pmax(1).le.destl(1) &
         & .or. desth(1).le.pmin(1) &
         & .or. pmax(2).le.destl(2) &
         & .or. desth(2).le.pmin(2))
    b = .not. b
  end function is_seg_intersect_d

!!!_   & set_seg_lat_cache
  subroutine set_seg_lat_cache_d &
       & (cache, lintg2, slat0, slat1, dslat, cco, lcache)
    use TOUZA_Emu_ugg,only: psgp_cachela
    implicit none
    integer,parameter :: KTGT=KDBL
    real(kind=KTGT),intent(out) :: cache(:, 0:)
    real(kind=KTGT),intent(out) :: lintg2(0:*)
    real(kind=KTGT),intent(in)  :: slat0,  slat1, dslat
    real(kind=KTGT),intent(in)  :: cco(*)
    integer,        intent(in)  :: lcache

    real(kind=KTGT) :: uslat
    real(kind=KTGT) :: slat, lat

    integer jj, jc

    uslat = (dslat / real(lcache, kind=KTGT))
    do jj = 0, lcache / 2 - 1
       jc = jj
       slat = slat0 + uslat * real(jj, kind=KTGT)
       lat  = asin(slat)
       call psgp_cachela(cache(:, jc), lat, cco)
       lintg2(jc) = coeff2_line_integ(slat)
       ! write(*, *) 'cache/la/1', jc, cache(:, jc)
    enddo
    do jj = 0, lcache / 2 - 1
       jc = lcache - jj
       slat = slat1 - uslat * real(jj, kind=KTGT)
       lat  = asin(slat)
       call psgp_cachela(cache(:, jc), lat, cco)
       lintg2(jc) = coeff2_line_integ(slat)
    enddo

    jc = lcache / 2
    slat = (slat1 + slat0) * 0.5_KTGT
    lat  = asin(slat)
    call psgp_cachela(cache(:, jc), lat, cco)
    lintg2(jc) = coeff2_line_integ(slat)
  end subroutine set_seg_lat_cache_d

!!!_   & set_seg_rlon_cache
  subroutine set_seg_rlon_cache_d &
       & (cache, lon0, lon1, dlon, cco, lcache)
    use TOUZA_Emu_ugg,only: psgp_cachelo_px, deg2rad
    implicit none
    integer,parameter :: KTGT=KDBL
    real(kind=KTGT),intent(out) :: cache(:, 0:)
    real(kind=KTGT),intent(in)  :: lon0,   lon1
    real(kind=KTGT),intent(in)  :: dlon
    real(kind=KTGT),intent(in)  :: cco(*)
    integer,        intent(in)  :: lcache
    real(kind=KTGT) :: ulon
    real(kind=KTGT) :: lon

    integer jj, jc

    ulon = dlon / real(lcache, kind=KTGT)
    do jj = 0, lcache / 2 - 1
       jc = jj
       lon = lon0 + ulon * real(jj, kind=KTGT)
       call psgp_cachelo_px(cache(:, jc), lon, cco)
       ! write(*, *) 'cache/lo/1', jc, cache(:, jc), lon0, lonofs
    enddo
    do jj = 0, lcache / 2 - 1
       jc = lcache - jj
       lon = lon1 - ulon * real(jj, kind=KTGT)
       call psgp_cachelo_px(cache(:, jc), lon, cco)
    enddo
    jc = lcache / 2
    lon = (lon0 + lon1) * 0.5_KTGT
    call psgp_cachelo_px(cache(:, jc), lon, cco)

  end subroutine set_seg_rlon_cache_d

!!!_   & is_adjacent_cells()
  integer function is_adjacent_cells (pos1, pos2) result (k)
    implicit none
    integer,intent(in) :: pos1(*), pos2(*)
    integer kx, ky
    kx = is_neighbor(pos1(coor_x), pos2(coor_x))
    if (kx.lt.0) then
       k = stt_error
    else if (kx.gt.1) then
       k = stt_discont
    else
       ky = is_neighbor(pos1(coor_y), pos2(coor_y))
       if (ky.lt.0) then
          k = stt_error
       else
          k = min(ky * 2 + kx, stt_discont)
       endif
    endif
  end function is_adjacent_cells

!!!_   & is_neighbor() - 0 if same, 1 if neighbor, 2 otherwise
  ELEMENTAL &
  integer function is_neighbor (pos1, pos2) result (k)
    implicit none
    integer,intent(in) :: pos1, pos2
    integer h, l
    !    o   o   o   o   o
    ! -3-2-1 0 1 2 3 4 5 6
    !  . . . . 2 2 3 3 4 4 5    (h+3) / 2    (h<1 excluded)
    !  0 1 1 2 2 3 3 4 4 5 5    (l+4) / 2
    !  2|1 1 0 0|
    !  2|1 1 0 0 -|
    !    2 2|1 1 0 0|
    !    2 2|1 1 0 0 -|
    h = max(pos1, pos2)
    l = min(pos1, pos2)
    k = max(0, ((h + 3) / 2) - ((l + 4) / 2))
  end function is_neighbor

!!!_   & anchor_pos()
  ELEMENTAL &
  integer function anchor_pos(pos1, pos2) result (k)
    implicit none
    integer,intent(in) :: pos1, pos2
    integer h, l
    !   -1   0   1   2   3
    !    o   o   o   o   o
    ! -3-2-1 0 1 2 3 4 5 6
    !    0 0 1 1 2 2 3 3 4  (l + 2) / 2
    !    . . . 1 1 2 2 3 3  (h + 1) / 2
    !    |   1   |
    !        | 2 |
    !        |   3 |
    !        |   3   |
    !          | 3   |
    !            | 4 |
    h = max(pos1, pos2)
    l = min(pos1, pos2)
    k = (((h + 1) / 2) + ((l + 2) / 2)) - 1
    ! v = real(k, kind=kind(MOLD)) / 2.0_KTGT
  end function anchor_pos

!!!_   & parallel_dlongi ()
  real(kind=KTGT) function parallel_dlongi_d &
       & (dlon1, dlon2) &
       & result(v)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Emu_ugg,only: hpsub_angle, sub_angle
    implicit none
    real(kind=KTGT),intent(in) :: dlon1(*)
    real(kind=KTGT),intent(in) :: dlon2(*)
    real(kind=KTGT) :: dlsc(2)

    dlsc(1:2) = _SUB_ANGLE(dlon2, dlon1)
    v = ATAN2(dlsc(1), dlsc(2))
    ! dla = ATAN2(dlon2(1), dlon2(2)) - ATAN2(dlon1(1), dlon1(2))
  end function parallel_dlongi_d

!!!_   & nsort4 - sort 4-element array by /network/ method
  PURE &
  subroutine nsort4_d(seq, xy, idx)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,parameter :: n = 4, m = 2
    integer,        intent(out) :: seq(n)
    real(kind=KTGT),intent(in)  :: xy(m, n)
    integer,        intent(in)  :: idx
    integer,parameter :: ja(5) = (/1, 2, 1, 3, 2/)
    integer,parameter :: jb(5) = (/3, 4, 2, 4, 3/)
    integer k, j0, j1, t
    logical swap
    do k = 1, 2
       j0 = ja(k)
       j1 = jb(k)
       if (xy(idx, j0).le.xy(idx, j1)) then
          seq(j0) = j0
          seq(j1) = j1
       else
          seq(j0) = j1
          seq(j1) = j0
       endif
    enddo
    do k = 3, 5
       j0 = ja(k)
       j1 = jb(k)
       swap = xy(idx, seq(j0)) .ge. xy(idx, seq(j1))
       if (swap) then
          swap = xy(idx, seq(j0)).gt.xy(idx, seq(j1)) .or. seq(j0).gt.seq(j1)
       endif
       if (swap) then
          t = seq(j0)
          seq(j0) = seq(j1)
          seq(j1) = t
       endif
    enddo
  end subroutine nsort4_d

!!!_   & ps2g_invert_table
  subroutine ps2g_invert_table_d &
       & (ierr,   &
       &  ipofs,  ipprj,  wpprj, mph, lpmem, &
       &  igofs,  igprj,  wgprj, mgh, lgmem, &
       &  sort)
    use TOUZA_Ami_std,only: choice, bisection_find
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: lgmem, lpmem
    integer,         intent(out) :: ipofs(0:*)
    integer,         intent(out) :: ipprj(0:*)
    real(kind=KTGT), intent(out) :: wpprj(0:ps2g_weights-1, 0:*)
    integer,         intent(in)  :: mph
    integer,         intent(in)  :: igofs(0:*)
    integer,         intent(in)  :: igprj(0:*)
    real(kind=KTGT), intent(in)  :: wgprj(0:ps2g_weights-1, 0:*)
    integer,         intent(in)  :: mgh
    integer,optional,intent(in)  :: sort      ! positive/negative to ascending/descending sort (default +1)

    integer ksort
    integer jgh
    integer jph
    integer jt, jx
    integer jxb, jxe, nx
    integer,allocatable :: nc(:)

    ierr = 0

    allocate(nc(0:mph-1), STAT=ierr)
    if (ierr.eq.0) then
       nc(0:mph-1) = 0
       do jgh = 0, mgh - 1
          do jt = igofs(jgh), igofs(jgh+1) - 1
             jph = igprj(jt)
             if (jph.ge.0.and.jph.lt.mph) nc(jph) = nc(jph) + 1
          enddo
       enddo
       ipofs(0) = 0
       do jph = 0, mph - 1
          ipofs(jph+1) = ipofs(jph) + nc(jph)
       enddo
       nc(0:mph-1) = 0
    endif
    if (ierr.eq.0) then
       ksort = choice(+1, sort)
       ! insertion sort, simple but not effective
       if (ksort.ne.0) then
          do jgh = 0, mgh - 1
             do jt = igofs(jgh), igofs(jgh+1) - 1
                jph = igprj(jt)
                if (jph.ge.0.and.jph.lt.mph) then
                   jxb = ipofs(jph)
                   jxe = jxb + nc(jph)
                   nx  = jxe - jxb
                   jx = bisection_find &
                        & (wgprj(ps2g_w0, jt), wpprj(ps2g_w0, jxb:jxe-1), nx, ksort, jxb)
                   ! write(*, *) 'invert', jgh, jph, jxb, jxe, nx, jx
                   wpprj(0:ps2g_weights-1, jx+1:jxe) = wpprj(0:ps2g_weights-1, jx:jxe-1)
                   ipprj(jx+1:jxe) = ipprj(jx:jxe-1)
                   wpprj(0:ps2g_weights-1, jx) = wgprj(0:ps2g_weights-1, jt)
                   ipprj(jx) = jgh
                   nc(jph) = nc(jph) + 1
                endif
             enddo
          enddo
       else
          do jgh = 0, mgh - 1
             ! write(*, *) 'bwd:s', jgh, igofs(jgh)
             do jt = igofs(jgh), igofs(jgh+1) - 1
                jph = igprj(jt)
                if (jph.ge.0.and.jph.lt.mph) then
                   jx = ipofs(jph) + nc(jph)
                   write(*, *) jgh, jt, jx
                   ipprj(jx) = jgh
                   wpprj(0:ps2g_weights-1, jx) = wgprj(0:ps2g_weights-1, jt)
                   nc(jph) = nc(jph) + 1
                endif
             enddo
          enddo
          ! do jph = 0, mph - 1
          !    jx = ipofs(jph)
          !    jt = ipofs(jph+1)
          !    write(*, *) 'bwd:0', jph, jx, jt, '/', ipprj(jx:jt-1)
          !    write(*, *) 'bwd:1', jph, jx, jt, '/', wpprj(ps2g_w0,  jx:jt-1)
          !    write(*, *) 'bwd:2', jph, jx, jt, '/', wpprj(ps2g_w1fla, jx:jt-1)
          !    write(*, *) 'bwd:3', jph, jx, jt, '/', wpprj(ps2g_w1flo, jx:jt-1)
          ! enddo
       endif
    endif
    if (ierr.eq.0) deallocate(nc, STAT=ierr)
  end subroutine ps2g_invert_table_d

!!!_   & debug_dump_cell
  subroutine debug_dump_cell_d &
       & (pcache, pstack, cachelo, cachela, destl, jdest, klev, jpos, udump)
    use TOUZA_Emu_ugg,only: psgp_fwd
    implicit none
    integer,parameter  :: KTGT=KDBL
    integer,parameter  :: KDBG=KFLT
    integer,        intent(in) :: pcache(2, 0:*)
    integer,        intent(in) :: pstack(prop_psg_size, 0:*) ! work area
    real(kind=KTGT),intent(in) :: cachelo(:, 0:)
    real(kind=KTGT),intent(in) :: cachela(:, 0:)
    real(kind=KTGT),intent(in) :: destl(*)
    integer,        intent(in) :: jdest
    integer,        intent(in) :: klev, jpos
    integer,        intent(in) :: udump

    integer,parameter :: nnode = 4
    integer cstep
    integer jcx, jcy
    real(kind=KTGT) :: npos(lxy, nnode)
    integer jerr

    cstep = pcache(2, klev)
    jcx = pstack(prop_gtx, jpos) * cstep
    jcy = pstack(prop_gty, jpos) * cstep
    npos(:, 1) = psgp_fwd(cachelo(:, jcx),         cachela(:, jcy))
    npos(:, 2) = psgp_fwd(cachelo(:, jcx + cstep), cachela(:, jcy))
    npos(:, 3) = psgp_fwd(cachelo(:, jcx + cstep), cachela(:, jcy + cstep))
    npos(:, 4) = psgp_fwd(cachelo(:, jcx),         cachela(:, jcy + cstep))
    npos(1, 1:4) = npos(1, 1:4) - destl(1)
    npos(2, 1:4) = npos(2, 1:4) - destl(2)

    write(udump, IOSTAT=jerr) real((/jdest, klev/), kind=KDBG), real(npos(:,1:4), kind=KDBG)
    if (jerr.ne.0) call msg('failure in debug_dump_cell')
  end subroutine debug_dump_cell_d

!!!_ + end AMI_TABLE
end module TOUZA_AMI_TABLE
!!!_@ test_ami_table - test program
#if TEST_AMI_TABLE
program test_ami_table
  use TOUZA_Ami_table,at_init=>init, at_diag=>diag
  use TOUZA_Nio,nio_init=>init, nio_diag=>diag, nio_finalize=>finalize
  use TOUZA_Std,only: arg_init, arg_diag, parse, get_option, decl_pos_arg
  use TOUZA_Std,only: inrange
  implicit none
  integer ierr
  integer n
  integer o, d
  integer ktest

  ierr = 0

  if (ierr.eq.0) call arg_init(ierr)
  if (ierr.eq.0) call decl_pos_arg(ierr, 'TEST')
  if (ierr.eq.0) call parse(ierr)
  if (ierr.eq.0) call arg_diag(ierr)

  if (ierr.eq.0) call at_init(ierr)
  if (ierr.eq.0) call at_diag(ierr)

  if (ierr.eq.0) call get_option(ierr, ktest, 'TEST', 0)

  if (ierr.eq.0) then
     if (ktest.eq.0) then
        n = 24
        do d = 1, 4
           do o = 0, d - 1
              call test_table_1d(ierr, n,  d, o, 8, 0)
           enddo
        enddo

        n = 24
        do d = 1, 4
           do o = 0, d - 1
              call test_table_1d(ierr, n,  8, 0, d, o)
           enddo
        enddo
     else if (ktest.eq.1) then
        call batch_test_symm(ierr)
     else
        ! call nio_init(ierr)
        ! if (ierr.eq.0) call batch_test_gs(ierr)
        write(*, *) 'No more g2ps test.'
        ierr = ERR_NOT_IMPLEMENTED
     endif
  endif

  write(*,*) 'fine = ', ierr

  stop
contains
  subroutine test_table_1d &
       & (ierr, n, ddst, odst, dsrc, osrc)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: n
    integer,intent(in)  :: ddst, odst
    integer,intent(in)  :: dsrc, osrc

    integer,parameter :: lx = 1024
    real(kind=KDBL)   :: xdst(lx + 1)
    real(kind=KDBL)   :: xsrc(lx + 1)

    integer,parameter :: ltbl = 12
    real(kind=KDBL),save :: ftbl(lx, ltbl)
    integer,save         :: itbl(lx, ltbl)

    integer ndst, nsrc
    integer mtbl

    ierr = 0

    if (mod(n, ddst).ne.0) ierr = -1
    if (mod(n, dsrc).ne.0) ierr = -1

    write (*, *) 'TEST:', dsrc, osrc, ddst, odst

    if (ierr.eq.0) then
       ndst = n / abs(ddst)
       nsrc = n / abs(dsrc)
       if (ndst.gt.lx) ierr = -1
       if (nsrc.gt.lx) ierr = -1
    endif
    if (ierr.eq.0) then
       call setco(xdst, ndst+1, odst, ddst)
       call setco(xsrc, nsrc+1, osrc, dsrc)
    endif
    if (ierr.eq.0) then
102    format('X:', A, 1x, I0, 1x, 128(1x, F4.0))
       write (*, 102) 'src', nsrc, xsrc(1:nsrc+1)
       write (*, 102) 'dst', ndst, xdst(1:ndst+1)
       call cyclic_interp_table &
            & (ierr, &
            &  ftbl, itbl,  mtbl, &
            &  xdst, ndst, &
            &  xsrc, nsrc, &
            &  lx,   ltbl)
    endif
    write(*, *) 'check = ', ierr
    write(*, *)
  end subroutine test_table_1d

  subroutine setco(x, n, o, d)
    implicit none
    real(kind=KDBL),intent(out) :: x(*)
    integer,        intent(in)  :: n, o, d
    integer j
    if (d.gt.0) then
       do j = 0, n - 1
          x(1 + j) = real(o + j * d, kind=KDBL)
       enddo
    else
       do j = 0, n - 1
          x(n - j) = real(o - j * d, kind=KDBL)
       enddo
    endif
    return
  end subroutine setco

  subroutine batch_test_symm(ierr)
    implicit none
    integer,intent(out) :: ierr

    real(kind=KDBL) :: xl, xh, dx, yl, yh, dy
    integer mx, my
    integer j
    ierr = 0

    dx = 4.0_KDBL
    dy = dx
    do j = -10, -1
       xl = real(j, kind=KDBL)
       yl = xl
       mx = 3
       my = mx
       call test_symm(ierr, xl, dx, mx, yl, dy, my)
    enddo
    mx = 5
    xl = -10.0_KDBL
    dx = +6.0_KDBL
    xh = xl + mx * dx
    write(*, *) '#### xl = ', xl, xh
    do j = 0, +6
       yl = xl + real(j, kind=KDBL)
       dy = dx
       yh = yl + mx * dy
       write(*, *) '## yl = ', yl, yh
       call test_symm(ierr, +xl, dx, mx, yl, +dy, mx)
       call test_symm(ierr, +xl, dx, mx, yh, -dy, mx)
       call test_symm(ierr, -xh, dx, mx, yh, -dy, mx)
       call test_symm(ierr, -xh, dx, mx, yl, +dy, mx)
    enddo

    mx = 5
    xl = -9.0_KDBL
    dx = +6.0_KDBL
    xh = xl + mx * dx
    write(*, *) '#### xl = ', xl, xh
    do j = 0, +6
       yl = xl + real(j, kind=KDBL)
       dy = dx
       yh = yl + mx * dy
       write(*, *) '## yl = ', yl, yh
       call test_symm(ierr, +xl, dx, mx, yl, +dy, mx)
       call test_symm(ierr, +xl, dx, mx, yh, -dy, mx)
       call test_symm(ierr, -xh, dx, mx, yh, -dy, mx)
       call test_symm(ierr, -xh, dx, mx, yl, +dy, mx)
    enddo

  end subroutine batch_test_symm

  subroutine test_symm(ierr, xl, dx, mx, yl, dy, my)
    implicit none
    integer,intent(out) :: ierr

    real(kind=KDBL),intent(in):: xl, dx, yl, dy
    integer,intent(in) :: mx, my
    character(len=4) :: s
    integer kasp
    integer symmz(0:7)

    integer k
    ierr = 0
    k = is_symmetric_plane(xl, dx, mx, yl, dy, my)
101 format('symm:', I0, ':', A, 1x, 2(1x, F9.1, 1x, F9.1, 1x, I0))
102 format('   zone:', I0, 1x, 8(1x, I3))
    s = ' '
    if (IBITS(k, 0, 1).eq.1) then
       s(1:1) = 'x'
    else
       s(1:1) = '-'
    endif
    if (IBITS(k, 1, 1).eq.1) then
       s(2:2) = 'y'
    else
       s(2:2) = '-'
    endif
    if (IBITS(k, 2, 1).eq.1) then
       s(3:3) = '/'
    else
       s(3:3) = '-'
    endif
    if (IBITS(k, 3, 1).eq.1) then
       s(4:4) = '\'
    else
       s(4:4) = '-'
    endif

    write(*, 101) k, s, xl, dx, mx, yl, dy, my

    kasp = +1
    call set_symmetric_zone(ierr, symmz, k, kasp)
    write(*, 102) kasp, symmz

    kasp = -1
    call set_symmetric_zone(ierr, symmz, k, kasp)
    write(*, 102) kasp, symmz

  end subroutine test_symm

end program test_ami_table
#endif /* TEST_AMI_TABLE */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

!!!_! ami_table.F90. - TOUZA/Ami/table amida-coupler table procedures
! Maintainer: SAITO Fuyuki
! Created: May 2 2022
#define TIME_STAMP 'Time-stamp: <2023/06/03 14:35:50 fuyuki ami_table.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022, 2023
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
!!!_@ TOUZA_Ami_table - ami-da utilities
module TOUZA_Ami_table
!!!_ + modules
  use TOUZA_Ami_std, as_init=>init, as_diag=>diag, as_finalize=>finalize
  use TOUZA_Emu,only: ncache_stereog_la, ncache_stereog_lo
!!!_ + default
  implicit none
  private
!!!_ + parameter
  integer,parameter,public :: equidistant_strict = 1
  integer,parameter,public :: equidistant_enough = 2
  integer,parameter,public :: non_equidistant = 4
!!!_ + public
!!!_ + static
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = AMI_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global

  integer,parameter :: stereo_np = +2
  integer,parameter :: stereo_nh = +1
  integer,parameter :: stereo_eq = 0
  integer,parameter :: stereo_sh = -1
  integer,parameter :: stereo_sp = -2

  integer,parameter :: mcla = ncache_stereog_la
  integer,parameter :: mclo = ncache_stereog_lo

  integer,parameter :: prop_xidx = 1
  integer,parameter :: prop_yidx = 2
  integer,parameter :: prop_lev  = 3
  integer,parameter :: stack_nprop = 3

  integer,parameter :: prop_gtx    = 1   ! geological, tile, x
  integer,parameter :: prop_gty    = 2   ! geological, tile, y
  integer,parameter :: prop_gtlev  = 3   ! geological, tile, level
  integer,parameter :: prop_psg_x1 = 4   ! polar stereographic, node 0, index 2x
  integer,parameter :: prop_psg_x2 = 5   ! polar stereographic, node 0, index 2x
  integer,parameter :: prop_psg_x3 = 6   ! polar stereographic, node 0, index 2x
  integer,parameter :: prop_psg_x4 = 7   ! polar stereographic, node 0, index 2x
  integer,parameter :: prop_psg_y1 = 8   ! polar stereographic, node 0, index 2y
  integer,parameter :: prop_psg_y2 = 9   ! polar stereographic, node 0, index 2y
  integer,parameter :: prop_psg_y3 = 10  ! polar stereographic, node 0, index 2y
  integer,parameter :: prop_psg_y4 = 11  ! polar stereographic, node 0, index 2y

  integer,parameter :: prop_psg_size = 11

#define __MDL__ 't'
#define _ERROR(E) (E - ERR_MASK_AMI_TABLE)
!!!_ + type
!!!_ + interfaces
  interface check_monotonic
     module procedure check_monotonic_d
  end interface check_monotonic

  interface divide_gg_cache_lon
     module procedure divide_gg_cache_lon_d
  end interface divide_gg_cache_lon
  interface divide_gg_cache_lat
     module procedure divide_gg_cache_lat_d
  end interface divide_gg_cache_lat

  interface div_g2ps_map
     module procedure div_g2ps_map_d
  end interface div_g2ps_map
  interface div_g2ps_cell
     module procedure div_g2ps_cell_d
  end interface div_g2ps_cell

  interface is_inside_dest
     module procedure is_inside_dest_d
  end interface is_inside_dest

  interface angle_adjust
     module procedure angle_adjust_d
  end interface angle_adjust
  interface nsort4
     module procedure nsort4_d
  end interface nsort4

  interface expand_g2ps_proj
     module procedure expand_g2ps_proj_d
  end interface expand_g2ps_proj
  interface pack_g2ps_sphe
     module procedure pack_g2ps_sphe_d
  end interface pack_g2ps_sphe
!!!_ + public procedures
  public init, diag, finalize
  public cyclic_interp_table
  public geo_interp_table
  public div_g2ps_map
  public expand_g2ps_proj, pack_g2ps_sphe

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

    real(kind=KDBL) :: lonwd, loned, dlon
    real(kind=KDBL) :: lonws, lones
    real(kind=KDBL) :: f

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
       call check_monotonic(idird, xdst(1:ndst+1))
       call check_monotonic(idirs, xsrc(1:nsrc+1))
       ! TO DO: monotonic decreasing coordinates
       if (idird.le.0 .or. idirs.le.0) ierr = -1
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

!!!_  - check_monotonic
  subroutine check_monotonic_d(idir, x, ddev, jbgn, jend)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,        intent(out)          :: idir
    real(kind=KARG),intent(in)           :: x(0:)
    real(kind=KARG),intent(out),optional :: ddev  ! delta maximum-minimum
    integer,        intent(out),optional :: jbgn, jend

    real(kind=KARG) :: dmax, dmin
    integer j, n

    n = size(x)
    if (n.le.1) then
       idir = 0
       continue
    else if (x(0).lt.x(1)) then
       ! monotonic increase
       idir = +1
       do j = 1, n - 1
          if (x(j-1).ge.x(j)) then
             idir = 0
             exit
          endif
       enddo
    else if (x(0).gt.x(1)) then
       ! monotonic decrease
       idir = -1
       do j = 1, n - 1
          if (x(j-1).le.x(j)) then
             idir = 0
             exit
          endif
       enddo
    else
       idir = 0
    endif
    if (idir.gt.0) then
       call set_if_present(jbgn, 0)
       call set_if_present(jend, n)
    else if (idir.lt.0) then
       call set_if_present(jbgn, n-1)
       call set_if_present(jend, -1)
    else
       call set_if_present(jbgn, 0)
       call set_if_present(jend, 0)
    endif
    if (present(ddev)) then
       if (idir.eq.0) then
          ddev = +HUGE(0.0_KARG)
       else
          dmax = MAXVAL(x(1:n-1) - x(0:n-2))
          dmin = MINVAL(x(1:n-1) - x(0:n-2))
          ddev = dmax - dmin
          if (ddev.eq.0.0_KARG) then
             idir = idir * equidistant_strict
          else
             dmax = MAXVAL(abs(x(0:n-1)))
             dmin = MINVAL(abs(x(0:n-1)), abs(x(0:n-1)).gt.0.0_KARG)
             if (abs(ddev * (dmin/dmax)).lt.epsilon(ddev)) then
                idir = idir * equidistant_enough
             else
                idir = idir * non_equidistant
             endif
          endif
       endif
    endif
    return
  end subroutine check_monotonic_d
!!!_ + geographical from/to stereographic mapping
!!!_  - is_inside_dest()
  logical function is_inside_dest_d &
       & (lon0, lon1, lonofs, clat, cco, destl, desth) &
       & result(b)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Emu_ugg,only: proj_pstereog_cachelo, proj_pstereog, ncache_stereog_co
    implicit none
    real(kind=KTGT),intent(in) :: lon0, lon1, lonofs
    real(kind=KTGT),intent(in) :: clat(mcla, 0:*)
    real(kind=KTGT),intent(in) :: cco(*)
    real(kind=KTGT),intent(in) :: destl(*), desth(*)

    real(kind=KTGT) :: clon(mclo, 0:1)
    real(kind=KTGT) :: pos(2, 4)
    real(kind=KTGT) :: pmin(2), pmax(2)

    b = .TRUE.
    call proj_pstereog_cachelo(clon(:, 0), lon0 + lonofs, cco)
    call proj_pstereog_cachelo(clon(:, 1), lon1 + lonofs, cco)
    ! write(*, *) cco(1:ncache_stereog_co)
    ! write(*, *) lon0+lonofs, lon1+lonofs
    ! write(*, *) clon(:, 0), clon(:, 1)

    pos(:, 1) = proj_pstereog(clon(:, 0), clat(:, 0))
    pos(:, 2) = proj_pstereog(clon(:, 1), clat(:, 0))
    pos(:, 3) = proj_pstereog(clon(:, 0), clat(:, 1))
    pos(:, 4) = proj_pstereog(clon(:, 1), clat(:, 1))
    ! todo: need special treatment for longitudes {ts, ts+90, ts+180, ts+270}

    pmin(1:2) = minval(pos(1:2, :), 2)
    pmax(1:2) = maxval(pos(1:2, :), 2)
    ! write(*, *) pmin, pmax

    b =  (pmax(1).le.destl(1).or.desth(1).le.pmin(1) &
         & .or. pmax(2).le.destl(2).or.desth(2).le.pmin(2))
    b = .not. b
  end function is_inside_dest_d

!!!_  - angle_adjust()
  real(kind=KTGT) function angle_adjust_d(a, o, deg) result(r)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: a, o
    logical,        intent(in) :: deg
    real(kind=KTGT) :: p
    integer n

    if (a.eq.o) then
       r = a
       return
    endif
    if (deg) then
       p = 360.0_KTGT
    else
       p = ATAN2(0.0_KTGT, -1.0_KTGT)
    endif
    if (a.gt.o) then
       n = FLOOR((a - o) / p)
       r = a - p * real(n, KIND=KTGT)
    else
       n = CEILING((o - a) / p)
       r = a + p * real(n, KIND=KTGT)
    endif

  end function angle_adjust_d

!!!_  - div_g2ps_map
  subroutine div_g2ps_map_d &
       & (ierr,    iofs,   iprj,   wprj,  nmem,  lmem,  &
       &  lonn,    latn,   wlath,  mlon,  mlat,  &
       &  lonorg,  latorg, latts,  major, ecc,   &
       &  xl,      dx,     mx,     yl,    dy,    my,     &
       &  lonlev,  latlev, inilev, tol,    &
       &  u,       levv,   tag,    udump)
    use TOUZA_Std,    only: join_list, choice
    use TOUZA_Emu_ugg,only: deg2rad, rad2deg
    use TOUZA_Emu_ugg,only: ncache_stereog_co
    use TOUZA_Emu_ugg,only: proj_pstereog_set
    use TOUZA_Emu_ugg,only: proj_pstereog_cachela
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,parameter :: lxy = 2
    integer,         intent(out) :: ierr
    integer,         intent(out) :: iofs(0:*)    ! [(lo,la)+1] to subscript-vector offset (lv)
    integer,         intent(out) :: iprj(0:*)    ! [lv] to g(xy)-index
    real(kind=KTGT), intent(out) :: wprj(0:*)    ! [lv] to weights
    integer,         intent(out) :: nmem
    integer,         intent(in)  :: lmem
    real(kind=KTGT), intent(in)  :: lonn(0:*), latn(0:*)   ! (radian) node longitude [0:mlon] and latitude [0:mlat]
    real(kind=KTGT), intent(in)  :: wlath(0:*)             ! (radian) [d sin(lat)] / 2 ... for historical reason
    integer,         intent(in)  :: mlon,      mlat
    real(kind=KTGT), intent(in)  :: lonorg,    latorg,  latts ! (degree if deg)
    real(kind=KTGT), intent(in)  :: major,     ecc
    real(kind=KTGT), intent(in)  :: xl, dx
    real(kind=KTGT), intent(in)  :: yl, dy
    integer,         intent(in)  :: mx, my
    integer,         intent(in)  :: lonlev, latlev, inilev
    real(kind=KTGT), intent(in),optional :: tol
    integer,         intent(in),optional :: u, levv
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: udump

    integer(kind=KIOFS) :: jposb, jpose

    integer :: fulllev
    integer,parameter :: piecelev = 2

    real(kind=KTGT) :: destl(lxy), desth(lxy), destd(lxy)
    real(kind=KTGT) :: dlon,  ulon,  lonofs
    real(kind=KTGT) :: slat0, slat1, dslat, uslat
    integer pole, dir_lon
    logical eqlon
    integer locache, lacache, latile, lotile
    integer mdest, lstack

    integer,        allocatable :: levco(:)
    integer,        allocatable :: pstack(:, :)
    real(kind=KTGT),allocatable :: ntiles(:), ptiles(:)

    real(kind=KTGT),allocatable :: clat(:, :), clon(:, :)
    real(kind=KTGT) :: cco(ncache_stereog_co)
    real(kind=KTGT) :: cnlat(mcla, 2)

    real(kind=KTGT) :: ra, rb, rorg(2)
    real(kind=KTGT) :: lorgi
    integer jlat,  jlon,  jgh
    integer jplat, jplon, npstr
    integer jdmax(2), jdmin(2)
    integer jdh, jdx, jdy
    integer jxlon, jp
    real(kind=KTGT) :: lonsp

    integer kbig, ksml
    real(kind=KTGT) :: cfw
    real(kind=KTGT) :: ua,  fa
    real(kind=KTGT) :: HPI
#if   TEST_AMI_TABLE
    real(kind=KTGT) :: schk, sref
#endif
    logical blatc

    integer jerr
    integer utmp, lv
    character(len=128) :: txt
    integer ud

    ! to do: need special treatments where near the lon_c+0,pi/2,pi,3pi/2

    ierr = 0
    utmp = get_logu(u, ulog)
    ud   = choice(-1, udump)
    lv = choice(lev_verbose, levv)

    iofs(0) = 0
    nmem = 0
    cfw = abs(choice(0.0_KTGT, tol))
    if (cfw.ge.1.0_KTGT) cfw = 1.0_KTGT / cfw

    ! health check
    if (dx.le.0.0_KTGT.or.dy.le.0.0_KTGT) then
       ierr = _ERROR(ERR_INVALID_PARAMETER)
       return
    endif
    if (latts.gt.0) then
       pole = +1
    else if (latts.lt.0) then
       pole = -1
    else
       ierr = _ERROR(ERR_INVALID_PARAMETER)
       return
    endif
    call check_monotonic(dir_lon, lonn(0:mlon), dlon)
    eqlon = (abs(dir_lon).le.equidistant_enough)
    if (eqlon) then
       dlon = deg2rad(360.0_KTGT / real(mlon, kind=KTGT))
       if (dir_lon.lt.0) dlon = - dlon
    else
       dlon = 0.0_KTGT
    endif
    lonofs = - lonorg
    lorgi  = 0.0_KTGT

    destl(1:2) = (/xl, yl/) / (/dx, dy/)
    destd(1:2) = 1.0_KTGT
    desth(1:2) = destl(1:2) + real((/mx, my/), kind=KTGT)

    fulllev = min(lonlev, latlev)

    lstack   = 1 + 3 * fulllev
    ! ltile    = 2 ** fulllev
    lotile   = 2 ** lonlev
    latile   = 2 ** latlev
    locache  = 2 ** (lonlev + piecelev)
    lacache  = 2 ** (latlev + piecelev)
    npstr    = 2 ** (fulllev + piecelev)
    mdest    = mx * my
#if   TEST_AMI_TABLE
    sref     = (2.0_KTGT**lonlev) * (2.0_KTGT**latlev)
#endif

    fa = 1.0_KTGT

    if (ierr.eq.0) allocate(levco(0:fulllev), STAT=ierr)
    if (ierr.eq.0) allocate(pstack(prop_psg_size, 0:lstack), STAT=ierr)
    if (ierr.eq.0) allocate(ntiles(0:mdest),  ptiles(0:mdest), STAT=ierr)
    if (ierr.eq.0) allocate(clat(mcla, 0:lacache), clon(mclo, 0:locache), STAT=ierr)
    ! clon(:, lcache+1) is for longitude origin special
    if (ierr.ne.0) then
       ierr = _ERROR(ERR_ALLOCATION)
       return
    endif

    ntiles(:) = 0.0_KTGT
    ptiles(:) = 0.0_KTGT

    call proj_pstereog_set(cco, ecc, major, latts, lorgi, pole, dx, dy)
    ! adjust projection center at caller side

    HPI = ATAN2(1.0_KTGT, 0.0_KTGT)
    if (dir_lon.gt.0) then
       kbig = 1
    else
       kbig = 0
    endif
    ksml = 1 - kbig
    jposb = 0

    do jlat = 0, mlat - 1
       blatc = .FALSE.
       slat0 = sin(latn(jlat))
       slat1 = sin(latn(jlat + 1))
       dslat = wlath(jlat) * 2.0_KTGT
       if (slat1.lt.slat0) dslat = - dslat
       uslat = abs(dslat / real(latile, kind=KTGT))

       call proj_pstereog_cachela(cnlat(:, 1), latn(jlat), cco)
       call proj_pstereog_cachela(cnlat(:, 2), latn(jlat + 1), cco)

101    format('g2ps:i: ', I0, ',', I0)
102    format('g2ps:o: ', I0, ',', I0)

       do jlon = 0, mlon - 1
          if (is_inside_dest(lonn(jlon), lonn(jlon + 1), lonofs, cnlat, cco, destl, desth)) then
             if (is_msglev_INFO(lv)) then
                write(txt, 101) jlon, jlat
                call msg(txt, tag, utmp)
             endif
             if (.not.blatc) then
                call divide_gg_cache_lat &
                     & (clat, cco, slat0, slat1, dslat, lacache)
                blatc = .TRUE.
             endif
             call divide_gg_cache_lon &
                  & (clon, cco, lonn(jlon), lonn(jlon + 1), dlon, lonofs, locache)
             if (dlon.eq.0.0_KTGT) then
                ulon = abs((lonn(jlon+1) - lonn(jlon)) / real(lotile, kind=KTGT))
             else
                ulon = abs(dlon / real(lotile, kind=KTGT))
             endif
             jdmax(:) = - HUGE(0)
             jdmin(:) = + HUGE(0)
             levco(0:fulllev) = 0
121          format('dump: ', I0, 1x, I0, 1x, I0, 1x, I0)
             do jplat = 0, lacache - 1, npstr
                do jplon = 0, locache - 1, npstr
                   call div_g2ps_cell &
                        & (ntiles,  ptiles,  levco,   jdmax,   jdmin,   &
                        &  pstack,  &
                        &  mx, my,  destl,   &
                        &  fulllev, inilev,  piecelev, &
                        &  clon(:, jplon:jplon+npstr-1), clat(:, jplat:jplat+npstr-1), cfw, ud)
                enddo
             enddo
             if (ud.ge.0) then
                inquire(ud, POS=jpose, IOSTAT=ierr)
                jpose = jpose - 1
                write(txt, 121) jlon, jlat, jposb, jpose
                call msg(txt, tag, utmp)
                jposb = jpose
             endif
             if (is_msglev_DETAIL(lv)) then
                call join_list(jerr, txt, levco(0:fulllev))
                call msg(txt, tag, utmp)
             endif
             ua = abs(uslat * ulon)
#if   TEST_AMI_TABLE
             schk = SUM(ntiles(0:mdest)) + SUM(ptiles(0:mdest))
             if (schk.ne.sref) write(*, *) 'schk ', jlon, jlat, schk, sref, &
                  & SUM(ntiles(0:mdest)), SUM(ptiles(0:mdest))
#endif
          else
             if (is_msglev_INFO(lv)) then
                write(txt, 102) jlon, jlat
                call msg(txt, tag, utmp)
             endif
             jdmin(1:2) = (/-1, -1/)
             jdmax(1:2) = (/-2, -2/)
             jdh = mdest
             ptiles(jdh) = 0.0_KTGT
             ntiles(jdh) = 1.0_KTGT
             if (dlon.eq.0.0_KTGT) then
                ua = abs(dslat * (lonn(jlon+1) - lonn(jlon)))
             else
                ua = abs(dslat * dlon)
             endif
          endif
          do jdy = jdmin(2), jdmax(2)
             do jdx = jdmin(1), jdmax(1)
                jdh = jdy * mx + jdx
                if (ntiles(jdh).gt.0.0_KTGT.or.ptiles(jdh).gt.0.0_KTGT) then
                   if (nmem.lt.lmem) then
                      iprj(nmem) = jdh
                      wprj(nmem) = ntiles(jdh) + ptiles(jdh) * fa
                      wprj(nmem) = wprj(nmem) * ua
                   endif
                   nmem = nmem + 1
                   ntiles(jdh) = 0.0_KTGT
                   ptiles(jdh) = 0.0_KTGT
                endif
             enddo
          enddo

          jdh = mdest
          if (ntiles(jdh).gt.0.0_KTGT.or.ptiles(jdh).gt.0.0_KTGT) then
             if (nmem.lt.lmem) then
                iprj(nmem) = jdh
                wprj(nmem) = ntiles(jdh) + ptiles(jdh) * fa
                wprj(nmem) = wprj(nmem) * ua
             endif
             nmem = nmem + 1
             ntiles(jdh) = 0.0_KTGT
             ptiles(jdh) = 0.0_KTGT
          endif

          jgh = jlat * mlon + jlon
          iofs(jgh+1) = nmem
       enddo
    enddo

    if (ierr.eq.0) deallocate(levco, pstack, ntiles, ptiles, clat, clon, STAT=ierr)
  end subroutine div_g2ps_map_d

!!!_  - div_g2ps_cell
  subroutine div_g2ps_cell_d &
       & (ntiles,  ptiles,  levco,    jdmax,   jdmin,   &
       &  pstack,  &
       &  mx, my,  destl,   &
       &  fulllev, inilev,  cachelev, cachelo, cachela, tol, udump)
    use TOUZA_Emu_ugg,only: proj_pstereog
    use TOUZA_Ami_std,only: inrange
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,parameter :: KDBG=KFLT
    integer,parameter :: lxy = 2
    real(kind=KTGT),intent(out)   :: ntiles(0:*)     ! [0:mx*my]  must be reset before call  (larger)
    real(kind=KTGT),intent(out)   :: ptiles(0:*)     ! [0:mx*my]  must be reset before call  (smaller)
    integer,        intent(inout) :: levco(0:*)      ! need initialize before call
    integer,        intent(inout) :: jdmax(lxy),  jdmin(lxy)  ! need initialize before call
    integer,        intent(out)   :: pstack(prop_psg_size, 0:*) ! work area
    integer,        intent(in)    :: mx, my
    real(kind=KTGT),intent(in)    :: destl(lxy)
    real(kind=KTGT),intent(in)    :: cachelo(:, 0:)
    real(kind=KTGT),intent(in)    :: cachela(:, 0:)
    integer,        intent(in)    :: fulllev, inilev, cachelev
    real(kind=KTGT),intent(in)    :: tol
    integer,        intent(in)    :: udump

    integer,parameter :: nnode = 4, xnode = 5

    integer pcache(2, 0:fulllev)
    real(kind=KTGT) :: acache(0:fulllev+cachelev)
    real(kind=KTGT) :: npos(lxy, nnode),  xpos(lxy, xnode),  apos(lxy)
    real(kind=KTGT) :: nidx(lxy, nnode),  xidx(lxy, xnode),  aidx(lxy)
    integer         :: idpos(lxy, nnode), ixpos(lxy, xnode), iapos(lxy)
    integer         :: idmin(lxy), idmax(lxy), idspan(lxy)
    logical         :: bsame(lxy)

    real(kind=KTGT) :: ap

    integer :: jfx, jfy, fwidth
    integer :: jcx, jcy, cstep
    integer :: jtx, jty
    integer :: jdx, jdy, jdh
    integer :: jpx, jpy, pwidth
    integer :: iwidth
    integer :: cwidth
    integer :: jdext
    integer :: jl, jpos, klev

    integer :: limlev
    integer xset(4), yset(4), nset, js
    logical xeven, yeven

    limlev = fulllev + cachelev

    fwidth = 2 ** fulllev
    iwidth = 2 ** inilev
    cwidth = 2 ** limlev
    pwidth = 2 ** cachelev

    jdext = mx * my

    pcache(:, 0) = (/fwidth, cwidth/)
    do jl = 1, fulllev
       pcache(:, jl) = pcache(:, jl - 1) / 2
       acache(jl) = 4.0_KTGT ** (fulllev - jl)
    enddo
    acache(0) = 4.0_KTGT ** fulllev
    acache(fulllev+1) = 4.0_KTGT ** (-1)
    acache(fulllev+2) = 4.0_KTGT ** (-1) / 2.0_KTGT

    idspan(1:2) = (/mx, my/) * 2

! 101 format('result: ', I0, 1x, I0, 1x, I0, 1x, I0, 1x, I0, 1x, 8E24.15)
    do jfy = 0, iwidth - 1
       do jfx = 0, iwidth - 1
          jpos = 0
          ! set base cell
          pstack((/prop_gtx, prop_gty, prop_gtlev/), jpos) = (/jfx, jfy, inilev/)
          cstep = pcache(2, inilev)
          jcx = jfx * cstep
          jcy = jfy * cstep
          ! cell division      node position
          !  2 1                3 4
          !  4 3                1 2
          npos(:, 1) = proj_pstereog(cachelo(:, jcx),         cachela(:, jcy))
          npos(:, 2) = proj_pstereog(cachelo(:, jcx + cstep), cachela(:, jcy))
          npos(:, 3) = proj_pstereog(cachelo(:, jcx),         cachela(:, jcy + cstep))
          npos(:, 4) = proj_pstereog(cachelo(:, jcx + cstep), cachela(:, jcy + cstep))

          nidx(1, 1:4) = (npos(1, 1:4) - destl(1))
          nidx(2, 1:4) = (npos(2, 1:4) - destl(2))

          ! notes: floor + ceiling  == odd  when between the cell boundaries (exclusive)
          !                         == even when exactly on the cell boundary
          ! idpos(:, :) = floor(nidx(:, :)) + ceiling(nidx(:, :))
          idpos(:, :) = floor(nidx(:, :) + tol) + ceiling(nidx(:, :) - tol)

          pstack(prop_psg_x1:prop_psg_x4, jpos) = idpos(1, 1:4)
          pstack(prop_psg_y1:prop_psg_y4, jpos) = idpos(2, 1:4)
          ! recursion
          do
             if (jpos.lt.0) exit
             idmin(1) = minval(pstack(prop_psg_x1:prop_psg_x4, jpos))
             idmax(1) = maxval(pstack(prop_psg_x1:prop_psg_x4, jpos))
             idmin(2) = minval(pstack(prop_psg_y1:prop_psg_y4, jpos))
             idmax(2) = maxval(pstack(prop_psg_y1:prop_psg_y4, jpos))
             klev = pstack(prop_gtlev, jpos)
             ! out of destination domain
             if (idmax(1).le.0 .or. idspan(1).le.idmin(1) &
                  & .or. idmax(2).le.0 .or. idspan(2).le.idmin(2)) then
                if (udump.ge.0) then
                   cstep = pcache(2, klev)
                   jcx = pstack(prop_gtx, jpos) * cstep
                   jcy = pstack(prop_gty, jpos) * cstep
                   npos(:, 1) = proj_pstereog(cachelo(:, jcx),         cachela(:, jcy))
                   npos(:, 2) = proj_pstereog(cachelo(:, jcx + cstep), cachela(:, jcy))
                   npos(:, 3) = proj_pstereog(cachelo(:, jcx + cstep), cachela(:, jcy + cstep))
                   npos(:, 4) = proj_pstereog(cachelo(:, jcx),         cachela(:, jcy + cstep))
                   npos(1, 1:4) = npos(1, 1:4) - destl(1)
                   npos(2, 1:4) = npos(2, 1:4) - destl(2)
                   write(udump) real((/jdext, klev/), kind=KDBG), real(npos(:,1:4), kind=KDBG)
                endif

                ntiles(jdext) = ntiles(jdext) + acache(klev)
                levco(klev) = levco(klev) + 1
                jpos = jpos - 1
                cycle
             endif
             bsame(1:2) = (idmin(1:2)+1.ge.idmax(1:2)) &
                  & .or.  (idmin(1:2)+2.eq.idmax(1:2) .and. mod(idmin(1:2), 2).eq.0)
             ! all nodes in the same destination
             if (bsame(1).and.bsame(2)) then
                ! negative floor already excluded
                jdx = idmin(1) / 2
                jdy = idmin(2) / 2
                jdmax(:) = max(jdmax(:), (/jdx, jdy/))
                jdmin(:) = min(jdmin(:), (/jdx, jdy/))
                jdh = jdy * mx + jdx

                if (udump.ge.0) then
                   cstep = pcache(2, klev)
                   jcx = pstack(prop_gtx, jpos) * cstep
                   jcy = pstack(prop_gty, jpos) * cstep
                   npos(:, 1) = proj_pstereog(cachelo(:, jcx),         cachela(:, jcy))
                   npos(:, 2) = proj_pstereog(cachelo(:, jcx + cstep), cachela(:, jcy))
                   npos(:, 3) = proj_pstereog(cachelo(:, jcx + cstep), cachela(:, jcy + cstep))
                   npos(:, 4) = proj_pstereog(cachelo(:, jcx),         cachela(:, jcy + cstep))
                   npos(1, 1:4) = npos(1, 1:4) - destl(1)
                   npos(2, 1:4) = npos(2, 1:4) - destl(2)
                   write(udump) real((/jdh, klev/), kind=KDBG), real(npos(:,1:4), kind=KDBG)
                endif

                ntiles(jdh) = ntiles(jdh) + acache(klev)
                levco(klev) = levco(klev) + 1
                jpos = jpos - 1
                cycle
             endif
             ! before limit division
             if (klev.lt.fulllev) then
                idpos(1, 1:4) = pstack(prop_psg_x1:prop_psg_x4, jpos)
                idpos(2, 1:4) = pstack(prop_psg_y1:prop_psg_y4, jpos)

                jtx = pstack(prop_gtx, jpos) * 2
                jty = pstack(prop_gty, jpos) * 2
                pstack(prop_gtx,   jpos:jpos+3) = (/jtx+1, jtx,   jtx+1, jtx/)
                pstack(prop_gty,   jpos:jpos+3) = (/jty+1, jty+1, jty,   jty/)
                pstack(prop_gtlev, jpos:jpos+3) = klev + 1

                cstep = pcache(2, klev + 1)
                jcx = (jtx + 1) * cstep
                jcy = (jty + 1) * cstep

                xpos(:,1) = proj_pstereog(cachelo(:, jcx),       cachela(:, jcy-cstep))
                xpos(:,2) = proj_pstereog(cachelo(:, jcx-cstep), cachela(:, jcy))
                xpos(:,3) = proj_pstereog(cachelo(:, jcx),       cachela(:, jcy))
                xpos(:,4) = proj_pstereog(cachelo(:, jcx+cstep), cachela(:, jcy))
                xpos(:,5) = proj_pstereog(cachelo(:, jcx),       cachela(:, jcy+cstep))

                xidx(1, 1:5) = (xpos(1, 1:5) - destl(1))
                xidx(2, 1:5) = (xpos(2, 1:5) - destl(2))
                ! do js = 1, 5
                !    if (abs(xpos(1,js)).le.1.0d-3) then
                !       write(*, *) 'close', jcx, jcy, cstep, js, xpos(:,js), xidx(:, js)-anint(xidx(:, js))
                !    endif
                ! enddo
                ixpos(:, :) = floor(xidx(:, :) + tol) + ceiling(xidx(:, :) - tol)

                pstack(prop_psg_x1:prop_psg_x4, jpos+3) = (/idpos(1,1), ixpos(1,1), ixpos(1,2), ixpos(1,3)/)
                pstack(prop_psg_y1:prop_psg_y4, jpos+3) = (/idpos(2,1), ixpos(2,1), ixpos(2,2), ixpos(2,3)/)
                pstack(prop_psg_x1:prop_psg_x4, jpos+2) = (/ixpos(1,1), idpos(1,2), ixpos(1,3), ixpos(1,4)/)
                pstack(prop_psg_y1:prop_psg_y4, jpos+2) = (/ixpos(2,1), idpos(2,2), ixpos(2,3), ixpos(2,4)/)
                pstack(prop_psg_x1:prop_psg_x4, jpos+1) = (/ixpos(1,2), ixpos(1,3), idpos(1,3), ixpos(1,5)/)
                pstack(prop_psg_y1:prop_psg_y4, jpos+1) = (/ixpos(2,2), ixpos(2,3), idpos(2,3), ixpos(2,5)/)
                pstack(prop_psg_x1:prop_psg_x4, jpos+0) = (/ixpos(1,3), ixpos(1,4), ixpos(1,5), idpos(1,4)/)
                pstack(prop_psg_y1:prop_psg_y4, jpos+0) = (/ixpos(2,3), ixpos(2,4), ixpos(2,5), idpos(2,4)/)

                jpos = jpos + 3
                cycle
             endif
             ! limit division
             levco(klev) = levco(klev) + 1
             klev = klev + 1
             idpos(1, 1:4) = pstack(prop_psg_x1:prop_psg_x4, jpos)
             idpos(2, 1:4) = pstack(prop_psg_y1:prop_psg_y4, jpos)
             cstep = cstep / 2
             do js = 1, 4
                iapos(1:2) = idpos(1:2, js)
                xeven = mod(iapos(1), 2).eq.0
                yeven = mod(iapos(2), 2).eq.0
                if (xeven.and.yeven) then
                   ! if node is on the intersects
                   apos(:) = proj_pstereog(cachelo(:, jcx + cstep), cachela(:, jcy + cstep))
                   aidx(1:2) = (apos(1:2) - destl(1:2))
                   iapos(:) = floor(aidx(:)) + ceiling(aidx(:))
                   xeven = mod(iapos(1), 2).eq.0
                   yeven = mod(iapos(2), 2).eq.0
                endif
                ! It is possible to have xeven and yeven both true,
                ! however, it should be rare for most practical cases.
                jdx = (iapos(1) + 2) / 2 - 1
                jdy = (iapos(2) + 2) / 2 - 1
                if (xeven) then
                   if (inrange(jdx, 0, mx - 1).and.inrange(jdy, 0, my - 1)) then
                      jdh = jdy * mx + jdx
                      ptiles(jdh) = ptiles(jdh) + acache(klev+1)
                      jdmax(:) = max(jdmax(:), (/jdx, jdy/))
                      jdmin(:) = min(jdmin(:), (/jdx, jdy/))
                   else
                      ptiles(jdext) = ptiles(jdext) + acache(klev+1)
                   endif
                   jdx = jdx - 1
                   if (inrange(jdx, 0, mx - 1).and.inrange(jdy, 0, my - 1)) then
                      jdh = jdy * mx + jdx
                      ptiles(jdh) = ptiles(jdh) + acache(klev+1)
                      jdmax(:) = max(jdmax(:), (/jdx, jdy/))
                      jdmin(:) = min(jdmin(:), (/jdx, jdy/))
                   else
                      ptiles(jdext) = ptiles(jdext) + acache(klev+1)
                   endif
                else if (yeven) then
                   if (inrange(jdx, 0, mx - 1).and.inrange(jdy, 0, my - 1)) then
                      jdh = jdy * mx + jdx
                      ptiles(jdh) = ptiles(jdh) + acache(klev+1)
                      jdmax(:) = max(jdmax(:), (/jdx, jdy/))
                      jdmin(:) = min(jdmin(:), (/jdx, jdy/))
                   else
                      ptiles(jdext) = ptiles(jdext) + acache(klev+1)
                   endif
                   jdy = jdy - 1
                   if (inrange(jdx, 0, mx - 1).and.inrange(jdy, 0, my - 1)) then
                      jdh = jdy * mx + jdx
                      ptiles(jdh) = ptiles(jdh) + acache(klev+1)
                      jdmax(:) = max(jdmax(:), (/jdx, jdy/))
                      jdmin(:) = min(jdmin(:), (/jdx, jdy/))
                   else
                      ptiles(jdext) = ptiles(jdext) + acache(klev+1)
                   endif
                else
                   if (inrange(jdx, 0, mx - 1).and.inrange(jdy, 0, my - 1)) then
                      jdh = jdy * mx + jdx
                      ptiles(jdh) = ptiles(jdh) + acache(klev)
                      jdmax(:) = max(jdmax(:), (/jdx, jdy/))
                      jdmin(:) = min(jdmin(:), (/jdx, jdy/))
                   else
                      ptiles(jdext) = ptiles(jdext) + acache(klev)
                   endif
                endif
             enddo
             jpos = jpos - 1
          enddo
       enddo
    enddo
  end subroutine div_g2ps_cell_d

!!!_  - divide_gg_cache_lat
  subroutine divide_gg_cache_lat_d &
       & (cache, cco, slat0, slat1, dslat, lcache)
    use TOUZA_Emu_ugg,only: proj_pstereog_cachela
    implicit none
    integer,parameter :: KTGT=KDBL
    real(kind=KTGT),intent(out) :: cache(:, 0:)
    real(kind=KTGT),intent(in)  :: cco(*)
    real(kind=KTGT),intent(in)  :: slat0,  slat1, dslat
    integer,        intent(in)  :: lcache

    real(kind=KTGT) :: uslat
    real(kind=KTGT) :: slat, lat

    integer jj, jc

    uslat = (dslat / real(lcache, kind=KTGT))
    do jj = 0, lcache / 2 - 1
       jc = jj
       slat = slat0 + uslat * real(jj, kind=KTGT)
       lat  = asin(slat)
       call proj_pstereog_cachela(cache(:, jc), lat, cco)
       ! write(*, *) 'cache/la/1', jc, cache(:, jc)
    enddo
    do jj = 0, lcache / 2 - 1
       jc = lcache - jj
       slat = slat1 - uslat * real(jj, kind=KTGT)
       lat  = asin(slat)
       call proj_pstereog_cachela(cache(:, jc), lat, cco)
    enddo

    jc = lcache / 2
    slat = (slat1 + slat0) * 0.5_KTGT
    lat  = asin(slat)
    call proj_pstereog_cachela(cache(:, jc), lat, cco)
  end subroutine divide_gg_cache_lat_d

!!!_  - divide_gg_cache_lon
  subroutine divide_gg_cache_lon_d &
       & (cache, cco, lon0, lon1, dlon, lonofs, lcache)
    use TOUZA_Emu_ugg,only: proj_pstereog_cachelo, deg2rad
    implicit none
    integer,parameter :: KTGT=KDBL
    real(kind=KTGT),intent(out) :: cache(:, 0:)
    real(kind=KTGT),intent(in)  :: cco(*)
    real(kind=KTGT),intent(in)  :: lon0,   lon1
    real(kind=KTGT),intent(in)  :: dlon,   lonofs
    integer,        intent(in)  :: lcache
    real(kind=KTGT) :: ulon
    real(kind=KTGT) :: lon, l0, l1

    integer jj, jc

    l0 = lon0 + lonofs
    l1 = lon1 + lonofs

    if (dlon.eq.0.0_KTGT) then
       ulon = (lon1 - lon0) / real(lcache, kind=KTGT)
       do jj = 0, lcache / 2 - 1
          jc = jj
          lon = (l0 + ulon * real(jj, kind=KTGT))
          call proj_pstereog_cachelo(cache(:, jc), lon, cco)
          ! write(*, *) 'cache/lo/1', jc, cache(:, jc)
       enddo
       do jj = 0, lcache / 2 - 1
          jc = lcache - jj
          lon = (l1 - ulon * real(jj, kind=KTGT))
          call proj_pstereog_cachelo(cache(:, jc), lon, cco)
       enddo
       jc = lcache / 2
       lon = (l0 + l1) * 0.5_KTGT
       call proj_pstereog_cachelo(cache(:, jc), lon, cco)
    else
       ulon = dlon / real(lcache, kind=KTGT)
       do jj = 0, lcache / 2 - 1
          jc = jj
          lon = l0 + ulon * real(jj, kind=KTGT)
          call proj_pstereog_cachelo(cache(:, jc), lon, cco)
          ! write(*, *) 'cache/lo/1', jc, cache(:, jc), lon0, lonofs
       enddo
       do jj = 0, lcache / 2 - 1
          jc = lcache - jj
          lon = l1 - ulon * real(jj, kind=KTGT)
          call proj_pstereog_cachelo(cache(:, jc), lon, cco)
       enddo
       jc = lcache / 2
       lon = (l0 + l1) * 0.5_KTGT
       call proj_pstereog_cachelo(cache(:, jc), lon, cco)
    endif
  end subroutine divide_gg_cache_lon_d

!!!_  & nsort4 - sort 4-element array by /network/ method
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
!!!_  - expand_g2ps_proj - expand result on projection (cartesian) domain [plain array]
  subroutine expand_g2ps_proj_d &
       & (ierr,   itab,   wtab, nnum, lt,   lnum, &
       &  iofs,   iprj,   wprj, mglb, &
       &  iundef, wundef, safe)
    use TOUZA_Ami_std,only: choice
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,         intent(out) :: ierr
    integer,         intent(out) :: itab(0:*)   ! [lt, lnum]
    real(kind=KTGT), intent(out) :: wtab(0:*)   ! [lt, lnum]
    integer,         intent(out) :: nnum
    integer,         intent(in)  :: lt, lnum
    integer,         intent(in)  :: iofs(0:*)   ! [lglb]
    integer,         intent(in)  :: iprj(0:*)
    real(kind=KTGT), intent(in)  :: wprj(0:*)
    integer,         intent(in)  :: mglb
    integer,         intent(in)  :: iundef
    real(kind=KTGT), intent(in)  :: wundef
    logical,optional,intent(in)  :: safe
    integer jg, jv, jp, jb, ne

    ierr = 0
    itab(0:lt-1) = 0
    do jv = 0, iofs(mglb) - 1
       jp = iprj(jv)
       ! write(*, *) jv, jp
       if (jp.ge.0.and.jp.lt.lt) itab(jp) = itab(jp) + 1
    enddo
    nnum = maxval(itab(0:lt-1))
    if (lnum.gt.0.and.nnum.gt.lnum) then
       ierr = ERR_INSUFFICIENT_BUFFER
       return
    endif
    ! write(*,*) itab(0:lt-1)
    if (choice(.TRUE., safe)) then
       if (ANY(itab(0:lt-1).eq.0)) then
          ierr = ERR_PANIC
          return
       endif
    endif
    ne = lt * nnum
    do jp = 0, lt - 1
       jb = itab(jp) * lt + jp
       itab(jb:ne-1:lt) = iundef
       wtab(jb:ne-1:lt) = wundef
    enddo
    jb = lt * (nnum - 1)
    do jg = 0, mglb - 1
       do jv = iofs(jg), iofs(jg+1) - 1
          jp = iprj(jv)
          if (jp.ge.0.and.jp.lt.lt) then
             itab(jp) = itab(jp) - 1
             jb = itab(jp) * lt + jp
             itab(jb) = jg
             wtab(jb) = wprj(jv)
          endif
       enddo
    enddo
  end subroutine expand_g2ps_proj_d
!!!_  - pack_g2ps_sphe - expand result on global sphere domain [packed array]
  subroutine pack_g2ps_sphe_d &
       & (ierr,   ne,    iends, issub, ipsub, w, le, &
       &  iofs,   iprj,  wprj,  mglb,  &
       &  iundef, wundef)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ne          ! iends,issub sizes
    integer,         intent(out) :: iends(0:*)  ! [e] CRS
    integer,         intent(out) :: issub(0:*)  ! [e] index(spherical domain)
    integer,         intent(out) :: ipsub(0:*)  ! [v] index(projected domain)
    real(kind=KTGT), intent(out) :: w(0:*)      ! [v] weight
    integer,         intent(in)  :: le          ! limit size of iends
    integer,         intent(in)  :: iofs(0:*)   ! [lglb]
    integer,         intent(in)  :: iprj(0:*)
    real(kind=KTGT), intent(in)  :: wprj(0:*)
    integer,         intent(in)  :: mglb
    integer,         intent(in)  :: iundef
    real(kind=KTGT), intent(in)  :: wundef

    integer m
    integer jg

    ierr = 0
    ne = 0
    do jg = 0, mglb - 1
       m = iofs(jg+1) - iofs(jg)
       ne = max(ne, m)
       write(*, *) 'members: ', jg, m
    enddo
    if (ne.gt.le) then
       ierr = ERR_INSUFFICIENT_BUFFER
       return
    endif
    return
  end subroutine pack_g2ps_sphe_d
!!!_ + end AMI_TABLE
end module TOUZA_AMI_TABLE
!!!_@ test_ami_table - test program
#if TEST_AMI_TABLE
program test_ami_table
  use TOUZA_Ami_table
  use TOUZA_Ami_std
  use TOUZA_Nio,nio_init=>init, nio_diag=>diag, nio_finalize=>finalize
  use TOUZA_Std,only: arg_init, arg_diag, parse, get_option, decl_pos_arg
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
     else
        call nio_init(ierr)
        if (ierr.eq.0) call batch_test_gs(ierr)
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
    real(kind=KDBL)   :: ftbl(lx, ltbl)
    integer           :: itbl(lx, ltbl)

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

  subroutine batch_test_gs (ierr)
    use TOUZA_Emu_ugg,only: gauss_latitude, mid_latitude
    use TOUZA_Emu_ugg,only: get_longitude,  mid_longitude
    use TOUZA_Emu_ugg,only: flatten_to_ecc, rad2deg, deg2rad
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr

    integer nlon, nlat
    integer lonb, lone, latb, late, mlon, mlat
    real(kind=KTGT),allocatable :: glat(:), wlat(:), glatm(:)
    real(kind=KTGT),allocatable :: glon(:), wlon(:), glonm(:)
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE =  1.0_KTGT
    real(kind=KTGT),parameter :: HALF = 0.5_KTGT
    real(kind=KTGT) :: span = ZERO
    real(kind=KTGT) :: wnml = ONE
    real(kind=KTGT) :: llorg(2), latts
    real(kind=KTGT) :: llorg_r(2), latts_r
    real(kind=KTGT),parameter :: def = -HUGE(ZERO)
    real(kind=KTGT) :: e, a, rf, r
    real(kind=KTGT) :: xl, xh, dx
    real(kind=KTGT) :: yl, yh, dy
    real(kind=KTGT) :: xx(5), yy(5)

    real(kind=KTGT) :: dlat0, dlat1, edlat, tol
    integer rlon(3), rlat(3)

    integer div(3)
    integer mxc, myc
    integer levv

    integer nmem, lmem, ngg
    real(kind=KTGT),allocatable :: wprj(:)
    integer,        allocatable :: iprj(:), iofs(:), iofsb(:)

    integer jx, jy
    integer jlo, jla, jp, jg
    character(len=1024) :: path, dump
    integer udump
    logical safe

    ierr = 0
    levv = 9
    dump = ' '

    if (ierr.eq.0) call get_option(ierr, rlon(:),    'lon', -1)
    if (ierr.eq.0) call get_option(ierr, rlat(:),    'lat', -1)
    if (ierr.eq.0) call get_option(ierr, llorg(1:2), 'org', def)
    if (ierr.eq.0) call get_option(ierr, latts,      'ts',  +70.0_KTGT)
    if (ierr.eq.0) call get_option(ierr, a, 'a', 6378137.0_KTGT)
    if (ierr.eq.0) call get_option(ierr, r, 'r', 6370000.0_KTGT)
    if (ierr.eq.0) call get_option(ierr, e, 'e', def)
    if (ierr.eq.0) call get_option(ierr, rf, 'rf', def)
    if (ierr.eq.0) call get_option(ierr, xx(1:5), 'x', def)   ! node boundaries, delta, and region extensions
    if (ierr.eq.0) call get_option(ierr, yy(1:5), 'y', def)
    if (ierr.eq.0) call get_option(ierr, div(:),  'div',  0)
    if (ierr.eq.0) call get_option(ierr, lmem, 'l', 0)
    if (ierr.eq.0) call get_option(ierr, path, 'o', ' ')
    if (ierr.eq.0) call get_option(ierr, dump, 'dump', ' ')
    if (ierr.eq.0) call get_option(ierr, tol,  'tol', 0.0_KTGT)

    if (ierr.eq.0) then
       if (rlon(1).lt.0) rlon(1) = 128
       if (rlat(1).lt.0) rlat(1) = rlon(1) / 2
       if (rlon(2).lt.0) rlon(2) = 0
       if (rlon(3).lt.0) rlon(3) = rlon(1) + rlon(2)
       if (rlat(2).lt.0) rlat(2) = 0
       if (rlat(3).lt.0) rlat(3) = rlat(1) + rlat(2)
       rlat(3) = max(rlat(2) + 1, rlat(3))
       rlon(3) = max(rlon(2) + 1, rlon(3))
       ! if (nlon.lt.0) nlon = 128
       ! if (nlat.lt.0) nlat = nlon / 2
       nlon = rlon(1)
       nlat = rlat(1)
       lonb = rlon(2)
       lone = rlon(3)
       latb = rlat(2)
       late = rlat(3)
       mlon = lone - lonb
       mlat = late - latb
       write(*, *) 'lon range: ', nlon, mlon, lonb, lone
       write(*, *) 'lat range: ', nlat, mlat, latb, late
    endif
    if (ierr.eq.0) then
       if (e.eq.def) then
          if (rf.eq.def) rf = 298.257223563_KTGT
          e = flatten_to_ecc(ONE / rf)
       endif
    endif
    if (ierr.eq.0) then
       if (xx(1).eq.def) xx(1) = -641150.0_KTGT - 1500.0_KTGT
       if (xx(2).eq.def) xx(2) = +867850.0_KTGT + 1500.0_KTGT
       if (xx(3).eq.def) xx(3) = 6000.0_KTGT
       if (yy(1).eq.def) yy(1) = -3375050.0_KTGT - 1500.0_KTGT
       if (yy(2).eq.def) yy(2) = -642050.0_KTGT  + 1500.0_KTGT
       if (yy(3).eq.def) yy(3) = 6000.0_KTGT

       if (xx(4).ne.def) xx(1) = xx(1) - xx(4) * xx(3)  ! region extension
       if (xx(5).ne.def) xx(2) = xx(2) + xx(5) * xx(3)
       if (yy(4).ne.def) yy(1) = yy(1) - yy(4) * yy(3)
       if (yy(5).ne.def) yy(2) = yy(2) + yy(5) * yy(3)

       if (div(1).le.0) div(1) = 10
       if (div(2).le.0) div(2) = div(1)
       div(3) = max(0, div(3))
    endif
    if (ierr.eq.0) then
       if (llorg(1).eq.def) llorg(1) = 360.0_KTGT -45.0_KTGT
       if (llorg(2).eq.def) llorg(2) = +90.0_KTGT
       llorg_r(1:2) = deg2rad(llorg(1:2))
       latts_r = deg2rad(latts)
    endif
    if (ierr.eq.0) then
       if (lmem.le.0) lmem = 32
       ngg = nlat * nlon
       lmem = lmem * ngg
       allocate (glat(0:nlat-1), wlat(0:nlat-1), glatm(0:nlat), &
            &    glon(0:nlon-1), wlon(0:nlon-1), glonm(0:nlon), &
            &    iofs(0:ngg),    iprj(0:lmem-1), wprj(0:lmem-1), &
            &    STAT=ierr)
    endif

    if (ierr.eq.0) call gauss_latitude(ierr, glat, wlat, nlat, span, wnml, prec=-1.0_KTGT)
    if (ierr.eq.0) glat(0:nlat-1) = asin(glat(0:nlat-1))
    if (ierr.eq.0) call mid_latitude(ierr, glatm, wlat, nlat)

    if (ierr.eq.0) call get_longitude(ierr, glon,  wlon, nlon, span=span, wnml=wnml)
    if (ierr.eq.0) call mid_longitude(ierr, glonm, glon, wlon, nlon, span / wnml)

    ! if (ierr.eq.0) glat(:) = glat(nlat-1:0:-1)
    ! if (ierr.eq.0) glatm(:) = glatm(nlat:0:-1)

    if (ierr.eq.0) then
202    format('node-lat:', I0, 1x, F9.3)
       do jy = 0, nlat
          write(*, 202) jy, rad2deg(glatm(jy))
       enddo
203    format('node-dlat:', I0, 1x, F9.6, 1x, F9.6, 1x, E9.2)
       do jy = 0, nlat - 1
          dlat0 = wlat(jy) * 2.0_KTGT
          dlat1 = abs(sin(glatm(jy+1)) - sin(glatm(jy)))
          edlat = (dlat0 - dlat1) / dlat0
          write(*, 203) jy, dlat0, dlat1, edlat
       enddo
201    format('node-lon:', I0, 1x, F9.3)
       do jx = 0, nlon
          write(*, 201) jx, rad2deg(glonm(jx))
       enddo
    endif
    if (ierr.eq.0) then
       xl = xx(1)
       xh = xx(2)
       dx = xx(3)
       yl = yy(1)
       yh = yy(2)
       dy = yy(3)
       mxc = int((xh - xl) / dx)
       myc = int((yh - yl) / dy)
       if (real(mxc, kind=KTGT) * dx .ne. (xh - xl)) ierr = _ERROR(ERR_INVALID_PARAMETER)
       if (real(myc, kind=KTGT) * dy .ne. (yh - yl)) ierr = _ERROR(ERR_INVALID_PARAMETER)
    endif
    udump = -1
    if (ierr.eq.0) then
213    format('domain:', A, ': ', I0, 1x, F9.1, 1x, 2F12.1)
       write(*, 213) 'x', mxc, dx, xl, xh
       write(*, 213) 'y', myc, dy, yl, yh
       if (dump.ne.' ') then
          udump = new_unit()
          ierr = min(0, udump)
          if (ierr.eq.0) then
             call sus_open(ierr, udump, dump, ACTION='W', STATUS='R')
          endif
       endif
    endif

    if (ierr.eq.0) then
       ! call div_g2ps_map &
       !      & (ierr,       iofs,       iprj,    wprj, nmem, lmem, &
       !      &  glonm,      glatm,      wlat,    nlon, nlat, &
       !      &  llorg_r(1), llorg_r(2), latts_r, a,    e,    &
       !      &  xl,         dx,         mxc,     yl,   dy,   myc,  &
       !      &  div(1),     div(2),     div(3),  tol,  levv=levv,  udump=udump)
       call div_g2ps_map &
            & (ierr,       iofs,       iprj,    wprj, nmem, lmem, &
            &  glonm(lonb:lone),       glatm(latb:late),    wlat(latb:late-1), mlon, mlat, &
            &  llorg_r(1), llorg_r(2), latts_r, a,    e,    &
            &  xl,         dx,         mxc,     yl,   dy,   myc,  &
            &  div(1),     div(2),     div(3),  tol,  levv=levv,  udump=udump)
       if (nmem.gt.lmem) then
          write(*, *) 'retry = ', nmem, lmem
          lmem = nmem
          deallocate(iprj, wprj, STAT=ierr)
          if (ierr.eq.0) allocate(iprj(0:lmem-1), wprj(0:lmem-1), STAT=ierr)
          if (ierr.eq.0) then
             if (udump.ge.0) rewind(udump, IOSTAT=ierr)
          endif
          if (ierr.eq.0) then
             ! call div_g2ps_map &
             !      & (ierr,       iofs,       iprj,    wprj, nmem, lmem, &
             !      &  glonm,      glatm,      wlat,    nlon, nlat, &
             !      &  llorg_r(1), llorg_r(2), latts_r, a,    e,    &
             !      &  xl,         dx,         mxc,     yl,   dy,   myc,  &
             !      &  div(1),     div(2),     div(3),  tol,  levv=levv)
             call div_g2ps_map &
                  & (ierr,       iofs,       iprj,    wprj, nmem, lmem, &
                  &  glonm(lonb:lone),       glatm(latb:late),    wlat(latb:late-1), mlon, mlat, &
                  &  llorg_r(1), llorg_r(2), latts_r, a,    e,    &
                  &  xl,         dx,         mxc,     yl,   dy,   myc,  &
                  &  div(1),     div(2),     div(3),  tol,  levv=levv,  udump=udump)
          endif
       endif
    endif
    if (ierr.eq.0) then
       safe = (mlon.eq.nlon) .and. (mlat.eq.nlat)
       if (.not.safe) then
          allocate(iofsb(0:ngg),STAT=ierr)
          if (ierr.eq.0) then
             iofsb(0:ngg) = iofs(0:ngg)
             jp = 1
             iofs(0) = 0
             jg = 1
             do jla = 0, nlat - 1
                do jlo = 0, nlon - 1
                   if (inrange(jlo, lonb, lone-1).and.inrange(jla, latb, late-1)) then
                      iofs(jg) = iofs(jg-1) + (iofsb(jp) - iofsb(jp-1))
                      jp = jp + 1
                   else
                      iofs(jg) = iofs(jg-1)
                   endif
                   jg = jg + 1
                enddo
             enddo
          endif
       endif
       call output_stereo &
            & (ierr, iofs, iprj, wprj, nmem, ngg, &
            &  xl,   dx,   mxc,  yl,   dy,   myc, &
            &  r,    path, safe)
    endif
  end subroutine batch_test_gs

  subroutine output_stereo &
       & (ierr,   iofs,   iprj,   wprj,  nmem,  ngg,    &
       &  xl,     dx,     mx,     yl,    dy,    my,     &
       &  r,      path,   safe)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: iofs(0:*)
    integer,         intent(in)  :: iprj(0:*)
    real(kind=KTGT), intent(in)  :: wprj(0:*)
    integer,         intent(in)  :: nmem, ngg
    real(kind=KTGT), intent(in)  :: xl, dx
    real(kind=KTGT), intent(in)  :: yl, dy
    integer,         intent(in)  :: mx, my
    real(kind=KTGT), intent(in)  :: r
    character(len=*),intent(in)  :: path
    logical,         intent(in)  :: safe
    integer u
    integer,        allocatable :: ngrd(:)
    real(kind=KTGT),allocatable :: wsum(:)

    integer,        allocatable :: itab(:)
    real(kind=KTGT),allocatable :: wtab(:)

    integer,        allocatable :: ipsub(:), issub(:), iends(:)
    real(kind=KTGT),allocatable :: wp2s(:)
    integer :: ne, le

    real(kind=KTGT) :: x, y, r2
    integer jx, jy, jh, mh, jz, jw
    integer lw
    integer jmem
    integer lnum, nnum
    integer         :: iundef = -999
    real(kind=KTGT) :: wundef = -999.0_KTGT

    integer kfmt
    integer krect
    character(len=litem) :: hd(nitem)

    ierr = 0
    mh = mx * my
    r2 = r * r
    if (ierr.eq.0) allocate(wsum(0:mh-1), ngrd(0:mh-1), STAT=ierr)
    if (ierr.eq.0) then
       wsum(:) = 0.0_KTGT
       ngrd(:) = 0
       do jmem = 0, nmem - 1
          jh = iprj(jmem)
          if (jh.ge.0.and.jh.lt.mh) then
             wsum(jh) = wsum(jh) + wprj(jmem)
             ngrd(jh) = ngrd(jh) + 1
          endif
       enddo
    endif
    lnum = 4
    if (ierr.eq.0) allocate(itab(0:mh*lnum-1), wtab(0:mh*lnum-1), STAT=ierr)
    if (ierr.eq.0) then
       call expand_g2ps_proj &
            & (ierr,   itab,   wtab, nnum, mh, lnum, &
            &  iofs,   iprj,   wprj, ngg,  &
            &  iundef, wundef, safe)
       if (ierr.ne.0) write(*, *) 'failed in expand_g2ps_proj: ', ierr
    endif
    if (ierr.eq.0) then
       le = -1
       call pack_g2ps_sphe &
            & (ierr,   ne,    iends, issub, ipsub, wp2s, le, &
            &  iofs,   iprj,  wprj,  ngg,   &
            &  iundef, wundef)
       write(*, *) 'pack = ', ne
       ierr = 0
    endif

    if (path.eq.' ') then
       if (ierr.eq.0) then
101       format('wsum:1: ', F10.1, 1x, F10.1, 1x, E16.9)
102       format('ngrd:1: ', F10.1, 1x, F10.1, 1x, I0)
103       format('ptab:1: ', F10.1, 1x, F10.1, 1x, I0, 2x, I0, 1x, E16.9)
          do jh = 0, mh - 1
             jy = jh / mx
             jx = mod(jh, mx)
             x = xl + dx * (real(jx, kind=KTGT) + 0.5_KTGT)
             y = yl + dy * (real(jy, kind=KTGT) + 0.5_KTGT)
             write(*, 101) x, y, wsum(jh) * r2
          enddo
          do jh = 0, mh - 1
             jy = jh / mx
             jx = mod(jh, mx)
             x = xl + dx * (real(jx, kind=KTGT) + 0.5_KTGT)
             y = yl + dy * (real(jy, kind=KTGT) + 0.5_KTGT)
             write(*, 102) x, y, ngrd(jh)
          enddo
          do jz = 0, nnum - 1
             do jh = 0, mh - 1
                jy = jh / mx
                jx = mod(jh, mx)
                jw = jz * mh + jh
                x = xl + dx * (real(jx, kind=KTGT) + 0.5_KTGT)
                y = yl + dy * (real(jy, kind=KTGT) + 0.5_KTGT)
                write(*, 103) x, y, jz, itab(jw), wtab(jw)
             enddo
          enddo
       endif
    else
       krect = REC_BIG

       if (ierr.eq.0) call sus_open(ierr, u, path, ACTION='W', STATUS='R')
       if (ierr.eq.0) call get_default_header(hd)
       if (ierr.eq.0) call put_header_cprop(ierr, hd, 'x', (/1, mx/), 1)
       if (ierr.eq.0) call put_header_cprop(ierr, hd, 'y', (/1, my/), 2)

       if (ierr.eq.0) call put_item(ierr, hd, 'MI4',  hi_DFMT)
       if (ierr.eq.0) call put_item(ierr, hd, 'iprj', hi_ITEM)
       if (ierr.eq.0) call put_header_cprop(ierr, hd, 'n', (/1, nnum/), 3)

       lw = mh * nnum
       if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
       if (ierr.eq.0) call nio_write_data(ierr, itab, lw, hd, krect, u)

       if (ierr.eq.0) call put_item(ierr, hd, 'MR8',  hi_DFMT)
       if (ierr.eq.0) call put_item(ierr, hd, 'wprj', hi_ITEM)
       where (wtab(0:lw-1).ne.wundef)
          wtab(0:lw-1) = wtab(0:lw-1) * r2
       end where
       if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
       if (ierr.eq.0) call nio_write_data(ierr, wtab, lw, hd, krect, u)

       if (ierr.eq.0) call put_header_cprop(ierr, hd, ' ', (/1, 1/), 3)
       if (ierr.eq.0) call put_item(ierr, hd, 'UR8',  hi_DFMT)
       if (ierr.eq.0) call put_item(ierr, hd, 'wsum', hi_ITEM)
       wsum(0:mh-1) = wsum(0:mh-1) * r2
       if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
       if (ierr.eq.0) call nio_write_data(ierr, wsum, mh, hd, krect, u)

       if (ierr.eq.0) call put_item(ierr, hd, 'UI4',  hi_DFMT)
       if (ierr.eq.0) call put_item(ierr, hd, 'nprj', hi_ITEM)
       if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
       if (ierr.eq.0) call nio_write_data(ierr, ngrd, mh, hd, krect, u)
    endif

    if (ierr.eq.0) deallocate(wsum, itab, wtab, STAT=ierr)
  end subroutine output_stereo

end program test_ami_table
#endif /* TEST_AMI_TABLE */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

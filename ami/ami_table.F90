!!!_! ami_table.F90. - TOUZA/Ami/table amida-coupler table procedures
! Maintainer: SAITO Fuyuki
! Created: May 2 2022
#define TIME_STAMP 'Time-stamp: <2023/04/16 23:02:58 fuyuki ami_table.F90>'
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

#define __MDL__ 't'
#define _ERROR(E) (E - ERR_MASK_AMI_TABLE)
!!!_ + type
!!!_ + interfaces
  interface check_monotonic
     module procedure check_monotonic_d
  end interface check_monotonic
  interface gen_gg_stereo_map
     module procedure gen_gg_stereo_map_d
  end interface gen_gg_stereo_map
!!!_ + public procedures
  public init, diag, finalize
  public cyclic_interp_table
  public geo_interp_table
  public gen_gg_stereo_map
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
!!!_  - gen_gg_stereo_map_d
  subroutine gen_gg_stereo_map_d &
       & (ierr,   iofs,   isrc,   wsrc,  nmem,  lmem,  &
       &  lonn,   latn,   mlon,   mlat,  r,     &
       &  lonorg, latorg, latts,  major, ecc,   &
       &  xl,     dx,     mx,     yl,    dy,    my,     &
       &  div,    levv,   u)
    use TOUZA_Emu_ugg,only: rad2deg, deg2rad
    use TOUZA_Emu_ugg,only: ncache_stereog_co, ncache_stereog_la, ncache_stereog_lo
    use TOUZA_Emu_ugg,only: proj_pstereog, proj_pstereog_set
    use TOUZA_Emu_ugg,only: proj_pstereog_cachela, proj_pstereog_cachelo
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,         intent(out) :: ierr
    integer,         intent(out) :: iofs(0:*)
    integer,         intent(out) :: isrc(0:*)
    real(kind=KTGT), intent(out) :: wsrc(0:*)
    integer,         intent(out) :: nmem
    integer,         intent(in)  :: lmem
    real(kind=KTGT), intent(in)  :: lonn(0:*), latn(0:*)   ! (radian) node longitude [0:mlon] and latitude [0:mlat]
    integer,         intent(in)  :: mlon,      mlat
    real(kind=KTGT), intent(in)  :: r
    real(kind=KTGT), intent(in)  :: lonorg,    latorg      ! (radian)
    real(kind=KTGT), intent(in)  :: latts,     major,    ecc
    real(kind=KTGT), intent(in)  :: xl, dx
    real(kind=KTGT), intent(in)  :: yl, dy
    integer,         intent(in)  :: mx, my
    integer,         intent(in)  :: div
    integer,optional,intent(in)  :: levv, u

    integer lv, utmp
    integer jsinla

    real(kind=KTGT) :: xh, yh
    integer mch
    real(kind=KTGT) :: dlo, dla,  divlo, divla
    integer dir_lo
    logical eqlon
    real(kind=KTGT) :: r2
    integer ndiv, mdiv
    integer lcache, lstack
    integer,allocatable :: estack(:)
    integer,allocatable :: ostack(:)
    real(kind=KTGT),allocatable :: wsum(:)
    integer mcla, mclo
    real(kind=KTGT) :: cco(0:ncache_stereog_lo - 1)
    real(kind=KTGT),allocatable :: cla(:, :), clo(:, :)

    real(kind=KTGT) :: adiv, wmin, wmax
    real(kind=KTGT) :: lon, lat
    real(kind=KTGT) :: xy(2, 4)
    integer :: jhs(4)
    integer jhtgt, jhmin, jhmax

    integer jst, kst
    integer jgx, jgy, jgh, mgh
    integer jdx, jdy, jdh
    integer jxs, jys
    integer nxt
    integer pole
    integer jt
    logical bpop

    ierr = 0
    nmem = 0

    lv = choice(lev_verbose, levv)
    utmp = get_logu(u, ulog)

    ! health check
    xh = xl + dx * real(mx, kind=KTGT)
    yh = yl + dy * real(my, kind=KTGT)

    if (latts.gt.0) then
       pole = +1
    else if (latts.lt.0) then
       pole = -1
    else
       ierr = _ERROR(ERR_INVALID_PARAMETER)
       return
    endif

    call check_monotonic(dir_lo, lonn(0:mlon), dlo)
    eqlon = (abs(dir_lo).le.equidistant_enough)
    if (eqlon) then
       dlo = (2.0_KTGT * ATAN2(0.0_KTGT,-1.0_KTGT)) / real(mlon, kind=KTGT)
       if (dir_lo.lt.0) dlo = -dlo
    else
       dlo = 0.0_KTGT
    endif

    r2 = (r * r)
    lstack = 1 + 3 * div
    mdiv = 2 ** div
    lcache = mdiv
    mch = mx * my
    mgh = mlat * mlon
    mcla = ncache_stereog_la + 1
    mclo = ncache_stereog_lo + 1
    jsinla = ncache_stereog_la

    if (ierr.eq.0) allocate(estack(0:lstack-1), ostack(0:div), STAT=ierr)
    if (ierr.eq.0) allocate(wsum(0:mch-1), STAT=ierr)
    if (ierr.eq.0) allocate(cla(0:mcla-1, 0:lcache), clo(0:mclo-1, 0:lcache), STAT=ierr)
    if (ierr.ne.0) then
       ierr = _ERROR(ERR_ALLOCATION)
       return
    endif
    call proj_pstereog_set(cco, ecc, major, latts, pole)

    ostack(0) = 0
    do jst = 0, div - 1
       ostack(jst+1) = ostack(jst) + (4 ** jst)
    enddo
    do jgy = 0, mlat - 1
       dla = (latn(jgy + 1) - latn(jgy))
       divla = (dla / real(mdiv, kind=KTGT))
       do jdy = 0, mdiv - 1
          lat = latn(jgy) + divla * real(jdy, kind=KTGT)
          cla(jsinla, jdy) = sin(lat)
          call proj_pstereog_cachela(cla(:, jdy), lat, cco, ecc)
       enddo
       jdy = mdiv
       lat = latn(jgy + 1)
       call proj_pstereog_cachela(cla(:, jdy), lat, cco, ecc)
       cla(jsinla, mdiv) = sin(latn(jgy+1))
       do jgx = 0, mlon - 1
          wsum(0:mch-1) = 0.0_KTGT
          if (.not.eqlon) dlo = (lonn(jgx + 1) - lonn(jgx))
          divlo = (dlo / real(mdiv, kind=KTGT))
          do jdx = 0, mdiv - 1
             lon = lonn(jgx) + divlo * real(jdx, kind=KTGT)
             call proj_pstereog_cachelo(clo(:, jdx), lon, cco, (/lonorg, latorg/))
          enddo
          jdx = mdiv
          lon = lonn(jgx + 1)
          call proj_pstereog_cachelo(clo(:, jdx), lon, cco, (/lonorg, latorg/))

          jst = 0
          estack(jst) = 0
          kst = 0
          ndiv = 1
          jgh = jgy * mlon + jgx
          jhmin = mch
          jhmax = -1
          write(*, *) 'gen3/stack: --- ', jgh, '/', mgh, jgx, jgy
          do
             if (jst.lt.0) exit
             ! write(*, *) jst, kst, estack(0:jst)
             do
                if (estack(jst).ge.ostack(kst)) exit
                kst  = kst - 1
                ndiv = ndiv / 2
             enddo
             jdh = estack(jst) - ostack(kst)
             jdx = mod(jdh, ndiv)
             jdy = jdh / ndiv
             nxt = (mdiv / ndiv)
             jdx = jdx * nxt
             jdy = jdy * nxt
             ! write(*, *) jdx, jdy, clo(:,jdx), cla(:,jdy)
             xy(:,1) = proj_pstereog(cco, clo(:,jdx),     cla(:,jdy))
             xy(:,2) = proj_pstereog(cco, clo(:,jdx+nxt), cla(:,jdy+nxt))
             xy(:,3) = proj_pstereog(cco, clo(:,jdx+nxt), cla(:,jdy))
             xy(:,4) = proj_pstereog(cco, clo(:,jdx),     cla(:,jdy+nxt))
             bpop = .FALSE.
             if (ALL(xy(1, :).lt.xl) .or. ALL(xh.lt.xy(1, :)) &
                  & .or. ALL(xy(2, :).lt.yl) .or. ALL(yh.lt.xy(2, :))) then
                bpop = .TRUE.
             else
                do jt = 1, 4
                   jxs = FLOOR((xy(1, jt) - xl) / dx)
                   jys = FLOOR((xy(2, jt) - yl) / dy)
                   jhs(jt) = jys * mx + jxs
                enddo
                divlo = (dlo / real(ndiv, kind=KTGT))
                if (ALL(jhs(1).eq.jhs(2:))) then
                   jhtgt = jhs(1)
                   adiv = abs((cla(jsinla,jdy+nxt) - cla(jsinla,jdy)) * divlo) * r2
                   ! write(*, *) 'adiv/2', jhtgt, adiv, cla(jsinla,jdy+nxt), cla(jsinla,jdy), divlo
                   wsum(jhtgt) = wsum(jhtgt) + adiv
                   bpop = .TRUE.
                   jhmin = min(jhmin, jhtgt)
                   jhmax = max(jhmax, jhtgt)
                   ! write(*,*) 'jhtgt', jhtgt, jhmin, jhmax
                else if (kst.eq.div) then
                   adiv = abs((cla(jsinla,jdy+nxt) - cla(jsinla,jdy)) * divlo) * r2
                   lon = lonn(jgx) + divlo * (real(jdx, kind=KTGT) + 0.5_KTGT)
                   lat = latn(jgy) + divla * (real(jdy, kind=KTGT) + 0.5_KTGT)
                   ! write(*, *) rad2deg(lonn(jgx)), rad2deg(lon), jdx, xy(1, :)
                   ! write(*, *) rad2deg(latn(jgy)), rad2deg(lat), jdy, xy(2, :)
                   jt = 4
                   xy(:,jt) = proj_pstereog(lon, lat, cco, ecc, (/lonorg, latorg/))
                   ! write(*, *) 'edge/x', xy(1, jt), xl, xh
                   ! write(*, *) 'edge/y', xy(2, jt), yl, yh
                   if (xy(1, jt).lt.xl .or. xh.lt.xy(1, jt) &
                        & .or.xy(2, jt).lt.yl .or. yh.lt.xy(2, jt)) then
                      continue
                   else
                      jxs = FLOOR((xy(1, jt) - xl) / dx)
                      jys = FLOOR((xy(2, jt) - yl) / dy)
                      ! write(*, *) 'div', jxs, jys, (xy(1, jt) - xl) / dx, (xy(2, jt) - yl) / dy
                      jhtgt = jys * mx + jxs
                      ! write(*, *) 'adiv/3', jhtgt, adiv, cla(jsinla,jdy+nxt), cla(jsinla,jdy), divlo
                      wsum(jhtgt) = wsum(jhtgt) + adiv
                      jhmin = min(jhmin, jhtgt)
                      jhmax = max(jhmax, jhtgt)
                      ! write(*,*) 'jhtgt', jhtgt, jhmin, jhmax
                   endif
                   bpop = .TRUE.
                else
                   kst = kst + 1
                   ndiv = ndiv * 2
                   jdx = (jdx / nxt) * 2
                   jdy = (jdy / nxt) * 2
                   estack(jst)   = jdy * ndiv + jdx + ostack(kst)
                   estack(jst+1) = estack(jst) + 1
                   estack(jst+2) = estack(jst) + ndiv
                   estack(jst+3) = estack(jst) + ndiv + 1
                   jst = jst + 3
                endif
             endif
             if (bpop) jst = jst - 1
          enddo
          ! write(*, *) jhmin, jhmax
          do jhtgt = jhmin, jhmax
             if (wsum(jhtgt).gt.0.0_KTGT) then
                if (nmem.lt.lmem) then
                   isrc(nmem) = jhtgt
                   wsrc(nmem) = wsum(jhtgt)
                endif
                nmem = nmem + 1
             endif
          enddo
          iofs(jgh) = nmem
       enddo
       jst = 0
    enddo
    if (ierr.eq.0) then
       if (nmem.le.lmem) then
          wsum(:) = 0.0_KTGT
          do jt = 0, nmem - 1
             jhtgt = isrc(jt)
             wsum(jhtgt) = wsum(jhtgt) + wsrc(jt)
          enddo
          wmax = MAXVAL(wsum(0:mch-1))
          wmin = MINVAL(wsum(0:mch-1))
          write(*, *) 'gg:area', wmax, wmin, dx * dy
       endif
       do jt = 0, mch - 1
          jys = jt / mx
          jxs = mod(jt, mx)
          write(*, *) 'wsum:', &
               & xl + dx * (real(jxs, kind=KTGT) + 0.5_KTGT), &
               & yl + dy * (real(jys, kind=KTGT) + 0.5_KTGT), wsum(jt)
       enddo
    endif
    if (ierr.eq.0) deallocate(estack, ostack, wsum, cla, clo, STAT=ierr)
  end subroutine gen_gg_stereo_map_d
!!!_ + end AMI_TABLE
end module TOUZA_AMI_TABLE
!!!_@ test_ami_table - test program
#if TEST_AMI_TABLE
program test_ami_table
  use TOUZA_Ami_table
  use TOUZA_Ami_std
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
        call batch_test_gs(ierr)
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
    real(kind=KTGT),allocatable :: glat(:), wlat(:), glatm(:)
    real(kind=KTGT),allocatable :: glon(:), wlon(:), glonm(:)
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE =  1.0_KTGT
    real(kind=KTGT),parameter :: HALF = 0.5_KTGT
    real(kind=KTGT) :: span = ZERO
    real(kind=KTGT) :: wnml = ONE
    real(kind=KTGT) :: llorg(2), latts
    real(kind=KTGT),parameter :: def = -HUGE(ZERO)
    real(kind=KTGT) :: e, a, rf, r
    real(kind=KTGT) :: xl, xh, dx
    real(kind=KTGT) :: yl, yh, dy
    real(kind=KTGT) :: xx(3), yy(3)
    integer div
    integer mxc, myc

    integer nmem, lmem, ngg
    real(kind=KTGT),allocatable :: wsrc(:)
    integer,        allocatable :: isrc(:), iofs(:)

    integer jx, jy

    ierr = 0
    if (ierr.eq.0) call get_option(ierr, nlon,       'lon', -1)
    if (ierr.eq.0) call get_option(ierr, nlat,       'lat', -1)
    if (ierr.eq.0) call get_option(ierr, llorg(1:2), 'org', def)
    if (ierr.eq.0) call get_option(ierr, latts,      'ts',  +70.0_KTGT)
    if (ierr.eq.0) call get_option(ierr, a, 'a', 6378137.0_KTGT)
    if (ierr.eq.0) call get_option(ierr, r, 'r', 6370000.0_KTGT)
    if (ierr.eq.0) call get_option(ierr, e, 'e', def)
    if (ierr.eq.0) call get_option(ierr, rf, 'rf', def)
    if (ierr.eq.0) call get_option(ierr, xx(1:3), 'x', def)   ! nodes boundary and delta
    if (ierr.eq.0) call get_option(ierr, yy(1:3), 'y', def)
    if (ierr.eq.0) call get_option(ierr, div, 'div', 0)
    if (ierr.eq.0) call get_option(ierr, lmem, 'l', 0)

    if (ierr.eq.0) then
       if (nlon.lt.0) nlon = 128
       if (nlat.lt.0) nlat = nlon / 2
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
       if (div.le.0) div = 8
    endif
    if (ierr.eq.0) then
       if (llorg(1).eq.def) llorg(1) = -45.0_KTGT
       if (llorg(2).eq.def) llorg(2) = +90.0_KTGT
       llorg(1:2) = deg2rad(llorg(1:2))
       latts = deg2rad(latts)
    endif
    if (ierr.eq.0) then
       if (lmem.le.0) lmem = 16
       ngg = nlat * nlon
       lmem = lmem * ngg
       allocate (glat(0:nlat-1), wlat(0:nlat-1), glatm(0:nlat), &
            &    glon(0:nlon-1), wlon(0:nlon-1), glonm(0:nlon), &
            &    iofs(0:ngg),    isrc(0:lmem-1), wsrc(0:lmem-1), &
            &    STAT=ierr)
    endif

    if (ierr.eq.0) call gauss_latitude(ierr, glat, wlat, nlat, span, wnml, prec=-1.0_KTGT)
    if (ierr.eq.0) glat(0:nlat-1) = asin(glat(0:nlat-1))
    if (ierr.eq.0) call mid_latitude(ierr, glatm, wlat, nlat)

    if (ierr.eq.0) call get_longitude(ierr, glon,  wlon, nlon, span=span, wnml=wnml)
    if (ierr.eq.0) call mid_longitude(ierr, glonm, glon, wlon, nlon, span / wnml)

    if (ierr.eq.0) then
202    format('node-lat:', I0, 1x, F9.3)
       do jy = 0, nlat
          write(*, 202) jy, rad2deg(glatm(jy))
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

    if (ierr.eq.0) then
       call gen_gg_stereo_map &
            & (ierr,     iofs,     isrc,  wsrc, nmem, lmem, &
            &  glonm,    glatm,    nlon,  nlat, r,    &
            &  llorg(1), llorg(2), latts, a,    e,    &
            &  xl,       dx,       mxc,   yl,   dy,   myc,  div)
    endif
    if (ierr.eq.0) then
       if (nmem.gt.lmem) then
          write(*, *) 'retry = ', nmem, lmem
          lmem = nmem
          deallocate(isrc, wsrc, STAT=ierr)
          if (ierr.eq.0) allocate(isrc(0:lmem-1), wsrc(0:lmem-1), STAT=ierr)
          if (ierr.eq.0) then
             call gen_gg_stereo_map &
                  & (ierr,     iofs,     isrc,  wsrc, nmem, lmem, &
                  &  glonm,    glatm,    nlon,  nlat, r,    &
                  &  llorg(1), llorg(2), latts, a,    e,    &
                  &  xl,       dx,       mxc,   yl,   dy,   myc,  div)
          endif
       endif
    endif
  end subroutine batch_test_gs
end program test_ami_table
#endif /* TEST_AMI_TABLE */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

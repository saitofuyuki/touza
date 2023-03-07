!!!_! ami_table.F90. - TOUZA/Ami/table amida-coupler table procedures
! Maintainer: SAITO Fuyuki
! Created: May 2 2022
#define TIME_STAMP 'Time-stamp: <2022/05/02 16:14:25 fuyuki ami_table.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022
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
  use TOUZA_Ami_std, &
       & as_init=>init, as_diag=>diag, as_finalize=>finalize
!!!_ + default
  implicit none
  private
!!!_ + parameter
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
#define __MDL__ 't'
!!!_ + type
!!!_ + interfaces
  interface check_monotonic
     module procedure check_monotonic_d
  end interface check_monotonic
!!!_ + public procedures
  public init, diag, finalize
  public cyclic_interp_table
  public geo_interp_table
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
       if (is_first_force(init_counts, md)) then
          ulog = choice(ulog, u)
          lev_verbose = lv
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call as_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
       endif
       if (is_first_force(init_counts, md)) then
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
       if (is_first_force(diag_counts, md)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __MDL__, utmp)
          endif
       endif
       lmd = control_deep(md)
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
       if (is_first_force(fine_counts, md)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md)
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
  subroutine check_monotonic_d(idir, x, jbgn, jend)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: idir
    real(kind=KARG), intent(in)  :: x(:)
    integer,optional,intent(out) :: jbgn, jend

    integer j, n

    n = size(x)
    if (n.le.1) then
       idir = 0
       continue
    else if (x(1).lt.x(2)) then
       ! monotonic increase
       idir = +1
       do j = 2, n
          if (x(j-1).ge.x(j)) then
             idir = 0
             exit
          endif
       enddo
    else if (x(1).gt.x(2)) then
       ! monotonic decrease
       idir = -1
       do j = 2, n
          if (x(j-1).le.x(j)) then
             idir = 0
             exit
          endif
       enddo
    else
       idir = 0
    endif
    if (idir.gt.0) then
       call set_if_present(jbgn, 1)
       call set_if_present(jend, n)
    else if (idir.lt.0) then
       call set_if_present(jbgn, n)
       call set_if_present(jend, 1)
    else
       call set_if_present(jbgn, 0)
       call set_if_present(jend, 0)
    endif
    return
  end subroutine check_monotonic_d
!!!_ + end AMI_TABLE
end module TOUZA_AMI_TABLE
!!!_@ test_ami_table - test program
#if TEST_AMI_TABLE
program test_ami_table
  use TOUZA_Ami_table
  use TOUZA_Ami_std
  implicit none
  integer ierr
  integer n
  integer o, d

  ierr = 0

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

end program test_ami_table
#endif /* TEST_AMI_TABLE */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

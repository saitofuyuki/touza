!!!_! convoy_psprop.F90 - TOUZA/Jmz/convoy polar sterographic properties
! Maintainer: SAITO Fuyuki
! Created: Apr 10 2024
#define TIME_STAMP 'Time-stamp: <2024/05/07 15:08:02 fuyuki convoy_psprop.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2024
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "jmz_config.h"
#endif
#include "jmz.h"
!!!_* macros
#define _PFX 'convoy:psprop: '
!!!_@ TOUZA/Jmz/convoy_psprop - polar sterographic projection helper
module convoy_psprop
!!!_ + Declaration
!!!_  - modules
  use Jmz_param
  use Jmz_base
  use convoy_util
  use TOUZA_Nio,only: nio_record_std
  use TOUZA_Nio,only: put_header_cprop, put_item, get_default_header
  use TOUZA_Nio,only: litem, nitem
  use TOUZA_Nio,only: nio_write_header, nio_write_data, nio_store_csr
  implicit none
  public
!!!_  - parameters
  integer,parameter,private :: llev = 2
  integer,parameter,private :: lev_ini = 1
  integer,parameter,private :: lev_max = 2
!!!_ + Body
  contains
!!!_  & qoxi_psprop
  subroutine qoxi_psprop &
       & (ierr, jpos, npos, levv)
    use TOUZA_Emu,only: ncache_psgp_co
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(inout)       :: jpos
    integer,intent(in)          :: npos
    integer,intent(in),optional :: levv
    integer :: mx, my
    real(kind=KTGT) :: xprop(mem_cp), yprop(mem_cp)
    real(kind=KTGT) :: major, flatten   ! semi-major radius [m], flattening
    character(len=lname) :: xname, yname

    real(kind=KTGT) :: cco(ncache_psgp_co)

    integer :: ufile
    character(len=lpath) :: ofile
    character(len=litem) :: head(nitem)
    integer :: krect

    integer :: levp(llev)
    real(kind=KTGT) :: tol

    ierr = 0
    if (ierr.eq.0) call parse_ellipsoid(ierr, major=major, flatten=flatten)
    if (ierr.eq.0) call parse_psgp(ierr, cco, flatten, major)
    if (ierr.eq.0) then
       call parse_psplane(ierr, mx, my, xprop, yprop, xname, yname, cco)
    endif

    if (ierr.eq.0) call parse_psprop_params(ierr, levp, tol)

    if (ierr.eq.0) then
       call get_param(ierr, ofile, jpos, ' ')
       if (ierr.eq.0) then
          jpos = jpos + 1
       else
          ofile = ' '
       endif
    endif
    if (ierr.eq.0) then
       if (ofile.eq.' ') then
          ufile = -1
          head(:) = ' '
          krect = 0
       else
          krect = nio_record_std()
          if (ierr.eq.0) call open_write(ierr, ufile, ofile, cfmt_gtool_seq)
          if (ierr.eq.0) call get_default_header(head)
       endif
    endif

    if (ierr.eq.0) then
       call batch_psprop &
            & (ierr,  jpos,   npos,    &
            &  mx,    xprop,  xname,   &
            &  my,    yprop,  yname,   &
            &  cco,   levp,   tol,     &
            &  ufile, head,   krect,   levv)
    endif
  end subroutine qoxi_psprop
!!!_  & batch_psprop
  subroutine batch_psprop &
       & (ierr,  jpos,   npos,    &
       &  mx,    xprop,  xname,   &
       &  my,    yprop,  yname,   &
       &  cco,   levp,   tol,     &
       &  ufile, head,   krect,   levv)
    use TOUZA_Emu,only: psgp_bwd_ll, psgp_bwd_sf
    use TOUZA_Emu,only: psgp_bwd_tr, area_distortion, psgp_inquire
    use TOUZA_Emu,only: psgp_bwd_length
    use TOUZA_Nio,only: axis_set_header, axis_loc, axis_wgt
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jpos
    integer,intent(in)    :: npos
    integer,         intent(in) :: mx,       my
    real(kind=KTGT), intent(in) :: xprop(*), yprop(*)
    character(len=*),intent(in) :: xname,    yname
    real(kind=KTGT), intent(in) :: cco(*)
    integer,         intent(in) :: levp(*)
    real(kind=KTGT), intent(in) :: tol
    integer,         intent(in) :: ufile
    character(len=*),intent(inout) :: head(*)
    integer,         intent(inout) :: krect
    integer,optional,intent(in)    :: levv
    real(kind=KTGT) :: xl, dx, yl, dy
    real(kind=KTGT),allocatable :: cxo(:),  cyo(:)
    real(kind=KTGT),allocatable :: cxb(:),  cyb(:)
    real(kind=KTGT),allocatable :: cxdbl(:),cydbl(:)
    real(kind=KTGT),allocatable :: glon(:), glat(:)
    real(kind=KTGT),allocatable :: rcos(:), rsin(:)
    real(kind=KTGT),allocatable :: gscf(:)
    real(kind=KTGT),allocatable :: trlon(:, :), trlat(:, :)
    real(kind=KTGT),allocatable :: gadf(:)
    real(kind=KTGT),allocatable :: dblsp(:)
    real(kind=KTGT),allocatable :: spres(:)
    real(kind=KTGT) :: e

    real(kind=KTGT) :: ll(NGEOG)
    real(kind=KTGT) :: dmy(NTRIG), dlo(NTRIG)
    real(kind=KTGT) :: pdis, pres

    integer,parameter :: larg = 256
    integer jitem
    integer mh
    integer kbatch, kitem

    integer,parameter :: var_center_lat = 0
    integer,parameter :: var_center_lon = 1
    integer,parameter :: var_center_scf = 2  ! scale factor
    integer,parameter :: var_center_adf = 3  ! area distortion factor
    integer,parameter :: var_center_rotsin = 4  ! conversion angle to geographic
    integer,parameter :: var_center_rotcos = 5  ! conversion angle to geographic

    integer,parameter :: var_center_cx = 6
    integer,parameter :: var_center_cy = 7

    integer,parameter :: var_double_xsp = 8  ! x spacing of node-center
    integer,parameter :: var_double_ysp = 9  ! y spacing of node-center
    integer,parameter :: var_double_xspres = 10
    integer,parameter :: var_double_yspres = 11
    integer,parameter :: nvar = 12

    integer,parameter :: batch_null = 0
    integer,parameter :: batch_all  = 1
    integer,parameter :: mbatch = 1

    integer,parameter :: lbv = 8
    character(len=lbv),save :: vname(0:nvar-1) = ' '
    character(len=lbv),save :: bname(0:mbatch) = ' '
    integer,save :: vmask(0:nvar-1) = -1
    integer,save :: bmask(0:mbatch) = -1

    integer lxdbl, lydbl, lhdbl
    integer mxdbl, mydbl, mhdbl
    integer jhdbl
    character(len=litem) :: xnamed, ynamed

    integer jx, jy, jh
    character(len=lmsg) :: txt

    ierr = 0

    if (ierr.eq.0) call put_item(ierr, head, DSET_PSPROP, hi_DSET)

    mh = mx * my

    lxdbl = mx * 2
    lydbl = my * 2
    lhdbl = (lxdbl + 1) * (lydbl + 1)

101 format(A, 'DBL')
    write(xnamed, 101) trim(xname)
    write(ynamed, 101) trim(yname)

    if (bmask(0).lt.0) then
       bmask(batch_null) = 0
       bmask(batch_all)  = 1

       bname(batch_null) = ' '
       bname(batch_all) = 'ALL'

       vmask(:) = bmask(batch_all)

       vname(var_center_lat) = 'LAT'
       vname(var_center_lon) = 'LON'
       vname(var_center_scf) = 'SF'
       vname(var_center_adf) = 'ADF'
       vname(var_center_rotsin) = 'RSIN'
       vname(var_center_rotcos) = 'RCOS'
       vname(var_center_cx) = 'CX'
       vname(var_center_cy) = 'CY'

       vname(var_double_xsp) = 'XSP'
       vname(var_double_ysp) = 'YSP'
       vname(var_double_xspres) = 'XSPRES'
       vname(var_double_yspres) = 'YSPRES'
    endif

    if (ierr.eq.0) then
       xl = xprop(cp_low)
       dx = xprop(cp_spacing)
       yl = yprop(cp_low)
       dy = yprop(cp_spacing)
       allocate(cxo(0:mx-1), cyo(0:my-1), &
            &   cxb(0:mx),   cyb(0:my),   &
            &   cxdbl(0:lxdbl), cydbl(0:lydbl), STAT=ierr)
    endif
    if (ierr.eq.0) then
       do jx = 0, mx - 1
          cxo(jx) = xl + dx * (real(jx, KIND=KTGT) + 0.5_KTGT)
       enddo
       do jy = 0, my - 1
          cyo(jy) = yl + dy * (real(jy, KIND=KTGT) + 0.5_KTGT)
       enddo
       do jx = 0, mx
          cxb(jx) = xl + dx * real(jx, KIND=KTGT)
       enddo
       do jy = 0, my
          cyb(jy) = yl + dy * real(jy, KIND=KTGT)
       enddo
       do jx = 0, lxdbl
          cxdbl(jx) = xl + dx * (real(jx, KIND=KTGT) / 2.0_KTGT)
       enddo
       do jy = 0, lydbl
          cydbl(jy) = yl + dy * (real(jy, KIND=KTGT) / 2.0_KTGT)
       enddo
    endif

    jitem = -1
    if (jpos.gt.npos) then
       kbatch = batch_all
       kitem = 0
    else
       kbatch = batch_null
       kitem = -1
    endif
    loop_parse: do
       if (ierr.eq.0) then
          call parse_output_var &
               & (ierr,  jitem, kbatch, kitem, jpos,  npos, &
               &  bname, bmask, mbatch, vname, vmask, nvar)
       endif
       if (ierr.ne.0) exit loop_parse
       if (jitem.lt.0) exit loop_parse
       select case(jitem)
       case(var_center_lat, var_center_lon)
          if (.not.allocated(glat)) then
             allocate(glon(0:mh-1), glat(0:mh-1), STAT=ierr)
          endif
          if (ierr.eq.0) then
             do jy = 0, my - 1
                do jx = 0, mx - 1
                   jh = jy * mx + jx
                   ll = psgp_bwd_ll(cxo(jx), cyo(jy), cco)
                   glat(jh) = ll(JLATI)
                   glon(jh) = ll(JLONGI)
                enddo
             enddo
          endif
          if (ierr.eq.0) then
             call set_header_plane(ierr, head, 'UR8', mx, xname, my, yname)
          endif
          select case(jitem)
          case(var_center_lat)
             if (ierr.eq.0) call put_item(ierr, head, 'lat', hi_ITEM)
             if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
             if (ierr.eq.0) call nio_write_data(ierr, glat, mh, head, krect, ufile)
          case(var_center_lon)
             if (ierr.eq.0) call put_item(ierr, head, 'lon', hi_ITEM)
             if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
             if (ierr.eq.0) call nio_write_data(ierr, glon, mh, head, krect, ufile)
          end select
       case(var_center_rotsin, var_center_rotcos)
          if (.not.allocated(rcos)) then
             allocate(rcos(0:mh-1), rsin(0:mh-1), STAT=ierr)
          endif
          if (ierr.eq.0) then
             do jy = 0, my - 1
                do jx = 0, mx - 1
                   jh = jy * mx + jx
                   call psgp_bwd_tr(dmy, dlo, cxo(jx), cyo(jy), cco)
                   rcos(jh) = dlo(JCOS)
                   rsin(jh) = dlo(JSIN)
                enddo
             enddo
          endif
          if (ierr.eq.0) then
             call set_header_plane(ierr, head, 'UR8', mx, xname, my, yname)
          endif
          select case(jitem)
          case(var_center_rotsin)
             if (ierr.eq.0) call put_item(ierr, head, 'rsin', hi_ITEM)
             if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
             if (ierr.eq.0) call nio_write_data(ierr, rsin, mh, head, krect, ufile)
          case(var_center_rotcos)
             if (ierr.eq.0) call put_item(ierr, head, 'rcos', hi_ITEM)
             if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
             if (ierr.eq.0) call nio_write_data(ierr, rcos, mh, head, krect, ufile)
          end select
       case(var_center_adf)
          if (.not.allocated(trlat)) then
             allocate(trlon(NTRIG, 0:mh-1), trlat(NTRIG, 0:mh-1), gadf(0:mh-1), STAT=ierr)
          endif
          if (ierr.eq.0) then
             do jy = 0, my - 1
                do jx = 0, mx - 1
                   jh = jy * mx + jx
                   call psgp_bwd_tr(trlat(:, jh), trlon(:, jh), cxo(jx), cyo(jy), cco)
                enddo
             enddo
             call psgp_inquire(ierr, cco, e=e)
          endif
          if (ierr.eq.0) then
             gadf(0:mh-1) = area_distortion(trlat(JSIN,0:mh-1), e)
          endif
          if (ierr.eq.0) then
             call set_header_plane(ierr, head, 'UR8', mx, xname, my, yname)
          endif
          if (ierr.eq.0) call put_item(ierr, head, 'adf', hi_ITEM)
          if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
          if (ierr.eq.0) call nio_write_data(ierr, gadf, mh, head, krect, ufile)
       case(var_center_scf)
          if (.not.allocated(gscf)) then
             allocate(gscf(0:mh-1), STAT=ierr)
          endif
          if (ierr.eq.0) then
             do jy = 0, my - 1
                do jx = 0, mx - 1
                   jh = jy * mx + jx
                   gscf(jh) = psgp_bwd_sf(cxo(jx), cyo(jy), cco)
                enddo
             enddo
          endif
          if (ierr.eq.0) then
             call set_header_plane(ierr, head, 'UR8', mx, xname, my, yname)
          endif
          if (ierr.eq.0) call put_item(ierr, head, 'sf', hi_ITEM)
          if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
          if (ierr.eq.0) call nio_write_data(ierr, gscf, mh, head, krect, ufile)
       case(var_center_cx)
          call axis_set_header(ierr, head, xname, mx, axis_loc)
          if (ierr.eq.0) call put_item(ierr, head, 'UR8', hi_DFMT)
          if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
          if (ierr.eq.0) call nio_write_data(ierr, cxo, mx, head, krect, ufile)
          ! recover DSET
          if (ierr.eq.0) call put_item(ierr, head, DSET_PSPROP, hi_DSET)
       case(var_center_cy)
          call axis_set_header(ierr, head, yname, my, axis_loc)
          if (ierr.eq.0) call put_item(ierr, head, 'UR8', hi_DFMT)
          if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
          if (ierr.eq.0) call nio_write_data(ierr, cyo, my, head, krect, ufile)
          ! recover DSET
          if (ierr.eq.0) call put_item(ierr, head, DSET_PSPROP, hi_DSET)
       case(var_double_xsp, var_double_xspres)
          if (.not.allocated(dblsp)) then
             allocate(dblsp(0:lhdbl-1), spres(0:lhdbl-1), STAT=ierr)
          endif
          mxdbl = mx * 2
          mydbl = my * 2 + 1
          mhdbl = mxdbl * mydbl
201       format('spacing:x: ', I0)
          do jy = 0, mydbl - 1
             if (is_verbose(msglev_INFO)) then
                write(txt, 201) jy
                call message(ierr, txt)
             endif
             do jx = 0, mxdbl - 1
                jhdbl = jy * mxdbl + jx
                call psgp_bwd_length &
                     & (pdis, &
                     &  cxdbl(jx), cydbl(jy), cxdbl(jx+1), cydbl(jy), &
                     &  cco, levp(lev_ini), levp(lev_max), tol, pres)
                dblsp(jhdbl) = pdis
                spres(jhdbl) = pres
             enddo
          enddo
          if (ierr.eq.0) then
             call set_header_plane(ierr, head, 'UR8', mxdbl, xnamed, mydbl, ynamed)
          endif
          select case(jitem)
          case(var_double_xsp)
             if (ierr.eq.0) call put_item(ierr, head, 'xsp', hi_ITEM)
             if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
             if (ierr.eq.0) call nio_write_data(ierr, dblsp, mhdbl, head, krect, ufile)
          case(var_double_xspres)
             if (ierr.eq.0) call put_item(ierr, head, 'xspres', hi_ITEM)
             if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
             if (ierr.eq.0) call nio_write_data(ierr, spres, mhdbl, head, krect, ufile)
          end select
       case(var_double_ysp, var_double_yspres)
          if (.not.allocated(dblsp)) then
             allocate(dblsp(0:lhdbl-1), spres(0:lhdbl-1), STAT=ierr)
          endif
          mxdbl = mx * 2 + 1
          mydbl = my * 2
          mhdbl = mxdbl * mydbl
202       format('spacing:y: ', I0)
          do jy = 0, mydbl - 1
             if (is_verbose(msglev_INFO)) then
                write(txt, 202) jy
                call message(ierr, txt)
             endif
             do jx = 0, mxdbl - 1
                jhdbl = jy * mxdbl + jx
                call psgp_bwd_length &
                     & (pdis, &
                     &  cxdbl(jx), cydbl(jy), cxdbl(jx), cydbl(jy+1), &
                     &  cco, levp(lev_ini), levp(lev_max), tol, pres)
                dblsp(jhdbl) = pdis
                spres(jhdbl) = pres
             enddo
          enddo
          if (ierr.eq.0) then
             call set_header_plane(ierr, head, 'UR8', mxdbl, xnamed, mydbl, ynamed)
          endif
          select case(jitem)
          case(var_double_ysp)
             if (ierr.eq.0) call put_item(ierr, head, 'ysp', hi_ITEM)
             if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
             if (ierr.eq.0) call nio_write_data(ierr, dblsp, mhdbl, head, krect, ufile)
          case(var_double_yspres)
             if (ierr.eq.0) call put_item(ierr, head, 'yspres', hi_ITEM)
             if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
             if (ierr.eq.0) call nio_write_data(ierr, spres, mhdbl, head, krect, ufile)
          end select
       case default
       end select
    enddo loop_parse

    if (ierr.eq.0) then
       if (allocated(cxo)) deallocate(cxo, cyo, cxb, cyb, cxdbl, cydbl, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (allocated(glat)) deallocate(glat, glon, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (allocated(glat)) deallocate(gscf, STAT=ierr)
    endif
  end subroutine batch_psprop
!!!_  - parse_ellipsoid
  subroutine parse_ellipsoid &
       & (ierr, major, minor, ecc, flatten, def)
    use TOUZA_Emu,only: flatten_to_ecc
    implicit none
    integer,         intent(out)          :: ierr
    real(kind=KTGT), intent(out),optional :: major, minor
    real(kind=KTGT), intent(out),optional :: ecc,   flatten
    character(len=*),intent(in), optional :: def

    character(len=*),parameter :: atag = 'ELLIPSOID'

    integer,parameter   :: larg = 128
    character(len=larg) :: aval, abuf
    integer,parameter   :: li = 2
    character(len=larg) :: aitems(li)
    integer ni

    real(kind=KTGT) :: a, f
    character(len=lmsg) :: txt
    ! ELLIPSOID|ELL|E=MAJOR:FLATTENING
    ! ELLIPSOID|ELL|E=SPECIAL
    ierr = 0
    if (ierr.eq.0) then
       call get_last_option(ierr, aval, atag, atag(1:3), atag(1:1), def=' ')
    endif
    if (ierr.eq.0) then
       if (aval.eq.' ') then
          if (present(def)) then
             aval = def
          endif
       endif
       if (aval.eq.' ') aval = 'WGS84'
       call split_list(ni, aitems, aval, sep_item, li)
       ierr = min(0, ni)
    endif
    if (ierr.eq.0) then
       if (ni.eq.1) then
          call upcase(abuf, aitems(1))
          select case (abuf)
          case('WGS84')
             a = 6378137.0_KTGT
             f = 1.0_KTGT / 298.257223563_KTGT
          case('MIROC')
             a = 6370000.0_KTGT
             f = 0.0_KTGT
          case default
             ni = 2
             aitems(2) = '0'
          end select
       endif
    endif
    if (ierr.eq.0) then
       if (ni.eq.2) then
          call parse_number(ierr, a, aitems(1))
          if (ierr.eq.0) call parse_number(ierr, f, aitems(2), 0.0_KTGT)
          if (ierr.ne.0) then
             write(*, *) 'cannot parse ellipsoid argument ', trim(aval)
          endif
       endif
    endif
    if (ierr.eq.0) then
       if (present(major)) then
          major = a
       endif
       if (present(minor)) then
          minor = (1.0_KTGT - f) * a
       endif
       if (present(flatten)) then
          flatten = f
       endif
       if (present(ecc)) then
          ecc = flatten_to_ecc(f)
       endif
    endif
    if (ierr.eq.0) then
       if (is_verbose(msglev_NORMAL)) then
101       format('ellipsoid = ', F12.2, 1x, ES16.9)
          write(txt, 101) a, f
          call message(ierr, txt)
       endif
    endif
  end subroutine parse_ellipsoid

!!!_  - parse_psgp - parse polar stereographic projection parameters
  subroutine parse_psgp &
       & (ierr,    cco, &
       &  flatten, major, def,   &
       &  clon,    clat,  tslat)
    use TOUZA_Std,only: choice
    use TOUZA_Emu,only: psgp_set_byf
    implicit none
    integer,         intent(out)          :: ierr
    real(kind=KTGT), intent(out)          :: cco(*)
    real(kind=KTGT), intent(in)           :: flatten, major
    character(len=*),intent(in),optional  :: def
    real(kind=KTGT), intent(out),optional :: clon, clat, tslat

    real(kind=KTGT) :: clond,  clatd        ! projection center lat/lon       [deg]
    real(kind=KTGT) :: lattsd               ! true scale latitude             [deg]
    real(kind=KTGT) :: lattsr               !                                 [rad]
    integer pole

    ierr = 0
    if (ierr.eq.0) call parse_psproj(ierr, clatd, clond, lattsd, def=def)
    if (ierr.eq.0) then
       lattsr = deg2rad(lattsd)
       if (clatd.gt.ZERO) then
          pole = +1
       else if (clatd.lt.ZERO) then
          pole = -1
       else
          pole = 0
       endif
       call psgp_set_byf(ierr, cco, flatten, major, lattsr, clond, pole, lodeg=.TRUE.)
    endif

    if (ierr.eq.0) then
       call set_if_present(clon, clond)
       call set_if_present(clat, clatd)
       call set_if_present(tslat, lattsd)
    endif
  end subroutine parse_psgp
!!!_  - parse_psproj
  subroutine parse_psproj &
       & (ierr, clat, clon, tslat, def)
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KTGT), intent(out)         :: clat, clon, tslat
    character(len=*),intent(in),optional :: def

    character(len=*),parameter :: atag = 'PROJ'

    real(kind=KTGT) :: vbuf
    integer,parameter   :: larg = 128
    character(len=larg) :: aval, abuf
    integer,parameter   :: li = 3
    character(len=larg) :: aitems(li)
    integer ni
    character(len=lmsg) :: txt

    ! PROJ|PR|P=LAT[,:]LON:TRUE
    ! PROJ|PR|P=SPECIAL:TRUE
    ierr = 0
    if (ierr.eq.0) then
       call get_last_option(ierr, aval, atag, atag(1:2), atag(1:1), def=' ')
    endif
    if (ierr.eq.0) then
       if (aval.eq.' ') then
          if (present(def)) then
             aval = def
          endif
       endif
       if (aval.eq.' ') aval = 'GL'
       aitems(:) = ' '
       call split_list(ni, aitems, aval, sep_item, li)
       ierr = min(0, ni)
    endif
    if (ierr.eq.0) then
       call parse_number(ierr, vbuf, aitems(1))
       if (ierr.ne.0) then
          ierr = 0
          aitems(3) = aitems(2)
          call upcase(abuf, aitems(1))
          select case (abuf)
          case('GL')
             aitems(1) = '90'
             aitems(2) = '-45'
             if (aitems(3).eq.' ') aitems(3) = '70'
          case('AA')
             aitems(1) = '-90'
             aitems(2) = '0'
             if (aitems(3).eq.' ') aitems(3) = '-71'
          case default
             ierr = ERR_INVALID_PARAMETER
          end select
       endif
    endif
    if (ierr.eq.0) then
       if (aitems(1).eq.' ') aitems(1) = '90'
       if (aitems(2).eq.' ') aitems(2) = '0'
       if (aitems(3).eq.' ') aitems(3) = aitems(1)
    endif
    if (ierr.eq.0) call parse_number(ierr, clat,  aitems(1))
    if (ierr.eq.0) call parse_number(ierr, clon,  aitems(2))
    if (ierr.eq.0) call parse_number(ierr, tslat, aitems(3))

    if (ierr.ne.0) then
       write(*, *) 'cannot parse ps-projection argument ', trim(aval)
    endif

    if (ierr.eq.0) then
       if (is_verbose(msglev_NORMAL)) then
101       format('projection = ', F10.1, F10.1, ' / ', F10.1)
          write(txt, 101) clat, clon, tslat
          call message(ierr, txt)
       endif
    endif
  end subroutine parse_psproj

!!!_  - parse_psplane
  subroutine parse_psplane &
       & (ierr, mx, my, xprop, yprop, xname, yname, &
       &  cco,  cdef,   kflag)
    use TOUZA_Emu,only: psgp_fwd, psgp_inquire
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: mx,       my
    real(kind=KTGT), intent(out)         :: xprop(*), yprop(*)
    character(len=*),intent(out)         :: xname,    yname
    real(kind=KTGT), intent(in)          :: cco(*)
    character(len=*),intent(in),optional :: cdef
    integer,         intent(in),optional :: kflag

    character(len=*),parameter :: otag = 'ORG'
    character(len=*),parameter :: ctag = 'C'
    character(len=*),parameter :: xtag = 'X'
    character(len=*),parameter :: ytag = 'Y'

    integer,parameter   :: larg = 256
    character(len=larg) :: oval
    character(len=larg) :: cval
    integer,parameter :: li = 3
    character(len=larg) :: aitem(li)
    real(kind=KTGT) :: xdef(mem_cp), ydef(mem_cp)
    real(kind=KTGT) :: gorg(NGEOG),  xyorg(2)
    real(kind=KTGT) :: sp, sh
    integer ni
    integer cf

    ierr = 0
    xdef(:) = cp_undef
    ydef(:) = cp_undef

    !  LH corresponds to grid-line default
    !  L-*-*-*-H
    !   s s s s
    cf = choice(cflag_boundary, kflag)
    ! ORG=SPECIAL
    ! ORG=LAT:LON
    if (ierr.eq.0) call get_last_option(ierr, oval, otag, def=' ')
    if (ierr.eq.0) then
       if (oval.eq.' ') then
          xyorg(:) = 0.0_KTGT
       else
          call upcase(oval)
          select case(oval)
          case('POLE', 'CENTER')
             call psgp_inquire(ierr, cco, olat=gorg(JLATI), olon=gorg(JLONGI))
          case('TS')
             call psgp_inquire(ierr, cco, tslat=gorg(JLATI), olon=gorg(JLONGI))
          case default
             call split_list(ni, gorg, oval, sep_item, NGEOG)
             if (ni.ne.NGEOG) then
                ierr = ERR_INVALID_PARAMETER
             else
                gorg(1:NGEOG) = deg2rad(gorg(1:NGEOG))
             endif
          end select
          if (ierr.eq.0) then
             xyorg = psgp_fwd(gorg(JLONGI), gorg(JLATI), cco)
          endif
       endif
       if (ierr.eq.0) then
          xdef(cp_ofs) = xyorg(1)
          ydef(cp_ofs) = xyorg(2)
       endif
    endif

    ! C=SPECIAL[:SPACING[:SHIFT]]
    if (ierr.eq.0) call get_last_option(ierr, cval, ctag, def=' ')
    if (ierr.eq.0) then
       if (cval.eq.' ') then
          if (present(cdef)) then
             cval = cdef
          endif
       endif
       aitem(1:3) = ' '
       call split_list(ni, aitem, cval, sep_item, 3)
       ierr = min(0, ni)
       if (ierr.eq.0) then
          call upcase(aitem(1))
          select case(aitem(1))
          case('GL')
             xdef(cp_low)  = -641150.0_KTGT - 1500.0_KTGT
             xdef(cp_high) = +867850.0_KTGT + 1500.0_KTGT
             ydef(cp_low)  = -3375050.0_KTGT - 1500.0_KTGT
             ydef(cp_high) = -642050.0_KTGT  + 1500.0_KTGT
          case('AA')
             xdef(cp_low)  = -3040000.0_KTGT
             xdef(cp_high) = +3040000.0_KTGT
             xdef(cp_shift) = -0.5_KTGT
             ydef(cp_low)  = -3040000.0_KTGT
             ydef(cp_high) = +3040000.0_KTGT
             ydef(cp_shift) = -0.5_KTGT
          case('SP')
             xdef(cp_low)  = -1024000.0_KTGT
             xdef(cp_high) = +1024000.0_KTGT
             ydef(cp_low)  = -1024000.0_KTGT
             ydef(cp_high) = +1024000.0_KTGT
          end select
       endif
       if (ierr.eq.0) then
          if (aitem(2).ne.' ') then
             call parse_number(ierr, sp, aitem(2))
             if (ierr.eq.0) then
                xdef(cp_spacing) = sp
                ydef(cp_spacing) = sp
             endif
          endif
       endif
       if (ierr.eq.0) then
          if (aitem(3).ne.' ') then
             call parse_number(ierr, sh, aitem(3))
             if (ierr.eq.0) then
                xdef(cp_shift) = sh
                ydef(cp_shift) = sh
             endif
          endif
       endif
    endif
    if (ierr.eq.0) then
       call parse_ps_coordinate(ierr, mx, xprop, xname, xdef, 'X', xtag, cf)
    endif
    if (ierr.eq.0) then
       call parse_ps_coordinate(ierr, my, yprop, yname, ydef, 'Y', ytag, cf)
    endif
  end subroutine parse_psplane

!!!_  - parse_ps_coordinate
  subroutine parse_ps_coordinate &
       & (ierr, m, prop, name, defp, defn, atag, cflag)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: m
    real(kind=KTGT), intent(out) :: prop(*)
    character(len=*),intent(out) :: name
    real(kind=KTGT), intent(in)  :: defp(*)
    character(len=*),intent(in)  :: defn
    character(len=*),intent(in)  :: atag
    integer,         intent(out) :: cflag

    integer,parameter   :: larg = 256
    character(len=larg) :: aval
    integer,parameter :: lgrp = 2
    character(len=larg) :: agrp(lgrp)
    character(len=larg) :: aitems(mem_cp)
    integer ji
    integer ngrp, ni
    real(kind=KTGT) :: vbuf
    character(len=lmsg) :: txt

    ierr = 0
    ! [XY]=[NAME/]SPACING[:LOW:HIGH[:ANCHOR[:SHIFT[:OFFSET]]]]

    if (ierr.eq.0) call get_last_option(ierr, aval, atag, def=' ')
    if (ierr.eq.0) then
       agrp(:) = ' '
       call split_list(ngrp, agrp, aval, sep_name, lgrp)
       ierr = min(0, ngrp)
    endif
    if (ierr.eq.0) then
       aitems(:) = ' '
       ni = ERR_PANIC
       if (ngrp.eq.1) then
          call split_list(ni, aitems, agrp(1), sep_item, mem_cp)
          call parse_number(ierr, vbuf, aitems(1))
          if (ierr.ne.0) then
             ierr = 0
             aitems(:) = ' '
          else
             agrp(1) = ' '
          endif
       else if (ngrp.eq.2) then
          call split_list(ni, aitems, agrp(2), sep_item, mem_cp)
       else if (ngrp.eq.0) then
          ni = 0
          aitems(:) = ' '
          agrp(1) = ' '
       endif
       ierr = min(ni, 0)
    endif
    if (ierr.eq.0) then
       name = agrp(1)
       if (name.eq.' ') name = defn
    endif
    do ji = 1, mem_cp
       if (ierr.eq.0) call parse_number(ierr, prop(ji), aitems(ji), defp(ji))
    enddo
    if (ierr.eq.0) then
       if (prop(cp_anchor).eq.cp_undef) prop(cp_anchor) = prop(cp_low)
       if (prop(cp_shift).eq.cp_undef) prop(cp_shift) = ZERO
       if (ANY(prop(1:mem_cp).eq.cp_undef)) ierr = ERR_INVALID_PARAMETER
    endif
    if (ierr.eq.0) then
       call adjust_coordinate &
            & (ierr, m, prop(cp_low), prop(cp_high), &
            &  prop(cp_spacing), prop(cp_anchor), prop(cp_shift), atag)
    endif
    if (ierr.eq.0) then
       if (cflag.eq.cflag_boundary) m = m - 1
    endif
    if (ierr.ne.0) then
109    format('cannot parse plane coordinate argument ', A, ' = ', A)
       write(txt, 109) trim(atag), trim(aval)
       call message(ierr, txt)
    endif
    if (ierr.eq.0) then
106    format('plane coordinate ', A, ' = ', '[', A, '] ', I0, &
            & 1x, F12.3, 1x, F12.3, 1x, F9.1, 1x, F12.3)
       if (is_verbose(msglev_NORMAL)) then
          write(txt, 106) trim(atag), trim(name), m, prop([cp_low, cp_high, cp_spacing, cp_shift])
          call message(ierr, txt)
       endif
    endif
  end subroutine parse_ps_coordinate

!!!_  - adjust_coordinate
  subroutine adjust_coordinate &
       & (ierr, m, cl, ch, dc, co, sh, tag)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: m
    real(kind=KTGT), intent(inout) :: cl, ch
    real(kind=KTGT), intent(in)    :: dc, co, sh
    character(len=*),intent(in)    :: tag

    integer ml, mh
    real(kind=KTGT) :: zl, zh

    ierr = 0

    ml = CEILING((co - cl) / dc)
    mh = CEILING((ch - co) / dc)

    zl = co - (real(ml, kind=KTGT) - sh) * dc
    zh = co + (real(mh, kind=KTGT) - sh) * dc

    if (zl.ne.cl .or. zh.ne.ch) then
219    format('extended domain:', A, ': ', 2F12.1, ' >> ', 2F12.1)
       write(*, 219) trim(tag), cl, ch, zl, zh
    endif

    cl = zl
    ch = zh
    ! m = ml + mh
    m = INT((zh - zl) / dc) + 1
  end subroutine adjust_coordinate

!!!_   . parse_prprop_params
  subroutine parse_psprop_params &
       & (ierr, levp, tol)
    implicit none
    integer,        intent(out) :: ierr
    integer,        intent(out) :: levp(*)
    real(kind=KTGT),intent(out) :: tol

    character(len=*),parameter :: ptag = 'LEV'
    character(len=*),parameter :: ttag = 'TOL'

    integer,parameter   :: larg = 128
    character(len=larg) :: aval
    character(len=larg) :: aitems(llev)
    integer ni
    character(len=lmsg) :: txt

    ierr = 0
    ni = -1
    if (ierr.eq.0) call get_last_option(ierr, aval, ptag, def=' ')
    if (ierr.eq.0) then
       aitems(:) = ' '
       call split_list(ni, aitems, aval, sep_item, llev)
       ierr = min(0, ni)
    endif
    if (ni.eq.0) then
       aitems(1) = '0'
       aitems(2) = '0'
       ni = ni + 2
    else if (ni.eq.1) then
       if (ierr.eq.0) then
          aitems(2) = aitems(1)
          aitems(1) = '0'
          ni = ni + 1
       endif
    endif
    if (ni.eq.2) then
       if (ierr.eq.0) call parse_number(ierr, levp(lev_ini), aitems(1), 0)
       if (ierr.eq.0) call parse_number(ierr, levp(lev_max), aitems(2), 0)
       if (ierr.eq.0) then
          if (levp(lev_max).le.0) levp(lev_max) = 7
          levp(lev_ini) = max(0, min(levp(lev_ini), levp(lev_max) - 1))
       endif
    else
       if (ierr.eq.0) ierr = ERR_INVALID_PARAMETER
    endif

    if (ierr.eq.0) call get_last_option(ierr, tol, ttag, def=ZERO)

    if (ierr.eq.0) then
       if (is_verbose(msglev_NORMAL)) then
108       format('levels = ', 4(1x, I0))
          write(txt, 108) levp(1:llev)
          call message(ierr, txt)
       endif
    endif
  end subroutine parse_psprop_params
!!!_ + end module convoy_psprop
end module convoy_psprop
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

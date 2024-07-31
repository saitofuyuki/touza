!!!_! convoy_ps2g.F90 - TOUZA/Jmz/convoy polar sterographic/geographic transform
! Maintainer: SAITO Fuyuki
! Created: Apr 10 2024
#define TIME_STAMP 'Time-stamp: <2024/07/15 19:08:49 fuyuki convoy_ps2g.F90>'
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
#define _PFX 'convoy:ps2g: '
!!!_@ TOUZA/Jmz/convoy_ps2g - polar sterographic/geographic transform
module convoy_ps2g
!!!_ + Declaration
!!!_  - modules
  use Jmz_param
  use Jmz_base
  use Jmz_geogr
  use convoy_util
  implicit none
  public
!!!_  - ps2g conversion properties
  integer,parameter :: pxlev_lat = 1
  integer,parameter :: pxlev_lon = 2
  integer,parameter :: pxlev_ini = 3
  integer,parameter :: pxlev_switch = 4
  integer,parameter :: lpx = 4
!!!_  - other options
  integer,parameter :: ps2g_opt_sort = 0
  integer,parameter :: ps2g_nopts = 1
!!!_ + Body
contains
!!!_  & main_ps2g
  subroutine main_ps2g &
       & (ierr, jpos, npos)
    use Jmz_psprop
    use TOUZA_Emu,only: ncache_psgp_co
    use TOUZA_Nio,only: nio_record_std
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jpos
    integer,intent(in)    :: npos

    integer :: mx, my
    real(kind=KTGT) :: xprop(mem_cp), yprop(mem_cp)
    character(len=lname) :: xname, yname

    real(kind=KTGT) :: clond     ! projection center lon [deg]
    real(kind=KTGT) :: major, flatten   ! semi-major radius [m], flattening
    real(kind=KTGT) :: cco(ncache_psgp_co)

    integer :: mlat,      mlon
    integer :: latdiv,    londiv
    integer :: latpad(2), lonpad(2)   ! dummy
    real(kind=KTGT)  :: latofs,  lonofs
    character(len=lname) :: latname, lonname
    character(len=4) :: lattype, lontype

    integer :: levps2g(lpx)
    integer :: lbuf
    real(kind=KTGT) :: tol

    integer jlatb, jlate
    integer jlonb, jlone

    integer :: ufile
    character(len=lpath) :: ofile
    character(len=litem) :: head(nitem)
    integer :: krect
    integer :: kopts(0:ps2g_nopts-1)
    character(len=*),parameter :: proc='ps2g'

    ierr = 0
    if (ierr.eq.0) call push_basename(ierr, 'ps2g')

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
          call open_write(ierr, ufile, ofile, cfmt_gtool_seq)
          if (ierr.eq.0) call get_default_header(head)
       endif
    endif

    if (ierr.eq.0) call parse_ellipsoid(ierr, major=major, flatten=flatten)
    if (ierr.eq.0) call parse_psgp(ierr, cco, flatten, major, clon=clond)

    if (ierr.eq.0) then
       call reset_coor_props(xprop, xname)
       call reset_coor_props(yprop, yname)
    endif

    if (ierr.eq.0) call parse_coor_org(ierr, xprop, yprop, cco, def=' ')
    if (ierr.eq.0) call parse_coor_ps(ierr, xprop, yprop, xname, yname, def=' ')
    if (ierr.eq.0) call parse_coordinate(ierr, mx, xprop, xname, 'X', def=' ')
    if (ierr.eq.0) call parse_coordinate(ierr, my, yprop, yname, 'Y', def=' ')
    if (ierr.eq.0) then
       ! transformed into the number of cells, not boundaries
       mx = mx - 1
       my = my - 1
    endif

    if (ierr.eq.0) then
       call parse_geogr_atmos_base &
            & (ierr, &
            &  mlat, latdiv, latpad, latofs, latname, lattype, &
            &  mlon, londiv, lonpad, lonofs, lonname, lontype)
    endif
    if (ierr.eq.0) then
       call set_check_atmos &
            & (ierr, &
            &  mlat, latdiv, latpad, latofs, latname, lattype, &
            &  mlon, londiv, lonpad, lonofs, lonname, lontype)
    endif

    if (ierr.eq.0) then
       call parse_ps2g_params &
            & (ierr, levps2g, tol, lbuf, jlatb, jlate, jlonb, jlone)
    endif
    if (ierr.eq.0) then
       call parse_ps2g_opts (ierr, kopts)
    endif
    if (ierr.eq.0) then
       call batch_ps2g &
            & (ierr,  jpos,   npos,    &
            &  mx,    xprop,  xname,   &
            &  my,    yprop,  yname,   &
            &  mlat,  latdiv, latofs,  lattype, latname, jlatb, jlate, &
            &  mlon,  londiv, lonofs,  lontype, lonname, jlonb, jlone, &
            &  clond, cco,    levps2g, tol,     lbuf,    kopts, &
            &  ufile, head,   krect)
    endif
    call trace_err(ierr, fun=proc)
    if (ierr.eq.0) call pop_basename(ierr, 'ps2g')
  end subroutine main_ps2g
!!!_  & batch_ps2g
  subroutine batch_ps2g &
       & (ierr,  jpos,   npos,    &
       &  mx,    xprop,  xname,   &
       &  my,    yprop,  yname,   &
       &  mlat,  latdiv, latofs,  lattype, latname, jlatb, jlate, &
       &  mlon,  londiv, lonofs,  lontype, lonname, jlonb, jlone, &
       &  clond, cco,    levps2g, tol,     lbuf,    kopts, &
       &  ufile, head,   krect)
    use TOUZA_Ami,only: at_init, at_diag, at_finalize
    use TOUZA_Ami,only: symm_ps2g_map
    use TOUZA_Ami,only: ps2g_no_switch, ps2g_fast_fallback
    use TOUZA_Ami,only: ps2g_weights, ps2g_w0, ps2g_w1flo, ps2g_w1fla
    use TOUZA_Emu,only: round_degree, round_2pi
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jpos
    integer,intent(in)    :: npos

    integer,         intent(in) :: mx,       my
    real(kind=KTGT), intent(in) :: xprop(*), yprop(*)
    character(len=*),intent(in) :: xname,    yname

    integer,         intent(in) :: mlat,     mlon
    integer,         intent(in) :: latdiv,   londiv
    real(kind=KTGT), intent(in) :: latofs,   lonofs
    character(len=*),intent(in) :: latname,  lonname
    character(len=*),intent(in) :: lattype,  lontype
    integer,         intent(in) :: jlatb,    jlate    ! user-specified range
    integer,         intent(in) :: jlonb,    jlone    ! in terms of base coordinate

    real(kind=KTGT), intent(in) :: clond    ! projection center lon [deg]
    real(kind=KTGT), intent(in) :: cco(*)

    integer,         intent(in) :: levps2g(*)
    real(kind=KTGT), intent(in) :: tol
    integer,         intent(in) :: lbuf
    integer,         intent(in) :: ufile
    character(len=*),intent(inout) :: head(*)
    integer,         intent(inout) :: krect
    integer,         intent(in) :: kopts(0:*)

    integer,        allocatable :: iofs(:), iprj(:)
    real(kind=KTGT),allocatable :: wprj(:, :)

    ! base coordinate (geographic)
    real(kind=KTGT),allocatable :: glat(:), gwlat(:), glatm(:)
    real(kind=KTGT),allocatable :: glon(:), gwlon(:), glonm(:)
    integer jglatb, jglate, nglat, mglat
    integer jglonb, jglone, nglon, mglon

    real(kind=KTGT) :: xl, dx, yl, dy
    integer lonlev, latlev, inilev, swlev, reqlev

    integer ngg
    integer nmem, lmem

    ! integer jq, j, jb, je
    real(kind=KTGT) :: loround, laround
    ! logical deg(NGEOG)
    character(len=lmsg) :: txt

    ierr = 0
    call at_init(ierr, levv=lev_debug)

    laround = real(round_2pi, kind=KTGT)
    loround = real(round_degree, kind=KTGT)
    ! deg(JLATI)  = .FALSE.
    ! deg(JLONGI) = .TRUE.

    if (ierr.eq.0) then
       mglat = mlat * latdiv
       mglon = mlon * londiv
       ! lwlon = mglon + lwedge
       allocate (gwlat(0:mglat-1), glatm(0:mglat),   glat(0:mglat-1),  &
            &    gwlon(0:mglon),   glonm(0:mglon+1), glon(0:mglon),  &
            &    STAT=ierr)
    endif
            ! &    wwlon(0:lwlon-1), wglon(0:lwlon), &
    if (ierr.eq.0) then
       call geog_latitude &
            & (ierr, glat,   gwlat,  glatm,  mlat, latdiv, latofs, lattype)
    endif
    if (ierr.eq.0) then
       call geog_longitude &
            & (ierr, glon,   gwlon,  glonm,  mlon, londiv, lonofs, lontype)
    endif
    if (ierr.eq.0) then
       call adjust_range(jglatb, jglate, jlatb, jlate, mglat)
       call adjust_range(jglonb, jglone, jlonb, jlone, mglon)
       nglat = jglate - jglatb
       nglon = jglone - jglonb
    endif
    if (ierr.eq.0) then
       xl = xprop(cp_low)
       dx = xprop(cp_spacing)
       yl = yprop(cp_low)
       dy = yprop(cp_spacing)

       lonlev = levps2g(pxlev_lon)
       latlev = levps2g(pxlev_lat)
       inilev = levps2g(pxlev_ini)
       swlev  = levps2g(pxlev_switch)
    endif
    if (ierr.eq.0) then
       ngg = nglat * nglon
       allocate (iofs(0:ngg), STAT=ierr)
    endif
    if (ierr.eq.0) then
       lmem = lbuf
       if (lmem.le.0) then
          ! dry-run, to estimate lmem
          allocate (iprj(0:1), wprj(0:ps2g_weights-1, 0:1), STAT=ierr)
          if (ierr.eq.0) then
             call symm_ps2g_map &
                  & (ierr,   iofs,   iprj,   wprj,   &
                  &  nmem,   0,      &
                  &  glatm,  gwlat,  jglatb, jglate, nglat,  &
                  &  glonm,  gwlon,  jglonb, jglone, mglon,  &
                  &  xl,     dx,     mx,     yl,     dy,     my,   &
                  &  cco,    &
                  &  lonlev, latlev, inilev, ps2g_fast_fallback, reqlev, tol,   &
                  &  loround,laround,levv=lev_verbose)
          endif
          if (ierr.eq.0) then
             lmem = nmem + mglat * mglon ! for safety
             ! lmem = nmem + ngg ! for safety
             deallocate (iprj, wprj, STAT=ierr)
          endif
          if (is_verbose(msglev_NORMAL)) then
104          format('dry-run: size ', I0, 1x, I0)
             write(txt, 104) nmem, lmem
             call message(ierr, txt)
          endif
          if (ierr.eq.0) then
105          format('dry-run: posibble minimum level ', I0)
             write(txt, 105) reqlev
             call message(ierr, txt)
             if (reqlev.gt.max(lonlev, latlev)) then
                ierr = ERR_INVALID_PARAMETER
                call message(ierr, 'not enough level.')
             endif
          endif
       else if (lmem.eq.0) then
          lmem = ngg * 32
       endif
    endif
    if (ierr.eq.0) then
       allocate (iprj(0:lmem-1), wprj(0:ps2g_weights-1, 0:lmem-1), &
            &    STAT=ierr)
    endif
    if (ierr.eq.0) then
       call symm_ps2g_map &
            & (ierr,   iofs,   iprj,   wprj,   &
            &  nmem,   lmem,   &
            &  glatm,  gwlat,  jglatb, jglate, nglat,  &
            &  glonm,  gwlon,  jglonb, jglone, mglon,  &
            &  xl,     dx,     mx,     yl,     dy,     my,   &
            &  cco,    &
            &  lonlev, latlev, inilev, swlev,  reqlev, tol,   &
            &  loround,laround,levv=lev_verbose)
    endif
    if (ierr.eq.0) then
       if (nmem.gt.lmem) ierr = ERR_INSUFFICIENT_BUFFER
    endif
    if (ierr.eq.0) then
       ! write(*, *) wprj(ps2g_w1flo, 0:nmem-1)
       call ps2g_output &
            & (ierr,   jpos,  npos,  &
            &  iofs,   iprj,  wprj,  nmem,   lmem,   &
            &  glat,   glatm, gwlat, jglatb, jglate, mglat, latname, laround, &
            &  glon,   glonm, gwlon, jglonb, jglone, mglon, lonname, loround, &
            &  mx,     my,    xname, yname,  cco,    kopts,   &
            &  ufile,  head,  krect)
    endif

    if (ierr.eq.0) deallocate(glat,  gwlat, glatm, glon, gwlon, glonm, STAT=ierr)

    if (ierr.eq.0) call at_diag(ierr)
    if (ierr.eq.0) call at_finalize(ierr)

    return
  end subroutine batch_ps2g
!!!_  & ps2g_output
  subroutine ps2g_output &
       & (ierr,   jpos,   npos,  &
       &  iofs,   iprj,   wprj,  nmem,   lmem,  &
       &  glat,   glatm,  gwlat, jglatb, jglate, mglat, latname, laround, &
       &  glon,   glonm,  gwlon, jglonb, jglone, mglon, lonname, loround, &
       &  mx,     my,     xname, yname,  cco,    kopts, &
       &  ufile,  head,   krect)
    use TOUZA_Ami,only: ps2g_invert_table
    use TOUZA_Ami,only: ps2g_weights, ps2g_w0, ps2g_w1flo, ps2g_w1fla
    use TOUZA_Ami,only: ps2g_w1blo, ps2g_w1bla
    use TOUZA_Nio,only: axis_set_header, axis_loc, axis_wgt, axis_cyclic
    use TOUZA_Nio,only: show_header
    use TOUZA_Emu,only: deg2rad, ang2deg, ang2rad
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: jpos
    integer,         intent(in)    :: npos
    integer,         intent(in)    :: lmem
    integer,         intent(in)    :: iofs(0:*)
    integer,         intent(in)    :: iprj(0:*)
    real(kind=KTGT), intent(in)    :: wprj(0:ps2g_weights-1, 0:*)
    ! real(kind=KTGT), intent(in)    :: wpla(0:*), wplo(0:*)
    integer,         intent(in)    :: nmem
    real(kind=KTGT), intent(in)    :: glat(0:*), glatm(0:*), gwlat(0:*)
    real(kind=KTGT), intent(in)    :: glon(0:*), glonm(0:*), gwlon(0:*)
    integer,         intent(in)    :: jglatb, jglate, mglat
    integer,         intent(in)    :: jglonb, jglone, mglon
    character(len=*),intent(in)    :: latname,lonname
    real(kind=KTGT), intent(in)    :: laround,loround
    integer,         intent(in)    :: mx,     my
    character(len=*),intent(in)    :: xname,  yname
    real(kind=KTGT), intent(in)    :: cco(*)
    ! logical,         intent(in)    :: deg(*)
    integer,         intent(in)    :: kopts(0:*)
    integer,         intent(in)    :: ufile
    character(len=*),intent(inout) :: head(*)
    integer,         intent(inout) :: krect

    integer,parameter :: larg = 256
    character(len=larg) :: atxt, utxt
    integer jitem
    integer mh, mv
    integer ng
    integer ksort
    integer kbatch, kitem

    integer,parameter :: var_plane_wsum = 0
    integer,parameter :: var_plane_wrem = 1

    integer,parameter :: var_plane_jbwd = 2
    integer,parameter :: var_plane_wbwd = 3
    integer,parameter :: var_plane_w2bwd = 4
    integer,parameter :: var_plane_w3bwd = 5
    integer,parameter :: var_plane_nbwd = 6

    integer,parameter :: var_geogr_jfwd = 7
    integer,parameter :: var_geogr_wfwd = 8
    integer,parameter :: var_geogr_w2fwd = 9
    integer,parameter :: var_geogr_w3fwd = 10
    ! integer,parameter :: var_geogr_clat = 11
    ! integer,parameter :: var_geogr_clon = 12
    integer,parameter :: var_geogr_nfwd = 11

    integer,parameter :: var_gcoor_lon = 12
    integer,parameter :: var_gcoor_lat = 13
    integer,parameter :: var_gcoor_wlon = 14
    integer,parameter :: var_gcoor_wlat = 15

    integer,parameter :: nvar = 16

    integer,parameter :: batch_all  = 1
    integer,parameter :: batch_std  = 2
    integer,parameter :: batch_gcoor = 3
    integer,parameter :: mbatch = 3

    integer,parameter :: lbv = 8
    character(len=lbv),save :: vname(0:nvar-1) = ' '
    character(len=lbv),save :: bname(0:mbatch) = ' '
    integer,save :: vmask(0:nvar-1) = -1
    integer,save :: bmask(0:mbatch) = -1

    ! real(kind=KTGT),allocatable :: wsum(:, :), wrem(:, :)
    real(kind=KTGT),allocatable :: wsum(:), wrem(:)
    real(kind=KTGT),allocatable :: wbwd(:, :)
    integer,        allocatable :: jbwd(:), bofs(:)
    character(len=lmsg) :: txt
    character(len=litem) :: ugeog
    real(kind=KTGT),allocatable :: gbuf(:)

    ierr = 0
    ksort = kopts(ps2g_opt_sort)
    ugeog = 'degree'

    if (bmask(0).lt.0) then
       bmask(batch_null)  = 0
       bmask(batch_all)   = 1
       bmask(batch_std)   = 1 + 2
       bmask(batch_gcoor) = 1 + 4

       bname(batch_null)  = ' '
       bname(batch_all)   = 'ALL'
       bname(batch_std)   = 'STD'
       bname(batch_gcoor) = 'GCO'

       vmask(:) = bmask(batch_all)
       vmask((/var_plane_wsum, var_plane_jbwd, &
            &  var_plane_wbwd, &
            &  var_geogr_jfwd, var_geogr_wfwd/)) = bmask(batch_std)

       vmask((/var_gcoor_lon,  var_gcoor_lat, &
            &  var_gcoor_wlon, var_gcoor_wlat/)) = bmask(batch_gcoor)

       vname(var_plane_wsum) = 'WSUM'
       vname(var_plane_wrem) = 'WREM'
       vname(var_plane_jbwd) = 'JBWD'
       vname(var_plane_wbwd) = 'WBWD'
       vname(var_plane_w2bwd) = 'W2BWD'
       vname(var_plane_w3bwd) = 'W3BWD'
       vname(var_plane_nbwd) = 'NBWD'
       vname(var_geogr_jfwd) = 'JFWD'
       vname(var_geogr_wfwd) = 'WFWD'
       vname(var_geogr_w2fwd) = 'W2FWD'
       vname(var_geogr_w3fwd) = 'W3FWD'
       ! vname(var_geogr_clat) = 'CLAT'
       ! vname(var_geogr_clon) = 'CLON'
       vname(var_geogr_nfwd) = 'NFWD'
       vname(var_gcoor_lon) = 'LON'
       vname(var_gcoor_lat) = 'LAT'
       vname(var_gcoor_wlon) = 'WLON'
       vname(var_gcoor_wlat) = 'WLAT'
    endif

    mh = mx * my
    ng = (jglate - jglatb) * (jglone - jglonb)

    jitem = -1

    if (jpos.gt.npos) then
       kbatch = batch_std
       kitem = 0
    else
       kbatch = batch_null
       kitem = -1
    endif

    if (ierr.eq.0) call put_item(ierr, head, DSET_PS2G, hi_DSET)
    loop_parse: do
       if (kbatch.eq.batch_null) then
          if (jpos.gt.npos) exit
          if (ierr.eq.0) call get_param(ierr, atxt, jpos, ' ')
          if (ierr.eq.0) call upcase(utxt, atxt)
          if (ierr.eq.0) then
             jitem = find_first(vname, utxt)
             if (jitem.lt.0) then
                kbatch = find_first(bname(1:), utxt, offset=1, no=-1)
                if (kbatch.gt.0) then
                   kitem = 0
                else
                   ierr = ERR_INVALID_PARAMETER
                   write(*, *) 'unknown variable/set: ', trim(atxt)
                   exit loop_parse
                endif
             endif
          endif
          jpos = jpos + 1
       endif
       if (kbatch.gt.batch_null) then
          jitem = -1
          do
             if (kitem.ge.nvar) exit
             if (IAND(vmask(kitem), bmask(kbatch)).eq.bmask(kbatch)) then
                jitem = kitem
                exit
             endif
             kitem = kitem + 1
          enddo
          if (jitem.lt.0) then
             kbatch = batch_null
             cycle
          endif
          kitem = jitem + 1
       endif
101    format('write ', I0, 1x, A)
       write(txt, 101) ierr, trim(vname(jitem))
       call message(ierr, txt, levm=msglev_NORMAL)

       select case(jitem)
       case(var_plane_wsum, var_plane_wrem)
          mv = mh * ps2g_weights
          if (.not.allocated(wsum)) then
             ! allocate(wsum(0:mh-1, 0:ps2g_weights-1), &
             !      &   wrem(0:mh-1, 0:ps2g_weights-1), STAT=ierr)
             allocate(wsum(mv), wrem(mv), STAT=ierr)
             if (ierr.eq.0) then
                call ps2g_wsum(ierr, wsum, wrem, iprj, wprj, mh, nmem, lmem)
             endif
          endif
          if (ierr.eq.0) then
             call set_header_plane &
                  & (ierr, head, 'UR8', mx, xname, my, yname, ps2g_weights, 'w')
          endif
          select case (jitem)
          case(var_plane_wsum)
             if (ierr.eq.0) call put_item(ierr, head, 'wsum', hi_ITEM)
             if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
             if (ierr.eq.0) call nio_write_data(ierr, wsum, mv, head, krect, ufile)
          case(var_plane_wrem)
             if (ierr.eq.0) call put_item(ierr, head, 'wrem', hi_ITEM)
             if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
             if (ierr.eq.0) call nio_write_data(ierr, wrem, mv, head, krect, ufile)
          end select
       case(var_plane_jbwd, var_plane_wbwd, var_plane_w2bwd, var_plane_w3bwd)
          if (.not.allocated(wbwd)) then
             allocate(bofs(0:mh), jbwd(0:nmem-1), &
                  &   wbwd(0:ps2g_weights-1, 0:nmem-1), STAT=ierr)
             if (ierr.eq.0) then
                call ps2g_invert_table &
                     & (ierr,  &
                     &  bofs,  jbwd, wbwd, mh, nmem,  &
                     &  iofs,  iprj, wprj, ng, lmem,  ksort)
             endif
          endif
          select case (jitem)
          case(var_plane_jbwd)
             if (ierr.eq.0) then
                call set_header_plane (ierr, head, 'PI4', mx, xname, my, yname)
             endif
             if (ierr.eq.0) call put_item(ierr, head, 'jbwd', hi_ITEM)
             if (ierr.eq.0) call nio_store_csr(ierr, jbwd, bofs, head, ufile, krect)
          case(var_plane_wbwd)
             if (ierr.eq.0) then
                call set_header_plane (ierr, head, 'PR8', mx, xname, my, yname)
             endif
             if (ierr.eq.0) call put_item(ierr, head, 'wbwd', hi_ITEM)
             if (ierr.eq.0) then
                call ps2g_write_csr_wprj &
                     & (ierr, wbwd, ps2g_w0, ps2g_weights, nmem, bofs, head, ufile, krect)
                ! call nio_store_csr &
                !      & (ierr, wbwd(0:nmem-1, ps2g_w0), bofs, head, ufile, krect)
             endif
          case(var_plane_w2bwd)
             if (ierr.eq.0) then
                call set_header_plane (ierr, head, 'PR8', mx, xname, my, yname)
             endif
             if (ierr.eq.0) call put_item(ierr, head, 'w2bwd', hi_ITEM)
             if (ierr.eq.0) then
                call ps2g_write_csr_wprj &
                     & (ierr, wbwd, ps2g_w1bla, ps2g_weights, nmem, bofs, head, ufile, krect)
                ! call nio_store_csr &
                !      & (ierr, wbwd(0:nmem-1, ps2g_w1fla), bofs, head, ufile, krect)
             endif
          case(var_plane_w3bwd)
             if (ierr.eq.0) then
                call set_header_plane (ierr, head, 'PR8', mx, xname, my, yname)
             endif
             if (ierr.eq.0) call put_item(ierr, head, 'w3bwd', hi_ITEM)
             if (ierr.eq.0) then
                call ps2g_write_csr_wprj &
                     & (ierr, wbwd, ps2g_w1blo, ps2g_weights, nmem, bofs, head, ufile, krect)
                ! call nio_store_csr &
                !      & (ierr, wbwd(0:nmem-1, ps2g_w1flo), bofs, head, ufile, krect)
             endif
          end select
       case(var_plane_nbwd)
       case(var_geogr_jfwd)
          call set_header_geogr &
               & (ierr, head, jglatb, jglate, latname, jglonb, jglone, lonname, 'PI4')
          if (ierr.eq.0) call put_item(ierr, head, 'jfwd', hi_ITEM)
          if (ierr.eq.0) call nio_store_csr(ierr, iprj, iofs, head, ufile, krect)
       case(var_geogr_wfwd)
          call set_header_geogr &
               & (ierr, head, jglatb, jglate, latname, jglonb, jglone, lonname, 'PR8')
          if (ierr.eq.0) call put_item(ierr, head, 'wfwd', hi_ITEM)
          if (ierr.eq.0) then
             call ps2g_write_csr_wprj &
                  & (ierr, wprj, ps2g_w0, ps2g_weights, lmem, iofs, head, ufile, krect)
             ! call nio_store_csr &
             !      & (ierr, wprj(0:lmem-1, ps2g_w0), iofs, head, ufile, krect)
          endif
       case(var_geogr_w2fwd)
          call set_header_geogr &
               & (ierr, head, jglatb, jglate, latname, jglonb, jglone, lonname, 'PR8')
          if (ierr.eq.0) call put_item(ierr, head, 'w2fwd', hi_ITEM)
          if (ierr.eq.0) then
             call ps2g_write_csr_wprj &
                  & (ierr, wprj, ps2g_w1fla, ps2g_weights, lmem, iofs, head, ufile, krect)
             ! call nio_store_csr &
             !      & (ierr, wprj(0:lmem-1, ps2g_w1bla), iofs, head, ufile, krect)
          endif
       case(var_geogr_w3fwd)
          call set_header_geogr &
               & (ierr, head, jglatb, jglate, latname, jglonb, jglone, lonname, 'PR8')
          if (ierr.eq.0) call put_item(ierr, head, 'w3fwd', hi_ITEM)
          if (ierr.eq.0) then
             call ps2g_write_csr_wprj &
                  & (ierr, wprj, ps2g_w1flo, ps2g_weights, lmem, iofs, head, ufile, krect)
             ! call nio_store_csr &
             !      & (ierr, wprj(0:lmem-1, ps2g_w1blo), iofs, head, ufile, krect)
          endif
       ! case(var_geogr_clat)
       !    call set_header_geogr &
       !         & (ierr, head, jglatb, jglate, latname, jglonb, jglone, lonname, 'PR8')
       !    if (ierr.eq.0) call put_item(ierr, head, 'clat', hi_ITEM)
       !    if (ierr.eq.0) then
       !       call nio_store_csr &
       !            & (ierr, wprj(0:lmem-1, ps2g_repla), iofs, head, ufile, krect)
       !    endif
       ! case(var_geogr_clon)
       !    call set_header_geogr &
       !         & (ierr, head, jglatb, jglate, latname, jglonb, jglone, lonname, 'PR8')
       !    if (ierr.eq.0) call put_item(ierr, head, 'clon', hi_ITEM)
       !    if (ierr.eq.0) then
       !       call nio_store_csr &
       !            & (ierr, wprj(0:lmem-1, ps2g_replo), iofs, head, ufile, krect)
       !    endif
       case(var_geogr_nfwd)
       case(var_gcoor_lat)
          ! mv = jglate - jglatb
          ! call set_header_geogr &
          !      & (ierr, head, jglatb, jglate, latname, 0, 0, ' ', 'UR8')
          ! if (ierr.eq.0) call put_item(ierr, head, 'lat', hi_ITEM)
          call axis_set_header(ierr, head, latname, mglat, axis_loc)
          if (ierr.eq.0) call put_item(ierr, head, 'UR8', hi_DFMT)
          if (ierr.eq.0) call put_item(ierr, head, ugeog, hi_UNIT)
          if (ierr.eq.0) call alloc_buffer(ierr, mglat)
          if (ierr.eq.0) then
             select case (ugeog)
             case('degree')
                gbuf(0:mglat-1) = ang2deg(glat(0:mglat-1), laround)
             case('radian')
                gbuf(0:mglat-1) = ang2rad(glat(0:mglat-1), laround)
             case default
                gbuf(0:mglat-1) = glat(0:mglat-1)
             end select
          endif
          ! if (deg(JLATI)) then
          !    if (ierr.eq.0) call put_item(ierr, head, 'degree', hi_UNIT)
          ! else
          !    if (ierr.eq.0) call put_item(ierr, head, 'radian', hi_UNIT)
          ! endif
          if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
          if (ierr.eq.0) call nio_write_data(ierr, gbuf, mglat, head, krect, ufile)
       case(var_gcoor_lon)
          ! mv = jglone - jglonb
          ! call set_header_geogr &
          !      & (ierr, head, 0, 0, ' ', jglonb, jglone, lonname, 'UR8')
          ! if (ierr.eq.0) call put_item(ierr, head, 'lon', hi_ITEM)
          call axis_set_header(ierr, head, lonname, mglon, axis_loc + axis_cyclic)
          if (ierr.eq.0) call put_item(ierr, head, 'UR8', hi_DFMT)
          if (ierr.eq.0) call put_item(ierr, head, ugeog, hi_UNIT)
          if (ierr.eq.0) call alloc_buffer(ierr, mglon+1)
          if (ierr.eq.0) then
             select case (ugeog)
             case('degree')
                gbuf(0:mglon) = ang2deg(glon(0:mglon), laround)
             case('radian')
                gbuf(0:mglon) = ang2rad(glon(0:mglon), laround)
             case default
                gbuf(0:mglon) = glon(0:mglon)
             end select
          endif
          ! if (deg(JLONGI)) then
          !    if (ierr.eq.0) call put_item(ierr, head, 'degree', hi_UNIT)
          ! else
          !    if (ierr.eq.0) call put_item(ierr, head, 'radian', hi_UNIT)
          ! endif
          if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
          if (ierr.eq.0) call nio_write_data(ierr, gbuf, mglon+1, head, krect, ufile)
       case(var_gcoor_wlat)
          ! call set_header_geogr &
          !      & (ierr, head, jglatb, jglate, latname, 0, 0, ' ', 'UR8')
          ! if (ierr.eq.0) call put_item(ierr, head, 'wlat', hi_ITEM)
          call axis_set_header(ierr, head, latname, mglat, axis_wgt)
          if (ierr.eq.0) call put_item(ierr, head, 'UR8', hi_DFMT)
          if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
          if (ierr.eq.0) call nio_write_data(ierr, gwlat, mglat, head, krect, ufile)
       case(var_gcoor_wlon)
          ! mv = jglone - jglonb
          ! call set_header_geogr &
          !      & (ierr, head, 0, 0, ' ', jglonb, jglone, lonname, 'UR8')
          ! if (ierr.eq.0) call put_item(ierr, head, 'wlon', hi_ITEM)
          call axis_set_header(ierr, head, lonname, mglon, axis_wgt + axis_cyclic)
          if (ierr.eq.0) call put_item(ierr, head, 'UR8', hi_DFMT)
          if (ierr.eq.0) call put_item(ierr, head, 'degree', hi_UNIT)
          ! if (ierr.eq.0) call show_header(ierr, head)
          if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
          if (ierr.eq.0) call nio_write_data(ierr, gwlon, mglon+1, head, krect, ufile)
       case default
       end select
       if (ierr.ne.0) exit
    enddo loop_parse

    if (ierr.eq.0) then
       if (allocated(wsum)) deallocate(wsum, wrem, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (allocated(bofs)) deallocate(bofs, jbwd, wbwd, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (allocated(gbuf)) deallocate(gbuf, STAT=ierr)
    endif
    return
  contains
    subroutine alloc_buffer(ierr, mem)
      implicit none
      integer,intent(out) :: ierr
      integer,intent(in)  :: mem
      integer n
      if (allocated(gbuf)) then
         n = size(gbuf)
         if (n.lt.mem) then
            deallocate(gbuf, STAT=ierr)
            if (ierr.eq.0) allocate(gbuf(0:mem-1), STAT=ierr)
         endif
      else
         allocate(gbuf(0:mem-1), STAT=ierr)
      endif
    end subroutine alloc_buffer
  end subroutine ps2g_output
!!!_  & ps2g_write_csr_wprj
  subroutine ps2g_write_csr_wprj &
       & (ierr, wprj, jw, lw, lmem, iofs, head, ufile, krect)
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(in)    :: lw, lmem
    integer,         intent(in)    :: jw
    integer,         intent(in)    :: iofs(0:*)
    real(kind=KTGT), intent(in)    :: wprj(0:lw-1, 0:*)
    integer,         intent(in)    :: ufile
    character(len=*),intent(inout) :: head(*)
    integer,         intent(inout) :: krect

    real(kind=KTGT) :: w(0:lmem-1)

    if (ierr.eq.0) then
       w(0:lmem-1) = wprj(jw, 0:lmem-1)
       call nio_store_csr &
            & (ierr, w, iofs, head, ufile, krect)
    endif
  end subroutine ps2g_write_csr_wprj
!!!_  & ps2g_wsum
  subroutine ps2g_wsum &
       & (ierr, wsum, wrem, iprj, wprj, mh, nmem, lmem)
    use TOUZA_Ami,only: ps2g_weights
    implicit none
    integer,        intent(out) :: ierr
    integer,        intent(in)  :: lmem
    integer,        intent(in)  :: mh, nmem
    real(kind=KTGT),intent(out) :: wsum(0:mh-1, 0:*), wrem(0:mh-1, 0:*)
    integer,        intent(in)  :: iprj(0:*)
    real(kind=KTGT),intent(in)  :: wprj(0:ps2g_weights-1, 0:*)
    integer jmem, jh
    real(kind=KTGT) :: ty(0:ps2g_weights-1), tt(0:ps2g_weights-1)
    ierr = 0
    wsum(0:mh-1, 0:ps2g_weights-1) = 0.0_KTGT
    wrem(0:mh-1, 0:ps2g_weights-1) = 0.0_KTGT
    do jmem = 0, nmem - 1
       jh = iprj(jmem)
       if (jh.ge.0.and.jh.lt.mh) then
          ty(0:ps2g_weights-1) = wprj(0:ps2g_weights-1, jmem) - wrem(jh, 0:ps2g_weights-1)
          tt(0:ps2g_weights-1) = wsum(jh,   0:ps2g_weights-1) + ty(0:ps2g_weights-1)
          wrem(jh, 0:ps2g_weights-1) = (tt(0:ps2g_weights-1) - wsum(jh, 0:ps2g_weights-1)) - ty(0:ps2g_weights-1)
          wsum(jh, 0:ps2g_weights-1) = tt(0:ps2g_weights-1)
       endif
    enddo
  end subroutine ps2g_wsum

!!!_  & parse_ps2g_params
  subroutine parse_ps2g_params &
       & (ierr, levp, tol, lbuf, jlatb, jlate, jlonb, jlone)
    use TOUZA_Ami,only: ps2g_auto_init, ps2g_end_switch, ps2g_no_switch
    implicit none
    integer,        intent(out) :: ierr
    integer,        intent(out) :: levp(*)
    real(kind=KTGT),intent(out) :: tol
    integer,        intent(out) :: lbuf
    integer,        intent(out) :: jlatb, jlate
    integer,        intent(out) :: jlonb, jlone

    character(len=*),parameter :: ptag = 'LEV'
    character(len=*),parameter :: ttag = 'TOL'
    character(len=*),parameter :: rtag = 'RANGE'
    character(len=*),parameter :: btag = 'BUF'

    integer,parameter   :: larg = 128
    character(len=larg) :: aval
    character(len=larg) :: aitems(lpx)
    integer,parameter   :: lco=2
    character(len=larg) :: aco(lco)
    integer ni, nco
    character(len=lmsg) :: txt

    ierr = 0
    if (ierr.eq.0) call get_last_option(ierr, aval, ptag, def=' ')
    if (ierr.eq.0) then
       aitems(:) = ' '
       call split_list(ni, aitems, aval, sep_attr, lpx)
       ierr = min(0, ni)
    endif
    if (ierr.eq.0) call parse_number(ierr, levp(pxlev_lat), aitems(pxlev_lat), 0)
    if (ierr.eq.0) call parse_number(ierr, levp(pxlev_lon), aitems(pxlev_lon), 0)
    if (ierr.eq.0) call parse_number(ierr, levp(pxlev_ini), aitems(pxlev_ini), ps2g_auto_init)
    if (ierr.eq.0) then
       if (aitems(pxlev_switch).eq.disable_switch) then
          levp(pxlev_switch) = ps2g_no_switch
       else
          call parse_number(ierr, levp(pxlev_switch), aitems(pxlev_switch), ps2g_end_switch)
       endif
    endif

    if (ierr.eq.0) then
       if (levp(pxlev_lat).le.0) levp(pxlev_lat) = 8
       if (levp(pxlev_lon).le.0) levp(pxlev_lon) = levp(pxlev_lat)
       levp(pxlev_ini) = min(max(0, levp(pxlev_ini)), levp(pxlev_lon), levp(pxlev_lat))
    endif

    if (ierr.eq.0) call get_last_option(ierr, aval, rtag, def=' ')
    if (ierr.eq.0) then
       aco(:) = ' '
       call split_list(nco, aco, aval, sep_attr, lco)
       ierr = min(0, nco)
    endif
    if (ierr.eq.0) call parse_range(ierr, jlatb, jlate, aco(1))
    if (ierr.eq.0) call parse_range(ierr, jlonb, jlone, aco(2))

    if (ierr.eq.0) call get_last_option(ierr, tol, ttag, def=ZERO)

    if (ierr.eq.0) call get_last_option(ierr, lbuf, btag, def=0)

    if (ierr.eq.0) then
       if (is_verbose(msglev_NORMAL)) then
108       format('levels = ', 4(1x, I0))
109       format('range = ', I0, ':', I0, 1x, I0, ':', I0)
          write(txt, 108) levp(1:lpx)
          call message(ierr, txt)
          write(txt, 109) jlatb, jlate, jlonb, jlone
          call message(ierr, txt)
       endif
    endif
  end subroutine parse_ps2g_params
!!!_  & parse_range
  subroutine parse_range &
       & (ierr, jbgn, jend, str)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: jbgn, jend
    character(len=*),intent(in)  :: str
    integer,parameter :: litem = 2
    integer aitem(litem)
    integer def(litem)
    integer ni
    ierr = 0

    def(:) = (/0, -1/)
    aitem(:) = def(:)
    call split_list(ni, aitem, str, sep_range, litem, def=def)
    ierr = min(0, ni)
    if (ierr.eq.0) then
       jbgn = aitem(1)
       jend = aitem(2)
    else
       jbgn = -1
       jend = -1
    endif
  end subroutine parse_range

!!!_  & reset_gprop
  subroutine reset_gprop (m, div, pad, o, name, t)
    implicit none
    integer,         intent(out) :: m
    integer,         intent(out) :: div
    integer,         intent(out) :: pad(*)
    real(kind=KTGT), intent(out) :: o
    character(len=*),intent(out) :: name
    character(len=*),intent(out) :: t
    m = -1
    div = -1
    pad(1:2) = -1
    o = ZERO
    name = ' '
    t = ' '
  end subroutine reset_gprop
!!!_  - parse_ps2g_opts
  subroutine parse_ps2g_opts &
       & (ierr, kopts)
    implicit none
    integer,        intent(out) :: ierr
    integer,        intent(out) :: kopts(0:*)

    character(len=*),parameter :: stag = 'SORT'

    integer,parameter   :: larg = 128
    character(len=larg) :: aval
    character(len=lmsg) :: txt

    ierr = 0
    if (ierr.eq.0) call get_last_option(ierr, aval, stag, def=' ')
    if (ierr.eq.0) then
       if (aval.eq.' ') aval = '+1'
       call parse_number(ierr, kopts(ps2g_opt_sort), aval)
    endif

    if (ierr.eq.0) then
       if (is_verbose(msglev_NORMAL)) then
108       format('sort = ', SP, I0)
109       format('sort = ', I0)
          if (kopts(ps2g_opt_sort).eq.0) then
             write(txt, 109) kopts(ps2g_opt_sort)
          else
             write(txt, 108) kopts(ps2g_opt_sort)
          endif
          call message(ierr, txt)
       endif
    endif
  end subroutine parse_ps2g_opts

!!!_  & parse_convoy_ps2g
  subroutine parse_convoy_ps2g &
       & (ierr, prime, jpos, npos, handle)
    implicit none
    integer,         intent(out)   :: ierr
    type(convoy_t),  intent(inout) :: prime
    integer,         intent(inout) :: jpos
    integer,         intent(in)    :: npos
    integer,         intent(in)    :: handle
    character(len=*),parameter :: proc = 'parse_convoy_ps2g'

    ierr = 0
    if (ierr.eq.0) then
       call parse_oprfile &
            & (ierr, prime, jpos, npos, handle, xcmd_csr, 'jfwd', 'wfwd', 'w2fwd', 'w3fwd')
    endif
    if (ierr.eq.0) then
       call parse_oprfile &
            & (ierr, prime, jpos, npos, handle, xcmd_csr, 'jbwd', 'wbwd', 'w2bwd', 'w3bwd')
    endif
    call trace_err(ierr, fun=proc)
  end subroutine parse_convoy_ps2g

!!!_ + end module convoy_ps2g
end module convoy_ps2g
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

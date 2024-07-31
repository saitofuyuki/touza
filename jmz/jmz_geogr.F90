!!!_! jmz_geogr.F90 - TOUZA/Jmz/geogr geographic(-like) coordinates
! Maintainer: SAITO Fuyuki
! Created: Apr 10 2024
#define TIME_STAMP 'Time-stamp: <2024/07/15 09:48:07 fuyuki jmz_geogr.F90>'
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
#define _PFX 'jmz:geogr: '
!!!_@ TOUZA/Jmz/geogr - geographic(-like) coordinates
module Jmz_geogr
!!!_ + Declaration
!!!_  - modules
  use Jmz_param
  use Jmz_base
  implicit none
  public
!!!_  - parameters
  integer,parameter,private :: KTGT=KRSTD
  character(len=*),parameter,private :: sep_name = sep_rename
!!!_  - geographic spherical domain properties
  character(len=*),parameter :: geo_gauss = 'G'   ! gaussian
  character(len=*),parameter :: geo_eqdis = 'E'   ! equidistant
  character(len=*),parameter :: geo_divsl = 'S'   ! divide with equal sine-latitude
  character(len=*),parameter :: geo_divll = 'L'   ! divide with equal latitude/longitude
  character(len=*),parameter :: geo_desco = 'D'   ! descending order

  character(len=*),parameter :: geo_all = geo_gauss // geo_eqdis // geo_divsl // geo_divll // geo_desco

  integer,parameter :: lpad = 1
  integer,parameter :: rpad = 2
!!!_ + Body
contains
!!!_  & parse_geogr_atmos_base - agcm geographic domain parser
  subroutine parse_geogr_atmos_base &
       & (ierr, &
       &  mlat, latdiv, latpad, latofs, latname, lattype, &
       &  mlon, londiv, lonpad, lonofs, lonname, lontype, &
       &  tag,  def)
    use TOUZA_Std,only: find_next_sep
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: mlat,      mlon
    integer,         intent(out)         :: latdiv,    londiv
    integer,         intent(out)         :: latpad(*), lonpad(*)
    real(kind=KTGT), intent(out)         :: latofs,    lonofs
    character(len=*),intent(out)         :: latname,   lonname
    character(len=*),intent(out)         :: lattype,   lontype
    character(len=*),intent(in),optional :: tag
    character(len=*),intent(in),optional :: def

    character(len=*),parameter :: gtag = 'GEOGR'

    integer,parameter   :: larg = 256
    character(len=larg) :: aval, uval
    character(len=*),parameter :: proc='parse_atmos_geogr_base'

    character(len=1) :: csep
    integer jsep, js, ls

    ! GEOGR|GEO|G=<special>[,[<name>/][<latprop>][,[[<name>/][<lonprop>]]]]
    ! GEOGR|GEO|G=<special>[/[<latprop>][/[<lonprop>]]]

    ierr = 0
    mlat = -1
    mlon = -1

    call reset_geogr_prop(mlat, latdiv, latpad, latofs, latname, lattype)
    call reset_geogr_prop(mlon, londiv, lonpad, lonofs, lonname, lontype)

    if (ierr.eq.0) then
       if (present(tag)) then
          call get_last_option &
               & (ierr, aval, &
               &  tag, tag(1:1), gtag, gtag(1:3), gtag(1:1), def=def)
       else
          call get_last_option &
               & (ierr, aval, gtag, gtag(1:3), gtag(1:1), def=def)
       endif
    endif
    if (ierr.eq.0) then
       ls = max(1, len_trim(aval))
       csep = ' '
       jsep = SCAN(aval, sep_item // sep_name)
       if (jsep.eq.0) then
          jsep = ls + 1
       else
          csep = aval(jsep:jsep)
       endif
       call upcase(uval, aval(1:jsep-1))
       call upcase(uval)
       if (uval(1:1).eq.'T') then
          select case (uval)
          case('T2')
             mlat = 4
             mlon = 8
          case('T5')
             mlat = 8
             mlon = 16
          case('T10')
             mlat = 16
             mlon = 32
          case('T21')
             mlat = 32
             mlon = 64
          case('T42')
             mlat = 64
             mlon = 128
          case('T85')
             mlat = 128
             mlon = 256
          case('T106')
             mlat = 180
             mlon = 360
          case('T213')
             mlat = 320
             mlon = 640
          case default
             ierr = ERR_INVALID_PARAMETER
          end select
       else if (uval.eq.' ') then
          mlat = 0   ! need configuration
          mlon = 0
       else
          ierr = ERR_INVALID_PARAMETER
       endif
       if (ierr.ne.0) then
          write(*, *) 'cannot parse geo-coordinate argument ', &
               & trim(gtag), '=', trim(aval)
       endif
    endif
    if (ierr.eq.0) then
       js = jsep
       jsep = find_next_sep(aval(1:ls), csep, js)
       if (js.lt.jsep) then
          call parse_geogr_coor &
               & (ierr, latdiv, latpad, latofs, latname, lattype, aval(js+1:jsep), mlat)
       endif
    endif
    if (ierr.eq.0) then
       js = jsep + 1
       jsep = find_next_sep(aval(1:ls), csep, js)
       if (js.lt.jsep) then
          call parse_geogr_coor &
               & (ierr, londiv, lonpad, lonofs, lonname, lontype, aval(js+1:jsep), mlon)
       endif
    endif
    call trace_err(ierr, fun=proc)
  end subroutine parse_geogr_atmos_base
!!!_  & parse_geogr_ocean_base - ogcm geographic domain parser
  subroutine parse_geogr_ocean_base &
       & (ierr,  &
       &  ugeof, &
       &  mlat,  latdiv, latpad, latofs, latname, lattype, &
       &  mlon,  londiv, lonpad, lonofs, lonname, lontype, &
       &  tag)
    use TOUZA_Std,only: find_next_sep
    use TOUZA_Ami,only: open_geofile_tripolar
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: ugeof                  ! io/unit, non-negative if GEOfile specified
    integer,         intent(out)         :: mlat,      mlon
    integer,         intent(out)         :: latdiv,    londiv
    integer,         intent(out)         :: latpad(*), lonpad(*)
    real(kind=KTGT), intent(out)         :: latofs,    lonofs
    character(len=*),intent(out)         :: latname,   lonname
    character(len=*),intent(out)         :: lattype,   lontype
    character(len=*),intent(in),optional :: tag

    integer,parameter   :: larg = 256
    character(len=larg) :: gtag
    character(len=larg) :: aval

    character(len=*),parameter :: gftag = 'GF'
    character(len=lpath) :: geofile
    integer gnx, gny1, gny2

    integer jsep, js, ls

    ! GF=GEOFILE[:VARIATION]
    ! OCN|O=[LATPROP][,[LONPROP]]

    ierr = 0
    ugeof = -1

    call choice_a(gtag, ' ', tag)
    if (gtag.eq.' ') gtag = 'OCN'

    call reset_geogr_prop(mlat, latdiv, latpad, latofs, latname, lattype)
    call reset_geogr_prop(mlon, londiv, lonpad, lonofs, lonname, lontype)

    mlat = 0  ! reset to parse
    mlon = 0

    if (ierr.eq.0) then
       call get_last_option(ierr, geofile, gftag, def=' ')
       if (ierr.eq.0) then
          ! VARIATION parser to be implemented
          if (geofile.ne.' ') ugeof = new_unit()
          ierr = min(0, ugeof)
          if (ierr.eq.0) then
             call open_geofile_tripolar &
                  & (ierr, gnx, gny1, gny2, geofile, ugeof)
          endif
          if (ierr.eq.0) then
             mlon = gnx
             mlat = gny1 + gny2
          endif
       endif
    endif

    if (ierr.eq.0) then
       call get_last_option(ierr, aval, gtag, gtag(1:3), gtag(1:1), def=' ')
    endif
    if (ierr.eq.0) then
       ls = max(1, len_trim(aval))
       jsep = 0
       ! no special, at the moment
    endif
    if (ierr.eq.0) then
       js = jsep
       jsep = find_next_sep(aval(1:ls), sep_item, js)
       if (js.lt.jsep) then
          call parse_geogr_coor &
               & (ierr, latdiv, latpad, latofs, latname, lattype, aval(js+1:jsep), mlat)
       endif
       js = jsep + 1
       jsep = find_next_sep(aval(1:ls), sep_item, js)
       if (js.lt.jsep) then
          call parse_geogr_coor &
               & (ierr, londiv, lonpad, lonofs, lonname, lontype, aval(js+1:jsep), mlon)
       endif
    endif
  end subroutine parse_geogr_ocean_base
!!!_  & parse_geogr_coor
  subroutine parse_geogr_coor &
       & (ierr, &
       &  div,  pad, o, name, t, &
       &  str,  m)
    use TOUZA_Std,only: inrange
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: div
    integer,         intent(out)   :: pad(*)
    real(kind=KTGT), intent(out)   :: o
    character(len=*),intent(out)   :: name
    character(len=*),intent(out)   :: t
    character(len=*),intent(in)    :: str
    integer,optional,intent(inout) :: m

    integer c
    integer lstr, jn, js1, js2, ls
    integer lsep

    integer mm
    integer ji, ni
    integer,parameter :: li = 4
    integer,parameter   :: larg = 256
    character(len=larg) :: aitems(li)
    character(len=*),parameter :: proc='parse_geogr_coor'

    !      0   1                        2     3
    ! NAME/NUM:DIV[+PADDING][-PADDING][:FLAG[:OFFSET]]    if m present and == 0
    ! NAME/DIV[+PADDING][-PADDING][:FLAG[:OFFSET]]
    ierr = 0
    lsep = len(sep_name)
    lstr = len_trim(str)
    jn = index(str, sep_name)
    mm = choice(-1, m)

    if (jn.eq.0) then
       c = IACHAR(str(1:1))
       if (inrange(c, iachar('0'), iachar('9'))) then
          jn = 1 - lsep
       else
          jn = lstr + lsep
       endif
    endif
    ! write(*, *) 'name:', str(1:jn-1)
    ! write(*, *) 'prop:', str(jn+lsep:lstr)
    name = str(1:jn-1)
    jn = jn + lsep
    if (jn.le.lstr) then
       aitems(:) = ' '   ! need to reset to allow empty
       call split_list(ni, aitems, str(jn:lstr), sep_item, li)
       ierr = min(0, ni)
       ji = 1
       if (mm.eq.0) then
          if (ierr.eq.0) call parse_number(ierr, m, aitems(ji), 0)
          ji = ji + 1
       endif
       if (ni.ge.ji) then
          ! DIV[+PADDING][-PADDING]
          ls = len_trim(aitems(ji))
          js1 = SCAN(aitems(ji)(1:),     sep_lpad // sep_rpad)
          js2 = SCAN(aitems(ji)(js1+1:), sep_lpad // sep_rpad)
          if (js2.gt.0) then
             js2 = js1 + js2
             if (ierr.eq.0) call parse_padding(ierr, pad, aitems(ji)(js2:))
          else
             js2 = ls + 1
          endif
          if (js1.gt.0) then
             if (ierr.eq.0) call parse_padding(ierr, pad, aitems(ji)(js1:js2-1))
          else
             js1 = ls + 1
          endif
          if (js1.gt.1) then
             if (ierr.eq.0) call parse_number(ierr, div, aitems(ji)(1:js1-1))
          endif
       endif
       ji = ji + 1
       if (ni.ge.ji) then
          ! FLAG
          if (ierr.eq.0) then
             t = aitems(ji)
             call upcase(t)
             if (verify(trim(t), geo_all).ne.0) ierr = ERR_INVALID_PARAMETER
          endif
       endif
       ji = ji + 1
       if (ni.ge.ji) then
          ! OFFSET
          if (ierr.eq.0) call parse_number(ierr, o, aitems(ji), 0.0_KTGT)
       endif
    endif
    call trace_err(ierr, fun=proc)
  end subroutine parse_geogr_coor
!!!_  & parse_padding
  subroutine parse_padding(ierr, pad, str)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: pad(*)
    character(len=*),intent(in)  :: str
    integer v
    ! write(*, *) 'pad:', trim(str)
    call parse_number(ierr, v, str(2:))
    if (ierr.eq.0) then
       select case(str(1:1))
       case(sep_lpad)
          pad(lpad) = v
       case(sep_rpad)
          pad(rpad) = v
       case default
          ierr = ERR_PANIC
       end select
    endif
  end subroutine parse_padding
! !!!_  & parse_range
!   subroutine parse_range &
!        & (ierr, jbgn, jend, str)
!     implicit none
!     integer,         intent(out) :: ierr
!     integer,         intent(out) :: jbgn, jend
!     character(len=*),intent(in)  :: str
!     integer,parameter :: litem = 2
!     integer aitem(litem)
!     integer def(litem)
!     integer ni
!     ierr = 0

!     def(:) = (/0, -1/)
!     aitem(:) = def(:)
!     call split_list(ni, aitem, str, sep_item, litem, def=def)
!     ierr = min(0, ni)
!     if (ierr.eq.0) then
!        jbgn = aitem(1)
!        jend = aitem(2)
!     else
!        jbgn = -1
!        jend = -1
!     endif
!   end subroutine parse_range
!!!_  & geog_latitude
  subroutine geog_latitude &
       & (ierr,  glat, gwlat,  glatm, &
       &  nbase, ndiv, ofs,    tp)
    use TOUZA_Emu,only: gauss_latitude, mid_latitude, round_2pi
    implicit none
    integer,         intent(out) :: ierr
    real(kind=KTGT), intent(out) :: glat(0:*)
    real(kind=KTGT), intent(out) :: gwlat(0:*)
    real(kind=KTGT), intent(out) :: glatm(0:*)
    integer,         intent(in)  :: nbase, ndiv
    real(kind=KTGT), intent(in)  :: ofs
    character(len=*),intent(in)  :: tp

    real(kind=KTGT) :: blat(0:nbase-1)
    real(kind=KTGT) :: wlat(0:nbase-1)
    real(kind=KTGT) :: blatm(0:nbase)

    real(kind=KTGT) :: round = real(round_2pi, kind=KTGT)
    real(kind=KTGT) :: wnml = ONE
    real(kind=KTGT) :: prec = -ONE

    ierr = 0

    if (ndiv.gt.1) then
       write(*, *) 'Not implemented: lat div = ', ndiv
       ierr = ERR_NOT_IMPLEMENTED
    endif

    ! todo:  tp ofs check

    if (ierr.eq.0) call gauss_latitude(ierr, blat, wlat, nbase, round, wnml, prec=prec)
    if (ierr.eq.0) blat(0:nbase-1) = asin(blat(0:nbase-1))
    if (ierr.eq.0) call mid_latitude(ierr, blatm, wlat, nbase)

    ! todo:  division

    if (ierr.eq.0) then
       glat(0:nbase-1)  = blat(0:nbase-1)
       glatm(0:nbase)   = blatm(0:nbase)
       ! wlat from gauss_latitude() is [d sin(lat)] / 2 for historical reason
       gwlat(0:nbase-1) = wlat(0:nbase-1) * TWO
    endif

    return
  end subroutine geog_latitude
!!!_  & geog_longitude
  subroutine geog_longitude &
       & (ierr,  glon, gwlon,  glonm, &
       &  nbase, ndiv, ofs,    tp)
    use TOUZA_Emu,only: get_longitude, mid_longitude, round_degree
    implicit none
    integer,         intent(out) :: ierr
    real(kind=KTGT), intent(out) :: glon(0:*)
    real(kind=KTGT), intent(out) :: gwlon(0:*)
    real(kind=KTGT), intent(out) :: glonm(0:*)
    integer,         intent(in)  :: nbase, ndiv
    real(kind=KTGT), intent(in)  :: ofs
    character(len=*),intent(in)  :: tp

    real(kind=KTGT) :: blon(0:nbase)
    real(kind=KTGT) :: wlon(0:nbase)
    real(kind=KTGT) :: blonm(0:nbase+1)

    real(kind=KTGT) :: round = real(round_degree, kind=KTGT)
    real(kind=KTGT) :: wnml = ZERO

    ierr = 0

    if (ndiv.gt.1) then
       write(*, *) 'Not implemented: lon div = ', ndiv
       ierr = ERR_NOT_IMPLEMENTED
    endif

    ! todo:  tp ofs check

    if (ierr.eq.0) call get_longitude(ierr, blon,  wlon, nbase, round=round, wnml=wnml, org=ofs)
    if (ierr.eq.0) call mid_longitude(ierr, blonm, blon, wlon, nbase)
    ! ! todo:  division

    if (ierr.eq.0) then
       if (INDEX(tp, geo_desco).gt.0) then
          glon(0:nbase)    = blon(nbase:0:-1)
          glonm(0:nbase+1) = blonm(nbase+1:0:-1)
          gwlon(0:nbase)   = wlon(nbase:0:-1)
       else
          glon(0:nbase)    = blon(0:nbase)
          glonm(0:nbase+1) = blonm(0:nbase+1)
          gwlon(0:nbase)   = wlon(0:nbase)
       endif
    endif
    return
  end subroutine geog_longitude
!!!_  & reset_geogr_prop
  subroutine reset_geogr_prop (m, div, pad, o, name, t)
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
  end subroutine reset_geogr_prop
!!!_  & adjust_range
  subroutine adjust_range(jbgnx, jendx, jbgni, jendi, mi)
    implicit none
    integer,intent(out) :: jbgnx, jendx
    integer,intent(in)  :: jbgni, jendi, mi

    jbgnx = jbgni
    jendx = jendi

    if (jbgnx.lt.0) jbgnx = mi + 1 + jbgnx
    if (jbgnx.lt.0) jbgnx = 0

    if (jendx.lt.0) jendx = mi + 1 + jendx
    if (jendx.le.0) jendx = jbgnx + 1
  end subroutine adjust_range
!!!_  & set_check_atmos
  subroutine set_check_atmos &
       & (ierr, &
       &  mlat, latdiv, latpad, latofs, latname, lattype, &
       &  mlon, londiv, lonpad, lonofs, lonname, lontype, &
       &  ijdim)
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: mlat,      mlon
    integer,         intent(inout) :: latdiv,    londiv
    integer,         intent(inout) :: latpad(*), lonpad(*)
    real(kind=KTGT), intent(inout) :: latofs,    lonofs
    character(len=*),intent(inout) :: latname,   lonname
    character(len=*),intent(inout) :: lattype,   lontype
    integer,optional,intent(in)    :: ijdim

    integer dim
    integer m, nlo, nla, k
    character(len=lmsg) :: txt

    ierr = 0
    dim = choice(0, ijdim)
    if (ierr.eq.0) then
       if (mlat.le.0.or.mlon.le.0) ierr = ERR_FEW_ARGUMENTS
    endif
    if (ierr.eq.0) then
       latdiv = max(1, latdiv)
       londiv = max(1, londiv)
       latpad(1:2) = max(0, latpad(1:2))
       lonpad(1:2) = max(0, lonpad(1:2))
       if (dim.gt.0) then
          do k = 0, 1
             nla = mlat + latpad(lpad) + latpad(rpad)
             nlo = mlon + lonpad(lpad) + lonpad(rpad) + k
             m = (nlo * londiv) * (nla * latdiv)
             if (m.eq.dim) then
                lonpad(rpad) = lonpad(rpad) + k
                exit
             endif
          enddo
          if (m.ne.dim) then
             ierr = ERR_FEW_ARGUMENTS
109          format('atmos size mismatch: ijdim seems ', I0)
             write(txt, 109) dim
             call message(ierr, txt, trace=.FALSE.)
          endif
       endif
    endif
    if (ierr.eq.0) then
201    format('GLON', I0)
202    format('GGLA', I0)
211    format('GLON', I0, 'x', I0)
212    format('GGLA', I0, 'x', I0)
       if (lonname.eq.' ') then
          if (londiv.gt.1) then
             write(lonname, 211) mlon, londiv
          else
             write(lonname, 201) mlon
          endif
       endif
       if (latname.eq.' ') then
          if (latdiv.gt.1) then
             write(latname, 212) mlat, latdiv
          else
             write(latname, 202) mlat
          endif
       endif
    endif
    if (ierr.eq.0) then
       if (latpad(lpad).ne.0 .or. latpad(rpad).ne.0 &
            & .or. lonpad(lpad).ne.0) then
          ierr = ERR_PANIC
119       format('cannot handle atmos padding ', 4(1x, I0))
          write(txt, 119) latpad(lpad:rpad), lonpad(lpad:rpad)
          call message(ierr, txt, trace=.FALSE.)
       endif
    endif
    if (ierr.ne.0) then
       call message(ierr, 'Need sufficient ATM domain configuration', trace=.FALSE.)
    endif
107 format('atmos coordinate ', A, ' = ', &
         & '[', A, '] (', I0, '+', I0, '+', I0, ')*', I0, 1x, A, 1x, F12.3)
    if (is_verbose(msglev_NORMAL).or.ierr.lt.0) then
       write(txt, 107) 'lat', trim(latname), mlat, latpad(1:2), latdiv, trim(lattype), latofs
       call message(ierr, txt, trace=.FALSE.)
       write(txt, 107) 'lon', trim(lonname), mlon, lonpad(1:2), londiv, trim(lontype), lonofs
       call message(ierr, txt, trace=.FALSE.)
    endif
  end subroutine set_check_atmos
!!!_  & set_check_ocean
  subroutine set_check_ocean &
       & (ierr, &
       &  mlat, latdiv, latpad, latofs, latname, lattype, &
       &  mlon, londiv, lonpad, lonofs, lonname, lontype, &
       &  nxydim)
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: mlat,      mlon
    integer,         intent(inout) :: latdiv,    londiv
    integer,         intent(inout) :: latpad(*), lonpad(*)
    real(kind=KTGT), intent(inout) :: latofs,    lonofs
    character(len=*),intent(inout) :: latname,   lonname
    character(len=*),intent(inout) :: lattype,   lontype
    integer,optional,intent(in)    :: nxydim

    integer dim
    integer m, nlo, nla, k
    character(len=lmsg) :: txt

    ierr = 0
    dim = choice(0, nxydim)
    if (ierr.eq.0) then
       if (mlat.le.0.or.mlon.le.0) ierr = ERR_FEW_ARGUMENTS
    endif
    if (ierr.eq.0) then
       latdiv = max(1, latdiv)
       londiv = max(1, londiv)
       latpad(1:2) = max(0, latpad(1:2))
       lonpad(1:2) = max(0, lonpad(1:2))
       if (dim.gt.0) then
          do k = 0, 2
             nla = mlat + latpad(lpad) + latpad(rpad) + k * 2
             nlo = mlon + lonpad(lpad) + lonpad(rpad) + k * 2
             m = (nlo * londiv) * (nla * latdiv)
             if (m.eq.dim) then
                lonpad(1:2) = lonpad(1:2) + k
                latpad(1:2) = latpad(1:2) + k
                exit
             endif
          enddo
          if (m.ne.dim) then
             ierr = ERR_FEW_ARGUMENTS
109          format('ocean size mismatch: nxydim seems ', I0)
             write(txt, 109) dim
             call message(ierr, txt, trace=.FALSE.)
          endif
       endif
    endif
    if (ierr.eq.0) then
201    format('OCLONTPT', I0)
202    format('OCLATTPT', I0)
       if (lonname.eq.' ') write(lonname, 201) mlon
       if (latname.eq.' ') write(latname, 202) mlat
    endif
    if (ierr.eq.0) then
       if (latpad(lpad).ne.latpad(rpad) &
            & .or. lonpad(lpad).ne.lonpad(rpad)) then
          ierr = ERR_PANIC
119       format('cannot handle ocean wings ', 4(1x, I0))
          write(txt, 119) latpad(lpad:rpad), lonpad(lpad:rpad)
          call message(ierr, txt, trace=.FALSE.)
       endif
    endif
    if (ierr.ne.0) then
       call message(ierr, 'Need sufficient OCN domain configuration', trace=.FALSE.)
    endif
107 format('ocean coordinate ', A, ' = ', &
         & '[', A, '] (', I0, '+', I0, '+', I0, ')*', I0, 1x, A, 1x, F12.3)
    if (is_verbose(msglev_NORMAL).or.ierr.lt.0) then
       write(txt, 107) 'lat', trim(latname), mlat, latpad(1:2), latdiv, trim(lattype), latofs
       call message(ierr, txt, trace=.FALSE.)
       write(txt, 107) 'lon', trim(lonname), mlon, lonpad(1:2), londiv, trim(lontype), lonofs
       call message(ierr, txt, trace=.FALSE.)
    endif
  end subroutine set_check_ocean
!!!_ + end module Jmz_geogr
end module jmz_geogr
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

!!!_! convoy_psprop.F90 - TOUZA/Jmz/convoy polar sterographic properties
! Maintainer: SAITO Fuyuki
! Created: Apr 10 2024
#define TIME_STAMP 'Time-stamp: <2024/06/22 15:08:06 fuyuki convoy_psprop.F90>'
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
  implicit none
  public
!!!_  - parameters
  integer,parameter,private :: KTGT=KDBL
  integer,parameter :: llev = 2
  integer,parameter :: lev_ini = 1
  integer,parameter :: lev_max = 2

!!!_ + Body
  contains
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

!!!_ + end module convoy_psprop
end module convoy_psprop
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

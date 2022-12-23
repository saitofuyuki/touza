!!!_! emu_ugg.F90 - touza/emu geographic geometry
! Maintainer: SAITO Fuyuki
! Created: Dec 23 2022
#define TIME_STAMP 'Time-stamp: <2023/01/16 17:08:38 fuyuki emu_ugg.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022, 2023
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_emu.h"
#include "touza_std.h"
!!!_@ TOUZA_Emu_ugg - Geometry procedures
module TOUZA_Emu_ugg
!!!_ = declaration
!!!_  - modules
  use TOUZA_Std,only: get_logu,     unit_global,  trace_fine,   trace_control
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
  interface pi_
     module procedure pi_d
  end interface pi_

!!!_  - public
  public init, diag, finalize
  public get_longitude, mid_longitude,  div_longitude
  public check_longitude, check_div_longitude
  public get_latitude,  gauss_latitude, mid_latitude
  public check_precision, is_equidistant
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
!!!_  - diag_pi
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

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE  = 1.0_KTGT
    real(kind=KTGT),parameter :: HALF = 0.5_KTGT
    real(kind=KTGT) :: sp
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
       if (present(span)) then
          sp = span_longitude(span)
          dxs = sp / real(base, kind=KTGT)
          if (is_equidistant(longi_b, base + 1, r=dxs)) then
             beq = .TRUE.
          else
             ierr = ERR_INVALID_PARAMETER
          endif
       endif
    endif

    if (ierr.eq.0) then
       if (beq) then
          dxd(0:base-1) = sp / real(base * div, kind=KTGT)
       else
          do js = 0, base - 1
             dxs = longi_b(js + 1) - longi_b(js)
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
             longi(jd) = longi_b(js)
          else
             o = HALF
             no = 1
          endif
          do js = 0, base - 1
             do ji = 0, div / 2 - no
                jd = js * div + ji
                longi(jd) = longi_b(js) + (real(ji, kind=KTGT) + o) * dxd(js)
             enddo
             do ji = div / 2 - no + 1, div - 1
                jd = js * div + ji
                longi(jd) = longi_b(js + 1) - (real((div - ji), kind=KTGT) - o) * dxd(js)
             enddo
          enddo
          if (mod(div, 2).eq.no) then
             do js = 0, base - 1
                jd = js * div + div / 2
                longi(jd) = longi_c(js)
             enddo
          endif
       case(DIV_EACH_INCR)
          if (boundary) then
             o = ZERO
             js = base
             jd = js * div + 0
             longi(jd) = longi_b(js)
          else
             o = HALF
          endif
          do js = 0, base - 1
             do ji = 0, div - 1
                jd = js * div + ji
                longi(jd) = longi_b(js) + (real(ji, kind=KTGT) + o) * dxd(js)
             enddo
          enddo
       case(DIV_ACCUMULATE)
          if (present(span)) then
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
             longi(jd) = longi_b(js) + d * o
             do jd = 1, no - 1
                longi(jd) = longi(jd - 1) + d
             enddo
          else
             ierr = ERR_INVALID_PARAMETER
          endif
       case default
          ierr = ERR_INVALID_SWITCH
       end select
    endif
    if (ierr.eq.0) deallocate(dxd, STAT=ierr)
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
       ! write(*, *) 'INVALID LATITUDE METHOD = ', method
       ! ierr = ERR_PANIC
    end select
    ! if (ierr.eq.0) then
    !    if (choice(.FALSE., reverse)) then
    !       call array_reverse(lati, n)
    !       call array_reverse(weight, n)
    !    endif
    ! endif

!     if (present(u)) then
!        sumgw = ZERO
!        do j = 1, n
!           sumgw = sumgw + weight(j)
!        enddo
! 101    format(' ### check SUMGW = ', E16.9)
!        if (u.ge.0) then
!           write(u, 101) sumgw
!        else
!           write(*, 101) sumgw
!        endif
!     endif
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
#ifdef TEST_EMU_UGG
program test_emu_ugg
  use TOUZA_Emu_ugg
  use TOUZA_Std,only: KFLT, KDBL
  use TOUZA_Std,only: KTGT=>KDBL
  use TOUZA_Std,only: arg_init, arg_diag, parse, get_option
  implicit none
  integer ierr
  integer nlat
  integer nlon(2)

  real(kind=KDBL) :: PI = ATAN2(0.0_KDBL, -1.0_KDBL)

  ierr = 0
101 format(A, ' = ', I0)

  call init(ierr, u=-1, levv=9, stdv=-1)
  call diag(ierr)

  if (ierr.eq.0) call arg_init(ierr)
  if (ierr.eq.0) call parse(ierr)
  if (ierr.eq.0) call arg_diag(ierr)
  if (ierr.eq.0) call get_option(ierr, nlat, 'lat', -1)
  if (ierr.eq.0) then
     if (nlat.lt.0) nlat = 128
  endif
  if (ierr.eq.0) call get_option(ierr, nlon(:), 'lon', -1)
  if (ierr.eq.0) then
     if (nlon(1).lt.0) nlon(1) = 128
  endif

  if (ierr.eq.0) then
     call test_ugg_prec(ierr)
  endif
  if (ierr.eq.0) then
     if (nlat.gt.0) call test_ugg_lat(ierr, nlat)
  endif
  if (ierr.eq.0) then
     if (nlon(1).gt.0) call test_ugg_lon(ierr, nlon(1), nlon(2))
  endif
  call finalize(ierr)
  write(*, 101) 'fine', ierr
  stop
contains
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
  subroutine test_ugg_lat(ierr, nlat)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: nlat
    real(kind=KTGT) :: glat(nlat),  wlat(nlat)
    real(kind=KTGT) :: glat2(nlat), wlat2(nlat)
    real(kind=KTGT) :: glatm(nlat+1)

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE =  1.0_KTGT
    real(kind=KTGT),parameter :: HALF = 0.5_KTGT

    real(kind=KTGT) :: span = ZERO
    real(kind=KTGT) :: wnml = ONE

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

201 format('lat:', I0, 1x, F9.3, 1x, 2E24.16, 2x, 2E24.16, 1x, L1, 1x, E9.3, 1x, 2E16.8)
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
  end subroutine test_ugg_lat
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

end program test_emu_ugg

#endif /* TEST_EMU_UGG */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

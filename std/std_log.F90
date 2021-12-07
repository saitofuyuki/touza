!!!_! std_log.F90 - touza/std simple logging helper
! Maintainer: SAITO Fuyuki
! Created: Jul 27 2011
#define TIME_STAMP 'Time-stamp: <2021/12/06 08:34:11 fuyuki std_log.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2011-2021
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
!!!_* include
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_std.h"
!!!_* Macros
#ifndef    HAVE_FC_CONCATENATION
#  define  HAVE_FC_CONCATENATION 0
#endif
!!!_@ TOUZA_Std_log - simple logging
module TOUZA_Std_log
!!!_ = declaration
  use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
!!!_  - default
  implicit none
  private
!!!_  - parameters
  integer,parameter,public :: unit_star   = -1
  integer,parameter,public :: unit_global = -2
  integer,parameter,public :: unit_none   = -999

  integer,parameter :: lpfx = 64
  integer,parameter :: ltxt = 1024

# define __MDL__ 'log'
# define __TAG__ STD_FORMAT_MDL(__MDL__)
!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT - ERR_MASK_STD_LOG
  integer,save :: default_unit = unit_star
!!!_  - interfaces
  interface msg
     module procedure msg_is
     module procedure msg_fs
     module procedure msg_ds
     module procedure msg_as
     module procedure msg_ia
     module procedure msg_fa
     module procedure msg_da
     module procedure msg_txt
  end interface msg

  interface msg_mdl
     module procedure msg_mdl_as
     module procedure msg_mdl_is
     module procedure msg_mdl_ia
     module procedure msg_mdl_txt
  end interface msg_mdl

  interface msg_grp
     module procedure msg_grp_ia
     module procedure msg_grp_txt
  end interface msg_grp

  interface msg_fun
     module procedure msg_fun_ia
     module procedure msg_fun_txt
  end interface msg_fun

!!!_  - message levels
  integer,parameter,public :: msglev_panic    = MSG_LEVEL_PANIC
  integer,parameter,public :: msglev_fatal    = MSG_LEVEL_FATAL
  integer,parameter,public :: msglev_critical = MSG_LEVEL_CRITICAL
  integer,parameter,public :: msglev_severe   = MSG_LEVEL_SEVERE
  integer,parameter,public :: msglev_warning  = MSG_LEVEL_WARNING
  integer,parameter,public :: msglev_normal   = MSG_LEVEL_NORMAL
  integer,parameter,public :: msglev_info     = MSG_LEVEL_INFO
  integer,parameter,public :: msglev_detail   = MSG_LEVEL_DETAIL
  integer,parameter,public :: msglev_debug    = MSG_LEVEL_DEBUG

!!!_  - public
  public init, diag, finalize
  public get_logu
  public msg,  msg_grp, msg_mdl, msg_fun
  public gen_tag
  public is_msglev
  public is_msglev_panic
  public is_msglev_fatal,   is_msglev_critical, is_msglev_severe
  public is_msglev_warning, is_msglev_normal,   is_msglev_info
  public is_msglev_detail,  is_msglev_debug
  public trace_control,     trace_fine

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: utl_init=>init, choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u             ! global log unit (untouch if unit_global)
    integer,intent(in),optional :: levv, mode
    integer md, lv, lmd
    integer utmp

    ierr = 0

    md = control_mode(mode, MODE_DEEPEST)
    init_mode = md

    if (md.ge.MODE_SURFACE) then
       err_default = ERR_SUCCESS
       lv = choice(lev_verbose, levv)
       if (is_first_force(init_counts, md)) then
          utmp = choice(unit_star, u)
          if (utmp.ne.unit_global) default_unit = utmp
          lev_verbose = lv
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call utl_init(ierr, default_unit, levv=lv, mode=lmd)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT - ERR_MASK_STD_LOG
    endif

    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: utl_diag=>diag, choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer lv, md, utmp, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, md)) then
          if (ierr.eq.0) then
             if (is_msglev_NORMAL(lv)) call msg_mdl_txt(TIME_STAMP, __MDL__, utmp)
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call utl_diag(ierr, utmp, lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: utl_finalize=>finalize, choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer lv, utmp, md, lmd

    ierr = err_default
    md = control_mode(mode, init_mode)
    utmp = get_logu(u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, md)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call utl_finalize(ierr, utmp, lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + trace
!!!_  - trace_fine ()
  subroutine trace_fine &
       & (ierr, mode, icount, dcount, fcount, &
       &  pkg, grp, mdl, fun, isfx, u, levv)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(inout)       :: ierr
    integer,         intent(in)          :: mode
    integer,         intent(in)          :: icount, dcount, fcount
    character(len=*),intent(in),optional :: pkg, grp, mdl
    character(len=*),intent(in),optional :: fun
    integer,         intent(in),optional :: isfx
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer lv
    character(len=128) :: tag
    character(len=128) :: txt

311 format('fine: ', I0, 1x, I0, 1x, I0, 1x, I0)
    lv = choice(msglev_normal, levv)
    if (is_msglev_DEBUG(lv)) then
       call gen_tag(tag, pkg, grp, mdl, fun, isfx)
       write(txt, 311) ierr, icount, dcount, fcount
       call msg(txt, tag, u)
    endif
    call trace_control &
         & (ierr, mode, pkg, grp, mdl, fun, isfx, u, levv)
    return
  end subroutine trace_fine

!!!_  - trace_control ()
  subroutine trace_control &
       & (ierr, mode, pkg, grp, mdl, fun, isfx, u, levv)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(inout)       :: ierr
    integer,         intent(in)          :: mode
    character(len=*),intent(in),optional :: pkg, grp, mdl
    character(len=*),intent(in),optional :: fun
    integer,         intent(in),optional :: isfx
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer n
    integer lv
    character(len=128) :: tag
    character(len=128) :: txt
    n = ierr
    if (ierr.ne.0) then
       lv = choice(msglev_normal, levv)
       call gen_tag(tag, pkg, grp, mdl, fun, isfx)
       if (IAND(mode, MODE_LOOSE).gt.0) then
301       format('loose: ', I0)
          if (is_msglev_NORMAL(lv)) then
             write(txt, 301) ierr
             call msg_txt(txt, tag, u)
          endif
          n = 0
       else if (is_msglev_FATAL(lv)) then
302       format('trace: ', I0)
          write(txt, 302) ierr
          call msg_txt(txt, tag, u)
       endif
    endif
    ierr = n
  end subroutine trace_control

!!!_ + module friends
!!!_  & msg_fun_ia - log message [function level]
  subroutine msg_fun_ia &
       & (fmt, vv, mdl, fun, u)
    implicit none
    character(len=*),intent(in)          :: fmt
    integer,         intent(in)          :: vv(:)
    character(len=*),intent(in)          :: mdl
    character(len=*),intent(in)          :: fun
    integer,         intent(in),optional :: u
    character(len=lpfx) :: tag

    call gen_tag(tag, pkg=PACKAGE_TAG, grp=__GRP__, mdl=mdl, fun=fun)
    call msg_ia(fmt, vv, tag, u)
    return
  end subroutine msg_fun_ia
!!!_  & msg_fun_txt - log message [function level]
  subroutine msg_fun_txt &
       & (txt, mdl, fun, u)
    implicit none
    character(len=*),intent(in)          :: txt
    character(len=*),intent(in)          :: mdl
    character(len=*),intent(in)          :: fun
    integer,         intent(in),optional :: u
    character(len=lpfx) :: tag

    call gen_tag(tag, pkg=PACKAGE_TAG, grp=__GRP__, mdl=mdl, fun=fun)
    call msg_txt(txt, tag, u)
    return
  end subroutine msg_fun_txt

!!!_  & msg_mdl - log message [module level]
  subroutine msg_mdl_ia &
       & (fmt, vv, mdl, u)
    implicit none
    character(len=*),intent(in)          :: fmt
    integer,         intent(in)          :: vv(:)
    character(len=*),intent(in)          :: mdl
    integer,         intent(in),optional :: u
    character(len=lpfx) :: tag

    call gen_tag(tag, pkg=PACKAGE_TAG, grp=__GRP__, mdl=mdl)
    call msg_ia(fmt, vv, tag, u)
    return
  end subroutine msg_mdl_ia

  subroutine msg_mdl_is &
       & (fmt, v, mdl, u)
    implicit none
    character(len=*),intent(in)          :: fmt
    integer,         intent(in)          :: v
    character(len=*),intent(in)          :: mdl
    integer,         intent(in),optional :: u
    character(len=lpfx) :: tag

    call gen_tag(tag, pkg=PACKAGE_TAG, grp=__GRP__, mdl=mdl)
    call msg_is(fmt, v, tag, u)
    return
  end subroutine msg_mdl_is

  subroutine msg_mdl_as &
       & (fmt, v, mdl, u)
    implicit none
    character(len=*),intent(in)          :: fmt
    character(len=*),intent(in)          :: v
    character(len=*),intent(in)          :: mdl
    integer,         intent(in),optional :: u
    character(len=lpfx) :: tag

    call gen_tag(tag, pkg=PACKAGE_TAG, grp=__GRP__, mdl=mdl)
    call msg_as(fmt, v, tag, u)
    return
  end subroutine msg_mdl_as

!!!_  & msg_mdl_txt - log message [module level]
  subroutine msg_mdl_txt &
       & (txt, mdl, u)
    implicit none
    character(len=*),intent(in)          :: txt
    character(len=*),intent(in)          :: mdl
    integer,         intent(in),optional :: u
    character(len=lpfx) :: tag

    call gen_tag(tag, pkg=PACKAGE_TAG, grp=__GRP__, mdl=mdl)
    call msg_txt(txt, tag, u)
    return
  end subroutine msg_mdl_txt

!!!_  & msg_grp_ia - log message [group level]
  subroutine msg_grp_ia &
       & (fmt, vv, grp, mdl, u)
    implicit none
    character(len=*),intent(in)          :: fmt
    integer,         intent(in)          :: vv(:)
    character(len=*),intent(in),optional :: grp
    character(len=*),intent(in),optional :: mdl
    integer,         intent(in),optional :: u
    character(len=lpfx) :: tag

    call gen_tag(tag, pkg=PACKAGE_TAG, grp=grp, mdl=mdl)
    call msg_ia(fmt, vv, tag, u)
    return
  end subroutine msg_grp_ia
!!!_  & msg_grp_txt - log message [module level]
  subroutine msg_grp_txt &
       & (txt, grp, mdl, u)
    implicit none
    character(len=*),intent(in)          :: txt
    character(len=*),intent(in),optional :: grp
    character(len=*),intent(in),optional :: mdl
    integer,         intent(in),optional :: u
    character(len=lpfx) :: tag

    call gen_tag(tag, pkg=PACKAGE_TAG, grp=grp, mdl=mdl)
    call msg_txt(txt, tag, u)
    return
  end subroutine msg_grp_txt

!!!_ + user subroutines
!!!_  & is_msglev() - check message level
  logical function is_msglev(levv, cond) &
       & result(r)
    integer,intent(in) :: levv
    integer,intent(in) :: cond
    r = VCHECK(levv, cond)
  end function is_msglev
!!!_  & is_msglev_* - check message level (specific)
  logical function is_msglev_PANIC(levv) &
       & result(r)
    integer,intent(in) :: levv
    r = VCHECK(levv, msglev_panic)
  end function is_msglev_PANIC

  logical function is_msglev_FATAL(levv) &
       & result(r)
    integer,intent(in) :: levv
    r = VCHECK(levv, msglev_fatal)
  end function is_msglev_FATAL

  logical function is_msglev_CRITICAL(levv) &
       & result(r)
    integer,intent(in) :: levv
    r = VCHECK(levv, msglev_critical)
  end function is_msglev_CRITICAL

  logical function is_msglev_SEVERE(levv) &
       & result(r)
    integer,intent(in) :: levv
    r = VCHECK(levv, msglev_severe)
  end function is_msglev_SEVERE

  logical function is_msglev_WARNING(levv) &
       & result(r)
    integer,intent(in) :: levv
    r = VCHECK(levv, msglev_warning)
  end function is_msglev_WARNING

  logical function is_msglev_NORMAL(levv) &
       & result(r)
    integer,intent(in) :: levv
    r = VCHECK(levv, msglev_normal)
  end function is_msglev_NORMAL

  logical function is_msglev_INFO(levv) &
       & result(r)
    integer,intent(in) :: levv
    r = VCHECK(levv, msglev_info)
  end function is_msglev_INFO

  logical function is_msglev_DETAIL(levv) &
       & result(r)
    integer,intent(in) :: levv
    r = VCHECK(levv, msglev_detail)
  end function is_msglev_DETAIL

  logical function is_msglev_DEBUG(levv) &
       & result(r)
    integer,intent(in) :: levv
    r = VCHECK(levv, msglev_debug)
  end function is_msglev_DEBUG

!!!_  & msg_*a - log message (format and array)
  subroutine msg_ia &
       & (fmt, vv, tag, u)
    implicit none
    character(len=*),intent(in)          :: fmt
    integer,         intent(in)          :: vv(:)
    character(len=*),intent(in)          :: tag
    integer,         intent(in),optional :: u
    character(len=ltxt) :: txt

    write(txt, fmt) vv(:)
    call msg_txt(txt, tag, u)

  end subroutine msg_ia

  subroutine msg_fa &
       & (fmt, vv, tag, u)
    use TOUZA_Std_prc,only: KFLT
    implicit none
    character(len=*),intent(in)          :: fmt
    real(kind=KFLT), intent(in)          :: vv(:)
    character(len=*),intent(in)          :: tag
    integer,         intent(in),optional :: u
    character(len=ltxt) :: txt

    write(txt, fmt) vv(:)
    call msg_txt(txt, tag, u)

  end subroutine msg_fa

  subroutine msg_da &
       & (fmt, vv, tag, u)
    use TOUZA_Std_prc,only: KDBL
    implicit none
    character(len=*),intent(in)          :: fmt
    real(kind=KDBL), intent(in)          :: vv(:)
    character(len=*),intent(in)          :: tag
    integer,         intent(in),optional :: u
    character(len=ltxt) :: txt

    write(txt, fmt) vv(:)
    call msg_txt(txt, tag, u)

  end subroutine msg_da

!!!_  & msg_*s - log message (format and single variable)
  subroutine msg_is &
       & (fmt, v, tag, u)
    implicit none
    character(len=*),intent(in)          :: fmt
    integer,         intent(in)          :: v
    character(len=*),intent(in)          :: tag
    integer,         intent(in),optional :: u
    character(len=ltxt) :: txt

    write(txt, fmt) v
    call msg_txt(txt, tag, u)

  end subroutine msg_is

  subroutine msg_fs &
       & (fmt, v, tag, u)
    use TOUZA_Std_prc,only: KFLT
    implicit none
    character(len=*),intent(in)          :: fmt
    real(kind=KFLT), intent(in)          :: v
    character(len=*),intent(in)          :: tag
    integer,         intent(in),optional :: u
    character(len=ltxt) :: txt

    write(txt, fmt) v
    call msg_txt(txt, tag, u)

  end subroutine msg_fs

  subroutine msg_ds &
       & (fmt, v, tag, u)
    use TOUZA_Std_prc,only: KDBL
    implicit none
    character(len=*),intent(in)          :: fmt
    real(kind=KDBL), intent(in)          :: v
    character(len=*),intent(in)          :: tag
    integer,         intent(in),optional :: u
    character(len=ltxt) :: txt

    write(txt, fmt) v
    call msg_txt(txt, tag, u)

  end subroutine msg_ds

  subroutine msg_as &
       & (fmt, v, tag, u)
    implicit none
    character(len=*),intent(in)          :: fmt
    character(len=*),intent(in)          :: v
    character(len=*),intent(in)          :: tag
    integer,         intent(in),optional :: u
    character(len=ltxt) :: txt
    integer jerr

    write(txt, fmt, IOSTAT=jerr) trim(v)
    if (jerr.eq.0) then
       call msg_txt(txt, tag, u)
    else
       if (HAVE_FC_CONCATENATION.ne.0) then
          txt = trim(fmt) // ' ' // trim(v)
          call msg_txt(txt, tag, u)
       else
          call msg_txt(fmt, tag, u)
          call msg_txt(v,   tag, u)
       endif
    endif
  end subroutine msg_as

!!!_  & msg_txt - message core
  subroutine msg_txt &
       & (txt, tag, u)
    use TOUZA_Std_utl,only: choice
    implicit none
    character(len=*),intent(in)          :: txt
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    integer ut
    ut = choice(unit_global, u)
    if (ut.eq.unit_global) ut = default_unit
101 format(_TOUZA_FORMAT_TAG(A), A)
102 format(A)
    if (present(tag)) then
       if      (ut.eq.unit_star) then
          write(*,   101) trim(tag), trim(txt)
       else if (ut.ge.0)  then
          write(ut,  101) trim(tag), trim(txt)
       endif
    else
       if      (ut.eq.unit_star) then
          write(*,   102) trim(txt)
       else if (ut.ge.0)  then
          write(ut,  102) trim(txt)
       endif
    endif
    return
  end subroutine msg_txt

!!!_  & gen_tag - generate message tag
  subroutine gen_tag &
       & (tag, pkg, grp, mdl, fun, isfx)
    implicit none
    character(len=*),intent(out)         :: tag
    character(len=*),intent(in),optional :: pkg, grp, mdl
    character(len=*),intent(in),optional :: fun
    integer,         intent(in),optional :: isfx

    character(len=256) :: pfx
101 format(_TOUZA_TAG_PGM(A,A,A))
102 format(_TOUZA_TAG_PG(A,A))
103 format(_TOUZA_TAG_P(A))
    if (present(pkg)) then
       if (present(grp)) then
          if (present(mdl)) then
             write(pfx, 101) trim(pkg), trim(grp), trim(mdl)
          else
             write(pfx, 102) trim(pkg), trim(grp)
          endif
       else
          write(pfx, 103) trim(pkg)
       endif
    else
       pfx = ' '
    endif
201 format(A, _TOUZA_TAG_F(A),  TOUZA_FUN_SEP, I0)
202 format(A, _TOUZA_TAG_F(A))
203 format(A, TOUZA_FUN_SEP, I0)
204 format(A)
    if (present(fun)) then
       if (present(isfx)) then
          write(tag, 201) trim(pfx), trim(fun), isfx
       else
          write(tag, 202) trim(pfx), trim(fun)
       endif
    else if (present(isfx)) then
       write(tag, 203) trim(pfx), isfx
    else
       write(tag, 204) trim(pfx)
    endif

  end subroutine gen_tag

!!!_  & get_logu - get logging unit
  PURE &
  integer function get_logu(u, udef) result(lu)
    implicit none
    integer,intent(in),optional :: u
    integer,intent(in),optional :: udef
    if (present(u)) then
       lu = u
    else if (present(udef)) then
       lu = udef
    else
       lu = unit_global
    endif
    if (lu.eq.unit_global) lu = default_unit
    return
  end function get_logu
!!!_ + end
end module TOUZA_Std_log
!!!_@ test_std_log - test program
#ifdef TEST_STD_LOG
program test_std_log
  use TOUZA_Std_log
  implicit none
  integer ierr

  call init(ierr)

  call diag(ierr)
  call diag(ierr)
  call msg('discarded',  'SKIP', -999)
  call msg('to unit 10', '10',   10)

  call test_gen_tag()
  call test_gen_tag('P')
  call test_gen_tag('P', 'G')
  call test_gen_tag('P', 'G', 'M')
  call test_gen_tag('P', 'G', 'M', 'F')
  call test_gen_tag('P', 'G', 'M', 'F', 123)
  call test_gen_tag(fun='F')
  call test_gen_tag('P', fun='F')
  call test_gen_tag('P', 'G', fun='F')
  call test_gen_tag(fun='F', isfx=123)
  call test_gen_tag('P', fun='F', isfx=123)
  call test_gen_tag('P', 'G', fun='F', isfx=123)
  call test_gen_tag(isfx=123)
  call test_gen_tag('P', isfx=123)
  call test_gen_tag('P', 'G', isfx=123)
  call test_gen_tag('P', 'G', 'M', isfx=123)

  call finalize(ierr, levv=+10)

  stop

contains
  subroutine test_gen_tag &
       & (pkg, grp, mdl, fun, isfx)
    implicit none
    character(len=*),intent(in),optional :: pkg, grp, mdl
    character(len=*),intent(in),optional :: fun
    integer,         intent(in),optional :: isfx

    character(len=256) :: tag
    call gen_tag(tag, pkg, grp, mdl, fun, isfx)
101 format('TAG:', 5L1, ': <', A, '>')
    write(*, 101) &
         & present(pkg), present(grp), present(mdl), present(fun), present(isfx), &
         & trim(tag)
    return
  end subroutine test_gen_tag
end program test_std_log
#endif /* TEST_STD_LOG */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

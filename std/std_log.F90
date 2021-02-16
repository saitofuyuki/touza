!!!_! std_log.F90 - touza/std simple logging helper
! Maintainer: SAITO Fuyuki
! Created: Jul 27 2011
#define TIME_STAMP 'Time-stamp: <2021/02/07 19:41:28 fuyuki std_log.F90>'
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
!!!_@ TOUZA_Std_log - simple logging
module TOUZA_Std_log
!!!_ = declaration
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
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL

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
  public msg,  msg_grp, msg_mdl, msg_fun
  public gen_tag
  public is_msglev
  public is_msglev_panic
  public is_msglev_fatal,   is_msglev_critical, is_msglev_severe
  public is_msglev_warning, is_msglev_normal,   is_msglev_info
  public is_msglev_detail,  is_msglev_debug

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, logu, levv, mode)
    use TOUZA_Std_utl,only: utl_init=>init, choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: logu
    integer,intent(in),optional :: levv, mode

    integer md, lv

    ierr = 0

    lv = choice(lev_verbose, levv)
    md = choice(INIT_DEFAULT, mode)
    if (md.eq.INIT_DEFAULT) md = INIT_DEEP

    if (md.gt.INIT_DEFAULT) then
       if (md.ge.INIT_DEEP) then
          if (ierr.eq.0) call utl_init(ierr, levv=lv, mode=md)
       endif
       if (init_counts.eq.0) then
          lev_verbose = lv
          default_unit = choice(unit_star, logu)
       endif
       init_counts = init_counts + 1
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
    integer lv, md, utmp

    ierr = 0
    utmp = get_u(u)
    lv = choice(lev_verbose, levv)
    md = choice(DIAG_DEFAULT, mode)
    if (md.eq.DIAG_DEFAULT) md = DIAG_DEEP

    if (md.gt.DIAG_DEFAULT) then
       if (IAND(md, DIAG_DEEP).gt.0) then
          if (ierr.eq.0) call utl_diag(ierr, utmp, lv, md)
       endif
       if (diag_counts.eq.0.or.IAND(md,DIAG_FORCE).gt.0) then
          if (ierr.eq.0) then
             if (VCHECK_NORMAL(lv)) call msg_mdl_txt(TIME_STAMP, __MDL__, utmp)
          endif
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
    integer lv, utmp

    ierr = 0 * choice(0,mode)
    lv = choice(lev_verbose, levv)
    utmp = get_u(u)
    call utl_finalize(ierr, utmp, lv, mode)
    return
  end subroutine finalize

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

    write(txt, fmt) v
    call msg_txt(txt, tag, u)

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

!!!_  & get_u - get logging unit
  integer function get_u(u) result(lu)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(in),optional :: u
    lu = choice(unit_global, u)
    if (lu.eq.unit_global) lu = default_unit
    return
  end function get_u
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

  call finalize(ierr)

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

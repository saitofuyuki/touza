!!!_! std_log.F90 - touza/std simple logging helper
! Maintainer: SAITO Fuyuki
! Created: Jul 27 2011
#define TIME_STAMP 'Time-stamp: <2023/04/17 09:47:01 fuyuki std_log.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2011-2023
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
#ifndef    HAVE_FORTRAN_FC_CONCATENATION
#  define  HAVE_FORTRAN_FC_CONCATENATION 0
#endif
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
# define _ERROR(E) (E - ERR_MASK_STD_LOG)
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
     module procedure msg_aa
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
     module procedure msg_grp_aa
     module procedure msg_grp_txt
  end interface msg_grp

  interface msg_fun
     module procedure msg_fun_ia
     module procedure msg_fun_txt
  end interface msg_fun

  interface banner
     module procedure banner_core
  end interface banner

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
  public get_logu,   set_defu
  public msg,  msg_grp, msg_mdl, msg_fun
  public gen_tag
  public is_msglev
  public is_msglev_panic
  public is_msglev_fatal,   is_msglev_critical, is_msglev_severe
  public is_msglev_warning, is_msglev_normal,   is_msglev_info
  public is_msglev_detail,  is_msglev_debug
  public banner
  public trace_control,     trace_fine,         trace_err
  public is_error_match
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: utl_init=>init, choice
    use TOUZA_Std_prc,only: prc_init=>init
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
       if (is_first_force(init_counts, mode)) then
          utmp = choice(unit_star, u)
          if (utmp.ne.unit_global) default_unit = utmp
          lev_verbose = lv
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call prc_init(ierr, default_unit, levv=lv, mode=lmd)
          if (ierr.eq.0) call utl_init(ierr, default_unit, levv=lv, mode=lmd)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = _ERROR(ERR_FAILURE_INIT)
    endif

    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: utl_diag=>diag, choice
    use TOUZA_Std_prc,only: prc_diag=>diag
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
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (is_msglev_NORMAL(lv)) call msg_mdl_txt(TIME_STAMP, __MDL__, utmp)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call prc_diag(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call utl_diag(ierr, utmp, lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: utl_finalize=>finalize, choice
    use TOUZA_Std_prc,only: prc_finalize=>finalize
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
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call prc_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call utl_finalize(ierr, utmp, lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + trace
!!!_  - trace_err
  subroutine trace_err &
       & (ierr, pkg, grp, mdl, fun, asfx, isfx, u, levv)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(in)          :: ierr
    character(len=*),intent(in),optional :: pkg, grp, mdl, fun
    character(len=*),intent(in),optional :: asfx
    integer,         intent(in),optional :: isfx
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer c
    integer lv
    integer mgrp, mmdl
    character(len=128) :: estr, tag

    lv = choice(msglev_normal, levv)
    if (ierr.ne.0.or.is_msglev_DEBUG(lv)) then
       if (ierr.lt.0) then
          c = - ierr
          mgrp = c / ERR_MASK_GROUP
          mmdl = mod(c, ERR_MASK_GROUP) / ERR_MASK_MODULE
          c = mod(c, ERR_MASK_MODULE)
101       format('trace_err:', I0, '=', I0, ',', I0, ',', I0)
          write(estr, 101) ierr, mgrp, mmdl, c
       else
102       format('trace_err:', I0)
          write(estr, 102) ierr
       endif
       call gen_tag(tag, pkg, grp, mdl, fun, asfx, isfx)
       call msg(estr, tag, u)
    endif

  end subroutine trace_err
!!!_  - trace_fine ()
  subroutine trace_fine &
       & (ierr, mode, icount, dcount, fcount, &
       &  pkg,  grp,  mdl,    fun,    asfx,   isfx, u, levv)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(inout)       :: ierr
    integer,         intent(in)          :: mode
    integer,         intent(in)          :: icount, dcount, fcount
    character(len=*),intent(in),optional :: pkg, grp, mdl
    character(len=*),intent(in),optional :: fun
    character(len=*),intent(in),optional :: asfx
    integer,         intent(in),optional :: isfx
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer lv
    character(len=128) :: tag
    character(len=128) :: txt
    integer jerr

311 format('fine: ', I0, 1x, I0, 1x, I0, 1x, I0)
    lv = choice(msglev_normal, levv)
    if (is_msglev_DEBUG(lv)) then
       call gen_tag(tag, pkg, grp, mdl, fun, asfx, isfx)
       write(txt, 311, IOSTAT=jerr) ierr, icount, dcount, fcount
       call msg(txt, tag, u)
    endif
    call trace_control &
         & (ierr, mode, pkg, grp, mdl, fun, asfx, isfx, u, levv)
    return
  end subroutine trace_fine

!!!_  - trace_control ()
  subroutine trace_control &
       & (ierr, mode, pkg, grp, mdl, fun, asfx, isfx, u, levv)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(inout)       :: ierr
    integer,         intent(in)          :: mode
    character(len=*),intent(in),optional :: pkg, grp, mdl
    character(len=*),intent(in),optional :: fun
    character(len=*),intent(in),optional :: asfx
    integer,         intent(in),optional :: isfx
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer n
    integer lv
    character(len=128) :: tag
    character(len=128) :: txt
    integer jerr

    n = ierr
    if (ierr.ne.0) then
       lv = choice(msglev_normal, levv)
       call gen_tag(tag, pkg, grp, mdl, fun, asfx, isfx)
       if (IAND(mode, MODE_LOOSE).gt.0) then
301       format('loose: ', I0)
          if (is_msglev_NORMAL(lv)) then
             write(txt, 301, IOSTAT=jerr) ierr
             call msg_txt(txt, tag, u)
          endif
          n = 0
       else if (is_msglev_FATAL(lv)) then
302       format('trace: ', I0)
          write(txt, 302, IOSTAT=jerr) ierr
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
!!!_  & msg_grp_aa - log message [module level]
  subroutine msg_grp_aa &
       & (fmt, vv, grp, mdl, u)
    implicit none
    character(len=*),intent(in)          :: fmt
    character(len=*),intent(in)          :: vv(:)
    character(len=*),intent(in),optional :: grp
    character(len=*),intent(in),optional :: mdl
    integer,         intent(in),optional :: u
    character(len=lpfx) :: tag

    call gen_tag(tag, pkg=PACKAGE_TAG, grp=grp, mdl=mdl)
    call msg_aa(fmt, vv(:), tag, u)
    return
  end subroutine msg_grp_aa

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
    integer jerr

    write(txt, fmt, IOSTAT=jerr) vv(:)
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
    integer jerr

    write(txt, fmt, IOSTAT=jerr) vv(:)
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
    integer jerr

    write(txt, fmt, IOSTAT=jerr) vv(:)
    call msg_txt(txt, tag, u)

  end subroutine msg_da

  subroutine msg_aa &
       & (fmt, vv, tag, u)
    use TOUZA_Std_prc,only: KDBL
    implicit none
    character(len=*),intent(in)          :: fmt
    character(len=*),intent(in)          :: vv(:)
    character(len=*),intent(in)          :: tag
    integer,         intent(in),optional :: u
    character(len=ltxt) :: txt
    integer j
    integer jerr

    write(txt, fmt, IOSTAT=jerr) (trim(vv(j)), j=1, size(vv))
    call msg_txt(txt, tag, u)

  end subroutine msg_aa

!!!_  & msg_*s - log message (format and single variable)
  subroutine msg_is &
       & (fmt, v, tag, u)
    implicit none
    character(len=*),intent(in)          :: fmt
    integer,         intent(in)          :: v
    character(len=*),intent(in)          :: tag
    integer,         intent(in),optional :: u
    character(len=ltxt) :: txt
    integer jerr

    write(txt, fmt, IOSTAT=jerr) v
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
    integer jerr

    write(txt, fmt, IOSTAT=jerr) v
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
    integer jerr

    write(txt, fmt, IOSTAT=jerr) v
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
       if (HAVE_FORTRAN_FC_CONCATENATION.ne.0) then
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
       if (tag.eq.' ') then
          if      (ut.eq.unit_star) then
             write(*,   102) trim(txt)
          else if (ut.ge.0)  then
             write(ut,  102) trim(txt)
          endif
       else
          if      (ut.eq.unit_star) then
             write(*,   101) trim(tag), trim(txt)
          else if (ut.ge.0)  then
             write(ut,  101) trim(tag), trim(txt)
          endif
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
       & (tag, pkg, grp, mdl, fun, asfx, isfx, label)
    use TOUZA_Std_utl,only: choice
    implicit none
    character(len=*),intent(out)         :: tag
    character(len=*),intent(in),optional :: pkg, grp, mdl
    character(len=*),intent(in),optional :: fun
    character(len=*),intent(in),optional :: asfx
    integer,         intent(in),optional :: isfx
    logical,         intent(in),optional :: label  ! boolean to apply format conversion
    character(len=1024) :: pfx
    integer jerr

101 format(_TOUZA_TAG_PGM(A,A,A))
102 format(_TOUZA_TAG_PG(A,A))
103 format(_TOUZA_TAG_P(A))
    jerr = 0
    if (present(pkg)) then
       if (present(grp)) then
          if (present(mdl)) then
             write(pfx, 101, IOSTAT=jerr) trim(pkg), trim(grp), trim(mdl)
          else
             write(pfx, 102, IOSTAT=jerr) trim(pkg), trim(grp)
          endif
       else
          write(pfx, 103, IOSTAT=jerr) trim(pkg)
       endif
    else
       pfx = ' '
    endif
201 format(A, _TOUZA_TAG_F(A),  TOUZA_FUN_SEP, I0)
202 format(A, _TOUZA_TAG_F(A))
203 format(A, TOUZA_FUN_SEP, I0)
204 format(A)
211 format(A, _TOUZA_TAG_F(A),  TOUZA_FUN_SEP, A, TOUZA_SFX_SEP, I0)
212 format(A, _TOUZA_TAG_F(A),  TOUZA_FUN_SEP, A)
213 format(A, TOUZA_FUN_SEP, A, TOUZA_SFX_SEP, I0)
214 format(A, TOUZA_FUN_SEP, A)

    if (present(fun)) then
       if (present(asfx)) then
          if (present(isfx)) then
             write(tag, 211, IOSTAT=jerr) trim(pfx), trim(fun), trim(asfx), isfx
          else
             write(tag, 212, IOSTAT=jerr) trim(pfx), trim(fun), trim(asfx)
          endif
       else if (present(isfx)) then
          write(tag, 201, IOSTAT=jerr) trim(pfx), trim(fun), isfx
       else
          write(tag, 202, IOSTAT=jerr) trim(pfx), trim(fun)
       endif
    else if (present(isfx)) then
       if (present(asfx)) then
          write(tag, 213, IOSTAT=jerr) trim(pfx), trim(asfx), isfx
       else
          write(tag, 203, IOSTAT=jerr) trim(pfx), isfx
       endif
    else if (present(asfx)) then
       write(tag, 214, IOSTAT=jerr) trim(pfx), trim(asfx)
    else
       write(tag, 204, IOSTAT=jerr) trim(pfx)
    endif

    if (choice(.false., label)) then
301    format(_TOUZA_FORMAT_TAG(A))
       pfx = tag
       write(tag, 301, IOSTAT=jerr) trim(pfx)
    endif
  end subroutine gen_tag

!!!_  & banner - output banner
  subroutine banner_core &
       & (ierr, txt, u, ch, ww, pat, indent)
    use TOUZA_Std_utl,only: choice, choice_a
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: txt
    integer,         intent(in),optional :: u
    character(len=*),intent(in),optional :: ch
    integer,         intent(in),optional :: ww
    integer,         intent(in),optional :: pat  ! pattern
    integer,         intent(in),optional :: indent
    integer ut
    integer wi
    integer ll
    integer rl, rw
    integer,parameter :: lc = 1 ! fixed
    character(len=lc) :: ci
    integer ind
    integer p
    character(len=64) :: fmt0, fmt1
    integer jerr

    ierr = 0

    ut = choice(unit_global, u)
    if (ut.eq.unit_global) ut = default_unit

    p = choice(0, pat)
    ind = max(0, choice(1, indent))

    call choice_a(ci, '+', ch)
    wi = max(choice(3, ww), lc)
    rw = wi / lc

    ll = len_trim(txt) + wi * 2 + 2
    rl = (ll - 1) / lc + 1
    ll = rl * lc

    if (ind.le.0) then
       fmt0 = '(A)'
       fmt1 = '(A, 1x, A, 1x, A)'
    else
111    format('(', I0, 'x, A)')
112    format('(', I0, 'x, A, 1x, A, 1x, A)')
       write(fmt0, 111, IOSTAT=jerr) ind
       write(fmt1, 112, IOSTAT=jerr) ind
    endif

    select case (p)
    case(-1)
       if (ut.ge.0) then
          write(ut, fmt1) repeat(trim(ci), rw), trim(txt), repeat(trim(ci), rw)
       else if (ut.eq.-1) then
          write(*,  fmt1) repeat(trim(ci), rw), trim(txt), repeat(trim(ci), rw)
       endif
    case default
       if (ut.ge.0) then
          write(ut, fmt0) repeat(trim(ci), rl)
          write(ut, fmt1) repeat(trim(ci), rw), trim(txt), repeat(trim(ci), rw)
          write(ut, fmt0) repeat(trim(ci), rl)
       else if (ut.eq.-1) then
          write(*, fmt0) repeat(trim(ci), rl)
          write(*, fmt1) repeat(trim(ci), rw), trim(txt), repeat(trim(ci), rw)
          write(*, fmt0) repeat(trim(ci), rl)
       endif
    end select
    return
  end subroutine banner_core

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

!!!_  & set_defu - set global logging unit
  subroutine set_defu(u)
    use TOUZA_Std_utl,only: uset_defu=>set_defu
    use TOUZA_Std_prc,only: pset_defu=>set_defu
    implicit none
    integer,intent(in) :: u
    default_unit = u
    call uset_defu(default_unit)
    call pset_defu(default_unit)
    return
  end subroutine set_defu

!!!_  & is_error_match () - check if error-code matches
  logical function is_error_match &
       & (ierr, jcode, mdl) &
       & result(b)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(in)          :: ierr
    integer,intent(in)          :: jcode
    integer,intent(in),optional :: mdl
    integer je
    if (ierr.ge.0) then
       b = (jcode.eq.ierr)
    else if (present(mdl)) then
       b = (jcode - mdl) .eq. ierr
    else
       je = IOR(ierr, NOT(ERR_MASK_MODULE - 1))
       if (je.eq.-ERR_MASK_MODULE) then
          b = (jcode.eq.0)
       else
          b = jcode .eq. je
       endif
    endif
  end function is_error_match
!!!_ + end
end module TOUZA_Std_log
!!!_@ test_std_log - test program
#ifdef TEST_STD_LOG
program test_std_log
  use TOUZA_Std_log
  implicit none
  integer ierr
  integer je

  call banner(ierr, 'test std_log', -1)
  call banner(ierr, 'test std_log', -1, '*')
  call banner(ierr, 'test std_log', -1, '=', 5)

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
  call test_gen_tag('P', 'G', 'M', 'F', isfx=123)
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

  do je = 0, ERR_MASK_MODULE-1
     call test_error(-je, ERR_EOF)
  enddo
  do je = 0, ERR_MASK_MODULE-1
     call test_error(-je - ERR_MASK_CAL, ERR_EOF, ERR_MASK_CAL)
  enddo
  do je = 0, ERR_MASK_MODULE-1
     call test_error(-je - ERR_MASK_NIO, ERR_EOF, ERR_MASK_CAL)
  enddo

  call test_error_code(ERR_MASK_NIO,      ERR_EOF)
  call test_error_code(ERR_MASK_NIO_CTRL, ERR_EOF)

  call finalize(ierr, levv=+10)

  stop

contains
  subroutine test_gen_tag &
       & (pkg, grp, mdl, fun, asfx, isfx)
    implicit none
    character(len=*),intent(in),optional :: pkg, grp, mdl
    character(len=*),intent(in),optional :: fun
    character(len=*),intent(in),optional :: asfx
    integer,         intent(in),optional :: isfx

    character(len=256) :: tag
    call gen_tag(tag, pkg, grp, mdl, fun, asfx, isfx)
101 format('TAG:', 6L1, ': <', A, '>')
    write(*, 101) &
         & present(pkg), present(grp),  present(mdl), &
         & present(fun), present(asfx), present(isfx), &
         & trim(tag)
    return
  end subroutine test_gen_tag

  subroutine test_error &
       & (ierr, jcode, mdl)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(in)          :: ierr
    integer,intent(in)          :: jcode
    integer,intent(in),optional :: mdl

101 format('error match: ', I0, ' = ', I0, ' & ', I0)
    if (is_error_match(ierr, jcode, mdl)) then
       write(*, 101) ierr, jcode, choice(0, mdl)
    endif

  end subroutine test_error

  subroutine test_error_code &
       & (egrp, ecode)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(in) :: egrp, ecode
    integer ierr
    ierr = - egrp + ecode
101 format('error code: ', I0, 1x, ' = ', I0, ',', I0)
    write(*, 101) ierr, egrp, ecode
    call trace_err(ierr)
    call trace_err(ierr, asfx=__FILE__, isfx=__LINE__)
  end subroutine test_error_code

end program test_std_log
#endif /* TEST_STD_LOG */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

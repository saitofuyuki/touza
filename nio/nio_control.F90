!!!_! nio_control.F90 - TOUZA/Nio control center
! Maintainer: SAITO Fuyuki
! Created: Dec 12 2022
#define TIME_STAMP 'Time-stamp: <2024/02/26 15:31:41 fuyuki nio_control.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022, 2023, 2024
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_nio.h"
#ifndef   OPT_NIO_CTRL_SIZE
#  define OPT_NIO_CTRL_SIZE 512
#endif
!!!_@ TOUZA_Nio_control - nio control center
module TOUZA_Nio_control
!!!_ = declaration
  use TOUZA_Nio_std,only: KI32, KI64, KDBL, KFLT, KIOFS
  use TOUZA_Nio_std,only: get_logu,    unit_global,   trace_fine,   trace_control
  use TOUZA_Nio_header, nh_init=>init, nh_diag=>diag, nh_finalize=>finalize
  implicit none
  private
!!!_  - public parameter
  integer,parameter,public :: nio_format_other = 0   ! not Nio format
  integer,parameter,public :: nio_format_std   = 1   ! standard Nio format, MIROC/gtool-3.5 compatible
  integer,parameter,public :: nio_format_ext   = 2   ! extended Nio format

  integer,parameter,public :: enable_cache      = 1    ! enable cache-mode
  integer,parameter,public :: enable_sequential = 2    ! enable sequential-mode
  integer,parameter,public :: enable_auto       = 4    ! enable auto detection

  character(len=*),parameter,public :: seps_file_item = '/?'   ! separators between file and item

  ! integer,parameter,public :: flag_default = enable_cache + enable_sequential
  integer,parameter,public :: flag_default = enable_cache
  !     embedded cache    no embedded cache
  ! 01    cache                cache
  ! 10    sequential           sequential
  ! 11    cache                sequential
!!!_  - private parameter
!!!_  - control tables
  integer,save      :: hh_ctrl = -1   ! hash-table id
  integer,parameter :: def_status = -1

  integer,parameter :: hseed   = 13
  integer,parameter :: mode_cache = 1
  integer,parameter :: mode_sequential = 2
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = NIO_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
#define __MDL__ 'x'
#define _ERROR(E) (E - ERR_MASK_NIO_CTRL)
!!!_  - interfaces
  interface nio_search
     module procedure nio_search_d
  end interface nio_search
  interface nio_time_undef
     module procedure nio_time_undef_d
  end interface nio_time_undef
  interface nio_time_repl
     module procedure nio_time_repl_d
  end interface nio_time_repl
!!!_  - public procedures
  public init, diag, finalize
  public nio_open, nio_close
  public nio_search
  public nio_show_status
  public nio_time_undef
  ! public nio_check_format
!!!_  - public shared
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init &
       & (ierr, u, levv, mode, stdv, icomm, nctrl)
    use TOUZA_Nio_std,   only: control_mode, control_deep, is_first_force
    use TOUZA_Nio_std,   only: ns_init=>init, choice, get_size_bytes, KDBL
    use TOUZA_Nio_header,only: nh_init=>init
    use TOUZA_Nio_record,only: nr_init=>init
    use TOUZA_Nio_cache, only: nc_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    integer,intent(in),optional :: icomm
    integer,intent(in),optional :: nctrl
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
          if (ierr.eq.0) call ns_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
          if (ierr.eq.0) call nh_init(ierr, u=ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_init(ierr, u=ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call nc_init(ierr, u=ulog, levv=lv, mode=lmd)
       endif
       if (init_counts.eq.0) then
          call init_table(ierr, nctrl)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Nio_std,   only: control_mode, control_deep, is_first_force
    use TOUZA_Nio_std,   only: ns_diag=>diag, choice, msg, is_msglev_normal, is_msglev_info
    use TOUZA_Nio_header,only: nh_diag=>diag
    use TOUZA_Nio_record,only: nr_diag=>diag
    use TOUZA_Nio_cache, only: nc_diag=>diag
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
          if (ierr.eq.0) call ns_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nh_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nc_diag(ierr, utmp, levv=lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Nio_std,   only: control_mode, control_deep, is_first_force
    use TOUZA_Nio_std,   only: ns_finalize=>finalize, choice
    use TOUZA_Nio_header,only: nh_finalize=>finalize
    use TOUZA_Nio_record,only: nr_finalize=>finalize
    use TOUZA_Nio_cache, only: nc_finalize=>finalize
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
          if (ierr.eq.0) call nh_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call ns_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nc_finalize(ierr, utmp, levv=lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize
!!!_  - init subcontracts
!!!_ + control table management
!!!_  & init_table - hash table initialization
  subroutine init_table &
       & (ierr, n)
    use TOUZA_Nio_std,only: choice, new_htable
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: n
    integer m

    ierr = 0
    m = choice(0, n)
    if (m.le.0) m = OPT_NIO_CTRL_SIZE
    m = m * 2
    if (hh_ctrl.lt.0) then
       hh_ctrl = new_htable('nio-control', m, lkey=0, nkey=1, nstt=1, def=def_status)
       ierr = min(0, hh_ctrl)
    endif
    return
  end subroutine init_table
!!!_  & register_file
  subroutine register_file (ierr, handle, ch, mode)
    use TOUZA_Nio_std,only: reg_entry
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: handle
    integer,intent(in)  :: ch
    integer,intent(in)  :: mode
    integer stt
    ierr = err_default
    if (ierr.eq.0) then
       stt = bind_handle_mode(ch, mode)
       ierr = reg_entry(hh_ctrl, ikey=(/ch/), status=(/stt/))
       ierr = min(0, ierr)
    endif
    if (ierr.eq.0) then
       handle = ch
    else
       handle = -1
    endif
  end subroutine register_file
!!!_  & query_bind()
  integer function query_bind (handle) result(b)
    use TOUZA_Nio_std,only: query_status
    implicit none
    integer,intent(in) :: handle
    integer ch
    integer jerr
    ch = handle
    call query_status(jerr, b, hh_ctrl, ikey=(/ch/))
    if (jerr.ne.0) then
       b = _ERROR(ERR_INVALID_PARAMETER)
    endif
  end function query_bind

!!!_  & bind_handle_mode()
  integer function bind_handle_mode (h, mode) result(b)
    implicit none
    integer,intent(in) :: h
    integer,intent(in) :: mode
    if (h.lt.0) then
       b = h
    else if (mode.lt.0) then
       b = mode
    else
       b = h * hseed + mode
    endif
  end function bind_handle_mode

!!!_  & extract_mode()
  integer function extract_mode (b) result(n)
    implicit none
    integer,intent(in) :: b
    if (b.lt.0) then
       n = b
    else
       n = mod(b, hseed)
    endif
  end function extract_mode
!!!_  & extract_subhandle()
  integer function extract_subhandle (b) result(n)
    implicit none
    integer,intent(in) :: b
    if (b.lt.0) then
       n = b
    else
       n = b / hseed
    endif
  end function extract_subhandle
!!!_  & is_cache_handle()
  integer function is_cache_bind (b) result(n)
    implicit none
    integer,intent(in) :: b
    integer :: mode

    mode = extract_mode(b)
    if (mode.eq.mode_cache) then
       n = extract_subhandle(b)
    else
       n = -1
    endif
  end function is_cache_bind

!!!_ + user interfaces (dispatcher)
! !!!_  - nio_check_format - Check whether file is Nio format
!   integer function nio_check_format (file, cat) result(n)
!     use TOUZA_Nio_std,only: new_unit, search_from_last
!     use TOUZA_Nio_std,only: sus_open, sus_close
!     use TOUZA_Nio_record,only: nio_read_header
!     implicit none
!     character(len=*),intent(in) :: file
!     integer,optional,intent(in) :: cat   ! category id for unit search in new_unit()
!     integer u
!     integer jerr
!     integer krect
!     character(len=litem) :: head(nitem)

!     n = ERR_INVALID_PARAMETER
!     u = new_unit(search_from_last, cat)
!     if (u.lt.0) then
!        n = u
!        return
!     endif
!     call sus_open(jerr, u, file, ACTION='R', STATUS='O')
!     if (jerr.eq.0) then
!        call nio_read_header(jerr, head,  krect, u)
!        if (jerr.eq.0) then
!           ! to check Nio extension
!           n = nio_format_std
!        else
!           n = nio_format_other
!           jerr = 0
!        endif
!     endif
!     if (jerr.eq.0) call sus_close(jerr, u, file)
!     if (jerr.lt.0) n = jerr
!   end function nio_check_format
!!!_  - nio_open - open Nio file
  subroutine nio_open &
       & (ierr, handle, path, flag, unit)
    use TOUZA_Nio_cache,only: cache_open_read
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: handle
    character(len=*),intent(in)  :: path
    integer,         intent(in)  :: flag
    integer,optional,intent(in)  :: unit
    integer f
    integer ch, m
    logical bcache, bseq

    ierr = err_default
    handle = -1
    ! todo: cache-mode check

    f = flag
    if (f.le.0) f = flag_default
    bcache = IAND(f, enable_cache).ne.0
    bseq   = IAND(f, enable_sequential).ne.0
    if (ierr.eq.0) then
       if (bcache) then
          if (bseq) then
             ierr = _ERROR(ERR_NOT_IMPLEMENTED)
          else
             m = mode_cache
             call cache_open_read(ierr, ch, path, flag, unit)
          endif
       else
          m = mode_sequential
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
          ! call ctrl_open_read(ierr, ch, path, flag, unit)
       endif
    endif
    if (ierr.eq.0) call register_file(ierr, handle, ch, m)
  end subroutine nio_open
!!!_  - nio_close - close Nio file
  subroutine nio_close &
       & (ierr, handle, path)
    use TOUZA_Nio_cache,only: cache_close
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: handle
    character(len=*),intent(in),optional :: path
    integer ch, b

    b = query_bind(handle)
    ierr = min(0, b)
    if (ierr.eq.0) then
       ch = is_cache_bind(b)
       if (ch.ge.0) then
          call cache_close(ierr, ch, path)
       else
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       endif
    endif
  end subroutine nio_close
!!!_  - nio_show_status - file diagnostic
  subroutine nio_show_status &
       & (ierr, handle, tag, u, levv)
    use TOUZA_Nio_cache,only: show_cache
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: handle
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer utmp
    integer ch, b
    ierr = 0
    utmp = get_logu(u, ulog)
    b = query_bind(handle)
    ierr = min(0, b)
    if (ierr.eq.0) then
       ch = is_cache_bind(b)
       if (ch.ge.0) then
          call show_cache(ierr, ch, tag, u, levv)
       else
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       endif
    endif
  end subroutine nio_show_status
!!!_  - nio_search
  subroutine nio_search_d &
       & (ierr,   status, &
       &  handle, item,   timel, timeh, func, iniv, inir)
    use TOUZA_Nio_std,only: KTGT=>KDBL
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_cache,only: cache_var_id, cache_time_rec, grp_suite
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: status        ! current status
    integer,         intent(in)  :: handle
    character(len=*),intent(in)  :: item
    real(kind=KTGT), intent(in)  :: timel, timeh
    integer,optional,intent(in)  :: iniv,  inir   ! search initialization flag
    interface
       logical function func(dstr, tstr, timel, timeh)
         use TOUZA_Nio_std,only: KTGT=>KDBL
         implicit none
         character(len=*),intent(in) :: dstr ! date string
         character(len=*),intent(in) :: tstr ! time string
         real(kind=KTGT), intent(in) :: timel, timeh
       end function func
    end interface

    real(kind=KTGT) :: tl, th
    integer b, ch, vid, rid

    ierr = 0
    tl = nio_time_repl(timel, -HUGE(timel))
    th = nio_time_repl(timeh, +HUGE(timeh))

    b = query_bind(handle)
    ierr = min(0, b)
    if (ierr.eq.0) then
       ch = is_cache_bind(b)
       if (ch.ge.0) then
          vid = cache_var_id(item, ch, iniv)
          rid = cache_time_rec(ch, vid, tl, th, func, inir)
          status = rid
       else
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       endif
    endif
    if (ierr.ne.0) status = -1
  end subroutine nio_search_d
!!!_  - nio_time_repl
  PURE &
  real(kind=KTGT) function nio_time_repl_d (v, rep) result (t)
    use TOUZA_Nio_std,only: KTGT=>KDBL
    real(kind=KTGT),intent(in) :: v
    real(kind=KTGT),intent(in) :: rep
    t = v
    if (v.eq.nio_time_undef(v)) t = rep
  end function nio_time_repl_d

!!!_  - nio_time_undef
  PURE &
  real(kind=KTGT) function nio_time_undef_d (mold) result (t)
    use TOUZA_Nio_std,only: KTGT=>KDBL
    real(kind=KTGT),intent(in) :: mold
    t = - HUGE(mold)
  end function nio_time_undef_d

!!!_ + sequential-mode manager
!!!_ + end module
end module TOUZA_Nio_control

!!!_@ test_nio_control - test program
#ifdef TEST_NIO_CONTROL
program test_nio_control
  use TOUZA_Std,only: parse, get_param, arg_diag, arg_init, get_nparam
  use TOUZA_Nio_control
  implicit none

  integer ierr
  integer jarg, narg
  integer,parameter :: lpath = 256
  character(len=lpath) :: file
  integer,parameter   :: lpar = 256
  character(len=lpar) :: carg
  integer handle

  ! usage
  !   test_nio_control FILE [VAR...]


101 format(A,' = ', I0)

  ierr = 0
  narg = 0

  call init(ierr, stdv=-9)
  write(*, 101) 'INIT', ierr
  if (ierr.eq.0) call diag(ierr, levv=+9)
  if (ierr.eq.0) call arg_init(ierr, levv=-9)
  if (ierr.eq.0) call parse(ierr)
  if (ierr.eq.0) call arg_diag(ierr)
  if (ierr.eq.0) narg = get_nparam()
  write(*, 101) 'parse', ierr

301 format('open:', I0, 1x, A,  ' = ', I0)
309 format('close:', I0, 1x, A,  ' = ', I0)

  jarg = 0
  jarg = jarg + 1
  if (ierr.eq.0) call get_param(ierr, file, jarg)
  if (ierr.eq.0) call nio_open(ierr, handle, file, flag_default)
  write(*, 301) ierr, trim(file), handle

  if (ierr.eq.0) call nio_show_status(ierr, handle)
  do
     if (jarg.gt.narg) exit
     if (ierr.eq.0) call get_param(ierr, carg, jarg)
     jarg = jarg + 1
  enddo

  if (ierr.eq.0) call nio_close(ierr, handle, file)
  write(*, 309) ierr, trim(file), handle

  if (ierr.eq.0) call finalize(ierr)
  write(*, 101) 'FINAL', ierr
  stop
contains
end program test_nio_control

#endif /* TEST_NIO_CONTROL */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

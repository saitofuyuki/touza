!!!_! std.F90 - touza/std interfaces
! Maintainer: SAITO Fuyuki
! Created: Jun 4 2020
#define TIME_STAMP 'Time-stamp: <2023/03/25 13:22:04 fuyuki std.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2020, 2021, 2022, 2023
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
!!!_@ TOUZA_Std - standard interfaces
module TOUZA_Std
!!!_ + Declaration
!!!_  - modules
  use TOUZA_Std_prc, prc_init=>init, prc_diag=>diag, prc_finalize=>finalize, pset_defu=>set_defu
  use TOUZA_Std_utl, utl_init=>init, utl_diag=>diag, utl_finalize=>finalize, uset_defu=>set_defu
  use TOUZA_Std_env, env_init=>init, env_diag=>diag, env_finalize=>finalize
  use TOUZA_Std_log, log_init=>init, log_diag=>diag, log_finalize=>finalize
  use TOUZA_Std_fun, fun_init=>init, fun_diag=>diag, fun_finalize=>finalize
  use TOUZA_Std_arg, arg_init=>init, arg_diag=>diag, arg_finalize=>finalize
  use TOUZA_Std_mwe, mwe_init=>init, mwe_diag=>diag, mwe_finalize=>finalize
  use TOUZA_Std_sus, sus_init=>init, sus_diag=>diag, sus_finalize=>finalize
  use TOUZA_Std_bld, bld_init=>init, bld_diag=>diag, bld_finalize=>finalize
  use TOUZA_Std_wsh, wsh_init=>init, wsh_diag=>diag, wsh_finalize=>finalize
  use TOUZA_Std_htb, htb_init=>init, htb_diag=>diag, htb_finalize=>finalize
  use TOUZA_Std_ipc, ipc_init=>init, ipc_diag=>diag, ipc_finalize=>finalize
!!!_  - default
  implicit none
  public
!!!_  - static
#define __GRP__ GROUP_TAG
  integer,save,private :: init_mode = 0
  integer,save,private :: init_counts = 0
  integer,save,private :: diag_counts = 0
  integer,save,private :: fine_counts = 0
  integer,save,private :: lev_verbose = 0
  integer,save,private :: err_default = ERR_NO_INIT
  integer,save,private :: ulog = unit_global

  character(len=64),save,private :: tagmsg = ' '
!!!_  - public
  public init, diag, finalize

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init &
       & (ierr,   &
       &  u,      levv, mode, &
       &  envtry, icomm)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode

    integer,intent(in),optional :: envtry
    integer,intent(in),optional :: icomm

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
          if (ierr.eq.0) call prc_init(ierr, u=ulog, levv=lv, mode=lmd)

          if (ierr.eq.0) call utl_init(ierr, u=ulog, levv=lv, mode=MODE_SURFACE) ! prc

          if (ierr.eq.0) call wsh_init(ierr, u=ulog, levv=lv, mode=MODE_SURFACE) ! prc utl
          if (ierr.eq.0) call log_init(ierr, u=ulog, levv=lv, mode=MODE_SURFACE) ! prc utl

          if (ierr.eq.0) call mwe_init(ierr, u=ulog, levv=lv, mode=MODE_SURFACE, icomm=icomm) ! utl log
          if (ierr.eq.0) call bld_init(ierr, u=ulog, levv=lv, mode=MODE_SURFACE) ! utl log
          if (ierr.eq.0) call htb_init(ierr, u=ulog, levv=lv, mode=MODE_SURFACE) ! utl log

          if (ierr.eq.0) call ipc_init(ierr, u=ulog, levv=lv, mode=MODE_SURFACE) ! prc utl log

          if (ierr.eq.0) call fun_init(ierr, u=ulog, levv=lv, mode=MODE_SURFACE) ! utl log mwe

          if (ierr.eq.0) call env_init(ierr, u=ulog, levv=lv, mode=MODE_SURFACE, levtry=envtry) ! prc utl log fun mwe

          if (ierr.eq.0) call sus_init(ierr, u=ulog, levv=lv, mode=MODE_SURFACE) ! prc utl log fun env
          if (ierr.eq.0) call arg_init(ierr, u=ulog, levv=lv, mode=MODE_SURFACE) ! prc utl log fun env
       endif
       if (is_first_force(init_counts, mode)) then
          call gen_tag(tagmsg, pkg=PACKAGE_TAG, grp=__GRP__)
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
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: u
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(uout, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (VCHECK_NORMAL(lv)) call msg(TIME_STAMP, tagmsg, u=utmp)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call prc_diag(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call utl_diag(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call wsh_diag(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call log_diag(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call mwe_diag(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call bld_diag(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call htb_diag(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call ipc_diag(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call fun_diag(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call env_diag(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call sus_diag(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call arg_diag(ierr, utmp, lv, mode=MODE_SURFACE)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag
!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: u
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(uout, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call prc_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call utl_finalize(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call wsh_finalize(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call log_finalize(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call mwe_finalize(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call bld_finalize(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call htb_finalize(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call ipc_finalize(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call fun_finalize(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call env_finalize(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call sus_finalize(ierr, utmp, lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call arg_finalize(ierr, utmp, lv, mode=MODE_SURFACE)
       endif
       fine_counts = fine_counts + 1
    endif

    return
  end subroutine finalize
end module TOUZA_Std
!!!_@ test_std - test program
#ifdef TEST_STD
program test_std
  use TOUZA_Std
  implicit none
  character(len=128) :: T
  integer ierr, levv

  call get_command_argument(1, T, STATUS=ierr)
  if (ierr.eq.0) then
     read(T, *) levv
  else
     levv = 0
  endif

111 format('TEST: levv=', I0)
101 format(A, ' = ', I0)
  ierr = 0
  write(*, 111) levv
  if (ierr.eq.0) call init(ierr, levv=levv)
  write(*, 101) 'init:0', ierr
  if (ierr.eq.0) call init(ierr, levv=levv)
  write(*, 101) 'init:1', ierr
  if (ierr.eq.0) call diag(ierr)
  write(*, 101) 'diag', ierr
  if (ierr.eq.0) call finalize(ierr)
  write(*, 101) 'fine', ierr
  stop
end program test_std

#endif /* TEST_STD */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

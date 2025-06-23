!!!_! std_mwe.F90 - touza/std MPI wrapper emulator
! Maintainer: SAITO Fuyuki
! Created: Nov 30 2020
#define TIME_STAMP 'Time-stamp: <2025/06/23 21:45:34 fuyuki std_mwe.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2020-2025
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
!!!_@ TOUZA_Std_mwe - MPI wrapper/emulator/dummy interfaces
module TOUZA_Std_mwe
!!!_ + declaration
!!!_  - modules
#if OPT_USE_MPI
  use mpi
#endif
  use TOUZA_Std_log,only: unit_global,  trace_fine,   trace_control
!!!_  - default
  implicit none
  private
!!!_  - parameter
  integer,parameter :: switch_error    = -999
  integer,parameter :: switch_enabled  = +1    /* MPI enabled */
  integer,parameter :: switch_disabled = 0     /* MPI included but disabled */
  integer,parameter :: switch_excluded = -1    /* MPI excluded */
  ! mpi_comm_comp results and more
  integer,parameter,public :: cc_ident = 0
  integer,parameter,public :: cc_congruent = 1
  integer,parameter,public :: cc_similar = 2
  integer,parameter,public :: cc_both_null = 3
  integer,parameter,public :: cc_unequal = 4
  integer,parameter,public :: cc_either_null = 5
#if OPT_USE_MPI
#else  /* not OPT_USE_MPI */
  ! dummy parameters
  integer,parameter :: MPI_SUCCESS = 0
  integer,parameter :: MPI_COMM_NULL  = 2
  integer,parameter :: MPI_COMM_SELF  = 1
  integer,parameter :: MPI_COMM_WORLD = 0
  integer,parameter :: MPI_UNDEFINED = -32766
  integer,parameter :: MPI_DATATYPE_NULL = 0
  integer,parameter :: MPI_GROUP_NULL = 0
  integer,parameter :: MPI_GROUP_EMPTY = 1
  integer,parameter :: MPI_ANY_TAG = -1
  integer,parameter :: MPI_ANY_SOURCE = -1
  integer,parameter :: MPI_INTEGER = 7
  integer,parameter :: MPI_CHARACTER = 5
  integer,parameter :: MPI_DOUBLE_PRECISION = 17
  integer,parameter :: MPI_STATUS_SIZE = 6
  integer,parameter :: MPI_IDENT = 0
  integer,parameter :: MPI_CONGRUENT = 1
  integer,parameter :: MPI_SIMILAR = 2
  integer,parameter :: MPI_UNEQUAL = 3
#endif /* not OPT_USE_MPI */
!!!_  - static
#define __MDL__ 'mwe'
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global

  integer,save :: icomm_default = MPI_COMM_NULL
  integer,save :: switch_mpi = switch_error

# define _ERROR(E) (E - ERR_MASK_STD_MWE)
!!!_  - public
  public init, diag, finalize
  public set_comm, get_comm
  public get_ni,   get_ni_safe
  public get_wni,  get_wni_safe
  public get_gni
  public comp_comms, comp_groups
  public is_mpi_activated
  public safe_mpi_init, safe_mpi_finalize
  public show_mpi_type
  public MPI_SUCCESS
  public MPI_COMM_WORLD, MPI_COMM_SELF, MPI_COMM_NULL
  public MPI_DATATYPE_NULL, MPI_GROUP_NULL, MPI_UNDEFINED
  public MPI_STATUS_SIZE,   MPI_GROUP_EMPTY
  public MPI_INTEGER,       MPI_CHARACTER
  public MPI_ANY_TAG,       MPI_ANY_SOURCE
  public MPI_DOUBLE_PRECISION
  public MPI_GROUP_TRANSLATE_RANKS, MPI_GROUP_SIZE, MPI_GROUP_RANK, MPI_GROUP_UNION
  public MPI_COMM_CREATE,   MPI_COMM_SPLIT, MPI_COMM_GROUP
  public MPI_WAIT, MPI_BARRIER
  public MPI_PROBE, MPI_GET_COUNT
  public MPI_COMM_COMPARE, MPI_IDENT, MPI_CONGRUENT, MPI_SIMILAR, MPI_UNEQUAL
  public MPI_GROUP_COMPARE

!!!_  - misc
  character(len=128) tmsg
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, icomm)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: choice
    ! use TOUZA_Std_utl,only: utl_init=>init ! included by TOUZA_Std_log
    use TOUZA_Std_log,only: log_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: icomm   ! communicator to apply among TOUZA system
    integer md, lv, lmd

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
          ! if (ierr.eq.0) call utl_init(ierr, ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call log_init(ierr, ulog, levv=lv, mode=lmd)
       endif
       if (is_first_force(init_counts, mode)) then
          if (ierr.eq.0) call set_comm(ierr, icomm, ulog, levv)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = _ERROR(ERR_FAILURE_INIT)
    endif
    return
  end subroutine init
!!!_  & diag
  subroutine diag(ierr, u, levv, mode, icomm)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: choice
    ! use TOUZA_Std_utl,only: utl_diag=>diag, choice
    use TOUZA_Std_log,only: log_diag=>diag, msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: icomm
    integer ir, nr
    integer utmp, md, lv, lmd
    integer jerr

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (VCHECK_NORMAL(lv)) call msg_mdl(TIME_STAMP, __MDL__, utmp)
          if (VCHECK_NORMAL(lv)) call msg_mdl('(''mpi switch = '', I0, )', switch_mpi, __MDL__, utmp)
#if OPT_USE_MPI
          if (VCHECK_DEBUG(lv)) then
             call msg_mdl('(''mpi_comm_world = '', I0, )', MPI_COMM_WORLD, __MDL__, utmp)
             call msg_mdl('(''mpi_comm_null = '', I0, )',  MPI_COMM_NULL, __MDL__, utmp)
          endif
#endif
          if (switch_mpi.gt.0) then
             if (VCHECK_NORMAL(lv)) then
                call get_ni_safe(ierr, nr, ir, icomm)
101             format('ranks = ', I0, 1x, I0, 1x, I0)
                if (present(icomm)) then
                   write(tmsg, 101, IOSTAT=jerr) ir, nr, icomm
                else
                   write(tmsg, 101, IOSTAT=jerr) ir, nr
                endif
                call msg_mdl(tmsg, __MDL__, utmp)
             endif
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          ! if (ierr.eq.0) call utl_diag(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call log_diag(ierr, utmp, lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: choice
    ! use TOUZA_Std_utl,only: utl_finalize=>finalize
    use TOUZA_Std_log,only: log_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
#if OPT_USE_MPI
       if (ierr.eq.0) then
          if (switch_mpi.gt.0) then
             if (is_mpi_activated()) then
                call MPI_finalize(ierr)
             endif
          endif
       endif
#endif /* OPT_USE_MPI */
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          ! if (ierr.eq.0) call utl_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call log_finalize(ierr, utmp, lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif

    return
  end subroutine finalize
!!!_ + init subcontracts
!!!_  - set_comm
  subroutine set_comm(ierr, icomm, u, levv)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl, is_msglev_WARNING
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: icomm
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer ic, ir
    logical isini
    integer lv, utmp

    ierr = 0

    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (switch_mpi.ne.switch_error) then
       if (.not.present(icomm)) return    ! not touch current switch
       if (is_msglev_WARNING(lv)) then
          call msg_mdl('(''TOUZA communicator changed:'', I0)', icomm, __MDL__, utmp)
       endif
    endif
#if OPT_USE_MPI
    ic = choice(MPI_COMM_WORLD, icomm)
    if (ic.eq.MPI_COMM_NULL) then
       switch_mpi = switch_disabled
       if (VCHECK_NORMAL(lv)) call msg_mdl('mpi disabled', __MDL__, utmp)
    else
       switch_mpi = switch_enabled
       call MPI_Initialized(isini, ierr)
       if (ierr.eq.0) then
          if (ic.ne.MPI_COMM_WORLD .and. (.not.isini)) then
             call msg_mdl('(''mpi not initialized ='', I0)', ic, __MDL__, utmp)
             ierr = _ERROR(ERR_MPI_PANIC)
          else
             if (.not.isini) call MPI_Init(ierr)
          endif
       endif
       if (ierr.eq.0) then
          call MPI_Comm_rank(ic, ir, ierr)
          if (ierr.ne.MPI_SUCCESS) then
             if (VCHECK_FATAL(lv)) then
                call msg_mdl('(''invalid communicator ='', I0)', ic, __MDL__, utmp)
                ierr = _ERROR(ERR_MPI_PANIC)
             endif
          endif
       else
          ic = MPI_COMM_NULL
       endif
    endif
#else  /* not OPT_USE_MPI */
    switch_mpi = switch_excluded
    ic = MPI_COMM_NULL
    if (choice(ic, icomm).ne.ic) then
       if (VCHECK_FATAL(lv)) call msg_mdl('cannot enable mpi.', __MDL__, utmp)
       ierr = _ERROR(ERR_OPR_DISABLE)
    endif
#endif /* not OPT_USE_MPI */
    icomm_default = ic
    return
  end subroutine set_comm

!!!_ + user subroutines
!!!_  & get_comm - return TOUZA communicator
  subroutine get_comm &
       & (ierr, icomm)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: icomm
    ierr = 0
    icomm = icomm_default
    return
  end subroutine get_comm
!!!_  & get_ni - return rank and size (guard MPI_COMM_NULL)
  subroutine get_ni &
       & (ierr, nrank, irank, icomm)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: nrank
    integer,intent(out)         :: irank
    integer,intent(in),optional :: icomm
    integer ic
    ierr = err_default
    if (ierr.eq.0) then
#if OPT_USE_MPI
       ic = choice(icomm_default, icomm)
       if (switch_mpi.eq.switch_enabled) then
          if (ic.ne.MPI_COMM_NULL) then
             if (ierr.eq.0) call MPI_Comm_size(ic, nrank, ierr)
             if (ierr.eq.0) call MPI_Comm_rank(ic, irank, ierr)
             if (ierr.ne.0) then
                nrank = -1
                irank = -1
             endif
          else
             nrank = 0
             irank = 0
          endif
       else
          nrank = 0
          irank = 0
       endif
#else  /* not OPT_USE_MPI */
       nrank = 0
       irank = 0
#endif /* not OPT_USE_MPI */
    else
       nrank = -1
       irank = -1
    endif
    return
  end subroutine get_ni

!!!_  & get_ni_safe - return rank and size (guard MPI_COMM_NULL) (check mpi)
  subroutine get_ni_safe &
       & (ierr, nrank, irank, icomm)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: nrank
    integer,intent(out)         :: irank
    integer,intent(in),optional :: icomm
    integer ic
    ierr = err_default
    if (ierr.eq.0) then
#if OPT_USE_MPI
       ic = choice(icomm_default, icomm)
       if (switch_mpi.eq.switch_enabled) then
          if (is_mpi_activated()) then
             nrank = -1
             irank = -1
             if (ic.ne.MPI_COMM_NULL) then
                if (ierr.eq.0) call MPI_Comm_size(ic, nrank, ierr)
                if (ierr.eq.0) call MPI_Comm_rank(ic, irank, ierr)
             endif
          else
             nrank = -9
             irank = -9
          endif
       else
          nrank = 0
          irank = 0
       endif
#else  /* not OPT_USE_MPI */
       nrank = 0
       irank = 0
#endif /* not OPT_USE_MPI */
    else
       nrank = -1
       irank = -1
    endif
    return
  end subroutine get_ni_safe

!!!_  & get_gni - return rank and size from group (guard MPI_GROUP_NULL)
  subroutine get_gni &
       & (ierr, igrp, nrank, irank)
    use TOUZA_Std_utl,only: choice, set_if_present
    implicit none
    integer,intent(out)          :: ierr
    integer,intent(in)           :: igrp
    integer,intent(out),optional :: nrank
    integer,intent(out),optional :: irank
    ierr = err_default
    if (ierr.eq.0) then
#if OPT_USE_MPI
       if (switch_mpi.eq.switch_enabled) then
          if (igrp.ne.MPI_GROUP_NULL) then
             if (present(nrank)) then
                nrank = -1
                if (ierr.eq.0) call MPI_Group_size(igrp, nrank, ierr)
             endif
             if (present(irank)) then
                irank = -1
                if (ierr.eq.0) call MPI_Group_rank(igrp, irank, ierr)
                if (irank.eq.MPI_UNDEFINED) irank = -1
             endif
          else
             call set_if_present(irank, -1)
             call set_if_present(nrank, -1)
          endif
       else
          call set_if_present(irank, 0)
          call set_if_present(nrank, 0)
       endif
#else  /* not OPT_USE_MPI */
       call set_if_present(irank, 0)
       call set_if_present(nrank, 0)
#endif /* not OPT_USE_MPI */
    else
       call set_if_present(irank, -2)
       call set_if_present(nrank, -2)
    endif
    return
  end subroutine get_gni
!!!_  & get_wni
  subroutine get_wni &
       & (ierr, nrank, irank, icomm)
    use TOUZA_Std_utl,only: choice, set_if_present
    implicit none
    integer,intent(out)          :: ierr
    integer,intent(out),optional :: nrank
    integer,intent(out),optional :: irank
    integer,intent(out),optional :: icomm

    ierr = err_default
#if OPT_USE_MPI
    if (ierr.eq.0) then
       if (switch_mpi.eq.switch_enabled) then
          if (present(nrank)) then
             nrank = -1
             if (ierr.eq.0) call MPI_Comm_size(MPI_COMM_WORLD, nrank, ierr)
          endif
          if (present(irank)) then
             irank = -1
             if (ierr.eq.0) call MPI_Comm_rank(MPI_COMM_WORLD, irank, ierr)
          endif
          call set_if_present(icomm, MPI_COMM_WORLD)
       else
          call set_if_present(irank, -1)
          call set_if_present(nrank, -1)
          call set_if_present(icomm, -1)
       endif
    else
       call set_if_present(irank, -2)
       call set_if_present(nrank, -2)
       call set_if_present(icomm, -2)
    endif
#else  /* not OPT_USE_MPI */
    call set_if_present(irank, 0)
    call set_if_present(nrank, 0)
    call set_if_present(icomm, 0)
#endif  /* not OPT_USE_MPI */
    return
  end subroutine get_wni
!!!_  & get_wni_safe
  subroutine get_wni_safe &
       & (ierr, nrank, irank, icomm)
    use TOUZA_Std_utl,only: choice, set_if_present
    implicit none
    integer,intent(out)          :: ierr
    integer,intent(out),optional :: nrank
    integer,intent(out),optional :: irank
    integer,intent(out),optional :: icomm

    ierr = err_default
    if (ierr.eq.0) then
       if (is_mpi_activated()) then
          call get_wni(ierr, nrank, irank, icomm)
       else
          call set_if_present(nrank, -9)
          call set_if_present(irank, -9)
          call set_if_present(icomm, MPI_COMM_WORLD)
       endif
    endif
    return
  end subroutine get_wni_safe
!!!_  & is_mpi_activated () - check if during init and finalize
  logical function is_mpi_activated () result(b)
    implicit none
#if OPT_USE_MPI
    integer jerr
    call MPI_Initialized(b, jerr)
    if (jerr.ne.0) b = .FALSE.
    if (b) then
       call MPI_Finalized(b, jerr)
       if (jerr.eq.0) then
          b = .not. b
       else
          b = .FALSE.
       endif
    endif
#else
    b = .FALSE.
#endif
    return
  end function is_mpi_activated

!!!_  & comp_comms - wrapper for MPI_Comm_compare
  integer function comp_comms(icomm0, icomm1) result(e)
    implicit none
    integer,intent(in) :: icomm0, icomm1
    integer :: jerr
    if (icomm0.eq.MPI_COMM_NULL) then
       if (icomm1.eq.MPI_COMM_NULL) then
          e = cc_both_null
       else
          e = cc_either_null
       endif
    else if (icomm1.eq.MPI_COMM_NULL) then
       e = cc_either_null
    else
       call MPI_Comm_compare(icomm0, icomm1, e, jerr)
       if (jerr.ne.MPI_SUCCESS) then
          e = _ERROR(ERR_PANIC)
       else
          select case (e)
          case (MPI_IDENT)
             e = cc_ident
          case (MPI_CONGRUENT)
             e = cc_congruent
          case (MPI_SIMILAR)
             e = cc_similar
          case (MPI_UNEQUAL)
             e = cc_unequal
          case default
             e = _ERROR(ERR_INVALID_SWITCH)
          end select
       endif
    endif
  end function comp_comms
!!!_  & comp_groups - wrapper for MPI_Group_compare
  integer function comp_groups(igrp0, igrp1) result(e)
    implicit none
    integer,intent(in) :: igrp0, igrp1
    integer :: jerr
    if (igrp0.eq.MPI_GROUP_NULL) then
       if (igrp1.eq.MPI_GROUP_NULL) then
          e = cc_both_null
       else
          e = cc_either_null
       endif
    else if (igrp1.eq.MPI_GROUP_NULL) then
       e = cc_either_null
    else
       call MPI_GROUP_compare(igrp0, igrp1, e, jerr)
       if (jerr.ne.MPI_SUCCESS) then
          e = _ERROR(ERR_PANIC)
       else
          select case (e)
          case (MPI_IDENT)
             e = cc_ident
          case (MPI_SIMILAR)
             e = cc_similar
          case (MPI_UNEQUAL)
             e = cc_unequal
          case default
             e = _ERROR(ERR_INVALID_SWITCH)
          end select
       endif
    endif
  end function comp_groups
!!!_  & safe_mpi_init
  subroutine safe_mpi_init(ierr)
    implicit none
    integer,intent(out) :: ierr
#if OPT_USE_MPI
    logical b
    call MPI_Initialized(b, ierr)
    if (ierr.eq.0) then
       if (.not.b) call MPI_Init(ierr)
    endif
#else
    ierr = 0
#endif
  end subroutine safe_mpi_init
!!!_  & safe_mpi_finalize
  subroutine safe_mpi_finalize(ierr)
    implicit none
    integer,intent(out) :: ierr
#if OPT_USE_MPI
    logical b
    call MPI_Finalized(b, ierr)
    if (ierr.eq.0) then
       if (.not.b) call MPI_Finalize(ierr)
    endif
#else
    ierr = 0
#endif
  end subroutine safe_mpi_finalize
!!!_  & show_mpi_type - check properties of mpi-type
  subroutine show_mpi_type &
       & (ierr, mt, tag, u)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: mt
    character(len=*),intent(in)  :: tag
    integer,optional,intent(in)  :: u
    integer utmp
#if OPT_USE_MPI
    integer sz
    integer(kind=MPI_ADDRESS_KIND) :: lb, ex
#endif
    ierr = 0
    utmp = choice(-1, u)
#if OPT_USE_MPI
    if (mt.eq.MPI_DATATYPE_NULL) then
       sz = 0
       lb = 0
       ex = 0
    else
       if (ierr.eq.0) call MPI_Type_size(mt, sz, ierr)
       if (ierr.eq.0) call MPI_Type_get_extent(mt, lb, ex, ierr)
    endif
101 format('mpi-type:', A, ': ', I0, 1x, I0, '+', I0)
109 format('mpi-type:', A, ': error = ', I0)
    if (ierr.eq.0) then
       if (utmp.ge.0) then
          write(utmp, 101) trim(tag), sz, lb, ex
       else if (utmp.eq.-1) then
          write(*,    101) trim(tag), sz, lb, ex
       endif
    else
       if (utmp.ge.0) then
          write(utmp, 109) trim(tag), ierr
       else if (utmp.eq.-1) then
          write(*,    109) trim(tag), ierr
       endif
    endif
#else
    ierr = _ERROR(ERR_MPI_PANIC)
111 format('mpi-type:', A, ': PANIC')
    if (utmp.ge.0) then
       write(utmp, 111) trim(tag)
    else if (utmp.eq.-1) then
       write(*,    111) trim(tag)
    endif
#endif
  end subroutine show_mpi_type
!!!_ + dummy interfaces
#if OPT_USE_MPI
#else /* not OPT_USE_MPI */
  subroutine MPI_COMM_CREATE &
       & (COMM, GROUP, NEWCOMM, IERROR)
    implicit none
    INTEGER   COMM, GROUP, NEWCOMM, IERROR
    IERROR = ERR_NOT_IMPLEMENTED
  end subroutine MPI_COMM_CREATE

  subroutine MPI_COMM_SPLIT &
       & (COMM, COLOR, KEY, NEWCOMM, IERROR)
    implicit none
    INTEGER   COMM, COLOR, KEY, NEWCOMM, IERROR
    IERROR = ERR_NOT_IMPLEMENTED
  end subroutine MPI_COMM_SPLIT

  subroutine MPI_COMM_GROUP &
       & (COMM, GROUP, IERROR)
    implicit none
    INTEGER   COMM, GROUP, IERROR
    IERROR = ERR_NOT_IMPLEMENTED
  end subroutine MPI_COMM_GROUP

  subroutine MPI_GROUP_SIZE &
       & (GROUP, SIZE, IERROR)
    implicit none
    INTEGER   GROUP, SIZE, IERROR
    IERROR = ERR_NOT_IMPLEMENTED
  end subroutine MPI_GROUP_SIZE

  subroutine MPI_GROUP_RANK &
       & (GROUP, RANK, IERROR)
    implicit none
    INTEGER   GROUP, RANK, IERROR
    IERROR = ERR_NOT_IMPLEMENTED
  end subroutine MPI_GROUP_RANK

  subroutine MPI_GROUP_UNION &
       & (GROUP1, GROUP2, NEWGROUP, IERROR)
    implicit none
    INTEGER   GROUP1, GROUP2, NEWGROUP, IERROR
    IERROR = ERR_NOT_IMPLEMENTED
  end subroutine MPI_GROUP_UNION

  subroutine MPI_WAIT &
       & (REQUEST, STATUS, IERROR)
    implicit none
    INTEGER   REQUEST, STATUS(*), IERROR
    IERROR = ERR_NOT_IMPLEMENTED
  end subroutine MPI_WAIT

  subroutine MPI_BARRIER &
       & (COMM, IERROR)
    implicit none
    INTEGER   COMM, IERROR
    IERROR = ERR_NOT_IMPLEMENTED
  end subroutine MPI_BARRIER

  subroutine MPI_GET_COUNT &
       & (STATUS, DATATYPE, COUNT, IERROR)
    implicit none
    INTEGER   STATUS(*), DATATYPE, COUNT, IERROR
    IERROR = ERR_NOT_IMPLEMENTED
  end subroutine MPI_GET_COUNT

  subroutine MPI_PROBE &
       & (SOURCE, TAG, COMM, STATUS, IERROR)
    implicit none
    INTEGER   SOURCE, TAG, COMM, STATUS(*), IERROR
    IERROR = ERR_NOT_IMPLEMENTED
  end subroutine MPI_PROBE

  subroutine MPI_COMM_COMPARE &
       & (COMM1, COMM2, RESULT, IERROR)
    implicit none
    INTEGER COMM1, COMM2, RESULT, IERROR
    RESULT = MPI_UNEQUAL
    IERROR = ERR_NOT_IMPLEMENTED
  end subroutine MPI_COMM_COMPARE

  subroutine MPI_GROUP_COMPARE &
       & (GROUP1, GROUP2, RESULT, IERROR)
    implicit none
    INTEGER GROUP1, GROUP2, RESULT, IERROR
    RESULT = MPI_UNEQUAL
    IERROR = ERR_NOT_IMPLEMENTED
  end subroutine MPI_GROUP_COMPARE

#endif /* not OPT_USE_MPI */
#if HAVE_FORTRAN_MPI_MPI_GROUP_TRANSLATE_RANKS
#else
  subroutine MPI_GROUP_TRANSLATE_RANKS &
       & (GROUP1, N, RANKS1, GROUP2, RANKS2, IERROR)
    implicit none
    INTEGER GROUP1, N, RANKS1(*), GROUP2, RANKS2(*), IERROR
    IERROR = ERR_NOT_IMPLEMENTED
  end subroutine MPI_GROUP_TRANSLATE_RANKS
#endif
end module TOUZA_Std_mwe
!!!_@ test_std_mpi - test program
#ifdef TEST_STD_MWE
program test_std_mwe
  use TOUZA_Std_mwe
  use TOUZA_Std_utl,only: parse_number
  implicit none
  integer ierr
  integer jarg
  character(len=128) :: arg
  integer ktest
  integer ibase
  integer icomm, ir, nr, icol

  jarg = 1
  call get_command_argument(jarg, arg, status=ierr)
  if (ierr.ne.0) arg = '0'
  call parse_number(ierr, ktest, arg)
  if (ierr.ne.0) ktest = 0
  ierr = 0
  write(*, *) 'test = ', ktest

  icol = 0

  if (ktest.eq.0) then
     icomm = MPI_COMM_WORLD
  else if (ktest.lt.0) then
     icomm = MPI_COMM_NULL
  else
     call MPI_Init(ierr)
     ibase = MPI_COMM_WORLD
     if (ierr.eq.0) call MPI_Comm_size(ibase, nr, ierr)
     if (ierr.eq.0) call MPI_Comm_rank(ibase, ir, ierr)
     if (ierr.eq.0) then
        if (ktest.eq.1) then
           if (ir .ge. nr / 2) icol = MPI_UNDEFINED
        else
           if (ir .lt. nr / 2) icol = MPI_UNDEFINED
        endif
     endif
     if (ierr.eq.0) call MPI_Comm_split(ibase, icol, ir, icomm, ierr)
  endif

  if (icol.ne.MPI_UNDEFINED) then
     ! call init(ierr, disable=.TRUE.)
     if (icomm.eq.MPI_COMM_WORLD) then
        call init(ierr)
     else
        call init(ierr, icomm=icomm)
     endif
     if (ierr.eq.0) call diag(ierr, levv=+10)
     if (ierr.eq.0) call finalize(ierr, levv=+10)
  else
     write(*, *) 'SKIPPED'
     ierr = 0
  endif

  call safe_mpi_finalize(ierr)

101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_std_mwe

#endif /* TEST_STD_MWE */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

!!!_! std_mwe.F90 - touza/std MPI wrapper emulator
! Maintainer: SAITO Fuyuki
! Created: Nov 30 2020
#define TIME_STAMP 'Time-stamp: <2021/11/21 10:11:06 fuyuki std_mwe.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2020, 2021
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
!!!_@ TOUZA_Std_mwe - MPI wrapper/emulator
module TOUZA_Std_mwe
!!!_ + declaration
!!!_  - modules
#if OPT_USE_MPI
  use mpi
#else
#  define MPI_COMM_NULL 0
#endif /* OPT_USE_MPI */
  use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
  use TOUZA_Std_log,only: unit_global,  trace_fine,   trace_control
!!!_  - default
  implicit none
  private
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
!!!_  - public
  public init, diag, finalize
  public get_ni
!!!_  - misc
  character(len=128) tmsg
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, icomm, irank)
    use TOUZA_Std_log,only: log_init=>init
    use TOUZA_Std_utl,only: utl_init=>init, choice, set_if_present
    implicit none
    integer,intent(out)          :: ierr
    integer,intent(in), optional :: u
    integer,intent(in), optional :: levv
    integer,intent(in), optional :: mode
    integer,intent(out),optional :: icomm ! default commnunicator (world)
    integer,intent(out),optional :: irank ! rank in icomm
    integer md, lv, lmd

    ierr = 0

    md = control_mode(mode, MODE_DEEPEST)
    init_mode = md

    if (md.ge.MODE_SURFACE) then
       err_default = ERR_SUCCESS
       lv = choice(lev_verbose, levv)
       if (is_first_force(init_counts, md)) then
          ulog = choice(ulog, u)
          lev_verbose = lv
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call utl_init(ierr, ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call log_init(ierr, ulog, levv=lv, mode=lmd)
       endif
       if (is_first_force(init_counts, md)) then
          if (ierr.eq.0) call init_batch(ierr, icomm, irank, ulog, levv)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT - ERR_MASK_STD_MWE
    endif
    return
  end subroutine init
!!!_  & diag
  subroutine diag(ierr, u, levv, mode, icomm)
    use TOUZA_Std_utl,only: utl_diag=>diag, choice
    use TOUZA_Std_log,only: log_diag=>diag, msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: icomm
    integer ir, nr
    integer utmp, md, lv, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, md)) then
          if (VCHECK_NORMAL(lv)) call msg_mdl(TIME_STAMP, __MDL__, utmp)
          if (VCHECK_NORMAL(lv)) then
             call get_ni(ierr, nr, ir, icomm)
101          format('ranks = ', I0, 1x, I0, 1x, I0)
             if (present(icomm)) then
                write(tmsg, 101) ir, nr, icomm
             else
                write(tmsg, 101) ir, nr
             endif
             call msg_mdl(tmsg, __MDL__, utmp)
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call utl_diag(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call log_diag(ierr, utmp, lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: utl_finalize=>finalize, choice
    use TOUZA_Std_log,only: log_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer utmp, lv, md, lmd
    logical isfin

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, md)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
#if OPT_USE_MPI
       if (ierr.eq.0) then
          call MPI_Finalized(isfin, ierr)
          if (ierr.eq.0) then
             if (.not.isfin) call MPI_finalize(ierr)
          endif
       endif
#endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call utl_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call log_finalize(ierr, utmp, lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif

    return
  end subroutine finalize
!!!_ + init subcontracts
!!!_  - init_batch
  subroutine init_batch(ierr, icomm, irank, u, levv)
    use TOUZA_Std_utl,only: choice, set_if_present
    implicit none
    integer,intent(out)          :: ierr
    integer,intent(out),optional :: icomm ! default commnunicator (world)
    integer,intent(out),optional :: irank ! rank in icomm
    integer,intent(in), optional :: u
    integer,intent(in), optional :: levv
    integer ic, ir
    logical isini

    ierr = 0

#if OPT_USE_MPI
    if (ierr.eq.0) then
       call MPI_Initialized(isini, ierr)
       if (ierr.eq.0) then
          if (.not.isini) call MPI_Init(ierr)
       endif
    endif
    if (ierr.eq.0) then
       ic = MPI_COMM_WORLD
       call MPI_Comm_rank(ic, ir, ierr)
    else
       ic = MPI_COMM_NULL
       ir = -1
    endif
#else  /* not OPT_USE_MPI */
    ic = MPI_COMM_NULL
    ir = -1
#endif /* not OPT_USE_MPI */
    call set_if_present(icomm, ic)
    call set_if_present(irank, ir)
    icomm_default = ic
    return
  end subroutine init_batch

!!!_ + user subroutines
!!!_  & get_ni - return rank and size
  subroutine get_ni &
       & (ierr, nrank, irank, icomm)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: nrank
    integer,intent(out)         :: irank
    integer,intent(in),optional :: icomm
    integer ic
    ierr = 0
    ic = choice(icomm_default, icomm)
    nrank = -1
    irank = -1
#if OPT_USE_MPI
    if (ierr.eq.0) call MPI_Comm_size(ic, nrank, ierr)
    if (ierr.eq.0) call MPI_Comm_rank(ic, irank, ierr)
#else  /* not OPT_USE_MPI */
    nrank = 0
    irank = 0
#endif /* not OPT_USE_MPI */
    return
  end subroutine get_ni
end module TOUZA_Std_mwe
!!!_@ test_std_mpi - test program
#ifdef TEST_STD_MWE
program test_std_mwe
  use TOUZA_Std_mwe
  implicit none
  integer ierr

  call init(ierr)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call finalize(ierr, levv=+10)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_std_mwe

#endif /* TEST_STD_MWE */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

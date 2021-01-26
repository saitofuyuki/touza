!!!_! std_mwe.F90 - touza/std MPI wrapper emulator
! Maintainer: SAITO Fuyuki
! Created: Nov 30 2020
#define TIME_STAMP 'Time-stamp: <2021/01/26 15:15:55 fuyuki std_mwe.F90>'
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
!!!_  - default
  implicit none
  private
!!!_  - static
#define __MDL__ 'mwe'
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL

  integer,save :: icomm_default = MPI_COMM_NULL
!!!_  - public
  public init, diag, finalize
  public get_ni
!!!_  - misc
  character(len=128) tmsg
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, icomm, irank, levv, mode)
    use TOUZA_Std_log,only: log_init=>init
    use TOUZA_Std_utl,only: utl_init=>init, choice, set_if_present
    implicit none
    integer,intent(out)          :: ierr
    integer,intent(out),optional :: icomm ! default commnunicator (world)
    integer,intent(out),optional :: irank ! rank in icomm
    integer,intent(in), optional :: levv
    integer,intent(in), optional :: mode
    integer ic, ir
    integer md, lv

    ierr = 0

    lv = choice(lev_verbose, levv)
    md = choice(INIT_DEFAULT, mode)
    if (md.eq.INIT_DEFAULT) md = INIT_DEEP

    if (md.gt.INIT_DEFAULT) then
       if (md.ge.INIT_DEEP) then
          if (ierr.eq.0) call utl_init(ierr, levv=lv, mode=md)
          if (ierr.eq.0) call log_init(ierr, levv=lv, mode=md)
       endif
       if (init_counts.eq.0) then
          lev_verbose = lv
#if OPT_USE_MPI
          if (ierr.eq.0) call MPI_Init(ierr)
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
       endif
       init_counts = init_counts + 1
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
    integer lv, md

    ierr = 0

    lv = choice(lev_verbose, levv)
    md = choice(DIAG_DEFAULT, mode)
    if (md.eq.DIAG_DEFAULT) md = DIAG_DEEP

    if (md.gt.DIAG_DEFAULT) then
       if (IAND(md, DIAG_DEEP).gt.0) then
          if (ierr.eq.0) call utl_diag(ierr, u, lv, md)
          if (ierr.eq.0) call log_diag(ierr, u, lv, md)
       endif
       if (diag_counts.eq.0.or.IAND(md,DIAG_FORCE).gt.0) then
          if (VCHECK_NORMAL(lv)) call msg_mdl(TIME_STAMP, __MDL__, u)
          if (VCHECK_NORMAL(lv)) then
             call get_ni(ierr, nr, ir, icomm)
101          format('ranks = ', I0, 1x, I0, 1x, I0)
             if (present(icomm)) then
                write(tmsg, 101) ir, nr, icomm
             else
                write(tmsg, 101) ir, nr
             endif
             call msg_mdl(tmsg, __MDL__, u)
          endif
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
    integer lv

    ierr = 0
    lv = choice(lev_verbose, levv)
#if OPT_USE_MPI
    call MPI_finalize(ierr)
#else  /* not OPT_USE_MPI */
    continue
#endif /* not OPT_USE_MPI */
    if (ierr.eq.0) call utl_finalize(ierr, u, lv, mode)

    return
  end subroutine finalize
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
  if (ierr.eq.0) call finalize(ierr)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_std_mwe

#endif /* TEST_STD_MWE */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

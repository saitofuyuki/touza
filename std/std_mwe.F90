!!!_! std_mwe.F90 - touza/std MPI wrapper emulator
! Maintainer: SAITO Fuyuki
! Created: Nov 30 2020
#define TIME_STAMP 'Time-stamp: <2021/01/07 11:55:22 fuyuki std_mwe.F90>'
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
  logical,save :: ofirst = .TRUE.
  integer,save :: icomm_default = MPI_COMM_NULL
!!!_  - public
  public init, diag, finalize
  public get_ni
!!!_  - misc
  character(len=128) tmsg
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, icomm, irank, ulog)
    use TOUZA_Std_utl,only: set_if_present
    implicit none
    integer,intent(out)          :: ierr
    integer,intent(out),optional :: icomm ! default commnunicator (world)
    integer,intent(out),optional :: irank ! rank in icomm
    integer,intent(in), optional :: ulog
    integer ic, ir
    ierr = 0
    if (ofirst) then
#if OPT_USE_MPI
       call MPI_Init(ierr)
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
    if (ofirst) ofirst = .false.
    if (present(ulog)) continue ! dummy
    return
  end subroutine init
!!!_  & diag
  subroutine diag(ierr, icomm, ulog)
    use TOUZA_Std_log,only: msg
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: icomm
    integer,intent(in),optional :: ulog
    integer ir, nr

    ierr = 0
    call msg(TIME_STAMP, 'STD/MWE', 0, ulog)
    call get_ni(ierr, nr, ir, icomm)
101 format('ranks = ', I0, 1x, I0, 1x, I0)
    if (present(icomm)) then
       write(tmsg, 101) ir, nr, icomm
    else
       write(tmsg, 101) ir, nr
    endif
    call msg(tmsg, 'STD/MWE', 0, ulog)
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, ulog)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: ulog
    ierr = 0
#if OPT_USE_MPI
    call MPI_finalize(ierr)
#else  /* not OPT_USE_MPI */
    continue
#endif /* not OPT_USE_MPI */
    if (present(ulog)) continue ! dummy
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
  integer ui, uo, ue

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
!!!_! ami_miroc.F90 - TOUZA/AMi MIROC compatible interfaces
! Maintainer: SAITO Fuyuki
! Created: Apr 3 2023
#define TIME_STAMP 'Time-stamp: <2023/04/03 15:02:59 fuyuki ami_miroc.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2023
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
!!!_* Includes
#ifndef   WITH_MIROC
#  define WITH_MIROC 0
#endif
#if WITH_MIROC
#  include "miroc.h"
#  include "ztouza.h"
#endif
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_nio.h"
!!!_* Macros
#ifndef    MIROC_INTEGER
#  define  MIROC_INTEGER 4
#endif
#ifndef    MIROC_DOUBLE
#  define  MIROC_DOUBLE 8
#endif
!!!_@ TOUZA_Ami_miroc - Ami-da miroc compatible interfaces
module TOUZA_Ami_miroc
!!!_ = declaration
!!!_  - default
  implicit none
  private
  integer,parameter,public :: KMD = MIROC_DOUBLE
!!!_  - miroc include original
#if WITH_MIROC

#else  /* not WITH_MIROC */

#endif /* not WITH_MIROC */
#define __MDL__ 'm'
!!!_  - interfaces (external)
  interface
  end interface
!!!_  - private
  logical,save,private :: binit = .FALSE.
  logical,save,private :: bdiag = .FALSE.

!!!_  - public
  public init, diag, finalize
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv)
    use TOUZA_Ami_legacy,only: al_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    ierr = 0
    if (.not.binit) then
       binit = .TRUE.
       if (ierr.eq.0) then
          call al_init (ierr, u, levv, mode, stdv)
       endif
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Ami_legacy,only: al_diag=>diag
    use TOUZA_Ami_std,only: ami_msg=>msg
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: levv
    ierr = 0
    if (.not.bdiag) then
       bdiag = .TRUE.
       if (ierr.eq.0) call al_diag(ierr, u, levv, mode)
       if (ierr.eq.0) call ami_msg(TIME_STAMP, __MDL__, u)
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_AMi_legacy,only: al_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    ierr = 0
    if (ierr.eq.0) call al_finalize(ierr, u, levv, mode)
    return
  end subroutine finalize
!!!_ + user subroutines
!!!_ + end module
end module TOUZA_Ami_Miroc
!!!_* /nonmodule/ interfaces
!!!_@ test_ami_miroc - test program
#ifdef TEST_AMI_MIROC
program test_ami_miroc
  use TOUZA_Ami_miroc
  implicit none
  integer ierr

  ierr = 0
  if (ierr.eq.0) call init(ierr, levv=-1)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call finalize(ierr)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_ami_miroc

#endif /* TEST_AMI_MIROC */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

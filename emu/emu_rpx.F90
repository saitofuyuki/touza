!!!_! emu_rpx.F90 - touza/emu runtype/parallel emulation
! Maintainer: SAITO Fuyuki
! Created: Jun 18 2020
#define TIME_STAMP 'Time-stamp: <2021/01/07 20:55:00 fuyuki emu_rpx.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2020, 2021
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_emu.h"
!!!_@ TOUZA_Emu_rpx - emulator of runtype/paralel procedures
module TOUZA_Emu_rpx
!!!_ = declaration
!!!_  - default
  implicit none
  private
!!!_  - static
  logical,save :: ofirst = .TRUE.
!!!_  - public
  public init, diag, finalize
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, ulog)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: ulog
    ierr = 0
    if (ofirst) then
       ofirst = .false.
    endif
    if (present(ulog)) continue ! dummy
    return
  end subroutine init
!!!_  & diag
  subroutine diag(ierr, ulog)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: ulog
    ierr = 0
    if (present(ulog)) continue ! dummy
    return
  end subroutine diag
!!!_  & finalize
  subroutine finalize(ierr, ulog)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: ulog
    ierr = 0
    if (present(ulog)) continue ! dummy
    return
  end subroutine finalize
!!!_ + user subroutines
end module  TOUZA_Emu_rpx

!!!_@ test_emu_rpx - test program
#ifdef TEST_EMU_RPX
program test_emu_rpx
  use TOUZA_Emu_rpx
  implicit none
  integer ierr
  call init(ierr)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call finalize(ierr)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_emu_rpx
#endif /* TEST_EMU_RPX */

!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

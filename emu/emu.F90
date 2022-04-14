!!!_! emu.F90 - touza/emu interfaces
! Maintainer: SAITO Fuyuki
! Created: Jun 6 2020
#define TIME_STAMP 'Time-stamp: <2022/02/07 16:48:08 fuyuki emu.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2020,2021,2022
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
!!!_* Includes
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_emu.h"
#include "touza_std.h"
!!!_* Macros
!!!_@ TOUZA_Emu - touza/emu interfaces
module TOUZA_Emu
  use TOUZA_Emu_usi, usi_init=>init, usi_diag=>diag, usi_finalize=>finalize
  use TOUZA_Std,only: &
       & control_mode, control_deep, is_first_force, &
       & get_logu,     unit_global,  trace_fine,   trace_control
!!!_  - default
  implicit none
  public
!!!_  - private static
  integer,save,private :: init_mode = 0
  integer,save,private :: init_counts = 0
  integer,save,private :: diag_counts = 0
  integer,save,private :: fine_counts = 0
  integer,save,private :: lev_verbose = EMU_MSG_LEVEL
  integer,save,private :: err_default = ERR_NO_INIT
  integer,save,private :: ulog = unit_global
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm)
    use TOUZA_Std,only: msg_grp, choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer,intent(in),optional :: stdv
    integer,intent(in),optional :: icomm
    integer lv, md, lmd

    ierr = 0

    md = control_mode(mode, MODE_DEEPER)
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
          if (ierr.eq.0) call usi_init(ierr, ulog, lv, mode=md, stdv=stdv, icomm=icomm)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init
!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std,only: choice, is_msglev_NORMAL, msg
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
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, md)) then
          if (is_msglev_NORMAL(lv)) then
             if (ierr.eq.0) call msg(TIME_STAMP, __GRP__, utmp)
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call usi_diag(ierr, utmp, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return

    return
  end subroutine diag
!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std,only: choice, is_msglev_NORMAL, msg_grp
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
       if (is_first_force(fine_counts, md)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call usi_finalize(ierr, utmp, lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize
end module TOUZA_Emu
!!!_@ test_emu - test program
#ifdef TEST_EMU
program test_emu
  use TOUZA_Emu
  implicit none
  integer ierr

  call init(ierr)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call finalize(ierr)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_emu

#endif /* TEST_EMU */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

!!!_! trapiche.F90 - TOUZA/Trapiche manager
! Maintainer: SAITO Fuyuki
! Created: Feb 26 2021
#define TIME_STAMP 'Time-stamp: <2025/05/23 11:20:37 fuyuki trapiche.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021-2025
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_trp.h"
!!!_@ TOUZA_Trp - Trapiche interfaces
module TOUZA_Trp
!!!_ = declaration
  use TOUZA_Trp_std,   ts_init=>init, ts_diag=>diag, ts_finalize=>finalize
  use TOUZA_Trp_pack,  tp_init=>init, tp_diag=>diag, tp_finalize=>finalize
  use TOUZA_Trp_float, tf_init=>init, tf_diag=>diag, tf_finalize=>finalize
  use TOUZA_Trp_ctl,   tc_init=>init, tc_diag=>diag, tc_finalize=>finalize
!!!_  = defaults
  implicit none
  public
!!!_  - static
  integer,save,private :: init_mode = 0
  integer,save,private :: init_counts = 0
  integer,save,private :: diag_counts = 0
  integer,save,private :: fine_counts = 0
  integer,save,private :: lev_verbose = 0
  integer,save,private :: err_default = ERR_NO_INIT
  integer,save,private :: ulog = unit_global

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    integer lv, md, lmd, chmd

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
          chmd = MODE_SURFACE
          if (ierr.eq.0) call ts_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv)
          if (ierr.eq.0) call tp_init(ierr, u=ulog, levv=lv, mode=chmd)
          if (ierr.eq.0) call tf_init(ierr, u=ulog, levv=lv, mode=chmd)
          if (ierr.eq.0) call tc_init(ierr, u=ulog, levv=lv, mode=chmd)
       endif
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
       init_counts = init_counts + 1
    endif

    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode

    integer utmp, lv, md, lmd, chmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, u=utmp)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          chmd = MODE_SURFACE
          if (ierr.eq.0) call ts_diag(ierr, utmp, lv, lmd)
          if (ierr.eq.0) call tp_diag(ierr, utmp, lv, chmd)
          if (ierr.eq.0) call tf_diag(ierr, utmp, lv, chmd)
          if (ierr.eq.0) call tc_diag(ierr, utmp, lv, chmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Trp_std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd, chmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          chmd = MODE_SURFACE
          if (ierr.eq.0) call ts_finalize(ierr, utmp, lv, lmd)
          if (ierr.eq.0) call tp_finalize(ierr, utmp, lv, chmd)
          if (ierr.eq.0) call tf_finalize(ierr, utmp, lv, chmd)
          if (ierr.eq.0) call tc_finalize(ierr, utmp, lv, chmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + user subroutines
!!!_ + private subroutines
end module TOUZA_Trp

!!!_@ test_trp - test program
#ifdef TEST_TRAPICHE
program test_trp
  use TOUZA_Trp
  implicit none
  integer ierr

  call init(ierr, stdv=+9)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call finalize(ierr, levv=+9)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_trp

#endif /* TEST_TRAPICHE */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

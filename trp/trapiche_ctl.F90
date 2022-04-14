!!!_! trapiche_ctl.F90 - TOUZA/Trapiche control
! Maintainer: SAITO Fuyuki
! Created: Nov 11 2021
#define TIME_STAMP 'Time-stamp: <2021/11/24 11:48:28 fuyuki trapiche_ctl.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_trp.h"
!!!_@ TOUZA_Trp_ctl - trapiche control
module TOUZA_Trp_ctl
  use TOUZA_Trp_std,only: &
       & control_mode, control_deep, is_first_force, &
       & unit_global,  trace_fine,   trace_control
!!!_ = declaration
  implicit none
  private
!!!_  - public switches
!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = TRP_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global

  integer,save :: def_pack = 0
  integer,save :: def_unpack = 0
  integer,save :: def_code = 0
# define __MDL__ 'c'
!!!_  - common
  character(len=256) :: tmsg
!!!_  - interface
!!!_  - public
  public init, diag, finalize
!!!_ + common interfaces
contains
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, kpack, kunpack, kcode)
    use TOUZA_Trp_std,only: choice, ts_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode ! initialization mode
    integer,intent(in),optional :: stdv ! verbose level of TOUZA_Std
    integer,intent(in),optional :: kpack ! packing method default
    integer,intent(in),optional :: kunpack ! packing method default
    integer,intent(in),optional :: kcode ! float en/decoding options default
    integer lv, md, lmd

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
          if (ierr.eq.0) call ts_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv)
       endif
       if (init_counts.eq.0) then
          if (present(kpack)) then
             def_pack = kpack
          endif
          if (present(kunpack)) then
             def_unpack = kunpack
          endif
          if (present(kcode)) then
             def_code = kcode
          endif
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Trp_std,only: choice, msg, ts_diag=>diag, is_msglev_normal
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
       call trace_control &
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, md)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) then
                call msg(TIME_STAMP, __MDL__, utmp)
                call msg('(''default pack = '', I0)', def_pack, __MDL__, utmp)
                call msg('(''default unpack = '', I0)', def_unpack, __MDL__, utmp)
                call msg('(''default code = '', I0)', def_code, __MDL__, utmp)
             endif
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ts_diag(ierr, utmp, lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Trp_std,only: choice, ts_finalize=>finalize
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
       if (is_first_force(fine_counts, md)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          call ts_finalize (ierr, utmp, lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + user subroutines
!!!_ + end of TOUZA_Trp_ctl
end module TOUZA_Trp_ctl

!!!_@ test_trapiche_ctl - test program
#ifdef TEST_TRAPICHE_CTL
program test_trp_ctl
  use TOUZA_Trp_ctl
  implicit none
  integer ierr
  integer lbits

  call init(ierr)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call finalize(ierr)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop

end program test_trp_ctl

#endif /* TEST_TRAPICHE_CTL */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

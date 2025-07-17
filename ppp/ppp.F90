!!!_! ppp.F90 - touza/ppp ppp manager
! Maintainer: SAITO Fuyuki
! Created: Jan 26 2022
#define TIME_STAMP 'Time-stamp: <2025/07/17 23:42:50 fuyuki ppp.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022-2025
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_ppp.h"
!!!_@ TOUZA_Ppp - ppp interfaces
module TOUZA_Ppp
!!!_ = declaration
!!!_  - modules
  use TOUZA_Ppp_std,only: unit_global
  use TOUZA_Ppp_amng,only: pa_init=>init, pa_diag=>diag, pa_finalize=>finalize
  use TOUZA_Ppp_king,only: pk_init=>init, pk_diag=>diag, pk_finalize=>finalize
  use TOUZA_Ppp_comm,only: pc_init=>init, pc_diag=>diag, pc_finalize=>finalize

  use TOUZA_Ppp_amng,only: new_agent_root,    new_agent_color,  new_agent_family
  use TOUZA_Ppp_amng,only: new_agent_derived, new_agent_spinoff
  use TOUZA_Ppp_amng,only: mod_agent_order
  use TOUZA_Ppp_amng,only: agents_translate
  use TOUZA_Ppp_amng,only: switch_agent,  push_agent, pop_agent, top_agent
  use TOUZA_Ppp_amng,only: inquire_agent, is_member
  use TOUZA_Ppp_amng,only: query_agent,   source_agent, check_agent, base_agent, clone_agent
  use TOUZA_Ppp_amng,only: trace_agent
  use TOUZA_Ppp_amng,only: is_child_agent
  use TOUZA_Ppp_amng,only: diag_maps_batch, show_status
  use TOUZA_Ppp_amng,only: lagent

  use TOUZA_Ppp_king,only: get_king, set_king, is_king
  use TOUZA_Ppp_king,only: diag_cache

  use TOUZA_Ppp_comm,only: barrier_trace
  use TOUZA_Ppp_comm,only: broadcast
  use TOUZA_Ppp_comm,only: transfer

!!!_  - default
  implicit none
  private
!!!_  - no export
  private :: unit_global
!!!_  - private static
  integer,save,private :: init_mode = 0
  integer,save,private :: init_counts = 0
  integer,save,private :: diag_counts = 0
  integer,save,private :: fine_counts = 0
  integer,save,private :: lev_verbose = PPP_MSG_LEVEL
  integer,save,private :: err_default = ERR_NO_INIT
  integer,save,private :: ulog = unit_global
!!!_  - public procedures
  public :: init, diag, finalize
  public :: pa_init, pa_diag, pa_finalize
  public :: pk_init, pk_diag, pk_finalize
  public :: pc_init, pc_diag, pc_finalize

  public :: new_agent_root,    new_agent_color,  new_agent_family
  public :: new_agent_derived, new_agent_spinoff
  public :: mod_agent_order
  public :: agents_translate
  public :: switch_agent,  push_agent, pop_agent, top_agent
  public :: inquire_agent, is_member
  public :: query_agent,   source_agent, check_agent, base_agent, clone_agent
  public :: trace_agent
  public :: is_child_agent
  public :: diag_maps_batch, show_status
  public :: lagent

  public :: get_king, set_king, is_king
  public :: diag_cache

  public :: barrier_trace
  public :: broadcast
  public :: transfer

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm)
    use TOUZA_Ppp_std,only: ps_init=>init
    use TOUZA_Ppp_std,only: control_mode, control_deep
    use TOUZA_Ppp_std,only: choice, is_first_force
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    integer,intent(in),optional :: icomm
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
          if (ierr.eq.0) call ps_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
          if (ierr.eq.0) call pa_init(ierr, u=ulog, levv=lv, mode=chmd)
          if (ierr.eq.0) call pk_init(ierr, u=ulog, levv=lv, mode=chmd)
          if (ierr.eq.0) call pc_init(ierr, u=ulog, levv=lv, mode=chmd)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Ppp_std,only: ps_diag=>diag
    use TOUZA_Ppp_std,only: control_mode, control_deep
    use TOUZA_Ppp_std,only: choice, is_first_force, get_logu
    use TOUZA_Ppp_std,only: trace_control
    use TOUZA_Ppp_std,only: is_msglev_NORMAL
    use TOUZA_Ppp_std,only: msg
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: levv
    integer utmp, lv, md, lmd, chmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
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
          if (ierr.eq.0) call ps_diag(ierr, ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call pa_diag(ierr, ulog, levv=lv, mode=chmd)
          if (ierr.eq.0) call pk_diag(ierr, ulog, levv=lv, mode=chmd)
          if (ierr.eq.0) call pc_diag(ierr, ulog, levv=lv, mode=chmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Ppp_std,only: ps_finalize=>finalize
    use TOUZA_Ppp_std,only: control_mode, control_deep
    use TOUZA_Ppp_std,only: choice, is_first_force, get_logu
    use TOUZA_Ppp_std,only: trace_fine
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd, chmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
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
          if (ierr.eq.0) call ps_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call pa_finalize(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call pk_finalize(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call pc_finalize(ierr, utmp, levv=lv, mode=chmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

end module TOUZA_Ppp

!!!_@ test_ppp - test program
#ifdef TEST_PPP
program test_ppp
  use TOUZA_Ppp
  implicit none
  integer ierr

  call init(ierr, levv=+99, stdv=+99)
  if (ierr.eq.0) then
     call diag(ierr)
  endif
  if (ierr.eq.0) then
     call finalize(ierr)
  endif
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_ppp

#endif /* TEST_PPP */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

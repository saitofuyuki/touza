!!!_! ppp_std.F90 - TOUZA/Ppp utilities (and bridge to Std)
! Maintainer: SAITO Fuyuki
! Created: Jan 26 2022
#define TIME_STAMP 'Time-stamp: <2025/08/13 11:31:33 fuyuki ppp_std.F90>'
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
!!!_@ TOUZA_Ppp_std - Ppp utilities
module TOUZA_Ppp_std
!!!_ = declaration
!!!_  - modules
  use TOUZA_Std,only: bld_init, bld_diag, bld_finalize
  use TOUZA_Std,only: mwe_init, mwe_diag, mwe_finalize
  use TOUZA_Std,only: choice,       choice_a
  use TOUZA_Std,only: ndigits
  use TOUZA_Std,only: control_deep, control_mode, is_first_force
  use TOUZA_Std,only: is_msglev
  use TOUZA_Std,only: is_msglev_debug,  is_msglev_info,   is_msglev_normal, is_msglev_detail
  use TOUZA_Std,only: is_msglev_severe, is_msglev_fatal
  use TOUZA_Std,only: set_defu,         is_unit_star
  use TOUZA_Std,only: get_logu,         unit_global,      trace_fine,       trace_control
  use TOUZA_Std,only: trace_err
  use TOUZA_Std,only: banner,           toggle_flush
  use TOUZA_Std,only: safe_mpi_init, safe_mpi_finalize
  use TOUZA_Std,only: get_wni
  use TOUZA_Std,only: get_comm, get_ni, get_gni, get_wni_safe, is_mpi_activated
  use TOUZA_Std,only: comp_comms, comp_groups, cc_unequal, cc_both_null
  use TOUZA_Std,only: MPI_COMM_NULL, MPI_GROUP_NULL, MPI_COMM_WORLD, MPI_UNDEFINED
  use TOUZA_Std,only: MPI_STATUS_SIZE, MPI_GROUP_EMPTY, MPI_INTEGER, MPI_CHARACTER
  use TOUZA_Std,only: MPI_MIN, MPI_MAX
  use TOUZA_Std,only: MPI_ANY_TAG,     MPI_ANY_SOURCE
  use TOUZA_Std,only: MPI_GROUP_TRANSLATE_RANKS, MPI_GROUP_SIZE, MPI_GROUP_RANK, MPI_GROUP_UNION
  use TOUZA_Std,only: MPI_COMM_SIZE, MPI_COMM_RANK, MPI_COMM_CREATE, MPI_COMM_SPLIT, MPI_COMM_GROUP
  use TOUZA_Std,only: MPI_WAIT, MPI_BARRIER, MPI_ABORT
  use TOUZA_Std,only: is_eof_ss
  use TOUZA_Std,only: lpath
  use TOUZA_Std,only: ipc_getcwd,  ipc_chdir
  use TOUZA_Std,only: new_htable,  new_entry, settle_entry
  use TOUZA_Std,only: diag_htable, reg_entry, query_status
!!!_  - default
  implicit none
  private
!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = PPP_MSG_LEVEL
  integer,save :: lev_stdv    = PPP_MSG_LEVEL - 1
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
!!!_  - common
  character(len=256) :: tmsg
# define __MDL__ 's'
!!!_  - interfaces
  interface msg
     module procedure msg_txt
     module procedure msg_i, msg_ia, msg_aa
  end interface msg
!!!_  - public procedures
  public :: init, diag, finalize
  public :: msg,  msg_mon, gen_tag
!!!_   . TOUZA_Std
  public :: bld_init, bld_diag, bld_finalize
  public :: mwe_init, mwe_diag, mwe_finalize
  public :: choice,       choice_a
  public :: ndigits
  public :: control_mode, control_deep, is_first_force
  public :: is_msglev
  public :: is_msglev_debug,  is_msglev_info,   is_msglev_normal, is_msglev_detail
  public :: is_msglev_severe, is_msglev_fatal
  public :: set_defu,         is_unit_star
  public :: get_logu,         unit_global,      trace_fine,       trace_control
  public :: trace_err
  public :: banner,           toggle_flush
  public :: safe_mpi_init, safe_mpi_finalize
  public :: get_wni
  public :: get_comm, get_ni, get_gni, get_wni_safe, is_mpi_activated
  public :: comp_comms, comp_groups, cc_unequal, cc_both_null
  public :: is_eof_ss
  public :: lpath
  public :: ipc_getcwd,  ipc_chdir
  public :: new_htable,  new_entry, settle_entry
  public :: diag_htable, reg_entry, query_status
  public :: MPI_COMM_NULL, MPI_GROUP_NULL, MPI_COMM_WORLD, MPI_UNDEFINED
  public :: MPI_STATUS_SIZE, MPI_GROUP_EMPTY
  public :: MPI_INTEGER, MPI_CHARACTER
  public :: MPI_MIN, MPI_MAX
  public :: MPI_ANY_TAG, MPI_ANY_SOURCE
  public :: MPI_GROUP_TRANSLATE_RANKS, MPI_GROUP_SIZE, MPI_GROUP_RANK, MPI_GROUP_UNION
  public :: MPI_COMM_SIZE, MPI_COMM_RANK, MPI_COMM_CREATE, MPI_COMM_SPLIT, MPI_COMM_GROUP
  public :: MPI_WAIT, MPI_BARRIER, MPI_ABORT

!!!_ + common interfaces
contains
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm)
    ! use TOUZA_Std,only: mwe_init
    use TOUZA_Std,only: env_init
    use TOUZA_Std,only: htb_init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    integer,intent(in),optional :: icomm
    integer lv, md, lmd, tsmd

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
       if (md.ge.MODE_DEEP) then
          lev_stdv = choice(lev_stdv, stdv)
          tsmd = MODE_SURFACE
          if (ierr.eq.0) call env_init(ierr, u=ulog, levv=lev_stdv, mode=lmd, icomm=icomm)
          ! if (ierr.eq.0) call mwe_init(ierr, u=ulog, levv=lev_stdv, mode=lmd, icomm=icomm)
          if (ierr.eq.0) call htb_init(ierr, u=ulog, levv=lev_stdv, mode=tsmd)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std,only: mwe_diag
    use TOUZA_Std,only: env_diag
    use TOUZA_Std,only: htb_diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer utmp, lv, md, lmd, tsmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __MDL__, utmp)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_DEEP) then
          tsmd = MODE_SURFACE
          if (ierr.eq.0) call env_diag(ierr, utmp, levv=lev_stdv, mode=lmd)
          ! if (ierr.eq.0) call mwe_diag(ierr, utmp, levv=lev_stdv, mode=lmd)
          if (ierr.eq.0) call htb_diag(ierr, utmp, levv=lev_stdv, mode=tsmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    ! use TOUZA_Std,only: mwe_finalize
    use TOUZA_Std,only: env_finalize
    use TOUZA_Std,only: htb_finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd, tsmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_DEEP) then
          tsmd = MODE_SURFACE
          if (ierr.eq.0) call env_finalize(ierr, utmp, lev_stdv, mode=lmd)
          ! if (ierr.eq.0) call mwe_finalize(ierr, utmp, lev_stdv, mode=lmd)
          if (ierr.eq.0) call htb_finalize(ierr, utmp, lev_stdv, mode=tsmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + user interfaces
!!!_  & gen_tag
  subroutine gen_tag &
       & (tag, mdl)
    use TOUZA_Std,only: std_gen_tag=>gen_tag
    implicit none
    character(len=*),intent(out)         :: tag
    character(len=*),intent(in),optional :: mdl
    call std_gen_tag(tag, pkg=PACKAGE_TAG, grp=__GRP__, mdl=mdl)
    return
  end subroutine gen_tag

!!!_  & msg_mon - msg for monitoring (light-weight)
  subroutine msg_mon &
       & (txt, tag, u)
    use TOUZA_Std,only: std_msg=>msg
    implicit none
    character(len=*),intent(in) :: txt
    character(len=*),intent(in) :: tag
    integer,         intent(in),optional :: u
    call std_msg(txt, tag, u, to_flush=.FALSE.)
    return
  end subroutine msg_mon

!!!_  & msg_txt - message dispatcher (to override std)
  subroutine msg_txt &
       & (txt, mdl, u)
    use TOUZA_Std,only: choice, std_msg=>msg
    implicit none
    character(len=*),intent(in)          :: txt
    character(len=*),intent(in),optional :: mdl
    integer,         intent(in),optional :: u
    character(len=1024) :: tag
    call gen_tag(tag, mdl=mdl)
    call std_msg(txt, tag, u)
    return
  end subroutine msg_txt
!!!_  & msg_i - message dispatcher (to override std)
  subroutine msg_i &
       & (fmt, v, mdl, u)
    use TOUZA_Std,only: choice, std_msg=>msg, gen_tag
    implicit none
    character(len=*),intent(in)          :: fmt
    integer,         intent(in)          :: v
    character(len=*),intent(in),optional :: mdl
    integer,         intent(in),optional :: u
    write(tmsg, fmt) v
    call msg_txt(tmsg, mdl, u)
    return
  end subroutine msg_i
!!!_  & msg_ia - message dispatcher (to override std)
  subroutine msg_ia &
       & (fmt, vv, mdl, u)
    implicit none
    character(len=*),intent(in)          :: fmt
    integer,         intent(in)          :: vv(:)
    character(len=*),intent(in),optional :: mdl
    integer,         intent(in),optional :: u
    write(tmsg, fmt) vv(:)
    call msg_txt(tmsg, mdl, u)
  end subroutine msg_ia
!!!_  & msg_aa - message dispatcher (to override std)
  subroutine msg_aa &
       & (fmt, vv, mdl, u)
    implicit none
    character(len=*),intent(in)          :: fmt
    character(len=*),intent(in)          :: vv(:)
    character(len=*),intent(in),optional :: mdl
    integer,         intent(in),optional :: u
    write(tmsg, fmt) vv(:)
    call msg_txt(tmsg, mdl, u)
  end subroutine msg_aa

end module TOUZA_Ppp_std

!!!_@ test_ppp_std - test program
#ifdef TEST_PPP_STD
program test_ppp_std
  use TOUZA_Ppp_std
  implicit none
  integer ierr

101 format(A, ' = ', I0)
  call init(ierr, stdv=+999)
  write(*, 101) 'INIT', ierr

  if (ierr.eq.0) call diag(ierr)
  write(*, 101) 'DIAG', ierr

  if (ierr.eq.0) call finalize(ierr, levv=+9)
  write(*, 101) 'FINAL', ierr
  stop
end program test_ppp_std

#endif /* TEST_PPP_STD */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

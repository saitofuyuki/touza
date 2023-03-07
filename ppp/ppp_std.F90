!!!_! ppp_std.F90 - TOUZA/Ppp utilities (and bridge to Std)
! Maintainer: SAITO Fuyuki
! Created: Jan 26 2022
#define TIME_STAMP 'Time-stamp: <2022/10/20 07:01:36 fuyuki ppp_std.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022
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
  use TOUZA_Std_utl,only: &
       & choice,       choice_a,     &
       & control_deep, control_mode, is_first_force
  use TOUZA_Std_log,only: &
       & is_msglev, &
       & is_msglev_debug,  is_msglev_info,   is_msglev_normal, is_msglev_detail, &
       & is_msglev_severe, is_msglev_fatal,  &
       & get_logu,         unit_global,      trace_fine,       trace_control
  use TOUZA_Std_mwe,only: &
       & get_comm, get_ni, get_gni, get_wni_safe, is_mpi_activated, &
       & MPI_COMM_NULL, MPI_GROUP_NULL, MPI_COMM_WORLD, MPI_UNDEFINED, &
       & MPI_STATUS_SIZE, MPI_GROUP_EMPTY, MPI_INTEGER, MPI_CHARACTER, &
       & MPI_ANY_TAG,     MPI_ANY_SOURCE

  use TOUZA_Std_env,only: is_eof_ss
  use TOUZA_Std_htb,only: &
       & new_htable,  new_entry, settle_entry, &
       & diag_htable, reg_entry, query_status
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
  public init, diag, finalize
  public msg
!!!_   . TOUZA_Std
  public choice,       choice_a
  public control_mode, control_deep, is_first_force
  public is_msglev
  public is_msglev_debug,  is_msglev_info,   is_msglev_normal, is_msglev_detail
  public is_msglev_severe, is_msglev_fatal
  public get_logu,         unit_global,      trace_fine,       trace_control
  public get_comm, get_ni, get_gni, get_wni_safe, is_mpi_activated
  public is_eof_ss
  public new_htable,  new_entry, settle_entry
  public diag_htable, reg_entry, query_status
  public MPI_COMM_NULL, MPI_GROUP_NULL, MPI_COMM_WORLD, MPI_UNDEFINED
  public MPI_STATUS_SIZE, MPI_GROUP_EMPTY
  public MPI_INTEGER, MPI_CHARACTER
  public MPI_ANY_TAG, MPI_ANY_SOURCE
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm)
    use TOUZA_Std_mwe,only: mwe_init=>init
    use TOUZA_Std_env,only: env_init=>init
    use TOUZA_Std_htb,only: htb_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    integer,intent(in),optional :: icomm
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
       if (md.ge.MODE_DEEP) then
          lev_stdv = choice(lev_stdv, stdv)
          if (ierr.eq.0) call mwe_init(ierr, u=ulog, levv=lev_stdv, mode=lmd, icomm=icomm)
          if (ierr.eq.0) call env_init(ierr, u=ulog, levv=lev_stdv, mode=lmd, icomm=icomm)
          if (ierr.eq.0) call htb_init(ierr, u=ulog, levv=lev_stdv, mode=lmd)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std_mwe,only: mwe_diag=>diag
    use TOUZA_Std_env,only: env_diag=>diag
    use TOUZA_Std_htb,only: htb_diag=>diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, md)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __MDL__, utmp)
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call mwe_diag(ierr, utmp, levv=lev_stdv, mode=lmd)
          if (ierr.eq.0) call env_diag(ierr, utmp, levv=lev_stdv, mode=lmd)
          if (ierr.eq.0) call htb_diag(ierr, utmp, levv=lev_stdv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_mwe,only: mwe_finalize=>finalize
    use TOUZA_Std_env,only: env_finalize=>finalize
    use TOUZA_Std_htb,only: htb_finalize=>finalize
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
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call env_finalize(ierr, utmp, lev_stdv, mode=lmd)
          if (ierr.eq.0) call mwe_finalize(ierr, utmp, lev_stdv, mode=lmd)
          if (ierr.eq.0) call htb_finalize(ierr, utmp, lev_stdv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + user interfaces
!!!_  & msg_txt - message dispatcher (to override std)
  subroutine msg_txt &
       & (txt, mdl, u)
    use TOUZA_Std,only: choice, std_msg=>msg, gen_tag
    implicit none
    character(len=*),intent(in)          :: txt
    character(len=*),intent(in),optional :: mdl
    integer,         intent(in),optional :: u
    character(len=1024) :: tag
    call gen_tag(tag, pkg=PACKAGE_TAG, grp=__GRP__, mdl=mdl)
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

  if (ierr.eq.0) call finalize(ierr)
  write(*, 101) 'FINAL', ierr
  stop
end program test_ppp_std

#endif /* TEST_PPP_STD */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

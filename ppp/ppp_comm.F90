!!!_! ppp_comm.F90 - TOUZA/ppp communication
! Maintainer: SAITO Fuyuki
! Created: Mar 2 2022
#define TIME_STAMP 'Time-stamp: <2022/04/18 08:22:03 fuyuki ppp_comm.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022
!           Japan Agency for Marine-Earth Science and Technology
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_ppp.h"
!!!_* macros
#ifndef   TEST_PPP_COMM
#  define TEST_PPP_COMM 0
#endif
!!!_@ TOUZA_Ppp_comm - MPI collective communication manager
module TOUZA_Ppp_comm
!!!_ + modules
  use TOUZA_Ppp_std,only: &
       & control_mode, control_deep, is_first_force, &
       & get_logu,     unit_global,  trace_fine,   trace_control
!!!_ + default
  implicit none
  private
!!!_ + public
!!!_ + static
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = PPP_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
#define __MDL__ 'c'
!!!_ + overloads
  interface broadcast
     module procedure broadcast_ia, broadcast_i
  end interface broadcast
!!!_ + interfaces
  public init, diag, finalize
  public barrier_trace
  public broadcast
!!!_ + common interfaces
contains
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm)
    use TOUZA_Ppp_std, only: ps_init=>init, choice
    use TOUZA_Ppp_amng,only: pa_init=>init
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
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ps_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
          if (ierr.eq.0) call pa_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
       endif
       if (is_first_force(init_counts, md)) then
          continue
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Ppp_std, only: choice, msg, ps_diag=>diag, is_msglev_normal
    use TOUZA_Ppp_amng,only: pa_diag=>diag
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
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, md)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __MDL__, utmp)
          endif
          if (ierr.eq.0) then
             continue
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ps_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call pa_diag(ierr, utmp, levv=lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Ppp_std, only: ps_finalize=>finalize, choice
    use TOUZA_Ppp_amng,only: pa_finalize=>finalize
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
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ps_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call pa_finalize(ierr, utmp, levv=lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
  end subroutine finalize

!!!_ + init subcontracts
!!!_ + diag subcontracts
!!!_ + barrier_trace
  subroutine barrier_trace &
       & (ierr, iagent, tag, u)
    use mpi,only: MPI_Barrier
    use TOUZA_Ppp_amng,only: inquire_agent
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in),optional :: iagent
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    integer icomm

    ierr = 0
    if (ierr.eq.0) call inquire_agent(ierr, icomm=icomm, iagent=iagent)
#if HAVE_FTRACE_REGION_BEGIN
    if (present(tag)) call ftrace_region_begin(tag)
#endif
    if (ierr.eq.0) call MPI_Barrier(ierr, icomm)
#if HAVE_FTRACE_REGION_END
    if (present(tag)) call ftrace_region_end(tag)
#endif
    return
  end subroutine barrier_trace

!!!_ + broadcast
  subroutine broadcast_i &
       & (ierr, buf, iagent, iroot)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(inout)       :: buf
    integer,intent(in),optional :: iagent
    integer,intent(in),optional :: iroot
    integer :: b(1)
    b(:) = buf
    call broadcast_ia(ierr, b(:), iagent, iroot)
    buf = b(1)
    return
  end subroutine broadcast_i
  subroutine broadcast_ia &
       & (ierr, buf, iagent, iroot)
    use TOUZA_Ppp_std,only: choice
    use TOUZA_Ppp_amng,only: inquire_agent
    use mpi,only: MPI_INTEGER
#  if HAVE_FORTRAN_MPI_MPI_BCAST
    use mpi,only: MPI_Bcast
#  endif
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(inout)       :: buf(:)
    integer,intent(in),optional :: iagent
    integer,intent(in),optional :: iroot

    integer n
    integer icomm, ik

    ierr = 0

    n = size(buf)
    ik = choice(0, iroot)
    if (ierr.eq.0) call inquire_agent(ierr, icomm=icomm, iagent=iagent)
    if (ierr.eq.0) call MPI_Bcast(buf, n, MPI_INTEGER, ik, icomm, ierr)

    return
  end subroutine broadcast_ia
!!!_ + gather
!!!_ + scatter
!!!_ + end TOUZA_Ppp_comm
end module TOUZA_Ppp_comm
!!!_@ test_ppp_comm - test program
#if TEST_PPP_COMM
program test_ppp_comm
  use MPI
  use TOUZA_Ppp_comm
  implicit none

  integer ierr
  integer jarg

  ierr = 0
  jarg = 0

101 format(A, ' = ', I0)
  call init(ierr, levv=+9)
  write(*, 101) 'INIT', ierr

  call diag(ierr)
  write(*, 101) 'DIAG', ierr
  call finalize(ierr)
  write(*, 101) 'FINAL', ierr
  stop

contains
end program test_ppp_comm
#endif /* TEST_PPP_COMM */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

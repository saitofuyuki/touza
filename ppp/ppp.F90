!!!_! ppp.F90 - touza/ppp ppp manager
! Maintainer: SAITO Fuyuki
! Created: Jan 26 2022
#define TIME_STAMP 'Time-stamp: <2022/04/17 23:01:43 fuyuki ppp.F90>'
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
!!!_@ TOUZA_Ppp - ppp interfaces
module TOUZA_Ppp
!!!_ = declaration
!!!_  - modules
  use TOUZA_Ppp_std,  ps_init=>init, ps_diag=>diag, ps_finalize=>finalize
  use TOUZA_Ppp_amng, pa_init=>init, pa_diag=>diag, pa_finalize=>finalize
  use TOUZA_Ppp_king, pk_init=>init, pk_diag=>diag, pk_finalize=>finalize
  use TOUZA_Ppp_comm, pc_init=>init, pc_diag=>diag, pc_finalize=>finalize
!!!_  - default
  implicit none
  public
!!!_  - private static
  integer,save,private :: init_mode = 0
  integer,save,private :: init_counts = 0
  integer,save,private :: diag_counts = 0
  integer,save,private :: fine_counts = 0
  integer,save,private :: lev_verbose = PPP_MSG_LEVEL
  integer,save,private :: err_default = ERR_NO_INIT
  integer,save,private :: ulog = unit_global
!!!_  - private procedures
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm)
    use TOUZA_Ppp_std,only: choice
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
          if (ierr.eq.0) call pa_init(ierr, u=ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call pk_init(ierr, u=ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call pc_init(ierr, u=ulog, levv=lv, mode=lmd)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Ppp_std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: levv
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, md)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, u=utmp)
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ps_diag(ierr, ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call pa_diag(ierr, ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call pk_diag(ierr, ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call pc_diag(ierr, ulog, levv=lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Ppp_std,only: choice
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
          if (ierr.eq.0) call ps_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call pa_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call pk_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call pc_finalize(ierr, utmp, levv=lv, mode=lmd)
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

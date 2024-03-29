!!!_! touza.F90 - touza administration
! Maintainer: SAITO Fuyuki
! Created: Jun 6 2020
#define TIME_STAMP 'Time-stamp: <2023/03/25 14:00:56 fuyuki touza.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2020, 2021, 2022, 2023
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!!!_* include
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza.h"
!!!_* Health check
#if ENABLE_TOUZA_STD
#else
#  error 'TOUZA/std package must be enabled.'
#endif
!!!_* Macros
!!!_@ TOUZA - touza interfaces
module TOUZA
!!!_ + modules
!!!_  - std(mandatory)
  use TOUZA_Std, std_init=>init, std_diag=>diag, std_finalize=>finalize
!!!_  - cal(conditional)
#if ENABLE_TOUZA_CAL
  use TOUZA_Cal, cal_init=>init, cal_diag=>diag, cal_finalize=>finalize
#endif /* ENABLE_TOUZA_CAL */
!!!_  - trp(conditional)
#if ENABLE_TOUZA_TRP
  use TOUZA_Trp, trp_init=>init, trp_diag=>diag, trp_finalize=>finalize, trp_msg=>msg
#endif /* ENABLE_TOUZA_TRP */
!!!_  - nio(conditional)
#if ENABLE_TOUZA_NIO
  use TOUZA_Nio, nio_init=>init, nio_diag=>diag, nio_finalize=>finalize, nio_msg=>msg
#endif /* ENABLE_TOUZA_NIO */
!!!_  - ppp(conditional)
#if ENABLE_TOUZA_PPP
  use TOUZA_Ppp, ppp_init=>init, ppp_diag=>diag, ppp_finalize=>finalize, ppp_msg=>msg
#endif /* ENABLE_TOUZA_PPP */
!!!_  - div(conditional)
#if ENABLE_TOUZA_DIV
  use TOUZA_Div, div_init=>init, div_diag=>diag, div_finalize=>finalize
#endif /* ENABLE_TOUZA_DIV */
!!!_ + default
  implicit none
  public
!!!_ + static
  integer,save,private :: init_mode = 0
  integer,save,private :: init_counts = 0
  integer,save,private :: diag_counts = 0
  integer,save,private :: fine_counts = 0
  integer,save,private :: lev_verbose = msglev_normal
  integer,save,private :: err_default = ERR_NO_INIT
  integer,save,private :: ulog = unit_global
!!!_ + public
  public :: init, diag, finalize
contains
!!!_ + common interfaces
!!!_  & init - touza system initialization batch
  subroutine init &
       & (ierr, u, levv, mode, icomm)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: icomm ! std argument
    integer lv, md, lmd

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
          if (ierr.eq.0) call std_init(ierr, u=ulog, levv=lv, mode=lmd, icomm=icomm)
#if ENABLE_TOUZA_PPP
          if (ierr.eq.0) call ppp_init(ierr, u=ulog, levv=lv, mode=lmd, icomm=icomm)
#endif /* ENABLE_TOUZA_PPP */
#if ENABLE_TOUZA_DIV
          if (ierr.eq.0) call div_init(ierr, u=ulog, levv=lv, mode=lmd)
#endif /* ENABLE_TOUZA_DIV */
#if ENABLE_TOUZA_CAL
          if (ierr.eq.0) call cal_init(ierr, u=ulog, levv=lv, mode=lmd)
#endif /* ENABLE_TOUZA_CAL */
#if ENABLE_TOUZA_TRP
          if (ierr.eq.0) call trp_init(ierr, u=ulog, levv=lv, mode=lmd)
#endif /* ENABLE_TOUZA_TRP */
#if ENABLE_TOUZA_NIO
          if (ierr.eq.0) call nio_init(ierr, u=ulog, levv=lv, mode=lmd)
#endif /* ENABLE_TOUZA_NIO */
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif

    return
  end subroutine init
!!!_  & diag - touza system diagnosis batch
  subroutine diag(ierr, u, levv, mode)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(uout, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control(ierr, md, pkg=__PKG__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __PKG__, u=utmp)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call std_diag(ierr, utmp, levv, mode=lmd)
#if ENABLE_TOUZA_PPP
          if (ierr.eq.0) call ppp_diag(ierr, utmp, levv, mode=lmd)
#else  /* not ENABLE_TOUZA_PPP */
          if (ierr.eq.0) call msg_grp('ppp disabled', u=utmp)
#endif /* not ENABLE_TOUZA_PPP */
#if ENABLE_TOUZA_DIV
          if (ierr.eq.0) call div_diag(ierr, utmp, levv, mode=lmd)
#else  /* not ENABLE_TOUZA_DIV */
          if (ierr.eq.0) call msg_grp('div disabled', u=utmp)
#endif /* not ENABLE_TOUZA_DIV */
#if ENABLE_TOUZA_CAL
          if (ierr.eq.0) call cal_diag(ierr, utmp, levv, mode=lmd)
#else  /* not ENABLE_TOUZA_CAL */
          if (ierr.eq.0) call msg_grp('cal disabled', u=utmp)
#endif /* not ENABLE_TOUZA_CAL */
#if ENABLE_TOUZA_TRP
          if (ierr.eq.0) call trp_diag(ierr, utmp, levv, mode=lmd)
#else  /* not ENABLE_TOUZA_TRP */
          if (ierr.eq.0) call msg_grp('trp disabled', u=utmp)
#endif /* not ENABLE_TOUZA_TRP */
#if ENABLE_TOUZA_NIO
          if (ierr.eq.0) call nio_diag(ierr, utmp, levv, mode=lmd)
#else  /* not ENABLE_TOUZA_NIO */
          if (ierr.eq.0) call msg_grp('nio disabled', u=utmp)
#endif /* not ENABLE_TOUZA_NIO */
       endif
       diag_counts = diag_counts + 1
    endif

    return
  end subroutine diag
!!!_  & finalize - touza system finalization batch
  subroutine finalize(ierr, u, levv, mode)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, md, lmd, lv

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(uout, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_DEEP) then
#if ENABLE_TOUZA_NIO
          if (ierr.eq.0) call nio_finalize(ierr, utmp, levv, mode=lmd)
#endif /* ENABLE_TOUZA_NIO */
#if ENABLE_TOUZA_TRP
          if (ierr.eq.0) call trp_finalize(ierr, utmp, levv, mode=lmd)
#endif /* ENABLE_TOUZA_TRP */
#if ENABLE_TOUZA_CAL
          if (ierr.eq.0) call cal_finalize(ierr, utmp, levv, mode=lmd)
#endif /* ENABLE_TOUZA_CAL */
#if ENABLE_TOUZA_DIV
          if (ierr.eq.0) call div_finalize(ierr, utmp, levv, mode=lmd)
#endif /* ENABLE_TOUZA_DIV */
#if ENABLE_TOUZA_PPP
          if (ierr.eq.0) call ppp_finalize(ierr, utmp, levv, mode=lmd)
#endif /* ENABLE_TOUZA_PPP */
          if (ierr.eq.0) call std_finalize(ierr, utmp, levv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize
end module TOUZA
!!!_@ test_touza - test program
#ifdef TEST_TOUZA
program test_touza
  use TOUZA
  implicit none
  integer ierr

  ! call init(ierr, cal=INIT_DEEP)
101 format(A, ' = ', I0)
  call init(ierr)
  write(*, 101) 'init', ierr
  if (ierr.eq.0) call diag(ierr, mode=MODE_LOOSE+MODE_DEEPER, levv=+99)
  write(*, 101) 'diag', ierr
  if (ierr.eq.0) call finalize(ierr, levv=+99)
  write(*, 101) 'finalize', ierr
  stop
end program test_touza

#endif /* TEST_TOUZA */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

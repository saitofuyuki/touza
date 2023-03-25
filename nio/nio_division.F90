!!!_! nio_division.F90 - TOUZA/Nio task and domain division
! Maintainer: SAITO Fuyuki
! Created: Nov 9 2021
#define TIME_STAMP 'Time-stamp: <2023/03/25 09:46:07 fuyuki nio_division.F90>'
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
#include "touza_nio.h"
!!!_@ TOUZA_Nio_division - Nio division controls
module TOUZA_Nio_division
!!!_ = declaration
!!!_  - modules
  use TOUZA_Nio_std,only: get_logu, unit_global, trace_fine, trace_control
!!!_  - default
  implicit none
  private
!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = NIO_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global

# define __MDL__ 'd'
!!!_  - interfaces
!!!_  - public procedures
  public init, diag, finalize
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm)
    use TOUZA_Nio_std,   only: control_mode,  control_deep, is_first_force
    use TOUZA_Nio_record,only: nr_init=>init
    use TOUZA_Nio_header,only: nh_init=>init
    use TOUZA_Nio_std,only: ns_init=>init, choice
    use TOUZA_Div,only: div_init=>init
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
       if (is_first_force(init_counts, mode)) then
          ulog = choice(ulog, u)
          lev_verbose = lv
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ns_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
          if (ierr.eq.0) call nh_init(ierr, u=ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_init(ierr, u=ulog, levv=lv, mode=lmd)
       endif
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call div_init(ierr, u=ulog, levv=lv, mode=lmd, icomm=icomm)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Nio_std,   only: control_mode,  control_deep, is_first_force
    use TOUZA_Nio_std,   only: ns_diag=>diag, msg, choice, is_msglev_normal
    use TOUZA_Nio_record,only: nr_diag=>diag
    use TOUZA_Nio_header,only: nh_diag=>diag
    use TOUZA_Div,       only: div_diag=>diag
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
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __MDL__, utmp)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ns_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nh_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_diag(ierr, utmp, levv=lv, mode=lmd)
       endif
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call div_diag(ierr, utmp, levv=lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Nio_std,   only: control_mode,  control_deep, is_first_force
    use TOUZA_Nio_std,   only: ns_diag=>diag, msg, choice, is_msglev_normal
    use TOUZA_Nio_std,   only: ns_finalize=>finalize, choice
    use TOUZA_Nio_record,only: nr_finalize=>finalize
    use TOUZA_Nio_header,only: nh_finalize=>finalize
    use TOUZA_Div,       only: div_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, MODE_DEEPEST)
    init_mode = md

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call div_finalize(ierr, utmp, lv, mode=lmd)
       endif
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ns_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call nh_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call nr_finalize(ierr, utmp, lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + user interfaces
!!!_ + end module
end module TOUZA_Nio_division

!!!_@ test_nio_division - test program
#ifdef TEST_NIO_DIVISION
program test_nio_division
  use TOUZA_Nio_division
  implicit none
  integer ierr

101 format(A, ' = ', I0)
  call init(ierr)
  write(*, 101) 'INIT', ierr

  if (ierr.eq.0) call diag(ierr)
  write(*, 101) 'DIAG', ierr

  if (ierr.eq.0) call finalize(ierr)
  write(*, 101) 'FINAL', ierr
  stop
end program test_nio_division

#endif /* TEST_NIO_DIVISION */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

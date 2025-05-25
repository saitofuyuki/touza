!!!_! nio_axis.F90 - TOUZA/Nio axis record special
! Maintainer: SAITO Fuyuki
! Created: Apr 4 2024
#define TIME_STAMP 'Time-stamp: <2025/05/23 11:41:24 fuyuki nio_axis.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2024, 2025
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_nio.h"
!!!_@ TOUZA_Nio_axis - nio axis interfaces
module TOUZA_Nio_axis
!!!_ = declaration
  use TOUZA_Nio_std,only: KI32, KI64, KDBL, KFLT
  use TOUZA_Nio_std,only: get_logu,      unit_global,  trace_fine,    trace_control
  use TOUZA_Nio_header,only: litem, nitem
  implicit none
  private
!!!_  - public parameters
  integer,parameter,public :: axis_loc    = 0
  integer,parameter,public :: axis_index  = 1  !!  Not well defined.  may I is for integer
  integer,parameter,public :: axis_cyclic = 2
  integer,parameter,public :: axis_wgt    = 4
!!!_  - private parameters
  integer,parameter :: nloc = 5, nspec = 8
  ! order corresponds to axis_* parameters above
  character(len=litem),save :: DSET_specials(0:nspec-1) = &
       & (/ 'AXLOC  ', 'IAXLOC ', 'CAXLOC ', 'CIAXLOC', &
       &    'AXWGT  ', 'IAXWGT ', 'CAXWGT ', 'CIAXWGT'  /)
  ! 'CCAXLOC'
  ! CCAXLOC is used in DIUR02 of official gtool, might be a bug.
  ! DSET_specials(1:nloc) corresponds to LOC, otherwise WGT.
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = NIO_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
#define __MDL__ 'a'
#define _ERROR(E) (E - ERR_MASK_NIO_AXIS)
!!!_  - interfaces
!!!_  - public procedures
  public init, diag, finalize
  public axis_set_header
  public is_axis_set, axis_parse
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init &
       & (ierr, &
       &  u,    levv, mode, stdv, icomm)
    use TOUZA_Nio_std,   only: control_mode,  control_deep, is_first_force
    use TOUZA_Nio_std,   only: ns_init=>init, choice
    ! use TOUZA_Nio_header,only: nh_init=>init
    use TOUZA_Nio_record,only: nr_init=>init
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
          if (ierr.eq.0) call ns_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
          ! if (ierr.eq.0) call nh_init(ierr, u=ulog, levv=lv, mode=chmd, stdv=stdv)
          if (ierr.eq.0) call nr_init(ierr, u=ulog, levv=lv, mode=chmd, stdv=stdv)
       endif
       if (init_counts.eq.0) then
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = _ERROR(ERR_FAILURE_INIT)
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Nio_std,only: control_mode,  control_deep, is_first_force
    use TOUZA_Nio_std,only: ns_diag=>diag, choice, msg, is_msglev_normal, is_msglev_info
    ! use TOUZA_Nio_header,only: nh_diag=>diag
    use TOUZA_Nio_record,only: nr_diag=>diag
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
       call trace_control &
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __MDL__, utmp)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          chmd = MODE_SURFACE
          if (ierr.eq.0) call ns_diag(ierr, utmp, levv=lv, mode=lmd)
          ! if (ierr.eq.0) call nh_diag(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call nr_diag(ierr, utmp, levv=lv, mode=chmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Nio_std,only: control_mode,  control_deep, is_first_force
    use TOUZA_Nio_std,only: ns_finalize=>finalize, choice
    ! use TOUZA_Nio_header,only: nh_finalize=>finalize
    use TOUZA_Nio_record,only: nr_finalize=>finalize
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
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          chmd = MODE_SURFACE
          if (ierr.eq.0) call ns_finalize(ierr, utmp, levv=lv, mode=lmd)
          ! if (ierr.eq.0) call nh_finalize(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call nr_finalize(ierr, utmp, levv=lv, mode=chmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize
!!!_ + user interfaces
!!!_  - axis_set_header
  subroutine axis_set_header &
       & (ierr, hd, &
       &  item, n,  flag)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_header,only: put_item
    use TOUZA_Nio_header,only: hi_ITEM, hi_DSET
    use TOUZA_Nio_record,only: put_header_cprop
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: hd(*)
    character(len=*),intent(in)          :: item
    integer,         intent(in)          :: n
    integer,         intent(in),optional :: flag
    integer m
    integer f
    ! ITEM: same as AITM1
    ! DSET: AXLOC, AXWGT, etc
    ierr = 0
    f = choice(axis_loc, flag)
    ! write(*, *) f, DSET_specials(f), item
    if (f.lt.0.or.f.ge.nspec) then
       ierr = _ERROR(ERR_INVALID_PARAMETER)
    endif
    if (ierr.eq.0) then
       m = n
       if (IAND(f, axis_cyclic).ne.0) m = m + 1
    endif
    if (ierr.eq.0) call put_item(ierr, hd, DSET_specials(f), hi_DSET)
    if (ierr.eq.0) call put_item(ierr, hd, item, hi_ITEM)
    if (ierr.eq.0) call put_header_cprop(ierr, hd, item, (/1, m/), 1)
    if (ierr.eq.0) call put_header_cprop(ierr, hd, ' ',  (/1, 1/), 2)
    if (ierr.eq.0) call put_header_cprop(ierr, hd, ' ',  (/1, 1/), 3)
  end subroutine axis_set_header

!!!_  & axis_parse()
  integer function axis_parse (dset) result(k)
    use TOUZA_Nio_std,only: find_first
    implicit none
    character(len=*),intent(in) :: dset
    k = find_first(DSET_specials, dset)
  end function axis_parse

!!!_  & is_axis_set()
  logical function is_axis_set (hd) result(b)
    use TOUZA_Nio_header,only: hi_DSET
    implicit none
    character(len=*),intent(in) :: hd(*)
    b = ANY(hd(hi_DSET).eq.DSET_specials(:))
  end function is_axis_set
!!!_ + end module TOUZA_Nio_axis
end module TOUZA_Nio_axis

!!!_@ test_nio_axis - test program
#ifdef TEST_NIO_AXIS
program test_nio_axis
  use TOUZA_Std,       only: parse, get_param, arg_diag, arg_init
  use TOUZA_Std,       only: upcase, get_option
  use TOUZA_Std,       only: MPI_COMM_NULL
  use TOUZA_Nio_axis
  implicit none
  integer ierr
  integer jarg
  integer ktest

  ierr = 0
  jarg = 0
101 format(A,' = ', I0)
  call init(ierr, stdv=-9, icomm=MPI_COMM_NULL)
  ! call init(ierr, stdv=-9, icomm=MPI_COMM_NULL, levv=+99)
  ! if (ierr.eq.0) call diag(ierr, u=-1, levv=+99)
  if (ierr.eq.0) call diag(ierr, levv=+9)
  if (ierr.eq.0) call arg_init(ierr, levv=-9)
  if (ierr.eq.0) call parse(ierr)
  if (ierr.eq.0) call arg_diag(ierr)
  if (ierr.eq.0) then
     jarg = jarg + 1
     call get_param(ierr, ktest, jarg, 0)
  endif
  if (ierr.eq.0) then
201  format('##### TEST ', I0, 1x, A)
     select case (ktest)
     case (0)
     case default
        write(*, *) 'INVALID TEST = ', ktest
        ierr = -1
     end select
  endif

  if (ierr.eq.0) call finalize(ierr)
  write(*, 101) 'FINAL', ierr
  stop
contains

end program test_nio_axis
#endif /* TEST_NIO_AXIS */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

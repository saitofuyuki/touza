!!!_! nio_control.F90 - TOUZA/Nio control center
! Maintainer: SAITO Fuyuki
! Created: Dec 12 2022
#define TIME_STAMP 'Time-stamp: <2022/12/15 10:09:21 fuyuki nio_control.F90>'
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
!!!_@ TOUZA_Nio_control - nio control center
module TOUZA_Nio_control
!!!_ = declaration
  use TOUZA_Nio_std,only: &
       & KI32, KI64, KDBL, KFLT, KIOFS, &
       & control_mode, control_deep, is_first_force, &
       & get_logu,     unit_global,  trace_fine,   trace_control
  use TOUZA_Nio_header, nh_init=>init, nh_diag=>diag, nh_finalize=>finalize
  implicit none
  private
!!!_  - public parameter
  integer,parameter,public :: nio_format_other = 0   ! not Nio format
  integer,parameter,public :: nio_format_std   = 1   ! standard Nio format, MIROC/gtool-3.5 compatible
  integer,parameter,public :: nio_format_ext   = 2   ! extended Nio format
!!!_  - private parameter
!!!_  - static
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = NIO_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
#define __MDL__ 'x'
!!!_  - interfaces
!!!_  - public procedures
  public init, diag, finalize
  public nio_check_format
!!!_  - public shared
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init &
       & (ierr, u, levv, mode, stdv, icomm)
    use TOUZA_Nio_std,   only: ns_init=>init, choice, get_size_bytes, KDBL
    use TOUZA_Nio_header,only: nh_init=>init
    use TOUZA_Nio_record,only: nr_init=>init
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
          if (ierr.eq.0) call ns_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
          if (ierr.eq.0) call nh_init(ierr, u=ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_init(ierr, u=ulog, levv=lv, mode=lmd)
       endif
       if (init_counts.eq.0) then
          continue
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Nio_std,   only: ns_diag=>diag, choice, msg, is_msglev_normal, is_msglev_info
    use TOUZA_Nio_header,only: nh_diag=>diag
    use TOUZA_Nio_record,only: nr_diag=>diag
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
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ns_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nh_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_diag(ierr, utmp, levv=lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Nio_std,   only: ns_finalize=>finalize, choice
    use TOUZA_Nio_header,only: nh_finalize=>finalize
    use TOUZA_Nio_record,only: nr_finalize=>finalize
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
          if (ierr.eq.0) call nh_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call ns_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_finalize(ierr, utmp, levv=lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize
!!!_  - init subcontracts
!!!_ + user interfaces
!!!_  - nio_check_format - Check whether file is Nio format
  integer function nio_check_format (file, cat) result(n)
    use TOUZA_Nio_std,only: new_unit, search_from_last
    use TOUZA_Nio_std,only: sus_open, sus_close
    use TOUZA_Nio_record,only: nio_read_header
    implicit none
    character(len=*),intent(in) :: file
    integer,optional,intent(in) :: cat   ! category id for unit search in new_unit()
    integer u
    integer jerr
    integer krect
    character(len=litem) :: head(nitem)

    n = ERR_INVALID_PARAMETER
    u = new_unit(search_from_last, cat)
    if (u.lt.0) then
       n = u
       return
    endif
    call sus_open(jerr, u, file, ACTION='R', STATUS='O')
    if (jerr.eq.0) then
       call nio_read_header(jerr, head,  krect, u)
       if (jerr.eq.0) then
          ! to check Nio extension
          n = nio_format_std
       else
          n = nio_format_other
          jerr = 0
       endif
    endif
    if (jerr.eq.0) call sus_close(jerr, u, file)
    if (jerr.lt.0) n = jerr
  end function nio_check_format
!!!_  - nio_open - open Nio file
!!!_  - nio_close - close Nio file
!!!_ + end module
end module TOUZA_Nio_control

!!!_@ test_nio_control - test program
#ifdef TEST_NIO_CONTROL
program test_nio_control
  use TOUZA_Std,only: parse, get_param, arg_diag, arg_init
  use TOUZA_Nio_control
  implicit none

  integer ierr
  integer jarg
  integer,parameter :: lpath = 256
  character(len=lpath) :: file
  integer kfmt

101 format(A,' = ', I0)

  ierr = 0
  jarg = 0
  call init(ierr, stdv=-9)
  if (ierr.eq.0) call diag(ierr, levv=+9)
  if (ierr.eq.0) call arg_init(ierr, levv=-9)
  if (ierr.eq.0) call parse(ierr)
  if (ierr.eq.0) call arg_diag(ierr)

201 format('format:', I0, 1x, A)
  if (ierr.eq.0) then
     do
        jarg = jarg + 1
        call get_param(ierr, file, jarg)
        if (file.eq.' ') cycle
        if (ierr.ne.0) then
           ierr = 0
           exit
        endif
        kfmt = nio_check_format(file)
        write(*, 201) kfmt, trim(file)
     enddo
  endif

  if (ierr.eq.0) call finalize(ierr)
  write(*, 101) 'FINAL', ierr
  stop
contains
end program test_nio_control

#endif /* TEST_NIO_CONTROL */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

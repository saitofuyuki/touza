!!!_! std_fun.F90 - touza/std file units manipulation
! Maintainer: SAITO Fuyuki
! Created: Jun 22 2020
#define TIME_STAMP 'Time-stamp: <2021/01/26 14:56:05 fuyuki std_fun.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2020, 2021
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
!!!_* include
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_std.h"
!!!_* macros
#ifndef   OPT_MAX_FILE_UNITS
#  define OPT_MAX_FILE_UNITS 512 /* maximum number of file units */
#endif
#ifndef   OPT_MAX_BLACK_UNITS
#  define OPT_MAX_BLACK_UNITS 128 /* maximum number of black-list units */
#endif
!!!_@ TOUZA_Std_fun - file units manipulation
module TOUZA_Std_fun
!!!_ = declaration
!!!_  - default
  implicit none
  private
!!!_  - parameter
  integer,parameter :: lblack = OPT_MAX_BLACK_UNITS
# define __MDL__ 'fun'
# define __TAG__ STD_FORMAT_MDL(__MDL__)
!!!_  - static
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL

  integer,save :: limu = OPT_MAX_FILE_UNITS

  integer,save :: nblack = 0
  integer,save :: ublist(lblack+1) = -1

  integer,save :: ulast = -1
!!!_  - public
  public init, diag, finalize
  public add_black_list, is_black_listed
  public brute_force_check_units
  public new_unit
  public new_unit_nn
!!!_ + common interfaces
contains
!!!_  & init
  subroutine init(ierr, levv, mode)
    use TOUZA_Std_utl, only: utl_init=>init, choice
    use TOUZA_Std_log, only: log_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: levv, mode
    integer md, lv

    ierr = 0

    lv = choice(lev_verbose, levv)
    md = choice(INIT_DEFAULT, mode)
    if (md.eq.INIT_DEFAULT) md = INIT_DEEP

    if (md.gt.INIT_DEFAULT) then
       if (md.ge.INIT_DEEP) then
          if (ierr.eq.0) call utl_init(ierr, levv=lv, mode=md)
          if (ierr.eq.0) call log_init(ierr, levv=lv, mode=md)
       endif
       if (init_counts.eq.0) then
          lev_verbose = lv
       endif
       init_counts = init_counts + 1
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std_utl, only: utl_diag=>diag, choice
    use TOUZA_Std_log, only: log_diag=>diag, msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: levv, mode
    integer,intent(in),optional :: u
    integer md, lv

    ierr = 0
    lv = choice(lev_verbose, levv)
    md = choice(DIAG_DEFAULT, mode)
    if (md.eq.DIAG_DEFAULT) md = DIAG_DEEP

    if (md.gt.DIAG_DEFAULT) then
       if (IAND(md, DIAG_DEEP).gt.0) then
          if (ierr.eq.0) call utl_diag(ierr, u, lv, md)
          if (ierr.eq.0) call log_diag(ierr, u, lv, md)
       endif
       if (diag_counts.eq.0.or.IAND(md,DIAG_FORCE).gt.0) then
          if (ierr.eq.0) then
             if (VCHECK_NORMAL(lv)) then
                call msg_mdl(TIME_STAMP, __MDL__, u)
             endif
             if (VCHECK_NORMAL(lv)) then
                call msg_mdl('(''limit file units = '', I0)', (/ limu /), __MDL__, u)
             endif
             if (VCHECK_INFO(lv)) then
                call diag_black_list(ierr, u)
             endif
          endif
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_utl, only: utl_finalize=>finalize
    use TOUZA_Std_log, only: log_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: levv, mode
    integer,intent(in),optional :: u
    ierr = 0
    if (ierr.eq.0) call log_finalize(ierr, u, levv, mode)
    if (ierr.eq.0) call utl_finalize(ierr, u, levv, mode)
    return
  end subroutine finalize

!!!_ + file open/close
!!!_ + unit number
!!!_  & new_unit () - return unbound i/o unit number
  integer function new_unit &
       & (ksw) &
       & result(un)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(in),optional :: ksw
    integer :: kswi
    integer :: ui
    integer :: uoff
    integer jerr
    logical opnd

    kswi = choice(+1, ksw)
    if (kswi.lt.0) then
       uoff = 0
    else if (kswi.eq.0) then
       uoff = mod(max(0, ulast), limu + 1)
    else
       uoff = mod(ulast + 1, limu + 1)
    endif
    do ui = 0, limu
       un = mod(uoff + ui, limu + 1)
       if (is_black_listed(un)) then
          continue
       else
          inquire(UNIT=un, IOSTAT=jerr, OPENED=opnd)
          if (jerr.eq.0 .and. .not.opnd) then
             ulast = un
             return
          endif
       endif
    enddo
    un = -1
    return
  end function new_unit

!!!_  & new_unit_nn () - return unbound i/o unit number (not existing file)
  integer function new_unit_nn &
       & (ksw) &
       & result(un)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(in),optional :: ksw
    integer jerr
    do
       un = new_unit(ksw)
       if (un.lt.0) exit
       open(UNIT=un, STATUS='NEW', IOSTAT=jerr)
       if (jerr.eq.0) then
          close(UNIT=un, STATUS='delete', IOSTAT=jerr)
          exit
       endif
       close(UNIT=un, IOSTAT=jerr)
    enddo
    return
  end function new_unit_nn

!!!_ + black list manipulation
!!!_  & add_black_list - register unit(s) in the black list
  subroutine add_black_list &
       & (ierr, ub, ue)
#   define __PROC__ 'add_black_list'
    use TOUZA_Std_log,only: msg_fun
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: ub
    integer,intent(in),optional :: ue
    integer ul, uh, nu
    integer ut
    integer jp, jins

    ierr = 0

    ! ublist is a sorted list of integers.
    ul = ub
    uh = min(limu, max(ul, choice(ul, ue)))

    jins = 1
    ublist(nblack+1) = limu + 1 ! sentry
    do
       if (ul.gt.uh) exit
       if (jins.gt.nblack+1) exit
       if (ul.eq.ublist(jins)) then
          ul = ul + 1
       else if (ul.lt.ublist(jins)) then
          ut = min(uh, ublist(jins) - 1)
          nu = ut - ul + 1
          if (nblack + nu.gt.lblack) then
             ierr = -1
             exit
          endif
          do jp = nblack, jins, -1
             ublist(jp + nu) = ublist(jp)
          enddo
          do jp = jins, jins + nu - 1
             ublist(jp) = ul + jp - jins
          enddo
          ul = ut + 1
          jins = jins + nu
          nblack = nblack + nu
       endif
       jins = jins + 1
    enddo
    if (ierr.ne.0) then
       if (VCHECK_SEVERE(lev_verbose)) &
            call msg_fun('Black list overflow', __MDL__, __PROC__)
    endif
#   undef __PROC__
    return
  end subroutine add_black_list
!!!_  & is_black_listed() - check if units is black-listed
  logical function is_black_listed &
       & (u) result (r)
    integer,intent(in) :: u
    integer jp
    do jp = 1, nblack
       if (u.eq.ublist(jp)) then
          r = .true.
          return
       endif
       if (u.lt.ublist(jp)) exit
    enddo
    r = .false.
    return
  end function is_black_listed
!!!_  & diag_black_list
  subroutine diag_black_list &
       & (ierr, u)
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer jp, jbl, jbh
    integer,parameter :: nstp = 10
    ierr = 0
    do jp = 0, nblack - 1, nstp
       jbl = jp + 1
       jbh = min(nblack, jp + nstp)
       call msg_mdl &
            & ('(''units in black list:'', 10(1x, I0))', ublist(jbl:jbh), &
            &  __MDL__, u)
    enddo
    return
  end subroutine diag_black_list
!!!_ + limit check
!!!_  & brute_force_check_units - numbers of units brute-force checker
  subroutine brute_force_check_units &
       & (ierr, limit, ulog)
#   define __PROC__ 'brute_force_check_units'
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: limit
    integer,intent(in),optional :: ulog

    integer,parameter   :: ubgn = 0
    integer,allocatable :: stt(:)
    integer jdmy
    integer jchk, lchk
    integer nopn, ncls, nopnd
    logical opnd
    integer uli
    character(len=1024) :: tmsg, txt

    ierr = 0
    lchk = choice(limu, limit)
    if (lchk.lt.0) lchk = max(0, limu)
    uli = choice(-1, ulog)
    allocate(stt(ubgn:lchk), STAT=ierr)
    stt(:) = 0
    nopn  = 0
    nopnd = 0
    if (ierr.eq.0) then
       do jchk = ubgn, lchk
          inquire(UNIT=jchk, IOSTAT=ierr, OPENED=opnd)
          tmsg = ' '
          if (ierr.eq.0) then
             if (OPND) then
                stt(jchk) = 1
                nopnd = nopnd + 1
             else
                stt(jchk) = 2
                call open_scratch(ierr, jchk, tmsg)
                if (ierr.eq.0) nopn = nopn + 1
             endif
          endif
101       format('file unit check[', I0, '] ', I0, 1x, I0, 1x, A)
          write(txt, 101) jchk, stt(jchk), ierr, trim(tmsg)
          call msg_mdl(txt, __MDL__, ulog)
          if (ierr.ne.0) exit
       enddo
    endif
    ncls = 0
    if (allocated(stt)) then
       do jchk = ubgn, lchk
          if (stt(jchk).eq.2) then
             close(UNIT=jchk, IOSTAT=jdmy)
             if (jdmy.eq.0) ncls = ncls + 1
          endif
       enddo
       deallocate(stt, STAT=jdmy)
    endif
    call msg_mdl('(''file unit check counts = '', I0, 1x, I0, 1x, I0)', &
         & (/nopnd, nopn, ncls/), &
         & __MDL__, ulog)
    return
#   undef __PROC__
  end subroutine brute_force_check_units

!!!_  & open_scratch - open wrapper
  subroutine open_scratch &
       & (ierr, u, tmsg)
#   define __PROC__ 'open_scratch'
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: u
    character(len=*),intent(out) :: tmsg
    ierr = 0
    tmsg = ' '
#if HAVE_OPEN_IOMSG
    open(UNIT=u, STATUS='SCRATCH', IOSTAT=iErr, IOMSG=tmsg)
#else  /* not HAVE_OPEN_IOMSG */
    open(UNIT=u, STATUS='SCRATCH', IOSTAT=iErr)
    if (ierr.ne.0) then
101    format('open error for scratch = ', I0)
       write(tmsg, 101) iErr
    endif
#endif /* not HAVE_OPEN_IOMSG */
    return
#   undef __PROC__
  end subroutine open_scratch

end module TOUZA_Std_fun

!!!_@ test_std_fun - test program
#ifdef TEST_STD_FUN
program test_std_fun
  use TOUZA_Std_fun
  implicit none
  integer ierr
  integer n, un

  call init(ierr)
  if (ierr.eq.0) call add_black_list(ierr, 30)
  if (ierr.eq.0) call add_black_list(ierr, 40)
  if (ierr.eq.0) call add_black_list(ierr, 50)
  if (ierr.eq.0) call add_black_list(ierr, 20)
  if (ierr.eq.0) call add_black_list(ierr, 35)
  if (ierr.eq.0) call add_black_list(ierr, 3, 8)
  if (ierr.eq.0) call add_black_list(ierr, 28, 33)

  if (ierr.eq.0) call diag(ierr, levv=+2)
  if (ierr.eq.0) call finalize(ierr)
  if (ierr.eq.0) then
     call brute_force_check_units(ierr, limit=20)
     call brute_force_check_units(ierr, limit=200)
  endif
  if (ierr.eq.0) then
     do n = 0, 30
        un = new_unit()
        write(*, *) 'new unit: ', n, un
     enddo
  endif

101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_std_fun
#endif /* TEST_STD_FUN */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

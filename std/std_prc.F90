!!!_! std_prc.F90 - touza/std precision(kind) manager
! Maintainer: SAITO Fuyuki
! Created: Sep 6 2020
#define TIME_STAMP 'Time-stamp: <2021/01/26 11:29:34 fuyuki std_prc.F90>'
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
#ifndef    OPT_REAL_SINGLE_DIGITS
#  define  OPT_REAL_SINGLE_DIGITS  6     /* decimal precision */
#endif
#ifndef    OPT_REAL_DOUBLE_DIGITS
#  define  OPT_REAL_DOUBLE_DIGITS  15    /* decimal precision */
#endif
#ifndef    OPT_REAL_SINGLE_EXP
#  define  OPT_REAL_SINGLE_EXP 37        /* decimal exponent range */
#endif
#ifndef    OPT_REAL_DOUBLE_EXP
#  define  OPT_REAL_DOUBLE_EXP 307       /* decimal exponent range */
#endif
!!!_@ TOUZA_Std_prc - precision
module TOUZA_Std_prc
!!!_ = declaration
  implicit none
  private
# define __TAG__ STD_FORMAT_MDL('prc')
!!!_  - real precisions
  integer,parameter :: dflt = OPT_REAL_SINGLE_DIGITS
  integer,parameter :: xflt = OPT_REAL_SINGLE_EXP
  integer,parameter :: ddbl = OPT_REAL_DOUBLE_DIGITS
  integer,parameter :: xdbl = OPT_REAL_DOUBLE_EXP

  integer,parameter,public :: KFLT = SELECTED_REAL_KIND(dflt, xflt)
  integer,parameter,public :: KDBL = SELECTED_REAL_KIND(ddbl, xdbl)
!!!_  - public procedures
  public init, diag, finalize
  public diag_real_kinds
  public diag_int_kinds
!!!_  - static
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, levv, mode)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: levv, mode
    integer md

    ierr = 0

    md = idef(mode, INIT_DEFAULT)
    if (md.eq.INIT_DEFAULT) md = INIT_DEEP

    if (md.gt.INIT_DEFAULT) then
       if (init_counts.eq.0) then
          lev_verbose = idef(levv, lev_verbose)
       endif
       init_counts = init_counts + 1
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md

    ierr = 0
    utmp = idef(u)
    lv = idef(levv, lev_verbose)
    md = idef(mode, DIAG_DEFAULT)
    if (md.eq.DIAG_DEFAULT) md = DIAG_DEEP

    if (md.gt.DIAG_DEFAULT) then
       if (diag_counts.eq.0.or.IAND(md,DIAG_FORCE).gt.0) then
101       format(__TAG__, A)
          if (VCHECK_NORMAL(lv)) then
             if (utmp.ge.0) then
                write(utmp, 101) TIME_STAMP
             else
                write(*,  101) TIME_STAMP
             endif
          endif
201       format(__TAG__, A, ' = ', I0, 2x, I0, ',', I0)
          if (VCHECK_NORMAL(lv)) then
             if (utmp.ge.0) then
                write(utmp, 201) 'single', KFLT, dflt, xflt
                write(utmp, 201) 'double', KDBL, ddbl, xdbl
             else
                write(*,  201) 'single', KFLT, dflt, xflt
                write(*,  201) 'double', KDBL, ddbl, xdbl
             endif
          endif
          if (VCHECK_DETAIL(lv)) then
             if (ierr.eq.0) call diag_int_kinds(ierr, u)
             if (ierr.eq.0) call diag_real_kinds(ierr, u)
          endif
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & diag_int_kinds - brute-force checker of integer kinds
  subroutine diag_int_kinds &
       & (ierr, u, maxr, minr)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: maxr, minr
    integer rbgn, rend, jr
    integer utmp
    integer k
    integer kprv, knxt

    ierr = 0
    utmp = idef(u)
    rbgn  = idef(minr, 1)
    rend = max(rbgn+1, idef(maxr, 128))

    kprv = selected_int_kind(rbgn)

101 format(__TAG__, 'integer:', I0, ' = ', I0)
    do jr = rbgn, rend
       k = selected_int_kind(jr)
       if (kprv.ne.k) then
          knxt = k
          if (utmp.ge.0) then
             write(utmp, 101) kprv, jr - 1
          else
             write(*, 101) kprv, jr - 1
          endif
          kprv = knxt
       endif
       if (knxt.lt.0) exit
    enddo

  end subroutine diag_int_kinds

!!!_  & diag_real_kinds - brute-force checker of real kinds
  subroutine diag_real_kinds &
       & (ierr, u, maxp, minp)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: maxp, minp
    integer jp, pend
    integer jx
    integer utmp
    integer k
    integer kprv, knxt

    ierr = 0
    utmp = idef(u)
    jp   = idef(minp, 1)
    pend = max(jp, idef(maxp, 128))

    kprv = selected_real_kind(jp)

101 format(__TAG__, 'real:', I0, ' = ', I0, ',', I0)
    do jp = jp, pend
       k = selected_real_kind(jp)
       if (kprv.ne.k) then
          knxt = k
          jx = 1
          do
             k = selected_real_kind(jp - 1, jx)
             if (kprv.ne.k) exit
             jx = jx + 1
          enddo
          if (utmp.ge.0) then
             write(utmp, 101) kprv, jp - 1, jx - 1
          else
             write(*, 101) kprv, jp - 1, jx - 1
          endif
          kprv = knxt
       endif
       if (knxt.lt.0) exit
    enddo

  end subroutine diag_real_kinds

!!!_   & finalize
  subroutine finalize(ierr, u, levv, mode)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    ierr = 0 * idef(u) * idef(levv) * idef(mode) ! dummy
    return
  end subroutine finalize

!!!_ + private subroutines
!!!_  & idef - choice() stand-alone
  integer function idef(i0, i1) result(iv)
    implicit none
    integer,intent(in),optional :: i0, i1
    if (present(i0)) then
       iv = i0
    else if (present(i1)) then
       iv = i1
    else
       iv = -1
    endif
    return
  end function idef
end module TOUZA_Std_prc

!!!_@ test_std_prc - test program
#ifdef TEST_STD_PRC
program test_std_prc
  use TOUZA_Std_prc
  implicit none
  integer ierr

  call init(ierr)
  if (ierr.eq.0) call diag(ierr, levv=-1)
  if (ierr.eq.0) call diag(ierr, levv=10, mode=DIAG_DEEP_FORCE)
  if (ierr.eq.0) call finalize(ierr)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_std_prc

#endif /* TEST_STD_PRC */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

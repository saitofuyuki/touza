!!!_! std_utl.F90 - touza/std utilities
! Maintainer: SAITO Fuyuki
! Created: Jun 4 2020
#define TIME_STAMP 'Time-stamp: <2021/02/16 18:09:45 fuyuki std_utl.F90>'
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
#ifndef   OPT_ENABLE_FORTRAN_ELEMENTAL
#  define OPT_ENABLE_FORTRAN_ELEMENTAL 0
#endif
!!!_* switch
#if OPT_ENABLE_FORTRAN_ELEMENTAL
#  define _ELEMENTAL ELEMENTAL
#else
#  define _ELEMENTAL
#endif
#define _CHOICE_DECL _ELEMENTAL
#define _CONDOP_DECL _ELEMENTAL
!!!_@ TOUZA_Std_utl - small utilities
module TOUZA_Std_utl
  use TOUZA_Std_prc, only: KFLT, KDBL
!!!_ = declaration
  implicit none
  private
!!!_  - paramters
# define __TAG__ STD_FORMAT_MDL('utl')
!!!_  - interfaces
  interface choice
     module procedure choice_i
     module procedure choice_l
     module procedure choice_f
     module procedure choice_d
  end interface choice

  interface set_if_present
     module procedure set_if_present_i
     module procedure set_if_present_l
     module procedure set_if_present_f
     module procedure set_if_present_d
     module procedure set_if_present_a
  end interface set_if_present

  interface condop
     module procedure condop_i
     module procedure condop_f
     module procedure condop_d
     module procedure condop_l
  end interface condop

  interface upcase
     module procedure upcase_m
     module procedure upcase_o
  end interface upcase

  interface downcase
     module procedure downcase_m
     module procedure downcase_o
  end interface downcase

!!!_  - public
  public init, diag, finalize
  public choice, choice_a
  public set_if_present
  public condop
  public chcount
  public upcase, downcase
!!!_  - static
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, levv, mode)
    use TOUZA_Std_prc,only: prc_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: levv, mode
    integer md, lv

    ierr = 0

    lv = choice(lev_verbose, levv)
    md = choice(INIT_DEFAULT, mode)
    if (md.eq.INIT_DEFAULT) md = INIT_DEEP

    if (md.gt.INIT_DEFAULT) then
       if (md.ge.INIT_DEEP) call prc_init(ierr, levv=lv, mode=md)
       if (init_counts.eq.0) then
          lev_verbose = lv
       endif
       init_counts = init_counts + 1
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std_prc,only: prc_diag=>diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp
    integer lv, md

    ierr = 0
    utmp = choice(-1, u)
    lv = choice(lev_verbose, levv)
    md = choice(DIAG_DEFAULT, mode)
    if (md.eq.DIAG_DEFAULT) md = DIAG_DEEP

    if (md.gt.DIAG_DEFAULT) then
       if (IAND(md, DIAG_DEEP).gt.0) then
          if (ierr.eq.0) call prc_diag(ierr, utmp, lv, md)
       endif
       if (diag_counts.eq.0.or.IAND(md,DIAG_FORCE).gt.0) then
          if (ierr.eq.0) then
101          format(__TAG__, A)
102          format(__TAG__, 'with elemental = ', I0)
             if (VCHECK_NORMAL(lv)) then
                if (utmp.ge.0) then
                   write(utmp, 101) TIME_STAMP
                else
                   write(*,    101) TIME_STAMP
                endif
             endif
             if (VCHECK_INFO(lv)) then
                if (utmp.ge.0) then
                   write(utmp, 102) OPT_ENABLE_FORTRAN_ELEMENTAL
                else
                   write(*,    102) OPT_ENABLE_FORTRAN_ELEMENTAL
                endif
             endif
          endif
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_prc,only: prc_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer lv

    ierr = 0
    lv = choice(lev_verbose, levv)
    call prc_finalize(ierr, u, lv, mode)
    return
  end subroutine finalize
!!!_ + user subroutines
!!!_  & choice() - return D if not present A, otherwise A
  _CHOICE_DECL integer function choice_i(d, a) result(r)
    integer,intent(in)          :: d
    integer,intent(in),optional :: a
    if (present(a)) then
       r = a
    else
       r = d
    endif
    return
  end function choice_i

  _CHOICE_DECL logical function choice_l(d, a) result(r)
    logical,intent(in)          :: d
    logical,intent(in),optional :: a
    if (present(a)) then
       r = a
    else
       r = d
    endif
    return
  end function choice_l

  _CHOICE_DECL real(kind=KFLT) function choice_f(d, a) result(r)
    real(kind=KFLT),intent(in)          :: d
    real(kind=KFLT),intent(in),optional :: a
    if (present(a)) then
       r = a
    else
       r = d
    endif
    return
  end function choice_f

  _CHOICE_DECL real(kind=KDBL) function choice_d(d, a) result(r)
    real(kind=KDBL),intent(in)          :: d
    real(kind=KDBL),intent(in),optional :: a
    if (present(a)) then
       r = a
    else
       r = d
    endif
    return
  end function choice_d

!!!_  & choice_a - work around for choice() string
  subroutine choice_a &
       & (v, d, a)
    implicit none
    character(len=*),intent(inout)       :: v
    character(len=*),intent(in),optional :: d  ! default
    character(len=*),intent(in),optional :: a  ! argument
    if (present(a)) then
       v = a
    else if (present(d)) then
       v = d
    else
       continue
    endif
    return
  end subroutine choice_a

!!!_  & set_if_present - set value if variable is present
  subroutine set_if_present_i(var, val)
    implicit none
    integer,intent(out),optional :: var
    integer,intent(in)           :: val
    if (present(var)) then
       var = val
    endif
    return
  end subroutine set_if_present_i

  subroutine set_if_present_f(var, val)
    implicit none
    real(kind=KFLT),intent(out),optional :: var
    real(kind=KFLT),intent(in)           :: val
    if (present(var)) then
       var = val
    endif
    return
  end subroutine set_if_present_f

  subroutine set_if_present_d(var, val)
    implicit none
    real(kind=KDBL),intent(out),optional :: var
    real(kind=KDBL),intent(in)           :: val
    if (present(var)) then
       var = val
    endif
    return
  end subroutine set_if_present_d

  subroutine set_if_present_l(var, val)
    implicit none
    logical,intent(out),optional :: var
    logical,intent(in)           :: val
    if (present(var)) then
       var = val
    endif
    return
  end subroutine set_if_present_l

  subroutine set_if_present_a(var, val)
    implicit none
    character(len=*),intent(out),optional :: var
    character(len=*),intent(in)           :: val
    if (present(var)) then
       var = val
    endif
    return
  end subroutine set_if_present_a

!!!_  & condop() - conditional operator
  _CONDOP_DECL integer function condop_i (l, vt, vf) result(r)
    implicit none
    logical,intent(in) :: l
    integer,intent(in) :: vt, vf
    if (l) then
       r = vt
    else
       r = vf
    endif
    return
  end function condop_i

  _CONDOP_DECL real(kind=KFLT) function condop_f (l, vt, vf) result(r)
    implicit none
    logical,        intent(in) :: l
    real(kind=KFLT),intent(in) :: vt, vf
    if (l) then
       r = vt
    else
       r = vf
    endif
    return
  end function condop_f

  _CONDOP_DECL real(kind=KDBL) function condop_d (l, vt, vf) result(r)
    implicit none
    logical,        intent(in) :: l
    real(kind=KDBL),intent(in) :: vt, vf
    if (l) then
       r = vt
    else
       r = vf
    endif
    return
  end function condop_d

  _CONDOP_DECL logical function condop_l (l, vt, vf) result(r)
    implicit none
    logical,intent(in) :: l
    logical,intent(in) :: vt, vf
    if (l) then
       r = vt
    else
       r = vf
    endif
    return
  end function condop_l

!!!_  & chcount - count character(s) occurrence
  integer function chcount (str, chs) &
       & result(n)
    implicit none
    character(len=*),intent(in) :: str
    character(len=*),intent(in) :: chs
    integer i, l
    n = 0
    l = len(str)
    ! write(*, *) 'COUNT', str, chs
    do i = 1, l
       if (index(chs, str(i:i)).gt.0) n = n + 1
    enddo
  end function chcount

!!!_  & upcase() - upper case conversion
  subroutine upcase_m(S)
    implicit none
    character(len=*),intent(inout) :: S
    integer ka, kz, kc, koff
    integer j
    ka = IACHAR('a')
    kz = IACHAR('z')
    koff = IACHAR('A') - ka
    do j = 1, len_trim(S)
       kc = IACHAR(S(j:j))
       if (kc.ge.ka.and.kc.le.kz) S(j:j) = ACHAR(kc + koff)
    enddo
  end subroutine upcase_m

  subroutine upcase_o(SO, SI)
    implicit none
    character(len=*),intent(out) :: SO
    character(len=*),intent(in)  :: SI
    SO = SI
    call upcase_m(SO)
  end subroutine upcase_o

!!!_  & downcase() - lower case conversion
  subroutine downcase_m(S)
    implicit none
    character(len=*),intent(inout) :: S
    integer ka, kz, kc, koff
    integer j
    ka = IACHAR('A')
    kz = IACHAR('Z')
    koff = IACHAR('a') - ka
    do j = 1, len_trim(S)
       kc = IACHAR(S(j:j))
       if (kc.ge.ka.and.kc.le.kz) S(j:j) = ACHAR(kc + koff)
    enddo
  end subroutine downcase_m

  subroutine downcase_o(SO, SI)
    implicit none
    character(len=*),intent(out) :: SO
    character(len=*),intent(in)  :: SI
    SO = SI
    call downcase_m(SO)
  end subroutine downcase_o

end module TOUZA_Std_utl

!!!_@ test_std_utl - test program
#if TEST_STD_UTL
program test_std_utl
  use TOUZA_Std_utl
  implicit none
  integer ierr
  character(len=128) :: T0, T1

  call init(ierr, levv=+8)
  call diag(ierr)

  T0 = 'abcABCxyzXYZ012:;/'
101 format('to:', A, 1x, '[', A, '] [', A, ']')
  call upcase(T1, T0)
  write(*, 101) 'U', trim(T0), trim(T1)
  call downcase(T1, T0)
  write(*, 101) 'D', trim(T0), trim(T1)

  call finalize(ierr)

  stop
end program test_std_utl
#endif /* TEST_STD_UTL */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

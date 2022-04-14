!!!_! std_utl.F90 - touza/std utilities
! Maintainer: SAITO Fuyuki
! Created: Jun 4 2020
#define TIME_STAMP 'Time-stamp: <2022/02/07 20:41:18 fuyuki std_utl.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2020, 2021, 2022
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
  use TOUZA_Std_prc, only: KFLT, KDBL, KI64
#define __MDL__ 'utl'
!!!_ = declaration
  implicit none
  private
!!!_  - paramters
# define __TAG__ STD_FORMAT_MDL('utl')
!!!_  - interfaces
  interface choice
     module procedure choice_i
     module procedure choice_long
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

  interface ndigits
     module procedure ndigits_i
  end interface ndigits

!!!_  - public
  public init, diag, finalize
  public choice, choice_a
  public set_if_present
  public condop
  public chcount
  public upcase, downcase
  public ndigits
  public control_mode, control_deep, is_first_force
  public set_defu
!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT - ERR_MASK_STD_UTL
  integer,save :: ulog = -1
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode)
    use TOUZA_Std_prc,only: prc_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer md, lv, lmd

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
          if (ierr.eq.0) call prc_init(ierr, ulog, levv=lv, mode=lmd)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT - ERR_MASK_STD_UTL
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
    integer lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (ierr.ne.0 .and. IAND(md, MODE_LOOSE).gt.0) then
          if (VCHECK_NORMAL(lv)) then
301          format(STD_FORMAT_FUN(__MDL__, 'diag'), 'loose: ', I0)
             if (utmp.ge.0) then
                write(utmp, 301) ierr
             else
                write(*,    301) ierr
             endif
          endif
          ierr = 0
       endif
       if (is_first_force(diag_counts, md)) then
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
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call prc_diag(ierr, utmp, lv, mode=lmd)
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

    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, md)) then
          if (VCHECK_DEBUG(lv)) then
311          format(STD_FORMAT_FUN(__MDL__, 'finalize'), 'fine: ', I0, 1x, I0, 1x, I0, 1x, I0)
             if (utmp.ge.0) then
                write(utmp, 311) ierr, init_counts, diag_counts, fine_counts
             else
                write(*,    311) ierr, init_counts, diag_counts, fine_counts
             endif
          endif
       endif
       if (ierr.ne.0 .and. IAND(md, MODE_LOOSE).gt.0) then
          if (VCHECK_NORMAL(lv)) then
301          format(STD_FORMAT_FUN(__MDL__, 'finalize'), 'loose: ', I0)
             if (utmp.ge.0) then
                write(utmp, 301) ierr
             else
                write(*,    301) ierr
             endif
          endif
          ierr = 0
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call prc_finalize(ierr, utmp, lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize
!!!_ + user subroutines
!!!_  & set_defu - set global logging unit
  subroutine set_defu(u)
    implicit none
    integer,intent(in) :: u
    ulog = u
    return
  end subroutine set_defu

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

  _CHOICE_DECL integer(kind=KI64) function choice_long(d, a) result(r)
    integer(kind=KI64),intent(in)          :: d
    integer(kind=KI64),intent(in),optional :: a
    if (present(a)) then
       r = a
    else
       r = d
    endif
    return
  end function choice_long

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

!!!_  & ndigits() - number of digits
  integer function ndigits_i(n) result(r)
    implicit none
    integer,intent(in) :: n
    integer b
    integer,parameter :: k8 = 10**8
    integer,parameter :: k4 = 10**4
    integer,parameter :: k2 = 10**2
    integer,parameter :: k1 = 10**1
    r = 1
    b = abs(n)
    do
       if (b.lt.k8) exit
       r = r + 8
       b = b / k8
    enddo
    if (b.ge.k4) then
       b = b / k4
       r = r + 4
    endif
    if (b.ge.k2) then
       b = b / k2
       r = r + 2
    endif
    if (b.ge.k1) then
       b = b / k1
       r = r + 1
    endif
    r = sign(r, n)
  end function ndigits_i
!!!_ + (system) control procedures
!!!_  - is_first_force () - check if first time or force
  logical function is_first_force(n, mode) result(b)
    implicit none
    integer,intent(in)          :: n
    integer,intent(in),optional :: mode
    b = (n.eq.0) .or. (IAND(choice(0, mode), MODE_FORCE).gt.0)
    return
  end function is_first_force
!!!_  - control_mode () - set init/diag/finalize mode
  integer function control_mode(mode, def) result(n)
    implicit none
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: def
    integer,parameter :: mskl = MODE_FORCE - 1
    n = choice(MODE_DEFAULT, mode)
    if (n.eq.MODE_DEFAULT) n = choice(MODE_DEEPEST, def)
    n = IAND(n, mskl)
    return
  end function control_mode
!!!_  - control_deep () - set init/diag/finalize mode at deep level
  integer function control_deep(mode) result(n)
    implicit none
    integer,intent(in) :: mode
    integer lmd
    integer,parameter :: mskd = MODE_BIT_DEEP - 1
    integer,parameter :: mskl = MODE_FORCE - 1
    integer,parameter :: mskh = NOT(mskl)
    lmd = IAND(mode, mskl)
    if (lmd.lt.MODE_DEEPEST) lmd = IAND(lmd, mskd)
    n = IOR(IAND(mode, mskh), lmd)
    return
  end function control_deep

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

  call test_ndigits(0)
  call test_ndigits(1)
  call test_ndigits(9)
  call test_ndigits(10)
  call test_ndigits(10**9)
  call test_ndigits(10**9-1)
  call test_ndigits(10**9+1)

  call finalize(ierr)

  stop
contains
  subroutine test_ndigits(n)
    implicit none
    integer,intent(in) :: n
    integer r
    r = ndigits(n)
102 format('ndigits: ', I0, ' = ', I0)
    write(*, 102) n, r
    return
  end subroutine test_ndigits
end program test_std_utl
#endif /* TEST_STD_UTL */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

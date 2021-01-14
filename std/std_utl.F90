!!!_! std_utl.F90 - touza/std utilities
! Maintainer: SAITO Fuyuki
! Created: Jun 4 2020
#define TIME_STAMP 'Time-stamp: <2021/01/13 09:21:39 fuyuki std_utl.F90>'
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

!!!_  - public
  public init, diag, finalize
  public choice, choice_a
  public set_if_present
  public condop
  public chcount
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    ierr = 0 * choice(0, u)
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    ierr = 0 * choice(0, u)
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    ierr = 0 * choice(0, u)
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
end module TOUZA_Std_utl
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

!!!_! std_utl.F90 - touza/std utilities
! Maintainer: SAITO Fuyuki
! Created: Jun 4 2020
#define TIME_STAMP 'Time-stamp: <2025/03/20 10:15:02 fuyuki std_utl.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2020-2025
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
#define _CHOICE_DECL  _ELEMENTAL
#define _CONDOP_DECL  _ELEMENTAL
#define _CONDREP_DECL _ELEMENTAL
!!!_@ TOUZA_Std_utl - small utilities
module TOUZA_Std_utl
  use TOUZA_Std_prc, only: KFLT, KDBL, KI64, KQPL
# define __MDL__ 'utl'
# define __TAG__ STD_FORMAT_MDL('utl')
# define _ERROR(E) (E - ERR_MASK_STD_UTL)
!!!_ = declaration
  implicit none
  private
!!!_  - paramters
  character(len=*),parameter :: separator_range  = '--'
  character(len=*),parameter :: separator_step   = '+'
  character(len=*),parameter :: separator_repeat = '*'
  character(len=*),parameter :: separator_item   = ' '
  character(len=*),parameter :: char_overflow  = '+'
  character(len=*),parameter :: char_underflow = '-'
!!!_  - interfaces
  interface choice
     module procedure choice_i,  choice_l,  choice_b,  choice_f,  choice_d
     module procedure choice_ia, choice_la, choice_ba, choice_fa, choice_da
  end interface choice

  interface set_if_present
     module procedure set_if_present_i, set_if_present_l
     module procedure set_if_present_f, set_if_present_d, set_if_present_a
  end interface set_if_present

  interface condop
     module procedure condop_i, condop_f, condop_d, condop_l
  end interface condop

  interface condrep
     module procedure condrep_i, condrep_f, condrep_d
  end interface condrep

  interface upcase
     module procedure upcase_m, upcase_o
  end interface upcase

  interface downcase
     module procedure downcase_m, downcase_o
  end interface downcase

  interface ndigits
     module procedure ndigits_i
  end interface ndigits

  interface compact_format
     module procedure compact_format_i
  end interface compact_format

  interface compact_string
     module procedure compact_string_f, compact_string_d
  end interface compact_string

  interface parse_number
     module procedure parse_number_i, parse_number_f, parse_number_d
  end interface parse_number

  interface join_list
     module procedure join_list_i, join_list_f, join_list_d, join_list_a
  end interface join_list
  interface join_item
     module procedure join_format_item_i, join_format_item_f, join_format_item_d
     module procedure join_mask_item_i, join_mask_item_f, join_mask_item_d
  end interface join_item
  interface join_format_item
     module procedure join_format_item_i, join_format_item_f, join_format_item_d
  end interface join_format_item

  interface split_list
     module procedure split_list_i, split_list_f, split_list_d, split_list_a
  end interface split_list

  interface find_first
     module procedure find_first_i, find_first_f, find_first_d, find_first_a
  end interface find_first

  interface find_first_range
     module procedure find_first_range_i
  end interface find_first_range

  interface inrange
     module procedure inrange_i, inrange_f, inrange_d
  end interface inrange

  interface swap_items
     module procedure swap_items_i, swap_items_f, swap_items_d
  end interface swap_items

  interface bisection_find
     module procedure bisection_find_i, bisection_find_f, bisection_find_d
  end interface bisection_find

  interface is_symbol
     module procedure is_symbol_set, is_symbol_def
  end interface is_symbol

#if OPT_REAL_QUADRUPLE_DIGITS > 0
  interface choice
     module procedure choice_q,  choice_qa
  end interface choice
  interface parse_number
     module procedure parse_number_q
  end interface parse_number
#endif
!!!_  - public
  public init, diag, finalize
  public choice, choice_a
  public set_if_present
  public condop, condrep
  public chcount
  public upcase, downcase, is_symbol
  public ndigits
  public control_mode, control_deep, is_first_force
  public control_lev
  public set_defu
  public parse_number
  public compact_format, compact_string
  public join_list,  split_list, split_heads
  public find_first, find_first_range
  public jot
  public inrange
  public begin_with, find_next_sep
  public swap_items
  public bisection_find
!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL
  integer,save :: err_default = _ERROR(ERR_NO_INIT)
  integer,save :: ulog = -1

  integer,save :: find_offset = 0   ! array start index in first_find family
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, offset)
    use TOUZA_Std_prc,only: prc_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer,intent(in),optional :: offset
    integer md, lv, lmd

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
          if (ierr.eq.0) call prc_init(ierr, ulog, levv=lv, mode=lmd)
       endif
       if (find_offset.ne.choice(find_offset, offset)) then
          ierr = ERR_NOT_IMPLEMENTED - ERR_MASK_STD_UTL
          write(*, *) 'cannot change offset to ', offset
          stop
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
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
101          format(__TAG__, A)
102          format(__TAG__, 'with elemental = ', I0)
103          format(__TAG__, 'find offset = ', I0)
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
             if (VCHECK_INFO(lv)) then
                if (utmp.ge.0) then
                   write(utmp, 103) find_offset
                else
                   write(*,    103) find_offset
                endif
             endif
          endif
       endif
       lmd = control_deep(md, mode)
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
       if (is_first_force(fine_counts, mode)) then
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
       lmd = control_deep(md, mode)
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
    implicit none
    integer,intent(in)          :: d
    integer,intent(in),optional :: a
    if (present(a)) then
       r = a
    else
       r = d
    endif
    return
  end function choice_i

  _CHOICE_DECL integer(kind=KTGT) function choice_l(d, a) result(r)
    use TOUZA_Std_prc,only: KTGT=>KI64
    implicit none
!!!_   . note ifort cannot compile with the following declaration
    ! integer,parameter :: KTGT=kind(r)
!!!_   . body
    integer(kind=KTGT),intent(in)          :: d
    integer(kind=KTGT),intent(in),optional :: a
    if (present(a)) then
       r = a
    else
       r = d
    endif
    return
  end function choice_l

  _CHOICE_DECL logical function choice_b(d, a) result(r)
    implicit none
    logical,intent(in)          :: d
    logical,intent(in),optional :: a
    if (present(a)) then
       r = a
    else
       r = d
    endif
    return
  end function choice_b

  _CHOICE_DECL real(kind=KTGT) function choice_f(d, a) result(r)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    real(kind=KTGT),intent(in)          :: d
    real(kind=KTGT),intent(in),optional :: a
    if (present(a)) then
       r = a
    else
       r = d
    endif
    return
  end function choice_f

  _CHOICE_DECL real(kind=KTGT) function choice_d(d, a) result(r)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in)          :: d
    real(kind=KTGT),intent(in),optional :: a
    if (present(a)) then
       r = a
    else
       r = d
    endif
    return
  end function choice_d
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  _CHOICE_DECL real(kind=KTGT) function choice_q(d, a) result(r)
    use TOUZA_Std_prc,only: KTGT=>KQPL
    implicit none
    real(kind=KTGT),intent(in)          :: d
    real(kind=KTGT),intent(in),optional :: a
    if (present(a)) then
       r = a
    else
       r = d
    endif
    return
  end function choice_q
#endif

  function choice_ia(d, a) result(r)
    implicit none
    integer,intent(in)           :: d(:)
    integer,intent(in),optional  :: a(:)
    integer,dimension(size(d,1)) :: r
    integer n
    if (present(a)) then
       n = min(size(d,1), size(a, 1))
       r(:n) = a(:n)
    else
       r(:) = d(:)
    endif
    return
  end function choice_ia

  function choice_la(d, a) result(r)
    use TOUZA_Std_prc,only: KTGT=>KI64
    implicit none
    integer(kind=KTGT),intent(in)           :: d(:)
    integer(kind=KTGT),intent(in),optional  :: a(:)
    integer(kind=KTGT),dimension(size(d,1)) :: r
    integer n
    if (present(a)) then
       n = min(size(d,1), size(a, 1))
       r(:n) = a(:n)
    else
       r(:) = d(:)
    endif
    return
  end function choice_la

  function choice_ba(d, a) result(r)
    implicit none
    logical,intent(in)           :: d(:)
    logical,intent(in),optional  :: a(:)
    logical,dimension(size(d,1)) :: r
    integer n
    if (present(a)) then
       n = min(size(d,1), size(a, 1))
       r(:n) = a(:n)
    else
       r(:) = d(:)
    endif
    return
  end function choice_ba

  function choice_fa(d, a) result(r)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    real(kind=KTGT),intent(in)           :: d(:)
    real(kind=KTGT),intent(in),optional  :: a(:)
    real(kind=KTGT),dimension(size(d,1)) :: r
    integer n
    if (present(a)) then
       n = min(size(d,1), size(a, 1))
       r(:n) = a(:n)
    else
       r(:) = d(:)
    endif
    return
  end function choice_fa

  function choice_da(d, a) result(r)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in)           :: d(:)
    real(kind=KTGT),intent(in),optional  :: a(:)
    real(kind=KTGT),dimension(size(d,1)) :: r
    integer n
    if (present(a)) then
       n = min(size(d,1), size(a, 1))
       r(:n) = a(:n)
    else
       r(:) = d(:)
    endif
    return
  end function choice_da
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  function choice_qa(d, a) result(r)
    use TOUZA_Std_prc,only: KTGT=>KQPL
    implicit none
    real(kind=KTGT),intent(in)           :: d(:)
    real(kind=KTGT),intent(in),optional  :: a(:)
    real(kind=KTGT),dimension(size(d,1)) :: r
    integer n
    if (present(a)) then
       n = min(size(d,1), size(a, 1))
       r(:n) = a(:n)
    else
       r(:) = d(:)
    endif
    return
  end function choice_qa
#endif

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
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    real(kind=KTGT),intent(out),optional :: var
    real(kind=KTGT),intent(in)           :: val
    if (present(var)) then
       var = val
    endif
    return
  end subroutine set_if_present_f

  subroutine set_if_present_d(var, val)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(out),optional :: var
    real(kind=KTGT),intent(in)           :: val
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

!!!_  & condrep() - conditional replace operator
  _CONDREP_DECL integer function condrep_i (v, src, rep) result(r)
    implicit none
    integer,intent(in) :: v
    integer,intent(in) :: src, rep
    if (v.eq.src) then
       r = rep
    else
       r = v
    endif
    return
  end function condrep_i
  _CONDREP_DECL real(kind=KTGT) function condrep_f (v, src, rep) result(r)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    real(kind=KTGT),intent(in) :: v
    real(kind=KTGT),intent(in) :: src, rep
    if (v.eq.src) then
       r = rep
    else
       r = v
    endif
    return
  end function condrep_f
  _CONDREP_DECL real(kind=KTGT) function condrep_d (v, src, rep) result(r)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: v
    real(kind=KTGT),intent(in) :: src, rep
    if (v.eq.src) then
       r = rep
    else
       r = v
    endif
    return
  end function condrep_d

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

  _CONDOP_DECL real(kind=KTGT) function condop_f (l, vt, vf) result(r)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    logical,        intent(in) :: l
    real(kind=KTGT),intent(in) :: vt, vf
    if (l) then
       r = vt
    else
       r = vf
    endif
    return
  end function condop_f

  _CONDOP_DECL real(kind=KTGT) function condop_d (l, vt, vf) result(r)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    logical,        intent(in) :: l
    real(kind=KTGT),intent(in) :: vt, vf
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

!!!_  & chcount() - count character(s) occurrence
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

!!!_  & is_symbol()
  logical function is_symbol_def (str) result(b)
    implicit none
    character(len=*),intent(in)          :: str
    integer,parameter :: la = IACHAR('a'), lz = IACHAR('z')
    integer,parameter :: ua = IACHAR('A'), uz = IACHAR('Z')
    integer,parameter :: d0 = IACHAR('0'), d9 = IACHAR('9')
    character(len=*),parameter :: nw = '_'
    character c
    integer j, jc
    do j = 1, len_trim(str)
       c = str(j:j)
       jc = IACHAR(c)
       if (la.le.jc.and.jc.le.lz) cycle
       if (ua.le.jc.and.jc.le.uz) cycle
       if (d0.le.jc.and.jc.le.d9) cycle
       if (INDEX(nw, c).gt.0) cycle
       b = .FALSE.
       return
    enddo
    b = .TRUE.
  end function is_symbol_def

  logical function is_symbol_set (str, set) result(b)
    implicit none
    character(len=*),intent(in) :: str
    character(len=*),intent(in) :: set
    integer,parameter :: la = IACHAR('a'), lz = IACHAR('z')
    integer,parameter :: ua = IACHAR('A'), uz = IACHAR('Z')
    integer,parameter :: d0 = IACHAR('0'), d9 = IACHAR('9')
    character c
    integer j, jc
    do j = 1, len_trim(str)
       c = str(j:j)
       jc = IACHAR(c)
       if (la.le.jc.and.jc.le.lz) cycle
       if (ua.le.jc.and.jc.le.uz) cycle
       if (d0.le.jc.and.jc.le.d9) cycle
       if (INDEX(set, c).gt.0) cycle
       b = .FALSE.
       return
    enddo
    b = .TRUE.
  end function is_symbol_set

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

!!!_  & compact_format
  subroutine compact_format_i(npos, str, v, nrep, pad, fmt, sep, clipl, cliph)
    implicit none
    integer,         intent(out)         :: npos   ! next position
    character(len=*),intent(out)         :: str
    integer,         intent(in)          :: v(0:)
    integer,         intent(in),optional :: nrep
    integer,         intent(in),optional :: pad
    character(len=*),intent(in),optional :: fmt
    character(len=*),intent(in),optional :: sep
    integer,         intent(in),optional :: clipl, cliph

    integer stt
    integer,parameter :: stt_wait = 0, stt_set = 1, stt_rep = 2, stt_range = 3

    integer jstr, lstr
    integer j, j0, n
    integer m, ref, d
    integer minrep
    integer ch, cl

    character(len=64) :: fmt_rep
    character(len=64) :: fmt_range
    character(len=64) :: fmt_range_step
    character(len=64) :: fmt_item

    character(len=128) :: buf
    character(len=128) :: bsep
    character(len=64)  :: ovf,  udf
    character(len=64)  :: buf0, buf1
    integer lb, lsep, lo, lu

    integer jerr

    jerr = 0

    npos = 0
    str = ' '
    ch = choice(+ HUGE(KIND(ch)),     cliph)
    cl = choice(- HUGE(KIND(cl)) - 1, clipl)

    call compact_format_gen_i(jerr, fmt_item, udf, ovf, fmt, pad)
    lo = len_trim(ovf)
    lu = len_trim(udf)

121 format('(I0,''', A, ''',A)')
122 format('(A, ''', A, ''',A)')
123 format('(A, ''', A, ''',A,''', A, ''', I0)')

    if (jerr.eq.0) write(fmt_rep,        121, IOSTAT=jerr) trim(separator_repeat)
    if (jerr.eq.0) write(fmt_range,      122, IOSTAT=jerr) trim(separator_range)
    if (jerr.eq.0) write(fmt_range_step, 123, IOSTAT=jerr) trim(separator_range), trim(separator_step)
    if (jerr.ne.0) then
       npos = _ERROR(ERR_PANIC)
       return
    endif

    lstr = len(str)
    if (present(sep)) then
       bsep = sep
    else
       bsep = separator_item
    endif
    lsep = max(1, len_trim(bsep))
    n = size(v, 1)

    minrep = max(2, choice(0, nrep))

    stt = stt_wait
    j = 0
    jstr = 0
    do
       if (j.ge.n) exit
       j0 = j
       ref = v(j0)
       j = j0 + 1
       if (j.lt.n) then
          ! write(*, *) 'compact/loop', j0, j, ref, v(j)
          m = 0
          if (ref.lt.cl) then
             d = 0
             do
                m = m + 1
                if (j.eq.n) exit
                if (v(j).ge.cl) exit
                j = j + 1
             enddo
          else if (ref.gt.ch) then
             d = 0
             do
                m = m + 1
                if (j.eq.n) exit
                if (v(j).le.ch) exit
                j = j + 1
             enddo
          else
             d = v(j) - ref
             do
                m = m + 1
                if (j.eq.n) exit
                if (v(j).gt.ch.or.v(j).lt.cl) exit
                if (v(j) - v(j-1).ne.d) exit
                j = j + 1
             enddo
          endif
       else
          m = 0
          d = 0
       endif
       if (m.gt.minrep) then
          call compact_format_item_i(buf0, ref,    fmt_item, udf(1:lu), ovf(1:lo), cl, ch)
          call compact_format_item_i(buf1, v(j-1), fmt_item, udf(1:lu), ovf(1:lo), cl, ch)
          if (d.eq.0) then
             write(buf, fmt_rep, IOSTAT=jerr) m, trim(buf0)
          else if (d.eq.+1.or.d.eq.-1) then
             write(buf, fmt_range, IOSTAT=jerr) trim(buf0), trim(buf1)
          else
             write(buf, fmt_range_step, IOSTAT=jerr) trim(buf0), trim(buf1), abs(d)
          endif
          if (jerr.ne.0) then
             npos = _ERROR(ERR_PANIC)
             return
          endif
       else
          call compact_format_item_i(buf, v(j0), fmt_item, udf(1:lu), ovf(1:lo), cl, ch)
          j = j0 + 1
       endif
       lb = len_trim(buf)
       if (jstr.eq.0) then
          if (lb.gt.lstr) then
             npos = -1
             exit
          endif
          str = trim(buf)
          jstr = lb
       else if (jstr + lb + lsep.gt.lstr) then
          npos = j0
          exit
       else
          str = trim(str) // bsep(1:lsep) // buf(1:lb)
          jstr = jstr + lsep + lb
       endif
    enddo
    return
  end subroutine compact_format_i
!!!_   . compact_format_gen
  subroutine compact_format_gen_i &
       & (ierr, fmt, ufc, ofc, single, pad)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(out)         :: fmt
    character(len=*),intent(out)         :: ufc, ofc
    character(len=*),intent(in),optional :: single
    integer,         intent(in),optional :: pad
    integer p

    ierr = 0
    ufc = ' '
    ofc = ' '
    if (present(single)) then
       fmt = single
    else
       p = choice(0, pad)
111    format('I', I0, '.', I0)
112    format('SP,I', I0, '.', I0, ',SS')
       if (p.eq.0) then
          fmt = 'I0'
       else if (p.lt.0) then
          write(fmt, 112, IOSTAT=ierr) 1 - p, -p
       else
          write(fmt, 111, IOSTAT=ierr) p, p
       endif
    endif
    if (ierr.eq.0) then
       fmt = '(' // trim(fmt) // ')'
       write(ofc, fmt=fmt, IOSTAT=ierr) 0
    endif
    if (ierr.eq.0) then
       p = len_trim(ofc)
       ofc = repeat(char_overflow, p)
       ufc = repeat(char_underflow, p)
    endif
    if (ierr.ne.0) ierr = _ERROR(ERR_PANIC)
    return
  end subroutine compact_format_gen_i
!!!_   . compact_format_item
  subroutine compact_format_item_i &
       & (str, num, fmt, ufc, ofc, low, high)
    implicit none
    character(len=*),intent(out) :: str
    integer,         intent(in)  :: num
    character(len=*),intent(in)  :: fmt
    character(len=*),intent(in)  :: ufc,  ofc
    integer,         intent(in)  :: low,  high
    integer jerr
    if (num.lt.low) then
       str = ufc
    else if (num.gt.high) then
       str = ofc
    else
       write(str, fmt, IOSTAT=jerr) num
    endif
  end subroutine compact_format_item_i

!!!_  & compact_string
  subroutine compact_string_f &
       & (ierr, str, v, fmt, ldelim, rdelim, append, mag, tol, decp)
    implicit none
    integer,parameter :: KTGT=KFLT
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: str
    real(kind=KTGT), intent(in)          :: v
    character(len=*),intent(in),optional :: fmt
    character(len=*),intent(in),optional :: ldelim, rdelim
    logical,         intent(in),optional :: append
    logical,         intent(in),optional :: decp    ! leave decimal point (.FALSE.)
    integer,         intent(in),optional :: mag
    real(kind=KTGT), intent(in),optional :: tol
    character(len=128) :: buf, bfmt
    integer nd, nr, m
    integer lt
    integer jpp, jpx, jpe, jpt

    ierr = 0
    if (present(fmt)) then
       if (fmt.eq.'*') then
          write(buf, *) v
       else
          write(buf, fmt) v
       endif
    else
       nd = PRECISION(v) + 1
       m = choice(nd, mag)
       ! if (AINT(v).eq.v.and.abs(v).lt.10.0_KTGT**m) then
       if (abs(v).lt.10.0_KTGT**m.and.abs(v).ge.1.0_KTGT) then
102       format('(F0.', I0, ')')
          write(bfmt, 102) nd
       else
          nr = ndigits(RANGE(v) / 2)
101       format('(ES', I0, '.', I0, 'E', I0, ')')
          write(bfmt, 101) nd + nr + 5, nd, nr
       endif
       write(buf, trim(bfmt)) v
    endif
    buf = adjustl(buf)
    lt = LEN_TRIM(buf)
    jpe = SCAN(buf(1:lt), 'Ee')
    ! write(*, *) '0:', jpe, trim(buf)
    if (jpe.gt.0) then
       jpx = jpe + 1
       if (VERIFY(buf(jpx:jpx), '+-').gt.0) jpx = jpx + 1
       jpt = VERIFY(buf(jpx+1:lt), '0')
       ! write(*, *) jpt, buf(jpx+1:lt)
       if (jpt.eq.0) then
          buf(jpe:lt) = ' '
       elseif (jpt.gt.0) then
          buf(jpx+1:lt) = buf(jpx+jpt:lt)
       endif
    else
       jpe = lt + 1
    endif
    jpp = INDEX(buf(1:lt), '.')
    ! write(*, *) '1:', jpp, trim(buf)
    ! write(*, *) '2:', jpe, buf(:jpe-1)
    if (jpp.gt.0) then
       jpt = VERIFY(buf(:jpe-1), '0', BACK=.TRUE.)
       if (jpt.ge.jpp) buf(jpt+1:lt) = buf(jpe:lt)
       if (choice(.FALSE., decp)) then
       else
          if (SCAN(buf(jpp+1:jpp+1), ' Ee').gt.0) buf(jpp:lt) = buf(jpp+1:lt)
       endif
    endif
    if (present(ldelim)) then
       buf = ldelim // trim(buf)
    endif
    if (present(rdelim)) then
       buf = trim(buf) // rdelim
    endif
    if (choice(.FALSE., append)) then
       str = trim(str) // trim(buf)
    else
       str = trim(buf)
    endif
  end subroutine compact_string_f
  subroutine compact_string_d &
       & (ierr, str, v, fmt, ldelim, rdelim, append, mag, tol, decp)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: str
    real(kind=KTGT), intent(in)          :: v
    character(len=*),intent(in),optional :: fmt
    character(len=*),intent(in),optional :: ldelim, rdelim
    logical,         intent(in),optional :: append
    logical,         intent(in),optional :: decp    ! leave decimal point (.FALSE.)
    integer,         intent(in),optional :: mag
    real(kind=KTGT), intent(in),optional :: tol
    character(len=128) :: buf, bfmt
    integer nd, nr, m
    integer lt
    integer jpp, jpx, jpe, jpt

    ierr = 0
    if (present(fmt)) then
       if (fmt.eq.'*') then
          write(buf, *) v
       else
          write(buf, fmt) v
       endif
    else
       nd = PRECISION(v) + 1
       m = choice(nd, mag)
       ! if (AINT(v).eq.v.and.abs(v).lt.10.0_KTGT**m) then
       if (abs(v).lt.10.0_KTGT**m.and.abs(v).ge.1.0_KTGT) then
102       format('(F0.', I0, ')')
          write(bfmt, 102) nd
       else
          nr = ndigits(RANGE(v) / 2)
101       format('(ES', I0, '.', I0, 'E', I0, ')')
          write(bfmt, 101) nd + nr + 5, nd, nr
       endif
       write(buf, trim(bfmt)) v
    endif
    buf = adjustl(buf)
    lt = LEN_TRIM(buf)
    jpe = SCAN(buf(1:lt), 'Ee')
    ! write(*, *) '0:', jpe, trim(buf)
    if (jpe.gt.0) then
       jpx = jpe + 1
       if (VERIFY(buf(jpx:jpx), '+-').gt.0) jpx = jpx + 1
       jpt = VERIFY(buf(jpx+1:lt), '0')
       ! write(*, *) jpt, buf(jpx+1:lt)
       if (jpt.eq.0) then
          buf(jpe:lt) = ' '
       elseif (jpt.gt.0) then
          buf(jpx+1:lt) = buf(jpx+jpt:lt)
       endif
    else
       jpe = lt + 1
    endif
    jpp = INDEX(buf(1:lt), '.')
    ! write(*, *) '1:', jpp, trim(buf)
    ! write(*, *) '2:', jpe, buf(:jpe-1)
    if (jpp.gt.0) then
       jpt = VERIFY(buf(:jpe-1), '0', BACK=.TRUE.)
       if (jpt.ge.jpp) buf(jpt+1:lt) = buf(jpe:lt)
       if (choice(.FALSE., decp)) then
       else
          if (SCAN(buf(jpp+1:jpp+1), ' Ee').gt.0) buf(jpp:lt) = buf(jpp+1:lt)
       endif
    endif
    if (present(ldelim)) then
       buf = ldelim // trim(buf)
    endif
    if (present(rdelim)) then
       buf = trim(buf) // rdelim
    endif
    if (choice(.FALSE., append)) then
       str = trim(str) // trim(buf)
    else
       str = trim(buf)
    endif
  end subroutine compact_string_d
!!!_  & parse_number - safely parse number from string
!!!_   . Note
  !!     It seems surprizing that read statement with fmt=* is actually
  !!     list-directed formatting, which means that the separator for input
  !!     is not only blank but also slash, comma and asterisk, according to
  !!     Fortran standard.
  !!     For this reason maybe, as far as I tried with GCC, a string starting
  !!     with '/' to parse integer does not through an error.
  !!     The following procedures are workarounds to avoid such behaviour.
  !!     Run test program and check the result of parse_number lines
  ! !!!                  read        workaround
  ! parse_number[123]  = (123 0)     (123 0)
  ! parse_number[12,3] = (12 0)      (-999 3)
  ! parse_number[12/3] = (12 0)      (-999 3)
  ! parse_number[12*3] = (3 0)       (-999 3)    !! not 12 but 12 times 3
  ! parse_number[/123] = (-999 0)    (-999 1)    !! null + end of list
  ! parse_number[*123] = (-999 5010) (-999 1)
  ! parse_number[,123] = (-999 0)    (-999 1)
  subroutine parse_number_i (ierr, num, str, def)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(inout)       :: num
    character(len=*),intent(in)          :: str
    integer,         intent(in),optional :: def
    integer buf
    if (str.eq.' ') then
       if (present(def)) then
          ierr = 0
          num = def
       else
          ierr = _ERROR(ERR_INVALID_ITEM)
       endif
       return
    endif
    ierr = check_number_string(str)
    if (ierr.eq.0) read(str, *, IOSTAT=ierr) buf
    if (ierr.ne.0) then
       ierr = _ERROR(ERR_STRING_IO)
    else
       num = buf
    endif
  end subroutine parse_number_i
  subroutine parse_number_f (ierr, num, str, def)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KTGT), intent(inout)       :: num
    character(len=*),intent(in)          :: str
    real(kind=KTGT), intent(in),optional :: def
    real(kind=KTGT) buf
    if (str.eq.' ') then
       if (present(def)) then
          ierr = 0
          num = def
       else
          ierr = _ERROR(ERR_INVALID_ITEM)
       endif
       return
    endif
    ierr = check_number_string(str)
    if (ierr.eq.0) read(str, *, IOSTAT=ierr) buf
    if (ierr.ne.0) then
       ierr = _ERROR(ERR_STRING_IO)
    else
       num = buf
    endif
  end subroutine parse_number_f
  subroutine parse_number_d (ierr, num, str, def)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KTGT), intent(inout)       :: num
    character(len=*),intent(in)          :: str
    real(kind=KTGT), intent(in),optional :: def
    real(kind=KTGT) buf
    if (str.eq.' ') then
       if (present(def)) then
          ierr = 0
          num = def
       else
          ierr = _ERROR(ERR_INVALID_ITEM)
       endif
       return
    endif
    ierr = check_number_string(str)
    if (ierr.eq.0) read(str, *, IOSTAT=ierr) buf
    if (ierr.ne.0) then
       ierr = _ERROR(ERR_STRING_IO)
    else
       num = buf
    endif
  end subroutine parse_number_d
#if OPT_REAL_QUADRUPLE_DIGITS > 0
  subroutine parse_number_q (ierr, num, str, def)
    use TOUZA_Std_prc,only: KTGT=>KQPL
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KTGT), intent(inout)       :: num
    character(len=*),intent(in)          :: str
    real(kind=KTGT), intent(in),optional :: def
    real(kind=KTGT) buf
    if (str.eq.' ') then
       if (present(def)) then
          ierr = 0
          num = def
       else
          ierr = _ERROR(ERR_INVALID_ITEM)
       endif
       return
    endif
    ierr = check_number_string(str)
    if (ierr.eq.0) read(str, *, IOSTAT=ierr) buf
    if (ierr.ne.0) then
       ierr = _ERROR(ERR_STRING_IO)
    else
       num = buf
    endif
  end subroutine parse_number_q
#endif
!!!_    & check_number_string()
  integer function check_number_string(str) result (n)
    implicit none
    character(len=*),intent(in)  :: str
    n = scan(trim(str), '/,*')
  end function check_number_string

!!!_  & join_list - convert array to string
  subroutine join_list_i &
       & (ierr, str, v, fmt, sep, ldelim, rdelim, mask, skip)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(out)         :: str
    integer,         intent(in)          :: v(0:)
    character(len=*),intent(in),optional :: fmt
    character(len=*),intent(in),optional :: sep, ldelim, rdelim
    logical,         intent(in),optional :: mask(0:)
    character(len=*),intent(in),optional :: skip

    integer lstr, jstr
    integer jv, nv
    integer nb
    character(len=64) :: buf
    character(len=64) :: xfmt
    character(len=64) :: xsep
    character(len=64) :: cskp
    integer              lsep

    ierr = 0
    str = ' '

    nv = size(v)
    if (nv.le.0) return

    call join_init_sep(xsep, lsep, sep)
    call choice_a(xfmt, '(I0)', fmt)

    jstr = 0
    lstr = len(str)

    if (present(mask)) then
       call choice_a(cskp, '_', skip)
       jv = 0
       call join_item(nb, buf, v(jv), xfmt, mask(jv), cskp)
       ierr = min(0, nb)
       if (ierr.eq.0) then
          jstr = jstr + nb
          str = buf(1:nb)
          do jv = 1, nv - 1
             call join_item(nb, buf, v(jv), xfmt, mask(jv), cskp)
             ierr = min(0, nb)
             if (ierr.eq.0) then
                jstr = jstr + nb + lsep
                if (jstr.gt.lstr) then
                   ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
                else
                   str = trim(str) // xsep(1:lsep) // buf(1:nb)
                endif
             endif
             if (ierr.ne.0) exit
          enddo
       endif
    else
       jv = 0
       call join_item(nb, buf, v(jv), xfmt)
       ierr = min(0, nb)
       if (ierr.eq.0) then
          jstr = jstr + nb
          str = buf(1:nb)
          do jv = 1, nv - 1
             call join_item(nb, buf, v(jv), xfmt)
             ierr = min(0, nb)
             if (ierr.eq.0) then
                jstr = jstr + nb + lsep
                if (jstr.gt.lstr) then
                   ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
                else
                   str = trim(str) // xsep(1:lsep) // buf(1:nb)
                endif
             endif
             if (ierr.ne.0) exit
          enddo
       endif
    endif
    if (ierr.eq.0) call add_ldelim(ierr, jstr, str, ldelim)
    if (ierr.eq.0) call add_rdelim(ierr, jstr, str, rdelim)
  end subroutine join_list_i

  subroutine join_list_f &
       & (ierr, str, v, fmt, sep, ldelim, rdelim, mask, skip)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(out)         :: str
    real(kind=KTGT), intent(in)          :: v(0:)
    character(len=*),intent(in),optional :: fmt
    character(len=*),intent(in),optional :: sep, ldelim, rdelim
    logical,         intent(in),optional :: mask(0:)
    character(len=*),intent(in),optional :: skip

    integer lstr, jstr
    integer jv, nv
    integer nb
    character(len=64) :: buf
    character(len=64) :: xfmt
    character(len=64) :: xsep
    character(len=64) :: cskp
    integer              lsep

    ierr = 0
    str = ' '

    nv = size(v)
    if (nv.le.0) return

    call join_init_sep(xsep, lsep, sep)
    call choice_a(xfmt, '*', fmt)

    jstr = 0
    lstr = len(str)

    if (present(mask)) then
       call choice_a(cskp, '_', skip)
       jv = 0
       call join_item(nb, buf, v(jv), xfmt, mask(jv), cskp)
       ierr = min(0, nb)
       if (ierr.eq.0) then
          jstr = jstr + nb
          str = buf(1:nb)
          do jv = 1, nv - 1
             call join_item(nb, buf, v(jv), xfmt, mask(jv), cskp)
             ierr = min(0, nb)
             if (ierr.eq.0) then
                jstr = jstr + nb + lsep
                if (jstr.gt.lstr) then
                   ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
                else
                   str = trim(str) // xsep(1:lsep) // buf(1:nb)
                endif
             endif
             if (ierr.ne.0) exit
          enddo
       endif
    else
       jv = 0
       call join_item(nb, buf, v(jv), xfmt)
       ierr = min(0, nb)
       if (ierr.eq.0) then
          jstr = jstr + nb
          str = buf(1:nb)
          do jv = 1, nv - 1
             call join_item(nb, buf, v(jv), xfmt)
             ierr = min(0, nb)
             if (ierr.eq.0) then
                jstr = jstr + nb + lsep
                if (jstr.gt.lstr) then
                   ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
                else
                   str = trim(str) // xsep(1:lsep) // buf(1:nb)
                endif
             endif
             if (ierr.ne.0) exit
          enddo
       endif
    endif
    if (ierr.eq.0) call add_ldelim(ierr, jstr, str, ldelim)
    if (ierr.eq.0) call add_rdelim(ierr, jstr, str, rdelim)
  end subroutine join_list_f

  subroutine join_list_d &
       & (ierr, str, v, fmt, sep, ldelim, rdelim, mask, skip)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(out)         :: str
    real(kind=KTGT), intent(in)          :: v(0:)
    character(len=*),intent(in),optional :: fmt
    character(len=*),intent(in),optional :: sep, ldelim, rdelim
    logical,         intent(in),optional :: mask(0:)
    character(len=*),intent(in),optional :: skip

    integer lstr, jstr
    integer jv, nv
    integer nb
    character(len=64) :: buf
    character(len=64) :: xfmt
    character(len=64) :: xsep
    character(len=64) :: cskp
    integer              lsep

    ierr = 0
    str = ' '

    nv = size(v)
    if (nv.le.0) return

    call join_init_sep(xsep, lsep, sep)
    call choice_a(xfmt, '*', fmt)

    jstr = 0
    lstr = len(str)

    if (present(mask)) then
       call choice_a(cskp, '_', skip)
       jv = 0
       call join_item(nb, buf, v(jv), xfmt, mask(jv), cskp)
       ierr = min(0, nb)
       if (ierr.eq.0) then
          jstr = jstr + nb
          str = buf(1:nb)
          do jv = 1, nv - 1
             call join_item(nb, buf, v(jv), xfmt, mask(jv), cskp)
             ierr = min(0, nb)
             if (ierr.eq.0) then
                jstr = jstr + nb + lsep
                if (jstr.gt.lstr) then
                   ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
                else
                   str = trim(str) // xsep(1:lsep) // buf(1:nb)
                endif
             endif
             if (ierr.ne.0) exit
          enddo
       endif
    else
       jv = 0
       call join_item(nb, buf, v(jv), xfmt)
       ierr = min(0, nb)
       if (ierr.eq.0) then
          jstr = jstr + nb
          str = buf(1:nb)
          do jv = 1, nv - 1
             call join_item(nb, buf, v(jv), xfmt)
             ierr = min(0, nb)
             if (ierr.eq.0) then
                jstr = jstr + nb + lsep
                if (jstr.gt.lstr) then
                   ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
                else
                   str = trim(str) // xsep(1:lsep) // buf(1:nb)
                endif
             endif
             if (ierr.ne.0) exit
          enddo
       endif
    endif
    if (ierr.eq.0) call add_ldelim(ierr, jstr, str, ldelim)
    if (ierr.eq.0) call add_rdelim(ierr, jstr, str, rdelim)
  end subroutine join_list_d

  subroutine join_list_a &
       & (ierr, str, v, fmt, sep, ldelim, rdelim)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(out)         :: str
    character(len=*),intent(in)          :: v(0:)
    character(len=*),intent(in),optional :: fmt
    character(len=*),intent(in),optional :: sep, ldelim, rdelim

    integer lstr, jstr
    integer jv, nv
    integer nb
    character(len=64) :: buf
    character(len=64) :: xfmt
    character(len=64) :: xsep
    integer              lsep

    ierr = 0
    str = ' '

    nv = size(v)
    if (nv.le.0) return

    call join_init_sep(xsep, lsep, sep)
    call choice_a(xfmt, '(A)', fmt)

    jstr = 0
    lstr = len(str)

    jv = 0
    write(buf, xfmt, IOSTAT=ierr) trim(v(jv))
    nb = len_trim(buf)
    jstr = jstr + nb
    str = buf(1:nb)

    do jv = 1, nv - 1
       write(buf, xfmt, IOSTAT=ierr) trim(v(jv))
       nb = len_trim(buf)
       jstr = jstr + nb + lsep
       if (ierr.eq.0) then
          if (jstr.gt.lstr) then
             ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          else
             str = trim(str) // xsep(1:lsep) // buf(1:nb)
          endif
       endif
    enddo
    if (ierr.eq.0) call add_ldelim(ierr, jstr, str, ldelim)
    if (ierr.eq.0) call add_rdelim(ierr, jstr, str, rdelim)
  end subroutine join_list_a

!!!_   . init_join
  subroutine join_init_sep(sep, lsep, arg)
    implicit none
    character(len=*),intent(out)         :: sep
    integer,         intent(out)         :: lsep
    character(len=*),intent(in),optional :: arg
    call choice_a(sep, ' ', arg)
    if (sep(1:1).eq.CHAR(0)) then
       lsep = 0
    else
       lsep = max(1, len_trim(sep))
    endif
  end subroutine join_init_sep
!!!_   . join_mask_item
  subroutine join_mask_item_d &
       & (n, str, v, fmt, mask, skip)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    integer,         intent(out) :: n
    character(len=*),intent(out) :: str
    real(kind=KTGT), intent(in)  :: v
    character(len=*),intent(in)  :: fmt
    logical,         intent(in)  :: mask
    character(len=*),intent(in)  :: skip

    if (mask) then
       str = skip
       n = len_trim(skip)
    else
       call join_format_item(n, str, v, fmt)
    endif
    return
  end subroutine join_mask_item_d
  subroutine join_mask_item_f &
       & (n, str, v, fmt, mask, skip)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    integer,         intent(out) :: n
    character(len=*),intent(out) :: str
    real(kind=KTGT), intent(in)  :: v
    character(len=*),intent(in)  :: fmt
    logical,         intent(in)  :: mask
    character(len=*),intent(in)  :: skip

    if (mask) then
       str = skip
       n = len_trim(skip)
    else
       call join_format_item(n, str, v, fmt)
    endif
    return
  end subroutine join_mask_item_f
  subroutine join_mask_item_i &
       & (n, str, v, fmt, mask, skip)
    implicit none
    integer,         intent(out) :: n
    character(len=*),intent(out) :: str
    integer,         intent(in)  :: v
    character(len=*),intent(in)  :: fmt
    logical,         intent(in)  :: mask
    character(len=*),intent(in)  :: skip

    if (mask) then
       str = skip
       n = len_trim(skip)
    else
       call join_format_item(n, str, v, fmt)
    endif
    return
  end subroutine join_mask_item_i

!!!_   . join_format_item
  subroutine join_format_item_d &
       & (n, str, v, fmt)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    integer,         intent(out) :: n
    character(len=*),intent(out) :: str
    real(kind=KTGT), intent(in)  :: v
    character(len=*),intent(in)  :: fmt
    integer jerr

    if (fmt.eq.'*') then
       write(str, *, IOSTAT=jerr) v
    else if (fmt.eq.' ') then
       call compact_string(jerr, str, v)
    else
       write(str, fmt, IOSTAT=jerr) v
    endif
    if (jerr.eq.0) then
       str = adjustl(str)
       n = len_trim(str)
    else
       n = _ERROR(ERR_PANIC)
    endif
    return
  end subroutine join_format_item_d
  subroutine join_format_item_f &
       & (n, str, v, fmt)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    integer,         intent(out) :: n
    character(len=*),intent(out) :: str
    real(kind=KTGT), intent(in)  :: v
    character(len=*),intent(in)  :: fmt
    integer jerr

    if (fmt.eq.'*') then
       write(str, *, IOSTAT=jerr) v
    else if (fmt.eq.' ') then
       call compact_string(jerr, str, v)
    else
       write(str, fmt, IOSTAT=jerr) v
    endif
    if (jerr.eq.0) then
       str = adjustl(str)
       n = len_trim(str)
    else
       n = _ERROR(ERR_PANIC)
    endif
    return
  end subroutine join_format_item_f
  subroutine join_format_item_i &
       & (n, str, v, fmt)
    implicit none
    integer,         intent(out) :: n
    character(len=*),intent(out) :: str
    integer,         intent(in)  :: v
    character(len=*),intent(in)  :: fmt
    integer jerr

    if (fmt.eq.'*') then
       write(str, *, IOSTAT=jerr) v
    else if (fmt.eq.' ') then
       write(str, '(I0)', IOSTAT=jerr) v
    else
       write(str, fmt, IOSTAT=jerr) v
    endif
    if (jerr.eq.0) then
       str = adjustl(str)
       n = len_trim(str)
    else
       n = _ERROR(ERR_PANIC)
    endif
    return
  end subroutine join_format_item_i

!!!_   . add_ldelim
  subroutine add_ldelim(ierr, jpos, str, delim)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(inout)       :: jpos
    character(len=*),intent(inout)       :: str
    character(len=*),intent(in),optional :: delim
    integer lstr
    integer ns

    ierr = 0
    if (present(delim)) then
       lstr = len(str)
       ns = max(1, len_trim(delim))
       jpos = jpos + ns
       if (jpos.gt.lstr) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       else
          str = delim(1:ns) // trim(str)
       endif
    endif
  end subroutine add_ldelim
!!!_   . add_rdelim
  subroutine add_rdelim(ierr, jpos, str, delim)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(inout)       :: jpos
    character(len=*),intent(inout)       :: str
    character(len=*),intent(in),optional :: delim
    integer lstr
    integer ns

    ierr = 0
    if (present(delim)) then
       lstr = len(str)
       ns = max(1, len_trim(delim))
       jpos = jpos + ns
       if (jpos.gt.lstr) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       else
          str = trim(str) // delim(1:ns)
       endif
    endif
  end subroutine add_rdelim

!!!_  & split_heads - split string and return head-array
  subroutine split_heads &
       & (n, h, str, sep, lim, empty)
    implicit none
    integer,          intent(out)         :: n         ! number of elements or error code
    integer,          intent(out)         :: h(0:*)
    character(len=*), intent(in)          :: str
    character(len=*), intent(in)          :: sep
    integer,          intent(in),optional :: lim       ! negative to count only; 0 to infinite
    logical,          intent(in),optional :: empty     ! allow empty element (ignored if def present)

    !  return head positions of items
    !    item: str(h(i)+1:h(i+1)-len(sep))
    !  h    h    h    h
    !  [aaa//bbb//ccc//]

    integer jpos, lstr, lsep
    integer js,   jh
    integer nlim
    integer jerr
    logical eallow

    jerr = 0
    nlim = choice(0, lim)
    if (nlim.eq.0) nlim = +HUGE(0)
    eallow = choice(.TRUE., empty)
    n = 0
    jpos = 0
    lstr = len_trim(str)
    if (lstr.eq.0) return
    lsep = max(1, len_trim(sep))   ! allow single blank only

    if (eallow) then
       jpos = 0
    else
       do
          if (jpos+lsep.gt.lstr) exit
          if (str(jpos+1:jpos+lsep).ne.sep(1:lsep)) exit
          jpos = jpos + lsep
       enddo
    endif
    if (nlim.ge.0) then
       if (n.gt.nlim) then
          jerr = -1
       else
          h(n) = jpos
       endif
    endif
    n = n + 1
    do
       js = index(str(jpos+1:lstr), sep(1:lsep))
       if (js.gt.0) then
          jh = jpos + js - 1
       else
          jh = lstr
       endif
       if (jpos.lt.jh.or.eallow) then
          if (nlim.ge.0) then
             if (n.gt.nlim) then
                jerr = -1
             else
                h(n) = jh + lsep
             endif
          endif
          n = n + 1
       endif
       jpos = jh + lsep
       if (js.eq.0) exit
       if (jerr.ne.0) exit
    enddo
    if (jerr.ne.0) then
       n = _ERROR(ERR_INVALID_PARAMETER)
    else
       n = n - 1
    endif
  end subroutine split_heads

!!!_  & split_list - convert array to string
  subroutine split_list_i &
       & (n, v, str, sep, lim, def, empty)
    implicit none
    integer,          intent(out)         :: n         ! number of elements or error code
    integer,          intent(inout)       :: v(0:)
    character(len=*), intent(in)          :: str
    character(len=*), intent(in)          :: sep
    integer,          intent(in),optional :: lim       ! negative to count only; 0 to infinite
    integer,          intent(in),optional :: def(0:*)  ! no bound check
    logical,          intent(in),optional :: empty     ! allow empty element (ignored if def present)

    integer jpos, lstr, lsep
    integer js,   jh
    integer nlim
    integer jerr
    logical eallow

    jerr = 0
    nlim = choice(0, lim)
    if (nlim.eq.0) nlim = +HUGE(0)
    eallow = choice(.TRUE., empty)

    n = 0
    jpos = 0
    lstr = len_trim(str)
    if (lstr.eq.0) return
    lsep = max(1, len_trim(sep))   ! allow single blank only
    js = 0
    do
       js = index(str(jpos+1:lstr), sep(1:lsep))
       if (js.eq.0) then
          jh = lstr
       else
          jh = jpos + js - 1
       endif
       ! write(*, *) '  split', n, js, jpos, jh, str(jpos+1:lstr)
       if (jpos.lt.jh) then
          if (nlim.ge.0) then
             if (n.ge.nlim) then
                jerr = _ERROR(ERR_OUT_OF_RANGE)
             else
                call parse_number(jerr, v(n), str(jpos+1:jh))
             endif
          endif
          n = n + 1
       else if (present(def)) then
          if (nlim.ge.0) then
             if (n.ge.nlim) then
                jerr = _ERROR(ERR_OUT_OF_RANGE)
             else
                v(n) = def(n)
             endif
          endif
          n = n + 1
       else if (eallow) then
          if (nlim.ge.0.and.n.ge.nlim) jerr = _ERROR(ERR_OUT_OF_RANGE)
          n = n + 1
       endif
       jpos = jh + lsep
       if (js.eq.0) exit
       if (jerr.ne.0) exit
    enddo
    if (jerr.lt.0) n = jerr
    return
  end subroutine split_list_i

  subroutine split_list_f &
       & (n, v, str, sep, lim, def, empty)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    integer,          intent(out)         :: n         ! number of elements or error code
    real(kind=KTGT),  intent(inout)       :: v(0:)
    character(len=*), intent(in)          :: str
    character(len=*), intent(in)          :: sep
    integer,          intent(in),optional :: lim       ! negative to count only; 0 to infinite
    real(kind=KTGT),  intent(in),optional :: def(0:*)  ! no bound check
    logical,          intent(in),optional :: empty     ! allow empty element (ignored if def present)

    integer jpos, lstr, lsep
    integer js,   jh
    integer nlim
    integer jerr
    logical eallow

    jerr = 0
    nlim = choice(0, lim)
    if (nlim.eq.0) nlim = +HUGE(0)
    eallow = choice(.TRUE., empty)

    n = 0
    jpos = 0
    lstr = len_trim(str)
    if (lstr.eq.0) return
    lsep = max(1, len_trim(sep))   ! allow single blank only
    js = 0
    do
       js = index(str(jpos+1:lstr), sep(1:lsep))
       if (js.eq.0) then
          jh = lstr
       else
          jh = jpos + js - 1
       endif
       if (jpos.lt.jh) then
          if (nlim.ge.0) then
             if (n.ge.nlim) then
                jerr = _ERROR(ERR_OUT_OF_RANGE)
             else
                call parse_number(jerr, v(n), str(jpos+1:jh))
             endif
          endif
          n = n + 1
       else if (present(def)) then
          if (nlim.ge.0) then
             if (n.ge.nlim) then
                jerr = _ERROR(ERR_OUT_OF_RANGE)
             else
                v(n) = def(n)
             endif
          endif
          n = n + 1
       else if (eallow) then
          if (nlim.ge.0.and.n.ge.nlim) jerr = _ERROR(ERR_OUT_OF_RANGE)
          n = n + 1
       endif
       jpos = jh + lsep
       if (js.eq.0) exit
       if (jerr.ne.0) exit
    enddo
    if (jerr.lt.0) n = jerr
    return
  end subroutine split_list_f

  subroutine split_list_d &
       & (n, v, str, sep, lim, def, empty)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    integer,          intent(out)         :: n         ! number of elements or error code
    real(kind=KTGT),  intent(inout)       :: v(0:)
    character(len=*), intent(in)          :: str
    character(len=*), intent(in)          :: sep
    integer,          intent(in),optional :: lim       ! negative to count only; 0 to infinite
    real(kind=KTGT),  intent(in),optional :: def(0:*)  ! no bound check
    logical,          intent(in),optional :: empty     ! allow empty element (ignored if def present)

    integer jpos, lstr, lsep
    integer js,   jh
    integer nlim
    integer jerr
    logical eallow

    jerr = 0
    nlim = choice(0, lim)
    if (nlim.eq.0) nlim = +HUGE(0)
    eallow = choice(.TRUE., empty)

    n = 0
    jpos = 0
    lstr = len_trim(str)
    if (lstr.eq.0) return
    lsep = max(1, len_trim(sep))   ! allow single blank only
    js = 0
    do
       js = index(str(jpos+1:lstr), sep(1:lsep))
       if (js.eq.0) then
          jh = lstr
       else
          jh = jpos + js - 1
       endif
       if (jpos.lt.jh) then
          if (nlim.ge.0) then
             if (n.ge.nlim) then
                jerr = _ERROR(ERR_OUT_OF_RANGE)
             else
                call parse_number(jerr, v(n), str(jpos+1:jh))
             endif
          endif
          n = n + 1
       else if (present(def)) then
          if (nlim.ge.0) then
             if (n.ge.nlim) then
                jerr = _ERROR(ERR_OUT_OF_RANGE)
             else
                v(n) = def(n)
             endif
          endif
          n = n + 1
       else if (eallow) then
          if (nlim.ge.0.and.n.ge.nlim) jerr = _ERROR(ERR_OUT_OF_RANGE)
          n = n + 1
       endif
       jpos = jh + lsep
       if (js.eq.0) exit
       if (jerr.ne.0) exit
    enddo
    if (jerr.lt.0) n = jerr
    return
  end subroutine split_list_d

  subroutine split_list_a &
       & (n, v, str, sep, lim, def, empty)
    implicit none
    integer,         intent(out)         :: n         ! number of elements or error code
    character(len=*),intent(inout)       :: v(0:)
    character(len=*),intent(in)          :: str
    character(len=*),intent(in)          :: sep
    integer,         intent(in),optional :: lim       ! negative to count only; 0 to infinite
    character(len=*),intent(in),optional :: def(0:*)  ! no bound check
    logical,         intent(in),optional :: empty     ! allow empty element (ignored if def present)

    integer jpos, lstr, lsep
    integer js,   jh
    integer nlim
    integer jerr
    logical eallow

    jerr = 0
    nlim = choice(0, lim)
    if (nlim.eq.0) nlim = +HUGE(0)
    eallow = choice(.TRUE., empty)

    n = 0
    jpos = 0
    lstr = len_trim(str)
    if (lstr.eq.0) return
    lsep = max(1, len_trim(sep))   ! allow single blank only
    js = 0
    do
       js = index(str(jpos+1:lstr), sep(1:lsep))
       if (js.eq.0) then
          jh = lstr
       else
          jh = jpos + js - 1
       endif
       ! write(*, *) '  split', n, js, jpos, jh, str(jpos+1:lstr)
       if (jpos.lt.jh) then
          if (nlim.ge.0) then
             if (n.ge.nlim) then
                jerr = _ERROR(ERR_OUT_OF_RANGE)
             else
                v(n) = str(jpos+1:jh)
             endif
          endif
          n = n + 1
       else if (present(def)) then
          if (nlim.ge.0) then
             if (n.ge.nlim) then
                jerr = _ERROR(ERR_OUT_OF_RANGE)
             else
                v(n) = def(n)
             endif
          endif
          n = n + 1
       else if (eallow) then
          if (nlim.ge.0.and.n.ge.nlim) jerr = _ERROR(ERR_OUT_OF_RANGE)
          n = n + 1
       endif
       jpos = jh + lsep
       if (js.eq.0) exit
       if (jerr.ne.0) exit
    enddo
    if (jerr.lt.0) n = jerr
    return
  end subroutine split_list_a

!!!_  & find_first_range() - find first occurence of array within range (inclusive)
  integer function find_first_range_i &
       & (list, low, high, start, back, offset, no) &
       & result(n)
    implicit none
    integer,intent(in)          :: list(0:)
    integer,intent(in),optional :: low
    integer,intent(in),optional :: high
    integer,intent(in),optional :: start
    logical,intent(in),optional :: back
    integer,intent(in),optional :: offset
    integer,intent(in),optional :: no
    integer j, jb, ll, ofs
    integer vl, vh
    ofs = choice(find_offset, offset)
    jb  = choice(ofs, start) - ofs
    ll  = size(list)
    vl = choice((- HUGE(0)) - 1, low)
    vh = choice((+ HUGE(0)) + 0, high)
    n = -1
    if (choice(.false., back)) then
       do j = ll - 1, jb, -1
          if (vl.le.list(j).and.list(j).le.vh) then
             n = j
             exit
          endif
       enddo
    else
       do j = jb, ll - 1
          if (vl.le.list(j).and.list(j).le.vh) then
             n = j
             exit
          endif
       enddo
    endif
    if (n.ge.0) then
       n = n + ofs
    else if (present(no)) then
       n = no
    else
       n = min(-1, ofs - 1)
    endif
  end function find_first_range_i

!!!_  & find_first() - find first occurence of array
  integer function find_first_i &
       & (list, val, start, back, offset, no) &
       & result(n)
    implicit none
    integer,intent(in)          :: list(0:)
    integer,intent(in)          :: val
    integer,intent(in),optional :: start
    logical,intent(in),optional :: back
    integer,intent(in),optional :: offset
    integer,intent(in),optional :: no

    integer j, jb, ll, ofs
    ofs = choice(find_offset, offset)
    jb  = choice(ofs, start) - ofs
    ll  = size(list)
    n = -1
    if (choice(.false., back)) then
       do j = ll - 1, jb, -1
          if (list(j).eq.val) then
             n = j
             exit
          endif
       enddo
    else
       do j = jb, ll - 1
          if (list(j).eq.val) then
             n = j
             exit
          endif
       enddo
    endif
    if (n.ge.0) then
       n = n + ofs
    else if (present(no)) then
       n = no
    else
       n = min(-1, ofs - 1)
    endif
  end function find_first_i

  integer function find_first_f &
       & (list, val, start, back, offset, no, tol) &
       & result(n)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    real(kind=KTGT),intent(in)          :: list(0:)
    real(kind=KTGT),intent(in)          :: val
    integer,        intent(in),optional :: start
    logical,        intent(in),optional :: back
    integer,        intent(in),optional :: offset
    integer,        intent(in),optional :: no
    real(kind=KTGT),intent(in),optional :: tol

    real(kind=KTGT) :: r
    integer j, jb, ll, ofs
    ofs = choice(find_offset, offset)
    jb  = choice(ofs, start) - ofs
    r   = choice(0.0_KTGT, tol)
    ll  = size(list)
    n = -1
    if (choice(.false., back)) then
       do j = ll - 1, jb, -1
          if (ABS(list(j)-val).le.r) then
             n = j
             exit
          endif
       enddo
    else
       do j = jb, ll - 1
          if (ABS(list(j)-val).le.r) then
             n = j
             exit
          endif
       enddo
    endif
    if (n.ge.0) then
       n = n + ofs
    else if (present(no)) then
       n = no
    else
       n = min(-1, ofs - 1)
    endif
  end function find_first_f
  integer function find_first_d &
       & (list, val, start, back, offset, no, tol) &
       & result(n)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in)          :: list(0:)
    real(kind=KTGT),intent(in)          :: val
    integer,        intent(in),optional :: start
    logical,        intent(in),optional :: back
    integer,        intent(in),optional :: offset
    integer,        intent(in),optional :: no
    real(kind=KTGT),intent(in),optional :: tol

    real(kind=KTGT) :: r
    integer j, jb, ll, ofs
    ofs = choice(find_offset, offset)
    jb  = choice(ofs, start) - ofs
    r   = choice(0.0_KTGT, tol)
    ll  = size(list)
    n = -1
    if (choice(.false., back)) then
       do j = ll - 1, jb, -1
          if (ABS(list(j)-val).le.r) then
             n = j
             exit
          endif
       enddo
    else
       do j = jb, ll - 1
          if (ABS(list(j)-val).le.r) then
             n = j
             exit
          endif
       enddo
    endif
    if (n.ge.0) then
       n = n + ofs
    else if (present(no)) then
       n = no
    else
       n = min(-1, ofs - 1)
    endif
  end function find_first_d

  integer function find_first_a &
       & (list, val, start, back, offset, no) &
       & result(n)
    implicit none
    character(len=*),intent(in) :: list(0:)
    character(len=*),intent(in) :: val
    integer,intent(in),optional :: start
    logical,intent(in),optional :: back
    integer,intent(in),optional :: offset
    integer,intent(in),optional :: no

    integer j, jb, ll, ofs
    ofs = choice(0, offset)
    jb  = choice(ofs, start) - ofs
    ll  = size(list)
    n = -1
    if (choice(.false., back)) then
       do j = ll - 1, jb, -1
          if (list(j).eq.val) then
             n = j
             exit
          endif
       enddo
    else
       do j = jb, ll - 1
          if (list(j).eq.val) then
             n = j
             exit
          endif
       enddo
    endif
    if (n.ge.0) then
       n = n + ofs
    else if (present(no)) then
       n = no
    else
       n = min(-1, ofs - 1)
    endif
  end function find_first_a

!!!_  & jot
  subroutine jot(v, n, b, e, s)
    implicit none
    integer,intent(out)         :: v(0:*)
    integer,intent(in),optional :: n, b, e, s
    integer nn, bb, ee, ss
    integer j,  k
    ss = choice(1, s)
    if (present(n)) then
       nn = n
       if (present(b)) then
          bb = b
          if (present(e)) then
             ee = e
          else
             ee = bb + nn * ss
          endif
       else if (present(e)) then
          ee = e
          bb = ee - nn * ss
       else
          bb = 0
          ee = bb + nn * ss
       endif
    else if (ss.eq.0) then
       ! s must be non-zero if n not present
       return
    else
       bb = choice(0, b)
       if (present(e)) then
          ee = e
          nn = (ee - bb - 1) / ss + 1
          ee = bb + nn * ss
       else
          return
       endif
    endif
    nn = max(0, nn)
    k = bb
    j = 0
    do
       if (k.eq.ee) exit
       if (j.eq.nn) exit
       v(j) = k
       k = k + ss
       j = j + 1
    enddo
  end subroutine jot

!!!_  & inrange() - return true if l<=v<=h
  ELEMENTAL logical function inrange_i (v, l, h, s) result (b)
    implicit none
    integer,intent(in) :: v, l, h
    logical,intent(in),optional :: s  ! allow h<l case
    if (choice(.FALSE., s)) then
       if (l.lt.h) then
          b = (l.le.v .and. v.le.h)
       else
          b = (h.le.v .and. v.le.l)
       endif
    else
       b = (l.le.v .and. v.le.h)
    endif
  end function inrange_i
  ELEMENTAL logical function inrange_f (v, l, h, s) result (b)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    real(kind=KTGT),intent(in) :: v, l, h
    logical,intent(in),optional :: s  ! allow h<l case
    if (choice(.FALSE., s)) then
       if (l.lt.h) then
          b = (l.le.v .and. v.le.h)
       else
          b = (h.le.v .and. v.le.l)
       endif
    else
       b = (l.le.v .and. v.le.h)
    endif
  end function inrange_f
  ELEMENTAL logical function inrange_d (v, l, h, s) result (b)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: v, l, h
    logical,intent(in),optional :: s  ! allow h<l case
    if (choice(.FALSE., s)) then
       if (l.lt.h) then
          b = (l.le.v .and. v.le.h)
       else
          b = (h.le.v .and. v.le.l)
       endif
    else
       b = (l.le.v .and. v.le.h)
    endif
  end function inrange_d

!!!_  & begin_with() - return true if STR begins with SUB
  PURE logical function begin_with(str, sub) result (b)
    implicit none
    character(len=*),intent(in) :: str
    character(len=*),intent(in) :: sub
    integer l
    l = len_trim(sub)
    if (len(str).lt.l) then
       b = .FALSE.
    else
       b = str(1:l) .eq. sub(1:l)
    endif
  end function begin_with

!!!_  & find_next_sep - return separator position or end
  integer function find_next_sep(str, sep, pos, offset) result(n)
    implicit none
    character(len=*),intent(in) :: str
    character(len=*),intent(in) :: sep   ! use as it is (no trimming)
    integer,optional,intent(in) :: pos
    integer,optional,intent(in) :: offset   ! default == find_offset
    integer p, o
    o = choice(find_offset, offset)
    p = choice(o, pos) - o
    n = index(str(p+1:), sep)
    if (n.gt.0) then
       n = n + p + (o - 1)
    else
       n = len(str) + o
    endif
  end function find_next_sep

!!!_  & swap_items - swap two items
  subroutine swap_items_i (a, b)
    implicit none
    integer,intent(inout) :: a
    integer,intent(inout) :: b
    integer t
    t = a
    a = b
    b = t
  end subroutine swap_items_i
  subroutine swap_items_f (a, b)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    real(kind=KTGT),intent(inout) :: a
    real(kind=KTGT),intent(inout) :: b
    real(kind=KTGT) t
    t = a
    a = b
    b = t
  end subroutine swap_items_f
  subroutine swap_items_d (a, b)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(inout) :: a
    real(kind=KTGT),intent(inout) :: b
    real(kind=KTGT) t
    t = a
    a = b
    b = t
  end subroutine swap_items_d

!!!_  - find insertion position in sorted array by bisection
  integer function bisection_find_i &
       & (val, list, n, dir, offset) &
       & result(pos)
    implicit none
    integer,intent(in)          :: val
    integer,intent(in)          :: list(0:*)
    integer,intent(in)          :: n
    integer,intent(in)          :: dir
    integer,intent(in),optional :: offset

    integer jb, je, jx
    integer o

    o = choice(0, offset)
    if (n.eq.0) then
       pos = o
       return
    endif
    pos = o - 1
    jb = 0
    je = n
    if (dir.lt.0) then
       do
          jx = jb + (je - jb) / 2
          if (jx.eq.jb) exit
          if (list(n - 1 - jx).lt.val) then
             jb = jx
          else
             je = jx
          endif
       enddo
       if (list(n - 1 - jx).lt.val) then
          pos = n - 1 - jx + o
       else
          pos = n - jx + o
       endif
    else
       do
          jx = jb + (je - jb) / 2
          if (jx.eq.jb) exit
          if (list(jx).lt.val) then
             jb = jx
          else
             je = jx
          endif
       enddo
       if (list(jx).lt.val) then
          pos = jx + 1 + o
       else
          pos = jx + o
       endif
    endif
  end function bisection_find_i
  integer function bisection_find_f &
       & (val, list, n, dir, offset) &
       & result(pos)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    real(kind=KTGT),intent(in)  :: val
    real(kind=KTGT),intent(in)  :: list(0:*)
    integer,intent(in)          :: n
    integer,intent(in)          :: dir
    integer,intent(in),optional :: offset

    integer jb, je, jx
    integer o

    o = choice(0, offset)
    if (n.eq.0) then
       pos = o
       return
    endif
    pos = o - 1
    jb = 0
    je = n
    if (dir.lt.0) then
       do
          jx = jb + (je - jb) / 2
          if (jx.eq.jb) exit
          if (list(n - 1 - jx).lt.val) then
             jb = jx
          else
             je = jx
          endif
       enddo
       if (list(n - 1 - jx).lt.val) then
          pos = n - 1 - jx + o
       else
          pos = n - jx + o
       endif
    else
       do
          jx = jb + (je - jb) / 2
          if (jx.eq.jb) exit
          if (list(jx).lt.val) then
             jb = jx
          else
             je = jx
          endif
       enddo
       if (list(jx).lt.val) then
          pos = jx + 1 + o
       else
          pos = jx + o
       endif
    endif
  end function bisection_find_f
  integer function bisection_find_d &
       & (val, list, n, dir, offset) &
       & result(pos)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in)  :: val
    real(kind=KTGT),intent(in)  :: list(0:*)
    integer,intent(in)          :: n
    integer,intent(in)          :: dir
    integer,intent(in),optional :: offset

    integer jb, je, jx
    integer o

    o = choice(0, offset)
    if (n.eq.0) then
       pos = o
       return
    endif
    pos = o - 1
    jb = 0
    je = n
    if (dir.lt.0) then
       do
          jx = jb + (je - jb) / 2
          if (jx.eq.jb) exit
          if (list(n - 1 - jx).lt.val) then
             jb = jx
          else
             je = jx
          endif
       enddo
       if (list(n - 1 - jx).lt.val) then
          pos = n - 1 - jx + o
       else
          pos = n - jx + o
       endif
    else
       do
          jx = jb + (je - jb) / 2
          if (jx.eq.jb) exit
          if (list(jx).lt.val) then
             jb = jx
          else
             je = jx
          endif
       enddo
       if (list(jx).lt.val) then
          pos = jx + 1 + o
       else
          pos = jx + o
       endif
    endif
  end function bisection_find_d

!!!_ + (system) control procedures
!!!_  & is_first_force () - check if first time or force
  logical function is_first_force(n, mode) result(b)
    implicit none
    integer,intent(in)          :: n
    integer,intent(in),optional :: mode
    b = (n.eq.0) .or. (IAND(choice(0, mode), MODE_FORCE).gt.0)
    return
  end function is_first_force

!!!_  & control_mode () - set init/diag/finalize mode
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

!!!_  & control_deep () - set init/diag/finalize mode at deep level
  integer function control_deep(mode, arg) result(n)
    implicit none
    integer,intent(in)          :: mode
    integer,intent(in),optional :: arg
    integer lmd
    integer,parameter :: mskd = MODE_BIT_DEEP - 1
    integer,parameter :: mskl = MODE_FORCE - 1
    integer,parameter :: mskh = NOT(mskl)
    lmd = IAND(mode, mskl)
    if (lmd.lt.MODE_DEEPEST) lmd = IAND(lmd, mskd)
    n = IOR(IAND(choice(mode, arg), mskh), lmd)
    return
  end function control_deep

!!!_  & control_lev () - set init/diag/finalize verbose level
  integer function control_lev(lev, ref, def) result(n)
    implicit none
    integer,intent(in) :: lev  ! target level
    integer,intent(in) :: ref  ! reference level
    integer,intent(in) :: def  ! default level
    ! return lev when it is higher than rev, otherwise def
    if (VCHECK(lev, ref)) then
       n = lev
    else
       n = def
    endif
    return
  end function control_lev
!!!_ + end module TOUZA_Std_utl
end module TOUZA_Std_utl


!!!_@ test_std_utl - test program
#if TEST_STD_UTL
program test_std_utl
  use TOUZA_Std_utl
  use TOUZA_Std_prc,only: KDBL
  implicit none
  integer,parameter :: KTGT=KDBL
  integer ierr
  character(len=128) :: T0, T1
  integer a(4), b(2)
  integer i
  real(kind=KTGT) :: v

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

  call test_compact((/0/))
  call test_compact((/0,0,0,0/))
  call test_compact((/0, 1, 2, 3, 4, 5/))
  call test_compact((/0, 1, 4, 9, 16, 25/))
  call test_compact((/0,0,0, &
       &              4,4,4, 4, &
       &              5,        &
       &              5,6,7,8,  &
       &              3,        &
       &              2,4,6/))
  call test_compact((/(i*50, i=-8, 8)/))
  call test_compact((/(i*i, i=0, 30)/))
  call test_compact((/((-1)**i * (i*i), i=0, 30)/))

  call test_compact_str(0.0_KTGT)
  call test_compact_str(+HUGE(0.0_KTGT))
  call test_compact_str(-HUGE(0.0_KTGT))
  call test_compact_str(1.e3_KTGT)
  v = 1.0_KTGT
  do i = 0, PRECISION(0.0_KTGT) + 1
     call test_compact_str(v)
     v = v * 10.0_KTGT + mod(i + 2, 10) * 1.0_KTGT
  enddo
  v = 1.0_KTGT
  do i = 0, PRECISION(0.0_KTGT) + 1
     call test_compact_str(v)
     v = v * 10.0_KTGT
  enddo
  v = 1.0_KTGT
  do i = 0, PRECISION(0.0_KTGT) + 1
     call test_compact_str(v)
     v = v / 2.0_KTGT
  enddo

  call test_nparser('123')
  call test_nparser('12,3')
  call test_nparser('12/3')
  call test_nparser('12*3')
  call test_nparser('/123')
  call test_nparser('*123')
  call test_nparser(',123')

  call finalize(ierr)

  a(:) = (/11, 12, 13, 14/)
  b(:) = (/21, 22/)
  call test_choice(0)
  call test_choice(0,y=111)
  call test_choice(1)
  call test_choice(1,x=a)
  call test_choice(1,x=b)

  call test_join((/0/))
  call test_join((/0,1,2/))

  call test_join_r((/0.0_KTGT/))
  call test_join_r((/0.0_KTGT,1.0_KTGT,2.0_KTGT/))
  call test_join_r((/1.0_KTGT,0.5_KTGT,0.25_KTGT/))

  call test_split(':')

  call test_split('::')
  call test_split(' ')
  call test_split('10')

  call test_split('10:')
  call test_split(':20')
  call test_split('10:20')

  call test_split('10::')
  call test_split(':20:')
  call test_split('::30')
  call test_split('10:20:')
  call test_split('10::30')
  call test_split(':20:30')
  call test_split('10:20:30')

  call test_split(':::')
  call test_split('10:::')
  call test_split(':20::')
  call test_split('::30:')
  call test_split('10:20::')
  call test_split('10::30:')
  call test_split(':20:30:')
  call test_split('10:20:30:')

  call test_split('::::')

  call test_split_heads('//', ' ')
  call test_split_heads('//', 'abcde')
  call test_split_heads('//', 'a//b//c//d')
  call test_split_heads('//', '//a//b//c//d')
  call test_split_heads('//', 'a//b//c//d//')
  call test_split_heads('//', '//a//b//c//d//')
  call test_split_heads('//', '////a//b//c//d')
  call test_split_heads('//', '////a//b//c//d////')
  call test_split_heads('//', '////a////b//c//d////')

  call test_find((/0/))
  call test_find((/0,1,2,0,3,4/))

  call test_jot(n=10)
  call test_jot(      b=1)
  call test_jot(n=10, b=1)
  call test_jot(           e=8)
  call test_jot(n=10,      e=8)
  call test_jot(      b=1, e=8)
  call test_jot(n=10, b=1, e=8)

  call test_find_next_sep('abcabcabc', 'ab')
  call test_find_next_sep('abcabcabc', 'ab', 2)
  call test_find_next_sep('abcabcabc', 'ca', 8)

  call test_bisection(6, +1)
  call test_bisection(7, +1)
  call test_bisection(0, +1)

  call test_bisection(6, -1)
  call test_bisection(7, -1)
  call test_bisection(0, -1)

  stop
contains
  subroutine test_choice(k, x, y)
    implicit none
    integer,intent(in)          :: k
    integer,intent(in),optional :: x(:)
    integer,intent(in),optional :: y
    integer p(3), q(3)

    q(:) = (/1, 2, 3/)

101 format('choice:', I0, 1x, L1, 1x, A, T20, 1x, 3(1x, I0))

    if (k.eq.0) then
       p(:) = 0
       p(:) = choice(1, y)
       write(*, 101) k, present(y), '1, y', p(:)

       p(:) = 0
       p(:) = choice(q(:), y)
       write(*, 101) k, present(y), 'q(:), y', p(:)
    else
       p(:) = 0
       p(:) = choice(1, x)
       write(*, 101) k, present(x), '1, x(:)', p(:)

       p(:) = 0
       p(:) = choice(q(:), x)
       write(*, 101) k, present(x), 'q(:), x(:)', p(:)
    endif

  end subroutine test_choice

  subroutine test_ndigits(n)
    implicit none
    integer,intent(in) :: n
    integer r
    r = ndigits(n)
102 format('ndigits: ', I0, ' = ', I0)
    write(*, 102) n, r
    return
  end subroutine test_ndigits

  subroutine test_compact(v)
    implicit none
    integer,intent(in) :: v(:)
    integer nr
    integer n
    character(len=64) :: text
101 format('compact input: (', I0, ') ', 128(1x, I0))
102 format('compact output: ', A, ' >> ', I0)
    n = size(v, 1)
    write(*, 101) n, v(:)
    call compact_format(nr, text, v(:))
    write(*, 102) trim(text), nr
    call compact_format(nr, text, v(:), nrep=3)
    write(*, 102) trim(text), nr
    call compact_format(nr, text, v(:), sep=',')
    write(*, 102) trim(text), nr
    call compact_format(nr, text, v(:), fmt='I3.3')
    write(*, 102) trim(text), nr
    call compact_format(nr, text, v(:), pad=2)
    write(*, 102) trim(text), nr
    call compact_format(nr, text, v(:), pad=-2)
    write(*, 102) trim(text), nr
    call compact_format(nr, text, v(:), pad=3, clipl=0, cliph=100)
    write(*, 102) trim(text), nr
    ! call compact_format(jerr, text, v(3:))
    ! write(*, 102) trim(text)
    return
  end subroutine test_compact

  subroutine test_compact_str(v)
    implicit none
    real(kind=KTGT),intent(in) :: v
    character(len=128) :: text
    integer ierr

101 format('compact2: ', A)
102 format('compact2:def: ', A)
103 format('compact2:mag: ', A)
    write(text, *) v
    write(*, 101) trim(adjustl(text))
    call compact_string(ierr, text, v)
    write(*, 102) trim(text)
    call compact_string(ierr, text, v, mag=5)
    write(*, 103) trim(text)
  end subroutine test_compact_str

  subroutine test_join(v)
    implicit none
    integer,intent(in) :: v(:)
    call test_join_sub(v)
    call test_join_sub(v, ' ')
    call test_join_sub(v, ',')
    call test_join_sub(v, ',', '[', ']')
  end subroutine test_join

  subroutine test_join_sub(v, sep, ld, rd)
    implicit none
    integer,intent(in) :: v(:)
    character(len=*),intent(in),optional :: sep, ld, rd
    character(len=128) :: msg
    character(len=32) :: strlong
    character(len=4)  :: strshort
    integer jerr
    msg = ' '
    if (present(sep)) then
       msg = trim(msg) // ' sep[' // sep // ']'
    endif
    if (present(ld)) then
       msg = trim(msg) // ' left:' // ld
    endif
    if (present(rd)) then
       msg = trim(msg) // ' right:' // rd
    endif
101 format('join:', A)
    write(*, 101) trim(msg)
    call join_list(jerr, strlong, v, sep=sep, ldelim=ld, rdelim=rd)
109 format('join:', A, ':', I0, ' >> ', A, ' //')
    write(*, 109) 'long:', jerr, trim(strlong)
    call join_list(jerr, strshort, v, sep=sep, ldelim=ld, rdelim=rd)
    write(*, 109) 'short:', jerr, trim(strshort)
  end subroutine test_join_sub

  subroutine test_join_r(v)
    implicit none
    real(kind=KTGT),intent(in) :: v(:)
    call test_join_rsub(v)
    call test_join_rsub(v, ' ')
    call test_join_rsub(v, ',')
    call test_join_rsub(v, ',', '[', ']')
    call test_join_rsub(v, fmt=' ')
  end subroutine test_join_r

  subroutine test_join_rsub(v, sep, ld, rd, fmt)
    implicit none
    real(kind=KTGT),intent(in) :: v(:)
    character(len=*),intent(in),optional :: sep, ld, rd, fmt
    character(len=128) :: msg
    character(len=32) :: strlong
    character(len=4)  :: strshort
    integer jerr
    msg = ' '
    if (present(sep)) then
       msg = trim(msg) // ' sep[' // sep // ']'
    endif
    if (present(ld)) then
       msg = trim(msg) // ' left:' // trim(ld)
    endif
    if (present(rd)) then
       msg = trim(msg) // ' right:' // trim(rd)
    endif
    if (present(fmt)) then
       msg = trim(msg) // ' fmt:' // trim(fmt)
    endif
101 format('join:', A)
    write(*, 101) trim(msg)
    call join_list(jerr, strlong, v, sep=sep, ldelim=ld, rdelim=rd, fmt=fmt)
109 format('join:', A, ':', I0, ' >> ', A, ' //')
    write(*, 109) 'long:', jerr, trim(strlong)
    call join_list(jerr, strshort, v, sep=sep, ldelim=ld, rdelim=rd, fmt=fmt)
    write(*, 109) 'short:', jerr, trim(strshort)
  end subroutine test_join_rsub

  subroutine test_split(str)
    implicit none
    character(len=*),intent(in) :: str
    call test_split_sub(str, -1, .TRUE.)
    call test_split_sub(str, -1, .FALSE.)
    call test_split_sub(str, 2,  .TRUE.)
    call test_split_sub(str, 2,  .FALSE.)
    call test_split_sub(str, 3,  .TRUE.)
    call test_split_sub(str, 3,  .FALSE.)
    call test_split_sub(str, 4,  .TRUE.)
    call test_split_sub(str, 4,  .FALSE.)
  end subroutine test_split

  subroutine test_split_sub(str, lim, empty)
    implicit none
    character(len=*),intent(in) :: str
    integer,         intent(in) :: lim
    logical,         intent(in) :: empty
    integer v(0:lim-1)
    integer def(0:lim-1)
    integer j
    integer n, m
    character(len=*),parameter :: sep = ':'

    do j = 0, lim - 1
       def(j) = - (j + 1)
    enddo

101 format('split/', A, L1, ':', I0, 1x, '[', A, '],', I0, 1x, 5(1x, I0))

    if (lim.lt.0) then
       ! count only
       call split_list(n, v, str, sep, lim,      empty=empty)
       write(*, 101) 'c', empty, n, trim(str), lim
    else
       call split_list(n, v, str, sep, lim, def, empty=empty)
       m = min(n, lim)
       write(*, 101) 'd', empty, n, trim(str), lim, v(0:m-1)

       v = def
       call split_list(n, v, str, sep, lim,      empty=empty)
       m = min(n, lim)
       write(*, 101) 'n', empty, n, trim(str), lim, v(0:m-1)
    endif
  end subroutine test_split_sub

  subroutine test_split_heads(sep, str)
    implicit none
    character(len=*),intent(in) :: sep
    character(len=*),intent(in) :: str
    call test_split_heads_sub(sep, str, .TRUE.)
    call test_split_heads_sub(sep, str, .FALSE.)
  end subroutine test_split_heads

  subroutine test_split_heads_sub(sep, str, empty)
    implicit none
    character(len=*),intent(in) :: sep
    character(len=*),intent(in) :: str
    logical,         intent(in) :: empty

    integer,parameter :: lim = 32
    integer h(0:lim-1)
    integer m, n

    call split_heads(m, h, str, sep, -1, empty)
101 format('split_heads/', L1, ':', I0, 1x, '[', A, ']', 16(1x, I0))
    write(*, 101) empty, m, trim(str)
    if (m.ge.0) then
       call split_heads(n, h, str, sep, 0,  empty)
       write(*, 101) empty, n, trim(str), h(0:n-1)
    endif
  end subroutine test_split_heads_sub

  subroutine test_find(v)
    implicit none
    integer,intent(in) :: v(:)
    call test_find_sub(v, 0)
    call test_find_sub(v, 1)
    call test_find_sub(v, 3)
  end subroutine test_find

  subroutine test_find_sub(v, val)
    implicit none
    integer,intent(in) :: v(0:)
    integer,intent(in) :: val
    integer j

101 format('find:list = ', 10(1x, I0))
102 format('find:found at ', I0, ' = ', I0, 1x, L1)
    write(*, 101) v
    j = 0
    do
       if (j.lt.0) exit
       j = find_first(v, val, j)
       if (j.ge.0) then
          write(*, 102) j, val, val.eq.v(j)
          j = j + 1
       endif
    enddo

  end subroutine test_find_sub

  subroutine test_nparser(str)
    implicit none
    character(len=*),intent(in) :: str
    integer na, nb
    integer ea, eb

    na = -999
    nb = -999
    read(str, *, IOSTAT=ea) na
    call parse_number(eb, nb, str)

101 format('parse_number[', A, '] = ', &
         & 2(' (', I0, 1x, I0, ')'))
    write(*, 101) trim(str), na, ea, nb, eb

    return
  end subroutine test_nparser

  subroutine test_jot(n, b, e)
    implicit none
    integer,intent(in),optional :: n
    integer,intent(in),optional :: b
    integer,intent(in),optional :: e

    call test_jot_sub(n, b, e)
    call test_jot_sub(n, b, e, -3)
    call test_jot_sub(n, b, e, -2)
    call test_jot_sub(n, b, e, -1)
    call test_jot_sub(n, b, e, 0)
    call test_jot_sub(n, b, e, +1)
    call test_jot_sub(n, b, e, +2)
    call test_jot_sub(n, b, e, +3)

  end subroutine test_jot

  subroutine test_jot_sub(n, b, e, s)
    implicit none
    integer,intent(in),optional :: n
    integer,intent(in),optional :: b
    integer,intent(in),optional :: e
    integer,intent(in),optional :: s

    integer v(0:100)
    integer d
    integer j
    character(len=4)  :: tag(4)
    character(len=64) :: args
    integer jerr

    d = -9999
    v(:) = d

    call jot(v, n, b, e, s)
    j = find_first(v, d, offset=0)
    call fmt_or_null(tag(1), n)
    call fmt_or_null(tag(2), b)
    call fmt_or_null(tag(3), e)
    call fmt_or_null(tag(4), s)

    call join_list(jerr, args, tag)

101 format('jot: ', A, ' (', I0, ') ', 100(1x, I0))
    write(*, 101) trim(args), j, v(0:j-1)
  end subroutine test_jot_sub

  subroutine fmt_or_null(str, n)
    implicit none
    character(len=*),intent(out)         :: str
    integer,         intent(in),optional :: n
    if (present(n)) then
       write(str, '(I0)') n
    else
       str = '-'
    endif
  end subroutine fmt_or_null

  subroutine test_find_next_sep(str, sep, pos)
    implicit none
    character(len=*),intent(in) :: str
    character(len=*),intent(in) :: sep
    integer,optional,intent(in) :: pos
    integer r

101 format('find_next_sep: [', A, '][', A,'] <', A, '> ', 1x, I0)
102 format('find_next_sep: [', A, '] <', A, '> ', 1x, I0)
    r = find_next_sep(str, sep, pos)
    if (present(pos)) then
       write(*, 101) str(1:pos-1), str(pos:), sep, r
    else
       write(*, 102) str, sep, r
    endif
  end subroutine test_find_next_sep

  subroutine test_bisection &
       & (n, dir)
    implicit none
    integer,intent(in) :: n
    integer,intent(in) :: dir
    integer :: list(0:n-1)
    integer j, v
    integer pos
    character(len=256) :: str, strl, strr
    integer jerr

    if (dir.lt.0) then
       do j = 0, n - 1
          list(n - j - 1) = j * 2 + 1
       enddo
    else
       do j = 0, n - 1
          list(j) = j * 2 + 1
       enddo
    endif
102 format('bisection/input: ', A)
    call join_list(jerr, str, list(0:n-1))
    write(*, 102) trim(str)
    do v = 0, 2 * n
       pos = bisection_find(v, list, n, dir)
101    format('bisection:', I0, 1x, A, ' [', I0, '] ', A)
       call join_list(jerr, strl, list(0:pos-1))
       call join_list(jerr, strr, list(pos:n-1))
       write(*, 101) pos, trim(strl), v, trim(strr)
    enddo

  end subroutine test_bisection

end program test_std_utl
#endif /* TEST_STD_UTL */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

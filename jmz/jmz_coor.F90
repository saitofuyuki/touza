!!!_! jmz_coor.F90 - TOUZA/Jmz coordinate (loop) manipulation
! Maintainer: SAITO Fuyuki
! Created: Oct 6 2023
#define TIME_STAMP 'Time-stamp: <2024/04/05 20:55:50 fuyuki jmz_coor.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2023, 2024
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "jmz_config.h"
#endif
#include "jmz.h"
!!!_* macros
#ifndef    OPT_JMZ_COORDINATE_KIND
#  define  OPT_JMZ_COORDINATE_KIND -1
#endif
!!!_ + OPT_JMZ_COORDINATE_KIND adjustment
#if       OPT_JMZ_COORDINATE_KIND < 0
#  undef  OPT_JMZ_COORDINATE_KIND
#  define OPT_JMZ_COORDINATE_KIND KIND(0)
#endif
!!!_@ TOUZA/Jmz/coor - jmz loop library
module Jmz_coor
!!!_ + Declaration
!!!_  - modules
  use Jmz_param
  use Jmz_base
!!!_  - default
  implicit none
  private
!!!_  - coordinate kind
  integer,parameter,public :: KCO = OPT_JMZ_COORDINATE_KIND
!!!_  - string
  integer,parameter,public :: lname = litem * 4

!!!_  - coordinate matching
  integer,parameter :: co_unset = -5
  integer,parameter :: co_del   = -4
  integer,parameter :: co_null  = -3
  integer,parameter :: co_ins   = -2
  integer,parameter :: co_wild  = -1
  integer,parameter :: co_normal = 0

!!!_  - coordinate(loop) type
  integer,parameter,public :: loop_error  = -2
  integer,parameter,public :: loop_unset  = -1
  integer,parameter,public :: loop_null   = 0       ! null coordinate  (expand to bgn:end)
  integer,parameter,public :: loop_reduce = 1       ! sweep coordinate (shrink to 0:1)
  integer,parameter,public :: loop_normal = 2

!!!_  - misc
  integer(kind=KCO),parameter :: C0 = 0_KCO
  integer(kind=KCO),parameter :: C1 = 1_KCO

!!!_  - range special
  integer(kind=KCO),parameter,public :: full_range = + HUGE(C0)             ! case low:
  integer(kind=KCO),parameter,public :: null_range = (- HUGE(C0)) - C1      ! case low (MUST BE NEGATIVE)

!!!_  - loop property
  type loop_t
     integer           :: flg = loop_unset
     integer(kind=KCO) :: bgn = - C1
     integer(kind=KCO) :: end = - C1
     integer(kind=KCO) :: ofs = C0
     integer(kind=KCO) :: cyc = - C1        ! 0 if still effective empty
     character(len=lname) :: name
  end type loop_t

  type(loop_t),save,public :: def_loop  = loop_t(loop_unset, null_range, null_range, C0, -C1, ' ')

  public :: loop_t

!!!_  - interfaces
  interface get_range_string
     module procedure get_range_string_t, get_range_string_c
  end interface get_range_string

  interface flat_index_l2p
     module procedure flat_index_l2p_be, flat_index_l2p_mem
  end interface flat_index_l2p
  interface logical_index_array
     module procedure logical_index_array_be, logical_index_array_mem
  end interface logical_index_array
  interface physical_index_flat
     module procedure physical_index_flat_be, physical_index_flat_mem
  end interface physical_index_flat
!!!_  - public
  public :: get_range_string
  public :: is_null_coor, count_effective, coordinate_type
  public :: show_lpp
  public :: pack_mems, gen_stride, flat_index_l2p
  public :: logical_index_array, physical_index_flat
!!!_ + Procedures
contains
!!!_  - show_lpp
  subroutine show_lpp &
       & (ierr, lpp, tag, u, levv, indent)
    use TOUZA_Std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    type(loop_t),    intent(in)          :: lpp(0:)
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer,         intent(in),optional :: indent
    integer utmp
    integer lv
    integer tab
    integer jf
    character(len=64) :: pfx
    character(len=16) :: cflg
    character(len=*),parameter :: loopc = 'eu*-n'

    integer jc

    ierr = 0
    lv = choice(lev_verbose, levv)
    utmp = choice(ulog, u)
    tab = choice(0, indent)
    if (present(tag)) then
       pfx = '[' // trim(tag) // ']'
    else
       pfx = ' '
    endif
    do jc = 0, size(lpp) - 1
111    format(A, 'loop', A, ': ', I0, 1x, A, 1x, I0, ':', I0, '(', A, ')+', I0, '/', I0)
112    format('#', I0, '#')
       jf = lpp(jc)%flg - loop_error
       if (jf.ge.0.and.jf.lt.len(loopc)) then
          cflg = loopc(1+jf:1+jf)
       else
          write(cflg, 112) lpp(jc)%flg
       endif
       if (utmp.ge.0) then
          write(utmp, 111) repeat(' ', tab), trim(pfx), jc, trim(lpp(jc)%name), &
               & lpp(jc)%bgn, lpp(jc)%end, trim(cflg), lpp(jc)%ofs, lpp(jc)%cyc
       else if (utmp.eq.-1) then
          write(*,    111) repeat(' ', tab), trim(pfx), jc, trim(lpp(jc)%name), &
               & lpp(jc)%bgn, lpp(jc)%end, trim(cflg), lpp(jc)%ofs, lpp(jc)%cyc
       endif
    enddo
  end subroutine show_lpp

!!!_  - user_index_bgn()
  ELEMENTAL integer function user_index_bgn(j, n) result(k)
    use Jmz_param,only: user_offset_bgn
    implicit none
    integer,intent(in)          :: j
    integer,intent(in),optional :: n
    if (j.eq.full_range .or. j.eq.null_range) then
       k = j
    else
       k = j + user_offset_bgn
    endif
  end function user_index_bgn
!!!_  - user_index_end()
  ELEMENTAL integer function user_index_end(j, n) result(k)
    use Jmz_param,only: user_offset_end
    implicit none
    integer,intent(in)          :: j
    integer,intent(in),optional :: n
    if (j.eq.full_range .or. j.eq.null_range) then
       k = j
    else
       k = j + user_offset_end
    endif
  end function user_index_end
!!!_  - system_index_bgn()
  ELEMENTAL integer function system_index_bgn(j, n) result(k)
    use Jmz_param,only: user_offset_bgn
    implicit none
    integer,intent(in)          :: j
    integer,intent(in),optional :: n
    if (j.eq.full_range .or. j.eq.null_range) then
       k = j
    else
       k = j - user_offset_bgn
    endif
  end function system_index_bgn
!!!_  - system_index_end()
  ELEMENTAL integer function system_index_end(j, n) result(k)
    use Jmz_param,only: user_offset_end
    implicit none
    integer,intent(in)          :: j
    integer,intent(in),optional :: n
    if (j.eq.full_range .or. j.eq.null_range) then
       k = j
    else
       k = j - user_offset_end
    endif
  end function system_index_end

!!!_  - logical_index
  ELEMENTAL integer function logical_index (l, p) result(n)
    implicit none
    integer,intent(in) :: l, p
    if (l.eq.null_range) then
       n = p
    else
       n = l
    endif
  end function logical_index

!!!_  - get_perm_string
  subroutine get_perm_string &
       & (ierr, str, cname, ctype, cpidx, pcp, lcp, mco)
    use TOUZA_Std,only: join_list
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    character(len=*),intent(in)  :: cname(0:*)
    integer,         intent(in)  :: ctype(0:*)
    integer,         intent(in)  :: cpidx(0:*)
    type(loop_t),    intent(in)  :: pcp(0:*)
    type(loop_t),    intent(in)  :: lcp(0:*)
    integer,         intent(in)  :: mco

    character(len=128)   :: pdom, ldom, ndom
    character(len=lname) :: co(0:mco-1)
    integer jodr, jco
    integer meff

    ierr = 0

    if (ierr.eq.0) call get_domain_string(ierr, pdom, pcp, mco)
    if (ierr.eq.0) call get_domain_string(ierr, ldom, lcp, mco)
    if (ierr.eq.0) then
       meff = -1
111    format(A, '=', A)
112    format('<', A, '>')
113    format('<', I0, '>')
114    format('<', I0, '=', A, '>')
       do jodr = 0, mco - 1
          jco = cpidx(jodr)
          select case(ctype(jodr))
          case(co_wild)
             if (jco.ge.0) then
                if (pcp(jco)%name.ne.' ') then
                   write(co(jodr), 111, IOSTAT=ierr) trim(pcp(jco)%name), '*'
                else
                   write(co(jodr), 114, IOSTAT=ierr) jco, '*'
                endif
             else
                write(co(jodr), 112, IOSTAT=ierr) '*'
             endif
          case(co_null)
             if (jco.ge.0) then
                if (pcp(jco)%name.ne.' ') then
                   write(co(jodr), 111, IOSTAT=ierr) trim(pcp(jco)%name), '-'
                else
                   write(co(jodr), 114, IOSTAT=ierr) jco, '-'
                endif
             else
                write(co(jodr), 112, IOSTAT=ierr) '-'
             endif
          case(co_normal)
             if (jco.ge.0) then
                if (pcp(jco)%name.eq.cname(jodr)) then
                   co(jodr) = cname(jodr)
                else
                   write(co(jodr), 111, IOSTAT=ierr) trim(pcp(jco)%name), trim(cname(jodr))
                endif
             else
                write(co(jodr), 112, IOSTAT=ierr) trim(cname(jodr))
             endif
          case default
             write(co(jodr), 113, IOSTAT=ierr) jodr
          end select
          if (jco.ge.0 .or. ctype(jodr).ne.co_null) meff = jodr
       enddo
       call join_list(ierr, ndom, co(0:meff), ldelim='[', rdelim=']')
    endif
    if (ierr.eq.0) then
101    format(A, 1x, A, ' > ', A)
       write(str, 101, IOSTAT=ierr) trim(pdom), trim(ldom), trim(ndom)
    endif
  end subroutine get_perm_string

!!!_  - get_domain_string
  subroutine get_domain_string(ierr, str, lpp, mco)
    use TOUZA_Std,only: join_list, choice
    use Jmz_param,only: rename_sep
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    type(loop_t),    intent(in)  :: lpp(0:*)
    integer,         intent(in)  :: mco

    character(len=64) :: cstr(0:mco-1)
    character(len=64) :: cran
    character(len=16) :: rsep

    integer jc, nc
    integer jp, ls
    integer jerr

    ierr = 0

    nc = 0
    do jc = 0, mco - 1
       call get_range_string(ierr, cran, lpp(jc))
102    format(A, A, A)
103    format(A, A)
       if (lpp(jc)%name.eq.' ') then
          cstr(jc) = trim(cran)
       else
          ls = len_trim(lpp(jc)%name)
          jp = index(lpp(jc)%name(1:ls), rename_sep, back=.TRUE.)
          if (jp.lt.ls-len(rename_sep)+1) then
             write(cstr(jc), 102, IOSTAT=jerr) trim(lpp(jc)%name), rename_sep, trim(cran)
          else
             write(cstr(jc), 103, IOSTAT=jerr) trim(lpp(jc)%name), trim(cran)
          endif
       endif
       if (lpp(jc)%flg.ge.loop_null) nc = jc     ! if lpp is set
       if (lpp(jc)%name.ne.' ') nc = jc
    enddo
    call join_list(ierr, str, cstr(0:nc), ldelim='[', rdelim=']')
  end subroutine get_domain_string

!!!_  - get_range_string
  subroutine get_range_string_t &
       & (ierr, str, lpp)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    type(loop_t),    intent(in)  :: lpp
    call get_range_string_c &
         & (ierr, str, lpp%bgn, lpp%end, lpp%flg, lpp%ofs)
  end subroutine get_range_string_t

  subroutine get_range_string_c &
       & (ierr, str, b, e, flg, o, c)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    integer,         intent(in)  :: b, e, flg
    integer,optional,intent(in)  :: o, c
    integer bb, ee
    character(len=32) ::obuf

    ierr = 0

111 format(':')
112 format(I0, ':')
113 format(':', I0)
114 format(I0, ':', I0)
118 format('(', I0, ')')
    bb = user_index_bgn(b)
    ee = user_index_end(e)
    if (bb.eq.null_range) then
       if (ee.eq.null_range) then
          write(str, 111, IOSTAT=ierr)
          if (flg.eq.loop_normal) str = trim(str) // '(1)'
       else
          write(str, 113, IOSTAT=ierr) ee
       endif
    else if (ee.eq.null_range) then
       write(str, 112, IOSTAT=ierr) bb
    else if (b.eq.0.and.e.eq.0) then
       ! str = '-'
       str = ' '
    else
       write(str, 114, IOSTAT=ierr) bb, ee
    endif

    if (flg.eq.loop_normal) then
       continue
    else if (flg.eq.loop_reduce) then
       str = trim(str) // '(-)'
    else if (flg.eq.loop_null) then
       str = trim(str) // '(0)'
    else if (flg.eq.loop_unset) then
       continue
    else
       write(obuf, 118, IOSTAT=ierr) flg
       str = trim(str) // trim(obuf)
    endif
    if (str.eq.' ') str = '-'
    if (present(o)) then
121    format(':', SP, I0)
       if (o.ne.0) then
          write(obuf, 121) o
          str = trim(str) // trim(obuf)
       endif
    endif
  end subroutine get_range_string_c

!!!_  - count_effective()
  integer function count_effective(lpp, mc) result(nf)
    use TOUZA_Std,only: choice
    implicit none
    type(loop_t),    intent(in) :: lpp(0:)
    integer,optional,intent(in) :: mc
    integer jc, nc
    nf = -1
    nc = choice(size(lpp), mc)
    do jc = 0, min(size(lpp), nc) - 1
       if (lpp(jc)%flg.ge.loop_null) nf = jc
       if (lpp(jc)%name.ne.' ') nf = jc
    enddo
    nf = nf + 1
  end function count_effective

!!!_  - is_null_coor() - check if argument loop_t corresponds to null-coordinate
  logical function is_null_coor(lpp) result (b)
    implicit none
    type(loop_t),intent(in) :: lpp
    b = (lpp%flg.le.loop_reduce.and.lpp%name.eq.' ')
  end function is_null_coor

!!!_  - coordinate_type()
  integer function coordinate_type(name, lcp, pcp) result(n)
    implicit none
    character(len=*),intent(in)          :: name
    type(loop_t),    intent(in)          :: lcp
    type(loop_t),    intent(in),optional :: pcp
    integer b, e

    if (name.ne.' ') then
       n = co_normal
       return
    endif
    n = co_unset
    if (present(pcp)) then
       if (lcp%flg.lt.loop_null) then
          if (pcp%flg.gt.loop_reduce) then
             n = co_wild
          else
             n = co_null
          endif
       else
          if (pcp%flg.le.loop_null) then
             b = lcp%bgn
             e = lcp%end
          else
             b = logical_index(lcp%bgn, pcp%bgn)
             e = logical_index(lcp%end, pcp%end)
          endif
          if (b.eq.null_range .eqv. e.eq.null_range) then
             if (b.eq.null_range) then
                n = co_null
             else if (b.lt.e) then
                n = co_wild
             else
                n = co_null
             endif
          else
             n = co_wild
          endif
       endif
    else
       if (lcp%flg.lt.loop_null) then
          n = co_null
       else
          b = lcp%bgn
          e = lcp%end
          if (b.eq.null_range .eqv. e.eq.null_range) then
             if (b.eq.null_range) then
                n = co_null
             else if (b.lt.e) then
                n = co_wild
             else
                n = co_null
             endif
          else
             n = co_wild
          endif
       endif
    endif

  end function coordinate_type

!!!_ + primitives
!!!_  & pack_mems
  subroutine pack_mems(dest, n, src, xtbl)
    implicit none
    integer,intent(out) :: dest(0:*)
    integer,intent(in)  :: n
    integer,intent(in)  :: src(0:*)
    integer,intent(in)  :: xtbl(0:*)
    integer j, jj
    jj = 0
    do j = 0, n - 1
       if (xtbl(j).ge.0) then
          dest(jj) = src(j)
          jj = jj + 1
       endif
    enddo
  end subroutine pack_mems
!!!_  & gen_stride
  subroutine gen_stride(strd, n, mem, xtbl)
    implicit none
    integer,intent(out)         :: strd(0:*)
    integer,intent(in)          :: n
    integer,intent(in)          :: mem(0:*)
    integer,intent(in),optional :: xtbl(0:*)   ! logical-physical exchanger
    integer :: j, jj
    integer :: t(0:n)
    if (n.le.0) return
    if (present(xtbl)) then
       t(0) = 1
       jj = 0
       do j = 0, n - 1
          if (xtbl(j).ge.0) then
             t(j+1) = mem(j) * t(j)
             jj = jj + 1
          else
             t(j+1) = t(j)
          endif
       enddo
       strd(jj:n-1) = 0
       do j = 0, n - 1
          jj = xtbl(j)
          if (jj.ge.0) then
             strd(jj) = t(j)
          endif
       enddo
    else
       j = 0
       strd(j) = 1
       do j = 0, n - 2
          strd(j+1) = strd(j) * max(1, mem(j))
       enddo
    endif
  end subroutine gen_stride
!!!_  & flat_index_l2p()
  PURE &
  integer function flat_index_l2p_be &
       & (lflat, lbgn, lend, n, pstr, pofs, pcyc) &
       &  result(pflat)
    implicit none
    integer,intent(in)          :: lflat
    integer,intent(in)          :: lbgn(0:*), lend(0:*)  ! logical range
    integer,intent(in)          :: n
    integer,intent(in)          :: pstr(0:*)
    integer,intent(in),optional :: pofs(0:*), pcyc(0:*)
    integer :: lidx(0:n-1)
    call logical_index_array(lidx, lflat, lbgn, lend, n)
    pflat = physical_index_flat(lidx, lbgn, lend, n, pstr, pofs, pcyc)
  end function flat_index_l2p_be
  PURE &
  integer function flat_index_l2p_mem &
       & (lflat, lmem, n, pstr, pofs, pcyc) &
       &  result(pflat)
    implicit none
    integer,intent(in)          :: lflat
    integer,intent(in)          :: lmem(0:*)
    integer,intent(in)          :: n
    integer,intent(in)          :: pstr(0:*)
    integer,intent(in),optional :: pofs(0:*), pcyc(0:*)
    integer :: lidx(0:n-1)
    call logical_index_array(lidx, lflat, lmem, n)
    ! write(*, *) 'lidx', lflat, lidx(0:n-1)
    pflat = physical_index_flat(lidx, lmem, n, pstr, pofs, pcyc)
  end function flat_index_l2p_mem

!!!_  & logical_index_array()
  PURE &
  subroutine logical_index_array_be(idx, flat, lbgn, lend, n)
    implicit none
    integer,intent(out) :: idx(0:*)
    integer,intent(in)  :: flat
    integer,intent(in)  :: lbgn(0:*), lend(0:*)
    integer,intent(in)  :: n
    integer :: lmem(0:n-1)

    lmem(0:n-1) = lend(0:n-1) - lbgn(0:n-1)
    call logical_index_array_mem(idx, flat, lmem, n)
  end subroutine logical_index_array_be
  PURE &
  subroutine logical_index_array_mem(idx, flat, lmem, n)
    implicit none
    integer,intent(out) :: idx(0:*)
    integer,intent(in)  :: flat
    integer,intent(in)  :: lmem(0:*)
    integer,intent(in)  :: n
    integer jf, j

    if (n.le.0) return

    jf = flat
    j = 0
    idx(j) = mod(jf, lmem(j))
    do j = 1, n - 1
       jf = jf / lmem(j-1)
       idx(j) = mod(jf, lmem(j))
    enddo
  end subroutine logical_index_array_mem
!!!_  & physical_index_flat()
  PURE &
  integer function physical_index_flat_mem &
       & (lidx, lmem, n, pstr, pofs, pcyc) &
       &  result(flat)
    implicit none
    integer,intent(in)          :: lidx(0:*)             ! logical indices
    integer,intent(in)          :: lmem(0:*)
    integer,intent(in)          :: n
    integer,intent(in)          :: pstr(0:*)
    integer,intent(in),optional :: pofs(0:*), pcyc(0:*)
    integer :: lbgn(0:n-1)
    lbgn(0:n-1) = 0
    flat = physical_index_flat_be(lidx, lbgn, lmem, n, pstr, pofs, pcyc)
  end function physical_index_flat_mem

  PURE &
  integer function physical_index_flat_be &
       & (lidx, lbgn, lend, n, pstr, pofs, pcyc) &
       &  result(flat)
    implicit none
    integer,intent(in)          :: lidx(0:*)             ! logical indices
    integer,intent(in)          :: lbgn(0:*), lend(0:*)  ! logical range
    integer,intent(in)          :: n
    integer,intent(in)          :: pstr(0:*)
    integer,intent(in),optional :: pofs(0:*), pcyc(0:*)
    integer po(0:n-1), pc(0:n-1)
    if (present(pofs)) then
       if (present(pcyc)) then
          flat = physical_index_flat_core(lidx, lbgn, lend, n, pstr, pofs, pcyc)
       else
          pc(0:n-1) = 0
          flat = physical_index_flat_core(lidx, lbgn, lend, n, pstr, pofs, pc)
       endif
    else if (present(pcyc)) then
       po(0:n-1) = 0
       flat = physical_index_flat_core(lidx, lbgn, lend, n, pstr, po, pcyc)
    else
       po(0:n-1) = 0
       pc(0:n-1) = 0
       flat = physical_index_flat_core(lidx, lbgn, lend, n, pstr, po, pc)
    endif
  end function physical_index_flat_be

!!!_  & physical_index_flat_core()
  PURE &
  integer function physical_index_flat_core &
       & (lidx, lbgn, lend, n, pstr, pofs, pcyc) &
       &  result(flat)
    implicit none
    integer,intent(in) :: lidx(0:*)                        ! logical indices
    integer,intent(in) :: lbgn(0:*), lend(0:*)             ! logical range
    integer,intent(in) :: n
    integer,intent(in) :: pofs(0:*), pstr(0:*), pcyc(0:*)  ! physical offsets, strides, cyclic flags

    integer j, jj

    flat = 0
    !NEC$ novector
    do j = 0, n - 1
       if (lbgn(j).le.lidx(j) .and. lidx(j).lt.lend(j)) then
          jj = pofs(j) + lidx(j)
          if (pcyc(j).gt.0) then
             flat = flat + modulo(jj, pcyc(j)) * pstr(j)
          else
             flat = flat + jj * pstr(j)
          endif
       else
          flat = -1
          exit
       endif
    enddo
  end function physical_index_flat_core

!!!_ + End Jmz_coor
end module Jmz_coor
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

!!!_! jmz_coor.F90 - TOUZA/Jmz coordinate (loop) manipulation
! Maintainer: SAITO Fuyuki
! Created: Oct 6 2023
#define TIME_STAMP 'Time-stamp: <2023/11/01 12:50:00 fuyuki jmz_coor.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2023
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
  use TOUZA_Std,only: KFLT, KDBL, KIOFS
  use TOUZA_Std,only: is_msglev
  use TOUZA_Std,only: msglev_DETAIL,    msglev_NORMAL,    msglev_INFO,    msglev_DEBUG
  use TOUZA_Std,only: msglev_WARNING
  use TOUZA_Std,only: is_msglev_DETAIL, is_msglev_NORMAL, is_msglev_INFO, is_msglev_DEBUG
  use TOUZA_Std,only: is_msglev_WARNING
  use TOUZA_Std,only: trace_err

  use TOUZA_Nio,only: litem
!!!_  - default
  implicit none
  private
!!!_  - i/o units
  integer,save :: ulog = -1
  integer,save :: uerr = -1
!!!_  - global flags
  integer,save :: lev_verbose = 0
  integer,save :: dbgv = -1
  integer,save :: stdv = -1

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
  integer(kind=KCO),parameter :: ZERO = 0_KCO
  integer(kind=KCO),parameter :: ONE  = 1_KCO
!!!_  - range special
  integer(kind=KCO),parameter,public :: full_range = + HUGE(ZERO)              ! case low:
  integer(kind=KCO),parameter,public :: null_range = (- HUGE(ZERO)) - ONE      ! case low (MUST BE NEGATIVE)

!!!_  - loop property
  type loop_t
     integer           :: flg = loop_unset
     integer(kind=KCO) :: bgn = - ONE
     integer(kind=KCO) :: end = - ONE
     integer(kind=KCO) :: ofs = ZERO
     integer(kind=KCO) :: cyc = - ONE        ! 0 if still effective empty
     character(len=lname) :: name
  end type loop_t

  type(loop_t),save,public :: def_loop  = loop_t(loop_unset, null_range, null_range, ZERO, -ONE, ' ')

  public :: loop_t

!!!_  - interfaces
  interface get_range_string
     module procedure get_range_string_t, get_range_string_c
  end interface get_range_string
!!!_  - public
  public :: init, finalize
  public :: get_range_string
  public :: is_null_coor, count_effective, coordinate_type
  public :: show_lpp
!!!_ + Procedures
contains
!!!_  - init
  subroutine init(ierr)
    implicit none
    integer,intent(out) :: ierr

    ierr = 0
  end subroutine init
!!!_  - finalize
  subroutine finalize(ierr, u)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u

    ierr = 0
  end subroutine finalize

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

!!!_ + End jmzlib
end module Jmz_coor
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

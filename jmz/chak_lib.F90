!!!_! chak_lib.F90 - TOUZA/Jmz CH(swiss) army knife library
! Maintainer: SAITO Fuyuki
! Created: Oct 13 2022
#define TIME_STAMP 'Time-stamp: <2023/06/28 16:46:57 fuyuki chak_lib.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022, 2023
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
#ifndef   OPT_DESC_LEN
#  define OPT_DESC_LEN 1024
#endif
#ifndef   OPT_CHAK_FILES
#  define OPT_CHAK_FILES    128
#endif
#ifndef   OPT_CHAK_BUFFERS
#  define OPT_CHAK_BUFFERS  256
#endif
#ifndef    OPT_CHAK_STACKS
#  define  OPT_CHAK_STACKS   512
#endif
!!!_@ TOUZA/Jmz/chak-lib - nio swiss army knife (library)
module chak_lib
!!!_ + Declaration
!!!_  - modules
  use jmzlib, jl_init=>init, jl_finalize=>finalize
  use TOUZA_Nio,only: litem, nitem, GFMT_END
  implicit none
  public
!!!_  - parameters
  integer,parameter :: KBUF = __KBUF

  integer,parameter :: lcoor = 6
  integer,parameter :: mcoor = 3    ! standard max coordinates

  real(kind=KBUF),parameter :: ZERO  = 0.0_KBUF
  real(kind=KBUF),parameter :: ONE   = 1.0_KBUF

  real(kind=KBUF),parameter :: TRUE  = ONE
  real(kind=KBUF),parameter :: FALSE = ZERO

  real(kind=KBUF),parameter :: ULIMIT = + HUGE(ZERO)
  real(kind=KBUF),parameter :: LLIMIT = - HUGE(ZERO)

  real(kind=KBUF),parameter :: UNDEF  = -999.0_KBUF
  ! real(kind=KBUF),parameter :: UNDEF  = LLIMIT

!!!_  - coordinate matching
  integer,parameter :: co_unset = -5
  integer,parameter :: co_del   = -4
  integer,parameter :: co_null  = -3
  integer,parameter :: co_ins   = -2
  integer,parameter :: co_wild  = -1
  integer,parameter :: co_normal = 0
!!!_  - file formats (except for gtool formats)
  integer,parameter :: cfmt_org = GFMT_END
  integer,parameter :: cfmt_ascii  = 1 + cfmt_org
  integer,parameter :: cfmt_binary = 2 + cfmt_org
  integer,parameter :: cfmt_flag_native = 0
  integer,parameter :: cfmt_flag_swap = 1
  integer,parameter :: cfmt_flag_big = 2
  integer,parameter :: cfmt_flag_little = 3
  integer,parameter :: cfmt_flags_bo = 4

  integer,parameter :: cfmt_binary_i4 = cfmt_binary
  integer,parameter :: cfmt_binary_r4 = cfmt_binary + cfmt_flags_bo
  integer,parameter :: cfmt_binary_r8 = cfmt_binary + cfmt_flags_bo * 2

  integer,parameter :: cfmt_cdf = cfmt_binary_r8 + 16
  ! integer,parameter :: cfmt_cdf_i4 = cfmt_cdf + 1
  ! integer,parameter :: cfmt_cdf_r4 = cfmt_cdf + 2
  ! integer,parameter :: cfmt_cdf_r8 = cfmt_cdf + 3

!!!_  - variable types
  integer,parameter :: kv_null = 0
  integer,parameter :: kv_int  = 1
  integer,parameter :: kv_flt  = 2
  integer,parameter :: kv_dbl  = 3

!!!_  - range special
  integer,parameter :: full_range = + HUGE(0)              ! case low:
  integer,parameter :: null_range = (- HUGE(0)) - 1        ! case low (MUST BE NEGATIVE)

!!!_  - character (symbols) for command-line
  character(len=*),parameter :: param_sep = '='
  character(len=*),parameter :: rename_sep = '/'
  character(len=*),parameter :: range_sep = ':'
  character(len=*),parameter :: item_sep = ','
  character(len=*),parameter :: rec_append_sep = '/'
  character(len=*),parameter :: rec_num_sep = '+'

  character(len=*),parameter :: insert_coor = '+'
  character(len=*),parameter :: delete_coor = '-'

  character(len=*),parameter :: shape_sweep_stack  = '+'
  character(len=*),parameter :: shape_sweep_accum  = '++'
  character(len=*),parameter :: shape_sweep_reduce = '='

!!!_  - character (symbols) for ascii output
  character(len=*),parameter :: amiss = '_'  ! character for missing value
  character(len=*),parameter :: aext  = '.'  ! character for external
!!!_  - string length
  integer,parameter :: lname = litem * 4
  integer,parameter :: ldesc = OPT_DESC_LEN

!!!_  - handles and types
  integer,parameter :: lfile   = OPT_CHAK_FILES
  integer,parameter :: lbuffer = OPT_CHAK_BUFFERS
  integer,parameter :: lstack  = OPT_CHAK_STACKS
  integer,parameter :: lopr    = 512
  integer,parameter :: oprlen  = 16

  integer,save :: mfile = 0
  integer,save :: mbuffer = 0
  integer,save :: mopr = 0

  integer,parameter :: lmodule = max(lopr, lfile, lbuffer, lstack) * 2
!!!_  - handle offsets and kinds
  integer,parameter :: hk_error = -1
  integer,parameter :: hk_opr = 0
  integer,parameter :: hk_file = 1
  integer,parameter :: hk_buffer = 2
  integer,parameter :: hk_anchor = 3
  integer,parameter :: hk_stack  = 4
  integer,parameter :: hk_overflow = 5

  integer,parameter :: ofs_opr    = lmodule * hk_opr
  integer,parameter :: ofs_file   = lmodule * hk_file
  integer,parameter :: ofs_buffer = lmodule * hk_buffer
  integer,parameter :: ofs_anchor = lmodule * hk_anchor
  integer,parameter :: ofs_stack  = lmodule * hk_stack

!!!_  - domain compromise mode for non-unary operations
  integer,parameter :: cmode_null      = 0
  integer,parameter :: cmode_each      = 1
  integer,parameter :: cmode_inclusive = 2
  integer,parameter :: cmode_intersect = 3
  integer,parameter :: cmode_first     = 4

  integer,parameter :: cmode_compromise = 7  ! mask

  integer,parameter :: cmode_xundef     = 8  ! exclude undefined at flushing
  integer,parameter :: cmode_column     = 16  ! columned

!!!_  - shape parser flag
  integer,parameter :: shape_error      = -1
  integer,parameter :: shape_element    = 0  ! interprete single integer as element    (SHAPE)
  integer,parameter :: shape_size       = 1  ! interprete single integer as size       (SIZE)
  integer,parameter :: shape_coordinate = 2  ! interprete single integer as coordinate (PERM)
  integer,parameter :: shape_reduction  = 3  ! interprete single integer as coordinate (reduction)
  integer,parameter :: shape_shift      = 4  ! interprete single integer as shift      (SHIFT)
!!!_  - coordinate(loop) type
  integer,parameter :: loop_error  = -2
  integer,parameter :: loop_unset  = -1
  integer,parameter :: loop_null   = 0       ! null coordinate  (expand to bgn:end)
  integer,parameter :: loop_reduce = 1       ! sweep coordinate (shrink to 0:1)
  integer,parameter :: loop_normal = 2

!!!_  - buffer status
  integer,parameter :: buf_used   = +1
  integer,parameter :: buf_free   = 0
  integer,parameter :: buf_locked = -1

!!!_  - common values
  real(kind=KBUF),save :: PI = ZERO
!!!_  - domain property
  type domain_t
     integer :: n                   ! total size
     integer :: mco                 ! coordinate size
     integer :: ofs(0:lcoor-1)
     integer :: cyc(0:lcoor-1)
     integer :: iter(0:lcoor-1)
     integer :: strd(0:lcoor)       ! with sentry
     integer :: bgn(0:lcoor-1)
     integer :: end(0:lcoor-1)
     integer :: cidx(0:lcoor-1)     ! coordinate index (physical)
     integer :: lidx(0:lcoor-1)     ! coordinate index (logical)
  end type domain_t
!!!_  - loop property
  type loop_t
     integer :: bgn = -1
     integer :: end = -1
     integer :: flg = loop_unset
     integer :: ofs = 0
     integer :: cyc = -1        ! 0 if still effective empty
     character(len=lname) :: name
  end type loop_t

  type(loop_t),save :: def_loop  = loop_t(null_range, null_range, loop_unset, 0, -1, ' ')

!!!_  - buffer property
  type buffer_t
     character(len=lname)    :: name              ! buffer name
     character(len=ldesc)    :: desc              ! description
     character(len=ldesc)    :: desc2             ! description (infix notation)
     integer                 :: ilev              ! infix notation level
     integer                 :: k = kv_null
     integer                 :: stt
     integer                 :: ncoor             ! (reserved) number of coordinate
     integer                 :: ci(0:lcoor-1)
     integer                 :: reff              ! reference file handle
     type(loop_t)            :: pcp(0:lcoor-1)    ! physical (source) coordinate properties
     real(kind=KBUF)         :: undef
     real(kind=KBUF),pointer :: vd(:) => NULL()
     integer,pointer         :: vi(:)
  end type buffer_t

!!!_  - buffer stack
  type stack_t
     integer      :: bh             ! buffer or stack handle
     type(loop_t) :: lcp(0:lcoor-1) ! logical (destination) coordinate properties
  end type stack_t

!!!_  - argument queue
  type queue_t
     integer :: term = -1            ! term handle
     integer :: nopr                 ! number of operands
     integer :: iter                 ! number of iterates etc
     integer :: cmode = cmode_null   ! operation mode
     character(len=ldesc)  :: desci
     character(len=ldesc)  :: desco
     type(loop_t) :: lcp(0:lcoor-1)  ! logical (destination) coordinate properties
     type(stack_t),pointer :: lefts(:)       ! result stack to push
     integer :: opt                  ! option for any use
  end type queue_t

!!!_ + Procedures
contains
!!!_  - initialization
!!!_   . init
  subroutine init(ierr)
    implicit none
    integer,intent(out) :: ierr

    ierr = 0
    if (ierr.eq.0) call jl_init(ierr)
    if (PI.eq.ZERO) PI = ATAN2(ZERO, -ONE)
  end subroutine init
!!!_   . finalize
  subroutine finalize(ierr, u)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u

    ierr = 0
    if (ierr.eq.0) call jl_finalize(ierr, u)
  end subroutine finalize
!!!_  - utilities
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
    character(len=64) :: pfx
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
111    format(A, 'loop', A, ': ', I0, 1x, A, 1x, I0, ':', I0, ':', I0, '+', I0, '/', I0)
       write(utmp, 111) repeat(' ', tab), trim(pfx), jc, trim(lpp(jc)%name), &
            & lpp(jc)%bgn, lpp(jc)%end, lpp(jc)%flg, lpp(jc)%ofs, lpp(jc)%cyc
    enddo
  end subroutine show_lpp
!!!_   . show_domain
  subroutine show_domain &
       & (ierr, dom, tag, u, levv, indent)
    use TOUZA_Std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    type(domain_t),  intent(in)          :: dom
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer,         intent(in),optional :: indent
    integer utmp
    integer lv
    integer tab
    character(len=64) :: pfx, cran, str
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
102 format(A, 'domain', A, ' total = ', I0)
103 format(A, 'domain', A, ':c ', I0, ' / = ', 6(1x, I0))
104 format(A, 'domain', A, ':l ', I0, ' / = ', 6(1x, I0))
    write(utmp, 102) repeat(' ', tab), trim(pfx), dom%n
    write(utmp, 103) repeat(' ', tab), trim(pfx), dom%mco, dom%cidx(0:dom%mco - 1)
    write(utmp, 104) repeat(' ', tab), trim(pfx), dom%mco, dom%lidx(0:dom%mco - 1)
    do jc = 0, dom%mco - 1
       call get_range_string(ierr, cran, dom%bgn(jc), dom%end(jc), loop_normal, c=dom%cyc(jc))
108    format('+', I0, '*', I0, ' (', I0, ')')
109    format('+', I0, '*', I0)
       if (dom%ofs(jc).eq.null_range) then
          write(str, 109) dom%strd(jc), dom%iter(jc)
       else
          write(str, 108) dom%strd(jc), dom%iter(jc), dom%ofs(jc)
       endif
111    format(A, 'domain', A, ': ', I0, 1x, A, 1x, A)
112    format(A, 'domain', A, ': ', I0, 1x, A, 1x, A, 1x, '{', I0, '}')
       if (dom%cyc(jc).gt.0) then
          write(utmp, 112) repeat(' ', tab), trim(pfx), jc, trim(cran), trim(str), dom%cyc(jc)
       else
          write(utmp, 111) repeat(' ', tab), trim(pfx), jc, trim(cran), trim(str)
       endif
    enddo
  end subroutine show_domain

!!!_   . diag_queue
  subroutine diag_queue &
       & (ierr, aq, tag, u, levv, indent)
    use TOUZA_Std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    type(queue_t),   intent(in)          :: aq
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer,         intent(in),optional :: indent
    integer utmp
    integer lv
    integer tab
    integer js, jh
    character(len=64) :: pfx, obj, spfx

    ierr = 0
    lv = choice(lev_verbose, levv)
    utmp = choice(ulog, u)
    tab = choice(0, indent)
    if (present(tag)) then
       pfx = '[' // trim(tag) // ']'
    else
       pfx = ' '
    endif
101 format(A, 'queue', A, ' = ', I0, 1x, I0, 1x, I0, 1x, I0, 1x, I0)
    write(utmp, 101) repeat(' ', tab), &
         & trim(pfx), aq%term, aq%nopr, aq%iter, aq%cmode, &
         & size(aq%lefts)
    if (ierr.eq.0) call show_lpp(ierr, aq%lcp, tag, utmp, lv, tab + 2)
    do js = 0, size(aq%lefts) - 1
121    format('buffer[', I0, ']')
122    format('stack[', I0, ']')
123    format('handle[', I0, ']')
       jh = buf_h2item(aq%lefts(js)%bh)
       if (jh.ge.0) then
          write(obj, 121) jh
       else
          jh = stack_h2item(aq%lefts(js)%bh)
          if (jh.ge.0) then
             write(obj, 122) jh
          else
             write(obj, 123) aq%lefts(js)%bh
          endif
       endif
111    format(A, ':', A)
112    format(A)
       if (present(tag)) then
          write(spfx, 111) trim(tag), trim(obj)
       else
          write(spfx, 112) trim(obj)
       endif
       if (ierr.eq.0) call show_lpp(ierr, aq%lefts(js)%lcp, spfx, utmp, lv, tab + 4)
    enddo
  end subroutine diag_queue

!!!_   . get_range_string
  subroutine get_range_string &
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
  end subroutine get_range_string

!!!_   . get_perm_string
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

!!!_   . get_domain_string
  subroutine get_domain_string(ierr, str, lpp, maxco)
    use TOUZA_Std,only: join_list, choice
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    type(loop_t),    intent(in)  :: lpp(0:*)
    integer,optional,intent(in)  :: maxco

    character(len=64) cstr(0:lcoor-1)
    character(len=64) :: cran

    integer jc, nc
    integer mc
    integer jp, ls
    integer jerr

    ierr = 0
    nc = 0
    mc = choice(lcoor, maxco)
    do jc = 0, mc - 1
       call get_range_string(ierr, cran, lpp(jc)%bgn, lpp(jc)%end, lpp(jc)%flg, lpp(jc)%ofs)
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

!!!_   . get_domain_perm
  subroutine get_domain_perm &
       & (ierr, str, dom, cname, ctype)
    use TOUZA_Std,only: join_list
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    type(domain_t),  intent(in)  :: dom
    character(len=*),intent(in)  :: cname(0:*)
    integer,         intent(in)  :: ctype(0:*)

    integer jodr, jco
    character(len=lname*2) :: cbuf(0:lcoor-1)

411 format(A, '/', I0)
    do jodr = 0, dom%mco - 1
       jco = dom%lidx(jodr)
       if (jco.lt.0) then
          cbuf(jodr) = '.'
       else if (ctype(jco).eq.co_null) then
          write(cbuf(jodr), 411, IOSTAT=ierr) '-', jco
       else if (ctype(jco).eq.co_wild) then
          write(cbuf(jodr), 411, IOSTAT=ierr) '*', jco
       else
          write(cbuf(jodr), 411, IOSTAT=ierr) trim(cname(jco)), jco
       endif
    enddo
    call join_list(ierr, str, cbuf(0:dom%mco-1), ldelim='[', rdelim=']')
  end subroutine get_domain_perm

!!!_   . get_domain_result
  subroutine get_domain_result &
       & (ierr, str, dom, pcp, cbgn, cend)
    use TOUZA_Std,only: join_list, choice
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    type(domain_t),  intent(in)  :: dom
    type(loop_t),    intent(in)  :: pcp(0:*)
    integer,optional,intent(in)  :: cbgn, cend

    integer jodr, jphyc
    character(len=lname)   :: cran
    character(len=lname*2) :: cbuf(0:lcoor-1)
    integer b, e, flg
    integer cb, ce

    ierr = 0
    str = ' '
    do jodr = 0, dom%mco - 1
       b = dom%bgn(jodr)
       e = dom%end(jodr)
       flg = pcp(jodr)%flg
       if (ierr.eq.0) call get_range_string(ierr, cran, b, e, flg)
       if (ierr.eq.0) then
          jphyc = dom%cidx(jodr)
101       format(A, '/', A)
102       format('/', A)
! 103       format('<', A, '>/', A)
          select case(jphyc)
          case(co_wild)
             write(cbuf(jodr), 101, IOSTAT=ierr) '*', trim(cran)
          case(co_null)
             write(cbuf(jodr), 101, IOSTAT=ierr) '-', trim(cran)
          case(0:)
             if (pcp(jodr)%name.eq.' ') then
                write(cbuf(jodr), 102, IOSTAT=ierr) trim(cran)
             else
                write(cbuf(jodr), 101, IOSTAT=ierr) trim(pcp(jodr)%name), trim(cran)
             endif
          case default
             cbuf(jodr) = trim(cran)
          end select
       endif
    enddo
    if (ierr.eq.0) then
       cb = choice(0, cbgn)
       ce = choice(dom%mco, cend)
       call join_list(ierr, str, cbuf(cb:ce-1))
    endif
  end subroutine get_domain_result
!!!_   . get_domain_shape
  subroutine get_domain_shape &
       & (ierr, str, dom, pcp, lcp, ref, cbgn, cend, ldelim, rdelim, sep)
    use TOUZA_Std,only: join_list, choice, choice_a
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(out)         :: str
    type(domain_t),  intent(in)          :: dom
    type(loop_t),    intent(in)          :: pcp(0:*), lcp(0:*)
    type(domain_t),  intent(in)          :: ref
    integer,optional,intent(in)          :: cbgn, cend
    character(len=*),intent(in),optional :: ldelim, rdelim, sep

    integer jodr, jphyc, jlogc
    character(len=lname)   :: cran
    character(len=lname*2) :: cbuf(0:lcoor-1)

    character(len=32) :: ld, rd, sp

    integer b, e, flg, odmy, cdmy
    integer cb, ce

    ierr = 0
    str = ' '
    do jodr = 0, dom%mco - 1
       if (ierr.eq.0) call get_logical_range(ierr, b, e, flg, odmy, cdmy, jodr, lcp, pcp, dom)
       if (ierr.eq.0) call get_range_string(ierr, cran, b, e, flg)
       if (ierr.eq.0) then
          jphyc = dom%cidx(jodr)
          jlogc = dom%lidx(jodr)
101       format(A, '/', A)
102       format('/', A)
103       format('<', A, '>/', A)
          select case(jphyc)
          case(co_wild)
             write(cbuf(jodr), 101, IOSTAT=ierr) '*', trim(cran)
          case(co_null)
             write(cbuf(jodr), 101, IOSTAT=ierr) '-', trim(cran)
          case(0:)
             if (pcp(jphyc)%name.eq.' ') then
                write(cbuf(jodr), 102, IOSTAT=ierr) trim(cran)
             else
                write(cbuf(jodr), 101, IOSTAT=ierr) trim(pcp(jphyc)%name), trim(cran)
             endif
          case default
             if (jlogc.ge.0) then
                write(cbuf(jodr), 103, IOSTAT=ierr) trim(lcp(jlogc)%name), trim(cran)
             else
                cbuf(jodr) = trim(cran)
             endif
          end select
       endif
    enddo
    if (ierr.eq.0) then
       cb = choice(0, cbgn)
       ce = choice(dom%mco, cend)
       call choice_a(ld, '[', ldelim)
       call choice_a(rd, ']', rdelim)
       call choice_a(sp, ' ', sep)
       call join_list(ierr, str, cbuf(cb:ce-1), sep=sp)
       if (ierr.eq.0) then
          if (ld.ne.' ') str = trim(ld) // trim(str)
          if (rd.ne.' ') str = trim(str) // trim(rd)
       endif
    endif
  end subroutine get_domain_shape

!!!_   . file_h2item()
  integer function file_h2item(handle) result(n)
    implicit none
    integer,intent(in) :: handle
    n = handle - ofs_file
    if (n.ge.0.and.n.lt.min(mfile, lfile)) then
       continue
    else
       n = ERR_INVALID_ITEM
    endif
  end function file_h2item
!!!_   . file_i2handle()
  integer function file_i2handle(item) result(n)
    implicit none
    integer,intent(in) :: item
    if (item.ge.0.and.item.lt.min(mfile, lfile)) then
       n = item + ofs_file
    else
       n = ERR_INVALID_ITEM
    endif
  end function file_i2handle
!!!_   . buf_h2item()
  ELEMENTAL integer function buf_h2item(handle) result(n)
    implicit none
    integer,intent(in) :: handle
    n = handle - ofs_buffer
    if (n.ge.0.and.n.lt.min(mbuffer, lbuffer)) then
       continue
    else
       n = ERR_INVALID_ITEM
    endif
  end function buf_h2item
!!!_   . buf_i2handle()
  integer function buf_i2handle(item) result(n)
    implicit none
    integer,intent(in) :: item
    if (item.ge.0.and.item.lt.min(mbuffer, lbuffer)) then
       n = item + ofs_buffer
    else
       n = ERR_INVALID_ITEM
    endif
  end function buf_i2handle

!!!_   . stack_h2item()
  ELEMENTAL integer function stack_h2item(handle) result(n)
    implicit none
    integer,intent(in) :: handle
    n = handle - ofs_stack
    if (n.ge.0.and.n.lt.lstack) then
       continue
    else
       n = ERR_INVALID_ITEM
    endif
  end function stack_h2item

!!!_   . stack_i2handle()
  integer function stack_i2handle(item) result(n)
    implicit none
    integer,intent(in) :: item
    if (item.ge.0.and.item.lt.lstack) then
       n = item + ofs_stack
    else
       n = ERR_INVALID_ITEM
    endif
  end function stack_i2handle

!!!_   . is_buffer()
  logical function is_buffer(handle) result(b)
    implicit none
    integer,intent(in) :: handle
    b = (handle_type(handle) .eq. hk_buffer)
  end function is_buffer

!!!_   . is_operator()
  logical function is_operator(handle) result(b)
    implicit none
    integer,intent(in) :: handle
    b = (handle_type(handle) .eq. hk_opr)
  end function is_operator

!!!_   . is_anchor()
  logical function is_anchor(handle) result(b)
    implicit none
    integer,intent(in) :: handle
    b = (handle_type(handle) .eq. hk_anchor)
  end function is_anchor

!!!_   . handle_type
  integer function handle_type(handle) result(k)
    implicit none
    integer,intent(in) :: handle

    if (handle.lt.0) then
       k = hk_error
    else
       k = handle / lmodule
       if (k.ge.hk_overflow) k = hk_error
    endif
    return
  end function handle_type

!!!_  - index function
!!!_   . conv_physical_index
  PURE &
  integer function conv_physical_index (jlog, domL, domR) result(n)
    implicit none
    integer,       intent(in) :: jlog
    type(domain_t),intent(in) :: domL, domR
    integer jc
    integer jcur, ncur
    n = 0
    ncur = jlog
    !NEC$ novector
    do jc = 0, domL%mco - 1
       jcur = mod(ncur, domL%iter(jc))
       ncur = ncur / domL%iter(jc)
       if (domR%bgn(jc).le.jcur.and.jcur.lt.domR%end(jc)) then
          jcur = domR%ofs(jc) + jcur
          if (domR%cyc(jc).gt.0) then
             n = n + modulo(jcur, domR%cyc(jc)) * domR%strd(jc)
          else
             n = n + jcur * domR%strd(jc)
          endif
       else
          n = -1
          exit
       endif
    enddo
  end function conv_physical_index

!!!_   . user_index_bgn()
  ELEMENTAL integer function user_index_bgn(j, n) result(k)
    implicit none
    integer,intent(in)          :: j
    integer,intent(in),optional :: n
    if (j.eq.full_range .or. j.eq.null_range) then
       k = j
    else
       k = j + user_offset_bgn
    endif
  end function user_index_bgn
!!!_   . user_index_end()
  ELEMENTAL integer function user_index_end(j, n) result(k)
    implicit none
    integer,intent(in)          :: j
    integer,intent(in),optional :: n
    if (j.eq.full_range .or. j.eq.null_range) then
       k = j
    else
       k = j + user_offset_end
    endif
  end function user_index_end
!!!_   . system_index_bgn()
  ELEMENTAL integer function system_index_bgn(j, n) result(k)
    implicit none
    integer,intent(in)          :: j
    integer,intent(in),optional :: n
    if (j.eq.full_range .or. j.eq.null_range) then
       k = j
    else
       k = j - user_offset_bgn
    endif
  end function system_index_bgn
!!!_   . system_index_end()
  ELEMENTAL integer function system_index_end(j, n) result(k)
    implicit none
    integer,intent(in)          :: j
    integer,intent(in),optional :: n
    if (j.eq.full_range .or. j.eq.null_range) then
       k = j
    else
       k = j - user_offset_end
    endif
  end function system_index_end
!!!_   . logical_index
  ELEMENTAL integer function logical_index (l, p) result(n)
    implicit none
    integer,intent(in) :: l, p
    if (l.eq.null_range) then
       n = p
    else
       n = l
    endif
  end function logical_index
!!!_   . physical_index
  PURE &
  integer function physical_index (lidx, dom) result(n)
    implicit none
    integer,       intent(in) :: lidx(0:*)
    type(domain_t),intent(in) :: dom
    integer jc, jj
    n = 0
    do jc = 0, dom%mco - 1
       if (dom%bgn(jc).le.lidx(jc).and.lidx(jc).lt.dom%end(jc)) then
          jj = dom%ofs(jc) + lidx(jc)
          if (dom%cyc(jc).gt.0) then
             n = n + modulo(jj, dom%cyc(jc)) * dom%strd(jc)
          else
             n = n + jj * dom%strd(jc)
          endif
       else
          n = -1
          exit
       endif
    enddo
  end function physical_index

!!!_   . incr_logical_index
  subroutine incr_logical_index(idx, dom, step)
    use TOUZA_Std,only: choice
    implicit none
    integer,         intent(inout) :: idx(0:*)
    type(domain_t),  intent(in)    :: dom
    integer,optional,intent(in)    :: step
    integer jc, k
    k = choice(1, step)
    do jc = 0, dom%mco - 1
       idx(jc) = idx(jc) + k
       k = idx(jc) / dom%iter(jc)
       idx(jc) = mod(idx(jc), dom%iter(jc))
    enddo
  end subroutine incr_logical_index

!!!_   . get_logical_range
  subroutine get_logical_range &
       & (ierr, b, e, flg, osh, cyc, jodr, lcp, pcp, dom, ref)
    integer,       intent(out)         :: ierr
    integer,       intent(out)         :: b, e, flg, osh, cyc
    integer,       intent(in)          :: jodr
    type(loop_t),  intent(in)          :: lcp(0:*)
    type(loop_t),  intent(in)          :: pcp(0:*)
    type(domain_t),intent(in)          :: dom
    type(domain_t),intent(in),optional :: ref

    integer low,   high
    integer jlogc, jphyc
    integer bp,    ep
    integer lflg
    character(len=128) :: txt

    ierr = 0
    if (present(ref)) then
       low  = ref%bgn(jodr)
       high = ref%end(jodr)
    else
       low = null_range
       high = null_range
    endif
    ! write(*, *) 'glr/0:', low, high

    jlogc = dom%lidx(jodr)
    jphyc = dom%cidx(jodr)
    if (jlogc.ge.0) then
       osh = lcp(jlogc)%ofs
       cyc = lcp(jlogc)%cyc
       lflg = lcp(jlogc)%flg
    else
       osh = 0
       cyc = 0
       lflg = loop_unset
    endif
    ! if (cyc.gt.0) write(*, *) 'glr/0', low, high
    if (jlogc.ge.0) then
       b = lcp(jlogc)%bgn
       e = lcp(jlogc)%end
       if (cyc.le.0) then
          if (b.ne.null_range) b = b + osh
          if (e.ne.null_range) e = e + osh
       endif
       b = logical_index(b, low)
       e = logical_index(e, high)
       ! b = logical_index(lcp(jlogc)%bgn + lcp(jlogc)%ofs, low)
       ! e = logical_index(lcp(jlogc)%end + lcp(jlogc)%ofs, high)
    else
       b = low
       e = high
    endif
    ! if (cyc.gt.0) write(*, *) 'glr/1', b, e
    ! write(*, *) 'glr/1:', b, e, cyc, osh

    flg = loop_unset
    if (jphyc.ge.0) then
       ! write(*, *) b, e, pcp(jphyc)%bgn, pcp(jphyc)%end, pcp(jphyc)%flg
       bp = pcp(jphyc)%bgn
       ep = pcp(jphyc)%end
       flg = pcp(jphyc)%flg
       if (flg.eq.loop_reduce) then
          bp = 0
          ep = 0
       endif
       if (flg.eq.loop_reduce.and.lflg.eq.loop_reduce) then
          b = bp
          e = ep
       else if (flg.ge.loop_null) then  ! if pcp is set
          if (cyc.le.0) then
             if (b.eq.null_range) b = bp + osh
             if (e.eq.null_range) e = ep + osh
          else
             if (b.eq.null_range) b = bp
             if (e.eq.null_range) e = ep
          endif
       endif
       if (cyc.gt.0) then
          if (flg.eq.loop_normal.and.cyc.gt.ep) then
             ierr = ERR_INVALID_PARAMETER
101          format('too much cyclic length at ', I0, ': ', I0, ' > ', I0, ':', I0)
             write(txt, 101) jphyc, cyc, bp, ep
             call message(ierr, txt)
          endif
       endif
    endif
    ! if (jlogc.ge.0) then
    !    if (lcp(jlogc)%flg.eq.loop_reduce) then
    !       write(*, *) 'glr/reduce', jodr, jlogc, b, e, flg
    !    endif
    ! endif
    ! if (cyc.gt.0) write(*, *) 'glr/9', b, e
    ! write(*, *) 'glr/9:', b, e
  end subroutine get_logical_range

!!!_   . settle_output_domain
  subroutine settle_output_domain(ierr, dom)
    implicit none
    integer,       intent(out)   :: ierr
    type(domain_t),intent(inout) :: dom

    integer jo

    ierr = 0
    if (ierr.eq.0) then
       dom%strd(0) = 1
       do jo = 1, dom%mco
          dom%strd(jo) = dom%strd(jo-1) * max(1, dom%end(jo-1) - dom%bgn(jo-1))
       enddo
       dom%n = dom%strd(dom%mco)
       do jo = 0, dom%mco - 1
          dom%strd(jo) = dom%strd(jo) * min(1, max(0, dom%end(jo) - dom%bgn(jo)))
       enddo
       do jo = 0, dom%mco - 1
          dom%end(jo) = max(dom%end(jo), 1 + dom%bgn(jo))
          ! dom%end(jo) = max(dom%end(jo), dom%bgn(jo))
          dom%iter(jo) = max(1, dom%end(jo) - dom%bgn(jo))
       enddo
    endif
  end subroutine settle_output_domain

!!!_   . adjust_reduce_domain
  subroutine adjust_reduce_domain(ierr, domL, domR, pcp, lcp)
    implicit none
    integer,       intent(out)   :: ierr
    type(domain_t),intent(inout) :: domL  ! domain for loop
    type(domain_t),intent(inout) :: domR  ! domain for reduction
    type(loop_t),  intent(inout) :: pcp(0:*)  ! physical coordinate to store reduction coordintate
    type(loop_t),  intent(in)    :: lcp(0:*)  ! logical coordinate

    integer jo, jc
    integer nx, nc
    integer b, e, flg, osh, cyc

    ierr = 0
    ! if (ierr.eq.0) call show_domain(ierr, domL, 'adjust/L0', indent=3)  ! range
    ! if (ierr.eq.0) call show_domain(ierr, domR, 'adjust/R0', indent=5)  ! no change

    if (ierr.eq.0) then
       nc = domR%mco
       nx = count(lcp(0:nc-1)%flg.eq.loop_reduce)
       do jo = 0, nc - 1
          pcp(jo)%bgn = domL%bgn(jo)
          pcp(jo)%end = domL%end(jo)
          pcp(jo)%flg = loop_null
          domR%bgn(jo) = 0
          domR%end(jo) = domL%end(jo) - domL%bgn(jo)
          domR%strd(jo) = 1
       enddo
       do jo = 0, nc - 1
          jc = domR%cidx(jo)
          ! write(*, *) '--- ', jc, ' ---'
          ! write(*, *) '    ', jo, domL%cidx(jo)
          ! write(*, *) '   l', lcp(jo)%bgn,  lcp(jo)%end,  lcp(jo)%flg
          if (jc.ge.0) then
             if (ierr.eq.0) then
                call get_logical_range(ierr, b, e, flg, osh, cyc, jo, lcp, pcp, domR)
             endif
             ! write(*, *) '   p', pcp(jc)%bgn,  pcp(jc)%end,  pcp(jc)%flg
             ! write(*, *) '   L', domL%bgn(jc), domL%end(jc)
             ! write(*, *) '   G', b, e, flg, osh, cyc
             if (ierr.eq.0) then
                domL%bgn(jc) = b
                domL%end(jc) = e
                domR%bgn(jc) = 0
                domR%end(jc) = e - b
                if (nx.eq.0.or.lcp(jo)%flg.eq.loop_reduce) then
                   domR%strd(jc) = 0
                   pcp(jc)%flg = loop_reduce
                else
                   domR%strd(jc) = 1
                   pcp(jc)%flg = loop_null
                endif
             endif
          endif
       enddo
    endif
    if (ierr.eq.0) then
       domL%strd(0) = 1
       do jo = 1, domL%mco
          domL%strd(jo) = domL%strd(jo-1) * max(1, domL%end(jo-1) - domL%bgn(jo-1))
       enddo
       domL%n = domL%strd(domL%mco)
       do jo = 0, domL%mco - 1
          domL%strd(jo) = domL%strd(jo) * min(1, max(0, domL%end(jo) - domL%bgn(jo)))
       enddo
       do jo = 0, domL%mco - 1
          domL%end(jo)  = max(domL%end(jo), 1 + domL%bgn(jo))
          domL%iter(jo) = max(1, domL%end(jo) - domL%bgn(jo))
       enddo
       domR%n = 1
       do jo = 1, nc
          jc = jo - 1
          if (domR%strd(jc).gt.0) then
             domR%strd(jc) = domR%n
             domR%n = domR%n * max(1, domR%end(jc) - domR%bgn(jc))
          endif
       enddo
       domR%cidx(0:nc-1) = domL%cidx(0:nc-1)
       domR%strd(nc) = domR%n
       domR%iter(0:nc-1) = domL%iter(0:nc-1)
       domR%ofs(0:nc-1) = 0
       domR%cyc(0:nc-1) = 0
    endif

    ! if (ierr.eq.0) call show_domain(ierr, domL, 'adjust/L9', indent=3)  ! range
    ! if (ierr.eq.0) call show_domain(ierr, domR, 'adjust/R9', indent=5)  ! no change
  end subroutine adjust_reduce_domain

!!!_   . settle_domain_stride
  subroutine settle_domain_stride &
       & (ierr, dom, pcp)
    implicit none
    integer,       intent(out)   :: ierr
    type(domain_t),intent(inout) :: dom
    type(loop_t),  intent(in)    :: pcp(0:*)

    integer jo, jc, w, stp
    integer strd(0:dom%mco)

    ierr = 0
    if (dom%mco.eq.0) then
       dom%n = 1
    else
       dom%n = max(1, dom%iter(0))
       do jo = 1, dom%mco - 1
          dom%n = dom%n * max(1, dom%iter(jo))
       enddo
    endif

    strd(0) = 1
    do jc = 1, dom%mco
       w = pcp(jc-1)%end - pcp(jc-1)%bgn
       ! stp = min(1, max(0, pcp(jc-1)%flg))
       ! 0 if null, sweep, 1 if normal
       stp = min(2, max(1, pcp(jc-1)%flg)) - 1
       strd(jc) = strd(jc-1) * max(1, w * stp)
    enddo
    do jo = 0, dom%mco - 1
       jc = dom%cidx(jo)
       if (jc.ge.0) then
          ! stp = min(1, max(0, pcp(jc)%flg))
          stp = min(2, max(1, pcp(jc)%flg)) - 1
          dom%strd(jo) = strd(jc) * stp
       else
          dom%strd(jo) = 0
       endif
    enddo
  end subroutine settle_domain_stride

!!!_  - coordinate manipulation
!!!_   . decompose_coordinate_mod
  subroutine decompose_coordinate_mod &
       & (ierr, jrep, lpp, arg, flag)
    use TOUZA_Std,only: split_list, condop, find_next_sep, parse_number
    implicit none
    integer,         intent(out)            :: ierr
    integer,         intent(out)            :: jrep     ! name/rename as (1:jrep)
    type(loop_t),    intent(inout),optional :: lpp
    character(len=*),intent(in)             :: arg
    integer,         intent(in)             :: flag     ! prefered style
    integer larg, lsep
    integer js0, js1
    integer,parameter:: rmem = 4
    integer rpos(rmem)
    integer,parameter :: rdef(rmem) = (/null_range, null_range, 0, 0/)
    integer nc
    integer jran
    logical no_range
    integer itmp
    character(len=*),parameter :: cdigits  = '0123456789'
    character(len=*),parameter :: csymbols = '+-'

    ! default rename_sep = '/'

    ! NAME/REPL//RANGE   == NAME/REPL/  RANGE      / before RANGE is absorbed
    ! NAME/REPL/RANGE    == NAME/REPL   RANGE
    ! NAME//RANGE        == NAME/       RANGE
    ! NAME/RANGE         == NAME        RANGE
    ! NAME/REPL/                                   no / absorption
    ! NAME/REPL
    !  NAME REPL  alpha+alnum
    !  RANGE      [num][:[num[:[num]]]]   begin:end:shift:cycle

    ierr = 0
    lsep = len(rename_sep)
    larg = len_trim(arg)

    no_range = .not.present(lpp)

    js0 = index(arg, rename_sep)
    jran = -1
    if (js0.gt.0) then
       ! first /
       js0 = js0 + lsep
       js1 = index(arg(js0:), rename_sep)
       if (js1.gt.0) then
          ! second /
          jran = js0 - 1 + js1 + lsep
          jrep = jran - lsep
          if (jran.le.larg) then
             if (arg(jran:jran+lsep-1).eq.rename_sep) then
                jran = jran + lsep
             else
                jrep = jrep - lsep
             endif
          endif
       endif
    else if (flag.eq.shape_coordinate.or.flag.eq.shape_reduction) then
       call parse_number(ierr, itmp, arg(1:larg))
       if (ierr.eq.0) then
          js0  = larg
          jrep = larg
          jran = larg + 1
       else
          ierr = 0
          js0 = 1
       endif
    else
       js0 = 1
    endif
    if (no_range) then
       if (jran.lt.0) jrep = larg
       return
    endif

    if (jran.lt.0) then
       if (index(arg(js0:larg), range_sep).eq.0) then
          call parse_number(ierr, itmp, arg(js0:larg))
          if (ierr.eq.0) then
             jran = js0
             jrep = jran - lsep - lsep
          else
             ierr = 0
             jran = larg + 1
             jrep = larg
          endif
       else
          jran = js0
          jrep = jran - lsep - lsep
       endif
    endif

    rpos(1) = system_index_bgn(null_range)
    rpos(2) = system_index_end(null_range)
    rpos(3) = 0
    rpos(4) = 0
    lpp = def_loop
    call split_list(nc, rpos, arg(jran:larg), range_sep, rmem, rdef(:))
    if (nc.lt.0) then
       ierr = nc
       call message(ierr, 'cannot parse range: ' // trim(arg(jran:larg)))
    else if (nc.eq.0) then
       lpp%bgn = system_index_bgn(rpos(1))
       lpp%end = system_index_end(rpos(2))
    else if (nc.eq.1) then
       if (flag.eq.shape_size) then
          ! force null coordinate if size==0
          lpp%bgn = 0
          lpp%end = rpos(1)
          lpp%flg = condop(rpos(1).eq.0, loop_null, loop_normal)
       else if (flag.ge.shape_shift) then
          lpp%ofs = rpos(1)
       else
          lpp%bgn = system_index_bgn(rpos(1))
          lpp%end = lpp%bgn + 1
          lpp%flg = loop_normal
       endif
    else if (nc.eq.2 .and. flag.ge.shape_shift) then
       lpp%ofs = rpos(1)
       lpp%cyc = rpos(2)
    else if (nc.lt.5) then
       lpp%bgn = system_index_bgn(rpos(1))
       lpp%end = system_index_end(rpos(2))
       lpp%flg = loop_normal
       if (lpp%end.ne.null_range .and. lpp%end.le.lpp%bgn) lpp%flg = loop_null
       if (nc.ge.3) lpp%ofs = rpos(3)
       if (nc.ge.4) lpp%cyc = rpos(4)
    else
       ierr = ERR_INVALID_PARAMETER
       call message(ierr, 'fail to extract range ' // trim(arg(jran:larg)))
    endif
    ! write(*, *) jrep, jran, '{' // arg(1:jrep) // '}{' // arg(jran:larg) // '} '
  end subroutine decompose_coordinate_mod

!!!_   . parse_format_shape
  subroutine parse_format_shape &
       & (nco, cname, irange, mco, fmt)
    use TOUZA_Std,only: find_next_sep
    implicit none
    integer,         intent(out) :: nco
    character(len=*),intent(out) :: cname(0:*)
    integer,         intent(out) :: irange(2, 0:*)
    integer,         intent(in)  :: mco
    character(len=*),intent(in)  :: fmt
    character(len=*),parameter :: csep = item_sep
    character(len=*),parameter :: rsep = range_sep
    integer jp, je, jr
    integer lf
    integer jerr
    type(loop_t) :: lpp

    jerr = 0
    lf = len_trim(fmt)
    nco = 0
    jp = 0
    do
       if (jp.ge.lf) exit
       je = find_next_sep(fmt, csep, jp)
       if (jerr.eq.0) call decompose_coordinate_mod(jerr, jr, lpp, fmt(jp+1:je), shape_size)
       if (jerr.eq.0) then
          if (lpp%flg.le.loop_null) then
             irange(1:2, nco) = (/0, 0/)
          else
             irange(1:2, nco) = (/lpp%bgn, lpp%end/)
          endif
          cname(nco) = fmt(jp+1:jp+jr)
       endif
       if (irange(2,nco).gt.0) irange(1,nco) = irange(1,nco) + 1
       nco = nco + 1
       jp = je + len(csep)
    enddo
    if (jerr.eq.0) then
       if (nco.gt.mco) then
          nco = ERR_INVALID_PARAMETER
          call message(nco, 'too many coordinates')
       endif
    endif
  end subroutine parse_format_shape

!!!_   . get_logical_shape
  subroutine get_logical_shape &
       & (ierr, nrphy, cname, ctype, cpidx, lcp, pcp, mco)
    use TOUZA_Std,only: find_first, parse_number, begin_with
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: nrphy        ! array ranks (physical)
    character(len=*),intent(out) :: cname(0:*)   ! coordinate name
    integer,         intent(out) :: ctype(0:*)   ! coordinate type (unset null wild normal)
    integer,         intent(out) :: cpidx(0:*)   ! corresponding (physical) index
    type(loop_t),    intent(in)  :: lcp(0:*), pcp(0:*)
    integer,         intent(in)  :: mco

    ! RANGE is parsed at decompose_coordinate_mod()

    ! NAME/REPL//RANGE   == NAME/REPL/  RANGE      slash (/) before RANGE is absorbed
    ! NAME/REPL/RANGE    == NAME/REPL   RANGE
    ! NAME///RANGE       == NAME//      RANGE
    ! NAME//RANGE        == NAME/       RANGE
    ! NAME/RANGE         == NAME        RANGE
    !  NAME REPL  alpha+alnum
    !  RANGE      [num][:[num]]

    ! NAME        order NAME (insert if new)
    ! NAME/       order NAME (error if new)
    ! NAME/REPL   order NAME and rename to REPL
    ! NAME//      order NAME and rename to blank (==wildcard)
    ! (idx)       not parsed (interpreted as bgn)
    ! idx/        idx coordinate (error if new)
    ! idx/REPL    idx coordinate and rename to REPL
    ! idx//       order NAME and rename to blank
    ! +           insert null(dummy) rank
    ! +[/]REPL    insert empty rank with REPL name
    ! -           delete null(dummy) rank and shift
    ! /           keep the order, same name
    ! //          keep the order and rename to blank
    ! (null)      no touch
    ! bgn[:[end]]
    integer jco
    integer nranks, nins, nskp
    integer jodr, jnull
    character(len=lname) :: cold, crep
    logical xold, xrep
    integer jerr
    integer tblp2l(0:mco-1)
    character(len=*),parameter :: delete_coor_cont = delete_coor // delete_coor
    character(len=*),parameter :: delete_coor_full = delete_coor_cont // delete_coor
    logical ceff

    ierr = 0
    tblp2l(:) = co_unset
    cpidx(0:mco-1) = co_unset
    cname(0:mco-1) = ' '
    ctype(0:mco-1) = co_unset
    ! count ranks
    nranks = 1
    do jco = mco - 1, 0, -1
       if (pcp(jco)%flg.ge.loop_null.or.pcp(jco)%name.ne.' ') then
          nranks = jco + 1
          exit
       endif
    enddo
    ! parse argument
    nins = 0
    do jodr = 0, mco - 1
       if (ierr.eq.0) then
          call parse_coordinate_repl(ierr, cold, xold, crep, xrep, lcp(jodr)%name)
          if (cold.eq.insert_coor) then
             jco = co_ins
             cold = ' '
          else if (begin_with(cold, delete_coor)) then
             if (cold.eq.delete_coor_full) then
                do jco = 0, mco - 1
                   if (is_null_coor(pcp(jco))) tblp2l(jco) = co_del
                enddo
             else if (cold.eq.delete_coor_cont) then
                do jco = jodr, mco - 1
                   if (is_null_coor(pcp(jco))) then
                      tblp2l(jco) = co_del
                   else
                      exit
                   endif
                enddo
                cycle
             else if (cold.eq.delete_coor) then
                if (is_null_coor(pcp(jodr))) tblp2l(jodr) = co_del
             else
                ierr = ERR_INVALID_PARAMETER
                call message(ierr, 'invalid argument: ' // trim(lcp(jodr)%name))
                exit
             endif
             cycle
          else if (cold.eq.' ') then
             if (xold) then
                jco = jodr
             else
                jco = co_null
             endif
          else
             call parse_number(jerr, jco, cold)
             if (jerr.eq.0) then
                jco = system_index_bgn(jco)
                if (jco.lt.0.or.jco.ge.nranks) then
                   jco = co_unset
                else
                   cold = pcp(jco)%name
                endif
             else
                jco = find_first(pcp(0:nranks-1)%name, cold, offset=0)
             endif
             if (jco.lt.0) then
                if (xold) then
                   ierr = ERR_INVALID_PARAMETER
                   call message(ierr, 'no coordinate ' // cold)
                   exit
                else
                   jco = co_ins
                   nins = nins + 1
                endif
             endif
          endif
          if (jco.ge.0) then
             tblp2l(jco) = jodr
             cpidx(jodr) = jco
             cname(jodr) = pcp(jco)%name
          else if (jco.eq.co_ins) then
             cpidx(jodr) = jco
             cname(jodr) = cold
          endif
          if (xrep) cname(jodr) = crep
! 101       format('parse_shape:', I0, 1x, I0, ' [', A, '] > ', '[', A, ',', L1,'][', A, ',', L1, ']')
!           write(*, 101) jodr, jco, trim(lcp(jodr)%name), trim(cold), xold, trim(crep), xrep
       endif
    enddo
    if (ierr.eq.0) then
       ! adjust unset
       jnull = 0
       nskp = 0
       jodr = 0
       do jco = 0, nranks - 1
          if (tblp2l(jco).eq.co_unset) then
             if (is_null_coor(pcp(jco))) then
                nskp = nskp + 1
                if (nskp.le.nins) then
                   jnull = find_first(cpidx(jnull:mco-1), co_null, offset=jnull, no=mco)
                   if (jnull.lt.mco) then
                      tblp2l(jco) = jnull
                      cpidx(jnull) = jco
                      jnull = jnull + 1
                   endif
                   cycle
                endif
             endif
             jodr = find_first(cpidx(jodr:mco-1), co_unset, offset=jodr)
             if (jodr.lt.0) then
                ierr = ERR_INSUFFICIENT_BUFFER
                call message(ierr, 'overflow in permutation:' // trim(pcp(jco)%name))
                exit
             endif
             tblp2l(jco) = jodr
             cpidx(jodr) = jco
             cname(jodr) = pcp(jco)%name
             jodr = jodr + 1
          endif
       enddo
    endif

    nrphy = 0
    if (ierr.eq.0) then
       ! adjust coordinate types
       do jodr = 0, mco - 1
          if (cpidx(jodr).ge.0) then
             ctype(jodr) = coordinate_type(cname(jodr), lcp(jodr), pcp(cpidx(jodr)))
             ! write(*, *) 'adjust', jodr, ctype(jodr), pcp(cpidx(jodr))%cyc
             ceff = pcp(cpidx(jodr))%cyc.ge.0
          else
             ctype(jodr) = coordinate_type(cname(jodr), lcp(jodr))
             ceff = .TRUE.
          endif
          if ((cpidx(jodr).ge.0.or.ctype(jodr).ne.co_null) .and. ceff) nrphy = jodr + 1
       enddo
    endif

  end subroutine get_logical_shape

!!!_   . match_perms
  subroutine match_perms &
       & (ierr,   nceff,  cnameL, clidxR, &
       &  nrankR, ctypeR, cnameR, mco,    nbuf, ltbl)
    use TOUZA_Std,only: choice, find_first, join_list
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: mco
    integer,         intent(out) :: nceff
    character(len=*),intent(out) :: cnameL(0:*)
    integer,         intent(out) :: clidxR(0:mco-1, 0:*)
    integer,         intent(in)  :: nrankR(0:*)
    integer,         intent(in)  :: ctypeR(0:mco-1, 0:*)
    character(len=*),intent(in)  :: cnameR(0:mco-1, 0:*)
    integer,         intent(in)  :: nbuf
    integer,optional,intent(in)  :: ltbl

    integer jbref
    integer jodr, jco, jt
    integer jb
    integer mref, meff(0:nbuf-1)
    integer gidx(0:mco-1, 0:nbuf-1)
    integer jup, jcur, jlast, joref
    integer ctref(0:mco-1)
    integer nlim, nmem
    ! integer jerr
    ! character(len=256) :: str

    integer nt
    integer,save :: mtbl = -1
    integer,             allocatable,save :: cout(:, :), nin(:), jbuf(:), jpack(:)
    character(len=lname),allocatable,save :: cbuf(:)
    character(len=128) :: str
    integer jerr

    ierr = 0
    nceff = -1
    if (nbuf.lt.1) then
       ierr = ERR_PANIC
       call message(ierr, 'empty buffers in permutation matching')
       return
    endif
    nt = choice(0, ltbl)
    if (nt.lt.mco) nt = mco * 3
    if (mtbl.lt.nt) then
       if (mtbl.ge.0) then
          deallocate(cout, nin, cbuf, jbuf, jpack, STAT=ierr)
          if (ierr.ne.0) return
       endif
       mtbl = nt
       allocate(cout(0:mtbl-1,0:mtbl-1), nin(0:mtbl-1), cbuf(0:mtbl-1), &
            &   jbuf(0:mtbl-1),          jpack(0:mtbl-1), &
            & STAT=ierr)
       if (ierr.ne.0) then
          ierr = ERR_ALLOCATION
          call message(ierr, 'allocation failed.')
          return
       endif
    endif
    cout(:,:) = -1
    nin(:) = -1
    cbuf(:) = ' '

    jbref = 0
    do jb = 0, nbuf - 1
       meff(jb) = count_coordinates(ctypeR(:, jb), cnameR(:, jb), mco)
       meff(jb) = max(meff(jb), nrankR(jb))
    enddo

    mref = maxval(meff(0:nbuf-1))
    ! set reference priority table
    jb = jbref
    nt = 0
    jup = -1
    do jodr = 0, meff(jb) - 1
       cbuf(nt) = cnameR(jodr, jb)
       if (jup.ge.0) cout(nt, jup) = nt
       gidx(jodr, jb) = nt
       jup = nt
       nt = nt + 1
    enddo
    do jodr = meff(jb), mref - 1
       cbuf(nt) = ' '
       if (jup.ge.0) cout(nt, jup) = nt
       gidx(jodr, jb) = nt
       jup = nt
       nt = nt + 1
    enddo
    ctref(0:mco-1) = ctypeR(0:mco-1, jb)
    do jb = 1, nbuf - 1
       do jodr = 0, meff(jb) - 1
          if (ctypeR(jodr, jb).eq.co_wild.and.ctref(jodr).eq.co_null) ctref(jodr) = co_wild
       enddo
    enddo
    ! set other priority table
    other: do jb = 1, nbuf - 1
       do jodr = 0, meff(jb) - 1
          select case(ctypeR(jodr, jb))
          case(co_normal)
             if (ctref(jodr).eq.co_wild) then
                jcur = jodr
             else
                jcur = find_first(cbuf(0:nt-1), cnameR(jodr, jb), offset=0)
                if (jcur.lt.0) then
                   jcur = nt
                   nt = nt + 1
                   if (nt.gt.mtbl) then
                      ierr = ERR_INSUFFICIENT_BUFFER
                      exit other
                   endif
                   cbuf(jcur) = cnameR(jodr, jb)
                endif
             endif
          case(co_wild)
             jcur = jodr
          case(co_null)
             jcur = -1
          case default
             jcur = -1
          end select
          gidx(jodr, jb) = jcur
       enddo
    enddo other

    if (ierr.eq.0) then
       ! duplication check
       do jb = 0, nbuf - 1
          do jodr = 0, meff(jb) - 1
             jco = gidx(jodr, jb)
             if (jco.lt.0) cycle
             if (count(gidx(jodr + 1:meff(jb) - 1, jb).eq.jco).gt.0) then
                ierr = ERR_INVALID_PARAMETER
                call message(ierr, 'duplicate tweaking ', (/jb, jco/))
301             format('duplicate:', I0, 1x, A)
                do jt = jodr, meff(jb) - 1
                   if (gidx(jt, jb).eq.jco) write(uerr, 301) jb, trim(cnameR(jt, jb))
                enddo
             endif
          enddo
       enddo
    endif
    if (ierr.eq.0) then
       ! adjust topology table
       do jb = 0, nbuf - 1
          jup = -1
          jlast = -1
          joref = -1
          do jodr = 0, meff(jb) - 1
             jcur = gidx(jodr, jb)
             if (jcur.ge.mref) then
                ! new coordinate
                if (jup.ge.0) cout(jcur, jup) = jcur
                if (joref.ge.0) cout(jcur, joref) = jcur
                jup = jcur
                jlast = jcur
                if (ctref(jodr).eq.co_null) cout(jcur, jodr) = jcur
                if (jodr.lt.mref-1) cout(jodr+1, jcur) = jodr+1
             else if (jcur.ge.0) then
                ! reference coordinate
                if (jlast.ge.0) cout(jcur, jlast) = jcur
                if (jcur.ge.joref) joref = jcur
             endif
          enddo
       enddo
    endif
    if (ierr.eq.0) then
       do jt = 0, nt - 1
          nin(jt) = count(cout(jt, 0:nt-1).ge.0)
          ! write(*, *) 'perm/topo', jt, nin(jt), '/', cout(0:nt-1, jt)
       enddo
    endif
    if (ierr.eq.0) then
       jodr = 0
       sort: do
          if (COUNT(nin(0:nt-1).eq.0).gt.1) then
             ierr = ERR_INVALID_PARAMETER
             call message(ierr, 'ambiguous fragile coordinates ', (/jodr/))
             do jt = 0, nt - 1
311             format('perm:collision: ', I0, 1x, A)
                if (nin(jt).eq.0) write(uerr, 311) jodr, cbuf(jt)
             enddo
          endif
          jco = find_first(nin(0:nt-1), 0, offset=0)
          if (jco.lt.0) exit sort
          jbuf(jodr) = jco
          jodr = jodr + 1
          nin(jco) = -1
          do jt = 0, nt - 1
             if (cout(jt, jco).ge.0) then
                nin(jt) = max(0, nin(jt) - 1)
             endif
          enddo
       enddo sort
       if (ierr.eq.0) then
          if (jodr.lt.nt) then
             ierr = ERR_INVALID_PARAMETER
             call message(ierr, 'cycle in fragile coordinates')
             do jt = 0, nt - 1
312             format('perm:cycle: ', I0, 1x, A)
                if (nin(jt).ge.0) write(uerr, 312) jodr, cbuf(jt)
             enddo
          endif
       endif
    endif
    if (ierr.eq.0) then
       ! packing
       jpack(0:nt-1) = -1
       nceff = 0
       jodr = 0
       joref = 0
       jlast = 0
       do
          if (jodr.ge.nt) exit
          nlim = 0
          do jcur = jlast, mref - 1
             if (ctref(jcur).ne.co_null) exit
             nlim = nlim + 1
          enddo
          joref = jlast + nlim
          if (joref.ge.mref) exit
          nmem = 0
          do jcur = jodr, nt - 1
             if (jbuf(jcur).eq.joref) exit
             if (jbuf(jcur).ge.mref) nmem = nmem + 1
          enddo
          nlim = max(0, nlim - nmem)
          ! write(*, *) nceff, jodr, jlast, joref, nlim, nmem
          do
             if (jbuf(jodr).ge.joref.or.nlim.gt.0) then
                jpack(jbuf(jodr)) = nceff
                if (nceff.lt.mco) cnameL(nceff) = cbuf(jbuf(jodr))
                nceff = nceff + 1
                if (jbuf(jodr).lt.joref) nlim = nlim - 1
             endif
             if (jbuf(jodr).eq.joref) exit
             jodr = jodr + 1
          enddo
          jodr = jodr + 1
          jlast = joref + 1
       enddo
       ! write(*, *) 'pack', jodr, nceff
    endif
    ! packing leftover
    if (ierr.eq.0) then
       nmem = count(jbuf(jodr:nt-1).ge.mref)
       if (nmem.gt.0) then
          nlim = (mref - nceff) - nmem
       else
          nlim = (meff(jbref) - nceff) - nmem
       endif
       ! write(*, *) nmem, nlim, nceff
       do
          if (jodr.ge.nt) exit
          if (jbuf(jodr).lt.mref) nlim = nlim - 1
          if (jbuf(jodr).ge.mref.or.nlim.ge.0) then
             jco = jbuf(jodr)
             jpack(jco) = nceff
             if (nceff.lt.mco) cnameL(nceff) = cbuf(jco)
             nceff = nceff + 1
          endif
          jodr = jodr + 1
       enddo
       if (nceff.gt.mco) then
          ierr = ERR_INSUFFICIENT_BUFFER
          call message(ierr, 'overflow in permutation matching', (/nceff/))
       endif
    endif

    ! set correspondence table
    if (ierr.eq.0) then
       clidxR(0:mco-1, 0:nbuf-1) = -1
       do jb = 0, nbuf - 1
          do jco = 0, meff(jb) - 1
             jt = gidx(jco, jb)
             if (jt.ge.0) then
                jodr = jpack(jt)
                if (jodr.ge.0) clidxR(jodr, jb) = jco
             endif
          enddo
       enddo
    endif

    if (ierr.ne.0) then
       do jb = 0, nbuf - 1
          call join_list(jerr, str, cnameR(0:mco-1, jb), sep=',', ldelim='[', rdelim=']')
901       format('shape:', I0, 1x, A)
          write(uerr, 901) jb, trim(str)
       enddo
    endif

    if (ierr.ne.0.or.is_msglev_DEBUG(lev_verbose)) then
       call debug_topo_table(cbuf, cout, nt, mtbl, 'final', nin)
    endif
  end subroutine match_perms

!!!_    * debug_topo_table
  subroutine debug_topo_table &
       & (cbuf, cout, nt, lt, tag, nin)
    implicit none
    integer,         intent(in) :: nt, lt
    integer,         intent(in) :: cout(0:lt-1, 0:*)
    character(len=*),intent(in) :: cbuf(0:*)
    character(len=*),intent(in) :: tag
    integer,optional,intent(in) :: nin(0:*)
    integer jt
    integer ni(0:lt-1)

    if (present(nin)) then
       ni(0:nt-1) = nin(0:nt-1)
    else
       do jt = 0, nt - 1
          ni(jt) = count(cout(jt, 0:nt-1).ge.0)
       enddo
    endif
302 format(4x, 'topo:', A, ':', I0, 1x, A8, 1x, I0, ' >', 12(1x, I3))
    do jt = 0, nt - 1
       write(*, 302) trim(tag), jt, trim(cbuf(jt)), ni(jt), cout(0:nt-1, jt)
    enddo

  end subroutine debug_topo_table

!!!_    * count_coordinates
  integer function count_coordinates &
       & (cidx, cname, mco) &
       & result(n)
    implicit none
    integer,         intent(in) :: cidx(0:*)
    character(len=*),intent(in) :: cname(0:*)
    integer,         intent(in) :: mco
    integer jo
    n = 0
    do jo = 0, mco - 1
       if (cname(jo).ne.' ' .or. cidx(jo).ge.co_wild) n = jo + 1
    enddo
  end function count_coordinates

!!!_   . count_effective()
  integer function count_effective(lpp, maxco) result(nc)
    use TOUZA_Std,only: choice
    implicit none
    type(loop_t),    intent(in) :: lpp(0:*)
    integer,optional,intent(in) :: maxco
    integer jc
    nc = -1
    do jc = 0, choice(lcoor, maxco) - 1
       if (lpp(jc)%flg.ge.loop_null) nc = jc
       if (lpp(jc)%name.ne.' ') nc = jc
    enddo
    nc = nc + 1
  end function count_effective
!!!_   . coordinate_type()
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

!!!_   . is_null_coor() - check if argument loop_t corresponds to null-coordinate
  logical function is_null_coor(lpp) result (b)
    implicit none
    type(loop_t),intent(in) :: lpp
    b = (lpp%flg.le.loop_reduce.and.lpp%name.eq.' ')
  end function is_null_coor

!!!_   . parse_coordinate_repl - parse coordinate argument complex
  subroutine parse_coordinate_repl &
       & (ierr, cold, xold, crep, xrep, str)
    use TOUZA_Std,only: parse_number
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: cold, crep
    logical,         intent(out) :: xold, xrep
    character(len=*),intent(in)  :: str          ! range parameter must be deleted before call

    integer lstr,  lsep
    integer jsep0, jsep1
    integer jp
    integer idmy
    ierr = 0

    lstr = len_trim(str)
    if (lstr.eq.0) then
       ! (null)
       xold = .FALSE.
       xrep = .FALSE.
       cold = ' '
       crep = ' '
       return
    endif
    jsep0 = index(str, rename_sep)
    if (jsep0.eq.0) then
       call parse_number(ierr, idmy, str(1:lstr))
       if (ierr.eq.0) then
          ! [+]NUMBER
          xold = .FALSE.
          xrep = .FALSE.
          cold = str(1:lstr)
          crep = ' '
          return
       endif
       ierr = 0
       if (index(str(1:lstr), insert_coor).eq.1) then
          ! +REPL
          xold = .FALSE.
          xrep = .TRUE.
          jp = len_trim(insert_coor)
          cold = str(1:jp)
          crep = str(jp+1:lstr)
       else
          ! NAME
          xold = .FALSE.
          xrep = .FALSE.
          cold = str(1:lstr)
          crep = ' '
       endif
       return
    endif

    ! [NAME]/[REPL[/]]
    xold = .TRUE.
    cold = str(1:jsep0-1)
    lsep = len(rename_sep)
    jsep1 = index(str(jsep0+lsep:), rename_sep)
    if (jsep1.eq.0) then
       ! [NAME]/REPL
       crep = str(jsep0+lsep:lstr)
       xrep = (jsep0+lsep) .le. lstr
    else
       ! [NAME]/REPL/
       jsep1 = jsep0 + lsep + jsep1 - 2
       crep = str(jsep0+lsep:jsep1)
       xrep = .TRUE.
    endif

  end subroutine parse_coordinate_repl

!!!_ + end chak
end module chak_lib
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

!!!_! chak.F90 - TOUZA/Jmz CH(swiss) Army Knife
! Maintainer: SAITO Fuyuki
! Created: Nov 25 2021
#define TIME_STAMP 'Time-stamp: <2023/06/19 13:26:37 fuyuki chak.F90>'
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
!!!_ + sizes
#ifndef    OPT_CHAK_STACKS
#  define  OPT_CHAK_STACKS   512
#endif
#ifndef    OPT_CHAK_QUEUE
#  define  OPT_CHAK_QUEUE   1024
#endif
! #ifndef   HAVE_FORTRAN_IEEE_ARITHMETIC
! #  define HAVE_FORTRAN_IEEE_ARITHMETIC 0
! #endif
!!!_@ TOUZA/Jmz/chak - nio swiss army knife
program chak
!!!_ + Declaration
!!!_  - modules
  use chak_lib,lib_init=>init, lib_finalize=>finalize
  use chak_opr,opr_init=>init, opr_diag=>diag, opr_finalize=>finalize
  use chak_file,file_init=>init
! #if HAVE_FORTRAN_IEEE_ARITHMETIC
!   use IEEE_ARITHMETIC
! #endif
  implicit none
!!!_  - parameters
  integer,parameter :: ERR_NO_CANDIDATE = 1
  integer,parameter :: ERR_FINISHED = 2

!!!_  - variables
  integer ierr
!!!_  - files
  integer,save      :: rcount = 0   ! read file count
  integer,save      :: wcount = 0   ! write file count

  integer,parameter :: def_write = 0
  integer,parameter :: def_read = def_write + 1
  integer,parameter :: bgn_file = def_read + 1

  type(file_t),target :: ofile(def_write:lfile-1)

  integer,parameter :: stt_none   = 0
  integer,parameter :: stt_free   = 1
  integer,parameter :: stt_locked = -1
!!!_  - buffers
  integer,save      :: lcount = 0   ! literal count
  integer,save      :: consts=0     ! predefine constants
  type(buffer_t),target :: obuffer(0:lbuffer-1)
!!!_  - buffer stack
  integer           :: mstack
  integer,parameter :: lstack=OPT_CHAK_STACKS
  type(stack_t)     :: bstack(0:lstack-1)
!!!_  - argument queue
  integer           :: mqueue
  integer,parameter :: lqueue=OPT_CHAK_QUEUE
  type(queue_t)     :: aqueue(0:lqueue-1)

  integer,parameter :: max_operands = 2
!!!_  - global flags
  integer :: def_cmode = cmode_null   ! default compromise mode

  character(len=lfmt),save :: afmt_int = '(I0)'
  character(len=lfmt),save :: afmt_flt = '(es16.12)'
  character(len=lfmt),save :: afmt_dbl = '(es16.12)'

!!!_  - misc
  integer irecw

  integer,parameter :: stat_help = 1
  integer           :: dryrun = 0

  integer,parameter :: levq_rec = -1   ! verbose(quiet) level to show record index
  integer,parameter :: levq_stack = levq_rec - 1
  integer,parameter :: levq_column = levq_stack - 1
  integer,parameter :: levq_coordinate = levq_column - 1

  character(len=*),parameter :: bfmt_write_buf   = '(''W'', I0)'
  character(len=*),parameter :: bfmt_read_buf    = '(''F'', I0)'
  character(len=*),parameter :: bfmt_read_filter = '(''F'', I0, ''.'', I0)'

!!!_ + Body
  ierr = 0

  mqueue = 0

  if (ierr.eq.0) call init(ierr)
  if (ierr.eq.0) call parse_args(ierr)

  if (ierr.eq.0) call init_all_files(ierr, lev_verbose)
  irecw = 0
  do
     if (ierr.eq.0) call batch_operation(ierr, irecw, lev_verbose)
     if (ierr.ne.0) exit
     irecw = irecw + 1
  enddo
  ierr = min(0, ierr)

  if (ierr.eq.0) call finalize(ierr)
#if HAVE_FORTRAN_EXIT
  if (ierr.ne.0) then
     call trace_err(ierr)
     call exit(1)
  endif
#elif HAVE_FORTRAN_ERROR_STOP
  if (ierr.ne.0) then
     call trace_err(ierr)
     error stop 1
  endif
#else /* not HAVE_FORTRAN_ERROR_STOP */
  if (ierr.ne.0) then
     call trace_err(ierr)
  endif
#endif /* not HAVE_FORTRAN_ERROR_STOP */
  stop
!!!_ + Subroutines
contains
!!!_  - commons
!!!_   . init
  subroutine init(ierr)
    use TOUZA_Nio,only: nio_init=>init, nr_init
    use TOUZA_Nio,only: set_bodr_wnative, BODR_CHECK_VERBOSE
    implicit none
    integer,intent(out) :: ierr

    ierr = 0
    if (ierr.eq.0) call lib_init(ierr)
    if (ierr.eq.0) call opr_init(ierr)
    if (ierr.eq.0) call file_init(ierr)
    if (ierr.eq.0) call register_predefined(ierr)
    if (ierr.eq.0) call init_sub(ierr)
    if (ierr.eq.0) call nio_init(ierr, levv=dbgv, stdv=stdv)
    if (ierr.eq.0) call set_bodr_wnative(ierr, BODR_CHECK_VERBOSE, levv=dbgv)
    if (ierr.eq.0) call nr_init(ierr, lazy=+1)

    if (ierr.eq.0) call reset_file(ierr, ofile(def_read),  ' ', mode_terminate, hflag_default)
    if (ierr.eq.0) call reset_file(ierr, ofile(def_write), ' ', mode_new,       hflag_default)
    if (ierr.eq.0) mfile = bgn_file
! #if HAVE_FORTRAN_IEEE_ARITHMETIC
!     UNDEF = IEEE_VALUE(ZERO, IEEE_QUIET_NAN)
! #endif
  end subroutine init
!!!_    * init_sub
  subroutine init_sub(ierr)
    implicit none
    integer,intent(out)         :: ierr
    integer p, r
    real(kind=KDBL),parameter :: ZD = 0.0_KDBL
    real(kind=KFLT),parameter :: ZF = 0.0_KFLT

    ierr = 0
101 format('(es', I0, '.', I0, ')')
    p = PRECISION(ZD)
    r = 2
    write(afmt_dbl, 101) p+5+r, p
    p = PRECISION(ZF)
    r = 2
    write(afmt_flt, 101) p+5+r, p
    return
  end subroutine init_sub

!!!_   . finalize
  subroutine finalize(ierr, u)
    use TOUZA_Nio,only: nio_diag=>diag, nio_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u

    ierr = 0
    if (is_msglev_INFO(lev_verbose)) then
       if (ierr.eq.0) call show_queue(ierr, u)
       if (ierr.eq.0) call show_files(ierr, u)
    endif
    if (is_msglev_DETAIL(lev_verbose)) then
       if (ierr.eq.0) call show_buffers(ierr, u)
    endif
    if (ierr.eq.0) call opr_diag(ierr, u, levv=dbgv)
    if (ierr.eq.0) call nio_diag(ierr, levv=dbgv)

    if (ierr.eq.0) call opr_finalize(ierr, u, levv=dbgv)
    if (ierr.eq.0) call nio_finalize(ierr, levv=dbgv)

    if (ierr.eq.0) call lib_finalize(ierr, u)
  end subroutine finalize

!!!_   . show_usage
  subroutine show_usage (ierr, u, levv)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer utmp
    integer lv

    ierr = 0
    utmp = choice(ulog, u)
    lv = choice(0, levv)

101 format('chak - CH(Swiss) Army Knife for gtool-3.5 format files')
102 format(2x, 'with ', A, 1x, A, '; ', A, 1x, A)
103 format(2x, 'with n*c*tcdf ', A)
    write(utmp, 101)
    write(utmp, 102) PACKAGE_NAME, PACKAGE_VERSION, &
         & TOUZA_NAME, TOUZA_VERSION
#if OPT_WITH_NCTCDF
    write(utmp, 103) 'enabled'
#else /* not OPT_WITH_NCTCDF */
    write(utmp, 103) 'disabled'
#endif /* not OPT_WITH_NCTCDF */
    return
  end subroutine show_usage

!!!_  - log arrays
!!!_   . show_queue
  subroutine show_queue(ierr, u)
    use TOUZA_Std,only: choice, join_list
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer utmp
    integer j
    integer js
    integer hk
    integer cmode
    character(len=128) :: buf
    character(len=lpath) :: str
    character cmd

    ierr = 0
    utmp = choice(ulog, u)
101 format('operator:', A,           ' -', I0, '+', I0, 1x, A, '>> ', A)
102 format('operator:', A, '(', I0, ') -', I0, '+', I0, 1x, A, '>> ', A)
111 format('file:', A)
121 format('stack:', A)
    js = 0
    do j = 0, min(mqueue, lqueue) - 1
       if (ierr.eq.0) then
          cmode = aqueue(j)%cmode
          if (cmode.eq.cmode_null) cmode = def_cmode
          select case(cmode)
          case(cmode_each)
             cmd = 'e'
          case(cmode_first)
             cmd = 'l'
          case(cmode_inclusive)
             cmd = 'i'
          case(cmode_intersect)
             cmd = 'x'
          case default
             cmd = ' '
          end select
       endif
       if (ierr.eq.0) then
          hk = handle_type(aqueue(j)%term)
          select case(hk)
          case(hk_opr)
             call query_opr_name(ierr, buf, aqueue(j)%term)
             if (ierr.eq.0) then
                if (aqueue(j)%iter.ne.0) then
                   write(str, 102, IOSTAT=ierr) trim(buf), aqueue(j)%iter, &
                        & aqueue(j)%nopr, size(aqueue(j)%lefts), cmd, trim(aqueue(j)%desco)
                else
                   write(str, 101, IOSTAT=ierr) trim(buf), &
                        & aqueue(j)%nopr, size(aqueue(j)%lefts), cmd, trim(aqueue(j)%desco)
                endif
             endif
          case(hk_file)
             write(str, 111, IOSTAT=ierr) trim(aqueue(j)%desco)
          case default
             write(str, 121, IOSTAT=ierr) trim(aqueue(j)%desco)
          end select
          ierr = 0
       endif
201    format('queue[', I0, '] ', I0, 1x, A)
       if (ierr.eq.0) then
          js = js - aqueue(j)%nopr + size(aqueue(j)%lefts)
          if (utmp.ge.0) then
             write(utmp, 201) user_index_bgn(j), js, trim(str)
          else if (utmp.eq.-1) then
             write(*,    201) user_index_bgn(j), js, trim(str)
          endif
       endif
    enddo
  end subroutine show_queue

!!!_   . show_files
  subroutine show_files(ierr, u, levv)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer utmp, lv
    integer j
    integer jbgn

    ierr = 0
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (is_msglev_DETAIL(lv-1)) then
       jbgn = 0
    else
       jbgn = bgn_file
    endif

    do j = jbgn, min(mfile, lfile) - 1
       if (ierr.eq.0) call show_file(ierr, ofile(j), utmp, lv)
    enddo
  end subroutine show_files

!!!_   . show_buffers
  subroutine show_buffers(ierr, u, levv)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer lv
    integer jbgn

    ierr = 0
    lv = choice(lev_verbose, levv)

    if (is_msglev_DEBUG(lv)) then
       jbgn = 0
    else
       jbgn = consts
    endif
    call show_buffers_core(ierr, obuffer, min(mbuffer, lbuffer), jbgn, u, levv)
  end subroutine show_buffers

!!!_   . show_buffers_core
  subroutine show_buffers_core(ierr, bufs, nbuf, jbgn, u, levv)
    use TOUZA_Std,only: choice
    implicit none
    integer,       intent(out)         :: ierr
    type(buffer_t),intent(in)          :: bufs(0:*)
    integer,       intent(in)          :: nbuf
    integer,       intent(in),optional :: jbgn
    integer,       intent(in),optional :: u
    integer,       intent(in),optional :: levv
    integer utmp, lv
    integer j, h, m
    character(len=128) :: bname
    character(len=128) :: txt
    character(len=1) :: cs
    character(len=1),parameter :: CSTT(stt_locked:stt_free) = (/'L', 'N', 'F'/)
    integer jerr

    ierr = 0
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    do j = choice(0, jbgn), nbuf - 1
       m = buffer_vmems(bufs(j))
101    format('I:', I0)
102    format('F:', I0)
103    format('D:', I0)
109    format('X/', I0)
       select case (bufs(j)%k)
       case (kv_int)
          write(txt, 101) m
       case (kv_flt)
          write(txt, 102) m
       case (kv_dbl)
          write(txt, 103) m
       case default
          write(txt, 109) bufs(j)%k
       end select
       cs = cstt(min(stt_free, max(stt_locked, bufs(j)%stt)))

201    format(A, 1x, A, 1x, A, 1x, E10.3)
211    format('buffer:', I0)
212    format('buffer:', A)
       h = buf_i2handle(j)
       if (bufs(j)%name.eq.' ') then
          write(bname, 211) h
       else
          write(bname, 212, IOSTAT=jerr) trim(bufs(j)%name)
       endif
       write(utmp, 201) trim(bname), cs, trim(txt), bufs(j)%undef
    enddo
  end subroutine show_buffers_core

!!!_   . show_stack
  subroutine show_stack(ierr, u, levv)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    call show_stack_core(ierr, bstack, min(mstack, lstack), obuffer, u, levv)
  end subroutine show_stack

!!!_   . show_stack_core
  subroutine show_stack_core(ierr, bs, ns, bufs, u, levv)
    use TOUZA_Std,only: choice
    implicit none
    integer,       intent(out)         :: ierr
    type(stack_t), intent(in)          :: bs(0:*)
    integer,       intent(in)          :: ns
    type(buffer_t),intent(in)          :: bufs(0:*)
    integer,       intent(in),optional :: u
    integer,       intent(in),optional :: levv
    integer utmp
    integer js, jb
    character(len=128)   :: pfx
    character(len=lpath) :: str
    character(len=lpath) :: desc
    character(len=128)   :: domain
    integer lv
    integer alev
    integer jas
    integer m
    integer jerr

    ierr = 0
    lv = choice(lev_verbose, levv)
    utmp = choice(ulog, u)
201 format('stack[', I0, ']')
    jas = user_index_bgn(0)
    do js = 0, ns - 1
       alev = anchor_h2level(bs(js)%bh)
       if (alev.ge.0) then
211       format('--- ', I0)
212       format('---')
213       format('===')
          if (alev.gt.1) then
             write(str, 211) alev
          else if (alev.eq.1) then
             write(str, 212)
          else
             write(str, 213)
          endif
          call message(ierr, trim(str), u=utmp, indent=6)
       else
          if (ierr.eq.0) call get_obj_string(ierr, str, bs(js)%bh, lv)
          jb = buf_h2item(bs(js)%bh)
          if (jb.ge.0) then
             if (bufs(jb)%desc.ne.' ') then
202             format(1x, '<', A, '>')
                write(desc, 202, IOSTAT=jerr) trim(bufs(jb)%desc)
                str = trim(str) // trim(desc)
             else
                m = buffer_vmems(bufs(jb))
                if (m.lt.0) str = trim(str) // ' -'
             endif
          endif
          if (is_msglev_DETAIL(lv)) then
             call get_domain_string(ierr, domain, bs(js)%lcp)
             str = trim(str) // ' ' // trim(domain)
          endif
          write(pfx, 201) jas
          call message(ierr, trim(pfx) // ' ' // trim(str), u=utmp, indent=4)
          jas = jas + 1
       endif
    enddo
  end subroutine show_stack_core

!!!_   . show_stack_perms
  subroutine show_stack_perms &
       & (ierr, dom, cname, ctype, cpidx, bufh, lstk, mco, nbuf, u, levv)
    use TOUZA_Std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: mco, nbuf
    type(domain_t),  intent(in)          :: dom(0:*)
    character(len=*),intent(in)          :: cname(0:mco-1, 0:*)
    integer,         intent(in)          :: ctype(0:mco-1, 0:*)
    integer,         intent(in)          :: cpidx(0:mco-1, 0:*)
    integer,         intent(in)          :: bufh(0:*)
    type(stack_t),   intent(in)          :: lstk(0:*)  ! only for logical coordinates
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer utmp, lv
    integer j
    integer hb, jb
    character(len=128) :: pstr, dstr

    ierr = 0
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    do j = 0, nbuf - 1
       hb = bufh(j)
       jb = buf_h2item(hb)
       if (ierr.eq.0) then
          call get_perm_string &
               & (ierr, pstr, cname(:,j), ctype(:,j), cpidx(:,j), obuffer(jb)%pcp, lstk(j)%lcp, mco)
       endif
       if (ierr.eq.0) then
          call get_domain_perm &
               & (ierr, dstr, dom(j), cname(:,j), ctype(:,j))
       endif
101    format(4x, A, ' >> ', A)
       write(utmp, 101) trim(pstr), trim(dstr)
    enddo

  end subroutine show_stack_perms

!!!_  - argument parser
!!!_   . parse_args
  subroutine parse_args(ierr)
    use TOUZA_Nio,only: nio_init=>init
    use TOUZA_Std,only: arg_init, arg_diag, parse, get_param
    implicit none
    integer,intent(out) :: ierr

    integer japos
    character(len=lpath) :: arg
    integer jerr
    integer stat

    ierr = 0

    mqueue = 0

    consts = mbuffer  ! save number of predefined constants here

    mstack = 0
    stat = 0

    if (ierr.eq.0) call arg_init(ierr, cha=' = ')   ! disable assignment
    if (ierr.eq.0) call parse(ierr)
    if (ierr.eq.0) then
       japos = 0
       ! hprev = opr_null
       do
          japos = japos + 1
          call get_param(jerr, arg, japos)
          if (jerr.ne.0) exit
          if (stat.ne.0) exit
          do
             if (ierr.eq.0) then
                call parse_arg_operator(ierr, arg)
                if (ierr.eq.0) exit
                ierr = min(0, ierr)
             endif
             if (ierr.eq.0) then
                call parse_arg_buffer(ierr, arg)
                if (ierr.eq.0) exit
                ierr = min(0, ierr)
             endif
             if (ierr.eq.0) then
                call parse_arg_literal(ierr, arg)
                if (ierr.eq.0) exit
                ierr = min(0, ierr)
             endif
             if (ierr.eq.0) then
                call parse_option(ierr, stat, japos, arg)
                if (ierr.eq.0) exit
                ierr = min(0, ierr)
             endif
             if (ierr.eq.0) then
                call parse_arg_file(ierr, arg)
                if (ierr.eq.0) exit
                ierr = min(0, ierr)
             endif
             if (ierr.eq.0) ierr = ERR_INVALID_ITEM
             exit
          enddo
          if (ierr.ne.0) exit
          ! call show_stack(ierr, levv=-99)
       enddo
       ! call show_buffers(ierr)
    endif
    select case (stat)
    case (stat_help)
       call show_usage(ierr, levv=lev_verbose)
       return
    end select
    ! if (ierr.eq.0) call set_rec_filter(ierr)
    ! if (ierr.eq.0) call settle_read_files(ierr)
    if (ierr.eq.0) call set_write_format(ierr)
    if (dbgv.gt.0) then
       if (ierr.eq.0) call nio_init(ierr, levv=dbgv, mode=MODE_SHALLOW+MODE_FORCE)
    endif
  end subroutine parse_args

!!!_   . parse_option
  subroutine parse_option &
       & (ierr, stat, japos, arg)
    use TOUZA_Std,only: parse_number, get_param
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: stat
    integer,         intent(inout) :: japos
    character(len=*),intent(in)    :: arg
    character(len=lpath) :: abuf
    integer n

    integer md, cmd, hmd
    integer ntmp
    integer hflag, hsub
    character(len=64) :: xopts
    integer xsub

    md = mode_unset
    cmd = mode_unset
    hmd = hedit_unset
    ierr = 0
    abuf = arg
    stat = 0
    hflag = hflag_unset
    hsub = 0
    xsub = 0
    xopts = ' '

    if (index('-+', abuf(1:1)).eq.0) then
       ierr = ERR_NO_CANDIDATE   ! not error
       return
    endif
    if      (abuf(1:2).eq.'-v') then
       n = count_option_levels(abuf(3:), 'v')
       if (n.lt.0) then
          ierr = n
       else
          lev_verbose = + n
       endif
    else if (abuf.eq.'+v') then
       lev_verbose = +999
    else if (abuf(1:2).eq.'-q') then
       n = count_option_levels(abuf(3:), 'q')
       if (n.lt.0) then
          ierr = n
       else
          lev_verbose = - n
       endif
    else if (abuf.eq.'+q') then
       lev_verbose = -999
    else if (abuf(1:2).eq.'-d') then
       n = count_option_levels(abuf(3:), 'd')
       if (n.lt.0) then
          ierr = n
       else
          dbgv = + n
       endif
    else if (abuf.eq.'+d') then
       dbgv = +999
    else if (abuf.eq.'-h') then
       stat = stat_help
    else if (abuf.eq.'-n') then
       dryrun = 1
    else if (abuf.eq.'-f') then   ! write-mode
       md = mode_write
    else if (abuf.eq.'-a') then
       md = mode_append
    else if (abuf.eq.'-k') then
       md = mode_new
    else if (abuf.eq.'-c') then   ! read-mode
       md = mode_cycle
    else if (abuf.eq.'-s') then
       md = mode_terminate
    else if (abuf.eq.'-p') then
       md = mode_persistent
    else if (abuf.eq.'-e') then
       cmd = cmode_each
    else if (abuf.eq.'-i') then   ! compromise mode
       cmd = cmode_inclusive
    else if (abuf.eq.'-x') then
       cmd = cmode_intersect
    else if (abuf.eq.'-l') then
       cmd = cmode_first
    else if (abuf.eq.'-N') then
       hflag = hflag_nulld        ! file or default read
       hsub = -1
    else if (abuf.eq.'+N') then
       hflag = hflag_nulld        ! special for default write
       hsub = +1
    else if (abuf(1:2).eq.'-X') then
       xsub = -1
       xopts = trim(abuf(3:))     ! file or default read
    else if (abuf(1:2).eq.'+X') then
       xsub = +1
       xopts = trim(abuf(3:))     ! special for default write
    else if (abuf.eq.'-P') then
       call check_only_global(ierr, abuf)
       if (ierr.eq.0) call set_user_offsets(ierr, 0, 0)
    else if (abuf.eq.'-F') then
       call check_only_global(ierr, abuf)
       if (ierr.eq.0) call set_user_offsets(ierr, 1, 0)
    else if (abuf.eq.'-H') then
       japos = japos + 1
       call get_param(ierr, abuf, japos)
       if (ierr.eq.0) call parse_number(ierr, ntmp, abuf)
       if (ierr.eq.0) hmd = ntmp
    else if (abuf(1:2).eq.'-H') then
       call parse_number(ierr, ntmp, abuf(3:))
       if (ierr.eq.0) hmd = ntmp
    else if (abuf.eq.'--demo') then
       if (ierr.eq.0) uerr = ulog
    else
       ierr = ERR_INVALID_ITEM
    endif
    if (ierr.eq.ERR_INVALID_ITEM) then
       call message(ierr, 'invalid option ' // trim(abuf), (/japos/))
    else if (ierr.ne.0) then
       call message(ierr, 'argument parser fails ' // trim(abuf), (/japos/))
    endif
    if (md.ne.mode_unset) then
       if (ierr.eq.0) call parse_file_option(ierr, md)
    else if (hmd.ne.hedit_unset) then
       if (ierr.eq.0) call parse_hedit_option(ierr, hmd)
    else if (hflag.ne.hflag_unset) then
       if (ierr.eq.0) call parse_hflag_option(ierr, hflag, hsub)
    else if (xsub.ne.0) then
       if (ierr.eq.0) call parse_xflag_option(ierr, xopts, xsub)
    else if (cmd.ne.mode_unset) then
       if (ierr.eq.0) call parse_operator_option(ierr, cmd)
    endif
  end subroutine parse_option
!!!_    * count_option_levels
  integer function count_option_levels(str, ch) result(n)
    use TOUZA_Std,only: parse_number
    implicit none
    character(len=*),intent(in) :: str
    character(len=1),intent(in) :: ch
    integer jerr
    call parse_number(jerr, n, str)
    if (jerr.ne.0) then
       n = verify(trim(str), ch, .TRUE.)
       if (n.ne.0) then
          n = ERR_INVALID_ITEM
       else
          n = len_trim(str) + 1
       endif
    endif
  end function count_option_levels
!!!_    * check_only_global
  subroutine check_only_global(ierr, arg)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: arg
    ierr = 0
    if (mqueue.gt.0) then
       ierr = ERR_INVALID_PARAMETER
       call message(ierr, trim(arg) // ' must be set before any stack and queue')
    endif
    return
  end subroutine check_only_global
!!!_   . parse_file_option
  subroutine parse_file_option(ierr, mode)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: mode

    integer jfile
    character(len=128) :: msg
    integer jerr

    ierr = 0
    if (mfile.gt.lfile) then
       ierr = ERR_INSUFFICIENT_BUFFER
       return
    endif

    jfile = mfile - 1

    if (is_read_mode(mode)) then
       if (jfile.lt.bgn_file) jfile = def_read
       if (is_read_mode(ofile(jfile)%mode)) then
          ofile(jfile)%mode = mode
       else
          ierr = ERR_INVALID_SWITCH
       endif
    else
       if (jfile.lt.bgn_file) jfile = def_write
       if (.not.is_read_mode(ofile(jfile)%mode)) then
          ofile(jfile)%mode = mode
       else
          ierr = ERR_INVALID_SWITCH
       endif
    endif
    if (ierr.ne.0) then
101    format('cannot set file option for file ', I0)
       write(msg, 101, IOSTAT=jerr) jfile
       call message(ierr, msg)
    endif
  end subroutine parse_file_option

!!!_   . parse_hedit_option
  subroutine parse_hedit_option(ierr, mode)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: mode

    integer jfile

    ierr = 0
    if (mfile.gt.lfile) then
       ierr = ERR_INSUFFICIENT_BUFFER
       return
    endif
    jfile = mfile - 1
    if (jfile.lt.bgn_file) jfile = def_write
    if (.not.is_read_mode(ofile(jfile)%mode)) then
       ofile(jfile)%hedit = mode
    endif
  end subroutine parse_hedit_option

!!!_   . parse_hflag_option
  subroutine parse_hflag_option(ierr, flag, sub)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: flag
    integer,intent(in)  :: sub

    integer jfile

    ierr = 0
    if (mfile.gt.lfile) then
       ierr = ERR_INSUFFICIENT_BUFFER
       return
    endif
    jfile = mfile - 1
    if (jfile.lt.bgn_file) then
       if (sub.gt.0) then
          jfile = def_write
       else
          jfile = def_read
       endif
    endif
    ofile(jfile)%hflag = flag
  end subroutine parse_hflag_option

!!!_   . parse_xflag_option
  subroutine parse_xflag_option(ierr, opts, mode)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: opts
    integer,         intent(in)  :: mode

    integer jfile

    ierr = 0
    if (mfile.gt.lfile) then
       ierr = ERR_INSUFFICIENT_BUFFER
       return
    endif
    jfile = mfile - 1
    if (jfile.lt.bgn_file) then
       if (mode.gt.0) then
          jfile = def_write
       else
          jfile = def_read
       endif
    endif
    ! enable full extension
    if (opts.eq.' ') then
       ofile(jfile)%bigg = bigg_off
    endif
    if (scan(opts, 'Bb').gt.0) then
       ofile(jfile)%bigg = bigg_on
    else if (scan(opts, 'Ss').gt.0) then
       ofile(jfile)%bigg = bigg_off
    endif
  end subroutine parse_xflag_option

!!!_   . parse_operator_option
  subroutine parse_operator_option(ierr, mode)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: mode

    ierr = 0
    if (mqueue.le.0) then
       def_cmode = mode
    else
       aqueue(mqueue-1)%cmode = mode
    endif
    return
  end subroutine parse_operator_option

!!!_   . parse_arg_file
  subroutine parse_arg_file &
       & (ierr, arg)
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(in)    :: arg
    integer hfile, hbuf
    integer jb
    integer lasth
    integer jf
    integer nbufs
    character(len=lname) :: bname
    integer oentr
    integer jerr

    ierr = 0
    call new_file(ierr, hfile, arg)
    if (ierr.eq.0) then
       if (hfile.lt.0) ierr = ERR_INVALID_ITEM
    endif
    if (ierr.eq.0) call last_queue(ierr, lasth)
    if (ierr.eq.0) then
       jf = file_h2item(hfile)
       if (lasth.eq.opr_OUTPUT) then
          ! file to write
          ! call show_stack(ierr)
          call pop_queue(ierr)
          ofile(jf)%mode  = ofile(def_write)%mode
          ofile(jf)%hedit = ofile(def_write)%hedit
          ofile(jf)%hflag = ofile(def_write)%hflag
          ofile(jf)%bigg  = ofile(def_write)%bigg
          call append_queue(ierr, hfile, pop=1, push=0)
          if (ierr.eq.0) call pop_stack(ierr, hbuf)
          if (ierr.eq.0) then
             jb = buf_h2item(hbuf)
             obuffer(jb)%stt = stt_locked
             ! fake operator for file handle
             write(bname, bfmt_write_buf, IOSTAT=jerr) user_index_bgn(wcount)
             if (ierr.eq.0) call reg_fake_opr(oentr, hfile, bname)
             if (ierr.eq.0) ofile(jf)%opr = oentr
             ierr = min(0, oentr)
          endif
          if (ierr.eq.0) wcount = wcount + 1
       else
          if (.not.associated(ofile(def_read)%rgrp)) then
             call add_default_records(ierr, ofile(def_read))
          endif
          if (ierr.eq.0) then
             rcount = rcount + 1
             nbufs = count_file_stacks(ofile(def_read))
             ! file to read
             ofile(jf)%mode = mode_read
             ofile(jf)%bigg = ofile(def_read)%bigg
             ofile(jf)%rgrp => ofile(def_read)%rgrp
             call alloc_file_buffers(ierr, oentr, nbufs, rcount-1, 0)
             if (ierr.eq.0) ofile(jf)%opr = oentr
          endif
          if (ierr.eq.0) then
             call append_queue_stack(ierr, hfile, 0, nbufs)
          endif
       endif
    endif
  end subroutine parse_arg_file

!!!_   . alloc_file_buffers
  subroutine alloc_file_buffers(ierr, entr, nbufs, jtag, jsub)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: entr
    integer,intent(in)  :: nbufs
    integer,intent(in)  :: jtag
    integer,intent(in)  :: jsub
    integer jj, jb, hbuf
    character(len=lname) :: tag
    integer jerr

    ierr = 0
    entr = -1
    do jj = 0, nbufs - 1
       if (ierr.eq.0) then
          if (nbufs.le.1.and.jsub.lt.1) then
             write(tag, bfmt_read_buf, IOSTAT=jerr) user_index_bgn(jtag)
          else
             write(tag, bfmt_read_filter, IOSTAT=jerr) user_index_bgn(jtag), user_index_bgn(jsub + jj)
          endif
       endif
       if (ierr.eq.0) call new_buffer(ierr, hbuf, tag)
       if (ierr.eq.0) then
          jb = buf_h2item(hbuf)
          obuffer(jb)%stt = stt_locked
       endif
       if (ierr.eq.0) call push_stack(ierr, hbuf)
       if (ierr.eq.0) call reg_fake_opr(entr, hbuf, tag)
       ierr = min(0, entr)
    enddo
  end subroutine alloc_file_buffers

!!!_   . parse_arg_literal
  subroutine parse_arg_literal &
       & (ierr, arg)
    use TOUZA_Std,only: parse_number
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(in)    :: arg

    integer vi
    real(kind=KFLT) :: vf
    real(kind=KDBL) :: vd
    integer jerr
    integer handle
    logical isx

    ierr = 0
    handle = -1
    ! read(arg, *, IOSTAT=jerr) vi
    call parse_number(jerr, vi, arg)
    if (jerr.eq.0) then
       call new_buffer_literal(ierr, handle, kv_int, real(vi, kind=KBUF), arg)
    else
       ! read(arg, *, IOSTAT=jerr) vd
       call parse_number(jerr, vd, arg)
       if (jerr.eq.0) then
          call new_buffer_literal(ierr, handle, kv_dbl, real(vd, kind=KBUF), arg)
       else
          call parse_number(jerr, vf, arg)
          if (jerr.eq.0) then
             call new_buffer_literal(ierr, handle, kv_flt, real(vf, kind=KBUF), arg)
          endif
       endif
    endif
    if (ierr.eq.0) then
       if (handle.lt.0) then
          ierr = ERR_NO_CANDIDATE
       else
          if (is_msglev_info(lev_verbose)) then
             inquire(FILE=arg, EXIST=isx, IOSTAT=ierr)
             ierr = 0
             if (isx) then
                call message(ierr, 'warning: ' // trim(arg) // ' is parsed not as a file but as a literal')
             endif
          endif
          call append_queue(ierr, handle, 0, 1, (/handle/))
       endif
       if (ierr.eq.0) call push_stack(ierr, handle)
    endif
  end subroutine parse_arg_literal

!!!_   . parse_arg_buffer
  subroutine parse_arg_buffer &
       & (ierr, arg)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: arg
    integer hopr,  lasth
    integer jfile
    logical cond

    ierr = 0
    hopr = parse_term_operator(arg)
    jfile = file_h2item(hopr)

    cond = hopr.ge.0
    if (cond) cond = .not. is_operator(hopr)

    if (cond) then
       if (jfile.lt.0) then
          ! fake operator (== tagged buffer)
          if (ierr.eq.0) call append_queue(ierr, hopr, 0, 1, (/hopr/))
          if (ierr.eq.0) call push_stack(ierr, hopr)
       else
          if (is_read_mode(ofile(jfile)%mode)) then
             ierr = ERR_PANIC
          else
             ! OUTPUT WRITE-TAG
             if (ierr.eq.0) call last_queue(ierr, lasth)
             if (ierr.eq.0) then
                if (lasth.eq.opr_OUTPUT) then
                   call pop_queue(ierr)
                   if (ierr.eq.0) call append_queue(ierr, hopr, pop=1, push=0)
                   if (ierr.eq.0) call pop_stack(ierr)
                else
                   ierr = ERR_PANIC
                endif
             endif
          endif
       endif
       if (ierr.ne.0) then
          call message(ierr, 'panic: ' // trim(arg) // ' cannot be parsed (forget = ?)')
       endif
    else
       ierr = ERR_NO_CANDIDATE   ! not error
    endif
  end subroutine parse_arg_buffer

!!!_   . parse_arg_operator
  subroutine parse_arg_operator &
       & (ierr, arg)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: arg

    integer hopr, pop, hlast
    integer npop  ! for DUP
    integer,parameter :: loprnd = 3
    integer,parameter :: lpush = 2
    integer hbufo(lpush)
    integer iter
    logical isx

    ierr = 0

    hbufo = -1
    hopr = parse_term_operator(arg)
    ! write(*, *) hopr, is_operator(hopr), trim(arg)
    if (is_msglev_info(lev_verbose)) then
       if (hopr.ge.0) then
          inquire(FILE=arg, EXIST=isx, IOSTAT=ierr)
          ierr = 0
          if (isx) then
             call message(ierr, 'warning: ' // trim(arg) // ' is parsed not as a file but as a symbol')
          endif
       endif
    endif
!!!_    * output special
    if (hopr.eq.opr_OUTPUT_KEEP) then
       call stack_DUP(ierr, opr_DUP, 0)
       pop = 1
       if (ierr.eq.0) call append_queue(ierr, opr_OUTPUT, pop, 0)
    else if (hopr.eq.opr_OUTPUT_POP) then
       pop = 1
       if (ierr.eq.0) call append_queue(ierr, opr_OUTPUT, pop, 0)
!!!_    * anchor
    else if (hopr.eq.opr_MARK) then
       if (ierr.eq.0) call stack_ANCHOR(ierr, hopr)
    else if (hopr.eq.opr_STOP) then
       if (ierr.eq.0) call stack_ANCHOR(ierr, hopr)
    else if (hopr.eq.opr_GO) then
       if (ierr.eq.0) call stack_ANCHOR(ierr, hopr)
!!!_    * que special (with last queue)
    else if (hopr.eq.opr_ITER) then
       if (ierr.eq.0) call stack_ITER_opr(ierr, hopr)
    else if (hopr.eq.opr_CUM) then
       if (ierr.eq.0) call stack_CUM_opr(ierr, hopr)
!!!_    * stacking special (with top stack)
    else if (hopr.eq.opr_DIST) then
       if (ierr.eq.0) call stack_DIST(ierr, hopr)
    else if (hopr.eq.opr_INSERT) then
       if (ierr.eq.0) call stack_INSERT(ierr, hopr)
    else if (hopr.eq.opr_REPEAT) then
       if (ierr.eq.0) call stack_REPEAT(ierr, hopr)
    else if (hopr.eq.opr_DUP) then
       if (ierr.eq.0) call stack_DUP(ierr, hopr, 0)
    else if (hopr.eq.opr_COPY) then
       if (ierr.eq.0) call stack_COPY(ierr, hopr, 0)
    else if (hopr.eq.opr_CLONE) then
       if (ierr.eq.0) call stack_COPY(ierr, hopr, 0)
    else if (hopr.eq.opr_EXCH) then
       if (ierr.eq.0) call stack_EXCH(ierr, hopr, 0)
    else if (hopr.eq.opr_POP) then
       if (ierr.eq.0) call last_queue(ierr, hlast, pop, iter=iter)
       if (ierr.eq.0) then
          if (hlast.eq.hopr.and.iter.eq.0) then
             continue
          else
             pop = 0
          endif
          npop = pop + 1
       endif
       if (ierr.eq.0) call stack_POP(ierr, pop, npop, iter, arg=arg)
    else if (hopr.eq.opr_PROP) then
       if (ierr.eq.0) call last_queue(ierr, hlast, pop, iter=iter)
       if (ierr.eq.0) then
          if (hlast.eq.hopr.and.iter.eq.0) then
             continue
          else
             pop = 0
          endif
          npop = pop + 1
       endif
       if (ierr.eq.0) call stack_POP(ierr, pop, npop, iter, hopr)
    else if (ANY(hopr.eq.(/opr_FLUSH, opr_DFLUSH, opr_CFLUSH/))) then
       pop = 0
       ! npop = mstack
       npop = stacks_above_anchor(pop, 0)
       if (ierr.eq.0) call stack_POP(ierr, pop, npop, iter, hopr)
!!!_    * buffer property operator
    else if (grp_buffer_bgn.le.hopr .and. hopr.lt.grp_buffer_end) then
       call parse_buffer_opr(ierr, hopr, arg)
    else if (grp_index_bgn.le.hopr .and. hopr.lt.grp_index_end) then
       call parse_index_opr(ierr, hopr, arg)
    else if (grp_header_bgn.le.hopr .and. hopr.lt.grp_header_end) then
       call parse_header_opr(ierr, hopr, arg)
!!!_    * reduction operators
    else if (grp_reduction_bgn.le.hopr .and. hopr.lt.grp_reduction_end) then
       call parse_reduction_opr(ierr, hopr, arg)
!!!_    * normal operators
    else if (hopr.ge.0) then
       if (is_operator(hopr)) then
          if (ierr.eq.0) call stack_normal_opr(ierr, 0, 0, hopr, 0)
       else
          ! fake operator
          ierr = ERR_NO_CANDIDATE   ! not error
          ! if (ierr.eq.0) call append_queue(ierr, hopr, 0, 1, (/hopr/))
          ! if (ierr.eq.0) call push_stack(ierr, hopr)
       endif
    else
       ierr = ERR_NO_CANDIDATE   ! not error
    endif
  end subroutine parse_arg_operator

!!!_   . parse_index_opr
  subroutine parse_index_opr (ierr, hopr, arg)
    use TOUZA_Std,only: parse_number
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: hopr
    character(len=*),intent(in)  :: arg
    integer jpar, jend
    integer hbuf, jb

    ierr = 0
    call pop_stack(ierr, hbuf, .TRUE.)
    if (ierr.eq.0) jb = buf_h2item(hbuf)
    if (ierr.eq.0) ierr = min(0, jb)
    if (ierr.eq.0) then
       jpar = index(arg, param_sep) + 1
       jend = len_trim(arg) + 1
       if (jpar.eq.1) jpar = jend + 1
       select case(hopr)
       case(opr_FLAT)
          if (jpar.le.jend) then
             call parse_flat_shape(ierr, arg(jpar:), hopr)
          else
             call parse_flat_shape(ierr, ' ', hopr)
          endif
       case default
          ierr = ERR_NOT_IMPLEMENTED
          call message(ierr, 'reserved operator(index) ' // trim(arg))
       end select
    endif
    return
  end subroutine parse_index_opr

!!!_   . parse_flat_shape
  subroutine parse_flat_shape (ierr, arg, hopr)
    use TOUZA_Std,only: split_list, condop
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: arg
    integer,         intent(in)  :: hopr

    integer pop, push

    integer jc
    integer jpb, jpe, larg
    character,parameter :: csep = item_sep       ! coordinate separaor

    integer jq
    integer jsbgn, jsend
    integer jrep
    integer flag

    ierr = 0
    pop = 0
    push = 1

    if (ierr.eq.0) call stack_buffer_opr(ierr, hopr)

    jq = mqueue - 1
    jsbgn = mstack - pop
    jsend = mstack

    jc = 0
    larg = len_trim(arg)
    jpb = 0
    do
       if (jpb.ge.larg) exit
       jpe = index(arg(jpb+1:), csep) + jpb
       if (jpe.eq.jpb) jpe = larg + 1
       if (jpb+1.gt.jpe-1) then
          continue
       else if (jc.ge.lcoor) then
          ierr = ERR_INSUFFICIENT_BUFFER
          call message(ierr, 'too many ranks to SHAPE:' // trim(arg))
       else
          flag = switch_shape_operator(hopr)
          call decompose_coordinate_mod(ierr, jrep, arg=arg(jpb+1:jpe-1), flag=flag)
          if (ierr.eq.0) then
             aqueue(jq)%lcp(jc)%name = adjustl(arg(jpb+1:jpb+jrep))
             aqueue(jq)%lcp(jc)%bgn  = null_range
             aqueue(jq)%lcp(jc)%end  = null_range
             aqueue(jq)%lcp(jc)%flg  = loop_unset
             aqueue(jq)%lcp(jc)%ofs  = 0
             aqueue(jq)%lcp(jc)%cyc  = 0
          endif
       endif
       jc = jc + 1
       jpb = jpe
    enddo
    return
  end subroutine parse_flat_shape

!!!_   . parse_buffer_opr
  subroutine parse_buffer_opr (ierr, hopr, arg)
    use TOUZA_Std,only: parse_number
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: hopr
    character(len=*),intent(in)  :: arg
    integer jpar, jend
    integer hbuf, jb
    integer ci
    real(kind=KBUF) :: undef
    integer oentr

    ierr = 0
    call pop_stack(ierr, hbuf, .TRUE.)
    if (ierr.eq.0) jb = buf_h2item(hbuf)
    if (ierr.eq.0) ierr = min(0, jb)
    if (ierr.eq.0) then
       jpar = index(arg, param_sep) + 1
       jend = len_trim(arg) + 1
       if (jpar.eq.1) jpar = jend + 1
       select case(hopr)
       case(opr_TAG)
          if (jpar.lt.jend) then
             obuffer(jb)%name = arg(jpar:jend-1)
             obuffer(jb)%stt  = stt_locked
             call reg_fake_opr(oentr, hbuf, obuffer(jb)%name)
             ierr = min(0, ierr)
          endif
       case(opr_PERM,opr_SHAPE,opr_SIZE,opr_SHIFT)
          call parse_buffer_shape(ierr, arg(jpar:), hopr)
       case(opr_MISS)
          if (jpar.lt.jend) then
             call parse_number(ierr, undef, arg(jpar:jend-1))
             if (ierr.eq.0) then
                obuffer(jb)%undef  = undef
             else
                ierr = ERR_INVALID_PARAMETER
                call message(ierr, 'failed to parse ' // trim(arg))
             endif
          else
             call stack_buffer_opr(ierr, hopr)
          endif
       case(opr_C0:opr_C3)
          ci = system_index_bgn(hopr - opr_C0)
          if (ci.lt.0 .or. ci.ge.mcoor) then
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'invalid coordinate ' // trim(arg))
          else if (jpar.le.jend) then
             call parse_coordinate_opr(ierr, ci, arg(jpar:jend-1), opr_TRANSF)
          else
             call stack_buffer_opr(ierr, opr_C0 + ci)
          endif
       case(opr_X:opr_Z)
          ci = (hopr - opr_X)
          if (jpar.le.jend) then
             call parse_coordinate_opr(ierr, ci, arg(jpar:jend-1), opr_TRANSF)
          else
             call stack_buffer_opr(ierr, opr_C0 + ci)
          endif
       case default
          ierr = ERR_NOT_IMPLEMENTED
          call message(ierr, 'reserved operator(buffer) ' // trim(arg))
       end select
    endif
    return
  end subroutine parse_buffer_opr

!!!_   . parse_buffer_shape
  subroutine parse_buffer_shape (ierr, arg, hopr)
    use TOUZA_Std,only: split_list, condop
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: arg
    integer,         intent(in)  :: hopr

    integer lasth, pop, push

    integer jc
    integer jpb, jpe, larg
    character,parameter :: csep = item_sep       ! coordinate separaor

    integer jq
    integer jsbgn, jsend
    type(loop_t) :: lpp
    integer jrep
    integer flag

    ierr = 0

    if (ierr.eq.0) call last_queue(ierr, lasth, pop, push)
    if (ierr.eq.0) then
       if (lasth.ne.hopr) then
          call inquire_opr_nstack(ierr, pop, push, hopr)
          if (ierr.eq.0) then
             if (pop.ne.push) then
                ierr = ERR_PANIC
                call message(ierr, 'panic in transformation property')
                return
             endif
          endif
          if (ierr.eq.0) then
             call append_queue_stack(ierr, hopr, pop, push)
          endif
       endif
    endif

    jq = mqueue - 1
    jsbgn = mstack - pop
    jsend = mstack

    jc = 0
    larg = len_trim(arg)
    jpb = 0
    do
       if (jpb.ge.larg) exit
       jpe = index(arg(jpb+1:), csep) + jpb
       if (jpe.eq.jpb) jpe = larg + 1
       if (jpb+1.gt.jpe-1) then
          continue
       else if (jc.ge.lcoor) then
          ierr = ERR_INSUFFICIENT_BUFFER
          call message(ierr, 'too many ranks to SHAPE:' // trim(arg))
       else
          flag = switch_shape_operator(hopr)
          call decompose_coordinate_mod(ierr, jrep, lpp, arg(jpb+1:jpe-1), flag)
          if (ierr.eq.0) then
             aqueue(jq)%lcp(jc) = lpp
             aqueue(jq)%lcp(jc)%name = adjustl(arg(jpb+1:jpb+jrep))
             bstack(jsbgn:jsend-1)%lcp(jc) = lpp
             bstack(jsbgn:jsend-1)%lcp(jc)%name = adjustl(arg(jpb+1:jpb+jrep))
          endif
          ! aqueue(jq)%lefts(:)%lcp(jc)%name   = adjustl(arg(jpb+1:jpe-1))
          ! bstack(jsbgn:jsend-1)%lcp(jc)%name = adjustl(arg(jpb+1:jpe-1))
       endif
       jc = jc + 1
       jpb = jpe
    enddo
    return
  end subroutine parse_buffer_shape

!!!_   . parse_coordinate_opr
  subroutine parse_coordinate_opr (ierr, cidx, arg, hopr)
    use TOUZA_Std,only: split_list, condop
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: cidx
    character(len=*),intent(in)  :: arg
    integer,         intent(in)  :: hopr
    integer lasth
    integer jq
    integer pop, push
    type(loop_t) :: lpp
    integer jrep
    integer flag

    ierr = 0

    if (ierr.eq.0) call last_queue(ierr, lasth, pop, push)
    if (ierr.eq.0) then
       if (lasth.ne.hopr) then
          call inquire_opr_nstack(ierr, pop, push, hopr)
          if (ierr.eq.0) then
             if (pop.ne.push) then
                ierr = ERR_PANIC
                call message(ierr, 'panic in transformation property')
                return
             endif
          endif
          if (ierr.eq.0) then
             ! call append_queue(ierr, hopr, pop, push, bstack(mstack-pop:mstack-push))
             call append_queue_stack(ierr, hopr, pop, push)
          endif
       endif
    endif
    if (ierr.eq.0) then
       flag = switch_shape_operator(hopr)
       call decompose_coordinate_mod(ierr, jrep, lpp, arg, flag)
       ! write(*, *) ierr, b, e, s, arg(1:jrep)
    endif
    if (ierr.eq.0) then
       jq = mqueue - 1
       aqueue(jq)%lcp(cidx) = lpp
       aqueue(jq)%lcp(cidx)%name = arg(1:jrep)
       bstack(mstack-pop:mstack-1)%lcp(cidx) = lpp
       bstack(mstack-pop:mstack-1)%lcp(cidx)%name = arg(1:jrep)
    endif
  end subroutine parse_coordinate_opr

!!!_   . stack_buffer_opr
  subroutine stack_buffer_opr (ierr, hopr)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hopr
    integer hbuf(1)
    ierr = 0

    if (ierr.eq.0) call search_free_buffer(ierr, hbuf, 1)
    if (ierr.eq.0) call push_stack(ierr, hbuf(1))
    if (ierr.eq.0) call append_queue(ierr, hopr, 0, 1, hbuf)

  end subroutine stack_buffer_opr

!!!_   . parse_header_opr
  subroutine parse_header_opr (ierr, hopr, arg)
    use TOUZA_Nio,only: hi_ITEM, hi_TITL1, hi_UNIT, hi_EDIT1
    use TOUZA_Nio,only: hi_MISS
    use TOUZA_Nio,only: put_item, store_item
    use TOUZA_Std,only: parse_number
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: hopr
    character(len=*),intent(in)  :: arg
    integer jpar,  jend
    integer lasth, jfw, jfr
    integer jerr
    integer jitem
    integer nbufs
    integer push
    integer oentr
    real(kind=KBUF) :: undef

    ierr = 0
    if (mfile.le.bgn_file) then
       jfr = def_read
       jfw = def_write
       push = 0
    else
       call last_queue(jerr, lasth, push=push)
       if (jerr.eq.0) then
          jfr = file_h2item(lasth)
       else
          jfr = ERR_INVALID_ITEM
       endif
       jfw = jfr
    endif

    if (ierr.eq.0) then
       jpar = index(arg, param_sep) + 1
       jend = len_trim(arg) + 1
       if (jpar.eq.1) jpar = jend
       select case(hopr)
       case(opr_FMT)
          if (jfw.ge.0) then
             call set_file_format(ierr, ofile(jfw), arg(jpar:))
          else
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'no file to set format ' // trim(arg))
          endif
       case(opr_TSEL)
          if (jpar.ge.jend) then
             ierr = ERR_INVALID_PARAMETER
             call message(ierr, 'need parameter')
             return
          endif
          if (jfr.ge.0) then
             ! write(*, *) 'stack/0', mstack, push
             if (jfr.gt.def_read) then
                if (associated(ofile(jfr)%rgrp, ofile(def_read)%rgrp)) then
                   ofile(jfr)%rgrp => NULL()
                   if (ierr.eq.0) call mpop_stack(ierr, n=push)
                   if (ierr.eq.0) call reset_buffers(ierr, push)
                   if (ierr.eq.0) call modify_queue(ierr, push=0)
                   push = 0
                endif
             endif
             ! write(*, *) 'stack/1', mstack
             if (ierr.eq.0) call parse_rec_filter(ierr, nbufs, ofile(jfr), arg(jpar:))
             ! write(*, *) 'stack/1', push, nbufs
             if (jfr.gt.def_read) then
                if (ierr.eq.0) call alloc_file_buffers(ierr, oentr, nbufs, rcount-1, push)
                if (ierr.eq.0) call modify_queue_stack(ierr, push=push+nbufs)
             endif
          else
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'no file to set record filter' // trim(arg))
          endif
          ! call show_queue(ierr)
          ! write(*, *) 'stack/9', mstack
          ! call show_stack(ierr)
       case(opr_ITEM, opr_UNIT, opr_TITLE, opr_EDIT)
          select case(hopr)
          case(opr_ITEM)
             jitem = hi_ITEM
          case(opr_UNIT)
             jitem = hi_UNIT
          case(opr_TITLE)
             jitem = hi_TITL1
          case(opr_EDIT)
             jitem = hi_EDIT1
          end select
          if (jfw.ge.0) then
             call put_item(ierr, ofile(jfw)%h, arg(jpar:), jitem, 0)
          else
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'no file to set header ' // trim(arg))
          endif
       case(opr_MISS)
          if (jfw.ge.0.and.jpar.lt.jend) then
             call parse_number(jerr, undef, trim(arg(jpar:)))
             if (jerr.eq.0) then
                call put_item(ierr, ofile(jfw)%h, undef, hi_MISS)
             else
                call store_item(ierr, ofile(jfw)%h, arg(jpar:), hi_MISS)
             endif
          else
             call parse_buffer_opr(ierr, hopr, arg)
          endif
       case(opr_DUR)
          call stack_buffer_opr(ierr, hopr)
       case default
          ierr = ERR_NOT_IMPLEMENTED
          call message(ierr, 'reserved operator(header) ' // trim(arg))
       end select
    endif
    return
  end subroutine parse_header_opr

!!!_   . parse_reduction_opr
  subroutine parse_reduction_opr (ierr, hopr, arg)
    use TOUZA_Std,only: parse_number
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: hopr
    character(len=*),intent(in)  :: arg
    integer jpar, jend
    integer hbuf, jb

    ierr = 0

    if (ierr.eq.0) call pop_stack(ierr, hbuf, .TRUE.)   ! only to check,  pop later
    if (ierr.eq.0) jb = buf_h2item(hbuf)
    if (ierr.eq.0) ierr = min(0, jb)
    if (ierr.eq.0) then
       jpar = index(arg, param_sep) + 1
       jend = len_trim(arg) + 1
       if (jpar.eq.1) jpar = jend + 1
       select case(hopr)
       case(opr_COUNT)
          if (jpar.le.jend) then
             call parse_reduction_shape(ierr, arg(jpar:), hopr)
          else
             call parse_reduction_shape(ierr, ' ', hopr)
          endif
       case default
          ierr = ERR_NOT_IMPLEMENTED
          call message(ierr, 'reserved operator(reduction) ' // trim(arg))
       end select
    endif
    return
  end subroutine parse_reduction_opr

!!!_   . parse_reduction_shape
  subroutine parse_reduction_shape (ierr, arg, hopr)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: arg
    integer,         intent(in)  :: hopr

    integer pop, push

    integer jc
    integer jpb, jpe, larg
    character,parameter :: csep = item_sep       ! coordinate separaor

    integer jq
    integer jsbgn, jsend
    integer jrep
    integer flag
    integer hbuf(1)
    type(loop_t) :: lpp

    ierr = 0
    pop = 1
    push = pop

    if (ierr.eq.0) call mpop_stack(ierr, hbuf(1:pop), pop)
    if (ierr.eq.0) call search_free_buffer(ierr, hbuf(1:pop), pop)
    if (ierr.eq.0) call mpush_stack(ierr, hbuf(1:push), push)
    if (ierr.eq.0) call append_queue(ierr, hopr, pop, push, hbuf(1:pop))

    jq = mqueue - 1
    jsbgn = mstack - pop
    jsend = mstack

    jc = 0
    larg = len_trim(arg)
    jpb = 0
    do
       if (jpb.ge.larg) exit
       jpe = index(arg(jpb+1:), csep) + jpb
       if (jpe.eq.jpb) jpe = larg + 1
       if (jpb+1.gt.jpe-1) then
          continue
       else if (jc.ge.lcoor) then
          ierr = ERR_INSUFFICIENT_BUFFER
          call message(ierr, 'too many ranks to reduction:' // trim(arg))
       else
          flag = switch_shape_operator(hopr)
          call decompose_coordinate_mod(ierr, jrep, lpp, arg=arg(jpb+1:jpe-1), flag=flag)
          if (ierr.eq.0) then
             lpp%flg = loop_reduce
             ! if (lpp%end.gt.0) lpp%end = - lpp%end
             ! write(*, *) jc, lpp
             aqueue(jq)%lcp(jc) = lpp
             aqueue(jq)%lcp(jc)%name = adjustl(arg(jpb+1:jpb+jrep))
             bstack(jsbgn:jsend-1)%lcp(jc) = lpp
             bstack(jsbgn:jsend-1)%lcp(jc)%name = adjustl(arg(jpb+1:jpb+jrep))
             ! aqueue(jq)%lefts(:)%lcp(jc)%name = adjustl(arg(jpb+1:jpb+jrep))
             ! aqueue(jq)%lefts(:)%lcp(jc)%bgn  = null_range
             ! aqueue(jq)%lefts(:)%lcp(jc)%end  = null_range
             ! aqueue(jq)%lefts(:)%lcp(jc)%stp  = 0
             ! aqueue(jq)%lefts(:)%lcp(jc)%ofs  = 0
             ! aqueue(jq)%lefts(:)%lcp(jc)%cyc  = 0
          endif
       endif
       jc = jc + 1
       jpb = jpe
    enddo
    return
  end subroutine parse_reduction_shape

!!!_   . set_write_format
  subroutine set_write_format(ierr)
    implicit none
    integer,intent(out) :: ierr
    integer jfile

    ierr = 0
    if (ierr.eq.0) then
       do jfile = bgn_file, min(mfile, lfile) - 1
          if (.not.is_read_mode(ofile(jfile)%mode)) then
             if (ofile(jfile)%fmt.eq.' ') ofile(jfile)%fmt = ofile(def_write)%fmt
          endif
       enddo
    endif
  end subroutine set_write_format

!!!_   . stack_normal_opr
  subroutine stack_normal_opr(ierr, opop, npop, hopr, iter)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: opop   ! old pop size (for iter > 0)
    integer,intent(in)  :: npop   ! new pop size (for iter > 0)
    integer,intent(in)  :: hopr
    integer,intent(in)  :: iter
    integer opush
    integer pop,   push
    integer upop,  upush
    integer j, jt
    integer apos, jpos, jdst
    integer lefth(0:max_operands-1)
    character(len=8) :: opr
    integer hbru, htmp(1)

    ierr = 0

    if (ierr.eq.0) call inquire_opr_nstack(ierr, upop, upush, hopr)
    if (ierr.eq.0) then
       if (hopr.eq.opr_NOP) then
          call append_queue(ierr, hopr, upop, upush)
          return
       else if (upop.le.0.or.upush.le.0) then
          call query_opr_name(ierr, opr, hopr)
          ierr = ERR_NOT_IMPLEMENTED
          call message(ierr, 'reserved operator(normal) ' // trim(opr))
       endif
    endif
    if (ierr.eq.0) then
       if (iter.eq.0) then
          pop = upop
          push = upush
          opush = 0
          if (mstack.lt.pop) then
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'few operands')
          else
             apos = last_anchor()
             if (mstack - (apos + 1).lt.pop) then
                ierr = ERR_INVALID_ITEM
                call message(ierr, 'anchor inside operands')
             endif
          endif
       else if (mod(npop, upop).ne.0) then
          ierr = ERR_INVALID_ITEM
          call message(ierr, 'invalid operands to iterate.')
       else
          opush = (opop / upop) * upush
          push = (npop / upop) * upush
          pop  = npop
       endif
    endif
    if (ierr.eq.0) then
       apos = mstack - pop - opush + opop
       if (is_operator_modify(hopr)) then
          continue
       else if (is_operator_reusable(hopr)) then
          ! pop == push
          hbru = -1
          do j = pop - opop - 1, 0, -1
             jpos = apos + j
             if (hbru.lt.0) then
                hbru = bstack(jpos)%bh
                if (ierr.eq.0) call search_free_buffer(ierr, htmp, 1)
                bstack(jpos)%bh = htmp(1)
             else
                htmp(1) = hbru
                hbru = bstack(jpos)%bh
                bstack(jpos)%bh = htmp(1)
             endif
             bstack(jpos)%lcp = def_loop
             if (is_buffer_locked(hbru)) hbru = -1
          enddo
       else if (max_operands.lt.upop.or.upop.lt.upush) then
          ierr = ERR_PANIC
          call message(ierr, 'PANIC, invalid operands')
          return
       else
          ! upop >= upush
          jdst = apos
          do j = pop - opop - upop, 0, -upop
             jpos = apos + j
             if (ierr.eq.0) call search_free_buffer(ierr, lefth, upush)
             bstack(jpos+upush:jpos+upop-1)%bh = -1
             bstack(jpos:jpos+upush-1)%bh = lefth(0:upush-1)
             do jt = jpos, jpos+upush-1
                bstack(jt)%lcp = def_loop
             enddo
          enddo
          if (opop.gt.0) then
             jpos = mstack - opush
             bstack(apos:apos+push-1) = bstack(apos:mstack-1:upop)
          endif
          if (ierr.eq.0) call mpop_stack(ierr, n=(pop - push - (opop - opush)))
       endif
    endif
    if (ierr.eq.0) then
       if (iter.eq.0) then
          call append_queue_stack(ierr, hopr, pop, push, iter)
       else
          call modify_queue_stack(ierr, pop, push, niter=iter)
       endif
    endif
    call message(ierr, 'stack_normal_opr', levm=-9)
  end subroutine stack_normal_opr

!!!_   . stack_ITER_opr
  subroutine stack_ITER_opr(ierr, hopr)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hopr
    integer lasth
    integer opop, opush, oiter
    integer npop, niter
    integer pop,  push
    integer apos, alev
!!!_    * note
    !  put the last queue term on every input step or 1 until the mark
    ! -1+1   A B C    SQR  ITER   ==  A SQR     B SQR   C SQR
    ! -2+1   A B C    SUB  ITER   ==  (error)
    ! -2+1   A B C D  SUB  ITER   ==  A B SUB   C D SUB
    ! -1+2   A B C    DUP  ITER   ==  A DUP     B DUP   C DUP    special
    ! -2+2   A B C D  EXCH ITER   ==  A B EXCH  C D EXCH         special
!!!_    * body
    ierr = 0
    if (ierr.eq.0) call last_queue(ierr, lasth, opop, opush, oiter)
    if (ierr.eq.0) then
       if (.not.is_operator(lasth)) then
          ierr = ERR_INVALID_ITEM
          call message(ierr, 'ITER requires operator')
       endif
    endif
    if (ierr.eq.0) then
       if (lasth.eq.opr_DUP) then
          call stack_DUP(ierr, lasth, 1)
       else if (lasth.eq.opr_EXCH) then
          call stack_EXCH(ierr, lasth, 1)
       else if (ANY(lasth.eq.(/opr_POP, opr_PROP/))) then
          npop = stacks_above_anchor(opop, opush)
          call stack_POP(ierr, opop, npop, 1, lasth)
       else if (lasth.eq.opr_DIST) then
          call stack_DIST(ierr, lasth)
       else if (lasth.eq.opr_INSERT) then
          call stack_INSERT(ierr, lasth)
       else if (lasth.eq.opr_REPEAT) then
          call stack_REPEAT(ierr, lasth)
       else
          call inquire_opr_nstack(ierr, pop, push, lasth)
          if (push.le.0.or.pop.le.0) then
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'non ITERable operator')
          else if (lasth.eq.opr_TRANSF) then
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'non ITERable operator (reserved)')
          else if (oiter.ne.0) then
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'cannot repeat ITER and/or CUM')
          else
             niter = 1
             apos = last_anchor()
             npop = mstack - (apos + 1) + opop - opush
             call stack_normal_opr(ierr, opop, npop, lasth, niter)
             alev = anchor_level(apos)
             if (alev.gt.0) then
                bstack(apos:mstack-2) = bstack(apos+1:mstack-1)
                call modify_queue(ierr, npop+1)
                call pop_stack(ierr)
             endif
          endif
       endif
    endif
  end subroutine stack_ITER_opr

!!!_   . stack_CUM_opr
  subroutine stack_CUM_opr(ierr, hopr)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hopr

    integer lasth, opop, opush, iter
    integer upop, upush
    integer npop, npush
    integer alev, apos
    integer jsrc
!!!_    * note
!!!_    * body
    ierr = 0
    if (ierr.eq.0) call last_queue(ierr, lasth, opop, opush, iter=iter)
    if (ierr.eq.0) then
       if (.not.is_operator(lasth)) then
          ierr = ERR_INVALID_ITEM
          call message(ierr, 'missing operation for CUM')
       else if (iter.ne.0) then
          ierr = ERR_INVALID_ITEM
          call message(ierr, 'cannot repeat CUM and/or ITER')
       endif
    endif
    if (ierr.eq.0) call inquire_opr_nstack(ierr, upop, upush, lasth)
    if (ierr.eq.0) then
       if (upush.ne.1.or.upop.le.1) then
          ierr = ERR_INVALID_ITEM
          call message(ierr, 'invalid operation for CUM')
          return
       endif
    endif

    if (ierr.eq.0) then
       apos = last_anchor()
       alev = anchor_level(apos)
       npush = upush
       npop = mstack - (apos + 1) + opop - opush
       jsrc = mstack - opush
       bstack(apos+1:apos+upush) = bstack(jsrc:jsrc+upush-1)
       if (ierr.eq.0) call mpop_stack(ierr, n=(npop - npush - (opop - opush)))
       if (alev.gt.0) then
          bstack(apos:mstack-2) = bstack(apos+1:mstack-1)
          call pop_stack(ierr)
          npop = npop + 1
       endif
    endif
    if (ierr.eq.0) call modify_queue(ierr, npop, niter=-1)
  end subroutine stack_CUM_opr

!!!_   . stack_DIST - cumulative DIST
  subroutine stack_DIST(ierr, hopr)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hopr

    integer lasth
    integer opop, opush, oiter
    integer npop, npush, niter
    integer apos, alev,  nopr
    integer jsrc, jdst,  j
    integer dpos
    integer nadd
!!!_    * note
    !    MARK A B C D E DIST                     == A E      B E     C E   D E    -1-5+8
    !    MARK A B C D E DIST DIST                == A D E    B D E   C D E        -1-5+9
    !    MARK A B C D E DIST DIST DIST           == A C D E  B C D E              -1-5+8
    !    MARK A B C D E DIST DIST DIST DIST      == A B C D E                     -1-5+5
    !    MARK A B C D E DIST DIST DIST DIST DIST == error
!!!_    * body
    ierr = 0
    if (ierr.eq.0) call last_queue(ierr, lasth, opop, opush, oiter)
    if (ierr.eq.0) then
       if (lasth.ne.hopr) then
          opop = 0
          opush = 0
          oiter = 0
       endif
    endif
    if (ierr.eq.0) then
       niter = oiter + 1
       if (oiter.eq.0) then
          apos = last_anchor()
          alev = anchor_level(apos)
          nopr = mstack - (apos + 1) - 1
          npop = nopr + 1
          opush = npop
          if (alev.gt.0) npop = npop + 1
       else
          apos = mstack - opush - 1
          alev = 0
          nopr = opush / niter - 1
          npop = opop
       endif
       npush = nopr * (niter + 1)
       dpos = mstack - niter
       if (npush.eq.0) then
          ierr = ERR_INVALID_ITEM
          call message(ierr, 'reached maximum DIST sequence.')
       endif
    endif
    if (ierr.eq.0) then
       nadd = npush - opush
       if (nadd.gt.0) then
          call mpush_stack(ierr, n=nadd)
       else if (nadd.lt.0) then
          call mpop_stack(ierr, n=-nadd)
       endif
    endif
    if (ierr.eq.0) then
       bstack(mstack-niter:mstack-1) = bstack(dpos:dpos+niter-1)
       do j = nopr - 1, 0, -1
          jsrc = apos + 1 + j * niter
          jdst = apos + 1 + j * (niter + 1)
          bstack(jdst) = bstack(jsrc)
          bstack(jdst+1:jdst+niter) = bstack(mstack-niter:mstack-1)
       enddo
       if (alev.gt.0) then
          ! fragile anchor
          bstack(apos:mstack-2) = bstack(apos+1:mstack-1)
          apos = apos - 1
          call pop_stack(ierr)
       endif
    endif
    if (ierr.eq.0) then
       if (mstack-(apos+1).ne.npush) then
          ierr = ERR_PANIC
          call message &
               & (ierr, 'assertion:stack_DIST. please report to maintainer ', (/mstack, apos, npush/))
          return
       endif
       if (oiter.eq.0) then
          call append_queue_stack(ierr, hopr, npop, npush, niter)
       else
          call modify_queue_stack(ierr, npop, npush, niter=niter)
       endif
    endif
  end subroutine stack_DIST

!!!_   . stack_REPEAT - cumulative REPEAT
  subroutine stack_REPEAT(ierr, hopr)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hopr

    integer lasth
    integer apop, apush
    integer opop, opush, oiter
    integer npop, npush, niter
    integer apos, alev
    integer nopr
!!!_    * note (example)
    !      STOP A B C      REPEAT             ==  STOP A B C      A B C
    !      MARK A B C      REPEAT             ==       A B C      A B C
    !      MARK A B C MARK REPEAT             ==       A B C MARK A B C
    !      STOP A B C MARK REPEAT             ==  STOP A B C MARK A B C
    !      MARK A B C STOP REPEAT             ==       A B C STOP A B C
    !      STOP A B C STOP REPEAT             ==  STOP A B C STOP A B C
    !      MARK A B C MARK REPEAT REPEAT      ==       A B C MARK A B C      A B C
    !      STOP A B C      REPEAT REPEAT      ==  STOP A B C      A B C      A B C
    !      STOP A B C      REPEAT STOP REPEAT ==  STOP A B C STOP A B C STOP A B C

    !      X MARK A B C REPEAT NOP REPEAT     ==  X A B C A B C  X A B C A B C

    !  anchor prefix is kept, not consumed during REPEAT

!!!_    * note (queue%iter usage)
    !    queue%iter stores number of operands for this operation.
!!!_    * body
    ierr = 0
    if (ierr.eq.0) call last_queue(ierr, lasth, apop, apush, oiter)
    if (ierr.eq.0) then
       if (lasth.eq.opr_ANCHOR.and.apop.eq.0) then
          call pop_queue(ierr)
          if (ierr.eq.0) call last_queue(ierr, lasth, opop, opush, oiter)
       else
          opop = apop
          opush = apush
          apop = 0
          apush = 0
       endif
    endif
    if (ierr.eq.0) then
       if (lasth.ne.hopr) then
          oiter = 0
          opop = 0
          opush = 0
       endif
    endif
    if (ierr.eq.0) then
       if (oiter.eq.0) then
          apos = last_anchor(- apush - 1)
          alev = anchor_level(apos)
          nopr = mstack - (apos + 1) - apush
          npush = nopr + apush + nopr
       else
          apos = mstack - opush - apush - 1
          alev = 0
          nopr = oiter
          npush = opush + apush + nopr
       endif
       npop = nopr
       call mpush_stack(ierr, n=nopr)
       if (ierr.eq.0) bstack(mstack-nopr:mstack-1) = bstack(apos+1:apos+nopr)
       niter = nopr
       if (alev.gt.0) then
          ! fragile anchor
          bstack(apos:mstack-2) = bstack(apos+1:mstack-1)
          call pop_stack(ierr)
          apos = apos - 1
          npop = npop + 1
       endif
       if (oiter.eq.0) then
          call append_queue_stack(ierr, hopr, npop, npush, niter)
       else
          call modify_queue_stack(ierr, npop, npush, niter=niter)
       endif
    endif
  end subroutine stack_REPEAT

!!!_   . stack_INSERT - cumulative INSERT
  subroutine stack_INSERT(ierr, hopr)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hopr

    integer lasth
    integer opop, opush, oiter
    integer npop, npush, niter
    integer apos, alev
    type(stack_t) :: bins
!!!_    * note
    !      MARK A B C D INSERT          ==      D A B C
    !      MARK A B C D INSERT INSERT   ==      C D A B
    !      STOP A B C D INSERT INSERT   == STOP C D A B
!!!_    * body
    ierr = 0
    if (ierr.eq.0) call last_queue(ierr, lasth, opop, opush, iter=oiter)
    if (ierr.eq.0) then
       if (lasth.ne.hopr) then
          opop = 0
          opush = 0
          oiter = 0
       endif
    endif
    if (ierr.eq.0) then
       if (oiter.eq.0) then
          apos = last_anchor()
          alev = anchor_level(apos)
          npop = mstack - (apos + 1)
          npush = npop
          if (alev.gt.0) npop = npop + 1
       else
          apos = mstack - opush - 1
          alev = 0
          npush = opush
          npop = opop
       endif
       niter = oiter + 1
       bins = bstack(mstack - 1)
       bstack(apos+2:mstack-1) = bstack(apos+1:mstack-2)
       bstack(apos+1) = bins
       if (alev.gt.0) then
          bstack(apos:mstack-2) = bstack(apos+1:mstack-1)
          call pop_stack(ierr)
       endif
    endif
    if (ierr.eq.0) then
       if (oiter.eq.0) then
          call append_queue_stack(ierr, hopr, npop, npush, iter=niter)
       else
          call modify_queue_stack(ierr, npop, npush, niter=niter)
       endif
    endif
  end subroutine stack_INSERT

!!!_   . stack_DUP - cumulative DUP
  subroutine stack_DUP(ierr, hopr, iter)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hopr
    integer,intent(in)  :: iter   ! 0 if bare DUP
    integer lasth
    integer oiter, opop, opush
    integer niter, npop, npush
    integer apos,  alev, nadd,  nopr
    integer jsrc,  jdst, j
    type(stack_t) :: tops(1)
!!!_    * Note
    !      A     DUP                   ==  A DUP[0+1]   AA       (fake -1+2 stacking, to avoid anchor between)
    !      A     DUP DUP               ==  A DUP[0+2]   AAA
    !      A     DUP DUP DUP           ==  A DUP[0+3]   AAAA
    ! STOP A     DUP         ITER      ==  A DUP[-1+2]  AA            (*)
    ! STOP A     DUP DUP     ITER      ==  A DUP[-1+3]  AAA           (*)
    ! STOP A     DUP         ITER ITER ==  A DUP[-1+3]  AAA
    ! STOP A B C DUP         ITER      ==  A DUP[-3+6]  AA BB CC      (*)
    ! STOP A B C DUP         ITER ITER ==  A DUP[-3+9]  AAA BBB CCC
    ! STOP A B C DUP DUP     ITER      ==  A DUP[-3+9]  AAA BBB CCC
    ! STOP A B C DUP DUP     ITER ITER ==  A DUP[-3+12] AAAA BBBB CCCC

    ! (*) First ITER distributes DUP at every operand,
    !     which means STOP A DUP ITER is equivalent to A DUP.

    ! Special case with anchor

    ! A STOP DUP      == A STOP A
    ! A STOP DUP DUP  == A STOP A A
    ! A STOP DUP ITER == *error*      Anchor inside operands to ITERated DUP raise an error.

!!!_    * body
    ierr = 0

    call last_queue(ierr, lasth, opop, opush, oiter)
    if (iter.eq.0) then
       ! bare DUP
       if (ierr.eq.0) then
          npop = 0
          call pop_stack_st(ierr, tops(1), keep=.TRUE., anchor=.FALSE.)
          if (ierr.eq.0) call push_stack_st(ierr, tops(1))
          if (lasth.ne.hopr.or.oiter.gt.0) then
             ! initial DUP
             npush = 1
             if (ierr.eq.0) call append_queue_stack(ierr, hopr, npop, npush)
          else
             npush = opush + 1
             ! succesive DUP
             if (ierr.eq.0) then
                call modify_queue_stack(ierr, npop, npush)
             endif
          endif
       endif
    else
       ! ITERated DUP
       apos = last_anchor()
       alev = anchor_level(apos)
       if (oiter.eq.0) then
          oiter = 1
          opush = opush + 1
          opop = 1
          niter = opush
          nopr  = mstack - (apos + 1) - (opush - 1)
          npop  = nopr
          npush = nopr * opush
          nadd = (npush - npop) - (opush - opop)
       else
          niter = oiter + 1
          nopr  = opush / oiter
          npop  = opop
          npush = opush + nopr
          nadd  = nopr
       endif
       if (npop.le.0.or.nopr.le.0) then
          ierr = ERR_INVALID_ITEM
          call message(ierr, 'Anchor inside DUPlication')
       endif
       if (ierr.eq.0) then
          if (mod(mstack + nadd - (apos + 1), nopr).ne.0) then
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'Anchor shifted during ITERated DUP')
          endif
       endif
       if (ierr.eq.0) then
          call mpush_stack(ierr, n=nadd)
          do j = nopr - 1, 0, -1
             jsrc = (apos + 1) + j * oiter
             jdst = (apos + 1) + j * niter
             bstack(jdst:jdst+niter-1) = bstack(jsrc)
          enddo
          if (alev.gt.0) then
             npop = npop + 1
             bstack(apos:mstack-2) = bstack(apos+1:mstack-1)
             call pop_stack(ierr)
          endif
          call modify_queue_stack(ierr, npop, npush, niter=niter)
       endif
    endif
  end subroutine stack_DUP

!!!_   . stack_COPY - non-cumulative COPY or CLONE
  subroutine stack_COPY(ierr, hopr, iter)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hopr
    integer,intent(in)  :: iter   ! 0 if bare DUP
    integer copyh(2)
    integer npop, npush
!!!_    * body
    ierr = 0

    if (ierr.eq.0) then
       if (hopr.eq.opr_CLONE) then
          npop = 1
          npush = 1
          if (ierr.eq.0) call pop_stack(ierr, copyh(1), anchor=.FALSE.)
          if (ierr.eq.0) call search_free_buffer(ierr, copyh(1:1), 1)
          if (ierr.eq.0) call push_stack(ierr, copyh(1))
          if (ierr.eq.0) call append_queue(ierr, opr_COPY, npop, npush, copyh(1:1))
       else
          npop = 1
          npush = 2
          if (ierr.eq.0) call pop_stack(ierr, copyh(1), keep=.TRUE., anchor=.FALSE.)
          if (ierr.eq.0) call search_free_buffer(ierr, copyh(2:2), 1)
          if (ierr.eq.0) call push_stack(ierr, copyh(2))
          if (ierr.eq.0) call append_queue(ierr, hopr, npop, npush, copyh)
       endif
    endif
  end subroutine stack_COPY

!!!_   . stack_EXCH - cumulative EXCH.  ITERated EXCH queue is compacted.
  subroutine stack_EXCH(ierr, hopr, iter)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hopr
    integer,intent(in)  :: iter

    integer lasth
    integer oiter, opop, opush
    integer niter, npop, npush
    integer apos,  alev, nopr
    type(stack_t) :: bexch
    integer jpos
    integer jsrc,  jdst, j
!!!_    * note i
    !      A B         EXCH      =  B A
    ! STOP A B         EXCH ITER =  B A
    ! STOP A B C D     EXCH ITER =  B A D C
    ! STOP A B C D E F EXCH ITER =  B A D C F E

    ! A STOP B EXCH              =  B STOP A
    ! A B C STOP D EXCH ITER     =  *error*
!!!_    * note ii
    ! Due to the limit of implementation, strange operation can be
    ! occured for ITERated EXCH.
    ! STOP A B  C D EXCH ITER ITER = A B  C D
    ! STOP A B  C D EXCH EXCH ITER = A B  D C
    ! Successive EXCH is meaningless and rarely used, so this feature is left.
!!!_    * note iii
    ! Although EXCH EXCH is identical to nothing,
    ! a special meaning is NOT given for successive EXCH queue.
    ! I implemeted once, but it is rather confusing.
!!!_    * body
    ierr = 0

    call last_queue(ierr, lasth, opop, opush, oiter)

    if (iter.eq.0) then
       ! bare EXCH
       if (ierr.eq.0) then
          if (lasth.eq.hopr.and.oiter.eq.0) then
             ! cumulative EXCH
             bexch = bstack(mstack - 1)
             bstack(mstack - 1)    = bstack(mstack - opop)
             bstack(mstack - opop) = bexch
             call modify_queue_stack(ierr, push=opop)
          else
             ! initial EXCH
             bexch = bstack(mstack - 1)
             if (is_anchor(bexch%bh)) then
                ierr = ERR_INVALID_ITEM
                call message(ierr, 'Anchor on the top')
             endif
             if (ierr.eq.0) then
                opop = 1
                do
                   opop = opop + 1
                   jpos = mstack - opop
                   if (jpos.lt.0) then
                      ierr = ERR_INVALID_ITEM
                      call message(ierr, 'No other operands to EXCH')
                      exit
                   endif
                   if (.not.is_anchor(bstack(jpos)%bh)) exit
                enddo
             endif
             if (ierr.eq.0) then
                bstack(mstack - 1) = bstack(jpos)
                bstack(jpos) = bexch
                call append_queue_stack(ierr, hopr, opop, opop)
             endif
          endif
       endif
    else
       ! ITERated EXCH
       if (ierr.eq.0) then
          niter = oiter + 1
          if (oiter.eq.0) then
             apos = last_anchor()
             alev = anchor_level(apos)
             nopr = mstack - (apos + 1)
             npush = nopr
             npop = nopr
             opush = nopr - 2
             if (alev.gt.0) npop = npop + 1
          else
             apos = mstack - opush - 1
             alev = 0
             nopr = opush
             npush = nopr
             npop = opop
          endif
          if (mod(nopr, 2).ne.0) then
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'EXCH operation on odd operands.')
          endif
       endif
       if (ierr.eq.0) then
          do j = 0, opush / 2 - 1
             jsrc = apos + 1 + j * 2
             jdst = jsrc + 1
             bexch = bstack(jsrc)
             bstack(jsrc) = bstack(jdst)
             bstack(jdst) = bexch
          enddo
          if (alev.gt.0) then
             bstack(apos:mstack-2) = bstack(apos+1:mstack-1)
             call pop_stack(ierr)
          endif
       endif
       if (ierr.eq.0) then
          call modify_queue_stack(ierr, npop, npush, niter=niter)
       endif
    endif
  end subroutine stack_EXCH

!!!_   . stack_POP - cumulative POP.  Succesive and/or ITERated POP queue is compacted.
  subroutine stack_POP(ierr, opop, npop, niter, hopr, arg)
    use TOUZA_Std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: opop
    integer,         intent(in)          :: npop
    integer,         intent(in)          :: niter
    integer,         intent(in),optional :: hopr
    character(len=*),intent(in),optional :: arg

    integer h
    integer righth(npop)
    integer lefth(1) ! dummy

    ierr = 0
    if (present(arg)) then
       call parse_buffer_opr(ierr, opr_TAG, arg)
    endif
    if (ierr.eq.0) call mpop_stack(ierr, righth, npop - opop)
    if (ierr.eq.0) then
       h = choice(opr_POP, hopr)
       if (opop.eq.0) then
          call append_queue(ierr, h, npop, 0, lefth)
       else
          call modify_queue(ierr, npop, 0, lefth, niter=niter)
       endif
    endif
  end subroutine stack_POP
!!!_   . stack_ANCHOR
  subroutine stack_ANCHOR(ierr, hopr)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hopr
    integer alev
    integer anch
    integer apos,  npop, npush
    integer lasth, opop, opush
    integer jdmy
    integer oprq

    ierr = 0
    alev = 0
    oprq = opr_ANCHOR

    select case(hopr)
    case(opr_MARK)
       alev = +1
    case(opr_STOP)
       alev = 0
    case(opr_GO)
       alev = -1
    case default
       ierr = ERR_INVALID_PARAMETER
       call message(ierr, 'unknown operation for anchor')
    end select

    if (ierr.eq.0) then
       call last_queue(jdmy, lasth, opop, opush)
       if (lasth.ne.oprq) then
          opop = 0
          opush = 0
       endif
    endif

    if (ierr.eq.0) then
       if (alev.lt.0) then
          apos = last_anchor()
          if (apos.ge.0) then
             npop = mstack - apos
             npush = npop - 1
             if (opop.eq.0) then
                call append_queue_stack(ierr, oprq, npop, npush)
             else
                npop = opop - opush + npop
                call modify_queue_stack(ierr, npop, npush)
             endif
             mstack = mstack - 1
             ! hard-coded shift
             bstack(apos:mstack-1) = bstack(apos+1:mstack)
          endif
       else
          anch = anchor_handle(alev)
          if (ierr.eq.0) call push_stack(ierr, anch)
          if (ierr.eq.0) then
             if (opush.eq.0) then
                call append_queue(ierr, oprq, 0, 1, (/anch/))
             else
                npush = opush + 1
                call modify_queue_stack(ierr, 0, npush)
             endif
          endif
       endif
    endif

  end subroutine stack_ANCHOR

!!!_   & anchor_handle ()
  integer function anchor_handle (lev) result(h)
    implicit none
    integer,intent(in) :: lev
    if (lev.ge.0) then
       h = ofs_anchor + lev
    else
       h = -1
    endif
  end function anchor_handle

!!!_   & anchor_level ()
  integer function anchor_level (pos) result(n)
    integer,intent(in) :: pos
    if (pos.lt.0) then
       n = 0
    else if (pos.lt.mstack) then
       n = anchor_h2level(bstack(pos)%bh)
    else
       n = -1
    endif
  end function anchor_level

!!!_   & anchor_h2level ()
  integer function anchor_h2level (handle) result(n)
    implicit none
    integer,intent(in) :: handle
    ! negative if robust, positive if fragile, 0 otherwise
    n = handle_type(handle)
    if (n.eq.hk_anchor) then
       n = handle - ofs_anchor
    else
       n = -1
    endif
    return
  end function anchor_h2level

!!!_   & last_anchor ()
  integer function last_anchor(start) result(n)
    use TOUZA_Std,only: choice
    implicit none
    integer,optional,intent(in) :: start
    integer j, m
    n = -1
    m = choice(-1, start)
    if (m.lt.0) then
       do j = min(mstack,lstack) + m, 0, -1
          if (is_anchor(bstack(j)%bh)) then
             n = j
             exit
          endif
       enddo
    else
       do j = m, min(mstack,lstack) - 1
          if (is_anchor(bstack(j)%bh)) then
             n = j
             exit
          endif
       enddo
    endif
  end function last_anchor

!!!_   & stacks_above_anchor ()
  integer function stacks_above_anchor (pop, push) result(n)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(in),optional :: pop, push
    integer apos, alev
    apos = last_anchor()
    alev = anchor_level(apos)
    n = mstack - (apos + 1) + max(0, min(1, alev))
    n = n + choice(0, pop) - choice(0, push)
  end function stacks_above_anchor

!!!_  - [a-]queue manager
!!!_   . append_queue_stack - append queue from top stacks
  subroutine append_queue_stack &
       & (ierr, handle, pop, push, iter)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: handle
    integer,intent(in)          :: pop, push
    integer,intent(in),optional :: iter
    integer jq

    ierr = 0

    jq = mqueue
    mqueue = mqueue + 1
    if (jq.lt.lqueue) then
       if (aqueue(jq)%term.ge.0) then
          deallocate(aqueue(jq)%lefts, STAT=ierr)
       endif
       if (ierr.eq.0) then
          aqueue(jq)%term = handle
          aqueue(jq)%nopr = pop
          aqueue(jq)%desci = ' '
          aqueue(jq)%desco = ' '
          aqueue(jq)%iter = choice(0, iter)
          aqueue(jq)%lcp(:) = def_loop
          allocate(aqueue(jq)%lefts(0:push-1), STAT=ierr)
       endif
       if (ierr.eq.0) then
          aqueue(jq)%lefts(0:push-1) = bstack(mstack-push:mstack-1)
       endif
    else
       ierr = ERR_INSUFFICIENT_BUFFER
    endif

  end subroutine append_queue_stack
!!!_   . append_queue
  subroutine append_queue &
       & (ierr, handle, pop, push, bufh, iter)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: handle
    integer,intent(in)          :: pop, push
    integer,intent(in),optional :: bufh(0:*)
    integer,intent(in),optional :: iter

    integer jq, j

    ierr = 0

    jq = mqueue
    mqueue = mqueue + 1
    if (jq.lt.lqueue) then
       if (aqueue(jq)%term.ge.0) then
          deallocate(aqueue(jq)%lefts, STAT=ierr)
       endif
       if (ierr.eq.0) then
          aqueue(jq)%term = handle
          aqueue(jq)%nopr = pop
          aqueue(jq)%desci = ' '
          aqueue(jq)%desco = ' '
          aqueue(jq)%iter = choice(0, iter)
          aqueue(jq)%lcp(:) = def_loop
          allocate(aqueue(jq)%lefts(0:push-1), STAT=ierr)
       endif
       if (ierr.eq.0) then
          aqueue(jq)%lefts(:)%bh = -1
          do j = 0, push - 1
             aqueue(jq)%lefts(j)%lcp(:) = def_loop
          enddo
          if (present(bufh)) then
             aqueue(jq)%lefts(0:push-1)%bh = bufh(0:push-1)
          endif
       endif
       ! write(*, *) 'append_queue', ierr, jq, push, pop
    else
       ierr = ERR_INSUFFICIENT_BUFFER
    endif
  end subroutine append_queue

!!!_   . modify_queue_stack
  subroutine modify_queue_stack &
       & (ierr, pop, push, term, niter, cmode, jqueue)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: pop
    integer,intent(in)          :: push
    integer,intent(in),optional :: term
    integer,intent(in),optional :: niter
    integer,intent(in),optional :: cmode
    integer,intent(in),optional :: jqueue

    integer jq

    ierr = 0
    jq = choice(-1, jqueue)
    if (jq.lt.0) jq = mqueue + jq
    if (jq.lt.0.or.jq.ge.lqueue) then
       ierr = ERR_INVALID_PARAMETER
    endif
    if (ierr.eq.0) then
       if (present(pop)) then
          aqueue(jq)%nopr = pop
       endif

       deallocate(aqueue(jq)%lefts, STAT=ierr)
       if (ierr.eq.0) allocate(aqueue(jq)%lefts(0:push-1), STAT=ierr)
       if (ierr.eq.0) aqueue(jq)%lefts(0:push-1) = bstack(mstack-push:mstack-1)

       if (present(term)) then
          aqueue(jq)%term = term
       endif
       if (present(niter)) then
          aqueue(jq)%iter = niter
       endif
       if (present(cmode)) then
          aqueue(jq)%cmode = cmode
       endif
    endif

    return
  end subroutine modify_queue_stack

!!!_   . modify_queue
  subroutine modify_queue &
       & (ierr, pop, push, bufh, term, niter, cmode, jqueue)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: pop
    integer,intent(in),optional :: push
    integer,intent(in),optional :: bufh(0:*)
    integer,intent(in),optional :: term
    integer,intent(in),optional :: niter
    integer,intent(in),optional :: cmode
    integer,intent(in),optional :: jqueue

    integer jq

    ierr = 0
    jq = choice(-1, jqueue)
    if (jq.lt.0) jq = mqueue + jq
    if (jq.lt.0.or.jq.ge.lqueue) then
       ierr = ERR_INVALID_PARAMETER
    endif
    if (ierr.eq.0) then
       if (present(pop)) then
          aqueue(jq)%nopr = pop
       endif
       if (present(push)) then
          if (push.eq.0) then
             deallocate(aqueue(jq)%lefts, STAT=ierr)
             if (ierr.eq.0) allocate(aqueue(jq)%lefts(0:push-1), STAT=ierr)
          else if (present(bufh)) then
             deallocate(aqueue(jq)%lefts, STAT=ierr)
             if (ierr.eq.0) allocate(aqueue(jq)%lefts(0:push-1), STAT=ierr)
             if (ierr.eq.0) aqueue(jq)%lefts(0:push-1)%bh = bufh(0:push-1)
          else
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'need both push and bufh')
          endif
       endif
       if (present(term)) then
          aqueue(jq)%term = term
       endif
       if (present(niter)) then
          aqueue(jq)%iter = niter
       endif
       if (present(cmode)) then
          aqueue(jq)%cmode = cmode
       endif
    endif

  end subroutine modify_queue

!!!_   . last_queue
  subroutine last_queue &
       & (ierr, handle, pop, push, iter)
    use TOUZA_Std,only: set_if_present
    implicit none
    integer,intent(out)          :: ierr
    integer,intent(out)          :: handle
    integer,intent(out),optional :: pop
    integer,intent(out),optional :: push
    integer,intent(out),optional :: iter
    integer jq

    ierr = 0
    jq = mqueue - 1
    if (jq.lt.0) then
       handle = -1
       call set_if_present(pop, -1)
       call set_if_present(push, -1)
    else
       handle = aqueue(jq)%term
       call set_if_present(pop, aqueue(jq)%nopr)
       call set_if_present(push, size(aqueue(jq)%lefts))
       call set_if_present(iter, aqueue(jq)%iter)
    endif
  end subroutine last_queue
!!!_   . pop_queue
  subroutine pop_queue &
       & (ierr, handle)
    integer,intent(out)          :: ierr
    integer,intent(out),optional :: handle
    integer jq

    ierr = 0
    mqueue = mqueue - 1
    jq = mqueue
    if (jq.lt.0) then
       ierr = ERR_INVALID_ITEM
       if (present(handle)) then
          handle = -1
       endif
    else
       if (present(handle)) then
          handle = aqueue(jq)%term
       endif
    endif
  end subroutine pop_queue

!!!_   . set_queue_descr
  subroutine set_queue_descr(ierr, aq)
    implicit none
    integer,      intent(out)   :: ierr
    type(queue_t),intent(inout) :: aq
    integer push, pop
    ierr = 0
    push = size(aq%lefts)
    pop  = aq%nopr
    if (ierr.eq.0) call get_obj_list(ierr, aq%desco, aq%lefts, push)
    if (ierr.eq.0) call get_obj_list(ierr, aq%desci, bstack(mstack-pop:mstack-1), pop)
    call message(ierr, 'trace:set_queue_descr', levm=-9)
  end subroutine set_queue_descr

!!!_   . trace_queue
  subroutine trace_queue (ierr, aq, levv, u)
    use TOUZA_Std,only: choice
    implicit none
    integer,         intent(out) :: ierr
    type(queue_t),   intent(in)  :: aq
    integer,optional,intent(in)  :: levv
    integer,optional,intent(in)  :: u
    integer lv, utmp
    integer handle
    integer j

    ierr = 0
    lv = choice(lev_verbose, levv)
    utmp = choice(ulog, u)

    handle = aq%term

    j = file_h2item(handle)
    if (j.ge.0) then
       call trace_file_access(ierr, handle, aq, lv, utmp)
    else
       j = buf_h2item(handle)
       if (j.ge.0) then
          call trace_buffer_access(ierr, handle, utmp)
       else
          call trace_operation(ierr, aq, utmp)
       endif
    endif
    call message(ierr, 'trace_queue', levm=-9)
  end subroutine trace_queue

!!!_   . trace_file_access
  subroutine trace_file_access (ierr, fileh, aq, levv, u)
    use TOUZA_Std,only: choice
    implicit none
    integer,      intent(out)         :: ierr
    integer,      intent(in)          :: fileh
    type(queue_t),intent(in)          :: aq
    integer,      intent(in),optional :: levv
    integer,      intent(in),optional :: u
    integer lv, utmp
    character(len=lpath) :: str
    integer jfile
    character(len=16)  :: acc
    character(len=128) :: btmp
    character(len=128) :: rtmp
    integer push, pop
    integer nrg
    integer jerr

    ierr = 0
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)
    jfile = file_h2item(fileh)
    ierr = min(0, jfile)

    push = size(aq%lefts)
    pop  = aq%nopr

    if (ierr.eq.0) call get_obj_string(ierr, str, fileh)
    if (ierr.eq.0) then
       if (associated(ofile(jfile)%rgrp)) then
          nrg = size(ofile(jfile)%rgrp)
          call get_recs_str(ierr, rtmp, ofile(jfile), nrg)
       else
101       format(I0)
          write(rtmp, 101, IOSTAT=jerr) user_index_bgn(ofile(jfile)%irec)
       endif
    else
       rtmp = ' '
    endif
    if (ierr.eq.0) then
       select case(ofile(jfile)%mode)
       case (mode_read)
          acc = 'read'
       case (mode_new)
          acc = 'new'
       case (mode_write)
          acc = 'write'
       case (mode_append)
          acc = 'append'
       case (mode_cycle)
          acc = 'cyclic'
       case (mode_persistent)
          acc = 'persistent'
       case (mode_terminate)
          acc = 'terminate'
       case default
          acc = 'unknown'
       end select
201    format('file:', A, 1x, A, '[', A, ']')
       call query_opr_name_e(jerr, btmp, ofile(jfile)%opr)
       if (jerr.eq.0) then
          write(str, 201, IOSTAT=jerr) trim(acc), trim(btmp), trim(rtmp)
       else
          write(str, 201, IOSTAT=jerr) trim(acc), trim(ofile(jfile)%name), trim(rtmp)
       endif
       if (push.gt.0) then
          if (ierr.eq.0) call get_obj_list(ierr, btmp, aq%lefts, push)
          if (ierr.eq.0) str = trim(str) // ' > ' // trim(btmp)
       else if (pop.gt.0) then
          if (ierr.eq.0) call get_obj_list(ierr, btmp, bstack(mstack-pop:mstack-1), pop)
          if (ierr.eq.0) str = trim(str) // ' < ' // trim(btmp)
       endif
       call message(ierr, str, levm=-1, u=u, indent=2)
    endif
    ! call message(ierr, str, levm=-9)
    return
  end subroutine trace_file_access

  subroutine trace_buffer_access (ierr, bufh, u)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: bufh
    integer,intent(in),optional :: u
    integer utmp
    character(len=256) :: str

    ierr = 0
    utmp = choice(-1, u)
    if (ierr.eq.0) call get_obj_string(ierr, str, bufh)
    call message(ierr, 'stack:' // str, u=utmp, indent=+2)
    return
  end subroutine trace_buffer_access

!!!_   . trace_operation
  subroutine trace_operation (ierr, aq, u)
    use TOUZA_Std,only: choice
    implicit none
    integer,      intent(out)         :: ierr
    type(queue_t),intent(in)          :: aq
    integer,      intent(in),optional :: u
    integer utmp
    integer push, pop
    character(len=256) :: str
    character(len=64)  :: opr
    integer jerr

    ierr = 0
    utmp = choice(ulog, u)

    push = size(aq%lefts)
    pop  = aq%nopr

101 format('operator:', A, ' -', I0, '+', I0, 1x, A, ' >> ', A)
    call query_opr_name(ierr, opr, aq%term)

    write(str, 101, IOSTAT=jerr) trim(opr), pop, push, trim(aq%desci), trim(aq%desco)
    call message(ierr, str, u=utmp, indent=+2)
    return
  end subroutine trace_operation

!!!_  - [b-]stack manager
!!!_   . push_stack
  subroutine push_stack &
       & (ierr, handle)
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(in)    :: handle

    ierr = 0
    if (mstack.ge.lstack.or.mstack.lt.0) then
       ierr = ERR_INSUFFICIENT_BUFFER
    else
       bstack(mstack)%bh = handle
       bstack(mstack)%lcp(:) = def_loop
       ! mark(mstack) = 0
    endif
    mstack = mstack + 1
  end subroutine push_stack

!!!_   . mpush_stack
  subroutine mpush_stack &
       & (ierr, handle, n)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: handle(*)
    integer,intent(in)          :: n
    integer jb, je
    integer j

    ierr = 0
    jb = mstack
    je = jb + n
    if (je.ge.lstack.or.jb.lt.0) then
       ierr = ERR_INSUFFICIENT_BUFFER
    else
       if (present(handle)) then
          bstack(jb:je-1)%bh = handle(1:n)
       else
          bstack(jb:je-1)%bh = -1 ! dummy
       endif
       do j = jb, je - 1
          bstack(j)%lcp(:) = def_loop
       enddo
    endif
    mstack = mstack + n
  end subroutine mpush_stack

  !!!_  - pop_stack
  subroutine pop_stack &
       & (ierr, handle, keep, anchor)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)          :: ierr
    integer,intent(out),optional :: handle
    logical,intent(in),optional  :: keep
    logical,intent(in),optional  :: anchor
    ! integer news
    integer hbuf(1)

    call mpop_stack(ierr, hbuf, 1, keep, anchor)
    if (present(handle)) then
       if (ierr.eq.0) handle = hbuf(1)
    endif
  end subroutine pop_stack
!!!_   . mpop_stack
  subroutine mpop_stack &
       & (ierr, handle, n, keep, anchor)
    use TOUZA_Std,only: choice, condop
    implicit none
    integer,intent(out)          :: ierr
    integer,intent(out),optional :: handle(0:*)
    integer,intent(in)           :: n
    logical,intent(in),optional  :: keep
    logical,intent(in),optional  :: anchor   ! whether to include anchors
    integer jh, js
    integer adj

    ierr = 0
    adj = condop(choice(.TRUE., anchor), 0, 1)

    jh = n
    js = mstack

    do
       if (jh.eq.0) exit
       if (js.le.0) then
          ierr = ERR_INSUFFICIENT_BUFFER
          call message(ierr, 'empty stack')
          return
       endif
       js = js - 1
       jh = jh - 1
       if (present(handle)) then
          handle(jh) = bstack(js)%bh
       endif
       if (is_anchor(bstack(js)%bh)) jh = jh + adj
    enddo
    if (.not.choice(.FALSE.,keep)) mstack = js
  end subroutine mpop_stack

!!!_   . push_stack
  subroutine push_stack_st &
       & (ierr, bs)
    implicit none
    integer,      intent(out) :: ierr
    type(stack_t),intent(in)  :: bs

    ierr = 0
    if (mstack.ge.lstack.or.mstack.lt.0) then
       ierr = ERR_INSUFFICIENT_BUFFER
    else
       bstack(mstack) = bs
    endif
    mstack = mstack + 1
  end subroutine push_stack_st

!!!_   . mpush_stack_st
  subroutine mpush_stack_st &
       & (ierr, lefts, n)
    implicit none
    integer,      intent(out)         :: ierr
    type(stack_t),intent(in),optional :: lefts(*)
    integer,      intent(in)          :: n
    integer jb, je
    integer j

    ierr = 0
    jb = mstack
    je = jb + n
    if (je.ge.lstack.or.jb.lt.0) then
       ierr = ERR_INSUFFICIENT_BUFFER
    else
       if (present(lefts)) then
          bstack(jb:je-1) = lefts(1:n)
       else
          bstack(jb:je-1)%bh = -1 ! dummy
          do j = jb, je - 1
             bstack(j)%lcp(:) = def_loop
          enddo
       endif
    endif
    mstack = mstack + n
  end subroutine mpush_stack_st

  !!!_  - pop_stack_st
  subroutine pop_stack_st &
       & (ierr, bs, keep, anchor)
    use TOUZA_Std,only: choice
    implicit none
    integer,      intent(out)            :: ierr
    type(stack_t),intent(inout),optional :: bs
    logical,      intent(in),   optional :: keep
    logical,      intent(in),   optional :: anchor
    type(stack_t) :: bufs(1)

    call mpop_stack_st(ierr, bufs, 1, keep, anchor)
    if (present(bs)) then
       if (ierr.eq.0) bs = bufs(1)
    endif
  end subroutine pop_stack_st

!!!_   . mpop_stack_st
  subroutine mpop_stack_st &
       & (ierr, bs, n, keep, anchor)
    use TOUZA_Std,only: choice, condop
    implicit none
    integer,      intent(out)            :: ierr
    type(stack_t),intent(inout),optional :: bs(0:*)
    integer,      intent(in)             :: n
    logical,      intent(in),   optional :: keep
    logical,      intent(in),   optional :: anchor   ! whether to include anchors
    integer jh, js
    ! integer jb, je
    integer adj

    ierr = 0
    adj = condop(choice(.TRUE., anchor), 0, 1)

    jh = n
    js = mstack

    do
       if (jh.eq.0) exit
       if (js.le.0) then
          ierr = ERR_INSUFFICIENT_BUFFER
          call message(ierr, 'empty stack')
          return
       endif
       js = js - 1
       jh = jh - 1
       if (present(bs)) then
          bs(jh) = bstack(js)
       endif
       if (is_anchor(bstack(js)%bh)) jh = jh + adj
    enddo
    if (.not.choice(.FALSE.,keep)) mstack = js
  end subroutine mpop_stack_st

!!!_  - buffer manager
!!!_   . new_buffer
  subroutine new_buffer(ierr, handle, name)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: handle
    character(len=*),intent(in),optional :: name
    integer jb

    ierr = 0

    jb = mbuffer
    mbuffer = mbuffer + 1
    if (jb.ge.lbuffer) then
       ierr = ERR_INSUFFICIENT_BUFFER
       handle = -1
    else
       handle = buf_i2handle(jb)
       obuffer(jb)%stt = stt_none
       if (present(name)) then
          obuffer(jb)%name = name
       else
          obuffer(jb)%name = ' '
       endif
       obuffer(jb)%reff = -1
       obuffer(jb)%desc = ' '
       obuffer(jb)%desc2 = ' '
       obuffer(jb)%ilev = ilev_unset

       obuffer(jb)%ci(:) = -1
       obuffer(jb)%pcp(:) = def_loop
    endif
  end subroutine new_buffer

!!!_   . fresh_buffer
  subroutine fresh_buffer(ierr, buf, name)
    implicit none
    integer,         intent(out)         :: ierr
    type(buffer_t),  intent(inout)       :: buf
    character(len=*),intent(in),optional :: name
    integer m

    ierr = 0
    if (present(name)) buf%name = name
    buf%reff = -1
    buf%desc = ' '
    buf%desc2 = ' '
    buf%ilev = ilev_unset
    buf%ci(:) = -1
    buf%pcp(:) = def_loop
    buf%k = kv_null

    m = buffer_vmems(buf)
    if (m.ge.0) deallocate(buf%vd, STAT=ierr)
    buf%vd => NULL()

  end subroutine fresh_buffer

!!!_   . reset_buffers - refresh allocated buffers
  subroutine reset_buffers(ierr, n)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: n
    integer bend

    ierr = 0

    bend = mbuffer
    mbuffer = mbuffer - n
    if (mbuffer.lt.0) then
       ierr = ERR_INSUFFICIENT_BUFFER
       return
    else
       obuffer(mbuffer:bend-1)%name = ' '
       obuffer(mbuffer:bend-1)%stt = stt_none
       obuffer(mbuffer:bend-1)%desc = ' '
       obuffer(mbuffer:bend-1)%desc2 = ' '
       obuffer(mbuffer:bend-1)%reff = -1
       obuffer(mbuffer:bend-1)%ilev = ilev_unset
    endif
  end subroutine reset_buffers

!!!_   . new_buffer_literal
  subroutine new_buffer_literal(ierr, handle, kv, v, repr, name)
    use TOUZA_Nio_header,only: hi_ASTR1, hi_ASTR2, hi_ASTR3
    use TOUZA_Nio_header,only: hi_AEND1, hi_AEND2, hi_AEND3
    use TOUZA_Nio_header,only: hi_DFMT
    use TOUZA_Nio_header,only: put_item
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: handle
    integer,         intent(in)          :: kv
    real(kind=KBUF), intent(in)          :: v
    character(len=*),intent(in)          :: repr
    character(len=*),intent(in),optional :: name

    integer jb
    integer m
    character(len=lname) :: bname
    integer jerr
    integer oentr

    ierr = 0
    call new_buffer(ierr, handle)
    if (ierr.eq.0) then
       jb = buf_h2item(handle)
       m = 1
       call alloc_buffer_t(ierr, obuffer(jb), m)
       if (ierr.eq.0) then
          if (present(name)) then
             call reg_fake_opr(oentr, handle, name)
             ierr = min(0, oentr)
             if (ierr.eq.0) obuffer(jb)%name = name
          else
101          format('L', I0)
             write(bname, 101, IOSTAT=jerr) user_index_bgn(lcount)
             lcount = lcount + 1
             obuffer(jb)%name  = bname
          endif
       endif
       if (ierr.eq.0) then
          obuffer(jb)%stt   = stt_locked
          obuffer(jb)%k     = kv
          obuffer(jb)%vd(:) = v
          obuffer(jb)%desc  = repr
          obuffer(jb)%desc2 = repr
          obuffer(jb)%ilev  = ilev_term
          obuffer(jb)%pcp(:) = def_loop
          obuffer(jb)%pcp(:)%bgn = 0
          obuffer(jb)%pcp(:)%end = 0
          obuffer(jb)%pcp(:)%flg = loop_unset
          obuffer(jb)%pcp(:)%name = ' '
          if (v.eq.UNDEF) then
             obuffer(jb)%undef = - UNDEF
          else
             obuffer(jb)%undef = + UNDEF
          endif
       endif
    endif

  end subroutine new_buffer_literal

!!!_   . alloc_buffer
  subroutine alloc_buffer(ierr, handle)
    use TOUZA_Nio,only: parse_header_size
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: handle
    integer jb
    integer n

    jb = buf_h2item(handle)
    ierr = min(0, jb)
    if (ierr.eq.0) then
       n = get_buffer_size(handle)
       ierr = min(0, n)
    endif
    if (ierr.eq.0) then
       call alloc_buffer_t(ierr, obuffer(jb), n)
    endif
  end subroutine alloc_buffer

!!!_   . alloc_buffer_t
  subroutine alloc_buffer_t &
       & (ierr, buf, n)
    implicit none
    integer,       intent(out)   :: ierr
    type(buffer_t),intent(inout) :: buf
    integer,       intent(in)    :: n
    integer m
    ierr = 0
    m = buffer_vmems(buf)
    if (m.ge.0) then
       if (n.gt.m) then
          deallocate(buf%vd, STAT=ierr)
          if (ierr.eq.0) allocate(buf%vd(0:n-1), STAT=ierr)
       endif
    else
       allocate(buf%vd(0:n-1), STAT=ierr)
    endif
    return
  end subroutine alloc_buffer_t

!!!_   . get_buffer_size
  integer function get_buffer_size(handle) result(n)
    implicit none
    integer,intent(in) :: handle
    integer jb, jc
    integer w
    integer stp
    jb = buf_h2item(handle)
    if (jb.ge.0) then
       n = 1
       do jc = 0, lcoor - 1
          w = (obuffer(jb)%pcp(jc)%end - obuffer(jb)%pcp(jc)%bgn)
          stp = min(1, max(0, obuffer(jb)%pcp(jc)%flg))
          n = n * max(1, w * stp)
       enddo
    else
       n = jb
    endif
  end function get_buffer_size

!!!_   . search_free_buffer
  subroutine search_free_buffer &
       & (ierr, lefth, n)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: lefth(*)
    integer,intent(in)  :: n

    integer jo, jb, hb, mb

    ierr = 0
    if (n.le.0) return

    lefth(1:n) = -1
    jb = 0
    mb = min(lbuffer, mbuffer)
    biter: do jo = 1, n
       do
          if (jb.ge.mb) exit
          hb = buf_i2handle(jb)
          jb = jb + 1
          if (obuffer(jb-1)%stt.eq.stt_locked &
               & .or. ANY(bstack(0:mstack-1)%bh.eq.hb)) cycle
          lefth(jo) = hb
          cycle biter
       enddo
       call new_buffer(ierr, hb)
       if (ierr.eq.0) lefth(jo) = hb
    enddo biter
  end subroutine search_free_buffer

!!!_   . is_buffer_locked()
  logical function is_buffer_locked(handle) result(b)
    implicit none
    integer,intent(in) :: handle
    integer jb
    jb = buf_h2item(handle)
    if (jb.lt.0) then
       b = .TRUE.
    else
       b = obuffer(jb)%stt.eq.stt_locked
    endif
  end function is_buffer_locked

!!!_   . register_predefined
  subroutine register_predefined(ierr)
    implicit none
    integer,intent(out) :: ierr
    integer handle

    ierr = 0
    if (ierr.eq.0) call new_buffer_literal(ierr, handle, kv_dbl, PI, 'PI', 'PI')
    if (ierr.eq.0) call new_buffer_literal(ierr, handle, kv_dbl, EXP(ONE),   'E',    'E')
    if (ierr.eq.0) call new_buffer_literal(ierr, handle, kv_dbl, HUGE(ZERO), 'HUGE', 'HUGE')
    if (ierr.eq.0) call new_buffer_literal(ierr, handle, kv_dbl, TINY(ZERO), 'TINY', 'TINY')

  end subroutine register_predefined

!!!_   . buffer_vmems
  integer function buffer_vmems(buf) result(n)
    implicit none
    type(buffer_t),intent(in) :: buf
    if (associated(buf%vd)) then
       n = size(buf%vd)
    else
       n = -1
    endif
  end function buffer_vmems

!!!_  - file manager
!!!_   . new_file
  subroutine new_file &
       & (ierr, handle, name)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: handle
    character(len=*),intent(in)  :: name
    integer jf

    ierr = 0
    jf = mfile
    mfile = mfile + 1
    if (jf.ge.lfile) then
       ierr = -1
       handle = -1
    else
       handle = file_i2handle(jf)
       call reset_file(ierr, ofile(jf), name)
    endif
  end subroutine new_file

!!!_   . read_push_file
  subroutine read_push_file &
       & (ierr, file, handle, pop, push, lefts, levv)
    use TOUZA_Std,only: sus_rseek, WHENCE_ABS
    ! use TOUZA_Std,only: KIOFS
    use TOUZA_Nio,only: parse_header_size
    implicit none
    integer,       intent(out)   :: ierr
    type(file_t),  intent(inout) :: file
    integer,       intent(in)    :: handle
    integer,       intent(in)    :: pop, push
    type(stack_t), intent(in)    :: lefts(0:*)
    integer,       intent(in)    :: levv

    integer jb
    integer js, jsb, jse, ms, ns, ji
    integer jrf
    integer jrg,  nrg
    integer n
    character(len=litem) :: head(nitem)
    integer xrec, crec
    ! integer(kind=KIOFS) :: jpos

    ierr = 0
    nrg = size(file%rgrp)
    jsb = 0
    do jrg = 0, nrg - 1
       if (file%rgrp(jrg)%term_flag.eq.mode_persistent) cycle

       ms = max_group_records(file%rgrp(jrg))
       jrf = file%rgrp(jrg)%cur
       ns = count_filter_records(file%rgrp(jrg)%filter(jrf))
       jse = jsb + ms
       ji  = file%rgrp(jrg)%sub
       ! write(*, *) 'push_file', jrg, jrf, ji, file%rgrp(jrg)%pos, ns, push
       crec = get_current_rec(file%rgrp(jrg)%filter(jrf), ji)
       if (file%kfmt.eq.cfmt_ascii) then
          continue
       else
          if (ierr.eq.0) call sus_rseek(ierr, file%u, file%rgrp(jrg)%pos, WHENCE_ABS)
       endif
       ! write(*, *) 'rseek', ierr
       do js = 0, ns - 1
          jb = buf_h2item(lefts(jsb + js)%bh)
          xrec = get_current_rec(file%rgrp(jrg)%filter(jrf), ji, js)
          ! inquire(file%u, POS=jpos)
          ! write(*, *) jrg, js, xrec, crec
          if (ierr.eq.0) call read_file_header(ierr, head, file, xrec, crec, ofile(def_read))
          ! write(*, *) 'read_header', ierr
          ! inquire(file%u, POS=jpos)
          ! write(*, *) 'after', jpos - 1
          if (ierr.eq.0) then
             if (jsb + js.eq.0) file%h(:) = head(:)
             n = parse_header_size(head, 0, lazy=1)
             obuffer(jb)%k = suggest_type(head)
             call alloc_buffer_t(ierr, obuffer(jb), n)
          endif
          if (ierr.eq.0) then
             call read_file_data(ierr, obuffer(jb)%vd, n, file)
             ! write(*, *) 'read_push_file', jg, js, ji, xrec, n, ierr
          endif
          if (ierr.eq.0) then
             call set_buffer_attrs(ierr, obuffer(jb), head, handle, file%hflag)
          endif
          if (ierr.eq.0) then
             crec = xrec + 1
          endif
       enddo
       if (ns.lt.ms) then
          if (is_msglev_NORMAL(lev_verbose)) then
             call message(ierr, 'caution: null stack(s)', (/ms - ns/))
          endif
          do js = ns, ms - 1
             jb = buf_h2item(lefts(jsb + js)%bh)
             call fresh_buffer(ierr, obuffer(jb))
          enddo
       endif
       jsb = jse
       if (ierr.eq.0) file%irec = crec
    enddo
  end subroutine read_push_file

!!!_   . set_buffer_attrs
  subroutine set_buffer_attrs &
       & (ierr, buf, head, handle, hflag)
    use TOUZA_Std,only: upcase
    use TOUZA_Nio,only: get_item, get_item_date, restore_item
    use TOUZA_Nio,only: hi_MISS, hi_ITEM, hi_DATE, hi_TIME
    implicit none
    integer,         intent(out)   :: ierr
    type(buffer_t),  intent(inout) :: buf
    character(len=*),intent(in)    :: head(*)
    integer,         intent(in)    :: handle
    integer,         intent(in)    :: hflag

    integer jerr
    character(len=128) :: txt, tsfx
    character(len=ldesc) :: tdesc

    ierr = 0
    if (ierr.eq.0) call get_item(ierr, head, buf%undef, hi_MISS, def=UNDEF)
    if (ierr.eq.0) then
       call get_item(ierr, head, buf%desc, hi_ITEM)
       if (buf%desc.eq.' ') then
          buf%desc = buf%name
          buf%desc2 = buf%name
       else
          tdesc = adjustl(buf%desc)
          call upcase(tdesc)
          if (verify(trim(tdesc), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789').eq.0) then
             buf%desc2 = adjustl(trim(buf%desc))
          else
             buf%desc2 = '<' // adjustl(trim(buf%desc)) // '>'
          endif
       endif
       buf%ilev = ilev_term

       buf%reff = handle
    endif
    if (ierr.eq.0) call get_header_lprops(ierr, buf%pcp, head, hflag)

    if (ierr.eq.0) then
       call get_log_suffix(ierr, tsfx, head)
    else
       tsfx = ' '
    endif
101 format('  read:', A, 1x, A, A)
    write(txt, 101, IOSTAT=jerr) trim(buf%name), trim(buf%desc), trim(tsfx)
    call message(ierr, txt, levm=msglev_normal, u=uerr)
    return
  end subroutine set_buffer_attrs

!!!_   . write_file
  subroutine write_file(ierr, file, bufh, jstk, levv)
    use TOUZA_Std,only: new_unit, sus_open, is_error_match
    use TOUZA_Std,only: get_login_name
    use TOUZA_Nio,only: nio_write_header, parse_header_size, nio_write_data
    use TOUZA_Nio,only: get_default_header, show_header, parse_record_fmt
    use TOUZA_Nio,only: REC_DEFAULT, REC_BIG
    use TOUZA_Nio,only: put_item, get_item, restore_item, store_item
    use TOUZA_Nio,only: get_item_date, put_item_date
    use TOUZA_Nio,only: hi_MISS, hi_ITEM, hi_DFMT,  hi_EDIT1, hi_TITL1, hi_ETTL1
    use TOUZA_Nio,only: hi_DATE, hi_TIME, hi_CSIGN, hi_CDATE, hi_MSIGN, hi_MDATE
    use TOUZA_Nio,only: fill_header
    implicit none
    integer,     intent(out)         :: ierr
    type(file_t),intent(inout)       :: file
    integer,     intent(in)          :: bufh
    integer,     intent(in)          :: jstk
    integer,     intent(in),optional :: levv
    integer n
    integer jc
    integer jb, jrefh
    character(len=128) :: txt, tsfx
    character(len=litem) :: tstr
    integer jerr
    logical :: is_tweak
    type(buffer_t) :: btmp
    integer kfmt
    integer idt(8)
    real(kind=KBUF) :: undef
    character(len=litem) :: head(nitem)
    integer jsi, jdi

    ierr = 0
    jb = 0
    ! if (ierr.eq.0) then
    !    if (file%bh.lt.0) ierr = ERR_PANIC
    ! endif
    if (ierr.eq.0) jb = buf_h2item(bufh)

    head(:) = ' '

    if (ierr.eq.0) then
       jrefh = file_h2item(obuffer(jb)%reff)
       call get_item(jerr, file%h, undef, hi_MISS)
       ! write(*, *) 'miss', jerr, undef
       if (jerr.ne.0) undef = obuffer(jb)%undef
       ! write(*, *) 'miss', jb, undef
       if (jrefh.ge.0) then
          head(:) = ofile(jrefh)%h(:)
       else if (ABS(undef).ne.ULIMIT) then
          call get_default_header(head, vmiss=undef)
       else
          call get_default_header(head)
          call store_item(ierr, head, ' ', hi_MISS)
       endif
    endif

    if (ierr.eq.0) then
       if (file%hedit.ge.hedit_sign) then
          call get_item(jerr, head, tstr, hi_CSIGN)
          if (jerr.ne.0) tstr = 'x'
          if (tstr.eq.' ') then
             call get_item(jerr, head, tstr, hi_CDATE)
             if (jerr.ne.0) tstr = 'x'
          endif
          if (tstr.eq.' ') then
             jsi = hi_CSIGN
             jdi = hi_CDATE
          else
             jsi = hi_MSIGN
             jdi = hi_MDATE
          endif
          call get_login_name(jerr, tstr)
          if (jerr.eq.0) call put_item(ierr, head, tstr, jsi)
          call date_and_time(values=idt(:))
          idt(4:6) = idt(5:7)
          call put_item_date(jerr, head, idt(1:6), jdi)
       endif
    endif

    if (ierr.eq.0) then
       if (file%hedit.ge.hedit_item) call put_item(ierr, head, obuffer(jb)%desc,  hi_ITEM, tol=1)
    endif
    if (ierr.eq.0) then
       if (file%hedit.ge.hedit_title) call put_item(ierr, head, obuffer(jb)%desc2, hi_TITL1, 0, tol=1)
    endif
    if (file%hedit.ge.hedit_edit) then
       if (ierr.eq.0) call put_item(ierr, head, obuffer(jb)%desc,  hi_EDIT1, 0, tol=1)
       if (ierr.eq.0) call put_item(ierr, head, obuffer(jb)%desc2, hi_ETTL1, 0, tol=1)
    endif
    if (ierr.eq.0) then
       call restore_item(jerr, head, tstr, hi_TIME)
       if (jerr.ne.0) tstr = ' '
       if (tstr.eq.' ') call put_item(ierr, head, 0,  hi_TIME)
    endif
    is_tweak = .FALSE.
    if (ierr.eq.0) then
       if (.not.is_tweak) then
          is_tweak = ANY(bstack(jstk)%lcp(:)%flg.ge.loop_null)
       endif
       if (.not.is_tweak) then
          is_tweak = ANY(bstack(jstk)%lcp(:)%name.ne.' ')
       endif
       if (is_tweak) then
          ! write(*, *) 'tweak'
          call tweak_buffer(ierr, btmp, bufh, jstk)
          if (ierr.eq.0) call put_header_lprops(ierr, head, btmp%pcp, file%hflag)
       else if (ANY(obuffer(jb)%pcp(:)%flg.eq.loop_null)) then
          btmp%pcp(:) = obuffer(jb)%pcp(:)
          if (file%hflag.eq.hflag_nulld) then
             do jc = 0, lcoor - 1
                if (btmp%pcp(jc)%flg.eq.loop_null) then
                   btmp%pcp(jc)%end = btmp%pcp(jc)%bgn
                endif
             enddo
          else
             do jc = 0, lcoor - 1
                if (btmp%pcp(jc)%flg.eq.loop_null) then
                   btmp%pcp(jc)%bgn = 0
                   btmp%pcp(jc)%end = 0
                endif
             enddo
          endif
          call put_header_lprops(ierr, head, btmp%pcp, file%hflag)
       else
          call put_header_lprops(ierr, head, obuffer(jb)%pcp, file%hflag)
       endif
    endif
    if (ierr.eq.0) then
       if (file%fmt.ne.' ') then
          call put_item(ierr, head, file%fmt,  hi_DFMT)
          call parse_record_fmt(jerr, kfmt, file%fmt)
       else
          call get_item(jerr, head, tstr,  hi_DFMT)
          if (jerr.ne.0) tstr = ' '
          if (tstr.eq.' ') then
             tstr = 'UR4'
             call put_item(ierr, head, tstr,  hi_DFMT)
          endif
          call parse_record_fmt(jerr, kfmt, tstr)
       endif
    endif
    if (ierr.eq.0) call fill_header(ierr, head, file%h, 1)

    call message(ierr, 'put_item', levm=-9)

    if (ierr.eq.0) call open_write_file(ierr, file)
    if (ierr.ne.0) then
       call message(ierr, 'failed to write open: ' // trim(file%name))
       return
    endif

    if (ierr.eq.0) then
       n = parse_header_size(head, 0, lazy=1)
    endif

    if (file%kfmt.le.cfmt_org) then
       if (ierr.eq.0) then
          ! file%t = REC_DEFAULT
          file%t = REC_BIG
          call nio_write_header(ierr, head, file%t, file%u)
          call message(ierr, 'write_header', levm=-9)
       endif
    endif
    if (ierr.eq.0) then
       if (is_tweak) then
          call write_file_data(ierr, btmp%vd,        n, head, file, obuffer(jb)%k)
       else
          call write_file_data(ierr, obuffer(jb)%vd, n, head, file, obuffer(jb)%k)
       endif
       if (ierr.eq.0) file%irec = file%irec + 1
       call message(ierr, 'write_data', levm=-9)
    endif

    if (ierr.eq.0) then
       call get_log_suffix(ierr, tsfx, head)
    else
       tsfx = ' '
    endif
101 format('    write:', A, 1x, A, A)
    write(txt, 101, IOSTAT=jerr) trim(obuffer(jb)%name), trim(obuffer(jb)%desc), trim(tsfx)
    call message(ierr, txt, levm=msglev_normal, u=uerr)

    if (is_tweak) then
       if (ierr.eq.0) deallocate(btmp%vd, STAT=ierr)
    endif

    if (ierr.ne.0) call show_header(ierr, head)
    return
  end subroutine write_file

!!!_   . get_log_suffix
  subroutine get_log_suffix &
       & (ierr, txt, head)
    use TOUZA_Nio,only: hi_DATE, hi_TIME, hi_DFMT
    use TOUZA_Nio,only: get_item_date, restore_item
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: txt
    character(len=*),intent(in)  :: head(*)

    character(len=128) :: txtd, txtt
    character(len=litem) :: tstr
    integer :: dt(6)
    integer jerr

    ierr = 0
    call get_item_date(jerr, head, dt(:), hi_DATE)
    if (jerr.ne.0) then
       txtd = ' '
    else
102    format(' DATE = ', I0, '/', I0, '/', I0, 1x, I2.2, ':', I2.2, ':', I2.2)
    write(txtd, 102, IOSTAT=jerr) dt(:)
    endif
    call restore_item(jerr, head, tstr, hi_TIME)
    if (jerr.ne.0) then
       txtt = ' '
    else if (tstr.eq.' ') then
       txtt = ' '
    else
103    format(' T = ', A)
       write(txtt, 103, IOSTAT=jerr) trim(adjustl(tstr))
    endif
    txt = trim(txtd) // trim(txtt)

    call restore_item(jerr, head, tstr, hi_DFMT)
    if (jerr.ne.0) tstr = ' '
    if (tstr.ne.' ') then
       txt = trim(txt) // ' [' // trim(tstr) // ']'
    endif
    return
  end subroutine get_log_suffix

!!!_   . tweak_buffer
  subroutine tweak_buffer (ierr, bdest, hsrc, jstk)
    implicit none
    integer,       intent(out)   :: ierr
    type(buffer_t),intent(inout) :: bdest
    integer,       intent(in)    :: hsrc
    integer,       intent(in)    :: jstk

    integer,parameter :: nbuf = 1
    integer        :: htmp(nbuf)
    type(domain_t) :: domL
    type(domain_t) :: domR(nbuf)
    type(stack_t)  :: stmp(nbuf)
    integer jset
    integer jbR
    real(kind=KBUF) :: fillR

    ierr = 0
    htmp = hsrc
    stmp = bstack(jstk)

    if (ierr.eq.0) call tweak_coordinates(ierr, domL, domR, htmp, stmp, nbuf, bdest)
    if (ierr.eq.0) call set_inclusive_domain(ierr, domL, domR, htmp, stmp, nbuf)
    if (ierr.eq.0) call settle_output_domain(ierr, domL)
    if (ierr.eq.0) call settle_input_domain(ierr, domR(1), htmp(1), stmp(1), domL)
    if (ierr.eq.0) call set_output_buffer(ierr, bdest, htmp, domL)

    ! if (ierr.eq.0) call show_domain(ierr, domL)

    if (ierr.eq.0) then
       jbR = buf_h2item(hsrc)
       jset = 1
       fillR = obuffer(jbR)%undef
       call apply_COPY &
            & (ierr, &
            &  bdest%vd,        domL, &
            &  obuffer(jbR)%vd, domR(jset), fillR)
    endif

  end subroutine tweak_buffer

!!!_   . suggest_type
  integer function suggest_type(head) result(k)
    use TOUZA_Nio_record
    use TOUZA_Nio_header,only: hi_DFMT, get_item
    implicit none
    character(len=*),intent(in) :: head(*)
    integer jerr
    character(len=litem) :: vp
    integer kfmt
    jerr = 0
    if (jerr.eq.0) call get_item(jerr, head, vp, hi_DFMT)
    if (jerr.eq.0) call parse_record_fmt(jerr, kfmt, vp)
    if (jerr.eq.0) then
       select case (kfmt)
       case (GFMT_UR4, GFMT_MR4)
          k = kv_flt
       case (GFMT_UR8, GFMT_MR8)
          k = kv_dbl
       case (GFMT_UI4, GFMT_MI4)
          k = kv_int
       case (GFMT_URC,GFMT_URC2)
          k = kv_dbl
       case (GFMT_URY:GFMT_URYend, GFMT_MRY:GFMT_MRYend)
          k = kv_dbl
       case (GFMT_URT, GFMT_MRT)
          k = kv_dbl
       case default
          k = kv_dbl
       end select
    endif
    if (jerr.ne.0) k = jerr
  end function suggest_type

!!!_   . is_read_buffer
  logical function is_read_buffer(handle) result(b)
    integer,intent(in) :: handle
    integer jf
    jf = file_h2item(handle)
    if (jf.lt.0) then
       b = .FALSE.
    else
       b = is_read_mode(ofile(jf)%mode)
    end if
  end function is_read_buffer

!!!_   . get_obj_list
  subroutine get_obj_list &
       & (ierr, str, bst, n)
    use TOUZA_Std,only: join_list
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    type(stack_t),   intent(in)  :: bst(:)
    integer,         intent(in)  :: n
    character(len=lname) :: buf(n)
    integer j
    ierr = 0
    do j = 1, n
       if (ierr.eq.0) call get_obj_string(ierr, buf(j), bst(j)%bh)
    enddo
    if (ierr.eq.0) call join_list(ierr, str, buf(1:n), sep=CHAR(0))
  end subroutine get_obj_list

!!!_   . get_obj_string
  subroutine get_obj_string &
       & (ierr, str, handle, levv)
    use TOUZA_Std,only: choice
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    integer,         intent(in)  :: handle
    integer,optional,intent(in)  :: levv
    integer j
    integer lv
    character(len=litem+16) :: buf

    ierr = 0
    lv = choice(lev_verbose, levv)
111 format('{', I0, '}')
112 format('{', A, '}')
113 format('{B', I0, '}')

102 format('file[', I0, ']')
103 format('unknown[', I0, ']')
104 format('opr[', A, ']')
105 format('anchor[', I0, ']')
106 format('anchor[-]')
    j = file_h2item(handle)
    if (j.ge.0) then
       write(str, 102, IOSTAT=ierr) handle
       return
    endif
    j = buf_h2item(handle)
    if (j.ge.0) then
       if (obuffer(j)%name.ne.' ') then
          write(str, 112, IOSTAT=ierr) trim(obuffer(j)%name)
       else if (dbgv.gt.0) then
          write(str, 111, IOSTAT=ierr) handle
       else
          write(str, 113, IOSTAT=ierr) j
       endif
       return
    endif
    j = anchor_h2level(handle)
    if (j.gt.0) then
       write(str, 105, IOSTAT=ierr) j
    else if (j.eq.0) then
       write(str, 106, IOSTAT=ierr)
    else
       call query_opr_name(ierr, buf, handle)
       if (ierr.eq.0) write(str, 104, IOSTAT=ierr) trim(buf)
       if (ierr.ne.0) write(str, 103, IOSTAT=ierr) handle
    endif
  end subroutine get_obj_string

!!!_  - operation queue dispatcher
!!!_   . init_all_files
  subroutine init_all_files &
       & (ierr, levv)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: levv
    integer jfile
    integer hf

    ierr = 0
    do jfile = bgn_file, min(mfile, lfile) - 1
       if (is_read_mode(ofile(jfile)%mode)) then
          hf = file_i2handle(jfile)
          if (ierr.eq.0) call open_read_file(ierr, ofile(jfile), ofile(def_read))
          if (ierr.eq.0) call cue_read_file(ierr,  ofile(jfile))
       endif
    enddo
  end subroutine init_all_files

!!!_   . batch_operation
  subroutine batch_operation(ierr, irecw, levv)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: irecw
    integer,intent(in)  :: levv
    integer jq
    integer hterm
    integer termk
    integer push, pop
    integer jerr
    integer jfile
    integer neof, nterm
    integer hf
    integer stt, sttj
    integer cmode
    integer js

    ierr = 0

    neof = 0
    nterm = 0

    if (irecw.gt.0.or.rcount.gt.0) call banner_record(ierr, irecw)
    if (ierr.ne.0) return

    mstack = 0
    do jq = 0, min(mqueue, lqueue) - 1
       hterm = aqueue(jq)%term
       termk = handle_type(hterm)
       if (aqueue(jq)%desci.eq.' ') call set_queue_descr(ierr, aqueue(jq))
       if (is_msglev_DETAIL(levv)) then
          call trace_queue(jerr, aqueue(jq), levv)
       endif
       push = size(aqueue(jq)%lefts)
       do js = 0, push - 1
          aqueue(jq)%lefts(js)%lcp = aqueue(jq)%lcp
       enddo
       select case(termk)
       case(hk_file)
          pop = aqueue(jq)%nopr
          if (ierr.eq.0) call push_file(ierr, hterm, pop, push, aqueue(jq)%lefts, levv)
       case(hk_buffer)
          if (ierr.eq.0) call push_buffer(ierr, hterm, levv)
       case(hk_opr)
          pop = aqueue(jq)%nopr
          if (ierr.eq.0) then
             cmode = aqueue(jq)%cmode
             if (cmode.eq.cmode_null) cmode = def_cmode
             call apply_operator &
                  & (ierr, hterm, aqueue(jq)%lefts, pop, push, cmode, irecw, levv)
          endif
       end select
       if (is_msglev_DETAIL(levv)) then
          ! call trace_queue(jerr, aqueue(jq), levv)
          call show_stack(jerr)
       endif
       if (ierr.ne.0) exit
    enddo
    if (ierr.eq.0) then
       if (mstack.gt.0) then
          call message(ierr, "operand(s) left on the stack ", (/mstack/), levm=msglev_WARNING, u=uerr)
          if (is_msglev_NORMAL(levv)) call show_stack(jerr, u=uerr)
       endif
    endif
    stt = stt_wait
    do jfile = bgn_file, min(mfile, lfile) - 1
       if (is_read_mode(ofile(jfile)%mode)) then
          hf = file_i2handle(jfile)
          if (ierr.eq.0) call cue_next_read(ierr, sttj, ofile(jfile))
          ! write(*, *) 'sttj = ', sttj, jfile, ierr
          if (ierr.eq.0) then
             if ((stt.eq.stt_cont.and.sttj.eq.stt_term) &
                  & .or. (stt.eq.stt_term.and.sttj.eq.stt_cont)) then
                stt = stt_lack
             else
                stt = min(stt, sttj)
             endif
          endif
       endif
    enddo
    ! write(*, *) 'stt = ', stt
    if (stt.eq.stt_lack) then
       ierr = ERR_EOF
       call message(ierr, 'insufficient records')
       call show_files(jerr)
    endif
    if (ierr.eq.0) then
       ! for debug
       ! ierr = ERR_FINISHED
       if (rcount.eq.0) ierr = ERR_FINISHED
       if (stt.eq.stt_wait.or.stt.eq.stt_term) ierr = ERR_FINISHED
    endif
  end subroutine batch_operation

!!!_   . banner_record
  subroutine banner_record(ierr, irecw)
    integer,intent(out) :: ierr
    integer,intent(in)  :: irecw
    ierr = 0
    if (irecw.ge.0) then
       call message &
            & (ierr, '### record:', (/user_index_bgn(irecw)/), &
            &  fmt='(I0)', levm=-levq_rec)
    endif
  end subroutine banner_record

!!!_   . push_file
  subroutine push_file &
       & (ierr, handle, pop, push, lefts, levv)
    implicit none
    integer,       intent(out) :: ierr
    integer,       intent(in)  :: handle
    integer,       intent(in)  :: pop, push
    type(stack_t), intent(in)  :: lefts(*)
    integer,       intent(in)  :: levv
    integer jfile
    integer bufh

    jfile = file_h2item(handle)
    ierr = min(0, jfile)
    if (ierr.eq.0) then
       if (is_read_mode(ofile(jfile)%mode)) then
          if (ierr.eq.0) call read_push_file(ierr, ofile(jfile), handle, pop, push, lefts, levv)
          if (ierr.eq.0) call mpush_stack_st(ierr, lefts, push)
       else
          if (ierr.eq.0) call pop_stack(ierr, bufh, keep=.TRUE.)
          if (ierr.eq.0) call write_file(ierr, ofile(jfile), bufh, mstack-1, levv)
          if (ierr.eq.0) call pop_stack(ierr)
       endif
    endif
  end subroutine push_file

!!!_   . push_buffer
  subroutine push_buffer &
       & (ierr, handle, levv)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: handle
    integer,intent(in)  :: levv
    ierr = 0
    if (ierr.eq.0) call push_stack(ierr, handle)
  end subroutine push_buffer

!!!_  - operations
!!!_   . apply_operator
  subroutine apply_operator &
       & (ierr, handle, lefts, pop, push, cmode, irec, levv)
    use TOUZA_Std,only: condrep
    implicit none
    integer,      intent(out)   :: ierr
    integer,      intent(in)    :: handle
    type(stack_t),intent(inout) :: lefts(*)
    integer,      intent(in)    :: push, pop
    integer,      intent(in)    :: cmode
    integer,      intent(in)    :: irec
    integer,      intent(in)    :: levv

    integer,parameter :: lmaxo = 8
    integer righth(pop)
    integer j, jb
    integer cm
    character(len=8) :: opr

    ierr = 0
    if (ierr.eq.0) call mpop_stack(ierr, righth, pop, keep=.TRUE.)
    if (ierr.eq.0) then
!!!_    * output
       if (handle.eq.opr_OUTPUT) then
          call flush_stack(ierr, pop, cmode_each)
       else if (handle.eq.opr_FLUSH) then
          cm = condrep(cmode, cmode_null, cmode_each)
          call flush_stack(ierr, pop, cm)
       else if (handle.eq.opr_DFLUSH) then
          cm = condrep(cmode, cmode_null, cmode_each)
          call flush_stack(ierr, pop, IOR(cm, cmode_xundef))
       else if (handle.eq.opr_CFLUSH) then
          cm = condrep(cmode, cmode_null, cmode_each)
          call flush_stack(ierr, pop, IOR(cm, cmode_column))
       else if (handle.eq.opr_PROP) then
          call list_stack(ierr, pop, irec)
!!!_    * transformation
       else if (handle.eq.opr_TRANSF) then
          continue
       else if (ANY(handle.eq.(/opr_PERM, opr_SHAPE, opr_SIZE, opr_SHIFT/))) then
          continue
!!!_    * index
       else if (handle.ge.opr_C0.and.handle.lt.opr_C3) then
          call apply_INDEX(ierr, handle, lefts(1:push))
       else if (handle.eq.opr_FLAT) then
          call apply_FLAT(ierr, handle, lefts(1:push))
!!!_    * miss
       else if (handle.eq.opr_MISS) then
          call apply_MISS(ierr, handle, lefts(1:push))
!!!_    * copy
       else if (handle.eq.opr_COPY) then
          call apply_opr_UNARY(ierr, handle, lefts(push:push), righth(pop:pop), apply_COPY)
!!!_    * operators
       else if (handle.eq.opr_NEG) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_NEG)
       else if (handle.eq.opr_INV) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_INV)
       else if (handle.eq.opr_ABS) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_ABS)
       else if (handle.eq.opr_SQR) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_SQR)
       else if (handle.eq.opr_SIGN1) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_SIGN1)
       else if (handle.eq.opr_ZSIGN) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_ZSIGN)
       else if (handle.eq.opr_FLOOR) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_FLOOR)
       else if (handle.eq.opr_CEIL) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_CEIL)
       else if (handle.eq.opr_ROUND) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_ROUND)
       else if (handle.eq.opr_TRUNC) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_TRUNC)
       else if (handle.eq.opr_INT) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_TRUNC)
       else if (handle.eq.opr_BITNOT) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_BITNOT)
       else if (handle.eq.opr_NOT) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_NOT, .TRUE.)
       else if (handle.eq.opr_BOOL) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_BOOL, .TRUE.)
       else if (handle.eq.opr_BIN) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_BIN, .TRUE.)
       else if (handle.eq.opr_SIGN) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_SIGN)
       else if (handle.eq.opr_EQB) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_EQB, .TRUE.)
       else if (handle.eq.opr_NEB) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_NEB, .TRUE.)
       else if (handle.eq.opr_LTB) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_LTB, .TRUE.)
       else if (handle.eq.opr_GTB) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_GTB, .TRUE.)
       else if (handle.eq.opr_LEB) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_LEB, .TRUE.)
       else if (handle.eq.opr_GEB) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_GEB, .TRUE.)
       else if (handle.eq.opr_EQ) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_EQ, .TRUE.)
       else if (handle.eq.opr_NE) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_NE, .TRUE.)
       else if (handle.eq.opr_LT) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_LT, .TRUE.)
       else if (handle.eq.opr_GT) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_GT, .TRUE.)
       else if (handle.eq.opr_LE) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_LE, .TRUE.)
       else if (handle.eq.opr_GE) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_GE, .TRUE.)
       else if (handle.eq.opr_AND) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_AND)
       else if (handle.eq.opr_MASK) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_MASK)
       else if (handle.eq.opr_ADD) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_ADD)
       else if (handle.eq.opr_SUB) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_SUB)
       else if (handle.eq.opr_MUL) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_MUL)
       else if (handle.eq.opr_DIV) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_DIV)
       else if (handle.eq.opr_IDIV) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_IDIV)
       else if (handle.eq.opr_MOD) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_MOD)
       else if (handle.eq.opr_POW) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_POW)
       else if (handle.eq.opr_MODULO) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_MODULO)
       else if (handle.eq.opr_BITAND) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_BITAND)
       else if (handle.eq.opr_BITOR) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_BITOR)
       else if (handle.eq.opr_BITXOR) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_BITXOR)
       else if (handle.eq.opr_LSHIFT) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_LSHIFT)
       else if (handle.eq.opr_RSHIFT) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_RSHIFT)
       else if (handle.eq.opr_MIN) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_MIN)
       else if (handle.eq.opr_MAX) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_MAX)
       else if (handle.eq.opr_EQF) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_EQF)
       else if (handle.eq.opr_NEF) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_NEF)
       else if (handle.eq.opr_LTF) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_LTF)
       else if (handle.eq.opr_GTF) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_GTF)
       else if (handle.eq.opr_LEF) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_LEF)
       else if (handle.eq.opr_GEF) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_GEF)
       else if (handle.eq.opr_OR) then
          call apply_opr_BINARY_lazy(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_lazy_OR)
       else if (handle.eq.opr_ROR) then
          call apply_opr_BINARY_lazy(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_lazy_ROR)
       else if (handle.eq.opr_XOR) then
          call apply_opr_BINARY_lazy(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_lazy_XOR)
       else if (handle.eq.opr_LAND) then
          call apply_opr_BINARY_lazy(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_lazy_AND)
       else if (handle.eq.opr_LMASK) then
          call apply_opr_BINARY_lazy(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_lazy_MASK)
       else if (handle.eq.opr_LLAY) then
          call apply_opr_BINARY_lazy(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_lazy_LAY, rev=.TRUE.)
       else if (handle.eq.opr_RLAY) then
          call apply_opr_BINARY_lazy(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_lazy_LAY, rev=.FALSE.)
       else if (handle.eq.opr_LADD) then
          call apply_opr_BINARY_lazy(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_lazy_ADD, ZERO)
       else if (handle.eq.opr_LSUB) then
          call apply_opr_BINARY_lazy(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_lazy_SUB, ZERO)
       else if (handle.eq.opr_LMUL) then
          call apply_opr_BINARY_lazy(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_lazy_MUL, ONE)
       else if (handle.eq.opr_LDIV) then
          call apply_opr_BINARY_lazy(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_lazy_DIV, ONE)
       else if (handle.eq.opr_LMIN) then
          call apply_opr_BINARY_lazy(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_lazy_LMIN, ULIMIT)
       else if (handle.eq.opr_LMAX) then
          call apply_opr_BINARY_lazy(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_lazy_LMAX, LLIMIT)
       else if (handle.eq.opr_SQRT) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_SQRT)
       else if (handle.eq.opr_EXP) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_EXP)
       else if (handle.eq.opr_LOG) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_LOG)
       else if (handle.eq.opr_LOG10) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_LOG10)
       else if (handle.eq.opr_SIN) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_SIN)
       else if (handle.eq.opr_COS) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_COS)
       else if (handle.eq.opr_TAN) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_TAN)
       else if (handle.eq.opr_ASIN) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_ASIN)
       else if (handle.eq.opr_ACOS) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_ACOS)
       else if (handle.eq.opr_ATAN) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_ATAN)
       else if (handle.eq.opr_ATAN2) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_ATAN2)
       else if (handle.eq.opr_SINH) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_SINH)
       else if (handle.eq.opr_COSH) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_COSH)
       else if (handle.eq.opr_TANH) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_TANH)
       else if (handle.eq.opr_R2D) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_R2D)
       else if (handle.eq.opr_D2R) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_D2R)
       else if (handle.eq.opr_HYPOT) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_HYPOT)
       else if (handle.eq.opr_EXPONENT) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_EXPONENT)
       else if (handle.eq.opr_FRACTION) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_FRACTION)
       else if (handle.eq.opr_SCALE) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_SCALE)
       else if (handle.eq.opr_NEAREST) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_NEAREST)
       else if (handle.eq.opr_SPACING) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_SPACING)
       else if (handle.eq.opr_RRSP) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_RRSP)
!!!_    * reduction
       else if (handle.eq.opr_COUNT) then
          call apply_opr_REDUCE(ierr, handle, lefts(1:push), righth(1:pop), apply_REDUCE_COUNT, ZERO)
!!!_    * ignored
       else if (grp_system_bgn.le.handle.and.handle.lt.grp_system_end) then
          continue
       else if (grp_stack_bgn.le.handle.and.handle.lt.grp_stack_end) then
          continue
!!!_    * reserved
       else
          call query_opr_name(ierr, opr, handle)
          ierr = ERR_NOT_IMPLEMENTED
          call message(ierr, 'reserved operator(apply) ' // trim(opr))
       endif
    endif
    if (ierr.eq.0) then
       do j = 1, push
          jb = buf_h2item(lefts(j)%bh)
          obuffer(jb)%k = adj_operator_type(handle, obuffer(jb)%k)
       enddo
    endif
    if (ierr.eq.0) call mpop_stack(ierr, n=pop)
    if (ierr.eq.0) then
       if (push.gt.0) call mpush_stack_st(ierr, lefts, push)
    endif
  end subroutine apply_operator

!!!_   . flush_stack
  subroutine flush_stack(ierr, pop, cmode, u)
    use TOUZA_Std,only: choice
    use TOUZA_Nio,only: show_header, parse_header_size
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: pop
    integer,intent(in)          :: cmode
    integer,intent(in),optional :: u

    integer jinp
    integer jb, hb
    integer js
    integer m

    integer nbuf
    integer bufh(0:pop-1)
    integer bufj(0:pop-1)
    type(stack_t) :: lstk(0:pop-1)

    ierr = 0

    nbuf = 0
    do jinp = 0, pop - 1
       js = mstack - pop + jinp
       hb = bstack(js)%bh
       jb = buf_h2item(hb)
       if (jb.ge.0) then
          m = buffer_vmems(obuffer(jb))
          if (m.ge.0) then
             bufj(nbuf) = jb
             bufh(nbuf) = hb
             lstk(nbuf) = bstack(js)
             nbuf = nbuf + 1
          endif
       endif
    enddo

    select case(IAND(cmode, cmode_compromise))
    case (cmode_each)
       if (IAND(cmode, cmode_column).eq.0) then
          call flush_buffer_each(ierr, nbuf, bufh, bufj, lstk, cmode, u)
       else
          call flush_buffer_column(ierr, nbuf, bufh, bufj, lstk, cmode, u)
       endif
    case default
       if (IAND(cmode, cmode_column).eq.0) then
          call flush_buffer_horizontally(ierr, nbuf, bufh, bufj, lstk, cmode, u)
       else
          call flush_buffer_horizontally_column(ierr, nbuf, bufh, bufj, lstk, cmode, u)
       endif
    end select
  end subroutine flush_stack

!!!_   . flush_buffer_each - flush out buffers for each
  subroutine flush_buffer_each(ierr, nbuf, bufh, bufj, lstk, cmode, u)
    use TOUZA_Std,only: choice
    use TOUZA_Nio,only: show_header, parse_header_size
    implicit none
    integer,      intent(out)         :: ierr
    integer,      intent(in)          :: nbuf
    integer,      intent(in)          :: bufh(0:*)
    integer,      intent(in)          :: bufj(0:*)
    type(stack_t),intent(in)          :: lstk(0:*)  ! only for logical coordinates
    integer,      intent(in)          :: cmode
    integer,      intent(in),optional :: u

    integer utmp
    integer jb, hb

    integer jbuf
    character(len=64) :: lcstr, pcstr, dstr
    character(len=64) :: fmt_nline
    character(len=64) :: fmt_uline
    character(len=64) :: fmt_xline

    type(domain_t) :: doml, domr(1)
    type(buffer_t) :: htmp
    integer mco, nco
    integer jl, jp
    integer lidx(0:lcoor-1), pidx(0:lcoor-1)
    integer nb
    integer btmp(1)
    character(len=256) :: cjoin, val

    logical skip_undef

    ierr = 0
    utmp = choice(ulog, u)

    skip_undef = IAND(cmode, cmode_xundef).ne.0

212 format('## stack[', I0, '] ', A, 1x, A)
213 format('## stack[', I0, '] ', A, 1x, A, ' // ', A)

231 format('##      ', A)
232 format('##   >  ', A)
233 format('##   >> ', A)

201 format('(', I0, '(I0, 1x), ', A, ')')
202 format('(', I0, '(I0, 1x), ''.'')')
203 format('(', I0, '(I0, 1x), ''_'')')
221 format('(', A, ')')
222 format('(', '''.'')')
223 format('(', '''_'')')
211 format('#', A, 1x, A)

    ! write(*, *) def_write%kfmt, trim(def_write%fmt)

    do jbuf = 0, nbuf - 1
       jb = bufj(jbuf)
       hb = bufh(jbuf)
       nb = 1
       btmp(1) = hb

       if (ierr.eq.0) then
          call get_compromise_domain(ierr, domL, domR, btmp, lstk(jbuf:jbuf), nb, cmode_inclusive, htmp)
       endif
       ! if (ierr.eq.0) call show_domain(ierr, domL,    'each/L', indent=4)
       ! if (ierr.eq.0) call show_domain(ierr, domR(1), 'each/R', indent=5)

       if (ierr.eq.0) call get_obj_string(ierr, val, hb)

       if (is_msglev(lev_verbose, -levq_rec)) then
          if (ierr.eq.0) write(utmp, 213) &
               & user_index_bgn(jbuf), trim(val), trim(obuffer(jb)%desc), trim(obuffer(jb)%desc2)
       else if (is_msglev(lev_verbose, -levq_stack)) then
          if (ierr.eq.0) write(utmp, 212) &
               & user_index_bgn(jbuf), trim(val), trim(obuffer(jb)%desc)
       endif
       if (is_msglev(lev_verbose, -levq_stack)) then
          if (ierr.eq.0) call get_domain_string(ierr, lcstr, lstk(jbuf)%lcp)
          if (ierr.eq.0) call get_domain_string(ierr, pcstr, obuffer(jb)%pcp)
          if (ierr.eq.0) then
             call get_domain_shape(ierr, dstr, domR(1), obuffer(jb)%pcp, lstk(jbuf)%lcp, domL)
          endif
          if (ierr.eq.0) write(utmp, 231) trim(pcstr)
          if (ierr.eq.0) write(utmp, 232) trim(lcstr)
          if (ierr.eq.0) write(utmp, 233) trim(dstr)
       endif
       if (is_msglev(lev_verbose, -levq_column)) then
          if (ierr.eq.0) call get_domain_result(ierr, cjoin, domL, htmp%pcp)
          if (ierr.eq.0) write(utmp, 211) trim(cjoin), trim(val)
       endif
       if (ierr.eq.0) then
          if (dryrun.gt.0) then
             write(utmp, '()')
          else
             mco = doml%mco
             nco = 0
             if (is_msglev(lev_verbose, -levq_coordinate)) nco = mco
             if (nco.gt.0) then
                write(fmt_xline, 202, IOSTAT=ierr) nco
                write(fmt_uline, 203, IOSTAT=ierr) nco
             else
                write(fmt_xline, 222, IOSTAT=ierr)
                write(fmt_uline, 223, IOSTAT=ierr)
             endif
             lidx(0:mco-1) = 0
             doml%bgn(0:mco-1) = user_index_bgn(doml%bgn(0:mco-1))

             select case(obuffer(jb)%k)
             case (kv_int)
                if (nco.gt.0) then
                   write(fmt_nline, 201, IOSTAT=ierr) nco, trim(afmt_int)
                else
                   write(fmt_nline, 221, IOSTAT=ierr) trim(afmt_int)
                endif
                do jl = 0, doml%n - 1
                   jp = physical_index(lidx, domr(1))
                   pidx(0:mco-1) = lidx(0:mco-1) + doml%bgn(0:mco-1)
                   if (jp.lt.0) then
                      if (.not. skip_undef) &
                           write(utmp, fmt_xline) pidx(0:nco-1)
                   else if (obuffer(jb)%vd(jp).eq.obuffer(jb)%undef) then
                      if (.not. skip_undef) &
                           write(utmp, fmt_uline) pidx(0:nco-1)
                   else
                      write(utmp, fmt_nline) pidx(0:nco-1), INT(obuffer(jb)%vd(jp))
                   endif
                   call incr_logical_index(lidx, doml)
                enddo
             case (kv_flt)
                if (nco.gt.0) then
                   write(fmt_nline, 201, IOSTAT=ierr) nco, trim(afmt_flt)
                else
                   write(fmt_nline, 221, IOSTAT=ierr) trim(afmt_flt)
                endif
                do jl = 0, doml%n - 1
                   jp = physical_index(lidx, domr(1))
                   pidx(0:mco-1) = lidx(0:mco-1) + doml%bgn(0:mco-1)
                   if (jp.lt.0) then
                      if (.not. skip_undef) &
                           write(utmp, fmt_xline) pidx(0:nco-1)
                   else if (obuffer(jb)%vd(jp).eq.obuffer(jb)%undef) then
                      if (.not. skip_undef) &
                           write(utmp, fmt_uline) pidx(0:nco-1)
                   else
                      write(utmp, fmt_nline) pidx(0:nco-1), REAL(obuffer(jb)%vd(jp), kind=KFLT)
                   endif
                   call incr_logical_index(lidx, doml)
                enddo
             case (kv_dbl)
                if (nco.gt.0) then
                   write(fmt_nline, 201, IOSTAT=ierr) nco, trim(afmt_dbl)
                else
                   write(fmt_nline, 221, IOSTAT=ierr) trim(afmt_dbl)
                endif
                do jl = 0, doml%n - 1
                   jp = physical_index(lidx, domr(1))
                   pidx(0:mco-1) = lidx(0:mco-1) + doml%bgn(0:mco-1)
                   if (jp.lt.0) then
                      if (.not. skip_undef) &
                           write(utmp, fmt_xline) pidx(0:nco-1)
                   else if (obuffer(jb)%vd(jp).eq.obuffer(jb)%undef) then
                      if (.not. skip_undef) &
                           write(utmp, fmt_uline) pidx(0:nco-1)
                   else
                      write(utmp, fmt_nline) pidx(0:nco-1), REAL(obuffer(jb)%vd(jp), kind=KDBL)
                   endif
                   call incr_logical_index(lidx, doml)
                enddo
             end select
          endif
       endif
    enddo
  end subroutine flush_buffer_each

!!!_   . flush_buffer_column - flush out buffers for each (columnized)
  subroutine flush_buffer_column(ierr, nbuf, bufh, bufj, lstk, cmode, u)
    use TOUZA_Std,only: choice
    use TOUZA_Nio,only: show_header, parse_header_size
    implicit none
    integer,      intent(out)         :: ierr
    integer,      intent(in)          :: nbuf
    integer,      intent(in)          :: bufh(0:*)
    integer,      intent(in)          :: bufj(0:*)
    type(stack_t),intent(in)          :: lstk(0:*)  ! only for logical coordinates
    integer,      intent(in)          :: cmode
    integer,      intent(in),optional :: u

    integer utmp
    integer jb, hb

    integer jbuf
    character(len=128) :: lcstr, pcstr, dstr
    character(len=64) :: fmt_nline
    character(len=64) :: fmt_xline
    character(len=64),allocatable :: vals(:)

    type(domain_t) :: doml, domr(1)
    type(buffer_t) :: htmp
    integer mco, nco
    integer jl, jp
    integer lidx(0:lcoor-1), pidx(0:lcoor-1)
    integer nb
    integer btmp(1)
    character(len=256) :: cjoin, val, ccols

    logical skip_undef
    integer jcolv, ncolv, j

    ierr = 0
    utmp = choice(ulog, u)

    allocate(vals(1:0), STAT=ierr)
    if (ierr.ne.0) then
       ierr = ERR_ALLOCATION
       return
    endif

    skip_undef = IAND(cmode, cmode_xundef).ne.0

212 format('## stack[', I0, '] ', A, 1x, A)
213 format('## stack[', I0, '] ', A, 1x, A, ' // ', A)

231 format('##      ', A)
232 format('##   >  ', A)
233 format('##   >> ', A)

201 format('(', I0, '(I0, 1x), ', I0, '(A, 1x))')
221 format('(', I0, '(A, 1x))')
211 format('#', A, 1x, A, 1x, A)

    ! write(*, *) def_write%kfmt, trim(def_write%fmt)

    do jbuf = 0, nbuf - 1
       jb = bufj(jbuf)
       hb = bufh(jbuf)
       nb = 1
       btmp(1) = hb

       if (ierr.eq.0) then
          call get_compromise_domain(ierr, domL, domR, btmp, lstk(jbuf:jbuf), nb, cmode_inclusive, htmp)
       endif
       if (ierr.eq.0) call get_obj_string(ierr, val, hb)

       if (is_msglev(lev_verbose, -levq_rec)) then
          if (ierr.eq.0) write(utmp, 213) &
               & user_index_bgn(jbuf), trim(val), trim(obuffer(jb)%desc), trim(obuffer(jb)%desc2)
       else if (is_msglev(lev_verbose, -levq_stack)) then
          if (ierr.eq.0) write(utmp, 212) &
               & user_index_bgn(jbuf), trim(val), trim(obuffer(jb)%desc)
       endif
       if (is_msglev(lev_verbose, -levq_stack)) then
          if (ierr.eq.0) call get_domain_string(ierr, lcstr, lstk(jbuf)%lcp)
          if (ierr.eq.0) call get_domain_string(ierr, pcstr, obuffer(jb)%pcp)
          if (ierr.eq.0) then
             call get_domain_shape(ierr, dstr, domR(1), obuffer(jb)%pcp, lstk(jbuf)%lcp, domL)
          endif
          if (ierr.eq.0) write(utmp, 231) trim(pcstr)
          if (ierr.eq.0) write(utmp, 232) trim(lcstr)
          if (ierr.eq.0) write(utmp, 233) trim(dstr)
       endif
       if (is_msglev(lev_verbose, -levq_column)) then
          if (ierr.eq.0) call get_domain_result(ierr, cjoin, domL, htmp%pcp, 1)
          if (ierr.eq.0) call get_domain_result(ierr, ccols, domL, htmp%pcp, 0, 1)
          if (ierr.eq.0) write(utmp, 211) trim(cjoin), trim(val), trim(ccols)
       endif
       if (ierr.eq.0) then
          ncolv = doml%end(0) - doml%bgn(0)
          if (size(vals).lt.ncolv) then
             deallocate(vals, STAT=ierr)
             if (ierr.eq.0) allocate(vals(0:ncolv-1), STAT=ierr)
             if (ierr.ne.0) ierr = ERR_ALLOCATION
          endif
       endif

       if (ierr.eq.0) then
          if (dryrun.gt.0) then
             write(utmp, '()')
          else
             mco = doml%mco
             nco = 0
             if (is_msglev(lev_verbose, -levq_coordinate)) nco = mco
             if (nco.gt.1) then
                write(fmt_xline, 201, IOSTAT=ierr) nco - 1, ncolv
             else
                write(fmt_xline, 221, IOSTAT=ierr) ncolv
             endif
             lidx(0:mco-1) = 0
             doml%bgn(0:mco-1) = user_index_bgn(doml%bgn(0:mco-1))
             select case(obuffer(jb)%k)
             case (kv_int)
                fmt_nline = afmt_int
                jcolv = 0
                do jl = 0, doml%n - 1
                   jp = physical_index(lidx, domr(1))
                   pidx(0:mco-1) = lidx(0:mco-1) + doml%bgn(0:mco-1)
                   if (jp.lt.0) then
                      if (.not. skip_undef) then
                         vals(jcolv) = '.'
                         jcolv = jcolv + 1
                      endif
                   else if (obuffer(jb)%vd(jp).eq.obuffer(jb)%undef) then
                      if (.not. skip_undef) then
                         vals(jcolv) = '_'
                         jcolv = jcolv + 1
                      endif
                   else
                      write(vals(jcolv), fmt_nline) INT(obuffer(jb)%vd(jp))
                      jcolv = jcolv + 1
                   endif
                   call incr_logical_index(lidx, doml)
                   if (lidx(0).eq.0) then
                      write(utmp, fmt_xline) pidx(1:nco-1), (trim(vals(j)),j=0,jcolv-1)
                      jcolv = 0
                   endif
                enddo
             case (kv_flt)
                fmt_nline = afmt_flt
                jcolv = 0
                do jl = 0, doml%n - 1
                   jp = physical_index(lidx, domr(1))
                   pidx(0:mco-1) = lidx(0:mco-1) + doml%bgn(0:mco-1)
                   if (jp.lt.0) then
                      if (.not. skip_undef) then
                         vals(jcolv) = '.'
                         jcolv = jcolv + 1
                      endif
                   else if (obuffer(jb)%vd(jp).eq.obuffer(jb)%undef) then
                      if (.not. skip_undef) then
                         vals(jcolv) = '_'
                         jcolv = jcolv + 1
                      endif
                   else
                      write(vals(jcolv), fmt_nline) REAL(obuffer(jb)%vd(jp), kind=KFLT)
                      jcolv = jcolv + 1
                   endif
                   call incr_logical_index(lidx, doml)
                   if (lidx(0).eq.0) then
                      write(utmp, fmt_xline) pidx(1:nco-1), (trim(vals(j)),j=0,jcolv-1)
                      jcolv = 0
                   endif
                enddo
             case (kv_dbl)
                fmt_nline = afmt_dbl
                jcolv = 0
                do jl = 0, doml%n - 1
                   jp = physical_index(lidx, domr(1))
                   pidx(0:mco-1) = lidx(0:mco-1) + doml%bgn(0:mco-1)
                   if (jp.lt.0) then
                      if (.not. skip_undef) then
                         vals(jcolv) = '.'
                         jcolv = jcolv + 1
                      endif
                   else if (obuffer(jb)%vd(jp).eq.obuffer(jb)%undef) then
                      if (.not. skip_undef) then
                         vals(jcolv) = '_'
                         jcolv = jcolv + 1
                      endif
                   else
                      write(vals(jcolv), fmt_nline) REAL(obuffer(jb)%vd(jp), kind=KDBL)
                      jcolv = jcolv + 1
                   endif
                   call incr_logical_index(lidx, doml)
                   if (lidx(0).eq.0) then
                      write(utmp, fmt_xline) pidx(1:nco-1), (trim(vals(j)),j=0,jcolv-1)
                      jcolv = 0
                   endif
                enddo
             end select
          endif
       endif
    enddo
    if (ierr.eq.0) then
       if (allocated(vals)) deallocate(vals, STAT=ierr)
    endif
  end subroutine flush_buffer_column

!!!_   . flush_buffer_horizontally - flush out buffers with horizontally pasting
  subroutine flush_buffer_horizontally(ierr, nbuf, bufh, bufj, lstk, cmode, u)
    use TOUZA_Std,only: choice, join_list
    implicit none
    integer,      intent(out)         :: ierr
    integer,      intent(in)          :: nbuf
    integer,      intent(in)          :: bufh(0:*)
    integer,      intent(in)          :: bufj(0:*)
    type(stack_t),intent(in)          :: lstk(0:*)
    integer,      intent(in)          :: cmode
    integer,      intent(in),optional :: u

    integer utmp

    type(domain_t) :: doml
    type(domain_t) :: domr(0:nbuf-1)

    integer j, jb, hb
    integer mco, nco
    integer jl, jp
    integer lidx(0:lcoor-1), pidx(0:lcoor-1)
    character(len=64) :: vals(0:nbuf-1)
    character(len=64) :: fmt_xline
    character(len=256) :: dstr
    character(len=256) :: cjoin, vjoin
    type(buffer_t) :: htmp

    logical skip_undef
    integer ccomp
    integer nundef

    ierr = 0
    utmp = choice(ulog, u)

    skip_undef = IAND(cmode, cmode_xundef).ne.0
    ccomp = IAND(cmode, cmode_compromise)

    if (ierr.eq.0) call get_compromise_domain(ierr, doml, domr, bufh, lstk, nbuf, ccomp, htmp)

    if (lev_verbose.ge.levq_column) then
       do j = 0, nbuf - 1
          hb = bufh(j)
          if (ierr.eq.0) call get_obj_string(ierr, vals(j), hb)
       enddo
       if (lev_verbose.ge.levq_stack) then
          do j = 0, nbuf - 1
             jb = bufj(j)
             hb = bufh(j)
202          format('## ', I0, 1x, A, 1x, A, 1x, A)
203          format('## ', I0, 1x, A, 1x, A, 1x, A, ' // ', A)
             if (ierr.eq.0) then
                call get_domain_shape(ierr, dstr, domr(j), obuffer(jb)%pcp, lstk(j)%lcp, doml)
             endif
             if (lev_verbose.ge.levq_rec) then
                if (ierr.eq.0) then
                   write(utmp, 203) user_index_bgn(j), trim(vals(j)), &
                        & trim(dstr), trim(obuffer(jb)%desc), trim(obuffer(jb)%desc2)
                endif
             else
                if (ierr.eq.0) then
                   write(utmp, 202) user_index_bgn(j), trim(vals(j)), &
                        & trim(dstr), trim(obuffer(jb)%desc)
                endif
             endif
          enddo
       endif
       if (ierr.eq.0) call get_domain_result(ierr, cjoin, doml, htmp%pcp)
       if (ierr.eq.0) call join_list(ierr, vjoin, vals(0:nbuf-1))
211    format('#', A, 1x, A)
       write(utmp, 211) trim(cjoin), trim(vjoin)
    endif

    if (ierr.eq.0) then
       mco = doml%mco
       nco = mco   ! nco: coordinate to output
       if (lev_verbose.lt.levq_coordinate) nco = 0
201    format('(', I0, '(I0, 1x), ', I0, '(A, 1x))')
221    format('(', I0, '(A, 1x))')
       if (nco.gt.0) then
          write(fmt_xline, 201, IOSTAT=ierr) nco, nbuf
       else
          write(fmt_xline, 221, IOSTAT=ierr) nbuf
       endif
       lidx(0:mco-1) = 0
       doml%bgn(0:mco-1) = user_index_bgn(doml%bgn(0:mco-1))
    endif

    if (ierr.eq.0) then
       if (dryrun.gt.0) then
          write(utmp, '()')
       else
          do jl = 0, doml%n - 1
             pidx(0:mco-1) = lidx(0:mco-1) + doml%bgn(0:mco-1)
             nundef = 0
             if (skip_undef) then
                do j = 0, nbuf - 1
                   jb = bufj(j)
                   jp = physical_index(lidx, domr(j))
                   if (jp.lt.0) then
                      nundef = nundef + 1
                   else if (obuffer(jb)%vd(jp).eq.obuffer(jb)%undef) then
                      nundef = nundef + 1
                   endif
                enddo
             endif
             if (nundef.lt.nbuf) then
                do j = 0, nbuf - 1
                   jb = bufj(j)
                   jp = physical_index(lidx, domr(j))
                   if (jp.lt.0) then
                      vals(j) = '.'
                   else if (obuffer(jb)%vd(jp).eq.obuffer(jb)%undef) then
                      vals(j) = '_'
                   else
                      select case(obuffer(jb)%k)
                      case (kv_int)
                         write(vals(j), afmt_int, IOSTAT=ierr) INT(obuffer(jb)%vd(jp))
                      case (kv_flt)
                         write(vals(j), afmt_flt, IOSTAT=ierr) REAL(obuffer(jb)%vd(jp), kind=KFLT)
                      case (kv_dbl)
                         write(vals(j), afmt_dbl, IOSTAT=ierr) REAL(obuffer(jb)%vd(jp), kind=KDBL)
                      case default
                         vals(j) = '*'
                      end select
                   endif
                enddo
                write(utmp, fmt_xline) pidx(0:nco-1), (trim(vals(j)),j=0,nbuf-1)
             endif
             call incr_logical_index(lidx, doml)
          enddo
       endif
    endif

  end subroutine flush_buffer_horizontally

!!!_   . flush_buffer_horizontally_column - flush out buffers with horizontally pasting (columnized)
  subroutine flush_buffer_horizontally_column(ierr, nbuf, bufh, bufj, lstk, cmode, u)
    use TOUZA_Std,only: choice, join_list
    implicit none
    integer,      intent(out)         :: ierr
    integer,      intent(in)          :: nbuf
    integer,      intent(in)          :: bufh(0:*)
    integer,      intent(in)          :: bufj(0:*)
    type(stack_t),intent(in)          :: lstk(0:*)  ! only for logical coordinates
    integer,      intent(in)          :: cmode
    integer,      intent(in),optional :: u

    integer utmp

    type(domain_t) :: doml
    type(domain_t) :: domr(0:nbuf)

    integer j, jb, hb
    integer mco, nco
    integer jlo, jp, jli
    integer lidx(0:lcoor-1), pidx(0:lcoor-1)
    integer vidx(0:lcoor-1)
    character(len=64),allocatable :: vals(:)
    character(len=64) :: fmt_xline
    character(len=256) :: dstr
    character(len=256) :: cjoin, vjoin
    type(buffer_t) :: htmp
    integer ncolv
    integer joffc(0:nbuf-1), jc

    logical skip_undef
    integer ccomp
    integer nundef
    integer cline, nline

    ierr = 0
    utmp = choice(ulog, u)

    skip_undef = IAND(cmode, cmode_xundef).ne.0
    ccomp = IAND(cmode, cmode_compromise)
    cline = 0
    ncolv = -1

    if (ierr.eq.0) call get_compromise_domain(ierr, doml, domr, bufh, lstk, nbuf, ccomp, htmp)
    if (ierr.eq.0) then
       ncolv = 0
       joffc(0) = 0
       do j = 0, nbuf - 1
          if (domr(j)%strd(cline).gt.0) then
             ! joffc(j+1) = domr(j)%end(cline) - domr(j)%bgn(cline)
             joffc(j+1) = domr(j)%iter(cline)
          else
             joffc(j+1) = 1
          endif
          joffc(j+1) = joffc(j) + joffc(j+1)
       enddo
       ncolv = joffc(nbuf)
       allocate(vals(0:ncolv-1), STAT=ierr)
       if (ierr.ne.0) then
          ierr = ERR_ALLOCATION
          return
       endif
       ! nline = doml%end(cline) - doml%bgn(cline)
       nline = doml%iter(cline)
    endif

    if (lev_verbose.ge.levq_column) then
       do j = 0, nbuf - 1
          hb = bufh(j)
          if (ierr.eq.0) call get_obj_string(ierr, vals(j), hb)
       enddo
       if (lev_verbose.ge.levq_stack) then
          do j = 0, nbuf - 1
             jb = bufj(j)
             hb = bufh(j)
202          format('## ', I0, 1x, A, 1x, A, 1x, A)
203          format('## ', I0, 1x, A, 1x, A, 1x, A, ' // ', A)
             if (ierr.eq.0) then
                call get_domain_shape(ierr, dstr, domr(j), obuffer(jb)%pcp, lstk(j)%lcp, doml)
             endif
             if (lev_verbose.ge.levq_rec) then
                if (ierr.eq.0) then
                   write(utmp, 203) user_index_bgn(j), trim(vals(j)), &
                        & trim(dstr), trim(obuffer(jb)%desc), trim(obuffer(jb)%desc2)
                endif
             else
                if (ierr.eq.0) then
                   write(utmp, 202) user_index_bgn(j), trim(vals(j)), &
                        & trim(dstr), trim(obuffer(jb)%desc)
                endif
             endif
             if (ierr.eq.0) then
                call get_domain_shape(ierr, dstr, domr(j), obuffer(jb)%pcp, lstk(j)%lcp, doml, 0, cline + 1)
             endif
             if (ierr.eq.0) vals(j) = trim(vals(j)) // trim(dstr)
          enddo
       endif
       if (ierr.eq.0) call get_domain_result(ierr, cjoin, doml, htmp%pcp, cline + 1)
       if (ierr.eq.0) call join_list(ierr, vjoin, vals(0:nbuf-1))
211    format('#', A, 1x, A)
       write(utmp, 211) trim(cjoin), trim(vjoin)
    endif

    if (ierr.eq.0) then
       mco = doml%mco
       nco = mco   ! nco: coordinate to output
       if (lev_verbose.lt.levq_coordinate) nco = 0
201    format('(', I0, '(I0, 1x), ', I0, '(A, 1x))')
221    format('(', I0, '(A, 1x))')
       if (nco.gt.1) then
          write(fmt_xline, 201, IOSTAT=ierr) nco - 1, ncolv
       else
          write(fmt_xline, 221, IOSTAT=ierr) ncolv
       endif
       lidx(0:mco-1) = 0
       doml%bgn(0:mco-1) = user_index_bgn(doml%bgn(0:mco-1))
    endif

    if (ierr.eq.0) then
       if (dryrun.gt.0) then
          write(utmp, '()')
       else
          do jlo = 0, doml%n - 1, nline
             pidx(0:mco-1) = lidx(0:mco-1) + doml%bgn(0:mco-1)
             nundef = 0
             do j = 0, nbuf - 1
                vidx(0:mco-1) = lidx(0:mco-1)
                jb = bufj(j)
                if (domr(j)%strd(cline).eq.0) then
                   jc = joffc(j)
                   jp = -1
                   do jli = 0, nline - 1
                      jp = physical_index(vidx, domr(j))
                      if (jp.ge.0) then
                         if (obuffer(jb)%vd(jp).eq.obuffer(jb)%undef) then
                            vals(jc) = '_'
                            nundef = nundef + 1
                         else
                            select case(obuffer(jb)%k)
                            case (kv_int)
                               write(vals(jc), afmt_int, IOSTAT=ierr) INT(obuffer(jb)%vd(jp))
                            case (kv_flt)
                               write(vals(jc), afmt_flt, IOSTAT=ierr) REAL(obuffer(jb)%vd(jp), kind=KFLT)
                            case (kv_dbl)
                               write(vals(jc), afmt_dbl, IOSTAT=ierr) REAL(obuffer(jb)%vd(jp), kind=KDBL)
                            case default
                               vals(jc) = '*'
                            end select
                         endif
                         exit
                      endif
                      call incr_logical_index(vidx, doml)
                   enddo
                   if (jp.lt.0) then
                      vals(jc) = '.'
                      nundef = nundef + 1
                   endif
                else
                   do jli = 0, nline - 1
                      jp = physical_index(vidx, domr(j))
                      jc = joffc(j) + jli
                      if (jp.lt.0) then
                         vals(jc) = '.'
                         nundef = nundef + 1
                      else if (obuffer(jb)%vd(jp).eq.obuffer(jb)%undef) then
                         vals(jc) = '_'
                         nundef = nundef + 1
                      else
                         select case(obuffer(jb)%k)
                         case (kv_int)
                            write(vals(jc), afmt_int, IOSTAT=ierr) INT(obuffer(jb)%vd(jp))
                         case (kv_flt)
                            write(vals(jc), afmt_flt, IOSTAT=ierr) REAL(obuffer(jb)%vd(jp), kind=KFLT)
                         case (kv_dbl)
                            write(vals(jc), afmt_dbl, IOSTAT=ierr) REAL(obuffer(jb)%vd(jp), kind=KDBL)
                         case default
                            vals(jc) = '*'
                         end select
                      endif
                      call incr_logical_index(vidx, doml)
                   enddo
                endif
             enddo
             if (ierr.eq.0) then
                write(utmp, fmt_xline, IOSTAT=ierr) pidx(cline+1:nco-1), (trim(vals(jc)),jc=0,ncolv-1)
             endif
             call incr_logical_index(lidx, doml, nline)
          enddo
       endif
    endif

  end subroutine flush_buffer_horizontally_column

!!!_   . list_stack
  subroutine list_stack(ierr, pop, irec, u)
    use TOUZA_Std,only: choice
    use TOUZA_Nio,only: show_header, parse_header_size
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: pop
    integer,intent(in)          :: irec
    integer,intent(in),optional :: u

    integer jinp
    integer jbuf
    integer hb
    integer js
    integer alev
    integer utmp
    character(len=64) :: cprop(0:lcoor-1)
    character(len=128) :: str, dstr

    type(buffer_t) :: htmp
    type(domain_t) :: doml, domr(1)
    integer btmp(1)
    type(stack_t) :: stmp(1)
    integer nb

    ierr = 0
    utmp = choice(ulog, u)
    nb = 1

    do jinp = 0, pop - 1
       js = mstack - pop + jinp
       hb = bstack(js)%bh

       alev = anchor_h2level(hb)
       if (alev.ge.0) cycle
       jbuf  = buf_h2item(hb)

       btmp(1) = hb
       stmp(1) = bstack(js)

       if (ierr.eq.0) then
          call get_compromise_domain(ierr, domL, domR, btmp, stmp, nb, cmode_inclusive, htmp)
       endif
       if (ierr.eq.0) then
          call get_domain_shape &
               & (ierr, dstr, domR(1), obuffer(jbuf)%pcp, bstack(js)%lcp, domL, &
               &  ldelim=' ', rdelim=' ', sep=',')
       endif
       if (ierr.eq.0) call get_obj_string(ierr, str, hb)
101    format(I0, 1x, I0, 1x, A, 1x, A, 1x, A)
       write(utmp, 101) &
            & user_index_bgn(irec), user_index_bgn(js), &
            & trim(str), trim(dstr), trim(obuffer(jbuf)%desc)
    enddo

  end subroutine list_stack

!!!_   . copy_set_header
  subroutine copy_set_header &
       & (ierr, bufh, bref, pop)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: bufh ! output buffer
    integer,intent(in)  :: bref ! reference buffer
    integer,intent(in)  :: pop

    integer jref
    integer jb

    ierr = 0
    if (pop.gt.0) then
       jref = buf_h2item(bref)
       jb = buf_h2item(bufh)
       obuffer(jb)%pcp(:) = obuffer(jref)%pcp(:)
       obuffer(jb)%undef = obuffer(jref)%undef
    else
       jb = buf_h2item(bufh)
       obuffer(jb)%undef = UNDEF
       obuffer(jb)%pcp(:) = def_loop
    endif
    return
  end subroutine copy_set_header

!!!_   . apply_INDEX
  subroutine apply_INDEX (ierr, handle, lefts)
    use TOUZA_Std,only: find_first
    implicit none
    integer,      intent(out)   :: ierr
    integer,      intent(in)    :: handle
    type(stack_t),intent(inout) :: lefts(0:)
    integer jotgt
    ! integer jlogc, jphyc
    integer lasth, jbref
    integer j, jb
    integer js
    integer b, e,  n, jc
    ! integer cpidx(0:lcoor-1), ctype(0:lcoor-1)
    ! character(len=litem) cname(0:lcoor-1)
    character(len=64) :: opr
    type(buffer_t) :: buf
    integer nb
    type(domain_t) :: doml, domr(1)
    integer btmp(1), ptmp(1)
    type(stack_t) :: stmp(1)

    ierr = 0
    jotgt = handle - opr_C0
    call query_opr_name(ierr, opr, handle)

    if (ierr.eq.0) call pop_stack(ierr, lasth, keep=.TRUE.)
    if (ierr.eq.0) then
       jbref = buf_h2item(lasth)
       ierr = min(0, jbref)
    endif
    if (ierr.eq.0) then
       js = mstack - 1
       btmp(1) = lasth
       stmp(1) = bstack(js)
       ptmp(1) = js
       nb = 1
       if (ierr.eq.0) call tweak_coordinates(ierr, domL, domR, btmp, stmp, nb, buf, clip=.FALSE.)
       if (ierr.eq.0) call set_inclusive_domain(ierr, domL, domR, btmp, stmp, nb)
       if (ierr.eq.0) call settle_output_domain(ierr, domL)
       if (ierr.eq.0) call set_output_buffer(ierr, buf, btmp, domL)
    endif
    ! debug
    ! if (ierr.eq.0) call show_domain(ierr, domL,    'index/L', indent=3)
    ! if (ierr.eq.0) call show_domain(ierr, domR(1), 'index/R', indent=4)
    ! if (ierr.eq.0) then
    !    write(*, *) '     index/c:', jotgt, domL%mco, domR(1)%mco
    ! endif

    if (ierr.eq.0) then
       if (jotgt.lt.domL%mco) then
          b = domL%bgn(jotgt)
          e = domL%end(jotgt)
          e = max(1 + b, e)
          n = max(1, e - b)
       else
          b = 0
          e = 0
          n = 1
       endif
       do j = 0, size(lefts) - 1
          jb = buf_h2item(lefts(j)%bh)
          obuffer(jb)%pcp(:) = def_loop

          obuffer(jb)%pcp(:) = buf%pcp(:)
          obuffer(jb)%pcp(0:domL%mco-1)%flg = loop_null
          obuffer(jb)%pcp(domL%mco:)%flg = loop_unset

          obuffer(jb)%pcp(jotgt)%bgn = b
          obuffer(jb)%pcp(jotgt)%end = e
          obuffer(jb)%pcp(jotgt)%flg = loop_normal
          obuffer(jb)%pcp(jotgt)%name = buf%pcp(jotgt)%name
          obuffer(jb)%undef = UNDEF
          if (ierr.eq.0) call alloc_buffer_t(ierr, obuffer(jb), n)
          if (ierr.eq.0) then
             obuffer(jb)%k = kv_int
             do jc = b, e - 1
                obuffer(jb)%vd(jc - b) = REAL(user_index_bgn(jc), kind=KDBL)
             enddo
          endif
          obuffer(jb)%ilev  = ilev_term
          obuffer(jb)%desc  = trim(opr)
          obuffer(jb)%desc2 = trim(opr)
       enddo
    endif
  end subroutine apply_INDEX

!!!_   . apply_FLAT
  subroutine apply_FLAT (ierr, handle, lefts)
    use TOUZA_Std,only: parse_number, join_list
    implicit none
    integer,      intent(out)   :: ierr
    integer,      intent(in)    :: handle
    type(stack_t),intent(inout) :: lefts(0:)
    ! integer jlogc, jphyc
    integer lasth, jbref
    integer j, jb
    integer js
    integer b, e,  n, jc
    ! integer cpidx(0:lcoor-1), ctype(0:lcoor-1)
    ! character(len=litem) cname(0:lcoor-1)
    character(len=64) :: opr, oprc
    type(buffer_t) :: buf
    integer nb
    type(domain_t) :: domL, domR(1), domP
    integer btmp(1), ptmp(1)
    integer nc
    type(stack_t) :: stmp(1)

    integer jcx, jt, jflat
    integer xco(0:lcoor-1)
    integer lidx(0:lcoor-1)

    ierr = 0

    call query_opr_name(ierr, opr, handle)

    if (ierr.eq.0) call pop_stack(ierr, lasth, keep=.TRUE.)
    if (ierr.eq.0) then
       jbref = buf_h2item(lasth)
       ierr = min(0, jbref)
    endif
    if (ierr.eq.0) then
       js = mstack - 1
       btmp(1) = lasth
       ptmp(1) = js
       stmp(1) = bstack(js)
       nb = 1
       if (ierr.eq.0) call tweak_coordinates(ierr, domL, domR, btmp, stmp, nb, buf)
       if (ierr.eq.0) call set_inclusive_domain(ierr, domL, domR, btmp, stmp, nb)
       if (ierr.eq.0) call settle_output_domain(ierr, domL)
       if (ierr.eq.0) call set_output_buffer(ierr, buf, btmp, domL)
    endif
    if (ierr.eq.0) then
       loop_stack: do j = 0, size(lefts) - 1
          nc = count_effective(lefts(j)%lcp)
          if (nc.le.0) then
             nc = domL%mco
             do jc = 0, nc - 1
                xco(jc) = jc
             enddo
          else
             do jc = 0, nc - 1
                call parse_number(ierr, jcx, lefts(j)%lcp(jc)%name)
                if (ierr.ne.0) then
                   do jt = 0, domL%mco - 1
                      if (buf%pcp(jt)%name.eq.lefts(j)%lcp(jc)%name) then
                         jcx = jt
                         ierr = 0
                         exit
                      endif
                   enddo
                else
                   jcx = system_index_bgn(jcx)
                endif
                if (jcx.lt.0.or.jcx.ge.domL%mco) ierr = -1
                if (ierr.ne.0) then
                   ierr = ERR_NOT_FOUND
                   call message(ierr, 'failed to search coordinate: ' // trim(lefts(j)%lcp(jc)%name))
                   exit loop_stack
                endif
                xco(jc) = jcx
             enddo
          endif
          jb = buf_h2item(lefts(j)%bh)
          obuffer(jb)%undef = UNDEF

          obuffer(jb)%pcp(:) = buf%pcp(:)
          domP%mco = domL%mco
          do jc = 0, domL%mco - 1
             if (ALL(jc.ne.xco(0:nc-1))) then
                obuffer(jb)%pcp(jc)%flg = loop_null
                domL%iter(jc) = 1
                domP%bgn(jc) = 0
                domP%end(jc) = 1
                domP%ofs(jc) = 0
                domP%cyc(jc) = 0
                domP%strd(jc) = 0
             endif
          enddo
          n = 1
          do jc = 0, nc - 1
             jcx = xco(jc)
             b = domL%bgn(jcx)
             e = domL%end(jcx)
             e = max(1 + b, e)
             domP%bgn(jcx) = 0
             domP%ofs(jcx) = b
             domP%cyc(jcx) = 0
             domP%end(jcx) = max(1, e - b)
             domP%strd(jcx) = n
             n = n * domP%end(jcx)
          enddo
          lidx(0:domL%mco-1) = 0
          if (ierr.eq.0) call alloc_buffer_t(ierr, obuffer(jb), n)
          if (ierr.eq.0) then
             obuffer(jb)%k = kv_int
             do jt = 0, n - 1
                jflat = physical_index(lidx, domP)
                obuffer(jb)%vd(jt) = REAL(user_index_bgn(jflat), kind=KDBL)
                call incr_logical_index(lidx, domL)
             enddo
          endif
          oprc = ' '
          if (ierr.eq.0) then
             if (nc.lt.domL%mco) then
                call join_list &
                     & (ierr, oprc, xco(0:nc-1), sep=',', ldelim='[', rdelim=']')
             endif
          endif
          obuffer(jb)%ilev  = ilev_term
          obuffer(jb)%desc  = trim(opr) // trim(oprc)
          obuffer(jb)%desc2 = trim(opr) // trim(oprc)
       enddo loop_stack
    endif
  end subroutine apply_FLAT

!!!_   . apply_MISS
  subroutine apply_MISS (ierr, handle, lefts)
    implicit none
    integer,      intent(out)   :: ierr
    integer,      intent(in)    :: handle
    type(stack_t),intent(inout) :: lefts(0:)
    integer lasth, jb, jbref
    ierr = 0

    if (ierr.eq.0) call pop_stack(ierr, lasth, keep=.TRUE.)
    if (ierr.eq.0) then
       jbref = buf_h2item(lasth)
       ierr = min(0, jbref)
    endif
    if (ierr.eq.0) then
       jb = buf_h2item(lefts(0)%bh)
       call alloc_buffer_t(ierr, obuffer(jb), 1)
    endif
    if (ierr.eq.0) then
       obuffer(jb)%k = obuffer(jbref)%k
       obuffer(jb)%vd(:) = obuffer(jbref)%undef
       if (obuffer(jbref)%undef.eq.LLIMIT) then
          obuffer(jb)%undef = ULIMIT
       else
          obuffer(jb)%undef = LLIMIT
       endif
    endif
  end subroutine apply_MISS

!!!_   . apply_opr_UNARY
  subroutine apply_opr_UNARY &
       & (ierr, hopr, lefts, bufi, func, bin)
    use TOUZA_Std,only: choice
    implicit none
    integer,      intent(out)         :: ierr
    integer,      intent(in)          :: hopr
    type(stack_t),intent(inout)       :: lefts(0:)
    integer,      intent(in)          :: bufi(0:)
    logical,      intent(in),optional :: bin
    interface
       subroutine func &
            & (ierr, Z, domZ, X, domX, F)
         use chak_lib,only: KFLT, KDBL, domain_t
         implicit none
         integer,          intent(out) :: ierr
         real(kind=__KBUF),intent(out) :: Z(0:*)
         real(kind=__KBUF),intent(in)  :: X(0:*)
         type(domain_t),   intent(in)  :: domZ, domX
         real(kind=__KBUF),intent(in)  :: F
       end subroutine func
    end interface

    integer jout
    integer jinp, ninp
    integer hbL, hbR
    integer jbL, jbR
    real(kind=KBUF) :: fillR
    logical check
    integer ofsi
    integer m

    integer,parameter :: nb = 1
    integer btmp(nb), ptmp(nb)
    type(domain_t) :: domL, domR(nb)
    type(stack_t) :: stmp(nb)

    ierr = 0
    check = choice(.FALSE., bin)

    ninp = size(bufi)
    ofsi = 0
    if (is_anchor(bufi(ofsi))) then
       ninp = ninp - 1
       ofsi = ofsi + 1
    endif

    if (size(lefts).ne.ninp) ierr = ERR_INVALID_PARAMETER
    if (ierr.ne.0) then
       call message(ierr, 'invalid argument length for unary operation.')
       return
    endif

    do jout = ninp - 1, 0, -1
       jinp = ofsi + jout
       ptmp(nb) = mstack - ninp + jout
       stmp(nb) = bstack(mstack - ninp + jout)
       if (ierr.eq.0) then
          hbL = lefts(jout)%bh
          hbR = bufi(jinp)
          jbL = buf_h2item(hbL)
          jbR = buf_h2item(hbR)
          fillR = obuffer(jbR)%undef
          btmp(nb) = hbR
          if (check.and. (fillR.eq.TRUE.or.fillR.eq.FALSE)) then
             ierr = ERR_PANIC
             call message(ierr, 'MISS value cannot be 1 nor 0')
          endif
       endif

       if (ierr.eq.0) then
          m = buffer_vmems(obuffer(jbR))
          if (m.lt.0) cycle
       endif
       if (ierr.eq.0) call copy_set_header(ierr, lefts(jout)%bh, bufi(jinp), 1)

       if (ierr.eq.0) call tweak_coordinates(ierr, domL, domR, btmp, stmp, nb, obuffer(jbL))
       if (ierr.eq.0) call set_inclusive_domain(ierr, domL, domR, btmp, stmp, nb)
       if (ierr.eq.0) call settle_output_domain(ierr, domL)
       if (ierr.eq.0) call settle_input_domain(ierr, domR(1), btmp(1), stmp(1), domL)
       if (ierr.eq.0) call set_output_buffer_h(ierr, lefts(jout)%bh, btmp, domL)

       ! if (ierr.eq.0) call show_domain(ierr, domL,    'unary/L', indent=3)
       ! if (ierr.eq.0) call show_domain(ierr, domR(1), 'unary/R', indent=4)

       if (ierr.eq.0) then
          call func &
               & (ierr, &
               &  obuffer(jbL)%vd, domL, &
               &  obuffer(jbR)%vd, domR(1), fillR)
       endif
       if (ierr.eq.0) call set_unary_descr(ierr, hbL, hbR, hopr)
    enddo

  end subroutine apply_opr_UNARY

!!!_   . apply_opr_BINARY_lazy
  subroutine apply_opr_BINARY_lazy &
       & (ierr, hopr, lefts, bufi, cmode, func, neutral, rev)
    use TOUZA_std,only: choice, jot
    implicit none
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: hopr
    type(stack_t),  intent(inout)       :: lefts(0:)
    integer,        intent(in)          :: bufi(0:)
    integer,        intent(in)          :: cmode
    real(kind=KBUF),intent(in),optional :: neutral
    logical,        intent(in),optional :: rev
    interface
       subroutine func &
            & (ierr, Z, domZ, FZ, X, domX, FX)
         use chak_lib,only: KFLT, KDBL, domain_t
         implicit none
         integer,          intent(out)   :: ierr
         real(kind=__KBUF),intent(inout) :: Z(0:*)
         real(kind=__KBUF),intent(in)    :: X(0:*)
         type(domain_t),   intent(in)    :: domZ, domX
         real(kind=__KBUF),intent(in)    :: FZ,   FX
       end subroutine func
    end interface

    integer,parameter :: defopr = 2

    integer nopr
    integer jout, nout
    integer jinp, ninp, jj
    integer hbL, hbR
    integer jbL, jbR
    integer ofsi
    integer jset
    type(domain_t) :: domL
    type(domain_t) :: domR(0:size(bufi)-1)
    integer        :: pstk(0:size(bufi)-1)

    real(kind=KBUF) :: fillL, fillR

    integer m
    integer ntmp
    integer btmp(0:size(bufi)-1)
    type(stack_t) :: stmp(0:size(bufi)-1)

    ierr = 0

    ninp = size(bufi)
    ofsi = 0
    if (is_anchor(bufi(ofsi))) then
       ninp = ninp - 1
       ofsi = ofsi + 1
    endif
    call jot(pstk, ninp, e=mstack)
    nout = size(lefts)
    nopr = ninp / nout
    ! Reduce every (ninp / nout) buffer to nout.
    ! i.e., error if (ninp % nout) != 0 or (ninp / nout) < 2

    if (mod(ninp, nout).ne.0) ierr = ERR_INVALID_PARAMETER
    if (nopr.lt.defopr) ierr = ERR_INVALID_PARAMETER
    if (ierr.ne.0) then
       call message(ierr, 'invalid operands for binary operation.')
       return
    endif

    do jout = nout - 1 , 0, -1
       jinp = jout * nopr + ofsi

       ntmp = 0
       do jj = jinp, jinp + nopr - 1
          hbR = bufi(jj)
          jbR = buf_h2item(hbR)
          m = buffer_vmems(obuffer(jbR))
          if (m.ge.0) then
             btmp(ntmp) = hbR
             stmp(ntmp) = bstack(pstk(jj-ofsi))
             ntmp = ntmp + 1
          endif
       enddo
       if (ntmp.lt.defopr) then
          ierr = ERR_INVALID_PARAMETER
          call message(ierr, 'too much null operands for binary operation.')
          return
       endif

       hbL = lefts(jout)%bh
       jbL = buf_h2item(hbL)

       if (ierr.eq.0) call copy_set_header(ierr, hbL, btmp(0), ntmp)
       if (ierr.eq.0) then
          call get_compromise_domain &
               & (ierr, domL, domR, btmp(0:ntmp-1), stmp(0:ntmp-1), ntmp, cmode, obuffer(jbL))
       endif
       if (choice(.FALSE., rev)) then
          if (ierr.eq.0) then
             jset = ntmp - 1
             hbR = btmp(jset)
             jbR = buf_h2item(hbR)
             fillL = obuffer(jbR)%undef
             fillR = choice(fillL, neutral)
             call apply_COPY &
                  & (ierr, &
                  &  obuffer(jbL)%vd, domL, &
                  &  obuffer(jbR)%vd, domR(jset), fillR)
          endif
          if (ierr.eq.0) then
             do jj = ntmp - 2, 0, -1
                jset = jj
                hbR = btmp(jj)
                jbR = buf_h2item(hbR)
                fillR = obuffer(jbR)%undef
                call func &
                     & (ierr, &
                     &  obuffer(jbL)%vd, domL,       fillL, &
                     &  obuffer(jbR)%vd, domR(jset), fillR)
             enddo
          endif
       else
          if (ierr.eq.0) then
             jset = 0
             hbR = btmp(jset)
             jbR = buf_h2item(hbR)
             fillL = obuffer(jbR)%undef
             fillR = choice(fillL, neutral)
             call apply_COPY &
                  & (ierr, &
                  &  obuffer(jbL)%vd, domL, &
                  &  obuffer(jbR)%vd, domR(jset), fillR)
          endif
          if (ierr.eq.0) then
             do jj = 1, ntmp - 1
                jset = jj
                hbR = btmp(jj)
                jbR = buf_h2item(hbR)
                fillR = obuffer(jbR)%undef
                call func &
                     & (ierr, &
                     &  obuffer(jbL)%vd, domL,       fillL, &
                     &  obuffer(jbR)%vd, domR(jset), fillR)
             enddo
          endif
       endif
       if (ierr.eq.0) then
          call set_binary_descr(ierr, hbL, btmp(0:ntmp-1), hopr)
       endif
    enddo
  end subroutine apply_opr_BINARY_lazy

!!!_   . apply_opr_BINARY
  subroutine apply_opr_BINARY &
       & (ierr, hopr, lefts, bufi, cmode, func, bin)
    use TOUZA_Std,only: choice, jot
    implicit none
    integer,      intent(out)         :: ierr
    integer,      intent(in)          :: hopr
    type(stack_t),intent(inout)       :: lefts(0:)
    integer,      intent(in)          :: bufi(0:)
    integer,      intent(in)          :: cmode
    logical,      intent(in),optional :: bin
    interface
       subroutine func &
            & (ierr, Z, domZ, FZ, X, domX, FX)
         use chak_lib,only: KFLT, KDBL, domain_t
         implicit none
         integer,          intent(out)   :: ierr
         real(kind=__KBUF),intent(inout) :: Z(0:*)
         real(kind=__KBUF),intent(in)    :: X(0:*)
         type(domain_t),   intent(in)    :: domZ, domX
         real(kind=__KBUF),intent(in)    :: FZ,   FX
       end subroutine func
    end interface

    integer,parameter :: defopr = 2

    integer nopr
    integer jout, nout
    integer jinp, ninp, jj
    integer hbL, hbR
    integer jbL, jbR
    integer ofsi
    integer jset
    type(domain_t) :: domL
    type(domain_t) :: domR(0:size(bufi)-1)
    integer        :: pstk(0:size(bufi)-1)

    real(kind=KBUF) :: fillL, fillR
    logical check

    integer m
    integer ntmp
    integer btmp(0:size(bufi)-1)
    type(stack_t) :: stmp(0:size(bufi)-1)

    ierr = 0
    check = choice(.FALSE., bin)

    ninp = size(bufi)
    ofsi = 0
    if (is_anchor(bufi(ofsi))) then
       ninp = ninp - 1
       ofsi = ofsi + 1
    endif
    call jot(pstk, ninp, e=mstack)
    ! write(*, *) 'jot', ninp, ofsi, pstk(0:ninp-1)
    nout = size(lefts)
    nopr = ninp / nout
    ! Reduce every (ninp / nout) buffer to nout.
    ! i.e., error if (ninp % nout) != 0 or (ninp / nout) < 2

    if (mod(ninp, nout).ne.0) ierr = ERR_INVALID_PARAMETER
    if (nopr.lt.defopr) ierr = ERR_INVALID_PARAMETER
    if (ierr.ne.0) then
       call message(ierr, 'invalid operands for binary operation.')
       return
    endif

    do jout = nout - 1 , 0, -1
       jinp = jout * nopr + ofsi

       ntmp = 0
       do jj = jinp, jinp + nopr - 1
          hbR = bufi(jj)
          jbR = buf_h2item(hbR)
          m = buffer_vmems(obuffer(jbR))
          ! write(*, *) 'tmp', jout, jinp, jj, bufi(jj), m
          if (m.ge.0) then
             btmp(ntmp) = hbR
             stmp(ntmp) = bstack(pstk(jj-ofsi))
             ntmp = ntmp + 1
          endif
       enddo
       if (ntmp.lt.defopr) then
          ierr = ERR_INVALID_PARAMETER
          call message(ierr, 'too much null operands for binary operation.')
          return
       endif

       hbL = lefts(jout)%bh
       jbL = buf_h2item(hbL)

       if (ierr.eq.0) call copy_set_header(ierr, hbL, btmp(0), ntmp)
       if (ierr.eq.0) then
          call get_compromise_domain &
               & (ierr, domL, domR, btmp(0:ntmp-1), stmp(0:ntmp-1), ntmp, cmode, obuffer(jbL))
       endif
       if (ierr.eq.0) then
          hbR = btmp(0)
          jbR = buf_h2item(hbR)
          fillL = obuffer(jbR)%undef
          if (check.and. (fillL.eq.TRUE.or.fillL.eq.FALSE)) then
             ierr = ERR_PANIC
             call message(ierr, 'MISS value cannot be 1 nor 0')
             return
          endif
       endif
       if (ierr.eq.0) then
          jset = 0
          call apply_COPY &
               & (ierr, &
               &  obuffer(jbL)%vd, domL, &
               &  obuffer(jbR)%vd, domR(jset), fillL)
       endif
       if (ierr.eq.0) then
          do jj = 1, ntmp - 1
             jset = jj
             hbR = btmp(jj)
             jbR = buf_h2item(hbR)
             fillR = obuffer(jbR)%undef
             if (check.and. (fillR.eq.TRUE.or.fillR.eq.FALSE)) then
                ierr = ERR_PANIC
                call message(ierr, 'MISS value cannot be 1 nor 0')
                return
             endif
             if (ierr.eq.0) then
                call func &
                     & (ierr, &
                     &  obuffer(jbL)%vd, domL,       fillL, &
                     &  obuffer(jbR)%vd, domR(jset), fillR)
             endif
          enddo
       endif
       if (ierr.eq.0) then
          call set_binary_descr(ierr, hbL, btmp(0:ntmp-1), hopr)
       endif
    enddo
  end subroutine apply_opr_BINARY

!!!_   . apply_opr_REDUCE
  subroutine apply_opr_REDUCE &
       & (ierr, hopr, lefts, bufi, func, neutral)
    use TOUZA_Std,only: choice
    implicit none
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: hopr
    type(stack_t),  intent(inout)       :: lefts(0:)
    integer,        intent(in)          :: bufi(0:)
    real(kind=KBUF),intent(in),optional :: neutral
    interface
       subroutine func &
            & (ierr, Z, domZ, domY, X, domX, F)
         use chak_lib,only: KFLT, KDBL, domain_t
         implicit none
         integer,          intent(out)   :: ierr
         real(kind=__KBUF),intent(inout) :: Z(0:*)
         real(kind=__KBUF),intent(in)    :: X(0:*)
         type(domain_t),   intent(in)    :: domZ, domY, domX
         real(kind=__KBUF),intent(in)    :: F
       end subroutine func
    end interface

    integer jout
    integer jinp, ninp
    integer hbL, hbR
    integer jbL, jbR
    real(kind=KBUF) :: fillR, fillL
    integer ofsi
    integer m

    integer,parameter :: nb = 1
    integer btmp(nb)
    type(domain_t) :: domL, domZ, domR(nb)
    type(stack_t)  :: ltmp(nb)

    ierr = 0

    ninp = size(bufi)
    ofsi = 0
    if (is_anchor(bufi(ofsi))) then
       ninp = ninp - 1
       ofsi = ofsi + 1
    endif
    if (size(lefts).ne.ninp) ierr = ERR_INVALID_PARAMETER
    if (ierr.ne.0) then
       write(*, *) ninp, size(lefts)
       call message(ierr, 'invalid argument length for reduction operation.')
       return
    endif

    do jout = ninp - 1, 0, -1
       jinp = ofsi + jout
       ! ptmp(nb) = mstack - ninp + jout
       ! stmp(nb) = bstack(mstack - ninp + jout)
       ltmp(nb) = lefts(jout)   ! use logical coordinate at left

       if (ierr.eq.0) then
          hbL = lefts(jout)%bh
          hbR = bufi(jinp)
          jbL = buf_h2item(hbL)
          jbR = buf_h2item(hbR)
          fillR = obuffer(jbR)%undef
          btmp(nb) = hbR
       endif

       if (ierr.eq.0) then
          m = buffer_vmems(obuffer(jbR))
          if (m.lt.0) cycle
       endif
       if (ierr.eq.0) call copy_set_header(ierr, lefts(jout)%bh, bufi(jinp), 1)
       ! if (ierr.eq.0) call show_lpp(ierr, lefts(jout)%lcp(:))

       if (ierr.eq.0) call tweak_coordinates(ierr, domL, domR, btmp, ltmp, nb, obuffer(jbL))
       if (ierr.eq.0) call set_inclusive_domain(ierr, domL, domR, btmp, ltmp, nb)
       if (ierr.eq.0) call settle_output_domain(ierr, domL)
       if (ierr.eq.0) call settle_input_domain(ierr, domR(1), btmp(1), ltmp(1), domL)
       if (ierr.eq.0) call settle_reduce_domain(ierr, domZ, obuffer(jbL), lefts(jout), domL)
       ! if (ierr.eq.0) call reduce_output_domain(ierr, domZ, domL, lefts(jout), obuffer(jbL))
       if (ierr.eq.0) call reduce_output_buffer(ierr, obuffer(jbL), lefts(jout), btmp, domZ)

       ! if (ierr.eq.0) call show_domain(ierr, domZ,    'reduce/Z', indent=3)
       ! if (ierr.eq.0) call show_domain(ierr, domL,    'reduce/L', indent=4)
       ! if (ierr.eq.0) call show_domain(ierr, domR(1), 'reduce/R', indent=5)
       fillL = choice(fillR, neutral)
       obuffer(jbL)%vd(:) = fillL

       if (ierr.eq.0) then
          call func &
               & (ierr, &
               &  obuffer(jbL)%vd, domZ,    domL, &
               &  obuffer(jbR)%vd, domR(1), fillR)
       endif
       if (ierr.eq.0) call set_unary_descr(ierr, hbL, hbR, hopr)
    enddo

  end subroutine apply_opr_REDUCE

!!!_   . set_unary_descr
  subroutine set_unary_descr &
       & (ierr, lefth, righth, oprh)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: lefth
    integer,intent(in)  :: righth
    integer,intent(in)  :: oprh
    integer jbl,  jbr
    integer ilevi, ilevo
    character(len=64) :: opr
    character(len=64) :: istr
    integer jerr

    ierr = 0
    if (ierr.eq.0) call query_opr_name(ierr, opr, oprh)

    jbl = buf_h2item(lefth)
    jbr = buf_h2item(righth)
    obuffer(jbl)%desc = trim(obuffer(jbr)%desc) // ' ' // trim(opr)

    if (ierr.eq.0) then
       ilevi = obuffer(jbr)%ilev
       call inquire_opr_infix(ierr, ilevo, istr, oprh)
       if (ierr.eq.0) then
101       format(A, '(', A, ')')
111       format(A, A)
112       format(A, '(', A, ')')
121       format(A, '[', A, ']')
          if (ilevo.eq.ilev_call) then
             write(obuffer(jbl)%desc2, 101, IOSTAT=jerr) trim(istr), trim(obuffer(jbr)%desc2)
          else if (ilevo.eq.ilev_neg) then
             if (ilevi.ge.ilevo) then
                write(obuffer(jbl)%desc2, 112, IOSTAT=jerr) trim(istr), trim(obuffer(jbr)%desc2)
             else
                write(obuffer(jbl)%desc2, 111, IOSTAT=jerr) trim(istr), trim(obuffer(jbr)%desc2)
             endif
          else
             write(obuffer(jbl)%desc2, 121, IOSTAT=jerr) trim(istr), trim(obuffer(jbr)%desc2)
          endif
          obuffer(jbl)%ilev = ilevo
       endif
    endif

  end subroutine set_unary_descr

!!!_   . set_binary_descr
  subroutine set_binary_descr &
       & (ierr, bufo, bufi, oprh)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: bufo
    integer,intent(in)  :: bufi(:)
    integer,intent(in)  :: oprh
    integer jinp, ninp
    integer jbl,  jbr, hbr
    character(len=64) :: opr
    integer ilevo
    character(len=64) :: istr
    character :: lsep, rsep

    ierr = 0
    ninp = size(bufi)
    jbl = buf_h2item(bufo)

    jinp = 1
    hbr = bufi(jinp)
    jbr = buf_h2item(hbr)
    obuffer(jbl)%desc = trim(obuffer(jbr)%desc)
    do jinp = 2, ninp
       hbr = bufi(jinp)
       jbr = buf_h2item(hbr)
       obuffer(jbl)%desc = trim(obuffer(jbl)%desc) // ' ' // trim(obuffer(jbr)%desc)
    enddo
    if (ierr.eq.0) call query_opr_name(ierr, opr, oprh)
    if (ierr.eq.0) then
       obuffer(jbl)%desc = trim(obuffer(jbl)%desc) // ' ' // trim(opr)
       if (ninp.gt.2) then
          obuffer(jbl)%desc = str_MARK // ' ' // trim(obuffer(jbl)%desc) // ' ' // str_CUM
       endif
    endif

    if (ierr.eq.0) then
       call inquire_opr_infix(ierr, ilevo, istr, oprh)
       if (ierr.eq.0) then
          if (ANY(ilevo.eq.(/ilev_add, ilev_exp, ilev_logical, &
               & ilev_mul, ilev_and, ilev_or, ilev_xor, ilev_shift/))) then
             jinp = 1
             hbr = bufi(jinp)
             jbr = buf_h2item(hbr)
             if (obuffer(jbr)%ilev.gt.ilevo) then
                obuffer(jbl)%desc2 = '(' // trim(obuffer(jbr)%desc2) // ')'
             else
                obuffer(jbl)%desc2 = trim(obuffer(jbr)%desc2)
             endif
             do jinp = 2, ninp
                hbr = bufi(jinp)
                jbr = buf_h2item(hbr)
                if (obuffer(jbr)%ilev.gt.ilevo) then
                   obuffer(jbl)%desc2 = trim(obuffer(jbl)%desc2) // trim(istr) // &
                        & '(' // trim(obuffer(jbr)%desc2) // ')'
                else
                   obuffer(jbl)%desc2 = trim(obuffer(jbl)%desc2) // trim(istr) // &
                        & trim(obuffer(jbr)%desc2)
                endif
             enddo
          else
             if (ilevo.eq.ilev_call) then
                lsep = '('
                rsep = ')'
             else
                lsep = '<'
                rsep = '>'
             endif
             jinp = 1
             hbr = bufi(jinp)
             jbr = buf_h2item(hbr)
             obuffer(jbl)%desc2 = trim(istr) // lsep // trim(obuffer(jbr)%desc2)
             do jinp = 2, ninp
                hbr = bufi(jinp)
                jbr = buf_h2item(hbr)
                obuffer(jbl)%desc2 = trim(obuffer(jbl)%desc2) // ',' // trim(obuffer(jbr)%desc2)
             enddo
             obuffer(jbl)%desc2 = trim(obuffer(jbl)%desc2) // rsep
          endif
          obuffer(jbl)%ilev = ilevo
       endif
    endif
  end subroutine set_binary_descr

!!!_   . get_compromise_domain
  subroutine get_compromise_domain(ierr, domL, domR, bufh, lstk, nbuf, cmode, bufo)
    use TOUZA_Std,only: choice, find_first
    implicit none
    integer,       intent(out)            :: ierr
    type(domain_t),intent(inout)          :: domL
    type(domain_t),intent(inout)          :: domR(0:*)
    integer,       intent(in)             :: bufh(0:*)
    type(stack_t), intent(in)             :: lstk(0:*)   ! stack array only for logical coordinate
    integer,       intent(in)             :: nbuf
    integer,       intent(in)             :: cmode
    type(buffer_t),intent(inout),optional :: bufo
    integer j
    integer nceff

    ierr = 0

    if (ierr.eq.0) call tweak_coordinates(ierr, domL, domR, bufh, lstk, nbuf, bufo)
    if (ierr.eq.0) call set_inclusive_domain(ierr, domL, domR, bufh, lstk, nbuf)
    if (ierr.eq.0) then
       nceff = domL%mco
       select case(cmode)
       case (cmode_first)
          call set_inclusive_domain(ierr, domL, domR, bufh, lstk, 1, intersect=.TRUE.)
       case (cmode_intersect)
          call set_inclusive_domain(ierr, domL, domR, bufh, lstk, nbuf, intersect=.TRUE.)
       end select
    endif
    if (ierr.eq.0) call settle_output_domain(ierr, domL)

    do j = 0, nbuf - 1
       if (ierr.eq.0) call settle_input_domain(ierr, domR(j), bufh(j), lstk(j), domL)
    enddo
    if (present(bufo)) then
       if (ierr.eq.0) then
          call set_output_buffer(ierr, bufo, bufh(0:nbuf-1), domL)
       endif
    endif

    if (is_msglev_DEBUG(lev_verbose)) then
       if (ierr.eq.0) call show_domain(ierr, domL, 'compromise/L', indent=6)
       do j = 0, nbuf - 1
          if (ierr.eq.0) call show_domain(ierr, domR(j), 'compromise/R', indent=6)
       enddo
    endif
  end subroutine get_compromise_domain

!!!_   . tweak_coordinates
  subroutine tweak_coordinates &
       & (ierr, domL, domR, bufh, lstk, nbuf, bufo, clip)
    use TOUZA_Std,only: choice, find_first
    implicit none
    integer,       intent(out)            :: ierr
    type(domain_t),intent(inout)          :: domL
    type(domain_t),intent(inout)          :: domR(0:*)
    integer,       intent(in)             :: bufh(0:*)
    type(stack_t), intent(in)             :: lstk(0:*)
    integer,       intent(in)             :: nbuf
    type(buffer_t),intent(inout),optional :: bufo       ! to store coordinate names only
    logical,       intent(in),   optional :: clip       ! to clip empty coordinates (TRUE)

    integer nceff
    character(len=lname) :: nameL(0:lcoor-1),  nameR(0:lcoor-1, 0:nbuf-1)
    integer j, jb, jc, jo
    integer jerr

    integer nrphy(0:nbuf-1)
    integer clidx(0:lcoor-1, 0:nbuf-1)
    integer cpidx(0:lcoor-1, 0:nbuf-1)
    integer ctype(0:lcoor-1, 0:nbuf-1)

    ierr = 0
    do j = 0, nbuf - 1
       jb = buf_h2item(bufh(j))
       if (ierr.eq.0) then
          call get_logical_shape &
               & (ierr, nrphy(j), nameR(:,j), ctype(:,j), cpidx(:,j), lstk(j)%lcp, obuffer(jb)%pcp, lcoor)
          ! write(*, *) 'ranks', j, nrphy, obuffer(jb)%pcp(:)%cyc
       endif
    enddo
    if (ierr.eq.0) then
       ! if (choice(.TRUE., clip)) nrphy(0:nbuf-1) = -1
       call match_perms(ierr, nceff, nameL, clidx, nrphy, ctype, nameR, lcoor, nbuf)
    endif

    if (ierr.eq.0) then
       do j = 0, nbuf - 1
          domR(j)%mco = nceff
          domR(j)%cidx(0:nceff-1) = -1
          domR(j)%strd(0:nceff) = -1
          do jo = 0, nceff - 1
             jc = clidx(jo, j)
             domR(j)%lidx(jo) = jc
             if (jc.ge.0) then
                domR(j)%cidx(jo) = cpidx(jc, j)
             endif
          enddo
       enddo
       domL%mco = nceff
       domL%ofs(0:nceff-1) = null_range
       domL%cyc(0:nceff-1) = 0
       domL%cidx(0:nceff-1) = -1
       domL%strd(0:nceff) = -1
       do jc = 0, nceff - 1
          domL%cidx(jc) = jc
          ! if (ALL(domR(0:nbuf-1)%cidx(jc).lt.0)) then
          !    domL%cidx(jc) = co_wild
          ! else
          !    domL%cidx(jc) = jc
          ! endif
       enddo
       if (present(bufo)) then
          bufo%pcp(:)%name = ' '
          bufo%pcp(0:nceff-1)%name = nameL(0:nceff-1)
       endif
    endif

    if (ierr.eq.0) then
       if (is_msglev(lev_verbose, msglev_DETAIL-1)) then
          call show_stack_perms &
               & (jerr, domR, nameR, ctype, cpidx, bufh, lstk, lcoor, nbuf)
       endif
    endif

  end subroutine tweak_coordinates

!!!_   . set_inclusive_domain
  subroutine set_inclusive_domain &
       & (ierr, domL, domR, bufh, lstk, nbuf, intersect)
    use TOUZA_Std,only: choice, find_first
    implicit none
    integer,         intent(out)   :: ierr
    type(domain_t),  intent(inout) :: domL
    type(domain_t),  intent(in)    :: domR(0:*)
    integer,         intent(in)    :: bufh(0:*)
    type(stack_t),   intent(in)    :: lstk(0:*)  ! only for logical coordinates
    integer,         intent(in)    :: nbuf
    logical,optional,intent(in)    :: intersect

    integer nceff
    integer j
    integer jb,    jo
    ! integer jphyc, jlogc
    integer b,   e,    flg,   odmy, cdmy
    integer low, high, flgx
    integer,parameter :: lini = HUGE(0)
    integer,parameter :: hini = (- HUGE(0)) - 1
    logical isi

    ierr = 0
    isi = choice(.FALSE., intersect)

    if (ierr.eq.0) then
       nceff = domL%mco
       do jo = 0, nceff - 1
          if (isi) then
             low  = domL%bgn(jo)
             high = domL%end(jo)
          else
             low = lini
             high = hini
          endif
          flgx = -1
          do j = 0, nbuf - 1
             jb = buf_h2item(bufh(j))
             call get_logical_range &
                  & (b, e, flg, odmy, cdmy, jo, lstk(j)%lcp, obuffer(jb)%pcp, domR(j))
             flgx = max(flgx, flg)
             if (isi) then
                if (b.ne.null_range) low = max(low, b)
                if (e.ne.null_range) high = min(high, e)
             else
                if (b.ne.null_range) low = min(low, b)
                if (e.ne.null_range) high = max(high, e)
             endif
          enddo
          if (low.eq.lini) low = 0
          ! if (high.eq.hini) high = low + max(0, stp)
          if (high.eq.hini) then
             if (flgx.gt.loop_null) high = low + 1
          endif
          domL%bgn(jo) = low
          domL%end(jo) = high
       enddo
    endif
    return
  end subroutine set_inclusive_domain

!!!_   . settle_input_domain
  subroutine settle_input_domain(ierr, dom, hbuf, lstk, ref)
    implicit none
    integer,       intent(out)   :: ierr
    type(domain_t),intent(inout) :: dom
    integer,       intent(in)    :: hbuf
    type(stack_t), intent(in)    :: lstk ! only for logical coordinates
    type(domain_t),intent(in)    :: ref

    integer jo
    integer b, e, flg, osh, cyc
    integer jb

    ierr = 0
    jb = buf_h2item(hbuf)
    ierr = min(0, jb)
    if (ierr.eq.0) then
       call settle_input_domain_core(ierr, dom, obuffer(jb), lstk, ref)
    endif

    ! do jo = 0, ref%mco - 1
    !    ! call set_logical_range(b, e, jo, bstack(jstk)%lcp, dom, ref)
    !    call get_logical_range &
    !         & (b, e, flg, osh, cyc, jo, lstk%lcp, obuffer(jb)%pcp, dom, ref)
    !    dom%bgn(jo) = b
    !    dom%end(jo) = max(e, 1+b)
    !    dom%iter(jo) = max(1, e - b)
    !    dom%ofs(jo) = osh
    !    dom%cyc(jo) = cyc
    !    ! write(*, *) 'sib0:', jo, dom%bgn(jo), dom%end(jo), dom%iter(jo), dom%ofs(jo)
    ! enddo
    ! if (ierr.eq.0) then
    !    call settle_domain_stride(ierr, dom, obuffer(jb)%pcp)
    ! endif
    ! ! do jo = 0, ref%mco - 1
    ! !    write(*, *) 'sib1:', jo, dom%bgn(jo), dom%end(jo), dom%iter(jo), dom%ofs(jo)
    ! ! enddo
    ! if (ierr.eq.0) then
    !    call settle_domain_loop_h(ierr, dom, hbuf, ref)
    ! endif
    ! ! do jo = 0, ref%mco - 1
    ! !    write(*, *) 'sib9:', jo, dom%bgn(jo), dom%end(jo), dom%iter(jo), dom%ofs(jo)
    ! ! enddo

  end subroutine settle_input_domain

!!!_   . settle_input_domain_core
  subroutine settle_input_domain_core(ierr, dom, buf, lstk, ref)
    implicit none
    integer,       intent(out)   :: ierr
    type(domain_t),intent(inout) :: dom
    type(buffer_t),intent(inout) :: buf
    type(stack_t), intent(in)    :: lstk ! only for logical coordinates
    type(domain_t),intent(in)    :: ref

    integer jo
    integer b, e, flg, osh, cyc

    ierr = 0

    do jo = 0, ref%mco - 1
       call get_logical_range &
            & (b, e, flg, osh, cyc, jo, lstk%lcp, buf%pcp, dom, ref)
       dom%bgn(jo) = b
       dom%end(jo) = max(e, 1+b)
       dom%iter(jo) = max(1, e - b)
       dom%ofs(jo) = osh
       dom%cyc(jo) = cyc
    enddo
    if (ierr.eq.0) then
       call settle_domain_stride(ierr, dom, buf%pcp)
    endif
    if (ierr.eq.0) then
       call settle_domain_loop(ierr, dom, buf, ref)
    endif
  end subroutine settle_input_domain_core

!!!_   . settle_reduce_domain
  subroutine settle_reduce_domain(ierr, dom, buf, lstk, ref)
    implicit none
    integer,       intent(out)   :: ierr
    type(domain_t),intent(inout) :: dom
    type(buffer_t),intent(inout) :: buf
    type(stack_t), intent(in)    :: lstk ! only for logical coordinates
    type(domain_t),intent(in)    :: ref

    integer jo, jc
    integer nx

    ierr = 0
    nx = count(lstk%lcp(0:ref%mco-1)%flg.eq.loop_reduce)
    dom = ref
    do jo = 0, dom%mco - 1
       if (nx.eq.0.or.lstk%lcp(jo)%flg.eq.loop_reduce) then
          dom%strd(jo) = 0
          dom%bgn(jo) = max(0, max(ref%bgn(jo), dom%bgn(jo)) - ref%bgn(jo))
          dom%end(jo) = max(0, min(ref%end(jo), dom%end(jo)) - ref%bgn(jo))
          buf%pcp(jo)%flg = loop_reduce
       else
          buf%pcp(jo)%flg = loop_normal
       endif
       buf%pcp(jo)%bgn = ref%bgn(jo)
       buf%pcp(jo)%end = ref%end(jo)
       dom%ofs(jo) = 0
    enddo
    if (ierr.eq.0) then
       dom%n = 1
       do jo = 1, dom%mco
          jc = jo - 1
          if (dom%strd(jc).gt.0) then
             dom%strd(jc) = dom%n
             dom%n = dom%n * max(1, dom%end(jc) - dom%bgn(jc))
          endif
       enddo
    endif

  end subroutine settle_reduce_domain

!!!_   . settle_domain_loop_h
  subroutine settle_domain_loop_h(ierr, dom, handle, refd)
    implicit none
    integer,       intent(out)   :: ierr
    type(domain_t),intent(inout) :: dom
    integer,       intent(in)    :: handle
    type(domain_t),intent(in)    :: refd
    integer jb
    jb = buf_h2item(handle)
    ierr = min(0, jb)
    if (ierr.eq.0) call settle_domain_loop(ierr, dom, obuffer(jb), refd)
    return
  end subroutine settle_domain_loop_h
!!!_   . settle_domain_loop
  subroutine settle_domain_loop(ierr, dom, buf, refd)
    use TOUZA_std,only: choice
    implicit none
    integer,       intent(out)   :: ierr
    type(domain_t),intent(inout) :: dom
    type(buffer_t),intent(in)    :: buf
    type(domain_t),intent(in)    :: refd
    integer jc, kc

    ierr = 0
    if (ierr.eq.0) then
       do jc = 0, dom%mco - 1
          kc = dom%cidx(jc)
          if (kc.ge.0) then
             if (buf%pcp(kc)%flg.le.loop_null) then
                dom%bgn(jc) = max(0, max(refd%bgn(jc), dom%bgn(jc)) - refd%bgn(jc))
                dom%end(jc) = max(0, min(refd%end(jc), dom%end(jc)) - refd%bgn(jc))
             else
                dom%bgn(jc) = max(refd%bgn(jc), dom%bgn(jc), buf%pcp(kc)%bgn + dom%ofs(jc)) - refd%bgn(jc)
                dom%end(jc) = min(refd%end(jc), dom%end(jc), buf%pcp(kc)%end + dom%ofs(jc)) - refd%bgn(jc)
             endif
             if (buf%pcp(kc)%bgn.eq.null_range) then
                dom%ofs(jc) = 0
             else
                dom%ofs(jc) = - (buf%pcp(kc)%bgn + dom%ofs(jc) - refd%bgn(jc))
             endif
          else
             dom%bgn(jc) = max(0, max(refd%bgn(jc), dom%bgn(jc)) - refd%bgn(jc))
             dom%end(jc) = max(0, min(refd%end(jc), dom%end(jc)) - refd%bgn(jc))
             dom%ofs(jc) = 0
          endif
          ! write(*, *) 'sdl1:', jc, dom%bgn(jc), dom%end(jc), dom%iter(jc), dom%ofs(jc)
          dom%end(jc) = max(dom%bgn(jc), dom%end(jc))
          dom%iter(jc) = max(1, dom%end(jc) - dom%bgn(jc))
       enddo
    endif

  end subroutine settle_domain_loop

!!!_   . set_output_buffer_h - set result buffer as inclusive domain
  subroutine set_output_buffer_h &
       & (ierr, hbufo, hbufi, dom)
    use TOUZA_Nio,only: parse_header_size
    implicit none
    integer,       intent(out) :: ierr
    integer,       intent(in)  :: hbufo    ! output buffer handle
    integer,       intent(in)  :: hbufi(:) ! input buffer handles
    type(domain_t),intent(in)  :: dom

    integer jb

    ierr = 0
    jb = buf_h2item(hbufo)
    ierr = min(0, jb)
    if (ierr.eq.0) call set_output_buffer(ierr, obuffer(jb), hbufi, dom)
  end subroutine set_output_buffer_h
!!!_   . set_output_buffer - set result buffer as inclusive domain (core)
  subroutine set_output_buffer &
       & (ierr, buf, hbufi, dom)
    use TOUZA_std,only: choice
    use TOUZA_Nio,only: parse_header_size
    implicit none
    integer,       intent(out)   :: ierr
    type(buffer_t),intent(inout) :: buf
    integer,       intent(in)    :: hbufi(0:)
    type(domain_t),intent(in)    :: dom
    integer kv
    integer jb
    integer jinp, ninp
    integer jc
    integer reff

    ierr = 0
    ninp = size(hbufi)
    ! set output buffer
    if (ierr.eq.0) then
       ! kv = kv_flt
       kv = kv_int
       reff = -1
       do jinp = 0, ninp - 1
          jb = buf_h2item(hbufi(jinp))
          kv = max(kv, obuffer(jb)%k)
          if (obuffer(jb)%reff.ge.0) then
             if (reff.lt.0) reff = obuffer(jb)%reff
          endif
       enddo
       buf%k = kv
       buf%reff = reff
    endif
    if (ierr.eq.0) then
       do jc = 0, dom%mco - 1
          if (dom%strd(jc).le.0) then
             buf%pcp(jc)%bgn = dom%bgn(jc)
             ! buf%pcp(jc)%end = dom%bgn(jc)
             buf%pcp(jc)%end = dom%end(jc)
             buf%pcp(jc)%flg = loop_null
          else
             buf%pcp(jc)%bgn = dom%bgn(jc)
             buf%pcp(jc)%end = dom%end(jc)
             buf%pcp(jc)%flg = loop_normal
          endif
          buf%pcp(jc)%ofs = 0
          buf%pcp(jc)%cyc = 0
       enddo
       do jc = dom%mco, lcoor - 1
          buf%pcp(jc) = def_loop
       enddo
       call alloc_buffer_t(ierr, buf, dom%n)
    endif
  end subroutine set_output_buffer

!!!_   . reduce_output_buffer - set result buffer as reduction
  subroutine reduce_output_buffer &
       & (ierr, buf, lstk, hbufi, dom)
    use TOUZA_std,only: choice
    use TOUZA_Nio,only: parse_header_size
    implicit none
    integer,       intent(out)   :: ierr
    type(stack_t), intent(inout) :: lstk
    type(buffer_t),intent(inout) :: buf
    integer,       intent(in)    :: hbufi(0:)
    type(domain_t),intent(in)    :: dom
    integer kv
    integer jb
    integer jinp, ninp
    integer jc
    integer reff
    integer nx

    ierr = 0
    ninp = size(hbufi)
    ! set output buffer
    if (ierr.eq.0) then
       ! kv = kv_flt
       kv = kv_int
       reff = -1
       do jinp = 0, ninp - 1
          jb = buf_h2item(hbufi(jinp))
          kv = max(kv, obuffer(jb)%k)
          if (obuffer(jb)%reff.ge.0) then
             if (reff.lt.0) reff = obuffer(jb)%reff
          endif
       enddo
       buf%k = kv
       buf%reff = reff
    endif
    if (ierr.eq.0) then
       lstk%lcp(:) = def_loop
    endif
    if (ierr.eq.0) then
       ! nx = count(lstk%lcp(0:dom%mco-1)%flg.eq.loop_reduce)
       ! do jc = 0, dom%mco - 1
       !    if (lstk%lcp(jc)%flg.eq.loop_reduce.or.nx.eq.0) then
       !       ! buf%pcp(jc)%bgn = 0
       !       ! buf%pcp(jc)%end = 0
       !       buf%pcp(jc)%bgn = dom%bgn(jc)
       !       buf%pcp(jc)%end = dom%end(jc)
       !       buf%pcp(jc)%flg = loop_reduce
       !       ! lstk%lcp(jc)%bgn = null_range
       !       ! lstk%lcp(jc)%end = null_range
       !    else
       !       buf%pcp(jc)%bgn = dom%bgn(jc)
       !       buf%pcp(jc)%end = dom%end(jc)
       !       buf%pcp(jc)%flg = loop_normal
       !    endif
       !    buf%pcp(jc)%ofs = 0
       !    buf%pcp(jc)%cyc = 0
       ! enddo
       do jc = dom%mco, lcoor - 1
          buf%pcp(jc) = def_loop
       enddo
       call alloc_buffer_t(ierr, buf, dom%n)
    endif
  end subroutine reduce_output_buffer

!!!_   & is_undef()
!   elemental logical function is_undef(A, N) result(b)
!     implicit none
!     real(kind=KBUF),intent(in) :: A
!     real(kind=KBUF),intent(in) :: N
! #if HAVE_FORTRAN_IEEE_ARITHMETIC
!     b = ieee_is_nan(A)
! #else
!     b = A .eq. N
! #endif
!   end function is_undef

!!!_ + end chak
end program chak
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

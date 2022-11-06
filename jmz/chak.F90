!!!_! chak.F90 - TOUZA/Jmz swiss(CH) army knife
! Maintainer: SAITO Fuyuki
! Created: Nov 25 2021
#define TIME_STAMP 'Time-stamp: <2022/11/06 09:01:21 fuyuki chak.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022
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
#ifndef    OPT_CHAK_FILES
#  define  OPT_CHAK_FILES    128
#endif
#ifndef    OPT_CHAK_BUFFERS
#  define  OPT_CHAK_BUFFERS  256
#endif
#ifndef    OPT_CHAK_STACKS
#  define  OPT_CHAK_STACKS   512
#endif
#ifndef    OPT_CHAK_QUEUE
#  define  OPT_CHAK_QUEUE   1024
#endif
#ifndef    OPT_CHAK_PRECISION
#  define  OPT_CHAK_PRECISION  0
#endif
!!!_ + others
#ifndef   OPT_PATH_LEN
#  define OPT_PATH_LEN 1024
#endif
#ifndef   OPT_DESC_LEN
#  define OPT_DESC_LEN 1024
#endif
#if OPT_CHAK_PRECISION == 1
#  define __KBUF KFLT
#else
#  define __KBUF KDBL
#endif
! #ifndef   HAVE_FORTRAN_IEEE_ARITHMETIC
! #  define HAVE_FORTRAN_IEEE_ARITHMETIC 0
! #endif
#define ERR_EXHAUST 1   /* special recurn code at cueing */
!!!_@ TOUZA/Jmz/chak - nio swiss army knife
program chak
!!!_ + Declaration
!!!_  - modules
  use chak_lib,lib_init=>init
  use chak_opr,opr_init=>init
! #if HAVE_FORTRAN_IEEE_ARITHMETIC
!   use IEEE_ARITHMETIC
! #endif
  implicit none
!!!_  - parameters
  integer,parameter :: kv_null = 0
  integer,parameter :: kv_int  = 1
  integer,parameter :: kv_flt  = 2
  integer,parameter :: kv_dbl  = 3

  integer,parameter :: mcoor = 3    ! standard max coordinates

  integer,parameter :: ERR_NO_CANDIDATE = 1
  integer,parameter :: ERR_FINISHED = 2

  integer,parameter :: hedit_unset = -9
  integer,parameter :: hedit_sign = 0
  integer,parameter :: hedit_edit = 1
  integer,parameter :: hedit_title = 2
  integer,parameter :: hedit_item = 3
  integer,parameter :: hedit_all = 9

!!!_  - variables
  integer ierr
!!!_  - files
  integer,save      :: rcount = 0   ! read file count
  integer,save      :: wcount = 0   ! write file count
  integer,save      :: mfile=0
  integer,parameter :: lfile=OPT_CHAK_FILES
  integer,parameter :: lpath=OPT_PATH_LEN
  integer,parameter :: ldesc=OPT_DESC_LEN
  integer,parameter :: lfmt =litem*4

  integer,parameter :: hflag_unset = -1
  integer,parameter :: hflag_default = 0
  integer,parameter :: hflag_nulld   = 1  ! strict null-coordinate mode

  type file_t
     character(len=lpath) :: name
     integer              :: u
     integer              :: t
     integer              :: irec, nrec
     integer              :: mode      ! access mode
     integer              :: bh = -1
     character(len=litem) :: h(nitem)
     integer              :: jrf = -1  ! current record filter
     type(loop_t),pointer :: recf(:)   ! record filter
     character(len=lfmt)  :: fmt
     integer              :: kfmt
     integer              :: hedit     ! header edit level
     integer              :: hflag = hflag_unset ! header parser flag
  end type file_t
  type(file_t),target :: ofile(0:lfile-1)
  type(file_t),target :: def_read
  type(file_t),target :: def_write
  ! access mode
  integer,parameter :: mode_unset = -999
  integer,parameter :: mode_cycle = -3       ! rewind if eof
  integer,parameter :: mode_persistent = -2  ! keep final if eof
  integer,parameter :: mode_terminate = -1   ! terminate if eof
  integer,parameter :: mode_read = 0

  integer,parameter :: mode_new = 1          ! error if exist
  integer,parameter :: mode_write = 2        ! force overwrite
  integer,parameter :: mode_append = 3       ! append

  integer,parameter :: stt_none   = 0
  integer,parameter :: stt_free   = 1
  integer,parameter :: stt_locked = -1

  ! domain compromise mode for non-unary operations
  integer,parameter :: cmode_null      = 0
  integer,parameter :: cmode_inclusive = 1
  integer,parameter :: cmode_intersect = 2
  integer,parameter :: cmode_first     = 3

!!!_  - buffers
  integer,save      :: lcount = 0   ! literal count
  integer,save      :: consts=0     ! predefine constants
  integer,save      :: mbuffer=0
  integer,parameter :: lbuffer=OPT_CHAK_BUFFERS
  type buffer_t
     character(len=lname)    :: name              ! buffer name
     character(len=ldesc)    :: desc              ! description
     character(len=ldesc)    :: desc2             ! description (infix notation)
     integer                 :: ilev              ! infix notation level
     integer                 :: m = -1            ! m=0 as free
     integer                 :: k = kv_null
     integer                 :: stt
     integer                 :: ncoor             ! (reserved) number of coordinate
     integer                 :: ci(0:lcoor-1)
     integer                 :: reff              ! reference file id
     type(loop_t)            :: pcp(0:lcoor-1)    ! physical (source) coordinate properties
     real(kind=KBUF)         :: undef
     real(kind=KBUF),pointer :: vd(:)
  end type buffer_t
  type(buffer_t),target :: obuffer(0:lbuffer-1)
!!!_  - buffer stack
  type stack_t
     integer      :: bh
     type(loop_t) :: lcp(0:lcoor-1) ! logical (destination) coordinate properties
  end type stack_t

  integer           :: mstack
  integer,parameter :: lstack=OPT_CHAK_STACKS
  type(stack_t)     :: bstack(0:lstack-1)
!!!_  - argument queue
  type queue_t
     integer :: term = -1            ! term handle
     integer :: nopr                 ! number of operands
     integer :: iter                 ! number of iterates etc
     integer :: cmode = cmode_null   ! operation mode
     character(len=ldesc)  :: desci
     character(len=ldesc)  :: desco
     type(stack_t),pointer :: lefts(:)       ! result stack to push
  end type queue_t
  integer           :: mqueue
  integer,parameter :: lqueue=OPT_CHAK_QUEUE
  type(queue_t)     :: aqueue(0:lqueue-1)
!!!_  - handle offsets and kinds
  integer,parameter :: hk_error = -1
  integer,parameter :: hk_opr = 0
  integer,parameter :: hk_file = 1
  integer,parameter :: hk_buffer = 2
  integer,parameter :: hk_anchor = 3
  integer,parameter :: hk_overflow = 4

  integer,parameter :: lmodule = max(lopr, lfile, lbuffer) * 2
  integer,parameter :: ofs_opr    = lmodule * hk_opr
  integer,parameter :: ofs_file   = lmodule * hk_file
  integer,parameter :: ofs_buffer = lmodule * hk_buffer
  integer,parameter :: ofs_anchor = lmodule * hk_anchor

  integer,parameter :: max_operands = 2
!!!_  - global flags
  integer :: def_cmode = cmode_inclusive   ! default compromise mode

  character(len=lfmt),save :: afmt_int = '(I0)'
  character(len=lfmt),save :: afmt_flt = '(es16.12)'
  character(len=lfmt),save :: afmt_dbl = '(es16.12)'
!!!_  - misc
  integer irecw

  integer,parameter :: stat_help = 1
  integer           :: dryrun = 0
!!!_ + Body
  ierr = 0

  mqueue = 0

  if (ierr.eq.0) call init(ierr)
  if (ierr.eq.0) call parse_args(ierr)

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
     write(*, *) 'exit = ', ierr
     call exit(1)
  endif
#elif HAVE_FORTRAN_ERROR_STOP
  if (ierr.ne.0) then
     write(*, *) 'exit = ', ierr
     error stop 1
  endif
#else /* not HAVE_FORTRAN_ERROR_STOP */
  if (ierr.ne.0) then
     write(*, *) 'exit = ', ierr
  endif
#endif /* not HAVE_FORTRAN_ERROR_STOP */
  stop
!!!_ + Subroutines
contains
!!!_  - commons
!!!_   . init
  subroutine init(ierr)
    use TOUZA_Nio,only: nio_init=>init, nr_init
    implicit none
    integer,intent(out) :: ierr

    ierr = 0
    if (ierr.eq.0) call lib_init(ierr)
    if (ierr.eq.0) call opr_init(ierr)
    if (ierr.eq.0) call register_predefined(ierr)
    if (ierr.eq.0) call init_sub(ierr)
    if (ierr.eq.0) call nio_init(ierr, levv=dbgv, stdv=stdv)
    if (ierr.eq.0) call nr_init(ierr, lazy=+1)

    if (ierr.eq.0) call reset_file(ierr, def_read,  ' ', mode_terminate, hflag_default)
    if (ierr.eq.0) call reset_file(ierr, def_write, ' ', mode_new,       hflag_default)
! #if HAVE_FORTRAN_IEEE_ARITHMETIC
!     UNDEF = IEEE_VALUE(ZERO, IEEE_QUIET_NAN)
! #endif
  end subroutine init
!!!_    * init_sub
  subroutine init_sub(ierr)
    ! use TOUZA_Std,only: ndigits
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
    use TOUZA_Std,only: env_finalize, htb_finalize, htb_diag
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
    if (ierr.eq.0) call nio_diag(ierr, levv=dbgv)
    if (ierr.eq.0) call htb_diag(ierr, levv=dbgv)
    if (ierr.eq.0) call nio_finalize(ierr, levv=dbgv)
    if (ierr.eq.0) call htb_finalize(ierr, levv=dbgv)
    if (ierr.eq.0) call env_finalize(ierr, levv=dbgv)
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

101 format('chak - Swiss(CH) Army Knife for gtool-3.5 format files')
102 format(2x, 'with ', A, 1x, A, '; ', A, 1x, A)
    write(utmp, 101)
    write(utmp, 102) PACKAGE_NAME, PACKAGE_VERSION, &
         & TOUZA_NAME, TOUZA_VERSION
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
          select case(aqueue(j)%cmode)
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
                   write(str, 102) trim(buf), aqueue(j)%iter, aqueue(j)%nopr, size(aqueue(j)%lefts), cmd, trim(aqueue(j)%desco)
                else
                   write(str, 101) trim(buf),                 aqueue(j)%nopr, size(aqueue(j)%lefts), cmd, trim(aqueue(j)%desco)
                endif
             endif
          case(hk_file)
             write(str, 111) trim(aqueue(j)%desco)
          case default
             write(str, 121) trim(aqueue(j)%desco)
          end select
       endif
201    format('queue[', I0, '] ', I0, 1x, A)
       if (ierr.eq.0) then
          js = js - aqueue(j)%nopr + size(aqueue(j)%lefts)
          write(utmp, 201) user_index_bgn(j), js, trim(str)
       endif
    enddo
  end subroutine show_queue

!!!_   . show_files
  subroutine show_files(ierr, u)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer utmp
    integer j, h, jb
    character(len=1) :: cm
    character(len=128) :: bname

    ierr = 0
    utmp = choice(ulog, u)

    do j = 0, min(mfile, lfile) - 1
201    format('file:', A, 1x, I0, 1x, A1, 1x, I0, '/', I0, 1x, A)
       h = file_i2handle(j)
       select case(ofile(j)%mode)
       case (mode_read)
          cm = 'r'
       case (mode_cycle)
          cm = 'c'
       case (mode_persistent)
          cm = 'p'
       case (mode_terminate)
          cm = 'l'
       case (mode_new)
          cm = 'n'
       case (mode_write)
          cm = 'f'
       case (mode_append)
          cm = 'a'
       case default
          cm = 'e'
       end select
       jb = buf_h2item(ofile(j)%bh)
       bname = obuffer(jb)%name
       write(utmp, 201) trim(bname), ofile(j)%u, cm, ofile(j)%irec, ofile(j)%nrec, trim(ofile(j)%name)
    enddo
  end subroutine show_files

!!!_   . show_buffers
  subroutine show_buffers(ierr, u, levv)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer utmp, lv
    integer j, h
    character(len=128) :: bname
    character(len=128) :: txt
    character(len=1) :: cs
    character(len=1),parameter :: CSTT(stt_locked:stt_free) = (/'L', 'N', 'F'/)
    integer jbgn

    ierr = 0
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (is_msglev_DEBUG(lv)) then
       jbgn = 0
    else
       jbgn = consts
    endif

    do j = jbgn, min(mbuffer, lbuffer) - 1
101    format('I:', I0)
102    format('F:', I0)
103    format('D:', I0)
109    format('X/', I0)
       select case (obuffer(j)%k)
       case (kv_int)
          write(txt, 101) obuffer(j)%m
       case (kv_flt)
          write(txt, 102) obuffer(j)%m
       case (kv_dbl)
          write(txt, 103) obuffer(j)%m
       case default
          write(txt, 109) obuffer(j)%k
       end select
       cs = cstt(min(stt_free, max(stt_locked, obuffer(j)%stt)))

201    format(A, 1x, A, 1x, A, 1x, E10.3)
211    format('buffer:', I0)
212    format('buffer:', A)
       h = buf_i2handle(j)
       if (obuffer(j)%name.eq.' ') then
          write(bname, 211) h
       else
          write(bname, 212) trim(obuffer(j)%name)
       endif
       write(utmp, 201) trim(bname), cs, trim(txt), obuffer(j)%undef
    enddo
  end subroutine show_buffers

!!!_   . show_stack
  subroutine show_stack(ierr, u, levv)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer utmp
    integer js, jb, m
    character(len=128) :: pfx
    character(len=lpath) :: str
    character(len=lpath) :: desc
    character(len=128)   :: domain
    integer lv
    integer alev
    integer jas

    ierr = 0
    lv = choice(lev_verbose, levv)
    utmp = choice(ulog, u)
201 format('stack[', I0, ']')
    jas = user_index_bgn(0)
    do js = 0, min(mstack, lstack) - 1
       if (ierr.eq.0) call get_obj_string(ierr, str, bstack(js)%bh, lv)
       alev = anchor_h2level(bstack(js)%bh)
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
          jb = buf_h2item(bstack(js)%bh)
          if (jb.ge.0) then
             m = obuffer(jb)%m
          else
             m = -1
          endif
          if (obuffer(jb)%desc.ne.' ') then
202          format(1x, '<', A, '>')
             write(desc, 202) trim(obuffer(jb)%desc)
             str = trim(str) // trim(desc)
          endif
          if (is_msglev_DETAIL(lv)) then
             call get_domain_string(ierr, domain, bstack(js)%lcp)
             str = trim(str) // ' ' // trim(domain)
          endif
          write(pfx, 201) jas
          call message(ierr, trim(pfx) // ' ' // trim(str), u=utmp, indent=4)
          jas = jas + 1
       endif
    enddo
  end subroutine show_stack

!!!_   . show_domain
  subroutine show_domain &
       & (ierr, dom, tag, u, levv)
    use TOUZA_Std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    type(domain_t),  intent(in)          :: dom
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer utmp
    integer lv
    character(len=64) :: pfx, cran
    integer jc
    ierr = 0
    lv = choice(lev_verbose, levv)
    utmp = choice(ulog, u)
    if (present(tag)) then
       pfx = '[' // trim(tag) // ']'
    else
       pfx = ' '
    endif
102 format('domain', A, ' total = ', I0)
103 format('domain', A, ': ', I0, ' / = ', 6(1x, I0))
    write(utmp, 102) trim(pfx), dom%n
    write(utmp, 103) trim(pfx), dom%mco, dom%cidx(0:dom%mco - 1)
    do jc = 0, dom%mco - 1
111    format('domain', A, ': ', I0, 1x, A, ' +', I0, '+', I0, ' (', I0, ')')
112    format('domain', A, ': ', I0, 1x, A, ' +', I0, '+', I0)
       call get_range_string(ierr, cran, dom%bgn(jc), dom%end(jc), 1)
       if (dom%ofs(jc).eq.null_range) then
          write(utmp, 112) trim(pfx), jc, trim(cran), dom%iter(jc), dom%strd(jc)
       else
          write(utmp, 111) trim(pfx), jc, trim(cran), dom%iter(jc), dom%strd(jc), dom%ofs(jc)
       endif
    enddo
  end subroutine show_domain

!!!_   . show_stack_perms
  subroutine show_stack_perms &
       & (ierr, dom, cname, ctype, cpidx, bufh, pstk, mco, nbuf, u, levv)
    use TOUZA_Std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: mco, nbuf
    type(domain_t),  intent(in)          :: dom(0:*)
    character(len=*),intent(in)          :: cname(0:mco-1, 0:*)
    integer,         intent(in)          :: ctype(0:mco-1, 0:*)
    integer,         intent(in)          :: cpidx(0:mco-1, 0:*)
    integer,         intent(in)          :: bufh(0:*)
    integer,         intent(in)          :: pstk(0:*)
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer utmp, lv
    integer j
    integer hb, jb, js
    character(len=128) :: pstr, dstr

    ierr = 0
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    do j = 0, nbuf - 1
       hb = bufh(j)
       jb = buf_h2item(hb)
       js = pstk(j)
       if (ierr.eq.0) then
          call get_perm_string &
               & (ierr, pstr, cname(:,j), ctype(:,j), cpidx(:,j), obuffer(jb)%pcp, bstack(js)%lcp, mco)
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
    use TOUZA_Std,only: arg_init, arg_diag, parse, get_param
    implicit none
    integer,intent(out) :: ierr

    integer japos
    character(len=lpath) :: arg
    integer jerr
    integer stat

    ierr = 0

    mqueue = 0
    mfile = 0

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
    if (ierr.eq.0) call set_rec_filter(ierr)
    if (ierr.eq.0) call set_write_format(ierr)
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
    integer jval, jvar

    integer md, cmd, hmd
    integer ntmp
    integer hflag, hsub

    md = mode_unset
    cmd = mode_unset
    hmd = hedit_unset
    ierr = 0
    abuf = arg
    stat = 0
    hflag = hflag_unset
    hsub = 0

    jvar = index(abuf, param_sep)
    if (jvar.eq.1) then
       ierr = ERR_INVALID_PARAMETER
       call message(ierr, 'invalid option ' // trim(abuf), (/japos/))
       return
    else if (jvar.gt.0) then
       jval = jvar + 1
       jvar = jvar - 1
    else
       if (index('-+', abuf(1:1)).eq.0) then
          ierr = ERR_NO_CANDIDATE   ! not error
          return
       endif
       if      (abuf(1:2).eq.'-v') then
          n = verify(trim(abuf), 'v', .TRUE.)
          if (n.ne.1) then
             ierr = ERR_INVALID_ITEM
          else
             lev_verbose = + (len_trim(abuf) - 1)
          endif
       else if (abuf.eq.'+v') then
          lev_verbose = +999
       else if (abuf(1:2).eq.'-q') then
          n = verify(trim(abuf), 'q', .TRUE.)
          if (n.ne.1) then
             ierr = ERR_INVALID_ITEM
          else
             lev_verbose = - (len_trim(abuf) - 1)
          endif
       else if (abuf.eq.'+q') then
          lev_verbose = -999
       else if (abuf(1:2).eq.'-d') then
          n = verify(trim(abuf), 'd', .TRUE.)
          if (n.ne.1) then
             ierr = ERR_INVALID_ITEM
          else
             dbgv = + (len_trim(abuf) - 1)
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
       else if (cmd.ne.mode_unset) then
          if (ierr.eq.0) call parse_operator_option(ierr, cmd)
       endif
    endif
  end subroutine parse_option
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

    type(file_t),pointer :: pfile
    character(len=128) :: msg

    ierr = 0
    if (mfile.gt.lfile) then
       ierr = ERR_INSUFFICIENT_BUFFER
       return
    endif

    pfile => ofile(mfile-1)

    if (is_read_mode(mode)) then
       if (mfile.eq.0) pfile => def_read
       if (is_read_mode(pfile%mode)) then
          pfile%mode = mode
       else
          ierr = ERR_INVALID_SWITCH
       endif
    else
       if (mfile.eq.0) pfile => def_write
       if (.not.is_read_mode(pfile%mode)) then
          pfile%mode = mode
       else
          ierr = ERR_INVALID_SWITCH
       endif
    endif
    if (ierr.ne.0) then
101    format('cannot set file option for file ', I0)
       write(msg, 101) mfile
       call message(ierr, msg)
    endif
  end subroutine parse_file_option

!!!_   . parse_hedit_option
  subroutine parse_hedit_option(ierr, mode)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: mode

    type(file_t),pointer :: pfile
    ! character(len=128) :: msg

    ierr = 0
    if (mfile.gt.lfile) then
       ierr = ERR_INSUFFICIENT_BUFFER
       return
    endif
    pfile => ofile(mfile-1)
    if (mfile.eq.0) pfile => def_write
    if (.not.is_read_mode(pfile%mode)) then
       pfile%hedit = mode
    endif
  end subroutine parse_hedit_option

!!!_   . parse_hflag_option
  subroutine parse_hflag_option(ierr, flag, sub)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: flag
    integer,intent(in)  :: sub

    type(file_t),pointer :: pfile

    ierr = 0
    if (mfile.gt.lfile) then
       ierr = ERR_INSUFFICIENT_BUFFER
       return
    endif
    pfile => ofile(mfile-1)
    if (mfile.eq.0) then
       if (sub.gt.0) then
          pfile => def_write
       else
          pfile => def_read
       endif
    endif
    pfile%hflag = flag
  end subroutine parse_hflag_option

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
    character(len=lname) :: bname
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
          ofile(jf)%mode  = def_write%mode
          ofile(jf)%hedit = def_write%hedit
          ofile(jf)%hflag = def_write%hflag
          call append_queue(ierr, hfile, pop=1, push=0)
          if (ierr.eq.0) call pop_stack(ierr, hbuf)
          if (ierr.eq.0) then
             jb = buf_h2item(hbuf)
             obuffer(jb)%stt = stt_locked
             ofile(jf)%bh = hbuf
1011         format('W', I0)
             write(bname, 1011) user_index_bgn(wcount)
             obuffer(jb)%name = bname
             wcount = wcount + 1
          endif
          if (ierr.eq.0) call reg_fake_opr(ierr, hbuf, bname)
          ! if (ierr.eq.0) call register_obj(ierr, hbuf, bname)
       else
          ! file to read
          ofile(jf)%mode = mode_read
1001      format('F', I0)
          write(bname, 1001) user_index_bgn(rcount)
          rcount = rcount + 1
          call new_buffer(ierr, hbuf, bname)
          if (ierr.eq.0) call push_stack(ierr, hbuf)
          if (ierr.eq.0) then
             ofile(jf)%bh = hbuf
             jb = buf_h2item(hbuf)
             obuffer(jb)%stt = stt_locked
             call append_queue(ierr, hfile, 0, 1, (/hbuf/))
          endif
          if (ierr.eq.0) call reg_fake_opr(ierr, hbuf, bname)
       endif
    endif
  end subroutine parse_arg_file

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
    else if (hopr.eq.opr_FLUSH) then
       pop = 0
       npop = mstack
       if (ierr.eq.0) call stack_POP(ierr, pop, npop, iter, opr_FLUSH)
!!!_    * buffer property operator
    else if (grp_buffer_bgn.le.hopr .and. hopr.lt.grp_buffer_end) then
       call parse_buffer_opr(ierr, hopr, arg)
    else if (grp_header_bgn.le.hopr .and. hopr.lt.grp_header_end) then
       call parse_header_opr(ierr, hopr, arg)
!!!_    * normal operators
    else if (hopr.ge.0) then
       if (is_operator(hopr)) then
          if (ierr.eq.0) call stack_normal_opr(ierr, 0, 0, hopr, 0)
       else
          ! fake operator
          if (ierr.eq.0) call append_queue(ierr, hopr, 0, 1, (/hopr/))
          if (ierr.eq.0) call push_stack(ierr, hopr)
       endif
    else
       ierr = ERR_NO_CANDIDATE   ! not error
    endif
  end subroutine parse_arg_operator

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
             call reg_fake_opr(ierr, hbuf, obuffer(jb)%name)
          endif
       case(opr_PERM)
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
          call message(ierr, 'reserved operator ' // trim(arg))
       end select
    endif
    return
  end subroutine parse_buffer_opr

!!!_   . parse_buffer_shape
  subroutine parse_buffer_shape (ierr, arg, hopr)
    use TOUZA_Std,only: split_list
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: arg
    integer,         intent(in)  :: hopr

    integer lasth, pop, push

    integer jc
    integer jpb, jpe, larg
    character,parameter :: csep=','       ! coordinate separaor

    integer jq
    integer jsbgn, jsend
    integer b, e, s, jrep

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
          call decompose_coordinate_mod(ierr, jrep, b, e, s, arg(jpb+1:jpe-1))
          if (ierr.eq.0) then
             aqueue(jq)%lefts(:)%lcp(jc)%name = adjustl(arg(jpb+1:jpb+jrep))
             aqueue(jq)%lefts(:)%lcp(jc)%bgn  = b
             aqueue(jq)%lefts(:)%lcp(jc)%end  = e
             aqueue(jq)%lefts(:)%lcp(jc)%stp  = s
             bstack(jsbgn:jsend-1)%lcp(jc)%name = adjustl(arg(jpb+1:jpb+jrep))
             bstack(jsbgn:jsend-1)%lcp(jc)%bgn = b
             bstack(jsbgn:jsend-1)%lcp(jc)%end = e
             bstack(jsbgn:jsend-1)%lcp(jc)%stp = s
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
    use TOUZA_Std,only: split_list
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: cidx
    character(len=*),intent(in)  :: arg
    integer,         intent(in)  :: hopr
    integer lasth
    integer jq
    integer pop, push
    integer b, e, s, jrep

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
       call decompose_coordinate_mod(ierr, jrep, b, e, s, arg)
       ! write(*, *) ierr, b, e, s, arg(1:jrep)
    endif
    if (ierr.eq.0) then
       jq = mqueue - 1
       aqueue(jq)%lefts(:)%lcp(cidx)%name = arg(1:jrep)
       aqueue(jq)%lefts(:)%lcp(cidx)%bgn = b
       aqueue(jq)%lefts(:)%lcp(cidx)%end = e
       aqueue(jq)%lefts(:)%lcp(cidx)%stp = s

       bstack(mstack-pop:mstack-1)%lcp(cidx)%name = arg(1:jrep)
       bstack(mstack-pop:mstack-1)%lcp(cidx)%bgn = b
       bstack(mstack-pop:mstack-1)%lcp(cidx)%end = e
       bstack(mstack-pop:mstack-1)%lcp(cidx)%stp = s
    endif
  end subroutine parse_coordinate_opr

!!!_   . decompose_coordinate_mod
  subroutine decompose_coordinate_mod &
       & (ierr, jrep, b, e, s, arg)
    use TOUZA_Std,only: split_list
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: jrep     ! name/rename as (1:jrep)
    integer,         intent(out) :: b, e, s
    character(len=*),intent(in)  :: arg
    integer larg, lsep
    integer js0, js1
    integer rpos(2)
    integer,parameter :: rdef(2) = (/null_range, null_range/)
    integer nc
    integer jran

    ! NAME/REPL//RANGE   == NAME/REPL/  RANGE      / before RANGE is absorbed
    ! NAME/REPL/RANGE    == NAME/REPL   RANGE
    ! NAME//RANGE        == NAME/       RANGE
    ! NAME/RANGE         == NAME        RANGE
    ! NAME/REPL/                                   no / absorption
    ! NAME/REPL
    !  NAME REPL  alpha+alnum
    !  RANGE      [num][:[num]]

    ierr = 0
    larg = len_trim(arg)
    lsep = len(rename_sep)
    js0 = index(arg, rename_sep)
    jran = -1
    if (js0.gt.0) then
       js0 = js0 + lsep
       js1 = index(arg(js0:), rename_sep)
       if (js1.gt.0) then
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
    else
       js0 = 1
    endif
    if (jran.lt.0) then
       if (index(('0123456789' // range_sep), arg(js0:js0)).eq.0) then
          jran = larg + 1
          jrep = larg
       else
          jran = js0
          jrep = jran - lsep - lsep
       endif
    endif

    rpos(1) = system_index_bgn(null_range)
    rpos(2) = system_index_end(null_range)
    s = -1
    call split_list(nc, rpos, arg(jran:larg), range_sep, 2, rdef(:))
    if (nc.lt.0) then
       ierr = nc
       call message(ierr, 'cannot parse range: ' // trim(arg(jran:larg)))
    else if (nc.eq.0) then
       b = system_index_bgn(rpos(1))
       e = system_index_end(rpos(2))
       continue
    else if (nc.eq.1) then
       b = system_index_bgn(rpos(1))
       e = b + 1
       s = 1
    else if (nc.eq.2) then
       b = system_index_bgn(rpos(1))
       e = system_index_end(rpos(2))
       s = 1
       if (e.le.b) s = 0
    else if (nc.gt.2) then
       ierr = ERR_INVALID_PARAMETER
       call message(ierr, 'fail to extract range ' // trim(arg(jran:larg)))
    endif
    ! write(*, *) jrep, jran, '{' // arg(1:jrep) // '}{' // arg(jran:larg) // '} ', b, e, s
  end subroutine decompose_coordinate_mod

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
    integer lasth, jf
    integer jerr
    integer jitem
    real(kind=KBUF) :: undef

    ierr = 0
    call last_queue(jerr, lasth)
    if (jerr.eq.0) then
       jf = file_h2item(lasth)
    else
       jf = -1
    endif

    if (ierr.eq.0) then
       jpar = index(arg, param_sep) + 1
       jend = len_trim(arg) + 1
       if (jpar.eq.1) jpar = jend
       select case(hopr)
       case(opr_FMT)
          ! if (jf.ge.0) then
          !    if (is_read_mode(ofile(jf)%mode)) jf = -1
          ! endif
          if (mfile.eq.0) then
             call set_file_format(ierr, def_write, arg(jpar:))
             ! def_write%fmt = trim(arg(jpar:))
          else if (jf.ge.0) then
             call set_file_format(ierr, ofile(jf), arg(jpar:))
             ! ofile(jf)%fmt = trim(arg(jpar:))
          else
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'no file to set format ' // trim(arg))
          endif
       case(opr_TSEL)
          if (mfile.eq.0) then
             call parse_rec_filter(ierr, def_read, arg(jpar:))
          else if (is_read_buffer(lasth)) then
             call parse_rec_filter(ierr, ofile(jf), arg(jpar:))
          else
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'no file to set record filter' // trim(arg))
          endif
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
          if (mfile.eq.0) then
             call put_item(ierr, def_write%h, arg(jpar:), jitem, 0)
          else if (jf.ge.0) then
             call put_item(ierr, ofile(jf)%h, arg(jpar:), jitem, 0)
          else
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'no file to set header ' // trim(arg))
          endif
       case(opr_MISS)
          if (jf.ge.0) then
             call parse_number(jerr, undef, trim(arg(jpar:)))
             if (jerr.eq.0) then
                call put_item(ierr, ofile(jf)%h, undef, hi_MISS)
             else
                call store_item(ierr, ofile(jf)%h, arg(jpar:), hi_MISS)
             endif
          else
             call parse_buffer_opr(ierr, hopr, arg)
          endif
          ! if (mfile.eq.0) then
          !    if (jerr.eq.0) then
          !       call put_item(ierr, def_write%h, undef, hi_MISS)
          !    else
          !       call store_item(ierr, def_write%h, arg(jpar:), hi_MISS)
          !    endif
          ! else
          ! else
          !    ierr = ERR_INVALID_ITEM
          !    call message(ierr, 'no file to set header ' // trim(arg))
          ! endif
       case default
          ierr = ERR_NOT_IMPLEMENTED
          call message(ierr, 'reserved header operator ' // trim(arg))
       end select
    endif
    return
  end subroutine parse_header_opr

!!!_   . set_file_format
  subroutine set_file_format &
       & (ierr, file, arg)
    use TOUZA_Std,only: upcase
    use TOUZA_Nio,only: parse_record_fmt
    implicit none
    integer,         intent(out)   :: ierr
    type(file_t),    intent(inout) :: file
    character(len=*),intent(in)    :: arg
    character(len=litem*4) :: abuf

    character(len=*),parameter :: asep = ','
    character(len=*),parameter :: bsep = ':'
    integer jp

    ierr = 0

    abuf = ' '
    call upcase(abuf, arg)
    call parse_record_fmt(ierr, file%kfmt, abuf)
    if (ierr.eq.0) then
       file%fmt = trim(abuf)
    else
       ierr = 0
       select case (abuf(1:1))
       case ('A')
          jp = index(abuf, asep)
          if (jp.eq.0) then
             file%fmt = trim(abuf(2:))
          else
             file%fmt = trim(abuf(jp+1:))
          endif
          file%kfmt = cfmt_ascii
       case ('B')
          jp = index(abuf, asep)
          if (jp.eq.0) then
             file%fmt = ' '
          else
             file%fmt = trim(abuf(jp+1:))
             ! clear shape
             abuf = abuf(1:jp-1)
          endif
          select case(abuf(2:3))
          case ('I4')
             file%kfmt = cfmt_binary_i4
          case ('R4')
             file%kfmt = cfmt_binary_r4
          case ('R8')
             file%kfmt = cfmt_binary_r8
          case default
             ierr = ERR_INVALID_PARAMETER
          end select
          if (ierr.eq.0) then
             jp = 4
             if (abuf(jp:jp).eq.bsep) jp = jp + 1
             select case(abuf(jp:jp))
             case('N')
                file%kfmt = file%kfmt + cfmt_flag_native
             case('S')
                file%kfmt = file%kfmt + cfmt_flag_swap
             case('B')
                file%kfmt = file%kfmt + cfmt_flag_big
             case('L')
                file%kfmt = file%kfmt + cfmt_flag_little
             case(' ')
                continue
             case default
                ierr = ERR_INVALID_PARAMETER
             end select
          endif
          if (ierr.ne.0) then
             call message(ierr, 'unknown format ' // trim(arg))
          endif
       case default
          ierr = ERR_INVALID_PARAMETER
          call message(ierr, 'unknown format ' // trim(arg))
       end select
    endif
    return

  end subroutine set_file_format

!!!_   . parse_rec_filter
  subroutine parse_rec_filter &
       & (ierr, file, arg)
    use TOUZA_Std,only: split_list
    implicit none
    integer,         intent(out)   :: ierr
    type(file_t),    intent(inout) :: file
    character(len=*),intent(in)    :: arg

    character,parameter :: lsep=','
    character,parameter :: rsep=':'
    integer jp, la, jc, je, nc
    integer jf, nf
    integer rpos(3)

    ierr = 0
    la = len_trim(arg)
    if (la.eq.0) then
       continue
    else
       nf = 0
       jp = 0
       do
          jc = index(arg(jp+1:la), lsep)
          ! write(*, *) jp, jc, nf, arg(jp+1:la)
          if (jc.eq.0) then
             if (jp.lt.la) nf = nf + 1
             exit
          endif
          if (jc.gt.1) nf = nf + 1
          jp = jp + jc + len(lsep) - 1
       enddo
       ! write(*,*) arg(1:la), nf
       if (nf.gt.0) then
          allocate(file%recf(0:nf-1), STAT=ierr)
          if (ierr.eq.0) then
             file%jrf = 0
             jp = 0
             do jf = 0, nf - 1
                do
                   je = index(arg(jp+1:la), lsep)
                   if (je.eq.1) then
                      jp = jp + len(lsep)
                      cycle
                   endif
                   exit
                enddo
                if (je.eq.0) then
                   je = la + 1
                else
                   je = jp + je
                endif
                !! for later index adjustment, null_range/full_range mixture used here
                !!   'low:' is stored as low,full_range
                !!   'low'  is stored as low,null_range
                call split_list(nc, rpos, arg(jp+1:je-1), rsep, 3, (/null_range, full_range, 1/))
                if (nc.le.0) then
                   ierr = ERR_INVALID_PARAMETER
                   call message(ierr, 'bad range ' // arg(jp+1:je-1))
                   exit
                else
                   file%recf(jf)%bgn = rpos(1)
                   if (nc.gt.1) then
                      file%recf(jf)%end = rpos(2)
                   else
                      file%recf(jf)%end = null_range
                   endif
                   if (nc.gt.2) then
                      file%recf(jf)%stp = rpos(3)
                   else
                      file%recf(jf)%stp = 1
                   endif
                endif
                jp = je
             enddo
          endif
       endif
    endif
  end subroutine parse_rec_filter

!!!_   . set_rec_filter
  subroutine set_rec_filter(ierr)
    implicit none
    integer,intent(out) :: ierr
    integer jfile
    ! integer jr
    ierr = 0
    if (def_read%jrf.lt.0) then
       if (ierr.eq.0) allocate(def_read%recf(0:0), STAT=ierr)
       if (ierr.eq.0) then
          def_read%jrf = 0
          def_read%recf(0)%bgn=0
          def_read%recf(0)%end=-1
          def_read%recf(0)%stp=1
       endif
    else
       call adjust_rec_filter(def_read)
    endif
    do jfile = 0, min(mfile, lfile) - 1
       if (is_read_mode(ofile(jfile)%mode)) then
          if (ofile(jfile)%jrf.lt.0) then
             ofile(jfile)%jrf = 0
             ofile(jfile)%recf => def_read%recf
          else
             call adjust_rec_filter(ofile(jfile))
          endif
       endif
    enddo

  end subroutine set_rec_filter

!!!_    * adjust_rec_filter
  subroutine adjust_rec_filter(file)
    implicit none
    type(file_t),intent(inout) :: file
    integer jr

    type(loop_t),pointer :: recf(:)

    integer nspec

    recf => file%recf

    nspec = 0
    do jr = lbound(recf,1), ubound(recf,1)
       if (recf(jr)%bgn.eq.null_range) then
          recf(jr)%bgn = 0
       else
          recf(jr)%bgn = system_index_bgn(recf(jr)%bgn)
       endif
       if (recf(jr)%end.eq.null_range) then
          recf(jr)%end = recf(jr)%bgn + 1
       else if (recf(jr)%end.eq.full_range) then
          recf(jr)%end = -1
       else
          recf(jr)%end = system_index_end(recf(jr)%end)
       endif
       if (recf(jr)%stp.eq.0) then
          if (recf(jr)%end.lt.0.or.recf(jr)%bgn.lt.recf(jr)%end) then
             recf(jr)%stp = +1
          else
             recf(jr)%stp = -1
          endif
       endif
       if (recf(jr)%end.lt.0) then
          nspec = -1
       else if (nspec.ge.0) then
          nspec = nspec + (recf(jr)%end - recf(jr)%bgn) / recf(jr)%stp
       endif
    enddo
    if (nspec.eq.1.and.file%mode.eq.mode_read) file%mode = mode_persistent
  end subroutine adjust_rec_filter

!!!_   . set_write_format
  subroutine set_write_format(ierr)
    implicit none
    integer,intent(out) :: ierr
    integer jfile

    ierr = 0
    if (ierr.eq.0) then
       do jfile = 0, min(mfile, lfile) - 1
          if (.not.is_read_mode(ofile(jfile)%mode)) then
             if (ofile(jfile)%fmt.eq.' ') ofile(jfile)%fmt = def_write%fmt
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
          call message(ierr, 'reserved operator ' // trim(opr))
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
       ! else if (lasth.eq.opr_POP) then
       !    call stack_POP(ierr, pop, npop, iter+1)
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
       if (present(push).NEQV.present(bufh)) then
          ierr = ERR_INVALID_ITEM
       else if (present(push)) then
          deallocate(aqueue(jq)%lefts, STAT=ierr)
          if (ierr.eq.0) allocate(aqueue(jq)%lefts(0:push-1), STAT=ierr)
          if (ierr.eq.0) aqueue(jq)%lefts(0:push-1)%bh = bufh(0:push-1)
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
       call trace_file_access(ierr, handle, lv, utmp)
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
  subroutine trace_file_access (ierr, fileh, levv, u)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: fileh
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: u
    integer lv, utmp
    character(len=lpath) :: str
    integer jfile
    character(len=16) :: acc
    character(len=lname) :: bname
    character(len=128)   :: domain
    type(loop_t) :: lpp(0:lcoor-1)

    ierr = 0
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)
    jfile = file_h2item(fileh)
    ierr = min(0, jfile)
    if (ierr.eq.0) call get_obj_string(ierr, str, fileh)
    if (ierr.eq.0) call get_obj_string(ierr, bname, ofile(jfile)%bh)
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
201    format('file:', A, 1x, A, '[', I0, '] > ', A)
       write(str, 201) trim(acc), trim(ofile(jfile)%name), &
            & user_index_bgn(ofile(jfile)%irec - 1), trim(bname)
       if (is_msglev_DETAIL(lv)) then
          if (ierr.eq.0) call get_header_lprops(ierr, lpp, ofile(jfile)%h)
          if (ierr.eq.0) call get_domain_string(ierr, domain, lpp, mcoor)
          str = trim(str) // ' ' // trim(domain)
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

    ierr = 0
    utmp = choice(ulog, u)

    push = size(aq%lefts)
    pop  = aq%nopr

101 format('operator:', A, ' -', I0, '+', I0, 1x, A, ' >> ', A)
    call query_opr_name(ierr, opr, aq%term)

    write(str, 101) trim(opr), pop, push, trim(aq%desci), trim(aq%desco)
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
       obuffer(jb)%m = 0
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

    ierr = 0
    call new_buffer(ierr, handle)
    if (ierr.eq.0) then
       jb = buf_h2item(handle)
       m = 1
       call alloc_buffer_t(ierr, obuffer(jb), m)
       if (ierr.eq.0) then
          if (present(name)) then
             call reg_fake_opr(ierr, handle, name)
             if (ierr.eq.0) obuffer(jb)%name = name
          else
101          format('L', I0)
             write(bname, 101) user_index_bgn(lcount)
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
          obuffer(jb)%pcp(:)%bgn = 0
          obuffer(jb)%pcp(:)%end = 0
          obuffer(jb)%pcp(:)%stp = -1
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

    ierr = 0
    if (buf%m.gt.0) then
       if (n.gt.buf%m) then
          deallocate(buf%vd, STAT=ierr)
          if (ierr.eq.0) allocate(buf%vd(0:n-1), STAT=ierr)
       endif
    else
       allocate(buf%vd(0:n-1), STAT=ierr)
    endif
    if (ierr.eq.0) buf%m = n
    return
  end subroutine alloc_buffer_t

!!!_   . get_buffer_size
  integer function get_buffer_size(handle) result(n)
    implicit none
    integer,intent(in) :: handle
    integer jb, jc
    integer w
    jb = buf_h2item(handle)
    if (jb.ge.0) then
       n = 1
       do jc = 0, lcoor - 1
          w = (obuffer(jb)%pcp(jc)%end - obuffer(jb)%pcp(jc)%bgn)
          n = n * max(1, w * max(0, obuffer(jb)%pcp(jc)%stp))
          ! n = n * max(1, w)
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

! !!!_   . register_obj
!   subroutine register_obj(ierr, handle, name)
!     use TOUZA_Std,only: reg_entry
!     implicit none
!     integer,         intent(out) :: ierr
!     integer,         intent(in)  :: handle
!     character(len=*),intent(in)  :: name
!     integer entr

!     ierr = 0
!     entr = reg_entry(name, htobj, handle)
!     ierr = min(0, entr)
!   end subroutine register_obj

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

!!!_   . reset_file
  subroutine reset_file(ierr, file, name, mode, flag)
    use TOUZA_Std,only: choice
    use TOUZA_Nio,only: GFMT_ERR
    implicit none
    integer,         intent(out)         :: ierr
    type(file_t),    intent(inout)       :: file
    character(len=*),intent(in),optional :: name
    integer,         intent(in),optional :: mode
    integer,         intent(in),optional :: flag

    ierr = 0

    file%u = -1
    file%irec = 0
    file%nrec = -1
    file%h = ' '
    file%fmt = ' '
    file%kfmt = GFMT_ERR
    file%hedit = hedit_all
    file%mode  = choice(mode_unset, mode)
    file%hflag = choice(hflag_unset, flag)

    if (present(name)) then
       file%name = name
    else
       file%name = ' '
    endif
  end subroutine reset_file

!!!_   . read_file
  subroutine read_file(ierr, neof, nterm, file, handle)
    use TOUZA_Std,only: new_unit, sus_open, is_error_match, upcase
    use TOUZA_Nio,only: nio_read_header, parse_header_size, nio_read_data, nio_skip_records
    use TOUZA_Nio,only: show_header, get_item, restore_item
    use TOUZA_Nio,only: hi_MISS, hi_ITEM, hi_DATE, hi_TIME
    implicit none
    integer,     intent(out)   :: ierr
    integer,     intent(inout) :: neof, nterm
    type(file_t),intent(inout) :: file
    integer,     intent(in)    :: handle

    character(len=litem) :: head(nitem)
    integer n
    integer jb
    character(len=128) :: txt
    character(len=litem) :: tstr
    character(len=ldesc) :: tdesc
    integer :: dt(6)
    integer jerr
    integer xrec

    ierr = 0

    jb = 0
    if (ierr.eq.0) then
       if (file%bh.lt.0) call new_buffer(ierr, file%bh)
    endif
    if (ierr.eq.0) jb = buf_h2item(file%bh)

    if (file%hflag.eq.hflag_unset) file%hflag = def_read%hflag

    if (ierr.eq.0) call open_cue_read_file(ierr, xrec, head, file)
    if (ierr.eq.ERR_EXHAUST) then
       ierr = 0
       neof = neof + 1
       if (file%mode.eq.mode_terminate) nterm = nterm + 1
       return
    endif
    if (is_error_match(ierr, ERR_EOF)) return

    if (ierr.eq.0) then
       file%h(:) = head(:)
       n = parse_header_size(file%h, 0, lazy=1)
       obuffer(jb)%k = suggest_type(file%h)
       call alloc_buffer_t(ierr, obuffer(jb), n)
    endif
    if (file%kfmt.le.cfmt_org) then
       if (ierr.eq.0) call banner_record(ierr)
       if (ierr.eq.0) call read_file_data(ierr, obuffer(jb)%vd, n, file)
    else
       if (ierr.eq.0) call read_file_data(ierr, obuffer(jb)%vd, n, file)
       if (ierr.eq.ERR_EXHAUST) then
          ierr = 0
          neof = neof + 1
          if (file%mode.eq.mode_terminate) nterm = nterm + 1
          return
       endif
       if (is_error_match(ierr, ERR_EOF)) return
       if (ierr.eq.0) call banner_record(ierr)
    endif

    if (ierr.eq.0) call get_item(ierr, file%h, obuffer(jb)%undef, hi_MISS, def=UNDEF)

    ! if (ierr.eq.0) then
    !    call nio_read_data(ierr, obuffer(jb)%vd, n, file%h, file%t, file%u)
    !    if (ierr.eq.0) file%irec = file%irec + 1
    ! endif

    if (ierr.eq.0) then
       call get_item(ierr, file%h, obuffer(jb)%desc, hi_ITEM)
       if (obuffer(jb)%desc.eq.' ') then
          obuffer(jb)%desc = obuffer(jb)%name
          obuffer(jb)%desc2 = obuffer(jb)%name
       else
          tdesc = adjustl(obuffer(jb)%desc)
          call upcase(tdesc)
          if (verify(trim(tdesc), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789').eq.0) then
             obuffer(jb)%desc2 = adjustl(trim(obuffer(jb)%desc))
          else
             obuffer(jb)%desc2 = '<' // adjustl(trim(obuffer(jb)%desc)) // '>'
          endif
       endif
       obuffer(jb)%ilev = ilev_term

       obuffer(jb)%reff = handle
    endif
    if (ierr.eq.0) call get_header_lprops(ierr, obuffer(jb)%pcp, file%h, file%hflag)

    if (ierr.eq.0) then
       call get_item(jerr, file%h, dt(:), hi_DATE)
       if (jerr.ne.0) dt(:) = -1
       call restore_item(jerr, file%h, tstr, hi_TIME)
       if (jerr.ne.0) tstr = ' '
    endif
101 format('  read:', A, 1x, A, ' T = ', A, ' DATE = ', I0, '/', I0, '/', I0, 1x, I2.2, ':', I2.2, ':', I2.2)
    write(txt, 101) trim(obuffer(jb)%name), trim(obuffer(jb)%desc), trim(adjustl(tstr)), dt(:)
    call message(ierr, txt, levm=msglev_normal, u=uerr)

    return
  end subroutine read_file

!!!_   . open_cue_read_file
  subroutine open_cue_read_file &
       & (ierr, xrec, head, file)
    use TOUZA_Std,only: new_unit, sus_open, is_error_match, upcase
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: xrec
    character(len=*),intent(out)   :: head(*)
    type(file_t),    intent(inout) :: file
    ierr = 0

    if (ierr.eq.0) then
       if (file%u.lt.0) then
          if (file%mode.eq.mode_read) file%mode = def_read%mode
          file%u = new_unit()
          ierr = min(0, file%u)
          if (ierr.eq.0) then
             if (file%kfmt.eq.cfmt_ascii) then
                open(UNIT=file%u, FILE=file%name, IOSTAT=ierr, &
                     & ACTION='READ', STATUS='OLD', FORM='FORMATTED', &
                     & ACCESS='SEQUENTIAL')
             else
                call sus_open(ierr, file%u, file%name, ACTION='R', STATUS='O')
             endif
          endif
          if (ierr.ne.0) then
             call message(ierr, 'failed to read open:'// trim(file%name))
             return
          endif
          file%irec = 0
          call init_read_rec(ierr, xrec, file)
       else
          call next_read_rec(ierr, xrec, file)
       endif
    endif
    if (ierr.eq.0) then
       select case(file%kfmt)
       case(cfmt_ascii)
          call cue_read_ascii(ierr, head, file, xrec)
       case(cfmt_binary:)
          call cue_read_binary(ierr, head, file, xrec)
       case default
          call cue_read_header(ierr, head, file, xrec)
       end select
    endif
  end subroutine open_cue_read_file

!!!_   . init_read_rec
  subroutine init_read_rec(ierr, xrec, file)
    implicit none
    integer,     intent(out)   :: ierr
    integer,     intent(out)   :: xrec
    type(file_t),intent(inout) :: file

    ierr = 0
    file%jrf = 0
    xrec = file%recf(file%jrf)%bgn
  end subroutine init_read_rec

!!!_   . next_read_rec
  subroutine next_read_rec(ierr, xrec, file)
    implicit none
    integer,     intent(out)   :: ierr
    integer,     intent(out)   :: xrec
    type(file_t),intent(inout) :: file

    type(loop_t),pointer :: recf
    integer nrf

    ierr = 0
    nrf = size(file%recf)
    if (file%jrf.lt.nrf) then
       recf => file%recf(file%jrf)
       if (recf%stp.gt.0) then
          xrec = file%irec + (recf%stp - 1)
          if (recf%end.lt.0) then
             continue
          else if (xrec.lt.recf%end) then
             continue
          else
             file%jrf = file%jrf + 1
             if (file%jrf.lt.nrf) then
                recf => file%recf(file%jrf)
                xrec = recf%bgn
             else
                ierr = ERR_EXHAUST
                xrec = -1
             endif
          endif
       else if (recf%stp.lt.0) then
          xrec = file%irec + (recf%stp - 1)
          if (xrec.gt.recf%end) then
             continue
          else
             file%jrf = file%jrf + 1
             if (file%jrf.lt.nrf) then
                recf => file%recf(file%jrf)
                xrec = recf%bgn
             else
                ierr = ERR_EXHAUST
                xrec = -1
             endif
          endif
       else
          ierr = ERR_PANIC
          xrec = -1
          call message(ierr, 'invalid record filter')
          return
       endif
    else
       ierr = ERR_EXHAUST
    endif
  end subroutine next_read_rec

!!!_   . cue_read_header
  subroutine cue_read_header &
       & (ierr, head, file, rec)
    use TOUZA_Std,only: is_error_match
    use TOUZA_Nio,only: nio_skip_records, nio_read_header
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(out)   :: head(*)
    type(file_t),    intent(inout) :: file
    integer,         intent(in)    :: rec

    type(loop_t),pointer :: recf
    integer nrf
    integer nfwd, nsuc
    integer xrec

    ierr = 0
    recf => file%recf(file%jrf)
    nrf = size(file%recf)
    xrec = rec
    proc: do
       if (file%nrec.ge.0.and.xrec.ge.file%nrec) then
          ierr = ERR_EOF
       else
          if (xrec.lt.file%irec) then
             rewind(file%u, IOSTAT=ierr)
             if (ierr.ne.0) then
                ierr = ERR_PANIC
                call message(ierr, 'rewind failed')
                return
             endif
             file%irec = 0
          endif

          nfwd = xrec - file%irec
          call nio_skip_records(ierr, nfwd, file%u, nskip=nsuc)
          file%irec = file%irec + nsuc
          if (xrec.lt.file%irec) ierr = ERR_EOF
       endif
       if (ierr.eq.0) call nio_read_header(ierr, head, file%t, file%u)

       if (is_error_match(ierr, ERR_EOF)) then
          file%nrec = file%irec
          if (recf%end.lt.0.and.recf%bgn.le.file%irec) then
             ! recf%end = file%nrec
             file%jrf = file%jrf + 1
             if (file%jrf.ge.nrf) then
                ierr = ERR_EXHAUST     ! exhaust record filters
             else
                recf => file%recf(file%jrf)
                xrec = recf%bgn
                cycle proc
             endif
          else
             call message(ierr, 'reach eof')
          endif
       endif
       exit proc
    enddo proc
  end subroutine cue_read_header

!!!_   . cue_read_ascii
  subroutine cue_read_ascii &
       & (ierr, head, file, rec)
    use TOUZA_Std,only: is_error_match
    use TOUZA_Nio,only: put_header_cprop, parse_header_size, put_item
    use TOUZA_Nio,only: get_default_header, hi_DFMT, fill_header
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(out)   :: head(*)
    type(file_t),    intent(inout) :: file
    integer,         intent(in)    :: rec

    type(loop_t),pointer :: recf
    integer nrf
    integer nfwd, nsuc
    integer xrec
    integer j, n
    integer irange(2, mcoor)
    integer nco, jc

    ierr = 0
    recf => file%recf(file%jrf)
    nrf = size(file%recf)
    xrec = rec

    if (ierr.eq.0) call get_default_header(head)
    if (ierr.eq.0) call put_item(ierr, head, 'UR8',  hi_DFMT)

    if (file%irec.eq.0.and.file%nrec.lt.0) then
       if (ierr.eq.0) call parse_format_shape(nco, irange, mcoor, file%fmt)
       if (nco.eq.0) then
          n = 0
          do
             read(unit=file%u, fmt=*, IOSTAT=ierr)
             if (ierr.ne.0) exit
             n = n + 1
          enddo
          rewind(unit=file%u, IOSTAT=ierr)
          nco = 1
          irange(:, 1) = (/1, n/)
          if (ierr.eq.0) file%nrec = 1
       else
          ierr = min(0, nco)
       endif
       if (ierr.eq.0) then
          do jc = 1, nco
             call put_header_cprop(ierr, file%h, ' ', irange(1:2, jc), jc)
          enddo
       endif
    endif
    if (ierr.eq.0) call fill_header(ierr, head, file%h, 1)
    proc: do
       if (file%nrec.ge.0.and.xrec.ge.file%nrec) then
          ierr = ERR_EOF
       else
          if (xrec.lt.file%irec) then
             rewind(file%u, IOSTAT=ierr)
             if (ierr.ne.0) then
                ierr = ERR_PANIC
                call message(ierr, 'rewind failed')
                return
             endif
             file%irec = 0
          endif
          nsuc = 0
          n = parse_header_size(head, 0, lazy=1)
          nfwd = xrec - file%irec
          recskip: do
             if (nsuc.ge.nfwd) exit recskip
             do j = 0, n - 1
                read(unit=file%u, fmt=*, IOSTAT=ierr)
                if (ierr.ne.0) then
                   if (j.eq.0) then
                      ierr = ERR_EOF
                   else
                      ierr = ERR_BROKEN_RECORD
                   endif
                   exit recskip
                endif
             enddo
             nsuc = nsuc + 1
          enddo recskip
          file%irec = file%irec + nsuc
       endif
       if (is_error_match(ierr, ERR_EOF)) then
          file%nrec = file%irec
          if (recf%end.lt.0.and.recf%bgn.le.file%irec) then
             ! recf%end = file%nrec
             file%jrf = file%jrf + 1
             if (file%jrf.ge.nrf) then
                ierr = ERR_EXHAUST     ! exhaust record filters
             else
                recf => file%recf(file%jrf)
                xrec = recf%bgn
                cycle proc
             endif
          else
             call message(ierr, 'reach eof')
          endif
       endif
       exit proc
    enddo proc
  end subroutine cue_read_ascii

!!!_   . cue_read_binary
  subroutine cue_read_binary &
       & (ierr, head, file, rec)
    use TOUZA_Std,only: is_error_match, KIOFS, get_size_strm
    use TOUZA_Std,only: WHENCE_CURRENT, sus_rseek
    use TOUZA_Nio,only: put_header_cprop, parse_header_size, put_item
    use TOUZA_Nio,only: get_default_header, hi_DFMT, fill_header

    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(out)   :: head(*)
    type(file_t),    intent(inout) :: file
    integer,         intent(in)    :: rec

    type(loop_t),pointer :: recf
    integer nrf
    integer nfwd
    integer xrec
    integer n
    integer irange(2, mcoor)
    integer nco, jc
    integer(kind=KIOFS) :: fsize
    integer usize

    ierr = 0
    recf => file%recf(file%jrf)
    nrf = size(file%recf)
    xrec = rec

    if (ierr.eq.0) call get_default_header(head)

    select case(file%kfmt)
    case(cfmt_binary_i4:cfmt_binary_i4+cfmt_flags_bo-1)
       usize = get_size_strm(0)
       if (ierr.eq.0) call put_item(ierr, head, 'UR8',  hi_DFMT)
    case(cfmt_binary_r4:cfmt_binary_r4+cfmt_flags_bo-1)
       usize = get_size_strm(real(0, kind=KFLT))
       if (ierr.eq.0) call put_item(ierr, head, 'UR4',  hi_DFMT)
    case(cfmt_binary_r8:cfmt_binary_r8+cfmt_flags_bo-1)
       usize = get_size_strm(real(0, kind=KDBL))
       if (ierr.eq.0) call put_item(ierr, head, 'UR8',  hi_DFMT)
    case default
       usize = 1
    end select

    if (file%irec.eq.0.and.file%nrec.lt.0) then
       if (ierr.eq.0) call parse_format_shape(nco, irange, mcoor, file%fmt)
       if (nco.eq.0) then
          n = 0
          if (ierr.eq.0) inquire(UNIT=file%u, IOSTAT=ierr, SIZE=fsize)
          n = int((fsize - 1) / usize + 1)
          nco = 1
          irange(:, 1) = (/1, n/)
          if (ierr.eq.0) file%nrec = 1
       else
          ierr = min(0, nco)
       endif
       if (ierr.eq.0) then
          do jc = 1, nco
             call put_header_cprop(ierr, file%h, ' ', irange(1:2, jc), jc)
          enddo
       endif
    endif
    if (ierr.eq.0) call fill_header(ierr, head, file%h, 1)
    proc: do
       if (file%nrec.ge.0.and.xrec.ge.file%nrec) then
          ierr = ERR_EOF
       else
          if (xrec.lt.file%irec) then
             rewind(file%u, IOSTAT=ierr)
             if (ierr.ne.0) then
                ierr = ERR_PANIC
                call message(ierr, 'rewind failed')
                return
             endif
             file%irec = 0
          endif
          n = parse_header_size(head, 0, lazy=1)
          nfwd = xrec - file%irec
          fsize = usize
          fsize = fsize * n * nfwd
          call sus_rseek(ierr, file%u, fsize, whence=WHENCE_CURRENT)
          if (ierr.ne.0) then
             ierr = ERR_BROKEN_RECORD
          else
             file%irec = file%irec + nfwd
          endif
       endif
       if (is_error_match(ierr, ERR_EOF)) then
          file%nrec = file%irec
          if (recf%end.lt.0.and.recf%bgn.le.file%irec) then
             ! recf%end = file%nrec
             file%jrf = file%jrf + 1
             if (file%jrf.ge.nrf) then
                ierr = ERR_EXHAUST     ! exhaust record filters
             else
                recf => file%recf(file%jrf)
                xrec = recf%bgn
                cycle proc
             endif
          else
             call message(ierr, 'reach eof')
          endif
       endif
       exit proc
    enddo proc
  end subroutine cue_read_binary

!!!_   . read_file_data
  subroutine read_file_data(ierr, v, n, file)
    use TOUZA_Std,only: is_error_match, KIOFS, get_size_strm, sus_read
    use TOUZA_Nio,only: nio_read_data
    implicit none
    integer,        intent(out)   :: ierr
    real(kind=KBUF),intent(out)   :: v(0:*)
    integer,        intent(in)    :: n
    type(file_t),   intent(inout) :: file
    integer j
    integer,        allocatable,save :: bufi(:)
    real(kind=KFLT),allocatable,save :: buff(:)
    real(kind=KDBL),allocatable,save :: bufd(:)
    integer(kind=KIOFS) :: jposb, jpose
    integer usize
    logical swap

    ierr = 0
    if (ierr.eq.0) then
       select case(file%kfmt)
       case(cfmt_ascii)
          do j = 0, n - 1
             read(unit=file%u, fmt=*, IOSTAT=ierr) v(j)
             if (ierr.ne.0) then
                if (j.eq.0) then
                   ierr = ERR_EOF
                else
                   ierr = ERR_BROKEN_RECORD
                endif
                exit
             endif
          enddo
          if (is_error_match(ierr, ERR_EOF)) then
             if (file%nrec.lt.0) then
                file%nrec = file%irec
                ierr = ERR_EXHAUST
             endif
          endif
       case(cfmt_binary_i4:cfmt_binary_i4+cfmt_flags_bo-1)
          swap = get_swap_switch(file%kfmt - cfmt_binary_i4)
          usize = get_size_strm(0)
          inquire(unit=file%u, pos=jposb, size=jpose, IOSTAT=ierr)
          if (ierr.eq.0) then
             if (jpose.lt.jposb) then
                ierr = ERR_EXHAUST
                if (file%nrec.lt.0) file%nrec = file%irec
             else if ((jpose-jposb+1)/usize.lt.n) then
                ierr = ERR_BROKEN_RECORD
             endif
          endif
          if (ierr.eq.0) then
             if (.not.allocated(bufi)) then
                allocate(bufi(0:n-1), STAT=ierr)
             else if (size(bufi).lt.n) then
                deallocate(bufi, STAT=ierr)
                if (ierr.eq.0) allocate(bufi(0:n-1), STAT=ierr)
             endif
             if (ierr.eq.0) call sus_read(ierr, file%u, bufi, n, swap)
             if (ierr.eq.0) then
                v(0:n-1) = real(bufi(0:n-1), kind=KBUF)
             endif
          endif
       case(cfmt_binary_r4:cfmt_binary_r4+cfmt_flags_bo-1)
          swap = get_swap_switch(file%kfmt - cfmt_binary_r4)
          usize = get_size_strm(real(0, kind=KFLT))
          inquire(unit=file%u, pos=jposb, size=jpose, IOSTAT=ierr)
          if (ierr.eq.0) then
             if (jpose.lt.jposb) then
                ierr = ERR_EXHAUST
                if (file%nrec.lt.0) file%nrec = file%irec
             else if ((jpose-jposb+1)/usize.lt.n) then
                ierr = ERR_BROKEN_RECORD
             endif
          endif
          if (ierr.eq.0) then
             if (.not.allocated(buff)) then
                allocate(buff(0:n-1), STAT=ierr)
             else if (size(buff).lt.n) then
                deallocate(buff, STAT=ierr)
                if (ierr.eq.0) allocate(buff(0:n-1), STAT=ierr)
             endif
             if (ierr.eq.0) call sus_read(ierr, file%u, buff, n, swap)
             if (ierr.eq.0) then
                v(0:n-1) = real(buff(0:n-1), kind=KBUF)
             endif
          endif
       case(cfmt_binary_r8:cfmt_binary_r8+cfmt_flags_bo-1)
          swap = get_swap_switch(file%kfmt - cfmt_binary_r8)
          usize = get_size_strm(real(0, kind=KDBL))
          inquire(unit=file%u, pos=jposb, size=jpose, IOSTAT=ierr)
          if (ierr.eq.0) then
             if (jpose.lt.jposb) then
                ierr = ERR_EXHAUST
                if (file%nrec.lt.0) file%nrec = file%irec
             else if ((jpose-jposb+1)/usize.lt.n) then
                ierr = ERR_BROKEN_RECORD
             endif
          endif
          if (ierr.eq.0) then
             if (.not.allocated(bufd)) then
                allocate(bufd(0:n-1), STAT=ierr)
             else if (size(bufd).lt.n) then
                deallocate(bufd, STAT=ierr)
                if (ierr.eq.0) allocate(bufd(0:n-1), STAT=ierr)
             endif
             if (ierr.eq.0) call sus_read(ierr, file%u, bufd, n, swap)
             if (ierr.eq.0) then
                v(0:n-1) = real(bufd(0:n-1), kind=KBUF)
             endif
          endif
       case default
          call nio_read_data(ierr, v, n, file%h, file%t, file%u)
       end select
    endif
    if (ierr.eq.0) file%irec = file%irec + 1

    if (ierr.ne.0) then
       if (ierr.ne.ERR_EXHAUST) then
          call message(ierr, 'failed to read ' // trim(file%name))
       endif
    endif
  end subroutine read_file_data

!!!_    * get_swap_switch
  logical function get_swap_switch (flag) result(b)
    use TOUZA_Std,only: kendi_mem, endian_BIG, endian_LITTLE
    implicit none
    integer,intent(in) :: flag
    select case(flag)
    case(cfmt_flag_native)
       b = .FALSE.
    case(cfmt_flag_swap)
       b = .TRUE.
    case(cfmt_flag_little)
       b = kendi_mem.eq.endian_BIG
    case(cfmt_flag_big)
       b = kendi_mem.eq.endian_LITTLE
    case default
       b = .FALSE.
    end select
  end function get_swap_switch
!!!_   . write_file
  subroutine write_file(ierr, file, jstk, levv)
    use TOUZA_Std,only: new_unit, sus_open, is_error_match
    use TOUZA_Std,only: get_login_name
    use TOUZA_Nio,only: nio_write_header, parse_header_size, nio_write_data
    use TOUZA_Nio,only: get_default_header, show_header, parse_record_fmt
    use TOUZA_Nio,only: REC_DEFAULT, REC_BIG
    use TOUZA_Nio,only: put_item, get_item, restore_item, store_item
    use TOUZA_Nio,only: hi_MISS, hi_ITEM, hi_DFMT,  hi_EDIT1, hi_TITL1, hi_ETTL1
    use TOUZA_Nio,only: hi_DATE, hi_TIME, hi_CSIGN, hi_CDATE, hi_MSIGN, hi_MDATE
    use TOUZA_Nio,only: fill_header
    implicit none
    integer,     intent(out)         :: ierr
    type(file_t),intent(inout)       :: file
    integer,     intent(in)          :: jstk
    integer,     intent(in),optional :: levv
    integer n
    integer jc
    integer jb, jrefh
    character(len=128) :: txt
    character(len=litem) :: tstr
    integer :: dt(6)
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
    if (ierr.eq.0) then
       if (file%bh.lt.0) ierr = ERR_PANIC
    endif
    if (ierr.eq.0) jb = buf_h2item(file%bh)

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
          call put_item(jerr, head, idt(1:6), jdi)
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
          is_tweak = ANY(bstack(jstk)%lcp(:)%stp.ge.0)
       endif
       if (.not.is_tweak) then
          is_tweak = ANY(bstack(jstk)%lcp(:)%name.ne.' ')
       endif
       if (is_tweak) then
          ! write(*, *) 'tweak'
          call tweak_buffer(ierr, btmp, file%bh, jstk)
          if (ierr.eq.0) call put_header_lprops(ierr, head, btmp%pcp, file%hflag)
       else if (ANY(obuffer(jb)%pcp(:)%stp.eq.0)) then
          btmp%pcp(:) = obuffer(jb)%pcp(:)
          if (file%hflag.eq.hflag_nulld) then
             do jc = 0, lcoor - 1
                if (btmp%pcp(jc)%stp.eq.0) then
                   btmp%pcp(jc)%end = btmp%pcp(jc)%bgn
                endif
             enddo
          else
             do jc = 0, lcoor - 1
                if (btmp%pcp(jc)%stp.eq.0) then
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
       call get_item(jerr, head, dt(:), hi_DATE)
       if (jerr.ne.0) dt(:) = -1
       call restore_item(jerr, head, tstr, hi_TIME)
       if (jerr.ne.0) tstr = ' '
    endif
101 format('  write:', A, 1x, A, ' T = ', A, ' DATE = ', I0, '/', I0, '/', I0, 1x, I2.2, ':', I2.2, ':', I2.2)
    write(txt, 101) trim(obuffer(jb)%name), trim(obuffer(jb)%desc), trim(adjustl(tstr)), dt(:)
    call message(ierr, txt, levm=msglev_normal, u=uerr)

    if (is_tweak) then
       if (ierr.eq.0) deallocate(btmp%vd, STAT=ierr)
    endif

    if (ierr.ne.0) call show_header(ierr, head)
    return
  end subroutine write_file

!!!_   . open_write_file
  subroutine open_write_file &
       & (ierr, file)
    use TOUZA_Std,only: new_unit, sus_open, is_error_match, upcase
    implicit none
    integer,     intent(out)   :: ierr
    type(file_t),intent(inout) :: file
    character(len=16) :: stt, pos

    ierr = 0

    if (file%u.lt.0) then
       file%u = new_unit()
       ierr = min(0, file%u)
       if (ierr.eq.0) then
          if (file%kfmt.eq.cfmt_ascii) then
             select case (file%mode)
             case (mode_new)
                stt = 'NEW'
                pos = 'ASIS'
             case (mode_write)
                stt = 'REPLACE'
                pos = 'ASIS'
             case (mode_append)
                stt = 'UNKNOWN'
                pos = 'APPEND'
             case default
                stt = 'UNKNOWN'
                pos = 'ASIS'
             end select
             open(UNIT=file%u, FILE=file%name, IOSTAT=ierr, &
                  & ACTION='WRITE', FORM='FORMATTED', &
                  & ACCESS='SEQUENTIAL', STATUS=trim(stt), POSITION=trim(pos))
          else
             select case (file%mode)
             case (mode_new)
                stt = 'N'
             case (mode_write)
                stt = 'R'
                pos = ' '
             case (mode_append)
                stt = 'U'
                pos = 'AP'
             case default
                stt = ' '
                pos = ' '
             end select
             call sus_open(ierr, file%u, file%name, ACTION='W', STATUS=stt, POSITION=pos)
          endif
          if (ierr.ne.0) then
             write(*, *) 'failed to write open:', trim(file%name)
             return
          endif
       endif
       file%irec = 0
    endif
  end subroutine open_write_file

!!!_   . write_file_data
  subroutine write_file_data &
       & (ierr, v, n, head, file, kv)
    use TOUZA_Std,only: is_error_match, KIOFS, get_size_strm, sus_write
    use TOUZA_Nio,only: nio_write_data
    implicit none
    integer,         intent(out)   :: ierr
    real(kind=KBUF), intent(in)    :: v(0:*)
    integer,         intent(in)    :: n
    character(len=*),intent(in)    :: head(*)
    type(file_t),    intent(inout) :: file
    integer,         intent(in)    :: kv

    integer,        allocatable,save :: bufi(:)
    real(kind=KFLT),allocatable,save :: buff(:)
    real(kind=KDBL),allocatable,save :: bufd(:)
    logical swap

    ierr = 0
    if (ierr.eq.0) then
       select case(file%kfmt)
       case(cfmt_ascii)
          call write_file_ascii(ierr, v, n, head, file, kv)
       case(cfmt_binary_i4:cfmt_binary_i4+cfmt_flags_bo-1)
          swap = get_swap_switch(file%kfmt - cfmt_binary_i4)
          if (ierr.eq.0) then
             if (.not.allocated(bufi)) then
                allocate(bufi(0:n-1), STAT=ierr)
             else if (size(bufi).lt.n) then
                deallocate(bufi, STAT=ierr)
                if (ierr.eq.0) allocate(bufi(0:n-1), STAT=ierr)
             endif
             if (ierr.eq.0) then
                bufi(0:n-1) = int(v(0:n-1))
             endif
             if (ierr.eq.0) call sus_write(ierr, file%u, bufi, n, swap)
          endif
       case(cfmt_binary_r4:cfmt_binary_r4+cfmt_flags_bo-1)
          swap = get_swap_switch(file%kfmt - cfmt_binary_r4)
          if (ierr.eq.0) then
             if (.not.allocated(buff)) then
                allocate(buff(0:n-1), STAT=ierr)
             else if (size(buff).lt.n) then
                deallocate(buff, STAT=ierr)
                if (ierr.eq.0) allocate(buff(0:n-1), STAT=ierr)
             endif
             if (ierr.eq.0) then
                buff(0:n-1) = real(v(0:n-1),kind=KFLT)
             endif
             if (ierr.eq.0) call sus_write(ierr, file%u, buff, n, swap)
          endif
       case(cfmt_binary_r8:cfmt_binary_r8+cfmt_flags_bo-1)
          swap = get_swap_switch(file%kfmt - cfmt_binary_r8)
          if (ierr.eq.0) then
             if (.not.allocated(bufd)) then
                allocate(bufd(0:n-1), STAT=ierr)
             else if (size(bufd).lt.n) then
                deallocate(bufd, STAT=ierr)
                if (ierr.eq.0) allocate(bufd(0:n-1), STAT=ierr)
             endif
             if (ierr.eq.0) then
                bufd(0:n-1) = real(v(0:n-1),kind=KDBL)
             endif
             if (ierr.eq.0) call sus_write(ierr, file%u, bufd, n, swap)
          endif
       case default
          call nio_write_data(ierr, v, n, head, file%t, file%u)
       end select
    endif

    if (ierr.ne.0) then
       call message(ierr, 'failed to write ' // trim(file%name))
    endif
  end subroutine write_file_data

!!!_   . write_file_ascii
  subroutine write_file_ascii &
       & (ierr, v, n, head, file, kv)
    implicit none
    integer,         intent(out)   :: ierr
    real(kind=KBUF), intent(in)    :: v(0:*)
    integer,         intent(in)    :: n
    character(len=*),intent(in)    :: head(*)
    type(file_t),    intent(inout) :: file
    integer,         intent(in)    :: kv
    integer j
    character(len=lfmt) :: fmt

    ierr = 0
    fmt = file%fmt
    if (fmt.ne.' ') then
       if (fmt(1:1).ne.'(') fmt = '(' // trim(fmt) // ')'
    endif
    select case(kv)
    case(kv_int)
       if (fmt.eq.' ') then
          do j = 0, n - 1
             write(unit=file%u, fmt=*, IOSTAT=ierr) int(v(j))
          enddo
       else
          do j = 0, n - 1
             write(unit=file%u, fmt=fmt, IOSTAT=ierr) int(v(j))
          enddo
       endif
    case(kv_flt)
       if (fmt.eq.' ') then
          do j = 0, n - 1
             write(unit=file%u, fmt=*, IOSTAT=ierr) real(v(j), kind=KFLT)
          enddo
       else
          do j = 0, n - 1
             write(unit=file%u, fmt=fmt, IOSTAT=ierr) real(v(j), kind=KFLT)
          enddo
       endif
    case(kv_dbl)
       if (fmt.eq.' ') then
          do j = 0, n - 1
             write(unit=file%u, fmt=*, IOSTAT=ierr) real(v(j), kind=KDBL)
          enddo
       else
          do j = 0, n - 1
             write(unit=file%u, fmt=fmt, IOSTAT=ierr) real(v(j), kind=KDBL)
          enddo
       endif
    end select
  end subroutine write_file_ascii

!!!_   . parse_format_shape
  subroutine parse_format_shape &
       & (nco, irange, mco, fmt)
    use TOUZA_Std,only: parse_number
    implicit none
    integer,         intent(out) :: nco
    integer,         intent(out) :: irange(2, 0:*)
    integer,         intent(in)  :: mco
    character(len=*),intent(in)  :: fmt
    character(len=*),parameter :: csep = ','
    character(len=*),parameter :: rsep = ':'
    integer jp, je, jr
    integer k
    integer lf

    lf = len_trim(fmt)
    nco = 0
    jp = 0
    do
       if (jp.ge.lf) exit
       if (nco.gt.mco) exit
       je = index(fmt(jp+1:lf), csep)
       if (je.eq.0) je = lf - jp + 1
       if (je.le.1) then
          irange(1:2, nco) = (/0, 0/)
       else
          jr = index(fmt(jp+1:jp+je), rsep)
          if (jr.le.0) then
             irange(1, nco) = 0
             call parse_number(ierr, k, fmt(jp+1:jp+je-1))
             if (ierr.eq.0) irange(2, nco) = system_index_end(k)
          else
             if (jr.gt.1) then
                call parse_number(ierr, k, fmt(jp+1:jp+jr-1))
                if (ierr.eq.0) irange(1, nco) = system_index_bgn(k)
             else
                irange(1, nco) = 0
             endif
             if (jr.lt.je-1) then
                if (ierr.eq.0) call parse_number(ierr, k, fmt(jp+jr+len(rsep):jp+je-1))
                if (ierr.eq.0) irange(2, nco) = system_index_end(k)
             else
                irange(2, nco) = 0
             endif
          endif
          if (ierr.ne.0) exit
          if (irange(2,nco).gt.0) irange(1,nco) = irange(1,nco) + 1
       endif
       nco = nco + 1
       jp = jp + je + len(csep) - 1
    enddo
    ! write(*, *) irange(:, 0:mco-1)
    if (nco.gt.mco) then
       nco = ERR_INVALID_PARAMETER
       call message(nco, 'too many coordinates')
    endif
  end subroutine parse_format_shape

!!!_   . tweak_buffer
  subroutine tweak_buffer (ierr, bdest, hsrc, jstk)
    implicit none
    integer,       intent(out)   :: ierr
    type(buffer_t),intent(inout) :: bdest
    integer,       intent(in)    :: hsrc
    integer,       intent(in)    :: jstk

    integer,parameter :: nbuf = 1
    integer        :: htmp(nbuf), ptmp(nbuf)
    type(domain_t) :: domL
    type(domain_t) :: domR(nbuf)
    integer jset
    integer jbR
    real(kind=KBUF) :: fillR

    ierr = 0
    htmp = hsrc
    ptmp = jstk

    if (ierr.eq.0) call tweak_coordinates(ierr, domL, domR, htmp, ptmp, nbuf, bdest)
    if (ierr.eq.0) call set_inclusive_domain(ierr, domL, domR, htmp, ptmp, nbuf)
    if (ierr.eq.0) call settle_output_domain(ierr, domL)
    if (ierr.eq.0) call settle_input_domain(ierr, domR(1), htmp(1), ptmp(1), domL)
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
       case (GFMT_URT:GFMT_URTend, GFMT_MRT:GFMT_MRTend)
          k = kv_dbl
       case default
          k = kv_dbl
       end select
    endif
    if (jerr.ne.0) k = jerr
  end function suggest_type

!!!_   . get_header_lprops
  subroutine get_header_lprops(ierr, lpp, head, flag)
    use TOUZA_Std,only: choice
    use TOUZA_Nio,only: get_item, hi_AITM1, hi_AITM2, hi_AITM3
    use TOUZA_Nio,only: get_header_cprop
    implicit none
    integer,         intent(out)   :: ierr
    type(loop_t),    intent(inout) :: lpp(0:*)
    character(len=*),intent(in)    :: head(*)
    integer,optional,intent(in)    :: flag

    integer jc
    integer irange(2, 0:mcoor-1)
    integer hf

    ierr = 0
    hf = choice(hflag_nulld, flag)

    irange = 0
    do jc = 0, mcoor - 1
       call get_header_cprop(lpp(jc)%name, irange(1, jc), head, 1+jc)
    enddo
    do jc = 0, mcoor - 1
       if (irange(2, jc).le.0) then
          lpp(jc)%bgn = irange(1, jc)
          lpp(jc)%end = irange(2, jc)
          lpp(jc)%stp = 0
       else
          lpp(jc)%bgn = irange(1, jc) - 1
          lpp(jc)%end = irange(2, jc)
          lpp(jc)%stp = 1
       endif
       if (IAND(hf, hflag_nulld).eq.0) then
          if (lpp(jc)%name.eq.' ' &
               .and. (lpp(jc)%end - lpp(jc)%bgn).eq.1) then
             lpp(jc)%stp = 0
          endif
       endif
    enddo
    do jc = mcoor, lcoor - 1
       lpp(jc)%name = ' '
       lpp(jc)%bgn = 0
       lpp(jc)%end = 0
       lpp(jc)%stp = -1
    enddo
  end subroutine get_header_lprops

!!!_   . put_header_lprops
  subroutine put_header_lprops(ierr, head, lpp, flag)
    use TOUZA_Nio,only: put_header_cprop
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: head(*)
    type(loop_t),    intent(in)  :: lpp(0:*)
    integer,         intent(in)  :: flag

    integer jc
    integer irange(2)

    ierr = 0

    do jc = 0, mcoor - 1
       if (lpp(jc)%bgn.eq.null_range .eqv. lpp(jc)%end.eq.null_range) then
          if (lpp(jc)%bgn.eq.null_range) then
             irange(:) = 0
          else
             irange(1) = lpp(jc)%bgn
             irange(2) = lpp(jc)%end
             if (irange(2).gt.0) irange(1) = irange(1) + 1
          endif
       else
          ierr = ERR_PANIC
          call message(ierr, 'invalid coordinate ranges ', (/jc/))
          return
       endif
       if (IAND(flag, hflag_nulld).eq.0) then
          if (lpp(jc)%name.eq.' ' &
               .and.irange(2).eq.0) then
             if (irange(1).le.0) irange(1) = 1
             irange(2) = irange(1)
          endif
       endif
       if (ierr.eq.0) call put_header_cprop(ierr, head, lpp(jc)%name, irange, 1+jc)
    enddo
    do jc = mcoor, lcoor - 1
       if (lpp(jc)%stp.ge.0) then
          ierr = ERR_PANIC
          call message(ierr, 'reach coordinate limits')
       endif
    enddo
  end subroutine put_header_lprops

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

!!!_   . is_read_mode
  logical function is_read_mode(mode) result(b)
    implicit none
    integer,intent(in) :: mode
    b = mode.le.mode_read
  end function is_read_mode
!!!_  - handle manager
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
    use TOUZA_Std,only: query_name, choice
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

102 format('file[', I0, ']')
103 format('unknown[', I0, ']')
104 format('opr[', A, ']')
105 format('anchor[', I0, ']')
106 format('anchor[-]')
    j = file_h2item(handle)
    if (j.ge.0) then
       write(str, 102) handle
       return
    endif
    j = buf_h2item(handle)
    if (j.ge.0) then
       if (obuffer(j)%name.ne.' ') then
          write(str, 112) trim(obuffer(j)%name)
       else
          write(str, 111) handle
       endif
       return
    endif
    j = anchor_h2level(handle)
    if (j.gt.0) then
       write(str, 105) j
    else if (j.eq.0) then
       write(str, 106)
    else
       call query_opr_name(ierr, buf, handle)
       if (ierr.eq.0) then
          write(str, 104) trim(buf)
       endif
       if (ierr.ne.0) write(str, 103) handle
    endif
  end subroutine get_obj_string

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

    ierr = 0
    nc = 0
    mc = choice(lcoor, maxco)
    do jc = 0, mc - 1
       call get_range_string(ierr, cran, lpp(jc)%bgn, lpp(jc)%end, lpp(jc)%stp)
102    format(A, A, A)
103    format(A, A)
       if (lpp(jc)%name.eq.' ') then
          cstr(jc) = trim(cran)
       else
          ls = len_trim(lpp(jc)%name)
          jp = index(lpp(jc)%name(1:ls), rename_sep, back=.TRUE.)
          if (jp.lt.ls-len(rename_sep)+1) then
             write(cstr(jc), 102) trim(lpp(jc)%name), rename_sep, trim(cran)
          else
             write(cstr(jc), 103) trim(lpp(jc)%name), trim(cran)
          endif
       endif
       if (lpp(jc)%stp.ge.0) nc = jc
       if (lpp(jc)%name.ne.' ') nc = jc
    enddo
    call join_list(ierr, str, cstr(0:nc), ldelim='[', rdelim=']')
  end subroutine get_domain_string

!!!_   . get_range_string
  subroutine get_range_string &
       & (ierr, str, b, e, s)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    integer,         intent(in)  :: b, e, s
    integer bb, ee

    ierr = 0

111 format(':')
112 format(I0, ':')
113 format(':', I0)
114 format(I0, ':', I0)
115 format(I0, ':', I0, '+', I0)
116 format(I0, ':', I0, '-', I0)
    bb = user_index_bgn(b)
    ee = user_index_end(e)
    if (bb.eq.null_range) then
       if (ee.eq.null_range) then
          write(str, 111)
       else
          write(str, 113) ee
       endif
    else if (ee.eq.null_range) then
       write(str, 112) bb
    else if (s.gt.0) then
       write(str, 114) bb, ee
    else if (b.eq.0.and.e.eq.0) then
       str = '-'
    else if (s.eq.0) then
       write(str, 115) bb, ee, s
    else if (s.lt.0) then
       write(str, 116) bb, ee, abs(s)
    else
       write(str, 114) bb, ee
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
                   write(co(jodr), 111) trim(pcp(jco)%name), '*'
                else
                   write(co(jodr), 114) jco, '*'
                endif
             else
                write(co(jodr), 112) '*'
             endif
          case(co_null)
             if (jco.ge.0) then
                if (pcp(jco)%name.ne.' ') then
                   write(co(jodr), 111) trim(pcp(jco)%name), '-'
                else
                   write(co(jodr), 114) jco, '-'
                endif
             else
                write(co(jodr), 112) '-'
             endif
          case(co_normal)
             if (jco.ge.0) then
                if (pcp(jco)%name.eq.cname(jodr)) then
                   co(jodr) = cname(jodr)
                else
                   write(co(jodr), 111) trim(pcp(jco)%name), trim(cname(jodr))
                endif
             else
                write(co(jodr), 112) trim(cname(jodr))
             endif
          case default
             write(co(jodr), 113) jodr
          end select
          if (jco.ge.0 .or. ctype(jodr).ne.co_null) meff = jodr
       enddo
       call join_list(ierr, ndom, co(0:meff), ldelim='[', rdelim=']')
    endif
    if (ierr.eq.0) then
101    format(A, 1x, A, ' > ', A)
       write(str, 101) trim(pdom), trim(ldom), trim(ndom)
    endif
  end subroutine get_perm_string

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
          write(cbuf(jodr), 411) '-', jco
       else if (ctype(jco).eq.co_wild) then
          write(cbuf(jodr), 411) '*', jco
       else
          write(cbuf(jodr), 411) trim(cname(jco)), jco
       endif
    enddo
    call join_list(ierr, str, cbuf(0:dom%mco-1), ldelim='[', rdelim=']')
  end subroutine get_domain_perm

!!!_   . get_domain_result
  subroutine get_domain_result &
       & (ierr, str, dom, pcp)
    use TOUZA_Std,only: join_list
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    type(domain_t),  intent(in)  :: dom
    type(loop_t),    intent(in)  :: pcp(0:*)

    integer jodr, jphyc
    character(len=lname)   :: cran
    character(len=lname*2) :: cbuf(0:lcoor-1)
    integer b, e, s

    ierr = 0
    str = ' '
    do jodr = 0, dom%mco - 1
       b = dom%bgn(jodr)
       e = dom%end(jodr)
       s = pcp(jodr)%stp
       if (ierr.eq.0) call get_range_string(ierr, cran, b, e, s)
       if (ierr.eq.0) then
          jphyc = dom%cidx(jodr)
101       format(A, '/', A)
102       format('/', A)
! 103       format('<', A, '>/', A)
          select case(jphyc)
          case(co_wild)
             write(cbuf(jodr), 101) '*', trim(cran)
          case(co_null)
             write(cbuf(jodr), 101) '-', trim(cran)
          case(0:)
             if (pcp(jodr)%name.eq.' ') then
                write(cbuf(jodr), 102) trim(cran)
             else
                write(cbuf(jodr), 101) trim(pcp(jodr)%name), trim(cran)
             endif
          case default
             cbuf(jodr) = trim(cran)
          end select
       endif
    enddo
    if (ierr.eq.0) call join_list(ierr, str, cbuf(0:dom%mco-1))
  end subroutine get_domain_result
!!!_   . get_domain_shape
  subroutine get_domain_shape &
       & (ierr, str, dom, pcp, lcp, ref)
    use TOUZA_Std,only: join_list
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    type(domain_t),  intent(in)  :: dom
    type(loop_t),    intent(in)  :: pcp(0:*), lcp(0:*)
    type(domain_t),  intent(in)  :: ref

    integer jodr, jphyc, jlogc
    character(len=lname)   :: cran
    character(len=lname*2) :: cbuf(0:lcoor-1)
    integer b, e, s

    ierr = 0
    str = ' '
    do jodr = 0, dom%mco - 1
       ! call get_logical_range(b, e, s, jodr, lcp, pcp, dom, ref)
       if (ierr.eq.0) call get_logical_range(b, e, s, jodr, lcp, pcp, dom)
       if (ierr.eq.0) call get_range_string(ierr, cran, b, e, s)
       if (ierr.eq.0) then
          jphyc = dom%cidx(jodr)
          jlogc = dom%lidx(jodr)
101       format(A, '/', A)
102       format('/', A)
103       format('<', A, '>/', A)
          select case(jphyc)
          case(co_wild)
             write(cbuf(jodr), 101) '*', trim(cran)
          case(co_null)
             write(cbuf(jodr), 101) '-', trim(cran)
          case(0:)
             if (pcp(jphyc)%name.eq.' ') then
                write(cbuf(jodr), 102) trim(cran)
             else
                write(cbuf(jodr), 101) trim(pcp(jphyc)%name), trim(cran)
             endif
          case default
             if (jlogc.ge.0) then
                write(cbuf(jodr), 103) trim(lcp(jlogc)%name), trim(cran)
             else
                cbuf(jodr) = trim(cran)
             endif
          end select
       endif
    enddo
    if (ierr.eq.0) call join_list(ierr, str, cbuf(0:dom%mco-1), ldelim='[', rdelim=']')
  end subroutine get_domain_shape

!!!_  - operation queue dispatcher
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

    ierr = 0

    neof = 0
    nterm = 0

    call banner_record(ierr, irecw)

    do jfile = 0, min(mfile, lfile) - 1
       if (is_read_mode(ofile(jfile)%mode)) then
          hf = file_i2handle(jfile)
          if (ierr.eq.0) call read_file(ierr, neof, nterm, ofile(jfile), hf)
       endif
    enddo
    if (rcount.gt.0) then
       if (neof.eq.rcount) then
          ierr = ERR_FINISHED
          return
       endif
       if (nterm.gt.0) then
          ierr = ERR_EOF
          call message(ierr, 'insufficient records')
          call show_files(jerr)
       endif
    endif
    if (ierr.ne.0) return

    mstack = 0
    do jq = 0, min(mqueue, lqueue) - 1
       hterm = aqueue(jq)%term
       termk = handle_type(hterm)
       if (aqueue(jq)%desci.eq.' ') call set_queue_descr(ierr, aqueue(jq))
       if (is_msglev_DETAIL(levv)) then
          call trace_queue(jerr, aqueue(jq), levv)
       endif
       select case(termk)
       case(hk_file)
          if (ierr.eq.0) call push_file(ierr, hterm, levv)
       case(hk_buffer)
          if (ierr.eq.0) call push_buffer(ierr, hterm, levv)
       case(hk_opr)
          push = size(aqueue(jq)%lefts)
          pop = aqueue(jq)%nopr
          if (ierr.eq.0) then
             call apply_operator &
                  & (ierr, hterm, aqueue(jq)%lefts, pop, push, aqueue(jq)%cmode, levv)
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

    if (ierr.eq.0) then
       if (rcount.eq.0) ierr = ERR_FINISHED
    endif
  end subroutine batch_operation

!!!_   . banner_record
  subroutine banner_record(ierr, irecw)
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: irecw   ! store if present, release otherwise
    integer,save :: brec = -1
    ierr = 0
    if (present(irecw)) then
       brec = irecw
    else if (brec.ge.0) then
       call message &
            & (ierr, '### record:', (/user_index_bgn(brec)/), &
            &  fmt='(I0)', levm=msglev_normal+1)
       brec = -1
    endif
  end subroutine banner_record

!!!_   . push_file
  subroutine push_file &
       & (ierr, handle, levv)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: handle
    integer,intent(in)  :: levv
    integer jfile

    jfile = file_h2item(handle)
    ierr = min(0, jfile)
    if (ierr.eq.0) then
       if (is_read_mode(ofile(jfile)%mode)) then
          if (ierr.eq.0) call push_stack(ierr, ofile(jfile)%bh)
       else
          call write_file(ierr, ofile(jfile), mstack-1, levv)
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
       & (ierr, handle, lefts, pop, push, cmode, levv)
    implicit none
    integer,      intent(out)   :: ierr
    integer,      intent(in)    :: handle
    type(stack_t),intent(inout) :: lefts(*)
    integer,      intent(in)    :: push, pop
    integer,      intent(in)    :: cmode
    integer,      intent(in)    :: levv

    integer,parameter :: lmaxo = 8
    integer righth(pop)
    integer j, jb
    character(len=8) :: opr

    ierr = 0
    ! write(*, *) rlev, size(lefth)

    if (ierr.eq.0) call mpop_stack(ierr, righth, pop, keep=.TRUE.)
    if (ierr.eq.0) then
!!!_    * output
       if (handle.eq.opr_OUTPUT) then
          call flush_stack(ierr, pop, cmode_null)
       else if (handle.eq.opr_FLUSH) then
          call flush_stack(ierr, pop, cmode)
!!!_    * transformation
       else if (handle.eq.opr_TRANSF) then
          continue
       else if (handle.eq.opr_PERM) then
          continue
!!!_    * index
       else if (handle.ge.opr_C0.and.handle.lt.opr_C3) then
          call apply_INDEX(ierr, handle, lefts(1:push))
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
       else if (handle.eq.opr_SIGN) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_SIGN)
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
!!!_    * ignored
       else if (grp_system_bgn.le.handle.and.handle.lt.grp_system_end) then
          continue
       else if (grp_stack_bgn.le.handle.and.handle.lt.grp_stack_end) then
          continue
!!!_    * reserved
       else
          call query_opr_name(ierr, opr, handle)
          ierr = ERR_NOT_IMPLEMENTED
          call message(ierr, 'reserved operator ' // trim(opr))
       endif
    endif
    if (ierr.eq.0) then
       if (grp_float_bgn.le.handle .and. handle.lt.grp_float_end) then
          do j = 1, push
             jb = buf_h2item(lefts(j)%bh)
             obuffer(jb)%k = kv_dbl
          enddo
       else if (ANY(handle.eq.(/opr_IDIV, opr_INT/))) then
          do j = 1, push
             jb = buf_h2item(lefts(j)%bh)
             obuffer(jb)%k = kv_int
          enddo
       endif
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

    integer nbuf
    integer bufh(0:pop-1)
    integer bufj(0:pop-1)
    integer pstk(0:pop-1)

    ierr = 0

    nbuf = 0
    do jinp = 0, pop - 1
       js = mstack - pop + jinp
       hb = bstack(js)%bh
       jb = buf_h2item(hb)
       if (jb.ge.0) then
          bufj(nbuf) = jb
          bufh(nbuf) = hb
          pstk(nbuf) = js
          nbuf = nbuf + 1
       endif
    enddo

    select case(cmode)
    case (cmode_null)
       call flush_buffer_each(ierr, nbuf, bufh, bufj, pstk, u)
    case default
       call flush_buffer_horizontally(ierr, nbuf, bufh, bufj, pstk, cmode, u)
    end select
  end subroutine flush_stack

!!!_   . flush_buffer_each - flush out buffers for each
  subroutine flush_buffer_each(ierr, nbuf, bufh, bufj, pstk, u)
    use TOUZA_Std,only: choice
    use TOUZA_Nio,only: show_header, parse_header_size
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: nbuf
    integer,intent(in)          :: bufh(0:*)
    integer,intent(in)          :: bufj(0:*)
    integer,intent(in)          :: pstk(0:*)   ! corresponding stack index for buffer
    integer,intent(in),optional :: u

    integer utmp
    integer jb, hb
    integer js

    integer jbuf
    character(len=64) :: lcstr, pcstr, dstr
    character(len=64) :: fmt_nline
    character(len=64) :: fmt_uline
    character(len=64) :: fmt_xline

    type(domain_t) :: doml, domr(1)
    type(buffer_t) :: htmp
    integer mco
    integer jl, jp
    integer lidx(0:lcoor-1), pidx(0:lcoor-1)
    integer nb
    integer btmp(1), ptmp(1)
    character(len=256) :: cjoin, val

    ierr = 0
    utmp = choice(ulog, u)

212 format('## stack[', I0, '] ', A, 1x, A)

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
       js = pstk(jbuf)
       nb = 1
       btmp(1) = hb
       ptmp(1) = js

       if (ierr.eq.0) then
          call get_compromise_domain(ierr, domL, domR, btmp, ptmp, nb, cmode_inclusive, htmp)
       endif
       if (ierr.eq.0) call get_obj_string(ierr, val, hb)

       if (is_msglev(lev_verbose, msglev_normal+1)) then
          if (ierr.eq.0) write(utmp, 212) user_index_bgn(jbuf), trim(val), trim(obuffer(jb)%desc)
          if (ierr.eq.0) call get_domain_string(ierr, lcstr, bstack(js)%lcp)
          if (ierr.eq.0) call get_domain_string(ierr, pcstr, obuffer(jb)%pcp)
          if (ierr.eq.0) then
             call get_domain_shape(ierr, dstr, domR(1), obuffer(jb)%pcp, bstack(js)%lcp, domL)
          endif
          if (ierr.eq.0) write(utmp, 231) trim(pcstr)
          if (ierr.eq.0) write(utmp, 232) trim(lcstr)
          if (ierr.eq.0) write(utmp, 233) trim(dstr)
       endif
       if (ierr.eq.0) call get_domain_result(ierr, cjoin, domL, htmp%pcp)
       if (ierr.eq.0) write(utmp, 211) trim(cjoin), trim(val)
       ! if (is_msglev_DEBUG(lev_verbose)) then
       !    if (ierr.eq.0) call show_domain(ierr, doml, 'left')
       !    if (ierr.eq.0) call show_domain(ierr, domr(1), 'right')
       ! endif
       if (ierr.eq.0) then
          if (dryrun.gt.0) then
             write(utmp, *)
          else
             mco = doml%mco
             if (mco.gt.0) then
                write(fmt_xline, 202) mco
                write(fmt_uline, 203) mco
             else
                write(fmt_xline, 222)
                write(fmt_uline, 223)
             endif
             lidx(0:mco-1) = 0
             doml%bgn(0:mco-1) = user_index_bgn(doml%bgn(0:mco-1))

             select case(obuffer(jb)%k)
             case (kv_int)
                if (mco.gt.0) then
                   write(fmt_nline, 201) mco, trim(afmt_int)
                else
                   write(fmt_nline, 221) trim(afmt_int)
                endif
                do jl = 0, doml%n - 1
                   jp = physical_index(lidx, domr(1))
                   pidx(0:mco-1) = lidx(0:mco-1) + doml%bgn(0:mco-1)
                   if (jp.lt.0) then
                      write(utmp, fmt_xline) pidx(0:mco-1)
                   else if (obuffer(jb)%vd(jp).eq.obuffer(jb)%undef) then
                      write(utmp, fmt_uline) pidx(0:mco-1)
                   else
                      write(utmp, fmt_nline) pidx(0:mco-1), INT(obuffer(jb)%vd(jp))
                   endif
                   call incr_logical_index(lidx, doml)
                enddo
             case (kv_flt)
                if (mco.gt.0) then
                   write(fmt_nline, 201) mco, trim(afmt_flt)
                else
                   write(fmt_nline, 221) trim(afmt_flt)
                endif
                do jl = 0, doml%n - 1
                   jp = physical_index(lidx, domr(1))
                   pidx(0:mco-1) = lidx(0:mco-1) + doml%bgn(0:mco-1)
                   if (jp.lt.0) then
                      write(utmp, fmt_xline) pidx(0:mco-1)
                   else if (obuffer(jb)%vd(jp).eq.obuffer(jb)%undef) then
                      write(utmp, fmt_uline) pidx(0:mco-1)
                   else
                      write(utmp, fmt_nline) pidx(0:mco-1), REAL(obuffer(jb)%vd(jp), kind=KFLT)
                   endif
                   call incr_logical_index(lidx, doml)
                enddo
             case (kv_dbl)
                if (mco.gt.0) then
                   write(fmt_nline, 201) mco, trim(afmt_dbl)
                else
                   write(fmt_nline, 221) trim(afmt_dbl)
                endif
                do jl = 0, doml%n - 1
                   jp = physical_index(lidx, domr(1))
                   pidx(0:mco-1) = lidx(0:mco-1) + doml%bgn(0:mco-1)
                   if (jp.lt.0) then
                      write(utmp, fmt_xline) pidx(0:mco-1)
                   else if (obuffer(jb)%vd(jp).eq.obuffer(jb)%undef) then
                      write(utmp, fmt_uline) pidx(0:mco-1)
                   else
                      write(utmp, fmt_nline) pidx(0:mco-1), REAL(obuffer(jb)%vd(jp), kind=KDBL)
                   endif
                   call incr_logical_index(lidx, doml)
                enddo
             end select
          endif
       endif
    enddo
  end subroutine flush_buffer_each

!!!_   . flush_buffer_horizontally - flush out buffers with horizontally pasting
  subroutine flush_buffer_horizontally(ierr, nbuf, bufh, bufj, pstk, cmode, u)
    use TOUZA_Std,only: choice, join_list
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: nbuf
    integer,intent(in)          :: bufh(0:*)
    integer,intent(in)          :: bufj(0:*)
    integer,intent(in)          :: pstk(0:*)   ! corresponding stack index for buffer
    integer,intent(in)          :: cmode
    integer,intent(in),optional :: u

    integer utmp

    type(domain_t) :: doml
    type(domain_t) :: domr(0:nbuf-1)

    integer j, jb, hb
    integer js
    integer mco
    integer jl, jp
    integer lidx(0:lcoor-1), pidx(0:lcoor-1)
    character(len=64) :: vals(0:nbuf-1)
    character(len=64) :: fmt_xline
    character(len=64) :: name
    character(len=256) :: dstr
    character(len=256) :: cjoin, vjoin
    type(buffer_t) :: htmp

    ierr = 0
    utmp = choice(ulog, u)

    if (ierr.eq.0) call get_compromise_domain(ierr, doml, domr, bufh, pstk, nbuf, cmode, htmp)

    do j = 0, nbuf - 1
       jb = bufj(j)
       hb = bufh(j)
       js = pstk(j)
202    format('## ', I0, 1x, A, 1x, A, 1x, A)
       if (ierr.eq.0) call get_obj_string(ierr, name, hb)
       if (ierr.eq.0) then
          call get_domain_shape(ierr, dstr, domr(j), obuffer(jb)%pcp, bstack(js)%lcp, doml)
       endif
       if (ierr.eq.0) vals(j) = name
       if (ierr.eq.0) then
          write(utmp, 202) user_index_bgn(j), trim(name), &
               & trim(dstr), trim(obuffer(jb)%desc)
       endif

    enddo
    if (ierr.eq.0) call get_domain_result(ierr, cjoin, doml, htmp%pcp)

    if (ierr.eq.0) then
       mco = doml%mco
201    format('(', I0, '(I0, 1x), ', I0, '(A, 1x))')
221    format('(', I0, '(A, 1x))')
       if (mco.gt.0) then
          write(fmt_xline, 201) mco, nbuf
       else
          write(fmt_xline, 221) nbuf
       endif
       lidx(0:mco-1) = 0
       doml%bgn(0:mco-1) = user_index_bgn(doml%bgn(0:mco-1))
       if (ierr.eq.0) call join_list(ierr, vjoin, vals(0:nbuf-1))
211    format('#', A, 1x, A)
       write(utmp, 211) trim(cjoin), trim(vjoin)
    endif
    if (ierr.eq.0) then
       if (dryrun.gt.0) then
          write(utmp, *)
       else
          do jl = 0, doml%n - 1
             pidx(0:mco-1) = lidx(0:mco-1) + doml%bgn(0:mco-1)
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
                      write(vals(j), afmt_int) INT(obuffer(jb)%vd(jp))
                   case (kv_flt)
                      write(vals(j), afmt_flt) REAL(obuffer(jb)%vd(jp), kind=KFLT)
                   case (kv_dbl)
                      write(vals(j), afmt_dbl) REAL(obuffer(jb)%vd(jp), kind=KDBL)
                   case default
                      vals(j) = '*'
                   end select
                endif
             enddo
             write(utmp, fmt_xline) pidx(0:mco-1), (trim(vals(j)),j=0,nbuf-1)
             call incr_logical_index(lidx, doml)
          enddo
       endif
    endif

  end subroutine flush_buffer_horizontally

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
       ptmp(1) = js
       nb = 1
       if (ierr.eq.0) call tweak_coordinates(ierr, domL, domR, btmp, ptmp, nb, buf)
       if (ierr.eq.0) call set_inclusive_domain(ierr, domL, domR, btmp, ptmp, nb)
       if (ierr.eq.0) call settle_output_domain(ierr, domL)
       if (ierr.eq.0) call set_output_buffer(ierr, buf, btmp, domL)
       ! write(*, *) buf%pcp(:)%bgn
       ! write(*, *) buf%pcp(:)%end
       ! write(*, *) buf%pcp(:)%stp
    endif
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
          obuffer(jb)%pcp(0:domL%mco-1)%stp = 0
          obuffer(jb)%pcp(domL%mco:)%stp = -1

          obuffer(jb)%pcp(jotgt)%bgn = b
          obuffer(jb)%pcp(jotgt)%end = e
          obuffer(jb)%pcp(jotgt)%stp = 1
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
          ! write(*, *) jb, obuffer(jb)%pcp(:)%bgn
          ! write(*, *) jb, obuffer(jb)%pcp(:)%end
          ! write(*, *) jb, obuffer(jb)%pcp(:)%stp
       enddo
    endif
  end subroutine apply_INDEX

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

    integer,parameter :: nb = 1
    integer btmp(nb), ptmp(nb)
    type(domain_t) :: domL, domR(nb)

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
       ptmp = mstack - ninp + jout
       if (ierr.eq.0) then
          hbL = lefts(jout)%bh
          hbR = bufi(jinp)
          jbL = buf_h2item(hbL)
          jbR = buf_h2item(hbR)
          fillR = obuffer(jbR)%undef
          btmp = hbR
          if (check.and. (fillR.eq.TRUE.or.fillR.eq.FALSE)) then
             ierr = ERR_PANIC
             call message(ierr, 'MISS value cannot be 1 nor 0')
          endif
       endif

       if (ierr.eq.0) call copy_set_header(ierr, lefts(jout)%bh, bufi(jinp), 1)

       if (ierr.eq.0) call tweak_coordinates(ierr, domL, domR, btmp, ptmp, nb, obuffer(jbL))
       if (ierr.eq.0) call set_inclusive_domain(ierr, domL, domR, btmp, ptmp, nb)
       if (ierr.eq.0) call settle_output_domain(ierr, domL)
       if (ierr.eq.0) call settle_input_domain(ierr, domR(1), btmp(1), ptmp(1), domL)
       if (ierr.eq.0) call set_output_buffer_h(ierr, lefts(jout)%bh, btmp, domL)
       ! if (is_msglev_DEBUG(lev_verbose)) then
       !    if (ierr.eq.0) call show_domain(ierr, domL)
       ! endif
       ! write(*, *) obuffer(jbR)%vd
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
       & (ierr, hopr, lefts, bufi, cmode, func, neutral)
    use TOUZA_std,only: choice, jot
    implicit none
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: hopr
    type(stack_t),  intent(inout)       :: lefts(0:)
    integer,        intent(in)          :: bufi(0:)
    integer,        intent(in)          :: cmode
    real(kind=KBUF),intent(in),optional :: neutral
    interface
       subroutine func &
            & (ierr, Z, domZ, FZ, X, domX, FX)
         use chak_lib,only: KFLT, KDBL, domain_t
         implicit none
         integer,          intent(out) :: ierr
         real(kind=__KBUF),intent(out) :: Z(0:*)
         real(kind=__KBUF),intent(in)  :: X(0:*)
         type(domain_t),   intent(in)  :: domZ, domX
         real(kind=__KBUF),intent(in)  :: FZ,   FX
       end subroutine func
    end interface

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
    if (nopr.lt.2) ierr = ERR_INVALID_PARAMETER
    if (ierr.ne.0) then
       call message(ierr, 'invalid operands for binary operation.')
       return
    endif

    do jout = nout - 1 , 0, -1
       jinp = jout * nopr + ofsi

       hbL = lefts(jout)%bh
       jbL = buf_h2item(hbL)

       if (ierr.eq.0) call copy_set_header(ierr, lefts(jout)%bh, bufi(jinp), nopr)
       if (ierr.eq.0) then
          call get_compromise_domain &
               & (ierr, domL, domR, bufi(jinp:jinp+nopr-1), pstk(jinp:jinp+nopr-1), nopr, cmode, obuffer(jbL))
       endif
       if (ierr.eq.0) then
          hbR = bufi(jinp)
          jbR = buf_h2item(hbR)
          fillL = obuffer(jbR)%undef
          fillR = choice(fillL, neutral)
       endif
       if (ierr.eq.0) then
          jset = 0
          call apply_COPY &
               & (ierr, &
               &  obuffer(jbL)%vd, domL, &
               &  obuffer(jbR)%vd, domR(jset), fillR)
       endif
       if (ierr.eq.0) then
          do jj = jinp + 1, jinp + nopr - 1
             jset = jj - jinp
             hbR = bufi(jj)
             jbR = buf_h2item(hbR)
             fillR = obuffer(jbR)%undef
             call func &
                  & (ierr, &
                  &  obuffer(jbL)%vd, domL,       fillL, &
                  &  obuffer(jbR)%vd, domR(jset), fillR)
          enddo
       endif
       if (ierr.eq.0) then
          call set_binary_descr(ierr, lefts(jout)%bh, bufi(jinp:jinp+nopr-1), hopr)
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
         integer,          intent(out) :: ierr
         real(kind=__KBUF),intent(out) :: Z(0:*)
         real(kind=__KBUF),intent(in)  :: X(0:*)
         type(domain_t),   intent(in)  :: domZ, domX
         real(kind=__KBUF),intent(in)  :: FZ,   FX
       end subroutine func
    end interface

    integer nopr
    integer jout, nout
    integer jinp, ninp, jj
    integer hbL, hbR
    integer jbL, jbR
    logical check
    integer ofsi
    integer jset
    type(domain_t) :: domL
    type(domain_t) :: domR(0:size(bufi)-1)
    integer        :: pstk(0:size(bufi)-1)

    real(kind=KBUF) :: fillL, fillR

    ierr = 0
    check = choice(.FALSE., bin)

    ninp = size(bufi)
    ofsi = 0
    if (is_anchor(bufi(ofsi))) then
       ninp = ninp - 1
       ofsi = ofsi + 1
    endif
    nout = size(lefts)
    nopr = ninp / nout
    call jot(pstk, ninp, e=mstack)

    ! Reduce every (ninp / nout) buffer to nout.
    ! i.e., error if (ninp % nout) != 0 or (ninp / nout) < 2

    if (mod(ninp, nout).ne.0) ierr = ERR_INVALID_PARAMETER
    if (nopr.lt.2) ierr = ERR_INVALID_PARAMETER
    if (ierr.ne.0) then
       call message(ierr, 'invalid operands for binary operation.')
       return
    endif

    do jout = nout - 1 , 0, -1
       jinp = jout * nopr + ofsi

       hbL = lefts(jout)%bh
       jbL = buf_h2item(hbL)

       if (ierr.eq.0) call copy_set_header(ierr, hbL, bufi(jinp), nopr)
       if (ierr.eq.0) then
          call get_compromise_domain &
               & (ierr, domL, domR, bufi(jinp:jinp+nopr-1), pstk(jinp:jinp+nopr-1), nopr, cmode, obuffer(jbL))
       endif
       if (ierr.eq.0) then
          hbR = bufi(jinp)
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
          do jj = jinp + 1, jinp + nopr - 1
             jset = jj - jinp
             hbR = bufi(jj)
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
          call set_binary_descr(ierr, lefts(jout)%bh, bufi(jinp:jinp+nopr-1), hopr)
       endif
    enddo
  end subroutine apply_opr_BINARY

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
             write(obuffer(jbl)%desc2, 101) trim(istr), trim(obuffer(jbr)%desc2)
          else if (ilevo.eq.ilev_neg) then
             if (ilevi.ge.ilevo) then
                write(obuffer(jbl)%desc2, 112) trim(istr), trim(obuffer(jbr)%desc2)
             else
                write(obuffer(jbl)%desc2, 111) trim(istr), trim(obuffer(jbr)%desc2)
             endif
          else
             write(obuffer(jbl)%desc2, 121) trim(istr), trim(obuffer(jbr)%desc2)
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
  subroutine get_compromise_domain(ierr, domL, domR, bufh, pstk, nbuf, cmode, bufo)
    use TOUZA_Std,only: choice, find_first
    implicit none
    integer,       intent(out)            :: ierr
    type(domain_t),intent(inout)          :: domL
    type(domain_t),intent(inout)          :: domR(0:*)
    integer,       intent(in)             :: bufh(0:*)
    integer,       intent(in)             :: pstk(0:*)   ! corresponding stack index for buffer
    integer,       intent(in)             :: nbuf
    integer,       intent(in)             :: cmode
    type(buffer_t),intent(inout),optional :: bufo
    integer j
    integer nceff

    ierr = 0

    if (ierr.eq.0) call tweak_coordinates(ierr, domL, domR, bufh, pstk, nbuf, bufo)
    if (ierr.eq.0) call set_inclusive_domain(ierr, domL, domR, bufh, pstk, nbuf)
    if (ierr.eq.0) then
       nceff = domL%mco
       select case(cmode)
       case (cmode_first)
          call set_inclusive_domain(ierr, domL, domR, bufh, pstk, 1, intersect=.TRUE.)
       case (cmode_intersect)
          call set_inclusive_domain(ierr, domL, domR, bufh, pstk, nbuf, intersect=.TRUE.)
       end select
    endif
    if (ierr.eq.0) call settle_output_domain(ierr, domL)

    do j = 0, nbuf - 1
       if (ierr.eq.0) call settle_input_domain(ierr, domR(j), bufh(j), pstk(j), domL)
    enddo
    if (present(bufo)) then
       if (ierr.eq.0) then
          call set_output_buffer(ierr, bufo, bufh(0:nbuf-1), domL)
       endif
    endif

    if (is_msglev_DEBUG(lev_verbose)) then
       if (ierr.eq.0) call show_domain(ierr, domL, 'compromise/L')
       do j = 0, nbuf - 1
          if (ierr.eq.0) call show_domain(ierr, domR(j), 'compromise/R')
       enddo
    endif
  end subroutine get_compromise_domain

!!!_   . tweak_coordinates
  subroutine tweak_coordinates &
       & (ierr, domL, domR, bufh, pstk, nbuf, bufo)
    use TOUZA_Std,only: choice, find_first
    implicit none
    integer,       intent(out)            :: ierr
    type(domain_t),intent(inout)          :: domL
    type(domain_t),intent(inout)          :: domR(0:*)
    integer,       intent(in)             :: bufh(0:*)
    integer,       intent(in)             :: pstk(0:*)
    integer,       intent(in)             :: nbuf
    type(buffer_t),intent(inout),optional :: bufo       ! to store coordinate names only

    integer nceff
    character(len=lname) :: nameL(0:lcoor-1),  nameR(0:lcoor-1, 0:nbuf-1)
    integer j, jb, jc, jo, js
    integer jerr

    integer clidx(0:lcoor-1, 0:nbuf-1)
    integer cpidx(0:lcoor-1, 0:nbuf-1)
    integer ctype(0:lcoor-1, 0:nbuf-1)

    ierr = 0
    do j = 0, nbuf - 1
       jb = buf_h2item(bufh(j))
       js = pstk(j)
       if (ierr.eq.0) then
          call get_logical_shape &
               & (ierr, nameR(:,j), ctype(:,j), cpidx(:,j), bstack(js)%lcp, obuffer(jb)%pcp, lcoor)
       endif
    enddo
    if (ierr.eq.0) then
       call match_perms(ierr, nceff, nameL, clidx, ctype, nameR, lcoor, nbuf)
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
               & (jerr, domR, nameR, ctype, cpidx, bufh, pstk, lcoor, nbuf)
       endif
    endif

  end subroutine tweak_coordinates

!!!_   . get_logical_shape
  subroutine get_logical_shape &
       & (ierr, cname, ctype, cpidx, lcp, pcp, mco)
    use TOUZA_Std,only: find_first, parse_number, begin_with
    implicit none
    integer,         intent(out) :: ierr
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

    ierr = 0
    tblp2l(:) = co_unset
    cpidx(0:mco-1) = co_unset
    cname(0:mco-1) = ' '
    ctype(0:mco-1) = co_unset
    ! count ranks
    nranks = 1
    do jco = mco - 1, 0, -1
       if (pcp(jco)%stp.ge.0.or.pcp(jco)%name.ne.' ') then
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

    if (ierr.eq.0) then
       nranks = 0
       ! adjust coordinate types
       do jodr = 0, mco - 1
          if (cpidx(jodr).ge.0) then
             ctype(jodr) = coordinate_type(cname(jodr), lcp(jodr), pcp(cpidx(jodr)))
          else
             ctype(jodr) = coordinate_type(cname(jodr), lcp(jodr))
          endif
          if (cpidx(jodr).ge.0.or.ctype(jodr).ne.co_null) nranks = jodr + 1
       enddo
    endif

  end subroutine get_logical_shape

!!!_   . match_perms
  subroutine match_perms &
       & (ierr,   nceff,  cnameL, clidxR, &
       &  ctypeR, cnameR, mco,    nbuf, ltbl)
    use TOUZA_Std,only: choice, find_first, find_first_range, join_list
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: mco
    integer,         intent(out) :: nceff
    character(len=*),intent(out) :: cnameL(0:*)
    integer,         intent(out) :: clidxR(0:mco-1, 0:*)
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
    enddo
    mref = maxval(meff(0:nbuf-1))
    ! write(*, *) 'perm', mref, '/', meff(0:nbuf-1)
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
       ! write(*, *) jbuf(0:nt-1)
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

302 format('topo:', A, ':', I0, 1x, A8, 1x, I0, ' >', 12(1x, I3))
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

!!!_   . set_inclusive_domain
  subroutine set_inclusive_domain &
       & (ierr, domL, domR, bufh, pstk, nbuf, intersect)
    use TOUZA_Std,only: choice, find_first
    implicit none
    integer,         intent(out)   :: ierr
    type(domain_t),  intent(inout) :: domL
    type(domain_t),  intent(in)    :: domR(0:*)
    integer,         intent(in)    :: bufh(0:*)
    integer,         intent(in)    :: pstk(0:*)
    integer,         intent(in)    :: nbuf
    logical,optional,intent(in)    :: intersect

    integer nceff
    integer j
    integer jb,    jo, js
    ! integer jphyc, jlogc
    integer b,   e,    s
    integer low, high, stp
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
          stp = -1
          do j = 0, nbuf - 1
             jb = buf_h2item(bufh(j))
             js = pstk(j)
             call get_logical_range(b, e, s, jo, bstack(js)%lcp, obuffer(jb)%pcp, domR(j))
             stp = max(stp, s)
             if (isi) then
                if (b.ne.null_range) low = max(low, b)
                if (e.ne.null_range) high = min(high, e)
             else
                if (b.ne.null_range) low = min(low, b)
                if (e.ne.null_range) high = max(high, e)
             endif
          enddo
          if (low.eq.lini) low = 0
          if (high.eq.hini) high = low + max(0, stp)
          domL%bgn(jo) = low
          domL%end(jo) = high
          ! write(*, *) 'inclusive', jo, domL%bgn(jo), domL%end(jo)
       enddo
    endif
    return
  end subroutine set_inclusive_domain

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
       ! write(*, *) 'output/b', dom%bgn(0:dom%mco-1)
       ! write(*, *) 'output/e', dom%end(0:dom%mco-1)
       ! write(*, *) 'output/s', dom%strd(0:dom%mco-1)
       ! write(*, *) 'output/i', dom%iter(0:dom%mco-1)
    endif
  end subroutine settle_output_domain

!!!_   . settle_input_domain
  subroutine settle_input_domain(ierr, dom, hbuf, jstk, ref)
    implicit none
    integer,       intent(out)   :: ierr
    type(domain_t),intent(inout) :: dom
    integer,       intent(in)    :: hbuf
    integer,       intent(in)    :: jstk
    type(domain_t),intent(in)    :: ref

    integer jo
    integer b,  e, s
    integer jb

    ierr = 0
    jb = buf_h2item(hbuf)

    do jo = 0, ref%mco - 1
       ! call set_logical_range(b, e, jo, bstack(jstk)%lcp, dom, ref)
       call get_logical_range &
            & (b, e, s, jo, bstack(jstk)%lcp, obuffer(jb)%pcp, dom, ref)
       dom%bgn(jo) = b
       dom%end(jo) = max(e, 1+b)
       dom%iter(jo) = max(1, e - b)
    enddo
    if (ierr.eq.0) then
       call settle_domain_stride(ierr, dom, obuffer(jb)%pcp)
    endif
    if (ierr.eq.0) then
       call settle_domain_loop_h(ierr, dom, hbuf, ref)
    endif

  end subroutine settle_input_domain

!!!_   . get_logical_range
  subroutine get_logical_range &
       & (b, e, s, jodr, lcp, pcp, dom, ref)
    integer,       intent(out)         :: b, e, s
    integer,       intent(in)          :: jodr
    type(loop_t),  intent(in)          :: lcp(0:*)
    type(loop_t),  intent(in)          :: pcp(0:*)
    type(domain_t),intent(in)          :: dom
    type(domain_t),intent(in),optional :: ref

    integer low, high
    integer jlogc, jphyc

    if (present(ref)) then
       low  = ref%bgn(jodr)
       high = ref%end(jodr)
    else
       low = null_range
       high = null_range
    endif

    jlogc = dom%lidx(jodr)
    jphyc = dom%cidx(jodr)
    if (jlogc.ge.0) then
       b = logical_index(lcp(jlogc)%bgn, low)
       e = logical_index(lcp(jlogc)%end, high)
    else
       b = low
       e = high
    endif
    s = -1
    if (jphyc.ge.0) then
       ! if (pcp(jphyc)%stp.gt.0) then
       if (pcp(jphyc)%stp.ge.0) then
          if (b.eq.null_range) b = pcp(jphyc)%bgn
          if (e.eq.null_range) e = pcp(jphyc)%end
       endif
       s = pcp(jphyc)%stp
    endif

  end subroutine get_logical_range

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
             if (buf%pcp(kc)%stp.le.0) then
                dom%bgn(jc) = max(0, max(refd%bgn(jc), dom%bgn(jc)) - refd%bgn(jc))
                dom%end(jc) = max(0, min(refd%end(jc), dom%end(jc)) - refd%bgn(jc))
             else
                dom%bgn(jc) = max(refd%bgn(jc), dom%bgn(jc), buf%pcp(kc)%bgn) - refd%bgn(jc)
                dom%end(jc) = min(refd%end(jc), dom%end(jc), buf%pcp(kc)%end) - refd%bgn(jc)
             endif
             if (buf%pcp(kc)%bgn.eq.null_range) then
                dom%ofs(jc) = 0
             else
                dom%ofs(jc) = - (buf%pcp(kc)%bgn - refd%bgn(jc))
             endif
          else
             dom%bgn(jc) = max(0, max(refd%bgn(jc), dom%bgn(jc)) - refd%bgn(jc))
             dom%end(jc) = max(0, min(refd%end(jc), dom%end(jc)) - refd%bgn(jc))
             dom%ofs(jc) = 0
          endif
          dom%end(jc) = max(dom%bgn(jc), dom%end(jc))
          dom%iter(jc) = max(1, dom%end(jc) - dom%bgn(jc))
       enddo
    endif

  end subroutine settle_domain_loop

!!!_   . settle_domain_stride
  subroutine settle_domain_stride &
       & (ierr, dom, pcp)
    implicit none
    integer,       intent(out)   :: ierr
    type(domain_t),intent(inout) :: dom
    type(loop_t),  intent(in)    :: pcp(0:*)

    integer jo, jc, w
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
       strd(jc) = strd(jc-1) * max(1, w * max(0, pcp(jc-1)%stp))
    enddo
    do jo = 0, dom%mco - 1
       jc = dom%cidx(jo)
       if (jc.ge.0) then
          dom%strd(jo) = strd(jc) * max(0, pcp(jc)%stp)
       else
          dom%strd(jo) = 0
       endif
    enddo
  end subroutine settle_domain_stride

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
             buf%pcp(jc)%stp = 0
          else
             buf%pcp(jc)%bgn = dom%bgn(jc)
             buf%pcp(jc)%end = dom%end(jc)
             buf%pcp(jc)%stp = 1
          endif
       enddo
       do jc = dom%mco, lcoor - 1
          buf%pcp(jc) = def_loop
       enddo
       call alloc_buffer_t(ierr, buf, dom%n)
    endif
  end subroutine set_output_buffer

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

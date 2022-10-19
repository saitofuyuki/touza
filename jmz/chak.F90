!!!_! chak.F90 - TOUZA/Jmz swiss(CH) army knife
! Maintainer: SAITO Fuyuki
! Created: Nov 25 2021
#define TIME_STAMP 'Time-stamp: <2022/10/19 20:56:27 fuyuki chak.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
! #  include "touza_config.h"
! #  undef PACKAGE
! #  undef PACKAGE_NAME
! #  undef PACKAGE_STRING
! #  undef PACKAGE_TARNAME
! #  undef PACKAGE_VERSION
! #  undef VERSION
#  include "jmz_config.h"
#endif
#include "jmz.h"
!!!_* macros
#ifndef   TEST_CHAK
#  define TEST_CHAK 0
#endif
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
  use chak_lib
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

  character(len=*),parameter :: paramd = '='

  integer,parameter :: hedit_unset = -9
  integer,parameter :: hedit_sign = 0
  integer,parameter :: hedit_edit = 1
  integer,parameter :: hedit_title = 2
  integer,parameter :: hedit_item = 3
  integer,parameter :: hedit_all = 9

!!!_  - operators
  integer,parameter :: lopr = 512

  !! operation symbols
#include "chak_decl.F90"

  integer,parameter :: ilev_unset = 0
  integer,parameter :: ilev_term = 1
  integer,parameter :: ilev_call = 2
  integer,parameter :: ilev_neg = 3
  integer,parameter :: ilev_exp = 4
  integer,parameter :: ilev_mul = 5
  integer,parameter :: ilev_add = 6
  integer,parameter :: ilev_logical = 7

  type opr_t
     integer :: push = -1
     integer :: pop = -1
     integer :: entr = -1
     integer :: ilev = ilev_unset
     character(len=8) :: istr = ' '
  end type opr_t
  type(opr_t) :: oprop(0:lopr-1)
  integer :: mopr = 0

!!!_  - variables
  integer ierr
!!!_  - files
  integer,save      :: rcount = 0   ! read file count
  integer,save      :: wcount = 0   ! write file count
  integer,save      :: mfile=0
  integer,parameter :: lfile=OPT_CHAK_FILES
  integer,parameter :: lpath=OPT_PATH_LEN
  integer,parameter :: lname=litem*4
  integer,parameter :: ldesc=OPT_DESC_LEN
  integer,parameter :: lfmt =litem*4
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
     integer              :: hedit     ! header edit level
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

  integer,parameter :: full_range = + HUGE(0)              ! case low:
  integer,parameter :: null_range = (- HUGE(0)) - 1        ! case low
!!!_  - loop property
  type loop_t
     integer :: bgn = -1
     integer :: end = -1
     integer :: stp = -1
     character(len=lname) :: name
  end type loop_t
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
!!!_  - table
  integer,save :: htopr = -1
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
  integer :: global_offset_bgn = 0     ! begin-index offset (user-friendly)
  integer :: global_offset_end = 0     ! end-index offset (user-friendly)

  integer lev_verbose, dbgv, stdv

  character(len=32) :: afmt_int = '(I0)'
  character(len=32) :: afmt_flt = '(E16.9)'
  character(len=32) :: afmt_dbl = '(E16.9)'
!!!_  - coordinate matching
  integer,parameter :: co_unset = -4
  integer,parameter :: co_null  = -3
  integer,parameter :: co_float = -2
  integer,parameter :: co_wild  = -1

!!!_  - misc
  integer :: ulog = -1
  integer :: uerr = -1
  integer irecw

  character,parameter :: rename_sep = '/'

  type(loop_t),save :: def_loop  = loop_t(null_range, null_range, -1, ' ')
  type(loop_t),save :: zero_loop = loop_t(0, 0, 0, ' ')
!!!_ + Body
  ierr = 0

  lev_verbose = 0
  dbgv = -1
  stdv = -1
  mqueue = 0

  if (ierr.eq.0) call init(ierr, stdv, dbgv)
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
!!!_  - init
  subroutine init(ierr, stdv, dbgv)
    use TOUZA_Nio,only: nio_init=>init, nr_init
    use TOUZA_Std,only: env_init, MPI_COMM_NULL, stdout=>uout, stderr=>uerr
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: stdv, dbgv
    ierr = 0
    if (ierr.eq.0) call env_init(ierr, levv=stdv, icomm=MPI_COMM_NULL)
    if (ierr.eq.0) ulog = stdout
    if (ierr.eq.0) uerr = stderr
    if (ierr.eq.0) call register_operators(ierr)
    if (ierr.eq.0) call register_predefined(ierr)
    if (ierr.eq.0) call nio_init(ierr, levv=dbgv, stdv=stdv)
    if (ierr.eq.0) call nr_init(ierr, lazy=+1)

    if (ierr.eq.0) call reset_file(ierr, def_read,  ' ', mode_terminate)
    if (ierr.eq.0) call reset_file(ierr, def_write, ' ', mode_new)
! #if HAVE_FORTRAN_IEEE_ARITHMETIC
!     UNDEF = IEEE_VALUE(ZERO, IEEE_QUIET_NAN)
! #endif
  end subroutine init
!!!_  - finalize
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
       if (ierr.eq.0) call show_buffers(ierr, u)
    endif
    if (ierr.eq.0) call nio_diag(ierr, levv=dbgv)
    if (ierr.eq.0) call htb_diag(ierr, levv=dbgv)
    if (ierr.eq.0) call nio_finalize(ierr, levv=dbgv)
    if (ierr.eq.0) call htb_finalize(ierr, levv=dbgv)
    if (ierr.eq.0) call env_finalize(ierr, levv=dbgv)
  end subroutine finalize

!!!_  - message
  subroutine message(ierr, msg, iadd, fmt, levm, u, indent)
    use TOUZA_Std,only: choice, join_list
    implicit none
    integer,         intent(in)          :: ierr
    character(len=*),intent(in)          :: msg     ! msg
    integer,         intent(in),optional :: iadd(:)
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: levm    ! message level
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: indent
    integer jerr
    integer lv, utmp
    character(len=1024) :: txt
    integer skp
    jerr = 0
    lv = choice(0, levm)
    utmp = choice(ulog, u)
    skp = choice(0, indent)
    if (ierr.ne.0.or.is_msglev(lev_verbose, lv)) then
       if (present(iadd)) then
          if (size(iadd).gt.0) then
             if (present(fmt)) then
                write(txt, fmt) iadd(:)
             else
                call join_list(jerr, txt, iadd(:), ldelim='(', rdelim=')')
             endif
             txt = trim(msg) // ' ' // trim(txt)
          endif
       else
          txt = msg
       endif
102    format('error:', I0, ': ', A)
101    format(A, A)
       if (ierr.ne.0) then
          write(utmp, 102) ierr, trim(txt)
       else
          write(utmp, 101) repeat(' ', skp), trim(txt)
       endif
    endif
  end subroutine message
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

!!!_   . show_order
  subroutine show_order(ierr, name, order, maxco, tag, idx, u)
    use TOUZA_Std,only: choice, join_list
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: name(0:*)
    integer,         intent(in)          :: order(0:*)
    integer,         intent(in)          :: maxco
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: idx
    integer,         intent(in),optional :: u
    character(len=32) :: pfx
    character(len=256) :: txt
    character(len=32) :: cprop(0:maxco-1)
    integer jc
    integer utmp

    ierr = 0
    if (present(idx)) then
101    format('order[', A, '/', I0, ']')
102    format('order[', I0, ']')
103    format('order[', A, ']')
       if (present(tag)) then
          write(pfx, 101) trim(tag), idx
       else
          write(pfx, 102) idx
       endif
    else if (present(tag)) then
       write(pfx, 103) trim(tag)
    else
       pfx = 'order'
    endif
    do jc = 0, maxco - 1
111    format('{', A, '}=', I0)
112    format('{', A, '}=*')
113    format('{', A, '}=', I0)
       if (order(jc).ge.0) then
          if (name(jc).eq.' ') then
             write(cprop(jc), 111) '*', order(jc)
          else
             write(cprop(jc), 111) trim(name(jc)), order(jc)
          endif
       else if (order(jc).eq.co_float) then
          if (name(jc).eq.' ') then
             write(cprop(jc), 112) '*'
          else
             write(cprop(jc), 112) trim(name(jc))
          endif
       else if (order(jc).eq.co_null) then
          if (name(jc).eq.' ') then
             cprop(jc) = '{-}'
          else
             write(cprop(jc), 113) trim(name(jc)), order(jc)
          endif
       else
          write(cprop(jc), 113) trim(name(jc)), order(jc)
       endif
    enddo
    call join_list(ierr, txt, cprop(0:maxco-1))
    utmp = choice(ulog, u)
201 format(A, 1x, A)
    write(utmp, 201) trim(pfx), trim(txt)
  end subroutine show_order

!!!_  - operator table
!!!_   . register_operators
  subroutine register_operators(ierr)
    use TOUZA_Std,only: htb_init, new_htable, reg_entry
    implicit none
    integer,intent(out) :: ierr

    ierr = 0
    call htb_init(ierr)
    htopr = new_htable('operators', 0, nstt=1, mem=lopr)
    ierr = min(0, htopr)

#   include "chak_reg.F90"

  end subroutine register_operators
!!!_   . reg_opr_prop
  subroutine reg_opr_prop &
       & (ierr, idopr, str, pop, push, ilev, istr)
    use TOUZA_Std,only: choice, reg_entry, store_xstatus
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: idopr
    character(len=*),intent(in)          :: str
    integer,         intent(in),optional :: pop
    integer,         intent(in),optional :: push
    integer,         intent(in),optional :: ilev
    character(len=*),intent(in),optional :: istr
    integer entr
    if (idopr.ge.lopr.or.idopr.lt.0) then
       ierr = ERR_PANIC
       call message(ierr, 'panic in operator registration')
       return
    endif
    entr = reg_entry(str, htopr, idopr)
    ierr = min(0, entr)
    if (ierr.eq.0) then
       if (oprop(idopr)%entr.lt.0) then
          oprop(idopr)%entr = entr
          oprop(idopr)%push = choice(-1, push)
          oprop(idopr)%pop = choice(-1, pop)
          oprop(idopr)%ilev = choice(ilev_unset, ilev)
          if (present(istr)) then
             oprop(idopr)%istr = istr
          else
             oprop(idopr)%istr = ' '
          endif
       else
          ! alias
          continue
       endif
       mopr = max(mopr, idopr + 1)
    endif
  end subroutine reg_opr_prop

!!!_   . reg_fake_opr
  subroutine reg_fake_opr &
       & (ierr, handle, str)
    use TOUZA_Std,only: reg_entry, query_entry
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: handle
    character(len=*),intent(in)  :: str
    integer entr
    ierr = 0
    entr = query_entry(str, htopr)
    if (entr.ge.0) then
       ierr = ERR_DUPLICATE_SET
       call message(ierr, 'duplicate registration ' // trim(str))
    else
       entr = reg_entry(str, htopr, handle)
    endif
  end subroutine reg_fake_opr

!!!_   . parse_term_operator()
  integer function parse_term_operator(str) result(n)
    use TOUZA_Std,only: query_status
    implicit none
    character(len=*),intent(in) :: str
    integer jb
    n = query_status(str, htopr)
    if (n.lt.0) then
       jb = index(str, paramd)
       if (jb.gt.1) then
          n = query_status(str(1:jb-1), htopr)
       endif
    endif
  end function parse_term_operator
!!!_   . inquire_opr_nstack
  subroutine inquire_opr_nstack(ierr, pop, push, handle)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: pop, push
    integer,intent(in)  :: handle
    integer jb
    ierr = 0

    if (handle.lt.0.or.handle.ge.mopr) then
       jb = buf_h2item(handle)
       if (jb.lt.0) then
          ierr = ERR_INVALID_ITEM
          call message(ierr, 'invalid operator handle to inquire', (/handle/))
       else
          pop = 0
          push = 1
       endif
    else
       pop = oprop(handle)%pop
       push = oprop(handle)%push
    endif
  end subroutine inquire_opr_nstack

!!!_   . inquire_opr_infix
  subroutine inquire_opr_infix(ierr, ilev, istr, handle)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ilev
    character(len=*),intent(out) :: istr
    integer,         intent(in)  :: handle
    ierr = 0

    if (handle.lt.0.or.handle.ge.mopr) then
       ierr = ERR_INVALID_ITEM
       call message(ierr, 'invalid operator handle to inquire', (/handle/))
       ilev = ilev_unset
       istr = ' '
    else
       ilev = oprop(handle)%ilev
       istr = oprop(handle)%istr
       if (istr.eq.' ') then
          call query_opr_name(ierr, istr, handle)
       endif
    endif
  end subroutine inquire_opr_infix

!!!_   . is_operator_modify()
  logical function is_operator_modify(handle) result(b)
    implicit none
    integer,intent(in) :: handle
    b = handle.eq.opr_TRANSF
  end function is_operator_modify

!!!_   . is_operator_reusable()
  logical function is_operator_reusable(handle) result(b)
    implicit none
    integer,intent(in) :: handle
    integer pop, push
    integer jerr
    call inquire_opr_nstack(jerr, pop, push, handle)
    b = pop.eq.1 .and. push.eq.1
    ! b = .FALSE.
  end function is_operator_reusable

!!!_   . query_opr_name
  subroutine query_opr_name &
       & (ierr, str, handle)
    use TOUZA_Std,only: query_name
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(out)         :: str
    integer,         intent(in)          :: handle
    if (handle.lt.0.or.handle.ge.mopr) then
       ierr = ERR_INVALID_ITEM
       call message(ierr, 'invalid operator handle', (/handle/))
       return
    endif
    call query_name(ierr, str, oprop(handle)%entr, htopr)
  end subroutine query_opr_name

!!!_  - argument parser
!!!_   . parse_args
  subroutine parse_args(ierr)
    use TOUZA_Std,only: arg_init, arg_diag, parse, get_param
    implicit none
    integer,intent(out) :: ierr

    integer japos
    character(len=lpath) :: arg
    integer jerr

    ierr = 0

    mqueue = 0
    mfile = 0

    consts = mbuffer  ! save number of predefined constants here

    mstack = 0

    if (ierr.eq.0) call arg_init(ierr, cha=' = ')   ! disable assignment
    if (ierr.eq.0) call parse(ierr)
    if (ierr.eq.0) then
       japos = 0
       ! hprev = opr_null
       do
          japos = japos + 1
          call get_param(jerr, arg, japos)
          if (jerr.ne.0) exit
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
                call parse_option(ierr, japos, arg)
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
    if (ierr.eq.0) call set_rec_filter(ierr)
    if (ierr.eq.0) call set_write_format(ierr)
  end subroutine parse_args

!!!_   . parse_option
  subroutine parse_option &
       & (ierr, japos, arg)
    use TOUZA_Std,only: parse_number, get_param
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: japos
    character(len=*),intent(in)    :: arg
    character(len=lpath) :: abuf
    integer n
    integer jval, jvar

    integer md, cmd, hmd
    integer ntmp

    md = mode_unset
    cmd = mode_unset
    hmd = hedit_unset
    ierr = 0
    abuf = arg

    jvar = index(abuf, paramd)
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
       else if (abuf.eq.'-P') then
          call check_only_global(ierr, abuf)
          if (ierr.eq.0) then
             global_offset_bgn = 0
             global_offset_end = 0
          endif
       else if (abuf.eq.'-F') then
          call check_only_global(ierr, abuf)
          if (ierr.eq.0) then
             global_offset_bgn = 1
             global_offset_end = 0
          endif
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
    character(len=128) :: msg

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
       jpar = index(arg, paramd) + 1
       jend = len_trim(arg) + 1
       if (jpar.eq.1) jpar = jend + 1
       select case(hopr)
       case(opr_TAG)
          if (jpar.lt.jend) then
             obuffer(jb)%name = arg(jpar:jend-1)
             obuffer(jb)%stt  = stt_locked
             call reg_fake_opr(ierr, hbuf, obuffer(jb)%name)
          endif
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

!!!_   . parse_coordinate_opr
  subroutine parse_coordinate_opr (ierr, cidx, arg, hopr)
    use TOUZA_Std,only: split_list
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: cidx
    character(len=*),intent(in)  :: arg
    integer,         intent(in)  :: hopr
    integer lasth
    character,parameter :: nsep=','
    character,parameter :: rsep=':'
    integer jq
    integer jp, larg
    integer rpos(2)
    integer,parameter :: rdef(2) = (/null_range, null_range/)
    integer nc
    integer pop, push
    integer stp

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
    ! OPR=NAME[/RENAME]
    ! OPR=RANGE
    ! OPR=NAME[/RENAME],[RANGE]
    larg = len_trim(arg)

    if (ierr.eq.0) then
       jq = mqueue - 1
       jp = index(arg, nsep)
       if (jp.eq.0) then
          call split_list(nc, rpos, arg, rsep, 2, rdef(:))
          if (nc.lt.0) jp = larg + 1
       endif
       if (jp.eq.1) then
          aqueue(jq)%lefts(:)%lcp(cidx)%name = ' '
       else if (jp.gt.1) then
          aqueue(jq)%lefts(:)%lcp(cidx)%name = adjustl(arg(1:jp-1))
       endif
       jp = jp + 1
       rpos(1) = system_index_bgn(null_range)
       rpos(2) = system_index_end(null_range)
       stp = -1
       if (jp.le.larg) then
          call split_list(nc, rpos, arg(jp:), rsep, 2, rdef)
          if (nc.lt.0) then
             ierr = nc
             call message(ierr, 'cannot parse coordinate operation (forget comma?): ' // trim(arg))
          else if (nc.eq.0) then
             continue
          else if (nc.eq.1) then
             rpos(1) = system_index_bgn(rpos(1))
             rpos(2) = rpos(1) + 1
             stp = 0
          else if (nc.eq.2) then
             stp = 0
          else if (nc.gt.2) then
             ierr = ERR_INVALID_PARAMETER
             call message(ierr, 'fail to extract coordinate ' // trim(arg))
          endif
       endif
       if (ierr.eq.0) then
          aqueue(jq)%lefts(:)%lcp(cidx)%bgn = system_index_bgn(rpos(1))
          aqueue(jq)%lefts(:)%lcp(cidx)%end = system_index_end(rpos(2))
          aqueue(jq)%lefts(:)%lcp(cidx)%stp = stp
       endif
       if (ierr.eq.0) then
          bstack(mstack-pop:mstack-1)%lcp(cidx)%bgn = system_index_bgn(rpos(1))
          bstack(mstack-pop:mstack-1)%lcp(cidx)%end = system_index_end(rpos(2))
          bstack(mstack-pop:mstack-1)%lcp(cidx)%stp = stp
       endif
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
    use TOUZA_Nio,only: put_item
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: hopr
    character(len=*),intent(in)  :: arg
    integer jpar, jend
    integer hbuf, jf
    integer jerr
    integer jitem

    ierr = 0
    call last_queue(jerr, hbuf)
    if (jerr.eq.0) then
       jf = file_h2item(hbuf)
    else
       jf = -1
    endif

    if (ierr.eq.0) then
       jpar = index(arg, paramd) + 1
       jend = len_trim(arg) + 1
       if (jpar.eq.1) jpar = jend
       select case(hopr)
       case(opr_DFMT)
          if (jf.ge.0) then
             if (is_read_mode(ofile(jf)%mode)) jf = -1
          endif
          if (mfile.eq.0) then
             def_write%fmt = trim(arg(jpar:))
          else if (jf.ge.0) then
             ofile(jf)%fmt = trim(arg(jpar:))
          else
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'no file to set format ' // trim(arg))
          endif
       case(opr_TSEL)
          if (mfile.eq.0) then
             call parse_rec_filter(ierr, def_read, arg(jpar:))
          else if (is_read_buffer(hbuf)) then
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
       case default
          ierr = ERR_NOT_IMPLEMENTED
          call message(ierr, 'reserved operator ' // trim(arg))
       end select
    endif
    return
  end subroutine parse_header_opr

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

!!!_   . stack_COPY - non-cumulative COPY
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
       npop = 1
       npush = 2
       if (ierr.eq.0) call pop_stack(ierr, copyh(1), keep=.TRUE., anchor=.FALSE.)
       if (ierr.eq.0) call search_free_buffer(ierr, copyh(2:2), 1)
       if (ierr.eq.0) call push_stack(ierr, copyh(2))
       if (ierr.eq.0) call append_queue(ierr, hopr, npop, npush, copyh)
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
    if (ierr.eq.0) call get_obj_list_st(ierr, aq%desco, aq%lefts, push)
    if (ierr.eq.0) call get_obj_list_st(ierr, aq%desci, bstack(mstack-pop:mstack-1), pop)
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
    use TOUZA_Std,only: join_list, choice
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
    use TOUZA_Std,only: join_list, choice
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
    use TOUZA_Std,only: join_list, choice
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
    jb = buf_h2item(handle)
    if (jb.ge.0) then
       n = 1
       do jc = 0, lcoor - 1
          n = n * max(1, obuffer(jb)%pcp(jc)%end - obuffer(jb)%pcp(jc)%bgn)
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
    if (ierr.eq.0) call new_buffer_literal(ierr, handle, kv_dbl, ATAN2(ZERO, -ONE), 'PI', 'PI')
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
  subroutine reset_file(ierr, file, name, mode)
    use TOUZA_Std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    type(file_t),    intent(inout)       :: file
    character(len=*),intent(in),optional :: name
    integer,         intent(in),optional :: mode

    ierr = 0

    file%u = -1
    file%irec = 0
    file%nrec = -1
    file%h = ' '
    file%fmt = ' '
    file%mode = choice(mode_unset, mode)
    file%hedit = hedit_all
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

    if (ierr.eq.0) then
       if (file%u.lt.0) then
          if (file%mode.eq.mode_read) file%mode = def_read%mode
          file%u = new_unit()
          ierr = min(0, file%u)
          if (ierr.eq.0) then
             call sus_open(ierr, file%u, file%name, ACTION='R', STATUS='O')
             if (ierr.ne.0) then
                call message(ierr, 'failed to read open:'// trim(file%name))
                return
             endif
          endif
          file%irec = 0
          call init_read_rec(ierr, xrec, file)
       else
          call next_read_rec(ierr, xrec, file)
       endif
    endif
    ! cueing
    if (ierr.eq.0) call cue_read_header(ierr, head, file, xrec)
    if (ierr.eq.ERR_EXHAUST) then
       ierr = 0
       neof = neof + 1
       if (file%mode.eq.mode_terminate) nterm = nterm + 1
       return
    endif
    if (is_error_match(ierr, ERR_EOF)) return
    if (ierr.eq.0) call banner_record(ierr)
    if (ierr.eq.0) then
       file%h(:) = head(:)
       n = parse_header_size(file%h, 0, lazy=1)
       obuffer(jb)%k = suggest_type(file%h)
       if (ierr.eq.0) call get_item(ierr, file%h, obuffer(jb)%undef, hi_MISS, def=UNDEF)
    endif
    if (ierr.eq.0) call alloc_buffer_t(ierr, obuffer(jb), n)
    if (ierr.eq.0) then
       call nio_read_data(ierr, obuffer(jb)%vd, n, file%h, file%t, file%u)
       if (ierr.eq.0) file%irec = file%irec + 1
    endif

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
             obuffer(jb)%desc2 = '[' // adjustl(trim(obuffer(jb)%desc)) // ']'
          endif
       endif
       obuffer(jb)%ilev = ilev_term

       obuffer(jb)%reff = handle
    endif
    if (ierr.eq.0) call get_header_lprops(ierr, obuffer(jb)%pcp, file%h)

    if (ierr.eq.0) then
       call get_item(jerr, file%h, dt(:), hi_DATE)
       if (jerr.ne.0) dt(:) = -1
       call restore_item(jerr, file%h, tstr, hi_TIME)
       if (jerr.ne.0) tstr = ' '
    endif
101 format('  read:', A, 1x, A, ' T = ', A, ' DATE = ', I0, '/', I0, '/', I0, 1x, I2.2, ':', I2.2, ':', I2.2)
    write(txt, 101) trim(obuffer(jb)%name), trim(obuffer(jb)%desc), trim(adjustl(tstr)), dt(:)
    call message(ierr, txt, levm=msglev_normal)

    return
  end subroutine read_file

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

!!!_   . write_file
  subroutine write_file(ierr, file, jstk, levv)
    use TOUZA_Std,only: new_unit, sus_open, is_error_match
    use TOUZA_Std,only: get_login_name
    use TOUZA_Nio,only: nio_write_header, parse_header_size, nio_write_data
    use TOUZA_Nio,only: get_default_header, show_header, parse_record_fmt
    use TOUZA_Nio,only: REC_DEFAULT, REC_BIG
    use TOUZA_Nio,only: put_item, get_item, restore_item, store_item
    use TOUZA_Nio,only: hi_MISS, hi_ITEM, hi_DFMT,  hi_EDIT1, hi_TITL1, hi_ETTL1
    use TOUZA_Nio,only: hi_DATE, hi_TIME, hi_MSIGN, hi_MDATE
    use TOUZA_Nio,only: GFMT_UR4, GFMT_MR4, GFMT_UI4, GFMT_MI4
    use TOUZA_Nio,only: fill_header
    implicit none
    integer,     intent(out)         :: ierr
    type(file_t),intent(inout)       :: file
    integer,     intent(in)          :: jstk
    integer,     intent(in),optional :: levv
    integer n
    integer jb, jrefh
    character(len=2) stt, pos
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

    ierr = 0
    jb = 0
    if (ierr.eq.0) then
       if (file%bh.lt.0) ierr = ERR_PANIC
    endif
    if (ierr.eq.0) jb = buf_h2item(file%bh)

    head(:) = ' '
    if (ierr.eq.0) then
       jrefh = file_h2item(obuffer(jb)%reff)
       if (jrefh.ge.0) then
          head(:) = ofile(jrefh)%h(:)
       else
          call get_default_header(head)
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

    if (ierr.eq.0) then
       undef = obuffer(jb)%undef
       if (ABS(undef).eq.ULIMIT) then
          call store_item(ierr, head, ' ', hi_MISS)
       else
          call put_item(ierr, head, undef, hi_MISS)
       endif
    endif

    if (ierr.eq.0) then
       if (file%hedit.ge.hedit_sign) then
          call get_login_name(jerr, tstr)
          if (jerr.eq.0) call put_item(ierr, head, tstr, hi_MSIGN)
          call date_and_time(values=idt(:))
          idt(4:6) = idt(5:7)
          call put_item(jerr, head, idt(1:6), hi_MDATE)
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
       is_tweak = ANY(bstack(jstk)%lcp(:)%stp.ge.0)
       if (.not.is_tweak) then
          is_tweak = ANY(bstack(jstk)%lcp(:)%name.ne.' ')
       endif
       if (is_tweak) then
          call tweak_buffer(ierr, btmp, file%bh, jstk)
          if (ierr.eq.0) call put_header_lprops(ierr, head, btmp%pcp)
       else
          call put_header_lprops(ierr, head, obuffer(jb)%pcp)
       endif
    endif
    if (ierr.eq.0) call fill_header(ierr, head, file%h, 1)

    call message(ierr, 'put_item', levm=-9)

    if (ierr.eq.0) then
       if (file%u.lt.0) then
          file%u = new_unit()
          ierr = min(0, file%u)
          if (ierr.eq.0) then
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
             if (ierr.ne.0) then
                write(*, *) 'failed to write open:', trim(file%name)
                return
             endif
          endif
          file%irec = 0
       endif
    endif

    if (ierr.eq.0) then
       ! file%t = REC_DEFAULT
       file%t = REC_BIG
       call nio_write_header(ierr, head, file%t, file%u)
       if (ierr.eq.0) file%irec = file%irec + 1
       call message(ierr, 'write_header', levm=-9)
    endif
    if (ierr.eq.0) then
       n = parse_header_size(head, 0, lazy=1)
    endif
    if (ierr.eq.0) then
       if (is_tweak) then
          call nio_write_data(ierr, btmp%vd, n, head, file%t, file%u)
       else
          call nio_write_data(ierr, obuffer(jb)%vd, n, head, file%t, file%u)
       endif
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
    call message(ierr, txt, levm=msglev_normal)

    if (is_tweak) then
       if (ierr.eq.0) deallocate(btmp%vd, STAT=ierr)
    endif

    if (ierr.ne.0) call show_header(ierr, head)
    return
  end subroutine write_file

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
  subroutine get_header_lprops(ierr, lpp, head)
    use TOUZA_Nio,only: get_item, hi_AITM1, hi_AITM2, hi_AITM3
    use TOUZA_Nio,only: get_header_cprop
    implicit none
    integer,         intent(out)   :: ierr
    type(loop_t),    intent(inout) :: lpp(0:*)
    character(len=*),intent(in)    :: head(*)

    integer jc
    integer irange(2, 0:mcoor-1)

    ierr = 0
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
    enddo
    do jc = mcoor, lcoor - 1
       lpp(jc)%name = ' '
       lpp(jc)%bgn = 0
       lpp(jc)%end = 0
       lpp(jc)%stp = -1
    enddo
  end subroutine get_header_lprops

!!!_   . put_header_lprops
  subroutine put_header_lprops(ierr, head, lpp)
    use TOUZA_Nio,only: put_header_cprop
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: head(*)
    type(loop_t),    intent(in)  :: lpp(0:*)

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

!!!_   . get_obj_list_st
  subroutine get_obj_list_st &
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
  end subroutine get_obj_list_st

!!!_   . get_obj_list
  subroutine get_obj_list &
       & (ierr, str, handle, n)
    use TOUZA_Std,only: join_list
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    integer,         intent(in)  :: handle(:)
    integer,         intent(in)  :: n
    character(len=lname) :: buf(n)
    integer j
    ierr = 0
    do j = 1, n
       if (ierr.eq.0) call get_obj_string(ierr, buf(j), handle(j))
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

    ierr = 0
    nc = 0
    mc = choice(lcoor, maxco)
    do jc = 0, mc - 1
       call get_range_string(ierr, cran, lpp(jc)%bgn, lpp(jc)%end, lpp(jc)%stp)
102    format(A, ',', A)
       if (lpp(jc)%name.eq.' ') then
          cstr(jc) = trim(cran)
       else
          write(cstr(jc), 102) trim(lpp(jc)%name), trim(cran)
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
    else
       write(str, 114) bb, ee
    endif

  end subroutine get_range_string

!!!_  - stacked operation dispatcher
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
          call trace_queue(jerr, aqueue(jq), levv)
          call show_stack(jerr)
       endif
       if (ierr.ne.0) exit
    enddo

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
            & (ierr, 'record:', (/user_index_bgn(brec)/), &
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
       else if (handle.eq.opr_SQRT) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_SQRT)
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
       else if (handle.eq.opr_TANH) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_TANH)
       else if (handle.eq.opr_ASIN) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_ASIN)
       else if (handle.eq.opr_ACOS) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_ACOS)
       else if (handle.eq.opr_EXPONENT) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_EXPONENT)
       else if (handle.eq.opr_FRACTION) then
          call apply_opr_UNARY(ierr, handle, lefts(1:push), righth(1:pop), apply_UNARY_FRACTION)
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
       else if (handle.eq.opr_ATAN2) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_ATAN2)
       else if (handle.eq.opr_SCALE) then
          call apply_opr_BINARY(ierr, handle, lefts(1:push), righth(1:pop), cmode, apply_BINARY_SCALE)
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
       if (ANY(handle.eq.(/opr_IDIV, opr_INT/))) then
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
    use TOUZA_Std,only: choice, join_list
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
    use TOUZA_Std,only: choice, join_list
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
    character(len=64) :: lcstr, pcstr
    character(len=64) :: fmt_nline
    character(len=64) :: fmt_uline
    character(len=64) :: fmt_xline

    type(domain_t) :: doml, domr(1)
    integer mco
    integer jl, jp
    integer lidx(0:lcoor-1), pidx(0:lcoor-1)
    integer nb
    integer btmp(1), ptmp(1)

    ierr = 0
    utmp = choice(ulog, u)

212 format('# stack[', I0, '] ', A)
211 format('#   ', A, ' > ', A)
201 format('(', I0, '(I0, 1x), ', A, ')')
202 format('(', I0, '(I0, 1x), ''.'')')
203 format('(', I0, '(I0, 1x), ''_'')')
221 format('(', A, ')')
222 format('(', '''.'')')
223 format('(', '''_'')')
    do jbuf = 0, nbuf - 1
       jb = bufj(jbuf)
       hb = bufh(jbuf)
       js = pstk(jbuf)
       nb = 1
       btmp(1) = hb
       ptmp(1) = js
       if (is_msglev(lev_verbose, msglev_normal+1)) then
          write(utmp, 212) user_index_bgn(jbuf), trim(obuffer(jb)%desc)
          if (ierr.eq.0) call get_domain_string(ierr, lcstr, bstack(js)%lcp)
          if (ierr.eq.0) call get_domain_string(ierr, pcstr, obuffer(jb)%pcp)
          if (ierr.eq.0) write(utmp, 211) trim(pcstr), trim(lcstr)
       endif

       if (ierr.eq.0) call tweak_coordinates(ierr, domL, domR, btmp, ptmp, nb)
       if (ierr.eq.0) call set_inclusive_domain(ierr, domL, domR, btmp, ptmp, nb)
       if (ierr.eq.0) call settle_output_domain(ierr, domL)
       if (ierr.eq.0) call settle_input_domain(ierr, domR(1), btmp(1), ptmp(1), domL)

       ! if (is_msglev_DEBUG(lev_verbose)) then
       !    if (ierr.eq.0) call show_domain(ierr, doml, 'left')
       !    if (ierr.eq.0) call show_domain(ierr, domr(1), 'right')
       ! endif

       if (ierr.eq.0) then
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
    integer jl, jp, jc
    integer lidx(0:lcoor-1), pidx(0:lcoor-1)
    character(len=64) :: cname(0:lcoor-1)
    character(len=64) :: vals(0:nbuf-1)
    character(len=64) :: fmt_xline
    character(len=64) :: name
    character(len=64) :: lcstr, pcstr
    character(len=256) :: cjoin, vjoin
    type(buffer_t) :: htmp

    ierr = 0
    utmp = choice(ulog, u)

    if (ierr.eq.0) call get_compromise_domain(ierr, doml, domr, bufh, pstk, nbuf, cmode, htmp)

    do j = 0, nbuf - 1
       jb = bufj(j)
       hb = bufh(j)
       js = pstk(j)
202    format('## ', I0, 1x, A, 1x, A, ' > ', A, 1x, A)
       if (ierr.eq.0) call get_obj_string(ierr, name, hb)
       if (ierr.eq.0) vals(j) = name
       if (ierr.eq.0) call get_domain_string(ierr, lcstr, bstack(js)%lcp)
       if (ierr.eq.0) call get_domain_string(ierr, pcstr, obuffer(jb)%pcp)
       if (ierr.eq.0) then
          write(utmp, 202) user_index_bgn(j), trim(name), &
               & trim(pcstr), trim(lcstr), &
               & trim(obuffer(jb)%desc)
       endif
    enddo
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
       do jc = 0, mco - 1
          cname(jc) = htmp%pcp(jc)%name
          if (cname(jc).eq.' ') cname(jc) = '-'
       enddo
       if (ierr.eq.0) call join_list(ierr, cjoin, cname(0:mco-1))
       if (ierr.eq.0) call join_list(ierr, vjoin, vals(0:nbuf-1))
211    format('#', A, 1x, A)
       write(utmp, 211) trim(cjoin), trim(vjoin)
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
    integer jctgt, jotgt
    integer lasth, jbref
    integer j, jb
    integer js
    integer b, e,  n, jc
    integer order(0:lcoor-1)
    character(len=litem) cname(0:lcoor-1)
    character(len=64) :: opr

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
       call order_coordinates &
            & (ierr, order, cname, bstack(js)%lcp, obuffer(jbref)%pcp, lcoor)
       if (ierr.eq.0) then
          jctgt = find_first(order(0:lcoor-1), jotgt, offset=0)
          if (jctgt.lt.0) jctgt = jotgt
       endif
    endif
    if (ierr.eq.0) then
       b = logical_index(bstack(js)%lcp(jctgt)%bgn, obuffer(jbref)%pcp(jctgt)%bgn)
       e = logical_index(bstack(js)%lcp(jctgt)%end, obuffer(jbref)%pcp(jctgt)%end)
       e = max(1 + b, e)
       n = max(1, e - b)
       do j = 0, size(lefts) - 1
          jb = buf_h2item(lefts(j)%bh)
          obuffer(jb)%pcp(:) = def_loop
          obuffer(jb)%pcp(jctgt)%bgn = b
          obuffer(jb)%pcp(jctgt)%end = e
          obuffer(jb)%pcp(jctgt)%stp = 1
          obuffer(jb)%pcp(jctgt)%name = cname(jctgt)
          obuffer(jb)%undef = UNDEF
          if (ierr.eq.0) call alloc_buffer_t(ierr, obuffer(jb), n)
          if (ierr.eq.0) then
             obuffer(jb)%k = kv_int
             do jc = b, e - 1
                obuffer(jb)%vd(jc - b) = REAL(user_index_bgn(jc), kind=KDBL)
             enddo
          endif
          obuffer(jb)%desc = trim(opr)
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
    integer ilevi, ilevo
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
          if (ANY(ilevo.eq.(/ilev_add, ilev_exp, ilev_logical, ilev_mul/))) then
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
                lsep = '['
                rsep = ']'
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
    integer jb,  jc, jo, js
    integer b,   e
    integer low, high
    integer nceff

    ierr = 0

    if (present(bufo)) then
       call tweak_coordinates(ierr, domL, domR, bufh, pstk, nbuf, bufo)
    else
       call tweak_coordinates(ierr, domL, domR, bufh, pstk, nbuf)
    endif
    if (ierr.eq.0) call set_inclusive_domain(ierr, domL, domR, bufh, pstk, nbuf)
    if (ierr.eq.0) then
       nceff = domL%mco
       select case(cmode)
       case (cmode_first)
          do j = 0, 0
             do jo = 0, nceff - 1
                low  = domL%bgn(jo)
                high = domL%end(jo)
                jb = buf_h2item(bufh(j))
                js = pstk(j)
                jc = domR(j)%cidx(jo)
                if (jc.ge.0) then
                   b = bstack(js)%lcp(jo)%bgn
                   e = bstack(js)%lcp(jo)%end
                   if (obuffer(jb)%pcp(jc)%stp.gt.0) then
                      if (b.eq.null_range) b = obuffer(jb)%pcp(jc)%bgn
                      if (e.eq.null_range) e = obuffer(jb)%pcp(jc)%end
                   endif
                   b = logical_index(b, low)
                   e = logical_index(e, high)
                   domL%bgn(jo) = b
                   domL%end(jo) = e
                endif
             enddo
          enddo
       case (cmode_intersect)
          do jo = 0, nceff - 1
             low  = domL%bgn(jo)
             high = domL%end(jo)
             do j = 0, nbuf - 1
                jb = buf_h2item(bufh(j))
                jc = domR(j)%cidx(jo)
                js = pstk(j)
                if (jc.ge.0) then
                   b = bstack(js)%lcp(jo)%bgn
                   e = bstack(js)%lcp(jo)%end
                   if (obuffer(jb)%pcp(jc)%stp.gt.0) then
                      if (b.eq.null_range) b = obuffer(jb)%pcp(jc)%bgn
                      if (e.eq.null_range) e = obuffer(jb)%pcp(jc)%end
                   endif
                   low  = max(low,  logical_index(b, low))
                   high = min(high, logical_index(e, high))
                endif
             enddo
             domL%bgn(jo) = low
             domL%end(jo) = high
          enddo
       end select
    endif
    if (ierr.eq.0) then
       call settle_output_domain(ierr, domL)
    endif
    do j = 0, nbuf - 1
       if (ierr.eq.0) call settle_input_domain(ierr, domR(j), bufh(j), pstk(j), domL)
    enddo
    if (present(bufo)) then
       if (ierr.eq.0) then
          call set_output_buffer(ierr, bufo, bufh(0:nbuf-1), domL)
       endif
    endif

    ! if (is_msglev_DEBUG(lev_verbose)) then
    !    if (ierr.eq.0) call show_domain(ierr, domL, 'compromise/L')
    !    do j = 0, nbuf - 1
    !       if (ierr.eq.0) call show_domain(ierr, domR(j), 'compromise/R')
    !    enddo
    ! endif
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
    type(buffer_t),intent(inout),optional :: bufo

    integer nceff
    character(len=lname) :: newc(0:lcoor-1),   nameR(0:lcoor-1, 0:nbuf-1)
    integer                 order(0:lcoor-1),  orderR(0:lcoor-1, 0:nbuf-1)
    integer j, jb, jc, jo, js

    ierr = 0
    do j = 0, nbuf - 1
       jb = buf_h2item(bufh(j))
       js = pstk(j)
       if (ierr.eq.0) then
          call order_coordinates &
               & (ierr, orderR(:,j), nameR(:,j), bstack(js)%lcp, obuffer(jb)%pcp, lcoor)
       endif
    enddo
    if (ierr.eq.0) then
       call match_coordinates(ierr, nceff, order, newc, orderR, nameR, lcoor, nbuf)
       if (nceff.gt.lcoor) then
          ierr = ERR_INSUFFICIENT_BUFFER
          call message(ierr, 'overflow in tweaking ', (/nceff/))
          return
       endif
       ! write(*, *) nceff
       ! write(*, *) order
       ! write(*, *) newc
    endif
    if (ierr.eq.0) then
       domL%mco = nceff
       domL%ofs(0:nceff-1) = null_range
       domL%cidx(0:nceff-1) = -1
       domL%strd(0:nceff) = -1
       do jc = 0, nceff - 1
          jo = order(jc)
          if (jo.ge.0) domL%cidx(jo) = jc
       enddo
       if (present(bufo)) then
          bufo%pcp(:)%name = ' '
          bufo%pcp(0:nceff-1)%name = newc(0:nceff-1)
       endif
    endif
    if (ierr.eq.0) then
       do j = 0, nbuf - 1
          domR(j)%mco = nceff
          domR(j)%cidx(0:nceff-1) = -1
          domR(j)%strd(0:nceff) = -1
          do jc = 0, lcoor - 1
             jo = orderR(jc, j)
             if (jo.ge.0) then
                domR(j)%cidx(jo) = jc
             else if (nameR(jc, j).ne.' ') then
                jo = find_first(newc(0:nceff-1), nameR(jc, j), offset=0)
                if (jo.ge.0) domR(j)%cidx(jo) = jc
             else if (jo.eq.co_wild) then
                domR(j)%cidx(jo) = jc
             endif
          enddo
       enddo
    endif
  end subroutine tweak_coordinates

!!!_   . order_coordinates
  subroutine order_coordinates(ierr, order, name, lcp, pcp, mco)
    use TOUZA_Std,only: find_first, parse_number, jot, inrange
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: order(0:*)
    character(len=*),intent(out) :: name(0:*)
    type(loop_t),    intent(in)  :: lcp(0:*), pcp(0:*)
    integer,         intent(in)  :: mco
    integer jc, jt, jo
    integer larg
    integer jpold, jpnew
    integer jctgt
    integer jcfrm, jcto
    integer jofrm, joto
    integer jcnew, jcold
    integer lsep
    integer bufo(0:mco-1)
    integer jerr
    character(len=lname) :: bufn(0:mco-1)

    character(len=2) :: csyms(0:2) = (/str_X, str_Y, str_Z/)
    integer tblc2o(0:mco-1), tblo2c(0:mco-1)
    integer oflg(0:mco-1)

    ierr = 0
    order(0:mco-1) = co_unset
    name(0:mco-1) = ' '
    lsep = len(rename_sep)
    bufo(:) = co_unset
    bufn(:) = ' '
    do jc = 0, mco - 1
       larg = len_trim(lcp(jc)%name)
       jctgt = -2
       if (larg.eq.0) then
          jpold = 0
          jpnew = -1
       else
          jpold = index(lcp(jc)%name, rename_sep)
          if (jpold.gt.0) then
             jpnew = index(lcp(jc)%name(jpold+lsep:), rename_sep)
             if (jpnew.gt.0) then
                jpnew = jpnew + jpold + lsep - 2
             else
                jpnew = larg
                if (jpold + lsep.gt.jpnew) jpnew = -1
             endif
             jpold = jpold - 1
          else
             jpold = larg
             jpnew = -1
          endif
       endif
       if (jpold.gt.0) then
          ! if force ordering
          call parse_number(jerr, jctgt, lcp(jc)%name(1:jpold))
          if (jerr.eq.0) then
             jctgt = system_index_bgn(jctgt)
             if (jctgt.lt.0.or.jctgt.ge.mco) jctgt = -1
          else
             jt = find_first(csyms, lcp(jc)%name(1:jpold), offset=0)
             jctgt = find_first(pcp(0:mco-1)%name, lcp(jc)%name(1:jpold), offset=0)
             if (jctgt.ge.0) then
                if (jt.ge.0) then
                   call message(ierr, 'ignore conflict of coordinate ' // lcp(jc)%name(1:jpold), levm=msglev_info)
                endif
             else
                jctgt = jt
             endif
          endif
          if (jctgt.lt.0) then
             ierr = ERR_INVALID_PARAMETER
             call message(ierr, 'fail to search coordinate ' // lcp(jc)%name(1:jpold))
             exit
          endif
       endif
       if (jctgt.ge.0) then
          if (order(jctgt).ge.0) then
             ierr = ERR_INVALID_PARAMETER
             call message(ierr, 'duplicate tweak in coordinate extraction/c', (/jctgt/))
             exit
          endif
          if (jpnew.gt.0) then
             if (jpold+lsep+1.gt.jpnew) then
                name(jctgt) = ' '
             else
                name(jctgt) = lcp(jc)%name(jpold+lsep+1:jpnew)
             endif
          else
             name(jctgt) = pcp(jctgt)%name
          endif
          ! change status only rename to empty
          order(jctgt) = jc
       else
          if (jpnew.gt.0) then
             if (jpold+lsep+1.gt.jpnew) then
                bufn(jc) = ' '
             else
                bufn(jc) = lcp(jc)%name(jpold+lsep+1:jpnew)
             endif
             bufo(jc) = coordinate_flexibility(lcp(jc), pcp(jc), jc, bufn(jc))
          else if (lcp(jc)%stp.ge.0) then
             bufn(jc) = pcp(jc)%name
             bufo(jc) = coordinate_flexibility(lcp(jc), pcp(jc), jc, bufn(jc))
          endif
       endif
    enddo
    if (ierr.eq.0) then
       do jc = 0, mco - 1
          if (order(jc).ne.co_unset .and. bufo(jc).ne.co_unset) then
             ierr = ERR_INVALID_PARAMETER
             call message(ierr, 'duplicate tweak in coordinate extraction/a', (/jc/))
             exit
          endif
       enddo
    endif

    ! automatic coordinate shifts
    if (ierr.eq.0) then
       call jot(tblc2o, mco)
       call jot(tblo2c, mco)
       do jc = 0, mco - 1
          if (bufo(jc).eq.co_unset) then
             bufn(jc) = pcp(jc)%name
             bufo(jc) = coordinate_flexibility(lcp(jc), pcp(jc), jc, bufn(jc))
          endif
       enddo
       oflg(0:mco-1) = co_unset
       do jc = 0, mco - 1
          if (order(jc).lt.0.and.bufo(jc).eq.co_null) oflg(jc) = co_null
       enddo
       do jcfrm = 0, mco - 1
          jofrm = tblc2o(jcfrm)
          joto = order(jcfrm)
          jcto = tblo2c(joto)
          if (joto.lt.0) cycle
          if (jofrm.lt.joto) then
             ! F<<<T (-1)
             jo = joto
             jcnew = jcfrm
             do
                if (jo.le.jofrm) exit
                jcold = tblo2c(jo)
                if (oflg(jo).lt.0) then
                   tblo2c(jo) = jcnew
                   tblc2o(jcnew) = jo
                   jcnew = jcold
                   if (oflg(jo).eq.co_null) then
                      oflg(jo) = co_unset
                      oflg(jofrm) = co_null
                      exit
                   endif
                endif
                jo = jo - 1
             enddo
             tblo2c(jofrm) = jcnew
             tblc2o(jcnew) = jofrm
             oflg(joto) = jcfrm
          else
             ! T>>>F (+1)
             jo = joto
             jcnew = jcfrm
             do
                if (jo.ge.jofrm) exit
                jcold = tblo2c(jo)
                if (oflg(jo).lt.0) then
                   tblo2c(jo) = jcnew
                   tblc2o(jcnew) = jo
                   jcnew = jcold
                   if (oflg(jo).eq.co_null) then
                      oflg(jo) = co_unset
                      oflg(jofrm) = co_null
                      exit
                   endif
                endif
                jo = jo + 1
             enddo
             tblo2c(jofrm) = jcnew
             tblc2o(jcnew) = jofrm
             oflg(joto) = jcfrm
          endif
       enddo
    endif
    ! final adjustment for wildcard coordinates
    if (ierr.eq.0) then
       do jc = 0, mco - 1
          if (bufo(jc).eq.co_wild.and.order(jc).lt.0) then
             jo = tblc2o(jc)
             if (jc.ne.jo) then
                jt = tblo2c(jc)
                if (bufo(jt).eq.co_float .and. order(jt).lt.0) then
                   tblc2o(jc) = jc
                   tblo2c(jc) = jc
                   tblo2c(jo) = jt
                   tblc2o(jt) = jo
                endif
             endif
          endif
       enddo
       do jc = 0, mco - 1
          if (bufo(jc).eq.co_wild.and.order(jc).lt.0) order(jc) = tblc2o(jc)
       enddo
       do jc = 0, mco - 1
          if (order(jc).lt.0) order(jc) = bufo(jc)
          if (name(jc).eq.' ') name(jc) = bufn(jc)
       enddo
    endif
    ! do jc = 0, mco - 1
    !    write(*, *) 'order/result', jc, '[' // trim(pcp(jc)%name) // ']',  &
    !         & pcp(jc)%stp, lcp(jc)%stp, &
    !         & ' ', sym(bufo(jc))  // ':', trim(bufn(jc)), &
    !         & ' >> ', sym(order(jc)) // ':', trim(name(jc))
    ! enddo
    ! ierr = 0
  end subroutine order_coordinates

!!!_   . coordinate_flexibility
  integer function coordinate_flexibility (lcp, pcp, org, name) result(k)
    implicit none
    type(loop_t),    intent(in) :: lcp
    type(loop_t),    intent(in) :: pcp
    integer,         intent(in) :: org
    character(len=*),intent(in) :: name
    if (name.ne.' ') then
       k = co_float
    else
       if (pcp%stp.gt.0) then
          ! k = org
          k = co_wild
       else if (lcp%bgn.eq.null_range.and.lcp%end.eq.null_range) then
          k = co_null
       else
          ! k = org
          k = co_wild
       endif
    endif
  end function coordinate_flexibility

!!!_   . match_coordinates
  subroutine match_coordinates(ierr, nceff, order, name, oadd, nadd, mco, nbuf)
    use TOUZA_Std,only: choice, find_first, find_first_range
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: mco
    integer,         intent(out)         :: nceff
    integer,         intent(inout)       :: order(0:*)
    character(len=*),intent(inout)       :: name(0:*)
    integer,         intent(inout)       :: oadd(0:mco-1, 0:*)
    character(len=*),intent(inout)       :: nadd(0:mco-1, 0:*)
    integer,         intent(in)          :: nbuf
    integer limtry
    integer                 newo(0:mco*2-1)
    character(len=lname) :: newc(0:mco*2-1)
    integer jbufa, jcoa
    integer jbufb, jcob
    integer jchk,  nb
    character(len=lname) :: nchk(0:nbuf-1)
    integer              :: ochk(0:nbuf-1)
    integer kta
    integer jx
    integer jcrob
    integer jtmp

    ierr = 0
    nceff = -1
    limtry = mco * 2
    order(0:mco-1) = co_unset
    name(0:mco-1)  = ' '

    newo(0:limtry-1) = co_unset
    newc(0:limtry-1) = ' '
!!!_    * assign robust coordinates
    ! do jbufa = 0, nbuf - 1
    !    call diag_order(ierr, nadd(:, jbufa), oadd(:, jbufa), mco, idx=jbufa)
    ! enddo
    loop_robust: do jbufa = 0, nbuf - 1
       do jcoa = 0, mco - 1
          kta = oadd(jcoa, jbufa)
          if (kta.ge.0) then
             if (nadd(jcoa, jbufa).ne.' ') then
                jx = find_first(newc(0:limtry-1), nadd(jcoa, jbufa), offset=0)
                if (jx.ge.0 .and. jx.ne.kta) then
                   ierr = ERR_INVALID_PARAMETER
                   call message(ierr, 'dispersed robust coordinate:' // trim(nadd(jcoa, jbufa)), (/jx, kta/))
                   exit loop_robust
                endif
             endif
             if (newo(kta).eq.co_unset) then
                newo(kta) = kta
                newc(kta) = nadd(jcoa, jbufa)
             else if (newc(kta).eq.' ') then
                newc(kta) = nadd(jcoa, jbufa)
             else if (nadd(jcoa, jbufa).eq.' ') then
                continue
             else if (newc(kta).eq.nadd(jcoa, jbufa)) then
                continue
             else
                ierr = ERR_INVALID_PARAMETER
                call message(ierr, 'conflict robust coordinates: ' &
                     & // trim(newc(kta)) // ' ' // trim(nadd(jcoa, jbufa)), &
                     & (/kta/))
                exit loop_robust
             endif
          endif
       enddo
    enddo loop_robust
    ! call show_order(ierr, newc, newo, mco, tag='match/0')
    ! do jbufa = 0, nbuf - 1
    !    call show_order(ierr, nadd(:, jbufa), oadd(:, jbufa), mco, tag='match/0', idx=jbufa)
    ! enddo
!!!_    * assign fragile and wild-card coordinates (twice)
    if (ierr.eq.0) then
       loop_wild: do jtmp = 0, 1
          do jbufa = 0, nbuf - 1
             do jcoa = 0, mco - 1
                if (oadd(jcoa, jbufa).eq.co_float) then
                   jcrob = find_first(newc(0:limtry-1), nadd(jcoa, jbufa), offset=0)
                   ! write(*, *) 'rob', jcoa, jbufa, jcrob, newo(jcoa), newc(jcoa)
                   if (jcrob.ge.0) then
                      if (oadd(jcrob, jbufa).eq.jcrob) then
                         ierr = ERR_INVALID_PARAMETER
                         call message(ierr, 'ambiguous wildcard coordinates ', (/jbufa, jcoa, jcrob/))
                         exit loop_wild
                      else
                         oadd(jcoa, jbufa) = jcrob
                      endif
                   else if (newo(jcoa).ge.0) then
                      jcrob = find_first(oadd(:, jbufa), jcoa, offset=0)
                      if (jcrob.ge.0) then
                         continue
                      else if (newc(jcoa).eq.' ') then
                         newc(jcoa) = nadd(jcoa, jbufa)
                         oadd(jcoa, jbufa) = jcoa
                      else if (newc(jcoa).eq.nadd(jcoa, jbufa)) then
                         oadd(jcoa, jbufa) = jcoa
                      endif
                   endif
                endif
             enddo
          enddo
       enddo loop_wild
    endif
    ! call show_order(ierr, newc, newo, mco, tag='match/1')
    ! do jbufa = 0, nbuf - 1
    !    call show_order(ierr, nadd(:, jbufa), oadd(:, jbufa), mco, tag='match/1', idx=jbufa)
    ! enddo
!!!_    * adjust fragiles
    if (ierr.eq.0) then
       nchk(0:nbuf-1) = ' '
       jcrob = find_first_range(newo(0:limtry-1), high=-1)
       loop_fragile: do jcoa = 0, mco - 1
          jchk = 0
          do jbufa = 0, nbuf - 1
             if (oadd(jcoa, jbufa).eq.co_float) then
                jx = find_first(newc(0:limtry-1), nadd(jcoa, jbufa), offset=0)
                if (jx.ge.0) then
                   oadd(jcoa, jbufa) = jx
                   cycle
                endif
                jx = find_first(nchk(0:jchk-1), nadd(jcoa, jbufa), offset=0)
                if (jx.lt.0) then
                   jx = jchk
                   jchk = jchk + 1
                   nchk(jx) = nadd(jcoa, jbufa)
                   ochk(jx) = jcoa
                endif
                do jbufb = 0, nbuf - 1
                   do jcob = jcoa, mco - 1
                      if (nadd(jcob, jbufb).eq.nchk(jx)) then
                         ochk(jx) = max(jcob, ochk(jx))
                         exit
                      endif
                   enddo
                enddo
             endif
          enddo
          if (jchk.gt.0) then
             jtmp = -1
             nb = HUGE(0)
             do jx = 0, jchk - 1
                if (nb.eq.ochk(jx)) then
                   jtmp = -1
                   exit
                else if (ochk(jx).lt.nb) then
                   nb = ochk(jx)
                   jtmp = jx
                endif
             enddo
             if (jtmp.lt.0) then
                ierr = ERR_INVALID_PARAMETER
                call message(ierr, 'ambiguous fragile coordinates at ', (/jcoa/))
                do jx = 0, jchk - 1
                   call message(ierr, 'coordinate ' // trim(nchk(jx)))
                enddo
                exit loop_fragile
             else
                newc(jcrob) = nchk(jtmp)
                newo(jcrob) = jcrob
                jcrob = find_first_range(newo(jcrob+1:limtry-1), high=-1, offset=jcrob+1)
             endif
          endif
       enddo loop_fragile
    endif
!!!_    * final
    if (ierr.eq.0) then
       do jbufa = 0, nbuf - 1
          do jcoa = 0, mco - 1
             if (nadd(jcoa, jbufa).eq.' ') then
                jcrob = oadd(jcoa, jbufa)
                if (jcrob.ge.0) nadd(jcoa, jbufa) = newc(jcrob)
             endif
          enddo
       enddo
    endif
    if (ierr.eq.0) then
       nceff = find_first_range(newo(0:limtry-1), low=0, offset=0, back=.TRUE.) + 1
       order(0:nceff) = newo(0:nceff)
       name(0:nceff) = newc(0:nceff)
    endif

    ! if (ierr.ne.0.or.is_msglev_DEBUG(lev_verbose)) then
    !    do jbufa = 0, nbuf - 1
    !       call diag_order(jerr, nadd(:, jbufa), oadd(:, jbufa), mco, tag='debug', idx=jbufa)
    !    enddo
    !    call diag_order(jerr, newc, newo, nceff, tag='debug')
    ! endif
  end subroutine match_coordinates

!!!_   . set_inclusive_domain
  subroutine set_inclusive_domain &
       & (ierr, domL, domR, bufh, pstk, nbuf)
    use TOUZA_Std,only: choice, find_first
    implicit none
    integer,       intent(out)   :: ierr
    type(domain_t),intent(inout) :: domL
    type(domain_t),intent(in)    :: domR(0:*)
    integer,       intent(in)    :: bufh(0:*)
    integer,       intent(in)    :: pstk(0:*)
    integer,       intent(in)    :: nbuf

    integer nceff
    integer j
    integer jb, jc, jo, js
    integer b,   e
    integer low, high, stp
    integer,parameter :: lini = HUGE(0)
    integer,parameter :: hini = (- HUGE(0)) - 1

    ierr = 0

    if (ierr.eq.0) then
       nceff = domL%mco
       do jo = 0, nceff - 1
          low = lini
          high = hini
          stp = -1
          do j = 0, nbuf - 1
             jb = buf_h2item(bufh(j))
             jc = domR(j)%cidx(jo)
             js = pstk(j)
             if (jc.ge.0) then
                b = bstack(js)%lcp(jo)%bgn
                e = bstack(js)%lcp(jo)%end
                if (obuffer(jb)%pcp(jc)%stp.gt.0) then
                   if (b.eq.null_range) b = obuffer(jb)%pcp(jc)%bgn
                   if (e.eq.null_range) e = obuffer(jb)%pcp(jc)%end
                endif
                if (b.ne.null_range) low = min(low, b)
                if (e.ne.null_range) high = max(high, e)
                stp = max(stp, obuffer(jb)%pcp(jc)%stp)
             endif
          enddo
          if (low.eq.lini) low = 0
          if (high.eq.hini) high = low + max(0, stp)
          domL%bgn(jo) = low
          domL%end(jo) = high
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

    integer jo,  jc
    integer low, high
    integer b,   e
    integer jb

    ierr = 0
    jb = buf_h2item(hbuf)

    do jo = 0, ref%mco - 1
       low  = ref%bgn(jo)
       high = ref%end(jo)

       jc = dom%cidx(jo)
       if (jc.ge.0) then
          b = logical_index(bstack(jstk)%lcp(jo)%bgn, low)
          e = logical_index(bstack(jstk)%lcp(jo)%end, high)
       else
          b = low
          e = high
       endif
       dom%bgn(jo) = b
       dom%end(jo) = max(e, 1+b)
       dom%iter(jo) = max(1, e - b)
    enddo
    if (ierr.eq.0) then
       call settle_domain_stride(ierr, dom, obuffer(jb)%pcp)
    endif
    if (ierr.eq.0) then
       call settle_domain_loop_h(ierr, dom, hbuf, ref, ref%mco)
    endif

  end subroutine settle_input_domain

!!!_   . settle_domain_loop_h
  subroutine settle_domain_loop_h(ierr, dom, handle, refd, maxco)
    implicit none
    integer,       intent(out)         :: ierr
    type(domain_t),intent(inout)       :: dom
    integer,       intent(in)          :: handle
    type(domain_t),intent(in)          :: refd
    integer,       intent(in),optional :: maxco
    integer jb
    jb = buf_h2item(handle)
    ierr = min(0, jb)
    if (ierr.eq.0) call settle_domain_loop(ierr, dom, obuffer(jb), refd, maxco)
    return
  end subroutine settle_domain_loop_h
!!!_   . settle_domain_loop
  subroutine settle_domain_loop(ierr, dom, buf, refd, maxco)
    use TOUZA_std,only: choice
    implicit none
    integer,       intent(out)         :: ierr
    type(domain_t),intent(inout)       :: dom
    type(buffer_t),intent(in)          :: buf
    type(domain_t),intent(in)          :: refd
    integer,       intent(in),optional :: maxco
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

    integer jo, jc
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
       strd(jc) = strd(jc-1) * max(1, pcp(jc-1)%end - pcp(jc-1)%bgn)
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
             buf%pcp(jc)%end = dom%bgn(jc)
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
  PURE integer function physical_index (lidx, dom) result(n)
    implicit none
    integer,       intent(in) :: lidx(0:*)
    type(domain_t),intent(in) :: dom
    integer jc
    n = 0
    do jc = 0, dom%mco - 1
       if (dom%bgn(jc).le.lidx(jc).and.lidx(jc).lt.dom%end(jc)) then
          n = n + (dom%ofs(jc) + lidx(jc)) * dom%strd(jc)
       else
          n = -1
          exit
       endif
    enddo
  end function physical_index

! !!!_   . conv_physical_index
!   PURE &
!   integer function conv_physical_index (jlog, domL, domR) result(n)
!     implicit none
!     integer,       intent(in) :: jlog
!     type(domain_t),intent(in) :: domL, domR
!     integer jc
!     integer jcur
!     n = 0
!     do jc = 0, domL%mco - 1
!        jcur = mod(jlog, domL%strd(jc + 1)) / domL%strd(jc)
!        if (domR%bgn(jc).le.jcur.and.jcur.lt.domR%end(jc)) then
!           n = n + (domR%ofs(jc) + jcur) * domR%strd(jc)
!        else
!           n = -1
!           exit
!        endif
!     enddo
!   end function conv_physical_index

!!!_   . incr_logical_index
  subroutine incr_logical_index(idx, dom)
    implicit none
    integer,       intent(inout) :: idx(0:*)
    type(domain_t),intent(in)    :: dom
    integer jc, k
    k = 1
    do jc = 0, dom%mco - 1
       idx(jc) = idx(jc) + k
       k = idx(jc) / dom%iter(jc)
       idx(jc) = mod(idx(jc), dom%iter(jc))
    enddo
  end subroutine incr_logical_index

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

!!!_  - other utilities
!!!_   . user_index_bgn()
  ELEMENTAL integer function user_index_bgn(j, n) result(k)
    implicit none
    integer,intent(in)          :: j
    integer,intent(in),optional :: n
    if (j.eq.full_range .or. j.eq.null_range) then
       k = j
    else
       k = j + global_offset_bgn
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
       k = j + global_offset_end
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
       k = j - global_offset_bgn
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
       k = j - global_offset_end
    endif
  end function system_index_end

!!!_ + end chak
end program chak
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

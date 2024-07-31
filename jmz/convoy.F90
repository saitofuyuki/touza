!!!_! convoy.F90 - TOUZA/Jmz ConvOy, I've got an idea.
! Maintainer: SAITO Fuyuki
! Created: Jun 16 2023
#define TIME_STAMP 'Time-stamp: <2024/06/28 07:34:15 fuyuki convoy.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2024
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
#define _PFX 'convoy: '
!!!_@ TOUZA/Jmz/convoy - Conversion is your Oyster
program convoy
!!!_ + Declaration
!!!_  - modules
  use Jmz_param
  use Jmz_base, base_init=>init, base_finalize=>finalize
  use TOUZA_Nio,only: put_header_cprop, put_item, get_default_header
  use TOUZA_Nio,only: litem, nitem
  use TOUZA_Nio,only: hi_DFMT, hi_ITEM, hi_DSET, hi_UNIT
  use TOUZA_Nio,only: nio_record_std
  use TOUZA_Nio,only: nio_write_header, nio_write_data, nio_store_csr
  use convoy_util
  use convoy_ps2g
  use convoy_r2g
!!!_  - default
  implicit none
!!!_  - variables
  integer ierr
!!!_  - parameter
  integer,parameter :: lim_transf = 2 ** MIN(bit_size(0) - 2, 10)
  integer,parameter :: transf_err  = (- HUGE(0)) - 1
  integer,parameter :: transf_none = - HUGE(0)

!!!_  - configurations
  character(len=lpath) :: cmd, ucmd
  integer jpos, npos
!!!_ + Body
!!!_  - driver
  ierr = 0

  if (ierr.eq.0) call base_init(ierr, basename='convoy')
  if (ierr.eq.0) call parse_global(ierr, jpos, npos)

  if (ierr.eq.0) then
     if (npos.eq.0) then
109     format(_PFX, 'no command. exit')
        write(*, 109)
     else
        call get_param(ierr, cmd, jpos)
        jpos = jpos + 1
119     format(_PFX, 'unknown command: ', A)
        if (ierr.eq.0) then
           call upcase(ucmd, cmd(1:4))
           select case (ucmd)
           case('PS2G', 'G2PS')
              call main_ps2g(ierr, jpos, npos)
           case('R2G')
              call main_r2g(ierr, jpos, npos)
           case default
              call parse_subcmd(ierr, jpos, npos, cmd)
              if (ierr.ne.0) then
                 write(*, 119) trim(cmd)
                 ierr = ERR_INVALID_PARAMETER
              endif
           end select
        endif
     endif
  endif
  if (ierr.eq.0) call base_finalize(ierr)
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
!!!_  - show_usage
  subroutine show_usage (ierr, u, levv)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer utmp
    integer lv

    ierr = 0
    utmp = choice(ulog, u)
    lv = choice(0, levv)

  end subroutine show_usage

!!!_ + conversion/projection/mapping/regridding/transformation
!!!_  - parse_subcmd
  subroutine parse_subcmd &
       & (ierr, jpos, npos, str)
    use TOUZA_Nio,only: cache_open_read, cache_group_size
    use TOUZA_Nio,only: cache_group
    use TOUZA_Nio,only: show_cache
    use Jmz_file,only: parse_file_item
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: jpos
    integer,         intent(in)    :: npos
    character(len=*),intent(in)    :: str

    integer htab, hgrp
    integer jg, ng
    integer jcmd
    integer mfx
    type(convoy_t) :: prime
    character(len=*),parameter :: proc = 'parse_subcmd'
    integer mpath, mitem

    ierr = 0
    jcmd = xcmd_none
    call parse_file_item(ierr, mpath, mitem, str)
    if (mpath.lt.0) then
       ierr = ERR_INVALID_PARAMETER
       return
    endif

    ! bx = is_file_exist(str, ierr)   ! bx == F (defined) at error
    ! ! INQUIRE(FILE=str, EXIST=bx, IOSTAT=ierr)
    ! if (.not.bx) then
    !    ierr = ERR_INVALID_PARAMETER
    !    return
    ! endif
    if (mpath.eq.0) mpath = len_trim(str)
    call cache_open_read(ierr, htab, str(1:mpath), flag=0)
    if (ierr.eq.0) call init_prime(ierr, prime)
    if (ierr.eq.0) then
       ng = cache_group_size(htab)
       do jg = 0, ng - 1
          hgrp = cache_group(jg, htab)
          jcmd = parse_convoy_operator(hgrp)
          select case(jcmd)
          case(xcmd_r2g)
             call parse_convoy_r2g(ierr, prime, jpos, npos, hgrp)
          case(xcmd_ps2g)
             call parse_convoy_ps2g(ierr, prime, jpos, npos, hgrp)
          case(xcmd_loc)
             call parse_convoy_loc(ierr, prime, jpos, npos, hgrp)
          end select
          if (ierr.ne.0) exit
       enddo
    endif
    if (ierr.eq.0) call settle_prime(ierr, prime, htab)
    if (ierr.eq.0) then
       if (mitem.gt.0) then
          call convoy_transform(ierr, mfx, prime, str(mitem:), jpos, npos)
       else
          call convoy_transform(ierr, mfx, prime, ' ', jpos, npos)
       endif
    endif
    if (ierr.eq.0) then
       if (mfx.eq.0 &
            & .or. is_verbose(msglev_INFO)) call show_prime(ierr, prime, tag=str)
    endif
    if (ierr.eq.0) then
       if (is_verbose(msglev_DETAIL)) call show_cache(ierr, htab)
    endif
    call trace_err(ierr, fun=proc)
  end subroutine parse_subcmd
!!!_   & parse_convoy_operator()
  integer function parse_convoy_operator (handle) result(k)
    use TOUZA_Nio,only: cache_group_name, axis_parse, axis_wgt
    implicit none
    integer,intent(in) :: handle
    character(len=litem) :: gname
    integer jerr
    integer xt
    k = xcmd_none
    call cache_group_name(jerr, gname, handle)
    if (jerr.eq.0) then
       call upcase(gname)
       select case(gname)
       case(DSET_R2G)
          k = xcmd_r2g
       case (DSET_PS2G)
          k = xcmd_ps2g
       case default
          xt = axis_parse(gname)
          if (xt.ge.0) then
             if (IAND(xt, axis_wgt).eq.0) then
                k = xcmd_loc
             endif
          endif
       end select
    endif
  end function parse_convoy_operator
!!!_ + convoy
!!!_  & convoy_parse_set
  subroutine convoy_parse_set &
       & (ierr, set_end, set_next, uout, &
       &  jpos, npos)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: set_end, set_next
    integer,intent(out) :: uout
    integer,intent(in)  :: jpos, npos
    integer jp
    character(len=lpath) :: arg

    ierr = 0
    jp = jpos
    set_end = -1
    set_next = -1
    uout = -1
    do
       if (jp.gt.npos) exit
       if (ierr.eq.0) call get_param(ierr, arg, jp)
       if (ierr.eq.0) then
          if (arg.eq.opr_assign) then
             set_end = jp
             exit
          endif
       endif
       if (ierr.ne.0) exit
       jp = jp + 1
    enddo
    ! write(*, *) set_end, trim(arg)
    if (ierr.eq.0) then
       if (set_end.ge.0) then
          jp = jp + 1
          if (jp.gt.npos) then
             set_next = npos + 1
          else
             call get_param(ierr, arg, jp)
             if (ierr.eq.0) then
                uout = new_unit()
                ierr = min(0, uout)
             endif
             if (ierr.eq.0) then
                call sus_open(ierr, uout, arg, ACTION='W', STATUS='R')
                ! write(*, *) uout, ierr, trim(arg)
             endif
             set_next = jp + 1
          endif
       else
          set_end  = npos + 1
          set_next = npos + 1
       endif
    endif
  end subroutine convoy_parse_set
!!!_  & parse_convoy_loc
  subroutine parse_convoy_loc &
       & (ierr, prime, jpos, npos, handle)
    use TOUZA_Nio,only: cache_var_size, cache_var_name, cache_co_size
    use TOUZA_Nio,only: cache_co_name, cache_group_recs, cache_group_name
    use TOUZA_Nio,only: cache_co_len
    use TOUZA_Nio,only: axis_parse, axis_cyclic
    implicit none
    integer,         intent(out)   :: ierr
    type(convoy_t),  intent(inout) :: prime
    integer,         intent(inout) :: jpos
    integer,         intent(in)    :: npos
    integer,         intent(in)    :: handle
    character(len=*),parameter :: proc = 'parse_convoy_loc'
    integer jv, nv
    integer jx, nx
    integer jr, nr
    character(len=litem) :: vname, cname, gname
    integer xt
    integer jt
    integer nbase, msrc

    ierr = 0
    nv = cache_var_size(handle)
    do jv = 0, nv - 1
       call cache_var_name(ierr, vname, handle, jv)
       if (ierr.eq.0) then
          nx = cache_co_size(handle, jv)
          if (nx.ne.1) cycle
          jx = 0
          call cache_co_name(ierr, cname, handle, jv, jx)
          if (cname.ne.vname) cycle
          nbase = cache_co_len(handle, jv, jx)
          nr = cache_group_recs(handle, jv)
          if (nr.lt.0) then
             ierr = ERR_PANIC
             exit
          endif
          call cache_group_name(ierr, gname, handle)
          xt = axis_parse(gname)
          msrc = nbase
          if (IAND(xt, axis_cyclic).ne.0) then
             xt = xcmd_fdcycle
             msrc = msrc - 1
          else
             xt = xcmd_fdplain
          endif
          ! write(*, *) jv, nx, vname, cname, nr, xt
          do jr = 0, nr - 1
             call add_transf(ierr, jt, prime)
             prime%tr(jt)%k       = xt
             prime%tr(jt)%h       = handle
             prime%tr(jt)%idjv    = jv
             prime%tr(jt)%recj    = jr
             prime%tr(jt)%namin   = 1
             prime%tr(jt)%namax   = 1
             prime%tr(jt)%nw      = 3
             prime%tr(jt)%idwv(:) = -1
             prime%tr(jt)%recw(:) = -1
             prime%tr(jt)%idwv(0) = jv
             prime%tr(jt)%recw(0) = jr
             prime%tr(jt)%nbase = nbase
             prime%tr(jt)%nxd = 1
             prime%tr(jt)%nxs = 1
             if (ierr.eq.0) call cache_co_name(ierr, prime%tr(jt)%dstx(jx), handle, jv, jx)
             if (ierr.eq.0) call cache_co_name(ierr, prime%tr(jt)%srcx(jx), handle, jv, jx)
             prime%tr(jt)%msrc = msrc
             prime%tr(jt)%mdst = msrc
          enddo
       endif
    enddo
    call trace_err(ierr, fun=proc)
  end subroutine parse_convoy_loc

!!!_  & convoy_transform
  subroutine convoy_transform &
       & (ierr, mfx, prime, opts, jpos, npos)
    use TOUZA_Nio,only: cache_open_read, grp_suite, cache_var_size
    use TOUZA_Nio,only: cache_group_recs
    use TOUZA_Nio,only: cache_co_size, cache_co_all
    use TOUZA_Nio,only: show_cache
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: mfx
    type(convoy_t),  intent(inout) :: prime
    character(len=*),intent(in)    :: opts
    integer,         intent(inout) :: jpos
    integer,         intent(in)    :: npos

    character(len=lpath) :: fin
    integer :: hin, uout
    integer hopr
    integer jv,  nv, nx
    integer mvx
    character(len=litem) :: xin(0:lcvax-1)
    integer              :: ktb(0:lcvax-1)
    integer set_end, set_next

    ierr = 0
    mfx = 0
    hopr = 0

    ! COMMAND INPUT.... = OUTPUT INPUT... = OUTPUT

    loop_parse: do
       if (jpos.gt.npos) exit loop_parse
       call convoy_parse_set(ierr, set_end, set_next, uout, jpos, npos)
       if (ierr.ne.0) exit loop_parse
       do
          if (jpos.ge.set_end) then
             jpos = set_next
             exit
          endif
          if (ierr.eq.0) call get_param(ierr, fin, jpos, ' ')
          if (ierr.eq.0) call cache_open_read(ierr, hin, fin, flag=0)
          if (ierr.eq.0) then
             nv = cache_var_size(hin)
             mvx = 0
             do jv = 0, nv - 1
                nx = cache_co_size(hin, jv)
                if (nx.gt.lcvax) ierr = ERR_INSUFFICIENT_BUFFER
                if (ierr.eq.0) call cache_co_all(ierr, xin, hin, jv)
                if (ierr.eq.0) call match_prime(ierr, hopr, ktb, prime, xin, nx, opts)
                if (ierr.eq.0) call diag_match(ierr, hopr, ktb, prime, xin, nx, fin, hin, jv)
                if (ierr.eq.0) then
                   if (hopr.eq.transf_none) cycle
                endif
                if (ierr.eq.0) then
                   call convoy_transform_var &
                        & (ierr, &
                        &  uout, prime, hopr, &
                        &  nx,   ktb,   jv,   hin, jpos, set_end)
                endif
                if (ierr.eq.0) mvx = mvx + 1
             enddo
             if (ierr.eq.0) then
                if (mvx.gt.0) mfx = mfx + 1
             endif
          endif
          if (ierr.ne.0) exit loop_parse
          jpos = jpos + 1
       enddo
       if (ierr.ne.0) exit loop_parse
    enddo loop_parse

  end subroutine convoy_transform

!!!_  & init_prime
  subroutine init_prime(ierr, pr)
    implicit none
    integer,       intent(out)   :: ierr
    type(convoy_t),intent(inout) :: pr
    ! hard coded
    integer,parameter :: ntini = 2

    ierr = 0
    pr%nt = 0
    allocate(pr%tr(0:ntini-1), STAT=ierr)
    if (ierr.ne.0) ierr = ERR_ALLOCATION
  end subroutine init_prime
!!!_  & match_prime
  subroutine match_prime(ierr, hopr, xtb, prime, xin, nxi, opts)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: hopr         ! operator(transform) handle, not index
    integer,         intent(out) :: xtb(0:*)
    type(convoy_t),  intent(in)  :: prime
    character(len=*),intent(in)  :: xin(0:*)
    integer,         intent(in)  :: nxi
    character(len=*),intent(in)  :: opts

    integer jopr, jdir
    integer jti
    integer ntr
    integer jerr
    logical bm

    ierr = 0
    jopr = -1
    ntr = 0
    do jti = 0, prime%nt - 1
       bm = is_match_xset(xin, nxi, prime%tr(jti)%srcx, prime%tr(jti)%nxs)
       if (bm) bm = is_match_filter(prime%tr(jti), jti, opts)
       if (bm) then
          jopr = jti
          jdir = +1
          ntr = ntr + 1
       endif
    enddo
    if (ntr.eq.0) then
       do jti = 0, prime%nt - 1
          bm = is_match_xset(xin, nxi, prime%tr(jti)%dstx, prime%tr(jti)%nxd)
          if (bm) bm = is_match_filter(prime%tr(jti), jti, opts)
          if (bm) then
             jopr = jti
             jdir = -1
             ntr = ntr + 1
          endif
       enddo
    endif
    if (ntr.gt.1) then
       ierr = ERR_NOT_FOUND
       hopr = transf_err
       call message(ierr, 'multiple candidate.')
       call show_prime(jerr, prime)
    else if (ntr.eq.0) then
       hopr = transf_none
    else if (jdir.eq.0) then
       hopr = transf_j2handle(jopr, +1)
       call gen_exchange_table(ierr, xtb, hopr, prime, xin, nxi)
       ierr = ERR_PANIC
    else
       hopr = transf_j2handle(jopr, jdir)
       call gen_exchange_table(ierr, xtb, hopr, prime, xin, nxi)
    endif
  end subroutine match_prime
!!!_  & is_match_xset()
  logical function is_match_xset &
       & (xa, na, xb, nb) &
       & result(b)
    implicit none
    character(len=*),intent(in)  :: xa(0:*), xb(0:*)
    integer,         intent(in)  :: na, nb
    integer jb, j
    j = -1
    do jb = 0, nb - 1
       j = find_first(xa(0:na-1), xb(jb))
       if (j.lt.0) exit
    enddo
    b = j.ge.0
  end function is_match_xset

!!!_  & is_match_filter()
  logical function is_match_filter &
       & (tr, jopr, opts) &
       & result(b)
    implicit none
    type(transf_t),  intent(in) :: tr
    integer,         intent(in) :: jopr
    character(len=*),intent(in) :: opts
    integer jerr
    integer k
    b = .TRUE.
    if (opts.eq.' ') return
    call parse_number(jerr, k, opts)
    if (jerr.eq.0) then
       b = k.eq.jopr
    endif
  end function is_match_filter

!!!_  & gen_exchange_table
  subroutine gen_exchange_table(ierr, xtb, hopr, prime, xin, nxi)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: xtb(0:*)
    integer,         intent(in)  :: hopr         ! operator(transform) handle, not index
    type(convoy_t),  intent(in)  :: prime
    character(len=*),intent(in)  :: xin(0:*)
    integer,         intent(in)  :: nxi

    integer jopr, jdir
    integer jxi, jxt

    ierr = 0
    xtb(0:nxi-1) = -1

    jxt = -1
    jopr = transf_h2index(hopr)
    jdir = transf_h2dir(hopr)

    if (jdir.gt.0) then
       do jxi = 0, prime%tr(jopr)%nxs - 1
          jxt = find_first(xin(0:nxi-1), prime%tr(jopr)%srcx(jxi))
          if (jxt.lt.0) exit
          xtb(jxt) = jxi
       enddo
    else if (jdir.lt.0) then
       do jxi = 0, prime%tr(jopr)%nxd - 1
          jxt = find_first(xin(0:nxi-1), prime%tr(jopr)%dstx(jxi))
          if (jxt.lt.0) exit
          xtb(jxt) = jxi
       enddo
    else
       ierr = ERR_INVALID_PARAMETER
    endif
    if (ierr.eq.0) then
       if (jxt.lt.0) ierr = ERR_PANIC
    endif
  end subroutine gen_exchange_table
!!!_  & diag_match
  subroutine diag_match(ierr, hopr, xtb, pr, xin, nx, fin, hin, vid)
    use TOUZA_Nio,only: cache_var_name
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: hopr         ! operator(transform) handle, not index
    integer,         intent(in)  :: xtb(0:*)
    type(convoy_t),  intent(in)  :: pr
    character(len=*),intent(in)  :: xin(0:*)
    integer,         intent(in)  :: nx
    character(len=*),intent(in)  :: fin
    integer,         intent(in)  :: hin, vid
    integer jopr, jdir
    character(len=lmsg) :: txt, txts, txtd
    character(len=litem) :: vname

    call cache_var_name(ierr, vname, hin, vid)
    if (ierr.ne.0) vname = '(error)'
    ierr = 0

    if (hopr.eq.transf_none) then
       if (is_verbose(msglev_WARNING)) then
          call join_list(ierr, txts, xin(0:nx-1), sep=',', ldelim='{', rdelim='}')
101       format('No candidate for ', A, '/', A, A)
          write(txt, 101) trim(fin), trim(vname), trim(txts)
          call message(ierr, txt, trace=.FALSE.)
       endif
    endif
    if (is_verbose(msglev_NORMAL)) then
       call str_source_var(ierr, txts, vname, nx, xin, xtb)
       call str_dest_coset(ierr, txtd, nx, xin, xtb, pr, hopr)
       jopr = transf_h2index(hopr)
       jdir = transf_h2dir(hopr)
102    format('transform[', I0, '] ', A, '/', A, ' :: ', A)
105    format('transform[>>', I0, '] ', A, '/', A, ' > ', A)
106    format('transform[<<', I0, '] ', A, '/', A, ' > ', A)
       if (jdir.gt.0) then
          write(txt, 105, IOSTAT=ierr) jopr, trim(fin), trim(txts), trim(txtd)
       else if (jdir.lt.0) then
          write(txt, 106, IOSTAT=ierr) jopr, trim(fin), trim(txts), trim(txtd)
       else
          write(txt, 102, IOSTAT=ierr) hopr, trim(fin), trim(txts), trim(txtd)
       endif
       call message(ierr, txt)
    endif
  end subroutine diag_match
!!!_  & show_prime
  subroutine show_prime (ierr, pr, tag, u, levm)
    implicit none
    integer,         intent(out)         :: ierr
    type(convoy_t),  intent(in)          :: pr
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levm
    integer jt
    integer,parameter :: lset = 256
    integer,parameter :: lw = 16
    character(len=lset) :: xs, xd
    character(len=lset) :: txt, txt2
    character(len=lset) :: txtw(0:lw-1)
    integer jw, mw

    ierr = 0
! 103 format(A, &
!          & '[', I0, ':', I0, '] ', A, ' > ', A, 2x, I0, '[', I0, ']', ':', I0, '[', I0, ']')
! 102 format('[', I0, ':', I0, '] ', A, ' > ', A, 2x, I0, '[', I0, ']', ':', I0, '[', I0, ']')
113 format(A, &
         & '[', I0, '] ', '(', I0, ':', I0, ') ', A, ' > ', A, 2x, I0, '[', I0, ']')
112 format('[', I0, '] ', '(', I0, ':', I0, ') ', A, ' > ', A, 2x, I0, '[', I0, ']')
    do jt = 0, pr%nt - 1
       call gen_cset_str(xs, pr%tr(jt)%srcx, pr%tr(jt)%msrc, pr%tr(jt)%nxs)
       call gen_cset_str(xd, pr%tr(jt)%dstx, pr%tr(jt)%mdst, pr%tr(jt)%nxd)
       if (present(tag)) then
          write(txt, 113) trim(tag), jt, &
               & pr%tr(jt)%namin, pr%tr(jt)%namax, &
               & trim(xs), trim(xd), &
               & pr%tr(jt)%idjv, pr%tr(jt)%recj
       else
          write(txt, 112) jt, &
               & pr%tr(jt)%namin, pr%tr(jt)%namax, &
               & trim(xs), trim(xd), &
               & pr%tr(jt)%idjv, pr%tr(jt)%recj
       endif
101    format(I0, '[', I0, ']')
       mw = 0
       do jw = 0, pr%tr(jt)%nw - 1
          if (pr%tr(jt)%idwv(jw).lt.0) cycle
          write(txtw(mw), 101) pr%tr(jt)%idwv(jw), pr%tr(jt)%recw(jw)
          mw = mw + 1
          if (mw.ge.lw) exit
       enddo
       if (mw.ge.lw) then
          call join_list(ierr, txt2, txtw(0:mw-1), ldelim='{', rdelim='...}')
       else
          call join_list(ierr, txt2, txtw(0:mw-1), ldelim='{', rdelim='}')
       endif
       txt = trim(txt) // ' ' // trim(txt2)
       call message(ierr, txt, levm=msglev_ANYWAY)
       ! do jw = 0, pr%tr(jt)%nw - 1
       !    if (pr%tr(jt)%idwv(jw).lt.0) cycle
       !    if (ierr.eq.0) then
       !       if (present(tag)) then
       !          write(txt, 103) trim(tag), jt, jw, trim(xs), trim(xd), &
       !               & pr%tr(jt)%idjv, pr%tr(jt)%recj, pr%tr(jt)%idwv(jw), pr%tr(jt)%recw(jw)
       !       else
       !          write(txt, 102) jt, jw, trim(xs), trim(xd), &
       !               & pr%tr(jt)%idjv, pr%tr(jt)%recj, pr%tr(jt)%idwv(jw), pr%tr(jt)%recw(jw)
       !       endif
       !       call message(ierr, txt, levm=msglev_ANYWAY)
       !    endif
       ! enddo
    enddo
  end subroutine show_prime
!!!_  & gen_cset_str
  subroutine gen_cset_str(str, cname, cmem, nx, num)
    implicit none
    character(len=*),intent(out) :: str
    character(len=*),intent(in)  :: cname(0:*)
    integer,         intent(in)  :: cmem(0:*)
    integer,         intent(in)  :: nx
    logical,optional,intent(in)  :: num
    integer jerr
    integer jx
    integer,parameter :: lstr = litem * 2
    character(len=lstr) :: cs(0:nx-1)

101 format(A, '/', I0)
    if (choice(.TRUE., num)) then
       do jx = 0, nx - 1
          write(cs(jx), 101) trim(cname(jx)), cmem(jx)
       enddo
       call join_list(jerr, str, cs(0:nx-1), sep=',')
    else
       call join_list(jerr, str, cname(0:nx-1), sep=',')
    endif
  end subroutine gen_cset_str

!!!_  & settle_prime
  subroutine settle_prime(ierr, pr, handle)
    use TOUZA_Nio,only: show_cache
    implicit none
    integer,       intent(out)   :: ierr
    type(convoy_t),intent(inout) :: pr
    integer,       intent(in)    :: handle   ! used at error

    integer nrem
    integer jt, ja, jb
    character(len=lmsg) :: xd, txt
    integer jerr

    ierr = 0
    nrem = 0
    do jt = 0, pr%nt - 1
       if (pr%tr(jt)%nxs.le.0) nrem = nrem + 1
    enddo
    if (nrem.eq.2) then
       ja = -1
       jb = -1
       do jt = 0, pr%nt - 1
          if (pr%tr(jt)%nxs.le.0) then
             ja = jt
             exit
          endif
       enddo
       do jt = ja + 1, pr%nt - 1
          if (pr%tr(jt)%nxs.le.0) then
             jb = jt
             exit
          endif
       enddo
       if (ja.lt.0 .or. jb.lt.0) ierr = ERR_PANIC
       if (ierr.eq.0) then
          pr%tr(ja)%nxs  = pr%tr(jb)%nxd
          pr%tr(ja)%msrc = pr%tr(jb)%mdst
          pr%tr(ja)%srcx = pr%tr(jb)%dstx
          pr%tr(jb)%nxs  = pr%tr(ja)%nxd
          pr%tr(jb)%msrc = pr%tr(ja)%mdst
          pr%tr(jb)%srcx = pr%tr(ja)%dstx
       endif
    else if (nrem.gt.0) then
       ierr = ERR_INVALID_PARAMETER
101    format('cannot detect source coordinate set: ', I0, 1x, A)
       do jt = 0, pr%nt - 1
          if (pr%tr(jt)%nxs.le.0) then
             call gen_cset_str(xd, pr%tr(jt)%dstx, pr%tr(jt)%mdst, pr%tr(jt)%nxd)
             write(txt, 101) jt, trim(xd)
             call message(ierr, txt)
          endif
       enddo
       call show_prime(jerr, pr)
       call show_cache(jerr, handle)
    endif
  end subroutine settle_prime
!!!_  & convoy_transform_var
  subroutine convoy_transform_var &
       & (ierr, &
       &  uout, pr,   hopr, &
       &  nx,   ktb,  vid,  hbase, jpos, set_end, &
       &  rbgn, rend, vmiss)
    use TOUZA_Nio,only: cache_open_read, grp_suite, cache_var_size
    implicit none
    integer,        intent(out)          :: ierr
    type(convoy_t), intent(inout),target :: pr
    integer,        intent(in)           :: uout
    integer,        intent(in)           :: hopr
    integer,        intent(in)           :: nx
    integer,        intent(in)           :: ktb(0:*)
    integer,        intent(in)           :: vid
    integer,        intent(in)           :: hbase
    integer,        intent(inout)        :: jpos
    integer,        intent(in)           :: set_end
    integer,        intent(in),optional  :: rbgn, rend
    real(kind=KTGT),intent(in),optional  :: vmiss

    integer jopr
    integer namin, namax
    integer,parameter :: lin = 16
    integer nin
    integer hin(0:lin-1)
    character(len=lpath) :: fin

    ierr = 0
    jopr = transf_h2index(hopr)
    ierr = min(0, jopr)
    if (ierr.eq.0) then
       namin = pr%tr(jopr)%namin
       namax = pr%tr(jopr)%namax
       nin = 0
       hin(nin) = hbase
       nin = nin + 1
       do
          if (nin.gt.namax) exit
          jpos = jpos + 1
          if (jpos.ge.set_end) exit
          if (ierr.eq.0) call get_param(ierr, fin, jpos, ' ')
          if (ierr.eq.0) then
             if (fin.eq.param_skip) then
                hin(nin) = -1
             else
                call cache_open_read(ierr, hin(nin), fin, flag=0)
             endif
             nin = nin + 1
          endif
       enddo
       hin(nin:namax-1) = -1
    endif
    if (ierr.eq.0) then
       select case (pr%tr(jopr)%k)
       case(xcmd_fdplain,xcmd_fdcycle)
          call convoy_transform_fd &
               & (ierr, &
               &  uout, pr,  hopr, &
               &  nx,   ktb, vid,  hin(0), rbgn, rend, vmiss)
       case(xcmd_csr)
          call convoy_transform_csr &
               & (ierr, &
               &  uout, pr,  hopr, &
               &  nx,   ktb, vid,  hin, namax, rbgn, rend, vmiss)
       case default
          ierr = ERR_INVALID_SWITCH
       end select
    endif
    return
  end subroutine convoy_transform_var

!!!_  & convoy_transform_fd
  subroutine convoy_transform_fd &
       & (ierr, &
       &  uout, pr,  hopr, &
       &  nx,   ktb, vid,  hin, rbgn, rend, vmiss)
    use TOUZA_Nio,only: cache_var_read, cache_get_attr
    use TOUZA_Nio,only: cache_group_recs, cache_read_header
    use TOUZA_Nio,only: cache_co_len
    implicit none
    integer,        intent(out)          :: ierr
    type(convoy_t), intent(inout),target :: pr
    integer,        intent(in)           :: uout
    integer,        intent(in)           :: hopr
    integer,        intent(in)           :: nx
    integer,        intent(in)           :: ktb(0:*)
    integer,        intent(in)           :: vid
    integer,        intent(in)           :: hin
    integer,        intent(in),optional  :: rbgn, rend
    real(kind=KTGT),intent(in),optional  :: vmiss

    integer msrc
    integer ldest
    real(kind=KTGT),allocatable :: vsrc(:), vdest(:)
    real(kind=KTGT),allocatable :: c(:)
    real(kind=KTGT) :: d, a, b

    integer jopr, jdir
    integer jrec, nrec, jrb, jre

    type(transf_t),pointer :: ptr
    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    real(kind=KTGT),parameter :: ONE = 1.0_KTGT
    real(kind=KTGT),parameter :: mold = ZERO

    real(kind=KTGT) :: f

    character(len=litem) :: unit
    integer nbase
    integer jw, nw
    integer jxp
    integer,parameter :: nxp = 1
    integer st(0:nxp-1), co(0:nxp-1)
    integer jh
    integer stride

    ierr = 0
    jopr = transf_h2index(hopr)
    jdir = transf_h2dir(hopr)
    ierr = min(0, jopr)
    if (ierr.eq.0) then
       jxp = 0

       ptr => pr%tr(jopr)
       nbase = ptr%nbase
       msrc = pr%tr(jopr)%msrc(jxp)

       jw = 0
       nw = ptr%nw
       st(jxp) = 0
       co(jxp) = nbase
       if (.not.allocated(ptr%wcsr)) then
          allocate(c(0:nbase-1), &
               & ptr%icsr(0:msrc*nw - 1), ptr%wcsr(0:msrc-1, 0:nw-1), STAT=ierr)
          if (ierr.eq.0) then
             call cache_var_read(ierr, c, ptr%h, ptr%idwv(jw), ptr%recw(jw), st, co)
          endif
          if (ierr.eq.0) then
             call cache_get_attr(ierr, unit, hi_UNIT, ptr%h, ptr%idwv(jw), ptr%recw(jw))
          endif
          if (ierr.eq.0) then
             call upcase(unit)
             if (unit.eq.'DEG'.or.unit.eq.'DEGREE') then
                f = rad2deg(ONE)
             else
                f = ONE
             endif
          endif
          ! x0 x1 x2
          ! x1-x0 x2-x1
          ! 1/(x1-x0) * (x2-x1)/(x2-x0) [V1-V0]
          ! 1/(x2-x1) * (x1-x0)/(x2-x0) [V2-V1]
          if (ierr.eq.0) then
             do jh = 0, msrc - 2
                ptr%wcsr(jh,   2) = c(jh+1) - c(jh)
                ptr%wcsr(jh+1, 1) = c(jh+1) - c(jh)
                ptr%icsr(2 * msrc + jh)   = + 1
                ptr%icsr(1 * msrc + jh+1) = - 1
                ptr%icsr(0 * msrc + jh)   = 0
             enddo
             if (pr%tr(jopr)%k.eq.xcmd_fdcycle) then
                jh = 0
                ptr%wcsr(jh, 1) = c(nbase-1) - c(nbase-2)
                ptr%icsr(1 * msrc + jh) = msrc - 1
                ptr%icsr(0 * msrc + jh) = 0
                jh = msrc - 1
                ptr%wcsr(jh, 2) = c(1) - c(0)
                ptr%icsr(2 * msrc + jh) = 1 - msrc
                ptr%icsr(0 * msrc + jh) = 0
                ! jh = nbase - 1
                ! ptr%wcsr(jh, 0:2) = ZERO
                ! ptr%icsr(0 * nbase + jh) = jh
                ! ptr%icsr(1 * nbase + jh) = jh
                ! ptr%icsr(2 * nbase + jh) = jh
             else
                jh = 0
                ptr%wcsr(jh, 1) = ZERO
                ptr%icsr(1 * msrc + jh) = 0
                ptr%icsr(0 * msrc + jh) = 0
                jh = msrc - 1
                ptr%wcsr(jh, 2) = ZERO
                ptr%icsr(2 * msrc + jh) = 0
                ptr%icsr(0 * msrc + jh) = 0
             endif
             do jh = 0, msrc - 1
                a = ptr%wcsr(jh, 1)
                b = ptr%wcsr(jh, 2)
                d = a + b
                if (d.eq.ZERO) then
                   ptr%wcsr(jh, 0:2) =  ZERO
                else if (a.eq.ZERO) then
                   ptr%wcsr(jh, 1) =  ZERO
                   ptr%wcsr(jh, 2) =  + ONE / b
                   ptr%wcsr(jh, 0) =  - (ptr%wcsr(jh, 1) + ptr%wcsr(jh, 2))
                else if (b.eq.ZERO) then
                   ptr%wcsr(jh, 1) =  - ONE / a
                   ptr%wcsr(jh, 2) =  ZERO
                   ptr%wcsr(jh, 0) =  - (ptr%wcsr(jh, 1) + ptr%wcsr(jh, 2))
                else
                   ptr%wcsr(jh, 1) =  - ONE / a * (b / d)
                   ptr%wcsr(jh, 2) =  + ONE / b * (a / d)
                   ptr%wcsr(jh, 0) =  - (ptr%wcsr(jh, 1) + ptr%wcsr(jh, 2))
                endif
             enddo
             ptr%wcsr(:, :) = ptr%wcsr(:, :) * f
          endif
          ! do jh = 0, msrc - 1
          !    write(*, *) jh, c(jh), &
          !         & ptr%icsr((/jh, msrc+jh, 2*msrc+jh/)), &
          !         & ptr%wcsr(jh, :)
          ! enddo
          if (ierr.eq.0) deallocate(c, STAT=ierr)
       endif
    endif
    ! if (ierr.eq.0) then
    !    call convoy_array_shape &
    !         & (stin,  coin,  memin, &
    !         &  mdest, ldest, msrc, ktb, nx, hin, vid, ptr)
    !    write(*, *) stin
    !    write(*, *) coin
    !    write(*, *) memin
    ! endif
    if (ierr.eq.0) then
       call convoy_array_stride &
            & (stride, ldest, &
            &  ktb,    nx,    hin, vid)
       allocate(vsrc(0:ldest-1), vdest(0:ldest-1), STAT=ierr)
    endif
    if (ierr.eq.0) then
       jrb = choice(0, rbgn)
       jre = choice(-1, rend)
       nrec = cache_group_recs(hin, vid)
       if (jrb.lt.0) jrb = nrec + jrb
       if (jre.lt.0) jre = nrec + jre + 1
       do jrec = jrb, jre - 1
          call convoy_read_transform_rs_fd &
               & (ierr,  vdest,  vsrc,  &
               &  ldest, stride, &
               &  hin,   vid,    jrec,  &
               &  ptr,   vmiss)
          if (ierr.eq.0) then
             call convoy_write &
                  & (ierr, &
                  &  uout, vdest, ldest, ktb, nx, hin, vid, jrec, ptr, perm=.FALSE.)
          endif
          if (ierr.ne.0) exit
       enddo
    endif
    if (ierr.eq.0) deallocate(vsrc, vdest, STAT=ierr)
  end subroutine convoy_transform_fd

!!!_  & convoy_read_transform_rs_fd - with reshape
  subroutine convoy_read_transform_rs_fd &
       & (ierr,  vdest,  wsrc, &
       &  mdest, stride, &
       &  hin,   vid,    rec,  &
       &  tr,    vmiss)
    use TOUZA_Nio,only: cache_var_read
    use TOUZA_Nio,only: cache_get_attr, hi_MISS
    use Jmz_coor,only: gen_stride, pack_mems, flat_index_l2p
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,        intent(out)   :: ierr
    real(kind=KTGT),intent(out)   :: vdest(0:*)
    real(kind=KTGT),intent(out)   :: wsrc(0:*)
    integer,        intent(in)    :: mdest
    integer,        intent(in)    :: stride
    integer,        intent(in)    :: hin, vid, rec
    type(transf_t), intent(in)    :: tr
    real(kind=KTGT),intent(in),optional :: vmiss
    integer j,  jhb, jhe
    integer jt, jm
    integer msrc
    integer jerr
    integer jc0
    integer jw0, jw1, nw, js0, js1
    real(kind=KTGT) :: undef

    ierr = 0

    if (present(vmiss)) then
       undef = vmiss
    else
       ! ignore error
       call cache_get_attr(jerr, undef, hi_MISS, hin, vid, rec)
       if (jerr.ne.0) undef = ZERO
    endif

    ! call gen_stride(pstr, nx, mem, ktb)

    if (ierr.eq.0) call cache_var_read(ierr, wsrc, hin, vid, rec)
    if (ierr.eq.0) then
       jc0 = 0
       msrc = tr%msrc(jc0)
       nw = tr%nw
       if (stride.eq.1) then
          jw0 = 0
          do j = 0, mdest - 1
             jt = mod(j, msrc)
             js0 = tr%icsr(0 * msrc + jt)
             ! write(*, *) j, js0, jt, wsrc(j + js0), tr%wcsr(jt, jw0)
             vdest(j) = wsrc(j + js0) * tr%wcsr(jt, jw0)
          enddo
          do jw0 = 1, nw - 1, 2
             jw1 = jw0 + 1
             do j = 0, mdest - 1
                jt = mod(j, msrc)
                js0 = tr%icsr(jw0 * msrc + jt)
                js1 = tr%icsr(jw1 * msrc + jt)
                vdest(j) = vdest(j) + &
                     & (+ wsrc(j + js0) * tr%wcsr(jt, jw0) &
                     &  + wsrc(j + js1) * tr%wcsr(jt, jw1))
             enddo
          enddo
       else
          jw0 = 0
          do jm = 0, mdest / stride - 1
             jt = mod(jm, msrc)
             js0 = tr%icsr(0 * msrc + jt) * stride
             jhb = jm * stride
             jhe = jhb + stride
             ! write(*, *) jhb, jhe, js0, jt
             vdest(jhb:jhe-1) = wsrc(jhb + js0:jhe + js0 - 1) * tr%wcsr(jt, jw0)
          enddo
          do jw0 = 1, nw - 1, 2
             jw1 = jw0 + 1
             do jm = 0, mdest / stride - 1
                jt = mod(jm, msrc)
                js0 = tr%icsr(jw0 * msrc + jt) * stride
                js1 = tr%icsr(jw1 * msrc + jt) * stride
                jhb = jm * stride
                jhe = jhb + stride
                vdest(jhb:jhe-1) = vdest(jhb:jhe-1) + &
                     & (+ wsrc(jhb + js0:jhe + js0 - 1) * tr%wcsr(jt, jw0) &
                     &  + wsrc(jhb + js1:jhe + js1 - 1) * tr%wcsr(jt, jw1))
             enddo
          enddo
       endif
    endif
  end subroutine convoy_read_transform_rs_fd

!!!_  & convoy_array_stride
  subroutine convoy_array_stride &
       & (str, ldest, ktb, nx, hin, vid)
    use TOUZA_Nio,only: cache_co_len, cache_var_len
    implicit none
    integer,       intent(out) :: str
    integer,       intent(out) :: ldest
    integer,       intent(in)  :: ktb(0:*)
    integer,       intent(in)  :: nx
    integer,       intent(in)  :: hin, vid
    integer jx

    ldest = cache_var_len(hin, vid)
    str = 1
    do jx = 0, nx - 1
       if (ktb(jx).ge.0) exit
       str = str * cache_co_len(hin, vid, jx)
    enddo
  end subroutine convoy_array_stride

!!!_  & convoy_transform_csr
  subroutine convoy_transform_csr &
       & (ierr, &
       &  uout, pr,   hopr, &
       &  nx,   ktb,  vid,  hin, nin, &
       &  rbgn, rend, vmiss)
    use TOUZA_Nio,only: cache_group_recs, cache_read_header
    use TOUZA_Nio,only: cache_sparse_review, cache_restore_csr
    use TOUZA_Nio,only: cache_co_len
    implicit none
    integer,        intent(out)          :: ierr
    type(convoy_t), intent(inout),target :: pr
    integer,        intent(in)           :: uout
    integer,        intent(in)           :: hopr
    integer,        intent(in)           :: nx
    integer,        intent(in)           :: ktb(0:*)
    integer,        intent(in)           :: vid
    integer,        intent(in)           :: hin(0:*)
    integer,        intent(in)           :: nin
    integer,        intent(in),optional  :: rbgn, rend
    real(kind=KTGT),intent(in),optional  :: vmiss

    ! integer,        allocatable :: htrns(:)  ! qjds-type only
    ! integer,        allocatable :: itrns(:)  ! shared for crs qjds
    ! real(kind=KTGT),allocatable :: wtrns(:)
    integer msrc
    integer ldest, mdest
    real(kind=KTGT),allocatable :: vsrc(:, :)
    real(kind=KTGT),allocatable :: vdest(:)

    integer nsv
    integer jopr, jdir
    integer jrec, nrec, jrb, jre

    integer :: stin(0:nx-1), coin(0:nx-1), memin(0:nx-1)
    type(transf_t),pointer :: ptr
    real(kind=KTGT),parameter :: mold = 0.0_KTGT
    integer nbase, mcols, npack, colc
    integer ksw
    integer hbase

    ierr = 0
    jopr = transf_h2index(hopr)
    jdir = transf_h2dir(hopr)
    ierr = min(0, jopr)

    hbase = hin(0)
    if (ierr.eq.0) then
       ptr => pr%tr(jopr)
       nbase = ptr%nbase
       mcols = ptr%mcols
       npack = ptr%npack
       colc  = ptr%colc
       ! write(*, *) nbase, mcols, npack, colc
    endif
    if (ierr.eq.0) call convoy_restore_csr(ierr, pr, hopr)
    if (ierr.eq.0) then
       ksw = is_ordered_cset(ptr, ktb, nx)
       ierr = min(0, ksw)
    endif
    if (ierr.eq.0) then
       call convoy_array_shape &
            & (stin,  coin,  memin, &
            &  mdest, ldest, msrc, ktb, nx, hbase, vid, ptr)
       allocate(vsrc(0:msrc-1, 0:nin-1), vdest(0:ldest-1), STAT=ierr)
    endif
    if (ierr.eq.0) then
       jrb = choice(0, rbgn)
       jre = choice(-1, rend)
       nrec = cache_group_recs(hbase, vid)
       if (jrb.lt.0) jrb = nrec + jrb
       if (jre.lt.0) jre = nrec + jre + 1
       do jrec = jrb, jre - 1
          if (ksw.eq.cset_ordered) then
             call convoy_read_transform_csr &
                  & (ierr,  vdest, vsrc,  &
                  &  mdest, ldest, msrc,  nsv,  &
                  &  stin,  coin,  memin, ktb,  nx, &
                  &  hin,   nin,   vid,   jrec,  &
                  &  ptr,   vmiss)
          else
             call convoy_read_transform_rs_csr &
                  & (ierr,  vdest, vsrc,  &
                  &  mdest, ldest, msrc,  &
                  &  stin,  coin,  memin, ktb, nx, &
                  &  hin,   nin,   vid,   jrec,  &
                  &  ptr,   vmiss)
          endif
          if (ierr.eq.0) then
             call convoy_write &
                  & (ierr, &
                  &  uout, vdest, ldest, ktb, nx, hbase, vid, jrec, ptr)
          endif
          if (ierr.ne.0) exit
       enddo
    endif
    if (ierr.eq.0) deallocate(vsrc, vdest, STAT=ierr)
  end subroutine convoy_transform_csr

!!!_  & convoy_array_shape
  subroutine convoy_array_shape &
       & (st,    co,    mem, &
       &  mdest, ldest, msrc, ktb, nx, hin, vid, tr)
    use TOUZA_Nio,only: cache_co_len
    implicit none
    integer,       intent(out) :: st(0:*)
    integer,       intent(out) :: co(0:*)
    integer,       intent(out) :: mem(0:*)
    integer,       intent(out) :: mdest, ldest
    integer,       intent(out) :: msrc
    integer,       intent(in)  :: ktb(0:*)
    integer,       intent(in)  :: nx
    integer,       intent(in)  :: hin, vid
    type(transf_t),intent(in)  :: tr
    integer jx

    msrc  = PRODUCT(tr%msrc(0:tr%nxs-1))
    mdest = PRODUCT(tr%mdst(0:tr%nxd-1))
    ldest = mdest
    do jx = 0, nx - 1
       st(jx) = 0
       mem(jx) = cache_co_len(hin, vid, jx)
       if (ktb(jx).lt.0) then
          ldest = ldest * mem(jx)
          co(jx) = 1
       else
          co(jx) = mem(jx)
       endif
    enddo
  end subroutine convoy_array_shape
!!!_  & convoy_restore_csr
  subroutine convoy_restore_csr &
       & (ierr, &
       &  pr, hopr)
    use TOUZA_Nio,only: cache_restore_csr
    implicit none
    integer,       intent(out)          :: ierr
    type(convoy_t),intent(inout),target :: pr
    integer,       intent(in)           :: hopr

    integer jopr, jdir
    type(transf_t),pointer :: ptr
    real(kind=KTGT),parameter :: mold = 0.0_KTGT
    integer nbase, mcols, npack, colc
    integer jw, nw

    jopr = transf_h2index(hopr)
    jdir = transf_h2dir(hopr)
    ierr = min(0, jopr)
    if (ierr.eq.0) then
       ptr => pr%tr(jopr)
       nbase = ptr%nbase
       mcols = ptr%mcols
       npack = ptr%npack
       colc  = ptr%colc
       nw    = ptr%nw
    endif
    if (ierr.eq.0) then
       if (.not.allocated(ptr%hcsr)) then
          allocate(ptr%hcsr(0:nbase), &
               &   ptr%icsr(0:npack-1), ptr%wcsr(0:npack-1, 0:nw-1), STAT=ierr)
          if (ierr.eq.0) then
             call cache_restore_csr &
                  & (ierr,  ptr%icsr, ptr%hcsr, &
                  &  ptr%h, ptr%idjv, ptr%recj, colc)
          endif
          do jw = 0, nw - 1
             if (ierr.eq.0) then
                call cache_restore_csr &
                     & (ierr,  ptr%wcsr(:, jw), ptr%hcsr, &
                     &  ptr%h, ptr%idwv(jw),    ptr%recw(jw), colc)
             endif
          enddo
       endif
       if (ierr.ne.0) ierr = ERR_ALLOCATION
    endif
  end subroutine convoy_restore_csr
!!!_  & convoy_read_transform_csr
  subroutine convoy_read_transform_csr &
       & (ierr,  vdest, wsrc, &
       &  mdest, ldest, msrc, nsv,  &
       &  st,    co,    mem,  ktb,  nx,   &
       &  hin,   nin,   vid,  rec,  &
       &  tr,    vmiss)
    use TOUZA_Nio,only: cache_var_read
    use TOUZA_Nio,only: cache_get_attr, hi_MISS
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,        intent(out)   :: ierr
    integer,        intent(in)    :: mdest, ldest, msrc, nsv
    real(kind=KTGT),intent(out)   :: vdest(0:*)
    real(kind=KTGT),intent(out)   :: wsrc(0:msrc-1, 0:*)
    integer,        intent(inout) :: st(0:*)
    integer,        intent(in)    :: co(0:*), mem(0:*)
    integer,        intent(in)    :: ktb(0:*)
    integer,        intent(in)    :: nx
    integer,        intent(in)    :: hin(0:*), nin
    integer,        intent(in)    :: vid, rec
    type(transf_t), intent(in)    :: tr
    real(kind=KTGT),intent(in),optional :: vmiss
    integer jlp
    integer jdp
    integer jsrc
    integer j, jc, jcb, jce
    integer nbase
    real(kind=KTGT) :: undef, dnm
    integer jerr
    integer jin, jdnm
    integer jdbg

    ierr = 0
    jdnm = 0

    if (present(vmiss)) then
       undef = vmiss
    else
       ! ignore error
       call cache_get_attr(jerr, undef, hi_MISS, hin(0), vid, rec)
       if (jerr.ne.0) undef = ZERO
    endif

    jlp = 0
    jdp = 0
    nbase = tr%nbase
    outer: do
       if (jlp.ge.nx) exit
       ! call cache_var_read(ierr, wsrc(0:msrc-1, 0), hin, vid, rec, st, co)
       do jin = 0, nin - 1
          if (hin(jin).ge.0) then
             if (ierr.eq.0) call cache_var_read(ierr, wsrc(0:msrc-1, jin), hin(jin), vid, rec, st, co)
          else
             wsrc(0:msrc-1, jin) = 0.0_KTGT
          endif
       enddo
       if (ierr.ne.0) exit
       vdest(jdp:jdp+mdest-1) = undef
       do j = 0, nbase - 1
          jcb = tr%hcsr(j)
          jce = tr%hcsr(j + 1)
          ! vdest(jdp + j) = SUM(wsrc(tr%icsr(jcb:jce-1)) * tr%wcsr(jcb:jce-1))
          if (jce-jcb.eq.1) then
             jc = jcb
             jsrc = tr%icsr(jc)
             if (jsrc.lt.msrc) then
                ! vdest(jdp + j) = wsrc(jsrc) * tr%wcsr(jc)
                dnm = tr%wcsr(jc, jdnm)
                vdest(jdp + j) = &
                     & + SUM(wsrc(jsrc, 0:nin-1) * tr%wcsr(jc, 0:nin-1))
                ! write(*, *) jdp + j, wsrc(jsrc, 0:2) * tr%wcsr(jc, 0:2)
                vdest(jdp + j) = vdest(jdp + j) / dnm
             endif
          else
             dnm = SUM(tr%wcsr(jcb:jce-1, jdnm))
             if (dnm.eq.ZERO) then
                continue
             else
                vdest(jdp + j) = ZERO
                do jc = jcb, jce - 1
                   jsrc = tr%icsr(jc)
                   if (jsrc.lt.msrc) then
                      vdest(jdp + j) = vdest(jdp + j) &
                           & + SUM(wsrc(jsrc, 0:nin-1) * tr%wcsr(jc, 0:nin-1))
                      ! vdest(jdp + j) = vdest(jdp + j) &
                      !      & + wsrc(jsrc, 0) * tr%wcsr(jc, jw0) &
                      !      & + wsrc(jsrc, 1) * tr%wcsr(jc, jw0+1) &
                      !      & + wsrc(jsrc, 2) * tr%wcsr(jc, jw0+2)
                      ! write(*, *) jsrc, wsrc(jsrc, 0:2) * tr%wcsr(jc, 0:2)
                   endif
                enddo
                vdest(jdp + j) = vdest(jdp + j) / dnm

                ! jdbg = jdp+j
                ! do jc = jcb, jce - 1
                !    jsrc = tr%icsr(jc)
                !    if (jsrc.eq.7871) then
                !       write(*, *) 'DBG:spc:', jdbg, jsrc, jc, &
                !            & wsrc(jsrc, 2), tr%wcsr(jc, 2), wsrc(jsrc, 2) * tr%wcsr(jc, 2)
                !    endif
                ! enddo
                ! if (ANY(jdbg.eq.(/8126, 8127, 8128/))) then
                !    write(*, *) 'DBG:out:', jdbg, vdest(jdbg), dnm
                !    do jc = jcb, jce - 1
                !       jsrc = tr%icsr(jc)
                !       if (jsrc.lt.msrc) then
                !          write(*, *) 'DBG:src:', jsrc, jc, &
                !               & wsrc(jsrc, 2), tr%wcsr(jc, 2), wsrc(jsrc, 2) * tr%wcsr(jc, 2)
                !       endif
                !    enddo
                ! endif
             endif
          endif
       enddo
       call next_slice(jlp, st, mem, ktb, nx)
       if (jlp.lt.0) exit
       jdp = jdp + mdest
    enddo outer

  end subroutine convoy_read_transform_csr

!!!_  & convoy_read_transform_rs_csr - with reshape
  subroutine convoy_read_transform_rs_csr &
       & (ierr,  vdest, wsrc, &
       &  mdest, ldest, msrc, &
       &  st,    co,    mem,  ktb, nx, &
       &  hin,   nin,   vid,  rec,  &
       &  tr,    vmiss)
    use TOUZA_Nio,only: cache_var_read
    use TOUZA_Nio,only: cache_get_attr, hi_MISS
    use Jmz_coor,only: gen_stride, pack_mems, flat_index_l2p
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,        intent(out)   :: ierr
    integer,        intent(in)    :: mdest, ldest, msrc
    real(kind=KTGT),intent(out)   :: vdest(0:*)
    real(kind=KTGT),intent(out)   :: wsrc(0:msrc, 0:*)
    integer,        intent(inout) :: st(0:*)
    integer,        intent(in)    :: co(0:*), mem(0:*)
    integer,        intent(in)    :: ktb(0:*)
    integer,        intent(in)    :: nx
    integer,        intent(in)    :: hin(0:*), nin
    integer,        intent(in)    :: vid, rec
    type(transf_t), intent(in)    :: tr
    real(kind=KTGT),intent(in),optional :: vmiss
    integer jlp
    integer jdp
    integer j, jcb, jce, jc, jrs
    integer nbase
    integer nxs
    integer pstr(0:nx)
    integer jerr
    real(kind=KTGT) :: undef, dnm
    integer jin
    integer jdnm

    ierr = 0
    jdnm = 0

    if (present(vmiss)) then
       undef = vmiss
    else
       ! ignore error
       call cache_get_attr(jerr, undef, hi_MISS, hin(0), vid, rec)
       if (jerr.ne.0) undef = ZERO
    endif

    nxs = tr%nxs
    call gen_stride(pstr, nx, mem, ktb)

    jlp = 0
    jdp = 0
    nbase = tr%nbase
    outer: do
       if (jlp.ge.nx) exit
       do jin = 0, nin - 1
          if (hin(jin).ge.0) then
             if (ierr.eq.0) call cache_var_read(ierr, wsrc(0:msrc-1, jin), hin(jin), vid, rec, st, co)
          else
             wsrc(0:msrc-1, jin) = 0.0_KTGT
          endif
       enddo
       ! call cache_var_read(ierr, wsrc, hin, vid, rec, st, co)
       if (ierr.ne.0) exit
       vdest(jdp:jdp+mdest-1) = undef
       do j = 0, nbase - 1
          jcb = tr%hcsr(j)
          jce = tr%hcsr(j + 1)
          if (jce-jcb.eq.1) then
             jc = jcb
             jrs = flat_index_l2p(tr%icsr(jc), tr%msrc, nxs, pstr)
             if (jrs.lt.msrc) then
                dnm = tr%wcsr(jc, jdnm)
                ! vdest(jdp + j) = wsrc(jrs) * tr%wcsr(jc)
                vdest(jdp + j) = &
                     & + SUM(wsrc(jrs, 0:nin-1) * tr%wcsr(jc, 0:nin-1))
                ! write(*, *) jdp + j, wsrc(jsrc, 0:2) * tr%wcsr(jc, 0:2)
                vdest(jdp + j) = vdest(jdp + j) / dnm
                ! vdest(jdp + j) = wsrc(jrs)
             endif
          else
             dnm = SUM(tr%wcsr(jcb:jce-1, jdnm))
             if (dnm.eq.ZERO) then
                continue
             else
                vdest(jdp+j) = ZERO
                do jc = jcb, jce - 1
                   jrs = flat_index_l2p(tr%icsr(jc), tr%msrc, nxs, pstr)
                   if (jrs.lt.msrc) then
                      vdest(jdp + j) = vdest(jdp + j) &
                           & + SUM(wsrc(jrs, 0:nin-1) * tr%wcsr(jc, 0:nin-1))
                   endif
                enddo
                ! do jc = jcb, jce - 1
                !    jrs = flat_index_l2p(tr%icsr(jc), tr%msrc, nxs, pstr)
                !    if (jrs.lt.msrc) then
                !       vdest(jdp + j) = vdest(jdp + j) + wsrc(jrs) * tr%wcsr(jc, jw0)
                !    endif
                ! enddo
                vdest(jdp + j) = vdest(jdp + j) / dnm
             endif
          endif
       enddo
       call next_slice(jlp, st, mem, ktb, nx)
       if (jlp.lt.0) exit
       jdp = jdp + mdest
    enddo outer

  end subroutine convoy_read_transform_rs_csr

!!!_  & next_slice
  subroutine next_slice(jlp, st, mem, ktb, nx)
    implicit none
    integer,intent(inout) :: jlp
    integer,intent(inout) :: st(0:*)
    integer,intent(in)    :: mem(0:*)
    integer,intent(in)    :: ktb(0:*)
    integer,intent(in)    :: nx
    do
       if (jlp.ge.nx) then
          jlp = -1
          return
       endif
       if (ktb(jlp).ge.0) then
          jlp = jlp + 1
          cycle
       endif
       st(jlp) = st(jlp) + 1
       if (st(jlp).lt.mem(jlp)) return
       st(jlp) = 0
       jlp = jlp + 1
    enddo
    jlp = 0
    ! 0 x x 1
  end subroutine next_slice
!!!_  & convoy_write
  subroutine convoy_write &
       & (ierr,  &
       &  uout,  vdest, ldest, &
       &  ktb,   nx,    href,  vid, rec,  tr, perm)
    use TOUZA_Nio,only: cache_read_header
    use TOUZA_Nio,only: cache_co_name, cache_co_range
    use TOUZA_Nio,only: put_item, hi_DFMT, REC_BIG
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: uout
    real(kind=KTGT), intent(in)  :: vdest(0:*)
    integer,         intent(in)  :: ldest
    integer,         intent(in)  :: href
    integer,         intent(in)  :: vid, rec
    integer,         intent(in)  :: ktb(0:*)
    integer,         intent(in)  :: nx
    type(transf_t),  intent(in)  :: tr
    logical,optional,intent(in)  :: perm   ! enable permutation (default true)
    integer krect
    character(len=litem) :: head(nitem)
    character(len=litem) :: xname
    integer jx, jj, ibgn, iend
    logical bp

    ierr = 0
    bp = choice(.TRUE., perm)
    if (uout.lt.0) then
       continue
    else
       call cache_read_header &
            & (ierr, head, href, vid, rec)
       if (ierr.eq.0) call put_item(ierr, head, 'UR8',  hi_DFMT)
       if (bp) then
          do jx = 0, tr%nxd - 1
             ibgn = 0
             iend = tr%mdst(jx)
             if (ierr.eq.0) then
                call put_header_cprop(ierr, head, tr%dstx(jx), (/ibgn+1, iend/), jx + 1)
             endif
          enddo
          jj = tr%nxd
          do jx = 0, nx - 1
             if (ktb(jx).lt.0) then
                call cache_co_name(ierr, xname, href, vid, jx)
                call cache_co_range(ierr, ibgn, iend, href, vid, jx)
                call put_header_cprop(ierr, head, xname, (/ibgn+1, iend/), jj + 1)
                jj = jj + 1
             endif
          enddo
       else
          do jx = 0, nx - 1
             call cache_co_name(ierr, xname, href, vid, jx)
             call cache_co_range(ierr, ibgn, iend, href, vid, jx)
             call put_header_cprop(ierr, head, xname, (/ibgn+1, iend/), jx + 1)
          enddo
       endif
       krect = REC_BIG
       if (ierr.eq.0) call nio_write_header(ierr, head, krect, uout)
       if (ierr.eq.0) call nio_write_data(ierr, vdest, ldest, head, krect, uout)
    endif
  end subroutine convoy_write

!!!_  & is_ordered_cset()
  integer function is_ordered_cset(tr, ktb, nx) result(k)
    implicit none
    type(transf_t),intent(in) :: tr
    integer,       intent(in) :: ktb(0:*)
    integer,       intent(in) :: nx
    integer j, cp
    cp = 0
    do j = 0, tr%nxs - 1
       if (ALL(ktb(0:nx-1).ne.j)) then
          k = cset_err
          return
       endif
    enddo
    k = cset_ordered
    do j = 0, nx - 1
       if (ktb(j).lt.0) cycle
       if (ktb(j).eq.cp) then
          cp = cp + 1
       else
          k = cset_shuffled
          exit
       endif
    enddo
  end function is_ordered_cset
!!!_ + common utilities
!!!_  & set_domain
  subroutine set_domain &
       & (ierr, n, mh, span, jsymm, low, high, d, org, tag)
    integer,parameter :: KTGT=KDBL
    integer,        intent(out) :: ierr
    integer,        intent(out) :: n, mh
    integer,        intent(out) :: span
    real(kind=KTGT),intent(out) :: jsymm
    real(kind=KTGT),intent(in)  :: low, high, d, org
    character(len=*),intent(in) :: tag

    real(kind=KTGT),parameter :: ZERO=0.0_KTGT
    real(kind=KTGT),parameter :: HALF=0.5_KTGT
    real(kind=KTGT) :: dh, wo

    ierr = 0

    n = -1
    mh = -1
    span = span_both
    jsymm = -999.0_KTGT       ! no symmetry (must be less than -0.5)

    if (ierr.eq.0) then
       ! must be monotonically increasing
       if (low.ge.high) ierr = ERR_INVALID_PARAMETER
    endif

    if (ierr.eq.0) then
       n = int((high - low) / d)
       if (real(n, kind=KTGT) * d .ne. (high - low)) then
          ierr = ERR_INVALID_PARAMETER
       endif
    endif

    ! write(*,*) tag, (low + org), org, (high + org)

    if (ierr.eq.0) then
       mh = n * 2
       if ((low + org).ge.ZERO) then
          span = span_positive
       else if ((high + org).le.ZERO) then
          span = span_negative
       else
          span = span_both
          dh = d / 2.0_KTGT
          ! dh = d  ! test
          wo = - (low + org) / dh
          if (wo.eq.aint(wo)) then
             jsymm = wo
          else if (wo.eq.aint(wo)+HALF) then
             jsymm = wo
          endif
       endif
    endif

101 format('cartesian:', A, ': ', I0, 1x, I0, 1x, I0, 1x, F7.2)
    write(*, 101) trim(tag), n, mh, span, jsymm
    ! write(*, *) trim(tag), n, mh, span, jsymm
    return
  end subroutine set_domain
!!!_  & symmetry_check
  subroutine symmetry_check &
       & (ierr, &
       &  v,    lx,  ly,   xsymm, ysymm,  fk, &
       &  tag)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,         intent(out) :: ierr
    real(kind=KTGT), intent(in)  :: v(0:*)
    integer,         intent(in)  :: lx, ly
    real(kind=KTGT), intent(in)  :: xsymm, ysymm
    integer,         intent(in)  :: fk
    character(len=*),intent(in)  :: tag

    real(kind=KTGT),parameter :: ZERO=0.0_KTGT

    integer jx,   jy
    integer jxt,  jyt
    integer jxcl, jxch
    integer jycl, jych
    integer jvs,  jvt
    integer nasym

    ierr = fk * 0
    ! x symmetry
101 format('asymm/x:', A, ': ', I0, '/', I0, 1x,  I0, 1x, 2ES25.16, 1x, ES10.3)
102 format('symm/x:', A,  ': ', I0, '/', I0, 1x,  I0)
111 format('asymm/y:', A, ': ', I0, 1x,  I0, '/', I0, 1x, 2ES25.16, 1x, ES10.3)
112 format('symm/y:', A,  ': ', I0, '/', I0, 1x,  I0)
    if (xsymm.gt.ZERO) then
       jxcl = CEILING(xsymm) - 1
       jxch = FLOOR(xsymm)   + 1
       nasym = 0
       do jy = 0, ly - 1
          do jx = 0, jxcl
             jxt = jxch + (jxcl - jx)
             if (jxt.lt.lx) then
                jvs = jy * lx + jx
                jvt = jy * lx + jxt
                if (v(jvs).ne.v(jvt)) then
                   write(*, 101) trim(tag), jx, jxt, jy, v(jvs), v(jvt), v(jvs)-v(jvt)
                   nasym = nasym + 1
                endif
             endif
          enddo
       enddo
       if (nasym.eq.0) then
          write(*, 102) trim(tag), jxcl, jxch, lx
       endif
    endif
    if (ysymm.gt.ZERO) then
       jycl = CEILING(ysymm) - 1
       jych = FLOOR(ysymm)   + 1
       nasym = 0
       do jy = 0, jycl
          jyt = jych + (jycl - jy)
          if (jyt.lt.ly) then
             do jx = 0, lx - 1
                jvs = jy  * lx + jx
                jvt = jyt * lx + jx
                if (v(jvs).ne.v(jvt)) then
                   write(*, 111) trim(tag), jx, jy, jyt, v(jvs), v(jvt), v(jvs)-v(jvt)
                   nasym = nasym + 1
                endif
             enddo
          endif
       enddo
       if (nasym.eq.0) then
          write(*, 112) trim(tag), jycl, jych, ly
       endif
    endif
    return
  end subroutine symmetry_check
!!!_  & configuration
! !!!_   . parse_sphere_geogr
!   subroutine parse_sphere_geogr &
!        & (ierr, &
!        &  mlat, latdiv, latpad, latofs, latname, lattype, &
!        &  mlon, londiv, lonpad, lonofs, lonname, lontype, &
!        &  atag, latag,  lotag)
!     implicit none
!     integer,         intent(out)         :: ierr
!     integer,         intent(out)         :: mlat,      mlon
!     integer,         intent(out)         :: latdiv,    londiv
!     integer,         intent(out)         :: latpad(*), lonpad(*)
!     real(kind=KTGT), intent(out)         :: latofs,    lonofs
!     character(len=*),intent(out)         :: latname,   lonname
!     character(len=*),intent(out)         :: lattype,   lontype
!     character(len=*),intent(in),optional :: atag,      latag,   lotag

!     integer,parameter   :: larg = 256
!     character(len=larg) :: gtag, glatag, glotag
!     character(len=larg) :: aval

!     character(len=4)    :: tdefla, tdeflo
!     integer             :: ndefla, ndeflo
!     real(kind=KTGT)     :: odefla, odeflo
!     integer             :: pdefla(2), pdeflo(2)

!     ierr = 0

!     call choice_a(gtag, ' ', atag)
!     if (gtag.eq.' ') gtag = 'G'

!     call choice_a(glatag, ' ', latag)
!     if (glatag.eq.' ') glatag = 'LA'

!     call choice_a(glotag, ' ', lotag)
!     if (glotag.eq.' ') glotag = 'LO'

!     tdefla = geo_gauss // geo_divll
!     tdeflo = geo_eqdis // geo_divll
!     odefla = 90.0_KTGT
!     odeflo = 0.0_KTGT
!     ndefla = -1
!     ndeflo = -1

!     pdefla(:) = -1
!     pdeflo(:) = -1

!     ! G=SPECIAL    base domain
!     if (ierr.eq.0) call get_option(ierr, aval, gtag)
!     if (ierr.eq.0) then
!        call upcase(aval)
!        if (aval(1:1).eq.'T') then
!           select case (aval)
!           case('T42')
!              ndefla = 64
!              ndeflo = 128
!           case('T85')
!              ndefla = 128
!              ndeflo = 256
!           case('T106')
!              ndefla = 180
!              ndeflo = 360
!           case('T213')
!              ndefla = 320
!              ndeflo = 640
!           case default
!              ierr = ERR_INVALID_PARAMETER
!           end select
!        else if (aval.ne.' ') then
!           ierr = ERR_INVALID_PARAMETER
!        endif
!        if (ierr.ne.0) then
!           write(*, *) 'cannot parse geo-coordinate argument ', &
!                & trim(gtag), '=', trim(aval)
!        endif
!     endif
!     if (ierr.eq.0) then
!        call parse_geo_coordinate &
!             & (ierr, &
!             &  mlat,   latofs, lattype, latname, latdiv, latpad, &
!             &  ndefla, odefla, tdefla,  'latg',  0,      pdefla, &
!             &  glatag)
!     endif
!     if (ierr.eq.0) then
!        call parse_geo_coordinate &
!             & (ierr, &
!             &  mlon,   lonofs, lontype, lonname, londiv, lonpad, &
!             &  ndeflo, odeflo, tdeflo,  'long',  0,      pdeflo, &
!             &  glotag)
!     endif
!   end subroutine parse_sphere_geogr

! !!!_   . parse_geo_coordinate
!   subroutine parse_geo_coordinate &
!        & (ierr, &
!        &  mem,  ofs,  tp,   name, div,  padding, &
!        &  mdef, odef, tdef, ndef, ddef, pdef,    &
!        &  atag)
!     implicit none
!     integer,         intent(out) :: ierr
!     integer,         intent(out) :: mem
!     real(kind=KTGT), intent(out) :: ofs
!     character(len=*),intent(out) :: tp
!     character(len=*),intent(out) :: name
!     integer,         intent(out) :: div
!     integer,         intent(out) :: padding(*)
!     integer,         intent(in)  :: mdef
!     real(kind=KTGT), intent(in)  :: odef
!     character(len=*),intent(in)  :: tdef
!     character(len=*),intent(in)  :: ndef
!     integer,         intent(in)  :: ddef
!     integer,         intent(in)  :: pdef(*)
!     character(len=*),intent(in)  :: atag

!     integer,parameter   :: larg = 256
!     character(len=larg) :: aval
!     integer,parameter :: lgrp = 2
!     character(len=larg) :: agrp(lgrp)
!     integer,parameter :: li = 4
!     character(len=larg) :: aitems(li)
!     integer ni, ngrp
!     real(kind=KTGT) :: vbuf
!     integer jerr
!     character(len=lmsg) :: txt

!     ! LA=[NAME/]NUM[:DIV[:OFS]][:TYPE[+PADDING[-PADDING]]]     if no default num
!     ! LA=[NAME/][NUM:][DIV:[OFS]][:TYPE[+PADDING[-PADDING]]]   if with default num

!     ierr = 0
!     if (ierr.eq.0) call get_last_option(ierr, aval, atag, def=' ')
!     if (ierr.eq.0) then
!        agrp(:) = ' '
!        call split_list(ngrp, agrp, aval, sep_rename, lgrp)
!        ierr = min(0, ngrp)
!     endif
!     if (ierr.eq.0) then
!        aitems(:) = ' '
!        if (ngrp.eq.2) then
!           call split_list(ni, aitems, agrp(2), sep_item, li)
!        else if (ngrp.eq.1) then
!           call split_list(ni, aitems, agrp(1), sep_item, li)
!           if (ni.gt.0) then
!              call parse_number(jerr, vbuf, aitems(1))
!              if (jerr.eq.0) then
!                 agrp(1) = ' '
!              else
!                 aitems(:) = ' '
!              endif
!           endif
!        else if (ngrp.eq.0) then
!           ni = 0
!        else
!           ni = ERR_INVALID_PARAMETER
!        endif
!     else
!        ni = ERR_INVALID_PARAMETER
!     endif
!     if (ierr.eq.0) ierr = min(0, ni)
!     if (ierr.eq.0) then
!        name = agrp(1)
!        if (name.eq.' ') name = ndef
!     endif
!     if (ierr.eq.0) then
!        ! shift TYPE option to last item
!        if (ni.ge.1.and.ni.lt.li) then
!           call parse_number(jerr, vbuf, aitems(ni))
!           if (jerr.ne.0) then
!              aitems(li) = aitems(ni)
!              aitems(ni) = ' '
!           endif
!        endif
!        if (mdef.ge.0.and.ni.lt.li) then
!           aitems(2:li-1) = aitems(1:li-2)
!           aitems(1) = ' '
!        endif
!     endif
!     if (ierr.eq.0) call parse_number(ierr, mem, aitems(1), mdef)
!     if (ierr.eq.0) call parse_number(ierr, div, aitems(2), 0)
!     if (ierr.eq.0) div = max(1, div)
!     if (ierr.eq.0) call parse_number(ierr, ofs, aitems(3), odef)
!     if (ierr.eq.0) then
!        tp = aitems(4)
!        if (tp.eq.' ') tp = tdef
!        call upcase(tp)
!        if (verify(trim(tp), geo_all).ne.0) ierr = ERR_INVALID_PARAMETER
!     endif

!     if (ierr.ne.0) then
!        write(*, *) 'cannot parse geo-coordinate argument ', &
!             & trim(atag), '=', trim(aval)
!     endif
! 107 format('sphere coordinate ', A, ' = ', '[', A, '] ', I0, '*', I0, 1x, A, 1x, F12.3)
!     if (ierr.eq.0) then
!        if (is_verbose(msglev_NORMAL)) then
!           write(txt, 107) trim(atag), trim(name), mem, div, trim(tp), ofs
!           call message(ierr, txt)
!        endif
!     endif
!   end subroutine parse_geo_coordinate

!!!_   . str_source_var
  subroutine str_source_var &
       & (ierr, str, name, nx, xin, ktb)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    character(len=*),intent(in)  :: name
    integer,         intent(in)  :: nx
    character(len=*),intent(in)  :: xin(0:*)
    integer,         intent(in)  :: ktb(0:*)
    character(len=256) :: bufx(0:nx-1)
    integer jx
    ierr = 0
101 format('<', A, '>')
102 format(A)
    do jx = 0, nx - 1
       if (ktb(jx).lt.0) then
          write(bufx(jx), 102) trim(xin(jx))
       else
          write(bufx(jx), 101) trim(xin(jx))
       endif
    enddo
    call join_list(ierr, str, bufx(0:nx-1), sep=',', ldelim='{', rdelim='}')
    if (ierr.eq.0) str = trim(name) // ' ' // trim(str)
  end subroutine str_source_var

!!!_   . str_dest_coset
  subroutine str_dest_coset &
       & (ierr, str, nx, xin, ktb, pr, hopr)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    integer,         intent(in)  :: nx
    character(len=*),intent(in)  :: xin(0:*)
    integer,         intent(in)  :: ktb(0:*)
    type(convoy_t),  intent(in)  :: pr
    integer,         intent(in)  :: hopr
    character(len=256) :: bufx(0:nx-1)
    character(len=256) :: bufc, bufy
    integer jopr
    integer jx, ny
    logical perm

    ierr = 0
    jopr = transf_h2index(hopr)
    ierr = min(0, jopr)
    if (ierr.eq.0) then
       perm = IAND(pr%tr(jopr)%k, xcmd_noperm).eq.0
       if (perm) then
          call gen_cset_str &
               & (bufc, pr%tr(jopr)%dstx, pr%tr(jopr)%mdst, pr%tr(jopr)%nxd, num=.FALSE.)
102       format(A)
103       format('<', A, '>')
          ny = 0
          do jx = 0, nx - 1
             if (ktb(jx).lt.0) then
                write(bufx(ny), 102) trim(xin(jx))
                ny = ny + 1
             endif
          enddo
111       format('{<', A, '>}')
112       format('{<', A, '>,', A, '}')
          if (ny.eq.0) then
             write(str, 111) trim(bufc)
          else
             call join_list(ierr, bufy, bufx(0:ny-1), sep=',')
             write(str, 112) trim(bufc), trim(bufy)
          endif
       else
          do jx = 0, nx - 1
             if (ktb(jx).lt.0) then
                write(bufx(jx), 102) trim(xin(jx))
             else
                write(bufx(jx), 103) trim(xin(jx))
             endif
          enddo
          call join_list(ierr, str, bufx(0:nx-1), sep=',', ldelim='{', rdelim='}')
       endif
    endif
  end subroutine str_dest_coset

!!!_   & transf_h2index()
  integer function transf_h2index (handle) result(idx)
    implicit none
    integer,intent(in) :: handle
    if (handle.lt.0) then
       idx = ERR_INVALID_PARAMETER
    else
       idx = IAND(handle, NOT(lim_transf))
       if (idx.ge.lim_transf) idx = ERR_INVALID_PARAMETER
    endif
  end function transf_h2index
!!!_   & transf_h2dir()
  integer function transf_h2dir (handle) result(dir)
    implicit none
    integer,intent(in) :: handle
    if (IAND(handle, lim_transf).eq.0) then
       dir = +1
    else
       dir = -1
    endif
  end function transf_h2dir
!!!_   & transf_j2handle()
  integer function transf_j2handle (idx, dir) result(h)
    implicit none
    integer,intent(in) :: idx, dir
    if (idx.lt.0) then
       h = ERR_INVALID_PARAMETER
    else if (idx.ge.lim_transf) then
       h = ERR_INSUFFICIENT_BUFFER
    else if (dir.gt.0) then
       h = idx
    else if (dir.lt.0) then
       h = IOR(idx, lim_transf)
    else
       h = ERR_INVALID_PARAMETER
    endif
  end function transf_j2handle

!!!_ + end program
end program convoy
!!!_* obsolete
#if 0 /* obsolete */
#endif /* obsolete */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

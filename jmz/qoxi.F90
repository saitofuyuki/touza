!!!_! qoxi.F90 - TOUZA/Jmz/polar sterographic projection helper
! Maintainer: SAITO Fuyuki
! Created: Dec 7 2023
#define TIME_STAMP 'Time-stamp: <2024/04/17 07:33:53 fuyuki qoxi.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2023,2024
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
#define _PFX 'qoxi: '
!!!_@ TOUZA/Jmz/qoxi - polar sterographic projection helper
program qoxi
!!!_ + Declaration
!!!_  - modules
  use Jmz_param
  use Jmz_base, base_init=>init, base_finalize=>finalize
  use TOUZA_Nio,only: put_header_cprop, put_item, get_default_header
  use TOUZA_Nio,only: litem, nitem
  use TOUZA_Nio,only: hi_DFMT, hi_ITEM, hi_DSET, hi_UNIT
  use TOUZA_Nio,only: nio_record_std
  use TOUZA_Nio,only: nio_write_header, nio_write_data, nio_store_csr
  use TOUZA_Nio,only: lopts_sparse
  use convoy_util
  use convoy_psprop
  use convoy_ps2g
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
!!!_  - type
  ! transformation
  integer,parameter :: lcvax = 3
  integer,parameter :: lweights = 3
  integer,parameter :: lcaux = 3
  type transf_t
     integer k
     integer nxs, nxd     ! number of coordinates
     character(len=litem) :: srcx(0:lcvax-1)      ! source,destination coordinates
     character(len=litem) :: dstx(0:lcvax-1)
     integer msrc(0:lcvax-1)
     integer mdst(0:lcvax-1)
     integer auxsrc(0:lcaux-1, 0:lcvax-1)
     integer auxdst(0:lcaux-1, 0:lcvax-1)
     integer namin, namax
     integer nw
     integer h             ! cache handle (suite or group)
     integer idjv,  idwv(0:lweights-1)   ! index, weight variable id
     integer recj,  recw(0:lweights-1)   ! record index
     integer nbase, mcols, npack
     integer colc
     integer popts(lopts_sparse)
     real(kind=KTGT),allocatable :: wcsr(:, :)
     integer,        allocatable :: icsr(:)
     integer,        allocatable :: hcsr(:)
     character(len=litem) :: opr      ! operator name
  end type transf_t
  ! auxiliary data
  type auxil_t
     integer k
     integer nx
     character(len=litem) :: xname(0:lcvax-1)
     integer mem(0:lcvax-1)
     integer h   ! cache handle (suite or group)
     integer idv
     integer rec
     integer reftr  ! reference transform
     real(kind=KTGT),allocatable :: vd(:)
     integer,        allocatable :: vi(:)
  end type auxil_t
  ! convoy
  type convoy_t
     integer :: nt = -1
     type(transf_t),pointer :: tr(:) => NULL()
     type(auxil_t), pointer :: aux(:) => NULL()
  end type convoy_t
!!!_ + Body
!!!_  - driver
  ierr = 0

  if (ierr.eq.0) call base_init(ierr, basename='qoxi')
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
              call qoxi_ps2g(ierr, jpos, npos)
           case('R2G')
              call qoxi_r2g(ierr, jpos, npos)
           case('PS')
              call qoxi_psprop(ierr, jpos, npos)
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
!!!_  & parse_convoy_ps2g
  subroutine parse_convoy_ps2g &
       & (ierr, prime, jpos, npos, handle)
    implicit none
    integer,         intent(out)   :: ierr
    type(convoy_t),  intent(inout) :: prime
    integer,         intent(inout) :: jpos
    integer,         intent(in)    :: npos
    integer,         intent(in)    :: handle
    character(len=*),parameter :: proc = 'parse_convoy_ps2g'

    ierr = 0
    if (ierr.eq.0) then
       call parse_oprfile &
            & (ierr, prime, jpos, npos, handle, xcmd_csr, 'jfwd', 'wfwd', 'w2fwd', 'w3fwd')
    endif
    if (ierr.eq.0) then
       call parse_oprfile &
            & (ierr, prime, jpos, npos, handle, xcmd_csr, 'jbwd', 'wbwd', 'w2bwd', 'w3bwd')
    endif
    call trace_err(ierr, fun=proc)
  end subroutine parse_convoy_ps2g

!!!_  & parse_convoy_r2g
  subroutine parse_convoy_r2g &
       & (ierr, prime, jpos, npos, handle)
    implicit none
    integer,         intent(out)   :: ierr
    type(convoy_t),  intent(inout) :: prime
    integer,         intent(inout) :: jpos
    integer,         intent(in)    :: npos
    integer,         intent(in)    :: handle
    character(len=*),parameter :: proc = 'parse_convoy_r2g'

    ierr = 0
    if (ierr.eq.0) call parse_oprfile(ierr, prime, jpos, npos, handle, xcmd_qjds, 'IJC2O', 'SATM')
    if (ierr.eq.0) call parse_oprfile(ierr, prime, jpos, npos, handle, xcmd_qjds, 'IJO2C', 'SOCN')
    call trace_err(ierr, fun=proc)
  end subroutine parse_convoy_r2g

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
!!!_  & add_transf
  subroutine add_transf(ierr, jt, pr)
    implicit none
    integer,       intent(out)   :: ierr
    integer,       intent(out)   :: jt
    type(convoy_t),intent(inout) :: pr

    type(transf_t),pointer :: tt(:)
    integer,parameter :: mt = 2

    ierr = 0
    jt = pr%nt
    if (jt.ge.size(pr%tr)) then
       allocate(tt(0:pr%nt+mt-1), STAT=ierr)
       if (ierr.eq.0) then
          tt(0:pr%nt-1) = pr%tr(0:pr%nt-1)
          deallocate(pr%tr, STAT=ierr)
       endif
       if (ierr.eq.0) pr%tr => tt
    endif
    if (ierr.eq.0) pr%nt = pr%nt + 1
    if (ierr.ne.0) ierr = ERR_ALLOCATION
  end subroutine add_transf
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
!!!_  & parse_oprfile
  subroutine parse_oprfile &
       & (ierr, pr, jpos, npos, handle, xsw, vidx, vwgt, vw2, vw3)
    use TOUZA_Nio,only: grp_suite, cache_var_id, cache_read_header
    use TOUZA_Nio,only: cache_unit, cache_co_name, cache_co_len
    use TOUZA_Nio,only: cache_group_recs, cache_sparse_review
    use TOUZA_Nio,only: nio_column_coor, nio_review_sparse
    use TOUZA_Nio,only: lopts_sparse, PROP_PTX_COLC
    use TOUZA_Nio,only: hi_MEMO1, get_item, restore_item
    implicit none
    integer,         intent(out)   :: ierr
    type(convoy_t),  intent(inout) :: pr
    integer,         intent(inout) :: jpos
    integer,         intent(in)    :: npos
    integer,         intent(in)    :: handle
    integer,         intent(in)    :: xsw
    character(len=*),intent(in)    :: vidx, vwgt
    character(len=*),intent(in),optional :: vw2, vw3

    character(len=*),parameter :: proc = 'parse_oprfile'

    integer id_jini, id_wini
    integer id_j
    integer nrec_j
    integer jt,      jrec
    integer utab
    real(kind=KTGT) :: mold = 0.0_KTGT
    character(len=litem) :: head(nitem)
    integer popts(lopts_sparse)
    integer nbase, mcols, npack, colc
    integer jx, ji
    integer jw, nw, jtbgn

    ierr = 0
    id_wini = 0

    utab = cache_unit(handle)
    colc = -1
    nw = 1
    if (present(vw2)) nw = nw + 1
    if (present(vw3)) nw = nw + 1

    id_jini = 0
    jtbgn = pr%nt
    do
       id_j = cache_var_id(vidx, handle, id_jini)
       if (id_j.lt.0) exit
       nrec_j = cache_group_recs(handle, id_j)
       if (nrec_j.lt.0) then
          ierr = ERR_PANIC
          exit
       endif
       do jrec = 0, nrec_j - 1
          call add_transf(ierr, jt, pr)
          if (ierr.eq.0) then
             pr%tr(jt)%idwv(:) = -1
             pr%tr(jt)%recw(:) = -1
             pr%tr(jt)%k        = xsw
             pr%tr(jt)%h        = handle
             pr%tr(jt)%idjv     = id_j
             pr%tr(jt)%recj     = jrec
             pr%tr(jt)%namin    = 1
             pr%tr(jt)%namax    = nw
             pr%tr(jt)%nw       = nw
          endif
          if (ierr.eq.0) then
             call cache_sparse_review &
                  & (ierr,   &
                  &  nbase,  mcols, npack, &
                  &  handle, id_j,  jrec,  mold, &
                  &  head,   popts, colc)
          endif
          if (ierr.eq.0) then
             pr%tr(jt)%nbase = nbase
             pr%tr(jt)%mcols = mcols
             pr%tr(jt)%npack = npack
             pr%tr(jt)%colc = colc
             if (colc.le.0) ierr = ERR_PANIC
          endif
          if (ierr.eq.0) then
             pr%tr(jt)%auxdst(:,:) = -1
             pr%tr(jt)%auxsrc(:,:) = -1
             pr%tr(jt)%nxd = colc - 1
             do jx = 0, colc - 2
                if (ierr.eq.0) then
                   call cache_co_name(ierr, pr%tr(jt)%dstx(jx), handle, id_j, jx)
                endif
                if (ierr.eq.0) then
                   pr%tr(jt)%mdst(jx) = cache_co_len(handle, id_j, jx)
                endif
             enddo
             pr%tr(jt)%nxs = 0
             do jx = 0, lcvax - 1
                ji = hi_MEMO1 + jx * 2
                if (ierr.eq.0) call get_item(ierr, head, pr%tr(jt)%srcx(jx),  ji)
                if (ierr.eq.0) then
                   if (pr%tr(jt)%srcx(jx).eq.' ') exit
                   pr%tr(jt)%nxs = pr%tr(jt)%nxs + 1
                   call restore_item(ierr, head, pr%tr(jt)%msrc(jx),  ji+1, def=-1)
                endif
             enddo
          endif
       enddo
       if (ierr.ne.0) exit
       id_jini = id_j + 1
    enddo
    jw = 0
    if (ierr.eq.0) then
       call add_opr_weight(ierr, pr, jtbgn, handle, jw, vwgt, vidx)
    endif
    jw = jw + 1
    if (present(vw2)) then
       if (ierr.eq.0) then
          call add_opr_weight(ierr, pr, jtbgn, handle, jw, vw2, vidx)
       endif
    endif
    jw = jw + 1
    if (present(vw3)) then
       if (ierr.eq.0) then
          call add_opr_weight(ierr, pr, jtbgn, handle, jw, vw3, vidx)
       endif
    endif

    call trace_err(ierr, fun=proc)
    return
  end subroutine parse_oprfile

!!!_  - add_opr_weight
  subroutine add_opr_weight &
       & (ierr, pr, jtbgn, handle, jw, vwgt, vidx)
    use TOUZA_Nio,only: cache_var_id, cache_group_recs
    implicit none
    integer,         intent(out)   :: ierr
    type(convoy_t),  intent(inout) :: pr
    integer,         intent(in)    :: jtbgn
    integer,         intent(in)    :: handle
    integer,         intent(in)    :: jw
    character(len=*),intent(in)    :: vwgt
    character(len=*),intent(in)    :: vidx
    integer jt
    integer id_j, id_jprv, jreci, nreci
    integer id_w, id_wini, nrecw
    character(len=lmsg) :: txt

    ierr = 0
    id_wini = 0
    id_jprv = -1
    nrecw = -1

119 format('No pair record of index and weight: ', A, 1x, A)
    do jt = jtbgn, pr%nt - 1
       id_j  = pr%tr(jt)%idjv
       if (id_jprv.ne.id_j) then
          id_w = cache_var_id(vwgt, handle, id_wini)
          if (id_w.lt.0) exit
          nrecw = cache_group_recs(handle, id_w)
          nreci = cache_group_recs(handle, id_j)
          if (nrecw.ne.nreci) then
             ierr = ERR_PANIC
             write(txt, 119) trim(vidx), trim(vwgt)
             call message(ierr, txt, levm=msglev_CRITICAL)
             exit
          endif
          id_wini = id_w + 1
          id_jprv = id_j
       endif
       jreci = pr%tr(jt)%recj
       if (jreci.lt.nrecw) then
          pr%tr(jt)%idwv(jw) = id_w
          pr%tr(jt)%recw(jw) = jreci
       else
          ierr = ERR_PANIC
          write(txt, 119) trim(vidx), trim(vwgt)
          call message(ierr, txt, levm=msglev_CRITICAL)
          exit
       endif
    enddo
  end subroutine add_opr_weight
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
!!!_ + r2g - R[AO]FILE to TOUZA/Ami format
!!!_  & qoxi_r2g
  subroutine qoxi_r2g &
       & (ierr, jpos, npos)
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jpos
    integer,intent(in)    :: npos

    integer :: ufile
    character(len=lpath) :: ofile
    character(len=litem) :: head(nitem)
    character(len=lpath) :: rafile, rofile

    character(len=*),parameter :: ratag = 'RAFILE'
    character(len=*),parameter :: rotag = 'ROFILE'

    integer :: krect

    ierr = 0
    if (ierr.eq.0) call push_basename(ierr, 'r2g')

    if (ierr.eq.0) then
       call get_param(ierr, ofile, jpos, ' ')
       if (ierr.eq.0) then
          jpos = jpos + 1
       else
          ofile = ' '
       endif
    endif
    if (ierr.eq.0) then
       if (ofile.eq.' ') then
          ufile = -1
          head(:) = ' '
          krect = 0
       else
          krect = nio_record_std()
          call open_write(ierr, ufile, ofile, cfmt_gtool_seq)
          if (ierr.eq.0) call get_default_header(head)
       endif
    endif
    if (ierr.eq.0) call get_last_option(ierr, rafile, ratag, ratag(1:2), def=' ')
    if (ierr.eq.0) call get_last_option(ierr, rofile, rotag, rotag(1:2), def=' ')
    if (ierr.eq.0) then
       if (rafile.eq.' '.and.rofile.eq.' ') then
          ierr = ERR_FEW_ARGUMENTS
          call message(ierr, 'neither RAfile nor ROfile specified.')
       endif
    endif
    if (ierr.eq.0) then
       call batch_r2g &
            & (ierr,   jpos,   npos, &
            &  rafile, rofile, &
            &  ufile,  head,   krect)
    endif
    if (ierr.eq.0) call pop_basename(ierr, 'r2g')
  end subroutine qoxi_r2g
!!!_  & batch_r2g
  subroutine batch_r2g &
       & (ierr,   jpos,   npos,    &
       &  rafile, rofile, &
       &  ufile,  head,   krect)
    use TOUZA_Ami,only: open_rafile_legacy, read_rafile_legacy
    use TOUZA_Ami,only: open_rofile_legacy, read_rofile_legacy1, read_rofile_legacy2
    use TOUZA_Ami,only: normalize_radata,   normalize_rodata
    use TOUZA_Ami,only: set_raheader,       set_roheader
    use TOUZA_Ami,only: write_radata_nio,   write_rodata_nio
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jpos
    integer,intent(in)    :: npos
    character(len=*),intent(in) :: rafile, rofile
    integer,         intent(in) :: ufile
    character(len=*),intent(inout) :: head(*)
    integer,         intent(inout) :: krect

    character(len=lmsg) :: txt
    integer ua, uo
    logical swap

    integer kra, ij_amax, len_a2m, nxygdm
    integer kro, ij_omax, len_o2a, ijdim

    integer,parameter :: KMD = KDBL
    integer,       allocatable :: ij_ahead(:), ijrecov_a2m(:), ijc2o(:)
    real(kind=KMD),allocatable :: satm(:), ru(:), rv(:), rocn(:)

    real(kind=KMD),allocatable :: ruo(:), rvo(:), flandg(:)
    integer,       allocatable :: ij_ohead(:), ijrecov_o2c(:), ijo2c(:)
    real(kind=KMD),allocatable :: socn(:)

    integer :: amlat,      amlon
    integer :: alatdiv,    alondiv
    integer :: alatpad(2), alonpad(2)
    real(kind=KTGT)  :: alatofs,  alonofs
    character(len=lname) :: alatname, alonname
    character(len=4) :: alattype, alontype

    integer :: owlat,      owlon
    integer :: omlat,      omlon
    integer :: olatdiv,    olondiv
    integer :: olatpad(2), olonpad(2)
    real(kind=KTGT)  :: olatofs,  olonofs
    character(len=lname) :: olatname, olonname
    character(len=4) :: olattype, olontype

    integer :: cplon
    integer :: cmlat, cmlon

    integer,parameter :: offset_legacy = 1
    character(len=16) :: rfmt

    ierr = 0
    rfmt = 'PS'

    if (ierr.eq.0) call put_item(ierr, head, DSET_R2G, hi_DSET)
    ! RAfile
    if (ierr.eq.0) then
       if (rafile.eq.' ') then
          nxygdm = -1
          ua = -1
       else
          ua = new_unit()
          ierr = min(0, ua)
       endif
       if (ua.ge.0) then
          if (ierr.eq.0) then
             call open_rafile_legacy &
                  & (ierr, kra, swap, ij_amax, len_a2m, nxygdm, rafile, ua)
101          format('rafile: ', A, 1x, L1, 2(1x, I0), 1x, I0)
             write(txt, 101) trim(rafile), swap, ij_amax, nxygdm, len_a2m
             call message(ierr, txt)
          endif
          if (ierr.eq.0) then
             allocate(ij_ahead(0:ij_amax), &
                  &   ijrecov_a2m(len_a2m), ijc2o(len_a2m), satm(len_a2m), &
                  &   ru(nxygdm),           rv(nxygdm),     rocn(nxygdm),  &
                  &   STAT=ierr)
          endif
          if (ierr.eq.0) then
             call read_rafile_legacy &
                  & (ierr,     &
                  &  ij_ahead, ijrecov_a2m, ijc2o,  satm, ru, rv, rocn, &
                  &  ij_amax,  len_a2m,     nxygdm, ua,   swap,   kra)
          endif
          if (ierr.eq.0) call sus_close(ierr, ua, rafile)
       endif
    endif

    ! ROfile
    if (ierr.eq.0) then
       if (rofile.eq.' ') then
          ijdim = -1
          uo = -1
       else
          uo = new_unit()
          ierr = min(0, uo)
       endif
       if (uo.ge.0) then
          if (ierr.eq.0) then
             call open_rofile_legacy &
                  & (ierr, kro, swap, ijdim, rofile, uo)
111          format('rofile:i: ', A, 1x, L1, 1x, I0)
             write(txt, 111) trim(rofile), swap, ijdim
             call message(ierr, txt)
          endif
          if (ierr.eq.0) then
             call read_rofile_legacy1 &
                  & (ierr,     &
                  &  ij_omax,  len_o2a, &
                  &  ijdim,    uo,      swap)
          endif
112       format('rofile:ii: ', A, 1x, L1, 2(1x, I0), 1x, I0)
          write(*, 112) trim(rofile), swap, ij_omax, ijdim, len_o2a
          if (ierr.eq.0) then
             allocate(ij_ohead(0:ij_omax),  &
                  &   ijrecov_o2c(len_o2a), ijo2c(len_o2a), socn(len_o2a), &
                  &   ruo(ijdim),           rvo(ijdim),     flandg(ijdim), &
                  &   STAT=ierr)
          endif
          if (ierr.eq.0) then
             ijo2c(:) = 0
             socn(:)  = 0.0_KDBL
             call read_rofile_legacy2 &
                  & (ierr,     &
                  &  ij_ohead, ijrecov_o2c, ijo2c, socn, flandg, ruo, rvo, &
                  &  ijdim,    ij_omax,     uo,    swap, kro)
          endif
          if (ierr.eq.0) call sus_close(ierr, uo, rofile)
       endif
    endif

    ! domains
    if (ierr.eq.0) then
       call parse_atmos_geogr_base &
            & (ierr, &
            &  amlat, alatdiv, alatpad, alatofs, alatname, alattype, &
            &  amlon, alondiv, alonpad, alonofs, alonname, alontype, &
            &  'ATM')
    endif
    if (ierr.eq.0) then
       call set_check_atmos &
            & (ierr, &
            &  amlat, alatdiv, alatpad, alatofs, alatname, alattype, &
            &  amlon, alondiv, alonpad, alonofs, alonname, alontype, &
            &  ijdim)
    endif
    if (ierr.eq.0) then
       call parse_ocean_geogr_base &
            & (ierr, &
            &  omlat, olatdiv, olatpad, olatofs, olatname, olattype, &
            &  omlon, olondiv, olonpad, olonofs, olonname, olontype)
    endif
    if (ierr.eq.0) then
       call set_check_ocean &
            & (ierr, &
            &  omlat, olatdiv, olatpad, olatofs, olatname, olattype, &
            &  omlon, olondiv, olonpad, olonofs, olonname, olontype, &
            &  nxygdm)
    endif
    if (ierr.eq.0) then
       owlon = olonpad(lpad)
       owlat = olatpad(lpad)
       cmlon = amlon * alondiv
       cplon = alonpad(rpad) * alondiv
       cmlat = amlat * alatdiv
    endif
    if (ua.ge.0) then
       if (ierr.eq.0) then
          call normalize_radata &
               & (ierr,        &
               &  ijrecov_a2m, ijc2o,   satm,  ru,    rv,    rocn, &
               &  ij_ahead,    ij_amax, &
               &  nxygdm,      omlon,   owlon, omlat, owlat, &
               &  cmlon,       cplon,   cmlat, offset_legacy)
       endif
       if (ierr.eq.0) then
          call set_raheader(ierr, head, olonname, olatname, nxygdm, omlon, omlat)
       endif
       if (ierr.eq.0) then
          call write_radata_nio &
               & (ierr, krect, &
               &  head,  ufile, rfmt,  &
               &  ijrecov_a2m, ijc2o, satm, ru, rv, rocn, ij_ahead, ij_amax)
       endif
       if (ierr.eq.0) then
          deallocate(ij_ahead, ijrecov_a2m, ijc2o, satm, ru, rv, rocn, STAT=ierr)
       endif
    endif
    if (uo.ge.0) then
       if (ierr.eq.0) then
          call normalize_rodata &
               & (ierr,        &
               &  ijrecov_o2c, ijo2c,   socn,  ruo,   rvo, flandg, &
               &  ij_ohead,    ij_omax, &
               &  omlon,       owlon,   omlat, owlat, &
               &  ijdim,       cmlon,   cplon, cmlat, offset_legacy)
       endif
       if (ierr.eq.0) call set_roheader(ierr, head, alonname, alatname, ijdim, cmlon, cmlat)
       if (ierr.eq.0) then
          call write_rodata_nio &
               & (ierr,   krect, &
               &  head,   ufile, rfmt,   &
               &  ijrecov_o2c, ijo2c, socn, ruo, rvo, flandg, ij_ohead, ij_omax)
       endif
       if (ierr.eq.0) then
          deallocate(ij_ohead, ijrecov_o2c, ijo2c, socn, ruo, rvo, flandg, STAT=ierr)
       endif
    endif

  end subroutine batch_r2g
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
!!!_   . parse_sphere_geogr
  subroutine parse_sphere_geogr &
       & (ierr, &
       &  mlat, latdiv, latpad, latofs, latname, lattype, &
       &  mlon, londiv, lonpad, lonofs, lonname, lontype, &
       &  atag, latag,  lotag)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: mlat,      mlon
    integer,         intent(out)         :: latdiv,    londiv
    integer,         intent(out)         :: latpad(*), lonpad(*)
    real(kind=KTGT), intent(out)         :: latofs,    lonofs
    character(len=*),intent(out)         :: latname,   lonname
    character(len=*),intent(out)         :: lattype,   lontype
    character(len=*),intent(in),optional :: atag,      latag,   lotag

    integer,parameter   :: larg = 256
    character(len=larg) :: gtag, glatag, glotag
    character(len=larg) :: aval

    character(len=4)    :: tdefla, tdeflo
    integer             :: ndefla, ndeflo
    real(kind=KTGT)     :: odefla, odeflo
    integer             :: pdefla(2), pdeflo(2)

    ierr = 0

    call choice_a(gtag, ' ', atag)
    if (gtag.eq.' ') gtag = 'G'

    call choice_a(glatag, ' ', latag)
    if (glatag.eq.' ') glatag = 'LA'

    call choice_a(glotag, ' ', lotag)
    if (glotag.eq.' ') glotag = 'LO'

    tdefla = geo_gauss // geo_divll
    tdeflo = geo_eqdis // geo_divll
    odefla = 90.0_KTGT
    odeflo = 0.0_KTGT
    ndefla = -1
    ndeflo = -1

    pdefla(:) = -1
    pdeflo(:) = -1

    ! G=SPECIAL    base domain
    if (ierr.eq.0) call get_option(ierr, aval, gtag)
    if (ierr.eq.0) then
       call upcase(aval)
       if (aval(1:1).eq.'T') then
          select case (aval)
          case('T42')
             ndefla = 64
             ndeflo = 128
          case('T85')
             ndefla = 128
             ndeflo = 256
          case('T106')
             ndefla = 180
             ndeflo = 360
          case('T213')
             ndefla = 320
             ndeflo = 640
          case default
             ierr = ERR_INVALID_PARAMETER
          end select
       else if (aval.ne.' ') then
          ierr = ERR_INVALID_PARAMETER
       endif
       if (ierr.ne.0) then
          write(*, *) 'cannot parse geo-coordinate argument ', &
               & trim(gtag), '=', trim(aval)
       endif
    endif
    if (ierr.eq.0) then
       call parse_geo_coordinate &
            & (ierr, &
            &  mlat,   latofs, lattype, latname, latdiv, latpad, &
            &  ndefla, odefla, tdefla,  'latg',  0,      pdefla, &
            &  glatag)
    endif
    if (ierr.eq.0) then
       call parse_geo_coordinate &
            & (ierr, &
            &  mlon,   lonofs, lontype, lonname, londiv, lonpad, &
            &  ndeflo, odeflo, tdeflo,  'long',  0,      pdeflo, &
            &  glotag)
    endif
  end subroutine parse_sphere_geogr

!!!_   . parse_geo_coordinate
  subroutine parse_geo_coordinate &
       & (ierr, &
       &  mem,  ofs,  tp,   name, div,  padding, &
       &  mdef, odef, tdef, ndef, ddef, pdef,    &
       &  atag)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: mem
    real(kind=KTGT), intent(out) :: ofs
    character(len=*),intent(out) :: tp
    character(len=*),intent(out) :: name
    integer,         intent(out) :: div
    integer,         intent(out) :: padding(*)
    integer,         intent(in)  :: mdef
    real(kind=KTGT), intent(in)  :: odef
    character(len=*),intent(in)  :: tdef
    character(len=*),intent(in)  :: ndef
    integer,         intent(in)  :: ddef
    integer,         intent(in)  :: pdef(*)
    character(len=*),intent(in)  :: atag

    integer,parameter   :: larg = 256
    character(len=larg) :: aval
    integer,parameter :: lgrp = 2
    character(len=larg) :: agrp(lgrp)
    integer,parameter :: li = 4
    character(len=larg) :: aitems(li)
    integer ni, ngrp
    real(kind=KTGT) :: vbuf
    integer jerr
    character(len=lmsg) :: txt

    ! LA=[NAME/]NUM[:DIV[:OFS]][:TYPE[+PADDING[-PADDING]]]     if no default num
    ! LA=[NAME/][NUM:][DIV:[OFS]][:TYPE[+PADDING[-PADDING]]]   if with default num

    ierr = 0
    if (ierr.eq.0) call get_last_option(ierr, aval, atag, def=' ')
    if (ierr.eq.0) then
       agrp(:) = ' '
       call split_list(ngrp, agrp, aval, sep_name, lgrp)
       ierr = min(0, ngrp)
    endif
    if (ierr.eq.0) then
       aitems(:) = ' '
       if (ngrp.eq.2) then
          call split_list(ni, aitems, agrp(2), sep_item, li)
       else if (ngrp.eq.1) then
          call split_list(ni, aitems, agrp(1), sep_item, li)
          if (ni.gt.0) then
             call parse_number(jerr, vbuf, aitems(1))
             if (jerr.eq.0) then
                agrp(1) = ' '
             else
                aitems(:) = ' '
             endif
          endif
       else if (ngrp.eq.0) then
          ni = 0
       else
          ni = ERR_INVALID_PARAMETER
       endif
    else
       ni = ERR_INVALID_PARAMETER
    endif
    if (ierr.eq.0) ierr = min(0, ni)
    if (ierr.eq.0) then
       name = agrp(1)
       if (name.eq.' ') name = ndef
    endif
    if (ierr.eq.0) then
       ! shift TYPE option to last item
       if (ni.ge.1.and.ni.lt.li) then
          call parse_number(jerr, vbuf, aitems(ni))
          if (jerr.ne.0) then
             aitems(li) = aitems(ni)
             aitems(ni) = ' '
          endif
       endif
       if (mdef.ge.0.and.ni.lt.li) then
          aitems(2:li-1) = aitems(1:li-2)
          aitems(1) = ' '
       endif
    endif
    if (ierr.eq.0) call parse_number(ierr, mem, aitems(1), mdef)
    if (ierr.eq.0) call parse_number(ierr, div, aitems(2), 0)
    if (ierr.eq.0) div = max(1, div)
    if (ierr.eq.0) call parse_number(ierr, ofs, aitems(3), odef)
    if (ierr.eq.0) then
       tp = aitems(4)
       if (tp.eq.' ') tp = tdef
       call upcase(tp)
       if (verify(trim(tp), geo_all).ne.0) ierr = ERR_INVALID_PARAMETER
    endif

    if (ierr.ne.0) then
       write(*, *) 'cannot parse geo-coordinate argument ', &
            & trim(atag), '=', trim(aval)
    endif
107 format('sphere coordinate ', A, ' = ', '[', A, '] ', I0, '*', I0, 1x, A, 1x, F12.3)
    if (ierr.eq.0) then
       if (is_verbose(msglev_NORMAL)) then
          write(txt, 107) trim(atag), trim(name), mem, div, trim(tp), ofs
          call message(ierr, txt)
       endif
    endif
  end subroutine parse_geo_coordinate

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
end program qoxi
!!!_* obsolete
#if 0 /* obsolete */
  ! return

  ! if (ierr.eq.0) then
  !    call parse_args &
  !         & (ierr,  &
  !         &  ofile,   major, flatten, cproj, latts, &
  !         &  iterini, iterlim)
  ! endif
  ! if (ofile.eq.' ') then
  !    write(*, *) 'Need output file'
  ! else
  !    if (ierr.eq.0) then
  !       cproj(1:NGEOG) = deg2rad(cproj(1:NGEOG))
  !       latts = deg2rad(latts)
  !    endif
  !    ! hard-coded invocation of main
  !    if (ierr.eq.0) then
  !       xl = -641150.0_KTGT - 1500.0_KTGT
  !       xh = +867850.0_KTGT + 1500.0_KTGT
  !       yl = -3375050.0_KTGT - 1500.0_KTGT
  !       yh = -642050.0_KTGT  + 1500.0_KTGT

  !       dx = 6000.0_KTGT
  !       dy = 6000.0_KTGT

  !       ! dx = 750000.0_KTGT
  !       ! dy = 750000.0_KTGT

  !       ! -750   0   +750
  !       !    x   x   x
  !       !  o   o   o   o
  !       !  0 1 2 3 4 5 6

  !       ! dx = 500000.0_KTGT
  !       ! dy = 500000.0_KTGT
  !       ! -750     0     +750
  !       !    x   x   x   x
  !       !  o   o   o   o   o
  !       !  0 1 2 3 4 5 6 7 8
  !       !    0:3 4   5:8
  !       !    0:3 3.5 4:7

  !       ! dx = 50000.0_KTGT
  !       ! dy = 50000.0_KTGT

  !       ! #define BENCH_W 1500000.0_KTGT
  !       ! ! #define BENCH_W 1400000.0_KTGT
  !       !      xl = - BENCH_W / 2.0_KTGT - dx / 2.0_KTGT
  !       !      xh = + BENCH_W / 2.0_KTGT + dx / 2.0_KTGT
  !       !      yl = - BENCH_W / 2.0_KTGT - dy / 2.0_KTGT
  !       !      yh = + BENCH_W / 2.0_KTGT + dy / 2.0_KTGT

  !       xpfx = 'TESTX'
  !       ypfx = 'TESTY'
  !       ! major = 6378137.0_KTGT
  !       ! flatten = 1.0_KTGT / 298.257223563_KTGT

  !       ! latts = deg2rad(70.0_KTGT)
  !       ! cproj(JLONGI) = deg2rad(-45.0_KTGT)
  !       ! cproj(JLATI)  = deg2rad(90.0_KTGT)
  !       gorg(JLONGI)  = cproj(JLONGI)
  !       gorg(JLATI)   = cproj(JLATI)

  !       ! latts = deg2rad(-71.0_KTGT)
  !       ! cproj(JLONGI) = deg2rad(5.0_KTGT)
  !       ! cproj(JLATI)  = deg2rad(-90.0_KTGT)
  !       ! gorg(JLONGI)  = cproj(JLONGI)
  !       ! gorg(JLATI)   = cproj(JLATI)

  !       ! gorg(JLATI)   = latts

  !       dtol = 0.0_KTGT
  !       atol = 0.0_KTGT

  !       call qoxi_main &
  !            & (ierr, &
  !            &  xl,      xh,      dx,    xpfx,  yl, yh, dy, ypfx, &
  !            &  gorg,    cproj,   latts, major, flatten, &
  !            &  iterini, iterlim, dtol,  atol,  ofile)
  !    endif
  ! endif

! !!!_  - init
!   subroutine init(ierr)
!     use TOUZA_Nio,only: nio_init=>init
!     implicit none
!     integer,intent(out) :: ierr
!     ierr = 0
!     call nio_init(ierr)
!   end subroutine init
! !!!_  - finalize
!   subroutine finalize(ierr, u)
!     use TOUZA_Nio,only: nio_finalize=>finalize
!     implicit none
!     integer,intent(out)         :: ierr
!     integer,intent(in),optional :: u
!     ierr = 0
!     call nio_finalize(ierr)
!   end subroutine finalize
! !!!_  - parse_args
!   subroutine parse_args &
!        & (ierr, &
!        &  opath, &
!        &  major,   flatten, cproj, latts, &
!        &  iterini, iterlim)
!     use TOUZA_Std,only: arg_init, arg_diag, parse
!     implicit none
!     integer,         intent(out) :: ierr
!     character(len=*),intent(out) :: opath
!     real(kind=KTGT), intent(out) :: major, flatten
!     real(kind=KTGT), intent(out) :: cproj(*), latts
!     integer,         intent(out) :: iterini,  iterlim

!     integer,parameter :: larg = 128
!     integer japos
!     character(len=lpath) :: arg
!     character(len=larg)  :: ellipse, proj
!     character(len=*),parameter :: sep_item = ':'
!     integer stat
!     integer ni
!     integer,parameter :: litems = 4
!     real(kind=KTGT) :: ritems(litems)
!     real(kind=KTGT),parameter :: ZERO=0.0_KTGT
!     character(len=larg) :: citems(litems)
!     integer :: iitems(litems)

!     integer jarg

!     ierr = 0
!     jarg = 1

!     if (ierr.eq.0) call arg_init(ierr)
!     if (ierr.eq.0) call parse(ierr)
!     if (ierr.eq.0) call arg_diag(ierr, mode=MODE_SURFACE)

!     if (ierr.eq.0) call get_param(ierr, opath, jarg, ' ')

!     ! E=MAJOR/FLATTENING
!     if (ierr.eq.0) call get_option(ierr, ellipse, 'E', ' ')
!     if (ierr.eq.0) then
!        if (index(ellipse, sep_item).gt.0) then
!           ritems(:) = ZERO
!           call split_list(ni, ritems, ellipse, sep_item, 2)
!           if (ni.lt.0) then
!              ierr = ni
!           else
!              if (ni.eq.1) ritems(2) = ZERO
!              if (ritems(1).eq.ZERO) ritems(1) = 6370000.0_KTGT
!              major = ritems(1)
!              flatten = ritems(2)
!           endif
!        else
!           if (ellipse.eq.' ') ellipse = 'WGS84'
!           call upcase(ellipse)
!           select case (ellipse)
!           case('WGS84')
!              major = 6378137.0_KTGT
!              flatten = 1.0_KTGT / 298.257223563_KTGT
!           case('MIROC')
!              major = 6370000.0_KTGT
!              flatten = 0.0_KTGT
!           case default
!              ierr = ERR_PANIC
!           end select
!        endif
!     endif
!     ! P=LAT/LON/TRUE
!     ! P=DEF/TRUE
!     if (ierr.eq.0) call get_option(ierr, proj, 'P', ' ')
!     if (ierr.eq.0) then
!        citems(:) = ' '
!        call split_list(ni, citems, proj, sep_item, 3)
!        if (ni.lt.0) ierr = ni
!     endif
!     if (ierr.eq.0) then
!        call upcase(citems(1))
!        select case(citems(1))
!        case('GL')
!           citems(3) = citems(2)
!           citems(1) = '90'
!           citems(2) = '-45'
!           if (citems(3).eq.' ') citems(3) = '70'
!        end select
!        if (citems(1).eq.' ') citems(1) = '90'
!        if (citems(2).eq.' ') citems(1) = '0'
!        call parse_number(ierr, cproj(1), citems(1))
!        if (ierr.eq.0) call parse_number(ierr, cproj(2), citems(2))
!        if (ierr.eq.0) call parse_number(ierr, latts, citems(3), cproj(1))   ! lat center if blank
!     endif
!     ! L=ITERATION-LIMIT/INITIAL
!     if (ierr.eq.0) call get_option(ierr, iitems(1:2), 'L', 0, sep=sep_item)
!     if (ierr.eq.0) then
!        if (iitems(1).le.0) iitems(1) = 7
!        if (iitems(2).lt.0) iitems(2) = max(0, iitems(1) - 2)
!        iterlim = iitems(1)
!        iterini = max(0, min(iitems(2), iterlim - 1))
!     endif

!     ! X=LOW/HIGH/SPACING
!     ! Y=LOW/HIGH/SPACING
!     ! O=LAT/LON

!   end subroutine parse_args

!!!_  - qoxi_main
  subroutine qoxi_main &
       & (ierr, &
       &  xl,      xh,      dx,    xpfx,  yl, yh, dy, ypfx, &
       &  gorg,    cproj,   latts, major, flatten, &
       &  iterini, iterlim, dtol,  atol,  &
       &  ofile)
    use TOUZA_Emu, only: ncache_psgp_co, psgp_set_byf, psgp_fwd
    use TOUZA_Emu, only: psgp_bwd_ll, psgp_bwd_length, psgp_bwd_area, psgp_bwd_geod
    use TOUZA_Emu, only: psgp_surf_area
    use TOUZA_Nio, only: litem, nitem, put_header_cprop
    use TOUZA_Nio, only: put_item, get_default_header, REC_BIG
    use TOUZA_Nio, only: nio_write_header, nio_write_data
    use TOUZA_Nio, only: hi_ITEM, hi_DFMT, hi_UNIT
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,intent(out) :: ierr
    real(kind=KTGT),intent(in)  :: xl, xh, dx       ! low/high/spacing on plane (x)   [m]
    real(kind=KTGT),intent(in)  :: yl, yh, dy       ! low/high/spacing on plane (y)   [m]
    character(len=*),intent(in) :: xpfx, ypfx       ! prefix of coordinate names
    real(kind=KTGT),intent(in)  :: gorg(*)          ! lat/lon of plane origin         [rad]
    real(kind=KTGT),intent(in)  :: cproj(*)         ! projection center lat/lon       [rad]
    real(kind=KTGT),intent(in)  :: latts            ! true scale latitude             [rad]
    real(kind=KTGT),intent(in)  :: major,   flatten ! semi-major radius [m], flatten
    integer,        intent(in)  :: iterini, iterlim ! iteration parameters
    real(kind=KTGT),intent(in)  :: dtol,    atol    ! tolerance on length and area
    character(len=*),intent(in) :: ofile            ! output file
!!!_   . xy-map properties
    integer xspan, yspan
    real(kind=KTGT) :: jxsymm, jysymm

    real(kind=KTGT) :: xyorg(2)
!!!_   . field variables to output
    real(kind=KTGT),allocatable :: dx_geo(:, :), dy_geo(:, :)    ! spacing by geodesic computation
    real(kind=KTGT),allocatable :: dx_nis(:, :), dy_nis(:, :)    ! spacing by numerical integration of inverse scale factor
    real(kind=KTGT),allocatable :: dx_res(:, :), dy_res(:, :)    ! residuals

    real(kind=KTGT),allocatable :: area_geo(:, :)
    real(kind=KTGT),allocatable :: area_nis(:, :)
    real(kind=KTGT),allocatable :: area_res(:, :)

    real(kind=KTGT),allocatable :: posx_car(:, :), posy_car(:, :)   ! cartesian coordinate on the projection (pole origin)
    real(kind=KTGT),allocatable :: posx_pln(:, :), posy_pln(:, :)   ! plane coordinate on the projection
    real(kind=KTGT),allocatable :: posx_geo(:, :), posy_geo(:, :)   ! virtual relative coordinate by geodesic
    real(kind=KTGT),allocatable :: posx_nis(:, :), posy_nis(:, :)   ! virtual relative coordinate by numerical integration

    real(kind=KTGT),allocatable :: lati(:, :), longi(:, :)

!!!_   . index
    integer nx, mxh
    integer ny, myh
    integer jx, jy, jxh, jyh
    integer lwx, lwy, lwv, lwn
!!!_   . output file
    integer krect
    integer ufile
    character(len=litem) :: hd(nitem)
!!!_   . misc
    real(kind=KTGT) :: ll(NGEOG)
    real(kind=KTGT) :: cco(ncache_psgp_co)

    real(kind=KTGT) :: glat1(NTRIG), dlo1(NTRIG)
    real(kind=KTGT) :: glat2(NTRIG), dlo2(NTRIG)

    real(kind=KTGT) :: pdis, parea, pres
    real(kind=KTGT) :: gdis, garea, gdres, gares
    real(kind=KTGT),parameter :: HALF=0.5_KTGT
    real(kind=KTGT),parameter :: ONE=1.0_KTGT
    real(kind=KTGT),parameter :: ZERO=0.0_KTGT

    real(kind=KTGT),allocatable :: gss(:), rss(:)  ! temporary geodesic area along x
    real(kind=KTGT) :: gsw, gsn, gse
    real(kind=KTGT) :: rsw, rsn, rse
    real(kind=KTGT) :: gdtmph(4), gdtmpv(4)
    real(kind=KTGT) :: atmp(2), rtmp(2)
    real(kind=KTGT) :: gds(2), gdn(2), gdw(2), gde(2)
    real(kind=KTGT) :: ahalf
!!!_   . body
    ierr = 0

102 format('qoxi: ', A, '=', F10.1)
103 format('qoxi: ', A, '=', ES16.9)
104 format('qoxi: ', A, '=', F10.1, ',', F10.1)
105 format('qoxi: ', A, '=', F10.1)
    write(*, 102) 'major',   major
    write(*, 103) 'flatten', flatten
    write(*, 104) 'cproj', rad2deg(cproj(1)), rad2deg(cproj(2))
    write(*, 105) 'latts', rad2deg(latts)

    if (ierr.eq.0) call psgp_set_byf(ierr, cco, flatten, major, latts, cproj)
    if (ierr.eq.0) xyorg = psgp_fwd(gorg(JLONGI), gorg(JLATI), cco)

    if (ierr.eq.0) call set_domain(ierr, nx, mxh, xspan, jxsymm, xl, xh, dx, xyorg(1), 'X')
    if (ierr.eq.0) call set_domain(ierr, ny, myh, yspan, jysymm, yl, yh, dy, xyorg(2), 'Y')

    if (ierr.eq.0) then
       allocate(posx_car(0:mxh, 0:myh), posx_pln(0:mxh, 0:myh), posx_geo(0:mxh, 0:myh), posx_nis(0:mxh, 0:myh), &
            &   posy_car(0:mxh, 0:myh), posy_pln(0:mxh, 0:myh), posy_geo(0:mxh, 0:myh), posy_nis(0:mxh, 0:myh), &
            &   dx_geo(0:mxh-1, 0:myh), dx_nis(0:mxh-1, 0:myh), dx_res(0:mxh-1, 0:myh), &
            &   dy_geo(0:mxh, 0:myh-1), dy_nis(0:mxh, 0:myh-1), dy_res(0:mxh, 0:myh-1), &
            &   area_geo(0:nx-1, 0:ny-1), area_nis(0:nx-1, 0:ny-1), area_res(0:nx-1, 0:ny-1), &
            &   lati(0:mxh, 0:myh), longi(0:mxh, 0:myh), &
            &   gss(0:nx-1), rss(0:nx-1), &
            &   STAT=ierr)
    endif
    if (ierr.eq.0) then
       do jx = 0, mxh
          posx_car(jx, :) = (xl + xyorg(1)) + dx * (real(jx, kind=KTGT)) * HALF
          posx_pln(jx, :) = xl              + dx * (real(jx, kind=KTGT)) * HALF
       enddo
       do jy = 0, myh
          posy_car(:, jy) = (yl + xyorg(2)) + dy * (real(jy, kind=KTGT)) * HALF
          posy_pln(:, jy) = yl              + dy * (real(jy, kind=KTGT)) * HALF
       enddo
    endif
201 format('qoxi:', A)
    if (ierr.eq.0) then
       write(*, 201) 'lat/lon'
       do jy = 0, myh
          do jx = 0, mxh
             ll = psgp_bwd_ll(posx_car(jx, jy), posy_car(jx, jy), cco)
             lati(jx, jy) = ll(JLATI)
             longi(jx, jy) = ll(JLONGI)
             ! write(*, *) jx, jy, rad2deg(lati(jx,jy)), rad2deg(longi(jx,jy))
          enddo
       enddo
    endif
    if (ierr.eq.0) then
       write(*, 201) 'dx num'
       do jy = 0, myh
          do jx = 0, mxh - 1
             call psgp_bwd_length &
                  & (pdis, &
                  &  posx_car(jx,   jy), posy_car(jx,   jy), &
                  &  posx_car(jx+1, jy), posy_car(jx+1, jy), &
                  &  cco, iterini, iterlim, dtol, pres)
             dx_nis(jx, jy) = pdis
             dx_res(jx, jy) = pres
          enddo
       enddo
       call symmetry_check &
            & (ierr, dx_nis, mxh, myh + 1, jxsymm - HALF, jysymm,  0, 'dxn')
       call symmetry_check &
            & (ierr, dx_res, mxh, myh + 1, jxsymm - HALF, jysymm,  0, 'rxn')
    endif
    if (ierr.eq.0) then
       write(*, 201) 'dy num'
       do jy = 0, myh - 1
          do jx = 0, mxh
             call psgp_bwd_length &
                  & (pdis, &
                  &  posx_car(jx, jy),   posy_car(jx, jy), &
                  &  posx_car(jx, jy+1), posy_car(jx, jy+1), &
                  &  cco, iterini, iterlim, dtol, pres)
             dy_nis(jx, jy) = pdis
             dy_res(jx, jy) = pres
          enddo
       enddo
       call symmetry_check &
            & (ierr, dy_nis, mxh + 1, myh, jxsymm, jysymm - HALF,  0, 'dyn')
       call symmetry_check &
            & (ierr, dy_res, mxh + 1, myh, jxsymm, jysymm - HALF,  0, 'ryn')
    endif

    if (ierr.eq.0) then
       write(*, 201) 'geodesic'
       ! southern edge
       jyh = 0
       do jxh = 0, mxh - 1, 2
          jx = jxh / 2
          call comp_cell_geod_side &
               & (gds, gss(jx), rss(jx), &
               &  posx_car(jxh,   jyh), posy_car(jxh,   jyh), &
               &  posx_car(jxh+2, jyh), posy_car(jxh+2, jyh), &
               &  cco, iterini, iterlim, atol)
          dx_geo(jxh:jxh+1, jyh) = gds(1:2)
          ! write(*, *) jxh, jyh, gds
       enddo
       do jyh = 0, myh - 1, 2
          jy = jyh / 2
          ! western edge
          jxh = 0
          call comp_cell_geod_side &
               & (gdw, gsw,  rsw, &
               &  posx_car(jxh, jyh+2), posy_car(jxh, jyh+2), &
               &  posx_car(jxh, jyh),   posy_car(jxh, jyh), &
               &  cco, iterini, iterlim, atol)
          dy_geo(jxh, jyh:jyh+1) = gdw(2:1:-1)
          do jxh = 0, mxh - 1, 2
             jx = jxh / 2
             call comp_cell_geod_hv &
                  & (gdtmph, gdtmpv, atmp, rtmp, &
                  &  posx_car(jxh,   jyh),   posy_car(jxh,   jyh), &
                  &  posx_car(jxh+2, jyh+2), posy_car(jxh+2, jyh+2), &
                  &  cco, iterini, iterlim, atol)
             dx_geo(jxh:jxh+1, jyh+1) = gdtmph(1:2)
             dx_geo(jxh:jxh+1, jyh+2) = gdtmph(3:4)
             dy_geo(jxh+1, jyh:jyh+1) = gdtmpv(1:2)
             dy_geo(jxh+2, jyh:jyh+1) = gdtmpv(3:4)
             area_geo(jx, jy) = - ((atmp(2) + gss(jx)) + (gsw + atmp(1)))

             if ((posx_car(jxh,jyh).lt.-xyorg(1).and.-xyorg(1).lt.posx_car(jxh+2,jyh+2)) &
                  & .and. (posy_car(jxh,jyh).lt.-xyorg(2).and.-xyorg(2).lt.posy_car(jxh+2,jyh+2))) then
                ahalf = psgp_surf_area(cco) * HALF
                if (area_geo(jx, jy).lt.ZERO) then
                   area_geo(jx, jy) = area_geo(jx, jy) + ahalf
                else
                   area_geo(jx, jy) = area_geo(jx, jy) - ahalf
                endif
             endif
             gss(jx) = - atmp(2)
             gsw = - atmp(1)
          enddo
       enddo
       call symmetry_check &
            & (ierr, area_geo, nx, ny, jxsymm, jysymm,  0, 'areag')
    endif

    if (ierr.eq.0) then
       write(*, 201) 'area num'
       do jy = 0, ny - 1
          jyh = jy * 2
          do jx = 0, nx - 1
             jxh = jx * 2
             ! write(*, *) &
             !      &  posx_car(jxh,   jyh),   posy_car(jxh,   jyh),   &
             !      &  posx_car(jxh+2, jyh+2), posy_car(jxh+2, jyh+2)
             call psgp_bwd_area &
                  & (parea, &
                  &  posx_car(jxh,   jyh),   posy_car(jxh,   jyh),   &
                  &  posx_car(jxh+2, jyh+2), posy_car(jxh+2, jyh+2), &
                  &  cco, iterini, iterlim, atol, pres)
             area_nis(jx, jy) = parea
             area_res(jx, jy) = pres
          enddo
       enddo
       call symmetry_check &
            & (ierr, area_nis, nx, ny, jxsymm, jysymm,  0, 'areai')
       call symmetry_check &
            & (ierr, area_res, nx, ny, jxsymm, jysymm,  0, 'rarea')
    endif

    if (ierr.eq.0) then
    endif

    if (ierr.eq.0) then
       ufile = new_unit()
       call sus_open(ierr, ufile, ofile, ACTION='W', STATUS='R')
    endif

    if (ierr.eq.0) call get_default_header(hd)
    if (ierr.eq.0) call put_item(ierr, hd, 'UR8',  hi_DFMT)

    if (ierr.eq.0) then
       krect = REC_BIG
       lwn = (nx  + 0) * (ny  + 0)
       lwv = (mxh + 1) * (myh + 1)
       lwx = (mxh + 0) * (myh + 1)
       lwy = (mxh + 1) * (myh + 0)
    endif
    ! 0:mxh, 0:myh
    if (ierr.eq.0) call put_header_cprop(ierr, hd, 'x', (/1, mxh+1/), 1)
    if (ierr.eq.0) call put_header_cprop(ierr, hd, 'y', (/1, myh+1/), 2)

    if (ierr.eq.0) call put_item(ierr, hd, 'cxp', hi_ITEM)
    if (ierr.eq.0) call put_item(ierr, hd, 'm', hi_UNIT)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krect, ufile)
    if (ierr.eq.0) call nio_write_data(ierr, RESHAPE(posx_car, (/lwv/)), lwv, hd, krect, ufile)

    if (ierr.eq.0) call put_item(ierr, hd, 'cyp', hi_ITEM)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krect, ufile)
    if (ierr.eq.0) call nio_write_data(ierr, RESHAPE(posy_car, (/lwv/)), lwv, hd, krect, ufile)

    if (ierr.eq.0) call put_item(ierr, hd, 'lat', hi_ITEM)
    if (ierr.eq.0) call put_item(ierr, hd, 'rad', hi_UNIT)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krect, ufile)
    if (ierr.eq.0) call nio_write_data(ierr, RESHAPE(lati, (/lwv/)), lwv, hd, krect, ufile)

    if (ierr.eq.0) call put_item(ierr, hd, 'lon', hi_ITEM)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krect, ufile)
    if (ierr.eq.0) call nio_write_data(ierr, RESHAPE(longi, (/lwv/)), lwv, hd, krect, ufile)

    ! 0:mxh-1, 0:myh
    if (ierr.eq.0) call put_header_cprop(ierr, hd, 'x', (/1, mxh/),   1)
    if (ierr.eq.0) call put_header_cprop(ierr, hd, 'y', (/1, myh+1/), 2)

    if (ierr.eq.0) call put_item(ierr, hd, 'dxi', hi_ITEM)
    if (ierr.eq.0) call put_item(ierr, hd, 'm',   hi_UNIT)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krect, ufile)
    if (ierr.eq.0) call nio_write_data(ierr, RESHAPE(dx_nis, (/lwx/)), lwx, hd, krect, ufile)

    if (ierr.eq.0) call put_item(ierr, hd, 'resdxi', hi_ITEM)
    if (ierr.eq.0) call put_item(ierr, hd, 'm',      hi_UNIT)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krect, ufile)
    if (ierr.eq.0) call nio_write_data(ierr, RESHAPE(dx_res, (/lwx/)), lwx, hd, krect, ufile)

    if (ierr.eq.0) call put_item(ierr, hd, 'dxg', hi_ITEM)
    if (ierr.eq.0) call put_item(ierr, hd, 'm',   hi_UNIT)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krect, ufile)
    if (ierr.eq.0) call nio_write_data(ierr, RESHAPE(dx_geo, (/lwx/)), lwx, hd, krect, ufile)

    ! 0:mxh, 0:myh-1
    if (ierr.eq.0) call put_header_cprop(ierr, hd, 'x', (/1, mxh+1/),   1)
    if (ierr.eq.0) call put_header_cprop(ierr, hd, 'y', (/1, myh/),     2)

    if (ierr.eq.0) call put_item(ierr, hd, 'dyi', hi_ITEM)
    if (ierr.eq.0) call put_item(ierr, hd, 'm',   hi_UNIT)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krect, ufile)
    if (ierr.eq.0) call nio_write_data(ierr, RESHAPE(dy_nis, (/lwy/)), lwy, hd, krect, ufile)

    if (ierr.eq.0) call put_item(ierr, hd, 'resdyi', hi_ITEM)
    if (ierr.eq.0) call put_item(ierr, hd, 'm',      hi_UNIT)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krect, ufile)
    if (ierr.eq.0) call nio_write_data(ierr, RESHAPE(dy_res, (/lwy/)), lwy, hd, krect, ufile)

    if (ierr.eq.0) call put_item(ierr, hd, 'dyg', hi_ITEM)
    if (ierr.eq.0) call put_item(ierr, hd, 'm',   hi_UNIT)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krect, ufile)
    if (ierr.eq.0) call nio_write_data(ierr, RESHAPE(dy_geo, (/lwy/)), lwy, hd, krect, ufile)

    ! 0:nx-1, 0:ny-1
    if (ierr.eq.0) call put_header_cprop(ierr, hd, 'x', (/1, nx/),   1)
    if (ierr.eq.0) call put_header_cprop(ierr, hd, 'y', (/1, ny/),   2)

    if (ierr.eq.0) call put_item(ierr, hd, 'areai', hi_ITEM)
    if (ierr.eq.0) call put_item(ierr, hd, 'm^2',   hi_UNIT)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krect, ufile)
    if (ierr.eq.0) call nio_write_data(ierr, RESHAPE(area_nis, (/lwn/)), lwn, hd, krect, ufile)

    if (ierr.eq.0) call put_item(ierr, hd, 'resareai', hi_ITEM)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krect, ufile)
    if (ierr.eq.0) call nio_write_data(ierr, RESHAPE(area_res, (/lwn/)), lwn, hd, krect, ufile)

    if (ierr.eq.0) call put_item(ierr, hd, 'areag', hi_ITEM)
    if (ierr.eq.0) call put_item(ierr, hd, 'm^2',   hi_UNIT)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krect, ufile)
    if (ierr.eq.0) call nio_write_data(ierr, RESHAPE(area_geo, (/lwn/)), lwn, hd, krect, ufile)

    write(*, *) 'qoxi_main', ierr
    return
  end subroutine qoxi_main

!!!_  - gen_table
!!!_  - comp_prim_plain - plain computation suite
!!!_  - comp_prim_nisf - computation suite by numerical integration of scale factors
!!!_  - comp_prim_geod - computation suite by numerical integration of geodesic
!!!_  - comp_cell_geod_side
  subroutine comp_cell_geod_side &
       & (dist, garea, ares, &
       &  x0, y0, x2, y2, cco, iterini, iterlim, atol)
    use TOUZA_Emu, only: psgp_bwd_geod
    implicit none
    integer,parameter :: KTGT=KDBL
    real(kind=KTGT),intent(out) :: dist(*)
    real(kind=KTGT),intent(out) :: garea
    real(kind=KTGT),intent(out) :: ares
    real(kind=KTGT),intent(in)  :: x0, y0, x2, y2
    real(kind=KTGT),intent(in)  :: cco(*)
    integer,        intent(in)  :: iterini, iterlim
    real(kind=KTGT),intent(in)  :: atol

    real(kind=KTGT) :: x1,  y1
    real(kind=KTGT) :: s01, s12
    real(kind=KTGT) :: r01, r12
    real(kind=KTGT),parameter :: HALF=0.5_KTGT

    x1 = (x0 + x2) * HALF
    y1 = (y0 + y2) * HALF

    call psgp_bwd_geod &
         & (s01, dist(1), &
         &  x0, y0, x1, y1, &
         &  cco, iterini, iterlim, atol, r01)

    call psgp_bwd_geod &
         & (s12, dist(2), &
         &  x1, y1, x2, y2, &
         &  cco, iterini, iterlim, atol, r12)

    garea = s01 + s12
    ares  = r01 + r12
    return
  end subroutine comp_cell_geod_side
!!!_  - comp_cell_geod_hv
  subroutine comp_cell_geod_hv &
       & (disth, distv, garea, ares, &
       &  x0, y0, x2, y2, cco, iterini, iterlim, atol)
    implicit none
    integer,parameter :: KTGT=KDBL
    real(kind=KTGT),intent(out) :: disth(*)
    real(kind=KTGT),intent(out) :: distv(*)
    real(kind=KTGT),intent(out) :: garea(*)    ! 1:east  2:north
    real(kind=KTGT),intent(out) :: ares(*)
    real(kind=KTGT),intent(in)  :: x0, y0, x2, y2
    real(kind=KTGT),intent(in)  :: cco(*)
    integer,        intent(in)  :: iterini, iterlim
    real(kind=KTGT),intent(in)  :: atol

    real(kind=KTGT) :: x1,  y1
    real(kind=KTGT),parameter :: HALF=0.5_KTGT
    real(kind=KTGT) :: dtmp(2), stmp, rtmp

    x1 = (x0 + x2) * HALF
    y1 = (y0 + y2) * HALF

    call comp_cell_geod_side &
         & (dtmp,  stmp, rtmp, &
         &  x0, y1, x2, y1, cco, iterini, iterlim, atol)
    disth(1:2) = dtmp(1:2)
    ! discard area

    call comp_cell_geod_side &
         & (dtmp,  stmp, rtmp, &
         &  x2, y2, x0, y2, cco, iterini, iterlim, atol)
    disth(3:4) = dtmp(2:1:-1)
    garea(2) = stmp
    ares(2) = rtmp

    call comp_cell_geod_side &
         & (dtmp,  stmp, rtmp, &
         &  x1, y0, x1, y2, cco, iterini, iterlim, atol)
    distv(1:2) = dtmp(1:2)
    ! discard area

    call comp_cell_geod_side &
         & (dtmp,  stmp, rtmp, &
         &  x2, y0, x2, y2, cco, iterini, iterlim, atol)
    distv(3:4) = dtmp(1:2)
    garea(1) = stmp
    ares(1) = rtmp

    return
  end subroutine comp_cell_geod_hv

!!!_  - batch_generate_cache

!!!_  - adjust_llrange
  subroutine adjust_llrange &
       & (nlogx, jbgnx, jendx, jbgni, jendi, m, &
       &  mdiv,  cdiv)
    implicit none
    integer,intent(out) :: nlogx              ! logical size, excluding wedges
    integer,intent(out) :: jbgnx, jendx
    integer,intent(in)  :: jbgni, jendi, m
    integer,intent(in),optional :: mdiv
    integer,intent(in),optional :: cdiv(0:*)

    integer jd, md

    jbgnx = jbgni
    jendx = jendi

    if (jbgnx.lt.0) jbgnx = m + 1 + jbgnx
    if (jbgnx.lt.0) jbgnx = 0

    if (jendx.lt.0) jendx = m + 1 + jendx
    if (jendx.le.0) jendx = jbgnx + 1

    nlogx = jendx - jbgnx

    md = choice(0, mdiv)
    if (md.gt.0) then
       do jd = md, 1, -1
          if (jendx.gt.cdiv(jd) - jd) then
             jendx = jendx + jd
             exit
          endif
       enddo
       do jd = md, 1, -1
          if (jbgnx.gt.cdiv(jd) - jd) then
             jbgnx = jbgnx + jd
             exit
          endif
       enddo
    endif
  end subroutine adjust_llrange

#endif /* obsolete */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

!!!_! std_htb.F90 - touza/std simple hash table manager
! Maintainer: SAITO Fuyuki
! Created: Jan 28 2022
#define TIME_STAMP 'Time-stamp: <2023/10/27 10:57:04 fuyuki std_htb.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022,2023
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
!!!_* issue in old GCC Fortran
#if __GFORTRAN__
#  if __GNUC__ < 9
#     if HAVE_F2003_DEFERRED_TYPE
#        warning "Force disable F2003 deferred-length character"
#        undef  HAVE_F2003_DEFERRED_TYPE
#        define HAVE_F2003_DEFERRED_TYPE 0
#     endif
#  endif
#endif
!!!_* macros
! #define _POINTER pointer
#if HAVE_F2003_ALLOCATABLE_MEMBER
#  define _POINTER allocatable
#else  /* not HAVE_F2003_ALLOCATABLE_MEMBER */
#  define _POINTER pointer
#endif /* not HAVE_F2003_ALLOCATABLE_MEMBER */
#ifndef    OPT_HASH_BASE
#  define  OPT_HASH_BASE 13
#endif
#if OPT_HASH_BASE <= 0
#  error "invalid OPT_HASH_BASE"
#endif
#ifndef    OPT_HASH_CATEGORIES
#  define  OPT_HASH_CATEGORIES 32
#endif
#ifndef    OPT_HASH_TABLE_SIZE
#  define  OPT_HASH_TABLE_SIZE 256
#endif
#ifndef    OPT_HASH_KEY_LENGTH
#  define  OPT_HASH_KEY_LENGTH  8
#endif
#ifndef    OPT_HASH_NAME_LENGTH
#  define  OPT_HASH_NAME_LENGTH 16
#endif
#ifndef    OPT_WATERMARK_ROOT
#  define  OPT_WATERMARK_ROOT 37
#endif
#ifndef    OPT_HASH_RECORD_HISTORY
#  define  OPT_HASH_RECORD_HISTORY 1
#endif
!!!_* switch
#ifndef   TEST_STD_HTB
#  define TEST_STD_HTB 0
#endif
#if TEST_STD_HTB == 2
#  if       HAVE_F2003_DEFERRED_TYPE
#    undef  HAVE_F2003_DEFERRED_TYPE
#    define HAVE_F2003_DEFERRED_TYPE 0
#  endif
#elif TEST_STD_HTB == 3
#  ifdef  OPT_HASH_RECORD_HISTORY
#  undef  OPT_HASH_RECORD_HISTORY
#  endif
#  define OPT_HASH_RECORD_HISTORY 2
#endif
!!!_@ TOUZA_Std_htb - simple hash table
module TOUZA_Std_htb
  use TOUZA_Std_log,only: unit_global,  trace_fine,   trace_control
!!!_ = declaration
!!!_  - default
  implicit none
  private
!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL
  integer,save :: ulog = unit_global

#define __MDL__ 'htb'
#define __TAG__ STD_FORMAT_MDL(__MDL__)
#define _ERROR(E) (E - ERR_MASK_STD_HTB)

  integer,save :: err_default = _ERROR(ERR_NO_INIT)
!!!_  - parameters
  integer,parameter,public :: eundef = -1   ! undefined entry sentry

  integer,parameter,public :: unset = (- HUGE(0)) - 1

  integer,parameter,public :: flag_default = 0
  integer,parameter,public :: flag_ignore  = 1  ! ignore duplicate
!!!_  - private parameters
  integer,parameter :: kmin = min(0, eundef)
  integer,parameter :: def_ikey = +HUGE(0)
  character,parameter :: cunset = '_'

  integer,parameter          :: dummy_ikey = unset
  character(len=*),parameter :: dummy_akey = ' '
!!!_  - tables
  integer,parameter :: lname = OPT_HASH_NAME_LENGTH
#if HAVE_F2003_DEFERRED_TYPE
  integer,parameter :: kunit = 0
#else
  integer,parameter :: kunit = OPT_HASH_KEY_LENGTH
#endif
!!!_  - system key table (string + integer array)
!!!_   . Note
  type ktable_t
#  if HAVE_F2003_DEFERRED_TYPE
     character(len=:),    pointer :: akey(:) => NULL()
     ! character(len=:),    pointer :: akey(:, :) => NULL()
     ! ! The shape ktable_t%akey(0:0, :) may be simpler, but
     ! ! it does not work due to (probably) compiler bug with
     ! ! gfortran 10 11 12.
#  else
     character(len=kunit),pointer :: akey(:, :) => NULL()
#  endif
     integer,             pointer :: ikey(:, :) => NULL()
     integer :: lkey   = 0  ! akey length
     integer :: nkey   = 0  ! ikey size
     integer :: memk   = 0  ! total key size
     integer :: span   = 0  ! hash range
     integer :: base   = 0  ! hash root
     integer :: awidth = 0  ! string length for hash computation
     integer :: iwidth = 0  ! index array length for hash computation
     integer :: jctrl  = -1 ! control index

     integer         :: nhist = -1         ! key registration history for experiment
     integer,pointer :: hist(:) => NULL()
  end type ktable_t
  type(ktable_t),allocatable,save :: ktable(:)

  integer,parameter :: msystem = 2
  integer,parameter :: sys_kk = 0, sys_kw = 1
!!!_  - hashed table control
  type ctable_t
     integer         :: nslot = -1
     integer         :: uslot = -1  ! slot unit length
     integer,pointer :: nxt(:)      ! starting index at next slot
     integer,pointer :: stt(:, :)   ! associates
     integer,pointer :: msh(:)      ! stride of same hash origin
     integer         :: def = unset ! default associates
  end type ctable_t
  ! note:  nxt < 0  not set
  !        nxt ==0  no next slot for this hash
  !        nxt >0   next slot starting index
  !        msh <= 0 not set (temporaly -origin is set before fix)
  !        msh >  0 number of items in the same family

  type(ctable_t),allocatable,target,save :: ctable(:)
  integer,save :: nctrl = 0, lctrl = 0
!!!_  - watermarked hash-table shell
  type wtable_t
     integer         :: kh   = -1
     integer         :: root = 0   ! watermark parameters
     integer         :: seed = 0   !
     integer         :: n    = 0   ! automatic element current size
     integer         :: l    = 0   ! automatic element limit size
     integer,pointer :: entr(:)  ! corresponding hash entry
  end type wtable_t

  type(wtable_t),allocatable,save :: wmarks(:)

  type(wtable_t),save :: ksystem
  type(wtable_t),save :: wsystem
  integer,save        :: wseed = 0
!!!_  - interfaces
  interface new_entry
     module procedure new_entry_k, new_entry_h
  end interface new_entry
  interface settle_entry
     module procedure settle_entry_k, settle_entry_h
  end interface settle_entry
  interface reg_entry
     module procedure reg_entry_k, reg_entry_h
  end interface reg_entry
  interface query_entry
     module procedure query_entry_k, query_entry_h
  end interface query_entry
  interface query_key
     module procedure query_key_k, query_key_h
  end interface query_key
  interface query_status
     module procedure query_status_ak, query_status_ah
     module procedure query_status_ik, query_status_ih
  end interface query_status
  interface query_status_entr
     module procedure query_status_entr_ak, query_status_entr_ah
     module procedure query_status_entr_ik, query_status_entr_ih
  end interface query_status_entr

  interface get_span
     module procedure get_span_h, get_span_k
  end interface get_span

  interface bind_control
     module procedure bind_control_k, bind_control_h
  end interface bind_control

  interface hash_std
     module procedure hash_std_k
  end interface hash_std

  interface insert_entry
     module procedure insert_entry_c, insert_entry_j
  end interface insert_entry
  interface is_occupied
     module procedure is_occupied_c, is_occupied_j
  end interface is_occupied
  interface load_status
     module procedure load_status_c, load_status_j
  end interface load_status
  interface save_status
     module procedure save_status_c, save_status_j
  end interface save_status

  interface diag_ktable
     module procedure diag_ktable_k, diag_ktable_h
  end interface diag_ktable
  interface diag_wtable
     module procedure diag_wtable_t, diag_wtable_h
  end interface diag_wtable

  interface repr_ctl
     module procedure repr_ctl_c, repr_ctl_j
  end interface repr_ctl

!!!_  - public
  public init,         diag,         finalize
  public new_htable,   new_wtable
  public diag_htable,  diag_wtable

  public new_entry,    reg_entry,    settle_entry
  public query_entry,  query_key,    query_status, query_status_entr
  public load_status,  save_status

  public reg_item,     reg_alias,    search_item
  public check_handle, check_index,  get_size_items
  public normalize,    watermark
  public get_keys

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, numw, numk, numc)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: utl_init=>init, choice
    use TOUZA_Std_log,only: log_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u      ! log unit
    integer,intent(in),optional :: levv   ! verbose level
    integer,intent(in),optional :: mode   ! initialization flag
    integer,intent(in),optional :: numw   ! watermarked hash-table size
    integer,intent(in),optional :: numk   ! system key-table size
    integer,intent(in),optional :: numc   ! contronl size

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
          if (ierr.eq.0) call utl_init(ierr, ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call log_init(ierr, ulog, levv=lv, mode=lmd)
       endif
       if (is_first_force(init_counts, mode)) then
          if (ierr.eq.0) call init_system_tables(ierr, numk, numw, numc)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = _ERROR(ERR_FAILURE_INIT)
    endif

    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: utl_diag=>diag, choice
    use TOUZA_Std_log,only: log_diag=>diag, msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer lv, md, utmp, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (VCHECK_NORMAL(lv)) then
                call msg_mdl(TIME_STAMP, __MDL__, utmp)
                call msg_mdl('(''key length = '', I0)', (/kunit/), __MDL__, utmp)
                call msg_mdl('(''name length = '', I0)', (/lname/), __MDL__, utmp)
                call msg_mdl('(''deferred type = '', I0)', (/HAVE_F2003_DEFERRED_TYPE/), __MDL__, utmp)
                call diag_wtable(ierr, ksystem, '(k system)', u=utmp)
                call diag_wtable(ierr, wsystem, '(w system)', u=utmp)
                call diag_wtable(ierr, u=utmp)
                call diag_htable(ierr, u=utmp)
             endif
             if (VCHECK_DEBUG(lv)) then
                call msg_mdl('(''init = '', I0)', (/init_counts/), __MDL__, utmp)
             endif
          endif
          if (VCHECK_DETAIL(lv)) then
             continue
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call utl_diag(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call log_diag(ierr, utmp, lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: utl_finalize=>finalize, choice
    use TOUZA_Std_log,only: log_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call free_ctable(ierr)
          if (ierr.eq.0) call free_ktable_h(ierr)
          if (ierr.eq.0) call free_wtable_h(ierr)
          if (ierr.eq.0) call free_wtable_t(ierr, ksystem)
          if (ierr.eq.0) call free_wtable_t(ierr, wsystem)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call utl_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call log_finalize(ierr, utmp, lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + init subcontracts
!!!_  & init_system_tables
  subroutine init_system_tables &
       & (ierr, memk, memw, memc)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: memk, memw, memc
    integer m
    integer sysk

    ierr = 0
    if (ierr.eq.0) call init_alloc_ctable(ierr, memc)
    ! ktable
    if (ierr.eq.0) then
       m = choice(0, memk)
       if (m.le.0) m = max(msystem, OPT_HASH_CATEGORIES)
       call init_alloc_ktable(ierr, m)
    endif
    ! system key-table
    if (ierr.eq.0) then
       call decl_system_bootstrap &
            & (ierr, ksystem, sysk, '(k system)', m, lname, 0)
    endif
    ! system watermark-table
    if (ierr.eq.0) then
       m = choice(0, memw)
       if (m.le.0) m = max(msystem, OPT_HASH_CATEGORIES)
       call init_alloc_wtable(ierr, m)
    endif
    if (ierr.eq.0) then
       call decl_system_table &
            & (ierr, wsystem, sysk, '(w system)', m, lname, 0)
    endif

    return
  end subroutine init_system_tables

!!!_  & decl_system_bootstrap
  subroutine decl_system_bootstrap &
       & (ierr, wtb, sysk,   &
       &  name, mem, lname,  nkey,  &
       &  span, seed)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    type(wtable_t),  intent(inout) :: wtb
    integer,         intent(out)   :: sysk
    character(len=*),intent(in)    :: name
    integer,         intent(in)    :: mem
    integer,         intent(in)    :: lname
    integer,         intent(in)    :: nkey
    integer,optional,intent(in)    :: span
    integer,optional,intent(in)    :: seed

    integer sp
    integer e

    ierr = 0
    sp = choice(0, span)
    if (sp.le.0) sp = mem * 2

    sysk = 0

    if (ierr.eq.0) call decl_wtable(ierr, wtb, mem, seed=seed, eini=1)
    if (ierr.eq.0) call decl_ktable(ierr, ktable(sysk), sp, lname, nkey)
    if (ierr.eq.0) call bind_control_k(ierr, ktable(sysk))
    if (ierr.eq.0) wtb%kh = watermark(sysk, wtb)
    if (ierr.eq.0) then
       e = reg_entry_k(ktable(sysk), ' ' // adjustl(name), status=(/wtb%kh/))
       ierr = min(0, e)
       if (ierr.eq.0) wtb%entr(sysk) = e
    endif
  end subroutine decl_system_bootstrap

!!!_  & decl_system_table
  subroutine decl_system_table &
       & (ierr, wtb, sysk,   &
       &  name, mem, lname,  nkey,  &
       &  span, seed)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    type(wtable_t),  intent(inout) :: wtb
    integer,         intent(in)    :: sysk
    character(len=*),intent(in)    :: name
    integer,         intent(in)    :: mem
    integer,         intent(in)    :: lname
    integer,         intent(in)    :: nkey
    integer,optional,intent(in)    :: span
    integer,optional,intent(in)    :: seed

    integer sp
    integer jktbl, e

    ierr = 0
    sp = choice(0, span)
    if (sp.le.0) sp = mem * 2

    jktbl = ksystem%n
    ksystem%n = ksystem%n + 1
    if (ksystem%n.ge.ksystem%l) ierr = _ERROR(ERR_OUT_OF_RANGE)
    if (jktbl.eq.sysk) ierr = _ERROR(ERR_DUPLICATE_SET)

    if (ierr.eq.0) call decl_wtable(ierr, wtb, mem, seed=seed)
    if (ierr.eq.0) call decl_ktable(ierr, ktable(jktbl), sp, lname, nkey)
    if (ierr.eq.0) call bind_control_k(ierr, ktable(jktbl))
    if (ierr.eq.0) wtb%kh = watermark(jktbl, ksystem)
    if (ierr.eq.0) then
       e = reg_entry_k(ktable(sysk), ' ' // adjustl(name), status=(/wtb%kh/))
       ierr = min(0, e)
       if (ierr.eq.0) ksystem%entr(jktbl) = e
    endif
  end subroutine decl_system_table

!!!_  & init_alloc_ktable
  subroutine init_alloc_ktable &
       & (ierr, mem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: mem
    integer m
    ierr = 0
    m = choice(0, mem)
    if (m.le.0) m = max(0, OPT_HASH_CATEGORIES)
    if (allocated(ktable)) then
       ierr = _ERROR(ERR_ALLOCATION)
       call msg_mdl('key-table cannot allocate.', __MDL__)
    endif
    if (ierr.eq.0) then
       allocate(ktable(0:m - 1), STAT=ierr)
    endif
    return
  end subroutine init_alloc_ktable

!!!_  - init_alloc_ctable
  subroutine init_alloc_ctable &
       & (ierr, mem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: mem
    ierr = 0
    lctrl = choice(0, mem)
    if (lctrl.le.0) lctrl = max(0, OPT_HASH_CATEGORIES)
    if (allocated(ctable)) then
       ierr = _ERROR(ERR_ALLOCATION)
       call msg_mdl('key-table cannot allocate.', __MDL__)
    endif
    if (ierr.eq.0) then
       allocate(ctable(0:lctrl - 1), STAT=ierr)
       nctrl = 0
    endif
    return
  end subroutine init_alloc_ctable

!!!_  & init_alloc_wtable
  subroutine init_alloc_wtable &
       & (ierr, mem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: mem
    integer m
    ierr = 0
    m = choice(0, mem)
    if (m.le.0) m = max(0, OPT_HASH_CATEGORIES)
    if (allocated(wmarks)) then
       ierr = _ERROR(ERR_ALLOCATION)
       call msg_mdl('watermarked hash-table cannot allocate.', __MDL__)
    endif
    if (ierr.eq.0) then
       allocate(wmarks(0:m - 1), STAT=ierr)
    endif
    return
  end subroutine init_alloc_wtable

!!!_  & diag_wtable
  subroutine diag_wtable_t &
       & (ierr, wtb, name, u)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,         intent(out)         :: ierr
    type(wtable_t),  intent(in)          :: wtb
    character(len=*),intent(in),optional :: name
    integer,         intent(in),optional :: u
    character(len=128) :: buf
    integer utmp
    ierr = 0
    utmp = choice(ulog, u)

101 format(A, '[WM*', I0, '+', I0, ']')
    write(buf, 101) trim(name), wtb%root, wtb%seed
    call diag_ktable(ierr, wtb%kh, buf, utmp)

    if (ierr.eq.0) then
102    format(2x, 'items: ', I0, '/', I0)
       write(buf, 102) wtb%n, wtb%l
       call msg_mdl(buf, __MDL__, utmp)
    endif
    return
  end subroutine diag_wtable_t
!!!_   & diag_wtable_w
  subroutine diag_wtable_h &
       & (ierr, wh, u)
    implicit none
    integer,         intent(out) :: ierr
    integer,optional,intent(in)  :: wh
    integer,optional,intent(in)  :: u
    integer jw, e
    character(len=lname) :: name

    ierr = 0
    if (present(wh)) then
       jw = check_wtable(wh)
       ierr = min(0, jw)
       if (ierr.eq.0) then
          e = wsystem%entr(jw)
          call query_key(ierr, wsystem%kh, e, name)
          call diag_wtable_t(ierr, wmarks(jw), name, u=u)
       endif
    else
       do jw = 0, min(wsystem%n, wsystem%l) - 1
          if (ierr.eq.0) then
             e = wsystem%entr(jw)
             call query_key(ierr, wsystem%kh, e, name)
             call diag_wtable_t(ierr, wmarks(jw), name, u=u)
          endif
       enddo
    endif
  end subroutine diag_wtable_h

!!!_   & diag_htable
  subroutine diag_htable &
       & (ierr, hk, u)
    implicit none
    integer,         intent(out) :: ierr
    integer,optional,intent(in)  :: hk
    integer,optional,intent(in)  :: u
    integer jk, e
    character(len=lname) :: name

    ierr = 0
    if (present(hk)) then
       jk = check_ktable(hk)
       ierr = min(0, jk)
       if (ierr.eq.0) e = ksystem%entr(jk)
       if (ierr.eq.0) call query_key(ierr, ksystem%kh, e, name)
       if (ierr.eq.0) call diag_ktable(ierr, ktable(jk), name, u=u)
    else
       do jk = 0, min(ksystem%n, ksystem%l) - 1
          if (ierr.eq.0) e = ksystem%entr(jk)
          ! e < 0 means system watermarks
          if (e.lt.0) cycle
          if (ierr.eq.0) call query_key(ierr, ksystem%kh, e, name)
          if (ierr.eq.0) then
             if (name(1:1) .ne. ' ') call diag_ktable(ierr, ktable(jk), name, u=u)
          endif
       enddo
    endif
  end subroutine diag_htable

!!!_  & diag_ktable
  subroutine diag_ktable_h &
       & (ierr, handle, name, u)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in),optional :: handle
    character(len=*),intent(in),optional :: name
    integer,         intent(in),optional :: u
    integer jk
    character(len=lname) :: nbuf

101 format('HT[', I0, ']')
    if (present(handle)) then
       jk = check_ktable(handle)
       ierr = min(0, jk)
       if (ierr.eq.0) then
          if (present(name)) then
             call diag_ktable_k(ierr, ktable(jk), name, u)
          else
             write(nbuf, 101) jk
             call diag_ktable_k(ierr, ktable(jk), nbuf, u)
          endif
       endif
    else
       do jk = 0, min(ksystem%n, ksystem%l) - 1
          if (ierr.eq.0) then
             write(nbuf, 101) jk
             call diag_ktable_k(ierr, ktable(jk), nbuf, u)
          endif
       enddo
    endif
  end subroutine diag_ktable_h

  subroutine diag_ktable_k &
       & (ierr, ktb, name, u)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,         intent(out)         :: ierr
    type(ktable_t),  intent(in)          :: ktb
    character(len=*),intent(in),optional :: name
    integer,         intent(in),optional :: u
    integer utmp
    character(len=128) :: txt
    character(len=ktb%lkey*2) :: key
    character(len=128) :: ctl, buf
    integer je
    integer nset

    ierr = 0
    utmp = choice(ulog, u)
101 format('hashtable:', A, ': ', A, 1x, A)
102 format('hashtable:', 1x,      A, 1x, A)
    if (ierr.eq.0) call repr_ktable(ierr, buf, ktb)
    if (ierr.eq.0) call repr_ctable_j(ierr, ctl, ktb%jctrl)
    if (ierr.eq.0) then
       if (present(name)) then
          write(txt, 101) trim(name), trim(buf), trim(ctl)
       else
          write(txt, 102) trim(buf), trim(ctl)
       endif
       call msg_mdl(txt, __MDL__, utmp)
    endif

301 format(2x, I0, ':', I0, 1x, A, 2x, A)
    if (ierr.eq.0) then
       nset = 0
       do je = 0, get_mems_j(ktb%jctrl) - 1
          if (is_occupied(ktb%jctrl, je)) then
             if (ierr.eq.0) call repr_key(ierr, key, ktb, je)
             if (ierr.eq.0) call repr_ctl(ierr, ctl, ktb%jctrl, je)
             if (ierr.eq.0) then
                write(txt, 301) nset, je, trim(ctl), trim(key)
                call msg_mdl(txt, __MDL__, utmp)
             endif
             nset = nset + 1
          endif
       enddo
       if (ierr.eq.0) call diag_ctable_j(ierr, ktb%jctrl, nset, utmp)
    endif
    if (ierr.eq.0) call diag_ktable_history(ierr, ktb, name, utmp)

    return
  end subroutine diag_ktable_k

!!!_  & diag_ktable_history
  subroutine diag_ktable_history &
       & (ierr, ktb, name, u)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,         intent(out)         :: ierr
    type(ktable_t),  intent(in)          :: ktb
    character(len=*),intent(in),optional :: name
    integer,         intent(in),optional :: u
    integer jh, je, e0
    character(len=128) :: txt
    character(len=ktb%lkey)   :: key
    character(len=ktb%lkey*2) :: repr
    integer utmp
    integer sc

    ierr = 0
    utmp = choice(ulog, u)

101 format(' score:', A, ': ', I0)
102 format(' score:', 1x,      I0)
    if (ierr.eq.0) then
       sc = get_score_j(ktb%jctrl, 100)
       if (present(name)) then
          write(txt, 101) trim(name), sc
       else
          write(txt, 102) trim(key),  sc
       endif
       call msg_mdl(txt, __MDL__, utmp)
    endif

    do jh = 0, ktb%nhist - 1
       je = ktb%hist(jh)
       if (ierr.eq.0) call repr_key(ierr, repr, ktb, je)
#    if HAVE_F2003_DEFERRED_TYPE
       if (ierr.eq.0) key = ktb%akey(je)
#    else
       if (ierr.eq.0) key = transfer(ktb%akey(:, je), key)
#    endif
       if (ierr.eq.0) then
          e0 = hash_std(ktb, akey=key, ikey=ktb%ikey(:, je))
       endif
201    format(2x, I0, ':', I0, '>', I0, 1x, A)
       if (ierr.eq.0) then
          write(txt, 201) jh, e0, je, trim(repr)
          call msg_mdl(txt, __MDL__, utmp)
       endif
    enddo

  end subroutine diag_ktable_history

!!!_  & diag_ctable
  subroutine diag_ctable_j &
       & (ierr, jctrl, nset, u)
    implicit none
    integer,         intent(out) :: ierr
    integer,optional,intent(in)  :: jctrl
    integer,optional,intent(in)  :: nset
    integer,optional,intent(in)  :: u
    integer jc

    ierr = 0
    if (present(jctrl)) then
       jc = check_ctable(jctrl)
       ierr = min(0, jc)
       if (ierr.eq.0) then
          call diag_ctable_c(ierr, ctable(jc), nset, u)
       endif
    else
       do jc = 0, min(nctrl, lctrl) - 1
          if (ierr.eq.0) then
             call diag_ctable_c(ierr, ctable(jc), -1, u)
          endif
       enddo
    endif
  end subroutine diag_ctable_j
  subroutine diag_ctable_c &
       & (ierr, ctb, nset, u)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,         intent(out) :: ierr
    type(ctable_t),  intent(in)  :: ctb
    integer,optional,intent(in)  :: nset
    integer,optional,intent(in)  :: u
    character(len=128) :: txt
    integer ns
    integer utmp

    ierr = 0
    utmp = choice(ulog, u)
    ns = choice(-1, nset)
203 format(2x, 'content: ', I0, '/', I0, ' +', I0)
204 format(2x, 'content: ', I0, '/', I0)
    if (ns.lt.0) ns = count(ctb%nxt(:).ge.0)
    if (ctb%nslot.gt.0) then
       write(txt, 203) ns, ctb%uslot, ctb%nslot
    else
       write(txt, 204) ns, ctb%uslot
    endif
    call msg_mdl(txt, __MDL__, utmp)
  end subroutine diag_ctable_c

!!!_  - free_ctable
  subroutine free_ctable(ierr)
    implicit none
    integer,intent(out)         :: ierr
    integer jc
    ierr = 0
    if (allocated(ctable)) then
       do jc = 0, min(nctrl, lctrl) - 1
          if (ierr.eq.0) then
             deallocate(ctable(jc)%nxt, ctable(jc)%stt, ctable(jc)%msh, STAT=ierr)
          endif
       enddo
       if (ierr.eq.0) deallocate(ctable, STAT=ierr)
    endif
    return
  end subroutine free_ctable

!!!_  - free_ktable
  subroutine free_ktable_h(ierr, handle)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: handle
    integer jk
    if (present(handle)) then
       jk = check_ktable(handle)
       ierr = min(0, jk)
       if (ierr.eq.0) then
          call free_ktable_k(ierr, ktable(jk))
       endif
    else
       if (allocated(ktable)) then
          do jk = 0, min(ksystem%n, ksystem%l) - 1
             if (ierr.eq.0) then
                call free_ktable_k(ierr, ktable(jk))
             endif
          enddo
          if (ierr.eq.0) deallocate(ktable, STAT=ierr)
       endif
    endif
  end subroutine free_ktable_h

  subroutine free_ktable_k(ierr, ktb)
    implicit none
    integer,        intent(out)   :: ierr
    type(ktable_t), intent(inout) :: ktb
    ierr = 0
    if (ierr.eq.0) then
       if (associated(ktb%akey)) deallocate(ktb%akey, ktb%ikey, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (associated(ktb%hist)) deallocate(ktb%hist, STAT=ierr)
    endif
    if (ierr.eq.0) then
       ktb%akey => NULL()
       ktb%ikey => NULL()
       ktb%hist => NULL()
    endif
  end subroutine free_ktable_k

!!!_  - free_wtable
  subroutine free_wtable_h(ierr, wh)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: wh
    integer jw
    ierr = 0
    if (present(wh)) then
       jw = check_wtable(wh)
       ierr = min(0, jw)
       if (ierr.eq.0) then
          call free_wtable_t(ierr, wmarks(jw))
       endif
    else
       if (allocated(wmarks)) then
          do jw = 0, min(wsystem%n, wsystem%l) - 1
             if (ierr.eq.0) then
                call free_wtable_t(ierr, wmarks(jw))
             endif
          enddo
          if (ierr.eq.0) deallocate(wmarks, STAT=ierr)
       endif
    endif
  end subroutine free_wtable_h

  subroutine free_wtable_t(ierr, wtb)
    implicit none
    integer,        intent(out)   :: ierr
    type(wtable_t), intent(inout) :: wtb
    ierr = 0
    if (ierr.eq.0) then
       if (associated(wtb%entr)) deallocate(wtb%entr, STAT=ierr)
       if (ierr.eq.0) then
          wtb%entr => NULL()
          wtb%kh = -1
          wtb%root = 0
          wtb%seed = 0
          wtb%n = 0
          wtb%l = 0
       endif
    endif
  end subroutine free_wtable_t

!!!_ + table creation
!!!_  & new_htable
  integer function new_htable &
       & (name, &
       &  span, lkey, nkey, base, awidth, iwidth, &
       &  nstt, def,  grow, test) &
       & result(hk)
    implicit none
    character(len=*),intent(in) :: name
    integer,         intent(in) :: span        ! hash domain span
    integer,optional,intent(in) :: lkey        ! length of string key (default: 0)
    integer,optional,intent(in) :: nkey        ! size of integer-array key (default: 0)
    integer,optional,intent(in) :: base        ! hash root
    integer,optional,intent(in) :: awidth      ! string length for hash computation (default: lkey)
    integer,optional,intent(in) :: iwidth      ! array size for hash computation    (default: nkey)
    integer,optional,intent(in) :: nstt
    integer,optional,intent(in) :: def
    logical,optional,intent(in) :: grow
    logical,optional,intent(in) :: test        ! experiment
    integer jerr

    jerr = 0
    hk = new_ktable(adjustl(name), span, lkey, nkey, base, awidth, iwidth, test)
    jerr = min(0, hk)
    if (jerr.eq.0) call bind_control(jerr, hk, nstt, def, grow)
    if (jerr.lt.0) hk = jerr
  end function new_htable

!!!_  & new_wtable
  integer function new_wtable &
       & (name, mem, &
       &  lkey, nkey, span, base, awidth, iwidth, &
       &  nstt, def,  grow, &
       &  root, seed, test) &
       &  result(wh)
    use TOUZA_Std_utl,only: choice
    implicit none
    character(len=*),intent(in) :: name
    integer,         intent(in) :: mem
    integer,         intent(in) :: lkey
    integer,optional,intent(in) :: nkey, span, base, awidth, iwidth
    integer,optional,intent(in) :: nstt, def
    logical,optional,intent(in) :: grow
    integer,optional,intent(in) :: root, seed
    logical,optional,intent(in) :: test        ! experiment
    integer kh, sp
    integer jerr

    wh = new_wtable_core(name, mem, root, seed)
    jerr = min(0, wh)
    if (jerr.eq.0) then
       sp = choice(0, span)
       if (sp.le.0) sp = mem * 2
       kh = new_ktable(' ' // adjustl(name), sp, lkey, nkey, base, awidth, iwidth, test)
       jerr = min(0, kh)
    endif
    if (jerr.eq.0) call bind_ktable(jerr, wh, kh, nstt, def, grow)
    if (jerr.ne.0) wh = jerr
  end function new_wtable

!!!_  & new_wtable_core
  integer function new_wtable_core &
       & (name, lim, root, seed) &
       &  result(wh)
    implicit none
    character(len=*),intent(in) :: name
    integer,         intent(in) :: lim
    integer,optional,intent(in) :: root, seed

    integer jerr
    integer jw

    ! kill leading spaces of NAME
    call reg_item_core(jerr, wh, wsystem, flag_default, adjustl(name))
    if (jerr.eq.0) then
       jw = check_wtable(wh)
       jerr = min(0, jw)
    endif
    if (jerr.eq.0) then
       call decl_wtable(jerr, wmarks(jw), lim, root, seed)
    endif
    if (jerr.ne.0) wh = jerr
    return
  end function new_wtable_core

!!!_ + key-table management
!!!_  & new_ktable
  integer function new_ktable &
       & (name, span, lkey, nkey, base, awidth, iwidth, test) &
       & result(hk)
    implicit none
    ! either lkey or nkey must be postive
    character(len=*),intent(in) :: name
    integer,         intent(in) :: span        ! hash domain span
    integer,optional,intent(in) :: lkey        ! length of string key (default: 0)
    integer,optional,intent(in) :: nkey        ! size of integer-array key (default: 0)
    integer,optional,intent(in) :: base        ! hash root
    integer,optional,intent(in) :: awidth      ! string length for hash computation (default: lkey)
    integer,optional,intent(in) :: iwidth      ! array size for hash computation    (default: nkey)
    logical,optional,intent(in) :: test

    integer jk
    integer jerr

    call reg_item_core(jerr, hk, ksystem, flag_default, name)
    ! write(*, *) 'new_k', jerr, hk, name
    if (jerr.eq.0) then
       jk = ktable_h2index(hk)
       jerr = min(0, jk)
    endif
    if (jerr.eq.0) then
       call decl_ktable &
            & (jerr, ktable(jk), span, lkey, nkey, base, awidth, iwidth, test)
    endif
    if (jerr.lt.0) hk = jerr
  end function new_ktable

!!!_  & decl_ktable
  subroutine decl_ktable &
       & (ierr,  ktb,  &
       &  span,  lkey, nkey, base, awidth, iwidth, test)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    type(ktable_t),  intent(inout) :: ktb
    integer,         intent(in)    :: span
    integer,optional,intent(in)    :: lkey, nkey
    integer,optional,intent(in)    :: base, awidth, iwidth
    logical,optional,intent(in)    :: test

    integer mem
    integer b
    integer lk, nk, kwi, kwa
    integer u
    logical btest

    ierr = 0
    if (ktb%span.gt.0) then
       ! already allocated
       ierr = _ERROR(ERR_DUPLICATE_SET)
    endif
    if (ierr.eq.0) then
       lk = max(0, choice(0, lkey))
       nk = max(0, choice(0, nkey))
       if (lk.le.0.and.nk.le.0) then
          ! either key must be effective
          ierr = _ERROR(ERR_INVALID_PARAMETER)
       endif
       mem = span
    endif
    if (ierr.eq.0) then
       if (lk.gt.0) then
#      if HAVE_F2003_DEFERRED_TYPE
          u = 1
#      else /* not HAVE_F2003_DEFERRED_TYPE */
          u = max(1, (lk - 1) / kunit + 1)
          lk = kunit * u
#      endif /* not HAVE_F2003_DEFERRED_TYPE */
       else
          u = 0
       endif
#      if HAVE_F2003_DEFERRED_TYPE
          allocate(character(len=lk)::ktb%akey(kmin:mem-1), STAT=ierr)
#      else /* not  HAVE_F2003_DEFERRED_TYPE */
          allocate(ktb%akey(0:u-1, kmin:mem-1), STAT=ierr)
#      endif /* not  HAVE_F2003_DEFERRED_TYPE */
    endif
    if (ierr.eq.0) then
       allocate(ktb%ikey(0:nk-1, kmin:mem-1), STAT=ierr)
    endif
    if (ierr.eq.0) then
       b = choice(0, base)
       if (b.le.0) b = OPT_HASH_BASE
    endif
    if (ierr.eq.0) then
       if (nk.le.0) then
          kwi = -1
       else
          kwi = min(nk, max(0, choice(nk, iwidth)))
       endif
    endif
    if (ierr.eq.0) then
       if (lk.le.0) then
          kwa = -1
       else
          kwa = min(choice(0, awidth), lk)
          ! kwa < 0:  no hash (linear)
          ! kwa == 0: full width
          if (kwa.lt.0) then
             kwa = 0
          else if (kwa.eq.0) then
             kwa = lk
          endif
       endif
    endif
    if (ierr.eq.0) then
       if (OPT_HASH_RECORD_HISTORY .eq. 0) then
          ktb%nhist = -1
       else
          btest = choice((OPT_HASH_RECORD_HISTORY.gt.1), test)
          if (btest) then
             allocate(ktb%hist(0:mem-1), STAT=ierr)
             if (ierr.eq.0) ktb%nhist = 0
             if (ierr.eq.0) ktb%hist  = -1
          else
             ktb%nhist = -1
             ktb%hist => NULL()
          endif
       endif
    endif

    if (ierr.eq.0) then
       ktb%lkey   = lk
       ktb%nkey   = nk
       ktb%memk   = mem
       ktb%span   = span
       ktb%base   = b
       ktb%awidth = kwa
       ktb%iwidth = kwi
    endif
    if (ierr.eq.0) ktb%ikey = def_ikey
    if (ierr.eq.0) ktb%akey = ' '
    return
  end subroutine decl_ktable

!!!_  & bind_control - bind control on key-table
  subroutine bind_control_h(ierr, handle, nstt, def, grow)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: handle   ! key-table handle
    integer,optional,intent(in)  :: nstt
    integer,optional,intent(in)  :: def
    logical,optional,intent(in)  :: grow
    integer jk

    jk = check_ktable(handle)
    ierr = min(0, jk)
    if (ierr.eq.0) then
       call bind_control_k(ierr, ktable(jk), nstt, def, grow)
    endif
  end subroutine bind_control_h

  subroutine bind_control_k(ierr, ktb, nstt, def, grow)
    implicit none
    integer,         intent(out)   :: ierr
    type(ktable_t),  intent(inout) :: ktb
    integer,optional,intent(in)    :: nstt
    integer,optional,intent(in)    :: def
    logical,optional,intent(in)    :: grow
    integer jc

    ierr = 0
    if (ktb%jctrl.ge.0) then
       ! already allocated
       ierr = _ERROR(ERR_DUPLICATE_SET)
    endif
    if (ierr.eq.0) then
       jc = new_ctable(ktb%span, nstt, def, grow)
       ierr = min(0, jc)
    endif
    if (ierr.eq.0) ktb%jctrl = jc
  end subroutine bind_control_k

!!!_  & new_entry()
  integer function new_entry_h &
       & (handle, akey, ikey) &
       & result(ee)
    implicit none
    integer,         intent(in)          :: handle
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)
    integer jk
    jk = check_ktable(handle)
    ee = min(0, jk)
    if (ee.eq.0) ee = new_entry_k(ktable(jk), akey, ikey)
  end function new_entry_h

  integer function new_entry_k &
       & (ktb, akey, ikey) &
       & result(ee)
    implicit none
    type(ktable_t),  intent(inout)       :: ktb
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)
    integer jini, jerr

    if (check_key_args(ktb, akey, ikey)) then
       jini = hash_std(ktb, akey, ikey)
       ee = insert_entry(ktb%jctrl, jini)
       if (ee.ge.0) then
          jerr = store_keys(ktb, ee, akey, ikey)
          if (jerr.lt.0) ee = jerr
       endif
    else
       ee = _ERROR(ERR_INVALID_PARAMETER)
    endif
  end function new_entry_k

!!!_  & settle_entry()
  integer function settle_entry_h(handle, entr, status) result(ierr)
    implicit none
    integer,         intent(in) :: handle
    integer,         intent(in) :: entr
    integer,optional,intent(in) :: status(0:)
    integer jk

    jk = check_ktable(handle)
    ierr = min(0, jk)
    if (ierr.eq.0) ierr = settle_entry_k(ktable(jk), entr, status)
  end function settle_entry_h

  integer function settle_entry_k(ktb, entr, status) result(ierr)
    implicit none
    type(ktable_t),  intent(inout) :: ktb
    integer,         intent(in)    :: entr
    integer,optional,intent(in)    :: status(0:)
    ierr = fix_entry_j(ktb%jctrl, entr, status)
    if (OPT_HASH_RECORD_HISTORY.ne.0) then
       if (ierr.eq.0) then
          if (ktb%nhist.ge.0) then
             ktb%hist(ktb%nhist) = entr
             ktb%nhist = ktb%nhist + 1
          endif
       endif
    endif
  end function settle_entry_k

!!!_  & reg_entry() - register new entry (declare and settle at once)
  integer function reg_entry_h &
       & (handle, akey, ikey, status, err) &
       & result(ee)
    implicit none
    integer,         intent(in)          :: handle
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)
    integer,         intent(in),optional :: status(0:)
    logical,         intent(in),optional :: err          ! return non-positive if true
    integer jk

    jk = check_ktable(handle)
    ee = min(0, jk)
    if (ee.eq.0) ee = reg_entry_k(ktable(jk), akey, ikey, status, err)

  end function reg_entry_h
  integer function reg_entry_k &
       & (ktb, akey, ikey, status, err) &
       & result(ee)
    use TOUZA_Std_utl,only: choice
    implicit none
    type(ktable_t),  intent(inout)       :: ktb
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)
    integer,         intent(in),optional :: status(0:)
    logical,         intent(in),optional :: err          ! return non-positive if true
    integer jerr

    ee = new_entry_k(ktb, akey, ikey)
    if (ee.ge.0) then
       jerr = settle_entry_k(ktb, ee, status)
       if (jerr.ne.0) ee = jerr
    endif
    if (choice(.FALSE., err)) ee = min(0, ee)
  end function reg_entry_k

!!!_  & query_entry() - search entry existence
  integer function query_entry_h &
       & (handle, akey, ikey) &
       & result(ee)
!!!_   . note
    ! query_entry family returns:
    !    non-netative if found
    !    eundef(==-1) if not found
    !    else         if other errors
!!!_   . body
    implicit none
    integer,         intent(in)          :: handle
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)
    integer jk

    jk = check_ktable(handle)
    ee = min(0, jk)
    if (ee.eq.0) ee = query_entry_k(ktable(jk), akey, ikey)
  end function query_entry_h

  integer function query_entry_k &
       & (ktb, akey, ikey) &
       & result(ee)
    implicit none
    type(ktable_t),  intent(in)          :: ktb
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)

    if (check_key_args(ktb, akey, ikey)) then
       ee = query_entry_core(ktb, akey, ikey)
    else
       ee = _ERROR(ERR_INVALID_PARAMETER)
    endif
  end function query_entry_k

!!!_  & query_entry_core() - search entry existence
  integer function query_entry_core &
       & (ktb, akey, ikey) &
       & result(ee)
    implicit none
    type(ktable_t),  intent(in)          :: ktb
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)
    integer jorg, jstp
    integer jc

    jc = check_ctable(ktb%jctrl)
    ee = min(0, jc)
    if (ee.lt.0) return

    jorg = hash_std(ktb, akey, ikey)
    jstp = 0
    do
       ee = loop_entry_c(ctable(jc), jorg, jstp)
       if (ee.lt.0) exit
       if (collate_keys(ktb, ee, akey, ikey)) return
    enddo
    ee = eundef
  end function query_entry_core

!!!_  & query_key - get keys from entry
  subroutine query_key_h &
       & (ierr, handle, entr, akey, ikey)
    implicit none
    integer,         intent(out)            :: ierr
    integer,         intent(in)             :: handle
    integer,         intent(in)             :: entr
    character(len=*),intent(inout),optional :: akey
    integer,         intent(inout),optional :: ikey(0:)
    integer jk

    jk = check_ktable(handle)
    ierr = min(0, jk)
    if (ierr.eq.0) call query_key_k(ierr, ktable(jk), entr, akey, ikey)
  end subroutine query_key_h

  subroutine query_key_k &
       & (ierr, ktb, entr, akey, ikey)
    implicit none
    integer,         intent(out)            :: ierr
    type(ktable_t),  intent(in)             :: ktb
    integer,         intent(in)             :: entr
    character(len=*),intent(inout),optional :: akey
    integer,         intent(inout),optional :: ikey(0:)

    ierr = restore_keys(ktb, entr, akey, ikey)
  end subroutine query_key_k

!!!_  & query_status - query entry associates
  subroutine query_status_ah &
       & (ierr, status, handle, akey, ikey)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(inout)       :: status(0:)
    integer,         intent(in)          :: handle
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)
    integer jk

    jk = check_ktable(handle)
    ierr = min(0, jk)
    if (ierr.eq.0) call query_status_ak(ierr, status, ktable(jk), akey, ikey)
  end subroutine query_status_ah

  subroutine query_status_ak &
       & (ierr, status, ktb, akey, ikey)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(inout)       :: status(0:)
    type(ktable_t),  intent(in)          :: ktb
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)

    integer ee, jerr

    ierr = 0
    if (check_key_args(ktb, akey, ikey)) then
       ee = query_entry_core(ktb, akey, ikey)
    else
       ee = eundef
       ierr = _ERROR(ERR_INVALID_PARAMETER)
    endif
    call load_status(jerr, status, ktb%jctrl, ee, 0)
    if (jerr.ne.0) ierr = jerr
  end subroutine query_status_ak

  subroutine query_status_ih &
       & (ierr, status, handle, akey, ikey)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: status
    integer,         intent(in)          :: handle
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)
    integer s(1)
    call query_status_ah(ierr, s, handle, akey, ikey)
    status = s(1)
  end subroutine query_status_ih
  subroutine query_status_ik &
       & (ierr, status, ktb, akey, ikey)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: status
    type(ktable_t),  intent(in)          :: ktb
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)
    integer s(1)
    call query_status_ak(ierr, s, ktb, akey, ikey)
    status = s(1)
  end subroutine query_status_ik

!!!_  & query_status_entr - query entry associates
  subroutine query_status_entr_ah &
       & (ierr, status, handle, entr)
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: status(0:)
    integer,intent(in)    :: handle
    integer,intent(in)    :: entr
    integer jk

    ierr = 0
    jk = check_ktable(handle)
    ierr = min(0, jk)
    if (ierr.eq.0) call query_status_entr_ak(ierr, status, ktable(jk), entr)
  end subroutine query_status_entr_ah
  subroutine query_status_entr_ak &
       & (ierr, status, ktb, entr)
    implicit none
    integer,       intent(out)   :: ierr
    integer,       intent(inout) :: status(0:)
    type(ktable_t),intent(in)    :: ktb
    integer,       intent(in)    :: entr
    ierr = 0
    call load_status(ierr, status, ktb%jctrl, entr, 0)
  end subroutine query_status_entr_ak
  subroutine query_status_entr_ih &
       & (ierr, status, handle, entr)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: status
    integer,intent(in)  :: handle
    integer,intent(in)  :: entr
    integer s(1)
    call query_status_entr_ah(ierr, s, handle, entr)
    status = s(1)
  end subroutine query_status_entr_ih
  subroutine query_status_entr_ik &
       & (ierr, status, ktb, entr)
    implicit none
    integer,       intent(out) :: ierr
    integer,       intent(out) :: status
    type(ktable_t),intent(in)  :: ktb
    integer,       intent(in)  :: entr
    integer s(1)
    call query_status_entr_ak(ierr, s, ktb, entr)
    status = s(1)
  end subroutine query_status_entr_ik

!!!_  & store_keys()
  integer function store_keys(ktb, entr, akey, ikey) result(ierr)
    implicit none
    type(ktable_t),  intent(inout)       :: ktb
    integer,         intent(in)          :: entr
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)
    integer m
#if HAVE_F2003_DEFERRED_TYPE
#else
    character(len=ktb%lkey) :: bs
#endif

    ierr = min(0, entr)
    if (entr.ge.ktb%memk) then
       call grow_ktable(ierr, ktb, entr)
    endif
    if (ierr.eq.0) then
       if (ktb%iwidth.ge.0) then
          m = min(size(ikey), ktb%nkey)
          ktb%ikey(0:m-1, entr) = ikey(0:m-1)
       endif
       if (ktb%awidth.ge.0) then
#       if HAVE_F2003_DEFERRED_TYPE
          ktb%akey(entr) = trim(akey)
#       else
          bs = akey
          ktb%akey(:, entr) = transfer(bs, ktb%akey(:, entr))
#       endif
       endif
    endif
  end function store_keys

!!!_  - restore_keys()
  integer function restore_keys(ktb, entr, akey, ikey) result(ierr)
    implicit none
    type(ktable_t),  intent(in)           :: ktb
    integer,         intent(in)           :: entr
    character(len=*),intent(out),optional :: akey
    integer,         intent(out),optional :: ikey(0:)
    integer m
#if HAVE_F2003_DEFERRED_TYPE
#else
    character(len=ktb%lkey) :: bs
#endif

    ierr = 0
    if (entr.lt.0.or.entr.ge.ktb%memk) then
       ierr = _ERROR(ERR_INVALID_PARAMETER)
    endif
    if (ierr.eq.0) then
       if (present(ikey).and.ktb%iwidth.ge.0) then
          m = min(size(ikey), ktb%nkey)
          ikey(0:m-1) = ktb%ikey(0:m-1, entr)
       endif
       if (present(akey).and.ktb%awidth.ge.0) then
#       if HAVE_F2003_DEFERRED_TYPE
          akey = ktb%akey(entr)
#       else
          bs = transfer(ktb%akey(:, entr), bs)
          akey = trim(bs)
#       endif
       endif
    else
       ! set dummy values
       call set_dummy_keys(ikey, akey)
    endif
  end function restore_keys

!!!_  & collate_keys()
  logical function collate_keys(ktb, entr, akey, ikey) result(b)
    implicit none
    type(ktable_t),  intent(in)          :: ktb
    integer,         intent(in)          :: entr
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)
#if HAVE_F2003_DEFERRED_TYPE
#else
    character(len=ktb%lkey) :: bs
#endif
    integer m
    b = .TRUE.
    if (ktb%iwidth.ge.0) then
       m = ktb%nkey
       b = ALL(ktb%ikey(0:m-1, entr).eq.ikey(0:m-1))
    endif
    if (b.and.ktb%awidth.ge.0) then
#     if HAVE_F2003_DEFERRED_TYPE
       b = ktb%akey(entr).eq.trim(akey)
#     else
       bs = transfer(ktb%akey(:, entr), bs)
       b = bs.eq.trim(akey)
#     endif
    endif
  end function collate_keys

!!!_  & grow_ktable()
  subroutine grow_ktable (ierr, ktb, mem)
    implicit none
    integer,       intent(out)   :: ierr
    type(ktable_t),intent(inout) :: ktb
    integer,       intent(in)    :: mem

#if HAVE_F2003_DEFERRED_TYPE
    character(len=:),    pointer :: akey(:) => NULL()
#else
    character(len=kunit),pointer :: akey(:, :) => NULL()
#endif
    integer,             pointer :: ikey(:, :) => NULL()
    integer,             pointer :: hist(:) => NULL()
    integer ni, mn, mo

    ierr = 0
    mo = ktb%memk
    mn = ((mem - 1) / ktb%span + 1) * ktb%span

    ni = ktb%nkey
    if (ierr.eq.0) then
       if (mn.gt.mo) then
          allocate(ikey(0:ni-1, kmin:mn-1), STAT=ierr)
          if (ierr.eq.0) then
             ikey(:, kmin:mo-1) = ktb%ikey(:, kmin:mo-1)
             ikey(:, mo:mn-1)   = def_ikey
             deallocate(ktb%ikey, STAT=ierr)
          endif
          if (ierr.eq.0) ktb%ikey => ikey

          if (ierr.eq.0) then
#           if HAVE_F2003_DEFERRED_TYPE
             allocate(character(len=ktb%lkey)::akey(kmin:mn-1), STAT=ierr)
             if (ierr.eq.0) akey(kmin:mo-1) = ktb%akey(kmin:mo-1)
             if (ierr.eq.0) akey(mo:mn-1)   = ' '
#           else
             ni = size(ktb%akey, 1)
             allocate(akey(0:ni-1, kmin:mn-1), STAT=ierr)
             if (ierr.eq.0) akey(:, kmin:mo-1) = ktb%akey(:, kmin:mo-1)
             if (ierr.eq.0) akey(:, mo:mn-1)   = ' '
#           endif
          endif
          if (ierr.eq.0) deallocate(ktb%akey, STAT=ierr)
          if (ierr.eq.0) ktb%akey => akey

          if (ktb%nhist.ge.0) then
             if (ierr.eq.0) then
                allocate(hist(0:mn-1), STAT=ierr)
             endif
             if (ierr.eq.0) then
                hist(0:mo-1)  = ktb%hist(0:mo-1)
                hist(mo:mn-1) = -1
                deallocate(ktb%hist, STAT=ierr)
             endif
             if (ierr.eq.0) ktb%hist => hist
          endif

          if (ierr.eq.0) ktb%memk = mn
       endif
    endif
  end subroutine grow_ktable

!!!_  & check_key_args()
  logical function check_key_args(ktb, akey, ikey) result(b)
    implicit none
    type(ktable_t),  intent(in)          :: ktb
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)

    if (ktb%awidth.ge.0 .and. .not.present(akey)) then
       b = .FALSE.
       return
    else if (ktb%iwidth.ge.0) then
       if (.not.present(ikey)) then
          b = .FALSE.
          return
       else if (size(ikey).lt.ktb%nkey) then
          b = .FALSE.
          return
       endif
    endif
    b = .TRUE.
    return
  end function check_key_args

!!!_  & get_span()
  integer function get_span_h(handle, full) result(n)
    implicit none
    integer,intent(in)          :: handle
    logical,intent(in),optional :: full
    integer jk

    jk = check_ktable(handle)
    if (jk.ge.0) then
       n = get_span_k(ktable(jk), full)
    else
       n = jk
    endif
  end function get_span_h

  integer function get_span_k(ktb, full) result(n)
    use TOUZA_Std_utl,only: choice
    implicit none
    type(ktable_t),  intent(in) :: ktb
    logical,optional,intent(in) :: full

    if (choice(.FALSE., full)) then
       n = ktb%memk
    else
       n = ktb%span
    endif
  end function get_span_k

!!!_  & hash_std_k()
  integer function hash_std_k &
       & (ktb, akey, ikey) &
       & result(n)
    implicit none
    type(ktable_t),  intent(in)          :: ktb
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)

    integer,parameter :: maskh = (2 ** (BIT_SIZE(0) - 2)) - 1
    integer j, b, l, w, x

    b = ktb%base
    l = ktb%span

    x = 0
    do j = 0, ktb%iwidth - 1
       x = x * b + mod(ikey(j), l)
    enddo
    w = min(ktb%awidth, len_trim(akey))
    do j = w, 1, -1
       x = x * b + ICHAR(akey(j:j))
    enddo
    !! following code causes error on SX aurora
    !! due to unknown reason, seems to relate idiom.
    ! do j = htb%width, w + 1, -1
    !    x = x * b + ICHAR(' ')
    ! enddo
    n = MOD(IAND(x, maskh), l)

  end function hash_std_k

!!!_  & repr_ktable
  subroutine repr_ktable(ierr, txt, ktb)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: txt
    type(ktable_t),  intent(in)  :: ktb

    integer lk, ni
    character(len=32) :: ti

    ierr = 0
    txt = ' '
    if (ktb%awidth.ge.0) then
       lk = ktb%lkey
101    format('a=', I0, '/', I0)
102    format('a=', I0)
       if (ktb%awidth.lt.lk) then
          write(txt, 101, IOSTAT=ierr) ktb%awidth, lk
       else
          write(txt, 102, IOSTAT=ierr) lk
       endif
    endif
    if (ktb%iwidth.ge.0) then
       ni = ktb%nkey
201    format('i=', I0, '/', I0)
202    format('i=', I0)
       if (ktb%iwidth.lt.ni) then
          write(ti, 201) ktb%iwidth, ni
       else
          write(ti, 202) ni
       endif
       txt = trim(txt) // ' ' // trim(ti)
    endif
    txt = adjustl(txt)
  end subroutine repr_ktable
!!!_  & repr_key
  subroutine repr_key(ierr, txt, ktb, entr)
    use TOUZA_Std_utl,only: join_list
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: txt
    type(ktable_t),  intent(in)  :: ktb
    integer,         intent(in)  :: entr

    character(len=128) :: ki
    character(len=ktb%lkey) :: bs

    ierr = 0
    if (ktb%awidth.ge.0) then
#if HAVE_F2003_DEFERRED_TYPE
       bs = trim(ktb%akey(entr))
#else
       bs = transfer(ktb%akey(:, entr), bs)
#endif
       if (bs.eq.' ') bs = ''''''
    else
       bs = ' '
    endif
    if (ktb%iwidth.ge.0) then
       call join_list(ierr, ki, ktb%ikey(:, entr), ldelim='[', rdelim=']')
    else
       ki = ' '
    endif
    txt = trim(bs) // ' ' // trim(ki)
    txt = adjustl(txt)
  end subroutine repr_key
!!!_ + hash-table controls
!!!_  & new_ctable()
  integer function new_ctable &
       & (mem, nstt, def, grow) &
       & result(jctrl)
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,         intent(in) :: mem
    integer,optional,intent(in) :: nstt
    integer,optional,intent(in) :: def
    logical,optional,intent(in) :: grow
    integer jerr

    jctrl = nctrl
    nctrl = nctrl + 1
    ! write(*, *) 'new/c:', jctrl, lctrl
    if (jctrl.ge.lctrl) then
       jctrl = _ERROR(ERR_INSUFFICIENT_BUFFER)
       return
    endif
    call decl_ctable(jerr, ctable(jctrl), mem, nstt, def, grow)
    if (jerr.lt.0) jctrl = jerr
    ! write(*, *) 'new/c:end:', jctrl, lctrl
  end function new_ctable

!!!_  & decl_ctable
  subroutine decl_ctable &
       & (ierr, ctb,  &
       &  mem,  nstt, def,  grow)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    type(ctable_t),  intent(inout) :: ctb
    integer,         intent(in)    :: mem
    integer,optional,intent(in)    :: nstt   ! hash-table associates
    integer,optional,intent(in)    :: def
    logical,optional,intent(in)    :: grow
    integer ns

    ierr = 0
    if (ctb%uslot.gt.0) then
       ierr = _ERROR(ERR_DUPLICATE_SET)
    endif
    if (ierr.eq.0) then
       ctb%def = choice(unset, def)
       ctb%uslot = max(1, mem)
       if (choice(.false., grow)) then
          ctb%nslot = 1
       else
          ctb%nslot = 0
       endif
    endif
    if (ierr.eq.0) then
       ns = max(0, choice(1, nstt))
       allocate(ctb%stt(0:ns-1, kmin:mem - 1), &
            &   ctb%nxt(kmin:mem - 1), ctb%msh(kmin:mem - 1), &
            &   STAT=ierr)
    endif
    if (ierr.eq.0) then
       ctb%stt(:,:) = ctb%def
       ctb%nxt(:)   = unset
       ctb%msh(:)   = 0
    endif

    return
  end subroutine decl_ctable

!!!_  & insert_entry() - unconditinaly store new entry at initial hash number j0
  integer function insert_entry_j(jctrl, j0) result(ee)
    implicit none
    integer,intent(in) :: jctrl
    integer,intent(in) :: j0
    integer jc
    jc = check_ctable(jctrl)
    ee = min(0, jc)
    if (ee.eq.0) ee = insert_entry_c(ctable(jc), j0)
  end function insert_entry_j

  integer function insert_entry_c(ctb, j0) result(ee)
    implicit none
    type(ctable_t),intent(inout) :: ctb
    integer,       intent(in)    :: j0
    integer ocur, oprv

    ocur = j0
    do
       ee = search_vacant_entry(ctb, ocur)
       if (ee.ge.0) return
       oprv = ocur
       ocur = ctb%nxt(oprv)
       if (ocur.le.0) exit
    enddo
    ! grow-mode
    if (ctb%nslot.gt.0) then
       if (oprv / ctb%uslot .lt. ctb%nslot - 1) then
          ! seek final slot
          ocur = (ctb%nslot - 1) * ctb%uslot + j0
          ee = search_vacant_entry(ctb, ocur)
       endif
       if (ee.lt.0) then
          ! full slots
          call grow_ctable(ee, ctb)
          if (ee.eq.0) then
             ee = (ctb%nslot - 1) * ctb%uslot + j0
             ctb%stt(:, ee) = ctb%def
          endif
       endif
       if (ee.ge.0) then
          ctb%msh(ee)    = -ee
          ctb%nxt(oprv) = ee
          return
       endif
    endif
    return
  end function insert_entry_c
!!!_  & search_vacant_entry()
  integer function search_vacant_entry (ctb, j0) result(e)
    implicit none
    type(ctable_t),intent(inout) :: ctb
    integer,       intent(in)    :: j0

    integer j
    integer sbgn, soff

    sbgn = (j0 / ctb%uslot) * ctb%uslot
    soff = mod(j0, ctb%uslot)
    do j = 0, ctb%uslot - 1
       e = mod(soff + j, ctb%uslot) + sbgn
       if (.not.is_occupied(ctb, e)) then   ! [e] not occupied
          ctb%msh(e)    = - j0              ! record hash origin for later settlement
          ctb%stt(:, e) = ctb%def
          return
       endif
    enddo
    e = -1
  end function search_vacant_entry

!!!_  & fix_entry()
  integer function fix_entry_j(jctrl, entr, status) result(ierr)
    implicit none
    integer,         intent(in) :: jctrl
    integer,         intent(in) :: entr
    integer,optional,intent(in) :: status(0:)
    integer jc
    jc = check_ctable(jctrl)
    ierr = min(0, jc)
    if (ierr.eq.0) ierr = fix_entry_c(ctable(jc), entr, status)
  end function fix_entry_j

  integer function fix_entry_c(ctb, entr, status) result(ierr)
    implicit none
    type(ctable_t),  intent(inout) :: ctb
    integer,         intent(in)    :: entr
    integer,optional,intent(in)    :: status(0:)

    integer j0, step
    integer ns

    ierr = 0

    ! Already settled
    if (is_occupied(ctb, entr)) ierr = _ERROR(ERR_PANIC)

    if (ierr.eq.0) then
       j0 = - ctb%msh(entr)
       step = mod(entr - j0 + ctb%uslot, ctb%uslot)
       ctb%msh(j0) = max(ctb%msh(j0), step + 1)
       ctb%nxt(entr) = 0
       if (present(status)) then
          ns = min(size(status), size(ctb%stt, 1))
          ctb%stt(0:ns-1, entr) = status(0:ns-1)
       endif
    endif
  end function fix_entry_c

!!!_  & save_status
  subroutine save_status_j(ierr, jctrl, entr, status, k)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(in)    :: jctrl
    integer,         intent(in)    :: entr
    integer,         intent(in)    :: status(0:)
    integer,optional,intent(in)    :: k     ! status id
    integer jc
    jc = check_ctable(jctrl)
    ierr = min(0, jctrl)
    if (ierr.eq.0) then
       call save_status_c(ierr, ctable(jc), entr, status, k)
    endif
  end subroutine save_status_j

  subroutine save_status_c (ierr, ctb, entr, status, k)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    type(ctable_t),  intent(inout) :: ctb
    integer,         intent(in)    :: entr
    integer,         intent(in)    :: status(0:)
    integer,optional,intent(in)    :: k     ! status id
    integer jb, je

    ierr = 0
    if (ierr.eq.0) then
       jb = choice(0, k)
       je = min(size(ctb%stt, 1), jb + size(status))
       ctb%stt(jb:je-1, entr) = status(0:je-jb-1)
    endif
  end subroutine save_status_c

!!!_  & load_status
  subroutine load_status_j(ierr, status, jctrl, entr, k)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: status(0:)
    integer,         intent(in)    :: jctrl
    integer,         intent(in)    :: entr
    integer,optional,intent(in)    :: k     ! status id
    integer jc

    jc = check_ctable(jctrl)
    ierr = min(0, jctrl)
    if (ierr.eq.0) call load_status_c(ierr, status, ctable(jc), entr, k)
  end subroutine load_status_j

  subroutine load_status_c(ierr, status, ctb, entr, k)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: status(0:)
    type(ctable_t),  intent(in)    :: ctb
    integer,         intent(in)    :: entr
    integer,optional,intent(in)    :: k     ! status id
    integer jb, je
    integer e

    ierr = 0
    jb = choice(0, k)
    je = min(size(ctb%stt, 1), jb + size(status))
    e = entr
    if (e.lt.eundef.or.e.gt.ubound(ctb%stt, 2)) then
       e = eundef
       ierr = _ERROR(ERR_INVALID_PARAMETER)
    endif
    status(0:je-jb-1) = ctb%stt(jb:je-1, e)
  end subroutine load_status_c

!!!_  & loop_entry()
  integer function loop_entry_nocheck(jc, jorg, jstp) result(n)
    integer,intent(in)    :: jc
    integer,intent(inout) :: jorg
    integer,intent(inout) :: jstp
    n = loop_entry_c(ctable(jc), jorg, jstp)
  end function loop_entry_nocheck

  integer function loop_entry_j(jctrl, jorg, jstp) result(n)
    integer,intent(in)    :: jctrl
    integer,intent(inout) :: jorg
    integer,intent(inout) :: jstp
    ! no check
    integer jc
    jc = check_ctable(jctrl)
    n = min(0, jc)
    if (n.eq.0) n = loop_entry_c(ctable(jc), jorg, jstp)
  end function loop_entry_j

  integer function loop_entry_c(ctb, jorg, jstp) result(entr)
    implicit none
    type(ctable_t),intent(in)    :: ctb
    integer,       intent(inout) :: jorg
    integer,       intent(inout) :: jstp

    integer sorg, jini

    do
       entr = jstp
       jstp = jstp + 1
       if (jstp.le.ctb%msh(jorg)) exit
       jstp = 0
       jorg = ctb%nxt(jorg)
       if (jorg.le.0) then
          entr = eundef
          exit
       endif
    enddo
    if (entr.ge.0) then
       sorg = (jorg / ctb%uslot) * ctb%uslot
       jini = jorg - sorg
       entr = mod(jini + entr, ctb%uslot) + sorg
    endif
  end function loop_entry_c

!!!_  & grow_ctable
  subroutine grow_ctable (ierr, ctb)
    implicit none
    integer,       intent(out)   :: ierr
    type(ctable_t),intent(inout) :: ctb

    integer,pointer :: stt(:,:), nxt(:), msh(:)
    integer mo, mn
    integer ns

    if (ctb%nslot.le.0) then
       ierr = _ERROR(ERR_OPR_DISABLE)
       return
    endif
    ierr = 0

    mo = ctb%uslot * ctb%nslot
    ctb%nslot = ctb%nslot + 1
    mn = ctb%uslot * ctb%nslot
    ns = size(ctb%stt, 1)

    allocate(stt(0:ns-1, kmin:mn - 1), nxt(kmin:mn - 1), msh(kmin:mn - 1), &
         &   STAT=ierr)
    if (ierr.eq.0) then
       stt(:, kmin:mo-1) = ctb%stt(:, kmin:mo-1)
       nxt(kmin:mo-1)    = ctb%nxt(kmin:mo-1)
       msh(kmin:mo-1)    = ctb%msh(kmin:mo-1)
       stt(:, mo:) = ctb%def
       nxt(mo:) = unset
       msh(mo:) = -1
       deallocate(ctb%stt, ctb%nxt, ctb%msh, STAT=ierr)
    endif
    if (ierr.eq.0) then
       ctb%stt => stt
       ctb%nxt => nxt
       ctb%msh => msh
    endif
    return
  end subroutine grow_ctable

!!!_  & get_mems()
  integer function get_mems_j(jctrl) result(n)
    implicit none
    integer,intent(in) :: jctrl
    integer jc
    jc = check_ctable(jctrl)
    if (jc.ge.0) then
       n = get_mems_c(ctable(jc))
    else
       n = jc
    endif
  end function get_mems_j

  integer function get_mems_c(ctb) result(n)
    implicit none
    type(ctable_t),intent(in) :: ctb
    n = max(1, ctb%nslot) * ctb%uslot
  end function get_mems_c

!!!_  & is_occupied()
  logical function is_occupied_j(jctrl, entr) result(b)
    implicit none
    integer,intent(in) :: jctrl
    integer,intent(in) :: entr
    integer jc
    jc = check_ctable(jctrl)
    if (jc.ge.0) then
       b = is_occupied_c(ctable(jc), entr)
    else
       b = .FALSE.
    endif
  end function is_occupied_j

  ELEMENTAL &
       & logical function is_occupied_c(ctb, entr) result(b)
    implicit none
    type(ctable_t),intent(in) :: ctb
    integer,       intent(in) :: entr
    ! no range check
    b = ctb%nxt(entr).ge.0
  end function is_occupied_c

!!!_  & repr_ctable
  subroutine repr_ctable_j(ierr, txt, jctrl)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: txt
    integer,         intent(in)  :: jctrl
    integer jc
    jc = check_ctable(jctrl)
    if (jc.ge.0) then
       call repr_ctable_c(ierr, txt, ctable(jc))
    else
       ierr = jc
       txt = ' '
    endif
  end subroutine repr_ctable_j

  subroutine repr_ctable_c(ierr, txt, ctb)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: txt
    type(ctable_t),  intent(in)  :: ctb
    integer ns
    ierr = 0
    ns = size(ctb%stt, 1)
    if (ns.gt.0) then
101    format('<', A, '>*', I0)
102    format('<', I0, '>*', I0)
       if (ctb%def.eq.unset) then
          write(txt, 101) cunset, ns
       else
          write(txt, 102) ctb%def, ns
       endif
    else
       txt = '-'
    endif
  end subroutine repr_ctable_c

!!!_  & repr_ctl
  subroutine repr_ctl_j(ierr, txt, jctrl, entr)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: txt
    integer,         intent(in)  :: jctrl
    integer,         intent(in)  :: entr
    integer jc
    jc = check_ctable(jctrl)
    if (jc.ge.0) then
       call repr_ctl_c(ierr, txt, ctable(jc), entr)
    else
       ierr = jc
       txt  = ' '
    endif
  end subroutine repr_ctl_j

  subroutine repr_ctl_c(ierr, txt, ctb, entr)
    use TOUZA_Std_utl,only: join_list
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: txt
    type(ctable_t),  intent(in)  :: ctb
    integer,         intent(in)  :: entr
    integer ns
    character(len=32) :: prop
    ierr = 0
101 format('[', I0, '+', I0, ']')
    write(prop, 101) max(0, ctb%msh(entr)), ctb%nxt(entr)
    ! write(prop, 101) ctb%msh(entr), ctb%nxt(entr)
    ns = size(ctb%stt, 1)
    if (ns.gt.0) then
       call join_list &
            & (ierr, txt, ctb%stt(:, entr), ldelim='<', rdelim='>', &
            &  mask=ctb%stt(:, entr).eq.unset, skip=cunset)
    else
       txt = '-'
    endif
    txt = trim(prop) // ' ' // trim(txt)
  end subroutine repr_ctl_c
!!!_  & get_score()
  integer function get_score_j(jctrl, nml) result(n)
    implicit none
    integer,intent(in)          :: jctrl
    integer,intent(in),optional :: nml
    integer jc
    jc = check_ctable(jctrl)
    if (jc.ge.0) then
       n = get_score_c(ctable(jc), nml)
    else
       n = jc
    endif
  end function get_score_j
  integer function get_score_c(ctb, nml) result(n)
    use TOUZA_Std_utl,only: choice
    implicit none
    type(ctable_t),intent(in)          :: ctb
    integer,       intent(in),optional :: nml
    integer j, m, jj, s
    n = 0
    do j = 0, ctb%uslot - 1
       jj = j
       s = 0
       m = 0
       do
          m = m + max(0, ctb%msh(jj))
          jj = ctb%nxt(jj)
          if (jj.le.0) exit
          s = s + 1
       enddo
       ! if (m.gt.0) write(*, *) 'score', j, m, s
       n = max(n, m + ctb%uslot * s)
    enddo
    s = choice(0, nml)
    if (s.gt.ctb%uslot) then
       n = max(0, n - 1) * nml / max(1, ctb%uslot)
    endif
  end function get_score_c

!!!_ + hash-table watermarks
!!!_  & decl_wtable
  subroutine decl_wtable &
       & (ierr,   wtb,  &
       &  lim,    root,   seed,  eini)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,         intent(out)   :: ierr
    type(wtable_t),  intent(inout) :: wtb
    integer,         intent(in)    :: lim
    integer,optional,intent(in)    :: root, seed
    integer,optional,intent(in)    :: eini   !! initial entry index

    integer r, s, ini

    ierr = 0
    ini = choice(0, eini)
    if (ierr.eq.0) then
       wtb%l = max(lim, ini)
       wtb%n = ini
       r = choice(0, root)
       if (r.le.0) r = OPT_WATERMARK_ROOT
       wtb%root = r
       s = choice(-1, seed)
       if (s.lt.0) then
          s = min(max(1, mod(wseed, r)), r - 1)
       endif
       s = mod(s, r)

       wtb%seed = s
       wseed = s + 1            ! update next seed
       wtb%kh = -1  ! to bind

       allocate(wtb%entr(0:wtb%l-1), STAT=ierr)
       if (ierr.eq.0) wtb%entr(:) = eundef
    endif
    return
  end subroutine decl_wtable

!!!_  & bind_ktable - bind key-table on watermark table
  subroutine bind_ktable &
       & (ierr, wh, kh, nstt, def, grow)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: wh   ! watermark-table handle
    integer,         intent(in)  :: kh   ! key-table handle
    integer,optional,intent(in)  :: nstt
    integer,optional,intent(in)  :: def
    logical,optional,intent(in)  :: grow
    integer jw

    jw = check_wtable(wh)
    ierr = min(0, jw)
    if (ierr.eq.0) then
       call bind_ktable_core(ierr, wmarks(jw), kh, nstt, def, grow)
    endif
  end subroutine bind_ktable
!!!_  & bind_ktable_core - bind key-table on watermark table
  subroutine bind_ktable_core &
       & (ierr, wtb, kh, nstt, def, grow)
    implicit none
    integer,         intent(out)   :: ierr
    type(wtable_t),  intent(inout) :: wtb
    integer,         intent(in)    :: kh   ! key-table handle
    integer,optional,intent(in)    :: nstt
    integer,optional,intent(in)    :: def
    logical,optional,intent(in)    :: grow

    ierr = 0
    wtb%kh = kh
    if (ierr.eq.0) call bind_control(ierr, wtb%kh, nstt, def, grow)
  end subroutine bind_ktable_core

!!!_  & reg_item
  subroutine reg_item &
       & (ierr, handle, wh, flag, akey, ikey)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: handle
    integer,         intent(in)          :: wh
    integer,         intent(in)          :: flag
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)
    integer jw

    jw = check_wtable(wh)
    ierr = min(0, jw)
    if (ierr.eq.0) then
       call reg_item_core(ierr, handle, wmarks(jw), flag, akey, ikey)
    else
       handle = ierr
    endif

  end subroutine reg_item
!!!_  & reg_item_core
  subroutine reg_item_core &
       & (ierr, handle, wtb, flag, akey, ikey)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: handle
    type(wtable_t),  intent(inout)       :: wtb
    integer,         intent(in)          :: flag
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)

    integer e

    ierr = 0
    if (ierr.eq.0) call query_status(ierr, handle, wtb%kh, akey, ikey)
    ! write(*, *) 'reg_item', ierr, wtb%kh, handle, akey
    if (ierr.eq.0) then
       if (handle.ge.0) then
          if (AND(flag, flag_ignore).eq.0) ierr = _ERROR(ERR_DUPLICATE_SET)
          return
       endif
       e = new_entry(wtb%kh, akey, ikey)
       ierr = min(0, e)
    endif
    if (ierr.eq.0) then
       call settle_item_core(ierr, handle, wtb, e)
    else
       handle = ierr
    endif
    return
  end subroutine reg_item_core

!!!_  & search_item
  subroutine search_item &
       & (ierr, handle, wh, akey, ikey)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: handle
    integer,         intent(in)          :: wh
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)
    integer jw

    jw = check_wtable(wh)
    ierr = min(0, jw)
    if (ierr.eq.0) then
       call search_item_core(ierr, handle, wmarks(jw), akey, ikey)
    else
       handle = ierr
    endif
  end subroutine search_item
!!!_  & search_item_core
  subroutine search_item_core &
       & (ierr, handle, wtb, akey, ikey)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: handle
    type(wtable_t),  intent(in)          :: wtb
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)

    ierr = 0
    if (ierr.eq.0) then
       call query_status(ierr, handle, wtb%kh, akey, ikey)
    else
       handle = ierr
    endif
    return
  end subroutine search_item_core

!!!_  & settle_item_core
  subroutine settle_item_core &
       & (ierr, handle, wtb, entr)
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: handle
    type(wtable_t),  intent(inout) :: wtb
    integer,         intent(in)    :: entr
    integer ji, eh
    integer,pointer :: enew(:)

    ierr = 0

    ji = wtb%n
    wtb%n = wtb%n + 1
    if (wtb%n.gt.size(wtb%entr)) then
       eh = (ji / wtb%l + 1) * wtb%l
       allocate(enew(0:eh-1), STAT=ierr)
       if (ierr.eq.0) then
          enew(0:ji-1) = wtb%entr(0:ji-1)
          deallocate(wtb%entr, STAT=ierr)
          if (ierr.eq.0) wtb%entr => enew
       endif
    endif
    handle = watermark(ji, wtb)
    if (ierr.eq.0) then
       wtb%entr(ji) = entr
       ierr = settle_entry(wtb%kh, entr, (/handle/))
    endif

  end subroutine settle_item_core

!!!_  & reg_alias
  subroutine reg_alias &
       & (ierr, handle, wh, flag, akey, ikey)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: handle
    integer,         intent(in)          :: wh
    integer,         intent(in)          :: flag
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)
    integer jw

    jw = check_wtable(wh)
    ierr = min(0, jw)
    if (ierr.eq.0) then
       call reg_alias_core(ierr, handle, wmarks(jw), flag, akey, ikey)
    endif
  end subroutine reg_alias
!!!_  & reg_alias_core
  subroutine reg_alias_core &
       & (ierr, handle, wtb, flag, akey, ikey)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: handle
    type(wtable_t),  intent(inout)       :: wtb
    integer,         intent(in)          :: flag
    character(len=*),intent(in),optional :: akey
    integer,         intent(in),optional :: ikey(0:)
    integer h, e

    ierr = 0
    if (ierr.eq.0) call query_status(ierr, h, wtb%kh, akey, ikey)
    if (ierr.eq.0) then
       if (h.ge.0) then
          if (AND(flag, flag_ignore).eq.0) ierr = _ERROR(ERR_DUPLICATE_SET)
          return
       endif
       e = new_entry(wtb%kh, akey, ikey)
       ierr = min(0, e)
    endif
    if (ierr.eq.0) ierr = settle_entry(wtb%kh, e, (/handle/))
    return
  end subroutine reg_alias_core

!!!_  & get_keys
  subroutine get_keys(ierr, handle, wh, akey, ikey)
    implicit none
    integer,         intent(out)          :: ierr
    integer,         intent(in)           :: handle
    integer,         intent(in)           :: wh
    character(len=*),intent(out),optional :: akey
    integer,         intent(out),optional :: ikey(0:)
    integer jw
    ierr = 0
    jw = check_wtable(wh)
    ierr = min(0, jw)
    if (ierr.eq.0) then
       call get_keys_core(ierr, handle, wmarks(jw), akey, ikey)
    else
       ! set dummy values
       call set_dummy_keys(ikey, akey)
    endif
  end subroutine get_keys
!!!_  & get_keys_core
  subroutine get_keys_core &
       & (ierr, handle, wtb, akey, ikey)
    implicit none
    integer,         intent(out)          :: ierr
    integer,         intent(in)           :: handle
    type(wtable_t),  intent(in)           :: wtb
    character(len=*),intent(out),optional :: akey
    integer,         intent(out),optional :: ikey(0:)
    integer e

    e = check_handle_core(handle, wtb)
    ierr = min(0, e)
    if (ierr.eq.0) then
       e = wtb%entr(e)
       call query_key(ierr, wtb%kh, e, akey, ikey)
    else
       ! set dummy values
       call set_dummy_keys(ikey, akey)
    endif
  end subroutine get_keys_core

!!!_  & get_size_items
  integer function get_size_items(wh) result(n)
    implicit none
    integer,intent(in) :: wh
    n = check_wtable(wh)
    if (n.ge.0) n = wmarks(n)%n
  end function get_size_items

!!!_  & watermark
  PURE integer function watermark(id, wt) result(h)
    implicit none
    integer,       intent(in) :: id
    type(wtable_t),intent(in) :: wt
    if (id.lt.0) then
       h = id
    else
       h = id * wt%root + wt%seed
    endif
  end function watermark
!!!_  & normalize()
  PURE integer function normalize(handle, wt) result(n)
    implicit none
    integer,       intent(in) :: handle
    type(wtable_t),intent(in) :: wt
    if (handle.lt.0) then
       n = handle
    else if (mod(handle, wt%root).ne.wt%seed) then
       n = _ERROR(ERR_INVALID_ITEM)
    else
       n = handle / wt%root
    endif
  end function normalize

!!!_ + handle and index managements
!!!_  & check_handle()
  PURE integer function check_handle(handle, wh) result(k)
    implicit none
    integer,intent(in) :: handle
    integer,intent(in) :: wh
    k = check_wtable(wh)
    if (k.ge.0) then
       k = check_handle_core(handle, wmarks(k))
    endif
  end function check_handle
!!!_  & check_handle_core()
  PURE integer function check_handle_core(handle, wt) result(k)
    implicit none
    integer,       intent(in) :: handle
    type(wtable_t),intent(in) :: wt
    k = normalize(handle, wt)
    if (k.lt.0 .or. k.ge.min(wt%n, wt%l)) then
       k = _ERROR(ERR_OUT_OF_RANGE)
    endif
  end function check_handle_core
!!!_  & check_index()
  PURE integer function check_index(idx, wh) result(k)
    implicit none
    integer,intent(in) :: idx
    integer,intent(in) :: wh
    k = check_wtable(wh)
    if (k.ge.0) then
       k = check_index_core(idx, wmarks(k))
    endif
  end function check_index
!!!_  & check_index_core()
  PURE integer function check_index_core(idx, wt) result(k)
    implicit none
    integer,       intent(in) :: idx
    type(wtable_t),intent(in) :: wt
    k = idx
    if (k.lt.0 .or. k.ge.min(wt%n, wt%l)) then
       k = _ERROR(ERR_OUT_OF_RANGE)
    endif
    k = watermark(k, wt)
  end function check_index_core

!!!_  & check_ctable() - check if valid ctable and return index
  PURE integer function check_ctable(handle) result(j)
    implicit none
    integer,intent(in) :: handle
    j = handle
    if (j.lt.0.or.j.ge.min(nctrl, lctrl)) j = _ERROR(ERR_OUT_OF_RANGE)
  end function check_ctable
!!!_  & check_ktable() - check if valid ktable and return index
  PURE &
  integer function check_ktable(handle) result(j)
    implicit none
    integer,intent(in) :: handle
    j = ktable_h2index(handle)
    ! write(*, *) 'check', j, handle, ksystem%n, ksystem%l
    if (j.ge.min(ksystem%n, ksystem%l)) j = _ERROR(ERR_OUT_OF_RANGE)
  end function check_ktable
!!!_  & check_wtable() - check if valid wtable and return index
  PURE integer function check_wtable (handle) result(j)
    implicit none
    integer,intent(in) :: handle
    j = wtable_h2index(handle)
    if (j.ge.min(wsystem%n, wsystem%l)) j = _ERROR(ERR_OUT_OF_RANGE)
  end function check_wtable

!!!_  & wtable_h2index()
  PURE integer function wtable_h2index(handle) result(j)
    implicit none
    integer,intent(in) :: handle
    j = normalize(handle, wsystem)
  end function wtable_h2index
!!!_  & wtable_j2handle() - identity mapping
  PURE integer function wtable_j2handle(idx) result(h)
    implicit none
    integer,intent(in) :: idx
    h = watermark(idx, wsystem)
  end function wtable_j2handle

!!!_  & ktable_h2index()
  PURE integer function ktable_h2index(handle) result(j)
    implicit none
    integer,intent(in) :: handle
    j = normalize(handle, ksystem)
  end function ktable_h2index
!!!_  & ktable_j2handle() - identity mapping
  PURE integer function ktable_j2handle(idx) result(h)
    implicit none
    integer,intent(in) :: idx
    h = watermark(idx, ksystem)
  end function ktable_j2handle

!!!_  - set_dummy_keys
  subroutine set_dummy_keys (ikey, akey)
    implicit none
    character(len=*),intent(out),optional :: akey
    integer,         intent(out),optional :: ikey(0:)
    if (present(akey)) then
       akey = dummy_akey
    endif
    if (present(ikey)) then
       ikey(:) = dummy_ikey
    endif
  end subroutine set_dummy_keys

!!!_ + end TOUZA_Std_htb
end module TOUZA_Std_htb

!!!_@ test_std_htb - test program
#if TEST_STD_HTB
program test_std_htb
  use TOUZA_Std_htb
  implicit none
  integer ierr

101 format(A, ' = ', I0)
  call init(ierr)
  write(*, 101) 'init', ierr

  call test_bare_batch(ierr)
  call test_grow_batch(ierr)
  call test_tags_batch(ierr)
  call test_wm_batch(ierr)

  call diag(ierr, levv=+99)
  ! if (ierr.eq.0) call diag(ierr, levv=+99)
  write(*, 101) 'diag', ierr
  if (ierr.eq.0) call finalize(ierr, levv=+10)
  write(*, 101) 'fine', ierr
  stop

contains
  subroutine test_bare_batch(ierr)
    implicit none
    integer,intent(out) :: ierr

    integer ca, cb, cc, ce, cf, cg
    integer e0, e1, e2, e3

    integer span, lkey
    ierr = 0

    span = 16
    lkey = 12
    ca  = new_htable('test-a', span, lkey, awidth=6)
    cb  = new_htable('test-b', span, lkey)
    cc  = new_htable('test-c', span, lkey)
    ce  = new_htable('test-e', span, lkey, awidth=-1)
    cf  = new_htable('test-f', span, lkey, awidth=-2)
    cg  = new_htable('test-gggggggG', span, lkey)

    if (ierr.eq.0) then
       e0  = new_entry(ca, 'item-0')
       ierr = settle_entry(ca, e0)
       e1  = reg_entry(ca, 'item-1', status=(/9/))
    endif
    if (ierr.eq.0) then
       e0  = new_entry(ca, 'item-0')
       ierr = settle_entry(ca, e0)

       e2  = new_entry(ca, 'item-2')
       e2  = new_entry(ca, 'item-2')
       ierr = settle_entry(ca, e2)

       e3  = new_entry(ca, 'item-3')
       ! no settlement
       e3  = new_entry(ca, 'item-3x')
       ierr = settle_entry(ca, e3)
       e3  = new_entry(ca, 'item-3')
       ierr = settle_entry(ca, e3)
    endif
  end subroutine test_bare_batch

  subroutine test_tags_batch(ierr)
    implicit none
    integer,intent(out) :: ierr
    integer c11, c22, c33
    integer c10, c21, c32

    integer span, lkey

    ierr = 0

    span = 16
    lkey = 12

    c11 = new_htable('tag-1', span, lkey, nkey=1)
    c22 = new_htable('tag-2', span, lkey, nkey=2)
    c33 = new_htable('tag-3', span, lkey, nkey=3)

    c10 = new_htable('tag-1/0', span, lkey, nkey=1, iwidth=0)
    c21 = new_htable('tag-2/1', span, lkey, nkey=2, iwidth=0)
    c32 = new_htable('tag-3/2', span, lkey, nkey=3, iwidth=0)

    call test_tags(ierr, c11)
    call test_tags(ierr, c22)
    call test_tags(ierr, c33)
    call test_tags(ierr, c10)
    call test_tags(ierr, c21)
    call test_tags(ierr, c32)

  end subroutine test_tags_batch

  subroutine test_tags(ierr, hh)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hh
    integer e

    ierr = 0

    e = reg_entry(hh, 'A', (/0, 0, 0/), (/0/))
    e = reg_entry(hh, 'A', (/1, 0, 0/), (/1/))
    e = reg_entry(hh, 'A', (/2, 0, 0/), (/2/))
    e = reg_entry(hh, 'A', (/0, 1, 0/), (/3/))
    e = reg_entry(hh, 'A', (/1, 1, 0/), (/4/))
    e = reg_entry(hh, 'A', (/2, 1, 0/), (/5/))
    e = reg_entry(hh, 'A', (/0, 1, 1/), (/6/))
    e = reg_entry(hh, 'A', (/1, 1, 1/), (/7/))
    e = reg_entry(hh, 'A', (/2, 1, 1/), (/8/))

  end subroutine test_tags

  subroutine test_grow_batch(ierr)
    integer,intent(out) :: ierr
    integer cd, ch, ci
    integer lkey, span

    ierr = 0

    span = 4
    lkey = 12

    cd  = new_htable('test-d', lkey, span, awidth=0)
    ch  = new_htable('test-h', lkey, span)
    ci  = new_htable('test-i', lkey, span, grow=.TRUE.)

    ! write(*, *) 'grow:', cd, ch, ci

    if (ierr.eq.0) call test_grow(ierr, cd)
    if (ierr.eq.0) call test_grow(ierr, ch)
    if (ierr.eq.0) call test_grow(ierr, ci)

  end subroutine test_grow_batch

  subroutine test_grow(ierr, hh)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hh
    integer ji, jo
    integer li, lo
    character(len=16) :: b
    integer e
    integer stt, s(1)

101 format('x', I1, '-', I1)

    ierr = 0
    li = 4
    lo = 3

    do jo = 0, lo
       do ji = 0, li
          write(b, 101) ji, jo
          e = new_entry(hh, b)
          stt = test_status(ji, jo, li, lo)
          if (e.ge.0) ierr = settle_entry(hh, e, (/stt/))
          ! call diag_ktable(ierr, hh)
       enddo
       ! 201       format('grow:', I0, 1x, I0, 1x, I0, 1x, A)
       ! write(*, 201) hh, ierr, e, trim(b)
    enddo
    ierr = 0
102 format('query:', I0, '[', A, '] = ', I0, 1x, I0, 1x, I0)
    do jo = 0, lo + 1
       do ji = 0, li + 1
          write(b, 101) ji, jo
          e = query_entry(hh, b)
          call query_status_entr(ierr, s, hh, e)
          ! write(*, 102) hh, trim(b), e, s
          stt = test_status(ji, jo, li, lo)
          if (stt.ne.s(1) .and.stt.ge.0) then
             ! write(*, *) 'query[', trim(b), '] = ', e, s, stt
             write(*, 102) hh, trim(b), e, s, stt
          endif
       enddo
    enddo
  end subroutine test_grow
  integer function test_status(ji, jo, li, lo) result(k)
    implicit none
    integer,intent(in):: ji, jo, li, lo
    k = jo * 100 + ji
    if (jo.gt.lo .or. ji.gt.li) k = -1
  end function test_status

  subroutine test_wm_batch(ierr)
    implicit none
    integer,intent(out) :: ierr

    integer wa, wb, wc

    ierr = 0
    wa = new_wtable('WM-a', mem=8, lkey=8)
    wb = new_wtable('WM-b', mem=8, lkey=8)
    wc = new_wtable('WM-c', mem=8, lkey=8)

    call test_wm(ierr, wa)
    call test_wm(ierr, wb)
    call test_wm(ierr, wc)

  end subroutine test_wm_batch

  subroutine test_wm(ierr, wh)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: wh
    character(len=16) :: buf
    integer j, h, h0, jh
    integer nreg

    ierr = 0

101 format('key-', I0)
102 format('alias-', I0)
111 format('reg_item:', I0, 2x, I0, 1x, I0)
112 format('search_item:', I0, 2x, I0, 1x, I0)
113 format('check_handle:', I0, 2x, I0, 1x, I0)
114 format('reg_alias:', I0, 2x, I0, 1x, I0)

    nreg = 5

    do j = 0, nreg - 1
       write(buf, 101) j
       call reg_item(ierr, h, wh, flag_default, buf)
       write(*, 111) ierr, j, h
    enddo

    j = min(2, nreg - 1)
    write(buf, 101) j
    call search_item(ierr, h, wh, buf)
    j = nreg + 1
    write(buf, 102) j
    call reg_alias(ierr, h, wh, flag_default, buf)
    write(*, 114) ierr, h

    do j = 0, nreg - 1
       write(buf, 101) j
       call search_item(ierr, h, wh, buf)
       write(*, 112) ierr, j, h
       if (ierr.ne.0) return
    enddo
    do j = nreg, nreg + 3
       write(buf, 101) j
       call search_item(ierr, h, wh, buf)
       write(*, 112) ierr, j, h
       ierr = 0
    enddo

    j = min(3, nreg - 1)
    write(buf, 101) j
    call search_item(ierr, h0, wh, buf)
    do j = -2, 2
       h = h0 + j
       jh = check_handle(h, wh)
       write(*, 113) jh, h, h0
    enddo

  end subroutine test_wm

end program test_std_htb

#endif /* TEST_STD_HTB */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

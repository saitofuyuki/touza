!!!_! std_htb.F90 - touza/std simple hash table manager
! Maintainer: SAITO Fuyuki
! Created: Jan 28 2022
#define TIME_STAMP 'Time-stamp: <2022/12/12 22:09:07 fuyuki std_htb.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022
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
!!!_* switch
!!!_* Notes
!      - a hash VALUE is computed against a combination of a string (KEY)
!        and optional integer array (TAG) of size NTAG.
!      - on the other hand, a hash NUMBER is computed against a string
!        and integer array of size KTAG, where 0 <= KTAG <= NTAG.
!      - for example, when KTAG == 0, a pair of the same KEY and different
!        TAG shares the same hash NUMBER, thus certain operation might be
!        easy, like to search all the hash VALUE with the same KEY through
!        the table
!!!_@ TOUZA_Std_htb - simple hash table
module TOUZA_Std_htb
  use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
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

  integer,save :: err_default = ERR_NO_INIT - ERR_MASK_STD_HTB
!!!_  - parameters
  integer,parameter,public :: unset = (- HUGE(0)) - 1

  integer,parameter,public :: flag_default = 0
  integer,parameter,public :: flag_ignore  = 1  ! ignore duplicate
!!!_  - private parameters
  integer,parameter :: kmin = -1
  integer,parameter :: hh_sys = 0
!!!_  - tables
  integer,parameter :: lname = OPT_HASH_NAME_LENGTH
  integer,parameter :: kunit = OPT_HASH_KEY_LENGTH
!!!_  - hash table primitive
  type htable_t
     integer                       :: mem = 0
     integer                       :: kmdl      ! module
     integer,             _POINTER :: nxt(:)    ! starting index at next slot
     integer,             _POINTER :: stt(:, :) ! status complex   (0:lstt-1, -1:mem-1)
     character(len=kunit),_POINTER :: kbuf(:)
     integer,             _POINTER :: mofs(:)
     integer                       :: width = 0
     integer                       :: base = OPT_HASH_BASE
     integer                       :: def = unset
     integer                       :: nslot = 0
     integer                       :: lstt = 0  ! size of status complex
     integer                       :: ntag = 0  ! integer tag size to store in status  (stt(0:ntag-1,:))
     integer                       :: ktag = 0  ! number of tag to use in hash computation
  end type htable_t
  type(htable_t),allocatable,save :: htable(:)
  integer,save :: ntable = 0, ltable = 0
  ! note:  nxt < 0  not set, or offset to origin to settle
  !        nxt ==0  no next slot for this hash
  !        nxt >0   next slot starting index
!!!_  - watermarked hash-table shell
  type wtable_t
     type(htable_t)   :: ht
     integer          :: root = 0   ! watermark parameters
     integer          :: seed = 0   !
     integer          :: n    = 0   ! automatic element current size
     integer          :: l    = 0   ! automatic element limit size
     integer,_POINTER :: entr(:)  ! corresponding hash entry
  end type wtable_t

  type(wtable_t),allocatable,save :: wmarks(:)

  type(wtable_t),save :: wsystem
  integer,save        :: wseed = 0
!!!_  - interfaces
  interface new_entry
     module procedure new_entry_bare, new_entry_stag, new_entry_atag, new_entry_func
  end interface new_entry
  interface reg_entry
     module procedure reg_entry_bare, reg_entry_stag, reg_entry_atag, reg_entry_func
  end interface reg_entry

  interface store_xstatus
     module procedure store_xstatus_scl, store_xstatus_arr
  end interface store_xstatus
  interface restore_xstatus
     module procedure restore_xstatus_scl, restore_xstatus_arr
  end interface restore_xstatus

  interface query_entry
     module procedure query_entry_bare, query_entry_stag, query_entry_atag, query_entry_func
  end interface query_entry
  interface query_status
     module procedure query_status_bare, query_status_stag, query_status_atag, query_status_func
     module procedure query_status_entr
  end interface query_status

  interface reg_item
     module procedure reg_item_bare, reg_item_stag, reg_item_atag
  end interface reg_item
  interface reg_alias
     module procedure reg_alias_bare, reg_alias_stag, reg_alias_atag
  end interface reg_alias
  interface search_item
     module procedure search_item_bare, search_item_stag, search_item_atag
  end interface search_item

  interface diag_htable
     module procedure diag_htable_t, diag_htable_h
  end interface diag_htable
  interface diag_wtable
     module procedure diag_wtable_t, diag_wtable_h
  end interface diag_wtable

  interface save_status
     module procedure save_status_scl_t, save_status_arr_t
  end interface save_status
  interface load_status
     module procedure load_status_scl_t, load_status_arr_t
  end interface load_status
  interface hash_core
     module procedure hash_core_scl, hash_core_arr
  end interface hash_core

!!!_  - public
  public init,         diag,         finalize
  public new_htable,   diag_htable
  public new_entry,    reg_entry,    settle_entry
  public query_entry,  query_status, query_name
  public store_xstatus,restore_xstatus
  public new_wtable,   diag_wtable
  public reg_item,     reg_alias,    search_item
  public check_handle, check_index,  get_size_items
  public normalize,    watermark
  public get_item_keys
  ! public store_props, restore_props
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, numh, numw)
    use TOUZA_Std_utl,only: utl_init=>init, choice
    use TOUZA_Std_log,only: log_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u      ! log unit
    integer,intent(in),optional :: levv   ! verbose level
    integer,intent(in),optional :: mode   ! initialization flag
    integer,intent(in),optional :: numh   ! hash-table size
    integer,intent(in),optional :: numw   ! watermarked hash-table size

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
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call utl_init(ierr, ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call log_init(ierr, ulog, levv=lv, mode=lmd)
       endif
       if (is_first_force(init_counts, mode)) then
          if (ierr.eq.0) call init_alloc_htable(ierr, numh)
          if (ierr.eq.0) call init_alloc_wtable(ierr, numw)
          if (ierr.eq.0) call init_system_htable(ierr)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT - ERR_MASK_STD_ENV
    endif

    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std_utl, only: utl_diag=>diag, choice
    use TOUZA_Std_log, only: log_diag=>diag, msg_mdl
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
                call diag_wtable(ierr, wsystem, '(wsystem)', u=utmp)
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
       lmd = control_deep(md)
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
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call utl_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call log_finalize(ierr, utmp, lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + init subcontracts
!!!_  & init_alloc_htable
  subroutine init_alloc_htable &
       & (ierr, mtable)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: mtable
    integer m
    ierr = 0
    m = choice(0, mtable)
    if (m.le.0) m = max(2, OPT_HASH_CATEGORIES)  ! 0:system 1:default
    ltable = m
    if (allocated(htable)) then
       ierr = ERR_ALLOCATION - ERR_MASK_STD_HTB
       call msg_mdl('hash-table cannot allocate.', __MDL__)
    endif
    if (ierr.eq.0) then
       allocate(htable(0:ltable - 1), STAT=ierr)
       ntable = hh_sys + 1
    endif
    return
  end subroutine init_alloc_htable

!!!_  & init_alloc_wtable
  subroutine init_alloc_wtable &
       & (ierr, mtable)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: mtable
    integer m
    ierr = 0
    m = choice(0, mtable)
    if (m.le.0) m = max(2, OPT_HASH_CATEGORIES)  ! 0:system 1:default
    if (allocated(wmarks)) then
       ierr = ERR_ALLOCATION - ERR_MASK_STD_HTB
       call msg_mdl('watermarked hash-table cannot allocate.', __MDL__)
    endif
    if (ierr.eq.0) then
       allocate(wmarks(0:m - 1), STAT=ierr)
    endif
    if (ierr.eq.0) then
       wseed = 1   !  hard-coded seed
       call alloc_wtable &
            & (ierr, wsystem, lname, ltable=m, seed=wseed)
    endif
    return
  end subroutine init_alloc_wtable
!!!_  & init_system_htable
  subroutine init_system_htable &
       & (ierr, hsize)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: hsize
    integer m

    ierr = 0
    m = max(ltable, choice(ltable * 2, hsize))
    if (ierr.eq.0) call alloc_htable(ierr, htable(hh_sys), lname, lbuf=m)
    ! if (ierr.eq.0) htable(hh_sys)%name = '(sys)'
    if (ierr.eq.0) htable(hh_sys)%nxt(hh_sys) = 0
    ! if (ierr.eq.0) htable(hh_sys)%stt(hh_sys) = hh_sys
    if (ierr.eq.0) call save_status(ierr, htable(hh_sys), hh_sys, hh_sys)
    return
  end subroutine init_system_htable

!!!_ + diag subcontracts
!!!_  & diag_htable
  subroutine diag_htable_t &
       & (ierr, htb, name, u)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,         intent(out)         :: ierr
    type(htable_t),  intent(in)          :: htb
    character(len=*),intent(in),optional :: name
    integer,         intent(in),optional :: u
    integer utmp
    character(len=128) :: txt
    character(len=128) :: stt
    character(len=128) :: tag
    character(len=htb%kmdl*kunit) :: kstr
    integer nset, jt, js, je
    integer pb, pe
    integer nt
    integer jp

    ierr = 0
    utmp = choice(ulog, u)
101 format('hashtable:', A, ':', 1x, I0, 1x, I0, 1x, I0, 1x, I0, 1x, 3(1x, I0))
102 format('hashtable:', 1x, I0, 1x, I0, 1x, I0, 1x, I0, 1x, 3(1x, I0))
    if (present(name)) then
       write(txt, 101)  trim(name), &
            & htb%kmdl, htb%base, htb%width, htb%def, &
            & htb%ntag, htb%ktag, htb%lstt
    else
       write(txt, 102) &
            & htb%kmdl, htb%base, htb%width, htb%def, &
            & htb%ntag, htb%ktag, htb%lstt
    endif
    call msg_mdl(txt, __MDL__, utmp)

    nset = 0
201 format(2x, I0, ':', I0, 1x, '[', I0, '+', I0, ']', 2x, A, 1x, A, '(', A, ')')
202 format(2x, I0, ':', I0, 1x, '[', I0, '+', I0, ']', 2x, A, 1x, A)
203 format(2x, 'content: ', I0, '/', I0, ' +', I0)
204 format(2x, 'content: ', I0, '/', I0)
    nt = htb%ntag

212 format(16(1x, I0))
211 format(16(I0, ','))

    do js = 0, max(1, htb%nslot) - 1
       do jt = 0, htb%mem - 1
          je = js * htb%mem + jt
          if (htb%nxt(je).ge.0) then
             pb = je * htb%kmdl
             pe = pb + htb%kmdl
             kstr = transfer(htb%kbuf(pb:pe-1), kstr)
             if (kstr.eq.' ') kstr=''''''
             write(stt, 212) htb%stt(nt:, je)
             if (nt.gt.0) then
                write(tag, 211) htb%stt(0:nt-1, je)
                jp = len_trim(tag)
                tag(jp:jp) = ' '
                write(txt, 201) &
                     & nset, je, &
                     & htb%mofs(je), htb%nxt(je), &
                     & trim(stt(2:)),  trim(kstr), trim(tag(:))
             else
                write(txt, 202) &
                     & nset, je, &
                     & htb%mofs(je), htb%nxt(je), &
                     & trim(stt(2:)),  trim(kstr)
             endif
             call msg_mdl(txt, __MDL__, utmp)
             nset = nset + 1
          endif
       enddo
    enddo
    if (htb%nslot.gt.0) then
       write(txt, 203) nset, htb%mem, htb%nslot
    else
       write(txt, 204) nset, htb%mem
    endif
    call msg_mdl(txt, __MDL__, utmp)

    return
  end subroutine diag_htable_t
!!!_   & diag_htable_h
  subroutine diag_htable_h &
       & (ierr, hh, u)
    implicit none
    integer,         intent(out) :: ierr
    integer,optional,intent(in)  :: hh
    integer,optional,intent(in)  :: u
    integer jh
    character(len=lname) :: name

101 format('HT[', I0, ']')
    if (present(hh)) then
       jh = check_htable(hh)
       ierr = min(0, jh)
       if (ierr.eq.0) then
          write(name, 101) jh
          call diag_htable_t(ierr, htable(jh), name, u=u)
       endif
    else
       do jh = 0, min(ntable, ltable) - 1
          if (ierr.eq.0) then
             write(name, 101) jh
             call diag_htable_t(ierr, htable(jh), name, u=u)
          endif
       enddo
    endif

  end subroutine diag_htable_h

!!!_  & diag_wtable
  subroutine diag_wtable_t &
       & (ierr, wt, name, u)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,         intent(out)         :: ierr
    type(wtable_t),  intent(in)          :: wt
    character(len=*),intent(in),optional :: name
    integer,         intent(in),optional :: u
    character(len=128) :: buf
    integer utmp
    ierr = 0
    utmp = choice(ulog, u)
101 format(A, '[WM*', I0, '+', I0, ']')
    write(buf, 101) trim(name), wt%root, wt%seed
    call diag_htable(ierr, wt%ht, buf, utmp)

102 format(2x, 'items: ', I0, '/', I0)
    write(buf, 102) wt%n, wt%l
    call msg_mdl(buf, __MDL__, utmp)

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
    if (present(wh)) then
       jw = check_wtable(wh)
       ierr = min(0, jw)
       if (ierr.eq.0) then
          e = wsystem%entr(jw)
          call query_name_core(ierr, name, e, wsystem%ht)
          call diag_wtable_t(ierr, wmarks(jw), name, u=u)
       endif
    else
       do jw = 0, min(wsystem%n, wsystem%l) - 1
          if (ierr.eq.0) then
             e = wsystem%entr(jw)
             call query_name_core(ierr, name, e, wsystem%ht)
             ! write(*, *) 'wm', jw, e, name
             call diag_wtable_t(ierr, wmarks(jw), name, u=u)
          endif
       enddo
    endif
  end subroutine diag_wtable_h

!!!_ + hash-table primitives
!!!_  & new_htable
  integer function new_htable &
       & (name, lkey, mem, def, ntag, ktag, nstt, base, width, grow) &
       & result(hh)
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    character(len=*),intent(in) :: name
    integer,         intent(in) :: lkey
    integer,optional,intent(in) :: mem
    integer,optional,intent(in) :: def
    integer,optional,intent(in) :: ntag, ktag, nstt
    integer,optional,intent(in) :: base, width
    logical,optional,intent(in) :: grow
    integer esys
    integer jerr

    esys = new_entry_core_bare(name, htable(hh_sys))
    if (esys.lt.0) then
       hh = esys
       return
    endif

    hh = ntable
    ntable = ntable + 1
    if (hh.ge.ltable) then
       hh = -1
       return
    endif
    call alloc_htable &
         & (jerr, htable(hh), lkey, def, ntag, ktag, def, nstt, base, width, mem, grow)
    if (jerr.eq.0) then
       ! htable(hh)%name = name
       call settle_entry_core(jerr, htable(hh_sys), esys, hh)
    endif
    if (jerr.lt.0) hh = -1
  end function new_htable

!!!_  & new_entry - declare new entry (need to settle)
  ! new_entry family does not key existence, but register unconditionally.
  integer function new_entry_bare &
       & (key, hh) &
       & result(e)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: hh
    e = check_htable(hh)
    if (e.ge.0) then
       e = new_entry_core_bare(key, htable(e))
    endif
  end function new_entry_bare

  integer function new_entry_stag &
       & (key, tag, hh) &
       & result(e)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: tag
    integer,         intent(in) :: hh
    e = check_htable(hh)
    if (e.ge.0) then
       e = new_entry_core_stag(key, tag, htable(e))
    endif
  end function new_entry_stag

  integer function new_entry_atag &
       & (key, tags, hh) &
       & result(e)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: tags(:)
    integer,         intent(in) :: hh
    e = check_htable(hh)
    if (e.ge.0) then
       e = new_entry_core_atag(key, tags(:), htable(e))
    endif
  end function new_entry_atag

  integer function new_entry_func &
       & (key, hh, func) &
       & result(e)
    implicit none
    integer,         intent(in) :: hh
    character(len=*),intent(in) :: key
    interface
       integer function func(key)
         implicit none
         character(len=*),intent(in) :: key
       end function func
    end interface
    e = check_htable(hh)
    if (e.ge.0) then
       e = new_entry_core_func(key, htable(e), func)
    endif
  end function new_entry_func

!!!_  & reg_entry - register new entry (declare and settle at once)
  integer function reg_entry_bare &
       & (key, hh, status) &
       & result(e)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: hh
    integer,optional,intent(in) :: status
    integer jerr, jh

    jh = check_htable(hh)
    if (jh.ge.0) then
       e = new_entry_core_bare(key, htable(jh))
       if (e.ge.0) then
          call settle_entry_core(jerr, htable(jh), e, status)
          if (jerr.lt.0) e = jerr
       endif
    else
       e = jh
    endif
  end function reg_entry_bare

  integer function reg_entry_stag &
       & (key, tag, hh, status) &
       & result(e)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: hh
    integer,         intent(in) :: tag
    integer,         intent(in) :: status
    integer jerr, jh

    jh = check_htable(hh)
    if (jh.ge.0) then
       e = new_entry_core_stag(key, tag, htable(jh))
       if (e.ge.0) then
          call settle_entry_core(jerr, htable(jh), e, status)
          if (jerr.lt.0) e = jerr
       endif
    else
       e = jh
    endif
  end function reg_entry_stag
  integer function reg_entry_atag &
       & (key, tags, hh, status) &
       & result(e)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: hh
    integer,         intent(in) :: tags(:)
    integer,         intent(in) :: status
    integer jerr, jh

    jh = check_htable(hh)
    if (jh.ge.0) then
       e = new_entry_core_atag(key, tags(:), htable(jh))
       if (e.ge.0) then
          call settle_entry_core(jerr, htable(jh), e, status)
          if (jerr.lt.0) e = jerr
       endif
    else
       e = jh
    endif
  end function reg_entry_atag

  integer function reg_entry_func &
       & (key, hh, status, func) &
       & result(e)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: hh
    integer,optional,intent(in) :: status
    interface
       integer function func(key)
         implicit none
         character(len=*),intent(in) :: key
       end function func
    end interface
    integer jerr, jh

    jh = check_htable(hh)
    if (jh.ge.0) then
       e = new_entry_core_func(key, htable(jh), func)
       if (e.ge.0) then
          call settle_entry_core(jerr, htable(jh), e, status)
          if (jerr.lt.0) e = jerr
       endif
    else
       e = jh
    endif
  end function reg_entry_func
!!!_  & settle_entry - entry settlement
  subroutine settle_entry &
       & (ierr, entr, hh, status)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: hh
    integer,         intent(in)  :: entr
    integer,optional,intent(in)  :: status
    integer jh
    jh = check_htable(hh)
    ierr = min(0, jh)
    if (ierr.eq.0) call settle_entry_core(ierr, htable(jh), entr, status)
  end subroutine settle_entry

!!!_  & query_entry
  integer function query_entry_bare &
       & (key, hh) &
       & result(e)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: hh
    e = check_htable(hh)
    if (e.ge.0) then
       e = query_entry_core_bare(key, htable(e))
    endif
  end function query_entry_bare
  integer function query_entry_stag &
       & (key, tag, hh) &
       & result(e)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: tag
    integer,         intent(in) :: hh
    e = check_htable(hh)
    if (e.ge.0) then
       e = query_entry_core_stag(key, tag, htable(e))
    endif
  end function query_entry_stag
  integer function query_entry_atag &
       & (key, tags, hh) &
       & result(e)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: tags(:)
    integer,         intent(in) :: hh
    e = check_htable(hh)
    if (e.ge.0) then
       e = query_entry_core_atag(key, tags(:), htable(e))
    endif
  end function query_entry_atag

  integer function query_entry_func &
       & (key, hh, func) &
       & result(e)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: hh
    interface
       integer function func(key)
         implicit none
         character(len=*),intent(in) :: key
       end function func
    end interface
    e = check_htable(hh)
    if (e.ge.0) then
       e = query_entry_core_func(key, htable(e), func)
    endif
  end function query_entry_func

!!!_  & query_name
  subroutine query_name &
       & (ierr, name, entr, hh)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: name
    integer,         intent(in)  :: entr
    integer,         intent(in)  :: hh
    integer jh
    jh = check_htable(hh)
    ierr = min(0, jh)
    if (ierr.eq.0) call query_name_core(ierr, name, entr, htable(jh))
  end subroutine query_name

!!!_  & query_status ()
  integer function query_status_bare &
       & (key, hh) &
       & result(stt)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: hh
    integer jx
    integer e
    integer jh

    jh = check_htable(hh)
    if (jh.ge.0) then
       e = query_entry_core_bare(key, htable(jh))
       jx = htable(jh)%ntag
       stt = htable(jh)%stt(jx, e)
    else
       stt = jh
    endif
  end function query_status_bare
  integer function query_status_stag &
       & (key, tag, hh) &
       & result(stt)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: tag
    integer,         intent(in) :: hh
    integer jx
    integer e
    integer jh

    jh = check_htable(hh)
    if (jh.ge.0) then
       e = query_entry_core_stag(key, tag, htable(jh))
       jx = htable(jh)%ntag
       stt = htable(jh)%stt(jx, e)
    else
       stt = jh
    endif
  end function query_status_stag
  integer function query_status_atag &
       & (key, tags, hh) &
       & result(stt)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: tags(:)
    integer,         intent(in) :: hh
    integer jh

    jh = check_htable(hh)
    if (jh.ge.0) then
       stt = query_status_core_atag(key, tags(:), htable(jh))
    else
       stt = jh
    endif
  end function query_status_atag
  integer function query_status_func &
       & (key, hh, func) &
       & result(stt)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: hh
    interface
       integer function func(key)
         implicit none
         character(len=*),intent(in) :: key
       end function func
    end interface
    integer e
    integer jx
    integer jh

    jh = check_htable(hh)
    if (jh.ge.0) then
       e = query_entry_core_func(key, htable(jh), func)
       jx = htable(jh)%ntag
       stt = htable(jh)%stt(jx, e)
    else
       stt = jh
    endif
  end function query_status_func

  integer function query_status_entr &
       & (entr, hh) &
       & result(stt)
    implicit none
    integer,intent(in) :: entr
    integer,intent(in) :: hh
    integer jx
    integer jh

    jh = check_htable(hh)
    if (jh.ge.0) then
       jx = htable(jh)%ntag
       stt = htable(jh)%stt(jx, entr)
    else
       stt = jh
    endif
  end function query_status_entr

!!!_  & store_xstatus - store extra status
  subroutine store_xstatus_scl &
       & (ierr, entr, hh, status, k)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: hh
    integer,         intent(in)  :: entr
    integer,         intent(in)  :: status
    integer,optional,intent(in)  :: k
    integer jh
    jh = check_htable(hh)
    ierr = min(0, jh)
    if (ierr.eq.0) call save_status(ierr, htable(jh), entr, status, choice(0, k))
  end subroutine store_xstatus_scl

  subroutine store_xstatus_arr &
       & (ierr, entr, hh, status, k)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: hh
    integer,         intent(in)  :: entr
    integer,         intent(in)  :: status(:)
    integer,optional,intent(in)  :: k
    integer jh
    jh = check_htable(hh)
    ierr = min(0, jh)
    if (ierr.eq.0) call save_status(ierr, htable(jh), entr, status(:), choice(0, k))
  end subroutine store_xstatus_arr

!!!_  & restore_xstatus - store extra status
  subroutine restore_xstatus_scl &
       & (ierr, status, entr, hh, k)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: status
    integer,         intent(in)  :: hh
    integer,         intent(in)  :: entr
    integer,optional,intent(in)  :: k
    integer jh
    jh = check_htable(hh)
    ierr = min(0, jh)
    if (ierr.eq.0) call load_status(ierr, status, htable(jh), entr, choice(0, k))
  end subroutine restore_xstatus_scl

  subroutine restore_xstatus_arr &
       & (ierr, status, entr, hh, k)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: status(:)
    integer,         intent(in)  :: hh
    integer,         intent(in)  :: entr
    integer,optional,intent(in)  :: k
    integer jh
    jh = check_htable(hh)
    ierr = min(0, jh)
    if (ierr.eq.0) call load_status(ierr, status(:), htable(jh), entr, choice(0, k))
  end subroutine restore_xstatus_arr

!!!_ + hash-table watermarks
!!!_  & new_wtable
  integer function new_wtable &
       & (name,   lkey, ltable, &
       &  root,   seed, &
       &  deftag, ntag, ktag,   defstt, nstt, base, width, lbuf, grow) &
       &  result(wh)
    implicit none
    character(len=*),intent(in) :: name
    integer,         intent(in) :: lkey
    integer,         intent(in) :: ltable
    integer,optional,intent(in) :: root, seed
    integer,optional,intent(in) :: lbuf
    integer,optional,intent(in) :: deftag, ntag, ktag
    integer,optional,intent(in) :: defstt, nstt
    integer,optional,intent(in) :: base, width
    logical,optional,intent(in) :: grow

    integer jerr
    integer jw

    call reg_item_core_bare(jerr, wh, wsystem, flag_default, name)
    ! write(*, *) 'new_wtable', 0, jerr, wh
    if (jerr.eq.0) then
       jw = check_wtable(wh)
       jerr = min(0, jw)
    endif
    ! write(*, *) 'new_wtable', 1, jerr, jw
    if (jerr.eq.0) then
       call alloc_wtable &
            & (jerr,   wmarks(jw), &
            &  lkey,   ltable,     root, seed,   &
            &  deftag, ntag,       ktag, defstt, nstt, base, width, lbuf, grow)
    endif
    ! write(*, *) 'new_wtable', 2, jerr
    if (jerr.ne.0) wh = jerr
    return
  end function new_wtable

!!!_  & reg_item
  subroutine reg_item_bare &
       & (ierr, handle, wh, flag, key)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: handle
    integer,         intent(in)  :: wh
    integer,         intent(in)  :: flag
    character(len=*),intent(in)  :: key
    integer jw

    jw = check_wtable(wh)
    ierr = min(0, jw)
    if (ierr.eq.0) then
       call reg_item_core_bare(ierr, handle, wmarks(jw), flag, key)
    endif

  end subroutine reg_item_bare

  subroutine reg_item_stag &
       & (ierr, handle, wh, flag, key, tag)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: handle
    integer,         intent(in)  :: wh
    integer,         intent(in)  :: flag
    character(len=*),intent(in)  :: key
    integer,         intent(in)  :: tag
    integer jw

    jw = check_wtable(wh)
    ierr = min(0, jw)
    if (ierr.eq.0) then
       call reg_item_core_stag(ierr, handle, wmarks(jw), flag, key, tag)
    endif
  end subroutine reg_item_stag

  subroutine reg_item_atag &
       & (ierr, handle, wh, flag, key, tags)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: handle
    integer,         intent(in)  :: wh
    integer,         intent(in)  :: flag
    character(len=*),intent(in)  :: key
    integer,         intent(in)  :: tags(:)
    integer jw

    jw = check_wtable(wh)
    ierr = min(0, jw)
    if (ierr.eq.0) then
       call reg_item_core_atag(ierr, handle, wmarks(jw), flag, key, tags(:))
    endif

  end subroutine reg_item_atag

!!!_  & reg_alias
  subroutine reg_alias_bare &
       & (ierr, handle, wh, flag, key)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: handle
    integer,         intent(in)  :: wh
    integer,         intent(in)  :: flag
    character(len=*),intent(in)  :: key
    integer jw

    jw = check_wtable(wh)
    ierr = min(0, jw)
    if (ierr.eq.0) then
       call set_item_core_bare(ierr, handle, wmarks(jw), flag, key)
    endif
  end subroutine reg_alias_bare

  subroutine reg_alias_stag &
       & (ierr, handle, wh, flag, key, tag)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: handle
    integer,         intent(in)  :: wh
    integer,         intent(in)  :: flag
    character(len=*),intent(in)  :: key
    integer,         intent(in)  :: tag
    integer jw

    jw = check_wtable(wh)
    ierr = min(0, jw)
    if (ierr.eq.0) then
       call set_item_core_stag(ierr, handle, wmarks(jw), flag, key, tag)
    endif
  end subroutine reg_alias_stag

  subroutine reg_alias_atag &
       & (ierr, handle, wh, flag, key, tags)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: handle
    integer,         intent(in)  :: wh
    integer,         intent(in)  :: flag
    character(len=*),intent(in)  :: key
    integer,         intent(in)  :: tags(:)
    integer jw

    jw = check_wtable(wh)
    ierr = min(0, jw)
    if (ierr.eq.0) then
       call set_item_core_atag(ierr, handle, wmarks(jw), flag, key, tags(:))
    endif
  end subroutine reg_alias_atag

!!!_  & search_item
  subroutine search_item_bare &
       & (ierr, handle, wh, key)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: handle
    integer,         intent(in)  :: wh
    character(len=*),intent(in)  :: key
    integer jw

    jw = check_wtable(wh)
    ierr = min(0, jw)
    if (ierr.eq.0) then
       call query_item_core_bare(ierr, handle, wmarks(jw), key)
    endif
  end subroutine search_item_bare

  subroutine search_item_stag &
       & (ierr, handle, wh, key, tag)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: handle
    integer,         intent(in)  :: wh
    character(len=*),intent(in)  :: key
    integer,         intent(in)  :: tag
    integer jw

    jw = check_wtable(wh)
    ierr = min(0, jw)
    if (ierr.eq.0) then
       call query_item_core_stag(ierr, handle, wmarks(jw), key, tag)
    endif
  end subroutine search_item_stag
  subroutine search_item_atag &
       & (ierr, handle, wh, key, tags)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: handle
    integer,         intent(in)  :: wh
    character(len=*),intent(in)  :: key
    integer,         intent(in)  :: tags(:)
    integer jw

    jw = check_wtable(wh)
    ierr = min(0, jw)
    if (ierr.eq.0) then
       call query_item_core_atag(ierr, handle, wmarks(jw), key, tags(:))
    endif
  end subroutine search_item_atag
!!!_  & check_handle()
  integer function check_handle(handle, wh) result(k)
    implicit none
    integer,intent(in) :: handle
    integer,intent(in) :: wh
    k = check_wtable(wh)
    if (k.ge.0) then
       k = check_handle_core(handle, wmarks(k))
    endif
  end function check_handle

!!!_  & check_index()
  integer function check_index(idx, wh) result(k)
    implicit none
    integer,intent(in) :: idx
    integer,intent(in) :: wh
    k = check_wtable(wh)
    if (k.ge.0) then
       k = check_index_core(idx, wmarks(k))
    endif
  end function check_index

!!!_  & get_item_keys
  subroutine get_item_keys(ierr, handle, wh, key, tags)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: handle
    integer,         intent(in)  :: wh
    character(len=*),intent(out) :: key
    integer,optional,intent(out) :: tags(:)
    integer jw
    ierr = 0
    jw = check_wtable(wh)
    ierr = min(0, jw)
    if (ierr.eq.0) then
       call query_keys_core(ierr, handle, wmarks(jw), key, tags)
    endif
  end subroutine get_item_keys

!!!_  & get_size_items
  integer function get_size_items(wh) result(n)
    implicit none
    integer,intent(in) :: wh
    n = check_wtable(wh)
    if (n.ge.0) n = wmarks(n)%n
  end function get_size_items

!!!_  & get_item_name
! !!!_  & store_props
!   subroutine store_props &
!        & (ierr, handle, wh, props)
!     implicit none
!     integer,intent(out) :: ierr
!     integer,intent(in)  :: handle
!     integer,intent(in)  :: wh
!     integer,intent(in)  :: props(:)
!     integer je
!     je = normalize(handle, wmarks(wh))
!     ierr = min(0, je)
!     if (ierr.eq.0) then
!        je = wmarks(wh)%entr(je)
!     endif
!   end subroutine store_props
! !!!_  & restore_props
!   subroutine restore_props &
!        & (ierr, handle, wh, props)
!     implicit none
!     integer,intent(out) :: ierr
!     integer,intent(in)  :: handle
!     integer,intent(in)  :: wh
!     integer,intent(out) :: props(:)
!     ierr = 0
!   end subroutine restore_props

!!!_ + core procedures
!!!_  & alloc_htable
  subroutine alloc_htable &
       & (ierr,   htb,   lkey,  &
       &  deftag, ntag,  ktag,  defstt, nstt, &
       &  base,   width, lbuf,  grow)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    type(htable_t),  intent(inout) :: htb
    integer,         intent(in)    :: lkey
    integer,optional,intent(in)    :: deftag
    integer,optional,intent(in)    :: ntag, ktag  ! hash-table associates (for collation)
    integer,optional,intent(in)    :: defstt
    integer,optional,intent(in)    :: nstt        ! hash-table associates (for properties)
    integer,optional,intent(in)    :: base, width       ! hash computation parameters
    integer,optional,intent(in)    :: lbuf
    logical,optional,intent(in)    :: grow

    integer m,  u,  b,  w
    integer nt, kt, ns, lx

    ierr = 0
    if (htb%mem.gt.0) then
       ! already allocated
       ierr = ERR_DUPLICATE_SET + ERR_MASK_STD_HTB
    endif
    if (ierr.eq.0) then
       nt = max(0, choice(0, ntag))
       kt = min(nt, max(0, choice(nt, ktag)))
       ns = max(1, choice(0, nstt))   !  minimum status size == 1
       lx = ns + nt
       ! write(*, *) 'nt = ', nt
       htb%ntag = nt
       htb%ktag = kt
       htb%lstt = lx

       m = choice(0, lbuf)
       if (m.le.0) m = OPT_HASH_TABLE_SIZE
       m = max(1, m)
       htb%mem = m
       u = max(1, (lkey - 1) / kunit + 1)
       htb%kmdl = u
       allocate(htb%stt(0:lx-1, kmin:m - 1), &
            &   htb%nxt(kmin:m - 1), &
            &   htb%mofs(kmin:m - 1), &
            &   htb%kbuf((kmin * u):(m * u -1)), &
            STAT=ierr)
    endif
    if (ierr.eq.0) then
       b = choice(0, base)
       if (b.le.0) b = OPT_HASH_BASE
       w = min(choice(0, width), kunit)
       ! w < -1:  no hash (sequential)
       ! w == -1: auto
       ! w == 0:  full width
       if (w.lt.-1) then
          w = 0
       else if (w.lt.0) then
          if (m.lt.128**1) then
             w = 3
          else if (m.lt.128**2) then
             w = 4
          else if (m.lt.128**3) then
             w = 5
          else if (m.lt.128**4) then
             w = 6
          else
             w = 8
          endif
       else if (w.eq.0) then
          w = kunit
       endif
       htb%base  = b
       htb%width = w
       htb%def = choice(unset, defstt)
    endif
    if (ierr.eq.0) htb%stt(0:nt-1,:) = choice(htb%def, deftag)
    if (ierr.eq.0) htb%stt(nt:,   :) = htb%def
    if (ierr.eq.0) htb%nxt(:) = unset
    if (ierr.eq.0) htb%mofs(:) = -1
    if (ierr.eq.0) htb%kbuf(:) = ' '
    if (ierr.eq.0) then
       if (choice(.false., grow)) then
          htb%nslot = 1
       else
          htb%nslot = 0
       endif
    endif
    return
  end subroutine alloc_htable

!!!_  & alloc_wtable
  subroutine alloc_wtable &
       & (ierr,   wt,     &
       &  lkey,   ltable, root,  seed, &
       &  deftag, ntag,   ktag,  defstt, nstt, &
       &  base,   width,  lbuf,  grow)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,         intent(out)   :: ierr
    type(wtable_t),  intent(inout) :: wt
    integer,         intent(in)    :: lkey
    integer,         intent(in)    :: ltable
    integer,optional,intent(in)    :: root, seed
    integer,optional,intent(in)    :: deftag
    integer,optional,intent(in)    :: ntag, ktag
    integer,optional,intent(in)    :: defstt
    integer,optional,intent(in)    :: nstt
    integer,optional,intent(in)    :: base, width
    integer,optional,intent(in)    :: lbuf
    logical,optional,intent(in)    :: grow

    integer b
    integer r, s

    ierr = 0
    b = choice(-1, lbuf)
    if (b.lt.0) b = ltable * 2

    call alloc_htable &
         & (ierr,   wt%ht, lkey, &
         &  deftag, ntag,  ktag, defstt, nstt, base, width, b, grow)
    if (ierr.eq.0) then
       wt%l = ltable
       wt%n = 0
       r = choice(0, root)
       if (r.le.0) r = OPT_WATERMARK_ROOT
       wt%root = r
       s = choice(-1, seed)
       if (s.lt.0) then
          s = min(max(2, mod(wseed, r)), r - 1)
       endif
       s = mod(s, r)

       wt%seed = s
       wseed = s + 1            ! update next seed

       allocate(wt%entr(0:ltable-1), STAT=ierr)
    endif
    return
  end subroutine alloc_wtable
!!!_  & settle_entry_core
  subroutine settle_entry_core &
       & (ierr, htb, entr, status)
    implicit none
    integer,         intent(out)   :: ierr
    type(htable_t),  intent(inout) :: htb
    integer,         intent(in)    :: entr
    integer,optional,intent(in)    :: status
    integer step, j0
    integer pb,   pe
    character(len=htb%kmdl*kunit) :: kstr
    integer jx

    ierr = check_range_entry(htb, entr)
    if (ierr.ne.0) return

    if (htb%nxt(entr).ge.0) then
       ! Already settled
       ierr = ERR_PANIC - ERR_MASK_STD_HTB
       return
    endif

    j0 = - (htb%nxt(entr) + 1) ! nxt is temporal, recorded in new_entry()

    step = mod(entr - j0 + htb%mem, htb%mem)
    htb%mofs(j0) = max(htb%mofs(j0), step)
    htb%nxt(entr) = 0
    if (present(status)) then
       jx = htb%ntag
       htb%stt(jx, entr) = status
    endif
    pb = entr * htb%kmdl
    pe = pb   + htb%kmdl
    kstr = transfer(htb%kbuf(pb:pe-1), kstr)
  end subroutine settle_entry_core

!!!_  & new_entry_core_bare - new entry
  integer function new_entry_core_bare &
       & (key, htb) &
       & result(e)
    implicit none
    character(len=*),intent(in)    :: key
    type(htable_t),  intent(inout) :: htb
    integer j0
    j0 = hash_core(key, htb, 0)
    e = new_entry_core(key, htb, j0)
  end function new_entry_core_bare
!!!_  & new_entry_core_stag - new entry with scalar tag
  integer function new_entry_core_stag &
       & (key, tag, htb) &
       & result(e)
    implicit none
    character(len=*),intent(in)    :: key
    integer,         intent(in)    :: tag
    type(htable_t),  intent(inout) :: htb
    integer j0
    j0 = hash_core(key, htb, tag)
    e = new_entry_core(key, htb, j0)
    if (e.ge.0) htb%stt(0, e) = tag
  end function new_entry_core_stag
!!!_  & new_entry_core_atag - new entry with array tag
  integer function new_entry_core_atag &
       & (key, tags, htb) &
       & result(e)
    implicit none
    character(len=*),intent(in)    :: key
    integer,         intent(in)    :: tags(:)
    type(htable_t),  intent(inout) :: htb
    integer j0
    integer nt
    j0 = hash_core(key, htb, tags(:))
    e = new_entry_core(key, htb, j0)
    if (e.ge.0) then
       nt = min(htb%ntag, size(tags, 1))
       htb%stt(0:nt-1, e) = tags(1:nt)
    endif
  end function new_entry_core_atag

!!!_   & new_entry_core_func
  integer function new_entry_core_func &
       & (key, htb, func) &
       & result(e)
    implicit none
    character(len=*),intent(in)    :: key
    type(htable_t),  intent(inout) :: htb
    interface
       integer function func(key)
         implicit none
         character(len=*),intent(in) :: key
       end function func
    end interface
    integer j0
    j0 = func(key)
    e = new_entry_core(key, htb, j0)
  end function new_entry_core_func

!!!_  & new_entry_core - store new entry at initial hash number j0
  integer function new_entry_core &
       & (key, htb, j0) &
       & result(e)
    implicit none
    character(len=*),intent(in)    :: key
    type(htable_t),  intent(inout) :: htb
    integer,         intent(in)    :: j0
    integer j
    integer pb, pe
    character(len=kunit*htb%kmdl) :: bs
    integer ocur, oprv
    integer sbgn, soff
    integer jerr
    integer jx

    jx = htb%ntag

    ocur = j0
    do
       sbgn = (ocur / htb%mem) * htb%mem
       soff = mod(ocur, htb%mem)
       do j = 0, htb%mem - 1
          e = mod(soff + j, htb%mem) + sbgn
          if (htb%nxt(e).lt.0) then   ! [e] not occupied
             pb = e  * htb%kmdl
             pe = pb + htb%kmdl
             bs = key
             htb%kbuf(pb:pe-1) = transfer(bs, htb%kbuf(pb:pe-1))
             htb%nxt(e) = - ocur - 1  ! record hash origin for later settlement
             htb%stt(jx:, e) = htb%def
             return
          endif
       enddo
       oprv = ocur
       ocur = htb%nxt(oprv)
       if (ocur.le.0) exit
    enddo
    if (htb%nslot.ge.1) then
       ocur = -1
       if (oprv / htb%mem .lt. htb%nslot - 1) then
          ! seek final slot
          sbgn = (htb%nslot - 1) * htb%mem
          soff = j0
          do j = 0, htb%mem - 1
             e = mod(soff + j, htb%mem) + sbgn
             if (htb%nxt(e).lt.0) then   ! [e] not occupied
                ocur = e
                exit
             endif
          enddo
       endif
       if (ocur.lt.0) then
          ! full slots
          call grow_htable(jerr, htb)
          if (jerr.eq.0) ocur = (htb%nslot - 1) * htb%mem + j0
       endif
       if (ocur.ge.0) then
          htb%nxt(oprv) = ocur
          e = ocur
          pb = e  * htb%kmdl
          pe = pb + htb%kmdl
          bs = key
          htb%kbuf(pb:pe-1) = transfer(bs, htb%kbuf(pb:pe-1))
          htb%nxt(e) = - ocur - 1
          htb%stt(jx:, e) = htb%def
          return
       endif
    endif
    e = -1
    return
  end function new_entry_core

!!!_   & query_entry_core_bare
  integer function query_entry_core_bare &
       & (key, htb) &
       & result(e)
    implicit none
    character(len=*),intent(in) :: key
    type(htable_t),  intent(in) :: htb
    interface
       integer function func(key)
         implicit none
         character(len=*),intent(in) :: key
       end function func
    end interface
    integer ocur, jini
    ocur = hash_core(key, htb, 0)
    jini = 0
    e = query_entry_core(key, htb, ocur, jini)
  end function query_entry_core_bare

!!!_   & query_entry_core_stag
  integer function query_entry_core_stag &
       & (key, tag, htb) &
       & result(e)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: tag
    type(htable_t),  intent(in) :: htb
    interface
       integer function func(key)
         implicit none
         character(len=*),intent(in) :: key
       end function func
    end interface
    integer ocur, jini
    ocur = hash_core(key, htb, tag)
    jini = 0
    do
       e = query_entry_core(key, htb, ocur, jini)
       if (e.lt.0) exit
       if (htb%stt(0, e).eq.tag) return
    enddo
  end function query_entry_core_stag

!!!_   & query_entry_core_atag
  integer function query_entry_core_atag &
       & (key, tags, htb) &
       & result(e)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: tags(:)
    type(htable_t),  intent(in) :: htb
    integer nt
    interface
       integer function func(key)
         implicit none
         character(len=*),intent(in) :: key
       end function func
    end interface
    integer ocur, jini
    nt = min(htb%ntag, size(tags, 1))
    ocur = hash_core(key, htb, tags(:))
    jini = 0
    do
       e = query_entry_core(key, htb, ocur, jini)
       if (e.lt.0) exit
       if (ALL(htb%stt(0:nt-1, e).eq.tags(1:nt))) return
    enddo
  end function query_entry_core_atag

!!!_   & query_entry_core_func
  integer function query_entry_core_func &
       & (key, htb, func) &
       & result(e)
    implicit none
    character(len=*),intent(in) :: key
    type(htable_t),  intent(in) :: htb
    interface
       integer function func(key)
         implicit none
         character(len=*),intent(in) :: key
       end function func
    end interface
    integer ocur, jini
    ocur = func(key)
    jini = 0
    e = query_entry_core(key, htb, ocur, jini)
  end function query_entry_core_func

!!!_  & query_entry_core - query entry at initial hash number j0
  integer function query_entry_core &
       & (key, htb, ocur, jini) &
       & result(e)
    implicit none
    character(len=*),intent(in)    :: key
    type(htable_t),  intent(in)    :: htb
    integer,         intent(inout) :: ocur    ! offset, to remember
    integer,         intent(inout) :: jini    ! initial shift, to remember
    integer j
    character(len=kunit*htb%kmdl) :: b
    integer pb, pe
    integer sbgn, soff

    do
       sbgn = (ocur / htb%mem) * htb%mem
       soff = mod(ocur, htb%mem)
       ! write(*, *) j0, ocur
       do j = jini, htb%mofs(ocur)
          e = mod(soff + j, htb%mem) + sbgn
          pb = e  * htb%kmdl
          pe = pb + htb%kmdl
          b = transfer(htb%kbuf(pb:pe-1), b)
          if (b.eq.key) then
             jini = j + 1
             return
          endif
       enddo
       jini = 0
       ocur = htb%nxt(ocur)
       if (ocur.le.0) exit
    enddo
    e = -1
  end function query_entry_core

!!!_  & query_status_core
  integer function query_status_core_bare &
       & (key, htb) &
       & result(stt)
    implicit none
    character(len=*),intent(in) :: key
    type(htable_t),  intent(in) :: htb
    integer e, jx

    e = query_entry_core_bare(key, htb)
    jx = htb%ntag
    stt = htb%stt(jx, e)

  end function query_status_core_bare

  integer function query_status_core_stag &
       & (key, tag, htb) &
       & result(stt)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: tag
    type(htable_t),  intent(in) :: htb
    integer e, jx

    e = query_entry_core_stag(key, tag, htb)
    jx = htb%ntag
    stt = htb%stt(jx, e)

  end function query_status_core_stag

  integer function query_status_core_atag &
       & (key, tags, htb) &
       & result(stt)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: tags(:)
    type(htable_t),  intent(in) :: htb
    integer e, jx

    e = query_entry_core_atag(key, tags(:), htb)
    jx = htb%ntag
    stt = htb%stt(jx, e)

  end function query_status_core_atag

!!!_  & query_name_core
  subroutine query_name_core &
       & (ierr, name, entr, htb)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: name
    integer,         intent(in)  :: entr
    type(htable_t),  intent(in)  :: htb

    integer pb, pe
    character(len = kunit * htb%kmdl) :: bs

    ierr = check_range_entry(htb, entr)
    if (ierr.eq.0) then
       pb = entr * htb%kmdl
       pe = pb   + htb%kmdl
       bs = transfer(htb%kbuf(pb:pe-1), bs)
       name = bs
    else
       name = ' '
    endif
    return
  end subroutine query_name_core

!!!_  & grow_htable
  subroutine grow_htable &
       & (ierr, htb)
    implicit none
    integer,       intent(out)   :: ierr
    type(htable_t),intent(inout) :: htb

    integer,allocatable :: ds(:,:), dn(:), dm(:)
    character(len=kunit),allocatable :: dk(:)
    integer m, u, g, mn
    integer lx
    if (htb%nslot.le.0) then
       ierr = ERR_OPR_DISABLE + ERR_MASK_STD_HTB
       return
    endif
    g  = htb%nslot
    m  = htb%mem * g
    u  = htb%kmdl
    lx = htb%lstt
    allocate(ds(0:lx-1, kmin:m - 1), dn(kmin:m - 1), dm(kmin:m - 1), &
         &   dk((kmin * u):(m * u -1)), STAT=ierr)
    if (ierr.eq.0) then
       ds(:, :) = htb%stt(:, :)
       dn(:) = htb%nxt(:)
       dm(:) = htb%mofs(:)
       dk(:) = htb%kbuf(:)
       deallocate(htb%stt, htb%nxt, htb%mofs, htb%kbuf, STAT=ierr)
    endif
    if (ierr.eq.0) then
       g = g + 1
       mn = htb%mem * g
       allocate(htb%stt(0:lx-1, kmin:mn - 1), &
            &   htb%nxt(kmin:mn - 1), &
            &   htb%mofs(kmin:mn - 1), &
            &   htb%kbuf((kmin * u):(mn * u -1)), &
            STAT=ierr)
    endif
    if (ierr.eq.0) then
       htb%stt(:, :m-1) = ds(:, :)
       htb%nxt(:m-1) = dn(:)
       htb%mofs(:m-1) = dm(:)
       htb%kbuf(:m * u -1) = dk(:)
       htb%stt(:, m:) = htb%def
       htb%nxt(m:) = unset
       htb%mofs(m:) = -1
       htb%kbuf(m*u:) = ' '

       htb%nslot = g
    endif
    return
  end subroutine grow_htable

!!!_  & hash_core()
  integer function hash_core_arr &
       & (key, htb, tags) &
       & result(n)
    implicit none
    character(len=*),intent(in) :: key
    type(htable_t),  intent(in) :: htb
    integer,         intent(in) :: tags(:)
    integer x, j, b
    integer nt
    nt = min(htb%ktag, size(tags, 1))
    x = 0
    b = htb%base
    do j = 1, nt
       x = x * b + tags(j)
    enddo
    n = hash_core_scl(key, htb, x)
  end function hash_core_arr

  integer function hash_core_scl &
       & (key, htb, ini) &
       & result(n)
    implicit none
    character(len=*),intent(in) :: key
    type(htable_t),  intent(in) :: htb
    integer,         intent(in) :: ini
    integer x
    integer j
    integer b, l, w
    integer,parameter :: maskh = (2 ** 30) - 1

    b = htb%base
    l = htb%mem
    w = min(htb%width, max(1, len_trim(key)))

    x = ini * htb%ktag
    do j = w, 1, -1
       x = x * b + ICHAR(key(j:j))
    enddo
    !! following code causes error on SX aurora
    !! due to unknown reason, seems to relate idiom.
    ! do j = htb%width, w + 1, -1
    !    x = x * b + ICHAR(' ')
    ! enddo
    n = MOD(IAND(x, maskh), l)

    return
  end function hash_core_scl

!!!_  & hash_std()
  integer function hash_std &
       & (key, hh, ini) &
       & result(n)
    implicit none
    character(len=*),intent(in) :: key
    integer,         intent(in) :: hh     ! no range check
    integer,         intent(in) :: ini
    n = hash_core(key, htable(hh), ini)
  end function hash_std

!!!_  & check_range_entry()
  integer function check_range_entry(htb, entr) result(e)
    implicit none
    type(htable_t),intent(in) :: htb
    integer,       intent(in) :: entr
    if (entr.lt.0 .or. entr.ge.(htb%mem * max(1, htb%nslot))) then
       e = ERR_OUT_OF_RANGE - ERR_MASK_STD_HTB
    else
       e = 0
    endif
  end function check_range_entry

!!!_  & reg_item_core
  subroutine reg_item_core_bare &
       & (ierr, handle, wt, flag, key)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: handle
    type(wtable_t),  intent(inout) :: wt
    integer,         intent(in)    :: flag
    character(len=*),intent(in)    :: key
    integer e

    handle = query_status_core_bare(key, wt%ht)
    if (handle.ge.0) then
       if (AND(flag, flag_ignore).eq.0) ierr = ERR_DUPLICATE_SET - ERR_MASK_STD_HTB
       return
    endif
    e = new_entry_core_bare(key, wt%ht)
    ierr = min(0, e)
    if (ierr.eq.0) call settle_item_core(ierr, handle, wt, e)
    return
  end subroutine reg_item_core_bare

  subroutine reg_item_core_stag &
       & (ierr, handle, wt, flag, key, tag)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: handle
    type(wtable_t),  intent(inout) :: wt
    integer,         intent(in)    :: flag
    character(len=*),intent(in)    :: key
    integer,         intent(in)    :: tag
    integer e

    handle = query_status_core_stag(key, tag, wt%ht)
    if (handle.ge.0) then
       if (AND(flag, flag_ignore).eq.0) ierr = ERR_DUPLICATE_SET - ERR_MASK_STD_HTB
       return
    endif
    e = new_entry_core_stag(key, tag, wt%ht)
    ierr = min(0, e)
    if (ierr.eq.0) call settle_item_core(ierr, handle, wt, e)
    return
  end subroutine reg_item_core_stag

  subroutine reg_item_core_atag &
       & (ierr, handle, wt, flag, key, tags)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: handle
    type(wtable_t),  intent(inout) :: wt
    integer,         intent(in)    :: flag
    character(len=*),intent(in)    :: key
    integer,         intent(in)    :: tags(:)
    integer e

    handle = query_status_core_atag(key, tags(:), wt%ht)
    if (handle.ge.0) then
       if (AND(flag, flag_ignore).eq.0) ierr = ERR_DUPLICATE_SET - ERR_MASK_STD_HTB
       return
    endif
    e = new_entry_core_atag(key, tags(:), wt%ht)
    ierr = min(0, e)
    if (ierr.eq.0) call settle_item_core(ierr, handle, wt, e)
    return
  end subroutine reg_item_core_atag

!!!_  & set_item_core
  subroutine set_item_core_bare &
       & (ierr, handle, wt, flag, key)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(in)    :: handle
    type(wtable_t),  intent(inout) :: wt
    integer,         intent(in)    :: flag
    character(len=*),intent(in)    :: key
    integer e

    e = query_status_core_bare(key, wt%ht)
    if (e.ge.0) then
       if (AND(flag, flag_ignore).eq.0) ierr = ERR_DUPLICATE_SET - ERR_MASK_STD_HTB
       return
    endif
    e = new_entry_core_bare(key, wt%ht)
    ierr = min(0, e)
    if (ierr.eq.0) then
       call settle_entry_core(ierr, wt%ht, e, handle)
    endif
    return
  end subroutine set_item_core_bare

  subroutine set_item_core_stag &
       & (ierr, handle, wt, flag, key, tag)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(in)    :: handle
    type(wtable_t),  intent(inout) :: wt
    integer,         intent(in)    :: flag
    character(len=*),intent(in)    :: key
    integer,         intent(in)    :: tag
    integer e

    e = query_status_core_stag(key, tag, wt%ht)
    if (e.ge.0) then
       if (AND(flag, flag_ignore).eq.0) ierr = ERR_DUPLICATE_SET - ERR_MASK_STD_HTB
       return
    endif
    e = new_entry_core_stag(key, tag, wt%ht)
    ierr = min(0, e)
    if (ierr.eq.0) then
       call settle_entry_core(ierr, wt%ht, e, handle)
    endif
    return
  end subroutine set_item_core_stag
  subroutine set_item_core_atag &
       & (ierr, handle, wt, flag, key, tags)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(in)    :: handle
    type(wtable_t),  intent(inout) :: wt
    integer,         intent(in)    :: flag
    character(len=*),intent(in)    :: key
    integer,         intent(in)    :: tags(:)
    integer e

    e = query_status_core_atag(key, tags(:), wt%ht)
    if (e.ge.0) then
       if (AND(flag, flag_ignore).eq.0) ierr = ERR_DUPLICATE_SET - ERR_MASK_STD_HTB
       return
    endif
    e = new_entry_core_atag(key, tags(:), wt%ht)
    ierr = min(0, e)
    if (ierr.eq.0) then
       call settle_entry_core(ierr, wt%ht, e, handle)
    endif
    return
  end subroutine set_item_core_atag

!!!_  & query_item_core
  subroutine query_item_core_bare &
       & (ierr, handle, wt, key)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: handle
    type(wtable_t),  intent(in)  :: wt
    character(len=*),intent(in)  :: key

    ierr = 0
    handle = query_status_core_bare(key, wt%ht)
    if (handle.lt.0) then
       ierr = ERR_NOT_FOUND - ERR_MASK_STD_HTB
    endif
  end subroutine query_item_core_bare

  subroutine query_item_core_stag &
       & (ierr, handle, wt, key, tag)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: handle
    type(wtable_t),  intent(in)  :: wt
    character(len=*),intent(in)  :: key
    integer,         intent(in)  :: tag

    ierr = 0
    handle = query_status_core_stag(key, tag, wt%ht)
    if (handle.lt.0) then
       ierr = ERR_NOT_FOUND - ERR_MASK_STD_HTB
    endif
  end subroutine query_item_core_stag
  subroutine query_item_core_atag &
       & (ierr, handle, wt, key, tags)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: handle
    type(wtable_t),  intent(in)  :: wt
    character(len=*),intent(in)  :: key
    integer,         intent(in)  :: tags(:)

    ierr = 0
    handle = query_status_core_atag(key, tags(:), wt%ht)
    if (handle.lt.0) then
       ierr = ERR_NOT_FOUND - ERR_MASK_STD_HTB
    endif
  end subroutine query_item_core_atag

!!!_  & settle_item_core
  subroutine settle_item_core &
       & (ierr, handle, wt, entr)
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: handle
    type(wtable_t),  intent(inout) :: wt
    integer,         intent(in)    :: entr
    integer ji

    ierr = 0
    ji = wt%n
    handle = watermark(ji, wt)
    wt%n = wt%n + 1
    if (wt%n.ge.wt%l) then
       ierr = ERR_INSUFFICIENT_BUFFER - ERR_MASK_STD_HTB
    endif
    if (ierr.eq.0) then
       wt%entr(ji) = entr
       call settle_entry_core(ierr, wt%ht, entr, handle)
    endif

  end subroutine settle_item_core

!!!_  - query_keys_core
  subroutine query_keys_core &
       & (ierr, handle, wt, key, tags)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: handle
    type(wtable_t),  intent(in)  :: wt
    character(len=*),intent(out) :: key
    integer,optional,intent(out) :: tags(:)
    integer e
    e = check_handle_core(handle, wt)
    ierr = min(0, e)
    if (ierr.eq.0) then
       e = wt%entr(e)
       call query_name_core(ierr, key, e, wt%ht)
    endif
    if (present(tags)) then
       if (ierr.eq.0) call get_tags(ierr, tags, e, wt%ht)
    endif
  end subroutine query_keys_core

!!!_  & check_handle_core()
  integer function check_handle_core(handle, wt) result(k)
    implicit none
    integer,       intent(in) :: handle
    type(wtable_t),intent(in) :: wt
    k = normalize(handle, wt)
    if (k.lt.0 .or. k.ge.min(wt%n, wt%l)) then
       k = ERR_OUT_OF_RANGE - ERR_MASK_STD_HTB
    endif
  end function check_handle_core

!!!_  & check_index_core()
  integer function check_index_core(idx, wt) result(k)
    implicit none
    integer,       intent(in) :: idx
    type(wtable_t),intent(in) :: wt
    k = idx
    if (k.lt.0 .or. k.ge.min(wt%n, wt%l)) then
       k = ERR_OUT_OF_RANGE - ERR_MASK_STD_HTB
    endif
    k = watermark(k, wt)
  end function check_index_core

!!!_  & check_htable() - check if valid htable and return index
  integer function check_htable (hh) result(k)
    implicit none
    integer,intent(in) :: hh
    if (hh.lt.hh_sys.or.hh.ge.min(ntable, ltable)) then
       k = ERR_OUT_OF_RANGE - ERR_MASK_STD_HTB
    else
       k = hh
    endif
  end function check_htable

!!!_  & check_wtable() - check if valid wtable and return index
  integer function check_wtable (wh) result(k)
    implicit none
    integer,intent(in) :: wh
    k = normalize(wh, wsystem)
    if (k.ge.min(wsystem%n, wsystem%l)) then
       k = ERR_OUT_OF_RANGE - ERR_MASK_STD_HTB
    endif
  end function check_wtable

!!!_  & save_status_scl
  subroutine save_status_scl_t (ierr, htb, entr, stt, k)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    type(htable_t),  intent(inout) :: htb
    integer,         intent(in)    :: entr
    integer,         intent(in)    :: stt
    integer,optional,intent(in)    :: k     ! status id
    integer jx
    ierr = check_range_entry(htb, entr)
    if (ierr.eq.0) then
       jx = htb%ntag + choice(0, k)
       htb%stt(jx, entr) = stt
    endif
  end subroutine save_status_scl_t
!!!_  & save_status_arr
  subroutine save_status_arr_t (ierr, htb, entr, stts, k)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    type(htable_t),  intent(inout) :: htb
    integer,         intent(in)    :: entr
    integer,         intent(in)    :: stts(:)
    integer,optional,intent(in)    :: k     ! status id
    integer jx, nx
    ierr = check_range_entry(htb, entr)
    if (ierr.eq.0) then
       nx = size(stts, 1)
       jx = htb%ntag + choice(0, k)
       htb%stt(jx:jx+nx-1, entr) = stts(:)
    endif
  end subroutine save_status_arr_t

!!!_  & load_status_scl
  subroutine load_status_scl_t (ierr, stt, htb, entr, k)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: stt
    type(htable_t),  intent(in)  :: htb
    integer,         intent(in)  :: entr
    integer,optional,intent(in)  :: k     ! status id
    integer jx
    ierr = check_range_entry(htb, entr)
    if (ierr.eq.0) then
       jx = htb%ntag + choice(0, k)
       stt = htb%stt(jx, entr)
    endif
  end subroutine load_status_scl_t
!!!_  & load_status_arr
  subroutine load_status_arr_t (ierr, stts, htb, entr, k)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: stts(:)
    type(htable_t),  intent(in)  :: htb
    integer,         intent(in)  :: entr
    integer,optional,intent(in)  :: k     ! status id
    integer jx, nx
    ierr = check_range_entry(htb, entr)
    if (ierr.eq.0) then
       nx = size(stts, 1)
       jx = htb%ntag + choice(0, k)
       stts(:) = htb%stt(jx:jx+nx-1, entr)
    endif
  end subroutine load_status_arr_t

!!!_  & get_tags
  subroutine get_tags (ierr, tags, entr, htb)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: tags(:)
    type(htable_t),  intent(in)  :: htb
    integer,         intent(in)  :: entr
    integer nt
    ierr = check_range_entry(htb, entr)
    if (ierr.eq.0) then
       nt = size(tags, 1)
       nt = min(nt, htb%ntag)
       tags(1:nt) = htb%stt(0:nt-1, entr)
    endif
  end subroutine get_tags

!!!_  - watermar
  integer function watermark(id, wt) result(h)
    implicit none
    integer,       intent(in) :: id
    type(wtable_t),intent(in) :: wt
    if (id.lt.0) then
       h = id
    else
       h = id * wt%root + wt%seed
    endif
  end function watermark
!!!_  - normalize()
  integer function normalize(handle, wt) result(n)
    implicit none
    integer,       intent(in) :: handle
    type(wtable_t),intent(in) :: wt
    if (handle.lt.0) then
       n = handle
    else if (mod(handle, wt%root).ne.wt%seed) then
       n = ERR_INVALID_ITEM - ERR_MASK_STD_HTB
    else
       n = handle / wt%root
    endif
  end function normalize

!!!_ + end TOUZA_Std_htb
end module TOUZA_Std_htb

!!!_@ test_std_htb - test program
#ifdef TEST_STD_HTB
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

  if (ierr.eq.0) call diag(ierr, levv=+99)
  write(*, 101) 'diag', ierr
  if (ierr.eq.0) call finalize(ierr, levv=+10)
  write(*, 101) 'fine', ierr
  stop

contains
  subroutine test_bare_batch(ierr)
    implicit none
    integer,intent(out) :: ierr

    integer ca, cb, cc, ce, cf, cg
    integer e0, e1, e2, e3, esp

    ierr = 0

    ca  = new_htable('test-a', 0, width=6)
    cb  = new_htable('test-b', 0, def=-2)
    cc  = new_htable('test-c', 0)
    ce  = new_htable('test-e', 0, width=-1)
    cf  = new_htable('test-f', 0, width=-2)
    cg  = new_htable('test-gggggggG', 0)
    ! write(*, *) 'new:', ca, cb, cc, ce, cf

    if (ierr.eq.0) then
       e0  = new_entry('item-0', ca)
       call settle_entry(ierr, e0, ca)
       e1  = reg_entry('item-1', ca, 0)
       esp = reg_entry(' ', ca, 44)

       e2  = new_entry('item-2', ca)
       ! no settlement
       e2  = new_entry('item-2', ca)
       call settle_entry(ierr, e2, ca)

       e3  = new_entry('item-3', ca)
       ! no settlement
       e3  = new_entry('item-3x', ca)
       call settle_entry(ierr, e3, ca)
       e3  = new_entry('item-3', ca)
       call settle_entry(ierr, e3, ca)
    endif
  end subroutine test_bare_batch

  subroutine test_tags_batch(ierr)
    implicit none
    integer,intent(out) :: ierr
    integer c11, c22, c33
    integer c10, c21, c32

    ierr = 0

    c11 = new_htable('tag-1', 0, ntag=1)
    c22 = new_htable('tag-2', 0, ntag=2)
    c33 = new_htable('tag-3', 0, ntag=3)

    c10 = new_htable('tag-1/0', 0, ntag=1, ktag=0)
    c21 = new_htable('tag-2/1', 0, ntag=2, ktag=1)
    c32 = new_htable('tag-3/2', 0, ntag=3, ktag=2)

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

    e = reg_entry('A', (/0, 0, 0/), hh, 0)
    e = reg_entry('A', (/1, 0, 0/), hh, 1)
    e = reg_entry('A', (/2, 0, 0/), hh, 2)
    e = reg_entry('A', (/0, 1, 0/), hh, 3)
    e = reg_entry('A', (/1, 1, 0/), hh, 4)
    e = reg_entry('A', (/2, 1, 0/), hh, 5)
    e = reg_entry('A', (/0, 1, 1/), hh, 6)
    e = reg_entry('A', (/1, 1, 1/), hh, 7)
    e = reg_entry('A', (/2, 1, 1/), hh, 8)

  end subroutine test_tags

  subroutine test_grow_batch(ierr)
    integer,intent(out) :: ierr
    integer cd, ch, ci
    ierr = 0

    cd  = new_htable('test-d', 0, width=0)
    ch  = new_htable('test-h', 0, mem=4)
    ci  = new_htable('test-i', 0, width=2, mem=4, grow=.TRUE.)

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
    integer stt, s

101 format('x', I1, '-', I1)

    ierr = 0
    li = 4
    lo = 3

    do jo = 0, lo
       do ji = 0, li
          write(b, 101) ji, jo
          e = new_entry(b, hh)
          stt = test_status(ji, jo, li, lo)
          call settle_entry(ierr, e, hh, stt)
          ! call diag_htable(ierr, hh)
       enddo
       ! 201       format('grow:', I0, 1x, I0, 1x, I0, 1x, A)
       ! write(*, 201) hh, ierr, e, trim(b)
    enddo
    ierr = 0
102 format('query:', I0, '[', A, '] = ', I0, 1x, I0, 1x, I0)
    do jo = 0, lo + 1
       do ji = 0, li + 1
          write(b, 101) ji, jo
          e = query_entry(b, hh)
          s = query_status(e, hh)
          stt = test_status(ji, jo, li, lo)
          if (stt.ne.s .and.stt.ge.0) then
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
    wa = new_wtable('WM-a', 8, 8)
    wb = new_wtable('WM-b', 8, 8)
    wc = new_wtable('WM-c', 8, 8)

    write(*, *) 'WM:', wa, wb, wc

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
    write(buf, 101) j
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

!!!_! nio_cache.F90 - TOUZA/Nio cache-record extension
! Maintainer: SAITO Fuyuki
! Created: Nov 9 2022
#define TIME_STAMP 'Time-stamp: <2023/03/25 09:42:57 fuyuki nio_cache.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022,2023
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_nio.h"
!!!_* macros
#if HAVE_F2003_ALLOCATABLE_MEMBER
#  define _POINTER allocatable
#else  /* not HAVE_F2003_ALLOCATABLE_MEMBER */
#  define _POINTER pointer
#endif /* not HAVE_F2003_ALLOCATABLE_MEMBER */

#ifndef   OPT_NIO_CACHES_SIZE
#  define OPT_NIO_CACHES_SIZE 512
#endif
!!!_@ TOUZA_Nio_cache - nio with cache
module TOUZA_Nio_cache
!!!_ = declaration
  use TOUZA_Nio_std,only: KI32, KI64, KDBL, KFLT, KIOFS
  use TOUZA_Nio_std,only: get_logu,     unit_global,  trace_fine,   trace_control
  use TOUZA_Nio_header,nh_init=>init, nh_diag=>diag, nh_finalize=>finalize
  implicit none
  private
!!!_  - public parameters
  integer,parameter :: lax = 3

  integer,parameter,public :: coll_default = 0
  integer,parameter,public :: coll_strict    = -1
  integer,parameter,public :: coll_std       = 1    ! ignore DFMT ITEM DATE TIME TDUR TIME2 UTIM2 SIZE MISS AITMn ASTRn AENDn
  integer,parameter,public :: coll_basic     = 2    ! plus ignore TITL UNIT EDIT ETTL MEMO DMIN DMAX DIVS DIVL STYP [CIR]OPTN
  integer,parameter,public :: coll_nosign    = 3    ! plus ignore [CM]DATE [CM]SIGN
  integer,parameter,public :: coll_nonum     = 4    ! plus ignore DSET FNUM DNUM
  integer,parameter,public :: coll_nospecial = 8    ! disable DSET special

  integer,parameter,public :: gid_suite = -9   ! special group id to set all the variables in single suite
                                               ! only meaningful when specify variable id
  integer,parameter,public :: vid_suite = -99  ! special variable id to set all the records in single suite
!!!_  - private parameter
  integer,parameter :: ucache = 16
  integer,parameter :: lpath = OPT_PATH_LEN
!!!_  - types
  type var_t
     character(len=litem) :: item = ' '
     character(len=litem) :: unit = ' '
     character(len=litem) :: dfmt = ' '
     character(len=litem) :: co(0:lax-1) = ' '
     integer              :: jbgn(0:lax-1) = 0
     integer              :: jend(0:lax-1) = 0
     integer              :: neff
     integer              :: ceff(0:lax-1) = -1  ! effective coordinate index
     integer              :: flag
     integer              :: rofs                ! start index of cache%rpos
     integer              :: lastrec = -1        ! last accessed record
  end type var_t

  type group_t
     integer :: nvar = -1                   ! size of v
     integer :: nrec = -1                   ! number of records
     character(len=litem) :: g              ! group name
     character(len=litem) :: h(nitem)       ! header
     character(len=litem),pointer :: d(:) => NULL()    ! date [rec]
     character(len=litem),pointer :: t(:) => NULL()    ! time [rec]
     ! temporary properties, to integrated into cache_t%v
     type(var_t),pointer :: v(:) => NULL()
     integer(kind=KIOFS),pointer  :: rpos(:, :) => NULL() ! record offset [rec, var]
     integer(kind=KIOFS),pointer  :: rlen(:, :) => NULL() ! record size   [rec, var]
  end type group_t

  type cache_t
     integer :: ngrp = -1
     type(group_t),pointer :: g(:)
     type(var_t),pointer   :: v(:)
     integer,pointer       :: o(:)   ! group-variable offset
     integer(kind=KIOFS),pointer :: rpos(:) => NULL() ! record offset [var+rec]
     integer(kind=KIOFS),pointer :: rlen(:) => NULL() ! record size   [var+rec]
     integer lrec
  end type cache_t
!!!_  - static
  integer,save :: cache_rev = 0
  integer,parameter :: nloc = 5, nspec = 9
  character(len=litem),save :: DSET_specials(nspec) = &
       & (/ 'AXLOC  ', 'IAXLOC ', 'CAXLOC ', 'CIAXLOC', 'CCAXLOC', &
       &    'AXWGT  ', 'IAXWGT ', 'CAXWGT ', 'CIAXWGT'  /)
  ! CCAXLOC is used in DIUR02 of official gtool, might be a bug.
  ! DSET_specials(1:nloc) corresponds to LOC, otherwise WGT.

  character(len=4),save :: dup_sep = '~'
!!!_  - controls
  integer,save :: hh_cache = -1
  type(cache_t),allocatable,save,target :: ctables(:)
  integer,save :: nctab=0, lctab=0
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = NIO_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
#define __MDL__ 'c'
#define _ERROR(E) (E - ERR_MASK_NIO_CACHE)
!!!_  - interfaces
  interface show_cache
     module procedure show_cache_t, show_cache_h
  end interface show_cache
  interface cache_get_attr
     module procedure cache_geti_attr_a, cache_getn_attr_a
     module procedure cache_geti_attr_f, cache_getn_attr_f
  end interface cache_get_attr

  interface cache_var_read
     module procedure cache_var_read_f
  end interface cache_var_read

  interface cache_rec_id
     module procedure cache_rec_id_d
  end interface cache_rec_id
!!!_  - public procedures
  public init, diag, finalize
  public init_group
  public show_cache
  public cache_scan_file, cache_settle
  public cache_store_v0
  public cache_open_read, cache_groups,   cache_vars
  public cache_close
  public cache_group_recs
  public cache_var_name,  cache_var_nco, cache_var_id
  public cache_co_name,   cache_co_size, cache_co_idx
  public cache_var_read
  public cache_rec_id
  public cache_get_attr
!!!_  - public shared
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init &
       & (ierr, u, levv, mode, stdv, icomm, ncache, sep)
    use TOUZA_Nio_std,   only: control_mode,  control_deep, is_first_force
    use TOUZA_Nio_std,   only: ns_init=>init, choice, get_size_bytes, KDBL
    use TOUZA_Nio_header,only: nh_init=>init
    use TOUZA_Nio_record,only: nr_init=>init
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv, mode, stdv
    integer,         intent(in),optional :: icomm
    integer,         intent(in),optional :: ncache
    character(len=*),intent(in),optional :: sep
    integer lv, md, lmd

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
          if (ierr.eq.0) call ns_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
          if (ierr.eq.0) call nh_init(ierr, u=ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_init(ierr, u=ulog, levv=lv, mode=lmd)
       endif
       if (present(sep)) then
          dup_sep = sep
       endif
       if (init_counts.eq.0) then
          if (ierr.eq.0) call init_table(ierr, ncache)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Nio_std,   only: control_mode,  control_deep, is_first_force
    use TOUZA_Nio_std,   only: ns_diag=>diag, choice, msg, is_msglev_normal, is_msglev_info
    use TOUZA_Nio_header,only: nh_diag=>diag
    use TOUZA_Nio_record,only: nr_diag=>diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __MDL__, u=utmp)
             if (is_msglev_normal(lv)) call msg('(''duplicate separator = '', A)', (/dup_sep/), __MDL__, utmp)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ns_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nh_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_diag(ierr, utmp, levv=lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Nio_std,   only: control_mode,  control_deep, is_first_force
    use TOUZA_Nio_std,   only: ns_finalize=>finalize, choice
    use TOUZA_Nio_header,only: nh_finalize=>finalize
    use TOUZA_Nio_record,only: nr_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call nh_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call ns_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_finalize(ierr, utmp, levv=lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize
!!!_  - init subcontracts
!!!_ + controls
!!!_  - init_table - hash table initialization
  subroutine init_table &
       & (ierr, n)
    use TOUZA_Nio_std,only: choice, new_htable
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: n
    integer m

    ierr = 0
    lctab = choice(0, n)
    if (lctab.le.0) lctab = OPT_NIO_CACHES_SIZE
    m = lctab * 2
    if (hh_cache.lt.0) then
       hh_cache = new_htable('nio-cache', m, lkey=0, nkey=1)
       ierr = min(0, hh_cache)
    endif
    if (ierr.eq.0) then
       nctab = 0
       allocate(ctables(0:lctab-1), STAT=ierr)
    endif
    return
  end subroutine init_table
!!!_  - cache_open_read
  subroutine cache_open_read &
       & (ierr, handle, path, flag, unit)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_std,only: reg_entry, new_unit, sus_open
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: handle
    character(len=*),intent(in)  :: path
    integer,         intent(in)  :: flag
    integer,optional,intent(in)  :: unit
    integer u
    integer jc

    ierr = 0
    u = choice(-1, unit)
    if (u.lt.0) u = new_unit()
    ierr = min(0, u)
    handle = -1
    jc = nctab
    if (ierr.eq.0) then
       nctab = nctab + 1
       if (nctab.ge.lctab) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
    if (ierr.eq.0) then
       ierr = reg_entry(hh_cache, ikey=(/u/), status=(/jc/))
       ierr = min(0, ierr)
    endif
    if (ierr.eq.0) then
       call sus_open(ierr, u, path, ACTION='R', STATUS='O')
    endif
    if (ierr.eq.0) call cache_scan_file(ierr, ctables(jc), u)
    if (ierr.eq.0) call cache_settle(ierr, ctables(jc))

    if (ierr.eq.0) handle = u

  end subroutine cache_open_read

!!!_  - cache_close
  subroutine cache_close &
       & (ierr, handle, path)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: handle
    character(len=*),intent(in),optional :: path

    integer ufile
    integer jc
    ierr = 0

    jc = cache_is_valid(handle)
    ierr = min(0, jc)
    if (ierr.eq.0) then
       ufile = cache_h2unit(handle)
       close(unit=ufile, IOSTAT=ierr)
    endif
    if (ierr.eq.0) call free_cache(ierr, ctables(jc))
  end subroutine cache_close

!!!_  - cache_h2index()
  integer function cache_h2index(handle) result(j)
    use TOUZA_Nio_std,only: query_status
    implicit none
    integer,intent(in) :: handle
    integer jerr
    call query_status(jerr, j, hh_cache, ikey=(/handle/))
    if (jerr.lt.0) j = jerr
  end function cache_h2index

!!!_  - cache_h2unit() - identical
  integer function cache_h2unit(handle) result(u)
    implicit none
    integer,intent(in) :: handle
    u = handle
  end function cache_h2unit
!!!_  - cache_groups()
  integer function cache_groups(handle) result(n)
    implicit none
    integer,intent(in)  :: handle
    integer jc
    jc = cache_h2index(handle)
    n = min(0, jc)
    if (n.eq.0) n = ctables(jc)%ngrp
  end function cache_groups
!!!_  - cache_group_recs()
  integer function cache_group_recs(handle, gid) result(n)
    implicit none
    integer,intent(in) :: handle
    integer,intent(in) :: gid
    integer jc
    jc = cache_is_valid(handle, gid)
    n = min(0, jc)
    if (n.eq.0) then
       if (gid.eq.gid_suite) then
          n = _ERROR(ERR_INVALID_PARAMETER)
       else
          n = ctables(jc)%g(gid)%nrec
       endif
    endif
  end function cache_group_recs
!!!_  - cache_vars(handle, gid) - return number of variables in a group or total
  integer function cache_vars(handle, gid) result(n)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,intent(in)          :: handle
    integer,intent(in),optional :: gid
    integer jc
    integer jg

    jg = choice(gid_suite, gid)
    if (jg.eq.gid_suite) then
       jc = cache_is_valid(handle)
       n = min(0, jc)
       if (n.eq.0) then
          n = ctables(jc)%o(ctables(jc)%ngrp)
       else
          n = _ERROR(ERR_INVALID_ITEM)
       endif
    else
       jc = cache_is_valid(handle, gid)
       n = min(0, jc)
       if (n.eq.0) then
          n = ctables(jc)%g(gid)%nvar
       else
          n = _ERROR(ERR_INVALID_ITEM)
       endif
    endif
  end function cache_vars

!!!_  - cache_var_name - return variable name corresponding to group/var id
  subroutine cache_var_name(ierr, name, handle, gid, vid)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: name
    integer,         intent(in)  :: handle
    integer,         intent(in)  :: gid
    integer,         intent(in)  :: vid
    integer jc, jv

    jv = cache_gv2vindex(handle, gid, vid)
    ierr = min(0, jv)
    if (ierr.eq.0) then
       jc = cache_h2index(handle)
       name = ctables(jc)%v(jv)%item
    else
       name = ' '
    endif
  end subroutine cache_var_name

!!!_  - cache_var_id - return variable id corresponding to name
  integer function cache_var_id(name, handle, gid, init) result(vid)
    use TOUZA_Nio_std,only: choice
    implicit none
    character(len=*),intent(in) :: name
    integer,         intent(in) :: handle
    integer,         intent(in) :: gid
    integer,optional,intent(in) :: init     ! for variable duplication
    integer jerr
    integer jc, jvo, jvb, jve

    ! Search first occurence of variable NAME from INIT.
    ! If strict matching is desired, set NAME as {NAME // trim (dup_sep) // index},
    ! e.g., PRCP~2.

    jc = cache_is_valid(handle, gid)
    jerr = min(0, jc)

    if (jerr.eq.0) then
       if (gid.eq.gid_suite) then
          jvo = 0
          jve = ctables(jc)%o(ctables(jc)%ngrp)
          jvb = choice(jvo, init)
       else
          jvo = ctables(jc)%o(gid)
          jve = ctables(jc)%o(gid + 1)
          jvb = jvo + choice(0, init)
       endif
       vid = var_query_id(ctables(jc)%v, name, jvb, jve)
       if (vid.ge.0) vid = vid - jvo
    else
       vid = jerr
    endif
  end function cache_var_id

!!!_  - cache_rec_id - return record id filtered by time range
  integer function cache_rec_id_d &
       & (handle, gid, vid, timel, timeh, func, init) result(ridx)
    use TOUZA_Nio_std,only: KTGT=>KDBL
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,         intent(in) :: handle
    integer,         intent(in) :: gid
    integer,         intent(in) :: vid
    real(kind=KTGT), intent(in) :: timel, timeh
    integer,optional,intent(in) :: init     ! for variable duplication
    interface
       logical function func(dstr, tstr, timel, timeh)
         use TOUZA_Nio_std,only: KTGT=>KDBL
         implicit none
         character(len=*),intent(in) :: dstr ! date string
         character(len=*),intent(in) :: tstr ! time string
         real(kind=KTGT), intent(in) :: timel, timeh
       end function func
    end interface

    integer jc, jg, jv, jr
    integer jerr
    integer rbgn, rend, rofs

    ridx = -1
    jc = cache_is_valid(handle, gid)
    jerr = min(0, jc)
    if (jerr.eq.0) then
       jg = cache_gv2gindex(handle, gid, vid)
       jv = cache_gv2vindex(handle, gid, vid)
       jerr = min(0, jg, jv)
    endif
    if (jerr.eq.0) then
       rofs = cache_gvr2rindex(handle, gid, vid, 0)
       rbgn = choice(rofs, init) - rofs
       rend = ctables(jc)%g(jg)%nrec
       do jr = rbgn, rend
          if (func(ctables(jc)%g(jg)%d(jr), ctables(jc)%g(jg)%t(jr), timel, timeh)) then
             ridx = jr
             exit
          endif
       enddo
    endif
    if (jerr.eq.0) then
       if (ridx.ge.0) ridx = ridx + rofs
    endif
  end function cache_rec_id_d
!!!_  - cache_var_nco - return number of effective dimensions
  integer function cache_var_nco(handle, gid, vid) result(ndim)
    implicit none
    integer,intent(in)  :: handle
    integer,intent(in)  :: gid
    integer,intent(in)  :: vid
    integer jc, jv
    integer jerr

    jv = cache_gv2vindex(handle, gid, vid)
    jerr = min(0, jv)
    if (jerr.eq.0) then
       jc = cache_h2index(handle)
       ndim = ctables(jc)%v(jv)%neff
    else
       ndim = jerr
    endif
  end function cache_var_nco

!!!_  - cache_co_name - return coordinate name
  subroutine cache_co_name(ierr, name, handle, gid, vid, cid)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: name
    integer,         intent(in)  :: handle
    integer,         intent(in)  :: gid
    integer,         intent(in)  :: vid
    integer,         intent(in)  :: cid
    integer jc, jeff, jv
    type(var_t),pointer :: v

    jv = cache_gv2vindex(handle, gid, vid)
    v => NULL()
    ierr = min(0, jv)
    if (ierr.eq.0) then
       jc = cache_h2index(handle)
       v => ctables(jc)%v(jv)
       if (cid.lt.0.or.cid.ge.v%neff) ierr = _ERROR(ERR_INVALID_ITEM)
    endif
    if (ierr.eq.0) then
       jeff = v%ceff(cid)
       if (jeff.lt.0) ierr = _ERROR(ERR_PANIC)
    endif
    if (ierr.eq.0) then
       name = v%co(jeff)
    endif
  end subroutine cache_co_name

!!!_  - cache_co_size - return coordinate size
  integer function cache_co_size(handle, gid, vid, cid) result(nerr)
    implicit none
    integer,         intent(in)  :: handle
    integer,         intent(in)  :: gid
    integer,         intent(in)  :: vid
    integer,         intent(in)  :: cid
    integer jc, jeff, jv
    type(var_t),pointer :: v

    jv = cache_gv2vindex(handle, gid, vid)
    v => NULL()
    nerr = min(0, jv)
    if (nerr.eq.0) then
       jc = cache_h2index(handle)
       v => ctables(jc)%v(jv)
       if (cid.lt.0.or.cid.ge.v%neff) nerr = _ERROR(ERR_INVALID_ITEM)
    endif
    if (nerr.eq.0) then
       jeff = v%ceff(cid)
       if (jeff.lt.0) nerr = _ERROR(ERR_PANIC)
    endif
    if (nerr.eq.0) then
       nerr = max(0, v%jend(jeff) - v%jbgn(jeff))
    endif
  end function cache_co_size

!!!_  - cache_co_idx - return coordinate index
  integer function cache_co_idx(handle, gid, vid, name) result(jeff)
    implicit none
    integer,         intent(in)  :: handle
    integer,         intent(in)  :: gid
    integer,         intent(in)  :: vid
    character(len=*),intent(in)  :: name
    integer jerr
    integer jc, jx, jv
    type(var_t),pointer :: v
    integer j

    jeff = -1
    v => NULL()
    jv = cache_gv2vindex(handle, gid, vid)
    jerr = min(0, jv)
    if (jerr.eq.0) then
       jc = cache_h2index(handle)
       v => ctables(jc)%v(jv)
       do j = 0, v%neff - 1
          jx = v%ceff(j)
          if (name.eq.v%co(jx)) then
             jeff = j
             return
          endif
       enddo
    endif
  end function cache_co_idx

!!!_  - cache_get_attr
  subroutine cache_geti_attr_a(ierr, attr, item, handle, gid, vid, rec)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_std,only: WHENCE_BEGIN
    use TOUZA_Nio_header,only: get_item
    use TOUZA_Nio_record,only: nio_read_header
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: attr
    integer,         intent(in)  :: item
    integer,         intent(in)  :: handle
    integer,optional,intent(in)  :: gid
    integer,optional,intent(in)  :: vid
    integer,optional,intent(in)  :: rec
    character(len=litem) :: h(nitem)
    integer jc, ridx
    integer ufile, krect
    integer(kind=KIOFS) rpos

    jc = cache_is_valid(handle, gid, vid)
    ierr = min(0, jc)
    if (ierr.eq.0) then
       if (.not.present(gid)) ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    endif
    if (ierr.eq.0) then
       if (present(vid)) then
          ufile = cache_h2unit(handle)
          ridx = cache_gvr2rindex(handle, gid, vid, choice(0, rec))
          ierr = min(0, ridx)
          if (ierr.eq.0) then
             rpos = ctables(jc)%rpos(ridx)
             call nio_read_header(ierr, h, krect, ufile, rpos, WHENCE_BEGIN)
             if (ierr.eq.0) call get_item(ierr, h, attr, item)
          endif
       else if (gid.eq.gid_suite) then
          ierr = _ERROR(ERR_INVALID_PARAMETER)
       else
          call get_item(ierr, ctables(jc)%g(gid)%h, attr, item)
       endif
    endif
  end subroutine cache_geti_attr_a
  subroutine cache_getn_attr_a(ierr, attr, item, handle, gid, vid, rec)
    use TOUZA_Nio_std,only: choice, WHENCE_BEGIN
    use TOUZA_Nio_header,only: get_item
    use TOUZA_Nio_record,only: nio_read_header
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: attr
    character(len=*),intent(in)  :: item
    integer,         intent(in)  :: handle
    integer,optional,intent(in)  :: gid
    integer,optional,intent(in)  :: vid
    integer,optional,intent(in)  :: rec
    integer jc, ridx
    character(len=litem) :: h(nitem)
    integer ufile, krect
    integer(kind=KIOFS) rpos

    jc = cache_is_valid(handle, gid, vid)
    ierr = min(0, jc)
    if (ierr.eq.0) then
       if (.not.present(gid)) ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    endif
    if (ierr.eq.0) then
       if (present(vid)) then
          ufile = cache_h2unit(handle)
          ridx = cache_gvr2rindex(handle, gid, vid, choice(0, rec))
          ierr = min(0, ridx)
          if (ierr.eq.0) then
             rpos = ctables(jc)%rpos(ridx)
             call nio_read_header(ierr, h, krect, ufile, rpos, WHENCE_BEGIN)
             if (ierr.eq.0) call get_item(ierr, h, attr, item)
          endif
       else if (gid.eq.gid_suite) then
          ierr = _ERROR(ERR_INVALID_PARAMETER)
       else
          call get_item(ierr, ctables(jc)%g(gid)%h, attr, item)
       endif
    endif
  end subroutine cache_getn_attr_a

  subroutine cache_geti_attr_f(ierr, attr, item, handle, gid, vid, rec)
    use TOUZA_Nio_std,only: KTGT=>KFLT
    use TOUZA_Nio_std,only: choice, WHENCE_BEGIN
    use TOUZA_Nio_header,only: get_item
    use TOUZA_Nio_record,only: nio_read_header
    implicit none
    integer,         intent(out) :: ierr
    real(kind=KTGT), intent(out) :: attr
    integer,         intent(in)  :: item
    integer,         intent(in)  :: handle
    integer,optional,intent(in)  :: gid
    integer,optional,intent(in)  :: vid
    integer,optional,intent(in)  :: rec
    character(len=litem) :: h(nitem)
    integer jc, ridx
    integer ufile, krect
    integer(kind=KIOFS) rpos

    jc = cache_is_valid(handle, gid, vid)
    ierr = min(0, jc)
    if (ierr.eq.0) then
       if (.not.present(gid)) ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    endif
    if (ierr.eq.0) then
       if (present(vid)) then
          ufile = cache_h2unit(handle)
          ridx = cache_gvr2rindex(handle, gid, vid, choice(0, rec))
          ierr = min(0, ridx)
          if (ierr.eq.0) then
             rpos = ctables(jc)%rpos(ridx)
             call nio_read_header(ierr, h, krect, ufile, rpos, WHENCE_BEGIN)
             if (ierr.eq.0) call get_item(ierr, h, attr, item)
          endif
       else if (gid.eq.gid_suite) then
          ierr = _ERROR(ERR_INVALID_PARAMETER)
       else
          call get_item(ierr, ctables(jc)%g(gid)%h, attr, item)
       endif
    endif
  end subroutine cache_geti_attr_f
  subroutine cache_getn_attr_f(ierr, attr, item, handle, gid, vid, rec)
    use TOUZA_Nio_std,only: KTGT=>KFLT
    use TOUZA_Nio_std,only: choice, WHENCE_BEGIN
    use TOUZA_Nio_header,only: get_item
    use TOUZA_Nio_record,only: nio_read_header
    implicit none
    integer,         intent(out) :: ierr
    real(kind=KTGT), intent(out) :: attr
    character(len=*),intent(in)  :: item
    integer,         intent(in)  :: handle
    integer,optional,intent(in)  :: gid
    integer,optional,intent(in)  :: vid
    integer,optional,intent(in)  :: rec
    integer jc, ridx
    character(len=litem) :: h(nitem)
    integer ufile, krect
    integer(kind=KIOFS) rpos

    jc = cache_is_valid(handle, gid, vid)
    ierr = min(0, jc)
    if (ierr.eq.0) then
       if (.not.present(gid)) ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    endif
    if (ierr.eq.0) then
       if (present(vid)) then
          ufile = cache_h2unit(handle)
          ridx = cache_gvr2rindex(handle, gid, vid, choice(0, rec))
          ierr = min(0, ridx)
          if (ierr.eq.0) then
             rpos = ctables(jc)%rpos(ridx)
             call nio_read_header(ierr, h, krect, ufile, rpos, WHENCE_BEGIN)
             if (ierr.eq.0) call get_item(ierr, h, attr, item)
          endif
       else if (gid.eq.gid_suite) then
          ierr = _ERROR(ERR_INVALID_PARAMETER)
       else
          call get_item(ierr, ctables(jc)%g(gid)%h, attr, item)
       endif
    endif
  end subroutine cache_getn_attr_f

!!!_  - cache_is_valid()
  integer function cache_is_valid (handle, gid, vid) result(jc)
    implicit none
    integer,intent(in)          :: handle
    integer,intent(in),optional :: gid
    integer,intent(in),optional :: vid
    jc = cache_h2index(handle)
    if (jc.lt.0) return
    if (.not.present(gid)) return
    if (gid.eq.gid_suite) then
       if (.not.present(vid)) return
       if (vid.lt.0.or.vid.ge.ctables(jc)%o(ctables(jc)%ngrp)) jc = _ERROR(ERR_INVALID_ITEM)
    else
       if (gid.lt.0.or.gid.ge.ctables(jc)%ngrp) jc = _ERROR(ERR_INVALID_ITEM)
       if (jc.lt.0) return
       if (.not.present(vid)) return
       if (vid.lt.0.or.vid.ge.ctables(jc)%g(gid)%nvar) jc = _ERROR(ERR_INVALID_ITEM)
    endif
  end function cache_is_valid

!!!_ + variable type procedures
!!!_  - var_query_id
  integer function var_query_id(var, name, jbgn, jend) result(vid)
    use TOUZA_Nio_std,only: choice
    implicit none
    type(var_t),     intent(in) :: var(0:*)
    character(len=*),intent(in) :: name
    integer,         intent(in) :: jbgn, jend
    integer jv

    integer jp
    integer lsep, lname

    ! Search first occurence of variable NAME from INIT.
    ! If strict matching is desired, set NAME as {NAME // trim (dup_sep) // index},
    ! e.g., PRCP~2.

    lsep = max(1, len_trim(dup_sep))
    lname = len_trim(name)
    jp = index(name(1:lname), dup_sep(1:lsep))
    if (jp.gt.0) then
       ! strict matching
       do jv = jbgn, jend - 1
          if (var(jv)%item.eq.name(1:lname)) then
             vid = jv
             return
          endif
       enddo
    else
       ! loose matching
       do jv = jbgn, jend - 1
          if (var(jv)%item(1:lname).eq.name(1:lname)) then
             if (var(jv)%item(lname+1:).eq.' ' &
                  & .or. var(jv)%item(lname+1:).eq.dup_sep(1:lsep)) then
                vid = jv
                return
             endif
          endif
       enddo
    endif
    vid = _ERROR(ERR_INVALID_PARAMETER)
  end function var_query_id

!!!_ + derived-type managers
!!!_  - init_group
  subroutine init_group (ierr, grp, recs, vars)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,      intent(out)         :: ierr
    type(group_t),intent(inout)       :: grp
    integer,      intent(in),optional :: recs, vars
    integer lr
    integer lv

    ierr = 0
    grp%nvar = 0
    grp%h(:) = ' '
    grp%g    = ' '
    grp%nrec = 0
    ! lv = choice(16, vars) ! hard-coded
    ! lr = choice(12, recs) ! hard-coded
    lv = choice(1, vars) ! hard-coded
    lr = choice(1, recs) ! hard-coded
    allocate(grp%v(0:lv-1), &
         &   grp%d(0:lr-1),           grp%t(0:lr-1), &
         &   grp%rpos(0:lr-1,0:lv-1), grp%rlen(0:lr-1,0:lv-1), &
         &   STAT=ierr)
  end subroutine init_group
!!!_  - add_members
  subroutine add_members &
       & (ierr, grp, recs, vars)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,      intent(out)         :: ierr
    type(group_t),intent(inout)       :: grp
    integer,      intent(in),optional :: recs   ! number of records to add
    integer,      intent(in),optional :: vars   ! number of variables to add
    integer mr, nr, lr
    integer mv, nv, lv
    type(var_t),pointer :: tmpv(:)
    character(len=litem),pointer :: tmpd(:),   tmpt(:)
    integer(kind=KIOFS),pointer  :: tmpo(:,:), tmpl(:, :)

    ierr = 0
    mr = choice(0, recs)
    mv = choice(0, vars)
    nv = size(grp%v)
    lv = nv + mv
    if (mv.gt.0) then
       allocate(tmpv(0:lv-1), STAT=ierr)
       if (ierr.eq.0) then
          tmpv(0:nv-1) = grp%v(0:nv-1)
          deallocate(grp%v, STAT=ierr)
       endif
       if (ierr.eq.0) grp%v => tmpv
    endif
    if (mv.gt.0.or.mr.gt.0) then
       nr = size(grp%d)
       lr = nr + mr
       allocate(tmpd(0:lr-1), tmpt(0:lr-1), tmpo(0:lr-1,0:lv-1), tmpl(0:lr-1,0:lv-1), &
            &   STAT=ierr)
       if (ierr.eq.0) then
          tmpd(0:nr-1) = grp%d(0:nr-1)
          tmpt(0:nr-1) = grp%t(0:nr-1)
          tmpo(0:nr-1, 0:nv-1) = grp%rpos(0:nr-1, 0:nv-1)
          tmpl(0:nr-1, 0:nv-1) = grp%rlen(0:nr-1, 0:nv-1)

          tmpo(nr:lr-1, 0:lv-1) = -1
          tmpl(nr:lr-1, 0:lv-1) = -1
          tmpo(0:lr-1, nv:lv-1) = -1
          tmpl(0:lr-1, nv:lv-1) = -1
          deallocate(grp%d, grp%t, grp%rpos, grp%rlen, STAT=ierr)
       endif
       if (ierr.eq.0) then
          grp%d => tmpd
          grp%t => tmpt
          grp%rpos => tmpo
          grp%rlen => tmpl
       endif
    endif
  end subroutine add_members
!!!_  - add_record
  subroutine add_record &
       & (ierr, grp, d, t, o, jvar)
    implicit none
    integer,            intent(out)   :: ierr
    type(group_t),      intent(inout) :: grp
    character(len=*),   intent(in)    :: d
    character(len=*),   intent(in)    :: t
    integer(kind=KIOFS),intent(in)    :: o
    integer,            intent(in)    :: jvar
    integer jr, mr
    integer jtgt

    ierr = 0
    mr = 16
    jtgt = -1
    do jr = 0, grp%nrec - 1
       if (grp%d(jr).eq.d.and.grp%t(jr).eq.t) then
          jtgt = jr
          exit
       endif
    enddo
    if (jtgt.lt.0) then
       jtgt = max(0, grp%nrec)
       grp%nrec = grp%nrec + 1
       if (jtgt.ge.size(grp%d)) call add_members(ierr, grp, recs=mr)
       if (ierr.eq.0) then
          grp%d(jtgt) = d
          grp%t(jtgt) = t
       endif
    endif
    if (ierr.eq.0) then
       grp%rpos(jtgt, jvar) = o
       grp%rlen(jtgt, jvar) = 0  ! wait
    endif
  end subroutine add_record

!!!_ + cache
!!!_  & show_cache
  subroutine show_cache_t(ierr, c, tag, u, levv)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    type(cache_t),   intent(in)          :: c
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer utmp, lv
    integer jg
    integer jvb, jve
    character(len=128) :: ttmp
    ierr = 0
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)
101 format(A, '/', I0)
102 format('group/', I0)
    do jg = 0, c%ngrp - 1
       if (present(tag)) then
          write(ttmp, 101) trim(tag), jg
       else
          write(ttmp, 102) jg
       endif
       jvb = c%o(jg)
       jve = c%o(jg+1)
       if (ierr.eq.0) then
          call show_group &
               & (ierr, c%g(jg), c%v(jvb:jve-1), c%rpos, c%rlen, ttmp, utmp, lv)
       endif
    enddo
  end subroutine show_cache_t

  subroutine show_cache_h(ierr, handle, tag, u, levv)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: handle
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer jc
    character(len=128)   :: ttmp
    character(len=lpath) :: file
    integer ufile, utmp
    ierr = 0
    utmp = get_logu(u, ulog)
    jc = cache_h2index(handle)
    ufile = cache_h2unit(handle)
    ierr = min(0, jc)
    if (ierr.eq.0) then
       if (present(tag)) then
          ttmp = tag
       else
101       format('cache[', I0, ']')
          write(ttmp, 101) handle
       endif
    endif
    if (ierr.eq.0) then
       inquire(UNIT=ufile, NAME=file, IOSTAT=ierr)
       if (ierr.ne.0) file = '(unknown)'
       ierr = 0
102    format(A, 1x, A)
       if (utmp.ge.0) then
          write(utmp, 102) trim(ttmp), trim(file)
       else if (utmp.eq.-1) then
          write(*,    102) trim(ttmp), trim(file)
       endif
    endif
    if (ierr.eq.0) then
       call show_cache(ierr, ctables(jc), ttmp, utmp, levv)
    endif
  end subroutine show_cache_h

!!!_  & show_group
  subroutine show_group &
       & (ierr, grp, var, rpos, rlen, tag, u, levv)
    use TOUZA_Nio_std,only: choice, join_list, is_msglev_DETAIL, is_msglev_INFO
    implicit none
    integer,            intent(out)         :: ierr
    type(group_t),      intent(in)          :: grp
    type(var_t),        intent(in)          :: var(0:*)
    integer(kind=KIOFS),intent(in)          :: rpos(0:*)
    integer(kind=KIOFS),intent(in)          :: rlen(0:*)
    character(len=*),   intent(in),optional :: tag
    integer,            intent(in),optional :: u
    integer,            intent(in),optional :: levv
    integer utmp, lv
    integer jvb, jve, nv, jvi, jv
    integer,parameter :: lcol = litem + 16
    integer,parameter :: mv = 4
    integer,parameter :: lline = lcol * (mv + 4)
    character(len=128) :: ttmp
    character(len=lcol) :: cbufs(0:mv-1)
    character(len=lline) :: line
    character(len=litem*2+1) :: dt
    integer jr, jc
    integer rofs

    ierr = 0
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)
101 format(A, 1x, I0, ':', I0, 1x, A)
102 format(A, 1x, 'F', 1x, A)
211 format(A, 1x, I0, ' [', A, '] ', A)
201 format(Z8.8, '+', Z0)
202 format(A, 1x, A)
111 format(A, '/', I0, ':', I0)
121 format(A, 1x, I0, 1x, A)
131 format(A, 1x, '<', A, '>', 1x, I0)
    ! write(*, *) grp%nvar, grp%nrec
    if (present(tag)) then
       ttmp = tag
    else
       ttmp = 'cache'
    endif
    if (is_msglev_DETAIL(levv-1)) then
       if (ierr.eq.0) call show_header(ierr, grp%h, tag=ttmp, u=utmp, lev=levv)
    endif
    if (ierr.eq.0) then
       if (utmp.ge.0) then
          write(utmp, 131) trim(ttmp), trim(grp%g), grp%nvar
       else if (utmp.eq.-1) then
          write(*,    131) trim(ttmp), trim(grp%g), grp%nvar
       endif
    endif
    do jvb = 0, grp%nvar - 1, mv
       jve = min(jvb + mv, grp%nvar)
       nv = jve - jvb
141    format(A, '[', I0, ']')
       do jv = 0, nv - 1
          write(cbufs(jv),141) trim(var(jvb+jv)%item), var(jvb+jv)%neff
       enddo
       if (ierr.eq.0) call join_list(ierr, line, cbufs(0:nv-1))
       if (ierr.eq.0) then
          if (utmp.ge.0) then
             write(utmp, 101) trim(ttmp), jvb, jve, trim(line)
          else if (utmp.eq.-1) then
             write(*,    101) trim(ttmp), jvb, jve, trim(line)
          endif
       endif
       if (ierr.eq.0) then
          do jv = 0, nv - 1
             cbufs(jv) = var(jvb+jv)%dfmt
          enddo
       endif
       if (ierr.eq.0) call join_list(ierr, line, cbufs(0:nv-1))
       if (ierr.eq.0) then
          if (utmp.ge.0) then
             write(utmp, 102) trim(ttmp), trim(line)
          else if (utmp.eq.-1) then
             write(*,    102) trim(ttmp), trim(line)
          endif
       endif
       do jc = 0, lax - 1
          if (ierr.eq.0) then
             do jvi = 0, nv - 1
                write(cbufs(jvi), 111) &
                     & trim(var(jvb+jvi)%co(jc)), var(jvb+jvi)%jbgn(jc), var(jvb+jvi)%jend(jc)
             enddo
             call join_list(ierr, line, cbufs(0:nv-1))
          endif
          if (ierr.eq.0) then
             if (utmp.ge.0) then
                write(utmp, 121) trim(ttmp), jc, trim(line)
             else if (utmp.eq.-1) then
                write(*, 121) trim(ttmp), jc, trim(line)
             endif
          endif
       enddo
       if (is_msglev_DETAIL(lv)) then
          if (ierr.eq.0) then
             do jr = 0, grp%nrec - 1
                do jvi = 0, nv - 1
                   rofs = var(jvb+jvi)%rofs
                   write(cbufs(jvi), 201) rpos(rofs + jr), rlen(rofs + jr)
                enddo
                if (ierr.eq.0) call join_list(ierr, line, cbufs(0:nv-1))
                write(dt, 202) trim(adjustl(grp%t(jr))), trim(adjustl(grp%d(jr)))
                if (utmp.ge.0) then
                   write(utmp, 211) trim(ttmp), jr, trim(dt), trim(line)
                else if (utmp.eq.-1) then
                   write(*,    211) trim(ttmp), jr, trim(dt), trim(line)
                endif
             enddo
          endif
       endif
    enddo

  end subroutine show_group

!!!_  & cache_scan_file
  subroutine cache_scan_file &
       & (ierr, c, u, flag, levv)
    use TOUZA_Nio_std,only: choice, condop, is_error_match
    use TOUZA_Nio_std,only: msg, is_msglev_DETAIL, is_msglev_INFO
    use TOUZA_Nio_std,only: sus_pos_a2rel, WHENCE_BEGIN
    use TOUZA_Nio_record,only: nio_read_header, nio_skip_records
    implicit none
    integer,         intent(out)   :: ierr
    type(cache_t),   intent(inout) :: c
    integer,         intent(in)    :: u
    integer,optional,intent(in)    :: flag
    integer,optional,intent(in)    :: levv

    integer jdrec
    integer lv
    integer jg, lg, ng, mg
    integer jv, mv, mvdef
    integer jr, mr, mrdef
    logical newv, newr
    type(group_t),pointer :: grp(:), gtmp(:)
    character(len=litem) :: h(nitem)
    integer rect
    integer j
    integer(kind=KIOFS) :: apos, rpos, msize

    ierr = 0
    lv = choice(lev_verbose, levv)

    ng = 0
    mg = 8
    lg = 24
    mvdef = 16
    mrdef = 12
    jr = -1
    jv = -1
    newr = .FALSE.
    jg = -1
    jdrec = 0
    rpos = -1

    if (ierr.eq.0) rewind(UNIT=u, IOSTAT=ierr)
    if (ierr.eq.0) allocate(grp(0:lg-1), STAT=ierr)

    do
       if (ierr.ne.0) exit
       if (ierr.eq.0) inquire(UNIT=u, POS=apos, IOSTAT=ierr)
       if (ierr.eq.0) rpos = sus_pos_a2rel(apos, u, WHENCE_BEGIN)
       if (ierr.eq.0) call nio_read_header(ierr, h, rect, u)
       if (is_msglev_DETAIL(lv)) then
          if (ierr.eq.0) &
               & call msg('(''record = '', I0, 1x, I0)', (/jdrec, int(rpos)/), __MDL__)
       endif
       if (ierr.eq.0) call collate_header(ierr, jg, grp, ng, h, flag)
       if (ierr.eq.0) then
          if (jg.lt.0) then
             jg = ng
             if (ng.ge.lg) then
                ! write(*, *) jg, mg, ng, lg
                lg = lg + mg
                allocate(gtmp(0:lg-1), STAT=ierr)
                if (ierr.eq.0) then
                   gtmp(0:ng-1)%nvar = grp(0:ng-1)%nvar
                   gtmp(0:ng-1)%nrec = grp(0:ng-1)%nrec
                   gtmp(0:ng-1)%g    = grp(0:ng-1)%g
                   do j = 0, ng - 1
                      gtmp(j)%h(:) = grp(j)%h(:)
                      gtmp(j)%v => grp(j)%v
                      gtmp(j)%d => grp(j)%d
                      gtmp(j)%t => grp(j)%t
                      gtmp(j)%rpos => grp(j)%rpos
                      gtmp(j)%rlen => grp(j)%rlen
                   enddo
                   deallocate(grp, STAT=ierr)
                endif
                if (ierr.eq.0) grp => gtmp
             endif
             ng = ng + 1
             if (ierr.eq.0) call init_group(ierr, grp(jg))
             if (ierr.eq.0) grp(jg)%h(:) = h(:)
             if (ierr.eq.0) grp(jg)%g = h(hi_DSET)
             if (is_msglev_DETAIL(lv)) then
                call msg('(''group = '', I0)', (/jg/), __MDL__)
             endif
          endif
       endif
       if (ierr.eq.0) call nio_skip_records(ierr, 1, u, head=h, krect=rect)
       if (ierr.eq.0) inquire(UNIT=u, POS=msize, IOSTAT=ierr)
       if (ierr.eq.0) call group_search_var(ierr, jv, grp(jg), h)
       if (ierr.eq.0) call group_search_rec(ierr, jr, grp(jg), h)
       ! write(*, *) 'search', ierr, jg, jr, jv, grp(jg)%nrec, grp(jg)%nvar
       if (ierr.eq.0) then
          newv = jv.lt.0
          newr = jr.lt.0
          if (.not.newv .and. .not.newr) then
             if (grp(jg)%rpos(jr, jv).ge.0) then
                if (is_msglev_INFO(lv)) then
                   call msg('(''dup = '', I0, 1x, I0)', (/jv, jr/), __MDL__)
                endif
                newv = .true.
             endif
          endif
          if (newr) then
             jr = grp(jg)%nrec
             grp(jg)%nrec = grp(jg)%nrec + 1
             if (is_msglev_DETAIL(lv)) then
                call msg('(''rec = '', I0)', (/jr/), __MDL__)
             endif
          endif
          if (newv) then
             jv = grp(jg)%nvar
             grp(jg)%nvar = grp(jg)%nvar + 1
             if (is_msglev_DETAIL(lv)) then
                call msg('(''var = '', I0)', (/jv/), __MDL__)
             endif
          endif
          mv = condop(jv.ge.size(grp(jg)%v), mvdef, 0)
          mr = condop(jr.ge.size(grp(jg)%d), mrdef, 0)
          ! write(*, *) 'new', ierr, jg, jr, jv, newr, newv, mr, mv
          if (mr.gt.0.or.mv.gt.0) then
             call add_members(ierr, grp(jg), mr, mv)
          endif
       endif
       if (ierr.eq.0) then
          if (newv) call new_var(ierr, grp(jg), jv, h)
          ! write(*, *) grp(jg)%v(:)
       endif
       if (ierr.eq.0) then
          if (newr) call new_rec(ierr, grp(jg), jr, h)
       endif
       if (ierr.eq.0) then
          msize = msize - apos
       endif
       if (ierr.eq.0) then
          grp(jg)%rpos(jr, jv) = rpos
          grp(jg)%rlen(jr, jv) = msize
          ! write(*, *) 'scan', ierr, jg, jr, jv, jpos, msize
       endif
       jdrec = jdrec + 1
    enddo
    if (is_error_match(ierr, ERR_EOF)) ierr = 0
    if (ierr.eq.0) then
       c%ngrp = ng
       c%g => grp
    else
       c%ngrp = -1
       c%g => NULL()
    endif
  end subroutine cache_scan_file

!!!_  - copy_var
  subroutine copy_var(dist, src, n)
    implicit none
    type(var_t),intent(inout) :: dist(0:*)
    type(var_t),intent(in)    :: src(0:*)
    integer,    intent(in)    :: n
    integer j

    do j = 0, n - 1
       dist(j)%item    = src(j)%item
       dist(j)%unit    = src(j)%unit
       dist(j)%dfmt    = src(j)%dfmt
       dist(j)%co(:)   = src(j)%co(:)
       dist(j)%jbgn(:) = src(j)%jbgn(:)
       dist(j)%jend(:) = src(j)%jend(:)
       dist(j)%neff    = src(j)%neff
       dist(j)%ceff(:) = src(j)%ceff(:)
       dist(j)%flag    = src(j)%flag
       dist(j)%rofs    = src(j)%rofs
    enddo
  end subroutine copy_var

!!!_  - cache_settle
  subroutine cache_settle(ierr, c)
    implicit none
    integer,      intent(out)   :: ierr
    type(cache_t),intent(inout) :: c
    integer jg
    integer lv, jv, jvb, jve, nv
    integer lrec, jrb, jre, jro, nr

    ierr = 0

    ! rpos rlen
    lrec = 0
    do jg = 0, c%ngrp - 1
       lrec = lrec + max(0, c%g(jg)%nrec) * max(0, c%g(jg)%nvar)
    enddo
    if (ierr.eq.0) then
       allocate(c%rpos(0:lrec-1), c%rlen(0:lrec-1), STAT=ierr)
    endif
    if (ierr.eq.0) then
       c%lrec = lrec
       jro = 0
       do jg = 0, c%ngrp - 1
          nr = max(0, c%g(jg)%nrec)
          nv = max(0, c%g(jg)%nvar)
          do jv = 0, c%g(jg)%nvar - 1
             jrb = jv * nr
             jre = jrb + nr
             c%rpos(jro+jrb:jro+jre-1) = c%g(jg)%rpos(0:nr-1, jv)
             c%rlen(jro+jrb:jro+jre-1) = c%g(jg)%rlen(0:nr-1, jv)
             c%g(jg)%v(jv)%rofs = jro + jrb  ! set here and copied below
          enddo
          jro = jro + nv * nr
       enddo
    endif
    jvb = 0
    if (ierr.eq.0) then
       lv = 0
       do jg = 0, c%ngrp - 1
          lv = lv + max(0, c%g(jg)%nvar)
       enddo
       allocate(c%v(0:lv-1), c%o(0:lv), STAT=ierr)
    endif
    if (ierr.eq.0) c%o(0) = 0
    jvb = 0
    do jg = 0, c%ngrp - 1
       nv = max(0, c%g(jg)%nvar)
       jve = jvb + nv
       if (ierr.eq.0) call settle_group(ierr, c%g(jg), jvb, jg)
       if (ierr.eq.0) call copy_var(c%v(jvb:jve-1), c%g(jg)%v(0:nv-1), nv)
       if (ierr.eq.0) c%o(jg+1) = jve
       jvb = jve
    enddo
    ! release temporal variable properties
    do jg = 0, c%ngrp - 1
       if (ierr.eq.0) deallocate(c%g(jg)%v, c%g(jg)%rpos, c%g(jg)%rlen, STAT=ierr)
       !! ! Following works, but need to adjust index (starting from 1).
       ! if (ierr.eq.0) c%g(jg)%v => c%v(c%o(jg):)
       !! ! Requires Fortran 2003, reserved.
       ! if (ierr.eq.0) c%g(jg)%v(0:) => c%v(c%o(jg):)
       c%g(jg)%v => NULL()
       c%g(jg)%rpos => NULL()
       c%g(jg)%rlen => NULL()
    enddo
    ! After settlement:
    !   Entity of variable properties are stored in c%v(:)
    !   g(:)%v points to corresponding head of c%v(:)
  end subroutine cache_settle

!!!_  - settle_group
  subroutine settle_group(ierr, grp, jvoff, jgrp)
    use TOUZA_Nio_record,only: put_header_cprop
    use TOUZA_Nio_header,only: put_item, hi_ITEM, hi_DFMT, hi_TITL1, hi_ETTL1, hi_UNIT
    implicit none
    integer,      intent(out)   :: ierr
    type(group_t),intent(inout) :: grp
    integer,      intent(in)    :: jvoff
    integer,      intent(in)    :: jgrp
    integer jv,  jt,  jc
    integer jvb, jve
    integer jrb, jre
    integer jgb, jge
    integer jerr
    integer ls
    character(len=litem*2) :: str

    ierr = 0
    ls = max(1, len_trim(dup_sep))
    jvb = jvoff
    jve = jvb + grp%nvar
    if (ierr.eq.0) call put_header_cprop(ierr, grp%h, 'VARIABLES', (/jvb + 1, jve/), 1)
    jrb = 0
    jre = grp%nrec
    if (ierr.eq.0) call put_header_cprop(ierr, grp%h, 'RECORDS', (/jrb + 1, jre/), 2)
    if (ierr.eq.0) then
       jgb = jgrp
       jge = jgb + 1
       call put_header_cprop(ierr, grp%h, 'GROUPS', (/jgb + 1, jge/), 3)
    endif
    if (ierr.eq.0) call put_item(ierr, grp%h, ' ', hi_TITL1, 0)
    if (ierr.eq.0) call put_item(ierr, grp%h, ' ', hi_UNIT)
    if (ierr.eq.0) call put_item(ierr, grp%h, ' ', hi_ETTL1, 0)
    if (ierr.eq.0) then
101    format('CACHE', I0)
       write(str, 101) cache_rev
       call put_item(ierr, grp%h, str, hi_ITEM)
    endif
    if (ierr.eq.0) then
102    format('XA', I0)
       write(str, 102) ucache
       call put_item(ierr, grp%h, str, hi_DFMT)
    endif
201 format(A, A, I0)
    do jv = 0, grp%nvar - 1
       if (index(grp%v(jv)%item, dup_sep(1:ls)).gt.0) cycle
       if (ANY(grp%v(jv)%item.eq.grp%v(jv+1:grp%nvar-1)%item)) then
          jc = 0
          do jt = jv + 1, grp%nvar - 1
             if (grp%v(jv)%item.eq.grp%v(jt)%item) then
                jc = jc + 1
                write(grp%v(jt)%item, 201, IOSTAT=jerr) trim(grp%v(jv)%item), dup_sep(1:ls), jc
             endif
          enddo
          jc = 0
          write(str, 201, IOSTAT=jerr) trim(grp%v(jv)%item), dup_sep(1:ls), jc
          grp%v(jv)%item = trim(str)
       endif
    enddo
  end subroutine settle_group

!!!_  - collate_header
  subroutine collate_header &
       & (ierr, jgrp, grp, ngrp, head, flag, emask)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_header
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: jgrp
    type(group_t),   intent(in)  :: grp(0:*)
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: ngrp
    integer,optional,intent(in)  :: flag
    logical,optional,intent(in)  :: emask(*)

    logical :: msk(nitem)
    integer j
    integer f
#if TEST_NIO_CACHE > 1
    integer ji
#endif
    ierr = 0

    if (present(emask)) then
       msk(1:nitem) = emask(1:nitem)
    else
       f = choice(coll_default, flag)
       if (f.eq.coll_default) f = coll_nosign
       msk(:) = .FALSE.
       gen_mask: do
          if (IAND(f, coll_nospecial).eq.0) then
             if (ANY(head(hi_DSET).eq.DSET_specials(:))) then
                msk(hi_DSET) = .TRUE.
                exit gen_mask
             endif
          endif
          if (f.ge.coll_std) then
             msk(hi_DFMT) = .TRUE.
             msk(hi_ITEM) = .TRUE.
             msk(hi_DATE) = .TRUE.
             msk(hi_TIME) = .TRUE.
             msk(hi_TDUR) = .TRUE.
             msk(hi_TIME2) = .TRUE.
             msk(hi_UTIM2) = .TRUE.
             msk(hi_SIZE) = .TRUE.
             msk(hi_MISS) = .TRUE.
             msk(hi_AITM1) = .TRUE.
             msk(hi_AITM2) = .TRUE.
             msk(hi_AITM3) = .TRUE.
             msk(hi_ASTR1) = .TRUE.
             msk(hi_ASTR2) = .TRUE.
             msk(hi_ASTR3) = .TRUE.
             msk(hi_AEND1) = .TRUE.
             msk(hi_AEND2) = .TRUE.
             msk(hi_AEND3) = .TRUE.
          endif
          if (f.ge.coll_basic) then
             msk(hi_UNIT) = .TRUE.
             msk(hi_TITL1:hi_TITL2) = .TRUE.
             msk(hi_EDIT1:hi_EDIT8) = .TRUE.
             msk(hi_ETTL1:hi_ETTL8) = .TRUE.
             msk(hi_MEMO1:hi_MEMO10) = .TRUE.
             msk(hi_DMIN) = .TRUE.
             msk(hi_DMAX) = .TRUE.
             msk(hi_DIVS) = .TRUE.
             msk(hi_DIVL) = .TRUE.
             msk(hi_STYP) = .TRUE.
             msk(hi_COPTN) = .TRUE.
             msk(hi_IOPTN) = .TRUE.
             msk(hi_ROPTN) = .TRUE.
          endif
          if (f.ge.coll_nosign) then
             msk(hi_CDATE) = .TRUE.
             msk(hi_CSIGN) = .TRUE.
             msk(hi_MDATE) = .TRUE.
             msk(hi_MSIGN) = .TRUE.
          endif
          if (f.ge.coll_nonum) then
             msk(hi_DSET) = .TRUE.
             msk(hi_FNUM) = .TRUE.
             msk(hi_DNUM) = .TRUE.
          endif
          exit gen_mask
       enddo gen_mask
    endif
    jgrp = -1
    do j = 0, ngrp - 1
#if TEST_NIO_CACHE > 1
       do ji = 1, nitem
          if (grp(j)%h(ji).ne.head(ji)) then
             write(*, *) 'collate', j, ji, &
                  & '[' // trim(grp(j)%h(ji)) // ']', &
                  & '[' // trim(head(ji)) // ']', msk(ji)
          endif
       enddo
#endif
       if (ALL(grp(j)%h(1:nitem).eq.head(1:nitem) .or. msk(1:nitem))) then
          jgrp = j
          exit
       endif
    enddo
  end subroutine collate_header

!!!_  - new_var
  subroutine new_var &
       & (ierr, grp, jvar, head)
    use TOUZA_Nio_std,only: choice, parse_number
    use TOUZA_Nio_record,only: get_header_cprop
    use TOUZA_Nio_header
    implicit none
    integer,         intent(out)   :: ierr
    type(group_t),   intent(inout) :: grp
    integer,         intent(in)    :: jvar
    character(len=*),intent(in)    :: head(*)
    integer jc
    character(len=litem) :: name
    integer irange(2)
    integer neff

    ierr = 0
    ! write(*, *) 'new_var', jvar
    if (ierr.eq.0) then
       grp%v(jvar)%item =  head(hi_ITEM)
       grp%v(jvar)%unit =  head(hi_UNIT)
       grp%v(jvar)%dfmt =  head(hi_DFMT)
       grp%v(jvar)%ceff(:) = -1
       neff = 0
       do jc = 0, lax - 1
          call get_header_cprop(name, irange, head, 1+jc)
          grp%v(jvar)%co(jc) = trim(name)
          grp%v(jvar)%jbgn(jc) = irange(1) - 1
          grp%v(jvar)%jend(jc) = irange(2)
          if (name.ne.' '.or.irange(2)-irange(1).gt.1) then
             grp%v(jvar)%ceff(neff) = jc
             neff = neff + 1
          endif
       enddo
       grp%v(jvar)%neff = neff
    endif
  end subroutine new_var

!!!_  - new_rec
  subroutine new_rec &
       & (ierr, grp, jrec, head)
    use TOUZA_Nio_header
    implicit none
    integer,         intent(out)   :: ierr
    type(group_t),   intent(inout) :: grp
    integer,         intent(in)    :: jrec
    character(len=*),intent(in)    :: head(*)
    ierr = 0
    grp%d(jrec) = head(hi_DATE)
    grp%t(jrec) = head(hi_TIME)
  end subroutine new_rec

!!!_  - group_search_var
  subroutine group_search_var &
       & (ierr, jvar, grp, head)
    use TOUZA_Nio_record,only: get_header_cprop
    use TOUZA_Nio_header
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: jvar
    type(group_t),   intent(in)  :: grp
    character(len=*),intent(in)  :: head(*)
    integer j, jc
    character(len=litem) :: name
    integer irange(2)
    ierr = 0
    loop_var: do j = 0, grp%nvar - 1
       ! write(*, *) 'search_var', j, trim(grp%v(j)%item)
       if (grp%v(j)%item.ne.head(hi_ITEM)) cycle loop_var
       if (grp%v(j)%unit.ne.head(hi_UNIT)) cycle loop_var
       do jc = 0, lax - 1
          call get_header_cprop(name, irange, head, 1+jc)
          irange(1) = irange(1) - 1
          if (grp%v(j)%co(jc).ne.name) cycle loop_var
          if (grp%v(j)%jbgn(jc).ne.irange(1)) cycle loop_var
          if (grp%v(j)%jend(jc).ne.irange(2)) cycle loop_var
       enddo
       jvar = j
       return
    enddo loop_var
    jvar = -1
  end subroutine group_search_var

!!!_  - group_search_rec
  subroutine group_search_rec &
       & (ierr, jrec, grp, head)
    use TOUZA_Nio_header
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: jrec
    type(group_t),   intent(in)  :: grp
    character(len=*),intent(in)  :: head(*)
    integer j

    ! used only for cache builds
    ierr = 0
    loop_rec: do j = 0, grp%nrec - 1
       if (grp%d(j).ne.head(hi_DATE)) cycle loop_rec
       if (grp%t(j).ne.head(hi_TIME)) cycle loop_rec
       jrec = j
       return
    enddo loop_rec
    jrec = -1
  end subroutine group_search_rec

!!!_  & free_cache
  subroutine free_cache &
       & (ierr, c)
    implicit none
    integer,      intent(out)   :: ierr
    type(cache_t),intent(inout) :: c
    integer jg
    ierr = 0
    if (ierr.eq.0) then
       do jg = 0, c%ngrp - 1
          if (ierr.eq.0) call free_group(ierr, c%g(jg))
       enddo
    endif
    if (ierr.eq.0) deallocate(c%g, c%v, c%o, STAT=ierr)
    if (ierr.eq.0) then
       c%g => NULL()
       c%v => NULL()
       c%o => NULL()
       c%ngrp = -1
    endif
  end subroutine free_cache
!!!_  - free_group
  subroutine free_group (ierr, grp)
    implicit none
    integer,      intent(out)         :: ierr
    type(group_t),intent(inout)       :: grp
    ierr = 0
    if (ierr.eq.0) then
       deallocate(grp%d, grp%t, grp%rpos, grp%rlen, STAT=ierr)
    endif
    if (ierr.eq.0) then
       grp%v => NULL()
       grp%d => NULL()
       grp%t => NULL()
       grp%rpos => NULL()
       grp%rlen => NULL()
       grp%nvar = -1
       grp%nrec = -1
       grp%g = ' '
       grp%h(:) = ' '
    endif
  end subroutine free_group
!!!_ + cached file access
!!!_  - cache_var_read
  subroutine cache_var_read_f &
       & (ierr, d, handle, gid, vid, rec, start, count)
    use TOUZA_Nio_std,only: KTGT=>KFLT, KIOFS, WHENCE_BEGIN
    use TOUZA_Nio_record,only: nio_read_header, nio_read_data
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: d(*)
    integer,        intent(in)  :: handle, gid, vid
    integer,        intent(in)  :: rec
    integer,        intent(in)  :: start(0:*), count(0:*)
    integer jc, jvg
    type(group_t),pointer :: g
    type(var_t),pointer :: v
    integer ofs(0:lax-1), mem(0:lax-1)
    character(len=litem) :: h(nitem)
    integer(kind=KIOFS) :: rpos
    integer krect, ufile
    integer jeff, jco
    integer n
    integer ridx

    ierr = 0
    v => NULL()
    g => NULL()
    jc = cache_is_valid(handle, gid, vid)
    ierr = min(0, jc)
    if (ierr.eq.0) then
       ufile = cache_h2unit(handle)
       ridx = cache_gvr2rindex(handle, gid, vid, rec)
       ierr = min(0, ridx)
       if (ierr.eq.0) then
          rpos = ctables(jc)%rpos(ridx)
          call nio_read_header(ierr, h, krect, ufile, rpos, WHENCE_BEGIN)
       endif
    endif
    ! write(*, *) 'var_read/header', ierr, rec, jpos, ufile
    if (ierr.eq.0) then
       jvg = cache_gv2vindex(handle, gid, vid)
       v => ctables(jc)%v(jvg)
       ofs(0:lax-1) = 0
       mem(0:lax-1) = v%jend(0:lax-1) - v%jbgn(0:lax-1)
       do jeff = 0, v%neff - 1
          jco = v%ceff(jeff)
          ofs(jco) = start(jeff)
          mem(jco) = count(jeff)
       enddo
       n = -1
       call nio_read_data &
            & (ierr, d, n, h, krect, ufile, start=ofs(0:lax-1), count=mem(0:lax-1))
       ! write(*, *) 'cache_var_read', ierr, n, ofs(0:lax-1), mem(0:lax-1)
    endif
  end subroutine cache_var_read_f

!!!_  - cache_gvr2rindex() - return /suite/ record index
  integer function cache_gvr2rindex(handle, gid, vid, rec) result(ridx)
    implicit none
    integer,intent(in) :: handle
    integer,intent(in) :: gid
    integer,intent(in) :: vid
    integer,intent(in) :: rec
    integer jerr
    integer jc, jg, jv
    jc = cache_is_valid(handle, gid, vid)
    jerr = min(0, jc)
    if (jerr.eq.0) then
       if (vid.eq.vid_suite) then
          if (rec.ge.0.and.rec.lt.ctables(jc)%lrec) then
             ridx = rec
          else
             jerr = -1
          endif
       else
          jg = cache_gv2gindex(handle, gid, vid)
          jv = cache_gv2vindex(handle, gid, vid)
          jerr = min(0, jg, jv)
          if (jerr.eq.0) then
             if (rec.ge.0.and.rec.lt.ctables(jc)%g(jg)%nrec) then
                ridx = rec + ctables(jc)%v(jv)%rofs
             else
                jerr = -1
             endif
          endif
       endif
    endif
    if (jerr.ne.0) then
       ridx = _ERROR(ERR_INVALID_PARAMETER)
    endif
  end function cache_gvr2rindex
!!!_  - cache_gv2vindex() - return /suite/ variable index
  integer function cache_gv2vindex(handle, gid, vid) result(jv)
    implicit none
    integer,intent(in) :: handle
    integer,intent(in) :: gid
    integer,intent(in) :: vid
    integer jc
    integer jerr
    jc = cache_is_valid(handle, gid, vid)
    jerr = min(0, jc)
    if (jerr.eq.0) then
       if (gid.eq.gid_suite) then
          jv = vid
       else
          jv = ctables(jc)%o(gid) + vid
       endif
    else
       jv = jerr
    endif
  end function cache_gv2vindex

!!!_  - cache_gv2gindex() - return /suite/ group index
  integer function cache_gv2gindex(handle, gid, vid) result(jg)
    implicit none
    integer,intent(in) :: handle
    integer,intent(in) :: gid
    integer,intent(in) :: vid
    integer jc, j
    integer jerr
    jc = cache_is_valid(handle, gid, vid)
    jerr = min(0, jc)
    if (jerr.eq.0) then
       if (gid.eq.gid_suite) then
          do j = 0, ctables(jc)%ngrp - 1
             if (ctables(jc)%o(j+1).gt.vid) then
                jg = j
                return
             endif
          enddo
          jg = _ERROR(ERR_PANIC)
       else
          jg = gid
       endif
    else
       jg = jerr
    endif
  end function cache_gv2gindex

!!!_ + cached file manager
!!!_  & cache_inquire
!!!_  & cache_inquire_variable
!!!_  & cache_inquire_dimension
!!!_ + cache table manager
!!!_ + user interfaces
!!!_  & cache_open
!!!_  & cache_close
!!!_  & cache_read_header
!!!_  & cache_write_header
!!!_  & cache_read_data
!!!_  & cache_write_data
!!!_  & cache_verify_record
!!!_  & cache_read_record
!!!_  & cache_create_record
!!!_  & cache_write_record
!!!_  & cache_search_record
!!!_  & cache_store_v0 - copy header entry to cache (v0)
  subroutine cache_store_v0 &
       & (ierr, nentr, cache, head, ofs, lb, swap)
    implicit none
    integer,            intent(out) :: ierr
    integer,            intent(out) :: nentr
    character(len=*),   intent(out) :: cache(*)
    character(len=*),   intent(in)  :: head(*)
    integer(KIND=KIOFS),intent(in)  :: ofs, lb
    logical,            intent(in)  :: swap
    integer,parameter :: ver = 0
    integer jc, jh

    ierr = 0
    jc = 1
    do
       jh = c2hitem(jc, ver)
       if (jh.lt.-9) exit
       if (jh.ge.0) cache(jc) = head(jh)
       jc = jc + 1
    enddo
    nentr = jc - 1
    call store_cache_lset(ierr, cache(1), ofs, lb, swap)
    call store_cache_aset(ierr, cache(2), head(hi_ASTR1), head(hi_AEND1), swap)
    call store_cache_aset(ierr, cache(3), head(hi_ASTR2), head(hi_AEND2), swap)
    call store_cache_aset(ierr, cache(4), head(hi_ASTR3), head(hi_AEND3), swap)

  end subroutine cache_store_v0

!!!_  & c2hitem()
  integer function c2hitem(idx, ver) result(n)
    implicit none
    integer,intent(in) :: idx
    integer,intent(in) :: ver
    integer j
    integer,parameter :: nv0 = 16
    integer,parameter :: xv0(0:nv0+1) = &
         & (/-999, &
         &   -1,      -1,      -1,      -1,      hi_AITM1, hi_AITM2, hi_AITM3, hi_ITEM, &
         &   hi_DSET, hi_DFMT, hi_DATE, hi_TIME, hi_UTIM,  hi_TDUR,  -1,       -1,      &
         &   -999/)
    if (ver.eq.0) then
       j = min(max(0, idx), nv0+1)
       n = xv0(j)
    else
       n = -999
    endif
  end function c2hitem

!!!_  & c2hitem_vtype ()
  integer function c2hitem_vtype(idx, sub, ver) result(n)
    implicit none
    integer,intent(in) :: idx
    integer,intent(in) :: sub   ! record type (0=common 1=time)
    integer,intent(in) :: ver
    integer j
    integer,parameter :: nv0 = 8
    integer,parameter :: xv0(0:nv0+1) = &
         & (/-999,     &
         &   hi_DFMT,  -1,       -1,       -1, &
         &   hi_AITM1, hi_AITM2, hi_AITM3, -1, &
         &   -999/)
    integer,parameter :: xv1(0:nv0+1) = &
         & (/-999,     &
         &   -1,       hi_DATE, hi_TIME,  hi_TDUR, &
         &   hi_TIME2, hi_UTIM, hi_UTIM2, -1,      &
         &   -999/)
    if (ver.eq.0) then
       j = min(max(0, idx), nv0+1)
       if (sub.eq.0) then
          n = xv0(j)
       else
          n = xv1(j)
       endif
    else
       n = -999
    endif
  end function c2hitem_vtype

!!!_  & store_cache_lset
  subroutine store_cache_lset &
       & (ierr, centr, l0, l1, swap)
    use TOUZA_Std,only: sus_eswap
    implicit none
    integer,            intent(out) :: ierr
    character(len=*),   intent(out) :: centr
    integer(kind=KIOFS),intent(in)  :: l0, l1
    logical,            intent(in)  :: swap
    integer(kind=KI64)  :: lbuf(2)

    ierr = 0
    if (swap) then
       lbuf(1) = sus_eswap(l0)
       lbuf(2) = sus_eswap(l1)
    else
       lbuf(1) = l0
       lbuf(2) = l1
    endif
    centr = ' '
    centr(1:8)  = transfer(lbuf(1), centr(1:8))
    centr(9:16) = transfer(lbuf(2), centr(9:16))

  end subroutine store_cache_lset

!!!_  & store_cache_aset
  subroutine store_cache_aset &
       & (ierr, centr, a0, a1, swap)
    use TOUZA_Std,only: parse_number
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: centr
    character(len=*),intent(in)  :: a0, a1
    logical,         intent(in)  :: swap
    integer n, jerr
    integer(kind=KI64)  :: lbuf(2)

    call parse_number(jerr, n, a0)
    if (jerr.ne.0) n = -1
    lbuf(1) = n
    call parse_number(jerr, n, a1)
    if (jerr.ne.0) n = -1
    lbuf(2) = n
    call store_cache_lset(ierr, centr, lbuf(1), lbuf(2), swap)

  end subroutine store_cache_aset

!!!_ + end module
end module TOUZA_Nio_cache

!!!_@ test_nio_cache - test program
#ifdef TEST_NIO_CACHE
program test_nio_cache
  use TOUZA_Std,only: parse, get_param, arg_diag, arg_init, KIOFS, get_nparam
  use TOUZA_Std,only: sus_open
  use TOUZA_Nio_header,only: nitem, litem
  use TOUZA_Nio_record,only: get_default_header
  use TOUZA_Nio_cache
  implicit none
  integer ierr
  integer jarg, narg
  integer,parameter :: lpath = 256
  character(len=lpath) :: file

  integer uh
  integer flag

  ierr = 0
  jarg = 0
  flag = 0

101 format(A,' = ', I0)
  call init(ierr, stdv=+9)
  if (ierr.eq.0) call arg_init(ierr, levv=-9)
  if (ierr.eq.0) call parse(ierr)
  if (ierr.eq.0) call arg_diag(ierr)
  if (ierr.eq.0) narg = get_nparam()

  if (ierr.eq.0) then
     do jarg = 1, narg
        call get_param(ierr, file, jarg, ' ')
        if (file.eq.' ') then
           write(*, *) 'need file to test.'
           ierr = -1
           exit
        endif
        if (ierr.eq.0) call cache_open_read(ierr, uh, file, flag)
        if (ierr.eq.0) call show_cache(ierr, uh, levv=+2)
     enddo
  endif
  if (ierr.eq.0) call diag(ierr, levv=+9, mode=MODE_DEEPEST)
  if (ierr.eq.0) call finalize(ierr)
  write(*, 101) 'FINAL', ierr
  stop
contains
end program test_nio_cache

#endif /* TEST_NIO_CACHE */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

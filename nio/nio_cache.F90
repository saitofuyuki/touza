!!!_! nio_cache.F90 - TOUZA/Nio cache-record extension
! Maintainer: SAITO Fuyuki
! Created: Nov 9 2022
#define TIME_STAMP 'Time-stamp: <2025/02/14 22:38:31 fuyuki nio_cache.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022,2023,2024,2025
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_nio.h"
#include "touza_nio_param.h"
!!!_* macros
#if HAVE_F2003_ALLOCATABLE_MEMBER
#  define _POINTER allocatable
#else  /* not HAVE_F2003_ALLOCATABLE_MEMBER */
#  define _POINTER pointer
#endif /* not HAVE_F2003_ALLOCATABLE_MEMBER */

#ifndef   OPT_NIO_CACHES_SIZE
#  define OPT_NIO_CACHES_SIZE  (2**8)
#endif
#ifndef   OPT_NIO_UNIT_LIMIT
#  define OPT_NIO_UNIT_LIMIT   12         /* base 2 power */
#endif
#ifndef   OPT_NIO_GROUPS_LIMIT
#  define OPT_NIO_GROUPS_LIMIT 10         /* base 2 power */
#endif
!!!_@ TOUZA_Nio_cache - nio with cache
module TOUZA_Nio_cache
!!!_ = declaration
  use TOUZA_Nio_std,only: KI32, KI64, KDBL, KFLT, KIOFS
  use TOUZA_Nio_std,only: get_logu,   unit_global,  trace_fine,   trace_control, trace_err
  use TOUZA_Nio_header,nh_init=>init, nh_diag=>diag, nh_finalize=>finalize
  use TOUZA_Nio_record,only: REC_ERROR
  implicit none
  private
!!!_  - public parameters
  integer,parameter,public :: coll_default = NIO_CACHE_COLL_DEFAULT

  ! std: ignore DFMT ITEM DATE TIME TDUR TIME2 UTIM2 SIZE MISS AITMn ASTRn AENDn
  integer,parameter,public :: coll_std       = NIO_CACHE_COLL_STD
  ! basic: std + ignore TITL UNIT EDIT ETTL MEMO DMIN DMAX DIVS DIVL STYP [CIR]OPTN
  integer,parameter,public :: coll_basic     = NIO_CACHE_COLL_BASIC
  ! nosign: basic + ignore [CM]DATE [CM]SIGN
  integer,parameter,public :: coll_nosign    = NIO_CACHE_COLL_NOSIGN
  ! nonum: nosign + ignore DSET FNUM DNUM
  integer,parameter,public :: coll_nonum     = NIO_CACHE_COLL_NONUM
  ! disable DSET special (bitwise)
  integer,parameter,public :: coll_nospecial = NIO_CACHE_COLL_NOSPECIAL
  ! allow variable name conflicts (bitwise)
  integer,parameter,public :: allow_var_dup = NIO_CACHE_ALLOW_VAR_DUP
  integer,parameter,public :: allow_grp_dup = NIO_CACHE_ALLOW_GRP_DUP
  integer,parameter,public :: allow_coor_dup = NIO_CACHE_ALLOW_COOR_DUP

  integer,parameter,public :: collm_std    = NIO_CACHE_COLL_MASK_STD
  integer,parameter,public :: collm_basic  = NIO_CACHE_COLL_MASK_BASIC
  integer,parameter,public :: collm_nosign = NIO_CACHE_COLL_MASK_NOSIGN
  integer,parameter,public :: collm_nonum  = NIO_CACHE_COLL_MASK_NONUM

  ! special variable id to set all the records in single suite
  integer,parameter,public :: var_suite = NIO_CACHE_VAR_SUITE

  character(len=*),parameter,public :: group_suite = ' '
!!!_  - private parameter
  integer,parameter :: lcoor = 4, mcoor = 3
  integer,parameter :: cmdl = mcoor

  integer,parameter :: pat_file   = OPT_NIO_UNIT_LIMIT
  integer,parameter :: mdl_file   = 2 ** pat_file
  integer,parameter :: msk_file   = mdl_file - 1
  integer,parameter :: lim_groups = 2 ** OPT_NIO_GROUPS_LIMIT
  integer,parameter :: msk_group  = lim_groups - 1
  ! special group id to set all the variables in single suite
  ! only meaningful when specify variable id
  integer,parameter,public :: grp_suite  = lim_groups - 1   !! 1....1 DO NOT CHANGE
  integer,parameter :: lgroups    = lim_groups - 4   ! use for handle/group integrated id

  integer,parameter :: ucache = 16
  integer,parameter :: lpath = OPT_PATH_LEN

  integer,parameter :: cnull = -1   ! null coordinate (size <=0 and blank)
!!!_  - types
  type var_t
     character(len=litem) :: item = ' '
     character(len=litem) :: unit = ' '
     character(len=litem) :: dfmt = ' '
     integer              :: neff
     integer              :: xh(0:lcoor-1)   = -1  ! coordinate handle
     integer              :: ceff(0:lcoor-1) = -1  ! effective coordinate index
     integer              :: flag
     integer              :: rofs                ! start index of cache%rpos
     integer              :: lastrec = -1        ! last accessed record
  end type var_t

  type group_t
     integer :: nvar = -1                   ! size of v
     integer :: nrec = -1                   ! number of records
     integer :: ncoor = -1                  ! number of coordinates
     character(len=litem) :: name           ! group name
     character(len=litem) :: h(nitem)       ! header
     character(len=litem),pointer :: d(:) => NULL()    ! date [rec]
     character(len=litem),pointer :: t(:) => NULL()    ! time [rec]
     integer,             pointer :: xh(:) => NULL()   ! coordinate handle
     ! temporal properties, to integrated into cache_t
     type(var_t),pointer :: v(:) => NULL()
     character(len=litem),pointer :: x(:) => NULL()    ! coordinates
     integer,             pointer :: jbgn(:) => NULL() ! coordinate ranges
     integer,             pointer :: jend(:) => NULL()
     integer(kind=KIOFS),pointer  :: rpos(:, :) => NULL() ! record offset [rec, var]
     integer(kind=KIOFS),pointer  :: rlen(:, :) => NULL() ! record size   [rec, var]
  end type group_t

  type cache_t
     integer :: ngrp = -1
     integer :: ncoor = -1
     type(group_t),pointer :: g(:)
     type(var_t),pointer   :: v(:)
     integer,pointer       :: o(:)   ! group-variable offset
     character(len=litem),pointer :: x(:) => NULL()    ! coordinates
     integer,             pointer :: jbgn(:) => NULL() ! coordinate ranges
     integer,             pointer :: jend(:) => NULL()
     integer(kind=KIOFS),pointer  :: rpos(:) => NULL() ! record offset [var+rec]
     integer(kind=KIOFS),pointer  :: dpos(:) => NULL() ! data-part offset [var+rec]
     integer(kind=KIOFS),pointer  :: rlen(:) => NULL() ! record size   [var+rec]
     integer :: lrec
     integer :: krect = REC_ERROR ! record-type, to remember
  end type cache_t
!!!_  - static
  integer,save :: cache_rev = 0
  integer,parameter :: nloc = 5, nspec = 9

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
  interface show_group
     module procedure show_group_t, show_group_i
  end interface show_group
  interface cache_get_attr
     module procedure cache_geti_attr_a, cache_getn_attr_a
     module procedure cache_geti_attr_i, cache_getn_attr_i
     module procedure cache_geti_attr_f, cache_getn_attr_f
     module procedure cache_geti_attr_d, cache_getn_attr_d
  end interface cache_get_attr

  interface cache_rec_time
     module procedure cache_rec_time_a
  end interface cache_rec_time
  interface cache_rec_date
     module procedure cache_rec_date_a
  end interface cache_rec_date

  interface cache_var_read
     module procedure cache_var_read_i, cache_var_read_f, cache_var_read_d
  end interface cache_var_read

  interface cache_unit
     module procedure extr_h2unit
  end interface cache_unit
  interface cache_group
     module procedure cache_group_n, cache_group_j
  end interface cache_group

  interface cache_time_rec
     module procedure cache_time_rec_d
  end interface cache_time_rec

  interface cache_sparse_review
     module procedure cache_sparse_review_i, cache_sparse_review_f, cache_sparse_review_d
  end interface cache_sparse_review
  interface cache_restore_csr
     module procedure cache_restore_csr_i, cache_restore_csr_d
  end interface cache_restore_csr

  interface query_rserial_time
     module procedure query_rserial_time_d
  end interface query_rserial_time

!!!_  - public procedures
  public init, diag, finalize
  public init_group
  public show_cache
  public cache_open_read,  cache_close
  public cache_unit,       cache_is_registered
  public cache_group_size, cache_group,  cache_group_name, cache_group_recs
  public cache_group_cserial
  public cache_group_coors,cache_group_coor_name, cache_group_coor_range
  public cache_var_size,   cache_var_id, cache_var_name,   cache_co_size
  public cache_co_serial
  public cache_co_all,     cache_co_idx, cache_co_name,    cache_co_len
  public cache_co_range,   cache_var_len
  public cache_time_rec,   cache_rec_date, cache_rec_time

  public cache_store_v0
  public cache_var_read
  public cache_read_header
  public cache_get_attr,      cache_get_header
  public cache_sparse_review, cache_restore_csr
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
    use TOUZA_Nio_axis,only: na_init=>init
    use TOUZA_Nio_sparse,only: np_init=>init
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
          if (ierr.eq.0) call nh_init(ierr, u=ulog, levv=lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call nr_init(ierr, u=ulog, levv=lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call na_init(ierr, u=ulog, levv=lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call np_init(ierr, u=ulog, levv=lv, mode=MODE_SURFACE)
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
    use TOUZA_Nio_axis,only: na_diag=>diag
    use TOUZA_Nio_sparse,only: np_diag=>diag
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
             if (is_msglev_normal(lv)) then
                call msg('(''OPT_NIO_GROUPS_LIMIT = '', I0)', (/OPT_NIO_GROUPS_LIMIT/), __MDL__, u=utmp)
                call msg('(''OPT_NIO_CACHES_SIZE = '', I0)', (/OPT_NIO_CACHES_SIZE/), __MDL__, u=utmp)
             endif
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ns_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nh_diag(ierr, utmp, levv=lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call nr_diag(ierr, utmp, levv=lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call na_diag(ierr, utmp, levv=lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call np_diag(ierr, utmp, levv=lv, mode=MODE_SURFACE)
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
    use TOUZA_Nio_axis,only: na_finalize=>finalize
    use TOUZA_Nio_sparse,only: np_finalize=>finalize
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
          if (ierr.eq.0) call ns_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nh_finalize(ierr, utmp, levv=lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call nr_finalize(ierr, utmp, levv=lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call na_finalize(ierr, utmp, levv=lv, mode=MODE_SURFACE)
          if (ierr.eq.0) call np_finalize(ierr, utmp, levv=lv, mode=MODE_SURFACE)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize
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
       if (ierr.ne.0) ierr = _ERROR(ERR_ALLOCATION)
    endif
    return
  end subroutine init_table
!!!_ + controls
!!!_  - cache_open_read
  subroutine cache_open_read &
       & (ierr, handle, path, flag, unit)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_std,only: reg_entry, new_unit, sus_open
    use TOUZA_Nio_std,only: is_msglev_WARNING, msg
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: handle
    character(len=*),intent(in)  :: path
    integer,optional,intent(in)  :: flag
    integer,optional,intent(in)  :: unit   ! allocate if not present
    integer u
    integer jc
    character(len=lpath) :: txt

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
    if (ierr.eq.0) ierr = register_cache(u, jc)
    if (ierr.eq.0) then
       call sus_open(ierr, u, path, ACTION='R', STATUS='O')
       ! see Note in nio_bindc::tnb_file_is_nio, about duplicate open for single file
       if (ierr.ne.0) then
          handle = cache_is_registered(path)
          if (handle.ge.0) then
             ierr = 0
             if (is_msglev_WARNING(lev_verbose)) then
101             format('duplicate registration = ', A)
                write(txt, 101) trim(path)
                call msg(txt)
             endif
             return
          endif
       endif
    endif
    if (ierr.eq.0) call scan_file(ierr, ctables(jc), u, flag)
    if (ierr.eq.0) call settle_cache(ierr, ctables(jc), flag)

    if (ierr.eq.0) then
       handle = conv_u2handle(u)
       ierr = min(0, handle)
    endif
    call trace_err(ierr, fun='cache_open_read', asfx=path)
  end subroutine cache_open_read

!!!_  - cache_close
  subroutine cache_close &
       & (ierr, handle, path)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: handle
    character(len=*),intent(in),optional :: path

    character(len=lpath) :: bufp
    integer ufile
    integer jc
    ierr = 0

    jc = is_valid(handle)
    ierr = min(0, jc)
    if (ierr.eq.0) then
       ufile = extr_h2unit(handle)
       if (present(path)) then
          if (path.ne.' ') then
             inquire(UNIT=ufile, NAME=bufp, IOSTAT=ierr)
             if (ierr.eq.0) then
                if (path.ne.bufp) ierr = _ERROR(ERR_PANIC)
             endif
          endif
       endif
       if (ierr.eq.0) close(unit=ufile, IOSTAT=ierr)
    endif
    if (ierr.eq.0) call free_cache(ierr, ctables(jc))
    call trace_err(ierr, fun='cache_close', isfx=handle, asfx=path)
  end subroutine cache_close

!!!_  - cache_is_registered()
  integer function cache_is_registered (path) result(handle)
    use TOUZA_Nio_std,only: is_file_opened
    implicit none
    character(len=*),intent(in)  :: path
    integer u, h, jc
    u = is_file_opened(path)
    if (u.ge.0) then
       handle = conv_u2handle(u)
       jc = extr_h2index(handle)
       if (jc.lt.0) handle = jc
    else
       handle = u
    endif
  end function cache_is_registered

!!!_  - cache_group_size()
  integer function cache_group_size(handle) result(n)
    implicit none
    integer,intent(in)  :: handle
    integer jc, gid
    gid = extr_h2group(handle)
    ! No hierarchies
    if (gid.ne.grp_suite) then
       n = _ERROR(ERR_INVALID_PARAMETER)
    else
       jc = extr_h2index(handle)
       n = min(0, jc)
       if (n.eq.0) then
          n = ctables(jc)%ngrp
       endif
    endif
  end function cache_group_size
!!!_  - cache_group - return group handle corresponding to name or index
  integer function cache_group_n(name, handle, init) result(h)
    use TOUZA_Nio_std,only: choice
    implicit none
    character(len=*),intent(in) :: name
    integer,         intent(in) :: handle
    integer,optional,intent(in) :: init     ! for variable duplication
    integer gid, gini
    integer jc, jgb, jge

    ! Search first occurence of group NAME from INIT.

    gid = extr_h2group(handle)
    if (gid.ne.grp_suite) then
       h = _ERROR(ERR_INVALID_PARAMETER)
    else
       jc = extr_h2index(handle)
       h = min(0, jc)
       if (h.eq.0) then
          if (name.eq.group_suite) then
             gid = grp_suite
          else
             jge = ctables(jc)%ngrp
             gini = choice(-1, init)
             if (gini.lt.0) then
                jgb = 0
             else
                jgb = extr_h2group(gini) + 1
             endif
             gid = query_gserial(ctables(jc)%g, name, jgb, jge)
          endif
          h = conv_h2handle(handle, gid)
       endif
    endif
  end function cache_group_n

  integer function cache_group_j(idx, handle) result(h)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,intent(in) :: idx
    integer,intent(in) :: handle
    integer gid
    integer jc

    gid = extr_h2group(handle)
    if (gid.ne.grp_suite) then
       h = _ERROR(ERR_INVALID_PARAMETER)
    else
       jc = extr_h2index(handle)
       h = min(0, jc)
       ! write(*, *) handle, gid, h, idx
       if (h.eq.0) then
          if (idx.ge.0.and.idx.lt.ctables(jc)%ngrp) then
             h = conv_h2handle(handle, idx)
          else
             h = _ERROR(ERR_OUT_OF_RANGE)
          endif
       endif
    endif
  end function cache_group_j

!!!_  - cache_group_name
  subroutine cache_group_name(ierr, name, handle)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: name
    integer,         intent(in)  :: handle
    integer jc, gid
    jc = is_valid(handle) ! check gid also
    ierr = min(0, jc)
    if (ierr.eq.0) gid = extr_h2group(handle)
    if (ierr.eq.0) ierr = min(0, gid)
    if (ierr.eq.0) then
       if (gid.eq.grp_suite) then
          name = group_suite
       else
          name = ctables(jc)%g(gid)%name
       endif
    endif
  end subroutine cache_group_name

!!!_  - cache_group_recs()
  integer function cache_group_recs(handle, vid) result(n)
    implicit none
    integer,intent(in)          :: handle
    integer,intent(in),optional :: vid
    integer jc, gser
    jc = is_valid(handle, vid) ! check gid vid also
    n = min(0, jc)
    if (n.eq.0) gser = get_gserial(handle, vid)
    if (n.eq.0) n = min(0, gser)
    if (n.eq.0) then
       n = ctables(jc)%g(gser)%nrec
    endif
  end function cache_group_recs

!!!_  & cache_group_coors() - return number of coordinates in a group or total
  integer function cache_group_coors(handle) result(n)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,intent(in) :: handle
    integer jc
    integer gid

    jc = is_valid(handle)
    n = min(0, jc)
    if (n.lt.0) return

    gid = extr_h2group(handle)
    if (gid.eq.grp_suite) then
       n = ctables(jc)%ncoor
    else
       n = ctables(jc)%g(gid)%ncoor
    endif
  end function cache_group_coors

!!!_  & cache_group_cserial()
  integer function cache_group_cserial(handle, cid) result(xh)
    implicit none
    integer,intent(in) :: handle
    integer,intent(in) :: cid
    integer jc, gid
    jc = is_valid(handle)
    xh = min(0, jc)
    if (xh.eq.0) then
       gid = extr_h2group(handle)
       if (gid.eq.grp_suite) then
          xh = cid
       else
          if (cid.lt.0.or.cid.ge.ctables(jc)%g(gid)%ncoor) then
             xh = _ERROR(ERR_OUT_OF_RANGE)
          else
             xh = ctables(jc)%g(gid)%xh(cid)
          endif
       endif
       if (xh.lt.0.or.xh.ge.ctables(jc)%ncoor) then
          xh = _ERROR(ERR_OUT_OF_RANGE)
       endif
    endif
  end function cache_group_cserial

!!!_  - cache_group_coor_name - return coordinate name
  subroutine cache_group_coor_name(ierr, name, handle, cid)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: name
    integer,         intent(in)  :: handle
    integer,         intent(in)  :: cid
    integer jc, xh
    ierr = 0
    xh = cache_group_cserial(handle, cid)
    ierr = min(0, xh)
    if (ierr.eq.0) then
       jc = is_valid(handle)
       name = ctables(jc)%x(xh)
    else
       name = ' '
    endif
  end subroutine cache_group_coor_name

!!!_  - cache_group_coor_range - return coordinate range
  subroutine cache_group_coor_range(ierr, jbgn, jend, handle, cid)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: jbgn, jend
    integer,         intent(in)  :: handle
    integer,         intent(in)  :: cid
    integer jc, xh

    ierr = 0
    xh = cache_group_cserial(handle, cid)
    ierr = min(0, xh)
    if (ierr.eq.0) then
       jc = is_valid(handle)
       jbgn = ctables(jc)%jbgn(xh)
       jend = ctables(jc)%jend(xh)
    else
       jbgn = -1
       jend = -2
    endif
  end subroutine cache_group_coor_range

!!!_  & cache_var_size() - return number of variables in a group or total
  integer function cache_var_size(handle) result(n)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,intent(in) :: handle
    integer jc
    integer gid

    jc = is_valid(handle)
    n = min(0, jc)
    if (n.lt.0) return

    gid = extr_h2group(handle)
    if (gid.eq.grp_suite) then
       n = ctables(jc)%o(ctables(jc)%ngrp)
    else
       n = ctables(jc)%g(gid)%nvar
    endif
  end function cache_var_size

!!!_  - cache_var_name - return variable name corresponding to group/var id
  subroutine cache_var_name(ierr, name, handle, vid)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: name
    integer,         intent(in)  :: handle
    integer,         intent(in)  :: vid
    integer jc, vser

    vser = get_vserial(handle, vid)
    ierr = min(0, vser)
    if (ierr.eq.0) then
       jc = extr_h2index(handle)
       name = ctables(jc)%v(vser)%item
    else
       name = ' '
    endif
  end subroutine cache_var_name

!!!_  - cache_var_id - return variable id corresponding to name
  integer function cache_var_id(name, handle, init) result(vid)
    use TOUZA_Nio_std,only: choice
    implicit none
    character(len=*),intent(in) :: name
    integer,         intent(in) :: handle
    integer,optional,intent(in) :: init     ! for variable duplication
    integer jerr
    integer jc, jvo, jvb, jve
    integer gid

    ! Search first occurence of variable NAME from INIT.
    ! If strict matching is desired, set NAME as {NAME // trim (dup_sep) // index},
    ! e.g., PRCP~2.

    jc = is_valid(handle)
    jerr = min(0, jc)

    if (jerr.eq.0) then
       gid = extr_h2group(handle)
       jerr = min(0, gid)
    endif
    if (jerr.eq.0) then
       if (gid.eq.grp_suite) then
          jvo = 0
          jve = ctables(jc)%o(ctables(jc)%ngrp)
          jvb = choice(jvo, init)
       else
          jvo = ctables(jc)%o(gid)
          jve = ctables(jc)%o(gid + 1)
          jvb = jvo + choice(0, init)
       endif
       vid = query_vserial(ctables(jc)%v, name, jvb, jve)
       ! convert v serial to v index
       if (vid.ge.0) vid = vid - jvo
    else
       vid = jerr
    endif
  end function cache_var_id

!!!_  & cache_var_len() - return array size of variable
  integer function cache_var_len(handle, vid) result(n)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,intent(in) :: handle
    integer,intent(in) :: vid
    integer jc, vser, jx, xh
    integer jerr

    vser = get_vserial(handle, vid)
    jerr = min(0, vser)
    if (jerr.eq.0) then
       jc = extr_h2index(handle)
       n = 1
       do jx = 0, ctables(jc)%v(vser)%neff - 1
          xh = ctables(jc)%v(vser)%xh(jx)
          n = n * (ctables(jc)%jend(xh) - ctables(jc)%jbgn(xh))
       enddo
    else
       n = jerr
    endif
  end function cache_var_len

!!!_  - cache_rec_time - return time string
  subroutine cache_rec_time_a &
       & (ierr, val, handle, vid, rec)
    use TOUZA_Nio_std,only: choice
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: val
    integer,         intent(in)  :: handle
    integer,optional,intent(in)  :: vid
    integer,         intent(in)  :: rec

    integer jc, gser

    gser = -1
    jc = is_valid(handle, vid)
    ierr = min(0, jc)
    if (ierr.eq.0) then
       gser = get_gserial(handle, vid)
       ierr = min(0, gser)
    endif
    if (ierr.eq.0) then
       if (rec.ge.0.and.rec.lt.ctables(jc)%g(gser)%nrec) then
          val = ctables(jc)%g(gser)%t(rec)
       else
          ierr = _ERROR(ERR_OUT_OF_RANGE)
       endif
    endif
  end subroutine cache_rec_time_a

!!!_  - cache_rec_date - return date string
  subroutine cache_rec_date_a &
       & (ierr, val, handle, vid, rec)
    use TOUZA_Nio_std,only: choice
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: val
    integer,         intent(in)  :: handle
    integer,optional,intent(in)  :: vid
    integer,         intent(in)  :: rec

    integer jc, gser

    gser = -1
    jc = is_valid(handle, vid)
    ierr = min(0, jc)
    if (ierr.eq.0) then
       gser = get_gserial(handle, vid)
       ierr = min(0, gser)
    endif
    if (ierr.eq.0) then
       if (rec.ge.0.and.rec.lt.ctables(jc)%g(gser)%nrec) then
          val = ctables(jc)%g(gser)%d(rec)
       else
          ierr = _ERROR(ERR_OUT_OF_RANGE)
       endif
    endif
  end subroutine cache_rec_date_a

!!!_  - cache_time_rec - return record id filtered by time range
  integer function cache_time_rec_d &
       & (handle, vid, timel, timeh, func, init) result(rid)
    use TOUZA_Nio_std,only: KTGT=>KDBL
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,         intent(in) :: handle
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

    integer jc, gser, vser, jr
    integer jerr
    integer rbgn, rend, rofs

    rid = -1
    jc = is_valid(handle)
    jerr = min(0, jc)
    if (jerr.eq.0) then
       gser = get_gserial(handle, vid)
       vser = get_vserial(handle, vid)
       jerr = min(0, gser, vser)
    endif
    if (jerr.eq.0) then
       rofs = get_rserial(handle, vid, 0)
       rbgn = choice(rofs, init) - rofs
       rend = ctables(jc)%g(gser)%nrec
       do jr = rbgn, rend
          if (func(ctables(jc)%g(gser)%d(jr), ctables(jc)%g(gser)%t(jr), timel, timeh)) then
             rid = jr
             exit
          endif
       enddo
    endif
  end function cache_time_rec_d
!!!_  - cache_co_size - return number of effective dimensions
  integer function cache_co_size(handle, vid) result(ndim)
    implicit none
    integer,intent(in)  :: handle
    integer,intent(in)  :: vid
    integer jc, vser
    integer jerr

    vser = get_vserial(handle, vid)
    jerr = min(0, vser)
    if (jerr.eq.0) then
       jc = extr_h2index(handle)
       ndim = ctables(jc)%v(vser)%neff
    else
       ndim = jerr
    endif
  end function cache_co_size

!!!_  - cache_co_serial - return coordinate serial id
  integer function cache_co_serial(handle, vid, cid) result(xh)
    implicit none
    integer,intent(in)  :: handle
    integer,intent(in)  :: vid
    integer,intent(in)  :: cid
    integer jc, jeff, vser
    type(var_t),pointer :: v

    vser = get_vserial(handle, vid)
    xh = min(0, vser)
    v => NULL()
    if (xh.eq.0) then
       jc = extr_h2index(handle)
       v => ctables(jc)%v(vser)
       if (cid.lt.0.or.cid.ge.v%neff) xh = _ERROR(ERR_INVALID_ITEM)
    endif
    if (xh.eq.0) then
       jeff = v%ceff(cid)
       if (jeff.lt.0) xh = _ERROR(ERR_PANIC)
    endif
    if (xh.eq.0) xh = v%xh(jeff)

  end function cache_co_serial

!!!_  - cache_co_len - return coordinate length
  integer function cache_co_len(handle, vid, cid) result(nerr)
    implicit none
    integer,intent(in)  :: handle
    integer,intent(in)  :: vid
    integer,intent(in)  :: cid
    integer jc, xh

    xh = cache_co_serial(handle, vid, cid)
    nerr = min(0, xh)
    if (nerr.eq.0) then
       jc = extr_h2index(handle)
       nerr = max(0, ctables(jc)%jend(xh) - ctables(jc)%jbgn(xh))
    endif
  end function cache_co_len

!!!_  - cache_co_range - return coordinate range
  subroutine cache_co_range(ierr, jbgn, jend, handle, vid, cid)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: jbgn, jend
    integer,intent(in)  :: handle
    integer,intent(in)  :: vid
    integer,intent(in)  :: cid
    integer jc, xh

    xh = cache_co_serial(handle, vid, cid)
    ierr = min(0, xh)
    if (ierr.eq.0) then
       jc = extr_h2index(handle)
       jbgn = ctables(jc)%jbgn(xh)
       jend = ctables(jc)%jend(xh)
    endif
  end subroutine cache_co_range

!!!_  - cache_co_all - return coordinate name
  subroutine cache_co_all(ierr, name, handle, vid)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: name(0:*)
    integer,         intent(in)  :: handle
    integer,         intent(in)  :: vid
    integer jx, nx
    nx = cache_co_size(handle, vid)
    ierr = min(0, nx)
    if (ierr.eq.0) then
       do jx = 0, nx - 1
          if (ierr.eq.0) call cache_co_name(ierr, name(jx), handle, vid, jx)
       enddo
    endif
  end subroutine cache_co_all
!!!_  - cache_co_name - return coordinate name
  subroutine cache_co_name(ierr, name, handle, vid, cid)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: name
    integer,         intent(in)  :: handle
    integer,         intent(in)  :: vid
    integer,         intent(in)  :: cid
    integer jc, xh

    xh = cache_co_serial(handle, vid, cid)
    ierr = min(0, xh)
    if (ierr.eq.0) then
       jc = extr_h2index(handle)
       name = ctables(jc)%x(xh)
    else
       name = ' '
    endif
  end subroutine cache_co_name

!!!_  - cache_co_idx - return coordinate index
  integer function cache_co_idx(handle, vid, name) result(jeff)
    implicit none
    integer,         intent(in)  :: handle
    integer,         intent(in)  :: vid
    character(len=*),intent(in)  :: name
    integer jerr
    integer jc, jx, vser, xh
    type(var_t),pointer :: v
    integer j

    jeff = _ERROR(ERR_NOT_FOUND)
    v => NULL()
    jc = extr_h2index(handle)
    jerr = min(0, jc)
    if (jerr.eq.0) then
       xh = query_coor(ctables(jc), name)
       jerr = min(0, xh)
    endif
    if (jerr.eq.0) then
       vser = get_vserial(handle, vid)
       jerr = min(0, vser)
    endif
    if (jerr.eq.0) then
       v => ctables(jc)%v(vser)
       do j = 0, v%neff - 1
          jx = v%ceff(j)
          if (xh.eq.v%xh(jx)) then
             jeff = j
             return
          endif
       enddo
    endif
  end function cache_co_idx

!!!_  - cache_get_header - get all the attributes as single string
  subroutine cache_get_header(ierr, head, handle, vid, rec)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_std,only: WHENCE_BEGIN
    use TOUZA_Nio_header,only: get_item
    use TOUZA_Nio_record,only: nio_read_header
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: head(*)
    integer,         intent(in)  :: handle
    integer,optional,intent(in)  :: vid
    integer,optional,intent(in)  :: rec
    integer jc, gid, rser
    integer ufile, krect
    integer(kind=KIOFS) rpos

    jc = is_valid(handle, vid)
    ierr = min(0, jc)
    if (present(vid)) then
       if (ierr.eq.0) then
          ufile = extr_h2unit(handle)
          rser = get_rserial(handle, vid, choice(0, rec))
          ierr = min(0, rser)
          if (ierr.eq.0) then
             rpos = ctables(jc)%rpos(rser)
             call nio_read_header(ierr, head, krect, ufile, rpos, WHENCE_BEGIN)
          endif
       endif
    else
       if (ierr.eq.0) gid = extr_h2group(handle)
       if (ierr.eq.0) ierr = min(0, gid)
       if (ierr.eq.0) then
          if (gid.eq.grp_suite) then
             ierr = _ERROR(ERR_INVALID_PARAMETER)
          else
             head(1:nitem) = ctables(jc)%g(gid)%h(1:nitem)
          endif
       endif
    endif
  end subroutine cache_get_header

!!!_  - cache_geti_attr - by item id
  subroutine cache_geti_attr_a(ierr, attr, item, handle, vid, rec)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_std,only: WHENCE_BEGIN
    use TOUZA_Nio_header,only: get_item
    use TOUZA_Nio_record,only: nio_read_header
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: attr
    integer,         intent(in)  :: item
    integer,         intent(in)  :: handle
    integer,optional,intent(in)  :: vid
    integer,optional,intent(in)  :: rec
    character(len=litem) :: h(nitem)
    integer jc, gid, rser
    integer ufile, krect
    integer(kind=KIOFS) rpos

    jc = is_valid(handle, vid)
    ierr = min(0, jc)
    if (present(vid)) then
       if (ierr.eq.0) then
          ufile = extr_h2unit(handle)
          rser = get_rserial(handle, vid, choice(0, rec))
          ierr = min(0, rser)
          ! write(*, *) 'geti', ierr, ufile, rser, item
          if (ierr.eq.0) then
             rpos = ctables(jc)%rpos(rser)
             call nio_read_header(ierr, h, krect, ufile, rpos, WHENCE_BEGIN)
             ! write(*, *) 'rpos', ierr, rpos
             if (ierr.eq.0) call get_item(ierr, h, attr, item)
             ! write(*, *) 'item', ierr
          endif
       endif
    else
       if (ierr.eq.0) gid = extr_h2group(handle)
       if (ierr.eq.0) ierr = min(0, gid)
       if (ierr.eq.0) then
          if (gid.eq.grp_suite) then
             ierr = _ERROR(ERR_INVALID_PARAMETER)
          else
             call get_item(ierr, ctables(jc)%g(gid)%h, attr, item)
          endif
       endif
    endif
  end subroutine cache_geti_attr_a
  subroutine cache_geti_attr_i(ierr, attr, item, handle, vid, rec)
    use TOUZA_Nio_std,only: choice, WHENCE_BEGIN
    use TOUZA_Nio_header,only: get_item
    use TOUZA_Nio_record,only: nio_read_header
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: attr
    integer,         intent(in)  :: item
    integer,         intent(in)  :: handle
    integer,optional,intent(in)  :: vid
    integer,optional,intent(in)  :: rec
    character(len=litem) :: h(nitem)
    integer jc, gid, rser
    integer ufile, krect
    integer(kind=KIOFS) rpos

    jc = is_valid(handle, vid)
    ierr = min(0, jc)
    if (present(vid)) then
       if (ierr.eq.0) then
          ufile = extr_h2unit(handle)
          rser = get_rserial(handle, vid, choice(0, rec))
          ierr = min(0, rser)
          if (ierr.eq.0) then
             rpos = ctables(jc)%rpos(rser)
             call nio_read_header(ierr, h, krect, ufile, rpos, WHENCE_BEGIN)
             if (ierr.eq.0) call get_item(ierr, h, attr, item)
          endif
       endif
    else
       if (ierr.eq.0) gid = extr_h2group(handle)
       if (ierr.eq.0) ierr = min(0, gid)
       if (ierr.eq.0) then
          if (gid.eq.grp_suite) then
             ierr = _ERROR(ERR_INVALID_PARAMETER)
          else
             call get_item(ierr, ctables(jc)%g(gid)%h, attr, item)
          endif
       endif
    endif
  end subroutine cache_geti_attr_i
  subroutine cache_geti_attr_f(ierr, attr, item, handle, vid, rec)
    use TOUZA_Nio_std,only: KTGT=>KFLT
    use TOUZA_Nio_std,only: choice, WHENCE_BEGIN
    use TOUZA_Nio_header,only: get_item
    use TOUZA_Nio_record,only: nio_read_header
    implicit none
    integer,         intent(out) :: ierr
    real(kind=KTGT), intent(out) :: attr
    integer,         intent(in)  :: item
    integer,         intent(in)  :: handle
    integer,optional,intent(in)  :: vid
    integer,optional,intent(in)  :: rec
    character(len=litem) :: h(nitem)
    integer jc, gid, rser
    integer ufile, krect
    integer(kind=KIOFS) rpos

    jc = is_valid(handle, vid)
    ierr = min(0, jc)
    if (present(vid)) then
       if (ierr.eq.0) then
          ufile = extr_h2unit(handle)
          rser = get_rserial(handle, vid, choice(0, rec))
          ierr = min(0, rser)
          if (ierr.eq.0) then
             rpos = ctables(jc)%rpos(rser)
             call nio_read_header(ierr, h, krect, ufile, rpos, WHENCE_BEGIN)
             if (ierr.eq.0) call get_item(ierr, h, attr, item)
          endif
       endif
    else
       if (ierr.eq.0) gid = extr_h2group(handle)
       if (ierr.eq.0) ierr = min(0, gid)
       if (ierr.eq.0) then
          if (gid.eq.grp_suite) then
             ierr = _ERROR(ERR_INVALID_PARAMETER)
          else
             call get_item(ierr, ctables(jc)%g(gid)%h, attr, item)
          endif
       endif
    endif
  end subroutine cache_geti_attr_f
  subroutine cache_geti_attr_d(ierr, attr, item, handle, vid, rec)
    use TOUZA_Nio_std,only: KTGT=>KDBL
    use TOUZA_Nio_std,only: choice, WHENCE_BEGIN
    use TOUZA_Nio_header,only: get_item
    use TOUZA_Nio_record,only: nio_read_header
    implicit none
    integer,         intent(out) :: ierr
    real(kind=KTGT), intent(out) :: attr
    integer,         intent(in)  :: item
    integer,         intent(in)  :: handle
    integer,optional,intent(in)  :: vid
    integer,optional,intent(in)  :: rec
    character(len=litem) :: h(nitem)
    integer jc, gid, rser
    integer ufile, krect
    integer(kind=KIOFS) rpos

    jc = is_valid(handle, vid)
    ierr = min(0, jc)
    if (present(vid)) then
       if (ierr.eq.0) then
          ufile = extr_h2unit(handle)
          rser = get_rserial(handle, vid, choice(0, rec))
          ierr = min(0, rser)
          if (ierr.eq.0) then
             rpos = ctables(jc)%rpos(rser)
             call nio_read_header(ierr, h, krect, ufile, rpos, WHENCE_BEGIN)
             if (ierr.eq.0) call get_item(ierr, h, attr, item)
          endif
       endif
    else
       if (ierr.eq.0) gid = extr_h2group(handle)
       if (ierr.eq.0) ierr = min(0, gid)
       if (ierr.eq.0) then
          if (gid.eq.grp_suite) then
             ierr = _ERROR(ERR_INVALID_PARAMETER)
          else
             call get_item(ierr, ctables(jc)%g(gid)%h, attr, item)
          endif
       endif
    endif
  end subroutine cache_geti_attr_d

!!!_  - cache_getn_attr - by item name
  subroutine cache_getn_attr_a(ierr, attr, item, handle, vid, rec)
    use TOUZA_Nio_std,only: choice, WHENCE_BEGIN
    use TOUZA_Nio_header,only: get_item
    use TOUZA_Nio_record,only: nio_read_header
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: attr
    character(len=*),intent(in)  :: item
    integer,         intent(in)  :: handle
    integer,optional,intent(in)  :: vid
    integer,optional,intent(in)  :: rec
    integer jc, gid, rser
    character(len=litem) :: h(nitem)
    integer ufile, krect
    integer(kind=KIOFS) rpos

    jc = is_valid(handle, vid)
    ierr = min(0, jc)
    if (present(vid)) then
       if (ierr.eq.0) then
          ufile = extr_h2unit(handle)
          rser = get_rserial(handle, vid, choice(0, rec))
          ierr = min(0, rser)
          if (ierr.eq.0) then
             rpos = ctables(jc)%rpos(rser)
             call nio_read_header(ierr, h, krect, ufile, rpos, WHENCE_BEGIN)
             if (ierr.eq.0) call get_item(ierr, h, attr, item)
          endif
       endif
    else
       if (ierr.eq.0) gid = extr_h2group(handle)
       if (ierr.eq.0) ierr = min(0, gid)
       if (ierr.eq.0) then
          if (gid.eq.grp_suite) then
             ierr = _ERROR(ERR_INVALID_PARAMETER)
          else
             call get_item(ierr, ctables(jc)%g(gid)%h, attr, item)
          endif
       endif
    endif
  end subroutine cache_getn_attr_a
  subroutine cache_getn_attr_i(ierr, attr, item, handle, vid, rec)
    use TOUZA_Nio_std,only: choice, WHENCE_BEGIN
    use TOUZA_Nio_header,only: get_item
    use TOUZA_Nio_record,only: nio_read_header
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: attr
    character(len=*),intent(in)  :: item
    integer,         intent(in)  :: handle
    integer,optional,intent(in)  :: vid
    integer,optional,intent(in)  :: rec
    integer jc, gid, rser
    character(len=litem) :: h(nitem)
    integer ufile, krect
    integer(kind=KIOFS) rpos

    jc = is_valid(handle, vid)
    ierr = min(0, jc)
    if (present(vid)) then
       if (ierr.eq.0) then
          ufile = extr_h2unit(handle)
          rser = get_rserial(handle, vid, choice(0, rec))
          ierr = min(0, rser)
          if (ierr.eq.0) then
             rpos = ctables(jc)%rpos(rser)
             call nio_read_header(ierr, h, krect, ufile, rpos, WHENCE_BEGIN)
             if (ierr.eq.0) call get_item(ierr, h, attr, item)
          endif
       endif
    else
       if (ierr.eq.0) gid = extr_h2group(handle)
       if (ierr.eq.0) ierr = min(0, gid)
       if (ierr.eq.0) then
          if (gid.eq.grp_suite) then
             ierr = _ERROR(ERR_INVALID_PARAMETER)
          else
             call get_item(ierr, ctables(jc)%g(gid)%h, attr, item)
          endif
       endif
    endif
  end subroutine cache_getn_attr_i
  subroutine cache_getn_attr_f(ierr, attr, item, handle, vid, rec)
    use TOUZA_Nio_std,only: KTGT=>KFLT
    use TOUZA_Nio_std,only: choice, WHENCE_BEGIN
    use TOUZA_Nio_header,only: get_item
    use TOUZA_Nio_record,only: nio_read_header
    implicit none
    integer,         intent(out) :: ierr
    real(kind=KTGT), intent(out) :: attr
    character(len=*),intent(in)  :: item
    integer,         intent(in)  :: handle
    integer,optional,intent(in)  :: vid
    integer,optional,intent(in)  :: rec
    integer jc, gid, rser
    character(len=litem) :: h(nitem)
    integer ufile, krect
    integer(kind=KIOFS) rpos

    jc = is_valid(handle, vid)
    ierr = min(0, jc)
    if (present(vid)) then
       if (ierr.eq.0) then
          ufile = extr_h2unit(handle)
          rser = get_rserial(handle, vid, choice(0, rec))
          ierr = min(0, rser)
          if (ierr.eq.0) then
             rpos = ctables(jc)%rpos(rser)
             call nio_read_header(ierr, h, krect, ufile, rpos, WHENCE_BEGIN)
             if (ierr.eq.0) call get_item(ierr, h, attr, item)
          endif
       endif
    else
       if (ierr.eq.0) gid = extr_h2group(handle)
       if (ierr.eq.0) ierr = min(0, gid)
       if (ierr.eq.0) then
          if (gid.eq.grp_suite) then
             ierr = _ERROR(ERR_INVALID_PARAMETER)
          else
             call get_item(ierr, ctables(jc)%g(gid)%h, attr, item)
          endif
       endif
    endif
  end subroutine cache_getn_attr_f
  subroutine cache_getn_attr_d(ierr, attr, item, handle, vid, rec)
    use TOUZA_Nio_std,only: KTGT=>KDBL
    use TOUZA_Nio_std,only: choice, WHENCE_BEGIN
    use TOUZA_Nio_header,only: get_item
    use TOUZA_Nio_record,only: nio_read_header
    implicit none
    integer,         intent(out) :: ierr
    real(kind=KTGT), intent(out) :: attr
    character(len=*),intent(in)  :: item
    integer,         intent(in)  :: handle
    integer,optional,intent(in)  :: vid
    integer,optional,intent(in)  :: rec
    integer jc, gid, rser
    character(len=litem) :: h(nitem)
    integer ufile, krect
    integer(kind=KIOFS) rpos

    jc = is_valid(handle, vid)
    ierr = min(0, jc)
    if (present(vid)) then
       if (ierr.eq.0) then
          ufile = extr_h2unit(handle)
          rser = get_rserial(handle, vid, choice(0, rec))
          ierr = min(0, rser)
          if (ierr.eq.0) then
             rpos = ctables(jc)%rpos(rser)
             call nio_read_header(ierr, h, krect, ufile, rpos, WHENCE_BEGIN)
             if (ierr.eq.0) call get_item(ierr, h, attr, item)
          endif
       endif
    else
       if (ierr.eq.0) gid = extr_h2group(handle)
       if (ierr.eq.0) ierr = min(0, gid)
       if (ierr.eq.0) then
          if (gid.eq.grp_suite) then
             ierr = _ERROR(ERR_INVALID_PARAMETER)
          else
             call get_item(ierr, ctables(jc)%g(gid)%h, attr, item)
          endif
       endif
    endif
  end subroutine cache_getn_attr_d

!!!_ + derived-type managers
!!!_  - init_group
  subroutine init_group (ierr, grp, recs, vars, coors)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,      intent(out)         :: ierr
    type(group_t),intent(inout)       :: grp
    integer,      intent(in),optional :: recs, vars, coors
    integer lr, lv, lc

    ierr = 0
    grp%nvar = 0
    grp%h(:) = ' '
    grp%name = ' '
    grp%nrec = 0
    grp%ncoor = 0
    ! lv = choice(16, vars) ! hard-coded
    ! lr = choice(12, recs) ! hard-coded
    lv = choice(1, vars) ! hard-coded
    lr = choice(1, recs) ! hard-coded
    lc = choice(lcoor, coors) ! hard-coded
    allocate(grp%v(0:lv-1), &
         &   grp%d(0:lr-1),           grp%t(0:lr-1), &
         &   grp%x(0:lc-1), grp%jbgn(0:lc-1), grp%jend(0:lc-1), &
         &   grp%rpos(0:lr-1,0:lv-1), grp%rlen(0:lr-1,0:lv-1), &
         &   STAT=ierr)
    ! if (ierr.eq.0) then
    !    ! add /empty/ coordinate
    !    ierr = min(0, group_add_coor(grp, ' '))
    ! endif
    if (ierr.ne.0) ierr = _ERROR(ERR_ALLOCATION)
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
       if (ierr.ne.0) ierr = _ERROR(ERR_ALLOCATION)
    endif
    if (mv.gt.0.or.mr.gt.0) then
       nr = size(grp%d)
       lr = nr + mr
       allocate(tmpd(0:lr-1), tmpt(0:lr-1), tmpo(0:lr-1,0:lv-1), tmpl(0:lr-1,0:lv-1), &
            &   STAT=ierr)
       if (ierr.ne.0) ierr = _ERROR(ERR_ALLOCATION)
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
    integer jx
    character(len=128) :: ttmp

    ierr = 0
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)

132 format(A, 3x, '{', I0, '}', 1x, A, 2x, I0, ':', I0)
    if (present(tag)) then
       ttmp = tag
    else
       ttmp = 'coor'
    endif

    ! write(*, *) utmp, lv, c%ncoor

    do jx = 0, c%ncoor - 1
       if (utmp.ge.0) then
          write(utmp, 132) trim(ttmp), jx, trim(c%x(jx)), c%jbgn(jx), c%jend(jx)
       else if (utmp.eq.-1) then
          write(*,    132) trim(ttmp), jx, trim(c%x(jx)), c%jbgn(jx), c%jend(jx)
       endif
    enddo

    if (ierr.eq.0) call show_group(ierr, c, tag=tag, u=utmp, levv=lv)
  end subroutine show_cache_t
  subroutine show_cache_h(ierr, handle, tag, u, levv)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: handle
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer jc, gid
    character(len=128)   :: ttmp
    character(len=lpath) :: file
    integer ufile, utmp
    ierr = 0
    utmp = get_logu(u, ulog)
    jc = extr_h2index(handle)
    gid = extr_h2group(handle)
    ierr = min(0, jc, gid)
    ! if (gid.eq.grp_suite) then

    ! else
       ufile = extr_h2unit(handle)
       if (ierr.eq.0) then
          if (present(tag)) then
             ttmp = tag
          else
101          format('cache[', I0, ']')
             write(ttmp, 101) handle
          endif
       endif
       if (ierr.eq.0) then
          inquire(UNIT=ufile, NAME=file, IOSTAT=ierr)
          if (ierr.ne.0) file = '(unknown)'
          ierr = 0
102       format(A, 1x, A)
          if (utmp.ge.0) then
             write(utmp, 102) trim(ttmp), trim(file)
          else if (utmp.eq.-1) then
             write(*,    102) trim(ttmp), trim(file)
          endif
       endif
       if (ierr.eq.0) then
          call show_cache(ierr, ctables(jc), ttmp, utmp, levv)
       endif
    ! endif
  end subroutine show_cache_h

!!!_  & show_group
  subroutine show_group_t &
       & (ierr, c, gser, tag, u, levv)
    use TOUZA_Nio_std,only: choice, join_list, is_msglev_DETAIL, is_msglev_INFO
    implicit none
    integer,         intent(out)         :: ierr
    type(cache_t),   intent(in)          :: c
    integer,         intent(in),optional :: gser
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv

    character(len=128)   :: ttmp
    integer jg, jgb, jge
    integer jvb, jve

101 format(A, '/', I0)
102 format('group/', I0)

    ierr = 0
    if (present(gser)) then
       jgb = gser
       jge = jgb + 1
       if (gser.lt.0.or.gser.ge.c%ngrp) ierr = _ERROR(ERR_OUT_OF_RANGE)
    else
       jgb = 0
       jge = c%ngrp
    endif

    do jg = jgb, jge - 1
       if (ierr.eq.0) then
          if (present(tag)) then
             write(ttmp, 101) trim(tag), jg
          else
             write(ttmp, 102) jg
          endif
          jvb = c%o(jg)
          jve = c%o(jg + 1)
          call show_group &
               & (ierr, c%g(jg), c%v(jvb:jve-1), c%rpos, c%rlen, ttmp, u, levv)
       endif
    enddo
  end subroutine show_group_t

  subroutine show_group_i &
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
    integer             :: xbufs(0:lcoor-1)
    character(len=lcol) :: cbufs(0:mv-1)
    character(len=lline) :: line
    character(len=litem*2+1) :: dt
    integer jr, jc, nx
    integer rofs

    ierr = 0
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)
101 format(A, 2x, I0, ':', I0, 1x, A)
102 format(A, 3x, 'F', 1x, A)
211 format(A, 3x, I0, ' [', A, '] ', A)
202 format(A, 3x, A)
122 format(A, 3x, 'C', 1x, A)
131 format(A, 1x, '<', A, '>', 1x, I0, 1x, I0)
201 format(Z8.8, '+', Z0)
151 format(A, 3x, I0, 1x '{', I0, '}')


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
          write(utmp, 131) trim(ttmp), trim(grp%name), grp%nvar, grp%ncoor
       else if (utmp.eq.-1) then
          write(*,    131) trim(ttmp), trim(grp%name), grp%nvar, grp%ncoor
       endif
    endif
    if (ierr.eq.0) then
       do jc = 0, grp%ncoor - 1
          if (utmp.ge.0) then
             write(utmp, 151) trim(ttmp), jc, grp%xh(jc)
          else if (utmp.eq.-1) then
             write(*,    151) trim(ttmp), jc, grp%xh(jc)
          endif
       enddo
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
       do jvi = 0, nv - 1
          if (ierr.eq.0) then
             xbufs(0:lcoor-1) = var(jvb+jvi)%xh(0:lcoor-1)
             nx = 0
             do jc = lcoor - 1, 0, -1
                if (xbufs(jc).ge.0) then
                   nx = 1 + jc
                   exit
                endif
             enddo
             call join_list &
                  & (ierr, cbufs(jvi), xbufs(0:nx-1), &
                  &  ldelim='{', rdelim='}', sep=',')
          endif
       enddo
       if (ierr.eq.0) then
          call join_list(ierr, line, cbufs(0:nv-1))
          if (utmp.ge.0) then
             write(utmp, 122) trim(ttmp), trim(line)
          else if (utmp.eq.-1) then
             write(*, 122) trim(ttmp), trim(line)
          endif
       endif
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

  end subroutine show_group_i

!!!_  & scan_file
  subroutine scan_file &
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
    rect = REC_ERROR

    if (ierr.eq.0) rewind(UNIT=u, IOSTAT=ierr)
    if (ierr.eq.0) then
       allocate(grp(0:lg-1), STAT=ierr)
       if (ierr.ne.0) ierr = _ERROR(ERR_ALLOCATION)
    endif
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
          ! write(*, *) 'collate:', jg
          if (jg.lt.0) then
             jg = ng
             if (ng.ge.lg) then
                ! write(*, *) jg, mg, ng, lg
                lg = lg + mg
                allocate(gtmp(0:lg-1), STAT=ierr)
                if (ierr.ne.0) ierr = _ERROR(ERR_ALLOCATION)
                if (ierr.eq.0) then
                   gtmp(0:ng-1)%nvar = grp(0:ng-1)%nvar
                   gtmp(0:ng-1)%nrec = grp(0:ng-1)%nrec
                   gtmp(0:ng-1)%ncoor = grp(0:ng-1)%ncoor
                   gtmp(0:ng-1)%name = grp(0:ng-1)%name
                   do j = 0, ng - 1
                      gtmp(j)%h(:) = grp(j)%h(:)
                      gtmp(j)%v => grp(j)%v
                      gtmp(j)%x => grp(j)%x
                      gtmp(j)%jbgn => grp(j)%jbgn
                      gtmp(j)%jend => grp(j)%jend
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
             if (ierr.eq.0) grp(jg)%name = h(hi_DSET)
             if (is_msglev_DETAIL(lv)) then
                call msg('(''group = '', I0)', (/jg/), __MDL__)
             endif
          endif
       endif
       if (ierr.eq.0) call nio_skip_records(ierr, 1, u, head=h, krect=rect)
       if (ierr.eq.0) inquire(UNIT=u, POS=msize, IOSTAT=ierr)
       if (ierr.eq.0) then
          jv = group_search_var(grp(jg), h)
          jr = group_search_rec(grp(jg), h)
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
             if (is_msglev_DETAIL(lv)) call msg('(''rec = '', I0)', (/jr/), __MDL__)
          endif
          if (newv) then
             jv = grp(jg)%nvar
             grp(jg)%nvar = grp(jg)%nvar + 1
             if (is_msglev_DETAIL(lv)) call msg('(''var = '', I0)', (/jv/), __MDL__)
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
       endif
       if (ierr.eq.0) then
          if (newr) call new_rec(ierr, grp(jg), jr, h)
       endif
       if (ierr.eq.0) msize = msize - apos
       if (ierr.eq.0) then
          grp(jg)%rpos(jr, jv) = rpos
          grp(jg)%rlen(jr, jv) = msize
       endif
       jdrec = jdrec + 1
    enddo
    if (is_error_match(ierr, ERR_EOF)) ierr = 0
    if (ierr.eq.0) then
       c%krect = rect
       c%ngrp = ng
       c%g => grp
    else
       c%ngrp = -1
       c%g => NULL()
    endif
  end subroutine scan_file

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
       dist(j)%neff    = src(j)%neff
       dist(j)%xh(:)   = src(j)%xh(:)
       dist(j)%ceff(:) = src(j)%ceff(:)
       dist(j)%flag    = src(j)%flag
       dist(j)%rofs    = src(j)%rofs
    enddo
  end subroutine copy_var

!!!_  - settle_cache
  subroutine settle_cache(ierr, c, flag)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,      intent(out)         :: ierr
    type(cache_t),intent(inout)       :: c
    integer,      intent(in),optional :: flag

    integer jg, jg2, jt
    integer lv, jv, jvb, jve, nv
    integer lrec, jrb, jre, jro, nr
    integer f
    integer ls
    integer jerr
    character(len=litem*2) :: str

    ierr = 0
    f = choice(coll_default, flag)

    if (ierr.eq.0) call dist_groups(ierr, c)
    if (ierr.eq.0) then
       ! rpos rlen
       lrec = 0
       do jg = 0, c%ngrp - 1
          lrec = lrec + max(0, c%g(jg)%nrec) * max(0, c%g(jg)%nvar)
       enddo
    endif
    if (ierr.eq.0) then
       allocate(c%rpos(0:lrec-1), c%dpos(0:lrec-1), c%rlen(0:lrec-1), STAT=ierr)
       if (ierr.ne.0) ierr = _ERROR(ERR_ALLOCATION)
    endif
    if (ierr.eq.0) then
       c%dpos(0:lrec-1) = -1
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
       if (ierr.ne.0) ierr = _ERROR(ERR_ALLOCATION)
    endif
    if (ierr.eq.0) c%o(0) = 0
    if (ierr.eq.0) call collect_coor(ierr, c, flag)

    jvb = 0
    do jg = 0, c%ngrp - 1
       nv = max(0, c%g(jg)%nvar)
       jve = jvb + nv
       if (ierr.eq.0) call settle_group(ierr, c%g(jg), jvb, jg, flag)
       if (ierr.eq.0) call copy_var(c%v(jvb:jve-1), c%g(jg)%v(0:nv-1), nv)
       if (ierr.eq.0) c%o(jg+1) = jve
       jvb = jve
    enddo
    ! duplicate group name adjustment
    if (IAND(f, allow_grp_dup).eq.0) then
201    format(A, A, I0)
       ls = max(1, len_trim(dup_sep))
       do jg = 0, c%ngrp - 1
          if (index(c%g(jg)%name, dup_sep(1:ls)).gt.0) cycle
          if (ANY(c%g(jg)%name.eq.c%g(jg+1:c%ngrp)%name)) then
             jt = 0
             do jg2 = jg + 1, c%ngrp - 1
                if (c%g(jg)%name.eq.c%g(jg2)%name) then
                   jt = jt + 1
                   write(c%g(jg2)%name, 201, IOSTAT=jerr) trim(c%g(jg)%name), dup_sep(1:ls), jt
                endif
             enddo
             jt = 0
             write(str, 201, IOSTAT=jerr) trim(c%g(jg)%name), dup_sep(1:ls), jt
             c%g(jg)%name = trim(str)
          endif
       enddo
    endif

    ! release temporal variable properties
    do jg = 0, c%ngrp - 1
       if (ierr.eq.0) then
          deallocate(c%g(jg)%v, c%g(jg)%x, c%g(jg)%jbgn, c%g(jg)%jend, &
               &     c%g(jg)%rpos, c%g(jg)%rlen, STAT=ierr)
       endif
       !! ! Following works, but need to adjust index (starting from 1).
       ! if (ierr.eq.0) c%g(jg)%v => c%v(c%o(jg):)
       !! ! Requires Fortran 2003, reserved.
       ! if (ierr.eq.0) c%g(jg)%v(0:) => c%v(c%o(jg):)
       c%g(jg)%v => NULL()
       c%g(jg)%x => NULL()
       c%g(jg)%jbgn => NULL()
       c%g(jg)%jend => NULL()
       c%g(jg)%rpos => NULL()
       c%g(jg)%rlen => NULL()
    enddo
    ! After settlement:
    !   Entity of variable properties are stored in c%v(:)
    !   g(:)%v points to corresponding head of c%v(:)
  end subroutine settle_cache

!!!_  . dist_groups
  subroutine dist_groups(ierr, c)
    implicit none
    integer,      intent(out)   :: ierr
    type(cache_t),intent(inout) :: c
    integer jg, lg, ng
    integer jv, nv, lv
    integer jvref, jvoff, jvnew
    integer jr, nr, jrnew
    integer nc

    logical bdist

    type(group_t),pointer :: grp(:)
    integer jgnew, ngnew
    integer nvnew
    integer,allocatable :: gdist(:), gofs(:), grecs(:), gvars(:)

    ierr = 0

    bdist = .FALSE.
    ng = c%ngrp
    ngnew = 0

    ! if (ierr.eq.0) then
    !    do jg = 0, ng - 1
    !       ! write(*, *) 'distg:coor:0', c%g(jg)%xh(:)
    !       nr = max(0, c%g(jg)%nrec)
    !       do jr = 0, nr - 1
    !          write(*, *) 'distg:date:0', jg, jr, c%g(jg)%d(jr)
    !          write(*, *) 'distg:time:0', jg, jr, c%g(jg)%t(jr)
    !       enddo
    !       do jv = 0, c%g(jg)%nvar - 1
    !          ! write(*, *) 'distg:var:0', c%g(jg)%v(jv)
    !          write(*, *) 'distg:pos:0', jg, jv, c%g(jg)%rpos(0:nr-1, jv)
    !          write(*, *) 'distg:len:0', jg, jv, c%g(jg)%rlen(0:nr-1, jv)
    !       enddo
    !    enddo
    ! endif
    if (ierr.eq.0) then
       outer: do jg = 0, ng - 1
          nr = max(0, c%g(jg)%nrec)
          do jv = 0, c%g(jg)%nvar - 1
             bdist = ANY(c%g(jg)%rlen(0:nr-1, jv).lt.0)
             if (bdist) exit outer
          enddo
       enddo outer
    endif
    if (.not.bdist) return
    if (ierr.eq.0) then
       lv = SUM(max(0, c%g(0:ng-1)%nvar))
       lg = lv
       allocate(gdist(0:lv-1), grecs(0:lg-1), gvars(0:lg-1), &
            &   gofs(0:c%ngrp), STAT=ierr)
    endif
    if (ierr.eq.0) then
       ! write(*, *) 'distg:bool:', bdist
       gofs(0) = 0
       gdist(0:lv-1) = -1
       jgnew = 0
       jvoff = 0
       do jg = 0, c%ngrp - 1
          nr = max(0, c%g(jg)%nrec)
          nv = max(0, c%g(jg)%nvar)
          do jvref = 0, c%g(jg)%nvar - 1
             if (gdist(jvoff + jvref).ge.0) cycle
             gdist(jvoff + jvref) = jgnew
             nvnew = 0
             ! write(*, *) 'distg:pattern:', jg, jvref, jgnew, c%g(jg)%rlen(0:nr-1, jvref).ge.0
             do jv = jvref + 1, c%g(jg)%nvar - 1
                if (ALL((c%g(jg)%rlen(0:nr-1, jvref).ge.0) &
                     & .eqv. (c%g(jg)%rlen(0:nr-1, jv).ge.0))) then
                   ! write(*, *) 'distg:found:  ', jg, jv, jgnew, c%g(jg)%rlen(0:nr-1, jvref).ge.0
                   gdist(jvoff + jv) = jgnew
                   nvnew = nvnew + 1
                endif
             enddo
             grecs(jgnew) = COUNT(c%g(jg)%rlen(0:nr-1, jvref).ge.0)
             gvars(jgnew) = nvnew + 1
             jgnew = jgnew + 1
          enddo
          gofs(jg+1) = jgnew
          jvoff = jvoff + nv
       enddo
       ngnew = jgnew
       ! write(*, *) 'distg:grecs: ', grecs(0:ngnew-1)
       ! write(*, *) 'distg:gvars: ', gvars(0:ngnew-1)
    endif
    if (ierr.eq.0) allocate(grp(0:ngnew-1), STAT=ierr)
    if (ierr.eq.0) then
       jvoff = 0
       do jg = 0, c%ngrp - 1
          ! write(*, *) 'distg:old:', c%g(jg)%nvar, c%g(jg)%nrec, c%g(jg)%ncoor
          ! write(*, *) 'distg:old:', c%g(jg)%name, c%g(jg)%h(1)
          do jgnew = gofs(jg), gofs(jg + 1) - 1
             nv = gvars(jgnew)
             nr = grecs(jgnew)
             nc = c%g(jg)%ncoor   ! to be improved
             ! cxtbl(0:nc-1) = 0
             ! if (ierr.eq.0) then
             !    do jv = 0, c%g(jg)%nvar - 1
             !       if (gdist(jvoff + jv).ne.jgnew) cycle
             !       do jc = 0, lcoor - 1
             !          xh = c%g(jg)%v(jv)%xh(jc)
             !          if (xh.ge.0) cxtbl(xh) = cxtbl(xh) + 1
             !       enddo
             !    enddo
             !    write(*, *) 'distg:cxtab:', jgnew, cxtbl(0:nc-1)
             ! endif

             ! write(*, *) 'distg:trans: ', jg, jgnew, nr, nv, nc
             if (ierr.eq.0) call init_group(ierr, grp(jgnew), nr, nv, nc)
             if (ierr.eq.0) then
                grp(jgnew)%x(0:nc-1) = c%g(jg)%x(0:nc-1)
                grp(jgnew)%jbgn(0:nc-1) = c%g(jg)%jbgn(0:nc-1)
                grp(jgnew)%jend(0:nc-1) = c%g(jg)%jend(0:nc-1)
             endif
             ! find reference variable
             if (ierr.eq.0) then
                jvref = -1
                do jv = 0, c%g(jg)%nvar - 1
                   if (gdist(jvoff + jv).eq.jgnew) then
                      jvref = jv
                      exit
                   endif
                enddo
                if (jvref.lt.0) ierr = _ERROR(ERR_PANIC)
             endif
             ! write(*, *) 'distg:ref: ', jg, jgnew, jvref
             if (ierr.eq.0) then
                grp(jgnew)%nvar = nv
                grp(jgnew)%nrec = nr
                grp(jgnew)%name = c%g(jg)%name
                grp(jgnew)%h(:) = c%g(jg)%h(:)
                grp(jgnew)%ncoor = c%g(jg)%ncoor

                jrnew = 0
                do jr = 0, c%g(jg)%nrec - 1
                   if (c%g(jg)%rlen(jr, jvref).ge.0) then
                      grp(jgnew)%d(jrnew) = c%g(jg)%d(jr)
                      grp(jgnew)%t(jrnew) = c%g(jg)%t(jr)
                      jrnew = jrnew + 1
                   endif
                enddo

                jvnew = 0
                do jv = 0, c%g(jg)%nvar - 1
                   if (gdist(jvoff + jv).ne.jgnew) cycle
                   jrnew = 0
                   grp(jgnew)%v(jvnew) = c%g(jg)%v(jv)
                   do jr = 0, c%g(jg)%nrec - 1
                      if (c%g(jg)%rlen(jr, jv).ge.0) then
                         grp(jgnew)%rlen(jrnew, jvnew) = c%g(jg)%rlen(jr, jv)
                         grp(jgnew)%rpos(jrnew, jvnew) = c%g(jg)%rpos(jr, jv)
                         jrnew = jrnew + 1
                      endif
                   enddo
                   jvnew = jvnew + 1
                enddo
             endif
          enddo
          jvoff = jvoff + c%g(jg)%nvar
       enddo
    endif
    ! if (ierr.eq.0) then
    !    do jg = 0, ngnew - 1
    !       nr = max(0, grp(jg)%nrec)
    !       nc = max(0, grp(jg)%ncoor)
    !       nv = max(0, grp(jg)%nvar)
    !       write(*, *) 'distg:dim:9 ', jg, nr, nv, nc
    !       do jr = 0, nr - 1
    !          write(*, *) 'distg:date:9', jg, jr, grp(jg)%d(jr)
    !          write(*, *) 'distg:time:9', jg, jr, grp(jg)%t(jr)
    !       enddo
    !       do jv = 0, grp(jg)%nvar - 1
    !          ! write(*, *) 'distg:var:9', grp(jg)%v(jv)
    !          write(*, *) 'distg:pos:9', jg, jv, grp(jg)%rpos(0:nr-1, jv)
    !          write(*, *) 'distg:len:9', jg, jv, grp(jg)%rlen(0:nr-1, jv)
    !       enddo
    !    enddo
    ! endif
    do jg = 0, c%ngrp - 1
       if (ierr.eq.0) call free_group(ierr, c%g(jg))
    enddo
    if (ierr.eq.0) deallocate(c%g, STAT=ierr)
    if (ierr.eq.0) then
       c%g => grp
       c%ngrp = ngnew
    endif
    if (ierr.eq.0) deallocate(gdist, grecs, gofs, gvars, STAT=ierr)
  end subroutine dist_groups

!!!_  - collect_coor
  subroutine collect_coor(ierr, c, flag)
    use TOUZA_Nio_std, only: find_first, choice
    implicit none
    integer,      intent(out)         :: ierr
    type(cache_t),intent(inout)       :: c
    integer,      intent(in),optional :: flag
    integer jg
    integer jx, jt, jv
    integer jc, jc2, nc, mc
    integer nttl
    integer ls, jerr
    integer f
    character(len=litem*2) :: str
    character(len=litem),allocatable :: xtmp(:)
    integer,             allocatable :: xbgn(:), xend(:)
    integer,             allocatable :: ktmp(:), kco(:)

    ierr = 0
    nttl = 0
    do jg = 0, c%ngrp - 1
       nttl = nttl + c%g(jg)%ncoor
    enddo
    allocate(xtmp(0:nttl-1), &
         &   xbgn(0:nttl-1), xend(0:nttl-1), &
         &   ktmp(0:nttl-1), kco(0:nttl-1), STAT=ierr)
    ! do jg = 0, c%ngrp - 1
    !    if (ierr.eq.0) then
    !       nttl = c%g(jg)%ncoor
    !       allocate(c%g(jg)%xh(0:nttl-1), STAT=ierr)
    !    endif
    ! enddo
    if (ierr.eq.0) then
       nttl = 0
       do jg = 0, c%ngrp - 1
          mc = c%g(jg)%ncoor
          ktmp(0:mc-1) = -1
          do jx = 0, mc - 1
             jt = -1
             do
                jt = find_first(xtmp(0:nttl-1), c%g(jg)%x(jx), start=jt+1)
                if (jt.lt.0) exit
                if (xbgn(jt).ne.c%g(jg)%jbgn(jx)) cycle
                if (xend(jt).ne.c%g(jg)%jend(jx)) cycle
                exit
             enddo
             if (jt.lt.0) then
                jt = nttl
                xtmp(jt) = c%g(jg)%x(jx)
                xbgn(jt) = c%g(jg)%jbgn(jx)
                xend(jt) = c%g(jg)%jend(jx)
                nttl = nttl + 1
             endif
             ! c%g(jg)%xh(jx) = jt
             ktmp(jx) = jt
          enddo
          kco(0:mc-1) = 0
          do jv = 0, c%g(jg)%nvar - 1
             do jx = 0, lcoor - 1
                jt = c%g(jg)%v(jv)%xh(jx)
                ! if (jt.ge.0) c%g(jg)%v(jv)%xh(jx) = ktmp(jt)
                if (jt.ge.0) then
                   ! c%g(jg)%v(jv)%xh(jx) = c%g(jg)%xh(jt)
                   c%g(jg)%v(jv)%xh(jx) = ktmp(jt)
                   kco(jt) = kco(jt) + 1
                endif
             enddo
          enddo
          nc = COUNT(kco(0:mc-1).gt.0)
          allocate(c%g(jg)%xh(0:nc-1), STAT=ierr)
          if (ierr.ne.0) exit
          jc = 0
          do jt = 0, mc - 1
             if (kco(jt).gt.0) then
                c%g(jg)%xh(jc) = ktmp(jt)
                jc = jc + 1
             endif
          enddo
          c%g(jg)%ncoor = nc
          ! write(*, *) 'kco:', jg, kco(0:mc-1)
          ! write(*, *) 'kcx:', jg, ktmp(0:mc-1)
       enddo
    endif
    if (ierr.eq.0) allocate(c%x(0:nttl-1), c%jbgn(0:nttl-1), c%jend(0:nttl-1), STAT=ierr)
    if (ierr.eq.0) then
       c%x(0:nttl-1) = xtmp(0:nttl-1)
       c%jbgn(0:nttl-1) = xbgn(0:nttl-1)
       c%jend(0:nttl-1) = xend(0:nttl-1)
       c%ncoor = nttl
    endif
    if (ierr.eq.0) then
       f = choice(coll_default, flag)
201    format(A, A, I0)
       ls = max(1, len_trim(dup_sep))
       if (IAND(flag, allow_coor_dup).eq.0) then
          do jc = 0, nttl - 1
             if (index(c%x(jc), dup_sep(1:ls)).gt.0) cycle
             if (ANY(c%x(jc).eq.c%x(jc+1:nttl-1))) then
                jc2 = 0
                do jt = jc + 1, nttl - 1
                   if (c%x(jc).eq.c%x(jt)) then
                      jc2 = jc2 + 1
                      write(c%x(jt), 201, IOSTAT=jerr) trim(c%x(jc)), dup_sep(1:ls), jc2
                   endif
                enddo
                jc2 = 0
                write(str, 201, IOSTAT=jerr) trim(c%x(jc)), dup_sep(1:ls), jc2
                c%x(jc) = trim(str)
             endif
          enddo
       endif
    endif

    if (ierr.eq.0) deallocate(xtmp, xbgn, xend, ktmp, kco, STAT=ierr)

    if (ierr.ne.0) ierr = _ERROR(ERR_ALLOCATION)
  end subroutine collect_coor

!!!_  - settle_group
  subroutine settle_group(ierr, grp, jvoff, jgrp, flag)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_record,only: put_header_cprop
    use TOUZA_Nio_header,only: put_item, hi_ITEM, hi_DFMT, hi_TITL1, hi_ETTL1, hi_UNIT
    implicit none
    integer,      intent(out)         :: ierr
    type(group_t),intent(inout)       :: grp
    integer,      intent(in)          :: jvoff
    integer,      intent(in)          :: jgrp
    integer,      intent(in),optional :: flag
    integer jv,  jt,  jc
    integer jvb, jve
    integer jrb, jre
    integer jgb, jge
    integer jerr
    integer ls
    character(len=litem*2) :: str
    integer f

    ierr = 0
    f = choice(coll_default, flag)
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
    if (IAND(flag, allow_var_dup).eq.0) then
201    format(A, A, I0)
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
    endif
  end subroutine settle_group

!!!_  - collate_header
  subroutine collate_header &
       & (ierr, jgrp, grp, ngrp, head, flag, emask)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_std,only: msg, is_msglev_debug
    use TOUZA_Nio_header
    use TOUZA_Nio_axis,only: is_axis_set
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
    integer ji
    character(len=128) :: txt

    ierr = 0

    if (present(emask)) then
       msk(1:nitem) = emask(1:nitem)
    else
       ! coll_default must be 0
       ! coll_nonum must be 1...1
       f = choice(coll_default, flag)
       f = IAND(f, coll_nonum + coll_nospecial)
       if (IAND(f, coll_nonum).eq.coll_default) f = IOR(f, coll_nosign)
       msk(:) = .FALSE.
       gen_mask: do
          if (IAND(f, coll_nospecial).gt.0) then
             if (is_axis_set(head)) then
!!$                msk(:hi_DSET-1) = .TRUE.
!!$                msk(hi_DSET+1:) = .TRUE.
                msk(hi_DSET) = .TRUE.
                exit gen_mask
             endif
          endif
          if (IAND(f, collm_std).gt.0) then
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
          if (IAND(f, collm_basic).gt.0) then
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
          if (IAND(f, collm_nosign).gt.0) then
             msk(hi_CDATE) = .TRUE.
             msk(hi_CSIGN) = .TRUE.
             msk(hi_MDATE) = .TRUE.
             msk(hi_MSIGN) = .TRUE.
          endif
          if (IAND(f, collm_nonum).gt.0) then
             msk(hi_DSET) = .TRUE.
             msk(hi_FNUM) = .TRUE.
             msk(hi_DNUM) = .TRUE.
          endif
          exit gen_mask
       enddo gen_mask
    endif
    jgrp = -1
    do j = 0, ngrp - 1
       if (is_msglev_DEBUG(lev_verbose)) then
          do ji = 1, nitem
             if (grp(j)%h(ji).ne.head(ji)) then
101             format('collate ', I0, 1x, I0, 1x, &
                     & '[', A, ']', 1x, &
                     & '[', A, ']', 1x, L1)
                write(txt, 101) j, ji, trim(grp(j)%h(ji)), trim(head(ji)), msk(ji)
                call msg(txt)
             endif
          enddo
       endif
       if (ALL(grp(j)%h(1:nitem).eq.head(1:nitem) .or. msk(1:nitem))) then
          jgrp = j
          exit
       endif
    enddo
    ! call show_header(ierr, head)
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
    integer xh
    character(len=litem) :: name
    integer irange(2), jbgn, jend
    integer neff

    ierr = 0
    ! write(*, *) 'new_var', jvar
    if (ierr.eq.0) then
       grp%v(jvar)%item =  head(hi_ITEM)
       grp%v(jvar)%unit =  head(hi_UNIT)
       grp%v(jvar)%dfmt =  head(hi_DFMT)
       grp%v(jvar)%ceff(:) = -1
       neff = 0
       do jc = 0, lcoor - 1
          if (ierr.eq.0) then
             call get_header_cprop(name, irange, head, 1+jc)
             jbgn = irange(1) - 1
             jend = irange(2)
             if (name.ne.' '.or.irange(2)-irange(1).gt.1) then
                xh = group_search_coor(grp, name, jbgn, jend)
                if (xh.lt.0) xh = group_add_coor(grp, name, jbgn, jend)
                ierr = min(0, xh)
                if (ierr.eq.0) then
                   grp%v(jvar)%ceff(neff) = jc
                   grp%v(jvar)%xh(neff) = xh
                   neff = neff + 1
                endif
             endif
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

!!!_  & group_add_coor()
  integer function group_add_coor(grp, coor, jbgn, jend) result(xh)
    implicit none
    type(group_t),   intent(inout) :: grp
    character(len=*),intent(in)    :: coor
    integer,         intent(in)    :: jbgn, jend

    character(len=litem),pointer :: tmpx(:)
    integer,             pointer :: tmpb(:), tmpe(:)
    integer m, n
    integer jerr

    jerr = 0
    xh = grp%ncoor
    m = size(grp%x)
    if (xh.ge.m) then
       n = max(m + cmdl, xh + 1)
       allocate(tmpx(0:n-1), tmpb(0:n-1), tmpe(0:n-1), STAT=jerr)
       if (jerr.eq.0) then
          tmpx(0:m-1) = grp%x(0:m-1)
          tmpb(0:m-1) = grp%jbgn(0:m-1)
          tmpe(0:m-1) = grp%jend(0:m-1)
          deallocate(grp%x, grp%jbgn, grp%jend, STAT=jerr)
       endif
       if (jerr.eq.0) grp%x => tmpx
       if (jerr.eq.0) grp%jbgn => tmpb
       if (jerr.eq.0) grp%jend => tmpe
       if (jerr.ne.0) jerr = _ERROR(ERR_ALLOCATION)
    endif
    if (jerr.eq.0) then
       grp%x(xh) = trim(coor)
       grp%jbgn(xh) = jbgn
       grp%jend(xh) = jend
       grp%ncoor = grp%ncoor + 1
    else
       xh = jerr
    endif
  end function group_add_coor

!!!_  & group_search_coor()
  integer function group_search_coor(grp, coor, jbgn, jend) result(xh)
    implicit none
    type(group_t),   intent(in) :: grp
    character(len=*),intent(in) :: coor
    integer,         intent(in) :: jbgn, jend
    integer jx
    xh = _ERROR(ERR_NOT_FOUND)
    do jx = 0, grp%ncoor - 1
       if (grp%x(jx).eq.coor) then
          if (grp%jbgn(jx).eq.jbgn.and.grp%jend(jx).eq.jend) then
             xh = jx
             exit
          endif
       endif
    enddo
  end function group_search_coor

!!!_  & group_search_var()
  integer function group_search_var(grp, head) result(v)
    use TOUZA_Nio_record,only: get_header_cprop, get_header_cname
    use TOUZA_Nio_header
    implicit none
    type(group_t),   intent(in)  :: grp
    character(len=*),intent(in)  :: head(*)
    integer j, jc
    character(len=litem) :: name
    integer irange(2), jbgn, jend
    integer xh, nx
    integer xhsrc(0:lcoor-1)

    v = _ERROR(ERR_NOT_FOUND)

    nx = 0
    do jc = 0, lcoor - 1
       call get_header_cprop(name, irange, head, 1+jc)
       jbgn = irange(1) - 1
       jend = irange(2)
       xh = group_search_coor(grp, name, jbgn, jend)
       if (xh.ge.0) then
          xhsrc(nx) = xh
          nx = nx + 1
       endif
    enddo

    loop_var: do j = 0, grp%nvar - 1
       if (grp%v(j)%neff.ne.nx) cycle loop_var
       if (ANY(grp%v(j)%xh(0:nx-1).ne.xhsrc(0:nx-1))) cycle loop_var
       if (grp%v(j)%item.ne.head(hi_ITEM)) cycle loop_var
       if (grp%v(j)%unit.ne.head(hi_UNIT)) cycle loop_var

       v = j
       return
    enddo loop_var
  end function group_search_var

!!!_  & group_search_rec()
  integer function group_search_rec(grp, head) result(r)
    use TOUZA_Nio_header
    implicit none
    type(group_t),   intent(in)  :: grp
    character(len=*),intent(in)  :: head(*)
    integer j

    ! used only for cache builds
    loop_rec: do j = 0, grp%nrec - 1
       if (grp%d(j).ne.head(hi_DATE)) cycle loop_rec
       if (grp%t(j).ne.head(hi_TIME)) cycle loop_rec
       r = j
       return
    enddo loop_rec
    r = _ERROR(ERR_NOT_FOUND)
  end function group_search_rec

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
    if (ierr.eq.0) then
       if (associated(c%g)) deallocate(c%g, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (associated(c%v)) deallocate(c%v, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (associated(c%o)) deallocate(c%o, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (associated(c%rpos)) deallocate(c%rpos, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (associated(c%dpos)) deallocate(c%dpos, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (associated(c%rlen)) deallocate(c%rlen, STAT=ierr)
    endif
    if (ierr.eq.0) then
       c%g => NULL()
       c%v => NULL()
       c%o => NULL()
       c%rpos => NULL()
       c%dpos => NULL()
       c%rlen => NULL()
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
       if (associated(grp%d)) deallocate(grp%d, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (associated(grp%xh)) deallocate(grp%xh, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (associated(grp%t)) deallocate(grp%t, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (associated(grp%rpos)) deallocate(grp%rpos, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (associated(grp%rlen)) deallocate(grp%rlen, STAT=ierr)
    endif
    ! deallocate(grp%d, grp%t, grp%rpos, grp%rlen, STAT=ierr)
    if (ierr.eq.0) then
       grp%v => NULL()
       grp%d => NULL()
       grp%t => NULL()
       grp%xh => NULL()
       grp%rpos => NULL()
       grp%rlen => NULL()
       grp%nvar = -1
       grp%nrec = -1
       grp%ncoor = -1
       grp%name = ' '
       grp%h(:) = ' '
    endif
  end subroutine free_group
!!!_ + cached file access
! !!!_  - cache_gvr2rindex() - return /suite/ record index
!   integer function cache_gvr2rindex(handle, gid, vid, rec) result(ridx)
!     implicit none
!     integer,intent(in) :: handle
!     integer,intent(in) :: gid
!     integer,intent(in) :: vid
!     integer,intent(in) :: rec
!     integer jerr
!     integer jc, jg, jv
!     jc = is_valid(handle, gid, vid)
!     jerr = min(0, jc)
!     if (jerr.eq.0) then
!        if (vid.eq.vid_suite) then
!           if (rec.ge.0.and.rec.lt.ctables(jc)%lrec) then
!              ridx = rec
!           else
!              jerr = -1
!           endif
!        else
!           jg = cache_gv2gindex(handle, gid, vid)
!           jv = cache_gv2vindex(handle, gid, vid)
!           jerr = min(0, jg, jv)
!           if (jerr.eq.0) then
!              if (rec.ge.0.and.rec.lt.ctables(jc)%g(jg)%nrec) then
!                 ridx = rec + ctables(jc)%v(jv)%rofs
!              else
!                 jerr = -1
!              endif
!           endif
!        endif
!     endif
!     if (jerr.ne.0) then
!        ridx = _ERROR(ERR_INVALID_PARAMETER)
!     endif
!   end function cache_gvr2rindex
! !!!_  - cache_gv2vindex() - return /suite/ variable index
!   integer function cache_gv2vindex(handle, gid, vid) result(jv)
!     implicit none
!     integer,intent(in) :: handle
!     integer,intent(in) :: gid
!     integer,intent(in) :: vid
!     integer jc
!     integer jerr
!     jc = is_valid(handle, gid, vid)
!     jerr = min(0, jc)
!     if (jerr.eq.0) then
!        if (gid.eq.gid_suite) then
!           jv = vid
!        else
!           jv = ctables(jc)%o(gid) + vid
!        endif
!     else
!        jv = jerr
!     endif
!   end function cache_gv2vindex

!!!_ + cached file manager
!!!_  & cache_inquire
!!!_  & cache_inquire_variable
!!!_  & cache_inquire_dimension
!!!_ + cache table manager
!!!_ + user interfaces
!!!_  & cache_open
!!!_  & cache_close
!!!_  & cache_read_header
  subroutine cache_read_header &
       & (ierr, head, handle, vid, rec, krect)
    use TOUZA_Nio_std,only: KIOFS, WHENCE_BEGIN, set_if_present, sus_getpos
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: head(*)
    integer,         intent(in)  :: handle, vid
    integer,         intent(in)  :: rec
    integer,optional,intent(out) :: krect
    integer jc
    integer ufile
    integer rser

    ierr = 0
    jc = is_valid(handle, vid)
    ierr = min(0, jc)
    if (ierr.eq.0) then
       ufile = extr_h2unit(handle)
       rser = get_rserial(handle, vid, rec)
       call cue_read_header(ierr, head, ctables(jc), ufile, rser)
       if (ierr.eq.0) then
          call set_if_present(krect, ctables(jc)%krect)
       endif
    endif
  end subroutine cache_read_header
!!!_  & cache_write_header
!!!_  & cache_read_data
!!!_  & cache_var_read
  subroutine cache_var_read_i &
       & (ierr, d, handle, vid, rec, start, count)
    use TOUZA_Nio_std,only: KIOFS, WHENCE_BEGIN
    use TOUZA_Nio_record,only: nio_read_data
    integer,intent(out)         :: ierr
    integer,intent(out)         :: d(*)
    integer,intent(in)          :: handle, vid
    integer,intent(in)          :: rec
    integer,intent(in),optional :: start(0:*), count(0:*)
    integer jc, vser
    type(group_t),pointer :: g
    type(var_t),pointer :: v
    integer ofs(0:lcoor-1), mem(0:lcoor-1)
    character(len=litem) :: head(nitem)
    integer krect, ufile
    integer n
    integer rser

    ierr = 0
    v => NULL()
    g => NULL()
    jc = is_valid(handle, vid)
    ierr = min(0, jc)
    if (ierr.eq.0) then
       ufile = extr_h2unit(handle)
       rser = get_rserial(handle, vid, rec)
       call cue_read_header(ierr, head, ctables(jc), ufile, rser)
       if (ierr.eq.0) krect = ctables(jc)%krect
    endif
    if (ierr.eq.0) then
       vser = get_vserial(handle, vid)
       ierr = min(0, vser)
    endif
    if (ierr.eq.0) then
       v => ctables(jc)%v(vser)
       call cache_var_slice(ofs, mem, lcoor, v, ctables(jc), start, count)
       n = -1
       call nio_read_data &
            & (ierr, d, n, head, krect, ufile, start=ofs(0:v%neff-1), count=mem(0:v%neff-1))
    endif
    call trace_err(ierr, 'cache_var_read')
  end subroutine cache_var_read_i
  subroutine cache_var_read_f &
       & (ierr, d, handle, vid, rec, start, count)
    use TOUZA_Nio_std,only: KTGT=>KFLT, KIOFS, WHENCE_BEGIN
    use TOUZA_Nio_record,only: nio_read_data
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(out)         :: d(*)
    integer,        intent(in)          :: handle, vid
    integer,        intent(in)          :: rec
    integer,        intent(in),optional :: start(0:*), count(0:*)
    integer jc, vser
    type(group_t),pointer :: g
    type(var_t),pointer :: v
    integer ofs(0:lcoor-1), mem(0:lcoor-1)
    character(len=litem) :: head(nitem)
    integer krect, ufile
    integer n
    integer rser

    ierr = 0
    v => NULL()
    g => NULL()
    jc = is_valid(handle, vid)
    ierr = min(0, jc)
    if (ierr.eq.0) then
       ufile = extr_h2unit(handle)
       rser = get_rserial(handle, vid, rec)
       call cue_read_header(ierr, head, ctables(jc), ufile, rser)
       if (ierr.eq.0) krect = ctables(jc)%krect
    endif
    if (ierr.eq.0) then
       vser = get_vserial(handle, vid)
       ierr = min(0, vser)
    endif
    if (ierr.eq.0) then
       v => ctables(jc)%v(vser)
       call cache_var_slice(ofs, mem, lcoor, v, ctables(jc), start, count)
       n = -1
       call nio_read_data &
            & (ierr, d, n, head, krect, ufile, start=ofs(0:v%neff-1), count=mem(0:v%neff-1))
    endif
    ! write(*, *) 'cache_var_read', ierr, n, handle, vid, vser, rser
    call trace_err(ierr, 'cache_var_read')
  end subroutine cache_var_read_f
  subroutine cache_var_read_d &
       & (ierr, d, handle, vid, rec, start, count)
    use TOUZA_Nio_std,only: KTGT=>KDBL, KIOFS, WHENCE_BEGIN
    use TOUZA_Nio_record,only: nio_read_data
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(out)         :: d(*)
    integer,        intent(in)          :: handle, vid
    integer,        intent(in)          :: rec
    integer,        intent(in),optional :: start(0:*), count(0:*)
    integer jc, vser
    type(group_t),pointer :: g
    type(var_t),pointer :: v
    integer ofs(0:lcoor-1), mem(0:lcoor-1)
    character(len=litem) :: head(nitem)
    integer krect, ufile
    integer n
    integer rser

    ierr = 0
    v => NULL()
    g => NULL()
    jc = is_valid(handle, vid)
    ierr = min(0, jc)
    if (ierr.eq.0) then
       ufile = extr_h2unit(handle)
       rser = get_rserial(handle, vid, rec)
       call cue_read_header(ierr, head, ctables(jc), ufile, rser)
       if (ierr.eq.0) krect = ctables(jc)%krect
    endif
    if (ierr.eq.0) then
       vser = get_vserial(handle, vid)
       ierr = min(0, vser)
    endif
    if (ierr.eq.0) then
       v => ctables(jc)%v(vser)
       call cache_var_slice(ofs, mem, lcoor, v, ctables(jc), start, count)
       n = -1
       call nio_read_data &
            & (ierr, d, n, head, krect, ufile, start=ofs(0:v%neff-1), count=mem(0:v%neff-1))
    endif
    call trace_err(ierr, 'cache_var_read')
  end subroutine cache_var_read_d

!!!_  & cache_var_slice
  subroutine cache_var_slice &
       & (ofs, mem, lcoor, v, c, start, count)
    implicit none
    integer,      intent(out)         :: ofs(0:*)
    integer,      intent(out)         :: mem(0:*)
    integer,      intent(in)          :: lcoor
    type(var_t),  intent(in)          :: v
    type(cache_t),intent(in)          :: c
    integer,      intent(in),optional :: start(0:*), count(0:*)
    integer jeff, jco, xh

    ofs(0:lcoor-1) = 0
    ! mem(0:lcoor-1) = v%jend(0:lcoor-1) - v%jbgn(0:lcoor-1)
    do jco = 0, lcoor - 1
       xh = v%xh(jco)
       if (xh.ge.0) then
          mem(jco) = c%jend(xh) - c%jbgn(xh)
       else
          mem(jco) = 0
       endif
    enddo
    if (present(start)) then
       if (present(count)) then
          do jeff = 0, v%neff - 1
             jco = v%ceff(jeff)
             ofs(jco) = start(jeff)
             mem(jco) = count(jeff)
          enddo
       else
          do jeff = 0, v%neff - 1
             jco = v%ceff(jeff)
             ofs(jco) = start(jeff)
             mem(jco) = 1
          enddo
       endif
    else if (present(count)) then
       do jeff = 0, v%neff - 1
          jco = v%ceff(jeff)
          ofs(jco) = 0
          mem(jco) = count(jeff)
       enddo
    endif
  end subroutine cache_var_slice

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

!!!_ + cache/sparse access
!!!_  & cache_post_review
  subroutine cache_post_review &
       & (ierr,   &
       &  nbase,  mcols, npack, &
       &  refh,   refp,  refc,  &
       &  head,   popts, colc)
    use TOUZA_Nio_sparse,only: lopts_sparse, nio_inquire_sparse
    use TOUZA_Nio_record,only: PROP_PTX_COLC
    implicit none
    integer,         intent(out)          :: ierr
    integer,         intent(out)          :: nbase,  mcols, npack
    character(len=*),intent(in)           :: refh(*)
    integer,         intent(in)           :: refp(*)
    integer,         intent(in)           :: refc
    character(len=*),intent(out),optional :: head(*)
    integer,         intent(out),optional :: popts(*)
    integer,         intent(out),optional :: colc
    ierr = 0
    if (ierr.eq.0) then
       if (present(colc)) then
          ! colc = refc
          colc = refp(PROP_PTX_COLC)  ! count from 1, oh.....
       endif
       if (present(popts)) then
          popts(1:lopts_sparse) = refp(1:lopts_sparse)
       endif
       if (present(head)) then
          head(1:nitem) = refh(1:nitem)
       endif
       call nio_inquire_sparse(refp, nbase, mcols, npack)
    endif
    return
  end subroutine cache_post_review
!!!_  & cache_sparse_review
  subroutine cache_sparse_review_d &
       & (ierr,   &
       &  nbase,  mcols, npack, &
       &  handle, vid,   rec, mold, &
       &  head,   popts, colc,  &
       &  cname)
    use TOUZA_Nio_sparse,only: lopts_sparse, nio_column_coor, nio_review_sparse
    use TOUZA_Nio_record,only: rev_pos_leave
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out)          :: ierr
    integer,         intent(out)          :: nbase,  mcols, npack
    integer,         intent(in)           :: handle, vid,   rec
    real(kind=KARG), intent(in)           :: mold
    character(len=*),intent(out),optional :: head(*)
    integer,         intent(out),optional :: popts(*)
    integer,         intent(out),optional :: colc
    character(len=*),intent(in), optional :: cname

    character(len=litem) :: xhd(nitem)
    integer              :: xpo(lopts_sparse)
    integer              :: xcc
    integer              :: krect
    integer ufile
    integer flag

    ierr = 0
    flag = rev_pos_leave

    if (ierr.eq.0) then
       call cache_read_header &
            & (ierr, xhd, handle, vid, rec, krect)
    endif
    if (ierr.eq.0) xcc = nio_column_coor(xhd, cname)
    if (ierr.eq.0) then
       ufile = extr_h2unit(handle)
       ierr = min(0, ufile)
    endif
    if (ierr.eq.0) then
       call nio_review_sparse &
            & (ierr, xpo, xhd, ufile, krect, mold, xcc, flag)
    endif
    if (ierr.eq.0) then
       call cache_post_review &
            & (ierr,   &
            &  nbase,  mcols, npack, &
            &  xhd,    xpo,   xcc,   &
            &  head,   popts, colc)
    endif
  end subroutine cache_sparse_review_d
  subroutine cache_sparse_review_f &
       & (ierr,   &
       &  nbase,  mcols, npack, &
       &  handle, vid,   rec, mold, &
       &  head,   popts, colc,  &
       &  cname)
    use TOUZA_Nio_sparse,only: lopts_sparse, nio_column_coor, nio_review_sparse
    use TOUZA_Nio_record,only: rev_pos_leave
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out)          :: ierr
    integer,         intent(out)          :: nbase,  mcols, npack
    integer,         intent(in)           :: handle, vid,   rec
    real(kind=KARG), intent(in)           :: mold
    character(len=*),intent(out),optional :: head(*)
    integer,         intent(out),optional :: popts(*)
    integer,         intent(out),optional :: colc
    character(len=*),intent(in), optional :: cname

    character(len=litem) :: xhd(nitem)
    integer              :: xpo(lopts_sparse)
    integer              :: xcc
    integer              :: krect
    integer ufile
    integer flag

    ierr = 0
    flag = rev_pos_leave

    if (ierr.eq.0) then
       call cache_read_header &
            & (ierr, xhd, handle, vid, rec, krect)
    endif
    if (ierr.eq.0) xcc = nio_column_coor(xhd, cname)
    if (ierr.eq.0) then
       ufile = extr_h2unit(handle)
       ierr = min(0, ufile)
    endif
    if (ierr.eq.0) then
       call nio_review_sparse &
            & (ierr, xpo, xhd, ufile, krect, mold, xcc, flag)
    endif
    if (ierr.eq.0) then
       call cache_post_review &
            & (ierr,   &
            &  nbase,  mcols, npack, &
            &  xhd,    xpo,   xcc,   &
            &  head,   popts, colc)
    endif
  end subroutine cache_sparse_review_f
  subroutine cache_sparse_review_i &
       & (ierr,   &
       &  nbase,  mcols, npack, &
       &  handle, vid,   rec, mold, &
       &  head,   popts, colc,  &
       &  cname)
    use TOUZA_Nio_sparse,only: lopts_sparse, nio_column_coor, nio_review_sparse
    use TOUZA_Nio_record,only: rev_pos_leave
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out)          :: ierr
    integer,         intent(out)          :: nbase,  mcols, npack
    integer,         intent(in)           :: handle, vid,   rec
    integer,         intent(in)           :: mold
    character(len=*),intent(out),optional :: head(*)
    integer,         intent(out),optional :: popts(*)
    integer,         intent(out),optional :: colc
    character(len=*),intent(in), optional :: cname

    character(len=litem) :: xhd(nitem)
    integer              :: xpo(lopts_sparse)
    integer              :: xcc
    integer              :: krect
    integer ufile
    integer flag

    ierr = 0
    flag = rev_pos_leave

    if (ierr.eq.0) then
       call cache_read_header &
            & (ierr, xhd, handle, vid, rec, krect)
    endif
    if (ierr.eq.0) xcc = nio_column_coor(xhd, cname)
    if (ierr.eq.0) then
       ufile = extr_h2unit(handle)
       ierr = min(0, ufile)
    endif
    if (ierr.eq.0) then
       call nio_review_sparse &
            & (ierr, xpo, xhd, ufile, krect, mold, xcc, flag)
    endif
    if (ierr.eq.0) then
       call cache_post_review &
            & (ierr,   &
            &  nbase,  mcols, npack, &
            &  xhd,    xpo,   xcc,   &
            &  head,   popts, colc)
    endif
  end subroutine cache_sparse_review_i
!!!_  & cache_restore_csr
  subroutine cache_restore_csr_d &
       & (ierr,   dcsr, hcsr, &
       &  handle, vid,  rec,  colc)
    use TOUZA_Nio_sparse,only: nio_restore_csr
    implicit none
    integer,parameter :: KARG=KDBL
    integer,        intent(out) :: ierr
    real(kind=KARG),intent(out) :: dcsr(0:*)
    integer,        intent(out) :: hcsr(0:*)
    integer,        intent(in)  :: handle, vid,   rec
    integer,        intent(in)  :: colc

    character(len=litem) :: head(nitem)
    integer ufile, krect
    integer jc,    rser

    ierr = 0
    jc = is_valid(handle, vid)
    ierr = min(0, jc)
    if (ierr.eq.0) then
       ufile = extr_h2unit(handle)
       rser = get_rserial(handle, vid, rec)
       call cue_read_header(ierr, head, ctables(jc), ufile, rser)
    endif
    if(ierr.eq.0) then
       krect = ctables(jc)%krect
       call nio_restore_csr &
            & (ierr, dcsr,   hcsr,  &
            &  head, ufile,  krect, colc)
    endif
  end subroutine cache_restore_csr_d
  subroutine cache_restore_csr_f &
       & (ierr,   dcsr, hcsr, &
       &  handle, vid,  rec,  colc)
    use TOUZA_Nio_sparse,only: nio_restore_csr
    implicit none
    integer,parameter :: KARG=KFLT
    integer,        intent(out) :: ierr
    real(kind=KARG),intent(out) :: dcsr(0:*)
    integer,        intent(out) :: hcsr(0:*)
    integer,        intent(in)  :: handle, vid,   rec
    integer,        intent(in)  :: colc

    character(len=litem) :: head(nitem)
    integer ufile, krect
    integer jc,    rser

    ierr = 0
    jc = is_valid(handle, vid)
    ierr = min(0, jc)
    if (ierr.eq.0) then
       ufile = extr_h2unit(handle)
       rser = get_rserial(handle, vid, rec)
       call cue_read_header(ierr, head, ctables(jc), ufile, rser)
    endif
    if(ierr.eq.0) then
       krect = ctables(jc)%krect
       call nio_restore_csr &
            & (ierr, dcsr,   hcsr,  &
            &  head, ufile,  krect, colc)
    endif
  end subroutine cache_restore_csr_f
  subroutine cache_restore_csr_i &
       & (ierr,   dcsr, hcsr, &
       &  handle, vid,  rec,  colc)
    use TOUZA_Nio_sparse,only: nio_restore_csr
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: dcsr(0:*)
    integer,intent(out) :: hcsr(0:*)
    integer,intent(in)  :: handle, vid,   rec
    integer,intent(in)  :: colc

    character(len=litem) :: head(nitem)
    integer ufile, krect
    integer jc,    rser

    ierr = 0
    jc = is_valid(handle, vid)
    ierr = min(0, jc)
    if (ierr.eq.0) then
       ufile = extr_h2unit(handle)
       rser = get_rserial(handle, vid, rec)
       call cue_read_header(ierr, head, ctables(jc), ufile, rser)
    endif
    if(ierr.eq.0) then
       krect = ctables(jc)%krect
       call nio_restore_csr &
            & (ierr, dcsr,   hcsr,  &
            &  head, ufile,  krect, colc)
    endif
  end subroutine cache_restore_csr_i

!!!_ + i/o core
!!!_  & cue_read_header
  subroutine cue_read_header &
       & (ierr, head, c, ufile, rser)
    use TOUZA_Nio_std,only: KIOFS, WHENCE_BEGIN, sus_getpos
    use TOUZA_Nio_record,only: nio_read_header
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(out)   :: head(*)
    type(cache_t),   intent(inout) :: c
    integer,         intent(in)    :: ufile
    integer,         intent(in)    :: rser
    integer krect
    integer(kind=KIOFS) :: rpos

    ierr = min(0, rser)
    if (ierr.eq.0) then
       if (rser.ge.c%lrec) ierr = _ERROR(ERR_OUT_OF_RANGE)
    endif
    if (ierr.eq.0) then
       rpos = c%rpos(rser)
       call nio_read_header(ierr, head, krect, ufile, rpos, WHENCE_BEGIN)
    endif
    if (ierr.eq.0) then
       c%krect = krect
       call sus_getpos(ierr, c%dpos(rser), ufile, WHENCE_BEGIN)
    endif
  end subroutine cue_read_header

!!!_ + various handles and serial index
!!!_  & register_cache()
  integer function register_cache(u, jcache) result(e)
    use TOUZA_Nio_std,only: reg_entry
    implicit none
    integer,intent(in) :: u
    integer,intent(in) :: jcache
    e = reg_entry(hh_cache, ikey=(/u/), status=(/jcache/))
    e = min(0, e)
  end function register_cache

!!!_  - is_valid() - check handle and group/variable range
  integer function is_valid (handle, vid) result(jc)
    implicit none
    integer,intent(in)          :: handle
    integer,intent(in),optional :: vid
    integer jg
    jc = extr_h2index(handle)
    if (jc.lt.0) return
    jg = extr_h2group(handle)
    if (jg.eq.grp_suite) then
       if (.not.present(vid)) return
       if (vid.lt.0.or.vid.ge.ctables(jc)%o(ctables(jc)%ngrp)) jc = _ERROR(ERR_INVALID_ITEM)
    else
       if (jg.lt.0.or.jg.ge.ctables(jc)%ngrp) jc = _ERROR(ERR_INVALID_ITEM)
       if (.not.present(vid)) return
       if (vid.lt.0.or.vid.ge.ctables(jc)%g(jg)%nvar) jc = _ERROR(ERR_INVALID_ITEM)
    endif
  end function is_valid

!!!_  & extr_h2index()
  integer function extr_h2index(handle) result(jcache)
    use TOUZA_Nio_std,only: query_status
    implicit none
    integer,intent(in) :: handle
    integer u
    integer jerr
    u = extr_h2unit(handle)
    call query_status(jerr, jcache, hh_cache, ikey=(/u/))
    if (jerr.lt.0) jcache = jerr
  end function extr_h2index

!!!_  & conv_u2handle()
  integer function conv_u2handle(unit, gid) result(h)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,intent(in)          :: unit
    integer,intent(in),optional :: gid
    integer g
    if (unit.lt.0) then
       h = _ERROR(ERR_INVALID_ITEM)
    else if (unit.ge.mdl_file) then
       h = _ERROR(ERR_OUT_OF_RANGE)
    else
       g = choice(grp_suite, gid)
       g = conv_g2image(g)
       if (g.lt.0) then
          h = _ERROR(ERR_OUT_OF_RANGE)
       else
          h = IOR(unit, ISHFT(g, pat_file))
       endif
    endif
  end function conv_u2handle

!!!_  & conv_h2handle()
  integer function conv_h2handle(handle, gid) result(h)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,intent(in)          :: handle
    integer,intent(in),optional :: gid
    integer g, u
    if (handle.lt.0) then
       h = _ERROR(ERR_INVALID_ITEM)
    else
       g = choice(grp_suite, gid)
       g = conv_g2image(g)
       if (g.lt.0) then
          h = _ERROR(ERR_OUT_OF_RANGE)
       else
          u = extr_h2unit(handle)
          if (u.lt.0) then
             h = _ERROR(ERR_INVALID_ITEM)
          else
             h = IOR(u, ISHFT(g, pat_file))
          endif
       endif
    endif
  end function conv_h2handle

!!!_  - extr_h2group() - without group range check
  integer function extr_h2group(handle) result(g)
    implicit none
    integer,intent(in) :: handle
    if (handle.lt.0) then
       g = _ERROR(ERR_INVALID_ITEM)
    else
       g = ISHFT(handle, - pat_file)
       g = conv_image2g(g)
    endif
  end function extr_h2group

!!!_  & conv_g2image()
  integer function conv_g2image(gid) result (p)
    implicit none
    integer,intent(in) :: gid
    if (gid.lt.0.or.gid.ge.lim_groups) then
       !! valid range is 0:lim
       p = _ERROR(ERR_OUT_OF_RANGE)
    else
       ! increment by 1 to store
       p = IAND(gid + 1, msk_group)
    endif
  end function conv_g2image

!!!_  & conv_image2g()
  integer function conv_image2g(pattern) result (gid)
    implicit none
    integer,intent(in) :: pattern
    if (pattern.lt.0.or.pattern.ge.lim_groups) then
       !! valid range is 0:lim
       gid = _ERROR(ERR_OUT_OF_RANGE)
    else
       ! decrement by 1 to store
       gid = IAND(pattern + msk_group, msk_group)
    endif
  end function conv_image2g

!!!_  - extr_h2unit()
  integer function extr_h2unit(handle) result(u)
    implicit none
    integer,intent(in) :: handle
    if (handle.lt.0) then
       u = _ERROR(ERR_INVALID_ITEM)
    else
       u = IAND(handle, msk_file)
    endif
  end function extr_h2unit

!!!_  & query_coor()
  integer function query_coor(c, coor) result(xh)
    implicit none
    type(cache_t),   intent(in)  :: c
    character(len=*),intent(in)  :: coor
    integer jx
    xh = _ERROR(ERR_NOT_FOUND)
    do jx = 0, c%ncoor - 1
       if (c%x(jx).eq.coor) then
          xh = jx
          exit
       endif
    enddo
  end function query_coor

!!!_  - query_gserial - name to group query
  integer function query_gserial(grp, name, jbgn, jend) result(gser)
    use TOUZA_Nio_std,only: choice
    implicit none
    type(group_t),   intent(in) :: grp(0:*)
    character(len=*),intent(in) :: name
    integer,         intent(in) :: jbgn, jend
    integer jg

    gser = _ERROR(ERR_INVALID_PARAMETER)
    do jg = jbgn, jend - 1
       if (grp(jg)%name.eq.name) then
          gser = jg
          exit
       endif
    enddo
  end function query_gserial
!!!_  - query_vserial - name to variable query
  integer function query_vserial(var, name, jbgn, jend) result(vser)
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
             vser = jv
             return
          endif
       enddo
    else
       ! loose matching
       do jv = jbgn, jend - 1
          if (var(jv)%item(1:lname).eq.name(1:lname)) then
             if (var(jv)%item(lname+1:).eq.' ' &
                  & .or. var(jv)%item(lname+1:lname+lsep).eq.dup_sep(1:lsep)) then
                vser = jv
                return
             endif
          endif
       enddo
    endif
    vser = _ERROR(ERR_INVALID_PARAMETER)
  end function query_vserial

!!!_  - query_rserial_time - return record id filtered by time range
  integer function query_rserial_time_d &
       & (handle, vid, timel, timeh, func, init) result(rser)
    use TOUZA_Nio_std,only: KTGT=>KDBL
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,         intent(in) :: handle
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

    integer jc, gser, vser, jr
    integer jerr
    integer rbgn, rend, rofs

    rser = -1
    jc = is_valid(handle)
    jerr = min(0, jc)
    if (jerr.eq.0) then
       gser = get_gserial(handle, vid)
       vser = get_vserial(handle, vid)
       jerr = min(0, gser, vser)
    endif
    if (jerr.eq.0) then
       rofs = get_rserial(handle, vid, 0)
       rbgn = choice(rofs, init) - rofs
       rend = ctables(jc)%g(gser)%nrec
       do jr = rbgn, rend
          if (func(ctables(jc)%g(gser)%d(jr), ctables(jc)%g(gser)%t(jr), timel, timeh)) then
             rser = jr
             exit
          endif
       enddo
    endif
    if (jerr.eq.0) then
       if (rser.ge.0) rser = rser + rofs
    endif
  end function query_rserial_time_d

!!!_  - get_gserial() - return /suite/ group index (serial)
  integer function get_gserial(handle, vid) result(gser)
    implicit none
    integer,intent(in)          :: handle
    integer,intent(in),optional :: vid
    integer jc, j
    integer jerr
    jc = is_valid(handle, vid)
    jerr = min(0, jc)
    if (jerr.eq.0) then
       gser = extr_h2group(handle)
       if (gser.eq.grp_suite) then
          if (.not.present(vid)) then
             gser = _ERROR(ERR_FEW_ARGUMENTS)
             return
          endif
          do j = 0, ctables(jc)%ngrp - 1
             if (ctables(jc)%o(j+1).gt.vid) then
                gser = j
                return
             endif
          enddo
          gser = _ERROR(ERR_PANIC)
       endif
    else
       gser = jerr
    endif
  end function get_gserial

!!!_  - get_vserial() - return /suite/ variable index (serial)
  integer function get_vserial(handle, vid) result(vser)
    implicit none
    integer,intent(in) :: handle
    integer,intent(in) :: vid
    integer gid
    integer jc
    integer jerr
    jc = is_valid(handle, vid)
    jerr = min(0, jc)
    if (jerr.eq.0) then
       gid = extr_h2group(handle)
       if (gid.eq.grp_suite) then
          vser = vid
       else
          vser = ctables(jc)%o(gid) + vid
       endif
    else
       vser = jerr
    endif
  end function get_vserial

!!!_  - get_rserial() - return /suite/ record index (serial)
  integer function get_rserial(handle, vid, rec) result(rser)
    implicit none
    integer,intent(in) :: handle
    integer,intent(in) :: vid
    integer,intent(in) :: rec
    integer jerr
    integer jc, gser, vser
    jc = is_valid(handle, vid)
    jerr = min(0, jc)
    if (jerr.eq.0) then
       if (vid.eq.var_suite) then
          if (rec.ge.0.and.rec.lt.ctables(jc)%lrec) then
             rser = rec
          else
             jerr = -1
          endif
       else
          gser = get_gserial(handle, vid)
          vser = get_vserial(handle, vid)
          jerr = min(0, gser, vser)
          ! write(*, *) 'get_rserial', gser, vser
          if (jerr.eq.0) then
             if (rec.ge.0.and.rec.lt.ctables(jc)%g(gser)%nrec) then
                rser = rec + ctables(jc)%v(vser)%rofs
             else
                jerr = -1
             endif
          endif
       endif
    endif
    if (jerr.ne.0) then
       rser = _ERROR(ERR_INVALID_PARAMETER)
    endif
  end function get_rserial

!!!_ + end module
end module TOUZA_Nio_cache

!!!_@ test_nio_cache - test program
#ifdef TEST_NIO_CACHE
program test_nio_cache
  use TOUZA_Std,only: KTGT=>KDBL
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
  real(kind=KTGT),allocatable :: d(:)

  integer uh
  integer flag
  integer ngrp
  integer jvar, nvar
  integer jco,  nco
  integer,parameter :: lco = 6
  integer clen(0:lco-1)
  integer xstart(0:lco-1), xcount(0:lco-1)
  integer jz, nh

  ierr = 0
  jarg = 0
  flag = 0

101 format(A,' = ', I0)
  call init(ierr, levv=+9, stdv=+9)
  ! call init(ierr, stdv=+9)
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
        if (ierr.eq.0) then
           ngrp = cache_group_size(uh)
111        format('groups: ', I0)
           write(*, 111) ngrp
        endif
        if (ierr.eq.0) then
           nvar = cache_var_size(uh)
112        format('variabls: ', I0)
           write(*, 112) nvar
           do jvar = 0, nvar - 1
              nco = cache_co_size(uh, jvar)
              do jco = 0, nco - 1
                 clen(jco) = cache_co_len(uh, jvar, jco)
              enddo
              nh = product(clen(0:nco-2))
              allocate(d(0:nh-1), STAT=ierr)
113           format('coordinates: ', I0, 1x, I0, 2x, 4(1x, I0))
114           format('read-level: ', I0, 1x, I0)
              write(*, 113) jvar, nco, clen(0:nco-1)
              xstart(0:nco-2) = 0
              xcount(0:nco-2) = clen(0:nco-2)
              xcount(0:nco-1) = 1
              do jz = 0, clen(nco-1) - 1
                 xstart(0:nco-1) = jz
                 call cache_var_read(ierr, d, uh, jvar, 0, xstart, xcount)
                 write(*, 114) jz, ierr
                 ! write(*, *) jz, d
              enddo
              deallocate(d, STAT=ierr)
           enddo
        endif
        if (ierr.eq.0) call cache_close(ierr, uh, file)
     enddo
  endif
  if (ierr.eq.0) call diag(ierr, levv=+9, mode=MODE_DEEP)
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

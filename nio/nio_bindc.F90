!!!_! nio_bindc.F90 - TOUZA/Nio bind(c) interfaces
! Maintainer: SAITO Fuyuki
! Created: Feb 16 2023
#define TIME_STAMP 'Time-stamp: <2024/07/23 18:09:40 fuyuki nio_bindc.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2023, 2024
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
!!!_@ TOUZA_Nio_bindc - nio record interfaces
module TOUZA_Nio_bindc
!!!_ = declaration
  use ISO_C_BINDING
  use TOUZA_Nio_std,only: KI32, KI64, KDBL, KFLT
  use TOUZA_Nio_std,only: get_logu,      unit_global,  trace_fine,    trace_control
  use TOUZA_Nio_std,only: ignore_bigger, ignore_small, ignore_always, def_block
  use TOUZA_Nio_header,only: litem
  implicit none
  private
!!!_  - public parameters
  integer,parameter :: lpath = OPT_PATH_LEN
  integer,parameter :: lvar  = 64
  ! integer(kind=C_INT),parameter,public :: len_path = OPT_PATH_LEN
  ! integer(kind=C_INT),parameter,public :: len_var  = 64
  ! integer(kind=C_INT),parameter,public :: len_item = litem
!!!_  - private parameter
  integer,parameter :: lax = 6   !  3 is enough
!!!_  - static
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = NIO_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
#define __MDL__ 'b'
#define _ERROR(E) (E - ERR_MASK_NIO_BINDC)
!!!_  - interfaces
!!!_  - public procedures
  public :: tnb_init,          tnb_diag,       tnb_finalize
  public :: tnb_file_is_nio,   tnb_file_open,  tnb_file_diag,  tnb_file_close
  public :: tnb_file_groups,   tnb_group,      tnb_search_group
  public :: tnb_group_name,    tnb_group_recs, tnb_group_coors
  public :: tnb_group_co_name, tnb_group_co_range, tnb_group_co_idx
  public :: tnb_group_vars,    tnb_search_var, tnb_var_name,   tnb_co_size
  public :: tnb_var_recs
  public :: tnb_co_name,       tnb_co_len,     tnb_co_idx,     tnb_co_serial
  public :: tnb_rec_date,      tnb_rec_time
  public :: tnb_attr_size,     tnb_attr_len
  public :: tnb_get_attr
  public :: tnb_get_attr_byid, tnb_get_attr_name
  public :: tnb_get_attr_int,  tnb_get_attr_float, tnb_get_attr_double
  public :: tnb_var_read_int,  tnb_var_read_float, tnb_var_read_double
  public :: init,              diag,           finalize
!!!_  - public shared
contains
!!!_ + c interfaces for internal
!!!_  & tnb_init()
  integer(kind=C_INT) function tnb_init &
       & (levv, mode) BIND(C) result(ierr)
    implicit none
    integer(kind=C_INT),intent(in),value :: levv, mode
    integer jerr
    integer stdv
    stdv = -1
    if (levv.gt.9) stdv = int(levv)
    call init(jerr, levv=int(levv), mode=int(mode), stdv=stdv)
    ierr = jerr
  end function tnb_init
!!!_  & tnb_diag()
  integer(kind=C_INT) function tnb_diag &
       & (levv, mode) BIND(C) result(ierr)
    implicit none
    integer(kind=C_INT),intent(in),value :: levv, mode
    integer jerr
    call diag(jerr, levv=int(levv), mode=int(mode))
    ierr = jerr
  end function tnb_diag
!!!_  & tnb_finalize()
  integer(kind=C_INT) function tnb_finalize &
       & (levv, mode) BIND(C) result(ierr)
    implicit none
    integer(kind=C_INT),intent(in),value :: levv, mode
    integer jerr
    call finalize(jerr, levv=int(levv), mode=int(mode))
    ierr = jerr
  end function tnb_finalize
!!!_ + c interfaces for modules
!!!_  & tnb_file_is_nio() - wrapper for nio_check_magic_file
  integer(kind=C_INT) function tnb_file_is_nio &
       & (path) BIND(C) result(krect)
    ! return non-negative if gtool-format.
    use TOUZA_Nio_record,only: nio_check_magic_file
    use TOUZA_Nio_std,only: new_unit, search_from_last, sus_close, sus_open
    use TOUZA_Nio_std,only: is_eof_ss
    implicit none
    character(len=1,kind=C_CHAR),intent(in) :: path(*)
    character(len=lpath) :: buf
    integer jerr
    integer u
    krect = 0
    u = new_unit(search_from_last)
    if (u.lt.0) then
       jerr = u
    else
       call c2f_string(buf, path)
       call sus_open(jerr, u, buf, ACTION='R', STATUS='O')
       if (jerr.eq.0) then
          jerr = nio_check_magic_file(u)
          krect = jerr
          call sus_close(jerr, u, buf)
       else if (is_eof_ss(jerr)) then
          jerr = _ERROR(ERR_EOF)
       else
          jerr = _ERROR(ERR_INVALID_PARAMETER)
       endif
    endif
    if (jerr.lt.0) krect = jerr
  end function tnb_file_is_nio
!!!_  & tnb_file_open()
  integer(kind=C_INT) function tnb_file_open &
       & (path, flag) BIND(C) result(handle)
    use TOUZA_Nio_cache,only: cache_open_read
    implicit none
    character(len=1,kind=C_CHAR),intent(in)       :: path(*)
    integer(kind=C_INT),         intent(in),value :: flag
    character(len=lpath) :: buf
    integer h
    integer jerr
    call c2f_string(buf, path)
    call cache_open_read(jerr, h, buf, int(flag))
    handle = h
    if (jerr.lt.0) handle = jerr
  end function tnb_file_open

!!!_  & tnb_file_diag()
  integer(kind=C_INT) function tnb_file_diag &
       & (handle, lev) BIND(C) result(ierr)
    use TOUZA_Nio_cache,only: show_cache
    implicit none
    integer(kind=C_INT),intent(in),value :: handle
    integer(kind=C_INT),intent(in),value :: lev
    integer jerr
    call show_cache(jerr, int(handle), levv=int(lev))
    ierr = jerr
  end function tnb_file_diag

!!!_  & tnb_file_close()
  integer(kind=C_INT) function tnb_file_close &
       & (handle) BIND(C) result(ierr)
    use TOUZA_Nio_cache,only: cache_close
    implicit none
    integer(kind=C_INT),intent(in),value :: handle
    integer jerr
    integer h

    ierr = 0
    h = handle
    call cache_close(jerr, h)
    if (jerr.lt.0) ierr = jerr
  end function tnb_file_close

!!!_  & tnb_file_groups()
  integer(kind=C_INT) function tnb_file_groups &
       & (handle) BIND(C) result(n)
    use TOUZA_Nio_cache,only: cache_group_size
    implicit none
    integer(kind=C_INT),intent(in),value :: handle
    n = cache_group_size(int(handle))
  end function tnb_file_groups

!!!_  - tnb_group()
  integer(kind=C_INT) function tnb_group &
       & (handle, gidx) BIND(C) result(gh)
    use TOUZA_Nio_cache,only: cache_group
    implicit none
    integer(kind=C_INT),intent(in),value :: handle
    integer(kind=C_INT),intent(in),value :: gidx
    gh = cache_group(int(gidx), int(handle))
  end function tnb_group

!!!_  - tnb_search_group()
  integer(kind=C_INT) function tnb_search_group &
       & (handle, name, refh) BIND(C) result(gh)
    use TOUZA_Nio_cache,only: cache_group
    implicit none
    integer(kind=C_INT),         intent(in),value :: handle
    character(len=1,kind=C_CHAR),intent(in)       :: name(*)
    integer(kind=C_INT),         intent(in),value :: refh
    character(len=lvar) :: buf
    call c2f_string(buf, name)
    gh = cache_group(buf, int(handle), int(refh))
  end function tnb_search_group

!!!_  - tnb_group_name()
  integer(kind=C_INT) function tnb_group_name &
       & (name, handle) BIND(C) result(ierr)
    use TOUZA_Nio_cache,only: cache_group_name
    implicit none
    character(len=1,kind=C_CHAR),intent(out)      :: name(*)
    integer(kind=C_INT),         intent(in),value :: handle
    character(len=lvar) :: buf
    call cache_group_name(ierr, buf, int(handle))
    if (ierr.eq.0) then
       call f2c_string(name, buf)
    else
       call f2c_string(name)
    endif
  end function tnb_group_name

!!!_  & tnb_group_coors()
  integer(kind=C_INT) function tnb_group_coors &
       & (handle) BIND(C) result(n)
    use TOUZA_Nio_cache,only: cache_group_coors
    implicit none
    integer(kind=C_INT),intent(in),value :: handle
    n = cache_group_coors(int(handle))
  end function tnb_group_coors

!!!_  - tnb_group_co_idx()
  integer(kind=C_INT) function tnb_group_co_idx &
       & (handle, cid) BIND(C) result(serial)
    use TOUZA_Nio_cache,only: cache_group_cserial
    use TOUZA_Nio_header,only: litem
    implicit none
    integer(kind=C_INT),intent(in),value :: handle
    integer(kind=C_INT),intent(in),value :: cid
    integer j
    j = cache_group_cserial(int(handle), int(cid))
    serial = j
  end function tnb_group_co_idx

!!!_  & tnb_group_co_name()
  integer(kind=C_INT) function tnb_group_co_name &
       & (name, handle, cid) BIND(C) result(ierr)
    use TOUZA_Nio_cache,only: cache_group_coor_name
    use TOUZA_Nio_header,only: litem
    implicit none
    character(len=1,kind=C_CHAR),intent(out)      :: name(*)
    integer(kind=C_INT),         intent(in),value :: handle
    integer(kind=C_INT),         intent(in),value :: cid
    integer jerr
    character(len=litem) :: buf

    call cache_group_coor_name(jerr, buf, int(handle), int(cid))
    if (jerr.eq.0) then
       call f2c_string(name, buf)
    else
       call f2c_string(name)
    endif
    ierr = jerr
  end function tnb_group_co_name

!!!_  & tnb_group_co_range()
  integer(kind=C_INT) function tnb_group_co_range &
       & (jbgn, jend, handle, cid) BIND(C) result(ierr)
    use TOUZA_Nio_cache,only: cache_group_coor_range
    use TOUZA_Nio_header,only: litem
    implicit none
    integer(kind=C_INT),intent(out)      :: jbgn
    integer(kind=C_INT),intent(out)      :: jend
    integer(kind=C_INT),intent(in),value :: handle
    integer(kind=C_INT),intent(in),value :: cid
    integer jerr
    integer jb, je

    call cache_group_coor_range(jerr, jb, je, int(handle), int(cid))
    jbgn = jb
    jend = je
    ierr = jerr
  end function tnb_group_co_range

!!!_  & tnb_group_vars()
  integer(kind=C_INT) function tnb_group_vars &
       & (handle) BIND(C) result(n)
    use TOUZA_Nio_cache,only: cache_var_size
    implicit none
    integer(kind=C_INT),intent(in),value :: handle
    n = cache_var_size(int(handle))
  end function tnb_group_vars

!!!_  - tnb_group_recs()
  integer(kind=C_INT) function tnb_group_recs &
       & (handle) BIND(C) result(nrecs)
    use TOUZA_Nio_cache,only: cache_group_recs
    implicit none
    integer(kind=C_INT),intent(in),value :: handle
    integer n
    n = cache_group_recs(int(handle))
    nrecs = n
  end function tnb_group_recs

!!!_  - tnb_var_name()
  integer(kind=C_INT) function tnb_var_name &
       & (name, handle, vid) BIND(C) result(ierr)
    use TOUZA_Nio_cache,only: cache_var_name
    implicit none
    character(len=1,kind=C_CHAR),intent(out)      :: name(*)
    integer(kind=C_INT),         intent(in),value :: handle
    integer(kind=C_INT),         intent(in),value :: vid
    character(len=lvar) :: buf
    call cache_var_name(ierr, buf, int(handle), int(vid))
    if (ierr.eq.0) then
       call f2c_string(name, buf)
    else
       call f2c_string(name)
    endif
  end function tnb_var_name

!!!_  - tnb_search_var()
  integer(kind=C_INT) function tnb_search_var &
       & (handle, name) BIND(C) result(vid)
    use TOUZA_Nio_cache,only: cache_var_id
    implicit none
    integer(kind=C_INT),         intent(in),value :: handle
    character(len=1,kind=C_CHAR),intent(in)       :: name(*)
    character(len=lvar) :: buf
    integer jv
    call c2f_string(buf, name)
    jv = cache_var_id(buf, int(handle))
    vid = jv
  end function tnb_search_var

!!!_  - tnb_co_size()
  integer(kind=C_INT) function tnb_co_size &
       & (handle, vid) BIND(C) result(nco)
    use TOUZA_Nio_cache,only: cache_co_size
    implicit none
    integer(kind=C_INT),intent(in),value :: handle
    integer(kind=C_INT),intent(in),value :: vid
    integer n
    n = cache_co_size(int(handle), int(vid))
    nco = n
  end function tnb_co_size

!!!_  - tnb_var_recs()
  integer(kind=C_INT) function tnb_var_recs &
       & (handle, vid) BIND(C) result(nrecs)
    use TOUZA_Nio_cache,only: cache_group_recs
    implicit none
    integer(kind=C_INT),intent(in),value :: handle
    integer(kind=C_INT),intent(in),value :: vid
    integer n
    n = cache_group_recs(int(handle), int(vid))
    nrecs = n
  end function tnb_var_recs

!!!_  - tnb_co_serial()
  integer(kind=C_INT) function tnb_co_serial &
       & (handle, vid, cid) BIND(C) result(ser)
    use TOUZA_Nio_cache,only: cache_co_serial, cache_co_size
    implicit none
    integer(kind=C_INT),intent(in),value :: handle
    integer(kind=C_INT),intent(in),value :: vid
    integer(kind=C_INT),intent(in),value :: cid
    integer nco, jco, xh
    nco = cache_co_size(int(handle), int(vid))
    ser = min(0, nco)
    if (ser.eq.0) then
       jco = nco - 1 - int(cid)
       xh = cache_co_serial(int(handle), int(vid), int(jco))
       ser = xh
    endif
  end function tnb_co_serial

!!!_  - tnb_co_name()
  integer(kind=C_INT) function tnb_co_name &
       & (name, handle, vid, cid) BIND(C) result(ierr)
    use TOUZA_Nio_cache,only: cache_co_name, cache_co_size
    use TOUZA_Nio_header,only: litem
    implicit none
    character(len=1,kind=C_CHAR),intent(out)      :: name(*)
    integer(kind=C_INT),         intent(in),value :: handle
    integer(kind=C_INT),         intent(in),value :: vid
    integer(kind=C_INT),         intent(in),value :: cid
    integer jerr
    character(len=litem) :: buf
    integer nco, jco
    nco = cache_co_size(int(handle), int(vid))
    jerr = min(0, nco)
    if (jerr.eq.0) then
       jco = nco - 1 - int(cid)
       call cache_co_name(jerr, buf, int(handle), int(vid), jco)
    endif
    if (jerr.eq.0) then
       call f2c_string(name, buf)
    else
       call f2c_string(name)
    endif
    ierr = jerr
  end function tnb_co_name

!!!_  - tnb_co_len()
  integer(kind=C_INT) function tnb_co_len &
       & (handle, vid, cid) BIND(C) result(nsize)
    use TOUZA_Nio_cache,only: cache_co_len, cache_co_size
    implicit none
    integer(kind=C_INT),intent(in),value :: handle
    integer(kind=C_INT),intent(in),value :: vid
    integer(kind=C_INT),intent(in),value :: cid
    integer ns
    integer nco, jco
    nco = cache_co_size(int(handle), int(vid))
    ns = min(0, nco)
    if (ns.eq.0) then
       jco = nco - 1 - int(cid)
       ns = cache_co_len(int(handle), int(vid), jco)
    endif
    nsize = ns
  end function tnb_co_len

!!!_  - tnb_co_idx()
  integer(kind=C_INT) function tnb_co_idx &
       & (handle, vid, name) BIND(C) result(cid)
    use TOUZA_Nio_cache,only: cache_co_idx, cache_co_size
    use TOUZA_Nio_header,only: litem
    implicit none
    integer(kind=C_INT),         intent(in),value :: handle
    integer(kind=C_INT),         intent(in),value :: vid
    character(len=1,kind=C_CHAR),intent(in)       :: name(*)
    integer ns
    integer nco, jco
    character(len=litem+16) :: buf

    call c2f_string(buf, name)
    cid = -1
    nco = cache_co_size(int(handle), int(vid))
    ns = min(0, nco)
    if (ns.eq.0) then
       jco = nco - 1 - int(cid)
       jco = cache_co_idx(int(handle), int(vid), buf)
       if (jco.ge.0) cid = nco - 1 - jco
    endif
  end function tnb_co_idx

! !!!_  - tnb_var_dims()
!   integer(kind=C_INT) function tnb_var_dims &
!        & (dims, handle, gid, vid) BIND(C) result(ndims)
!     use TOUZA_Nio_cache,only: cache_var_dims
!     implicit none
!     integer(kind=C_INT),intent(out)      :: dims(0:*)
!     integer(kind=C_INT),intent(in),value :: handle
!     integer(kind=C_INT),intent(in),value :: gid
!     integer(kind=C_INT),intent(in),value :: vid
!     integer jerr
!     integer n, d(0:3)  ! though 0:2 is enough
!     call cache_var_dims(jerr, n, d, int(handle), int(gid), int(vid))
!     if (jerr.ne.0) then
!        ndims = jerr
!     else
!        ndims = n
!        dims(n-1:0:-1) = d(0:n-1)
!     endif
!   end function tnb_var_dims

!!!_  - tnb_rec_time()
  integer(kind=C_INT) function tnb_rec_time &
       & (time, handle, vid, rec) BIND(C) result(ierr)
    use TOUZA_Nio_header,only: litem, nitem
    use TOUZA_Nio_cache,only: cache_rec_time
    implicit none
    character(len=1,kind=C_CHAR),intent(out)      :: time(*)
    integer(kind=C_INT),         intent(in),value :: handle
    integer(kind=C_INT),         intent(in),value :: vid
    integer(kind=C_INT),         intent(in),value :: rec
    character(len=litem) :: buf
    integer jerr
    ierr = 0
    if (vid.lt.0) then
       call cache_rec_time(jerr, buf, int(handle), rec=int(rec))
    else
       call cache_rec_time(jerr, buf, int(handle), int(vid), int(rec))
    endif
    if (jerr.eq.0) then
       call f2c_string(time, buf)
    else
       call f2c_string(time)
    endif
    ierr = jerr
  end function tnb_rec_time

!!!_  - tnb_rec_date()
  integer(kind=C_INT) function tnb_rec_date &
       & (time, handle, vid, rec) BIND(C) result(ierr)
    use TOUZA_Nio_header,only: litem, nitem
    use TOUZA_Nio_cache,only: cache_rec_date
    implicit none
    character(len=1,kind=C_CHAR),intent(out)      :: time(*)
    integer(kind=C_INT),         intent(in),value :: handle
    integer(kind=C_INT),         intent(in),value :: vid
    integer(kind=C_INT),         intent(in),value :: rec
    character(len=litem) :: buf
    integer jerr
    ierr = 0
    if (vid.lt.0) then
       call cache_rec_date(jerr, buf, int(handle), rec=int(rec))
    else
       call cache_rec_date(jerr, buf, int(handle), int(vid), int(rec))
    endif
    if (jerr.eq.0) then
       call f2c_string(time, buf)
    else
       call f2c_string(time)
    endif
    ierr = jerr
  end function tnb_rec_date

!!!_  & tnb_attr_size() - (hard-coded) number of attributes
  integer(kind=C_INT) function tnb_attr_size &
       & (handle, vid, rec) BIND(C) result(n)
    use TOUZA_Nio_header,only: nitem
    implicit none
    integer(kind=C_INT),intent(in),value :: handle
    integer(kind=C_INT),intent(in),value :: vid
    integer(kind=C_INT),intent(in),value :: rec
    n = nitem
  end function tnb_attr_size

!!!_  & tnb_attr_len() - (hard-coded) length of attribute string
  integer(kind=C_INT) function tnb_attr_len &
       & (item, handle, vid, rec) BIND(C) result(n)
    use TOUZA_Nio_header,only: litem
    implicit none
    character(len=1,kind=C_CHAR),intent(in)       :: item(*)
    integer(kind=C_INT),         intent(in),value :: handle
    integer(kind=C_INT),         intent(in),value :: vid
    integer(kind=C_INT),         intent(in),value :: rec
    n = litem
  end function tnb_attr_len

!!!_  - tnb_get_attr()
  integer(kind=C_INT) function tnb_get_attr &
       & (attr, item, handle, vid, rec) BIND(C) result(ierr)
    use TOUZA_Nio_header,only: litem, nitem
    use TOUZA_Nio_cache,only: cache_get_attr
    implicit none
    character(len=1,kind=C_CHAR),intent(out)      :: attr(*)
    character(len=1,kind=C_CHAR),intent(in)       :: item(*)
    integer(kind=C_INT),         intent(in),value :: handle
    integer(kind=C_INT),         intent(in),value :: vid
    integer(kind=C_INT),         intent(in),value :: rec
    character(len=litem*nitem) :: buf
    character(len=litem) :: ibuf
    integer jerr
    ierr = 0
    call c2f_string(ibuf, item)
    if (vid.lt.0) then
       call cache_get_attr(jerr, buf, ibuf, int(handle))
    else if (rec.lt.0) then
       call cache_get_attr(jerr, buf, ibuf, int(handle), int(vid))
    else
       call cache_get_attr(jerr, buf, ibuf, int(handle), int(vid), int(rec))
    endif
    if (jerr.eq.0) then
       call f2c_string(attr, buf)
    else
       call f2c_string(attr)
    endif
    ierr = jerr
  end function tnb_get_attr

!!!_  - tnb_get_attr_int()
  integer(kind=C_INT) function tnb_get_attr_int &
       & (attr, item, handle, vid, rec) BIND(C) result(ierr)
    use TOUZA_Nio_header,only: litem, nitem
    use TOUZA_Nio_cache,only: cache_get_attr
    implicit none
    integer(kind=C_INT),         intent(out)      :: attr
    character(len=1,kind=C_CHAR),intent(in)       :: item(*)
    integer(kind=C_INT),         intent(in),value :: handle
    integer(kind=C_INT),         intent(in),value :: vid
    integer(kind=C_INT),         intent(in),value :: rec
    character(len=litem) :: ibuf
    integer jerr
    ierr = 0
    call c2f_string(ibuf, item)
    if (vid.lt.0) then
       call cache_get_attr(jerr, attr, ibuf, int(handle))
    else if (rec.lt.0) then
       call cache_get_attr(jerr, attr, ibuf, int(handle), int(vid))
    else
       call cache_get_attr(jerr, attr, ibuf, int(handle), int(vid), int(rec))
    endif
    ierr = jerr
  end function tnb_get_attr_int
!!!_  - tnb_get_attr_float()
  integer(kind=C_INT) function tnb_get_attr_float &
       & (attr, item, handle, vid, rec) BIND(C) result(ierr)
    use TOUZA_Nio_header,only: litem, nitem
    use TOUZA_Nio_cache,only: cache_get_attr
    implicit none
    real(kind=C_FLOAT),          intent(out)      :: attr
    character(len=1,kind=C_CHAR),intent(in)       :: item(*)
    integer(kind=C_INT),         intent(in),value :: handle
    integer(kind=C_INT),         intent(in),value :: vid
    integer(kind=C_INT),         intent(in),value :: rec
    character(len=litem) :: ibuf
    integer jerr
    ierr = 0
    call c2f_string(ibuf, item)
    if (vid.lt.0) then
       call cache_get_attr(jerr, attr, ibuf, int(handle))
    else if (rec.lt.0) then
       call cache_get_attr(jerr, attr, ibuf, int(handle), int(vid))
    else
       call cache_get_attr(jerr, attr, ibuf, int(handle), int(vid), int(rec))
    endif
    ierr = jerr
  end function tnb_get_attr_float
!!!_  - tnb_get_attr_double()
  integer(kind=C_INT) function tnb_get_attr_double &
       & (attr, item, handle, vid, rec) BIND(C) result(ierr)
    use TOUZA_Nio_header,only: litem, nitem
    use TOUZA_Nio_cache,only: cache_get_attr
    implicit none
    real(kind=C_DOUBLE),         intent(out)      :: attr
    character(len=1,kind=C_CHAR),intent(in)       :: item(*)
    integer(kind=C_INT),         intent(in),value :: handle
    integer(kind=C_INT),         intent(in),value :: vid
    integer(kind=C_INT),         intent(in),value :: rec
    character(len=litem) :: ibuf
    integer jerr
    ierr = 0
    call c2f_string(ibuf, item)
    if (vid.lt.0) then
       call cache_get_attr(jerr, attr, ibuf, int(handle))
    else if (rec.lt.0) then
       call cache_get_attr(jerr, attr, ibuf, int(handle), int(vid))
    else
       call cache_get_attr(jerr, attr, ibuf, int(handle), int(vid), int(rec))
    endif
    ierr = jerr
  end function tnb_get_attr_double

!!!_  - tnb_get_attr_byid()
  integer(kind=C_INT) function tnb_get_attr_byid &
       & (attr, item, handle, vid, rec) BIND(C) result(ierr)
    use TOUZA_Nio_header,only: litem, nitem
    use TOUZA_Nio_cache,only: cache_get_attr
    implicit none
    character(len=1,kind=C_CHAR),intent(out)      :: attr(*)
    integer(kind=C_INT),         intent(in),value :: item
    integer(kind=C_INT),         intent(in),value :: handle
    integer(kind=C_INT),         intent(in),value :: vid
    integer(kind=C_INT),         intent(in),value :: rec
    character(len=litem*nitem) :: buf
    integer ibuf
    integer jerr
    ierr = 0
    ibuf = item
    if (vid.lt.0) then
       call cache_get_attr(jerr, buf, ibuf, int(handle))
    else if (rec.lt.0) then
       call cache_get_attr(jerr, buf, ibuf, int(handle), int(vid))
    else
       call cache_get_attr(jerr, buf, ibuf, int(handle), int(vid), int(rec))
    endif
    if (jerr.eq.0) then
       call f2c_string(attr, buf)
    else
       call f2c_string(attr)
    endif
    ierr = jerr
  end function tnb_get_attr_byid

!!!_  - tnb_get_attr_name()
  integer(kind=C_INT) function tnb_get_attr_name &
       & (name, item) BIND(C) result(ierr)
    use TOUZA_Nio_header,only: get_hitem
    implicit none
    character(len=1,kind=C_CHAR),intent(out)      :: name(*)
    integer(kind=C_INT),         intent(in),value :: item
    character(len=6) :: buf
    integer ibuf
    ierr = 0
    ibuf = item
    call get_hitem(buf, ibuf)
    call f2c_string(name, buf)
  end function tnb_get_attr_name

!!!_  - tnb_var_read_int()
  integer(kind=C_INT) function tnb_var_read_int &
       & (d, rec, start, count, handle, vid) BIND(C) result(ierr)
    use TOUZA_Nio_cache,only: cache_var_read, cache_co_size
    implicit none
    integer(kind=C_INT),   intent(out)      :: d(*)
    integer(kind=C_SIZE_T),intent(in),value :: rec
    integer(kind=C_SIZE_T),intent(in)       :: start(0:*), count(0:*)
    integer(kind=C_INT),   intent(in),value :: handle
    integer(kind=C_INT),   intent(in),value :: vid
    integer nco, jco, jeff
    integer jerr
    integer st(0:lax-1), co(0:lax-1)

    nco = cache_co_size(int(handle), int(vid))
    jerr = min(0, nco)
    do jeff = 0, nco - 1
       jco = nco - 1 - jeff
       st(jco) = int(start(jeff))
       co(jco) = int(count(jeff))
    enddo

    if (jerr.eq.0) then
       call cache_var_read &
            & (jerr,        d,           &
            &  int(handle), int(vid),    &
            &  int(rec),    st(0:nco-1), co(0:nco-1))
    endif
    ierr = jerr
  end function tnb_var_read_int
!!!_  - tnb_var_read_float()
  integer(kind=C_INT) function tnb_var_read_float &
       & (d, rec, start, count, handle, vid) BIND(C) result(ierr)
    use TOUZA_Nio_cache,only: cache_var_read, cache_co_size
    implicit none
    real(kind=C_FLOAT),    intent(out)      :: d(*)
    integer(kind=C_SIZE_T),intent(in),value :: rec
    integer(kind=C_SIZE_T),intent(in)       :: start(0:*), count(0:*)
    integer(kind=C_INT),   intent(in),value :: handle
    integer(kind=C_INT),   intent(in),value :: vid
    integer nco, jco, jeff
    integer jerr
    integer st(0:lax-1), co(0:lax-1)

    nco = cache_co_size(int(handle), int(vid))
    jerr = min(0, nco)
    do jeff = 0, nco - 1
       jco = nco - 1 - jeff
       st(jco) = int(start(jeff))
       co(jco) = int(count(jeff))
    enddo

    if (jerr.eq.0) then
       call cache_var_read &
            & (jerr,        d,           &
            &  int(handle), int(vid),    &
            &  int(rec),    st(0:nco-1), co(0:nco-1))
    endif
    ierr = jerr
  end function tnb_var_read_float
!!!_  - tnb_var_read_double()
  integer(kind=C_INT) function tnb_var_read_double &
       & (d, rec, start, count, handle, vid) BIND(C) result(ierr)
    use TOUZA_Nio_cache,only: cache_var_read, cache_co_size
    implicit none
    real(kind=C_DOUBLE),   intent(out)      :: d(*)
    integer(kind=C_SIZE_T),intent(in),value :: rec
    integer(kind=C_SIZE_T),intent(in)       :: start(0:*), count(0:*)
    integer(kind=C_INT),   intent(in),value :: handle
    integer(kind=C_INT),   intent(in),value :: vid
    integer nco, jco, jeff
    integer jerr
    integer st(0:lax-1), co(0:lax-1)

    nco = cache_co_size(int(handle), int(vid))
    jerr = min(0, nco)
    do jeff = 0, nco - 1
       jco = nco - 1 - jeff
       st(jco) = int(start(jeff))
       co(jco) = int(count(jeff))
    enddo

    if (jerr.eq.0) then
       call cache_var_read &
            & (jerr,        d,           &
            &  int(handle), int(vid),    &
            &  int(rec),    st(0:nco-1), co(0:nco-1))
    endif
    ierr = jerr
  end function tnb_var_read_double

!!!_ + common interfaces
!!!_  & init
  subroutine init &
       & (ierr, u, levv, mode, stdv)
    use TOUZA_Nio_std,   only: control_mode,  control_deep, is_first_force
    use TOUZA_Nio_std,   only: ns_init=>init, choice
    use TOUZA_Nio_record,only: nr_init=>init
    use TOUZA_Nio_cache, only: nc_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
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
          if (ierr.eq.0) call ns_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv)
          if (ierr.eq.0) call nr_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv)
          if (ierr.eq.0) call nc_init(ierr, u=ulog, levv=lv, mode=lmd)
       endif
       if (md.ge.MODE_DEEP) then

       endif
       if (init_counts.eq.0) then

       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = _ERROR(ERR_FAILURE_INIT)
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Nio_std,   only: control_mode,  control_deep, is_first_force
    use TOUZA_Nio_std,   only: ns_diag=>diag, choice, msg, is_msglev_normal, is_msglev_info
    use TOUZA_Nio_record,only: nr_diag=>diag
    use TOUZA_Nio_cache, only: nc_diag=>diag
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
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __MDL__, utmp)
             if (is_msglev_normal(lv)) then

             endif
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ns_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nc_diag(ierr, utmp, levv=lv, mode=lmd)
       endif
       if (md.ge.MODE_DEEP) then

       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Nio_std,   only: control_mode,  control_deep, is_first_force
    use TOUZA_Nio_std,   only: ns_finalize=>finalize, choice
    use TOUZA_Nio_record,only: nr_finalize=>finalize
    use TOUZA_Nio_cache, only: nc_finalize=>finalize
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
          if (ierr.eq.0) call nr_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nc_finalize(ierr, utmp, levv=lv, mode=lmd)
       endif
       if (md.ge.MODE_DEEP) then

       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize
!!!_ + user procedures
!!!_ + internal procedures
!!!_  & c2f_string - convert char to character
  subroutine c2f_string(dest, src)
    implicit none
    character(len=*),            intent(out) :: dest
    character(len=1,kind=C_CHAR),intent(in)  :: src(0:*)
    integer j, l
    l = 0
    do
       if (src(l).eq.C_NULL_CHAR) exit
       l = l + 1
    enddo
    dest = ' '
    do j = 0, l - 1
       dest(j+1:j+1) = src(j)
    enddo
  end subroutine c2f_string

!!!_  & f2c_string - convert character to char
  subroutine f2c_string(dest, src)
    implicit none
    character(len=1,kind=C_CHAR),intent(out) :: dest(0:*)
    character(len=*),optional,   intent(in)  :: src
    integer j, l
    if (present(src)) then
       l = len_trim(src)
       do j = 0, l - 1
          dest(j) = src(j+1:j+1)
       enddo
       dest(l) = C_NULL_CHAR
    else
       dest(0) = C_NULL_CHAR
    endif
  end subroutine f2c_string
!!!_ + end module
end module TOUZA_Nio_bindc

!!!_@ test_nio_bindc - test program
#ifdef TEST_NIO_BINDC
program test_nio_bindc
  use TOUZA_Nio_bindc
  implicit none
  integer ierr

  ierr = 0
101 format(A,' = ', I0)
  call init(ierr)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call finalize(ierr)
  write(*, 101) 'FINAL', ierr
  stop
contains

end program test_nio_bindc

#endif /* TEST_NIO_BINDC */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

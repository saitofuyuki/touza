!!!_! nio_nctcdf.F90 - TOUZA/Nio nanchatte netcdf interface
! Maintainer: SAITO Fuyuki
! Created: Jul 28 2022
#define TIME_STAMP 'Time-stamp: <2023/03/20 21:54:00 fuyuki nio_nctcdf.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022, 2023
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_nio.h"
#ifndef   OPT_NCTCDF_MAX_FILES
#  define OPT_NCTCDF_MAX_FILES 256
#endif
!!!_@ TOUZA_Nio_nctcdf - nio netcdf interfaces
module TOUZA_Nio_nctcdf
!!!_ = declaration
  use netcdf
  use TOUZA_Nio_std,only: &
       & KI32, KI64, KDBL, KFLT, &
       & control_mode, control_deep, is_first_force, &
       & get_logu,     unit_global,  trace_fine,   trace_control
  implicit none
  private
!!!_  - parameters
  integer,public,parameter :: lname = 8

  integer,parameter :: status_none = 0
  integer,parameter :: status_define = 1
  integer,parameter :: status_data = 2

  character(len=*),parameter :: var_header='gtheader'
  character(len=*),parameter :: var_time='time'
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = NIO_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
#define __MDL__ 'n'
#define _ERROR(E) (E - ERR_MASK_NIO_NCTCDF)
!!!_  - type
  type nctcdf_t
     integer :: ncid
     integer :: status
     integer :: vhid      ! header variable
     integer :: drecid    ! record dimension
     integer :: nrec      ! record
     integer :: varid
     integer :: vtid      ! time variable
     integer :: dimid(3)
     integer :: ndim(3)
  end type nctcdf_t
!!!_  - static
  integer,save :: mncf=-1, lncf=-1
  type(nctcdf_t),allocatable,save :: ncfiles(:)
!!!_  - interfaces
  interface nct_write_data
     module procedure nct_write_data_d,nct_write_data_f,nct_write_data_i
  end interface nct_write_data
!!!_  - public procedures
  public init, diag, finalize
  public nct_open_write, nct_close_write
  public nct_define_write, nct_write_data
!!!_  - public shared
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init &
       & (ierr, u, levv, mode, stdv, maxf)
    use TOUZA_Nio_std,   only: ns_init=>init, choice, get_size_bytes, KDBL
    use TOUZA_Nio_header,only: nh_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    integer,intent(in),optional :: maxf
    integer lv, md, lmd

    ierr = 0

    md = control_mode(mode, MODE_DEEPEST)
    init_mode = md

    if (md.ge.MODE_SURFACE) then
       err_default = ERR_SUCCESS
       lv = choice(lev_verbose, levv)
       if (is_first_force(init_counts, md)) then
          ulog = choice(ulog, u)
          lev_verbose = lv
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ns_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv)
          if (ierr.eq.0) call nh_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv)
       endif
       if (md.ge.MODE_DEEP) then
          continue
       endif
       if (init_counts.eq.0) then
          if (ierr.eq.0) call init_alloc(ierr, maxf)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Nio_std,only: ns_diag=>diag, choice, msg, is_msglev_normal, is_msglev_info
    use TOUZA_Nio_header,only: nh_diag=>diag
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
       if (is_first_force(diag_counts, md)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __MDL__, utmp)
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ns_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nh_diag(ierr, utmp, levv=lv, mode=lmd)
       endif
       if (md.ge.MODE_DEEP) then
          continue
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Nio_std,   only: ns_finalize=>finalize, choice
    use TOUZA_Nio_header,only: nh_finalize=>finalize
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
       if (is_first_force(fine_counts, md)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ns_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nh_finalize(ierr, utmp, levv=lv, mode=lmd)
       endif
       if (md.ge.MODE_DEEP) then
          continue
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + init subcontracts
!!!_  - init_alloc
  subroutine init_alloc &
       & (ierr, max_files)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: max_files

    integer mf

    ierr = 0
    if (ierr.eq.0) then
       mf = choice(0, max_files)
       if (mf.le.0) mf = OPT_NCTCDF_MAX_FILES
    endif
    if (ierr.eq.0) then
       lncf = mf
       mncf = 0
       allocate(ncfiles(0:lncf-1), STAT=ierr)
    endif
    return
  end subroutine init_alloc
!!!_ + user interfaces
!!!_  - nct_open_read
  subroutine nct_open_read &
       & (ierr, handle, path, action)
    use TOUZA_Nio_header,only: nitem, litem
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: handle
    character(len=*),intent(in)  :: path
    character(len=*),intent(in)  :: action
    ierr = _ERROR(ERR_NOT_IMPLEMENTED)
  end subroutine nct_open_read
!!!_  - nct_open_write
  subroutine nct_open_write &
       & (ierr, handle, path, status, type_time, iomsg)
    use TOUZA_Nio_header,only: nitem, litem
    use TOUZA_Nio_std,only: sus_is_status_new, choice
    implicit none
    integer,         intent(out)            :: ierr
    integer,         intent(out)            :: handle
    character(len=*),intent(in)             :: path
    character(len=*),intent(in)             :: status
    integer,         intent(in),   optional :: type_time
    character(len=*),intent(inout),optional :: iomsg
    integer istt
    integer ncid
    integer vhid, vtid
    integer dim_rec
    integer dim_nitem, dim_litem
    integer mode
    integer tt

    ierr = 0

    if (sus_is_status_new(status, 'N')) then
       mode = nf90_noclobber
    else
       mode = nf90_clobber
    endif

    istt = nf90_create(path=path, cmode=mode, ncid=ncid)
    if (istt.eq.NF90_NOERR) then
       handle = mncf
       mncf = mncf + 1
       if (mncf.ge.lncf) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    else
       ierr = _ERROR(ERR_PANIC)
    endif
    if (ierr.eq.0) then
       ncfiles(handle)%ncid = ncid
       ncfiles(handle)%status = status_define
       ncfiles(handle)%ndim(:) = 0

       ncfiles(handle)%nrec = 0
       istt = nf90_def_dim(ncid, 'rec', nf90_unlimited, dim_rec)
       if (istt.ne.NF90_NOERR) ierr = _ERROR(ERR_PANIC)
       if (ierr.eq.0) ncfiles(handle)%drecid = dim_rec
    endif
    if (ierr.eq.0) then
       istt = nf90_def_dim(ncid, 'nitem', nitem, dim_nitem)
       if (istt.eq.NF90_NOERR) istt = nf90_def_dim(ncid, 'litem', litem, dim_litem)
       if (istt.ne.NF90_NOERR) ierr = _ERROR(ERR_PANIC)
    endif
    if (ierr.eq.0) then
       istt = nf90_def_var(ncid, var_header, nf90_char, (/dim_litem, dim_nitem, dim_rec/), vhid)
       if (istt.ne.NF90_NOERR) ierr = _ERROR(ERR_PANIC)
       if (ierr.eq.0) ncfiles(handle)%vhid = vhid
    endif
    if (ierr.eq.0) then
       tt = choice(nf90_double, type_time)
       istt = nf90_def_var(ncid, var_time, tt, (/dim_rec/), vtid)
       if (istt.ne.NF90_NOERR) ierr = _ERROR(ERR_PANIC)
       if (ierr.eq.0) ncfiles(handle)%vtid = vtid
    endif
    if (istt.ne.NF90_NOERR) then
       if (present(iomsg)) then
          iomsg = trim(nf90_strerror(istt))
       endif
    endif
  end subroutine nct_open_write

!!!_  - nct_close_write
  subroutine nct_close_write &
       & (ierr, handle)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: handle
    integer istt
    integer ncid

    ierr = 0
    ncid = ncfiles(handle)%ncid
    istt = nf90_close(ncid)
    if (ierr .ne. NF90_NOERR) ierr = _ERROR(ERR_PANIC)
  end subroutine nct_close_write

!!!_  - nct_define_write
  subroutine nct_define_write &
       & (ierr, handle, head)
    use TOUZA_Nio_header,only: litem, nitem, hi_ITEM, hi_TIME, get_item
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: handle
    character(len=*),intent(in)  :: head(*)

    integer istt
    integer ncid, varid
    character(len=litem) :: hitm, htim
    integer ofs(3)
    integer itime(1)

    ierr = 0
    ncid = ncfiles(handle)%ncid
    if (ierr.eq.0) call get_item(ierr, head, hitm, hi_ITEM)
    if (ierr.eq.0) then
       istt = nf90_inq_varid(ncid, hitm, varid)
       if (istt.ne.NF90_NOERR) then
          call nct_define_variable(ierr, varid, handle, head)
       endif
       if (ierr.eq.0) ncfiles(handle)%varid = varid
    endif
    if (ierr.eq.0) then
       varid = ncfiles(handle)%vhid
       ofs(1:2) = (/1, 1/)
       ofs(3) = ncfiles(handle)%nrec + 1
       istt = nf90_put_var(ncid, varid, head(1:nitem), ofs(:), (/litem, nitem, 1/))
       if (ierr .ne. NF90_NOERR) ierr = _ERROR(ERR_PANIC)
    endif
    if (ierr.eq.0) call get_item(ierr, head, itime(1), hi_TIME)
    if (ierr.eq.0) then
       varid = ncfiles(handle)%vtid
       ofs(1) = ncfiles(handle)%nrec + 1
       istt = nf90_put_var(ncid, varid, itime, ofs(1:1), (/1/))
       if (ierr .ne. NF90_NOERR) ierr = _ERROR(ERR_PANIC)
    else
       ierr = 0
    endif

  end subroutine nct_define_write

!!!_  & nct_write_data
  subroutine nct_write_data_d &
       & (ierr, &
       &  d,    ld, handle, head, sync)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(*)
    integer,         intent(in)  :: ld
    integer,         intent(in)  :: handle
    character(len=*),intent(in)  :: head(*)
    logical,optional,intent(in)  :: sync

    integer ncid, varid
    integer ofs(4), dims(4)
    integer istt

    ierr = 0
    ncid = ncfiles(handle)%ncid
    varid = ncfiles(handle)%varid

    ofs(1:3) = (/1, 1, 1/)
    ofs(4) = ncfiles(handle)%nrec + 1
    dims(1:3) = ncfiles(handle)%ndim(1:3)
    dims(4) = 1

    istt = nf90_put_var(ncid, varid, d(1:ld), ofs(:), dims(:))
    if (istt.ne.NF90_NOERR) ierr = _ERROR(ERR_PANIC)

    if (ierr.eq.0) ncfiles(handle)%nrec = ofs(4)

    if (ierr.eq.0) then
       if (choice(.TRUE., sync)) then
          istt = nf90_sync(ncid)
          if (istt.ne.NF90_NOERR) ierr = _ERROR(ERR_PANIC)
       endif
    endif
    return
  end subroutine nct_write_data_d
  subroutine nct_write_data_f &
       & (ierr, &
       &  d,    ld, handle, head, sync)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(*)
    integer,         intent(in)  :: ld
    integer,         intent(in)  :: handle
    character(len=*),intent(in)  :: head(*)
    logical,optional,intent(in)  :: sync

    integer ncid, varid
    integer ofs(4), dims(4)
    integer istt

    ierr = 0
    ncid = ncfiles(handle)%ncid
    varid = ncfiles(handle)%varid

    ofs(1:3) = (/1, 1, 1/)
    ofs(4) = ncfiles(handle)%nrec + 1
    dims(1:3) = ncfiles(handle)%ndim(1:3)
    dims(4) = 1

    istt = nf90_put_var(ncid, varid, d(1:ld), ofs(:), dims(:))
    if (istt.ne.NF90_NOERR) ierr = _ERROR(ERR_PANIC)

    if (ierr.eq.0) ncfiles(handle)%nrec = ofs(4)

    if (ierr.eq.0) then
       if (choice(.TRUE., sync)) then
          istt = nf90_sync(ncid)
          if (istt.ne.NF90_NOERR) ierr = _ERROR(ERR_PANIC)
       endif
    endif
    return
  end subroutine nct_write_data_f
  subroutine nct_write_data_i &
       & (ierr, &
       &  d,    ld, handle, head, sync)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: d(*)
    integer,         intent(in)  :: ld
    integer,         intent(in)  :: handle
    character(len=*),intent(in)  :: head(*)
    logical,optional,intent(in)  :: sync

    integer ncid, varid
    integer ofs(4), dims(4)
    integer istt

    ierr = 0
    ncid = ncfiles(handle)%ncid
    varid = ncfiles(handle)%varid

    ofs(1:3) = (/1, 1, 1/)
    ofs(4) = ncfiles(handle)%nrec + 1
    dims(1:3) = ncfiles(handle)%ndim(1:3)
    dims(4) = 1

    istt = nf90_put_var(ncid, varid, d(1:ld), ofs(:), dims(:))
    if (istt.ne.NF90_NOERR) ierr = _ERROR(ERR_PANIC)

    if (ierr.eq.0) ncfiles(handle)%nrec = ofs(4)

    if (ierr.eq.0) then
       if (choice(.TRUE., sync)) then
          istt = nf90_sync(ncid)
          if (istt.ne.NF90_NOERR) ierr = _ERROR(ERR_PANIC)
       endif
    endif
    return
  end subroutine nct_write_data_i

!!!_  - nct_define_variable
  subroutine nct_define_variable &
       & (ierr, varid, handle, head)
    use TOUZA_Nio_std,only: upcase
    use TOUZA_Nio_header,only: litem, get_item, hi_ITEM, hi_DFMT, hi_MISS
    use TOUZA_Nio_header,only: hi_AITM1, hi_ASTR1, hi_AEND1
    use TOUZA_Nio_header,only: hi_AITM2, hi_ASTR2, hi_AEND2
    use TOUZA_Nio_header,only: hi_AITM3, hi_ASTR3, hi_AEND3
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: varid
    integer,         intent(in)  :: handle
    character(len=*),intent(in)  :: head(*)

    integer istt
    integer ncid
    integer dimid(4)
    integer ndim(3)
    integer nfmt
    character(len=litem) :: hitm, hfmt
    character(len=3) :: f
    integer vmiss_i
    real(kind=KFLT) vmiss_f
    real(kind=KDBL) vmiss_d
    integer fill

    ierr = 0
    ncid = ncfiles(handle)%ncid
    if (ierr.eq.0) call nct_define_dim(ierr, dimid(1), ndim(1), handle, head, 1, hi_AITM1, hi_ASTR1, hi_AEND1)
    if (ierr.eq.0) call nct_define_dim(ierr, dimid(2), ndim(2), handle, head, 2, hi_AITM2, hi_ASTR2, hi_AEND2)
    if (ierr.eq.0) call nct_define_dim(ierr, dimid(3), ndim(3), handle, head, 3, hi_AITM3, hi_ASTR3, hi_AEND3)

    if (ierr.eq.0) ncfiles(handle)%dimid(1:3) = dimid(1:3)
    if (ierr.eq.0) ncfiles(handle)%ndim(1:3) = ndim(1:3)

    if (ierr.eq.0) call get_item(ierr, head, hitm, hi_ITEM)
    if (ierr.eq.0) call get_item(ierr, head, hfmt, hi_DFMT)
    if (ierr.eq.0) then
       call upcase(f, hfmt)
       select case(f(1:3))
       case('CR8')
          nfmt = nf90_double
       case('CR4')
          nfmt = nf90_float
       case('CI4')
          nfmt = nf90_int
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    if (ierr.eq.0) then
       dimid(4) = ncfiles(handle)%drecid
       istt = nf90_def_var(ncid, hitm, nfmt, dimid(:), varid)
       if (istt.ne.NF90_NOERR) ierr = _ERROR(ERR_PANIC)
    endif
    if (ierr.eq.0) then
       fill = 0
       select case(nfmt)
       case(nf90_double)
          call get_item(istt, head, vmiss_d, hi_MISS, def=0.0_KDBL)
          if (istt.eq.0) then
             if (vmiss_d.ne.0.0_KDBL) istt = nf90_def_var_fill(ncid, varid, fill, vmiss_d)
          endif
       case(nf90_float)
          call get_item(istt, head, vmiss_f, hi_MISS, def=0.0_KFLT)
          if (istt.eq.0) then
             if (vmiss_f.ne.0.0_KFLT) istt = nf90_def_var_fill(ncid, varid, fill, vmiss_f)
          endif
       case(nf90_int)
          call get_item(istt, head, vmiss_i, hi_MISS, def=0)
          if (istt.eq.0) then
             if (vmiss_i.ne.0) istt = nf90_def_var_fill(ncid, varid, fill, vmiss_i)
          endif
       case default
          istt = 0
       end select
       if (istt.ne.NF90_NOERR) ierr = _ERROR(ERR_PANIC)
    endif
    if (ierr.eq.0) then
       istt = nf90_enddef(ncid)
       if (istt.ne.NF90_NOERR) ierr = _ERROR(ERR_PANIC)
    endif

  end subroutine nct_define_variable

!!!_  - nct_define_dim
  subroutine nct_define_dim &
       & (ierr, dimid, ndim, handle, head, cidx, hax, hmin, hmax)
    use TOUZA_Nio_header,only: litem, get_item
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: dimid
    integer,         intent(out) :: ndim
    integer,         intent(in)  :: handle
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: cidx
    integer,         intent(in)  :: hax, hmin, hmax

    integer ncid
    character(len=litem) :: xname
    integer :: jmin, jmax
    integer istt

    ierr = 0

    ncid = ncfiles(handle)%ncid
    if (ierr.eq.0) call get_item(ierr, head, xname, hax)
    if (ierr.eq.0) call get_item(ierr, head, jmin, hmin)
    if (ierr.eq.0) call get_item(ierr, head, jmax, hmax)

    if (ierr.eq.0) then
101    format('COOR', I3.3)
       if (xname.eq.' ') write(xname, 101) cidx
       ndim = jmax - jmin + 1
       istt = nf90_def_dim(ncid, xname, ndim, dimid)
       if (istt.ne.NF90_NOERR) ierr = _ERROR(ERR_PANIC)
    endif
  end subroutine nct_define_dim
!!!_ + end module
end module TOUZA_Nio_nctcdf

!!!_@ test_nio_nctcdf - test program
#ifdef TEST_NIO_NCTCDF
program test_nio_nctcdf
  use TOUZA_Std,only: parse, get_param, arg_diag, arg_init
  use TOUZA_Nio_std,only: KBUF=>KDBL
  use TOUZA_Nio_nctcdf, nn_init=>init, nn_diag=>diag, nn_finalize=>finalize
  use TOUZA_Nio_header, nh_init=>init

  implicit none
  integer ierr
  integer jarg
  integer ktest
  integer hfile1, hfile2
  integer nx, ny, nz, nn

  real(kind=KBUF) :: vmiss = -999.0_KBUF
  real(kind=KBUF),allocatable :: v(:)
  character(len=litem) :: hd(nitem)

  ierr = 0
  jarg = 0
101 format(A,' = ', 16(1x, I0))
  call nn_init(ierr, stdv=+9)
  ! if (ierr.eq.0) call diag(ierr, u=-1, levv=+99)
  if (ierr.eq.0) call nn_diag(ierr, levv=+9)
  if (ierr.eq.0) call arg_init(ierr, levv=-9)
  if (ierr.eq.0) call parse(ierr)
  if (ierr.eq.0) call arg_diag(ierr)
  if (ierr.eq.0) then
     jarg = jarg + 1
     call get_param(ierr, ktest, jarg, 0)
  endif

  if (ierr.eq.0) call nct_open_write(ierr, hfile1, '00test1.nc', 'W')
  if (ierr.eq.0) call nct_open_write(ierr, hfile2, '00test2.nc', 'W')
  write(*, 101) 'OPEN', ierr, hfile1, hfile2

  nx = 5
  ny = 7
  nz = 2
  nn = nx * ny * nz

  allocate(v(1:nn), STAT=ierr)

  hd(:) = ' '
  if (ierr.eq.0) call put_item(ierr, hd, 9010,   hi_IDFM)
  if (ierr.eq.0) call put_item(ierr, hd, 1,      hi_ASTR1)
  if (ierr.eq.0) call put_item(ierr, hd, nx,     hi_AEND1)
  if (ierr.eq.0) call put_item(ierr, hd, 'x',    hi_AITM1)
  if (ierr.eq.0) call put_item(ierr, hd, 1,      hi_ASTR2)
  if (ierr.eq.0) call put_item(ierr, hd, ny,     hi_AEND2)
  if (ierr.eq.0) call put_item(ierr, hd, 'y',    hi_AITM2)
  if (ierr.eq.0) call put_item(ierr, hd, 1,      hi_ASTR3)
  if (ierr.eq.0) call put_item(ierr, hd, nz,     hi_AEND3)
  if (ierr.eq.0) call put_item(ierr, hd, 'z',    hi_AITM3)
  if (ierr.eq.0) call put_item(ierr, hd, vmiss,  hi_MISS)

  if (ierr.eq.0) call put_item(ierr, hd, 'T2', hi_ITEM)

  if (ierr.eq.0) call show_header(ierr, hd)
  if (ierr.eq.0) call put_item(ierr, hd, 11,  hi_TIME)

  if (ierr.eq.0) call put_item(ierr, hd, 'CR8',  hi_DFMT)
  if (ierr.eq.0) call nct_define_write(ierr, hfile1, hd)
  if (ierr.eq.0) call put_item(ierr, hd, 'CR4',  hi_DFMT)
  if (ierr.eq.0) call nct_define_write(ierr, hfile2, hd)

  v(:) = 123
  if (ierr.eq.0) call nct_write_data(ierr, v, nn, hfile1, hd)
  if (ierr.eq.0) call nct_define_write(ierr, hfile1, hd)
  if (ierr.eq.0) call nct_write_data(ierr, v, nn, hfile2, hd)
  if (ierr.eq.0) call nct_define_write(ierr, hfile2, hd)

  if (ierr.eq.0) call put_item(ierr, hd, 22,  hi_TIME)
  v(:) = 234
  if (ierr.eq.0) call nct_write_data(ierr, v, nn, hfile1, hd)
  if (ierr.eq.0) call nct_write_data(ierr, v, nn, hfile2, hd)

  if (ierr.eq.0) call nct_close_write(ierr, hfile1)
  if (ierr.eq.0) call nct_close_write(ierr, hfile2)

  if (ierr.eq.0) call nn_finalize(ierr)
  write(*, 101) 'FINAL', ierr
  stop
contains

end program test_nio_nctcdf

#endif /* TEST_NIO_NCTCDF */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

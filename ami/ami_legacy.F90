!!!_! ami_legacy.F90 - TOUZA/Ami/legacy
! Maintainer: SAITO Fuyuki
! Created: Jan 19 2023
#define TIME_STAMP 'Time-stamp: <2024/06/27 10:15:45 fuyuki ami_legacy.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2023,2024
!           Japan Agency for Marine-Earth Science and Technology
!
!!!_* include
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_ami.h"
!!!_* macros
#ifndef   TEST_AMI_LEGACY
#  define TEST_AMI_LEGACY 0
#endif
!!!_@ TOUZA_Ami_legacy - ami-da procedures for legacy format
module TOUZA_Ami_legacy
!!!_ + modules
  use TOUZA_Ami_std, as_init=>init, as_diag=>diag, as_finalize=>finalize
!!!_ + default
  implicit none
  private
!!!_ + parameter
  integer,parameter,public :: legacy_error = -1
  integer,parameter,public :: legacy_miroc4 = 1
  integer,parameter,public :: legacy_miroc5 = 2
!!!_ + static
!!!_  - buffer
  integer,allocatable,save :: buf_ijo(:)
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = AMI_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
#define __MDL__ 'l'
#define _ERROR(E) (E - ERR_MASK_AMI_LEGACY)
!!!_ + interface
  interface open_rafile_legacy
     module procedure open_rafile_legacy_d
  end interface open_rafile_legacy
  interface read_rafile_legacy
     module procedure read_rafile_legacy_d
  end interface read_rafile_legacy
  interface write_rafile_legacy
     module procedure write_rafile_legacy_d
  end interface write_rafile_legacy

  interface open_rofile_legacy
     module procedure open_rofile_legacy_d
  end interface open_rofile_legacy
  interface read_rofile_legacy1
     module procedure read_rofile_legacy1_d
  end interface read_rofile_legacy1
  interface read_rofile_legacy2
     module procedure read_rofile_legacy2_d
  end interface read_rofile_legacy2
  interface read_rofile_legacy3
     module procedure read_rofile_legacy3_d
  end interface read_rofile_legacy3
  interface write_rofile_legacy1
     module procedure write_rofile_legacy1_d
  end interface write_rofile_legacy1
  interface write_rofile_legacy2
     module procedure write_rofile_legacy2_d
  end interface write_rofile_legacy2

  interface open_geofile_tripolar
     module procedure open_geofile_tripolar_d
  end interface open_geofile_tripolar
  interface read_geofile_tripolar
     module procedure read_geofile_tripolar_d
  end interface read_geofile_tripolar
  interface read_skip_geofile_tripolar
     module procedure read_skip_geofile_tripolar_d
  end interface read_skip_geofile_tripolar

!!!_ + public procedures
  public init, diag, finalize
  public open_rafile_legacy,  open_rofile_legacy
  public read_rafile_legacy
  public read_rofile_legacy1, read_rofile_legacy2, read_rofile_legacy3
  public write_rafile_legacy
  public write_rofile_legacy1, write_rofile_legacy2
  public open_geofile_tripolar, read_geofile_tripolar
!!!_ + function-like macros
!!!_ + procedures
contains
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    integer,intent(in),optional :: icomm
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
          if (ierr.eq.0) call as_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
       endif
       if (is_first_force(init_counts, mode)) then
          continue
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
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
          endif
       endif
       if (md.ge.MODE_SHALLOW) then
          if (allocated(buf_ijo)) then
             if (is_msglev_INFO(lv)) then
                if (ierr.eq.0) then
                   call msg('(''buffer allocated = '', I0)', (/size(buf_ijo)/), __MDL__, utmp)
                endif
             endif
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call as_diag(ierr, utmp, levv=lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
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
          if (ierr.eq.0) call as_finalize(ierr, utmp, levv=lv, mode=lmd)
       endif
       if (allocated(buf_ijo)) then
          if (ierr.eq.0) deallocate(buf_ijo, STAT=ierr)
       endif
       fine_counts = fine_counts + 1
    endif
  end subroutine finalize
!!!_ + user procedures
!!!_  - open_rafile_legacy - open RAFILE to read and check basic properties
  subroutine open_rafile_legacy_d &
       & (ierr, klegacy, &
       &  swap, ij_amax, len_a2m, nxygdm, &
       &  file, u,       mold)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: is_error_match
    use TOUZA_Ami_std,only: sus_open, sus_read_irec, sus_skip_irec
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: klegacy
    integer,         intent(out)         :: ij_amax, len_a2m, nxygdm
    logical,         intent(out)         :: swap
    character(len=*),intent(in)          :: file
    integer,         intent(in)          :: u
    real(kind=KTGT), intent(in),optional :: mold

    integer l
    integer,parameter :: tol = 1  ! first record size tolerance
    integer v(1)

    ierr = 0
    klegacy = legacy_error

    ij_amax = 0
    len_a2m = 0
    nxygdm = 0

    swap = .false.
    if (ierr.eq.0) call sus_open(ierr, u, file, ACTION='R', STATUS='O')
    if (ierr.eq.0) then
       ! check first record
       call sus_record_mems_irec(ierr, l, u, mold=INT(0), swap=swap)
       if (ierr.ne.0.or.l.gt.tol) then
          rewind(u, IOSTAT=ierr)
          if (ierr.eq.0) then
             swap = .not.swap
             call sus_record_mems_irec(ierr, l, u, mold=INT(0), swap=swap)
          endif
       endif
       if (ierr.ne.0.or.l.gt.tol) then
          ierr = _ERROR(ERR_PANIC)
          return
       endif
    endif
    if (ierr.eq.0) rewind(u, IOSTAT=ierr)
    ! record 0
    if (ierr.eq.0) call sus_read_irec(ierr, u, v, 1, swap)
    if (ierr.eq.0) ij_amax = v(1)
    ! record 1
    if (ierr.eq.0) call sus_skip_irec(ierr, u, 1, swap=swap)
    ! record 2
    if (ierr.eq.0) call sus_read_irec(ierr, u, v, 1, swap)
    if (ierr.eq.0) len_a2m = v(1)
    ! record 3 4 5
    if (ierr.eq.0) call sus_skip_irec(ierr, u, 3, swap=swap)
    ! record 6
    if (ierr.eq.0) call sus_record_mems_irec(ierr, l, u, mold, swap=swap)
    if (ierr.eq.0) then
       if (mod(l, 2).eq.0) then
          nxygdm = l / 2
       else
          ierr = _ERROR(ERR_BROKEN_RECORD)
       endif
    endif
    if (ierr.eq.0) call sus_skip_irec(ierr, u, 1, swap=swap)
    ! record 7
    if (ierr.eq.0) then
       call sus_record_mems_irec(ierr, l, u, mold, swap=swap)
       if (is_error_match(ierr, ERR_EOF)) then
          ierr = 0
          klegacy = legacy_miroc4
       else if (ierr.eq.0) then
          if (nxygdm.ne.l) then
             ierr = _ERROR(ERR_BROKEN_RECORD)
          else
             klegacy = legacy_miroc5
          endif
       endif
    endif
    if (ierr.eq.0) rewind(u, IOSTAT=ierr)

    if (ierr.ne.0) then
       ierr = _ERROR(ERR_BROKEN_RECORD)
    endif

  end subroutine open_rafile_legacy_d
!!!_  - read_rafile_legacy - read RAFILE
  subroutine read_rafile_legacy_d &
       & (ierr,     &
       &  ij_ahead, ijrecov_a2m, ijc2o,  satm, ru, rv,  rocn, &
       &  ij_amax,  len_a2m,     nxygdm, u,    swap,    klegacy)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: choice
    use TOUZA_Ami_std,only: sus_is_stream_unit, sus_read_irec, sus_suspend_read_irec
    implicit none
    integer,        intent(out)          :: ierr
    integer,        intent(out)          :: ij_ahead(0:*)                ! ij_amax
    integer,        intent(out)          :: ijrecov_a2m(*), ijc2o(*)     ! len_a2m
    real(kind=KTGT),intent(out)          :: satm(*)                      ! len_a2m
    real(kind=KTGT),intent(out)          :: ru(*), rv(*)                 ! nxygdm
    real(kind=KTGT),intent(out),optional :: rocn(*)                      ! nxygdm
    integer,        intent(in)           :: ij_amax, len_a2m, nxygdm
    integer,        intent(in)           :: u
    logical,        intent(in),optional  :: swap
    integer,        intent(in),optional  :: klegacy
    integer v(2)
    logical sw
    integer kfmt

    ierr = 0
    kfmt = choice(legacy_miroc5, klegacy)

    if (sus_is_stream_unit(u)) then
       if (ierr.eq.0) call sus_read_irec(ierr, u, v(1:1),      1,         swap)
       if (ierr.eq.0) call sus_read_irec(ierr, u, ij_ahead,    ij_amax+1, swap)
       if (ierr.eq.0) call sus_read_irec(ierr, u, v(2:2),      1,         swap)
       if (ierr.eq.0) call sus_read_irec(ierr, u, ijrecov_a2m, len_a2m,   swap)
       if (ierr.eq.0) call sus_read_irec(ierr, u, ijc2o,       len_a2m,   swap)
       if (ierr.eq.0) call sus_read_irec(ierr, u, satm,        len_a2m,   swap)
       ! ru rv are in the same record
       if (ierr.eq.0) call sus_suspend_read_irec(ierr, u, ru, nxygdm, suspend_begin, swap)
       if (ierr.eq.0) call sus_suspend_read_irec(ierr, u, rv, nxygdm, suspend_end,   swap)
       if (ierr.eq.0) then
          if (kfmt.ne.legacy_miroc4.and.present(rocn)) then
             call sus_read_irec(ierr, u, rocn,        nxygdm,    swap)
          endif
       endif
    else
       sw = choice(.FALSE., swap)
       if (swap) then
          ierr = _ERROR(ERR_INVALID_PARAMETER)
       else
          if (ierr.eq.0) read(u, IOSTAT=ierr) v(1:1)
          if (ierr.eq.0) read(u, IOSTAT=ierr) ij_ahead(0:ij_amax)
          if (ierr.eq.0) read(u, IOSTAT=ierr) v(2:2)
          if (ierr.eq.0) read(u, IOSTAT=ierr) ijrecov_a2m(1:len_a2m)
          if (ierr.eq.0) read(u, IOSTAT=ierr) ijc2o(1:len_a2m)
          if (ierr.eq.0) read(u, IOSTAT=ierr) satm(1:len_a2m)
          if (ierr.eq.0) read(u, IOSTAT=ierr) ru(1:nxygdm), rv(1:nxygdm)
          if (ierr.eq.0) then
             if (kfmt.ne.legacy_miroc4.and.present(rocn)) then
                read(u, IOSTAT=ierr) rocn(1:nxygdm)
             endif
          endif
       endif
    endif
    if (ierr.eq.0) then
       if (v(1).ne.ij_amax .or. v(2).ne.len_a2m) then
          ierr = _ERROR(ERR_BROKEN_RECORD)
       endif
    endif
  end subroutine read_rafile_legacy_d
!!!_  - write_rafile_legacy - write RAFILE
  subroutine write_rafile_legacy_d &
       & (ierr,     &
       &  ij_ahead, ijrecov_a2m, ijc2o,  satm, ru, rv, rocn, &
       &  len_a2m,  ij_amax,     nxygdm, u,    swap,   klegacy)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: conv_b2strm, choice
    use TOUZA_Ami_std,only: sus_is_stream_unit, sus_write_irec, sus_suspend_write_irec
    use TOUZA_Ami_std,only: suspend_begin, suspend_end
    implicit none
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: ij_ahead(0:*)            ! ij_amax
    integer,        intent(in)          :: ijrecov_a2m(*), ijc2o(*) ! len_a2m
    real(kind=KTGT),intent(in)          :: satm(*)                  ! len_a2m
    real(kind=KTGT),intent(in)          :: ru(*), rv(*)             ! nxygdm
    real(kind=KTGT),intent(in),optional :: rocn(*)                  ! nxygdm
    integer,        intent(in)          :: len_a2m
    integer,        intent(in)          :: ij_amax, nxygdm
    integer,        intent(in)          :: u
    logical,        intent(in),optional :: swap
    integer,        intent(in),optional :: klegacy
    character(len=16) :: ans
    integer v(2)
    logical sw
    integer kfmt

    ierr = 0
    kfmt = choice(legacy_miroc5, klegacy)

    inquire(unit=u, STREAM=ans, IOSTAT=ierr)
    v(1) = ij_amax
    v(2) = len_a2m
    if (ierr.eq.0) then
       select case (ans(1:1))
       case('Y', 'y')
          if (ierr.eq.0) call sus_write_irec(ierr, u, v(1:1),      1,         swap)
          if (ierr.eq.0) call sus_write_irec(ierr, u, ij_ahead,    ij_amax+1, swap)
          if (ierr.eq.0) call sus_write_irec(ierr, u, v(2:2),      1,         swap)
          if (ierr.eq.0) call sus_write_irec(ierr, u, ijrecov_a2m, len_a2m,   swap)
          if (ierr.eq.0) call sus_write_irec(ierr, u, ijc2o,       len_a2m,   swap)
          if (ierr.eq.0) call sus_write_irec(ierr, u, satm,        len_a2m,   swap)
             ! ru rv are in the same record
          if (ierr.eq.0) then
             call sus_suspend_write_irec(ierr, u, ru, nxygdm, suspend_begin, swap)
          endif
          if (ierr.eq.0) then
             call sus_suspend_write_irec(ierr, u, rv, nxygdm, suspend_end, swap)
          endif
          if (ierr.eq.0) then
             if (kfmt.ne.legacy_miroc4.and.present(rocn)) then
                call sus_write_irec(ierr, u, rocn,        nxygdm,    swap)
             endif
          endif
       case default
          sw = choice(.FALSE., swap)
          if (swap) then
             ierr = _ERROR(ERR_INVALID_PARAMETER)
          else
             if (ierr.eq.0) write(u, IOSTAT=ierr) v(1:1)
             if (ierr.eq.0) write(u, IOSTAT=ierr) ij_ahead(ij_amax+1)
             if (ierr.eq.0) write(u, IOSTAT=ierr) v(2:2)
             if (ierr.eq.0) write(u, IOSTAT=ierr) ijrecov_a2m(1:len_a2m)
             if (ierr.eq.0) write(u, IOSTAT=ierr) ijc2o(1:len_a2m)
             if (ierr.eq.0) write(u, IOSTAT=ierr) satm(1:len_a2m)
             if (ierr.eq.0) write(u, IOSTAT=ierr) ru(1:nxygdm), rv(1:nxygdm)
             if (ierr.eq.0) then
                if (kfmt.ne.legacy_miroc4.and.present(rocn)) then
                   write(u, IOSTAT=ierr) rocn(1:nxygdm)
                endif
             endif
          endif
       end select
    endif
  end subroutine write_rafile_legacy_d
!!!_  - open_rofile_legacy - open ROFILE to read and check basic properties
  subroutine open_rofile_legacy_d &
       & (ierr, &
       &  klegacy, swap, ijdim, file, u, mold)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: sus_open, sus_read_irec, sus_skip_irec
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: klegacy
    logical,         intent(inout)       :: swap       ! need initial guess
    integer,         intent(out)         :: ijdim
    character(len=*),intent(in)          :: file
    integer,         intent(in)          :: u
    real(kind=KTGT), intent(in),optional :: mold

    integer l

    ierr = 0
    klegacy = legacy_error

    ijdim = 0
    if (ierr.eq.0) call sus_open(ierr, u, file, ACTION='R', STATUS='O')
    if (ierr.eq.0) then
       ! check first record
       ! record 0
       call sus_record_mems_irec(ierr, l, u, mold=INT(0), swap=swap)
       if (ierr.ne.0) then
          rewind(u, IOSTAT=ierr)
          if (ierr.eq.0) then
             swap = .not.swap
             call sus_record_mems_irec(ierr, l, u, mold=INT(0), swap=swap)
          endif
       endif
       if (ierr.ne.0) then
          ierr = _ERROR(ERR_PANIC)
          return
       endif
    endif
    if (ierr.eq.0) ijdim = l
    if (ierr.eq.0) then
       ! record 0..ijdim
       call sus_skip_irec(ierr, u, ijdim + 1, swap=swap)
    endif
    if (ierr.eq.0) then
       call sus_record_mems_irec(ierr, l, u, mold, swap=swap)
    endif
    if (ierr.eq.0) then
       if (l.eq.ijdim) then
          klegacy = legacy_miroc5
       else
          call sus_record_mems_irec(ierr, l, u, mold=INT(0), swap=swap)
          if (ierr.eq.0) then
             if (l.ne.ijdim) ierr = _ERROR(ERR_BROKEN_RECORD)
          endif
          if (ierr.eq.0) then
             call sus_skip_irec(ierr, u, ijdim + 1, swap=swap)
          endif
          if (ierr.eq.0) then
             klegacy = legacy_miroc4
          else
             ierr = _ERROR(ERR_BROKEN_RECORD)
          endif
       endif
    endif
    if (ierr.eq.0) rewind(u, IOSTAT=ierr)
  end subroutine open_rofile_legacy_d
!!!_  - read_rofile_legacy1_core - read ROFILE first step
  subroutine read_rofile_legacy1_core_d &
       & (ierr,     &
       &  ij_omax,  len_o2a, ij_o, &
       &  ijdim,    u,       swap)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: sus_is_stream_unit, sus_read_irec
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ij_omax
    integer,         intent(out) :: len_o2a
    integer,         intent(out) :: ij_o(*)                     ! ijdim
    integer,         intent(in)  :: ijdim
    integer,         intent(in)  :: u
    logical,optional,intent(in)  :: swap
    logical sw

    ierr = 0
    ij_omax = 0
    if (sus_is_stream_unit(u)) then
       if (ierr.eq.0) call sus_read_irec(ierr, u, ij_o, ijdim, swap)
    else
       sw = choice(.FALSE., swap)
       if (swap) then
          ierr = _ERROR(ERR_INVALID_PARAMETER)
       else
          if (ierr.eq.0) read(u, IOSTAT=ierr) ij_o(1:ijdim)
       endif
    endif
    if (ierr.eq.0) then
       ij_omax = maxval(ij_o(1:ijdim))
       len_o2a = SUM(ij_o(1:ijdim))
    endif
  end subroutine read_rofile_legacy1_core_d
!!!_  - read_rofile_legacy1 - read ROFILE first step
  subroutine read_rofile_legacy1_d &
       & (ierr,     &
       &  ij_omax,  len_o2a, &
       &  ijdim,    u,       swap, ij_o)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: sus_is_stream_unit, sus_read_irec
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ij_omax
    integer,         intent(out) :: len_o2a
    integer,         intent(in)  :: ijdim
    integer,         intent(in)  :: u
    logical,optional,intent(in)  :: swap
    integer,optional,intent(out) :: ij_o(*)
    if (present(ij_o)) then
       call read_rofile_legacy1_core_d &
            & (ierr, ij_omax,  len_o2a, ij_o, ijdim, u, swap)
    else
       ierr = 0
       if (allocated(buf_ijo)) deallocate(buf_ijo, STAT=ierr)
       if (ierr.eq.0) allocate(buf_ijo(1:ijdim), STAT=ierr)
       if (ierr.eq.0) then
          call read_rofile_legacy1_core_d &
               & (ierr, ij_omax,  len_o2a, buf_ijo, ijdim, u, swap)
       endif
    endif
  end subroutine read_rofile_legacy1_d

!!!_  - read_rofile_legacy2_core - read ROFILE second step
  subroutine read_rofile_legacy2_core_d &
       & (ierr,   &
       &  ij_ohead, ijrecov_o2c, ijo2c,   socn, &
       &  ij_o,     ijdim,       ij_omax, u,    swap)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: choice
    use TOUZA_Ami_std,only: sus_is_stream_unit, sus_read_irec, sus_suspend_read_irec
    implicit none
    integer,        intent(out)          :: ierr
    integer,        intent(out)          :: ij_ohead(0:*)
    integer,        intent(out)          :: ijrecov_o2c(*)
    integer,        intent(out)          :: ijo2c(*)
    real(kind=KTGT),intent(out)          :: socn (*)
    integer,        intent(in)           :: ij_o(*)                     ! ijdim
    integer,        intent(in)           :: ij_omax
    integer,        intent(in)           :: ijdim
    integer,        intent(in)           :: u
    logical,        intent(in),optional  :: swap
    logical sw
    integer jo, jz
    integer vi(ij_omax), np(ij_omax)
    real(kind=KTGT) :: vv(ij_omax)

    ierr = 0

    ij_ohead(0:ij_omax) = 0
    do jo = 1, ijdim
       jz = ij_o(jo)
       ij_ohead(1:jz) = ij_ohead(1:jz) + 1
    enddo
    do jz = 1, ij_omax
       ij_ohead(jz) = ij_ohead(jz) + ij_ohead(jz - 1)
    enddo

    ! compute ijrecov_o2c
    np(1:ij_omax) = 1
    do jo = 1, ijdim
       do jz = 1, ij_o(jo)
          ijrecov_o2c(ij_ohead(jz - 1) + np(jz)) = jo
          np(jz) = np(jz) + 1
       enddo
    enddo

    np(1:ij_omax) = 1
    if (sus_is_stream_unit(u)) then
       do jo = 1, ijdim
          if (ierr.eq.0) call sus_suspend_read_irec(ierr, u, vi, ij_o(jo), suspend_begin, swap)
          if (ierr.eq.0) call sus_suspend_read_irec(ierr, u, vv, ij_o(jo), suspend_end,   swap)
          if (ierr.eq.0) then
             do jz = 1, ij_o(jo)
                ijo2c(ij_ohead(jz - 1) + np(jz)) = vi(jz)
                socn (ij_ohead(jz - 1) + np(jz)) = vv(jz)
                np(jz) = np(jz) + 1
             enddo
          endif
       enddo
    else
       sw = choice(.FALSE., swap)
       if (swap) then
          ierr = _ERROR(ERR_INVALID_PARAMETER)
       else
          do jo = 1, ijdim
             if (ierr.eq.0) read(u, IOSTAT=ierr) vi(1:ij_o(jo)), vv(1:ij_o(jo))
             if (ierr.eq.0) then
                do jz = 1, ij_o(jo)
                   ijo2c(ij_ohead(jz - 1) + np(jz)) = vi(jz)
                   socn (ij_ohead(jz - 1) + np(jz)) = vv(jz)
                   np(jz) = np(jz) + 1
                enddo
             endif
          enddo
             ! if (ierr.eq.0) read(u, IOSTAT=ierr) &
             !      & (ijo2c(jz), jz = jo, jo+ijdim*ij_o(jo)-1, ijdim), &
             !      & (socn (jz), jz = jo, jo+ijdim*ij_o(jo)-1, ijdim)
       endif
    endif
  end subroutine read_rofile_legacy2_core_d

!!!_  - read_rofile_legacy2 - read ROFILE second step
  subroutine read_rofile_legacy2_d &
       & (ierr,   &
       &  ij_ohead, ijrecov_o2c, ijo2c,   socn,    flandg, ruo, rvo, &
       &  ijdim,    ij_omax, u,  swap,    klegacy, ij_o)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: sus_is_stream_unit, sus_read_irec, sus_suspend_read_irec
    implicit none
    integer,        intent(out)          :: ierr
    integer,        intent(out)          :: ij_ohead(0:*)
    integer,        intent(out)          :: ijrecov_o2c(*)
    integer,        intent(out)          :: ijo2c(*)
    real(kind=KTGT),intent(out)          :: socn (*)
    real(kind=KTGT),intent(out),optional :: flandg(*), ruo(*), rvo(*)   ! ijdim
    integer,        intent(in)           :: ij_omax
    integer,        intent(in)           :: ijdim
    integer,        intent(in)           :: u
    logical,        intent(in),optional  :: swap
    integer,        intent(in),optional  :: klegacy
    integer,        intent(out),optional :: ij_o(*)

    integer kfmt

    if (present(ij_o)) then
       call read_rofile_legacy2_core_d &
            & (ierr,   &
            &  ij_ohead, ijrecov_o2c, ijo2c,   socn, &
            &  ij_o,     ijdim,       ij_omax, u,    swap)
    else
       ierr = 0
       if (allocated(buf_ijo)) then
          if (size(buf_ijo).ne.ijdim) ierr = _ERROR(ERR_INVALID_ITEM)
       else
          ierr = _ERROR(ERR_PANIC)
       endif
       if (ierr.eq.0) then
          call read_rofile_legacy2_core_d &
               & (ierr,   &
               &  ij_ohead, ijrecov_o2c, ijo2c,   socn, &
               &  buf_ijo,  ijdim,       ij_omax, u,    swap)
       endif
       if (ierr.eq.0) deallocate(buf_ijo, STAT=ierr)
    endif

    kfmt = choice(legacy_miroc5, klegacy)
    if (kfmt.ne.legacy_miroc4) then
       if (sus_is_stream_unit(u)) then
          if (present(flandg)) then
             if (ierr.eq.0) call sus_read_irec(ierr, u, flandg, ijdim, swap)
          endif
          ! ruo rvo are in the same record
          if (present(ruo).and.present(rvo)) then
             if (ierr.eq.0) call sus_suspend_read_irec(ierr, u, ruo, ijdim, suspend_begin, swap)
             if (ierr.eq.0) call sus_suspend_read_irec(ierr, u, rvo, ijdim, suspend_end,   swap)
          endif
       else
          if (present(flandg)) then
             if (ierr.eq.0) read(u, IOSTAT=ierr) flandg(1:ijdim)
          endif
          if (present(ruo).and.present(rvo)) then
             if (ierr.eq.0) read(u, IOSTAT=ierr) ruo(1:ijdim), rvo(1:ijdim)
          endif
       endif
    endif
  end subroutine read_rofile_legacy2_d

!!!_  - read_rofile_legacy3 - read ROFILE third step (miroc4 only)
  subroutine read_rofile_legacy3_d &
       & (ierr,   &
       &  ij_olhead, ijrecov_o2cl, ijo2cl, slnd,    &
       &  ijdim,     ij_omax,      u,      swap,    klegacy, ij_o)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: sus_is_stream_unit, sus_read_irec, sus_suspend_read_irec
    implicit none
    integer,        intent(out)          :: ierr
    integer,        intent(out)          :: ij_olhead(0:*)
    integer,        intent(out)          :: ijrecov_o2cl(*)
    integer,        intent(out)          :: ijo2cl(*)
    real(kind=KTGT),intent(out)          :: slnd (*)
    integer,        intent(in)           :: ij_omax
    integer,        intent(in)           :: ijdim
    integer,        intent(in)           :: u
    logical,        intent(in),optional  :: swap
    integer,        intent(in),optional  :: klegacy
    integer,        intent(out),optional :: ij_o(*)

    integer kfmt

    kfmt = choice(legacy_miroc5, klegacy)

    if (kfmt.ne.legacy_miroc4) then
       ierr = _ERROR(ERR_INVALID_PARAMETER)
       return
    endif

    if (present(ij_o)) then
       call read_rofile_legacy2_core_d &
            & (ierr,   &
            &  ij_olhead, ijrecov_o2cl, ijo2cl,  slnd, &
            &  ij_o,      ijdim,        ij_omax, u,    swap)
    else
       ierr = 0
       if (allocated(buf_ijo)) then
          if (size(buf_ijo).ne.ijdim) ierr = _ERROR(ERR_INVALID_ITEM)
       else
          ierr = _ERROR(ERR_PANIC)
       endif
       if (ierr.eq.0) then
          call read_rofile_legacy2_core_d &
               & (ierr,   &
               &  ij_olhead, ijrecov_o2cl, ijo2cl,  slnd, &
               &  buf_ijo,   ijdim,        ij_omax, u,    swap)
       endif
       if (ierr.eq.0) deallocate(buf_ijo, STAT=ierr)
    endif
  end subroutine read_rofile_legacy3_d

!!!_  - write_rofile_legacy1 - write ROFILE
  subroutine write_rofile_legacy1_d &
       & (ierr,     &
       &  ij_ohead, ijrecov_o2c, ijo2c, socn, &
       &  ij_omax,  ijdim,       u,     swap)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: sus_read_irec, sus_is_stream_unit
    implicit none
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: ij_ohead(0:*)
    integer,        intent(in)          :: ijrecov_o2c(*)
    integer,        intent(in)          :: ijo2c(*)                    ! ijdim, ij_omax
    real(kind=KTGT),intent(in)          :: socn (*)                    ! ijdim, ij_omax
    integer,        intent(in)          :: ij_omax,  ijdim
    integer,        intent(in)          :: u
    logical,        intent(in),optional :: swap

    logical sw
    integer jo, jz
    integer vi(ij_omax), np(ij_omax)
    real(kind=KTGT) :: vv(ij_omax)
    integer,allocatable :: ij_o(:)

    ierr = 0
    np(1:ij_omax) = 1
    if (ierr.eq.0) allocate(ij_o(ijdim), STAT=ierr)
    if (ierr.eq.0) then
       ij_o(:) = 0
       do jz = 1, ij_omax
          do jo = ij_ohead(jz-1) + 1, ij_ohead(jz)
             ij_o(ijrecov_o2c(jo)) = ij_o(ijrecov_o2c(jo)) + 1
          enddo
       enddo
    endif
    if (sus_is_stream_unit(u)) then
       if (ierr.eq.0) call sus_write_irec(ierr, u, ij_o, ijdim, swap)
       do jo = 1, ijdim
          do jz = 1, ij_o(jo)
             vi(jz) = ijo2c(ij_ohead(jz - 1) + np(jz))
             vv(jz) = socn (ij_ohead(jz - 1) + np(jz))
             np(jz) = np(jz) + 1
          enddo
          if (ierr.eq.0) call sus_suspend_write_irec(ierr, u, vi, ij_o(jo), suspend_begin, swap)
          if (ierr.eq.0) call sus_suspend_write_irec(ierr, u, vv, ij_o(jo), suspend_end,   swap)
       enddo
    else
       sw = choice(.FALSE., swap)
       if (swap) then
          ierr = _ERROR(ERR_INVALID_PARAMETER)
       else
          if (ierr.eq.0) write(u, IOSTAT=ierr) ij_o(1:ijdim)
          do jo = 1, ijdim
             do jz = 1, ij_o(jo)
                vi(jz) = ijo2c(ij_ohead(jz - 1) + np(jz))
                vv(jz) = socn (ij_ohead(jz - 1) + np(jz))
                np(jz) = np(jz) + 1
             enddo
             if (ierr.eq.0) write(u, IOSTAT=ierr) vi(1:ij_o(jo)), vv(1:ij_o(jo))
          enddo
       endif
    endif
    if (ierr.eq.0) deallocate(ij_o, STAT=ierr)
  end subroutine write_rofile_legacy1_d

!!!_  - write_rofile_legacy2 - write ROFILE second step (for miroc5)
  subroutine write_rofile_legacy2_d &
       & (ierr,     &
       &  ruo, rvo, flandg, &
       &  ijdim,    u,      swap, klegacy)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: sus_read_irec, sus_is_stream_unit
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(in)          :: ruo(*), rvo(*), flandg(*)   ! ijdim
    integer,        intent(in)          :: ijdim
    integer,        intent(in)          :: u
    logical,        intent(in),optional :: swap
    integer,        intent(in),optional :: klegacy

    integer kfmt

    ierr = 0
    kfmt = choice(legacy_miroc5, klegacy)

    if (kfmt.eq.legacy_miroc4) then
       ierr = _ERROR(ERR_INVALID_PARAMETER)
       return
    endif

    if (sus_is_stream_unit(u)) then
       if (ierr.eq.0) call sus_write_irec(ierr, u, flandg, ijdim, swap)
       ! ruo rvo are in the same record
       if (ierr.eq.0) call sus_suspend_write_irec(ierr, u, ruo, ijdim, suspend_begin, swap)
       if (ierr.eq.0) call sus_suspend_write_irec(ierr, u, rvo, ijdim, suspend_end,   swap)
    else
       if (ierr.eq.0) write(u, IOSTAT=ierr) flandg(1:ijdim)
       if (ierr.eq.0) write(u, IOSTAT=ierr) ruo(1:ijdim), rvo(1:ijdim)
    endif
  end subroutine write_rofile_legacy2_d

!!!_  - open_geofile_tripolar - open GEO-file (tripolar)
  subroutine open_geofile_tripolar_d &
       & (ierr, &
       &  nx,   ny1,  ny2, &
       &  file, u,         &
       &  nz,   kz,   nic, ibbl, &
       &  slat, plon, plat)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: sus_open
    implicit none
    integer,         intent(out)          :: ierr
    integer,         intent(out)          :: nx, ny1, ny2
    character(len=*),intent(in)           :: file
    integer,         intent(in)           :: u
    integer,         intent(out),optional :: nz,   kz,   nic, ibbl
    real(kind=KTGT), intent(out),optional :: slat, plon, plat

    integer j
    integer di(2)
    integer nzi, kzi, nici, ibbli
    real(kind=KTGT) :: slati, ploni, plati
    real(kind=KTGT) :: dv

    ierr = 0

    nx = 0
    ny1 = 0
    ny2 = 0

    if (ierr.eq.0) then
       call sus_open &
            & (ierr, u, file, &
            &  ACTION='R', STATUS='O', ACCESS='SEQ', FORM='F')
    endif

    if (ierr.eq.0) then
       call read_skip_geofile_tripolar &
            & (ierr,      u,         &
            &  nx,        ny1,       ny2,     &
            &  nz=nz,     kz=kz,     nic=nic, ibbl=ibbl, &
            &  slat=slat, plon=plon, plat=plat)
    endif
  end subroutine open_geofile_tripolar_d

!!!_  - read_geofile_tripolar - read GEO-file (tripolar)
  subroutine read_geofile_tripolar_d &
       & (ierr, &
       &  u,    &
       &  dy,   dz,   hi,   ageo, bgeo)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    integer,         intent(out)          :: ierr
    integer,         intent(in)           :: u
    real(kind=KTGT), intent(out),optional :: dy(*),   dz(*), hi(*)
    real(kind=KTGT), intent(out),optional :: ageo(*), bgeo(*)

    ierr = 0
    if (ierr.eq.0) rewind(u, IOSTAT=ierr)
    if (ierr.eq.0) then
       call read_skip_geofile_tripolar &
            & (ierr,  u,     &
            &  dy=dy, dz=dz, hi=hi, ageo=ageo, bgeo=bgeo)
    endif

  end subroutine read_geofile_tripolar_d
!!!_  - read_skip_geofile_tripolar - read GEO-file (tripolar) core
  subroutine read_skip_geofile_tripolar_d &
       & (ierr, &
       &  u,    &
       &  nx,   ny1,  ny2,  nz,   kz, nic, ibbl, &
       &  slat, plon, plat, &
       &  dy,   dz,   hi,   ageo, bgeo)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    integer,         intent(out)          :: ierr
    integer,         intent(in)           :: u
    integer,         intent(out),optional :: nx,      ny1,   ny2
    integer,         intent(out),optional :: nz,      kz,    nic, ibbl
    real(kind=KTGT), intent(out),optional :: slat,    plon,  plat
    real(kind=KTGT), intent(out),optional :: dy(*),   dz(*), hi(*)
    real(kind=KTGT), intent(out),optional :: ageo(*), bgeo(*)

    integer         :: nxi,   ny1i,  ny2i
    integer         :: nzi,   kzi,   nici, ibbli
    real(kind=KTGT) :: slati, ploni, plati

    integer         :: di(2)
    real(kind=KTGT) :: d

    integer,parameter :: to_read_none = -1
    integer,parameter :: to_read_nx = 0
    integer,parameter :: to_read_ny = 1
    integer,parameter :: to_read_nz = 2
    integer,parameter :: to_read_kz = 3
    integer,parameter :: to_read_ni = 4
    integer,parameter :: to_read_ij = 5 ! dummy
    integer,parameter :: to_read_sl = 6
    integer,parameter :: to_read_dy = 7
    integer,parameter :: to_read_dz = 8
    integer,parameter :: to_read_hi = 9
    integer,parameter :: to_read_pp = 10
    integer,parameter :: to_read_ag = 11
    integer,parameter :: to_read_ib = 12
    integer,parameter :: to_read_bg = 13

    integer rsw
    integer j, ny, nh

    ierr = 0
    if (present(bgeo)) then
       rsw = to_read_bg
    else if (present(ibbl)) then
       rsw = to_read_ib
    else if (present(ageo)) then
       rsw = to_read_ag
    else if (present(plon).or.present(plat)) then
       rsw = to_read_pp
    else if (present(hi)) then
       rsw = to_read_hi
    else if (present(dz)) then
       rsw = to_read_dz
    else if (present(dy)) then
       rsw = to_read_dy
    else if (present(slat)) then
       rsw = to_read_sl
    else if (present(nic)) then
       rsw = to_read_ni
    else if (present(kz)) then
       rsw = to_read_kz
    else if (present(nz)) then
       rsw = to_read_nz
    else if (present(ny1).or.present(ny2)) then
       rsw = to_read_ny
    else if (present(nx)) then
       rsw = to_read_nx
    else
       rsw = to_read_none
    endif

    if (rsw.ge.to_read_nx) then
       if (ierr.eq.0) read(u, *, IOSTAT=ierr)  nxi
    endif
    if (rsw.ge.to_read_ny) then
       if (ierr.eq.0) read(u, *, IOSTAT=ierr)  ny1i, ny2i
       if (ierr.eq.0) then
          ny = ny1i + ny2i
          nh = nxi * ny
       endif
    endif
    if (rsw.ge.to_read_nz) then
       if (ierr.eq.0) read(u, *, IOSTAT=ierr)  nzi
    endif
    if (rsw.ge.to_read_kz) then
       if (ierr.eq.0) read(u, *, IOSTAT=ierr)  kzi
    endif
    if (rsw.ge.to_read_ni) then
       if (ierr.eq.0) read(u, *, IOSTAT=ierr)  nici
    endif
    if (rsw.ge.to_read_ij) then
       if (ierr.eq.0) read(u, *, IOSTAT=ierr)  di(1:2)   ! inodes jnodes
    endif
    if (rsw.ge.to_read_sl) then
       if (ierr.eq.0) read(u, *, IOSTAT=ierr)  slati
    endif
    if (rsw.ge.to_read_dy) then
       if (present(dy)) then
          if (ierr.eq.0) read(u, *, IOSTAT=ierr)  dy(1:ny)
       else
          if (ierr.eq.0) read(u, *, IOSTAT=ierr)  (d, j = 1, ny)
       endif
    endif
    if (rsw.ge.to_read_dz) then
       if (present(dz)) then
          if (ierr.eq.0) read(u, *, IOSTAT=ierr)  dz(1:nzi)
       else
          if (ierr.eq.0) read(u, *, IOSTAT=ierr)  (d, j = 1, nzi)
       endif
    endif
    if (rsw.ge.to_read_hi) then
       if (present(hi)) then
          if (ierr.eq.0) read(u, *, IOSTAT=ierr)  hi(1:nici)
       else
          if (ierr.eq.0) read(u, *, IOSTAT=ierr)  (d, j = 1, nici)
       endif
    endif
    if (rsw.ge.to_read_pp) then
       if (ierr.eq.0) read(u, *, IOSTAT=ierr)  ploni, plati
    endif
    if (rsw.ge.to_read_ag) then
       if (present(ageo)) then
          if (ierr.eq.0) read(u, *, IOSTAT=ierr)  ageo(1:nh)
       else
          if (ierr.eq.0) read(u, *, IOSTAT=ierr)  (d, j = 1, nh)
       endif
    endif
    if (rsw.ge.to_read_ib) then
       if (ierr.eq.0) read(u, *, IOSTAT=ierr)  ibbli
    endif
    if (rsw.ge.to_read_bg) then
       if (present(bgeo)) then
          if (ierr.eq.0) read(u, *, IOSTAT=ierr)  bgeo(1:nh)
       else
          if (ierr.eq.0) read(u, *, IOSTAT=ierr)  (d, j = 1, nh)
       endif
    endif

    if (ierr.eq.0) then
       call set_if_present(nx,   nxi)
       call set_if_present(ny1,  ny1i)
       call set_if_present(ny2,  ny2i)
       call set_if_present(nz,   nzi)
       call set_if_present(kz,   kzi)
       call set_if_present(nic,  nici)
       call set_if_present(ibbl, ibbli)
       call set_if_present(slat, slati)
       call set_if_present(plon, ploni)
       call set_if_present(plat, plati)
    endif
  end subroutine read_skip_geofile_tripolar_d

!!!_ + end Ami_legacy
end module TOUZA_Ami_legacy
!!!_@ test_ami_legacy - test program
#if TEST_AMI_LEGACY
program test_ami_legacy
  use TOUZA_Ami_legacy
  use TOUZA_Std,only: parse, decl_pos_arg, get_option, get_param, arg_diag, arg_init, KDBL
  use TOUZA_Std,only: new_unit
  use TOUZA_Std,only: sus_open, sus_close
  implicit none
  integer,parameter :: KMD = KDBL
  integer ierr
  integer jarg
  integer,parameter :: lpath = 256
  character(len=lpath) :: rafile
  character(len=lpath) :: rofile
  character(len=lpath) :: geofile
  character(len=lpath) :: opfx, opath

  integer u
  logical swap

  integer ij_amax, len_a2m, nxygdm
  integer,       allocatable :: ij_ahead(:), ijrecov_a2m(:), ijc2o(:)
  real(kind=KMD),allocatable :: satm(:), ru(:), rv(:), rocn(:)

  integer ij_omax, len_o2a, ijdim
  integer,       allocatable :: ij_ohead(:), ijrecov_o2c(:), ijo2c(:)
  real(kind=KMD),allocatable :: socn (:), ruo(:), rvo(:), flandg(:)

  integer ij_olmax, len_o2cl
  integer,       allocatable :: ij_olhead(:), ijrecov_o2cl(:), ijo2cl(:)
  real(kind=KMD),allocatable :: slnd(:)

  integer mdomo, mdoma
  integer kraf,  krof

  integer gnx, gny1, gny2, ibbl
  real(kind=KMD) slat, plon, plat

  ierr = 0
  jarg = 0
  rafile = ' '
  rofile = ' '
  opath = ' '

  nxygdm = 0
  ijdim = 0
  ij_omax = 0

  ! [RA=]FILE [RO=]FILE

  call init(ierr)
  if (ierr.eq.0) call arg_init(ierr, levv=-9)

  if (ierr.eq.0) call decl_pos_arg(ierr, 'RA')
  if (ierr.eq.0) call decl_pos_arg(ierr, 'RO')
  if (ierr.eq.0) call decl_pos_arg(ierr, 'OUT')
  if (ierr.eq.0) call parse(ierr)

  if (ierr.eq.0) call get_option(ierr, rafile, 'RA',  ' ')
  if (ierr.eq.0) call get_option(ierr, rofile, 'RO',  ' ')
  if (ierr.eq.0) call get_option(ierr, opfx,   'OUT',  ' ')

  if (ierr.eq.0) call get_option(ierr, geofile, 'GEO',  ' ')

  if (ierr.eq.0) call arg_diag(ierr)

  u = new_unit()
  ierr = min(0, u)

  if (ierr.eq.0) then
     if (rafile.eq.' ') then
        write(*, *) 'need rafile path'
     else
        call open_rafile_legacy &
             & (ierr, kraf, swap, ij_amax, len_a2m, nxygdm, rafile, u)
101     format('open/rafile: ', A, 1x, L1, 1x, '(', I0, ') ', 2(1x, I0), 1x, I0, ' / ', I0)
        mdoma = nxygdm * ij_amax
        write(*, 101) trim(rafile), swap, kraf, ij_amax, nxygdm, len_a2m, mdoma
        if (ierr.eq.0) then
           allocate(ij_ahead(0:ij_amax), &
                &   ijrecov_a2m(len_a2m), ijc2o(len_a2m), satm(len_a2m), &
                &   ru(nxygdm),           rv(nxygdm),     &
                &   STAT=ierr)
        endif
        if (ierr.eq.0) then
           if (kraf.ne.legacy_miroc4) then
              allocate(rocn(nxygdm),  STAT=ierr)
           endif
        endif
        if (ierr.eq.0) then
           call read_rafile_legacy &
                & (ierr,     &
                &  ij_ahead, ijrecov_a2m, ijc2o,  satm, ru, rv, rocn, &
                &  ij_amax,  len_a2m,     nxygdm, u,  swap, kraf)
        endif
     endif
     if (ierr.eq.0) call sus_close(ierr, u, rafile)
  endif
  if (ierr.eq.0) then
     if (rofile.ne.' ') then
        call open_rofile_legacy(ierr, krof, swap, ijdim, rofile, u)
111     format('open/rofile: ', A, 1x, L1, 1x, '(', I0, ') ', I0)
        write(*, 111) trim(rofile), swap, krof, ijdim
        if (ierr.eq.0) then
           call read_rofile_legacy1 &
                & (ierr,     &
                &  ij_omax,  len_o2a, &
                &  ijdim,    u,       swap)
        endif
112     format('open/rofile: ', A, 1x, L1, 1x, '(', I0, ') ', 2(1x, I0), 1x, I0, ' / ', I0)
        mdomo = ijdim * ij_omax
        write(*, 112) trim(rofile), swap, krof, ij_omax, ijdim, len_o2a, mdomo
        if (ierr.eq.0) then
           allocate(ij_ohead(0:ij_omax),  &
                &   ijrecov_o2c(len_o2a), ijo2c(len_o2a), socn(len_o2a), &
                &   STAT=ierr)
        endif
        if (ierr.eq.0) then
           if (krof.ne.legacy_miroc4) then
              allocate(ruo(ijdim),           rvo(ijdim),     flandg(ijdim), &
                   &   STAT=ierr)
           endif
        endif
        if (ierr.eq.0) then
           ijo2c(:) = 0
           socn(:)  = 0.0_KDBL
           call read_rofile_legacy2 &
                & (ierr,     &
                &  ij_ohead, ijrecov_o2c, ijo2c, socn, flandg, ruo, rvo, &
                &  ijdim,    ij_omax,     u,     swap, krof)
        endif
        if (ierr.eq.0) then
           if (krof.eq.legacy_miroc4) then
              call read_rofile_legacy1 &
                   & (ierr,     &
                   &  ij_olmax, len_o2cl, &
                   &  ijdim,    u,       swap)
              if (ierr.eq.0) then
                 allocate(ij_olhead(0:ij_olmax),  &
                      &   ijrecov_o2cl(len_o2cl), ijo2cl(len_o2cl), slnd(len_o2cl), &
                      &   STAT=ierr)
              endif
              if (ierr.eq.0) then
                 ijo2cl(:) = 0
                 slnd(:)  = 0.0_KDBL
                 call read_rofile_legacy3 &
                      & (ierr,      &
                      &  ij_olhead, ijrecov_o2cl, ijo2cl, slnd, &
                      &  ijdim,     ij_omax,      u,      swap, krof)
              endif
           endif
        endif
     endif
     if (ierr.eq.0) call sus_close(ierr, u, rofile)
  endif

  if (ierr.eq.0) then
     if (opfx.ne.' ') then
        opath = trim(opfx) // '-ra.dat'
        call sus_open(ierr, u, opath, ACTION='W', STATUS='R')
        if (ierr.eq.0) then
           call write_rafile_legacy &
                & (ierr,     &
                &  ij_ahead, ijrecov_a2m, ijc2o,  satm, ru, rv, rocn, &
                &  len_a2m,  ij_amax,     nxygdm, u,  swap, kraf)
        endif
        if (ierr.eq.0) opath = trim(opfx) // '-ro.dat'
        if (ierr.eq.0) call sus_open(ierr, u, opath, ACTION='W', STATUS='R')
        if (ierr.eq.0) then
           call write_rofile_legacy1 &
                & (ierr,     &
                &  ij_ohead, ijrecov_o2c, ijo2c, socn, &
                &  ij_omax,  ijdim,       u,     swap)
        endif
        if (ierr.eq.0) then
           if (krof.eq.legacy_miroc4) then
              call write_rofile_legacy1 &
                   & (ierr,     &
                   &  ij_olhead, ijrecov_o2cl, ijo2cl, slnd, &
                   &  ij_omax,   ijdim,        u,      swap)
           else
              call write_rofile_legacy2 &
                   & (ierr,     &
                   &  ruo, rvo, flandg, &
                   &  ijdim,    u,     swap, krof)
           endif
        endif
     endif
  endif

  if (ierr.eq.0) then
     if (geofile.ne.' ') then
        call open_geofile_tripolar &
             & (ierr, gnx, gny1, gny2, geofile, u, &
             &  ibbl=ibbl, slat=slat, plon=plon, plat=plat)
        if (ierr.eq.0) then
           write(*, *) 'geofile:n = ', gnx,  gny1, gny2, ibbl
           write(*, *) 'geofile:p = ', slat, plon, plat
        endif
     endif
  endif

  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call finalize(ierr)
  write(*,*) 'fine = ', ierr
  stop
end program test_ami_legacy
#endif /* TEST_AMI_LEGACY */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

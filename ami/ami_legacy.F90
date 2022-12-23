!!!_! ami_legacy.F90 - TOUZA/Ami/legacy
! Maintainer: SAITO Fuyuki
! Created: Jan 19 2023
#define TIME_STAMP 'Time-stamp: <2023/03/10 15:44:16 fuyuki ami_legacy.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2023
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
!!!_ + static
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = AMI_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
#define __MDL__ 'l'
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
  interface write_rofile_legacy
     module procedure write_rofile_legacy_d
  end interface write_rofile_legacy

!!!_ + public procedures
  public init, diag, finalize
  public open_rafile_legacy,  open_rofile_legacy
  public read_rafile_legacy,  read_rofile_legacy1, read_rofile_legacy2
  public write_rafile_legacy, write_rofile_legacy
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
       fine_counts = fine_counts + 1
    endif
  end subroutine finalize
!!!_ + user procedures
!!!_  - open_rafile_legacy - open RAFILE to read and check basic properties
  subroutine open_rafile_legacy_d &
       & (ierr, &
       &  swap, ij_amax, len_a2m, nxygdm, &
       &  file, u,       mold)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: sus_open, sus_read_irec, sus_skip_irec
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: ij_amax, len_a2m, nxygdm
    logical,         intent(out)         :: swap
    character(len=*),intent(in)          :: file
    integer,         intent(in)          :: u
    real(kind=KTGT), intent(in),optional :: mold

    integer l
    integer,parameter :: tol = 1  ! first record size tolerance
    integer v(1)

    ierr = 0

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
          ierr = ERR_PANIC
          return
       endif
    endif
    if (ierr.eq.0) rewind(u, IOSTAT=ierr)
    if (ierr.eq.0) call sus_read_irec(ierr, u, v, 1, swap)
    if (ierr.eq.0) ij_amax = v(1)
    if (ierr.eq.0) call sus_skip_irec(ierr, u, 1, swap=swap)
    if (ierr.eq.0) call sus_read_irec(ierr, u, v, 1, swap)
    if (ierr.eq.0) len_a2m = v(1)
    if (ierr.eq.0) call sus_skip_irec(ierr, u, 4, swap=swap)
    if (ierr.eq.0) call sus_record_mems_irec(ierr, l, u, mold=real(0,kind=kind(mold)), swap=swap)
    if (ierr.eq.0) nxygdm = l
    if (ierr.eq.0) rewind(u, IOSTAT=ierr)

    if (ierr.ne.0) then
       ierr = ERR_BROKEN_RECORD
    endif

  end subroutine open_rafile_legacy_d
!!!_  - read_rafile_legacy - read RAFILE
  subroutine read_rafile_legacy_d &
       & (ierr,     &
       &  ij_ahead, ijrecov_a2m, ijc2o,  satm, ru, rv, rocn, &
       &  ij_amax,  len_a2m,     nxygdm, u,    swap)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: choice
    use TOUZA_Ami_std,only: sus_is_stream_unit, sus_read_irec, sus_suspend_read_irec
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ij_ahead(0:*)                ! ij_amax
    integer,         intent(out) :: ijrecov_a2m(*), ijc2o(*)     ! len_a2m
    real(kind=KTGT), intent(out) :: satm(*)                      ! len_a2m
    real(kind=KTGT), intent(out) :: ru(*), rv(*), rocn(*)        ! nxygdm
    integer,         intent(in)  :: ij_amax, len_a2m, nxygdm
    integer,         intent(in)  :: u
    logical,optional,intent(in)  :: swap
    integer v(2)
    logical sw

    ierr = 0
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
       if (ierr.eq.0) call sus_read_irec(ierr, u, rocn,        nxygdm,    swap)
    else
       sw = choice(.FALSE., swap)
       if (swap) then
          ierr = ERR_INVALID_PARAMETER
       else
          if (ierr.eq.0) read(u, IOSTAT=ierr) v(1:1)
          if (ierr.eq.0) read(u, IOSTAT=ierr) ij_ahead(0:ij_amax)
          if (ierr.eq.0) read(u, IOSTAT=ierr) v(2:2)
          if (ierr.eq.0) read(u, IOSTAT=ierr) ijrecov_a2m(1:len_a2m)
          if (ierr.eq.0) read(u, IOSTAT=ierr) ijc2o(1:len_a2m)
          if (ierr.eq.0) read(u, IOSTAT=ierr) satm(1:len_a2m)
          if (ierr.eq.0) read(u, IOSTAT=ierr) ru(1:nxygdm), rv(1:nxygdm)
          if (ierr.eq.0) read(u, IOSTAT=ierr) rocn(1:nxygdm)
       endif
    endif
    if (ierr.eq.0) then
       if (v(1).ne.ij_amax .or. v(2).ne.len_a2m) then
          ierr = ERR_BROKEN_RECORD
       endif
    endif
  end subroutine read_rafile_legacy_d
!!!_  - write_rafile_legacy - write RAFILE
  subroutine write_rafile_legacy_d &
       & (ierr,     &
       &  ij_ahead, ijrecov_a2m, ijc2o,  satm, ru, rv, rocn, &
       &  ij_amax,  len_a2m,     nxygdm, u,    swap)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: conv_b2strm, choice
    use TOUZA_Ami_std,only: sus_is_stream_unit, sus_write_irec, sus_suspend_write_irec
    use TOUZA_Ami_std,only: suspend_begin, suspend_end
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: ij_ahead(0:*)                ! ij_amax
    integer,         intent(in)  :: ijrecov_a2m(*), ijc2o(*)     ! len_a2m
    real(kind=KTGT), intent(in)  :: satm(*)                      ! len_a2m
    real(kind=KTGT), intent(in)  :: ru(*), rv(*), rocn(*)        ! nxygdm
    integer,         intent(in)  :: ij_amax, len_a2m, nxygdm
    integer,         intent(in)  :: u
    logical,optional,intent(in)  :: swap
    character(len=16) :: ans
    integer v(2)
    logical sw

    ierr = 0
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
          if (ierr.eq.0) call sus_write_irec(ierr, u, rocn,        nxygdm,    swap)
       case default
          sw = choice(.FALSE., swap)
          if (swap) then
             ierr = ERR_INVALID_PARAMETER
          else
             if (ierr.eq.0) write(u, IOSTAT=ierr) v(1:1)
             if (ierr.eq.0) write(u, IOSTAT=ierr) ij_ahead(ij_amax+1)
             if (ierr.eq.0) write(u, IOSTAT=ierr) v(2:2)
             if (ierr.eq.0) write(u, IOSTAT=ierr) ijrecov_a2m(1:len_a2m)
             if (ierr.eq.0) write(u, IOSTAT=ierr) ijc2o(1:len_a2m)
             if (ierr.eq.0) write(u, IOSTAT=ierr) satm(1:len_a2m)
             if (ierr.eq.0) write(u, IOSTAT=ierr) ru(1:nxygdm), rv(1:nxygdm)
             if (ierr.eq.0) write(u, IOSTAT=ierr) rocn(1:nxygdm)
          endif
       end select
    endif
  end subroutine write_rafile_legacy_d
!!!_  - open_rofile_legacy - open ROFILE to read and check basic properties
  subroutine open_rofile_legacy_d &
       & (ierr, &
       &  swap, ijdim, file, u, mold)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: sus_open, sus_read_irec, sus_skip_irec
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: ijdim
    logical,         intent(inout)       :: swap
    character(len=*),intent(in)          :: file
    integer,         intent(in)          :: u
    real(kind=KTGT), intent(in),optional :: mold

    integer l

    ierr = 0

    ijdim = 0
    if (ierr.eq.0) call sus_open(ierr, u, file, ACTION='R', STATUS='O')
    if (ierr.eq.0) then
       ! check first record
       call sus_record_mems_irec(ierr, l, u, mold=INT(0), swap=swap)
       if (ierr.ne.0) then
          rewind(u, IOSTAT=ierr)
          if (ierr.eq.0) then
             swap = .not.swap
             call sus_record_mems_irec(ierr, l, u, mold=INT(0), swap=swap)
          endif
       endif
       if (ierr.ne.0) then
          ierr = ERR_PANIC
          return
       endif
    endif
    if (ierr.eq.0) ijdim = l
    if (ierr.eq.0) rewind(u, IOSTAT=ierr)
  end subroutine open_rofile_legacy_d
!!!_  - read_rofile_legacy1 - read ROFILE first step
  subroutine read_rofile_legacy1_d &
       & (ierr,     &
       &  ij_omax,  ij_o, &
       &  ijdim,    u,    swap)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: sus_is_stream_unit, sus_read_irec
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ij_omax
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
          ierr = ERR_INVALID_PARAMETER
       else
          if (ierr.eq.0) read(u, IOSTAT=ierr) ij_o(1:ijdim)
       endif
    endif
    if (ierr.eq.0) then
       ij_omax = maxval(ij_o(1:ijdim))
    endif
  end subroutine read_rofile_legacy1_d
!!!_  - read_rofile_legacy2 - read ROFILE second step
  subroutine read_rofile_legacy2_d &
       & (ierr,   &
       &  ijo2c,  socn,  flandg,  ruo, rvo, &
       &  ij_o,   ijdim, ij_omax, u,   swap)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: sus_is_stream_unit, sus_read_irec, sus_suspend_read_irec
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ijo2c(*)
    real(kind=KTGT), intent(out) :: socn (*)
    real(kind=KTGT), intent(out) :: flandg(*), ruo(*), rvo(*)   ! ijdim
    integer,         intent(in)  :: ij_o(*)                     ! ijdim
    integer,         intent(in)  :: ij_omax
    integer,         intent(in)  :: ijdim
    integer,         intent(in)  :: u
    logical,optional,intent(in)  :: swap
    logical sw
    integer jo, jz
    integer vi(ij_omax)
    real(kind=KTGT) :: vv(ij_omax)

    ierr = 0
    if (sus_is_stream_unit(u)) then
       do jo = 1, ijdim
          if (ierr.eq.0) call sus_suspend_read_irec(ierr, u, vi, ij_o(jo), suspend_begin, swap)
          if (ierr.eq.0) call sus_suspend_read_irec(ierr, u, vv, ij_o(jo), suspend_end,   swap)
          if (ierr.eq.0) then
             ijo2c(jo:jo+ijdim*ij_o(jo)-1:ijdim) = vi(1:ij_o(jo))
             socn (jo:jo+ijdim*ij_o(jo)-1:ijdim) = vv(1:ij_o(jo))
          endif
       enddo
       if (ierr.eq.0) call sus_read_irec(ierr, u, flandg, ijdim, swap)
       ! ruo rvo are in the same record
       if (ierr.eq.0) call sus_suspend_read_irec(ierr, u, ruo, ijdim, suspend_begin, swap)
       if (ierr.eq.0) call sus_suspend_read_irec(ierr, u, rvo, ijdim, suspend_end,   swap)
    else
       sw = choice(.FALSE., swap)
       if (swap) then
          ierr = ERR_INVALID_PARAMETER
       else
          do jo = 1, ijdim
             if (ierr.eq.0) read(u, IOSTAT=ierr) &
                  & (ijo2c(jz), jz = jo, jo+ijdim*ij_o(jo)-1, ijdim), &
                  & (socn (jz), jz = jo, jo+ijdim*ij_o(jo)-1, ijdim)
          enddo
       endif
       if (ierr.eq.0) read(u, IOSTAT=ierr) flandg(1:ijdim)
       if (ierr.eq.0) read(u, IOSTAT=ierr) ruo(1:ijdim), rvo(1:ijdim)
    endif
  end subroutine read_rofile_legacy2_d
!!!_  - write_rofile_legacy - write ROFILE
  subroutine write_rofile_legacy_d &
       & (ierr,     &
       &  ij_o,     ijo2c, socn, flandg, ruo, rvo, &
       &  ij_omax,  ijdim, u,    swap)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: sus_read_irec, sus_is_stream_unit
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: ij_o(*)                     ! ijdim
    integer,         intent(in)  :: ijo2c(*)                    ! ijdim, ij_omax
    real(kind=KTGT), intent(in)  :: socn (*)                    ! ijdim, ij_omax
    real(kind=KTGT), intent(in)  :: flandg(*), ruo(*), rvo(*)   ! ijdim
    integer,         intent(in)  :: ij_omax,  ijdim
    integer,         intent(in)  :: u
    logical,optional,intent(in)  :: swap

    logical sw
    integer jo, jz
    integer vi(ij_omax)
    real(kind=KTGT) :: vv(ij_omax)

    ierr = 0
    if (sus_is_stream_unit(u)) then
       if (ierr.eq.0) call sus_write_irec(ierr, u, ij_o, ijdim, swap)
       do jo = 1, ijdim
          vi(1:ij_o(jo)) = ijo2c(jo:jo+ijdim*ij_o(jo)-1:ijdim)
          vv(1:ij_o(jo)) = socn (jo:jo+ijdim*ij_o(jo)-1:ijdim)
          if (ierr.eq.0) call sus_suspend_write_irec(ierr, u, vi, ij_o(jo), suspend_begin, swap)
          if (ierr.eq.0) call sus_suspend_write_irec(ierr, u, vv, ij_o(jo), suspend_end,   swap)
       enddo
       if (ierr.eq.0) call sus_write_irec(ierr, u, flandg, ijdim, swap)
       ! ruo rvo are in the same record
       if (ierr.eq.0) call sus_suspend_write_irec(ierr, u, ruo, ijdim, suspend_begin, swap)
       if (ierr.eq.0) call sus_suspend_write_irec(ierr, u, rvo, ijdim, suspend_end,   swap)
    else
       sw = choice(.FALSE., swap)
       if (swap) then
          ierr = ERR_INVALID_PARAMETER
       else
          if (ierr.eq.0) write(u, IOSTAT=ierr) ij_o(1:ijdim)
          do jo = 1, ijdim
             if (ierr.eq.0) write(u, IOSTAT=ierr) &
                  & (ijo2c(jz), jz = jo, jo+ijdim*ij_o(jo)-1, ijdim), &
                  & (socn (jz), jz = jo, jo+ijdim*ij_o(jo)-1, ijdim)
          enddo
       endif
       if (ierr.eq.0) write(u, IOSTAT=ierr) flandg(1:ijdim)
       if (ierr.eq.0) write(u, IOSTAT=ierr) ruo(1:ijdim), rvo(1:ijdim)
    endif
  end subroutine write_rofile_legacy_d

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
  character(len=lpath) :: opfx, opath

  integer u
  logical swap

  integer ij_amax, len_a2m, nxygdm
  integer,       allocatable :: ij_ahead(:), ijrecov_a2m(:), ijc2o(:)
  real(kind=KMD),allocatable :: satm(:), ru(:), rv(:), rocn(:)

  integer ijdim
  integer ij_omax
  integer,       allocatable :: ij_o (:)
  integer,       allocatable :: ijo2c(:)
  real(kind=KMD),allocatable :: socn (:)
  real(kind=KMD),allocatable :: ruo(:), rvo(:), flandg(:)

  integer mdomo, mdoma

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

  if (ierr.eq.0) call arg_diag(ierr)

  u = new_unit()
  ierr = min(0, u)

  if (ierr.eq.0) then
     if (rafile.eq.' ') then
        write(*, *) 'need rafile path'
     else
        call open_rafile_legacy(ierr, swap, ij_amax, len_a2m, nxygdm, rafile, u)
101     format('open/rafile: ', A, 1x, L1, 2(1x, I0), 1x, I0, ' / ', I0)
        mdoma = nxygdm * ij_amax
        write(*, 101) trim(rafile), swap, ij_amax, nxygdm, len_a2m, mdoma
        if (ierr.eq.0) then
           allocate(ij_ahead(0:ij_amax), &
                &   ijrecov_a2m(len_a2m), ijc2o(len_a2m), satm(len_a2m), &
                &   ru(nxygdm),           rv(nxygdm),     rocn(nxygdm),  &
                &   STAT=ierr)
        endif
        if (ierr.eq.0) then
           call read_rafile_legacy &
                & (ierr, &
                &  ij_ahead, ijrecov_a2m, ijc2o,  satm, ru, rv, rocn, &
                &  ij_amax,  len_a2m,     nxygdm, u,    swap)
        endif
     endif
     if (ierr.eq.0) call sus_close(ierr, u, rafile)
  endif
  if (ierr.eq.0) then
     if (rofile.ne.' ') then
        call open_rofile_legacy(ierr, swap, ijdim, rofile, u)
111     format('open/rofile: ', A, 1x, L1, 1x, I0)
        write(*, 111) trim(rofile), swap, ijdim
        if (ierr.eq.0) then
           allocate(ij_o(1:ijdim), STAT=ierr)
        endif
        if (ierr.eq.0) then
           ij_o(:) = 0
           call read_rofile_legacy1 &
                & (ierr,     &
                &  ij_omax,  ij_o, &
                &  ijdim,    u,    swap)
        endif
112     format('open/rofile: ', A, 1x, L1, 2(1x, I0), 1x, I0, ' / ', I0)
        mdomo = ijdim * ij_omax
        write(*, 112) trim(rofile), swap, ij_omax, ijdim, sum(ij_o(1:ijdim)), mdomo
        if (ierr.eq.0) then
           allocate(ijo2c(mdomo), socn(mdomo), &
                &   ruo(ijdim),   rvo(ijdim),  flandg(ijdim),  &
                &   STAT=ierr)
        endif
        if (ierr.eq.0) then
           ijo2c(:) = 0
           socn(:)  = 0.0_KDBL
           call read_rofile_legacy2 &
                & (ierr,     &
                &  ijo2c,  socn,  flandg,  ruo, rvo, &
                &  ij_o,   ijdim, ij_omax, u,   swap)
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
                &  ij_amax,  len_a2m,     nxygdm, u,    swap)
        endif
        if (ierr.eq.0) opath = trim(opfx) // '-ro.dat'
        if (ierr.eq.0) call sus_open(ierr, u, opath, ACTION='W', STATUS='R')
        if (ierr.eq.0) then
           call write_rofile_legacy &
                & (ierr,     &
                &  ij_o,     ijo2c, socn, flandg, ruo, rvo, &
                &  ij_omax,  ijdim, u,    swap)
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

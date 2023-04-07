!!!_! ami_nio.F90 - TOUZA/Ami/nio-format interfaces
! Maintainer: SAITO Fuyuki
! Created: Jan 19 2023
#define TIME_STAMP 'Time-stamp: <2023/04/05 16:35:25 fuyuki ami_nio.F90>'
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
#ifndef   TEST_AMI_NIO
#  define TEST_AMI_NIO 0
#endif
#ifndef   OPT_AMI_NIO_FORMAT_DEFAULT
#  define OPT_AMI_NIO_FORMAT_DEFAULT REC_BIG     /* default byte-order of nio format */
#endif
!!!_@ TOUZA_Ami_nio - ami-da procedures for nio-format
module TOUZA_Ami_nio
!!!_ + modules
  use TOUZA_Ami_std, as_init=>init, as_diag=>diag, as_finalize=>finalize
  use TOUZA_Nio,only: REC_ASIS, REC_DEFAULT, REC_SWAP, REC_BIG, REC_LITTLE
  use TOUZA_Nio,only: hi_MEMO1, hi_MEMO2
!!!_ + default
  implicit none
  private
!!!_ + parameter
!!!_  - tolerance level
  integer,parameter,public :: tol_strict     = 0     ! require strict matching
  integer,parameter,public :: tol_skip_src   = 1     ! skip source matching
  integer,parameter,public :: tol_skip_cname = 2     ! skip coordinate name matching (size check only)
!!!_  - special header properties
  integer,parameter,public :: xhi_AITEM = hi_MEMO1
  integer,parameter,public :: xhi_ASIZE = hi_MEMO2
  integer,parameter,public :: xhi_step = 2
  integer,parameter,public :: xhi_lim  = 3
!!!_ + public
  integer,parameter :: vmiss = -999
  integer,parameter,public :: fmt_packed = 0
  integer,parameter,public :: fmt_expand = 1
!!!_  - item (variable) key
  character(len=*),parameter,public :: item_c2o    = 'IJC2O'
  character(len=*),parameter,public :: item_a2m    = 'IJRECOV_A2M'
  character(len=*),parameter,public :: item_satm   = 'SATM'
  character(len=*),parameter,public :: item_ru     = 'RU'
  character(len=*),parameter,public :: item_rv     = 'RV'
  character(len=*),parameter,public :: item_rocn   = 'ROCN'
  character(len=*),parameter,public :: item_o2c    = 'IJO2C'
  character(len=*),parameter,public :: item_socn   = 'SOCN'
  character(len=*),parameter,public :: item_flandg = 'FLANDG'
  character(len=*),parameter,public :: item_ruo    = 'RUO'
  character(len=*),parameter,public :: item_rvo    = 'RVO'
!!!_ + static
!!!_  - map file cache
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = AMI_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
#define __MDL__ 'n'
#define _ERROR(E) (E - ERR_MASK_AMI_NIO)
!!!_ + interface
  interface normalize_radata
     module procedure normalize_radata_d
  end interface normalize_radata
  interface normalize_rodata
     module procedure normalize_rodata_d
  end interface normalize_rodata
  interface check_remove_wings
     module procedure check_remove_wings_d, check_remove_wings_i
  end interface check_remove_wings
  interface write_radata_nio
     module procedure write_radata_nio_d
  end interface write_radata_nio
  interface write_rodata_nio
     module procedure write_rodata_nio_d
  end interface write_rodata_nio
  interface unpack_array
     module procedure unpack_array_d, unpack_array_i
  end interface unpack_array
  interface pack_array
     module procedure pack_array_d, pack_array_i
  end interface pack_array
!!!_ + public procedures
  public init, diag, finalize
  public normalize_radata, set_raheader, write_radata_nio
  public normalize_rodata, set_roheader, write_rodata_nio
  public set_rheader_extra
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
  subroutine open_mapfile &
       & (ierr,     &
       &  lmap,     liter,       &
       &  file,     u,           &
       &  cdest,    ndest,       csrc, nsrc, &
       &  item_map, item_offset, mode, tol)
    use TOUZA_Ami_std,only: KTIME=>KDBL
    use TOUZA_Nio,only: nio_check_magic_file
    use TOUZA_Nio,only: nio_time_undef
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: lmap          ! mapping array size (defined elements)
    integer,         intent(out) :: liter         ! mapping iteration size
    character(len=*),intent(in)  :: file
    integer,         intent(in)  :: u
    character(len=*),intent(in)  :: cdest(:)      ! destination coordinate names
    character(len=*),intent(in)  :: csrc(:)       ! source coordinate names
    integer,         intent(in)  :: ndest(:)      ! destination coordinate sizes
    integer,         intent(in)  :: nsrc(:)       ! source coordinate sizes
    character(len=*),intent(in)  :: item_map      ! name of map item
    character(len=*),intent(in)  :: item_offset   ! name of offset item
    integer,optional,intent(in)  :: mode          ! file access mode
    integer,optional,intent(in)  :: tol           ! tolerance level
    real(kind=KTIME) :: timel, timeh

    integer ltol

    ierr = 0
    timel = nio_time_undef(timel)
    timeh = nio_time_undef(timeh)

    call sus_open(ierr, u, file, ACTION='R', STATUS='O')
    if (ierr.eq.0) ierr = nio_check_magic_file(u)
    if (ierr.eq.0) then
       ! search item_map
    endif

  end subroutine open_mapfile
!!!_  - open_rfile_nio
  subroutine open_rfile_nio_d &
       & (ierr, &
       &  ij_amax, len_a2m, mx, my, xco, yco, &
       &  file,    u,       mold)
    ! return error if failed (including non-nio format case)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: sus_open, sus_read_irec, sus_skip_irec
    use TOUZA_Nio,only: nio_check_magic_file
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: ij_amax, len_a2m
    integer,         intent(out)         :: mx,      my
    character(len=*),intent(out)         :: xco,     yco
    character(len=*),intent(in)          :: file
    integer,         intent(in)          :: u
    real(kind=KTGT), intent(in),optional :: mold

    ierr = 0
    ij_amax = -1
    len_a2m = -1
    mx      = -1
    my      = -1
    xco     = ' '
    yco     = ' '
    call sus_open(ierr, u, file, ACTION='R', STATUS='O')
    if (ierr.eq.0) ierr = nio_check_magic_file(u)
    if (ierr.eq.0) then
    endif
  end subroutine open_rfile_nio_d

!!!_  - normalize_radata
  subroutine normalize_radata_d &
       & (ierr,        &
       &  ijrecov_a2m, ijc2o,   satm, ru,  rv, rocn, &
       &  ij_ahead,    ij_amax, &
       &  nxygdm,      mxo,     wxo,  myo, wyo, &
       &  mxc,         pxc,     myc,  offset)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: ijrecov_a2m(*), ijc2o(*)     ! len_a2m
    real(kind=KTGT), intent(inout) :: satm(*)                      ! len_a2m
    real(kind=KTGT), intent(inout) :: ru(*), rv(*), rocn(*)        ! nxygdm
    integer,         intent(inout) :: ij_ahead(0:*)                ! ij_amax
    integer,         intent(in)    :: ij_amax
    integer,         intent(in)    :: nxygdm, mxo, wxo, myo, wyo
    integer,         intent(in)    :: mxc,    pxc, myc
    integer,         intent(in)    :: offset

    integer lpack

    ierr = 0

    lpack = ij_ahead(ij_amax) - ij_ahead(0)

    if (mxo * myo.gt.0) then
       if (ierr.eq.0) call check_remove_wings(ierr, ru,   nxygdm, 1, mxo, wxo, wxo, myo, wyo, 'ru')
       if (ierr.eq.0) call check_remove_wings(ierr, rv,   nxygdm, 1, mxo, wxo, wxo, myo, wyo, 'rv')
       if (ierr.eq.0) call check_remove_wings(ierr, rocn, nxygdm, 1, mxo, wxo, wxo, myo, wyo, 'rocn')

       if (ierr.eq.0) then
          call domain_normalize &
               & (ierr, ijrecov_a2m,   lpack, &
               &  'ijrecov_a2m', mxo,  myo,   wxo, wxo, wyo, offset)
       endif
    endif

    if (mxc * myc.gt.0) then
       if (ierr.eq.0) then
          call domain_normalize &
               & (ierr, ijc2o,  lpack, &
               &  'ijc2o', mxc, myc,   0, pxc, 0, offset)
       endif
    endif

    if (ierr.eq.0) then
       ij_ahead(1:ij_amax) = ij_ahead(1:ij_amax) - ij_ahead(0)
       ij_ahead(0) = 0
    endif

  end subroutine normalize_radata_d

!!!_  - normalize_rodata
  subroutine normalize_rodata_d &
       & (ierr,        &
       &  ijrecov_o2c, ijo2c,   socn, ruo, rvo, flandg, &
       &  ij_ohead,    ij_omax, &
       &  mxo,         wxo,     myo,  wyo, &
       &  ijdim,       mxc,     pxc,  myc, offset)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: ijrecov_o2c(*), ijo2c(*)    ! len_o2c
    real(kind=KTGT), intent(inout) :: socn (*)
    real(kind=KTGT), intent(inout) :: ruo(*), rvo(*), flandg(*)   ! ijdim
    integer,         intent(inout) :: ij_ohead(0:*)               ! ij_omax
    integer,         intent(in)    :: ij_omax
    integer,         intent(in)    :: mxo,   wxo, myo, wyo
    integer,         intent(in)    :: ijdim, mxc, pxc, myc
    integer,         intent(in)    :: offset

    integer lpack
    real(kind=KTGT) :: ZERO = 0.0_KTGT


    ierr = 0
    lpack = ij_ohead(ij_omax) - ij_ohead(0)

    if (ierr.eq.0) call check_remove_wings(ierr, flandg, ijdim, 1, mxc, 0, pxc, myc, 0, 'flandg')
    if (ierr.eq.0) call check_remove_wings(ierr, ruo,    ijdim, 1, mxc, 0, pxc, myc, 0, 'ruo')
    if (ierr.eq.0) call check_remove_wings(ierr, rvo,    ijdim, 1, mxc, 0, pxc, myc, 0, 'rvo')

    if (ierr.eq.0) then
       call domain_normalize &
            & (ierr, ijrecov_o2c, lpack, &
            &  'ijrecov_o2c', mxc,  myc, 0, pxc, 0, offset)
    endif
    if (ierr.eq.0) then
       call domain_normalize &
            & (ierr, ijo2c, lpack, &
            &  'ijo2c', mxo, myo, wxo, wxo, wyo, offset)
    endif

    if (ierr.eq.0) then
       ij_ohead(1:ij_omax) = ij_ohead(1:ij_omax) - ij_ohead(0)
       ij_ohead(0) = 0
    endif

    ! if (ierr.eq.0) call check_remove_wings(ierr, socn,   ijdim, ij_omax, mxc, 0, pxc, myc, 0, 'socn')
    ! if (ierr.eq.0) call check_remove_wings(ierr, ijo2c,  ijdim, ij_omax, mxc, 0, pxc, myc, 0, 'ijo2c')
    ! if (ierr.eq.0) call check_remove_wings(ierr, ij_o,   ijdim, 1,       mxc, 0, pxc, myc, 0, 'ij_o')
!     if (ierr.eq.0) then
!        nh = mxc * myc
!        do jh = 1, nh
!           do jz = 0, ij_o(jh) - 1
!              jp = nh * jz + jh
!              if (is_dummy(ijo2c(jp), mxo, myo, wxo, wxo, wyo, 1)) then
!                 ierr = -1
! 101             format('Invalid dummy data detected: ', I0, 1x, I0, 1x, I0)
!                 write(txt, 101) jp, ijo2c(jp), plane_ij(ijo2c(jp), mxo, wxo, wxo, 1)
!                 call msg(txt, __MDL__, utmp)
!              else
!                 ijo2c(jp) = plane_logical(ijo2c(jp), mxo, wxo, wxo, wyo, 1)
!              endif
!           enddo
!           do jz = ij_o(jh), ij_omax - 1
!              jp = nh * jz + jh
!              if (socn(jp).ne.ZERO) then
!                 call msg('(''strange data in SOCN: '', I0, 1x, I0)', (/jh, jz/), &
!                      &   __MDL__, utmp)
!              else
!                 socn(jp) = real(vmiss, kind=KTGT)
!              endif
!              if (ijo2c(jp).ne.0) then
!                 call msg('(''strange data in IJO2C: '', I0, 1x, I0)', (/jh, jz/), &
!                      &   __MDL__, utmp)
!              else
!                 ijo2c(jp) = vmiss
!              endif
!           enddo
!        enddo
!     endif

  end subroutine normalize_rodata_d
!!!_  - domain_normalize
  subroutine domain_normalize(ierr, v, n, tag, mx, my, ww, we, ws, ofs, u)
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: v(*)
    integer,         intent(in)    :: n
    character(len=*),intent(in)    :: tag
    integer,         intent(in)    :: mx, my
    integer,         intent(in)    :: ww, we, ws
    integer,         intent(in)    :: ofs
    integer,optional,intent(in)    :: u

    integer j
    integer utmp
    character(len=256) :: txt

    ierr = 0
    utmp = get_logu(u, ulog)
101 format('dummy:', A, ': ', I0, 1x, '[', I0, 1x, I0, ']')
    do j = 1, n
       if (is_dummy(v(j), mx, my, ww, we, ws, ofs)) then
          ierr = -1
          write(txt, 101) trim(tag), j, v(j), plane_ij(v(j), mx, ww, we, ofs)
          call msg(txt, __MDL__, utmp)
       else
          v(j) = plane_logical(v(j), mx, ww, we, ws, ofs)
       endif
    enddo
  end subroutine domain_normalize
!!!_  - is_dummy()
  logical function is_dummy(jh, mx, my, ww, we, ws, ofs) result(b)
    implicit none
    integer,intent(in) :: jh
    integer,intent(in) :: mx, my
    integer,intent(in) :: ww, we, ws
    integer,intent(in) :: ofs
    integer jx, jy
    jy =     (jh - ofs) / (mx + ww + we)
    jx = mod((jh - ofs),  (mx + ww + we))
    b = .not. &
         & ((ww.le.jx .and. jx.lt.mx+ww) .and. (ws.le.jy .and. jy.lt.my+ws))
  end function is_dummy
!!!_  - plane_ij()
  function plane_ij(jh, mx, ww, we, ofs) result(h)
    implicit none
    integer :: h(2)
    integer,intent(in) :: jh
    integer,intent(in) :: mx
    integer,intent(in) :: ww, we
    integer,intent(in) :: ofs
    h(2) =     (jh - ofs) / (mx + ww + we)   + ofs
    h(1) = mod((jh - ofs),  (mx + ww + we))  + ofs
  end function plane_ij

!!!_  - plane_logical()
  integer function plane_logical(jh, mx, ww, we, ws, ofs) result(n)
    implicit none
    integer,intent(in) :: jh
    integer,intent(in) :: mx
    integer,intent(in) :: ww, we, ws
    integer,intent(in) :: ofs
    integer jx, jy
    jy =     (jh - ofs) / (mx + ww + we)  - ww
    jx = mod((jh - ofs),  (mx + ww + we)) - ws
    ! n = jy * mx + jx + ofs
    n = jy * mx + jx
  end function plane_logical

!!!_  - check_remove_wings
  subroutine check_remove_wings_d(ierr, v, lh, nz, mx, ww, we, my, wy, tag, u)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    integer,         intent(out)   :: ierr
    real(kind=KTGT), intent(inout) :: v(*)   ! (lh,nz) to (mx,my,nz)
    integer,         intent(in)    :: nz
    integer,         intent(in)    :: lh, mx, ww, we, my, wy
    character(len=*),intent(in)    :: tag
    integer,optional,intent(in)    :: u
    integer jz
    integer lx, ly
    integer jx, jy, jh, jhl, jhh, jyt
    integer jsrc, jdst
    integer nsize
    real(kind=KTGT),parameter :: ZERO = real(0, KTGT)
    character(len=256) :: txt
    integer utmp

    ierr = 0
    utmp = get_logu(u, ulog)

    lx = mx + ww + we
    ly = my + 2 * wy

101 format('health check/', A, ':', E16.9, 1x, I0, 1x, I0)

    do jz = 0, nz - 1
       jhl = 1 + lh * jz
       jhh = wy * lx + ww
       if (ANY(v(jhl:jhh).ne.ZERO)) then
          do jh = jhl, jhh
             if (v(jh).ne.ZERO) then
                jx = mod(jh - 1, lx) + 1
                jy =(jh - 1) / ly + 1
                write(txt, 101) trim(tag), v(jh), jx, jy
                call msg(txt, __MDL__, utmp)
             endif
          enddo
          ierr = -1
       endif
       jhl = lh - (wy * lx + we) + 1 + lh * jz
       jhh = lh
       if (ANY(v(jhl:jhh).ne.ZERO)) then
          do jh = jhl, jhh
             if (v(jh).ne.ZERO) then
                jx = mod(jh - 1, lx) + 1
                jy =(jh - 1) / ly + 1
                write(txt, 101) trim(tag), v(jh), jx, jy
                call msg(txt, __MDL__, utmp)
             endif
          enddo
          ierr = -1
       endif
       do jyt = wy + 1, ly - wy - 1
          jhl = (jyt - 1) * lx + lx - we + 1 + lh * jz
          jhh = jhl + ww + we - 1
          if (ANY(v(jhl:jhh).ne.ZERO)) then
             do jh = jhl, jhh
                if (v(jh).ne.ZERO) then
                   jx = mod(jh - 1, lx) + 1
                   jy = (jh - 1) / ly + 1
                   write(txt, 101) trim(tag), v(jh), jx, jy
                   call msg(txt, __MDL__, utmp)
                endif
             enddo
             ierr = -1
          endif
       enddo
    enddo
    if (ierr.eq.0) then
       nsize = mx * my
       if (nsize.ne.lh) then
          do jz = 0, nz - 1
             do jy = 1, my
                jsrc = (jy + wy - 1) * lx + ww + lh * jz
                jdst = (jy - 1) * mx           + nsize * jz
                v(jdst+1:jdst+mx) = v(jsrc+1:jsrc+mx)
             enddo
          enddo
       endif
    endif
  end subroutine check_remove_wings_d

  subroutine check_remove_wings_i(ierr, v, lh, nz, mx, ww, we, my, wy, tag, u)
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: v(*)
    integer,         intent(in)    :: nz
    integer,         intent(in)    :: lh, mx, ww, we, my, wy
    character(len=*),intent(in)    :: tag
    integer,optional,intent(in)    :: u
    integer jz
    integer lx, ly
    integer jx, jy, jh, jhl, jhh, jyt
    integer jsrc, jdst
    integer nsize
    integer,parameter :: ZERO = 0
    integer utmp
    character(len=256) :: txt

    ierr = 0
    utmp = get_logu(u, ulog)

    lx = mx + ww + we
    ly = my + 2 * wy

101 format('health check/', A, ':', E16.9, 1x, I0, 1x, I0)

    do jz = 0, nz - 1
       jhl = 1 + lh * jz
       jhh = wy * lx + ww
       if (ANY(v(jhl:jhh).ne.ZERO)) then
          do jh = jhl, jhh
             if (v(jh).ne.ZERO) then
                jx = mod(jh - 1, lx) + 1
                jy =(jh - 1) / ly + 1
                write(txt, 101) trim(tag), v(jh), jx, jy
                call msg(txt, __MDL__, utmp)
             endif
          enddo
          ierr = -1
       endif
       jhl = lh - (wy * lx + we) + 1 + lh * jz
       jhh = lh
       if (ANY(v(jhl:jhh).ne.ZERO)) then
          do jh = jhl, jhh
             if (v(jh).ne.ZERO) then
                jx = mod(jh - 1, lx) + 1
                jy =(jh - 1) / ly + 1
                write(txt, 101) trim(tag), v(jh), jx, jy
                call msg(txt, __MDL__, utmp)
             endif
          enddo
          ierr = -1
       endif
       do jyt = wy + 1, ly - wy - 1
          jhl = (jyt - 1) * lx + lx - we + 1 + lh * jz
          jhh = jhl + ww + we - 1
          if (ANY(v(jhl:jhh).ne.ZERO)) then
             do jh = jhl, jhh
                if (v(jh).ne.ZERO) then
                   jx = mod(jh - 1, lx) + 1
                   jy = (jh - 1) / ly + 1
                   write(txt, 101) trim(tag), v(jh), jx, jy
                   call msg(txt, __MDL__, utmp)
                endif
             enddo
             ierr = -1
          endif
       enddo
    enddo
    if (ierr.eq.0) then
       nsize = mx * my
       if (nsize.ne.lh) then
          do jz = 0, nz - 1
             do jy = 1, my
                jsrc = (jy + wy - 1) * lx + ww + lh * jz
                jdst = (jy - 1) * mx           + nsize * jz
                v(jdst+1:jdst+mx) = v(jsrc+1:jsrc+mx)
             enddo
          enddo
       endif
    endif
  end subroutine check_remove_wings_i
!!!_  - set_raheader
  subroutine set_raheader(ierr, hd, axocn, ayocn, nxygdm, mxo, myo)
    use TOUZA_Nio,only: get_default_header, put_header_cprop
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: hd(*)
    character(len=*),intent(in)  :: axocn,  ayocn
    integer,         intent(in)  :: nxygdm, mxo, myo
    ierr = 0
    call get_default_header(hd)
    if (mxo * myo.eq.0) then
       if (ierr.eq.0) call put_header_cprop(ierr, hd, 'ocplane', (/1, nxygdm/), 1)
       if (ierr.eq.0) call put_header_cprop(ierr, hd, ' ',       (/1, 1/), 2)
    else
       if (ierr.eq.0) call put_header_cprop(ierr, hd, axocn, (/1, mxo/), 1)
       if (ierr.eq.0) call put_header_cprop(ierr, hd, ayocn, (/1, myo/), 2)
    endif

  end subroutine set_raheader

!!!_  - set_roheader
  subroutine set_roheader(ierr, hd, axatm, ayatm, ijdim, mxa, mya)
    use TOUZA_Nio,only: get_default_header, put_header_cprop
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: hd(*)
    character(len=*),intent(in)  :: axatm, ayatm
    integer,         intent(in)  :: ijdim, mxa, mya
    ierr = 0
    call get_default_header(hd)
    if (mxa * mya.eq.0) then
       if (ierr.eq.0) call put_header_cprop(ierr, hd, 'aplane', (/1, ijdim/), 1)
       if (ierr.eq.0) call put_header_cprop(ierr, hd, ' ',      (/1, 1/), 2)
    else
       if (ierr.eq.0) call put_header_cprop(ierr, hd, axatm, (/1, mxa/), 1)
       if (ierr.eq.0) call put_header_cprop(ierr, hd, ayatm, (/1, mya/), 2)
    endif
  end subroutine set_roheader

!!!_  - set_rheader_extra
  subroutine set_rheader_extra(ierr, hd, cidx, name, mem)
    use TOUZA_Nio,only: put_item, store_item
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: hd(*)
    integer,         intent(in)  :: cidx
    character(len=*),intent(in)  :: name
    integer,         intent(in)  :: mem
    integer jhi_name, jhi_size
    ierr = 0
    if (cidx.lt.1.or.cidx.gt.xhi_lim) ierr = _ERROR(ERR_INVALID_PARAMETER)
    if (ierr.eq.0) then
       jhi_name = xhi_AITEM + (cidx - 1) * xhi_step
       jhi_size = xhi_ASIZE + (cidx - 1) * xhi_step
    endif
    if (ierr.eq.0) call put_item(ierr, hd, name, jhi_name)
    if (ierr.eq.0) call store_item(ierr, hd, mem,  jhi_size)
  end subroutine set_rheader_extra

!!!_  - write_project_data
  subroutine write_project_data_d &
       & (ierr, krect, &
       &  hd,   u,     fmt,   &
       &  imap, iitem, rmap,  ritem, citer, subv, sitem, ends)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(inout)       :: krect
    character(len=*),intent(inout)       :: hd(*)
    integer,         intent(in)          :: u
    character(len=*),intent(in)          :: fmt        ! [UMP]R[48]
    integer,         intent(in)          :: imap(0:*)  ! integer mapping array
    real(kind=KTGT), intent(in)          :: rmap(0:*)  ! real mapping array
    character(len=*),intent(in)          :: iitem
    character(len=*),intent(in)          :: ritem
    integer,         intent(in),optional :: citer      ! iteration coordinate index (default: 3)
    integer,         intent(in),optional :: subv(0:*)  ! subscript vetcor
    integer,         intent(in),optional :: ends(0:*)  ! iteration ends
    character(len=*),intent(in),optional :: sitem
    ierr = 0
  end subroutine write_project_data_d

!!!_  - write_radata_nio
  subroutine write_radata_nio_d &
       & (ierr, krect, &
       &  hd,   u,     fmt,   &
       &  ijrecov_a2m, ijc2o, satm, ru, rv, rocn, ij_ahead, ij_amax)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Nio,only: hi_DFMT, hi_ITEM, litem
    use TOUZA_Nio,only: nio_write_header, nio_write_data, put_item
    use TOUZA_Nio,only: put_header_cprop, get_header_cprop
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: krect
    character(len=*),intent(inout) :: hd(*)
    integer,         intent(in)    :: fmt
    integer,         intent(in)    :: ij_ahead(0:*)                ! ij_amax
    integer,         intent(in)    :: ijrecov_a2m(*), ijc2o(*)     ! len_a2m
    real(kind=KTGT), intent(in)    :: satm(*)                      ! len_a2m
    real(kind=KTGT), intent(in)    :: ru(*), rv(*), rocn(*)        ! (nxygdm)
    integer,         intent(in)    :: ij_amax
    integer,         intent(in)    :: u

    character(len=litem) :: dummy
    integer r(2)
    integer lw, lx, ly, lh
    integer len_a2m
    integer,allocatable :: bi(:)
    real(kind=KTGT),allocatable :: bb(:)

    ierr = 0
    len_a2m = ij_ahead(ij_amax)
    if (ierr.eq.0) call put_header_cprop(ierr, hd, 'amax', (/1, ij_amax/), 3)
    if (ierr.eq.0) then
       call get_header_cprop(dummy, r, hd, 1)
       lx = max(0, r(2) - r(1)) + 1
       call get_header_cprop(dummy, r, hd, 2)
       ly = max(0, r(2) - r(1)) + 1
       lh = lx * ly
    endif

    if (IAND(fmt, fmt_expand).gt.0) then
       lw = lh * ij_amax
       if (ierr.eq.0) allocate(bi(0:lw-1), bb(0:lw-1), STAT=ierr)
       if (ierr.eq.0) then
          call unpack_array(ierr, bi, lh, ij_amax, ijc2o,  ijrecov_a2m, ij_ahead, vmiss)
       endif
       if (ierr.eq.0) call put_item(ierr, hd, 'MI4', hi_DFMT)
       if (ierr.eq.0) call put_item(ierr, hd, 'IJC2O', hi_ITEM)
       if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
       if (ierr.eq.0) call nio_write_data(ierr, bi, lw, hd, krect, u)

       if (ierr.eq.0) then
          call unpack_array(ierr, bb, lh, ij_amax, satm,  ijrecov_a2m, ij_ahead, real(vmiss, kind=KTGT))
       endif
       if (ierr.eq.0) call put_item(ierr, hd, 'MR8', hi_DFMT)
       if (ierr.eq.0) call put_item(ierr, hd, 'SATM', hi_ITEM)
       if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
       if (ierr.eq.0) call nio_write_data(ierr, bb, lw, hd, krect, u)

       if (ierr.eq.0) deallocate(bi, bb, STAT=ierr)
    else
       ! PTn format store ij_ahead(1:), not (0:)
       if (ierr.eq.0) call put_item(ierr, hd, 'PI4', hi_DFMT)
       if (ierr.eq.0) call put_item(ierr, hd, 'IJRECOV_A2M', hi_ITEM)
       if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
       if (ierr.eq.0) call nio_write_data(ierr, ijrecov_a2m, len_a2m, hd, krect, u, kopts=ij_ahead(1:ij_amax))

       if (ierr.eq.0) call put_item(ierr, hd, 'PI4 IJRECOV_A2M', hi_DFMT)

       if (ierr.eq.0) call put_item(ierr, hd, 'IJC2O', hi_ITEM)
       if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
       if (ierr.eq.0) call nio_write_data(ierr, ijc2o, len_a2m, hd, krect, u, kopts=ij_ahead(1:ij_amax))

       if (ierr.eq.0) call put_item(ierr, hd, 'PR8 IJRECOV_A2M', hi_DFMT)
       if (ierr.eq.0) call put_item(ierr, hd, 'SATM', hi_ITEM)
       if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
       if (ierr.eq.0) call nio_write_data(ierr, satm, len_a2m, hd, krect, u, kopts=ij_ahead(1:ij_amax))
    endif
    if (ierr.eq.0) call put_item(ierr, hd, 'UR8', hi_DFMT)
    if (ierr.eq.0) call put_header_cprop(ierr, hd, ' ', (/1, 1/), 3)
    if (ierr.eq.0) call put_item(ierr, hd, 'RU', hi_ITEM)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
    if (ierr.eq.0) call nio_write_data(ierr, ru, lh, hd, krect, u)

    if (ierr.eq.0) call put_item(ierr, hd, 'RV', hi_ITEM)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
    if (ierr.eq.0) call nio_write_data(ierr, rv, lh, hd, krect, u)

    if (ierr.eq.0) call put_item(ierr, hd, 'ROCN', hi_ITEM)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
    if (ierr.eq.0) call nio_write_data(ierr, rocn, lh, hd, krect, u)

  end subroutine write_radata_nio_d

!!!_  - write_rodata_nio
  subroutine write_rodata_nio_d &
       & (ierr,  krect, &
       &  hd,    u,     fmt,    &
       &  ijrecov_o2c,  ijo2c, socn,  ruo, rvo, flandg, ij_ohead, ij_omax)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Nio,only: hi_DFMT, hi_ITEM, litem
    use TOUZA_Nio,only: nio_write_header, nio_write_data, put_item
    use TOUZA_Nio,only: get_header_cprop, put_header_cprop
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: krect
    character(len=*),intent(inout) :: hd(*)
    integer,         intent(in)    :: fmt
    integer,         intent(in)    :: ijrecov_o2c(*), ijo2c(*)     ! len_a2m
    real(kind=KTGT), intent(in)    :: socn(*)
    real(kind=KTGT), intent(in)    :: ruo(*), rvo(*), flandg(*)
    integer,         intent(in)    :: ij_ohead(0:*)
    integer,         intent(in)    :: ij_omax
    integer,         intent(in)    :: u

    character(len=litem) :: dummy
    integer r(2)
    integer len_o2a
    integer lw, lx, ly, lh
    integer,allocatable :: bi(:)
    real(kind=KTGT),allocatable :: bb(:)

    ierr = 0
    if (ierr.eq.0) call put_header_cprop(ierr, hd, 'omax', (/1, ij_omax/), 3)
    if (ierr.eq.0) then
       call get_header_cprop(dummy, r, hd, 1)
       lx = max(0, r(2) - r(1)) + 1
       call get_header_cprop(dummy, r, hd, 2)
       ly = max(0, r(2) - r(1)) + 1
       lh = lx * ly
       lw = lh * ij_omax
    endif

    len_o2a = ij_ohead(ij_omax)

    if (IAND(fmt, fmt_expand).gt.0) then
       if (ierr.eq.0) allocate(bi(0:lw-1), bb(0:lw-1), STAT=ierr)
       if (ierr.eq.0) then
          call unpack_array(ierr, bi, lh, ij_omax, ijo2c,  ijrecov_o2c, ij_ohead, vmiss)
       endif
       if (ierr.eq.0) call put_item(ierr, hd, 'MI4', hi_DFMT)
       if (ierr.eq.0) call put_item(ierr, hd, 'IJO2C', hi_ITEM)
       if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
       if (ierr.eq.0) call nio_write_data(ierr, bi, lw, hd, krect, u)

       if (ierr.eq.0) then
          call unpack_array(ierr, bb, lh, ij_omax, socn, ijrecov_o2c, ij_ohead, real(vmiss, kind=KTGT))
       endif
       if (ierr.eq.0) call put_item(ierr, hd, 'MR8', hi_DFMT)
       if (ierr.eq.0) call put_item(ierr, hd, 'SOCN', hi_ITEM)
       if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
       if (ierr.eq.0) call nio_write_data(ierr, bb, lw, hd, krect, u)

       if (ierr.eq.0) deallocate(bi, bb, STAT=ierr)
    else
       if (ierr.eq.0) call put_item(ierr, hd, 'PI4', hi_DFMT)
       if (ierr.eq.0) call put_item(ierr, hd, 'IJRECOV_O2C', hi_ITEM)
       if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
       if (ierr.eq.0) call nio_write_data(ierr, ijrecov_o2c, len_o2a, hd, krect, u, kopts=ij_ohead(1:ij_omax))

       if (ierr.eq.0) call put_item(ierr, hd, 'PI4 IJRECOV_O2C', hi_DFMT)

       if (ierr.eq.0) call put_item(ierr, hd, 'IJO2C', hi_ITEM)
       if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
       if (ierr.eq.0) call nio_write_data(ierr, ijo2c, len_o2a, hd, krect, u, kopts=ij_ohead(1:ij_omax))

       if (ierr.eq.0) call put_item(ierr, hd, 'PR8 IJRECOV_O2C', hi_DFMT)
       if (ierr.eq.0) call put_item(ierr, hd, 'SOCN', hi_ITEM)
       if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
       if (ierr.eq.0) call nio_write_data(ierr, socn, len_o2a, hd, krect, u, kopts=ij_ohead(1:ij_omax))
    endif

    if (ierr.eq.0) call put_item(ierr, hd, 'UR8', hi_DFMT)
    if (ierr.eq.0) call put_header_cprop(ierr, hd, ' ', (/1, 1/), 3)
    if (ierr.eq.0) call put_item(ierr, hd, 'FLANDG', hi_ITEM)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
    if (ierr.eq.0) call nio_write_data(ierr, flandg, lh, hd, krect, u)

    if (ierr.eq.0) call put_item(ierr, hd, 'RUO', hi_ITEM)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
    if (ierr.eq.0) call nio_write_data(ierr, ruo, lh, hd, krect, u)

    if (ierr.eq.0) call put_item(ierr, hd, 'RVO', hi_ITEM)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krect, u)
    if (ierr.eq.0) call nio_write_data(ierr, rvo, lh, hd, krect, u)

  end subroutine write_rodata_nio_d
!!!_   . pack_array
  subroutine pack_array_d &
       & (ierr, &
       &  vout, posh, strds, &
       &  vin,  lh,   mz,    fill)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: vout(*)     ! [strds(mz)]
    integer,        intent(out) :: posh(*)     ! [strds(mz)]
    integer,        intent(out) :: strds(0:*)
    real(kind=KTGT),intent(in)  :: vin(*)      ! [lh, mz]
    integer,        intent(in)  :: lh, mz
    real(kind=KTGT),intent(in)  :: fill

    integer jz, jh, jv, jp

    ierr = 0
    jp = 1
    strds(0) = 0
    do jz = 0, mz - 1
       do jh = 1, lh
          jv = jz * mz + jh
          if (vin(jv).eq.fill) cycle
          posh(jp) = jh
          vout(jp) = vin(jv)
          jp = jp + 1
       enddo
       strds(jz + 1) = jp
    enddo

  end subroutine pack_array_d

  subroutine pack_array_i &
       & (ierr, &
       &  vout, posh, strds, &
       &  vin,  lh,   mz,    fill)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: vout(*)     ! [strds(mz)]
    integer,intent(out) :: posh(*)     ! [strds(mz)]
    integer,intent(out) :: strds(0:*)
    integer,intent(in)  :: vin(*)      ! [lh, mz]
    integer,intent(in)  :: lh, mz
    integer,intent(in)  :: fill

    integer jz, jh, jv, jp

    ierr = 0
    jp = 1
    strds(0) = 0
    do jz = 0, mz - 1
       do jh = 1, lh
          jv = jz * mz + jh
          if (vin(jv).eq.fill) cycle
          posh(jp) = jh
          vout(jp) = vin(jv)
          jp = jp + 1
       enddo
       strds(jz + 1) = jp
    enddo

  end subroutine pack_array_i

!!!_   . unpack_array
  subroutine unpack_array_d &
       & (ierr, &
       &  vout, lh,   mz, &
       &  vin,  posh, strds, fill)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KTGT),intent(out) :: vout(0:*)     ! [lh, mz]
    integer,        intent(in)  :: lh, mz
    real(kind=KTGT),intent(in)  :: vin(0:*)      ! [strds(mz)]
    integer,        intent(in)  :: posh(0:*)     ! [strds(mz)]
    integer,        intent(in)  :: strds(0:*)
    real(kind=KTGT),intent(in)  :: fill

    integer jz, js, jp, ltotal

    ierr = 0

    ltotal = lh * mz
    vout(0:ltotal-1) = fill

    do jz = 0, mz - 1
       do js = strds(jz), strds(jz+1) - 1
          jp = jz * lh + posh(js)
          vout(jp) = vin(js)
       enddo
    enddo
  end subroutine unpack_array_d

  subroutine unpack_array_i &
       & (ierr, &
       &  vout, lh,   mz, &
       &  vin,  posh, strds, fill)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: vout(0:*)     ! [lh, mz]
    integer,intent(in)  :: lh, mz
    integer,intent(in)  :: vin(0:*)      ! [strds(mz)]
    integer,intent(in)  :: posh(0:*)     ! [strds(mz)]
    integer,intent(in)  :: strds(0:*)
    integer,intent(in)  :: fill

    integer jz, js, jp, ltotal

    ierr = 0

    ltotal = lh * mz
    vout(0:ltotal-1) = fill

    do jz = 0, mz - 1
       do js = strds(jz), strds(jz+1) - 1
          jp = jz * lh + posh(js)
          vout(jp) = vin(js)
       enddo
    enddo
  end subroutine unpack_array_i
!!!_ + end Ami_nio
end module TOUZA_Ami_nio
!!!_@ test_ami_nio - test program
#if TEST_AMI_NIO
program test_ami_nio
  use TOUZA_Ami_legacy,only: al_init=>init
  use TOUZA_Ami_legacy,only: open_rafile_legacy, read_rafile_legacy
  use TOUZA_Ami_legacy,only: open_rofile_legacy, read_rofile_legacy1, read_rofile_legacy2
  use TOUZA_Ami_nio
  use TOUZA_Std,only: parse, decl_pos_arg, get_option, get_param, arg_diag, arg_init, KDBL
  use TOUZA_Std,only: new_unit
  use TOUZA_Std,only: sus_close
  use TOUZA_Nio,nio_init=>init, nio_diag=>diag, nio_finalize=>finalize
  implicit none
  integer,parameter :: KMD = KDBL
  integer ierr
  integer jarg
  integer,parameter :: lpath = 256
  character(len=lpath) :: rafile
  character(len=lpath) :: rofile

  character(len=lpath) :: xfile
  integer u
  logical swap

  integer ij_amax, len_a2m, nxygdm
  integer,       allocatable :: ij_ahead(:), ijrecov_a2m(:), ijc2o(:)
  real(kind=KMD),allocatable :: satm(:), ru(:), rv(:), rocn(:)

  integer ij_omax, len_o2a, ijdim
  integer,       allocatable :: ij_ohead(:), ijrecov_o2c(:), ijo2c(:)
  real(kind=KMD),allocatable :: socn (:), ruo(:), rvo(:), flandg(:)

  integer mdomo, mdoma

  character(len=litem) :: axatm, ayatm
  character(len=litem) :: axocn, ayocn
  integer mxa, dxa, pxa         ! lxa = (mxa + pxa) * dxa
  integer mya, dya              ! lya = (mya + 0)   * dya

  integer mxo, lxo, wxo         ! lxo = mxo + wxo * 2
  integer myo, lyo, wyo         ! lyo = myo + wyo * 2

  integer mxc, lxc, pxc
  integer myc, lyc

  integer kfmt
  integer krect
  character(len=litem) :: hd(nitem)
  character(len=128) :: arg

  integer,parameter :: offset_legacy = 1

  ierr = 0
  jarg = 0
  rafile = ' '
  rofile = ' '
  xfile = ' '

  nxygdm = 0
  ijdim = 0
  ij_omax = 0

  ! [RA=]FILE [RO=]FILE [OUT=]FILE

  ! AX=[NAME/]NUM[,DIV[,PAD]]   default: DIV=1,PAD=1  NAME=GLON
  ! AY=[NAME/]NUM[,DIV]                  DIV=1        NAME=GGLA
  ! OX=[NAME/]NUM[,WING]                 WING=2       NAME=OCLONT
  ! OY=[NAME/]NUM[,WING]                 WING=2       NAME=OCLATT

  call init(ierr)
  call nio_init(ierr)
  if (ierr.eq.0) call arg_init(ierr, levv=-9)

  if (ierr.eq.0) call decl_pos_arg(ierr, 'RA')
  if (ierr.eq.0) call decl_pos_arg(ierr, 'RO')
  if (ierr.eq.0) call decl_pos_arg(ierr, 'OUT')
  if (ierr.eq.0) call parse(ierr)

  if (ierr.eq.0) call parse_arg_coor(ierr, 'AX', 'GLON',   (/0, 1, 1/), axatm, mxa, dxa, pxa)
  if (ierr.eq.0) call parse_arg_coor(ierr, 'AY', 'GGLA',   (/0, 1/),    ayatm, mya, dya)
  if (ierr.eq.0) call parse_arg_coor(ierr, 'OX', 'OCLONT', (/0, 2/),    axocn, mxo, wxo)
  if (ierr.eq.0) call parse_arg_coor(ierr, 'OY', 'OCLATT', (/0, 2/),    ayocn, myo, wyo)

  if (ierr.eq.0) call get_option(ierr, rafile, 'RA',  ' ')
  if (ierr.eq.0) call get_option(ierr, rofile, 'RO',  ' ')
  if (ierr.eq.0) call get_option(ierr, xfile,  'OUT', ' ')

  if (ierr.eq.0) call get_option(ierr, arg,  'FMT', ' ')
  kfmt = 0
  if (ierr.eq.0) then
     if (index(arg, 'x').gt.0) kfmt = IOR(kfmt, fmt_expand)
  endif


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
           call read_rofile_legacy1 &
                & (ierr,     &
                &  ij_omax,  len_o2a, &
                &  ijdim,    u,       swap)
        endif
112     format('open/rofile: ', A, 1x, L1, 2(1x, I0), 1x, I0, ' / ', I0)
        mdomo = ijdim * ij_omax
        write(*, 112) trim(rofile), swap, ij_omax, ijdim, len_o2a, mdomo
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
                &  ijdim,    ij_omax,     u,     swap)
        endif
     endif
     if (ierr.eq.0) call sus_close(ierr, u, rofile)
  endif

  ! atm/cpl domain
  lxc = 0
  pxc = 0
  lyc = 0
  mxc = mxa * dxa
  myc = mya * dya
  if (ierr.eq.0) then
     if (mxc.gt.0) then
        pxc = pxa * dxa
        lxc = mxc + pxc
        if (myc.eq.0) myc = ijdim / lxc
        lyc = myc
        if (lxc * lyc .ne. ijdim) then
           ierr = -1
           write(*, *) 'invalid atm domain = ', lxc * lyc, ijdim
        endif
     endif
  endif
  if (ierr.eq.0) then
201  format('atm: ', I0, &
          & 1x, '(', I0, '+', I0, ')*', I0, '=', I0, &
          & 1x, I0, '*', I0, '=', I0)
     write(*, 201) ijdim, mxa, pxa, dxa, lxc, mya, dya, lyc
  endif
  ! ocn domain
  lxo = 0
  lyo = 0
  if (ierr.eq.0) then
     if (mxo.gt.0) then
        lxo = mxo + wxo * 2
        if (myo.eq.0) myo = (nxygdm / lxo) - wyo * 2
        lyo = myo  + wyo * 2
        if (lxo * lyo .ne. nxygdm) then
           ierr = -1
           write(*, *) 'invalid ocn domain = ', lxo * lyo, nxygdm
        endif
     endif
  endif
  if (ierr.eq.0) then
202  format('ocn: ', I0, &
          & 1x, I0, ':', I0, '=', I0, &
          & 1x, I0, ':', I0, '=', I0)
     write(*, 202) nxygdm, mxo, wxo, lxo, myo, wyo, lyo
  endif

  if (ierr.eq.0) then
     call normalize_radata &
          & (ierr, &
          &  ijrecov_a2m, ijc2o,   satm, ru,  rv, rocn, &
          &  ij_ahead,    ij_amax, &
          &  nxygdm,      mxo,     wxo,  myo, wyo, &
          &  mxc,         pxc,     lyc,  offset_legacy)
  endif
  if (ierr.eq.0) then
     call normalize_rodata &
          & (ierr,        &
          &  ijrecov_o2c, ijo2c,   socn, ruo, rvo, flandg, &
          &  ij_ohead,    ij_omax, &
          &  mxo,         wxo,     myo,  wyo, &
          &  ijdim,       mxc,     pxc,  lyc, offset_legacy)
  endif

  if (xfile.ne.' ') then
     if (ierr.eq.0) then
        krect = REC_BIG
        if (ierr.eq.0) call sus_open(ierr, u, xfile, ACTION='W', STATUS='R')
        if (ierr.eq.0) then
           call set_raheader &
                & (ierr, hd, axocn, ayocn, nxygdm, mxo, myo)
        endif
        ! if (ierr.eq.0) call set_rheader_extra(ierr, hd, 1, axatm, mxc)
        ! if (ierr.eq.0) call set_rheader_extra(ierr, hd, 2, ayatm, myc)
        if (ierr.eq.0) then
           call write_radata_nio &
                & (ierr, krect, &
                &  hd,   u,     kfmt,  &
                &  ijrecov_a2m, ijc2o, satm, ru, rv, rocn, ij_ahead, ij_amax)
        endif
        if (ierr.eq.0) call set_roheader(ierr, hd, axatm, ayatm, ijdim, mxc, myc)
        ! if (ierr.eq.0) call set_rheader_extra(ierr, hd, 1, axocn, mxo)
        ! if (ierr.eq.0) call set_rheader_extra(ierr, hd, 2, ayocn, myo)
        if (ierr.eq.0) then
           call write_rodata_nio &
                & (ierr,  krect, &
                &  hd,    u,    kfmt,   &
                &  ijrecov_o2c, ijo2c, socn, ruo, rvo, flandg, ij_ohead, ij_omax)
        endif
     endif
  endif

  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call finalize(ierr)
  write(*,*) 'fine = ', ierr
  stop
contains
  subroutine parse_arg_coor &
       & (ierr, key,   adef, idefs, &
       &  name, mbase, madd, mopt)
    use TOUZA_Std,only: split_heads, split_list
    use TOUZA_Std,only: get_option
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: key
    character(len=*),intent(in)  :: adef
    integer,         intent(in)  :: idefs(0:*)
    character(len=*),intent(out) :: name
    integer,         intent(out) :: mbase, madd
    integer,optional,intent(out) :: mopt

    character(len=128) :: param
    character(len=*),parameter :: asep = '/'
    character(len=*),parameter :: isep = ','

    integer ls
    integer ji, ni
    integer pos(0:4), jb, je
    integer mpar(0:3)
    integer np

    ierr = 0
    call get_option(ierr, param, key, ' ')
    if (ierr.eq.0) then
       ls = len(asep)
       call split_heads(ni, pos, param, asep, 2, empty=.TRUE.)
       ji = 0
       if (ni.gt.1) then
          jb = pos(ji) + 1
          je = pos(ji+1) - ls
          if (jb.gt.je) then
             name = adef
          else
             name = param(jb:je)
          endif
          ji = ji + 1
       else
          name = adef
       endif
       np = 2
       mpar(0:np-1) = idefs(0:np-1)
       if (present(mopt)) then
          mpar(np) = idefs(np)
          np = np + 1
       endif
       if (ji.lt.ni) then
          jb = pos(ji) + 1
          call split_list(ni, mpar(0:np-1), param(jb:), isep, np, empty=.TRUE.)
       endif
    endif
    if (ierr.eq.0) then
       mbase = mpar(0)
       madd = mpar(1)
       if (present(mopt)) then
          mopt = mpar(2)
       endif
    endif
  end subroutine parse_arg_coor

end program test_ami_nio
#endif /* TEST_AMI_NIO */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

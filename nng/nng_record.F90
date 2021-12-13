!!!_! nng_record.F90 - TOUZA/Nng record interfaces
! Maintainer: SAITO Fuyuki
! Created: Oct 29 2021
#define TIME_STAMP 'Time-stamp: <2021/12/13 08:49:30 fuyuki nng_record.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_nng.h"
!!!_@ TOUZA_Nng_record - nng record interfaces
module TOUZA_Nng_record
!!!_ = declaration
  use TOUZA_Nng_std,only: &
       & KI32, KI64, KDBL, KFLT, &
       & control_mode, control_deep, is_first_force, &
       & get_logu,     unit_global,  trace_fine,   trace_control
  use TOUZA_Nng_header,only: litem, nitem
  use TOUZA_Trp, only: &
       & KCODE_CLIPPING,  KCODE_ROUND,      &
       & KCODE_TRANSPOSE, KCODE_SEQUENTIAL, KCODE_INCREMENTAL, &
       & KCODE_MANUAL,    RELLENO_SEQUENTIAL
  implicit none
  private
!!!_  - public parameters
  integer,parameter,public :: KRMIS=KDBL ! real kind of vmiss

  integer,parameter,public :: REC_ERROR   = -9
  integer,parameter,public :: REC_ASIS    = -1
  integer,parameter,public :: REC_DEFAULT = 0
  integer,parameter,public :: REC_SWAP    = 1  ! with byte-swap
  integer,parameter,public :: REC_LSEP    = 2  ! with 64-bit record marker
  integer,parameter,public :: REC_BIG     = 4  ! force big-endian    (use in init only)
  integer,parameter,public :: REC_LITTLE  = 8  ! force little-endian (use in init only)

  integer,parameter,public :: REC_LEFT  = 2**8 ! number of extra header items (shift)

  integer,parameter :: HEADER_LIMIT  = 2**16     ! 00 00 01 00 [l]
  integer,parameter :: HEADER_SPACES = 538976288 ! 20 20 20 20 [l]

  integer,parameter :: GFMT_ID_LEGACY = 9010

  integer,parameter,public :: BODR_ASSUME_SYSTEM  = 0    ! assume file byte-order == system
  integer,parameter,public :: BODR_ASSUME_FILE    = 1    ! assume file byte-order == common
  integer,parameter,public :: BODR_CHECK_VERBOSE  = 2    ! check for each unit at open-write

!!!_   . formats
  integer,parameter,public :: GFMT_ERR  = -1
  integer,parameter,public :: GFMT_MASK = 2048  /* mask bit */

  integer,parameter,public :: GFMT_UR8  = 1
  integer,parameter,public :: GFMT_UR4  = 2
  integer,parameter,public :: GFMT_URC  = 3
  integer,parameter,public :: GFMT_URC2 = 4

  integer,parameter,public :: GFMT_MR8  = GFMT_UR8 + GFMT_MASK
  integer,parameter,public :: GFMT_MR4  = GFMT_UR4 + GFMT_MASK

  integer,parameter,public :: GFMT_URY    = 32
  integer,parameter,public :: GFMT_URYend = GFMT_URY + BIT_SIZE(0_KI32) - 1
  integer,parameter,public :: GFMT_MRY    = GFMT_URY    + GFMT_MASK
  integer,parameter,public :: GFMT_MRYend = GFMT_URYend + GFMT_MASK

  integer,parameter,public :: GFMT_URS  = 128  ! reserved AND discarded
  integer,parameter,public :: GFMT_ZRN  = 129  ! reserved AND discarded
  integer,parameter,public :: GFMT_JRN  = 130  ! reserved AND discarded

  integer,parameter,public :: GFMT_UI0  = 144  ! invalid itself
  integer,parameter,public :: GFMT_UI1  = GFMT_UI0 + 1
  integer,parameter,public :: GFMT_UI4  = GFMT_UI0 + 4
  integer,parameter,public :: GFMT_UI8  = GFMT_UI0 + 8

  integer,parameter,public :: GFMT_MI1  = GFMT_UI1 + GFMT_MASK
  integer,parameter,public :: GFMT_MI4  = GFMT_UI4 + GFMT_MASK
  integer,parameter,public :: GFMT_MI8  = GFMT_UI8 + GFMT_MASK

  integer,parameter,public :: GFMT_URT     = 256
  integer,parameter,public :: GFMT_URTend  = GFMT_URT + 64
  integer,parameter,public :: GFMT_MRT     = GFMT_URT    + GFMT_MASK
  integer,parameter,public :: GFMT_MRTend  = GFMT_URTend + GFMT_MASK

!!!_    * URT details
  integer,parameter,public :: PROP_DEFAULT = (- HUGE(0)) - 1

  integer,parameter,public :: PROP_URT_MANTISSA = 1 /* mantissa bits */
  integer,parameter,public :: PROP_URT_XBITS    = 2 /* exponent bits */
  integer,parameter,public :: PROP_URT_XBOTTOM  = 3 /* exponent lower limit (relative to exp(1)) */
  integer,parameter,public :: PROP_URT_XTOP     = 4 /* exponent lower limit (relative to exp(1)) */
  integer,parameter,public :: PROP_URT_CODES    = 5 /* kcode switches */
  integer,parameter,public :: PROP_URT_BREAK    = 6 /* array break */

  integer,parameter,public :: GFMT_URT_BREAK_NONE  = 0
  integer,parameter,public :: GFMT_URT_BREAK_Z     = 1
  integer,parameter,public :: GFMT_URT_BREAK_PROC  = 2
  integer,parameter,public :: GFMT_URT_BREAK_ZPROC = 3

  integer,parameter,public :: XID_URT = 1632916053 ! 'URTa'
  ! integer,parameter,public :: XID_MRT = 1632916045 ! 'MRTa'
  integer,parameter,public :: XTRP_ID = 0  ! index to store nng/urt i
  integer,parameter,public :: XTRP_NX = 1  ! index to store nng/urt number of extra properties

!!!_  - private parameter
  integer,parameter :: GPROP_NUMX  = 1
  integer,parameter :: GPROP_NUMY  = 2
  integer,parameter :: GPROP_NUMZ  = 3

  integer,parameter :: GPROP_VMISS = 1

  integer,parameter :: IMISS_URC = 65534

!!!_  - static
  real(KIND=KRMIS),save :: def_VMISS = -999.0_KRMIS
  integer,save :: def_encode_legacy   = KCODE_MANUAL   ! automatic KCODE_SEQUENTIAL
  integer,save :: def_decode_legacy   = KCODE_MANUAL   ! automatic KCODE_SEQUENTIAL
  integer,save :: def_encode_trapiche = KCODE_TRANSPOSE + KCODE_MANUAL
  integer,save :: def_decode_trapiche = KCODE_MANUAL

  integer,save :: def_krectw = 0  !  default record switch to write

  character(len=litem),save :: head_def(nitem) = ' '

  integer,save :: bodr_wnative = BODR_ASSUME_SYSTEM
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = NNG_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
#define __MDL__ 'r'

  integer,save :: nlhead_std = 0 ! standard gtool header total length in bytes

  integer,save :: udiag = -2    ! io unit to diag urt properties
!!!_  - interfaces
  interface nng_read_data
     module procedure nng_read_data_f, nng_read_data_d, nng_read_data_i
  end interface nng_read_data
  interface nng_write_data
     module procedure nng_write_data_f, nng_write_data_d, nng_write_data_i
  end interface nng_write_data

  interface nng_read_data_core
     module procedure nng_read_data_core_f, nng_read_data_core_d, nng_read_data_core_i
  end interface nng_read_data_core
  interface nng_write_data_core
     module procedure nng_write_data_core_f, nng_write_data_core_d, nng_write_data_core_i
  end interface nng_write_data_core

  interface get_data_record
     module procedure get_data_record_i,  get_data_record_f,  get_data_record_d
     module procedure get_data_record_i1, get_data_record_f1, get_data_record_d1
  end interface get_data_record

  interface put_data_record
     module procedure put_data_record_i,  put_data_record_f,  put_data_record_d
     module procedure put_data_record_i1, put_data_record_f1, put_data_record_d1
  end interface put_data_record

  interface get_data_drecord
     module procedure get_data_drecord_f, get_data_drecord_i
     module procedure get_data_record_d
  end interface get_data_drecord
  interface get_data_frecord
     module procedure get_data_frecord_d, get_data_frecord_i
     module procedure get_data_record_f
  end interface get_data_frecord
  interface get_data_irecord
     module procedure get_data_irecord_f, get_data_irecord_d
     module procedure get_data_record_i
  end interface get_data_irecord

  interface put_data_drecord
     module procedure put_data_drecord_f, put_data_drecord_i
     module procedure put_data_record_d
  end interface put_data_drecord
  interface put_data_frecord
     module procedure put_data_frecord_d, put_data_frecord_i
     module procedure put_data_record_f
  end interface put_data_frecord
  interface put_data_irecord
     module procedure put_data_irecord_f, put_data_irecord_d
     module procedure put_data_record_i
  end interface put_data_irecord

  interface get_data_urc
     module procedure get_data_urc_f, get_data_urc_d, get_data_urc_i
  end interface get_data_urc
  interface put_data_urc
     module procedure put_data_urc_f, put_data_urc_d, put_data_urc_i
  end interface put_data_urc

  interface get_data_mr4
     module procedure get_data_mr4_f, get_data_mr4_d, get_data_mr4_i
  end interface get_data_mr4
  interface get_data_mr8
     module procedure get_data_mr8_f, get_data_mr8_d, get_data_mr8_i
  end interface get_data_mr8
  interface get_data_mry
     module procedure get_data_mry_f, get_data_mry_d, get_data_mry_i
  end interface get_data_mry
  interface get_data_ury
     module procedure get_data_ury_f, get_data_ury_d, get_data_ury_i
  end interface get_data_ury

  interface put_data_mr4
     module procedure put_data_mr4_f, put_data_mr4_d, put_data_mr4_i
  end interface put_data_mr4
  interface put_data_mr8
     module procedure put_data_mr8_f, put_data_mr8_d, put_data_mr8_i
  end interface put_data_mr8
  interface put_data_mry
     module procedure put_data_mry_f, put_data_mry_d, put_data_mry_i
  end interface put_data_mry
  interface put_data_ury
     module procedure put_data_ury_f, put_data_ury_d, put_data_ury_i
  end interface put_data_ury

  interface put_data_urt_plain
     module procedure put_data_urt_plain_d, put_data_urt_plain_f
  end interface put_data_urt_plain
  interface put_data_mrt_plain
     module procedure put_data_mrt_plain_d, put_data_mrt_plain_f
  end interface put_data_mrt_plain
  interface put_data_urt_core
     module procedure put_data_urt_core_d, put_data_urt_core_f
  end interface put_data_urt_core

  interface get_data_urt
     module procedure get_data_urt_d, get_data_urt_f
  end interface get_data_urt
  interface get_data_urt_core
     module procedure get_data_urt_core_d, get_data_urt_core_f
  end interface get_data_urt_core
  interface get_data_mrt
     module procedure get_data_mrt_d, get_data_mrt_f
  end interface get_data_mrt

  interface get_data_mi4
     module procedure get_data_mi4_f, get_data_mi4_d, get_data_mi4_i
  end interface get_data_mi4
  interface put_data_mi4
     module procedure put_data_mi4_f, put_data_mi4_d, put_data_mi4_i
  end interface put_data_mi4

  interface normalize_xry
     module procedure normalize_xry_d
  end interface normalize_xry
  interface mask_encode
     module procedure mask_encode_di, mask_encode_fi, mask_encode_ii
  end interface mask_encode
  interface mask_decode
     module procedure mask_decode_di, mask_decode_fi, mask_decode_ii
  end interface mask_decode

  interface parse_header_size
     module procedure parse_header_size_n
     module procedure parse_header_size_i
  end interface parse_header_size

!!!_  - public procedures
  public init, diag, finalize
  public set_default_switch
  public set_default_header, get_default_header
  public nng_read_header,    nng_write_header
  public nng_read_data,      nng_write_data
  public nng_skip_records
  public parse_header_base,  parse_record_fmt
  public parse_header_size
  public get_switch
  public set_urt_defs
  public switch_urt_diag

!!!_  - public shared
!!!_   . from Trp
  public KCODE_CLIPPING
  public KCODE_TRANSPOSE, KCODE_SEQUENTIAL, KCODE_INCREMENTAL
  public KCODE_MANUAL,    RELLENO_SEQUENTIAL

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init &
       & (ierr,   u,      levv,  mode,  stdv,  &
       &  bodrw,  krectw, klenc, kldec, knenc, kndec, &
       &  vmiss,  utime,  csign, msign)
    use TOUZA_Nng_std,   only: choice, get_size_bytes, KDBL
    use TOUZA_Nng_header,only: nh_init=>init, litem, nitem
    use TOUZA_Nng_io,    only: ns_init=>init
    use TOUZA_Trp,       only: trp_init=>init
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv, mode, stdv
    integer,         intent(in),optional :: bodrw       ! byte-order native flag
    integer,         intent(in),optional :: krectw      ! default record switch (write)
    integer,         intent(in),optional :: klenc,kldec ! packing method for legacy-format (ury)
    integer,         intent(in),optional :: knenc,kndec ! packing method for new format (urt)
    real(kind=KDBL), intent(in),optional :: vmiss       ! default header properties
    character(len=*),intent(in),optional :: utime
    character(len=*),intent(in),optional :: csign, msign
    integer lv, md, lmd
    character(len=litem) :: hdummy

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
          if (ierr.eq.0) call nh_init(ierr, u=ulog, levv=lv, mode=lmd)
       endif
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call trp_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv)
       endif
       if (init_counts.eq.0) then
          nlhead_std = get_size_bytes(hdummy, nitem)
          if (ierr.eq.0) call set_bodr_wnative(ierr, bodrw, ulog, lv)
          if (ierr.eq.0) call set_default_switch(ierr, krectw, ulog, lv)
          if (ierr.eq.0) then
             call init_batch(ierr, klenc, kldec, knenc, kndec, ulog, lv)
          endif
          if (ierr.eq.0) then
             call set_default_header(ierr, vmiss, utime, csign, msign)
          endif
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Nng_std,   only: choice, msg, is_msglev_normal, is_msglev_info
    use TOUZA_Nng_header,only: nh_diag=>diag, show_header
    use TOUZA_Nng_io,    only: ns_diag=>diag
    use TOUZA_Trp,       only: trp_diag=>diag
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
             if (is_msglev_normal(lv)) call msg('(''byte-order assumption = '', I0)', bodr_wnative, __MDL__, utmp)
             if (is_msglev_info(lv)) then
                call show_header(ierr, head_def, ' ', utmp, lv)
             endif
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ns_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nh_diag(ierr, utmp, levv=lv, mode=lmd)
       endif
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call trp_diag(ierr, utmp, levv=lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Nng_std,   only: choice
    use TOUZA_Nng_header,only: nh_finalize=>finalize
    use TOUZA_Nng_io,    only: ns_finalize=>finalize
    use TOUZA_Trp,       only: trp_finalize=>finalize
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
          if (ierr.eq.0) call nh_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call ns_finalize(ierr, utmp, levv=lv, mode=lmd)
       endif
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call trp_finalize(ierr, utmp, levv=lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_  - init subcontracts
!!!_   & set_bodr_wnative
  subroutine set_bodr_wnative(ierr, bodrw, u, levv)
    use TOUZA_Nng_std,only: &
         & choice, msg, is_msglev_info, is_msglev_fatal, &
         & kendi_mem, kendi_file
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: bodrw
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    ierr = 0
    bodr_wnative = choice(bodr_wnative, bodrw)
    select case (bodr_wnative)
    case (BODR_ASSUME_SYSTEM)
       if (is_msglev_info(levv)) then
          call msg('(''assume system byte-order when write = '', I0)', kendi_mem, __MDL__, u)
       endif
    case (BODR_ASSUME_FILE)
       if (is_msglev_info(levv)) then
          call msg('(''assume estimated file byte-order when write = '', I0)', kendi_file, __MDL__, u)
       endif
    case (BODR_CHECK_VERBOSE)
       if (is_msglev_info(levv)) then
          call msg('check file byte-order when write',  __MDL__, u)
       endif
    case default
       ierr = -1
       if (is_msglev_fatal(levv)) then
          call msg('(''invalid byte-order switch = '', I0)', bodr_wnative, __MDL__, u)
       endif
    end select
    return
  end subroutine set_bodr_wnative

!!!_   . set_default_switch
  subroutine set_default_switch &
       & (ierr, krectw, u, levv)
    use TOUZA_Nng_std,only: choice, msg, is_msglev_fatal
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: krectw        ! default record switch (write)
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv

    integer ktmp
    logical bl, bb

    ierr = 0

    ktmp = choice(def_krectw, krectw)
    bl = IAND(ktmp, REC_LITTLE).gt.0
    bb = IAND(ktmp, REC_BIG).gt.0
    if      (bl.and.bb) then
       ierr = -1
       if (is_msglev_fatal(levv)) call msg('(''invalid record switch = '', I0)', ktmp, __MDL__, u)
    else
       def_krectw = ktmp
    endif
    return
  end subroutine set_default_switch

!!!_   . init_batch
  subroutine init_batch &
       & (ierr, klenc, kldec, knenc, kndec, u, levv)
    use TOUZA_Nng_std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: klenc,kldec ! packing method for legacy-format (ury)
    integer,intent(in),optional :: knenc,kndec ! packing method for new format (urt)
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv

    ierr = 0

    def_encode_legacy   = choice(def_encode_legacy,   klenc)
    def_encode_trapiche = choice(def_encode_trapiche, knenc)
    def_decode_legacy   = choice(def_decode_legacy,   kldec)
    def_decode_trapiche = choice(def_decode_trapiche, kndec)

    return
  end subroutine init_batch

!!!_   . set_default_header
  subroutine set_default_header &
       & (ierr,  &
       &  vmiss, &
       &  utime, &
       &  csign, msign)
    use TOUZA_Nng_header,only: &
         & put_item, &
         & hi_IDFM,  hi_UTIM,  hi_FNUM,  hi_DNUM,  &
         & hi_ASTR1, hi_AEND1, hi_ASTR2, hi_AEND2, hi_ASTR3, hi_AEND3,  &
         & hi_MISS,  hi_DMIN,  hi_DMAX,  hi_DIVS,  hi_DIVL,  &
         & hi_STYP,  hi_IOPTN, hi_ROPTN, hi_CSIGN, hi_MSIGN
    use TOUZA_Nng_std,only: KDBL
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KDBL), intent(in),optional :: vmiss
    character(len=*),intent(in),optional :: utime
    character(len=*),intent(in),optional :: csign, msign
    ierr = 0
    if (ierr.eq.0) call put_item(ierr, head_def, GFMT_ID_LEGACY, hi_IDFM)
    if (ierr.eq.0) call put_item(ierr, head_def, 1,              hi_FNUM)
    if (ierr.eq.0) call put_item(ierr, head_def, 1,              hi_DNUM)
    if (present(utime)) then
       if (ierr.eq.0) call put_item(ierr, head_def, utime,  hi_UTIM)
    else
       if (ierr.eq.0) call put_item(ierr, head_def, 'HOUR', hi_UTIM)
    endif
    if (ierr.eq.0) call put_item(ierr, head_def, 1, hi_ASTR1)
    if (ierr.eq.0) call put_item(ierr, head_def, 1, hi_AEND1)
    if (ierr.eq.0) call put_item(ierr, head_def, 1, hi_ASTR2)
    if (ierr.eq.0) call put_item(ierr, head_def, 1, hi_AEND2)
    if (ierr.eq.0) call put_item(ierr, head_def, 1, hi_ASTR3)
    if (ierr.eq.0) call put_item(ierr, head_def, 1, hi_AEND3)

    if (ierr.eq.0) call put_item(ierr, head_def, 1, hi_STYP)

    if (present(vmiss)) then
       if (ierr.eq.0) call put_item(ierr, head_def, vmiss, hi_MISS)
       if (ierr.eq.0) call put_item(ierr, head_def, vmiss, hi_DMIN)
       if (ierr.eq.0) call put_item(ierr, head_def, vmiss, hi_DMAX)
       if (ierr.eq.0) call put_item(ierr, head_def, vmiss, hi_DIVS)
       if (ierr.eq.0) call put_item(ierr, head_def, vmiss, hi_DIVL)
    else
       if (ierr.eq.0) call put_item(ierr, head_def, def_VMISS, hi_MISS)
       if (ierr.eq.0) call put_item(ierr, head_def, def_VMISS, hi_DMIN)
       if (ierr.eq.0) call put_item(ierr, head_def, def_VMISS, hi_DMAX)
       if (ierr.eq.0) call put_item(ierr, head_def, def_VMISS, hi_DIVS)
       if (ierr.eq.0) call put_item(ierr, head_def, def_VMISS, hi_DIVL)
    endif

    if (ierr.eq.0) call put_item(ierr, head_def, 0,        hi_IOPTN)
    if (ierr.eq.0) call put_item(ierr, head_def, 0.0_KDBL, hi_ROPTN)

    if (present(csign)) then
       if (ierr.eq.0) call put_item(ierr, head_def, csign, hi_CSIGN)
    endif
    if (present(msign)) then
       if (ierr.eq.0) call put_item(ierr, head_def, msign, hi_MSIGN)
    endif
    return
  end subroutine set_default_header

!!!_ + user interfaces
!!!_  - get_default_header - get default header
  subroutine get_default_header &
       & (head)
    use TOUZA_Nng_header,only: litem, nitem
    implicit none
    character(len=*),intent(out) :: head(*)
    head(1:nitem) = head_def(1:nitem)
    return
  end subroutine get_default_header

!!!_  & nng_read_header - read header and set current properties
  subroutine nng_read_header &
       & (ierr, &
       &  head,  krect, u)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: head(*)
    integer,         intent(out) :: krect
    integer,         intent(in)  :: u

    ierr = err_default
    if (ierr.eq.0) call get_record_prop(ierr, krect, u)
    if (ierr.eq.0) call get_header(ierr, head, u, krect)
    return
  end subroutine nng_read_header

!!!_  & nng_write_header - write header and set current properties (if necessary)
  subroutine nng_write_header &
       & (ierr, &
       &  head,  krect, u)
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(in)    :: head(*)
    integer,         intent(inout) :: krect
    integer,         intent(in)    :: u

    ierr = err_default
    if (ierr.eq.0) call set_wrecord_prop(ierr, krect, u)
    if (ierr.eq.0) call put_header(ierr, head, u, krect)

  end subroutine nng_write_header

!!!_  & nng_read_data - read data block
  subroutine nng_read_data_d &
       & (ierr, &
       &  d,    ld, head, krect, u)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: ld
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u

    integer kfmt
    integer kaxs(3)
    real(kind=KRMIS) :: vmiss

    ierr = err_default
    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    if (ierr.eq.0) then
       call nng_read_data_core &
            & (ierr, &
            &  d,    ld, kfmt, kaxs, vmiss, krect, u)
    endif
    return
  end subroutine nng_read_data_d
  subroutine nng_read_data_f &
       & (ierr, &
       &  d,    ld, head, krect, u)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: ld
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u

    integer kfmt
    integer kaxs(3)
    real(kind=KRMIS) :: vmiss

    ierr = err_default
    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    if (ierr.eq.0) then
       call nng_read_data_core &
            & (ierr, &
            &  d,    ld, kfmt, kaxs, vmiss, krect, u)
    endif
    return
  end subroutine nng_read_data_f
  subroutine nng_read_data_i &
       & (ierr, &
       &  d,    ld, head, krect, u)
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(out) :: d(*)
    integer,           intent(in)  :: ld
    character(len=*),  intent(in)  :: head(*)
    integer,           intent(in)  :: krect
    integer,           intent(in)  :: u

    integer kfmt
    integer kaxs(3)
    real(kind=KRMIS) :: vmiss

    ierr = err_default
    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    if (ierr.eq.0) then
       call nng_read_data_core &
            & (ierr, &
            &  d,    ld, kfmt, kaxs, vmiss, krect, u)
    endif
    return
  end subroutine nng_read_data_i

!!!_  & nng_write_data - write data block
  subroutine nng_write_data_d &
       & (ierr, &
       &  d,    ld, head, krect, u, kopts)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(in)          :: d(*)
    integer,         intent(in)          :: ld
    character(len=*),intent(in)          :: head(*)
    integer,         intent(in)          :: krect
    integer,         intent(in)          :: u
    integer,         intent(in),optional :: kopts(:)

    integer kfmt
    integer kaxs(3)
    real(kind=KRMIS) :: vmiss

    ierr = err_default
    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    if (ierr.eq.0) then
       call nng_write_data_core &
            & (ierr, &
            &  d,    ld, kfmt, kaxs, vmiss, krect, u, kopts)
    endif
    return
  end subroutine nng_write_data_d
  subroutine nng_write_data_f &
       & (ierr, &
       &  d,    ld, head, krect, u, kopts)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(in)          :: d(*)
    integer,         intent(in)          :: ld
    character(len=*),intent(in)          :: head(*)
    integer,         intent(in)          :: krect
    integer,         intent(in)          :: u
    integer,         intent(in),optional :: kopts(:)

    integer kfmt
    integer kaxs(3)
    real(kind=KRMIS) :: vmiss

    ierr = err_default
    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    if (ierr.eq.0) then
       call nng_write_data_core &
            & (ierr, &
            &  d,    ld, kfmt, kaxs, vmiss, krect, u, kopts)
    endif
    return
  end subroutine nng_write_data_f
  subroutine nng_write_data_i &
       & (ierr, &
       &  d,    ld, head, krect, u, kopts)
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)         :: ierr
    integer(kind=KARG),intent(in)          :: d(*)
    integer,           intent(in)          :: ld
    character(len=*),  intent(in)          :: head(*)
    integer,           intent(in)          :: krect
    integer,           intent(in)          :: u
    integer,           intent(in),optional :: kopts(:)

    integer kfmt
    integer kaxs(3)
    real(kind=KRMIS) :: vmiss

    ierr = err_default
    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    if (ierr.eq.0) then
       call nng_write_data_core &
            & (ierr, &
            &  d,    ld, kfmt, kaxs, vmiss, krect, u, kopts)
    endif
    return
  end subroutine nng_write_data_i

!!!_  & nng_read_data_core - read data block
  subroutine nng_read_data_core_d &
       & (ierr, &
       &  d,    ld, kfmt, kaxs, vmiss, krect, u)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: ld
    integer,         intent(in)  :: kfmt
    integer,         intent(in)  :: kaxs(*)
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u

    integer n, nh, nk

    ierr = 0

    n = kaxs(1) * kaxs(2) * kaxs(3)
    if (n.gt.ld) then
       ierr = -1
       return
    endif

    select case (kfmt)
    case (GFMT_UR4)
       call get_data_frecord(ierr, d, n, u, krect)
    case (GFMT_UR8)
       call get_data_drecord(ierr, d, n, u, krect)
    case (GFMT_UI4)
       call get_data_irecord(ierr, d, n, u, krect)
    case (GFMT_MR4)
       call get_data_mr4(ierr, d, n, u, krect, vmiss)
    case (GFMT_MR8)
       call get_data_mr8(ierr, d, n, u, krect, vmiss)
    case (GFMT_MI4)
       call get_data_mi4(ierr, d, n, u, krect, vmiss)
    case (GFMT_URC, GFMT_URC2)
       nh = kaxs(1) * kaxs(2)
       nk = kaxs(3)
       call get_data_urc &
            & (ierr, d, nh, nk, u, krect, vmiss, IMISS_URC, kfmt)
    case (GFMT_URY:GFMT_URYend)
       nh = kaxs(1) * kaxs(2)
       nk = kaxs(3)
       call get_data_ury &
            & (ierr, d, nh, nk, u, krect, vmiss, kfmt)
    case (GFMT_MRY:GFMT_MRYend)
       nh = kaxs(1) * kaxs(2)
       nk = kaxs(3)
       call get_data_mry &
            & (ierr, d, nh, nk, u, krect, vmiss, kfmt)
    case (GFMT_URT:GFMT_URTend)
       call get_data_urt &
            & (ierr, d, n, kaxs, u, krect, vmiss, kfmt)
    case (GFMT_MRT:GFMT_MRTend)
       call get_data_mrt &
            & (ierr, d, n, kaxs, u, krect, vmiss, kfmt)
    case default
       ierr = -1
    end select

    return
  end subroutine nng_read_data_core_d
  subroutine nng_read_data_core_f &
       & (ierr, &
       &  d,    ld, kfmt, kaxs, vmiss, krect, u)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: ld
    integer,         intent(in)  :: kfmt
    integer,         intent(in)  :: kaxs(*)
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u

    integer n, nh, nk

    ierr = 0
    n = kaxs(1) * kaxs(2) * kaxs(3)
    if (n.gt.ld) then
       ierr = -1
       return
    endif

    select case (kfmt)
    case (GFMT_UR4)
       call get_data_frecord(ierr, d, n, u, krect)
    case (GFMT_UR8)
       call get_data_drecord(ierr, d, n, u, krect)
    case (GFMT_UI4)
       call get_data_irecord(ierr, d, n, u, krect)
    case (GFMT_MR4)
       call get_data_mr4(ierr, d, n, u, krect, vmiss)
    case (GFMT_MR8)
       call get_data_mr8(ierr, d, n, u, krect, vmiss)
    case (GFMT_MI4)
       call get_data_mi4(ierr, d, n, u, krect, vmiss)
    case (GFMT_URC, GFMT_URC2)
       nh = kaxs(1) * kaxs(2)
       nk = kaxs(3)
       call get_data_urc &
            & (ierr, d, nh, nk, u, krect, vmiss, IMISS_URC, kfmt)
    case (GFMT_URY:GFMT_URYend)
       nh = kaxs(1) * kaxs(2)
       nk = kaxs(3)
       call get_data_ury &
            & (ierr, d, nh, nk, u, krect, vmiss, kfmt)
    case (GFMT_MRY:GFMT_MRYend)
       nh = kaxs(1) * kaxs(2)
       nk = kaxs(3)
       call get_data_mry &
            & (ierr, d, nh, nk, u, krect, vmiss, kfmt)
    case default
       ierr = -1
    end select

    return
  end subroutine nng_read_data_core_f
  subroutine nng_read_data_core_i &
       & (ierr, &
       &  d,    ld, kfmt, kaxs, vmiss, krect, u)
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(out) :: d(*)
    integer,           intent(in)  :: ld
    integer,           intent(in)  :: kfmt
    integer,           intent(in)  :: kaxs(*)
    real(kind=KRMIS),  intent(in)  :: vmiss
    integer,           intent(in)  :: krect
    integer,           intent(in)  :: u

    integer n, nh, nk

    ierr = 0
    n = kaxs(1) * kaxs(2) * kaxs(3)
    if (n.gt.ld) then
       ierr = -1
       return
    endif

    select case (kfmt)
    case (GFMT_UR4)
       call get_data_frecord(ierr, d, n, u, krect)
    case (GFMT_UR8)
       call get_data_drecord(ierr, d, n, u, krect)
    case (GFMT_UI4)
       call get_data_irecord(ierr, d, n, u, krect)
    case (GFMT_MR4)
       call get_data_mr4(ierr, d, n, u, krect, vmiss)
    case (GFMT_MR8)
       call get_data_mr8(ierr, d, n, u, krect, vmiss)
    case (GFMT_MI4)
       call get_data_mi4(ierr, d, n, u, krect, vmiss)
    case (GFMT_URC, GFMT_URC2)
       nh = kaxs(1) * kaxs(2)
       nk = kaxs(3)
       call get_data_urc &
            & (ierr, d, nh, nk, u, krect, vmiss, IMISS_URC, kfmt)
    case (GFMT_URY:GFMT_URYend)
       nh = kaxs(1) * kaxs(2)
       nk = kaxs(3)
       call get_data_ury &
            & (ierr, d, nh, nk, u, krect, vmiss, kfmt)
    case (GFMT_MRY:GFMT_MRYend)
       nh = kaxs(1) * kaxs(2)
       nk = kaxs(3)
       call get_data_mry &
            & (ierr, d, nh, nk, u, krect, vmiss, kfmt)
    case default
       ierr = -1
    end select

    return
  end subroutine nng_read_data_core_i

!!!_  & nng_write_data_core - write data block
  subroutine nng_write_data_core_d &
       & (ierr, &
       &  d,    ld, kfmt, kaxs, vmiss, krect, u, kopts)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(in)          :: d(*)
    integer,         intent(in)          :: ld
    integer,         intent(in)          :: kfmt
    integer,         intent(in)          :: kaxs(*)
    real(kind=KRMIS),intent(in)          :: vmiss
    integer,         intent(in)          :: krect
    integer,         intent(in)          :: u
    integer,         intent(in),optional :: kopts(:)

    integer n, nh, nk

    ierr = 0
    n = kaxs(1) * kaxs(2) * kaxs(3)
    if (n.gt.ld) then
       ierr = -1
       return
    endif

    select case (kfmt)
    case (GFMT_UR4)
       call put_data_frecord(ierr, d, n, u, krect)
    case (GFMT_UR8)
       call put_data_drecord(ierr, d, n, u, krect)
    case (GFMT_UI4)
       call put_data_irecord(ierr, d, n, u, krect)
    case (GFMT_MR4)
       call put_data_mr4(ierr, d, n, u, krect, vmiss)
    case (GFMT_MR8)
       call put_data_mr8(ierr, d, n, u, krect, vmiss)
    case (GFMT_MI4)
       call put_data_mi4(ierr, d, n, u, krect, vmiss)
    case (GFMT_URC, GFMT_URC2)
       ierr = -1
    case (GFMT_URY:GFMT_URYend)
       nh = kaxs(1) * kaxs(2)
       nk = kaxs(3)
       call put_data_ury &
            & (ierr, d, nh, nk, u, krect, vmiss, kfmt)
    case (GFMT_MRY:GFMT_MRYend)
       nh = kaxs(1) * kaxs(2)
       nk = kaxs(3)
       call put_data_mry &
            & (ierr, d, nh, nk, u, krect, vmiss, kfmt)
    case (GFMT_URT:GFMT_URTend)
       call put_data_urt_plain &
            & (ierr, d, n, u, krect, vmiss, kfmt, kopts)
    case (GFMT_MRT:GFMT_MRTend)
       call put_data_mrt_plain &
            & (ierr, d, n, u, krect, vmiss, kfmt, kopts)
    case default
       ierr = -1
    end select

    return
  end subroutine nng_write_data_core_d
  subroutine nng_write_data_core_f &
       & (ierr, &
       &  d,    ld, kfmt, kaxs, vmiss, krect, u, kopts)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(in)          :: d(*)
    integer,         intent(in)          :: ld
    integer,         intent(in)          :: kfmt
    integer,         intent(in)          :: kaxs(*)
    real(kind=KRMIS),intent(in)          :: vmiss
    integer,         intent(in)          :: krect
    integer,         intent(in)          :: u
    integer,         intent(in),optional :: kopts(:)

    integer n, nh, nk

    ierr = 0
    n = kaxs(1) * kaxs(2) * kaxs(3)
    if (n.gt.ld) then
       ierr = -1
       return
    endif

    select case (kfmt)
    case (GFMT_UR4)
       call put_data_frecord(ierr, d, n, u, krect)
    case (GFMT_UR8)
       call put_data_drecord(ierr, d, n, u, krect)
    case (GFMT_UI4)
       call put_data_irecord(ierr, d, n, u, krect)
    case (GFMT_MR4)
       call put_data_mr4(ierr, d, n, u, krect, vmiss)
    case (GFMT_MR8)
       call put_data_mr8(ierr, d, n, u, krect, vmiss)
    case (GFMT_MI4)
       call put_data_mi4(ierr, d, n, u, krect, vmiss)
    case (GFMT_URC, GFMT_URC2)
       ierr = -1
    case (GFMT_URY:GFMT_URYend)
       nh = kaxs(1) * kaxs(2)
       nk = kaxs(3)
       call put_data_ury &
            & (ierr, d, nh, nk, u, krect, vmiss, kfmt)
    case (GFMT_MRY:GFMT_MRYend)
       nh = kaxs(1) * kaxs(2)
       nk = kaxs(3)
       call put_data_mry &
            & (ierr, d, nh, nk, u, krect, vmiss, kfmt)
    case default
       ierr = -1
    end select

    return
  end subroutine nng_write_data_core_f
  subroutine nng_write_data_core_i &
       & (ierr, &
       &  d,    ld, kfmt, kaxs, vmiss, krect, u, kopts)
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)         :: ierr
    integer(kind=KARG),intent(in)          :: d(*)
    integer,           intent(in)          :: ld
    integer,           intent(in)          :: kfmt
    integer,           intent(in)          :: kaxs(*)
    real(kind=KRMIS),  intent(in)          :: vmiss
    integer,           intent(in)          :: krect
    integer,           intent(in)          :: u
    integer,           intent(in),optional :: kopts(:)

    integer n, nh, nk

    ierr = 0
    n = kaxs(1) * kaxs(2) * kaxs(3)
    if (n.gt.ld) then
       ierr = -1
       return
    endif

    select case (kfmt)
    case (GFMT_UR4)
       call put_data_frecord(ierr, d, n, u, krect)
    case (GFMT_UR8)
       call put_data_drecord(ierr, d, n, u, krect)
    case (GFMT_UI4)
       call put_data_irecord(ierr, d, n, u, krect)
    case (GFMT_MR4)
       call put_data_mr4(ierr, d, n, u, krect, vmiss)
    case (GFMT_MR8)
       call put_data_mr8(ierr, d, n, u, krect, vmiss)
    case (GFMT_MI4)
       call put_data_mi4(ierr, d, n, u, krect, vmiss)
    case (GFMT_URC, GFMT_URC2)
       ierr = -1
    case (GFMT_URY:GFMT_URYend)
       nh = kaxs(1) * kaxs(2)
       nk = kaxs(3)
       call put_data_ury &
            & (ierr, d, nh, nk, u, krect, vmiss, kfmt)
    case (GFMT_MRY:GFMT_MRYend)
       nh = kaxs(1) * kaxs(2)
       nk = kaxs(3)
       call put_data_mry &
            & (ierr, d, nh, nk, u, krect, vmiss, kfmt)
    case default
       ierr = -1
    end select

    return
  end subroutine nng_write_data_core_i

!!!_  & nng_skip_records - forward record skip
  subroutine nng_skip_records &
       & (ierr, n, u)
    use TOUZA_Nng_io,only: WHENCE_CURRENT, ssq_skip_irec, ssq_skip_lrec
    use TOUZA_Nng_header,only: &
         & nitem, litem, hi_DFMT, get_item
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: n
    integer,intent(in)  :: u

    integer j, krect
    character(len=litem) :: head(nitem)
    character(len=litem) :: vp
    integer kfmt
    integer nrec
    logical swap, lrec

    if (n .lt. 0) then
       ierr = ERR_NOT_IMPLEMENTED
       return
    endif

    ierr = 0
    do j = 0, n - 1
       if (ierr.eq.0) call nng_read_header(ierr, head, krect, u)
       if (ierr.eq.0) call get_item(ierr, head, vp, hi_DFMT)
       if (ierr.eq.0) call parse_record_fmt(ierr, kfmt, vp)
       if (ierr.eq.0) then
          select case (kfmt)
          case (GFMT_UR4, GFMT_UR8, GFMT_UI4)
             nrec = 1
          case (GFMT_MR4, GFMT_MR8, GFMT_MI4)
             nrec = 3
          case (GFMT_URC, GFMT_URC2)
             nrec = parse_header_size(head, 3)
             if (nrec.le.0) then
                ierr = -1
             else
                nrec = nrec * 4
             endif
          case (GFMT_URY:GFMT_URYend)
             nrec = 2
          case (GFMT_MRY:GFMT_MRYend)
             nrec = 6
          case (GFMT_URT:GFMT_URTend)
             nrec = 1
          case (GFMT_MRT:GFMT_MRTend)
             nrec = 1
          case default
             ierr = -1
          end select
       endif
       if (ierr.eq.0) then
          swap = IAND(krect, REC_SWAP).ne.0
          lrec = IAND(krect, REC_LSEP).ne.0
          if (lrec) then
             call ssq_skip_lrec(ierr, u, nrec, WHENCE_CURRENT, swap=swap)
          else
             call ssq_skip_irec(ierr, u, nrec, WHENCE_CURRENT, swap=swap)
          endif
       endif
    enddo
    return
  end subroutine nng_skip_records

!!!_ + gtool-3 standard formats
!!!_  - get_data_urc - URC
  subroutine get_data_urc_d &
       & (ierr, &
       &  d, nh, nk, u, krect, vmiss, imiss, kfmt)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,            intent(out) :: ierr
    integer,            intent(in)  :: nh, nk
    real(kind=KARG),    intent(out) :: d(*)
    integer,            intent(in)  :: krect
    integer,            intent(in)  :: u
    integer(kind=KISRC),intent(in)  :: imiss
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kfmt
    integer jk
    integer mh
    integer(kind=KISRC) :: nd, ne
    integer(kind=KISRC) :: icom((nh + 1)/2)
    integer(kind=KISRC) :: idec(nh+1)
    real(kind=KRSRC)    :: vmin, f1, f2, bs
    integer,parameter   :: mbits = BIT_SIZE(0_KISRC) / 2
    integer jdb, jde
    integer kpack

    ierr = 0
    mh = (nh + 1) / 2
    do jk = 1, nk
       if (ierr.eq.0) call get_data_record(ierr, vmin,  u, krect)
       if (ierr.eq.0) call get_data_record(ierr, nd,    u, krect)
       if (ierr.eq.0) call get_data_record(ierr, ne,    u, krect)
       if (ierr.eq.0) call get_data_record(ierr, icom, mh, u, krect)
       if (ierr.eq.0) then
          kpack = legacy_unpacking(mbits, nh)
          call pack_restore(ierr, idec, icom, nh, mbits, kpack)
       endif
       if (ierr.eq.0) then
          jdb = nh * (jk - 1) + 1
          jde = nh * jk
          if (ne.eq.imiss) then
             where(idec(1:nh).eq.imiss)
                d(jdb:jde) = vmiss
             elsewhere
                d(jdb:jde) = vmin
             end where
          else
             f1 = 10.0_KRSRC ** (-nd)
             f2 = 2.0_KRSRC ** ne
             if (kfmt.eq.GFMT_URC) then
                bs = f1 * f2 * 0.5_KRSRC
             else
                bs = 0.0_KRSRC
             endif
             where(idec(1:nh).eq.imiss)
                d(jdb:jde) = vmiss
             elsewhere
                d(jdb:jde) = (idec(1:nh) * f2 + vmin) * f1 + bs
             endwhere
          endif
       endif
    enddo
    return
  end subroutine get_data_urc_d
  subroutine get_data_urc_f &
       & (ierr, &
       &  d, nh, nk, u, krect, vmiss, imiss, kfmt)
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KDBL, KRBUF=KDBL
    integer,            intent(out) :: ierr
    integer,            intent(in)  :: nh, nk
    real(kind=KARG),    intent(out) :: d(*)
    integer,            intent(in)  :: krect
    integer,            intent(in)  :: u
    integer(kind=KISRC),intent(in)  :: imiss
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kfmt
    real(kind=KRBUF) :: b(nh * nk)
    integer n

    ierr = 0
    n = nh * nk
    call get_data_urc_d &
         & (ierr, &
         &  b, nh, nk, u, krect, vmiss, imiss, kfmt)
    if (ierr.eq.0) d(1:n) = real(b(1:n), KIND=KARG)

    return
  end subroutine get_data_urc_f
  subroutine get_data_urc_i &
       & (ierr, &
       &  d, nh, nk, u, krect, vmiss, imiss, kfmt)
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32, KRSRC=KDBL, KRBUF=KDBL
    integer,            intent(out) :: ierr
    integer,            intent(in)  :: nh, nk
    integer(kind=KARG), intent(out) :: d(*)
    integer,            intent(in)  :: krect
    integer,            intent(in)  :: u
    integer(kind=KISRC),intent(in)  :: imiss
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kfmt
    real(kind=KRBUF) :: b(nh * nk)
    integer n

    ierr = 0
    n = nh * nk
    call get_data_urc_d &
         & (ierr, &
         &  b, nh, nk, u, krect, vmiss, imiss, kfmt)
    if (ierr.eq.0) d(1:n) = int(b(1:n), KIND=KARG)

    return
  end subroutine get_data_urc_i

!!!_  - put_data_urc - URC
  subroutine put_data_urc_d &
       & (ierr, &
       &  d, nh, nk, u, krect, vmiss, imiss, kfmt)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,            intent(out) :: ierr
    integer,            intent(in)  :: nh, nk
    real(kind=KARG),    intent(in)  :: d(*)
    integer,            intent(in)  :: krect
    integer,            intent(in)  :: u
    integer(kind=KISRC),intent(in)  :: imiss
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kfmt
    integer jk
    integer mh
    integer(kind=KISRC) :: nd, ne
    integer(kind=KISRC) :: icom((nh + 1)/2)
    integer(kind=KISRC) :: idec(nh+1)
    real(kind=KRSRC)    :: vmin, f1, f2, bs
    integer,parameter   :: mbits = BIT_SIZE(0_KISRC) / 2
    integer jdb, jde

    ierr = -1
    return
  end subroutine put_data_urc_d
  subroutine put_data_urc_f &
       & (ierr, &
       &  d, nh, nk, u, krect, vmiss, imiss, kfmt)
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KDBL, KRBUF=KDBL
    integer,            intent(out) :: ierr
    integer,            intent(in)  :: nh, nk
    real(kind=KARG),    intent(in)  :: d(*)
    integer,            intent(in)  :: krect
    integer,            intent(in)  :: u
    integer(kind=KISRC),intent(in)  :: imiss
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kfmt
    real(kind=KRBUF) :: b(nh * nk)
    integer n

    ierr = 0
    n = nh * nk
    b(1:n) = real(d(1:n), KIND=KRBUF)
    call put_data_urc_d &
         & (ierr, &
         &  b, nh, nk, u, krect, vmiss, imiss, kfmt)
    return
  end subroutine put_data_urc_f
  subroutine put_data_urc_i &
       & (ierr, &
       &  d, nh, nk, u, krect, vmiss, imiss, kfmt)
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32, KRSRC=KDBL, KRBUF=KDBL
    integer,            intent(out) :: ierr
    integer,            intent(in)  :: nh, nk
    integer(kind=KARG), intent(in)  :: d(*)
    integer,            intent(in)  :: krect
    integer,            intent(in)  :: u
    integer(kind=KISRC),intent(in)  :: imiss
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kfmt
    real(kind=KRBUF) :: b(nh * nk)
    integer n

    ierr = 0
    n = nh * nk
    b(1:n) = real(d(1:n), KIND=KRBUF)
    call put_data_urc_d &
         & (ierr, &
         &  b, nh, nk, u, krect, vmiss, imiss, kfmt)
    return
  end subroutine put_data_urc_i

!!!_  - get_data_ury - URY
  subroutine get_data_ury_d &
       & (ierr, &
       &  d, nh, nk, u, krect, vmiss, kfmt)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: nh, nk
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt

    integer             :: nbits, mcom
    integer             :: idec(nh)
    integer(kind=KISRC) :: icom(nh * nk)
    integer(kind=KISRC) :: imiss
    real(kind=KRSRC)    :: dma(2 * nk)
    integer,parameter   :: khld = 0_KISRC
    integer             :: jk, jdb, jde, jc
    integer kpack

    ierr = 0
    nbits = kfmt - GFMT_URY
    mcom  = count_packed(nbits, nh, khld)
    imiss = ISHFT(1, nbits) - 1
    call get_data_record(ierr, dma, 2 * nk, u, krect)
    if (ierr.eq.0) call get_data_record(ierr, icom, mcom * nk, u, krect)
    if (ierr.eq.0) then
       kpack = legacy_unpacking(nbits, nh)
       do jk = 1, nk
          jc = mcom * (jk - 1) + 1
          call pack_restore(ierr, idec, icom(jc:), nh, nbits, kpack)
          jdb = nh * (jk - 1) + 1
          jde = nh * jk
          where (idec(1:nh).ne.imiss)
             d(jdb:jde) = dma(2 * jk - 1) + real(idec(1:nh), kind=KARG) * dma(2 * jk)
          elsewhere
             d(jdb:jde) = vmiss
          end where
       enddo
    endif
    return
  end subroutine get_data_ury_d
  subroutine get_data_ury_f &
       & (ierr, &
       &  d, nh, nk, u, krect, vmiss, kfmt)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: nh, nk
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt

    integer             :: nbits, mcom
    integer             :: idec(nh)
    integer(kind=KISRC) :: icom(nh * nk)
    integer(kind=KISRC) :: imiss
    real(kind=KRSRC)    :: dma(2 * nk)
    integer,parameter   :: khld = 0_KISRC
    integer             :: jk, jdb, jde
    integer kpack

    ierr = 0
    nbits = kfmt - GFMT_URY
    mcom  = count_packed(nbits, nh, khld)
    imiss = ISHFT(1, nbits) - 1

    call get_data_record(ierr, dma, 2 * nk, u, krect)
    if (ierr.eq.0) call get_data_record(ierr, icom, mcom * nk, u, krect)
    if (ierr.eq.0) then
       kpack = legacy_unpacking(nbits, nh)
       do jk = 1, nk
          call pack_restore(ierr, idec, icom, nh, nbits, kpack)
          jdb = nh * (jk - 1) + 1
          jde = nh * jk
          where (idec(1:nh).ne.imiss)
             d(jdb:jde) = real(dma(2 * jk - 1) + real(idec(1:nh), kind=KRSRC) * dma(2 * jk), &
                  &            kind=KARG)
          elsewhere
             d(jdb:jde) = real(vmiss, kind=KARG)
          end where
       enddo
    endif
    return
  end subroutine get_data_ury_f
  subroutine get_data_ury_i &
       & (ierr, &
       &  d, nh, nk, u, krect, vmiss, kfmt)
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32, KRSRC=KDBL
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(out) :: d(*)
    integer,           intent(in)  :: nh, nk
    integer,           intent(in)  :: krect
    integer,           intent(in)  :: u
    real(kind=KRMIS),  intent(in)  :: vmiss
    integer,           intent(in)  :: kfmt

    real(kind=KRSRC) :: b(nh * nk)
    integer n

    ierr = 0
    n = nh * nk
    call get_data_ury_d &
         & (ierr, &
         &  b, nh, nk, u, krect, vmiss, kfmt)
    if (ierr.eq.0) d(1:n) = int(b(1:n), KIND=KARG)

    return
  end subroutine get_data_ury_i

!!!_  - put_data_ury - URY
  subroutine put_data_ury_d &
       & (ierr, &
       &  d, nh, nk, u, krect, vmiss, kfmt)
    use TOUZA_Trp,only: count_packed, pack_store
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(*)
    integer,         intent(in)  :: nh, nk
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt

    integer             :: nbits, mcom
    integer             :: idec(nh)
    integer(kind=KISRC) :: icom(nh * nk)
    integer(kind=KISRC) :: imiss
    real(kind=KRSRC)    :: dma(2 * nk)
    integer,parameter   :: khld = 0_KISRC
    integer             :: jk, jdb, jde, jc
    integer kpack

    ierr = 0
    nbits = kfmt - GFMT_URY
    mcom  = count_packed(nbits, nh, khld)
    imiss = IBITS(HUGE(0_KISRC), 0, nbits)
    kpack = legacy_packing(nbits, nh)
    do jk = 1, nk
       jc = mcom * (jk - 1) + 1
       jdb = nh * (jk - 1) + 1
       jde = nh * jk
       call normalize_xry &
            & (ierr, idec, dma(2*jk-1:2*jk), d(jdb:jde), nh, imiss, vmiss)
       call pack_store &
            & (ierr, icom(jc:), idec, nh, nbits, kpack)
    enddo
    call put_data_record(ierr, dma, 2 * nk, u, krect)
    if (ierr.eq.0) call put_data_record(ierr, icom, mcom * nk, u, krect)
    return
  end subroutine put_data_ury_d
  subroutine put_data_ury_f &
       & (ierr, &
       &  d, nh, nk, u, krect, vmiss, kfmt)
    use TOUZA_Trp,only: count_packed, pack_store
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(*)
    integer,         intent(in)  :: nh, nk
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt

    integer             :: nbits, mcom
    integer             :: idec(nh)
    integer(kind=KISRC) :: icom(nh * nk)
    integer(kind=KISRC) :: imiss
    real(kind=KRSRC)    :: dma(2 * nk)
    integer,parameter   :: khld = 0_KISRC
    integer             :: jk, jdb, jde, jc
    real(kind=KRSRC)    :: buf(nh)
    integer kpack

    ierr = 0
    nbits = kfmt - GFMT_URY
    mcom  = count_packed(nbits, nh, khld)
    imiss = IBITS(HUGE(0_KISRC), 0, nbits)
    kpack = legacy_packing(nbits, nh)
    do jk = 1, nk
       jc = mcom * (jk - 1) + 1
       jdb = nh * (jk - 1) + 1
       jde = nh * jk
       buf(1:nh) = real(d(jdb:jde),kind=KRSRC)
       call normalize_xry &
            & (ierr, idec, dma(2*jk-1:2*jk), buf, nh, imiss, vmiss)
       call pack_store &
            & (ierr, icom(jc:), idec, nh, nbits, kpack)
    enddo
    call put_data_record(ierr, dma, 2 * nk, u, krect)
    if (ierr.eq.0) call put_data_record(ierr, icom, mcom * nk, u, krect)
    return
  end subroutine put_data_ury_f
  subroutine put_data_ury_i &
       & (ierr, &
       &  d, nh, nk, u, krect, vmiss, kfmt)
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32, KRSRC=KDBL, KRBUF=KDBL
    integer,            intent(out) :: ierr
    integer,            intent(in)  :: nh, nk
    integer(kind=KARG), intent(in)  :: d(*)
    integer,            intent(in)  :: krect
    integer,            intent(in)  :: u
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kfmt
    real(kind=KRBUF) :: b(nh * nk)
    integer n

    ierr = 0
    n = nh * nk
    b(1:n) = real(d(1:n), KIND=KRBUF)
    call put_data_ury_d &
         & (ierr, &
         &  b, nh, nk, u, krect, vmiss, kfmt)
    return
  end subroutine put_data_ury_i

!!!_  - get_data_mr8 - MR8
  subroutine get_data_mr8_d &
       & (ierr, &
       &  d, n, u, krect, vmiss)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer(kind=KISRC) :: mb
    integer(kind=KISRC) :: icom(n)
    real(kind=KRSRC)    :: buf(n)
    integer(kind=KISRC),parameter :: khld = 0_KISRC
    integer ncom
    integer kpack

    ierr = 0
    ncom = count_packed(1, n, khld)
    call get_data_record(ierr, mb, u, krect)
    if (ierr.eq.0) call get_data_record(ierr, icom, ncom, u, krect)
    if (ierr.eq.0) call get_data_record(ierr, buf,  mb,   u, krect)

    if (ierr.eq.0) then
       kpack = legacy_unpacking(1, n)
       call mask_decode &
            & (ierr,  d, n, buf, icom, vmiss, kpack)
    endif

    return
  end subroutine get_data_mr8_d
  subroutine get_data_mr8_f &
       & (ierr, &
       &  d, n, u, krect, vmiss)
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss

    real(kind=KRSRC) :: buf(n)

    ierr = 0
    call get_data_mr8_d(ierr, buf, n, u, krect, vmiss)

    if (ierr.eq.0) d(1:n) = real(buf(1:n), KIND=KARG)
    return
  end subroutine get_data_mr8_f
  subroutine get_data_mr8_i &
       & (ierr, &
       &  d, n, u, krect, vmiss)
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32, KRSRC=KDBL
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(out) :: d(*)
    integer,           intent(in)  :: n
    integer,           intent(in)  :: krect
    integer,           intent(in)  :: u
    real(kind=KRMIS),  intent(in)  :: vmiss

    real(kind=KRSRC) :: buf(n)

    ierr = 0
    call get_data_mr8_d(ierr, buf, n, u, krect, vmiss)

    if (ierr.eq.0) d(1:n) = int(buf(1:n), KIND=KARG)
    return
  end subroutine get_data_mr8_i

!!!_  - put_data_mr8 - MR8
  subroutine put_data_mr8_d &
       & (ierr, &
       &  d, n, u, krect, vmiss)
    use TOUZA_Trp,only: count_packed, pack_store
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer(kind=KISRC) :: mb
    integer(kind=KISRC) :: icom(n)
    real(kind=KRSRC)    :: buf(n)
    integer(kind=KISRC),parameter :: khld = 0_KISRC
    integer ncom
    integer kpack

    ierr = 0
    kpack = legacy_packing(1, n)

    call mask_encode &
       & (ierr, mb,   ncom,   icom,   buf, &
       &  d,    n,    vmiss,  kpack)

    if (ierr.eq.0) call put_data_record(ierr, mb, u, krect)
    if (ierr.eq.0) call put_data_record(ierr, icom, ncom, u, krect)
    if (ierr.eq.0) call put_data_record(ierr, buf,  mb,   u, krect)

    return
  end subroutine put_data_mr8_d
  subroutine put_data_mr8_f &
       & (ierr, &
       &  d, n, u, krect, vmiss)
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss

    real(kind=KRSRC) :: buf(n)

    ierr = 0
    buf(1:n) = real(d(1:n), KIND=KRSRC)
    call put_data_mr8_d(ierr, buf, n, u, krect, vmiss)

    return
  end subroutine put_data_mr8_f
  subroutine put_data_mr8_i &
       & (ierr, &
       &  d, n, u, krect, vmiss)
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32, KRSRC=KDBL
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(in)  :: d(*)
    integer,           intent(in)  :: n
    integer,           intent(in)  :: krect
    integer,           intent(in)  :: u
    real(kind=KRMIS),  intent(in)  :: vmiss

    real(kind=KRSRC) :: buf(n)

    ierr = 0
    buf(1:n) = real(d(1:n), KIND=KRSRC)
    call put_data_mr8_d(ierr, buf, n, u, krect, vmiss)

    return
  end subroutine put_data_mr8_i

!!!_  - get_data_mr4 - MR4
  subroutine get_data_mr4_f &
       & (ierr, &
       &  d, n, u, krect, vmiss)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer(kind=KISRC) :: mb
    integer(kind=KISRC) :: icom(n)
    real(kind=KRSRC)    :: buf(n)
    integer(kind=KISRC),parameter :: khld = 0_KISRC
    integer ncom
    integer kpack

    ierr = 0
    ncom = count_packed(1, n, khld)
    call get_data_record(ierr, mb, u, krect)
    if (ierr.eq.0) call get_data_record(ierr, icom, ncom, u, krect)
    if (ierr.eq.0) call get_data_record(ierr, buf,  mb,   u, krect)

    if (ierr.eq.0) then
       kpack = legacy_unpacking(1, n)
       call mask_decode &
            & (ierr,  d, n, buf, icom, vmiss, kpack)
    endif
    return
  end subroutine get_data_mr4_f
  subroutine get_data_mr4_d &
       & (ierr, &
       &  d, n, u, krect, vmiss)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss

    real(kind=KRSRC) :: buf(n)

    ierr = 0
    call get_data_mr4_f(ierr, buf, n, u, krect, vmiss)

    if (ierr.eq.0) d(1:n) = real(buf(1:n), KIND=KARG)

    return
  end subroutine get_data_mr4_d
  subroutine get_data_mr4_i &
       & (ierr, &
       &  d, n, u, krect, vmiss)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32, KRSRC=KFLT
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(out) :: d(*)
    integer,           intent(in)  :: n
    integer,           intent(in)  :: krect
    integer,           intent(in)  :: u
    real(kind=KRMIS),  intent(in)  :: vmiss

    real(kind=KRSRC) :: buf(n)
    ierr = 0
    call get_data_mr4_f(ierr, buf, n, u, krect, vmiss)

    if (ierr.eq.0) d(1:n) = int(buf(1:n), KIND=KARG)

    return
  end subroutine get_data_mr4_i

!!!_  - put_data_mr4 - MR4
  subroutine put_data_mr4_f &
       & (ierr, &
       &  d, n, u, krect, vmiss)
    use TOUZA_Trp,only: count_packed, pack_store
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss

    integer(kind=KISRC) :: mb
    integer(kind=KISRC) :: icom(n)
    real(kind=KRSRC)    :: buf(n)
    integer ncom
    integer kpack

    ierr = 0
    kpack = legacy_packing(1, n)
    call mask_encode &
       & (ierr, mb,   ncom,   icom,   buf, &
       &  d,    n,    vmiss,  kpack)

    if (ierr.eq.0) call put_data_record(ierr, mb, u, krect)
    if (ierr.eq.0) call put_data_record(ierr, icom, ncom, u, krect)
    if (ierr.eq.0) call put_data_record(ierr, buf,  mb,   u, krect)

    return
  end subroutine put_data_mr4_f
  subroutine put_data_mr4_d &
       & (ierr, &
       &  d, n, u, krect, vmiss)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss

    real(kind=KRSRC) :: buf(n)

    ierr = 0
    buf(1:n) = real(d(1:n), KIND=KRSRC)
    call put_data_mr4_f(ierr, buf, n, u, krect, vmiss)

    return
  end subroutine put_data_mr4_d
  subroutine put_data_mr4_i &
       & (ierr, &
       &  d, n, u, krect, vmiss)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32, KRSRC=KFLT
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(in)  :: d(*)
    integer,           intent(in)  :: n
    integer,           intent(in)  :: krect
    integer,           intent(in)  :: u
    real(kind=KRMIS),  intent(in)  :: vmiss

    real(kind=kRSRC) :: buf(n)

    ierr = 0
    buf(1:n) = real(d(1:n), KIND=KRSRC)
    call put_data_mr4_f(ierr, buf, n, u, krect, vmiss)

    return
  end subroutine put_data_mr4_i

!!!_  - get_data_mry - MRY
  subroutine get_data_mry_d &
       & (ierr, &
       &  d, nh, nk, u, krect, vmiss, kfmt)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: nh, nk
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt

    integer             :: nm
    integer             :: imsk(nh * nk)
    integer(kind=KISRC) :: imco(nh * nk)

    integer             :: mch(nk)
    integer             :: mofs(0:nk+1)

    integer             :: nbits, mcom
    integer             :: idec(nh)
    integer(kind=KISRC) :: icom(nh * nk)
    integer(kind=KISRC) :: imiss
    real(kind=KRSRC)    :: dma(2 * nk)
    integer,parameter   :: khld = 0_KISRC
    integer             :: jk, jdb, jde, jm, jc, jh, jb
    integer kpackm, kpackb

    ierr = 0

    nbits = kfmt - GFMT_MRY

    nm = count_packed(1, nh, khld)
    mofs(0) = 0

    if (ierr.eq.0) call get_data_record(ierr, mcom,     u,          krect)
    if (ierr.eq.0) call get_data_record(ierr, mch,      nk,      u, krect)
    if (ierr.eq.0) call get_data_record(ierr, mofs(2:), nk,      u, krect)
    if (ierr.eq.0) call get_data_record(ierr, dma,      2 * nk,  u, krect)
    if (ierr.eq.0) call get_data_record(ierr, imco,     nm * nk, u, krect)
    if (ierr.eq.0) call get_data_record(ierr, icom,     mcom,    u, krect)

    if (ierr.eq.0) then
       do jk = 1, nk
          mofs(jk) = mofs(jk-1) + mofs(jk+1)
       enddo
       kpackm = legacy_unpacking(1,     nh)
       kpackb = legacy_unpacking(nbits, nh)
       do jk = 1, nk
          jm = (jk - 1) * nm + 1
          jc = 1 + mofs(jk-1)
          call pack_restore(ierr, imsk, imco(jm:), nh, 1, kpackm)
          call pack_restore(ierr, idec, icom(jc:), mch(jk), nbits, kpackb)
          jb = 1
          jdb = (jk - 1) * nh
          do jh = 1, nh
             if (imsk(jh).eq.1) then
                d(jdb + jh) = dma(2 * jk - 1) &
                     & + real(idec(jb), kind=KARG) * dma(2 * jk)
                jb = jb + 1
             else
                d(jdb + jh) = vmiss
             endif
          enddo
       enddo
    endif
    return
  end subroutine get_data_mry_d
  subroutine get_data_mry_f &
       & (ierr, &
       &  d, nh, nk, u, krect, vmiss, kfmt)
    implicit none
    integer,parameter :: KARG=KFLT, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: nh, nk
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt

    real(kind=KRSRC) :: buf(nh * nk)
    integer n

    ierr = 0
    n = nh * nk
    call get_data_mry_d(ierr, buf, nh, nk, u, krect, vmiss, kfmt)
    if (ierr.eq.0) d(1:n) = real(buf(1:n), KIND=KARG)

    return
  end subroutine get_data_mry_f
  subroutine get_data_mry_i &
       & (ierr, &
       &  d, nh, nk, u, krect, vmiss, kfmt)
    implicit none
    integer,parameter :: KARG=KI32, KRSRC=KDBL
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(out) :: d(*)
    integer,           intent(in)  :: nh, nk
    integer,           intent(in)  :: krect
    integer,           intent(in)  :: u
    real(kind=KRMIS),  intent(in)  :: vmiss
    integer,           intent(in)  :: kfmt

    real(kind=KRSRC) :: buf(nh * nk)
    integer n

    ierr = 0
    n = nh * nk
    call get_data_mry_d(ierr, buf, nh, nk, u, krect, vmiss, kfmt)
    if (ierr.eq.0) d(1:n) = int(buf(1:n), KIND=KARG)

    return
  end subroutine get_data_mry_i

!!!_  - put_data_mry - MRY
  subroutine put_data_mry_d &
       & (ierr, &
       &  d, nh, nk, u, krect, vmiss, kfmt)
    use TOUZA_Trp,only: count_packed, pack_store
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(*)
    integer,         intent(in)  :: nh, nk
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt

    integer             :: nm
    integer             :: imsk(nh * nk)
    integer(kind=KISRC) :: imco(nh * nk)

    integer             :: mch(nk)
    integer             :: mofs(nk)

    integer             :: nbits, mcom
    integer             :: idec(nh)
    integer(kind=KISRC) :: icom(nh * nk)
    integer(kind=KISRC) :: imiss
    real(kind=KRSRC)    :: dma(2 * nk)
    real(kind=KRSRC)    :: buf(nh)
    integer,parameter   :: khld = 0_KISRC
    integer             :: jk, jdb, jde, jm, jc, jh, jb, je
    integer             :: nc, ne
    integer kpackm, kpackb

    ierr = 0

    nbits = kfmt - GFMT_MRY
    nm = count_packed(1, nh, khld)
    imiss = IBITS(HUGE(0_KISRC), 0, nbits)

    jc = 1
    kpackm = legacy_packing(1,     nh)
    kpackb = legacy_packing(nbits, nh)

    do jk = 1, nk
       jdb = (jk - 1) * nh
       ne = 0
       do jh = 1, nh
          if (d(jdb+jh).ne.vmiss) then
             imsk(jh) = 1
             ne = ne + 1
             buf(ne) = d(jdb+jh)
          else
             imsk(jh) = 0
          endif
       enddo
       nc = count_packed(nbits, ne, khld)
       mofs(jk) = nc
       mch(jk)  = ne

       call normalize_xry &
            & (ierr, idec, dma(2*jk-1:2*jk), buf(1:), ne, imiss, vmiss)
       jm = (jk - 1) * nm + 1
       call pack_store(ierr, imco(jm:), imsk, nh, 1, kpackm)
       call pack_store(ierr, icom(jc:), idec, ne, nbits, kpackb)
       jc = jc + nc
    enddo
    mcom = jc
    if (ierr.eq.0) call put_data_record(ierr, mcom,           u, krect)
    if (ierr.eq.0) call put_data_record(ierr, mch,   nk,      u, krect)
    if (ierr.eq.0) call put_data_record(ierr, mofs,  nk,      u, krect)
    if (ierr.eq.0) call put_data_record(ierr, dma,   2 * nk,  u, krect)
    if (ierr.eq.0) call put_data_record(ierr, imco,  nm * nk, u, krect)
    if (ierr.eq.0) call put_data_record(ierr, icom,  mcom,    u, krect)

    return
  end subroutine put_data_mry_d
  subroutine put_data_mry_f &
       & (ierr, &
       &  d, nh, nk, u, krect, vmiss, kfmt)
    implicit none
    integer,parameter :: KARG=KFLT, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(*)
    integer,         intent(in)  :: nh, nk
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt

    real(kind=KRSRC) :: buf(nh * nk)
    integer n

    ierr = 0
    n = nh * nk
    buf(1:n) = real(d(1:n), KIND=KDBL)
    call put_data_mry_d(ierr, buf, nh, nk, u, krect, vmiss, kfmt)

    return
  end subroutine put_data_mry_f
  subroutine put_data_mry_i &
       & (ierr, &
       &  d, nh, nk, u, krect, vmiss, kfmt)
    implicit none
    integer,parameter :: KARG=KI32, KRSRC=KDBL
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(in)  :: d(*)
    integer,           intent(in)  :: nh, nk
    integer,           intent(in)  :: krect
    integer,           intent(in)  :: u
    real(kind=KRMIS),  intent(in)  :: vmiss
    integer,           intent(in)  :: kfmt

    real(kind=KRSRC) :: buf(nh * nk)
    integer n

    ierr = 0
    n = nh * nk
    buf(1:n) = real(d(1:n), KIND=KDBL)
    call put_data_mry_d(ierr, buf, nh, nk, u, krect, vmiss, kfmt)

    return
  end subroutine put_data_mry_i

!!!_ + gtool-3 discarded extension
!!!_  - URJ
!!!_  - URZ
!!!_  - URS

!!!_ + *RT system (gtool-3 extension)
!!!_  - put_data_mrt_plain - URY:TOUZA/Trapiche plain format (full bundle)
  subroutine put_data_mrt_plain_d &
       & (ierr, &
       &  d, n, u, krect, vmiss, kfmt, kopts)
    use TOUZA_Trp,only: suggest_filling
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(in)          :: d(*)
    integer,         intent(in)          :: n
    integer,         intent(in)          :: krect
    integer,         intent(in)          :: u
    real(kind=KRMIS),intent(in)          :: vmiss
    integer,         intent(in)          :: kfmt
    integer,         intent(in),optional :: kopts(:)

    integer(kind=KISRC) :: ibagaz(0:2*n)
    integer mbits, xbits, xtop, xbtm
    integer nbgz
    integer,parameter :: mlim = DIGITS(0.0_KRSRC) - 1

    integer(kind=KISRC) :: mb
    integer(kind=KISRC) :: icom(n)
    real(kind=KARG)     :: buf(n)
    integer ncom
    integer kpack, kcode

    ierr = err_default

    kcode = def_encode_trapiche
    mbits = kfmt - GFMT_MRT
    if (ierr.eq.0) then
       call parse_urt_options &
            & (ierr, mbits, xbits, xtop, xbtm, kcode, mlim, kopts)
    endif

    kpack = suggest_filling(mbits, n, def_encode_trapiche)

    call mask_encode &
         & (ierr, mb,   ncom,   icom,   buf, &
         &  d,    n,    vmiss,  kpack)
    if (ierr.eq.0) icom(ncom+1) = kpack
    if (ierr.eq.0) call put_data_record(ierr, icom, ncom+1, u, krect, post=.true.)
    if (ierr.eq.0) then
       call put_data_urt_core &
            & (ierr,  &
            &  buf,   mb,    u,     krect,  .true., .false., &
            &  vmiss, mbits, xbits, xtop, xbtm,   kcode)
    endif
    return
  end subroutine put_data_mrt_plain_d
  subroutine put_data_mrt_plain_f &
       & (ierr, &
       &  d, n, u, krect, vmiss, kfmt, kopts)
    use TOUZA_Trp,only: suggest_filling
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(in)          :: d(*)
    integer,         intent(in)          :: n
    integer,         intent(in)          :: krect
    integer,         intent(in)          :: u
    real(kind=KRMIS),intent(in)          :: vmiss
    integer,         intent(in)          :: kfmt
    integer,         intent(in),optional :: kopts(:)

    integer(kind=KISRC) :: ibagaz(0:2*n)
    integer mbits, xbits, xtop, xbtm
    integer nbgz
    integer,parameter :: mlim = DIGITS(0.0_KRSRC) - 1

    integer(kind=KISRC) :: mb
    integer(kind=KISRC) :: icom(n)
    real(kind=KARG)     :: buf(n)
    integer ncom
    integer kpack, kcode

    ierr = err_default

    kcode = def_encode_trapiche
    mbits = kfmt - GFMT_MRT
    if (ierr.eq.0) then
       call parse_urt_options &
            & (ierr, mbits, xbits, xtop, xbtm, kcode, mlim, kopts)
    endif

    kpack = suggest_filling(mbits, n, def_encode_trapiche)

    call mask_encode &
         & (ierr, mb,   ncom,   icom,   buf, &
         &  d,    n,    vmiss,  kpack)
    if (ierr.eq.0) icom(ncom+1) = kpack
    if (ierr.eq.0) call put_data_record(ierr, icom, ncom+1, u, krect, post=.true.)
    if (ierr.eq.0) then
       call put_data_urt_core &
            & (ierr,  &
            &  buf,   mb,    u,     krect,  .true., .false., &
            &  vmiss, mbits, xbits, xtop, xbtm,   kcode)
    endif
    return
  end subroutine put_data_mrt_plain_f

!!!_  - put_data_urt_plain - URY:TOUZA/Trapiche plain format (full bundle)
  subroutine put_data_urt_plain_d &
       & (ierr, &
       &  d, n, u, krect, vmiss, kfmt, kopts)
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(in)          :: d(*)
    integer,         intent(in)          :: n
    integer,         intent(in)          :: krect
    integer,         intent(in)          :: u
    real(kind=KRMIS),intent(in)          :: vmiss
    integer,         intent(in)          :: kfmt
    integer,         intent(in),optional :: kopts(:)

    integer(kind=KISRC) :: ibagaz(0:2*n)
    integer mbits, xbits, xtop, xbtm
    integer nbgz
    integer,parameter :: mlim = DIGITS(0.0_KRSRC) - 1
    integer kcode

    ierr = err_default
    mbits = kfmt - GFMT_URT
    kcode = def_encode_trapiche
    if (ierr.eq.0) then
       call parse_urt_options &
            & (ierr, mbits, xbits, xtop, xbtm, kcode, mlim, kopts)
    endif

    if (ierr.eq.0) then
       call put_data_urt_core &
            & (ierr,  &
            &  d,     n,     u,     krect,  .false., .false., &
            &  vmiss, mbits, xbits, xtop, xbtm,    kcode)
    endif
    return
  end subroutine put_data_urt_plain_d
  subroutine put_data_urt_plain_f &
       & (ierr, &
       &  d, n, u, krect, vmiss, kfmt, kopts)
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(in)          :: d(*)
    integer,         intent(in)          :: n
    integer,         intent(in)          :: krect
    integer,         intent(in)          :: u
    real(kind=KRMIS),intent(in)          :: vmiss
    integer,         intent(in)          :: kfmt
    integer,         intent(in),optional :: kopts(:)

    integer(kind=KISRC) :: ibagaz(0:2*n)
    integer mbits, xbits, xtop, xbtm
    integer nbgz
    integer,parameter :: mlim = DIGITS(0.0_KRSRC) - 1
    integer kcode

    ierr = err_default
    mbits = kfmt - GFMT_URT
    kcode = def_encode_trapiche
    if (ierr.eq.0) then
       call parse_urt_options &
            & (ierr, mbits, xbits, xtop, xbtm, kcode, mlim, kopts)
    endif

    if (ierr.eq.0) then
       call put_data_urt_core &
            & (ierr,  &
            &  d,     n,     u,     krect,  .false., .false., &
            &  vmiss, mbits, xbits, xtop, xbtm,    kcode)
    endif
    return
  end subroutine put_data_urt_plain_f

!!!_  - put_data_urt_core
  subroutine put_data_urt_core_d &
       & (ierr, &
       &  d,     n,     u,     krect,  pre,  post,  &
       &  vmiss, mbits, xbits, xtop, xbtm, kcode, kapp)
    use TOUZA_Trp,only: &
         & count_packed, encode_alloc, retrieve_nbgz, &
         & KB_HEAD, guardar_extra
    use TOUZA_Trp,only: show_bagazo_props
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(in)          :: d(*)
    integer,         intent(in)          :: n     ! d size
    integer,         intent(in)          :: u
    integer,         intent(in)          :: krect
    logical,         intent(in)          :: pre, post
    real(kind=KRMIS),intent(in)          :: vmiss
    integer,         intent(in)          :: mbits
    integer,         intent(in)          :: xbits, xtop, xbtm
    integer,         intent(in)          :: kcode
    integer,         intent(in),optional :: kapp(0:)

    integer(kind=KISRC) :: ibagaz(0:2*n)
    integer nbgz, napp, jp

    ierr = err_default
    if (present(kapp)) then
       napp = size(kapp)
    else
       napp = 0
    endif

    if (ierr.eq.0) then
       call encode_alloc &
            & (ierr,   &
            &  ibagaz, &
            &  d,      n,     vmiss, &
            &  mbits,  xbits, xtop,  xbtm, kcode)
    endif
    if (ierr.eq.0) call guardar_extra(ierr, ibagaz, -1, 0)
    if (ierr.eq.0) call guardar_extra(ierr, ibagaz, XTRP_ID, XID_URT)
    if (ierr.eq.0) call guardar_extra(ierr, ibagaz, XTRP_NX, napp)
    if (ierr.eq.0) then
       nbgz = retrieve_nbgz(ibagaz)
       if (napp.gt.0) then
          jp = KB_HEAD + nbgz
          ibagaz(jp:jp+napp-1) = kapp(0:napp-1)
          nbgz = nbgz + napp
       endif
    endif
    if (ierr.eq.0) then
       call put_data_record &
            & (ierr, ibagaz(0:KB_HEAD-1), KB_HEAD, u, krect, pre=pre, post=.TRUE.)
    endif
    if (ierr.eq.0) then
       call put_data_record &
            & (ierr, ibagaz(KB_HEAD:KB_HEAD+nbgz-1), nbgz, u, krect, pre=.TRUE., post=post)
    endif
    if (ierr.eq.0) then
       if (udiag.ge.-1) call show_bagazo_props(ierr, ibagaz, udiag)
    endif
    return
  end subroutine put_data_urt_core_d
  subroutine put_data_urt_core_f &
       & (ierr, &
       &  d,     n,     u,     krect,  pre,  post,  &
       &  vmiss, mbits, xbits, xtop, xbtm, kcode, kapp)
    use TOUZA_Trp,only: &
         & count_packed, encode_alloc, retrieve_nbgz, &
         & KB_HEAD, guardar_extra
    use TOUZA_Trp,only: show_bagazo_props
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(in)          :: d(*)
    integer,         intent(in)          :: n     ! d size
    integer,         intent(in)          :: u
    integer,         intent(in)          :: krect
    logical,         intent(in)          :: pre, post
    real(kind=KRMIS),intent(in)          :: vmiss
    integer,         intent(in)          :: mbits
    integer,         intent(in)          :: xbits, xtop, xbtm
    integer,         intent(in)          :: kcode
    integer,         intent(in),optional :: kapp(0:)

    integer(kind=KISRC) :: ibagaz(0:2*n)
    integer nbgz, napp, jp
    real(kind=KRSRC) :: rmiss

    ierr = err_default
    if (present(kapp)) then
       napp = size(kapp)
    else
       napp = 0
    endif

    if (ierr.eq.0) then
       rmiss = real(vmiss, kind=KRSRC)
       call encode_alloc &
            & (ierr,   &
            &  ibagaz, &
            &  d,      n,     rmiss, &
            &  mbits,  xbits, xtop,  xbtm, kcode)
    endif
    if (ierr.eq.0) call guardar_extra(ierr, ibagaz, -1, 0)
    if (ierr.eq.0) call guardar_extra(ierr, ibagaz, XTRP_ID, XID_URT)
    if (ierr.eq.0) call guardar_extra(ierr, ibagaz, XTRP_NX, napp)
    if (ierr.eq.0) then
       nbgz = retrieve_nbgz(ibagaz)
       if (napp.gt.0) then
          jp = KB_HEAD + nbgz
          ibagaz(jp:jp+napp-1) = kapp(0:napp-1)
          nbgz = nbgz + napp
       endif
    endif
    if (ierr.eq.0) then
       call put_data_record &
            & (ierr, ibagaz(0:KB_HEAD-1), KB_HEAD, u, krect, pre=pre, post=.TRUE.)
    endif
    if (ierr.eq.0) then
       call put_data_record &
            & (ierr, ibagaz(KB_HEAD:KB_HEAD+nbgz-1), nbgz, u, krect, pre=.TRUE., post=post)
    endif
    if (ierr.eq.0) then
       if (udiag.ge.-1) call show_bagazo_props(ierr, ibagaz, udiag)
    endif
    return
  end subroutine put_data_urt_core_f
!!!_  - get_data_mrt - MRT
  subroutine get_data_mrt_d &
       & (ierr, &
       &  d, n, kaxs, u, krect, vmiss, kfmt)
    use TOUZA_Trp,only: &
         & decode_alloc, retrieve_nbgz, retrieve_ncnz, retrieve_extra, &
         & count_packed, suggest_filling
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: n, kaxs(*)
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt

    integer(kind=KISRC) :: mb
    integer(kind=KISRC) :: icom(n)
    real(kind=KRSRC)    :: buf(n)
    integer(kind=KISRC),parameter :: khld = 0_KISRC
    integer ncom
    logical sub
    integer kpack

    ierr = err_default

    sub = .TRUE.
    ncom = count_packed(1, n, khld)
    ! icom(ncom+1) stores filling method
    if (ierr.eq.0) call get_data_record(ierr, icom, ncom+1, u, krect, sub=sub)
    if (ierr.eq.0 .and. .NOT.sub) then
       ierr = -1
    endif
    if (ierr.eq.0) then
       sub = .FALSE.
       call get_data_urt_core &
            & (ierr, &
            &  buf,   n,  u, krect,  sub, &
            &  vmiss, def_decode_trapiche)
    endif
    if (ierr.eq.0) then
       kpack = icom(ncom + 1)
       kpack = suggest_filling(1, n, kcode=def_decode_trapiche, kfill=kpack)
       call mask_decode &
            & (ierr,  d, n, buf, icom, vmiss, kpack)
    endif
    return
  end subroutine get_data_mrt_d
  subroutine get_data_mrt_f &
       & (ierr, &
       &  d, n, kaxs, u, krect, vmiss, kfmt)
    use TOUZA_Trp,only: &
         & decode_alloc, retrieve_nbgz, retrieve_ncnz, retrieve_extra, &
         & count_packed, suggest_filling
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: n, kaxs(*)
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt

    integer(kind=KISRC) :: mb
    integer(kind=KISRC) :: icom(n)
    real(kind=KRSRC)    :: buf(n)
    integer(kind=KISRC),parameter :: khld = 0_KISRC
    integer ncom
    logical sub
    integer kpack

    ierr = err_default

    sub = .TRUE.
    ncom = count_packed(1, n, khld)
    ! icom(ncom+1) stores filling method
    if (ierr.eq.0) call get_data_record(ierr, icom, ncom+1, u, krect, sub=sub)
    if (ierr.eq.0 .and. .NOT.sub) then
       ierr = -1
    endif
    if (ierr.eq.0) then
       sub = .FALSE.
       call get_data_urt_core &
            & (ierr, &
            &  buf,   n,  u, krect,  sub, &
            &  vmiss, def_decode_trapiche)
    endif
    if (ierr.eq.0) then
       kpack = icom(ncom + 1)
       kpack = suggest_filling(1, n, kcode=def_decode_trapiche, kfill=kpack)
       call mask_decode &
            & (ierr,  d, n, buf, icom, vmiss, kpack)
    endif
    return
  end subroutine get_data_mrt_f

!!!_  - get_data_urt - URY:TOUZA/Trapiche
  subroutine get_data_urt_d &
       & (ierr, &
       &  d, n, kaxs, u, krect, vmiss, kfmt, &
       &  napp, kapp)
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out)          :: ierr
    real(kind=KARG), intent(out)          :: d(*)
    integer,         intent(in)           :: n, kaxs(*)
    integer,         intent(in)           :: krect
    integer,         intent(in)           :: u
    real(kind=KRMIS),intent(in)           :: vmiss
    integer,         intent(in)           :: kfmt
    integer,         intent(out),optional :: napp
    integer,         intent(out),optional :: kapp(0:)

    logical sub

    ierr = err_default

    sub = .FALSE.

    if (ierr.eq.0) then
       call get_data_urt_core &
            & (ierr, &
            &  d,     n,      u,    krect,  sub, &
            &  vmiss, def_decode_trapiche,     &
            &  napp,  kapp)
    endif
    return
  end subroutine get_data_urt_d
  subroutine get_data_urt_f &
       & (ierr, &
       &  d, n, kaxs, u, krect, vmiss, kfmt, &
       &  napp, kapp)
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out)          :: ierr
    real(kind=KARG), intent(out)          :: d(*)
    integer,         intent(in)           :: n, kaxs(*)
    integer,         intent(in)           :: krect
    integer,         intent(in)           :: u
    real(kind=KRMIS),intent(in)           :: vmiss
    integer,         intent(in)           :: kfmt
    integer,         intent(out),optional :: napp
    integer,         intent(out),optional :: kapp(0:)

    logical sub

    ierr = err_default

    sub = .FALSE.

    if (ierr.eq.0) then
       call get_data_urt_core &
            & (ierr, &
            &  d,     n,      u,    krect,  sub, &
            &  vmiss, def_decode_trapiche,     &
            &  napp,  kapp)
    endif
    return
  end subroutine get_data_urt_f

!!!_  - get_data_urt_core
  subroutine get_data_urt_core_d &
       & (ierr, &
       &  d,     n,     u,    krect,  sub, &
       &  vmiss, kcode, napp, kapp)
    use TOUZA_Trp,only: &
         & decode_alloc, retrieve_nbgz, retrieve_ncnz, retrieve_extra, &
         & KB_HEAD,      show_bagazo_props
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out)          :: ierr
    real(kind=KARG), intent(out)          :: d(*)
    integer,         intent(in)           :: n
    integer,         intent(in)           :: krect
    integer,         intent(in)           :: u
    logical,         intent(inout)        :: sub
    real(kind=KRMIS),intent(in)           :: vmiss
    integer,         intent(in)           :: kcode
    integer,         intent(out),optional :: napp
    integer,         intent(out),optional :: kapp(0:)

    integer(kind=KISRC) :: ibagaz(0:2*n)
    integer nbgz, ncnz, nall
    integer na,   ma, xid, jp
    logical cont

    ierr = err_default
    na = 0

    if (ierr.eq.0) then
       cont = .TRUE.
       call get_data_record(ierr, ibagaz(0:KB_HEAD-1), KB_HEAD, u, krect, sub=cont)
    endif
    if (ierr.eq.0.and. .NOT.CONT) then
       ! no packed data follows
       ierr = -1
    endif
    if (ierr.eq.0) then
       if (udiag.ge.-1) call show_bagazo_props(ierr, ibagaz, udiag)
    endif
    if (ierr.eq.0) then
       xid  = retrieve_extra(ibagaz, XTRP_ID)
       if (xid.ne.XID_URT) ierr = -1
    endif
    if (ierr.eq.0) then
       na = retrieve_extra(ibagaz, XTRP_NX)
       nbgz = retrieve_nbgz(ibagaz)
       nall = nbgz + na
       call get_data_record(ierr, ibagaz(KB_HEAD:KB_HEAD+nall-1), nall, u, krect, sub=sub)
    endif

    if (ierr.eq.0) then
       ncnz = retrieve_ncnz(ibagaz)
       if (ncnz.gt.n) then
          ierr = -1
       else
          call decode_alloc(ierr, d, ibagaz, ncnz, vmiss, kcode)
       endif
    endif
    if (ierr.eq.0) then
       if (present(napp)) then
          napp = na
       endif
       if (present(kapp)) then
          ma = max(na, size(kapp))
          jp = KB_HEAD + nbgz
          if (ma.gt.0) kapp(0:ma-1) = ibagaz(jp:jp+ma-1)
       endif
    endif
    return
  end subroutine get_data_urt_core_d
  subroutine get_data_urt_core_f &
       & (ierr, &
       &  d,     n,     u,    krect,  sub, &
       &  vmiss, kcode, napp, kapp)
    use TOUZA_Trp,only: &
         & decode_alloc, retrieve_nbgz, retrieve_ncnz, retrieve_extra, &
         & KB_HEAD,      show_bagazo_props
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out)          :: ierr
    real(kind=KARG), intent(out)          :: d(*)
    integer,         intent(in)           :: n
    integer,         intent(in)           :: krect
    integer,         intent(in)           :: u
    logical,         intent(inout)        :: sub
    real(kind=KRMIS),intent(in)           :: vmiss
    integer,         intent(in)           :: kcode
    integer,         intent(out),optional :: napp
    integer,         intent(out),optional :: kapp(0:)

    integer(kind=KISRC) :: ibagaz(0:2*n)
    integer nbgz, ncnz, nall
    integer na,   ma, xid, jp
    logical cont
    real(kind=KRSRC) :: rmiss

    ierr = err_default
    na = 0

    if (ierr.eq.0) then
       cont = .TRUE.
       call get_data_record(ierr, ibagaz(0:KB_HEAD-1), KB_HEAD, u, krect, sub=cont)
    endif
    if (ierr.eq.0.and. .NOT.CONT) then
       ! no packed data follows
       ierr = -1
    endif
    if (ierr.eq.0) then
       if (udiag.ge.-1) call show_bagazo_props(ierr, ibagaz, udiag)
    endif
    if (ierr.eq.0) then
       xid  = retrieve_extra(ibagaz, XTRP_ID)
       if (xid.ne.XID_URT) ierr = -1
    endif
    if (ierr.eq.0) then
       na = retrieve_extra(ibagaz, XTRP_NX)
       nbgz = retrieve_nbgz(ibagaz)
       nall = nbgz + na
       call get_data_record(ierr, ibagaz(KB_HEAD:KB_HEAD+nall-1), nall, u, krect, sub=sub)
    endif

    if (ierr.eq.0) then
       ncnz = retrieve_ncnz(ibagaz)
       if (ncnz.gt.n) then
          ierr = -1
       else
          rmiss = real(vmiss, kind=KRSRC)
          call decode_alloc(ierr, d, ibagaz, ncnz, rmiss, kcode)
       endif
    endif
    if (ierr.eq.0) then
       if (present(napp)) then
          napp = na
       endif
       if (present(kapp)) then
          ma = max(na, size(kapp))
          jp = KB_HEAD + nbgz
          if (ma.gt.0) kapp(0:ma-1) = ibagaz(jp:jp+ma-1)
       endif
    endif
    return
  end subroutine get_data_urt_core_f

!!!_  & switch_urt_diag
  subroutine switch_urt_diag &
       & (atag, itag, u)
    use TOUZA_Trp,only: push_show_tags
    implicit none
    character(len=*),intent(in),optional :: atag
    integer,         intent(in),optional :: itag
    integer,         intent(in),optional :: u
    call push_show_tags(atag, itag)
    if (present(u)) then
       udiag = u
    endif
  end subroutine switch_urt_diag
!!!_  & parse_urt_options
  subroutine parse_urt_options &
       & (ierr, &
       &  mbits, xbits, xtop, xbtm, kcode, mlim, &
       &  kopts)
    use TOUZA_Trp,only: XNOTOP, XNOBTM
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(inout)       :: mbits
    integer,intent(out)         :: xbits, xtop, xbtm
    integer,intent(inout)       :: kcode
    integer,intent(in)          :: mlim
    integer,intent(in),optional :: kopts(:)

    integer,parameter :: lo = PROP_URT_BREAK
    integer ko(lo)
    integer m, kc

    ierr = 0
    call set_urt_defs(ko)
    if (present(kopts)) then
       m = min(lo, size(kopts))
       ko(1:m) = kopts(1:m)
    endif
    ! mantissa bits
    kc = ko(PROP_URT_MANTISSA)
    if (kc.ge.0) mbits = kc
    mbits = max(0, min(mbits, mlim))
    if (mbits.eq.0) mbits = mlim
    ! exponent bits
    xbits = max(-1, ko(PROP_URT_XBITS)) ! -1 for auto exponent range
    ! exponent range
    xtop = ko(PROP_URT_XTOP)
    if (xtop.eq.PROP_DEFAULT) xtop = XNOTOP
    xbtm = ko(PROP_URT_XBOTTOM)
    if (xbtm.eq.PROP_DEFAULT) xbtm = XNOBTM
    ! other codes (clipping, etc)
    kc = ko(PROP_URT_CODES)
    if (kc.gt.0) then
       kcode = IOR(kcode, IAND(kc, KCODE_CLIPPING))
       kcode = IOR(kcode, IAND(kc, KCODE_ROUND))
    endif
    return
  end subroutine parse_urt_options

!!!_  & set_urt_defs
  subroutine set_urt_defs (kopts)
    implicit none
    integer,intent(out) :: kopts(:)
    kopts(:) = PROP_DEFAULT
    return
  end subroutine set_urt_defs

!!!_ + gtool extenstion (MI4)
!!!_  - get_data_mi4 - MI4
  subroutine get_data_mi4_i &
       & (ierr, &
       &  d, n, u, krect, vmiss)
    use TOUZA_Trp,only: count_packed
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(out) :: d(*)
    integer,           intent(in)  :: n
    integer,           intent(in)  :: krect
    integer,           intent(in)  :: u
    real(kind=KRMIS),  intent(in)  :: vmiss
    integer(kind=KISRC) :: mb
    integer(kind=KISRC) :: icom(n), buf(n)
    integer(kind=KISRC),parameter :: khld = 0_KISRC
    integer ncom
    integer kpack
    ierr = 0
    ncom = count_packed(1, n, khld)
    call get_data_record(ierr, mb, u, krect)
    if (ierr.eq.0) call get_data_record(ierr, icom, ncom, u, krect)
    if (ierr.eq.0) call get_data_record(ierr, buf,  mb,   u, krect)

    if (ierr.eq.0) then
       kpack = legacy_unpacking(1, n)
       call mask_decode &
            & (ierr,  d, n, buf, icom, vmiss, kpack)
    endif

    return
  end subroutine get_data_mi4_i
  subroutine get_data_mi4_d &
       & (ierr, &
       &  d, n, u, krect, vmiss)
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss

    integer(kind=KISRC) :: buf(n)
    ierr = 0
    call get_data_mi4_i(ierr, buf, n, u, krect, vmiss)

    if (ierr.eq.0) d(1:n) = real(buf(1:n), KIND=KARG)

    return
  end subroutine get_data_mi4_d
  subroutine get_data_mi4_f &
       & (ierr, &
       &  d, n, u, krect, vmiss)
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss

    integer(kind=KISRC) :: buf(n)
    ierr = 0
    call get_data_mi4_i(ierr, buf, n, u, krect, vmiss)

    if (ierr.eq.0) d(1:n) = real(buf(1:n), KIND=KARG)

    return
  end subroutine get_data_mi4_f
!!!_  - put_data_mi4 - MI4
  subroutine put_data_mi4_i &
       & (ierr, &
       &  d, n, u, krect, vmiss)
    use TOUZA_Trp,only: count_packed
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(in)  :: d(*)
    integer,           intent(in)  :: n
    integer,           intent(in)  :: krect
    integer,           intent(in)  :: u
    real(kind=KRMIS),  intent(in)  :: vmiss
    integer(kind=KISRC) :: mb
    integer(kind=KISRC) :: icom(n), buf(n)
    integer(kind=KISRC),parameter :: khld = 0_KISRC
    integer ncom
    integer kpack

    ierr = 0
    kpack = legacy_packing(1, n)
    call mask_encode &
       & (ierr, mb,   ncom,   icom,   buf, &
       &  d,    n,    vmiss,  kpack)

    if (ierr.eq.0) call put_data_record(ierr, mb, u, krect)
    if (ierr.eq.0) call put_data_record(ierr, icom, ncom, u, krect)
    if (ierr.eq.0) call put_data_record(ierr, buf,  mb,   u, krect)

    return
  end subroutine put_data_mi4_i
  subroutine put_data_mi4_f &
       & (ierr, &
       &  d, n, u, krect, vmiss)
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss

    integer(kind=KISRC) :: buf(n)

    ierr = 0
    buf(1:n) = int(d(1:n), KIND=KISRC)
    call put_data_mi4_i(ierr, buf, n, u, krect, vmiss)

    return
  end subroutine put_data_mi4_f
  subroutine put_data_mi4_d &
       & (ierr, &
       &  d, n, u, krect, vmiss)
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss

    integer(kind=KISRC) :: buf(n)

    ierr = 0
    buf(1:n) = int(d(1:n), KIND=KISRC)
    call put_data_mi4_i(ierr, buf, n, u, krect, vmiss)

    return
  end subroutine put_data_mi4_d

!!!_  - MRT

!!!_ + utilities
!!!_  & get_record_prop - get sequential record properties (byte-order and separator size)
  subroutine get_record_prop &
       & (ierr, krect, u)
    use TOUZA_Nng_std,   only: KI32, KI64, KIOFS, is_eof_ss
    use TOUZA_Nng_header,only: nitem, litem
    use TOUZA_Nng_io,    only: &
         & WHENCE_ABS, &
         & ssq_read_isep, ssq_rseek, &
         & ssq_eswap
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: krect
    integer,intent(in)  :: u

    integer(kind=KIOFS) :: jpos
    integer(kind=KI32)  :: iseph, isepl
    integer(kind=KIOFS) :: nlh

    ierr = err_default
    krect  = 0
    nlh  = 0
    !! CAUTION: header record length must be no more than 0xFFFF

    ! 000000xy   000000xy  yx000000
    ! 00xy....   00xy....  ....yx00

    ! yx000000   yx000000  000000xy
    ! yx00....   yx00....  ....00xy

    if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
    if (ierr.eq.0) call ssq_read_isep(ierr, u, iseph)
    if (is_eof_ss(ierr)) then
       ierr = ERR_EOF
       return
    endif
    if (ierr.eq.0) call ssq_read_isep(ierr, u, isepl)
    if (ierr.eq.0) then
       if (iseph.eq.0) then
          ! [00 00 00 00] [00 00 04 00]  long/big
          if (isepl.lt.HEADER_LIMIT) then
             KRECT = IOR(KRECT, REC_LSEP) ! long native
             nlh = isepl
          else
             KRECT = IOR(IOR(KRECT, REC_LSEP), REC_SWAP) ! long swap
             nlh = ssq_eswap(isepl)
          endif
       else if (isepl.eq.0) then
          ! [00 04 00 00] [00 00 00 00]  long/little
          if (iseph.lt.HEADER_LIMIT) then
             KRECT = IOR(KRECT, REC_LSEP) ! long native
             nlh = iseph
          else
             KRECT = IOR(IOR(KRECT, REC_LSEP), REC_SWAP) ! long swap
             nlh = ssq_eswap(iseph)
          endif
       else if (isepl.eq.HEADER_SPACES) then
          ! [ff ff fc 00] [20 20 20 20] short/big/cont
          ! [00 fc ff ff] [20 20 20 20] short/little/cont
          ! [00 00 04 00] [20 20 20 20] short/big
          ! [00 04 00 00] [20 20 20 20] short/little
          if (abs(iseph).lt.HEADER_LIMIT) then
             KRECT = KRECT                ! short native
             nlh = abs(iseph)
          else
             KRECT = IOR(KRECT, REC_SWAP) ! short swap
             nlh = abs(ssq_eswap(iseph))
          endif
       else
          nlh = nlhead_std - 1
       endif
       call ssq_rseek(ierr, u, jpos, whence=WHENCE_ABS)
       if (nlh.lt.nlhead_std .or. nlh.ge.HEADER_LIMIT) KRECT = REC_ERROR
       ! write(*, *) 'SEP', iseph, isepl, nlh, KRECT, ierr
    else
       KRECT = REC_ERROR
    endif

    return
  end subroutine get_record_prop
!!!_  & get_header - read header block
  subroutine get_header &
       & (ierr, &
       &  head,  u, krect)
    use TOUZA_Nng_std,   only: KIOFS
    use TOUZA_Nng_header,only: nitem
    use TOUZA_Nng_io,    only: ssq_read_lrec, ssq_read_irec
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: head(*)
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u

    logical swap, lrec
    integer idfm

    ierr = 0
    if (KRECT.ge.0) then
       swap = IAND(krect, REC_SWAP).ne.0
       lrec = IAND(krect, REC_LSEP).ne.0
       if (lrec) then
          call ssq_read_lrec(ierr, u, head, nitem, swap)
       else
          call ssq_read_irec(ierr, u, head, nitem, swap)
       endif
       ! write(*, *) 'header', ierr, KRECT, head(1), swap, lrec
       if (ierr.eq.0) then
          idfm = check_id_format(head)
          if (idfm.lt.0) ierr = idfm
       endif
    else
       head(1:nitem) = ' '
       ierr = ERR_UNKNOWN_FORMAT
    endif
    ! write(*, *) 'header', ierr, KRECT
    ! write(*, *) '/', head(1), '/'
    return
  end subroutine get_header

!!!_  & set_wrecord_prop - set sequential record properties to write (byte-order and separator size)
  subroutine set_wrecord_prop &
       & (ierr, krect, u, kendi)
    use TOUZA_Nng_std,only: &
         & choice, check_bodr_unit, KIOFS, kendi_mem, kendi_file
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(inout)       :: krect
    integer,intent(in)          :: u
    integer,intent(in),optional :: kendi
    integer ke, kr
    integer(kind=KIOFS) :: jpos

    ierr = err_default

    if (present(kendi)) then
       ke = kendi
    else
       select case(bodr_wnative)
       case (BODR_ASSUME_SYSTEM)
          ke = kendi_mem
       case (BODR_ASSUME_FILE)
          ke = kendi_file
       case (BODR_CHECK_VERBOSE)
          if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
          if (ierr.eq.0) call check_bodr_unit(ierr, ke, utest=u, jrec=0)
          if (ierr.eq.0) write(UNIT=u, IOSTAT=ierr, POS=jpos)
       case default
          ke = kendi_mem
       end select
    endif
    if (ierr.eq.0) then
       call get_switch(kr, ke, krect)
       krect = kr
    endif

    return
  end subroutine set_wrecord_prop

!!!_  & get_switch - get record-format switch to write
  subroutine get_switch (krect, kendi, kcfg)
    use TOUZA_Nng_std,only: choice, endian_BIG, endian_LITTLE
    implicit none
    integer,intent(out)         :: krect
    integer,intent(in)          :: kendi  ! estimated file byte-order
    integer,intent(in),optional :: kcfg   ! user setting to overwrite default

    integer ktmp

    krect = REC_DEFAULT

    ktmp = choice(def_krectw, kcfg)
    if (ktmp.lt.0) ktmp = def_krectw

    if (IAND(ktmp, REC_LSEP).gt.0) krect = krect + REC_LSEP
    if (IAND(ktmp, REC_SWAP).gt.0) then
       krect = krect + REC_SWAP
    else if (IAND(ktmp, REC_BIG).gt.0) then
       if (kendi.eq.endian_LITTLE) krect = krect + REC_SWAP
    else if (IAND(ktmp, REC_LITTLE).gt.0) then
       if (kendi.eq.endian_BIG) krect = krect + REC_SWAP
    endif

    return
  end subroutine get_switch

!!!_  & put_header - write header block
  subroutine put_header &
       & (ierr, &
       &  head, u, krect)
    use TOUZA_Nng_std,   only: KIOFS
    use TOUZA_Nng_header,only: nitem
    use TOUZA_Nng_io,    only: ssq_write_lrec, ssq_write_irec
    implicit none
    integer,            intent(out) :: ierr
    character(len=*),   intent(in)  :: head(*)
    integer,            intent(in)  :: krect
    integer,            intent(in)  :: u

    logical swap, lrec

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call ssq_write_lrec(ierr, u, head, nitem, swap)
    else
       call ssq_write_irec(ierr, u, head, nitem, swap)
    endif
    return
  end subroutine put_header
!!!_  & get_data_record - read data block
  subroutine get_data_record_i &
       & (ierr, &
       &  d,  n, u, krect, sub)
    use TOUZA_Nng_io, only: &
         & ssq_read_lrec, ssq_read_irec
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)            :: ierr
    integer(kind=KARG),intent(out)            :: d(*)
    integer,           intent(in)             :: n
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: u
    logical,           intent(inout),optional :: sub

    logical swap, lrec

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call ssq_read_lrec(ierr, u, d, n, swap)
    else
       call ssq_read_irec(ierr, u, d, n, swap, sub)
    endif
    return
  end subroutine get_data_record_i
  subroutine get_data_record_f &
       & (ierr, &
       &  d,  n, u, krect, sub)
    use TOUZA_Nng_io, only: &
         & ssq_read_lrec, ssq_read_irec
    implicit none
    integer,parameter :: KARG=KFLT
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(*)
    integer,        intent(in)             :: n
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: u
    logical,        intent(inout),optional :: sub

    logical swap, lrec

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call ssq_read_lrec(ierr, u, d, n, swap)
    else
       call ssq_read_irec(ierr, u, d, n, swap, sub)
    endif
    return
  end subroutine get_data_record_f
  subroutine get_data_record_d &
       & (ierr, &
       &  d,  n, u, krect, sub)
    use TOUZA_Nng_io, only: &
         & ssq_read_lrec, ssq_read_irec
    implicit none
    integer,parameter :: KARG=KDBL
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(*)
    integer,        intent(in)             :: n
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: u
    logical,        intent(inout),optional :: sub

    logical swap, lrec

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call ssq_read_lrec(ierr, u, d, n, swap)
    else
       call ssq_read_irec(ierr, u, d, n, swap, sub)
    endif
    return
  end subroutine get_data_record_d

  subroutine get_data_record_i1 &
       & (ierr, &
       &  d,  u, krect, sub)
    use TOUZA_Nng_io, only: &
         & ssq_read_lrec, ssq_read_irec
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)            :: ierr
    integer(kind=KARG),intent(out)            :: d
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: u
    logical,           intent(inout),optional :: sub

    logical swap, lrec
    integer(kind=KARG) :: b(1)

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call ssq_read_lrec(ierr, u, b, 1, swap)
    else
       call ssq_read_irec(ierr, u, b, 1, swap, sub)
    endif
    if (ierr.eq.0) d = b(1)
    return
  end subroutine get_data_record_i1
  subroutine get_data_record_f1 &
       & (ierr, &
       &  d,  u, krect, sub)
    use TOUZA_Nng_io, only: &
         & ssq_read_lrec, ssq_read_irec
    implicit none
    integer,parameter :: KARG=KFLT
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: u
    logical,        intent(inout),optional :: sub

    logical swap, lrec
    real(kind=KARG) :: b(1)

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call ssq_read_lrec(ierr, u, b, 1, swap)
    else
       call ssq_read_irec(ierr, u, b, 1, swap, sub)
    endif
    if (ierr.eq.0) d = b(1)
    return
  end subroutine get_data_record_f1
  subroutine get_data_record_d1 &
       & (ierr, &
       &  d, u, krect, sub)
    use TOUZA_Nng_io, only: &
         & ssq_read_lrec, ssq_read_irec
    implicit none
    integer,parameter :: KARG=KDBL
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: u
    logical,        intent(inout),optional :: sub

    logical swap, lrec
    real(kind=KARG) :: b(1)

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call ssq_read_lrec(ierr, u, b, 1, swap)
    else
       call ssq_read_irec(ierr, u, b, 1, swap, sub)
    endif
    if (ierr.eq.0) d = b(1)
    return
  end subroutine get_data_record_d1
!!!_  & get_data_irecord - read integer data block with conversion
  subroutine get_data_irecord_f &
       & (ierr, &
       &  d,  n, u, krect, sub)
    use TOUZA_Nng_io, only: &
         & ssq_read_lrec, ssq_read_irec
    implicit none
    integer,parameter :: KARG=KFLT, KSRC=KI32
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(*)
    integer,        intent(in)             :: n
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: u
    logical,        intent(inout),optional :: sub

    integer(kind=KSRC) :: w(n)
    call get_data_record_i (ierr, w, n, u, krect, sub)
    if (ierr.eq.0) then
       d(1:n) = real(w(1:n), kind=KARG)
    endif
    return
  end subroutine get_data_irecord_f
  subroutine get_data_irecord_d &
       & (ierr, &
       &  d,  n, u, krect, sub)
    use TOUZA_Nng_io, only: &
         & ssq_read_lrec, ssq_read_irec
    implicit none
    integer,parameter :: KARG=KDBL, KSRC=KI32
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(*)
    integer,        intent(in)             :: n
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: u
    logical,        intent(inout),optional :: sub

    integer(kind=KSRC) :: w(n)
    call get_data_record_i (ierr, w, n, u, krect, sub)
    if (ierr.eq.0) then
       d(1:n) = real(w(1:n), kind=KARG)
    endif
    return
  end subroutine get_data_irecord_d
!!!_  & get_data_frecord - read float data block with conversion
  subroutine get_data_frecord_i &
       & (ierr, &
       &  d,  n, u, krect, sub)
    use TOUZA_Nng_io, only: &
         & ssq_read_lrec, ssq_read_irec
    implicit none
    integer,parameter :: KARG=KI32, KSRC=KFLT
    integer,           intent(out)            :: ierr
    integer(kind=KARG),intent(out)            :: d(*)
    integer,           intent(in)             :: n
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: u
    logical,           intent(inout),optional :: sub

    real(kind=KSRC) :: w(n)
    call get_data_record_f (ierr, w, n, u, krect, sub)
    if (ierr.eq.0) then
       d(1:n) = int(w(1:n), kind=KARG)
    endif
    return
  end subroutine get_data_frecord_i
  subroutine get_data_frecord_d &
       & (ierr, &
       &  d,  n, u, krect, sub)
    use TOUZA_Nng_io, only: &
         & ssq_read_lrec, ssq_read_irec
    implicit none
    integer,parameter :: KARG=KDBL, KSRC=KFLT
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(*)
    integer,        intent(in)             :: n
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: u
    logical,        intent(inout),optional :: sub

    real(kind=KSRC) :: w(n)
    call get_data_record_f (ierr, w, n, u, krect, sub)
    if (ierr.eq.0) then
       d(1:n) = real(w(1:n), kind=KARG)
    endif
    return
  end subroutine get_data_frecord_d

!!!_  & get_data_drecord - read double data block with conversion
  subroutine get_data_drecord_i &
       & (ierr, &
       &  d,  n, u, krect, sub)
    use TOUZA_Nng_io, only: &
         & ssq_read_lrec, ssq_read_irec
    implicit none
    integer,parameter :: KARG=KI32, KSRC=KDBL
    integer,           intent(out)            :: ierr
    integer(kind=KARG),intent(out)            :: d(*)
    integer,           intent(in)             :: n
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: u
    logical,           intent(inout),optional :: sub

    real(kind=KSRC) :: w(n)
    call get_data_record_d (ierr, w, n, u, krect, sub)
    if (ierr.eq.0) then
       d(1:n) = int(w(1:n), kind=KARG)
    endif
    return
  end subroutine get_data_drecord_i
  subroutine get_data_drecord_f &
       & (ierr, &
       &  d,  n, u, krect, sub)
    use TOUZA_Nng_io, only: &
         & ssq_read_lrec, ssq_read_irec
    implicit none
    integer,parameter :: KARG=KFLT, KSRC=KDBL
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(*)
    integer,        intent(in)             :: n
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: u
    logical,        intent(inout),optional :: sub

    real(kind=KSRC) :: w(n)
    call get_data_record_d (ierr, w, n, u, krect, sub)
    if (ierr.eq.0) then
       d(1:n) = real(w(1:n), kind=KARG)
    endif
    return
  end subroutine get_data_drecord_f
!!!_  & put_data_record - write data block
  subroutine put_data_record_i &
       & (ierr, &
       &  d,  n, u, krect, pre, post)
    use TOUZA_Nng_io, only: &
         & ssq_write_lrec, ssq_write_irec
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)         :: ierr
    integer(kind=KARG),intent(in)          :: d(*)
    integer,           intent(in)          :: n
    integer,           intent(in)          :: krect
    integer,           intent(in)          :: u
    logical,           intent(in),optional :: pre, post

    logical swap, lrec

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call ssq_write_lrec(ierr, u, d, n, swap)
    else
       call ssq_write_irec(ierr, u, d, n, swap, pre, post)
    endif
    return
  end subroutine put_data_record_i
  subroutine put_data_record_f &
       & (ierr, &
       &  d,  n, u, krect, pre, post)
    use TOUZA_Nng_io, only: &
         & ssq_write_lrec, ssq_write_irec
    implicit none
    integer,parameter :: KARG=KFLT
    integer,        intent(out)         :: ierr
    real(kind=KARG),intent(in)          :: d(*)
    integer,        intent(in)          :: n
    integer,        intent(in)          :: krect
    integer,        intent(in)          :: u
    logical,        intent(in),optional :: pre, post

    logical swap, lrec

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call ssq_write_lrec(ierr, u, d, n, swap)
    else
       call ssq_write_irec(ierr, u, d, n, swap, pre, post)
    endif
    return
  end subroutine put_data_record_f
  subroutine put_data_record_d &
       & (ierr, &
       &  d,  n, u, krect, pre, post)
    use TOUZA_Nng_io, only: &
         & ssq_write_lrec, ssq_write_irec
    implicit none
    integer,parameter :: KARG=KDBL
    integer,        intent(out)         :: ierr
    real(kind=KARG),intent(in)          :: d(*)
    integer,        intent(in)          :: n
    integer,        intent(in)          :: krect
    integer,        intent(in)          :: u
    logical,        intent(in),optional :: pre, post

    logical swap, lrec

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call ssq_write_lrec(ierr, u, d, n, swap)
    else
       call ssq_write_irec(ierr, u, d, n, swap, pre, post)
    endif
    return
  end subroutine put_data_record_d

  subroutine put_data_record_i1 &
       & (ierr, &
       &  d,  u, krect, pre, post)
    use TOUZA_Nng_io, only: &
         & ssq_write_lrec, ssq_write_irec
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)         :: ierr
    integer(kind=KARG),intent(in)          :: d
    integer,           intent(in)          :: krect
    integer,           intent(in)          :: u
    logical,           intent(in),optional :: pre, post

    logical swap, lrec
    integer(kind=KARG) :: b(1)

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    b(1) = d
    if (lrec) then
       call ssq_write_lrec(ierr, u, b, 1, swap)
    else
       call ssq_write_irec(ierr, u, b, 1, swap, pre, post)
    endif
    return
  end subroutine put_data_record_i1
  subroutine put_data_record_f1 &
       & (ierr, &
       &  d,  u, krect, pre, post)
    use TOUZA_Nng_io, only: &
         & ssq_write_lrec, ssq_write_irec
    implicit none
    integer,parameter :: KARG=KFLT
    integer,        intent(out)         :: ierr
    real(kind=KARG),intent(in)          :: d
    integer,        intent(in)          :: krect
    integer,        intent(in)          :: u
    logical,        intent(in),optional :: pre, post

    logical swap, lrec
    real(kind=KARG) :: b(1)

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    b(1) = d
    if (lrec) then
       call ssq_write_lrec(ierr, u, b, 1, swap)
    else
       call ssq_write_irec(ierr, u, b, 1, swap, pre, post)
    endif
    return
  end subroutine put_data_record_f1
  subroutine put_data_record_d1 &
       & (ierr, &
       &  d, u, krect, pre, post)
    use TOUZA_Nng_io, only: &
         & ssq_write_lrec, ssq_write_irec
    implicit none
    integer,parameter :: KARG=KDBL
    integer,        intent(out)         :: ierr
    real(kind=KARG),intent(in)          :: d
    integer,        intent(in)          :: krect
    integer,        intent(in)          :: u
    logical,        intent(in),optional :: pre, post

    logical swap, lrec
    real(kind=KARG) :: b(1)

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    b(1) = d
    if (lrec) then
       call ssq_write_lrec(ierr, u, b, 1, swap)
    else
       call ssq_write_irec(ierr, u, b, 1, swap, pre, post)
    endif
    return
  end subroutine put_data_record_d1
!!!_  & put_data_irecord - read integer data block with conversion
  subroutine put_data_irecord_f &
       & (ierr, &
       &  d,  n, u, krect, pre, post)
    implicit none
    integer,parameter :: KARG=KFLT, KSRC=KI32
    integer,        intent(out)         :: ierr
    real(kind=KARG),intent(in)          :: d(*)
    integer,        intent(in)          :: n
    integer,        intent(in)          :: krect
    integer,        intent(in)          :: u
    logical,        intent(in),optional :: pre, post


    integer(kind=KSRC) :: w(n)

    w(1:n) = int(d(1:n), kind=KSRC)
    call put_data_record_i (ierr, w, n, u, krect, pre, post)
    return
  end subroutine put_data_irecord_f
  subroutine put_data_irecord_d &
       & (ierr, &
       &  d,  n, u, krect, pre, post)
    implicit none
    integer,parameter :: KARG=KDBL, KSRC=KI32
    integer,        intent(out)         :: ierr
    real(kind=KARG),intent(in)          :: d(*)
    integer,        intent(in)          :: n
    integer,        intent(in)          :: krect
    integer,        intent(in)          :: u
    logical,        intent(in),optional :: pre, post

    integer(kind=KSRC) :: w(n)
    w(1:n) = int(d(1:n), kind=KSRC)
    call put_data_record_i (ierr, w, n, u, krect, pre, post)
    return
  end subroutine put_data_irecord_d
!!!_  & put_data_frecord - read float data block with conversion
  subroutine put_data_frecord_i &
       & (ierr, &
       &  d,  n, u, krect, pre, post)
    implicit none
    integer,parameter :: KARG=KI32, KSRC=KFLT
    integer,           intent(out)         :: ierr
    integer(kind=KARG),intent(in)          :: d(*)
    integer,           intent(in)          :: n
    integer,           intent(in)          :: krect
    integer,           intent(in)          :: u
    logical,           intent(in),optional :: pre, post

    real(kind=KSRC) :: w(n)
    w(1:n) = real(d(1:n), kind=KSRC)
    call put_data_record_f (ierr, w, n, u, krect, pre, post)
    return
  end subroutine put_data_frecord_i
  subroutine put_data_frecord_d &
       & (ierr, &
       &  d,  n, u, krect, pre, post)
    implicit none
    integer,parameter :: KARG=KDBL, KSRC=KFLT
    integer,        intent(out)         :: ierr
    real(kind=KARG),intent(in)          :: d(*)
    integer,        intent(in)          :: n
    integer,        intent(in)          :: krect
    integer,        intent(in)          :: u
    logical,        intent(in),optional :: pre, post

    real(kind=KSRC) :: w(n)
    w(1:n) = real(d(1:n), kind=KSRC)
    call put_data_record_f (ierr, w, n, u, krect, pre, post)
    return
  end subroutine put_data_frecord_d

!!!_  & put_data_drecord - read double data block with conversion
  subroutine put_data_drecord_i &
       & (ierr, &
       &  d,  n, u, krect, pre, post)
    implicit none
    integer,parameter :: KARG=KI32, KSRC=KDBL
    integer,           intent(out)         :: ierr
    integer(kind=KARG),intent(in)          :: d(*)
    integer,           intent(in)          :: n
    integer,           intent(in)          :: krect
    integer,           intent(in)          :: u
    logical,           intent(in),optional :: pre, post

    real(kind=KSRC) :: w(n)
    w(1:n) = real(d(1:n), kind=KSRC)
    call put_data_record_d (ierr, w, n, u, krect, pre, post)
    return
  end subroutine put_data_drecord_i
  subroutine put_data_drecord_f &
       & (ierr, &
       &  d,  n, u, krect, pre, post)
    implicit none
    integer,parameter :: KARG=KFLT, KSRC=KDBL
    integer,        intent(out)         :: ierr
    real(kind=KARG),intent(in)          :: d(*)
    integer,        intent(in)          :: n
    integer,        intent(in)          :: krect
    integer,        intent(in)          :: u
    logical,        intent(in),optional :: pre, post

    real(kind=KSRC) :: w(n)
    w(1:n) = real(d(1:n), kind=KSRC)
    call put_data_record_d (ierr, w, n, u, krect, pre, post)
    return
  end subroutine put_data_drecord_f

!!!_  & parse_header_base - parse minimum properties
  subroutine parse_header_base &
       & (ierr, kfmt, kaxs, vmiss, head)
    use TOUZA_Nng_header,only: litem, &
         & hi_DFMT,  hi_MISS,  &
         & hi_ASTR1, hi_ASTR2, hi_ASTR3, &
         & hi_AEND1, hi_AEND2, hi_AEND3, &
         & get_item
    use TOUZA_Nng_std,only: KDBL, KFLT, KI32, KI64
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: kfmt
    integer,         intent(out) :: kaxs(*)
    real(kind=KRMIS),intent(out) :: vmiss
    character(len=*),intent(in)  :: head(*)

    character(len=litem) :: vp
    integer idfm
    integer j

    ierr = err_default

    if (ierr.eq.0) then
       idfm = check_id_format(head)
       if (idfm.lt.0) ierr = idfm
    endif

    if (ierr.eq.0) call get_item(ierr, head, vp, hi_DFMT)
    if (ierr.eq.0) call parse_record_fmt(ierr, kfmt, vp)

    if (ierr.eq.0) call parse_record_cmem(ierr, kaxs(1), head, hi_ASTR1, hi_AEND1)
    if (ierr.eq.0) call parse_record_cmem(ierr, kaxs(2), head, hi_ASTR2, hi_AEND2)
    if (ierr.eq.0) call parse_record_cmem(ierr, kaxs(3), head, hi_ASTR3, hi_AEND3)

    if (ierr.eq.0) call get_item(ierr, head, vmiss, hi_MISS, def=def_VMISS)

  end subroutine parse_header_base

!!!_  & parse_header_size - parse size properties
  integer function parse_header_size_n &
       & (head, kidx) &
       & result (n)
    use TOUZA_Nng_header,only: &
         & hi_ASTR1, hi_ASTR2, hi_ASTR3, &
         & hi_AEND1, hi_AEND2, hi_AEND3
    implicit none
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: kidx
    integer jerr
    integer kaxs(3)
    jerr = 0
    select case (kidx)
    case (1)
       call parse_record_cmem(jerr, n, head, hi_ASTR1, hi_AEND1)
    case (2)
       call parse_record_cmem(jerr, n, head, hi_ASTR2, hi_AEND2)
    case (3)
       call parse_record_cmem(jerr, n, head, hi_ASTR3, hi_AEND3)
    case default
       jerr = 0
       if (jerr.eq.0) call parse_record_cmem(jerr, kaxs(1), head, hi_ASTR1, hi_AEND1)
       if (jerr.eq.0) call parse_record_cmem(jerr, kaxs(2), head, hi_ASTR2, hi_AEND2)
       if (jerr.eq.0) call parse_record_cmem(jerr, kaxs(3), head, hi_ASTR3, hi_AEND3)
       if (jerr.eq.0) n = kaxs(1) * kaxs(2) * kaxs(3)
    end select

    if (jerr.ne.0) n = -1
    return
  end function parse_header_size_n
  integer(kind=KI32) function parse_header_size_i &
       & (head, kidx, khld) &
       & result (n)
    use TOUZA_Nng_header,only: &
         & hi_ASTR1, hi_ASTR2, hi_ASTR3, &
         & hi_AEND1, hi_AEND2, hi_AEND3
    implicit none
    integer,parameter :: KARG=KI32
    character(len=*),  intent(in)  :: head(*)
    integer,           intent(in)  :: kidx
    integer(kind=KARG),intent(in)  :: khld
    integer jerr
    integer kaxs(3)
    jerr = 0
    select case (kidx)
    case (1)
       call parse_record_cmem(jerr, n, head, hi_ASTR1, hi_AEND1)
    case (2)
       call parse_record_cmem(jerr, n, head, hi_ASTR2, hi_AEND2)
    case (3)
       call parse_record_cmem(jerr, n, head, hi_ASTR3, hi_AEND3)
    case default
       jerr = 0
       if (jerr.eq.0) call parse_record_cmem(jerr, kaxs(1), head, hi_ASTR1, hi_AEND1)
       if (jerr.eq.0) call parse_record_cmem(jerr, kaxs(2), head, hi_ASTR2, hi_AEND2)
       if (jerr.eq.0) call parse_record_cmem(jerr, kaxs(3), head, hi_ASTR3, hi_AEND3)
       if (jerr.eq.0) n = kaxs(1) * kaxs(2) * kaxs(3)
    end select

    if (jerr.ne.0) n = -1
    return
  end function parse_header_size_i

!!!_  & parse_record_fmt - parse format
  subroutine parse_record_fmt &
       & (ierr, kfmt, str)
    use TOUZA_Nng_std,only: KDBL, KFLT, KI32, KI64
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: kfmt
    character(len=*),intent(in)  :: str
    integer kk

    ierr = 0
    kfmt = 0

    select case (str(1:1))
    case ('U')
       kfmt = 0
    case ('M')
       kfmt = GFMT_MASK
    case default
       ierr = -1
    end select

    if (ierr.eq.0) then
       select case (str(2:2))
       case ('R')
          select case (str(3:3))
          case ('C')
             if (str(4:4).eq.'2') then
                kfmt = kfmt + GFMT_URC2
             else
                kfmt = kfmt + GFMT_URC
             endif
          case ('Y', 'X')
             kfmt = kfmt + GFMT_URY
             read(str(4:), *, IOSTAT=ierr) kk
             if (ierr.eq.0) then
                if (kk.gt.(GFMT_URYend - GFMT_URY)) ierr = -1
                if (kk.lt.1) ierr = -1
             endif
             if (ierr.eq.0) kfmt = kfmt + kk
          case ('T')
             kfmt = kfmt + GFMT_URT
             read(str(4:), *, IOSTAT=ierr) kk
             if (ierr.eq.0) then
                kfmt = kfmt + min(max(0, kk), GFMT_URTend-GFMT_URT)
             else
                ierr = 0
             endif
          case default
             read(str(3:), *, IOSTAT=ierr) kk
             if (ierr.eq.0) then
                if (kk.eq.4) then
                   kfmt = kfmt + GFMT_UR4
                else if (kk.eq.8) then
                   kfmt = kfmt + GFMT_UR8
                else
                   ierr = -1
                endif
             endif
          end select
       case ('I')
          read(str(3:), *, IOSTAT=ierr) kk
          if (ierr.eq.0) then
             if (kk.eq.1) then
                kfmt = kfmt + GFMT_UI1
             else if  (kk.eq.4) then
                kfmt = kfmt + GFMT_UI4
             else if (kk.eq.8) then
                kfmt = kfmt + GFMT_UI8
             else
                ierr = -1
             endif
          endif
       case default
          ierr = -1
       end select
    endif
    if (ierr.ne.0) kfmt = GFMT_ERR

  end subroutine parse_record_fmt

!!!_  & parse_record_cmem - parse coordinate members
  subroutine parse_record_cmem &
       & (ierr, nmem, head, ibgn, iend)
    use TOUZA_Nng_header,only: get_item
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: nmem
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: ibgn, iend

    integer nmin, nmax

    ierr = 0
    if (ierr.eq.0) call get_item(ierr, head, nmin, ibgn)
    if (ierr.eq.0) call get_item(ierr, head, nmax, iend)
    if (ierr.eq.0) then
       nmem = nmax - nmin + 1
    else
       nmem = -1
    endif
  end subroutine parse_record_cmem

!!!_  & mask_encode - mask encoding
  subroutine mask_encode_di &
       & (ierr, mb,   nc,     icom,   b, &
       &  d,    n,    vmiss,  kpack)
    use TOUZA_Trp,only: count_packed, pack_store
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: mb        ! number of unmasked items
    integer,            intent(out) :: nc        ! packed mask length
    integer(kind=KISRC),intent(out) :: icom(*)   ! packed mask
    real(kind=KARG),    intent(out) :: b(*)      ! compressed data sequence
    real(kind=KARG),    intent(in)  :: d(*)      ! input
    integer,            intent(in)  :: n
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kpack

    integer :: imsk(n)
    integer(kind=KISRC),parameter :: khld = 0_KISRC
    integer ncom
    integer kp
    integer j

    ierr = 0
    nc   = count_packed(1, n, khld)

    where(d(1:n).eq.vmiss)
       imsk(1:n) = 0
    elsewhere
       imsk(1:n) = 1
    end where
    mb = 0
    do j = 1, n
       if (imsk(j).eq.1) then
          mb = mb + 1
          b(mb) = d(j)
       endif
    enddo
    call pack_store(ierr, icom, imsk, n, 1, kpack)
    return
  end subroutine mask_encode_di
  subroutine mask_encode_fi &
       & (ierr, mb,   nc,     icom,   b, &
       &  d,    n,    vmiss,  kpack)
    use TOUZA_Trp,only: count_packed, pack_store
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: mb        ! number of unmasked items
    integer,            intent(out) :: nc        ! packed mask length
    integer(kind=KISRC),intent(out) :: icom(*)   ! packed mask
    real(kind=KARG),    intent(out) :: b(*)      ! compressed data sequence
    real(kind=KARG),    intent(in)  :: d(*)      ! input
    integer,            intent(in)  :: n
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kpack

    integer :: imsk(n)
    integer(kind=KISRC),parameter :: khld = 0_KISRC
    integer ncom
    integer kp
    integer j

    ierr = 0
    nc   = count_packed(1, n, khld)

    where(d(1:n).eq.vmiss)
       imsk(1:n) = 0
    elsewhere
       imsk(1:n) = 1
    end where

    mb = 0
    do j = 1, n
       if (imsk(j).eq.1) then
          mb = mb + 1
          b(mb) = d(j)
       endif
    enddo
    call pack_store(ierr, icom, imsk, n, 1, kpack)
    return
  end subroutine mask_encode_fi
  subroutine mask_encode_ii &
       & (ierr, mb,   nc,     icom,   b, &
       &  d,    n,    vmiss,  kpack)
    use TOUZA_Trp,only: count_packed, pack_store
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: mb        ! number of unmasked items
    integer,            intent(out) :: nc        ! packed mask length
    integer(kind=KISRC),intent(out) :: icom(*)   ! packed mask
    integer(kind=KARG), intent(out) :: b(*)      ! compressed data sequence
    integer(kind=KARG), intent(in)  :: d(*)      ! input
    integer,            intent(in)  :: n
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kpack

    integer :: imsk(n)
    integer(kind=KISRC),parameter :: khld = 0_KISRC
    integer ncom
    integer kp
    integer j
    integer(kind=KARG) :: imiss

    ierr = 0
    nc   = count_packed(1, n, khld)
    imiss = int(vmiss, kind=KARG)

    where(d(1:n).eq.imiss)
       imsk(1:n) = 0
    elsewhere
       imsk(1:n) = 1
    end where

    mb = 0
    do j = 1, n
       if (imsk(j).eq.1) then
          mb = mb + 1
          b(mb) = d(j)
       endif
    enddo
    call pack_store(ierr, icom, imsk, n, 1, kpack)
    return
  end subroutine mask_encode_ii
!!!_  & mask_decode - mask decoding
  subroutine mask_decode_di &
       & (ierr,  d,     n,  &
       &  b,     icom,  &
       &  vmiss, kpack)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32
    integer,            intent(out) :: ierr
    real(kind=KARG),    intent(out) :: d(*)      ! output
    integer,            intent(in)  :: n
    real(kind=KARG),    intent(in)  :: b(*)      ! compressed data sequence
    integer(kind=KISRC),intent(in)  :: icom(*)   ! packed mask
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kpack

    integer :: imsk(n)
    integer(kind=KISRC),parameter :: khld = 0_KISRC
    integer j, jb

    ierr = 0
    if (ierr.eq.0) then
       call pack_restore(ierr, imsk, icom, n, 1, kpack)
    endif
    ! do j = 1, n
    !    write(*,*) 'MSK', j, imsk(j), icom(j)
    ! enddo
    if (ierr.eq.0) then
       jb = 1
       do j = 1, n
          if (imsk(j).eq.1) then
             d(j) = b(jb)
             jb = jb + 1
          else
             d(j) = vmiss
          endif
       enddo
    endif

    return
  end subroutine mask_decode_di
  subroutine mask_decode_fi &
       & (ierr,  d,     n,  &
       &  b,     icom,  &
       &  vmiss, kpack)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32
    integer,            intent(out) :: ierr
    real(kind=KARG),    intent(out) :: d(*)      ! output
    integer,            intent(in)  :: n
    real(kind=KARG),    intent(in)  :: b(*)      ! compressed data sequence
    integer(kind=KISRC),intent(in)  :: icom(*)   ! packed mask
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kpack

    integer :: imsk(n)
    integer(kind=KISRC),parameter :: khld = 0_KISRC
    integer j, jb

    ierr = 0
    if (ierr.eq.0) then
       call pack_restore(ierr, imsk, icom, n, 1, kpack)
    endif
    if (ierr.eq.0) then
       jb = 1
       do j = 1, n
          if (imsk(j).eq.1) then
             d(j) = b(jb)
             jb = jb + 1
          else
             d(j) = vmiss
          endif
       enddo
    endif

    return
  end subroutine mask_decode_fi
  subroutine mask_decode_ii &
       & (ierr,  d,     n,  &
       &  b,     icom,  &
       &  vmiss, kpack)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32
    integer,            intent(out) :: ierr
    integer(kind=KARG), intent(out) :: d(*)      ! output
    integer,            intent(in)  :: n
    integer(kind=KARG), intent(in)  :: b(*)      ! compressed data sequence
    integer(kind=KISRC),intent(in)  :: icom(*)   ! packed mask
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kpack

    integer :: imsk(n)
    integer(kind=KISRC),parameter :: khld = 0_KISRC
    integer j, jb
    integer(kind=KARG) :: imiss

    ierr = 0
    imiss = int(vmiss, kind=KARG)
    if (ierr.eq.0) then
       call pack_restore(ierr, imsk, icom, n, 1, kpack)
    endif
    if (ierr.eq.0) then
       jb = 1
       do j = 1, n
          if (imsk(j).eq.1) then
             d(j) = b(jb)
             jb = jb + 1
          else
             d(j) = imiss
          endif
       enddo
    endif

    return
  end subroutine mask_decode_ii

!!!_  & normalize_xry - ury core
  subroutine normalize_xry_d &
       & (ierr, idec, dma, d, nh, imiss, vmiss)
    implicit none
    integer,parameter ::  KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,            intent(out) :: ierr
    integer(kind=KISRC),intent(out) :: idec(*)
    real(kind=KRSRC),   intent(out) :: dma(*)
    real(kind=KARG),    intent(in)  :: d(*)
    integer,            intent(in)  :: nh
    integer(kind=KISRC),intent(in)  :: imiss ! integer index for missing (1...1)
    real(kind=KRMIS),   intent(in)  :: vmiss

    real(kind=KRSRC) :: VH, VL, VP, VN
    real(kind=KRSRC) :: v0, dv, fv
    real(kind=KRSRC),parameter :: zero = 0.0_KRSRC
    real(kind=KRSRC),parameter :: one  = 1.0_KRSRC
    integer :: KP, KN
    integer(kind=KISRC) :: mbin

    ierr = 0

    VH = MAXVAL(d(1:nh), d(1:nh).ne.vmiss)
    VL = MINVAL(d(1:nh), d(1:nh).ne.vmiss)

    mbin = max(0_KISRC, imiss - 1_KISRC)
    ! imiss =  1 3 7 15 ...
    ! mbin  =  0 2 6 14 ...

    ! write(*, *) 'normal:', VH, VL, vmiss, mbin
    if (VH.le.VL) then
       ! no data or d == const
       dv = zero
    else if (mbin.eq.0_KISRC) then
       ! (can prefer ZERO)
       ! prefer abs max
       if (ABS(VH).gt.ABS(VL)) then
          ! useless to set dv to decode, but value range is recorded for information
          dv = VL - VH
          VL = VH
       else
          dv = VH - VL
       endif
    else
       dv = (VH - VL) / REAL(mbin, KIND=KRSRC)
       if (VL.ge.zero.or.VH.lt.zero) then
          ! 0 <= d or d < 0
          continue
       else if (VH.eq.zero) then
          ! d <= 0
          VL = VH
          dv = -dv
       else
          ! L..y0x....H
          KN = FLOOR(- VL / dv)
          VN = VL + REAL(KN, KIND=KRSRC) * dv
          if (VN.eq.zero) then
             continue
          else
             KP = FLOOR(+ VH / dv)
             VP = VH - REAL(KP, KIND=KRSRC) * dv
             ! mbin >=2, thus either KP or KN >= 1
             if (VP.eq.zero) then
                ! should be much rare case
                VL = VH
                dv = -dv
             else if (VH.ge.-VL) then
                ! keep ZERO and ABS MAX
                ! LN..0.P.....H
                ! LN.0..P.....H
                dv = - VH / REAL(KP, KIND=KRSRC)
                VL = - dv * REAL(KP, KIND=KRSRC)
             else
                ! L.....N..0.PH
                ! L.....N.0..PH
                dv = - VL / REAL(KN, KIND=KRSRC)
                VL = - dv * REAL(KN, KIND=KRSRC)
             endif
          endif
       endif
    endif
    if (dv.eq.zero) then
       fv = zero
    else
       fv = one / dv
    endif
    where(d(1:nh).ne.vmiss)
       idec(1:nh) = MIN(MAX(0_KISRC, NINT((d(1:nh) - VL) * fv)), mbin)
    elsewhere
       idec(1:nh) = imiss
    end where

    dma(1) = VL
    dma(2) = dv

    return
  end subroutine normalize_xry_d

!!!_  - legacy_packing ()
  integer function legacy_packing (nbits, n) result (m)
    use TOUZA_Trp,only: suggest_filling
    implicit none
    integer,intent(in) :: nbits
    integer,intent(in) :: n
    integer kc
    kc = IAND(IOR(def_encode_legacy, KCODE_SEQUENTIAL), NOT(KCODE_TRANSPOSE))
    m = suggest_filling(nbits, n, kc)
    ! write (*, *) 'legacy', m, kc, def_encode_legacy, IOR(def_encode_legacy, KCODE_SEQUENTIAL)
  end function legacy_packing
!!!_  - legacy_unpacking ()
  integer function legacy_unpacking (nbits, n) result (m)
    use TOUZA_Trp,only: suggest_filling
    implicit none
    integer,intent(in) :: nbits
    integer,intent(in) :: n
    m = suggest_filling(nbits, n, def_decode_legacy, RELLENO_SEQUENTIAL)
  end function legacy_unpacking

!!!_  & check_id_format()
  integer function check_id_format &
       & (head) &
       & result(n)
    use TOUZA_Nng_header,only: hi_IDFM, get_item
    implicit none
    character(len=*),intent(in) :: head(*)
    integer jerr
    call get_item(jerr, head, n, hi_IDFM)
    if (jerr.eq.0) then
       if (n.ne.GFMT_ID_LEGACY) n = ERR_NOT_GTOOL_FORMAT
    else
       n = jerr
    endif
    return
  end function check_id_format

end module TOUZA_Nng_record

!!!_@ test_nng_record - test program
#ifdef TEST_NNG_RECORD
program test_nng_record
  use TOUZA_Std,       only: parse, get_param, arg_diag, arg_init
  use TOUZA_Nng_std,   only: KDBL,  KIOFS
  use TOUZA_Nng_header,only: nitem, litem
  use TOUZA_Nng_io,    only: ssq_write_irec, ssq_write_lrec
  use TOUZA_Nng_record
  implicit none
  integer ierr
  integer jarg
  integer ktest

  ierr = 0
  jarg = 0
101 format(A,' = ', I0)
  call init(ierr, stdv=-9)
  ! if (ierr.eq.0) call diag(ierr, u=-1, levv=+99)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call arg_init(ierr, levv=-9)
  if (ierr.eq.0) call parse(ierr)
  if (ierr.eq.0) call arg_diag(ierr)
  if (ierr.eq.0) then
     jarg = jarg + 1
     call get_param(ierr, ktest, jarg, 0)
  endif
  if (ierr.eq.0) then
201  format('##### TEST ', I0, 1x, A)
     select case (ktest)
     case (0)
        write(*, 201) ktest, 'format detection'
        call test_batch_record_fmt(ierr)
     case (1)
        write(*, 201) ktest, 'record-type detection'
        call test_auto_record(ierr, jarg)
     case (2)
        write(*, 201) ktest, 'read/write gtool file'
        call test_read_write_ext(ierr, jarg)
     case (3)
        write(*, 201) ktest, 'write with various encoding'
        call test_encoding(ierr, jarg)
     case default
        write(*, *) 'INVALID TEST = ', ktest
        ierr = -1
     end select
  endif

  if (ierr.eq.0) call finalize(ierr)
  write(*, 101) 'FINAL', ierr
  stop
contains
!!!_ + test_batch_record_fmt - format parser tests
  subroutine test_batch_record_fmt &
       & (ierr)
    integer,intent(out) :: ierr

    call test_record_fmt(ierr, 'UR8',   GFMT_UR8)
    call test_record_fmt(ierr, 'UR4',   GFMT_UR4)
    call test_record_fmt(ierr, 'URC',   GFMT_URC)
    call test_record_fmt(ierr, 'URC2',  GFMT_URC2)
    call test_record_fmt(ierr, 'URY01', GFMT_URY + 1)
    call test_record_fmt(ierr, 'URY31', GFMT_URY + 31)
    call test_record_fmt(ierr, 'UI4',   GFMT_UI1)

    call test_record_fmt(ierr, 'URT',   GFMT_URT)
    call test_record_fmt(ierr, 'URT24', GFMT_URT+24)

    call test_record_fmt(ierr, 'MR8',   GFMT_MR8)
    call test_record_fmt(ierr, 'MR4',   GFMT_MR4)
    call test_record_fmt(ierr, 'MRY01', GFMT_MRY + 1)
    call test_record_fmt(ierr, 'MRY31', GFMT_MRY + 31)
    call test_record_fmt(ierr, 'MI4',   GFMT_MI4)

    call test_record_fmt(ierr, 'UI6',   GFMT_ERR)
    call test_record_fmt(ierr, 'URY32', GFMT_ERR)
    call test_record_fmt(ierr, 'MI6',   GFMT_ERR)
    call test_record_fmt(ierr, 'MRY32', GFMT_ERR)
    return
  end subroutine test_batch_record_fmt
!!!_  - test_record_fmt - format parser test sub
  subroutine test_record_fmt &
       & (ierr, str, kans)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: str
    integer,         intent(in)  :: kans
    integer kfmt

    call parse_record_fmt(ierr, kfmt, str)
101 format('FORMAT[', A, '] = ', I0, 1x, I0, 1x, 'success')
102 format('FORMAT[', A, '] = ', I0, 1x, I0, 1x, 'fail ', I0)
    if (kfmt.eq.kans) then
       ierr = 0
    endif
    if (ierr.eq.0) then
       write(*, 101) trim(str), kfmt / GFMT_MASK, MOD(kfmt, GFMT_MASK)
    else
       write(*, 102) trim(str), kfmt / GFMT_MASK, MOD(kfmt, GFMT_MASK), kans
    endif

  end subroutine test_record_fmt

!!!_ + test_auto_record - auto record parser tests
  subroutine test_auto_record &
       & (ierr, jarg)
    use TOUZA_Nng_std,   only: KDBL,  KIOFS
    use TOUZA_Nng_header,only: nitem, litem, hi_ITEM, put_item, get_item
    use TOUZA_Nng_io,    only: &
         & ssq_open, ssq_close, ssq_write_irec, ssq_write_lrec
    use TOUZA_Nng_record
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jarg

    integer,parameter :: nhi = nitem + 4
    character(len=litem) hd(nhi)
    character(len=litem) hitm
    integer,parameter :: lxh = 4
    character(len=litem) xhd(lxh)
    integer krect
    integer j
    integer kfmt, kaxs(3)
    real(KIND=KDBL) :: vmiss
    character(len=1024) :: file

    integer jrec
    integer udat

    ierr = 0

    if (ierr.eq.0) then
       jarg = jarg + 1
       call get_param(ierr, file, jarg, ' ')
       if (file.eq.' ') then
          write(*, *) 'need file to write.'
          ierr = -1
       endif
    endif

    udat = 11
    hd(:nitem) = ' '
    hd(nitem+1:nhi) = 'xxx'
    xhd(:) = 'extra'

    if (ierr.eq.0) call set_test_header(ierr, hd)

    if (ierr.eq.0) then
       call ssq_open(ierr, udat, file, ACTION='W', STATUS='R')
       if (ierr.eq.0) call put_item(ierr, hd, 'def', hi_ITEM)
       if (ierr.eq.0) call ssq_write_irec(ierr, udat, hd, nhi)
       if (ierr.eq.0) call put_item(ierr, hd, 'swap', hi_ITEM)
       if (ierr.eq.0) call ssq_write_irec(ierr, udat, hd, nhi, swap=.TRUE.)
       if (ierr.eq.0) call put_item(ierr, hd, 'long', hi_ITEM)
       if (ierr.eq.0) call ssq_write_lrec(ierr, udat, hd, nhi)
       if (ierr.eq.0) call put_item(ierr, hd, 'long/swap', hi_ITEM)
       if (ierr.eq.0) call ssq_write_lrec(ierr, udat, hd, nhi, swap=.TRUE.)

       if (ierr.eq.0) call put_item(ierr, hd, 'cont', hi_ITEM)
       if (ierr.eq.0) call ssq_write_irec(ierr, udat, hd,  nhi, post=.TRUE.)
       if (ierr.eq.0) call ssq_write_irec(ierr, udat, xhd, lxh, pre=.TRUE.)

       if (ierr.eq.0) call put_item(ierr, hd, 'cont/swap', hi_ITEM)
       if (ierr.eq.0) call ssq_write_irec(ierr, udat, hd,  nhi, swap=.TRUE., post=.TRUE.)
       if (ierr.eq.0) call ssq_write_irec(ierr, udat, xhd, lxh, swap=.TRUE., pre=.TRUE.)

       if (ierr.eq.0) call ssq_close(ierr, udat, file)
    endif
301 format('FULL/W:', I0, 1x, I0, 1x, I0)
    write (*, 301) ierr

    if (ierr.eq.0) call ssq_open(ierr, udat, file, ACTION='R')

    jrec = 0
401 format('FULL:', I0, 1x, I0, 1x, I0)
411 format(I0, ': ', A, '/')
421 format('  FMT=', 2(1x, I0), 1x, 'COOR=', 3(1x,I0), 1x, E16.9)
    do
       if (ierr.eq.0) then
          hd(:) = ' '
          call nng_read_header(ierr, hd, krect, udat)
          write (*, 401) ierr, jrec, krect
       endif
       if (ierr.ne.0) then
          ierr = 0
          exit
       endif
       if (ierr.eq.0) then
          call get_item(ierr, hd, hitm, hi_ITEM)
          if (hd(j).ne.' ') write (*, 411) jrec, TRIM(ADJUSTL(hitm))
          ! do j = 1, nitem
          !    if (hd(j).ne.' ') write (*, 411) j, TRIM(ADJUSTL(hd(j)))
          ! enddo
       endif
       if (ierr.eq.0) then
          call parse_header_base(ierr, kfmt, kaxs, vmiss, hd)
          write(*, 421) kfmt / GFMT_MASK, MOD(kfmt, GFMT_MASK), kaxs(1:3), vmiss
       endif
       jrec = jrec + 1
    enddo
    return
  end subroutine test_auto_record

!!!_  - set_test_header - set dummy header
  subroutine set_test_header &
       & (ierr, hd)
    use TOUZA_Nng_header
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: hd(*)

    ierr = 0
    if (ierr.eq.0) call put_item(ierr, hd, 9010,   hi_IDFM)
    if (ierr.eq.0) call put_item(ierr, hd, 'test', hi_ITEM)
    if (ierr.eq.0) call put_item(ierr, hd, 'URY24',hi_DFMT)
    if (ierr.eq.0) call put_item(ierr, hd, 'm',    hi_UNIT)
    if (ierr.eq.0) call put_item(ierr, hd, 1,      hi_ASTR1)
    if (ierr.eq.0) call put_item(ierr, hd, 8,      hi_AEND1)
    if (ierr.eq.0) call put_item(ierr, hd, 'xx',   hi_AITM1)
    if (ierr.eq.0) call put_item(ierr, hd, 1,      hi_ASTR2)
    if (ierr.eq.0) call put_item(ierr, hd, 4,      hi_AEND2)
    if (ierr.eq.0) call put_item(ierr, hd, 'yy',   hi_AITM2)
    if (ierr.eq.0) call put_item(ierr, hd, 1,      hi_ASTR3)
    if (ierr.eq.0) call put_item(ierr, hd, 2,      hi_AEND3)
    if (ierr.eq.0) call put_item(ierr, hd, 'zz',   hi_AITM3)

    return
  end subroutine set_test_header
!!!_ + test_read_write_ext - read/write external gtool files
  subroutine test_read_write_ext &
       & (ierr, jarg)
    use TOUZA_Std,    only: get_param, upcase
    use TOUZA_Nng_std,only: KBUF=>KDBL
    use TOUZA_Nng_header
    use TOUZA_Nng_io,only: ssq_open, ssq_close
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jarg
    character(len=256) :: rfile, wfile
    character(len=32)  :: fmt

    integer uread, uwrite
    integer jrec
    integer j, n, l, nloop
    integer jpos

    integer,parameter :: nhi = nitem + 4
    character(len=litem) hd(nhi)
    integer krect, krectw
    integer,parameter :: lmax = 2 ** 24
    real(kind=KBUF),allocatable :: v(:)

    ierr = 0
101 format('test:', I0, 1x, A, 1x, A, 1x, A)
401 format('header/r:', I0, 1x, I0, 1x, I0)
402 format('data/r:', I0, 1x, I0)
301 format('v:', I0, 1x, I0, 1x, E24.16)
501 format('header/w:', I0, 1x, I0, 1x, I0)
502 format('data/w:', I0, 1x, I0, 1x, I0)

    jarg = jarg + 1
    if (ierr.eq.0) call get_param(ierr, rfile, jarg, ' ')
    if (rfile.eq.' ') return
    jarg = jarg + 1
    if (ierr.eq.0) call get_param(ierr, wfile, jarg, ' ')
    jarg = jarg + 1
    if (ierr.eq.0) call get_param(ierr, fmt, jarg, ' ')
    jarg = jarg + 1
    if (ierr.eq.0) call get_param(ierr, nloop, jarg, 0)
    nloop = max(0, nloop)
    ierr = 0

    if (fmt.eq.' ') fmt = 'UR8'
    call upcase(fmt)
    write(*, 101) jarg, trim(rfile), trim(wfile), trim(fmt)

    uread = 21

    if (ierr.eq.0) call ssq_open(ierr, uread,  rfile, ACTION='R')
    if (ierr.eq.0) then
       if (wfile.eq.' ') then
          uwrite = -1
       else
          uwrite = uread + 1
          call ssq_open(ierr, uwrite, wfile, ACTION='W', STATUS='R')
       endif
    endif

    if (ierr.eq.0) allocate(v(1:lmax), STAT=ierr)

    jrec = 0
    do
       if (ierr.eq.0) call nng_read_header(ierr, hd, krect, uread)
       write (*, 401) ierr, jrec, krect
       if (ierr.eq.0) call nng_read_data(ierr, v, lmax, hd, krect, uread)
       write (*, 402) ierr, jrec
       if (ierr.eq.0) then
          n = parse_header_size(hd, 0)
          if (uwrite.lt.0) then
             do j = 1, n
                write(*, 301) jrec, j, v(j)
             enddo
          else
             krectw = krect
             if (ierr.eq.0) call put_item(ierr, hd, trim(fmt), hi_DFMT)
             if (ierr.eq.0) call nng_write_header(ierr, hd, krectw, uwrite)
             write(*, 501) ierr, jrec, l
             if (ierr.eq.0) call nng_write_data(ierr, v, lmax, hd, krectw, uwrite)
             write(*, 502) ierr, jrec, l
             if (ierr.eq.0) inquire(unit=uwrite, IOSTAT=ierr, pos=JPOS)
             do l = 0, nloop - 1
                if (ierr.eq.0) call nng_write_header(ierr, hd, krectw, uwrite)
                if (ierr.eq.0) call nng_write_data(ierr, v, lmax, hd, krectw, uwrite)
                if (ierr.eq.0) write(unit=uwrite, IOSTAT=ierr, pos=JPOS)
             enddo
             if (ierr.ne.0) exit
          endif
       endif
       if (ierr.ne.0) then
          ierr = 0
          exit
       endif
       jrec = jrec + 1
    enddo
    if (ierr.eq.0) call ssq_close(ierr, uread, rfile)
    if (ierr.eq.0.and.uwrite.ge.0) call ssq_close(ierr, uwrite, wfile)
    if (ierr.eq.0) deallocate(v, STAT=ierr)
    return
  end subroutine test_read_write_ext

!!!_ + test_encoding - write extreme data
  subroutine test_encoding &
       & (ierr, jarg)
    use TOUZA_Std,only: endian_LITTLE, endian_BIG
    use TOUZA_Nng_std,only: KBUF=>KDBL
    use TOUZA_Nng_io, only: ssq_open,  ssq_close
    use TOUZA_Nng_header
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jarg
    integer nx, ny, nz, nn
    real(kind=KBUF) :: vmiss = -999.0_KBUF
    real(kind=KBUF),allocatable :: v(:)
    integer krect
    integer ufile
    integer kendi
    character(len=1024) :: wfile
    character(len=litem) :: hd(nitem)
    character(len=128)  :: tswap

    ierr = 0
    ufile = 41

    if (ierr.eq.0) then
       jarg = jarg + 1
       call get_param(ierr, wfile, jarg, ' ')
       if (wfile.eq.' ') then
          write(*, *) 'need file to write.'
          ierr = -1
          return
       endif
    endif
    krect = 0
    if (ierr.eq.0) then
       jarg = jarg + 1
       call get_param(ierr, tswap, jarg, ' ')
       select case (tswap(1:1))
       case ('B', 'b') ! big
          krect = REC_BIG
       case ('L', 'l') ! little
          krect = REC_LITTLE
       case ('S', 's') ! swap
          krect = REC_SWAP
       case default
          krect = 0
       end select
       call set_default_switch(ierr, krect)
    endif

    ! nx = 7
    ! ny = 11
    ! nz = 13
    nx = 5
    ny = 1
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

    if (ierr.eq.0) call ssq_open(ierr, ufile, wfile, ACTION='RW', STATUS='R')

    if (ierr.eq.0) then
       ! all zero
       v(:) = 0.0_KBUF
       call test_encoding_sub(ierr, hd, v, nn, nz, vmiss, ufile, '0')
    endif
    if (ierr.eq.0) then
       ! all positive
       v(:) = +1.0_KBUF
       call test_encoding_sub(ierr, hd, v, nn, nz, vmiss, ufile, '+1')
    endif
    if (ierr.eq.0) then
       ! all negative
       v(:) = -1.0_KBUF
       call test_encoding_sub(ierr, hd, v, nn, nz, vmiss, ufile, '-1')
    endif
    if (ierr.eq.0) then
       ! all missing
       v(:) = vmiss
       call test_encoding_sub(ierr, hd, v, nn, nz, vmiss, ufile, 'miss')
    endif
    if (ierr.eq.0) then
       ! all huge
       v(:) = +HUGE(vmiss)
       call test_encoding_sub(ierr, hd, v, nn, nz, vmiss, ufile, '+H')
    endif
    if (ierr.eq.0) then
       ! all huge
       v(:) = -HUGE(vmiss)
       call test_encoding_sub(ierr, hd, v, nn, nz, vmiss, ufile, '-H')
    endif
    if (ierr.eq.0) then
       ! all huge
       v(:) = +TINY(vmiss)
       call test_encoding_sub(ierr, hd, v, nn, nz, vmiss, ufile, '+T')
    endif
    if (ierr.eq.0) then
       ! all huge
       v(:) = -TINY(vmiss)
       call test_encoding_sub(ierr, hd, v, nn, nz, vmiss, ufile, '-T')
    endif

    if (ierr.eq.0) call ssq_close(ierr, ufile, wfile)

  end subroutine test_encoding
!!!_  - test_encoding_sub
  subroutine test_encoding_sub &
       & (ierr, hd, v, n, nz, vmiss, ufile, tag)
    use TOUZA_Nng_std,only: KBUF=>KDBL
    use TOUZA_Nng_header
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(inout) :: hd(*)
    real(kind=KBUF), intent(inout) :: v(*)
    real(kind=KBUF), intent(in)    :: vmiss
    integer,         intent(in)    :: ufile
    integer,         intent(in)    :: n, nz
    character(len=*),intent(in)    :: tag

    real(kind=KBUF),parameter :: x0 = 0.0_KBUF
    real(kind=KBUF),parameter :: x1 = 1.0_KBUF
    real(kind=KBUF),parameter :: x2 = 2.0_KBUF
    real(kind=KBUF),parameter :: xh = HUGE(x0)
    real(kind=KBUF),parameter :: xt = TINY(x0)

    ierr = 0

    if (ierr.eq.0) then
       call test_encoding_check(ierr, hd, v, n, nz, vmiss, ufile, tag, ' ')
    endif
    if (ierr.eq.0) then
       v(1) = x1
       call test_encoding_check(ierr, hd, v, n, nz, vmiss, ufile, tag, '+1')
    endif
    if (ierr.eq.0) then
       v(1) = -x1
       call test_encoding_check(ierr, hd, v, n, nz, vmiss, ufile, tag, '-1')
    endif
    if (ierr.eq.0) then
       v(1) = +xh
       call test_encoding_check(ierr, hd, v, n, nz, vmiss, ufile, tag, '+H')
    endif
    if (ierr.eq.0) then
       v(1) = -xh
       call test_encoding_check(ierr, hd, v, n, nz, vmiss, ufile, tag, '-H')
    endif
    if (ierr.eq.0) then
       v(1) = +xt
       call test_encoding_check(ierr, hd, v, n, nz, vmiss, ufile, tag, '+T')
    endif
    if (ierr.eq.0) then
       v(1) = -xt
       call test_encoding_check(ierr, hd, v, n, nz, vmiss, ufile, tag, '-T')
    endif

    return
  end subroutine test_encoding_sub

!!!_  - test_encoding_check
  subroutine test_encoding_check &
       & (ierr, hd, v, n, nz, vmiss, ufile, tag, tmod)
    use TOUZA_Nng_std,only: KBUF=>KDBL
    use TOUZA_Nng_header
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(inout) :: hd(*)
    real(kind=KBUF), intent(in)    :: v(*)
    real(kind=KBUF), intent(in)    :: vmiss
    integer,         intent(in)    :: ufile
    integer,         intent(in)    :: n, nz
    character(len=*),intent(in)    :: tag, tmod

    character(len=litem) :: hdi(nitem)
    character(len=litem) :: fmti
    character(len=litem) :: txt
    real(kind=KBUF),allocatable :: w(:)
    integer jrec
    integer krecti
    integer nh
    integer jz, jhb, jhe
    integer jpos

    ierr = 0

    nh = n / nz

101 format('############ ', A, 1x, A)
    write(*, 101) trim(tag), trim(tmod)

    ! if (ierr.eq.0) call ssq_open(ierr, ufile,  wfile, ACTION='W')

    if (ierr.eq.0) inquire(UNIT=ufile, IOSTAT=ierr, POS=jpos)

    write(txt, '(''extreme:'', A,''/'',A)') trim(tag), trim(tmod)

    if (ierr.eq.0) call put_item(ierr, hd, trim(txt), hi_ITEM)

    if (ierr.eq.0) call test_encoding_write(ierr, hd, v, n, ufile, 'URY01')
    if (ierr.eq.0) call test_encoding_write(ierr, hd, v, n, ufile, 'MRY01')
    if (ierr.eq.0) call test_encoding_write(ierr, hd, v, n, ufile, 'URY02')
    if (ierr.eq.0) call test_encoding_write(ierr, hd, v, n, ufile, 'MRY02')
    if (ierr.eq.0) call test_encoding_write(ierr, hd, v, n, ufile, 'URY31')
    if (ierr.eq.0) call test_encoding_write(ierr, hd, v, n, ufile, 'MRY31')

    if (ierr.eq.0) write(UNIT=ufile, IOSTAT=ierr, POS=jpos)

    if (ierr.eq.0) allocate(w(1:n), STAT=ierr)
    jrec = 0
301 format('extreme:check:', I0, 1x, A, 1x, I0, 1x, E16.9, 1x, '[', 2E9.1, '] [', 2E9.1, ']')
    do
       w(1:n) = 123.4e5_KBUF
       if (ierr.eq.0) call nng_read_header(ierr, hdi, krecti, ufile)
       if (ierr.ne.0) then
          ierr = 0
          exit
       endif
       if (ierr.eq.0) call nng_read_data(ierr, w, n, hdi, krecti, ufile)
       if (ierr.eq.0) call get_item(ierr, hdi, fmti, hi_DFMT)
       if (ierr.eq.0) then
          do jz = 0, nz - 1
             jhb = nh * jz + 1
             jhe = nh * (jz + 1)
             write(*, 301) jrec, trim(fmti), jz, &
                  & maxval(abs(w(jhb:jhe)-v(jhb:jhe))), &
                  & maxval(w(jhb:jhe)), minval(w(jhb:jhe)), &
                  & maxval(v(jhb:jhe)), minval(v(jhb:jhe))
          enddo
          ! write(*, *) w(1:n)
          ! write(*, *) v(1:n)
       endif
       jrec = jrec + 1
    enddo
    ! if (ierr.eq.0) call ssq_close(ierr, ufile, wfile)
    if (ierr.eq.0) deallocate(w, STAT=ierr)
    return
  end subroutine test_encoding_check

  subroutine test_encoding_write &
       & (ierr, hd, v, n, u, fmt)
    use TOUZA_Nng_std,only: KBUF=>KDBL
    use TOUZA_Nng_header
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(inout) :: hd(*)
    real(kind=KBUF), intent(in)    :: v(*)
    integer,         intent(in)    :: n
    integer,         intent(in)    :: u
    character(len=*),intent(in)    :: fmt

    integer krectw

    ierr = 0
    krectw = REC_DEFAULT
    if (ierr.eq.0) call put_item(ierr, hd, trim(fmt), hi_DFMT)
    if (ierr.eq.0) call nng_write_header(ierr, hd, krectw, u)
    if (ierr.eq.0) call nng_write_data(ierr, v, n, hd, krectw, u)
101 format('extreme:', A, ': ', I0)
    write(*, 101) trim(fmt), ierr
    return
  end subroutine test_encoding_write

end program test_nng_record

#endif /* TEST_NNG_RECORD */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

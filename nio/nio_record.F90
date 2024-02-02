!!!_! nio_record.F90 - TOUZA/Nio record interfaces
! Maintainer: SAITO Fuyuki
! Created: Oct 29 2021
#define TIME_STAMP 'Time-stamp: <2024/02/02 09:43:47 fuyuki nio_record.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021, 2022, 2023
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_nio.h"
!!!_@ TOUZA_Nio_record - nio record interfaces
module TOUZA_Nio_record
!!!_ = declaration
  use TOUZA_Nio_std,only: KI32, KI64, KDBL, KFLT
  use TOUZA_Nio_std,only: get_logu,      unit_global,  trace_fine,    trace_control
  use TOUZA_Nio_std,only: ignore_bigger, ignore_small, ignore_always, def_block
  use TOUZA_Nio_std,only: search_from_last
  use TOUZA_Nio_header,only: litem, nitem
  use TOUZA_Trp,only: KCODE_CLIPPING,  KCODE_ROUND
  use TOUZA_Trp,only: KCODE_TRANSPOSE, KCODE_SEQUENTIAL, KCODE_INCREMENTAL
  use TOUZA_Trp,only: KCODE_MANUAL,    RELLENO_SEQUENTIAL
  implicit none
  private
!!!_  - public parameters
  integer,parameter,public :: KRMIS=KDBL ! real kind of vmiss

  integer,parameter,public :: REC_ERROR     = -9
  integer,parameter,public :: REC_ASIS      = -1
  integer,parameter,public :: REC_DEFAULT   = 0
  integer,parameter,public :: REC_SWAP      = 1  ! with byte-swap
  integer,parameter,public :: REC_LSEP      = 2  ! with 64-bit record marker
  integer,parameter,public :: REC_BIG       = 4  ! force big-endian    (use in init only)
  integer,parameter,public :: REC_LITTLE    = 8  ! force little-endian (use in init only)
  integer,parameter,public :: REC_SUB_ALLOW = 16 ! enable subrecord separator mode (Big-GTOOL3 disabled at write)

  integer,parameter,public :: REC_LEFT  = 2**8 ! number of extra header items (shift)

  integer,parameter :: HEADER_LIMIT  = 2**16     ! 00 00 01 00 [l]
  integer,parameter :: HEADER_SPACES = 538976288 ! 20 20 20 20 [l]
  character(len=*),parameter :: HEADER_FILLS = '    '  ! same as HEADER_SPACES

  integer,parameter :: GFMT_ID_LEGACY = 9010

  integer,parameter,public :: BODR_ASSUME_SYSTEM  = 0    ! assume file byte-order == system
  integer,parameter,public :: BODR_ASSUME_FILE    = 1    ! assume file byte-order == common
  integer,parameter,public :: BODR_CHECK_VERBOSE  = 2    ! check for each unit at open-write

!!!_   . formats
  integer,parameter,public :: GFMT_ERR  = -1
  integer,parameter,public :: GFMT_MASK = 2048  /* mask bit */
  integer,parameter,public :: GFMT_LPAD = 4096  /* list-padding bit */
  integer,parameter,public :: GFMT_SUBV = GFMT_LPAD + 1024  /* list-padding subscript vector */

  integer,parameter,public :: GFMT_UR8  = 1
  integer,parameter,public :: GFMT_UR4  = 2
  integer,parameter,public :: GFMT_URC  = 3
  integer,parameter,public :: GFMT_URC2 = 4

  integer,parameter,public :: GFMT_MR8  = GFMT_UR8 + GFMT_MASK
  integer,parameter,public :: GFMT_MR4  = GFMT_UR4 + GFMT_MASK

  integer,parameter,public :: GFMT_PR8  = GFMT_UR8 + GFMT_LPAD
  integer,parameter,public :: GFMT_PR4  = GFMT_UR4 + GFMT_LPAD

  integer,parameter,public :: GFMT_URY    = 32
  integer,parameter,public :: GFMT_URYend = GFMT_URY + BIT_SIZE(0_KI32)
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

  integer,parameter,public :: GFMT_PI1   = GFMT_UI1 + GFMT_LPAD
  integer,parameter,public :: GFMT_PI4   = GFMT_UI4 + GFMT_LPAD
  integer,parameter,public :: GFMT_PI8   = GFMT_UI8 + GFMT_LPAD
  integer,parameter,public :: GFMT_PI4SV = GFMT_UI4 + GFMT_SUBV

  integer,parameter,public :: GFMT_URT  = 256
  integer,parameter,public :: GFMT_MRT  = GFMT_URT + GFMT_MASK

  integer,parameter,public :: GFMT_END  = GFMT_LPAD * 2
!!!_    * URT details
  integer,parameter,public :: PROP_DEFAULT = (- HUGE(0)) - 1

  integer,parameter,public :: PROP_URT_MANTISSA = 1 /* mantissa bits */
  integer,parameter,public :: PROP_URT_XBITS    = 2 /* exponent bits */
  integer,parameter,public :: PROP_URT_XBOTTOM  = 3 /* exponent lower limit (relative to exp(1)) */
  integer,parameter,public :: PROP_URT_XTOP     = 4 /* exponent lower limit (relative to exp(1)) */
  integer,parameter,public :: PROP_URT_CODES    = 5 /* kcode switches */
  integer,parameter,public :: PROP_URT_BREAK    = 6 /* array break */

  ! positive reserved for explicit chunk size
  integer,parameter,public :: GFMT_URT_BREAK_NONE  = 0
  integer,parameter,public :: GFMT_URT_BREAK_LEVEL = -1
  integer,parameter,public :: GFMT_URT_BREAK_PROC  = -2
  integer,parameter,public :: GFMT_URT_BREAK_LPROC = -3

  ! integer,parameter,public :: XID_URT = 1632916053 ! 'URTa'
  integer,parameter,public :: XID_URT = 1649693269 ! 'URTb'
  ! integer,parameter,public :: XID_MRT = 1632916045 ! 'MRTa'
  integer,parameter,public :: XTRP_ID = 0  ! index to store nio/urt i
  integer,parameter,public :: XTRP_NX = 1  ! index to store nio/urt number of extra properties

  integer,parameter,public :: lopts = 6

!!!_   . data reviewing flag
  integer,parameter,public :: rev_pos_dhead = 1   ! rewind to data heads at exit
  integer,parameter,public :: rev_pos_leave = 0   ! leave position at exit
!!!_   . packed data expansion flag
  integer,parameter,public :: packed_ignore = -1
  integer,parameter,public :: packed_read   = 0
  integer,parameter,public :: packed_check  = 1

  integer,parameter :: packed_ends_coordinate = 3

  character,parameter,public :: sep_subvitem = ' '
!!!_  - private parameter
  integer,parameter :: GPROP_NUMX  = 1
  integer,parameter :: GPROP_NUMY  = 2
  integer,parameter :: GPROP_NUMZ  = 3

  integer,parameter :: GPROP_VMISS = 1

  integer,parameter :: IMISS_URC = 65534

  integer,parameter :: MAGIC_DUMMY_SEP = -1

  integer,parameter :: laxs = 3
!!!_  - static
  real(KIND=KRMIS),save :: def_VMISS = -999.0_KRMIS
  integer,save :: def_encode_legacy   = KCODE_MANUAL   ! automatic KCODE_SEQUENTIAL
  integer,save :: def_decode_legacy   = KCODE_MANUAL   ! automatic KCODE_SEQUENTIAL
  integer,save :: def_encode_trapiche = KCODE_TRANSPOSE + KCODE_MANUAL
  integer,save :: def_decode_trapiche = KCODE_MANUAL

  integer,save :: def_krectw = 0  !  default record switch to write
  integer,save :: sw_subrec = ignore_bigger

  character(len=litem),save :: head_def(nitem) = ' '

  integer,save :: bodr_wnative = BODR_ASSUME_SYSTEM

  integer,save :: def_lazy_size = -1   ! lazy level for size parser

  integer,save :: lrec_urt = 0
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = NIO_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
#define __MDL__ 'r'
#define _ERROR(E) (E - ERR_MASK_NIO_RECORD)

  integer,save :: nlhead_std = 0 ! standard gtool header total length in bytes
  integer,save :: nisep      = 0 ! marker length as character (32-bit)
  integer,save :: nlsep      = 0 ! marker length as character (64-bit)

  integer,save :: udiag = -2    ! io unit to diag urt properties

  integer,save :: uwork = search_from_last  ! shared io-unit for temporal works

!!!_   . common work area
  integer,        allocatable,save :: worki(:)
  real(kind=KFLT),allocatable,save :: workf(:)
  real(kind=KDBL),allocatable,save :: workd(:)

  integer(kind=KI32),allocatable,save :: wpack(:)   ! packed

  integer(kind=KI32),allocatable,save :: wmask(:)   ! mask
  integer,           allocatable,save :: wdsubv(:)  ! subscript vector (destination)
  integer,           allocatable,save :: wssubv(:)  ! subscript vector (source)

  integer,parameter :: lwtag = 32
  character(len=lwtag),save :: wtag_i = ' ', wtag_f  = ' ', wtag_d = ' '
  character(len=lwtag),save :: wtag_p = ' ', wtag_m  = ' ', wtag_s = ' '
!!!_  - interfaces
  interface nio_read_data
     module procedure nio_read_data_f, nio_read_data_d, nio_read_data_i
  end interface nio_read_data
  interface nio_write_data
     module procedure nio_write_data_f, nio_write_data_d, nio_write_data_i
  end interface nio_write_data
  interface nio_read_data_slice
     module procedure nio_read_data_slice_f, nio_read_data_slice_d, nio_read_data_slice_i
  end interface nio_read_data_slice
  interface nio_read_data_packed
     module procedure nio_read_data_packed_f, nio_read_data_packed_d, nio_read_data_packed_i
  end interface nio_read_data_packed

  interface nio_read_data_core
     module procedure nio_read_data_core_f, nio_read_data_core_d, nio_read_data_core_i
  end interface nio_read_data_core
  interface nio_write_data_core
     module procedure nio_write_data_core_f, nio_write_data_core_d, nio_write_data_core_i
  end interface nio_write_data_core

  interface get_data_record
     module procedure get_data_record_i,  get_data_record_f,  get_data_record_d
     ! module procedure get_data_record_i1, get_data_record_f1, get_data_record_d1
     module procedure get_data_record_i1, get_data_record_d1
  end interface get_data_record

  interface get_data_record_slice
     module procedure get_data_record_slice_i, get_data_record_slice_f, get_data_record_slice_d
  end interface get_data_record_slice
  interface get_data_record_runl
     ! module procedure get_data_record_runl_i, get_data_record_runl_f, get_data_record_runl_d
     module procedure get_data_record_runl_i
  end interface get_data_record_runl
  interface get_data_record_list
     module procedure get_data_record_list_i, get_data_record_list_f, get_data_record_list_d
  end interface get_data_record_list
  interface get_data_record_suspend
     ! module procedure get_data_record_suspend_i, get_data_record_suspend_f, get_data_record_suspend_d
     module procedure get_data_record_suspend_i
  end interface get_data_record_suspend

  interface put_data_record
     module procedure put_data_record_i,  put_data_record_f,  put_data_record_d
     ! module procedure put_data_record_i1, put_data_record_f1, put_data_record_d1
     module procedure put_data_record_i1
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

  interface put_data_mi4
     module procedure put_data_mi4_f, put_data_mi4_d, put_data_mi4_i
  end interface put_data_mi4

  interface put_data_pr4
     module procedure put_data_pr4_f, put_data_pr4_d, put_data_pr4_i
  end interface put_data_pr4
  interface put_data_pr8
     module procedure put_data_pr8_f, put_data_pr8_d, put_data_pr8_i
  end interface put_data_pr8
  interface put_data_pi4
     module procedure put_data_pi4_f, put_data_pi4_d, put_data_pi4_i
  end interface put_data_pi4

  interface get_data_pr4
     module procedure get_data_pr4_f, get_data_pr4_d, get_data_pr4_i
  end interface get_data_pr4
  interface get_data_pr8
     module procedure get_data_pr8_f, get_data_pr8_d, get_data_pr8_i
  end interface get_data_pr8
  interface get_data_pi4
     module procedure get_data_pi4_f, get_data_pi4_d, get_data_pi4_i
  end interface get_data_pi4

  interface restore_mr8_plain
     module procedure restore_mr8_plain_d, restore_mr8_plain_f, restore_mr8_plain_i
  end interface restore_mr8_plain
  interface restore_mr4_plain
     module procedure restore_mr4_plain_d, restore_mr4_plain_f, restore_mr4_plain_i
  end interface restore_mr4_plain
  interface restore_mi4_plain
     module procedure restore_mi4_plain_d, restore_mi4_plain_f, restore_mi4_plain_i
  end interface restore_mi4_plain

  interface restore_ur8_packed
     module procedure restore_ur8_packed_d, restore_ur8_packed_f, restore_ur8_packed_i
  end interface restore_ur8_packed
  interface restore_ur4_packed
     module procedure restore_ur4_packed_d, restore_ur4_packed_f, restore_ur4_packed_i
  end interface restore_ur4_packed
  interface restore_ui4_packed
     module procedure restore_ui4_packed_d, restore_ui4_packed_f, restore_ui4_packed_i
  end interface restore_ui4_packed

  interface restore_mr8_packed
     module procedure restore_mr8_packed_d, restore_mr8_packed_f, restore_mr8_packed_i
  end interface restore_mr8_packed
  interface restore_mr4_packed
     module procedure restore_mr4_packed_d, restore_mr4_packed_f, restore_mr4_packed_i
  end interface restore_mr4_packed
  interface restore_mi4_packed
     module procedure restore_mi4_packed_d, restore_mi4_packed_f, restore_mi4_packed_i
  end interface restore_mi4_packed

  interface restore_pr8_packed
     module procedure restore_pr8_packed_d, restore_pr8_packed_f, restore_pr8_packed_i
  end interface restore_pr8_packed
  interface restore_pr4_packed
     module procedure restore_pr4_packed_d, restore_pr4_packed_f, restore_pr4_packed_i
  end interface restore_pr4_packed
  interface restore_pi4_packed
     module procedure restore_pi4_packed_d, restore_pi4_packed_f, restore_pi4_packed_i
  end interface restore_pi4_packed

  interface restore_ptn_plain_subv
     module procedure restore_ptn_plain_subv_i, restore_ptn_plain_subv_f, restore_ptn_plain_subv_d
  end interface restore_ptn_plain_subv

  interface normalize_xry
     module procedure normalize_xry_d
  end interface normalize_xry
  interface mask_encode
     module procedure mask_encode_di, mask_encode_fi, mask_encode_ii
  end interface mask_encode
  interface mask_decode
     module procedure mask_decode_ddi, mask_decode_dfi, mask_decode_dii
     module procedure mask_decode_fdi, mask_decode_ffi, mask_decode_fii
     module procedure mask_decode_idi, mask_decode_ifi, mask_decode_iii
  end interface mask_decode
  interface subv_decode
     module procedure subv_decode_dd, subv_decode_df, subv_decode_di
     module procedure subv_decode_fd, subv_decode_ff, subv_decode_fi
     module procedure subv_decode_id, subv_decode_if, subv_decode_ii
  end interface subv_decode
  interface subv_encode
     module procedure subv_encode_d, subv_encode_f, subv_encode_i
  end interface subv_encode
  interface mask_decode_subv
     module procedure mask_decode_subv_i
  end interface mask_decode_subv

  interface pack_plain_data
     module procedure pack_plain_data_dd, pack_plain_data_df, pack_plain_data_di
     module procedure pack_plain_data_fd, pack_plain_data_ff, pack_plain_data_fi
     module procedure pack_plain_data_id, pack_plain_data_if, pack_plain_data_ii
  end interface pack_plain_data

  interface parse_header_size
     module procedure parse_header_size_n
     module procedure parse_header_size_i
  end interface parse_header_size

  interface nio_check_magic_file
     module procedure nio_check_magic_file_name, nio_check_magic_file_unit
  end interface nio_check_magic_file
  interface review_ur8
     module procedure review_ur8_d, review_ur8_f, review_ur8_i
  end interface review_ur8
  interface review_ur4
     module procedure review_ur4_d, review_ur4_f, review_ur4_i
  end interface review_ur4
  interface review_ui4
     module procedure review_ui4_d, review_ui4_f, review_ui4_i
  end interface review_ui4
!!!_  - public procedures
  public init, diag, finalize
  public set_default_switch
  public set_default_header, get_default_header
  public nio_check_magic_file
  public nio_read_header,    nio_write_header
  public nio_read_data,      nio_write_data
  public nio_read_data_slice
  public nio_read_data_packed
  public nio_skip_records
  public nio_bwd_record
  public parse_header_base,  parse_record_fmt
  public parse_header_size
  public get_header_cprop,   put_header_cprop
  public set_urt_defs,       parse_urt_options,  show_urt_options
  public switch_urt_diag
  public set_switch_subrec,  nio_allow_sub
  public review_ur8,         review_ur4,         review_ui4
  public review_mtn,         review_ptn
  public set_bodr_wnative
  public subv_encode

  public nio_count_defined, decompose_packed_item
!!!_  - public shared
!!!_   . from Std
  public ignore_bigger, ignore_small, ignore_always, def_block
!!!_   . from Trp
  public KCODE_CLIPPING
  public KCODE_TRANSPOSE, KCODE_SEQUENTIAL, KCODE_INCREMENTAL
  public KCODE_MANUAL,    RELLENO_SEQUENTIAL

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init &
       & (ierr,   u,      levv,   mode,  stdv,  &
       &  bodrw,  krectw, subrec, klenc, kldec, knenc, kndec, lazy, &
       &  vmiss,  utime,  csign,  msign, icomm)
    use TOUZA_Nio_std,   only: control_mode,  control_deep, is_first_force
    use TOUZA_Nio_std,   only: ns_init=>init, choice, get_size_bytes, KDBL, max_members
    use TOUZA_Nio_header,only: nh_init=>init, litem, nitem
    use TOUZA_Trp,       only: trp_init=>init
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv, mode, stdv
    integer,         intent(in),optional :: bodrw       ! byte-order native flag
    integer,         intent(in),optional :: krectw      ! default record switch (write)
    integer,         intent(in),optional :: subrec      ! default subrecord switch (read)
    integer,         intent(in),optional :: klenc,kldec ! packing method for legacy-format (ury)
    integer,         intent(in),optional :: knenc,kndec ! packing method for new format (urt)
    integer,         intent(in),optional :: lazy        ! lazy level for size parse
    real(kind=KDBL), intent(in),optional :: vmiss       ! default header properties
    character(len=*),intent(in),optional :: utime
    character(len=*),intent(in),optional :: csign, msign
    integer,         intent(in),optional :: icomm
    integer lv, md, lmd
    character(len=litem) :: hdummy

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
       endif
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call trp_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv)
       endif
       if (init_counts.eq.0) then
          nlhead_std = get_size_bytes(hdummy, nitem)
          nisep = (get_size_bytes(0_KI32) * litem) / get_size_bytes(hdummy)
          nlsep = (get_size_bytes(0_KI64) * litem) / get_size_bytes(hdummy)

          if (ierr.eq.0) call set_bodr_wnative(ierr, bodrw, ulog, lv)
          if (ierr.eq.0) call set_default_switch(ierr, krectw, ulog, lv)
          if (ierr.eq.0) sw_subrec = choice(sw_subrec, subrec)
          if (ierr.eq.0) then
             call init_batch(ierr, klenc, kldec, knenc, kndec, ulog, lv)
          endif
          if (ierr.eq.0) then
             call set_default_header(ierr, vmiss, utime, csign, msign)
          endif
          if (ierr.eq.0) lrec_urt = max_members(1)
       endif
       def_lazy_size = max(-1, min(+1, choice(def_lazy_size, lazy)))
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = _ERROR(ERR_FAILURE_INIT)
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Nio_std,   only: control_mode,  control_deep, is_first_force
    use TOUZA_Nio_std,   only: ns_diag=>diag, choice, msg, is_msglev_normal, is_msglev_info
    use TOUZA_Nio_std,   only: gen_tag
    use TOUZA_Nio_header,only: nh_diag=>diag, show_header
    use TOUZA_Trp,       only: trp_diag=>diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd
    logical swap, lsep, bendi, lendi, asub
    character(len=128) :: txt

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
             if (is_msglev_normal(lv)) call msg('(''byte-order assumption = '', I0)', bodr_wnative, __MDL__, utmp)
             if (is_msglev_normal(lv)) call msg('(''lazy-level = '', I0)', def_lazy_size, __MDL__, utmp)
             if (is_msglev_normal(lv)) then
                swap  = IAND(def_krectw, REC_SWAP).gt.0
                lsep  = IAND(def_krectw, REC_LSEP).gt.0
                bendi = IAND(def_krectw, REC_BIG).gt.0
                lendi = IAND(def_krectw, REC_LITTLE).gt.0
                asub  = IAND(def_krectw, REC_SUB_ALLOW).gt.0
101             format('swap=', L1, ' lsep=', L1, ' endi=', 2L1, ' sub=', L1)
                write(txt, 101) swap, lsep, bendi, lendi, asub
                call msg(txt, __MDL__, utmp)
             endif
             if (is_msglev_info(lv)) then
                call gen_tag(txt, __MDL__, asfx='default', label=.TRUE.)
                call show_header(ierr, head_def, txt, utmp, lv)
             endif
             if (is_msglev_info(lv)) then
                call msg('(''standard heeder length = '', I0, 1x, I0, 1x, I0)', &
                     &   (/nlhead_std, nisep, nlsep/), __MDL__, utmp)
             endif
             if (is_msglev_info(lv)) then
                call msg('(''urt limit record elements = '', I0)', (/lrec_urt/), __MDL__, utmp)
             endif
             if (is_msglev_normal(lv)) then
                if (ierr.eq.0) call diag_works(ierr, utmp, lv)
             endif
          endif
       endif
       lmd = control_deep(md, mode)
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
    use TOUZA_Nio_std,   only: control_mode,  control_deep, is_first_force
    use TOUZA_Nio_std,   only: ns_finalize=>finalize, choice
    use TOUZA_Nio_header,only: nh_finalize=>finalize
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
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call nh_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call ns_finalize(ierr, utmp, levv=lv, mode=lmd)
       endif
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call trp_finalize(ierr, utmp, levv=lv, mode=lmd)
       endif
       if (ierr.eq.0) call finalize_destroy(ierr)
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize
!!!_  - init subcontracts
!!!_   & set_bodr_wnative
  subroutine set_bodr_wnative(ierr, bodrw, u, levv)
    use TOUZA_Nio_std,only: &
         & choice, msg, is_msglev_info, is_msglev_fatal, &
         & kendi_mem, kendi_file
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: bodrw
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer lv

    ierr = 0
    lv = choice(lev_verbose, levv)

    bodr_wnative = choice(bodr_wnative, bodrw)
    select case (bodr_wnative)
    case (BODR_ASSUME_SYSTEM)
       if (is_msglev_info(lv)) then
          call msg('(''assume system byte-order when write = '', I0)', kendi_mem, __MDL__, u)
       endif
    case (BODR_ASSUME_FILE)
       if (is_msglev_info(lv)) then
          call msg('(''assume estimated file byte-order when write = '', I0)', kendi_file, __MDL__, u)
       endif
    case (BODR_CHECK_VERBOSE)
       if (is_msglev_info(lv)) then
          call msg('check file byte-order when write',  __MDL__, u)
       endif
    case default
       ierr = -1
       if (is_msglev_fatal(lv)) then
          call msg('(''invalid byte-order switch = '', I0)', bodr_wnative, __MDL__, u)
       endif
    end select
    return
  end subroutine set_bodr_wnative

!!!_   . set_default_switch
  subroutine set_default_switch &
       & (ierr, krectw, u, levv)
    use TOUZA_Nio_std,only: choice, msg, is_msglev_fatal
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
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: klenc,kldec ! packing method for legacy-format (ury)
    integer,intent(in),optional :: knenc,kndec ! packing method for new format (urt)
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv

    ierr = 0 * choice(0, u) * choice(0, levv)

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
    use TOUZA_Nio_header,only: &
         & put_item, &
         & hi_IDFM,  hi_UTIM,  hi_FNUM,  hi_DNUM,  &
         & hi_ASTR1, hi_AEND1, hi_ASTR2, hi_AEND2, hi_ASTR3, hi_AEND3,  &
         & hi_MISS,  hi_DMIN,  hi_DMAX,  hi_DIVS,  hi_DIVL,  &
         & hi_STYP,  hi_IOPTN, hi_ROPTN, hi_CSIGN, hi_MSIGN
    use TOUZA_Nio_std,only: KDBL
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

!!!_  - diag subcontracts
  subroutine diag_works(ierr, u, levv)
    use TOUZA_Nio_std,only: choice, is_msglev_info, msg
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u, levv
    integer utmp, lv
    integer n
    ierr = 0
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)
    if (allocated(worki)) then
       n = size(worki)
    else
       n = -1
    endif
    if (is_msglev_info(levv)) then
       call msg('(''work:i = '', I0)', n, __MDL__, u)
    endif
    if (allocated(workf)) then
       n = size(workf)
    else
       n = -1
    endif
    if (is_msglev_info(levv)) then
       call msg('(''work:f = '', I0)', n, __MDL__, u)
    endif
    if (allocated(workd)) then
       n = size(workd)
    else
       n = -1
    endif
    if (is_msglev_info(levv)) then
       call msg('(''work:d = '', I0)', n, __MDL__, u)
    endif
    return
  end subroutine diag_works

!!!_  - finalize subcontracts
!!!_   . finalize_destroy
  subroutine finalize_destroy(ierr)
    implicit none
    integer,intent(out) :: ierr
    ierr = 0
    if (allocated(worki)) then
       if (ierr.eq.0) deallocate(worki, STAT=ierr)
    endif
    if (allocated(workf)) then
       if (ierr.eq.0) deallocate(workf, STAT=ierr)
    endif
    if (allocated(workd)) then
       if (ierr.eq.0) deallocate(workd, STAT=ierr)
    endif
    if (allocated(wpack)) then
       if (ierr.eq.0) deallocate(wpack, STAT=ierr)
    endif
    if (allocated(wmask)) then
       if (ierr.eq.0) deallocate(wmask, STAT=ierr)
    endif
    if (allocated(wdsubv)) then
       if (ierr.eq.0) deallocate(wdsubv, STAT=ierr)
    endif
    if (allocated(wssubv)) then
       if (ierr.eq.0) deallocate(wssubv, STAT=ierr)
    endif
  end subroutine finalize_destroy

!!!_ + user interfaces
!!!_  - get_default_header - get default header
  subroutine get_default_header &
       & (head,  &
       &  vmiss, utime, csign, msign)
    use TOUZA_Nio_header,only: &
         & put_item, &
         & hi_UTIM,  hi_MISS,  hi_DMIN,  hi_DMAX,  hi_DIVS,  hi_DIVL,  &
         & hi_CSIGN, hi_MSIGN
    implicit none
    character(len=*),intent(out)         :: head(*)
    real(kind=KDBL), intent(in),optional :: vmiss
    character(len=*),intent(in),optional :: utime
    character(len=*),intent(in),optional :: csign, msign
    integer jerr

    jerr = 0
    head(1:nitem) = head_def(1:nitem)
    if (present(utime)) then
       if (jerr.eq.0) call put_item(jerr, head, utime, hi_UTIM)
    endif
    if (present(vmiss)) then
       if (jerr.eq.0) call put_item(jerr, head, vmiss, hi_MISS)
       if (jerr.eq.0) call put_item(jerr, head, vmiss, hi_DMIN)
       if (jerr.eq.0) call put_item(jerr, head, vmiss, hi_DMAX)
       if (jerr.eq.0) call put_item(jerr, head, vmiss, hi_DIVS)
       if (jerr.eq.0) call put_item(jerr, head, vmiss, hi_DIVL)
    endif
    if (present(csign)) then
       if (jerr.eq.0) call put_item(jerr, head, csign, hi_CSIGN)
    endif
    if (present(msign)) then
       if (jerr.eq.0) call put_item(jerr, head, msign, hi_MSIGN)
    endif

    return
  end subroutine get_default_header

!!!_  & nio_check_magic_file() - check if gtool format
  integer function nio_check_magic_file_name &
       & (file) &
       & result(krect)
    use TOUZA_Nio_std,only: new_unit, sus_open, sus_close
    implicit none
    character(len=*),intent(in) :: file
    integer jerr
    integer u
    character(len=litem) :: head(nitem)

    krect = 0
    u = new_unit(uwork)
    jerr = min(0, u)
    if (jerr.eq.0) then
       uwork = u
       call sus_open(jerr, u, file, ACTION='R', STATUS='O')
    endif
    if (jerr.eq.0) call nio_read_header(jerr, head, krect, u)
    if (jerr.eq.0) call sus_close(jerr, u, file)

    if (jerr.lt.0) krect = jerr
  end function nio_check_magic_file_name

  integer function nio_check_magic_file_unit &
       & (u, pos, whence) &
       & result(krect)
    use TOUZA_Nio_std,only: KIOFS, WHENCE_ABS, sus_rseek, sus_getpos
    implicit none
    integer,            intent(in)          :: u
    integer(kind=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(kind=KIOFS) :: apini
    integer jerr, jdmy
    character(len=litem) :: head(nitem)

    krect = 0
    call sus_getpos(jerr, apini, u)
    if (jerr.eq.0) call nio_read_header(jerr, head, krect, u, pos, whence)
    call sus_rseek(jdmy, u, apini, WHENCE_ABS)
    if (jerr.eq.0) then
       if (jdmy.ne.0) jerr = jdmy
    endif
    if (jerr.gt.0) then
       krect = _ERROR(ERR_PANIC)
    else if (jerr.lt.0) then
       krect = jerr
    endif
  end function nio_check_magic_file_unit

!!!_  & nio_check_magic_header - check magic bytes at current point
  subroutine nio_check_magic_header &
       & (ierr, &
       &  u,    krect, ndrec)
    use TOUZA_Nio_std,only: KIOFS, sus_rseek, WHENCE_ABS, sus_getpos
    use TOUZA_Nio_header,only: litem
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: krect
    integer,intent(in)  :: u
    integer,intent(in)  :: ndrec
    integer jerr
    integer idfm
    integer(kind=KIOFS) :: apini, apos
    character(len=litem) :: head(nitem)
    integer nd
    ierr = 0
    call sus_getpos(ierr, apini, u)
    if (ierr.eq.0) then
       if (IAND(krect, REC_LSEP).gt.0) then
          apos = apini + nlsep
       else
          apos = apini + nisep
       endif
       read(UNIT=u, IOSTAT=ierr, POS=apos) head(1:nitem)
    endif
    if (ierr.eq.0) then
       idfm = check_id_format(head)
       if (idfm.lt.0) ierr = idfm
    endif
    if (ierr.eq.0) then
       call nio_data_records(nd, head)
       if (nd.ne.ndrec) ierr = _ERROR(ERR_BROKEN_RECORD)
    endif

    call sus_rseek(jerr, u, apini, WHENCE_ABS)
    if (ierr.eq.0) ierr = jerr
    return
  end subroutine nio_check_magic_header

!!!_  & nio_read_header - read header and set current properties
  subroutine nio_read_header &
       & (ierr, &
       &  head,  krect, u, pos, whence)
    use TOUZA_Nio_std,   only: KI32, KI64, KIOFS, is_eof_ss
    use TOUZA_Nio_std,   only: WHENCE_ABS, sus_read_isep, sus_read_lsep, sus_skip_irec, sus_rseek, sus_eswap
    use TOUZA_Nio_std,   only: conv_b2strm
    use TOUZA_Nio_std,   only: sus_pos_r2abs, sus_getpos
    use TOUZA_Nio_header,only: nitem, litem
    implicit none
    integer,            intent(out)         :: ierr
    character(len=*),   intent(out)         :: head(*)
    integer,            intent(out)         :: krect
    integer,            intent(in)          :: u
    integer(kind=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence

    integer,parameter :: lbuf = 16  ! enough for most case
    character(len=lbuf) :: bapp

    integer(kind=KIOFS) :: apos, aposf
    integer(kind=KI32)  :: iseph, isepl, isepf
    integer(kind=KI64)  :: lsepf
    integer(kind=KIOFS) :: nlh
    logical swap, lrec

    character(len=litem) :: htop
    integer ltop
    integer idfm
    integer jerr

    ierr = err_default

    krect = 0
    nlh = 0
    ltop = litem - nisep

    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
    else
       if (ierr.eq.0) call sus_getpos(ierr, apos, u)
    endif
    if (ierr.eq.0) then
       !! read SEP + HEADERS
       read(UNIT=u, POS=apos, IOSTAT=ierr) &
            & iseph, &                     ! isep
            & isepl, htop(1:ltop), &       ! item 0
            & head(2:nitem)
       ! write(*, *) ierr, is_eof_ss(ierr)
       if (is_eof_ss(ierr)) then
          call sus_getpos(jerr, aposf, u)
          ! write(*, *) jerr, apos, aposf
          if (apos.eq.aposf) then
             ierr = _ERROR(ERR_EOF)
             head(1:nitem) = ' '
          else
             ierr = _ERROR(ERR_BROKEN_RECORD)
          endif
          return
       endif
    endif

    if (ierr.eq.0) then
       call check_magic_bytes(krect, nlh, iseph, isepl)

       if (nlh.lt.nlhead_std .or. nlh.ge.HEADER_LIMIT) then
          KRECT = REC_ERROR
          call sus_rseek(ierr, u, apos, whence=WHENCE_ABS)
          if (ierr.eq.0) ierr = _ERROR(ERR_UNKNOWN_FORMAT)
       else
          swap = IAND(krect, REC_SWAP).ne.0
          lrec = IAND(krect, REC_LSEP).ne.0
          if (lrec) then
             read(UNIT=u, IOSTAT=ierr) bapp(1:nisep)
             call shift_header(head, litem, nitem, nisep, bapp)
             ! read foot separator
             aposf = apos + conv_b2strm(abs(nlh) + nisep * 2)
             call sus_read_lsep(ierr, u, lsepf, pos=aposf, swap=swap)
          else
             head(1) = HEADER_FILLS // htop(1:ltop)
             ! read foot separator
             aposf = apos + conv_b2strm(abs(nlh) + nisep)
             call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
             ! skip the rest
             if (iseph.lt.0) call sus_skip_irec(ierr, u, 1, swap=swap)
          endif
          if (ierr.eq.0) then
             idfm = check_id_format(head)
             if (idfm.lt.0) ierr = idfm
          endif
       endif
    endif

    ! if (ierr.eq.0) then
    !    call sus_rseek(ierr, u, apos, whence=WHENCE_ABS)
    ! endif
    ! ierr = err_default !! TESTING
    return
  end subroutine nio_read_header

!!!_   . shift_header
  subroutine shift_header &
       & (head, litem, nitem, nisep, bapp)
    implicit none
    character(len=*),intent(inout) :: head(*)
    integer,         intent(in)    :: litem, nitem, nisep
    character(len=*),intent(in)    :: bapp
    integer j
    do j = 1, nitem - 1
       head(j) = head(j)(nisep+1:litem) // head(j+1)(1:nisep)
    enddo
    j = nitem
    head(j) = head(j)(nisep+1:litem) // bapp(1:nisep)
    return
  end subroutine shift_header

!!!_  - check_magic_bytes
  subroutine check_magic_bytes &
       & (krect, nlenh, iseph, isepl)
    use TOUZA_Nio_std,only: KIOFS, sus_eswap
    implicit none
    integer,            intent(out) :: krect
    integer(kind=KIOFS),intent(out) :: nlenh
    integer,            intent(in)  :: iseph, isepl

    krect = 0
    if (iseph.eq.0) then
       ! [00 00 00 00] [00 00 04 00]  long/big
       if (isepl.lt.HEADER_LIMIT) then
          krect = IOR(krect, REC_LSEP) ! long native
          nlenh = isepl
       else
          krect = IOR(IOR(krect, REC_LSEP), REC_SWAP) ! long swap
          nlenh = sus_eswap(isepl)
       endif
    else if (isepl.eq.0) then
       ! [00 04 00 00] [00 00 00 00]  long/little
       if (iseph.lt.HEADER_LIMIT) then
          krect = IOR(krect, REC_LSEP) ! long native
          nlenh = iseph
       else
          krect = IOR(IOR(krect, REC_LSEP), REC_SWAP) ! long swap
          nlenh = sus_eswap(iseph)
       endif
    else if (isepl.eq.HEADER_SPACES) then
       ! [ff ff fc 00] [20 20 20 20] short/big/cont
       ! [00 fc ff ff] [20 20 20 20] short/little/cont
       ! [00 00 04 00] [20 20 20 20] short/big
       ! [00 04 00 00] [20 20 20 20] short/little
       if (abs(iseph).lt.HEADER_LIMIT) then
          krect = krect                ! short native
          nlenh = abs(iseph)
       else
          krect = IOR(krect, REC_SWAP) ! short swap
          nlenh = abs(sus_eswap(iseph))
       endif
       ! head(1) = HEADER_FILLS // htop(1:ltop)
    else
       nlenh = nlhead_std - 1
    endif
    return
  end subroutine check_magic_bytes

!!!_  & nio_write_header - write header and set current properties (if necessary)
  subroutine nio_write_header &
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

  end subroutine nio_write_header

!!!_  & nio_read_data - read data block
  subroutine nio_read_data_d &
       & (ierr, &
       &  d,    ld, head, krect, u, kopts, start, count)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)             ! data array
    integer,         intent(in)  :: ld               ! limit size of d
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,optional,intent(out) :: kopts(:)
    integer,optional,intent(in)  :: start(:)
    integer,optional,intent(in)  :: count(:)

    integer kfmt
    integer kaxs(laxs)
    real(kind=KRMIS) :: vmiss

    ierr = err_default
    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    if (ierr.eq.0) then
       if (present(start).or.present(count)) then
          call nio_read_data_slice &
               & (ierr, &
               &  d,    ld, kfmt, kaxs, vmiss, krect, u, start, count, kopts)
       else
          call nio_read_data_core &
               & (ierr, &
               &  d,    ld, kfmt, kaxs, vmiss, krect, u, kopts)
       endif
    endif
    return
  end subroutine nio_read_data_d
  subroutine nio_read_data_f &
       & (ierr, &
       &  d,    ld, head, krect, u, kopts, start, count)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: ld
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,optional,intent(out) :: kopts(:)
    integer,optional,intent(in)  :: start(:)
    integer,optional,intent(in)  :: count(:)

    integer kfmt
    integer kaxs(laxs)
    real(kind=KRMIS) :: vmiss

    ierr = err_default
    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    if (ierr.eq.0) then
       if (present(start).or.present(count)) then
          call nio_read_data_slice &
               & (ierr, &
               &  d,    ld, kfmt, kaxs, vmiss, krect, u, start, count, kopts)
       else
          call nio_read_data_core &
               & (ierr, &
               &  d,    ld, kfmt, kaxs, vmiss, krect, u, kopts)
       endif
    endif
    return
  end subroutine nio_read_data_f
  subroutine nio_read_data_i &
       & (ierr, &
       &  d,    ld, head, krect, u, kopts, start, count)
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(out) :: d(*)
    integer,           intent(in)  :: ld
    character(len=*),  intent(in)  :: head(*)
    integer,           intent(in)  :: krect
    integer,           intent(in)  :: u
    integer,optional,  intent(out) :: kopts(:)
    integer,optional,  intent(in)  :: start(:)
    integer,optional,  intent(in)  :: count(:)

    integer kfmt
    integer kaxs(laxs)
    real(kind=KRMIS) :: vmiss

    ierr = err_default
    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    if (ierr.eq.0) then
       if (present(start).or.present(count)) then
          call nio_read_data_slice &
               & (ierr, &
               &  d,    ld, kfmt, kaxs, vmiss, krect, u, start, count, kopts)
       else
          call nio_read_data_core &
               & (ierr, &
               &  d,    ld, kfmt, kaxs, vmiss, krect, u, kopts)
       endif
    endif
    return
  end subroutine nio_read_data_i

!!!_  & nio_write_data - write data block
  subroutine nio_write_data_d &
       & (ierr, &
       &  d,    ld, head, krect, u, kopts)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(*)
    integer,         intent(in)  :: ld
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,optional,intent(in)  :: kopts(:)

    integer kfmt
    integer kaxs(laxs)
    real(kind=KRMIS) :: vmiss

    ierr = err_default
    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    ! write(*, *) kaxs
    if (ierr.eq.0) then
       call nio_write_data_core &
            & (ierr, &
            &  d,    ld, kfmt, kaxs, vmiss, krect, u, kopts)
    endif
    return
  end subroutine nio_write_data_d
  subroutine nio_write_data_f &
       & (ierr, &
       &  d,    ld, head, krect, u, kopts)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(*)
    integer,         intent(in)  :: ld
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,optional,intent(in)  :: kopts(:)

    integer kfmt
    integer kaxs(laxs)
    real(kind=KRMIS) :: vmiss

    ierr = err_default
    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    if (ierr.eq.0) then
       call nio_write_data_core &
            & (ierr, &
            &  d,    ld, kfmt, kaxs, vmiss, krect, u, kopts)
    endif
    return
  end subroutine nio_write_data_f
  subroutine nio_write_data_i &
       & (ierr, &
       &  d,    ld, head, krect, u, kopts)
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(in)  :: d(*)
    integer,           intent(in)  :: ld
    character(len=*),  intent(in)  :: head(*)
    integer,           intent(in)  :: krect
    integer,           intent(in)  :: u
    integer,optional,  intent(in)  :: kopts(:)

    integer kfmt
    integer kaxs(laxs)
    real(kind=KRMIS) :: vmiss

    ierr = err_default
    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    ! write(*, *) 'parse', ierr
    if (ierr.eq.0) then
       call nio_write_data_core &
            & (ierr, &
            &  d,    ld, kfmt, kaxs, vmiss, krect, u, kopts)
    endif
    ! write(*, *) 'write_core', ierr
    return
  end subroutine nio_write_data_i

!!!_  & nio_read_data_core - read data block (full)
  subroutine nio_read_data_core_d &
       & (ierr, &
       &  d,    ld, kfmt, kaxs, vmiss, krect, u, kopts)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: ld       ! limit size of d
    integer,         intent(in)  :: kfmt
    integer,         intent(in)  :: kaxs(*)
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,optional,intent(out) :: kopts(:)

    integer nd           ! data size array
    integer mw, mh, mk   ! data sizes in file (3d, plane, levels)

    ierr = 0
    if (ierr.eq.0) call nio_read_data_set(ierr, nd, mw, kaxs, laxs, kfmt, ld)
    if (ierr.eq.0) then
       select case (kfmt)
       case (GFMT_UR4)
          call get_data_frecord(ierr, d, nd, u, krect)
       case (GFMT_UR8)
          call get_data_drecord(ierr, d, nd, u, krect)
       case (GFMT_UI4)
          call get_data_irecord(ierr, d, nd, u, krect)
       case (GFMT_MR4)
          call restore_mr4_plain(ierr, d, nd, u, krect, vmiss, mw)
       case (GFMT_MR8)
          call restore_mr8_plain(ierr, d, nd, u, krect, vmiss, mw)
       case (GFMT_MI4)
          call restore_mi4_plain(ierr, d, nd, u, krect, vmiss, mw)
       case (GFMT_URC, GFMT_URC2)
          mh = max(1, kaxs(1)) * max(1, kaxs(2))
          mk = max(1, kaxs(3))
          call get_data_urc &
               & (ierr, d, mh, mk, u, krect, vmiss, IMISS_URC, kfmt)
       case (GFMT_URY:GFMT_URYend-1)
          mh = max(1, kaxs(1)) * max(1, kaxs(2))
          mk = max(1, kaxs(3))
          call get_data_ury &
               & (ierr, d, nd, u, krect, vmiss, kfmt, mh, mk)
       case (GFMT_MRY:GFMT_MRYend-1)
          mh = max(1, kaxs(1)) * max(1, kaxs(2))
          mk = max(1, kaxs(3))
          call get_data_mry &
               & (ierr, d, nd, u, krect, vmiss, kfmt, mh, mk)
       case (GFMT_URT)
          call get_data_urt &
               & (ierr, d, nd, u, krect, vmiss, kopts)
       case (GFMT_MRT)
          call get_data_mrt &
               & (ierr, d, nd, u, krect, vmiss, kopts)
       case (GFMT_PI4SV)
          call restore_ptn_plain_subv &
               & (ierr, d, ld, u, krect, vmiss, kfmt, kaxs)
       case (GFMT_PR8)
          mk = max(1, kaxs(3))
          call get_data_pr8 &
               & (ierr, d, nd, u, krect, mk, kopts)
       case (GFMT_PR4)
          mk = max(1, kaxs(3))
          call get_data_pr4 &
               & (ierr, d, nd, u, krect, mk, kopts)
       case (GFMT_PI4)
          mk = max(1, kaxs(3))
          call get_data_pi4 &
               & (ierr, d, nd, u, krect, mk, kopts)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    return
  end subroutine nio_read_data_core_d
  subroutine nio_read_data_core_f &
       & (ierr, &
       &  d,    ld, kfmt, kaxs, vmiss, krect, u, kopts)
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
    integer,optional,intent(out) :: kopts(:)

    integer nd           ! data size array
    integer mw, mh, mk   ! data sizes in file (3d, plane, levels)

    ierr = 0
    if (ierr.eq.0) call nio_read_data_set(ierr, nd, mw, kaxs, laxs, kfmt, ld)
    if (ierr.eq.0) then
       select case (kfmt)
       case (GFMT_UR4)
          call get_data_frecord(ierr, d, nd, u, krect)
       case (GFMT_UR8)
          call get_data_drecord(ierr, d, nd, u, krect)
       case (GFMT_UI4)
          call get_data_irecord(ierr, d, nd, u, krect)
       case (GFMT_MR4)
          call restore_mr4_plain(ierr, d, nd, u, krect, vmiss, mw)
       case (GFMT_MR8)
          call restore_mr8_plain(ierr, d, nd, u, krect, vmiss, mw)
       case (GFMT_MI4)
          call restore_mi4_plain(ierr, d, nd, u, krect, vmiss, mw)
       case (GFMT_URC, GFMT_URC2)
          mh = max(1, kaxs(1)) * max(1, kaxs(2))
          mk = max(1, kaxs(3))
          call get_data_urc &
               & (ierr, d, mh, mk, u, krect, vmiss, IMISS_URC, kfmt)
       case (GFMT_URY:GFMT_URYend-1)
          mh = max(1, kaxs(1)) * max(1, kaxs(2))
          mk = max(1, kaxs(3))
          call get_data_ury &
               & (ierr, d, nd, u, krect, vmiss, kfmt, mh, mk)
       case (GFMT_MRY:GFMT_MRYend-1)
          mh = max(1, kaxs(1)) * max(1, kaxs(2))
          mk = max(1, kaxs(3))
          call get_data_mry &
               & (ierr, d, nd, u, krect, vmiss, kfmt, mh, mk)
       case (GFMT_URT)
          call get_data_urt &
               & (ierr, d, nd, u, krect, vmiss, kopts)
       case (GFMT_MRT)
          call get_data_mrt &
               & (ierr, d, nd, u, krect, vmiss, kopts)
       case (GFMT_PI4SV)
          call restore_ptn_plain_subv &
               & (ierr, d, ld, u, krect, vmiss, kfmt, kaxs)
       case (GFMT_PR8)
          mk = max(1, kaxs(3))
          call get_data_pr8 &
               & (ierr, d, nd, u, krect, mk, kopts)
       case (GFMT_PR4)
          mk = max(1, kaxs(3))
          call get_data_pr4 &
               & (ierr, d, nd, u, krect, mk, kopts)
       case (GFMT_PI4)
          mk = max(1, kaxs(3))
          call get_data_pi4 &
               & (ierr, d, nd, u, krect, mk, kopts)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    return
  end subroutine nio_read_data_core_f
  subroutine nio_read_data_core_i &
       & (ierr, &
       &  d,    ld, kfmt, kaxs, vmiss, krect, u, kopts)
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
    integer,optional,  intent(out) :: kopts(:)

    integer nd           ! data size array
    integer mw, mh, mk   ! data sizes in file (3d, plane, levels)

    ierr = 0
    if (ierr.eq.0) call nio_read_data_set(ierr, nd, mw, kaxs, laxs, kfmt, ld)
    if (ierr.eq.0) then
       select case (kfmt)
       case (GFMT_UR4)
          call get_data_frecord(ierr, d, nd, u, krect)
       case (GFMT_UR8)
          call get_data_drecord(ierr, d, nd, u, krect)
       case (GFMT_UI4)
          call get_data_irecord(ierr, d, nd, u, krect)
       case (GFMT_MR4)
          call restore_mr4_plain(ierr, d, nd, u, krect, vmiss, mw)
       case (GFMT_MR8)
          call restore_mr8_plain(ierr, d, nd, u, krect, vmiss, mw)
       case (GFMT_MI4)
          call restore_mi4_plain(ierr, d, nd, u, krect, vmiss, mw)
       case (GFMT_URC, GFMT_URC2)
          mh = max(1, kaxs(1)) * max(1, kaxs(2))
          mk = max(1, kaxs(3))
          call get_data_urc &
               & (ierr, d, mh, mk, u, krect, vmiss, IMISS_URC, kfmt)
       case (GFMT_URY:GFMT_URYend-1)
          mh = max(1, kaxs(1)) * max(1, kaxs(2))
          mk = max(1, kaxs(3))
          call get_data_ury &
               & (ierr, d, nd, u, krect, vmiss, kfmt, mh, mk)
       case (GFMT_MRY:GFMT_MRYend-1)
          mh = max(1, kaxs(1)) * max(1, kaxs(2))
          mk = max(1, kaxs(3))
          call get_data_mry &
               & (ierr, d, nd, u, krect, vmiss, kfmt, mh, mk)
       case (GFMT_URT)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
          ! call get_data_urt &
          !      & (ierr, d, nd, u, krect, vmiss, kopts)
       case (GFMT_MRT)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
          ! call get_data_mrt &
          !      & (ierr, d, nd, u, krect, vmiss, kopts)
       case (GFMT_PI4SV)
          call restore_ptn_plain_subv &
               & (ierr, d, ld, u, krect, vmiss, kfmt, kaxs)
       case (GFMT_PR8)
          mk = max(1, kaxs(3))
          call get_data_pr8 &
               & (ierr, d, nd, u, krect, mk, kopts)
       case (GFMT_PR4)
          mk = max(1, kaxs(3))
          call get_data_pr4 &
               & (ierr, d, nd, u, krect, mk, kopts)
       case (GFMT_PI4)
          mk = max(1, kaxs(3))
          call get_data_pi4 &
               & (ierr, d, nd, u, krect, mk, kopts)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    return
  end subroutine nio_read_data_core_i

!!!_  & nio_read_data_slice - read data block (partial array)
  subroutine nio_read_data_slice_d &
       & (ierr, &
       &  d,    ld,    kfmt, kaxs, vmiss, krect, u, &
       &  start,count, kopts)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: ld         ! limit size of d. negative for infinite.
    integer,         intent(in)  :: kfmt
    integer,         intent(in)  :: kaxs(*)
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: start(:)
    integer,         intent(in)  :: count(:)
    integer,optional,intent(out) :: kopts(:)

    integer nd           ! data size array
    integer mw, mh, mk   ! data sizes in file (3d, plane, levels)
    integer bes(3, laxs)

    ierr = 0
    call nio_read_slice_set &
       & (ierr, bes, nd, mw, kaxs, laxs, kfmt, ld, start, count)
    if (ierr.eq.0) then
       select case (kfmt)
       case (GFMT_UR4)
          call get_data_frecord_slice_d(ierr, d, nd, u, krect, mw, bes, laxs)
       case (GFMT_UR8)
          call get_data_record_slice_d(ierr, d, nd, u, krect, mw, bes, laxs)
       case (GFMT_UI4)
          call get_data_irecord_slice_d(ierr, d, nd, u, krect, mw, bes, laxs)
       case (GFMT_MR4)
          call restore_mr4_plain(ierr, d, nd, u, krect, vmiss, mw, bes, laxs)
       case (GFMT_MR8)
          call restore_mr8_plain(ierr, d, nd, u, krect, vmiss, mw, bes, laxs)
       case (GFMT_MI4)
          call restore_mi4_plain(ierr, d, nd, u, krect, vmiss, mw, bes, laxs)
       case (GFMT_URC, GFMT_URC2)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_URY:GFMT_URYend-1)
          mh = max(1, kaxs(1)) * max(1, kaxs(2))
          mk = max(1, kaxs(3))
          call get_data_ury &
               & (ierr, d, nd, u, krect, vmiss, kfmt, mh, mk, bes, laxs)
       case (GFMT_MRY:GFMT_MRYend-1)
          mh = max(1, kaxs(1)) * max(1, kaxs(2))
          mk = max(1, kaxs(3))
          call get_data_mry &
               & (ierr, d, nd, u, krect, vmiss, kfmt, mh, mk, bes, laxs)
       case (GFMT_URT)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_MRT)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_PR8,GFMT_PR4,GFMT_PI4)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    return
  end subroutine nio_read_data_slice_d
  subroutine nio_read_data_slice_f &
       & (ierr,  &
       &  d,     ld,    kfmt, kaxs, vmiss, krect, u, &
       &  start, count, kopts)
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
    integer,         intent(in)  :: start(:)
    integer,         intent(in)  :: count(:)
    integer,optional,intent(out) :: kopts(:)

    integer nd           ! data size array
    integer mw, mh, mk   ! data sizes in file (3d, plane, levels)
    integer bes(3, laxs)

    ierr = 0
    call nio_read_slice_set &
       & (ierr, bes, nd, mw, kaxs, laxs, kfmt, ld, start, count)
    if (ierr.eq.0) then
       select case (kfmt)
       case (GFMT_UR4)
          call get_data_record_slice_f(ierr, d, nd, u, krect, mw, bes, laxs)
       case (GFMT_UR8)
          call get_data_drecord_slice_f(ierr, d, nd, u, krect, mw, bes, laxs)
       case (GFMT_UI4)
          call get_data_irecord_slice_f(ierr, d, nd, u, krect, mw, bes, laxs)
       case (GFMT_MR4)
          call restore_mr4_plain(ierr, d, nd, u, krect, vmiss, mw, bes, laxs)
       case (GFMT_MR8)
          call restore_mr8_plain(ierr, d, nd, u, krect, vmiss, mw, bes, laxs)
       case (GFMT_MI4)
          call restore_mi4_plain(ierr, d, nd, u, krect, vmiss, mw, bes, laxs)
       case (GFMT_URC, GFMT_URC2)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_URY:GFMT_URYend-1)
          mh = max(1, kaxs(1)) * max(1, kaxs(2))
          mk = max(1, kaxs(3))
          call get_data_ury &
               & (ierr, d, nd, u, krect, vmiss, kfmt, mh, mk, bes, laxs)
       case (GFMT_MRY:GFMT_MRYend-1)
          mh = max(1, kaxs(1)) * max(1, kaxs(2))
          mk = max(1, kaxs(3))
          call get_data_mry &
               & (ierr, d, nd, u, krect, vmiss, kfmt, mh, mk, bes, laxs)
       case (GFMT_URT)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_MRT)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_PR8,GFMT_PR4,GFMT_PI4)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    return
  end subroutine nio_read_data_slice_f
  subroutine nio_read_data_slice_i &
       & (ierr,  &
       &  d,     ld,    kfmt, kaxs, vmiss, krect, u, &
       &  start, count, kopts)
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
    integer,           intent(in)  :: start(:)
    integer,           intent(in)  :: count(:)
    integer,optional,  intent(out) :: kopts(:)

    integer nd           ! data size array
    integer mw, mh, mk   ! data sizes in file (3d, plane, levels)
    integer bes(3, laxs)

    ierr = 0
    call nio_read_slice_set &
       & (ierr, bes, nd, mw, kaxs, laxs, kfmt, ld, start, count)
    if (ierr.eq.0) then
       select case (kfmt)
       case (GFMT_UR4)
          call get_data_frecord_slice_i(ierr, d, nd, u, krect, mw, bes, laxs)
       case (GFMT_UR8)
          call get_data_drecord_slice_i(ierr, d, nd, u, krect, mw, bes, laxs)
       case (GFMT_UI4)
          call get_data_record_slice_i(ierr, d, nd, u, krect, mw, bes, laxs)
       case (GFMT_MR4)
          call restore_mr4_plain(ierr, d, nd, u, krect, vmiss, mw, bes, laxs)
       case (GFMT_MR8)
          call restore_mr8_plain(ierr, d, nd, u, krect, vmiss, mw, bes, laxs)
       case (GFMT_MI4)
          call restore_mi4_plain(ierr, d, nd, u, krect, vmiss, mw, bes, laxs)
       case (GFMT_URC, GFMT_URC2)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_URY:GFMT_URYend-1)
          mh = max(1, kaxs(1)) * max(1, kaxs(2))
          mk = max(1, kaxs(3))
          call get_data_ury &
               & (ierr, d, nd, u, krect, vmiss, kfmt, mh, mk, bes, laxs)
       case (GFMT_MRY:GFMT_MRYend-1)
          mh = max(1, kaxs(1)) * max(1, kaxs(2))
          mk = max(1, kaxs(3))
          call get_data_mry &
               & (ierr, d, nd, u, krect, vmiss, kfmt, mh, mk, bes, laxs)
       case (GFMT_URT)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_MRT)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_PR8,GFMT_PR4,GFMT_PI4)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    return
  end subroutine nio_read_data_slice_i

!!!_  & nio_read_data_set
  subroutine nio_read_data_set &
       & (ierr, nd, mw, kaxs, nr, kfmt, ld)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: nd   ! target size (to access)
    integer,intent(out) :: mw   ! full size (in file)
    integer,intent(in)  :: kaxs(*)
    integer,intent(in)  :: nr
    integer,intent(in)  :: kfmt
    integer,intent(in)  :: ld

    ierr = 0
    mw = product(max(1, kaxs(1:nr)))
    nd = mw
    if (IAND(kfmt, GFMT_LPAD).eq.0) then
       if (ld.ge.0.and.nd.gt.ld) then
          ierr = _ERROR(ERR_SIZE_MISMATCH)
       endif
    else if (ld.lt.0) then
       nd = mw
    else
       nd = ld
    endif
  end subroutine nio_read_data_set
!!!_  & nio_read_slice_set
  subroutine nio_read_slice_set &
       & (ierr, bes, nd, mw, kaxs, nr, kfmt, ld, start, count)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: bes(3, *)
    integer,intent(out) :: nd   ! target size (to access)
    integer,intent(out) :: mw   ! full size (in file)
    integer,intent(in)  :: kaxs(*)
    integer,intent(in)  :: nr
    integer,intent(in)  :: kfmt
    integer,intent(in)  :: ld
    integer,intent(in)  :: start(:), count(:)

    ierr = 0
    mw = product(max(1, kaxs(1:nr)))
    call bes_triplet(ierr, bes, kaxs, start, count)
    if (ierr.eq.0) then
       nd = count_bes(bes, nr)
       if (IAND(kfmt, GFMT_LPAD).eq.0) then
          if (ld.ge.0.and.nd.gt.ld) then
             ierr = _ERROR(ERR_SIZE_MISMATCH)
          endif
       else if (ld.lt.0) then
          nd = mw
       else
          nd = ld
       endif
    endif
  end subroutine nio_read_slice_set

!!!_  & nio_read_data_packed - read data block as packed storage
  subroutine nio_read_data_packed_d &
       & (ierr, &
       &  d,     ldata, subv, ends, head, krect, u, &
       &  citer, check)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out)   :: ierr
    real(kind=KARG), intent(out)   :: d(*)
    integer,         intent(in)    :: ldata        ! limit size of d. negative for infinite.
    integer,         intent(inout) :: subv(0:*)
    integer,         intent(inout) :: ends(0:*)
    character(len=*),intent(in)    :: head(*)
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: u
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check        ! whether subv, ends are references

    integer ndata, mfull
    integer mk

    integer kfmt, kdmyf
    integer kaxs(laxs)
    real(kind=KRMIS) :: vmiss

    ierr = 0

    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    if (ierr.eq.0) then
       kdmyf = GFMT_LPAD
       call nio_read_data_set(ierr, ndata, mfull, kaxs, laxs, kdmyf, ldata)
    endif

    if (ierr.eq.0) then
       select case (kfmt)
       case (GFMT_UR4)
          call restore_ur4_packed &
               & (ierr, &
               &  d,    ldata, subv, ends, u, krect, vmiss, mfull, kaxs, citer, check)
       case (GFMT_UR8)
          call restore_ur8_packed &
               & (ierr, &
               &  d,    ldata, subv, ends, u, krect, vmiss, mfull, kaxs, citer, check)
       case (GFMT_UI4)
          call restore_ui4_packed &
               & (ierr, &
               &  d,    ldata, subv, ends, u, krect, vmiss, mfull, kaxs, citer, check)
       case (GFMT_MR4)
          call restore_mr4_packed &
               & (ierr, &
               &  d,    ldata, subv, ends, u, krect, mfull, kaxs, citer, check)
       case (GFMT_MR8)
          call restore_mr8_packed &
               & (ierr, &
               &  d,    ldata, subv, ends, u, krect, mfull, kaxs, citer, check)
       case (GFMT_MI4)
          call restore_mi4_packed &
               & (ierr, &
               &  d,    ldata, subv, ends, u, krect, mfull, kaxs, citer, check)
       case (GFMT_URC, GFMT_URC2)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_URY:GFMT_URYend-1)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_MRY:GFMT_MRYend-1)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_URT)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_MRT)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
          mk = max(1, kaxs(3))
       case (GFMT_PI4SV)
          call restore_ptn_packed_subv &
               & (ierr, &
               &  subv, ldata, ends, u, krect, kfmt, kaxs, citer)
       case (GFMT_PR8)
          call restore_pr8_packed &
               & (ierr, &
               &  d,    ldata, ends, u, krect, kaxs, citer, check)
       case (GFMT_PR4)
          call restore_pr4_packed &
               & (ierr, &
               &  d,    ldata, ends, u, krect, kaxs, citer, check)
       case (GFMT_PI4)
          call restore_pi4_packed &
               & (ierr, &
               &  d,    ldata, ends, u, krect, kaxs, citer, check)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    return
  end subroutine nio_read_data_packed_d
  subroutine nio_read_data_packed_f &
       & (ierr, &
       &  d,     ldata, subv, ends, head, krect, u, &
       &  citer, check)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out)   :: ierr
    real(kind=KARG), intent(out)   :: d(*)
    integer,         intent(in)    :: ldata        ! limit size of d. negative for infinite.
    integer,         intent(inout) :: subv(0:*)
    integer,         intent(inout) :: ends(0:*)
    character(len=*),intent(in)    :: head(*)
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: u
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check        ! whether subv, ends are references

    integer ndata, mfull
    integer mk

    integer kfmt
    integer kaxs(laxs)
    real(kind=KRMIS) :: vmiss

    ierr = 0

    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    if (ierr.eq.0) then
       call nio_read_data_set(ierr, ndata, mfull, kaxs, laxs, kfmt, ldata)
    endif

    if (ierr.eq.0) then
       select case (kfmt)
       case (GFMT_UR4)
          call restore_ur4_packed &
               & (ierr, &
               &  d,    ldata, subv, ends, u, krect, vmiss, mfull, kaxs, citer, check)
       case (GFMT_UR8)
          call restore_ur8_packed &
               & (ierr, &
               &  d,    ldata, subv, ends, u, krect, vmiss, mfull, kaxs, citer, check)
       case (GFMT_UI4)
          call restore_ui4_packed &
               & (ierr, &
               &  d,    ldata, subv, ends, u, krect, vmiss, mfull, kaxs, citer, check)
       case (GFMT_MR4)
          call restore_mr4_packed &
               & (ierr, &
               &  d,    ldata, subv, ends, u, krect, mfull, kaxs, citer, check)
       case (GFMT_MR8)
          call restore_mr8_packed &
               & (ierr, &
               &  d,    ldata, subv, ends, u, krect, mfull, kaxs, citer, check)
       case (GFMT_MI4)
          call restore_mi4_packed &
               & (ierr, &
               &  d,    ldata, subv, ends, u, krect, mfull, kaxs, citer, check)
       case (GFMT_URC, GFMT_URC2)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_URY:GFMT_URYend-1)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_MRY:GFMT_MRYend-1)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_URT)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_MRT)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
          mk = max(1, kaxs(3))
       case (GFMT_PI4SV)
          call restore_ptn_packed_subv &
               & (ierr, &
               &  subv, ldata, ends, u, krect, kfmt, kaxs, citer)
       case (GFMT_PR8)
          call restore_pr8_packed &
               & (ierr, &
               &  d,    ldata, ends, u, krect, kaxs, citer, check)
       case (GFMT_PR4)
          call restore_pr4_packed &
               & (ierr, &
               &  d,    ldata, ends, u, krect, kaxs, citer, check)
       case (GFMT_PI4)
          call restore_pi4_packed &
               & (ierr, &
               &  d,    ldata, ends, u, krect, kaxs, citer, check)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    return
  end subroutine nio_read_data_packed_f
  subroutine nio_read_data_packed_i &
       & (ierr, &
       &  d,     ldata, subv, ends, head, krect, u, &
       &  citer, check)
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)   :: ierr
    integer(kind=KARG),intent(out)   :: d(*)
    integer,           intent(in)    :: ldata        ! limit size of d. negative for infinite.
    integer,           intent(inout) :: subv(0:*)
    integer,           intent(inout) :: ends(0:*)
    character(len=*),  intent(in)    :: head(*)
    integer,           intent(in)    :: krect
    integer,           intent(in)    :: u
    integer,optional,  intent(in)    :: citer        ! iteration index
    integer,optional,  intent(in)    :: check        ! whether subv, ends are references

    integer ndata, mfull
    integer mk

    integer kfmt
    integer kaxs(laxs)
    real(kind=KRMIS) :: vmiss

    ierr = 0

    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    if (ierr.eq.0) then
       call nio_read_data_set(ierr, ndata, mfull, kaxs, laxs, kfmt, ldata)
    endif

    if (ierr.eq.0) then
       select case (kfmt)
       case (GFMT_UR4)
          call restore_ur4_packed &
               & (ierr, &
               &  d,    ldata, subv, ends, u, krect, vmiss, mfull, kaxs, citer, check)
       case (GFMT_UR8)
          call restore_ur8_packed &
               & (ierr, &
               &  d,    ldata, subv, ends, u, krect, vmiss, mfull, kaxs, citer, check)
       case (GFMT_UI4)
          call restore_ui4_packed &
               & (ierr, &
               &  d,    ldata, subv, ends, u, krect, vmiss, mfull, kaxs, citer, check)
       case (GFMT_MR4)
          call restore_mr4_packed &
               & (ierr, &
               &  d,    ldata, subv, ends, u, krect, mfull, kaxs, citer, check)
       case (GFMT_MR8)
          call restore_mr8_packed &
               & (ierr, &
               &  d,    ldata, subv, ends, u, krect, mfull, kaxs, citer, check)
       case (GFMT_MI4)
          call restore_mi4_packed &
               & (ierr, &
               &  d,    ldata, subv, ends, u, krect, mfull, kaxs, citer, check)
       case (GFMT_URC, GFMT_URC2)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_URY:GFMT_URYend-1)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_MRY:GFMT_MRYend-1)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_URT)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_MRT)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
          mk = max(1, kaxs(3))
       case (GFMT_PI4SV)
          call restore_ptn_packed_subv &
               & (ierr, &
               &  subv, ldata, ends, u, krect, kfmt, kaxs, citer)
       case (GFMT_PR8)
          call restore_pr8_packed &
               & (ierr, &
               &  d,    ldata, ends, u, krect, kaxs, citer, check)
       case (GFMT_PR4)
          call restore_pr4_packed &
               & (ierr, &
               &  d,    ldata, ends, u, krect, kaxs, citer, check)
       case (GFMT_PI4)
          call restore_pi4_packed &
               & (ierr, &
               &  d,    ldata, ends, u, krect, kaxs, citer, check)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    return
  end subroutine nio_read_data_packed_i

!!!_  & nio_write_data_core - write data block
  subroutine nio_write_data_core_d &
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
    n = max(1, kaxs(1)) * max(1, kaxs(2)) * max(1, kaxs(3))
    if (IAND(kfmt, GFMT_LPAD).eq.0) then
       if (n.gt.ld) then
          ierr = _ERROR(ERR_SIZE_MISMATCH)
          return
       endif
    else
       n = ld
       if (ld.le.0) n = ld
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
       ierr = _ERROR(ERR_DEPRECATED_FORMAT)
    case (GFMT_URY:GFMT_URYend-1)
       nh = max(1, kaxs(1)) * max(1, kaxs(2))
       nk = max(1, kaxs(3))
       call put_data_ury &
            & (ierr, d, nh, nk, u, krect, vmiss, kfmt)
    case (GFMT_MRY:GFMT_MRYend-1)
       nh = max(1, kaxs(1)) * max(1, kaxs(2))
       nk = max(1, kaxs(3))
       call put_data_mry &
            & (ierr, d, nh, nk, u, krect, vmiss, kfmt)
    case (GFMT_URT)
       call put_data_urt_plain &
            & (ierr, d, n, u, krect, vmiss, kaxs, kopts)
    case (GFMT_MRT)
       call put_data_mrt_plain &
            & (ierr, d, n, u, krect, vmiss, kaxs, kopts)
    case (GFMT_PR4)
       nk = max(1, kaxs(3))
       call put_data_pr4 &
            & (ierr, d, n, u, krect, nk, kopts)
    case (GFMT_PR8)
       nk = max(1, kaxs(3))
       call put_data_pr8 &
            & (ierr, d, n, u, krect, nk, kopts)
    case (GFMT_PI4, GFMT_PI4SV)
       nk = max(1, kaxs(3))
       call put_data_pi4 &
            & (ierr, d, n, u, krect, nk, kopts)
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select

    return
  end subroutine nio_write_data_core_d
  subroutine nio_write_data_core_f &
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
    n = max(1, kaxs(1)) * max(1, kaxs(2)) * max(1, kaxs(3))
    if (IAND(kfmt, GFMT_LPAD).eq.0) then
       if (n.gt.ld) then
          ierr = _ERROR(ERR_SIZE_MISMATCH)
          return
       endif
    else
       n = ld
       if (ld.le.0) n = ld
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
       ierr = _ERROR(ERR_DEPRECATED_FORMAT)
    case (GFMT_URY:GFMT_URYend-1)
       nh = max(1, kaxs(1)) * max(1, kaxs(2))
       nk = max(1, kaxs(3))
       call put_data_ury &
            & (ierr, d, nh, nk, u, krect, vmiss, kfmt)
    case (GFMT_MRY:GFMT_MRYend-1)
       nh = max(1, kaxs(1)) * max(1, kaxs(2))
       nk = max(1, kaxs(3))
       call put_data_mry &
            & (ierr, d, nh, nk, u, krect, vmiss, kfmt)
    case (GFMT_URT)
       call put_data_urt_plain &
            & (ierr, d, n, u, krect, vmiss, kaxs, kopts)
    case (GFMT_MRT)
       call put_data_mrt_plain &
            & (ierr, d, n, u, krect, vmiss, kaxs, kopts)
    case (GFMT_PR4)
       nk = max(1, kaxs(3))
       call put_data_pr4 &
            & (ierr, d, n, u, krect, nk, kopts)
    case (GFMT_PR8)
       nk = max(1, kaxs(3))
       call put_data_pr8 &
            & (ierr, d, n, u, krect, nk, kopts)
    case (GFMT_PI4, GFMT_PI4SV)
       nk = max(1, kaxs(3))
       call put_data_pi4 &
            & (ierr, d, n, u, krect, nk, kopts)
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select

    return
  end subroutine nio_write_data_core_f
  subroutine nio_write_data_core_i &
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
    n = max(1, kaxs(1)) * max(1, kaxs(2)) * max(1, kaxs(3))
    if (IAND(kfmt, GFMT_LPAD).eq.0) then
       if (n.gt.ld) then
          ierr = _ERROR(ERR_SIZE_MISMATCH)
          return
       endif
    else
       n = ld
       if (ld.le.0) n = ld
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
       ierr = _ERROR(ERR_DEPRECATED_FORMAT)
    case (GFMT_URY:GFMT_URYend-1)
       nh = max(1, kaxs(1)) * max(1, kaxs(2))
       nk = max(1, kaxs(3))
       call put_data_ury &
            & (ierr, d, nh, nk, u, krect, vmiss, kfmt)
    case (GFMT_MRY:GFMT_MRYend-1)
       nh = max(1, kaxs(1)) * max(1, kaxs(2))
       nk = max(1, kaxs(3))
       call put_data_mry &
            & (ierr, d, nh, nk, u, krect, vmiss, kfmt)
    case (GFMT_PR4)
       nk = max(1, kaxs(3))
       call put_data_pr4 &
            & (ierr, d, n, u, krect, nk, kopts)
    case (GFMT_PR8)
       nk = max(1, kaxs(3))
       call put_data_pr8 &
            & (ierr, d, n, u, krect, nk, kopts)
    case (GFMT_PI4, GFMT_PI4SV)
       nk = max(1, kaxs(3))
       call put_data_pi4 &
            & (ierr, d, n, u, krect, nk, kopts)
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select

    return
  end subroutine nio_write_data_core_i

!!!_  & nio_skip_records - forward/backward gtool-record skip
  subroutine nio_skip_records &
       & (ierr, n, u, nskip, head, krect)
    use TOUZA_Nio_std,   only: WHENCE_CURRENT, sus_skip_irec, sus_skip_lrec, choice
    use TOUZA_Nio_header,only: &
         & nitem, litem, hi_DFMT, get_item
    implicit none
    integer,         intent(out)          :: ierr
    integer,         intent(in)           :: n
    integer,         intent(in)           :: u
    integer,         intent(out),optional :: nskip    ! number of skipped record
    character(len=*),intent(in), optional :: head(*)  ! current record header
    integer,         intent(in), optional :: krect    ! current record header type

    integer j, ns, jbgn
    integer kri, ksubm
    character(len=litem) :: hi(nitem)
!!!_   . note
    ! When argument head is present, krect is also mandatory (the opposite is not)
    !   File position must be at the header end (data begin)
    !   argument head is used as the first header instead to read next.
    !   Usual header/data reading is performed from the second block
!!!_   . note (dummy separator record)
    ! Big-GTOOL record cannot be skipped backward, because there is no chance to
    ! detect the length of data-block.
!!!_   . body
    ierr = 0
    ns = 0
    jbgn = 0

    if (present(head).and. .not.present(krect)) then
       ierr = _ERROR(ERR_PANIC)
       return
    endif

    if (n .lt. 0) then
       if (present(head)) then   ! head as placeholder
          ! back to header head
          call nio_skip_prec(ierr, u, -1, krect)
          if (ierr.eq.0) jbgn = jbgn + 1
       endif
       if (ierr.eq.0) ns = jbgn
       do j = jbgn, - n - 1
          if (ierr.eq.0) call nio_bwd_record(ierr, u, krect)
          if (ierr.ne.0) exit
          ns = ns + 1
       enddo
    else
       if (present(head)) then
          if (ierr.eq.0) call nio_skip_data(ierr, u, head, krect)
          if (ierr.eq.0) jbgn = jbgn + 1
       endif
       if (ierr.eq.0) ns = jbgn
       ksubm = IAND(choice(0, krect), REC_SUB_ALLOW)
       do j = jbgn, n - 1
          if (ierr.ne.0) exit
          if (ierr.eq.0) call nio_read_header(ierr, hi, kri, u)
          if (ierr.eq.0) call nio_skip_data(ierr, u, hi, IOR(kri, ksubm))  ! copy argument SUB switch
          if (ierr.eq.0) ns = ns + 1
       enddo
    endif
    if (present(nskip)) then
       nskip = ns
    endif
    return
  end subroutine nio_skip_records

!!!_  & nio_skip_data - forward one data record
  subroutine nio_skip_data &
       & (ierr, u, head, krect)
    use TOUZA_Nio_std,only: KIOFS, sus_rseek, WHENCE_ABS, WHENCE_CURRENT
    use TOUZA_Nio_std,only: sus_skip_irec, sus_skip_lrec
    ! use TOUZA_Nio_header,only: &
    !      & nitem, litem, hi_DFMT, get_item
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: u
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: krect
    logical swap, lrec
    integer nrec

    integer kfmt
    integer kaxs(laxs)
    real(kind=KRMIS) :: vmiss   ! dummy
!!!_   . body
    ierr = 0
    if (ierr.eq.0) then
       lrec = IAND(krect, REC_LSEP).ne.0
       if (lrec) then
          swap = IAND(krect, REC_SWAP).ne.0
          call nio_data_records(nrec, head)
          ierr = min(0, nrec)
          if (ierr.eq.0) call sus_skip_lrec(ierr, u, nrec, WHENCE_CURRENT, swap=swap)
       else
          call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
          if (ierr.eq.0) call nio_skip_data_irec(ierr, kfmt, kaxs, krect, u)
       endif
    endif
    return
  end subroutine nio_skip_data

!!!_  & nio_skip_data_irec - forward one data record with irec-type record
  subroutine nio_skip_data_irec &
       & (ierr, kfmt, kaxs, krect, u)
    use TOUZA_Nio_std,only: KIOFS, sus_rseek, WHENCE_ABS, WHENCE_CURRENT
    use TOUZA_Nio_std,only: sus_read_irec
    use TOUZA_Trp,only: count_packed
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: kfmt
    integer,intent(in)  :: kaxs(*)
    integer,intent(in)  :: krect
    integer,intent(in)  :: u
    integer n
    integer(kind=KI32) :: vi(1)
    real(kind=KFLT)    :: vf(1)
    real(kind=KDBL)    :: vd(1)
    logical swap
    integer div
    integer,parameter :: KISRC=KI32
    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer(kind=KISRC) :: mb
    integer nrec
    integer nbits
    integer,parameter :: KRSRC=KDBL
    integer nn, nh, nm, nk

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    div = read_sep_flag(krect)
    n = max(1, kaxs(1)) * max(1, kaxs(2)) * max(1, kaxs(3))
    select case (kfmt)
    case(GFMT_UR4)
       call sus_read_irec(ierr, u, vf, 0, swap, div=div, lmem=n)
    case(GFMT_UR8)
       call sus_read_irec(ierr, u, vd, 0, swap, div=div, lmem=n)
    case(GFMT_UI4)
       call sus_read_irec(ierr, u, vi, 0, swap, div=div, lmem=n)
    case(GFMT_MR4)
       ncom = count_packed(1, n, mold)
       call get_data_record(ierr, mb, u, krect)
       if (ierr.eq.0) call sus_read_irec(ierr, u, vi, 0, swap, div=div, lmem=ncom)
       if (ierr.eq.0) call sus_read_irec(ierr, u, vf, 0, swap, div=div, lmem=mb)
    case(GFMT_MR8)
       ncom = count_packed(1, n, mold)
       call get_data_record(ierr, mb, u, krect)
       if (ierr.eq.0) call sus_read_irec(ierr, u, vi, 0, swap, div=div, lmem=ncom)
       if (ierr.eq.0) call sus_read_irec(ierr, u, vd, 0, swap, div=div, lmem=mb)
    case(GFMT_MI4)
       ncom = count_packed(1, n, mold)
       call get_data_record(ierr, mb, u, krect)
       if (ierr.eq.0) call sus_read_irec(ierr, u, vi, 0, swap, div=div, lmem=ncom)
       if (ierr.eq.0) call sus_read_irec(ierr, u, vi, 0, swap, div=div, lmem=mb)
    case(GFMT_URC, GFMT_URC2)
       !! to do: dummy separator
       nrec = max(1, kaxs(3)) * 4
       call nio_skip_prec(ierr, u, nrec, krect)
    case (GFMT_URY:GFMT_URYend-1)
       nbits = kfmt - GFMT_URY
       nh = max(1, kaxs(1)) * max(1, kaxs(2))
       nk = max(1, kaxs(3))
       ncom  = count_packed(nbits, nh, mold)
       nn = nk * 2
       if (ierr.eq.0) call sus_read_irec(ierr, u, vd, 0, swap, div=div, lmem=nn)
       nn = nk * ncom
       if (ierr.eq.0) call sus_read_irec(ierr, u, vi, 0, swap, div=div, lmem=nn)
    case (GFMT_MRY:GFMT_MRYend-1)
       nbits = kfmt - GFMT_URY
       nh = max(1, kaxs(1)) * max(1, kaxs(2))
       nk = max(1, kaxs(3))
       nm = count_packed(nbits, nh, mold)
       if (ierr.eq.0) call get_data_record(ierr, ncom, u, krect)
       if (ierr.eq.0) call sus_read_irec(ierr, u, vi, 0, swap, div=div, lmem=nk)
       if (ierr.eq.0) call sus_read_irec(ierr, u, vi, 0, swap, div=div, lmem=nk)
       nn = nk * 2
       if (ierr.eq.0) call sus_read_irec(ierr, u, vd, 0, swap, div=div, lmem=nn)
       nn = nk * nm
       if (ierr.eq.0) call sus_read_irec(ierr, u, vi, 0, swap, div=div, lmem=nn)
       if (ierr.eq.0) call sus_read_irec(ierr, u, vi, 0, swap, div=div, lmem=ncom)
    case (GFMT_URT, GFMT_MRT)
       !! to do: dummy separator
       nrec = 1
       call nio_skip_prec(ierr, u, nrec, krect)
    case (GFMT_PI4, GFMT_PR4, GFMT_PR8, GFMT_PI4SV)
       !! to do: dummy separator
       nrec = 1
       call nio_skip_prec(ierr, u, nrec, krect)
    case default
       ierr = _ERROR(ERR_UNKNOWN_FORMAT)
    end select

  end subroutine nio_skip_data_irec

!!!_  & nio_data_records - inquire number of data records
  subroutine nio_data_records &
       & (n, head)
    use TOUZA_Nio_header,only: hi_DFMT, get_item
    implicit none
    integer,         intent(out) :: n
    character(len=*),intent(in)  :: head(*)

    character(len=litem) :: vp
    integer kfmt
    integer jerr
!!!_   . body
    jerr = 0
    n = 0
    if (jerr.eq.0) call get_item(jerr, head, vp, hi_DFMT)
    if (jerr.eq.0) call parse_record_fmt(jerr, kfmt, vp)
    if (jerr.eq.0) then
       select case (kfmt)
       case (GFMT_UR4, GFMT_UR8, GFMT_UI4)
          n = 1
       case (GFMT_MR4, GFMT_MR8, GFMT_MI4)
          n = 3
       case (GFMT_PI4, GFMT_PR4, GFMT_PR8, GFMT_PI4SV)
          n = 1
       case (GFMT_URC, GFMT_URC2)
          n = parse_header_size(head, 3)
          if (n.le.0) then
             jerr = _ERROR(ERR_UNKNOWN_FORMAT)
          else
             n = n * 4
          endif
       case (GFMT_URY:GFMT_URYend-1)
          n = 2
       case (GFMT_MRY:GFMT_MRYend-1)
          n = 6
       case (GFMT_URT)
          n = 1
       case (GFMT_MRT)
          n = 1
       case default
          jerr = _ERROR(ERR_UNKNOWN_FORMAT)
       end select
    endif
    if (jerr.lt.0) n = jerr
  end subroutine nio_data_records
!!!_  - nio_bwd_record - backword one record skip (lazy trial)
  subroutine nio_bwd_record &
       & (ierr, u, krect, limtry)
    use TOUZA_Nio_std,   only: choice
    use TOUZA_Nio_std,   only: sus_skip_irec, sus_skip_lrec, KIOFS
    use TOUZA_Nio_std,   only: sus_rseek, WHENCE_CURRENT, WHENCE_ABS
    use TOUZA_Nio_std,   only: sus_getpos
    use TOUZA_Nio_header,only: nitem, litem, hi_DFMT, get_item
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: u
    integer,intent(in),optional :: krect    ! initial guess
    integer,intent(in),optional :: limtry   ! trial limit
    integer l
    logical swap, lrec
    integer(kind=KIOFS) :: apini
    integer ndrec
    integer ltry
    integer kri

    ierr = 0
    ltry = choice(0, limtry)
    call sus_getpos(ierr, apini, u)
    if (ltry.le.0) ltry = 6  ! MRT records
    kri = choice(REC_DEFAULT, krect)
    swap = IAND(kri, REC_SWAP).gt.0
    lrec = IAND(kri, REC_LSEP).gt.0

    try_all: do l = 0, 3
       ndrec = 0
       ! write(*, *) 'bwd:', l, swap, lrec
       if (lrec) then
          kri = REC_LSEP
          if (swap) kri = IOR(kri, REC_SWAP)
          try_lrec: do
             if (ierr.eq.0) call sus_skip_lrec(ierr, u, -1, swap=swap)
             if (ierr.ne.0) exit try_lrec
             if (ierr.eq.0) call nio_check_magic_header(ierr, u, kri, ndrec)
             if (ierr.eq.0) return
             ndrec = ndrec + 1
             if (ndrec.gt.ltry) exit try_lrec
             ierr = 0
          enddo try_lrec
       else
          kri = 0
          if (swap) kri = IOR(kri, REC_SWAP)
          try_irec: do
             if (ierr.eq.0) call sus_skip_irec(ierr, u, -1, swap=swap)
             if (ierr.ne.0) exit try_irec
             if (ierr.eq.0) call nio_check_magic_header(ierr, u, kri, ndrec)
             if (ierr.eq.0) return
             ndrec = ndrec + 1
             if (ndrec.gt.ltry) exit try_irec
             ierr = 0
          enddo try_irec
       endif
       swap = .not.swap
       if (l.eq.1) lrec = .not.lrec
       call sus_rseek(ierr, u, apini, whence=WHENCE_ABS)
       ierr = 0
    enddo try_all
    if (ierr.eq.0) ierr = _ERROR(ERR_BROKEN_RECORD)
    return
  end subroutine nio_bwd_record
!!!_ + gtool-3 standard formats
!!!_  - review_ur8 - review U[RI]n data
  subroutine review_ur8_d &
       & (ierr,  ndata, &
       &  head,  u,     krect, v, flag)
    use TOUZA_Nio_std,only: KIOFS, KTGT=>KDBL
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_header,only: restore_item, hi_MISS
    implicit none
    integer,parameter :: KISRC=KI32
    integer,         intent(out)          :: ierr
    integer,         intent(out)          :: ndata       ! (physical) number of defined elements
    character(len=*),intent(in)           :: head(*)
    integer,         intent(in)           :: u
    integer,         intent(in)           :: krect
    real(kind=KTGT), intent(out),optional :: v(0:*)
    integer,         intent(in), optional :: flag
    integer jerr

    integer(kind=KIOFS) :: apini
    integer f
    integer ld
    real(kind=KRMIS) :: vmiss

    ierr = 0
    ! note: position must be just after gtool header part

    f = choice(-1, flag)
    if (f.lt.0) f = rev_pos_dhead

    ld = parse_header_size(head, -1, 1)
    ndata = -1

    if (ierr.eq.0) call restore_item(jerr, head, vmiss, hi_MISS)
    if (ierr.eq.0) then
       if (jerr.eq.0) then
          if (ierr.eq.0) call pre_review(ierr, apini, u, f)
          if (present(v)) then
             if (ierr.eq.0) call get_data_drecord(ierr, v, ld, u, krect)
             if (ierr.eq.0) ndata = COUNT(v(0:ld-1).ne.vmiss)
          else
             if (ierr.eq.0) call alloc_workd(ierr, ld, 'rev:ur8d')
             if (ierr.eq.0) call get_data_drecord(ierr, workd, ld, u, krect)
             if (ierr.eq.0) ndata = COUNT(workd(0:ld-1).ne.vmiss)
             if (ierr.eq.0) call alloc_workd(ierr, -1, 'rev:ur8d')
          endif
          if (ierr.eq.0) call post_review(ierr, apini, u, f)
       else
          ! no valid VMISS
          ndata = ld
          if (ierr.eq.0) call pre_review(ierr, apini, u, f)
          if (present(v)) then
             if (ierr.eq.0) call get_data_drecord(ierr, v, ld, u, krect)
          else
             if (ierr.eq.0) call get_data_drecord(ierr, workd, 0, u, krect)
          endif
          if (ierr.eq.0) call post_review(ierr, apini, u, f)
       endif
    endif
  end subroutine review_ur8_d
  subroutine review_ur8_f &
       & (ierr,  ndata, &
       &  head,  u,     krect, v, flag)
    use TOUZA_Nio_std,only: KIOFS, KTGT=>KFLT
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_header,only: restore_item, hi_MISS
    implicit none
    integer,parameter :: KISRC=KI32
    integer,         intent(out)          :: ierr
    integer,         intent(out)          :: ndata       ! (physical) number of defined elements
    character(len=*),intent(in)           :: head(*)
    integer,         intent(in)           :: u
    integer,         intent(in)           :: krect
    real(kind=KTGT), intent(out)          :: v(0:*)
    integer,         intent(in), optional :: flag
    integer ld
    ierr = 0
    ld = parse_header_size(head, -1, 1)
    if (ierr.eq.0) call alloc_workd(ierr, ld, 'rev:ur8f')
    if (ierr.eq.0) then
       call review_ur8_d(ierr, ndata, head, u, krect, workd, flag)
    endif
    if (ierr.eq.0) then
       v(0:ld-1) = REAL(workd(0:ld-1), KIND=KTGT)
    endif
    if (ierr.eq.0) call alloc_workd(ierr, -1, 'rev:ur8f')
  end subroutine review_ur8_f
  subroutine review_ur8_i &
       & (ierr,  ndata, &
       &  head,  u,     krect, v, flag)
    use TOUZA_Nio_std,only: KIOFS
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_header,only: restore_item, hi_MISS
    implicit none
    integer,parameter :: KISRC=KI32
    integer,         intent(out)          :: ierr
    integer,         intent(out)          :: ndata       ! (physical) number of defined elements
    character(len=*),intent(in)           :: head(*)
    integer,         intent(in)           :: u
    integer,         intent(in)           :: krect
    integer,         intent(out)          :: v(0:*)
    integer,         intent(in), optional :: flag
    integer ld
    ierr = 0
    ld = parse_header_size(head, -1, 1)
    if (ierr.eq.0) call alloc_workd(ierr, ld, 'rev:ur8i')
    if (ierr.eq.0) then
       call review_ur8_d(ierr, ndata, head, u, krect, workd, flag)
    endif
    if (ierr.eq.0) then
       v(0:ld-1) = INT(workd(0:ld-1))
    endif
    if (ierr.eq.0) call alloc_workd(ierr, -1, 'rev:ur8i')
  end subroutine review_ur8_i
!!!_  - review_ur4 - review U[RI]n data
  subroutine review_ur4_f &
       & (ierr,  ndata, &
       &  head,  u,     krect, v, flag)
    use TOUZA_Nio_std,only: KIOFS, KTGT=>KFLT
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_header,only: restore_item, hi_MISS
    implicit none
    integer,parameter :: KISRC=KI32
    integer,         intent(out)          :: ierr
    integer,         intent(out)          :: ndata       ! (physical) number of defined elements
    character(len=*),intent(in)           :: head(*)
    integer,         intent(in)           :: u
    integer,         intent(in)           :: krect
    real(kind=KTGT), intent(out),optional :: v(0:*)
    integer,         intent(in), optional :: flag
    integer jerr

    integer(kind=KIOFS) :: apini
    integer f
    integer ld
    real(kind=KRMIS) :: vmiss
    real(kind=KTGT)  :: vmt

    ierr = 0
    ! note: position must be just after gtool header part

    f = choice(-1, flag)
    if (f.lt.0) f = rev_pos_dhead

    ld = parse_header_size(head, -1, 1)
    ndata = -1

    if (ierr.eq.0) call restore_item(jerr, head, vmiss, hi_MISS)
    if (ierr.eq.0) then
       vmt = real(vmiss, kind=KTGT)
       if (jerr.eq.0) then
          if (ierr.eq.0) call pre_review(ierr, apini, u, f)
          if (present(v)) then
             if (ierr.eq.0) call get_data_frecord(ierr, v, ld, u, krect)
             if (ierr.eq.0) ndata = COUNT(v(0:ld-1).ne.vmt)
          else
             if (ierr.eq.0) call alloc_workf(ierr, ld, 'rev:ur4f')
             if (ierr.eq.0) call get_data_frecord(ierr, workf, ld, u, krect)
             if (ierr.eq.0) ndata = COUNT(workf(0:ld-1).ne.vmt)
             if (ierr.eq.0) call alloc_workf(ierr, -1, 'rev:ur4f')
          endif
          if (ierr.eq.0) call post_review(ierr, apini, u, f)
       else
          ! no valid VMISS
          ndata = ld
          if (ierr.eq.0) call pre_review(ierr, apini, u, f)
          if (present(v)) then
             if (ierr.eq.0) call get_data_frecord(ierr, v, ld, u, krect)
          else
             if (ierr.eq.0) call get_data_frecord(ierr, workf, 0, u, krect)
          endif
          if (ierr.eq.0) call post_review(ierr, apini, u, f)
       endif
    endif
  end subroutine review_ur4_f
  subroutine review_ur4_d &
       & (ierr,  ndata, &
       &  head,  u,     krect, v, flag)
    use TOUZA_Nio_std,only: KIOFS, KTGT=>KDBL
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_header,only: restore_item, hi_MISS
    implicit none
    integer,parameter :: KISRC=KI32
    integer,         intent(out)          :: ierr
    integer,         intent(out)          :: ndata       ! (physical) number of defined elements
    character(len=*),intent(in)           :: head(*)
    integer,         intent(in)           :: u
    integer,         intent(in)           :: krect
    real(kind=KTGT), intent(out)          :: v(0:*)
    integer,         intent(in), optional :: flag
    integer ld
    ierr = 0
    ld = parse_header_size(head, -1, 1)
    if (ierr.eq.0) call alloc_workf(ierr, ld, 'res:ur4d')
    if (ierr.eq.0) then
       call review_ur4_f(ierr, ndata, head, u, krect, workf, flag)
    endif
    if (ierr.eq.0) then
       v(0:ld-1) = REAL(workf(0:ld-1), KIND=KTGT)
    endif
    if (ierr.eq.0) call alloc_workf(ierr, -1, 'res:ur4d')
  end subroutine review_ur4_d
  subroutine review_ur4_i &
       & (ierr,  ndata, &
       &  head,  u,     krect, v, flag)
    use TOUZA_Nio_std,only: KIOFS
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_header,only: restore_item, hi_MISS
    implicit none
    integer,parameter :: KISRC=KI32
    integer,         intent(out)          :: ierr
    integer,         intent(out)          :: ndata       ! (physical) number of defined elements
    character(len=*),intent(in)           :: head(*)
    integer,         intent(in)           :: u
    integer,         intent(in)           :: krect
    integer,         intent(out)          :: v(0:*)
    integer,         intent(in), optional :: flag
    integer ld
    ierr = 0
    ld = parse_header_size(head, -1, 1)
    if (ierr.eq.0) call alloc_workf(ierr, ld, 'rev:ur4i')
    if (ierr.eq.0) then
       call review_ur4_f(ierr, ndata, head, u, krect, workf, flag)
    endif
    if (ierr.eq.0) then
       v(0:ld-1) = INT(workf(0:ld-1))
    endif
    if (ierr.eq.0) call alloc_workf(ierr, -1, 'rev:ur4i')
  end subroutine review_ur4_i
!!!_  - review_ui4 - review U[RI]n data
  subroutine review_ui4_i &
       & (ierr,  ndata, &
       &  head,  u,     krect, v, flag)
    use TOUZA_Nio_std,only: KIOFS
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_header,only: restore_item, hi_MISS
    implicit none
    integer,parameter :: KISRC=KI32
    integer,         intent(out)          :: ierr
    integer,         intent(out)          :: ndata       ! (physical) number of defined elements
    character(len=*),intent(in)           :: head(*)
    integer,         intent(in)           :: u
    integer,         intent(in)           :: krect
    integer,         intent(out),optional :: v(0:*)
    integer,         intent(in), optional :: flag
    integer jerr

    integer(kind=KIOFS) :: apini
    integer f
    integer ld
    real(kind=KRMIS) :: vmiss
    integer :: vmt

    ierr = 0
    ! note: position must be just after gtool header part

    f = choice(-1, flag)
    if (f.lt.0) f = rev_pos_dhead

    ld = parse_header_size(head, -1, 1)
    ndata = -1

    if (ierr.eq.0) call restore_item(jerr, head, vmiss, hi_MISS)
    if (ierr.eq.0) then
       vmt = INT(vmiss)
       if (jerr.eq.0) then
          if (ierr.eq.0) call pre_review(ierr, apini, u, f)
          if (present(v)) then
             if (ierr.eq.0) call get_data_irecord(ierr, v, ld, u, krect)
             if (ierr.eq.0) ndata = COUNT(v(0:ld-1).ne.vmt)
          else
             if (ierr.eq.0) call alloc_worki(ierr, ld, 'rev:ui4i')
             if (ierr.eq.0) call get_data_irecord(ierr, worki, ld, u, krect)
             if (ierr.eq.0) ndata = COUNT(worki(0:ld-1).ne.vmt)
             if (ierr.eq.0) call alloc_worki(ierr, -1, 'rev:ui4i')
          endif
          if (ierr.eq.0) call post_review(ierr, apini, u, f)
       else
          ! no valid VMISS
          ndata = ld
          if (ierr.eq.0) call pre_review(ierr, apini, u, f)
          if (present(v)) then
             if (ierr.eq.0) call get_data_irecord(ierr, v, ld, u, krect)
          else
             if (ierr.eq.0) call get_data_irecord(ierr, worki, 0, u, krect)
          endif
          if (ierr.eq.0) call post_review(ierr, apini, u, f)
       endif
    endif
  end subroutine review_ui4_i
  subroutine review_ui4_d &
       & (ierr,  ndata, &
       &  head,  u,     krect, v, flag)
    use TOUZA_Nio_std,only: KIOFS, KTGT=>KDBL
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_header,only: restore_item, hi_MISS
    implicit none
    integer,         intent(out)          :: ierr
    integer,         intent(out)          :: ndata       ! (physical) number of defined elements
    character(len=*),intent(in)           :: head(*)
    integer,         intent(in)           :: u
    integer,         intent(in)           :: krect
    real(kind=KTGT), intent(out)          :: v(0:*)
    integer,         intent(in), optional :: flag
    integer ld
    ierr = 0
    ld = parse_header_size(head, -1, 1)
    if (ierr.eq.0) call alloc_worki(ierr, ld, 'rev:ui4d')
    if (ierr.eq.0) then
       call review_ur4_i(ierr, ndata, head, u, krect, worki, flag)
    endif
    if (ierr.eq.0) then
       v(0:ld-1) = REAL(worki(0:ld-1), KIND=KTGT)
    endif
    if (ierr.eq.0) call alloc_worki(ierr, -1, 'rev:ui4d')
  end subroutine review_ui4_d
  subroutine review_ui4_f &
       & (ierr,  ndata, &
       &  head,  u,     krect, v, flag)
    use TOUZA_Nio_std,only: KIOFS, KTGT=>KFLT
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_header,only: restore_item, hi_MISS
    implicit none
    integer,         intent(out)          :: ierr
    integer,         intent(out)          :: ndata       ! (physical) number of defined elements
    character(len=*),intent(in)           :: head(*)
    integer,         intent(in)           :: u
    integer,         intent(in)           :: krect
    real(kind=KTGT), intent(out)          :: v(0:*)
    integer,         intent(in), optional :: flag
    integer ld
    ierr = 0
    ld = parse_header_size(head, -1, 1)
    if (ierr.eq.0) call alloc_worki(ierr, ld, 'rev:ui4f')
    if (ierr.eq.0) then
       call review_ur4_i(ierr, ndata, head, u, krect, worki, flag)
    endif
    if (ierr.eq.0) then
       v(0:ld-1) = REAL(worki(0:ld-1), KIND=KTGT)
    endif
    if (ierr.eq.0) call alloc_worki(ierr, -1, 'rev:ui4f')
  end subroutine review_ui4_f

!!!_  - restore_ur8_packed_d
  subroutine restore_ur8_packed_d &
       & (ierr, &
       &  d,    ldata, subv, ends, u, krect, vmiss, mfull, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KDBL, KRSRC=KDBL
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(out)         :: d(0:*)
    integer,         intent(in)          :: ldata        ! limit size of of d
    integer,         intent(inout)       :: subv(0:*)
    integer,         intent(inout)       :: ends(0:*)
    integer,         intent(in)          :: u
    integer,         intent(in)          :: krect
    real(kind=KRMIS),intent(in)          :: vmiss
    integer,         intent(in)          :: mfull        ! (logical) full data size
    integer,         intent(in)          :: kaxs(*)
    integer,         optional,intent(in) :: citer        ! iteration index
    integer,         optional,intent(in) :: check        ! whether subv, ends are references

    ! note: position must be just after gtool header part
    ierr = 0
    if (ierr.eq.0) call alloc_workd(ierr, mfull, 'res:ur8d')
    ! data array, as is
    if (ierr.eq.0) call get_data_drecord(ierr, workd, mfull, u, krect)
    if (ierr.eq.0) then
       call pack_plain_data &
            & (ierr, d, ldata, subv, ends, vmiss, workd, kaxs, citer, check)
    endif
    if (ierr.eq.0) call alloc_workd(ierr, -1, 'res:ur8d')
  end subroutine restore_ur8_packed_d
  subroutine restore_ur8_packed_f &
       & (ierr, &
       &  d,    ldata, subv, ends, u, krect, vmiss, mfull, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KFLT, KRSRC=KDBL
    integer,         intent(out)         :: ierr
    real(kind=KARG),    intent(out)         :: d(0:*)
    integer,         intent(in)          :: ldata        ! limit size of of d
    integer,         intent(inout)       :: subv(0:*)
    integer,         intent(inout)       :: ends(0:*)
    integer,         intent(in)          :: u
    integer,         intent(in)          :: krect
    real(kind=KRMIS),intent(in)          :: vmiss
    integer,         intent(in)          :: mfull        ! (logical) full data size
    integer,         intent(in)          :: kaxs(*)
    integer,         optional,intent(in) :: citer        ! iteration index
    integer,         optional,intent(in) :: check        ! whether subv, ends are references

    ! note: position must be just after gtool header part
    ierr = 0
    if (ierr.eq.0) call alloc_workd(ierr, mfull, 'res:ur8d')
    ! data array, as is
    if (ierr.eq.0) call get_data_drecord(ierr, workd, mfull, u, krect)
    if (ierr.eq.0) then
       call pack_plain_data &
            & (ierr, d, ldata, subv, ends, vmiss, workd, kaxs, citer, check)
    endif
    if (ierr.eq.0) call alloc_workd(ierr, -1, 'res:ur8d')
  end subroutine restore_ur8_packed_f
  subroutine restore_ur8_packed_i &
       & (ierr, &
       &  d,    ldata, subv, ends, u, krect, vmiss, mfull, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KI32, KRSRC=KDBL
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: d(0:*)
    integer,         intent(in)          :: ldata        ! limit size of of d
    integer,         intent(inout)       :: subv(0:*)
    integer,         intent(inout)       :: ends(0:*)
    integer,         intent(in)          :: u
    integer,         intent(in)          :: krect
    real(kind=KRMIS),intent(in)          :: vmiss
    integer,         intent(in)          :: mfull        ! (logical) full data size
    integer,         intent(in)          :: kaxs(*)
    integer,         optional,intent(in) :: citer        ! iteration index
    integer,         optional,intent(in) :: check        ! whether subv, ends are references

    ! note: position must be just after gtool header part
    ierr = 0
    if (ierr.eq.0) call alloc_workd(ierr, mfull, 'res:ur8d')
    ! data array, as is
    if (ierr.eq.0) call get_data_drecord(ierr, workd, mfull, u, krect)
    if (ierr.eq.0) then
       call pack_plain_data &
            & (ierr, d, ldata, subv, ends, vmiss, workd, kaxs, citer, check)
    endif
    if (ierr.eq.0) call alloc_workd(ierr, -1, 'res:ur8d')
  end subroutine restore_ur8_packed_i

!!!_  - restore_ur4_packed_f
  subroutine restore_ur4_packed_f &
       & (ierr, &
       &  d,    ldata, subv, ends, u, krect, vmiss, mfull, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KFLT, KRSRC=KFLT
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(out)         :: d(0:*)
    integer,         intent(in)          :: ldata        ! limit size of of d
    integer,         intent(inout)       :: subv(0:*)
    integer,         intent(inout)       :: ends(0:*)
    integer,         intent(in)          :: u
    integer,         intent(in)          :: krect
    real(kind=KRMIS),intent(in)          :: vmiss
    integer,         intent(in)          :: mfull        ! (logical) full data size
    integer,         intent(in)          :: kaxs(*)
    integer,         optional,intent(in) :: citer        ! iteration index
    integer,         optional,intent(in) :: check        ! whether subv, ends are references

    ! note: position must be just after gtool header part
    ierr = 0
    if (ierr.eq.0) call alloc_workf(ierr, mfull, 'res:ur4f')
    ! data array, as is
    if (ierr.eq.0) call get_data_frecord(ierr, workf, mfull, u, krect)
    if (ierr.eq.0) then
       call pack_plain_data &
            & (ierr, d, ldata, subv, ends, vmiss, workf, kaxs, citer, check)
    endif
    if (ierr.eq.0) call alloc_workd(ierr, -1, 'res:ur4f')
  end subroutine restore_ur4_packed_f
  subroutine restore_ur4_packed_d &
       & (ierr, &
       &  d,    ldata, subv, ends, u, krect, vmiss, mfull, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KDBL, KRSRC=KFLT
    integer,         intent(out)         :: ierr
    real(kind=KARG),    intent(out)         :: d(0:*)
    integer,         intent(in)          :: ldata        ! limit size of of d
    integer,         intent(inout)       :: subv(0:*)
    integer,         intent(inout)       :: ends(0:*)
    integer,         intent(in)          :: u
    integer,         intent(in)          :: krect
    real(kind=KRMIS),intent(in)          :: vmiss
    integer,         intent(in)          :: mfull        ! (logical) full data size
    integer,         intent(in)          :: kaxs(*)
    integer,         optional,intent(in) :: citer        ! iteration index
    integer,         optional,intent(in) :: check        ! whether subv, ends are references

    ! note: position must be just after gtool header part
    ierr = 0
    if (ierr.eq.0) call alloc_workf(ierr, mfull, 'res:ur4f')
    ! data array, as is
    if (ierr.eq.0) call get_data_frecord(ierr, workf, mfull, u, krect)
    if (ierr.eq.0) then
       call pack_plain_data &
            & (ierr, d, ldata, subv, ends, vmiss, workf, kaxs, citer, check)
    endif
    if (ierr.eq.0) call alloc_workd(ierr, -1, 'res:ur4f')
  end subroutine restore_ur4_packed_d
  subroutine restore_ur4_packed_i &
       & (ierr, &
       &  d,    ldata, subv, ends, u, krect, vmiss, mfull, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KI32, KRSRC=KFLT
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: d(0:*)
    integer,         intent(in)          :: ldata        ! limit size of of d
    integer,         intent(inout)       :: subv(0:*)
    integer,         intent(inout)       :: ends(0:*)
    integer,         intent(in)          :: u
    integer,         intent(in)          :: krect
    real(kind=KRMIS),intent(in)          :: vmiss
    integer,         intent(in)          :: mfull        ! (logical) full data size
    integer,         intent(in)          :: kaxs(*)
    integer,         optional,intent(in) :: citer        ! iteration index
    integer,         optional,intent(in) :: check        ! whether subv, ends are references

    ! note: position must be just after gtool header part
    ierr = 0
    if (ierr.eq.0) call alloc_workf(ierr, mfull, 'res:ur4f')
    ! data array, as is
    if (ierr.eq.0) call get_data_frecord(ierr, workf, mfull, u, krect)
    if (ierr.eq.0) then
       call pack_plain_data &
            & (ierr, d, ldata, subv, ends, vmiss, workf, kaxs, citer, check)
    endif
    if (ierr.eq.0) call alloc_workd(ierr, -1, 'res:ur4f')
  end subroutine restore_ur4_packed_i

!!!_  - restore_ui4_packed_i
  subroutine restore_ui4_packed_i &
       & (ierr, &
       &  d,    ldata, subv, ends, u, krect, vmiss, mfull, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: d(0:*)
    integer,         intent(in)          :: ldata        ! limit size of of d
    integer,         intent(inout)       :: subv(0:*)
    integer,         intent(inout)       :: ends(0:*)
    integer,         intent(in)          :: u
    integer,         intent(in)          :: krect
    real(kind=KRMIS),intent(in)          :: vmiss
    integer,         intent(in)          :: mfull        ! (logical) full data size
    integer,         intent(in)          :: kaxs(*)
    integer,         optional,intent(in) :: citer        ! iteration index
    integer,         optional,intent(in) :: check        ! whether subv, ends are references

    ! note: position must be just after gtool header part
    ierr = 0
    if (ierr.eq.0) call alloc_worki(ierr, mfull, 'res:ui4i')
    ! data array, as is
    if (ierr.eq.0) call get_data_irecord(ierr, worki, mfull, u, krect)
    if (ierr.eq.0) then
       call pack_plain_data &
            & (ierr, d, ldata, subv, ends, vmiss, worki, kaxs, citer, check)
    endif
    if (ierr.eq.0) call alloc_worki(ierr, -1, 'res:ui4i')
  end subroutine restore_ui4_packed_i
  subroutine restore_ui4_packed_d &
       & (ierr, &
       &  d,    ldata, subv, ends, u, krect, vmiss, mfull, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(out)         :: d(0:*)
    integer,         intent(in)          :: ldata        ! limit size of of d
    integer,         intent(inout)       :: subv(0:*)
    integer,         intent(inout)       :: ends(0:*)
    integer,         intent(in)          :: u
    integer,         intent(in)          :: krect
    real(kind=KRMIS),intent(in)          :: vmiss
    integer,         intent(in)          :: mfull        ! (logical) full data size
    integer,         intent(in)          :: kaxs(*)
    integer,         optional,intent(in) :: citer        ! iteration index
    integer,         optional,intent(in) :: check        ! whether subv, ends are references

    ! note: position must be just after gtool header part
    ierr = 0
    if (ierr.eq.0) call alloc_worki(ierr, mfull, 'res:ui4i')
    ! data array, as is
    if (ierr.eq.0) call get_data_irecord(ierr, worki, mfull, u, krect)
    if (ierr.eq.0) then
       call pack_plain_data &
            & (ierr, d, ldata, subv, ends, vmiss, worki, kaxs, citer, check)
    endif
    if (ierr.eq.0) call alloc_worki(ierr, -1, 'res:ui4i')
  end subroutine restore_ui4_packed_d
  subroutine restore_ui4_packed_f &
       & (ierr, &
       &  d,    ldata, subv, ends, u, krect, vmiss, mfull, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    integer,parameter :: KARG=KFLT
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(out)         :: d(0:*)
    integer,         intent(in)          :: ldata        ! limit size of of d
    integer,         intent(inout)       :: subv(0:*)
    integer,         intent(inout)       :: ends(0:*)
    integer,         intent(in)          :: u
    integer,         intent(in)          :: krect
    real(kind=KRMIS),intent(in)          :: vmiss
    integer,         intent(in)          :: mfull        ! (logical) full data size
    integer,         intent(in)          :: kaxs(*)
    integer,         optional,intent(in) :: citer        ! iteration index
    integer,         optional,intent(in) :: check        ! whether subv, ends are references

    ! note: position must be just after gtool header part
    ierr = 0
    if (ierr.eq.0) call alloc_worki(ierr, mfull, 'res:ui4i')
    ! data array, as is
    if (ierr.eq.0) call get_data_irecord(ierr, worki, mfull, u, krect)
    if (ierr.eq.0) then
       call pack_plain_data &
            & (ierr, d, ldata, subv, ends, vmiss, worki, kaxs, citer, check)
    endif
    if (ierr.eq.0) call alloc_worki(ierr, -1, 'res:ui4i')
  end subroutine restore_ui4_packed_f

!!!_  - pack_plain_data
  subroutine pack_plain_data_dd &
       & (ierr, &
       &  d,    ldata, subv, ends, vmiss, src, kaxs, citer, check)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KDBL, KRSRC=KDBL
    integer,         intent(out)   :: ierr
    real(kind=KARG), intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: subv(0:*)
    integer,         intent(inout) :: ends(0:*)
    real(kind=KRMIS),intent(in)    :: vmiss
    real(kind=KRSRC),intent(in)    :: src(0:*)
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check        ! whether subv, ends are references

    integer ch
    integer mi, mo, ci
    integer jo
    integer jdb, jde
    integer jso

    ! note: position must be just after gtool header part
    ierr = 0
    ch = choice(packed_read, check)
    ci = choice(0, citer)

    if (ci.le.0.or.ci.gt.laxs) then
       mo = 1
       mi = product(max(1, kaxs(1:laxs)))
    else
       mo = max(1, product(max(1, kaxs(ci:laxs))))
       mi = max(1, product(max(1, kaxs(1:ci-1))))
    endif

    select case(ch)
    case(packed_check)
       ! consistency check for subv ends
       if (ierr.eq.0) call alloc_wsubv(ierr, max(ldata, mo), 'pack:d')
       if (ierr.eq.0) call subv_encode(ierr, wdsubv, wssubv, ldata, src, mi, mo, vmiss)
       if (ierr.eq.0) then
          if (ANY(ends(0:mo-1).ne.wssubv(0:mo-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'pack:d')
    case(packed_read)
       ! new creation of subv
       call subv_encode(ierr, subv, ends, ldata, src, mi, mo, vmiss)
    case(packed_ignore)
       continue
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    if (ierr.eq.0) then
       jdb = 0
       do jo = 0, mo - 1
          jde = ends(jo)
          jso = mi * jo
          d(jdb:jde-1) = real(src(subv(jdb:jde-1) + jso), kind=KARG)
          jdb = jde
       enddo
    endif
    return
  end subroutine pack_plain_data_dd
  subroutine pack_plain_data_df &
       & (ierr, &
       &  d,    ldata, subv, ends, vmiss, src, kaxs, citer, check)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KFLT, KRSRC=KDBL
    integer,         intent(out)   :: ierr
    real(kind=KARG), intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: subv(0:*)
    integer,         intent(inout) :: ends(0:*)
    real(kind=KRMIS),intent(in)    :: vmiss
    real(kind=KRSRC),intent(in)    :: src(0:*)
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check        ! whether subv, ends are references

    integer ch
    integer mi, mo, ci
    integer jo
    integer jdb, jde
    integer jso

    ! note: position must be just after gtool header part
    ierr = 0
    ch = choice(packed_read, check)
    ci = choice(0, citer)

    if (ci.le.0.or.ci.gt.laxs) then
       mo = 1
       mi = product(max(1, kaxs(1:laxs)))
    else
       mo = max(1, product(max(1, kaxs(ci:laxs))))
       mi = max(1, product(max(1, kaxs(1:ci-1))))
    endif

    select case(ch)
    case(packed_check)
       ! consistency check for subv ends
       if (ierr.eq.0) call alloc_wsubv(ierr, max(ldata, mo), 'pack:d')
       if (ierr.eq.0) call subv_encode(ierr, wdsubv, wssubv, ldata, src, mi, mo, vmiss)
       if (ierr.eq.0) then
          if (ANY(ends(0:mo-1).ne.wssubv(0:mo-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'pack:d')
    case(packed_read)
       ! new creation of subv
       call subv_encode(ierr, subv, ends, ldata, src, mi, mo, vmiss)
    case(packed_ignore)
       continue
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    if (ierr.eq.0) then
       jdb = 0
       do jo = 0, mo - 1
          jde = ends(jo)
          jso = mi * jo
          d(jdb:jde-1) = real(src(subv(jdb:jde-1) + jso), kind=KARG)
          jdb = jde
       enddo
    endif
    return
  end subroutine pack_plain_data_df
  subroutine pack_plain_data_di &
       & (ierr, &
       &  d,    ldata, subv, ends, vmiss, src, kaxs, citer, check)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KRSRC=KDBL
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: subv(0:*)
    integer,         intent(inout) :: ends(0:*)
    real(kind=KRMIS),intent(in)    :: vmiss
    real(kind=KRSRC),intent(in)    :: src(0:*)
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check        ! whether subv, ends are references

    integer ch
    integer mi, mo, ci
    integer jo
    integer jdb, jde
    integer jso

    ! note: position must be just after gtool header part
    ierr = 0
    ch = choice(packed_read, check)
    ci = choice(0, citer)

    if (ci.le.0.or.ci.gt.laxs) then
       mo = 1
       mi = product(max(1, kaxs(1:laxs)))
    else
       mo = max(1, product(max(1, kaxs(ci:laxs))))
       mi = max(1, product(max(1, kaxs(1:ci-1))))
    endif

    select case(ch)
    case(packed_check)
       ! consistency check for subv ends
       if (ierr.eq.0) call alloc_wsubv(ierr, max(ldata, mo), 'pack:d')
       if (ierr.eq.0) call subv_encode(ierr, wdsubv, wssubv, ldata, src, mi, mo, vmiss)
       if (ierr.eq.0) then
          if (ANY(ends(0:mo-1).ne.wssubv(0:mo-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'pack:d')
    case(packed_read)
       ! new creation of subv
       call subv_encode(ierr, subv, ends, ldata, src, mi, mo, vmiss)
    case(packed_ignore)
       continue
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    if (ierr.eq.0) then
       jdb = 0
       do jo = 0, mo - 1
          jde = ends(jo)
          jso = mi * jo
          d(jdb:jde-1) = int(src(subv(jdb:jde-1) + jso))
          jdb = jde
       enddo
    endif
    return
  end subroutine pack_plain_data_di
  subroutine pack_plain_data_ff &
       & (ierr, &
       &  d,    ldata, subv, ends, vmiss, src, kaxs, citer, check)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KFLT, KRSRC=KFLT
    integer,         intent(out)   :: ierr
    real(kind=KARG), intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: subv(0:*)
    integer,         intent(inout) :: ends(0:*)
    real(kind=KRMIS),intent(in)    :: vmiss
    real(kind=KRSRC),intent(in)    :: src(0:*)
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check        ! whether subv, ends are references

    integer ch
    integer mi, mo, ci
    integer jo
    integer jdb, jde
    integer jso

    ! note: position must be just after gtool header part
    ierr = 0
    ch = choice(packed_read, check)
    ci = choice(0, citer)

    if (ci.le.0.or.ci.gt.laxs) then
       mo = 1
       mi = product(max(1, kaxs(1:laxs)))
    else
       mo = max(1, product(max(1, kaxs(ci:laxs))))
       mi = max(1, product(max(1, kaxs(1:ci-1))))
    endif

    select case(ch)
    case(packed_check)
       ! consistency check for subv ends
       if (ierr.eq.0) call alloc_wsubv(ierr, max(ldata, mo), 'pack:f')
       if (ierr.eq.0) call subv_encode(ierr, wdsubv, wssubv, ldata, src, mi, mo, vmiss)
       if (ierr.eq.0) then
          if (ANY(ends(0:mo-1).ne.wssubv(0:mo-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'pack:f')
    case(packed_read)
       ! new creation of subv
       call subv_encode(ierr, subv, ends, ldata, src, mi, mo, vmiss)
    case(packed_ignore)
       continue
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    if (ierr.eq.0) then
       jdb = 0
       do jo = 0, mo - 1
          jde = ends(jo)
          jso = mi * jo
          d(jdb:jde-1) = real(src(subv(jdb:jde-1) + jso), kind=KARG)
          jdb = jde
       enddo
    endif
    return
  end subroutine pack_plain_data_ff
  subroutine pack_plain_data_fd &
       & (ierr, &
       &  d,    ldata, subv, ends, vmiss, src, kaxs, citer, check)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KDBL, KRSRC=KFLT
    integer,         intent(out)   :: ierr
    real(kind=KARG), intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: subv(0:*)
    integer,         intent(inout) :: ends(0:*)
    real(kind=KRMIS),intent(in)    :: vmiss
    real(kind=KRSRC),intent(in)    :: src(0:*)
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check        ! whether subv, ends are references

    integer ch
    integer mi, mo, ci
    integer jo
    integer jdb, jde
    integer jso

    ! note: position must be just after gtool header part
    ierr = 0
    ch = choice(packed_read, check)
    ci = choice(0, citer)

    if (ci.le.0.or.ci.gt.laxs) then
       mo = 1
       mi = product(max(1, kaxs(1:laxs)))
    else
       mo = max(1, product(max(1, kaxs(ci:laxs))))
       mi = max(1, product(max(1, kaxs(1:ci-1))))
    endif

    select case(ch)
    case(packed_check)
       ! consistency check for subv ends
       if (ierr.eq.0) call alloc_wsubv(ierr, max(ldata, mo), 'pack:f')
       if (ierr.eq.0) call subv_encode(ierr, wdsubv, wssubv, ldata, src, mi, mo, vmiss)
       if (ierr.eq.0) then
          if (ANY(ends(0:mo-1).ne.wssubv(0:mo-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'pack:f')
    case(packed_read)
       ! new creation of subv
       call subv_encode(ierr, subv, ends, ldata, src, mi, mo, vmiss)
    case(packed_ignore)
       continue
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    if (ierr.eq.0) then
       jdb = 0
       do jo = 0, mo - 1
          jde = ends(jo)
          jso = mi * jo
          d(jdb:jde-1) = real(src(subv(jdb:jde-1) + jso), kind=KARG)
          jdb = jde
       enddo
    endif
    return
  end subroutine pack_plain_data_fd
  subroutine pack_plain_data_fi &
       & (ierr, &
       &  d,    ldata, subv, ends, vmiss, src, kaxs, citer, check)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KRSRC=KFLT
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: subv(0:*)
    integer,         intent(inout) :: ends(0:*)
    real(kind=KRMIS),intent(in)    :: vmiss
    real(kind=KRSRC),intent(in)    :: src(0:*)
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check        ! whether subv, ends are references

    integer ch
    integer mi, mo, ci
    integer jo
    integer jdb, jde
    integer jso

    ! note: position must be just after gtool header part
    ierr = 0
    ch = choice(packed_read, check)
    ci = choice(0, citer)

    if (ci.le.0.or.ci.gt.laxs) then
       mo = 1
       mi = product(max(1, kaxs(1:laxs)))
    else
       mo = max(1, product(max(1, kaxs(ci:laxs))))
       mi = max(1, product(max(1, kaxs(1:ci-1))))
    endif

    select case(ch)
    case(packed_check)
       ! consistency check for subv ends
       if (ierr.eq.0) call alloc_wsubv(ierr, max(ldata, mo), 'pack:f')
       if (ierr.eq.0) call subv_encode(ierr, wdsubv, wssubv, ldata, src, mi, mo, vmiss)
       if (ierr.eq.0) then
          if (ANY(ends(0:mo-1).ne.wssubv(0:mo-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'pack:f')
    case(packed_read)
       ! new creation of subv
       call subv_encode(ierr, subv, ends, ldata, src, mi, mo, vmiss)
    case(packed_ignore)
       continue
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    if (ierr.eq.0) then
       jdb = 0
       do jo = 0, mo - 1
          jde = ends(jo)
          jso = mi * jo
          d(jdb:jde-1) = int(src(subv(jdb:jde-1) + jso))
          jdb = jde
       enddo
    endif
    return
  end subroutine pack_plain_data_fi
  subroutine pack_plain_data_ii &
       & (ierr, &
       &  d,    ldata, subv, ends, vmiss, src, kaxs, citer, check)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: subv(0:*)
    integer,         intent(inout) :: ends(0:*)
    real(kind=KRMIS),intent(in)    :: vmiss
    integer,         intent(in)    :: src(0:*)
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check        ! whether subv, ends are references

    integer ch
    integer mi, mo, ci
    integer jo
    integer jdb, jde
    integer jso

    ! note: position must be just after gtool header part
    ierr = 0
    ch = choice(packed_read, check)
    ci = choice(0, citer)

    if (ci.le.0.or.ci.gt.laxs) then
       mo = 1
       mi = product(max(1, kaxs(1:laxs)))
    else
       mo = max(1, product(max(1, kaxs(ci:laxs))))
       mi = max(1, product(max(1, kaxs(1:ci-1))))
    endif

    select case(ch)
    case(packed_check)
       ! consistency check for subv ends
       if (ierr.eq.0) call alloc_wsubv(ierr, max(ldata, mo), 'pack:i')
       if (ierr.eq.0) call subv_encode(ierr, wdsubv, wssubv, ldata, src, mi, mo, vmiss)
       if (ierr.eq.0) then
          if (ANY(ends(0:mo-1).ne.wssubv(0:mo-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'pack:i')
    case(packed_read)
       ! new creation of subv
       call subv_encode(ierr, subv, ends, ldata, src, mi, mo, vmiss)
    case(packed_ignore)
       continue
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    if (ierr.eq.0) then
       jdb = 0
       do jo = 0, mo - 1
          jde = ends(jo)
          jso = mi * jo
          d(jdb:jde-1) = int(src(subv(jdb:jde-1) + jso))
          jdb = jde
       enddo
    endif
    return
  end subroutine pack_plain_data_ii
  subroutine pack_plain_data_id &
       & (ierr, &
       &  d,    ldata, subv, ends, vmiss, src, kaxs, citer, check)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out)   :: ierr
    real(kind=KARG), intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: subv(0:*)
    integer,         intent(inout) :: ends(0:*)
    real(kind=KRMIS),intent(in)    :: vmiss
    integer,         intent(in)    :: src(0:*)
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check        ! whether subv, ends are references

    integer ch
    integer mi, mo, ci
    integer jo
    integer jdb, jde
    integer jso

    ! note: position must be just after gtool header part
    ierr = 0
    ch = choice(packed_read, check)
    ci = choice(0, citer)

    if (ci.le.0.or.ci.gt.laxs) then
       mo = 1
       mi = product(max(1, kaxs(1:laxs)))
    else
       mo = max(1, product(max(1, kaxs(ci:laxs))))
       mi = max(1, product(max(1, kaxs(1:ci-1))))
    endif

    select case(ch)
    case(packed_check)
       ! consistency check for subv ends
       if (ierr.eq.0) call alloc_wsubv(ierr, max(ldata, mo), 'pack:i')
       if (ierr.eq.0) call subv_encode(ierr, wdsubv, wssubv, ldata, src, mi, mo, vmiss)
       if (ierr.eq.0) then
          if (ANY(ends(0:mo-1).ne.wssubv(0:mo-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'pack:i')
    case(packed_read)
       ! new creation of subv
       call subv_encode(ierr, subv, ends, ldata, src, mi, mo, vmiss)
    case(packed_ignore)
       continue
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    if (ierr.eq.0) then
       jdb = 0
       do jo = 0, mo - 1
          jde = ends(jo)
          jso = mi * jo
          d(jdb:jde-1) = real(src(subv(jdb:jde-1) + jso), kind=KARG)
          jdb = jde
       enddo
    endif
    return
  end subroutine pack_plain_data_id
  subroutine pack_plain_data_if &
       & (ierr, &
       &  d,    ldata, subv, ends, vmiss, src, kaxs, citer, check)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out)   :: ierr
    real(kind=KARG), intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: subv(0:*)
    integer,         intent(inout) :: ends(0:*)
    real(kind=KRMIS),intent(in)    :: vmiss
    integer,         intent(in)    :: src(0:*)
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check        ! whether subv, ends are references

    integer ch
    integer mi, mo, ci
    integer jo
    integer jdb, jde
    integer jso

    ! note: position must be just after gtool header part
    ierr = 0
    ch = choice(packed_read, check)
    ci = choice(0, citer)

    if (ci.le.0.or.ci.gt.laxs) then
       mo = 1
       mi = product(max(1, kaxs(1:laxs)))
    else
       mo = max(1, product(max(1, kaxs(ci:laxs))))
       mi = max(1, product(max(1, kaxs(1:ci-1))))
    endif

    select case(ch)
    case(packed_check)
       ! consistency check for subv ends
       if (ierr.eq.0) call alloc_wsubv(ierr, max(ldata, mo), 'pack:i')
       if (ierr.eq.0) call subv_encode(ierr, wdsubv, wssubv, ldata, src, mi, mo, vmiss)
       if (ierr.eq.0) then
          if (ANY(ends(0:mo-1).ne.wssubv(0:mo-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'pack:i')
    case(packed_read)
       ! new creation of subv
       call subv_encode(ierr, subv, ends, ldata, src, mi, mo, vmiss)
    case(packed_ignore)
       continue
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    if (ierr.eq.0) then
       jdb = 0
       do jo = 0, mo - 1
          jde = ends(jo)
          jso = mi * jo
          d(jdb:jde-1) = real(src(subv(jdb:jde-1) + jso), kind=KARG)
          jdb = jde
       enddo
    endif
    return
  end subroutine pack_plain_data_if

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
    ! integer jk
    ! integer mh
    ! integer(kind=KISRC) :: nd, ne
    ! integer(kind=KISRC) :: icom((nh + 1)/2)
    ! integer(kind=KISRC) :: idec(nh+1)
    ! real(kind=KRSRC)    :: vmin, f1, f2
    ! integer,parameter   :: mbits = BIT_SIZE(0_KISRC) / 2
    ! integer jdb, jde

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
       &  d, nd, u, krect, vmiss, kfmt, mh, mk, bes, nr)
    use TOUZA_Trp,only: count_packed,  pack_restore
    use TOUZA_Trp,only: gen_bfc_slice, pack_restore_dunp
    use TOUZA_Trp,only: npropd, p_cbgn
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(KIND=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: nd
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(KIND=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt
    integer,         intent(in)  :: mh, mk
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr

    integer             :: nbits, mcom
    integer             :: idec(0:mh-1)
    integer(KIND=KISRC) :: icom(0:mh * mk - 1)
    integer(KIND=KISRC) :: imiss
    real(KIND=KRSRC)    :: dma(0:2 * mk - 1)
    integer,parameter   :: mold = 0_KISRC
    integer             :: jk, jdb, jde
    integer             :: jc
    integer,parameter :: cz = 3
    integer kpack
    integer :: dunp(npropd, 0:mh), rfil(0:mh)
    integer :: nunp,               nfil
    integer :: nh

    ierr = 0
    nbits = kfmt - GFMT_URY
    mcom  = count_packed(nbits, mh, mold)
    ! imiss = ISHFT(1, nbits) - 1
    imiss = IBITS(NOT(0), 0, nbits)
    kpack = legacy_unpacking(nbits, mh)

    call get_data_record(ierr, dma, 2 * mk, u, krect)
    if (present(bes)) then
       nh = 0
       if (.not.present(nr)) ierr = _ERROR(ERR_INVALID_ITEM)
       if (ierr.eq.0) then
          if (nd.ge.0.and.nd.lt.count_bes(bes, nr)) then
             ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
             return
          endif
       endif
       if (ierr.eq.0) then
          nfil = 0
          rfil(nfil) = mcom * bes(1, cz)
          call get_data_record_suspend(ierr, icom, 0, u, krect, +1, rfil, nfil)
       endif
       if (ierr.eq.0) then
          call gen_bfc_slice &
               & (ierr, dunp, nunp, rfil, nfil, nbits, mh, bes, cz - 1, kpack, mold)
          if (ierr.eq.0) nh = dunp(p_cbgn, nunp)
       endif
       do jk = bes(1, cz), bes(2, cz) - 1
          if (ierr.eq.0) then
             call get_data_record_suspend(ierr, icom, mcom, u, krect, 0, rfil, nfil)
          endif
          if (ierr.eq.0) then
             call pack_restore_dunp(ierr, idec, icom, mh, nbits, kpack, dunp, nunp)
          endif
          jdb = nh * (jk - bes(1, cz))
          jde = jdb + nh
          where (idec(0:nh-1).ne.imiss)
             d(jdb:jde-1) = real(dma(2 * jk) + real(idec(0:nh-1), kind=KRSRC) * dma(2 * jk + 1), &
                  &              kind=KARG)
          elsewhere
             d(jdb:jde-1) = real(vmiss, kind=KARG)
          end where
       enddo
       if (ierr.eq.0) then
          call get_data_record_suspend(ierr, icom, 0, u, krect, -1)
       endif
    else
       if (ierr.eq.0) call get_data_record(ierr, icom, mcom * mk, u, krect)
       if (ierr.eq.0) then
          do jk = 0, mk - 1
             jc = jk * mcom
             call pack_restore(ierr, idec, icom(jc:), mh, nbits, kpack)
             jdb = mh * jk
             jde = mh * (jk + 1)
             where (idec(0:mh-1).ne.imiss)
                d(jdb:jde-1) = real(dma(2 * jk) + real(idec(0:mh-1), kind=KRSRC) * dma(2 * jk + 1), &
                     &              kind=KARG)
             elsewhere
                d(jdb:jde-1) = real(vmiss, kind=KARG)
             end where
          enddo
       endif
    endif
    return
  end subroutine get_data_ury_d
  subroutine get_data_ury_f &
       & (ierr, &
       &  d, nd, u, krect, vmiss, kfmt, mh, mk, bes, nr)
    use TOUZA_Trp,only: count_packed,  pack_restore
    use TOUZA_Trp,only: gen_bfc_slice, pack_restore_dunp
    use TOUZA_Trp,only: npropd, p_cbgn
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(KIND=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: nd
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(KIND=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt
    integer,         intent(in)  :: mh, mk
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr

    integer             :: nbits, mcom
    integer             :: idec(0:mh-1)
    integer(KIND=KISRC) :: icom(0:mh * mk - 1)
    integer(KIND=KISRC) :: imiss
    real(KIND=KRSRC)    :: dma(0:2 * mk - 1)
    integer,parameter   :: mold = 0_KISRC
    integer             :: jk, jdb, jde
    integer             :: jc
    integer,parameter :: cz = 3
    integer kpack
    integer :: dunp(npropd, 0:mh), rfil(0:mh)
    integer :: nunp,               nfil
    integer :: nh

    ierr = 0
    nbits = kfmt - GFMT_URY
    mcom  = count_packed(nbits, mh, mold)
    ! imiss = ISHFT(1, nbits) - 1
    imiss = IBITS(NOT(0), 0, nbits)
    kpack = legacy_unpacking(nbits, mh)

    call get_data_record(ierr, dma, 2 * mk, u, krect)
    if (present(bes)) then
       nh = 0
       if (.not.present(nr)) ierr = _ERROR(ERR_INVALID_ITEM)
       if (ierr.eq.0) then
          if (nd.ge.0.and.nd.lt.count_bes(bes, nr)) then
             ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
             return
          endif
       endif
       if (ierr.eq.0) then
          nfil = 0
          rfil(nfil) = mcom * bes(1, cz)
          call get_data_record_suspend(ierr, icom, 0, u, krect, +1, rfil, nfil)
       endif
       if (ierr.eq.0) then
          call gen_bfc_slice &
               & (ierr, dunp, nunp, rfil, nfil, nbits, mh, bes, cz - 1, kpack, mold)
          if (ierr.eq.0) nh = dunp(p_cbgn, nunp)
       endif
       do jk = bes(1, cz), bes(2, cz) - 1
          if (ierr.eq.0) then
             call get_data_record_suspend(ierr, icom, mcom, u, krect, 0, rfil, nfil)
          endif
          if (ierr.eq.0) then
             call pack_restore_dunp(ierr, idec, icom, mh, nbits, kpack, dunp, nunp)
          endif
          jdb = nh * (jk - bes(1, cz))
          jde = jdb + nh
          where (idec(0:nh-1).ne.imiss)
             d(jdb:jde-1) = real(dma(2 * jk) + real(idec(0:nh-1), kind=KRSRC) * dma(2 * jk + 1), &
                  &              kind=KARG)
          elsewhere
             d(jdb:jde-1) = real(vmiss, kind=KARG)
          end where
       enddo
       if (ierr.eq.0) then
          call get_data_record_suspend(ierr, icom, 0, u, krect, -1)
       endif
    else
       if (ierr.eq.0) call get_data_record(ierr, icom, mcom * mk, u, krect)
       if (ierr.eq.0) then
          do jk = 0, mk - 1
             jc = jk * mcom
             call pack_restore(ierr, idec, icom(jc:), mh, nbits, kpack)
             jdb = mh * jk
             jde = mh * (jk + 1)
             where (idec(0:mh-1).ne.imiss)
                d(jdb:jde-1) = real(dma(2 * jk) + real(idec(0:mh-1), kind=KRSRC) * dma(2 * jk + 1), &
                     &              kind=KARG)
             elsewhere
                d(jdb:jde-1) = real(vmiss, kind=KARG)
             end where
          enddo
       endif
    endif
    return
  end subroutine get_data_ury_f
  subroutine get_data_ury_i &
       & (ierr, &
       &  d, nd, u, krect, vmiss, kfmt, mh, mk, bes, nr)
    use TOUZA_Trp,only: count_packed,  pack_restore
    use TOUZA_Trp,only: gen_bfc_slice, pack_restore_dunp
    use TOUZA_Trp,only: npropd,        p_cbgn
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32, KRSRC=KDBL
    integer,           intent(out) :: ierr
    integer(KIND=KARG),intent(out) :: d(0:*)
    integer,           intent(in)  :: nd
    integer,           intent(in)  :: krect
    integer,           intent(in)  :: u
    real(KIND=KRMIS),  intent(in)  :: vmiss
    integer,           intent(in)  :: kfmt
    integer,           intent(in)  :: mh, mk
    integer,optional,  intent(in)  :: bes(3, *)
    integer,optional,  intent(in)  :: nr

    integer             :: nbits, mcom
    integer             :: idec(0:mh-1)
    integer(KIND=KISRC) :: icom(0:mh * mk - 1)
    integer(KIND=KISRC) :: imiss
    real(KIND=KRSRC)    :: dma(0:2 * mk - 1)
    integer,parameter   :: mold = 0_KISRC
    integer             :: jk, jdb, jde
    integer             :: jc
    integer,parameter :: cz = 3
    integer kpack
    integer :: dunp(npropd, 0:mh), rfil(0:mh)
    integer :: nunp,               nfil
    integer :: nh

    ierr = 0
    nbits = kfmt - GFMT_URY
    mcom  = count_packed(nbits, mh, mold)
    ! imiss = ISHFT(1, nbits) - 1
    imiss = IBITS(NOT(0), 0, nbits)
    kpack = legacy_unpacking(nbits, mh)

    call get_data_record(ierr, dma, 2 * mk, u, krect)
    if (present(bes)) then
       nh = 0
       if (.not.present(nr)) ierr = _ERROR(ERR_INVALID_ITEM)
       if (ierr.eq.0) then
          if (nd.ge.0.and.nd.lt.count_bes(bes, nr)) then
             ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
             return
          endif
       endif
       if (ierr.eq.0) then
          nfil = 0
          rfil(nfil) = mcom * bes(1, cz)
          call get_data_record_suspend(ierr, icom, 0, u, krect, +1, rfil, nfil)
       endif
       if (ierr.eq.0) then
          call gen_bfc_slice &
               & (ierr, dunp, nunp, rfil, nfil, nbits, mh, bes, cz - 1, kpack, mold)
          if (ierr.eq.0) nh = dunp(p_cbgn, nunp)
       endif
       do jk = bes(1, cz), bes(2, cz) - 1
          if (ierr.eq.0) then
             call get_data_record_suspend(ierr, icom, mcom, u, krect, 0, rfil, nfil)
          endif
          if (ierr.eq.0) then
             call pack_restore_dunp(ierr, idec, icom, mh, nbits, kpack, dunp, nunp)
          endif
          jdb = nh * (jk - bes(1, cz))
          jde = jdb + nh
          where (idec(0:nh-1).ne.imiss)
             d(jdb:jde-1) = int(dma(2 * jk) + real(idec(0:nh-1), kind=KRSRC) * dma(2 * jk + 1), &
                  &              kind=KARG)
          elsewhere
             d(jdb:jde-1) = int(vmiss, kind=KARG)
          end where
       enddo
       if (ierr.eq.0) then
          call get_data_record_suspend(ierr, icom, 0, u, krect, -1)
       endif
    else
       if (ierr.eq.0) call get_data_record(ierr, icom, mcom * mk, u, krect)
       if (ierr.eq.0) then
          do jk = 0, mk - 1
             jc = jk * mcom
             call pack_restore(ierr, idec, icom(jc:), mh, nbits, kpack)
             jdb = mh * jk
             jde = mh * (jk + 1)
             where (idec(0:mh-1).ne.imiss)
                d(jdb:jde-1) = int(dma(2 * jk) + real(idec(0:mh-1), kind=KRSRC) * dma(2 * jk + 1), &
                     &             kind=KARG)
             elsewhere
                d(jdb:jde-1) = int(vmiss, kind=KARG)
             end where
          enddo
       endif
    endif
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
    integer,parameter   :: mold = 0_KISRC
    integer             :: jk, jdb, jde, jc
    integer kpack

    ierr = 0
    nbits = kfmt - GFMT_URY
    mcom  = count_packed(nbits, nh, mold)
    ! notes on IBITS()
    ! ipc_IBITS() substitution is not necessary because nbits < bit_size().
    ! see TOUZA_Std_ipc.
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
    integer,parameter   :: mold = 0_KISRC
    integer             :: jk, jdb, jde, jc
    real(kind=KRSRC)    :: buf(nh)
    integer kpack

    ierr = 0
    nbits = kfmt - GFMT_URY
    mcom  = count_packed(nbits, nh, mold)
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
    integer(kind=KISRC),parameter :: mold = 0_KISRC
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
       &  d, nd, u, krect, vmiss, kfmt, mh, mk, bes, nr)
    use TOUZA_Trp,only: count_packed, pack_restore, pack_restore_dunp
    use TOUZA_Trp,only: mask_to_idxl, gen_bfc_idxl, npropd
    use TOUZA_Nio_std,only: KIOFS
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(KIND=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: nd
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(KIND=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt
    integer,         intent(in)  :: mh, mk
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr

    integer             :: nm
    integer             :: imsk(0:mh * mk)
    integer(KIND=KISRC) :: imco(0:mh * mk)

    integer             :: mch(0:mk-1)
    integer             :: mofs(0:mk)

    integer             :: nbits, mcom
    integer             :: idec(0:mh)
    integer(KIND=KISRC) :: icom(0:mh * mk)
    real(KIND=KRSRC)    :: dma(0:2 * mk)
    integer,parameter   :: mold = 0_KISRC
    integer             :: jk, jdb, jm, jc, jh, jb
    integer kpackm, kpackb
    integer,parameter :: cz = 3
    integer :: nidx
    integer :: idxs(0:mh), idxd(0:mh)
    integer :: dunp(npropd, 0:mh), rfil(0:mh)
    integer :: nunp,               nfil
    integer :: nk,  nh,  nmd
    integer :: jmp
    integer :: jprv

    ierr = 0

    nbits = kfmt - GFMT_MRY

    nm = count_packed(1, mh, mold)

    kpackm = legacy_unpacking(1,     mh)
    kpackb = legacy_unpacking(nbits, mh)
    nh = 0

    if (ierr.eq.0) call get_data_record(ierr, mcom,     u,          krect)
    if (ierr.eq.0) call get_data_record(ierr, mch,      mk,      u, krect)
    if (ierr.eq.0) call get_data_record(ierr, mofs(1:), mk,      u, krect)
    if (ierr.eq.0) call get_data_record(ierr, dma,      2 * mk,  u, krect)
    if (ierr.eq.0) then
       mofs(0) = 0
       do jk = 1, mk
          mofs(jk) = mofs(jk-1) + mofs(jk)
       enddo
    endif
    if (present(bes)) then
       if (.not.present(nr)) ierr = _ERROR(ERR_INVALID_ITEM)
       if (ierr.eq.0) then
          if (nd.ge.0.and.nd.lt.count_bes(bes, nr)) then
             ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
             return
          endif
       endif
       if (ierr.eq.0) then
          nfil = 2
          nk = bes(2, cz) - bes(1, cz)
          rfil(0) = nm * bes(1, cz)
          rfil(1) = nm * nk
          call get_data_record_runl(ierr, imco, nm * nk, u, krect, rfil, nfil)
       endif
       if (ierr.eq.0) then
          nfil = 0
          jk = bes(1, cz)
          rfil(nfil) = mofs(jk)
          call get_data_record_suspend(ierr, icom, 0, u, krect, +1, rfil, nfil)
       endif
       do jk = bes(1, cz), bes(2, cz) - 1
          nmd = mofs(jk+1) - mofs(jk)
          jmp = nm * (jk - bes(1, cz))
          if (ierr.eq.0) then
             call mask_to_idxl(ierr, idxd, idxs, nidx, imco(jmp:), mh, bes, cz - 1, kpackm)
          endif
          if (ierr.eq.0) then
             call gen_bfc_idxl(ierr, dunp, nunp, rfil, nfil, nbits, mch(jk), idxs, nidx, kpackb, mold)
          endif
          if (ierr.eq.0) then
             call get_data_record_suspend(ierr, icom, nmd, u, krect, 0, rfil, nfil)
          endif
          if (ierr.eq.0) then
             call pack_restore_dunp(ierr, idec, icom, nmd, nbits, kpackb, dunp, nunp)
          endif
          if (ierr.eq.0) then
             nh = count_bes(bes, 2)
             jdb = nh * (jk - bes(1, cz))
             jprv = 0
             do jc = 0, nidx - 1
                jh = idxd(jc)
                d(jdb+jprv:jdb+jh-1) = real(vmiss, kind=KARG)
                d(jdb+jh) = &
                     & real(dma(2 * jk) + real(idec(jc), kind=KRSRC) * dma(2 * jk + 1), &
                     &      kind=KARG)
                jprv = jh + 1
             enddo
             d(jdb+jprv:jdb+nh-1) = real(vmiss, kind=KARG)
          endif
       enddo
       if (ierr.eq.0) then
          call get_data_record_suspend(ierr, icom, 0, u, krect, -1)
       endif
    else
       if (ierr.eq.0) call get_data_record(ierr, imco,     nm * mk, u, krect)
       if (ierr.eq.0) call get_data_record(ierr, icom,     mcom,    u, krect)

       if (ierr.eq.0) then
          do jk = 0, mk - 1
             jm = jk * nm
             jc = mofs(jk)
             call pack_restore(ierr, imsk, imco(jm:), mh, 1, kpackm)
             call pack_restore(ierr, idec, icom(jc:), mch(jk), nbits, kpackb)
             jb = 0
             jdb = jk * mh
             do jh = 0, mh - 1
                if (imsk(jh).eq.1) then
                   d(jdb + jh) = &
                        & real(dma(2 * jk) + real(idec(jb), kind=KRSRC) * dma(2 * jk + 1), &
                        &      kind=KARG)
                   jb = jb + 1
                else
                   d(jdb + jh) = real(vmiss, kind=KARG)
                endif
             enddo
          enddo
       endif
    endif
    return
  end subroutine get_data_mry_d
  subroutine get_data_mry_f &
       & (ierr, &
       &  d, nd, u, krect, vmiss, kfmt, mh, mk, bes, nr)
    use TOUZA_Trp,only: count_packed, pack_restore, pack_restore_dunp
    use TOUZA_Trp,only: mask_to_idxl, gen_bfc_idxl, npropd
    use TOUZA_Nio_std,only: KIOFS
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(KIND=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: nd
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(KIND=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt
    integer,         intent(in)  :: mh, mk
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr

    integer             :: nm
    integer             :: imsk(0:mh * mk)
    integer(KIND=KISRC) :: imco(0:mh * mk)

    integer             :: mch(0:mk-1)
    integer             :: mofs(0:mk)

    integer             :: nbits, mcom
    integer             :: idec(0:mh)
    integer(KIND=KISRC) :: icom(0:mh * mk)
    real(KIND=KRSRC)    :: dma(0:2 * mk)
    integer,parameter   :: mold = 0_KISRC
    integer             :: jk, jdb, jm, jc, jh, jb
    integer kpackm, kpackb
    integer,parameter :: cz = 3
    integer :: nidx
    integer :: idxs(0:mh), idxd(0:mh)
    integer :: dunp(npropd, 0:mh), rfil(0:mh)
    integer :: nunp,               nfil
    integer :: nk,  nh,  nmd
    integer :: jmp
    integer :: jprv

    ierr = 0

    nbits = kfmt - GFMT_MRY

    nm = count_packed(1, mh, mold)

    kpackm = legacy_unpacking(1,     mh)
    kpackb = legacy_unpacking(nbits, mh)
    nh = 0

    if (ierr.eq.0) call get_data_record(ierr, mcom,     u,          krect)
    if (ierr.eq.0) call get_data_record(ierr, mch,      mk,      u, krect)
    if (ierr.eq.0) call get_data_record(ierr, mofs(1:), mk,      u, krect)
    if (ierr.eq.0) call get_data_record(ierr, dma,      2 * mk,  u, krect)
    if (ierr.eq.0) then
       mofs(0) = 0
       do jk = 1, mk
          mofs(jk) = mofs(jk-1) + mofs(jk)
       enddo
    endif
    if (present(bes)) then
       if (.not.present(nr)) ierr = _ERROR(ERR_INVALID_ITEM)
       if (ierr.eq.0) then
          if (nd.ge.0.and.nd.lt.count_bes(bes, nr)) then
             ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
             return
          endif
       endif
       if (ierr.eq.0) then
          nfil = 2
          nk = bes(2, cz) - bes(1, cz)
          rfil(0) = nm * bes(1, cz)
          rfil(1) = nm * nk
          call get_data_record_runl(ierr, imco, nm * nk, u, krect, rfil, nfil)
       endif
       if (ierr.eq.0) then
          nfil = 0
          jk = bes(1, cz)
          rfil(nfil) = mofs(jk)
          call get_data_record_suspend(ierr, icom, 0, u, krect, +1, rfil, nfil)
       endif
       do jk = bes(1, cz), bes(2, cz) - 1
          nmd = mofs(jk+1) - mofs(jk)
          jmp = nm * (jk - bes(1, cz))
          if (ierr.eq.0) then
             call mask_to_idxl(ierr, idxd, idxs, nidx, imco(jmp:), mh, bes, cz - 1, kpackm)
          endif
          if (ierr.eq.0) then
             call gen_bfc_idxl(ierr, dunp, nunp, rfil, nfil, nbits, mch(jk), idxs, nidx, kpackb, mold)
          endif
          if (ierr.eq.0) then
             call get_data_record_suspend(ierr, icom, nmd, u, krect, 0, rfil, nfil)
          endif
          if (ierr.eq.0) then
             call pack_restore_dunp(ierr, idec, icom, nmd, nbits, kpackb, dunp, nunp)
          endif
          if (ierr.eq.0) then
             nh = count_bes(bes, 2)
             jdb = nh * (jk - bes(1, cz))
             jprv = 0
             do jc = 0, nidx - 1
                jh = idxd(jc)
                d(jdb+jprv:jdb+jh-1) = real(vmiss, kind=KARG)
                d(jdb+jh) = &
                     & real(dma(2 * jk) + real(idec(jc), kind=KRSRC) * dma(2 * jk + 1), &
                     &      kind=KARG)
                jprv = jh + 1
             enddo
             d(jdb+jprv:jdb+nh-1) = real(vmiss, kind=KARG)
          endif
       enddo
       if (ierr.eq.0) then
          call get_data_record_suspend(ierr, icom, 0, u, krect, -1)
       endif
    else
       if (ierr.eq.0) call get_data_record(ierr, imco,     nm * mk, u, krect)
       if (ierr.eq.0) call get_data_record(ierr, icom,     mcom,    u, krect)

       if (ierr.eq.0) then
          do jk = 0, mk - 1
             jm = jk * nm
             jc = mofs(jk)
             call pack_restore(ierr, imsk, imco(jm:), mh, 1, kpackm)
             call pack_restore(ierr, idec, icom(jc:), mch(jk), nbits, kpackb)
             jb = 0
             jdb = jk * mh
             do jh = 0, mh - 1
                if (imsk(jh).eq.1) then
                   d(jdb + jh) = &
                        & real(dma(2 * jk) + real(idec(jb), kind=KRSRC) * dma(2 * jk + 1), &
                        &      kind=KARG)
                   jb = jb + 1
                else
                   d(jdb + jh) = real(vmiss, kind=KARG)
                endif
             enddo
          enddo
       endif
    endif
    return
  end subroutine get_data_mry_f
  subroutine get_data_mry_i &
       & (ierr, &
       &  d, nd, u, krect, vmiss, kfmt, mh, mk, bes, nr)
    use TOUZA_Trp,only: count_packed, pack_restore, pack_restore_dunp
    use TOUZA_Trp,only: mask_to_idxl, gen_bfc_idxl, npropd
    use TOUZA_Nio_std,only: KIOFS
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32, KRSRC=KDBL
    integer,           intent(out) :: ierr
    integer(KIND=KARG),intent(out) :: d(0:*)
    integer,           intent(in)  :: nd
    integer,           intent(in)  :: krect
    integer,           intent(in)  :: u
    real(KIND=KRMIS),  intent(in)  :: vmiss
    integer,           intent(in)  :: kfmt
    integer,           intent(in)  :: mh, mk
    integer,optional,  intent(in)  :: bes(3, *)
    integer,optional,  intent(in)  :: nr

    integer             :: nm
    integer             :: imsk(0:mh * mk)
    integer(KIND=KISRC) :: imco(0:mh * mk)

    integer             :: mch(0:mk-1)
    integer             :: mofs(0:mk)

    integer             :: nbits, mcom
    integer             :: idec(0:mh)
    integer(KIND=KISRC) :: icom(0:mh * mk)
    real(KIND=KRSRC)    :: dma(0:2 * mk)
    integer,parameter   :: mold = 0_KISRC
    integer             :: jk, jdb, jm, jc, jh, jb
    integer kpackm, kpackb
    integer,parameter :: cz = 3
    integer :: nidx
    integer :: idxs(0:mh), idxd(0:mh)
    integer :: dunp(npropd, 0:mh), rfil(0:mh)
    integer :: nunp,               nfil
    integer :: nk,  nh,  nmd
    integer :: jmp
    integer :: jprv

    ierr = 0

    nbits = kfmt - GFMT_MRY

    nm = count_packed(1, mh, mold)

    kpackm = legacy_unpacking(1,     mh)
    kpackb = legacy_unpacking(nbits, mh)
    nh = 0

    if (ierr.eq.0) call get_data_record(ierr, mcom,     u,          krect)
    if (ierr.eq.0) call get_data_record(ierr, mch,      mk,      u, krect)
    if (ierr.eq.0) call get_data_record(ierr, mofs(1:), mk,      u, krect)
    if (ierr.eq.0) call get_data_record(ierr, dma,      2 * mk,  u, krect)
    if (ierr.eq.0) then
       mofs(0) = 0
       do jk = 1, mk
          mofs(jk) = mofs(jk-1) + mofs(jk)
       enddo
    endif
    if (present(bes)) then
       if (.not.present(nr)) ierr = _ERROR(ERR_INVALID_ITEM)
       if (ierr.eq.0) then
          if (nd.ge.0.and.nd.lt.count_bes(bes, nr)) then
             ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
             return
          endif
       endif
       if (ierr.eq.0) then
          nfil = 2
          nk = bes(2, cz) - bes(1, cz)
          rfil(0) = nm * bes(1, cz)
          rfil(1) = nm * nk
          call get_data_record_runl(ierr, imco, nm * nk, u, krect, rfil, nfil)
       endif
       if (ierr.eq.0) then
          nfil = 0
          jk = bes(1, cz)
          rfil(nfil) = mofs(jk)
          call get_data_record_suspend(ierr, icom, 0, u, krect, +1, rfil, nfil)
       endif
       do jk = bes(1, cz), bes(2, cz) - 1
          nmd = mofs(jk+1) - mofs(jk)
          jmp = nm * (jk - bes(1, cz))
          if (ierr.eq.0) then
             call mask_to_idxl(ierr, idxd, idxs, nidx, imco(jmp:), mh, bes, cz - 1, kpackm)
          endif
          if (ierr.eq.0) then
             call gen_bfc_idxl(ierr, dunp, nunp, rfil, nfil, nbits, mch(jk), idxs, nidx, kpackb, mold)
          endif
          if (ierr.eq.0) then
             call get_data_record_suspend(ierr, icom, nmd, u, krect, 0, rfil, nfil)
          endif
          if (ierr.eq.0) then
             call pack_restore_dunp(ierr, idec, icom, nmd, nbits, kpackb, dunp, nunp)
          endif
          if (ierr.eq.0) then
             nh = count_bes(bes, 2)
             jdb = nh * (jk - bes(1, cz))
             jprv = 0
             do jc = 0, nidx - 1
                jh = idxd(jc)
                d(jdb+jprv:jdb+jh-1) = int(vmiss, kind=KARG)
                d(jdb+jh) = &
                     & int(dma(2 * jk) + real(idec(jc), kind=KRSRC) * dma(2 * jk + 1), &
                     &      kind=KARG)
                jprv = jh + 1
             enddo
             d(jdb+jprv:jdb+nh-1) = int(vmiss, kind=KARG)
          endif
       enddo
       if (ierr.eq.0) then
          call get_data_record_suspend(ierr, icom, 0, u, krect, -1)
       endif
    else
       if (ierr.eq.0) call get_data_record(ierr, imco,     nm * mk, u, krect)
       if (ierr.eq.0) call get_data_record(ierr, icom,     mcom,    u, krect)

       if (ierr.eq.0) then
          do jk = 0, mk - 1
             jm = jk * nm
             jc = mofs(jk)
             call pack_restore(ierr, imsk, imco(jm:), mh, 1, kpackm)
             call pack_restore(ierr, idec, icom(jc:), mch(jk), nbits, kpackb)
             jb = 0
             jdb = jk * mh
             do jh = 0, mh - 1
                if (imsk(jh).eq.1) then
                   d(jdb + jh) = &
                        & int(dma(2 * jk) + real(idec(jb), kind=KRSRC) * dma(2 * jk + 1), &
                        &     kind=KARG)
                   jb = jb + 1
                else
                   d(jdb + jh) = int(vmiss, kind=KARG)
                endif
             enddo
          enddo
       endif
    endif
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
    integer,parameter   :: mold = 0_KISRC
    integer             :: jk, jdb, jm, jc, jh
    integer             :: nc, ne
    integer kpackm, kpackb

    ierr = 0

    nbits = kfmt - GFMT_MRY
    nm = count_packed(1, nh, mold)
    imiss = IBITS(HUGE(0_KISRC), 0, nbits)

    jc = 0
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
       nc = count_packed(nbits, ne, mold)
       mofs(jk) = nc
       mch(jk)  = ne

       call normalize_xry &
            & (ierr, idec, dma(2*jk-1:2*jk), buf(1:), ne, imiss, vmiss)
       jm = (jk - 1) * nm + 1
       call pack_store(ierr, imco(jm:), imsk, nh, 1, kpackm)
       call pack_store(ierr, icom(jc+1:), idec, ne, nbits, kpackb)
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

!!!_ + M[RI]n core
!!!_  - review_mtn - review M[RI]n data
  subroutine review_mtn &
       & (ierr,  nmask, ndata, &
       &  head,  u,     krect, flag)
    use TOUZA_Nio_std,only: KIOFS, sus_getpos, sus_rseek, WHENCE_ABS
    use TOUZA_Nio_std,only: choice
    use TOUZA_Trp,    only: count_packed
    implicit none
    integer,parameter :: KISRC=KI32
    integer,         intent(out) :: ierr
    integer,         intent(out) :: nmask       ! (physical) size of mask part
    integer,         intent(out) :: ndata       ! (physical) size of data part
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    integer,optional,intent(in)  :: flag

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer(kind=KIOFS) :: apini
    integer mfull  ! logical full-size

    ierr = 0

    ! note: position must be just after gtool header part

    ! Typical usage:
    !   call review_mtn(..., nmask=nmask, ndata)
    !   allocate cmask(nmask)
    !   allocate subv(ndata), d(ndata)
    !   call restore_mr8_packed_d(...)
    f = choice(-1, flag)
    if (f.lt.0) f = rev_pos_dhead

    if (ierr.eq.0) call pre_review(ierr, apini, u, f)
    ! packed data array size
    if (ierr.eq.0) call get_data_record(ierr, ndata, u, krect)
    if (ierr.eq.0) then
       mfull = parse_header_size(head, 0, 1)
       nmask = count_packed(1, mfull, mold)
    endif
    if (ierr.eq.0) call post_review(ierr, apini, u, f)
  end subroutine review_mtn

!!!_  - restore_mr8_plain_d - restore MR8 data to plain (full) array
  subroutine restore_mr8_plain_d &
       & (ierr, &
       &  d,    ldata, u, krect, vmiss, mfull, bes, nr)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: ldata        ! limit size of of d
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: mfull        ! (logical) full data size
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack
    integer ndata, mdata, nsub

    ! note: position must be just after gtool header part
    ierr = 0
    kpack = legacy_unpacking(1, mfull)

    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mdata, u, krect)

    ! mask array
    ncom = count_packed(1, mfull, mold)
    if (ierr.eq.0) call alloc_wmask(ierr, ncom, 'res:mr8d', mold)
    if (ierr.eq.0) call get_data_record(ierr, wmask, ncom, u, krect)

    if (present(bes)) then
       if (.not.present(nr)) ierr = _ERROR(ERR_FEW_ARGUMENTS)
       if (ierr.eq.0) then
          ! ndata: maximum possible
          ndata = count_bes(bes, nr)
          if (ldata.ge.0.and.ldata.lt.ndata) then
             ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
             return
          endif
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, ndata, 'res:mr8d')
       if (ierr.eq.0) call alloc_workd(ierr, ndata, 'res:mr8d')
       if (ierr.eq.0) then
          call mask_to_idxl(ierr, wdsubv, wssubv, nsub, wmask, mdata, bes, nr, kpack)
       endif
       if (ierr.eq.0) then
          call get_data_record_list(ierr, workd, ndata, u, krect, mdata, wssubv, nsub)
       endif
       if (ierr.eq.0) then
          call subv_decode(ierr, d, ndata, workd, wdsubv, nsub, vmiss)
       endif
       if (ierr.eq.0) call alloc_workd(ierr, -1, 'res:mr8d')
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:mr8d')
    else
       if (ldata.ge.0.and.ldata.lt.mfull) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          return
       endif
       if (ierr.eq.0) call alloc_workd(ierr, mdata, 'res:mr8d')
       if (ierr.eq.0) call get_data_record(ierr, workd, mdata, u, krect)
       if (ierr.eq.0) then
          call mask_decode(ierr, d, mfull, workd, wmask, vmiss, kpack)
       endif
       if (ierr.eq.0) call alloc_workd(ierr, -1, 'res:mr8d')
    endif
    if (ierr.eq.0) call alloc_wmask(ierr, -1, 'res:mr8d', mold)
  end subroutine restore_mr8_plain_d
  subroutine restore_mr8_plain_f &
       & (ierr, &
       &  d,    ldata, u, krect, vmiss, mfull, bes, nr)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: ldata        ! limit size of of d
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: mfull        ! (logical) full data size
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack
    integer ndata, mdata, nsub

    ! note: position must be just after gtool header part
    ierr = 0
    kpack = legacy_unpacking(1, mfull)

    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mdata, u, krect)

    ! mask array
    ncom = count_packed(1, mfull, mold)
    if (ierr.eq.0) call alloc_wmask(ierr, ncom, 'res:mr8f', mold)
    if (ierr.eq.0) call get_data_record(ierr, wmask, ncom, u, krect)

    if (present(bes)) then
       if (.not.present(nr)) ierr = _ERROR(ERR_FEW_ARGUMENTS)
       if (ierr.eq.0) then
          ! ndata: maximum possible
          ndata = count_bes(bes, nr)
          if (ldata.ge.0.and.ldata.lt.ndata) then
             ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
             return
          endif
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, ndata, 'res:mr8f')
       if (ierr.eq.0) call alloc_workd(ierr, ndata, 'res:mr8f')
       if (ierr.eq.0) then
          call mask_to_idxl(ierr, wdsubv, wssubv, nsub, wmask, mdata, bes, nr, kpack)
       endif
       if (ierr.eq.0) then
          call get_data_record_list(ierr, workd, ndata, u, krect, mdata, wssubv, nsub)
       endif
       if (ierr.eq.0) then
          call subv_decode(ierr, d, ndata, workd, wdsubv, nsub, vmiss)
       endif
       if (ierr.eq.0) call alloc_workd(ierr, -1, 'res:mr8f')
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:mr8f')
    else
       if (ldata.ge.0.and.ldata.lt.mfull) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          return
       endif
       if (ierr.eq.0) call alloc_workd(ierr, mdata, 'res:mr8f')
       if (ierr.eq.0) call get_data_record(ierr, workd, mdata, u, krect)
       if (ierr.eq.0) then
          call mask_decode(ierr, d, mfull, workd, wmask, vmiss, kpack)
       endif
       if (ierr.eq.0) call alloc_workd(ierr, -1, 'res:mr8f')
    endif
    if (ierr.eq.0) call alloc_wmask(ierr, -1, 'res:mr8f', mold)
  end subroutine restore_mr8_plain_f
  subroutine restore_mr8_plain_i &
       & (ierr, &
       &  d,    ldata, u, krect, vmiss, mfull, bes, nr)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32, KRSRC=KDBL
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(out) :: d(0:*)
    integer,           intent(in)  :: ldata        ! limit size of of d
    integer,           intent(in)  :: u
    integer,           intent(in)  :: krect
    real(kind=KRMIS),  intent(in)  :: vmiss
    integer,           intent(in)  :: mfull        ! (logical) full data size
    integer,optional,  intent(in)  :: bes(3, *)
    integer,optional,  intent(in)  :: nr

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack
    integer ndata, mdata, nsub

    ! note: position must be just after gtool header part
    ierr = 0
    kpack = legacy_unpacking(1, mfull)

    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mdata, u, krect)

    ! mask array
    ncom = count_packed(1, mfull, mold)
    if (ierr.eq.0) call alloc_wmask(ierr, ncom, 'res:mr8i', mold)
    if (ierr.eq.0) call get_data_record(ierr, wmask, ncom, u, krect)

    if (present(bes)) then
       if (.not.present(nr)) ierr = _ERROR(ERR_FEW_ARGUMENTS)
       if (ierr.eq.0) then
          ! ndata: maximum possible
          ndata = count_bes(bes, nr)
          if (ldata.ge.0.and.ldata.lt.ndata) then
             ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
             return
          endif
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, ndata, 'res:mr8i')
       if (ierr.eq.0) call alloc_workd(ierr, ndata, 'res:mr8i')
       if (ierr.eq.0) then
          call mask_to_idxl(ierr, wdsubv, wssubv, nsub, wmask, mdata, bes, nr, kpack)
       endif
       if (ierr.eq.0) then
          call get_data_record_list(ierr, workd, ndata, u, krect, mdata, wssubv, nsub)
       endif
       if (ierr.eq.0) then
          call subv_decode(ierr, d, ndata, workd, wdsubv, nsub, vmiss)
       endif
       if (ierr.eq.0) call alloc_workd(ierr, -1, 'res:mr8i')
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:mr8i')
    else
       if (ldata.ge.0.and.ldata.lt.mfull) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          return
       endif
       if (ierr.eq.0) call alloc_workd(ierr, mdata, 'res:mr8i')
       if (ierr.eq.0) call get_data_record(ierr, workd, mdata, u, krect)
       if (ierr.eq.0) then
          call mask_decode(ierr, d, mfull, workd, wmask, vmiss, kpack)
       endif
       if (ierr.eq.0) call alloc_workd(ierr, -1, 'res:mr8i')
    endif
    if (ierr.eq.0) call alloc_wmask(ierr, -1, 'res:mr8i', mold)
  end subroutine restore_mr8_plain_i

!!!_  - restore_mr4_plain_f - restore MR4 data to plain (full) array
  subroutine restore_mr4_plain_f &
       & (ierr, &
       &  d,    ldata, u, krect, vmiss, mfull, bes, nr)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: ldata        ! limit size of of d
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: mfull        ! (logical) full data size
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack
    integer ndata, mdata, nsub

    ! note: position must be just after gtool header part
    ierr = 0
    kpack = legacy_unpacking(1, mfull)

    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mdata, u, krect)

    ! mask array
    ncom = count_packed(1, mfull, mold)
    if (ierr.eq.0) call alloc_wmask(ierr, ncom, 'res:mr4f', mold)
    if (ierr.eq.0) call get_data_record(ierr, wmask, ncom, u, krect)

    if (present(bes)) then
       if (.not.present(nr)) ierr = _ERROR(ERR_FEW_ARGUMENTS)
       if (ierr.eq.0) then
          ! ndata: maximum possible
          ndata = count_bes(bes, nr)
          if (ldata.ge.0.and.ldata.lt.ndata) then
             ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
             return
          endif
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, ndata, 'res:mr4f')
       if (ierr.eq.0) call alloc_workf(ierr, ndata, 'res:mr4f')
       if (ierr.eq.0) then
          call mask_to_idxl(ierr, wdsubv, wssubv, nsub, wmask, mdata, bes, nr, kpack)
       endif
       if (ierr.eq.0) then
          call get_data_record_list(ierr, workf, ndata, u, krect, mdata, wssubv, nsub)
       endif
       if (ierr.eq.0) then
          call subv_decode(ierr, d, ndata, workf, wdsubv, nsub, vmiss)
       endif
       if (ierr.eq.0) call alloc_workf(ierr, -1, 'res:mr4f')
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:mr4f')
    else
       if (ldata.ge.0.and.ldata.lt.mfull) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          return
       endif
       if (ierr.eq.0) call alloc_workf(ierr, mdata, 'res:mr4f')
       if (ierr.eq.0) call get_data_record(ierr, workf, mdata, u, krect)
       if (ierr.eq.0) then
          call mask_decode(ierr, d, mfull, workf, wmask, vmiss, kpack)
       endif
       if (ierr.eq.0) call alloc_workf(ierr, -1, 'res:mr4f')
    endif
    if (ierr.eq.0) call alloc_wmask(ierr, -1, 'res:mr4f', mold)
  end subroutine restore_mr4_plain_f
  subroutine restore_mr4_plain_d &
       & (ierr, &
       &  d,    ldata, u, krect, vmiss, mfull, bes, nr)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: ldata        ! limit size of of d
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: mfull        ! (logical) full data size
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack
    integer ndata, mdata, nsub

    ! note: position must be just after gtool header part
    ierr = 0
    kpack = legacy_unpacking(1, mfull)

    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mdata, u, krect)

    ! mask array
    ncom = count_packed(1, mfull, mold)
    if (ierr.eq.0) call alloc_wmask(ierr, ncom, 'res:mr4d', mold)
    if (ierr.eq.0) call get_data_record(ierr, wmask, ncom, u, krect)

    if (present(bes)) then
       if (.not.present(nr)) ierr = _ERROR(ERR_FEW_ARGUMENTS)
       if (ierr.eq.0) then
          ! ndata: maximum possible
          ndata = count_bes(bes, nr)
          if (ldata.ge.0.and.ldata.lt.ndata) then
             ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
             return
          endif
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, ndata, 'res:mr4d')
       if (ierr.eq.0) call alloc_workf(ierr, ndata, 'res:mr4d')
       if (ierr.eq.0) then
          call mask_to_idxl(ierr, wdsubv, wssubv, nsub, wmask, mdata, bes, nr, kpack)
       endif
       if (ierr.eq.0) then
          call get_data_record_list(ierr, workf, ndata, u, krect, mdata, wssubv, nsub)
       endif
       if (ierr.eq.0) then
          call subv_decode(ierr, d, ndata, workf, wdsubv, nsub, vmiss)
       endif
       if (ierr.eq.0) call alloc_workf(ierr, -1, 'res:mr4d')
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:mr4d')
    else
       if (ldata.ge.0.and.ldata.lt.mfull) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          return
       endif
       if (ierr.eq.0) call alloc_workf(ierr, mdata, 'res:mr4d')
       if (ierr.eq.0) call get_data_record(ierr, workf, mdata, u, krect)
       if (ierr.eq.0) then
          call mask_decode(ierr, d, mfull, workf, wmask, vmiss, kpack)
       endif
       if (ierr.eq.0) call alloc_workf(ierr, -1, 'res:mr4d')
    endif
    if (ierr.eq.0) call alloc_wmask(ierr, -1, 'res:mr4d', mold)
  end subroutine restore_mr4_plain_d
  subroutine restore_mr4_plain_i &
       & (ierr, &
       &  d,    ldata, u, krect, vmiss, mfull, bes, nr)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32, KRSRC=KFLT
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(out) :: d(0:*)
    integer,           intent(in)  :: ldata        ! limit size of of d
    integer,           intent(in)  :: u
    integer,           intent(in)  :: krect
    real(kind=KRMIS),  intent(in)  :: vmiss
    integer,           intent(in)  :: mfull        ! (logical) full data size
    integer,optional,  intent(in)  :: bes(3, *)
    integer,optional,  intent(in)  :: nr

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack
    integer ndata, mdata, nsub

    ! note: position must be just after gtool header part
    ierr = 0
    kpack = legacy_unpacking(1, mfull)

    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mdata, u, krect)

    ! mask array
    ncom = count_packed(1, mfull, mold)
    if (ierr.eq.0) call alloc_wmask(ierr, ncom, 'res:mr4i', mold)
    if (ierr.eq.0) call get_data_record(ierr, wmask, ncom, u, krect)

    if (present(bes)) then
       if (.not.present(nr)) ierr = _ERROR(ERR_FEW_ARGUMENTS)
       if (ierr.eq.0) then
          ! ndata: maximum possible
          ndata = count_bes(bes, nr)
          if (ldata.ge.0.and.ldata.lt.ndata) then
             ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
             return
          endif
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, ndata, 'res:mr4i')
       if (ierr.eq.0) call alloc_workf(ierr, ndata, 'res:mr4i')
       if (ierr.eq.0) then
          call mask_to_idxl(ierr, wdsubv, wssubv, nsub, wmask, mdata, bes, nr, kpack)
       endif
       if (ierr.eq.0) then
          call get_data_record_list(ierr, workf, ndata, u, krect, mdata, wssubv, nsub)
       endif
       if (ierr.eq.0) then
          call subv_decode(ierr, d, ndata, workf, wdsubv, nsub, vmiss)
       endif
       if (ierr.eq.0) call alloc_workf(ierr, -1, 'res:mr4i')
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:mr4i')
    else
       if (ldata.ge.0.and.ldata.lt.mfull) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          return
       endif
       if (ierr.eq.0) call alloc_workf(ierr, mdata, 'res:mr4i')
       if (ierr.eq.0) call get_data_record(ierr, workf, mdata, u, krect)
       if (ierr.eq.0) then
          call mask_decode(ierr, d, mfull, workf, wmask, vmiss, kpack)
       endif
       if (ierr.eq.0) call alloc_workf(ierr, -1, 'res:mr4i')
    endif
    if (ierr.eq.0) call alloc_wmask(ierr, -1, 'res:mr4i', mold)
  end subroutine restore_mr4_plain_i

!!!_  - restore_mi4_plain_i - restore MI4 data to plain (full) array
  subroutine restore_mi4_plain_i &
       & (ierr, &
       &  d,    ldata, u, krect, vmiss, mfull, bes, nr)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(out) :: d(0:*)
    integer,           intent(in)  :: ldata        ! limit size of of d
    integer,           intent(in)  :: u
    integer,           intent(in)  :: krect
    real(kind=KRMIS),  intent(in)  :: vmiss
    integer,           intent(in)  :: mfull        ! (logical) full data size
    integer,optional,  intent(in)  :: bes(3, *)
    integer,optional,  intent(in)  :: nr

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack
    integer ndata, mdata, nsub

    ! note: position must be just after gtool header part
    ierr = 0
    kpack = legacy_unpacking(1, mfull)

    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mdata, u, krect)

    ! mask array
    ncom = count_packed(1, mfull, mold)
    if (ierr.eq.0) call alloc_wmask(ierr, ncom, 'res:mi4i', mold)
    if (ierr.eq.0) call get_data_record(ierr, wmask, ncom, u, krect)

    if (present(bes)) then
       if (.not.present(nr)) ierr = _ERROR(ERR_FEW_ARGUMENTS)
       if (ierr.eq.0) then
          ! ndata: maximum possible
          ndata = count_bes(bes, nr)
          if (ldata.ge.0.and.ldata.lt.ndata) then
             ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
             return
          endif
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, ndata, 'res:mi4i')
       if (ierr.eq.0) call alloc_worki(ierr, ndata, 'res:mi4i')
       if (ierr.eq.0) then
          call mask_to_idxl(ierr, wdsubv, wssubv, nsub, wmask, mdata, bes, nr, kpack)
       endif
       if (ierr.eq.0) then
          call get_data_record_list(ierr, worki, ndata, u, krect, mdata, wssubv, nsub)
       endif
       if (ierr.eq.0) then
          call subv_decode(ierr, d, ndata, worki, wdsubv, nsub, vmiss)
       endif
       if (ierr.eq.0) call alloc_worki(ierr, -1, 'res:mi4i')
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:mi4i')
    else
       if (ldata.ge.0.and.ldata.lt.mfull) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          return
       endif
       if (ierr.eq.0) call alloc_worki(ierr, mdata, 'res:mi4i')
       if (ierr.eq.0) call get_data_record(ierr, worki, mdata, u, krect)
       if (ierr.eq.0) then
          call mask_decode(ierr, d, mfull, worki, wmask, vmiss, kpack)
       endif
       if (ierr.eq.0) call alloc_worki(ierr, -1, 'res:mi4i')
    endif
    if (ierr.eq.0) call alloc_wmask(ierr, -1, 'res:mi4i', mold)
  end subroutine restore_mi4_plain_i
  subroutine restore_mi4_plain_d &
       & (ierr, &
       &  d,    ldata, u, krect, vmiss, mfull, bes, nr)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: ldata        ! limit size of of d
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: mfull        ! (logical) full data size
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack
    integer ndata, mdata, nsub

    ! note: position must be just after gtool header part
    ierr = 0
    kpack = legacy_unpacking(1, mfull)

    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mdata, u, krect)

    ! mask array
    ncom = count_packed(1, mfull, mold)
    if (ierr.eq.0) call alloc_wmask(ierr, ncom, 'res:mi4d', mold)
    if (ierr.eq.0) call get_data_record(ierr, wmask, ncom, u, krect)

    if (present(bes)) then
       if (.not.present(nr)) ierr = _ERROR(ERR_FEW_ARGUMENTS)
       if (ierr.eq.0) then
          ! ndata: maximum possible
          ndata = count_bes(bes, nr)
          if (ldata.ge.0.and.ldata.lt.ndata) then
             ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
             return
          endif
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, ndata, 'res:mi4d')
       if (ierr.eq.0) call alloc_worki(ierr, ndata, 'res:mi4d')
       if (ierr.eq.0) then
          call mask_to_idxl(ierr, wdsubv, wssubv, nsub, wmask, mdata, bes, nr, kpack)
       endif
       if (ierr.eq.0) then
          call get_data_record_list(ierr, worki, ndata, u, krect, mdata, wssubv, nsub)
       endif
       if (ierr.eq.0) then
          call subv_decode(ierr, d, ndata, worki, wdsubv, nsub, vmiss)
       endif
       if (ierr.eq.0) call alloc_worki(ierr, -1, 'res:mi4d')
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:mi4d')
    else
       if (ldata.ge.0.and.ldata.lt.mfull) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          return
       endif
       if (ierr.eq.0) call alloc_worki(ierr, mdata, 'res:mi4d')
       if (ierr.eq.0) call get_data_record(ierr, worki, mdata, u, krect)
       if (ierr.eq.0) then
          call mask_decode(ierr, d, mfull, worki, wmask, vmiss, kpack)
       endif
       if (ierr.eq.0) call alloc_worki(ierr, -1, 'res:mi4d')
    endif
    if (ierr.eq.0) call alloc_wmask(ierr, -1, 'res:mi4d', mold)
  end subroutine restore_mi4_plain_d
  subroutine restore_mi4_plain_f &
       & (ierr, &
       &  d,    ldata, u, krect, vmiss, mfull, bes, nr)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: ldata        ! limit size of of d
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: mfull        ! (logical) full data size
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack
    integer ndata, mdata, nsub

    ! note: position must be just after gtool header part
    ierr = 0
    kpack = legacy_unpacking(1, mfull)

    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mdata, u, krect)

    ! mask array
    ncom = count_packed(1, mfull, mold)
    if (ierr.eq.0) call alloc_wmask(ierr, ncom, 'res:mi4f', mold)
    if (ierr.eq.0) call get_data_record(ierr, wmask, ncom, u, krect)

    if (present(bes)) then
       if (.not.present(nr)) ierr = _ERROR(ERR_FEW_ARGUMENTS)
       if (ierr.eq.0) then
          ! ndata: maximum possible
          ndata = count_bes(bes, nr)
          if (ldata.ge.0.and.ldata.lt.ndata) then
             ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
             return
          endif
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, ndata, 'res:mi4f')
       if (ierr.eq.0) call alloc_worki(ierr, ndata, 'res:mi4f')
       if (ierr.eq.0) then
          call mask_to_idxl(ierr, wdsubv, wssubv, nsub, wmask, mdata, bes, nr, kpack)
       endif
       if (ierr.eq.0) then
          call get_data_record_list(ierr, worki, ndata, u, krect, mdata, wssubv, nsub)
       endif
       if (ierr.eq.0) then
          call subv_decode(ierr, d, ndata, worki, wdsubv, nsub, vmiss)
       endif
       if (ierr.eq.0) call alloc_worki(ierr, -1, 'res:mi4f')
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:mi4f')
    else
       if (ldata.ge.0.and.ldata.lt.mfull) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          return
       endif
       if (ierr.eq.0) call alloc_worki(ierr, mdata, 'res:mi4f')
       if (ierr.eq.0) call get_data_record(ierr, worki, mdata, u, krect)
       if (ierr.eq.0) then
          call mask_decode(ierr, d, mfull, worki, wmask, vmiss, kpack)
       endif
       if (ierr.eq.0) call alloc_worki(ierr, -1, 'res:mi4f')
    endif
    if (ierr.eq.0) call alloc_wmask(ierr, -1, 'res:mi4f', mold)
  end subroutine restore_mi4_plain_f

!!!_  - restore_mr8_packed_d - restore MR8 data to packed array
  subroutine restore_mr8_packed_d &
       & (ierr, &
       &  d,    ldata, subv, ends, u, krect, mfull, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out)   :: ierr
    real(kind=KARG), intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: subv(0:*)
    integer,         intent(inout) :: ends(0:*)
    integer,         intent(in)    :: u
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: mfull        ! (logical) full data size
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check        ! whether subv, ends are references

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack
    integer mdata
    integer ch
    integer mi, mo, ci, mp

    ! note: position must be just after gtool header part
    ierr = 0
    kpack = legacy_unpacking(1, mfull)
    ch = choice(packed_read, check)
    ci = choice(0, citer)

    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mdata, u, krect)
    if (ierr.eq.0) then
       if (ldata.ge.0.and.ldata.lt.mdata) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          return
       endif
    endif

    ! mask array
    ncom = count_packed(1, mfull, mold)
    if (ch.eq.packed_ignore) then
       if (ierr.eq.0) call nio_skip_prec(ierr, u, 1, krect)
    else
       if (ierr.eq.0) call alloc_wmask(ierr, ncom, 'res:mr8d', mold)
       if (ierr.eq.0) call get_data_record(ierr, wmask, ncom, u, krect)
    endif

    ! data array, as is
    if (ierr.eq.0) call get_data_drecord(ierr, d, mdata, u, krect)

    if (ci.le.0.or.ci.gt.laxs) then
       mo = 1
       mi = product(max(1, kaxs(1:laxs)))
    else
       mo = max(1, product(max(1, kaxs(ci:laxs))))
       mi = max(1, product(max(1, kaxs(1:ci-1))))
    endif

    select case(ch)
    case(packed_check)
       ! consistency check for subv ends
       if (ierr.eq.0) call alloc_wsubv(ierr, mfull, 'res:mr8d')
       if (ierr.eq.0) call mask_decode_subv(ierr, wdsubv, wssubv, mi, mo, wmask, mfull, kpack)
       if (ierr.eq.0) then
          if (ANY(ends(0:mo-1).ne.wssubv(0:mo-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) then
          mp = ends(mo-1)
          if (ANY(subv(0:mp-1).ne.wdsubv(0:mp-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:mr8d')
    case(packed_read)
       ! read subv ends
       if (ierr.eq.0) call mask_decode_subv(ierr, subv, ends, mi, mo, wmask, mfull, kpack)
    case(packed_ignore)
       continue
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    if (ch.ne.packed_ignore) then
       if (ierr.eq.0) call alloc_wmask(ierr, -1, 'res:mr8d', mold)
    endif
    return
  end subroutine restore_mr8_packed_d
  subroutine restore_mr8_packed_f &
       & (ierr, &
       &  d,    ldata, subv, ends, u, krect, mfull, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out)   :: ierr
    real(kind=KARG), intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: subv(0:*)
    integer,         intent(inout) :: ends(0:*)
    integer,         intent(in)    :: u
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: mfull        ! (logical) full data size
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check        ! whether subv, ends are references

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack
    integer mdata
    integer ch
    integer mi, mo, ci, mp

    ! note: position must be just after gtool header part
    ierr = 0
    kpack = legacy_unpacking(1, mfull)
    ch = choice(packed_read, check)
    ci = choice(0, citer)

    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mdata, u, krect)
    if (ierr.eq.0) then
       if (ldata.ge.0.and.ldata.lt.mdata) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          return
       endif
    endif

    ! mask array
    ncom = count_packed(1, mfull, mold)
    if (ch.eq.packed_ignore) then
       if (ierr.eq.0) call nio_skip_prec(ierr, u, 1, krect)
    else
       if (ierr.eq.0) call alloc_wmask(ierr, ncom, 'res:mr8f', mold)
       if (ierr.eq.0) call get_data_record(ierr, wmask, ncom, u, krect)
    endif

    ! data array, as is
    if (ierr.eq.0) call get_data_drecord(ierr, d, mdata, u, krect)

    if (ci.le.0.or.ci.gt.laxs) then
       mo = 1
       mi = product(max(1, kaxs(1:laxs)))
    else
       mo = max(1, product(max(1, kaxs(ci:laxs))))
       mi = max(1, product(max(1, kaxs(1:ci-1))))
    endif

    select case(ch)
    case(packed_check)
       ! consistency check for subv ends
       if (ierr.eq.0) call alloc_wsubv(ierr, mfull, 'res:mr8f')
       if (ierr.eq.0) call mask_decode_subv(ierr, wdsubv, wssubv, mi, mo, wmask, mfull, kpack)
       if (ierr.eq.0) then
          if (ANY(ends(0:mo-1).ne.wssubv(0:mo-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) then
          mp = ends(mo-1)
          if (ANY(subv(0:mp-1).ne.wdsubv(0:mp-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:mr8f')
    case(packed_read)
       ! read subv ends
       if (ierr.eq.0) call mask_decode_subv(ierr, subv, ends, mi, mo, wmask, mfull, kpack)
    case(packed_ignore)
       continue
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    if (ch.ne.packed_ignore) then
       if (ierr.eq.0) call alloc_wmask(ierr, -1, 'res:mr8f', mold)
    endif
    return
  end subroutine restore_mr8_packed_f
  subroutine restore_mr8_packed_i &
       & (ierr, &
       &  d,    ldata, subv, ends, u, krect, mfull, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32, KRSRC=KDBL
    integer,           intent(out)   :: ierr
    integer(kind=KARG),intent(out)   :: d(0:*)
    integer,           intent(in)    :: ldata        ! limit size of of d
    integer,           intent(inout) :: subv(0:*)
    integer,           intent(inout) :: ends(0:*)
    integer,           intent(in)    :: u
    integer,           intent(in)    :: krect
    integer,           intent(in)    :: mfull        ! (logical) full data size
    integer,           intent(in)    :: kaxs(*)
    integer,optional,  intent(in)    :: citer        ! iteration index
    integer,optional,  intent(in)    :: check        ! whether subv, ends are references

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack
    integer mdata
    integer ch
    integer mi, mo, ci, mp

    ! note: position must be just after gtool header part
    ierr = 0
    kpack = legacy_unpacking(1, mfull)
    ch = choice(packed_read, check)
    ci = choice(0, citer)

    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mdata, u, krect)
    if (ierr.eq.0) then
       if (ldata.ge.0.and.ldata.lt.mdata) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          return
       endif
    endif

    ! mask array
    ncom = count_packed(1, mfull, mold)
    if (ch.eq.packed_ignore) then
       if (ierr.eq.0) call nio_skip_prec(ierr, u, 1, krect)
    else
       if (ierr.eq.0) call alloc_wmask(ierr, ncom, 'res:mr8i', mold)
       if (ierr.eq.0) call get_data_record(ierr, wmask, ncom, u, krect)
    endif

    ! data array, as is
    if (ierr.eq.0) call get_data_drecord(ierr, d, mdata, u, krect)

    if (ci.le.0.or.ci.gt.laxs) then
       mo = 1
       mi = product(max(1, kaxs(1:laxs)))
    else
       mo = max(1, product(max(1, kaxs(ci:laxs))))
       mi = max(1, product(max(1, kaxs(1:ci-1))))
    endif

    select case(ch)
    case(packed_check)
       ! consistency check for subv ends
       if (ierr.eq.0) call alloc_wsubv(ierr, mfull, 'res:mr8i')
       if (ierr.eq.0) call mask_decode_subv(ierr, wdsubv, wssubv, mi, mo, wmask, mfull, kpack)
       if (ierr.eq.0) then
          if (ANY(ends(0:mo-1).ne.wssubv(0:mo-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) then
          mp = ends(mo-1)
          if (ANY(subv(0:mp-1).ne.wdsubv(0:mp-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:mr8i')
    case(packed_read)
       ! read subv ends
       if (ierr.eq.0) call mask_decode_subv(ierr, subv, ends, mi, mo, wmask, mfull, kpack)
    case(packed_ignore)
       continue
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    if (ch.ne.packed_ignore) then
       if (ierr.eq.0) call alloc_wmask(ierr, -1, 'res:mr8i', mold)
    endif
    return
  end subroutine restore_mr8_packed_i
!!!_  - restore_mr4_packed_f - restore MR4 data to packed array
  subroutine restore_mr4_packed_f &
       & (ierr, &
       &  d,    ldata, subv, ends, u, krect, mfull, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out)   :: ierr
    real(kind=KARG), intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: subv(0:*)
    integer,         intent(inout) :: ends(0:*)
    integer,         intent(in)    :: u
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: mfull        ! (logical) full data size
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check        ! whether subv, ends are references

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack
    integer mdata
    integer ch
    integer mi, mo, ci, mp

    ! note: position must be just after gtool header part
    ierr = 0
    kpack = legacy_unpacking(1, mfull)
    ch = choice(packed_read, check)
    ci = choice(0, citer)

    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mdata, u, krect)
    if (ierr.eq.0) then
       if (ldata.ge.0.and.ldata.lt.mdata) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          return
       endif
    endif

    ! mask array
    ncom = count_packed(1, mfull, mold)
    if (ch.eq.packed_ignore) then
       if (ierr.eq.0) call nio_skip_prec(ierr, u, 1, krect)
    else
       if (ierr.eq.0) call alloc_wmask(ierr, ncom, 'res:mr4f', mold)
       if (ierr.eq.0) call get_data_record(ierr, wmask, ncom, u, krect)
    endif

    ! data array, as is
    if (ierr.eq.0) call get_data_frecord(ierr, d, mdata, u, krect)

    if (ci.le.0.or.ci.gt.laxs) then
       mo = 1
       mi = product(max(1, kaxs(1:laxs)))
    else
       mo = max(1, product(max(1, kaxs(ci:laxs))))
       mi = max(1, product(max(1, kaxs(1:ci-1))))
    endif

    select case(ch)
    case(packed_check)
       ! consistency check for subv ends
       if (ierr.eq.0) call alloc_wsubv(ierr, mfull, 'res:mr4f')
       if (ierr.eq.0) call mask_decode_subv(ierr, wdsubv, wssubv, mi, mo, wmask, mfull, kpack)
       if (ierr.eq.0) then
          if (ANY(ends(0:mo-1).ne.wssubv(0:mo-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) then
          mp = ends(mo-1)
          if (ANY(subv(0:mp-1).ne.wdsubv(0:mp-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:mr4f')
    case(packed_read)
       ! read subv ends
       if (ierr.eq.0) call mask_decode_subv(ierr, subv, ends, mi, mo, wmask, mfull, kpack)
    case(packed_ignore)
       continue
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    if (ierr.eq.0) call alloc_wmask(ierr, -1, 'res:mr4f', mold)
    return
  end subroutine restore_mr4_packed_f
  subroutine restore_mr4_packed_d &
       & (ierr, &
       &  d,    ldata, subv, ends, u, krect, mfull, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out)   :: ierr
    real(kind=KARG), intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: subv(0:*)
    integer,         intent(inout) :: ends(0:*)
    integer,         intent(in)    :: u
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: mfull        ! (logical) full data size
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check        ! whether subv, ends are references

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack
    integer mdata
    integer ch
    integer mi, mo, ci, mp

    ! note: position must be just after gtool header part
    ierr = 0
    kpack = legacy_unpacking(1, mfull)
    ch = choice(packed_read, check)
    ci = choice(0, citer)

    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mdata, u, krect)
    if (ierr.eq.0) then
       if (ldata.ge.0.and.ldata.lt.mdata) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          return
       endif
    endif

    ! mask array
    ncom = count_packed(1, mfull, mold)
    if (ch.eq.packed_ignore) then
       if (ierr.eq.0) call nio_skip_prec(ierr, u, 1, krect)
    else
       if (ierr.eq.0) call alloc_wmask(ierr, ncom, 'res:mr4d', mold)
       if (ierr.eq.0) call get_data_record(ierr, wmask, ncom, u, krect)
    endif

    ! data array, as is
    if (ierr.eq.0) call get_data_frecord(ierr, d, mdata, u, krect)

    if (ci.le.0.or.ci.gt.laxs) then
       mo = 1
       mi = product(max(1, kaxs(1:laxs)))
    else
       mo = max(1, product(max(1, kaxs(ci:laxs))))
       mi = max(1, product(max(1, kaxs(1:ci-1))))
    endif

    select case(ch)
    case(packed_check)
       ! consistency check for subv ends
       if (ierr.eq.0) call alloc_wsubv(ierr, mfull, 'res:mr4d')
       if (ierr.eq.0) call mask_decode_subv(ierr, wdsubv, wssubv, mi, mo, wmask, mfull, kpack)
       if (ierr.eq.0) then
          if (ANY(ends(0:mo-1).ne.wssubv(0:mo-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) then
          mp = ends(mo-1)
          if (ANY(subv(0:mp-1).ne.wdsubv(0:mp-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:mr4d')
    case(packed_read)
       ! read subv ends
       if (ierr.eq.0) call mask_decode_subv(ierr, subv, ends, mi, mo, wmask, mfull, kpack)
    case(packed_ignore)
       continue
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    if (ch.ne.packed_ignore) then
       if (ierr.eq.0) call alloc_wmask(ierr, -1, 'res:mr4d', mold)
    endif
    return
  end subroutine restore_mr4_packed_d
  subroutine restore_mr4_packed_i &
       & (ierr, &
       &  d,    ldata, subv, ends, u, krect, mfull, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32, KRSRC=KFLT
    integer,           intent(out)   :: ierr
    integer(kind=KARG),intent(out)   :: d(0:*)
    integer,           intent(in)    :: ldata        ! limit size of of d
    integer,           intent(inout) :: subv(0:*)
    integer,           intent(inout) :: ends(0:*)
    integer,           intent(in)    :: u
    integer,           intent(in)    :: krect
    integer,           intent(in)    :: mfull        ! (logical) full data size
    integer,           intent(in)    :: kaxs(*)
    integer,optional,  intent(in)    :: citer        ! iteration index
    integer,optional,  intent(in)    :: check        ! whether subv, ends are references

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack
    integer mdata
    integer ch
    integer mi, mo, ci, mp

    ! note: position must be just after gtool header part
    ierr = 0
    kpack = legacy_unpacking(1, mfull)
    ch = choice(packed_read, check)
    ci = choice(0, citer)

    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mdata, u, krect)
    if (ierr.eq.0) then
       if (ldata.ge.0.and.ldata.lt.mdata) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          return
       endif
    endif

    ! mask array
    ncom = count_packed(1, mfull, mold)
    if (ch.eq.packed_ignore) then
       if (ierr.eq.0) call nio_skip_prec(ierr, u, 1, krect)
    else
       if (ierr.eq.0) call alloc_wmask(ierr, ncom, 'res:mr4i', mold)
       if (ierr.eq.0) call get_data_record(ierr, wmask, ncom, u, krect)
    endif

    ! data array, as is
    if (ierr.eq.0) call get_data_frecord(ierr, d, mdata, u, krect)

    if (ci.le.0.or.ci.gt.laxs) then
       mo = 1
       mi = product(max(1, kaxs(1:laxs)))
    else
       mo = max(1, product(max(1, kaxs(ci:laxs))))
       mi = max(1, product(max(1, kaxs(1:ci-1))))
    endif

    select case(ch)
    case(packed_check)
       ! consistency check for subv ends
       if (ierr.eq.0) call alloc_wsubv(ierr, mfull, 'res:mr4i')
       if (ierr.eq.0) call mask_decode_subv(ierr, wdsubv, wssubv, mi, mo, wmask, mfull, kpack)
       if (ierr.eq.0) then
          if (ANY(ends(0:mo-1).ne.wssubv(0:mo-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) then
          mp = ends(mo-1)
          if (ANY(subv(0:mp-1).ne.wdsubv(0:mp-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:mr4i')
    case(packed_read)
       ! read subv ends
       if (ierr.eq.0) call mask_decode_subv(ierr, subv, ends, mi, mo, wmask, mfull, kpack)
    case(packed_ignore)
       continue
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    if (ch.ne.packed_ignore) then
       if (ierr.eq.0) call alloc_wmask(ierr, -1, 'res:mr4i', mold)
    endif
    return
  end subroutine restore_mr4_packed_i

!!!_  - restore_mi4_packed_i - restore MI4 data to packed array
  subroutine restore_mi4_packed_i &
       & (ierr, &
       &  d,    ldata, subv, ends, u, krect, mfull, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32
    integer,           intent(out)   :: ierr
    integer(kind=KARG),intent(out)   :: d(0:*)
    integer,           intent(in)    :: ldata        ! limit size of of d
    integer,           intent(inout) :: subv(0:*)
    integer,           intent(inout) :: ends(0:*)
    integer,           intent(in)    :: u
    integer,           intent(in)    :: krect
    integer,           intent(in)    :: mfull        ! (logical) full data size
    integer,           intent(in)    :: kaxs(*)
    integer,optional,  intent(in)    :: citer        ! iteration index
    integer,optional,  intent(in)    :: check        ! whether subv, ends are references

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack
    integer mdata
    integer ch
    integer mi, mo, ci, mp

    ! note: position must be just after gtool header part
    ierr = 0
    kpack = legacy_unpacking(1, mfull)
    ch = choice(packed_read, check)
    ci = choice(0, citer)

    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mdata, u, krect)
    if (ierr.eq.0) then
       if (ldata.ge.0.and.ldata.lt.mdata) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          return
       endif
    endif

    ! mask array
    ncom = count_packed(1, mfull, mold)
    if (ch.eq.packed_ignore) then
       if (ierr.eq.0) call nio_skip_prec(ierr, u, 1, krect)
    else
       if (ierr.eq.0) call alloc_wmask(ierr, ncom, 'res:mi4i', mold)
       if (ierr.eq.0) call get_data_record(ierr, wmask, ncom, u, krect)
    endif

    ! data array, as is
    if (ierr.eq.0) call get_data_irecord(ierr, d, mdata, u, krect)

    if (ci.le.0.or.ci.gt.laxs) then
       mo = 1
       mi = product(max(1, kaxs(1:laxs)))
    else
       mo = max(1, product(max(1, kaxs(ci:laxs))))
       mi = max(1, product(max(1, kaxs(1:ci-1))))
    endif

    select case(ch)
    case(packed_check)
       ! consistency check for subv ends
       if (ierr.eq.0) call alloc_wsubv(ierr, mfull, 'res:mi4i')
       if (ierr.eq.0) call mask_decode_subv(ierr, wdsubv, wssubv, mi, mo, wmask, mfull, kpack)
       if (ierr.eq.0) then
          if (ANY(ends(0:mo-1).ne.wssubv(0:mo-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) then
          mp = ends(mo-1)
          if (ANY(subv(0:mp-1).ne.wdsubv(0:mp-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:mi4i')
    case(packed_read)
       ! read subv ends
       if (ierr.eq.0) call mask_decode_subv(ierr, subv, ends, mi, mo, wmask, mfull, kpack)
    case(packed_ignore)
       continue
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    if (ch.ne.packed_ignore) then
       if (ierr.eq.0) call alloc_wmask(ierr, -1, 'res:mi4i', mold)
    endif
    return
  end subroutine restore_mi4_packed_i
  subroutine restore_mi4_packed_d &
       & (ierr, &
       &  d,    ldata, subv, ends, u, krect, mfull, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32
    integer,         intent(out)   :: ierr
    real(kind=KARG), intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: subv(0:*)
    integer,         intent(inout) :: ends(0:*)
    integer,         intent(in)    :: u
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: mfull        ! (logical) full data size
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check        ! whether subv, ends are references

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack
    integer mdata
    integer ch
    integer mi, mo, ci, mp

    ! note: position must be just after gtool header part
    ierr = 0
    kpack = legacy_unpacking(1, mfull)
    ch = choice(packed_read, check)
    ci = choice(0, citer)

    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mdata, u, krect)
    if (ierr.eq.0) then
       if (ldata.ge.0.and.ldata.lt.mdata) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          return
       endif
    endif

    ! mask array
    ncom = count_packed(1, mfull, mold)
    if (ch.eq.packed_ignore) then
       if (ierr.eq.0) call nio_skip_prec(ierr, u, 1, krect)
    else
       if (ierr.eq.0) call alloc_wmask(ierr, ncom, 'res:mi4d', mold)
       if (ierr.eq.0) call get_data_record(ierr, wmask, ncom, u, krect)
    endif

    ! data array, as is
    if (ierr.eq.0) call get_data_irecord(ierr, d, mdata, u, krect)

    if (ci.le.0.or.ci.gt.laxs) then
       mo = 1
       mi = product(max(1, kaxs(1:laxs)))
    else
       mo = max(1, product(max(1, kaxs(ci:laxs))))
       mi = max(1, product(max(1, kaxs(1:ci-1))))
    endif

    select case(ch)
    case(packed_check)
       ! consistency check for subv ends
       if (ierr.eq.0) call alloc_wsubv(ierr, mfull, 'res:mi4d')
       if (ierr.eq.0) call mask_decode_subv(ierr, wdsubv, wssubv, mi, mo, wmask, mfull, kpack)
       if (ierr.eq.0) then
          if (ANY(ends(0:mo-1).ne.wssubv(0:mo-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) then
          mp = ends(mo-1)
          if (ANY(subv(0:mp-1).ne.wdsubv(0:mp-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:mi4d')
    case(packed_read)
       ! read subv ends
       if (ierr.eq.0) call mask_decode_subv(ierr, subv, ends, mi, mo, wmask, mfull, kpack)
    case(packed_ignore)
       continue
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    if (ch.ne.packed_ignore) then
       if (ierr.eq.0) call alloc_wmask(ierr, -1, 'res:mi4d', mold)
    endif
    return
  end subroutine restore_mi4_packed_d
  subroutine restore_mi4_packed_f &
       & (ierr, &
       &  d,    ldata, subv, ends, u, krect, mfull, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32
    integer,         intent(out)   :: ierr
    real(kind=KARG), intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: subv(0:*)
    integer,         intent(inout) :: ends(0:*)
    integer,         intent(in)    :: u
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: mfull        ! (logical) full data size
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check        ! whether subv, ends are references

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack
    integer mdata
    integer ch
    integer mi, mo, ci, mp

    ! note: position must be just after gtool header part
    ierr = 0
    kpack = legacy_unpacking(1, mfull)
    ch = choice(packed_read, check)
    ci = choice(0, citer)

    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mdata, u, krect)
    if (ierr.eq.0) then
       if (ldata.ge.0.and.ldata.lt.mdata) then
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          return
       endif
    endif

    ! mask array
    ncom = count_packed(1, mfull, mold)
    if (ch.eq.packed_ignore) then
       if (ierr.eq.0) call nio_skip_prec(ierr, u, 1, krect)
    else
       if (ierr.eq.0) call alloc_wmask(ierr, ncom, 'res:mi4f', mold)
       if (ierr.eq.0) call get_data_record(ierr, wmask, ncom, u, krect)
    endif

    ! data array, as is
    if (ierr.eq.0) call get_data_irecord(ierr, d, mdata, u, krect)

    if (ci.le.0.or.ci.gt.laxs) then
       mo = 1
       mi = product(max(1, kaxs(1:laxs)))
    else
       mo = max(1, product(max(1, kaxs(ci:laxs))))
       mi = max(1, product(max(1, kaxs(1:ci-1))))
    endif

    select case(ch)
    case(packed_check)
       ! consistency check for subv ends
       if (ierr.eq.0) call alloc_wsubv(ierr, mfull, 'res:mi4f')
       if (ierr.eq.0) call mask_decode_subv(ierr, wdsubv, wssubv, mi, mo, wmask, mfull, kpack)
       if (ierr.eq.0) then
          if (ANY(ends(0:mo-1).ne.wssubv(0:mo-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) then
          mp = ends(mo-1)
          if (ANY(subv(0:mp-1).ne.wdsubv(0:mp-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:mi4f')
    case(packed_read)
       ! read subv ends
       if (ierr.eq.0) call mask_decode_subv(ierr, subv, ends, mi, mo, wmask, mfull, kpack)
    case(packed_ignore)
       continue
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    if (ch.ne.packed_ignore) then
       if (ierr.eq.0) call alloc_wmask(ierr, -1, 'res:mi4f', mold)
    endif
    return
  end subroutine restore_mi4_packed_f

!!!_ + gtool-3 discarded extension
!!!_  - JRn
!!!_  - ZRn
!!!_  - URS

!!!_ + *RT system (gtool-3 extension)
!!!_  - put_data_mrt_plain - URY:TOUZA/Trapiche plain format (full bundle)
  subroutine put_data_mrt_plain_d &
       & (ierr, &
       &  d, n, u, krect, vmiss, kaxs, kopts)
    use TOUZA_Trp,only: suggest_filling
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kaxs(*)
    integer,optional,intent(in)  :: kopts(:)

    integer mbits, xbits, xtop, xbtm
    integer,parameter :: mlim = DIGITS(0.0_KRSRC) - 1

    integer j, m
    integer(kind=KISRC) :: mb
    integer(kind=KISRC) :: icom(0:n-1)
    real(kind=KARG)     :: buf(n)
    integer ncom,  na
    integer kpack, kcode
    logical pre, post
    integer nbreak

    ierr = err_default

    kcode = def_encode_trapiche
    if (ierr.eq.0) then
       call extract_urt_options &
            & (ierr, mbits, xbits, xtop, xbtm, nbreak, kcode, mlim, kaxs, kopts)
    endif

    if (ierr.eq.0) call put_data_urt_cache(ierr, n, nbreak, u, krect)

    if (ierr.eq.0) then
       do j = 0, n - 1, nbreak
          m = min(nbreak, n - j)
          pre  = .TRUE.
          post = j + m .lt. n
          kpack = suggest_filling(mbits, m, def_encode_trapiche)
          if (ierr.eq.0) then
             call mask_encode &
                  & (ierr, mb,   ncom,   icom(2:), buf,   &
                  &  d(j:j+m-1), m,      vmiss,    kpack)
          endif
          if (ierr.eq.0) then
             na = ncom + 2
             icom(0) = m
             icom(1) = kpack
             ! write(*, *) na, ncom, m, kpack, icom(0:na-1)
          endif
          ! write(*, *) kpack, ncom
          ! if (ierr.eq.0) call put_data_record(ierr, icom, ncom+1, u, krect, post=.true.)
          ! write(*, *) j, nbreak, m, mb, ncom
          if (ierr.eq.0) then
             call put_data_urt_core &
                  & (ierr,  &
                  &  buf,   mb,    u,     krect, pre,   post, &
                  &  vmiss, mbits, xbits, xtop,  xbtm,  kcode,   icom(0:na-1))
          endif
       enddo
    endif
    return
  end subroutine put_data_mrt_plain_d
  subroutine put_data_mrt_plain_f &
       & (ierr, &
       &  d, n, u, krect, vmiss, kaxs, kopts)
    use TOUZA_Trp,only: suggest_filling
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kaxs(*)
    integer,optional,intent(in)  :: kopts(:)

    integer mbits, xbits, xtop, xbtm
    integer,parameter :: mlim = DIGITS(0.0_KRSRC) - 1

    integer j, m
    integer(kind=KISRC) :: mb
    integer(kind=KISRC) :: icom(0:n-1)
    real(kind=KARG)     :: buf(n)
    integer ncom,  na
    integer kpack, kcode
    logical pre, post
    integer nbreak

    ierr = err_default

    kcode = def_encode_trapiche
    if (ierr.eq.0) then
       call extract_urt_options &
            & (ierr, mbits, xbits, xtop, xbtm, nbreak, kcode, mlim, kaxs, kopts)
    endif

    if (ierr.eq.0) call put_data_urt_cache(ierr, n, nbreak, u, krect)

    if (ierr.eq.0) then
       do j = 0, n - 1, nbreak
          m = min(nbreak, n - j)
          pre  = .TRUE.
          post = j + m .lt. n
          kpack = suggest_filling(mbits, m, def_encode_trapiche)
          if (ierr.eq.0) then
             call mask_encode &
                  & (ierr, mb,   ncom, icom(2:), buf, &
                  &  d(j:j+m-1), m,    vmiss,    kpack)
          endif
          if (ierr.eq.0) then
             na = ncom + 2
             icom(0) = m
             icom(1) = kpack
          endif
          ! if (ierr.eq.0) call put_data_record(ierr, icom, ncom+1, u, krect, post=.true.)
          if (ierr.eq.0) then
             call put_data_urt_core &
                  & (ierr,  &
                  &  buf,   mb,    u,     krect, pre,    post,    &
                  &  vmiss, mbits, xbits, xtop,  xbtm,   kcode,   icom(0:na-1))
          endif
       enddo
    endif
    return
  end subroutine put_data_mrt_plain_f

!!!_  - put_data_urt_plain - URY:TOUZA/Trapiche plain format (full bundle)
  subroutine put_data_urt_plain_d &
       & (ierr, &
       &  d, n, u, krect, vmiss, kaxs, kopts)
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kaxs(*)
    integer,optional,intent(in)  :: kopts(:)

    integer j, m
    integer mbits, xbits, xtop, xbtm
    integer,parameter :: mlim = DIGITS(0.0_KRSRC) - 1
    integer kcode
    logical pre, post
    integer nbreak

    ierr = err_default
    kcode = def_encode_trapiche
    ! write(*, *) 'in', ierr
    if (ierr.eq.0) then
       call extract_urt_options &
            & (ierr, mbits, xbits, xtop, xbtm, nbreak, kcode, mlim, kaxs, kopts)
    endif
    ! write(*, *) 'extract', ierr

    if (ierr.eq.0) call put_data_urt_cache(ierr, n, nbreak, u, krect)
    ! write(*, *) 'cache', ierr

    if (ierr.eq.0) then
       do j = 0, n - 1, nbreak
          m = min(nbreak, n - j)
          ! pre  = j.gt.0
          pre  = .TRUE.
          post = j + m .lt. n
          ! write(*, *) j, nbreak, m, n
          if (ierr.eq.0) then
             call put_data_urt_core &
                  & (ierr,  &
                  &  d(j:j+m-1),   m,     u,     krect, pre,  post, &
                  &  vmiss, mbits, xbits, xtop,  xbtm,  kcode)
          endif
       enddo
    endif
    ! write(*, *) 'data', ierr
    return
  end subroutine put_data_urt_plain_d
  subroutine put_data_urt_plain_f &
       & (ierr, &
       &  d, n, u, krect, vmiss, kaxs, kopts)
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kaxs(*)
    integer,optional,intent(in)  :: kopts(:)

    integer j, m
    integer mbits, xbits, xtop, xbtm
    integer,parameter :: mlim = DIGITS(0.0_KRSRC) - 1
    integer kcode
    logical pre, post
    integer nbreak

    ierr = err_default
    kcode = def_encode_trapiche
    if (ierr.eq.0) then
       call extract_urt_options &
            & (ierr, mbits, xbits, xtop, xbtm, nbreak, kcode, mlim, kaxs, kopts)
    endif
    if (ierr.eq.0) call put_data_urt_cache(ierr, n, nbreak, u, krect)

    if (ierr.eq.0) then
       do j = 0, n - 1, nbreak
          m = min(nbreak, n - j)
          ! pre  = j.gt.0
          pre  = .TRUE.
          post = j + m .lt. n
          if (ierr.eq.0) then
             call put_data_urt_core &
                  & (ierr,  &
                  &  d(j:j+m-1),   m,     u,     krect, pre,  post, &
                  &  vmiss, mbits, xbits, xtop,  xbtm,  kcode)
          endif
       enddo
    endif
    return
  end subroutine put_data_urt_plain_f

!!!_  - put_data_urt_cache
  subroutine put_data_urt_cache &
       & (ierr,  &
       &  n,     nbreak,  u,  krect)
    use TOUZA_Nio_std,only: sus_pad_irec
    implicit none
    integer,parameter :: KISRC=KI32
    integer,intent(out) :: ierr
    integer,intent(in)  :: n     ! d size
    integer,intent(in)  :: nbreak
    integer,intent(in)  :: u
    integer,intent(in)  :: krect
    integer mem
    integer,parameter :: mpr = 2
    integer(kind=KISRC) :: fill = 0
    logical swap, lrec

    ierr = err_default
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       return
    endif
    if (ierr.eq.0) then
       mem = (n - 1) / nbreak + 1
       mem = min(max(1, mem), lrec_urt / mpr) * mpr
       ! force subrecord design
       call sus_pad_irec(ierr, u, fill, mem, swap, .FALSE., .TRUE., dummy=def_block)
    endif
    return
  end subroutine put_data_urt_cache

!!!_  - get_data_urt_cache
  subroutine get_data_urt_cache &
       & (ierr,  &
       &  cache, mem, n, u,  krect)
    use TOUZA_Nio_std,only: sus_record_mems_irec, sus_read_irec
    implicit none
    integer,parameter :: KISRC=KI32
    integer,            intent(out) :: ierr
    integer(kind=KISRC),intent(out) :: cache(*)
    integer,            intent(out) :: mem
    integer,            intent(in)  :: n
    integer,            intent(in)  :: u
    integer,            intent(in)  :: krect
    integer,parameter :: mpr = 2
    integer ni
    logical swap, lrec, sub

    ierr = err_default
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       return
    endif
    sub = .TRUE.
    if (ierr.eq.0) call sus_record_mems_irec(ierr, mem, u, cache(1), swap, sub)
    if (ierr.eq.0) then
       mem = mem / mpr
       ni = mem
       if (n.gt.0) ni = min(ni, n)
       sub = .TRUE.
       ! force subrecord design
       call sus_read_irec(ierr, u, cache, ni, swap, sub, div=def_block)
    endif
    ! write(*, *) 'cache', ierr, mem, ni
    return
  end subroutine get_data_urt_cache

!!!_  - put_data_urt_core
  subroutine put_data_urt_core_d &
       & (ierr,  &
       &  d,     n,     u,     krect, pre,  post,  &
       &  vmiss, mbits, xbits, xtop,  xbtm, kcode, kapp)
    use TOUZA_Trp,only: &
         & count_packed, encode_alloc, retrieve_nbgz, &
         & KB_HEAD, guardar_extra
    use TOUZA_Trp,only: show_bagazo_props
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(*)
    integer,         intent(in)  :: n     ! d size
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    logical,         intent(in)  :: pre, post
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: mbits
    integer,         intent(in)  :: xbits, xtop, xbtm
    integer,         intent(in)  :: kcode
    integer,optional,intent(in)  :: kapp(0:)

    integer(kind=KISRC) :: ibagaz(0:2*n+KB_HEAD)
    integer nbgz, napp
    integer rectx

    ierr = err_default
    if (present(kapp)) then
       napp = size(kapp)
    else
       napp = 0
    endif
    rectx = IOR(krect, REC_SUB_ALLOW)  ! force sub-record splitting

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
       call put_data_record &
            & (ierr, ibagaz(0:KB_HEAD-1), KB_HEAD, u, rectx, pre=pre, post=.TRUE.)
    endif
    if (ierr.eq.0) then
       if (napp.gt.0) then
          call put_data_record &
               & (ierr, kapp(0:napp-1), napp, u, rectx, pre=.TRUE., post=.TRUE.)
       endif
    endif
    if (ierr.eq.0) then
       nbgz = retrieve_nbgz(ibagaz)
       call put_data_record &
            & (ierr, ibagaz(KB_HEAD:KB_HEAD+nbgz-1), nbgz, u, rectx, pre=.TRUE., post=post)
    endif
    if (ierr.eq.0) then
       if (udiag.ge.-1) call show_bagazo_props(ierr, ibagaz, udiag)
    endif
    return
  end subroutine put_data_urt_core_d
  subroutine put_data_urt_core_f &
       & (ierr,  &
       &  d,     n,     u,     krect, pre,  post,  &
       &  vmiss, mbits, xbits, xtop,  xbtm, kcode, kapp)
    use TOUZA_Trp,only: &
         & count_packed, encode_alloc, retrieve_nbgz, &
         & KB_HEAD, guardar_extra
    use TOUZA_Trp,only: show_bagazo_props
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(*)
    integer,         intent(in)  :: n     ! d size
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    logical,         intent(in)  :: pre, post
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: mbits
    integer,         intent(in)  :: xbits, xtop, xbtm
    integer,         intent(in)  :: kcode
    integer,optional,intent(in)  :: kapp(0:)

    integer(kind=KISRC) :: ibagaz(0:2*n+KB_HEAD)
    integer nbgz, napp
    integer rectx
    real(kind=KRSRC) :: rmiss

    ierr = err_default
    if (present(kapp)) then
       napp = size(kapp)
    else
       napp = 0
    endif
    rectx = IOR(krect, REC_SUB_ALLOW)  ! force sub-record splitting

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
       call put_data_record &
            & (ierr, ibagaz(0:KB_HEAD-1), KB_HEAD, u, rectx, pre=pre, post=.TRUE.)
    endif
    if (ierr.eq.0) then
       if (napp.gt.0) then
          call put_data_record &
               & (ierr, kapp(0:napp-1), napp, u, rectx, pre=.TRUE., post=.TRUE.)
       endif
    endif
    if (ierr.eq.0) then
       call put_data_record &
            & (ierr, ibagaz(KB_HEAD:KB_HEAD+nbgz-1), nbgz, u, rectx, pre=.TRUE., post=post)
    endif
    if (ierr.eq.0) then
       if (udiag.ge.-1) call show_bagazo_props(ierr, ibagaz, udiag)
    endif
    return
  end subroutine put_data_urt_core_f
!!!_  - get_data_mrt - MRT
  subroutine get_data_mrt_d &
       & (ierr, &
       &  d, n, u, krect, vmiss, kopts)
    use TOUZA_Trp,only: &
         & decode_alloc, retrieve_nbgz, retrieve_ncnz, retrieve_extra, &
         & count_packed, suggest_filling
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,optional,intent(out) :: kopts(:)

    integer(kind=KISRC) :: icom(0:n-1)
    real(kind=KRSRC)    :: buf(n)
    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom, na
    logical sub
    integer kpack
    integer jv, mv, nv, mp
    integer,parameter :: lcd = 2
    integer cdummy(lcd)
    integer cmem

    ierr = err_default

    if (ierr.eq.0) call get_data_urt_cache(ierr, cdummy, cmem, lcd, u, krect)

    nv = n
    jv = 0

    do
       if (ierr.eq.0) then
          sub = .TRUE.
          call get_data_urt_core &
               & (ierr, &
               &  buf,   mv, nv,  u, krect,   sub,  &
               &  vmiss, def_decode_trapiche, kopts, napp=na, kapp=icom)
       endif
       if (ierr.eq.0) then
          ncom = na - 2
          mp = icom(0)
          kpack = icom(1)
          kpack = suggest_filling(1, mp, kcode=def_decode_trapiche, kfill=kpack)

          call mask_decode &
               & (ierr,  d(jv:jv+mp-1), mp, buf, icom(2:), vmiss, kpack)
       endif
       if (ierr.eq.0) then
          jv = jv + mp
          nv = nv - mp
          if (nv.eq.0) exit
          if (nv.lt.0) then
             ierr = _ERROR(ERR_BROKEN_RECORD)
          endif
          if (.not.sub) ierr = _ERROR(ERR_BROKEN_RECORD)
       endif
       if (ierr.ne.0) exit
    enddo
    return
  end subroutine get_data_mrt_d
  subroutine get_data_mrt_f &
       & (ierr, &
       &  d, n, u, krect, vmiss, kopts)
    use TOUZA_Trp,only: &
         & decode_alloc, retrieve_nbgz, retrieve_ncnz, retrieve_extra, &
         & count_packed, suggest_filling
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,optional,intent(out) :: kopts(:)

    integer(kind=KISRC) :: icom(0:n-1)
    real(kind=KRSRC)    :: buf(n)
    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom, na
    logical sub
    integer kpack
    integer jv, mv, nv, mp
    integer,parameter :: lcd = 2
    integer cdummy(lcd)
    integer cmem

    ierr = err_default

    if (ierr.eq.0) call get_data_urt_cache(ierr, cdummy, cmem, lcd, u, krect)

    nv = n
    jv = 0

    do
       if (ierr.eq.0) then
          sub = .TRUE.
          call get_data_urt_core &
               & (ierr, &
               &  buf,   mv, nv,  u, krect,   sub,  &
               &  vmiss, def_decode_trapiche, kopts, napp=na, kapp=icom)
       endif
       if (ierr.eq.0) then
          ncom = na - 2
          mp = icom(0)
          kpack = icom(1)
          kpack = suggest_filling(1, mp, kcode=def_decode_trapiche, kfill=kpack)

          call mask_decode &
               & (ierr,  d(jv:jv+mp-1), mp, buf, icom(2:), vmiss, kpack)
       endif
       if (ierr.eq.0) then
          jv = jv + mp
          nv = nv - mp
          if (nv.eq.0) exit
          if (nv.lt.0) then
             ierr = _ERROR(ERR_BROKEN_RECORD)
          endif
          if (.not.sub) ierr = _ERROR(ERR_BROKEN_RECORD)
       endif
       if (ierr.ne.0) exit
    enddo

    return
  end subroutine get_data_mrt_f

!!!_  - get_data_urt - URY:TOUZA/Trapiche
  subroutine get_data_urt_d &
       & (ierr, &
       &  d, n,  u, krect, vmiss, &
       &  kopts, napp, kapp)
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,optional,intent(out) :: kopts(:)
    integer,optional,intent(out) :: napp
    integer,optional,intent(out) :: kapp(0:)

    logical sub
    integer jv,   mv,   nv
    integer japp, ma
    integer,parameter :: lcd = 2
    integer cdummy(lcd)
    integer cmem

    ierr = err_default

    if (ierr.eq.0) call get_data_urt_cache(ierr, cdummy, cmem, lcd, u, krect)

    jv = 0
    nv = n
    japp = 0
    sub = .TRUE.
    do
       if (present(kapp)) then
          if (ierr.eq.0) then
             call get_data_urt_core &
                  & (ierr,       &
                  &  d(jv:jv+nv-1), mv,  nv,  u,  krect,  sub, &
                  &  vmiss,      def_decode_trapiche,       &
                  &  kopts,      ma,      kapp(japp:))
             if (ierr.eq.0) japp = japp + ma
          endif
       else
          if (ierr.eq.0) then
             call get_data_urt_core &
                  & (ierr,       &
                  &  d(jv:jv+nv-1), mv,  nv,  u,  krect,  sub, &
                  &  vmiss,      def_decode_trapiche,       &
                  &  kopts,      ma)
          endif
       endif
       if (ierr.eq.0) then
          japp = japp + ma
          jv = jv + mv
          nv = nv - mv
          if (nv.eq.0) exit
          if (nv.lt.0) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          if (.not.sub) ierr = _ERROR(ERR_BROKEN_RECORD)
       endif
       if (ierr.ne.0) exit
    enddo
    if (present(napp)) then
       napp = japp
    endif
    return
  end subroutine get_data_urt_d
  subroutine get_data_urt_f &
       & (ierr, &
       &  d, n, u, krect, vmiss, &
       &  kopts,napp, kapp)
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,optional,intent(out) :: kopts(:)
    integer,optional,intent(out) :: napp
    integer,optional,intent(out) :: kapp(0:)

    logical sub
    integer jv,   mv,   nv
    integer japp, ma
    integer,parameter :: lcd = 2
    integer cdummy(lcd)
    integer cmem

    ierr = err_default

    if (ierr.eq.0) call get_data_urt_cache(ierr, cdummy, cmem, lcd, u, krect)

    jv = 0
    nv = n
    japp = 0
    sub = .TRUE.
    do
       if (present(kapp)) then
          if (ierr.eq.0) then
             call get_data_urt_core &
                  & (ierr,       &
                  &  d(jv:jv+nv-1), mv,  nv,  u,  krect,  sub, &
                  &  vmiss,      def_decode_trapiche,       &
                  &  kopts,      ma,      kapp(japp:))
             if (ierr.eq.0) japp = japp + ma
          endif
       else
          if (ierr.eq.0) then
             call get_data_urt_core &
                  & (ierr,       &
                  &  d(jv:jv+nv-1), mv,  nv,  u,  krect,  sub, &
                  &  vmiss,      def_decode_trapiche,       &
                  &  kopts,      ma)
          endif
       endif
       if (ierr.eq.0) then
          japp = japp + ma
          jv = jv + mv
          nv = nv - mv
          if (nv.eq.0) exit
          if (nv.lt.0) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          if (.not.sub) ierr = _ERROR(ERR_BROKEN_RECORD)
       endif
       if (ierr.ne.0) exit
    enddo
    if (present(napp)) then
       napp = japp
    endif
    return
  end subroutine get_data_urt_f
  subroutine get_data_urt_core_d &
       & (ierr, &
       &  d,     m,     n,    u,    krect,  sub, &
       &  vmiss, kcode, kopts,napp, kapp)
    use TOUZA_Trp,only: &
         & decode_alloc, retrieve_nbgz, retrieve_ncnz, retrieve_extra, &
         & KB_HEAD,      show_bagazo_props
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out)   :: ierr
    real(kind=KARG), intent(out)   :: d(*)
    integer,         intent(out)   :: m    ! elements success
    integer,         intent(in)    :: n    ! elements limit
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: u
    logical,         intent(inout) :: sub
    real(kind=KRMIS),intent(in)    :: vmiss
    integer,         intent(in)    :: kcode
    integer,optional,intent(out)   :: kopts(:)
    integer,optional,intent(out)   :: napp
    integer,optional,intent(out)   :: kapp(0:)

    integer(kind=KISRC) :: ibagaz(0:2*n+KB_HEAD)
    integer(kind=KISRC) :: kdmy(1)
    integer nbgz, ncnz
    integer na,   ma, xid
    logical cont
    integer rectx

    ierr = err_default
    na = 0
    rectx = IOR(krect, REC_SUB_ALLOW)  ! force sub-record splitting

    if (ierr.eq.0) then
       cont = .TRUE.
       call get_data_record(ierr, ibagaz(0:KB_HEAD-1), KB_HEAD, u, rectx, sub=cont)
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
       if (present(napp)) then
          napp = na
       endif
       if (na.gt.0) then
          cont = .TRUE.
          if (present(kapp)) then
             ma = min(na, size(kapp))
             call get_data_record(ierr, kapp(0:ma-1), ma, u, rectx, sub=cont)
          else
             call get_data_record(ierr, kdmy, 1, u, rectx, sub=cont)
          endif
       endif
    endif
    if (ierr.eq.0) then
       nbgz = retrieve_nbgz(ibagaz)
       call get_data_record(ierr, ibagaz(KB_HEAD:KB_HEAD+nbgz-1), nbgz, u, rectx, sub)
    endif

    if (ierr.eq.0) then
       ncnz = retrieve_ncnz(ibagaz)
       if (ncnz.gt.n) then
          ierr = -1
       else
          call decode_alloc(ierr, d, ibagaz, ncnz, vmiss, kcode)
       endif
       m = ncnz
    endif
    return
  end subroutine get_data_urt_core_d
  subroutine get_data_urt_core_f &
       & (ierr, &
       &  d,     m,     n,     u,    krect,  sub, &
       &  vmiss, kcode, kopts, napp, kapp)
    use TOUZA_Trp,only: &
         & decode_alloc, retrieve_nbgz, retrieve_ncnz, retrieve_extra, &
         & KB_HEAD,      show_bagazo_props
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out)   :: ierr
    real(kind=KARG), intent(out)   :: d(*)
    integer,         intent(out)   :: m
    integer,         intent(in)    :: n
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: u
    logical,         intent(inout) :: sub
    real(kind=KRMIS),intent(in)    :: vmiss
    integer,         intent(in)    :: kcode
    integer,optional,intent(out)   :: kopts(:)
    integer,optional,intent(out)   :: napp
    integer,optional,intent(out)   :: kapp(0:)

    integer(kind=KISRC) :: ibagaz(0:2*n+KB_HEAD)
    integer(kind=KISRC) :: kdmy(1)
    integer nbgz, ncnz
    integer na,   ma, xid
    logical cont
    integer rectx
    real(kind=KRSRC) :: rmiss

    ierr = err_default
    na = 0
    rectx = IOR(krect, REC_SUB_ALLOW)  ! force sub-record splitting

    if (ierr.eq.0) then
       cont = .TRUE.
       call get_data_record(ierr, ibagaz(0:KB_HEAD-1), KB_HEAD, u, rectx, sub=cont)
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
       if (present(napp)) then
          napp = na
       endif
       if (na.gt.0) then
          cont = .TRUE.
          if (present(kapp)) then
             ma = min(na, size(kapp))
             call get_data_record(ierr, kapp(0:ma-1), ma, u, rectx, sub=cont)
          else
             call get_data_record(ierr, kdmy, 1, u, rectx, sub=cont)
          endif
       endif
    endif
    if (ierr.eq.0) then
       nbgz = retrieve_nbgz(ibagaz)
       call get_data_record(ierr, ibagaz(KB_HEAD:KB_HEAD+nbgz-1), nbgz, u, rectx, sub=sub)
    endif

    if (ierr.eq.0) then
       ncnz = retrieve_ncnz(ibagaz)
       if (ncnz.gt.n) then
          ierr = -1
       else
          rmiss = real(vmiss, kind=KRSRC)
          call decode_alloc(ierr, d, ibagaz, ncnz, rmiss, kcode)
       endif
       m = ncnz
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
!!!_  & extract_urt_options
  subroutine extract_urt_options &
       & (ierr, &
       &  mbits, xbits, xtop, xbtm, nbreak, kcode, mlim, &
       &  kaxs,  kopts)
    use TOUZA_Trp,only: XNOTOP, XNOBTM
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(inout)       :: mbits
    integer,intent(out)         :: xbits, xtop, xbtm
    integer,intent(out)         :: nbreak
    integer,intent(inout)       :: kcode
    integer,intent(in)          :: mlim
    integer,intent(in)          :: kaxs(*)
    integer,intent(in),optional :: kopts(:)

    integer,parameter :: lo = lopts
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
    nbreak = ko(PROP_URT_BREAK)
    if (nbreak.eq.GFMT_URT_BREAK_LEVEL) then
       if (kaxs(3).eq.1) then
          if (kaxs(1).eq.1) then
             nbreak = kaxs(2)
          else
             nbreak = kaxs(1)
          endif
       else
          nbreak = kaxs(1) * kaxs(2)
       endif
    else if (nbreak.lt.0) then
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    else if (nbreak.eq.0) then
       nbreak = kaxs(1) * kaxs(2) * kaxs(3)
    endif
    ! to do: nbreak limiter

    return
  end subroutine extract_urt_options

!!!_  & set_urt_defs
  subroutine set_urt_defs (kopts)
    implicit none
    integer,intent(out) :: kopts(:)
    kopts(:) = PROP_DEFAULT
    kopts(PROP_URT_BREAK) = GFMT_URT_BREAK_LEVEL
    return
  end subroutine set_urt_defs

!!!_  & parse_urt_options
  subroutine parse_urt_options &
       & (ierr, kopts, &
       &  str,  pat,   isep,  vsep,  psep)
    use TOUZA_Nio_std,only: choice_a, split_list, parse_number
    use TOUZA_Trp,only: parse_codes, helper_props
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: kopts(*)
    character(len=*),intent(in)          :: str
    integer,         intent(in),optional :: pat
    character(len=*),intent(in),optional :: isep, vsep, psep
    character(len=4) :: spi, spv, spp
    integer             lsi, lsv, lsp
    integer jp, jb, je
    integer lstr
    integer m
    integer,parameter :: lo = lopts

    integer,parameter :: KBUF = KDBL
    real(kind=KDBL) :: refr(3)
    real(kind=KBUF),parameter :: rdef = -1.0_KBUF
    integer,parameter :: xdef = (- HUGE(0)) - 1
    integer,parameter :: bdef = -1

    integer nbreak
    integer mbits, xbits
    integer xlu(2)
    integer kcode

    ierr = 0
    call set_urt_defs(kopts(1:lo))

    call choice_a(spi, ',', isep)
    call choice_a(spv, '/', vsep)
    call choice_a(spp, '=', psep)
    lsi = max(1, len_trim(spi))
    lsv = max(1, len_trim(spv))
    lsp = max(1, len_trim(spp))

    refr(:) = rdef
    xlu(:) = xdef
    mbits = bdef
    xbits = bdef
    nbreak = GFMT_URT_BREAK_NONE
    kcode = 0

    lstr = len_trim(str)
    jb = 0
    do
       if (jb.ge.lstr) exit

       je = index(str(jb+1:lstr), spi(1:lsi)) + jb
       if (je.eq.jb) je = lstr + 1
       if (jb.lt.je) then
          jp = index(str(jb+1:je-1), spp(1:lsp)) + jb
          if (jp.eq.jb) then
             ierr = _ERROR(ERR_INVALID_PARAMETER)
             exit
          endif
          ! write(*, *) jb, je, jp, str(jb+1:jp-1), ' / ', str(jp+lsp:je-1)
          select case (str(jb+1:jp-1))
          case('r', 'R')
             call split_list(m, refr(1:3), str(jp+lsp:je-1), spv(1:lsv), empty=.TRUE.)
             ! write(*, *) refr
             ierr = min(m, 0)
          case('m', 'M')
             call parse_number(ierr, mbits, str(jp+lsp:je-1), def=bdef)
          case('x', 'X')
             call parse_number(ierr, xbits, str(jp+lsp:je-1), def=bdef)
          case('e', 'E')
             call split_list(m, xlu(1:2), str(jp+lsp:je-1), spv(1:lsv), empty=.TRUE.)
             ierr = min(m, 0)
          case('c', 'C')
             call parse_codes(ierr, kcode, str(jp+lsp:je-1))
          case('b', 'B')
             select case(str(jp+lsp:je-1))
             case('n', 'N')
                nbreak = GFMT_URT_BREAK_NONE
             case('l', 'L')
                nbreak = GFMT_URT_BREAK_LEVEL
             case('p', 'P')
                nbreak = GFMT_URT_BREAK_PROC
             case('b', 'B')
                nbreak = GFMT_URT_BREAK_LPROC
             case default
                call parse_number(ierr, nbreak, str(jp+lsp:je-1), def=GFMT_URT_BREAK_NONE)
             end select
          case default
          end select
       endif
       if (ierr.ne.0) exit
       jb = je + lsi - 1
    enddo
    ! write(*, *) jb, je, ierr, kcode, nbreak, mbits, xbits, xlu, refr
    if (ierr.eq.0) then
       kopts(PROP_URT_CODES) = kcode
       kopts(PROP_URT_BREAK) = nbreak

       if (xlu(1).gt.xdef) kopts(PROP_URT_XBOTTOM)  = xlu(1)
       if (xlu(2).gt.xdef) kopts(PROP_URT_XTOP)     = xlu(2)
       if (xbits.gt.bdef)  kopts(PROP_URT_XBITS)    = xbits
       if (mbits.gt.bdef)  kopts(PROP_URT_MANTISSA) = mbits

       if (refr(1).gt.0.0_KBUF) then
          refr(2) = max(refr(2), refr(1))
          call helper_props(mbits, xbits, xlu(1), refr(2), refr(1), refr(3))
          kopts(PROP_URT_XBOTTOM)  = xlu(1)
          kopts(PROP_URT_XBITS)    = xbits
          kopts(PROP_URT_MANTISSA) = mbits
       endif
    endif
  end subroutine parse_urt_options
!!!_  - show_urt_options
  subroutine show_urt_options(ierr, kopts, tag, u)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: kopts(*)
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    character(len=128) :: buf
    integer utmp
    ierr = 0
    utmp = get_logu(u, ulog)
101 format('m', I0, 'e', I0, 1x, I0, ':', I0)
102 format(A, ': ', 'm', I0, 'e', I0, 1x, I0, ':', I0)
    if (present(tag)) then
       write(buf, 102) trim(tag), &
            & kopts(PROP_URT_MANTISSA), kopts(PROP_URT_XBITS), &
            & kopts(PROP_URT_XBOTTOM), kopts(PROP_URT_XTOP)
    else
       write(buf, 101) &
            & kopts(PROP_URT_MANTISSA), kopts(PROP_URT_XBITS), &
            & kopts(PROP_URT_XBOTTOM), kopts(PROP_URT_XTOP)
    endif
201 format(A)
    if (utmp.ge.0) then
       write(utmp, 201) trim(buf)
    else if (utmp.eq.-1) then
       write(*,    201) trim(buf)
    endif
  end subroutine show_urt_options

!!!_ + gtool extenstion (P[IR]n)
!!!_  - review_ptn - review P[RI]n data
  subroutine review_ptn &
       & (ierr,  ndata, &
       &  head,  u,     krect, ends, flag)
    use TOUZA_Nio_std,only: KIOFS, sus_getpos, sus_rseek, WHENCE_ABS
    use TOUZA_Nio_std,only: choice
    use TOUZA_Trp,    only: count_packed
    implicit none
    integer,parameter :: KISRC=KI32
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ndata       ! (physical) size of data part
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    integer,optional,intent(out) :: ends(*)
    integer,optional,intent(in)  :: flag
    integer nk

    ierr = 0
    nk = parse_header_size(head, packed_ends_coordinate)
    call review_ptn_core(ierr, ndata, nk, u, krect, ends, flag)
  end subroutine review_ptn
!!!_  - review_ptn_core - review P[RI]n data
  subroutine review_ptn_core &
       & (ierr,  ndata, &
       &  mk,    u,     krect, ends, flag)
    use TOUZA_Nio_std,only: KIOFS, sus_getpos, sus_rseek, WHENCE_ABS
    use TOUZA_Nio_std,only: choice
    use TOUZA_Trp,    only: count_packed
    implicit none
    integer,parameter :: KISRC=KI32
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ndata       ! (physical) size of data part
    integer,         intent(in)  :: mk
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    integer,optional,intent(out) :: ends(*)
    integer,optional,intent(in)  :: flag

    integer(kind=KIOFS) :: apini
    integer f
    logical cont
    integer,parameter :: nfil = 2
    integer :: rfil(0:nfil)
    integer :: d(1)

    ierr = 0
    ndata = -1
    ! note: position must be just after gtool header part
    f = choice(-1, flag)
    if (f.lt.0) then
       if (present(ends)) then
          f = 0
       else
          f = rev_pos_dhead
       endif
    endif
    if (ierr.eq.0) call pre_review(ierr, apini, u, f)
    cont = .TRUE.
    if (present(ends)) then
       if (ierr.eq.0) call get_data_record(ierr, ends(1:mk), mk, u, krect, sub=cont)
       if (ierr.eq.0) ndata = ends(mk)
    else
       if (ierr.eq.0) then
          rfil(0) = mk - 1
          rfil(1) = 1
          rfil(2) = 0
       endif
       if (ierr.eq.0) call get_data_record_runl(ierr, d, 1, u, krect, rfil, nfil, sub=cont)
       if (ierr.eq.0) ndata = d(1)
    endif
    if (ierr.eq.0) call post_review(ierr, apini, u, f)
  end subroutine review_ptn_core

!!!_  - restore_pr8_plain_d - restore PR8 data to plain (full) array
  ! subroutine restore_pr8_plain_d &
  !      & (ierr, &
  !      &  d,    ldata, u, krect, vmiss, mfull, subv, bes, nr)
  !   use TOUZA_Trp,only: count_packed, pack_restore
  !   use TOUZA_Trp,only: mask_to_idxl
  !   implicit none
  !   integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
  !   integer,         intent(out) :: ierr
  !   real(kind=KARG), intent(out) :: d(0:*)
  !   integer,         intent(in)  :: ldata        ! limit size of of d
  !   integer,         intent(in)  :: u
  !   integer,         intent(in)  :: krect
  !   real(kind=KRMIS),intent(in)  :: vmiss
  !   integer,         intent(in)  :: mfull        ! (logical) full data size
  !   integer,optional,intent(in)  :: subv(0:*)
  !   integer,optional,intent(in)  :: bes(3, *)
  !   integer,optional,intent(in)  :: nr

  ! end subroutine restore_pr8_plain_d

!!!_  - restore_ptn_check
  subroutine restore_ptn_check &
       & (ierr,  &
       &  ndata, ends, u, krect, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: ndata
    integer,         intent(inout) :: ends(0:*)
    integer,         intent(in)    :: u
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check

    integer mo, ms
    integer mfk
    integer ch, ci
    logical cont

    ierr = 0
    ndata = 0
    ch = choice(packed_read, check)
    ci = choice(0, citer)
    mfk = max(1, product(max(1, kaxs(packed_ends_coordinate:laxs))))
    cont = .TRUE.

    select case(ch)
    case(packed_check)
       if (ierr.eq.0) call alloc_wsubv(ierr, mfk, 'res:ptn')
       if (ierr.eq.0) then
          call review_ptn_core(ierr, ndata, mfk, u, krect, ends=wssubv(0:mfk-1), flag=rev_pos_leave)
       endif
       if (ierr.eq.0) then
          ci = choice(0, citer)
          if (ci.le.0.or.ci.gt.laxs) then
             mo = 1
          else
             mo = max(1, product(max(1, kaxs(ci:laxs))))
          endif
          if (mfk.eq.mo) then
             if (ANY(ends(0:mo-1).ne.wssubv(0:mfk-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          else if (mfk.gt.mo) then
             if (mod(mfk, mo).eq.0) then
                ms = mfk / mo
                if (ANY(ends(0:mo-1).ne.wssubv(ms-1:mfk-1:ms))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
             else
                ierr = _ERROR(ERR_INVALID_PARAMETER)
             endif
          else
             if (mod(mo, mfk).eq.0) then
                ms = mo / mfk
                if (ANY(ends(ms-1:mo-1:ms).ne.wssubv(0:mfk-1))) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
             else
                ierr = _ERROR(ERR_INVALID_PARAMETER)
             endif
          endif
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:ptn')
    case(packed_read)
       !!! must adjust by later call
       if (ierr.eq.0) then
          call get_data_irecord(ierr, ends(0:mfk-1), mfk, u, krect, sub=cont)
       endif
       if (ierr.eq.0) ndata = ends(mfk-1)
    case(packed_ignore)
       if (ierr.eq.0) &
            & call review_ptn_core(ierr, ndata, mfk, u, krect, flag=rev_pos_leave)
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select

  end subroutine restore_ptn_check
!!!_  - restore_ptn_packed_subv - restore P[IR]n data to subscript vector
  subroutine restore_ptn_packed_subv &
       & (ierr, &
       &  subv, ldata, ends, u, krect, kfmt, kaxs, citer)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: subv(0:*)
    integer,         intent(in)  :: ldata        ! limit size of of d
    integer,         intent(out) :: ends(0:*)
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: kfmt
    integer,         intent(in)  :: kaxs(*)
    integer,optional,intent(in)  :: citer        ! iteration index

    integer mi, mo, ci
    integer mfh, mfk

    ierr = 0

    ci = choice(0, citer)
    if (ci.le.0.or.ci.gt.laxs) then
       mo = 1
       mi = product(max(1, kaxs(1:laxs)))
    else
       mo = max(1, product(max(1, kaxs(ci:laxs))))
       mi = max(1, product(max(1, kaxs(1:ci-1))))
    endif

    mfh = max(1, product(max(1, kaxs(1:packed_ends_coordinate-1))))
    mfk = max(1, product(max(1, kaxs(packed_ends_coordinate:laxs))))

    if (mfk.eq.mo) then
       if (ierr.eq.0) then
          select case (kfmt)
          case (GFMT_PI4, GFMT_PI4SV)
             call restore_pi4_packed &
                  & (ierr, subv, ldata, ends, u, krect, kaxs, citer, packed_read)
          case (GFMT_PR4)
             call restore_pr4_packed &
                  & (ierr, subv, ldata, ends, u, krect, kaxs, citer, packed_read)
          case (GFMT_PR8)
             call restore_pr8_packed &
                  & (ierr, subv, ldata, ends, u, krect, kaxs, citer, packed_read)
          case default
             ierr = _ERROR(ERR_INVALID_SWITCH)
          end select
       endif
    else
       if (ierr.eq.0) call alloc_wsubv(ierr, mfk, 'res:ptn-subv')
       !! [CAUTION] wssubv confliction never occurs when packed_read
       if (ierr.eq.0) then
          select case (kfmt)
          case (GFMT_PI4, GFMT_PI4SV)
             call restore_pi4_packed &
                  & (ierr, subv, ldata, wssubv, u, krect, kaxs, citer, packed_read)
          case (GFMT_PR4)
             call restore_pr4_packed &
                  & (ierr, subv, ldata, wssubv, u, krect, kaxs, citer, packed_read)
          case (GFMT_PR8)
             call restore_pr8_packed &
                  & (ierr, subv, ldata, wssubv, u, krect, kaxs, citer, packed_read)
          case default
             ierr = _ERROR(ERR_INVALID_SWITCH)
          end select
       endif
       if (ierr.eq.0) then
          call tweak_subv(ierr, subv, ends, mi, mo, wssubv, mfh, mfk)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, 'res:ptn-subv')
    endif
  end subroutine restore_ptn_packed_subv

!!!_  - restore_ptn_plain_subv_i - restore P[IR]n subscript vector to plain
  subroutine restore_ptn_plain_subv_i &
       & (ierr, &
       &  d, ldata, u, krect, vmiss, kfmt, kaxs)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: d(0:*)
    integer,         intent(in)  :: ldata
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt
    integer,         intent(in)  :: kaxs(*)
    integer vmt
    integer ci
    integer jfi, jfk
    integer nd
    integer mfh, mfk
    integer jvb, jve, jv
    integer jd
#define __PROC__ 'restore_ptn_plain_subv_i'

    ierr = 0
    vmt = int(vmiss)
    ci = packed_ends_coordinate
    mfk = max(1, product(max(1, kaxs(packed_ends_coordinate:laxs))))
    mfh = max(1, product(max(1, kaxs(1:packed_ends_coordinate-1))))
    if (ierr.eq.0) call alloc_wsubv(ierr, mfk, __PROC__)
    if (ierr.eq.0) then
       call review_ptn_core(ierr, nd, mfk, u, krect, ends=wssubv(0:mfk-1), flag=rev_pos_leave)
    endif
    if (ierr.eq.0) call alloc_worki(ierr, nd,  __PROC__)
    if (ierr.eq.0) call get_data_irecord(ierr, worki(0:nd-1), nd, u, krect)
    if (ierr.eq.0) then
       d(0:ldata-1) = vmt
       do jfk = 0, mfk - 1
          jvb = wssubv(jfk)
          jve = wssubv(jfk+1)
          do jv = jvb, jve - 1
             jd = mfh * jfk + worki(jv)
             d(jd) = jv
          enddo
       enddo
    endif
    if (ierr.eq.0) call alloc_worki(ierr, -1,  __PROC__)
    if (ierr.eq.0) call alloc_wsubv(ierr, -1,  __PROC__)
#undef __PROC__
  end subroutine restore_ptn_plain_subv_i
  subroutine restore_ptn_plain_subv_d &
       & (ierr, &
       &  d, ldata, u, krect, vmiss, kfmt, kaxs)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: ldata
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt
    integer,         intent(in)  :: kaxs(*)
    integer vmt
    integer ci
    integer jfi, jfk
    integer nd
    integer mfh, mfk
    integer jvb, jve, jv
    integer jd
#define __PROC__ 'restore_ptn_plain_subv_d'

    ierr = 0
    vmt = int(vmiss)
    ci = packed_ends_coordinate
    mfk = max(1, product(max(1, kaxs(packed_ends_coordinate:laxs))))
    mfh = max(1, product(max(1, kaxs(1:packed_ends_coordinate-1))))
    if (ierr.eq.0) call alloc_wsubv(ierr, mfk, __PROC__)
    if (ierr.eq.0) then
       call review_ptn_core(ierr, nd, mfk, u, krect, ends=wssubv(0:mfk-1), flag=rev_pos_leave)
    endif
    if (ierr.eq.0) call alloc_worki(ierr, nd,  __PROC__)
    if (ierr.eq.0) call get_data_irecord(ierr, worki(0:nd-1), nd, u, krect)
    if (ierr.eq.0) then
       d(0:ldata-1) = vmt
       do jfk = 0, mfk - 1
          jvb = wssubv(jfk)
          jve = wssubv(jfk+1)
          do jv = jvb, jve - 1
             jd = mfh * jfk + worki(jv)
             d(jd) = real(jv, kind=KARG)
          enddo
       enddo
    endif
    if (ierr.eq.0) call alloc_worki(ierr, -1,  __PROC__)
    if (ierr.eq.0) call alloc_wsubv(ierr, -1,  __PROC__)
#undef __PROC__
  end subroutine restore_ptn_plain_subv_d
  subroutine restore_ptn_plain_subv_f &
       & (ierr, &
       &  d, ldata, u, krect, vmiss, kfmt, kaxs)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: ldata
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt
    integer,         intent(in)  :: kaxs(*)
    integer vmt
    integer ci
    integer jfi, jfk
    integer nd
    integer mfh, mfk
    integer jvb, jve, jv
    integer jd
#define __PROC__ 'restore_ptn_plain_subv_d'

    ierr = 0
    vmt = int(vmiss)
    ci = packed_ends_coordinate
    mfk = max(1, product(max(1, kaxs(packed_ends_coordinate:laxs))))
    mfh = max(1, product(max(1, kaxs(1:packed_ends_coordinate-1))))
    if (ierr.eq.0) call alloc_wsubv(ierr, mfk, __PROC__)
    if (ierr.eq.0) then
       call review_ptn_core(ierr, nd, mfk, u, krect, ends=wssubv(0:mfk-1), flag=rev_pos_leave)
    endif
    if (ierr.eq.0) call alloc_worki(ierr, nd,  __PROC__)
    if (ierr.eq.0) call get_data_irecord(ierr, worki(0:nd-1), nd, u, krect)
    if (ierr.eq.0) then
       d(0:ldata-1) = vmt
       do jfk = 0, mfk - 1
          jvb = wssubv(jfk)
          jve = wssubv(jfk+1)
          do jv = jvb, jve - 1
             jd = mfh * jfk + worki(jv)
             d(jd) = real(jv, kind=KARG)
          enddo
       enddo
    endif
    if (ierr.eq.0) call alloc_worki(ierr, -1,  __PROC__)
    if (ierr.eq.0) call alloc_wsubv(ierr, -1,  __PROC__)
#undef __PROC__
  end subroutine restore_ptn_plain_subv_f

!!!_  - restore_pr8_packed_d - restore PR8 data to packed array
  subroutine restore_pr8_packed_d &
       & (ierr, &
       &  d,    ldata, ends, u, krect, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out)   :: ierr
    real(kind=KARG), intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: ends(0:*)
    integer,         intent(in)    :: u
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check

    integer l

    ierr = 0
    call restore_ptn_check(ierr, l, ends, u, krect, kaxs, citer, check)
    if (ierr.eq.0) then
       if (ldata.ge.0.and.ldata.lt.l) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
    if (ierr.eq.0) call get_data_drecord(ierr, d(0:l-1), l, u, krect)
  end subroutine restore_pr8_packed_d
  subroutine restore_pr8_packed_f &
       & (ierr, &
       &  d,    ldata, ends, u, krect, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out)   :: ierr
    real(kind=KARG),    intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: ends(0:*)
    integer,         intent(in)    :: u
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check

    integer l

    ierr = 0
    call restore_ptn_check(ierr, l, ends, u, krect, kaxs, citer, check)
    if (ierr.eq.0) then
       if (ldata.ge.0.and.ldata.lt.l) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
    if (ierr.eq.0) call get_data_drecord(ierr, d(0:l-1), l, u, krect)
  end subroutine restore_pr8_packed_f
  subroutine restore_pr8_packed_i &
       & (ierr, &
       &  d,    ldata, ends, u, krect, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out)   :: ierr
    integer(kind=KARG),    intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: ends(0:*)
    integer,         intent(in)    :: u
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check

    integer l

    ierr = 0
    call restore_ptn_check(ierr, l, ends, u, krect, kaxs, citer, check)
    if (ierr.eq.0) then
       if (ldata.ge.0.and.ldata.lt.l) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
    if (ierr.eq.0) call get_data_drecord(ierr, d(0:l-1), l, u, krect)
  end subroutine restore_pr8_packed_i
!!!_  - restore_pr4_packed_d - restore PR4 data to packed array
  subroutine restore_pr4_packed_d &
       & (ierr, &
       &  d,    ldata, ends, u, krect, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out)   :: ierr
    real(kind=KARG), intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: ends(0:*)
    integer,         intent(in)    :: u
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check

    integer l

    ierr = 0
    call restore_ptn_check(ierr, l, ends, u, krect, kaxs, citer, check)
    if (ierr.eq.0) then
       if (ldata.ge.0.and.ldata.lt.l) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
    if (ierr.eq.0) call get_data_frecord(ierr, d(0:l-1), l, u, krect)
  end subroutine restore_pr4_packed_d
  subroutine restore_pr4_packed_f &
       & (ierr, &
       &  d,    ldata, ends, u, krect, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out)   :: ierr
    real(kind=KARG),    intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: ends(0:*)
    integer,         intent(in)    :: u
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check

    integer l

    ierr = 0
    call restore_ptn_check(ierr, l, ends, u, krect, kaxs, citer, check)
    if (ierr.eq.0) then
       if (ldata.ge.0.and.ldata.lt.l) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
    if (ierr.eq.0) call get_data_frecord(ierr, d(0:l-1), l, u, krect)
  end subroutine restore_pr4_packed_f
  subroutine restore_pr4_packed_i &
       & (ierr, &
       &  d,    ldata, ends, u, krect, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out)   :: ierr
    integer(kind=KARG),    intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: ends(0:*)
    integer,         intent(in)    :: u
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check

    integer l

    ierr = 0
    call restore_ptn_check(ierr, l, ends, u, krect, kaxs, citer, check)
    if (ierr.eq.0) then
       if (ldata.ge.0.and.ldata.lt.l) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
    if (ierr.eq.0) call get_data_frecord(ierr, d(0:l-1), l, u, krect)
  end subroutine restore_pr4_packed_i
!!!_  - restore_pi4_packed_d - restore PI4 data to packed array
  subroutine restore_pi4_packed_d &
       & (ierr, &
       &  d,    ldata, ends, u, krect, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32
    integer,         intent(out)   :: ierr
    real(kind=KARG), intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: ends(0:*)
    integer,         intent(in)    :: u
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check

    integer l

    ierr = 0
    call restore_ptn_check(ierr, l, ends, u, krect, kaxs, citer, check)
    if (ierr.eq.0) then
       if (ldata.ge.0.and.ldata.lt.l) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
    if (ierr.eq.0) call get_data_irecord(ierr, d(0:l-1), l, u, krect)
  end subroutine restore_pi4_packed_d
  subroutine restore_pi4_packed_f &
       & (ierr, &
       &  d,    ldata, ends, u, krect, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32
    integer,         intent(out)   :: ierr
    real(kind=KARG),    intent(out)   :: d(0:*)
    integer,         intent(in)    :: ldata        ! limit size of of d
    integer,         intent(inout) :: ends(0:*)
    integer,         intent(in)    :: u
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: kaxs(*)
    integer,optional,intent(in)    :: citer        ! iteration index
    integer,optional,intent(in)    :: check

    integer l

    ierr = 0
    call restore_ptn_check(ierr, l, ends, u, krect, kaxs, citer, check)
    if (ierr.eq.0) then
       if (ldata.ge.0.and.ldata.lt.l) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
    if (ierr.eq.0) call get_data_irecord(ierr, d(0:l-1), l, u, krect)
  end subroutine restore_pi4_packed_f
  subroutine restore_pi4_packed_i &
       & (ierr, &
       &  d,    ldata, ends, u, krect, kaxs, citer, check)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32
    integer,           intent(out)   :: ierr
    integer(kind=KARG),intent(out)   :: d(0:*)
    integer,           intent(in)    :: ldata        ! limit size of of d
    integer,           intent(inout) :: ends(0:*)
    integer,           intent(in)    :: u
    integer,           intent(in)    :: krect
    integer,           intent(in)    :: kaxs(*)
    integer,optional,  intent(in)    :: citer        ! iteration index
    integer,optional,  intent(in)    :: check

    integer l

    ierr = 0
    call restore_ptn_check(ierr, l, ends, u, krect, kaxs, citer, check)
    if (ierr.eq.0) then
       if (ldata.ge.0.and.ldata.lt.l) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
    if (ierr.eq.0) call get_data_irecord(ierr, d(0:l-1), l, u, krect)
  end subroutine restore_pi4_packed_i

!!!_  - get_data_pr8 - PR8: list-based packed array
  subroutine get_data_pr8_d &
       & (ierr, &
       &  d, n, u, krect, nk, ofs)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: nk
    integer,optional,intent(out) :: ofs(:)
    integer l
    integer o(nk)
    logical cont
    ierr = 0
    if (ierr.eq.0) then
       cont = .TRUE.
       call get_data_record &
            & (ierr, o(1:nk), nk, u, krect, sub=cont)
       l = o(nk)
       if (present(ofs)) then
          ofs(1:nk) = o(1:nk)
       endif
    endif
    if (ierr.eq.0) then
       if (n.ge.0.and.n.lt.l) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
    if (ierr.eq.0) then
       call get_data_record &
            & (ierr, d(0:l-1), l, u, krect)
    endif
  end subroutine get_data_pr8_d
  subroutine get_data_pr8_f &
       & (ierr, &
       &  d, n, u, krect, nk, ofs)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: nk
    integer,optional,intent(out) :: ofs(:)
    integer l
    integer o(nk)
    logical cont
    ierr = 0
    if (ierr.eq.0) then
       cont = .TRUE.
       call get_data_record &
            & (ierr, o(1:nk), nk, u, krect, sub=cont)
       l = o(nk)
       if (present(ofs)) then
          ofs(1:nk) = o(1:nk)
       endif
    endif
    if (ierr.eq.0) then
       if (n.ge.0.and.n.lt.l) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
    if (ierr.eq.0) then
       call get_data_drecord &
            & (ierr, d(0:l-1), l, u, krect)
    endif
  end subroutine get_data_pr8_f
  subroutine get_data_pr8_i &
       & (ierr, &
       &  d, n, u, krect, nk, ofs)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: nk
    integer,optional,intent(out) :: ofs(:)
    integer l
    integer o(nk)
    logical cont
    ierr = 0
    if (ierr.eq.0) then
       cont = .TRUE.
       call get_data_record &
            & (ierr, o(1:nk), nk, u, krect, sub=cont)
       l = o(nk)
       if (present(ofs)) then
          ofs(1:nk) = o(1:nk)
       endif
    endif
    if (ierr.eq.0) then
       if (n.ge.0.and.n.lt.l) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
    if (ierr.eq.0) then
       call get_data_drecord &
            & (ierr, d(0:l-1), l, u, krect)
    endif
  end subroutine get_data_pr8_i
!!!_  - get_data_pr4 - PR8: list-based packed array
  subroutine get_data_pr4_d &
       & (ierr, &
       &  d, n, u, krect, nk, ofs)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: nk
    integer,optional,intent(out) :: ofs(:)
    integer l
    integer o(nk)
    logical cont
    ierr = 0
    if (ierr.eq.0) then
       cont = .TRUE.
       call get_data_record &
            & (ierr, o(1:nk), nk, u, krect, sub=cont)
       l = o(nk)
       if (present(ofs)) then
          ofs(1:nk) = o(1:nk)
       endif
    endif
    if (ierr.eq.0) then
       if (n.ge.0.and.n.lt.l) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
    if (ierr.eq.0) then
       call get_data_frecord &
            & (ierr, d(0:l-1), l, u, krect)
    endif
  end subroutine get_data_pr4_d
  subroutine get_data_pr4_f &
       & (ierr, &
       &  d, n, u, krect, nk, ofs)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: nk
    integer,optional,intent(out) :: ofs(:)
    integer l
    integer o(nk)
    logical cont
    ierr = 0
    if (ierr.eq.0) then
       cont = .TRUE.
       call get_data_record &
            & (ierr, o(1:nk), nk, u, krect, sub=cont)
       l = o(nk)
       if (present(ofs)) then
          ofs(1:nk) = o(1:nk)
       endif
    endif
    if (ierr.eq.0) then
       if (n.ge.0.and.n.lt.l) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
    if (ierr.eq.0) then
       call get_data_record &
            & (ierr, d(0:l-1), l, u, krect)
    endif
  end subroutine get_data_pr4_f
  subroutine get_data_pr4_i &
       & (ierr, &
       &  d, n, u, krect, nk, ofs)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: nk
    integer,optional,intent(out) :: ofs(:)
    integer l
    integer o(nk)
    logical cont
    ierr = 0
    if (ierr.eq.0) then
       cont = .TRUE.
       call get_data_record &
            & (ierr, o(1:nk), nk, u, krect, sub=cont)
       l = o(nk)
       if (present(ofs)) then
          ofs(1:nk) = o(1:nk)
       endif
    endif
    if (ierr.eq.0) then
       if (n.ge.0.and.n.lt.l) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
    if (ierr.eq.0) then
       call get_data_frecord &
            & (ierr, d(0:l-1), l, u, krect)
    endif
  end subroutine get_data_pr4_i
!!!_  - get_data_pi4 - PR8: list-based packed array
  subroutine get_data_pi4_d &
       & (ierr, &
       &  d, n, u, krect, nk, ofs)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: nk
    integer,optional,intent(out) :: ofs(:)
    integer l
    integer o(nk)
    logical cont
    ierr = 0
    if (ierr.eq.0) then
       cont = .TRUE.
       call get_data_record &
            & (ierr, o(1:nk), nk, u, krect, sub=cont)
       l = o(nk)
       if (present(ofs)) then
          ofs(1:nk) = o(1:nk)
       endif
    endif
    if (ierr.eq.0) then
       if (n.ge.0.and.n.lt.l) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
    if (ierr.eq.0) then
       call get_data_irecord &
            & (ierr, d(0:l-1), l, u, krect)
    endif
  end subroutine get_data_pi4_d
  subroutine get_data_pi4_f &
       & (ierr, &
       &  d, n, u, krect, nk, ofs)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: nk
    integer,optional,intent(out) :: ofs(:)
    integer l
    integer o(nk)
    logical cont
    ierr = 0
    if (ierr.eq.0) then
       cont = .TRUE.
       call get_data_record &
            & (ierr, o(1:nk), nk, u, krect, sub=cont)
       l = o(nk)
       if (present(ofs)) then
          ofs(1:nk) = o(1:nk)
       endif
    endif
    if (ierr.eq.0) then
       if (n.ge.0.and.n.lt.l) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
    if (ierr.eq.0) then
       call get_data_irecord &
            & (ierr, d(0:l-1), l, u, krect)
    endif
  end subroutine get_data_pi4_f
  subroutine get_data_pi4_i &
       & (ierr, &
       &  d, n, u, krect, nk, ofs)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: nk
    integer,optional,intent(out) :: ofs(:)
    integer l
    integer o(nk)
    logical cont
    ierr = 0
    if (ierr.eq.0) then
       cont = .TRUE.
       call get_data_record &
            & (ierr, o(1:nk), nk, u, krect, sub=cont)
       l = o(nk)
       if (present(ofs)) then
          ofs(1:nk) = o(1:nk)
       endif
    endif
    if (ierr.eq.0) then
       if (n.ge.0.and.n.lt.l) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    endif
    if (ierr.eq.0) then
       call get_data_record &
            & (ierr, d(0:l-1), l, u, krect)
    endif
  end subroutine get_data_pi4_i

!!!_  - put_data_pr8 - PR8: list-based packed array
  subroutine put_data_pr8_d &
       & (ierr, &
       &  d, n, u, krect, nk, ofs)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: nk
    integer,optional,intent(in)  :: ofs(:)
    integer l
    integer o(nk)
    ierr = 0
    if (present(ofs)) then
       call put_data_record &
            & (ierr, ofs(1:nk), nk, u, krect, pre=.FALSE., post=.TRUE.)
       l = ofs(nk)
    else
       o(1:nk-1) = 0
       o(nk) = n
       l = n
       call put_data_record &
            & (ierr, o(1:nk), nk, u, krect, pre=.FALSE., post=.TRUE.)
    endif
    if (ierr.eq.0) then
       call put_data_record &
            & (ierr, d(0:l-1), l, u, krect, pre=.TRUE., post=.FALSE.)
    endif
  end subroutine put_data_pr8_d

  subroutine put_data_pr8_f &
       & (ierr, &
       &  d, n, u, krect, nk, ofs)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: nk
    integer,optional,intent(in)  :: ofs(:)
    integer l
    integer o(nk)
    ierr = 0
    if (present(ofs)) then
       call put_data_record &
            & (ierr, ofs(1:nk), nk, u, krect, pre=.FALSE., post=.TRUE.)
       l = ofs(nk)
    else
       o(1:nk-1) = 0
       o(nk) = n
       l = n
       call put_data_record &
            & (ierr, o(1:nk), nk, u, krect, pre=.FALSE., post=.TRUE.)
    endif
    if (ierr.eq.0) then
       call put_data_drecord &
            & (ierr, d(0:l-1), l, u, krect, pre=.TRUE., post=.FALSE.)
    endif
  end subroutine put_data_pr8_f
  subroutine put_data_pr8_i &
       & (ierr, &
       &  d, n, u, krect, nk, ofs)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: nk
    integer,optional,intent(in)  :: ofs(:)
    integer l
    integer o(nk)
    ierr = 0
    if (present(ofs)) then
       call put_data_record &
            & (ierr, ofs(1:nk), nk, u, krect, pre=.FALSE., post=.TRUE.)
       l = ofs(nk)
    else
       o(1:nk-1) = 0
       o(nk) = n
       l = n
       call put_data_record &
            & (ierr, o(1:nk), nk, u, krect, pre=.FALSE., post=.TRUE.)
    endif
    if (ierr.eq.0) then
       call put_data_frecord &
            & (ierr, d(0:l-1), l, u, krect, pre=.TRUE., post=.FALSE.)
    endif
  end subroutine put_data_pr8_i

!!!_  - put_data_pr4 - PR4: list-based packed array
  subroutine put_data_pr4_f &
       & (ierr, &
       &  d, n, u, krect, nk, ofs)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: nk
    integer,optional,intent(in)  :: ofs(:)
    integer l
    integer o(nk)
    ierr = 0
    if (present(ofs)) then
       call put_data_record &
            & (ierr, ofs(1:nk), nk, u, krect, pre=.FALSE., post=.TRUE.)
       l = ofs(nk)
    else
       o(1:nk-1) = 0
       o(nk) = n
       l = n
       call put_data_record &
            & (ierr, o(1:nk), nk, u, krect, pre=.FALSE., post=.TRUE.)
    endif
    if (ierr.eq.0) then
       call put_data_record &
            & (ierr, d(0:l-1), l, u, krect, pre=.TRUE., post=.FALSE.)
    endif
  end subroutine put_data_pr4_f
  subroutine put_data_pr4_d &
       & (ierr, &
       &  d, n, u, krect, nk, ofs)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: nk
    integer,optional,intent(in)  :: ofs(:)
    integer l
    integer o(nk)
    ierr = 0
    if (present(ofs)) then
       call put_data_record &
            & (ierr, ofs(1:nk), nk, u, krect, pre=.FALSE., post=.TRUE.)
       l = ofs(nk)
    else
       o(1:nk-1) = 0
       o(nk) = n
       l = n
       call put_data_record &
            & (ierr, o(1:nk), nk, u, krect, pre=.FALSE., post=.TRUE.)
    endif
    if (ierr.eq.0) then
       call put_data_frecord &
            & (ierr, d(0:l-1), l, u, krect, pre=.TRUE., post=.FALSE.)
    endif
  end subroutine put_data_pr4_d
  subroutine put_data_pr4_i &
       & (ierr, &
       &  d, n, u, krect, nk, ofs)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: nk
    integer,optional,intent(in)  :: ofs(:)
    integer l
    integer o(nk)
    ierr = 0
    if (present(ofs)) then
       call put_data_record &
            & (ierr, ofs(1:nk), nk, u, krect, pre=.FALSE., post=.TRUE.)
       l = ofs(nk)
    else
       o(1:nk-1) = 0
       o(nk) = n
       l = n
       call put_data_record &
            & (ierr, o(1:nk), nk, u, krect, pre=.FALSE., post=.TRUE.)
    endif
    if (ierr.eq.0) then
       call put_data_frecord &
            & (ierr, d(0:l-1), l, u, krect, pre=.TRUE., post=.FALSE.)
    endif
  end subroutine put_data_pr4_i

!!!_  - put_data_pi4 - PI4: list-based packed array
  subroutine put_data_pi4_i &
       & (ierr, &
       &  d, n, u, krect, nk, ofs)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: nk
    integer,optional,intent(in)  :: ofs(:)
    integer l
    integer o(nk)
    ierr = 0
    if (present(ofs)) then
       call put_data_record &
            & (ierr, ofs(1:nk), nk, u, krect, pre=.FALSE., post=.TRUE.)
       l = ofs(nk)
    else
       o(1:nk-1) = 0
       o(nk) = n
       l = n
       call put_data_record &
            & (ierr, o(1:nk), nk, u, krect, pre=.FALSE., post=.TRUE.)
    endif
    if (ierr.eq.0) then
       call put_data_record &
            & (ierr, d(0:l-1), l, u, krect, pre=.TRUE., post=.FALSE.)
    endif
  end subroutine put_data_pi4_i
  subroutine put_data_pi4_d &
       & (ierr, &
       &  d, n, u, krect, nk, ofs)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: nk
    integer,optional,intent(in)  :: ofs(:)
    integer l
    integer o(nk)
    ierr = 0
    if (present(ofs)) then
       call put_data_record &
            & (ierr, ofs(1:nk), nk, u, krect, pre=.FALSE., post=.TRUE.)
       l = ofs(nk)
    else
       o(1:nk-1) = 0
       o(nk) = n
       l = n
       call put_data_record &
            & (ierr, o(1:nk), nk, u, krect, pre=.FALSE., post=.TRUE.)
    endif
    if (ierr.eq.0) then
       call put_data_irecord &
            & (ierr, d(0:l-1), l, u, krect, pre=.TRUE., post=.FALSE.)
    endif
  end subroutine put_data_pi4_d
  subroutine put_data_pi4_f &
       & (ierr, &
       &  d, n, u, krect, nk, ofs)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: nk
    integer,optional,intent(in)  :: ofs(:)
    integer l
    integer o(nk)
    ierr = 0
    if (present(ofs)) then
       call put_data_record &
            & (ierr, ofs(1:nk), nk, u, krect, pre=.FALSE., post=.TRUE.)
       l = ofs(nk)
    else
       o(1:nk-1) = 0
       o(nk) = n
       l = n
       call put_data_record &
            & (ierr, o(1:nk), nk, u, krect, pre=.FALSE., post=.TRUE.)
    endif
    if (ierr.eq.0) then
       call put_data_irecord &
            & (ierr, d(0:l-1), l, u, krect, pre=.TRUE., post=.FALSE.)
    endif
  end subroutine put_data_pi4_f

!!!_ + gtool extenstion (MI4)
!!!_  - put_data_mi4 - MI4
  subroutine put_data_mi4_i &
       & (ierr, &
       &  d, n, u, krect, vmiss)
    use TOUZA_Trp,only: count_packed
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(in)  :: d(0:*)
    integer,           intent(in)  :: n
    integer,           intent(in)  :: krect
    integer,           intent(in)  :: u
    real(kind=KRMIS),  intent(in)  :: vmiss
    integer(kind=KISRC) :: mb
    integer(kind=KISRC) :: icom(n), buf(n)
    integer(kind=KISRC),parameter :: mold = 0_KISRC
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

!!!_ + gtool extension MRT

!!!_ + utilities
!!!_  & get_record_prop - get sequential record properties (byte-order and separator size)
  subroutine get_record_prop &
       & (ierr, krect, u)
    use TOUZA_Nio_std,   only: &
         & KI32, KI64, KIOFS, is_eof_ss, &
         & WHENCE_ABS, sus_read_isep, sus_rseek, sus_eswap
    use TOUZA_Nio_std,   only: sus_getpos
    use TOUZA_Nio_header,only: nitem, litem
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: krect
    integer,intent(in)  :: u

    integer(kind=KIOFS) :: apos
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

    if (ierr.eq.0) call sus_getpos(ierr, apos, u)
    if (ierr.eq.0) call sus_read_isep(ierr, u, iseph)
    if (is_eof_ss(ierr)) then
       ierr = _ERROR(ERR_EOF)
       return
    endif
    if (ierr.eq.0) call sus_read_isep(ierr, u, isepl)
    if (ierr.eq.0) then
       if (iseph.eq.0) then
          ! [00 00 00 00] [00 00 04 00]  long/big
          if (isepl.lt.HEADER_LIMIT) then
             KRECT = IOR(KRECT, REC_LSEP) ! long native
             nlh = isepl
          else
             KRECT = IOR(IOR(KRECT, REC_LSEP), REC_SWAP) ! long swap
             nlh = sus_eswap(isepl)
          endif
       else if (isepl.eq.0) then
          ! [00 04 00 00] [00 00 00 00]  long/little
          if (iseph.lt.HEADER_LIMIT) then
             KRECT = IOR(KRECT, REC_LSEP) ! long native
             nlh = iseph
          else
             KRECT = IOR(IOR(KRECT, REC_LSEP), REC_SWAP) ! long swap
             nlh = sus_eswap(iseph)
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
             nlh = abs(sus_eswap(iseph))
          endif
       else
          nlh = nlhead_std - 1
       endif
       call sus_rseek(ierr, u, apos, whence=WHENCE_ABS)
       if (nlh.lt.nlhead_std .or. nlh.ge.HEADER_LIMIT) KRECT = REC_ERROR
       ! write(*, *) 'SEP', iseph, isepl, nlh, KRECT, ierr
    else
       KRECT = REC_ERROR
    endif

    return
  end subroutine get_record_prop
! !!!_  & get_header - read header block
!   subroutine get_header &
!        & (ierr, &
!        &  head,  u, krect)
!     use TOUZA_Nio_std,   only: KIOFS, sus_read_lrec, sus_read_irec
!     use TOUZA_Nio_header,only: nitem
!     implicit none
!     integer,         intent(out) :: ierr
!     character(len=*),intent(out) :: head(*)
!     integer,         intent(in)  :: krect
!     integer,         intent(in)  :: u

!     logical swap, lrec
!     integer idfm

!     ierr = 0
!     if (KRECT.ge.0) then
!        swap = IAND(krect, REC_SWAP).ne.0
!        lrec = IAND(krect, REC_LSEP).ne.0
!        if (lrec) then
!           call sus_read_lrec(ierr, u, head, nitem, swap)
!        else
!           call sus_read_irec(ierr, u, head, nitem, swap)
!        endif
!        ! write(*, *) 'header', ierr, KRECT, head(1), swap, lrec
!        if (ierr.eq.0) then
!           idfm = check_id_format(head)
!           if (idfm.lt.0) ierr = idfm
!        endif
!     else
!        head(1:nitem) = ' '
!        ierr = _ERROR(ERR_UNKNOWN_FORMAT)
!     endif
!     ! write(*, *) 'header', ierr, KRECT
!     ! write(*, *) '/', head(1), '/'
!     return
!   end subroutine get_header

!!!_  & set_wrecord_prop - set sequential record properties to write (byte-order and separator size)
  subroutine set_wrecord_prop &
       & (ierr, krect, ufile, kendi)
    use TOUZA_Nio_std,only: choice, check_bodr_unit, KIOFS, kendi_mem, kendi_file, endian_OTHER
    use TOUZA_Nio_std,only: sus_getpos, msg, is_msglev_DEBUG
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(inout)       :: krect
    integer,intent(in)          :: ufile
    integer,intent(in),optional :: kendi
    integer ke, kr
    integer(kind=KIOFS) :: apos

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
          ke = endian_OTHER
       case default
          ke = kendi_mem
       end select
    endif
    if (ke.eq.endian_OTHER) then
       if (ierr.eq.0) call sus_getpos(ierr, apos, ufile)
       if (ierr.eq.0) call check_bodr_unit(ierr, ke, utest=ufile, jrec=0)
       if (ierr.eq.0) write(UNIT=ufile, IOSTAT=ierr, POS=apos)
    endif
    if (ierr.eq.0) then
       call get_switch(kr, ke, krect)
       krect = kr
    endif
    if (is_msglev_DEBUG(lev_verbose)) then
       if (ierr.eq.0) then
          call msg('(''byte-order assumption['', I0, ''] = '', I0)', (/ufile, ke/), __MDL__)
       endif
    endif
    return
  end subroutine set_wrecord_prop

!!!_  & get_switch - get record-format switch to write
  subroutine get_switch (krect, kendi, kcfg)
    use TOUZA_Nio_std,only: choice, endian_BIG, endian_LITTLE
    implicit none
    integer,intent(out)         :: krect
    integer,intent(in)          :: kendi  ! estimated file byte-order
    integer,intent(in),optional :: kcfg   ! user setting to overwrite default

    integer ktmp

    krect = REC_DEFAULT

    ktmp = choice(def_krectw, kcfg)
    if (ktmp.lt.0) ktmp = def_krectw
    if (ktmp.lt.0) ktmp = REC_DEFAULT

    krect = IOR(krect, IAND(ktmp, REC_LSEP))
    if (IAND(ktmp, REC_SWAP).gt.0) then
       krect = krect + REC_SWAP
    else if (IAND(ktmp, REC_BIG).gt.0) then
       if (kendi.eq.endian_LITTLE) krect = krect + REC_SWAP
    else if (IAND(ktmp, REC_LITTLE).gt.0) then
       if (kendi.eq.endian_BIG) krect = krect + REC_SWAP
    endif
    krect = IOR(krect, IAND(ktmp, REC_SUB_ALLOW))

    return
  end subroutine get_switch

!!!_  & put_header - write header block
  subroutine put_header &
       & (ierr, &
       &  head, u, krect)
    use TOUZA_Nio_std,   only: KIOFS, sus_write_lrec, sus_write_irec
    use TOUZA_Nio_header,only: nitem
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
       call sus_write_lrec(ierr, u, head, nitem, swap)
    else
       call sus_write_irec(ierr, u, head, nitem, swap)
    endif
    return
  end subroutine put_header
!!!_  & get_data_record - read data block
  subroutine get_data_record_i &
       & (ierr, &
       &  d,  n, u, krect, sub)
    use TOUZA_Nio_std,only: sus_read_lrec, sus_read_irec
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)            :: ierr
    integer(kind=KARG),intent(out)            :: d(*)
    integer,           intent(in)             :: n
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: u
    logical,           intent(inout),optional :: sub

    logical swap, lrec
    integer div

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call sus_read_lrec(ierr, u, d, n, swap)
    else
       div = read_sep_flag(krect)
       call sus_read_irec(ierr, u, d, n, swap, sub, div)
    endif
    return
  end subroutine get_data_record_i
  subroutine get_data_record_f &
       & (ierr, &
       &  d,  n, u, krect, sub)
    use TOUZA_Nio_std,only: sus_read_lrec, sus_read_irec
    implicit none
    integer,parameter :: KARG=KFLT
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(*)
    integer,        intent(in)             :: n
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: u
    logical,        intent(inout),optional :: sub

    logical swap, lrec
    integer div

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call sus_read_lrec(ierr, u, d, n, swap)
    else
       div = read_sep_flag(krect)
       call sus_read_irec(ierr, u, d, n, swap, sub, div)
    endif
    return
  end subroutine get_data_record_f
  subroutine get_data_record_d &
       & (ierr, &
       &  d,  n, u, krect, sub)
    use TOUZA_Nio_std,only: sus_read_lrec, sus_read_irec
    implicit none
    integer,parameter :: KARG=KDBL
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(*)
    integer,        intent(in)             :: n
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: u
    logical,        intent(inout),optional :: sub

    logical swap, lrec
    integer div

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call sus_read_lrec(ierr, u, d, n, swap)
    else
       div = read_sep_flag(krect)
       call sus_read_irec(ierr, u, d, n, swap, sub, div)
    endif
    return
  end subroutine get_data_record_d

  subroutine get_data_record_i1 &
       & (ierr, &
       &  d,  u, krect, sub)
    use TOUZA_Nio_std,only: sus_read_lrec, sus_read_irec
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)            :: ierr
    integer(kind=KARG),intent(out)            :: d
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: u
    logical,           intent(inout),optional :: sub

    logical swap, lrec
    integer div
    integer(kind=KARG) :: b(1)

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call sus_read_lrec(ierr, u, b, 1, swap)
    else
       div = read_sep_flag(krect)
       call sus_read_irec(ierr, u, b, 1, swap, sub, div)
    endif
    if (ierr.eq.0) d = b(1)
    return
  end subroutine get_data_record_i1
  ! subroutine get_data_record_f1 &
  !      & (ierr, &
  !      &  d,  u, krect, sub)
  !   use TOUZA_Nio_std,only: sus_read_lrec, sus_read_irec
  !   implicit none
  !   integer,parameter :: KARG=KFLT
  !   integer,        intent(out)            :: ierr
  !   real(kind=KARG),intent(out)            :: d
  !   integer,        intent(in)             :: krect
  !   integer,        intent(in)             :: u
  !   logical,        intent(inout),optional :: sub

  !   logical swap, lrec
  !   integer div
  !   real(kind=KARG) :: b(1)

  !   ierr = 0
  !   swap = IAND(krect, REC_SWAP).ne.0
  !   lrec = IAND(krect, REC_LSEP).ne.0
  !   if (lrec) then
  !      call sus_read_lrec(ierr, u, b, 1, swap)
  !   else
  !      div = read_sep_flag(krect)
  !      call sus_read_irec(ierr, u, b, 1, swap, sub, div)
  !   endif
  !   if (ierr.eq.0) d = b(1)
  !   return
  ! end subroutine get_data_record_f1
  subroutine get_data_record_d1 &
       & (ierr, &
       &  d, u, krect, sub)
    use TOUZA_Nio_std,only: sus_read_lrec, sus_read_irec
    implicit none
    integer,parameter :: KARG=KDBL
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: u
    logical,        intent(inout),optional :: sub

    logical swap, lrec
    integer div
    real(kind=KARG) :: b(1)

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call sus_read_lrec(ierr, u, b, 1, swap)
    else
       div = read_sep_flag(krect)
       call sus_read_irec(ierr, u, b, 1, swap, sub, div)
    endif
    if (ierr.eq.0) d = b(1)
    return
  end subroutine get_data_record_d1
!!!_  & get_data_record_slice - read data block (slice)
  subroutine get_data_record_slice_i &
       & (ierr, &
       &  d,  nd, u, krect, md, bes, nr, sub)
    use TOUZA_Nio_std,only: sus_slice_read_irec
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)            :: ierr
    integer(kind=KARG),intent(out)            :: d(*)
    integer,           intent(in)             :: nd        ! d size
    integer,           intent(in)             :: u
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: md        ! size of data record
    integer,           intent(in)             :: bes(3, *)
    integer,           intent(in)             :: nr
    logical,           intent(inout),optional :: sub

    logical swap, lrec
    integer div

    ierr = 0
    if (nd.ge.0.and.nd.lt.count_bes(bes, nr)) then
       ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       return
    endif

    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    else
       div = read_sep_flag(krect)
       call sus_slice_read_irec(ierr, u, d, bes, nr, swap, sub, div, md)
    endif
    return
  end subroutine get_data_record_slice_i
  subroutine get_data_record_slice_f &
       & (ierr, &
       &  d,  nd, u, krect, md, bes, nr, sub)
    use TOUZA_Nio_std,only: sus_slice_read_irec
    implicit none
    integer,parameter :: KARG=KFLT
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(*)
    integer,        intent(in)             :: nd        ! d size
    integer,        intent(in)             :: u
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: md        ! size of data record
    integer,        intent(in)             :: bes(3, *)
    integer,        intent(in)             :: nr
    logical,        intent(inout),optional :: sub

    logical swap, lrec
    integer div

    ierr = 0
    if (nd.ge.0.and.nd.lt.count_bes(bes, nr)) then
       ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       return
    endif

    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    else
       div = read_sep_flag(krect)
       call sus_slice_read_irec(ierr, u, d, bes, nr, swap, sub, div, md)
    endif
    return
  end subroutine get_data_record_slice_f
  subroutine get_data_record_slice_d &
       & (ierr, &
       &  d,  nd, u, krect, md, bes, nr, sub)
    use TOUZA_Nio_std,only: sus_slice_read_irec
    implicit none
    integer,parameter :: KARG=KDBL
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(*)
    integer,        intent(in)             :: nd        ! d size
    integer,        intent(in)             :: u
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: md        ! size of data record
    integer,        intent(in)             :: bes(3, *)
    integer,        intent(in)             :: nr
    logical,        intent(inout),optional :: sub

    logical swap, lrec
    integer div

    ierr = 0
    if (nd.ge.0.and.nd.lt.count_bes(bes, nr)) then
       ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       return
    endif

    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    else
       div = read_sep_flag(krect)
       call sus_slice_read_irec(ierr, u, d, bes, nr, swap, sub, div, md)
    endif
    return
  end subroutine get_data_record_slice_d
!!!_  & get_data_record_runl - read data block (runlength)
  subroutine get_data_record_runl_i &
       & (ierr, &
       &  d,  n, u, krect, runl, nrl, sub)
    use TOUZA_Nio_std,only: sus_runl_read_irec
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)            :: ierr
    integer(kind=KARG),intent(out)            :: d(*)
    integer,           intent(in)             :: n
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: u
    integer,           intent(in)             :: runl(*)
    integer,           intent(in)             :: nrl
    logical,           intent(inout),optional :: sub

    logical swap, lrec
    integer div

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    else
       div = read_sep_flag(krect)
       call sus_runl_read_irec(ierr, u, d, runl, nrl, swap, sub, div, n)
    endif
    return
  end subroutine get_data_record_runl_i
  ! subroutine get_data_record_runl_f &
  !      & (ierr, &
  !      &  d,  n, u, krect, runl, nrl, sub)
  !   use TOUZA_Nio_std,only: sus_runl_read_irec
  !   implicit none
  !   integer,parameter :: KARG=KFLT
  !   integer,        intent(out)            :: ierr
  !   real(kind=KARG),intent(out)            :: d(*)
  !   integer,        intent(in)             :: n
  !   integer,        intent(in)             :: krect
  !   integer,        intent(in)             :: u
  !   integer,        intent(in)             :: runl(*)
  !   integer,        intent(in)             :: nrl
  !   logical,        intent(inout),optional :: sub

  !   logical swap, lrec
  !   integer div

  !   ierr = 0
  !   swap = IAND(krect, REC_SWAP).ne.0
  !   lrec = IAND(krect, REC_LSEP).ne.0
  !   if (lrec) then
  !      ierr = _ERROR(ERR_NOT_IMPLEMENTED)
  !   else
  !      div = read_sep_flag(krect)
  !      call sus_runl_read_irec(ierr, u, d, runl, nrl, swap, sub, div, n)
  !   endif
  !   return
  ! end subroutine get_data_record_runl_f
  ! subroutine get_data_record_runl_d &
  !      & (ierr, &
  !      &  d,  n, u, krect, runl, nrl, sub)
  !   use TOUZA_Nio_std,only: sus_runl_read_irec
  !   implicit none
  !   integer,parameter :: KARG=KDBL
  !   integer,        intent(out)            :: ierr
  !   real(kind=KDBL),intent(out)            :: d(*)
  !   integer,        intent(in)             :: n
  !   integer,        intent(in)             :: krect
  !   integer,        intent(in)             :: u
  !   integer,        intent(in)             :: runl(*)
  !   integer,        intent(in)             :: nrl
  !   logical,        intent(inout),optional :: sub

  !   logical swap, lrec
  !   integer div

  !   ierr = 0
  !   swap = IAND(krect, REC_SWAP).ne.0
  !   lrec = IAND(krect, REC_LSEP).ne.0
  !   if (lrec) then
  !      ierr = _ERROR(ERR_NOT_IMPLEMENTED)
  !   else
  !      div = read_sep_flag(krect)
  !      call sus_runl_read_irec(ierr, u, d, runl, nrl, swap, sub, div, n)
  !   endif
  !   return
  ! end subroutine get_data_record_runl_d
!!!_  & get_data_record_list
  subroutine get_data_record_list_i &
       & (ierr, &
       &  d,  nd, u, krect, md, list, nl, sub)
    use TOUZA_Nio_std,only: sus_list_read_irec
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)            :: ierr
    integer(kind=KARG),intent(out)            :: d(*)
    integer,           intent(in)             :: nd        ! d size
    integer,           intent(in)             :: u
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: md        ! size of data record
    integer,           intent(in)             :: list(*)
    integer,           intent(in)             :: nl
    logical,           intent(inout),optional :: sub

    logical swap, lrec
    integer div

    ierr = 0
    if (nd.ge.0.and.nd.lt.nl) then
       ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       return
    endif

    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    else
       div = read_sep_flag(krect)
       call sus_list_read_irec(ierr, u, d, list, nl, swap, sub, div, md)
    endif
    return
  end subroutine get_data_record_list_i
  subroutine get_data_record_list_f &
       & (ierr, &
       &  d,  nd, u, krect, md, list, nl, sub)
    use TOUZA_Nio_std,only: sus_list_read_irec
    implicit none
    integer,parameter :: KARG=KFLT
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(*)
    integer,        intent(in)             :: nd        ! d size
    integer,        intent(in)             :: u
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: md        ! size of data record
    integer,        intent(in)             :: list(*)
    integer,        intent(in)             :: nl
    logical,        intent(inout),optional :: sub

    logical swap, lrec
    integer div

    ierr = 0
    if (nd.ge.0.and.nd.lt.nl) then
       ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       return
    endif

    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    else
       div = read_sep_flag(krect)
       call sus_list_read_irec(ierr, u, d, list, nl, swap, sub, div, md)
    endif
    return
  end subroutine get_data_record_list_f
  subroutine get_data_record_list_d &
       & (ierr, &
       &  d,  nd, u, krect, md, list, nl, sub)
    use TOUZA_Nio_std,only: sus_list_read_irec
    implicit none
    integer,parameter :: KARG=KDBL
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(*)
    integer,        intent(in)             :: nd        ! d size
    integer,        intent(in)             :: u
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: md        ! size of data record
    integer,        intent(in)             :: list(*)
    integer,        intent(in)             :: nl
    logical,        intent(inout),optional :: sub

    logical swap, lrec
    integer div

    ierr = 0
    if (nd.ge.0.and.nd.lt.nl) then
       ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       return
    endif

    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    else
       div = read_sep_flag(krect)
       call sus_list_read_irec(ierr, u, d, list, nl, swap, sub, div, md)
    endif
    return
  end subroutine get_data_record_list_d
!!!_  & get_data_record_suspend
  subroutine get_data_record_suspend_i &
       & (ierr, &
       &  d,  mem, u, krect, sw, rfil, nfil)
    use TOUZA_Nio_std,only: sus_suspend_read_irec
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)         :: ierr
    integer(kind=KARG),intent(out)         :: d(0:*)
    integer,           intent(in)          :: mem        ! d size or full size if with filter
    integer,           intent(in)          :: u
    integer,           intent(in)          :: krect
    integer,           intent(in)          :: sw        ! suspend-mode switch
    integer,           intent(in),optional :: rfil(0:*)
    integer,           intent(in),optional :: nfil

    logical swap, lrec
    integer div
    integer jf,    jd
    integer nread, nskip

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    else
       div = read_sep_flag(krect)
       if (present(rfil)) then
          jd = 0
          do jf = 0, nfil - 1, 2
             nskip = rfil(jf)
             nread = rfil(jf + 1)
             ! write(*, *) 'suspend:', jf, nskip, nread
             if (ierr.eq.0) then
                call sus_suspend_read_irec &
                     & (ierr, u, d(jd:jd+nread-1), nread, sw, swap=swap, div=div, nskip=nskip)
             endif
             jd = jd + nread
          enddo
          if (ierr.eq.0) then
             nskip = rfil(nfil)
             ! write(*, *) 'suspend/bottom:', nskip
             call sus_suspend_read_irec &
                  & (ierr, u, d, 0, sw, swap=swap, div=div, nskip=nskip)
          endif
       else
          call sus_suspend_read_irec(ierr, u, d, mem, sw, swap=swap, div=div)
       endif
    endif
    return
  end subroutine get_data_record_suspend_i
  ! subroutine get_data_record_suspend_f &
  !      & (ierr, &
  !      &  d,  nd, u, krect, sw, nskip)
  !   use TOUZA_Nio_std,only: sus_suspend_read_irec
  !   implicit none
  !   integer,parameter :: KARG=KFLT
  !   integer,        intent(out)         :: ierr
  !   real(kind=KARG),intent(out)         :: d(0:*)
  !   integer,        intent(in)          :: nd        ! d size
  !   integer,        intent(in)          :: u
  !   integer,        intent(in)          :: krect
  !   integer,        intent(in)          :: sw        ! suspend-mode switch
  !   integer,        intent(in),optional :: nskip

  !   logical swap, lrec
  !   integer div

  !   ierr = 0
  !   swap = IAND(krect, REC_SWAP).ne.0
  !   lrec = IAND(krect, REC_LSEP).ne.0
  !   if (lrec) then
  !      ierr = _ERROR(ERR_NOT_IMPLEMENTED)
  !   else
  !      div = read_sep_flag(krect)
  !      call sus_suspend_read_irec(ierr, u, d, nd, sw, swap=swap, div=div, nskip=nskip)
  !   endif
  !   return
  ! end subroutine get_data_record_suspend_f
  ! subroutine get_data_record_suspend_d &
  !      & (ierr, &
  !      &  d,  nd, u, krect, sw, nskip)
  !   use TOUZA_Nio_std,only: sus_suspend_read_irec
  !   implicit none
  !   integer,parameter :: KARG=KDBL
  !   integer,        intent(out)         :: ierr
  !   real(kind=KARG),intent(out)         :: d(0:*)
  !   integer,        intent(in)          :: nd        ! d size
  !   integer,        intent(in)          :: u
  !   integer,        intent(in)          :: krect
  !   integer,        intent(in)          :: sw        ! suspend-mode switch
  !   integer,        intent(in),optional :: nskip

  !   logical swap, lrec
  !   integer div

  !   ierr = 0
  !   swap = IAND(krect, REC_SWAP).ne.0
  !   lrec = IAND(krect, REC_LSEP).ne.0
  !   if (lrec) then
  !      ierr = _ERROR(ERR_NOT_IMPLEMENTED)
  !   else
  !      div = read_sep_flag(krect)
  !      call sus_suspend_read_irec(ierr, u, d, nd, sw, swap=swap, div=div, nskip=nskip)
  !   endif
  !   return
  ! end subroutine get_data_record_suspend_d
!!!_  & get_data_irecord - read integer data block with conversion
  subroutine get_data_irecord_f &
       & (ierr, &
       &  d,  n, u, krect, sub)
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
!!!_  & get_data_irecord_slice - read integer data block with conversion
  subroutine get_data_irecord_slice_f &
       & (ierr, &
       &  d,  nd, u, krect, md, bes, r, sub)
    implicit none
    integer,parameter :: KARG=KFLT, KSRC=KI32
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(*)
    integer,        intent(in)             :: nd        ! d size
    integer,        intent(in)             :: u
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: md        ! size of data record
    integer,        intent(in)             :: bes(3, *)
    integer,        intent(in)             :: r
    logical,        intent(inout),optional :: sub

    integer(kind=KSRC) :: w(nd)
    call get_data_record_slice_i (ierr, w, nd, u, krect, md, bes, r, sub)
    if (ierr.eq.0) then
       d(1:nd) = real(w(1:nd), kind=KARG)
    endif
    return
  end subroutine get_data_irecord_slice_f
  subroutine get_data_irecord_slice_d &
       & (ierr, &
       &  d,  nd, u, krect, md, bes, r, sub)
    implicit none
    integer,parameter :: KARG=KDBL, KSRC=KI32
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(*)
    integer,        intent(in)             :: nd        ! d size
    integer,        intent(in)             :: u
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: md        ! size of data record
    integer,        intent(in)             :: bes(3, *)
    integer,        intent(in)             :: r
    logical,        intent(inout),optional :: sub

    integer(kind=KSRC) :: w(nd)
    call get_data_record_slice_i (ierr, w, nd, u, krect, md, bes, r, sub)
    if (ierr.eq.0) then
       d(1:nd) = real(w(1:nd), kind=KARG)
    endif
    return
  end subroutine get_data_irecord_slice_d

!!!_  & get_data_frecord_slice - read float data block with conversion
  subroutine get_data_frecord_slice_i &
       & (ierr, &
       &  d,  nd, u, krect, md, bes, r, sub)
    implicit none
    integer,parameter :: KARG=KI32, KSRC=KFLT
    integer,           intent(out)            :: ierr
    integer(kind=KARG),intent(out)            :: d(*)
    integer,           intent(in)             :: nd        ! d size
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: u
    integer,           intent(in)             :: md        ! size of data record
    integer,           intent(in)             :: bes(3, *)
    integer,           intent(in)             :: r
    logical,           intent(inout),optional :: sub

    real(kind=KSRC) :: w(nd)
    call get_data_record_slice_f (ierr, w, nd, u, krect, md, bes, r, sub)
    if (ierr.eq.0) then
       d(1:nd) = int(w(1:nd), kind=KARG)
    endif
    return
  end subroutine get_data_frecord_slice_i
  subroutine get_data_frecord_slice_d &
       & (ierr, &
       &  d,  nd, u, krect, md, bes, r, sub)
    implicit none
    integer,parameter :: KARG=KDBL, KSRC=KFLT
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(*)
    integer,        intent(in)             :: nd        ! d size
    integer,        intent(in)             :: u
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: md        ! size of data record
    integer,        intent(in)             :: bes(3, *)
    integer,        intent(in)             :: r
    logical,        intent(inout),optional :: sub

    real(kind=KSRC) :: w(nd)
    call get_data_record_slice_f (ierr, w, nd, u, krect, md, bes, r, sub)
    if (ierr.eq.0) then
       d(1:nd) = real(w(1:nd), kind=KARG)
    endif
    return
  end subroutine get_data_frecord_slice_d
!!!_  & get_data_drecord_slice - read double data block with conversion
  subroutine get_data_drecord_slice_i &
       & (ierr, &
       &  d,  nd, u, krect, md, bes, r, sub)
    implicit none
    integer,parameter :: KARG=KI32, KSRC=KDBL
    integer,           intent(out)            :: ierr
    integer(kind=KARG),intent(out)            :: d(*)
    integer,           intent(in)             :: nd        ! d size
    integer,           intent(in)             :: u
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: md        ! size of data record
    integer,           intent(in)             :: bes(3, *)
    integer,           intent(in)             :: r
    logical,           intent(inout),optional :: sub

    real(kind=KSRC) :: w(nd)
    call get_data_record_slice_d (ierr, w, nd, u, krect, md, bes, r, sub)
    if (ierr.eq.0) then
       d(1:nd) = int(w(1:nd), kind=KARG)
    endif
    return
  end subroutine get_data_drecord_slice_i
  subroutine get_data_drecord_slice_f &
       & (ierr, &
       &  d,  nd, u, krect, md, bes, r, sub)
    implicit none
    integer,parameter :: KARG=KFLT, KSRC=KDBL
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(*)
    integer,        intent(in)             :: nd        ! d size
    integer,        intent(in)             :: u
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: md        ! size of data record
    integer,        intent(in)             :: bes(3, *)
    integer,        intent(in)             :: r
    logical,        intent(inout),optional :: sub

    real(kind=KSRC) :: w(nd)
    call get_data_record_slice_d (ierr, w, nd, u, krect, md, bes, r, sub)
    if (ierr.eq.0) then
       d(1:nd) = real(w(1:nd), kind=KARG)
    endif
    return
  end subroutine get_data_drecord_slice_f

!!!_  & put_data_record - write data block
  subroutine put_data_record_i &
       & (ierr, &
       &  d,  n, u, krect, pre, post)
    use TOUZA_Nio_std,only: sus_write_lrec, sus_write_irec
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)         :: ierr
    integer(kind=KARG),intent(in)          :: d(*)
    integer,           intent(in)          :: n
    integer,           intent(in)          :: krect
    integer,           intent(in)          :: u
    logical,           intent(in),optional :: pre, post

    logical swap, lrec
    integer dummy

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call sus_write_lrec(ierr, u, d, n, swap)
    else
       dummy = dummy_sep_flag(krect)
       call sus_write_irec(ierr, u, d, n, swap, pre, post, dummy)
    endif
    return
  end subroutine put_data_record_i
  subroutine put_data_record_f &
       & (ierr, &
       &  d,  n, u, krect, pre, post)
    use TOUZA_Nio_std,only: sus_write_lrec, sus_write_irec
    implicit none
    integer,parameter :: KARG=KFLT
    integer,        intent(out)         :: ierr
    real(kind=KARG),intent(in)          :: d(*)
    integer,        intent(in)          :: n
    integer,        intent(in)          :: krect
    integer,        intent(in)          :: u
    logical,        intent(in),optional :: pre, post

    logical swap, lrec
    integer dummy

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call sus_write_lrec(ierr, u, d, n, swap)
    else
       dummy = dummy_sep_flag(krect)
       call sus_write_irec(ierr, u, d, n, swap, pre, post, dummy)
    endif
    return
  end subroutine put_data_record_f
  subroutine put_data_record_d &
       & (ierr, &
       &  d,  n, u, krect, pre, post)
    use TOUZA_Nio_std,only: sus_write_lrec, sus_write_irec
    implicit none
    integer,parameter :: KARG=KDBL
    integer,        intent(out)         :: ierr
    real(kind=KARG),intent(in)          :: d(*)
    integer,        intent(in)          :: n
    integer,        intent(in)          :: krect
    integer,        intent(in)          :: u
    logical,        intent(in),optional :: pre, post

    logical swap, lrec
    integer dummy

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call sus_write_lrec(ierr, u, d, n, swap)
    else
       dummy = dummy_sep_flag(krect)
       call sus_write_irec(ierr, u, d, n, swap, pre, post, dummy)
    endif
    return
  end subroutine put_data_record_d

  subroutine put_data_record_i1 &
       & (ierr, &
       &  d,  u, krect, pre, post)
    use TOUZA_Nio_std,only: sus_write_lrec, sus_write_irec
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)         :: ierr
    integer(kind=KARG),intent(in)          :: d
    integer,           intent(in)          :: krect
    integer,           intent(in)          :: u
    logical,           intent(in),optional :: pre, post

    logical swap, lrec
    integer dummy
    integer(kind=KARG) :: b(1)

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    b(1) = d
    if (lrec) then
       call sus_write_lrec(ierr, u, b, 1, swap)
    else
       dummy = dummy_sep_flag(krect)
       call sus_write_irec(ierr, u, b, 1, swap, pre, post, dummy)
    endif
    return
  end subroutine put_data_record_i1
  ! subroutine put_data_record_f1 &
  !      & (ierr, &
  !      &  d,  u, krect, pre, post)
  !   use TOUZA_Nio_std,only: sus_write_lrec, sus_write_irec
  !   implicit none
  !   integer,parameter :: KARG=KFLT
  !   integer,        intent(out)         :: ierr
  !   real(kind=KARG),intent(in)          :: d
  !   integer,        intent(in)          :: krect
  !   integer,        intent(in)          :: u
  !   logical,        intent(in),optional :: pre, post

  !   logical swap, lrec
  !   integer dummy
  !   real(kind=KARG) :: b(1)

  !   ierr = 0
  !   swap = IAND(krect, REC_SWAP).ne.0
  !   lrec = IAND(krect, REC_LSEP).ne.0
  !   b(1) = d
  !   if (lrec) then
  !      call sus_write_lrec(ierr, u, b, 1, swap)
  !   else
  !      dummy = dummy_sep_flag(krect)
  !      call sus_write_irec(ierr, u, b, 1, swap, pre, post, dummy)
  !   endif
  !   return
  ! end subroutine put_data_record_f1
  ! subroutine put_data_record_d1 &
  !      & (ierr, &
  !      &  d, u, krect, pre, post)
  !   use TOUZA_Nio_std,only: sus_write_lrec, sus_write_irec
  !   implicit none
  !   integer,parameter :: KARG=KDBL
  !   integer,        intent(out)         :: ierr
  !   real(kind=KARG),intent(in)          :: d
  !   integer,        intent(in)          :: krect
  !   integer,        intent(in)          :: u
  !   logical,        intent(in),optional :: pre, post

  !   logical swap, lrec
  !   integer dummy
  !   real(kind=KARG) :: b(1)

  !   ierr = 0
  !   swap = IAND(krect, REC_SWAP).ne.0
  !   lrec = IAND(krect, REC_LSEP).ne.0
  !   b(1) = d
  !   if (lrec) then
  !      call sus_write_lrec(ierr, u, b, 1, swap)
  !   else
  !      dummy = dummy_sep_flag(krect)
  !      call sus_write_irec(ierr, u, b, 1, swap, pre, post, dummy)
  !   endif
  !   return
  ! end subroutine put_data_record_d1
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

!!!_  & dummy_sep_flag()
  integer function dummy_sep_flag(krect) result(k)
    implicit none
    integer,intent(in) :: krect
    if (IAND(krect, REC_SUB_ALLOW).eq.0) then
       k = MAGIC_DUMMY_SEP
    else
       k = 0
    endif
  end function dummy_sep_flag

!!!_  & read_sep_flag()
  integer function read_sep_flag(krect) result(k)
    implicit none
    integer,intent(in) :: krect
    if (IAND(krect, REC_SUB_ALLOW).eq.0) then
       k = sw_subrec
    else
       k = ignore_small
    endif
  end function read_sep_flag

!!!_  & nio_allow_sub()
  integer function nio_allow_sub(krect, cond) result(k)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,intent(in)          :: krect
    logical,intent(in),optional :: cond
    ! set SUB bit when COND is TRUE or absent
    if (choice(.TRUE., cond)) then
       k = IOR(krect, REC_SUB_ALLOW)
    else
       k = krect
    endif
  end function nio_allow_sub
!!!_  & set_switch_subrec()
  integer function set_switch_subrec(sw) result(n)
    implicit none
    integer,intent(in) :: sw
    n = sw_subrec
    sw_subrec = sw
  end function set_switch_subrec
!!!_  & parse_header_base - parse minimum properties
  subroutine parse_header_base &
       & (ierr, kfmt, kaxs, vmiss, head)
    use TOUZA_Nio_header,only: litem, &
         & hi_DFMT,  hi_MISS,  &
         & hi_ASTR1, hi_ASTR2, hi_ASTR3, &
         & hi_AEND1, hi_AEND2, hi_AEND3, &
         & get_item
    use TOUZA_Nio_std,only: KDBL, KFLT, KI32, KI64
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: kfmt
    integer,         intent(out) :: kaxs(*)
    real(kind=KRMIS),intent(out) :: vmiss
    character(len=*),intent(in)  :: head(*)

    character(len=litem) :: vp
    integer idfm
    integer jc

    ierr = err_default

    if (ierr.eq.0) then
       idfm = check_id_format(head)
       if (idfm.lt.0) ierr = idfm
    endif

    if (ierr.eq.0) call get_item(ierr, head, vp, hi_DFMT)
    if (ierr.eq.0) call parse_record_fmt(ierr, kfmt, vp)

    ! if (ierr.eq.0) then
    !    call parse_record_cmem(kaxs(1), head, hi_ASTR1, hi_AEND1)
    !    call parse_record_cmem(kaxs(2), head, hi_ASTR2, hi_AEND2)
    !    call parse_record_cmem(kaxs(3), head, hi_ASTR3, hi_AEND3)
    ! endif
    if (ierr.eq.0) then
       do jc = 1, laxs
          kaxs(jc) = parse_header_size(head, jc)
       enddo
    endif
    if (ierr.eq.0) call get_item(ierr, head, vmiss, hi_MISS, def=def_VMISS)

  end subroutine parse_header_base

!!!_  & nio_skip_prec - skip (physical) records
  subroutine nio_skip_prec &
       & (ierr, u, nrec, krect)
    use TOUZA_Nio_std,only: WHENCE_CURRENT, sus_skip_irec, sus_skip_lrec
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: u
    integer,intent(in)  :: nrec
    integer,intent(in)  :: krect
    logical swap, lrec
!!!_   . body
    ierr = 0
    if (ierr.eq.0) then
       swap = IAND(krect, REC_SWAP).ne.0
       lrec = IAND(krect, REC_LSEP).ne.0
       if (lrec) then
          call sus_skip_lrec(ierr, u, nrec, WHENCE_CURRENT, swap=swap)
       else
          call sus_skip_irec(ierr, u, nrec, WHENCE_CURRENT, swap=swap)
       endif
    endif
    return
  end subroutine nio_skip_prec

!!!_  & parse_header_size - parse size properties
  integer function parse_header_size_n &
       & (head, kidx, lazy) &
       & result (n)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_header,only: &
         & hi_ASTR1, hi_ASTR2, hi_ASTR3, &
         & hi_AEND1, hi_AEND2, hi_AEND3
    implicit none
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: kidx
    integer,optional,intent(in)  :: lazy    ! lazy level (default = strict)
    integer kaxs(laxs)
    integer lz

    lz = choice(def_lazy_size, lazy)
    select case (kidx)
    case (1)
       call parse_record_cmem(n, head, hi_ASTR1, hi_AEND1)
       if (lz.ge.0) n = max(lz, n)
    case (2)
       call parse_record_cmem(n, head, hi_ASTR2, hi_AEND2)
       if (lz.ge.0) n = max(lz, n)
    case (3)
       call parse_record_cmem(n, head, hi_ASTR3, hi_AEND3)
       if (lz.ge.0) n = max(lz, n)
    case default
       call parse_record_cmem(kaxs(1), head, hi_ASTR1, hi_AEND1)
       call parse_record_cmem(kaxs(2), head, hi_ASTR2, hi_AEND2)
       call parse_record_cmem(kaxs(3), head, hi_ASTR3, hi_AEND3)
       if (lz.ge.0) kaxs(:) = max(lz, kaxs(:))
       ! write(*, *) kaxs(:)
       if (ANY(kaxs(:).lt.0)) then
          n = kaxs(1)
          if (n.ge.0) n = kaxs(2)
          if (n.ge.0) n = kaxs(3)
       else
          n = kaxs(1) * kaxs(2) * kaxs(3)
       endif
    end select

    return
  end function parse_header_size_n
  integer(kind=KI32) function parse_header_size_i &
       & (head, kidx, lazy, mold) &
       & result (n)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_header,only: &
         & hi_ASTR1, hi_ASTR2, hi_ASTR3, &
         & hi_AEND1, hi_AEND2, hi_AEND3
    implicit none
    integer,parameter :: KARG=KI32
    character(len=*),  intent(in)  :: head(*)
    integer,           intent(in)  :: kidx
    integer,optional,  intent(in)  :: lazy
    integer(kind=KARG),intent(in)  :: mold
    integer kaxs(laxs)
    integer lz
    lz = choice(def_lazy_size, lazy) + (0 * mold)
    select case (kidx)
    case (1)
       call parse_record_cmem(n, head, hi_ASTR1, hi_AEND1)
       if (lz.ge.0) n = max(lz, n)
    case (2)
       call parse_record_cmem(n, head, hi_ASTR2, hi_AEND2)
       if (lz.ge.0) n = max(lz, n)
    case (3)
       call parse_record_cmem(n, head, hi_ASTR3, hi_AEND3)
       if (lz.ge.0) n = max(lz, n)
    case default
       call parse_record_cmem(kaxs(1), head, hi_ASTR1, hi_AEND1)
       call parse_record_cmem(kaxs(2), head, hi_ASTR2, hi_AEND2)
       call parse_record_cmem(kaxs(3), head, hi_ASTR3, hi_AEND3)
       if (lz.ge.0) kaxs(:) = max(lz, kaxs(:))
       if (ANY(kaxs(:).lt.0)) then
          n = kaxs(1)
          if (n.ge.0) n = kaxs(2)
          if (n.ge.0) n = kaxs(3)
       else
          n = kaxs(1) * kaxs(2) * kaxs(3)
       endif
    end select
    return
  end function parse_header_size_i

!!!_  & parse_record_fmt - parse format
  subroutine parse_record_fmt &
       & (ierr, kfmt, str)
    use TOUZA_Nio_std,only: KDBL, KFLT, KI32, KI64
    use TOUZA_Nio_std,only: parse_number
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: kfmt
    character(len=*),intent(in)  :: str
    integer kk
    integer je

    ierr = 0
    kfmt = 0
    je = index(str, sep_subvitem)
    if (je.eq.0) je = len_trim(str) + 1

    select case (str(1:1))
    case ('U')
       kfmt = 0
    case ('M')
       kfmt = GFMT_MASK
    case ('P')
       if (je.gt.len_trim(str)) then
          kfmt = GFMT_SUBV
       else
          kfmt = GFMT_LPAD
       endif
    case default
       ierr = _ERROR(ERR_UNKNOWN_FORMAT)
    end select

    if (ierr.eq.0) then
       select case (str(2:2))
       case ('R')
          select case (str(3:3))
          case ('C')
             if (kfmt.ne.0) then
                ierr = _ERROR(ERR_UNKNOWN_FORMAT)
             else
                if (str(4:4).eq.'2') then
                   kfmt = kfmt + GFMT_URC2
                else
                   kfmt = kfmt + GFMT_URC
                endif
             endif
          case ('Y', 'X')
             kfmt = kfmt + GFMT_URY
             call parse_number(ierr, kk, str(4:je-1), -1)
             if (ierr.eq.0) then
                if (kk.gt.(GFMT_URYend - GFMT_URY)) ierr = -1
                if (kk.lt.1) ierr = _ERROR(ERR_UNKNOWN_FORMAT)
             endif
             if (ierr.eq.0) kfmt = kfmt + kk
          case ('T')
             kfmt = kfmt + GFMT_URT
             ! read(str(4:), *, IOSTAT=ierr) kk
             ! if (ierr.eq.0) then
             !    kfmt = kfmt + min(max(0, kk), GFMT_URTend-GFMT_URT)
             ! else
             !    ierr = 0
             ! endif
          case default
             call parse_number(ierr, kk, str(3:je-1), -1)
             if (ierr.eq.0) then
                if (kk.eq.4) then
                   kfmt = kfmt + GFMT_UR4
                else if (kk.eq.8) then
                   kfmt = kfmt + GFMT_UR8
                else
                   ierr = _ERROR(ERR_UNKNOWN_FORMAT)
                endif
             else
                ierr = _ERROR(ERR_UNKNOWN_FORMAT)
             endif
          end select
       case ('I')
          call parse_number(ierr, kk, str(3:je-1), -1)
          if (ierr.eq.0) then
             if (kk.eq.1) then
                kfmt = kfmt + GFMT_UI1
             else if  (kk.eq.4) then
                kfmt = kfmt + GFMT_UI4
             else if (kk.eq.8) then
                kfmt = kfmt + GFMT_UI8
             else
                ierr = _ERROR(ERR_UNKNOWN_FORMAT)
             endif
          else
             ierr = _ERROR(ERR_UNKNOWN_FORMAT)
          endif
       case default
          ierr = _ERROR(ERR_UNKNOWN_FORMAT)
       end select
    endif
    if (ierr.ne.0) kfmt = GFMT_ERR

  end subroutine parse_record_fmt

!!!_  & is_packed_subv()
  logical function is_packed_subv(hd) result(b)
    use TOUZA_Nio_header,only: hi_DFMT
    implicit none
    character(len=*),intent(in) :: hd(*)
    integer jc
    jc = index(hd(hi_DFMT), sep_subvitem)
    if (jc.eq.0) then
       b = .TRUE.
    else
       b = hd(hi_DFMT)(jc+1:).eq.' '
    endif
  end function is_packed_subv
!!!_  & decompose_packed_item
  subroutine decompose_packed_item (svitem, dfmt)
    implicit none
    character(len=*),intent(out) :: svitem
    character(len=*),intent(in)  :: dfmt
    integer jc
    jc = index(dfmt, sep_subvitem)
    if (jc.eq.0) then
       svitem = ' '
    else
       svitem = dfmt(jc+1:)
    endif
  end subroutine decompose_packed_item

!!!_  & parse_record_cmem - parse coordinate members
  subroutine parse_record_cmem &
       & (nmem, head, ibgn, iend)
    use TOUZA_Nio_header,only: get_item
    implicit none
    integer,         intent(out) :: nmem
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: ibgn, iend

    integer nmin, nmax
    integer jerr

    jerr = 0
    if (jerr.eq.0) call get_item(jerr, head, nmin, ibgn)
    if (jerr.eq.0) call get_item(jerr, head, nmax, iend)
    if (jerr.eq.0) then
       nmem = max(0, nmax - nmin + 1)
    else
       nmem = jerr
    endif
  end subroutine parse_record_cmem

!!!_  & get_header_cprop - get coordinate properties
  subroutine get_header_cprop &
       & (name, irange, head, kidx)
    use TOUZA_Nio_header,only: &
         & get_item, &
         & hi_AITM1, hi_AITM2, hi_AITM3, &
         & hi_ASTR1, hi_ASTR2, hi_ASTR3, &
         & hi_AEND1, hi_AEND2, hi_AEND3
    implicit none
    character(len=*),intent(out) :: name
    integer,         intent(out) :: irange(*)
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: kidx
    integer jerr
    select case (kidx)
    case (1)
       call get_item(jerr, head, name, hi_AITM1)
       if (jerr.eq.0) call get_item(jerr, head, irange(1), hi_ASTR1)
       if (jerr.eq.0) call get_item(jerr, head, irange(2), hi_AEND1)
    case (2)
       call get_item(jerr, head, name, hi_AITM2)
       if (jerr.eq.0) call get_item(jerr, head, irange(1), hi_ASTR2)
       if (jerr.eq.0) call get_item(jerr, head, irange(2), hi_AEND2)
    case (3)
       call get_item(jerr, head, name, hi_AITM3)
       if (jerr.eq.0) call get_item(jerr, head, irange(1), hi_ASTR3)
       if (jerr.eq.0) call get_item(jerr, head, irange(2), hi_AEND3)
    case default
       jerr = -1
    end select
    if (jerr.ne.0) then
       name = ' '
       irange(1) = -1
       irange(2) = 0
    endif
  end subroutine get_header_cprop
!!!_  & put_header_cprop - put coordinate properties
  subroutine put_header_cprop &
       & (ierr, head, name, irange, kidx)
    use TOUZA_Nio_header,only: &
         & put_item, &
         & hi_AITM1, hi_AITM2, hi_AITM3, &
         & hi_ASTR1, hi_ASTR2, hi_ASTR3, &
         & hi_AEND1, hi_AEND2, hi_AEND3
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(inout) :: head(*)
    character(len=*),intent(in)    :: name
    integer,         intent(in)    :: irange(*)
    integer,         intent(in)    :: kidx
    ierr = 0
    select case (kidx)
    case (1)
       call put_item(ierr, head, name, hi_AITM1)
       if (ierr.eq.0) call put_item(ierr, head, irange(1), hi_ASTR1)
       if (ierr.eq.0) call put_item(ierr, head, irange(2), hi_AEND1)
    case (2)
       call put_item(ierr, head, name, hi_AITM2)
       if (ierr.eq.0) call put_item(ierr, head, irange(1), hi_ASTR2)
       if (ierr.eq.0) call put_item(ierr, head, irange(2), hi_AEND2)
    case (3)
       call put_item(ierr, head, name, hi_AITM3)
       if (ierr.eq.0) call put_item(ierr, head, irange(1), hi_ASTR3)
       if (ierr.eq.0) call put_item(ierr, head, irange(2), hi_AEND3)
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
  end subroutine put_header_cprop

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
    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer j

    ierr = 0
    nc   = count_packed(1, n, mold)

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
    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer j

    ierr = 0
    nc   = count_packed(1, n, mold)

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
    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer j
    integer(kind=KARG) :: imiss

    ierr = 0
    nc   = count_packed(1, n, mold)
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
  subroutine mask_decode_ddi &
       & (ierr,  d,     n,  &
       &  b,     icom,  &
       &  vmiss, kpack)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KDBL, KRSRC=KDBL, KISRC=KI32
    integer,            intent(out) :: ierr
    real(kind=KARG),    intent(out) :: d(0:*)      ! output
    integer,            intent(in)  :: n
    real(kind=KRSRC),   intent(in)  :: b(0:*)      ! compressed data sequence
    integer(kind=KISRC),intent(in)  :: icom(0:*)   ! packed mask
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kpack

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer j, jb

    ierr = 0
    if (ierr.eq.0) call alloc_wpack(ierr, n, 'ddi', mold)
    if (ierr.eq.0) call pack_restore(ierr, wpack, icom, n, 1, kpack)
    if (ierr.eq.0) then
       jb = 0
       do j = 0, n - 1
          if (wpack(j).eq.1) then
             d(j) = real(b(jb), kind=KARG)
             jb = jb + 1
          else
             d(j) = real(vmiss, kind=KARG)
          endif
       enddo
    endif
    if (ierr.eq.0) call alloc_wpack(ierr, -1, 'ddi', mold)
    return
  end subroutine mask_decode_ddi
  subroutine mask_decode_fdi &
       & (ierr,  d,     n,  &
       &  b,     icom,  &
       &  vmiss, kpack)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KFLT, KRSRC=KDBL, KISRC=KI32
    integer,            intent(out) :: ierr
    real(kind=KARG),    intent(out) :: d(0:*)      ! output
    integer,            intent(in)  :: n
    real(kind=KRSRC),   intent(in)  :: b(0:*)      ! compressed data sequence
    integer(kind=KISRC),intent(in)  :: icom(0:*)   ! packed mask
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kpack

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer j, jb

    ierr = 0
    if (ierr.eq.0) call alloc_wpack(ierr, n, 'fdi', mold)
    if (ierr.eq.0) call pack_restore(ierr, wpack, icom, n, 1, kpack)
    if (ierr.eq.0) then
       jb = 0
       do j = 0, n - 1
          if (wpack(j).eq.1) then
             d(j) = real(b(jb), kind=KARG)
             jb = jb + 1
          else
             d(j) = real(vmiss, kind=KARG)
          endif
       enddo
    endif
    if (ierr.eq.0) call alloc_wpack(ierr, -1, 'fdi', mold)
    return
  end subroutine mask_decode_fdi
  subroutine mask_decode_idi &
       & (ierr,  d,     n,  &
       &  b,     icom,  &
       &  vmiss, kpack)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KI32, KRSRC=KDBL, KISRC=KI32
    integer,            intent(out) :: ierr
    integer(kind=KARG), intent(out) :: d(0:*)      ! output
    integer,            intent(in)  :: n
    real(kind=KRSRC),   intent(in)  :: b(0:*)      ! compressed data sequence
    integer(kind=KISRC),intent(in)  :: icom(0:*)   ! packed mask
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kpack

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer j, jb

    ierr = 0
    if (ierr.eq.0) call alloc_wpack(ierr, n, 'idi', mold)
    if (ierr.eq.0) call pack_restore(ierr, wpack, icom, n, 1, kpack)
    if (ierr.eq.0) then
       jb = 0
       do j = 0, n - 1
          if (wpack(j).eq.1) then
             d(j) = int(b(jb), kind=KARG)
             jb = jb + 1
          else
             d(j) = int(vmiss, kind=KARG)
          endif
       enddo
    endif
    if (ierr.eq.0) call alloc_wpack(ierr, -1, 'idi', mold)
    return
  end subroutine mask_decode_idi

  subroutine mask_decode_dfi &
       & (ierr,  d,     n,  &
       &  b,     icom,  &
       &  vmiss, kpack)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KDBL, KRSRC=KFLT, KISRC=KI32
    integer,            intent(out) :: ierr
    real(kind=KARG),    intent(out) :: d(0:*)      ! output
    integer,            intent(in)  :: n
    real(kind=KRSRC),   intent(in)  :: b(0:*)      ! compressed data sequence
    integer(kind=KISRC),intent(in)  :: icom(0:*)   ! packed mask
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kpack

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer j, jb

    ierr = 0
    if (ierr.eq.0) call alloc_wpack(ierr, n, 'dfi', mold)
    if (ierr.eq.0) call pack_restore(ierr, wpack, icom, n, 1, kpack)
    if (ierr.eq.0) then
       jb = 0
       do j = 0, n - 1
          if (wpack(j).eq.1) then
             d(j) = real(b(jb), kind=KARG)
             jb = jb + 1
          else
             d(j) = real(vmiss, kind=KARG)
          endif
       enddo
    endif
    if (ierr.eq.0) call alloc_wpack(ierr, -1, 'dfi', mold)
    return
  end subroutine mask_decode_dfi
  subroutine mask_decode_ffi &
       & (ierr,  d,     n,  &
       &  b,     icom,  &
       &  vmiss, kpack)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KFLT, KRSRC=KFLT, KISRC=KI32
    integer,            intent(out) :: ierr
    real(kind=KARG),    intent(out) :: d(0:*)      ! output
    integer,            intent(in)  :: n
    real(kind=KRSRC),   intent(in)  :: b(0:*)      ! compressed data sequence
    integer(kind=KISRC),intent(in)  :: icom(0:*)   ! packed mask
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kpack

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer j, jb

    ierr = 0
    if (ierr.eq.0) call alloc_wpack(ierr, n, 'ffi', mold)
    if (ierr.eq.0) call pack_restore(ierr, wpack, icom, n, 1, kpack)
    if (ierr.eq.0) then
       jb = 0
       do j = 0, n - 1
          if (wpack(j).eq.1) then
             d(j) = real(b(jb), kind=KARG)
             jb = jb + 1
          else
             d(j) = real(vmiss, kind=KARG)
          endif
       enddo
    endif
    if (ierr.eq.0) call alloc_wpack(ierr, -1, 'ffi', mold)
    return
  end subroutine mask_decode_ffi
  subroutine mask_decode_ifi &
       & (ierr,  d,     n,  &
       &  b,     icom,  &
       &  vmiss, kpack)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KI32, KRSRC=KFLT, KISRC=KI32
    integer,            intent(out) :: ierr
    integer(kind=KARG), intent(out) :: d(0:*)      ! output
    integer,            intent(in)  :: n
    real(kind=KRSRC),   intent(in)  :: b(0:*)      ! compressed data sequence
    integer(kind=KISRC),intent(in)  :: icom(0:*)   ! packed mask
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kpack

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer j, jb

    ierr = 0
    if (ierr.eq.0) call alloc_wpack(ierr, n, 'ifi', mold)
    if (ierr.eq.0) call pack_restore(ierr, wpack, icom, n, 1, kpack)
    if (ierr.eq.0) then
       jb = 0
       do j = 0, n - 1
          if (wpack(j).eq.1) then
             d(j) = int(b(jb), kind=KARG)
             jb = jb + 1
          else
             d(j) = int(vmiss, kind=KARG)
          endif
       enddo
    endif
    if (ierr.eq.0) call alloc_wpack(ierr, -1, 'ifi', mold)
    return
  end subroutine mask_decode_ifi

  subroutine mask_decode_dii &
       & (ierr,  d,     n,  &
       &  b,     icom,  &
       &  vmiss, kpack)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32
    integer,            intent(out) :: ierr
    real(kind=KARG),    intent(out) :: d(0:*)      ! output
    integer,            intent(in)  :: n
    integer(kind=KISRC),intent(in)  :: b(0:*)      ! compressed data sequence
    integer(kind=KISRC),intent(in)  :: icom(0:*)   ! packed mask
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kpack

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer j, jb

    ierr = 0
    if (ierr.eq.0) call alloc_wpack(ierr, n, 'dii', mold)
    if (ierr.eq.0) call pack_restore(ierr, wpack, icom, n, 1, kpack)
    if (ierr.eq.0) then
       jb = 0
       do j = 0, n - 1
          if (wpack(j).eq.1) then
             d(j) = real(b(jb), kind=KARG)
             jb = jb + 1
          else
             d(j) = real(vmiss, kind=KARG)
          endif
       enddo
    endif
    if (ierr.eq.0) call alloc_wpack(ierr, -1, 'dii', mold)
    return
  end subroutine mask_decode_dii
  subroutine mask_decode_fii &
       & (ierr,  d,     n,  &
       &  b,     icom,  &
       &  vmiss, kpack)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32
    integer,            intent(out) :: ierr
    real(kind=KARG),    intent(out) :: d(0:*)      ! output
    integer,            intent(in)  :: n
    integer(kind=KISRC),intent(in)  :: b(0:*)      ! compressed data sequence
    integer(kind=KISRC),intent(in)  :: icom(0:*)   ! packed mask
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kpack

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer j, jb

    ierr = 0
    if (ierr.eq.0) call alloc_wpack(ierr, n, 'fii', mold)
    if (ierr.eq.0) call pack_restore(ierr, wpack, icom, n, 1, kpack)
    if (ierr.eq.0) then
       jb = 0
       do j = 0, n - 1
          if (wpack(j).eq.1) then
             d(j) = real(b(jb), kind=KARG)
             jb = jb + 1
          else
             d(j) = real(vmiss, kind=KARG)
          endif
       enddo
    endif
    if (ierr.eq.0) call alloc_wpack(ierr, -1, 'fii', mold)
    return
  end subroutine mask_decode_fii
  subroutine mask_decode_iii &
       & (ierr,  d,     n,  &
       &  b,     icom,  &
       &  vmiss, kpack)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32
    integer,            intent(out) :: ierr
    integer(kind=KARG), intent(out) :: d(0:*)      ! output
    integer,            intent(in)  :: n
    integer(kind=KISRC),intent(out) :: b(0:*)      ! output
    integer(kind=KISRC),intent(in)  :: icom(0:*)   ! packed mask
    real(kind=KRMIS),   intent(in)  :: vmiss
    integer,            intent(in)  :: kpack

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer j, jb

    ierr = 0
    if (ierr.eq.0) call alloc_wpack(ierr, n, 'iii', mold)
    if (ierr.eq.0) call pack_restore(ierr, wpack, icom, n, 1, kpack)
    if (ierr.eq.0) then
       jb = 0
       do j = 0, n - 1
          if (wpack(j).eq.1) then
             d(j) = int(b(jb), kind=KARG)
             jb = jb + 1
          else
             d(j) = int(vmiss, kind=KARG)
          endif
       enddo
    endif
    if (ierr.eq.0) call alloc_wpack(ierr, -1, 'iii', mold)
    return
  end subroutine mask_decode_iii

!!!_  & mask_decode_subv - mask decoding into subscript vector
  subroutine mask_decode_subv_i &
       & (ierr,  subv,  ends, mi, mo, &
       &  icom,  mfull, kpack)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KISRC=KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: subv(0:*)
    integer,            intent(out) :: ends(0:*)
    integer,            intent(in)  :: mi, mo
    integer(kind=KISRC),intent(in)  :: icom(0:*)   ! packed mask
    integer,            intent(in)  :: mfull
    integer,            intent(in)  :: kpack

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ji, jo
    integer js, jv

    ierr = 0
    if (ierr.eq.0) call alloc_wpack(ierr, mfull, 'subvi', mold)
    if (ierr.eq.0) call pack_restore(ierr, wpack, icom, mfull, 1, kpack)
    if (ierr.eq.0) then
       js = 0
       do jo = 0, mo - 1
          do ji = 0, mi - 1
             jv = jo * mi + ji
             if (wpack(jv).ne.0) then
                subv(js) = ji
                js = js + 1
             endif
          enddo
          ends(jo) = js
       enddo
    endif
    if (ierr.eq.0) call alloc_wpack(ierr, -1, 'subvi', mold)
    return
  end subroutine mask_decode_subv_i

!!!_  & subv_encode - subscript vector encoding
  subroutine subv_encode_d &
       & (ierr,  subv,  ends, nsub, &
       &  src,   mi,    mo,   vmiss)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    integer,         intent(out) :: subv(0:*)
    integer,         intent(out) :: ends(0:*)
    integer,         intent(in)  :: nsub
    real(kind=KARG), intent(in)  :: src(0:*)
    integer,         intent(in)  :: mi, mo
    real(kind=KRMIS),intent(in)  :: vmiss
    real(kind=KARG) :: vmt
    integer jx, js, jo, ji
    ierr = 0
    vmt = real(vmiss, kind=KARG)
    jx = 0
    do jo = 0, mo - 1
       do ji = 0, mi - 1
          js = mi * jo + ji
          if (src(js).ne.vmt) then
             subv(jx) = ji
             jx = jx + 1
          endif
       enddo
       ends(jo) = jx
    enddo
    if (jx.gt.nsub) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
  end subroutine subv_encode_d
  subroutine subv_encode_f &
       & (ierr,  subv,  ends, nsub, &
       &  src,   mi,    mo,   vmiss)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out) :: ierr
    integer,         intent(out) :: subv(0:*)
    integer,         intent(out) :: ends(0:*)
    integer,         intent(in)  :: nsub
    real(kind=KARG), intent(in)  :: src(0:*)
    integer,         intent(in)  :: mi, mo
    real(kind=KRMIS),intent(in)  :: vmiss
    real(kind=KARG) :: vmt
    integer jx, js, jo, ji
    ierr = 0
    vmt = real(vmiss, kind=KARG)
    jx = 0
    do jo = 0, mo - 1
       do ji = 0, mi - 1
          js = mi * jo + ji
          if (src(js).ne.vmt) then
             subv(jx) = ji
             jx = jx + 1
          endif
       enddo
       ends(jo) = jx
    enddo
    if (jx.gt.nsub) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
  end subroutine subv_encode_f
  subroutine subv_encode_i &
       & (ierr,  subv,  ends, nsub, &
       &  src,   mi,    mo,   vmiss)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: subv(0:*)
    integer,         intent(out) :: ends(0:*)
    integer,         intent(in)  :: nsub
    integer,         intent(in)  :: src(0:*)
    integer,         intent(in)  :: mi, mo
    real(kind=KRMIS),intent(in)  :: vmiss
    integer :: vmt
    integer jx, js, jo, ji
    ierr = 0
    vmt = int(vmiss)
    jx = 0
    do jo = 0, mo - 1
       do ji = 0, mi - 1
          js = mi * jo + ji
          if (src(js).ne.vmt) then
             subv(jx) = ji
             jx = jx + 1
          endif
       enddo
       ends(jo) = jx
    enddo
    if (jx.gt.nsub) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
  end subroutine subv_encode_i

!!!_  & subv_decode - subscript vector decoding
  subroutine subv_decode_dd &
       & (ierr,  d,     n,    &
       &  b,     subv,  nsub, vmiss)
    implicit none
    integer,parameter :: KARG=KDBL, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)      ! output
    integer,         intent(in)  :: n
    real(kind=KRSRC),intent(in)  :: b(0:*)      ! compressed data sequence
    integer,         intent(in)  :: subv(0:*)
    integer,         intent(in)  :: nsub
    real(kind=KRMIS),intent(in)  :: vmiss

    real(kind=KARG) :: vm
    integer j, jb, je

    ierr = 0
    d(subv(0:nsub-1)) = real(b(0:nsub-1), kind=KARG)
    vm = real(vmiss, kind=KARG)
    jb = 0
    do j = 0, nsub - 1
       je = subv(j)
       d(jb:je-1) = vm
       jb = je + 1
    enddo
    d(jb:n-1) = vm
    return
  end subroutine subv_decode_dd
  subroutine subv_decode_fd &
       & (ierr,  d,     n,    &
       &  b,     subv,  nsub, vmiss)
    implicit none
    integer,parameter :: KARG=KFLT, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)      ! output
    integer,         intent(in)  :: n
    real(kind=KRSRC),intent(in)  :: b(0:*)      ! compressed data sequence
    integer,         intent(in)  :: subv(0:*)
    integer,         intent(in)  :: nsub
    real(kind=KRMIS),intent(in)  :: vmiss

    real(kind=KARG) :: vm
    integer j, jb, je

    ierr = 0
    d(subv(0:nsub-1)) = real(b(0:nsub-1), kind=KARG)
    vm = real(vmiss, kind=KARG)
    jb = 0
    do j = 0, nsub - 1
       je = subv(j)
       d(jb:je-1) = vm
       jb = je + 1
    enddo
    d(jb:n-1) = vm
    return
  end subroutine subv_decode_fd
  subroutine subv_decode_id &
       & (ierr,  d,     n,    &
       &  b,     subv,  nsub, vmiss)
    implicit none
    integer,parameter :: KARG=KI32, KRSRC=KDBL
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(out) :: d(0:*)      ! output
    integer,           intent(in)  :: n
    real(kind=KRSRC),  intent(in)  :: b(0:*)      ! compressed data sequence
    integer,           intent(in)  :: subv(0:*)
    integer,           intent(in)  :: nsub
    real(kind=KRMIS),  intent(in)  :: vmiss

    integer(kind=KARG) :: vm
    integer j, jb, je

    ierr = 0
    d(subv(0:nsub-1)) = int(b(0:nsub-1), kind=KARG)
    vm = int(vmiss, kind=KARG)
    jb = 0
    do j = 0, nsub - 1
       je = subv(j)
       d(jb:je-1) = vm
       jb = je + 1
    enddo
    d(jb:n-1) = vm
    return
  end subroutine subv_decode_id
  subroutine subv_decode_df &
       & (ierr,  d,     n,    &
       &  b,     subv,  nsub, vmiss)
    implicit none
    integer,parameter :: KARG=KDBL, KRSRC=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)      ! output
    integer,         intent(in)  :: n
    real(kind=KRSRC),intent(in)  :: b(0:*)      ! compressed data sequence
    integer,         intent(in)  :: subv(0:*)
    integer,         intent(in)  :: nsub
    real(kind=KRMIS),intent(in)  :: vmiss

    real(kind=KARG) :: vm
    integer j, jb, je

    ierr = 0
    d(subv(0:nsub-1)) = real(b(0:nsub-1), kind=KARG)
    vm = real(vmiss, kind=KARG)
    jb = 0
    do j = 0, nsub - 1
       je = subv(j)
       d(jb:je-1) = vm
       jb = je + 1
    enddo
    d(jb:n-1) = vm
    return
  end subroutine subv_decode_df
  subroutine subv_decode_ff &
       & (ierr,  d,     n,    &
       &  b,     subv,  nsub, vmiss)
    implicit none
    integer,parameter :: KARG=KFLT, KRSRC=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)      ! output
    integer,         intent(in)  :: n
    real(kind=KRSRC),intent(in)  :: b(0:*)      ! compressed data sequence
    integer,         intent(in)  :: subv(0:*)
    integer,         intent(in)  :: nsub
    real(kind=KRMIS),intent(in)  :: vmiss

    real(kind=KARG) :: vm
    integer j, jb, je

    ierr = 0
    d(subv(0:nsub-1)) = real(b(0:nsub-1), kind=KARG)
    vm = real(vmiss, kind=KARG)
    jb = 0
    do j = 0, nsub - 1
       je = subv(j)
       d(jb:je-1) = vm
       jb = je + 1
    enddo
    d(jb:n-1) = vm
    return
  end subroutine subv_decode_ff
  subroutine subv_decode_if &
       & (ierr,  d,     n,    &
       &  b,     subv,  nsub, vmiss)
    implicit none
    integer,parameter :: KARG=KI32, KRSRC=KFLT
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(out) :: d(0:*)      ! output
    integer,           intent(in)  :: n
    real(kind=KRSRC),  intent(in)  :: b(0:*)      ! compressed data sequence
    integer,           intent(in)  :: subv(0:*)
    integer,           intent(in)  :: nsub
    real(kind=KRMIS),  intent(in)  :: vmiss

    integer(kind=KARG) :: vm
    integer j, jb, je

    ierr = 0
    d(subv(0:nsub-1)) = int(b(0:nsub-1), kind=KARG)
    vm = int(vmiss, kind=KARG)
    jb = 0
    do j = 0, nsub - 1
       je = subv(j)
       d(jb:je-1) = vm
       jb = je + 1
    enddo
    d(jb:n-1) = vm
    return
  end subroutine subv_decode_if

  subroutine subv_decode_di &
       & (ierr,  d,     n,    &
       &  b,     subv,  nsub, vmiss)
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32
    integer,            intent(out) :: ierr
    real(kind=KARG),    intent(out) :: d(0:*)      ! output
    integer,            intent(in)  :: n
    integer(kind=KISRC),intent(in)  :: b(0:*)      ! compressed data sequence
    integer,            intent(in)  :: subv(0:*)
    integer,            intent(in)  :: nsub
    real(kind=KRMIS),   intent(in)  :: vmiss

    real(kind=KARG) :: vm
    integer j, jb, je

    ierr = 0
    d(subv(0:nsub-1)) = real(b(0:nsub-1), kind=KARG)
    vm = real(vmiss, kind=KARG)
    jb = 0
    do j = 0, nsub - 1
       je = subv(j)
       d(jb:je-1) = vm
       jb = je + 1
    enddo
    d(jb:n-1) = vm
    return
  end subroutine subv_decode_di
  subroutine subv_decode_fi &
       & (ierr,  d,     n,    &
       &  b,     subv,  nsub, vmiss)
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32
    integer,            intent(out) :: ierr
    real(kind=KARG),    intent(out) :: d(0:*)      ! output
    integer,            intent(in)  :: n
    integer(kind=KISRC),intent(in)  :: b(0:*)      ! compressed data sequence
    integer,            intent(in)  :: subv(0:*)
    integer,            intent(in)  :: nsub
    real(kind=KRMIS),   intent(in)  :: vmiss

    real(kind=KARG) :: vm
    integer j, jb, je

    ierr = 0
    d(subv(0:nsub-1)) = real(b(0:nsub-1), kind=KARG)
    vm = real(vmiss, kind=KARG)
    jb = 0
    do j = 0, nsub - 1
       je = subv(j)
       d(jb:je-1) = vm
       jb = je + 1
    enddo
    d(jb:n-1) = vm
    return
  end subroutine subv_decode_fi
  subroutine subv_decode_ii &
       & (ierr,  d,     n,    &
       &  b,     subv,  nsub, vmiss)
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32
    integer,            intent(out) :: ierr
    integer(kind=KARG), intent(out) :: d(0:*)      ! output
    integer,            intent(in)  :: n
    integer(kind=KISRC),intent(out) :: b(0:*)      ! compressed data sequence
    integer,            intent(in)  :: subv(0:*)
    integer,            intent(in)  :: nsub
    real(kind=KRMIS),   intent(in)  :: vmiss

    integer(kind=KARG) :: vm
    integer j, jb, je

    ierr = 0
    d(subv(0:nsub-1)) = int(b(0:nsub-1), kind=KARG)
    vm = int(vmiss, kind=KARG)
    jb = 0
    do j = 0, nsub - 1
       je = subv(j)
       d(jb:je-1) = vm
       jb = je + 1
    enddo
    d(jb:n-1) = vm
    return
  end subroutine subv_decode_ii

!!!_  & tweak_subv - tweak subv along coordinate
  subroutine tweak_subv &
       & (ierr,  &
       &  subv,  ends,  mi, mo, &
       &  srcd,  mh,    mk)
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: subv(0:*)
    integer,intent(out)   :: ends(0:*)
    integer,intent(in)    :: mi, mo
    integer,intent(in)    :: srcd(0:*)
    integer,intent(in)    :: mh, mk
    integer jk, jvg, jvi, jvo
    integer js, jsb, jse
    ierr = 0
    ! NOTE: must be sorted (no check)
    jsb = 0
    ends(0:mo-1) = 0
    do jk = 0, mk - 1
       jse = srcd(jk)
       do js = jsb, jse - 1
          jvg = subv(js) + mh * jk
          jvi = mod(jvg, mi)
          jvo = jvg / mi
          ends(jvo) = ends(jvo) + 1
          subv(js) = jvi
       enddo
       jsb = jse
    enddo
    do jvo = 1, mo - 1
       ends(jvo) = ends(jvo) + ends(jvo-1)
    enddo
  end subroutine tweak_subv

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
    real(kind=KRSRC) :: dv, fv
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
    use TOUZA_Nio_header,only: hi_IDFM, get_item
    implicit none
    character(len=*),intent(in) :: head(*)
    integer jerr
    call get_item(jerr, head, n, hi_IDFM)
    if (jerr.eq.0) then
       if (n.ne.GFMT_ID_LEGACY) n = _ERROR(ERR_NOT_GTOOL_FORMAT)
    else
       n = jerr
    endif
    return
  end function check_id_format

!!!_  & count_bes()
  PURE &
  integer function count_bes(bes, nc) result(n)
    implicit none
    integer,intent(in)          :: bes(3, *)
    integer,intent(in),optional :: nc
    if (present(nc)) then
       n = max(1, product(max(1, bes(2, 1:nc) - bes(1, 1:nc))))
    else
       n = max(1, product(max(1, bes(2, 1:laxs) - bes(1, 1:laxs))))
    endif
  end function count_bes

!!!_  & bes_triplet
  subroutine bes_triplet(ierr, bes, kaxs, start, count)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: bes(3, *)
    integer,intent(in)  :: kaxs(*)
    integer,intent(in)  :: start(:)
    integer,intent(in)  :: count(:)
    integer nr
    ierr = 0
    nr = min(size(start), size(count))
    if (nr.gt.laxs) then
       ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    else
       bes(3, 1:laxs) = max(1, kaxs(1:laxs))
       bes(1, 1:nr)   = max(0, start(1:nr))
       bes(2, 1:nr)   = start(1:nr) + max(0, count(1:nr))
       bes(1, nr+1:laxs) = 0
       bes(2, nr+1:laxs) = bes(3, nr+1:laxs)
    endif
  end subroutine bes_triplet
!!!_ + common work area
  subroutine diag_alloc(cmd, name, otag, ntag, n, u, levv)
    use TOUZA_Nio_std,only: choice, msg
    use TOUZA_Nio_std,only: is_msglev_NORMAL, is_msglev_INFO, is_msglev_FATAL
    implicit none
    character(len=*),intent(in)  :: cmd
    character(len=*),intent(in)  :: name
    character(len=*),intent(in)  :: otag, ntag
    integer,         intent(in)  :: n
    integer,optional,intent(in)  :: u, levv
    integer utmp, lv
    character(len=128) :: txt
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)

101 format('alloc/unlock [', A, '] ', I0, 1x, A, ' > ', A)
111 format('alloc/error [', A, '] ', I0, 1x, A, ' > ', A)
121 format('alloc/lock [', A, '] ', I0, 1x, A, ' > ', A)

    select case(cmd)
    case('u')
       if (otag.ne.ntag) then
          if (is_msglev_NORMAL(lv)) then
             write(txt, 101) trim(name), n, trim(otag), trim(ntag)
             call msg(txt, __MDL__, utmp)
          endif
       else
          if (is_msglev_INFO(lv)) then
             write(txt, 101) trim(name), n, trim(otag), trim(ntag)
             call msg(txt, __MDL__, utmp)
          endif
       endif
    case('e')
       if (is_msglev_FATAL(lv)) then
          write(txt, 111) trim(name), n, trim(otag), trim(ntag)
          call msg(txt, __MDL__, utmp)
       endif
    case default
       if (is_msglev_INFO(lv)) then
          write(txt, 121) trim(name), n, trim(otag), trim(ntag)
          call msg(txt, __MDL__, utmp)
       endif
    end select
  end subroutine diag_alloc

!!!_  - alloc_worki
  subroutine alloc_worki(ierr, n, tag)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: n
    character(len=*),intent(in)  :: tag
    ierr = 0
    if (n.lt.0) then
       call diag_alloc('u', 'worki', wtag_i, tag, size(worki))
       wtag_i = ' '
       return
    endif
    if (wtag_i.ne.' ') then
       call diag_alloc('e', 'worki', wtag_i, tag, size(worki))
       ierr = _ERROR(ERR_PANIC)
       return
    endif
    call diag_alloc('l', 'worki', wtag_i, tag, n)
    if (ierr.eq.0) wtag_i = tag
    if (allocated(worki)) then
       if (n.le.size(worki)) return
       deallocate(worki, STAT=ierr)
    endif
    if (ierr.eq.0) allocate(worki(0:n-1), STAT=ierr)
  end subroutine alloc_worki

!!!_  - alloc_workf
  subroutine alloc_workf(ierr, n, tag)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: n
    character(len=*),intent(in)  :: tag
    ierr = 0
    if (n.lt.0) then
       call diag_alloc('u', 'workf', wtag_f, tag, size(workf))
       wtag_f = ' '
       return
    endif
    if (wtag_f.ne.' ') then
       call diag_alloc('e', 'workf', wtag_f, tag, size(workf))
       ierr = _ERROR(ERR_PANIC)
       return
    endif
    call diag_alloc('l', 'workf', wtag_f, tag, n)
    if (ierr.eq.0) wtag_f = tag
    if (allocated(workf)) then
       if (n.le.size(workf)) return
       deallocate(workf, STAT=ierr)
    endif
    if (ierr.eq.0) allocate(workf(0:n-1), STAT=ierr)
  end subroutine alloc_workf
!!!_  - alloc_workd
  subroutine alloc_workd(ierr, n, tag)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: n
    character(len=*),intent(in)  :: tag
    ierr = 0
    if (n.lt.0) then
       call diag_alloc('u', 'workd', wtag_d, tag, size(workd))
       wtag_d = ' '
       return
    endif
    if (wtag_d.ne.' ') then
       call diag_alloc('e', 'workd', wtag_d, tag, size(workd))
       ierr = _ERROR(ERR_PANIC)
       return
    endif
    call diag_alloc('l', 'workd', wtag_d, tag, n)
    if (ierr.eq.0) wtag_d = tag
    if (allocated(workd)) then
       if (n.le.size(workd)) return
       deallocate(workd, STAT=ierr)
    endif
    if (ierr.eq.0) allocate(workd(0:n-1), STAT=ierr)
  end subroutine alloc_workd

!!!_  - alloc_wpack
  subroutine alloc_wpack(ierr, n, tag, mold)
    implicit none
    integer,parameter :: KISRC=KI32
    integer,            intent(out) :: ierr
    integer,            intent(in)  :: n
    character(len=*),   intent(in)  :: tag
    integer(kind=KISRC),intent(in)  :: mold
    ierr = 0 * mold
    if (n.lt.0) then
       call diag_alloc('u', 'wpack', wtag_p, tag, size(wpack))
       wtag_p = ' '
       return
    endif
    if (wtag_p.ne.' ') then
       call diag_alloc('e', 'wpack', wtag_p, tag, size(wpack))
       ierr = _ERROR(ERR_PANIC)
       return
    endif
    call diag_alloc('l', 'wpack', wtag_p, tag, n)
    if (ierr.eq.0) wtag_p = tag
    if (allocated(wpack)) then
       if (n.le.size(wpack)) return
       deallocate(wpack, STAT=ierr)
    endif
    if (ierr.eq.0) allocate(wpack(0:n-1), STAT=ierr)
  end subroutine alloc_wpack

!!!_  - alloc_wmask
  subroutine alloc_wmask(ierr, n, tag, mold)
    implicit none
    integer,parameter :: KISRC=KI32
    integer,            intent(out) :: ierr
    integer,            intent(in)  :: n
    character(len=*),   intent(in)  :: tag
    integer(kind=KISRC),intent(in)  :: mold
    ierr = 0 * mold
    if (n.lt.0) then
       call diag_alloc('u', 'wmask', wtag_m, tag, size(wmask))
       wtag_m = ' '
       return
    endif
    if (wtag_m.ne.' ') then
       call diag_alloc('e', 'wmask', wtag_m, tag, size(wmask))
       ierr = _ERROR(ERR_PANIC)
       return
    endif
    call diag_alloc('l', 'wmask', wtag_m, tag, n)
    if (ierr.eq.0) wtag_m = tag
    if (allocated(wmask)) then
       if (n.le.size(wmask)) return
       deallocate(wmask, STAT=ierr)
    endif
    if (ierr.eq.0) allocate(wmask(0:n-1), STAT=ierr)
    return
  end subroutine alloc_wmask

!!!_  - alloc_wsubv
  subroutine alloc_wsubv(ierr, n, tag)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: n
    character(len=*),intent(in)  :: tag

    ierr = 0
    if (n.lt.0) then
       call diag_alloc('u', 'wsubv', wtag_s, tag, size(wdsubv))
       wtag_s = ' '
       return
    endif
    if (wtag_s.ne.' ') then
       call diag_alloc('e', 'wsubv', wtag_s, tag, size(wdsubv))
       ierr = _ERROR(ERR_PANIC)
       return
    endif
    call diag_alloc('l', 'wsubv', wtag_s, tag, n)
    if (allocated(wdsubv)) then
       if (n.gt.size(wdsubv)) deallocate(wdsubv, STAT=ierr)
    endif
    if (allocated(wssubv)) then
       if (n.gt.size(wssubv)) deallocate(wssubv, STAT=ierr)
    endif
    if (.not.allocated(wdsubv)) then
       if (ierr.eq.0) allocate(wdsubv(0:n-1), STAT=ierr)
    endif
    if (.not.allocated(wssubv)) then
       if (ierr.eq.0) allocate(wssubv(0:n-1), STAT=ierr)
    endif
    if (ierr.eq.0) wtag_s = tag
  end subroutine alloc_wsubv

!!!_ + misc
!!!_  & pre_review
  subroutine pre_review &
       & (ierr, apini, u, flag)
    use TOUZA_Nio_std,only: sus_getpos, WHENCE_ABS, KIOFS, choice
    implicit none
    integer,            intent(out)         :: ierr
    integer(kind=KIOFS),intent(out)         :: apini
    integer,            intent(in)          :: u
    integer,            optional,intent(in) :: flag
    ierr = 0
    if (present(flag)) continue     ! dummy
    if (ierr.eq.0) call sus_getpos(ierr, apini, u, WHENCE_ABS)
    ! write(*, *) 'pre  = ', apini - 1
  end subroutine pre_review
!!!_  & post_review
  subroutine post_review &
       & (ierr, apini, u, flag)
    use TOUZA_Nio_std,only: sus_rseek, WHENCE_ABS, KIOFS, choice
    implicit none
    integer,            intent(out)         :: ierr
    integer(kind=KIOFS),intent(in)          :: apini
    integer,            intent(in)          :: u
    integer,            optional,intent(in) :: flag
    integer f
    ierr = 0
    f = choice(0, flag)
    if (IAND(f, rev_pos_dhead).ne.0) then
       if (ierr.eq.0) call sus_rseek(ierr, u, apini, whence=WHENCE_ABS)
       ! write(*, *) 'post  = ', apini - 1
    endif
  end subroutine post_review

!!!_ + custom made procedures
!!!_  & nio_count_defined - count defined elements along specific coordinate
  subroutine nio_count_defined &
       & (ierr, ends,  &
       &  head, krect, u, cid, flag)
    use TOUZA_Nio_std,only: sus_rseek, WHENCE_ABS, KIOFS
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ends(0:*) ! displacement array
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: cid
    integer,optional,intent(in)  :: flag

    integer kfmt
    integer kaxs(laxs)
    real(kind=KRMIS) :: vmiss
    integer md

    ierr = err_default
    if (cid.le.0.or.cid.gt.laxs) ierr = _ERROR(ERR_INVALID_PARAMETER)
    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)

    if (ierr.eq.0) then
       md = product(max(1, kaxs(1:laxs)))
       select case (kfmt)
       case (GFMT_UR4,GFMT_UR8,GFMT_UI4)
          call nio_count_defined_urn &
               & (ierr, ends,  &
               &  krect, u, md, cid, laxs, kaxs, kfmt, vmiss, flag)
       case (GFMT_MR4,GFMT_MR8,GFMT_MI4)
          call nio_count_defined_mrn &
               & (ierr, ends,  &
               &  krect, u, md, cid, laxs, kaxs, flag)
       case (GFMT_URC, GFMT_URC2)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_URY:GFMT_URYend-1)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_MRY:GFMT_MRYend-1)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_URT)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_MRT)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_PR8)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_PR4)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case (GFMT_PI4)
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif

    ! if (ierr.eq.0) then
    !    bes(1, 1:laxs) = 0
    !    bes(2, 1:laxs) = kaxs(1:laxs)
    !    bes(3, 1:laxs) = kaxs(1:laxs)
    !    mw = product(max(1, kaxs(1:laxs)))
    !    mh = mw / max(1, kaxs(cid))
    ! endif
    ! if (ierr.eq.0) then
    !    select case (kfmt)
    !    case (GFMT_UR4)
    !       call alloc_workf(ierr, mh)
    !       vmissf = real(vmiss, kind=KFLT)
    !    case (GFMT_UR8)
    !       call alloc_workd(ierr, mh)
    !       vmissd = real(vmiss, kind=KDBL)
    !    case (GFMT_UI4)
    !       call alloc_worki(ierr, mh)
    !       vmissi = int(vmiss)
    !    case (GFMT_MR4,GFMT_MR8,GFMT_MI4)
    !       call alloc_worki(ierr, mh)
    !    case (GFMT_URC, GFMT_URC2)
    !    case (GFMT_URY:GFMT_URYend-1)
    !    case (GFMT_MRY:GFMT_MRYend-1)
    !    case (GFMT_URT)
    !    case (GFMT_MRT)
    !    case (GFMT_PR8)
    !    case (GFMT_PR4)
    !    case (GFMT_PI4)
    !    case default
    !       ierr = _ERROR(ERR_INVALID_SWITCH)
    !    end select
    ! endif
    ! if (ierr.eq.0) inquire(unit=u, POS=jpos, IOSTAT=ierr)
    ! if (ierr.eq.0) then
    !    do jx = 0, kaxs(cid) - 1
    !       bes(1:2, cid) = (/jx, jx+1/)
    !       write(*, *) ierr, jx, bes(1:3, 1:laxs)
    !       select case (kfmt)
    !       case (GFMT_UR4)
    !          if (ierr.eq.0) call get_data_record_slice(ierr, workf, mh, u, krect, mw, bes, laxs)
    !          if (ierr.eq.0) ends(jx) = COUNT(workf(0:mh-1).ne.vmissf)
    !       case (GFMT_UR8)
    !          if (ierr.eq.0) call get_data_record_slice(ierr, workd, mh, u, krect, mw, bes, laxs)
    !          if (ierr.eq.0) ends(jx) = COUNT(workd(0:mh-1).ne.vmissd)
    !       case (GFMT_UI4)
    !          if (ierr.eq.0) call get_data_record_slice(ierr, worki, mh, u, krect, mw, bes, laxs)
    !          if (ierr.eq.0) ends(jx) = COUNT(worki(0:mh-1).ne.vmissi)
    !       case (GFMT_MR4)
    !       case (GFMT_MR8)
    !       case (GFMT_MI4)
    !       case (GFMT_URC, GFMT_URC2)
    !       case (GFMT_URY:GFMT_URYend-1)
    !       case (GFMT_MRY:GFMT_MRYend-1)
    !       case (GFMT_URT)
    !       case (GFMT_MRT)
    !       case (GFMT_PR8)
    !       case (GFMT_PR4)
    !       case (GFMT_PI4)
    !       case default
    !          ierr = _ERROR(ERR_INVALID_SWITCH)
    !       end select
    !       if (ierr.eq.0) call sus_rseek(ierr, u, jpos, whence=WHENCE_ABS)
    !    enddo
    ! endif
    ! if (ierr.eq.0) then
    !    select case (kfmt)
    !    case (GFMT_UR4,GFMT_UR8,GFMT_UI4)
    !       do jx = 1, kaxs(cid) - 1
    !          ends(jx) = ends(jx-1) + ends(jx)
    !       enddo
    !    end select
    ! endif
    !    select case (kfmt)
    !    case (GFMT_UR4)
    !    case (GFMT_UR8)
    !    case (GFMT_UI4)
    !    case (GFMT_MR4)
    !    case (GFMT_MR8)
    !    case (GFMT_MI4)
    !    case (GFMT_URC, GFMT_URC2)
    !    case (GFMT_URY:GFMT_URYend-1)
    !    case (GFMT_MRY:GFMT_MRYend-1)
    !    case (GFMT_URT)
    !    case (GFMT_MRT)
    !    case (GFMT_PR8)
    !    case (GFMT_PR4)
    !    case (GFMT_PI4)
    !    case default
    !       ierr = _ERROR(ERR_INVALID_SWITCH)
    !    end select
    ! endif
    ! if (ierr.eq.0) then
    !    select case (kfmt)
    !    case (GFMT_UR4)
    !       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    !    case (GFMT_UR8)
    !       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    !    case (GFMT_UI4)
    !       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    !    case (GFMT_MR4)
    !       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    !    case (GFMT_MR8)
    !       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    !    case (GFMT_MI4)
    !       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    !    case (GFMT_URC, GFMT_URC2)
    !       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    !    case (GFMT_URY:GFMT_URYend-1)
    !       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    !    case (GFMT_MRY:GFMT_MRYend-1)
    !       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    !    case (GFMT_URT)
    !       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    !    case (GFMT_MRT)
    !       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    !    case (GFMT_PR8)
    !       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    !    case (GFMT_PR4)
    !       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    !    case (GFMT_PI4)
    !       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    !    case default
    !       ierr = _ERROR(ERR_INVALID_SWITCH)
    !    end select
    ! endif
  end subroutine nio_count_defined
!!!_  & nio_count_defined_urn - count defined elements along specific coordinate (U[IR]n)
  subroutine nio_count_defined_urn &
       & (ierr,  ends,  &
       &  krect, u, md, cid, nr, kaxs, kfmt, vmiss, flag)
    use TOUZA_Nio_std,only: sus_rseek, WHENCE_ABS, KIOFS, choice, sus_getpos
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ends(0:*) ! displacement array
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: md
    integer,         intent(in)  :: cid
    integer,         intent(in)  :: nr
    integer,         intent(in)  :: kaxs(*)
    integer,         intent(in)  :: kfmt
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,optional,intent(in)  :: flag

    integer bes(3, nr)
    real(kind=KDBL)  :: vmissd
    real(kind=KFLT)  :: vmissf
    integer          :: vmissi
    integer jx
    integer mw, mh
    integer(kind=KIOFS) :: jpos
    integer f

    ierr = err_default
    if (ierr.eq.0) then
       bes(1, 1:nr) = 0
       bes(2, 1:nr) = kaxs(1:nr)
       bes(3, 1:nr) = kaxs(1:nr)
       mw = product(max(1, kaxs(1:nr)))
       mh = mw / max(1, kaxs(cid))
    endif
    if (ierr.eq.0) call sus_getpos(ierr, jpos, u)
    if (ierr.eq.0) then
       select case (kfmt)
       case (GFMT_UR4)
          vmissf = real(vmiss, kind=KFLT)
          call alloc_workf(ierr, mh, 'def:urn')
          do jx = 0, kaxs(cid) - 1
             bes(1:2, cid) = (/jx, jx+1/)
             if (ierr.eq.0) call sus_rseek(ierr, u, jpos, whence=WHENCE_ABS)
             if (ierr.eq.0) call get_data_record_slice(ierr, workf, mh, u, krect, md, bes, nr)
             if (ierr.eq.0) ends(jx) = COUNT(workf(0:mh-1).ne.vmissf)
             ! write(*, *) 'ur4', ierr, jx, ends(jx)
          enddo
          call alloc_workf(ierr, -1, 'def:urn')
       case (GFMT_UR8)
          vmissd = real(vmiss, kind=KDBL)
          call alloc_workd(ierr, mh, 'def:urn')
          do jx = 0, kaxs(cid) - 1
             bes(1:2, cid) = (/jx, jx+1/)
             if (ierr.eq.0) call sus_rseek(ierr, u, jpos, whence=WHENCE_ABS)
             if (ierr.eq.0) call get_data_record_slice(ierr, workd, mh, u, krect, md, bes, nr)
             if (ierr.eq.0) ends(jx) = COUNT(workd(0:mh-1).ne.vmissd)
             ! write(*, *) 'ur8', ierr, jx, ends(jx)
          enddo
          call alloc_workd(ierr, -1, 'def:urn')
       case (GFMT_UI4)
          vmissi = int(vmiss)
          call alloc_worki(ierr, mh, 'def:urn')
          do jx = 0, kaxs(cid) - 1
             if (ierr.eq.0) call sus_rseek(ierr, u, jpos, whence=WHENCE_ABS)
             if (ierr.eq.0) call get_data_record_slice(ierr, worki, mh, u, krect, md, bes, nr)
             if (ierr.eq.0) ends(jx) = COUNT(worki(0:mh-1).ne.vmissi)
          enddo
          call alloc_worki(ierr, -1, 'def:urn')
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    if (ierr.eq.0) then
       do jx = 1, kaxs(cid) - 1
          ends(jx) = ends(jx-1) + ends(jx)
       enddo
    endif
    f = choice(0, flag)
    if (IAND(f, rev_pos_dhead).ne.0) then
       if (ierr.eq.0) call sus_rseek(ierr, u, jpos, whence=WHENCE_ABS)
    endif
  end subroutine nio_count_defined_urn
!!!_  & nio_count_defined_mrn - count defined elements along specific coordinate (M[IR]n)
  subroutine nio_count_defined_mrn &
       & (ierr,  ends,  &
       &  krect, u, md, cid, nr, kaxs, flag)
    use TOUZA_Nio_std,only: sus_rseek, WHENCE_ABS, KIOFS, choice, sus_getpos
    use TOUZA_Trp,only: count_packed, mask_count_defined
    implicit none
    integer,parameter :: KISRC=KI32
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ends(0:*) ! displacement array
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,         intent(in)  :: md
    integer,         intent(in)  :: cid
    integer,         intent(in)  :: nr
    integer,         intent(in)  :: kaxs(*)
    integer,optional,intent(in)  :: flag

    integer bes(3, nr)
    integer jx
    integer(kind=KIOFS) :: jpos
    integer ncom
    integer kpack
    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer f

    ierr = err_default
    if (ierr.eq.0) then
       bes(1, 1:nr) = 0
       bes(2, 1:nr) = kaxs(1:nr)
       bes(3, 1:nr) = kaxs(1:nr)
       kpack = legacy_unpacking(1, md)
       ncom = count_packed(1, md, mold)
       call alloc_worki(ierr, ncom, 'def:mrn')
    endif
    if (ierr.eq.0) call sus_getpos(ierr, jpos, u)
    if (ierr.eq.0) call nio_skip_prec(ierr, u, 1, krect)   ! mb
    if (ierr.eq.0) call get_data_record(ierr, worki, ncom, u, krect) ! mask

    if (ierr.eq.0) then
       do jx = 0, kaxs(cid) - 1
          bes(1:2, cid) = (/jx, jx+1/)
          if (ierr.eq.0) then
             call mask_count_defined(ierr, ends(jx), worki, md, bes, nr, kpack)
          endif
       enddo
    endif
    if (ierr.eq.0) then
       do jx = 1, kaxs(cid) - 1
          ends(jx) = ends(jx-1) + ends(jx)
       enddo
    endif
    f = choice(0, flag)
    if (IAND(f, rev_pos_dhead).ne.0) then
       if (ierr.eq.0) call sus_rseek(ierr, u, jpos, whence=WHENCE_ABS)
    else
       if (ierr.eq.0) call nio_skip_prec(ierr, u, 1, krect)   ! data
    endif
    call alloc_worki(ierr, -1, 'def:mrn')
  end subroutine nio_count_defined_mrn
!!!_ + end module TOUZA_Nio_record
end module TOUZA_Nio_record

!!!_@ test_nio_record - test program
#ifdef TEST_NIO_RECORD
program test_nio_record
  use TOUZA_Std,       only: parse, get_param, arg_diag, arg_init
  use TOUZA_Std,       only: MPI_COMM_NULL
  use TOUZA_Nio_std,   only: KDBL,  KIOFS, sus_write_irec, sus_write_lrec
  use TOUZA_Nio_header,only: nitem, litem
  use TOUZA_Nio_record
  implicit none
  integer ierr
  integer jarg
  integer ktest

  ierr = 0
  jarg = 0
101 format(A,' = ', I0)
  call init(ierr, stdv=-9, icomm=MPI_COMM_NULL)
  ! call init(ierr, stdv=-9, icomm=MPI_COMM_NULL, levv=+99)
  ! if (ierr.eq.0) call diag(ierr, u=-1, levv=+99)
  if (ierr.eq.0) call diag(ierr, levv=+9)
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
     case (4)
        write(*, 201) ktest, 'read gtool file backward'
        call test_read_backward(ierr, jarg)
     case (5)
        write(*, 201) ktest, 'read gtool file skipping'
        call test_read_skip(ierr, jarg)
     case (6)
        write(*, 201) ktest, 'parse_urt_options'
        call test_batch_parse_urt(ierr, jarg)
     case (7)
        write(*, 201) ktest, 'read slice'
        call test_batch_read_slice(ierr, jarg)
     case (8)
        write(*, 201) ktest, 'various properties'
        call test_batch_read_props(ierr, jarg)
     case (9)
        write(*, 201) ktest, 'packed storage'
        call test_batch_read_packed(ierr, jarg)
     case (10)
        write(*, 201) ktest, 'byte order'
        call test_batch_byte_order(ierr, jarg)
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
    ! call test_record_fmt(ierr, 'URT24', GFMT_URT+24)

    call test_record_fmt(ierr, 'MR8',   GFMT_MR8)
    call test_record_fmt(ierr, 'MR4',   GFMT_MR4)
    call test_record_fmt(ierr, 'MRY01', GFMT_MRY + 1)
    call test_record_fmt(ierr, 'MRY31', GFMT_MRY + 31)
    call test_record_fmt(ierr, 'MI4',   GFMT_MI4)

    call test_record_fmt(ierr, 'PR8',   GFMT_PR8)
    call test_record_fmt(ierr, 'PR4',   GFMT_PR4)
    call test_record_fmt(ierr, 'PI4',   GFMT_PI4)

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
    use TOUZA_std,only: is_error_match
    use TOUZA_Nio_std,   only: KDBL,  KIOFS, &
         & sus_open, sus_close, sus_write_irec, sus_write_lrec
    use TOUZA_Nio_header,only: nitem, litem, hi_ITEM, put_item, get_item
    use TOUZA_Nio_record
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jarg

    integer,parameter :: nhi = nitem + 4
    character(len=litem) hd(nhi)
    character(len=litem) hitm
    integer,parameter :: lxh = 4
    character(len=litem) xhd(lxh)
    integer krect
    integer kfmt, kaxs(3)
    real(KIND=KDBL) :: vmiss
    character(len=1024) :: file
    integer lp

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
       call sus_open(ierr, udat, file, ACTION='W', STATUS='R')
       do lp = 0, 1
          if (ierr.eq.0) call put_item(ierr, hd, 'def', hi_ITEM)
          if (ierr.eq.0) call sus_write_irec(ierr, udat, hd, nhi)
          if (ierr.eq.0) call put_item(ierr, hd, 'swap', hi_ITEM)
          if (ierr.eq.0) call sus_write_irec(ierr, udat, hd, nhi, swap=.TRUE.)
          if (ierr.eq.0) call put_item(ierr, hd, 'long', hi_ITEM)
          if (ierr.eq.0) call sus_write_lrec(ierr, udat, hd, nhi)
          if (ierr.eq.0) call put_item(ierr, hd, 'long:swap', hi_ITEM)
          if (ierr.eq.0) call sus_write_lrec(ierr, udat, hd, nhi, swap=.TRUE.)

          if (ierr.eq.0) call put_item(ierr, hd, 'cont', hi_ITEM)
          if (ierr.eq.0) call sus_write_irec(ierr, udat, hd,  nhi, post=.TRUE.)
          if (ierr.eq.0) call sus_write_irec(ierr, udat, xhd, lxh, pre=.TRUE.)

          if (ierr.eq.0) call put_item(ierr, hd, 'cont:swap', hi_ITEM)
          if (ierr.eq.0) call sus_write_irec(ierr, udat, hd,  nhi, swap=.TRUE., post=.TRUE.)
          if (ierr.eq.0) call sus_write_irec(ierr, udat, xhd, lxh, swap=.TRUE., pre=.TRUE.)
       enddo
       if (ierr.eq.0) call sus_close(ierr, udat, file)
    endif
301 format('FULL/W:', I0, 1x, I0, 1x, I0)
    write (*, 301) ierr

    if (ierr.eq.0) call sus_open(ierr, udat, file, ACTION='R')

    jrec = 0
401 format('FULL:', I0, 1x, I0, 1x, I0)
402 format('FULL:EOF', 1x, I0, 1x, I0)
411 format(I0, ': ', A, '/')
421 format('  FMT=', 2(1x, I0), 1x, 'COOR=', 3(1x,I0), 1x, E16.9)
    do
       if (ierr.eq.0) then
          hd(:) = ' '
          call nio_read_header(ierr, hd, krect, udat)
          if (is_error_match(ierr, ERR_EOF)) then
             write (*, 402) jrec, krect
             ierr = 0
             exit
          else
             write (*, 401) ierr, jrec, krect
          endif
       endif
       if (ierr.ne.0) exit
       if (ierr.eq.0) then
          call get_item(ierr, hd, hitm, hi_ITEM)
          if (hitm.ne.' ') write (*, 411) jrec, TRIM(ADJUSTL(hitm))
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
    use TOUZA_Nio_header
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
    use TOUZA_Nio_std,only: KBUF=>KDBL
    use TOUZA_Nio_header
    use TOUZA_Std_sus,only: sus_open, sus_close
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
    jpos = -1

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

    if (ierr.eq.0) call sus_open(ierr, uread,  rfile, ACTION='R')
    if (ierr.eq.0) then
       if (wfile.eq.' ') then
          uwrite = -1
       else
          uwrite = uread + 1
          call sus_open(ierr, uwrite, wfile, ACTION='W', STATUS='R')
       endif
    endif

    if (ierr.eq.0) allocate(v(1:lmax), STAT=ierr)

    if (ierr.eq.0) krect = nio_check_magic_file(uread)

    if (ierr.eq.0) then
       if (krect.lt.0) then
411       format('not a gtool file: ', I0, 1x, A)
          write(*, 411) krect, trim(rfile)
       else
          jrec = 0
          do
             if (ierr.eq.0) call nio_read_header(ierr, hd, krect, uread)
             write (*, 401) ierr, jrec, krect
             if (ierr.eq.0) call nio_read_data(ierr, v, lmax, hd, krect, uread)
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
                   if (ierr.eq.0) call nio_write_header(ierr, hd, krectw, uwrite)
                   write(*, 501) ierr, jrec, l
                   if (ierr.eq.0) call nio_write_data(ierr, v, lmax, hd, krectw, uwrite)
                   write(*, 502) ierr, jrec, l
                   if (ierr.eq.0) inquire(unit=uwrite, IOSTAT=ierr, pos=JPOS)
                   do l = 0, nloop - 1
                      if (ierr.eq.0) call nio_write_header(ierr, hd, krectw, uwrite)
                      if (ierr.eq.0) call nio_write_data(ierr, v, lmax, hd, krectw, uwrite)
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
       endif
    endif
    if (ierr.eq.0) call sus_close(ierr, uread, rfile)
    if (ierr.eq.0.and.uwrite.ge.0) call sus_close(ierr, uwrite, wfile)
    if (ierr.eq.0) deallocate(v, STAT=ierr)
    return
  end subroutine test_read_write_ext

!!!_ + test_read_backward - read external gtool file (backward)
  subroutine test_read_backward &
       & (ierr, jarg)
    use TOUZA_Std,    only: get_param, upcase
    use TOUZA_Nio_std,only: KBUF=>KDBL
    use TOUZA_Nio_header
    use TOUZA_Std_sus,only: sus_open, sus_close, sus_rseek, WHENCE_ABS
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jarg
    character(len=256) :: rfile

    integer uread
    integer jrec
    integer j, n
    integer roff

    integer,parameter :: nhi = nitem + 4
    character(len=litem) hd(nhi)
    integer krect
    integer,parameter :: lmax = 2 ** 24
    real(kind=KBUF),allocatable :: v(:)
    integer(kind=KIOFS) :: jpini

    ierr = 0
101 format('test/bwd:', I0, 1x, A)
401 format('header/r:', I0, 1x, I0, 1x, I0)
402 format('data/r:', I0, 1x, I0)
301 format('v:', I0, 1x, I0, 1x, E24.16)

    jarg = jarg + 1
    if (ierr.eq.0) call get_param(ierr, rfile, jarg, ' ')
    if (rfile.eq.' ') return
    jarg = jarg + 1
    if (ierr.eq.0) call get_param(ierr, roff, jarg, 0)

    ierr = 0

    write(*, 101) jarg, trim(rfile)

    uread = 21

    if (ierr.eq.0) call sus_open(ierr, uread, rfile, ACTION='R', POSITION='AP')

    if (ierr.eq.0) allocate(v(1:lmax), STAT=ierr)

    jrec = 0
    ! krect = REC_DEFAULT
    krect = REC_SWAP + REC_LSEP  ! force check from farthest
    do
       if (ierr.eq.0) call nio_bwd_record(ierr, uread, krect=krect)
       if (ierr.eq.0) inquire(unit=uread, IOSTAT=ierr, pos=jpini)
       if (ierr.eq.0) call nio_read_header(ierr, hd, krect, uread)
       write (*, 401) ierr, jrec, krect
       if (ierr.eq.0) call nio_read_data(ierr, v, lmax, hd, krect, uread)
       write (*, 402) ierr, jrec
       if (ierr.eq.0) then
          n = parse_header_size(hd, 0)
          do j = 1, n
             write(*, 301) roff - jrec, j, v(j)
          enddo
       endif
       if (ierr.eq.0) call sus_rseek(ierr, uread, jpini, WHENCE_ABS)
       if (ierr.ne.0) exit
       if (jpini.le.1) exit
       jrec = jrec + 1
    enddo
    if (ierr.eq.0) call sus_close(ierr, uread, rfile)
    if (ierr.eq.0) deallocate(v, STAT=ierr)
    return
  end subroutine test_read_backward

!!!_ + test_read_skip - read external gtool file (skip)
  subroutine test_read_skip &
       & (ierr, jarg)
    use TOUZA_Std,    only: get_param, upcase
    use TOUZA_Nio_std,only: KBUF=>KDBL
    use TOUZA_Nio_header
    use TOUZA_Std_sus,only: sus_open, sus_close, sus_rseek, WHENCE_ABS
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jarg
    character(len=256) :: rfile

    integer uread
    integer jrec
    integer roff
    integer nrec

    integer,parameter :: nhi = nitem + 4
    character(len=litem) hd(nhi)
    integer krect
    integer,parameter :: lmax = 2 ** 24
    real(kind=KBUF),allocatable :: v(:)
    integer dnum

    ierr = 0
101 format('test/skip:', I0, 1x, A)
! 401 format('header/r:', I0, 1x, I0, 1x, I0)
! 402 format('data/r:', I0, 1x, I0)
301 format('dnum:', I0, 1x, I0, 1x, I0, 1x, I0)

    jarg = jarg + 1
    if (ierr.eq.0) call get_param(ierr, rfile, jarg, ' ')
    if (rfile.eq.' ') return
    jarg = jarg + 1
    if (ierr.eq.0) call get_param(ierr, roff, jarg, 0)

    ierr = 0

    write(*, 101) jarg, trim(rfile)

    uread = 21

    if (ierr.eq.0) call sus_open(ierr, uread, rfile, ACTION='R')
    if (ierr.eq.0) allocate(v(1:lmax), STAT=ierr)

    jrec = 0
    ! krect = REC_DEFAULT

    nrec = 1
    if (ierr.eq.0) call nio_read_header(ierr, hd, krect, uread)
    if (ierr.eq.0) call nio_read_data(ierr, v, lmax, hd, krect, uread)
    if (ierr.eq.0) call get_item(ierr, hd, dnum, hi_DNUM)
    if (ierr.eq.0) write(*, 301) ierr, dnum, jrec, nrec
    if (ierr.eq.0) jrec = jrec + nrec

    nrec = 5
    if (ierr.eq.0) call nio_skip_records(ierr, nrec, uread)
    if (ierr.eq.0) call nio_read_header(ierr, hd, krect, uread)
    if (ierr.eq.0) call get_item(ierr, hd, dnum, hi_DNUM)
    if (ierr.eq.0) jrec = jrec + nrec
    if (ierr.eq.0) write(*, 301) ierr, dnum, jrec, nrec

    nrec = 5
    if (ierr.eq.0) call nio_skip_records(ierr, nrec, uread, head=hd, krect=krect)
    if (ierr.eq.0) call nio_read_header(ierr, hd, krect, uread)
    if (ierr.eq.0) call nio_read_data(ierr, v, lmax, hd, krect, uread)
    if (ierr.eq.0) call get_item(ierr, hd, dnum, hi_DNUM)
    if (ierr.eq.0) jrec = jrec + nrec
    if (ierr.eq.0) write(*, 301) ierr, dnum, jrec, nrec

    nrec = -5
    if (ierr.eq.0) call nio_skip_records(ierr, nrec, uread)
    if (ierr.eq.0) call nio_read_header(ierr, hd, krect, uread)
    if (ierr.eq.0) call get_item(ierr, hd, dnum, hi_DNUM)
    if (ierr.eq.0) jrec = jrec + nrec
    if (ierr.eq.0) write(*, 301) ierr, dnum, jrec, nrec

    nrec = -5
    if (ierr.eq.0) call nio_skip_records(ierr, nrec, uread, head=hd, krect=krect)
    if (ierr.eq.0) call nio_read_header(ierr, hd, krect, uread)
    if (ierr.eq.0) call nio_read_data(ierr, v, lmax, hd, krect, uread)
    if (ierr.eq.0) call get_item(ierr, hd, dnum, hi_DNUM)
    if (ierr.eq.0) jrec = jrec + nrec
    if (ierr.eq.0) write(*, 301) ierr, dnum, jrec, nrec

    if (ierr.eq.0) call sus_close(ierr, uread, rfile)
    if (ierr.eq.0) deallocate(v, STAT=ierr)
    return
  end subroutine test_read_skip

!!!_ + test_encoding - write extreme data
  subroutine test_encoding &
       & (ierr, jarg)
    use TOUZA_Std,only: endian_LITTLE, endian_BIG
    use TOUZA_Nio_std,only: KBUF=>KDBL
    use TOUZA_Std_sus,only: sus_open,  sus_close
    use TOUZA_Nio_header
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jarg
    integer nx, ny, nz, nn
    real(kind=KBUF) :: vmiss = -999.0_KBUF
    real(kind=KBUF),allocatable :: v(:)
    integer krect
    integer ufile
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

    if (ierr.eq.0) call sus_open(ierr, ufile, wfile, ACTION='RW', STATUS='R')

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

    if (ierr.eq.0) call sus_close(ierr, ufile, wfile)

  end subroutine test_encoding
!!!_  - test_encoding_sub
  subroutine test_encoding_sub &
       & (ierr, hd, v, n, nz, vmiss, ufile, tag)
    use TOUZA_Nio_std,only: KBUF=>KDBL
    use TOUZA_Nio_header
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
    use TOUZA_Nio_std,only: KBUF=>KDBL
    use TOUZA_Nio_header
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
    integer jpos, jposb

    ierr = 0
    jpos = 0
    nh = n / nz

101 format('############ ', A, 1x, A)
    write(*, 101) trim(tag), trim(tmod)

    ! if (ierr.eq.0) call sus_open(ierr, ufile,  wfile, ACTION='W')

    if (ierr.eq.0) inquire(UNIT=ufile, IOSTAT=ierr, POS=jpos)

    write(txt, '(''extreme:'', A,''/'',A)') trim(tag), trim(tmod)

    if (ierr.eq.0) call put_item(ierr, hd, trim(txt), hi_ITEM)

    if (ierr.eq.0) call test_encoding_write(ierr, hd, v, n, ufile, 'URY01')
    if (ierr.eq.0) call test_encoding_write(ierr, hd, v, n, ufile, 'MRY01')
    if (ierr.eq.0) call test_encoding_write(ierr, hd, v, n, ufile, 'URY02')
    if (ierr.eq.0) call test_encoding_write(ierr, hd, v, n, ufile, 'MRY02')
    if (ierr.eq.0) call test_encoding_write(ierr, hd, v, n, ufile, 'URY31')
    if (ierr.eq.0) call test_encoding_write(ierr, hd, v, n, ufile, 'MRY31')

    if (ierr.eq.0) call test_encoding_write(ierr, hd, v, n, ufile, 'PR8')
    if (ierr.eq.0) call test_encoding_write(ierr, hd, v, n, ufile, 'PR4')
    if (ierr.eq.0) call test_encoding_write(ierr, hd, v, n, ufile, 'PI4')

    if (ierr.eq.0) inquire(UNIT=ufile, IOSTAT=ierr, POS=jposb)
    if (ierr.eq.0) write(UNIT=ufile, IOSTAT=ierr, POS=jpos)

    if (ierr.eq.0) allocate(w(1:n), STAT=ierr)
    jrec = 0
301 format('extreme:check:', I0, 1x, A, 1x, I0, 1x, E16.9, 1x, '[', 2E9.1, '] [', 2E9.1, ']')
    do
       w(1:n) = 123.4e5_KBUF
       if (ierr.eq.0) call nio_read_header(ierr, hdi, krecti, ufile)
       if (ierr.ne.0) then
          ierr = 0
          exit
       endif
       if (ierr.eq.0) call nio_read_data(ierr, w, n, hdi, krecti, ufile)
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
    if (ierr.eq.0) write(UNIT=ufile, IOSTAT=ierr, POS=jposb)
    ! if (ierr.eq.0) call sus_close(ierr, ufile, wfile)
    if (ierr.eq.0) deallocate(w, STAT=ierr)
    return
  end subroutine test_encoding_check

  subroutine test_encoding_write &
       & (ierr, hd, v, n, u, fmt)
    use TOUZA_Nio_std,only: KBUF=>KDBL
    use TOUZA_Nio_header
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(inout) :: hd(*)
    real(kind=KBUF), intent(in)    :: v(*)
    integer,         intent(in)    :: n
    integer,         intent(in)    :: u
    character(len=*),intent(in)    :: fmt
    integer,save :: dnum = 0

    integer krectw

    ierr = 0
    krectw = REC_ASIS
    ! krectw = REC_DEFAULT
    if (ierr.eq.0) call put_item(ierr, hd, trim(fmt), hi_DFMT)
    if (ierr.eq.0) call put_item(ierr, hd, dnum, hi_DNUM)
    if (ierr.eq.0) call nio_write_header(ierr, hd, krectw, u)
    if (ierr.eq.0) call nio_write_data(ierr, v, n, hd, krectw, u)
101 format('extreme:', A, ': ', I0, 1x, I0)
    write(*, 101) trim(fmt), ierr, dnum
    dnum = dnum + 1
    return
  end subroutine test_encoding_write

!!!_ + test_batch_parse_urt - urt option parser tests
  subroutine test_batch_parse_urt &
       & (ierr, jarg)
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jarg
    ierr = 0
    call test_parse_urt(ierr, 'x=7')
    call test_parse_urt(ierr, 'x=7,m=13')
    call test_parse_urt(ierr, 'x=7,m=13,e=-6/8')
    call test_parse_urt(ierr, 'x=7,m=13,e=-6/8,')
    call test_parse_urt(ierr, 'x=7,m=13,e=-6/')
    call test_parse_urt(ierr, 'x=7,m=13,e=-6')
    call test_parse_urt(ierr, 'x=7,m=13,e=/8')
    call test_parse_urt(ierr, 'x=7,m=13,e=/')
    call test_parse_urt(ierr, 'x=7,m=13,r=1e3/1e10')
    call test_parse_urt(ierr, 'x=7,m=13,r=1e3/1e10/1')
    call test_parse_urt(ierr, 'x=7,m=13,r=1e3/1e10/1,b=30000')
    call test_parse_urt(ierr, 'x=7,m=13,r=1e3/1e10/1,b=l')
    call test_parse_urt(ierr, 'x=7,m=13,r=1/64')
    call test_parse_urt(ierr, 'x=7,m=13,r=1/64/1')
    call test_parse_urt(ierr, 'x=7,m=13,r=1/64/2')

    return
  end subroutine test_batch_parse_urt

  subroutine test_parse_urt &
       & (ierr, str)
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: str
    integer,parameter :: lo = lopts
    integer kopts(lo)

    ierr = 0

    call parse_urt_options(ierr, kopts, str)
    call show_urt_options(ierr, kopts, str)

  end subroutine test_parse_urt
!!!_ + test_batch_read_slice
  subroutine test_batch_read_slice &
       & (ierr, jarg)
    use TOUZA_Std,    only: get_param, upcase, get_option
    use TOUZA_Nio_std,only: KBUF=>KDBL
    use TOUZA_Nio_std,only: is_error_match
    use TOUZA_Nio_std,only: is_eof_ss
    use TOUZA_Nio_header
    use TOUZA_Std_sus,only: sus_open, sus_close, sus_rseek, WHENCE_ABS
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jarg
    character(len=256) :: rfile

    integer uread
    integer jrec
    integer j, n

    integer,parameter :: nhi = nitem + 4
    character(len=litem) hd(nhi)
    integer krect
    integer,parameter :: lmax = 2 ** 24
    real(kind=KBUF),allocatable :: v(:)
    integer start(3), count(3)
    integer sc(2)

    ierr = 0
101 format('test/slice:', I0, 1x, A)
401 format('header/r:', I0, 1x, I0, 1x, I0)
402 format('data/r:', I0, 1x, I0)
403 format('open/r:', I0)
301 format('v:', I0, 1x, I0, 1x, E24.16)

    jarg = jarg + 1
    if (ierr.eq.0) call get_param(ierr, rfile, jarg, ' ')
    if (rfile.eq.' ') return
    ierr = 0

    if (ierr.eq.0) call get_option(ierr, sc, 'X', -1)
    if (ierr.eq.0) start(1) = max(0, sc(1))
    if (ierr.eq.0) count(1) = max(1, sc(2) - sc(1))
    if (ierr.eq.0) call get_option(ierr, sc, 'Y', -1)
    if (ierr.eq.0) start(2) = max(0, sc(1))
    if (ierr.eq.0) count(2) = max(1, sc(2) - sc(1))
    if (ierr.eq.0) call get_option(ierr, sc, 'Z', -1)
    if (ierr.eq.0) start(3) = max(0, sc(1))
    if (ierr.eq.0) count(3) = max(1, sc(2) - sc(1))

    write(*, 101) jarg, trim(rfile)

    uread = 21

    if (ierr.eq.0) call sus_open(ierr, uread, rfile, ACTION='R', STATUS='O')
    write (*, 403) ierr

    if (ierr.eq.0) allocate(v(0:lmax), STAT=ierr)

    jrec = 0
    ! krect = REC_DEFAULT
    krect = REC_SWAP + REC_LSEP  ! force check from farthest
    do
       if (ierr.eq.0) call nio_read_header(ierr, hd, krect, uread)
       write (*, 401) ierr, jrec, krect
       if (is_error_match(ierr, ERR_EOF)) then
          ierr = 0
          exit
       endif
       if (ierr.eq.0) call nio_read_data(ierr, v, lmax, hd, krect, uread, start=start, count=count)
       write (*, 402) ierr, jrec
       if (ierr.eq.0) then
          n = product(count(1:3))
          do j = 0, n - 1
             write(*, 301) jrec, j, v(j)
          enddo
       endif
       if (ierr.ne.0) exit
       jrec = jrec + 1
    enddo
    if (ierr.eq.0) call sus_close(ierr, uread, rfile)
    if (ierr.eq.0) deallocate(v, STAT=ierr)
    return
  end subroutine test_batch_read_slice

!!!_ + test_batch_read_props
  subroutine test_batch_read_props &
       & (ierr, jarg)
    use TOUZA_Std,    only: get_param, upcase, get_option
    use TOUZA_Nio_std,only: KBUF=>KDBL
    use TOUZA_Nio_std,only: is_error_match
    use TOUZA_Nio_std,only: is_eof_ss
    use TOUZA_Nio_header
    use TOUZA_Std_sus,only: sus_open, sus_close, sus_rseek, WHENCE_ABS
    use TOUZA_Nio_record,only: REC_DEFAULT
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jarg
    character(len=256) :: rfile
    integer cid

    integer jrec, krect, uread
    character(len=litem) hd(nitem)
    integer,parameter :: lmax = 2 ** 24
    integer,allocatable :: ends(:)
    integer nx

    ierr = 0

    jarg = jarg + 1
    if (ierr.eq.0) call get_param(ierr, rfile, jarg, ' ')
    if (rfile.eq.' ') return
    ierr = 0

    if (ierr.eq.0) call get_option(ierr, cid, 'C', 0)
    if (ierr.eq.0) then
       cid = min(max(1, cid), 3)
    endif

101 format('test/props:', I0, 1x, A)
401 format('header/r:', I0, 1x, I0, 1x, I0)
403 format('open/r:', I0)
201 format('ends:', 32(1x, I0))
209 format('ends: failed ', I0)

    write(*, 101) jarg, trim(rfile)

    uread = 21
    if (ierr.eq.0) call sus_open(ierr, uread, rfile, ACTION='R', STATUS='O')
    write (*, 403) ierr
    if (ierr.eq.0) allocate(ends(0:lmax), STAT=ierr)
    if (ierr.eq.0) ends(0) = 0

    jrec = 0
    krect = REC_DEFAULT
    do
       if (ierr.eq.0) call nio_read_header(ierr, hd, krect, uread)
       write (*, 401) ierr, jrec, krect
       if (is_error_match(ierr, ERR_EOF)) then
          ierr = 0
          exit
       endif
       if (ierr.ne.0) exit
       if (ierr.eq.0) nx = parse_header_size(hd, cid)
       if (ierr.eq.0) call nio_count_defined(ierr, ends(1:), hd, krect, uread, cid)
       if (ierr.eq.0) then
          write(*, 201) ends(1:nx) - ends(0:nx-1)
       else
          write(*, 209) ierr
       endif
    enddo
    if (ierr.eq.0) call sus_close(ierr, uread, rfile)
    if (ierr.eq.0) deallocate(ends, STAT=ierr)
    return
  end subroutine test_batch_read_props

!!!_ + test_batch_read_packed
  subroutine test_batch_read_packed &
       & (ierr, jarg)
    use TOUZA_Std,    only: get_param, upcase, get_option
    use TOUZA_Nio_std,only: KBUF=>KDBL
    use TOUZA_Nio_std,only: is_error_match
    use TOUZA_Nio_std,only: is_eof_ss
    use TOUZA_Nio_header
    use TOUZA_Std_sus,only: sus_open, sus_close, sus_rseek, WHENCE_ABS
    use TOUZA_Nio_record,only: REC_DEFAULT
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jarg
    character(len=256) :: rfile
    integer cid

    integer jrec, krect, uread
    character(len=litem) hd(nitem)
    real(kind=KBUF),allocatable :: v(:)
    integer,allocatable :: ends(:), subv(:)
    character(len=litem) :: dfmt, item, item_sv
    integer nmask, ndata, nchk
    integer,parameter :: laxs = 3
    integer :: kaxs(laxs)
    integer jc
    integer niter
    integer jz
    integer jv, jvb, jve
    integer check
    integer idummy(1)

    ierr = 0

    jarg = jarg + 1
    if (ierr.eq.0) call get_param(ierr, rfile, jarg, ' ')
    if (rfile.eq.' ') return
    ierr = 0

    if (ierr.eq.0) call get_option(ierr, cid, 'C', 0)
    if (ierr.eq.0) then
       cid = min(max(0, cid), 3)
    endif

101 format('test/packed:', I0, 1x, A)
401 format('header/r:', I0, 1x, I0, 1x, I0)
403 format('open/r:', I0)
! 201 format('ends:', 32(1x, I0))
! 209 format('ends: failed ', I0)

    write(*, 101) jarg, trim(rfile)

    allocate(ends(0:0), v(0:0), subv(0:0), STAT=ierr)

    uread = 21
    if (ierr.eq.0) call sus_open(ierr, uread, rfile, ACTION='R', STATUS='O')
    write (*, 403) ierr

    jrec = 0
    krect = REC_DEFAULT
    do
       if (ierr.eq.0) call nio_read_header(ierr, hd, krect, uread)
       write (*, 401) ierr, jrec, krect
       if (is_error_match(ierr, ERR_EOF)) then
          ierr = 0
          exit
       endif
       if (ierr.ne.0) exit
       if (ierr.eq.0) call get_item(ierr, hd, item, hi_ITEM)
       do jc = 1, 3
          if (ierr.eq.0) kaxs(jc) = parse_header_size(hd, jc, 1)
       enddo
       ! write(*, *) 'kaxs:', ierr, kaxs
       if (ierr.eq.0) then
          if (cid.eq.0) then
             niter = 1
          else
             niter = product(kaxs(cid:laxs))
          endif
       endif
       if (ierr.eq.0) call get_item(ierr, hd, dfmt, hi_DFMT)
       if (ierr.eq.0) call upcase(dfmt)
       if (ierr.eq.0) then
          if (niter+1.gt.size(ends)) then
             deallocate(ends)
             allocate(ends(0:niter), STAT=ierr)
          endif
       endif
       ! write(*, *) 'allocate ends', ierr, niter
       if (ierr.eq.0) then
          write(*, *) 'read:', trim(item), ' ', cid, niter
          if (dfmt(1:1).eq.'M') then
             if (dfmt(2:2).eq.'I') then
                check = 0
             else
                check = 1
             endif
             call review_mtn(ierr, nmask, ndata, hd, uread, krect)
             ! write(*, *) trim(item), ' ', trim(dfmt), nmask, ndata
             if (ierr.eq.0) then
                if (ndata.gt.size(v)) then
                   deallocate(v, subv)
                   allocate(v(0:ndata-1), subv(0:ndata-1), STAT=ierr)
                endif
             endif
             if (ierr.eq.0) then
                call nio_read_data_packed &
                     & (ierr, &
                     &  v,   ndata, subv, ends(1:), hd, krect, uread, &
                     &  cid, check)
             endif
             write(*, *) 'read_packed/mask = ', ierr, check, ndata
             write(*, *) 'read_packed/mask = ', ends(1:niter)
          else if (dfmt(1:1).eq.'P') then
             call decompose_packed_item(item_sv, dfmt)
             if (item_sv.eq.' ') then
                check = 0
                call review_ptn(ierr, ndata, hd, uread, krect)
                if (ndata.gt.size(subv)) then
                   deallocate(subv)
                   allocate(subv(0:ndata-1), STAT=ierr)
                endif
                if (ierr.eq.0) then
                   call nio_read_data_packed &
                        & (ierr, &
                        &  idummy, -1, subv, ends(1:), hd, krect, uread, &
                        &  cid,    check)
                endif
                niter = 0
             else
                check = 1
                call review_ptn(ierr, nchk, hd, uread, krect)
                if (nchk.ne.ndata) ierr = ERR_PANIC
                if (ierr.eq.0) then
                   if (ndata.gt.size(v)) then
                      deallocate(v)
                      allocate(v(0:ndata-1), STAT=ierr)
                   endif
                endif
                if (ierr.eq.0) then
                   call nio_read_data_packed &
                        & (ierr, &
                        &  v,    ndata, subv, ends(1:), hd, krect, uread, &
                        &  cid,  check)
                endif
             endif
             write(*, *) 'read_packed/pack = ', ierr, check, ndata, trim(item_sv)
          else
             niter = 0
             call nio_skip_records(ierr, 1, uread, head=hd, krect=krect)
          endif
       endif
       if (ierr.eq.0) then
1001      format(A, 1x, I0, 1x, I0, 1x, I0)
1002      format(A, 1x, I0, 1x, I0, 1x, E10.3)
          if (dfmt(2:2).eq.'I') then
             ends(0) = 0
             do jz = 0, niter - 1
                jvb = ends(jz)
                jve = ends(jz+1)
                do jv = jvb, jve - 1
                   write(*, 1001) trim(item), jz, subv(jv), int(v(jv))
                enddo
             enddo
          else
             ends(0) = 0
             do jz = 0, niter - 1
                jvb = ends(jz)
                jve = ends(jz+1)
                do jv = jvb, jve - 1
                   write(*, 1002) trim(item), jz, subv(jv), v(jv)
                enddo
             enddo
          endif
       endif
       jrec = jrec + 1
    enddo
    if (ierr.eq.0) call sus_close(ierr, uread, rfile)
    if (ierr.eq.0) deallocate(ends, STAT=ierr)
    return
  end subroutine test_batch_read_packed

!!!_ + test_batch_byte_order - write extreme data
  subroutine test_batch_byte_order &
       & (ierr, jarg)
    use TOUZA_Std,only: endian_LITTLE, endian_BIG
    use TOUZA_Nio_std,only: KBUF=>KDBL
    use TOUZA_Std_sus,only: sus_open,  sus_close
    use TOUZA_Nio_header
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jarg
    integer nx, ny, nz, nn
    real(kind=KBUF) :: vmiss = -999.0_KBUF
    real(kind=KBUF),allocatable :: v(:)
    integer krect
    integer ufile
    character(len=1024) :: wfile
    character(len=litem) :: hd(nitem)
    character(len=128)  :: tswap
    integer krectw

    ierr = 0
    ufile = 41

    if (ierr.eq.0) call set_bodr_wnative(ierr, BODR_CHECK_VERBOSE, levv=+9)

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
    if (ierr.eq.0) call put_item(ierr, hd, 'UR4',  hi_DFMT)

    if (ierr.eq.0) call sus_open(ierr, ufile, wfile, ACTION='RW', STATUS='R')

    krectw = REC_ASIS
    if (ierr.eq.0) v(:) = +1.0_KBUF
    if (ierr.eq.0) call nio_write_header(ierr, hd, krectw, ufile)
    if (ierr.eq.0) call nio_write_data(ierr, v, nn, hd, krectw, ufile)
    if (ierr.eq.0) call sus_close(ierr, ufile, wfile)

  end subroutine test_batch_byte_order

end program test_nio_record
#endif /* TEST_NIO_RECORD */
!!!_* obsolete
#if 0 /* meta-comment */
  ! interface get_data_mr4
  !    module procedure get_data_mr4_f, get_data_mr4_d, get_data_mr4_i
  ! end interface get_data_mr4
  ! interface get_data_mr8
  !    module procedure get_data_mr8_f, get_data_mr8_d, get_data_mr8_i
  ! end interface get_data_mr8
  ! interface get_data_mi4
  !    module procedure get_data_mi4_f, get_data_mi4_d, get_data_mi4_i
  ! end interface get_data_mi4

!!!_  - get_data_mr4 - MR4
  ! subroutine get_data_mr4_f &
  !      & (ierr, &
  !      &  d, nd, u, krect, vmiss, md, bes, nr)
  !   use TOUZA_Trp,only: count_packed, pack_restore
  !   use TOUZA_Trp,only: mask_to_idxl
  !   implicit none
  !   integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
  !   integer,         intent(out) :: ierr
  !   real(kind=KARG), intent(out) :: d(0:*)
  !   integer,         intent(in)  :: nd
  !   integer,         intent(in)  :: krect
  !   integer,         intent(in)  :: u
  !   real(kind=KRMIS),intent(in)  :: vmiss
  !   integer,         intent(in)  :: md
  !   integer,optional,intent(in)  :: bes(3, *)
  !   integer,optional,intent(in)  :: nr

  !   integer(kind=KISRC) :: mb
  !   integer(kind=KISRC) :: icom(md)
  !   real(kind=KRSRC)    :: buf(0:nd-1)
  !   integer(kind=KISRC),parameter :: mold = 0_KISRC
  !   integer :: ldst(0:nd-1), lsrc(0:nd-1)
  !   integer :: nx
  !   integer ncom
  !   integer kpack

  !   ierr = 0
  !   kpack = legacy_unpacking(1, md)
  !   ncom = count_packed(1, md, mold)
  !   if (ierr.eq.0) call get_data_record(ierr, mb, u, krect)
  !   if (ierr.eq.0) call get_data_record(ierr, icom, ncom, u, krect)
  !   ! write(*, *) ierr, md, ncom
  !   if (present(bes)) then
  !      if (.not.present(nr)) ierr = _ERROR(ERR_INVALID_ITEM)
  !      if (ierr.eq.0) then
  !         call mask_to_idxl(ierr, ldst, lsrc, nx, icom, md, bes, nr, kpack)
  !      endif
  !      if (ierr.eq.0) then
  !         call get_data_record_list(ierr, buf, mb, u, krect, mb, lsrc, nx)
  !         d(0:nd-1) = real(vmiss, kind=KARG)
  !         d(ldst(0:nx-1)) = buf(0:nx-1)
  !      endif
  !   else
  !      if (ierr.eq.0) call get_data_record(ierr, buf,  mb,   u, krect)
  !      if (ierr.eq.0) then
  !         call mask_decode &
  !              & (ierr,  d, nd, buf, icom, vmiss, kpack)
  !      endif
  !   endif
  !   return
  ! end subroutine get_data_mr4_f
  ! subroutine get_data_mr4_d &
  !      & (ierr, &
  !      &  d, nd, u, krect, vmiss, md, bes, nr)
  !   use TOUZA_Trp,only: count_packed, pack_restore
  !   implicit none
  !   integer,parameter :: KARG=KDBL, KRSRC=KFLT
  !   integer,         intent(out) :: ierr
  !   real(kind=KARG), intent(out) :: d(0:*)
  !   integer,         intent(in)  :: nd
  !   integer,         intent(in)  :: krect
  !   integer,         intent(in)  :: u
  !   real(kind=KRMIS),intent(in)  :: vmiss
  !   integer,         intent(in)  :: md
  !   integer,optional,intent(in)  :: bes(3, *)
  !   integer,optional,intent(in)  :: nr

  !   real(kind=KRSRC) :: buf(0:nd-1)

  !   ierr = 0
  !   call get_data_mr4_f(ierr, buf, nd, u, krect, vmiss, md, bes, nr)
  !   if (ierr.eq.0) d(0:nd-1) = real(buf(0:nd-1), KIND=KARG)
  !   return
  ! end subroutine get_data_mr4_d
  ! subroutine get_data_mr4_i &
  !      & (ierr, &
  !      &  d, nd, u, krect, vmiss, md, bes, nr)
  !   use TOUZA_Trp,only: count_packed, pack_restore
  !   implicit none
  !   integer,parameter :: KARG=KI32, KRSRC=KFLT
  !   integer,           intent(out) :: ierr
  !   integer(kind=KARG),intent(out) :: d(0:*)
  !   integer,           intent(in)  :: nd
  !   integer,           intent(in)  :: krect
  !   integer,           intent(in)  :: u
  !   real(kind=KRMIS),  intent(in)  :: vmiss
  !   integer,           intent(in)  :: md
  !   integer,optional,  intent(in)  :: bes(3, *)
  !   integer,optional,  intent(in)  :: nr

  !   real(kind=KRSRC) :: buf(0:nd-1)

  !   ierr = 0
  !   call get_data_mr4_f(ierr, buf, nd, u, krect, vmiss, md, bes, nr)
  !   if (ierr.eq.0) d(0:nd-1) = int(buf(0:nd-1), KIND=KARG)
  !   return
  ! end subroutine get_data_mr4_i

!!!_  - get_data_mr8 - MR8
  ! subroutine get_data_mr8_d &
  !      & (ierr, &
  !      &  d, nd, u, krect, vmiss, md, bes, nr)
  !   use TOUZA_Trp,only: count_packed, pack_restore
  !   use TOUZA_Trp,only: mask_to_idxl
  !   implicit none
  !   integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
  !   integer,         intent(out) :: ierr
  !   real(kind=KARG), intent(out) :: d(0:*)
  !   integer,         intent(in)  :: nd
  !   integer,         intent(in)  :: krect
  !   integer,         intent(in)  :: u
  !   real(kind=KRMIS),intent(in)  :: vmiss
  !   integer,         intent(in)  :: md
  !   integer,optional,intent(in)  :: bes(3, *)
  !   integer,optional,intent(in)  :: nr

  !   integer(kind=KISRC) :: mb
  !   integer(kind=KISRC) :: icom(0:md-1)
  !   real(kind=KRSRC)    :: buf(0:nd-1)
  !   integer(kind=KISRC),parameter :: mold = 0_KISRC
  !   integer :: ldst(0:nd-1), lsrc(0:nd-1)
  !   integer :: nx
  !   integer ncom
  !   integer kpack

  !   ierr = 0
  !   kpack = legacy_unpacking(1, md)
  !   ncom = count_packed(1, md, mold)
  !   if (ierr.eq.0) call get_data_record(ierr, mb, u, krect)
  !   if (ierr.eq.0) call get_data_record(ierr, icom, ncom, u, krect)
  !   ! write(*, *) ncom
  !   if (present(bes)) then
  !      if (.not.present(nr)) ierr = _ERROR(ERR_INVALID_ITEM)
  !      if (ierr.eq.0) then
  !         call mask_to_idxl(ierr, ldst, lsrc, nx, icom, md, bes, nr, kpack)
  !      endif
  !      if (ierr.eq.0) then
  !         call get_data_record_list(ierr, buf, mb, u, krect, mb, lsrc, nx)
  !         d(0:nd-1) = vmiss
  !         d(ldst(0:nx-1)) = buf(0:nx-1)
  !      endif
  !   else
  !      if (ierr.eq.0) call get_data_record(ierr, buf,  mb,   u, krect)
  !      if (ierr.eq.0) then
  !         call mask_decode &
  !              & (ierr,  d, nd, buf, icom, vmiss, kpack)
  !      endif
  !   endif
  !   return
  ! end subroutine get_data_mr8_d
  ! subroutine get_data_mr8_f &
  !      & (ierr, &
  !      &  d, nd, u, krect, vmiss, md, bes, nr)
  !   implicit none
  !   integer,parameter :: KARG=KFLT, KRSRC=KDBL
  !   integer,         intent(out) :: ierr
  !   real(kind=KARG), intent(out) :: d(0:*)
  !   integer,         intent(in)  :: nd
  !   integer,         intent(in)  :: krect
  !   integer,         intent(in)  :: u
  !   real(kind=KRMIS),intent(in)  :: vmiss
  !   integer,         intent(in)  :: md
  !   integer,optional,intent(in)  :: bes(3, *)
  !   integer,optional,intent(in)  :: nr

  !   real(kind=KRSRC) :: buf(0:nd-1)

  !   ierr = 0
  !   call get_data_mr8_d(ierr, buf, nd, u, krect, vmiss, md, bes, nr)
  !   if (ierr.eq.0) d(0:nd-1) = real(buf(0:nd-1), KIND=KARG)
  !   return
  ! end subroutine get_data_mr8_f
  ! subroutine get_data_mr8_i &
  !      & (ierr, &
  !      &  d, nd, u, krect, vmiss, md, bes, nr)
  !   implicit none
  !   integer,parameter :: KARG=KI32, KRSRC=KDBL
  !   integer,           intent(out) :: ierr
  !   integer(kind=KARG),intent(out) :: d(0:*)
  !   integer,           intent(in)  :: nd
  !   integer,           intent(in)  :: krect
  !   integer,           intent(in)  :: u
  !   real(kind=KRMIS),  intent(in)  :: vmiss
  !   integer,           intent(in)  :: md
  !   integer,optional,  intent(in)  :: bes(3, *)
  !   integer,optional,  intent(in)  :: nr

  !   real(kind=KRSRC) :: buf(0:nd-1)

  !   ierr = 0
  !   call get_data_mr8_d(ierr, buf, nd, u, krect, vmiss, md, bes, nr)
  !   if (ierr.eq.0) d(0:nd-1) = int(buf(0:nd-1), KIND=KARG)
  !   return
  ! end subroutine get_data_mr8_i

!!!_  - get_data_mi4 - MI4
  ! subroutine get_data_mi4_i &
  !      & (ierr, &
  !      &  d, nd, u, krect, vmiss, md, bes, nr)
  !   use TOUZA_Trp,only: count_packed
  !   use TOUZA_Trp,only: mask_to_idxl
  !   implicit none
  !   integer,parameter :: KARG=KI32, KISRC=KI32
  !   integer,           intent(out) :: ierr
  !   integer(kind=KARG),intent(out) :: d(0:*)
  !   integer,           intent(in)  :: nd
  !   integer,           intent(in)  :: krect
  !   integer,           intent(in)  :: u
  !   real(kind=KRMIS),  intent(in)  :: vmiss
  !   integer,           intent(in)  :: md
  !   integer,optional,  intent(in)  :: bes(3, *)
  !   integer,optional,  intent(in)  :: nr

  !   integer(kind=KISRC) :: mb
  !   integer(kind=KISRC) :: icom(md)
  !   integer(kind=KISRC) :: buf(0:nd-1)
  !   integer(kind=KISRC),parameter :: mold = 0_KISRC
  !   integer :: ldst(0:nd-1), lsrc(0:nd-1)
  !   integer :: nx
  !   integer ncom
  !   integer kpack

  !   ierr = 0
  !   kpack = legacy_unpacking(1, md)
  !   ncom = count_packed(1, md, mold)
  !   if (ierr.eq.0) call get_data_record(ierr, mb, u, krect)
  !   if (ierr.eq.0) call get_data_record(ierr, icom, ncom, u, krect)
  !   if (present(bes)) then
  !      if (.not.present(nr)) ierr = _ERROR(ERR_INVALID_ITEM)
  !      if (ierr.eq.0) then
  !         call mask_to_idxl(ierr, ldst, lsrc, nx, icom, md, bes, nr, kpack)
  !      endif
  !      if (ierr.eq.0) then
  !         call get_data_record_list(ierr, buf, mb, u, krect, mb, lsrc, nx)
  !         d(0:nd-1) = int(vmiss, kind=KARG)
  !         d(ldst(0:nx-1)) = buf(0:nx-1)
  !      endif
  !   else
  !      if (ierr.eq.0) call get_data_record(ierr, buf,  mb,   u, krect)
  !      if (ierr.eq.0) then
  !         call mask_decode &
  !              & (ierr,  d, nd, buf, icom, vmiss, kpack)
  !      endif
  !   endif
  !   return
  ! end subroutine get_data_mi4_i
  ! subroutine get_data_mi4_d &
  !      & (ierr, &
  !      &  d, nd, u, krect, vmiss, md, bes, nr)
  !   implicit none
  !   integer,parameter :: KARG=KDBL, KISRC=KI32
  !   integer,         intent(out) :: ierr
  !   real(kind=KARG), intent(out) :: d(0:*)
  !   integer,         intent(in)  :: nd
  !   integer,         intent(in)  :: krect
  !   integer,         intent(in)  :: u
  !   real(kind=KRMIS),intent(in)  :: vmiss
  !   integer,         intent(in)  :: md
  !   integer,optional,intent(in)  :: bes(3, *)
  !   integer,optional,intent(in)  :: nr

  !   integer(kind=KISRC) :: buf(0:nd-1)

  !   ierr = 0
  !   call get_data_mi4_i(ierr, buf, nd, u, krect, vmiss, md, bes, nr)
  !   if (ierr.eq.0) d(0:nd-1) = real(buf(0:nd-1), KIND=KARG)
  !   return
  ! end subroutine get_data_mi4_d
  ! subroutine get_data_mi4_f &
  !      & (ierr, &
  !      &  d, nd, u, krect, vmiss, md, bes, nr)
  !   implicit none
  !   integer,parameter :: KARG=KFLT, KISRC=KI32
  !   integer,         intent(out) :: ierr
  !   real(kind=KARG), intent(out) :: d(0:*)
  !   integer,         intent(in)  :: nd
  !   integer,         intent(in)  :: krect
  !   integer,         intent(in)  :: u
  !   real(kind=KRMIS),intent(in)  :: vmiss
  !   integer,         intent(in)  :: md
  !   integer,optional,intent(in)  :: bes(3, *)
  !   integer,optional,intent(in)  :: nr

  !   integer(kind=KISRC) :: buf(0:nd-1)

  !   ierr = 0
  !   call get_data_mi4_i(ierr, buf, nd, u, krect, vmiss, md, bes, nr)
  !   if (ierr.eq.0) d(0:nd-1) = real(buf(0:nd-1), KIND=KARG)
  !   return
  ! end subroutine get_data_mi4_f

#endif /* meta-comment */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

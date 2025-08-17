!!!_! nio_record.F90 - TOUZA/Nio record interfaces
! Maintainer: SAITO Fuyuki
! Created: Oct 29 2021
#define TIME_STAMP 'Time-stamp: <2025/07/16 16:12:15 fuyuki nio_record.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021-2025
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
  use TOUZA_Nio_std,only: trace_err
  use TOUZA_Nio_header,only: litem, nitem
  use TOUZA_Trp,only: KCODE_CLIPPING,  KCODE_ROUND
  use TOUZA_Trp,only: KCODE_TRANSPOSE, KCODE_SEQUENTIAL, KCODE_INCREMENTAL
  use TOUZA_Trp,only: KCODE_MANUAL,    RELLENO_SEQUENTIAL
  implicit none
  private
!!!_  - public parameters
  integer,parameter,public :: laxs = 3   ! number of coordinates

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
  integer,parameter,public :: GFMT_PRY    = GFMT_URY    + GFMT_LPAD
  integer,parameter,public :: GFMT_PRYend = GFMT_URYend + GFMT_LPAD

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

  integer,parameter,public :: GFMT_PI1  = GFMT_UI1 + GFMT_LPAD
  integer,parameter,public :: GFMT_PI4  = GFMT_UI4 + GFMT_LPAD
  integer,parameter,public :: GFMT_PI8  = GFMT_UI8 + GFMT_LPAD

  integer,parameter,public :: GFMT_URT  = 256
  integer,parameter,public :: GFMT_MRT  = GFMT_URT + GFMT_MASK
  integer,parameter,public :: GFMT_PRT  = GFMT_URT + GFMT_LPAD

  integer,parameter,public :: GFMT_END  = GFMT_LPAD * 2
!!!_    * URT details
  integer,parameter,public :: PROP_DEFAULT = (- HUGE(0)) - 1

  integer,parameter,public :: PROP_URT_MANTISSA = 1 /* mantissa bits */
  integer,parameter,public :: PROP_URT_XBITS    = 2 /* exponent bits */
  integer,parameter,public :: PROP_URT_XBOTTOM  = 3 /* exponent lower limit (relative to exp(1)) */
  integer,parameter,public :: PROP_URT_XTOP     = 4 /* exponent lower limit (relative to exp(1)) */
  integer,parameter,public :: PROP_URT_CODES    = 5 /* kcode switches */
  integer,parameter,public :: PROP_URT_BREAK    = 6 /* array break */

  integer,parameter,public :: lopts_urt = 6

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

!!!_    * PTx details
  integer,parameter,public :: PTX_VERSION = 813200464  ! 'Ptx0'

  integer,parameter,public :: PROP_PTX_VAR     = 1   ! PTx variation (reserved)
  integer,parameter,public :: PROP_PTX_COLC    = 2   ! column coordinate (count from 1, oh.....)
  integer,parameter,public :: PROP_PTX_MCOL    = 3   ! column size
  integer,parameter,public :: PROP_PTX_CODES   = 4   ! kcode switch (transpose)
  integer,parameter,public :: PROP_PTX_MBITS   = 5   ! size array bits unit (config) or size array bits (stored) (0)
  integer,parameter,public :: PROP_PTX_OFFSET  = 6   ! size shift for larger values (0)
  integer,parameter,public :: PROP_PTX_KEEP    = 7   ! maximum size not to shift (1)
  integer,parameter,public :: PROP_PTX_DATA    = 8   ! packed data size

  integer,parameter,public :: lopts_ptx = 8

  integer,parameter,public :: lopts = max(lopts_urt, lopts_ptx)
!!!_   . data reviewing flag
  integer,parameter,public :: rev_pos_dhead     = 0   ! rewind to data heads at exit
  integer,parameter,public :: rev_pos_leave     = 1   ! leave position at exit
  integer,parameter,public :: rev_plain_storage = 2   ! review storage size (default: expanded-size)

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
     module procedure get_data_record_runl_i, get_data_record_runl_f, get_data_record_runl_d
  end interface get_data_record_runl
  interface get_data_record_suspend
     ! module procedure get_data_record_suspend_i, get_data_record_suspend_f, get_data_record_suspend_d
     module procedure get_data_record_suspend_i
  end interface get_data_record_suspend

  interface get_data_irecord_list
     module procedure get_data_irecord_list_f, get_data_irecord_list_d
     module procedure get_data_irecord_list_i
  end interface get_data_irecord_list
  interface get_data_frecord_list
     module procedure get_data_frecord_list_f, get_data_frecord_list_d
     module procedure get_data_frecord_list_i
  end interface get_data_frecord_list
  interface get_data_drecord_list
     module procedure get_data_drecord_list_f, get_data_drecord_list_d
     module procedure get_data_drecord_list_i
  end interface get_data_drecord_list

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

  interface get_data_drecord_runl
     module procedure get_data_drecord_runl_f, get_data_drecord_runl_i
     module procedure get_data_record_runl_d
  end interface get_data_drecord_runl
  interface get_data_frecord_runl
     module procedure get_data_frecord_runl_d, get_data_frecord_runl_i
     module procedure get_data_record_runl_f
  end interface get_data_frecord_runl
  interface get_data_irecord_runl
     module procedure get_data_irecord_runl_f, get_data_irecord_runl_d
     module procedure get_data_record_runl_i
  end interface get_data_irecord_runl

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

  interface put_data_mry
     module procedure put_data_mry_f, put_data_mry_d, put_data_mry_i
  end interface put_data_mry
  interface put_data_ury
     module procedure put_data_ury_f, put_data_ury_d, put_data_ury_i
  end interface put_data_ury

  interface put_data_urt
     module procedure put_data_urt_d, put_data_urt_f
  end interface put_data_urt
  interface put_data_mrt
     module procedure put_data_mrt_d, put_data_mrt_f
  end interface put_data_mrt
  interface put_data_urt_core
     module procedure put_data_urt_core_d, put_data_urt_core_f
  end interface put_data_urt_core

  interface get_data_urt
     module procedure get_data_urt_d, get_data_urt_f
  end interface get_data_urt
  interface get_data_urt_core
     module procedure get_data_urt_core_d, get_data_urt_core_f
  end interface get_data_urt_core
  interface get_data_urt_runl
     module procedure get_data_urt_runl_d, get_data_urt_runl_f
  end interface get_data_urt_runl
  interface get_data_mrt_runl
     module procedure get_data_mrt_runl_d, get_data_mrt_runl_f
  end interface get_data_mrt_runl
  interface get_data_mrt
     module procedure get_data_mrt_d, get_data_mrt_f
  end interface get_data_mrt

  ! interface restore_ur8_packed
  !    module procedure restore_ur8_packed_d, restore_ur8_packed_f, restore_ur8_packed_i
  ! end interface restore_ur8_packed
  ! interface restore_ur4_packed
  !    module procedure restore_ur4_packed_d, restore_ur4_packed_f, restore_ur4_packed_i
  ! end interface restore_ur4_packed
  ! interface restore_ui4_packed
  !    module procedure restore_ui4_packed_d, restore_ui4_packed_f, restore_ui4_packed_i
  ! end interface restore_ui4_packed

  interface normalize_xry
     module procedure normalize_xry_d
  end interface normalize_xry
  interface mask_encode
     module procedure mask_encode_di, mask_encode_fi, mask_encode_ii
  end interface mask_encode
  interface mask_decode
     module procedure mask_decode_ddi, mask_decode_ffi, mask_decode_iii
  end interface mask_decode
  interface subv_decode
     module procedure subv_decode_dd, subv_decode_ff, subv_decode_ii
  end interface subv_decode
  interface subv_encode
     module procedure subv_encode_d, subv_encode_f, subv_encode_i
  end interface subv_encode
  ! interface mask_decode_subv
  !    module procedure mask_decode_subv_i
  ! end interface mask_decode_subv

  ! interface pack_plain_data
  !    module procedure pack_plain_data_dd, pack_plain_data_df, pack_plain_data_di
  !    module procedure pack_plain_data_fd, pack_plain_data_ff, pack_plain_data_fi
  !    module procedure pack_plain_data_id, pack_plain_data_if, pack_plain_data_ii
  ! end interface pack_plain_data

  interface parse_header_size
     module procedure parse_header_size_n
     module procedure parse_header_size_i
  end interface parse_header_size

  interface nio_check_magic_file
     module procedure nio_check_magic_file_name, nio_check_magic_file_unit
  end interface nio_check_magic_file
  ! interface review_ur8
  !    module procedure review_ur8_d, review_ur8_f, review_ur8_i
  ! end interface review_ur8
  ! interface review_ur4
  !    module procedure review_ur4_d, review_ur4_f, review_ur4_i
  ! end interface review_ur4
  ! interface review_ui4
  !    module procedure review_ui4_d, review_ui4_f, review_ui4_i
  ! end interface review_ui4

!!!_   . MTn
  interface mtn_write_array
     module procedure mtn_write_array_d, mtn_write_array_f, mtn_write_array_i
  end interface mtn_write_array
  interface mtn_write_data
     module procedure mtn_write_data_d, mtn_write_data_f, mtn_write_data_i
  end interface mtn_write_data

  interface mtn_read_array
     module procedure mtn_read_array_d, mtn_read_array_f, mtn_read_array_i
  end interface mtn_read_array
  interface mtn_read_slice
     module procedure mtn_read_slice_d, mtn_read_slice_f, mtn_read_slice_i
  end interface mtn_read_slice
  interface mtn_read_data
     module procedure mtn_read_data_d, mtn_read_data_f, mtn_read_data_i
  end interface mtn_read_data

!!!_   . PTx
  interface ptx_write_array
     module procedure ptx_write_array_d, ptx_write_array_f, ptx_write_array_i
  end interface ptx_write_array
  interface ptx_read_array
     module procedure ptx_read_array_d, ptx_read_array_f, ptx_read_array_i
  end interface ptx_read_array

  interface ptx_write_data
     module procedure ptx_write_data_d, ptx_write_data_f, ptx_write_data_i
  end interface ptx_write_data
  interface ptx_read_data
     module procedure ptx_read_data_d, ptx_read_data_f, ptx_read_data_i
  end interface ptx_read_data

  interface ptx_pack_data
     module procedure ptx_pack_data_d, ptx_pack_data_f, ptx_pack_data_i
  end interface ptx_pack_data
  interface ptx_expand_data
     module procedure ptx_expand_data_d, ptx_expand_data_f, ptx_expand_data_i
  end interface ptx_expand_data

  interface ptx_gen_ccvec
     module procedure ptx_gen_ccvec_d, ptx_gen_ccvec_f, ptx_gen_ccvec_i
  end interface ptx_gen_ccvec
  interface ptx_parse_array
     module procedure ptx_parse_array_d, ptx_parse_array_f, ptx_parse_array_i
  end interface ptx_parse_array

  interface ptx_check_vec
     module procedure ptx_check_vec_d, ptx_check_vec_f, ptx_check_vec_i
  end interface ptx_check_vec

  interface ptx_encode_ccvec
     module procedure ptx_encode_ccvec_mod, ptx_encode_ccvec_set
  end interface ptx_encode_ccvec

  interface alloc_work
     module procedure alloc_work_d, alloc_work_f, alloc_work_i
  end interface alloc_work

!!!_  - public procedures
  public init, diag, finalize
  public set_default_switch
  public set_default_header, get_default_header
  public nio_record_std, nio_record_def
  public nio_check_magic_file
  public nio_read_header,    nio_write_header
  public nio_read_data,      nio_write_data
  public nio_read_data_slice
  public nio_review_record
  public nio_skip_records
  public nio_bwd_record
  public parse_header_base,  parse_record_fmt
  public parse_header_size
  public get_header_cprop,   put_header_cprop
  public get_header_cname,   put_header_cname
  public inquire_header_coor,search_null_coor,   shift_header_coor
  public set_urt_defs,       parse_urt_options,  show_urt_options
  public switch_urt_diag
  public set_switch_subrec,  nio_allow_sub
  ! public review_ur8,         review_ur4,         review_ui4
  ! public review_mtn,         review_ptn
  public set_bodr_wnative
  public subv_encode

  public mtn_review
  public mtn_read_array, mtn_read_data
  public mtn_write_array, mtn_write_data

  public nio_count_defined, decompose_packed_item
  public ptx_check_vec
  public ptx_def_options, ptx_parse_options, ptx_set_shape
  public ptx_review,      ptx_parse_array
  public ptx_write_array, ptx_write_data
  public ptx_read_array,  ptx_read_data
  public ptx_expand_data, ptx_set_loops
  public ptx_gen_ccvec,   ptx_pack_data
  public ptx_row_size

  public pre_review,      post_review
  public is_review_leave
  public is_match_format
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
    integer lv, md, lmd, ttmd
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
          if (ierr.eq.0) call nh_init(ierr, u=ulog, levv=lv, mode=MODE_SURFACE)
       endif
       if (md.ge.MODE_DEEP) then
          ! ttmd = MODE_SHALLOW
          ttmd = lmd
          if (ierr.eq.0) call trp_init(ierr, u=ulog, levv=lv, mode=ttmd, stdv=stdv)
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
          if (ierr.eq.0) call nh_diag(ierr, utmp, levv=lv, mode=MODE_SURFACE)
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
          if (ierr.eq.0) call nh_finalize(ierr, utmp, levv=lv, mode=MODE_SURFACE)
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
    use TOUZA_Nio_std,only: choice, msg, is_msglev_info, is_msglev_fatal
    use TOUZA_Nio_std,only: kendi_mem, kendi_file
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
    use TOUZA_Nio_header,only: put_item
    use TOUZA_Nio_header,only: hi_IDFM,  hi_UTIM,  hi_FNUM,  hi_DNUM
    use TOUZA_Nio_header,only: hi_ASTR1, hi_AEND1, hi_ASTR2, hi_AEND2, hi_ASTR3, hi_AEND3
    use TOUZA_Nio_header,only: hi_MISS,  hi_DMIN,  hi_DMAX,  hi_DIVS,  hi_DIVL
    use TOUZA_Nio_header,only: hi_STYP,  hi_IOPTN, hi_ROPTN, hi_CSIGN, hi_MSIGN
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
!!!_  & nio_record_std() - get standard record type == (REC_BIG)
  integer function nio_record_std() result(k)
    implicit none
    k = REC_BIG
  end function nio_record_std

!!!_  & nio_record_def() - get default record type == (REC_DEFAULT)
  integer function nio_record_def() result(k)
    implicit none
    k = REC_DEFAULT
  end function nio_record_def

!!!_  & get_default_header - get default header
  subroutine get_default_header &
       & (head,  &
       &  vmiss, utime, csign, msign)
    use TOUZA_Nio_header,only: put_item
    use TOUZA_Nio_header,only: hi_UTIM,  hi_MISS,  hi_DMIN,  hi_DMAX,  hi_DIVS,  hi_DIVL
    use TOUZA_Nio_header,only: hi_CSIGN, hi_MSIGN
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
!!!_  & check_magic_bytes
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
    use TOUZA_Nio_std,only: debug_status
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
    ! write(*, *) ierr, kfmt
    ! call debug_status(ierr, u, tag='read:0')
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
    ! write(*, *) ierr, ld
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
       case (GFMT_MR4, GFMT_MR8, GFMT_MI4)
          call mtn_read_array(ierr, d, nd, u, krect, vmiss, kfmt, mw)
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
       case (GFMT_PR8,GFMT_PR4,GFMT_PI4)
          call ptx_read_array(ierr, d, nd, u, krect, vmiss, kaxs, kfmt)
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
       case (GFMT_MR4, GFMT_MR8, GFMT_MI4)
          call mtn_read_array(ierr, d, nd, u, krect, vmiss, kfmt, mw)
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
       case (GFMT_PR8,GFMT_PR4,GFMT_PI4)
          call ptx_read_array(ierr, d, nd, u, krect, vmiss, kaxs, kfmt)
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
       case (GFMT_MR4, GFMT_MR8, GFMT_MI4)
          call mtn_read_array(ierr, d, nd, u, krect, vmiss, kfmt, mw)
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
       case (GFMT_PR8,GFMT_PR4,GFMT_PI4)
          call ptx_read_array(ierr, d, nd, u, krect, vmiss, kaxs, kfmt)
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
       case (GFMT_MR4, GFMT_MR8, GFMT_MI4)
          call mtn_read_array(ierr, d, nd, u, krect, vmiss, kfmt, mw, bes, laxs)
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
          call get_data_urt &
               & (ierr, d, nd, u, krect, vmiss, kopts, bes=bes, nr=laxs)
       case (GFMT_MRT)
          call get_data_mrt &
               & (ierr, d, nd, u, krect, vmiss, kopts, bes=bes, nr=laxs)
       case (GFMT_PR8,GFMT_PR4,GFMT_PI4)
          call ptx_read_array(ierr, d, nd, u, krect, vmiss, kaxs, kfmt, bes, laxs)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    call trace_err(ierr, 'nio_read_data_slice')
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
       case (GFMT_MR4, GFMT_MR8, GFMT_MI4)
          call mtn_read_array(ierr, d, nd, u, krect, vmiss, kfmt, mw, bes, laxs)
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
          call get_data_urt &
               & (ierr, d, nd, u, krect, vmiss, kopts, bes=bes, nr=laxs)
       case (GFMT_MRT)
          call get_data_mrt &
               & (ierr, d, nd, u, krect, vmiss, kopts, bes=bes, nr=laxs)
       case (GFMT_PR8,GFMT_PR4,GFMT_PI4)
          call ptx_read_array(ierr, d, nd, u, krect, vmiss, kaxs, kfmt, bes, laxs)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    call trace_err(ierr, 'nio_read_data_slice')
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
       case (GFMT_MR4, GFMT_MR8, GFMT_MI4)
          call mtn_read_array(ierr, d, nd, u, krect, vmiss, kfmt, mw, bes, laxs)
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
          call ptx_read_array(ierr, d, nd, u, krect, vmiss, kaxs, kfmt, bes, laxs)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    call trace_err(ierr, 'nio_read_data_slice')
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
    ! write(*, *) nr, mw, size(start), size(count)
    call bes_triplet(ierr, bes, kaxs, nr, start, count)
    if (ierr.eq.0) then
       nd = count_bes(bes, nr)
       if (ld.ge.0.and.nd.gt.ld) ierr = _ERROR(ERR_SIZE_MISMATCH)
    endif
  end subroutine nio_read_slice_set

!!!_  & nio_review_record
  subroutine nio_review_record &
       & (ierr,  nprop, nmask,  ndata, &
       &  head,  u,     krect,  flag)
    use TOUZA_Nio_std,only: KIOFS, sus_getpos, sus_rseek, WHENCE_ABS
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_header,only: litem, hi_DFMT, get_item
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: nprop       ! arbitrary property size
    integer,         intent(out) :: nmask       ! physical size of mask(index, etc) part
    integer,         intent(out) :: ndata       ! physical size of data part
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    integer,optional,intent(in)  :: flag
    character(len=litem) :: vp
    integer kfmt

    ierr = 0
    if (ierr.eq.0) call get_item(ierr, head, vp, hi_DFMT)
    if (ierr.eq.0) call parse_record_fmt(ierr, kfmt, vp)
    if (ierr.eq.0) then
       select case (kfmt)
       case (GFMT_MR4,GFMT_MR8,GFMT_MI4)
          call review_mtn(ierr, nprop, nmask, ndata, head, u, krect,  flag)
       case (GFMT_PR4,GFMT_PR8,GFMT_PI4)
          call review_ptx(ierr, nprop, nmask, ndata, head, u, krect,  flag)
       case default
          nprop = -1
          nmask = 0
          ndata = 0
       end select
    endif
  end subroutine nio_review_record
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
    if (n.gt.ld) then
       ierr = _ERROR(ERR_SIZE_MISMATCH)
       return
    endif

    select case (kfmt)
    case (GFMT_UR4)
       call put_data_frecord(ierr, d, n, u, krect)
    case (GFMT_UR8)
       call put_data_drecord(ierr, d, n, u, krect)
    case (GFMT_UI4)
       call put_data_irecord(ierr, d, n, u, krect)
    case (GFMT_MR4,GFMT_MR8,GFMT_MI4)
       call mtn_write_array(ierr, d, n, u, krect, vmiss, kfmt)
    ! case (GFMT_MR4)
    !    call put_data_mr4(ierr, d, n, u, krect, vmiss)
    ! case (GFMT_MR8)
    !    call put_data_mr8(ierr, d, n, u, krect, vmiss)
    ! case (GFMT_MI4)
    !    call put_data_mi4(ierr, d, n, u, krect, vmiss)
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
       call put_data_urt &
            & (ierr, d, n, u, krect, vmiss, kaxs, kopts)
    case (GFMT_MRT)
       call put_data_mrt &
            & (ierr, d, n, u, krect, vmiss, kaxs, kopts)
    case (GFMT_PR4,GFMT_PR8,GFMT_PI4)
       call ptx_write_array(ierr, d, n, u, krect, vmiss, kaxs, kfmt, kopts)
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
    if (n.gt.ld) then
       ierr = _ERROR(ERR_SIZE_MISMATCH)
       return
    endif

    select case (kfmt)
    case (GFMT_UR4)
       call put_data_frecord(ierr, d, n, u, krect)
    case (GFMT_UR8)
       call put_data_drecord(ierr, d, n, u, krect)
    case (GFMT_UI4)
       call put_data_irecord(ierr, d, n, u, krect)
    case (GFMT_MR4,GFMT_MR8,GFMT_MI4)
       call mtn_write_array(ierr, d, n, u, krect, vmiss, kfmt)
    ! case (GFMT_MR4)
    !    call put_data_mr4(ierr, d, n, u, krect, vmiss)
    ! case (GFMT_MR8)
    !    call put_data_mr8(ierr, d, n, u, krect, vmiss)
    ! case (GFMT_MI4)
    !    call put_data_mi4(ierr, d, n, u, krect, vmiss)
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
       call put_data_urt &
            & (ierr, d, n, u, krect, vmiss, kaxs, kopts)
    case (GFMT_MRT)
       call put_data_mrt &
            & (ierr, d, n, u, krect, vmiss, kaxs, kopts)
    case (GFMT_PR4,GFMT_PR8,GFMT_PI4)
       call ptx_write_array(ierr, d, n, u, krect, vmiss, kaxs, kfmt, kopts)
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
    if (n.gt.ld) then
       ierr = _ERROR(ERR_SIZE_MISMATCH)
       return
    endif

    select case (kfmt)
    case (GFMT_UR4)
       call put_data_frecord(ierr, d, n, u, krect)
    case (GFMT_UR8)
       call put_data_drecord(ierr, d, n, u, krect)
    case (GFMT_UI4)
       call put_data_irecord(ierr, d, n, u, krect)
    case (GFMT_MR4,GFMT_MR8,GFMT_MI4)
       call mtn_write_array(ierr, d, n, u, krect, vmiss, kfmt)
    ! case (GFMT_MR4)
    !    call put_data_mr4(ierr, d, n, u, krect, vmiss)
    ! case (GFMT_MR8)
    !    call put_data_mr8(ierr, d, n, u, krect, vmiss)
    ! case (GFMT_MI4)
    !    call put_data_mi4(ierr, d, n, u, krect, vmiss)
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
    case (GFMT_PR4,GFMT_PR8,GFMT_PI4)
       call ptx_write_array(ierr, d, n, u, krect, vmiss, kaxs, kfmt, kopts)
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select

    return
  end subroutine nio_write_data_core_i

!!!_  & nio_skip_records - forward/backward gtool-record skip
  subroutine nio_skip_records &
       & (ierr, n, u, nskip, head, krect)
    use TOUZA_Nio_std,   only: WHENCE_CURRENT, sus_skip_irec, sus_skip_lrec, choice
    use TOUZA_Nio_header,only: nitem, litem, hi_DFMT, get_item
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
       ! call debug_file_pos(u, 'mi4:0')
       call get_data_record(ierr, mb, u, krect)
       ! call debug_file_pos(u, 'mi4:1')
       if (ierr.eq.0) call sus_read_irec(ierr, u, vi, 0, swap, div=div, lmem=ncom)
       ! call debug_file_pos(u, 'mi4:2')
       if (ierr.eq.0) call sus_read_irec(ierr, u, vi, 0, swap, div=div, lmem=mb)
       ! call debug_file_pos(u, 'mi4:9')
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
    case (GFMT_PI4, GFMT_PR4, GFMT_PR8)
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
       case (GFMT_PI4, GFMT_PR4, GFMT_PR8)
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
!!!_  & nio_bwd_record - backword one record skip (lazy trial)
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

!!!_ + URC
!!!_  & get_data_urc - URC
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
!!!_  & put_data_urc - URC. *ABORT* on write.
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

!!!_ + [MU]RY
!!!_  & get_data_ury - URY
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
!!!_  & put_data_ury - URY
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

!!!_  & get_data_mry - MRY
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
!!!_  & put_data_mry - MRY
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

!!!_ + [MTn] core
!!!_  & mtn_review - review M[RI]n data
  subroutine mtn_review &
       & (ierr,  nmask, ndata, &
       &  head,  u,     krect, flag)
    use TOUZA_Nio_std,only: KIOFS, sus_getpos, sus_rseek, WHENCE_ABS
    use TOUZA_Nio_std,only: choice
    use TOUZA_Trp,    only: count_packed
    implicit none
    integer,parameter :: KISRC=KI32
    integer,         intent(out) :: ierr
    integer,         intent(out) :: nmask       ! physical size of mask(index, etc) part
    integer,         intent(out) :: ndata       ! physical size of data part
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    integer,optional,intent(in)  :: flag

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer(kind=KIOFS) :: apini
    integer mfull  ! logical full-size
    integer f

    ierr = 0
    ! note: position must be just after gtool header part

    ndata = -1
    nmask = -1

    f = choice(0, flag)

    if (ierr.eq.0) call pre_review(ierr, apini, u, f)
    ! packed data array size
    if (ierr.eq.0) call get_data_record(ierr, ndata, u, krect)
    if (ierr.eq.0) then
       mfull = parse_header_size(head, 0, 1)
       if (IAND(f, rev_plain_storage).eq.0) then
          nmask = mfull
       else
          nmask = count_packed(1, mfull, mold)
       endif
    endif
    if (ierr.eq.0) call post_review(ierr, apini, u, f)
  end subroutine mtn_review

!!!_  & mtn_write_array - store MTn data from plain (full) array
  subroutine mtn_write_array_d &
       & (ierr, &
       &  d, mfull, &
       &  u, krect, vmiss, kfmt)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(0:*)
    integer,         intent(in)  :: mfull
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt
    integer kpack
    integer ncom, npack
    integer,parameter :: mold = 0_KISRC
    character(len=*),parameter :: proc = 'mtn:wa'
#define _WORK workd

    ierr = 0
    kpack = legacy_packing(1, mfull)
    ncom = count_packed(1, mfull, mold)
    if (ierr.eq.0) call alloc_wmask(ierr, ncom, proc, mold)
    if (ierr.eq.0) call alloc_work(ierr, mfull, proc, d(0))    ! maximum allocation
    if (ierr.eq.0) then
       call mask_encode &
            & (ierr, npack, wmask,  _WORK, &
            &  d,    mfull, vmiss,  kpack)
    endif
    if (ierr.eq.0) then
       call mtn_write_data &
            & (ierr,  &
            &  wmask, _WORK, u, krect, kfmt, npack, mfull)
    endif
    if (ierr.eq.0) call alloc_wmask(ierr, -1, proc, mold)
    if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
#undef _WORK
  end subroutine mtn_write_array_d
  subroutine mtn_write_array_f &
       & (ierr, &
       &  d, mfull, &
       &  u, krect, vmiss, kfmt)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(0:*)
    integer,         intent(in)  :: mfull
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt
    integer kpack
    integer ncom, npack
    integer,parameter :: mold = 0_KISRC
    character(len=*),parameter :: proc = 'mtn:wa'
#define _WORK workf

    ierr = 0
    kpack = legacy_packing(1, mfull)
    ncom = count_packed(1, mfull, mold)
    if (ierr.eq.0) call alloc_wmask(ierr, ncom, proc, mold)
    if (ierr.eq.0) call alloc_work(ierr, mfull, proc, d(0))    ! maximum allocation
    if (ierr.eq.0) then
       call mask_encode &
            & (ierr, npack, wmask,  _WORK, &
            &  d,    mfull, vmiss,  kpack)
    endif
    if (ierr.eq.0) then
       call mtn_write_data &
            & (ierr,  &
            &  wmask, _WORK, u, krect, kfmt, npack, mfull)
    endif
    if (ierr.eq.0) call alloc_wmask(ierr, -1, proc, mold)
    if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
#undef _WORK
  end subroutine mtn_write_array_f
  subroutine mtn_write_array_i &
       & (ierr, &
       &  d, mfull, &
       &  u, krect, vmiss, kfmt)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(in)  :: d(0:*)
    integer,           intent(in)  :: mfull
    integer,           intent(in)  :: u
    integer,           intent(in)  :: krect
    real(kind=KRMIS),  intent(in)  :: vmiss
    integer,           intent(in)  :: kfmt
    integer kpack
    integer ncom, npack
    integer,parameter :: mold = 0_KISRC
    character(len=*),parameter :: proc = 'mtn:wa'
#define _WORK worki

    ierr = 0
    kpack = legacy_packing(1, mfull)
    ncom = count_packed(1, mfull, mold)
    if (ierr.eq.0) call alloc_wmask(ierr, ncom, proc, mold)
    if (ierr.eq.0) call alloc_work(ierr, mfull, proc, d(0))    ! maximum allocation
    if (ierr.eq.0) then
       call mask_encode &
            & (ierr, npack, wmask,  _WORK, &
            &  d,    mfull, vmiss,  kpack)
    endif
    if (ierr.eq.0) then
       call mtn_write_data &
            & (ierr,  &
            &  wmask, _WORK, u, krect, kfmt, npack, mfull)
    endif
    if (ierr.eq.0) call alloc_wmask(ierr, -1, proc, mold)
    if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
#undef _WORK
  end subroutine mtn_write_array_i

!!!_  & mtn_write_data - store MTn image
  subroutine mtn_write_data_d &
       & (ierr, &
       &  bmask, dpack, u, krect, kfmt, mpack, mfull)
    use TOUZA_Trp,only: count_packed
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32
    integer,            intent(out) :: ierr
    integer(kind=KISRC),intent(in)  :: bmask(0:*)   ! bit-packed mask
    real(kind=KARG),    intent(in)  :: dpack(0:*)
    integer,            intent(in)  :: u
    integer,            intent(in)  :: krect
    integer,            intent(in)  :: kfmt
    integer,            intent(in)  :: mpack
    integer,            intent(in)  :: mfull

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom

    ! file position MUST be after header record

    ierr = 0
    ncom = count_packed(1, mfull, mold)

    if (ierr.eq.0) call put_data_record(ierr, mpack, u, krect)
    if (ierr.eq.0) call put_data_record(ierr, bmask, ncom,  u, krect)
    if (ierr.eq.0) then
       select case(kfmt)
       case(GFMT_MR8)
          call put_data_drecord(ierr, dpack, mpack, u, krect)
       case(GFMT_MR4)
          call put_data_frecord(ierr, dpack, mpack, u, krect)
       case(GFMT_MI4)
          call put_data_irecord(ierr, dpack, mpack, u, krect)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    return
  end subroutine mtn_write_data_d
  subroutine mtn_write_data_f &
       & (ierr, &
       &  bmask, dpack, u, krect, kfmt, mpack, mfull)
    use TOUZA_Trp,only: count_packed
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32
    integer,            intent(out) :: ierr
    integer(kind=KISRC),intent(in)  :: bmask(0:*)
    real(kind=KARG),    intent(in)  :: dpack(0:*)
    integer,            intent(in)  :: u
    integer,            intent(in)  :: krect
    integer,            intent(in)  :: kfmt
    integer,            intent(in)  :: mpack
    integer,            intent(in)  :: mfull

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack

    ! file position MUST be after header record

    ierr = 0
    ncom = count_packed(1, mfull, mold)
    kpack = legacy_packing(1, mfull)

    if (ierr.eq.0) call put_data_record(ierr, mpack, u, krect)
    if (ierr.eq.0) call put_data_record(ierr, bmask, ncom,  u, krect)
    if (ierr.eq.0) then
       select case(kfmt)
       case(GFMT_MR8)
          call put_data_drecord(ierr, dpack, mpack, u, krect)
       case(GFMT_MR4)
          call put_data_frecord(ierr, dpack, mpack, u, krect)
       case(GFMT_MI4)
          call put_data_irecord(ierr, dpack, mpack, u, krect)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    return
  end subroutine mtn_write_data_f

  subroutine mtn_write_data_i &
       & (ierr, &
       &  bmask, dpack, u, krect, kfmt, mpack, mfull)
    use TOUZA_Trp,only: count_packed
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32
    integer,            intent(out) :: ierr
    integer(kind=KISRC),intent(in)  :: bmask(0:*)
    integer(kind=KARG), intent(in)  :: dpack(0:*)
    integer,            intent(in)  :: u
    integer,            intent(in)  :: krect
    integer,            intent(in)  :: kfmt
    integer,            intent(in)  :: mpack
    integer,            intent(in)  :: mfull

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncom
    integer kpack

    ! file position MUST be after header record

    ierr = 0
    ncom = count_packed(1, mfull, mold)
    kpack = legacy_packing(1, mfull)

    if (ierr.eq.0) call put_data_record(ierr, mpack, u, krect)
    if (ierr.eq.0) call put_data_record(ierr, bmask, ncom,  u, krect)
    if (ierr.eq.0) then
       select case(kfmt)
       case(GFMT_MR8)
          call put_data_drecord(ierr, dpack, mpack, u, krect)
       case(GFMT_MR4)
          call put_data_frecord(ierr, dpack, mpack, u, krect)
       case(GFMT_MI4)
          call put_data_irecord(ierr, dpack, mpack, u, krect)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    return
  end subroutine mtn_write_data_i

!!!_  & mtn_read_array - restore MTn data to plain (full) array
  subroutine mtn_read_array_d &
       & (ierr, &
       &  d, ldata, &
       &  u, krect, vmiss, kfmt, mfull, bes, nr)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: ldata        ! limit size of dpack (defined)
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt
    integer,         intent(in)  :: mfull
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr
    integer ncom, kpack
    integer mpack
    integer,parameter :: mold = 0_KISRC

    character(len=*),parameter :: proc = 'mtn:ra'
#define _WORK workd

    ierr = 0
    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mpack, u, krect)

    if (present(bes)) then
       if (.not.present(nr)) ierr = _ERROR(ERR_FEW_ARGUMENTS)
       if (ierr.eq.0) then
          call mtn_read_slice &
               & (ierr,  &
               &  d,     ldata, &
               &  u,     krect, vmiss, kfmt, mpack, mfull, bes, nr)
       endif
    else
       ncom = count_packed(1, mfull, mold)
       kpack = legacy_packing(1, mfull)
       if (ierr.eq.0) call alloc_wmask(ierr, ncom, proc, mold)
       if (ierr.eq.0) call alloc_work(ierr, mpack, proc, d(0))
       if (ierr.eq.0) then
          call mtn_read_data &
               & (ierr, &
               &  wmask, _WORK, &
               &  u,     krect, kfmt, mpack, mfull)
       endif
       if (ierr.eq.0) then
          call mask_decode(ierr, d, mfull, _WORK, wmask, vmiss, kpack)
       endif
       if (ierr.eq.0) call alloc_wmask(ierr, -1, proc, mold)
       if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
    endif
#undef _WORK
  end subroutine mtn_read_array_d
  subroutine mtn_read_array_f &
       & (ierr, &
       &  d, ldata, &
       &  u, krect, vmiss, kfmt, mfull, bes, nr)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: ldata        ! limit size of dpack (defined)
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt
    integer,         intent(in)  :: mfull
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr
    integer ncom, kpack
    integer mpack
    integer,parameter :: mold = 0_KISRC

    character(len=*),parameter :: proc = 'mtn:ra'
#define _WORK workf

    ierr = 0
    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mpack, u, krect)

    if (present(bes)) then
       if (.not.present(nr)) ierr = _ERROR(ERR_FEW_ARGUMENTS)
       if (ierr.eq.0) then
          call mtn_read_slice &
               & (ierr,  &
               &  d,     ldata, &
               &  u,     krect, vmiss, kfmt, mpack, mfull, bes, nr)
       endif
    else
       ncom = count_packed(1, mfull, mold)
       kpack = legacy_packing(1, mfull)
       if (ierr.eq.0) call alloc_wpack(ierr, ncom, proc, mold)
       if (ierr.eq.0) call alloc_work(ierr, mpack, proc, d(0))
       if (ierr.eq.0) then
          call mtn_read_data &
               & (ierr, &
               &  wpack, _WORK, &
               &  u,     krect, kfmt, mpack, mfull)
       endif
       if (ierr.eq.0) then
          call mask_decode(ierr, d, mfull, _WORK, wpack, vmiss, kpack)
       endif
       if (ierr.eq.0) call alloc_wpack(ierr, -1, proc, mold)
       if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
    endif
#undef _WORK
  end subroutine mtn_read_array_f
  subroutine mtn_read_array_i &
       & (ierr, &
       &  d, ldata, &
       &  u, krect, vmiss, kfmt, mfull, bes, nr)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(out) :: d(0:*)
    integer,           intent(in)  :: ldata        ! limit size of dpack (defined)
    integer,           intent(in)  :: u
    integer,           intent(in)  :: krect
    real(kind=KRMIS),  intent(in)  :: vmiss
    integer,           intent(in)  :: kfmt
    integer,           intent(in)  :: mfull
    integer,optional,  intent(in)  :: bes(3, *)
    integer,optional,  intent(in)  :: nr
    integer ncom, kpack
    integer mpack
    integer,parameter :: mold = 0_KISRC

    character(len=*),parameter :: proc = 'mtn:ra'
#define _WORK worki

    ierr = 0
    ! (physical/packed) data size
    if (ierr.eq.0) call get_data_record(ierr, mpack, u, krect)

    if (present(bes)) then
       if (.not.present(nr)) ierr = _ERROR(ERR_FEW_ARGUMENTS)
       if (ierr.eq.0) then
          call mtn_read_slice &
               & (ierr,  &
               &  d,     ldata, &
               &  u,     krect, vmiss, kfmt, mpack, mfull, bes, nr)
       endif
    else
       ncom = count_packed(1, mfull, mold)
       kpack = legacy_packing(1, mfull)
       if (ierr.eq.0) call alloc_wpack(ierr, ncom, proc, mold)
       if (ierr.eq.0) call alloc_work(ierr, mpack, proc, d(0))
       if (ierr.eq.0) then
          call mtn_read_data &
               & (ierr, &
               &  wpack, _WORK, &
               &  u,     krect, kfmt, mpack, mfull)
       endif
       if (ierr.eq.0) then
          call mask_decode(ierr, d, mfull, _WORK, wpack, vmiss, kpack)
       endif
       if (ierr.eq.0) call alloc_wpack(ierr, -1, proc, mold)
       if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
    endif
#undef _WORK
  end subroutine mtn_read_array_i

!!!_  & mtn_read_slice - restore MTn data slice
  subroutine mtn_read_slice_d &
       & (ierr,  &
       &  d,     ldata, &
       &  u,     krect, vmiss, kfmt, mpack, mfull, bes, nr)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: ldata
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt
    integer,         intent(in)  :: mpack
    integer,         intent(in)  :: mfull
    integer,         intent(in)  :: bes(3, *)
    integer,         intent(in)  :: nr

    integer ncom
    integer kpack
    integer lplain, nsub

    integer,parameter :: mold = 0_KISRC
    character(len=*),parameter :: proc = 'mtn:rs'
#define _WORK workd

    ierr = 0
    ncom  = count_packed(1, mfull, mold)
    kpack = legacy_unpacking(1, mfull)

    if (ierr.eq.0) call alloc_wpack(ierr, ncom, proc, mold)
    if (ierr.eq.0) call get_data_record(ierr, wpack, ncom, u, krect)

    if (ierr.eq.0) then
       lplain = count_bes(bes, nr)
       call alloc_wsubv(ierr, lplain, proc)
    endif
    if (ierr.eq.0) then
       call mask_to_idxl(ierr, wdsubv, wssubv, nsub, wpack, lplain, bes, nr, kpack)
    endif

    if (ierr.eq.0) call alloc_work(ierr, mpack, proc, d(0))
    if (ierr.eq.0) then
       select case(kfmt)
       case(GFMT_MR8)
          call get_data_drecord_list(ierr, _WORK, wssubv, nsub, u, krect, mpack)
       case(GFMT_MR4)
          call get_data_frecord_list(ierr, _WORK, wssubv, nsub, u, krect, mpack)
       case(GFMT_MI4)
          call get_data_irecord_list(ierr, _WORK, wssubv, nsub, u, krect, mpack)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    if (ierr.eq.0) then
       call subv_decode(ierr, d, lplain, _WORK, wdsubv, nsub, vmiss)
    endif
    if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
    if (ierr.eq.0) call alloc_wsubv(ierr, -1, proc)
    if (ierr.eq.0) call alloc_wpack(ierr, -1, proc, mold)
#undef _WORK
  end subroutine mtn_read_slice_d
  subroutine mtn_read_slice_f &
       & (ierr,  &
       &  d,     ldata, &
       &  u,     krect, vmiss, kfmt, mpack, mfull, bes, nr)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: ldata
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kfmt
    integer,         intent(in)  :: mpack
    integer,         intent(in)  :: mfull
    integer,         intent(in)  :: bes(3, *)
    integer,         intent(in)  :: nr

    integer ncom
    integer kpack
    integer lplain, nsub

    integer,parameter :: mold = 0_KISRC
    character(len=*),parameter :: proc = 'mtn:rs'
#define _WORK workf

    ierr = 0
    ncom  = count_packed(1, mfull, mold)
    kpack = legacy_unpacking(1, mfull)

    if (ierr.eq.0) call alloc_wpack(ierr, ncom, proc, mold)
    if (ierr.eq.0) call get_data_record(ierr, wpack, ncom, u, krect)

    if (ierr.eq.0) then
       lplain = count_bes(bes, nr)
       call alloc_wsubv(ierr, lplain, proc)
    endif
    if (ierr.eq.0) then
       call mask_to_idxl(ierr, wdsubv, wssubv, nsub, wpack, lplain, bes, nr, kpack)
    endif

    if (ierr.eq.0) call alloc_work(ierr, mpack, proc, d(0))
    if (ierr.eq.0) then
       select case(kfmt)
       case(GFMT_MR8)
          call get_data_drecord_list(ierr, _WORK, wssubv, nsub, u, krect, mpack)
       case(GFMT_MR4)
          call get_data_frecord_list(ierr, _WORK, wssubv, nsub, u, krect, mpack)
       case(GFMT_MI4)
          call get_data_irecord_list(ierr, _WORK, wssubv, nsub, u, krect, mpack)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    if (ierr.eq.0) then
       call subv_decode(ierr, d, lplain, _WORK, wdsubv, nsub, vmiss)
    endif
    if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
    if (ierr.eq.0) call alloc_wsubv(ierr, -1, proc)
    if (ierr.eq.0) call alloc_wpack(ierr, -1, proc, mold)
#undef _WORK
  end subroutine mtn_read_slice_f
  subroutine mtn_read_slice_i &
       & (ierr,  &
       &  d,     ldata, &
       &  u,     krect, vmiss, kfmt, mpack, mfull, bes, nr)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Trp,only: mask_to_idxl
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(out) :: d(0:*)
    integer,           intent(in)  :: ldata
    integer,           intent(in)  :: u
    integer,           intent(in)  :: krect
    real(kind=KRMIS),  intent(in)  :: vmiss
    integer,           intent(in)  :: kfmt
    integer,           intent(in)  :: mpack
    integer,           intent(in)  :: mfull
    integer,           intent(in)  :: bes(3, *)
    integer,           intent(in)  :: nr

    integer ncom
    integer kpack
    integer lplain, nsub

    integer,parameter :: mold = 0_KISRC
    character(len=*),parameter :: proc = 'mtn:rs'
#define _WORK worki

    ierr = 0
    ncom  = count_packed(1, mfull, mold)
    kpack = legacy_unpacking(1, mfull)

    if (ierr.eq.0) call alloc_wpack(ierr, ncom, proc, mold)
    if (ierr.eq.0) call get_data_record(ierr, wpack, ncom, u, krect)

    if (ierr.eq.0) then
       lplain = count_bes(bes, nr)
       call alloc_wsubv(ierr, lplain, proc)
    endif
    if (ierr.eq.0) then
       call mask_to_idxl(ierr, wdsubv, wssubv, nsub, wpack, lplain, bes, nr, kpack)
    endif

    if (ierr.eq.0) call alloc_work(ierr, mpack, proc, d(0))
    if (ierr.eq.0) then
       select case(kfmt)
       case(GFMT_MR8)
          call get_data_drecord_list(ierr, _WORK, wssubv, nsub, u, krect, mpack)
       case(GFMT_MR4)
          call get_data_frecord_list(ierr, _WORK, wssubv, nsub, u, krect, mpack)
       case(GFMT_MI4)
          call get_data_irecord_list(ierr, _WORK, wssubv, nsub, u, krect, mpack)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    if (ierr.eq.0) then
       call subv_decode(ierr, d, lplain, _WORK, wdsubv, nsub, vmiss)
    endif
    if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
    if (ierr.eq.0) call alloc_wsubv(ierr, -1, proc)
    if (ierr.eq.0) call alloc_wpack(ierr, -1, proc, mold)
#undef _WORK
  end subroutine mtn_read_slice_i

!!!_  & mtn_read_data - restore MTn image
  subroutine mtn_read_data_d &
       & (ierr, &
       &  bmask, dpack, &
       &  u,     krect, kfmt, mpack, mfull)
    use TOUZA_Trp,only: count_packed
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32
    integer,            intent(out) :: ierr
    integer(kind=KISRC),intent(out) :: bmask(0:*)
    real(kind=KARG),    intent(out) :: dpack(0:*)
    integer,            intent(in)  :: u
    integer,            intent(in)  :: krect
    integer,            intent(in)  :: kfmt
    integer,            intent(in)  :: mpack
    integer,            intent(in)  :: mfull

    integer,parameter :: mold = 0_KISRC
    integer kpack
    integer ncom

    ! file position MUST be after [MTn physical data size] record

    ierr = 0
    ncom = count_packed(1, mfull, mold)
    kpack = legacy_unpacking(1, mfull)
    if (ierr.eq.0) call get_data_record(ierr, bmask, ncom, u, krect)
    if (ierr.eq.0) then
       select case(kfmt)
       case(GFMT_MR8)
          call get_data_drecord(ierr, dpack, mpack, u, krect)
       case(GFMT_MR4)
          call get_data_frecord(ierr, dpack, mpack, u, krect)
       case(GFMT_MI4)
          call get_data_irecord(ierr, dpack, mpack, u, krect)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
  end subroutine mtn_read_data_d
  subroutine mtn_read_data_f &
       & (ierr, &
       &  bmask, dpack, &
       &  u,     krect, kfmt, mpack, mfull)
    use TOUZA_Trp,only: count_packed
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32
    integer,            intent(out) :: ierr
    integer(kind=KISRC),intent(out) :: bmask(0:*)
    real(kind=KARG),       intent(out) :: dpack(0:*)
    integer,            intent(in)  :: u
    integer,            intent(in)  :: krect
    integer,            intent(in)  :: kfmt
    integer,            intent(in)  :: mpack
    integer,            intent(in)  :: mfull

    integer,parameter :: mold = 0_KISRC
    integer kpack
    integer ncom

    ! file position MUST be after [MTn physical data size] record

    ierr = 0
    ncom = count_packed(1, mfull, mold)
    kpack = legacy_unpacking(1, mfull)
    if (ierr.eq.0) call get_data_record(ierr, bmask, ncom, u, krect)
    if (ierr.eq.0) then
       select case(kfmt)
       case(GFMT_MR8)
          call get_data_drecord(ierr, dpack, mpack, u, krect)
       case(GFMT_MR4)
          call get_data_frecord(ierr, dpack, mpack, u, krect)
       case(GFMT_MI4)
          call get_data_irecord(ierr, dpack, mpack, u, krect)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
  end subroutine mtn_read_data_f
  subroutine mtn_read_data_i &
       & (ierr, &
       &  bmask, dpack, &
       &  u,     krect, kfmt, mpack, mfull)
    use TOUZA_Trp,only: count_packed
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32
    integer,            intent(out) :: ierr
    integer(kind=KISRC),intent(out) :: bmask(0:*)
    integer(kind=KARG),       intent(out) :: dpack(0:*)
    integer,            intent(in)  :: u
    integer,            intent(in)  :: krect
    integer,            intent(in)  :: kfmt
    integer,            intent(in)  :: mpack
    integer,            intent(in)  :: mfull

    integer,parameter :: mold = 0_KISRC
    integer kpack
    integer ncom

    ! file position MUST be after [MTn physical data size] record

    ierr = 0
    ncom = count_packed(1, mfull, mold)
    kpack = legacy_unpacking(1, mfull)
    if (ierr.eq.0) call get_data_record(ierr, bmask, ncom, u, krect)
    if (ierr.eq.0) then
       select case(kfmt)
       case(GFMT_MR8)
          call get_data_drecord(ierr, dpack, mpack, u, krect)
       case(GFMT_MR4)
          call get_data_frecord(ierr, dpack, mpack, u, krect)
       case(GFMT_MI4)
          call get_data_irecord(ierr, dpack, mpack, u, krect)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
  end subroutine mtn_read_data_i

!!!_ + [MTn] obsolete
!!!_  & review_mtn - review M[RI]n data
  subroutine review_mtn &
       & (ierr,  nprop, nmask, ndata, &
       &  head,  u,     krect, flag)
    use TOUZA_Nio_std,only: KIOFS, sus_getpos, sus_rseek, WHENCE_ABS
    use TOUZA_Nio_std,only: choice
    use TOUZA_Trp,    only: count_packed
    implicit none
    integer,parameter :: KISRC=KI32
    integer,         intent(out) :: ierr
    integer,         intent(out) :: nprop       ! arbitrary property size
    integer,         intent(out) :: nmask       ! physical size of mask(index, etc) part
    integer,         intent(out) :: ndata       ! physical size of data part
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    integer,optional,intent(in)  :: flag

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer(kind=KIOFS) :: apini
    integer f
    integer mfull  ! logical full-size

    ierr = 0
    ! note: position must be just after gtool header part

    ! Typical usage:
    !   call review_mtn(..., nmask=nmask, ndata)
    !   allocate cmask(nmask)
    !   allocate subv(ndata), d(ndata)
    !   call restore_mr8_packed_d(...)
    nprop = 0
    ndata = -1
    nmask = -1

    f = choice(0, flag)

    if (ierr.eq.0) call pre_review(ierr, apini, u, f)
    ! write(*, *) 'pre: ', apini - 1
    ! packed data array size
    ! call debug_file_pos(u, 'review:0')
    if (ierr.eq.0) call get_data_record(ierr, ndata, u, krect)
    ! call debug_file_pos(u, 'review:1')
    if (ierr.eq.0) then
       mfull = parse_header_size(head, 0, 1)
       if (IAND(f, rev_plain_storage).eq.0) then
          nmask = mfull
       else
          nmask = count_packed(1, mfull, mold)
       endif
    endif
    if (ierr.eq.0) call post_review(ierr, apini, u, f)
    ! call debug_file_pos(u, 'review:9')
    ! write(*, *) f
  end subroutine review_mtn


!!!_ + gtool-3 discarded extension
!!!_  - JRn
!!!_  - ZRn
!!!_  - URS

!!!_ + *RT system (gtool-3 extension)
!!!_  & put_data_mrt - MRT:TOUZA/Trapiche masked format (full bundle)
  subroutine put_data_mrt_d &
       & (ierr, &
       &  d, n, u, krect, vmiss, kaxs, kopts)
    use TOUZA_Trp,only: suggest_filling, count_packed
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
    integer,parameter :: mold = 0_KISRC

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
          ncom = count_packed(1, m, mold)
          if (ierr.eq.0) then
             call mask_encode &
                  & (ierr, mb,   icom(2:), buf,   &
                  &  d(j:j+m-1), m,        vmiss, kpack)
          endif
          if (ierr.eq.0) then
             na = ncom + 2
             icom(0) = m
             icom(1) = kpack
             ! write(*, *) na, ncom, m, kpack, icom(0:na-1)
          endif
          ! write(*, *) j, m, pre, post, mb
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
  end subroutine put_data_mrt_d
  subroutine put_data_mrt_f &
       & (ierr, &
       &  d, n, u, krect, vmiss, kaxs, kopts)
    use TOUZA_Trp,only: suggest_filling, count_packed
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
    integer,parameter :: mold = 0_KISRC

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
          ncom = count_packed(1, m, mold)
          if (ierr.eq.0) then
             call mask_encode &
                  & (ierr, mb,   icom(2:), buf,   &
                  &  d(j:j+m-1), m,        vmiss, kpack)
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
  end subroutine put_data_mrt_f
!!!_  & get_data_mrt - MRT:TOUZA/Trapiche
  subroutine get_data_mrt_d &
       & (ierr,  &
       &  d,     n,   u, krect, vmiss, &
       &  kopts, bes, nr)
    use TOUZA_Trp,only: retrieve_nbgz, retrieve_ncnz, retrieve_extra
    use TOUZA_Trp,only: count_packed, suggest_filling
    use TOUZA_Nio_std,only: set_runl_loop
    use TOUZA_Trp,only: KB_HEAD
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,optional,intent(out) :: kopts(:)
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr

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
    integer nrl, lrl, jrpos
    character(len=*),parameter :: proc = 'mrt:g'
    integer kdmy(1)

    ierr = err_default
    if (present(bes).or.present(nr)) then
       if (.not.present(nr)) ierr = _ERROR(ERR_INVALID_ITEM)
       if (ierr.eq.0) then
          lrl = 2 * product(bes(3, 2:nr)) + 4
          lrl = max(lrl, product(bes(3, 1:nr)) * 2 * KB_HEAD + 1)
          call alloc_wsubv(ierr, lrl, proc)
       endif
       if (ierr.eq.0) call set_runl_loop(wdsubv, nrl, bes, nr)
       ! write(*, *) 'runl:', wdsubv(0:nrl)
       ! ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    endif

    if (ierr.eq.0) call get_data_urt_cache(ierr, cdummy, cmem, lcd, u, krect)

    nv = n
    jv = 0
    jrpos = 0
    do
       sub = .TRUE.
       ! call debug_file_pos(u, 'MRT')
       if (present(bes)) then
          if (ierr.eq.0) then
             call get_data_mrt_runl &
                  & (ierr,  &
                  &  d(jv:n-1),  mp,     nv,   u,     krect, sub, &
                  &  wssubv, wdsubv, jrpos,&
                  &  vmiss,  def_decode_trapiche, kopts, napp=na, icom=icom)
          endif
          ! write(*, *) ierr, jv, mp, nv, na, icom(0:1)
       else
          if (ierr.eq.0) then
             call get_data_urt_core &
                  & (ierr, &
                  &  buf,   mv,      nv,   u,    krect, sub, &
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
       endif
       if (ierr.eq.0) then
          ! write(*, *) present(bes), jv, mp, nv, sub
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
    if (ierr.eq.0) then
       if (sub) call get_data_record(ierr, kdmy, 0, u, krect)
    endif
    if (present(bes)) then
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, proc)
    endif
    return
  end subroutine get_data_mrt_d
  subroutine get_data_mrt_f &
       & (ierr,  &
       &  d,     n,   u, krect, vmiss, &
       &  kopts, bes, nr)
    use TOUZA_Trp,only: retrieve_nbgz, retrieve_ncnz, retrieve_extra
    use TOUZA_Trp,only: count_packed, suggest_filling
    use TOUZA_Nio_std,only: set_runl_loop
    use TOUZA_Trp,only: KB_HEAD
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,optional,intent(out) :: kopts(:)
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr

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
    integer nrl, lrl, jrpos
    character(len=*),parameter :: proc = 'mrt:g'
    integer kdmy(1)

    ierr = err_default
    if (present(bes).or.present(nr)) then
       if (.not.present(nr)) ierr = _ERROR(ERR_INVALID_ITEM)
       if (ierr.eq.0) then
          lrl = 2 * product(bes(3, 2:nr)) + 4
          lrl = max(lrl, product(bes(3, 1:nr)) * 2 * KB_HEAD + 1)
          call alloc_wsubv(ierr, lrl, proc)
       endif
       if (ierr.eq.0) call set_runl_loop(wdsubv, nrl, bes, nr)
    endif

    if (ierr.eq.0) call get_data_urt_cache(ierr, cdummy, cmem, lcd, u, krect)

    nv = n
    jv = 0
    jrpos = 0

    do
       sub = .TRUE.
       if (present(bes)) then
          ! call debug_file_pos(u, 'MRT')
          if (ierr.eq.0) then
             call get_data_mrt_runl &
                  & (ierr,  &
                  &  d(jv:n-1),  mp,     nv,   u,     krect, sub, &
                  &  wssubv, wdsubv, jrpos,&
                  &  vmiss,  def_decode_trapiche, kopts, napp=na, icom=icom)
          endif
          ! write(*, *) ierr, jv, mp, nv, na, icom(0:1)
       else
          if (ierr.eq.0) then
             call get_data_urt_core &
                  & (ierr, &
                  &  buf,   mv,      nv,   u,    krect, sub, &
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
       endif
       ! if (ierr.eq.0) then
       !    sub = .TRUE.
       !    call get_data_urt_core &
       !         & (ierr, &
       !         &  buf,   mv, nv,  u, krect,   sub,  &
       !         &  vmiss, def_decode_trapiche, kopts, napp=na, kapp=icom)
       ! endif
       ! if (ierr.eq.0) then
       !    ncom = na - 2
       !    mp = icom(0)
       !    kpack = icom(1)
       !    kpack = suggest_filling(1, mp, kcode=def_decode_trapiche, kfill=kpack)

       !    call mask_decode &
       !         & (ierr,  d(jv:jv+mp-1), mp, buf, icom(2:), vmiss, kpack)
       ! endif
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
    if (ierr.eq.0) then
       if (sub) call get_data_record(ierr, kdmy, 0, u, krect)
    endif
    if (present(bes)) then
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, proc)
    endif
    return
  end subroutine get_data_mrt_f
!!!_  & put_data_urt - URT:TOUZA/Trapiche plain format (full bundle)
  subroutine put_data_urt_d &
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
  end subroutine put_data_urt_d
  subroutine put_data_urt_f &
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
  end subroutine put_data_urt_f
!!!_  & get_data_urt - URT:TOUZA/Trapiche
  subroutine get_data_urt_d &
       & (ierr,  &
       &  d,     n,    u,    krect, vmiss, &
       &  kopts, bes,  nr)
    use TOUZA_Nio_std,only: set_runl_loop
    use TOUZA_Trp,only: KB_HEAD
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,optional,intent(out) :: kopts(:)
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr

    logical sub
    integer jv,   mv,   nv
    integer,parameter :: lcd = 2
    integer cdummy(lcd)
    integer cmem
    integer nrl, lrl, jrpos
    character(len=*),parameter :: proc = 'urt:g'
    integer kdmy(1)

    ierr = err_default
    if (present(bes)) then
       if (.not.present(nr)) ierr = _ERROR(ERR_INVALID_ITEM)
       if (ierr.eq.0) then
          lrl = 2 * product(bes(3, 2:nr)) + 4
          lrl = max(lrl, product(bes(3, 1:nr)) * 2 * KB_HEAD + 1)
          call alloc_wsubv(ierr, lrl, proc)
       endif
       if (ierr.eq.0) call set_runl_loop(wdsubv, nrl, bes, nr)
    endif

    if (ierr.eq.0) call get_data_urt_cache(ierr, cdummy, cmem, lcd, u, krect)

    jv = 0
    nv = n
    sub = .TRUE.
    jrpos = 0
    do
       if (ierr.eq.0) then
          if (present(bes)) then
             call get_data_urt_runl &
                  & (ierr,       &
                  &  d(jv:jv+nv-1), mv,  nv,  u,  krect,  sub, &
                  &  wssubv,     wdsubv,  jrpos,  &
                  &  vmiss,      def_decode_trapiche,       &
                  &  kopts)
          else
             call get_data_urt_core &
                  & (ierr,       &
                  &  d(jv:jv+nv-1), mv,  nv,  u,  krect,  sub, &
                  &  vmiss,      def_decode_trapiche,       &
                  &  kopts)
          endif
       endif
       if (ierr.eq.0) then
          jv = jv + mv
          nv = nv - mv
          if (nv.eq.0) exit
          if (nv.lt.0) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          if (.not.sub) ierr = _ERROR(ERR_BROKEN_RECORD)
       endif
       if (ierr.ne.0) exit
    enddo
    if (present(bes)) then
       if (ierr.eq.0) then
          if (sub) call get_data_record(ierr, kdmy, 1, u, krect)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, proc)
    endif

    return
  end subroutine get_data_urt_d
  subroutine get_data_urt_f &
       & (ierr,  &
       &  d,     n,    u,    krect, vmiss, &
       &  kopts, bes,   nr)
    use TOUZA_Nio_std,only: set_runl_loop
    use TOUZA_Trp,only: KB_HEAD
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,optional,intent(out) :: kopts(:)
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr

    logical sub
    integer jv,   mv,   nv
    integer,parameter :: lcd = 2
    integer cdummy(lcd)
    integer cmem
    integer nrl, lrl, jrpos
    character(len=*),parameter :: proc = 'urt:g'
    integer kdmy(1)

    ierr = err_default
    if (present(bes).or.present(nr)) then
       if (.not.present(nr)) ierr = _ERROR(ERR_INVALID_ITEM)
       if (ierr.eq.0) then
          lrl = 2 * product(bes(3, 2:nr)) + 4
          lrl = max(lrl, product(bes(3, 1:nr)) * 2 * KB_HEAD + 1)
          call alloc_wsubv(ierr, lrl, proc)
       endif
       if (ierr.eq.0) call set_runl_loop(wdsubv, nrl, bes, nr)
    endif

    if (ierr.eq.0) call get_data_urt_cache(ierr, cdummy, cmem, lcd, u, krect)

    jv = 0
    nv = n
    sub = .TRUE.
    jrpos = 0
    do
       if (ierr.eq.0) then
          if (present(bes)) then
             call get_data_urt_runl &
                  & (ierr,       &
                  &  d(jv:jv+nv-1), mv,  nv,  u,  krect,  sub, &
                  &  wssubv,     wdsubv,  jrpos,  &
                  &  vmiss,      def_decode_trapiche,       &
                  &  kopts)
          else
             call get_data_urt_core &
                  & (ierr,       &
                  &  d(jv:jv+nv-1), mv,  nv,  u,  krect,  sub, &
                  &  vmiss,      def_decode_trapiche,       &
                  &  kopts)
          endif
          ! if (ierr.eq.0) japp = japp + ma
       endif
       if (ierr.eq.0) then
          jv = jv + mv
          nv = nv - mv
          if (nv.eq.0) exit
          if (nv.lt.0) ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          if (.not.sub) ierr = _ERROR(ERR_BROKEN_RECORD)
       endif
       if (ierr.ne.0) exit
    enddo
    if (present(bes)) then
       if (ierr.eq.0) then
          if (sub) call get_data_record(ierr, kdmy, 1, u, krect)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, proc)
    endif

    return
  end subroutine get_data_urt_f

!!!_  & put_data_urt_cache
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
!!!_  & get_data_urt_cache
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

!!!_  & put_data_urt_core
  subroutine put_data_urt_core_d &
       & (ierr,  &
       &  d,     n,     u,     krect, pre,  post,  &
       &  vmiss, mbits, xbits, xtop,  xbtm, kcode, kapp)
    use TOUZA_Trp,only: count_packed, encode_alloc, retrieve_nbgz
    use TOUZA_Trp,only: KB_HEAD, guardar_extra
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
    use TOUZA_Trp,only: count_packed, encode_alloc, retrieve_nbgz
    use TOUZA_Trp,only: KB_HEAD, guardar_extra
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
!!!_  & get_data_urt_core
  subroutine get_data_urt_core_d &
       & (ierr, &
       &  d,     m,     n,    u,    krect,  sub, &
       &  vmiss, kcode, kopts,napp, kapp)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Trp,only: decode_alloc, retrieve_nbgz, retrieve_ncnz, retrieve_extra
    use TOUZA_Trp,only: KB_HEAD,      show_bagazo_props
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
    integer,optional,intent(inout) :: kapp(0:)

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
       ! write(*, *) 'before', nbgz, sub, present(kapp)
       ! call debug_file_pos(u, 'core/before')
       if (nbgz.gt.0) then
          call get_data_record(ierr, ibagaz(KB_HEAD:KB_HEAD+nbgz-1), nbgz, u, rectx, sub)
       endif
    endif

    if (ierr.eq.0) then
       ncnz = retrieve_ncnz(ibagaz)
       ! write(*, *) 'after', ncnz, sub
       ! call debug_file_pos(u, 'core/after')
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
    use TOUZA_Nio_std,only: choice
    use TOUZA_Trp,only: decode_alloc, retrieve_nbgz, retrieve_ncnz, retrieve_extra
    use TOUZA_Trp,only: KB_HEAD,      show_bagazo_props
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
    integer,optional,intent(inout) :: kapp(0:)

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
       if (nbgz.gt.0) then
          call get_data_record(ierr, ibagaz(KB_HEAD:KB_HEAD+nbgz-1), nbgz, u, rectx, sub=sub)
       endif
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
!!!_  & get_data_urt_runl
  subroutine get_data_urt_runl_d &
       & (ierr, &
       &  d,      m,     n,    u,    krect, sub, &
       &  ibagaz, runl,  jrpos, &
       &  vmiss,  kcode, kopts)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Trp,only: decode_alloc, retrieve_nbgz, retrieve_ncnz, retrieve_extra
    use TOUZA_Trp,only: KB_HEAD,      show_bagazo_props
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,            intent(out)   :: ierr
    real(kind=KARG),    intent(out)   :: d(0:*)
    integer,            intent(out)   :: m    ! elements success
    integer,            intent(in)    :: n    ! elements limit
    integer,            intent(in)    :: krect
    integer,            intent(in)    :: u
    logical,            intent(inout) :: sub
    real(kind=KRMIS),   intent(in)    :: vmiss
    integer(kind=KISRC),intent(out)   :: ibagaz(0:*)   ! work area
    integer,            intent(inout) :: runl(0:*)
    integer,            intent(inout) :: jrpos
    integer,            intent(in)    :: kcode
    integer,optional,   intent(out)   :: kopts(:)

    ! integer(kind=KISRC) :: ibagaz(0:2*n+KB_HEAD)
    integer(kind=KISRC) :: kdmy(1)
    integer nbgz, ncnz
    integer na, xid
    logical cont
    integer jv, nv
    integer rectx
    character(len=*),parameter :: proc = 'urt:g'
#define _WORK workd

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
       if (na.gt.0) then
          cont = .TRUE.
          call get_data_record(ierr, kdmy, 1, u, rectx, sub=cont)
       endif
    endif
    if (ierr.eq.0) then
       ncnz = retrieve_ncnz(ibagaz)
       if (runl(jrpos).ge.ncnz) then
          ! skip all
          m = 0
          runl(jrpos) = runl(jrpos) - ncnz
          call get_data_record(ierr, ibagaz(KB_HEAD:KB_HEAD), 1, u, rectx, sub)
          return
       endif
    endif

    if (ierr.eq.0) then
       ! read full sheet, to be improved
       nbgz = retrieve_nbgz(ibagaz)
       if (nbgz.gt.0) then
          call get_data_record(ierr, ibagaz(KB_HEAD:KB_HEAD+nbgz-1), nbgz, u, rectx, sub)
       endif
    endif

    ! decode full sheet, to be improved
    if (ierr.eq.0) call alloc_work(ierr, ncnz, proc, d(0))
    if (ierr.eq.0) call decode_alloc(ierr, _WORK, ibagaz, ncnz, vmiss, kcode)
    if (ierr.eq.0) then
       m = 0
       jv = 0
       do
          jv = jv + runl(jrpos)
          ! write(*, *) 'store', m, jrpos, jv, runl(jrpos:jrpos+1)
          if (jv.ge.ncnz) exit
          nv = min(ncnz - jv, runl(jrpos+1))
          ! write(*, *) '     ', nv, n
          if (m+nv.gt.n) then
             ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
             exit
          endif
          d(m:m+nv-1) = _WORK(jv:jv+nv-1)
          m = m + nv
          jv = jv + nv
          if (nv.lt.runl(jrpos+1)) then
             runl(jrpos) = 0
             runl(jrpos+1) = runl(jrpos+1) - nv
             exit
          endif
          jrpos = jrpos + 2
       enddo
    endif
    if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
    return
#undef _WORK
  end subroutine get_data_urt_runl_d
  subroutine get_data_urt_runl_f &
       & (ierr, &
       &  d,      m,     n,    u,    krect, sub, &
       &  ibagaz, runl,  jrpos, &
       &  vmiss,  kcode, kopts)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Trp,only: decode_alloc, retrieve_nbgz, retrieve_ncnz, retrieve_extra
    use TOUZA_Trp,only: KB_HEAD,      show_bagazo_props
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,            intent(out)   :: ierr
    real(kind=KARG),    intent(out)   :: d(0:*)
    integer,            intent(out)   :: m    ! elements success
    integer,            intent(in)    :: n    ! elements limit
    integer,            intent(in)    :: krect
    integer,            intent(in)    :: u
    logical,            intent(inout) :: sub
    real(kind=KRMIS),   intent(in)    :: vmiss
    integer(kind=KISRC),intent(out)   :: ibagaz(0:*)   ! work area
    integer,            intent(inout) :: runl(0:*)
    integer,            intent(inout) :: jrpos
    integer,            intent(in)    :: kcode
    integer,optional,   intent(out)   :: kopts(:)

    ! integer(kind=KISRC) :: ibagaz(0:2*n+KB_HEAD)
    integer(kind=KISRC) :: kdmy(1)
    integer nbgz, ncnz
    integer na, xid
    logical cont
    integer jv, nv
    integer rectx
    real(kind=KRSRC) :: rmiss
    character(len=*),parameter :: proc = 'urt:g'
#define _WORK workf

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
       if (na.gt.0) then
          cont = .TRUE.
          call get_data_record(ierr, kdmy, 1, u, rectx, sub=cont)
       endif
    endif
    if (ierr.eq.0) then
       ncnz = retrieve_ncnz(ibagaz)
       if (runl(jrpos).ge.ncnz) then
          ! skip all
          m = 0
          runl(jrpos) = runl(jrpos) - ncnz
          call get_data_record(ierr, ibagaz(KB_HEAD:KB_HEAD), 1, u, rectx, sub)
          return
       endif
    endif

    if (ierr.eq.0) then
       ! read full sheet, to be improved
       nbgz = retrieve_nbgz(ibagaz)
       if (nbgz.gt.0) then
          call get_data_record(ierr, ibagaz(KB_HEAD:KB_HEAD+nbgz-1), nbgz, u, rectx, sub)
       endif
    endif

    ! decode full sheet, to be improved
    if (ierr.eq.0) call alloc_work(ierr, ncnz, proc, d(0))
    if (ierr.eq.0) then
       rmiss = real(vmiss, kind=KRSRC)
       call decode_alloc(ierr, _WORK, ibagaz, ncnz, rmiss, kcode)
    endif
    if (ierr.eq.0) then
       m = 0
       jv = 0
       do
          jv = jv + runl(jrpos)
          ! write(*, *) 'store', m, jrpos, jv, runl(jrpos:jrpos+1)
          if (jv.ge.ncnz) exit
          nv = min(ncnz - jv, runl(jrpos+1))
          ! write(*, *) '     ', nv, n
          if (m+nv.gt.n) then
             ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
             exit
          endif
          d(m:m+nv-1) = _WORK(jv:jv+nv-1)
          m = m + nv
          jv = jv + nv
          if (nv.lt.runl(jrpos+1)) then
             runl(jrpos) = 0
             runl(jrpos+1) = runl(jrpos+1) - nv
             exit
          endif
          jrpos = jrpos + 2
       enddo
    endif
    if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
    return
#undef _WORK
  end subroutine get_data_urt_runl_f

!!!_  & get_data_mrt_runl
  subroutine get_data_mrt_runl_d &
       & (ierr, &
       &  d,      m,     n,    u,    krect, sub, &
       &  ibagaz, runl,  jrpos, &
       &  vmiss,  kcode, kopts,napp, icom)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Trp,only: decode_alloc, retrieve_nbgz, retrieve_ncnz, retrieve_extra
    use TOUZA_Trp,only: KB_HEAD,      show_bagazo_props, suggest_filling
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,            intent(out)   :: ierr
    real(kind=KARG),    intent(out)   :: d(0:*)
    integer,            intent(out)   :: m    ! elements success
    integer,            intent(in)    :: n    ! elements limit
    integer,            intent(in)    :: krect
    integer,            intent(in)    :: u
    logical,            intent(inout) :: sub
    real(kind=KRMIS),   intent(in)    :: vmiss
    integer(kind=KISRC),intent(out)   :: ibagaz(0:*)   ! work area
    integer,            intent(inout) :: runl(0:*)
    integer,            intent(inout) :: jrpos
    integer,            intent(in)    :: kcode
    integer,optional,   intent(out)   :: kopts(:)
    integer,            intent(out)   :: napp
    integer,            intent(inout) :: icom(0:)

    ! integer(kind=KISRC) :: ibagaz(0:2*n+KB_HEAD)
    integer nbgz, ncnz
    integer na, ma, xid, mp
    logical cont
    integer jm, jc, mm
    integer rectx
    integer kpack, ncom
    character(len=*),parameter :: proc = 'mrt:g'
#define _WORK workd

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
       napp = na
       if (na.gt.0) then
          cont = .TRUE.
          ma = min(na, size(icom))
          call get_data_record(ierr, icom(0:ma-1), ma, u, rectx, sub=cont)
       endif
    endif
    if (ierr.eq.0) then
       mp = icom(0)
       if (runl(jrpos).ge.mp) then
          ! skip all
          m = 0
          runl(jrpos) = runl(jrpos) - mp
          call get_data_record(ierr, ibagaz(KB_HEAD:KB_HEAD), 1, u, rectx, sub)
          return
       endif
    endif

    if (ierr.eq.0) then
       ncnz = retrieve_ncnz(ibagaz)

       ! read full sheet, to be improved
       nbgz = retrieve_nbgz(ibagaz)
       if (nbgz.gt.0) then
          call get_data_record(ierr, ibagaz(KB_HEAD:KB_HEAD+nbgz-1), nbgz, u, rectx, sub)
       endif
    endif

    if (ierr.eq.0) then
       ncom = na - 2
       kpack = icom(1)
       kpack = suggest_filling(1, mp, kcode=def_decode_trapiche, kfill=kpack)
    endif
    if (ierr.eq.0) call alloc_wpack(ierr, mp, proc, 0_KISRC)
    if (ierr.eq.0) call pack_restore(ierr, wpack, icom(2:), mp, 1, kpack)
    ! decode full sheet, to be improved
    if (ierr.eq.0) call alloc_work(ierr, ncnz, proc, d(0))
    if (ierr.eq.0) call decode_alloc(ierr, _WORK, ibagaz, ncnz, vmiss, kcode)
    if (ierr.eq.0) then
       ! write(*,*) 'cnz', mp, ncnz
       ! write(*,*) 'mask', wpack(0:mp-1)
       ! write(*,*) 'work', _WORK(0:ncnz-1)
       m = 0
       jm = 0
       jc = 0
       do
          mm = min(runl(jrpos), mp - jm)
          runl(jrpos) = runl(jrpos) - mm
          if (runl(jrpos).gt.0) exit
          jc = jc + SUM(wpack(jm:jm+mm-1))
          jm = jm + mm
          mm = min(runl(jrpos+1), mp - jm)
          do jm = jm, jm + mm - 1
             if (wpack(jm).eq.0) then
                d(m) = vmiss
             else
                d(m) = _WORK(jc)
                jc = jc + 1
             endif
             m = m + 1
          enddo
          runl(jrpos+1) = runl(jrpos+1) - mm
          if (runl(jrpos+1).gt.0) exit
          jrpos = jrpos + 2
       enddo
    endif
    if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
    if (ierr.eq.0) call alloc_wpack(ierr, -1, proc, 0_KISRC)
    return
#undef _WORK
  end subroutine get_data_mrt_runl_d
  subroutine get_data_mrt_runl_f &
       & (ierr, &
       &  d,      m,     n,    u,    krect, sub, &
       &  ibagaz, runl,  jrpos, &
       &  vmiss,  kcode, kopts,napp, icom)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Trp,only: decode_alloc, retrieve_nbgz, retrieve_ncnz, retrieve_extra
    use TOUZA_Trp,only: KB_HEAD,      show_bagazo_props, suggest_filling
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KFLT
    integer,            intent(out)   :: ierr
    real(kind=KARG),    intent(out)   :: d(0:*)
    integer,            intent(out)   :: m    ! elements success
    integer,            intent(in)    :: n    ! elements limit
    integer,            intent(in)    :: krect
    integer,            intent(in)    :: u
    logical,            intent(inout) :: sub
    real(kind=KRMIS),   intent(in)    :: vmiss
    integer(kind=KISRC),intent(out)   :: ibagaz(0:*)   ! work area
    integer,            intent(inout) :: runl(0:*)
    integer,            intent(inout) :: jrpos
    integer,            intent(in)    :: kcode
    integer,optional,   intent(out)   :: kopts(:)
    integer,            intent(out)   :: napp
    integer,            intent(inout) :: icom(0:)

    ! integer(kind=KISRC) :: ibagaz(0:2*n+KB_HEAD)
    integer nbgz, ncnz
    integer na, ma, xid, mp
    logical cont
    integer jm, jc, mm
    integer rectx
    integer kpack, ncom
    character(len=*),parameter :: proc = 'mrt:g'
    real(kind=KRSRC) :: rmiss
#define _WORK workf

    ierr = err_default
    rmiss = real(vmiss, kind=KRSRC)

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
       napp = na
       if (na.gt.0) then
          cont = .TRUE.
          ma = min(na, size(icom))
          call get_data_record(ierr, icom(0:ma-1), ma, u, rectx, sub=cont)
       endif
    endif
    if (ierr.eq.0) then
       mp = icom(0)
       if (runl(jrpos).ge.mp) then
          ! skip all
          m = 0
          runl(jrpos) = runl(jrpos) - mp
          call get_data_record(ierr, ibagaz(KB_HEAD:KB_HEAD), 1, u, rectx, sub)
          return
       endif
    endif

    if (ierr.eq.0) then
       ncnz = retrieve_ncnz(ibagaz)

       ! read full sheet, to be improved
       nbgz = retrieve_nbgz(ibagaz)
       if (nbgz.gt.0) then
          call get_data_record(ierr, ibagaz(KB_HEAD:KB_HEAD+nbgz-1), nbgz, u, rectx, sub)
       endif
    endif

    if (ierr.eq.0) then
       ncom = na - 2
       kpack = icom(1)
       kpack = suggest_filling(1, mp, kcode=def_decode_trapiche, kfill=kpack)
    endif
    if (ierr.eq.0) call alloc_wpack(ierr, mp, proc, 0_KISRC)
    if (ierr.eq.0) call pack_restore(ierr, wpack, icom(2:), mp, 1, kpack)
    ! decode full sheet, to be improved
    if (ierr.eq.0) call alloc_work(ierr, ncnz, proc, d(0))
    if (ierr.eq.0) call decode_alloc(ierr, _WORK, ibagaz, ncnz, rmiss, kcode)
    if (ierr.eq.0) then
       ! write(*,*) 'cnz', mp, ncnz
       ! write(*,*) 'mask', wpack(0:mp-1)
       ! write(*,*) 'work', _WORK(0:ncnz-1)
       m = 0
       jm = 0
       jc = 0
       do
          mm = min(runl(jrpos), mp - jm)
          runl(jrpos) = runl(jrpos) - mm
          if (runl(jrpos).gt.0) exit
          jc = jc + SUM(wpack(jm:jm+mm-1))
          jm = jm + mm
          mm = min(runl(jrpos+1), mp - jm)
          do jm = jm, jm + mm - 1
             if (wpack(jm).eq.0) then
                d(m) = rmiss
             else
                d(m) = _WORK(jc)
                jc = jc + 1
             endif
             m = m + 1
          enddo
          runl(jrpos+1) = runl(jrpos+1) - mm
          if (runl(jrpos+1).gt.0) exit
          jrpos = jrpos + 2
       enddo
    endif
    if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
    if (ierr.eq.0) call alloc_wpack(ierr, -1, proc, 0_KISRC)
    return
#undef _WORK
  end subroutine get_data_mrt_runl_f

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

    integer,parameter :: lo = lopts_urt
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
    integer,parameter :: lo = lopts_urt

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
             ! write(*, *) m, refr
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
          ! write(*, *) 'refr', refr(1:3)
          call helper_props(mbits, xbits, xlu(1), refr(2), refr(1), refr(3))
          kopts(PROP_URT_XBOTTOM)  = xlu(1)
          if (kopts(PROP_URT_XBITS).eq.PROP_DEFAULT) then
             kopts(PROP_URT_XBITS) = xbits
          else
             kopts(PROP_URT_XTOP) = xlu(1) + max(0, kopts(PROP_URT_XBITS))
          endif
          kopts(PROP_URT_MANTISSA) = mbits
       endif
    endif
    ! write(*, *) 'urt', kopts((/PROP_URT_XBOTTOM, PROP_URT_XTOP, PROP_URT_XBITS/))
  end subroutine parse_urt_options
!!!_  & show_urt_options
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

!!!_ + [PTx] Partial Compressed Row Storage (P[IR]x)
!!!_  & review_ptx - review P[RI]n plain properties
  subroutine review_ptx &
       & (ierr,  nprop, nmask, ndata, &
       &  head,  u,     krect, flag)
    use TOUZA_Nio_std,only: KIOFS, sus_getpos, sus_rseek, WHENCE_ABS
    use TOUZA_Nio_std,only: choice
    use TOUZA_Trp,    only: count_packed
    implicit none
    integer,parameter :: KISRC=KI32
    integer,         intent(out) :: ierr
    integer,         intent(out) :: nprop       ! arbitrary property size
    integer,         intent(out) :: nmask       ! physical size of mask(index, etc) part
    integer,         intent(out) :: ndata       ! physical size of data part
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    integer,optional,intent(in)  :: flag

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    logical cont
    integer nbase, mbits
    integer kaxs(laxs)
    integer f
    integer opts(lopts_ptx)
    integer(kind=KIOFS) :: apini

    ierr = 0
    nprop = lopts_ptx
    ndata = -1
    nmask = -1
    ! note: position must be just after gtool header part
    f = choice(0, flag)
    if (ierr.eq.0) call pre_review(ierr, apini, u, f)
    cont = .TRUE.
    if (ierr.eq.0) call get_data_record(ierr, opts, lopts_ptx, u, krect, sub=cont)
    if (ierr.eq.0) ndata = opts(PROP_PTX_DATA)
    if (ierr.eq.0) then
       mbits = opts(PROP_PTX_MBITS)
       call parse_header_size_all(kaxs, head)
       nbase = ptx_vec_size(kaxs, laxs, opts)
       if (IAND(f, rev_plain_storage).eq.0) then
          nmask = nbase
       else
          nmask = count_packed(mbits, nbase, mold)
       endif
    endif
    if (ierr.eq.0) call post_review(ierr, apini, u, f)
  end subroutine review_ptx

!!!_  & ptx_write_array - write flat array in PTx format
  subroutine ptx_write_array_d &
       & (ierr, &
       &  d, n, u, krect, vmiss, kaxs, kfmt, kopts)
    use TOUZA_Trp,only: pack_store
    implicit none
    integer,parameter :: KARG=KDBL
    integer,parameter :: KISRC=KI32
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kaxs(*)
    integer,         intent(in)  :: kfmt
    integer,optional,intent(in)  :: kopts(:)

    integer,parameter :: mold = 0_KISRC
    character(len=*),parameter :: wtag = 'wptx:a'

    integer nbase, npack
    integer popts(lopts_ptx)

#define _WORK workd

    ierr = 0

    if (ierr.eq.0) call ptx_def_options(ierr, popts, kopts)
    if (ierr.eq.0) call ptx_set_shape(popts, kaxs, laxs)
    if (ierr.eq.0) nbase = ptx_row_size(popts, kaxs, laxs)
    if (ierr.eq.0) call alloc_wmask(ierr, nbase, wtag, mold)
    if (ierr.eq.0) call ptx_gen_ccvec(ierr, popts, wmask, nbase, d, kaxs, laxs, vmiss)
    if (ierr.eq.0) npack = popts(PROP_PTX_DATA)
    if (ierr.eq.0) call alloc_work(ierr, npack, wtag, d(0))
    if (ierr.eq.0) call ptx_pack_data(ierr, _WORK, d, wmask, kaxs, laxs, popts)

    if (ierr.eq.0) then
       call ptx_write_data &
            & (ierr,  &
            &  popts, wmask, nbase, _WORK, npack, &
            &  u,     krect, kfmt)
    endif

    if (ierr.eq.0) call alloc_work(ierr, -1, wtag, d(0))
    if (ierr.eq.0) call alloc_wmask(ierr, -1, wtag, mold)
#undef _WORK
  end subroutine ptx_write_array_d
  subroutine ptx_write_array_f &
       & (ierr, &
       &  d, n, u, krect, vmiss, kaxs, kfmt, kopts)
    use TOUZA_Trp,only: pack_store
    implicit none
    integer,parameter :: KARG=KFLT
    integer,parameter :: KISRC=KI32
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(in)  :: d(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kaxs(*)
    integer,         intent(in)  :: kfmt
    integer,optional,intent(in)  :: kopts(:)

    integer,parameter :: mold = 0_KISRC
    character(len=*),parameter :: wtag = 'wptx:a'

    integer nbase, npack
    integer popts(lopts_ptx)

#define _WORK workf

    ierr = 0

    if (ierr.eq.0) call ptx_def_options(ierr, popts, kopts)
    if (ierr.eq.0) call ptx_set_shape(popts, kaxs, laxs)
    if (ierr.eq.0) nbase = ptx_row_size(popts, kaxs, laxs)
    if (ierr.eq.0) call alloc_wmask(ierr, nbase, wtag, mold)
    if (ierr.eq.0) call ptx_gen_ccvec(ierr, popts, wmask, nbase, d, kaxs, laxs, vmiss)
    if (ierr.eq.0) npack = popts(PROP_PTX_DATA)
    if (ierr.eq.0) call alloc_work(ierr, npack, wtag, d(0))
    if (ierr.eq.0) call ptx_pack_data(ierr, _WORK, d, wmask, kaxs, laxs, popts)

    if (ierr.eq.0) then
       call ptx_write_data &
            & (ierr,  &
            &  popts, wmask, nbase, _WORK, npack, &
            &  u,     krect, kfmt)
    endif

    if (ierr.eq.0) call alloc_work(ierr, -1, wtag, d(0))
    if (ierr.eq.0) call alloc_wmask(ierr, -1, wtag, mold)
#undef _WORK
  end subroutine ptx_write_array_f
  subroutine ptx_write_array_i &
       & (ierr, &
       &  d, n, u, krect, vmiss, kaxs, kfmt, kopts)
    use TOUZA_Trp,only: pack_store
    implicit none
    integer,parameter :: KARG=KI32
    integer,parameter :: KISRC=KI32
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(in)  :: d(0:*)
    integer,           intent(in)  :: n
    integer,           intent(in)  :: krect
    integer,           intent(in)  :: u
    real(kind=KRMIS),  intent(in)  :: vmiss
    integer,           intent(in)  :: kaxs(*)
    integer,           intent(in)  :: kfmt
    integer,optional,  intent(in)  :: kopts(:)

    integer,parameter :: mold = 0_KISRC
    character(len=*),parameter :: wtag = 'wptx:a'

    integer nbase, npack
    integer popts(lopts_ptx)

#define _WORK worki

    ierr = 0

    if (ierr.eq.0) call ptx_def_options(ierr, popts, kopts)
    if (ierr.eq.0) call ptx_set_shape(popts, kaxs, laxs)
    if (ierr.eq.0) nbase = ptx_row_size(popts, kaxs, laxs)
    if (ierr.eq.0) call alloc_wmask(ierr, nbase, wtag, mold)
    if (ierr.eq.0) call ptx_gen_ccvec(ierr, popts, wmask, nbase, d, kaxs, laxs, vmiss)
    if (ierr.eq.0) npack = popts(PROP_PTX_DATA)
    if (ierr.eq.0) call alloc_work(ierr, npack, wtag, d(0))
    if (ierr.eq.0) call ptx_pack_data(ierr, _WORK, d, wmask, kaxs, laxs, popts)

    if (ierr.eq.0) then
       call ptx_write_data &
            & (ierr,  &
            &  popts, wmask, nbase, _WORK, npack, &
            &  u,     krect, kfmt)
    endif

    if (ierr.eq.0) call alloc_work(ierr, -1, wtag, d(0))
    if (ierr.eq.0) call alloc_wmask(ierr, -1, wtag, mold)
#undef _WORK
  end subroutine ptx_write_array_i

!!!_  & ptx_gen_ccvec
  subroutine ptx_gen_ccvec_d &
       & (ierr, popts, ccvec, nbase, d, xmems, mx, vmiss)
    use TOUZA_Trp,only: count_packed
    use TOUZA_Trp,only: first_bit
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: popts(*)
    integer,         intent(out)   :: ccvec(0:*)
    real(kind=KARG), intent(in)    :: d(0:*)
    integer,         intent(in)    :: nbase
    integer,         intent(in)    :: xmems(*)
    integer,         intent(in)    :: mx
    real(kind=KRMIS),intent(in)    :: vmiss

    integer  jbase, jbgn,  jend
    integer  jint,  jout
    integer  nint,  ntgt,  nmem, nout, npack
    integer  j
    integer  jf, c
    real(kind=KARG) :: vu

    ierr = 0
    vu = real(vmiss, kind=KARG)

    call ptx_set_loops(nint, nmem, nout, xmems, mx, popts)
    ntgt = nmem * nint

    npack = 0
    if (ierr.eq.0) then
       do jbase = 0, nbase - 1
          jout = jbase / nint
          jint = mod(jbase, nint)
          jbgn = jout * ntgt + jint
          jend = jbgn + ntgt
          c = 0
          do j = nmem - 1, 0, -1
             jf = jbgn + nint * j
             if (d(jf).ne.vu) then
                c = j + 1
                exit
             endif
          enddo
          npack = npack + c
          ccvec(jbase) = c
       enddo
    endif
    popts(PROP_PTX_DATA) = npack
  end subroutine ptx_gen_ccvec_d
  subroutine ptx_gen_ccvec_f &
       & (ierr, popts, ccvec, nbase, d, xmems, mx, vmiss)
    use TOUZA_Trp,only: count_packed
    use TOUZA_Trp,only: first_bit
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: popts(*)
    integer,         intent(out)   :: ccvec(0:*)
    real(kind=KARG), intent(in)    :: d(0:*)
    integer,         intent(in)    :: nbase
    integer,         intent(in)    :: xmems(*)
    integer,         intent(in)    :: mx
    real(kind=KRMIS),intent(in)    :: vmiss

    integer  jbase, jbgn,  jend
    integer  jint,  jout
    integer  nint,  ntgt,  nmem, nout, npack
    integer  j
    integer  jf, c
    real(kind=KARG) :: vu

    ierr = 0
    vu = real(vmiss, kind=KARG)

    call ptx_set_loops(nint, nmem, nout, xmems, mx, popts)
    ntgt = nmem * nint

    npack = 0
    if (ierr.eq.0) then
       do jbase = 0, nbase - 1
          jout = jbase / nint
          jint = mod(jbase, nint)
          jbgn = jout * ntgt + jint
          jend = jbgn + ntgt
          c = 0
          do j = nmem - 1, 0, -1
             jf = jbgn + nint * j
             if (d(jf).ne.vu) then
                c = j + 1
                exit
             endif
          enddo
          npack = npack + c
          ccvec(jbase) = c
       enddo
    endif
    popts(PROP_PTX_DATA) = npack
  end subroutine ptx_gen_ccvec_f
  subroutine ptx_gen_ccvec_i &
       & (ierr, popts, ccvec, nbase, d, xmems, mx, vmiss)
    use TOUZA_Trp,only: count_packed
    use TOUZA_Trp,only: first_bit
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)   :: ierr
    integer,           intent(inout) :: popts(*)
    integer,           intent(out)   :: ccvec(0:*)
    integer(kind=KARG),intent(in)    :: d(0:*)
    integer,           intent(in)    :: nbase
    integer,           intent(in)    :: xmems(*)
    integer,           intent(in)    :: mx
    real(kind=KRMIS),  intent(in)    :: vmiss

    integer  jbase, jbgn,  jend
    integer  jint,  jout
    integer  nint,  ntgt,  nmem, nout, npack
    integer  j
    integer  jf, c
    real(kind=KARG) :: vu

    ierr = 0
    vu = real(vmiss, kind=KARG)

    call ptx_set_loops(nint, nmem, nout, xmems, mx, popts)
    ntgt = nmem * nint

    npack = 0
    if (ierr.eq.0) then
       do jbase = 0, nbase - 1
          jout = jbase / nint
          jint = mod(jbase, nint)
          jbgn = jout * ntgt + jint
          jend = jbgn + ntgt
          c = 0
          do j = nmem - 1, 0, -1
             jf = jbgn + nint * j
             if (d(jf).ne.vu) then
                c = j + 1
                exit
             endif
          enddo
          npack = npack + c
          ccvec(jbase) = c
       enddo
    endif
    popts(PROP_PTX_DATA) = npack
  end subroutine ptx_gen_ccvec_i

!!!_  & ptx_parse_array
  subroutine ptx_parse_array_d &
       & (ierr, popts, d, md, vmiss, colc, xmems, mx)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: popts(*)
    real(kind=KARG), intent(in)    :: d(0:*)
    integer,         intent(in)    :: md
    real(kind=KRMIS),intent(in)    :: vmiss
    integer,         intent(in)    :: colc
    integer,         intent(in)    :: xmems(*)
    integer,         intent(in)    :: mx

    integer  npack
    integer  jint,  jout, jmem
    integer  nint,  nout, nmem
    integer  j
    integer  c, nc
    integer  ci
    real(kind=KARG) :: vu

    ierr = 0
    vu = real(vmiss, kind=KARG)

    ci = colc
    if (ci.le.0) ci = mx
    nint = product(max(1, xmems(1:ci-1)))
    nmem = max(1, xmems(ci))
    if (ci.gt.mx) then
       nout = md / nint / nmem
    else
       nout = product(max(1, xmems(ci+1:mx)))
    endif
    npack = 0
    nc = 0
    do jout = 0, nout - 1
       do jint = 0, nint - 1
          c = -1
          do jmem = nmem - 1, 0, -1
             j = (jout * nmem + jmem) * nint + jint
             if (d(j).ne.vu) then
                c = jmem
                exit
             endif
          enddo
          c = c + 1
          nc = max(nc, c)
          npack = npack + nc
       enddo
    enddo
    popts(PROP_PTX_DATA) = npack
    popts(PROP_PTX_COLC) = ci
    popts(PROP_PTX_MCOL) = nc

  end subroutine ptx_parse_array_d
  subroutine ptx_parse_array_f &
       & (ierr, popts, d, md, vmiss, colc, xmems, mx)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: popts(*)
    real(kind=KARG),    intent(in)    :: d(0:*)
    integer,         intent(in)    :: md
    real(kind=KRMIS),intent(in)    :: vmiss
    integer,         intent(in)    :: colc
    integer,         intent(in)    :: xmems(*)
    integer,         intent(in)    :: mx

    integer  npack
    integer  jint,  jout, jmem
    integer  nint,  nout, nmem
    integer  j
    integer  c, nc
    integer  ci
    real(kind=KARG) :: vu

    ierr = 0
    vu = real(vmiss, kind=KARG)

    ci = colc
    if (ci.le.0) ci = mx
    nint = product(max(1, xmems(1:ci-1)))
    nmem = max(1, xmems(ci))
    if (ci.gt.mx) then
       nout = md / nint / nmem
    else
       nout = product(max(1, xmems(ci+1:mx)))
    endif
    npack = 0
    nc = 0
    do jout = 0, nout - 1
       do jint = 0, nint - 1
          c = -1
          do jmem = nmem - 1, 0, -1
             j = (jout * nmem + jmem) * nint + jint
             if (d(j).ne.vu) then
                c = jmem
                exit
             endif
          enddo
          c = c + 1
          nc = max(nc, c)
          npack = npack + nc
       enddo
    enddo
    popts(PROP_PTX_DATA) = npack
    popts(PROP_PTX_COLC) = ci
    popts(PROP_PTX_MCOL) = nc

  end subroutine ptx_parse_array_f
  subroutine ptx_parse_array_i &
       & (ierr, popts, d, md, vmiss, colc, xmems, mx)
    implicit none
    integer,parameter :: KARG=KI32
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: popts(*)
    integer(kind=KARG),    intent(in)    :: d(0:*)
    integer,         intent(in)    :: md
    real(kind=KRMIS),intent(in)    :: vmiss
    integer,         intent(in)    :: colc
    integer,         intent(in)    :: xmems(*)
    integer,         intent(in)    :: mx

    integer  npack
    integer  jint,  jout, jmem
    integer  nint,  nout, nmem
    integer  j
    integer  c, nc
    integer  ci
    integer(kind=KARG) :: vu

    ierr = 0
    vu = int(vmiss, kind=KARG)

    ci = colc
    if (ci.le.0) ci = mx
    nint = product(max(1, xmems(1:ci-1)))
    nmem = max(1, xmems(ci))
    if (ci.gt.mx) then
       nout = md / nint / nmem
    else
       nout = product(max(1, xmems(ci+1:mx)))
    endif
    npack = 0
    nc = 0
    do jout = 0, nout - 1
       do jint = 0, nint - 1
          c = -1
          do jmem = nmem - 1, 0, -1
             j = (jout * nmem + jmem) * nint + jint
             if (d(j).ne.vu) then
                c = jmem
                exit
             endif
          enddo
          c = c + 1
          nc = max(nc, c)
          npack = npack + nc
       enddo
    enddo
    popts(PROP_PTX_DATA) = npack
    popts(PROP_PTX_COLC) = ci
    popts(PROP_PTX_MCOL) = nc

  end subroutine ptx_parse_array_i

!!!_  & ptx_pack_data
  subroutine ptx_pack_data_d &
       & (ierr, dpack, d, ccvec, kaxs, lx, opts)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,        intent(out) :: ierr
    real(kind=KARG),intent(out) :: dpack(0:*)
    real(kind=KARG),intent(in)  :: d(0:*)
    integer,        intent(in)  :: ccvec(0:*)
    integer,        intent(in)  :: kaxs(*)
    integer,        intent(in)  :: lx
    integer,        intent(in)  :: opts(*)
    integer jx,     ji
    integer nx, nm, ni
    integer jb, je
    integer jp, np, jd

    ierr = 0
    call ptx_set_loops(ni, nm, nx, kaxs, lx, opts)

    jd = 0
    if (ierr.eq.0) then
       do jx = 0, nx - 1
          do ji = 0, ni - 1
             jp = jx * ni + ji
             np = ccvec(jp)
             if (np.gt.0) then
                jb = ji + ni * (0  + nm * jx)
                je = ji + ni * (np + nm * jx)
                dpack(jd:jd+np-1) = d(jb:je-1:ni)
                jd = jd + np
             endif
          enddo
       enddo
    endif
  end subroutine ptx_pack_data_d
  subroutine ptx_pack_data_f &
       & (ierr, dpack, d, ccvec, kaxs, lx, opts)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,        intent(out) :: ierr
    real(kind=KARG),intent(out) :: dpack(0:*)
    real(kind=KARG),intent(in)  :: d(0:*)
    integer,        intent(in)  :: ccvec(0:*)
    integer,        intent(in)  :: kaxs(*)
    integer,        intent(in)  :: lx
    integer,        intent(in)  :: opts(*)
    integer jx,     ji
    integer nx, nm, ni
    integer jb, je
    integer jp, np, jd

    ierr = 0
    call ptx_set_loops(ni, nm, nx, kaxs, lx, opts)

    jd = 0
    if (ierr.eq.0) then
       do jx = 0, nx - 1
          do ji = 0, ni - 1
             jp = jx * ni + ji
             np = ccvec(jp)
             if (np.gt.0) then
                jb = ji + ni * (0  + nm * jx)
                je = ji + ni * (np + nm * jx)
                dpack(jd:jd+np-1) = d(jb:je-1:ni)
                jd = jd + np
             endif
          enddo
       enddo
    endif
  end subroutine ptx_pack_data_f
  subroutine ptx_pack_data_i &
       & (ierr, dpack, d, ccvec, kaxs, lx, opts)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: dpack(0:*)
    integer,intent(in)  :: d(0:*)
    integer,intent(in)  :: ccvec(0:*)
    integer,intent(in)  :: kaxs(*)
    integer,intent(in)  :: lx
    integer,intent(in)  :: opts(*)
    integer jx,     ji
    integer nx, nm, ni
    integer jb, je
    integer jp, np, jd

    ierr = 0
    call ptx_set_loops(ni, nm, nx, kaxs, lx, opts)

    jd = 0
    if (ierr.eq.0) then
       do jx = 0, nx - 1
          do ji = 0, ni - 1
             jp = jx * ni + ji
             np = ccvec(jp)
             if (np.gt.0) then
                jb = ji + ni * (0  + nm * jx)
                je = ji + ni * (np + nm * jx)
                dpack(jd:jd+np-1) = d(jb:je-1:ni)
                jd = jd + np
             endif
          enddo
       enddo
    endif
  end subroutine ptx_pack_data_i

!!!_  & ptx_write_data - write PTx data core
  subroutine ptx_write_data_d &
       & (ierr,  &
       &  popts, ccvec, nbase, dpack, lpack, &
       &  u,     krect, kfmt)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,        intent(out)   :: ierr
    integer,        intent(inout) :: popts(:)
    integer,        intent(inout) :: ccvec(0:*)   ! column-count vector, to destroy
    integer,        intent(in)    :: nbase        ! size of ccvec (row)
    real(kind=KARG),intent(in)    :: dpack(0:*)
    integer,        intent(in)    :: lpack        ! limit size of dpack (defined)
    integer,        intent(in)    :: u
    integer,        intent(in)    :: krect
    integer,        intent(in)    :: kfmt

    integer npack

    ierr = 0

    call ptx_write_counts &
         & (ierr, popts, ccvec, nbase, u, krect)
    if (ierr.eq.0) then
       npack = popts(PROP_PTX_DATA)
       if (lpack.gt.0.and.npack.gt.lpack) then
          ierr = _ERROR(ERR_INVALID_ITEM)
       endif
    endif

    if (ierr.eq.0) then
       select case(kfmt)
       case(GFMT_PR8)
          call put_data_drecord(ierr, dpack, npack, u, krect, pre=.TRUE., post=.FALSE.)
       case(GFMT_PR4)
          call put_data_frecord(ierr, dpack, npack, u, krect, pre=.TRUE., post=.FALSE.)
       case(GFMT_PI4)
          call put_data_irecord(ierr, dpack, npack, u, krect, pre=.TRUE., post=.FALSE.)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    return
  end subroutine ptx_write_data_d
  subroutine ptx_write_data_f &
       & (ierr,  &
       &  popts, ccvec, nbase, dpack, lpack, &
       &  u,     krect, kfmt)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: popts(:)
    integer,         intent(inout) :: ccvec(0:*)   ! column-count vector, TO BE MODIFIED
    integer,         intent(in)    :: nbase        ! size of ccvec (row)
    real(kind=KARG), intent(in)    :: dpack(0:*)
    integer,         intent(in)    :: lpack        ! limit size of dpack (defined)
    integer,         intent(in)    :: u
    integer,         intent(in)    :: krect
    integer,         intent(in)    :: kfmt

    integer npack

    ierr = 0

    call ptx_write_counts &
         & (ierr, popts, ccvec, nbase, u, krect)
    if (ierr.eq.0) then
       npack = popts(PROP_PTX_DATA)
       if (lpack.gt.0.and.npack.gt.lpack) then
          ierr = _ERROR(ERR_INVALID_ITEM)
       endif
    endif

    if (ierr.eq.0) then
       select case(kfmt)
       case(GFMT_PR8)
          call put_data_drecord(ierr, dpack, npack, u, krect, pre=.TRUE., post=.FALSE.)
       case(GFMT_PR4)
          call put_data_frecord(ierr, dpack, npack, u, krect, pre=.TRUE., post=.FALSE.)
       case(GFMT_PI4)
          call put_data_irecord(ierr, dpack, npack, u, krect, pre=.TRUE., post=.FALSE.)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    return
  end subroutine ptx_write_data_f
  subroutine ptx_write_data_i &
       & (ierr,  &
       &  popts, ccvec, nbase, dpack, lpack, &
       &  u,     krect, kfmt)
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)   :: ierr
    integer,           intent(inout) :: popts(:)
    integer,           intent(inout) :: ccvec(0:*)   ! column-count vector, TO BE MODIFIED
    integer,           intent(in)    :: nbase        ! size of ccvec (row)
    integer(kind=KARG),intent(in)    :: dpack(0:*)
    integer,           intent(in)    :: lpack        ! limit size of dpack (defined)
    integer,           intent(in)    :: u
    integer,           intent(in)    :: krect
    integer,           intent(in)    :: kfmt

    integer npack

    ierr = 0

    call ptx_write_counts &
         & (ierr, popts, ccvec, nbase, u, krect)
    if (ierr.eq.0) then
       npack = popts(PROP_PTX_DATA)
       if (lpack.gt.0.and.npack.gt.lpack) then
          ierr = _ERROR(ERR_INVALID_ITEM)
       endif
    endif

    if (ierr.eq.0) then
       select case(kfmt)
       case(GFMT_PR8)
          call put_data_drecord(ierr, dpack, npack, u, krect, pre=.TRUE., post=.FALSE.)
       case(GFMT_PR4)
          call put_data_frecord(ierr, dpack, npack, u, krect, pre=.TRUE., post=.FALSE.)
       case(GFMT_PI4)
          call put_data_irecord(ierr, dpack, npack, u, krect, pre=.TRUE., post=.FALSE.)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    return
  end subroutine ptx_write_data_i

!!!_  & ptx_write_counts - write PTx option and count parts
  subroutine ptx_write_counts &
       & (ierr, popts, ccvec, nbase, &
       &  u,    krect)
    use TOUZA_Trp,only: count_packed, pack_store
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KISRC=KI32
    integer,intent(out)   :: ierr
    integer,intent(inout) :: popts(*)      ! to define packing parameters
    integer,intent(inout) :: ccvec(0:*)    ! to destroy
    integer,intent(in)    :: nbase         ! size of ccvec
    integer,intent(in)    :: u
    integer,intent(in)    :: krect

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer ncenc
    integer mbits, kpack
    character(len=*),parameter :: wtag = 'wptx:c'

    ierr = 0
    if (ierr.eq.0) then
       call ptx_encode_ccvec &
            & (ierr, popts, ccvec, nbase)
    endif
    if (ierr.eq.0) then
       mbits = popts(PROP_PTX_MBITS)
       kpack = popts(PROP_PTX_CODES)
       ncenc = count_packed(mbits, nbase, mold)
       if (ierr.eq.0) call alloc_wpack(ierr, ncenc, wtag, mold)
       call pack_store(ierr, wpack, ccvec, nbase, mbits, kpack)
    endif
    if (ierr.eq.0) then
       call put_data_record(ierr, popts, lopts_ptx, u, krect, pre=.FALSE., post=.TRUE.)
       ! write(*, *) popts(1:lopts_ptx)
    endif
    if (ierr.eq.0) then
       call put_data_record(ierr, wpack, ncenc, u, krect, pre=.TRUE.,  post=.TRUE.)
    endif

    if (ierr.eq.0) call alloc_wpack(ierr, -1, wtag, mold)
    return
  end subroutine ptx_write_counts

!!!_  & ptx_encode_ccvec
  subroutine ptx_encode_ccvec_set &
       & (ierr, popts, dadj, ccvec, nbase)
    implicit none
    integer,parameter :: KISRC=KI32
    integer,intent(out)   :: ierr
    integer,intent(inout) :: popts(*)
    integer,intent(out)   :: dadj(0:*)    ! intermediate encoded array to pack
    integer,intent(in)    :: ccvec(0:*)
    integer,intent(in)    :: nbase

    integer xl, xm

    ierr = 0
    call ptx_encode_ccvec_check &
         & (ierr, popts, ccvec, nbase)
    if (ierr.eq.0) then
       xl = popts(PROP_PTX_OFFSET)
       xm = popts(PROP_PTX_KEEP)
       where (ccvec(0:nbase-1).ge.xl)
          dadj(0:nbase-1) = xm + 1 + (ccvec(0:nbase-1) - xl)
       end where
    else
       dadj(0:nbase-1) = 0
    endif
  end subroutine ptx_encode_ccvec_set

!!!_  & ptx_encode_ccvec_mod
  subroutine ptx_encode_ccvec_mod &
       & (ierr, popts, ccvec, nbase)
    implicit none
    integer,parameter :: KISRC=KI32
    integer,intent(out)   :: ierr
    integer,intent(inout) :: popts(*)
    integer,intent(inout) :: ccvec(0:*)
    integer,intent(in)    :: nbase

    integer  xl, xm

    call ptx_encode_ccvec_check &
         & (ierr, popts, ccvec, nbase)
    if (ierr.eq.0) then
       xl = popts(PROP_PTX_OFFSET)
       xm = popts(PROP_PTX_KEEP)
       where (ccvec(0:nbase-1).ge.xl)
          ccvec(0:nbase-1) = xm + 1 + (ccvec(0:nbase-1) - xl)
       end where
       ! ccvec(0:nbase-1) = max(0, ccvec(0:nbase-1) - xl)
    endif
  end subroutine ptx_encode_ccvec_mod

!!!_  & ptx_encode_ccvec_check
  subroutine ptx_encode_ccvec_check &
       & (ierr, popts, ccvec, nbase)
    use TOUZA_Trp,only: first_bit
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: popts(*)       ! need PROP_PTX_KEEP
    integer,intent(in)    :: ccvec(0:*)
    integer,intent(in)    :: nbase

    integer  xh, xl, xm
    integer  jbase
    integer  mbits, ndata

    ierr = 0

    xh = 0
    xl = HUGE(0)
    xm = POPTS(PROP_PTX_KEEP)
    ndata = 0
    if (ierr.eq.0) then
       do jbase = 0, nbase - 1
          ndata = ndata + ccvec(jbase)
          xh = max(xh, ccvec(jbase))
          if (ccvec(jbase).gt.xm) xl = min(xl, ccvec(jbase))
       enddo
    endif
    if (ierr.eq.0) then
       mbits = max(1, popts(PROP_PTX_MBITS))
       if (xl.gt.xh) then
          ! all data <= xm
          xh = (first_bit(xh) / mbits + 1) * mbits
       else
          xh = (first_bit(xh - xl + 1 + xm) / mbits + 1) * mbits
       endif
       popts(PROP_PTX_MBITS) = xh
       popts(PROP_PTX_OFFSET) = xl
       popts(PROP_PTX_DATA) = ndata
    else
       popts(PROP_PTX_MBITS) = -1
       popts(PROP_PTX_OFFSET) = -1
       popts(PROP_PTX_DATA) = -1
    endif
  end subroutine ptx_encode_ccvec_check

!!!_  & ptx_read_array - read PTx data
  subroutine ptx_read_array_d &
       & (ierr, &
       &  d,    ldata, &
       &  u,    krect, vmiss, kaxs,  kfmt, bes, nr)
    use TOUZA_Trp,only: pack_restore
    implicit none
    integer,parameter :: KARG=KDBL
    integer,parameter :: KISRC=KI32
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: ldata        ! limit size of dpack (defined)
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kaxs(*)
    integer,         intent(in)  :: kfmt
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    character(len=*),parameter :: wtag = 'rptx:a'
    integer nbase, npack
    integer popts(lopts_ptx)
    integer flag
    integer kaxsp(laxs)

#define _WORK workd
    ierr = 0
    flag = rev_pos_leave
    call ptx_review &
         & (ierr,  popts, &
         &  u,     krect, flag)
    if (ierr.eq.0) nbase = ptx_row_size(popts, kaxs, laxs)
    if (ierr.eq.0) npack = popts(PROP_PTX_DATA)
    if (ierr.eq.0) call alloc_wmask(ierr, nbase, wtag, mold)
    if (ierr.eq.0) call alloc_work(ierr, npack, wtag, d(0))
    if (ierr.eq.0) then
       call ptx_read_data &
            & (ierr,  &
            &  wmask, nbase, _WORK, npack, &
            &  popts, u,     krect, kfmt,  bes, nr)
    endif

    if (present(bes)) then
       if (ierr.eq.0) then
          if (.not.present(nr)) ierr = _ERROR(ERR_INVALID_ITEM)
       endif
       if (ierr.eq.0) then
          kaxsp(1:laxs) = bes(2, 1:laxs) - bes(1, 1:laxs)
          call ptx_expand_data &
               & (ierr, d, _WORK, wmask, vmiss, kaxsp, laxs, popts)
       endif
    else
       ! write(*, *) wmask(0:nbase-1)
       ! write(*, *) _WORK(0:npack-1)
       if (ierr.eq.0) then
          call ptx_expand_data &
               & (ierr, d, _WORK, wmask, vmiss, kaxs, laxs, popts)
       endif
    endif

    if (ierr.eq.0) call alloc_wmask(ierr, -1, wtag, mold)
    if (ierr.eq.0) call alloc_work(ierr, -1, wtag, d(0))
#undef _WORK
  end subroutine ptx_read_array_d
  subroutine ptx_read_array_f &
       & (ierr, &
       &  d,    ldata, &
       &  u,    krect, vmiss, kaxs,  kfmt, bes, nr)
    use TOUZA_Trp,only: pack_restore
    implicit none
    integer,parameter :: KARG=KFLT
    integer,parameter :: KISRC=KI32
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: ldata        ! limit size of dpack (defined)
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: kaxs(*)
    integer,         intent(in)  :: kfmt
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    character(len=*),parameter :: wtag = 'rptx:a'
    integer nbase, npack
    integer popts(lopts_ptx)
    integer flag
    integer kaxsp(laxs)

#define _WORK workf
    ierr = 0
    flag = rev_pos_leave
    call ptx_review &
         & (ierr,  popts, &
         &  u,     krect, flag)
    if (ierr.eq.0) nbase = ptx_row_size(popts, kaxs, laxs)
    if (ierr.eq.0) npack = popts(PROP_PTX_DATA)
    if (ierr.eq.0) call alloc_wmask(ierr, nbase, wtag, mold)
    if (ierr.eq.0) call alloc_work(ierr, npack, wtag, d(0))
    if (ierr.eq.0) then
       call ptx_read_data &
            & (ierr,  &
            &  wmask, nbase, _WORK, npack, &
            &  popts, u,     krect, kfmt,  bes, nr)
    endif

    ! write(*, *) 'bes:', present(bes)
    if (present(bes)) then
       if (ierr.eq.0) then
          if (.not.present(nr)) ierr = _ERROR(ERR_INVALID_ITEM)
       endif
       if (ierr.eq.0) then
          kaxsp(1:laxs) = bes(2, 1:laxs) - bes(1, 1:laxs)
          call ptx_expand_data &
               & (ierr, d, _WORK, wmask, vmiss, kaxsp, laxs, popts)
       endif
    else
       ! write(*, *) 'full'
       if (ierr.eq.0) then
          call ptx_expand_data &
               & (ierr, d, _WORK, wmask, vmiss, kaxs, laxs, popts)
       endif
    endif

    if (ierr.eq.0) call alloc_wmask(ierr, -1, wtag, mold)
    if (ierr.eq.0) call alloc_work(ierr, -1, wtag, d(0))
#undef _WORK
  end subroutine ptx_read_array_f
  subroutine ptx_read_array_i &
       & (ierr, &
       &  d,    ldata, &
       &  u,    krect, vmiss, kaxs,  kfmt, bes, nr)
    use TOUZA_Trp,only: pack_restore
    implicit none
    integer,parameter :: KARG=KI32
    integer,parameter :: KISRC=KI32
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(out) :: d(0:*)
    integer,           intent(in)  :: ldata        ! limit size of dpack (defined)
    integer,           intent(in)  :: u
    integer,           intent(in)  :: krect
    real(kind=KRMIS),  intent(in)  :: vmiss
    integer,           intent(in)  :: kaxs(*)
    integer,           intent(in)  :: kfmt
    integer,optional,  intent(in)  :: bes(3, *)
    integer,optional,  intent(in)  :: nr

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    character(len=*),parameter :: wtag = 'rptx:a'
    integer nbase, npack
    integer popts(lopts_ptx)
    integer flag
    integer kaxsp(laxs)

#define _WORK worki
    ierr = 0
    flag = rev_pos_leave
    call ptx_review &
         & (ierr,  popts, &
         &  u,     krect, flag)
    if (ierr.eq.0) nbase = ptx_row_size(popts, kaxs, laxs)
    if (ierr.eq.0) npack = popts(PROP_PTX_DATA)
    if (ierr.eq.0) call alloc_wmask(ierr, nbase, wtag, mold)
    if (ierr.eq.0) call alloc_work(ierr, npack, wtag, d(0))
    if (ierr.eq.0) then
       call ptx_read_data &
            & (ierr,  &
            &  wmask, nbase, _WORK, npack, &
            &  popts, u,     krect, kfmt,  bes, nr)
    endif

    if (present(bes)) then
       if (ierr.eq.0) then
          if (.not.present(nr)) ierr = _ERROR(ERR_INVALID_ITEM)
       endif
       if (ierr.eq.0) then
          kaxsp(1:laxs) = bes(2, 1:laxs) - bes(1, 1:laxs)
          call ptx_expand_data &
               & (ierr, d, _WORK, wmask, vmiss, kaxsp, laxs, popts)
       endif
    else
       if (ierr.eq.0) then
          call ptx_expand_data &
               & (ierr, d, _WORK, wmask, vmiss, kaxs, laxs, popts)
       endif
    endif

    if (ierr.eq.0) call alloc_wmask(ierr, -1, wtag, mold)
    if (ierr.eq.0) call alloc_work(ierr, -1, wtag, d(0))
#undef _WORK
  end subroutine ptx_read_array_i

!!!_  & ptx_review - review PTx properties
  subroutine ptx_review &
       & (ierr,  popts, &
       &  u,     krect, flag)
    use TOUZA_Nio_std,only: KIOFS, sus_getpos, sus_rseek, WHENCE_ABS
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KISRC=KI32
    integer,         intent(out) :: ierr
    integer,         intent(out) :: popts(*)
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    integer,optional,intent(in)  :: flag

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    logical cont
    integer f
    integer(kind=KIOFS) :: apini
    ! integer ni, nm, nx, mo

    ierr = 0
    ! note: position must be just after gtool header part
    f = choice(0, flag)
    if (ierr.eq.0) call pre_review(ierr, apini, u, f)
    cont = .TRUE.
    if (ierr.eq.0) call get_data_record(ierr, popts, lopts_ptx, u, krect, sub=cont)
    ! if (ierr.eq.0) then
    !    npack = opts(PROP_PTX_DATA)
    !    call ptx_set_loops(ni, nm, nx, kaxs, laxs, opts)
    !    nbase = ni * nx
    ! endif
    ! if (ierr.eq.0) then
    !    mo = min(size(kopts), lopts_ptx)
    !    kopts(1:mo) = opts(1:mo)
    ! endif
    if (ierr.eq.0) call post_review(ierr, apini, u, f)
  end subroutine ptx_review

!!!_  & ptx_read_data - read PTx data core
  subroutine ptx_read_data_d &
       & (ierr,  &
       &  ccvec, nbase, dpack, npack, &
       &  popts, u,     krect, kfmt,  bes, nr)
    use TOUZA_Trp,only: pack_restore
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ccvec(0:*)   ! column-count vector
    integer,         intent(in)  :: nbase        ! size of ccvec (row)
    real(kind=KARG), intent(out) :: dpack(0:*)
    integer,         intent(in)  :: npack
    integer,         intent(in)  :: popts(*)
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: kfmt
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr

    integer nrl,   npeff
    logical cont
    character(len=*),parameter :: wtag = 'rptx:d'
    integer j

    ierr = 0
    cont = .TRUE.
    call ptx_read_counts &
         & (ierr, ccvec, nbase, popts, u, krect)
    ! write(*, *) 'ccvec', ccvec(0:nbase-1)
    if (present(bes)) then
       if (ierr.eq.0) then
          if (.not.present(nr)) ierr = _ERROR(ERR_INVALID_ITEM)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, nbase * 2 + 1, wtag)
       if (ierr.eq.0) then
          wdsubv(0) = 0
          do j = 1, nbase
             wdsubv(j) = wdsubv(j-1) + ccvec(j-1)
          enddo
          call ptx_set_slice &
               & (ierr, wssubv, nrl, npeff, ccvec, wdsubv, nbase, &
               &  bes,  nr,     popts)
       endif
       if (ierr.eq.0) then
          select case(kfmt)
          case(GFMT_PR8)
             call get_data_drecord_runl(ierr, dpack, npeff, u, krect, wssubv, nrl, sub=cont)
          case(GFMT_PR4)
             call get_data_frecord_runl(ierr, dpack, npeff, u, krect, wssubv, nrl, sub=cont)
          case(GFMT_PI4)
             call get_data_irecord_runl(ierr, dpack, npeff, u, krect, wssubv, nrl, sub=cont)
          case default
             ierr = _ERROR(ERR_INVALID_SWITCH)
          end select
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, wtag)
    else
       if (ierr.eq.0) then
          select case(kfmt)
          case(GFMT_PR8)
             call get_data_drecord(ierr, dpack, npack, u, krect, sub=cont)
          case(GFMT_PR4)
             call get_data_frecord(ierr, dpack, npack, u, krect, sub=cont)
          case(GFMT_PI4)
             call get_data_irecord(ierr, dpack, npack, u, krect, sub=cont)
          case default
             ierr = _ERROR(ERR_INVALID_SWITCH)
          end select
       endif
    endif

  end subroutine ptx_read_data_d
  subroutine ptx_read_data_f &
       & (ierr,  &
       &  ccvec, nbase, dpack, npack, &
       &  popts, u,     krect, kfmt,  bes, nr)
    use TOUZA_Trp,only: pack_restore
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ccvec(0:*)   ! column-count vector
    integer,         intent(in)  :: nbase        ! size of ccvec (row)
    real(kind=KARG), intent(out) :: dpack(0:*)
    integer,         intent(in)  :: npack
    integer,         intent(in)  :: popts(*)
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: kfmt
    integer,optional,intent(in)  :: bes(3, *)
    integer,optional,intent(in)  :: nr

    integer nrl,   npeff
    logical cont
    character(len=*),parameter :: wtag = 'rptx:d'
    integer j

    ierr = 0
    cont = .TRUE.
    call ptx_read_counts &
         & (ierr, ccvec, nbase, popts, u, krect)
    if (present(bes)) then
       if (ierr.eq.0) then
          if (.not.present(nr)) ierr = _ERROR(ERR_INVALID_ITEM)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, nbase * 2 + 1, wtag)
       if (ierr.eq.0) then
          wdsubv(0) = 0
          do j = 1, nbase
             wdsubv(j) = wdsubv(j-1) + ccvec(j-1)
          enddo
          call ptx_set_slice &
               & (ierr, wssubv, nrl, npeff, ccvec, wdsubv, nbase, &
               &  bes,  nr,     popts)
       endif
       if (ierr.eq.0) then
          select case(kfmt)
          case(GFMT_PR8)
             call get_data_drecord_runl(ierr, dpack, npeff, u, krect, wssubv, nrl, sub=cont)
          case(GFMT_PR4)
             call get_data_frecord_runl(ierr, dpack, npeff, u, krect, wssubv, nrl, sub=cont)
          case(GFMT_PI4)
             call get_data_irecord_runl(ierr, dpack, npeff, u, krect, wssubv, nrl, sub=cont)
          case default
             ierr = _ERROR(ERR_INVALID_SWITCH)
          end select
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, wtag)
    else
       if (ierr.eq.0) then
          select case(kfmt)
          case(GFMT_PR8)
             call get_data_drecord(ierr, dpack, npack, u, krect, sub=cont)
          case(GFMT_PR4)
             call get_data_frecord(ierr, dpack, npack, u, krect, sub=cont)
          case(GFMT_PI4)
             call get_data_irecord(ierr, dpack, npack, u, krect, sub=cont)
          case default
             ierr = _ERROR(ERR_INVALID_SWITCH)
          end select
       endif
    endif

  end subroutine ptx_read_data_f
  subroutine ptx_read_data_i &
       & (ierr,  &
       &  ccvec, nbase, dpack, npack, &
       &  popts, u,     krect, kfmt,  bes, nr)
    use TOUZA_Trp,only: pack_restore
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out) :: ierr
    integer,           intent(out) :: ccvec(0:*)   ! column-count vector
    integer,           intent(in)  :: nbase        ! size of ccvec (row)
    integer(kind=KARG),intent(out) :: dpack(0:*)
    integer,           intent(in)  :: npack
    integer,           intent(in)  :: popts(*)
    integer,           intent(in)  :: u
    integer,           intent(in)  :: krect
    integer,           intent(in)  :: kfmt
    integer,optional,  intent(in)  :: bes(3, *)
    integer,optional,  intent(in)  :: nr

    integer nrl,   npeff
    logical cont
    character(len=*),parameter :: wtag = 'rptx:d'
    integer j

    ierr = 0
    cont = .TRUE.
    call ptx_read_counts &
         & (ierr, ccvec, nbase, popts, u, krect)
    if (present(bes)) then
       if (ierr.eq.0) then
          if (.not.present(nr)) ierr = _ERROR(ERR_INVALID_ITEM)
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, nbase * 2 + 1, wtag)
       if (ierr.eq.0) then
          wdsubv(0) = 0
          do j = 1, nbase
             wdsubv(j) = wdsubv(j-1) + ccvec(j-1)
          enddo
          call ptx_set_slice &
               & (ierr, wssubv, nrl, npeff, ccvec, wdsubv, nbase, &
               &  bes,  nr,     popts)
       endif
       if (ierr.eq.0) then
          select case(kfmt)
          case(GFMT_PR8)
             call get_data_drecord_runl(ierr, dpack, npeff, u, krect, wssubv, nrl, sub=cont)
          case(GFMT_PR4)
             call get_data_frecord_runl(ierr, dpack, npeff, u, krect, wssubv, nrl, sub=cont)
          case(GFMT_PI4)
             call get_data_irecord_runl(ierr, dpack, npeff, u, krect, wssubv, nrl, sub=cont)
          case default
             ierr = _ERROR(ERR_INVALID_SWITCH)
          end select
       endif
       if (ierr.eq.0) call alloc_wsubv(ierr, -1, wtag)
    else
       if (ierr.eq.0) then
          select case(kfmt)
          case(GFMT_PR8)
             call get_data_drecord(ierr, dpack, npack, u, krect, sub=cont)
          case(GFMT_PR4)
             call get_data_frecord(ierr, dpack, npack, u, krect, sub=cont)
          case(GFMT_PI4)
             call get_data_irecord(ierr, dpack, npack, u, krect, sub=cont)
          case default
             ierr = _ERROR(ERR_INVALID_SWITCH)
          end select
       endif
    endif

  end subroutine ptx_read_data_i

!!!_  & ptx_read_counts - read PTx count parts
  subroutine ptx_read_counts &
       & (ierr, ccvec, nbase, popts, &
       &  u,    krect)
    use TOUZA_Trp,only: count_packed, pack_restore
    implicit none
    integer,parameter :: KISRC=KI32
    integer,intent(out) :: ierr
    integer,intent(out) :: ccvec(0:*)
    integer,intent(in)  :: nbase
    integer,intent(in)  :: popts(*)
    integer,intent(in)  :: u
    integer,intent(in)  :: krect

    integer(kind=KISRC),parameter :: mold = 0_KISRC

    integer xm, xl
    integer ncenc
    integer mbits, kpack
    logical cont

    character(len=*),parameter :: wtag = 'rptx:d'

    cont = .TRUE.
    ierr = 0

    if (ierr.eq.0) then
       mbits = popts(PROP_PTX_MBITS)
       ncenc = count_packed(mbits, nbase, mold)
       kpack = popts(PROP_PTX_CODES)
    endif
    if (ierr.eq.0) call alloc_wpack(ierr, ncenc, wtag, mold)
    if (ierr.eq.0) call get_data_record(ierr, wpack, ncenc, u, krect, sub=cont)
    if (ierr.eq.0) call pack_restore(ierr, ccvec, wpack, nbase, mbits, kpack)

    if (ierr.eq.0) then
       xm = popts(PROP_PTX_KEEP)
       xl = popts(PROP_PTX_OFFSET)
       where (ccvec(0:nbase-1).gt.xm)
          ccvec(0:nbase-1) = (ccvec(0:nbase-1) - xm) + xl - 1
       end where
    endif

    if (ierr.eq.0) call alloc_wpack(ierr, -1, wtag, mold)

    return
  end subroutine ptx_read_counts

!!!_  & ptx_set_slice
  subroutine ptx_set_slice &
       & (ierr, runl, nrl, npeff, ccsli, ofs, nbase, &
       &  bes,  nr,   opts)
    use TOUZA_Nio_std,only: set_slice_loop, init_offset, next_offset
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(out)   :: nrl
    integer,intent(out)   :: runl(0:*)
    integer,intent(out)   :: npeff        ! active number on the slice
    integer,intent(out)   :: ccsli(0:*)   ! column count vector for slice
    integer,intent(in)    :: ofs(0:*)
    integer,intent(in)    :: nbase
    integer,intent(in)    :: bes(3, 1:*)
    integer,intent(in)    :: nr
    integer,intent(in)    :: opts(*)
    integer mbes(3, 1:nr)
    integer mbgn, mend, mtgt
    integer ml,   mh,   meff
    integer stp(1:nr), itr(1:nr), idx(1:nr)
    integer rr
    integer jsrc, jprv
    integer m
    integer cmem
    integer ncs

    ierr = 0
    npeff = 0

    if (opts(PROP_PTX_CODES).ge.0) then
       cmem = nr
    else
       cmem = 1
    endif

    if (ierr.eq.0) then
       mbes(:, 1:nr)  = bes(:, 1:nr)
       mbes(:, cmem) = (/0, 1, 1/)

       ! write(*, *) 'mbes', cmem
       ! write(*, *) mbes(:, 1:nr)

       mtgt = bes(3, cmem)
       mbgn = bes(1, cmem)
       mend = bes(2, cmem)

       call set_slice_loop(rr, stp, itr, mbes, nr)

       idx(1:rr) = 0
       jsrc = init_offset(mbes, nr)
       m = 1
       nrl = 0
       ncs = 0
       jprv = 0
       ! runl(nrl) = ofs(jsrc)  ! initial skip
       runl(nrl) = 0  ! initial skip
       do
          ! write(*, *) 'src', jsrc
          if (jsrc.lt.0) exit
          meff = ofs(jsrc + 1) - ofs(jsrc)
          ml = min(meff, mbgn)
          mh = min(meff, mend)
          ! write(*, *) 'mlh', jsrc, meff, mbgn, mend, ml, mh, ofs(jsrc) - jprv
          ! leftover
          call ptx_set_runl(runl, nrl, ofs(jsrc) - jprv, 0)
          ! to skip
          call ptx_set_runl(runl, nrl, ml, 0)
          ! to read
          call ptx_set_runl(runl, nrl, mh - ml, 1)
          ! to skip
          call ptx_set_runl(runl, nrl, meff - mh, 0)

          ! write(*, *) 'runl', nrl, runl(0:nrl)

          jprv = ofs(jsrc + 1)
          ccsli(ncs) = mh - ml
          npeff = npeff + ccsli(ncs)
          ncs = ncs + 1
          call next_offset(jsrc, idx, stp, itr, rr, m)
       enddo
       jsrc = nbase
       call ptx_set_runl(runl, nrl, ofs(jsrc) - jprv, 0)
       if (mod(nrl, 2).eq.1) then
          nrl = nrl + 1
          runl(nrl) = 0
       endif
    endif

  end subroutine ptx_set_slice
!!!_  & ptx_set_runl
  subroutine ptx_set_runl &
       & (runl, nrl, nadd, mode)
    implicit none
    integer,intent(inout) :: runl(0:*)
    integer,intent(inout) :: nrl
    integer,intent(in)    :: nadd
    integer,intent(in)    :: mode

    if (nadd.gt.0) then
       if (mod(nrl, 2).eq.mode) then
          runl(nrl) = runl(nrl) + nadd
       else
          nrl = nrl + 1
          runl(nrl) = nadd
       endif
    endif
  end subroutine ptx_set_runl


!!!_  & ptx_expand_data
  subroutine ptx_expand_data_d &
       & (ierr, d, dpack, ccvec, vmiss, xmems, mx, popts)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    real(kind=KARG), intent(in)  :: dpack(0:*)
    integer,         intent(in)  :: ccvec(0:*)
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: xmems(*)
    integer,         intent(in)  :: mx
    integer,         intent(in)  :: popts(:)

    integer jo,     ji
    integer no, nm, ni
    integer jb, je
    integer jp, np, jd

    ierr = 0
    call ptx_set_loops(ni, nm, no, xmems, mx, popts)
    ! write(*, *) 'popts', popts
    ! write(*, *) 'mems', mx, xmems(1:mx)
    ! write(*, *) 'loops', ni, nm, no
    if (ierr.eq.0) then
       jd = 0
       do jo = 0, no - 1
          do ji = 0, ni - 1
             jp = jo * ni + ji
             np = ccvec(jp)
             jb = ji + ni * (0  + nm * jo)
             je = ji + ni * (np + nm * jo)
             ! write(*, *) 'expand:v', jd, ji, jo, jb, je
             d(jb:je-1:ni) = real(dpack(jd:jd+np-1), kind=KARG)
             jb = je
             je = ji + ni * (nm + nm * jo)
             ! write(*, *) 'expand:u', jd, ji, jo, jb, je
             d(jb:je-1:ni) = real(vmiss, kind=KARG)
             jd = jd + np
          enddo
       enddo
    endif
  end subroutine ptx_expand_data_d
  subroutine ptx_expand_data_f &
       & (ierr, d, dpack, ccvec, vmiss, xmems, mx, popts)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    real(kind=KARG), intent(in)  :: dpack(0:*)
    integer,         intent(in)  :: ccvec(0:*)
    real(kind=KRMIS),intent(in)  :: vmiss
    integer,         intent(in)  :: xmems(*)
    integer,         intent(in)  :: mx
    integer,         intent(in)  :: popts(:)

    integer jo,     ji
    integer no, nm, ni
    integer jb, je
    integer jp, np, jd

    ierr = 0
    call ptx_set_loops(ni, nm, no, xmems, mx, popts)
    ! write(*, *) 'popts', popts
    ! write(*, *) 'mems', mx, xmems(1:mx)
    ! write(*, *) 'loops', ni, nm, no
    if (ierr.eq.0) then
       jd = 0
       do jo = 0, no - 1
          do ji = 0, ni - 1
             jp = jo * ni + ji
             np = ccvec(jp)
             jb = ji + ni * (0  + nm * jo)
             je = ji + ni * (np + nm * jo)
             ! write(*, *) 'expand:v', ji, jo, jb, je
             d(jb:je-1:ni) = real(dpack(jd:jd+np-1), kind=KARG)
             jb = je
             je = ji + ni * (nm + nm * jo)
             ! write(*, *) 'expand:u', ji, jo, jb, je
             d(jb:je-1:ni) = real(vmiss, kind=KARG)
             jd = jd + np
          enddo
       enddo
    endif
  end subroutine ptx_expand_data_f
  subroutine ptx_expand_data_i &
       & (ierr, d, dpack, ccvec, vmiss, xmems, mx, popts)
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out) :: ierr
    integer(kind=KARG),intent(out) :: d(0:*)
    integer(kind=KARG),intent(in)  :: dpack(0:*)
    integer,           intent(in)  :: ccvec(0:*)
    real(kind=KRMIS),  intent(in)  :: vmiss
    integer,           intent(in)  :: xmems(*)
    integer,           intent(in)  :: mx
    integer,           intent(in)  :: popts(:)

    integer jo,     ji
    integer no, nm, ni
    integer jb, je
    integer jp, np, jd

    ierr = 0
    call ptx_set_loops(ni, nm, no, xmems, mx, popts)
    if (ierr.eq.0) then
       jd = 0
       do jo = 0, no - 1
          do ji = 0, ni - 1
             jp = jo * ni + ji
             np = ccvec(jp)
             jb = ji + ni * (0  + nm * jo)
             je = ji + ni * (np + nm * jo)
             d(jb:je-1:ni) = int(dpack(jd:jd+np-1), kind=KARG)
             jb = je
             je = ji + ni * (nm + nm * jo)
             d(jb:je-1:ni) = int(vmiss, kind=KARG)
             jd = jd + np
          enddo
       enddo
    endif
  end subroutine ptx_expand_data_i

!!!_  & ptx_def_options
  subroutine ptx_def_options(ierr, popts, uopts)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: popts(*)
    integer,intent(in),optional :: uopts(:)
    integer nu

    ierr = 0
    if (present(uopts)) then
       nu = size(uopts)
    else
       nu = -1
    endif
    call set_ptx_option(popts, PROP_PTX_VAR,     PTX_VERSION,    nu, uopts)
    call set_ptx_option(popts, PROP_PTX_COLC,    -1,   nu, uopts)
    call set_ptx_option(popts, PROP_PTX_MCOL,    -1,   nu, uopts)
    call set_ptx_option(popts, PROP_PTX_CODES,   def_encode_trapiche, nu, uopts)
    call set_ptx_option(popts, PROP_PTX_MBITS,   0,  nu, uopts)
    call set_ptx_option(popts, PROP_PTX_KEEP,    1,  nu, uopts)

    ! to modify
    popts(PROP_PTX_OFFSET) = -1
    popts(PROP_PTX_DATA)   = -1

    return
  end subroutine ptx_def_options
!!!_  & ptx_parse_options
  subroutine ptx_parse_options(ierr, nbase, popts, kaxs, lx, uopts)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: nbase
    integer,intent(out)         :: popts(*)
    integer,intent(in)          :: kaxs(*)
    integer,intent(in)          :: lx
    integer,intent(in),optional :: uopts(:)

    ierr = 0
    call ptx_def_options(ierr, popts, uopts)
    if (ierr.eq.0) then
       nbase = ptx_vec_size(kaxs, lx, popts)
    else
       nbase = -1
    endif
    return
  end subroutine ptx_parse_options
!!!_   . ptx_vec_size ()
  integer function ptx_vec_size(kaxs, lx, opts) result(n)
    implicit none
    integer,intent(in) :: kaxs(*)
    integer,intent(in) :: lx
    integer,intent(in) :: opts(*)
    integer ni, nm, nx
    call ptx_set_loops(ni, nm, nx, kaxs, lx, opts)
    n = ni * nx
  end function ptx_vec_size
!!!_   . set_ptx_option
  subroutine set_ptx_option (opts, kp, def, nu, uopts)
    implicit none
    integer,intent(inout)       :: opts(*)
    integer,intent(in)          :: kp
    integer,intent(in)          :: def
    integer,intent(in)          :: nu
    integer,intent(in),optional :: uopts(:)
    if (nu.ge.kp) then
       opts(kp) = uopts(kp)
    else
       opts(kp) = def
    endif
  end subroutine set_ptx_option

!!!_  & ptx_set_loops
  subroutine ptx_set_loops &
       & (ni, nm, no, xmems, mx, popts)
    implicit none
    integer,intent(out) :: ni, nm, no
    integer,intent(in)  :: xmems(*)
    integer,intent(in)  :: mx
    integer,intent(in)  :: popts(*)
    integer ci
    no = product(max(1, xmems(1:mx)))
    nm = popts(PROP_PTX_MCOL)
    ci = popts(PROP_PTX_COLC)
    if (ci.lt.0) ci = mx
    ! write(*, *) 'set', ci, no, nm
    if (ci.le.0) then
       ni = 1
    else if (ci.gt.mx) then
       ni = no
       no = 1
    else
       ! nm = max(nm, xmems(ci))
       nm = min(nm, xmems(ci))
       ni = product(max(1, xmems(1:ci-1)))
       no = product(max(1, xmems(ci+1:mx)))
    endif
  end subroutine ptx_set_loops

!!!_   . ptx_row_size ()
  integer function ptx_row_size(popts, xmems, mx) result(n)
    implicit none
    integer,intent(in) :: popts(*)
    integer,intent(in) :: xmems(*)
    integer,intent(in) :: mx
    integer ci

    n = product(max(1, xmems(1:mx)))
    ci = popts(PROP_PTX_COLC)
    if (ci.lt.0) ci = mx
    if (ci.gt.0.and.ci.le.mx) n = n / max(1, xmems(ci))

  end function ptx_row_size

!!!_  - ptx_get_shape
  subroutine ptx_get_shape &
       & (nrow, mcol, popts, xmems, mx)
    implicit none
    integer,intent(out) :: nrow  ! csr matrix number of rows
    integer,intent(out) :: mcol  ! csr matrix maximum columns
    integer,intent(in)  :: popts(*)
    integer,intent(in)  :: xmems(*)    ! coordinate sizes
    integer,intent(in)  :: mx          ! size of xmems

    integer ci

    nrow = ptx_row_size(popts, xmems, mx)
    ci = popts(PROP_PTX_COLC)
    if (ci.lt.0) ci = mx
    if (ci.gt.0.and.ci.le.mx) then
       mcol = max(1, xmems(ci))
    else
       mcol = popts(PROP_PTX_MCOL)
    endif
    return
  end subroutine ptx_get_shape

!!!_  - ptx_set_shape
  subroutine ptx_set_shape &
       & (popts, xmems, mx, ncols)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,intent(inout)       :: popts(*)
    integer,intent(in)          :: xmems(*)    ! coordinate sizes
    integer,intent(in)          :: mx          ! size of xmems
    integer,intent(in),optional :: ncols       ! to override

    integer ci, mc

    ci = popts(PROP_PTX_COLC)
    if (ci.lt.0) ci = mx
    popts(PROP_PTX_COLC) = ci

    mc = popts(PROP_PTX_MCOL)
    mc = max(mc, choice(mc, ncols))

    if (ci.le.0.or.ci.gt.mx) then
       popts(PROP_PTX_MCOL) = max(1, mc)
    else
       popts(PROP_PTX_MCOL) = max(xmems(ci), mc)
    endif
    return
  end subroutine ptx_set_shape

!!!_ + [PTx] obsolete implementation
!!!_  & ptx_check_vec
  subroutine ptx_check_vec_d &
       & (ierr, npack, opts, xmem, d, nbase, kaxs, lx, vmiss)
    use TOUZA_Trp,only: count_packed
    use TOUZA_Trp,only: first_bit
    implicit none
    integer,parameter :: KARG=KDBL
    integer,parameter :: KISRC=KI32
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: npack
    integer,         intent(inout) :: opts(:)
    integer,         intent(out)   :: xmem(0:*)
    real(kind=KARG), intent(in)    :: d(0:*)
    integer,         intent(in)    :: nbase
    integer,         intent(in)    :: kaxs(*)
    integer,         intent(in)    :: lx
    real(kind=KRMIS),intent(in)    :: vmiss

    integer,parameter :: mold = 0_KISRC
    integer  xh, xl
    integer  jbase, jbgn,  jend
    integer  jint,  jout
    integer  nint,  ntgt,  nmem, nx
    integer  j
    integer  jf
    integer  mbits, ndata
    real(kind=KARG) :: vu

    ierr = 0
    vu = real(vmiss, kind=KARG)

    call ptx_set_loops(nint, nmem, nx, kaxs, lx, opts)
    ntgt = nmem * nint

    ndata = 0
    xh = 0
    xl = HUGE(0)
    if (ierr.eq.0) then
       do jbase = 0, nbase - 1
          jout = jbase / nint
          jint = mod(jbase, nint)
          jbgn = jout * ntgt + jint
          jend = jbgn + ntgt
          jf = 0
          do j = jbgn, jend - 1, nint
             if (d(j).eq.vu) exit
             jf = jf + 1
          enddo
          jbgn = jbgn + nint * (jf + 1)
          if (ANY(d(jbgn:jend-1:nint).ne.vu)) then
             ierr = _ERROR(ERR_INVALID_ITEM)
             exit
          endif
          xmem(jbase) = jf
          ndata = ndata + xmem(jbase)
          xh = max(xh, xmem(jbase))
          if (xmem(jbase).ne.0) xl = min(xl, xmem(jbase))
       enddo
    endif
    if (ierr.eq.0) then
       mbits = max(1, opts(PROP_PTX_MBITS))
       if (xl.gt.xh) then
          xl = 0
       else
          xl = xl - 1
       endif
       xh = (first_bit(xh - xl) / mbits + 1) * mbits
       opts(PROP_PTX_MBITS) = xh
       opts(PROP_PTX_OFFSET) = xl
       npack = count_packed(xh, nbase, mold)
       xmem(0:nbase-1) = max(0, xmem(0:nbase-1) - xl)
    else
       npack = -1
       ndata = -1
    endif
    opts(PROP_PTX_DATA) = ndata
  end subroutine ptx_check_vec_d
  subroutine ptx_check_vec_f &
       & (ierr, npack, opts, xmem, d, nbase, kaxs, lx, vmiss)
    use TOUZA_Trp,only: count_packed
    use TOUZA_Trp,only: first_bit
    implicit none
    integer,parameter :: KARG=KFLT
    integer,parameter :: KISRC=KI32
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: npack
    integer,         intent(inout) :: opts(:)
    integer,         intent(out)   :: xmem(0:*)
    real(kind=KARG),    intent(in)    :: d(0:*)
    integer,         intent(in)    :: nbase
    integer,         intent(in)    :: kaxs(*)
    integer,         intent(in)    :: lx
    real(kind=KRMIS),intent(in)    :: vmiss

    integer,parameter :: mold = 0_KISRC
    integer  xh, xl
    integer  jbase, jbgn,  jend
    integer  jint,  jout
    integer  nint,  ntgt,  nmem, nx
    integer  j
    integer  jf
    integer  mbits, ndata
    real(kind=KARG) :: vu

    ierr = 0
    vu = real(vmiss, kind=KARG)

    call ptx_set_loops(nint, nmem, nx, kaxs, lx, opts)
    ntgt = nmem * nint

    ndata = 0
    xh = 0
    xl = HUGE(0)
    if (ierr.eq.0) then
       do jbase = 0, nbase - 1
          jout = jbase / nint
          jint = mod(jbase, nint)
          jbgn = jout * ntgt + jint
          jend = jbgn + ntgt
          jf = 0
          do j = jbgn, jend - 1, nint
             if (d(j).eq.vu) exit
             jf = jf + 1
          enddo
          jbgn = jbgn + nint * (jf + 1)
          if (ANY(d(jbgn:jend-1:nint).ne.vu)) then
             ierr = _ERROR(ERR_INVALID_ITEM)
             exit
          endif
          xmem(jbase) = jf
          ndata = ndata + xmem(jbase)
          xh = max(xh, xmem(jbase))
          if (xmem(jbase).ne.0) xl = min(xl, xmem(jbase))
       enddo
    endif
    if (ierr.eq.0) then
       mbits = max(1, opts(PROP_PTX_MBITS))
       if (xl.gt.xh) then
          xl = 0
       else
          xl = xl - 1
       endif
       xh = (first_bit(xh - xl) / mbits + 1) * mbits
       opts(PROP_PTX_MBITS) = xh
       opts(PROP_PTX_OFFSET) = xl
       npack = count_packed(xh, nbase, mold)
       xmem(0:nbase-1) = max(0, xmem(0:nbase-1) - xl)
    else
       npack = -1
       ndata = -1
    endif
    opts(PROP_PTX_DATA) = ndata
  end subroutine ptx_check_vec_f
  subroutine ptx_check_vec_i &
       & (ierr, npack, opts, xmem, d, nbase, kaxs, lx, vmiss)
    use TOUZA_Trp,only: count_packed
    use TOUZA_Trp,only: first_bit
    implicit none
    integer,parameter :: KISRC=KI32
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: npack
    integer,         intent(inout) :: opts(:)
    integer,         intent(out)   :: xmem(0:*)
    integer,         intent(in)    :: d(0:*)
    integer,         intent(in)    :: nbase
    integer,         intent(in)    :: kaxs(*)
    integer,         intent(in)    :: lx
    real(kind=KRMIS),intent(in)    :: vmiss

    integer,parameter :: mold = 0_KISRC
    integer  xh, xl
    integer  jbase, jbgn,  jend
    integer  jint,  jout
    integer  nint,  ntgt,  nmem, nx
    integer  j
    integer  jf
    integer  mbits, ndata
    integer  vu

    ierr = 0
    vu = int(vmiss)

    call ptx_set_loops(nint, nmem, nx, kaxs, lx, opts)
    ntgt = nmem * nint

    ndata = 0
    xh = 0
    xl = HUGE(0)
    if (ierr.eq.0) then
       do jbase = 0, nbase - 1
          jout = jbase / nint
          jint = mod(jbase, nint)
          jbgn = jout * ntgt + jint
          jend = jbgn + ntgt
          jf = 0
          do j = jbgn, jend - 1, nint
             if (d(j).eq.vu) exit
             jf = jf + 1
          enddo
          jbgn = jbgn + nint * (jf + 1)
          if (ANY(d(jbgn:jend-1:nint).ne.vu)) then
             ierr = _ERROR(ERR_INVALID_ITEM)
             exit
          endif
          xmem(jbase) = jf
          ndata = ndata + xmem(jbase)
          xh = max(xh, xmem(jbase))
          if (xmem(jbase).ne.0) xl = min(xl, xmem(jbase))
       enddo
    endif
    if (ierr.eq.0) then
       mbits = max(1, opts(PROP_PTX_MBITS))
       if (xl.gt.xh) then
          xl = 0
       else
          xl = xl - 1
       endif
       xh = (first_bit(xh - xl) / mbits + 1) * mbits
       opts(PROP_PTX_MBITS) = xh
       opts(PROP_PTX_OFFSET) = xl
       npack = count_packed(xh, nbase, mold)
       xmem(0:nbase-1) = max(0, xmem(0:nbase-1) - xl)
    else
       npack = -1
       ndata = -1
    endif
    opts(PROP_PTX_DATA) = ndata
  end subroutine ptx_check_vec_i

! !!!_  - ptx_transf_rjds
!   subroutine ptx_transf_rjds_dd &
!        & (ierr,  d,     dpack, ends, nsub, &
!        &  citer, npcol, kaxs,  lc,   opts)
!     implicit none
!     integer,parameter :: KARG=KDBL, KRSRC=KDBL
!     integer,            intent(out)   :: ierr
!     real(kind=KARG),    intent(out)   :: d(0:*)
!     real(kind=KRSRC),   intent(in)    :: dpack(0:*)
!     integer,            intent(inout) :: ends(0:*)
!     integer,            intent(in)    :: nsub
!     integer,            intent(in)    :: citer         ! target coordinate
!     integer,            intent(in)    :: npcol(0:*)
!     integer,            intent(in)    :: kaxs(*)
!     integer,            intent(in)    :: lc
!     integer,            intent(in)    :: opts(*)

!     integer jc
!     integer ni, nm, nx, ns, ne, np
!     integer ji, jm, jx, js, je, jp
!     integer jorg

!     ierr = 0

!     jc = opts(PROP_PTX_COOR)
!     ni = product(kaxs(1:jc-1))
!     nm = kaxs(jc)
!     nx = product(kaxs(jc+1:laxs))

!     if (citer.ge.1.and.citer.le.lc) then
!        ne = kaxs(citer)
!     else
!        ne = ni * nm * nx
!     endif

!     ends(1:ne-1) = ends(0:ne-2)
!     ends(0) = 0
!     jorg = 0
!     if (jc.eq.citer) then
!        ns = ni * nx
!        do js = 0, ns - 1
!           do jp = 0, npcol(js) - 1
!              d(ends(jp)) = real(dpack(jorg + jp), kind=KARG)
!              ends(jp) = ends(jp) + 1
!           enddo
!           jorg = jorg + npcol(js)
!        enddo
!     endif
!   end subroutine ptx_transf_rjds_dd

!!!_ + utilities
!!!_  & get_record_prop - get sequential record properties (byte-order and separator size)
  subroutine get_record_prop &
       & (ierr, krect, u)
    use TOUZA_Nio_std, only: KI32, KI64, KIOFS, is_eof_ss
    use TOUZA_Nio_std, only: WHENCE_ABS, sus_read_isep, sus_rseek, sus_eswap
    use TOUZA_Nio_std, only: sus_getpos
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
       &  d,  n, u, krect, sub, skip)
    use TOUZA_Nio_std,only: sus_read_lrec, sus_read_irec, choice
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)            :: ierr
    integer(kind=KARG),intent(out)            :: d(*)
    integer,           intent(in)             :: n
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: u
    logical,           intent(inout),optional :: sub
    logical,           intent(in),   optional :: skip

    logical swap, lrec
    integer div

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call sus_read_lrec(ierr, u, d, n, swap)
    else
       div = read_sep_flag(krect)
       if (choice(.FALSE., skip)) then
          call sus_read_irec(ierr, u, d, 0, swap, sub, div, n)
       else
          call sus_read_irec(ierr, u, d, n, swap, sub, div)
       endif
    endif
    return
  end subroutine get_data_record_i
  subroutine get_data_record_f &
       & (ierr, &
       &  d,  n, u, krect, sub, skip)
    use TOUZA_Nio_std,only: sus_read_lrec, sus_read_irec, choice
    implicit none
    integer,parameter :: KARG=KFLT
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(*)
    integer,        intent(in)             :: n
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: u
    logical,        intent(inout),optional :: sub
    logical,        intent(in),   optional :: skip

    logical swap, lrec
    integer div

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call sus_read_lrec(ierr, u, d, n, swap)
    else
       div = read_sep_flag(krect)
       if (choice(.FALSE., skip)) then
          call sus_read_irec(ierr, u, d, 0, swap, sub, div, n)
       else
          call sus_read_irec(ierr, u, d, n, swap, sub, div)
       endif
    endif
    return
  end subroutine get_data_record_f
  subroutine get_data_record_d &
       & (ierr, &
       &  d,  n, u, krect, sub, skip)
    use TOUZA_Nio_std,only: sus_read_lrec, sus_read_irec, choice
    implicit none
    integer,parameter :: KARG=KDBL
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(*)
    integer,        intent(in)             :: n
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: u
    logical,        intent(inout),optional :: sub
    logical,        intent(in),   optional :: skip

    logical swap, lrec
    integer div

    ierr = 0
    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       call sus_read_lrec(ierr, u, d, n, swap)
    else
       div = read_sep_flag(krect)
       if (choice(.FALSE., skip)) then
          call sus_read_irec(ierr, u, d, 0, swap, sub, div, n)
       else
          call sus_read_irec(ierr, u, d, n, swap, sub, div)
       endif
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
    call trace_err(ierr, 'get_data_record_slice')
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
    call trace_err(ierr, 'get_data_record_slice')
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
    call trace_err(ierr, 'get_data_record_slice')
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
  subroutine get_data_record_runl_f &
       & (ierr, &
       &  d,  n, u, krect, runl, nrl, sub)
    use TOUZA_Nio_std,only: sus_runl_read_irec
    implicit none
    integer,parameter :: KARG=KFLT
    integer,           intent(out)            :: ierr
    real(kind=KARG),   intent(out)            :: d(*)
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
  end subroutine get_data_record_runl_f
  subroutine get_data_record_runl_d &
       & (ierr, &
       &  d,  n, u, krect, runl, nrl, sub)
    use TOUZA_Nio_std,only: sus_runl_read_irec
    implicit none
    integer,parameter :: KARG=KDBL
    integer,           intent(out)            :: ierr
    real(kind=KARG),   intent(out)            :: d(*)
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
  end subroutine get_data_record_runl_d
!!!_  & get_data_drecord_list
  subroutine get_data_drecord_list_d &
       & (ierr, &
       &  d,  list, nd, u, krect, md, sub)
    use TOUZA_Nio_std,only: sus_list_read_irec
    implicit none
    integer,parameter :: KARG=KDBL
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(0:*)
    integer,        intent(in)             :: list(0:*)
    integer,        intent(in)             :: nd        ! d list size
    integer,        intent(in)             :: u
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: md        ! size of data record
    logical,        intent(inout),optional :: sub

    logical swap, lrec
    integer div

    ierr = 0

    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    else
       div = read_sep_flag(krect)
       call sus_list_read_irec(ierr, u, d, list, nd, swap, sub, div, md)
    endif
    return
  end subroutine get_data_drecord_list_d
  subroutine get_data_drecord_list_f &
       & (ierr, &
       &  d,  list, nd, u, krect, md, sub)
    use TOUZA_Nio_std,only: sus_list_read_irec
    implicit none
    integer,parameter :: KARG=KFLT, KRSRC=KDBL
    integer,           intent(out)            :: ierr
    real(kind=KARG),   intent(out)            :: d(0:*)
    integer,           intent(in)             :: list(0:*)
    integer,           intent(in)             :: nd        ! d list size
    integer,           intent(in)             :: u
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: md        ! size of data record
    logical,           intent(inout),optional :: sub

    character(len=*),parameter :: proc = 'gdl'
#define _WORK workd

    ierr = 0
    call alloc_work(ierr, nd, proc, real(0, kind=KRSRC))
    if (ierr.eq.0) then
       call get_data_drecord_list_d &
            & (ierr, &
            &  _WORK,  list, nd, u, krect, md, sub)
    endif
    if (ierr.eq.0) then
       d(0:nd-1) = real(_WORK(0:nd-1), kind=KARG)
    endif
    if (ierr.eq.0) call alloc_work(ierr, -1, proc, real(0, kind=KRSRC))
#undef _WORK
    return
  end subroutine get_data_drecord_list_f
  subroutine get_data_drecord_list_i &
       & (ierr, &
       &  d,  list, nd, u, krect, md, sub)
    use TOUZA_Nio_std,only: sus_list_read_irec
    implicit none
    integer,parameter :: KARG=KI32, KRSRC=KDBL
    integer,           intent(out)            :: ierr
    integer(kind=KARG),intent(out)            :: d(0:*)
    integer,           intent(in)             :: list(0:*)
    integer,           intent(in)             :: nd        ! d list size
    integer,           intent(in)             :: u
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: md        ! size of data record
    logical,           intent(inout),optional :: sub

    character(len=*),parameter :: proc = 'gdl'
#define _WORK workd

    ierr = 0
    call alloc_work(ierr, nd, proc, real(0, kind=KRSRC))
    if (ierr.eq.0) then
       call get_data_drecord_list_d &
            & (ierr, &
            &  _WORK,  list, nd, u, krect, md, sub)
    endif
    if (ierr.eq.0) then
       d(0:nd-1) = int(_WORK(0:nd-1), kind=KARG)
    endif
    if (ierr.eq.0) call alloc_work(ierr, -1, proc, real(0, kind=KRSRC))
#undef _WORK
    return
  end subroutine get_data_drecord_list_i
!!!_  & get_data_frecord_list
  subroutine get_data_frecord_list_f &
       & (ierr, &
       &  d,  list, nd, u, krect, md, sub)
    use TOUZA_Nio_std,only: sus_list_read_irec
    implicit none
    integer,parameter :: KARG=KFLT
    integer,        intent(out)            :: ierr
    real(kind=KARG),intent(out)            :: d(0:*)
    integer,        intent(in)             :: list(0:*)
    integer,        intent(in)             :: nd        ! d list size
    integer,        intent(in)             :: u
    integer,        intent(in)             :: krect
    integer,        intent(in)             :: md        ! size of data record
    logical,        intent(inout),optional :: sub

    logical swap, lrec
    integer div

    ierr = 0

    swap = IAND(krect, REC_SWAP).ne.0
    lrec = IAND(krect, REC_LSEP).ne.0
    if (lrec) then
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    else
       div = read_sep_flag(krect)
       call sus_list_read_irec(ierr, u, d, list, nd, swap, sub, div, md)
    endif
    return
  end subroutine get_data_frecord_list_f
  subroutine get_data_frecord_list_d &
       & (ierr, &
       &  d,  list, nd, u, krect, md, sub)
    use TOUZA_Nio_std,only: sus_list_read_irec
    implicit none
    integer,parameter :: KARG=KDBL, KRSRC=KFLT
    integer,           intent(out)            :: ierr
    real(kind=KARG),   intent(out)            :: d(0:*)
    integer,           intent(in)             :: list(0:*)
    integer,           intent(in)             :: nd        ! d list size
    integer,           intent(in)             :: u
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: md        ! size of data record
    logical,           intent(inout),optional :: sub

    character(len=*),parameter :: proc = 'gfl'
#define _WORK workf

    ierr = 0
    call alloc_work(ierr, nd, proc, real(0, kind=KRSRC))
    if (ierr.eq.0) then
       call get_data_frecord_list_f &
            & (ierr, &
            &  _WORK,  list, nd, u, krect, md, sub)
    endif
    if (ierr.eq.0) then
       d(0:nd-1) = real(_WORK(0:nd-1), kind=KARG)
    endif
    if (ierr.eq.0) call alloc_work(ierr, -1, proc, real(0, kind=KRSRC))
#undef _WORK
    return
  end subroutine get_data_frecord_list_d
  subroutine get_data_frecord_list_i &
       & (ierr, &
       &  d,  list, nd, u, krect, md, sub)
    use TOUZA_Nio_std,only: sus_list_read_irec
    implicit none
    integer,parameter :: KARG=KI32, KRSRC=KFLT
    integer,           intent(out)            :: ierr
    integer(kind=KARG),intent(out)            :: d(0:*)
    integer,           intent(in)             :: list(0:*)
    integer,           intent(in)             :: nd        ! d list size
    integer,           intent(in)             :: u
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: md        ! size of data record
    logical,           intent(inout),optional :: sub

    character(len=*),parameter :: proc = 'gfl'
#define _WORK workf

    ierr = 0
    call alloc_work(ierr, nd, proc, real(0, kind=KRSRC))
    if (ierr.eq.0) then
       call get_data_frecord_list_f &
            & (ierr, &
            &  _WORK,  list, nd, u, krect, md, sub)
    endif
    if (ierr.eq.0) then
       d(0:nd-1) = int(_WORK(0:nd-1), kind=KARG)
    endif
    if (ierr.eq.0) call alloc_work(ierr, -1, proc, real(0, kind=KRSRC))
#undef _WORK
    return
  end subroutine get_data_frecord_list_i
!!!_  & get_data_irecord_list
  subroutine get_data_irecord_list_i &
       & (ierr, &
       &  d,  list, nd, u, krect, md, sub)
    use TOUZA_Nio_std,only: sus_list_read_irec
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)            :: ierr
    integer(kind=KARG),intent(out)            :: d(0:*)
    integer,           intent(in)             :: list(0:*)
    integer,           intent(in)             :: nd        ! d list size
    integer,           intent(in)             :: u
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: md        ! size of data record
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
       call sus_list_read_irec(ierr, u, d, list, nd, swap, sub, div, md)
    endif
    return
  end subroutine get_data_irecord_list_i
  subroutine get_data_irecord_list_f &
       & (ierr, &
       &  d,  list, nd, u, krect, md, sub)
    use TOUZA_Nio_std,only: sus_list_read_irec
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32
    integer,           intent(out)            :: ierr
    real(kind=KARG),   intent(out)            :: d(0:*)
    integer,           intent(in)             :: list(0:*)
    integer,           intent(in)             :: nd        ! d list size
    integer,           intent(in)             :: u
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: md        ! size of data record
    logical,           intent(inout),optional :: sub

    character(len=*),parameter :: proc = 'gil'
#define _WORK worki

    ierr = 0
    call alloc_work(ierr, nd, proc, int(0, kind=KISRC))
    if (ierr.eq.0) then
       call get_data_irecord_list_i &
            & (ierr, &
            &  _WORK,  list, nd, u, krect, md, sub)
    endif
    if (ierr.eq.0) then
       d(0:nd-1) = real(_WORK(0:nd-1), kind=KARG)
    endif
    if (ierr.eq.0) call alloc_work(ierr, -1, proc, int(0, kind=KISRC))
#undef _WORK
    return
  end subroutine get_data_irecord_list_f
  subroutine get_data_irecord_list_d &
       & (ierr, &
       &  d,  list, nd, u, krect, md, sub)
    use TOUZA_Nio_std,only: sus_list_read_irec
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32
    integer,           intent(out)            :: ierr
    real(kind=KARG),   intent(out)            :: d(0:*)
    integer,           intent(in)             :: list(0:*)
    integer,           intent(in)             :: nd        ! d list size
    integer,           intent(in)             :: u
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: md        ! size of data record
    logical,           intent(inout),optional :: sub

    character(len=*),parameter :: proc = 'gil'
#define _WORK worki

    ierr = 0
    call alloc_work(ierr, nd, proc, int(0, kind=KISRC))
    if (ierr.eq.0) then
       call get_data_irecord_list_i &
            & (ierr, &
            &  _WORK,  list, nd, u, krect, md, sub)
    endif
    if (ierr.eq.0) then
       d(0:nd-1) = real(_WORK(0:nd-1), kind=KARG)
    endif
    if (ierr.eq.0) call alloc_work(ierr, -1, proc, int(0, kind=KISRC))
#undef _WORK
    return
  end subroutine get_data_irecord_list_d

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

!!!_  & get_data_irecord_runl - read data block (runlength) int32
  subroutine get_data_irecord_runl_f &
       & (ierr, &
       &  d,  n, u, krect, runl, nrl, sub)
    implicit none
    integer,parameter :: KARG=KI32
    integer,parameter :: KISRC=KI32
    integer,           intent(out)            :: ierr
    real(kind=KARG),   intent(out)            :: d(0:*)
    integer,           intent(in)             :: n
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: u
    integer,           intent(in)             :: runl(*)
    integer,           intent(in)             :: nrl
    logical,           intent(inout),optional :: sub
    character(len=*),parameter :: wtag = 'gdr'
    ierr = 0
    call alloc_work(ierr, n, wtag, int(0, kind=KISRC))
    if (ierr.eq.0) then
       call get_data_record_runl &
            & (ierr, worki, n, u, krect, runl, nrl, sub)
    endif
    if (ierr.eq.0) then
       d(0:n-1) = real(worki(0:n-1), kind=KARG)
    endif
    call alloc_work(ierr, -1, wtag, int(0, kind=KISRC))
  end subroutine get_data_irecord_runl_f
  subroutine get_data_irecord_runl_d &
       & (ierr, &
       &  d,  n, u, krect, runl, nrl, sub)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,parameter :: KISRC=KI32
    integer,           intent(out)            :: ierr
    real(kind=KARG),   intent(out)            :: d(0:*)
    integer,           intent(in)             :: n
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: u
    integer,           intent(in)             :: runl(*)
    integer,           intent(in)             :: nrl
    logical,           intent(inout),optional :: sub
    character(len=*),parameter :: wtag = 'gdr'
    ierr = 0
    call alloc_work(ierr, n, wtag, int(0, kind=KISRC))
    if (ierr.eq.0) then
       call get_data_record_runl &
            & (ierr, worki, n, u, krect, runl, nrl, sub)
    endif
    if (ierr.eq.0) then
       d(0:n-1) = real(worki(0:n-1), kind=KARG)
    endif
    call alloc_work(ierr, -1, wtag, int(0, kind=KISRC))
  end subroutine get_data_irecord_runl_d
!!!_  & get_data_frecord_runl - read data block (runlength) binary32
  subroutine get_data_frecord_runl_i &
       & (ierr, &
       &  d,  n, u, krect, runl, nrl, sub)
    implicit none
    integer,parameter :: KARG=KI32
    integer,parameter :: KRSRC=KFLT
    integer,           intent(out)            :: ierr
    integer(kind=KARG),intent(out)            :: d(0:*)
    integer,           intent(in)             :: n
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: u
    integer,           intent(in)             :: runl(*)
    integer,           intent(in)             :: nrl
    logical,           intent(inout),optional :: sub
    character(len=*),parameter :: wtag = 'gdr'
    ierr = 0
    call alloc_work(ierr, n, wtag, real(0, kind=KRSRC))
    if (ierr.eq.0) then
       call get_data_record_runl &
            & (ierr, workf, n, u, krect, runl, nrl, sub)
    endif
    if (ierr.eq.0) then
       d(0:n-1) = int(workf(0:n-1))
    endif
    call alloc_work(ierr, -1, wtag, real(0, kind=KRSRC))
  end subroutine get_data_frecord_runl_i
  subroutine get_data_frecord_runl_d &
       & (ierr, &
       &  d,  n, u, krect, runl, nrl, sub)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,parameter :: KRSRC=KFLT
    integer,           intent(out)            :: ierr
    real(kind=KARG),   intent(out)            :: d(0:*)
    integer,           intent(in)             :: n
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: u
    integer,           intent(in)             :: runl(*)
    integer,           intent(in)             :: nrl
    logical,           intent(inout),optional :: sub
    character(len=*),parameter :: wtag = 'gdr'
    ierr = 0
    call alloc_work(ierr, n, wtag, real(0, kind=KRSRC))
    if (ierr.eq.0) then
       call get_data_record_runl &
            & (ierr, workf, n, u, krect, runl, nrl, sub)
    endif
    if (ierr.eq.0) then
       d(0:n-1) = real(workf(0:n-1), kind=KARG)
    endif
    call alloc_work(ierr, -1, wtag, real(0, kind=KRSRC))
  end subroutine get_data_frecord_runl_d
!!!_  & get_data_drecord_runl - read data block (runlength) binary64
  subroutine get_data_drecord_runl_i &
       & (ierr, &
       &  d,  n, u, krect, runl, nrl, sub)
    implicit none
    integer,parameter :: KARG=KI32
    integer,parameter :: KRSRC=KDBL
    integer,           intent(out)            :: ierr
    integer(kind=KARG),intent(out)            :: d(0:*)
    integer,           intent(in)             :: n
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: u
    integer,           intent(in)             :: runl(*)
    integer,           intent(in)             :: nrl
    logical,           intent(inout),optional :: sub
    character(len=*),parameter :: wtag = 'gdr'
    ierr = 0
    call alloc_work(ierr, n, wtag, real(0, kind=KRSRC))
    if (ierr.eq.0) then
       call get_data_record_runl &
            & (ierr, workd, n, u, krect, runl, nrl, sub)
    endif
    if (ierr.eq.0) then
       d(0:n-1) = int(workd(0:n-1))
    endif
    call alloc_work(ierr, -1, wtag, real(0, kind=KRSRC))
  end subroutine get_data_drecord_runl_i
  subroutine get_data_drecord_runl_f &
       & (ierr, &
       &  d,  n, u, krect, runl, nrl, sub)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,parameter :: KRSRC=KDBL
    integer,           intent(out)            :: ierr
    real(kind=KARG),   intent(out)            :: d(0:*)
    integer,           intent(in)             :: n
    integer,           intent(in)             :: krect
    integer,           intent(in)             :: u
    integer,           intent(in)             :: runl(*)
    integer,           intent(in)             :: nrl
    logical,           intent(inout),optional :: sub
    character(len=*),parameter :: wtag = 'gdr'
    ierr = 0
    call alloc_work(ierr, n, wtag, real(0, kind=KRSRC))
    if (ierr.eq.0) then
       call get_data_record_runl &
            & (ierr, workd, n, u, krect, runl, nrl, sub)
    endif
    if (ierr.eq.0) then
       d(0:n-1) = real(workd(0:n-1), kind=KARG)
    endif
    call alloc_work(ierr, -1, wtag, real(0, kind=KRSRC))
  end subroutine get_data_drecord_runl_f

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
    use TOUZA_Nio_header,only: litem
    use TOUZA_Nio_header,only: hi_DFMT,  hi_MISS
    use TOUZA_Nio_header,only: hi_ASTR1, hi_ASTR2, hi_ASTR3
    use TOUZA_Nio_header,only: hi_AEND1, hi_AEND2, hi_AEND3
    use TOUZA_Nio_header,only: get_item
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

!!!_  & parse_header_size_all
  subroutine parse_header_size_all &
       & (kaxs, head)
    implicit none
    integer,         intent(out) :: kaxs(*)
    character(len=*),intent(in)  :: head(*)
    integer jc
    do jc = 1, laxs
       kaxs(jc) = parse_header_size(head, jc)
    enddo
  end subroutine parse_header_size_all

!!!_  & parse_header_size - parse size properties
  integer function parse_header_size_n &
       & (head, kidx, lazy) &
       & result (n)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_header,only: hi_ASTR1, hi_ASTR2, hi_ASTR3
    use TOUZA_Nio_header,only: hi_AEND1, hi_AEND2, hi_AEND3
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
  function parse_header_size_i &
       & (head, kidx, lazy, mold) &
       & result (n)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_header,only: hi_ASTR1, hi_ASTR2, hi_ASTR3
    use TOUZA_Nio_header,only: hi_AEND1, hi_AEND2, hi_AEND3
    implicit none
    integer,parameter :: KARG=KI32
    integer(KIND=KARG) :: n
    character(len=*),  intent(in)  :: head(*)
    integer,           intent(in)  :: kidx
    integer,optional,  intent(in)  :: lazy
    integer(KIND=KARG),intent(in)  :: mold
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
       kfmt = GFMT_LPAD
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

!!!_  & get_header_cname - get coordinate name
  subroutine get_header_cname &
       & (name, head, kidx)
    use TOUZA_Nio_header,only: get_item
    use TOUZA_Nio_header,only: hi_AITM1, hi_AITM2, hi_AITM3
    implicit none
    character(len=*),intent(out) :: name
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: kidx
    integer jerr
    select case (kidx)
    case (1)
       call get_item(jerr, head, name, hi_AITM1)
    case (2)
       call get_item(jerr, head, name, hi_AITM2)
    case (3)
       call get_item(jerr, head, name, hi_AITM3)
    case default
       jerr = -1
    end select
    if (jerr.ne.0) then
       name = ' '
    endif
  end subroutine get_header_cname
!!!_  & put_header_cname - put coordinate names
  subroutine put_header_cname &
       & (ierr, head, name, kidx)
    use TOUZA_Nio_header,only: put_item
    use TOUZA_Nio_header,only: hi_AITM1, hi_AITM2, hi_AITM3
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(inout) :: head(*)
    character(len=*),intent(in)    :: name
    integer,         intent(in)    :: kidx
    ierr = 0
    select case (kidx)
    case (1)
       call put_item(ierr, head, name, hi_AITM1)
    case (2)
       call put_item(ierr, head, name, hi_AITM2)
    case (3)
       call put_item(ierr, head, name, hi_AITM3)
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
  end subroutine put_header_cname

!!!_  & get_header_cprop - get coordinate properties
  subroutine get_header_cprop &
       & (name, irange, head, kidx)
    use TOUZA_Nio_header,only: get_item
    use TOUZA_Nio_header,only: hi_AITM1, hi_AITM2, hi_AITM3
    use TOUZA_Nio_header,only: hi_ASTR1, hi_ASTR2, hi_ASTR3
    use TOUZA_Nio_header,only: hi_AEND1, hi_AEND2, hi_AEND3
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
    use TOUZA_Nio_header,only: put_item
    use TOUZA_Nio_header,only: hi_AITM1, hi_AITM2, hi_AITM3
    use TOUZA_Nio_header,only: hi_ASTR1, hi_ASTR2, hi_ASTR3
    use TOUZA_Nio_header,only: hi_AEND1, hi_AEND2, hi_AEND3
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

!!!_  & inquire_header_coor - search coordinate
  subroutine inquire_header_coor &
       & (head, name, idx, jbgn, jend, mem)
    use TOUZA_Nio_std,only: set_if_present
    implicit none
    character(len=*),intent(in)  :: head(*)
    character(len=*),intent(in)  :: name
    integer,optional,intent(out) :: idx
    integer,optional,intent(out) :: jbgn, jend
    integer,optional,intent(out) :: mem
    integer j, jc
    integer irange(2)
    character(len=litem) :: co

    jc = 0
    do j = 1, laxs
       call get_header_cprop(co, irange, head, j)
       if (co.eq.name) then
          jc = j
          exit
       endif
    enddo
    if (jc.gt.0) then
       call set_if_present(idx,  jc)
       call set_if_present(jbgn, irange(1))
       call set_if_present(jend, irange(2))
       if (present(mem)) then
          mem = irange(2) - irange(1) + 1
       endif
    else
       call set_if_present(idx,  -1)
       call set_if_present(jbgn, -1)
       call set_if_present(jend, -1)
       call set_if_present(mem,  -1)
    endif
  end subroutine inquire_header_coor

!!!_  & shift_header_coor - shift coordinate
  subroutine shift_header_coor &
       & (ierr, head, cdest, cend)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(inout) :: head(*)
    integer,         intent(in)    :: cdest
    integer,optional,intent(in)    :: cend   ! [maximum]
    integer j, je, js
    integer irange(2)
    character(len=litem) :: co

    ierr = 0

    je = choice(-1, cend)
    if (je.lt.0) je = laxs + 1

    if (cdest.lt.1.or.cdest.gt.laxs) then
       ierr = _ERROR(ERR_INVALID_PARAMETER)
       return
    endif
    if (cdest.eq.je) return

    if (cdest.gt.je) then
       je = je + 1
       js = -1
       ! . E o o D
       ! . - E o o
    else
       je = je - 1
       js = +1
       ! . D o o E
       ! . o o E -
    endif
    do j = cdest, je, js
       if (ierr.eq.0) then
          call get_header_cprop(co, irange, head, j + js)
          call put_header_cprop(ierr, head, co, irange, j)
       endif
    enddo
    return
  end subroutine shift_header_coor

!!!_  & search_null_coor - search blank coordinate
  integer function search_null_coor &
       & (head, dir, named, one) &
       & result (c)
    use TOUZA_Nio_std,only: choice
    implicit none
    character(len=*),intent(in) :: head(*)
    integer,optional,intent(in) :: dir      ! [-1]   search direction
    logical,optional,intent(in) :: named    ! [true] allow non-blank name
    logical,optional,intent(in) :: one      ! [true] allow size=1
    integer j, jb, je, js
    integer irange(2), m
    character(len=litem) :: co
    logical bn, bo

    js = choice(0, dir)
    bn = choice(.TRUE., named)
    bo = choice(.TRUE., one)

    if (js.le.0) then
       js = -1
       jb = laxs
       je = 1
    else
       js = 1
       je = laxs
       jb = 1
    endif

    c = 0
    ! write(*, *) 'search range', jb, je, js
    do j = jb, je, js
       call get_header_cprop(co, irange, head, j)
       m = max(0, irange(2) - irange(1))
       ! write(*, *) 'search', j, bo, bn, m, co
       if (m.gt.1) cycle
       if (m.eq.1   .and. .not.bo) cycle
       if (co.ne.' '.and. .not.bn) cycle
       c = j
       exit
    enddo

  end function search_null_coor

!!!_  & mask_encode - mask encoding
  subroutine mask_encode_di &
       & (ierr, mb,   icom,   b, &
       &  d,    n,    vmiss,  kpack)
    use TOUZA_Trp,only: count_packed, pack_store
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: mb        ! number of unmasked items
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
       & (ierr, mb,   icom,   b, &
       &  d,    n,    vmiss,  kpack)
    use TOUZA_Trp,only: count_packed, pack_store
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: mb        ! number of unmasked items
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
       & (ierr, mb,   icom,   b, &
       &  d,    n,    vmiss,  kpack)
    use TOUZA_Trp,only: count_packed, pack_store
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: mb        ! number of unmasked items
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

!!!_  & legacy_packing ()
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
!!!_  & legacy_unpacking ()
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
    integer,intent(in) :: bes(3, *)
    integer,intent(in) :: nc
    n = max(1, product(max(1, bes(2, 1:nc) - bes(1, 1:nc))))
  end function count_bes

!!!_  & bes_triplet
  subroutine bes_triplet(ierr, bes, kaxs, lx, start, count)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: bes(3, *)
    integer,intent(in)  :: kaxs(*)
    integer,intent(in)  :: lx
    integer,intent(in)  :: start(:)
    integer,intent(in)  :: count(:)
    integer nr
    ierr = 0
    nr = min(size(start), size(count))
    if (nr.gt.lx) then
       ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
    else
       bes(3, 1:lx) = max(1, kaxs(1:lx))
       bes(1, 1:nr) = max(0, start(1:nr))
       bes(2, 1:nr) = min(bes(3, 1:nr), start(1:nr) + max(0, count(1:nr)))
       bes(1, nr+1:lx) = 0
       bes(2, nr+1:lx) = bes(3, nr+1:lx)
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
! !!!_  & alloc_worki
!   subroutine alloc_worki(ierr, n, tag, mold)
!     implicit none
!     integer,         intent(out) :: ierr
!     integer,         intent(in)  :: n
!     character(len=*),intent(in)  :: tag
!     integer,optional,intent(in)  :: mold
!     ierr = 0
!     if (n.lt.0) then
!        call diag_alloc('u', 'worki', wtag_i, tag, size(worki))
!        wtag_i = ' '
!        return
!     endif
!     if (wtag_i.ne.' ') then
!        call diag_alloc('e', 'worki', wtag_i, tag, size(worki))
!        ierr = _ERROR(ERR_PANIC)
!        return
!     endif
!     call diag_alloc('l', 'worki', wtag_i, tag, n)
!     if (ierr.eq.0) wtag_i = tag
!     if (allocated(worki)) then
!        if (n.le.size(worki)) return
!        deallocate(worki, STAT=ierr)
!     endif
!     if (ierr.eq.0) allocate(worki(0:n-1), STAT=ierr)
!   end subroutine alloc_worki
! !!!_  & alloc_workf
!   subroutine alloc_workf(ierr, n, tag, mold)
!     implicit none
!     integer,parameter :: KARG=KFLT
!     integer,         intent(out)         :: ierr
!     integer,         intent(in)          :: n
!     character(len=*),intent(in)          :: tag
!     real(kind=KARG), intent(in),optional :: mold
!     ierr = 0
!     if (n.lt.0) then
!        call diag_alloc('u', 'workf', wtag_f, tag, size(workf))
!        wtag_f = ' '
!        return
!     endif
!     if (wtag_f.ne.' ') then
!        call diag_alloc('e', 'workf', wtag_f, tag, size(workf))
!        ierr = _ERROR(ERR_PANIC)
!        return
!     endif
!     call diag_alloc('l', 'workf', wtag_f, tag, n)
!     if (ierr.eq.0) wtag_f = tag
!     if (allocated(workf)) then
!        if (n.le.size(workf)) return
!        deallocate(workf, STAT=ierr)
!     endif
!     if (ierr.eq.0) allocate(workf(0:n-1), STAT=ierr)
!   end subroutine alloc_workf
!!!_  & alloc_work
  subroutine alloc_work_d(ierr, n, tag, mold)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: n
    character(len=*),intent(in)  :: tag
    real(kind=KARG), intent(in)  :: mold
    character(len=*),parameter :: wstr = 'workd'
#define _WORK workd
#define _WTAG wtag_d
    if (mold.eq.0) continue  ! dummy
    ierr = 0
    if (n.lt.0) then
       call diag_alloc('u', wstr, _WTAG, tag, size(_WORK))
       _WTAG = ' '
       return
    endif
    if (_WTAG.ne.' ') then
       call diag_alloc('e', wstr, _WTAG, tag, size(_WORK))
       ierr = _ERROR(ERR_PANIC)
       return
    endif
    call diag_alloc('l', wstr, _WTAG, tag, n)
    if (ierr.eq.0) _WTAG = tag
    if (allocated(_WORK)) then
       if (n.le.size(_WORK)) return
       deallocate(_WORK, STAT=ierr)
    endif
    if (ierr.eq.0) allocate(_WORK(0:n-1), STAT=ierr)
#undef _WORK
#undef _WTAG
  end subroutine alloc_work_d

  subroutine alloc_work_f(ierr, n, tag, mold)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: n
    character(len=*),intent(in)  :: tag
    real(kind=KARG), intent(in)  :: mold
    character(len=*),parameter :: wstr = 'workf'
#define _WORK workf
#define _WTAG wtag_f
    if (mold.eq.0) continue  ! dummy
    ierr = 0
    if (n.lt.0) then
       call diag_alloc('u', wstr, _WTAG, tag, size(_WORK))
       _WTAG = ' '
       return
    endif
    if (_WTAG.ne.' ') then
       call diag_alloc('e', wstr, _WTAG, tag, size(_WORK))
       ierr = _ERROR(ERR_PANIC)
       return
    endif
    call diag_alloc('l', wstr, _WTAG, tag, n)
    if (ierr.eq.0) _WTAG = tag
    if (allocated(_WORK)) then
       if (n.le.size(_WORK)) return
       deallocate(_WORK, STAT=ierr)
    endif
    if (ierr.eq.0) allocate(_WORK(0:n-1), STAT=ierr)
#undef _WORK
#undef _WTAG
  end subroutine alloc_work_f

  subroutine alloc_work_i(ierr, n, tag, mold)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: n
    character(len=*),intent(in)  :: tag
    integer,         intent(in)  :: mold
    character(len=*),parameter :: wstr = 'worki'
#define _WORK worki
#define _WTAG wtag_i
    if (mold.eq.0) continue  ! dummy
    ierr = 0
    if (n.lt.0) then
       call diag_alloc('u', wstr, _WTAG, tag, size(_WORK))
       _WTAG = ' '
       return
    endif
    if (_WTAG.ne.' ') then
       call diag_alloc('e', wstr, _WTAG, tag, size(_WORK))
       ierr = _ERROR(ERR_PANIC)
       return
    endif
    call diag_alloc('l', wstr, _WTAG, tag, n)
    if (ierr.eq.0) _WTAG = tag
    if (allocated(_WORK)) then
       if (n.le.size(_WORK)) return
       deallocate(_WORK, STAT=ierr)
    endif
    if (ierr.eq.0) allocate(_WORK(0:n-1), STAT=ierr)
#undef _WORK
#undef _WTAG
  end subroutine alloc_work_i

!!!_  & alloc_work[ifd] - compatible procedures
  subroutine alloc_worki(ierr, n, tag)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: n
    character(len=*),intent(in)  :: tag
    call alloc_work(ierr, n, tag, int(0))
  end subroutine alloc_worki
  subroutine alloc_workf(ierr, n, tag)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: n
    character(len=*),intent(in)  :: tag
    call alloc_work(ierr, n, tag, real(0, KIND=KARG))
  end subroutine alloc_workf
  subroutine alloc_workd(ierr, n, tag)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: n
    character(len=*),intent(in)  :: tag
    call alloc_work(ierr, n, tag, real(0, KIND=KARG))
  end subroutine alloc_workd

!!!_  & alloc_wpack
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
!!!_  & alloc_wmask
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
!!!_  & alloc_wsubv
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
    ierr = 0
    if (is_review_leave(flag)) return
    if (ierr.eq.0) call sus_rseek(ierr, u, apini, whence=WHENCE_ABS)
  end subroutine post_review
!!!_  & is_review_leave()
  logical function is_review_leave &
       & (flag) &
       & result(b)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,optional,intent(in) :: flag
    integer f
    f = choice(0, flag)
    b = IAND(f, rev_pos_leave).ne.0
  end function is_review_leave
!!!_  & is_nio_format()
  logical function is_match_format &
       & (kfmt, st, pr) &
       & result(b)
    use TOUZA_Nio_std,only: choice, choice_a, upcase
    implicit none
    integer,         intent(in)          :: kfmt
    character(len=*),intent(in),optional :: st   ! storage   U P M
    character(len=*),intent(in),optional :: pr   ! precision I F D R Y T C
    character(len=16) :: s, p
    integer fs, fp

    if (kfmt.lt.0) then
       b = .FALSE.
       return
    endif
    call choice_a(s, ' ', st)
    call choice_a(p, ' ', pr)
    call upcase(s)
    call upcase(p)

    fs = IAND(kfmt, GFMT_MASK+GFMT_LPAD)
    fp = mod(kfmt, min(GFMT_MASK, GFMT_LPAD))

    b = .TRUE.
    if (b) then
       select case (p(1:1))
       case('I')
          b = fp.gt.GFMT_UI0 .and. fp.le.GFMT_UI8
       case('R')
          b = fp.ge.GFMT_UR8 .and. fp.le.GFMT_UR4
       case('D')
          b = fp.eq.GFMT_UR8
       case('F')
          b = fp.eq.GFMT_UR4
       case('Y')
          b = fp.ge.GFMT_URY .and. fp.le.GFMT_URYend
       case('C')
          b = fp.ge.GFMT_URC .and. fp.le.GFMT_URC2
       case('T')
          b = fp.eq.GFMT_URT
       end select
    endif
    if (b) then
       select case (s(1:1))
       case('U')
          b = fs.eq.0
       case('M')
          b = fs.eq.GFMT_MASK
       case('P')
          b = fs.eq.GFMT_LPAD
       end select
    endif

  end function is_match_format

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
    if (IAND(f, rev_pos_leave).eq.0) then
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
    if (IAND(f, rev_pos_leave).eq.0) then
       if (ierr.eq.0) call sus_rseek(ierr, u, jpos, whence=WHENCE_ABS)
    else
       if (ierr.eq.0) call nio_skip_prec(ierr, u, 1, krect)   ! data
    endif
    call alloc_worki(ierr, -1, 'def:mrn')
  end subroutine nio_count_defined_mrn
!!!_  - debug_file_pos
  subroutine debug_file_pos(u, tag)
    use TOUZA_Nio_std,only: KIOFS
    implicit none
    integer,         intent(in) :: u
    character(len=*),intent(in) :: tag
    integer(kind=KIOFS) :: jpos
    integer jerr
    inquire(u, POS=jpos, IOSTAT=jerr)
    if (jerr.ne.0) jpos = 0
101 format(A, ': ', I0, 1x, I0, 1x, Z8.8)
    write(*, 101) trim(tag), u, jerr, jpos - 1
  end subroutine debug_file_pos
!!!_ + end module TOUZA_Nio_record
end module TOUZA_Nio_record

!!!_@ test_nio_record - test program
#ifdef TEST_NIO_RECORD
program test_nio_record
  use TOUZA_Std,       only: parse, get_param, arg_diag, arg_init
  use TOUZA_Std,       only: upcase, get_option
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
  call init(ierr, stdv=+9, icomm=MPI_COMM_NULL)
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
        write(*, 201) ktest, 'packed (removed)'
        ! call test_batch_read_packed(ierr, jarg)
     case (10)
        write(*, 201) ktest, 'byte order'
        call test_batch_byte_order(ierr, jarg)
     case (11)
        write(*, 201) ktest, 'ptx (removed)'
        ! call test_check_ptx(ierr, jarg)
     case default
        write(*, *) 'INVALID TEST = ', ktest
        ierr = -1
     end select
  endif

  if (ierr.eq.0) call finalize(ierr, levv=+9)
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
    use TOUZA_Nio_std,only: KDBL,  KIOFS
    use TOUZA_Nio_std,only: sus_open, sus_close, sus_write_irec, sus_write_lrec
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
    character(len=litem) :: hd(nhi)
    character(len=litem) :: citem
    integer krect, krectw
    integer,parameter :: lmax = 2 ** 24
    real(kind=KBUF),allocatable :: v(:)

    ierr = 0
    jpos = -1

101 format('test:', I0, 1x, A, 1x, A, 1x, A)
401 format('header/r:', I0, 1x, I0, 1x, I0, 1x, A)
402 format('data/r:', I0, 1x, I0)
301 format('v:', I0, 1x, I0, 1x, E24.16)
501 format('header/w:', I0, 1x, I0)
502 format('data/w:', I0, 1x, I0)

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
             if (ierr.eq.0) call get_item(ierr, hd, citem, hi_ITEM)
             write (*, 401) ierr, jrec, krect, trim(citem)
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
                   write(*, 501) ierr, jrec
                   if (ierr.eq.0) call nio_write_data(ierr, v, lmax, hd, krectw, uwrite)
                   write(*, 502) ierr, jrec
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
    jposb = 0
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
    integer(kind=KIOFS) :: jpos

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
401 format('header/r:', I0, 1x, I0, 1x, I0, 1x, Z8.8)
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
       inquire(uread, POS=jpos, IOSTAT=ierr)
       if (ierr.eq.0) call nio_read_header(ierr, hd, krect, uread)
       write (*, 401) ierr, jrec, krect, jpos - 1
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
!!!_  & restore_pr8_rjds - restore PR8 data to JDS-like array
  subroutine restore_pr8_rjds_d &
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
    integer nbase, npack, ndata
    integer opts(lopts_ptx)
    integer mbits, kpack
    logical cont
    integer cmem,  cofs
    integer ch, ci

    ierr = 0

    ch = choice(packed_read, check)
    ci = choice(3, citer)

    cont = .TRUE.
    if (ierr.eq.0) call get_data_record(ierr, opts, lopts_ptx, u, krect, sub=cont)
    if (ierr.eq.0) then
       mbits = opts(PROP_PTX_MBITS)
       nbase = ptx_vec_size(kaxs, laxs, opts)
       npack = count_packed(mbits, nbase, mold)
       ndata = opts(PROP_PTX_DATA)
       kpack = opts(PROP_PTX_CODES)
    endif
    if (ierr.eq.0) call alloc_wmask(ierr, nbase + 1, 'rst:pr8d', mold)
    if (ierr.eq.0) call alloc_wpack(ierr, npack, 'rst:pr8d', mold)
    if (ierr.eq.0) call get_data_record(ierr, wpack, npack, u, krect, sub=cont)
    if (ierr.eq.0) call pack_restore(ierr, wmask, wpack, nbase, mbits, kpack)
    if (ierr.eq.0) call ptx_expand_mems(ierr, wmask, nbase, opts)

    if (ierr.eq.0) call alloc_workd(ierr, ndata, 'rst:pr8d')
    ! read data as is
    if (ierr.eq.0) call get_data_record(ierr, workd, ndata, u, krect, sub=cont)

    if (ierr.eq.0) then
       call ptx_decode_subv &
            & (ierr,  d,     subv,  ends, &
            &  workd, ldata, &
            &  ci,    wmask, kaxs,  laxs,  opts)
    endif
    if (ierr.eq.0) call alloc_workd(ierr, -1, 'rst:pr8d')
    if (ierr.eq.0) call alloc_wmask(ierr, -1, 'rst:pr8d', mold)
    if (ierr.eq.0) call alloc_wpack(ierr, -1, 'rst:pr8d', mold)

    return
  end subroutine restore_pr8_rjds_d
!!!_  & ptx_decode_subv - subscript vector decoding
  subroutine ptx_decode_subv_dd &
       & (ierr,  d,     subv,  ends, &
       &  dsrc,  nsub,  &
       &  citer, npcol, kaxs,  lc,   opts)
    implicit none
    integer,parameter :: KARG=KDBL, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(out) :: subv(0:*)
    integer,         intent(out) :: ends(0:*)
    real(kind=KRSRC),intent(in)  :: dsrc(0:*)
    integer,         intent(in)  :: nsub
    integer,         intent(in)  :: citer         ! target coordinate
    integer,         intent(in)  :: npcol(0:*)
    integer,         intent(in)  :: kaxs(*)
    integer,         intent(in)  :: lc
    integer,         intent(in)  :: opts(*)
    integer jc
    integer ni, nm, nx, ns, ne, np
    integer ji, jm, jx, js, je, jp
    integer jorg

    ierr = 0

    call ptx_set_loops(ni, nm, nx, kaxs, laxs, opts)
    ni = product(kaxs(1:jc-1))
    nx = product(kaxs(jc+1:lc))

    if (citer.ge.1.and.citer.le.lc) then
       ne = kaxs(citer)
    else
       ne = product(kaxs(1:lc))
    endif
    ends(0:ne-1) = 0
    if (jc.eq.citer) then
       do js = 0, ns - 1
          np = npcol(js)
          ends(0:np-1) = ends(0:np-1) + 1
       enddo
       ends(1:ne-1) = ends(0:ne-2)
       ends(0) = 0
       do je = 1, ne - 1
          ends(je) = ends(je - 1) + ends(je)
       enddo
       jorg = 0
       do js = 0, ns - 1
          do jp = 0, npcol(js) - 1
             d(ends(jp)) = real(dsrc(jorg + jp), kind=KARG)
             subv(ends(jp)) = js
             ends(jp) = ends(jp) + 1
          enddo
          jorg = jorg + npcol(js)
       enddo
    else
       ni = product(kaxs(1:citer-1))
       if (jc.lt.citer) ni = ni / nm
       nm = kaxs(citer)
       nx = ns / nm / ni
       do js = 0, ns - 1
          jp = mod(js / ni, nm)
          ends(jp) = ends(jp) + npcol(js)
       enddo
       ends(1:ne-1) = ends(0:ne-2)
       ends(0) = 0
       do je = 1, ne - 1
          ends(je) = ends(je - 1) + ends(je)
       enddo
       write(*, *) ends(0:ne-1)
       stop
    endif
  end subroutine ptx_decode_subv_dd
!!!_  & restore_ptn_data - restore PR8 data to image or intermediate
  subroutine restore_ptn_data_d &
       & (ierr, &
       &  d,    ldata, subv, lsub, props, u, krect, kaxs, kfmt, flag)
    implicit none
    integer,parameter :: KARG=KDBL, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(0:*)
    integer,         intent(in)  :: ldata        ! limit size of of d
    integer,optional,intent(out) :: subv(0:*)
    integer,optional,intent(in)  :: lsub
    integer,optional,intent(out) :: props(*)
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: kaxs(*)
    integer,         intent(in)  :: kfmt
    integer,optional,intent(in)  :: flag

    logical cont

    ierr = 0

    cont = .TRUE.
    call restore_ptn_subv &
         & (ierr, &
         &  subv, lsub, props, u, krect, kaxs, flag)

    if (ierr.eq.0) then
       select case(kfmt)
       case(GFMT_PR8)
          call get_data_drecord(ierr, d, ldata, u, krect, sub=cont)
       case(GFMT_PR4)
          call get_data_frecord(ierr, d, ldata, u, krect, sub=cont)
       case(GFMT_PI4)
          call get_data_irecord(ierr, d, ldata, u, krect, sub=cont)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
  end subroutine restore_ptn_data_d
  subroutine restore_ptn_data_f &
       & (ierr, &
       &  d,    ldata, subv, lsub, props, u, krect, kaxs, kfmt, flag)
    implicit none
    integer,parameter :: KARG=KFLT, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG),    intent(out) :: d(0:*)
    integer,         intent(in)  :: ldata        ! limit size of of d
    integer,optional,intent(out) :: subv(0:*)
    integer,optional,intent(in)  :: lsub
    integer,optional,intent(out) :: props(*)
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: kaxs(*)
    integer,         intent(in)  :: kfmt
    integer,optional,intent(in)  :: flag

    logical cont

    ierr = 0
    cont = .TRUE.
    call restore_ptn_subv &
         & (ierr, &
         &  subv, lsub, props, u, krect, kaxs, flag)

    if (ierr.eq.0) then
       select case(kfmt)
       case(GFMT_PR8)
          call get_data_drecord(ierr, d, ldata, u, krect, sub=cont)
       case(GFMT_PR4)
          call get_data_frecord(ierr, d, ldata, u, krect, sub=cont)
       case(GFMT_PI4)
          call get_data_irecord(ierr, d, ldata, u, krect, sub=cont)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
  end subroutine restore_ptn_data_f
  subroutine restore_ptn_data_i &
       & (ierr, &
       &  d,    ldata, subv, lsub, props, u, krect, kaxs, kfmt, flag)
    implicit none
    integer,parameter :: KARG=KI32, KISRC=KI32, KRSRC=KDBL
    integer,         intent(out) :: ierr
    integer(kind=KARG),    intent(out) :: d(0:*)
    integer,         intent(in)  :: ldata        ! limit size of of d
    integer,optional,intent(out) :: subv(0:*)
    integer,optional,intent(in)  :: lsub
    integer,optional,intent(out) :: props(*)
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: kaxs(*)
    integer,         intent(in)  :: kfmt
    integer,optional,intent(in)  :: flag

    logical cont

    ierr = 0
    cont = .TRUE.
    call restore_ptn_subv &
         & (ierr, &
         &  subv, lsub, props, u, krect, kaxs, flag)

    if (ierr.eq.0) then
       select case(kfmt)
       case(GFMT_PR8)
          call get_data_drecord(ierr, d, ldata, u, krect, sub=cont)
       case(GFMT_PR4)
          call get_data_frecord(ierr, d, ldata, u, krect, sub=cont)
       case(GFMT_PI4)
          call get_data_irecord(ierr, d, ldata, u, krect, sub=cont)
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
  end subroutine restore_ptn_data_i
!!!_  & restore_ptn_subv
  subroutine restore_ptn_subv &
       & (ierr, &
       &  subv, lsub, props, u, krect, kaxs, flag)
    use TOUZA_Trp,only: count_packed, pack_restore
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,parameter :: KISRC=KI32
    integer,         intent(out) :: ierr
    integer,optional,intent(out) :: subv(0:*)
    integer,optional,intent(in)  :: lsub
    integer,optional,intent(out) :: props(*)
    integer,         intent(in)  :: u
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: kaxs(*)
    integer,optional,intent(in)  :: flag

    integer(kind=KISRC),parameter :: mold = 0_KISRC
    integer nbase, npack, ndata
    integer opts(lopts_ptx)
    integer mbits, kpack
    logical cont
    integer f
    integer(kind=KISRC) :: dummy(1)

    ierr = 0
    cont = .TRUE.
    f = choice(0, flag)
    if (ierr.eq.0) call get_data_record(ierr, opts, lopts_ptx, u, krect, sub=cont)
    if (ierr.eq.0) then
       if (present(props)) then
          props(1:lopts_ptx) = opts(1:lopts_ptx)
       endif
    endif
    if (ierr.eq.0) then
       mbits = opts(PROP_PTX_MBITS)
       nbase = ptx_vec_size(kaxs, laxs, opts)
       npack = count_packed(mbits, nbase, mold)
       ndata = opts(PROP_PTX_DATA)
       kpack = opts(PROP_PTX_CODES)
    endif
    if (present(subv)) then
       if (IAND(f, rev_plain_storage).eq.0) then
          if (ierr.eq.0) call alloc_wpack(ierr, npack, 'rst:ptn', mold)
          if (ierr.eq.0) call get_data_record(ierr, wpack, npack, u, krect, sub=cont)
          if (ierr.eq.0) call pack_restore(ierr, subv, wpack, nbase, mbits, kpack)
          if (ierr.eq.0) call ptx_expand_mems(ierr, subv, nbase, opts)
          if (ierr.eq.0) call alloc_wpack(ierr, -1, 'rst:ptn', mold)
       else
          if (ierr.eq.0) call get_data_record(ierr, subv, npack, u, krect, sub=cont)
       endif
    else
       if (ierr.eq.0) call get_data_record(ierr, dummy, npack, u, krect, sub=cont, skip=.TRUE.)
    endif

  end subroutine restore_ptn_subv

!!!_  & ptx_expand_mems
  subroutine ptx_expand_mems (ierr, vbase, nbs, opts)
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: vbase(0:*)
    integer,intent(in)    :: nbs
    integer,intent(in)    :: opts(*)
    integer ofs

    ofs = opts(PROP_PTX_OFFSET)
    ierr = 0
    if (ierr.eq.0) then
       where (vbase(0:nbs-1).gt.0)
          vbase(0:nbs-1) = vbase(0:nbs-1) + ofs
       end where
    endif
    return
  end subroutine ptx_expand_mems

!!!_  & nio_restore_data - read data block (plain)
  subroutine nio_restore_data_d &
       & (ierr, &
       &  d,    ld,    subi, ns, props, &
       &  head, krect, u)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: ld       ! limit size of d
    integer,optional,intent(out) :: subi(*)
    integer,optional,intent(in)  :: ns
    integer,optional,intent(out) :: props(*)
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u

    integer kfmt
    integer kaxs(laxs)
    real(kind=KRMIS) :: vmiss

    ierr = err_default
    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    if (ierr.eq.0) then
       select case (kfmt)
       case (GFMT_PR8, GFMT_PR4, GFMT_PI4)
          call restore_ptn_data(ierr, d, ld, subi, ns, props, u, krect, kaxs, kfmt)
       case default
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       end select
    endif
    return
  end subroutine nio_restore_data_d
  subroutine nio_restore_data_f &
       & (ierr, &
       &  d,    ld,    subi, ns, props, &
       &  head, krect, u)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out) :: ierr
    real(kind=KARG), intent(out) :: d(*)
    integer,         intent(in)  :: ld       ! limit size of d
    integer,optional,intent(out) :: subi(*)
    integer,optional,intent(in)  :: ns
    integer,optional,intent(out) :: props(*)
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u

    integer kfmt
    integer kaxs(laxs)
    real(kind=KRMIS) :: vmiss

    ierr = err_default
    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    if (ierr.eq.0) then
       select case (kfmt)
       case (GFMT_PR8, GFMT_PR4, GFMT_PI4)
          call restore_ptn_data(ierr, d, ld, subi, ns, props, u, krect, kaxs, kfmt)
       case default
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       end select
    endif
    return
  end subroutine nio_restore_data_f
  subroutine nio_restore_data_i &
       & (ierr, &
       &  d,    ld,    subi, ns, props, &
       &  head, krect, u)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: d(*)
    integer,         intent(in)  :: ld       ! limit size of d
    integer,optional,intent(out) :: subi(*)
    integer,optional,intent(in)  :: ns
    integer,optional,intent(out) :: props(*)
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: krect
    integer,         intent(in)  :: u

    integer kfmt
    integer kaxs(laxs)
    real(kind=KRMIS) :: vmiss

    ierr = err_default
    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    if (ierr.eq.0) then
       select case (kfmt)
       case (GFMT_PR8, GFMT_PR4, GFMT_PI4)
          call restore_ptn_data(ierr, d, ld, subi, ns, props, u, krect, kaxs, kfmt)
       case default
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
       end select
    endif
    return
  end subroutine nio_restore_data_i

!!!_ + test_batch_read_packed
  subroutine test_batch_read_packed &
       & (ierr, jarg)
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

    integer(kind=KIOFS) :: jpos
    integer jrec, krect, uread
    character(len=litem) hd(nitem)
    real(kind=KBUF),allocatable :: v(:)
    integer,allocatable :: ends(:), subv(:)
    character(len=litem) :: dfmt, item, item_sv
    integer nmask, ndata, nprop
    integer,parameter :: laxs = 3
    integer :: kaxs(laxs)
    integer jc
    integer niter
    integer jz
    integer jv, jvb, jve
    integer check
    integer idummy(1)
    integer props(32)

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
401 format('header/r:', I0, 1x, I0, 1x, I0, 1x, Z8.8)
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
       inquire(uread, POS=jpos, IOSTAT=ierr)
       ierr = 0
       if (ierr.eq.0) call nio_read_header(ierr, hd, krect, uread)
       write (*, 401) ierr, jrec, krect, jpos - 1
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
          call nio_review_record(ierr, nprop, nmask, ndata, hd, uread, krect)
          if (nprop.ge.0) then
             write(*, *) 'review: ', trim(item), ' ', trim(dfmt), nprop, nmask, ndata
             if (ierr.eq.0) then
                if (ndata.gt.size(v)) then
                   deallocate(v)
                   allocate(v(0:ndata-1), STAT=ierr)
                endif
             endif
             if (ierr.eq.0) then
                if (nmask.gt.size(subv)) then
                   deallocate(subv)
                   allocate(subv(0:nmask), STAT=ierr)
                endif
             endif
             if (ierr.eq.0) then
                call nio_restore_data &
                     & (ierr, &
                     &  v, ndata, subv(1:), nmask, props, hd, krect, uread)
                if (ierr.ne.0) then
                   call nio_skip_records(ierr, 1, uread, head=hd, krect=krect)
                endif
             endif
             ! write(*, *) 'read_packed/mask = ', ierr, check, ndata
             ! write(*, *) 'read_packed/mask = ', ends(1:niter)
          else
             niter = 0
             call nio_skip_records(ierr, 1, uread, head=hd, krect=krect)
          endif
       endif
       if (ierr.eq.0) then
1001      format(A, 1x, I0, 1x, I0, 1x, I0)
1002      format(A, 1x, I0, 1x, I0, 1x, E10.3)
          if (dfmt(2:2).eq.'I') then
             jvb = 0
             do jz = 1, nmask
                jve = jvb + subv(jz)
                do jv = jvb, jve - 1
                   write(*, 1001) trim(item), jz, jv - jvb, int(v(jv))
                enddo
                jvb = jve
             enddo
          else
             jvb = 0
             do jz = 1, nmask
                jve = jvb + subv(jz)
                do jv = jvb, jve - 1
                   write(*, 1002) trim(item), jz, jv - jvb, v(jv)
                enddo
                jvb = jve
             enddo
          endif
       endif
       write(*, *) 'record = ', jrec, ierr
       jrec = jrec + 1
    enddo
    if (ierr.eq.0) call sus_close(ierr, uread, rfile)
    if (ierr.eq.0) deallocate(ends, STAT=ierr)
    return
  end subroutine test_batch_read_packed

!   subroutine test_check_ptx (ierr, jarg)
!     use TOUZA_Trp,only: first_bit
!     implicit none
!     integer,parameter :: KTGT=KDBL
!     integer,intent(out)   :: ierr
!     integer,intent(inout) :: jarg

!     integer,parameter :: ld = 512

!     integer :: npack, ndata
!     integer :: opts(lopts_ptx)
!     integer :: xmem(0:ld)
!     real(kind=KTGT) :: d(0:ld)
!     integer :: nbase
!     integer :: kaxs(3), m
!     real(kind=KRMIS),parameter :: vmiss = 0.0_KRMIS
!     real(kind=KRMIS),parameter :: vdef  = 1.0_KRMIS
!     integer i
!     integer jc, j,  jb
!     integer jx, jm, ji
!     integer nx, nm, ni

!     ierr = 0
!     jarg = jarg + 1
!     if (ierr.eq.0) call get_param(ierr, jc, jarg, 3)

!     kaxs(:) = (/2, 3, 4/)
!     if (ierr.eq.0) call get_option(ierr, kaxs, 'domain', unset=.TRUE.)

!     if (ierr.eq.0) then
!        m = product(kaxs(:))
!        d(0:m-1) = vmiss

!        ni = product(kaxs(1:jc-1))
!        nm = kaxs(jc)
!        nx = m / ni / nm

!        do jx = 0, nx - 1
!           do jm = 0, nm - 1
!              do ji = 0, ni - 1
!                 j = ji + ni * jx
!                 if (mod(j, nm).ge.jm) then
!                    j = ji + ni * (jm + nm * jx)
!                    d(j) = vdef
!                 endif
!              enddo
!           enddo
!        enddo
!        do jx = 0, nx - 1
!           do ji = 0, ni - 1
!              j  = ji + ni * (0  + nm * jx)
!              jm = ji + ni * (nm + nm * jx)
! 101          format('ptx:source: ', I0, 1x, I0, 1x, 20(1x, I0))
!              write(*, 101) ji, jx, int(d(j:jm-1:ni))
!           enddo
!        enddo

!        opts(PROP_PTX_MBITS) = 4

!        opts(PROP_PTX_COOR) = jc
!        nbase = m / kaxs(jc)
!     endif

!     if (ierr.eq.0) then
!        call  ptx_check_vec &
!             & (ierr, npack, opts, xmem, d, nbase, kaxs, vmiss)
!        write(*, *) 'ptx:result:', ierr, jc, npack, ndata
!        do jb = 0, nbase - 1
!           write(*, *) 'ptx:mem:', jc, jb, xmem(jb)
!        enddo
!     endif
!   end subroutine test_check_ptx

#endif /* meta-comment */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

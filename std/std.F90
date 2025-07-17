!!!_! std.F90 - touza/std interfaces
! Maintainer: SAITO Fuyuki
! Created: Jun 4 2020
#define TIME_STAMP 'Time-stamp: <2025/07/17 23:38:37 fuyuki std.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2020-2025
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
!!!_* include
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_std.h"
!!!_@ TOUZA_Std - standard interfaces
module TOUZA_Std
!!!_ + Declaration
!!!_  - modules
  use TOUZA_Std_prc,only: prc_init=>init, prc_diag=>diag, prc_finalize=>finalize, pset_defu=>set_defu
  use TOUZA_Std_utl,only: utl_init=>init, utl_diag=>diag, utl_finalize=>finalize, uset_defu=>set_defu
  use TOUZA_Std_env,only: env_init=>init, env_diag=>diag, env_finalize=>finalize
  use TOUZA_Std_log,only: log_init=>init, log_diag=>diag, log_finalize=>finalize
  use TOUZA_Std_fun,only: fun_init=>init, fun_diag=>diag, fun_finalize=>finalize
  use TOUZA_Std_arg,only: arg_init=>init, arg_diag=>diag, arg_finalize=>finalize
  use TOUZA_Std_mwe,only: mwe_init=>init, mwe_diag=>diag, mwe_finalize=>finalize
  use TOUZA_Std_sus,only: sus_init=>init, sus_diag=>diag, sus_finalize=>finalize
  use TOUZA_Std_bld,only: bld_init=>init, bld_diag=>diag, bld_finalize=>finalize
  use TOUZA_Std_wsh,only: wsh_init=>init, wsh_diag=>diag, wsh_finalize=>finalize
  use TOUZA_Std_htb,only: htb_init=>init, htb_diag=>diag, htb_finalize=>finalize
  use TOUZA_Std_ipc,only: ipc_init=>init, ipc_diag=>diag, ipc_finalize=>finalize

  use TOUZA_Std_prc,only: KFLT, KDBL, KQPL
  use TOUZA_Std_prc,only: KI8,  KI16, KI32, KI64
  use TOUZA_Std_prc,only: check_real_props
  use TOUZA_Std_prc,only: check_real_zero, check_real_one, check_real_inf, check_real_dnm
  use TOUZA_Std_prc,only: check_real_mantissa
  use TOUZA_Std_prc,only: diag_real_props

  use TOUZA_Std_utl,only: choice, choice_a
  use TOUZA_Std_utl,only: set_if_present
  use TOUZA_Std_utl,only: condop, condrep
  use TOUZA_Std_utl,only: chcount
  use TOUZA_Std_utl,only: upcase, downcase, is_symbol
  use TOUZA_Std_utl,only: ndigits
  use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
  use TOUZA_Std_utl,only: control_lev
  use TOUZA_Std_utl,only: parse_number
  use TOUZA_Std_utl,only: compact_format, compact_string
  use TOUZA_Std_utl,only: join_list,  split_list, split_heads
  use TOUZA_Std_utl,only: find_first, find_first_range
  use TOUZA_Std_utl,only: jot
  use TOUZA_Std_utl,only: inrange
  use TOUZA_Std_utl,only: begin_with, find_next_sep
  use TOUZA_Std_utl,only: swap_items
  use TOUZA_Std_utl,only: bisection_find

  use TOUZA_Std_log,only: unit_global
  use TOUZA_Std_log,only: get_logu,   set_defu
  use TOUZA_Std_log,only: is_unit_star
  use TOUZA_Std_log,only: msg,  msg_grp, msg_mdl, msg_fun
  use TOUZA_Std_log,only: gen_tag
  use TOUZA_Std_log,only: is_msglev
  use TOUZA_Std_log,only: is_msglev_anyway,  is_msglev_panic
  use TOUZA_Std_log,only: is_msglev_fatal,   is_msglev_critical, is_msglev_severe
  use TOUZA_Std_log,only: is_msglev_warning, is_msglev_normal,   is_msglev_info
  use TOUZA_Std_log,only: is_msglev_detail,  is_msglev_debug
  use TOUZA_Std_log,only: banner
  use TOUZA_Std_log,only: trace_control,     trace_fine,         trace_err
  use TOUZA_Std_log,only: is_error_match
  use TOUZA_Std_log,only: toggle_flush
  use TOUZA_Std_log,only: msglev_anyway
  use TOUZA_Std_log,only: msglev_panic, msglev_fatal, msglev_critical, msglev_severe
  use TOUZA_Std_log,only: msglev_warning, msglev_normal, msglev_info, msglev_detail, msglev_debug

  use TOUZA_Std_env,only: uin, uout, uerr
  use TOUZA_Std_env,only: init_batch
  use TOUZA_Std_env,only: init_unfmtd_recl, get_size_ufd
  use TOUZA_Std_env,only: init_unfmtd_strm, get_unit_strm, get_size_strm
  use TOUZA_Std_env,only: init_file_bodr,   check_byte_order, check_bodr_unit
  use TOUZA_Std_env,only: init_io_status,   is_eof_ss
  use TOUZA_Std_env,only: get_size_bytes,   conv_b2strm,   get_mems_bytes
  use TOUZA_Std_env,only: get_login_name,   get_host_name
  use TOUZA_Std_env,only: is_new_line,      new_line_ascii
  use TOUZA_Std_env,only: KIOFS
  use TOUZA_Std_env,only: endian_UNKNOWN, endian_ERROR, endian_BIG, endian_LITTLE, endian_OTHER
  use TOUZA_Std_env,only: kendi_mem, kendi_file
  use TOUZA_Std_env,only: lpath
  use TOUZA_Std_env,only: nb_recl, nc_strm
  use TOUZA_Std_env,only: nbits_byte

  use TOUZA_Std_mwe,only: set_comm, get_comm
  use TOUZA_Std_mwe,only: get_ni,   get_ni_safe
  use TOUZA_Std_mwe,only: get_wni,  get_wni_safe
  use TOUZA_Std_mwe,only: get_gni
  use TOUZA_Std_mwe,only: comp_comms, comp_groups
  use TOUZA_Std_mwe,only: is_mpi_activated
  use TOUZA_Std_mwe,only: safe_mpi_init, safe_mpi_finalize
  use TOUZA_Std_mwe,only: show_mpi_type
  use TOUZA_Std_mwe,only: MPI_SUCCESS
  use TOUZA_Std_mwe,only: MPI_COMM_WORLD, MPI_COMM_SELF, MPI_COMM_NULL
  use TOUZA_Std_mwe,only: MPI_DATATYPE_NULL, MPI_GROUP_NULL, MPI_UNDEFINED
  use TOUZA_Std_mwe,only: MPI_STATUS_SIZE,   MPI_GROUP_EMPTY
  use TOUZA_Std_mwe,only: MPI_INTEGER,       MPI_CHARACTER
  use TOUZA_Std_mwe,only: MPI_MIN,           MPI_MAX
  use TOUZA_Std_mwe,only: MPI_ANY_TAG,       MPI_ANY_SOURCE
  use TOUZA_Std_mwe,only: MPI_DOUBLE_PRECISION
  use TOUZA_Std_mwe,only: MPI_GROUP_TRANSLATE_RANKS, MPI_GROUP_SIZE, MPI_GROUP_RANK, MPI_GROUP_UNION
  use TOUZA_Std_mwe,only: MPI_COMM_CREATE,   MPI_COMM_SPLIT, MPI_COMM_GROUP
  use TOUZA_Std_mwe,only: MPI_COMM_SIZE,     MPI_COMM_RANK
  use TOUZA_Std_mwe,only: MPI_WAIT, MPI_BARRIER
  use TOUZA_Std_mwe,only: MPI_PROBE, MPI_GET_COUNT
  use TOUZA_Std_mwe,only: MPI_COMM_COMPARE, MPI_IDENT, MPI_CONGRUENT, MPI_SIMILAR, MPI_UNEQUAL
  use TOUZA_Std_mwe,only: MPI_GROUP_COMPARE
  use TOUZA_Std_mwe,only: MPI_ABORT
  use TOUZA_Std_mwe,only: cc_ident, cc_congruent, cc_similar, cc_both_null, cc_unequal, cc_either_null

  use TOUZA_Std_fun,only: set_category_bound, set_category_default
  use TOUZA_Std_fun,only: brute_force_check_units
  use TOUZA_Std_fun,only: new_unit
  use TOUZA_Std_fun,only: new_unit_tmp, set_tempfile
  use TOUZA_Std_fun,only: set_tmptmpl
  use TOUZA_Std_fun,only: is_file_exist, is_unit_opened, is_file_opened
  use TOUZA_Std_fun,only: search_from_head, search_from_last, search_from_next
  use TOUZA_Std_fun,only: kucat_black

  use TOUZA_Std_arg,only: decl_pos_arg
  use TOUZA_Std_arg,only: parse
  use TOUZA_Std_arg,only: get_cmd
  use TOUZA_Std_arg,only: get_nparam, get_nargs
  use TOUZA_Std_arg,only: get_param, get_array, get_option
  use TOUZA_Std_arg,only: get_arg,   get_key,   get_value,  get_entry
  use TOUZA_Std_arg,only: count_entries, query_nth_entry, forward_entry
  use TOUZA_Std_arg,only: parse_param
  use TOUZA_Std_arg,only: inq_end_flags
  use TOUZA_Std_arg,only: check_param
  use TOUZA_Std_arg,only: cmdline_count_wrap, cmdline_arg_wrap

  use TOUZA_Std_htb,only: new_htable,   new_wtable
  use TOUZA_Std_htb,only: diag_htable,  diag_wtable
  use TOUZA_Std_htb,only: new_entry,    reg_entry,    settle_entry
  use TOUZA_Std_htb,only: query_entry,  query_key,    query_status, query_status_entr
  use TOUZA_Std_htb,only: load_status,  save_status
  use TOUZA_Std_htb,only: reg_item,     reg_alias,    search_item
  use TOUZA_Std_htb,only: check_handle, check_index,  get_size_items
  use TOUZA_Std_htb,only: normalize,    watermark
  use TOUZA_Std_htb,only: get_keys
  use TOUZA_Std_htb,only: eundef, unset, flag_default, flag_ignore

  use TOUZA_Std_ipc,only: ipc_IBITS,  exam_IBITS
  use TOUZA_Std_ipc,only: ipc_HYPOT
  use TOUZA_Std_ipc,only: ipc_ASINH,  ipc_ACOSH, ipc_ATANH
  use TOUZA_Std_ipc,only: ipc_ETIME
  use TOUZA_Std_ipc,only: ipc_GETCWD, ipc_CHDIR
  use TOUZA_Std_ipc,only: ipc_EXIT

  use TOUZA_Std_sus,only: sus_open, sus_close
  use TOUZA_Std_sus,only: sus_spec_form, sus_spec_action, sus_spec_status, sus_spec_position
  use TOUZA_Std_sus,only: sus_is_status_new
  use TOUZA_Std_sus,only: sus_write_irec, sus_read_irec, sus_skip_irec, sus_check_irec
  use TOUZA_Std_sus,only: sus_write_lrec, sus_read_lrec, sus_skip_lrec, sus_check_lrec
  use TOUZA_Std_sus,only: sus_pad_irec,   sus_blank_irec
  use TOUZA_Std_sus,only: sus_suspend_write_irec, sus_edit_slice_irec
  use TOUZA_Std_sus,only: sus_suspend_read_irec,  sus_slice_read_irec
  use TOUZA_Std_sus,only: sus_runl_read_irec,     sus_list_read_irec
  use TOUZA_Std_sus,only: sus_write_isep, sus_read_isep, sus_read
  use TOUZA_Std_sus,only: sus_write_lsep, sus_read_lsep, sus_write
  use TOUZA_Std_sus,only: sus_rseek, sus_getpos
  use TOUZA_Std_sus,only: sus_pos_a2rel, sus_pos_r2abs, sus_rel_pos
  use TOUZA_Std_sus,only: sus_eswap
  use TOUZA_Std_sus,only: sus_swap, sus_eswap_hl
  use TOUZA_Std_sus,only: sus_pad
  use TOUZA_Std_sus,only: sus_record_mems_irec
  use TOUZA_Std_sus,only: max_members, is_irec_overflow, is_irec_overflow_mix
  use TOUZA_Std_sus,only: sus_size_irec
  use TOUZA_Std_sus,only: sus_is_stream_unit
  use TOUZA_Std_sus,only: debug_status
  use TOUZA_Std_sus,only: set_slice_loop, init_offset, next_offset
  use TOUZA_Std_sus,only: set_runl_loop
  use TOUZA_Std_sus,only: WHENCE_BEGIN, WHENCE_CURRENT, WHENCE_END, WHENCE_ABS
  use TOUZA_Std_sus,only: def_block, ignore_small, ignore_bigger, ignore_always
  use TOUZA_Std_sus,only: suspend_begin, suspend_mid, suspend_end

!!!_  - default
  implicit none
  private
!!!_  - static
#define __GRP__ GROUP_TAG
  integer,save,private :: init_mode = 0
  integer,save,private :: init_counts = 0
  integer,save,private :: diag_counts = 0
  integer,save,private :: fine_counts = 0
  integer,save,private :: lev_verbose = 0
  integer,save,private :: err_default = ERR_NO_INIT
  integer,save,private :: ulog = unit_global

  character(len=64),save,private :: tagmsg = ' '
!!!_  - public
  public :: init, diag, finalize

  public :: prc_init, prc_diag, prc_finalize
  public :: utl_init, utl_diag, utl_finalize
  public :: env_init, env_diag, env_finalize
  public :: log_init, log_diag, log_finalize
  public :: fun_init, fun_diag, fun_finalize
  public :: arg_init, arg_diag, arg_finalize
  public :: mwe_init, mwe_diag, mwe_finalize
  public :: sus_init, sus_diag, sus_finalize
  public :: bld_init, bld_diag, bld_finalize
  public :: wsh_init, wsh_diag, wsh_finalize
  public :: htb_init, htb_diag, htb_finalize
  public :: ipc_init, ipc_diag, ipc_finalize

  public :: KFLT, KDBL, KQPL
  public :: KI8,  KI16, KI32, KI64
  public :: check_real_props
  public :: check_real_zero, check_real_one, check_real_inf, check_real_dnm
  public :: check_real_mantissa
  public :: diag_real_props

  public :: choice, choice_a
  public :: set_if_present
  public :: condop, condrep
  public :: chcount
  public :: upcase, downcase, is_symbol
  public :: ndigits
  public :: control_mode, control_deep, is_first_force
  public :: control_lev
  public :: parse_number
  public :: compact_format, compact_string
  public :: join_list,  split_list, split_heads
  public :: find_first, find_first_range
  public :: jot
  public :: inrange
  public :: begin_with, find_next_sep
  public :: swap_items
  public :: bisection_find

  public :: unit_global
  public :: get_logu,   set_defu
  public :: is_unit_star
  public :: msg,  msg_grp, msg_mdl, msg_fun
  public :: gen_tag
  public :: is_msglev
  public :: is_msglev_anyway,  is_msglev_panic
  public :: is_msglev_fatal,   is_msglev_critical, is_msglev_severe
  public :: is_msglev_warning, is_msglev_normal,   is_msglev_info
  public :: is_msglev_detail,  is_msglev_debug
  public :: banner
  public :: trace_control,     trace_fine,         trace_err
  public :: is_error_match
  public :: toggle_flush
  public :: msglev_anyway
  public :: msglev_panic, msglev_fatal, msglev_critical, msglev_severe
  public :: msglev_warning, msglev_normal, msglev_info, msglev_detail, msglev_debug

  public :: init_batch
  public :: init_unfmtd_recl, get_size_ufd
  public :: init_unfmtd_strm, get_unit_strm, get_size_strm
  public :: init_file_bodr,   check_byte_order, check_bodr_unit
  public :: init_io_status,   is_eof_ss
  public :: get_size_bytes,   conv_b2strm,   get_mems_bytes
  public :: get_login_name,   get_host_name
  public :: is_new_line,      new_line_ascii
  public :: KIOFS
  public :: endian_UNKNOWN, endian_ERROR, endian_BIG, endian_LITTLE, endian_OTHER
  public :: kendi_mem, kendi_file
  public :: uin, uout, uerr
  public :: lpath
  public :: nb_recl, nc_strm
  public :: nbits_byte

  public :: set_comm, get_comm
  public :: get_ni,   get_ni_safe
  public :: get_wni,  get_wni_safe
  public :: get_gni
  public :: comp_comms, comp_groups
  public :: is_mpi_activated
  public :: safe_mpi_init, safe_mpi_finalize
  public :: show_mpi_type
  public :: MPI_SUCCESS
  public :: MPI_COMM_WORLD, MPI_COMM_SELF, MPI_COMM_NULL
  public :: MPI_DATATYPE_NULL, MPI_GROUP_NULL, MPI_UNDEFINED
  public :: MPI_STATUS_SIZE,   MPI_GROUP_EMPTY
  public :: MPI_INTEGER,       MPI_CHARACTER
  public :: MPI_MIN,           MPI_MAX
  public :: MPI_ANY_TAG,       MPI_ANY_SOURCE
  public :: MPI_DOUBLE_PRECISION
  public :: MPI_GROUP_TRANSLATE_RANKS, MPI_GROUP_SIZE, MPI_GROUP_RANK, MPI_GROUP_UNION
  public :: MPI_COMM_CREATE,   MPI_COMM_SPLIT, MPI_COMM_GROUP
  public :: MPI_COMM_SIZE,     MPI_COMM_RANK
  public :: MPI_WAIT, MPI_BARRIER
  public :: MPI_PROBE, MPI_GET_COUNT
  public :: MPI_COMM_COMPARE, MPI_IDENT, MPI_CONGRUENT, MPI_SIMILAR, MPI_UNEQUAL
  public :: MPI_GROUP_COMPARE
  public :: MPI_ABORT
  public :: cc_ident, cc_congruent, cc_similar, cc_both_null, cc_unequal, cc_either_null

  public :: set_category_bound, set_category_default
  public :: brute_force_check_units
  public :: new_unit
  public :: new_unit_tmp, set_tempfile
  public :: set_tmptmpl
  public :: is_file_exist, is_unit_opened, is_file_opened
  public :: search_from_head, search_from_last, search_from_next
  public :: kucat_black

  public :: decl_pos_arg
  public :: parse
  public :: get_cmd
  public :: get_nparam, get_nargs
  public :: get_param, get_array, get_option
  public :: get_arg,   get_key,   get_value,  get_entry
  public :: count_entries, query_nth_entry, forward_entry
  public :: parse_param
  public :: inq_end_flags
  public :: check_param
  public :: cmdline_count_wrap, cmdline_arg_wrap

  public :: new_htable,   new_wtable
  public :: diag_htable,  diag_wtable
  public :: new_entry,    reg_entry,    settle_entry
  public :: query_entry,  query_key,    query_status, query_status_entr
  public :: load_status,  save_status
  public :: reg_item,     reg_alias,    search_item
  public :: check_handle, check_index,  get_size_items
  public :: normalize,    watermark
  public :: get_keys
  public :: eundef, unset, flag_default, flag_ignore

  public :: ipc_IBITS,  exam_IBITS
  public :: ipc_HYPOT
  public :: ipc_ASINH,  ipc_ACOSH, ipc_ATANH
  public :: ipc_ETIME
  public :: ipc_GETCWD, ipc_CHDIR
  public :: ipc_EXIT

  public :: sus_open, sus_close
  public :: sus_spec_form, sus_spec_action, sus_spec_status, sus_spec_position
  public :: sus_is_status_new
  public :: sus_write_irec, sus_read_irec, sus_skip_irec, sus_check_irec
  public :: sus_write_lrec, sus_read_lrec, sus_skip_lrec, sus_check_lrec
  public :: sus_pad_irec,   sus_blank_irec
  public :: sus_suspend_write_irec, sus_edit_slice_irec
  public :: sus_suspend_read_irec,  sus_slice_read_irec
  public :: sus_runl_read_irec,     sus_list_read_irec
  public :: sus_write_isep, sus_read_isep, sus_read
  public :: sus_write_lsep, sus_read_lsep, sus_write
  public :: sus_rseek, sus_getpos
  public :: sus_pos_a2rel, sus_pos_r2abs, sus_rel_pos
  public :: sus_eswap
  public :: sus_swap, sus_eswap_hl
  public :: sus_pad
  public :: sus_record_mems_irec
  public :: max_members, is_irec_overflow, is_irec_overflow_mix
  public :: sus_size_irec
  public :: sus_is_stream_unit
  public :: debug_status
  public :: set_slice_loop, init_offset, next_offset
  public :: set_runl_loop
  public :: WHENCE_BEGIN, WHENCE_CURRENT, WHENCE_END, WHENCE_ABS
  public :: def_block, ignore_small, ignore_bigger, ignore_always
  public :: suspend_begin, suspend_mid, suspend_end

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init &
       & (ierr,   &
       &  u,      levv, mode, &
       &  envtry, icomm)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode

    integer,intent(in),optional :: envtry
    integer,intent(in),optional :: icomm

    integer lv, md, lmd
    integer chmd

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
          chmd = MODE_SURFACE
          if (ierr.eq.0) call prc_init(ierr, u=ulog, levv=lv, mode=lmd)

          if (ierr.eq.0) call utl_init(ierr, u=ulog, levv=lv, mode=chmd) ! prc

          if (ierr.eq.0) call wsh_init(ierr, u=ulog, levv=lv, mode=chmd) ! prc utl
          if (ierr.eq.0) call log_init(ierr, u=ulog, levv=lv, mode=chmd) ! prc utl

          if (ierr.eq.0) call mwe_init(ierr, u=ulog, levv=lv, mode=chmd, icomm=icomm) ! utl log
          if (ierr.eq.0) call bld_init(ierr, u=ulog, levv=lv, mode=chmd) ! utl log
          if (ierr.eq.0) call htb_init(ierr, u=ulog, levv=lv, mode=chmd) ! utl log

          if (ierr.eq.0) call ipc_init(ierr, u=ulog, levv=lv, mode=chmd) ! prc utl log

          if (ierr.eq.0) call fun_init(ierr, u=ulog, levv=lv, mode=chmd) ! utl log mwe

          if (ierr.eq.0) call env_init(ierr, u=ulog, levv=lv, mode=chmd, levtry=envtry) ! prc utl log fun mwe

          if (ierr.eq.0) call sus_init(ierr, u=ulog, levv=lv, mode=chmd) ! prc utl log fun env
          if (ierr.eq.0) call arg_init(ierr, u=ulog, levv=lv, mode=chmd) ! prc utl log fun env
       endif
       if (is_first_force(init_counts, mode)) then
          call gen_tag(tagmsg, pkg=PACKAGE_TAG, grp=__GRP__)
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
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: u
    integer utmp, lv, md, lmd
    integer chmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(uout, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (VCHECK_NORMAL(lv)) call msg(TIME_STAMP, tagmsg, u=utmp)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          chmd = MODE_SURFACE
          if (ierr.eq.0) call prc_diag(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call utl_diag(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call wsh_diag(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call log_diag(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call mwe_diag(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call bld_diag(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call htb_diag(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call ipc_diag(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call fun_diag(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call env_diag(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call sus_diag(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call arg_diag(ierr, utmp, lv, mode=chmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag
!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: u
    integer utmp, lv, md, lmd
    integer chmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(uout, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          chmd = MODE_SURFACE
          if (ierr.eq.0) call prc_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call utl_finalize(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call wsh_finalize(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call log_finalize(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call mwe_finalize(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call bld_finalize(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call htb_finalize(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call ipc_finalize(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call fun_finalize(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call env_finalize(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call sus_finalize(ierr, utmp, lv, mode=chmd)
          if (ierr.eq.0) call arg_finalize(ierr, utmp, lv, mode=chmd)
       endif
       fine_counts = fine_counts + 1
    endif

    return
  end subroutine finalize
end module TOUZA_Std
!!!_@ test_std - test program
#ifdef TEST_STD
program test_std
  use TOUZA_Std
  implicit none
  character(len=128) :: C
  character(len=128) :: T
  integer ierr, levv

  call get_command(C, STATUS=ierr)
  if (ierr.ne.0) C = '(unknown)'
  call get_command_argument(1, T, STATUS=ierr)
  if (ierr.eq.0) then
     read(T, *) levv
  else
     levv = -999
  endif

121 format('Usage: ', A, ' [<levv>]')
111 format('TEST: levv=', I0)
101 format(A, ' = ', I0)
  ierr = 0
  write(*, 121) trim(C)
  write(*, 111) levv
  if (ierr.eq.0) call init(ierr, levv=levv)
  write(*, 101) 'init:0', ierr
  if (ierr.eq.0) call init(ierr, levv=levv)
  write(*, 101) 'init:1', ierr
  if (ierr.eq.0) call diag(ierr)
  write(*, 101) 'diag', ierr
  if (ierr.eq.0) call finalize(ierr)
  write(*, 101) 'fine', ierr
  stop
end program test_std

#endif /* TEST_STD */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

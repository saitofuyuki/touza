!!!_! std_sus.F90 - TOUZA/Std stream i/o to emulate unformatted sequential access
! Maintainer: SAITO Fuyuki
! Transferred: Dec 24 2021
! Created: Oct 17 2021 (nng_io)
#define TIME_STAMP 'Time-stamp: <2023/03/20 21:41:40 fuyuki std_sus.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021,2022,2023
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif

#ifndef   TEST_STD_SUS
#  define TEST_STD_SUS 0
#endif
#if       TEST_STD_SUS == 2
#  define OPT_RECL_MAX_BYTES 26    /* 8m + 2 */
#endif

#include "touza_std.h"
#if    HAVE_FORTRAN_OPEN_STREAM
#else
#      error "stream access unavailable"
#endif
#if    HAVE_FORTRAN_INQUIRE_POS
#else
#      error "inquire position specifier unavailable"
#endif
#ifndef    OPT_PROHIBIT_AUTO_WORKAROUND
#  define  OPT_PROHIBIT_AUTO_WORKAROUND 0
#endif
#if  OPT_PROHIBIT_AUTO_WORKAROUND
#else
#  if __NEC__
#    if __NEC_VERSION__ <= 30201
#      ifndef OPT_STREAM_RPOS_WORKAROUND
#        warning "OPT_STREAM_RPOS_WORKAROUND enabled automatic."
#        define OPT_STREAM_RPOS_WORKAROUND 1
#      endif
#    endif
#  endif
#endif
#ifndef    OPT_STREAM_RPOS_WORKAROUND
#  define  OPT_STREAM_RPOS_WORKAROUND 0   /* workaround to seek position (read) */
#endif
! #ifndef    OPT_STREAM_WPOS_WORKAROUND
! #  define  OPT_STREAM_WPOS_WORKAROUND 0   /* workaround to seek position (write) */
! #endif
#ifndef    OPT_ENABLE_LONG_RECORD
#  define  OPT_ENABLE_LONG_RECORD 1 /* 64-bit subrecord markers */
#endif
#ifndef   OPT_READ_SWAP_WITH_WORK
#  define OPT_READ_SWAP_WITH_WORK 1 /* automatic work-array is used for swap */
#endif
#ifndef   OPT_RECL_MAX_BYTES
#  define OPT_RECL_MAX_BYTES 0
#endif
!!!_@ TOUZA_Std_sus - TOUZA sequential access by stream i/o interfaces
module TOUZA_Std_sus
!!!_ = declaration
  use TOUZA_Std_prc,only: KI32, KI64, KFLT, KDBL
  use TOUZA_Std_env,only: KIOFS, LBU=>nbits_byte
  use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
  use TOUZA_Std_log,only: unit_global,  trace_fine,   trace_control
  implicit none
  private
!!!_  - public parameters
  integer,parameter,public :: WHENCE_BEGIN = -1
  integer,parameter,public :: WHENCE_CURRENT = 0
  integer,parameter,public :: WHENCE_END = +1
  integer,parameter,public :: WHENCE_ABS = -99

#if OPT_RECL_MAX_BYTES <= 0
  ! confirmed under gcc sx-aurora
  integer,parameter,public :: RECL_MAX_BYTES = HUGE(0_KI32) - 4 * 2
#else
  integer,parameter,public :: RECL_MAX_BYTES = OPT_RECL_MAX_BYTES
#endif
  ! integer,parameter,public :: RECL_MAX_BYTES = 24

  integer,parameter,public :: def_block      = 0
  integer,parameter,public :: ignore_small   = -1
  integer,parameter,public :: ignore_bigger  = -2
  integer,parameter,public :: ignore_always  = -3

  integer,parameter,public :: suspend_begin = +1  ! suspend-mode record begin
  integer,parameter,public :: suspend_mid   = 0
  integer,parameter,public :: suspend_end   = -1
!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
#define __MDL__ 'sus'
#define __TAG__ STD_FORMAT_MDL(__MDL__)
#define _ERROR(E) (E - ERR_MASK_STD_SUS)

  integer,save :: mstrm_isep = 0, mstrm_lsep = 0   !! separator sizes in stream unit

  integer,save :: maxmemi_i = 0, maxmemi_l = 0 !! max members in a sub-record (32-bit marker)
  integer,save :: maxmemi_f = 0, maxmemi_d = 0

  integer,save,public :: RECL_MAX_STREAM = 0

  integer(kind=KI32),save :: lsubr = 0

  integer,save :: last_iostat = 0
  integer,save :: last_line = 0

  integer,            save :: suspend_wu = -1        !! cached unit for suspend mode
  integer,            save :: suspend_wsub = 0       !! cached unit record length in bytes
  integer(kind=KIOFS),save :: suspend_wposh = -1     !! cached record header position for suspend mode

  integer,            save :: suspend_ru = -1        !! cached unit for suspend mode
  integer(kind=KI32), save :: suspend_rseph = 0      !! cached unit record length in bytes
  integer(kind=KIOFS),save :: suspend_rposf = -1     !! cached record footer position for suspend mode

!!!_  - interfaces
  interface sus_write_irec
     module procedure sus_write_irec_i, sus_write_irec_l, sus_write_irec_d, sus_write_irec_f, sus_write_irec_a
  end interface sus_write_irec
  interface sus_write_lrec
     module procedure sus_write_lrec_i, sus_write_lrec_l, sus_write_lrec_d, sus_write_lrec_f, sus_write_lrec_a
  end interface sus_write_lrec

  interface sus_pad_irec
     module procedure sus_pad_irec_i, sus_pad_irec_l, sus_pad_irec_d, sus_pad_irec_f, sus_pad_irec_a
  end interface sus_pad_irec
  interface sus_blank_irec
     module procedure sus_blank_irec_i, sus_blank_irec_l, sus_blank_irec_d, sus_blank_irec_f, sus_blank_irec_a
  end interface sus_blank_irec

  interface sus_suspend_write_irec
     module procedure sus_suspend_write_irec_i, sus_suspend_write_irec_l
     module procedure sus_suspend_write_irec_f, sus_suspend_write_irec_d, sus_suspend_write_irec_a
  end interface sus_suspend_write_irec
  interface sus_suspend_read_irec
     module procedure sus_suspend_read_irec_i, sus_suspend_read_irec_l
     module procedure sus_suspend_read_irec_f, sus_suspend_read_irec_d, sus_suspend_read_irec_a
  end interface sus_suspend_read_irec

  interface sus_read_irec
     module procedure sus_read_irec_i, sus_read_irec_l, sus_read_irec_d, sus_read_irec_f, sus_read_irec_a
  end interface sus_read_irec
  interface sus_read_lrec
     module procedure sus_read_lrec_i, sus_read_lrec_l, sus_read_lrec_d, sus_read_lrec_f, sus_read_lrec_a
  end interface sus_read_lrec

  interface sus_slice_read_irec
     module procedure sus_slice_read_irec_i, sus_slice_read_irec_l
     module procedure sus_slice_read_irec_d, sus_slice_read_irec_f, sus_slice_read_irec_a
  end interface sus_slice_read_irec

  interface sus_write_isep
     module procedure sus_write_isep_l, sus_write_isep_i
  end interface sus_write_isep
  interface sus_write_lsep
     module procedure sus_write_lsep_l, sus_write_lsep_i
  end interface sus_write_lsep

  interface sus_edit_slice_irec
     module procedure sus_edit_slice_irec_i, sus_edit_slice_irec_l
     module procedure sus_edit_slice_irec_d, sus_edit_slice_irec_f, sus_edit_slice_irec_a
  end interface sus_edit_slice_irec

  interface sus_runl_read_irec
     module procedure sus_runl_read_irec_i, sus_runl_read_irec_l
     module procedure sus_runl_read_irec_f, sus_runl_read_irec_d, sus_runl_read_irec_a
  end interface sus_runl_read_irec
  interface sus_list_read_irec
     module procedure sus_list_read_irec_i, sus_list_read_irec_l
     module procedure sus_list_read_irec_f, sus_list_read_irec_d, sus_list_read_irec_a
  end interface sus_list_read_irec

  interface sus_read_isep
     module procedure sus_read_isep_l, sus_read_isep_i
  end interface sus_read_isep
  interface sus_read_lsep
     module procedure sus_read_lsep_l, sus_read_lsep_i
  end interface sus_read_lsep

  interface sus_record_mems_irec
     module procedure sus_record_mems_irec_i, sus_record_mems_irec_l
     module procedure sus_record_mems_irec_f, sus_record_mems_irec_d
  end interface sus_record_mems_irec

  interface sus_write
     module procedure sus_write_i, sus_write_l, sus_write_d, sus_write_f, sus_write_a
  end interface sus_write

  interface sus_pad
     module procedure sus_pad_i, sus_pad_l, sus_pad_d, sus_pad_f, sus_pad_a
  end interface sus_pad

  interface sus_read
     module procedure sus_read_i, sus_read_l, sus_read_d, sus_read_f, sus_read_a
  end interface sus_read

  interface sus_eswap
     module procedure sus_eswap_i, sus_eswap_l
  end interface sus_eswap
  interface sus_swap
     module procedure sus_swap_i, sus_swap_l
  end interface sus_swap

  interface mstrm_sep
     module procedure mstrm_sep_i, mstrm_sep_l
  end interface mstrm_sep

  interface is_irec_overflow
     module procedure is_irec_overflow_i, is_irec_overflow_l, is_irec_overflow_a
     module procedure is_irec_overflow_f, is_irec_overflow_d
  end interface is_irec_overflow

  interface max_members
     module procedure max_members_a, max_members_i, max_members_l, max_members_f, max_members_d
  end interface max_members
  interface rest_members
     module procedure rest_members_a, rest_members_i, rest_members_l, rest_members_f, rest_members_d
  end interface rest_members

  interface check_dummy_irec
     module procedure check_dummy_irec_i, check_dummy_irec_l
  end interface check_dummy_irec

  interface sus_size_irec
     module procedure sus_size_irec_la,  sus_size_irec_a
     module procedure sus_size_irec_li,  sus_size_irec_i
     module procedure sus_size_irec_ll,  sus_size_irec_l
     module procedure sus_size_irec_lf,  sus_size_irec_f
     module procedure sus_size_irec_ld,  sus_size_irec_d
  end interface sus_size_irec

  interface sus_write_iset
     module procedure sus_write_iset_i, sus_write_iset_l
     module procedure sus_write_iset_d, sus_write_iset_f, sus_write_iset_a
  end interface sus_write_iset
  interface sus_pad_iset
     module procedure sus_pad_iset_i, sus_pad_iset_l
     module procedure sus_pad_iset_d, sus_pad_iset_f, sus_pad_iset_a
  end interface sus_pad_iset
!!!_  - public procedures
  public init, diag, finalize
  public sus_open, sus_close
  public sus_spec_form, sus_spec_action, sus_spec_status, sus_spec_position
  public sus_is_status_new

  public sus_write_irec, sus_read_irec, sus_skip_irec, sus_check_irec
  public sus_write_lrec, sus_read_lrec, sus_skip_lrec, sus_check_lrec
  public sus_pad_irec,   sus_blank_irec
  public sus_suspend_write_irec, sus_edit_slice_irec
  public sus_suspend_read_irec,  sus_slice_read_irec
  public sus_runl_read_irec,     sus_list_read_irec

  public sus_write_isep, sus_read_isep, sus_read
  public sus_write_lsep, sus_read_lsep, sus_write

  public sus_rseek, sus_getpos
  public sus_pos_a2rel, sus_pos_r2abs
  public sus_eswap
  public sus_pad
  public sus_record_mems_irec
  public max_members, is_irec_overflow, is_irec_overflow_mix
  public sus_size_irec
  public sus_is_stream_unit

#if TEST_STD_SUS
  public set_slice_loop, init_offset, next_offset
#endif
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, icomm)
    use TOUZA_Std_prc,only: prc_init=>init
    use TOUZA_Std_utl,only: utl_init=>init, choice
    use TOUZA_Std_fun,only: fun_init=>init
    use TOUZA_Std_log,only: log_init=>init
    use TOUZA_Std_env,only: env_init=>init, get_unit_strm, get_size_bytes, conv_b2strm
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
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
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call prc_init(ierr, ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call utl_init(ierr, ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call log_init(ierr, ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call fun_init(ierr, ulog, levv=lv, mode=lmd, icomm=icomm)
          if (ierr.eq.0) call env_init(ierr, ulog, levv=lv, mode=lmd, icomm=icomm)
       endif
       if (is_first_force(init_counts, mode)) then
          ! if (ierr.eq.0) call sus_check_kinds_literal(ierr, ulog)
          if (ierr.eq.0) then
             call sus_check_envs(ierr, ulog, levv=lv, icomm=icomm)
          endif
          if (ierr.eq.0) call sus_check_stream_pos(ierr)
          if (ierr.eq.0) then
             mstrm_isep = get_unit_strm(0_KI32)
             mstrm_lsep = get_unit_strm(0_KI64)
          endif
          if (ierr.eq.0) then
             lsubr = RECL_MAX_BYTES
             maxmemi_i = lsubr / get_size_bytes(0_KI32)
             maxmemi_l = lsubr / get_size_bytes(0_KI64)
             maxmemi_f = lsubr / get_size_bytes(0.0_KFLT)
             maxmemi_d = lsubr / get_size_bytes(0.0_KDBL)
             if (maxmemi_i.le.0) ierr = _ERROR(ERR_FATAL)
             if (maxmemi_l.le.0) ierr = _ERROR(ERR_FATAL)
             if (maxmemi_f.le.0) ierr = _ERROR(ERR_FATAL)
             if (maxmemi_d.le.0) ierr = _ERROR(ERR_FATAL)
             if (ierr.ne.0) then
                write(*,*) 'FATAL:', maxmemi_i, maxmemi_l, maxmemi_f, maxmemi_d
             endif
             if (ierr.eq.0) RECL_MAX_STREAM = conv_b2strm(RECL_MAX_BYTES)
          endif
       endif
       if (ierr.ne.0) err_default = _ERROR(ERR_FAILURE_INIT)
       init_counts = init_counts + 1
    endif

    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std_prc,only: prc_diag=>diag
    use TOUZA_Std_utl,only: utl_diag=>diag, choice
    use TOUZA_Std_fun,only: fun_diag=>diag
    use TOUZA_Std_log,only: log_diag=>diag, msg_mdl, is_msglev_normal
    use TOUZA_Std_env,only: env_diag=>diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (VCHECK_NORMAL(lv)) then
                call msg_mdl(TIME_STAMP, __MDL__, utmp)
                call msg_mdl('(''maximum record size = '', I0, 1x, I0)', &
                     & (/OPT_RECL_MAX_BYTES, lsubr/), __MDL__, utmp)
             endif
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call prc_diag(ierr, utmp, levv=lv, mode=md)
          if (ierr.eq.0) call utl_diag(ierr, utmp, levv=lv, mode=md)
          if (ierr.eq.0) call log_diag(ierr, utmp, levv=lv, mode=md)
          if (ierr.eq.0) call fun_diag(ierr, utmp, levv=lv, mode=md)
          if (ierr.eq.0) call env_diag(ierr, utmp, levv=lv, mode=md)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_prc,only: prc_finalize=>finalize
    use TOUZA_Std_utl,only: utl_finalize=>finalize, choice
    use TOUZA_Std_fun,only: fun_finalize=>finalize
    use TOUZA_Std_log,only: log_finalize=>finalize, msg, is_msglev_info
    use TOUZA_Std_env,only: env_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call prc_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call utl_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call log_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call fun_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call env_finalize(ierr, utmp, levv=lv, mode=lmd)
       endif
       if (is_msglev_info(lv)) then
          call msg('(''final iostat saved = '', I0, 1x, I0)', &
               &   (/last_iostat, last_line/), __MDL__, u)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_  - init subcontracts
!!!_   & sus_check_envs - on-demand environment checker (std_env dispatcher)
  subroutine sus_check_envs &
       & (ierr, ulog, levv, levtry, iroot, icomm)
    use TOUZA_Std_env,only: init_unfmtd_recl, init_unfmtd_strm
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: ulog
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: levtry
    integer,intent(in),optional :: iroot, icomm
    ierr = 0
    if (ierr.eq.0) call init_unfmtd_recl(ierr, ulog, levv, levtry, iroot, icomm)
    if (ierr.eq.0) call init_unfmtd_strm(ierr, ulog, levv, levtry, iroot, icomm)
    return
  end subroutine sus_check_envs
!!!_   & sus_check_stream_pos - health_check
  subroutine sus_check_stream_pos &
       & (ierr, utest, ulog)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    use TOUZA_Std_fun,only: new_unit
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: utest
    integer,intent(in),optional :: ulog
    integer ut
    integer(kind=KIOFS) :: apos(2)

    ierr = 0
    ut = choice(-1, utest)
    if (ut.lt.0) then
       if (ut.lt.0) ut = new_unit()
       if (ut.lt.0) ierr = _ERROR(ERR_NO_IO_UNIT)
    endif
    if (ierr.eq.0) then
       open(UNIT=ut, IOSTAT=ierr, &
            &        ACCESS='STREAM', FORM='UNFORMATTED', STATUS='SCRATCH', ACTION='READWRITE')
    endif
    if (ierr.eq.0) write(ut, IOSTAT=ierr) 'TEST'
    if (ierr.eq.0) call sus_getpos(ierr, apos(1), ut)
    if (ierr.eq.0) write(ut, IOSTAT=ierr, POS=1)
    if (ierr.eq.0) call sus_getpos(ierr, apos(2), ut)
    if (ierr.eq.0) then
       if (apos(1).eq.apos(2)) then
          call msg_mdl('(''positioning/write not works '', I0, 1x, I0)', &
               & int(apos(:)), __MDL__, ulog)
          ierr = _ERROR(ERR_OPR_DISABLE)
       endif
    endif
    if (ierr.eq.0) then
       if (ierr.eq.0) write(ut, IOSTAT=ierr, POS=apos(1))
       if (ierr.eq.0) read(ut, IOSTAT=ierr, POS=1)
       if (ierr.eq.0) call sus_getpos(ierr, apos(2), ut)
    endif
    if (ierr.eq.0) then
       if (apos(1).eq.apos(2)) then
          call msg_mdl('(''positioning/read not works '', I0, 1x, I0)', &
               & int(apos(:)), __MDL__, ulog)
#if OPT_STREAM_RPOS_WORKAROUND
          call msg_mdl('workaround enabled', __MDL__, ulog)
#else /* not OPT_STREAM_RPOS_WORKAROUND */
          call msg_mdl('need rebuild with workaround', __MDL__, ulog)
          ierr = _ERROR(ERR_OPR_DISABLE)
#endif /* not OPT_STREAM_RPOS_WORKAROUND */
       endif
    endif
    if (ierr.eq.0) then
       close(UNIT=ut, IOSTAT=ierr, STATUS='DELETE')
    endif
    if (ierr.ne.0) then
       call msg_mdl('(''failed: '', I0)', ierr, __MDL__, ulog)
    endif
  end subroutine sus_check_stream_pos

!!!_   & sus_check_kinds_literal - health_check
  subroutine sus_check_kinds_literal &
       & (ierr, u)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,parameter :: KBUF=KI32
    integer utmp

    ierr = 0

201 format('kind:', A, ' = ', I0)
    utmp = choice(ulog, u)
    if (utmp.ge.0) then
       write(utmp, 201) 'KBUF',    KBUF
       write(utmp, 201) 'KI32',    KI32
       write(utmp, 201) '0_4',     KIND(0_4)
       write(utmp, 201) '0_KI32',  KIND(0_KI32)
       write(utmp, 201) '0_KBUF',  KIND(0_KBUF)
    else
       write(*, 201) 'KBUF',   KBUF
       write(*, 201) 'KI32',   KI32
       write(*, 201) '0_4',    KIND(0_4)
       write(*, 201) '0_KI32', KIND(0_KI32)
       write(*, 201) '0_KBUF', KIND(0_KBUF)
    endif
  end subroutine sus_check_kinds_literal

!!!_ + user subroutines
!!!_  & sus_open - open stream
  subroutine sus_open &
       & (ierr, u,      file, &
       &  form, status, action, position, access, iomsg)
    implicit none
    integer,         intent(out)            :: ierr
    integer,         intent(in)             :: u
    character(len=*),intent(in)             :: file
    character(len=*),intent(in),   optional :: form
    character(len=*),intent(in),   optional :: status
    character(len=*),intent(in),   optional :: action
    character(len=*),intent(in),   optional :: position
    character(len=*),intent(in),   optional :: access
    character(len=*),intent(inout),optional :: iomsg

    character(len=16) :: STT, ACT, FRM, POS, ACC
    integer jerr

    ierr = ERR_SUCCESS

    call sus_spec_status(STT, 'U', status)
    call sus_spec_action(ACT, 'R', action)
    call sus_spec_form(FRM, 'U', form)
    call sus_spec_position(POS, ' ', position)
    call sus_spec_access(ACC, 'ST', access)

#if      HAVE_FORTRAN_OPEN_IOMSG
    if (ierr.eq.0) then
       if (present(iomsg)) then
          open(UNIT=u, IOSTAT=ierr, &
               &       FILE=file, ACCESS=ACC, &
               &       FORM=FRM,  STATUS=STT, ACTION=ACT, POSITION=POS, IOMSG=iomsg)
       else
          open(UNIT=u, IOSTAT=ierr, &
               &       FILE=file, ACCESS=ACC, &
               &       FORM=FRM,  STATUS=STT, ACTION=ACT, POSITION=POS)
       endif
    endif
#else /* not HAVE_FORTRAN_OPEN_IOMSG */
    if (ierr.eq.0) then
       open(UNIT=u, IOSTAT=ierr, &
            &       FILE=file, ACCESS=ACC, &
            &       FORM=FRM,  STATUS=STT, ACTION=ACT, POSITION=POS)
       if (present(iomsg)) then
101       format('sus_open error = ', I0)
          if (ierr.ne.0) then
             write(iomsg, 101, IOSTAT=jerr) ierr
          endif
       endif
    endif
#endif /* not HAVE_FORTRAN_OPEN_IOMSG */
    if (ierr.ne.0) ierr = ERR_IO_GENERAL
  end subroutine sus_open
!!!_  & sus_close - close stream
  subroutine sus_close(ierr, u, file)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: u
    character(len=*),intent(in)  :: file
    ierr = ERR_SUCCESS
    close(UNIT=u, IOSTAT=ierr)
  end subroutine sus_close
!!!_  & sus_spec_form
  subroutine sus_spec_form(form, def, str)
    use TOUZA_Std_utl,only: upcase
    implicit none
    character(len=*),intent(out)         :: form
    character(len=*),intent(in)          :: def
    character(len=*),intent(in),optional :: str
    call choice_b(form, def, str)
    call upcase(form)
    if (FORM(1:1).eq.'U') then
       FORM='UNFORMATTED'
    else if (FORM(1:1).eq.'F') then
       FORM='FORMATTED'
    endif
  end subroutine sus_spec_form
!!!_  & sus_spec_action
  subroutine sus_spec_action(action, def, str)
    use TOUZA_Std_utl,only: upcase
    implicit none
    character(len=*),intent(out)         :: action
    character(len=*),intent(in)          :: def
    character(len=*),intent(in),optional :: str
    call choice_b(action, def, str)
    call upcase(action)
    if (ACTION.eq.'RW') then
       ACTION = 'READWRITE'
    else if (ACTION.eq.'R') then
       ACTION = 'READ'
    else if (ACTION.eq.'W') then
       ACTION = 'WRITE'
    endif
  end subroutine sus_spec_action
!!!_  & sus_spec_status
  subroutine sus_spec_status(status, def, str)
    use TOUZA_Std_utl,only: upcase
    implicit none
    character(len=*),intent(out)         :: status
    character(len=*),intent(in)          :: def
    character(len=*),intent(in),optional :: str
    call choice_b(status, def, str)
    call upcase(status)
    if (status(1:1).eq.'U') then
       status = 'UNKNOWN'
    else if (status(1:1).eq.'O') then
       status = 'OLD'
    else if (status(1:1).eq.'N') then
       status = 'NEW'
    else if (status(1:1).eq.'R') then
       status = 'REPLACE'
    endif
  end subroutine sus_spec_status
!!!_  & sus_spec_position
  subroutine sus_spec_position(position, def, str)
    use TOUZA_Std_utl,only: upcase
    implicit none
    character(len=*),intent(out)         :: position
    character(len=*),intent(in)          :: def
    character(len=*),intent(in),optional :: str
    call choice_b(position, def, str)
    call upcase(position)
    if (position(1:2).eq.'AP') then
       position = 'APPEND'
    else if (position(1:1).eq.'R') then
       position = 'REWIND'
    else
       position = 'ASIS'
    endif
  end subroutine sus_spec_position
!!!_  & sus_spec_access
  subroutine sus_spec_access(access, def, str)
    use TOUZA_Std_utl,only: upcase
    implicit none
    character(len=*),intent(out)         :: access
    character(len=*),intent(in)          :: def
    character(len=*),intent(in),optional :: str
    call choice_b(access, def, str)
    call upcase(access)
    if (access(1:2).eq.'ST') then
       access = 'STREAM'
    else if (access(1:1).eq.'D') then
       access = 'DIRECT'
    else
       access = 'SEQUENTIAL'
    endif
  end subroutine sus_spec_access
!!!_  & sus_is_status_new
  logical function sus_is_status_new(str, def) result(b)
    use TOUZA_Std_utl,only: choice_a, upcase
    implicit none
    character(len=*),intent(in)          :: str
    character(len=*),intent(in),optional :: def
    character(len=16) :: status
    call choice_a(status, def, str)
    call upcase(status)
    b = status(1:1).eq.'N'
  end function sus_is_status_new
!!!_  & sus_check_irec - health check of 32-bit marker record
  subroutine sus_check_irec &
       & (ierr, u, swap, dir, jopos, whence)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI32
    integer,            intent(out)          :: ierr
    integer,            intent(in)           :: u
    logical,            intent(in), optional :: swap
    integer,            intent(in), optional :: dir    ! negative to check backward
    integer(KIND=KIOFS),intent(out),optional :: jopos  ! file position of the opposite end
    integer,            intent(in), optional :: whence ! jopos type
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: apos,  apini
    integer d
    integer jerr

    ierr = 0
    d = choice(+1, dir)
    if (ierr.eq.0) call sus_getpos(ierr, apini, u)
    !
    apos = apini
    if (d.lt.0) then
       ! backward check
       do
          if (ierr.eq.0) apos = apos - mstrm_sep(isepf)
          if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=apos, swap=swap)
          if (ierr.eq.0) apos = apos - conv_b2strm(abs(isepf)) - mstrm_sep(iseph)
          if (ierr.eq.0) call sus_read_isep(ierr, u, iseph, pos=apos, swap=swap)
          if (ierr.eq.0) then
             if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
          else
             ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          endif
          if (ierr.ne.0) exit
          if (isepf.ge.0) exit
       enddo
    else
       ! forward check
       do
          if (ierr.eq.0) call sus_read_isep(ierr, u, iseph, swap=swap)
          if (ierr.eq.0) apos = apos + conv_b2strm(abs(iseph)) + mstrm_sep(iseph)
          if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=apos, swap=swap)
          if (ierr.eq.0) call sus_getpos(ierr, apos, u)
          if (ierr.eq.0) then
             if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
          else
             ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          endif
          if (ierr.ne.0) exit
          if (iseph.ge.0) exit
       enddo
    endif
    if (present(jopos)) then
       if (ierr.eq.0) jopos = sus_pos_a2rel(apos, u, whence)
    endif
    ! reset to initial position
#if OPT_STREAM_RPOS_WORKAROUND
    call sus_rseek_workaround(jerr, u, apini)
#else /* not OPT_STREAM_RPOS_WORKAROUND */
    read(UNIT=u, IOSTAT=jerr, POS=apini)
#endif /* not OPT_STREAM_RPOS_WORKAROUND */
    if (ierr.eq.0) ierr = jerr
  end subroutine sus_check_irec
!!!_  & sus_check_lrec - health check of 64-bit marker record
  subroutine sus_check_lrec &
       & (ierr, u, swap, dir, jopos, whence)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI64
    integer,            intent(out)          :: ierr
    integer,            intent(in)           :: u
    logical,            intent(in), optional :: swap
    integer,            intent(in), optional :: dir    ! negative to check backward
    integer(KIND=KIOFS),intent(out),optional :: jopos  ! file position of the opposite end
    integer,            intent(in), optional :: whence
    integer(KIND=KISEP) :: lseph, lsepf
    integer(KIND=KIOFS) :: apos,  apini
    integer d
    integer jerr

    ierr = 0
    d = choice(+1, dir)
    if (ierr.eq.0) call sus_getpos(ierr, apini, u)
    !
    apos = apini
    if (d.lt.0) then
       ! backward check
       do
          if (ierr.eq.0) apos = apos - mstrm_sep(lsepf)
          if (ierr.eq.0) call sus_read_lsep(ierr, u, lsepf, pos=apos, swap=swap)
          if (ierr.eq.0) apos = apos - conv_b2strm(lsepf) - mstrm_sep(lseph)
          if (ierr.eq.0) call sus_read_lsep(ierr, u, lseph, pos=apos, swap=swap)
          if (ierr.eq.0) then
             if (lseph.ne.lsepf) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
          else
             ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          endif
          exit
       enddo
    else
       ! forward check
       do
          if (ierr.eq.0) call sus_read_lsep(ierr, u, lseph, swap=swap)
          if (ierr.eq.0) apos = apos + conv_b2strm(lseph) + mstrm_sep(lseph)
          if (ierr.eq.0) call sus_read_lsep(ierr, u, lsepf, pos=apos, swap=swap)
          if (ierr.eq.0) call sus_getpos(ierr, apos, u)
          if (ierr.eq.0) then
             if (lseph.ne.lsepf) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
          else
             ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          endif
          exit
       enddo
    endif
    if (present(jopos)) then
       if (ierr.eq.0) jopos = sus_pos_a2rel(apos, u, whence)
    endif
    ! reset to initial position
#if OPT_STREAM_RPOS_WORKAROUND
    call sus_rseek_workaround(jerr, u, apini)
#else /* not OPT_STREAM_RPOS_WORKAROUND */
    read(UNIT=u, IOSTAT=jerr, POS=apini)
#endif /* not OPT_STREAM_RPOS_WORKAROUND */
    if (ierr.eq.0) ierr = jerr
  end subroutine sus_check_lrec
!!!_  & sus_skip_irec - forward/backward 32-bit marker records
  ! call sus_skip_irec(ierr, u, WHENCE)      cue only
  ! call sus_skip_irec(ierr, u, N, WHENCE)   skip N records from WHENCE
  subroutine sus_skip_irec &
       & (ierr, u, n, whence, swap)
    use TOUZA_Std_env,only: conv_b2strm, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI32
    integer,intent(out)         :: ierr
    integer,intent(in)          :: u
    integer,intent(in),optional :: n
    integer,intent(in),optional :: whence
    logical,intent(in),optional :: swap
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: apos
    integer j

    ierr = err_default

    if (ierr.eq.0) call sus_rseek(ierr, u, whence=whence)

    if (.not.present(n)) return

    if (ierr.eq.0) call sus_getpos(ierr, apos, u)
    if (n.gt.0) then
       j = 0
       do
          if (ierr.eq.0) then
             call sus_read_isep(ierr, u, iseph, swap=swap)
             if (is_eof_ss(ierr)) then
                ierr = _ERROR(ERR_EOF)
                exit
             endif
          endif
          if (ierr.eq.0) apos = apos + conv_b2strm(abs(iseph)) + mstrm_sep(iseph)
          if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=apos, swap=swap)
          if (ierr.eq.0) call sus_getpos(ierr, apos, u)
          if (ierr.eq.0) then
             if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
          else
             ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          endif
          if (ierr.ne.0) exit
          if (iseph.ge.0) j = j + 1
          if (j.eq.n) exit
       enddo
    else if (n.lt.0) then
       j = 0
       do
          if (ierr.eq.0) apos = apos - mstrm_sep(isepf)
          if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=apos, swap=swap)
          if (ierr.eq.0) apos = apos - conv_b2strm(abs(isepf)) - mstrm_sep(iseph)
          if (ierr.eq.0) call sus_read_isep(ierr, u, iseph, pos=apos, swap=swap)
          if (ierr.eq.0) then
             if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
          else
             ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          endif
          if (ierr.ne.0) exit
          if (isepf.ge.0) j = j + 1
          if (j.eq.-n) exit
       enddo
#if OPT_STREAM_RPOS_WORKAROUND
       if (ierr.eq.0) call sus_rseek_workaround(ierr, u, apos)
#else /* not OPT_STREAM_RPOS_WORKAROUND */
       if (ierr.eq.0) read(UNIT=u, IOSTAT=ierr, POS=apos)
#endif /* not OPT_STREAM_RPOS_WORKAROUND */
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_skip_irec
!!!_  & sus_skip_lrec - forward/backward 64-bit marker records
  subroutine sus_skip_lrec &
       & (ierr, u, n, whence, swap)
    use TOUZA_Std_env,only: conv_b2strm, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI64
    integer,intent(out)         :: ierr
    integer,intent(in)          :: u
    integer,intent(in),optional :: n
    integer,intent(in),optional :: whence
    logical,intent(in),optional :: swap
    integer(KIND=KISEP) :: lseph, lsepf
    integer(KIND=KIOFS) :: apos
    integer j

    ierr = err_default

    if (ierr.eq.0) call sus_rseek(ierr, u, whence=whence)

    if (.not.present(n)) return

    call sus_getpos(ierr, apos, u)
    if (n.gt.0) then
       do j = 1, n
          if (ierr.eq.0) then
             call sus_read_lsep(ierr, u, lseph, swap=swap)
             if (is_eof_ss(ierr)) then
                ierr = _ERROR(ERR_EOF)
                exit
             endif
          endif
          if (ierr.eq.0) apos = apos + conv_b2strm(lseph) + mstrm_sep(lseph)
          if (ierr.eq.0) call sus_read_lsep(ierr, u, lsepf, pos=apos, swap=swap)
          if (ierr.eq.0) call sus_getpos(ierr, apos, u)
          if (ierr.eq.0) then
             if (lseph.ne.lsepf) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
          else
             ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          endif
          if (ierr.ne.0) exit
       enddo
    else if (n.lt.0) then
       do j = 1, -n
          if (ierr.eq.0) apos = apos - mstrm_sep(lsepf)
          if (ierr.eq.0) call sus_read_lsep(ierr, u, lsepf, pos=apos, swap=swap)
          if (ierr.eq.0) apos = apos - conv_b2strm(lsepf) - mstrm_sep(lseph)
          if (ierr.eq.0) call sus_read_lsep(ierr, u, lseph, pos=apos, swap=swap)
          if (ierr.eq.0) then
             if (lseph.ne.lsepf) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
          else
             ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          endif
          if (ierr.ne.0) exit
       enddo
#if OPT_STREAM_RPOS_WORKAROUND
       if (ierr.eq.0) call sus_rseek_workaround(ierr, u, apos)
#else /* not OPT_STREAM_RPOS_WORKAROUND */
       if (ierr.eq.0) read(UNIT=u, IOSTAT=ierr, POS=apos)
#endif /* not OPT_STREAM_RPOS_WORKAROUND */
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_skip_lrec
!!!_  & sus_write_irec - write a record with 32bit-marker
  subroutine sus_write_irec_i &
       & (ierr, u, v, n, swap, pre, post, dummy)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u           ! file unit
    integer(KIND=KARG),intent(in)          :: V(0:*)      ! data array
    integer,           intent(in)          :: n           ! size of V (no check)
    logical,           intent(in),optional :: swap        ! byte-order switch
    logical,           intent(in),optional :: pre, post   ! continuation flag
    integer,           intent(in),optional :: dummy       ! dummy separator
    integer(KIND=KISEP) :: isep
    integer j, m, ns
    integer d

    ierr = err_default
    d = choice(0, dummy)
    ns = max_members(V(0))
    if (n.le.ns) then
       isep = get_size_bytes(V(0), n)
       if (ierr.eq.0) call sus_write_iset(ierr, u, V, n, isep, pre, post, swap)
    else if (d.lt.0) then
       isep = d
       ! ignore pre post
       if (ierr.eq.0) call sus_write_iset(ierr, u, V, n, isep, .FALSE., .FALSE., swap)
    else
       m = n
       j = 0
       isep = get_size_bytes(V(0), ns)
       ! first
       if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, pre, .TRUE., swap)
       ! middle
       do
          m = m - ns
          j = j + ns
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, .TRUE., .TRUE., swap)
       enddo
       ! last
       ns = m
       isep = get_size_bytes(V(0), ns)
       if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, .TRUE., post, swap)
    endif
    return
  end subroutine sus_write_irec_i
  subroutine sus_write_irec_l &
       & (ierr, u, v, n, swap, pre, post, dummy)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI64
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u
    integer(KIND=KARG),intent(in)          :: V(0:*)
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    logical,           intent(in),optional :: pre, post
    integer,           intent(in),optional :: dummy
    integer(KIND=KISEP) :: isep
    integer j, m, ns
    integer d

    ierr = err_default
    d = choice(0, dummy)
    ns = max_members(V(0))
    if (n.le.ns) then
       isep = get_size_bytes(V(0), n)
       if (ierr.eq.0) call sus_write_iset(ierr, u, V, n, isep, pre, post, swap)
    else if (d.lt.0) then
       isep = d
       ! ignore pre post
       if (ierr.eq.0) call sus_write_iset(ierr, u, V, n, isep, .FALSE., .FALSE., swap)
    else
       m = n
       j = 0
       isep = get_size_bytes(V(0), ns)
       ! first
       if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, pre, .TRUE., swap)
       ! middle
       do
          m = m - ns
          j = j + ns
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, .TRUE., .TRUE., swap)
       enddo
       ! last
       ns = m
       isep = get_size_bytes(V(0), ns)
       if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, .TRUE., post, swap)
    endif
    return
  end subroutine sus_write_irec_l
  subroutine sus_write_irec_f &
       & (ierr, u, v, n, swap, pre, post, dummy)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KFLT
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u
    real(KIND=KARG),intent(in)          :: V(0:*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    logical,        intent(in),optional :: pre, post
    integer,        intent(in),optional :: dummy
    integer(KIND=KISEP) :: isep
    integer j, m, ns
    integer d

    ierr = err_default
    d = choice(0, dummy)
    ns = max_members(V(0))
    if (n.le.ns) then
       isep = get_size_bytes(V(0), n)
       if (ierr.eq.0) call sus_write_iset(ierr, u, V, n, isep, pre, post, swap)
    else if (d.lt.0) then
       isep = d
       ! ignore pre post
       if (ierr.eq.0) call sus_write_iset(ierr, u, V, n, isep, .FALSE., .FALSE., swap)
    else
       m = n
       j = 0
       isep = get_size_bytes(V(0), ns)
       ! first
       if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, pre, .TRUE., swap)
       ! middle
       do
          m = m - ns
          j = j + ns
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, .TRUE., .TRUE., swap)
       enddo
       ! last
       ns = m
       isep = get_size_bytes(V(0), ns)
       if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, .TRUE., post, swap)
    endif
    return
  end subroutine sus_write_irec_f
  subroutine sus_write_irec_d &
       & (ierr, u, v, n, swap, pre, post, dummy)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KDBL
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u
    real(KIND=KARG),intent(in)          :: V(0:*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    logical,        intent(in),optional :: pre, post
    integer,        intent(in),optional :: dummy
    integer(KIND=KISEP) :: isep
    integer j, m, ns
    integer d

    ierr = err_default
    d = choice(0, dummy)
    ns = max_members(V(0))
    if (n.le.ns) then
       isep = get_size_bytes(V(0), n)
       if (ierr.eq.0) call sus_write_iset(ierr, u, V, n, isep, pre, post, swap)
    else if (d.lt.0) then
       isep = d
       ! ignore pre post
       if (ierr.eq.0) call sus_write_iset(ierr, u, V, n, isep, .FALSE., .FALSE., swap)
    else
       m = n
       j = 0
       isep = get_size_bytes(V(0), ns)
       ! first
       if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, pre, .TRUE., swap)
       ! middle
       do
          m = m - ns
          j = j + ns
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, .TRUE., .TRUE., swap)
       enddo
       ! last
       ns = m
       isep = get_size_bytes(V(0), ns)
       if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, .TRUE., post, swap)
    endif
    return
  end subroutine sus_write_irec_d
  subroutine sus_write_irec_a &
       & (ierr, u, v, n, swap, pre, post, dummy)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: u
    character(len=*),intent(in)          :: V(0:*)
    integer,         intent(in)          :: n
    logical,         intent(in),optional :: swap
    logical,         intent(in),optional :: pre, post
    integer,         intent(in),optional :: dummy
    integer(KIND=KISEP) :: isep
    integer j, m, ns
    integer d

    ierr = err_default
    d = choice(0, dummy)
    ns = max_members(V(0))
    if (ns.le.0) ierr = _ERROR(ERR_PANIC)
    if (n.le.ns) then
       isep = get_size_bytes(V(0), n)
       if (ierr.eq.0) call sus_write_iset(ierr, u, V, n, isep, pre, post, swap)
    else if (d.lt.0) then
       isep = d
       ! ignore pre post
       if (ierr.eq.0) call sus_write_iset(ierr, u, V, n, isep, .FALSE., .FALSE., swap)
    else
       m = n
       j = 0
       isep = get_size_bytes(V(0), ns)
       ! first
       if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, pre, .TRUE., swap)
       ! middle
       do
          m = m - ns
          j = j + ns
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, .TRUE., .TRUE., swap)
       enddo
       ! last
       ns = m
       isep = get_size_bytes(V(0), ns)
       if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, .TRUE., post, swap)
    endif
    return
  end subroutine sus_write_irec_a
!!!_  & sus_write_lrec - write a record with 64bit-marker
  subroutine sus_write_lrec_i &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KI32
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u
    integer(KIND=KARG),intent(in)          :: V(0:*)
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    integer(KIND=KISEP) :: lsep
    ierr = err_default
    lsep = get_size_bytes(V(0), int(n, kind=KISEP))
    if (ierr.eq.0) call sus_write_lsep(ierr, u, lsep, swap=swap)
    if (ierr.eq.0) call sus_write(ierr, u, V, n, swap)
    if (ierr.eq.0) call sus_write_lsep(ierr, u, lsep, swap=swap)
    return
  end subroutine sus_write_lrec_i
  subroutine sus_write_lrec_l &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KI64
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u
    integer(KIND=KARG),intent(in)          :: V(0:*)
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    integer(KIND=KISEP) :: lsep
    ierr = err_default
    lsep = get_size_bytes(V(0), int(n, kind=KISEP))
    if (ierr.eq.0) call sus_write_lsep(ierr, u, lsep, swap=swap)
    if (ierr.eq.0) call sus_write(ierr, u, V, n, swap)
    if (ierr.eq.0) call sus_write_lsep(ierr, u, lsep, swap=swap)
    return
  end subroutine sus_write_lrec_l
  subroutine sus_write_lrec_f &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KFLT
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u
    real(KIND=KARG),intent(in)          :: V(0:*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    integer(KIND=KISEP) :: lsep
    ierr = err_default
    lsep = get_size_bytes(V(0), int(n, kind=KISEP))
    if (ierr.eq.0) call sus_write_lsep(ierr, u, lsep, swap=swap)
    if (ierr.eq.0) call sus_write(ierr, u, V, n, swap)
    if (ierr.eq.0) call sus_write_lsep(ierr, u, lsep, swap=swap)
    return
  end subroutine sus_write_lrec_f
  subroutine sus_write_lrec_d &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KDBL
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u
    real(KIND=KARG),intent(in)          :: V(0:*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    integer(KIND=KISEP) :: lsep
    ierr = err_default
    lsep = get_size_bytes(V(0), int(n, kind=KISEP))
    if (ierr.eq.0) call sus_write_lsep(ierr, u, lsep, swap=swap)
    if (ierr.eq.0) call sus_write(ierr, u, V, n, swap)
    if (ierr.eq.0) call sus_write_lsep(ierr, u, lsep, swap=swap)
    return
  end subroutine sus_write_lrec_d
  subroutine sus_write_lrec_a &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI64
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: u
    character(len=*),intent(in)          :: V(0:*)
    integer,         intent(in)          :: n
    logical,         intent(in),optional :: swap
    integer(KIND=KISEP) :: lsep
    ierr = err_default
    lsep = get_size_bytes(V(0), int(n, kind=KISEP))
    if (ierr.eq.0) call sus_write_lsep(ierr, u, lsep, swap=swap)
    if (ierr.eq.0) call sus_write(ierr, u, V, n, swap)
    if (ierr.eq.0) call sus_write_lsep(ierr, u, lsep, swap=swap)
    return
  end subroutine sus_write_lrec_a
!!!_  & sus_pad_irec - pad single value on a record with 32bit-marker
  subroutine sus_pad_irec_i &
       & (ierr, u, v, n, swap, pre, post, dummy)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u           ! file unit
    integer(KIND=KARG),intent(in)          :: V           ! single value to pad
    integer,           intent(in)          :: n           ! size of V (no check)
    logical,           intent(in),optional :: swap        ! byte-order switch
    logical,           intent(in),optional :: pre, post   ! continuation flag
    integer,           intent(in),optional :: dummy       ! dummy separator
    integer(KIND=KISEP) :: isep
    integer j, m, ns
    integer d

    ierr = err_default
    d = choice(0, dummy)
    ns = max_members(V)
    if (n.le.ns) then
       isep = get_size_bytes(V, n)
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, n, isep, pre, post, swap)
    else if (d.lt.0) then
       isep = d
       ! ignore pre post
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, n, isep, .FALSE., .FALSE., swap)
    else
       m = n
       j = 0
       isep = get_size_bytes(V, ns)
       ! first
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, ns, isep, pre, .TRUE., swap)
       ! middle
       do
          m = m - ns
          j = j + ns
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_pad_iset(ierr, u, V, ns, isep, .TRUE., .TRUE., swap)
       enddo
       ! last
       ns = m
       isep = get_size_bytes(V, ns)
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, ns, isep, .TRUE., post, swap)
    endif
    return
  end subroutine sus_pad_irec_i
  subroutine sus_pad_irec_l &
       & (ierr, u, v, n, swap, pre, post, dummy)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI64
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u
    integer(KIND=KARG),intent(in)          :: V
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    logical,           intent(in),optional :: pre, post
    integer,           intent(in),optional :: dummy
    integer(KIND=KISEP) :: isep
    integer j, m, ns
    integer d

    ierr = err_default
    d = choice(0, dummy)
    ns = max_members(V)
    if (n.le.ns) then
       isep = get_size_bytes(V, n)
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, n, isep, pre, post, swap)
    else if (d.lt.0) then
       isep = d
       ! ignore pre post
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, n, isep, .FALSE., .FALSE., swap)
    else
       m = n
       j = 0
       isep = get_size_bytes(V, ns)
       ! first
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, ns, isep, pre, .TRUE., swap)
       ! middle
       do
          m = m - ns
          j = j + ns
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_pad_iset(ierr, u, V, ns, isep, .TRUE., .TRUE., swap)
       enddo
       ! last
       ns = m
       isep = get_size_bytes(V, ns)
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, ns, isep, .TRUE., post, swap)
    endif
    return
  end subroutine sus_pad_irec_l
  subroutine sus_pad_irec_f &
       & (ierr, u, v, n, swap, pre, post, dummy)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KFLT
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u
    real(KIND=KARG),intent(in)          :: V
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    logical,        intent(in),optional :: pre, post
    integer,        intent(in),optional :: dummy
    integer(KIND=KISEP) :: isep
    integer j, m, ns
    integer d

    ierr = err_default
    d = choice(0, dummy)
    ns = max_members(V)
    if (n.le.ns) then
       isep = get_size_bytes(V, n)
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, n, isep, pre, post, swap)
    else if (d.lt.0) then
       isep = d
       ! ignore pre post
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, n, isep, .FALSE., .FALSE., swap)
    else
       m = n
       j = 0
       isep = get_size_bytes(V, ns)
       ! first
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, ns, isep, pre, .TRUE., swap)
       ! middle
       do
          m = m - ns
          j = j + ns
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_pad_iset(ierr, u, V, ns, isep, .TRUE., .TRUE., swap)
       enddo
       ! last
       ns = m
       isep = get_size_bytes(V, ns)
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, ns, isep, .TRUE., post, swap)
    endif
    return
  end subroutine sus_pad_irec_f
  subroutine sus_pad_irec_d &
       & (ierr, u, v, n, swap, pre, post, dummy)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KDBL
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u
    real(KIND=KARG),intent(in)          :: V
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    logical,        intent(in),optional :: pre, post
    integer,        intent(in),optional :: dummy
    integer(KIND=KISEP) :: isep
    integer j, m, ns
    integer d

    ierr = err_default
    d = choice(0, dummy)
    ns = max_members(V)
    if (n.le.ns) then
       isep = get_size_bytes(V, n)
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, n, isep, pre, post, swap)
    else if (d.lt.0) then
       isep = d
       ! ignore pre post
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, n, isep, .FALSE., .FALSE., swap)
    else
       m = n
       j = 0
       isep = get_size_bytes(V, ns)
       ! first
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, ns, isep, pre, .TRUE., swap)
       ! middle
       do
          m = m - ns
          j = j + ns
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_pad_iset(ierr, u, V, ns, isep, .TRUE., .TRUE., swap)
       enddo
       ! last
       ns = m
       isep = get_size_bytes(V, ns)
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, ns, isep, .TRUE., post, swap)
    endif
    return
  end subroutine sus_pad_irec_d
  subroutine sus_pad_irec_a &
       & (ierr, u, v, n, swap, pre, post, dummy)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: u
    character(len=*),intent(in)          :: V
    integer,         intent(in)          :: n
    logical,         intent(in),optional :: swap
    logical,         intent(in),optional :: pre, post
    integer,         intent(in),optional :: dummy
    integer(KIND=KISEP) :: isep
    integer j, m, ns
    integer d

    ierr = err_default
    d = choice(0, dummy)
    ns = max_members(V)
    if (n.le.ns) then
       isep = get_size_bytes(V, n)
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, n, isep, pre, post, swap)
    else if (d.lt.0) then
       isep = d
       ! ignore pre post
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, n, isep, .FALSE., .FALSE., swap)
    else
       m = n
       j = 0
       isep = get_size_bytes(V, ns)
       ! first
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, ns, isep, pre, .TRUE., swap)
       ! middle
       do
          m = m - ns
          j = j + ns
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_pad_iset(ierr, u, V, ns, isep, .TRUE., .TRUE., swap)
       enddo
       ! last
       ns = m
       isep = get_size_bytes(V, ns)
       if (ierr.eq.0) call sus_pad_iset(ierr, u, V, ns, isep, .TRUE., post, swap)
    endif
    return
  end subroutine sus_pad_irec_a
!!!_  & sus_blank_irec - write blank(undefined) record with 32bit-marker
  subroutine sus_blank_irec_i &
       & (ierr, u, mold, n, swap, pre, post, dummy)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    use TOUZA_Std_env,only: conv_b2strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u           ! file unit
    integer(KIND=KARG),intent(in)          :: mold        ! placeholder
    integer,           intent(in)          :: n           ! size of blank
    logical,           intent(in),optional :: swap        ! byte-order switch
    logical,           intent(in),optional :: pre, post   ! continuation flag
    integer,           intent(in),optional :: dummy       ! dummy separator
    integer(KIND=KISEP) :: isep
    integer(KIND=KIOFS) :: skip
    integer m, ns
    integer d

    ierr = err_default
    d = choice(0, dummy)
    ns = max_members(mold)
    if (n.le.ns) then
       isep = get_size_bytes(mold, n)
       skip = conv_b2strm(abs(isep))
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, pre, post, swap)
    else if (d.lt.0) then
       isep = d
       skip = get_size_strm(mold, int(n, kind=KIOFS))
       ! ignore pre post
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, .FALSE., .FALSE., swap)
    else
       m = n
       isep = get_size_bytes(mold, ns)
       skip = conv_b2strm(abs(isep))
       ! first
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, pre, .TRUE., swap)
       ! middle
       do
          m = m - ns
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, .TRUE., .TRUE., swap)
       enddo
       ! last
       ns = m
       isep = get_size_bytes(mold, ns)
       skip = conv_b2strm(abs(isep))
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, .TRUE., post, swap)
    endif
    return
  end subroutine sus_blank_irec_i
  subroutine sus_blank_irec_l &
       & (ierr, u, mold, n, swap, pre, post, dummy)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    use TOUZA_Std_env,only: conv_b2strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI64
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u           ! file unit
    integer(KIND=KARG),intent(in)          :: mold        ! placeholder
    integer,           intent(in)          :: n           ! size of blank
    logical,           intent(in),optional :: swap        ! byte-order switch
    logical,           intent(in),optional :: pre, post   ! continuation flag
    integer,           intent(in),optional :: dummy       ! dummy separator
    integer(KIND=KISEP) :: isep
    integer(KIND=KIOFS) :: skip
    integer m, ns
    integer d

    ierr = err_default
    d = choice(0, dummy)
    ns = max_members(mold)
    if (n.le.ns) then
       isep = get_size_bytes(mold, n)
       skip = conv_b2strm(abs(isep))
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, pre, post, swap)
    else if (d.lt.0) then
       isep = d
       skip = get_size_strm(mold, int(n, kind=KIOFS))
       ! ignore pre post
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, .FALSE., .FALSE., swap)
    else
       m = n
       isep = get_size_bytes(mold, ns)
       skip = conv_b2strm(abs(isep))
       ! first
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, pre, .TRUE., swap)
       ! middle
       do
          m = m - ns
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, .TRUE., .TRUE., swap)
       enddo
       ! last
       ns = m
       isep = get_size_bytes(mold, ns)
       skip = conv_b2strm(abs(isep))
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, .TRUE., post, swap)
    endif
    return
  end subroutine sus_blank_irec_l
  subroutine sus_blank_irec_f &
       & (ierr, u, mold, n, swap, pre, post, dummy)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    use TOUZA_Std_env,only: conv_b2strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KFLT
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u           ! file unit
    real(KIND=KARG),   intent(in)          :: mold        ! placeholder
    integer,           intent(in)          :: n           ! size of blank
    logical,           intent(in),optional :: swap        ! byte-order switch
    logical,           intent(in),optional :: pre, post   ! continuation flag
    integer,           intent(in),optional :: dummy       ! dummy separator
    integer(KIND=KISEP) :: isep
    integer(KIND=KIOFS) :: skip
    integer m, ns
    integer d

    ierr = err_default
    d = choice(0, dummy)
    ns = max_members(mold)
    if (n.le.ns) then
       isep = get_size_bytes(mold, n)
       skip = conv_b2strm(abs(isep))
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, pre, post, swap)
    else if (d.lt.0) then
       isep = d
       skip = get_size_strm(mold, int(n, kind=KIOFS))
       ! ignore pre post
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, .FALSE., .FALSE., swap)
    else
       m = n
       isep = get_size_bytes(mold, ns)
       skip = conv_b2strm(abs(isep))
       ! first
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, pre, .TRUE., swap)
       ! middle
       do
          m = m - ns
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, .TRUE., .TRUE., swap)
       enddo
       ! last
       ns = m
       isep = get_size_bytes(mold, ns)
       skip = conv_b2strm(abs(isep))
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, .TRUE., post, swap)
    endif
    return
  end subroutine sus_blank_irec_f
  subroutine sus_blank_irec_d &
       & (ierr, u, mold, n, swap, pre, post, dummy)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    use TOUZA_Std_env,only: conv_b2strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KDBL
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u           ! file unit
    real(KIND=KARG),   intent(in)          :: mold        ! placeholder
    integer,           intent(in)          :: n           ! size of blank
    logical,           intent(in),optional :: swap        ! byte-order switch
    logical,           intent(in),optional :: pre, post   ! continuation flag
    integer,           intent(in),optional :: dummy       ! dummy separator
    integer(KIND=KISEP) :: isep
    integer(KIND=KIOFS) :: skip
    integer m, ns
    integer d

    ierr = err_default
    d = choice(0, dummy)
    ns = max_members(mold)
    if (n.le.ns) then
       isep = get_size_bytes(mold, n)
       skip = conv_b2strm(abs(isep))
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, pre, post, swap)
    else if (d.lt.0) then
       isep = d
       skip = get_size_strm(mold, int(n, kind=KIOFS))
       ! ignore pre post
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, .FALSE., .FALSE., swap)
    else
       m = n
       isep = get_size_bytes(mold, ns)
       skip = conv_b2strm(abs(isep))
       ! first
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, pre, .TRUE., swap)
       ! middle
       do
          m = m - ns
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, .TRUE., .TRUE., swap)
       enddo
       ! last
       ns = m
       isep = get_size_bytes(mold, ns)
       skip = conv_b2strm(abs(isep))
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, .TRUE., post, swap)
    endif
    return
  end subroutine sus_blank_irec_d
  subroutine sus_blank_irec_a &
       & (ierr, u, mold, n, swap, pre, post, dummy)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    use TOUZA_Std_env,only: conv_b2strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u           ! file unit
    character(len=*),  intent(in)          :: mold        ! placeholder
    integer,           intent(in)          :: n           ! size of blank
    logical,           intent(in),optional :: swap        ! byte-order switch
    logical,           intent(in),optional :: pre, post   ! continuation flag
    integer,           intent(in),optional :: dummy       ! dummy separator
    integer(KIND=KISEP) :: isep
    integer(KIND=KIOFS) :: skip
    integer m, ns
    integer d

    ierr = err_default
    d = choice(0, dummy)
    ns = max_members(mold)
    if (n.le.ns) then
       isep = get_size_bytes(mold, n)
       skip = conv_b2strm(abs(isep))
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, pre, post, swap)
    else if (d.lt.0) then
       isep = d
       skip = get_size_strm(mold, int(n, kind=KIOFS))
       ! ignore pre post
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, .FALSE., .FALSE., swap)
    else
       m = n
       isep = get_size_bytes(mold, ns)
       skip = conv_b2strm(abs(isep))
       ! first
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, pre, .TRUE., swap)
       ! middle
       do
          m = m - ns
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, .TRUE., .TRUE., swap)
       enddo
       ! last
       ns = m
       isep = get_size_bytes(mold, ns)
       skip = conv_b2strm(abs(isep))
       if (ierr.eq.0) call sus_blank_iset(ierr, u, skip, isep, .TRUE., post, swap)
    endif
    return
  end subroutine sus_blank_irec_a
!!!_  & sus_read_irec - read a record with 32bit-marker
  subroutine sus_read_irec_i &
       & (ierr, u, v, n, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,           intent(out)            :: ierr
    integer,           intent(in)             :: u       ! file unit
    integer(KIND=KARG),intent(out)            :: V(0:*)  ! data array
    integer,           intent(in)             :: n       ! size of V (no check)
    logical,           intent(in),   optional :: swap    ! byte-order switch
    logical,           intent(inout),optional :: sub     ! subrecord mode switch (may be updated)
    integer,           intent(in),   optional :: div     ! separator treatment
    integer,           intent(in),   optional :: lmem    ! expected total members in the current record
    ! <sub>
    !   if no SUB or F: work unit = record
    !                   cue to next record at exit
    !      SUB=T:       work unit = sub-record
    !                   read until v is filled.
    !                   cue to next sub-record at exit
    !                   modify to F when no more subrecords
    integer j, m, ns, nt
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: apos
    logical bdmy

    ierr = err_default
    bdmy = .FALSE.
    j = 0
    m = n
    nt = choice(m, lmem)
    do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, apos, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             ns = m
             apos = apos + get_size_strm(V(0), int(nt, kind=KIOFS))
             nt = 0
          else
             ns = get_mems_bytes(abs(iseph), V(0))
             nt = max(0, nt - ns)
             ns = min(m, ns)
             apos = apos + conv_b2strm(abs(iseph))
          endif
          m = m - ns
       endif
       if (ierr.eq.0) then
          if (ns.gt.0) call sus_read(ierr, u, V(j:j+ns-1), ns, swap)
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=apos, swap=swap)
       if (bdmy) then
          iseph = 0
       else
          if (ierr.eq.0) then
             if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
          endif
       endif
       if (ierr.eq.0) then
          if (iseph.ge.0) exit
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit
       endif
       if (m.eq.0.and.nt.eq.0) exit
       j = j + ns
    enddo
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (m.gt.0), iseph, sub, swap)
    return
  end subroutine sus_read_irec_i
  subroutine sus_read_irec_l &
       & (ierr, u, v, n, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI64
    integer,           intent(out)            :: ierr
    integer,           intent(in)             :: u
    integer(KIND=KARG),intent(out)            :: V(0:*)
    integer,           intent(in)             :: n
    logical,           intent(in),   optional :: swap
    logical,           intent(inout),optional :: sub   ! subrecord mode switch (may be updated)
    integer,           intent(in),   optional :: div   ! separator treatment
    integer,           intent(in),   optional :: lmem  ! total members
    ! <sub>
    !   if no SUB or F: work unit = record
    !                   cue to next record at exit
    !      SUB=T:       work unit = sub-record
    !                   read until v is filled.
    !                   cue to next sub-record at exit
    !                   modify to F when no more subrecords
    integer j, m, ns, nt
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: apos
    logical bdmy

    ierr = err_default
    bdmy = .FALSE.
    j = 0
    m = n
    nt = choice(m, lmem)
    do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, apos, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             ns = m
             apos = apos + get_size_strm(V(0), int(nt, kind=KIOFS))
             nt = 0
          else
             ns = get_mems_bytes(abs(iseph), V(0))
             nt = max(0, nt - ns)
             ns = min(m, ns)
             apos = apos + conv_b2strm(abs(iseph))
          endif
          m = m - ns
       endif
       if (ierr.eq.0) then
          if (ns.gt.0) call sus_read(ierr, u, V(j:j+ns-1), ns, swap)
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=apos, swap=swap)
       if (bdmy) then
          iseph = 0
       else
          if (ierr.eq.0) then
             if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
          endif
       endif
       if (ierr.eq.0) then
          if (iseph.ge.0) exit
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit
       endif
       if (m.eq.0.and.nt.eq.0) exit
       j = j + ns
    enddo
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (m.gt.0), iseph, sub, swap)
    return
  end subroutine sus_read_irec_l
  subroutine sus_read_irec_f &
       & (ierr, u, v, n, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KFLT
    integer,        intent(out)            :: ierr
    integer,        intent(in)             :: u
    real(KIND=KARG),intent(out)            :: V(0:*)
    integer,        intent(in)             :: n
    logical,        intent(in),   optional :: swap
    logical,        intent(inout),optional :: sub   ! subrecord mode switch (may be updated)
    integer,        intent(in),   optional :: div   ! separator treatment
    integer,        intent(in),   optional :: lmem  ! total members
    ! <sub>
    !   if no SUB or F: work unit = record
    !                   cue to next record at exit
    !      SUB=T:       work unit = sub-record
    !                   read until v is filled.
    !                   cue to next sub-record at exit
    !                   modify to F when no more subrecords
    integer j, m, ns, nt
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: apos
    logical bdmy

    ierr = err_default
    bdmy = .FALSE.
    j = 0
    m = n
    nt = choice(m, lmem)
    do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, apos, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             ns = m
             apos = apos + get_size_strm(V(0), int(nt, kind=KIOFS))
             nt = 0
          else
             ns = get_mems_bytes(abs(iseph), V(0))
             nt = max(0, nt - ns)
             ns = min(m, ns)
             apos = apos + conv_b2strm(abs(iseph))
          endif
          m = m - ns
       endif
       if (ierr.eq.0) then
          if (ns.gt.0) call sus_read(ierr, u, V(j:j+ns-1), ns, swap)
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=apos, swap=swap)
       if (bdmy) then
          iseph = 0
       else
          if (ierr.eq.0) then
             if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
          endif
       endif
       if (ierr.eq.0) then
          if (iseph.ge.0) exit
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit
       endif
       if (m.eq.0.and.nt.eq.0) exit
       j = j + ns
    enddo
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (m.gt.0), iseph, sub, swap)
    return
  end subroutine sus_read_irec_f
  subroutine sus_read_irec_d &
       & (ierr, u, v, n, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KDBL
    integer,        intent(out)            :: ierr
    integer,        intent(in)             :: u
    real(KIND=KARG),intent(out)            :: V(0:*)
    integer,        intent(in)             :: n
    logical,        intent(in),   optional :: swap
    logical,        intent(inout),optional :: sub   ! subrecord mode switch (may be updated)
    integer,        intent(in),   optional :: div   ! separator treatment
    integer,        intent(in),   optional :: lmem  ! total members
    ! <sub>
    !   if no SUB or F: work unit = record
    !                   cue to next record at exit
    !      SUB=T:       work unit = sub-record
    !                   read until v is filled.
    !                   cue to next sub-record at exit
    !                   modify to F when no more subrecords
    integer j, m, ns, nt
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: apos
    logical bdmy

    ierr = err_default
    bdmy = .FALSE.
    j = 0
    m = n
    nt = choice(m, lmem)
    do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, apos, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             ns = m
             apos = apos + get_size_strm(V(0), int(nt, kind=KIOFS))
             nt = 0
          else
             ns = get_mems_bytes(abs(iseph), V(0))
             nt = max(0, nt - ns)
             ns = min(m, ns)
             apos = apos + conv_b2strm(abs(iseph))
          endif
          m = m - ns
       endif
       if (ierr.eq.0) then
          if (ns.gt.0) call sus_read(ierr, u, V(j:j+ns-1), ns, swap)
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=apos, swap=swap)
       if (bdmy) then
          iseph = 0
       else
          if (ierr.eq.0) then
             if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
          endif
       endif
       if (ierr.eq.0) then
          if (iseph.ge.0) exit
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit
       endif
       if (m.eq.0.and.nt.eq.0) exit
       j = j + ns
    enddo
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (m.gt.0), iseph, sub, swap)
    return
  end subroutine sus_read_irec_d
  subroutine sus_read_irec_a &
       & (ierr, u, v, n, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,         intent(out)            :: ierr
    integer,         intent(in)             :: u
    character(len=*),intent(out)            :: V(0:*)
    integer,         intent(in)             :: n
    logical,         intent(in),   optional :: swap
    logical,         intent(inout),optional :: sub   ! subrecord mode switch (may be updated)
    integer,         intent(in),   optional :: div   ! separator treatment
    integer,         intent(in),   optional :: lmem  ! total members
    ! <sub>
    !   if no SUB or F: work unit = record
    !                   cue to next record at exit
    !      SUB=T:       work unit = sub-record
    !                   read until v is filled.
    !                   cue to next sub-record at exit
    !                   modify to F when no more subrecords
    integer j, m, ns, nt
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: apos
    logical bdmy

    ierr = err_default
    bdmy = .FALSE.
    j = 0
    m = n
    nt = choice(m, lmem)
    do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, apos, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             ns = m
             apos = apos + get_size_strm(V(0), int(nt, kind=KIOFS))
             nt = 0
          else
             ns = get_mems_bytes(abs(iseph), V(0))
             nt = max(0, nt - ns)
             ns = min(m, ns)
             apos = apos + conv_b2strm(abs(iseph))
          endif
          m = m - ns
       endif
       if (ierr.eq.0) then
          if (ns.gt.0) call sus_read(ierr, u, V(j:j+ns-1), ns, swap)
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=apos, swap=swap)
       if (bdmy) then
          iseph = 0
       else
          if (ierr.eq.0) then
             if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
          endif
       endif
       if (ierr.eq.0) then
          if (iseph.ge.0) exit
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit
       endif
       if (m.eq.0.and.nt.eq.0) exit
       j = j + ns
    enddo
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (m.gt.0), iseph, sub, swap)
    return
  end subroutine sus_read_irec_a
!!!_  & sus_read_lrec - read a record with 64bit-marker
  subroutine sus_read_lrec_i &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KI32
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u
    integer(KIND=KARG),intent(out)         :: V(0:*)
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    integer m
    integer(KIND=KISEP) :: lseph, lsepf
    integer(KIND=KIOFS) :: apos
    ierr = err_default
    if (ierr.eq.0) then
       call sus_read_lsep(ierr, u, lseph, swap=swap)
       if (is_eof_ss(ierr)) then
          ierr = _ERROR(ERR_EOF)
          return
       endif
    endif
    if (ierr.eq.0) call sus_getpos(ierr, apos, u)
    if (ierr.eq.0) then
       apos = apos + conv_b2strm(abs(lseph))
       ! to do: check overflow
       m = int(get_mems_bytes(lseph, V(0)))
       if (n.gt.m) then
          ierr = _ERROR(ERR_INVALID_RECORD_SIZE)
       else
          m = min(m, n)
          call sus_read(ierr, u, V, m, swap)
       endif
    endif
    if (ierr.eq.0) call sus_read_lsep(ierr, u, lsepf, pos=apos, swap=swap)
    if (ierr.eq.0) then
       if (lseph.ne.lsepf) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
    else
       ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
    endif
    return
  end subroutine sus_read_lrec_i
  subroutine sus_read_lrec_l &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KI64
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u
    integer(KIND=KARG),intent(out)         :: V(0:*)
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    integer m
    integer(KIND=KISEP) :: lseph, lsepf
    integer(KIND=KIOFS) :: apos
    ierr = err_default
    if (ierr.eq.0) then
       call sus_read_lsep(ierr, u, lseph, swap=swap)
       if (is_eof_ss(ierr)) then
          ierr = _ERROR(ERR_EOF)
          return
       endif
    endif
    if (ierr.eq.0) call sus_getpos(ierr, apos, u)
    if (ierr.eq.0) then
       apos = apos + conv_b2strm(abs(lseph))
       ! to do: check overflow
       m = int(get_mems_bytes(lseph, V(0)))
       if (n.gt.m) then
          ierr = _ERROR(ERR_INVALID_RECORD_SIZE)
       else
          m = min(m, n)
          call sus_read(ierr, u, V, m, swap)
       endif
    endif
    if (ierr.eq.0) call sus_read_lsep(ierr, u, lsepf, pos=apos, swap=swap)
    if (ierr.eq.0) then
       if (lseph.ne.lsepf) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
    else
       ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
    endif
    return
  end subroutine sus_read_lrec_l
  subroutine sus_read_lrec_f &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KFLT
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u
    real(KIND=KARG),intent(out)         :: V(0:*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    integer m
    integer(KIND=KISEP) :: lseph, lsepf
    integer(KIND=KIOFS) :: apos
    ierr = err_default
    if (ierr.eq.0) then
       call sus_read_lsep(ierr, u, lseph, swap=swap)
       if (is_eof_ss(ierr)) then
          ierr = _ERROR(ERR_EOF)
          return
       endif
    endif
    if (ierr.eq.0) call sus_getpos(ierr, apos, u)
    if (ierr.eq.0) then
       apos = apos + conv_b2strm(abs(lseph))
       ! to do: check overflow
       m = int(get_mems_bytes(lseph, V(0)))
       if (n.gt.m) then
          ierr = _ERROR(ERR_INVALID_RECORD_SIZE)
       else
          m = min(m, n)
          call sus_read(ierr, u, V, m, swap)
       endif
    endif
    if (ierr.eq.0) call sus_read_lsep(ierr, u, lsepf, pos=apos, swap=swap)
    if (ierr.eq.0) then
       if (lseph.ne.lsepf) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
    else
       ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
    endif
    return
  end subroutine sus_read_lrec_f
  subroutine sus_read_lrec_d &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KDBL
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u
    real(KIND=KARG),intent(out)         :: V(0:*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    integer m
    integer(KIND=KISEP) :: lseph, lsepf
    integer(KIND=KIOFS) :: apos
    ierr = err_default
    if (ierr.eq.0) then
       call sus_read_lsep(ierr, u, lseph, swap=swap)
       if (is_eof_ss(ierr)) then
          ierr = _ERROR(ERR_EOF)
          return
       endif
    endif
    if (ierr.eq.0) call sus_getpos(ierr, apos, u)
    if (ierr.eq.0) then
       apos = apos + conv_b2strm(abs(lseph))
       ! to do: check overflow
       m = int(get_mems_bytes(lseph, V(0)))
       if (n.gt.m) then
          ierr = _ERROR(ERR_INVALID_RECORD_SIZE)
       else
          m = min(m, n)
          call sus_read(ierr, u, V, m, swap)
       endif
    endif
    if (ierr.eq.0) call sus_read_lsep(ierr, u, lsepf, pos=apos, swap=swap)
    if (ierr.eq.0) then
       if (lseph.ne.lsepf) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
    else
       ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
    endif
    return
  end subroutine sus_read_lrec_d
  subroutine sus_read_lrec_a &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI64
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: u
    character(len=*),intent(out)         :: V(0:*)
    integer,         intent(in)          :: n
    logical,         intent(in),optional :: swap
    integer m
    integer(KIND=KISEP) :: lseph, lsepf
    integer(KIND=KIOFS) :: apos
    ierr = err_default
    if (ierr.eq.0) then
       call sus_read_lsep(ierr, u, lseph, swap=swap)
       if (is_eof_ss(ierr)) then
          ierr = _ERROR(ERR_EOF)
          return
       endif
    endif
    if (ierr.eq.0) call sus_getpos(ierr, apos, u)
    if (ierr.eq.0) then
       apos = apos + conv_b2strm(abs(lseph))
       ! to do: check overflow
       m = int(get_mems_bytes(lseph, V(0)))
       if (n.gt.m) then
          ierr = _ERROR(ERR_INVALID_RECORD_SIZE)
       else
          m = min(m, n)
          call sus_read(ierr, u, V, m, swap)
       endif
    endif
    if (ierr.eq.0) call sus_read_lsep(ierr, u, lsepf, pos=apos, swap=swap)
    if (ierr.eq.0) then
       if (lseph.ne.lsepf) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
    else
       ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
    endif
    return
  end subroutine sus_read_lrec_a
!!!_  & sus_suspend_write_irec - suspend-mode write a record with 32-bit marker
!!!_   & sus_suspend_write_irec_i
  subroutine sus_suspend_write_irec_i &
       & (ierr, u, v, n, sw, swap, dummy)
    use TOUZA_Std_utl,only: choice, condop
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u           ! file unit
    integer(KIND=KARG),intent(in)          :: V(0:*)      ! data array
    integer,           intent(in)          :: n           ! size of V (no check)
    integer,           intent(in)          :: sw          ! positive/negative for head/foot
    logical,           intent(in),optional :: swap        ! byte-order switch
    integer,           intent(in),optional :: dummy       ! dummy separator
    integer(KIND=KISEP) :: isep
    integer j, m, ns
    integer dsep
    integer(KIND=KIOFS) :: apos
    integer ss

    ierr = err_default
    dsep = choice(0, dummy)

    if (ierr.eq.0) call sus_suspend_write_irec_head(ierr, sw, u, dsep, swap)
    if (ierr.eq.0) then
       ns = rest_members(abs(suspend_wsub), V(0))
       ss = condop((suspend_wsub.lt.0), -1, +1)
       if (n.le.ns.or.dsep.lt.0) then
          call sus_write(ierr, u, V, n, swap)
          if (ierr.eq.0) suspend_wsub = suspend_wsub + ss * (get_size_bytes(V(0), n))
       else
          ! close current subrecord
          m = n
          j = 0
          isep = abs(suspend_wsub) + get_size_bytes(V(0), ns)
          call sus_getpos(ierr, apos, u)
          if (ierr.eq.0) call sus_write_isep(ierr, u, sep=isep, swap=swap, sub=.TRUE., pos=suspend_wposh)
          if (ierr.eq.0) call sus_write(ierr, u, V(j:j+ns-1), ns, swap, pos=apos)
          if (ierr.eq.0) call sus_write_isep(ierr, u, sep=isep, swap=swap, sub=(suspend_wsub.lt.0))
          m = m - ns
          j = j + ns
          ns = max_members(V(0))
          ! middle
          isep = get_size_bytes(V(0), ns)
          do
             if (m.le.ns) exit
             if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, .TRUE., .TRUE., swap)
             m = m - ns
             j = j + ns
          enddo
          ! open final subrecord
          isep = get_size_bytes(V(0), m)
          if (ierr.eq.0) call sus_getpos(ierr, suspend_wposh, u)
          if (ierr.eq.0) call sus_write_isep(ierr, u, sep=dsep, swap=swap)  ! write dummy separator
          if (ierr.eq.0) call sus_write(ierr, u, V(j:j+m-1), m, swap)
          if (ierr.eq.0) suspend_wsub = - isep
       endif
    endif
    if (ierr.eq.0) call sus_suspend_write_irec_foot(ierr, sw, u, dsep, swap)
    return
  end subroutine sus_suspend_write_irec_i
  subroutine sus_suspend_write_irec_l &
       & (ierr, u, v, n, sw, swap, dummy)
    use TOUZA_Std_utl,only: choice, condop
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI64
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u           ! file unit
    integer(KIND=KARG),intent(in)          :: V(0:*)      ! data array
    integer,           intent(in)          :: n           ! size of V (no check)
    integer,           intent(in)          :: sw          ! positive/negative for head/foot
    logical,           intent(in),optional :: swap        ! byte-order switch
    integer,           intent(in),optional :: dummy       ! dummy separator
    integer(KIND=KISEP) :: isep
    integer j, m, ns
    integer dsep
    integer(KIND=KIOFS) :: apos
    integer ss

    ierr = err_default
    dsep = choice(0, dummy)

    if (ierr.eq.0) call sus_suspend_write_irec_head(ierr, sw, u, dsep, swap)
    if (ierr.eq.0) then
       ns = rest_members(abs(suspend_wsub), V(0))
       ss = condop((suspend_wsub.lt.0), -1, +1)
       if (n.le.ns.or.dsep.lt.0) then
          call sus_write(ierr, u, V, n, swap)
          if (ierr.eq.0) suspend_wsub = suspend_wsub + ss * (get_size_bytes(V(0), n))
       else
          ! close current subrecord
          m = n
          j = 0
          isep = abs(suspend_wsub) + get_size_bytes(V(0), ns)
          call sus_getpos(ierr, apos, u)
          if (ierr.eq.0) call sus_write_isep(ierr, u, sep=isep, swap=swap, sub=.TRUE., pos=suspend_wposh)
          if (ierr.eq.0) call sus_write(ierr, u, V(j:j+ns-1), ns, swap, pos=apos)
          if (ierr.eq.0) call sus_write_isep(ierr, u, sep=isep, swap=swap, sub=(suspend_wsub.lt.0))
          m = m - ns
          j = j + ns
          ns = max_members(V(0))
          ! middle
          isep = get_size_bytes(V(0), ns)
          do
             if (m.le.ns) exit
             if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, .TRUE., .TRUE., swap)
             m = m - ns
             j = j + ns
          enddo
          ! open final subrecord
          isep = get_size_bytes(V(0), m)
          if (ierr.eq.0) call sus_getpos(ierr, suspend_wposh, u)
          if (ierr.eq.0) call sus_write_isep(ierr, u, sep=dsep, swap=swap)  ! write dummy separator
          if (ierr.eq.0) call sus_write(ierr, u, V(j:j+m-1), m, swap)
          if (ierr.eq.0) suspend_wsub = - isep
       endif
    endif
    if (ierr.eq.0) call sus_suspend_write_irec_foot(ierr, sw, u, dsep, swap)
    return
  end subroutine sus_suspend_write_irec_l
  subroutine sus_suspend_write_irec_f &
       & (ierr, u, v, n, sw, swap, dummy)
    use TOUZA_Std_utl,only: choice, condop
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KFLT
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u           ! file unit
    real(KIND=KARG),intent(in)          :: V(0:*)      ! data array
    integer,        intent(in)          :: n           ! size of V (no check)
    integer,        intent(in)          :: sw          ! positive/negative for head/foot
    logical,        intent(in),optional :: swap        ! byte-order switch
    integer,        intent(in),optional :: dummy       ! dummy separator
    integer(KIND=KISEP) :: isep
    integer j, m, ns
    integer dsep
    integer(KIND=KIOFS) :: apos
    integer ss

    ierr = err_default
    dsep = choice(0, dummy)

    if (ierr.eq.0) call sus_suspend_write_irec_head(ierr, sw, u, dsep, swap)
    if (ierr.eq.0) then
       ns = rest_members(abs(suspend_wsub), V(0))
       ss = condop((suspend_wsub.lt.0), -1, +1)
       if (n.le.ns.or.dsep.lt.0) then
          call sus_write(ierr, u, V, n, swap)
          if (ierr.eq.0) suspend_wsub = suspend_wsub + ss * (get_size_bytes(V(0), n))
       else
          ! close current subrecord
          m = n
          j = 0
          isep = abs(suspend_wsub) + get_size_bytes(V(0), ns)
          call sus_getpos(ierr, apos, u)
          if (ierr.eq.0) call sus_write_isep(ierr, u, sep=isep, swap=swap, sub=.TRUE., pos=suspend_wposh)
          if (ierr.eq.0) call sus_write(ierr, u, V(j:j+ns-1), ns, swap, pos=apos)
          if (ierr.eq.0) call sus_write_isep(ierr, u, sep=isep, swap=swap, sub=(suspend_wsub.lt.0))
          m = m - ns
          j = j + ns
          ns = max_members(V(0))
          ! middle
          isep = get_size_bytes(V(0), ns)
          do
             if (m.le.ns) exit
             if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, .TRUE., .TRUE., swap)
             m = m - ns
             j = j + ns
          enddo
          ! open final subrecord
          isep = get_size_bytes(V(0), m)
          if (ierr.eq.0) call sus_getpos(ierr, suspend_wposh, u)
          if (ierr.eq.0) call sus_write_isep(ierr, u, sep=dsep, swap=swap)  ! write dummy separator
          if (ierr.eq.0) call sus_write(ierr, u, V(j:j+m-1), m, swap)
          if (ierr.eq.0) suspend_wsub = - isep
       endif
    endif
    if (ierr.eq.0) call sus_suspend_write_irec_foot(ierr, sw, u, dsep, swap)
    return
  end subroutine sus_suspend_write_irec_f
  subroutine sus_suspend_write_irec_d &
       & (ierr, u, v, n, sw, swap, dummy)
    use TOUZA_Std_utl,only: choice, condop
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KDBL
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u           ! file unit
    real(KIND=KARG),intent(in)          :: V(0:*)      ! data array
    integer,        intent(in)          :: n           ! size of V (no check)
    integer,        intent(in)          :: sw          ! positive/negative for head/foot
    logical,        intent(in),optional :: swap        ! byte-order switch
    integer,        intent(in),optional :: dummy       ! dummy separator
    integer(KIND=KISEP) :: isep
    integer j, m, ns
    integer dsep
    integer(KIND=KIOFS) :: apos
    integer ss

    ierr = err_default
    dsep = choice(0, dummy)

    if (ierr.eq.0) call sus_suspend_write_irec_head(ierr, sw, u, dsep, swap)
    if (ierr.eq.0) then
       ns = rest_members(abs(suspend_wsub), V(0))
       ss = condop((suspend_wsub.lt.0), -1, +1)
       if (n.le.ns.or.dsep.lt.0) then
          call sus_write(ierr, u, V, n, swap)
          if (ierr.eq.0) suspend_wsub = suspend_wsub + ss * (get_size_bytes(V(0), n))
       else
          ! close current subrecord
          m = n
          j = 0
          isep = abs(suspend_wsub) + get_size_bytes(V(0), ns)
          call sus_getpos(ierr, apos, u)
          if (ierr.eq.0) call sus_write_isep(ierr, u, sep=isep, swap=swap, sub=.TRUE., pos=suspend_wposh)
          if (ierr.eq.0) call sus_write(ierr, u, V(j:j+ns-1), ns, swap, pos=apos)
          if (ierr.eq.0) call sus_write_isep(ierr, u, sep=isep, swap=swap, sub=(suspend_wsub.lt.0))
          m = m - ns
          j = j + ns
          ns = max_members(V(0))
          ! middle
          isep = get_size_bytes(V(0), ns)
          do
             if (m.le.ns) exit
             if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, .TRUE., .TRUE., swap)
             m = m - ns
             j = j + ns
          enddo
          ! open final subrecord
          isep = get_size_bytes(V(0), m)
          if (ierr.eq.0) call sus_getpos(ierr, suspend_wposh, u)
          if (ierr.eq.0) call sus_write_isep(ierr, u, sep=dsep, swap=swap)  ! write dummy separator
          if (ierr.eq.0) call sus_write(ierr, u, V(j:j+m-1), m, swap)
          if (ierr.eq.0) suspend_wsub = - isep
       endif
    endif
    if (ierr.eq.0) call sus_suspend_write_irec_foot(ierr, sw, u, dsep, swap)
    return
  end subroutine sus_suspend_write_irec_d
  subroutine sus_suspend_write_irec_a &
       & (ierr, u, v, n, sw, swap, dummy)
    use TOUZA_Std_utl,only: choice, condop
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: u           ! file unit
    character(len=*),intent(in)          :: V(0:*)      ! data array
    integer,         intent(in)          :: n           ! size of V (no check)
    integer,         intent(in)          :: sw          ! positive/negative for head/foot
    logical,         intent(in),optional :: swap        ! byte-order switch
    integer,         intent(in),optional :: dummy       ! dummy separator
    integer(KIND=KISEP) :: isep
    integer j, m, ns
    integer dsep
    integer(KIND=KIOFS) :: apos
    integer ss

    ierr = err_default
    dsep = choice(0, dummy)

    if (ierr.eq.0) call sus_suspend_write_irec_head(ierr, sw, u, dsep, swap)
    if (ierr.eq.0) then
       ns = rest_members(abs(suspend_wsub), V(0))
       ss = condop((suspend_wsub.lt.0), -1, +1)
       if (n.le.ns.or.dsep.lt.0) then
          call sus_write(ierr, u, V, n, swap)
          if (ierr.eq.0) suspend_wsub = suspend_wsub + ss * (get_size_bytes(V(0), n))
       else
          ! close current subrecord
          m = n
          j = 0
          isep = abs(suspend_wsub) + get_size_bytes(V(0), ns)
          call sus_getpos(ierr, apos, u)
          if (ierr.eq.0) call sus_write_isep(ierr, u, sep=isep, swap=swap, sub=.TRUE., pos=suspend_wposh)
          if (ierr.eq.0) call sus_write(ierr, u, V(j:j+ns-1), ns, swap, pos=apos)
          if (ierr.eq.0) call sus_write_isep(ierr, u, sep=isep, swap=swap, sub=(suspend_wsub.lt.0))
          m = m - ns
          j = j + ns
          ns = max_members(V(0))
          ! middle
          isep = get_size_bytes(V(0), ns)
          do
             if (m.le.ns) exit
             if (ierr.eq.0) call sus_write_iset(ierr, u, V(j:j+ns-1), ns, isep, .TRUE., .TRUE., swap)
             m = m - ns
             j = j + ns
          enddo
          ! open final subrecord
          isep = get_size_bytes(V(0), m)
          if (ierr.eq.0) call sus_getpos(ierr, suspend_wposh, u)
          if (ierr.eq.0) call sus_write_isep(ierr, u, sep=dsep, swap=swap)  ! write dummy separator
          if (ierr.eq.0) call sus_write(ierr, u, V(j:j+m-1), m, swap)
          if (ierr.eq.0) suspend_wsub = - isep
       endif
    endif
    if (ierr.eq.0) call sus_suspend_write_irec_foot(ierr, sw, u, dsep, swap)
    return
  end subroutine sus_suspend_write_irec_a
!!!_   & sus_suspend_write_irec_head
  subroutine sus_suspend_write_irec_head(ierr, sw, u, dsep, swap)
    implicit none
    integer,parameter :: KISEP=KI32
    integer,intent(out)         :: ierr
    integer,intent(in)          :: sw
    integer,intent(in)          :: u
    integer,intent(in)          :: dsep
    logical,intent(in),optional :: swap        ! byte-order switch
    integer(KIND=KISEP) :: isep

    ! check and update suspend_wu suspend_wsub suspend_wposh
    ierr = 0
    ! unit check
    if (sw.gt.0) then
       ! head
       if (suspend_wu.ge.0) then
          ierr = _ERROR(ERR_INVALID_PARAMETER)
       else
          suspend_wu  = u
          suspend_wsub = 0
          isep = int(dsep, KIND=KISEP)
          call sus_getpos(ierr, suspend_wposh, u)
          if (ierr.eq.0) call sus_write_isep(ierr, u, sep=isep, swap=swap)  ! write dummy separator
       endif
    else if (suspend_wu.ne.u) then
       ierr = _ERROR(ERR_INVALID_PARAMETER)
    endif
  end subroutine sus_suspend_write_irec_head
!!!_   & sus_suspend_write_irec_foot
  subroutine sus_suspend_write_irec_foot(ierr, sw, u, dsep, swap)
    implicit none
    integer,parameter :: KISEP=KI32
    integer,intent(out)         :: ierr
    integer,intent(in)          :: sw
    integer,intent(in)          :: u
    integer,intent(in)          :: dsep
    logical,intent(in),optional :: swap        ! byte-order switch
    integer(KIND=KISEP) :: isep
    integer(KIND=KIOFS) :: apos
    ! check and reset suspend_wu suspend_wsub suspend_wposh
    ierr = 0
    if (sw.lt.0) then
       if (dsep.lt.0) then
          call sus_write_isep(ierr, u, sep=dsep, swap=swap)
       else
          isep = abs(suspend_wsub)
          if (ierr.eq.0) call sus_getpos(ierr, apos, u)
          if (ierr.eq.0) call sus_write_isep(ierr, u, sep=isep, swap=swap, sub=.FAlSE., pos=suspend_wposh)
          if (ierr.eq.0) call sus_write_isep(ierr, u, sep=isep, swap=swap, sub=(suspend_wsub.lt.0), pos=apos)
       endif
       if (ierr.eq.0) then
          suspend_wposh = -1
          suspend_wsub = 0
          suspend_wu = -1
       endif
    endif
  end subroutine sus_suspend_write_irec_foot
!!!_  & sus_suspend_read_irec - suspend-mode write a record with 32-bit marker
!!!_   & sus_suspend_read_irec_[ilfda]
  subroutine sus_suspend_read_irec_i &
       & (ierr, u, v, n, sw, swap, div, lstrm, nskip)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(out)         :: V(0:*)
    integer,            intent(in)          :: n
    integer,            intent(in)          :: sw
    logical,            intent(in),optional :: swap
    integer,            intent(in),optional :: div    ! separator treatment
    integer(kind=KIOFS),intent(in),optional :: lstrm  ! total length in stream io unit
    integer,            intent(in),optional :: nskip
    integer j, m, ns
    integer(KIND=KISEP) :: isepf
    integer(KIND=KIOFS) :: apos,  apost
    integer(KIND=KIOFS) :: nx

    ierr = err_default
    if (ierr.eq.0) then
       call suspend_read_irec_head(ierr, u, sw, get_size_bytes(V(0)), swap, div, lstrm)
    endif
    if (ierr.eq.0) then
       if (choice(0, nskip).gt.0) then
          nx = get_size_strm(V(0), int(nskip, kind=KIOFS))
          call skip_suspend_read_irec(ierr, u, nx, swap)
       endif
    endif
    if (ierr.eq.0) then
       j = 0
       m = n
       do
          if (ierr.eq.0) call sus_getpos(ierr, apos, u)
          apost = apos + get_size_strm(V(j), int(m, kind=KIOFS))
          if (apost.lt.suspend_rposf) then
             ns = m
          else
             ns = int(get_mems_bytes((suspend_rposf - apos), V(j)))
          endif
          if (ns.gt.0) call sus_read(ierr, u, V(j:j+ns-1), ns, swap)
          if (ierr.eq.0) then
             j = j + ns
             m = m - ns
             if (m.eq.0) exit
             call sus_read_isep(ierr, u, isepf, pos=suspend_rposf, swap=swap)
          endif
          if (ierr.eq.0) then
             if (abs(suspend_rseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
          endif
          if (ierr.eq.0) then
             if (suspend_rseph.ge.0) then
                ierr = _ERROR(ERR_INVALID_RECORD_SIZE)
                exit
             endif
          endif
          if (ierr.eq.0) then
             call sus_read_isep(ierr, u, suspend_rseph, swap=swap)
             if (is_eof_ss(ierr)) then
                ierr = _ERROR(ERR_EOF)
                exit
             endif
             if (ierr.eq.0) call sus_getpos(ierr, apos, u)
             if (ierr.eq.0) suspend_rposf = apos + conv_b2strm(abs(suspend_rseph))
          endif
          if (ierr.ne.0) then
             ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
             exit
          endif
       enddo
    endif
    if (ierr.eq.0) call suspend_read_irec_foot(ierr, u, sw, swap)
    return
  end subroutine sus_suspend_read_irec_i
  subroutine sus_suspend_read_irec_l &
       & (ierr, u, v, n, sw, swap, div, lstrm, nskip)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI64
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(out)         :: V(0:*)
    integer,            intent(in)          :: n
    integer,            intent(in)          :: sw
    logical,            intent(in),optional :: swap
    integer,            intent(in),optional :: div    ! separator treatment
    integer(kind=KIOFS),intent(in),optional :: lstrm  ! total length in stream io unit
    integer,            intent(in),optional :: nskip
    integer j, m, ns
    integer(KIND=KISEP) :: isepf
    integer(KIND=KIOFS) :: apos,  apost
    integer(KIND=KIOFS) :: nx

    ierr = err_default
    if (ierr.eq.0) then
       call suspend_read_irec_head(ierr, u, sw, get_size_bytes(V(0)), swap, div, lstrm)
    endif
    if (ierr.eq.0) then
       if (choice(0, nskip).gt.0) then
          nx = get_size_strm(V(0), int(nskip, kind=KIOFS))
          call skip_suspend_read_irec(ierr, u, nx, swap)
       endif
    endif
    if (ierr.eq.0) then
       j = 0
       m = n
       do
          if (ierr.eq.0) call sus_getpos(ierr, apos, u)
          apost = apos + get_size_strm(V(j), int(m, kind=KIOFS))
          if (apost.lt.suspend_rposf) then
             ns = m
          else
             ns = int(get_mems_bytes((suspend_rposf - apos), V(j)))
          endif
          if (ns.gt.0) call sus_read(ierr, u, V(j:j+ns-1), ns, swap)
          if (ierr.eq.0) then
             j = j + ns
             m = m - ns
             if (m.eq.0) exit
             call sus_read_isep(ierr, u, isepf, pos=suspend_rposf, swap=swap)
          endif
          if (ierr.eq.0) then
             if (abs(suspend_rseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
          endif
          if (ierr.eq.0) then
             if (suspend_rseph.ge.0) then
                ierr = _ERROR(ERR_INVALID_RECORD_SIZE)
                exit
             endif
          endif
          if (ierr.eq.0) then
             call sus_read_isep(ierr, u, suspend_rseph, swap=swap)
             if (is_eof_ss(ierr)) then
                ierr = _ERROR(ERR_EOF)
                exit
             endif
             if (ierr.eq.0) call sus_getpos(ierr, apos, u)
             if (ierr.eq.0) suspend_rposf = apos + conv_b2strm(abs(suspend_rseph))
          endif
          if (ierr.ne.0) then
             ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
             exit
          endif
       enddo
    endif
    if (ierr.eq.0) call suspend_read_irec_foot(ierr, u, sw, swap)
    return
  end subroutine sus_suspend_read_irec_l
  subroutine sus_suspend_read_irec_f &
       & (ierr, u, v, n, sw, swap, div, lstrm, nskip)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KFLT
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    real(KIND=KARG),    intent(out)         :: V(0:*)
    integer,            intent(in)          :: n
    integer,            intent(in)          :: sw
    logical,            intent(in),optional :: swap
    integer,            intent(in),optional :: div    ! separator treatment
    integer(kind=KIOFS),intent(in),optional :: lstrm  ! total length in stream io unit
    integer,            intent(in),optional :: nskip
    integer j, m, ns
    integer(KIND=KISEP) :: isepf
    integer(KIND=KIOFS) :: apos,  apost
    integer(KIND=KIOFS) :: nx

    ierr = err_default
    if (ierr.eq.0) then
       call suspend_read_irec_head(ierr, u, sw, get_size_bytes(V(0)), swap, div, lstrm)
    endif
    if (ierr.eq.0) then
       if (choice(0, nskip).gt.0) then
          nx = get_size_strm(V(0), int(nskip, kind=KIOFS))
          call skip_suspend_read_irec(ierr, u, nx, swap)
       endif
    endif
    if (ierr.eq.0) then
       j = 0
       m = n
       do
          if (ierr.eq.0) call sus_getpos(ierr, apos, u)
          apost = apos + get_size_strm(V(j), int(m, kind=KIOFS))
          if (apost.lt.suspend_rposf) then
             ns = m
          else
             ns = int(get_mems_bytes((suspend_rposf - apos), V(j)))
          endif
          if (ns.gt.0) call sus_read(ierr, u, V(j:j+ns-1), ns, swap)
          if (ierr.eq.0) then
             j = j + ns
             m = m - ns
             if (m.eq.0) exit
             call sus_read_isep(ierr, u, isepf, pos=suspend_rposf, swap=swap)
          endif
          if (ierr.eq.0) then
             if (abs(suspend_rseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
          endif
          if (ierr.eq.0) then
             if (suspend_rseph.ge.0) then
                ierr = _ERROR(ERR_INVALID_RECORD_SIZE)
                exit
             endif
          endif
          if (ierr.eq.0) then
             call sus_read_isep(ierr, u, suspend_rseph, swap=swap)
             if (is_eof_ss(ierr)) then
                ierr = _ERROR(ERR_EOF)
                exit
             endif
             if (ierr.eq.0) call sus_getpos(ierr, apos, u)
             if (ierr.eq.0) suspend_rposf = apos + conv_b2strm(abs(suspend_rseph))
          endif
          if (ierr.ne.0) then
             ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
             exit
          endif
       enddo
    endif
    if (ierr.eq.0) call suspend_read_irec_foot(ierr, u, sw, swap)
    return
  end subroutine sus_suspend_read_irec_f
  subroutine sus_suspend_read_irec_d &
       & (ierr, u, v, n, sw, swap, div, lstrm, nskip)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KDBL
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    real(KIND=KARG),    intent(out)         :: V(0:*)
    integer,            intent(in)          :: n
    integer,            intent(in)          :: sw
    logical,            intent(in),optional :: swap
    integer,            intent(in),optional :: div    ! separator treatment
    integer(kind=KIOFS),intent(in),optional :: lstrm  ! total length in stream io unit
    integer,            intent(in),optional :: nskip
    integer j, m, ns
    integer(KIND=KISEP) :: isepf
    integer(KIND=KIOFS) :: apos,  apost
    integer(KIND=KIOFS) :: nx

    ierr = err_default
    if (ierr.eq.0) then
       call suspend_read_irec_head(ierr, u, sw, get_size_bytes(V(0)), swap, div, lstrm)
    endif
    if (ierr.eq.0) then
       if (choice(0, nskip).gt.0) then
          nx = get_size_strm(V(0), int(nskip, kind=KIOFS))
          call skip_suspend_read_irec(ierr, u, nx, swap)
       endif
    endif
    if (ierr.eq.0) then
       j = 0
       m = n
       do
          if (ierr.eq.0) call sus_getpos(ierr, apos, u)
          apost = apos + get_size_strm(V(j), int(m, kind=KIOFS))
          if (apost.lt.suspend_rposf) then
             ns = m
          else
             ns = int(get_mems_bytes((suspend_rposf - apos), V(j)))
          endif
          if (ns.gt.0) call sus_read(ierr, u, V(j:j+ns-1), ns, swap)
          if (ierr.eq.0) then
             j = j + ns
             m = m - ns
             if (m.eq.0) exit
             call sus_read_isep(ierr, u, isepf, pos=suspend_rposf, swap=swap)
          endif
          if (ierr.eq.0) then
             if (abs(suspend_rseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
          endif
          if (ierr.eq.0) then
             if (suspend_rseph.ge.0) then
                ierr = _ERROR(ERR_INVALID_RECORD_SIZE)
                exit
             endif
          endif
          if (ierr.eq.0) then
             call sus_read_isep(ierr, u, suspend_rseph, swap=swap)
             if (is_eof_ss(ierr)) then
                ierr = _ERROR(ERR_EOF)
                exit
             endif
             if (ierr.eq.0) call sus_getpos(ierr, apos, u)
             if (ierr.eq.0) suspend_rposf = apos + conv_b2strm(abs(suspend_rseph))
          endif
          if (ierr.ne.0) then
             ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
             exit
          endif
       enddo
    endif
    if (ierr.eq.0) call suspend_read_irec_foot(ierr, u, sw, swap)
    return
  end subroutine sus_suspend_read_irec_d
  subroutine sus_suspend_read_irec_a &
       & (ierr, u, v, n, sw, swap, div, lstrm, nskip)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    character(len=*),   intent(out)         :: V(0:*)
    integer,            intent(in)          :: n
    integer,            intent(in)          :: sw
    logical,            intent(in),optional :: swap
    integer,            intent(in),optional :: div    ! separator treatment
    integer(kind=KIOFS),intent(in),optional :: lstrm  ! total length in stream io unit
    integer,            intent(in),optional :: nskip
    integer j, m, ns
    integer(KIND=KISEP) :: isepf
    integer(KIND=KIOFS) :: apos,  apost
    integer(KIND=KIOFS) :: nx

    ierr = err_default
    if (ierr.eq.0) then
       call suspend_read_irec_head(ierr, u, sw, get_size_bytes(V(0)), swap, div, lstrm)
    endif
    if (ierr.eq.0) then
       if (choice(0, nskip).gt.0) then
          nx = get_size_strm(V(0), int(nskip, kind=KIOFS))
          call skip_suspend_read_irec(ierr, u, nx, swap)
       endif
    endif
    if (ierr.eq.0) then
       j = 0
       m = n
       do
          if (ierr.eq.0) call sus_getpos(ierr, apos, u)
          apost = apos + get_size_strm(V(j), int(m, kind=KIOFS))
          if (apost.lt.suspend_rposf) then
             ns = m
          else
             ns = int(get_mems_bytes((suspend_rposf - apos), V(j)))
          endif
          if (ns.gt.0) call sus_read(ierr, u, V(j:j+ns-1), ns, swap)
          if (ierr.eq.0) then
             j = j + ns
             m = m - ns
             if (m.eq.0) exit
             call sus_read_isep(ierr, u, isepf, pos=suspend_rposf, swap=swap)
          endif
          if (ierr.eq.0) then
             if (abs(suspend_rseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
          endif
          if (ierr.eq.0) then
             if (suspend_rseph.ge.0) then
                ierr = _ERROR(ERR_INVALID_RECORD_SIZE)
                exit
             endif
          endif
          if (ierr.eq.0) then
             call sus_read_isep(ierr, u, suspend_rseph, swap=swap)
             if (is_eof_ss(ierr)) then
                ierr = _ERROR(ERR_EOF)
                exit
             endif
             if (ierr.eq.0) call sus_getpos(ierr, apos, u)
             if (ierr.eq.0) suspend_rposf = apos + conv_b2strm(abs(suspend_rseph))
          endif
          if (ierr.ne.0) then
             ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
             exit
          endif
       enddo
    endif
    if (ierr.eq.0) call suspend_read_irec_foot(ierr, u, sw, swap)
    return
  end subroutine sus_suspend_read_irec_a
!!!_   & suspend_read_irec_head
  subroutine suspend_read_irec_head &
       & (ierr, u, sw, gsb, swap, div, lstrm)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm
    implicit none
    integer,parameter :: KISEP=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer,            intent(in)          :: sw
    integer,            intent(in)          :: gsb    ! get_size_bytes(V(0))
    logical,            intent(in),optional :: swap
    integer,            intent(in),optional :: div    ! separator treatment
    integer(kind=KIOFS),intent(in),optional :: lstrm  ! total length in stream io unit

    integer(KIND=KIOFS) :: apos
    integer(KIND=KIOFS) :: ls
    logical bdmy
    ierr = 0
    ls = choice(0_KIOFS, lstrm)
    ! unit check
    if (sw.gt.0) then
       ! head
       if (suspend_ru.ge.0) then
          ierr = _ERROR(ERR_INVALID_PARAMETER)
       else
          suspend_ru  = u
          suspend_rposf = -1
          suspend_rseph = 0
          if (ierr.eq.0) call sus_read_isep(ierr, u, suspend_rseph, swap=swap)
          if (ierr.eq.0) call sus_getpos(ierr, apos, u)
          if (ierr.eq.0) then
             call check_dummy_irec(bdmy, ls, RECL_MAX_STREAM, gsb, suspend_rseph, div)
             if (bdmy) then
                suspend_rposf = apos + ls
                suspend_rseph = 0
             else
                suspend_rposf = apos + conv_b2strm(abs(suspend_rseph))
             endif
          endif
       endif
    else if (suspend_ru.ne.u) then
       ierr = _ERROR(ERR_INVALID_PARAMETER)
    endif
  end subroutine suspend_read_irec_head
!!!_   & skip_suspend_read_irec
  subroutine skip_suspend_read_irec &
       & (ierr, u, skip, swap)
    use TOUZA_Std_env,only: conv_b2strm, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI32
    integer,            intent(out) :: ierr
    integer,            intent(in)  :: u
    integer(kind=KIOFS),intent(in)  :: skip ! skip in stream unit
    logical,optional,   intent(in)  :: swap
    integer(kind=KIOFS) :: apos, apost
    integer(kind=KIOFS) :: ns,   nx
    integer(kind=KISEP) :: isepf

    ierr = 0
    ns = skip
    do
       if (ierr.eq.0) call sus_getpos(ierr, apos, u)
       if (ierr.eq.0) then
          apost = min(suspend_rposf, apos + ns)
          nx = apost - apos
          call sus_rseek(ierr, u, apost, whence=WHENCE_ABS)
       endif
       if (ierr.eq.0) then
          ns = ns - nx
          if (ns.le.0) exit
          call sus_read_isep(ierr, u, isepf, pos=suspend_rposf, swap=swap)
       endif
       if (ierr.eq.0) then
          if (abs(suspend_rseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.eq.0) then
          if (suspend_rseph.ge.0) then
             ierr = _ERROR(ERR_INVALID_RECORD_SIZE)
             exit
          endif
       endif
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, suspend_rseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit
          endif
          if (ierr.eq.0) call sus_getpos(ierr, apos, u)
          if (ierr.eq.0) suspend_rposf = apos + conv_b2strm(abs(suspend_rseph))
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit
       endif
    enddo
  end subroutine skip_suspend_read_irec
!!!_   & suspend_read_irec_foot
  subroutine suspend_read_irec_foot &
       & (ierr, u, sw, swap)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI32
    integer,intent(out)         :: ierr
    integer,intent(in)          :: u
    integer,intent(in)          :: sw
    logical,intent(in),optional :: swap

    integer(kind=KISEP) :: isepf
    ierr = 0
    if (sw.lt.0) then
       call sus_read_isep(ierr, u, isepf, pos=suspend_rposf, swap=swap)
       if (ierr.eq.0) then
          if (suspend_rseph.lt.0) call sus_skip_irec(ierr, u, 1, swap=swap)
       endif
       if (ierr.eq.0) then
          suspend_ru = -1
          suspend_rseph = 0
          suspend_rposf = -1
       endif
    endif
  end subroutine suspend_read_irec_foot

!!!_  & sus_slice_read_irec - read subarray from a record  with 32-bit marker
!!!_   & sus_slice_read_irec_[ilfda]
  subroutine sus_slice_read_irec_i &
       & (ierr, u, v, bes, r, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,           intent(out)            :: ierr
    integer,           intent(in)             :: u
    integer(KIND=KARG),intent(out)            :: V(0:*)
    integer,           intent(in)             :: bes(3, 0:*) ! begin/end/stride triplet
    integer,           intent(in)             :: r           ! ranks
    logical,           intent(in),   optional :: swap
    logical,           intent(inout),optional :: sub
    integer,           intent(in),   optional :: div
    integer,           intent(in),   optional :: lmem        ! total members

    integer rr
    integer stp(0:r-1), itr(0:r-1)
    integer idx(0:r-1)
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = total_members(bes, r, lmem)

    call set_slice_loop(rr, stp, itr, bes, r)
    idx(0:rr-1) = 0
    jsrc = init_offset(bes, r)
    jb = 0
    jdst = 0

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
             nt = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             nt = max(0, nt - ns)
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             m = min((je - jsrc), itr(0) - idx(0))
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_read(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             call next_offset(jsrc, idx, stp, itr, rr, m)
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       if (jsrc.lt.0.and.nt.eq.0) exit
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jsrc.ge.0), iseph, sub, swap)
  end subroutine sus_slice_read_irec_i
  subroutine sus_slice_read_irec_l &
       & (ierr, u, v, bes, r, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI64
    integer,           intent(out)            :: ierr
    integer,           intent(in)             :: u
    integer(KIND=KARG),intent(out)            :: V(0:*)
    integer,           intent(in)             :: bes(3, 0:*) ! begin/end/stride triplet
    integer,           intent(in)             :: r           ! ranks
    logical,           intent(in),   optional :: swap
    logical,           intent(inout),optional :: sub
    integer,           intent(in),   optional :: div
    integer,           intent(in),   optional :: lmem        ! total members

    integer rr
    integer stp(0:r-1), itr(0:r-1)
    integer idx(0:r-1)
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = total_members(bes, r, lmem)

    call set_slice_loop(rr, stp, itr, bes, r)
    idx(0:rr-1) = 0
    jsrc = init_offset(bes, r)
    jb = 0
    jdst = 0

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
             nt = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             nt = max(0, nt - ns)
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             m = min((je - jsrc), itr(0) - idx(0))
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_read(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             call next_offset(jsrc, idx, stp, itr, rr, m)
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       if (jsrc.lt.0.and.nt.eq.0) exit
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jsrc.ge.0), iseph, sub, swap)
  end subroutine sus_slice_read_irec_l
  subroutine sus_slice_read_irec_f &
       & (ierr, u, v, bes, r, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KFLT
    integer,        intent(out)            :: ierr
    integer,        intent(in)             :: u
    real(KIND=KARG),intent(out)            :: V(0:*)
    integer,        intent(in)             :: bes(3, 0:*) ! begin/end/stride triplet
    integer,        intent(in)             :: r           ! ranks
    logical,        intent(in),   optional :: swap
    logical,        intent(inout),optional :: sub
    integer,        intent(in),   optional :: div
    integer,        intent(in),   optional :: lmem        ! total members

    integer rr
    integer stp(0:r-1), itr(0:r-1)
    integer idx(0:r-1)
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = total_members(bes, r, lmem)

    call set_slice_loop(rr, stp, itr, bes, r)
    idx(0:rr-1) = 0
    jsrc = init_offset(bes, r)
    jb = 0
    jdst = 0

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
             nt = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             nt = max(0, nt - ns)
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             m = min((je - jsrc), itr(0) - idx(0))
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_read(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             call next_offset(jsrc, idx, stp, itr, rr, m)
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       if (jsrc.lt.0.and.nt.eq.0) exit
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jsrc.ge.0), iseph, sub, swap)
  end subroutine sus_slice_read_irec_f
  subroutine sus_slice_read_irec_d &
       & (ierr, u, v, bes, r, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KDBL
    integer,        intent(out)            :: ierr
    integer,        intent(in)             :: u
    real(KIND=KARG),intent(out)            :: V(0:*)
    integer,        intent(in)             :: bes(3, 0:*) ! begin/end/stride triplet
    integer,        intent(in)             :: r           ! ranks
    logical,        intent(in),   optional :: swap
    logical,        intent(inout),optional :: sub
    integer,        intent(in),   optional :: div
    integer,        intent(in),   optional :: lmem        ! total members

    integer rr
    integer stp(0:r-1), itr(0:r-1)
    integer idx(0:r-1)
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = total_members(bes, r, lmem)

    call set_slice_loop(rr, stp, itr, bes, r)
    idx(0:rr-1) = 0
    jsrc = init_offset(bes, r)
    jb = 0
    jdst = 0

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
             nt = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             nt = max(0, nt - ns)
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             m = min((je - jsrc), itr(0) - idx(0))
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_read(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             call next_offset(jsrc, idx, stp, itr, rr, m)
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       if (jsrc.lt.0.and.nt.eq.0) exit
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jsrc.ge.0), iseph, sub, swap)
  end subroutine sus_slice_read_irec_d
  subroutine sus_slice_read_irec_a &
       & (ierr, u, v, bes, r, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,         intent(out)            :: ierr
    integer,         intent(in)             :: u
    character(len=*),intent(out)            :: V(0:*)
    integer,         intent(in)             :: bes(3, 0:*) ! begin/end/stride triplet
    integer,         intent(in)             :: r           ! ranks
    logical,         intent(in),   optional :: swap
    logical,         intent(inout),optional :: sub
    integer,         intent(in),   optional :: div
    integer,         intent(in),   optional :: lmem        ! total members

    integer rr
    integer stp(0:r-1), itr(0:r-1)
    integer idx(0:r-1)
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = total_members(bes, r, lmem)

    call set_slice_loop(rr, stp, itr, bes, r)
    idx(0:rr-1) = 0
    jsrc = init_offset(bes, r)
    jb = 0
    jdst = 0

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
             nt = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             nt = max(0, nt - ns)
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             m = min((je - jsrc), itr(0) - idx(0))
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_read(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             call next_offset(jsrc, idx, stp, itr, rr, m)
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       if (jsrc.lt.0.and.nt.eq.0) exit
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jsrc.ge.0), iseph, sub, swap)
  end subroutine sus_slice_read_irec_a
!!!_  & sus_edit_slice_irec - edit subarray on a record  with 32-bit marker
  subroutine sus_edit_slice_irec_i &
       & (ierr, u, v, bes, r, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,           intent(out)            :: ierr        !
    integer,           intent(in)             :: u           ! file unit
    integer(KIND=KARG),intent(in)             :: V(0:*)      ! data array
    integer,           intent(in)             :: bes(3, 0:*) ! begin/end/stride triplet
    integer,           intent(in)             :: r           ! ranks of bes
    logical,           intent(in),   optional :: swap        ! byte-order switch
    logical,           intent(inout),optional :: sub         ! subrecord mode switch (may be updated)
    integer,           intent(in),   optional :: div         ! separator treatment
    integer,           intent(in),   optional :: lmem        ! expected total members in the current record

    integer rr
    integer stp(0:r-1), itr(0:r-1)
    integer idx(0:r-1)
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = total_members(bes, r, lmem)

    call set_slice_loop(rr, stp, itr, bes, r)
    idx(0:rr-1) = 0
    jsrc = init_offset(bes, r)
    jb = 0
    jdst = 0

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       ! write(*, *) jb, jdst, iseph, aposh, swap
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
             nt = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             nt = max(0, nt - ns)
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             m = min((je - jsrc), itr(0) - idx(0))
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_write(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             call next_offset(jsrc, idx, stp, itr, rr, m)
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       if (jsrc.lt.0.and.nt.eq.0) exit
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jsrc.ge.0), iseph, sub, swap)
  end subroutine sus_edit_slice_irec_i
  subroutine sus_edit_slice_irec_l &
       & (ierr, u, v, bes, r, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI64
    integer,           intent(out)            :: ierr
    integer,           intent(in)             :: u
    integer(KIND=KARG),intent(in)             :: V(0:*)
    integer,           intent(in)             :: bes(3, 0:*) ! begin/end/stride triplet
    integer,           intent(in)             :: r           ! ranks
    logical,           intent(in),   optional :: swap
    logical,           intent(inout),optional :: sub
    integer,           intent(in),   optional :: div
    integer,           intent(in),   optional :: lmem        ! total members

    integer rr
    integer stp(0:r-1), itr(0:r-1)
    integer idx(0:r-1)
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = total_members(bes, r, lmem)

    call set_slice_loop(rr, stp, itr, bes, r)
    idx(0:rr-1) = 0
    jsrc = init_offset(bes, r)
    jb = 0
    jdst = 0

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       ! write(*, *) jb, jdst, iseph, aposh, swap
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
             nt = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             nt = max(0, nt - ns)
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             m = min((je - jsrc), itr(0) - idx(0))
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_write(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             call next_offset(jsrc, idx, stp, itr, rr, m)
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       if (jsrc.lt.0.and.nt.eq.0) exit
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jsrc.ge.0), iseph, sub, swap)
  end subroutine sus_edit_slice_irec_l
  subroutine sus_edit_slice_irec_f &
       & (ierr, u, v, bes, r, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KFLT
    integer,        intent(out)            :: ierr
    integer,        intent(in)             :: u
    real(KIND=KARG),intent(in)             :: V(0:*)
    integer,        intent(in)             :: bes(3, 0:*) ! begin/end/stride triplet
    integer,        intent(in)             :: r           ! ranks
    logical,        intent(in),   optional :: swap
    logical,        intent(inout),optional :: sub
    integer,        intent(in),   optional :: div
    integer,        intent(in),   optional :: lmem        ! total members

    integer rr
    integer stp(0:r-1), itr(0:r-1)
    integer idx(0:r-1)
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = total_members(bes, r, lmem)

    call set_slice_loop(rr, stp, itr, bes, r)
    idx(0:rr-1) = 0
    jsrc = init_offset(bes, r)
    jb = 0
    jdst = 0

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       ! write(*, *) jb, jdst, iseph, aposh, swap
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
             nt = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             nt = max(0, nt - ns)
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             m = min((je - jsrc), itr(0) - idx(0))
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_write(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             call next_offset(jsrc, idx, stp, itr, rr, m)
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       if (jsrc.lt.0.and.nt.eq.0) exit
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jsrc.ge.0), iseph, sub, swap)
  end subroutine sus_edit_slice_irec_f
  subroutine sus_edit_slice_irec_d &
       & (ierr, u, v, bes, r, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KDBL
    integer,        intent(out)            :: ierr
    integer,        intent(in)             :: u
    real(KIND=KARG),intent(in)             :: V(0:*)
    integer,        intent(in)             :: bes(3, 0:*) ! begin/end/stride triplet
    integer,        intent(in)             :: r           ! ranks
    logical,        intent(in),   optional :: swap
    logical,        intent(inout),optional :: sub
    integer,        intent(in),   optional :: div
    integer,        intent(in),   optional :: lmem        ! total members

    integer rr
    integer stp(0:r-1), itr(0:r-1)
    integer idx(0:r-1)
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = total_members(bes, r, lmem)

    call set_slice_loop(rr, stp, itr, bes, r)
    idx(0:rr-1) = 0
    jsrc = init_offset(bes, r)
    jb = 0
    jdst = 0

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       ! write(*, *) jb, jdst, iseph, aposh, swap
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
             nt = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             nt = max(0, nt - ns)
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             m = min((je - jsrc), itr(0) - idx(0))
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_write(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             call next_offset(jsrc, idx, stp, itr, rr, m)
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       if (jsrc.lt.0.and.nt.eq.0) exit
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jsrc.ge.0), iseph, sub, swap)
  end subroutine sus_edit_slice_irec_d
  subroutine sus_edit_slice_irec_a &
       & (ierr, u, v, bes, r, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,         intent(out)            :: ierr
    integer,         intent(in)             :: u
    character(len=*),intent(in)             :: V(0:*)
    integer,         intent(in)             :: bes(3, 0:*) ! begin/end/stride triplet
    integer,         intent(in)             :: r           ! ranks
    logical,         intent(in),   optional :: swap
    logical,         intent(inout),optional :: sub
    integer,         intent(in),   optional :: div
    integer,         intent(in),   optional :: lmem        ! total members

    integer rr
    integer stp(0:r-1), itr(0:r-1)
    integer idx(0:r-1)
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = total_members(bes, r, lmem)

    call set_slice_loop(rr, stp, itr, bes, r)
    idx(0:rr-1) = 0
    jsrc = init_offset(bes, r)
    jb = 0
    jdst = 0

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       ! write(*, *) jb, jdst, iseph, aposh, swap
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
             nt = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             nt = max(0, nt - ns)
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             m = min((je - jsrc), itr(0) - idx(0))
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_write(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             call next_offset(jsrc, idx, stp, itr, rr, m)
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       if (jsrc.lt.0.and.nt.eq.0) exit
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jsrc.ge.0), iseph, sub, swap)
  end subroutine sus_edit_slice_irec_a
!!!_  & sus_runl_read_irec - run-length read from a record with 32-bit marker
  subroutine sus_runl_read_irec_i &
       & (ierr, u, v, runl, n, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,           intent(out)            :: ierr
    integer,           intent(in)             :: u
    integer(KIND=KARG),intent(out)            :: V(0:*)
    integer,           intent(in)             :: runl(0:*) ! run-length list {skip,read,...}
    integer,           intent(in)             :: n         ! size of runl
    logical,           intent(in),   optional :: swap
    logical,           intent(inout),optional :: sub
    integer,           intent(in),   optional :: div
    integer,           intent(in),   optional :: lmem      ! total members
    ! note
    !    set initial skip = 0 if starting from origin
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy
    integer jr, rfin, nrem

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = choice(0, lmem)

    jsrc = 0
    jb = 0
    jdst = 0
    jr = 0
    nrem = 0

    rfin = (n / 2) * 2

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (nrem.eq.0) then
                if (jr.ge.rfin) exit
                jsrc = jsrc + runl(jr)
                jr = jr + 1
                nrem = runl(jr)
                jr = jr + 1
             endif
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             m = min((je - jsrc), nrem)
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_read(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             jsrc = jsrc + m
             nrem = nrem - m
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jr.lt.rfin), iseph, sub, swap)
  end subroutine sus_runl_read_irec_i
  subroutine sus_runl_read_irec_l &
       & (ierr, u, v, runl, n, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI64
    integer,           intent(out)            :: ierr
    integer,           intent(in)             :: u
    integer(KIND=KARG),intent(out)            :: V(0:*)
    integer,           intent(in)             :: runl(0:*) ! run-length list {skip,read,...}
    integer,           intent(in)             :: n         ! size of runl
    logical,           intent(in),   optional :: swap
    logical,           intent(inout),optional :: sub
    integer,           intent(in),   optional :: div
    integer,           intent(in),   optional :: lmem      ! total members
    ! note
    !    set initial skip = 0 if starting from origin
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy
    integer jr, rfin, nrem

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = choice(0, lmem)

    jsrc = 0
    jb = 0
    jdst = 0
    jr = 0
    nrem = 0

    rfin = (n / 2) * 2

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (nrem.eq.0) then
                if (jr.ge.rfin) exit
                jsrc = jsrc + runl(jr)
                jr = jr + 1
                nrem = runl(jr)
                jr = jr + 1
             endif
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             m = min((je - jsrc), nrem)
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_read(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             jsrc = jsrc + m
             nrem = nrem - m
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jr.lt.rfin), iseph, sub, swap)
  end subroutine sus_runl_read_irec_l
  subroutine sus_runl_read_irec_f &
       & (ierr, u, v, runl, n, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KFLT
    integer,           intent(out)            :: ierr
    integer,           intent(in)             :: u
    real(KIND=KARG),   intent(out)            :: V(0:*)
    integer,           intent(in)             :: runl(0:*) ! run-length list {skip,read,...}
    integer,           intent(in)             :: n         ! size of runl
    logical,           intent(in),   optional :: swap
    logical,           intent(inout),optional :: sub
    integer,           intent(in),   optional :: div
    integer,           intent(in),   optional :: lmem      ! total members
    ! note
    !    set initial skip = 0 if starting from origin
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy
    integer jr, rfin, nrem

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = choice(0, lmem)

    jsrc = 0
    jb = 0
    jdst = 0
    jr = 0
    nrem = 0

    rfin = (n / 2) * 2

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (nrem.eq.0) then
                if (jr.ge.rfin) exit
                jsrc = jsrc + runl(jr)
                jr = jr + 1
                nrem = runl(jr)
                jr = jr + 1
             endif
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             m = min((je - jsrc), nrem)
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_read(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             jsrc = jsrc + m
             nrem = nrem - m
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jr.lt.rfin), iseph, sub, swap)
  end subroutine sus_runl_read_irec_f
  subroutine sus_runl_read_irec_d &
       & (ierr, u, v, runl, n, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KDBL
    integer,           intent(out)            :: ierr
    integer,           intent(in)             :: u
    real(KIND=KARG),   intent(out)            :: V(0:*)
    integer,           intent(in)             :: runl(0:*) ! run-length list {skip,read,...}
    integer,           intent(in)             :: n         ! size of runl
    logical,           intent(in),   optional :: swap
    logical,           intent(inout),optional :: sub
    integer,           intent(in),   optional :: div
    integer,           intent(in),   optional :: lmem      ! total members
    ! note
    !    set initial skip = 0 if starting from origin
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy
    integer jr, rfin, nrem

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = choice(0, lmem)

    jsrc = 0
    jb = 0
    jdst = 0
    jr = 0
    nrem = 0

    rfin = (n / 2) * 2

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (nrem.eq.0) then
                if (jr.ge.rfin) exit
                jsrc = jsrc + runl(jr)
                jr = jr + 1
                nrem = runl(jr)
                jr = jr + 1
             endif
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             m = min((je - jsrc), nrem)
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_read(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             jsrc = jsrc + m
             nrem = nrem - m
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jr.lt.rfin), iseph, sub, swap)
  end subroutine sus_runl_read_irec_d
  subroutine sus_runl_read_irec_a &
       & (ierr, u, v, runl, n, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,           intent(out)            :: ierr
    integer,           intent(in)             :: u
    character(len=*),  intent(out)            :: V(0:*)
    integer,           intent(in)             :: runl(0:*) ! run-length list {skip,read,...}
    integer,           intent(in)             :: n         ! size of runl
    logical,           intent(in),   optional :: swap
    logical,           intent(inout),optional :: sub
    integer,           intent(in),   optional :: div
    integer,           intent(in),   optional :: lmem      ! total members
    ! note
    !    set initial skip = 0 if starting from origin
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy
    integer jr, rfin, nrem

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = choice(0, lmem)

    jsrc = 0
    jb = 0
    jdst = 0
    jr = 0
    nrem = 0

    rfin = (n / 2) * 2

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (nrem.eq.0) then
                if (jr.ge.rfin) exit
                jsrc = jsrc + runl(jr)
                jr = jr + 1
                nrem = runl(jr)
                jr = jr + 1
             endif
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             m = min((je - jsrc), nrem)
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_read(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             jsrc = jsrc + m
             nrem = nrem - m
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jr.lt.rfin), iseph, sub, swap)
  end subroutine sus_runl_read_irec_a
!!!_  & sus_list_read_irec - index-list read from a record with 32-bit marker
!!!_   & sus_list_read_irec_[ilfda]
  subroutine sus_list_read_irec_i &
       & (ierr, u, v, list, n, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,           intent(out)            :: ierr
    integer,           intent(in)             :: u
    integer(KIND=KARG),intent(out)            :: V(0:*)
    integer,           intent(in)             :: list(0:*) ! index list (must be ascendingly sorted)
    integer,           intent(in)             :: n         ! size of list
    logical,           intent(in),   optional :: swap
    logical,           intent(inout),optional :: sub
    integer,           intent(in),   optional :: div
    integer,           intent(in),   optional :: lmem        ! total members
    ! note
    !    set initial read = 0 if starting from skip.
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy
    integer jr

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = choice(0, lmem)

    jb = 0
    jdst = 0
    jr = 0
    jsrc = list(jr)
    m = 1

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_read(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             jr = jr + 1
             if (jr.ge.n) exit
             jsrc = list(jr)
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jr.lt.n), iseph, sub, swap)
  end subroutine sus_list_read_irec_i
  subroutine sus_list_read_irec_l &
       & (ierr, u, v, list, n, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI64
    integer,           intent(out)            :: ierr
    integer,           intent(in)             :: u
    integer(KIND=KARG),intent(out)            :: V(0:*)
    integer,           intent(in)             :: list(0:*) ! index list (must be ascendingly sorted)
    integer,           intent(in)             :: n         ! size of list
    logical,           intent(in),   optional :: swap
    logical,           intent(inout),optional :: sub
    integer,           intent(in),   optional :: div
    integer,           intent(in),   optional :: lmem        ! total members
    ! note
    !    set initial read = 0 if starting from skip.
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy
    integer jr

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = choice(0, lmem)

    jb = 0
    jdst = 0
    jr = 0
    jsrc = list(jr)
    m = 1

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_read(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             jr = jr + 1
             if (jr.ge.n) exit
             jsrc = list(jr)
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jr.lt.n), iseph, sub, swap)
  end subroutine sus_list_read_irec_l
  subroutine sus_list_read_irec_f &
       & (ierr, u, v, list, n, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KFLT
    integer,           intent(out)            :: ierr
    integer,           intent(in)             :: u
    real(KIND=KARG),   intent(out)            :: V(0:*)
    integer,           intent(in)             :: list(0:*) ! index list (must be ascendingly sorted)
    integer,           intent(in)             :: n         ! size of list
    logical,           intent(in),   optional :: swap
    logical,           intent(inout),optional :: sub
    integer,           intent(in),   optional :: div
    integer,           intent(in),   optional :: lmem        ! total members
    ! note
    !    set initial read = 0 if starting from skip.
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy
    integer jr

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = choice(0, lmem)

    jb = 0
    jdst = 0
    jr = 0
    jsrc = list(jr)
    m = 1

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_read(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             jr = jr + 1
             if (jr.ge.n) exit
             jsrc = list(jr)
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jr.lt.n), iseph, sub, swap)
  end subroutine sus_list_read_irec_f
  subroutine sus_list_read_irec_d &
       & (ierr, u, v, list, n, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KDBL
    integer,           intent(out)            :: ierr
    integer,           intent(in)             :: u
    real(KIND=KARG),   intent(out)            :: V(0:*)
    integer,           intent(in)             :: list(0:*) ! index list (must be ascendingly sorted)
    integer,           intent(in)             :: n         ! size of list
    logical,           intent(in),   optional :: swap
    logical,           intent(inout),optional :: sub
    integer,           intent(in),   optional :: div
    integer,           intent(in),   optional :: lmem        ! total members
    ! note
    !    set initial read = 0 if starting from skip.
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy
    integer jr

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = choice(0, lmem)

    jb = 0
    jdst = 0
    jr = 0
    jsrc = list(jr)
    m = 1

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_read(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             jr = jr + 1
             if (jr.ge.n) exit
             jsrc = list(jr)
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jr.lt.n), iseph, sub, swap)
  end subroutine sus_list_read_irec_d
  subroutine sus_list_read_irec_a &
       & (ierr, u, v, list, n, swap, sub, div, lmem)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss, get_unit_strm
    use TOUZA_Std_env,only: get_size_bytes, get_size_strm
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,           intent(out)            :: ierr
    integer,           intent(in)             :: u
    character(len=*),  intent(out)            :: V(0:*)
    integer,           intent(in)             :: list(0:*) ! index list (must be ascendingly sorted)
    integer,           intent(in)             :: n         ! size of list
    logical,           intent(in),   optional :: swap
    logical,           intent(inout),optional :: sub
    integer,           intent(in),   optional :: div
    integer,           intent(in),   optional :: lmem        ! total members
    ! note
    !    set initial read = 0 if starting from skip.
    integer jsrc, jdst
    integer m,  nt, ns
    integer jb, je
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aposh, aposf, apos
    integer us
    logical bdmy
    integer jr

    ierr = err_default
    us = get_unit_strm(V(0))
    nt = choice(0, lmem)

    jb = 0
    jdst = 0
    jr = 0
    jsrc = list(jr)
    m = 1

    loop_rec: do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit loop_rec
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, aposh, u)
       if (ierr.eq.0) then
          call check_dummy_irec(bdmy, nt, max_members(V(0)), get_size_bytes(V(0)), iseph, div)
          if (bdmy) then
             aposf = aposh + get_size_strm(V(0), int(nt, kind=KIOFS))
             je = jb + nt   ! == nt
             iseph = 0
          else
             aposf = aposh + conv_b2strm(abs(iseph))
             ns = get_mems_bytes(abs(iseph), V(0))
             je = jb + ns
          endif
       endif
       if (ierr.eq.0) then
          do
             if (jsrc.lt.0) exit
             if (jsrc.ge.je) exit
             apos = aposh + us * (jsrc - jb)
             if (ierr.eq.0) call sus_read(ierr, u, V(jdst:jdst+m-1), m, swap, apos)
             if (ierr.ne.0) exit loop_rec
             jdst = jdst + m
             jr = jr + 1
             if (jr.ge.n) exit
             jsrc = list(jr)
          enddo
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=aposf, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit loop_rec
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit loop_rec
       endif
       jb = je
    enddo loop_rec
    if (ierr.eq.0) call sus_access_irec_foot(ierr, u, (jr.lt.n), iseph, sub, swap)
  end subroutine sus_list_read_irec_a
!!!_  & sus_record_mems_irec - get size of 32bit-marker record in terms of type unit
  subroutine sus_record_mems_irec_i &
       & (ierr, n, u, mold, swap, sub)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,           intent(out)            :: ierr
    integer,           intent(out)            :: n
    integer,           intent(in)             :: u
    integer(kind=KARG),intent(in)             :: mold
    logical,           intent(in),   optional :: swap
    logical,           intent(inout),optional :: sub
    ! if no SUB or F: return total members
    !    SUB=T:       return sub members.   set F when no more subrecords
    integer ns
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aporg, apos
    logical bsub

    ierr = 0
    n = 0
    bsub = choice(.FALSE., sub)

    if (ierr.eq.0) call sus_getpos(ierr, aporg, u)
    if (ierr.eq.0) apos = aporg
    do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, apos, u)
       if (ierr.eq.0) then
          apos = apos + conv_b2strm(abs(iseph))
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=apos, swap=swap)
       if (ierr.eq.0) then
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.eq.0) then
          ns = get_mems_bytes(abs(iseph), mold)
          n = n + ns
          if (iseph.ge.0) exit
          if (bsub) exit
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit
       endif
    enddo
    if (present(sub)) then
       if (ierr.eq.0) sub = iseph.lt.0
    endif
    if (ierr.eq.0) call sus_rseek(ierr, u, aporg, whence=WHENCE_ABS)
  end subroutine sus_record_mems_irec_i
  subroutine sus_record_mems_irec_l &
       & (ierr, n, u, mold, swap, sub)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI64
    integer,           intent(out)            :: ierr
    integer,           intent(out)            :: n
    integer,           intent(in)             :: u
    integer(kind=KARG),intent(in)             :: mold
    logical,           intent(in),   optional :: swap
    logical,           intent(inout),optional :: sub
    ! if no SUB or F: return total members
    !    SUB=T:       return sub members.   set F when no more subrecords
    integer ns
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aporg, apos
    logical bsub

    ierr = 0
    n = 0
    bsub = choice(.FALSE., sub)

    if (ierr.eq.0) call sus_getpos(ierr, aporg, u)
    if (ierr.eq.0) apos = aporg
    do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, apos, u)
       if (ierr.eq.0) then
          apos = apos + conv_b2strm(abs(iseph))
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=apos, swap=swap)
       if (ierr.eq.0) then
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.eq.0) then
          ns = get_mems_bytes(abs(iseph), mold)
          n = n + ns
          if (iseph.ge.0) exit
          if (bsub) exit
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit
       endif
    enddo
    if (present(sub)) then
       if (ierr.eq.0) sub = iseph.lt.0
    endif
    if (ierr.eq.0) call sus_rseek(ierr, u, aporg, whence=WHENCE_ABS)
  end subroutine sus_record_mems_irec_l
  subroutine sus_record_mems_irec_f &
       & (ierr, n, u, mold, swap, sub)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KFLT
    integer,        intent(out)            :: ierr
    integer,        intent(out)            :: n
    integer,        intent(in)             :: u
    real(kind=KARG),intent(in)             :: mold
    logical,        intent(in),   optional :: swap
    logical,        intent(inout),optional :: sub
    ! if no SUB or F: return total members
    !    SUB=T:       return sub members.   set F when no more subrecords
    integer ns
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aporg, apos
    logical bsub

    ierr = 0
    n = 0
    bsub = choice(.FALSE., sub)

    if (ierr.eq.0) call sus_getpos(ierr, aporg, u)
    if (ierr.eq.0) apos = aporg
    do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, apos, u)
       if (ierr.eq.0) then
          apos = apos + conv_b2strm(abs(iseph))
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=apos, swap=swap)
       if (ierr.eq.0) then
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.eq.0) then
          ns = get_mems_bytes(abs(iseph), mold)
          n = n + ns
          if (iseph.ge.0) exit
          if (bsub) exit
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit
       endif
    enddo
    if (present(sub)) then
       if (ierr.eq.0) sub = iseph.lt.0
    endif
    if (ierr.eq.0) call sus_rseek(ierr, u, aporg, whence=WHENCE_ABS)
  end subroutine sus_record_mems_irec_f
  subroutine sus_record_mems_irec_d &
       & (ierr, n, u, mold, swap, sub)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KDBL
    integer,        intent(out)            :: ierr
    integer,        intent(out)            :: n
    integer,        intent(in)             :: u
    real(kind=KARG),intent(in)             :: mold
    logical,        intent(in),   optional :: swap
    logical,        intent(inout),optional :: sub
    ! if no SUB or F: return total members
    !    SUB=T:       return sub members.   set F when no more subrecords
    integer ns
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: aporg, apos
    logical bsub

    ierr = 0
    n = 0
    bsub = choice(.FALSE., sub)

    if (ierr.eq.0) call sus_getpos(ierr, aporg, u)
    if (ierr.eq.0) apos = aporg
    do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = _ERROR(ERR_EOF)
             exit
          endif
       endif
       if (ierr.eq.0) call sus_getpos(ierr, apos, u)
       if (ierr.eq.0) then
          apos = apos + conv_b2strm(abs(iseph))
       endif
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=apos, swap=swap)
       if (ierr.eq.0) then
          if (abs(iseph).ne.abs(isepf)) ierr = _ERROR(ERR_INCONSISTENT_RECORD_MARKERS)
       endif
       if (ierr.eq.0) then
          ns = get_mems_bytes(abs(iseph), mold)
          n = n + ns
          if (iseph.ge.0) exit
          if (bsub) exit
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit
       endif
    enddo
    if (present(sub)) then
       if (ierr.eq.0) sub = iseph.lt.0
    endif
    if (ierr.eq.0) call sus_rseek(ierr, u, aporg, whence=WHENCE_ABS)
  end subroutine sus_record_mems_irec_d
!!!_ + data/separator set
!!!_  & sus_access_irec_foot
    subroutine sus_access_irec_foot &
       & (ierr, u, cond, iseph, sub, swap)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI32
    integer,            intent(out)   :: ierr
    integer,            intent(in)    :: u
    logical,            intent(in)    :: cond
    integer(kind=KISEP),intent(in)    :: iseph
    logical,optional,   intent(inout) :: sub
    logical,optional,   intent(in)    :: swap
    ierr = 0
    if (ierr.eq.0) then
       if (cond) then
          ierr = _ERROR(ERR_INVALID_RECORD_SIZE)
       else if (choice(.false., sub)) then
          ! sub exist and T
          sub = (iseph.lt.0)    ! subrecord succeeds
       else if (iseph.lt.0) then
          ! skip all
          call sus_skip_irec(ierr, u, 1, swap=swap)
       endif
    endif
  end subroutine sus_access_irec_foot
!!!_  & sus_write_iset
  subroutine sus_write_iset_i &
       & (ierr, u, v, n, isep, pre, post, swap, jpos, whence)
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u         ! file unit
    integer(KIND=KARG), intent(in)          :: V(*)      ! data array
    integer,            intent(in)          :: n         ! size of V (no check)
    integer(kind=KISEP),intent(in)          :: isep      ! 32-bit separator
    logical,            intent(in)          :: pre, post ! continuation flag
    logical,            intent(in)          :: swap      ! byte-order switch
    integer(kind=KIOFS),intent(in),optional :: jpos      ! position in stream unit
    integer,            intent(in),optional :: whence
    ierr = 0
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, post, swap, jpos, whence)
    if (ierr.eq.0) call sus_write(ierr, u, V, n, swap)
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, pre, swap)
  end subroutine sus_write_iset_i
  subroutine sus_write_iset_l &
       & (ierr, u, v, n, isep, pre, post, swap, jpos, whence)
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI64
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(in)          :: V(*)
    integer,            intent(in)          :: n
    integer(kind=KISEP),intent(in)          :: isep
    logical,            intent(in)          :: pre, post
    logical,            intent(in)          :: swap
    integer(kind=KIOFS),intent(in),optional :: jpos      ! position in stream unit
    integer,            intent(in),optional :: whence
    ierr = 0
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, post, swap, jpos, whence)
    if (ierr.eq.0) call sus_write(ierr, u, V, n, swap)
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, pre, swap)
  end subroutine sus_write_iset_l
  subroutine sus_write_iset_f &
       & (ierr, u, v, n, isep, pre, post, swap, jpos, whence)
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KFLT
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    real(KIND=KARG),    intent(in)          :: V(*)
    integer,            intent(in)          :: n
    integer(kind=KISEP),intent(in)          :: isep
    logical,            intent(in)          :: pre, post
    logical,            intent(in)          :: swap
    integer(kind=KIOFS),intent(in),optional :: jpos      ! position in stream unit
    integer,            intent(in),optional :: whence
    ierr = 0
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, post, swap, jpos, whence)
    if (ierr.eq.0) call sus_write(ierr, u, V, n, swap)
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, pre, swap)
  end subroutine sus_write_iset_f
  subroutine sus_write_iset_d &
       & (ierr, u, v, n, isep, pre, post, swap, jpos, whence)
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KDBL
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    real(KIND=KARG),    intent(in)          :: V(*)
    integer,            intent(in)          :: n
    integer(kind=KISEP),intent(in)          :: isep
    logical,            intent(in)          :: pre, post
    logical,            intent(in)          :: swap
    integer(kind=KIOFS),intent(in),optional :: jpos      ! position in stream unit
    integer,            intent(in),optional :: whence
    ierr = 0
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, post, swap, jpos, whence)
    if (ierr.eq.0) call sus_write(ierr, u, V, n, swap)
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, pre, swap)
  end subroutine sus_write_iset_d
  subroutine sus_write_iset_a &
       & (ierr, u, v, n, isep, pre, post, swap, jpos, whence)
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KDBL
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    character(len=*),   intent(in)          :: V(*)
    integer,            intent(in)          :: n
    integer(kind=KISEP),intent(in)          :: isep
    logical,            intent(in)          :: pre, post
    logical,            intent(in)          :: swap
    integer(kind=KIOFS),intent(in),optional :: jpos      ! position in stream unit
    integer,            intent(in),optional :: whence
    ierr = 0
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, post, swap, jpos, whence)
    if (ierr.eq.0) call sus_write(ierr, u, V, n, swap)
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, pre, swap)
  end subroutine sus_write_iset_a
!!!_  & sus_pad_iset
  subroutine sus_pad_iset_i &
       & (ierr, u, v, n, isep, pre, post, swap, jpos, whence)
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(in)          :: V
    integer,            intent(in)          :: n
    integer(kind=KISEP),intent(in)          :: isep
    logical,            intent(in)          :: pre, post
    logical,            intent(in)          :: swap
    integer(kind=KIOFS),intent(in),optional :: jpos      ! position in stream unit
    integer,            intent(in),optional :: whence
    ierr = 0
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, post, swap, jpos, whence)
    if (ierr.eq.0) call sus_pad(ierr, u, V, n, swap)
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, pre, swap)
  end subroutine sus_pad_iset_i
  subroutine sus_pad_iset_l &
       & (ierr, u, v, n, isep, pre, post, swap, jpos, whence)
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI64
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(in)          :: V
    integer,            intent(in)          :: n
    integer(kind=KISEP),intent(in)          :: isep
    logical,            intent(in)          :: pre, post
    logical,            intent(in)          :: swap
    integer(kind=KIOFS),intent(in),optional :: jpos      ! position in stream unit
    integer,            intent(in),optional :: whence
    ierr = 0
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, post, swap, jpos, whence)
    if (ierr.eq.0) call sus_pad(ierr, u, V, n, swap)
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, pre, swap)
  end subroutine sus_pad_iset_l
  subroutine sus_pad_iset_f &
       & (ierr, u, v, n, isep, pre, post, swap, jpos, whence)
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KFLT
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    real(KIND=KARG),    intent(in)          :: V
    integer,            intent(in)          :: n
    integer(kind=KISEP),intent(in)          :: isep
    logical,            intent(in)          :: pre, post
    logical,            intent(in)          :: swap
    integer(kind=KIOFS),intent(in),optional :: jpos      ! position in stream unit
    integer,            intent(in),optional :: whence
    ierr = 0
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, post, swap, jpos, whence)
    if (ierr.eq.0) call sus_pad(ierr, u, V, n, swap)
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, pre, swap)
  end subroutine sus_pad_iset_f
  subroutine sus_pad_iset_d &
       & (ierr, u, v, n, isep, pre, post, swap, jpos, whence)
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KDBL
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    real(KIND=KARG),    intent(in)          :: V
    integer,            intent(in)          :: n
    integer(kind=KISEP),intent(in)          :: isep
    logical,            intent(in)          :: pre, post
    logical,            intent(in)          :: swap
    integer(kind=KIOFS),intent(in),optional :: jpos      ! position in stream unit
    integer,            intent(in),optional :: whence
    ierr = 0
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, post, swap, jpos, whence)
    if (ierr.eq.0) call sus_pad(ierr, u, V, n, swap)
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, pre, swap)
  end subroutine sus_pad_iset_d
  subroutine sus_pad_iset_a &
       & (ierr, u, v, n, isep, pre, post, swap, jpos, whence)
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    character(len=*),   intent(in)          :: V
    integer,            intent(in)          :: n
    integer(kind=KISEP),intent(in)          :: isep
    logical,            intent(in)          :: pre, post
    logical,            intent(in)          :: swap
    integer(kind=KIOFS),intent(in),optional :: jpos      ! position in stream unit
    integer,            intent(in),optional :: whence
    ierr = 0
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, post, swap, jpos, whence)
    if (ierr.eq.0) call sus_pad(ierr, u, V, n, swap)
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, pre, swap)
  end subroutine sus_pad_iset_a
!!!_  & sus_blank_iset
  subroutine sus_blank_iset &
       & (ierr, u, skip, isep, pre, post, swap, jpos, whence)
    implicit none
    integer,parameter :: KISEP=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(kind=KIOFS),intent(in)          :: skip
    integer(kind=KISEP),intent(in)          :: isep
    logical,            intent(in)          :: pre, post
    logical,            intent(in)          :: swap
    integer(kind=KIOFS),intent(in),optional :: jpos      ! position in stream unit
    integer,            intent(in),optional :: whence
    integer(kind=KIOFS) :: jposf
    ierr = 0
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, post, swap, jpos, whence)
    if (ierr.eq.0) call sus_getpos(ierr, jposf, u)
    if (ierr.eq.0) call sus_write_isep(ierr, u, isep, pre,  swap, jposf + skip)
  end subroutine sus_blank_iset
!!!_ + separator
!!!_  & sus_write_isep - write 32-bit separator
  subroutine sus_write_isep_i (ierr, u, sep, sub, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(in)          :: sep
    logical,            intent(in),optional :: sub
    logical,            intent(in),optional :: swap
    integer(KIND=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(KIND=KISEP) :: isep
    integer(KIND=KIOFS) :: apos

    isep = sep
    if (choice(.false., sub)) isep = - isep
    if (choice(.false., swap)) isep = sus_eswap(isep)
    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       write(UNIT=u, IOSTAT=ierr, POS=apos) isep
    else
       write(UNIT=u, IOSTAT=ierr) isep
    endif
  end subroutine sus_write_isep_i
  subroutine sus_write_isep_l (ierr, u, sep, sub, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI64
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(in)          :: sep
    logical,            intent(in),optional :: sub
    logical,            intent(in),optional :: swap
    integer(KIND=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(KIND=KISEP) :: isep
    integer(KIND=KIOFS) :: apos

    ! todo: check overflow
    isep = int(sep, KIND=KISEP)
    if (choice(.false., sub)) isep = - isep
    if (choice(.false., swap)) isep = sus_eswap(isep)
    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       write(UNIT=u, IOSTAT=ierr, POS=apos) isep
    else
       write(UNIT=u, IOSTAT=ierr) isep
    endif
  end subroutine sus_write_isep_l
!!!_  & sus_write_lsep - write 64-bit separator
  subroutine sus_write_lsep_i (ierr, u, sep, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(in)          :: sep
    logical,            intent(in),optional :: swap
    integer(KIND=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(KIND=KISEP) :: lsep
    integer(KIND=KIOFS) :: apos

    if (choice(.false., swap)) then
       lsep = sus_eswap(sep)
    else
       lsep = sep
    endif
    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       write(UNIT=u, IOSTAT=ierr, POS=apos) lsep
    else
       write(UNIT=u, IOSTAT=ierr) lsep
    endif
  end subroutine sus_write_lsep_i
  subroutine sus_write_lsep_l (ierr, u, sep, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KI64
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(in)          :: sep
    logical,            intent(in),optional :: swap
    integer(KIND=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(KIND=KISEP) :: lsep
    integer(KIND=KIOFS) :: apos

    if (choice(.false., swap)) then
       lsep = sus_eswap(sep)
    else
       lsep = sep
    endif
    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       write(UNIT=u, IOSTAT=ierr, POS=apos) lsep
    else
       write(UNIT=u, IOSTAT=ierr) lsep
    endif
  end subroutine sus_write_lsep_l
!!!_  & sus_read_isep - read 32-bit separator
  subroutine sus_read_isep_i (ierr, u, sep, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(out)         :: sep
    integer(KIND=KIOFS),intent(in),optional :: pos
    logical,            intent(in),optional :: swap
    integer,            intent(in),optional :: whence
    integer(KIND=KIOFS) :: apos

    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       read(UNIT=u, IOSTAT=ierr, POS=apos) sep
    else
       read(UNIT=u, IOSTAT=ierr) sep
    endif
    if (choice(.false., swap)) then
       if (ierr.eq.0) sep = sus_eswap(sep)
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_read_isep_i
  subroutine sus_read_isep_l (ierr, u, sep, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI64
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(out)         :: sep
    integer(KIND=KIOFS),intent(in),optional :: pos
    logical,            intent(in),optional :: swap
    integer,            intent(in),optional :: whence
    integer(KIND=KISEP) :: isep
    integer(KIND=KIOFS) :: apos

    ! todo: check overflow
    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       read(UNIT=u, IOSTAT=ierr, POS=apos) isep
    else
       read(UNIT=u, IOSTAT=ierr) isep
    endif
    if (choice(.false., swap)) then
       if (ierr.eq.0) then
          isep = sus_eswap(isep)
          sep = isep
       endif
    else
       if (ierr.eq.0) sep = isep
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_read_isep_l
!!!_  & sus_read_lsep - read 64-bit separator
  subroutine sus_read_lsep_i (ierr, u, sep, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(out)         :: sep
    integer(KIND=KIOFS),intent(in),optional :: pos
    logical,            intent(in),optional :: swap
    integer,            intent(in),optional :: whence
    integer(KIND=KISEP) :: lsep
    integer(KIND=KIOFS) :: apos

    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       read(UNIT=u, IOSTAT=ierr, POS=apos) lsep
    else
       read(UNIT=u, IOSTAT=ierr) lsep
    endif
    ! to do: check overflow
    if (choice(.false., swap)) then
       if (ierr.eq.0) then
          lsep = sus_eswap(lsep)
          sep = int(lsep, KIND=KARG)
       endif
    else
       if (ierr.eq.0) sep = int(lsep, KIND=KARG)
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_read_lsep_i
  subroutine sus_read_lsep_l (ierr, u, sep, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KI64
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(out)         :: sep
    integer(KIND=KIOFS),intent(in),optional :: pos
    logical,            intent(in),optional :: swap
    integer,            intent(in),optional :: whence
    integer(KIND=KIOFS) :: apos

    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       read(UNIT=u, IOSTAT=ierr, POS=apos) sep
    else
       read(UNIT=u, IOSTAT=ierr) sep
    endif
    if (choice(.false., swap)) then
       if (ierr.eq.0) sep = sus_eswap(sep)
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_read_lsep_l
!!!_ + data
!!!_  & sus_write - write data with optional byte-swapping
  subroutine sus_write_i &
       & (ierr, u, v, n, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(in)          :: V(*)
    integer,            intent(in)          :: n
    logical,            intent(in),optional :: swap
    integer(kind=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(KIND=KIOFS) :: apos
    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       if (choice(.false.,swap)) then
          write(UNIT=u, IOSTAT=ierr, POS=apos) sus_eswap(V(1:n))
       else
          write(UNIT=u, IOSTAT=ierr, POS=apos) V(1:n)
       endif
    else
       if (choice(.false.,swap)) then
          write(UNIT=u, IOSTAT=ierr) sus_eswap(V(1:n))
       else
          write(UNIT=u, IOSTAT=ierr) V(1:n)
       endif
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_write_i
  subroutine sus_write_l &
       & (ierr, u, v, n, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KI64
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(in)          :: V(*)
    integer,            intent(in)          :: n
    logical,            intent(in),optional :: swap
    integer(kind=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(KIND=KIOFS) :: apos

    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       if (choice(.false.,swap)) then
          write(UNIT=u, IOSTAT=ierr, POS=apos) sus_eswap(V(1:n))
       else
          write(UNIT=u, IOSTAT=ierr, POS=apos) V(1:n)
       endif
    else
       if (choice(.false.,swap)) then
          write(UNIT=u, IOSTAT=ierr) sus_eswap(V(1:n))
       else
          write(UNIT=u, IOSTAT=ierr) V(1:n)
       endif
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_write_l
  subroutine sus_write_f &
       & (ierr, u, v, n, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KFLT, KBUF=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    real(KIND=KARG),    intent(in)          :: V(*)
    integer,            intent(in)          :: n
    logical,            intent(in),optional :: swap
    integer(kind=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(KIND=KIOFS) :: apos

    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       if (choice(.false.,swap)) then
          write(UNIT=u, IOSTAT=ierr, POS=apos) sus_eswap(TRANSFER(V(1:n), 0_KBUF, n))
       else
          write(UNIT=u, IOSTAT=ierr, POS=apos) V(1:n)
       endif
    else
       if (choice(.false.,swap)) then
          write(UNIT=u, IOSTAT=ierr) sus_eswap(TRANSFER(V(1:n), 0_KBUF, n))
       else
          write(UNIT=u, IOSTAT=ierr) V(1:n)
       endif
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_write_f
  subroutine sus_write_d &
       & (ierr, u, v, n, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KDBL, KBUF=KI64
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    real(KIND=KARG),    intent(in)          :: V(*)
    integer,            intent(in)          :: n
    logical,            intent(in),optional :: swap
    integer(kind=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(KIND=KIOFS) :: apos

    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       if (choice(.false.,swap)) then
          write(UNIT=u, IOSTAT=ierr, POS=apos) sus_eswap(TRANSFER(V(1:n), 0_KBUF, n))
       else
          write(UNIT=u, IOSTAT=ierr, POS=apos) V(1:n)
       endif
    else
       if (choice(.false.,swap)) then
          write(UNIT=u, IOSTAT=ierr) sus_eswap(TRANSFER(V(1:n), 0_KBUF, n))
       else
          write(UNIT=u, IOSTAT=ierr) V(1:n)
       endif
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_write_d
  subroutine sus_write_a &
       & (ierr, u, v, n, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    character(len=*),   intent(in)          :: V(*)
    integer,            intent(in)          :: n
    logical,            intent(in),optional :: swap
    integer(kind=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(KIND=KIOFS) :: apos

    if (present(swap)) continue
    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       write(UNIT=u, IOSTAT=ierr, POS=apos) V(1:n)
    else
       write(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_write_a
!!!_  & sus_pad - pad single data with optional byte-swapping
  subroutine sus_pad_i &
       & (ierr, u, v, n, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(in)          :: V
    integer,            intent(in)          :: n
    logical,            intent(in),optional :: swap
    integer(kind=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(KIND=KIOFS) :: apos
    integer j
    integer(kind=KARG) :: B
    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       if (choice(.false.,swap)) then
          B = sus_eswap(V)
          write(UNIT=u, IOSTAT=ierr, POS=apos) (B, j=0, n-1)
       else
          write(UNIT=u, IOSTAT=ierr, POS=apos) (V, j=0, n-1)
       endif
    else
       if (choice(.false.,swap)) then
          B = sus_eswap(V)
          write(UNIT=u, IOSTAT=ierr) (B, j=0, n-1)
       else
          write(UNIT=u, IOSTAT=ierr) (V, j=0, n-1)
       endif
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_pad_i
  subroutine sus_pad_l &
       & (ierr, u, v, n, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KI64
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(in)          :: V
    integer,            intent(in)          :: n
    logical,            intent(in),optional :: swap
    integer(kind=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(KIND=KIOFS) :: apos
    integer j
    integer(kind=KARG) :: B
    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       if (choice(.false.,swap)) then
          B = sus_eswap(V)
          write(UNIT=u, IOSTAT=ierr, POS=apos) (B, j=0, n-1)
       else
          write(UNIT=u, IOSTAT=ierr, POS=apos) (V, j=0, n-1)
       endif
    else
       if (choice(.false.,swap)) then
          B = sus_eswap(V)
          write(UNIT=u, IOSTAT=ierr) (B, j=0, n-1)
       else
          write(UNIT=u, IOSTAT=ierr) (V, j=0, n-1)
       endif
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_pad_l
  subroutine sus_pad_f &
       & (ierr, u, v, n, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KFLT, KBUF=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    real(KIND=KARG),    intent(in)          :: V
    integer,            intent(in)          :: n
    logical,            intent(in),optional :: swap
    integer(kind=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(KIND=KIOFS) :: apos
    integer j
    integer(kind=KBUF) :: B(1)

    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       if (choice(.false.,swap)) then
          B = sus_eswap(TRANSFER(V, 0_KBUF, 1))
          write(UNIT=u, IOSTAT=ierr, POS=apos) (B(1), j=0, n-1)
       else
          write(UNIT=u, IOSTAT=ierr, POS=apos) (V, j=0, n-1)
       endif
    else
       if (choice(.false.,swap)) then
          B = sus_eswap(TRANSFER(V, 0_KBUF, 1))
          write(UNIT=u, IOSTAT=ierr) (B(1), j=0, n-1)
       else
          write(UNIT=u, IOSTAT=ierr) (V, j=0, n-1)
       endif
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_pad_f
  subroutine sus_pad_d &
       & (ierr, u, v, n, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KDBL, KBUF=KI64
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    real(KIND=KARG),    intent(in)          :: V
    integer,            intent(in)          :: n
    logical,            intent(in),optional :: swap
    integer(kind=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(KIND=KIOFS) :: apos
    integer j
    integer(kind=KBUF) :: B(1)

    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       if (choice(.false.,swap)) then
          B = sus_eswap(TRANSFER(V, 0_KBUF, 1))
          write(UNIT=u, IOSTAT=ierr, POS=apos) (B(1), j=0, n-1)
       else
          write(UNIT=u, IOSTAT=ierr, POS=apos) (V, j=0, n-1)
       endif
    else
       if (choice(.false.,swap)) then
          B = sus_eswap(TRANSFER(V, 0_KBUF, 1))
          write(UNIT=u, IOSTAT=ierr) (B(1), j=0, n-1)
       else
          write(UNIT=u, IOSTAT=ierr) (V, j=0, n-1)
       endif
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_pad_d
  subroutine sus_pad_a &
       & (ierr, u, v, n, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KDBL, KBUF=KI64
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    character(len=*),   intent(in)          :: V
    integer,            intent(in)          :: n
    logical,            intent(in),optional :: swap
    integer(kind=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(KIND=KIOFS) :: apos
    integer j

    if (present(swap)) continue
    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       write(UNIT=u, IOSTAT=ierr, POS=apos) (V, j = 0, n - 1)
    else
       write(UNIT=u, IOSTAT=ierr) (V, j = 0, n - 1)
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_pad_a
!!!_  & sus_read - read data with optional byte-swapping
  subroutine sus_read_i &
       & (ierr, u, v, n, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(out)         :: V(*)
    integer,            intent(in)          :: n
    logical,            intent(in),optional :: swap
    integer(kind=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(KIND=KIOFS) :: apos

    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       read(UNIT=u, IOSTAT=ierr, POS=apos) V(1:n)
    else
       read(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
    if (choice(.false.,swap)) then
       if (ierr.eq.0) V(1:n) = sus_eswap(V(1:n))
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_read_i
  subroutine sus_read_l &
       & (ierr, u, v, n, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KI64
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(out)         :: V(*)
    integer,            intent(in)          :: n
    logical,            intent(in),optional :: swap
    integer(kind=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(KIND=KIOFS) :: apos

    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       read(UNIT=u, IOSTAT=ierr, POS=apos) V(1:n)
    else
       read(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
    if (choice(.false.,swap)) then
       if (ierr.eq.0) V(1:n) = sus_eswap(V(1:n))
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_read_l
  subroutine sus_read_f &
       & (ierr, u, v, n, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KFLT, KBUF=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    real(KIND=KARG),    intent(out)         :: V(*)
    integer,            intent(in)          :: n
    logical,            intent(in),optional :: swap
    integer(kind=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(KIND=KIOFS) :: apos
#if OPT_READ_SWAP_WITH_WORK
    integer(KIND=KBUF) :: W(n)
#endif /* OPT_READ_SWAP_WITH_WORK */
    if (choice(.false.,swap)) then
#if OPT_READ_SWAP_WITH_WORK
       if (present(pos)) then
          apos = sus_pos_r2abs(pos, u, whence)
          read(UNIT=u, IOSTAT=ierr, POS=apos) W(1:n)
       else
          read(UNIT=u, IOSTAT=ierr) W(1:n)
       endif
       if (ierr.eq.0) then
          V(1:n) = TRANSFER(sus_eswap(W(1:n)), 0.0_KARG, n)
       endif
#else /* not OPT_READ_SWAP_WITH_WORK */
       if (present(pos)) then
          apos = sus_pos_r2abs(pos, u, whence)
          read(UNIT=u, IOSTAT=ierr, POS=apos) V(1:n)
       else
          read(UNIT=u, IOSTAT=ierr) V(1:n)
       endif
       if (ierr.eq.0) then
          V(1:n) = TRANSFER(sus_eswap(TRANSFER(V(1:n), 0_KBUF, n)), 0.0_KARG, n)
       endif
#endif /* not OPT_READ_SWAP_WITH_WORK */
    else
       if (present(pos)) then
          apos = sus_pos_r2abs(pos, u, whence)
          read(UNIT=u, IOSTAT=ierr, POS=apos) V(1:n)
       else
          read(UNIT=u, IOSTAT=ierr) V(1:n)
       endif
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_read_f
  subroutine sus_read_d &
       & (ierr, u, v, n, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KDBL, KBUF=KI64
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    real(KIND=KARG),    intent(out)         :: V(*)
    integer,            intent(in)          :: n
    logical,            intent(in),optional :: swap
    integer(kind=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(KIND=KIOFS) :: apos
#if OPT_READ_SWAP_WITH_WORK
    integer(kind=KBUF) :: W(n)
#endif /* OPT_READ_SWAP_WITH_WORK */
    if (choice(.false.,swap)) then
#if OPT_READ_SWAP_WITH_WORK
       if (present(pos)) then
          apos = sus_pos_r2abs(pos, u, whence)
          read(UNIT=u, IOSTAT=ierr, POS=apos) W(1:n)
       else
          read(UNIT=u, IOSTAT=ierr) W(1:n)
       endif
       if (ierr.eq.0) then
          V(1:n) = TRANSFER(sus_eswap(W(1:n)), 0.0_KARG, n)
       endif
#else /* not OPT_READ_SWAP_WITH_WORK */
       if (present(pos)) then
          apos = sus_pos_r2abs(pos, u, whence)
          read(UNIT=u, IOSTAT=ierr, POS=apos) V(1:n)
       else
          read(UNIT=u, IOSTAT=ierr) V(1:n)
       endif
       if (ierr.eq.0) then
          V(1:n) = TRANSFER(sus_eswap(TRANSFER(V(1:n), 0_KBUF, n)), 0.0_KARG, n)
       endif
#endif /* not OPT_READ_SWAP_WITH_WORK */
    else
       if (present(pos)) then
          apos = sus_pos_r2abs(pos, u, whence)
          read(UNIT=u, IOSTAT=ierr, POS=apos) V(1:n)
       else
          read(UNIT=u, IOSTAT=ierr) V(1:n)
       endif
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_read_d
  subroutine sus_read_a &
       & (ierr, u, v, n, swap, pos, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    character(len=*),   intent(out)         :: V(*)
    integer,            intent(in)          :: n
    logical,            intent(in),optional :: swap
    integer(kind=KIOFS),intent(in),optional :: pos
    integer,            intent(in),optional :: whence
    integer(KIND=KIOFS) :: apos

    if (present(swap)) continue
    if (present(pos)) then
       apos = sus_pos_r2abs(pos, u, whence)
       read(UNIT=u, IOSTAT=ierr, POS=apos) V(1:n)
    else
       read(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_read_a
!!!_ + private subroutines
!!!_  & sus_eswap() - elemental
  ELEMENTAL &
       integer(KIND=KI32) function sus_eswap_i(V) &
       & result(R)
    implicit none
    integer,parameter :: KARG=KI32
    integer(KARG),intent(in) :: V
    integer,parameter :: LTGT = BIT_SIZE(0_KARG)
    integer j
    R = IBITS(V, 0, LBU)
    do j = LBU, LTGT - 1, LBU
       R = IOR(ISHFT(R, LBU), IBITS(V, j, LBU))
    enddo
  end function sus_eswap_i

  ELEMENTAL &
       integer(KIND=KI64) function sus_eswap_l(V) &
       & result(R)
    implicit none
    integer,parameter :: KARG=KI64
    integer(KARG),intent(in) :: V
    integer,parameter :: LTGT = BIT_SIZE(0_KARG)
    integer j
    R = IBITS(V, 0, LBU)
    do j = LBU, LTGT - 1, LBU
       R = IOR(ISHFT(R, LBU), IBITS(V, j, LBU))
    enddo
  end function sus_eswap_l
!!!_  & sus_eswap_hl() - elemental (higher/lower bits independent swap)
  ELEMENTAL &
       integer(KIND=KI64) function sus_eswap_hl(V) &
       & result(R)
    implicit none
    integer,parameter :: KARG=KI64
    integer(KARG),intent(in) :: V
    integer,parameter :: LTGT = BIT_SIZE(0_KARG)
    integer,parameter :: NHF = LTGT / 2
    integer(KARG) :: VH, VL
    integer j

    VL = IBITS(V, 0, LBU)
    do j = LBU, NHF - 1, LBU
       VL = IOR(ISHFT(VL, LBU), IBITS(V, j, LBU))
    enddo
    VH = IBITS(V, NHF - 1, LBU)
    do j = NHF + LBU, LTGT - 1, LBU
       VH = IOR(ISHFT(VH, LBU), IBITS(V, j, LBU))
    enddo

    R = IOR(ISHFT(VH, NHF), VL)
  end function sus_eswap_hl
!!!_  & sus_swap() - swap expanded
  ELEMENTAL &
       integer(KIND=KI32) function sus_swap_i(V) &
       & result(R)
    implicit none
    integer(kind=KI32),intent(in) :: V
    R = IOR(IOR(ISHFT(IBITS(V, 0*LBU, LBU), 3*LBU),  &
         &      ISHFT(IBITS(V, 1*LBU, LBU), 2*LBU)), &
         &  IOR(ISHFT(IBITS(V, 2*LBU, LBU), 1*LBU),  &
         &            IBITS(V, 3*LBU, LBU)))
    return
  end function sus_swap_i
  ELEMENTAL &
       integer(KIND=KI64) function sus_swap_l(V) &
       & result(R)
    implicit none
    integer(kind=KI64),intent(in) :: V
    R = IOR(IOR(IOR(ISHFT(IBITS(V, LBU*0, LBU), LBU*7),   &
         &          ISHFT(IBITS(V, LBU*1, LBU), LBU*6)),  &
         &      IOR(ISHFT(IBITS(V, LBU*2, LBU), LBU*5),   &
         &          ISHFT(IBITS(V, LBU*3, LBU), LBU*4))), &
         &  IOR(IOR(ISHFT(IBITS(V, LBU*4, LBU), LBU*3),   &
         &          ISHFT(IBITS(V, LBU*5, LBU), LBU*2)),  &
         &      IOR(ISHFT(IBITS(V, LBU*6, LBU), LBU*1),   &
         &          ISHFT(IBITS(V, LBU*7, LBU), LBU*0))))
    return
  end function sus_swap_l
!!!_  & sus_pos_a2rel() - convert absolute (system) position to various
  integer(kind=KIOFS) function sus_pos_a2rel &
       & (apos, u, whence) &
       & result(rpos)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer(kind=KIOFS),intent(in) :: apos      ! absolute pos, i.e., get by INQUIRE()
    integer,            intent(in) :: u
    integer,optional,   intent(in) :: whence
    integer wh
    integer jerr
    integer(kind=KIOFS) :: tmp
    integer(kind=KIOFS),parameter :: errp = - HUGE(0_KIOFS)
    integer(kind=KIOFS),parameter :: ONE  = 1_KIOFS

    wh = choice(WHENCE_ABS, whence)
    select case (wh)
    case (WHENCE_ABS)
       rpos = apos
    case(WHENCE_BEGIN)
       rpos = apos - ONE
    case(WHENCE_END)
       inquire(UNIT=u, IOSTAT=jerr, SIZE=tmp)
       if (jerr.eq.0) then
          rpos = apos - tmp - ONE
       else
          rpos = errp
       endif
    case(WHENCE_CURRENT)
       call sus_getpos(jerr, tmp, u)
       if (jerr.eq.0) then
          rpos = apos - tmp
       else
          rpos = errp
       endif
    case default
       rpos = errp
    end select
  end function sus_pos_a2rel

!!!_  & sus_pos_r2abs() - convert relative position to absolute
  integer(kind=KIOFS) function sus_pos_r2abs &
       & (rpos, u, whence) &
       & result(apos)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer(kind=KIOFS),intent(in) :: rpos
    integer,            intent(in) :: u
    integer,optional,   intent(in) :: whence
    integer wh
    integer jerr
    integer(kind=KIOFS) :: tmp
    integer(kind=KIOFS),parameter :: ONE  = 1_KIOFS

    wh = choice(WHENCE_ABS, whence)
    select case (wh)
    case (WHENCE_ABS)
       apos = rpos
    case(WHENCE_BEGIN)
       apos = rpos + ONE
    case(WHENCE_END)
       inquire(UNIT=u, IOSTAT=jerr, SIZE=tmp)
       if (jerr.eq.0) then
          apos = rpos + tmp + ONE
       else if (jerr.gt.0) then
          apos = _ERROR(ERR_PANIC)
       else
          apos = jerr
       endif
    case(WHENCE_CURRENT)
       call sus_getpos(jerr, tmp, u)
       if (jerr.eq.0) then
          apos = rpos + tmp
       else if (jerr.gt.0) then
          apos = _ERROR(ERR_PANIC)
       else
          apos = jerr
       endif
    case default
       apos = _ERROR(ERR_INVALID_SWITCH)
    end select
  end function sus_pos_r2abs

!!!_  & sus_getpos - get current position
  subroutine sus_getpos &
       & (ierr, pos, u, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,            intent(out)         :: ierr
    integer(KIND=KIOFS),intent(out)         :: pos
    integer,            intent(in)          :: u
    integer,            intent(in),optional :: whence

    integer wh
    integer(KIND=KIOFS) :: jpos
    integer(kind=KIOFS),parameter :: ONE  = 1_KIOFS

    inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
    if (ierr.eq.0) then
       wh = choice(WHENCE_ABS, whence)
       select case (wh)
       case (WHENCE_ABS)
          pos = jpos
       case(WHENCE_BEGIN)
          pos = jpos - ONE
       case(WHENCE_END)
          pos = jpos
          inquire(UNIT=u, IOSTAT=ierr, SIZE=jpos)
          if (ierr.eq.0) pos = pos - jpos - ONE
       case(WHENCE_CURRENT)
          pos = 0             ! meaningless....
       case default
          ierr = _ERROR(ERR_INVALID_SWITCH)
       end select
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
  end subroutine sus_getpos

!!!_  & sus_rseek - seek position to read
  subroutine sus_rseek &
       & (ierr, u, step, whence, fmt)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KIOFS),intent(in),optional :: step
    integer,            intent(in),optional :: whence
    logical,            intent(in),optional :: fmt     ! formatted or not

    integer(KIND=KIOFS) :: apos, st
    integer wh

    ierr = 0

    wh = choice(WHENCE_CURRENT, whence)
    st = choice(0_KIOFS, step)

    if (wh.eq.WHENCE_END) then
       inquire(UNIT=u, IOSTAT=ierr, SIZE=apos)
       if (ierr.eq.0) apos = apos + 1_KIOFS + st
    else if (wh.eq.WHENCE_BEGIN) then
       apos = 1_KIOFS + st
    else if (wh.eq.WHENCE_ABS) then
       apos = st
    else if (st.eq.0) then
       return
    else
       call sus_getpos(ierr, apos, u)
       if (ierr.eq.0) apos = apos + st
    endif

    if (ierr.eq.0) then
#if OPT_STREAM_RPOS_WORKAROUND
       call sus_rseek_workaround(ierr, u, apos)
#else /* not OPT_STREAM_RPOS_WORKAROUND */
       if (choice(.FALSE., fmt)) then
          read(UNIT=u, FMT=*, IOSTAT=ierr, POS=apos)
       else
          read(UNIT=u, IOSTAT=ierr, POS=apos)
       endif
#endif /* not OPT_STREAM_RPOS_WORKAROUND */
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
    return
  end subroutine sus_rseek
!!!_  & sus_rseek_workaround - seek position to read (workaround)
  subroutine sus_rseek_workaround &
       & (ierr, u, apos)
    !! caution: T assumed to be 1-byte
    use TOUZA_Std_env,only: nc_strm
    implicit none
    integer,            intent(out) :: ierr
    integer,            intent(in)  :: u
    integer(KIND=KIOFS),intent(in)  :: apos
    character T
    if (apos.le.1_KIOFS) then
       rewind(UNIT=u, IOSTAT=ierr)
    else
       read(u, IOSTAT=ierr, POS=(apos-nc_strm)) T
    endif
    if (ierr.gt.0) ierr = _ERROR(ERR_IO_GENERAL)
    return
  end subroutine sus_rseek_workaround
!!!_  & is_irec_overflow() - check if array size exceeds irec limit
  logical function is_irec_overflow_i(m, mold) result(b)
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(in) :: m
    integer(kind=KARG),intent(in) :: mold
    b = m .gt. max_members(mold)
  end function is_irec_overflow_i
  logical function is_irec_overflow_l(m, mold) result(b)
    implicit none
    integer,parameter :: KARG=KI64
    integer,           intent(in) :: m
    integer(kind=KARG),intent(in) :: mold
    b = m .gt. max_members(mold)
  end function is_irec_overflow_l
  logical function is_irec_overflow_f(m, mold) result(b)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,        intent(in) :: m
    real(kind=KARG),intent(in) :: mold
    b = m .gt. max_members(mold)
  end function is_irec_overflow_f
  logical function is_irec_overflow_d(m, mold) result(b)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,        intent(in) :: m
    real(kind=KARG),intent(in) :: mold
    b = m .gt. max_members(mold)
  end function is_irec_overflow_d
  logical function is_irec_overflow_a(m, mold) result(b)
    implicit none
    integer,         intent(in) :: m
    character(len=*),intent(in) :: mold
    b = m .gt. max_members(mold)
  end function is_irec_overflow_a
!!!_  & is_irec_overflow_mix
  logical function is_irec_overflow_mix(ni, nl, nf, nd) result(b)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,intent(in),optional :: ni  ! size of integer
    integer,intent(in),optional :: nl  ! size of long integer
    integer,intent(in),optional :: nf  ! size of float
    integer,intent(in),optional :: nd  ! size of double
    integer m
    integer lsubr

    lsubr = RECL_MAX_BYTES
    m = choice(0, ni)
    b = m .gt. (lsubr / get_size_bytes(0_KI32))
    if (.not.b) then
       lsubr = lsubr - get_size_bytes(0_KI32, m)
       m = choice(0, nl)
       b = m .gt. (lsubr / get_size_bytes(0_KI64))
    endif
    if (.not.b) then
       lsubr = lsubr - get_size_bytes(0_KI64, m)
       m = choice(0, nf)
       b = m .gt. (lsubr / get_size_bytes(0.0_KFLT))
    endif
    if (.not.b) then
       lsubr = lsubr - get_size_bytes(0.0_KFLT, m)
       m = choice(0, nd)
       b = m .gt. (lsubr / get_size_bytes(0.0_KDBL))
    endif

  end function is_irec_overflow_mix
!!!_  & sus_size_irec - total record size in stream i/o unit
  integer(kind=KMEM) function sus_size_irec_li (mold, n) result(l)
    use TOUZA_Std_prc,only: KTGT=>KI32, KMEM=>KI64, KSEP=>KI32
    use TOUZA_Std_env,only: get_size_strm
    implicit none
    integer(kind=KTGT),intent(in) :: mold
    integer(kind=KMEM),intent(in) :: n
    l = get_size_strm(mold, n) + mstrm_sep(0_KSEP) * 2
  end function sus_size_irec_li
  integer(kind=KMEM) function sus_size_irec_ll (mold, n) result(l)
    use TOUZA_Std_prc,only: KTGT=>KI64, KMEM=>KI64, KSEP=>KI32
    use TOUZA_Std_env,only: get_size_strm
    implicit none
    integer(kind=KTGT),intent(in) :: mold
    integer(kind=KMEM),intent(in) :: n
    l = get_size_strm(mold, n) + mstrm_sep(0_KSEP) * 2
  end function sus_size_irec_ll
  integer(kind=KMEM) function sus_size_irec_lf (mold, n) result(l)
    use TOUZA_Std_prc,only: KTGT=>KFLT, KMEM=>KI64, KSEP=>KI32
    use TOUZA_Std_env,only: get_size_strm
    implicit none
    real(kind=KTGT),   intent(in) :: mold
    integer(kind=KMEM),intent(in) :: n
    l = get_size_strm(mold, n) + mstrm_sep(0_KSEP) * 2
  end function sus_size_irec_lf
  integer(kind=KMEM) function sus_size_irec_ld (mold, n) result(l)
    use TOUZA_Std_prc,only: KTGT=>KDBL, KMEM=>KI64, KSEP=>KI32
    use TOUZA_Std_env,only: get_size_strm
    implicit none
    real(kind=KTGT),   intent(in) :: mold
    integer(kind=KMEM),intent(in) :: n
    l = get_size_strm(mold, n) + mstrm_sep(0_KSEP) * 2
  end function sus_size_irec_ld
  integer(kind=KMEM) function sus_size_irec_la (mold, n) result(l)
    use TOUZA_Std_prc,only: KMEM=>KI64, KSEP=>KI32
    use TOUZA_Std_env,only: get_size_strm
    implicit none
    character(len=*),  intent(in) :: mold
    integer(kind=KMEM),intent(in) :: n
    l = get_size_strm(mold, n) + mstrm_sep(0_KSEP) * 2
  end function sus_size_irec_la

  integer(kind=KMEM) function sus_size_irec_i (mold, n) result(l)
    use TOUZA_Std_prc,only: KTGT=>KI32, KMEM=>KI32, KSEP=>KI32
    use TOUZA_Std_env,only: get_size_strm
    implicit none
    integer(kind=KTGT),intent(in) :: mold
    integer(kind=KMEM),intent(in),optional :: n
    l = get_size_strm(mold, n) + mstrm_sep(0_KSEP) * 2
  end function sus_size_irec_i
  integer(kind=KMEM) function sus_size_irec_l (mold, n) result(l)
    use TOUZA_Std_prc,only: KTGT=>KI64, KMEM=>KI32, KSEP=>KI32
    use TOUZA_Std_env,only: get_size_strm
    implicit none
    integer(kind=KTGT),intent(in) :: mold
    integer(kind=KMEM),intent(in),optional :: n
    l = get_size_strm(mold, n) + mstrm_sep(0_KSEP) * 2
  end function sus_size_irec_l
  integer(kind=KMEM) function sus_size_irec_f (mold, n) result(l)
    use TOUZA_Std_prc,only: KTGT=>KFLT, KMEM=>KI32, KSEP=>KI32
    use TOUZA_Std_env,only: get_size_strm
    implicit none
    real(kind=KTGT),   intent(in) :: mold
    integer(kind=KMEM),intent(in),optional :: n
    l = get_size_strm(mold, n) + mstrm_sep(0_KSEP) * 2
  end function sus_size_irec_f
  integer(kind=KMEM) function sus_size_irec_d (mold, n) result(l)
    use TOUZA_Std_prc,only: KTGT=>KDBL, KMEM=>KI32, KSEP=>KI32
    use TOUZA_Std_env,only: get_size_strm
    implicit none
    real(kind=KTGT),   intent(in) :: mold
    integer(kind=KMEM),intent(in),optional :: n
    l = get_size_strm(mold, n) + mstrm_sep(0_KSEP) * 2
  end function sus_size_irec_d
  integer(kind=KMEM) function sus_size_irec_a (mold, n) result(l)
    use TOUZA_Std_prc,only: KMEM=>KI32, KSEP=>KI32
    use TOUZA_Std_env,only: get_size_strm
    implicit none
    character(len=*),  intent(in) :: mold
    integer(kind=KMEM),intent(in),optional :: n
    l = get_size_strm(mold, n) + mstrm_sep(0_KSEP) * 2
  end function sus_size_irec_a
!!!_  & sus_is_unit_stream() - check whether stream access
  logical function sus_is_stream_unit(u) result(b)
    implicit none
    integer,intent(in) :: u
    character(len=16) :: ans
    integer jerr
    inquire(unit=u, STREAM=ans, IOSTAT=jerr)
    if (jerr.ne.0) ans = ' '
    select case (ans(1:1))
    case('Y', 'y')
       b = .TRUE.
    case default
       b = .FALSE.
    end select
  end function sus_is_stream_unit
!!!_  & choice_b - choice_a wrapper, use d if blank
  subroutine choice_b &
       & (v, d, a)
    use TOUZA_Std_utl,only: choice_a
    implicit none
    character(len=*),intent(out)         :: v
    character(len=*),intent(in)          :: d  ! default
    character(len=*),intent(in),optional :: a  ! argument
    if (present(a)) then
       v = a
       if (v.eq.' ') v = d
    else
       v = d
    endif
    return
  end subroutine choice_b
!!!_  & total_members()
  integer function total_members(bes, r, n) result(m)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(in)          :: bes(3, 0:*)
    integer,intent(in)          :: r
    integer,intent(in),optional :: n
    m = choice(-1, n)
    if (m.lt.0) m = product(bes(3, 0:r-1))
  end function total_members
!!!_  & mstrm_sep ()
  PURE &
  integer function mstrm_sep_i (mold) result(m)
    implicit none
    integer,parameter :: KISEP=KI32
    integer(kind=KISEP),intent(in) :: mold
    m = mstrm_isep + (0 * kind(mold))
  end function mstrm_sep_i
  PURE &
  integer function mstrm_sep_l (mold) result(m)
    implicit none
    integer,parameter :: KISEP=KI64
    integer(kind=KISEP),intent(in) :: mold
    m = mstrm_lsep + (0 * kind(mold))
  end function mstrm_sep_l
!!!_  & max_members ()
  PURE &
  integer function max_members_a (mold) result(m)
    implicit none
    character(len=*),intent(in) :: mold
    m = lsubr / max(1, len(mold))
  end function max_members_a

  PURE &
  integer function max_members_i (mold) result(m)
    implicit none
    integer,parameter :: KARG=KI32
    integer(kind=KARG),intent(in) :: mold
    m = maxmemi_i + (0 * kind(mold))
  end function max_members_i
  PURE &
  integer function max_members_l (mold) result(m)
    implicit none
    integer,parameter :: KARG=KI64
    integer(kind=KARG),intent(in) :: mold
    m = maxmemi_l + (0 * kind(mold))
  end function max_members_l
  PURE &
  integer function max_members_f (mold) result(m)
    implicit none
    integer,parameter :: KARG=KFLT
    real(kind=KARG),intent(in) :: mold
    m = maxmemi_f + (0 * kind(mold))
  end function max_members_f
  PURE &
  integer function max_members_d (mold) result(m)
    implicit none
    integer,parameter :: KARG=KDBL
    real(kind=KARG),intent(in) :: mold
    m = maxmemi_d + (0 * kind(mold))
  end function max_members_d
!!!_  & rest_members ()
  PURE &
  integer function rest_members_a (nbytes, mold) result(m)
    use TOUZA_Std_env,only: get_mems_bytes
    use TOUZA_Std_prc,only: KMEM=>KI32
    implicit none
    integer(kind=KMEM),intent(in) :: nbytes
    character(len=*),  intent(in) :: mold
    m = max_members(mold) - get_mems_bytes(nbytes, mold)
  end function rest_members_a
  PURE &
  integer function rest_members_i (nbytes, mold) result(m)
    use TOUZA_Std_env,only: get_mems_bytes
    use TOUZA_Std_prc,only: KTGT=>KI32,KMEM=>KI32
    implicit none
    integer(kind=KMEM),intent(in) :: nbytes
    integer(kind=KTGT),intent(in) :: mold
    m = max_members(mold) - get_mems_bytes(nbytes, mold)
  end function rest_members_i
  PURE &
  integer function rest_members_l (nbytes, mold) result(m)
    use TOUZA_Std_env,only: get_mems_bytes
    use TOUZA_Std_prc,only: KTGT=>KI64,KMEM=>KI32
    implicit none
    integer(kind=KMEM),intent(in) :: nbytes
    integer(kind=KTGT),intent(in) :: mold
    m = max_members(mold) - get_mems_bytes(nbytes, mold)
  end function rest_members_l
  integer function rest_members_f (nbytes, mold) result(m)
    use TOUZA_Std_env,only: get_mems_bytes
    use TOUZA_Std_prc,only: KTGT=>KFLT,KMEM=>KI32
    implicit none
    integer(kind=KMEM),intent(in) :: nbytes
    real(kind=KTGT),   intent(in) :: mold
    m = max_members(mold) - get_mems_bytes(nbytes, mold)
  end function rest_members_f
  integer function rest_members_d (nbytes, mold) result(m)
    use TOUZA_Std_env,only: get_mems_bytes
    use TOUZA_Std_prc,only: KTGT=>KDBL,KMEM=>KI32
    implicit none
    integer(kind=KMEM),intent(in) :: nbytes
    real(kind=KTGT),   intent(in) :: mold
    m = max_members(mold) - get_mems_bytes(nbytes, mold)
  end function rest_members_d
!!!_  & transf_iostat
  integer function transf_iostat &
       & (istat, ierr, line) &
       & result (n)
    implicit none
    integer,intent(in) :: istat
    integer,intent(in) :: ierr
    integer,intent(in) :: line
    last_iostat = istat
    last_line = line
    n = ierr
    return
  end function transf_iostat
!!!_  & check_dummy_irec
  subroutine check_dummy_irec_i &
       & (dummy, mem, lim, ubyte, isep, div)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    logical,            intent(out) :: dummy
    integer,            intent(in)  :: mem
    integer,            intent(in)  :: lim
    integer(kind=KISEP),intent(in)  :: isep
    integer,            intent(in)  :: ubyte
    integer,optional,   intent(in)  :: div
    integer d

    d = choice(def_block, div)
    select case(d)
    case(ignore_small)
       ! dummy = (abs(isep).lt.ubyte).or.(mem.gt.lim)
       dummy = (abs(isep).lt.ubyte)
    case(ignore_bigger)
       dummy = mem.gt.lim
    case(ignore_always)
       dummy = .TRUE.
    case default
       dummy = .FALSE.
    end select

  end subroutine check_dummy_irec_i

  subroutine check_dummy_irec_l &
       & (dummy, mem, lim, ubyte, isep, div)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    logical,            intent(out) :: dummy
    integer(kind=KIOFS),intent(in)  :: mem
    integer,            intent(in)  :: lim
    integer(kind=KISEP),intent(in)  :: isep
    integer,            intent(in)  :: ubyte
    integer,optional,   intent(in)  :: div
    integer d

    d = choice(def_block, div)
    select case(d)
    case(ignore_small)
       ! dummy = (abs(isep).lt.ubyte).or.(mem.gt.lim)
       dummy = (abs(isep).lt.ubyte)
    case(ignore_bigger)
       dummy = mem.gt.lim
    case(ignore_always)
       dummy = .TRUE.
    case default
       dummy = .FALSE.
    end select

  end subroutine check_dummy_irec_l
!!!_  & set_slice_loop
  subroutine set_slice_loop(rr, stp, itr, bes, r)
    implicit none
    integer,intent(out) :: rr
    integer,intent(out) :: stp(0:*), itr(0:*)
    integer,intent(in)  :: bes(3, 0:*)
    integer,intent(in)  :: r
    integer j, l, m, n, s
    rr = 0
    n = 1
    s = 0
    l = 0
    m = 1
    itr(rr) = 0
    stp(rr) = 0
    do j = 0, r - 1
       m = max(1, l) * (bes(2, j) - bes(1, j))
       l = max(1, l) * bes(3, j)
       if (m.eq.l) then
          continue
       else
          itr(rr) = m
          stp(rr) = s
          s = s + n * (l - m)
          n = n * l
          l = 0
          rr = rr + 1
       endif
    enddo
    if (l.gt.0.and.rr.lt.r) then
       itr(rr) = m
       stp(rr) = s
       rr = rr + 1
    endif
  end subroutine set_slice_loop
!!!_  & init_offset()
  PURE &
  integer function init_offset(bes, r) result(n)
    implicit none
    integer,intent(in) :: bes(3, 0:*)
    integer,intent(in) :: r
    integer j, m
    m = 1
    n = 0
    do j = 0, r - 1
       n = n + bes(1, j) * m
       m = m * bes(3, j)
    enddo
  end function init_offset
!!!_  & next_offset()
  subroutine next_offset(jsrc, idx, stp, itr, rr, m)
    implicit none
    integer,intent(inout) :: jsrc
    integer,intent(inout) :: idx(0:*)
    integer,intent(in)    :: stp(0:*), itr(0:*)
    integer,intent(in)    :: rr
    integer,intent(in)    :: m
    integer rj
    idx(0) = idx(0) + m
    jsrc = jsrc + m
    if (idx(0).lt.itr(0)) return

    rj = 1
    idx(0) = 0
    do
       if (rj.ge.rr) then
          jsrc = -1
          return
       endif
       idx(rj) = idx(rj) + 1
       if (idx(rj).lt.itr(rj)) then
          jsrc = jsrc + stp(rj)
          return
       endif
       idx(rj) = 0
       rj = rj + 1
    enddo
  end subroutine next_offset
!!!_ + end module TOUZA_Std_sus
end module TOUZA_Std_sus

!!!_@ test_std_sus - test program
#if   TEST_STD_SUS == 0
#elif TEST_STD_SUS < 3
program test_std_sus
  use TOUZA_Std_sus
  use TOUZA_Std_arg,only: cmdline_arg_wrap, cmdline_count_wrap
  integer ierr
  logical swap, lsep
  integer,parameter :: lpath = 1024
  character(len=lpath) :: file
  integer narg, jarg
  integer,parameter :: larg = 128
  character(len=larg) :: arg
  integer mem

101 format(A, ' = ', I0)
  ierr = 0
  if (ierr.eq.0) call init(ierr)
  if (ierr.eq.0) call diag(ierr, u=-1, levv=+1)

  if (ierr.eq.0) then
     jarg = 0
     narg = cmdline_count_wrap()

     mem = 64
     if (jarg.lt.narg) then
        jarg = jarg + 1
        call cmdline_arg_wrap(jarg, arg)
     else
        arg = ' '
     endif
     swap = index(arg, 's').gt.0
     lsep = index(arg, 'L').gt.0
111  format('outsus-', I0, L1, L1, '.dat')
     write(file, 111) TEST_STD_SUS, swap, lsep
     call batch_test_i(ierr, file, arg, swap, lsep, mem)
     if (.not.lsep) then
        call batch_test_suspend_i(ierr, file, arg, swap, mem)
     endif
  endif
  if (ierr.eq.0) call batch_overflow_mix(ierr)

  if (ierr.eq.0) call finalize(ierr)
  write(*, 101) 'FINAL', ierr
  stop
contains
  subroutine batch_test_i(ierr, file, arg, swap, lsep, mem)
    use TOUZA_Std_prc,only: KSRC=>KI32
    use TOUZA_Std_env,only: KIOFS
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: file
    character(len=*),intent(in)  :: arg
    logical,         intent(in)  :: swap, lsep
    integer,         intent(in)  :: mem
    integer(kind=KSRC) :: v(0:mem-1), ref(0:mem-1), bref(0:mem-1)
    integer(kind=KSRC) :: c(0:mem-1)
    integer(kind=KSRC),parameter :: pad = -9
    integer(kind=KSRC) :: mold
    integer j
    integer u
    integer d
    integer jrec
    logical bchk
    integer,parameter :: lr = 3
    integer jr, mr
    integer w,  mdiv, m
    integer mpar, msub
    integer bes(3, 0:lr-1), idx(0:lr-1)
    integer jj
    integer(kind=KIOFS) :: jpos
    logical sub

    ierr = 0
    u = 10
    if (ierr.eq.0) call sus_open(ierr, u, file, ACTION='RW', STATUS='R')
    if (ierr.eq.0) then
       do j = 0, mem - 1
          v(j) = j * 65536 + j
       enddo
       m = 1
       w = 3
       mr = lr
       mdiv = 1
       do j = 0, lr - 1
          bes(1:3, j) = (/1, w-1, w/)
          m = m * w
          if (m.gt.mem) then
             mr = j
             exit
          endif
          mdiv = mdiv * (w - 2)
          w = w + 1
       enddo
       idx(0:mr-1) = 0
       jj = 0
       bref(0:mem-1) = pad
       do j = 0, mem - 1
          ! write(*, *) jp, ixd(0:nr-1), jl
          if (ALL(idx(0:mr-1).ge.bes(1,0:mr-1)) &
               & .and. ALL(idx(0:mr-1).lt.bes(2,0:mr-1))) then
             ref(jj) = v(j)
             bref(j) = v(j)
             jj = jj + 1
             if (jj.eq.mdiv) exit
          endif
          do jr = 0, mr - 1
             idx(jr) = idx(jr) + 1
             if (idx(jr).lt.bes(3,jr)) exit
             idx(jr) = 0
          enddo
       enddo
       ! write(*, *) mr, bes(1:3, 0:mr-1)
       ! write(*, *) mdiv, ref(0:mdiv-1)
    endif

    if (ierr.eq.0) then
       if (lsep) then
          if (ierr.eq.0) call sus_write_lrec(ierr, u, v(0:mem-1), mem, swap=swap)
       else
          if (ierr.eq.0) call sus_write_irec(ierr, u, v(0:mem-1), mem, swap=swap, dummy=-1)

          if (ierr.eq.0) call sus_getpos(ierr, jpos, u)
          if (ierr.eq.0) call sus_pad_irec(ierr, u, pad, mem, swap=swap, dummy=-1)
          if (ierr.eq.0) read(u, POS=jpos, IOSTAT=ierr)
          if (ierr.eq.0) call sus_edit_slice_irec(ierr, u, ref(0:mem-1), bes, mr, swap=swap, div=-1, lmem=mem)

          if (ierr.eq.0) call sus_write_irec(ierr, u, v(0:mem-1), mem, swap=swap)

          if (ierr.eq.0) call sus_getpos(ierr, jpos, u)
          if (ierr.eq.0) call sus_pad_irec(ierr, u, pad, mem, swap=swap)
          if (ierr.eq.0) read(u, POS=jpos, IOSTAT=ierr)
          if (ierr.eq.0) call sus_edit_slice_irec(ierr, u, ref(0:mem-1), bes, mr, swap=swap, lmem=mem)
       endif
    endif
    if (ierr.eq.0) call sus_close(ierr, u, file)
    if (ierr.eq.0) call sus_open(ierr, u, file, ACTION='R', STATUS='O')
    if (ierr.eq.0) then
       c(0:mem-1) = -1
       if (lsep) then
          if (ierr.eq.0) call sus_read_lrec(ierr, u, c(0:mem-1), mem, swap=swap)
          if (ierr.eq.0) then
             if (ANY(c(0:mem-1).ne.v(0:mem-1))) then
201             format('invalid at ', I0, 1x, I0, 1x, I0)
                do j = 0, mem - 1
                   if (c(j).ne.v(j)) write(*, 201) j, c(j), v(j)
                enddo
             endif
          endif
       else
202       format('whole inconsistent i-record ', I0, 1x, I0)
203       format('whole error i-record ', I0, 1x, I0, ' = ', I0)
204       format('whole success i-record ', I0, 1x, I0)
          do d = 0, -3, -1
             if (ierr.eq.0) rewind(u, IOSTAT=ierr)
             do jrec = 0, 3
                c(0:mem-1) = max(-1, pad+1)
                if (ierr.eq.0) call sus_read_irec(ierr, u, c(0:mem-1), mem, swap=swap, div=d)
                if (ierr.eq.0) then
                   if (mod(jrec,2).eq.0) then
                      bchk = ANY(c(0:mem-1).ne.v(0:mem-1))
                   else
                      bchk = ANY(c(0:mem-1).ne.bref(0:mem-1))
                   endif
                   if (bchk) then
                      write(*, 202) d, jrec
                      ! write(*, *) c(0:mem-1)
                   else
                      write(*, 204) d, jrec
                      ! write(*, *) c(0:mem-1)
                   endif
                else
                   write(*, 203) d, jrec, ierr
                   ierr = 0
                endif
             enddo
          enddo
302       format('slice inconsistent i-record ', I0, 1x, I0)
303       format('slice error i-record ', I0, 1x, I0, ' = ', I0)
304       format('slice success i-record ', I0, 1x, I0)
          do d = 0, -3, -1
             if (ierr.eq.0) rewind(u, IOSTAT=ierr)
             do jrec = 0, 3
                c(0:mem-1) = max(-1, pad+1)
                if (ierr.eq.0) call sus_slice_read_irec(ierr, u, c(0:mem-1), bes, mr, swap=swap, div=d, lmem=mem)
                if (ierr.eq.0) then
                   if (mod(jrec,2).eq.0) then
                      bchk = ANY(c(0:mdiv-1).ne.ref(0:mdiv-1))
                   else
                      bchk = ANY(c(0:mdiv-1).ne.ref(0:mdiv-1))
                   endif
                   ! do j = 0, mdiv - 1
                   !    write(*, *) j, c(j), ref(j)
                   ! enddo
                   if (bchk) then
                      write(*, 302) d, jrec
                   else
                      write(*, 304) d, jrec
                   endif
                else
                   write(*, 303) d, jrec, ierr
                   ierr = 0
                endif
             enddo
          enddo
402       format('parital inconsistent i-record ', I0, 1x, I0)
403       format('partial error i-record ', I0, 1x, I0, ' = ', I0)
404       format('partial success i-record ', I0, 1x, I0)
405       format('   partial pos ', L1, 1x, Z4.4)
406       format('   partial pos fwd ', Z4.4)
          mpar = min(1 + max_members(mold), mem)
          do d = 0, -3, -1
             if (ierr.eq.0) rewind(u, IOSTAT=ierr)
             do jrec = 0, 3
                c(0:mem-1) = max(-1, pad+1)
                sub = .TRUE.
                msub = min(mpar * 2, mem)
                if (jrec.lt.2) msub = mem
                if (ierr.eq.0) call sus_read_irec(ierr, u, c(0:mpar-1), mpar, sub=sub, swap=swap, div=d, lmem=msub)
                if (ierr.eq.0) call sus_getpos(ierr, jpos, u)
                if (ierr.eq.0) then
                   if (mod(jrec,2).eq.0) then
                      bchk = ANY(c(0:mpar-1).ne.v(0:mpar-1))
                   else
                      bchk = ANY(c(0:mpar-1).ne.bref(0:mpar-1))
                   endif
                   ! do j = 0, mdiv - 1
                   !    write(*, *) j, c(j), ref(j)
                   ! enddo
                   if (bchk) then
                      write(*, 402) d, jrec
                   else
                      write(*, 404) d, jrec
                   endif
                else
                   write(*, 403) d, jrec, ierr
                   ierr = 0
                   sub = .FALSE.
                endif
                write (*, 405) sub, jpos - 1
                if (ierr.eq.0) then
                   if (sub) then
                      call sus_skip_irec(ierr, u, 1, swap=swap)
                      if (ierr.eq.0) call sus_getpos(ierr, jpos, u)
                      write(*, 406) jpos - 1
                   endif
                endif
             enddo
          enddo
       endif
    endif
    if (ierr.eq.0) call sus_close(ierr, u, file)
  end subroutine batch_test_i

  subroutine batch_test_suspend_i(ierr, file, arg, swap, mem)
    use TOUZA_Std_prc,only: KSRC=>KI32
    use TOUZA_Std_env,only: KIOFS
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: file
    character(len=*),intent(in)  :: arg
    logical,         intent(in)  :: swap
    integer,         intent(in)  :: mem
    integer(kind=KSRC) :: v(0:mem-1)
    integer(kind=KSRC) :: c(0:mem-1)
    integer(kind=KIOFS) :: jposh
    integer u
    integer j, stp, n

    u = 10
    ierr = 0
    if (ierr.eq.0) call sus_open(ierr, u, file, ACTION='RW', STATUS='O', POSITION='AP')
    if (ierr.eq.0) call sus_getpos(ierr, jposh, u)
    if (ierr.eq.0) then
111    format('suspend: head = ', Z8.8)
       write(*, 111) jposh - 1

       do j = 0, mem - 1
          v(j) = j * 65536 + j
       enddo
       call sus_suspend_write_irec(ierr, u, v, 0, sw=+1, swap=swap)
       stp = max(1, mem / 4) + 2
       do j = 0, mem - 1, stp
          n = min(mem, j + stp) - j
          call sus_suspend_write_irec(ierr, u, v(j:j+n-1), n, sw=0, swap=swap)
       enddo
       call sus_suspend_write_irec(ierr, u, v, 0, sw=-1, swap=swap)
    endif
    if (ierr.eq.0) call sus_rseek(ierr, u, jposh, whence=WHENCE_ABS)
    if (ierr.eq.0) call sus_read_irec(ierr, u, c, mem, swap)
    if (ierr.eq.0) then
101    format('suspend:success')
102    format('suspend:fail ', I0, 1x, I0, 1x, I0)
       if (ALL(v(0:mem-1).eq.c(0:mem-1))) then
          write(*, 101)
       else
          do j = 0, mem - 1
             if (v(j).ne.c(j)) write(*, 102) j, v(j), c(j)
          enddo
       endif
    endif

    if (ierr.eq.0) call sus_rseek(ierr, u, jposh, whence=WHENCE_ABS)
    if (ierr.eq.0) then
       call sus_suspend_read_irec(ierr, u, v, 0, sw=+1, swap=swap)
       stp = max(1, mem / 5) + 3
       do j = 0, mem - 1, stp
          n = min(mem, j + stp) - j
          if (ierr.eq.0) call sus_suspend_read_irec(ierr, u, c(j:j+n-1), n, sw=0, swap=swap)
       enddo
       if (ierr.eq.0) call sus_suspend_read_irec(ierr, u, v, 0, sw=-1, swap=swap)
    endif
    if (ierr.eq.0) then
121    format('suspend/read:success')
122    format('suspend/read:fail ', I0, 1x, I0, 1x, I0)
       if (ALL(v(0:mem-1).eq.c(0:mem-1))) then
          write(*, 121)
       else
          do j = 0, mem - 1
             if (v(j).ne.c(j)) write(*, 122) j, v(j), c(j)
          enddo
       endif
    endif

  end subroutine batch_test_suspend_i

  subroutine batch_overflow_mix &
       & (ierr)
    implicit none
    integer,intent(out) :: ierr
    integer ni, nl, nf, nd

    ierr = 0
    do ni = 0, 8
       do nl = 0, 4
          do nf = 0, 8
             do nd = 0, 4
                call test_overflow_mix(ierr, ni, nl, nf, nd)
             enddo
          enddo
       enddo
    enddo
  end subroutine batch_overflow_mix
  subroutine test_overflow_mix &
       & (ierr, ni, nl, nf, nd)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: ni, nl, nf, nd
    logical b
    ierr = 0
    b = is_irec_overflow_mix(ni, nl, nf, nd)
101 format('mix/overflow:', L1, 1x, I0, 1x, 4(1x, I0))
    write(*, 101) b, ((ni + nf) * 4 + (nl + nd) * 8), ni, nl, nf, nd
  end subroutine test_overflow_mix

end program test_std_sus
#elif TEST_STD_SUS
program test_std_sus
  use TOUZA_Std_prc,only: KI32,KI64,KFLT,KDBL
  use TOUZA_Std_sus
  implicit none
  integer ierr

  integer u
  integer,parameter :: lv = 1024
  integer,parameter :: la = 16
  integer(kind=KI32) :: vis(lv), vid(lv)
  integer(kind=KI64) :: vls(lv), vld(lv)
  real(kind=KFLT)    :: vfs(lv), vfd(lv)
  real(kind=KDBL)    :: vds(lv), vdd(lv)
  character(len=la)  :: vas(lv), vad(lv)

  character(len=512) :: file = 'out.sus'
  character(len=512) :: file2 = 'out.sus2'
  character(len=512) :: file3 = 'out.sus3'
  character(len=512) :: file4 = 'out.sus4'

  integer dims(4)

  integer kendi
  integer j
  integer mi, ml, mf, md, ma
  logical swap

  integer nm, jb, je

101 format(A, ' = ', I0)

  call init(ierr)
  if (ierr.eq.0) call diag(ierr, u=-1, levv=+1)

  mi = min(lv, 8)
  ml = mi / 2
  mf = mi
  md = mf / 2
  ma = mi * 4 / la

  do j = 1, mi
     vis(j) = j * j
  enddo
  do j = 1, ml
     vls(j) = j * j
  enddo
  do j = 1, mf
     vfs(j) = real(j, KIND=KFLT) ** 2.0_KFLT
  enddo
  do j = 1, md
     vds(j) = real(j, KIND=KDBL) ** 2.0_KDBL
  enddo
  do j = 1, ma
     write(vas(j), '(I16.16)') j * j
  enddo

  write(*, *) 'members = ', mi, ml, mf, md, ma, la

  u = 10
  if (ierr.eq.0) call sus_open(ierr, u, file, ACTION='W', STATUS='R')

  if (ierr.eq.0) then
     call batch_write(ierr, u, vis, mi, vls, ml, vfs, mf, vds, md, vas, ma, 'IN')
  endif
  if (ierr.eq.0) then
     call batch_write(ierr, u, vis, mi, vls, ml, vfs, mf, vds, md, vas, ma, 'LN')
  endif
  if (ierr.eq.0) then
     call batch_write(ierr, u, vis, mi, vls, ml, vfs, mf, vds, md, vas, ma, 'IS')
  endif
  if (ierr.eq.0) then
     call batch_write(ierr, u, vis, mi, vls, ml, vfs, mf, vds, md, vas, ma, 'LS')
  endif

  if (ierr.eq.0) call sus_close(ierr, u, file)

  if (ierr.eq.0) call sus_open(ierr, u, file, ACTION='R')

  if (ierr.eq.0) then
     call batch_read_check(ierr, u, vis, mi, vls, ml, vfs, mf, vds, md, vas, ma, 'IN')
  endif
  if (ierr.eq.0) then
     call batch_read_check(ierr, u, vis, mi, vls, ml, vfs, mf, vds, md, vas, ma, 'LN')
  endif
  if (ierr.eq.0) then
     call batch_read_check(ierr, u, vis, mi, vls, ml, vfs, mf, vds, md, vas, ma, 'IS')
  endif
  if (ierr.eq.0) then
     call batch_read_check(ierr, u, vis, mi, vls, ml, vfs, mf, vds, md, vas, ma, 'LS')
  endif

  if (ierr.eq.0) call sus_close(ierr, u, file)
  if (ierr.eq.0) open(UNIT=u, FILE=file, IOSTAT=ierr, &
       &              ACCESS='SEQUENTIAL', FORM='UNFORMATTED', ACTION='READ')
  if (ierr.eq.0) then
     call batch_seqread_check(ierr, u, vis, mi, vls, ml, vfs, mf, vds, md, vas, ma, 'IN')
  endif
  if (ierr.eq.0) then
     call batch_seqread_check(ierr, u, vis, mi, vls, ml, vfs, mf, vds, md, vas, ma, 'LN')
  endif
  if (ierr.eq.0) then
     call batch_seqread_check(ierr, u, vis, mi, vls, ml, vfs, mf, vds, md, vas, ma, 'IS')
  endif
  if (ierr.eq.0) then
     call batch_seqread_check(ierr, u, vis, mi, vls, ml, vfs, mf, vds, md, vas, ma, 'LS')
  endif

  if (ierr.eq.0) then
     call test_slice_loop(ierr)
  endif
  if (ierr.eq.0) close(UNIT=u, IOSTAT=ierr)
  if (ierr.eq.0) call sus_open(ierr, u, file2, ACTION='W', STATUS='R')
  if (ierr.eq.0) then
     dims(:) = (/4, 5, 7, 8/)   ! avoid 3 multiples
     call test_slice_create(ierr, u, dims)
     if (ierr.eq.0) call sus_close(ierr, u, file2)
     if (ierr.eq.0) call sus_open(ierr, u, file2, ACTION='R')
     if (ierr.eq.0) call batch_slice_read(ierr, u, dims)
  endif

  if (ierr.eq.0) call sus_open(ierr, u, file3, ACTION='W', STATUS='R')
  do j = 0, 1
     swap = j.ne.0
     if (ierr.eq.0) call sus_pad_irec(ierr, u, 123, 6, swap)
  enddo
  if (ierr.eq.0) call sus_close(ierr, u, file3)

  nm = 16
  jb = 2
  je = 14
  if (ierr.eq.0) call sus_open(ierr, u, file4, ACTION='RW', STATUS='R')
  if (ierr.eq.0) call test_blank_create(ierr, u, nm)
  if (ierr.eq.0) rewind(u, IOSTAT=ierr)
  if (ierr.eq.0) call test_slice_write(ierr, u, jb, je, nm)
  if (ierr.eq.0) rewind(u, IOSTAT=ierr)
  if (ierr.eq.0) call test_slice_read(ierr, u, jb, je, nm)
  if (ierr.eq.0) rewind(u, IOSTAT=ierr)
  if (ierr.eq.0) call test_slice_write(ierr, u, 0, nm, nm)
  if (ierr.eq.0) rewind(u, IOSTAT=ierr)
  if (ierr.eq.0) call test_slice_read(ierr, u, jb, je, nm)

  if (ierr.eq.0) rewind(u, IOSTAT=ierr)
  if (ierr.eq.0) call test_runl_read(ierr, u, jb, je, nm)
  if (ierr.eq.0) rewind(u, IOSTAT=ierr)
  if (ierr.eq.0) call test_list_read(ierr, u, jb, je, nm)

  if (ierr.eq.0) call sus_close(ierr, u, file4)

  if (ierr.eq.0) call finalize(ierr)
  write(*, 101) 'FINAL', ierr
  stop
contains
  subroutine batch_read_check &
       & (ierr, u, vi, ni, vl, nl, vf, nf, vd, nd, va, na, flag)
    implicit none
    integer,           intent(out) :: ierr
    integer,           intent(in)  :: u
    integer,           intent(in)  :: ni, nl, nf, nd, na
    integer(kind=KI32),intent(in)  :: vi(*)
    integer(kind=KI64),intent(in)  :: vl(*)
    real(kind=KFLT),   intent(in)  :: vf(*)
    real(kind=KDBL),   intent(in)  :: vd(*)
    character(len=*),  intent(in)  :: va(*)
    character(len=*),  intent(in)  :: flag

    integer(kind=KI32) :: xi(lv)
    integer(kind=KI64) :: xl(lv)
    real(kind=KFLT)    :: xf(lv)
    real(kind=KDBL)    :: xd(lv)
    character(len=la)  :: xa(lv)

    logical swap

    ierr = 0

    swap = index(flag, 'S') .gt. 0

    if (index(flag, 'I').gt.0) then
       if (ierr.eq.0) call sus_read_irec(ierr, u, xi(1:ni), ni, swap=swap)
       if (ierr.eq.0) call sus_read_irec(ierr, u, xl(1:nl), nl, swap=swap)
       if (ierr.eq.0) call sus_read_irec(ierr, u, xf(1:nf), nf, swap=swap)
       if (ierr.eq.0) call sus_read_irec(ierr, u, xd(1:nd), nd, swap=swap)
       if (ierr.eq.0) call sus_read_irec(ierr, u, xa(1:na), na, swap=swap)
    else
       if (ierr.eq.0) call sus_read_lrec(ierr, u, xi(1:ni), ni, swap=swap)
       if (ierr.eq.0) call sus_read_lrec(ierr, u, xl(1:nl), nl, swap=swap)
       if (ierr.eq.0) call sus_read_lrec(ierr, u, xf(1:nf), nf, swap=swap)
       if (ierr.eq.0) call sus_read_lrec(ierr, u, xd(1:nd), nd, swap=swap)
       if (ierr.eq.0) call sus_read_lrec(ierr, u, xa(1:na), na, swap=swap)
    endif

101 format('CHECK/', A,': ', I0)
    if (ierr.eq.0) write(*, 101) 'I', COUNT(vi(1:ni).ne.xi(1:ni))
    if (ierr.eq.0) write(*, 101) 'L', COUNT(vl(1:nl).ne.xl(1:nl))
    if (ierr.eq.0) write(*, 101) 'F', COUNT(vf(1:nf).ne.xf(1:nf))
    if (ierr.eq.0) write(*, 101) 'D', COUNT(vd(1:nd).ne.xd(1:nd))
    if (ierr.eq.0) write(*, 101) 'A', COUNT(va(1:na).ne.xa(1:na))

    return
  end subroutine batch_read_check

  subroutine batch_write &
       & (ierr, u, vi, ni, vl, nl, vf, nf, vd, nd, va, na, flag)
    implicit none
    integer,           intent(out) :: ierr
    integer,           intent(in)  :: u
    integer,           intent(in)  :: ni, nl, nf, nd, na
    integer(kind=KI32),intent(in)  :: vi(*)
    integer(kind=KI64),intent(in)  :: vl(*)
    real(kind=KFLT),   intent(in)  :: vf(*)
    real(kind=KDBL),   intent(in)  :: vd(*)
    character(len=*),  intent(in)  :: va(*)
    character(len=*),  intent(in)  :: flag

    logical swap

    ierr = 0

    swap = index(flag, 'S') .gt. 0

    if (index(flag, 'I').gt.0) then
       if (ierr.eq.0) call sus_write_irec(ierr, u, vi(1:ni), ni, swap=swap)
       if (ierr.eq.0) call sus_write_irec(ierr, u, vl(1:nl), nl, swap=swap)
       if (ierr.eq.0) call sus_write_irec(ierr, u, vf(1:nf), nf, swap=swap)
       if (ierr.eq.0) call sus_write_irec(ierr, u, vd(1:nd), nd, swap=swap)
       if (ierr.eq.0) call sus_write_irec(ierr, u, va(1:na), na, swap=swap)
    else
       if (ierr.eq.0) call sus_write_lrec(ierr, u, vi(1:ni), ni, swap=swap)
       if (ierr.eq.0) call sus_write_lrec(ierr, u, vl(1:nl), nl, swap=swap)
       if (ierr.eq.0) call sus_write_lrec(ierr, u, vf(1:nf), nf, swap=swap)
       if (ierr.eq.0) call sus_write_lrec(ierr, u, vd(1:nd), nd, swap=swap)
       if (ierr.eq.0) call sus_write_lrec(ierr, u, va(1:na), na, swap=swap)
    endif

    return
  end subroutine batch_write

  subroutine batch_seqread_check &
       & (ierr, u, vi, ni, vl, nl, vf, nf, vd, nd, va, na, flag)
    implicit none
    integer,           intent(out) :: ierr
    integer,           intent(in)  :: u
    integer,           intent(in)  :: ni, nl, nf, nd, na
    integer(kind=KI32),intent(in)  :: vi(*)
    integer(kind=KI64),intent(in)  :: vl(*)
    real(kind=KFLT),   intent(in)  :: vf(*)
    real(kind=KDBL),   intent(in)  :: vd(*)
    character(len=*),  intent(in)  :: va(*)
    character(len=*),  intent(in)  :: flag

    integer(kind=KI32) :: xi(lv)
    integer(kind=KI64) :: xl(lv)
    real(kind=KFLT)    :: xf(lv)
    real(kind=KDBL)    :: xd(lv)
    character(len=la)  :: xa(lv)

    logical swap

    ierr = 0

    if (index(flag, 'I').eq.0) return
    if (index(flag, 'S').gt.0) return

    if (ierr.eq.0) read(UNIT=U, IOSTAT=ierr) xi(1:ni)
    if (ierr.eq.0) read(UNIT=U, IOSTAT=ierr) xl(1:nl)
    if (ierr.eq.0) read(UNIT=U, IOSTAT=ierr) xf(1:nf)
    if (ierr.eq.0) read(UNIT=U, IOSTAT=ierr) xd(1:nd)
    if (ierr.eq.0) read(UNIT=U, IOSTAT=ierr) xa(1:na)

101 format('CHECK/SEQ/', A,': ', I0)
    if (ierr.eq.0) write(*, 101) 'I', COUNT(vi(1:ni).ne.xi(1:ni))
    if (ierr.eq.0) write(*, 101) 'L', COUNT(vl(1:nl).ne.xl(1:nl))
    if (ierr.eq.0) write(*, 101) 'F', COUNT(vf(1:nf).ne.xf(1:nf))
    if (ierr.eq.0) write(*, 101) 'D', COUNT(vd(1:nd).ne.xd(1:nd))
    if (ierr.eq.0) write(*, 101) 'A', COUNT(va(1:na).ne.xa(1:na))

    return
  end subroutine batch_seqread_check

  subroutine test_slice_loop(ierr)
    implicit none
    integer,intent(out) :: ierr
    ierr = 0

    if (ierr.eq.0) call test_slice_loop_sub(ierr, 1, (/0,8,8/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 1, (/1,5,8/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 1, (/0,1,1/))

    if (ierr.eq.0) call test_slice_loop_sub(ierr, 2, (/0,8,8,  0,6,6/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 2, (/1,5,8,  0,6,6/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 2, (/0,1,1,  0,6,6/))

    if (ierr.eq.0) call test_slice_loop_sub(ierr, 2, (/0,8,8,  2,4,6/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 2, (/1,5,8,  2,4,6/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 2, (/0,1,1,  2,4,6/))

    if (ierr.eq.0) call test_slice_loop_sub(ierr, 2, (/0,8,8,  0,1,1/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 2, (/1,5,8,  0,1,1/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 2, (/0,1,1,  0,1,1/))

    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/0,8,8,  0,6,6,  0,4,4/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/1,5,8,  0,6,6,  0,4,4/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/0,1,1,  0,6,6,  0,4,4/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/0,8,8,  2,4,6,  0,4,4/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/1,5,8,  2,4,6,  0,4,4/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/0,1,1,  2,4,6,  0,4,4/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/0,8,8,  0,1,1,  0,4,4/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/1,5,8,  0,1,1,  0,4,4/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/0,1,1,  0,1,1,  0,4,4/))

    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/0,8,8,  0,6,6,  1,3,4/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/1,5,8,  0,6,6,  1,3,4/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/0,1,1,  0,6,6,  1,3,4/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/0,8,8,  2,4,6,  1,3,4/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/1,5,8,  2,4,6,  1,3,4/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/0,1,1,  2,4,6,  1,3,4/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/0,8,8,  0,1,1,  1,3,4/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/1,5,8,  0,1,1,  1,3,4/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/0,1,1,  0,1,1,  1,3,4/))

    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/0,8,8,  0,6,6,  0,1,1/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/1,5,8,  0,6,6,  0,1,1/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/0,1,1,  0,6,6,  0,1,1/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/0,8,8,  2,4,6,  0,1,1/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/1,5,8,  2,4,6,  0,1,1/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/0,1,1,  2,4,6,  0,1,1/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/0,8,8,  0,1,1,  0,1,1/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/1,5,8,  0,1,1,  0,1,1/))
    if (ierr.eq.0) call test_slice_loop_sub(ierr, 3, (/0,1,1,  0,1,1,  0,1,1/))

  end subroutine test_slice_loop

  subroutine test_slice_loop_sub(ierr, r, bes)
    use TOUZA_Std_utl,only: join_list
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: r
    integer,intent(in)  :: bes(3, 0:*)

    integer jt
    integer j
    integer rr
    integer stp(0:r-1), itr(0:r-1)
    integer lidx(0:r-1), pidx(0:r-1)
    character(len=32)  :: str(0:r-1)
    character(len=256) :: txti, txto
    integer jerr

    integer jsrc
    integer m, mm, k

    call set_slice_loop(rr, stp, itr, bes, r)
111 format('slice: ', A, ' > ', A)

101 format(I0, ':', I0, '/', I0)
102 format(I0, '+', I0)
    do j = 0, r - 1
       write(str(j), 101) bes(1:3, j)
    enddo
    call join_list(jerr, txti, str(0:r-1))
    do j = 0, rr - 1
       write(str(j), 102) itr(j), stp(j)
    enddo
    call join_list(jerr, txto, str(0:rr-1))

    write(*, 111) trim(txti), trim(txto)

    do jt = 0, 1
       if (jt.eq.0) then
          m = itr(0)
       else if (itr(0).eq.1) then
          cycle
       else
          m = max(1, itr(0) / 2)
       endif
       jsrc = init_offset(bes, r)
       lidx(0:rr-1) = 0
       do
          if (jsrc.lt.0) exit
          k = jsrc
          do j = 0, r - 1
             pidx(j) = mod(k, bes(3, j))
             k = k / bes(3, j)
          enddo
          call join_list(jerr, txti, lidx(0:rr-1))
          call join_list(jerr, txto, pidx(0:r-1))
121       format('slice/seq+', I0, ': ', I0, ' / ', A, ' > ', A)
          write(*, 121) m, jsrc, trim(txti), trim(txto)
          mm = min(m, itr(0) - lidx(0))
          call next_offset(jsrc, lidx, stp, itr, rr, mm)
       enddo
    enddo

  end subroutine test_slice_loop_sub

  subroutine test_blank_create  &
       & (ierr, u, nm)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: u
    integer,intent(in)  :: nm

    integer(kind=KI32),parameter :: hmi = 0
    integer(kind=KI64),parameter :: hml = 0
    real(kind=KFLT),   parameter :: hmf = 0
    real(kind=KDBL),   parameter :: hmd = 0
    character(len=16), parameter :: hma = ' '
    integer jt
    logical swap
    logical pre, post
    integer nd, md, jb, je

    ierr = 0
    do jt = 0, 1
       swap = (jt.ne.0)
       if (ierr.eq.0) call sus_blank_irec(ierr, u, hmi, nm, swap)
       if (ierr.eq.0) call sus_blank_irec(ierr, u, hml, nm, swap)
       if (ierr.eq.0) call sus_blank_irec(ierr, u, hmf, nm, swap)
       if (ierr.eq.0) call sus_blank_irec(ierr, u, hmd, nm, swap)
       if (ierr.eq.0) call sus_blank_irec(ierr, u, hma, nm, swap)
    enddo

    nd = nm / 3 + 1
    do jt = 0, 1
       pre = .FALSE.
       post = .TRUE.
       swap = (jt.ne.0)
       do jb = 0, nm - 1, nd
          md = min(nd, nm - jb)
          je = jb + md
          post = je.lt.nm
          if (ierr.eq.0) call sus_blank_irec(ierr, u, hmi, md, swap, pre, post)
          pre = .TRUE.
       enddo
       pre = .FALSE.
       post = .TRUE.
       do jb = 0, nm - 1, nd
          md = min(nd, nm - jb)
          je = jb + md
          post = je.lt.nm
          if (ierr.eq.0) call sus_blank_irec(ierr, u, hml, md, swap, pre, post)
          pre = .TRUE.
       enddo
       pre = .FALSE.
       post = .TRUE.
       do jb = 0, nm - 1, nd
          md = min(nd, nm - jb)
          je = jb + md
          post = je.lt.nm
          if (ierr.eq.0) call sus_blank_irec(ierr, u, hmf, md, swap, pre, post)
          pre = .TRUE.
       enddo
       pre = .FALSE.
       post = .TRUE.
       do jb = 0, nm - 1, nd
          md = min(nd, nm - jb)
          je = jb + md
          post = je.lt.nm
          if (ierr.eq.0) call sus_blank_irec(ierr, u, hmd, md, swap, pre, post)
          pre = .TRUE.
       enddo
       pre = .FALSE.
       post = .TRUE.
       do jb = 0, nm - 1, nd
          md = min(nd, nm - jb)
          je = jb + md
          post = je.lt.nm
          if (ierr.eq.0) call sus_blank_irec(ierr, u, hma, md, swap, pre, post)
          pre = .TRUE.
       enddo
    enddo

  end subroutine test_blank_create

  subroutine test_slice_write  &
       & (ierr, u, jb, je, nm)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: u
    integer,intent(in)  :: jb, je, nm

    integer(kind=KI32) :: vi(0:nm-1)
    integer(kind=KI64) :: vl(0:nm-1)
    real(kind=KFLT)    :: vf(0:nm-1)
    real(kind=KDBL)    :: vd(0:nm-1)
    character(len=16)  :: va(0:nm-1)
    integer j
    integer jt
    logical swap
    integer,parameter :: r = 1
    integer bes(3, r)

    ierr = 0

    do j = 0, nm - 1
       vi(j) = j
       vl(j) = j
       vf(j) = j
       vd(j) = j
       write(va(j), '(I8.8)') j
    enddo

    bes(:, r) = (/jb, je, nm/)
    do jt = 0, 1
       swap = (jt.ne.0)
       if (ierr.eq.0) call sus_edit_slice_irec(ierr, u, vi(jb:je-1), bes, r, swap=swap)
       if (ierr.eq.0) call sus_edit_slice_irec(ierr, u, vl(jb:je-1), bes, r, swap=swap)
       if (ierr.eq.0) call sus_edit_slice_irec(ierr, u, vf(jb:je-1), bes, r, swap=swap)
       if (ierr.eq.0) call sus_edit_slice_irec(ierr, u, vd(jb:je-1), bes, r, swap=swap)
       if (ierr.eq.0) call sus_edit_slice_irec(ierr, u, va(jb:je-1), bes, r, swap=swap)
    enddo
    do jt = 0, 1
       swap = (jt.ne.0)
       if (ierr.eq.0) call sus_edit_slice_irec(ierr, u, vi(jb:je-1), bes, r, swap=swap)
       if (ierr.eq.0) call sus_edit_slice_irec(ierr, u, vl(jb:je-1), bes, r, swap=swap)
       if (ierr.eq.0) call sus_edit_slice_irec(ierr, u, vf(jb:je-1), bes, r, swap=swap)
       if (ierr.eq.0) call sus_edit_slice_irec(ierr, u, vd(jb:je-1), bes, r, swap=swap)
       if (ierr.eq.0) call sus_edit_slice_irec(ierr, u, va(jb:je-1), bes, r, swap=swap)
    enddo

  end subroutine test_slice_write

  subroutine test_slice_read  &
       & (ierr, u, jb, je, nm)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: u
    integer,intent(in)  :: jb, je, nm

    integer(kind=KI32) :: vi(0:nm-1), xi(0:nm-1)
    integer(kind=KI64) :: vl(0:nm-1), xl(0:nm-1)
    real(kind=KFLT)    :: vf(0:nm-1), xf(0:nm-1)
    real(kind=KDBL)    :: vd(0:nm-1), xd(0:nm-1)
    character(len=16)  :: va(0:nm-1), xa(0:nm-1)
    integer j
    integer jt
    logical swap
    integer,parameter :: r = 1
    integer bes(3, r)

    ierr = 0

    do j = 0, nm - 1
       vi(j) = j
       vl(j) = j
       vf(j) = j
       vd(j) = j
       write(va(j), '(I8.8)') j
    enddo

    bes(:, r) = (/jb, je, nm/)

101 format('slice/check:', A, 1x, L1)
    do jt = 0, 1
       swap = (jt.ne.0)
       if (ierr.eq.0) call sus_slice_read_irec(ierr, u, xi(jb:je-1), bes, r, swap=swap)
       if (ierr.eq.0) call sus_slice_read_irec(ierr, u, xl(jb:je-1), bes, r, swap=swap)
       if (ierr.eq.0) call sus_slice_read_irec(ierr, u, xf(jb:je-1), bes, r, swap=swap)
       if (ierr.eq.0) call sus_slice_read_irec(ierr, u, xd(jb:je-1), bes, r, swap=swap)
       if (ierr.eq.0) call sus_slice_read_irec(ierr, u, xa(jb:je-1), bes, r, swap=swap)
       if (ierr.eq.0) then
          write(*, 101) 'i', ALL(vi(jb:je-1).eq.xi(jb:je-1))
          write(*, 101) 'l', ALL(vl(jb:je-1).eq.xl(jb:je-1))
          write(*, 101) 'f', ALL(vf(jb:je-1).eq.xf(jb:je-1))
          write(*, 101) 'd', ALL(vd(jb:je-1).eq.xd(jb:je-1))
          write(*, 101) 'a', ALL(va(jb:je-1).eq.xa(jb:je-1))
       endif
    enddo
    do jt = 0, 1
       swap = (jt.ne.0)
       if (ierr.eq.0) call sus_slice_read_irec(ierr, u, xi(jb:je-1), bes, r, swap=swap)
       if (ierr.eq.0) call sus_slice_read_irec(ierr, u, xl(jb:je-1), bes, r, swap=swap)
       if (ierr.eq.0) call sus_slice_read_irec(ierr, u, xf(jb:je-1), bes, r, swap=swap)
       if (ierr.eq.0) call sus_slice_read_irec(ierr, u, xd(jb:je-1), bes, r, swap=swap)
       if (ierr.eq.0) call sus_slice_read_irec(ierr, u, xa(jb:je-1), bes, r, swap=swap)
       if (ierr.eq.0) then
          write(*, 101) 'i', ALL(vi(jb:je-1).eq.xi(jb:je-1))
          write(*, 101) 'l', ALL(vl(jb:je-1).eq.xl(jb:je-1))
          write(*, 101) 'f', ALL(vf(jb:je-1).eq.xf(jb:je-1))
          write(*, 101) 'd', ALL(vd(jb:je-1).eq.xd(jb:je-1))
          write(*, 101) 'a', ALL(va(jb:je-1).eq.xa(jb:je-1))
       endif
    enddo

  end subroutine test_slice_read

  subroutine test_runl_read  &
       & (ierr, u, jb, je, nm)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: u
    integer,intent(in)  :: jb, je, nm

    integer(kind=KI32) :: vi(0:nm-1), xi(0:nm-1)
    integer(kind=KI64) :: vl(0:nm-1), xl(0:nm-1)
    real(kind=KFLT)    :: vf(0:nm-1), xf(0:nm-1)
    real(kind=KDBL)    :: vd(0:nm-1), xd(0:nm-1)
    character(len=16)  :: va(0:nm-1), xa(0:nm-1)
    integer j
    integer jt
    logical swap
    integer,parameter :: nrl = 3
    integer runl(0:nrl-1)

    ierr = 0

    do j = 0, nm - 1
       vi(j) = j
       vl(j) = j
       vf(j) = j
       vd(j) = j
       write(va(j), '(I8.8)') j
    enddo

    runl(:) = (/jb, je - jb, 999/)

101 format('runl/check:', A, 1x, L1)
    do jt = 0, 1
       swap = (jt.ne.0)
       if (ierr.eq.0) call sus_runl_read_irec(ierr, u, xi(jb:je-1), runl, nrl, swap=swap)
       if (ierr.eq.0) call sus_runl_read_irec(ierr, u, xl(jb:je-1), runl, nrl, swap=swap)
       if (ierr.eq.0) call sus_runl_read_irec(ierr, u, xf(jb:je-1), runl, nrl, swap=swap)
       if (ierr.eq.0) call sus_runl_read_irec(ierr, u, xd(jb:je-1), runl, nrl, swap=swap)
       if (ierr.eq.0) call sus_runl_read_irec(ierr, u, xa(jb:je-1), runl, nrl, swap=swap)
       if (ierr.eq.0) then
          write(*, 101) 'i', ALL(vi(jb:je-1).eq.xi(jb:je-1))
          write(*, 101) 'l', ALL(vl(jb:je-1).eq.xl(jb:je-1))
          write(*, 101) 'f', ALL(vf(jb:je-1).eq.xf(jb:je-1))
          write(*, 101) 'd', ALL(vd(jb:je-1).eq.xd(jb:je-1))
          write(*, 101) 'a', ALL(va(jb:je-1).eq.xa(jb:je-1))
       endif
    enddo
    do jt = 0, 1
       swap = (jt.ne.0)
       if (ierr.eq.0) call sus_runl_read_irec(ierr, u, xi(jb:je-1), runl, nrl, swap=swap)
       if (ierr.eq.0) call sus_runl_read_irec(ierr, u, xl(jb:je-1), runl, nrl, swap=swap)
       if (ierr.eq.0) call sus_runl_read_irec(ierr, u, xf(jb:je-1), runl, nrl, swap=swap)
       if (ierr.eq.0) call sus_runl_read_irec(ierr, u, xd(jb:je-1), runl, nrl, swap=swap)
       if (ierr.eq.0) call sus_runl_read_irec(ierr, u, xa(jb:je-1), runl, nrl, swap=swap)
       if (ierr.eq.0) then
          write(*, 101) 'i', ALL(vi(jb:je-1).eq.xi(jb:je-1))
          write(*, 101) 'l', ALL(vl(jb:je-1).eq.xl(jb:je-1))
          write(*, 101) 'f', ALL(vf(jb:je-1).eq.xf(jb:je-1))
          write(*, 101) 'd', ALL(vd(jb:je-1).eq.xd(jb:je-1))
          write(*, 101) 'a', ALL(va(jb:je-1).eq.xa(jb:je-1))
       endif
    enddo

  end subroutine test_runl_read

  subroutine test_list_read  &
       & (ierr, u, jb, je, nm)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: u
    integer,intent(in)  :: jb, je, nm

    integer(kind=KI32) :: vi(0:nm-1), xi(0:nm-1)
    integer(kind=KI64) :: vl(0:nm-1), xl(0:nm-1)
    real(kind=KFLT)    :: vf(0:nm-1), xf(0:nm-1)
    real(kind=KDBL)    :: vd(0:nm-1), xd(0:nm-1)
    character(len=16)  :: va(0:nm-1), xa(0:nm-1)
    integer j
    integer jt
    logical swap
    integer nl
    integer list(0:nm-1)

    ierr = 0

    do j = 0, nm - 1
       vi(j) = j
       vl(j) = j
       vf(j) = j
       vd(j) = j
       write(va(j), '(I8.8)') j
    enddo
    do j = jb, je - 1
       list(j-jb) = j
    enddo
    nl = je - jb
    ! write(*, *) list(0:nl - 1)

101 format('list/check:', A, 1x, L1)
    do jt = 0, 1
       swap = (jt.ne.0)
       if (ierr.eq.0) call sus_list_read_irec(ierr, u, xi(jb:je-1), list, nl, swap=swap)
       if (ierr.eq.0) call sus_list_read_irec(ierr, u, xl(jb:je-1), list, nl, swap=swap)
       if (ierr.eq.0) call sus_list_read_irec(ierr, u, xf(jb:je-1), list, nl, swap=swap)
       if (ierr.eq.0) call sus_list_read_irec(ierr, u, xd(jb:je-1), list, nl, swap=swap)
       if (ierr.eq.0) call sus_list_read_irec(ierr, u, xa(jb:je-1), list, nl, swap=swap)
       if (ierr.eq.0) then
          write(*, 101) 'i', ALL(vi(jb:je-1).eq.xi(jb:je-1))
          write(*, 101) 'l', ALL(vl(jb:je-1).eq.xl(jb:je-1))
          write(*, 101) 'f', ALL(vf(jb:je-1).eq.xf(jb:je-1))
          write(*, 101) 'd', ALL(vd(jb:je-1).eq.xd(jb:je-1))
          write(*, 101) 'a', ALL(va(jb:je-1).eq.xa(jb:je-1))
       endif
    enddo
    do jt = 0, 1
       swap = (jt.ne.0)
       if (ierr.eq.0) call sus_list_read_irec(ierr, u, xi(jb:je-1), list, nl, swap=swap)
       if (ierr.eq.0) call sus_list_read_irec(ierr, u, xl(jb:je-1), list, nl, swap=swap)
       if (ierr.eq.0) call sus_list_read_irec(ierr, u, xf(jb:je-1), list, nl, swap=swap)
       if (ierr.eq.0) call sus_list_read_irec(ierr, u, xd(jb:je-1), list, nl, swap=swap)
       if (ierr.eq.0) call sus_list_read_irec(ierr, u, xa(jb:je-1), list, nl, swap=swap)
       if (ierr.eq.0) then
          write(*, 101) 'i', ALL(vi(jb:je-1).eq.xi(jb:je-1))
          write(*, 101) 'l', ALL(vl(jb:je-1).eq.xl(jb:je-1))
          write(*, 101) 'f', ALL(vf(jb:je-1).eq.xf(jb:je-1))
          write(*, 101) 'd', ALL(vd(jb:je-1).eq.xd(jb:je-1))
          write(*, 101) 'a', ALL(va(jb:je-1).eq.xa(jb:je-1))
       endif
    enddo

  end subroutine test_list_read

  subroutine test_slice_create  &
       & (ierr, u, dims)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: u
    integer,intent(in)  :: dims(0:)

    integer,allocatable :: vi(:)
    integer jr, jm
    integer nr, nm
    integer jb,  je,   nd,  md
    logical pre, post, swap
    integer jt

    ierr = 0
    nr = size(dims)
    nm = 1
    do jr = 0, nr - 1
       nm = nm * dims(jr)
    enddo
    allocate(vi(0:nm-1), STAT=ierr)
    if (ierr.eq.0) then
       do jm = 0, nm - 1
          vi(jm) = jm
       enddo
    endif
    if (ierr.eq.0) call sus_write_irec(ierr, u, vi, nm)
    if (ierr.eq.0) call sus_write_irec(ierr, u, vi, nm, .TRUE.)

    nd = nm / 3 + 1
    do jt = 0, 1
       pre = .FALSE.
       post = .TRUE.
       swap = (jt.ne.0)
       do jb = 0, nm - 1, nd
          md = min(nd, nm - jb)
          je = jb + md
          post = je.lt.nm
          if (ierr.eq.0) call sus_write_irec(ierr, u, vi(jb:je), md, swap, pre, post)
          pre = .TRUE.
       enddo
    enddo
    if (ierr.eq.0) deallocate(vi, STAT=ierr)

  end subroutine test_slice_create

  subroutine batch_slice_read  &
       & (ierr, u, dims)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: u
    integer,intent(in)  :: dims(0:)

    integer,allocatable :: vi(:)
    integer,allocatable :: vx(:)
    integer bes(3, 0:size(dims))

    integer jt, jtt
    integer jp, np
    integer nr, jr
    integer nm
    logical swap
    integer jerr

    ierr = 0
    nr = size(dims)
    nm = 1
    do jr = 0, nr - 1
       nm = nm * dims(jr)
    enddo
    allocate(vi(0:nm*2-1), vx(0:nm*2-1), STAT=ierr)

    np = 2 ** nr
    bes(3, 0:nr-1) = dims(0:nr-1)
    bes(1:3, nr) = (/0, 2, 2/)

    do jp = 0, np - 1
       ! if (jp.ne.1) cycle
       do jr = 0, nr - 1
          if (ibits(jp, jr, 1).eq.0) then
             bes(1, jr) = 0
             bes(2, jr) = dims(jr)
          else
             bes(1, jr) = 1
             bes(2, jr) = dims(jr) - 1
          endif
       enddo
       if (ierr.eq.0) call test_slice_ref(ierr, vx, bes, nr)
       if (ierr.eq.0) rewind(UNIT=u, IOSTAT=ierr)
       do jt = 0, 3
          swap = mod(jt, 2).eq.1
          if (ierr.eq.0) call sub_slice_read(ierr, vi, vx, u, swap, bes, nr, jp, jt)
       enddo

       do jt = 0, 3
          if (ierr.eq.0) rewind(UNIT=u, IOSTAT=ierr)
          do jtt = 0, jt - 1
             swap = mod(jtt, 2).eq.1
             if (ierr.eq.0) call sus_skip_irec(ierr, u, 1, swap=swap)
          enddo
          swap = mod(jt, 2).eq.1
          call sub_slice_read(jerr, vi, vx, u, swap, bes, nr + 1, jp, jt)
       enddo
    enddo

    if (ierr.eq.0) deallocate(vi, vx, STAT=ierr)
  end subroutine batch_slice_read

  subroutine sub_slice_read  &
       & (ierr, vi, vx, u, swap, bes, nr, jp, jt)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: vi(0:*)
    integer,intent(in)  :: vx(0:*)
    integer,intent(in)  :: u
    logical,intent(in)  :: swap
    integer,intent(in)  :: bes(3, 0:*)
    integer,intent(in)  :: nr
    integer,intent(in)  :: jp, jt
    integer jr
    integer j, m
    m = 1
    do jr = 0, nr - 1
       m = m * (bes(2,jr) - bes(1,jr))
    enddo
    ierr = 0
    vi(0:m-1) = -1
    call sus_slice_read_irec(ierr, u, vi, bes, nr, swap=swap)
    if (ierr.eq.0) then
       if (ALL(vi(0:m-1).eq.vx(0:m-1))) then
          write(*, *) jp, jt, 'identical', m
       else
          do j = 0, m - 1
             if (vi(j).ne.vx(j)) then
                write(*, *) jp, jt, j, vi(j), vx(j)
             endif
          enddo
       endif
    else
       write(*, *) jp, jt, 'error = ', ierr
    endif
  end subroutine sub_slice_read

  subroutine test_slice_ref &
       & (ierr, vx, bes, nr)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: vx(0:*)
    integer,intent(in)  :: bes(3, 0:*)
    integer,intent(in)  :: nr
    integer idx(0:nr-1)
    integer jr, np
    integer jp, jl
    ierr = 0

    j = 0
    np = 1
    do jr = 0, nr - 1
       np = np * bes(3, jr)
    enddo
    idx(0:nr-1) = 0
    jl = 0
    do jp = 0, np - 1
       ! write(*, *) jp, ixd(0:nr-1), jl
       if (ALL(idx(0:nr-1).ge.bes(1,0:nr-1)) &
            & .and. ALL(idx(0:nr-1).lt.bes(2,0:nr-1))) then
          vx(jl) = jp
          jl = jl + 1
       endif
       do jr = 0, nr - 1
          idx(jr) = idx(jr) + 1
          if (idx(jr).lt.bes(3,jr)) exit
          idx(jr) = 0
       enddo
    enddo
  end subroutine test_slice_ref

end program test_std_sus

#endif /* TEST_STD_SUS */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

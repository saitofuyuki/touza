!!!_! std_sus.F90 - TOUZA/Std stream i/o to emulate unformatted sequential access
! Maintainer: SAITO Fuyuki
! Transferred: Dec 24 2021
! Created: Oct 17 2021 (nng_io)
#define TIME_STAMP 'Time-stamp: <2022/10/24 13:22:07 fuyuki std_sus.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021,2022
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_std.h"
#if    HAVE_FORTRAN_OPEN_STREAM
#else
#      error "stream access unavailable"
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

  integer,parameter,public :: RECL_MAX_BYTES = HUGE(0_KI32) - 4 * 2
  ! integer,parameter,public :: RECL_MAX_BYTES = 24

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

  integer,save :: mstrm_isep = 0, mstrm_lsep = 0   !! separator sizes in stream unit

  integer,save :: maxmemi_i = 0, maxmemi_l = 0 !! max members in a sub-record (32-bit marker)
  integer,save :: maxmemi_f = 0, maxmemi_d = 0

  integer(kind=KI32),save :: lsubr = 0

  integer,save :: last_iostat = 0
!!!_  - interfaces
  interface sus_write_irec
     module procedure sus_write_irec_i
     module procedure sus_write_irec_l
     module procedure sus_write_irec_d
     module procedure sus_write_irec_f
     module procedure sus_write_irec_a
  end interface sus_write_irec
  interface sus_write_lrec
     module procedure sus_write_lrec_i
     module procedure sus_write_lrec_l
     module procedure sus_write_lrec_d
     module procedure sus_write_lrec_f
     module procedure sus_write_lrec_a
  end interface sus_write_lrec

  interface sus_read_irec
     module procedure sus_read_irec_i
     module procedure sus_read_irec_l
     module procedure sus_read_irec_d
     module procedure sus_read_irec_f
     module procedure sus_read_irec_a
  end interface sus_read_irec
  interface sus_read_lrec
     module procedure sus_read_lrec_i
     module procedure sus_read_lrec_l
     module procedure sus_read_lrec_d
     module procedure sus_read_lrec_f
     module procedure sus_read_lrec_a
  end interface sus_read_lrec

  interface sus_write_isep
     module procedure sus_write_isep_l
     module procedure sus_write_isep_i
  end interface sus_write_isep
  interface sus_write_lsep
     module procedure sus_write_lsep_l
     module procedure sus_write_lsep_i
  end interface sus_write_lsep

  interface sus_read_isep
     module procedure sus_read_isep_l
     module procedure sus_read_isep_i
  end interface sus_read_isep
  interface sus_read_lsep
     module procedure sus_read_lsep_l
     module procedure sus_read_lsep_i
  end interface sus_read_lsep

  interface sus_write
     module procedure sus_write_i
     module procedure sus_write_l
     module procedure sus_write_d
     module procedure sus_write_f
     module procedure sus_write_a
  end interface sus_write

  interface sus_read
     module procedure sus_read_i
     module procedure sus_read_l
     module procedure sus_read_d
     module procedure sus_read_f
     module procedure sus_read_a
  end interface sus_read

  interface sus_eswap
     module procedure sus_eswap_i
     module procedure sus_eswap_l
  end interface sus_eswap

  interface sus_swap
     module procedure sus_swap_i
     module procedure sus_swap_l
  end interface sus_swap

  interface mstrm_sep
     module procedure mstrm_sep_i
     module procedure mstrm_sep_l
  end interface mstrm_sep

  interface max_members
     module procedure max_members_a
     module procedure max_members_i
     module procedure max_members_l
     module procedure max_members_f
     module procedure max_members_d
  end interface max_members

!!!_  - public procedures
  public init, diag, finalize
  public sus_open, sus_close

  public sus_write_irec,  sus_read_irec,  sus_skip_irec
  public sus_write_lrec,  sus_read_lrec,  sus_skip_lrec

  ! public sus_write_begin_irec, sus_write_end_irec
  ! public sus_read_begin_irec,  sus_read_end_irec

  public sus_write_isep,  sus_read_isep, sus_read
  public sus_write_lsep,  sus_read_lsep, sus_write

  public sus_rseek
  public sus_eswap

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, icomm)
    use TOUZA_Std_prc,only: prc_init=>init
    use TOUZA_Std_utl,only: utl_init=>init, choice
    use TOUZA_Std_fun,only: fun_init=>init
    use TOUZA_Std_log,only: log_init=>init
    use TOUZA_Std_env,only: env_init=>init, get_size_strm, get_size_bytes
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
             mstrm_isep = get_size_strm(0_KI32)
             mstrm_lsep = get_size_strm(0_KI64)
          endif
          if (ierr.eq.0) then
             lsubr = RECL_MAX_BYTES
             maxmemi_i = lsubr / get_size_bytes(0_KI32)
             maxmemi_l = lsubr / get_size_bytes(0_KI64)
             maxmemi_f = lsubr / get_size_bytes(0.0_KFLT)
             maxmemi_d = lsubr / get_size_bytes(0.0_KDBL)
             if (maxmemi_i.le.0) ierr = ERR_FATAL
             if (maxmemi_l.le.0) ierr = ERR_FATAL
             if (maxmemi_f.le.0) ierr = ERR_FATAL
             if (maxmemi_d.le.0) ierr = ERR_FATAL
             if (ierr.ne.0) then
                write(*,*) 'FATAL:', maxmemi_i, maxmemi_l, maxmemi_f, maxmemi_d
             endif
          endif
       endif
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT - ERR_MASK_STD_SUS
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
          call msg('(''final iostat saved = '', I0)', last_iostat, __MDL__, u)
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
    integer jposa, jposb

    ierr = 0
    ut = choice(-1, utest)
    if (ut.lt.0) then
       if (ut.lt.0) ut = new_unit()
       if (ut.lt.0) ierr = ERR_NO_IO_UNIT - ERR_MASK_STD_SUS
    endif
    if (ierr.eq.0) then
       open(UNIT=ut, IOSTAT=ierr, &
            &        ACCESS='STREAM', FORM='UNFORMATTED', STATUS='SCRATCH', ACTION='READWRITE')
    endif
    if (ierr.eq.0) write(ut, IOSTAT=ierr) 'TEST'
    if (ierr.eq.0) inquire(ut, IOSTAT=ierr, POS=jposa)
    if (ierr.eq.0) write(ut, IOSTAT=ierr, POS=1)
    if (ierr.eq.0) inquire(ut, IOSTAT=ierr, POS=jposb)
    if (ierr.eq.0) then
       if (jposa.eq.jposb) then
          call msg_mdl('(''positioning/write not works '', I0, 1x, I0)', &
               & (/jposa, jposb/), __MDL__, ulog)
          ierr = ERR_OPR_DISABLE - ERR_MASK_STD_SUS
       endif
    endif
    if (ierr.eq.0) then
       if (ierr.eq.0) write(ut, IOSTAT=ierr, POS=jposa)
       if (ierr.eq.0) read(ut, IOSTAT=ierr, POS=1)
       if (ierr.eq.0) inquire(ut, IOSTAT=ierr, POS=jposb)
    endif
    if (ierr.eq.0) then
       if (jposa.eq.jposb) then
          call msg_mdl('(''positioning/read not works '', I0, 1x, I0)', &
               & (/jposa, jposb/), __MDL__, ulog)
#if OPT_STREAM_RPOS_WORKAROUND
          call msg_mdl('workaround enabled', __MDL__, ulog)
#else /* not OPT_STREAM_RPOS_WORKAROUND */
          call msg_mdl('need rebuild with workaround', __MDL__, ulog)
          ierr = ERR_OPR_DISABLE + ERR_MASK_STD_SUS
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
!!!_  - sus_open - open stream
  subroutine sus_open &
       & (ierr, u,      file, &
       &  form, status, action, position)
    use TOUZA_Std_utl,only: choice_a, upcase
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: u
    character(len=*),intent(in)          :: file
    character(len=*),intent(in),optional :: form
    character(len=*),intent(in),optional :: status
    character(len=*),intent(in),optional :: action
    character(len=*),intent(in),optional :: position

    integer(kind=KIOFS) :: jpos
    character(len=16) :: STT, ACT, FRM, POS

    ierr = ERR_SUCCESS

    call choice_a(STT, 'U', status)
    call choice_a(ACT, 'R', action)
    call choice_a(FRM, 'U', form)
    call choice_a(POS, ' ', position)

    call upcase(STT)
    call upcase(ACT)
    call upcase(FRM)
    call upcase(POS)

    if (STT(1:1).eq.'U'.or.STT.eq.' ') then
       STT = 'UNKNOWN'
    else if (STT(1:1).eq.'O') then
       STT = 'OLD'
    else if (STT(1:1).eq.'N') then
       STT = 'NEW'
    else if (STT(1:1).eq.'R') then
       STT = 'REPLACE'
    endif

    if (ACT.eq.'RW') then
       ACT = 'READWRITE'
    else if (ACT.eq.'R'.or.ACT.eq.' ') then
       ACT = 'READ'
    else if (ACT.eq.'W') then
       ! ACT = 'READWRITE'
       ACT = 'WRITE'
    endif

    if (FRM(1:1).eq.'U'.or.FRM.eq.' ') then
       FRM='UNFORMATTED'
    else if (FORM(1:1).eq.'F') then
       FRM='FORMATTED'
    endif

    if (POS(1:2).eq.'AP') then
       POS = 'APPEND'
    else if (POS(1:1).eq.'R') then
       POS = 'REWIND'
    else
       POS = 'ASIS'
    endif

    if (ierr.eq.0) then
       open(UNIT=u, IOSTAT=ierr, &
            &       FILE=file, ACCESS='STREAM', &
            &       FORM=FRM,  STATUS=STT, ACTION=ACT, POSITION=POS)
    endif
  end subroutine sus_open
!!!_  - sus_close - close stream
  subroutine sus_close(ierr, u, file)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: u
    character(len=*),intent(in)  :: file
    ierr = ERR_SUCCESS
    close(UNIT=u, IOSTAT=ierr)
  end subroutine sus_close

!!!_  - sus_skip_irec - forward/backward 32-bit marker records
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
    integer(KIND=KIOFS) :: jpos
    integer j

    ierr = err_default

    if (ierr.eq.0) call sus_rseek(ierr, u, whence=whence)

    if (.not.present(n)) return

    if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
    if (n.gt.0) then
       j = 0
       do
          if (ierr.eq.0) then
             call sus_read_isep(ierr, u, iseph, swap=swap)
             if (is_eof_ss(ierr)) then
                ierr = ERR_EOF
                exit
             endif
          endif
          if (ierr.eq.0) jpos = jpos + conv_b2strm(abs(iseph)) + mstrm_sep(iseph)
          if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=jpos, swap=swap)
          if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
          if (ierr.eq.0) then
             if (abs(iseph).ne.abs(isepf)) ierr = ERR_INCONSISTENT_RECORD_MARKERS
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
          if (ierr.eq.0) jpos = jpos - mstrm_sep(isepf)
          if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=jpos, swap=swap)
          if (ierr.eq.0) jpos = jpos - conv_b2strm(abs(isepf)) - mstrm_sep(iseph)
          if (ierr.eq.0) call sus_read_isep(ierr, u, iseph, pos=jpos, swap=swap)
          if (ierr.eq.0) then
             if (abs(iseph).ne.abs(isepf)) ierr = ERR_INCONSISTENT_RECORD_MARKERS
          else
             ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          endif
          if (ierr.ne.0) exit
          if (isepf.ge.0) j = j + 1
          if (j.eq.-n) exit
       enddo
#if OPT_STREAM_RPOS_WORKAROUND
       if (ierr.eq.0) call sus_rseek_workaround(ierr, u, jpos)
#else /* not OPT_STREAM_RPOS_WORKAROUND */
       if (ierr.eq.0) read(UNIT=u, IOSTAT=ierr, POS=jpos)
#endif /* not OPT_STREAM_RPOS_WORKAROUND */
    endif
  end subroutine sus_skip_irec

!!!_  - sus_skip_lrec - forward/backward 64-bit marker records
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
    integer(KIND=KIOFS) :: jpos
    integer j

    ierr = err_default

    if (ierr.eq.0) call sus_rseek(ierr, u, whence=whence)

    if (.not.present(n)) return

    inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
    if (n.gt.0) then
       do j = 1, n
          if (ierr.eq.0) then
             call sus_read_lsep(ierr, u, lseph, swap=swap)
             if (is_eof_ss(ierr)) then
                ierr = ERR_EOF
                exit
             endif
          endif
          if (ierr.eq.0) jpos = jpos + conv_b2strm(lseph) + mstrm_sep(lseph)
          if (ierr.eq.0) call sus_read_lsep(ierr, u, lsepf, pos=jpos, swap=swap)
          if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
          if (ierr.eq.0) then
             if (lseph.ne.lsepf) ierr = ERR_INCONSISTENT_RECORD_MARKERS
          else
             ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          endif
          if (ierr.ne.0) exit
       enddo
    else if (n.lt.0) then
       do j = 1, -n
          if (ierr.eq.0) jpos = jpos - mstrm_sep(lsepf)
          if (ierr.eq.0) call sus_read_lsep(ierr, u, lsepf, pos=jpos, swap=swap)
          if (ierr.eq.0) jpos = jpos - conv_b2strm(lsepf) - mstrm_sep(lseph)
          if (ierr.eq.0) call sus_read_lsep(ierr, u, lseph, pos=jpos, swap=swap)
          if (ierr.eq.0) then
             if (lseph.ne.lsepf) ierr = ERR_INCONSISTENT_RECORD_MARKERS
          else
             ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          endif
          if (ierr.ne.0) exit
       enddo
#if OPT_STREAM_RPOS_WORKAROUND
       if (ierr.eq.0) call sus_rseek_workaround(ierr, u, jpos)
#else /* not OPT_STREAM_RPOS_WORKAROUND */
       if (ierr.eq.0) read(UNIT=u, IOSTAT=ierr, POS=jpos)
#endif /* not OPT_STREAM_RPOS_WORKAROUND */
    endif
  end subroutine sus_skip_lrec

!!!_  - sus_write_irec - write a record with 32bit-marker
  subroutine sus_write_irec_i &
       & (ierr, u, v, n, swap, pre, post)
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u
    integer(KIND=KARG),intent(in)          :: V(*)
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    logical,           intent(in),optional :: pre, post
    integer(KIND=KISEP) :: isep
    integer j, m, ns

    ierr = err_default
    ns = max_members(V(1))
    ! write(*, *) 'irec/i', ierr, ns, n
    if (n.le.ns) then
       isep = get_size_bytes(V(1), n)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) call sus_write(ierr, u, V, n, swap)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=pre)
    else
       m = n
       j = 0
       isep = get_size_bytes(V(1), ns)
       ! first
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       if (ierr.eq.0) call sus_write(ierr, u, V(j+1:j+ns), ns, swap)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=pre)
       ! write(*, *) 'irec/i/first', ierr, ns, n, m, j
       ! middle
       do
          m = m - ns
          j = j + ns
          ! write(*, *) 'irec/i/middle', ierr, ns, n, m, j
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
          if (ierr.eq.0) call sus_write(ierr, u, V(j+1:j+ns), ns, swap)
          if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       enddo
       ! write(*, *) 'irec/i/last', ierr, ns, n, m, j
       ! last
       isep = get_size_bytes(V(1), m)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) call sus_write(ierr, u, V(j+1:j+m), m, swap)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
    endif
    ! stop
    return
  end subroutine sus_write_irec_i
  subroutine sus_write_irec_l &
       & (ierr, u, v, n, swap, pre, post)
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI64
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u
    integer(KIND=KARG),intent(in)          :: V(*)
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    logical,           intent(in),optional :: pre, post
    integer(KIND=KISEP) :: isep
    integer j, m, ns

    ierr = err_default
    ns = max_members(V(1))
    if (n.le.ns) then
       isep = get_size_bytes(V(1), n)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) call sus_write(ierr, u, V, n, swap)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=pre)
    else
       m = n
       j = 0
       isep = get_size_bytes(V(1), ns)
       ! first
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       if (ierr.eq.0) call sus_write(ierr, u, V(j+1:j+ns), ns, swap)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=pre)
       ! middle
       do
          m = m - ns
          j = j + ns
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
          if (ierr.eq.0) call sus_write(ierr, u, V(j+1:j+ns), ns, swap)
          if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       enddo
       ! last
       isep = get_size_bytes(V(1), m)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) call sus_write(ierr, u, V(j+1:j+m), m, swap)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
    endif
    return
  end subroutine sus_write_irec_l
  subroutine sus_write_irec_f &
       & (ierr, u, v, n, swap, pre, post)
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KFLT
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u
    real(KIND=KARG),intent(in)          :: V(*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    logical,        intent(in),optional :: pre, post
    integer(KIND=KISEP) :: isep
    integer j, m, ns

    ierr = err_default
    ns = max_members(V(1))
    if (n.le.ns) then
       isep = get_size_bytes(V(1), n)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) call sus_write(ierr, u, V, n, swap)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=pre)
    else
       m = n
       j = 0
       isep = get_size_bytes(V(1), ns)
       ! first
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       if (ierr.eq.0) call sus_write(ierr, u, V(j+1:j+ns), ns, swap)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=pre)
       ! middle
       do
          m = m - ns
          j = j + ns
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
          if (ierr.eq.0) call sus_write(ierr, u, V(j+1:j+ns), ns, swap)
          if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       enddo
       ! last
       isep = get_size_bytes(V(1), m)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) call sus_write(ierr, u, V(j+1:j+m), m, swap)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
    endif
    return
  end subroutine sus_write_irec_f
  subroutine sus_write_irec_d &
       & (ierr, u, v, n, swap, pre, post)
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KDBL
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u
    real(KIND=KARG),intent(in)          :: V(*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    logical,        intent(in),optional :: pre, post
    integer(KIND=KISEP) :: isep
    integer j, m, ns

    ierr = err_default
    ns = max_members(V(1))
    if (n.le.ns) then
       isep = get_size_bytes(V(1), n)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) call sus_write(ierr, u, V, n, swap)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=pre)
    else
       m = n
       j = 0
       isep = get_size_bytes(V(1), ns)
       ! first
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       if (ierr.eq.0) call sus_write(ierr, u, V(j+1:j+ns), ns, swap)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=pre)
       ! middle
       do
          m = m - ns
          j = j + ns
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
          if (ierr.eq.0) call sus_write(ierr, u, V(j+1:j+ns), ns, swap)
          if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       enddo
       ! last
       isep = get_size_bytes(V(1), m)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) call sus_write(ierr, u, V(j+1:j+m), m, swap)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
    endif
    return
  end subroutine sus_write_irec_d
  subroutine sus_write_irec_a &
       & (ierr, u, v, n, swap, pre, post)
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI32
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: u
    character(len=*),intent(in)          :: V(*)
    integer,         intent(in)          :: n
    logical,         intent(in),optional :: swap
    logical,         intent(in),optional :: pre, post
    integer(KIND=KISEP) :: isep
    integer j, m, ns

    ierr = err_default
    ns = max_members(V(1))
    if (ns.le.0) ierr = ERR_PANIC
    ! write (*, *) 'write/a', ns, n
    if (n.le.ns) then
       isep = get_size_bytes(V(1), n)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) call sus_write(ierr, u, V, n, swap)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=pre)
    else
       m = n
       j = 0
       isep = get_size_bytes(V(1), ns)
       ! first
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       if (ierr.eq.0) call sus_write(ierr, u, V(j+1:j+ns), ns, swap)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=pre)
       ! middle
       do
          m = m - ns
          j = j + ns
          if (m.le.ns) exit
          if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
          if (ierr.eq.0) call sus_write(ierr, u, V(j+1:j+ns), ns, swap)
          if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       enddo
       ! last
       isep = get_size_bytes(V(1), m)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) call sus_write(ierr, u, V(j+1:j+m), m, swap)
       if (ierr.eq.0) call sus_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
    endif
    return
  end subroutine sus_write_irec_a

!!!_  - sus_write_lrec - write a record with 64bit-marker
  subroutine sus_write_lrec_i &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_env,only: get_size_bytes
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KI32
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u
    integer(KIND=KARG),intent(in)          :: V(*)
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    integer(KIND=KISEP) :: lsep
    ierr = err_default
    lsep = get_size_bytes(V(1), int(n, kind=KISEP))
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
    integer(KIND=KARG),intent(in)          :: V(*)
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    integer(KIND=KISEP) :: lsep
    ierr = err_default
    lsep = get_size_bytes(V(1), int(n, kind=KISEP))
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
    real(KIND=KARG),intent(in)          :: V(*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    integer(KIND=KISEP) :: lsep
    ierr = err_default
    lsep = get_size_bytes(V(1), int(n, kind=KISEP))
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
    real(KIND=KARG),intent(in)          :: V(*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    integer(KIND=KISEP) :: lsep
    ierr = err_default
    lsep = get_size_bytes(V(1), int(n, kind=KISEP))
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
    character(len=*),intent(in)          :: V(*)
    integer,         intent(in)          :: n
    logical,         intent(in),optional :: swap
    integer(KIND=KISEP) :: lsep
    ierr = err_default
    lsep = get_size_bytes(V(1), int(n, kind=KISEP))
    if (ierr.eq.0) call sus_write_lsep(ierr, u, lsep, swap=swap)
    if (ierr.eq.0) call sus_write(ierr, u, V, n, swap)
    if (ierr.eq.0) call sus_write_lsep(ierr, u, lsep, swap=swap)
    return
  end subroutine sus_write_lrec_a

!!!_  - sus_read_irec - read a record with 32bit-marker
  subroutine sus_read_irec_i &
       & (ierr, u, v, n, swap, sub)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,           intent(out)            :: ierr
    integer,           intent(in)             :: u
    integer(KIND=KARG),intent(out)            :: V(*)
    integer,           intent(in)             :: n
    logical,           intent(in),   optional :: swap
    logical,           intent(inout),optional :: sub
    ! if no SUB or F: ignore subrecords
    !    SUB=T:       read until v is filled.  set F when no more subrecords
    integer j, m, ns
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: jpos
    ierr = err_default
    j = 0
    m = n
    do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = ERR_EOF
             exit
          endif
       endif
       if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
       if (ierr.eq.0) then
          jpos = jpos + conv_b2strm(abs(iseph))
          ns = min(m, get_mems_bytes(abs(iseph), V(1)))
          m = m - ns
       endif
       if (ierr.eq.0) call sus_read(ierr, u, V(j+1:j+ns), ns, swap)
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=jpos, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit
          if (abs(iseph).ne.abs(isepf)) ierr = ERR_INCONSISTENT_RECORD_MARKERS
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit
       endif
       if (m.eq.0) exit
       j = j + ns
    enddo
    if (ierr.eq.0) then
       if (m.gt.0) then
          ierr = ERR_INVALID_RECORD_SIZE
       else if (choice(.false., sub)) then
          ! sub exist and T
          sub = (iseph.lt.0)    ! subrecord succeeds
       else if (iseph.lt.0) then
          ! skip all
          call sus_skip_irec(ierr, u, 1, swap=swap)
       endif
    endif
    return
  end subroutine sus_read_irec_i
  subroutine sus_read_irec_l &
       & (ierr, u, v, n, swap, sub)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI64
    integer,           intent(out)            :: ierr
    integer,           intent(in)             :: u
    integer(KIND=KARG),intent(out)            :: V(*)
    integer,           intent(in)             :: n
    logical,           intent(in),   optional :: swap
    logical,           intent(inout),optional :: sub
    integer j, m, ns
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: jpos
    ierr = err_default
    j = 0
    m = n
    do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = ERR_EOF
             exit
          endif
       endif
       if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
       if (ierr.eq.0) then
          jpos = jpos + conv_b2strm(abs(iseph))
          ns = min(m, get_mems_bytes(abs(iseph), V(1)))
          m = m - ns
       endif
       if (ierr.eq.0) call sus_read(ierr, u, V(j+1:j+ns), ns, swap)
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=jpos, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit
          if (abs(iseph).ne.abs(isepf)) ierr = ERR_INCONSISTENT_RECORD_MARKERS
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit
       endif
       if (m.eq.0) exit
       j = j + ns
    enddo
    if (ierr.eq.0) then
       if (m.gt.0) then
          ierr = ERR_INVALID_RECORD_SIZE
       else if (choice(.false., sub)) then
          ! sub exist and T
          sub = (iseph.lt.0)    ! subrecord succeeds
       else if (iseph.lt.0) then
          ! skip all
          call sus_skip_irec(ierr, u, 1, swap=swap)
       endif
    endif
    return
  end subroutine sus_read_irec_l
  subroutine sus_read_irec_f &
       & (ierr, u, v, n, swap, sub)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KFLT
    integer,        intent(out)            :: ierr
    integer,        intent(in)             :: u
    real(KIND=KARG),intent(out)            :: V(*)
    integer,        intent(in)             :: n
    logical,        intent(in),   optional :: swap
    logical,        intent(inout),optional :: sub
    integer j, m, ns
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: jpos
    ierr = err_default
    j = 0
    m = n
    do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = ERR_EOF
             exit
          endif
       endif
       if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
       if (ierr.eq.0) then
          jpos = jpos + conv_b2strm(abs(iseph))
          ns = min(m, get_mems_bytes(abs(iseph), V(1)))
          m = m - ns
       endif
       if (ierr.eq.0) call sus_read(ierr, u, V(j+1:j+ns), ns, swap)
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=jpos, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit
          if (abs(iseph).ne.abs(isepf)) ierr = ERR_INCONSISTENT_RECORD_MARKERS
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit
       endif
       if (m.eq.0) exit
       j = j + ns
    enddo
    if (ierr.eq.0) then
       if (m.gt.0) then
          ierr = ERR_INVALID_RECORD_SIZE
       else if (choice(.false., sub)) then
          ! sub exist and T
          sub = (iseph.lt.0)    ! subrecord succeeds
       else if (iseph.lt.0) then
          ! skip all
          call sus_skip_irec(ierr, u, 1, swap=swap)
       endif
    endif
    return
  end subroutine sus_read_irec_f
  subroutine sus_read_irec_d &
       & (ierr, u, v, n, swap, sub)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KDBL
    integer,        intent(out)            :: ierr
    integer,        intent(in)             :: u
    real(KIND=KARG),intent(out)            :: V(*)
    integer,        intent(in)             :: n
    logical,        intent(in),   optional :: swap
    logical,        intent(inout),optional :: sub
    integer j, m, ns
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: jpos
    ierr = err_default
    j = 0
    m = n
    do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = ERR_EOF
             exit
          endif
       endif
       if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
       if (ierr.eq.0) then
          jpos = jpos + conv_b2strm(abs(iseph))
          ns = min(m, get_mems_bytes(abs(iseph), V(1)))
          m = m - ns
       endif
       if (ierr.eq.0) call sus_read(ierr, u, V(j+1:j+ns), ns, swap)
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=jpos, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit
          if (abs(iseph).ne.abs(isepf)) ierr = ERR_INCONSISTENT_RECORD_MARKERS
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit
       endif
       if (m.eq.0) exit
       j = j + ns
    enddo
    if (ierr.eq.0) then
       if (m.gt.0) then
          ierr = ERR_INVALID_RECORD_SIZE
       else if (choice(.false., sub)) then
          ! sub exist and T
          sub = (iseph.lt.0)    ! subrecord succeeds
       else if (iseph.lt.0) then
          ! skip all
          call sus_skip_irec(ierr, u, 1, swap=swap)
       endif
    endif
    return
  end subroutine sus_read_irec_d
  subroutine sus_read_irec_a &
       & (ierr, u, v, n, swap, sub)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI32
    integer,         intent(out)            :: ierr
    integer,         intent(in)             :: u
    character(len=*),intent(out)            :: V(*)
    integer,         intent(in)             :: n
    logical,         intent(in),   optional :: swap
    logical,         intent(inout),optional :: sub
    integer j, m, ns
    integer(KIND=KISEP) :: iseph, isepf
    integer(KIND=KIOFS) :: jpos
    ierr = err_default
    j = 0
    m = n
    do
       if (ierr.eq.0) then
          call sus_read_isep(ierr, u, iseph, swap=swap)
          if (is_eof_ss(ierr)) then
             ierr = ERR_EOF
             exit
          endif
       endif
       if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
       if (ierr.eq.0) then
          jpos = jpos + conv_b2strm(abs(iseph))
          ns = min(m, get_mems_bytes(abs(iseph), V(1)))
          m = m - ns
       endif
       if (ierr.eq.0) call sus_read(ierr, u, V(j+1:j+ns), ns, swap)
       if (ierr.eq.0) call sus_read_isep(ierr, u, isepf, pos=jpos, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit
          if (abs(iseph).ne.abs(isepf)) ierr = ERR_INCONSISTENT_RECORD_MARKERS
       endif
       if (ierr.ne.0) then
          ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
          exit
       endif
       if (m.eq.0) exit
       j = j + ns
    enddo
    ! write (*, *) 'irec/a', ierr, m, iseph, isepf
    if (ierr.eq.0) then
       if (m.gt.0) then
          ierr = ERR_INVALID_RECORD_SIZE
       else if (choice(.false., sub)) then
          ! sub exist and T
          sub = (iseph.lt.0)    ! subrecord succeeds
       else if (iseph.lt.0) then
          ! skip all
          call sus_skip_irec(ierr, u, 1, swap=swap)
       endif
    endif
    ! write (*, *) 'irec/a/final', ierr
    return
  end subroutine sus_read_irec_a

!!!_  - sus_read_lrec - read a record with 64bit-marker
  subroutine sus_read_lrec_i &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_env,only: conv_b2strm, get_mems_bytes, is_eof_ss
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KI32
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u
    integer(KIND=KARG),intent(out)         :: V(*)
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    integer m
    integer(KIND=KISEP) :: lseph, lsepf
    integer(KIND=KIOFS) :: jpos
    ierr = err_default
    if (ierr.eq.0) then
       call sus_read_lsep(ierr, u, lseph, swap=swap)
       if (is_eof_ss(ierr)) then
          ierr = ERR_EOF
          return
       endif
    endif
    if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
    if (ierr.eq.0) then
       jpos = jpos + conv_b2strm(abs(lseph))
       ! to do: check overflow
       m = int(get_mems_bytes(lseph, V(1)))
       if (n.gt.m) then
          ierr = ERR_INVALID_RECORD_SIZE
       else
          m = min(m, n)
          call sus_read(ierr, u, V, m, swap)
       endif
    endif
    if (ierr.eq.0) call sus_read_lsep(ierr, u, lsepf, pos=jpos, swap=swap)
    if (ierr.eq.0) then
       if (lseph.ne.lsepf) ierr = ERR_INCONSISTENT_RECORD_MARKERS
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
    integer(KIND=KARG),intent(out)         :: V(*)
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    integer m
    integer(KIND=KISEP) :: lseph, lsepf
    integer(KIND=KIOFS) :: jpos
    ierr = err_default
    if (ierr.eq.0) then
       call sus_read_lsep(ierr, u, lseph, swap=swap)
       if (is_eof_ss(ierr)) then
          ierr = ERR_EOF
          return
       endif
    endif
    if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
    if (ierr.eq.0) then
       jpos = jpos + conv_b2strm(abs(lseph))
       ! to do: check overflow
       m = int(get_mems_bytes(lseph, V(1)))
       if (n.gt.m) then
          ierr = ERR_INVALID_RECORD_SIZE
       else
          m = min(m, n)
          call sus_read(ierr, u, V, m, swap)
       endif
    endif
    if (ierr.eq.0) call sus_read_lsep(ierr, u, lsepf, pos=jpos, swap=swap)
    if (ierr.eq.0) then
       if (lseph.ne.lsepf) ierr = ERR_INCONSISTENT_RECORD_MARKERS
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
    real(KIND=KARG),intent(out)         :: V(*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    integer m
    integer(KIND=KISEP) :: lseph, lsepf
    integer(KIND=KIOFS) :: jpos
    ierr = err_default
    if (ierr.eq.0) then
       call sus_read_lsep(ierr, u, lseph, swap=swap)
       if (is_eof_ss(ierr)) then
          ierr = ERR_EOF
          return
       endif
    endif
    if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
    if (ierr.eq.0) then
       jpos = jpos + conv_b2strm(abs(lseph))
       ! to do: check overflow
       m = int(get_mems_bytes(lseph, V(1)))
       if (n.gt.m) then
          ierr = ERR_INVALID_RECORD_SIZE
       else
          m = min(m, n)
          call sus_read(ierr, u, V, m, swap)
       endif
    endif
    if (ierr.eq.0) call sus_read_lsep(ierr, u, lsepf, pos=jpos, swap=swap)
    if (ierr.eq.0) then
       if (lseph.ne.lsepf) ierr = ERR_INCONSISTENT_RECORD_MARKERS
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
    real(KIND=KARG),intent(out)         :: V(*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    integer m
    integer(KIND=KISEP) :: lseph, lsepf
    integer(KIND=KIOFS) :: jpos
    ierr = err_default
    if (ierr.eq.0) then
       call sus_read_lsep(ierr, u, lseph, swap=swap)
       if (is_eof_ss(ierr)) then
          ierr = ERR_EOF
          return
       endif
    endif
    if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
    if (ierr.eq.0) then
       jpos = jpos + conv_b2strm(abs(lseph))
       ! to do: check overflow
       m = int(get_mems_bytes(lseph, V(1)))
       if (n.gt.m) then
          ierr = ERR_INVALID_RECORD_SIZE
       else
          m = min(m, n)
          call sus_read(ierr, u, V, m, swap)
       endif
    endif
    if (ierr.eq.0) call sus_read_lsep(ierr, u, lsepf, pos=jpos, swap=swap)
    if (ierr.eq.0) then
       if (lseph.ne.lsepf) ierr = ERR_INCONSISTENT_RECORD_MARKERS
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
    character(len=*),intent(out)         :: V(*)
    integer,         intent(in)          :: n
    logical,         intent(in),optional :: swap
    integer m
    integer(KIND=KISEP) :: lseph, lsepf
    integer(KIND=KIOFS) :: jpos
    ierr = err_default
    if (ierr.eq.0) then
       call sus_read_lsep(ierr, u, lseph, swap=swap)
       if (is_eof_ss(ierr)) then
          ierr = ERR_EOF
          return
       endif
    endif
    if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
    if (ierr.eq.0) then
       jpos = jpos + conv_b2strm(abs(lseph))
       ! to do: check overflow
       m = int(get_mems_bytes(lseph, V(1)))
       if (n.gt.m) then
          ierr = ERR_INVALID_RECORD_SIZE
       else
          m = min(m, n)
          call sus_read(ierr, u, V, m, swap)
       endif
    endif
    if (ierr.eq.0) call sus_read_lsep(ierr, u, lsepf, pos=jpos, swap=swap)
    if (ierr.eq.0) then
       if (lseph.ne.lsepf) ierr = ERR_INCONSISTENT_RECORD_MARKERS
    else
       ierr = transf_iostat(ierr, ERR_BROKEN_RECORD, __LINE__)
    endif
    return
  end subroutine sus_read_lrec_a
!!!_  - sus_write - write data with optional byte-swapping
  subroutine sus_write_i &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u
    integer(KIND=KARG),intent(in)          :: V(*)
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    if (choice(.false.,swap)) then
       write(UNIT=u, IOSTAT=ierr) sus_eswap(V(1:n))
    else
       write(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
  end subroutine sus_write_i

  subroutine sus_write_l &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KI64
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u
    integer(KIND=KARG),intent(in)          :: V(*)
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    if (choice(.false.,swap)) then
       write(UNIT=u, IOSTAT=ierr) sus_eswap(V(1:n))
    else
       write(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
  end subroutine sus_write_l
  subroutine sus_write_f &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KFLT, KBUF=KI32
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u
    real(KIND=KARG),intent(in)          :: V(*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    if (choice(.false.,swap)) then
       write(UNIT=u, IOSTAT=ierr) sus_eswap(TRANSFER(V(1:n), 0_KBUF, n))
    else
       write(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
  end subroutine sus_write_f
  subroutine sus_write_d &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KDBL, KBUF=KI64
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u
    real(KIND=KARG),intent(in)          :: V(*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    if (choice(.false.,swap)) then
       write(UNIT=u, IOSTAT=ierr) sus_eswap(TRANSFER(V(1:n), 0_KBUF, n))
    else
       write(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
  end subroutine sus_write_d
  subroutine sus_write_a &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: u
    character(len=*),intent(in)          :: V(*)
    integer,         intent(in)          :: n
    logical,         intent(in),optional :: swap
    write(UNIT=u, IOSTAT=ierr) V(1:n)
  end subroutine sus_write_a
!!!_  - sus_read - read data with optional byte-swapping
  subroutine sus_read_i &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u
    integer(KIND=KARG),intent(out)         :: V(*)
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    if (choice(.false.,swap)) then
       read(UNIT=u, IOSTAT=ierr) V(1:n)
       if (ierr.eq.0) V(1:n) = sus_eswap(V(1:n))
    else
       read(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
  end subroutine sus_read_i
  subroutine sus_read_l &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KI64
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u
    integer(KIND=KARG),intent(out)         :: V(*)
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    if (choice(.false.,swap)) then
       read(UNIT=u, IOSTAT=ierr) V(1:n)
       if (ierr.eq.0) V(1:n) = sus_eswap(V(1:n))
    else
       read(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
  end subroutine sus_read_l
  subroutine sus_read_f &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KFLT, KBUF=KI32
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u
    real(KIND=KARG),intent(out)         :: V(*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
#if OPT_READ_SWAP_WITH_WORK
    integer(KIND=KBUF) :: W(n)
#endif /* OPT_READ_SWAP_WITH_WORK */
    if (choice(.false.,swap)) then
#if OPT_READ_SWAP_WITH_WORK
       read(UNIT=u, IOSTAT=ierr) W(1:n)
       if (ierr.eq.0) then
          V(1:n) = TRANSFER(sus_eswap(W(1:n)), 0.0_KARG, n)
       endif
#else /* not OPT_READ_SWAP_WITH_WORK */
       read(UNIT=u, IOSTAT=ierr) V(1:n)
       if (ierr.eq.0) then
          V(1:n) = TRANSFER(sus_eswap(TRANSFER(V(1:n), 0_KBUF, n)), 0.0_KARG, n)
       endif
#endif /* not OPT_READ_SWAP_WITH_WORK */
    else
       read(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
  end subroutine sus_read_f
  subroutine sus_read_d &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KDBL, KBUF=KI64
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u
    real(KIND=KARG),intent(out)         :: V(*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
#if OPT_READ_SWAP_WITH_WORK
    integer(kind=KBUF) :: W(n)
#endif /* OPT_READ_SWAP_WITH_WORK */
    if (choice(.false.,swap)) then
#if OPT_READ_SWAP_WITH_WORK
       read(UNIT=u, IOSTAT=ierr) W(1:n)
       if (ierr.eq.0) then
          V(1:n) = TRANSFER(sus_eswap(W(1:n)), 0.0_KARG, n)
       endif
#else /* not OPT_READ_SWAP_WITH_WORK */
       read(UNIT=u, IOSTAT=ierr) V(1:n)
       if (ierr.eq.0) then
          V(1:n) = TRANSFER(sus_eswap(TRANSFER(V(1:n), 0_KBUF, n)), 0.0_KARG, n)
       endif
#endif /* not OPT_READ_SWAP_WITH_WORK */
    else
       read(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
  end subroutine sus_read_d
  subroutine sus_read_a &
       & (ierr, u, v, n, swap)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: u
    character(len=*),intent(out)         :: V(*)
    integer,         intent(in)          :: n
    logical,         intent(in),optional :: swap
    read(UNIT=u, IOSTAT=ierr) V(1:n)
  end subroutine sus_read_a
!!!_  - sus_write_isep - write 32-bit separator
  subroutine sus_write_isep_i (ierr, u, sep, pos, swap, sub)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(in)          :: sep
    integer(KIND=KIOFS),intent(in),optional :: pos
    logical,            intent(in),optional :: swap
    logical,            intent(in),optional :: sub
    integer(KIND=KISEP) :: isep

    isep = sep
    if (choice(.false., sub)) isep = - isep
    if (choice(.false., swap)) isep = sus_eswap(isep)
    if (present(pos)) then
       write(UNIT=u, IOSTAT=ierr, POS=pos) isep
    else
       write(UNIT=u, IOSTAT=ierr) isep
    endif
  end subroutine sus_write_isep_i
  subroutine sus_write_isep_l (ierr, u, sep, pos, swap, sub)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI64
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(in)          :: sep
    integer(KIND=KIOFS),intent(in),optional :: pos
    logical,            intent(in),optional :: swap
    logical,            intent(in),optional :: sub
    integer(KIND=KISEP) :: isep

    ! todo: check overflow
    isep = int(sep, KIND=KISEP)
    if (choice(.false., sub)) isep = - isep
    if (choice(.false., swap)) isep = sus_eswap(isep)
    if (present(pos)) then
       write(UNIT=u, IOSTAT=ierr, POS=pos) isep
    else
       write(UNIT=u, IOSTAT=ierr) isep
    endif
  end subroutine sus_write_isep_l
!!!_  - sus_write_lsep - write 64-bit separator
  subroutine sus_write_lsep_i (ierr, u, sep, pos, swap)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(in)          :: sep
    integer(KIND=KIOFS),intent(in),optional :: pos
    logical,            intent(in),optional :: swap
    integer(KIND=KISEP) :: lsep

    if (choice(.false., swap)) then
       lsep = sus_eswap(sep)
    else
       lsep = sep
    endif
    if (present(pos)) then
       write(UNIT=u, IOSTAT=ierr, POS=pos) lsep
    else
       write(UNIT=u, IOSTAT=ierr) lsep
    endif
  end subroutine sus_write_lsep_i
  subroutine sus_write_lsep_l (ierr, u, sep, pos, swap)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KI64
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(in)          :: sep
    integer(KIND=KIOFS),intent(in),optional :: pos
    logical,            intent(in),optional :: swap
    integer(KIND=KISEP) :: lsep

    if (choice(.false., swap)) then
       lsep = sus_eswap(sep)
    else
       lsep = sep
    endif
    if (present(pos)) then
       write(UNIT=u, IOSTAT=ierr, POS=pos) lsep
    else
       write(UNIT=u, IOSTAT=ierr) lsep
    endif
  end subroutine sus_write_lsep_l

!!!_  - sus_read_isep - read 32-bit separator
  subroutine sus_read_isep_i (ierr, u, sep, pos, swap)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(out)         :: sep
    integer(KIND=KIOFS),intent(in),optional :: pos
    logical,            intent(in),optional :: swap

    if (present(pos)) then
       read(UNIT=u, IOSTAT=ierr, POS=pos) sep
    else
       read(UNIT=u, IOSTAT=ierr) sep
    endif
    if (choice(.false., swap)) then
       if (ierr.eq.0) sep = sus_eswap(sep)
    endif
  end subroutine sus_read_isep_i
  subroutine sus_read_isep_l (ierr, u, sep, pos, swap)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI32, KARG=KI64
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(out)         :: sep
    integer(KIND=KIOFS),intent(in),optional :: pos
    logical,            intent(in),optional :: swap
    integer(KIND=KISEP) :: isep
    ! todo: check overflow
    if (present(pos)) then
       read(UNIT=u, IOSTAT=ierr, POS=pos) isep
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
  end subroutine sus_read_isep_l
!!!_  - sus_read_lsep - read 64-bit separator
  subroutine sus_read_lsep_i (ierr, u, sep, pos, swap)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(out)         :: sep
    integer(KIND=KIOFS),intent(in),optional :: pos
    logical,            intent(in),optional :: swap
    integer(KIND=KISEP) :: lsep

    if (present(pos)) then
       read(UNIT=u, IOSTAT=ierr, POS=pos) lsep
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
  end subroutine sus_read_lsep_i
  subroutine sus_read_lsep_l (ierr, u, sep, pos, swap)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KI64
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(out)         :: sep
    integer(KIND=KIOFS),intent(in),optional :: pos
    logical,            intent(in),optional :: swap
    if (present(pos)) then
       read(UNIT=u, IOSTAT=ierr, POS=pos) sep
    else
       read(UNIT=u, IOSTAT=ierr) sep
    endif
    if (choice(.false., swap)) then
       if (ierr.eq.0) sep = sus_eswap(sep)
    endif
  end subroutine sus_read_lsep_l

!!!_  - sus_eswap() - elemental
  ELEMENTAL integer(KIND=KI32) function sus_eswap_i(V) &
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

  ELEMENTAL integer(KIND=KI64) function sus_eswap_l(V) &
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

!!!_  - sus_eswap_hl() - elemental (higher/lower bits independent swap)
  ELEMENTAL integer(KIND=KI64) function sus_eswap_hl(V) &
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

!!!_  - sus_swap() - swap expanded
  elemental integer(KIND=KI32) function sus_swap_i(V) &
       & result(R)
    implicit none
    integer(kind=KI32),intent(in) :: V
    R = IOR(IOR(ISHFT(IBITS(V, 0*LBU, LBU), 3*LBU),  &
         &      ISHFT(IBITS(V, 1*LBU, LBU), 2*LBU)), &
         &  IOR(ISHFT(IBITS(V, 2*LBU, LBU), 1*LBU),  &
         &            IBITS(V, 3*LBU, LBU)))
    return
  end function sus_swap_i

  elemental integer(KIND=KI64) function sus_swap_l(V) &
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

!!!_  - sus_rseek - seek position to read
  subroutine sus_rseek &
       & (ierr, u, step, whence)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KIOFS),intent(in),optional :: step
    integer,            intent(in),optional :: whence

    integer(KIND=KIOFS) :: jpos, st
    integer wh

    ierr = 0

    wh = choice(WHENCE_CURRENT, whence)
    st = choice(0_KIOFS, step)

    if (wh.eq.WHENCE_END) then
       inquire(UNIT=u, IOSTAT=ierr, SIZE=jpos)
       if (ierr.eq.0) jpos = jpos + 1_KIOFS + st
    else if (wh.eq.WHENCE_BEGIN) then
       jpos = 1_KIOFS + st
    else if (wh.eq.WHENCE_ABS) then
       jpos = st
    else if (st.eq.0) then
       return
    else
       inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
       if (ierr.eq.0) jpos = jpos + st
    endif

    if (ierr.eq.0) then
#if OPT_STREAM_RPOS_WORKAROUND
       call sus_rseek_workaround(ierr, u, jpos)
#else /* not OPT_STREAM_RPOS_WORKAROUND */
       read(UNIT=u, IOSTAT=ierr, POS=jpos)
#endif /* not OPT_STREAM_RPOS_WORKAROUND */
    endif
    return
  end subroutine sus_rseek

!!!_  - sus_rseek_workaround - seek position to read (workaround)
  subroutine sus_rseek_workaround &
       & (ierr, u, jpos)
    !! caution: T assumed to be 1-byte
    use TOUZA_Std_env,only: nc_strm
    implicit none
    integer,            intent(out) :: ierr
    integer,            intent(in)  :: u
    integer(KIND=KIOFS),intent(in)  :: jpos
    character T
    if (jpos.le.1_KIOFS) then
       rewind(UNIT=u, IOSTAT=ierr)
    else
       read(u, IOSTAT=ierr, POS=jpos-nc_strm) T
    endif
    return
  end subroutine sus_rseek_workaround

!!!_ + private subroutines
!!!_  - mstrm_sep ()
  PURE &
  integer function mstrm_sep_i (vhld) result(m)
    implicit none
    integer,parameter :: KISEP=KI32
    integer(kind=KISEP),intent(in) :: vhld
    m = mstrm_isep
  end function mstrm_sep_i
  PURE &
  integer function mstrm_sep_l (vhld) result(m)
    implicit none
    integer,parameter :: KISEP=KI64
    integer(kind=KISEP),intent(in) :: vhld
    m = mstrm_lsep
  end function mstrm_sep_l

!!!_  - max_members ()
  PURE &
  integer function max_members_a (t) result(m)
    implicit none
    character(len=*),intent(in) :: t
    m = lsubr / max(1, len(t))
  end function max_members_a

  PURE &
  integer function max_members_i (vhld) result(m)
    implicit none
    integer,parameter :: KARG=KI32
    integer(kind=KARG),intent(in) :: vhld
    m = maxmemi_i
  end function max_members_i
  PURE &
  integer function max_members_l (vhld) result(m)
    implicit none
    integer,parameter :: KARG=KI64
    integer(kind=KARG),intent(in) :: vhld
    m = maxmemi_l
  end function max_members_l
  PURE &
  integer function max_members_f (vhld) result(m)
    implicit none
    integer,parameter :: KARG=KFLT
    real(kind=KARG),intent(in) :: vhld
    m = maxmemi_f
  end function max_members_f
  PURE &
  integer function max_members_d (vhld) result(m)
    implicit none
    integer,parameter :: KARG=KDBL
    real(kind=KARG),intent(in) :: vhld
    m = maxmemi_d
  end function max_members_d

!!!_  - transf_iostat
  integer function transf_iostat &
       & (istat, ierr, line) &
       & result (n)
    implicit none
    integer,intent(in) :: istat
    integer,intent(in) :: ierr
    integer,intent(in) :: line
    last_iostat = istat
    n = ierr
    return
  end function transf_iostat

end module TOUZA_Std_sus

!!!_@ test_std_sus - test program
#ifdef TEST_STD_SUS
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

  integer kendi
  integer j
  integer mi, ml, mf, md, ma

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
end program test_std_sus

#endif /* TEST_STD_SUS */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

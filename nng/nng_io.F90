!!!_! nng_io.F90 - TOUZA/Nng unformatted sequential access emulator
! Maintainer: SAITO Fuyuki
! Created: Oct 17 2021
#define TIME_STAMP 'Time-stamp: <2021/12/05 22:14:18 fuyuki nng_io.F90>'
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
#ifndef    OPT_INTEGER_OFFSET_KIND
#  define  OPT_INTEGER_OFFSET_KIND 0
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
!!!_@ TOUZA_Nng_io - Nng stream interfaces
module TOUZA_Nng_io
!!!_ = declaration
#if OPT_USE_MPI
  use mpi,only: MPI_OFFSET_KIND
#endif /* OPT_USE_MPI */
  use TOUZA_Nng_std,only: &
       & KI32, KI64, KFLT, KDBL, &
       & LBU=>nbits_byte, &
       & control_mode, control_deep, is_first_force, &
       & unit_global,  trace_fine,   trace_control,  &
       & get_logu,     kendi_file,   kendi_mem
  implicit none
  private
!!!_  - public parameters
#if OPT_INTEGER_OFFSET_KIND
#else /* not OPT_INTEGER_OFFSET_KIND */
#  undef OPT_INTEGER_OFFSET_KIND
#  if OPT_USE_MPI
#    define OPT_INTEGER_OFFSET_KIND MPI_OFFSET_KIND
#  else
#    define OPT_INTEGER_OFFSET_KIND KI32
#  endif
#endif /* not OPT_INTEGER_OFFSET_KIND */
  integer,parameter,public :: KIOFS = OPT_INTEGER_OFFSET_KIND

  integer,parameter,public :: WHENCE_BEGIN = -1
  integer,parameter,public :: WHENCE_CURRENT = 0
  integer,parameter,public :: WHENCE_END = +1
  integer,parameter,public :: WHENCE_ABS = -99

  integer,parameter,public :: RECL_MAX_BYTES = HUGE(0_KI32) - 4 * 2
  ! integer,parameter,public :: RECL_MAX_BYTES = 24

  integer,parameter,public :: BODR_ASSUME_SYSTEM  = 0    ! assume file byte-order == system
  integer,parameter,public :: BODR_ASSUME_FILE    = 1    ! assume file byte-order == common
  integer,parameter,public :: BODR_CHECK_VERBOSE  = 2    ! check for each unit at open-write

!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = NNG_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
#define __MDL__ 'i'

  integer,save :: mstrm_isep = 0, mstrm_lsep = 0   !! separator sizes in stream unit

  integer,save :: maxmemi_i = 0, maxmemi_l = 0 !! max members in a sub-record (32-bit marker)
  integer,save :: maxmemi_f = 0, maxmemi_d = 0

  integer,save :: bodr_wnative = BODR_ASSUME_SYSTEM

  integer(kind=KI32),save :: lsubr = 0
!!!_  - interfaces
  interface ssq_write_irec
     module procedure ssq_write_irec_i
     module procedure ssq_write_irec_l
     module procedure ssq_write_irec_d
     module procedure ssq_write_irec_f
     module procedure ssq_write_irec_a
  end interface ssq_write_irec
  interface ssq_write_lrec
     module procedure ssq_write_lrec_i
     module procedure ssq_write_lrec_l
     module procedure ssq_write_lrec_d
     module procedure ssq_write_lrec_f
     module procedure ssq_write_lrec_a
  end interface ssq_write_lrec

  interface ssq_read_irec
     module procedure ssq_read_irec_i
     module procedure ssq_read_irec_l
     module procedure ssq_read_irec_d
     module procedure ssq_read_irec_f
     module procedure ssq_read_irec_a
  end interface ssq_read_irec
  interface ssq_read_lrec
     module procedure ssq_read_lrec_i
     module procedure ssq_read_lrec_l
     module procedure ssq_read_lrec_d
     module procedure ssq_read_lrec_f
     module procedure ssq_read_lrec_a
  end interface ssq_read_lrec

  interface ssq_write_isep
     module procedure ssq_write_isep_l
     module procedure ssq_write_isep_i
  end interface ssq_write_isep
  interface ssq_write_lsep
     module procedure ssq_write_lsep_l
     module procedure ssq_write_lsep_i
  end interface ssq_write_lsep

  interface ssq_read_isep
     module procedure ssq_read_isep_l
     module procedure ssq_read_isep_i
  end interface ssq_read_isep
  interface ssq_read_lsep
     module procedure ssq_read_lsep_l
     module procedure ssq_read_lsep_i
  end interface ssq_read_lsep

  interface ssq_write
     module procedure ssq_write_i
     module procedure ssq_write_l
     module procedure ssq_write_d
     module procedure ssq_write_f
     ! module procedure ssq_write_a
  end interface ssq_write

  interface ssq_read
     module procedure ssq_read_i
     module procedure ssq_read_l
     module procedure ssq_read_d
     module procedure ssq_read_f
     ! module procedure ssq_read_a
  end interface ssq_read

  interface ssq_eswap
     module procedure ssq_eswap_i
     module procedure ssq_eswap_l
  end interface ssq_eswap

  interface ssq_swap
     module procedure ssq_swap_i
     module procedure ssq_swap_l
  end interface ssq_swap

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
  public ssq_open, ssq_close

  public ssq_write_irec,  ssq_read_irec,  ssq_skip_irec
  public ssq_write_lrec,  ssq_read_lrec,  ssq_skip_lrec

  ! public ssq_write_begin_irec, ssq_write_end_irec
  ! public ssq_read_begin_irec,  ssq_read_end_irec

  public ssq_write_isep,  ssq_read_isep
  public ssq_write_lsep,  ssq_read_lsep

  public ssq_rseek
  public ssq_eswap

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, bodr)
    use TOUZA_Nng_std,only: KI32, KI64, KFLT, KDBL, &
         & choice, get_size_strm, get_size_bytes, &
         & ns_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    integer,intent(in),optional :: bodr
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
       endif
       if (is_first_force(init_counts, md)) then
          ! if (ierr.eq.0) call ssq_check_kinds_literal(ierr, ulog)
          if (ierr.eq.0) call init_batch(ierr, bodr, ulog, lv)
          if (ierr.eq.0) call ssq_check_stream_pos(ierr)
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
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
       init_counts = init_counts + 1
    endif

    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Nng_std,only: choice, msg, ns_diag=>diag, is_msglev_normal
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
             if (is_msglev_normal(lv)) call msg('(''offset kind = '', I0)', KIOFS, __MDL__, utmp)
             if (is_msglev_normal(lv)) call msg('(''byte-order assumption = '', I0)', bodr_wnative, __MDL__, utmp)
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ns_diag(ierr, utmp, levv=lv, mode=md)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Nng_std,only: choice, ns_finalize=>finalize
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
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_  - init subcontracts
!!!_   & init_batch
  subroutine init_batch(ierr, bodr, u, levv)
    use TOUZA_Nng_std,only: choice, msg, is_msglev_info, is_msglev_fatal
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: bodr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    ierr = 0
    bodr_wnative = choice(bodr_wnative, bodr)
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
  end subroutine init_batch
!!!_  - diag subcontracts
!!!_   & ssq_check_stream_pos - health_check
  subroutine ssq_check_stream_pos &
       & (ierr, utest, ulog)
    use TOUZA_Nng_std,only: choice, msg, new_unit
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: utest
    integer,intent(in),optional :: ulog
    integer ut, ul
    integer jposa, jposb

    ierr = 0
    ut = choice(-1, utest)
    if (ut.lt.0) then
       if (ut.lt.0) ut = new_unit()
       if (ut.lt.0) ierr = -1
    endif
    if (ierr.eq.0) then
       open(UNIT=ut, IOSTAT=ierr, &
            &       ACCESS='STREAM', FORM='UNFORMATTED', STATUS='SCRATCH', ACTION='READWRITE')
    endif
    if (ierr.eq.0) write(ut, IOSTAT=ierr) 'TEST'
    if (ierr.eq.0) inquire(ut, IOSTAT=ierr, POS=jposa)
    if (ierr.eq.0) write(ut, IOSTAT=ierr, POS=1)
    if (ierr.eq.0) inquire(ut, IOSTAT=ierr, POS=jposb)
    if (ierr.eq.0) then
       if (jposa.eq.jposb) then
          call msg('(''positioning/write not works '', I0, 1x, I0)', &
               & (/jposa, jposb/), __GRP__, ulog)
          ierr = -1
       endif
    endif
    if (ierr.eq.0) then
       if (ierr.eq.0) write(ut, IOSTAT=ierr, POS=jposa)
       if (ierr.eq.0) read(ut, IOSTAT=ierr, POS=1)
       if (ierr.eq.0) inquire(ut, IOSTAT=ierr, POS=jposb)
    endif
    if (ierr.eq.0) then
       if (jposa.eq.jposb) then
          call msg('(''positioning/read not works '', I0, 1x, I0)', &
               & (/jposa, jposb/), __GRP__, ulog)
#if OPT_STREAM_RPOS_WORKAROUND
          call msg('workaround enabled', __GRP__, ulog)
#else /* not OPT_STREAM_RPOS_WORKAROUND */
          call msg('need rebuild with workaround', __GRP__, ulog)
          ierr = -1
#endif /* not OPT_STREAM_RPOS_WORKAROUND */
       endif
    endif
    if (ierr.eq.0) then
       close(UNIT=ut, IOSTAT=ierr, STATUS='DELETE')
    endif
    if (ierr.ne.0) then
       call msg('(''failed: '', I0)', ierr, __GRP__, ulog)
    endif
  end subroutine ssq_check_stream_pos

!!!_   & ssq_check_kinds_literal - health_check
  subroutine ssq_check_kinds_literal &
       & (ierr, u)
    use TOUZA_Nng_std,only: choice, KI32, KI64, KBUF=>KI32
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer utmp
    integer jposa, jposb

201 format('kind:', A, ' = ', I0)
    utmp = get_logu(u, ulog)
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
  end subroutine ssq_check_kinds_literal

!!!_ + user subroutines
!!!_  - ssq_open
  subroutine ssq_open &
       & (ierr, u,      file, &
       &  form, status, action, kendi)
    use TOUZA_Nng_std,only: choice_a, kendi_file, kendi_mem, check_bodr_files
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: u
    character(len=*),intent(in)  :: file
    character(len=*),intent(in), optional :: form
    character(len=*),intent(in), optional :: status
    character(len=*),intent(in), optional :: action
    integer,         intent(out),optional :: kendi

    character(len=16) :: STT, ACT, FRM

    ierr = ERR_SUCCESS

    call choice_a(STT, 'U', status)
    call choice_a(ACT, 'R', action)
    call choice_a(FRM, 'U', form)

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
       ACT = 'WRITE'
    endif

    if (FRM(1:1).eq.'U'.or.FRM.eq.' ') then
       FRM='UNFORMATTED'
    else if (FORM(1:1).eq.'F') then
       FRM='FORMATTED'
    endif

    if (present(kendi) .and. ACT.ne.'READ') then
       select case(bodr_wnative)
       case (BODR_ASSUME_SYSTEM)
          kendi = kendi_mem
       case (BODR_ASSUME_FILE)
          kendi = kendi_file
       case (BODR_CHECK_VERBOSE)
          call check_bodr_files (ierr, kendi, ubgn=u)
       case default
          kendi = kendi_mem
       end select
    endif
    if (ierr.eq.0) then
       open(UNIT=u, IOSTAT=ierr, &
            &       FILE=file,   ACCESS='STREAM', FORM=FRM, STATUS=STT, ACTION=ACT)
    endif
  end subroutine ssq_open
!!!_  - ssq_close
  subroutine ssq_close(ierr, u, file)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: u
    character(len=*),intent(in)  :: file
    ierr = ERR_SUCCESS
    close(UNIT=u, IOSTAT=ierr)
  end subroutine ssq_close

!!!_  - ssq_skip_irec - forward/backward 32-bit marker records
  subroutine ssq_skip_irec &
       & (ierr, u, n, whence, swap)
    use TOUZA_Nng_std,only: conv_b2strm
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

    if (ierr.eq.0) call ssq_rseek(ierr, u, whence=whence)

    if (.not.present(n)) return

    if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
    if (n.gt.0) then
       j = 0
       do
          if (ierr.eq.0) call ssq_read_isep(ierr, u, iseph, swap=swap)
          if (ierr.eq.0) jpos = jpos + conv_b2strm(abs(iseph)) + mstrm_sep(iseph)
          if (ierr.eq.0) call ssq_read_isep(ierr, u, isepf, pos=jpos, swap=swap)
          if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
          if (ierr.eq.0) then
             if (abs(iseph).ne.abs(isepf)) ierr = ERR_INCONSISTENT_RECORD_MARKERS
          endif
          if (ierr.ne.0) exit
          if (iseph.ge.0) j = j + 1
          if (j.eq.n) exit
       enddo
    else if (n.lt.0) then
       j = 0
       do
          if (ierr.eq.0) jpos = jpos - mstrm_sep(isepf)
          if (ierr.eq.0) call ssq_read_isep(ierr, u, isepf, pos=jpos, swap=swap)
          if (ierr.eq.0) jpos = jpos - conv_b2strm(abs(isepf)) - mstrm_sep(iseph)
          if (ierr.eq.0) call ssq_read_isep(ierr, u, iseph, pos=jpos, swap=swap)
          if (ierr.eq.0) then
             if (abs(iseph).ne.abs(isepf)) ierr = ERR_INCONSISTENT_RECORD_MARKERS
          endif
          if (ierr.ne.0) exit
          if (isepf.ge.0) j = j + 1
          if (j.eq.-n) exit
       enddo
#if OPT_STREAM_RPOS_WORKAROUND
       if (ierr.eq.0) call ssq_rseek_workaround(ierr, u, jpos)
#else /* not OPT_STREAM_RPOS_WORKAROUND */
       if (ierr.eq.0) read(UNIT=u, IOSTAT=ierr, POS=jpos)
#endif /* not OPT_STREAM_RPOS_WORKAROUND */
    endif
  end subroutine ssq_skip_irec

!!!_  - ssq_skip_lrec - forward/backward 64-bit marker records
  subroutine ssq_skip_lrec &
       & (ierr, u, n, whence, swap)
    use TOUZA_Nng_std,only: conv_b2strm
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

    if (ierr.eq.0) call ssq_rseek(ierr, u, whence=whence)

    if (.not.present(n)) return

    inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
    if (n.gt.0) then
       do j = 1, n
          if (ierr.eq.0) call ssq_read_lsep(ierr, u, lseph, swap=swap)
          if (ierr.eq.0) jpos = jpos + conv_b2strm(lseph) + mstrm_sep(lseph)
          if (ierr.eq.0) call ssq_read_lsep(ierr, u, lsepf, pos=jpos, swap=swap)
          if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
          if (ierr.eq.0) then
             if (lseph.ne.lsepf) ierr = ERR_INCONSISTENT_RECORD_MARKERS
          endif
       enddo
    else if (n.lt.0) then
       do j = 1, -n
          if (ierr.eq.0) jpos = jpos - mstrm_sep(lsepf)
          if (ierr.eq.0) call ssq_read_lsep(ierr, u, lsepf, pos=jpos, swap=swap)
          if (ierr.eq.0) jpos = jpos - conv_b2strm(lsepf) - mstrm_sep(lseph)
          if (ierr.eq.0) call ssq_read_lsep(ierr, u, lseph, pos=jpos, swap=swap)
          if (ierr.eq.0) then
             if (lseph.ne.lsepf) ierr = ERR_INCONSISTENT_RECORD_MARKERS
          endif
       enddo
#if OPT_STREAM_RPOS_WORKAROUND
       if (ierr.eq.0) call ssq_rseek_workaround(ierr, u, jpos)
#else /* not OPT_STREAM_RPOS_WORKAROUND */
       if (ierr.eq.0) read(UNIT=u, IOSTAT=ierr, POS=jpos)
#endif /* not OPT_STREAM_RPOS_WORKAROUND */
    endif
  end subroutine ssq_skip_lrec

!!!_  - ssq_write_irec - write a record with 32bit-marker
  subroutine ssq_write_irec_i &
       & (ierr, u, v, n, swap, pre, post)
    use TOUZA_Nng_std,only: get_size_bytes
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
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) call ssq_write(ierr, u, V, n, swap)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=pre)
    else
       m = n
       j = 0
       isep = get_size_bytes(V(1), ns)
       ! first
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       if (ierr.eq.0) call ssq_write(ierr, u, V(j+1:j+ns), ns, swap)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=pre)
       ! write(*, *) 'irec/i/first', ierr, ns, n, m, j
       ! middle
       do
          m = m - ns
          j = j + ns
          ! write(*, *) 'irec/i/middle', ierr, ns, n, m, j
          if (m.le.ns) exit
          if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
          if (ierr.eq.0) call ssq_write(ierr, u, V(j+1:j+ns), ns, swap)
          if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       enddo
       ! write(*, *) 'irec/i/last', ierr, ns, n, m, j
       ! last
       isep = get_size_bytes(V(1), m)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) call ssq_write(ierr, u, V(j+1:j+m), m, swap)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
    endif
    ! stop
    return
  end subroutine ssq_write_irec_i
  subroutine ssq_write_irec_l &
       & (ierr, u, v, n, swap, pre, post)
    use TOUZA_Nng_std,only: get_size_bytes
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
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) call ssq_write(ierr, u, V, n, swap)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=pre)
    else
       m = n
       j = 0
       isep = get_size_bytes(V(1), ns)
       ! first
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       if (ierr.eq.0) call ssq_write(ierr, u, V(j+1:j+ns), ns, swap)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=pre)
       ! middle
       do
          m = m - ns
          j = j + ns
          if (m.le.ns) exit
          if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
          if (ierr.eq.0) call ssq_write(ierr, u, V(j+1:j+ns), ns, swap)
          if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       enddo
       ! last
       isep = get_size_bytes(V(1), m)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) call ssq_write(ierr, u, V(j+1:j+m), m, swap)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
    endif
    return
  end subroutine ssq_write_irec_l
  subroutine ssq_write_irec_f &
       & (ierr, u, v, n, swap, pre, post)
    use TOUZA_Nng_std,only: get_size_bytes
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
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) call ssq_write(ierr, u, V, n, swap)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=pre)
    else
       m = n
       j = 0
       isep = get_size_bytes(V(1), ns)
       ! first
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       if (ierr.eq.0) call ssq_write(ierr, u, V(j+1:j+ns), ns, swap)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=pre)
       ! middle
       do
          m = m - ns
          j = j + ns
          if (m.le.ns) exit
          if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
          if (ierr.eq.0) call ssq_write(ierr, u, V(j+1:j+ns), ns, swap)
          if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       enddo
       ! last
       isep = get_size_bytes(V(1), m)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) call ssq_write(ierr, u, V(j+1:j+m), m, swap)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
    endif
    return
  end subroutine ssq_write_irec_f
  subroutine ssq_write_irec_d &
       & (ierr, u, v, n, swap, pre, post)
    use TOUZA_Nng_std,only: get_size_bytes
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
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) call ssq_write(ierr, u, V, n, swap)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=pre)
    else
       m = n
       j = 0
       isep = get_size_bytes(V(1), ns)
       ! first
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       if (ierr.eq.0) call ssq_write(ierr, u, V(j+1:j+ns), ns, swap)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=pre)
       ! middle
       do
          m = m - ns
          j = j + ns
          if (m.le.ns) exit
          if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
          if (ierr.eq.0) call ssq_write(ierr, u, V(j+1:j+ns), ns, swap)
          if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       enddo
       ! last
       isep = get_size_bytes(V(1), m)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) call ssq_write(ierr, u, V(j+1:j+m), m, swap)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
    endif
    return
  end subroutine ssq_write_irec_d
  subroutine ssq_write_irec_a &
       & (ierr, u, v, n, swap, pre, post)
    use TOUZA_Nng_std,only: get_size_bytes
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
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) write(UNIT=u, IOSTAT=ierr) V(1:n)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=pre)
    else
       m = n
       j = 0
       isep = get_size_bytes(V(1), ns)
       ! first
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       if (ierr.eq.0) write(UNIT=u, IOSTAT=ierr) V(j+1:j+ns)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=pre)
       ! middle
       do
          m = m - ns
          j = j + ns
          if (m.le.ns) exit
          if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
          if (ierr.eq.0) write(UNIT=u, IOSTAT=ierr) V(j+1:j+ns)
          if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
       enddo
       ! last
       isep = get_size_bytes(V(1), m)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=post)
       if (ierr.eq.0) write(UNIT=u, IOSTAT=ierr) V(j+1:j+m)
       if (ierr.eq.0) call ssq_write_isep(ierr, u, isep, swap=swap, sub=.TRUE.)
    endif
    return
  end subroutine ssq_write_irec_a

!!!_  - ssq_write_lrec - write a record with 64bit-marker
  subroutine ssq_write_lrec_i &
       & (ierr, u, v, n, swap)
    use TOUZA_Nng_std,only: get_size_bytes
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
    if (ierr.eq.0) call ssq_write_lsep(ierr, u, lsep, swap=swap)
    if (ierr.eq.0) call ssq_write(ierr, u, V, n, swap)
    if (ierr.eq.0) call ssq_write_lsep(ierr, u, lsep, swap=swap)
    return
  end subroutine ssq_write_lrec_i
  subroutine ssq_write_lrec_l &
       & (ierr, u, v, n, swap)
    use TOUZA_Nng_std,only: get_size_bytes
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
    if (ierr.eq.0) call ssq_write_lsep(ierr, u, lsep, swap=swap)
    if (ierr.eq.0) call ssq_write(ierr, u, V, n, swap)
    if (ierr.eq.0) call ssq_write_lsep(ierr, u, lsep, swap=swap)
    return
  end subroutine ssq_write_lrec_l
  subroutine ssq_write_lrec_f &
       & (ierr, u, v, n, swap)
    use TOUZA_Nng_std,only: get_size_bytes
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
    if (ierr.eq.0) call ssq_write_lsep(ierr, u, lsep, swap=swap)
    if (ierr.eq.0) call ssq_write(ierr, u, V, n, swap)
    if (ierr.eq.0) call ssq_write_lsep(ierr, u, lsep, swap=swap)
    return
  end subroutine ssq_write_lrec_f
  subroutine ssq_write_lrec_d &
       & (ierr, u, v, n, swap)
    use TOUZA_Nng_std,only: get_size_bytes
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
    if (ierr.eq.0) call ssq_write_lsep(ierr, u, lsep, swap=swap)
    if (ierr.eq.0) call ssq_write(ierr, u, V, n, swap)
    if (ierr.eq.0) call ssq_write_lsep(ierr, u, lsep, swap=swap)
    return
  end subroutine ssq_write_lrec_d
  subroutine ssq_write_lrec_a &
       & (ierr, u, v, n, swap)
    use TOUZA_Nng_std,only: get_size_bytes
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
    if (ierr.eq.0) call ssq_write_lsep(ierr, u, lsep, swap=swap)
    if (ierr.eq.0) write(UNIT=u, IOSTAT=ierr) V(1:n)
    if (ierr.eq.0) call ssq_write_lsep(ierr, u, lsep, swap=swap)
    return
  end subroutine ssq_write_lrec_a

!!!_  - ssq_read_irec - read a record with 32bit-marker
  subroutine ssq_read_irec_i &
       & (ierr, u, v, n, swap, sub)
    use TOUZA_Nng_std,only: choice, conv_b2strm, get_mems_bytes
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
       if (ierr.eq.0) call ssq_read_isep(ierr, u, iseph, swap=swap)
       if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
       if (ierr.eq.0) then
          jpos = jpos + conv_b2strm(abs(iseph))
          ns = min(m, get_mems_bytes(abs(iseph), V(1)))
          m = m - ns
       endif
       if (ierr.eq.0) call ssq_read(ierr, u, V(j+1:j+ns), ns, swap)
       if (ierr.eq.0) call ssq_read_isep(ierr, u, isepf, pos=jpos, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit
          if (abs(iseph).ne.abs(isepf)) ierr = ERR_INCONSISTENT_RECORD_MARKERS
       endif
       if (ierr.ne.0) exit
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
          call ssq_skip_irec(ierr, u, 1, swap=swap)
       endif
    endif
    return
  end subroutine ssq_read_irec_i
  subroutine ssq_read_irec_l &
       & (ierr, u, v, n, swap, sub)
    use TOUZA_Nng_std,only: choice, conv_b2strm, get_mems_bytes
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
       if (ierr.eq.0) call ssq_read_isep(ierr, u, iseph, swap=swap)
       if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
       if (ierr.eq.0) then
          jpos = jpos + conv_b2strm(abs(iseph))
          ns = min(m, get_mems_bytes(abs(iseph), V(1)))
          m = m - ns
       endif
       if (ierr.eq.0) call ssq_read(ierr, u, V(j+1:j+ns), ns, swap)
       if (ierr.eq.0) call ssq_read_isep(ierr, u, isepf, pos=jpos, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit
          if (abs(iseph).ne.abs(isepf)) ierr = ERR_INCONSISTENT_RECORD_MARKERS
       endif
       if (ierr.ne.0) exit
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
          call ssq_skip_irec(ierr, u, 1, swap=swap)
       endif
    endif
    return
  end subroutine ssq_read_irec_l
  subroutine ssq_read_irec_f &
       & (ierr, u, v, n, swap, sub)
    use TOUZA_Nng_std,only: choice, conv_b2strm, get_mems_bytes
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
       if (ierr.eq.0) call ssq_read_isep(ierr, u, iseph, swap=swap)
       if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
       if (ierr.eq.0) then
          jpos = jpos + conv_b2strm(abs(iseph))
          ns = min(m, get_mems_bytes(abs(iseph), V(1)))
          m = m - ns
       endif
       if (ierr.eq.0) call ssq_read(ierr, u, V(j+1:j+ns), ns, swap)
       if (ierr.eq.0) call ssq_read_isep(ierr, u, isepf, pos=jpos, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit
          if (abs(iseph).ne.abs(isepf)) ierr = ERR_INCONSISTENT_RECORD_MARKERS
       endif
       if (ierr.ne.0) exit
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
          call ssq_skip_irec(ierr, u, 1, swap=swap)
       endif
    endif
    return
  end subroutine ssq_read_irec_f
  subroutine ssq_read_irec_d &
       & (ierr, u, v, n, swap, sub)
    use TOUZA_Nng_std,only: choice, conv_b2strm, get_mems_bytes
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
       if (ierr.eq.0) call ssq_read_isep(ierr, u, iseph, swap=swap)
       if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
       if (ierr.eq.0) then
          jpos = jpos + conv_b2strm(abs(iseph))
          ns = min(m, get_mems_bytes(abs(iseph), V(1)))
          m = m - ns
       endif
       if (ierr.eq.0) call ssq_read(ierr, u, V(j+1:j+ns), ns, swap)
       if (ierr.eq.0) call ssq_read_isep(ierr, u, isepf, pos=jpos, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit
          if (abs(iseph).ne.abs(isepf)) ierr = ERR_INCONSISTENT_RECORD_MARKERS
       endif
       if (ierr.ne.0) exit
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
          call ssq_skip_irec(ierr, u, 1, swap=swap)
       endif
    endif
    return
  end subroutine ssq_read_irec_d
  subroutine ssq_read_irec_a &
       & (ierr, u, v, n, swap, sub)
    use TOUZA_Nng_std,only: choice, conv_b2strm, get_mems_bytes
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
       if (ierr.eq.0) call ssq_read_isep(ierr, u, iseph, swap=swap)
       if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
       if (ierr.eq.0) then
          jpos = jpos + conv_b2strm(abs(iseph))
          ns = min(m, get_mems_bytes(abs(iseph), V(1)))
          m = m - ns
       endif
       ! if (ierr.eq.0) call ssq_read(ierr, u, V(j+1:j+ns), ns, swap)
       if (ierr.eq.0) read(UNIT=u, IOSTAT=ierr) V(j+1:j+ns)
       if (ierr.eq.0) call ssq_read_isep(ierr, u, isepf, pos=jpos, swap=swap)
       if (ierr.eq.0) then
          if (iseph.ge.0) exit
          if (abs(iseph).ne.abs(isepf)) ierr = ERR_INCONSISTENT_RECORD_MARKERS
       endif
       if (ierr.ne.0) exit
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
          call ssq_skip_irec(ierr, u, 1, swap=swap)
       endif
    endif
    ! write (*, *) 'irec/a/final', ierr
    return
  end subroutine ssq_read_irec_a

!!!_  - ssq_read_lrec - read a record with 64bit-marker
  subroutine ssq_read_lrec_i &
       & (ierr, u, v, n, swap)
    use TOUZA_Nng_std,only: choice, conv_b2strm, get_mems_bytes
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
    if (ierr.eq.0) call ssq_read_lsep(ierr, u, lseph, swap=swap)
    if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
    if (ierr.eq.0) then
       jpos = jpos + conv_b2strm(abs(lseph))
       m = get_mems_bytes(lseph, V(1))
       if (n.gt.m) then
          ierr = ERR_INVALID_RECORD_SIZE
       else
          m = min(m, n)
          call ssq_read(ierr, u, V, m, swap)
       endif
    endif
    if (ierr.eq.0) call ssq_read_lsep(ierr, u, lsepf, pos=jpos, swap=swap)
    if (ierr.eq.0) then
       if (lseph.ne.lsepf) ierr = ERR_INCONSISTENT_RECORD_MARKERS
    endif
    return
  end subroutine ssq_read_lrec_i
  subroutine ssq_read_lrec_l &
       & (ierr, u, v, n, swap)
    use TOUZA_Nng_std,only: choice, conv_b2strm, get_mems_bytes
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
    if (ierr.eq.0) call ssq_read_lsep(ierr, u, lseph, swap=swap)
    if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
    if (ierr.eq.0) then
       jpos = jpos + conv_b2strm(abs(lseph))
       m = get_mems_bytes(lseph, V(1))
       if (n.gt.m) then
          ierr = ERR_INVALID_RECORD_SIZE
       else
          m = min(m, n)
          call ssq_read(ierr, u, V, m, swap)
       endif
    endif
    if (ierr.eq.0) call ssq_read_lsep(ierr, u, lsepf, pos=jpos, swap=swap)
    if (ierr.eq.0) then
       if (lseph.ne.lsepf) ierr = ERR_INCONSISTENT_RECORD_MARKERS
    endif
    return
  end subroutine ssq_read_lrec_l
  subroutine ssq_read_lrec_f &
       & (ierr, u, v, n, swap)
    use TOUZA_Nng_std,only: choice, conv_b2strm, get_mems_bytes
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
    if (ierr.eq.0) call ssq_read_lsep(ierr, u, lseph, swap=swap)
    if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
    if (ierr.eq.0) then
       jpos = jpos + conv_b2strm(abs(lseph))
       m = get_mems_bytes(lseph, V(1))
       if (n.gt.m) then
          ierr = ERR_INVALID_RECORD_SIZE
       else
          m = min(m, n)
          call ssq_read(ierr, u, V, m, swap)
       endif
    endif
    if (ierr.eq.0) call ssq_read_lsep(ierr, u, lsepf, pos=jpos, swap=swap)
    if (ierr.eq.0) then
       if (lseph.ne.lsepf) ierr = ERR_INCONSISTENT_RECORD_MARKERS
    endif
    return
  end subroutine ssq_read_lrec_f
  subroutine ssq_read_lrec_d &
       & (ierr, u, v, n, swap)
    use TOUZA_Nng_std,only: choice, conv_b2strm, get_mems_bytes
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
    if (ierr.eq.0) call ssq_read_lsep(ierr, u, lseph, swap=swap)
    if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
    if (ierr.eq.0) then
       jpos = jpos + conv_b2strm(abs(lseph))
       m = get_mems_bytes(lseph, V(1))
       if (n.gt.m) then
          ierr = ERR_INVALID_RECORD_SIZE
       else
          m = min(m, n)
          call ssq_read(ierr, u, V, m, swap)
       endif
    endif
    if (ierr.eq.0) call ssq_read_lsep(ierr, u, lsepf, pos=jpos, swap=swap)
    if (ierr.eq.0) then
       if (lseph.ne.lsepf) ierr = ERR_INCONSISTENT_RECORD_MARKERS
    endif
    return
  end subroutine ssq_read_lrec_d
  subroutine ssq_read_lrec_a &
       & (ierr, u, v, n, swap)
    use TOUZA_Nng_std,only: conv_b2strm, get_mems_bytes
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
    if (ierr.eq.0) call ssq_read_lsep(ierr, u, lseph, swap=swap)
    if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
    if (ierr.eq.0) then
       jpos = jpos + conv_b2strm(abs(lseph))
       m = get_mems_bytes(lseph, V(1))
       if (n.gt.m) then
          ierr = ERR_INVALID_RECORD_SIZE
       else
          m = min(m, n)
          read(UNIT=u, IOSTAT=ierr) V(1:m)
          ! call ssq_read(ierr, u, V, m, swap)
       endif
    endif
    if (ierr.eq.0) call ssq_read_lsep(ierr, u, lsepf, pos=jpos, swap=swap)
    if (ierr.eq.0) then
       if (lseph.ne.lsepf) ierr = ERR_INCONSISTENT_RECORD_MARKERS
    endif
    return
  end subroutine ssq_read_lrec_a
! !!!_  - ssq_write_begin_irec
!   subroutine ssq_write_begin_irec &
!        & (ierr, u, jpos, lrec, swap)
!     use TOUZA_Nng_std,only: choice
!     implicit none
!     integer,            intent(out)         :: ierr
!     integer,            intent(in)          :: u
!     integer(KIND=KIOFS),intent(out)         :: jpos
!     integer(KIND=KI32), intent(in),optional :: lrec
!     logical,            intent(in),optional :: swap
!     integer(KIND=KI32) :: isep
!     ierr = 0
!     inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
!     if (ierr.eq.0) then
!        isep = choice(0, lrec)
!        call ssq_write_isep(ierr, u, isep, swap=swap)
!     endif
!     return
!   end subroutine ssq_write_begin_irec
! !!!_  - ssq_write_end_irec
!   subroutine ssq_write_end_irec &
!        & (ierr, u, jposh, sync, swap)
!     use TOUZA_Std_env,only: nc_strm, get_rlb
!     use TOUZA_Nng_std,only: choice
!     implicit none
!     integer,            intent(out)         :: ierr
!     integer,            intent(in)          :: u
!     integer(KIND=KIOFS),intent(in)          :: jposh
!     logical,            intent(in),optional :: sync
!     logical,            intent(in),optional :: swap
!     integer(KIND=KIOFS) :: jposf
!     integer(KIND=KI32)  :: isep, lbsep
!     ierr = 0
!     inquire(UNIT=u, IOSTAT=ierr, POS=jposf)
!     if (ierr.eq.0) then
!        lbsep = get_rlb(isep)
!        isep = (jposf - jposh) * nc_strm - lbsep
!        if (choice(.true., sync)) then
!           call ssq_write_isep(ierr, u, isep, pos=jposh, swap=swap)
!        endif
!        if (ierr.eq.0) then
!           call ssq_write_isep(ierr, u, isep, pos=jposf, swap=swap)
!        endif
!     endif
!     return
!   end subroutine ssq_write_end_irec
!!!_  - ssq_read_begin_irec
  subroutine ssq_read_begin_irec &
       & (ierr, u, jpos, swap)
    use TOUZA_Nng_std,only: nc_strm
    implicit none
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KIOFS),intent(out)         :: jpos
    logical,            intent(in),optional :: swap
    integer(KIND=KI32) :: isep
    ierr = 0
    if (ierr.eq.0) call ssq_read_isep(ierr, u, isep, swap=swap)
    if (ierr.eq.0) inquire(UNIT=u, IOSTAT=ierr, POS=jpos)
    if (ierr.eq.0) jpos = jpos + isep / nc_strm
    return
  end subroutine ssq_read_begin_irec
!!!_  - ssq_read_end_irec
  subroutine ssq_read_end_irec &
       & (ierr, u, jpos, swap)
    implicit none
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KIOFS),intent(in)          :: jpos
    logical,            intent(in),optional :: swap
    integer(KIND=KI32)  :: isep
    call ssq_read_isep(ierr, u, isep, pos=jpos, swap=swap)
    return
  end subroutine ssq_read_end_irec
!!!_  - ssq_write
  subroutine ssq_write_i &
       & (ierr, u, v, n, swap)
    use TOUZA_Nng_std,only: choice
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u
    integer(KIND=KARG),intent(in)          :: V(*)
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    if (choice(.false.,swap)) then
       write(UNIT=u, IOSTAT=ierr) ssq_eswap(V(1:n))
    else
       write(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
  end subroutine ssq_write_i

  subroutine ssq_write_l &
       & (ierr, u, v, n, swap)
    use TOUZA_Nng_std,only: choice
    implicit none
    integer,parameter :: KARG=KI64
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u
    integer(KIND=KARG),intent(in)          :: V(*)
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    if (choice(.false.,swap)) then
       write(UNIT=u, IOSTAT=ierr) ssq_eswap(V(1:n))
    else
       write(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
  end subroutine ssq_write_l
  subroutine ssq_write_f &
       & (ierr, u, v, n, swap)
    use TOUZA_Nng_std,only: choice
    implicit none
    integer,parameter :: KARG=KFLT, KBUF=KI32
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u
    real(KIND=KARG),intent(in)          :: V(*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    if (choice(.false.,swap)) then
       write(UNIT=u, IOSTAT=ierr) ssq_eswap(TRANSFER(V(1:n), 0_KBUF, n))
    else
       write(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
  end subroutine ssq_write_f
  subroutine ssq_write_d &
       & (ierr, u, v, n, swap)
    use TOUZA_Nng_std,only: choice
    implicit none
    integer,parameter :: KARG=KDBL, KBUF=KI64
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u
    real(KIND=KARG),intent(in)          :: V(*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    if (choice(.false.,swap)) then
       write(UNIT=u, IOSTAT=ierr) ssq_eswap(TRANSFER(V(1:n), 0_KBUF, n))
    else
       write(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
  end subroutine ssq_write_d
!!!_  - ssq_read
  subroutine ssq_read_i &
       & (ierr, u, v, n, swap)
    use TOUZA_Nng_std,only: choice
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u
    integer(KIND=KARG),intent(out)         :: V(*)
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    if (choice(.false.,swap)) then
       read(UNIT=u, IOSTAT=ierr) V(1:n)
       if (ierr.eq.0) V(1:n) = ssq_eswap(V(1:n))
    else
       read(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
  end subroutine ssq_read_i
  subroutine ssq_read_l &
       & (ierr, u, v, n, swap)
    use TOUZA_Nng_std,only: choice
    implicit none
    integer,parameter :: KARG=KI64
    integer,           intent(out)         :: ierr
    integer,           intent(in)          :: u
    integer(KIND=KARG),intent(out)         :: V(*)
    integer,           intent(in)          :: n
    logical,           intent(in),optional :: swap
    if (choice(.false.,swap)) then
       read(UNIT=u, IOSTAT=ierr) V(1:n)
       if (ierr.eq.0) V(1:n) = ssq_eswap(V(1:n))
    else
       read(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
  end subroutine ssq_read_l
  subroutine ssq_read_f &
       & (ierr, u, v, n, swap)
    use TOUZA_Nng_std,only: choice
    implicit none
    integer,parameter :: KARG=KFLT, KBUF=KI32
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u
    real(KIND=KARG),intent(out)         :: V(*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    if (choice(.false.,swap)) then
       read(UNIT=u, IOSTAT=ierr) V(1:n)
       if (ierr.eq.0) then
          V(1:n) = TRANSFER(ssq_eswap(TRANSFER(V(1:n), 0_KBUF, n)), 0.0_KARG, n)
       endif
    else
       read(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
  end subroutine ssq_read_f
  subroutine ssq_read_d &
       & (ierr, u, v, n, swap)
    use TOUZA_Nng_std,only: choice
    implicit none
    integer,parameter :: KARG=KDBL, KBUF=KI64
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: u
    real(KIND=KARG),intent(out)         :: V(*)
    integer,        intent(in)          :: n
    logical,        intent(in),optional :: swap
    if (choice(.false.,swap)) then
       read(UNIT=u, IOSTAT=ierr) V(1:n)
       if (ierr.eq.0) then
          V(1:n) = TRANSFER(ssq_eswap(TRANSFER(V(1:n), 0_KBUF, n)), 0.0_KARG, n)
       endif
    else
       read(UNIT=u, IOSTAT=ierr) V(1:n)
    endif
  end subroutine ssq_read_d
!!!_ + private subroutines
!!!_  - ssq_write_isep
  subroutine ssq_write_isep_i (ierr, u, sep, pos, swap, sub)
    use TOUZA_Nng_std,only: choice
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
    if (choice(.false., swap)) isep = ssq_eswap(isep)
    if (present(pos)) then
       write(UNIT=u, IOSTAT=ierr, POS=pos) isep
    else
       write(UNIT=u, IOSTAT=ierr) isep
    endif
  end subroutine ssq_write_isep_i
  subroutine ssq_write_isep_l (ierr, u, sep, pos, swap, sub)
    use TOUZA_Nng_std,only: choice
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
    if (choice(.false., swap)) isep = ssq_eswap(isep)
    if (present(pos)) then
       write(UNIT=u, IOSTAT=ierr, POS=pos) isep
    else
       write(UNIT=u, IOSTAT=ierr) isep
    endif
  end subroutine ssq_write_isep_l
!!!_  - ssq_write_lsep
  subroutine ssq_write_lsep_i (ierr, u, sep, pos, swap)
    use TOUZA_Nng_std,only: choice
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KI32
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(in)          :: sep
    integer(KIND=KIOFS),intent(in),optional :: pos
    logical,            intent(in),optional :: swap
    integer(KIND=KISEP) :: lsep

    if (choice(.false., swap)) then
       lsep = ssq_eswap(sep)
    else
       lsep = sep
    endif
    if (present(pos)) then
       write(UNIT=u, IOSTAT=ierr, POS=pos) lsep
    else
       write(UNIT=u, IOSTAT=ierr) lsep
    endif
  end subroutine ssq_write_lsep_i
  subroutine ssq_write_lsep_l (ierr, u, sep, pos, swap)
    use TOUZA_Nng_std,only: choice
    implicit none
    integer,parameter :: KISEP=KI64, KARG=KI64
    integer,            intent(out)         :: ierr
    integer,            intent(in)          :: u
    integer(KIND=KARG), intent(in)          :: sep
    integer(KIND=KIOFS),intent(in),optional :: pos
    logical,            intent(in),optional :: swap
    integer(KIND=KISEP) :: lsep

    if (choice(.false., swap)) then
       lsep = ssq_eswap(sep)
    else
       lsep = sep
    endif
    if (present(pos)) then
       write(UNIT=u, IOSTAT=ierr, POS=pos) lsep
    else
       write(UNIT=u, IOSTAT=ierr) lsep
    endif
  end subroutine ssq_write_lsep_l

!!!_  - ssq_read_isep
  subroutine ssq_read_isep_i (ierr, u, sep, pos, swap)
    use TOUZA_Nng_std,only: choice
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
       if (ierr.eq.0) sep = ssq_eswap(sep)
    endif
  end subroutine ssq_read_isep_i
  subroutine ssq_read_isep_l (ierr, u, sep, pos, swap)
    use TOUZA_Nng_std,only: choice
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
          isep = ssq_eswap(isep)
          sep = isep
       endif
    else
       if (ierr.eq.0) sep = isep
    endif
  end subroutine ssq_read_isep_l
!!!_  - ssq_read_lsep
  subroutine ssq_read_lsep_i (ierr, u, sep, pos, swap)
    use TOUZA_Nng_std,only: choice
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
    if (choice(.false., swap)) then
       if (ierr.eq.0) then
          lsep = ssq_eswap(lsep)
          sep = lsep
       endif
    else
       if (ierr.eq.0) sep = lsep
    endif
  end subroutine ssq_read_lsep_i
  subroutine ssq_read_lsep_l (ierr, u, sep, pos, swap)
    use TOUZA_Nng_std,only: choice
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
       if (ierr.eq.0) sep = ssq_eswap(sep)
    endif
  end subroutine ssq_read_lsep_l

!!!_  - ssq_eswap() - elemental
  ELEMENTAL integer(KIND=KI32) function ssq_eswap_i(V) &
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
  end function ssq_eswap_i

  ELEMENTAL integer(KIND=KI64) function ssq_eswap_l(V) &
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
  end function ssq_eswap_l

!!!_  - ssq_eswap_hl() - elemental (higher/lower bits independent swap)
  ELEMENTAL integer(KIND=KI64) function ssq_eswap_hl(V) &
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
  end function ssq_eswap_hl

!!!_  - ssq_swap() - swap expanded
  elemental integer(KIND=KI32) function ssq_swap_i(V) &
       & result(R)
    use TOUZA_Nng_std,only: KI32
    implicit none
    integer(kind=KI32),intent(in) :: V
    R = IOR(IOR(ISHFT(IBITS(V, 0*LBU, LBU), 3*LBU),  &
         &      ISHFT(IBITS(V, 1*LBU, LBU), 2*LBU)), &
         &  IOR(ISHFT(IBITS(V, 2*LBU, LBU), 1*LBU),  &
         &            IBITS(V, 3*LBU, LBU)))
    return
  end function ssq_swap_i

  elemental integer(KIND=KI64) function ssq_swap_l(V) &
       & result(R)
    use TOUZA_Nng_std,only: KI64
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
  end function ssq_swap_l

!!!_  - ssq_rseek - seek position to read
  subroutine ssq_rseek &
       & (ierr, u, step, whence)
    use TOUZA_Nng_std,only: choice

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
       if (ierr.eq.0) jpos = jpos + st
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
       call ssq_rseek_workaround(ierr, u, jpos)
#else /* not OPT_STREAM_RPOS_WORKAROUND */
       read(UNIT=u, IOSTAT=ierr, POS=jpos)
#endif /* not OPT_STREAM_RPOS_WORKAROUND */
    endif
    return
  end subroutine ssq_rseek

!!!_  - ssq_rseek_workaround - seek position to read (workaround)
  subroutine ssq_rseek_workaround &
       & (ierr, u, jpos)
    !! caution: T assumed to be 1-byte
    use TOUZA_Nng_std,only: nc_strm
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
  end subroutine ssq_rseek_workaround

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

end module TOUZA_Nng_io

!!!_@ test_nng_io - test program
#ifdef TEST_NNG_IO
program test_nng_io
  use TOUZA_Nng_std,only: KI32,KI64,KFLT,KDBL
  use TOUZA_Nng_io
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

  character(len=512) :: file = 'out.nng'

  integer kendi
  integer j
  integer mi, ml, mf, md, ma

101 format(A, ' = ', I0)

  call init(ierr, bodr=BODR_CHECK_VERBOSE)
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
  if (ierr.eq.0) call ssq_open(ierr, u, file, ACTION='W', STATUS='R', kendi=kendi)
  if (ierr.eq.0) then
     write(*, *) 'endianness = ', kendi, u
  endif

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

  if (ierr.eq.0) call ssq_close(ierr, u, file)

  if (ierr.eq.0) call ssq_open(ierr, u, file, ACTION='R')

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

  if (ierr.eq.0) call ssq_close(ierr, u, file)
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
       if (ierr.eq.0) call ssq_read_irec(ierr, u, xi(1:ni), ni, swap=swap)
       if (ierr.eq.0) call ssq_read_irec(ierr, u, xl(1:nl), nl, swap=swap)
       if (ierr.eq.0) call ssq_read_irec(ierr, u, xf(1:nf), nf, swap=swap)
       if (ierr.eq.0) call ssq_read_irec(ierr, u, xd(1:nd), nd, swap=swap)
       if (ierr.eq.0) call ssq_read_irec(ierr, u, xa(1:na), na, swap=swap)
    else
       if (ierr.eq.0) call ssq_read_lrec(ierr, u, xi(1:ni), ni, swap=swap)
       if (ierr.eq.0) call ssq_read_lrec(ierr, u, xl(1:nl), nl, swap=swap)
       if (ierr.eq.0) call ssq_read_lrec(ierr, u, xf(1:nf), nf, swap=swap)
       if (ierr.eq.0) call ssq_read_lrec(ierr, u, xd(1:nd), nd, swap=swap)
       if (ierr.eq.0) call ssq_read_lrec(ierr, u, xa(1:na), na, swap=swap)
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
       if (ierr.eq.0) call ssq_write_irec(ierr, u, vi(1:ni), ni, swap=swap)
       if (ierr.eq.0) call ssq_write_irec(ierr, u, vl(1:nl), nl, swap=swap)
       if (ierr.eq.0) call ssq_write_irec(ierr, u, vf(1:nf), nf, swap=swap)
       if (ierr.eq.0) call ssq_write_irec(ierr, u, vd(1:nd), nd, swap=swap)
       if (ierr.eq.0) call ssq_write_irec(ierr, u, va(1:na), na, swap=swap)
    else
       if (ierr.eq.0) call ssq_write_lrec(ierr, u, vi(1:ni), ni, swap=swap)
       if (ierr.eq.0) call ssq_write_lrec(ierr, u, vl(1:nl), nl, swap=swap)
       if (ierr.eq.0) call ssq_write_lrec(ierr, u, vf(1:nf), nf, swap=swap)
       if (ierr.eq.0) call ssq_write_lrec(ierr, u, vd(1:nd), nd, swap=swap)
       if (ierr.eq.0) call ssq_write_lrec(ierr, u, va(1:na), na, swap=swap)
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
end program test_nng_io

#endif /* TEST_NNG_IO */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

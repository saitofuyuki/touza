!!!_! std_env.F90 - touza/std standard environments
! Maintainer: SAITO Fuyuki
! Created: May 30 2020
#define TIME_STAMP 'Time-stamp: <2023/02/05 21:52:06 fuyuki std_env.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2020-2023
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
!!!_* Notes
!     Environment checkers using external files are ON-DEMAND.
!     Currently, following are on-demand ones:
!       - file byte-order
!       - storage size unit
!       - eof status
!     while following are not:
!       - memory byte-order
!       - standard input/output units (not involve
!         read/write access of external files)
!!!_* include
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_std.h"
!!!_* macros
#ifndef   OPT_STDIN_UNIT
#  define OPT_STDIN_UNIT -1  /* default standard input unit */
#endif
#ifndef   OPT_STDOUT_UNIT
#  define OPT_STDOUT_UNIT -1 /* default standard output unit */
#endif
#ifndef   OPT_STDERR_UNIT
#  define OPT_STDERR_UNIT -1 /* default standard error unit */
#endif
#ifndef   OPT_STD_UNITS_TRY
#  define OPT_STD_UNITS_TRY -1  /* default try limit for brute-force std units check */
#endif
#ifndef   OPT_FILE_STORAGE_BITS
#  define OPT_FILE_STORAGE_BITS 0  /* file storage unit in BITS */
#endif
#ifndef   OPT_STREAM_STORAGE_BITS
#  define OPT_STREAM_STORAGE_BITS 0  /* file storage unit in BITS for stream i/o */
#endif
#ifndef   OPT_CHAR_STORAGE_BITS
#  define OPT_CHAR_STORAGE_BITS 0  /* character storage unit in BITS */
#endif
#ifndef   OPT_BYTE_BITS
#  define OPT_BYTE_BITS 0  /* (parameter) number of bits in a byte (character) */
#endif
#ifndef   OPT_PATH_LEN
#  define OPT_PATH_LEN 1024 /* file path limit length */
#endif
#ifndef   OPT_IOSTAT_EOF
#  define OPT_IOSTAT_EOF 0 /* end-of-file IOSTAT */
#endif
#ifndef   OPT_INTEGER_OFFSET_KIND
#  define OPT_INTEGER_OFFSET_KIND 0  /* kind to store file position */
#endif
!!!_ + debug
#ifndef   TEST_STD_ENV
#  define TEST_STD_ENV 0
#endif
#ifndef   DEBUG_PRIVATES
#  define DEBUG_PRIVATES TEST_STD_ENV
#endif
!!!_@ TOUZA_Std_env - standard environments
module TOUZA_Std_env
  use TOUZA_Std_prc,only: KI32, KI64
  use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
  use TOUZA_Std_log,only: unit_global,  trace_fine,   trace_control
!!!_ = declaration
!!!_  - ISO_FORTRAN_ENV module
#if HAVE_FORTRAN_ISO_FORTRAN_ENV
  use ISO_FORTRAN_ENV,only: &
       &  OUTPUT_UNIT, INPUT_UNIT, ERROR_UNIT, &
       &  FILE_STORAGE_SIZE, CHARACTER_STORAGE_SIZE

#  if OPT_STDOUT_UNIT < 0
#    undef  OPT_STDOUT_UNIT
#    define OPT_STDOUT_UNIT OUTPUT_UNIT
#  else
#    warning "Force to use OPT_STDOUT_UNIT"
#  endif

#  if OPT_STDIN_UNIT < 0
#    undef  OPT_STDIN_UNIT
#    define OPT_STDIN_UNIT  INPUT_UNIT
#  else
#    warning "Force to use OPT_STDIN_UNIT"
#  endif

#  if OPT_STDERR_UNIT < 0
#    undef  OPT_STDERR_UNIT
#    define OPT_STDERR_UNIT  ERROR_UNIT
#  else
#    warning "Force to use OPT_STDERR_UNIT"
#  endif

#  if OPT_FILE_STORAGE_BITS >= 0
#    warning "Force to use OPT_FILE_STORAGE_BITS"
#  else
#    undef  OPT_FILE_STORAGE_BITS
#    define OPT_FILE_STORAGE_BITS FILE_STORAGE_SIZE
#  endif

#  if OPT_CHAR_STORAGE_BITS >= 0
#    warning "Force to use OPT_CHAR_STORAGE_BITS"
#  else
#    undef  OPT_CHAR_STORAGE_BITS
#    define OPT_CHAR_STORAGE_BITS CHARACTER_STORAGE_SIZE
#  endif

#  if OPT_BYTE_BITS > 0
#    warning "Force to use OPT_BYTE_BITS"
#  else
#    undef  OPT_BYTE_BITS
#    define OPT_BYTE_BITS CHARACTER_STORAGE_SIZE
#  endif

#  if OPT_FILE_STREAM_BITS > 0
#    warning "Force to use OPT_FILE_STREAM_BITS"
#  else
#    undef  OPT_FILE_STREAM_BITS
#    define OPT_FILE_STREAM_BITS FILE_STORAGE_SIZE
#  endif

#else  /* not HAVE_FORTRAN_ISO_FORTRAN_ENV */

#  if OPT_BYTE_BITS > 0
#    warning "Force to use OPT_BYTE_BITS"
#  else
#    undef  OPT_BYTE_BITS
#    define OPT_BYTE_BITS 8   /* typically 1 byte = 8 bits */
#  endif

#endif /* not HAVE_FORTRAN_ISO_FORTRAN_ENV */
!!!_  - ISO_FORTRAN_ENV/IOSTAT_EOF
#if    HAVE_FORTRAN_ISO_FORTRAN_ENV_IOSTAT_END
  use ISO_FORTRAN_ENV,only: IOSTAT_END
#  if OPT_IOSTAT_EOF
#    warning "Force to use OPT_IOSTAT_EOF"
#  else /* not OPT_IOSTAT_EOF */
#    undef   OPT_IOSTAT_EOF
#    define  OPT_IOSTAT_EOF IOSTAT_END
#  endif /* not OPT_IOSTAT_EOF */
#endif /* HAVE_FORTRAN_ISO_FORTRAN_ENV_IOSTAT_END */
!!!_  - ISO_C_BINDING module
#if HAVE_FORTRAN_ISO_C_BINDING
#endif /* HAVE_FORTRAN_ISO_C_BINDING */
!!!_  - MPI for offset only
#if OPT_INTEGER_OFFSET_KIND
#else /* not OPT_INTEGER_OFFSET_KIND */
#  undef OPT_INTEGER_OFFSET_KIND
#  if OPT_USE_MPI
#    define OPT_INTEGER_OFFSET_KIND MPI_OFFSET_KIND
  use mpi,only: MPI_OFFSET_KIND
#  else
#    define OPT_INTEGER_OFFSET_KIND KI64
#  endif
#endif /* not OPT_INTEGER_OFFSET_KIND */
!!!_  - default
  implicit none
  private
# define __MDL__ 'env'
# define _ERROR(E) (E - ERR_MASK_STD_ENV)
!!!_  - parameters
  integer,parameter,public :: KIOFS = OPT_INTEGER_OFFSET_KIND
  integer,parameter,public :: endian_UNKNOWN = 0
  integer,parameter,public :: endian_ERROR  = -1
  integer,parameter,public :: endian_BIG    = 1234
  integer,parameter,public :: endian_LITTLE = 4321
  integer,parameter,public :: endian_OTHER  = 9999

  integer,parameter,public :: nbits_byte = OPT_BYTE_BITS
!!!_  - public constants
  integer,save,public :: uin  = OPT_STDIN_UNIT
  integer,save,public :: uout = OPT_STDOUT_UNIT
  integer,save,public :: uerr = OPT_STDERR_UNIT

  ! direct unformatted
  integer,save,public :: nb_recl = 0          ! number of bytes per unit record length
  ! stream unformatted
  integer,save,public :: nc_strm = 0          ! number of characters per unit length for stream i/o

  integer,save,public :: kendi_mem  = endian_UNKNOWN
  integer,save,public :: kendi_file = endian_UNKNOWN

  integer,save,public :: err_eof = OPT_IOSTAT_EOF
!!!_  - static
  integer,save :: lfileu = OPT_FILE_STORAGE_BITS
  integer,save :: lcharu = OPT_CHAR_STORAGE_BITS
  integer,save :: lstrmu = OPT_STREAM_STORAGE_BITS

  integer,save :: lreci = 0, lrecl = 0 ! (direct) record lengths of single 32/64-bit integer
  integer,save :: lrecf = 0, lrecd = 0 ! (direct) record lengths of single float/double

  integer,save :: mstrmi = 0, mstrml = 0 ! (stream) unit length of single float/double
  integer,save :: mstrmf = 0, mstrmd = 0 ! (stream) unit length single float/double

  integer,save :: nbyti = 0, nbytl = 0 ! (bytes) single float/double
  integer,save :: nbytf = 0, nbytd = 0 ! (bytes) single float/double

  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL
  integer,save :: ulog = unit_global

  integer,save :: err_default = ERR_NO_INIT - ERR_MASK_STD_ENV

  character(len=*),parameter :: etest_org = 'abcd'
  integer,save :: etest_big = 0, etest_little = 0

!!!_  - public
  public init, diag, finalize
  public init_batch
  public init_unfmtd_recl, get_size_ufd
  public init_unfmtd_strm, get_unit_strm, get_size_strm
  public init_file_bodr,   check_byte_order, check_bodr_unit
  public init_io_status,   is_eof_ss
  public get_size_bytes,   conv_b2strm,   get_mems_bytes
  public get_login_name,   get_host_name
#if DEBUG_PRIVATES
  public check_bodr_mem, check_bodr_files
  public brute_force_stdu
  public brute_force_recl_unit_rw, brute_force_recl_unit_w
  public brute_force_recl_type
#endif
!!!_  - interfaces
  interface brute_force_recl_type
     module procedure brute_force_recl_type_i
     module procedure brute_force_recl_type_l
     module procedure brute_force_recl_type_f
     module procedure brute_force_recl_type_d
  end interface brute_force_recl_type

  interface get_size_ufd
     module procedure get_size_ufd_i
     module procedure get_size_ufd_l
     module procedure get_size_ufd_f
     module procedure get_size_ufd_d
  end interface get_size_ufd

  interface get_unit_strm
     module procedure get_unit_strm_i
     module procedure get_unit_strm_l
     module procedure get_unit_strm_f
     module procedure get_unit_strm_d
     module procedure get_unit_strm_a
  end interface get_unit_strm

  interface get_size_strm
     module procedure get_size_strm_i, get_size_strm_li
     module procedure get_size_strm_l, get_size_strm_ll
     module procedure get_size_strm_f, get_size_strm_lf
     module procedure get_size_strm_d, get_size_strm_ld
     module procedure get_size_strm_a, get_size_strm_la
  end interface get_size_strm

  interface get_size_bytes
     module procedure get_size_bytes_a,  get_size_bytes_la
     module procedure get_size_bytes_i,  get_size_bytes_li
     module procedure get_size_bytes_l,  get_size_bytes_ll
     module procedure get_size_bytes_f,  get_size_bytes_lf
     module procedure get_size_bytes_d,  get_size_bytes_ld
  end interface get_size_bytes

  interface get_mems_bytes
     module procedure get_mems_bytes_a, get_mems_bytes_la
     module procedure get_mems_bytes_i, get_mems_bytes_li
     module procedure get_mems_bytes_l, get_mems_bytes_ll
     module procedure get_mems_bytes_f, get_mems_bytes_lf
     module procedure get_mems_bytes_d, get_mems_bytes_ld
  end interface get_mems_bytes

  interface conv_b2strm
     module procedure conv_b2strm_i
     module procedure conv_b2strm_l
  end interface conv_b2strm

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, levtry, icomm)
    use TOUZA_Std_utl,only: utl_init=>init, choice
    use TOUZA_Std_log,only: log_init=>init
    use TOUZA_Std_prc,only: prc_init=>init
    use TOUZA_Std_fun,only: fun_init=>init
    use TOUZA_Std_mwe,only: mwe_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u      ! log unit
    integer,intent(in),optional :: levv   ! verbose level
    integer,intent(in),optional :: mode   ! initialization flag
    integer,intent(in),optional :: levtry ! brute-force level
    integer,intent(in),optional :: icomm  ! mwe argument

    integer md, lv, lmd

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
          if (ierr.eq.0) call mwe_init(ierr, ulog, levv=lv, mode=lmd, icomm=icomm)
          if (ierr.eq.0) call fun_init(ierr, ulog, levv=lv, mode=lmd, icomm=icomm)
       endif
       if (is_first_force(init_counts, mode)) then
          if (ierr.eq.0) call set_endian_tester(ierr)
          if (ierr.eq.0) call init_batch(ierr, levtry, u=ulog, levv=lv)
       endif
       ! write(*, *) 'env/batch', ierr
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = _ERROR(ERR_FAILURE_INIT)
    endif
    ! write(*, *) 'env', ierr, init_counts

    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std_utl, only: utl_diag=>diag, choice
    use TOUZA_Std_log, only: log_diag=>diag, msg_mdl
    use TOUZA_Std_prc, only: prc_diag=>diag
    use TOUZA_Std_fun, only: fun_diag=>diag
    use TOUZA_Std_mwe, only: mwe_diag=>diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer lv, md, utmp, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(uout, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (VCHECK_NORMAL(lv)) then
                call msg_mdl(TIME_STAMP, __MDL__, utmp)
                call msg_mdl('(''offset kind = '', I0)', KIOFS, __MDL__, utmp)
             endif
             if (VCHECK_DEBUG(lv)) then
                call msg_mdl('(''init = '', I0)', (/init_counts/), __MDL__, utmp)
             endif
          endif
          if (VCHECK_DETAIL(lv)) then
             if (ierr.eq.0) call diag_stdu(ierr, utmp)
             if (ierr.eq.0) call diag_recl(ierr, utmp)
             if (ierr.eq.0) call diag_strm(ierr, utmp)
             if (ierr.eq.0) call diag_bodr(ierr, utmp)
             if (ierr.eq.0) call diag_stat(ierr, utmp)
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call prc_diag(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call utl_diag(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call log_diag(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call mwe_diag(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call fun_diag(ierr, utmp, lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: utl_finalize=>finalize, choice
    use TOUZA_Std_log,only: log_finalize=>finalize
    use TOUZA_Std_prc,only: prc_finalize=>finalize
    use TOUZA_Std_fun,only: fun_finalize=>finalize
    use TOUZA_Std_mwe,only: mwe_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(uout, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call prc_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call utl_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call log_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call mwe_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call fun_finalize(ierr, utmp, lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + init subcontracts
!!!_  & set_endian_tester
  subroutine set_endian_tester (ierr)
    implicit none
    integer,intent(out) :: ierr
    integer j
    ierr = 0
    if (etest_big .eq. 0) then
       do j = 1, len_trim(etest_org)
          etest_big = etest_big * 256 + IACHAR(etest_org(j:j))
       enddo
    endif
    if (etest_little .eq. 0) then
       do j = len_trim(etest_org), 1, -1
          etest_little = etest_little * 256 + IACHAR(etest_org(j:j))
       enddo
    endif
  end subroutine set_endian_tester
!!!_  & init_batch - initialization (environment checks for non-on-demand properties)
  subroutine init_batch &
       & (ierr, levtry, u, levv)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: levtry ! negative to skip
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer lv

    ierr = 0
    lv = choice(lev_verbose, levv)
    if (choice(0, levtry).ge.0) then
       if (ierr.eq.0) call check_std_units(ierr, uin, uout, uerr, lv, levtry)
       if (ierr.eq.0) call health_check_stdu(ierr, lv)
       if (ierr.eq.0) call check_bodr_mem(ierr, kendi_mem, u, levv)
    endif
    return
  end subroutine init_batch
!!!_ + diag subcontracts
!!!_  & diag_stdu
  subroutine diag_stdu(ierr, u)
    use TOUZA_Std_log,only: msg_mdl
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer utmp

    ierr = err_default

#   if HAVE_FORTRAN_ISO_FORTRAN_ENV
      utmp = choice(OUTPUT_UNIT, u)
      call msg_mdl &
           & ('(''ISO_FORTRAN_ENV enabled = '', 3(1x, I0))', &
           &  (/INPUT_UNIT, OUTPUT_UNIT, ERROR_UNIT/), &
           &  __MDL__, utmp)
#   else /* not HAVE_FORTRAN_ISO_FORTRAN_ENV */
102 format(__MDL__, 'ISO_FORTRAN_ENV disabled')
      utmp = choice(-1, u)
      call msg_mdl &
           & ('ISO_FORTRAN_ENV disabled',  __MDL__, utmp)
#   endif

    call msg_mdl &
         & ('(''units = '', 3(1x, I0))', (/uin, uout, uerr/), __MDL__, utmp)

    return
  end subroutine diag_stdu

!!!_  & diag_stat
  subroutine diag_stat(ierr, u)
    use TOUZA_Std_log,only: msg_mdl
    use TOUZA_Std_utl,only: choice
#if HAVE_FORTRAN_ISO_FORTRAN_ENV_IOSTAT_END
    use ISO_FORTRAN_ENV,only: IOSTAT_END
#endif
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer utmp

    ierr = err_default

#   if HAVE_FORTRAN_ISO_FORTRAN_ENV_IOSTAT_END
      utmp = choice(OUTPUT_UNIT, u)
      call msg_mdl &
           & ('(''ISO_FORTRAN_ENV::IOSTAT_END enabled = '', I0)', &
           &  IOSTAT_END, &
           &  __MDL__, utmp)
#   else /* not HAVE_FORTRAN_ISO_FORTRAN_ENV */
      utmp = choice(-1, u)
      call msg_mdl &
           & ('ISO_FORTRAN_ENV::IOSTAT_END disabled',  __MDL__, utmp)
#   endif

    call msg_mdl &
         & ('(''eof = '', I0)', err_eof, __MDL__, utmp)

    return
  end subroutine diag_stat

!!!_  & diag_recl
  subroutine diag_recl(ierr, u)
#if HAVE_FORTRAN_ISO_FORTRAN_ENV
  use ISO_FORTRAN_ENV,only: &
       &  FILE_STORAGE_SIZE, CHARACTER_STORAGE_SIZE
#endif
    use TOUZA_Std_log,only: msg_mdl
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer utmp

    ierr = err_default
    utmp = choice(uout, u)

    call msg_mdl &
         & ('(''storage bits = '', I0, 1x, I0)', &
         &  (/lfileu, lcharu/), &
         &  __MDL__, utmp)
#if HAVE_FORTRAN_ISO_FORTRAN_ENV
    call msg_mdl &
         & ('(''storage bits (iso_fortran_env) = '', I0, 1x, I0)', &
         &  (/FILE_STORAGE_SIZE, CHARACTER_STORAGE_SIZE/), &
         &  __MDL__, utmp)
#endif /* HAVE_FORTRAN_ISO_FORTRAN_ENV */
    call msg_mdl &
         & ('(''size = '', I0, 1x, I0, 1x, I0, 1x, I0)', &
         &  (/nbyti, nbytl, nbytf, nbytd/), &
         &  __MDL__, utmp)
    call msg_mdl &
         & ('(''direct unit bytes = '', I0)', nb_recl, __MDL__, utmp)
    call msg_mdl &
         & ('(''direct recl = '', I0, 1x, I0, 1x, I0, 1x, I0)', &
         &  (/lreci, lrecl, lrecf, lrecd/), &
         &  __MDL__, utmp)
    return
  end subroutine diag_recl

!!!_  & diag_strm
  subroutine diag_strm(ierr, u)
#if HAVE_FORTRAN_ISO_FORTRAN_ENV
  use ISO_FORTRAN_ENV,only: &
       &  FILE_STORAGE_SIZE, CHARACTER_STORAGE_SIZE
#endif
    use TOUZA_Std_log,only: msg_mdl
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer utmp

    ierr = err_default
    utmp = choice(uout, u)

    call msg_mdl &
         & ('(''storage bits = '', I0, 1x, I0)', &
         &  (/lfileu, lcharu/), &
         &  __MDL__, utmp)
#if HAVE_FORTRAN_ISO_FORTRAN_ENV
    call msg_mdl &
         & ('(''storage bits (iso_fortran_env) = '', I0, 1x, I0)', &
         &  (/FILE_STORAGE_SIZE, CHARACTER_STORAGE_SIZE/), &
         &  __MDL__, utmp)
#endif /* HAVE_FORTRAN_ISO_FORTRAN_ENV */
    call msg_mdl &
         & ('(''stream unit bytes = '', I0)', nc_strm,  __MDL__, utmp)
    call msg_mdl &
         & ('(''stream length = '', I0, 1x, I0, 1x, I0, 1x, I0)', &
         &  (/mstrmi, mstrml, mstrmf, mstrmd/), &
         &  __MDL__, utmp)
    return
  end subroutine diag_strm

!!!_  & diag_bodr
  subroutine diag_bodr(ierr, u)
    use TOUZA_Std_log,only: msg_mdl
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer utmp

    ierr = err_default
    utmp = choice(uout, u)

    call msg_mdl &
         & ('(''memory endianness = '', I0)', kendi_mem, &
         &  __MDL__, utmp)
    call msg_mdl &
         & ('(''file endianness = '', I0)', kendi_file, &
         &  __MDL__, utmp)
    return
  end subroutine diag_bodr

!!!_ + i/o units (auto)
!!!_  & check_std_units - standard i/o units
  subroutine check_std_units &
       & (ierr, ustdi, ustdo, ustde, levv, levtry)
    use TOUZA_Std_log,only: msg_mdl
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(inout)       :: ustdi, ustdo, ustde
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: levtry
    integer lv, ltry
    integer ui, uo, ue

    lv = choice(lev_verbose, levv)
    if (ustdi.lt.0.or.ustdo.lt.0.or.ustde.lt.0) then
       ltry = choice(OPT_STD_UNITS_TRY, levtry)
       if (ltry.ge.0) then
          if (VCHECK_INFO(lv)) then
             call msg_mdl &
                  & ('(''try brute-force finder: '', I0)', (/ltry/), __MDL__)
          endif
          call brute_force_stdu(ierr, ui, uo, ue, 0, ltry, lv)
          if (ierr.eq.0) then
             if (ustdi.lt.0) ustdi = ui
             if (ustdo.lt.0) ustdo = uo
             if (ustde.lt.0) ustde = ue
             if (ustde.lt.0) ustde = uo
          endif
       endif
    endif
    return
  end subroutine check_std_units

!!!_  & brute_force_stdu - lazy trial to find standard units
  subroutine brute_force_stdu &
       & (ierr, ustdi, ustdo, ustde, ubgn, uend, levv)
    ! CAUTION: results are not guaranteed.
    ! Search standard io units between UBGN and UEND
    ! Set USTDI, USTDO, USTDE as input, output, error respectively, if found,
    ! otherwise -1.
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: ustdi, ustdo, ustde
    integer,intent(in),optional :: ubgn,  uend
    integer,intent(in),optional :: levv
    integer ub, ue
    integer jchk
    logical OPND
    character(len=16) :: TA
    character(len=128) :: txt
    integer lv
    integer jerr

    ierr = 0

    lv = choice(lev_verbose, levv)

    ustdi = -1
    ustdo = -1
    ustde = -1

    ub  = max(0, choice(0, ubgn))
    ue  = choice(-1, uend)
    if (ue.lt.0) ue = ub + 10
    do jchk = ub, ue
       if (ierr.eq.0) inquire(UNIT=jchk, IOSTAT=ierr, OPENED=opnd)
       if (ierr.eq.0.and.OPND) then
          inquire(unit=jchk, IOSTAT=ierr, ACTION=TA)
          if (ierr.eq.0) then
             if (TA.eq.'READ') then
                if (ustdi.lt.0) ustdi = jchk
             else if (TA.eq.'WRITE') then
                if (ustde.lt.0) then
                   ustde = jchk
                else if (ustdo.lt.0) then
                   ustdo = jchk
                endif
             endif
          endif
       endif
101    format('stdu:', I0, ' = ', I0, 1x, L, 1x, A)
       if (VCHECK_DEBUG(lv)) then
          write(txt, 101, IOSTAT=jerr) jchk, ierr, OPND, trim(TA)
          call msg_mdl(txt, __MDL__)
       endif
    enddo
    if (ustdo.lt.0.and.ustde.ge.0) then
       ustdo = ustde
       ustde = -1
    endif
    return
  end subroutine brute_force_stdu

!!!_  & health_check_stdu
  subroutine health_check_stdu(ierr, levv, u)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: u
    integer lv, utmp
    character(len=128) :: txt

    ierr = ERR_SUCCESS

    lv   = choice(lev_verbose, levv)
    utmp = choice(-1,u)

101 format('not set: ', A)
    if (uin.lt.0) then
       if (VCHECK_FATAL(lv)) then
          write(txt, 101) 'OPT_STDIN_UNIT'
          call msg_mdl(txt, __MDL__, utmp)
       endif
       ierr = _ERROR(ERR_FAILURE_INIT)
    endif
    if (uout.lt.0) then
       if (VCHECK_FATAL(lv)) then
          write(txt, 101) 'OPT_STDOUT_UNIT'
          call msg_mdl(txt, __MDL__, utmp)
       endif
       ierr = _ERROR(ERR_FAILURE_INIT)
    endif
    if (uerr.lt.0) then
       if (VCHECK_FATAL(lv)) then
          write(txt, 101) 'OPT_STDERR_UNIT'
          call msg_mdl(txt, __MDL__, utmp)
       endif
       ierr = _ERROR(ERR_FAILURE_INIT)
    endif

    return
  end subroutine health_check_stdu

!!!_ + on-demand environment checkers util
!!!_  - note
!       if without MPI:  IROOT, ICOMM ignored
!       if with MPI:
!           null IROOT:    IROOT=0
!           if IROOT < 0:  every rank run checker
!           if IROOT >=0:  rank=IROOT runs, and the result broadcasted
!!!_  & set_mpi
  subroutine set_mpi &
       & (ierr, irank, nrank, iroot, icomm, tag, iru, icu, u, levv)
    use TOUZA_Std_mwe,only: get_ni, get_comm, MPI_COMM_NULL
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
#if OPT_USE_MPI
    use mpi,only: MPI_COMM_SELF
#endif
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: irank, nrank, iroot, icomm
    character(len=*),intent(in)  :: tag
    integer,optional,intent(in)  :: iru, icu
    integer,optional,intent(in)  :: u
    integer,optional,intent(in)  :: levv
    integer utmp
    integer lv

    ierr = 0
    utmp = choice(uout, u)
    lv = choice(lev_verbose, levv)

#if OPT_USE_MPI
    iroot = choice(0, iru)
    if (VCHECK_INFO(lv)) then
       call msg_mdl('(''on-demand env checker: '', A)', tag, __MDL__, utmp)
    endif
    if (iroot.lt.0) then
       icomm = MPI_COMM_SELF
       iroot = 0
       if (VCHECK_INFO(lv)) then
          call msg_mdl('(''on-demand env checker: self '')',  __MDL__, utmp)
       endif
    else
       if (present(icu)) then
          icomm = icu
       else
          if (ierr.eq.0) call get_comm(ierr, icomm)
       endif
    endif
    if (ierr.eq.0) call get_ni(ierr, nrank, irank, icomm)
    if (ierr.eq.0) then
       if (nrank.gt.0.and.iroot.ge.nrank) ierr = _ERROR(ERR_FAILURE_INIT)
    endif
    if (ierr.eq.0) then
       if (VCHECK_INFO(lv)) then
          call msg_mdl('(''on-demand env checker: broadcast '', I0, 1x, I0)',  (/iroot, irank/), __MDL__, utmp)
       endif
    endif
#else
    icomm = MPI_COMM_NULL
    irank = 0
    nrank = 0
    iroot = 0
#endif
    return
  end subroutine set_mpi

!!!_  & health_check_ufd
  subroutine health_check_ufd &
       & (ierr, vm, vi, t, u, levv)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,         intent(inout) :: ierr   ! to keep ierr
    integer,         intent(inout) :: vm
    integer,         intent(in)    :: vi
    character(len=*),intent(in)    :: t
    integer,optional,intent(in)    :: u
    integer,optional,intent(in)    :: levv
    character(len=128) :: buf
    integer lv
    integer jerr

    lv = choice(lev_verbose, levv)
    if (vm.eq.0) then
       ! was not set
    else if (vm.gt.0) then
101    format('conflict in ', A, ': ', I0, 1x, I0)
       if (vm.ne.vi) then
          if (VCHECK_SEVERE(lv)) then
             write(buf, 101, IOSTAT=jerr) trim(t), vm, vi
             call msg_mdl(buf, __MDL__, u)
          endif
       endif
    else if (vm.lt.0) then
102    format('error in ', A, ': ', I0, 1x, I0)
       if (vi.gt.0) then
          if (VCHECK_SEVERE(lv)) then
             write(buf, 102, IOSTAT=jerr) trim(t), vm, vi
             call msg_mdl(buf, __MDL__, u)
          endif
       endif
    endif
    vm = vi
    if (vm.lt.0) ierr = ERR_PANIC
  end subroutine health_check_ufd

!!!_ + unformatted direct access (on-demand)
!!!_  & init_unfmtd_recl - initialize unformatted direct access (user interface)
  subroutine init_unfmtd_recl &
       & (ierr, u, levv, levtry, iroot, icomm)
#if OPT_USE_MPI
    use mpi,only: MPI_INTEGER, MPI_Abort
#  if HAVE_FORTRAN_MPI_MPI_BCAST
    use mpi,only: MPI_Bcast
#  endif
#endif
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: levtry
    integer,intent(in),optional :: iroot, icomm

    integer ir, ic, ik, nr
    integer,parameter :: mb=5
    integer ibuf(mb)

    ierr = ERR_SUCCESS
    nr = 0

    call set_mpi(ierr, ir, nr, ik, ic, 'recl', iroot, icomm, u, levv)
    if (ierr.eq.0) then
       ibuf(1:5) = (/lrecd, lrecf, lreci, lrecl, nb_recl/)
       if (ir.eq.ik .or. ik.lt.0) then
          if (ANY(ibuf(:).eq.0)) then
             call check_recl &
                  & (ierr, ibuf, uout, lfileu, lcharu, levtry)
          endif
       endif
    endif
    if (ierr.eq.0) then
#if OPT_USE_MPI
       if (nr.ge.1) call MPI_Bcast(ibuf, mb, MPI_INTEGER, ik, ic, ierr)
#endif
    endif
    if (ierr.eq.0) then
       call health_check_ufd(ierr, lrecd,   ibuf(1), 'RECL:d', u, levv)
       call health_check_ufd(ierr, lrecf,   ibuf(2), 'RECL:f', u, levv)
       call health_check_ufd(ierr, lreci,   ibuf(3), 'RECL:i', u, levv)
       call health_check_ufd(ierr, lrecl,   ibuf(4), 'RECL:l', u, levv)
       call health_check_ufd(ierr, nb_recl, ibuf(5), 'RECL/B', u, levv)
    endif
    if (ierr.eq.0) then
       nbyti = (nb_recl * lreci)
       nbytl = (nb_recl * lrecl)
       nbytf = (nb_recl * lrecf)
       nbytd = (nb_recl * lrecd)
    endif
#if OPT_USE_MPI
    if (ierr.ne.0) then
       if (nr.ge.1) call MPI_Abort(ic, ERR_PANIC, ierr)
    endif
#endif
    return
  end subroutine init_unfmtd_recl

!!!_  & check_recl
  subroutine check_recl &
       & (ierr, ibuf, u, lbf, lbc, levtry, ksw)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_fun,only: new_unit_tmp, new_unit
    use TOUZA_Std_prc,only: KDBL, KFLT
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: ibuf(*)
    integer,intent(in),optional :: u
    integer,intent(in),optional :: lbf, lbc
    integer,intent(in),optional :: levtry
    integer,intent(in),optional :: ksw       ! brute-force checker choice

    character(len=OPT_PATH_LEN) :: tmpf

    integer lrd, lrf, lri, lrl, lunit
    integer utest
    logical isf
    integer kswi

    ierr = 0

    lrd = -1
    lrf = -1
    lri = -1
    lrl = -1
    lunit = -1

    utest = -1
    isf = choice(0, levtry).gt.0
    kswi = choice(0, ksw)

    lunit = 0
    if (present(lbf).and.present(lbc)) then
       lunit = max(0, lbf) / max(1, lbc)
    endif
    if (lunit.le.0 .or. isf) then
       if (kswi.eq.1) then
          if (utest.lt.0) call new_unit_tmp(utest, tmpf)
          if (utest.lt.0) ierr = _ERROR(ERR_NO_IO_UNIT)
          if (ierr.eq.0) call brute_force_recl_unit_rw(ierr, lunit, utest, tmpf, u)
       else
          if (utest.lt.0) utest = new_unit()
          if (utest.lt.0) ierr = _ERROR(ERR_NO_IO_UNIT)
          if (ierr.eq.0) call brute_force_recl_unit_w(ierr, lunit, utest)
       endif
    endif

    if (ierr.eq.0) then
#     if HAVE_FORTRAN_INQUIRE_IOLENGTH
       if (.NOT.isf) then
          INQUIRE(IOLENGTH=lrd) real(0, KIND=KDBL)
          INQUIRE(IOLENGTH=lrf) real(0, KIND=KFLT)
          INQUIRE(IOLENGTH=lri) int(0, KIND=KI32)
          INQUIRE(IOLENGTH=lrl) int(0, KIND=KI64)
       endif
#     else  /* not HAVE_FORTRAN_INQUIRE_IOLENGTH */
       isf = .true.
#     endif /* not HAVE_FORTRAN_INQUIRE_IOLENGTH */
       if (isf) then
          if (kswi.eq.1) then
             if (utest.lt.0) call new_unit_tmp(utest, tmpf)
             if (utest.lt.0) ierr = _ERROR(ERR_NO_IO_UNIT)
             if (ierr.eq.0) call brute_force_recl_type(ierr, lrd, utest, tmpf, real(0, KIND=KDBL))
             if (ierr.eq.0) call brute_force_recl_type(ierr, lrf, utest, tmpf, real(0, KIND=KFLT))
             if (ierr.eq.0) call brute_force_recl_type(ierr, lri, utest, tmpf, int(0, KIND=KI32))
             if (ierr.eq.0) call brute_force_recl_type(ierr, lrl, utest, tmpf, int(0, KIND=KI64))
          else
             if (utest.lt.0) utest = new_unit()
             if (utest.lt.0) ierr = _ERROR(ERR_NO_IO_UNIT)
             if (ierr.eq.0) call brute_force_recl_type_bs(ierr, lrd, utest, check_single_scratch_d, 8, lunit)
             if (ierr.eq.0) call brute_force_recl_type_bs(ierr, lrf, utest, check_single_scratch_f, 4, lunit)
             if (ierr.eq.0) call brute_force_recl_type_bs(ierr, lri, utest, check_single_scratch_i, 4, lunit)
             if (ierr.eq.0) call brute_force_recl_type_bs(ierr, lrl, utest, check_single_scratch_l, 8, lunit)
          endif
       endif
    endif

    ibuf(1:5) = (/lrd, lrf, lri, lrl, lunit/)

    return
  end subroutine check_recl

!!!_  & brute_force_recl_unit_w - lazy trial to find file storage unit (ii)
  subroutine brute_force_recl_unit_w &
       & (ierr, lunit, utest)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: lunit
    integer,intent(in)  :: utest

    integer lrec
    character(len=*),parameter :: teststr = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    integer j

    ierr = 0
    lunit = -1
    lrec = 1

    if (ierr.eq.0) then
       open(UNIT=utest,           RECL=lrec,       &
            & STATUS='SCRATCH',   ACCESS='DIRECT', &
            & FORM='UNFORMATTED', ACTION='WRITE',  IOSTAT=ierr)
    endif
    if (ierr.eq.0) then
       do j = 1, len(teststr)
          write(UNIT=utest, REC=1, IOSTAT=ierr) teststr(1:j)
          if (ierr.ne.0) then
             lunit = j
             exit
          endif
       enddo
       ierr = 0
       lunit = max(0, lunit) - 1
       if (lunit.gt.0) lunit = lunit / lrec
    endif
    if (ierr.eq.0) close(UNIT=utest, IOSTAT=ierr)

    return
  end subroutine brute_force_recl_unit_w

!!!_  & brute_force_recl_type_bs - lazy trial to find record length for unit type (core ii)
  subroutine brute_force_recl_type_bs &
       & (ierr, lrec, utest, xfunc, nini, lunit)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: lrec
    integer,intent(in)          :: utest
    !!! logical :: xfunc ! logical function(int, int, int)
    integer,intent(in)          :: nini  ! initial guess
    integer,intent(in),optional :: lunit ! unit record length in bytes

    interface
       logical function xfunc(ierr, utest, n)
         implicit none
         integer,intent(out) :: ierr
         integer,intent(in)  :: utest
         integer,intent(in)  :: n
       end function xfunc
    end interface

    logical sccs
    integer lu
    integer ngood, nbad, ntry

    ierr = 0
    lrec = -1

    lu = choice(0, lunit)
    if (lu.le.0) ierr = _ERROR(ERR_INVALID_PARAMETER)
    if (ierr.ne.0) return

    ! find good
    ngood = nini / lu
    do
       sccs = xfunc(ierr, utest, ngood)
       if (ierr.ne.0) exit
       if (sccs) exit
       ngood = ngood * 2
    enddo
    if (ierr.ne.0) return
    if (ngood.le.1) then
       lrec = ngood
       return
    endif
    ! find bad
    nbad = max(1, ngood / 2)
    do
       sccs = xfunc(ierr, utest, nbad)
       if (ierr.ne.0) exit
       if (.not.sccs) exit
       nbad = nbad / 2
       if (nbad.le.0) exit
    enddo
    if (ierr.ne.0) return
    if (nbad.lt.1) then
       lrec = 1
       return
    endif
    do
       ! bisection
       if (nbad+1.ge.ngood) then
          lrec = ngood
          return
       endif
       ntry = (ngood + nbad) / 2
       sccs = xfunc(ierr, utest, ntry)
       if (ierr.ne.0) return
       if (sccs) then
          ngood = ntry
       else
          nbad = ntry
       endif
    enddo
    return
  end subroutine brute_force_recl_type_bs

!!!_  & check_single_scratch () - checker functions
  logical function check_single_scratch_d &
       & (ierr, utest, n) result(sccs)
    use TOUZA_Std_prc, only: KDBL
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: utest
    integer,intent(in)  :: n
    real(kind=KDBL),parameter :: V = 0
    integer jchk

    ierr = 0
    sccs = .false.
    ierr = check_scratch_open(utest, n)
    if (ierr.ne.0) return
    write(utest, REC=1, IOSTAT=jchk) V
    sccs = (jchk.eq.0)
    close(UNIT=utest, STATUS='DELETE', IOSTAT=ierr)
    return
  end function check_single_scratch_d

  logical function check_single_scratch_f &
       & (ierr, utest, n) result(sccs)
    use TOUZA_Std_prc, only: KFLT
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: utest
    integer,intent(in)  :: n
    real(kind=KFLT),parameter :: V = 0
    integer jchk

    ierr = 0
    sccs = .false.
    ierr = check_scratch_open(utest, n)
    if (ierr.ne.0) return
    write(utest, REC=1, IOSTAT=jchk) V
    sccs = (jchk.eq.0)
    close(UNIT=utest, STATUS='DELETE', IOSTAT=ierr)
    return
  end function check_single_scratch_f

  logical function check_single_scratch_i &
       & (ierr, utest, n) result(sccs)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: utest
    integer,intent(in)  :: n
    integer(KIND=KI32),parameter :: V = 0
    integer jchk

    ierr = 0
    sccs = .false.
    ierr = check_scratch_open(utest, n)
    if (ierr.ne.0) return
    write(utest, REC=1, IOSTAT=jchk) V
    sccs = (jchk.eq.0)
    close(UNIT=utest, STATUS='DELETE', IOSTAT=ierr)
    return
  end function check_single_scratch_i

  logical function check_single_scratch_l &
       & (ierr, utest,  n) result(sccs)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: utest
    integer,intent(in)  :: n
    integer(KIND=KI64),parameter :: V = 0
    integer jchk

    ierr = 0
    sccs = .false.
    ierr = check_scratch_open(utest, n)
    if (ierr.ne.0) return
    write(utest, REC=1, IOSTAT=jchk) V
    sccs = (jchk.eq.0)
    close(UNIT=utest, STATUS='DELETE', IOSTAT=ierr)
    return
  end function check_single_scratch_l

!!!_  & check_scratch_open ()
  integer function check_scratch_open &
       & (u, n) result(ierr)
    implicit none
    integer,intent(in) :: u
    integer,intent(in) :: n
    open(UNIT=u, RECL=n, &
         & STATUS='SCRATCH', &
         & ACCESS='DIRECT',  FORM='UNFORMATTED', ACTION='WRITE', IOSTAT=ierr)
    return
  end function check_scratch_open

!!!_  - obsolete checkers (not scratch)
!!!_   & brute_force_recl_unit_rw - lazy trial to find file storage unit (i)
  subroutine brute_force_recl_unit_rw &
       & (ierr, lunit, utest, fn, u)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: lunit
    integer,         intent(in)          :: utest
    character(len=*),intent(in)          :: fn
    integer,         intent(in),optional :: u

    integer lrec
    character(len=*),parameter :: teststr = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=1) :: C

    ierr = 0 * choice(0, u)
    lunit = -1
    ! call check_new_file(ierr, utest, fn, u) ! for safety
    ! if (ierr.ne.0) return

    if (ierr.eq.0) then
       lrec = len(teststr) + 10
       open(UNIT=utest, FILE=fn,  RECL=lrec,       &
            & STATUS='UNKNOWN',   ACCESS='DIRECT', &
            & FORM='UNFORMATTED', ACTION='WRITE',  IOSTAT=ierr)
    endif
    if (ierr.eq.0) write(UNIT=utest, REC=1, IOSTAT=ierr) teststr
    if (ierr.eq.0) close(UNIT=utest, IOSTAT=ierr)
    if (ierr.eq.0) then
       lrec = 1
       open(UNIT=utest, FILE=fn,  RECL=lrec, &
            & STATUS='UNKNOWN',   ACCESS='DIRECT', &
            & FORM='UNFORMATTED', ACTION='READWRITE', IOSTAT=ierr)
    endif
    if (ierr.eq.0) read(UNIT=utest, REC=2, IOSTAT=ierr) C
    if (ierr.eq.0) close(UNIT=utest, STATUS='DELETE', IOSTAT=ierr)
    if (ierr.eq.0) then
       lunit = INDEX(teststr, C) - 1
    endif

    return
  end subroutine brute_force_recl_unit_rw

!!!_   & brute_force_recl_type - lazy trial to find record length for unit type
  subroutine brute_force_recl_type_d &
       & (ierr, lrec, utest, fn, mold, lunit, nini, u)
    use TOUZA_Std_prc, only: KDBL
    use TOUZA_Std_utl, only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: lrec
    integer,         intent(in)          :: utest
    character(len=*),intent(in)          :: fn
    real(kind=KDBL), intent(in)          :: mold  ! dummy placeholder
    integer,         intent(in),optional :: lunit ! unit record length in bytes
    integer,         intent(in),optional :: nini  ! initial guess
    integer,         intent(in),optional :: u

    integer ni
    ni = choice(8, nini) + 0 * KIND(mold)
    call brute_force_recl_type_core &
         & (ierr, lrec, utest, fn, check_single_write_d, ni, lunit, u)

  end subroutine brute_force_recl_type_d

  subroutine brute_force_recl_type_f &
       & (ierr, lrec, utest, fn, mold, lunit, nini, u)
    use TOUZA_Std_prc, only: KFLT
    use TOUZA_Std_utl, only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: lrec
    integer,         intent(in)          :: utest
    character(len=*),intent(in)          :: fn
    real(kind=KFLT), intent(in)          :: mold  ! dummy placeholder
    integer,         intent(in),optional :: lunit ! unit record length in bytes
    integer,         intent(in),optional :: nini  ! initial guess
    integer,         intent(in),optional :: u

    integer ni
    ni = choice(4, nini) + 0 * KIND(mold)
    call brute_force_recl_type_core &
         & (ierr, lrec, utest, fn, check_single_write_f, ni, lunit, u)

  end subroutine brute_force_recl_type_f

  subroutine brute_force_recl_type_i &
       & (ierr, lrec, utest, fn, mold, lunit, nini, u)
    use TOUZA_Std_prc, only: KI32
    use TOUZA_Std_utl, only: choice
    implicit none
    integer,           intent(out)         :: ierr
    integer,           intent(out)         :: lrec
    integer,           intent(in)          :: utest
    character(len=*),  intent(in)          :: fn
    integer(kind=KI32),intent(in)          :: mold  ! dummy placeholder
    integer,           intent(in),optional :: lunit ! unit record length in bytes
    integer,           intent(in),optional :: nini  ! initial guess
    integer,           intent(in),optional :: u

    integer ni
    ni = choice(4, nini) + 0 * KIND(mold)
    call brute_force_recl_type_core &
         & (ierr, lrec, utest, fn, check_single_write_i, ni, lunit, u)

  end subroutine brute_force_recl_type_i

  subroutine brute_force_recl_type_l &
       & (ierr, lrec, utest, fn, mold, lunit, nini, u)
    use TOUZA_Std_prc, only: KI64
    use TOUZA_Std_utl, only: choice
    implicit none
    integer,           intent(out)         :: ierr
    integer,           intent(out)         :: lrec
    integer,           intent(in)          :: utest
    character(len=*),  intent(in)          :: fn
    integer(kind=KI64),intent(in)          :: mold  ! dummy placeholder
    integer,           intent(in),optional :: lunit ! unit record length in bytes
    integer,           intent(in),optional :: nini  ! initial guess
    integer,           intent(in),optional :: u

    integer ni
    ni = choice(8, nini) + 0 * KIND(mold)
    call brute_force_recl_type_core &
         & (ierr, lrec, utest, fn, check_single_write_l, ni, lunit, u)

  end subroutine brute_force_recl_type_l

!!!_   & brute_force_recl_type_core - lazy trial to find record length for unit type (core)
  subroutine brute_force_recl_type_core &
       & (ierr, lrec, utest, fn, xfunc, nini, lunit, u)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: lrec
    integer,         intent(in)          :: utest
    character(len=*),intent(in)          :: fn
    logical                              :: xfunc ! logical function(int, int, char, int)
    integer,         intent(in)          :: nini  ! initial guess
    integer,         intent(in),optional :: lunit ! unit record length in bytes
    integer,         intent(in),optional :: u

    logical sccs
    integer lu
    integer ngood, nbad, ntry

    ierr = 0 * choice(0, u)
    lrec = -1
    ! call check_new_file(ierr, utest, fn, u)
    ! if (ierr.ne.0) return

    lu = choice(0, lunit)
    ! if (lu.le.0) call brute_force_storage_unit(ierr, lu, utest, fn, u)
    if (lu.le.0) ierr = _ERROR(ERR_INVALID_PARAMETER)
    if (ierr.ne.0) return

    ! find good
    ngood = nini / lu
    do
       sccs = xfunc(ierr, utest, fn, ngood)
       if (ierr.ne.0) exit
       if (sccs) exit
       ngood = ngood * 2
    enddo
    if (ierr.ne.0) return
    if (ngood.le.1) then
       lrec = ngood
       return
    endif
    ! find bad
    nbad = max(1, ngood / 2)
    do
       sccs = xfunc(ierr, utest, fn, nbad)
       if (ierr.ne.0) exit
       if (.not.sccs) exit
       nbad = nbad / 2
       if (nbad.le.0) exit
    enddo
    if (ierr.ne.0) return
    if (nbad.lt.1) then
       lrec = 1
       return
    endif
    do
       ! bisection
       if (nbad+1.ge.ngood) then
          lrec = ngood
          return
       endif
       ntry = (ngood + nbad) / 2
       sccs = xfunc(ierr, utest, fn, ntry)
       if (ierr.ne.0) return
       if (sccs) then
          ngood = ntry
       else
          nbad = ntry
       endif
    enddo
    return
  end subroutine brute_force_recl_type_core

!!!_   & check_single_write () - checker functions
  logical function check_single_write_d &
       & (ierr, utest, fn, n) result(sccs)
    use TOUZA_Std_prc, only: KDBL
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: utest
    integer,         intent(in)  :: n
    character(len=*),intent(in)  :: fn
    real(kind=KDBL),parameter :: V = 0
    integer jchk

    ierr = 0
    sccs = .false.
    ierr = check_single_open(utest, n, fn)
    if (ierr.ne.0) return
    write(utest, REC=1, IOSTAT=jchk) V
    sccs = (jchk.eq.0)
    close(UNIT=utest, STATUS='DELETE', IOSTAT=ierr)
    return
  end function check_single_write_d

  logical function check_single_write_f &
       & (ierr, utest, fn, n) result(sccs)
    use TOUZA_Std_prc, only: KFLT
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: utest
    integer,         intent(in)  :: n
    character(len=*),intent(in)  :: fn
    real(kind=KFLT),parameter :: V = 0
    integer jchk

    ierr = 0
    sccs = .false.
    ierr = check_single_open(utest, n, fn)
    if (ierr.ne.0) return
    write(utest, REC=1, IOSTAT=jchk) V
    sccs = (jchk.eq.0)
    close(UNIT=utest, STATUS='DELETE', IOSTAT=ierr)
    return
  end function check_single_write_f

  logical function check_single_write_i &
       & (ierr, utest, fn, n) result(sccs)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: utest
    integer,         intent(in)  :: n
    character(len=*),intent(in)  :: fn
    integer(KIND=KI32),parameter :: V = 0
    integer jchk

    ierr = 0
    sccs = .false.
    ierr = check_single_open(utest, n, fn)
    if (ierr.ne.0) return
    write(utest, REC=1, IOSTAT=jchk) V
    sccs = (jchk.eq.0)
    close(UNIT=utest, STATUS='DELETE', IOSTAT=ierr)
    return
  end function check_single_write_i

  logical function check_single_write_l &
       & (ierr, utest, fn, n) result(sccs)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: utest
    integer,         intent(in)  :: n
    character(len=*),intent(in)  :: fn
    integer(KIND=KI64),parameter :: V = 0
    integer jchk

    ierr = 0
    sccs = .false.
    ierr = check_single_open(utest, n, fn)
    if (ierr.ne.0) return
    write(utest, REC=1, IOSTAT=jchk) V
    sccs = (jchk.eq.0)
    close(UNIT=utest, STATUS='DELETE', IOSTAT=ierr)
    return
  end function check_single_write_l

!!!_   & check_single_open ()
  integer function check_single_open &
       & (u, n, p) result(ierr)
    implicit none
    integer,         intent(in) :: u
    integer,         intent(in) :: n
    character(len=*),intent(in) :: p
    open(UNIT=u, FILE=p,RECL=n, &
         & STATUS='NEW',    &
         & ACCESS='DIRECT', FORM='UNFORMATTED', ACTION='WRITE', IOSTAT=ierr)
    return
  end function check_single_open

!!!_ + stream io unit-length detection (on-demand)
!!!_  & init_unfmtd_strm
  subroutine init_unfmtd_strm &
       & (ierr, u, levv, levtry, iroot, icomm)
#if OPT_USE_MPI
    use mpi,only: MPI_INTEGER, MPI_Abort
#  if HAVE_FORTRAN_MPI_MPI_BCAST
    use mpi,only: MPI_Bcast
#  endif
#endif
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: levtry
    integer,intent(in),optional :: iroot, icomm

    integer ir, nr, ic, ik
    integer,parameter :: mb=1
    integer ibuf(mb)

    ierr = ERR_SUCCESS
    nr = 0

    ! needs nbyt*
    if (ierr.eq.0) call init_unfmtd_recl(ierr, u, levv, levtry, iroot, icomm)

    if (ierr.eq.0) call set_mpi(ierr, ir, nr, ik, ic, 'strm', iroot, icomm, u, levv)
    if (ierr.eq.0) then
       ibuf(1:1) = (/nc_strm/)
       if (ir.eq.ik .or. ik.lt.0) then
          if (ANY(ibuf(:).eq.0)) then
             call check_strm &
                  & (ierr, ibuf, uout, lstrmu, levtry)
          endif
       endif
    endif
    if (ierr.eq.0) then
#if OPT_USE_MPI
       if (nr.ge.1) call MPI_Bcast(ibuf, mb, MPI_INTEGER, ik, ic, ierr)
#endif
    endif
    if (ierr.eq.0) then
       call health_check_ufd(ierr, nc_strm, ibuf(1), 'STRM/B', u, levv)
    endif
    if (ierr.eq.0) then
       mstrmi = nbyti / max(1, nc_strm)
       mstrml = nbytl / max(1, nc_strm)
       mstrmf = nbytf / max(1, nc_strm)
       mstrmd = nbytd / max(1, nc_strm)
    endif
#if OPT_USE_MPI
    if (ierr.ne.0) then
       if (nr.ge.1) call MPI_Abort(ic, ERR_PANIC, ierr)
    endif
#endif
    return
  end subroutine init_unfmtd_strm

!!!_  & check_strm
  subroutine check_strm &
       & (ierr, ibuf, u, lbs, levtry)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_fun,only: new_unit_tmp, new_unit
    use TOUZA_Std_prc,only: KDBL, KFLT
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: ibuf(*)
    integer,intent(in),optional :: u
    integer,intent(in),optional :: lbs
    integer,intent(in),optional :: levtry

    integer lustr
    integer utest
    logical isf

    ierr = 0
    utest = -1
    lustr = -1
    isf = choice(0, levtry).gt.0

    if (ierr.eq.0) then
       lustr = 0
       if (present(lbs)) then
          lustr = max(0, lbs)
       endif
       if (lustr.le.0 .or. isf) then
          if (utest.lt.0) utest = new_unit()
          if (utest.lt.0) ierr = _ERROR(ERR_NO_IO_UNIT)
          if (ierr.eq.0) call brute_force_stream_unit(ierr, lustr, utest, u)
       endif
    endif
    ibuf(1:1) = (/lustr/)
    return
  end subroutine check_strm

!!!_  & brute_force_stream_unit - lazy trial to find file stream i/o unit
  subroutine brute_force_stream_unit &
       & (ierr, lustr, utest, u)
    use TOUZA_Std_log,only: msg_mdl
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: lustr
    integer,intent(in)          :: utest
    integer,intent(in),optional :: u

    integer(kind=KIOFS) :: jposh, jposf
    integer lrec
    character(len=*),parameter :: teststr = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=8) :: CS

    ierr = 0
#if HAVE_FORTRAN_OPEN_STREAM
    if (ierr.ne.0) return
#   if HAVE_FORTRAN_INQUIRE_POS
    if (ierr.eq.0) then
       open(UNIT=utest, &
            & STATUS='SCRATCH',   ACCESS='STREAM', &
            & FORM='UNFORMATTED', ACTION='WRITE',  IOSTAT=ierr)
    endif
    if (ierr.eq.0) INQUIRE(UNIT=utest, POS=jposh, IOSTAT=ierr)
    if (ierr.eq.0) then
       lrec = len(CS)
       CS = teststr(1:lrec)
       write(UNIT=utest, IOSTAT=ierr) CS
    endif
    if (ierr.eq.0) INQUIRE(UNIT=utest, POS=jposf, IOSTAT=ierr)
    if (ierr.eq.0) then
       if (jposf.gt.jposh) then
          lustr = lrec / int(jposf - jposh, kind=kind(lustr))
       endif
    endif
    if (ierr.eq.0) close(UNIT=utest, STATUS='DELETE', IOSTAT=ierr)
    if (ierr.ne.0) then
       call msg_mdl('stream i/o checker failed', __MDL__, u)
    endif
#   else  /* not HAVE_FORTRAN_INQUIRE_POS */
    call msg_mdl('inquire pos disabled', __MDL__, u)
#   endif /* not HAVE_FORTRAN_INQUIRE_POS */
#else /* not HAVE_FORTRAN_OPEN_STREAM */
    ierr = _ERROR(ERR_OPR_DISABLE)
    call msg_mdl('stream access unavailable', __MDL__, u)
#endif /* not HAVE_FORTRAN_OPEN_STREAM */
    return
  end subroutine brute_force_stream_unit

!!!_ + byte-order (auto for memory, on-demand for files)
!!!_  & init_file_bodr
  subroutine init_file_bodr &
       & (ierr, u, levv, ubgn, uend, ustp, iroot, icomm)
#if OPT_USE_MPI
    use mpi,only: MPI_INTEGER, MPI_Abort
#  if HAVE_FORTRAN_MPI_MPI_BCAST
    use mpi,only: MPI_Bcast
#  endif
#endif
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: ubgn,  uend, ustp
    integer,intent(in),optional :: iroot, icomm

    integer ir, nr, ic, ik
    integer,parameter :: mb=1
    integer ibuf(mb)

    ierr = 0
    nr = 0
    call set_mpi(ierr, ir, nr, ik, ic, 'bodr', iroot, icomm, u, levv)

    if (ierr.eq.0) then
       ibuf(1:1) = (/kendi_file/)
       call check_bodr_files(ierr, ibuf(1), u, levv, ubgn, uend, ustp)
    endif
    if (ierr.eq.0) then
#if OPT_USE_MPI
       if (nr.ge.1) call MPI_Bcast(ibuf, mb, MPI_INTEGER, ik, ic, ierr)
#endif
    endif
    if (ierr.eq.0) then
       call health_check_ufd(ierr, kendi_file, ibuf(1), 'BODR', u, levv)
    endif
#if OPT_USE_MPI
    if (ierr.ne.0) then
       if (nr.ge.1) call MPI_Abort(ic, ERR_PANIC, ierr)
    endif
#endif
    return
  end subroutine init_file_bodr

!!!_  & check_bodr_files - check byte-order (files)
  subroutine check_bodr_files &
       & (ierr, KENDI, u, levv, ubgn, uend, ustp)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    use TOUZA_Std_fun,only: new_unit
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: kendi
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: ubgn, uend, ustp
    integer ju
    integer ub, ue, us
    integer ul, lv
    integer lrec
    logical opnd
    integer kcur

    ierr = 0
    lv = choice(lev_verbose, levv)
    ul = choice(-1, u)

    kendi = endian_UNKNOWN

    if (ierr.eq.0) call set_endian_tester(ierr)

    ub = choice(-1, ubgn)
    ue = ub - 1
    us = 1
    if (ub.lt.0) ub = new_unit()
    if (ub.lt.0) ierr = _ERROR(ERR_NO_IO_UNIT)
    if (ierr.eq.0) then
       ue = choice(-1, uend)
       if (present(ubgn)) then
          ue = max(ub, uend)
       else
          ue = max(ub, ub + ue)
       endif
       us = max(1, choice(-1, ustp))
    endif
    do ju = ub, ue, us
       if (ierr.eq.0) inquire(UNIT=ju, IOSTAT=ierr, OPENED=OPND)
       if (ierr.eq.0) then
          if (OPND) cycle
          lrec = 32
          open(UNIT=ju,  RECL=lrec, &
               & ACCESS='DIRECT',  FORM='UNFORMATTED', &
               & STATUS='SCRATCH', ACTION='READWRITE', IOSTAT=ierr)
       endif
       if (ierr.eq.0) call check_bodr_unit(ierr, kcur, ju, jrec=1)
       if (ierr.eq.0) then
          if (VCHECK_DETAIL(lv)) then
             call msg_mdl &
                  & ('(''endianness:'', I0, 1x, I0)', (/ju, kcur/), __MDL__, ul)
          endif
          if (kendi.eq.endian_UNKNOWN) then
             kendi = kcur
          else if (kendi.ne.kcur) then
             if (VCHECK_SEVERE(lv)) then
                call msg_mdl &
                     & ('(''endianness incompatible:'', I0, 1x, I0)', (/ju-us, kendi/), __MDL__, ul)
                call msg_mdl &
                     & ('(''endianness incompatible:'', I0, 1x, I0)', (/ju,    kcur/), __MDL__, ul)
             endif
             kendi = endian_OTHER
          endif
       endif
       if (ierr.eq.0) close(UNIT=ju, STATUS='DELETE', IOSTAT=ierr)
    enddo

    return
  end subroutine check_bodr_files

!!!_  & check_bodr_unit - check byte-order (single unit)
  ! file positioning must be managed by caller
  subroutine check_bodr_unit &
    & (ierr, kendi, utest, jrec, u, levv)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: kendi
    integer,intent(in)          :: utest  ! test unit (must be opened)
    integer,intent(in),optional :: jrec   ! direct>0  stream==0  sequential<0
    integer,intent(in),optional :: u      ! log unit
    integer,intent(in),optional :: levv
    integer rec
    integer ul, lv
    integer(kind=KI32)  :: TI
    integer(kind=KIOFS) :: jend
    ierr = 0
    kendi = endian_ERROR
    lv = choice(lev_verbose, levv)
    ul = choice(-1, u)

    rec = choice(0, jrec)
    if (rec.gt.0) then
       ! direct access (test at current, keep test)
       if (ierr.eq.0) write(UNIT=utest, IOSTAT=ierr, REC=rec) etest_org
       if (ierr.eq.0) read(UNIT=utest,  IOSTAT=ierr, REC=rec) TI
    else if (rec.eq.0) then
       ! stream access (test at final, truncate)
       if (ierr.eq.0) inquire(UNIT=utest, IOSTAT=ierr, SIZE=jend)
       if (ierr.eq.0) write(UNIT=utest, IOSTAT=ierr, POS=jend+1) etest_org
       if (ierr.eq.0) read(UNIT=utest,  IOSTAT=ierr, POS=jend+1) TI
       if (ierr.eq.0) write(UNIT=utest, IOSTAT=ierr, POS=jend+1)
       if (ierr.eq.0) endfile(UNIT=utest, IOSTAT=ierr)
    else
       ! sequential access (test at current, truncate there)
       if (ierr.eq.0) write(UNIT=utest, IOSTAT=ierr) etest_org
       if (ierr.eq.0) backspace(UNIT=utest,  IOSTAT=ierr)
       if (ierr.eq.0) read(UNIT=utest,  IOSTAT=ierr) TI
       if (ierr.eq.0) backspace(UNIT=utest,  IOSTAT=ierr)
    endif
    if (ierr.eq.0) then
       if (TI.eq.etest_little) then
          kendi = endian_LITTLE
       else if (TI.eq.etest_big) then
          kendi = endian_BIG
       endif
    else
       if (VCHECK_SEVERE(lv)) then
          call msg_mdl('(''endianness(unit) failed = '', I0, 1x, I0)', (/ierr, utest/), __MDL__, ul)
       endif
    endif
  end subroutine check_bodr_unit

!!!_  & check_bodr_mem - check byte-order (memory)
  subroutine check_bodr_mem &
       & (ierr, KENDI, u, levv)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: kendi
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer ul
    integer,parameter :: L = 4
    character(len=1) :: C(L)
    character(len=L) :: T
    integer lv

    ierr = 0
    lv = choice(lev_verbose, levv)
    ul = choice(-1, u)

    write(C, '(4A)', IOSTAT=ierr) ' '
    if (ierr.eq.0) then
       C = transfer(1684234849, C, L)
       write(T, '(4A)', IOSTAT=ierr) C
    endif
    if (ierr.eq.0) then
       if (T(1:4).eq.'abcd') then
          kendi = endian_LITTLE
       else if (T(1:4).eq.'dcba') then
          kendi = endian_BIG
       else
          ierr = _ERROR(ERR_PANIC)
       endif
    endif
    if (ierr.ne.0) then
       ierr = _ERROR(ERR_PANIC)
       kendi = endian_ERROR
       if (VCHECK_SEVERE(lv)) then
          call msg_mdl &
               & ('(''check endianness = '', A)', T, __MDL__, ul)
       endif
    endif
    return
  end subroutine check_bodr_mem

!!!_  & check_byte_order - check byte-order (interface, file)
  subroutine check_byte_order &
       & (ierr, KENDI, utest, force, u, levv)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: kendi
    integer,intent(in)          :: utest
    logical,intent(in),optional :: force
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv

    integer ul, lv
    integer lrec
    logical opnd

    logical bf

    ierr = 0
    bf = choice(.FALSE., force)

    if ((kendi_file.eq.endian_BIG .or. kendi_file.eq.endian_LITTLE) &
         & .and. (.not.bf)) then
       kendi = kendi_file
       return
    endif

    lv = choice(lev_verbose, levv)
    ul = choice(-1, u)

    kendi = endian_UNKNOWN

    if (ierr.eq.0) call set_endian_tester(ierr)

    if (ierr.eq.0) inquire(UNIT=utest, IOSTAT=ierr, OPENED=OPND)
    if (ierr.eq.0) then
       if (OPND) then
          ierr = ERR_PANIC
          return
       endif
    endif
    if (ierr.eq.0) then
       lrec = 32
       open(UNIT=utest,  RECL=lrec, &
            & ACCESS='DIRECT',  FORM='UNFORMATTED', &
            & STATUS='SCRATCH', ACTION='READWRITE', IOSTAT=ierr)
    endif
    if (ierr.eq.0) call check_bodr_unit(ierr, kendi, utest, jrec=1)
    if (ierr.eq.0) close(UNIT=utest, STATUS='DELETE', IOSTAT=ierr)
    if (ierr.eq.0) then
       if (VCHECK_DETAIL(lv)) then
          call msg_mdl &
               & ('(''endianness:'', I0, 1x, I0)', (/utest, kendi/), __MDL__, ul)
       endif
       if (kendi.ne.kendi_file) then
          if (VCHECK_SEVERE(lv)) then
             call msg_mdl &
                  & ('(''endianness incompatible:'', I0, 1x, I0)', (/utest, kendi/), __MDL__, ul)
          endif
       endif
    endif

    return
  end subroutine check_byte_order

!!!_ + io status (on-demand)
!!!_  & init_io_status
  subroutine init_io_status &
       & (ierr, u, levv, iroot, icomm)
#if OPT_USE_MPI
    use mpi,only: MPI_INTEGER, MPI_Abort
#  if HAVE_FORTRAN_MPI_MPI_BCAST
    use mpi,only: MPI_Bcast
#  endif
#endif
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: iroot, icomm

    integer ir, nr, ic, ik
    integer,parameter :: mb=1
    integer ibuf(mb)

    ierr = ERR_SUCCESS
    nr = 0

    call set_mpi(ierr, ir, nr, ik, ic, 'ios', iroot, icomm, u, levv)
    if (ierr.eq.0) then
       ibuf(1:1) = (/err_eof/)
       if (ir.eq.ik .or. ik.lt.0) then
          if (ANY(ibuf(:).eq.0)) then
             call check_eof(ierr, ibuf(1), u, levv)
          endif
       endif
    endif
    if (ierr.eq.0) then
#if OPT_USE_MPI
       if (nr.ge.1) call MPI_Bcast(ibuf, mb, MPI_INTEGER, ik, ic, ierr)
#endif
    endif
    if (ierr.eq.0) err_eof = ibuf(1)
#if OPT_USE_MPI
    if (ierr.ne.0) then
       if (nr.ge.1) call MPI_Abort(ic, ERR_PANIC, ierr)
    endif
#endif
    return
  end subroutine init_io_status

!!!_  & check_eof
  subroutine check_eof &
       & (ierr, keof, u, levv)
    use TOUZA_Std_log,only: msg_mdl
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_fun,only: new_unit
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(inout)       :: keof
    integer,intent(in),optional :: u, levv

    integer utest
    integer j
    integer jerr_eof
    integer lv
    character(len=128) :: txt
    integer jerr

    ierr = 0
    lv = choice(lev_verbose, levv)

    jerr_eof = 0
    utest = new_unit()
    if (utest.lt.0) ierr = _ERROR(ERR_NO_IO_UNIT)
    if (ierr.eq.0) then
       open(UNIT=utest, &
            & STATUS='SCRATCH',   ACCESS='SEQUENTIAL', &
            & FORM='UNFORMATTED', ACTION='READWRITE',  IOSTAT=ierr)
    endif
    if (ierr.eq.0) write(UNIT=utest, IOSTAT=ierr) 0
    if (ierr.eq.0) rewind(UNIT=utest, IOSTAT=ierr)
    if (ierr.eq.0) read(UNIT=utest, IOSTAT=ierr)
    if (ierr.eq.0) then
       read(UNIT=utest, IOSTAT=jerr_eof) j
       close(UNIT=utest, IOSTAT=ierr)
    endif
    if (ierr.ne.0) then
       if (VCHECK_NORMAL(lv)) then
101       format('eof check failed = ', I0)
          write(txt, 101, IOSTAT=jerr) ierr
          call msg_mdl(txt, __MDL__, u)
       endif
    else if (keof.eq.0) then
       if (jerr_eof.eq.0) then
          if (VCHECK_NORMAL(lv)) then
             call msg_mdl('eof not detected', __MDL__, u)
          endif
       else
          keof = jerr_eof
102       format('eof detected = ', I0)
          if (VCHECK_DEBUG(lv)) then
             write(txt, 102, IOSTAT=jerr) jerr_eof
             call msg_mdl(txt, __MDL__, u)
          endif
       endif
    else if (keof.ne.jerr_eof) then
       keof = jerr_eof
103    format('eof ignored = ', I0)
       if (VCHECK_NORMAL(lv)) then
          write(txt, 103, IOSTAT=jerr) jerr_eof
          call msg_mdl(txt, __MDL__, u)
       endif
    else
104    format('eof kept = ', I0)
       if (VCHECK_DEBUG(lv)) then
          write(txt, 104, IOSTAT=jerr) jerr_eof
          call msg_mdl(txt, __MDL__, u)
       endif
    endif
    return
  end subroutine check_eof

!!!_  & is_eof_ss() - check if iostat is eof (stream or sequential)
  logical function is_eof_ss(e) result(b)
    implicit none
    integer,intent(in) :: e

    !!! force check even if err_eof is not detected
    if (err_eof.eq.0) then
       b = e.lt.0               ! eor and eof are not distinguished
    else
       b = e.eq.err_eof
    endif
    return
    ! integer jerr
    ! jerr = err_default
    ! if (jerr.eq.0) then
    !    b = (e.eq.err_eof) .and. (e.ne.0)
    ! else
    !    b = .false.
    ! endif
  end function is_eof_ss

!!!_ + conversion
!!!_  & conv_b2strm - convert bytes to sizes in stream units
  PURE &
  integer(KIND=KTGT) function conv_b2strm_i(nb) result (ms)
    use TOUZA_Std_prc,only: KTGT=>KI32
    implicit none
    integer(KIND=KTGT),intent(in) :: nb
    ms = nb / nc_strm
  end function conv_b2strm_i
  PURE &
  integer(KIND=KTGT) function conv_b2strm_l(nb) result (ms)
    use TOUZA_Std_prc,only: KTGT=>KI64
    implicit none
    integer(KIND=KTGT),intent(in) :: nb
    ms = nb / nc_strm
  end function conv_b2strm_l
!!!_ + queries
!!!_  & get_unit_strm - get unit length in stream i/o
  PURE &
  integer function get_unit_strm_i (mold) result(l)
    use TOUZA_Std_prc,only: KTGT=>KI32
    implicit none
    integer(KIND=KTGT),intent(in) :: mold
    l = mstrmi + 0 * KIND(mold)
  end function get_unit_strm_i
  PURE &
  integer function get_unit_strm_l (mold) result(l)
    use TOUZA_Std_prc,only: KTGT=>KI64
    implicit none
    integer(KIND=KTGT),intent(in) :: mold
    l = mstrml + 0 * KIND(mold)
  end function get_unit_strm_l
  PURE &
  integer function get_unit_strm_f(mold) result(l)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    real(KIND=KTGT),intent(in) :: mold
    l = mstrmf + 0 * KIND(mold)
  end function get_unit_strm_f
  PURE &
  integer function get_unit_strm_d(mold) result(l)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    real(KIND=KTGT),intent(in) :: mold
    l = mstrmd + 0 * KIND(mold)
  end function get_unit_strm_d
  PURE &
  integer function get_unit_strm_a(mold) result(l)
    implicit none
    character(len=*),intent(in) :: mold
    l = conv_b2strm(get_size_bytes(mold, 1))
  end function get_unit_strm_a

!!!_  & get_size_strm - get total length in stream i/o
  PURE &
  integer(kind=KMEM) function get_size_strm_i (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KI32,KMEM=>KI32
    implicit none
    integer(KIND=KTGT),intent(in)          :: mold
    integer(KIND=KMEM),intent(in),optional :: n
    l = mstrmi * choice(1_KMEM, n)  + 0 * KIND(mold)
  end function get_size_strm_i
  PURE &
  integer(kind=KMEM) function get_size_strm_l (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KI64,KMEM=>KI32
    implicit none
    integer(KIND=KTGT),intent(in)          :: mold
    integer(KIND=KMEM),intent(in),optional :: n
    l = mstrml * choice(1_KMEM, n)  + 0 * KIND(mold)
  end function get_size_strm_l
  PURE &
  integer(kind=KMEM) function get_size_strm_f (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KFLT,KMEM=>KI32
    implicit none
    real(KIND=KTGT),   intent(in)          :: mold
    integer(KIND=KMEM),intent(in),optional :: n
    l = mstrmf * choice(1_KMEM, n)  + 0 * KIND(mold)
  end function get_size_strm_f
  PURE &
  integer(kind=KMEM) function get_size_strm_d (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KDBL,KMEM=>KI32
    implicit none
    real(KIND=KTGT),   intent(in)          :: mold
    integer(KIND=KMEM),intent(in),optional :: n
    l = mstrmd * choice(1_KMEM, n)  + 0 * KIND(mold)
  end function get_size_strm_d
  PURE &
  integer(kind=KMEM) function get_size_strm_a (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KMEM=>KI32
    implicit none
    character(len=*),  intent(in)          :: mold
    integer(KIND=KMEM),intent(in),optional :: n
    l = (len(mold) / max(1, nc_strm)) * choice(1_KMEM, n)
  end function get_size_strm_a
  PURE &
  integer(kind=KMEM) function get_size_strm_li (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KI32,KMEM=>KI64
    implicit none
    integer(KIND=KTGT),intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: n
    l = mstrmi * choice(1_KMEM, n)  + 0 * KIND(mold)
  end function get_size_strm_li
  PURE &
  integer(kind=KMEM) function get_size_strm_ll (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KI64,KMEM=>KI64
    implicit none
    integer(KIND=KTGT),intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: n
    l = mstrml * choice(1_KMEM, n)  + 0 * KIND(mold)
  end function get_size_strm_ll
  PURE &
  integer(kind=KMEM) function get_size_strm_lf (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KFLT,KMEM=>KI64
    implicit none
    real(KIND=KTGT),   intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: n
    l = mstrmf * choice(1_KMEM, n)  + 0 * KIND(mold)
  end function get_size_strm_lf
  PURE &
  integer(kind=KMEM) function get_size_strm_ld (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KDBL,KMEM=>KI64
    implicit none
    real(KIND=KTGT),   intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: n
    l = mstrmd * choice(1_KMEM, n)  + 0 * KIND(mold)
  end function get_size_strm_ld
  PURE &
  integer(kind=KMEM) function get_size_strm_la (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KMEM=>KI64
    implicit none
    character(len=*),  intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: n
    l = (len(mold) / max(1, nc_strm)) * choice(1_KMEM, n)
  end function get_size_strm_la

!!!_  & get_size_bytes - get unit/total length in bytes
  PURE &
  integer(kind=KMEM) function get_size_bytes_a (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KMEM=>KI32
    implicit none
    character(len=*),  intent(in)          :: mold
    integer(KIND=KMEM),intent(in),optional :: n
    !! todo: nbyta detection
    l = len(mold) * choice(1_KMEM, n)  + 0 * KIND(mold)
  end function get_size_bytes_a
  PURE &
  integer(kind=KMEM) function get_size_bytes_i (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KI32,KMEM=>KI32
    implicit none
    integer(KIND=KTGT),intent(in)          :: mold
    integer(KIND=KMEM),intent(in),optional :: n
    l = nbyti * choice(1_KMEM, n)  + 0 * KIND(mold)
  end function get_size_bytes_i
  PURE &
  integer(kind=KMEM) function get_size_bytes_l (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KI64,KMEM=>KI32
    implicit none
    integer(KIND=KTGT),intent(in)          :: mold
    integer(KIND=KMEM),intent(in),optional :: n
    l = nbytl * choice(1_KMEM, n)  + 0 * KIND(mold)
  end function get_size_bytes_l
  PURE &
  integer(kind=KMEM) function get_size_bytes_f (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KFLT,KMEM=>KI32
    implicit none
    real(KIND=KTGT),   intent(in)          :: mold
    integer(KIND=KMEM),intent(in),optional :: n
    l = nbytf * choice(1_KMEM, n) + 0 * KIND(mold)
  end function get_size_bytes_f
  PURE &
  integer(kind=KMEM) function get_size_bytes_d (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KDBL,KMEM=>KI32
    implicit none
    real(KIND=KTGT),   intent(in)          :: mold
    integer(KIND=KMEM),intent(in),optional :: n
    l = nbytd * choice(1_KMEM, n)  + 0 * KIND(mold)
  end function get_size_bytes_d

  PURE &
  integer(kind=KMEM) function get_size_bytes_la (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KMEM=>KI64
    implicit none
    character(len=*),  intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: n
    !! todo: nbyta detection
    l = len(mold) * n  + 0 * KIND(mold)
  end function get_size_bytes_la
  PURE &
  integer(kind=KMEM) function get_size_bytes_li (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KI32,KMEM=>KI64
    implicit none
    integer(KIND=KTGT),intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: n
    l = nbyti * n  + 0 * KIND(mold)
  end function get_size_bytes_li
  PURE &
  integer(kind=KMEM) function get_size_bytes_ll (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KI64,KMEM=>KI64
    implicit none
    integer(KIND=KTGT),intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: n
    l = nbytl * n  + 0 * KIND(mold)
  end function get_size_bytes_ll
  PURE &
  integer(kind=KMEM) function get_size_bytes_lf (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KFLT,KMEM=>KI64
    implicit none
    real(KIND=KTGT),   intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: n
    l = nbytf * n + 0 * KIND(mold)
  end function get_size_bytes_lf
  PURE &
  integer(kind=KMEM) function get_size_bytes_ld (mold, n) result(l)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KDBL,KMEM=>KI64
    implicit none
    real(KIND=KTGT),   intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: n
    l = nbytd * n  + 0 * KIND(mold)
  end function get_size_bytes_ld

!!!_  & get_mems_bytes - get members from byte-length
  PURE &
  integer(kind=KMEM) function get_mems_bytes_a (l, mold) result(n)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KMEM=>KI32
    implicit none
    character(len=*),  intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: l
    !! todo: nbyta detection
    n = l / len(mold)   + 0 * KIND(mold)
  end function get_mems_bytes_a
  PURE &
  integer(kind=KMEM) function get_mems_bytes_i (l, mold) result(n)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KI32,KMEM=>KI32
    implicit none
    integer(KIND=KTGT),intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: l
    n = l / nbyti  + 0 * KIND(mold)
  end function get_mems_bytes_i
  PURE &
  integer(kind=KMEM) function get_mems_bytes_l (l, mold) result(n)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KI64,KMEM=>KI32
    implicit none
    integer(KIND=KTGT),intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: l
    n = l / nbytl  + 0 * KIND(mold)
  end function get_mems_bytes_l
  PURE &
  integer(kind=KMEM) function get_mems_bytes_f (l, mold) result(n)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KFLT,KMEM=>KI32
    implicit none
    real(KIND=KTGT),   intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: l
    n = l / nbytf  + 0 * KIND(mold)
  end function get_mems_bytes_f
  PURE &
  integer(kind=KMEM) function get_mems_bytes_d (l, mold) result(n)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KDBL,KMEM=>KI32
    implicit none
    real(KIND=KTGT),   intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: l
    n = l / nbytd  + 0 * KIND(mold)
  end function get_mems_bytes_d

  PURE &
  integer(kind=KMEM) function get_mems_bytes_la (l, mold) result(n)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KMEM=>KI64
    implicit none
    character(len=*),  intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: l
    !! todo: nbyta detection
    n = l / len(mold)   + 0 * KIND(mold)
  end function get_mems_bytes_la
  PURE &
  integer(kind=KMEM) function get_mems_bytes_li (l, mold) result(n)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KI32,KMEM=>KI64
    implicit none
    integer(KIND=KTGT),intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: l
    n = l / nbyti  + 0 * KIND(mold)
  end function get_mems_bytes_li
  PURE &
  integer(kind=KMEM) function get_mems_bytes_ll (l, mold) result(n)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KI64,KMEM=>KI64
    implicit none
    integer(KIND=KTGT),intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: l
    n = l / nbytl  + 0 * KIND(mold)
  end function get_mems_bytes_ll
  PURE &
  integer(kind=KMEM) function get_mems_bytes_lf (l, mold) result(n)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KFLT,KMEM=>KI64
    implicit none
    real(KIND=KTGT),   intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: l
    n = l / nbytf  + 0 * KIND(mold)
  end function get_mems_bytes_lf
  PURE &
  integer(kind=KMEM) function get_mems_bytes_ld (l, mold) result(n)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_prc,only: KTGT=>KDBL,KMEM=>KI64
    implicit none
    real(KIND=KTGT),   intent(in) :: mold
    integer(KIND=KMEM),intent(in) :: l
    n = l / nbytd  + 0 * KIND(mold)
  end function get_mems_bytes_ld

!!!_  & get_size_ufd - get unit length in direct i/o
  PURE &
  integer function get_size_ufd_d (mold) result(l)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),intent(in) :: mold
    l = lrecd + 0 * kind(mold)
  end function get_size_ufd_d
  PURE &
  integer function get_size_ufd_f (mold) result(l)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    real(kind=KTGT),intent(in) :: mold
    l = lrecf + 0 * kind(mold)
  end function get_size_ufd_f
  PURE &
  integer function get_size_ufd_i (mold) result(l)
    use TOUZA_Std_prc,only: KTGT=>KI32
    implicit none
    integer(KIND=KTGT),intent(in) :: mold
    l = lreci + 0 * kind(mold)
  end function get_size_ufd_i
  PURE &
  integer function get_size_ufd_l (mold) result(l)
    use TOUZA_Std_prc,only: KTGT=>KI64
    implicit none
    integer(KIND=KTGT),intent(in) :: mold
    l = lreci + 0 * kind(mold)
  end function get_size_ufd_l

!!!_ + system procedures wrapper
!!!_  & get_login_name
  subroutine get_login_name &
       & (ierr, name)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: name
#if HAVE_FORTRAN_GETLOG
    ierr = 0
    call GETLOG(name)
#else
    ierr = 0
    name = ' '
#endif
  end subroutine get_login_name
!!!_  & get_host_name
  subroutine get_host_name &
       & (ierr, name)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: name
#if HAVE_FORTRAN_HOSTNM
    ierr = 0
    call HOSTNM(name)
#else
    ierr = 0
    name = ' '
#endif
  end subroutine get_host_name
!!!_  & get_env_var
!!!_ + end TOUZA_Std_env
end module TOUZA_Std_env

!!!_@ test_std_env - test program
#if TEST_STD_ENV
program test_std_env
  use TOUZA_Std_prc, only: KDBL, KFLT
  use TOUZA_Std_env
  implicit none
  integer ierr
  integer ui, uo, ue
  integer ut
  integer kendi
  character(len=128) :: str

  ierr = 0
  ut=10

  call init(ierr)
  if (ierr.eq.0) call diag(ierr, levv=+99)
  if (ierr.eq.0) then
     call brute_force_stdu(ierr, ui, uo, ue, levv=1024)
     write(*, *) 'STD = ', ui, uo, ue
  endif
  if (ierr.eq.0) then
     call init_unfmtd_recl(ierr, uo, levv=1024, levtry=16)
  endif
  if (ierr.eq.0) then
     call init_unfmtd_strm(ierr, uo, levv=1024, levtry=16)
  endif

  kendi = endian_ERROR
  if (ierr.eq.0) then
     call check_bodr_mem (ierr, kendi, uo, +10)
     write(*, *) 'ENDIANNESS(mem) = ', kendi, ierr
  endif
  if (ierr.eq.0) then
     call init_file_bodr (ierr, u=uo, levv=+10, ubgn=10, uend=20, ustp=3)
  endif
  kendi = endian_ERROR
  if (ierr.eq.0) then
     do ut = 10, 20, 1
        if (ierr.eq.0) call check_byte_order(ierr, kendi, ut)
        write(*, *) 'ENDIANNESS(unit) = ', ut, kendi, ierr
     enddo
  endif

  if (ierr.eq.0) call get_login_name(ierr, str)
  if (ierr.eq.0) write(*, *) 'login = ', trim(str)
  if (ierr.eq.0) call get_host_name(ierr, str)
  if (ierr.eq.0) write(*, *) 'host = ', trim(str)

  if (ierr.eq.0) call diag(ierr, mode=MODE_FORCE+MODE_SHALLOW, levv=+10)
  if (ierr.eq.0) call finalize(ierr, levv=+10)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_std_env

#endif /* TEST_STD_ENV */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

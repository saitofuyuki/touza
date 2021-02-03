!!!_! std_env.F90 - touza/std standard environments
! Maintainer: SAITO Fuyuki
! Created: May 30 2020
#define TIME_STAMP 'Time-stamp: <2021/02/03 09:07:03 fuyuki std_env.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2020, 2021
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
#ifndef   OPT_CHAR_STORAGE_BITS
#  define OPT_CHAR_STORAGE_BITS 0  /* character storage unit in BITS */
#endif
!!!_@ TOUZA_Std_env - standard environments
module TOUZA_Std_env
!!!_ = declaration
!!!_  - ISO_FORTRAN_ENV module
#if HAVE_ISO_FORTRAN_ENV
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

#  if OPT_FILE_STORAGE_BITS
#    warning "Force to use OPT_FILE_STORAGE_BITS"
#  else
#    undef  OPT_FILE_STORAGE_BITS
#    define OPT_FILE_STORAGE_BITS FILE_STORAGE_SIZE
#  endif

#  if OPT_CHAR_STORAGE_BITS
#    warning "Force to use OPT_CHAR_STORAGE_BITS"
#  else
#    undef  OPT_CHAR_STORAGE_BITS
#    define OPT_CHAR_STORAGE_BITS CHARACTER_STORAGE_SIZE
#  endif

#endif /* HAVE_ISO_FORTRAN_ENV */
!!!_  - ISO_C_BINDING module
#if HAVE_ISO_C_BINDING
#endif /* HAVE_ISO_C_BINDING */
!!!_  - default
  implicit none
  private
!!!_  - parameters
# define __MDL__ 'env'

  integer,save,public :: uin  = OPT_STDIN_UNIT
  integer,save,public :: uout = OPT_STDOUT_UNIT
  integer,save,public :: uerr = OPT_STDERR_UNIT
  integer,save,public :: lbrec = 0                       ! unit record length in bytes
  integer,save,public :: lreci = 0, lrecf = 0, lrecd = 0 ! record lengths of single integer/float/double
!!!_  - static
  integer,save :: lfileu = OPT_FILE_STORAGE_BITS
  integer,save :: lcharu = OPT_CHAR_STORAGE_BITS

  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL
!!!_  - publicG
  public init, diag, finalize
  public brute_force_std_units
  public check_storage_units
  public brute_force_storage_unit
  public brute_force_recl
  public get_rlu, get_rlb

!!!_  - interfaces
  interface brute_force_recl
     module procedure brute_force_recl_d
     module procedure brute_force_recl_f
     module procedure brute_force_recl_i
  end interface brute_force_recl

  interface get_rlu
     module procedure get_rlu_d
     module procedure get_rlu_f
     module procedure get_rlu_i
  end interface get_rlu

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, levtry, levv, mode)
    use TOUZA_Std_utl,only: utl_init=>init, choice
    use TOUZA_Std_log,only: log_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: levtry ! brute-force level
    integer,intent(in),optional :: levv   ! verbose level
    integer,intent(in),optional :: mode   ! initialization flag

    integer md, lv

    ierr = 0

    lv = choice(lev_verbose, levv)
    md = choice(INIT_DEFAULT, mode)
    if (md.eq.INIT_DEFAULT) md = INIT_DEEP

    if (md.gt.INIT_DEFAULT) then
       if (md.ge.INIT_DEEP) then
          if (ierr.eq.0) call utl_init(ierr, levv=lv, mode=md)
          if (ierr.eq.0) call log_init(ierr, levv=lv, mode=md)
       endif
       if (init_counts.eq.0) then
          lev_verbose = lv
          if (ierr.eq.0) then
             call check_std_units (ierr, uin, uout, uerr, lev_verbose, levtry)
          endif
          if (ierr.eq.0) then
             call check_storage_units &
                  & (ierr, lrecd, lrecf, lreci, lbrec, uout, lfileu, lcharu, levtry)
          endif
          if (ierr.eq.0) call health_check(ierr, lev_verbose)
       endif
       init_counts = init_counts + 1
    endif

    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std_utl, only: utl_diag=>diag, choice
    use TOUZA_Std_log, only: log_diag=>diag, msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer lv, md, utmp

    ierr = 0
    utmp = choice(uout, u)
    lv = choice(lev_verbose, levv)
    md = choice(DIAG_DEFAULT, mode)
    if (md.eq.DIAG_DEFAULT) md = DIAG_DEEP

    if (md.gt.DIAG_DEFAULT) then
       if (IAND(md, DIAG_DEEP).gt.0) then
          if (ierr.eq.0) call utl_diag(ierr, utmp, lv, md)
          if (ierr.eq.0) call log_diag(ierr, utmp, lv, md)
       endif
       if (diag_counts.eq.0.or.IAND(md,DIAG_FORCE).gt.0) then
          if (ierr.eq.0) then
             if (VCHECK_NORMAL(lv)) then
                call msg_mdl(TIME_STAMP, __MDL__, utmp)
             endif
             if (VCHECK_DEBUG(lv)) then
                call msg_mdl('(''init = '', I0)', (/init_counts/), __MDL__, utmp)
             endif
          endif
          if (VCHECK_DETAIL(lv)) then
             if (ierr.eq.0) call diag_stdu(ierr, utmp)
             if (ierr.eq.0) call diag_recl(ierr, utmp)
          endif
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_   & health_check
  subroutine health_check(ierr, levv, u)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: u
    integer lv, utmp
    character(len=128) :: txt

    ierr = 0
    lv   = choice(lev_verbose, levv)
    utmp = choice(-1,u)

101 format('not set: ', A)
    if (uin.lt.0) then
       if (VCHECK_FATAL(lv)) then
          write(txt, 101) 'OPT_STDIN_UNIT'
          call msg_mdl(txt, __MDL__, utmp)
       endif
       ierr = ERR_FAILURE_INIT
    endif
    if (uout.lt.0) then
       if (VCHECK_FATAL(lv)) then
          write(txt, 101) 'OPT_STDOUT_UNIT'
          call msg_mdl(txt, __MDL__, utmp)
       endif
       ierr = ERR_FAILURE_INIT
    endif
    if (uerr.lt.0) then
       if (VCHECK_FATAL(lv)) then
          write(txt, 101) 'OPT_STDERR_UNIT'
          call msg_mdl(txt, __MDL__, utmp)
       endif
       ierr = ERR_FAILURE_INIT
    endif

    return
  end subroutine health_check
!!!_   & diag_stdu
  subroutine diag_stdu(ierr, u)
    use TOUZA_Std_log,only: msg_mdl
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer utmp

    ierr = 0
#   if HAVE_ISO_FORTRAN_ENV
      utmp = choice(OUTPUT_UNIT, u)
      call msg_mdl &
           & ('(''ISO_FORTRAN_ENV enabled = '', 3(1x, I0))', &
           &  (/INPUT_UNIT, OUTPUT_UNIT, ERROR_UNIT/), &
           &  __MDL__, utmp)
#   else /* not HAVE_ISO_FORTRAN_ENV */
102 format(__MDL__, 'ISO_FORTRAN_ENV disabled')
      utmp = choice(-1, u)
      call msg_mdl &
           & ('ISO_FORTRAN_ENV disabled',  __MDL__, utmp)
#   endif

    call msg_mdl &
         & ('(''units = '', 3(1x, I0))', (/uin, uout, uerr/), __MDL__, utmp)

    return
  end subroutine diag_stdu

!!!_   & diag_recl
  subroutine diag_recl(ierr, u)
    use TOUZA_Std_log,only: msg_mdl
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer utmp

    ierr = 0
    utmp = choice(uout, u)

    call msg_mdl &
         & ('(''storage bits = '', I0, 1x, I0)', &
         &  (/lfileu, lcharu/), &
         &  __MDL__, utmp)
    call msg_mdl &
         & ('(''storage recl = '', I0, '' / '', I0, 1x, I0, 1x, I0)', &
         &  (/lbrec,  lreci, lrecf, lrecd/), &
         &  __MDL__, utmp)
    return
  end subroutine diag_recl

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_utl, only: utl_finalize=>finalize, choice
    use TOUZA_Std_log, only: log_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer lv
    ierr = 0
    lv = choice(lev_verbose, levv)
    if (ierr.eq.0) call log_finalize(ierr, u, lv, mode)
    if (ierr.eq.0) call utl_finalize(ierr, u, lv, mode)
    return
  end subroutine finalize
!!!_ + queries
!!!_  & get_rlb - get unit record length in bytes
  integer function get_rlb() result(l)
    implicit none
    l = lbrec
  end function get_rlb

!!!_  & get_rlu - get record length unit
  integer function get_rlu_d (V) result(l)
    use TOUZA_Std_prc,only: KDBL
    implicit none
    real(kind=KDBL),intent(in) :: V
    l = lrecd + 0 * kind(V)
  end function get_rlu_d
  integer function get_rlu_f (V) result(l)
    use TOUZA_Std_prc,only: KFLT
    implicit none
    real(kind=KFLT),intent(in) :: V
    l = lrecf + 0 * kind(V)
  end function get_rlu_f
  integer function get_rlu_i (V) result(l)
    implicit none
    integer,intent(in) :: V
    l = lreci + 0 * kind(V)
  end function get_rlu_i
!!!_ + i/o units
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
          call brute_force_std_units(ierr, ui, uo, ue, 0, ltry, lv)
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

!!!_  & brute_force_std_units - lazy trial to find standard units
  subroutine brute_force_std_units &
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
          write(txt, 101) jchk, ierr, OPND, trim(TA)
          call msg_mdl(txt, __MDL__)
       endif
    enddo
    if (ustdo.lt.0.and.ustde.ge.0) then
       ustdo = ustde
       ustde = -1
    endif
    return
  end subroutine brute_force_std_units

!!!_ + storage size/record length detection
!!!_  & check_storage_units
  subroutine check_storage_units &
       & (ierr, lrd, lrf, lri, lunit, ulog, lbf, lbc, levtry)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_fun,only: new_unit_nn
    use TOUZA_Std_prc,only: KDBL, KFLT
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: lrd, lrf, lri
    integer,intent(out)         :: lunit
    integer,intent(in),optional :: ulog
    integer,intent(in),optional :: lbf, lbc
    integer,intent(in),optional :: levtry

    integer utest
    logical isf

    ierr = 0
    utest = -1
    isf = choice(0, levtry).gt.0

    lunit = 0
    if (present(lbf).and.present(lbc)) then
       lunit = max(0, lbf) / max(1, lbc)
    endif

    if (lunit.le.0 .or. isf) then
       if (utest.lt.0) utest = new_unit_nn()
       if (utest.lt.0) ierr = -1
       if (ierr.eq.0) call brute_force_storage_unit(ierr, lunit, utest, ulog)
    endif

    if (ierr.eq.0) then
#     if HAVE_INQUIRE_IOLENGTH
       if (.NOT.isf) then
          INQUIRE(IOLENGTH=lrd) real(0, KIND=KDBL)
          INQUIRE(IOLENGTH=lrf) real(0, KIND=KFLT)
          INQUIRE(IOLENGTH=lri) int(0)
       endif
#     else  /* not HAVE_INQUIRE_IOLENGTH */
       isf = .true.
#     endif /* not HAVE_INQUIRE_IOLENGTH */
       if (isf) then
          if (utest.lt.0) utest = new_unit_nn()
          if (utest.lt.0) ierr = -1
          if (ierr.eq.0) call brute_force_recl(ierr, lrd, utest, real(0, KIND=KDBL))
          if (ierr.eq.0) call brute_force_recl(ierr, lrf, utest, real(0, KIND=KFLT))
          if (ierr.eq.0) call brute_force_recl(ierr, lri, utest, int(0))
       endif
    endif
    return
  end subroutine check_storage_units

!!!_  & brute_force_storage_unit - lazy trial to find file storage unit
  subroutine brute_force_storage_unit &
       & (ierr, lunit, utest, ulog)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: lunit
    integer,intent(in)          :: utest
    integer,intent(in),optional :: ulog
    integer lrec
    character(len=*),parameter :: teststr = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=1) :: C

    ierr = 0
    lunit = -1
    call check_new_file(ierr, utest, ulog)
    if (ierr.ne.0) return

    if (ierr.eq.0) then
       lrec = len(teststr) + 10
       open(UNIT=utest, STATUS='UNKNOWN', ACCESS='DIRECT', &
            & FORM='UNFORMATTED', ACTION='WRITE', RECL=lrec, IOSTAT=ierr)
    endif
    if (ierr.eq.0) write(UNIT=utest, REC=1, IOSTAT=ierr) teststr
    if (ierr.eq.0) close(UNIT=utest, IOSTAT=ierr)
    if (ierr.eq.0) then
       lrec = 1
       open(UNIT=utest, STATUS='UNKNOWN', ACCESS='DIRECT', &
            & FORM='UNFORMATTED', ACTION='READWRITE', RECL=lrec, IOSTAT=ierr)
    endif
    if (ierr.eq.0) read(UNIT=utest, REC=2, IOSTAT=ierr) C
    if (ierr.eq.0) close(UNIT=utest, STATUS='delete', IOSTAT=ierr)
    if (ierr.eq.0) then
       lunit = INDEX(teststr, C) - 1
    endif

    return
  end subroutine brute_force_storage_unit

!!!_  & brute_force_recl - lazy trial to find record length for unit type
  subroutine brute_force_recl_d &
       & (ierr, lrec, utest, v, lunit, nini, ulog)
    use TOUZA_Std_prc, only: KDBL
    use TOUZA_Std_utl, only: choice
    implicit none
    integer,        intent(out)         :: ierr
    integer,        intent(out)         :: lrec
    integer,        intent(in)          :: utest
    real(kind=KDBL),intent(in)          :: v     ! dummy placeholder
    integer,        intent(in),optional :: lunit ! unit record length in bytes
    integer,        intent(in),optional :: nini  ! initial guess
    integer,        intent(in),optional :: ulog

    integer ni
    ni = choice(8, nini) + 0 * KIND(V)
    call brute_force_recl_core &
         & (ierr, lrec, utest, check_single_write_d, ni, lunit, ulog)

  end subroutine brute_force_recl_d

  subroutine brute_force_recl_f &
       & (ierr, lrec, utest, v, lunit, nini, ulog)
    use TOUZA_Std_prc, only: KFLT
    use TOUZA_Std_utl, only: choice
    implicit none
    integer,        intent(out)         :: ierr
    integer,        intent(out)         :: lrec
    integer,        intent(in)          :: utest
    real(kind=KFLT),intent(in)          :: v     ! dummy placeholder
    integer,        intent(in),optional :: lunit ! unit record length in bytes
    integer,        intent(in),optional :: nini  ! initial guess
    integer,        intent(in),optional :: ulog

    integer ni
    ni = choice(4, nini) + 0 * KIND(V)
    call brute_force_recl_core &
         & (ierr, lrec, utest, check_single_write_f, ni, lunit, ulog)

  end subroutine brute_force_recl_f

  subroutine brute_force_recl_i &
       & (ierr, lrec, utest, v, lunit, nini, ulog)
    use TOUZA_Std_prc, only: KDBL
    use TOUZA_Std_utl, only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: lrec
    integer,intent(in)          :: utest
    integer,intent(in)          :: v     ! dummy placeholder
    integer,intent(in),optional :: lunit ! unit record length in bytes
    integer,intent(in),optional :: nini  ! initial guess
    integer,intent(in),optional :: ulog

    integer ni
    ni = choice(4, nini) + 0 * KIND(V)
    call brute_force_recl_core &
         & (ierr, lrec, utest, check_single_write_i, ni, lunit, ulog)

  end subroutine brute_force_recl_i

!!!_  & brute_force_recl_core - lazy trial to find record length for unit type (core)
  subroutine brute_force_recl_core &
       & (ierr, lrec, utest, xfunc, nini, lunit, ulog)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: lrec
    integer,intent(in)          :: utest
    logical                     :: xfunc
    integer,intent(in)          :: nini  ! initial guess
    integer,intent(in),optional :: lunit ! unit record length in bytes
    integer,intent(in),optional :: ulog

    logical sccs
    integer lu
    integer ngood, nbad, ntry

    ierr = 0
    lrec = -1
    call check_new_file(ierr, utest, ulog)
    if (ierr.ne.0) return

    lu = choice(0, lunit)
    if (lu.le.0) call brute_force_storage_unit(ierr, lu, utest, ulog)
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
  end subroutine brute_force_recl_core

!!!_  & check_single_write () - checker functions
  logical function check_single_write_d &
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
    open(UNIT=utest, STATUS='NEW', ACCESS='DIRECT', &
         & FORM='UNFORMATTED', ACTION='WRITE', RECL=n, IOSTAT=ierr)
    if (ierr.ne.0) return
    write(utest, REC=1, IOSTAT=jchk) V
    sccs = (jchk.eq.0)
    close(UNIT=utest, STATUS='delete', IOSTAT=ierr)
    return
  end function check_single_write_d

  logical function check_single_write_f &
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
    open(UNIT=utest, STATUS='NEW', ACCESS='DIRECT', &
         & FORM='UNFORMATTED', ACTION='WRITE', RECL=n, IOSTAT=ierr)
    if (ierr.ne.0) return
    write(utest, REC=1, IOSTAT=jchk) V
    sccs = (jchk.eq.0)
    close(UNIT=utest, STATUS='delete', IOSTAT=ierr)
    return
  end function check_single_write_f

  logical function check_single_write_i &
       & (ierr, utest, n) result(sccs)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: utest
    integer,intent(in)  :: n
    integer,parameter :: V = 0
    integer jchk

    ierr = 0
    sccs = .false.
    open(UNIT=utest, STATUS='NEW', ACCESS='DIRECT', &
         & FORM='UNFORMATTED', ACTION='WRITE', RECL=n, IOSTAT=ierr)
    if (ierr.ne.0) return
    write(utest, REC=1, IOSTAT=jchk) V
    sccs = (jchk.eq.0)
    close(UNIT=utest, STATUS='delete', IOSTAT=ierr)
    return
  end function check_single_write_i

!!!_  & check_new_file - check if new (unnamed) file
  subroutine check_new_file(ierr, utgt, ulog)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: utgt
    integer,intent(in),optional :: ulog
    integer ul

    ierr = 0
    ul = choice(-1, ulog)
    open(UNIT=utgt, STATUS='NEW', IOSTAT=ierr)
    if (ierr.ne.0) then
       if (VCHECK_SEVERE(lev_verbose)) then
          call msg_mdl &
               & ('(''ERROR: not a new file = '', I0, 1x, I0)', &
               &  (/utgt, ierr/), __MDL__, ul)
       endif
    endif
    if (ierr.eq.0) close(UNIT=utgt, STATUS='delete', IOSTAT=ierr)
    return
  end subroutine check_new_file

end module TOUZA_Std_env

!!!_@ test_std_env - test program
#ifdef TEST_STD_ENV
program test_std_env
  use TOUZA_Std_prc, only: KDBL, KFLT
  use TOUZA_Std_env
  implicit none
  integer ierr
  integer ui, uo, ue
  integer lrb, lrd, lrf, lri

  call init(ierr)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) then
     call brute_force_std_units(ierr, ui, uo, ue, levv=1024)
     write(*, *) 'STD = ', ui, uo, ue
  endif
  if (ierr.eq.0) then
     call check_storage_units (ierr, lrd, lrf, lri, lrb, uo, levtry=16)
     write(*, *) 'RECU:B = ', lrb
     write(*, *) 'RECL:I = ', lri
     write(*, *) 'RECL:F = ', lrf
     write(*, *) 'RECL:D = ', lrd
  endif

  if (ierr.eq.0) call finalize(ierr)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_std_env

#endif /* TEST_STD_ENV */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

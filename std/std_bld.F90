!!!_! std_bld.F90 - touza/std build environments
! Maintainer: SAITO Fuyuki
! Created: Oct 27 2021
#define TIME_STAMP 'Time-stamp: <2023/01/17 11:44:48 fuyuki std_bld.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021, 2022, 2023
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
#include "autorevision.h"
!!!_@ TOUZA_Std_bld - build environments
module TOUZA_Std_bld
  use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
  use TOUZA_Std_log,only: unit_global,  trace_fine,   trace_control
!!!_ + default
  implicit none
  private
!!!_ + parameters
# define __MDL__ 'bld'
!!!_ + public constants
!!!_ + static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
!!!_ + public
  public init, diag, finalize
  public check_all,  check_touza
!!!_ + interfaces
  interface msg_macro
     module procedure msg_macro_i
     module procedure msg_macro_a
  end interface msg_macro
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: utl_init=>init, choice
    use TOUZA_Std_log,only: log_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u      ! log unit
    integer,intent(in),optional :: levv   ! verbose level
    integer,intent(in),optional :: mode   ! initialization flag

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
          if (ierr.eq.0) call utl_init(ierr, ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call log_init(ierr, ulog, levv=lv, mode=lmd)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT - ERR_MASK_STD_BLD
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
             if (VCHECK_NORMAL(lv)) call msg_mdl(TIME_STAMP, __MDL__, utmp)
             if (VCHECK_DEBUG(lv)) then
                call msg_mdl('(''init = '', I0)', (/init_counts/), __MDL__, utmp)
             endif
          endif
          if (VCHECK_DETAIL(lv)) then
             if (ierr.eq.0) call check_all(ierr, utmp)
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call utl_diag(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call log_diag(ierr, utmp, lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_utl, only: utl_diag=>diag, choice
    use TOUZA_Std_log, only: log_diag=>diag, msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
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
          continue
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + user interfaces
!!!_  & check_all - check and set environments
  subroutine check_all &
       & (ierr, ulog)
    use TOUZA_Std_utl, only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: ulog

    ierr = 0

    if (ierr.eq.0) call check_touza(ierr, ulog)
    if (ierr.eq.0) call check_gcc(ierr, ulog)
    if (ierr.eq.0) call check_nec(ierr, ulog)
    if (ierr.eq.0) call check_intel(ierr, ulog)
    return
  end subroutine check_all
!!!_ + library
  subroutine check_touza &
       & (ierr, ulog)
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: ulog
    ierr = 0
#ifndef VCS_UUID
#  define VCS_UUID ' '
#endif
#ifndef VCS_NUM
#  define VCS_NUM ' '
#endif
#ifndef VCS_DATA
#  define VCS_DATA ' '
#endif
#ifndef VCS_BRANCH
#  define VCS_BRANCH ' '
#endif
#ifndef VCS_TAG
#  define VCS_TAG ' '
#endif
#ifndef VCS_TICK
#  define VCS_TICK ' '
#endif
#ifndef VCS_EXTRA
#  define VCS_EXTRA ' '
#endif
#ifndef VCS_ACTION_STAMP
#  define VCS_ACTION_STAMP ' '
#endif
#ifndef VCS_FULL_HASH
#  define VCS_FULL_HASH ' '
#endif
#ifndef VCS_WC_MODIFIED
#  define VCS_WC_MODIFIED ' '
#endif
    if (ierr.eq.0) call msg_macro(ierr, 'VCS_UUID', VCS_UUID, ulog)
    if (ierr.eq.0) call msg_macro(ierr, 'VCS_NUM',  VCS_NUM, ulog)
    if (ierr.eq.0) call msg_macro(ierr, 'VCS_DATA', VCS_DATE, ulog)
    if (ierr.eq.0) call msg_macro(ierr, 'VCS_BRANCH', VCS_BRANCH, ulog)
    if (ierr.eq.0) call msg_macro(ierr, 'VCS_TAG', VCS_TAG, ulog)
    if (ierr.eq.0) call msg_macro(ierr, 'VCS_TICK', VCS_TICK, ulog)
    if (ierr.eq.0) call msg_macro(ierr, 'VCS_EXTRA', VCS_EXTRA, ulog)
    if (ierr.eq.0) call msg_macro(ierr, 'VCS_ACTION_STAMP', VCS_ACTION_STAMP, ulog)
    if (ierr.eq.0) call msg_macro(ierr, 'VCS_FULL_HASH', VCS_FULL_HASH, ulog)
    if (ierr.eq.0) call msg_macro(ierr, 'VCS_WC_MODIFIED', VCS_WC_MODIFIED, ulog)
  end subroutine check_touza
!!!_ + individual systems
!!!_  & check_gcc - gcc build information
  subroutine check_gcc &
       & (ierr, ulog)
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: ulog
    ierr = 0
#if __GFORTRAN__
# ifndef __GNUC__
# define __GNUC__ ' '
# endif
# ifndef __GNUC_MINOR__
# define __GNUC_MINOR__ ' '
# endif
# ifndef __GNUC_PATCHLEVEL__
# define __GNUC_PATCHLEVEL__ ' '
# endif
# ifndef __CHAR_BIT__
# define __CHAR_BIT__ ' '
# endif
# ifndef __VERSION__
# define __VERSION__ ' '
# endif
# ifndef __OPTIMIZE__
# define __OPTIMIZE__ ' '
# endif
# ifndef __OPTIMIZE_SIZE__
# define __OPTIMIZE_SIZE__ ' '
# endif
# ifndef __NO_INLINE__
# define __NO_INLINE__ ' '
# endif
# ifndef __LP64__
# define __LP64__ ' '
# endif
# ifndef __BYTE_ORDER__
# define __BYTE_ORDER__ ' '
# endif
# ifndef __ORDER_LITTLE_ENDIAN__
# define __ORDER_LITTLE_ENDIAN__ ' '
# endif
# ifndef __ORDER_BIG_ENDIAN__
# define __ORDER_BIG_ENDIAN__ ' '
# endif
# ifndef __ORDER_PDP_ENDIAN__
# define __ORDER_PDP_ENDIAN__ ' '
# endif
    if (ierr.eq.0) call msg_macro(ierr, '__GFORTRAN__', __GFORTRAN__, ulog)
    if (ierr.eq.0) call msg_macro(ierr, '__GNUC__', __GNUC__, ulog)
    if (ierr.eq.0) call msg_macro(ierr, '__GNUC_MINOR__', __GNUC_MINOR__, ulog)
    if (ierr.eq.0) call msg_macro(ierr, '__GNUC_PATCHLEVEL__', __GNUC_PATCHLEVEL__, ulog)
    if (ierr.eq.0) call msg_macro(ierr, '__VERSION__', __VERSION__, ulog)
    if (ierr.eq.0) call msg_macro(ierr, '__OPTIMIZE__', __OPTIMIZE__, ulog)
    if (ierr.eq.0) call msg_macro(ierr, '__OPTIMIZE_SIZE__', __OPTIMIZE_SIZE__, ulog)
    if (ierr.eq.0) call msg_macro(ierr, '__NO_INLINE__', __NO_INLINE__, ulog)
    if (ierr.eq.0) call msg_macro(ierr, '__LP64__', __LP64__, ulog)
    if (ierr.eq.0) call msg_macro(ierr, '__CHAR_BIT__', __CHAR_BIT__, ulog)
    if (ierr.eq.0) call msg_macro(ierr, '__BYTE_ORDER__', __BYTE_ORDER__, ulog)
    if (ierr.eq.0) call msg_macro(ierr, '__ORDER_LITTLE_ENDIAN__', __ORDER_LITTLE_ENDIAN__, ulog)
    if (ierr.eq.0) call msg_macro(ierr, '__ORDER_BIG_ENDIAN__', __ORDER_BIG_ENDIAN__, ulog)
    if (ierr.eq.0) call msg_macro(ierr, '__ORDER_PDP_ENDIAN__', __ORDER_PDP_ENDIAN__, ulog)
#else /* not __GFORTRAN__ */
    if (ierr.eq.0) call msg_macro(ierr, '__GFORTRAN__', ' ', ulog)
#endif /* not __GFORTRAN__ */
    return
  end subroutine check_gcc
!!!_  & check_nec - nec build information
  subroutine check_nec &
       & (ierr, ulog)
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: ulog
    ierr = 0
#if __NEC__
# ifndef __NEC_VERSION__
# define __NEC_VERSION__ ' '
# endif
# ifndef __VECTOR__
# define __VECTOR__ ' '
# endif
# ifndef __OPTIMIZE__
# define __OPTIMIZE__ ' '
# endif
# ifndef __VERSION__
# define __VERSION__ ' '
# endif
    if (ierr.eq.0) call msg_macro(ierr, '__NEC__', __NEC__, ulog)
    if (ierr.eq.0) call msg_macro(ierr, '__VERSION__', __VERSION__, ulog)
    if (ierr.eq.0) call msg_macro(ierr, '__NEC_VERSION__', __NEC_VERSION__, ulog)
    if (ierr.eq.0) call msg_macro(ierr, '__VECTOR__', __VECTOR__, ulog)
    if (ierr.eq.0) call msg_macro(ierr, '__OPTIMIZE__', __OPTIMIZE__, ulog)
#else /* not __NEC__ */
    if (ierr.eq.0) call msg_macro(ierr, '__NEC__', ' ', ulog)
#endif /* not __NEC__ */
    return
  end subroutine check_nec

!!!_  & check_intel - intel build information
  subroutine check_intel &
       & (ierr, ulog)
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: ulog
    ierr = 0
#ifdef __INTEL_COMPILER
# ifndef __INTEL_COMPILER_BUILD_DATE
# define __INTEL_COMPILER_BUILD_DATE ' '
# endif
# ifndef __INTEL_COMPILER_UPDATE
# define __INTEL_COMPILER_UPDATE ' '
# endif
    if (ierr.eq.0) call msg_macro(ierr, '__INTEL_COMPILER', __INTEL_COMPILER, ulog)
    if (ierr.eq.0) call msg_macro(ierr, '__INTEL_COMPILER_BUILD_DATE ', __INTEL_COMPILER_BUILD_DATE, ulog)
    if (ierr.eq.0) call msg_macro(ierr, '__INTEL_COMPILER_UPDATE', __INTEL_COMPILER_UPDATE, ulog)
#else /* not __INTEL_COMPILER__ */
    if (ierr.eq.0) call msg_macro(ierr, '__INTEL_COMPILER', ' ', ulog)
#endif /* not __INTEL_COMPILER__ */
    return
  end subroutine check_intel

!!!_ + utilities
  subroutine msg_macro_i &
       & (ierr, m, v, ulog)
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: m
    integer,         intent(in)          :: v
    integer,         intent(in),optional :: ulog
    character(len=1024) :: txt

    ierr = 0
101 format('build info:', A, 1x, I0)
    write(txt, 101) trim(m), v
    call msg_mdl(txt, __MDL__, ulog)

  end subroutine msg_macro_i
  subroutine msg_macro_a &
       & (ierr, m, v, ulog)
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: m
    character(len=*),intent(in)          :: v
    integer,         intent(in),optional :: ulog
    character(len=1024) :: txt

    ierr = 0
101 format('build info:', A, 1x, A)
    write(txt, 101) trim(m), trim(v)
    call msg_mdl(txt, __MDL__, ulog)

  end subroutine msg_macro_a

end module TOUZA_Std_bld

!!!_@ test_std_bld - test program
#ifdef TEST_STD_BLD
program test_std_bld
  use TOUZA_Std_bld
  implicit none
  integer ierr

  call init(ierr)
  if (ierr.eq.0) call diag(ierr, levv=+99)
  if (ierr.eq.0) call finalize(ierr, levv=+10)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_std_bld

#endif /* TEST_STD_BLD */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

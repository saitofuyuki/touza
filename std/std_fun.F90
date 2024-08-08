!!!_! std_fun.F90 - touza/std file units manipulation
! Maintainer: SAITO Fuyuki
! Created: Jun 22 2020
#define TIME_STAMP 'Time-stamp: <2024/08/01 09:48:04 fuyuki std_fun.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2020,2021,2022,2023,2024
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
#ifndef   OPT_UNIT_CATEGORIES
#  define OPT_UNIT_CATEGORIES 2   /* number of i/o unit categories */
#endif
#ifndef   OPT_MIN_FILE_UNIT
#  define OPT_MIN_FILE_UNIT 10   /* service unit minimum (black-listed if less than) */
#endif
#ifndef   OPT_MAX_FILE_UNIT
#  define OPT_MAX_FILE_UNIT 2048 /* service unit maximum (black-listed if more than) */
#endif
#ifndef   OPT_PATH_LEN
#  define OPT_PATH_LEN 1024 /* file path limit length */
#endif
#ifndef   OPT_TEMPORARY_FORMAT
#  define OPT_TEMPORARY_FORMAT '(''Temp__ToUzA__'', I0, ''-'', I0)'
#endif
!!!_@ TOUZA_Std_fun - file units manipulation
module TOUZA_Std_fun
!!!_ = declaration
  use TOUZA_Std_log,only: unit_global,  trace_fine,   trace_control
!!!_  - default
  implicit none
  private
!!!_  - parameter
  integer,parameter,public :: search_from_head = -3 ! search from category head
  integer,parameter,public :: search_from_last = -2 ! search from last unit
  integer,parameter,public :: search_from_next = -1 ! search from last unit + 1 (default)

  integer,parameter,public :: kucat_black = -1
  integer,parameter :: lucat = OPT_UNIT_CATEGORIES
# define __MDL__ 'fun'
# define __TAG__ STD_FORMAT_MDL(__MDL__)
#define _ERROR(E) (E - ERR_MASK_STD_FUN)
!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global

  integer,save :: kucat_def = 0
  integer,save :: ucend(kucat_black:lucat-1) = -9
  integer,save :: ulast(0:lucat-1) = -9
  integer,save :: limu = OPT_MAX_FILE_UNIT

  integer,save :: ltry_newu = 1024

  character(len=OPT_PATH_LEN),save :: tmp_fmt = OPT_TEMPORARY_FORMAT
  integer,save :: tmp_id = -1
!!!_  - public
  public init, diag, finalize
  public set_category_bound, set_category_default
  ! public add_black_list, is_black_listed
  public brute_force_check_units
  public new_unit
  public new_unit_tmp, set_tempfile
  public set_tmptmpl
  public is_file_exist, is_unit_opened, is_file_opened
!!!_ + common interfaces
contains
!!!_  & init
  subroutine init(ierr, u, levv, mode, ubgn, uend, cdef, icomm)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_mwe,only: mwe_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer,intent(in),optional :: ubgn, uend ! service unit boundaries (to override OPT_*_FILE_UNIT)
    integer,intent(in),optional :: cdef       ! unit category default
    integer,intent(in),optional :: icomm      ! mwe argument
    integer md, lv, lmd
    integer jc

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
          if (ierr.eq.0) call mwe_init(ierr, ulog, levv=lv, mode=lmd, icomm=icomm)
       endif
       if (is_first_force(init_counts, mode)) then
          if (ierr.eq.0) call set_tmptmpl(ierr)
          if (ierr.eq.0) call set_category_default(ierr, choice(kucat_def, cdef))
          if (ierr.eq.0) call set_category_bound(ierr, kucat_black, choice(OPT_MIN_FILE_UNIT, ubgn))
          do jc = 0, lucat - 1
             if (ierr.eq.0) call set_category_bound(ierr, jc, choice(OPT_MAX_FILE_UNIT, uend))
          enddo
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = _ERROR(ERR_FAILURE_INIT)
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    use TOUZA_Std_mwe,only: mwe_diag=>diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: levv, mode
    integer,intent(in),optional :: u
    integer md, lv, lmd, utmp

    ierr = err_default

    md = control_mode(mode, init_mode)
    lv = choice(lev_verbose, levv)
    utmp = choice(ulog, u)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (VCHECK_NORMAL(lv)) then
                call msg_mdl(TIME_STAMP, __MDL__, utmp)
                call msg_mdl('(''temporary file format = '', A)',  tmp_fmt, __MDL__, utmp)
                call msg_mdl('(''temporary file number = '', I0)', tmp_id,  __MDL__, utmp)
             endif
             if (VCHECK_INFO(lv)) call diag_category(ierr, utmp)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call mwe_diag(ierr, utmp, lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: mwe_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: levv, mode
    integer,intent(in),optional :: u
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
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call mwe_finalize(ierr, utmp, levv, mode=lmd)
       endif
    endif
    return
  end subroutine finalize

!!!_ + subcontracts
!!!_  & set_tmptmpl - set template for temporary file
  subroutine set_tmptmpl(ierr, icomm)
    use TOUZA_Std_mwe,only: get_ni
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: icomm
    integer ir, nr
    ierr = 0
    call get_ni(ierr, nr, ir, icomm)
    if (ierr.eq.0) call set_tempfile(' ', ir)
  end subroutine set_tmptmpl
!!!_  & set_category_bound - set i/o unit range limit
  subroutine set_category_bound (ierr, category, uend)
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: category
    integer,intent(in)  :: uend
    ierr = 0
    if (category.lt.kucat_black .or. category.ge.lucat) then
       ierr = _ERROR(ERR_INVALID_PARAMETER)
       call msg_mdl('(''invalid unit category = '', I0)', (/category/), __MDL__)
       return
    endif
    ucend(category) = uend
    return
  end subroutine set_category_bound

!!!_  & set_category_default - set default category
  subroutine set_category_default (ierr, category)
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: category
    ierr = 0
    if (category.lt.kucat_black .or. category.ge.lucat) then
       ierr = _ERROR(ERR_INVALID_PARAMETER)
       call msg_mdl('(''invalid unit category = '', I0)', (/category/), __MDL__)
       return
    endif
    kucat_def = category
    return
  end subroutine set_category_default

!!!_  & diag_category
  subroutine diag_category &
       & (ierr, u)
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer jc
    ierr = err_default
    call msg_mdl &
         & ('(''default unit category: '', I0, 1x, I0)', kucat_def, &
         &  __MDL__, u)
    do jc = kucat_black, lucat - 1
       call msg_mdl &
            & ('(''unit boundary: '', I0, 1x, I0)', (/jc, ucend(jc)/), &
            &  __MDL__, u)
    enddo
    return
  end subroutine diag_category

!!!_ + file open/close
!!!_  & set_tempfile - set temporary path format
  subroutine set_tempfile (fmt, id)
    implicit none
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: id
    if (present(fmt)) then
       if (fmt.eq.' ') then
          tmp_fmt = OPT_TEMPORARY_FORMAT
       else
          tmp_fmt = fmt
       endif
    else
       tmp_fmt = OPT_TEMPORARY_FORMAT
    endif
    if (present(id)) then
       tmp_id = id
    endif
    return
  end subroutine set_tempfile
!!!_ + unit number
!!!_  & new_unit () - return unbound i/o unit number
  integer function new_unit &
       & (base, category) &
       & result(un)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(in),optional :: base     ! unit base to search
    integer,intent(in),optional :: category ! category
    integer :: ui
    integer :: uoff
    integer :: ub, ue
    integer jerr
    logical opnd
    integer kc

    un = -1

    uoff = choice(search_from_next, base)

    kc = choice(kucat_def, category)
    if (kc.le.kucat_black .or. kc.ge.lucat) return

    ub = ucend(kc-1)
    ue = ucend(kc)
    if (ub.ge.ue .or. ue.le.0) return

    if (uoff.eq.search_from_head) then
       uoff = ub
    else if (uoff.eq.search_from_last) then
       uoff = max(ub, ulast(kc))
    else if (uoff.eq.search_from_next) then
       uoff = max(ub, ulast(kc) + 1)
    else if (uoff.lt.ub .or. uoff.ge.ue) then
       ! category unmatch
       return
    endif

    do ui = uoff, ue - 1
       opnd = is_unit_opened(ui, jerr)
       ! inquire(UNIT=ui, IOSTAT=jerr, OPENED=opnd)
       if (jerr.eq.0 .and. .not.opnd) then
          un = ui
          ulast(kc) = un
          return
       endif
    enddo
    do ui = ub, uoff - 1
       opnd = is_unit_opened(ui, jerr)
       ! inquire(UNIT=ui, IOSTAT=jerr, OPENED=opnd)
       if (jerr.eq.0 .and. .not.opnd) then
          un = ui
          ulast(kc) = un
          return
       endif
    enddo
    return
  end function new_unit

!!!_  & new_unit_tmp () - return unbound i/o unit number and temporal file name
  subroutine new_unit_tmp &
       & (unit, file, base, category)
    implicit none
    integer,         intent(out)         :: unit
    character(len=*),intent(out)         :: file
    integer,         intent(in),optional :: base
    integer,         intent(in),optional :: category
    integer jerr
    integer jc
    file = ' '
    unit = new_unit(base, category)
    if (unit.lt.0) return

    do jc = 0, ltry_newu
       write(file, tmp_fmt, IOSTAT=jerr) tmp_id, jc
       if (jerr.eq.0) open(UNIT=unit, FILE=file, STATUS='NEW', IOSTAT=jerr)
       if (jerr.eq.0) then
          close(UNIT=unit, IOSTAT=jerr)   ! not delete but keep
          return
       endif
       close(UNIT=unit, IOSTAT=jerr)
    enddo
    unit = -1
    file = ' '
    return
  end subroutine new_unit_tmp

!!!_ + limit check
!!!_  & brute_force_check_units - numbers of units brute-force checker
  subroutine brute_force_check_units &
       & (ierr, limit, ulog)
#   define __PROC__ 'brute_force_check_units'
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: limit
    integer,intent(in),optional :: ulog

    integer,parameter   :: ubgn = 0
    integer,allocatable :: stt(:)
    integer jdmy
    integer jchk, lchk
    integer nopn, ncls, nopnd
    logical opnd
    integer uli
    character(len=1024) :: tmsg, txt
    integer jerr

    ierr = 0
    lchk = choice(limu, limit)
    if (lchk.lt.0) lchk = max(0, limu)
    uli = choice(-1, ulog)
    allocate(stt(ubgn:lchk), STAT=ierr)
    nopn  = 0
    nopnd = 0
    if (ierr.ne.0) then
       ierr = _ERROR(ERR_ALLOCATION)
    else
       stt(:) = 0
       do jchk = ubgn, lchk
          ! inquire(UNIT=jchk, IOSTAT=ierr, OPENED=opnd)
          opnd = is_unit_opened(jchk, jerr)
          tmsg = ' '
          if (ierr.eq.0) then
             if (OPND) then
                stt(jchk) = 1
                nopnd = nopnd + 1
             else
                stt(jchk) = 2
                call open_scratch(ierr, jchk, tmsg)
                if (ierr.eq.0) nopn = nopn + 1
             endif
          endif
101       format('file unit check[', I0, '] ', I0, 1x, I0, 1x, A)
          write(txt, 101, IOSTAT=jerr) jchk, stt(jchk), ierr, trim(tmsg)
          call msg_mdl(txt, __MDL__, uli)
          if (ierr.ne.0) exit
       enddo
    endif
    ncls = 0
    if (allocated(stt)) then
       do jchk = ubgn, lchk
          if (stt(jchk).eq.2) then
             close(UNIT=jchk, IOSTAT=jdmy)
             if (jdmy.eq.0) ncls = ncls + 1
          endif
       enddo
       deallocate(stt, STAT=jdmy)
    endif
    call msg_mdl('(''file unit check counts = '', I0, 1x, I0, 1x, I0)', &
         & (/nopnd, nopn, ncls/), &
         & __MDL__, uli)
    return
#   undef __PROC__
  end subroutine brute_force_check_units

!!!_  & open_scratch - open wrapper
  subroutine open_scratch &
       & (ierr, u, tmsg)
#   define __PROC__ 'open_scratch'
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: u
    character(len=*),intent(out) :: tmsg
#if HAVE_FORTRAN_OPEN_IOMSG
#else
    integer jerr
#endif
    ierr = 0
    tmsg = ' '
#if HAVE_FORTRAN_OPEN_IOMSG
    open(UNIT=u, STATUS='SCRATCH', IOSTAT=iErr, IOMSG=tmsg)
#else  /* not HAVE_FORTRAN_OPEN_IOMSG */
    open(UNIT=u, STATUS='SCRATCH', IOSTAT=iErr)
    if (ierr.ne.0) then
101    format('open error for scratch = ', I0)
       write(tmsg, 101, IOSTAT=jerr) iErr
    endif
#endif /* not HAVE_FORTRAN_OPEN_IOMSG */
    return
#   undef __PROC__
  end subroutine open_scratch

!!!_ + inquire wrappers
!!!_  & is_file_opened() - return connected unit or negative value
  integer function is_file_opened &
       & (file, iostat, iomsg) &
       & result(u)
    implicit none
    character(len=*),intent(in)           :: file
    integer,         intent(out),optional :: iostat
    character(len=*),intent(out),optional :: iomsg
    integer jerr
#if HAVE_FORTRAN_OPEN_IOMSG
    inquire(FILE=file, NUMBER=u, IOSTAT=jerr, IOMSG=iomsg)
    if (jerr.ne.0) then
       u = min(-1, jerr)
    endif
    if (present(iostat)) then
       iostat = jerr
    endif
#else  /* not HAVE_FORTRAN_OPEN_IOMSG */
    inquire(FILE=file, NUMBER=u, IOSTAT=jerr)
    if (jerr.ne.0) then
       u = min(-1, jerr)
    endif
    if (present(iostat)) then
       iostat = jerr
    endif
    if (jerr.ne.0) then
       if (present(iomsg)) then
101       format('error at unit check: ', A)
          write(iomsg, 101, IOSTAT=jerr) trim(file)
       endif
    endif
#endif /* not HAVE_FORTRAN_OPEN_IOMSG */
  end function is_file_opened
!!!_  & is_unit_opened() - return a defined value (.false.) even at error
  logical function is_unit_opened &
       & (unit, iostat, iomsg) &
       & result(b)
    implicit none
    integer,         intent(in)           :: unit
    integer,         intent(out),optional :: iostat
    character(len=*),intent(out),optional :: iomsg
    integer jerr
#if HAVE_FORTRAN_OPEN_IOMSG
    inquire(UNIT=unit, OPENED=b, IOSTAT=jerr, IOMSG=iomsg)
    if (jerr.ne.0) b = .FALSE.
    if (present(iostat)) then
       iostat = jerr
    endif
#else  /* not HAVE_FORTRAN_OPEN_IOMSG */
    inquire(UNIT=unit, OPENED=b, IOSTAT=jerr)
    if (jerr.ne.0) b = .FALSE.
    if (present(iostat)) then
       iostat = jerr
    endif
    if (jerr.ne.0) then
       if (present(iomsg)) then
101       format('error at open check: ', I0)
          write(iomsg, 101, IOSTAT=jerr) unit
       endif
    endif
#endif /* not HAVE_FORTRAN_OPEN_IOMSG */
  end function is_unit_opened

!!!_  & is_file_exist() - return a defined value (.false.) even at error
  logical function is_file_exist &
       & (file, iostat, iomsg) &
       & result(b)
    implicit none
    character(len=*),intent(in)           :: file
    integer,         intent(out),optional :: iostat
    character(len=*),intent(out),optional :: iomsg
    integer jerr
#if HAVE_FORTRAN_OPEN_IOMSG
    inquire(FILE=file, EXIST=b, IOSTAT=jerr, IOMSG=iomsg)
    if (jerr.ne.0) b = .FALSE.
    if (present(iostat)) then
       iostat = jerr
    endif
#else  /* not HAVE_FORTRAN_OPEN_IOMSG */
    inquire(FILE=file, EXIST=b, IOSTAT=jerr)
    if (jerr.ne.0) b = .FALSE.
    if (present(iostat)) then
       iostat = jerr
    endif
    if (jerr.ne.0) then
       if (present(iomsg)) then
101       format('error at existence check: ', A)
          write(iomsg, 101, IOSTAT=jerr) trim(file)
       endif
    endif
#endif /* not HAVE_FORTRAN_OPEN_IOMSG */
  end function is_file_exist
end module TOUZA_Std_fun

!!!_@ test_std_fun - test program
#ifdef TEST_STD_FUN
program test_std_fun
  use TOUZA_Std_fun
  implicit none
  integer ierr
  integer n, un
  integer jc
  integer utmp
  character(len=128) :: fn

  call init(ierr, ubgn=20, uend=80)
  if (ierr.eq.0) call set_category_bound(ierr, 0, 70)
  if (ierr.eq.0) call diag(ierr, levv=+2)

  if (ierr.eq.0) then
     call brute_force_check_units(ierr, limit=20)
     call brute_force_check_units(ierr, limit=200)
  endif
  if (ierr.eq.0) then
     do jc = 0, 1
        do n = 0, 30
           un = new_unit(category=jc)
           write(*, *) 'new unit: ', jc, n, un
        enddo
     enddo
  endif
  if (ierr.eq.0) then
     call new_unit_tmp(utmp, fn)
201  format('temporary file = ', I0, ': ', A)
     write(*, 201) utmp, trim(fn)
     if (utmp.ge.0) then
        open(utmp, FILE=fn)
        close(utmp, STATUS='delete')
     endif
  endif

  if (ierr.eq.0) call finalize(ierr, levv=+10)

101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_std_fun
#endif /* TEST_STD_FUN */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

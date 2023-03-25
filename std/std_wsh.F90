!!!_! std_wsh.F90 - touza/std standard work-sharing
! Maintainer: SAITO Fuyuki
! Created: May 30 2020
#define TIME_STAMP 'Time-stamp: <2023/03/25 10:04:29 fuyuki std_wsh.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021,2022,2023
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
!!!_@ TOUZA_Std_env - standard environments
module TOUZA_Std_wsh
  use TOUZA_Std_prc,only: KI32, KI64, KFLT, KDBL
!!!_ = declaration
!!!_  - default
  implicit none
  private
!!!_  - parameters
# define __MDL__ 'wsh'
# define __TAG__ STD_FORMAT_MDL(__MDL__)
# define _ERROR(E) (E - ERR_MASK_STD_WSH)
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL
  integer,save :: ulog = -1

  integer,save :: err_default = _ERROR(ERR_NO_INIT)
!!!_  - public parameter
  integer,parameter,public :: LWI = 4
  integer,parameter,public :: LWL = 4
  integer,parameter,public :: LWF = 4
  integer,parameter,public :: LWD = 4
!!!_  - public static
!!!_   . integer works
  integer(kind=KI32),allocatable,save,public :: wi0(:)
  integer(kind=KI32),allocatable,save,public :: wi1(:)
  integer(kind=KI32),allocatable,save,public :: wi2(:)
  integer(kind=KI32),allocatable,save,public :: wi3(:)
!!!_   . long works
  integer(kind=KI64),allocatable,save,public :: wl0(:)
  integer(kind=KI64),allocatable,save,public :: wl1(:)
  integer(kind=KI64),allocatable,save,public :: wl2(:)
  integer(kind=KI64),allocatable,save,public :: wl3(:)
!!!_   . float works
  real(kind=KFLT),allocatable,save,public :: wf0(:)
  real(kind=KFLT),allocatable,save,public :: wf1(:)
  real(kind=KFLT),allocatable,save,public :: wf2(:)
  real(kind=KFLT),allocatable,save,public :: wf3(:)
!!!_   . double works
  real(kind=KDBL),allocatable,save,public :: wd0(:)
  real(kind=KDBL),allocatable,save,public :: wd1(:)
  real(kind=KDBL),allocatable,save,public :: wd2(:)
  real(kind=KDBL),allocatable,save,public :: wd3(:)
!!!_  - public
  public init, diag, finalize
  public diag_allocs
!!!_  - interfaces
  interface diag_wsize
     module procedure diag_wsize_i
     module procedure diag_wsize_l
     module procedure diag_wsize_f
     module procedure diag_wsize_d
  end interface diag_wsize
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: utl_init=>init, choice
    use TOUZA_Std_prc,only: prc_init=>init
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
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call prc_init(ierr, ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call utl_init(ierr, ulog, levv=lv, mode=lmd)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = _ERROR(ERR_FAILURE_INIT)
    endif

    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: utl_diag=>diag, choice
    use TOUZA_Std_prc,only: prc_diag=>diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer lv, md, utmp, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (ierr.ne.0 .and. IAND(md, MODE_LOOSE).gt.0) then
          if (VCHECK_NORMAL(lv)) then
301          format(STD_FORMAT_FUN(__MDL__, 'diag'), 'loose: ', I0)
             if (utmp.ge.0) then
                write(utmp, 301) ierr
             else
                write(*,    301) ierr
             endif
          endif
          ierr = 0
       endif
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
101          format(__TAG__, A)
             if (VCHECK_NORMAL(lv)) then
                if (utmp.ge.0) then
                   write(utmp, 101) TIME_STAMP
                else
                   write(*,    101) TIME_STAMP
                endif
             endif
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call utl_diag(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call prc_diag(ierr, utmp, lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: utl_finalize=>finalize, choice
    use TOUZA_Std_prc,only: prc_finalize=>finalize
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
          if (VCHECK_DEBUG(lv)) then
311          format(STD_FORMAT_FUN(__MDL__, 'finalize'), 'fine: ', I0, 1x, I0, 1x, I0, 1x, I0)
             if (utmp.ge.0) then
                write(utmp, 311) ierr, init_counts, diag_counts, fine_counts
             else
                write(*,    311) ierr, init_counts, diag_counts, fine_counts
             endif
          endif
          if (ierr.eq.0) call dealloc_all_i(ierr, utmp, lv)
          if (ierr.eq.0) call dealloc_all_l(ierr, utmp, lv)
          if (ierr.eq.0) call dealloc_all_f(ierr, utmp, lv)
          if (ierr.eq.0) call dealloc_all_d(ierr, utmp, lv)
       endif
       if (ierr.ne.0 .and. IAND(md, MODE_LOOSE).gt.0) then
          if (VCHECK_NORMAL(lv)) then
301          format(STD_FORMAT_FUN(__MDL__, 'finalize'), 'loose: ', I0)
             if (utmp.ge.0) then
                write(utmp, 301) ierr
             else
                write(*,    301) ierr
             endif
          endif
          ierr = 0
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call prc_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call utl_finalize(ierr, utmp, lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ & diag subcontracts
!!!_  - diag_allocs
  subroutine diag_allocs (ierr, u, levv)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv

    ierr = err_default
    if (ierr.eq.0) call diag_all_i(ierr, u, levv)
    if (ierr.eq.0) call diag_all_l(ierr, u, levv)
    if (ierr.eq.0) call diag_all_f(ierr, u, levv)
    if (ierr.eq.0) call diag_all_d(ierr, u, levv)
    return
  end subroutine diag_allocs
!!!_  - diag_all
  subroutine diag_all_i (ierr, u, levv)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv

    ierr = err_default
    if (ierr.eq.0) call diag_wsize(ierr, 'wi0', wi0, u, levv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wi1', wi1, u, levv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wi2', wi2, u, levv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wi3', wi3, u, levv)
  end subroutine diag_all_i
  subroutine diag_all_l (ierr, u, levv)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv

    ierr = err_default
    if (ierr.eq.0) call diag_wsize(ierr, 'wl0', wl0, u, levv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wl1', wl1, u, levv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wl2', wl2, u, levv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wl3', wl3, u, levv)
  end subroutine diag_all_l
  subroutine diag_all_f (ierr, u, levv)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv

    ierr = err_default
    if (ierr.eq.0) call diag_wsize(ierr, 'wf0', wf0, u, levv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wf1', wf1, u, levv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wf2', wf2, u, levv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wf3', wf3, u, levv)
  end subroutine diag_all_f
  subroutine diag_all_d (ierr, u, levv)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv

    ierr = err_default
    if (ierr.eq.0) call diag_wsize(ierr, 'wd0', wd0, u, levv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wd1', wd1, u, levv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wd2', wd2, u, levv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wd3', wd3, u, levv)
  end subroutine diag_all_d

!!!_  - diag_wsize
  subroutine diag_wsize_i(ierr, txt, v, u, levv)
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)            :: ierr
    character(len=*),  intent(in)             :: txt
    integer(kind=KARG),intent(in),allocatable :: v(:)
    integer,           intent(in),optional    :: u
    integer,           intent(in),optional    :: levv

    integer n

    ierr = 0
    if (allocated(v)) then
       n = size(v)
    else
       n = -1
    endif
    call diag_wsize_core(ierr, txt, n, u, levv)
  end subroutine diag_wsize_i
  subroutine diag_wsize_l(ierr, txt, v, u, levv)
    implicit none
    integer,parameter :: KARG=KI64
    integer,           intent(out)            :: ierr
    character(len=*),  intent(in)             :: txt
    integer(kind=KARG),intent(in),allocatable :: v(:)
    integer,           intent(in),optional    :: u
    integer,           intent(in),optional    :: levv

    integer n

    ierr = 0
    if (allocated(v)) then
       n = size(v)
    else
       n = -1
    endif
    call diag_wsize_core(ierr, txt, n, u, levv)
  end subroutine diag_wsize_l
  subroutine diag_wsize_f(ierr, txt, v, u, levv)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out)            :: ierr
    character(len=*),intent(in)             :: txt
    real(kind=KARG), intent(in),allocatable :: v(:)
    integer,         intent(in),optional    :: u
    integer,         intent(in),optional    :: levv

    integer n

    ierr = 0
    if (allocated(v)) then
       n = size(v)
    else
       n = -1
    endif
    call diag_wsize_core(ierr, txt, n, u, levv)
  end subroutine diag_wsize_f
  subroutine diag_wsize_d(ierr, txt, v, u, levv)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out)            :: ierr
    character(len=*),intent(in)             :: txt
    real(kind=KARG), intent(in),allocatable :: v(:)
    integer,         intent(in),optional    :: u
    integer,         intent(in),optional    :: levv

    integer n

    ierr = 0
    if (allocated(v)) then
       n = size(v)
    else
       n = -1
    endif
    call diag_wsize_core(ierr, txt, n, u, levv)
  end subroutine diag_wsize_d
!!!_  - diag_wsize_core
  subroutine diag_wsize_core(ierr, txt, n, u, levv)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out)            :: ierr
    character(len=*),intent(in)             :: txt
    integer,         intent(in)             :: n
    integer,         intent(in),optional    :: u
    integer,         intent(in),optional    :: levv

    integer utmp, lv

    ierr = 0
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

101 format(STD_FORMAT_FUN(__MDL__, 'diag'), 'alloc:', A, '=', I0)
102 format(STD_FORMAT_FUN(__MDL__, 'diag'), 'no-alloc:', A)
    if (VCHECK_DETAIL(lv)) then
       if (n.ge.0) then
          if (utmp.ge.0) then
             write(utmp, 101) trim(txt), n
          else if (utmp.eq.-1) then
             write(*,    101) trim(txt), n
          endif
       else
          if (utmp.ge.0) then
             write(utmp, 102) trim(txt)
          else if (utmp.eq.-1) then
             write(*,    102) trim(txt)
          endif
       endif
    endif
  end subroutine diag_wsize_core

!!!_ & finalize subcontracts
!!!_  - dealloc_all_i
  subroutine dealloc_all_i (ierr, u, levv)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv

    integer utmp, lv

    ierr = err_default
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (ierr.eq.0) call diag_wsize(ierr, 'wi0', wi0, utmp, lv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wi1', wi1, utmp, lv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wi2', wi2, utmp, lv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wi3', wi3, utmp, lv)
    if (ierr.eq.0) then
       if (allocated(wi0)) deallocate(wi0, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (allocated(wi1)) deallocate(wi1, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (allocated(wi2)) deallocate(wi2, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (allocated(wi3)) deallocate(wi3, STAT=ierr)
    endif
  end subroutine dealloc_all_i
!!!_  - dealloc_all_l
  subroutine dealloc_all_l (ierr, u, levv)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv

    integer utmp, lv

    ierr = err_default
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (ierr.eq.0) call diag_wsize(ierr, 'wl0', wl0, utmp, lv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wl1', wl1, utmp, lv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wl2', wl2, utmp, lv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wl3', wl3, utmp, lv)
    if (ierr.eq.0) then
       if (allocated(wl0)) deallocate(wl0, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (allocated(wl1)) deallocate(wl1, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (allocated(wl2)) deallocate(wl2, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (allocated(wl3)) deallocate(wl3, STAT=ierr)
    endif
  end subroutine dealloc_all_l
!!!_  - dealloc_all_f
  subroutine dealloc_all_f (ierr, u, levv)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv

    integer utmp, lv

    ierr = err_default
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (ierr.eq.0) call diag_wsize(ierr, 'wf0', wf0, utmp, lv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wf1', wf1, utmp, lv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wf2', wf2, utmp, lv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wf3', wf3, utmp, lv)
    if (ierr.eq.0) then
       if (allocated(wf0)) deallocate(wf0, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (allocated(wf1)) deallocate(wf1, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (allocated(wf2)) deallocate(wf2, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (allocated(wf3)) deallocate(wf3, STAT=ierr)
    endif
  end subroutine dealloc_all_f
!!!_  - dealloc_all_d
  subroutine dealloc_all_d (ierr, u, levv)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv

    integer utmp, lv

    ierr = err_default
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (ierr.eq.0) call diag_wsize(ierr, 'wd0', wd0, utmp, lv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wd1', wd1, utmp, lv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wd2', wd2, utmp, lv)
    if (ierr.eq.0) call diag_wsize(ierr, 'wd3', wd3, utmp, lv)
    if (ierr.eq.0) then
       if (allocated(wd0)) deallocate(wd0, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (allocated(wd1)) deallocate(wd1, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (allocated(wd2)) deallocate(wd2, STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (allocated(wd3)) deallocate(wd3, STAT=ierr)
    endif
  end subroutine dealloc_all_d

end module TOUZA_Std_wsh

!!!_@ test_std_wsh - test program
#ifdef TEST_STD_WSH
program test_std_wsh
  use TOUZA_Std_wsh
  implicit none
  integer ierr

  call init(ierr)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call finalize(ierr)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_std_wsh

#endif /* TEST_STD_WSH */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

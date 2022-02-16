!!!_! std_htb.F90 - touza/std simple hash table manager
! Maintainer: SAITO Fuyuki
! Created: Jan 28 2022
#define TIME_STAMP 'Time-stamp: <2022/02/05 15:50:35 fuyuki std_htb.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022
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
#ifndef    OPT_HASH_BASE
#  define  OPT_HASH_BASE 13
#endif
#ifndef    OPT_HASH_CARRY
#  define  OPT_HASH_CARRY 1
#endif
!!!_* switch
!!!_@ TOUZA_Std_htb - simple hash table
module TOUZA_Std_htb
  use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
  use TOUZA_Std_log,only: unit_global,  trace_fine,   trace_control
!!!_ = declaration
!!!_  - default
  implicit none
  private
!!!_  - parameters
!!!_  - types
  type hashtbl_t
     integer :: base
     integer :: carry
  end type hashtbl_t
!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL
  integer,save :: ulog = unit_global

#define __MDL__ 'htb'
#define __TAG__ STD_FORMAT_MDL(__MDL__)

  integer,save :: err_default = ERR_NO_INIT - ERR_MASK_STD_HTB
!!!_  - public
  public init, diag, finalize
!!!_  - interfaces
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
       if (is_first_force(init_counts, mode)) then
          continue
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT - ERR_MASK_STD_ENV
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
    integer lv, md, utmp, lmd

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
             if (VCHECK_DEBUG(lv)) then
                call msg_mdl('(''init = '', I0)', (/init_counts/), __MDL__, utmp)
             endif
          endif
          if (VCHECK_DETAIL(lv)) then
             continue
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
    use TOUZA_Std_utl,only: utl_finalize=>finalize, choice
    use TOUZA_Std_log,only: log_finalize=>finalize
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
          if (ierr.eq.0) call utl_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call log_finalize(ierr, utmp, lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + init subcontracts
!!!_ + diag subcontracts
end module TOUZA_Std_htb

!!!_@ test_std_htb - test program
#ifdef TEST_STD_HTB
program test_std_htb
  use TOUZA_Std_htb
  implicit none
  integer ierr
101 format(A, ' = ', I0)
  call init(ierr)
  write(*, 101) 'init', ierr
  if (ierr.eq.0) call diag(ierr, levv=+99)
  write(*, 101) 'diag', ierr
  if (ierr.eq.0) call finalize(ierr, levv=+10)
  write(*, 101) 'fine', ierr
  stop
end program test_std_htb

#endif /* TEST_STD_HTB */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

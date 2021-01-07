!!!_! std.F90 - touza/std interfaces
! Maintainer: SAITO Fuyuki
! Created: Jun 4 2020
#define TIME_STAMP 'Time-stamp: <2021/01/07 13:35:12 fuyuki std.F90>'
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
!!!_@ TOUZA_Std - standard interfaces
module TOUZA_Std
  use TOUZA_Std_prc,only: KFLT, KDBL
  use TOUZA_Std_utl,only: choice, choice_a, set_if_present, chcount
  use TOUZA_Std_env,only: uin, uout
  use TOUZA_Std_log,only: &
       & msg, unit_star, unit_none, unit_global
  use TOUZA_Std_fun,only: &
       & add_black_list, brute_force_check_units, &
       & new_unit
  use TOUZA_Std_arg,only: &
       & arg_init => init,    arg_diag => diag, &
       & decl_pos_arg, parse, &
       & get_param, get_option, get_args, check_param
  use TOUZA_Std_mwe,only: &
       & mwe_init=>init, mwe_diag=>diag, mwe_finalize=>finalize, &
       & get_ni
!!!_  - default
  implicit none
  private
!!!_  - static
  logical,save :: ofirst = .true.
!!!_  - public
  public init, diag, finalize
!!!_   . prc
  public KFLT, KDBL
!!!_   . utl
  public choice, choice_a, set_if_present, chcount
!!!_   . env
  public uin, uout
!!!_   . arg
  public :: arg_init, arg_diag
  public :: decl_pos_arg
  public :: parse
  public :: get_param, get_option, get_args, check_param
!!!_   . fun
  public add_black_list
  public brute_force_check_units
  public new_unit
!!!_   . log
  public msg
  public unit_star, unit_none, unit_global
!!!_   . mwe
  public mwe_init, mwe_diag, mwe_finalize
  public get_ni

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, logu, loglev)
    use TOUZA_Std_prc,only: prc_init => init
    use TOUZA_Std_utl,only: utl_init => init
    use TOUZA_Std_env,only: env_init => init
    use TOUZA_Std_log,only: log_init => init, log_msg => msg
    use TOUZA_Std_fun,only: fun_init => init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: logu
    integer,intent(in),optional :: loglev
    integer lu
    ierr = 0
    if (present(loglev)) continue ! dummy
    if (ofirst) then
       if (ierr.eq.0) call mwe_init(ierr)
       if (ierr.eq.0) call prc_init(ierr)
       if (ierr.eq.0) call utl_init(ierr)
       if (ierr.eq.0) call env_init(ierr)
       if (ierr.eq.0) then
          lu = choice(uout, logu)
          call log_init(ierr, lu)
       endif
       if (ierr.eq.0) call fun_init(ierr)

       if (ierr.eq.0) call arg_init(ierr)

#ifdef PACKAGE_STD_STRING
       if (ierr.eq.0) call log_msg(PACKAGE_STD_STRING, 'STD', 0, logu)
#endif
#ifndef    PACKAGE_STRING
#  define  PACKAGE_STRING 'std 0.00'
#endif
       if (ierr.eq.0) call log_msg(PACKAGE_STRING, 'STD', 0, logu)
       if (ierr.eq.0) call log_msg(TIME_STAMP, 'STD', 0, logu)
    endif
    if (ofirst) ofirst = .false.
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u)
    use TOUZA_Std_prc,only: prc_diag => diag
    use TOUZA_Std_utl,only: utl_diag => diag
    use TOUZA_Std_log,only: log_diag => diag
    use TOUZA_Std_env,only: env_diag => diag
    use TOUZA_Std_fun,only: fun_diag => diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer ut

    ierr = 0
    ut = choice(uout, u)
    if (ierr.eq.0) call mwe_diag(ierr, u)
    if (ierr.eq.0) call prc_diag(ierr, ut)
    if (ierr.eq.0) call utl_diag(ierr, ut)
    if (ierr.eq.0) call env_diag(ierr, ut)
    if (ierr.eq.0) call log_diag(ierr, u)
    if (ierr.eq.0) call fun_diag(ierr, u)
    if (ierr.eq.0) call arg_diag(ierr, u)
    return
  end subroutine diag
!!!_  & finalize
  subroutine finalize(ierr, u)
    use TOUZA_Std_utl,only: prc_finalize => finalize
    use TOUZA_Std_utl,only: utl_finalize => finalize
    use TOUZA_Std_env,only: env_finalize => finalize
    use TOUZA_Std_env,only: log_finalize => finalize
    use TOUZA_Std_fun,only: fun_finalize => finalize
    use TOUZA_Std_arg,only: arg_finalize => finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer ut

    ierr = 0
    ut = choice(uout, u)
    if (ierr.eq.0) call prc_finalize(ierr, u)
    if (ierr.eq.0) call log_finalize(ierr, u)
    if (ierr.eq.0) call env_finalize(ierr, ut)
    if (ierr.eq.0) call utl_finalize(ierr, ut)
    if (ierr.eq.0) call fun_finalize(ierr, ut)
    if (ierr.eq.0) call arg_finalize(ierr, ut)
    if (ierr.eq.0) call mwe_finalize(ierr, ut)
    return
  end subroutine finalize
end module TOUZA_Std
!!!_@ test_std - test program
#ifdef TEST_STD
program test_std
  use TOUZA_Std
  implicit none
  integer ierr

  call init(ierr)
  if (ierr.eq.0) then
     call diag(ierr)
  endif
  if (ierr.eq.0) then
     call finalize(ierr)
  endif
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_std

#endif /* TEST_STD */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

!!!_! std.F90 - touza/std interfaces
! Maintainer: SAITO Fuyuki
! Created: Jun 4 2020
#define TIME_STAMP 'Time-stamp: <2021/03/06 09:55:25 fuyuki std.F90>'
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
  use TOUZA_Std_prc,only: KFLT, KDBL, KI8, KI32, KI64, &
       & check_real_zero, check_real_one, check_real_inf, check_real_dnm
  use TOUZA_Std_utl,only: choice, choice_a, condop, set_if_present, chcount
  use TOUZA_Std_env,only: &
       & uin, uout, uerr, lbrec, lreci, lrecf, lrecd, &
       & get_rlu,   get_rlb
  use TOUZA_Std_log,only: &
       & unit_star, unit_none, unit_global, &
       & msg,            msg_grp,         gen_tag, &
       & msglev_panic,   &
       & msglev_fatal,   msglev_critical, msglev_severe, &
       & msglev_warning, msglev_normal,   msglev_info, &
       & msglev_detail,  msglev_debug, &
       & is_msglev, &
       & is_msglev_panic, &
       & is_msglev_fatal,   is_msglev_critical, is_msglev_severe, &
       & is_msglev_warning, is_msglev_normal,   is_msglev_info, &
       & is_msglev_detail,  is_msglev_debug
  use TOUZA_Std_fun,only: &
       & add_black_list, brute_force_check_units, &
       & new_unit,       new_unit_tmp,          set_tempfile
  use TOUZA_Std_arg,only: &
       & arg_init => init, arg_diag => diag, &
       & decl_pos_arg, parse, &
       & get_param,    get_array, get_option, get_arg,      check_param, &
       & get_key,      get_value, get_value_seq

  use TOUZA_Std_mwe,only: &
       & mwe_init=>init, mwe_diag=>diag, mwe_finalize=>finalize, &
       & get_ni
!!!_  - default
  implicit none
  private
!!!_  - static
#define __GRP__ GROUP_TAG
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: lev_verbose = 0

  character(len=64),save :: tagmsg = ' '
!!!_  - public
  public init, diag, finalize
!!!_   . prc
  public KFLT, KDBL, KI8, KI32, KI64
  public check_real_zero, check_real_one, check_real_inf, check_real_dnm
!!!_   . utl
  public choice, choice_a, condop, set_if_present, chcount
!!!_   . env
  public uin, uout, uerr, lbrec, lreci, lrecf, lrecd
  public get_rlu,   get_rlb
!!!_   . arg
  public :: arg_init, arg_diag
  public :: decl_pos_arg
  public :: parse
  public :: get_param, get_array, get_option, get_arg, check_param
  public :: get_key,   get_value, get_value_seq
!!!_   . fun
  public add_black_list
  public brute_force_check_units
  public new_unit, new_unit_tmp, set_tempfile
!!!_   . log
  public msg, msg_grp, gen_tag
  public unit_star, unit_none, unit_global
  public msglev_panic
  public msglev_fatal,   msglev_critical, msglev_severe
  public msglev_warning, msglev_normal,   msglev_info
  public msglev_detail,  msglev_debug
  public is_msglev
  public is_msglev_panic
  public is_msglev_fatal,   is_msglev_critical, is_msglev_severe
  public is_msglev_warning, is_msglev_normal,   is_msglev_info
  public is_msglev_detail,  is_msglev_debug
!!!_   . mwe
  public mwe_init, mwe_diag, mwe_finalize
  public get_ni

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init &
       & (ierr, &
       &  logu, envtry,     &
       &  levv, mode,       &
       &  prc,  utl,  log,  fun, env, arg, mwe)
    use TOUZA_Std_prc,only: prc_init=>init
    use TOUZA_Std_utl,only: utl_init=>init
    use TOUZA_Std_env,only: env_init=>init
    use TOUZA_Std_log,only: log_init=>init
    use TOUZA_Std_fun,only: fun_init=>init
    use TOUZA_Std_mwe,only: mwe_init=>init
    use TOUZA_Std_arg,only: arg_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: logu
    integer,intent(in),optional :: envtry

    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: prc,  utl,  log,  fun, env, arg, mwe

    integer lv, md

    ierr = 0

    lv = choice(lev_verbose, levv)
    md = choice(INIT_DEFAULT, mode)
    if (md.eq.INIT_DEFAULT) md = INIT_DEEP

    if (md.gt.INIT_DEFAULT) then
       if (md.ge.INIT_DEEP) then
          if (ierr.eq.0) call mwe_init(ierr,          levv=lv, mode=choice(md, mwe))
          if (ierr.eq.0) call prc_init(ierr,          levv=lv, mode=choice(md, prc))
          if (ierr.eq.0) call utl_init(ierr,          levv=lv, mode=choice(md, utl))
          if (ierr.eq.0) call log_init(ierr, logu,    levv=lv, mode=choice(md, log))
          if (ierr.eq.0) call env_init(ierr, envtry,  levv=lv, mode=choice(md, env))
          if (ierr.eq.0) call fun_init(ierr,          levv=lv, mode=choice(md, fun))
          if (ierr.eq.0) call arg_init(ierr,          levv=lv, mode=choice(md, arg))
       endif
       if (init_counts.eq.0) then
          call gen_tag(tagmsg, pkg=PACKAGE_TAG, grp=__GRP__)
          lev_verbose = lv
       endif
       init_counts = init_counts + 1
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std_prc,only: prc_diag=>diag
    use TOUZA_Std_utl,only: utl_diag=>diag
    use TOUZA_Std_log,only: log_diag=>diag
    use TOUZA_Std_env,only: env_diag=>diag
    use TOUZA_Std_fun,only: fun_diag=>diag
    use TOUZA_Std_mwe,only: mwe_diag=>diag
    use TOUZA_Std_arg,only: arg_diag=>diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: u
    integer utmp, lv, md

    ierr = 0
    utmp = choice(uout, u)
    lv = choice(lev_verbose, levv)
    md = choice(DIAG_DEFAULT, mode)
    if (md.eq.DIAG_DEFAULT) md = DIAG_DEEP

    if (md.gt.DIAG_DEFAULT) then
       if (diag_counts.eq.0.or.IAND(md,DIAG_FORCE).gt.0) then
          if (ierr.eq.0) then
             if (VCHECK_NORMAL(lv)) then
                call msg(TIME_STAMP, tagmsg, u=utmp)
             endif
          endif
       endif
       if (IAND(md, DIAG_DEEP).gt.0) then
          if (ierr.eq.0) call mwe_diag(ierr, utmp, lv, md)
          if (ierr.eq.0) call prc_diag(ierr, utmp, lv, md)
          if (ierr.eq.0) call utl_diag(ierr, utmp, lv, md)
          if (ierr.eq.0) call env_diag(ierr, utmp, lv, md)
          if (ierr.eq.0) call log_diag(ierr, utmp, lv, md)
          if (ierr.eq.0) call fun_diag(ierr, utmp, lv, md)
          if (ierr.eq.0) call arg_diag(ierr, utmp, lv, md)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag
!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_mwe,only: mwe_finalize => finalize
    use TOUZA_Std_prc,only: prc_finalize => finalize
    use TOUZA_Std_utl,only: utl_finalize => finalize
    use TOUZA_Std_env,only: env_finalize => finalize
    use TOUZA_Std_log,only: log_finalize => finalize
    use TOUZA_Std_fun,only: fun_finalize => finalize
    use TOUZA_Std_arg,only: arg_finalize => finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: u
    integer utmp, lv

    ierr = 0
    utmp = choice(uout, u)
    lv = choice(lev_verbose, levv)
    if (ierr.eq.0) call prc_finalize(ierr, utmp, lv, mode)
    if (ierr.eq.0) call log_finalize(ierr, utmp, lv, mode)
    if (ierr.eq.0) call env_finalize(ierr, utmp, lv, mode)
    if (ierr.eq.0) call utl_finalize(ierr, utmp, lv, mode)
    if (ierr.eq.0) call fun_finalize(ierr, utmp, lv, mode)
    if (ierr.eq.0) call arg_finalize(ierr, utmp, lv, mode)
    if (ierr.eq.0) call mwe_finalize(ierr, utmp, lv, mode)
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

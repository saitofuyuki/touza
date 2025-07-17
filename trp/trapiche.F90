!!!_! trapiche.F90 - TOUZA/Trapiche manager
! Maintainer: SAITO Fuyuki
! Created: Feb 26 2021
#define TIME_STAMP 'Time-stamp: <2025/07/17 19:04:15 c0210 trapiche.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021-2025
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_trp.h"
!!!_@ TOUZA_Trp - Trapiche interfaces
module TOUZA_Trp
!!!_ = declaration
  use TOUZA_Trp_std,only: unit_global
  use TOUZA_Trp_std,only: first_bit
  use TOUZA_Trp_pack,only:  tp_init=>init, tp_diag=>diag, tp_finalize=>finalize
  use TOUZA_Trp_float,only: tf_init=>init, tf_diag=>diag, tf_finalize=>finalize
  use TOUZA_Trp_ctl,only:   tc_init=>init, tc_diag=>diag, tc_finalize=>finalize

  use TOUZA_Trp_pack,only: RELLENO_TRANSPOSE, RELLENO_SEQUENTIAL, RELLENO_STRIDE, RELLENO_MANUAL
  use TOUZA_Trp_pack,only: npropd
  use TOUZA_Trp_pack,only: p_cbgn, p_bofs, p_orgm, p_xofs
  use TOUZA_Trp_pack,only: count_packed
  use TOUZA_Trp_pack,only: pack_store,          pack_restore
  use TOUZA_Trp_pack,only: pack_restore_slice,  pack_restore_dunp
  use TOUZA_Trp_pack,only: unparse_relleno
  use TOUZA_Trp_pack,only: show_props_trn,    show_packed
  use TOUZA_Trp_pack,only: div_ceiling,       div_ceiling_safe
  use TOUZA_Trp_pack,only: mask_to_idxl,      mask_to_idxl_seq
  use TOUZA_Trp_pack,only: mask_count_defined
  use TOUZA_Trp_pack,only: pack_gen_dspl,     pack_gen_runl,     gen_bfc_slice
  use TOUZA_Trp_pack,only: gen_bfc_idxl
  use TOUZA_Trp_pack,only: popcount_tab
  use TOUZA_Trp_pack,only: set_loop_slice
  use TOUZA_Trp_pack,only: is_in_slice

  use TOUZA_Trp_float,only: XnoTop, XnoBtm
  use TOUZA_Trp_float,only: KB_HEAD
  use TOUZA_Trp_float,only: helper_props
  use TOUZA_Trp_float,only: encode_alloc, encode_stack, encode_trig
  use TOUZA_Trp_float,only: decode_alloc, decode_stack, decode_work
  use TOUZA_Trp_float,only: health_check
  use TOUZA_Trp_float,only: asignar
  use TOUZA_Trp_float,only: guardar_extra
  use TOUZA_Trp_float,only: diluir
  use TOUZA_Trp_float,only: retrieve_ncnz, retrieve_nbgz, retrieve_extra
  use TOUZA_Trp_float,only: suggest_filling
  use TOUZA_Trp_float,only: push_show_tags
  use TOUZA_Trp_float,only: show_bagazo_props, show_bagazo_patterns, show_pattern_float, binstr_float
  use TOUZA_Trp_float,only: compare_report,    compare_element
  use TOUZA_Trp_float,only: parse_codes, unparse_codes
  use TOUZA_Trp_float,only: KCODE_DEFAULT, KCODE_TRANSPOSE, KCODE_SEQUENTIAL, KCODE_INCREMENTAL
  use TOUZA_Trp_float,only: KCODE_MANUAL,  KCODE_CLIPPING,  KCODE_SIGN_ZERO,  KCODE_ROUND

!!!_  = defaults
  implicit none
  public
!!!_  - no export
  private :: unit_global
!!!_  - static
  integer,save,private :: init_mode = 0
  integer,save,private :: init_counts = 0
  integer,save,private :: diag_counts = 0
  integer,save,private :: fine_counts = 0
  integer,save,private :: lev_verbose = 0
  integer,save,private :: err_default = ERR_NO_INIT
  integer,save,private :: ulog = unit_global

!!!_  - public procedures
  public :: init, diag, finalize

  public :: tp_init, tp_diag, tp_finalize
  public :: RELLENO_TRANSPOSE, RELLENO_SEQUENTIAL, RELLENO_STRIDE, RELLENO_MANUAL
  public :: npropd
  public :: p_cbgn, p_bofs, p_orgm, p_xofs
  public :: count_packed
  public :: pack_store,          pack_restore
  public :: pack_restore_slice,  pack_restore_dunp
  public :: unparse_relleno
  public :: show_props_trn,    show_packed
  public :: div_ceiling,       div_ceiling_safe
  public :: mask_to_idxl,      mask_to_idxl_seq
  public :: mask_count_defined
  public :: pack_gen_dspl,     pack_gen_runl,     gen_bfc_slice
  public :: gen_bfc_idxl
  public :: popcount_tab
  public :: set_loop_slice
  public :: is_in_slice

  public :: tf_init, tf_diag, tf_finalize
  public :: XnoTop, XnoBtm
  public :: KB_HEAD
  public :: helper_props
  public :: encode_alloc, encode_stack, encode_trig
  public :: decode_alloc, decode_stack, decode_work
  public :: health_check
  public :: asignar
  public :: guardar_extra
  public :: diluir
  public :: retrieve_ncnz, retrieve_nbgz, retrieve_extra
  public :: suggest_filling
  public :: push_show_tags
  public :: show_bagazo_props, show_bagazo_patterns, show_pattern_float, binstr_float
  public :: compare_report,    compare_element
  public :: parse_codes, unparse_codes
  public :: KCODE_DEFAULT, KCODE_TRANSPOSE, KCODE_SEQUENTIAL, KCODE_INCREMENTAL
  public :: KCODE_MANUAL,  KCODE_CLIPPING,  KCODE_SIGN_ZERO,  KCODE_ROUND

  public :: tc_init, tc_diag, tc_finalize

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv)
    use TOUZA_Trp_std,only: ts_init=>init
    use TOUZA_Trp_std,only: control_mode, control_deep
    use TOUZA_Trp_std,only: choice, is_first_force
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    integer lv, md, lmd, chmd

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
          chmd = MODE_SURFACE
          if (ierr.eq.0) call ts_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv)
          if (ierr.eq.0) call tp_init(ierr, u=ulog, levv=lv, mode=chmd)
          if (ierr.eq.0) call tf_init(ierr, u=ulog, levv=lv, mode=chmd)
          if (ierr.eq.0) call tc_init(ierr, u=ulog, levv=lv, mode=chmd)
       endif
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
       init_counts = init_counts + 1
    endif

    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Trp_std,only: ts_diag=>diag
    use TOUZA_Trp_std,only: control_mode, control_deep
    use TOUZA_Trp_std,only: choice, is_first_force
    use TOUZA_Trp_std,only: trace_control
    use TOUZA_Trp_std,only: is_msglev_NORMAL
    use TOUZA_Trp_std,only: msg
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode

    integer utmp, lv, md, lmd, chmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, u=utmp)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          chmd = MODE_SURFACE
          if (ierr.eq.0) call ts_diag(ierr, utmp, lv, lmd)
          if (ierr.eq.0) call tp_diag(ierr, utmp, lv, chmd)
          if (ierr.eq.0) call tf_diag(ierr, utmp, lv, chmd)
          if (ierr.eq.0) call tc_diag(ierr, utmp, lv, chmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Trp_std,only: ts_finalize=>finalize
    use TOUZA_Trp_std,only: control_mode, control_deep
    use TOUZA_Trp_std,only: choice, is_first_force
    use TOUZA_Trp_std,only: trace_fine
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd, chmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          chmd = MODE_SURFACE
          if (ierr.eq.0) call ts_finalize(ierr, utmp, lv, lmd)
          if (ierr.eq.0) call tp_finalize(ierr, utmp, lv, chmd)
          if (ierr.eq.0) call tf_finalize(ierr, utmp, lv, chmd)
          if (ierr.eq.0) call tc_finalize(ierr, utmp, lv, chmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + user subroutines
!!!_ + private subroutines
end module TOUZA_Trp

!!!_@ test_trp - test program
#ifdef TEST_TRAPICHE
program test_trp
  use TOUZA_Trp
  implicit none
  integer ierr

  call init(ierr, stdv=+9)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call finalize(ierr, levv=+9)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_trp

#endif /* TEST_TRAPICHE */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

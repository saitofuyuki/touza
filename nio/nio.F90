!!!_! nio.F90 - TOUZA/Nio manager
! Maintainer: SAITO Fuyuki
! Created: Oct 11 2021
#define TIME_STAMP 'Time-stamp: <2025/08/04 13:13:07 fuyuki nio.F90>'
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
#include "touza_nio.h"
!!!_@ TOUZA_Nio - Nio interfaces
module TOUZA_Nio
!!!_ = declaration
!!!_  - modules
  use TOUZA_Nio_std,only: unit_global
  use TOUZA_Nio_header,only:  nh_init=>init, nh_diag=>diag, nh_finalize=>finalize
  use TOUZA_Nio_record,only:  nr_init=>init, nr_diag=>diag, nr_finalize=>finalize
  use TOUZA_Nio_axis,only:    na_init=>init, na_diag=>diag, na_finalize=>finalize
  use TOUZA_Nio_sparse,only:  np_init=>init, np_diag=>diag, np_finalize=>finalize
  use TOUZA_Nio_cache,only:   nc_init=>init, nc_diag=>diag, nc_finalize=>finalize
  use TOUZA_Nio_control,only: nx_init=>init, nx_diag=>diag, nx_finalize=>finalize
  use TOUZA_Nio_bindc,only:   nb_init=>init, nb_diag=>diag, nb_finalize=>finalize
# if OPT_WITH_NCTCDF
  use TOUZA_Nio_nctcdf,only:  nn_init=>init, nn_diag=>diag, nn_finalize=>finalize
# endif

  use TOUZA_Nio_header,only: nitem, litem, lhead
  use TOUZA_Nio_header,only: hi_IDFM,  hi_DSET,  hi_ITEM,   hi_EDIT1, hi_EDIT2, hi_EDIT3, hi_EDIT4, hi_EDIT5
  use TOUZA_Nio_header,only: hi_EDIT6, hi_EDIT7, hi_EDIT8,  hi_FNUM,  hi_DNUM,  hi_TITL1, hi_TITL2, hi_UNIT
  use TOUZA_Nio_header,only: hi_ETTL1, hi_ETTL2, hi_ETTL3,  hi_ETTL4, hi_ETTL5, hi_ETTL6, hi_ETTL7, hi_ETTL8
  use TOUZA_Nio_header,only: hi_TIME,  hi_UTIM,  hi_DATE,   hi_TDUR,  hi_AITM1, hi_ASTR1, hi_AEND1, hi_AITM2
  use TOUZA_Nio_header,only: hi_ASTR2, hi_AEND2, hi_AITM3,  hi_ASTR3, hi_AEND3, hi_DFMT,  hi_MISS,  hi_DMIN
  use TOUZA_Nio_header,only: hi_DMAX,  hi_DIVS,  hi_DIVL,   hi_STYP,  hi_COPTN, hi_IOPTN, hi_ROPTN, hi_TIME2
  use TOUZA_Nio_header,only: hi_UTIM2, hi_MEMO1, hi_MEMO2,  hi_MEMO3, hi_MEMO4, hi_MEMO5, hi_MEMO6, hi_MEMO7
  use TOUZA_Nio_header,only: hi_MEMO8, hi_MEMO9, hi_MEMO10, hi_CDATE, hi_CSIGN, hi_MDATE, hi_MSIGN, hi_SIZE
  use TOUZA_Nio_header,only: hi_DATE1, hi_DATE2
  use TOUZA_Nio_header,only: put_item,       get_item,       store_item,       restore_item
  use TOUZA_Nio_header,only: put_item_date,  get_item_date,  store_item_date,  restore_item_date
  use TOUZA_Nio_header,only: fill_header
  use TOUZA_Nio_header,only: show_header
  use TOUZA_Nio_header,only: parse_date_tuple, unparse_date_tuple
  use TOUZA_Nio_header,only: get_hitem

  use TOUZA_Nio_control,only: nio_open, nio_close
  use TOUZA_Nio_control,only: nio_search
  use TOUZA_Nio_control,only: nio_show_status
  use TOUZA_Nio_control,only: nio_time_undef

  use TOUZA_Nio_record,only: set_default_switch
  use TOUZA_Nio_record,only: set_default_header, get_default_header
  use TOUZA_Nio_record,only: nio_record_std, nio_record_def
  use TOUZA_Nio_record,only: nio_check_magic_file
  use TOUZA_Nio_record,only: nio_read_header,    nio_write_header
  use TOUZA_Nio_record,only: nio_read_data,      nio_write_data
  use TOUZA_Nio_record,only: nio_read_data_slice
  use TOUZA_Nio_record,only: nio_review_record
  use TOUZA_Nio_record,only: nio_skip_records
  use TOUZA_Nio_record,only: nio_bwd_record
  use TOUZA_Nio_record,only: parse_header_base,  parse_record_fmt
  use TOUZA_Nio_record,only: parse_header_size
  use TOUZA_Nio_record,only: get_header_cprop,   put_header_cprop
  use TOUZA_Nio_record,only: get_header_cname,   put_header_cname
  use TOUZA_Nio_record,only: inquire_header_coor,search_null_coor,   shift_header_coor
  use TOUZA_Nio_record,only: set_urt_defs,       parse_urt_options,  show_urt_options
  use TOUZA_Nio_record,only: switch_urt_diag
  use TOUZA_Nio_record,only: set_switch_subrec,  nio_allow_sub
  use TOUZA_Nio_record,only: set_bodr_wnative
  use TOUZA_Nio_record,only: subv_encode
  use TOUZA_Nio_record,only: mtn_review
  use TOUZA_Nio_record,only: mtn_read_array, mtn_read_data
  use TOUZA_Nio_record,only: mtn_write_array, mtn_write_data
  use TOUZA_Nio_record,only: nio_count_defined, decompose_packed_item
  use TOUZA_Nio_record,only: ptx_check_vec
  use TOUZA_Nio_record,only: ptx_def_options, ptx_parse_options, ptx_set_shape
  use TOUZA_Nio_record,only: ptx_review,      ptx_parse_array
  use TOUZA_Nio_record,only: ptx_write_array, ptx_write_data
  use TOUZA_Nio_record,only: ptx_read_array,  ptx_read_data
  use TOUZA_Nio_record,only: ptx_expand_data, ptx_set_loops
  use TOUZA_Nio_record,only: ptx_gen_ccvec,   ptx_pack_data
  use TOUZA_Nio_record,only: ptx_row_size
  use TOUZA_Nio_record,only: pre_review,      post_review
  use TOUZA_Nio_record,only: is_review_leave
  use TOUZA_Nio_record,only: is_match_format
  use TOUZA_Nio_record,only: laxs
  use TOUZA_Nio_record,only: REC_ERROR, REC_ASIS, REC_DEFAULT, REC_SWAP
  use TOUZA_Nio_record,only: REC_LSEP,  REC_BIG,  REC_LITTLE,  REC_SUB_ALLOW
  use TOUZA_Nio_record,only: REC_LEFT
  use TOUZA_Nio_record,only: GFMT_ERR, GFMT_END
  use TOUZA_Nio_record,only: GFMT_UR8, GFMT_UR4, GFMT_URC, GFMT_URC2
  use TOUZA_Nio_record,only: GFMT_MR8, GFMT_MR4
  use TOUZA_Nio_record,only: GFMT_PR4, GFMT_PR8
  use TOUZA_Nio_record,only: GFMT_URY, GFMT_MRY, GFMT_PRY
  use TOUZA_Nio_record,only: GFMT_UI4, GFMT_UI8
  use TOUZA_Nio_record,only: GFMT_MI4, GFMT_MI8
  use TOUZA_Nio_record,only: GFMT_PI4, GFMT_PI8
  use TOUZA_Nio_record,only: GFMT_URT, GFMT_MRT, GFMT_PRT
  use TOUZA_Nio_record,only: BODR_CHECK_VERBOSE
  use TOUZA_Nio_record,only: PROP_PTX_COLC
  use TOUZA_Nio_record,only: lopts

  use TOUZA_Nio_sparse,only: nio_column_coor
  use TOUZA_Nio_sparse,only: nio_review_sparse, nio_inquire_sparse
  use TOUZA_Nio_sparse,only: nio_store_csr,     nio_restore_csr
  use TOUZA_Nio_sparse,only: nio_store_qjds,    nio_restore_qjds
  use TOUZA_Nio_sparse,only: lopts_sparse

  use TOUZA_Nio_axis,only: axis_cyclic, axis_loc, axis_parse, axis_set_header, axis_wgt

  use TOUZA_Nio_cache,only: cache_co_all, cache_co_name, cache_co_len
  use TOUZA_Nio_cache,only: cache_co_range, cache_var_len
  use TOUZA_Nio_cache,only: cache_get_attr
  use TOUZA_Nio_cache,only: cache_group_size, cache_group, cache_group_name, cache_group_recs
  use TOUZA_Nio_cache,only: cache_open_read
  use TOUZA_Nio_cache,only: cache_read_header
  use TOUZA_Nio_cache,only: cache_sparse_review, cache_restore_csr
  use TOUZA_Nio_cache,only: cache_unit
  use TOUZA_Nio_cache,only: cache_var_read
  use TOUZA_Nio_cache,only: cache_var_size, cache_var_id, cache_var_name, cache_co_size
  use TOUZA_Nio_cache,only: grp_suite
  use TOUZA_Nio_cache,only: show_cache

# if OPT_WITH_NCTCDF
  use TOUZA_Nio_nctcdf,only: nct_define_write, nct_write_data
  use TOUZA_Nio_nctcdf,only: nct_open_write
# endif

!!!_  - default
  implicit none
  private
!!!_  - no export
  private :: unit_global
!!!_  - private static
  integer,save,private :: init_mode = 0
  integer,save,private :: init_counts = 0
  integer,save,private :: diag_counts = 0
  integer,save,private :: fine_counts = 0
  integer,save,private :: lev_verbose = NIO_MSG_LEVEL
  integer,save,private :: err_default = ERR_NO_INIT
  integer,save,private :: ulog = unit_global

!!!_  - public procedures
  public :: init, diag, finalize

  public :: nh_init, nh_diag, nh_finalize
  public :: nitem, litem, lhead
  public :: hi_IDFM,  hi_DSET,  hi_ITEM,   hi_EDIT1, hi_EDIT2, hi_EDIT3, hi_EDIT4, hi_EDIT5
  public :: hi_EDIT6, hi_EDIT7, hi_EDIT8,  hi_FNUM,  hi_DNUM,  hi_TITL1, hi_TITL2, hi_UNIT
  public :: hi_ETTL1, hi_ETTL2, hi_ETTL3,  hi_ETTL4, hi_ETTL5, hi_ETTL6, hi_ETTL7, hi_ETTL8
  public :: hi_TIME,  hi_UTIM,  hi_DATE,   hi_TDUR,  hi_AITM1, hi_ASTR1, hi_AEND1, hi_AITM2
  public :: hi_ASTR2, hi_AEND2, hi_AITM3,  hi_ASTR3, hi_AEND3, hi_DFMT,  hi_MISS,  hi_DMIN
  public :: hi_DMAX,  hi_DIVS,  hi_DIVL,   hi_STYP,  hi_COPTN, hi_IOPTN, hi_ROPTN, hi_TIME2
  public :: hi_UTIM2, hi_MEMO1, hi_MEMO2,  hi_MEMO3, hi_MEMO4, hi_MEMO5, hi_MEMO6, hi_MEMO7
  public :: hi_MEMO8, hi_MEMO9, hi_MEMO10, hi_CDATE, hi_CSIGN, hi_MDATE, hi_MSIGN, hi_SIZE
  public :: hi_DATE1, hi_DATE2
  public :: put_item,       get_item,       store_item,       restore_item
  public :: put_item_date,  get_item_date,  store_item_date,  restore_item_date
  public :: fill_header
  public :: show_header
  public :: parse_date_tuple, unparse_date_tuple
  public :: get_hitem

  ! TOUZA_Nio_record
  public :: nr_init, nr_diag, nr_finalize
  public :: set_default_switch
  public :: set_default_header, get_default_header
  public :: nio_record_std, nio_record_def
  public :: nio_check_magic_file
  public :: nio_read_header,    nio_write_header
  public :: nio_read_data,      nio_write_data
  public :: nio_read_data_slice
  public :: nio_review_record
  public :: nio_skip_records
  public :: nio_bwd_record
  public :: parse_header_base,  parse_record_fmt
  public :: parse_header_size
  public :: get_header_cprop,   put_header_cprop
  public :: get_header_cname,   put_header_cname
  public :: inquire_header_coor,search_null_coor,   shift_header_coor
  public :: set_urt_defs,       parse_urt_options,  show_urt_options
  public :: switch_urt_diag
  public :: set_switch_subrec,  nio_allow_sub
  public :: set_bodr_wnative
  public :: subv_encode
  public :: mtn_review
  public :: mtn_read_array, mtn_read_data
  public :: mtn_write_array, mtn_write_data
  public :: nio_count_defined, decompose_packed_item
  public :: ptx_check_vec
  public :: ptx_def_options, ptx_parse_options, ptx_set_shape
  public :: ptx_review,      ptx_parse_array
  public :: ptx_write_array, ptx_write_data
  public :: ptx_read_array,  ptx_read_data
  public :: ptx_expand_data, ptx_set_loops
  public :: ptx_gen_ccvec,   ptx_pack_data
  public :: ptx_row_size
  public :: pre_review,      post_review
  public :: is_review_leave
  public :: is_match_format
  public :: laxs
  public :: BODR_CHECK_VERBOSE
  public :: lopts
  public :: PROP_PTX_COLC

  public :: na_init, na_diag, na_finalize

  public :: np_init, np_diag, np_finalize
  public :: nio_column_coor
  public :: nio_review_sparse, nio_inquire_sparse
  public :: nio_store_csr,     nio_restore_csr
  public :: nio_store_qjds,    nio_restore_qjds
  public :: REC_ERROR, REC_ASIS, REC_DEFAULT, REC_SWAP
  public :: REC_LSEP,  REC_BIG,  REC_LITTLE,  REC_SUB_ALLOW
  public :: REC_LEFT
  public :: GFMT_ERR, GFMT_END
  public :: GFMT_UR8, GFMT_UR4, GFMT_URC, GFMT_URC2
  public :: GFMT_MR8, GFMT_MR4
  public :: GFMT_PR4, GFMT_PR8
  public :: GFMT_URY, GFMT_MRY, GFMT_PRY
  public :: GFMT_UI4, GFMT_UI8
  public :: GFMT_MI4, GFMT_MI8
  public :: GFMT_PI4, GFMT_PI8
  public :: GFMT_URT, GFMT_MRT, GFMT_PRT

  public :: nc_init, nc_diag, nc_finalize

  public :: nx_init, nx_diag, nx_finalize
  public :: nio_open, nio_close
  public :: nio_search
  public :: nio_show_status
  public :: nio_time_undef

  public :: nb_init, nb_diag, nb_finalize
# if OPT_WITH_NCTCDF
  public :: nn_init, nn_diag, nn_finalize
  public :: nct_open_write
  public :: nct_define_write, nct_write_data
# endif

  ! TOUZA_Nio_axis
  public :: axis_parse, axis_wgt, axis_cyclic, axis_set_header, axis_loc

  ! TOUZA_Nio_cache
  public :: cache_var_size, cache_var_id, cache_var_name, cache_co_size
  public :: grp_suite
  public :: cache_open_read
  public :: show_cache
  public :: cache_group_size, cache_group, cache_group_name, cache_group_recs
  public :: cache_co_all, cache_co_name, cache_co_len
  public :: cache_var_read
  public :: cache_get_attr
  public :: cache_read_header
  public :: cache_co_range, cache_var_len
  public :: cache_sparse_review, cache_restore_csr
  public :: cache_unit

  ! TOUZA_Nio_sparse
  public :: lopts_sparse

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm)
    use TOUZA_Nio_std,only: ns_init=>init
    use TOUZA_Nio_std,only: control_mode, control_deep
    use TOUZA_Nio_std,only: choice, is_first_force
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    integer,intent(in),optional :: icomm
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
          if (ierr.eq.0) call ns_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
          ! if (ierr.eq.0) call nh_init(ierr, u=ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_init(ierr, u=ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call na_init(ierr, u=ulog, levv=lv, mode=chmd)
          if (ierr.eq.0) call nc_init(ierr, u=ulog, levv=lv, mode=chmd)
          if (ierr.eq.0) call np_init(ierr, u=ulog, levv=lv, mode=chmd)
          if (ierr.eq.0) call nx_init(ierr, u=ulog, levv=lv, mode=chmd)
          if (ierr.eq.0) call nb_init(ierr, u=ulog, levv=lv, mode=chmd)
# if OPT_WITH_NCTCDF
          if (ierr.eq.0) call nn_init(ierr, u=ulog, levv=lv, mode=chmd)
# endif
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Nio_std,only: ns_diag=>diag
    use TOUZA_Nio_std,only: control_mode, control_deep
    use TOUZA_Nio_std,only: choice, is_first_force, get_logu
    use TOUZA_Nio_std,only: trace_control
    use TOUZA_Nio_std,only: is_msglev_NORMAL
    use TOUZA_Nio_std,only: msg
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: levv
    integer utmp, lv, md, lmd, chmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
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
          if (ierr.eq.0) call ns_diag(ierr, utmp, levv=lv, mode=lmd)
          ! if (ierr.eq.0) call nh_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call na_diag(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call nc_diag(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call np_diag(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call nx_diag(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call nb_diag(ierr, utmp, levv=lv, mode=chmd)
# if OPT_WITH_NCTCDF
          if (ierr.eq.0) call nn_diag(ierr, utmp, levv=lv, mode=chmd)
# endif
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Nio_std,only: ns_finalize=>finalize
    use TOUZA_Nio_std,only: control_mode, control_deep
    use TOUZA_Nio_std,only: choice, is_first_force, get_logu
    use TOUZA_Nio_std,only: trace_fine
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd, chmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
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
          if (ierr.eq.0) call ns_finalize(ierr, utmp, levv=lv, mode=lmd)
          ! if (ierr.eq.0) call nh_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call na_finalize(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call nc_finalize(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call np_finalize(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call nx_finalize(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call nb_finalize(ierr, utmp, levv=lv, mode=chmd)
# if OPT_WITH_NCTCDF
          if (ierr.eq.0) call nn_finalize(ierr, utmp, levv=lv, mode=chmd)
# endif
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + user subroutines
!!!_ + private subroutines
end module TOUZA_Nio

!!!_@ test_nio - test program
#ifdef TEST_NIO
program test_nio
  use TOUZA_Nio
  implicit none
  integer ierr

  ierr = 0
  if (ierr.eq.0) call init(ierr, levv=+9, stdv=+9)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call finalize(ierr, levv=+9)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_nio

#endif /* TEST_NIO */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

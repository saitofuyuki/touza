!!!_! nio_std.F90 - TOUZA/Nio utilities (and bridge to Std)
! Maintainer: SAITO Fuyuki
! Created: Nov 9 2021
#define TIME_STAMP 'Time-stamp: <2025/05/23 11:04:01 fuyuki nio_std.F90>'
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
!!!_@ TOUZA_Nio_std - Nio utilities
module TOUZA_Nio_std
!!!_ = declaration
!!!_  - modules
  use TOUZA_Std,only: KI32,             KI64,           KDBL,             KFLT
  use TOUZA_Std,only: choice,           choice_a,       condop,           upcase
  use TOUZA_Std,only: set_if_present
  use TOUZA_Std,only: control_deep,     control_mode,   is_first_force,   parse_number
  use TOUZA_Std,only: split_list,       join_list,      find_first
  use TOUZA_Std,only: is_msglev
  use TOUZA_Std,only: is_msglev_debug,  is_msglev_info, is_msglev_normal, is_msglev_detail
  use TOUZA_Std,only: is_msglev_severe, is_msglev_fatal,is_msglev_warning
  use TOUZA_Std,only: get_logu,         unit_global,    trace_fine,       trace_control
  use TOUZA_Std,only: trace_err
  use TOUZA_Std,only: is_error_match
  use TOUZA_Std,only: KIOFS
  use TOUZA_Std,only: nc_strm,          nbits_byte
  use TOUZA_Std,only: conv_b2strm,      get_size_bytes
  use TOUZA_Std,only: get_mems_bytes
  use TOUZA_Std,only: kendi_file,       kendi_mem,      check_bodr_unit,  check_byte_order
  use TOUZA_Std,only: endian_BIG,       endian_LITTLE,  endian_OTHER
  use TOUZA_Std,only: is_eof_ss
  use TOUZA_Std,only: new_unit,         search_from_last
  use TOUZA_Std,only: is_file_opened
  use TOUZA_Std,only: WHENCE_BEGIN,     WHENCE_ABS,     WHENCE_CURRENT,   WHENCE_END
  use TOUZA_Std,only: sus_open,         sus_close
  use TOUZA_Std,only: sus_write_irec,   sus_read_irec,  sus_skip_irec,    sus_pad_irec
  use TOUZA_Std,only: sus_write_lrec,   sus_read_lrec,  sus_skip_lrec
  use TOUZA_Std,only: sus_write_isep,   sus_read_isep
  use TOUZA_Std,only: sus_write_lsep,   sus_read_lsep
  use TOUZA_Std,only: sus_slice_read_irec, sus_runl_read_irec
  use TOUZA_Std,only: sus_list_read_irec,  sus_suspend_read_irec
  use TOUZA_Std,only: sus_rseek,        sus_eswap
  use TOUZA_Std,only: sus_size_irec
  use TOUZA_Std,only: sus_pos_a2rel,    sus_pos_r2abs,    sus_getpos
  use TOUZA_Std,only: max_members,      is_irec_overflow, sus_record_mems_irec
  use TOUZA_Std,only: def_block,        ignore_small,     ignore_bigger,  ignore_always
  use TOUZA_Std,only: sus_is_status_new
  use TOUZA_Std,only: set_slice_loop,   init_offset,      next_offset,    set_runl_loop
  use TOUZA_Std,only: debug_status
  use TOUZA_Std,only: new_htable,       reg_entry,        query_status
!!!_  - default
  implicit none
  private
!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = NIO_MSG_LEVEL
  integer,save :: lev_stdv    = NIO_MSG_LEVEL - 1
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global

!!!_  - common
  character(len=256) :: tmsg
# define __MDL__ 's'
#define _ERROR(E) (E - ERR_MASK_NIO_STD)
!!!_  - interfaces
  interface msg
     module procedure msg_txt
     module procedure msg_aa, msg_i, msg_ia
  end interface msg
!!!_  - public procedures
  public init,    diag, finalize
  public gen_tag, msg
!!!_   . TOUZA_Std
  public :: KI32,             KI64,           KDBL,             KFLT
  public :: choice,           choice_a,       condop,           upcase
  public :: set_if_present
  public :: control_deep,     control_mode,   is_first_force,   parse_number
  public :: split_list,       join_list,      find_first
  public :: is_msglev
  public :: is_msglev_debug,  is_msglev_info, is_msglev_normal, is_msglev_detail
  public :: is_msglev_severe, is_msglev_fatal,is_msglev_warning
  public :: get_logu,         unit_global,    trace_fine,       trace_control
  public :: trace_err
  public :: is_error_match
  public :: KIOFS
  public :: nc_strm,          nbits_byte
  public :: conv_b2strm,      get_size_bytes
  public :: get_mems_bytes
  public :: kendi_file,       kendi_mem,      check_bodr_unit,  check_byte_order
  public :: endian_BIG,       endian_LITTLE,  endian_OTHER
  public :: is_eof_ss
  public :: new_unit,         search_from_last
  public :: is_file_opened
  public :: WHENCE_BEGIN,     WHENCE_ABS,     WHENCE_CURRENT,   WHENCE_END
  public :: sus_open,         sus_close
  public :: sus_write_irec,   sus_read_irec,  sus_skip_irec,    sus_pad_irec
  public :: sus_write_lrec,   sus_read_lrec,  sus_skip_lrec
  public :: sus_write_isep,   sus_read_isep
  public :: sus_write_lsep,   sus_read_lsep
  public :: sus_slice_read_irec, sus_runl_read_irec
  public :: sus_list_read_irec,  sus_suspend_read_irec
  public :: sus_rseek,        sus_eswap
  public :: sus_size_irec
  public :: sus_pos_a2rel,    sus_pos_r2abs,    sus_getpos
  public :: max_members,      is_irec_overflow, sus_record_mems_irec
  public :: def_block,        ignore_small,     ignore_bigger,  ignore_always
  public :: sus_is_status_new
  public :: set_slice_loop,   init_offset,      next_offset,    set_runl_loop
  public :: debug_status
  public :: new_htable,       reg_entry,        query_status
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm)
    ! use TOUZA_Std,only: env_init
    use TOUZA_Std,only: sus_init, bld_init, htb_init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    integer,intent(in),optional :: icomm
    integer lv, md, lmd, tsmd

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
       if (md.ge.MODE_DEEP) then
          lev_stdv = choice(lev_stdv, stdv)
          tsmd = MODE_SURFACE
          ! if (ierr.eq.0) call env_init(ierr, u=ulog, levv=lev_stdv, mode=lmd, icomm=icomm) ! included by TOUZA_Std_sus
          if (ierr.eq.0) call sus_init(ierr, u=ulog, levv=lev_stdv, mode=lmd, icomm=icomm)
          if (ierr.eq.0) call bld_init(ierr, u=ulog, levv=lev_stdv, mode=tsmd)
          if (ierr.eq.0) call htb_init(ierr, u=ulog, levv=lev_stdv, mode=tsmd)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    ! use TOUZA_Std,only: env_diag
    use TOUZA_Std,only: sus_diag, bld_diag, htb_diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer utmp, lv, md, lmd, tsmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __MDL__, u=utmp)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_DEEP) then
          tsmd = MODE_SURFACE
          ! if (ierr.eq.0) call env_diag(ierr, utmp, levv=lev_stdv, mode=lmd)
          if (ierr.eq.0) call sus_diag(ierr, utmp, levv=lev_stdv, mode=lmd)
          if (ierr.eq.0) call bld_diag(ierr, utmp, levv=lev_stdv, mode=tsmd)
          if (ierr.eq.0) call htb_diag(ierr, utmp, levv=lev_stdv, mode=tsmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    ! use TOUZA_Std,only: env_finalize
    use TOUZA_Std,only: sus_finalize, bld_finalize, htb_finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd, tsmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_DEEP) then
          tsmd = MODE_SURFACE
          ! if (ierr.eq.0) call env_finalize(ierr, utmp, lev_stdv, mode=lmd)
          if (ierr.eq.0) call sus_finalize(ierr, utmp, lev_stdv, mode=lmd)
          if (ierr.eq.0) call bld_finalize(ierr, utmp, lev_stdv, mode=tsmd)
          if (ierr.eq.0) call htb_finalize(ierr, utmp, lev_stdv, mode=tsmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + user interfaces
!!!_  & gen_tag - tag generator (to override std)
  subroutine gen_tag &
       & (tag, mdl, fun, asfx, isfx, label)
    use TOUZA_Std,only: std_gen_tag=>gen_tag
    implicit none
    character(len=*),intent(out)         :: tag
    character(len=*),intent(in),optional :: mdl
    character(len=*),intent(in),optional :: fun
    character(len=*),intent(in),optional :: asfx
    integer,         intent(in),optional :: isfx
    logical,         intent(in),optional :: label
    call std_gen_tag &
         & (tag, pkg=PACKAGE_TAG, grp=__GRP__, mdl=mdl, fun=fun, asfx=asfx, isfx=isfx, label=label)
  end subroutine gen_tag
!!!_  & msg_txt - message dispatcher (to override std)
  subroutine msg_txt &
       & (txt, mdl, u)
    use TOUZA_Std,only: choice, std_msg=>msg
    implicit none
    character(len=*),intent(in)          :: txt
    character(len=*),intent(in),optional :: mdl
    integer,         intent(in),optional :: u
    character(len=1024) :: tag
    call gen_tag(tag, mdl)
    call std_msg(txt, tag, u)
    return
  end subroutine msg_txt
!!!_  & msg_aa - message dispatcher (to override std)
  subroutine msg_aa &
       & (fmt, v, mdl, u)
    use TOUZA_Std,only: choice
    implicit none
    character(len=*),intent(in)          :: fmt
    character(len=*),intent(in)          :: v(:)
    character(len=*),intent(in),optional :: mdl
    integer,         intent(in),optional :: u
    write(tmsg, fmt) v(:)
    call msg_txt(tmsg, mdl, u)
    return
  end subroutine msg_aa
!!!_  & msg_i - message dispatcher (to override std)
  subroutine msg_i &
       & (fmt, v, mdl, u)
    use TOUZA_Std,only: choice
    implicit none
    character(len=*),intent(in)          :: fmt
    integer,         intent(in)          :: v
    character(len=*),intent(in),optional :: mdl
    integer,         intent(in),optional :: u
    write(tmsg, fmt) v
    call msg_txt(tmsg, mdl, u)
    return
  end subroutine msg_i
!!!_  & msg_ia - message dispatcher (to override std)
  subroutine msg_ia &
       & (fmt, vv, mdl, u)
    implicit none
    character(len=*),intent(in)          :: fmt
    integer,         intent(in)          :: vv(:)
    character(len=*),intent(in),optional :: mdl
    integer,         intent(in),optional :: u
    write(tmsg, fmt) vv(:)
    call msg_txt(tmsg, mdl, u)
  end subroutine msg_ia

end module TOUZA_Nio_std

!!!_@ test_nio_std - test program
#ifdef TEST_NIO_STD
program test_nio_std
  use TOUZA_Nio_std
  implicit none
  integer ierr

101 format(A, ' = ', I0)
  call init(ierr, levv=+9, stdv=+9)
  write(*, 101) 'INIT', ierr

  if (ierr.eq.0) call diag(ierr)
  write(*, 101) 'DIAG', ierr

  if (ierr.eq.0) call finalize(ierr)
  write(*, 101) 'FINAL', ierr
  stop
end program test_nio_std

#endif /* TEST_NIO_STD */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

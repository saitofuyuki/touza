!!!_! nio_std.F90 - TOUZA/Nio utilities (and bridge to Std)
! Maintainer: SAITO Fuyuki
! Created: Nov 9 2021
#define TIME_STAMP 'Time-stamp: <2022/06/04 11:01:17 fuyuki nio_std.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021, 2022
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
  use TOUZA_Std_prc,only: KI32, KI64, KDBL, KFLT
  use TOUZA_Std_utl,only: &
       & choice, choice_a, condop, upcase, &
       & control_deep, control_mode, is_first_force
  use TOUZA_Std_log,only: &
       & is_msglev, &
       & is_msglev_debug,  is_msglev_info,   is_msglev_normal, is_msglev_detail, &
       & is_msglev_severe, is_msglev_fatal,  &
       & get_logu,         unit_global,      trace_fine,       trace_control
  use TOUZA_Std_env,only: &
       & KIOFS,           &
       & nc_strm,         nbits_byte, &
       & conv_b2strm,     get_size_bytes, &
       & get_mems_bytes,  get_size_strm,  &
       & kendi_file,      kendi_mem,      check_bodr_unit, check_byte_order, &
       & endian_BIG,      endian_LITTLE,  endian_OTHER,    &
       & is_eof_ss
  use TOUZA_Std_fun,only: new_unit
  use TOUZA_Std_sus,only: &
       & WHENCE_BEGIN,    WHENCE_ABS,     WHENCE_CURRENT,  WHENCE_END, &
       & sus_open,        sus_close, &
       & sus_write_irec,  sus_read_irec,  sus_skip_irec, &
       & sus_write_lrec,  sus_read_lrec,  sus_skip_lrec, &
       & sus_write_isep,  sus_read_isep,  &
       & sus_write_lsep,  sus_read_lsep,  &
       & sus_rseek,       sus_eswap
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
!!!_  - interfaces
  interface msg
     module procedure msg_txt
     module procedure msg_i, msg_ia
  end interface msg
!!!_  - public procedures
  public init, diag, finalize
  public msg
!!!_   . TOUZA_Std
  public KI32, KI64, KDBL, KFLT
  public choice, choice_a, condop, upcase
  public control_deep, control_mode, is_first_force
  public is_msglev
  public is_msglev_debug,  is_msglev_info,   is_msglev_normal, is_msglev_detail
  public is_msglev_severe, is_msglev_fatal
  public get_logu,         unit_global,      trace_fine,       trace_control
  public KIOFS
  public nc_strm,         nbits_byte
  public conv_b2strm,     get_size_bytes
  public get_mems_bytes,  get_size_strm
  public kendi_file,      kendi_mem,         check_bodr_unit,  check_byte_order
  public endian_BIG,      endian_LITTLE,     endian_OTHER
  public is_eof_ss
  public new_unit
  public WHENCE_BEGIN,    WHENCE_ABS,     WHENCE_CURRENT,  WHENCE_END
  public sus_open,        sus_close
  public sus_write_irec,  sus_read_irec,  sus_skip_irec
  public sus_write_lrec,  sus_read_lrec,  sus_skip_lrec
  public sus_write_isep,  sus_read_isep
  public sus_write_lsep,  sus_read_lsep
  public sus_rseek,       sus_eswap
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm)
    use TOUZA_Std_env,only: env_init=>init
    use TOUZA_Std_sus,only: sus_init=>init
    use TOUZA_Std_bld,only: bld_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    integer,intent(in),optional :: icomm
    integer lv, md, lmd

    ierr = 0

    md = control_mode(mode, MODE_DEEPEST)
    init_mode = md

    if (md.ge.MODE_SURFACE) then
       err_default = ERR_SUCCESS
       lv = choice(lev_verbose, levv)
       if (is_first_force(init_counts, md)) then
          ulog = choice(ulog, u)
          lev_verbose = lv
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_DEEP) then
          lev_stdv = choice(lev_stdv, stdv)
          if (ierr.eq.0) call bld_init(ierr, u=ulog, levv=lev_stdv, mode=lmd)
          if (ierr.eq.0) call env_init(ierr, u=ulog, levv=lev_stdv, mode=lmd, icomm=icomm)
          if (ierr.eq.0) call sus_init(ierr, u=ulog, levv=lev_stdv, mode=lmd, icomm=icomm)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std_env,only: env_diag=>diag
    use TOUZA_Std_sus,only: sus_diag=>diag
    use TOUZA_Std_bld,only: bld_diag=>diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, md)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __MDL__, utmp)
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call bld_diag(ierr, utmp, levv=lev_stdv, mode=lmd)
          if (ierr.eq.0) call env_diag(ierr, utmp, levv=lev_stdv, mode=lmd)
          if (ierr.eq.0) call sus_diag(ierr, utmp, levv=lev_stdv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_env,only: env_finalize=>finalize
    use TOUZA_Std_sus,only: sus_finalize=>finalize
    use TOUZA_Std_bld,only: bld_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, md)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call bld_finalize(ierr, utmp, lev_stdv, mode=lmd)
          if (ierr.eq.0) call env_finalize(ierr, utmp, lev_stdv, mode=lmd)
          if (ierr.eq.0) call sus_finalize(ierr, utmp, lev_stdv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + user interfaces
!!!_  & msg_txt - message dispatcher (to override std)
  subroutine msg_txt &
       & (txt, mdl, u)
    use TOUZA_Std,only: choice, std_msg=>msg, gen_tag
    implicit none
    character(len=*),intent(in)          :: txt
    character(len=*),intent(in),optional :: mdl
    integer,         intent(in),optional :: u
    character(len=1024) :: tag
    call gen_tag(tag, pkg=PACKAGE_TAG, grp=__GRP__, mdl=mdl)
    call std_msg(txt, tag, u)
    return
  end subroutine msg_txt
!!!_  & msg_i - message dispatcher (to override std)
  subroutine msg_i &
       & (fmt, v, mdl, u)
    use TOUZA_Std,only: choice, std_msg=>msg, gen_tag
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
  call init(ierr)
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

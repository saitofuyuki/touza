!!!_! ami_std.F90 - TOUZA/Ami bridge to Std
! Maintainer: SAITO Fuyuki
! Created: May 2 2022
#define TIME_STAMP 'Time-stamp: <2023/05/23 08:27:55 fuyuki ami_std.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022, 2023
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_ami.h"
!!!_@ TOUZA_Ami_std - Ami utilities
module TOUZA_Ami_std
!!!_ = declaration
!!!_  - modules
  use TOUZA_Std_prc,only: KDBL,         KFLT
  use TOUZA_Std_utl,only: choice,       choice_a,     set_if_present
  use TOUZA_Std_utl,only: control_deep, control_mode, is_first_force
  use TOUZA_Std_utl,only: find_first,   inrange
  use TOUZA_Std_log,only: is_msglev
  use TOUZA_Std_log,only: is_msglev_debug,  is_msglev_info,   is_msglev_normal, is_msglev_detail
  use TOUZA_Std_log,only: is_msglev_severe, is_msglev_fatal
  use TOUZA_Std_log,only: get_logu,         unit_global,      trace_fine,       trace_control
  use TOUZA_Std_env,only: conv_b2strm,      KIOFS
  use TOUZA_Std_env,only: endian_BIG,       endian_LITTLE
  use TOUZA_Std_fun,only: new_unit
  use TOUZA_Std_sus,only: sus_open, sus_close, sus_skip_irec
  use TOUZA_Std_sus,only: sus_read_isep,  sus_read_irec,  sus_read,  sus_suspend_read_irec
  use TOUZA_Std_sus,only: sus_write_isep, sus_write_irec, sus_write, sus_suspend_write_irec
  use TOUZA_Std_sus,only: suspend_begin, suspend_end
  use TOUZA_Std_sus,only: sus_record_mems_irec
  use TOUZA_Std_sus,only: sus_is_stream_unit

!!!_  - default
  implicit none
  private
!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = AMI_MSG_LEVEL
  integer,save :: lev_stdv    = AMI_MSG_LEVEL - 1
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
!!!_  - common
  character(len=256) :: tmsg
# define __MDL__ 's'
!!!_  - interfaces
  interface msg
     module procedure msg_txt
     module procedure msg_i, msg_ia, msg_aa
  end interface msg
!!!_  - public procedures
  public init, diag, finalize
  public msg
!!!_   . TOUZA_Std
  public KDBL,         KFLT
  public choice,       choice_a,     set_if_present
  public control_mode, control_deep, is_first_force
  public find_first,   inrange
  public is_msglev
  public is_msglev_debug,  is_msglev_info,   is_msglev_normal, is_msglev_detail
  public is_msglev_severe, is_msglev_fatal
  public get_logu,         unit_global,      trace_fine,       trace_control
  public conv_b2strm,      KIOFS
  public new_unit
  public sus_open, sus_close, sus_skip_irec
  public sus_read_isep,  sus_read_irec,  sus_read,  sus_suspend_read_irec
  public sus_write_isep, sus_write_irec, sus_write, sus_suspend_write_irec
  public suspend_begin, suspend_end
  public sus_record_mems_irec
  public sus_is_stream_unit
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm)
    use TOUZA_Std_mwe,only: mwe_init=>init
    use TOUZA_Std_env,only: env_init=>init
    use TOUZA_Std_fun,only: fun_init=>init
    use TOUZA_Std_sus,only: sus_init=>init
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
       if (is_first_force(init_counts, mode)) then
          ulog = choice(ulog, u)
          lev_verbose = lv
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_DEEP) then
          lev_stdv = choice(lev_stdv, stdv)
          if (ierr.eq.0) call mwe_init(ierr, u=ulog, levv=lev_stdv, mode=lmd, icomm=icomm)
          if (ierr.eq.0) call fun_init(ierr, u=ulog, levv=lev_stdv, mode=lmd, icomm=icomm)
          if (ierr.eq.0) call env_init(ierr, u=ulog, levv=lev_stdv, mode=lmd, icomm=icomm)
          if (ierr.eq.0) call sus_init(ierr, u=ulog, levv=lev_stdv, mode=lmd)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std_mwe,only: mwe_diag=>diag
    use TOUZA_Std_env,only: env_diag=>diag
    use TOUZA_Std_sus,only: sus_diag=>diag
    use TOUZA_Std_fun,only: fun_diag=>diag
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
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __MDL__, utmp)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call mwe_diag(ierr, utmp, levv=lev_stdv, mode=lmd)
          if (ierr.eq.0) call fun_diag(ierr, utmp, levv=lev_stdv, mode=lmd)
          if (ierr.eq.0) call env_diag(ierr, utmp, levv=lev_stdv, mode=lmd)
          if (ierr.eq.0) call sus_diag(ierr, utmp, levv=lev_stdv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_mwe,only: mwe_finalize=>finalize
    use TOUZA_Std_env,only: env_finalize=>finalize
    use TOUZA_Std_sus,only: sus_finalize=>finalize
    use TOUZA_Std_fun,only: fun_finalize=>finalize
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
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call env_finalize(ierr, utmp, lev_stdv, mode=lmd)
          if (ierr.eq.0) call mwe_finalize(ierr, utmp, lev_stdv, mode=lmd)
          if (ierr.eq.0) call sus_finalize(ierr, utmp, lev_stdv, mode=lmd)
          if (ierr.eq.0) call fun_finalize(ierr, utmp, lev_stdv, mode=lmd)
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
!!!_  & msg_aa - message dispatcher (to override std)
  subroutine msg_aa &
       & (fmt, vv, mdl, u)
    implicit none
    character(len=*),intent(in)          :: fmt
    character(len=*),intent(in)          :: vv(:)
    character(len=*),intent(in),optional :: mdl
    integer,         intent(in),optional :: u
    write(tmsg, fmt) vv(:)
    call msg_txt(tmsg, mdl, u)
  end subroutine msg_aa

end module TOUZA_Ami_std

!!!_@ test_ami_std - test program
#ifdef TEST_AMI_STD
program test_ami_std
  use TOUZA_Ami_std
  implicit none
  integer ierr

101 format(A, ' = ', I0)
  call init(ierr, stdv=+999)
  write(*, 101) 'INIT', ierr

  if (ierr.eq.0) call diag(ierr)
  write(*, 101) 'DIAG', ierr

  if (ierr.eq.0) call finalize(ierr)
  write(*, 101) 'FINAL', ierr
  stop
end program test_ami_std

#endif /* TEST_AMI_STD */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

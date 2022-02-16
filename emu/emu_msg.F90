!!!_! emu_msg.F90 - touza/emu usubs::msgs emulation
! Maintainer: SAITO Fuyuki
! Created: Feb 12 2022
#define TIME_STAMP 'Time-stamp: <2022/02/12 10:11:53 fuyuki emu_msg.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_emu.h"
#include "touza_std.h"
!!!_@ TOUZA_Emu_msg - usysio interfaces
module TOUZA_Emu_msg
!!!_ = declaration
!!!_  - modules
  use TOUZA_Std,only: &
       & control_mode, control_deep, is_first_force, &
       & get_logu,     unit_global,  trace_fine,   trace_control
!!!_  - default
  implicit none
  private
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = EMU_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
# define __MDL__ 'msg'
!!!_  - interfaces
  interface
     subroutine MSSET()
     end subroutine MSSET
     subroutine MSSUNI(IOUTZ)
       implicit none
       integer,intent(in) :: IOUTZ
     end subroutine MSSUNI
     subroutine MSGUNI(IOUTZ)
       implicit none
       integer,intent(out) :: IOUTZ
     end subroutine MSGUNI
     subroutine MSGA(HMSG)
       implicit none
       character(len=*),intent(in) :: HMSG
     end subroutine MSGA
     subroutine MSGB(HMSG, HMSG2)
       implicit none
       character(len=*),intent(in) :: HMSG
       character(len=*),intent(in) :: HMSG2
     end subroutine MSGB
     subroutine MSGC(HMSG, HMSG2, HMSG3)
       implicit none
       character(len=*),intent(in) :: HMSG
       character(len=*),intent(in) :: HMSG2
       character(len=*),intent(in) :: HMSG3
     end subroutine MSGC
     subroutine MSGD(HMSG, HMSG2, HMSG3, HMSG4)
       implicit none
       character(len=*),intent(in) :: HMSG
       character(len=*),intent(in) :: HMSG2
       character(len=*),intent(in) :: HMSG3
       character(len=*),intent(in) :: HMSG4
     end subroutine MSGD
     subroutine MSGAI(HMSG, IMSG)
       implicit none
       character(len=*),intent(in) :: HMSG
       integer,         intent(in) :: IMSG
     end subroutine MSGAI
     subroutine MSGBI(HMSG, HMSG2, IMSG)
       implicit none
       character(len=*),intent(in) :: HMSG
       character(len=*),intent(in) :: HMSG2
       integer,         intent(in) :: IMSG
     end subroutine MSGBI
     subroutine MSGAJ(HMSG, IMSG, IMSG2)
       implicit none
       character(len=*),intent(in) :: HMSG
       integer,         intent(in) :: IMSG
       integer,         intent(in) :: IMSG2
     end subroutine MSGAJ
     subroutine MSGBJ(HMSG, HMSG2, IMSG, IMSG2)
       implicit none
       character(len=*),intent(in) :: HMSG
       character(len=*),intent(in) :: HMSG2
       integer,         intent(in) :: IMSG
       integer,         intent(in) :: IMSG2
     end subroutine MSGBJ
     subroutine MSGAK(HMSG, IMSG, IMSG2, IMSG3)
       implicit none
       character(len=*),intent(in) :: HMSG
       integer,         intent(in) :: IMSG
       integer,         intent(in) :: IMSG2
       integer,         intent(in) :: IMSG3
     end subroutine MSGAK
     subroutine MSGBOX(HSTR0)
       implicit none
       character(len=*),intent(in) :: HSTR0
     end subroutine MSGBOX
  end interface
!!!_  - public
  public init, diag, finalize
  public MSSET, MSSUNI, MSGUNI
  public MSGA,  MSGB,   MSGC,  MSGD
  public MSGAI, MSGBI,  MSGAJ, MSGBJ, MSGAK
  public MSGBOX
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv)
    use TOUZA_Std,only: &
         & msg_grp, choice, log_init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer,intent(in),optional :: stdv
    integer lv, md, lmd

    ierr = 0

    md = control_mode(mode, MODE_DEEPER)
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
          if (ierr.eq.0) call log_init(ierr, u=ulog, levv=stdv, mode=lmd)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init
!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std,only: choice, is_msglev_NORMAL, msg_grp, &
         & log_diag
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
       call trace_control &
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, md)) then
          if (is_msglev_NORMAL(lv)) then
             if (ierr.eq.0) then
                call msg_grp(TIME_STAMP, __GRP__, __MDL__, utmp)
             endif
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call log_diag(ierr, u=utmp, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std,only: choice, &
         & log_finalize
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
          if (ierr.eq.0) call log_finalize(ierr, u=utmp, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize
!!!_ + user subroutines
!!!_ + end TOUZA_Emu_msg
end module TOUZA_Emu_msg

!!!_* non-module Procedures
!!!_ + MSSET - set unit no. for message as default
subroutine MSSET()
  call GETJFP(IOUT)
  return
end subroutine MSSET
!!!_ + MSSUNI - set unit no.
subroutine MSSUNI(IOUTZ)
  implicit none
  integer,intent(in) :: IOUTZ
  return
end subroutine MSSUNI
!!!_ + MSGUNI - get unit no.
subroutine MSGUNI(IOUTZ)
  implicit none
  integer,intent(out) :: IOUTZ
  return
end subroutine MSGUNI
!!!_ + MSGA - message(str)
subroutine MSGA(HMSG)
  implicit none
  character(len=*),intent(in) :: HMSG
  return
end subroutine MSGA
!!!_ + MSGB - message(str, str)
subroutine MSGB(HMSG, HMSG2)
  implicit none
  character(len=*),intent(in) :: HMSG
  character(len=*),intent(in) :: HMSG2
  return
end subroutine MSGB
!!!_ + MSGC - message(str, str, str)
subroutine MSGC(HMSG, HMSG2, HMSG3)
  implicit none
  character(len=*),intent(in) :: HMSG
  character(len=*),intent(in) :: HMSG2
  character(len=*),intent(in) :: HMSG3
  return
end subroutine MSGC
!!!_ + MSGD - message(str, str, str, str)
subroutine MSGD(HMSG, HMSG2, HMSG3, HMSG4)
  implicit none
  character(len=*),intent(in) :: HMSG
  character(len=*),intent(in) :: HMSG2
  character(len=*),intent(in) :: HMSG3
  character(len=*),intent(in) :: HMSG4
  return
end subroutine MSGD
!!!_ + MSGAI - message(str, int)
subroutine MSGAI(HMSG, IMSG)
  implicit none
  character(len=*),intent(in) :: HMSG
  integer,         intent(in) :: IMSG
  return
end subroutine MSGAI
!!!_ + MSGBI - message(str, str, int)
subroutine MSGBI(HMSG, HMSG2, IMSG)
  implicit none
  character(len=*),intent(in) :: HMSG
  character(len=*),intent(in) :: HMSG2
  integer,         intent(in) :: IMSG
  return
end subroutine MSGBI
!!!_ + MSGAJ - message(str, int, int)
subroutine MSGAJ(HMSG, IMSG, IMSG2)
  implicit none
  character(len=*),intent(in) :: HMSG
  integer,         intent(in) :: IMSG
  integer,         intent(in) :: IMSG2
  return
end subroutine MSGAJ
!!!_ + MSGBJ - message(str, str, int, int)
subroutine MSGBJ(HMSG, HMSG2, IMSG, IMSG2)
  implicit none
  character(len=*),intent(in) :: HMSG
  character(len=*),intent(in) :: HMSG2
  integer,         intent(in) :: IMSG
  integer,         intent(in) :: IMSG2
  return
end subroutine MSGBJ
!!!_ + MSGAK - message(str, int, int, int)
subroutine MSGAK(HMSG, IMSG, IMSG2, IMSG3)
  implicit none
  character(len=*),intent(in) :: HMSG
  integer,         intent(in) :: IMSG
  integer,         intent(in) :: IMSG2
  integer,         intent(in) :: IMSG3
  return
end subroutine MSGAK
!!!_ + MSGBOX
subroutine MSGBOX(HSTR0)
  implicit none
  character(len=*),intent(in) :: HSTR0
  return
end subroutine MSGBOX
!!!_@ test_emu_msg - test program
#ifdef TEST_EMU_MSG
program test_emu_msg
  use TOUZA_Emu_msg
  implicit none
  integer ierr
  integer ir

  ierr = 0
101 format(A, ' = ', I0)

  call init(ierr, u=-1, levv=9, stdv=-1)
  call diag(ierr)
  call finalize(ierr)
  write(*, 101) 'fine', ierr
  stop
end program test_emu_msg

#endif /* TEST_EMU_MSG */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

!!!_! emu_usi.F90 - touza/emu usysio emulation
! Maintainer: SAITO Fuyuki
! Created: May 30 2020
#define TIME_STAMP 'Time-stamp: <2024/02/01 11:12:21 fuyuki emu_usi.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2020-2024
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
!!!_@ TOUZA_Emu_usi - usysio interfaces
module TOUZA_Emu_usi
!!!_ = declaration
!!!_  - modules
  use TOUZA_Std,only: get_logu,     unit_global,  trace_fine,   trace_control
!!!_  - default
  implicit none
  private
!!!_  - parameter
  integer,parameter :: lpath = 128
  integer,parameter :: max_digits = 11
  integer,parameter :: lmsg = 256
!!!_  - static
  integer,save :: pos_arg = -1

  logical,save :: sw_stdi = .FALSE.
  logical,save :: sw_stdo = .FALSE.

  character(len=lpath),save :: pfx_sysin  = 'SYSIN'
  character(len=lpath),save :: sfx_sysin  = '.CL'
  character(len=lpath),save :: pfx_sysout = 'SYSOUT'
  character(len=lpath),save :: sfx_sysout = '.PE'
  integer,save              :: digits_sysin  = 1
  integer,save              :: digits_sysout = 3

  integer,save,public :: ISET=-1, JSET=-1
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = EMU_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global

  logical,save :: lock_rewind = .FALSE.
  character(len=16),save :: lock_tag = ' '
# define __MDL__ 'usi'
!!!_  - public
  integer,save,public :: IRANK = -1, NRANK = -1
  integer,save,public :: icolor = -1, ncolor = -1

  integer,save,public :: IFILE = -1, JFILE = -1
  integer,save,public :: CROOT = 0
!!!_  - interfaces
  interface
     subroutine SETNML(IFILE, JFILE)
       implicit none
       integer,intent(in) :: IFILE, JFILE
     end subroutine SETNML

     subroutine SETCLR(MYCOLR)
       implicit none
       integer,intent(in) :: MYCOLR
     end subroutine SETCLR

     subroutine SETRNK(MYRANK)
       implicit none
       integer,intent(in) :: MYRANK
     end subroutine SETRNK

     subroutine SETSIZ(MYSIZE)
       implicit none
       integer,intent(in) :: MYSIZE
     end subroutine SETSIZ

     subroutine OPNNML(IOS)
       implicit none
       integer,intent(out) :: IOS   !! io status
     end subroutine OPNNML

     subroutine REWNML(IFPAR, JFPAR)
       implicit none
       integer,intent(out) :: IFPAR, JFPAR
     end subroutine REWNML

     subroutine GETIFP(IFPAR)
       implicit none
       integer,intent(out) :: IFPAR
     end subroutine GETIFP

     subroutine GETJFP(JFPAR)
       implicit none
       integer,intent(out) :: JFPAR
     end subroutine GETJFP

  end interface

!!!_  - public
  public init, diag, finalize
  public update_color, update_ranks
  public open_sysin,   open_sysout
  public get_sysu
  public is_locked_rewind, rewind_lock, rewind_unlock
  public show_lock_status
  public SETNML, SETCLR, SETRNK, SETSIZ
  public OPNNML, REWNML, GETIFP, GETJFP
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm, pos, stdi, stdo)
    use TOUZA_Std,only: control_mode, control_deep, is_first_force
    use TOUZA_Std,only: &
         & msg_grp, choice, mwe_init, arg_init, log_init, env_init, fun_init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer,intent(in),optional :: stdv
    integer,intent(in),optional :: icomm
    integer,intent(in),optional :: pos
    logical,intent(in),optional :: stdi, stdo
    integer lv, md, lmd

    ierr = 0

    md = control_mode(mode, MODE_DEEPER)
    init_mode = md
    if (md.ge.MODE_SURFACE) then
       err_default = ERR_SUCCESS
       lv = choice(lev_verbose, levv)

       if (is_first_force(init_counts, mode)) then
          ulog = choice(ulog, u)
          lev_verbose = lv
          pos_arg  = choice(pos_arg, pos)
          sw_stdi  = choice(sw_stdi, stdi)
          sw_stdo  = choice(sw_stdo, stdo)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call mwe_init(ierr, u=ulog, levv=stdv, mode=lmd, icomm=icomm)
          if (ierr.eq.0) call env_init(ierr, u=ulog, levv=stdv, mode=lmd, icomm=icomm)
          if (ierr.eq.0) call fun_init(ierr, u=ulog, levv=stdv, mode=lmd, icomm=icomm)
          if (ierr.eq.0) call arg_init(ierr, u=ulog, levv=stdv, mode=lmd)
          if (ierr.eq.0) call log_init(ierr, u=ulog, levv=stdv, mode=lmd)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init
!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std,only: control_mode, control_deep, is_first_force
    use TOUZA_Std,only: choice, is_msglev_NORMAL, msg_grp, &
         & arg_diag, mwe_diag, log_diag, env_diag, fun_diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd
    character(len=lpath) :: file
    integer jerr

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (is_msglev_NORMAL(lv)) then
             if (ierr.eq.0) then
                call msg_grp(TIME_STAMP, __GRP__, __MDL__, utmp)
                call msg_grp('(''sysin unit = '', I0)', (/IFILE/), __GRP__, __MDL__, utmp)
                if (IFILE.ge.0) then
                   inquire(NAME=file, UNIT=IFILE, IOSTAT=jerr)
                   if (jerr.eq.0) &
                        & call msg_grp('(''sysin file = '', A)', (/file/), __GRP__, __MDL__, utmp)
                endif
                call msg_grp('(''sysout unit = '', I0)', (/JFILE/), __GRP__, __MDL__, utmp)
                if (JFILE.ge.0) then
                   inquire(NAME=file, UNIT=JFILE, IOSTAT=jerr)
                   if (jerr.eq.0) &
                        & call msg_grp('(''sysout file = '', A)', (/file/), __GRP__, __MDL__, utmp)
                endif
             endif
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call mwe_diag(ierr, u=utmp, mode=lmd)
          if (ierr.eq.0) call arg_diag(ierr, u=utmp, mode=lmd)
          if (ierr.eq.0) call log_diag(ierr, u=utmp, mode=lmd)
          if (ierr.eq.0) call fun_diag(ierr, u=utmp, mode=lmd)
          if (ierr.eq.0) call env_diag(ierr, u=utmp, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std,only: control_mode, control_deep, is_first_force
    use TOUZA_Std,only: choice, &
         & mwe_finalize, arg_finalize, log_finalize, env_finalize, fun_finalize
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
          if (ierr.eq.0) call mwe_finalize(ierr, u=utmp, mode=lmd)
          if (ierr.eq.0) call arg_finalize(ierr, u=utmp, mode=lmd)
          if (ierr.eq.0) call log_finalize(ierr, u=utmp, mode=lmd)
          if (ierr.eq.0) call fun_finalize(ierr, u=utmp, mode=lmd)
          if (ierr.eq.0) call env_finalize(ierr, u=utmp, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize
!!!_ + user subroutines
!!!_  & update_color
  subroutine update_color(icol, ncol, icomm)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(in)          :: icol
    integer,intent(in),optional :: ncol
    integer,intent(in),optional :: icomm
    icolor = icol
    ncolor = choice(ncolor, ncol)
    CROOT  = choice(CROOT, icomm)
    return
  end subroutine update_color
!!!_  & update_ranks
  subroutine update_ranks(ir, nr)
    use TOUZA_Std,only: choice
    integer,intent(in),optional :: ir
    integer,intent(in),optional :: nr
    irank = choice(irank, ir)
    nrank = choice(nrank, nr)
  end subroutine update_ranks

!!!_  & open_sysin - YYSYSI compatible
  subroutine open_sysin(ierr)
    use TOUZA_Std,only: &
         & choice, &
         & parse, get_nparam, get_param, &
         & msg_grp, get_wni, uin, is_msglev_INFO, new_unit
    implicit none
    integer,intent(out)         :: ierr
    character(len=lpath) :: file, pfx
    integer jp, np, na
    integer nrw
    integer jz
    logical bo
    integer utmp

    ierr = 0
    file = ' '
    na = 0
    jp = -1

    if (ierr.eq.0) call parse(ierr)
    if (ierr.eq.0) then
       jp = max(1, pos_arg)
       np = get_nparam()
       na = np - (jp - 1)
       if ((na.gt.1) &
            & .and. ((ncolor.gt.0.and.ncolor.gt.na) &
            &        .or.(icolor.ge.0.and.icolor.gt.na))) then
          call msg_grp('(''insufficient command-line argument: '', I0, 1x, I0, 1x, I0)', &
               &       (/icolor, ncolor, na/), __GRP__, __MDL__, ulog)
          ierr = -1
       endif
    endif
    if (ierr.eq.0) then
       if (na.eq.1) then
          call get_param(ierr, file, jp, ' ')
       else
          call get_param(ierr, file, jp + max(0, icolor), ' ')
       endif
    endif
    if (ierr.eq.0) then
       if (file.eq.' '.and.sw_stdi) file = '-'
       if (file.eq.'-') then
          ! stdin; warn if non-single ranks
          call get_wni(ierr, nrank=nrw)
          if (nrw.gt.1.or.nrw.lt.0) call msg_grp('stdin enabled.', __GRP__, __MDL__, ulog)
       else if (file.eq.' ') then
          call search_sysin(ierr, file, pfx_sysin, sfx_sysin, icolor, digits_sysin)
          if (file.eq.' ') ierr = -1
       else
          jz = verify(file, '0', .TRUE.)
          if (jz.eq.0) then
             pfx = file
             call search_sysin(ierr, file, pfx, sfx_sysin, icolor, digits_sysin)
          else
             if (icolor.lt.0) then
                pfx = file
                call search_sysin(ierr, file, pfx, ' ', icolor)
             else
                pfx = file(1:jz)
                call search_sysin(ierr, file, pfx, ' ', icolor, digits_sysin)
             endif
          endif
       endif
    endif
    if (file.eq.' ') ierr = -1
    if (ierr.eq.0) then
       if (IFILE.ge.0) then
          if (is_msglev_INFO(lev_verbose)) then
             call msg_grp('(''old sysin unit: '', I0)', (/IFILE/), __GRP__, __MDL__, ulog)
          endif
       endif
       if (file.eq.'-') then
          IFILE = uin
       else
          inquire(NUMBER=utmp, OPENED=bo, FILE=file, IOSTAT=ierr)
          if (ierr.eq.0) then
             if (.not.bo) then
                utmp = ISET
                if (utmp.lt.0) utmp = new_unit()
                if (utmp.lt.0) then
                   ierr = -1
                else
                   if (IFILE.ne.uin.and.IFILE.ge.0) close(IFILE)
                   open(unit=utmp, FILE=file, FORM='FORMATTED', STATUS='OLD', &
                        & ACCESS='SEQUENTIAL', IOSTAT=ierr)
                endif
             endif
             IFILE = utmp
          endif
       endif
       if (is_msglev_INFO(lev_verbose)) then
          call msg_grp('(''new sysin unit: '', I0)', (/IFILE/), __GRP__, __MDL__, ulog)
       endif
    endif
    return
  end subroutine open_sysin
!!!_  & get_sysu
  subroutine get_sysu(sysi, syso)
    implicit none
    integer,intent(out),optional :: sysi
    integer,intent(out),optional :: syso
    if (present(sysi)) then
       sysi = IFILE
    endif
    if (present(syso)) then
       syso = JFILE
    endif
    return
  end subroutine get_sysu
!!!_  & open_sysout - YYSYSO compatible
  subroutine open_sysout(ierr)
    use TOUZA_Std,only: &
         & choice, ndigits, &
         & msg_grp, get_wni, uout, is_msglev_INFO, new_unit
    implicit none
    integer,intent(out)         :: ierr
    character(len=lpath) :: file
    character(len=lmsg)  :: txt
    integer nr, ir
    integer nd
    integer utmp
    logical bo

    ierr = 0
    file = ' '

    if (ierr.eq.0) then
       if (JFILE.ge.0) then
          if (is_msglev_INFO(lev_verbose)) then
             call msg_grp('(''old sysout unit: '', I0)', (/JFILE/), __GRP__, __MDL__)
          endif
       endif
       if (sw_stdo) then
          file = '-'
          JFILE = uout
       else
          nr = nrank
          ir = irank
          if (nr.lt.0.or.ir.lt.0) then
             call get_wni(ierr, nrank=nr, irank=ir)
          endif
          if (nr.ne.0) then
             nd = max(ndigits(nr), digits_sysout)
             call gen_path(file, pfx_sysout, sfx_sysout, max(0, ir), nd)
          else
             file = pfx_sysout
          endif
          inquire(NUMBER=utmp, OPENED=bo, FILE=file, IOSTAT=ierr)
          if (ierr.eq.0) then
             if (.not.bo) then
                utmp = JSET
                if (utmp.lt.0) utmp = new_unit()
                if (utmp.lt.0) then
                   ierr = -1
                else
                   if (JFILE.ne.uout.and.JFILE.ge.0) close(JFILE)
                   open(unit=utmp, FILE=file, FORM='FORMATTED', &
                        & ACCESS='SEQUENTIAL', ACTION='WRITE', IOSTAT=ierr)
                endif
             endif
             JFILE = utmp
          endif
       endif
       if (is_msglev_INFO(lev_verbose)) then
          call msg_grp('(''new sysout unit: '', I0)', (/JFILE/), __GRP__, __MDL__)
       endif
    endif
    if (is_msglev_INFO(lev_verbose)) then
104    format('result:sysout=', A)
       write(txt, 104) trim(file)
       call msg_grp(txt, __GRP__, __MDL__)
    endif

    return
  end subroutine open_sysout

!!!_  & search_sysin
  subroutine search_sysin &
       & (ierr, file, pfx, sfx, num, digits)
    use TOUZA_Std,only: msg_grp, is_msglev_INFO, is_msglev_DEBUG
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: file
    character(len=*),intent(in)  :: pfx, sfx
    integer,         intent(in)  :: num
    integer,optional,intent(in)  :: digits
    integer nf
    integer j
    logical bx
    character(len=lpath) :: path
    character(len=lmsg)  :: txt
    ierr = 0
    nf = 0
    file = ' '
    if (present(digits)) then
       do j = max(1, digits), max_digits
          call gen_path(path, pfx, sfx, (max(0, num)), j)
          if (is_msglev_DEBUG(lev_verbose)) then
             call msg_grp('(''search '', A)', (/path/), __GRP__, __MDL__)
          endif
          if (ierr.eq.0) inquire(FILE=path, EXIST=bx, IOSTAT=ierr)
          if (ierr.eq.0.and.bx) then
             nf = nf + 1
             if (file.eq.' ') then
                file = path
             else
101             format('multiple candidates for sysin: ', A)
                if (nf.eq.2) then
                   write(txt, 101) trim(file)
                   call msg_grp(txt, __GRP__, __MDL__)
                endif
                write(txt, 101) trim(path)
                call msg_grp(txt, __GRP__, __MDL__)
             endif
          endif
       enddo
    endif
    if (ierr.eq.0) inquire(FILE=pfx, EXIST=bx, IOSTAT=ierr)
    if (ierr.eq.0) then
       if (is_msglev_DEBUG(lev_verbose)) then
          call msg_grp('(''search '', A)', (/pfx/), __GRP__, __MDL__)
       endif
102    format('ignore sysin candidate (rank=', I0, '): ', A)
       if (bx) then
          if (num.lt.0) then
             if (file.ne.' ') then
                write(txt, 102) num, trim(file)
                call msg_grp(txt, __GRP__, __MDL__)
             endif
             file = pfx
          else if (nf.eq.0) then
             file = pfx
          else if (nf.eq.1) then
             write(txt, 102) num, trim(pfx)
             call msg_grp(txt, __GRP__, __MDL__)
          else
             call msg_grp('too much sysin candidates', __GRP__, __MDL__)
             ierr = -1
          endif
       else if (nf.gt.1) then
          call msg_grp('too much sysin candidates', __GRP__, __MDL__)
          ierr = -1
       endif
    endif
    if (file.eq.' ') then
103    format('not found sysin: ', A, 1x, A, 1x, I0)
       write(txt, 103) trim(pfx), trim(sfx), num
       call msg_grp(txt, __GRP__, __MDL__)
       ierr = -1
    else if (is_msglev_INFO(lev_verbose)) then
104    format('result:sysin=', A)
       write(txt, 104) trim(file)
       call msg_grp(txt, __GRP__, __MDL__)
    endif
  end subroutine search_sysin
!!!_  & gen_path
  subroutine gen_path &
       & (path, pfx, sfx, num, mlow)
    implicit none
    character(len=*),intent(out) :: path
    character(len=*),intent(in)  :: pfx, sfx
    integer,         intent(in)  :: num, mlow
    integer,parameter :: lint=11
    character(len=lint) :: dbuf
    integer jz
    ! the format need manually adjust with max_digits
    write(dbuf, '(I11.11)') abs(num)
    if (num.eq.0) then
       jz = max(1, lint - mlow + 1)
    else
       jz = verify(dbuf, '0')
       jz = max(1, min(lint - mlow + 1, jz))
    endif
    path = trim(pfx) // trim(sfx) // dbuf(jz:lint)
  end subroutine gen_path
!!!_  - rewind_lock
  subroutine rewind_lock(tag, u)
    use TOUZA_Std,only: msg_grp
    implicit none
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    character(len=128) :: txt
    integer utmp
    lock_rewind = .TRUE.
    if (present(tag)) then
       utmp = get_logu(u, ulog)
101    format('rewind locked at ', A)
       if (tag.ne.' ') then
          write(txt, 101) trim(tag)
          lock_tag = trim(tag)
       else
          txt = 'rewind locked'
       endif
       call msg_grp(txt, __GRP__, __MDL__, utmp)
    endif
  end subroutine rewind_lock

!!!_  - rewind_unlock
  subroutine rewind_unlock(tag, u)
    use TOUZA_Std,only: msg_grp
    implicit none
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    character(len=128) :: txt
    integer utmp
    lock_rewind = .FALSE.
    if (present(tag)) then
       utmp = get_logu(u, ulog)
101    format('rewind unlocked at ', A)
       if (tag.ne.' ') then
          write(txt, 101) trim(tag)
       else
          txt = 'rewind unlocked'
       endif
       call msg_grp(txt, __GRP__, __MDL__, utmp)
    else if (lock_tag.ne.' ') then
       write(txt, 101) trim(lock_tag)
       call msg_grp(txt, __GRP__, __MDL__, utmp)
    endif
    lock_tag = ' '
  end subroutine rewind_unlock

!!!_  - is_locked_rewind
  logical function is_locked_rewind() result(b)
    implicit none
    b = lock_rewind
  end function is_locked_rewind
!!!_  - show_lock_status
  subroutine show_lock_status(u)
    use TOUZA_Std,only: msg_grp
    implicit none
    integer,intent(in),optional :: u
    integer utmp
    character(len=128) :: txt
    utmp = get_logu(u, ulog)
    if (lock_rewind) then
101    format('rewind locked at ', A)
       if (lock_tag.eq.' ') then
          txt = 'rewind locked'
       else
          write(txt, 101) trim(lock_tag)
       endif
    else
       txt = 'rewind unlocked'
    endif
    call msg_grp(txt, __GRP__, __MDL__, utmp)
  end subroutine show_lock_status

!!!_  - end TOUZA_Emu_usi
end module TOUZA_Emu_usi

!!!_* non-module Procedures
!!!_ + OPNNML - open SYSIN, SYSOUT
subroutine OPNNML(IOS)
  use TOUZA_Emu_usi,only: open_sysin, open_sysout
  implicit none
  integer,intent(out) :: IOS   !! io status
  integer jerri, jerro
  call open_sysin(jerri)
  call open_sysout(jerro)
  IOS = jerri
  if (IOS.eq.0) IOS = jerro
  return
end subroutine OPNNML

!!!_ + REWNML - rewind input and return input/output units
subroutine REWNML(IFPAR, JFPAR)
  use TOUZA_Emu_usi,only: get_sysu, is_locked_rewind, show_lock_status
  implicit none
  integer,intent(out) :: IFPAR, JFPAR
  integer jerr
  call get_sysu(sysi=IFPAR, syso=JFPAR)
  if (IFPAR.ge.0) then
     if (is_locked_rewind()) then
        call show_lock_status(JFPAR)
        write(JFPAR, *) 'PANIC.  ABORT.'
        stop
     else
        REWIND(unit=IFPAR, IOSTAT=jerr)
     endif
  endif
  RETURN
END subroutine REWNML

!!!_ + GETIFP - return input unit
subroutine GETIFP(IFPAR)
  use TOUZA_Emu_usi,only: get_sysu
  implicit none
  integer,intent(out) :: IFPAR
  call get_sysu(sysi=IFPAR)
  RETURN
END subroutine GETIFP

!!!_ + GETJFP - return output unit
subroutine GETJFP(JFPAR)
  use TOUZA_Emu_usi,only: get_sysu
  implicit none
  integer,intent(out) :: JFPAR
  call get_sysu(syso=JFPAR)
  RETURN
END subroutine GETJFP

!!!_ + SETNML - set SYSIN, SYSOUT logical unit num.
subroutine SETNML(IFILE, JFILE)
  use TOUZA_Emu_usi,only: emu_iset=>iset, emu_jset=>jset
  implicit none
  integer,intent(in) :: IFILE, JFILE
  emu_iset = IFILE
  emu_jset = JFILE
  return
end subroutine SETNML

!!!_ + SETCLR - set color index eused to make SYSIN filename
subroutine SETCLR(MYCOLR)
  use TOUZA_Emu_usi,only: update_color
  implicit none
  integer,intent(in) :: MYCOLR
  call update_color(icol=MYCOLR)
  return
end subroutine SETCLR

!!!_ + SETRNK - set myrank used to make SYSOUT filename
subroutine SETRNK(MYRANK)
  use TOUZA_Emu_usi,only: update_ranks
  implicit none
  integer,intent(in) :: MYRANK
  call update_ranks(ir=MYRANK)
  return
end subroutine SETRNK

!!!_ + SETSIZ - set number of ranks used to make SYSOUT filename
subroutine SETSIZ(MYSIZE)
  use TOUZA_Emu_usi,only: update_ranks
  implicit none
  integer,intent(in) :: MYSIZE
  call update_ranks(nr=MYSIZE)
  return
end subroutine SETSIZ

!!!_@ test_emu_usi - test program
#ifdef TEST_EMU_USI
program test_emu_usi
  use TOUZA_std,only: get_wni
  use TOUZA_Emu_usi
  implicit none
  integer ierr
  integer ir

  ierr = 0
101 format(A, ' = ', I0)

  call init(ierr, u=-1, levv=9, stdv=-1)
  if (ierr.eq.0) call SETNML(98, 99)

  if (ierr.eq.0) call open_sysin(ierr)
  write(*, 101) 'open_sysin', ierr

  if (ierr.eq.0) call get_wni(ierr, irank=ir)
  if (ierr.eq.0) call update_color(ir)
  if (ierr.eq.0) call open_sysin(ierr)
  write(*, 101) 'open_sysin', ierr
  if (ierr.eq.0) call open_sysout(ierr)
  write(*, 101) 'open_sysout', ierr
  call diag(ierr)
  call finalize(ierr)
  write(*, 101) 'fine', ierr
  stop
end program test_emu_usi

#endif /* TEST_EMU_USI */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

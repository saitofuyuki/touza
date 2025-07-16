!!!_! emu_usi.F90 - touza/emu usysio emulation
! Maintainer: SAITO Fuyuki
! Created: May 30 2020
#define TIME_STAMP 'Time-stamp: <2025/07/17 09:33:00 fuyuki emu_usi.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2020-2025
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
  use TOUZA_Std,only: MPI_COMM_NULL
!!!_  - default
  implicit none
  private
!!!_  - parameter
  integer,parameter :: lpath = 128
  integer,parameter :: max_digits = 11
  integer,parameter :: lmsg = 256

  character(len=*),parameter :: sp_stdin = '-'
  character(len=*),parameter :: sp_dummy = '--'
  character(len=*),parameter :: sp_stdout = '-'

!!!_  - static
  integer,save :: pos_arg = -1          ! negative to ignore command-line arguments

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
  integer,save,public :: CROOT = MPI_COMM_NULL
!!!_  - interfaces
  interface
     subroutine SETNML(IFILE, JFILE)
       implicit none
       integer,intent(in) :: IFILE, JFILE
     end subroutine SETNML

     subroutine SETBCM(MYCOMM)
       implicit none
       integer,intent(in) :: MYCOMM
     end subroutine SETBCM

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
!!!_   . private interface
  interface msg_grp
     module procedure msg_grp_ia
     module procedure msg_grp_aa
     module procedure msg_grp_txt
  end interface msg_grp
!!!_  - public
  public init, diag, finalize
  public update_color, update_ranks
  public open_bind_sysin, open_bind_sysout
  public open_sysin_primary, search_sysin_colored
  public get_sysu
  public is_locked_rewind, rewind_lock, rewind_unlock
  public show_lock_status
!!!_   . legacy procedures
  public legacy_open_sysin, legacy_open_sysout
!!!_   . miroc compatible interfaces
  public SETNML, SETBCM, SETCLR, SETRNK, SETSIZ
  public OPNNML, REWNML, GETIFP, GETJFP
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm, pos, stdi, stdo)
    use TOUZA_Std,only: control_mode, control_deep, is_first_force
    use TOUZA_Std,only: choice, mwe_init, arg_init, env_init, fun_init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer,intent(in),optional :: stdv
    integer,intent(in),optional :: icomm
    integer,intent(in),optional :: pos
    logical,intent(in),optional :: stdi, stdo
    integer lv, md, lmd
    integer tsmd

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
          tsmd = MODE_SURFACE
          if (ierr.eq.0) call mwe_init(ierr, u=ulog, levv=stdv, mode=lmd, icomm=icomm)
          if (ierr.eq.0) call env_init(ierr, u=ulog, levv=stdv, mode=tsmd, icomm=icomm)
          if (ierr.eq.0) call fun_init(ierr, u=ulog, levv=stdv, mode=tsmd, icomm=icomm)
          if (ierr.eq.0) call arg_init(ierr, u=ulog, levv=stdv, mode=tsmd)
          ! if (ierr.eq.0) call log_init(ierr, u=ulog, levv=stdv, mode=lmd)   !! mwe
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init
!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std,only: control_mode, control_deep, is_first_force
    use TOUZA_Std,only: choice, is_msglev_NORMAL
    use TOUZA_Std,only: arg_diag, mwe_diag, env_diag, fun_diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd
    integer tsmd
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
                call msg_grp(TIME_STAMP, utmp)
                call msg_grp('(''sysin unit = '', I0)', (/IFILE/), utmp)
                if (IFILE.ge.0) then
                   inquire(NAME=file, UNIT=IFILE, IOSTAT=jerr)
                   if (jerr.eq.0) &
                        & call msg_grp('(''sysin file = '', A)', (/file/), utmp)
                endif
                call msg_grp('(''sysout unit = '', I0)', (/JFILE/), utmp)
                if (JFILE.ge.0) then
                   inquire(NAME=file, UNIT=JFILE, IOSTAT=jerr)
                   if (jerr.eq.0) &
                        & call msg_grp('(''sysout file = '', A)', (/file/), utmp)
                endif
             endif
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_DEEP) then
          tsmd = MODE_SURFACE
          if (ierr.eq.0) call mwe_diag(ierr, u=utmp, mode=lmd)
          if (ierr.eq.0) call env_diag(ierr, u=utmp, mode=tsmd)
          if (ierr.eq.0) call fun_diag(ierr, u=utmp, mode=tsmd)
          if (ierr.eq.0) call arg_diag(ierr, u=utmp, mode=tsmd)
          ! if (ierr.eq.0) call log_diag(ierr, u=utmp, mode=tsmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag
!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std,only: control_mode, control_deep, is_first_force
    use TOUZA_Std,only: choice
    use TOUZA_Std,only: mwe_finalize, arg_finalize, log_finalize, env_finalize, fun_finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd
    integer tsmd

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
          if (ierr.eq.0) call mwe_finalize(ierr, u=utmp, mode=lmd)
          if (ierr.eq.0) call env_finalize(ierr, u=utmp, mode=tsmd)
          if (ierr.eq.0) call fun_finalize(ierr, u=utmp, mode=tsmd)
          if (ierr.eq.0) call arg_finalize(ierr, u=utmp, mode=tsmd)
          ! if (ierr.eq.0) call log_finalize(ierr, u=utmp, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize
!!!_  & msg_grp
  subroutine msg_grp_txt &
       & (txt, u, to_flush)
    use TOUZA_Std,only: ts_msg_grp=>msg_grp
    implicit none
    character(len=*),intent(in)          :: txt
    integer,         intent(in),optional :: u
    logical,         intent(in),optional :: to_flush
    call ts_msg_grp(txt, __GRP__, __MDL__, u, to_flush)
    return
  end subroutine msg_grp_txt

  subroutine msg_grp_ia &
       & (fmt, vv, u, to_flush)
    use TOUZA_Std,only: ts_msg_grp=>msg_grp
    implicit none
    character(len=*),intent(in)          :: fmt
    integer,         intent(in)          :: vv(:)
    integer,         intent(in),optional :: u
    logical,         intent(in),optional :: to_flush
    call ts_msg_grp(fmt, vv(:), __GRP__, __MDL__, u, to_flush)
    return
  end subroutine msg_grp_ia

  subroutine msg_grp_aa &
       & (fmt, vv, u, to_flush)
    use TOUZA_Std,only: ts_msg_grp=>msg_grp
    implicit none
    character(len=*),intent(in)          :: fmt
    character(len=*),intent(in)          :: vv(:)
    integer,         intent(in),optional :: u
    logical,         intent(in),optional :: to_flush
    call ts_msg_grp(fmt, vv(:), __GRP__, __MDL__, u, to_flush)
    return
  end subroutine msg_grp_aa

!!!_ + user subroutines
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
!!!_  & open_bind_sysin - open_sysin core
  subroutine open_bind_sysin &
       & (ierr, usys, file, tag, u)
    use TOUZA_Std,only: is_msglev_WARNING, is_msglev_INFO
    use TOUZA_Std,only: new_unit, uin
    use TOUZA_Std,only: get_wni
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: usys
    character(len=*),intent(in)          :: file
    character(len=*),intent(in)          :: tag
    integer,         intent(in),optional :: u        ! log unit

    integer utmp
    integer nrw
    logical bo
    character(len=256) :: txt
    character(len=lpath) :: f

    ierr = 0
    utmp = get_logu(u, ulog)
    f = file
    if (f.eq.' ') f = pfx_sysin

    usys = -1
    if (ierr.eq.0) then
       if (IFILE.ge.0) then
          if (is_msglev_WARNING(lev_verbose)) then
101          format('old sysin unit/', A, ': ', I0)
             write(txt, 101) trim(tag), IFILE
             call msg_grp(txt, utmp)
          endif
       endif
    endif
    if (ierr.eq.0) then
       if (f.eq.sp_stdin) then
          usys = uin
       else
          usys = ISET
          if (usys.lt.0) usys = new_unit()
       endif
       ierr = min(0, usys)
       if (ierr.eq.0) then
          if (ISET.ge.0.and.ISET.ne.uin) then
             inquire(UNIT=ISET, OPENED=bo, IOSTAT=ierr)
             if (ierr.eq.0.and.bo) close(ISET, IOSTAT=ierr)
          endif
       endif
    endif
    if (ierr.eq.0) then
       if (f.eq.sp_dummy) then
          if (is_msglev_WARNING(lev_verbose)) then
102          format('dummy input for sysin/', A)
             write(txt, 102) trim(tag)
             call msg_grp(txt, utmp)
          endif
          open(unit=usys, FORM='FORMATTED', STATUS='SCRATCH', &
               & ACCESS='SEQUENTIAL', ACTION='READWRITE', IOSTAT=ierr)
       else if (f.eq.sp_stdin) then
          call get_wni(ierr, nrank=nrw)
          if (nrw.gt.1.or.nrw.lt.0) then
             if (is_msglev_WARNING(lev_verbose)) then
103             format('enabled stdin on mpi/', A)
                write(txt, 103) trim(tag)
                call msg_grp(txt, utmp)
             endif
          endif
       else
          open(unit=usys, FILE=f, FORM='FORMATTED', STATUS='OLD', &
               & ACCESS='SEQUENTIAL', ACTION='READ', IOSTAT=ierr)
       endif
    endif
    if (ierr.eq.0) then
       IFILE = usys
       if (is_msglev_INFO(lev_verbose)) then
104       format('Open sysin/', A, ': ', A)
          write(txt, 104) trim(tag), trim(f)
          call msg_grp(txt, utmp)
       endif
    else
105    format('Failed to open sysin/', A, ': ', A)
       write(txt, 105) trim(tag), trim(f)
       call msg_grp(txt, utmp)
       ierr = ERR_FAILURE_INIT
    endif
  end subroutine open_bind_sysin
!!!_  & open_bind_sysout - YYSYSO compatible
  subroutine open_bind_sysout(ierr, usys, file, sfx, u)
    use TOUZA_Std,only: choice, ndigits
    use TOUZA_Std,only: get_wni, uout, is_msglev_INFO, new_unit
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: usys
    character(len=*),intent(in)          :: file
    character(len=*),intent(in),optional :: sfx
    integer,         intent(in),optional :: u        ! log unit

    character(len=lmsg)  :: txt
    character(len=lpath) :: f, pbuf, sbuf
    integer nr, ir
    integer nd
    integer utmp
    logical bo

    ierr = 0
    utmp = get_logu(u, ulog)

    f = file
    if (f.eq.' ') then
       if (sw_stdo) then
          f = sp_stdout
       else
          f = pfx_sysout
       endif
    endif

    if (f.eq.sp_stdout) then
       usys = uout
    else
       nr = nrank
       ir = irank
       if (nr.lt.0.or.ir.lt.0) then
          call get_wni(ierr, nrank=nr, irank=ir)
       endif
       if (nr.ne.0) then
          nd = max(ndigits(nr), digits_sysout)
          pbuf = f
          if (present(sfx)) then
             sbuf = sfx
          else
             sbuf = sfx_sysout
          endif
          call gen_path(f, pbuf, sbuf, max(0, ir), nd)
       else
          continue
       endif
       inquire(NUMBER=usys, OPENED=bo, FILE=f, IOSTAT=ierr)
       if (ierr.eq.0) then
          if (.not.bo) then
             usys = JSET
             if (usys.lt.0) usys = new_unit()
             if (usys.lt.0) then
                ierr = -1
             else
                ! if (JFILE.ne.uout.and.JFILE.ge.0) close(JFILE)
                open(unit=usys, FILE=f, FORM='FORMATTED', &
                     & ACCESS='SEQUENTIAL', ACTION='WRITE', IOSTAT=ierr)
             endif
          endif
       endif
    endif
    if (is_msglev_INFO(lev_verbose)) then
       if (JFILE.ge.0) then
          call msg_grp('(''old sysout unit: '', I0)', (/JFILE/), utmp)
       endif
       call msg_grp('(''new sysout unit: '', I0)', (/usys/), utmp)
    endif

104 format('result:sysout=', A)
    write(txt, 104) trim(f)
    if (is_msglev_INFO(lev_verbose)) then
       call msg_grp(txt, utmp)
    endif
    if (JFILE.ge.0.and.usys.ne.JFILE) then
       call msg_grp('sysout: switched.', utmp)
    endif

    if (ierr.eq.0) then
       JFILE = usys
    endif

    return
  end subroutine open_bind_sysout
!!!_  & open_sysin_primary - open /primary/ SYSIN file
  subroutine open_sysin_primary &
       & (ierr, uprim, file, pos, u)
    use TOUZA_Std,only: is_msglev_WARNING, is_msglev_INFO
    use TOUZA_Std,only: new_unit
    use TOUZA_Std,only: get_wni
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: uprim    ! primary unit
    character(len=*),intent(in),optional :: file
    integer,         intent(in),optional :: pos
    integer,         intent(in),optional :: u        ! log unit

    integer utmp
    character(len=lpath) :: pprim

    ierr = 0

    utmp = get_logu(u, ulog)
    uprim = -1
    pprim = ' '

    if (ierr.eq.0) call parse_arg_sysin(ierr, pprim, pos, utmp)
    if (ierr.eq.0) then
       if (pprim.eq.' ') then
          if (present(file)) then
             pprim = file
          endif
       endif
       if (pprim.eq.' '.and.sw_stdi) pprim = sp_stdin
    endif
    if (ierr.eq.0) call open_bind_sysin(ierr, uprim, pprim, 'primary', utmp)
  end subroutine open_sysin_primary
!!!_  & parse_arg_sysin
  subroutine parse_arg_sysin &
       & (ierr, file, pos, u)
    use TOUZA_Std,only: choice
    use TOUZA_Std,only: parse, get_nparam, get_param
    use TOUZA_Std,only: is_msglev_INFO, is_msglev_WARNING
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(out)         :: file
    integer,         intent(in),optional :: pos
    integer,         intent(in),optional :: u

    integer utmp
    integer jp, np

    ierr = 0
    utmp = get_logu(u, ulog)
    jp = choice(pos, pos_arg)
    file = ' '
    if (jp.lt.0) then
       if (is_msglev_INFO(lev_verbose)) then
          call msg_grp('command-line argument ignored.', utmp)
       endif
       file = ' '
    else
       if (ierr.eq.0) call parse(ierr)
       if (ierr.eq.0) then
          jp = max(1, jp)
          np = get_nparam()
          if (jp.le.np) then
             call get_param(ierr, file, jp, ' ')
          else
             if (is_msglev_WARNING(lev_verbose)) then
                call msg_grp('no command-line argument.', utmp)
             endif
          endif
       endif
    endif
  end subroutine parse_arg_sysin
!!!_  & search_sysin_colored
  subroutine search_sysin_colored &
       & (num, file, idx, pfx, sfx, digits, u)
    use TOUZA_Std,only: choice, choice_a, get_logu
    use TOUZA_Std,only: is_msglev_INFO, is_msglev_DEBUG
    implicit none
    integer,         intent(out)         :: num      ! number or error code
    character(len=*),intent(out)         :: file
    integer,         intent(in)          :: idx
    character(len=*),intent(in),optional :: pfx, sfx
    integer,         intent(in),optional :: digits
    integer,         intent(in),optional :: u
    integer j
    integer md
    logical bx
    integer jerr
    integer utmp
    character(len=lpath) :: path, pbuf, sbuf
    character(len=lmsg)  :: txt

    jerr = 0
    utmp = get_logu(u, ulog)

    num = 0
    file = ' '
    md = choice(max_digits, digits)

    ! Default prefix with blank pfx
    call choice_a(pbuf, ' ', pfx)
    if (pbuf.eq.' ') pbuf = pfx_sysin

    ! Empty suffix with blank sfx
    if (present(sfx)) then
       sbuf = sfx
    else
       sbuf = sfx_sysin
    endif

    do j = 1, max(1, md)
       call gen_path(path, pbuf, sbuf, (max(0, idx)), j)
       if (is_msglev_DEBUG(lev_verbose)) then
          call msg_grp('(''search '', A)', (/path/), utmp)
       endif
       if (jerr.eq.0) inquire(FILE=path, EXIST=bx, IOSTAT=jerr)
       if (jerr.eq.0.and.bx) then
          num = num + 1
          if (file.eq.' ') then
             file = path
          else
101          format('multiple candidates for sysin: ', A)
             if (num.eq.2) then
                write(txt, 101) trim(file)
                call msg_grp(txt, utmp)
             endif
             write(txt, 101) trim(path)
             call msg_grp(txt, utmp)
          endif
       endif
    enddo
    if (jerr.ne.0) num = -1
  end subroutine search_sysin_colored
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
!!!_  & rewind_lock
  subroutine rewind_lock(tag, u)
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
       call msg_grp(txt, utmp)
    endif
  end subroutine rewind_lock
!!!_  & rewind_unlock
  subroutine rewind_unlock(tag, u)
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
       call msg_grp(txt, utmp)
    else if (lock_tag.ne.' ') then
       write(txt, 101) trim(lock_tag)
       call msg_grp(txt, utmp)
    endif
    lock_tag = ' '
  end subroutine rewind_unlock
!!!_  & is_locked_rewind
  logical function is_locked_rewind() result(b)
    implicit none
    b = lock_rewind
  end function is_locked_rewind
!!!_  & show_lock_status
  subroutine show_lock_status(u)
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
    call msg_grp(txt, utmp)
  end subroutine show_lock_status
!!!_ + Deprecated
!!!_  & legacy_open_sysin - YYSYSI compatible
  subroutine legacy_open_sysin(ierr, u)
    use TOUZA_Std,only: choice
    use TOUZA_Std,only: parse, get_nparam, get_param
    use TOUZA_Std,only: get_wni, uin, is_msglev_INFO, new_unit
    implicit none
    integer,         intent(out)          :: ierr
    integer,         intent(out),optional :: u
    character(len=lpath) :: file, pfx
    integer jp, np, na
    integer nrw
    integer jz
    logical bo
    integer utmp

    ierr = 0
    file = ' '
    na = 0

    if (pos_arg.lt.0) then
       call msg_grp('command-line argument ignored.', ulog)
    else
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
                  &       (/icolor, ncolor, na/), ulog)
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
    endif
    if (ierr.eq.0) then
       if (file.eq.' '.and.sw_stdi) file = '-'
       if (file.eq.'-') then
          ! stdin; warn if non-single ranks
          call get_wni(ierr, nrank=nrw)
          if (nrw.gt.1.or.nrw.lt.0) call msg_grp('stdin enabled.', ulog)
       else if (file.eq.' ') then
          call legacy_search_sysin(ierr, file, pfx_sysin, sfx_sysin, icolor, digits_sysin)
          if (file.eq.' ') ierr = -1
       else
          jz = verify(file, '0', .TRUE.)
          if (jz.eq.0) then
             pfx = file
             call legacy_search_sysin(ierr, file, pfx, sfx_sysin, icolor, digits_sysin)
          else
             if (icolor.lt.0) then
                pfx = file
                call legacy_search_sysin(ierr, file, pfx, ' ', icolor)
             else
                pfx = file(1:jz)
                call legacy_search_sysin(ierr, file, pfx, ' ', icolor, digits_sysin)
             endif
          endif
       endif
    endif
    if (file.eq.' ') ierr = -1
    if (ierr.eq.0) then
       if (IFILE.ge.0) then
          if (is_msglev_INFO(lev_verbose)) then
             call msg_grp('(''old sysin unit: '', I0)', (/IFILE/), ulog)
          endif
       endif
       if (file.eq.'-') then
          IFILE = uin
       else
          inquire(NUMBER=utmp, OPENED=bo, FILE=file, IOSTAT=ierr)
          write(*, *) 'opened:', utmp, bo, ' ', trim(FILE)
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
          call msg_grp('(''new sysin unit: '', I0)', (/IFILE/), ulog)
       endif
    endif
    if (present(u)) then
       u = IFILE
    endif
    return
  end subroutine legacy_open_sysin
!!!_  & legacy_search_sysin
  subroutine legacy_search_sysin &
       & (ierr, file, pfx, sfx, num, digits)
    use TOUZA_Std,only: is_msglev_INFO, is_msglev_DEBUG
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
             call msg_grp('(''search '', A)', (/path/))
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
                   call msg_grp(txt)
                endif
                write(txt, 101) trim(path)
                call msg_grp(txt)
             endif
          endif
       enddo
    endif
    if (ierr.eq.0) inquire(FILE=pfx, EXIST=bx, IOSTAT=ierr)
    if (ierr.eq.0) then
       if (is_msglev_DEBUG(lev_verbose)) then
          call msg_grp('(''search '', A)', (/pfx/))
       endif
102    format('ignore sysin candidate (rank=', I0, '): ', A)
       if (bx) then
          if (num.lt.0) then
             if (file.ne.' ') then
                write(txt, 102) num, trim(file)
                call msg_grp(txt)
             endif
             file = pfx
          else if (nf.eq.0) then
             file = pfx
          else if (nf.eq.1) then
             write(txt, 102) num, trim(pfx)
             call msg_grp(txt)
          else
             call msg_grp('too much sysin candidates')
             ierr = -1
          endif
       else if (nf.gt.1) then
          call msg_grp('too much sysin candidates')
          ierr = -1
       endif
    endif
    if (file.eq.' ') then
103    format('not found sysin: ', A, 1x, A, 1x, I0)
       write(txt, 103) trim(pfx), trim(sfx), num
       call msg_grp(txt)
       ierr = -1
    else if (is_msglev_INFO(lev_verbose)) then
104    format('result:sysin=', A)
       write(txt, 104) trim(file)
       call msg_grp(txt)
    endif
  end subroutine legacy_search_sysin

!!!_  & legacy_open_sysout
  subroutine legacy_open_sysout(ierr)
    implicit none
    integer,intent(out) :: ierr
    integer udummy
    call open_bind_sysout(ierr, udummy, ' ')
  end subroutine legacy_open_sysout
!!!_ + end TOUZA_Emu_usi
end module TOUZA_Emu_usi
!!!_* non-module Procedures
!!!_ + OPNNML - open SYSIN, SYSOUT
subroutine OPNNML(IOS)
  use TOUZA_Emu_usi,only: legacy_open_sysin, legacy_open_sysout
  implicit none
  integer,intent(out) :: IOS   !! io status
  integer jerri, jerro
  call legacy_open_sysin(jerri)
  call legacy_open_sysout(jerro)
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

!!!_ + SETBCM - set base communicator
subroutine SETBCM(MYCOMM)
  use TOUZA_Emu_usi,only: usi_init=>init
  implicit none
  integer,intent(in) :: MYCOMM
  integer jerr
  call usi_init(jerr, icomm=MYCOMM)
  return
end subroutine SETBCM

!!!_ + SETNML - set SYSIN, SYSOUT logical unit num.
subroutine SETNML(IFILE, JFILE)
  use TOUZA_Emu_usi,only: emu_iset=>iset, emu_jset=>jset
  implicit none
  integer,intent(in) :: IFILE, JFILE
  emu_iset = IFILE
  emu_jset = JFILE
  return
end subroutine SETNML

!!!_ + SETCLR - set color index for SYSIN filename
subroutine SETCLR(MYCOLR)
  use TOUZA_Emu_usi,only: update_color
  implicit none
  integer,intent(in) :: MYCOLR
  call update_color(icol=MYCOLR)
  return
end subroutine SETCLR

!!!_ + SETRNK - set rank index for SYSOUT filename
subroutine SETRNK(MYRANK)
  use TOUZA_Emu_usi,only: update_ranks
  implicit none
  integer,intent(in) :: MYRANK
  call update_ranks(ir=MYRANK)
  return
end subroutine SETRNK

!!!_ + SETSIZ - set number of ranks for SYSOUT filename
subroutine SETSIZ(MYSIZE)
  use TOUZA_Emu_usi,only: update_ranks
  implicit none
  integer,intent(in) :: MYSIZE
  call update_ranks(nr=MYSIZE)
  return
end subroutine SETSIZ

!!!_@ test_emu_usi - test program
#if TEST_EMU_USI
program test_emu_usi
  use TOUZA_std,only: get_wni
  use TOUZA_Emu_usi,only: init, diag, finalize
  use TOUZA_Emu_usi,only: SETNML, OPNNML, REWNML
  use TOUZA_Emu_usi,only: SETRNK, SETSIZ, SETCLR
  use TOUZA_Emu_usi,only: open_bind_sysin, open_bind_sysout, update_color
  implicit none
  integer ierr
  integer ir
#if TEST_EMU_USI == 1
  integer nr, icolor
#endif
  integer ifpar, jfpar

  ierr = 0
101 format(A, ' = ', I0, 1x, I0)

  call init(ierr, u=-1, levv=+9, stdv=+9, pos=0)
#if TEST_EMU_USI == 1
  if (ierr.eq.0) call SETNML(98, 99)
  if (ierr.eq.0) call get_wni(ierr, irank=ir, nrank=nr)
  if (ierr.eq.0) then
     call SETSIZ(nr)
     if (ir.ge.1) call SETRNK(ir)
  endif
  if (ierr.eq.0) call OPNNML(ierr)
  if (ierr.eq.0) call REWNML(ifpar, jfpar)
  if (ierr.eq.0) then
     write(*, *) 'Primary: ', ifpar, jfpar
     write(jfpar, *) 'Primary: ', ifpar, jfpar
  else
     write(*, *) 'Primary: error= ', ierr
  endif

  if (ierr.eq.0) then
     icolor = ir
     call SETCLR(icolor)
  endif

  if (ierr.eq.0) call OPNNML(ierr)
  if (ierr.eq.0) call REWNML(ifpar, jfpar)
  if (ierr.eq.0) then
     write(*, *) 'Colored: ', ifpar, jfpar
     write(jfpar, *) 'Colored: ', ifpar, jfpar
  else
     write(*, *) 'Colored: error= ', ierr
  endif
#else /* not TEST_EMU_USI == 1 */
  if (ierr.eq.0) call open_bind_sysin(ierr, ifpar, ' ', 'primary')
  write(*, 101) 'open_bind_sysin', ierr
  write(*, *) 'Primary: ', ifpar

  if (ierr.eq.0) call get_wni(ierr, irank=ir)
  if (ierr.eq.0) call update_color(ir)
  if (ierr.eq.0) call open_bind_sysin(ierr, ifpar, ' ', 'secondary')
  write(*, 101) 'open_bind_sysin', ierr
  write(*, *) 'Colored: ', ifpar
  if (ierr.eq.0) call open_bind_sysout(ierr, jfpar, ' ')
  write(*, 101) 'open_bind_sysout', ierr
  write(*, *) 'sysout: ', jfpar
#endif /* not TEST_EMU_USI == 1 */
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

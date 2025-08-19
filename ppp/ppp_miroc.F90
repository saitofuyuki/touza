!!!_! ppp_miroc.F90 - TOUZA/Ppp MIROC compatible interfaces
! Maintainer: SAITO Fuyuki
! Created: Feb 2 2022
#define TIME_STAMP 'Time-stamp: <2025/08/19 18:05:29 fuyuki ppp_miroc.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022-2025
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
!!!_* Includes
#ifndef   WITH_MIROC
#  define WITH_MIROC 0
#endif
#if WITH_MIROC
#  include "miroc.h"
#  include "ztouza.h"
#endif
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_ppp.h"
!!!_* Macros
#ifndef    MIROC_INTEGER
#  define  MIROC_INTEGER 4
#endif
#ifndef    MIROC_DOUBLE
#  define  MIROC_DOUBLE 8
#endif

#ifndef    MIROC_BOOTSTRAP
#  define  MIROC_BOOTSTRAP ' '    /* primary sysin file-name (default if blank) */
#endif

#ifndef    TEST_PPP_MIROC
#  define  TEST_PPP_MIROC 0
#endif
!!!_@ TOUZA_Ppp_miroc - Ppp miroc compatible interfaces
module TOUZA_Ppp_miroc
!!!_ = declaration
  use TOUZA_Ppp_std,only: unit_global
  use TOUZA_Ppp,only: diag_maps_batch, barrier_trace
  use TOUZA_Ppp,only: set_king, get_king, is_king
!!!_  - default
  implicit none
  private
  integer,parameter,public :: KMD = MIROC_DOUBLE
!!!_  - miroc include original
#if WITH_MIROC
#else  /* not WITH_MIROC */
#endif /* not WITH_MIROC */
#define __MDL__ 'm'
!!!_  - parmater
  integer,parameter,public :: lmod = 9

  integer,parameter,public :: flag_allow_both = 2**0    ! allow both sysin.CLxxx and sysin existence
  integer,parameter,public :: flag_allow_base = 2**1    ! allow sysin at colored directory mode

  integer,parameter,public :: flag_default = flag_allow_base

  integer,parameter,public :: lverify=128
!!!   - public static
  integer,save,public :: nproc_quit = 0
  integer,save,public :: icomm_quit = 0
!!!_  - interface aliases
  interface diag_agent_maps
     module procedure diag_maps_batch
  end interface diag_agent_maps
  interface terminate
     module procedure terminate_core
     module procedure terminate_aa, terminate_ai
  end interface terminate
!!!_  - interfaces (external)
  interface
     subroutine XCKINI(AFFILS, N, GREETING, ICROOT)
       implicit none
       character(len=*),intent(in) :: AFFILS(*) ! array of agents I belong to.
       integer,         intent(in) :: N
       external                    :: GREETING
       integer,         intent(in) :: ICROOT
     end subroutine XCKINI
     subroutine XCKINI_legacy(HDRVR, GREETING, ICROOT)
       implicit none
       character(len=*),intent(in) :: HDRVR         ! sequence of <CI> I belong to.
       external                    :: greeting
       integer,         intent(in) :: ICROOT
     end subroutine XCKINI_legacy
     subroutine XCKINI_ils(AFFILS, N, ICROOT)
       implicit none
       character(len=*),intent(in)  :: AFFILS(*) ! array of agents I belong to.
       integer,         intent(in)  :: N
       integer,         intent(in)  :: ICROOT
     end subroutine XCKINI_ils
     subroutine MMGetColor(ICLR, NCLR)
       implicit none
       integer,intent(out) :: ICLR, NCLR     ! color
     end subroutine MMGetColor
     subroutine XMGetColor(ICLR, NCLR)
       implicit none
       integer,intent(out) :: ICLR, NCLR     ! color
     end subroutine XMGetColor
     subroutine XMComm(OCMZ, HCTZ)
       implicit none
       logical,         intent(out) :: OCMZ     ! whether or not I belong to comm.
       character(len=*),intent(in)  :: HCTZ     ! <CI>
     end subroutine XMComm
     subroutine XMIComm(ICMZ, HCTZ)
       implicit none
       integer,         intent(out) :: ICMZ     ! communicator handle
       character(len=*),intent(in)  :: HCTZ     ! <CI>
     end subroutine XMIComm
     subroutine XMProc(NPRZ, IRKZ, HCTZ)
       implicit none
       integer,         intent(out) :: NPRZ     ! # of rank
       integer,         intent(out) :: IRKZ     ! PE number
       character(len=*),intent(in)  :: HCTZ     ! <CI>
     end subroutine XMProc
     subroutine XMGETK(HM, HC, IR, HR)
       implicit none
       integer,         intent(out) :: IR       ! King rank for the input module
       character(len=*),intent(in)  :: HM       ! Module indicator
       character(len=*),intent(in)  :: HC       ! <CI>
       character(len=*),intent(in)  :: HR       ! message text
     end subroutine XMGETK
     subroutine XMOKNG(HM, HC, OR, HR)
       implicit none
       logical,         intent(out) :: OR
       character(len=*),intent(in)  :: HM
       character(len=*),intent(in)  :: HC
       character(len=*),intent(in)  :: HR
     end subroutine XMOKNG
     subroutine XMquit(NPR0I, ICOM0I)
       implicit none
       integer,intent(in) :: NPR0I, ICOM0I
     end subroutine XMquit
     subroutine XMabort(LEV)
       implicit none
       integer,intent(in) :: LEV
     end subroutine XMabort
     subroutine XMabort0()
       implicit none
     end subroutine XMabort0
     subroutine XMFinal(OBARR)
       implicit none
       logical,intent(in) :: OBARR  ! final barrier switch
     end subroutine XMFinal
  end interface
!!!_  - private
  integer,save,private :: init_mode = 0
  integer,save,private :: init_counts = 0
  integer,save,private :: diag_counts = 0
  integer,save,private :: fine_counts = 0
  integer,save,private :: lev_verbose = PPP_MSG_LEVEL
  integer,save,private :: err_default = ERR_NO_INIT
  integer,save,private :: ulog = unit_global

  integer,save :: icolor_world = -1
  integer,save :: ncolor_world = -1

  character(len=*),parameter :: primary_sysin = MIROC_BOOTSTRAP
!!!_  - public
  public init, diag, finalize
  public init_rainbow, init_sysio, switch_dir
  public init_king
  public affils_legacy
  public get_wcolor
  public gen_agent_union
  public push_agent, pop_agent, top_agent, switch_agent, spinoff_agent
  public diag_agent_maps
  public query_handle, query_nprocs, query_comm
  public terminate
!!!_   . export
  public barrier_trace
  public set_king, get_king, is_king
!!!_   . legacy
  public XCKINI, MMGetColor, XMGetColor, XMIComm, XMCOMM, XMGETK, XMProc, XMOKNG
  public XMquit, XMabort,    XMabort0,   XMFinal
  public XCKINI_ils
  public XCKINI_legacy
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init &
       & (ierr, u, levv, mode, stdv, icomm)
    use TOUZA_Ppp,only: ppp_init=>init
    use TOUZA_Ppp_std,only: control_mode, control_deep, is_first_force
    use TOUZA_Ppp_std,only: choice, is_msglev_NORMAL
    use TOUZA_Ppp_std,only: mwe_init, bld_init
    use TOUZA_Emu,only: usi_init
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
       if (md.ge.MODE_DEEP) then
          ! chmd = MODE_SURFACE
          ! if (ierr.eq.0) call mwe_init(ierr, u, stdv, mode=lmd, icomm=icomm)
          if (ierr.eq.0) call usi_init(ierr, u, levv, mode=lmd, stdv=stdv, icomm=icomm)
       endif
       if (md.ge.MODE_SHALLOW) then
          chmd = MODE_SURFACE
          if (ierr.eq.0) call bld_init(ierr, u, levv, mode=chmd)
          if (ierr.eq.0) call ppp_init(ierr, u, levv, mode=lmd, stdv=stdv, icomm=icomm)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Ppp,only: ppp_diag=>diag
    use TOUZA_Ppp_std,only: ppp_msg=>msg
    use TOUZA_Ppp_std,only: control_mode, control_deep, is_first_force
    use TOUZA_Ppp_std,only: mwe_diag, get_logu, choice, is_msglev_NORMAL
    use TOUZA_Ppp_std,only: bld_diag
    use TOUZA_Emu,only: usi_diag
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
       if (is_first_force(diag_counts, mode)) then
          if (is_msglev_normal(lv)) then
             if (ierr.eq.0) call ppp_msg(TIME_STAMP, __MDL__, u)
             if (ierr.eq.0) call ppp_msg('(''primary sysin : '', A)', &
                  & (/primary_sysin/), __MDL__, u)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          chmd = MODE_SURFACE
          if (ierr.eq.0) call ppp_diag(ierr, u, levv, lmd)
          if (ierr.eq.0) call bld_diag(ierr, u, levv, chmd)
       endif
       if (md.ge.MODE_DEEP) then
          ! if (ierr.eq.0) call mwe_diag(ierr, u, levv, lmd)
          if (ierr.eq.0) call usi_diag(ierr, u, levv, lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Ppp_std,only: mwe_finalize, bld_finalize
    use TOUZA_Ppp,only: ppp_finalize=>finalize
    use TOUZA_Ppp_std,only: control_mode, control_deep, is_first_force
    use TOUZA_Ppp_std,only: get_logu, choice, trace_fine
    use TOUZA_Emu,only: usi_finalize
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
               &  grp=__GRP__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ppp_finalize(ierr, u, levv, lmd)
       endif
       if (md.ge.MODE_DEEP) then
          chmd = MODE_SURFACE
          if (ierr.eq.0) call usi_finalize(ierr, u, levv, mode=lmd)
          ! if (ierr.eq.0) call mwe_finalize(ierr, u, levv, mode=lmd)
          if (ierr.eq.0) call bld_finalize(ierr, u, levv, mode=chmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + Core procedures for MIROC
!!!_  & init_rainbow - XCKINI compatible procedure
  subroutine init_rainbow &
       & (ierr, cdir, cagent, cid, affils, icomm)
    use TOUZA_Emu,only: open_sysin_primary
    use TOUZA_Emu,only: update_ranks, update_color
    use TOUZA_Ppp_std,only: get_comm, trace_err
    use TOUZA_Ppp,only: new_agent_root, new_agent_color, new_agent_family
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: cdir       ! directory for each color
    character(len=*),intent(out) :: cagent     ! agent for each color
    character(len=*),intent(out) :: cid        ! agent id to verify
    character(len=*),intent(in)  :: affils(:)
    integer,optional,intent(in)  :: icomm

    integer nrank, irank
    integer ifpar
    integer ncolor, icolor

    integer ic

    ierr = 0

    if (ierr.eq.0) call init_world(ierr, nrank, irank)
    if (ierr.eq.0) call update_ranks(nr=nrank, ir=irank)

    ! master sysin
    if (ierr.eq.0) call open_sysin_primary(ierr, ifpar, primary_sysin)

    if (ierr.eq.0) then
       call config_rainbow &
            & (ierr,  ncolor, icolor, cdir, cagent, cid, &
            &  nrank, irank,  ifpar)
    endif
    if (ierr.eq.0) call update_color(icol=icolor, ncol=ncolor)
    ! store cache
    if (ierr.eq.0) then
       icolor_world = icolor
       ncolor_world = ncolor
    endif

    if (ierr.eq.0) then
       if (present(icomm)) then
          ic = icomm
       else
          call get_comm(ierr, ic)
       endif
    endif
    if (ierr.eq.0) call new_agent_root(ierr, ic)
    if (ierr.eq.0) call new_agent_color(ierr, icolor, cagent)
    if (ierr.eq.0) call new_agent_family(ierr, affils(:))

    call trace_err(ierr, fun='init_rainbow')
    return
  end subroutine init_rainbow

!!!_  & init_sysio - XCKINI compatible procedure
  subroutine init_sysio &
       & (ierr, cdir, cagent, cid, config, greeting, flag)
    use TOUZA_Ppp_std,only: lpath, choice
    use TOUZA_Ppp_std,only: set_defu
    use TOUZA_Emu,only: open_bind_sysin, search_sysin_colored
    use TOUZA_Emu,only: get_sysu, open_bind_sysout
    use TOUZA_Emu,only: update_ranks
    use TOUZA_PPP,only: inquire_agent
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: cdir
    character(len=*),intent(in)          :: cagent
    character(len=*),intent(in)          :: cid
    character(len=*),intent(in),optional :: config
    integer,         intent(in),optional :: flag
    optional :: greeting
    interface
       subroutine greeting(jfpar)
         implicit none
         integer,intent(in) :: jfpar
       end subroutine greeting
    end interface

    integer nrank, irank
    integer ifpar, jfpar
    integer ncolor, icolor
    character(len=lpath) :: csysin
    integer ncs
    integer f
    logical bworld

    ierr = 0

    bworld = (cdir.eq.' ')

    icolor = icolor_world
    ncolor = ncolor_world

    f = choice(flag_default, flag)
    ncs = 0

    call search_sysin(ierr, csysin, cdir, cagent, config, flag)
    if (cdir.ne.' ') then
       if (ierr.eq.0) call switch_dir(ierr, cdir)
    endif
    if (ierr.eq.0) then
       if (ncolor.gt.0) then
          call open_bind_sysin(ierr, ifpar, csysin, 'colored')
       else
          call get_sysu(sysi=ifpar)
       endif
    endif

    if (ierr.eq.0) call inquire_agent(ierr, source=+1, irank=irank, nrank=nrank)

    if (ierr.eq.0) then
       if (.not.bworld) then
          if (ierr.eq.0) call update_ranks(nr=nrank, ir=irank)
       endif
    endif
    if (ierr.eq.0) call open_bind_sysout(ierr, jfpar, ' ')

    if (ierr.eq.0) then
       call set_defu(jfpar)
       if (present(greeting)) then
          call greeting(jfpar)
       endif
    endif
    if (ierr.eq.0) then
       call verify_sysin(ierr, cid, ifpar, jfpar)
    endif

    if (ierr.eq.0) then
       call diag_rainbow &
            & (ierr,   &
            &  ncolor, icolor, cdir,   ' ', &
            &  nrank,  irank,  jfpar)
    endif

    return
  end subroutine init_sysio

!!!_  & search_sysin
  subroutine search_sysin &
       & (ierr, csysin, cdir, cagent, config, flag)
    use TOUZA_Ppp_std,only: lpath, choice, choice_a
    use TOUZA_Ppp_std,only: set_defu
    use TOUZA_Emu,only: open_bind_sysin, search_sysin_colored
    use TOUZA_Emu,only: get_sysu, open_bind_sysout
    use TOUZA_Emu,only: update_ranks
    use TOUZA_PPP,only: inquire_agent
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(out)         :: csysin
    character(len=*),intent(in)          :: cdir
    character(len=*),intent(in)          :: cagent
    character(len=*),intent(in),optional :: config
    integer,         intent(in),optional :: flag

    integer ncolor, icolor
    integer ncs
    integer f
    logical bworld

    ierr = 0

    bworld = (cdir.eq.' ')

    icolor = icolor_world
    ncolor = ncolor_world

    f = choice(flag_default, flag)
    ncs = 0

    if (ncolor.gt.0) then
       ! [CAUTION]
       !    Secondary sysin (configuration) is opened *AFTER* chdir.
       !    You may use the absolute path to avoid a complex relative path.
       !    See realpath(1), to compute a relative path in shell scripts.
       if (ierr.eq.0) then
          call search_sysin_colored(ncs, csysin, icolor, pfx=config)
       endif
       if (cdir.eq.' ') then
          if (ncs.le.0) then
             if (IAND(f, flag_allow_base).eq.0) then
                call terminate(1, 'Legacy colored-SYSIN not found.')
                return
             endif
102          format('WARNING: enable non-colored-sysin with subdir run')
             if (ulog.lt.0) then
                write(*, 102)
             else
                write(ulog, 102)
             endif
             call choice_a(csysin, ' ', config)
          else if (ncs.gt.1) then
             call terminate(1, 'Panic in Legacy colored-SYSIN search.')
             return
          endif
       else
          ! For safety, SYSIN.CLxxx should not exist when enabled subdirectory run.
          if (ncs.gt.0) then
             if (IAND(f, flag_allow_both).eq.0) then
                call terminate(1, 'Legacy colored-SYSIN exists with subdir run.')
                return
             endif
101          format('WARNING: found colored-sysin with subdir run: ', A)
             if (ulog.lt.0) then
                write(*, 101) trim(csysin)
             else
                write(ulog, 101) trim(csysin)
             endif
          endif
          call choice_a(csysin, ' ', config)
       endif
    else
       call choice_a(csysin, ' ', config)
    endif

    return
  end subroutine search_sysin

!!!_  & switch_dir - chdir wrapper
  subroutine switch_dir &
       & (ierr, dir, u)
    use TOUZA_Ppp_std,only: ipc_GETCWD, ipc_CHDIR, lpath
    use TOUZA_Ppp_std,only: get_logu, choice, is_unit_star
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: dir
    integer,optional,intent(in)  :: u

    character(len=lpath) :: cwd0, cwd1
    integer utmp

    ierr = 0
    utmp = get_logu(u, ulog)

    if (dir.ne.' ') then
       if (ierr.eq.0) call ipc_GETCWD(cwd0, ierr)
       if (ierr.eq.0) call ipc_CHDIR(trim(dir), ierr)
       if (ierr.eq.0) call ipc_GETCWD(cwd1, ierr)
101    format('chdir:', A, ' > ', A)
109    format('chdir failed:', A)
       if (ierr.eq.0) then
          if (is_unit_star(utmp)) then
             write(*, 101) trim(cwd0), trim(cwd1)
          else if (utmp.ge.0) then
             write(utmp, 101) trim(cwd0), trim(cwd1)
          endif
       else
          if (utmp.lt.0) then
             write(*, 109) trim(dir)
          else
             write(utmp, 109) trim(dir)
          endif
       endif
    endif
  end subroutine switch_dir

!!!_  & init_world
  subroutine init_world &
       & (ierr, nrank, irank)
    use TOUZA_Ppp_std,only: get_ni, get_wni, safe_mpi_init
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: nrank, irank
    integer icomm

    ierr = 0
    call safe_mpi_init(ierr)
    call get_wni(ierr, nrank, irank, icomm)

    nproc_quit = nrank
    icomm_quit = icomm
  end subroutine init_world

!!!_  & config_rainbow - rainbow sectioning with namelists
  subroutine config_rainbow &
       & (ierr,   &
       &  ncolor, icolor, cdir,   agent,  cid, &
       &  nrank,  irank,  ifpar,  jfpar)
    use TOUZA_Ppp_std,only: lpath
    use TOUZA_Ppp_std,only: is_eof_ss
    use TOUZA_Ppp_std,only: get_logu, choice, is_unit_star
    use TOUZA_Ppp_std,only: trace_err
    use TOUZA_PPP,only: lagent
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ncolor,icolor
    character(len=*),intent(out) :: cdir
    character(len=*),intent(out) :: agent
    character(len=*),intent(out) :: cid
    integer,         intent(in)  :: nrank, irank
    integer,         intent(in)  :: ifpar
    integer,optional,intent(in)  :: jfpar

    integer utmp
    integer  ISPLR, MSPLR
    character(len=lpath)  :: DIR
    character(len=lagent) :: NAME
    character(len=lverify) :: ID
    namelist /NMWSPL/ ISPLR, MSPLR, DIR, NAME, ID

    ! To split for each 10 ranks, set number of ranks as:
    !    &nmwspl msplr=10, /

    ! or (traditional) explicitly define the head rank for each color:
    !    &nmwspl isplr=0, /
    !    &nmwspl isplr=10, /
    !    &nmwspl isplr=20, /
    !    :

    ! Moreover, msplr can be set twice or more, to define different
    ! ranks for each color:
    !    &nmwspl msplr=5, /      ! color 0   0:5
    !    &nmwspl msplr=10, /     !       1   5:15
    !    &nmwspl msplr=7, /      !       2   15:22

    ! Above is the mostly same as traditional setting:
    !    &nmwspl isplr=0, /
    !    &nmwspl isplr=5, /
    !    &nmwspl isplr=15, /
    !    &nmwspl isplr=22, /

    ! There is a slightly inconsistent behavior between above two:
    ! The former repeats the final specification msplr=7 after color 3,
    ! while the latter bundles all the remaing ranks to the single color 3.

    integer nmem
    integer mreq, jprv, jnxt, mleft
    integer jerr
    character(len=lpath) :: defd
#if HAVE_FORTRAN_OPEN_IOMSG
    character(len=128) :: tmsg
#endif
    ierr = 0

    utmp = get_logu(jfpar, ulog)

    icolor = 0
    ncolor = 0
    cdir = ' '
    agent = ' '
    cid = ' '

    nmem = -1
    defd = ' '
    mreq = 0
    jprv = 0
    rewind(unit=ifpar, IOSTAT=ierr)
    do
       if (ierr.ne.0) exit
       if (ierr.eq.0) then
          isplr = -1
          msplr = -1
          DIR = ' '
          NAME = ' '
          ID = ' '
#if HAVE_FORTRAN_OPEN_IOMSG
          read (ifpar, nmwspl, IOSTAT=jerr, IOMSG=tmsg)
#else
          read (ifpar, nmwspl, IOSTAT=jerr)
#endif
          if (jerr.eq.0) then
             if (is_unit_star(utmp)) then
                write(*, nmwspl)
             else if (utmp.ge.0) then
                write(utmp, nmwspl)
             endif
          else
             if (.not.is_eof_ss(jerr)) then
                ierr = jerr
#if HAVE_FORTRAN_OPEN_IOMSG
                call trace_err(jerr, fun='config_rainbow', asfx=trim(tmsg))
#endif
             endif
             exit
          endif
       endif
       if (isplr.ge.0.and.msplr.ge.0) then
109       format('xmcomm: PANIC.  Both ISPLR and MSPLR are set.')
          if (is_unit_star(utmp)) then
             write(*, 109)
          else if (utmp.ge.0) then
             write(utmp, 109)
          endif
          ierr = -1
          exit
       endif
       if (isplr.eq.0) cycle
       if (isplr.gt.0) msplr = isplr - jprv
       if (msplr.gt.0) nmem = msplr
       if (nmem.le.0) nmem = nrank - jprv   ! full ranks if even non-positive

       jnxt = jprv + nmem
       if (irank.ge.jnxt) then
          icolor = icolor + 1
       else if (irank.ge.jprv) then
          cdir = DIR
          agent = NAME
          cid = ID
       endif
       if (nrank.ge.jnxt) ncolor = ncolor + 1
       if (isplr.ge.0) then
          if (jprv.lt.nrank) mreq = jnxt
          nmem = 0  ! for compatibility
       else
          if (msplr.ge.0.or.jprv.lt.nrank) mreq = jnxt
       endif
       jprv = jnxt
    enddo
    if (ierr.eq.0) then
       if (mreq.gt.nrank) then
          if (is_unit_star(utmp)) then
             write(*, 108) mreq, nrank
          else if (utmp.ge.0) then
             write(utmp, 108) mreq, nrank
          endif
          ierr = -1
       endif
    endif
    if (ierr.eq.0) then
       mleft = max(mreq, nrank) - jprv
       if (mleft.gt.0) then
          if (nmem.le.0) nmem = mleft
          if (mod(mleft, nmem).ne.0) then
             jnxt = (mleft / nmem + 1) * nmem + jprv
108          format('xmcomm: PANIC.  Insufficient ranks ', I0, '>', I0)
             if (is_unit_star(utmp)) then
                write(*, 108) jnxt, nrank
             else if (utmp.ge.0) then
                write(utmp, 108) jnxt, nrank
             endif
             ierr = -1
          else if (ncolor.eq.0) then
             continue
          else
             if (ncolor.eq.1) then
107             format('xmcomm: warning. monochrome-rainbow mode.')
                if (is_unit_star(utmp)) then
                   write(*, 107)
                else if (utmp.ge.0) then
                   write(utmp, 107)
                endif
             endif
             ncolor = ncolor + mleft / nmem
             if (irank.ge.jprv) icolor = icolor + (irank - jprv) / nmem
          endif
       endif
    endif
    if (ierr.eq.0) then
       call diag_rainbow &
            & (ierr,   &
            &  ncolor, icolor, cdir, agent, &
            &  nrank,  irank,  utmp)
    endif
    call trace_err(ierr, fun='config_rainbow')
    return
  end subroutine config_rainbow

!!!_  & diag_rainbow - report of rainbow sectioning
  subroutine diag_rainbow &
       & (ierr,   &
       &  ncolor, icolor, cdir,   agent, &
       &  nrank,  irank,  u)
    use TOUZA_Ppp_std,only: get_logu, is_unit_star
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: ncolor,icolor
    character(len=*),intent(in)  :: cdir
    character(len=*),intent(in)  :: agent
    integer,         intent(in)  :: nrank, irank
    integer,optional,intent(in)  :: u

    integer utmp
    character(len=128) :: tag

    ierr = 0
    utmp = get_logu(u, ulog)

101 format(A, 1x, I0, 1x, I0)
102 format(A, 1x, I0, 1x, I0, 1x, '[', A, ']')
103 format('xmcomm: color<', A, ':', I0, '/', I0, '>')
104 format('xmcomm: color(', I0, '/', I0, ')')
    if (ierr.eq.0) then
       if (agent.ne.' ') then
          write(tag, 103) trim(agent), icolor, ncolor
       else
          write(tag, 104) icolor, ncolor
       endif
       if (cdir.ne.' ') then
          if (is_unit_star(utmp)) then
             write(*, 102) trim(tag), irank, nrank, trim(cdir)
          else if (utmp.ge.0) then
             write(utmp, 102) trim(tag), irank, nrank, trim(cdir)
          endif
       else
          if (is_unit_star(utmp)) then
             write(*, 101) trim(tag), irank, nrank
          else if (utmp.ge.0) then
             write(utmp, 101) trim(tag), irank, nrank
          endif
       endif
    endif

  end subroutine diag_rainbow

!!!_  & init_king - king configuration with namelists
  subroutine init_king &
       & (ierr,   &
       &  ifpar,  jfpar)
    use TOUZA_Ppp,only: set_king
    use TOUZA_Ppp_std,only: choice, is_eof_ss
    use TOUZA_Emu,only: get_sysu
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in),optional :: ifpar, jfpar

    character(len=lmod) :: HMOD, HREF
    integer krank
    integer uin, uout
    NAMELIST /NMKING/ HMOD, KRANK, HREF
    ierr = 0

    uin =  choice(-1, ifpar)
    uout = choice(-1, jfpar)
    if (uin.lt.0) call get_sysu(sysi=uin)
    if (uout.lt.0) call get_sysu(syso=uout)

    ! call set_king(ierr, 0, 'A', 'A')
    ! call set_king(ierr, 0, 'O', 'O')

    rewind(unit=uin, IOSTAT=ierr)
    do
       if (ierr.ne.0) exit
       if (ierr.eq.0) then
          HMOD = ' '
          KRANK = -1
          HREF = ' '
          read (uin, nmking, IOSTAT=ierr)
          write(uout, nmking)
       endif
       if (ierr.eq.0) then
          if (KRANK.ge.0) then
             if (HREF.eq.' ') HREF = HMOD(1:1)
             call set_king(ierr, KRANK, HMOD, HREF)
          endif
       endif
    enddo
    if (is_eof_ss(ierr)) ierr = 0
    return
  end subroutine init_king
!!!_  & affils_legacy - create affils array from string
  subroutine affils_legacy &
       & (ierr, affils, na, dstr)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: affils(:)
    integer,         intent(out) :: na
    character(len=*),intent(in)  :: dstr

    integer la
    integer js, j, ls

    ierr = 0

    la = size(affils)
    ls = len_trim(dstr)
    na = 0
    js = 1
    do
       if (js.gt.ls) exit
       na = na + 1
       if (na.gt.la) then
! 101       format('affiliaion overflows: ', A, 1x, I0)
!           write(jfpar, 101) trim(dstr), la
          ierr = -1
          return
       endif
       j = 0
       if (js.lt.ls) j = verify(dstr(js+1:ls), '0123456789')
       if (j.eq.0) then
          j = ls
       else
          j = js + j - 1
       endif
       affils(na) = dstr(js:j)
       js = j + 1
    enddo
    return
  end subroutine affils_legacy

!!!_  & verify_sysin
  subroutine verify_sysin &
       & (ierr, cid, ifpar, jfpar)
    use TOUZA_Ppp_std,only: lpath
    use TOUZA_Ppp_std,only: ipc_getcwd
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: cid
    integer,         intent(in)  :: ifpar, jfpar

    character(len=*),parameter :: wild = '*'

    character(len=lverify) :: ID
    character(len=lpath) :: file, dir
    namelist /NMVERIFY/ ID
    integer jerr

    ierr = 0
    ID = ' '
    if (ierr.eq.0) rewind(ifpar, IOSTAT=ierr)
    if (ierr.eq.0) read(ifpar, NMVERIFY, IOSTAT=ierr)
    if (ierr.eq.0) then
       write(jfpar, NMVERIFY, IOSTAT=ierr)
    endif
101 format('sysin: skip verification')
103 format('sysin: wild-card matching for ', A)
104 format('sysin: verified = ', A)
107 format('sysin: Need explicit verifcation id = ', A)
108 format('sysin: Invalid verifcation id = ', A, 1x, A)
109 format('sysin: Primary sysin needs verication id = ', A)
    ierr = 0
    if (cid.eq.' ') then
       if (id.eq.' ') then
          write(jfpar, 101)
       else
          write(jfpar, 109) trim(cid)
          ierr = ERR_FAILURE_INIT
       endif
    else if (id.eq.wild) then
       write(jfpar, 103) trim(cid)
    else if (id.eq.cid) then
       write(jfpar, 104) trim(cid)
    else if (id.eq.' ') then
       write(jfpar, 107) trim(cid)
       ierr = ERR_FAILURE_INIT
    else if (id.eq.' ') then
       write(jfpar, 108) trim(cid), trim(id)
       ierr = ERR_FAILURE_INIT
    endif
    if (ierr.ne.0) then
201    format('CHECK THE CONTENTS: ', A, ' in ', A)
202    format('CHECK THE CONTENTS: ', A)
       inquire(UNIT=ifpar, NAME=file, IOSTAT=jerr)
       if (jerr.eq.0.and.file.ne.' ') then
          call ipc_GETCWD(dir, jerr)
          if (jerr.eq.0) then
             write(jfpar, 201) trim(file), trim(dir)
          else
             write(jfpar, 202) trim(file)
          endif
       endif
    endif
  end subroutine verify_sysin
!!!_ + user interfaces
!!!_  & get_wcolor - query colors under the world
  subroutine get_wcolor(ncolor, icolor)
    implicit none
    integer,intent(out) :: ncolor, icolor
    ncolor = ncolor_world
    icolor = icolor_world
  end subroutine get_wcolor
!!!_  & gen_agent_union - create union agent (XMADRV)
  subroutine gen_agent_union(NEWAGENT, NAME, ALIST, N)
    use TOUZA_Ppp,only: new_agent_derived, query_agent
    implicit none
    integer,         intent(out) :: NEWAGENT
    character(len=*),intent(in)  :: NAME
    character(len=*),intent(in)  :: ALIST(*)
    integer,         intent(in)  :: N
    integer jerr
    call new_agent_derived(jerr, NAME, ALIST(1:N))
    if (jerr.ne.0) then
       NEWAGENT = -1
       return
    endif
    NEWAGENT = query_agent(NAME)
    return
  end subroutine gen_agent_union
!!!_  & push_agent - push agent on stack (XMAPSH)
  subroutine push_agent(ierr, iagnt)
    use TOUZA_Ppp,only: ppp_push_agent=>push_agent
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: iagnt
    call ppp_push_agent(ierr, iagnt)
    if (ierr.ne.0) then
       call terminate(1, 'Fail to push agent', iagnt)
       return
    endif
    return
  end subroutine push_agent
!!!_  & pop_agent - pop agent from stack (XMAPOP)
  subroutine pop_agent(ierr, iagnt)
    use TOUZA_Ppp,only: ppp_pop_agent=>pop_agent
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: iagnt
    call ppp_pop_agent(ierr, iagnt)
    if (ierr.ne.0) then
       call terminate(1, 'Fail to pop agent', iagnt)
       return
    endif
    return
  end subroutine pop_agent
!!!_  & top_agent - get top agent on stack (XMATOP)
  subroutine top_agent(ierr, iagnt)
    use TOUZA_Ppp,only: ppp_top_agent=>top_agent
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: iagnt
    call ppp_top_agent(ierr, iagnt)
    if (ierr.ne.0) then
       call terminate(1, 'Fail to get top agent', ierr)
       return
    endif
    return
  end subroutine top_agent
!!!_  & switch_agent - switch agent on stack (XMASWT)
  subroutine switch_agent(ierr, iagnt)
    use TOUZA_Ppp,only: ppp_switch_agent=>switch_agent
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: iagnt
    call ppp_switch_agent(ierr, iagnt)
    if (ierr.ne.0) then
       call terminate(1, 'Fail to switch agent', iagnt)
       return
    endif
    return
  end subroutine switch_agent
!!!_  & spinoff_agent - create spinoff agent if not exists
  subroutine spinoff_agent &
       & (ierr, iaspin, name, iagnt)
    use TOUZA_Ppp,only: is_child_agent, new_agent_spinoff
    use TOUZA_Ppp,only: top_agent, pop_agent, clone_agent, query_agent
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: iaspin
    character(len=*),intent(in)  :: name
    integer,         intent(in)  :: iagnt
    integer jac
    ierr = 0
    iaspin = -1
    if (is_child_agent(name, iagnt)) then
       jac = clone_agent(iagnt)
       if (jac.ge.0) iaspin = query_agent(name, jac)
    else
       ! to do: improve amng
       call new_agent_spinoff(ierr, name, iagnt, switch=+1)
       if (ierr.eq.0) call top_agent(ierr, iaspin)
       if (ierr.eq.0) call pop_agent(ierr)
    endif
  end subroutine spinoff_agent

!!!_  & query_handle - get agent handle from agent string (XMCiJA)
  subroutine query_handle(IAGNT, HCTZ)
    use TOUZA_Emu,only: get_sysu
    use TOUZA_Ppp,only: query_agent
    use TOUZA_Ppp_std,only: msg
    implicit none
    integer,         intent(out) :: IAGNT
    character(len=*),intent(in)  :: HCTZ     ! <CI>
    integer jfpar
    IAGNT = query_agent(HCTZ)
    if (IAGNT.ge.0) return
    call get_sysu(syso=jfpar)
101 format(' ### query_handle: INVALID HCTZ: ',  A)
    write(jfpar, 101) trim(HCTZ)
    IAGNT = -9
    return
  end subroutine query_handle
!!!_  & query_nprocs - get size and rank from agent (XMjaPr)
  subroutine query_nprocs(NPRZ, IRKZ, IAGNT)
    use TOUZA_Emu,only: get_sysu
    use TOUZA_Ppp,only: inquire_agent
    implicit none
    integer,intent(out) :: NPRZ, IRKZ
    integer,intent(in)  :: IAGNT
    integer jfpar
    integer jerr
    call inquire_agent(jerr, iagent=IAGNT, nrank=NPRZ, irank=IRKZ)
    if (jerr.eq.0) return
    call get_sysu(syso=jfpar)
101 format(' ### query_nprocs: INVALID agent: ',  I0)
    write(jfpar, 101) IAGNT
    NPRZ = 0
    IRKZ = -1
    return
  end subroutine query_nprocs
!!!_  & query_comm - get communicator handle from agent (XMjaCM)
  subroutine query_comm(ICMZ, IAGNT)
    use TOUZA_Emu,only: get_sysu
    use TOUZA_Ppp,only: inquire_agent
    use TOUZA_Ppp_std,only: MPI_COMM_NULL
    implicit none
    integer,intent(out) :: ICMZ
    integer,intent(in)  :: IAGNT
    integer jfpar
    integer jerr
    call inquire_agent(jerr, iagent=IAGNT, icomm=ICMZ)
    if (jerr.eq.0) return
    call get_sysu(syso=jfpar)
101 format(' ### query_comm: INVALID agent: ',  I0)
    write(jfpar, 101) IAGNT
    ICMZ = MPI_COMM_NULL
    return
  end subroutine query_comm
!!!_  & terminate - abort
  subroutine terminate_core(LEV, MSG)
    use TOUZA_Emu,only: get_sysu
    use TOUZA_Ppp_std,only: MPI_Abort, banner
    implicit none
    integer,         intent(in)          :: LEV
    character(len=*),intent(in),optional :: MSG
    integer ierr
    integer jfpar
    call get_sysu(syso=jfpar)
    if (present(msg)) then
       if (jfpar.ge.0) then
          write(jfpar, *) trim(msg)
          call flush(jfpar)
       else
          write(*, *) trim(msg)
       endif
    endif
    if (nproc_quit .le. 0) then ! serial run
       call banner(ierr, 'Abort Serial Execution.', u=jfpar)
    else
       call banner(ierr, 'Abort Parallel Execution.', u=jfpar)

! #ifdef OPT_MPE
!        CALL MX_Fin_MPI
! #endif
       CALL MPI_Abort(icomm_quit, LEV, ierr)
    endif
    if (ierr.eq.0) call finalize(ierr)
  end subroutine terminate_core
  subroutine terminate_aa(LEV, STR0, STR1)
    implicit none
    integer,         intent(in) :: LEV
    character(len=*),intent(in) :: STR0, STR1
    character(len=128) :: msg
101 format(A, 1x, A)
    write(msg, 101) trim(STR0), trim(STR1)
    call terminate_core(LEV, MSG)
  end subroutine terminate_aa
  subroutine terminate_ai(LEV, STR, NUM)
    implicit none
    integer,         intent(in) :: LEV
    character(len=*),intent(in) :: STR
    integer,         intent(in) :: NUM
    character(len=128) :: msg
101 format(A, 1x, I0)
    write(msg, 101) trim(STR), NUM
    call terminate_core(LEV, MSG)
  end subroutine terminate_ai

!!!_ + end module
end module TOUZA_Ppp_Miroc
!!!_* /nonmodule/ interfaces
!!!_ + xmcomm
!!!_  & XCKINI
subroutine XCKINI(AFFILS, N, GREETING, ICROOT)
  use TOUZA_Ppp_miroc,only: init, diag, terminate
  use TOUZA_Ppp_miroc,only: init_rainbow, init_sysio, init_king
  use TOUZA_Ppp_miroc,only: lverify
  use TOUZA_Ppp_amng, only: lagent
  use TOUZA_Ppp_std, only: lpath
  implicit none
  character(len=*),intent(in) :: AFFILS(*) ! array of agents I belong to.
  integer,         intent(in) :: N
  external                    :: GREETING
  integer,         intent(in) :: ICROOT
  integer jerr
  character(len=lpath) :: cdir
  character(len=lagent) :: cagent
  character(len=lverify) :: cid
  call init(jerr, levv=+9, stdv=+9, icomm=ICROOT)
  if (jerr.eq.0) then
     call init_rainbow(jerr, cdir, cagent, cid, AFFILS(1:N), icomm=ICROOT)
  endif
  if (jerr.eq.0) then
     call init_sysio(jerr, cdir, cagent, cid, greeting=GREETING)
  endif
  if (jerr.ne.0) then
     call terminate(1, 'XCKINI FAILED')
     return
  endif
  if (jerr.eq.0) call init_king(jerr)
  call diag(jerr, levv=+2)
  return
end subroutine XCKINI
!!!_  & XCKINI_legacy
subroutine XCKINI_legacy(HDRVR, GREETING, ICROOT)
  use TOUZA_Ppp_miroc,only: init, diag, affils_legacy, terminate
  use TOUZA_Ppp_miroc,only: init_rainbow, init_sysio, init_king
  use TOUZA_Ppp_miroc,only: lverify
  use TOUZA_Ppp_amng, only: lagent
  use TOUZA_Ppp_std, only: lpath
  implicit none
  character(len=*),intent(in) :: HDRVR         ! sequence of <CI> I belong to.
  external                    :: GREETING
  integer,         intent(in) :: ICROOT
  integer jerr
  integer na
  integer,parameter :: maff = 16
  character(len=lagent) :: affils(maff), cagent
  character(len=lpath) :: cdir
  character(len=lverify) :: cid

  jerr = 0

  call init(jerr, levv=+9, stdv=+9, icomm=ICROOT)
  if (jerr.eq.0) call affils_legacy(jerr, affils, na, HDRVR)
  if (jerr.eq.0) then
     call init_rainbow(jerr, cdir, cagent, cid, affils(1:na), icomm=ICROOT)
  endif
  if (jerr.eq.0) then
     ! call init_rainbow(jerr, cdir, icomm=ICROOT)
     call init_sysio(jerr, cdir, cagent, cid, greeting=greeting)
  endif
  if (jerr.ne.0) then
     call terminate(1, 'XCKINI LEGACY FAILED: ', HDRVR)
     return
  endif
  if (jerr.eq.0) call init_king(jerr)

  call diag(jerr, levv=+2)
  return
end subroutine XCKINI_legacy
!!!_  & XCKINI_ils - Simplified agent initialization for MIROC+ILS ILS side
subroutine XCKINI_ils(AFFILS, N, ICROOT)
  use TOUZA_Ppp_miroc,only: init, diag, terminate
  use TOUZA_Ppp_miroc,only: init_rainbow, switch_dir
  use TOUZA_Ppp_miroc,only: lverify
  use TOUZA_Ppp_amng, only: lagent, inquire_agent
  use TOUZA_Ppp_std, only: lpath
  implicit none
  character(len=*),intent(in)  :: AFFILS(*) ! array of agents I belong to.
  integer,         intent(in)  :: N
  integer,         intent(in)  :: ICROOT
  integer jerr
  character(len=lpath) :: cdir
  character(len=lagent) :: cagent
  character(len=lverify) :: cid

  call init(jerr, levv=+9, stdv=+9, icomm=ICROOT)
  if (jerr.eq.0) then
     call init_rainbow(jerr, cdir, cagent, cid, AFFILS(1:N), icomm=ICROOT)
  endif
  if (jerr.eq.0) then
     if (cdir.ne.' ') call switch_dir(jerr, cdir)
  endif
  if (jerr.ne.0) then
     call terminate(1, 'XCKINI FAILED')
     return
  endif
  call diag(jerr, levv=+2)
  return
end subroutine XCKINI_ils

!!!_  & XMSETK (XCKINI entry) - DELETED
subroutine XMSETK(OKING, HC)
  use TOUZA_Ppp_miroc,only: terminate
  implicit none
  logical,         intent(out) :: OKING    ! whether or not I am the king
  character(len=*),intent(in)  :: HC       ! <CI>
  OKING = .FALSE.
  call terminate(1, 'DELETED: XMSETK', HC)
end subroutine XMSETK
!!!_  & XMGETK (XCKINI entry) - legacy, deprecated
subroutine XMGETK(HM, HC, IR, HR)
  use TOUZA_Ppp,only: get_king, inquire_agent
  use TOUZA_Ppp_miroc,only: lmod
  implicit none
  integer,         intent(out) :: IR       ! King rank for the input module
  character(len=*),intent(in)  :: HM       ! Module indicator
  character(len=*),intent(in)  :: HC       ! <CI>
  character(len=*),intent(in)  :: HR       ! message text
  integer jerr
  character(len=lmod) :: HA
  logical ismem
  integer jfp
  HA = HM(1:1)
  call get_king(jerr, IR, HM, HA)
  if (jerr.eq.0) then
     if (HC.ne.' ') then
        call get_king(jerr, IR, HM, HC(1:1))
     else
        call inquire_agent(jerr, agent=HA, ismem=ismem)
        if (.not.ismem) IR = -999
     endif
     ! if (HC(1:1).eq.'/'.or.HC(1:1).eq.'W') then
     !    call get_king(jerr, IR, HM, 'W')
     ! else if (HC.ne.' ') then
     !    call get_king(jerr, IR, HM, HC(1:1))
     ! endif
  endif
  call getjfp(jfp)
101 format('XMGETK: DEPRECATED (', A, ')')
  if (jfp.ge.0) then
     write(jfp, 101) trim(HR)
  else
     write(*, 101) trim(HR)
  endif
end subroutine XMGETK
!!!_  & XMOKNG
subroutine XMOKNG(HM, HC, OR, HR)
  implicit none
  logical,         intent(out) :: OR
  character(len=*),intent(in)  :: HM
  character(len=*),intent(in)  :: HC
  character(len=*),intent(in)  :: HR
  integer iking, IR, NR
  call xmgetk(HM, HC, iking, HR)
  call xmproc(NR, IR, HC)
  OR = (IR.ge.0) .and. (IR.eq.iking)
end subroutine XMOKNG
!!!_  & XMProc (XCKINI entry)
subroutine XMProc(NPRZ, IRKZ, HCTZ)
  use TOUZA_Ppp_std,only: MPI_UNDEFINED
  use TOUZA_Ppp,only: inquire_agent
  use TOUZA_Ppp_miroc,only: terminate
  implicit none
  integer,         intent(out) :: NPRZ     ! # of rank
  integer,         intent(out) :: IRKZ     ! PE number
  character(len=*),intent(in)  :: HCTZ     ! <CI>
  integer jerr

  call inquire_agent(jerr, agent=HCTZ, nrank=NPRZ, irank=IRKZ)
  if (jerr.eq.0) then
     if (IRKZ.eq.MPI_UNDEFINED) then
        NPRZ = -999
        IRKZ = -999
     endif
     return
  endif
  call terminate(1, ' ### XMproc: INVALID HCTZ:', HCTZ)
end subroutine XMProc
!!!_  & XMComm (XCKINI entry)
subroutine XMComm(OCMZ, HCTZ)
  use TOUZA_Ppp,only: inquire_agent
  use TOUZA_Ppp_miroc,only: terminate
  implicit none
  logical,         intent(out) :: OCMZ     ! whether or not I belong to comm.
  character(len=*),intent(in)  :: HCTZ     ! <CI>
  integer jerr
  integer ir
  call inquire_agent(jerr, agent=HCTZ, irank=ir)
  if (jerr.eq.0) then
     OCMZ = (ir.ge.0)
     return
  endif
  call terminate(1, ' ### XMcomm: INVALID HCTZ:', HCTZ)
end subroutine XMComm
!!!_  & XMIComm (XCKINI entry)
subroutine XMIComm(ICMZ, HCTZ)
  use TOUZA_Ppp,only: inquire_agent
  use TOUZA_Ppp_miroc,only: terminate
  implicit none
  integer,         intent(out) :: ICMZ     ! communicator handle
  character(len=*),intent(in)  :: HCTZ     ! <CI>
  integer jerr
  call inquire_agent(jerr, agent=HCTZ, icomm=ICMZ)
  if (jerr.eq.0) return
  call terminate(1, ' ### XMIcomm: INVALID HCTZ:', HCTZ)
end subroutine XMIComm
!!!_  & XMGetColor
subroutine XMGetColor(ICLR, NCLR)
  use TOUZA_Ppp_miroc,only: get_wcolor
  implicit none
  integer,intent(out) :: ICLR, NCLR     ! color
  call get_wcolor(NCLR, ICLR)
end subroutine XMGetColor
!!!_  & MMGetColor (XCKINI entry)  (alias to xmgetcolor)
subroutine MMGetColor(ICLR, NCLR)
  use TOUZA_Ppp_miroc,only: get_wcolor
  implicit none
  integer,intent(out) :: ICLR, NCLR     ! color
  call get_wcolor(NCLR, ICLR)
end subroutine MMGetColor
!!!_  & XMquit - initialize quit
subroutine XMquit(NPR0I, ICOM0I)
  use TOUZA_Ppp_miroc, only: nproc_quit, icomm_quit
  implicit none
  integer,intent(in) :: NPR0I, ICOM0I
  nproc_quit = NPR0I
  icomm_quit = ICOM0I
  return
end subroutine XMquit
!!!_  & XMabort (XMquit entry)
subroutine XMabort(LEV)
  use TOUZA_Ppp_miroc,only: terminate
  implicit none
  integer,intent(in) :: LEV
  call terminate(LEV)
end subroutine XMabort
!!!_  & XMabort0 (XMquit entry)
subroutine XMabort0
  use TOUZA_Ppp_miroc,only: terminate
  implicit none
  call terminate(1)
end subroutine XMabort0
!!!_  & XMFinal (XMquit entry)
subroutine XMFinal(OBARR)
  use TOUZA_Ppp_miroc,only: nproc_quit, icomm_quit, finalize
#if OPT_USE_MPI
  use MPI,only: MPI_Barrier, MPI_Finalize, MPI_Finalized
#endif
  use TOUZA_Ppp_std,only: safe_mpi_finalize, banner
  implicit none
  logical,intent(in) :: OBARR  ! final barrier switch
  integer ierr, jerr

  ierr = 0
  if (nproc_quit .le. 0) then ! serial run
     call banner(jerr, 'End Serial Execution.')
     call finalize(ierr)
  else
     call banner(jerr, 'End Parallel Execution.')
#if OPT_USE_MPI
     if (obarr) call MPI_Barrier(icomm_quit, ierr)
! #ifdef OPT_MPE
!      CALL MX_Fin_MPI
! #endif
#endif /* OPT_USE_MPI */
     if (ierr.eq.0) call safe_mpi_finalize(ierr)
     if (ierr.eq.0) call finalize(ierr)
  endif
end subroutine XMFinal
!!!_@ test_ppp_miroc - test program
#if TEST_PPP_MIROC
program test_ppp_miroc
  use TOUZA_Ppp,only: diag_cache, show_status, lagent
  use TOUZA_Ppp,only: is_member, inquire_agent, query_agent
  use TOUZA_Ppp,only: new_agent_spinoff, new_agent_root, new_agent_derived
  use TOUZA_Ppp_miroc
  use TOUZA_Ppp_std,only: safe_mpi_init
  use TOUZA_Ppp_std,only: safe_mpi_finalize
  use TOUZA_Ppp_std,only: MPI_Comm_size, MPI_Comm_rank
  use TOUZA_Ppp_std,only: MPI_COMM_WORLD, MPI_UNDEFINED
  implicit none
  integer ierr
#define _DRIVER_A 'AB0C1'
#define _DRIVER_O 'OO0O1'
#define _DRIVER_S 'AB0C1OO0O1'
#define _DRIVER_Z 'Z'

#define _DRIVER_5MA 'A    '
#define _DRIVER_5MO 'O    '
#define _DRIVER_5IM 'MAT  '
#define _DRIVER_5IC 'CAMA '
#define _DRIVER_5II 'IO   '
#define _DRIVER_5M  'MIROC'   /* AGCM OGCM */
#define _DRIVER_5I  'ILS  '   /* MAT CAMA IO */
#define _DRIVER_5X  'MICI '   /* AGCM MAT CAMA */

#define _DRIVER_5G  'GICI '   /* AGCM + ILS */

#define _TEST_A         1   /* A only */
#define _TEST_O         2   /* O only */
#define _TEST_AO_BOTH   3   /* A + O */
#define _TEST_AO_SWITCH 4   /* A or O (switched internally) */
#define _TEST_MI        5   /* MIROC ILS emulation */

# if TEST_PPP_MIROC == _TEST_A
#   define _DRIVER _DRIVER_A
# elif TEST_PPP_MIROC == _TEST_O
#   define _DRIVER _DRIVER_O
# elif TEST_PPP_MIROC == _TEST_AO_BOTH
#   define _DRIVER _DRIVER_S
# elif TEST_PPP_MIROC == _TEST_AO_SWITCH
#   define _IS_A(J,N) MOD(J+3,4).lt.2
#   define _IS_O(J,N) MOD(J,4).eq.0
# else
#   define _DRIVER(J,N) MOD(J, 5)
# endif
  integer iaref
  integer nrw, irw

  integer kaa, koo
  integer kaw, kow
  integer jfpar
  integer icomma, nra, ira
  integer icommo, nro, iro
  integer jdmy
  integer,parameter :: NRLIM = 17
  integer ibase, icomm, icol
  character(len=lagent) :: pname
#if TEST_PPP_MIROC < _TEST_MI
  integer jseq
#endif

  external greeting
  ierr = 0

  call safe_mpi_Init(jdmy)
  ibase = MPI_COMM_WORLD
  call MPI_Comm_size(ibase, nrw, jdmy)
  call MPI_Comm_rank(ibase, irw, jdmy)
#if TEST_PPP_MIROC < _TEST_MI
  if (nrw.gt.NRLIM) then
     jseq = irw - (nrw - nrlim) / 2
     icol = 0
     if (jseq.lt.0.or.jseq.ge.NRLIM) icol = MPI_UNDEFINED
#   if OPT_USE_MPI
     if (ierr.eq.0) call MPI_Comm_split(ibase, icol, jseq, icomm, jdmy)
     if (ierr.eq.0) ibase = icomm
#   endif
  endif
#else /* not TEST_PPP_MIROC < _TEST_MI */
  icol = 0
#endif /* not TEST_PPP_MIROC < _TEST_MI */

  if (icol.ne.MPI_UNDEFINED) then
#if TEST_PPP_MIROC < _TEST_AO_SWITCH
     call XCKINI_legacy(_DRIVER, greeting, ibase)
#elif TEST_PPP_MIROC == _TEST_AO_SWITCH
     if (_IS_A(irw, nrw)) then
        call XCKINI_legacy(_DRIVER_A, greeting, ibase)
     else if (_IS_O(irw, nrw)) then
        call XCKINI_legacy(_DRIVER_O, greeting, ibase)
     else
        call XCKINI_legacy(_DRIVER_Z, greeting, ibase)
     endif
#else
     select case (_DRIVER(irw, nrw))
     case(0)
        call XCKINI((/_DRIVER_5MA, _DRIVER_5M, _DRIVER_5X/), 3, greeting, ibase)
     case(1)
        call XCKINI((/_DRIVER_5MO, _DRIVER_5M/), 2, greeting, ibase)
     case(2)
        call XCKINI_ils((/_DRIVER_5IM, _DRIVER_5I, _DRIVER_5X/), 3, ibase)
     case(3)
        call XCKINI_ils((/_DRIVER_5IC, _DRIVER_5I, _DRIVER_5X/), 3, ibase)
     case(4)
        call XCKINI_ils((/_DRIVER_5II, _DRIVER_5I/), 2, ibase)
     case default
        write(*, *) 'PANIC. '
        stop
     end select
#endif
#if TEST_PPP_MIROC == _TEST_MI
     ! iaref = query_agent(_DRIVER_5M)
     ! call new_agent_spinoff(ierr, 'W', iaref)
     call new_agent_derived(ierr, 'W',    (/_DRIVER_5M/))
     call new_agent_derived(ierr, _DRIVER_5G, (/_DRIVER_5MA, _DRIVER_5I/))
     ! call switch_agent(ierr, iaref)
#else
     call gen_agent_union(jdmy, 'W', (/'A', 'O'/), 2)
#endif
     call gen_agent_union(jdmy, 'H', (/'@'/), 1)

     iaref = query_agent('/')

     call getjfp(JFPAR)
     call diag_agent_maps(ierr, iaref, JFPAR)

     call show_status(ierr, tag='current')

     call inquire_agent(ierr, source=1, icomm=icomm, name=pname)
     if (jfpar.ge.0) then
        write(jfpar, *) 'parent:', trim(pname), icomm
     else
        write(*, *) 'parent:', trim(pname), icomm
     endif


     call XMGETK('AM', ' ', kaa, 'a root')
     call XMGETK('AM', 'W', kaw, 'a root')
     call XMGETK('OM', ' ', koo, 'o root')
     call XMGETK('OM', 'W', kow, 'o root')

     if (jfpar.ge.0) then
        write(jfpar, *) 'king:a/w', kaw
        write(jfpar, *) 'king:o/@', koo
        write(jfpar, *) 'king:a/@', kaa
        write(jfpar, *) 'king:o/w', kow
        call flush(jfpar)
     else
        write(*, *) 'king:a/w', kaw
        write(*, *) 'king:o/@', koo
        write(*, *) 'king:a/@', kaa
        write(*, *) 'king:o/w', kow
     endif

     call XMIcomm(icomma, 'A')
     call XMproc(NRA, IRA, 'A')
     call XMIcomm(icommo, 'O')
     call XMproc(NRO, IRO, 'O')

     if (jfpar.ge.0) then
        write(jfpar, *) 'A', NRA, IRA, icomma
        write(jfpar, *) 'O', NRO, IRO, icommo
        call flush(jfpar)
     else
        write(*, *) 'A', NRA, IRA, icomma
        write(*, *) 'O', NRO, IRO, icommo
     endif

     call diag_cache(ierr)
     call XMfinal(.FALSE.)
  else
     call safe_mpi_finalize(jdmy)
  endif

  stop
end program test_ppp_miroc
!!!_ + greeting
subroutine greeting(jfpar)
  use TOUZA_std,only: banner     ! direct use of touza_std, intentional
  implicit none
  integer,intent(in) :: jfpar
  integer jerr
  call banner(jerr, 'PPP MIROC', jfpar)
end subroutine greeting
#endif /* TEST_PPP_MIROC */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

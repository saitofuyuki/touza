!!!_! ppp_miroc.F90 - TOUZA/Ppp MIROC compatible interfaces
! Maintainer: SAITO Fuyuki
! Created: Feb 2 2022
#define TIME_STAMP 'Time-stamp: <2025/05/23 13:10:29 fuyuki ppp_miroc.F90>'
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
       integer,optional,intent(in) :: ICROOT
       external :: greeting
     end subroutine XCKINI
     subroutine XCKINI_legacy(HDRVR, GREETING, ICROOT)
       implicit none
       character(len=*),intent(in) :: HDRVR         ! sequence of <CI> I belong to.
       integer,optional,intent(in) :: ICROOT
       external :: greeting
     end subroutine XCKINI_legacy
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
       logical,optional,intent(in) :: OBARR  ! final barrier switch
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
!!!_  - public
  public init, diag, finalize
  public get_wcolor
  public affils_legacy
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
  public XCKINI_legacy
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm, affils, greeting)
    use TOUZA_Ppp,only: ppp_init=>init
    use TOUZA_Ppp,only: control_mode, control_deep, is_first_force
    use TOUZA_Ppp,only: choice, is_msglev_NORMAL
    use TOUZA_Std,only: mwe_init, bld_init
    use TOUZA_Emu,only: usi_init
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv, mode, stdv
    integer,         intent(in),optional :: icomm
    character(len=*),intent(in),optional :: affils(:)
    optional :: greeting
    interface
       subroutine greeting(jfpar)
         implicit none
         integer,intent(in) :: jfpar
       end subroutine greeting
    end interface

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
       if (present(affils)) then
          if (ierr.eq.0) call init_batch(ierr, affils(:), greeting, icomm=icomm)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Ppp,only: ppp_diag=>diag, ppp_msg=>msg
    use TOUZA_Std,only: control_mode, control_deep, is_first_force
    use TOUZA_Std,only: mwe_diag, get_logu, choice, is_msglev_NORMAL
    use TOUZA_Std,only: bld_diag
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
    use TOUZA_Std,only: mwe_finalize, bld_finalize
    use TOUZA_Ppp,only: ppp_finalize=>finalize
    use TOUZA_Ppp,only: control_mode, control_deep, is_first_force
    use TOUZA_Ppp,only: get_logu, choice, trace_fine
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

!!!_ + init subcontracts
!!!_  & init_batch - XCKINI compatible procedure
  subroutine init_batch &
       & (ierr, affils, greeting, icomm)
    use TOUZA_Std,only: set_defu
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: affils(:)
    integer,optional,intent(in)  :: icomm
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

    ierr = 0
    call init_root(ierr, nrank, irank)

    if (ierr.eq.0) call init_sysio(ierr, ifpar, jfpar, nrank, irank)
    call set_defu(jfpar)
    if (present(greeting)) then
       call greeting(jfpar)
    endif

    if (ierr.eq.0) &
         & call init_rainbow(ierr, ncolor, icolor, nrank, irank, ifpar, jfpar)
    if (ierr.eq.0) &
         call init_sysi_colored(ierr, ifpar, jfpar, ncolor, icolor)
    if (ierr.eq.0) then
       call init_comms &
            & (ierr, affils(:), ncolor, icolor, nrank, irank, ifpar, jfpar, icomm)
    endif
    if (ierr.eq.0) &
         call init_king(ierr, ifpar, jfpar)

    ! store cache
    if (ierr.eq.0) then
       icolor_world = icolor
       ncolor_world = ncolor
    endif
    return
  end subroutine init_batch
!!!_  & init_root
  subroutine init_root &
       & (ierr, nrank, irank)
    use TOUZA_Std,only: get_ni, get_wni
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: nrank, irank
    integer icomm
    logical ocheck

    ierr = 0
    call CLCSTR('Comm.')
    call MPI_Initialized(ocheck, ierr)
    if (.not.ocheck) CALL MPI_Init(ierr)
#ifdef OPT_MPE
    call MX_Init_MPE
#endif
    call get_wni(ierr, nrank, irank, icomm)

    nproc_quit = nrank
    icomm_quit = icomm

    call CLCEND('Comm.')
  end subroutine init_root
!!!_  - init_sysio
  subroutine init_sysio &
       & (ierr, ifpar, jfpar, nrank, irank)
    use TOUZA_Emu,only: update_ranks, open_sysin, open_sysout
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: ifpar, jfpar
    integer,intent(in)  :: nrank, irank
    integer jerri, jerro

    ierr = 0

    call update_ranks(nr=nrank, ir=irank)
    call open_sysin(jerri)
    call open_sysout(jerro)
    call REWNML(ifpar, jfpar)

    if (jerro.ne.0) ierr = jerro
    if (jerri.ne.0) ierr = jerri

    return
  end subroutine init_sysio

!!!_  - init_sysi_colored
  subroutine init_sysi_colored &
       & (ierr, ifpar, jfpar, ncolor, icolor)
    use TOUZA_Emu,only: update_color, open_sysin, get_sysu
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: ifpar
    integer,intent(in)  :: jfpar
    integer,intent(in)  :: ncolor, icolor

    ierr = 0
    call update_color(icol=icolor, ncol=ncolor)
    if (ierr.eq.0) call open_sysin(ierr)
    if (ierr.eq.0) call get_sysu(sysi=ifpar)
    return
  end subroutine init_sysi_colored

!!!_  & init_rainbow - rainbow sectioning with namelists
  subroutine init_rainbow &
       & (ierr,   &
       &  ncolor, icolor, &
       &  nrank,  irank,  ifpar,  jfpar)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: ncolor, icolor
    integer,intent(in)  :: nrank,  irank
    integer,intent(in)  :: ifpar,  jfpar

    integer  ISPLR
    namelist /NMWSPL/ ISPLR

    ierr = 0
    icolor = 0
    ncolor = 0

    rewind(unit=ifpar, IOSTAT=ierr)
    do
       if (ierr.ne.0) exit
       if (ierr.eq.0) then
          isplr = -1
          read (ifpar, nmwspl, IOSTAT=ierr)
          write(jfpar, nmwspl)
       endif
       if (ierr.eq.0) then
          ! isplr==0 is trivial
          if (isplr.gt.0 .and. isplr.lt.nrank) then
             ncolor = ncolor + 1
             if (isplr.gt.irank) icolor = icolor + 1
          endif
       endif
    enddo
    if (ncolor.gt.0) ncolor = ncolor + 1
    ierr = max(0, ierr) ! clear eof
101 format('xmcomm: color = ', I0, 1x, I0, 1x, I0)
    if (ierr.eq.0) then
       icolor = max(0, ncolor - icolor - 1)
       write(jfpar, 101) irank, icolor, ncolor
    endif

    return
  end subroutine init_rainbow
!!!_  & init_comms
  subroutine init_comms &
       & (ierr,   &
       &  affils, &
       &  ncolor, icolor, &
       &  nrank,  irank,  ifpar,  jfpar, icomm)
    use MPI,only: MPI_COMM_WORLD
    use TOUZA_Std,only: choice, get_comm
    use TOUZA_Ppp,only: lagent, &
         & new_agent_color, new_agent_derived, &
         & new_agent_family, new_agent_root
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: affils(:)
    integer,         intent(in)  :: ncolor, icolor
    integer,         intent(in)  :: nrank,  irank
    integer,         intent(in)  :: ifpar,  jfpar
    integer,optional,intent(in)  :: icomm

    integer ic

    ierr = 0

    if (present(icomm)) then
       ic = icomm
    else
       call get_comm(ierr, ic)
    endif

    if (ierr.eq.0) call new_agent_root(ierr, ic)
    if (ierr.eq.0) call new_agent_color(ierr, icolor, 'X')
    if (ierr.eq.0) call new_agent_family(ierr, affils(:))

  end subroutine init_comms

!!!_  & init_king - king configuration with namelists
  subroutine init_king &
       & (ierr,   &
       &  ifpar,  jfpar)
    use TOUZA_Ppp,only: is_eof_ss, set_king
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: ifpar,  jfpar

    character(len=lmod) :: HMOD, HREF
    integer krank
    NAMELIST /NMKING/ HMOD, KRANK, HREF
    ierr = 0

    ! call set_king(ierr, 0, 'A', 'A')
    ! call set_king(ierr, 0, 'O', 'O')

    rewind(unit=ifpar, IOSTAT=ierr)
    do
       if (ierr.ne.0) exit
       if (ierr.eq.0) then
          HMOD = ' '
          KRANK = -1
          HREF = ' '
          read (ifpar, nmking, IOSTAT=ierr)
          write(jfpar, nmking)
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
!!!_  - affils_legacy - create affils array from string
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
    use TOUZA_Ppp,only: is_child_agent, new_agent_spinoff, &
         &              top_agent, pop_agent, clone_agent, query_agent
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
    use TOUZA_Ppp,only: query_agent, msg
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
    use MPI, only: MPI_COMM_NULL
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
    use MPI,only: MPI_Abort
    implicit none
    integer,         intent(in)          :: LEV
    character(len=*),intent(in),optional :: MSG
    integer ierr
    integer jfpar
    if (present(msg)) then
       call get_sysu(syso=jfpar)
       if (jfpar.ge.0) then
          write(jfpar, *) trim(msg)
       else
          write(*, *) trim(msg)
       endif
    endif
    if (nproc_quit .le. 0) then ! serial run
       call MSGBOX('Abort Serial Execution.')
    else
       call MSGBOX('Abort Parallel Execution.')

       CALL CLCSTR('Comm.')
#ifdef OPT_MPE
       CALL MX_Fin_MPI
#endif
       CALL MPI_Abort(icomm_quit, LEV, ierr)
       CALL CLCEND( 'Comm.' )
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
  use TOUZA_Ppp_miroc,only: init, diag
  implicit none
  character(len=*),intent(in) :: AFFILS(*) ! array of agents I belong to.
  integer,         intent(in) :: N
  integer,optional,intent(in) :: ICROOT
  external :: greeting
  integer jerr
  call init &
       & (jerr, levv=+9, stdv=+9, &
       &  icomm=ICROOT, affils=AFFILS(1:N), greeting=greeting)
  call diag(jerr, levv=+2)
  return
end subroutine XCKINI
!!!_  & XCKINI_legacy
subroutine XCKINI_legacy(HDRVR, GREETING, ICROOT)
  use TOUZA_Ppp_miroc,only: init, diag, affils_legacy, terminate
  use TOUZA_Ppp_amng, only: lagent
  implicit none
  character(len=*),intent(in) :: HDRVR         ! sequence of <CI> I belong to.
  integer,optional,intent(in) :: ICROOT
  external :: greeting
  integer jerr
  integer na
  integer,parameter :: maff = 16
  character(len=lagent) :: affils(maff)

  jerr = 0
  call affils_legacy(jerr, affils, na, HDRVR)
  if (jerr.ne.0) then
     call terminate(1, 'XCKINI LEGACY FAILED: ', HDRVR)
     return
  endif

  call init &
       & (jerr, levv=+9, stdv=+9, &
       &  icomm=ICROOT, affils=affils(1:na), greeting=greeting)
  call diag(jerr, levv=+2)
  return
end subroutine XCKINI_legacy
!!!_  & XMSETK (XCKINI entry) - DELETED
subroutine XMSETK(OKING, HC)
  use TOUZA_Ppp_miroc,only: terminate
  implicit none
  logical,         intent(out) :: OKING    ! whether or not I am the king
  character(len=*),intent(in)  :: HC       ! <CI>
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
  use MPI,only: MPI_UNDEFINED
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
  use MPI,only: MPI_Barrier, MPI_Finalize, MPI_Finalized
  use TOUZA_Ppp_miroc,only: nproc_quit, icomm_quit, finalize
  use TOUZA_Std,only: choice
  implicit none
  logical,optional,intent(in) :: OBARR  ! final barrier switch
  integer ierr
  logical ocheck, bb

  ierr = 0
  if (nproc_quit .le. 0) then ! serial run
     call MSGBOX('End Serial Execution.')
     if (ierr.eq.0) call finalize(ierr)
  else
     call MSGBOX('End Parallel Execution.')

     call CLCSTR('Comm.')
     bb = choice(.TRUE., obarr)
     if (bb) call MPI_Barrier(icomm_quit, ierr)
#ifdef OPT_MPE
     CALL MX_Fin_MPI
#endif
     if (ierr.eq.0) call finalize(ierr)
     call MPI_Finalized(ocheck, ierr)
     if (.not.ocheck) call MPI_Finalize(ierr)
     call CLCEND('Comm.')
  endif
end subroutine XMFinal
!!!_@ test_ppp_miroc - test program
#if TEST_PPP_MIROC
program test_ppp_miroc
  use TOUZA_Ppp,only: diag_cache
  use TOUZA_Ppp_miroc
  use MPI
  implicit none
  integer ierr
# if TEST_PPP_MIROC == 1
#   define _DRIVER 'AB0C1'
# elif TEST_PPP_MIROC == 2
#   define _DRIVER 'OO0O1'
# else
#   define _DRIVER 'AB0C1OO0O1'
# endif
  integer nrw, irw

  integer kaa, koo
  integer kaw, kow
  integer jfpar
  integer icomma, nra, ira
  integer icommo, nro, iro
  integer jdmy
  integer,parameter :: NRLIM = 17
  integer ibase, icomm, jseq, icol
  external greeting
  ierr = 0

  call MPI_Init(jdmy)
  ibase = MPI_COMM_WORLD
  call MPI_Comm_size(ibase, nrw, jdmy)
  call MPI_Comm_rank(ibase, irw, jdmy)
  if (nrw.gt.NRLIM) then
     jseq = irw - (nrw - nrlim) / 2
     icol = 0
     if (jseq.lt.0.or.jseq.ge.NRLIM) icol = MPI_UNDEFINED
     if (ierr.eq.0) call MPI_Comm_split(ibase, icol, jseq, icomm, jdmy)
     ibase = icomm
  endif

  if (icol.ne.MPI_UNDEFINED) then
     call XCKINI_legacy(_DRIVER, greeting, icomm)
     call gen_agent_union(jdmy, 'W', (/'A', 'O'/), 2)
     call gen_agent_union(jdmy, 'H', (/'@'/), 1)

     call getjfp(JFPAR)
     call diag_agent_maps(ierr, JFPAR)

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
     call MPI_Finalize(jdmy)
  endif


  stop
end program test_ppp_miroc
!!!_ + greeting
subroutine greeting(jfpar)
  use TOUZA_Std,only: banner
  implicit none
  integer,intent(in) :: jfpar
  integer jerr
  call banner(jerr, 'PPP MIROC', jfpar)
end subroutine greeting

!!!_ + dummy subroutines for test
subroutine MSGBOX(A)
  implicit none
  character(len=*),intent(in) :: A
  write(*, *) 'msgbox:', trim(A)
end subroutine MSGBOX
subroutine CLCSTR(A)
  implicit none
  character(len=*),intent(in) :: A
end subroutine CLCSTR
subroutine CLCEND(A)
  implicit none
  character(len=*),intent(in) :: A
end subroutine CLCEND
#endif /* TEST_PPP_MIROC */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

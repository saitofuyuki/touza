!!!_! ppp_miroc.F90 - TOUZA/Ppp MIROC compatible interfaces
! Maintainer: SAITO Fuyuki
! Created: Feb 2 2022
#define TIME_STAMP 'Time-stamp: <2022/02/15 18:13:30 fuyuki ppp_miroc.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022
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
!!!_  - interfaces (external)
  interface
     subroutine XCKINI(HDRVR, GREETING)
       implicit none
       character(len=*),intent(in) :: HDRVR         ! sequence of <CI> I belong to.
       external :: GREETING
     end subroutine XCKINI
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
     subroutine XMFinal()
       implicit none
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
  public XCKINI, MMGetColor, XMGetColor, XMIComm, XMCOMM, XMGETK, XMProc, XMOKNG
  public XMquit, XMabort,    XMabort0,   XMFinal
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm, dstr, greeting)
    use TOUZA_Ppp,only: ppp_init=>init, choice, &
         & control_mode, control_deep, is_first_force, is_msglev_NORMAL
    use TOUZA_Std,only: mwe_init
    use TOUZA_Emu,only: usi_init
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv, mode, stdv
    integer,         intent(in),optional :: icomm
    character(len=*),intent(in),optional :: dstr
    optional :: greeting
    interface
       subroutine greeting(jfpar)
         implicit none
         integer,intent(in) :: jfpar
       end subroutine greeting
    end interface

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
          if (ierr.eq.0) call mwe_init(ierr, u, stdv, mode=lmd, icomm=icomm)
          if (ierr.eq.0) call usi_init(ierr, u, levv, mode=lmd, stdv=stdv, icomm=icomm)
       endif
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ppp_init(ierr, u, levv, mode=md, stdv=stdv, icomm=icomm)
       endif
       if (present(dstr)) then
          if (ierr.eq.0) call init_batch(ierr, dstr, greeting)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Ppp,only: ppp_diag=>diag, ppp_msg=>msg
    use TOUZA_Std,only: mwe_diag, control_mode, control_deep, get_logu, choice, is_first_force, is_msglev_NORMAL
    use TOUZA_Emu,only: usi_diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: levv
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(diag_counts, md)) then
          if (is_msglev_normal(lv)) then
             if (ierr.eq.0) call ppp_msg(TIME_STAMP, __MDL__, u)
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ppp_diag(ierr, u, levv, md)
       endif
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call mwe_diag(ierr, u, levv, lmd)
          if (ierr.eq.0) call usi_diag(ierr, u, levv, lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std,only: mwe_finalize
    use TOUZA_Ppp,only: ppp_finalize=>finalize, &
         & control_mode, control_deep, get_logu, choice, &
         & is_first_force, trace_fine
    use TOUZA_Emu,only: usi_finalize
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
               &  grp=__GRP__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ppp_finalize(ierr, u, levv, md)
       endif
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call usi_finalize(ierr, u, levv, mode=lmd)
          if (ierr.eq.0) call mwe_finalize(ierr, u, levv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + init subcontracts
!!!_  & init_batch - XCKINI compatible procedure
  subroutine init_batch &
       & (ierr, dstr, greeting)
    use TOUZA_Std,only: set_defu
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: dstr
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
    if (ierr.eq.0) &
         call init_comms(ierr, dstr, ncolor, icolor, nrank, irank, ifpar, jfpar)
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

    ierr = 0
    call update_ranks(nr=nrank, ir=irank)
    if (ierr.eq.0) call open_sysin(ierr)
    if (ierr.eq.0) call open_sysout(ierr)
    if (ierr.eq.0) call REWNML(ifpar, jfpar)
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
       &  dstr,   &
       &  ncolor, icolor, &
       &  nrank,  irank,  ifpar,  jfpar, icomm)
    use MPI,only: MPI_COMM_WORLD
    use TOUZA_Std,only: choice
    use TOUZA_Ppp,only: ldrv, &
         & new_agent_color, new_agent_derived, &
         & new_agent_family, new_agent_root
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: dstr
    integer,         intent(in)  :: ncolor, icolor
    integer,         intent(in)  :: nrank,  irank
    integer,         intent(in)  :: ifpar,  jfpar
    integer,optional,intent(in)  :: icomm

    integer ic
    integer,parameter :: mdrvs = 16
    character(len=ldrv) :: drivers(mdrvs)

    integer js, j, nd, ls

    ierr = 0

    ls = len_trim(dstr)
    nd = 0
    js = 1
    do
       if (js.gt.ls) exit
       nd = nd + 1
       if (nd.gt.mdrvs) then
101       format('driver overflows: ', A, 1x, I0)
          write(jfpar, 101) trim(dstr), mdrvs
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
       drivers(nd) = dstr(js:j)
       js = j + 1
    enddo

    ic = choice(MPI_COMM_WORLD, icomm)
    if (ierr.eq.0) call new_agent_root(ierr, ic)
    if (ierr.eq.0) call new_agent_color(ierr, icolor, 'X')
    if (ierr.eq.0) call new_agent_family(ierr, drivers(1:nd))
    if (ierr.eq.0) call new_agent_derived(ierr, 'W', (/'A', 'O'/))
    if (ierr.eq.0) call new_agent_derived(ierr, 'H', (/'@'/))

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
!!!_ + user interfaces
  subroutine get_wcolor(ncolor, icolor)
    implicit none
    integer,intent(out) :: ncolor, icolor
    ncolor = ncolor_world
    icolor = icolor_world
  end subroutine get_wcolor
!!!_ + end module
end module TOUZA_Ppp_Miroc
!!!_* /nonmodule/ interfaces
!!!_ + xmcomm
!!!_  & XCKINI
subroutine XCKINI(HDRVR, GREETING)
  use TOUZA_Ppp_miroc,only: init, diag
  implicit none
  character(len=*),intent(in) :: HDRVR         ! sequence of <CI> I belong to.
  external :: greeting
  integer jerr
  call init(jerr, levv=+9, stdv=+9, dstr=HDRVR, greeting=greeting)
  call diag(jerr, levv=+1)
  return
end subroutine XCKINI
!!!_  & XMSETK (XCKINI entry) - DELETED
subroutine XMSETK(OKING, HC)
  implicit none
  logical,         intent(out) :: OKING    ! whether or not I am the king
  character(len=*),intent(in)  :: HC       ! <CI>
  call XMabort_msg('DELETED: XMSETK', HC)
end subroutine XMSETK
!!!_  & XMGETK (XCKINI entry) - legacy, deprecated
subroutine XMGETK(HM, HC, IR, HR)
  use TOUZA_Ppp,only: get_king
  use TOUZA_Ppp_miroc,only: lmod
  implicit none
  integer,         intent(out) :: IR       ! King rank for the input module
  character(len=*),intent(in)  :: HM       ! Module indicator
  character(len=*),intent(in)  :: HC       ! <CI>
  character(len=*),intent(in)  :: HR       ! message text
  integer jerr
  character(len=lmod) :: HA
  HA = HM(1:1)
  call get_king(jerr, IR, HM, ' ', HA)
  if (jerr.eq.0) then
     if (HC(1:1).eq.'/'.or.HC(1:1).eq.'W') then
        call get_king(jerr, IR, HM, 'W')
     else if (HC.ne.' ') then
        call get_king(jerr, IR, HM, HC(1:1))
     endif
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
  call XMabort_msg(' ### XMproc: INVALID HCTZ:', HCTZ)
end subroutine XMProc
!!!_  & XMComm (XCKINI entry)
subroutine XMComm(OCMZ, HCTZ)
  use TOUZA_Ppp,only: inquire_agent
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
  call XMabort_msg(' ### XMcomm: INVALID HCTZ:', HCTZ)
end subroutine XMComm
!!!_  & XMIComm (XCKINI entry)
subroutine XMIComm(ICMZ, HCTZ)
  use TOUZA_Ppp,only: inquire_agent
  implicit none
  integer,         intent(out) :: ICMZ     ! communicator handle
  character(len=*),intent(in)  :: HCTZ     ! <CI>
  integer jerr
  call inquire_agent(jerr, agent=HCTZ, icomm=ICMZ)
  if (jerr.eq.0) return
  call XMabort_msg(' ### XMIcomm: INVALID HCTZ:', HCTZ)
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
!!!_ + XMabort_msg
subroutine XMabort_msg(a, b)
  use TOUZA_Emu,only: get_sysu
  implicit none
  character(len=*),intent(in) :: a
  character(len=*),intent(in) :: b
  integer jfpar
  call get_sysu(syso=jfpar)
  write(jfpar, *) trim(a), ' ', trim(b)
  CALL XMABORT(1)
end subroutine XMabort_msg
!!!_  & XMabort (XMquit entry)
subroutine XMabort(LEV)
  use MPI,only: MPI_Abort
  use TOUZA_Ppp_miroc, only: nproc_quit, icomm_quit, finalize
  implicit none
  integer,intent(in) :: LEV
  integer ierr
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
end subroutine XMabort
!!!_  & XMabort0 (XMquit entry)
subroutine XMabort0
  implicit none
  call XMabort(1)
end subroutine XMabort0
!!!_  & XMFinal (XMquit entry)
subroutine XMFinal()
  use MPI,only: MPI_Barrier, MPI_Finalize, MPI_Finalized
  use TOUZA_Ppp_miroc,only: nproc_quit, icomm_quit, finalize
  implicit none
  integer ierr
  logical ocheck

  ierr = 0
  if (nproc_quit .le. 0) then ! serial run
     call MSGBOX('End Serial Execution.')
     if (ierr.eq.0) call finalize(ierr)
  else
     call MSGBOX('End Parallel Execution.')

     call CLCSTR('Comm.')
     call MPI_Barrier(icomm_quit, ierr)
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
  implicit none
  integer ierr
# if TEST_PPP_MIROC == 1
#   define _DRIVER 'A'
# elif TEST_PPP_MIROC == 2
#   define _DRIVER 'O'
# else
#   define _DRIVER 'AO'
# endif
  integer kaa, koo
  integer kaw, kow
  integer jfpar
  integer icomma, nra, ira
  integer icommo, nro, iro
  external greeting
  ierr = 0

  call XCKINI(_DRIVER, greeting)
  call getjfp(JFPAR)

  call XMGETK('AM', ' ', kaa, 'a root')
  call XMGETK('AM', 'W', kaw, 'a root')
  call XMGETK('OM', ' ', koo, 'o root')
  call XMGETK('OM', 'W', kow, 'o root')

  write(jfpar, *) 'king:a/w', kaw
  write(jfpar, *) 'king:o/@', koo
  write(jfpar, *) 'king:a/@', kaa
  write(jfpar, *) 'king:o/w', kow

  call XMIcomm(icomma, 'A')
  call XMproc(NRA, IRA, 'A')
  call XMIcomm(icommo, 'O')
  call XMproc(NRO, IRO, 'O')

  write(jfpar, *) 'A:', ira, nra, icomma
  write(jfpar, *) 'O:', iro, nro, icommo

  call diag_cache(ierr)
  call XMfinal()

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

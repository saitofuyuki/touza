!!!_! ppp_comm.F90 - TOUZA/ppp communicator control (xmcomm core replacement)
! Maintainer: SAITO Fuyuki
! Created: Jan 25 2022
#define TIME_STAMP 'Time-stamp: <2022/02/16 15:28:10 fuyuki ppp_comm.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022
!           Japan Agency for Marine-Earth Science and Technology
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_ppp.h"
!!!_* macros
#ifndef   TEST_PPP_COMM
#  define TEST_PPP_COMM 0
#endif
#ifndef   DEBUG_COMM
#  define DEBUG_COMM TEST_PPP_COMM
#endif
#ifndef   OPT_DRIVER_LEN
#  define OPT_DRIVER_LEN 8
#endif
#ifndef   OPT_DRIVER_MAX
#  define OPT_DRIVER_MAX 128
#endif
!!!_@ TOUZA_Ppp_comm - MPI communicator control
module TOUZA_Ppp_comm
!!!_ + modules
  use MPI,only: MPI_COMM_NULL, MPI_GROUP_NULL
  use TOUZA_Ppp_std,only: &
       & control_mode, control_deep, is_first_force, &
       & get_logu,     unit_global,  trace_fine,   trace_control
!!!_ + default
  implicit none
  private
!!!_ + parameter
  integer,parameter,public :: ldrv = OPT_DRIVER_LEN
  integer,parameter,public :: lgtbl = OPT_DRIVER_MAX
!!!_ + public
!!!_  - reserved <CI>
  character(len=*),parameter,public :: ci_root = '/'
  character(len=*),parameter,public :: ci_base = '%'     ! clone of source communicator
  character(len=*),parameter,public :: ci_unit = '@'     ! minimum independent combination
!!!_ + flags (fltable)
  integer,parameter :: flag_none    = -1
  integer,parameter :: flag_self    = 0
  integer,parameter :: flag_friend  = 1
  integer,parameter :: flag_other   = 2
  integer,parameter :: flag_derived = 3
  integer,parameter :: flag_root    = 4
  integer,parameter :: flag_base    = 5
  integer,parameter :: flag_unit    = 6
  character(len=*),parameter :: flagch(flag_none:flag_unit) = &
       & (/'N', 'S', 'F', 'O', 'D', 'R', 'B', 'U' /)
!!!_ + static
!!!_  - communicator manager
  integer,save             :: mgtbl = 0
  character(len=ldrv),save :: gtbl_agent(0:lgtbl) = ' '            ! agent string (communicator indicator <CI>)
  integer,save             :: gtbl_comm(0:lgtbl)  = MPI_COMM_NULL  ! communicator
  integer,save             :: gtbl_mgrp(0:lgtbl)  = MPI_GROUP_NULL ! mpi group
  integer,save             :: gtbl_src(0:lgtbl)   = 0              ! source communicator index
  integer,save             :: gtbl_flg(0:lgtbl)   = flag_none      ! flag

  integer,save             :: cur_agent = -1
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = PPP_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
#define __MDL__ 'c'
!!!_ + overload
  interface switch_agent
     module procedure switch_agent_ai
  end interface switch_agent
  interface inquire_agent
     module procedure inquire_agent_a
     module procedure inquire_agent_i
  end interface inquire_agent
!!!_ + interfaces
  public init, diag, finalize
  public new_agent_root
  public new_agent_color, new_agent_family
  public new_agent_derived
  public inquire_agent
  public query_agent, source_agent
!!!_ + common interfaces
contains
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm)
    use TOUZA_Ppp_std,only: choice, ps_init=>init
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
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ps_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
       endif
       if (is_first_force(init_counts, md)) then
          continue
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Ppp_std,only: choice, msg, ps_diag=>diag, is_msglev_normal
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
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __MDL__, utmp)
          endif
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call diag_table(ierr, utmp)
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ps_diag(ierr, utmp, levv=lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Ppp_std,only: ps_finalize=>finalize, choice
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
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ps_finalize(ierr, utmp, levv=lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
  end subroutine finalize

!!!_ + diag subcontracts
  subroutine diag_table &
       & (ierr, u)
    use MPI,only: &
         & MPI_COMM_WORLD, MPI_Comm_rank, &
         & MPI_Group_rank, MPI_Group_size
    use TOUZA_Ppp_std,only: get_wni_safe, get_ni, get_gni, msg
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u

    integer utmp
    integer jt
    integer irw
    integer ico, irc, nrc
    integer igr, irg, nrg
    character(len=256) :: txt
    character(len=1) :: TM

    ierr = 0
    utmp = get_logu(u, ulog)

    call get_wni_safe(ierr, irank=irw)

    if (irw.lt.0) then
       call msg('mpi deactivated.', __MDL__, utmp)
    else
101    format('comm:', I0, 1x, I0, ':', A, 1x, 2('[', I0, ':', I0, ']'), 1x, I0, 1x, A)
       do jt = 0, mgtbl - 1
          ico = gtbl_comm(jt)
          call get_ni(ierr, nrc, irc, ico)
          igr = gtbl_mgrp(jt)
          call get_gni(ierr, igr, nrg, irg)
          if (cur_agent.eq.jt) then
             TM = '*'
          else
             TM = ' '
          endif
          write(txt, 101) irw, jt, &
               & trim(gtbl_agent(jt)), &
               & irg, nrg, irc, nrg, &
               & gtbl_src(jt), trim(flagch(gtbl_flg(jt)) // TM)
          call msg(txt, __MDL__, utmp)
       enddo
    endif
  end subroutine diag_table
!!!_ + manipulation
!!!_  & query_agent() - return agent id from NAME
  integer function query_agent &
       & (name, iagent) &
       & result(n)
    implicit none
    character(len=*),intent(in) :: name
    integer,optional,intent(in) :: iagent ! current agent if null
    integer jas
    integer jt

    n = -1
    jas = source_agent(iagent)
    if (jas.lt.0) return
    if (name.eq.' ') then
       jt = cur_agent
       if (gtbl_src(jt).eq.jas) then
          n = jt
       endif
    else
       do jt = 0, mgtbl - 1
          ! search agents with the same source
          if (gtbl_src(jt).eq.jas) then
             if (gtbl_agent(jt).eq.name) then
                n = jt
                exit
             endif
          endif
       enddo
    endif
    return
  end function query_agent
!!!_  & inquire_agent
  subroutine inquire_agent_a &
       & (ierr,  &
       &  agent, &
       &  irank, nrank, icomm, igroup, name)
    implicit none
    integer,         intent(out)          :: ierr
    character(len=*),intent(in)           :: agent
    integer,         intent(out),optional :: irank
    integer,         intent(out),optional :: nrank
    integer,         intent(out),optional :: icomm
    integer,         intent(out),optional :: igroup
    character(len=*),intent(out),optional :: name
    integer ja
    ja = query_agent(agent)
    call inquire_agent_i(ierr, ja, irank, nrank, icomm, igroup, name)
    return
  end subroutine inquire_agent_a

  subroutine inquire_agent_i &
       & (ierr,   &
       &  iagent, &
       &  irank,  nrank, icomm, igroup, name)
    implicit none
    integer,         intent(out)          :: ierr
    integer,         intent(in), optional :: iagent
    integer,         intent(out),optional :: irank
    integer,         intent(out),optional :: nrank
    integer,         intent(out),optional :: icomm
    integer,         intent(out),optional :: igroup
    character(len=*),intent(out),optional :: name
    integer ja
    integer ic, ig

    ierr = 0
    ja = check_agent(iagent)
    if (ja.lt.0) ierr = -1

    if (ierr.eq.0) ic = gtbl_comm(ja)
    if (ierr.eq.0) ig = gtbl_mgrp(ja)
    if (present(irank)) then
       if (ierr.eq.0) then
          call MPI_Group_rank(ig, irank, ierr)
       else
          irank = -1
       endif
    endif
    if (present(nrank)) then
       if (ierr.eq.0) then
          call MPI_Group_size(ig, nrank, ierr)
       else
          nrank = -1
       endif
    endif
    if (present(icomm)) then
       if (ierr.eq.0) then
          icomm = ic
       else
          icomm = MPI_COMM_NULL
       endif
    endif
    if (present(igroup)) then
       if (ierr.eq.0) then
          igroup = ig
       else
          igroup = MPI_GROUP_NULL
       endif
    endif
    if (present(name)) then
       if (ierr.eq.0) then
          name = gtbl_agent(ja)
       else
          name = ' '
       endif
    endif
  end subroutine inquire_agent_i
!!!_  & switch_agent
  subroutine switch_agent_ai (ierr, iagent)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: iagent
    ierr = 0
    if (iagent.lt.0.or.iagent.ge.mgtbl) ierr = -1
    if (ierr.eq.0) cur_agent = iagent
  end subroutine switch_agent_ai
!!!_ + registration
!!!_  & new_agent_root
  subroutine new_agent_root &
       & (ierr, icomm, name)
    use TOUZA_Ppp_std,only: get_ni, get_comm, choice_a
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in),optional :: icomm
    character(len=*),intent(in),optional :: name

    integer icsrc
    integer jagent
    character(len=ldrv) ctmp

    ierr = 0
    if (present(icomm)) then
       icsrc = icomm
    else
       call get_comm(ierr, icsrc)
    endif
    if (ierr.eq.0) call choice_a(ctmp, ci_root, name)
    if (ierr.eq.0) call add_entry_comm(ierr, jagent, ctmp, icsrc)
    if (ierr.eq.0) call switch_agent(ierr, jagent)
    return
  end subroutine new_agent_root

!!!_  & new_agent_color
  subroutine new_agent_color &
       & (ierr, color, name, src)
    use MPI,only: MPI_Comm_split
    use TOUZA_Ppp_std,only: get_ni
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: color
    character(len=*),intent(in),optional :: name
    integer,         intent(in),optional :: src

    integer jas,   jau
    integer icsrc, icunit
    integer irank, nrank

    ! no group control except for local color
    jas = check_agent(src)
    if (jas.lt.0) ierr = -1

    if (ierr.eq.0) then
       icsrc = gtbl_comm(jas)
       call get_ni(ierr, nrank, irank, icsrc)
    endif
    if (ierr.eq.0) then
       call MPI_Comm_split(icsrc, color, irank, icunit, ierr)
    endif
    if (ierr.eq.0) then
       ! jau is dummy
       ! clone base agent from source
       call add_entry_comm(ierr, jau, ci_base, src=jas, flag=flag_base)
    endif
    if (ierr.eq.0) then
       ! add unit agent
       call add_entry_comm(ierr, jau, ci_unit, icunit, src=jas, flag=flag_unit)
    endif
    if (ierr.eq.0) then
       ! clone unit agent to NAME
       if (present(NAME)) then
          call add_entry_comm(ierr, jau, NAME, icunit, src=jas, flag=flag_derived)
       endif
    endif
    if (ierr.eq.0) call switch_agent(ierr, jau)
    return
  end subroutine new_agent_color
!!!_  & new_agent_family
  subroutine new_agent_family &
       & (ierr, drivers, src)
    use TOUZA_Ppp_std,only: get_ni
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: drivers(:)   ! self drivers to belong to
    integer,optional,intent(in)  :: src

    integer icsrc, icunit
    integer igsrc
    integer jas,   jau
    integer nt
    integer nrsrc, irsrc
    integer,parameter :: lttbl = lgtbl
    character(len=ldrv) :: tci(lttbl)
    integer             :: tui(lttbl)
    integer             :: tgr(lttbl)
    integer,allocatable :: rgw(:)
    ierr = 0

    jas = check_agent(src)
    if (jas.lt.0) ierr = -1

    if (ierr.eq.0) then
       icsrc = gtbl_comm(jas)
       igsrc = gtbl_mgrp(jas)
       call merge_comm_table(ierr, nt, tci, tui, drivers(:), icsrc)
    endif

    if (ierr.eq.0) call get_ni(ierr, nrsrc, irsrc, icsrc)
    if (ierr.eq.0) allocate(rgw(0:nrsrc-1), STAT=ierr)
    if (ierr.eq.0) then
       call batch_group_split(ierr, rgw, tgr, tci, nt, drivers(:), icsrc, igsrc, irsrc)
    endif
    if (ierr.eq.0) deallocate(rgw, STAT=ierr)

    if (ierr.eq.0) then
       call gen_comm_unit(ierr, icunit, tci, tui, nt, drivers(:), icsrc)
    endif
    if (ierr.eq.0) then
       ! jau is dummy
       ! clone base agent from source
       call add_entry_comm(ierr, jau, ci_base, src=jas, flag=flag_base)
    endif
    if (ierr.eq.0) then
       ! add unit agent
       call add_entry_comm(ierr, jau, ci_unit, icunit, src=jas, flag=flag_unit)
    endif
    if (ierr.eq.0) then
       call add_agent_family(ierr, tgr, tci, tui, nt, drivers(:), jas)
    endif
    if (ierr.eq.0) call switch_agent(ierr, jau)
  end subroutine new_agent_family

!!!_  & new_agent_derived
  subroutine new_agent_derived &
       & (ierr, name, drivers, iagent)
    use MPI,only: MPI_UNDEFINED
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: name         ! new name
    character(len=*),intent(in)  :: drivers(:)   ! derived drivers to belong to
    integer,optional,intent(in)  :: iagent       ! current agent-id if null

    integer jas, jad
    integer igdrv
    integer flg
    integer ird

    ierr = 0

    jas = source_agent(iagent)
    if (jas.lt.0) ierr = -1
    if (ierr.eq.0) then
       call derive_group &
            & (ierr,      igdrv,  &
            &  gtbl_mgrp, gtbl_agent, gtbl_src, mgtbl, jas, drivers)
    endif
    if (ierr.eq.0) call MPI_Group_rank(igdrv, ird, ierr)
    if (ierr.eq.0) then
       if (ird.eq.MPI_UNDEFINED) then
          flg = flag_other
       else
          flg = flag_derived
       endif
       call add_entry_group(ierr, jad, name, igdrv, jas, flg)
    endif
     return
  end subroutine new_agent_derived

!!!_ + internal procedures
!!!_  & check_agent
  integer function check_agent &
       & (iagent) &
       & result(n)
    use TOUZA_Ppp_std,only: choice
    implicit none
    integer,intent(in),optional :: iagent
    n = choice(cur_agent, iagent)
    if (n.lt.0.or.n.ge.mgtbl) then
       n = -1
    else if (gtbl_flg(n).le.flag_none) then
       n = -1
    endif
  end function check_agent

!!!_  & source_agent
  integer function source_agent &
       & (iagent) &
       & result(n)
    use TOUZA_Ppp_std,only: choice
    implicit none
    integer,intent(in),optional :: iagent
    n = check_agent(iagent)
    if (n.lt.0) return
    n = gtbl_src(n)
  end function source_agent

!!!_  & add_agent_family
  subroutine add_agent_family &
       & (ierr, &
       &  tgr,  &
       &  tci,  tui,  nt,  drivers, src)
    use MPI,only: MPI_Comm_create
    use TOUZA_Ppp_std,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: tgr(:)
    character(len=*),intent(in)  :: tci(:)
    integer,         intent(in)  :: tui(:)
    character(len=*),intent(in)  :: drivers(:)
    integer,         intent(in)  :: nt
    integer,         intent(in)  :: src

    integer ja, je, jt
    integer icsrc, icnew
    integer uself
    ierr = 0

    ja = mgtbl
    mgtbl = mgtbl + nt
    if (mgtbl.gt.lgtbl) then
       ierr = -1
       return
    endif

    je = ja + nt - 1
    gtbl_agent(ja:je) = tci(1:nt)
    gtbl_mgrp(ja:je)  = tgr(1:nt)
    gtbl_src(ja:je)   = src

    icsrc = gtbl_comm(src)
    do jt = 1, nt
       if (ierr.eq.0) then
          call MPI_Comm_create(icsrc, tgr(jt), icnew, ierr)
       endif
       if (ierr.eq.0) gtbl_comm(ja + jt - 1) = icnew
    enddo
    ! find self unit id
    uself = -1
    do jt = 1, nt
       if (tci(jt).eq.drivers(1)) then
          uself = tui(jt)
          exit
       endif
    enddo
    if (uself.lt.0) then
       ierr = -1
       return
    endif

    do jt = 1, nt
       je = ja + jt - 1
       if (ANY(tci(jt).eq.drivers(:))) then
          gtbl_flg(je) = flag_self
       else if (tui(jt).eq.uself) then
          gtbl_flg(je) = flag_friend
       else
          gtbl_flg(je) = flag_other
       endif
    enddo

  end subroutine add_agent_family
!!!_  & add_entry_comm
  subroutine add_entry_comm &
       & (ierr, iagent, name, icomm, src, flag)
    use MPI,only: MPI_Comm_group
    use TOUZA_Ppp_std,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: iagent
    character(len=*),intent(in)  :: name
    integer,optional,intent(in)  :: icomm ! either icomm or src must be exit
    integer,optional,intent(in)  :: src
    integer,optional,intent(in)  :: flag

    integer jgrp

    ierr = 0
    iagent = mgtbl
    mgtbl = mgtbl + 1
    if (mgtbl.gt.lgtbl) then
       ierr = -1
       return
    endif

    gtbl_agent(iagent) = name
    gtbl_flg(iagent)   = choice(flag_root, flag)

    if (present(icomm)) then
       gtbl_comm(iagent) = icomm
       gtbl_src(iagent)  = choice(-1, src)
       call MPI_Comm_group(icomm, jgrp, ierr)
       if (ierr.eq.0) then
          gtbl_mgrp(iagent) = jgrp
       else
          gtbl_mgrp(iagent) = MPI_GROUP_NULL
          ierr = -1
       endif
    else if (present(src)) then
       gtbl_src(iagent) = choice(-1, src)
       if (src.lt.0.or.src.ge.mgtbl) then
          ierr = -1
       else
          gtbl_comm(iagent) = gtbl_comm(src)
          gtbl_mgrp(iagent) = gtbl_mgrp(src)
       endif
    else
       ierr = -1
    endif
    return
  end subroutine add_entry_comm

!!!_  & add_entry_group
  subroutine add_entry_group &
       & (ierr, iagent, name, igroup, src, flag)
    use MPI,only: MPI_Comm_create
    use TOUZA_Ppp_std,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: iagent
    character(len=*),intent(in)  :: name
    integer,         intent(in)  :: igroup
    integer,         intent(in)  :: src
    integer,         intent(in)  :: flag

    integer icsrc, icnew

    ierr = 0
    iagent = mgtbl
    mgtbl = mgtbl + 1
    if (mgtbl.gt.lgtbl) then
       ierr = -1
       return
    endif

    gtbl_agent(iagent) = name
    gtbl_flg(iagent)   = flag
    gtbl_src(iagent)   = src
    gtbl_mgrp(iagent)  = igroup

    icsrc = gtbl_comm(src)
    call MPI_Comm_create(icsrc, igroup, icnew, ierr)
    if (ierr.eq.0) gtbl_comm(iagent) = icnew
    return
  end subroutine add_entry_group

!!!_  & merge_comm_table - merge <CI> table
  subroutine merge_comm_table &
       & (ierr, &
       &  ntotal,  tbl_ci,  tbl_ui, &
       &  drivers, icomm)
    use MPI,only: &
         & MPI_STATUS_SIZE, MPI_CHARACTER, MPI_INTEGER
#  if HAVE_FORTRAN_MPI_MPI_BCAST
    use MPI,only: MPI_Send, MPI_Bcast
#  endif
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ntotal       ! total number of <CI>
    character(len=*),intent(out) :: tbl_ci(*)    ! <CI> table merged
    integer,         intent(out) :: tbl_ui(*)    ! <CI> unit-id table
    character(len=*),intent(in)  :: drivers(:)   ! drivers (self <CI>) table
    integer,         intent(in)  :: icomm        ! mpi communicator

    integer nrank, irank, iroot
    integer,parameter :: lbuf = OPT_DRIVER_MAX
    integer,parameter :: ndiv = 3

    character(len=ldrv) :: dbuf(lbuf)
    integer             :: ubuf(lbuf)
    integer mstp,  ktag
    integer ipsrc, ipdst
    integer msend
    integer j

    ierr = 0
    iroot = 0

    call get_cnr(ierr, icomm, irank, nrank)

    if (ierr.eq.0) then
       ntotal = size(drivers)
       dbuf(1:ntotal) = drivers(1:ntotal)
       ubuf(1:ntotal) = 0
    endif

    if (ierr.eq.0) then
       if (irank.eq.iroot) then
          ! recieve only
          mstp = ndiv
          do
             ktag = 0
             do j = 1, ndiv - 1
                ipsrc = irank + j * (mstp / ndiv)
                if (ipsrc.ge.nrank) exit
                ktag = ktag + 1
                if (ierr.eq.0) then
                   call recv_drivers &
                        & (ierr,  ntotal, dbuf, ubuf, &
                        &  icomm, ipsrc,  ktag)
                endif
             enddo
             if (ktag.eq.0) exit
             mstp = mstp * ndiv
          enddo
       else
          mstp = ndiv
          do
             if (mod(irank, mstp).ne.0) then
                ! send and exit
                ipdst = (irank / mstp) * mstp
                ktag  = (irank - ipdst) / (mstp / ndiv)
                msend = ntotal * ldrv
                if (ierr.eq.0) then
                   call MPI_Send &
                        & (ubuf(1:ntotal), ntotal, MPI_INTEGER, &
                        &  ipdst, ktag,    icomm,  ierr)
                endif
                if (ierr.eq.0) then
                   call MPI_Send &
                        & (dbuf(1:ntotal), msend, MPI_CHARACTER, &
                        &  ipdst, ktag,    icomm, ierr)
                endif
                exit
             else
                ! recieve and continue
                ktag = 0
                do j = 1, ndiv - 1
                   ipsrc = irank + j * (mstp / ndiv)
                   if (ipsrc.ge.nrank) exit
                   ktag = ktag + 1
                   if (ierr.eq.0) then
                      call recv_drivers &
                           & (ierr,  ntotal, dbuf,  ubuf, &
                           &  icomm, ipsrc,  ktag)
                   endif
                enddo
                mstp = mstp * ndiv
             endif
          enddo
       endif
    endif
    if (ierr.eq.0) then
       call MPI_Bcast(ntotal, 1, MPI_INTEGER, iroot, icomm, ierr)
    endif
    if (ierr.eq.0) then
       call MPI_Bcast(ubuf(1:ntotal), ntotal, MPI_INTEGER, iroot, icomm, ierr)
    endif
    if (ierr.eq.0) then
       msend = ntotal * ldrv
       call MPI_Bcast(dbuf(1:ntotal), msend, MPI_CHARACTER, iroot, icomm, ierr)
    endif
    if (ierr.eq.0) then
       tbl_ci(1:ntotal) = dbuf(1:ntotal)
       tbl_ui(1:ntotal) = ubuf(1:ntotal)
    endif
    return
  end subroutine merge_comm_table
!!!_  & recv_drivers
  subroutine recv_drivers &
       & (ierr,  ntotal, dbuf,  ubuf, &
       &  icomm, ipsrc,  ktag)
    use MPI,only: &
         & MPI_STATUS_SIZE, MPI_CHARACTER, MPI_INTEGER, &
         & MPI_Probe, MPI_Get_count
#  if HAVE_FORTRAN_MPI_MPI_BCAST
    use MPI,only: MPI_Recv
#  endif
    implicit none
    integer,            intent(out)   :: ierr
    integer,            intent(inout) :: ntotal
    character(len=ldrv),intent(inout) :: dbuf(:)
    integer,            intent(inout) :: ubuf(:)
    integer,            intent(in)    :: icomm
    integer,            intent(in)    :: ipsrc, ktag

    integer,parameter :: lrcv = OPT_DRIVER_MAX
    character(len=ldrv) :: drcv(lrcv)
    integer             :: urcv(lrcv)
    integer :: istts(MPI_STATUS_SIZE)
    integer nrcv
    integer j, jj
    integer jold
    integer,parameter :: uref = HUGE(0)
    integer uold, unew
    integer mold
    integer norg

    ierr = 0

    norg = ntotal
    mold = maxval(ubuf(1:norg)) + 1

    call MPI_Probe(ipsrc, ktag, icomm, istts, ierr)
    if (ierr.eq.0) call MPI_Get_count(istts, MPI_INTEGER, nrcv, ierr)
    if (ierr.eq.0) then
       call MPI_Recv &
            & (urcv(1:nrcv), nrcv, MPI_INTEGER, &
            &  ipsrc, ktag,  icomm, istts, ierr)
    endif
    if (ierr.eq.0) then
       call MPI_Recv &
            & (drcv(1:nrcv), (nrcv * ldrv), MPI_CHARACTER, &
            &  ipsrc, ktag,  icomm, istts, ierr)
    endif
    if (ierr.eq.0) then
       ! merge
       urcv(1:nrcv) = urcv(1:nrcv) + mold
       do j = 1, nrcv
          jold = 0
          ! FINDLOC()
          do jj = 1, ntotal
             if (dbuf(jj).eq.drcv(j)) then
                jold = jj
                exit
             endif
          enddo
          if (jold.eq.0) then
             ! new <CI>
             ntotal = ntotal + 1
             dbuf(ntotal) = drcv(j)
             ubuf(ntotal) = urcv(j)
          else
             ! existing <CI>
              uold = max(ubuf(jold), urcv(j))
              unew = min(ubuf(jold), urcv(j))
              where (urcv(1:nrcv)  .eq.uold) urcv(1:nrcv)   = unew
              where (ubuf(1:ntotal).eq.uold) ubuf(1:ntotal) = unew
          endif
       enddo
    endif
    if (ierr.eq.0) then
       ! adjustment, not efficient
       unew = maxval(ubuf(1:norg)) + 1
       jj = norg + 1
       do
          uold = uref
          do j = jj, ntotal
             if (ubuf(j).ge.unew) uold = min(ubuf(j), uold)
          enddo
          if (uold.eq.uref) exit
          where(ubuf(jj:ntotal).eq.uold) ubuf(jj:ntotal) = unew
          unew = unew + 1
       enddo
    endif
    return
  end subroutine recv_drivers

!!!_  - batch_group_split
  subroutine batch_group_split &
       & (ierr,   ranks,  tbl_gr, &
       &  tbl_ci, ntotal, drivers, icomm, igsrc, ir)
    use MPI,only: &
         & MPI_UNDEFINED, MPI_COMM_NULL, MPI_INTEGER
#  if HAVE_FORTRAN_MPI_MPI_BCAST
    use MPI,only: MPI_Gather, MPI_Bcast
#  endif
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ranks(0:)  ! work-array
    integer,         intent(out) :: tbl_gr(*)
    character(len=*),intent(in)  :: tbl_ci(*)
    integer,         intent(in)  :: ntotal
    character(len=*),intent(in)  :: drivers(:)
    integer,         intent(in)  :: icomm
    integer,         intent(in)  :: igsrc
    integer,         intent(in)  :: ir

    integer ibuf(1)
    integer iroot
    integer jgnew
    integer j, m

    ierr = 0
    if (ierr.eq.0) then
       iroot = 0
       do j = 1, ntotal
          if (ANY(drivers(:).eq.tbl_ci(j))) then
             ibuf(1) = ir
          else
             ibuf(1) = -1
          endif
          call MPI_Gather(ibuf, 1, MPI_INTEGER, ranks, 1, MPI_INTEGER, iroot, icomm, ierr)
          if (ierr.ne.0) exit
          if (ir.eq.iroot) then
             m = COUNT(ranks(:).ge.0)
             ranks(0:m-1) = PACK(ranks(:), ranks(:).ge.0)
          endif
          call MPI_Bcast(m, 1, MPI_INTEGER, iroot, icomm, ierr)
          if (ierr.eq.0) then
             call MPI_Bcast(ranks(0:m-1), m, MPI_INTEGER, iroot, icomm, ierr)
          endif
          if (ierr.eq.0) call MPI_Group_incl(igsrc, m, ranks(0:m-1), jgnew, ierr)
          if (ierr.eq.0) tbl_gr(j) = jgnew
       enddo
    endif

  end subroutine batch_group_split

!!!_  - gen_comm_unit
  subroutine gen_comm_unit &
       & (ierr,   icunit, &
       &  tbl_ci, tbl_ui, ntotal, drivers, icomm)
    use MPI,only: &
         & MPI_UNDEFINED, MPI_COMM_NULL, &
         & MPI_Comm_split
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: icunit
    character(len=*),intent(in)  :: tbl_ci(*)
    integer,         intent(in)  :: tbl_ui(*)
    integer,         intent(in)  :: ntotal
    character(len=*),intent(in)  :: drivers(*)
    integer,         intent(in)  :: icomm

    integer irank
    integer ju, munit
    integer js
    integer newc
    logical inc

    ierr = 0
    call get_cnr(ierr, icomm, irank)

    if (ierr.eq.0) then
       munit = maxval(tbl_ui(1:ntotal))
       ! write(*, *) irank, munit, tbl_ui(1:ntotal)
       do ju = 0, munit
          inc = .FALSE.
          do js = 1, ntotal
             ! only drivers(1) is used
             if (tbl_ci(js).eq.drivers(1)) then
                inc = (tbl_ui(js).eq.ju)
                exit
             endif
          enddo
          if (inc) then
             CALL MPI_Comm_split &
                  & (icomm,  ju,  irank, newc, ierr)
             icunit = newc
          else
             CALL MPI_Comm_split &
                  & (icomm,  MPI_UNDEFINED, -999, newc, ierr)
          endif
       enddo
    endif
  end subroutine gen_comm_unit

!!!_  & derive_group
  subroutine derive_group &
       & (ierr,    igdrv,  &
       &  tbl_gr,  tbl_ci, tbl_bs, nt, src, drivers)
    use MPI,only: &
         & MPI_GROUP_EMPTY, &
         & MPI_Group_union
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: igdrv
    integer,         intent(in)  :: tbl_gr(0:*)
    integer,         intent(in)  :: tbl_bs(0:*)
    character(len=*),intent(in)  :: tbl_ci(0:*)
    integer,         intent(in)  :: nt
    character(len=*),intent(in)  :: drivers(:)
    integer,         intent(in)  :: src

    integer jt
    integer igdst, igtmp

    ierr = 0
    igtmp = MPI_GROUP_EMPTY
    igdrv = igtmp
    do jt = 0, nt - 1
       if (tbl_bs(jt).ne.src) cycle
       if (ANY(drivers(:).eq.tbl_ci(jt))) then
          if (ierr.eq.0) call MPI_Group_union(igtmp, tbl_gr(jt), igdst, ierr)
          if (ierr.eq.0) igtmp = igdst
       endif
    enddo
    if (ierr.eq.0) igdrv = igtmp
  end subroutine derive_group

!!!_  - get_cnr
  subroutine get_cnr &
       & (ierr, icomm, irank, nrank)
    use TOUZA_Ppp_std,only: get_comm, get_ni, choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: icomm
    integer,optional,intent(out) :: irank, nrank
    integer nr, ir

    if (ierr.eq.0) call get_ni(ierr, nr, ir, icomm)
    if (ierr.eq.0) then
       if (present(irank)) then
          irank = ir
       endif
       if (present(nrank)) then
          nrank = nr
       endif
    endif
  end subroutine get_cnr

!!!_ + end TOUZA_Ppp_comm
end module TOUZA_Ppp_comm
!!!_@ test_ppp_comm - test program
#if TEST_PPP_COMM
program test_ppp_comm
  use MPI
  use TOUZA_Ppp_comm
  implicit none

  integer ierr
  integer icw
  integer irw, nrw
  integer mclr
  integer color
  integer j
  integer jarg
  integer ktest
  character(len=256) :: T

  ierr = 0
  jarg = 0

101 format(A, ' = ', I0)
  call init(ierr)
  write(*, 101) 'INIT', ierr

  icw = MPI_COMM_WORLD
  call MPI_Comm_rank(icw, irw, ierr)
  call MPI_Comm_size(icw, nrw, ierr)

  if (ierr.eq.0) then
     jarg = jarg + 1
     call get_command_argument(jarg, T, STATUS=ierr)
     if (ierr.ne.0) T = '0'
     read(T, *, IOSTAT=ierr) ktest
  endif

  mclr = 11
  color = 0
  do j = 0, nrw / mclr
     if (irw.ge.j*mclr .and. irw.lt.(j+1)*mclr) exit
     color = color + 1
  enddo
  call test_ppp_agent(icw, color, ktest)

  call diag(ierr)
  write(*, 101) 'DIAG', ierr
  call finalize(ierr)
  write(*, 101) 'FINAL', ierr
  stop
contains
  subroutine test_ppp_agent(icomm, color, ktest)
    use TOUZA_Std,only: get_ni
    implicit none
    integer,intent(in) :: icomm
    integer,intent(in) :: color
    integer,intent(in) :: ktest
    integer,parameter :: ltest = 32
    integer nself
    character(len=ldrv) :: drvs(ltest)
    integer irank, nrank

    ierr = 0

    if (ierr.eq.0) then
       call new_agent_root(ierr, icomm)
    endif
    if (ierr.eq.0) call new_agent_color(ierr, color)

    if (ierr.eq.0) call inquire_agent(ierr, irank=irank, nrank=nrank)
    if (ierr.eq.0) then
       call set_drivers(nself, drvs, irank, nrank, ktest)
    endif
    if (ierr.eq.0) then
       call new_agent_family(ierr, drvs(1:nself))
    endif
    if (ierr.eq.0) then
       call new_agent_derived(ierr, 'W', (/'A', 'X'/))
    endif
    if (ierr.eq.0) then
       call new_agent_derived(ierr, 'H', (/'@'/))
    endif
    return
  end subroutine test_ppp_agent

  subroutine set_drivers(nself, drvs, irank, nrank, ktest)
    implicit none
    integer,         intent(out) :: nself
    character(len=*),intent(out) :: drvs(:)
    integer,         intent(in)  :: irank, nrank
    integer,         intent(in)  :: ktest
    integer j
    integer ispl

    drvs(:) = ' '
    if (ktest.eq.1) then
       ! mpmd emulates
       ispl = max(0, nrank / 3 * 2)
       if (irank.lt.ispl) then
          drvs(1:1) = (/'A'/)
       else
          drvs(1:1) = (/'O'/)
       endif
    else
       ! default test
       if (irank.eq.0) then
          drvs(1:2) = (/'A', 'B'/)
       else if (irank.eq.1) then
          drvs(1:2) = (/'C', 'D'/)
       else if (irank.eq.2) then
          drvs(1) = 'A'
       else if (irank.eq.3) then
          drvs(1) = 'B'
       else if (irank.eq.4) then
          drvs(1:2) = (/'A', 'C'/)
       else if (irank.eq.5) then
          drvs(1:1) = (/'X'/)
       else if (irank.eq.6) then
          drvs(1:4) = (/'X', 'Y', 'Z', 'P'/)
       else if (irank.eq.7) then
          drvs(1:2) = (/'X', 'A'/)
       endif
    endif
    nself = 0
    do j = 1, size(drvs)
       if (drvs(j).eq.' ') exit
       nself = nself + 1
    enddo
    if (nself.eq.0) then
       nself = nself + 1
       drvs(1:nself) = (/'Q'/)
    endif

101 format('drivers: ', I0, 1x, I0, 1x, A)
    do j = 1, nself
       write(*, 101) irank, j, trim(drvs(j))
    enddo
  end subroutine set_drivers

end program test_ppp_comm
#endif /* TEST_PPP_COMM */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

!!!_! ppp_amng.F90 - TOUZA/ppp agent manager (xmcomm core replacement)
! Maintainer: SAITO Fuyuki
! Created: Jan 25 2022
#define TIME_STAMP 'Time-stamp: <2022/05/23 10:28:06 c0210 ppp_amng.F90>'
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
#ifndef   TEST_PPP_AMNG
#  define TEST_PPP_AMNG 0
#endif
#ifndef   DEBUG_COMM
#  define DEBUG_COMM TEST_PPP_AMNG
#endif
#ifndef   OPT_AGENT_LEN
#  define OPT_AGENT_LEN 8
#endif
#ifndef   OPT_AGENTS_MAX
#  define OPT_AGENTS_MAX 128
#endif
#ifndef   OPT_AGENTS_STACK
#  define OPT_AGENTS_STACK 16
#endif
!!!_@ TOUZA_Ppp_amng - MPI communicator control
module TOUZA_Ppp_amng
!!!_ + modules
  use MPI,only: MPI_COMM_NULL, MPI_GROUP_NULL
  use TOUZA_Ppp_std,only: &
       & control_mode, control_deep, is_first_force, &
       & get_logu,     unit_global,  trace_fine,   trace_control
!!!_ + default
  implicit none
  private
!!!_ + parameter
  integer,parameter,public :: lagent = OPT_AGENT_LEN
  integer,parameter,public :: latbl  = OPT_AGENTS_MAX
!!!_  - operations
  integer,parameter,public :: OPR_PLAIN   = 0  ! as it is
  integer,parameter,public :: OPR_REVERSE = 1  ! reverse rank order
  integer,parameter,public :: OPR_DSHIFT  = 2  ! shift (decrement)
  integer,parameter,public :: OPR_FLOAT   = 3  ! float specified ranks
!!!_ + public
!!!_  - reserved agent string
  character(len=*),parameter,public :: asp_world = '*'     ! mpi_comm_world
  character(len=*),parameter,public :: asp_root  = '/'     ! alias of root agent
  character(len=*),parameter,public :: asp_base  = '%'     ! alias of source agent
  character(len=*),parameter,public :: asp_unit  = '@'     ! least mutual exclusive combination
  character(len=*),parameter,public :: asp_pfxa  = '#'     ! prefix for auto-name
  character(len=*),parameter,public :: asp_top   = ' '     ! top stack
!!!_ + flags
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

  integer,parameter :: map_div = 80
  integer,parameter :: map_mod = 8

  character,parameter :: map_mem_s = 'X'
  character,parameter :: map_mem_o = 'o'
  character,parameter :: map_src_s = '-'
  character,parameter :: map_src_o = '_'
  character,parameter :: map_other = '.'
  character,parameter :: map_rule  = '|'

!!!_ + type
  type aprop_t
     character(len=lagent) :: name = ' '            ! agent string (aka communicator indicator)
     integer               :: comm = MPI_COMM_NULL  ! communicator
     integer               :: mgrp = MPI_GROUP_NULL ! mpi group
     integer               :: isrc = 0              ! source agent index
     integer               :: kflg = flag_none      ! flag
  end type aprop_t
  type(aprop_t),save :: atblp(-1:latbl)
!!!_ + static
!!!_  - agent table
  integer,save :: matbl = 0
  integer,save :: aworld = -1
!!!_  - agent stack
  integer,save :: jstack = -1
  integer,save :: lstack = 0
  integer,allocatable,save :: astack(:)
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = PPP_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
#define __MDL__ 'a'
!!!_ + overload
  interface switch_agent
     module procedure switch_agent_ai
  end interface switch_agent
  interface push_agent
     module procedure push_agent_ai
  end interface push_agent
  interface pop_agent
     module procedure pop_agent_ai
  end interface pop_agent
  interface top_agent
     module procedure top_agent_ai
  end interface top_agent
  interface inquire_agent
     module procedure inquire_agent_a
     module procedure inquire_agent_i
  end interface inquire_agent
  interface agents_translate
     module procedure agents_translate_a
     module procedure agents_translate_i
  end interface agents_translate
  interface base_agent
     module procedure base_agent_i
  end interface base_agent
!!!_ + interfaces
  public init, diag, finalize
  public new_agent_root
  public new_agent_color, new_agent_family
  public new_agent_derived
  public mod_agent_order
  public agents_translate
  public switch_agent, push_agent, pop_agent, top_agent
  public inquire_agent
  public query_agent,  source_agent, check_agent, base_agent
  public diag_maps_batch
!!!_ + common interfaces
contains
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm, nstack)
    use TOUZA_Ppp_std,only: choice, ps_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    integer,intent(in),optional :: icomm
    integer,intent(in),optional :: nstack
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
          if (ierr.eq.0) call init_stack(ierr, nstack)
          if (ierr.eq.0) call init_world(ierr, u=ulog)
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
             if (is_msglev_normal(lv)) call diag_batch(ierr, utmp)
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

!!!_ + init subcontracts
!!!_  - init_world - add special agent (world)
  subroutine init_world &
       & (ierr, u)
    use MPI,only: &
         & MPI_COMM_WORLD, MPI_Comm_rank, &
         & MPI_Group_rank, MPI_Group_size
    use TOUZA_Ppp_std,only: get_wni_safe, get_ni, get_gni, msg
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    ierr = 0
    call new_agent_root(ierr, MPI_COMM_WORLD, asp_world)
    if (ierr.eq.0) aworld = check_agent()
    return
  end subroutine init_world
!!!_  - init_stack - stack initialization
  subroutine init_stack &
       & (ierr, n)
    use TOUZA_Ppp_std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: n
    integer m

    ierr = 0
    m = choice(0, n)
    if (m.le.0) m = max(1, OPT_AGENTS_STACK)
    if (m.gt.lstack) then
       if (lstack.gt.0) deallocate(astack, STAT=ierr)
       if (ierr.eq.0) allocate(astack(0:m-1), STAT=ierr)
       if (ierr.eq.0) then
          astack(jstack+1:m-1) = -1
          lstack = m
          jstack = max(0, jstack)
       endif
    endif
    return
  end subroutine init_stack
!!!_ + diag subcontracts
!!!_  - diag_batch
  subroutine diag_batch &
       & (ierr, u)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    ierr = 0
    if (ierr.eq.0) call diag_table(ierr, u)

    ! if (ierr.eq.0) call diag_maps(ierr, aworld, u)
    if (ierr.eq.0) call diag_maps_batch(ierr, aworld, u)
    if (ierr.eq.0) call diag_stack (ierr, u)

  end subroutine diag_batch
!!!_  - diag_table
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
       do jt = 0, matbl - 1
          ico = atblp(jt)%comm
          call get_ni(ierr, nrc, irc, ico)
          igr = atblp(jt)%mgrp
          call get_gni(ierr, igr, nrg, irg)
          if (astack(jstack).eq.jt) then
             TM = '+'
          else
             TM = ' '
          endif
          write(txt, 101) irw, jt, &
               & trim(atblp(jt)%name), &
               & irg, nrg, irc, nrg, &
               & atblp(jt)%isrc, trim(flagch(atblp(jt)%kflg) // TM)
          call msg(txt, __MDL__, utmp)
       enddo
    endif
  end subroutine diag_table
!!!_  - diag_stack
  subroutine diag_stack &
       & (ierr, u)
    use TOUZA_Ppp_std,only: msg, choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer js, jt
    character(len=256) :: txt
    ierr = 0
    do js = 0, jstack
       jt = astack(js)
101    format('stack:', I0, 2x, I0, 1x, A)
       write(txt, 101) js, jt, trim(atblp(jt)%name)
       call msg(txt, __MDL__, u)
    enddo
  end subroutine diag_stack
!!!_  - diag_maps_batch
  subroutine diag_maps_batch &
       & (ierr, iaref, u)
    use TOUZA_Std,only: choice, ndigits
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: iaref
    integer,intent(in),optional :: u
    integer utmp
    integer ja, jas
    integer ref
    integer nrref, irref
    character(len=64) :: fmt_rr
    integer nd

    ierr = 0
    utmp = get_logu(u, ulog)
    ref = choice(aworld, iaref)

    call inquire_agent(ierr, iagent=ref, nrank=nrref, irank=irref)

    if (ierr.eq.0) then
       nd = ndigits(nrref)
101    format('(I', I0, '.', I0, ', ''-'', I', I0, '.', I0, ')')
       write(fmt_rr, 101) nd, nd, nd, nd
    endif
    do ja = 0, matbl - 1
       if (atblp(ja)%isrc.lt.0) then
          if (ierr.eq.0) call diag_map_root(ierr, ja, ref, nrref, irref, utmp, fmt_rr)
       else if (atblp(ja)%kflg.eq.flag_base) then
          jas = source_agent(ja)
          if (ierr.eq.0) call diag_map_family(ierr, jas, ref, nrref, irref, utmp, fmt_rr)
       endif
    enddo
  end subroutine diag_maps_batch

!!!_  - diag_map_root
  subroutine diag_map_root &
       & (ierr, iagent, iaref, nrref, irref, u, fmt_rr)
    use TOUZA_Ppp_std,only: msg
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: iagent
    integer,         intent(in)          :: iaref
    integer,         intent(in)          :: nrref, irref
    integer,         intent(in),optional :: u
    character(len=*),intent(in),optional :: fmt_rr
    integer utmp

    integer,parameter :: lmap = map_div + map_div / map_mod + 3
    character(len=lmap) :: map

    character(len=32)  :: rr
    character(len=128) :: txt
    character(len=32)  :: ttgt, tref

    integer jrbgn, jrend, mm
    integer nrtgt, irtgt

    ierr = 0
    utmp = get_logu(u, ulog)

    call inquire_agent(ierr, iagent=iagent, nrank=nrtgt, irank=irtgt)
102 format('[', I0, ':', A, 1x, I0, '/', I0, ']')
    write(ttgt, 102) iagent, trim(atblp(iagent)%name), irtgt, nrtgt
    write(tref, 102) iaref,  trim(atblp(iaref)%name),  irref, nrref

    do jrbgn = 0, nrref - 1, map_div
       jrend = min(nrref, jrbgn + map_div) - 1
       mm = jrend - jrbgn + 1
       if (present(fmt_rr)) then
          write(rr, fmt_rr) jrbgn, jrend
       else
101       format(I0, '-', I0)
          write(rr, 101) jrbgn, jrend
       endif
       if (ierr.eq.0) then
          call diag_map_string &
               & (ierr, map, iagent, iaref, jrbgn, jrend, map_mod, map_rule)
111       format('[', A, '] ', &
               &  A, 1x, A, 1x, A)
          write(txt, 111) trim(rr), trim(map), trim(ttgt), trim(tref)
          call msg(txt, __MDL__, utmp)
       endif
    enddo
  end subroutine diag_map_root

!!!_  - diag_map_family
  subroutine diag_map_family &
       & (ierr, iasrc, iaref, nrref, irref, u, fmt_rr)
    use MPI,only: MPI_UNDEFINED
    use TOUZA_Ppp_std,only: msg
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: iasrc
    integer,         intent(in)          :: iaref, nrref, irref
    integer,         intent(in),optional :: u
    character(len=*),intent(in),optional :: fmt_rr
    integer utmp
    integer ja

    integer,parameter :: lmap = map_div + map_div / map_mod + 3
    character(len=lmap) :: map

    character(len=32)  :: rr
    character(len=128) :: txt
    character(len=32)  :: ttgt

    integer jrbgn, jrend, mm
    integer nrtgt, irtgt

    ierr = 0
    utmp = get_logu(u, ulog)

    if (ierr.eq.0) call diag_map_root(ierr, iasrc, iaref, nrref, irref, utmp, fmt_rr)

    do ja = 0, matbl - 1
       if (atblp(ja)%isrc.eq.iasrc) then

          ierr = 0
          utmp = get_logu(u, ulog)

          call inquire_agent(ierr, iagent=ja, nrank=nrtgt, irank=irtgt)
102       format('[', I0, ':', A, 1x, I0, '/', I0, ']')
103       format('[', I0, ':', A, 1x, '-', '/', I0, ']')
          if (irtgt.eq.MPI_UNDEFINED) then
             write(ttgt, 103) ja, trim(atblp(ja)%name), nrtgt
          else
             write(ttgt, 102) ja, trim(atblp(ja)%name), irtgt, nrtgt
          endif
          do jrbgn = 0, nrref - 1, map_div
             jrend = min(nrref, jrbgn + map_div) - 1
             mm = jrend - jrbgn + 1
             if (present(fmt_rr)) then
                write(rr, fmt_rr) jrbgn, jrend
             else
101             format(I0, '-', I0)
                write(rr, 101) jrbgn, jrend
             endif
             if (ierr.eq.0) then
                call diag_map_string &
                     & (ierr, map, ja, iaref, jrbgn, jrend, map_mod, map_rule)
111             format('[', A, '] ', &
                     &  A, 1x, A)
                write(txt, 111) trim(rr), trim(map), trim(ttgt)
                call msg(txt, __MDL__, utmp)
             endif
          enddo
       endif
    enddo

  end subroutine diag_map_family

!!!_  - diag_map_string
  subroutine diag_map_string &
       & (ierr, map, iagent, iref, irbgn, irend, md, sep)
    use TOUZA_Ppp_std,only: get_gni, choice, choice_a
#  if HAVE_FORTRAN_MPI_MPI_GROUP_TRANSLATE_RANKS
    use MPI,only: MPI_Group_translate_ranks
#  endif
    use MPI,only: MPI_Group_rank, MPI_GROUP_NULL
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(out)         :: map
    integer,         intent(in)          :: iagent
    integer,         intent(in)          :: iref
    integer,         intent(in)          :: irbgn, irend
    integer,         intent(in),optional :: md
    character(len=*),intent(in),optional :: sep

    integer gtgt, gref, gsrc
    integer rtgt(irbgn:irend)
    integer rref(irbgn:irend)
    integer rsrc(irbgn:irend)
    integer j, n, jp
    integer isrc
    integer ir

    integer mi, ls
    character(len=8) :: si

    ierr = 0

    mi = choice(0, md)
    call choice_a(si, ' ', sep)
    ls = max(1, len_trim(si))

    gtgt = atblp(iagent)%mgrp

    isrc = source_agent(iagent)

    if (gtgt.eq.MPI_GROUP_NULL) ierr = -1
    if (ierr.eq.0) gref = atblp(iref)%mgrp
    if (ierr.eq.0) gsrc = atblp(isrc)%mgrp
    if (ierr.eq.0) call get_gni(ierr, gref, irank=ir)

    if (ierr.eq.0) then
       do j = irbgn, irend
          rref(j) = j
       enddo
       n = irend - irbgn + 1
    endif
    if (ierr.eq.0) then
       if (gtgt.eq.MPI_GROUP_NULL) then
          rtgt(irbgn:irend) = -1
       else
          call MPI_Group_translate_ranks(gref, n, rref, gtgt, rtgt, ierr)
       endif
    endif
    if (ierr.eq.0) then
       if (gsrc.eq.MPI_GROUP_NULL) then
          rsrc(irbgn:irend) = -1
       else
          call MPI_Group_translate_ranks(gref, n, rref, gsrc, rsrc, ierr)
       endif
    endif
    if (ierr.eq.0) then
       map = ' '
       jp = 0
       do j = irbgn, irend
          jp = jp + 1
          if (rtgt(j).ge.0) then
             if (j.eq.ir) then
                map(jp:jp) = map_mem_s
             else
                map(jp:jp) = map_mem_o
             endif
          else if (rsrc(j).ge.0) then
             if (j.eq.ir) then
                map(jp:jp) = map_src_s
             else
                map(jp:jp) = map_src_o
             endif
          else
             map(jp:jp) = map_other
          endif
          if (mi.gt.0.and.j.lt.irend) then
             if (mod((j - irbgn+1), mi).eq.0) then
                map(jp+1:jp+ls) = si(1:ls)
                jp = jp + ls
             endif
          endif
       enddo
    endif

  end subroutine diag_map_string
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
    if (name.eq.asp_world) then
       n = aworld
    else if (name.eq.asp_top) then
       jt = astack(jstack)
       if (atblp(jt)%isrc.eq.jas) n = jt
    else
       do jt = 0, matbl - 1
          ! search agents with the same source
          if (atblp(jt)%isrc.eq.jas) then
             if (atblp(jt)%name.eq.name) then
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
       &  irank, nrank, icomm, igroup, name, ismem)
    implicit none
    integer,         intent(out)          :: ierr
    character(len=*),intent(in)           :: agent
    integer,         intent(out),optional :: irank
    integer,         intent(out),optional :: nrank
    integer,         intent(out),optional :: icomm
    integer,         intent(out),optional :: igroup
    character(len=*),intent(out),optional :: name
    logical,         intent(out),optional :: ismem
    integer ja
    ja = query_agent(agent)
    call inquire_agent_i(ierr, ja, irank, nrank, icomm, igroup, name, ismem)
    return
  end subroutine inquire_agent_a

  subroutine inquire_agent_i &
       & (ierr,   &
       &  iagent, &
       &  irank,  nrank, icomm, igroup, name, ismem)
    implicit none
    integer,         intent(out)          :: ierr
    integer,         intent(in), optional :: iagent
    integer,         intent(out),optional :: irank
    integer,         intent(out),optional :: nrank
    integer,         intent(out),optional :: icomm
    integer,         intent(out),optional :: igroup
    character(len=*),intent(out),optional :: name
    logical,         intent(out),optional :: ismem
    integer ja
    integer ic, ig, ir

    ierr = 0
    ja = check_agent(iagent)
    ic = MPI_COMM_NULL
    if (ja.lt.0) ierr = -1

    if (ja.ge.0) ic = atblp(ja)%comm
    if (ja.ge.0) ig = atblp(ja)%mgrp

    if (present(irank)) then
       if (ja.ge.0) then
          call MPI_Group_rank(ig, irank, ierr)
       else
          irank = -1
       endif
    endif
    if (present(ismem)) then
       if (ja.ge.0) then
          call MPI_Group_rank(ig, ir, ierr)
       else
          ir = -1
       endif
       ismem = ir.ge.0
    endif
    if (present(nrank)) then
       if (ja.ge.0) then
          call MPI_Group_size(ig, nrank, ierr)
       else
          nrank = -1
       endif
    endif
    if (present(icomm)) then
       if (ja.ge.0) then
          icomm = ic
       else
          icomm = MPI_COMM_NULL
       endif
    endif
    if (present(igroup)) then
       if (ja.ge.0) then
          igroup = ig
       else
          igroup = MPI_GROUP_NULL
       endif
    endif
    if (present(name)) then
       if (ja.ge.0) then
          name = atblp(ja)%name
       else
          name = ' '
       endif
    endif
  end subroutine inquire_agent_i
!!!_  & base_agent() - query base (i.e., alias of source) agent
  integer function base_agent_i (iagent) result(n)
    implicit none
    integer,intent(in),optional :: iagent
    integer jatmp
    integer jas
    jas = source_agent(iagent)
    if (jas.lt.0) then
       n = -1
    else
       do jatmp = 1, matbl
          ! usually jas + 1 is the alias
          n = mod(jas + jatmp, matbl)
          if (atblp(n)%isrc.eq.jas) then
             if (atblp(n)%kflg.eq.flag_base) return
          endif
       enddo
       n = -1
    endif
  end function base_agent_i
!!!_  & switch_agent
  subroutine switch_agent_ai (ierr, iagent)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: iagent
    ierr = 0
    if (iagent.lt.0.or.iagent.ge.matbl) ierr = -1
    if (jstack.lt.0.or.jstack.ge.lstack) ierr = -1
    if (ierr.eq.0) astack(jstack) = iagent
  end subroutine switch_agent_ai
!!!_  & push_agent
  subroutine push_agent_ai (ierr, iagent)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: iagent
    ierr = 0
    jstack = jstack + 1
    if (jstack.ge.lstack) ierr = -1
    if (ierr.eq.0) then
       call switch_agent(ierr, iagent)
    endif
    return
  end subroutine push_agent_ai
!!!_  & pop_agent
  subroutine pop_agent_ai (ierr, iagent)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: iagent
    ierr = 0
    if (present(iagent)) then
       if (astack(jstack).ne.iagent) ierr = -1
    endif
    if (ierr.eq.0) then
       jstack = jstack - 1
       if (jstack.lt.0) ierr = -1
    endif
    return
  end subroutine pop_agent_ai
!!!_  & top_agent
  subroutine top_agent_ai (ierr, iagent)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: iagent
    ierr = 0
    iagent = astack(jstack)
    return
  end subroutine top_agent_ai
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
    character(len=lagent) :: ctmp

    ierr = 0
    if (present(icomm)) then
       icsrc = icomm
    else
       call get_comm(ierr, icsrc)
    endif
    if (ierr.eq.0) then
       if (present(name)) then
          ctmp = name
       else
101       format(A, I0)
          write(ctmp, 101) asp_pfxa, matbl
       endif
    endif
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
    integer icsrc, icunit, icroot
    integer irank, nrank

    ! no group control except for local color
    jas = check_agent(src)
    if (jas.lt.0) ierr = -1

    if (ierr.eq.0) then
       icsrc = atblp(jas)%comm
       call get_ni(ierr, nrank, irank, icsrc)
    endif
    if (ierr.eq.0) then
       call MPI_Comm_split(icsrc, color, irank, icunit, ierr)
    endif
    if (ierr.eq.0) then
       ! jau is dummy
       ! clone base agent from source
       call add_entry_comm(ierr, jau, asp_base, src=jas, flag=flag_base)
    endif
    if (ierr.eq.0) then
       ! jau is dummy
       ! clone root agent
       jau = root_agent(jas)
       if (jau.ge.0) then
          icroot = atblp(jau)%comm
          call add_entry_comm(ierr, jau, asp_root, icroot, src=jas, flag=flag_root)
       endif
    endif
    if (ierr.eq.0) then
       ! add unit agent
       call add_entry_comm(ierr, jau, asp_unit, icunit, src=jas, flag=flag_unit)
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
       & (ierr, affils, src)
    use TOUZA_Ppp_std,only: get_ni, msg, is_msglev_debug
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: affils(:)  ! (affiliaion) array of agents to belong to
    integer,optional,intent(in)  :: src

    integer icsrc, icunit, icroot
    integer igsrc
    integer jas,   jau
    ! integer jt
    integer nt
    integer nrsrc, irsrc
    integer,parameter :: lttbl = latbl
    character(len=lagent) :: tci(lttbl)
    integer               :: tui(lttbl)
    integer               :: tgr(lttbl)
    integer,allocatable   :: rgw(:)
    ierr = 0

    jas = check_agent(src)
    if (jas.lt.0) ierr = -1

    if (ierr.eq.0) then
       icsrc = atblp(jas)%comm
       igsrc = atblp(jas)%mgrp
       call merge_comm_table(ierr, nt, tci, tui, affils(:), icsrc)
       ! if (is_msglev_debug(lev_verbose)) then
       !    do jt = 1, nt
       !       call msg(tci(jt), __MDL__, ulog)
       !       call msg('(I0)', tui(jt), __MDL__, ulog)
       !    enddo
       ! endif
    endif

    if (ierr.eq.0) call get_ni(ierr, nrsrc, irsrc, icsrc)
    if (ierr.eq.0) allocate(rgw(0:nrsrc-1), STAT=ierr)
    if (ierr.eq.0) then
       call batch_group_split(ierr, rgw, tgr, tci, nt, affils(:), icsrc, igsrc, irsrc)
    endif
    if (ierr.eq.0) deallocate(rgw, STAT=ierr)

    if (ierr.eq.0) then
       call gen_comm_unit(ierr, icunit, tci, tui, nt, affils(:), icsrc)
    endif
    if (ierr.eq.0) then
       ! jau is dummy
       ! clone base agent from source
       call add_entry_comm(ierr, jau, asp_base, src=jas, flag=flag_base)
    endif
    if (ierr.eq.0) then
       ! jau is dummy
       ! clone root agent
       jau = root_agent(jas)
       if (jau.ge.0) then
          icroot = atblp(jau)%comm
          call add_entry_comm(ierr, jau, asp_root, icroot, src=jas, flag=flag_root)
       endif
    endif
    if (ierr.eq.0) then
       ! add unit agent
       call add_entry_comm(ierr, jau, asp_unit, icunit, src=jas, flag=flag_unit)
    endif
    if (ierr.eq.0) then
       call add_agent_family(ierr, tgr, tci, tui, nt, affils(:), jas)
    endif
    if (ierr.eq.0) call switch_agent(ierr, jau)
  end subroutine new_agent_family

!!!_  & new_agent_derived
  subroutine new_agent_derived &
       & (ierr, name, alist, iagent)
    use MPI,only: MPI_UNDEFINED
    use TOUZA_Ppp_std,only: msg
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: name      ! new name
    character(len=*),intent(in)  :: alist(:)  ! (affiliation) array of agents to unite
    integer,optional,intent(in)  :: iagent    ! current agent-id if null

    integer jas, jad
    integer igdrv
    integer flg
    integer ird

    ierr = 0

    jas = source_agent(iagent)
    if (jas.lt.0) ierr = -1
    if (ierr.eq.0) then
       jad = query_agent(name, iagent)
       if (jad.ge.0) then
          call msg('(''Already registerd: '', A)', (/trim(name)/), __MDL__, ulog)
          ierr = -1
          return
       endif
    endif

    if (ierr.eq.0) then
       call derive_group &
            & (ierr,  igdrv, &
            &  atblp, matbl, jas, alist)
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

!!!_ + other utilities
!!!_  & mod_agent_order - reorder rank in agent/group
  subroutine mod_agent_order &
       & (ierr, atgt, opr, keys, iagent)
    use MPI,only: MPI_UNDEFINED
    use TOUZA_Ppp_std,only: msg
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: atgt      ! target agent
    integer,         intent(in)  :: opr       ! operation flag
    integer,optional,intent(in)  :: keys(:)   !
    integer,optional,intent(in)  :: iagent

    integer ja
    integer jr, nrank, jshft
    integer,allocatable :: ranks(:)
    integer jk
    integer igtgt, ignew, icnew, icsrc
    logical b

    ierr = 0
    b = .FALSE.

    ja = query_agent(atgt, iagent)
    if (ja.lt.0) ierr = -1
    if (ierr.eq.0) igtgt = atblp(ja)%mgrp
    if (ierr.eq.0) call inquire_agent(ierr, iagent=ja, nrank=nrank)
    if (ierr.eq.0) allocate(ranks(0:nrank-1), STAT=ierr)
    if (ierr.eq.0) then
       b = .TRUE.
       select case(opr)
       case (OPR_REVERSE)
          do jr = 0, nrank - 1
             ranks(nrank-1-jr) = jr
          enddo
       case (OPR_DSHIFT)
          if (present(keys)) then
             jshft = keys(1)
          else
             jshft = 1
          endif
          do jr = 0, nrank - 1
             ranks(jr) = mod(nrank + jshft + jr, nrank)
          enddo
       case (OPR_FLOAT)
          jr = 0
          do jk = 1, size(keys)
             if (ANY(keys(jk).eq.ranks(0:jr-1))) then
                continue
             else if (keys(jk).ge.0 .and. keys(jk).lt.nrank) then
                ranks(jr) = keys(jk)
                jr = jr + 1
             endif
          enddo
          do jk = 0, nrank - 1
             if (ANY(jk.eq.ranks(0:jr-1))) then
                continue
             else
                ranks(jr) = jk
                jr = jr + 1
             endif
          enddo
       case default
          b = .FALSE.
       end select
    endif
    if (b) then
       if (ierr.eq.0) call MPI_Group_incl(igtgt, nrank, ranks(0:nrank-1), ignew, ierr)
       if (ierr.eq.0) then
          icsrc = atblp(ja)%comm
          if (icsrc.eq.MPI_COMM_NULL) then
             icnew = icsrc
          else
             call MPI_Comm_create(icsrc, ignew, icnew, ierr)
          endif
       endif
       if (ierr.eq.0) then
          atblp(ja)%comm = icnew
          atblp(ja)%mgrp = ignew
       endif
    endif
    if (ierr.eq.0) deallocate(ranks, STAT=ierr)

  end subroutine mod_agent_order
!!!_  & agents_translate - translate ranks between agents
  subroutine agents_translate_a &
       & (ierr, irtgt, atgt, irsrc, asrc)
    use MPI,only: MPI_UNDEFINED
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: irtgt
    character(len=*),intent(in)          :: atgt    ! target agent
    integer,         intent(in),optional :: irsrc   ! source rank  (self if not present)
    character(len=*),intent(in),optional :: asrc    ! source agent (current if not present)

    integer iatgt, iasrc
    ierr = 0
    iatgt = query_agent(atgt)
    if (present(asrc)) then
       iasrc = query_agent(asrc)
       call agents_translate_i(ierr, irtgt, iatgt, irsrc, iasrc)
    else
       call agents_translate_i(ierr, irtgt, iatgt, irsrc)
    endif
    return
  end subroutine agents_translate_a

  subroutine agents_translate_i &
       & (ierr, irtgt, iatgt, irsrc, iasrc)
    use MPI,only: MPI_UNDEFINED
#  if HAVE_FORTRAN_MPI_MPI_GROUP_TRANSLATE_RANKS
    use MPI,only: MPI_Group_translate_ranks
#  endif
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: irtgt
    integer,intent(in),optional :: iatgt   ! target agent (current if not present)
    integer,intent(in),optional :: irsrc   ! source rank  (self if not present)
    integer,intent(in),optional :: iasrc   ! source agent (current if not present)

    integer jat, jas
    integer igt, igs
    integer irs(1), irt(1)
    integer nrsrc

    ierr = 0
    irtgt = MPI_UNDEFINED

    jas = check_agent(iasrc)
    jat = check_agent(iatgt)

    if (jas.lt.0) ierr = -1
    if (jat.lt.0) ierr = -1
    if (ierr.eq.0) then
       if (present(irsrc)) then
          call inquire_agent(ierr, iagent=jas, nrank=nrsrc)
          if (irsrc.lt.nrsrc) then
             irs(1) = irsrc
          else
             ierr = -1
          endif
       else
          call inquire_agent(ierr, iagent=jas, irank=irs(1))
          if (irs(1).lt.0) ierr = -1
       endif
    endif
    if (ierr.eq.0) call inquire_agent(ierr, iagent=jas, igroup=igs)
    if (ierr.eq.0) call inquire_agent(ierr, iagent=jat, igroup=igt)
    if (ierr.eq.0) then
       call MPI_Group_translate_ranks(igs, 1, irs(:), igt, irt(:), ierr)
    endif
    if (ierr.eq.0) irtgt = irt(1)
  end subroutine agents_translate_i
!!!_ + internal procedures
!!!_  & check_agent
  integer function check_agent &
       & (iagent) &
       & result(n)
    use TOUZA_Ppp_std,only: choice
    implicit none
    integer,intent(in),optional :: iagent
    n = choice(astack(jstack), iagent)
    if (n.lt.0.or.n.ge.matbl) then
       n = -1
    else if (atblp(n)%kflg.le.flag_none) then
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
    n = atblp(n)%isrc
  end function source_agent

!!!_  & root_agent
  integer function root_agent &
       & (iagent) &
       & result(n)
    use TOUZA_Ppp_std,only: choice
    implicit none
    integer,intent(in),optional :: iagent
    n = check_agent(iagent)
    do
       if (n.lt.0) return
       if (atblp(n)%kflg.eq.flag_root) return
       n = atblp(n)%isrc
    enddo
    return
  end function root_agent

!!!_  & add_agent_family
  subroutine add_agent_family &
       & (ierr, &
       &  tgr,  &
       &  tci,  tui,  nt, affils, src)
    use MPI,only: MPI_Comm_create
    use TOUZA_Ppp_std,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: tgr(:)
    character(len=*),intent(in)  :: tci(:)
    integer,         intent(in)  :: tui(:)
    character(len=*),intent(in)  :: affils(:)
    integer,         intent(in)  :: nt
    integer,         intent(in)  :: src

    integer ja, je, jt
    integer icsrc, icnew
    integer uself
    ierr = 0

    ja = matbl
    matbl = matbl + nt
    if (matbl.gt.latbl) then
       ierr = -1
       return
    endif

    je = ja + nt - 1

    atblp(ja:je)%name = tci(1:nt)
    atblp(ja:je)%mgrp = tgr(1:nt)
    atblp(ja:je)%isrc = src
    icsrc = atblp(src)%comm

    do jt = 1, nt
       if (ierr.eq.0) then
          call MPI_Comm_create(icsrc, tgr(jt), icnew, ierr)
       endif
       if (ierr.eq.0) atblp(ja + jt - 1)%comm = icnew
    enddo
    ! find self unit id
    uself = -1
    do jt = 1, nt
       if (tci(jt).eq.affils(1)) then
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
       if (ANY(tci(jt).eq.affils(:))) then
          atblp(je)%kflg = flag_self
       else if (tui(jt).eq.uself) then
          atblp(je)%kflg = flag_friend
       else
          atblp(je)%kflg = flag_other
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
    iagent = matbl
    matbl = matbl + 1
    if (matbl.gt.latbl) then
       ierr = -1
       return
    endif

    atblp(iagent)%name = name
    atblp(iagent)%kflg = choice(flag_root, flag)
    if (present(icomm)) then
       atblp(iagent)%comm = icomm
       atblp(iagent)%isrc = choice(-1, src)
       call MPI_Comm_group(icomm, jgrp, ierr)
       if (ierr.eq.0) then
          atblp(iagent)%mgrp = jgrp
       else
          atblp(iagent)%mgrp = MPI_GROUP_NULL
          ierr = -1
       endif
    else if (present(src)) then
       atblp(iagent)%isrc = choice(-1, src)
       if (src.lt.0.or.src.ge.matbl) then
          ierr = -1
       else
          atblp(iagent)%comm = atblp(src)%comm
          atblp(iagent)%mgrp = atblp(src)%mgrp
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
    iagent = matbl
    matbl = matbl + 1
    if (matbl.gt.latbl) then
       ierr = -1
       return
    endif

    atblp(iagent)%name = name
    atblp(iagent)%kflg = flag
    atblp(iagent)%isrc = src
    atblp(iagent)%mgrp = igroup
    icsrc = atblp(src)%comm

    call MPI_Comm_create(icsrc, igroup, icnew, ierr)
    if (ierr.eq.0) atblp(iagent)%comm = icnew

    return
  end subroutine add_entry_group

!!!_  & merge_comm_table - merge agent table
  subroutine merge_comm_table &
       & (ierr, &
       &  ntotal, tbl_ci, tbl_ui, &
       &  affils, icomm)
    use MPI,only: &
         & MPI_STATUS_SIZE, MPI_CHARACTER, MPI_INTEGER
#  if HAVE_FORTRAN_MPI_MPI_BCAST
    use MPI,only: MPI_Send, MPI_Bcast
#  endif
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ntotal     ! total number of agents
    character(len=*),intent(out) :: tbl_ci(*)  ! agent table merged
    integer,         intent(out) :: tbl_ui(*)  ! agent unit-id table
    character(len=*),intent(in)  :: affils(:)  ! (affiliaion) array of agents to belong to
    integer,         intent(in)  :: icomm      ! mpi communicator

    integer nrank, irank, iroot
    integer,parameter :: lbuf = OPT_AGENTS_MAX
    integer,parameter :: ndiv = 3

    character(len=lagent) :: dbuf(lbuf)
    integer               :: ubuf(lbuf)
    integer mstp,  ktag
    integer ipsrc, ipdst
    integer msend
    integer j

    ierr = 0
    iroot = 0

    call get_cnr(ierr, icomm, irank, nrank)

    if (ierr.eq.0) then
       ntotal = 0
       do j = 1, size(affils)
          if (affils(j).ne.' ') then
             ntotal = ntotal + 1
             dbuf(ntotal) = affils(j)
             ubuf(ntotal) = 0
          endif
       enddo
       ! write(*, *) 'affils', ntotal, dbuf(1:ntotal)
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
                   call recv_affils &
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
                msend = ntotal * lagent
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
                      call recv_affils &
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
       msend = ntotal * lagent
       call MPI_Bcast(dbuf(1:ntotal), msend, MPI_CHARACTER, iroot, icomm, ierr)
    endif
    if (ierr.eq.0) then
       tbl_ci(1:ntotal) = dbuf(1:ntotal)
       tbl_ui(1:ntotal) = ubuf(1:ntotal)
    endif
    return
  end subroutine merge_comm_table
!!!_  & recv_affils
  subroutine recv_affils &
       & (ierr,  ntotal, dbuf,  ubuf, &
       &  icomm, ipsrc,  ktag)
    use MPI,only: &
         & MPI_STATUS_SIZE, MPI_CHARACTER, MPI_INTEGER, &
         & MPI_Probe, MPI_Get_count
#  if HAVE_FORTRAN_MPI_MPI_BCAST
    use MPI,only: MPI_Recv
#  endif
    implicit none
    integer,              intent(out)   :: ierr
    integer,              intent(inout) :: ntotal
    character(len=lagent),intent(inout) :: dbuf(:)
    integer,              intent(inout) :: ubuf(:)
    integer,              intent(in)    :: icomm
    integer,              intent(in)    :: ipsrc, ktag

    integer,parameter     :: lrcv = OPT_AGENTS_MAX
    character(len=lagent) :: drcv(lrcv)
    integer               :: urcv(lrcv)
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
            & (drcv(1:nrcv), (nrcv * lagent), MPI_CHARACTER, &
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
             ! new agent
             ntotal = ntotal + 1
             dbuf(ntotal) = drcv(j)
             ubuf(ntotal) = urcv(j)
          else
             ! existing agent
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
  end subroutine recv_affils

!!!_  & batch_group_split
  subroutine batch_group_split &
       & (ierr,   ranks,  tbl_gr, &
       &  tbl_ci, ntotal, affils, icomm, igsrc, ir)
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
    character(len=*),intent(in)  :: affils(:)
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
          if (ANY(affils(:).eq.tbl_ci(j))) then
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

!!!_  & gen_comm_unit
  subroutine gen_comm_unit &
       & (ierr,   icunit, &
       &  tbl_ci, tbl_ui, ntotal, affils, icomm)
    use MPI,only: &
         & MPI_UNDEFINED, MPI_COMM_NULL, &
         & MPI_Comm_split
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: icunit
    character(len=*),intent(in)  :: tbl_ci(*)
    integer,         intent(in)  :: tbl_ui(*)
    integer,         intent(in)  :: ntotal
    character(len=*),intent(in)  :: affils(*)
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
             ! only affils(1) is used
             if (tbl_ci(js).eq.affils(1)) then
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
       & (ierr, igdrv, &
       &  tbl,  nt,    src, alist)
    use MPI,only: &
         & MPI_GROUP_EMPTY, &
         & MPI_Group_union
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: igdrv
    type(aprop_t),   intent(in)  :: tbl(-1:*)
    integer,         intent(in)  :: nt
    character(len=*),intent(in)  :: alist(:)
    integer,         intent(in)  :: src

    integer jt
    integer igdst, igtmp

    ierr = 0
    igtmp = MPI_GROUP_EMPTY
    igdrv = igtmp
    do jt = 0, nt - 1
       if (tbl(jt)%isrc.ne.src) cycle
       if (ANY(alist(:).eq.tbl(jt)%name)) then
          if (ierr.eq.0) call MPI_Group_union(igtmp, tbl(jt)%mgrp, igdst, ierr)
          if (ierr.eq.0) igtmp = igdst
       endif
    enddo
    if (ierr.eq.0) igdrv = igtmp
  end subroutine derive_group

!!!_  & get_cnr
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

!!!_ + end TOUZA_Ppp_amng
end module TOUZA_Ppp_amng
!!!_@ test_ppp_amng - test program
#if TEST_PPP_AMNG
program test_ppp_amng
  use MPI
  use TOUZA_Ppp_amng
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
  call init(ierr, levv=+9)
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
    character(len=lagent) :: drvs(ltest)
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
    if (ierr.eq.0) then
       call new_agent_derived(ierr, 'H', (/'B', 'C'/))
       ierr = max(0, ierr)
    endif
    if (ierr.eq.0) then
       call mod_agent_order(ierr, 'Q', OPR_REVERSE)
    endif
    if (ierr.eq.0) then
       call test_translate(ierr, 'A', 'B')
       call test_translate(ierr, 'A', 'W')
       call test_translate(ierr, 'A', 'Q')
       call test_translate(ierr, 'A', '/')
       call test_translate(ierr, 'A', '%')
       call test_translate(ierr, 'B', '/')
       call test_translate(ierr, 'B', '%')
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
          drvs(1:3) = (/'A', ' ', 'B'/)
       else if (irank.eq.1) then
          drvs(1:3) = (/'C', 'D', 'C'/)
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
    nself = size(drvs)
    do j = size(drvs), 1, -1
       if (drvs(j).ne.' ') exit
       nself = nself - 1
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

  subroutine test_translate (ierr, asrc, atgt)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: asrc
    character(len=*),intent(in)  :: atgt
    integer ir, nrsrc
    integer irtgt
    ierr = 0

    call inquire_agent(ierr, asrc, nrank=nrsrc)
101 format('TRANSLATE: ', A, '[', I0, '/', I0, '] >> ', A, '[', I0, ']')

    if (ierr.eq.0) then
       call agents_translate(ierr, irtgt, atgt, asrc=asrc)
       write(*, 101) trim(asrc), -1, nrsrc, trim(atgt), irtgt
       ierr = 0
    endif
    if (ierr.eq.0) then
       do ir = 0, (nrsrc - 1) + 1
          call agents_translate(ierr, irtgt, atgt, ir, asrc)
          write(*, 101) trim(asrc), ir, nrsrc, trim(atgt), irtgt
          ierr = 0
       enddo
    endif
  end subroutine test_translate

end program test_ppp_amng
#endif /* TEST_PPP_AMNG */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

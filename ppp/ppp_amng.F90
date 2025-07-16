!!!_! ppp_amng.F90 - TOUZA/ppp agent manager (xmcomm core replacement)
! Maintainer: SAITO Fuyuki
! Created: Jan 25 2022
#define TIME_STAMP 'Time-stamp: <2025/07/16 22:09:18 fuyuki ppp_amng.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022-2025
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
  use TOUZA_Ppp_std,only: MPI_COMM_NULL, MPI_GROUP_NULL
  use TOUZA_Ppp_std,only: get_logu,     unit_global,  trace_fine,   trace_control
  use TOUZA_Ppp_std,only: MPI_GROUP_TRANSLATE_RANKS, MPI_GROUP_SIZE, MPI_GROUP_RANK
  use TOUZA_Ppp_std,only: MPI_COMM_CREATE, MPI_COMM_SPLIT, MPI_COMM_GROUP, MPI_GROUP_UNION
!!!_ + default
  implicit none
  private
!!!_ + parameter
  integer,parameter,public :: lagent = OPT_AGENT_LEN
  integer,parameter,public :: latbl  = OPT_AGENTS_MAX
!!!_  - operations
  integer,parameter,public :: OPR_PLAIN   = 0  ! as it is
  integer,parameter,public :: OPR_REVERSE = 1  ! reverse rank order
  integer,parameter,public :: OPR_SHIFT   = 2  ! shift (increment)
  integer,parameter,public :: OPR_FLOAT   = 3  ! float specified ranks
  integer,parameter,public :: OPR_IMPORT  = 4  ! import specific agent order
!!!_ + public
!!!_  - reserved agent string
  character(len=*),parameter,public :: asp_world = '*'     ! mpi_comm_world
  character(len=*),parameter,public :: asp_root  = '/'     ! alias of root agent
  character(len=*),parameter,public :: asp_base  = '%'     ! alias of source agent
  character(len=*),parameter,public :: asp_unit  = '@'     ! least mutual exclusive combination
  character(len=*),parameter,public :: asp_pfxa  = '#'     ! prefix for auto-name
  character(len=*),parameter,public :: asp_top   = ' '     ! top stack
  character(len=*),parameter,public :: asp_parent = '<'    ! parent(source) agent
  character(len=*),parameter,public :: asp_child  = '>'    ! base of children agent
!!!_ + flags
  integer,parameter :: flag_none    = -1
  integer,parameter :: flag_self    = 0
  integer,parameter :: flag_friend  = 1
  integer,parameter :: flag_other   = 2
  integer,parameter :: flag_derived = 3
  integer,parameter :: flag_root    = 4
  integer,parameter :: flag_base    = 5
  integer,parameter :: flag_unit    = 6
  integer,parameter :: flag_xunit   = 7 ! other unit
  character(len=*),parameter :: flagch(flag_none:flag_xunit) = &
       & (/'N', 'S', 'F', 'O', 'D', 'R', 'B', 'U', 'X' /)

  integer,parameter :: map_div = 80
  integer,parameter :: map_mod = 8

  character,parameter :: map_mem_s = 'X'
  character,parameter :: map_mem_o = 'o'
  character,parameter :: map_src_s = '-'
  character,parameter :: map_src_o = '_'
  character,parameter :: map_other = '.'
  character,parameter :: map_rule  = '|'

  integer,parameter :: agent_unset = -2
  integer,parameter :: agent_stack = -1
!!!_ + type
  type aprop_t
     character(len=lagent) :: name = ' '            ! agent string (aka communicator indicator)
     integer               :: comm = MPI_COMM_NULL  ! communicator
     integer               :: mgrp = MPI_GROUP_NULL ! mpi group
     integer               :: isrc = 0              ! source agent index
     integer               :: kflg = flag_none      ! flag
     integer               :: ir   = -1
     integer               :: nr   = -1
  end type aprop_t
  type(aprop_t),save :: atblp(-1:latbl)
!!!_ + static
  integer,save :: hh_agent = -1
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
# define _ERROR(E) (E - ERR_MASK_PPP_AMNG)
!!!_  - others
  character(len=128), save :: mon_tag=' '
  integer,save :: lmt=0
!!!_ + overload
  interface switch_agent
     module procedure switch_agent_ai, switch_agent_ni
  end interface switch_agent
  interface push_agent
     module procedure push_agent_ai, push_agent_ni
  end interface push_agent
  interface pop_agent
     module procedure pop_agent_ai,  pop_agent_ni
  end interface pop_agent
  interface top_agent
     module procedure top_agent_ai
  end interface top_agent
  interface inquire_agent
     module procedure inquire_agent_a, inquire_agent_i
  end interface inquire_agent
  interface is_member
     module procedure is_member_a, is_member_i
  end interface is_member
  interface agents_translate
     module procedure agents_translate_a, agents_translate_i
  end interface agents_translate
  interface base_agent
     module procedure base_agent_i
  end interface base_agent
  interface clone_agent
     module procedure clone_agent_i
  end interface clone_agent
!!!_ + interfaces
  public init, diag, finalize
  public new_agent_root,    new_agent_color,  new_agent_family
  public new_agent_derived, new_agent_spinoff
  public mod_agent_order
  public agents_translate
  public switch_agent,  push_agent, pop_agent, top_agent
  public inquire_agent, is_member
  public query_agent,   source_agent, check_agent, base_agent, clone_agent
  public trace_agent
  public is_child_agent
  public diag_maps_batch, show_status
!!!_ + common interfaces
contains
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm, nstack, nagent)
    use TOUZA_Ppp_std,only: control_mode, control_deep, is_first_force
    use TOUZA_Ppp_std,only: choice, ps_init=>init
    use TOUZA_Ppp_std,only: gen_tag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    integer,intent(in),optional :: icomm
    integer,intent(in),optional :: nstack
    integer,intent(in),optional :: nagent
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

       call gen_tag(mon_tag, __MDL__)
       lmt = len_trim(mon_tag)

       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ps_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
       endif
       if (is_first_force(init_counts, mode)) then
          if (ierr.eq.0) call init_table(ierr, nagent)
          if (ierr.eq.0) call init_stack(ierr, nstack)
          if (ierr.eq.0) call init_world(ierr, u=ulog)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = _ERROR(ERR_FAILURE_INIT)
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Ppp_std,only: control_mode, control_deep, is_first_force
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
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __MDL__, utmp)
          endif
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call diag_batch(ierr, utmp)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ps_diag(ierr, utmp, levv=lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Ppp_std,only: control_mode, control_deep, is_first_force
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
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
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
    use TOUZA_Ppp_std,only: MPI_COMM_WORLD
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
!!!_  - init_table - hash table initialization
  subroutine init_table &
       & (ierr, n)
    use TOUZA_Ppp_std,only: choice, new_htable
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: n
    integer m

    ierr = 0
    m = choice(0, n)
    if (m.le.0) m = latbl * 2
    if (hh_agent.lt.0) then
       hh_agent = new_htable('agents', m, lagent, nkey=1, def=agent_unset)
       if (hh_agent.lt.0) ierr = -1
    endif
    return
  end subroutine init_table
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
    use TOUZA_Ppp_std,only: get_wni_safe, get_ni, get_gni, msg, diag_htable
    use TOUZA_Ppp_std,only: comp_comms, comp_groups, cc_unequal, cc_both_null
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u

    integer utmp
    integer jt,   jt2
    integer kctp, jcsrc
    integer kgtp, jgsrc
    integer irw
    integer ico, irc, nrc
    integer igr, irg, nrg
    character(len=256) :: txt
    character(len=1) :: TM

    ierr = 0
    utmp = get_logu(u, ulog)

    call diag_htable(ierr, hh_agent, utmp)

    call get_wni_safe(ierr, irank=irw)

    if (irw.lt.0) then
       call msg('mpi deactivated.', __MDL__, utmp)
    else
101    format('comm:', I0, 1x, I0, ':', A, 1x, 2('[', I0, ':', I0, ']'), 1x, I0, 1x, A)
102    format('comm:', I0, 1x, I0, ':', A, 1x, 2('[', I0, ':', I0, ']'), 1x, I0, 1x, A, &
            & ' = ', I0, '(', I0, ') ', I0, '(', I0, ')')
       do jt = 0, matbl - 1
          ico = atblp(jt)%comm
          jcsrc = -1
          do jt2 = 0, jt - 1
             kctp = comp_comms(ico, atblp(jt2)%comm)
             if (kctp.lt.0) cycle
             if (kctp.ge.cc_both_null) cycle
             jcsrc = jt2
             exit
          enddo
          jgsrc = -1
          igr = atblp(jt)%mgrp
          do jt2 = 0, jt - 1
             kgtp = comp_groups(igr, atblp(jt2)%mgrp)
             if (kgtp.lt.0) cycle
             if (kgtp.ge.cc_both_null) cycle
             jgsrc = jt2
             exit
          enddo
          call get_ni(ierr, nrc, irc, ico)
          call get_gni(ierr, igr, nrg, irg)
          if (astack(jstack).eq.jt) then
             TM = '+'
          else
             TM = ' '
          endif
          if (jcsrc.ge.0) then
             write(txt, 102) irw, jt, &
                  & trim(atblp(jt)%name), &
                  & irg, nrg, irc, nrg, &
                  & atblp(jt)%isrc, trim(flagch(atblp(jt)%kflg) // TM), &
                  & jcsrc, kctp, jgsrc, kgtp
          else
             write(txt, 101) irw, jt, &
                  & trim(atblp(jt)%name), &
                  & irg, nrg, irc, nrg, &
                  & atblp(jt)%isrc, trim(flagch(atblp(jt)%kflg) // TM)
          endif
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
    use TOUZA_Ppp_std,only: choice, ndigits, msg
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
          call msg('----', __MDL__, utmp)
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
    use TOUZA_Ppp_std,only: msg, MPI_UNDEFINED
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
! #  if HAVE_FORTRAN_MPI_MPI_GROUP_TRANSLATE_RANKS
!     use MPI,only: MPI_Group_translate_ranks
! #  endif
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
!!!_  & show_stack_simple
  subroutine show_stack_simple(ierr, iagent, dir, levv, u)
    use TOUZA_Ppp_std,only: choice, msg
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: iagent
    integer,intent(in)  :: dir
    integer,intent(in),optional  :: u
    integer,intent(in),optional  :: levv
    integer lv, utmp
    character(len=256) :: buf
    integer jt

    ierr = 0
    lv = choice(lev_verbose, levv)
    utmp = get_logu(u, ulog)

101 format('stack: << ', I0, ':', A)
102 format('stack: <> ', I0, ':', A)
103 format('stack: >> ', I0, ':', A)
    if (dir.lt.0) then
       jt = astack(jstack + 1)
       write(buf, 101) jt, trim(atblp(jt)%name)
    else if (dir.eq.0) then
       jt = iagent
       write(buf, 102) jt, trim(atblp(jt)%name)
    else
       jt = iagent
       write(buf, 103) jt, trim(atblp(jt)%name)
    endif
    call msg(buf, __MDL__, utmp)
  end subroutine show_stack_simple

!!!_  & show_stack
  subroutine show_stack(ierr, iagent, dir, levv, u)
    use TOUZA_Ppp_std,only: choice, msg_mon
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: iagent
    integer,intent(in)  :: dir
    integer,intent(in),optional  :: u
    integer,intent(in),optional  :: levv
    integer lv, utmp
    character(len=256) :: buf, b
    integer js, jt

    ierr = 0
    lv = choice(lev_verbose, levv)
    utmp = get_logu(u, ulog)
    buf = 'stack:'

101 format('[', I0, ':', A, ']')
    do js = 0, jstack - max(0, dir)
       jt = astack(js)
       write(b, 101) jt, trim(atblp(jt)%name)
       buf = trim(buf) // trim(b)
    enddo
    if (dir.lt.0) then
       jt = astack(jstack + 1)
       write(b, 101) jt, trim(atblp(jt)%name)
       buf = trim(buf) // ' << ' // trim(b)
    else if (dir.eq.0) then
       jt = iagent
       write(b, 101) jt, trim(atblp(jt)%name)
       buf = trim(buf) // ' <> ' // trim(b)
    else
       jt = iagent
       write(b, 101) jt, trim(atblp(jt)%name)
       buf = trim(buf) // ' >> ' // trim(b)
    endif
    call msg_mon(buf, mon_tag(1:lmt), utmp)
  end subroutine show_stack

!!!_  & show_status
  subroutine show_status(ierr, u, levv, tag)
    use TOUZA_Ppp_std,only: choice, msg_mon
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    character(len=*),intent(in),optional :: tag
    integer utmp, lv
    integer js, jt, ja
    integer ir, nr
    integer,parameter :: ltxt = lagent * latbl
    character(len=ltxt) :: txt1, txt2
    ierr = 0
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)
    do js = 0, jstack
       jt = astack(js)
       ir = atblp(jt)%ir
       nr = atblp(jt)%nr
101    format('[', I0, ':', A, 1x, I0, '/', I0, ']')
       ja = jt
       txt2 = ' '
       do
          write(txt1, 101) ja, trim(atblp(ja)%name), ir, nr
          txt2 = trim(txt2) // trim(txt1)
          ja = atblp(ja)%isrc
          if (ja.lt.0) exit
       enddo
102    format(A, ': agent on stack<', I0, '> ', A)
103    format('agent on stack<', I0, '> ', A)
       if (present(tag)) then
          write(txt1, 102) trim(tag), js, trim(txt2)
       else
          write(txt1, 103) js, trim(txt2)
       endif
       call msg_mon(txt1, mon_tag, utmp)
    enddo
  end subroutine show_status
!!!_ + manipulation
!!!_  & query_agent() - return agent id from NAME
  integer function query_agent &
       & (name, iaref) &
       & result(n)
    implicit none
    character(len=*),intent(in) :: name
    integer,optional,intent(in) :: iaref ! current agent if null
    integer jas
    integer jt

    n = -1
    jas = source_agent(iaref)
    if (jas.lt.0) return
    if (name.eq.asp_top) then
       jt = astack(jstack)
       if (atblp(jt)%isrc.eq.jas) n = jt
    else if (name.eq.asp_child) then
       n = clone_agent(iaref)
    else if (name.eq.asp_parent) then
       n = source_agent(iaref)
    else
       n = query_agent_core(name, jas)
    endif
    return
  end function query_agent
!!!_  & inquire_agent
  subroutine inquire_agent_a &
       & (ierr,  &
       &  agent, source, &
       &  irank, nrank, icomm, igroup, name, ismem)
    implicit none
    integer,         intent(out)          :: ierr
    character(len=*),intent(in)           :: agent
    integer,         intent(in), optional :: source   ! to trace back source agents
    integer,         intent(out),optional :: irank
    integer,         intent(out),optional :: nrank
    integer,         intent(out),optional :: icomm
    integer,         intent(out),optional :: igroup
    character(len=*),intent(out),optional :: name
    logical,         intent(out),optional :: ismem
    integer ja
    ja = query_agent(agent)
    call inquire_agent_i(ierr, ja, source, irank, nrank, icomm, igroup, name, ismem)
    return
  end subroutine inquire_agent_a

  subroutine inquire_agent_i &
       & (ierr,   &
       &  iagent, source, &
       &  irank,  nrank,  icomm, igroup, name, ismem)
    use TOUZA_Ppp_std,only: choice
    implicit none
    integer,         intent(out)          :: ierr
    integer,         intent(in), optional :: iagent
    integer,         intent(in), optional :: source   ! to trace back source agents (negative to root)
    integer,         intent(out),optional :: irank
    integer,         intent(out),optional :: nrank
    integer,         intent(out),optional :: icomm
    integer,         intent(out),optional :: igroup
    character(len=*),intent(out),optional :: name
    logical,         intent(out),optional :: ismem
    integer ja
    integer ic, ig, ir

    ierr = 0
    ! ja = check_agent(iagent)
    ja = trace_agent(iagent, source)
    ic = MPI_COMM_NULL
    if (ja.lt.0) then
       ierr = -1
    endif

    if (ja.ge.0) ic = atblp(ja)%comm
    if (ja.ge.0) ig = atblp(ja)%mgrp

    if (present(irank)) then
       if (ja.ge.0) then
          ! call MPI_Group_rank(ig, irank, ierr)
          irank = atblp(ja)%ir
       else
          irank = -1
       endif
    endif
    if (present(ismem)) then
       if (ja.ge.0) then
          ! call MPI_Group_rank(ig, ir, ierr)
          ir = atblp(ja)%ir
       else
          ir = -1
       endif
       ismem = ir.ge.0
    endif
    if (present(nrank)) then
       if (ja.ge.0) then
          ! call MPI_Group_size(ig, nrank, ierr)
          nrank = atblp(ja)%nr
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
!!!_  & clone_agent - query self-copy in childlen agent
  integer function clone_agent_i (iagent) result(n)
    implicit none
    integer,intent(in),optional :: iagent
    integer jatmp
    integer jas
    jas = check_agent(iagent)
    if (jas.lt.0) then
       n = -1
    else
       do jatmp = 1, matbl
          n = mod(jas + jatmp, matbl)
          if (atblp(n)%isrc.eq.jas) then
             if (atblp(n)%kflg.eq.flag_base) return
          endif
       enddo
       n = -1
    endif
  end function clone_agent_i
!!!_  & switch_agent
  subroutine switch_agent_ai (ierr, iagent, lev)
    use TOUZA_Ppp_std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: iagent
    integer,intent(in),optional :: lev      ! 0 to switch, positive to push, negative to pop
    integer a
    ierr = 0
    a = min(+1, max(-1, choice(0, lev)))
    jstack = jstack + a
    if (jstack.lt.0.or.jstack.ge.lstack) ierr = -1
    ! if (ierr.eq.0) call show_stack_simple(ierr, iagent, a)
    if (ierr.eq.0) call show_stack(ierr, iagent, a)
    if (a.ge.0) then
       if (iagent.lt.0.or.iagent.ge.matbl) ierr = -1
       if (ierr.eq.0) astack(jstack) = iagent
    endif
  end subroutine switch_agent_ai
  subroutine switch_agent_ni (ierr, name, iaref, lev, iagent)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: name
    integer,optional,intent(in)  :: iaref
    integer,optional,intent(in)  :: lev
    integer,optional,intent(out) :: iagent  ! new agent handle
    integer ja
    ierr = 0
    ja = query_agent(name, iaref)
    call switch_agent(ierr, ja, lev)
    if (present(iagent)) then
       iagent = ja
    endif
    return
  end subroutine switch_agent_ni
!!!_  & push_agent
  subroutine push_agent_ai (ierr, iagent)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: iagent
    call switch_agent(ierr, iagent, +1)
    return
  end subroutine push_agent_ai
  subroutine push_agent_ni (ierr, name, iaref, iagent)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: name
    integer,optional,intent(in)  :: iaref
    integer,optional,intent(out) :: iagent  ! new agent handle
    integer ja
    ierr = 0
    ja = query_agent(name, iaref)
    call push_agent(ierr, ja)
    if (present(iagent)) then
       iagent = ja
    endif
    return
  end subroutine push_agent_ni
!!!_  & pop_agent
  subroutine pop_agent_ai (ierr, iagent)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: iagent
    ierr = 0
    if (present(iagent)) then
       if (astack(jstack).ne.iagent) ierr = -1
    endif
    if (ierr.eq.0) call switch_agent(ierr, -1, -1)
    return
  end subroutine pop_agent_ai
  subroutine pop_agent_ni (ierr, name, iaref)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: name
    integer,optional,intent(in)  :: iaref
    integer ja
    ierr = 0
    ja = query_agent(name, iaref)
    call pop_agent(ierr, ja)
    return
  end subroutine pop_agent_ni
!!!_  & top_agent
  subroutine top_agent_ai (ierr, iagent)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: iagent
    ierr = 0
    iagent = astack(jstack)
    return
  end subroutine top_agent_ai
!!!_  & is_member () - check if self rank is member of agent
  logical function is_member_i (iagent) result(b)
    integer,intent(in) :: iagent
    integer jerr
    call inquire_agent(jerr, iagent, ismem=b)
    if (jerr.ne.0) b = .false.
  end function is_member_i
  logical function is_member_a (agent) result(b)
    character(len=*),intent(in) :: agent
    integer ja
    integer jerr
    ja = query_agent(agent)
    call inquire_agent(jerr, ja, ismem=b)
    if (jerr.ne.0) b = .false.
  end function is_member_a

!!!_  & is_child_agent - check if matches children
  logical function is_child_agent (name, iagent) result(b)
    implicit none
    character(len=*),intent(in) :: name
    integer,optional,intent(in) :: iagent
    integer jac
    jac = clone_agent(iagent)
    if (jac.ge.0) jac = query_agent(name, jac)
    b = (jac.ge.0)
  end function is_child_agent
!!!_ + registration
!!!_  & new_agent_root
  subroutine new_agent_root &
       & (ierr, icomm, name, switch)
    use TOUZA_Ppp_std,only: get_ni, get_comm, choice, choice_a
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in),optional :: icomm
    character(len=*),intent(in),optional :: name
    integer,         intent(in),optional :: switch

    integer icsrc
    integer jagent
    character(len=lagent) :: ctmp
    integer sw

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
    sw = choice(0, switch)
    if (ierr.eq.0) then
       if (sw.ge.0) call switch_agent(ierr, jagent, sw)
    endif
    return
  end subroutine new_agent_root

!!!_  & new_agent_color
  subroutine new_agent_color &
       & (ierr, color, name, src, switch)
! #if OPT_USE_MPI
!     use MPI,only: MPI_Comm_split
! #endif /* OPT_USE_MPI */
    use TOUZA_Ppp_std,only: MPI_INTEGER
    use TOUZA_Ppp_std,only: MPI_MAX, MPI_MIN
#  if HAVE_FORTRAN_MPI_MPI_BCAST
    use MPI,only: MPI_Bcast, MPI_Reduce
#  endif
    use TOUZA_Ppp_std,only: get_ni, choice, msg
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: color
    character(len=*),intent(in),optional :: name
    integer,         intent(in),optional :: src
    integer,         intent(in),optional :: switch

    integer jas,   jau
    integer icsrc, icunit
    integer irank, nrank, iroot
    integer jc
    integer ncol,  icol,  cbuf(2)
    integer sw

    integer nt, ntx
    integer,parameter :: lttbl = latbl
    character(len=lagent) :: affils(1)
    character(len=lagent) :: aname
    character(len=lagent) :: tci(lttbl)
    integer               :: tui(lttbl)

    character(len=128) :: txt

    ! no group control except for local color
    ierr = 0

    iroot = 0

    jas = check_agent(src)
    if (jas.lt.0) ierr = -1

    if (ierr.eq.0) then
       icsrc = atblp(jas)%comm
       call get_ni(ierr, nrank, irank, icsrc)
    endif
    if (ierr.eq.0) then
       if (present(name)) then
          aname = name
       else
          aname = ' '
       endif
    endif
#if OPT_USE_MPI
    if (ierr.eq.0) then
       call MPI_Reduce &
            & (color, cbuf(1), 1, MPI_INTEGER, MPI_MIN, iroot, icsrc, ierr)
    endif
    if (ierr.eq.0) then
       call MPI_Reduce &
            & (color, cbuf(2), 1, MPI_INTEGER, MPI_MAX, iroot, icsrc, ierr)
    endif
    if (ierr.eq.0) then
       call MPI_Bcast(cbuf, 2, MPI_INTEGER, iroot, icsrc, ierr)
    endif
#else /* not OPT_USE_MPI */
    if (ierr.eq.0) then
       cbuf(:) = color
    endif
#endif /* not OPT_USE_MPI */
    if (ierr.eq.0) then
       ncol = cbuf(2) - cbuf(1) + 1
       icol = color - cbuf(1)
       call merge_color_table(ierr, nt, tci, tui, aname, icol, ncol, icsrc)
       ntx = 0
       do jc = 1, ncol
          if (tci(jc).ne.' ') ntx = ntx + 1
       enddo
       if (nt.ne.ncol.or.(ntx.ne.0.and.ntx.ne.ncol)) then
          call msg('(''INVALIE COLOR-AGENT SETS: '', I0, 1x, I0, 1x, I0)', &
               &   (/ncol, nt, ntx/), __MDL__, ulog)
          do jc = 1, nt
101          format(I0, 1x, A)
             write(txt, 101) jc, trim(tci(jc))
             call msg(txt, __MDL__, ulog)
          enddo
          ierr = _ERROR(ERR_INVALID_PARAMETER)
       endif
    endif
    if (ierr.eq.0) then
       if (ntx.eq.ncol) then
          affils(1) = tci(icol + 1)
          call new_agent_table &
               & (ierr, icol, ncol, tci(1:ncol), tui(1:ncol), src, switch)
          return
       endif
    endif

    if (ierr.eq.0) then
       call MPI_Comm_split(icsrc, color, irank, icunit, ierr)
       ! communcator name
    endif
    if (ierr.eq.0) then
       ! jau is dummy
       call add_entries_base(ierr, jau, jas)
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
    sw = choice(0, switch)
    if (ierr.eq.0) then
       if (sw.ge.0) call switch_agent(ierr, jau, sw)
    endif
    return
  end subroutine new_agent_color

!!!_  & new_agent_table - core procedure of new_agent_color
  subroutine new_agent_table &
       & (ierr, icolor, ncolor, tci, tui, src, switch)
    use TOUZA_Ppp_std,only: choice, get_ni, msg, is_msglev_debug
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: icolor
    integer,         intent(in)  :: ncolor
    character(len=*),intent(in)  :: tci(:)
    integer,         intent(in)  :: tui(:)
    integer,optional,intent(in)  :: src
    integer,optional,intent(in)  :: switch

    integer icsrc
    integer igsrc
    integer jas,   jau
    ! integer jt
    integer nrsrc, irsrc
    ! integer               :: tgr(ncolor)
    integer               :: tgu(ncolor)
    integer,allocatable   :: rgw(:)
    integer sw

    ierr = 0

    jas = check_agent(src)
    if (jas.lt.0) ierr = -1

    if (ierr.eq.0) then
       icsrc = atblp(jas)%comm
       igsrc = atblp(jas)%mgrp
    endif

    if (ierr.eq.0) call get_ni(ierr, nrsrc, irsrc, icsrc)
    ! write(*, *) 'ni', nrsrc, irsrc, icsrc
    if (ierr.eq.0) allocate(rgw(0:nrsrc-1), STAT=ierr)
    if (ierr.eq.0) then
       call batch_group_color(ierr, rgw, tgu, icolor, ncolor, icsrc, igsrc, irsrc)
    endif
    if (ierr.eq.0) deallocate(rgw, STAT=ierr)

    if (ierr.eq.0) then
       ! jau is dummy
       call add_entries_base(ierr, jau, jas)
    endif
    if (ierr.eq.0) then
       call add_agent_units(ierr, jau, tgu, tui, ncolor, jas, tci)
    endif

    sw = choice(0, switch)
    if (ierr.eq.0) then
       if (sw.ge.0) call switch_agent(ierr, jau, sw)
    endif
  end subroutine new_agent_table

!!!_  & new_agent_family
  subroutine new_agent_family &
       & (ierr, affils, src, switch)
    use TOUZA_Ppp_std,only: choice, get_ni, msg, is_msglev_debug
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: affils(:)  ! (affiliaion) array of agents to belong to
    integer,optional,intent(in)  :: src
    integer,optional,intent(in)  :: switch
    ! integer,optional,intent(in)  :: opr   ! operation on unit agents
    ! integer,optional,intent(in)  :: keys(:)   !

    integer icsrc
    integer igsrc
    integer jas,   jau
    ! integer jt
    integer nt
    integer nrsrc, irsrc
    integer,parameter :: lttbl = latbl
    character(len=lagent) :: tci(lttbl)
    integer               :: tui(lttbl)
    integer               :: tgr(lttbl)
    integer               :: tgu(lttbl)
    integer,allocatable   :: rgw(:)
    integer sw

    ierr = 0

    jas = check_agent(src)
    if (jas.lt.0) ierr = -1

    if (ierr.eq.0) then
       icsrc = atblp(jas)%comm
       igsrc = atblp(jas)%mgrp
       call merge_comm_table(ierr, nt, tci, tui, affils(:), icsrc)
    endif

    if (ierr.eq.0) call get_ni(ierr, nrsrc, irsrc, icsrc)
    ! write(*, *) 'ni', nrsrc, irsrc, icsrc
    if (ierr.eq.0) allocate(rgw(0:nrsrc-1), STAT=ierr)
    if (ierr.eq.0) then
       call batch_group_split(ierr, rgw, tgr, tci, nt, affils(:), icsrc, igsrc, irsrc)
    endif
    if (ierr.eq.0) deallocate(rgw, STAT=ierr)

    if (ierr.eq.0) then
       call gen_comm_unit(ierr, tgu, tui, tgr, nt, icsrc)
    endif

    if (ierr.eq.0) then
       ! jau is dummy
       call add_entries_base(ierr, jau, jas)
    endif
    if (ierr.eq.0) then
       call add_agent_units(ierr, jau, tgu, tui, nt, jas)
    endif
    if (ierr.eq.0) then
       call add_agent_family(ierr, tgr, tci, tui, nt, affils(:), jas)
    endif

    sw = choice(0, switch)
    if (ierr.eq.0) then
       if (sw.ge.0) call switch_agent(ierr, jau, sw)
    endif
  end subroutine new_agent_family

!!!_  & new_agent_derived
  subroutine new_agent_derived &
       & (ierr, name, alist, iagent)
#if OPT_USE_MPI
    use MPI,only: MPI_Group_rank
#endif
    use TOUZA_Ppp_std,only: msg, MPI_UNDEFINED
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
            &  atblp, jas, alist)
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

!!!_  & new_agent_spinoff
  subroutine new_agent_spinoff &
       & (ierr, name, iagent, switch)
    use TOUZA_Ppp_std,only: msg, choice
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: name      ! new name
    integer,optional,intent(in)  :: iagent    ! current agent-id if null
    integer,optional,intent(in)  :: switch

    integer jas, jac, jax
    integer sw

    ierr = 0

    jac = clone_agent(iagent)
    ! to implement later
    if (jac.lt.0) then
       jas = check_agent(iagent)
       call add_entries_base(ierr, jac, jas)
       if (ierr.eq.0) then
          jas = source_agent(jac)
          call add_entry_copy(ierr, jax, asp_unit, jac, jas, flag=flag_unit)
       endif
    endif
    if (ierr.eq.0) then
       jas = source_agent(jac)
       call add_entry_copy &
            & (ierr, jax, name, jac, jas, flag_derived)
    endif
    if (ierr.eq.0) then
       sw = choice(0, switch)
       if (sw.ge.0) call switch_agent(ierr, jax, sw)
    endif
  end subroutine new_agent_spinoff
!!!_ + other utilities
!!!_  & mod_agent_order - reorder rank in agent/group
  subroutine mod_agent_order &
       & (ierr, atgt, opr, keys, iagent)
! #  if HAVE_FORTRAN_MPI_MPI_GROUP_TRANSLATE_RANKS
!     use MPI,only: MPI_Group_translate_ranks
! #  endif
    use TOUZA_Ppp_std,only: msg, MPI_UNDEFINED
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

    integer jaref, jgref, nrref
    integer,allocatable :: rref(:)
    integer,allocatable :: rdst(:)

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
       case (OPR_SHIFT)
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
       case (OPR_IMPORT)
          if (present(keys)) then
             jaref = keys(1)
          else
             ierr = -1
          endif
          ! todo: check on reference agent
          if (ierr.eq.0) jgref = atblp(jaref)%mgrp
          if (ierr.eq.0) call MPI_Group_size(jgref, nrref, ierr)
          if (ierr.eq.0) allocate(rref(0:max(nrank,nrref)-1), rdst(0:nrref-1), STAT=ierr)
          if (ierr.eq.0) then
             do jk = 0, nrref - 1
                rref(jk) = jk
             enddo
             call MPI_Group_translate_ranks(jgref, nrref, rref, igtgt, rdst, ierr)
          endif
          if (ierr.eq.0) then
             ! write(*, *) 'rref', rref
             ! write(*, *) 'rdst', rdst
             rref(0:nrank-1) = -1
             jr = 0
             do jk = 0, nrref - 1
                if (rdst(jk).ne.MPI_UNDEFINED) then
                   rref(rdst(jk)) = jr
                   jr = jr + 1
                endif
             enddo
             do jk = 0, nrank - 1
                if (rref(jk).lt.0) then
                   rref(jk) = jr
                   jr = jr + 1
                endif
             enddo
             do jk = 0, nrank - 1
                ranks(rref(jk)) = jk
             enddo
             ! write(*, *) 'rank', ranks
          endif
          if (ierr.eq.0) deallocate(rref, rdst, STAT=ierr)
          ! b = .FALSE.
       case default
          b = .FALSE.
       end select
    endif

    if (b) then
       ! write(*, *) 'order', igtgt, ranks(0:nrank-1)
       return
       if (ierr.eq.0) call MPI_Group_incl(igtgt, nrank, ranks(0:nrank-1), ignew, ierr)
       if (ierr.eq.0) then
          icsrc = atblp(ja)%comm
          if (icsrc.eq.MPI_COMM_NULL) then
             icnew = icsrc
          else
             call MPI_Comm_create(icsrc, ignew, icnew, ierr)
             ! communicator name
          endif
       endif
       if (ierr.eq.0) then
          atblp(ja)%comm = icnew
          ! atblp(ja)%mgrp = ignew
          call cache_agent_props(ierr, atblp(ja), ignew)
       endif
    endif
    if (ierr.eq.0) deallocate(ranks, STAT=ierr)

  end subroutine mod_agent_order
!!!_  & agents_translate - translate ranks between agents
  subroutine agents_translate_a &
       & (ierr, irtgt, atgt, irsrc, asrc)
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
    use TOUZA_Ppp_std,only: MPI_UNDEFINED
! #  if HAVE_FORTRAN_MPI_MPI_GROUP_TRANSLATE_RANKS
!     use MPI,only: MPI_Group_translate_ranks
! #  endif
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

!!!_  & trace_agent
  integer function trace_agent &
       & (iagent, lev) &
       & result(n)
    use TOUZA_Ppp_std,only: choice
    implicit none
    integer,intent(in),optional :: iagent
    integer,intent(in),optional :: lev
    integer ntr
    ntr = choice(0, lev)
    if (ntr.lt.0) then
       n = root_agent(iagent)
    else
       n = check_agent(iagent)
       do
          if (ntr.le.0) exit
          if (n.lt.0) exit
          n = atblp(n)%isrc
          ntr = ntr - 1
       enddo
    endif
  end function trace_agent

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

!!!_  & add_agent_units
  subroutine add_agent_units &
       & (ierr, &
       &  jau,  &
       &  tgu,  &
       &  tui,  nt, src, names)
#if OPT_USE_MPI
    ! use MPI,only: MPI_Comm_create, MPI_Group_rank
    use MPI,only: MPI_Group_rank
#endif
    use TOUZA_Ppp_std,only: choice, MPI_UNDEFINED
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: jau
    integer,         intent(in)          :: tgu(0:*)
    integer,         intent(in)          :: tui(:)
    integer,         intent(in)          :: nt
    integer,         intent(in)          :: src
    character(len=*),intent(in),optional :: names(0:*)

    character(len=lagent) :: buf
    integer munit
    integer ju
    integer ja
    integer ird
    integer igdrv

    ierr = 0
    munit = maxval(tui(1:nt))
101 format(A, I0)
    jau = -1
    do ju = 0, munit
       igdrv = tgu(ju)
       if (ierr.eq.0) call MPI_Group_rank(igdrv, ird, ierr)
       if (ierr.eq.0) then
          buf = ' '
          if (present(names)) then
             buf = names(ju)
          endif
          if (buf.eq.' ') write(buf, 101) trim(asp_unit), ju
          if (ird.eq.MPI_UNDEFINED) then
             call add_entry_group(ierr, ja, buf, igdrv, src, flag_xunit)
          else
             ! set jau to return
             call add_entry_group(ierr, jau, buf, igdrv, src, flag_unit)
             ! call add_entry_group(ierr, jau, asp_unit, igdrv, src, flag_unit)
          endif
       endif
    enddo

    if (ierr.eq.0) then
       ierr = add_agent_alias(asp_unit, src, jau)
       ierr = min(0, ierr)
    endif
    ! write(*, *) 'add_agent_units', ierr, jau
  end subroutine add_agent_units

!!!_  & add_agent_family
  subroutine add_agent_family &
       & (ierr, &
       &  tgr,  &
       &  tci,  tui,  nt, affils, src)
! #if OPT_USE_MPI
!     use MPI,only: MPI_Comm_create
! #endif
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

    call add_agent(ierr, ja, tci(1:nt), src)
    if (ierr.lt.0) return

    je = ja + nt - 1

    atblp(ja:je)%name = tci(1:nt)
    ! atblp(ja:je)%mgrp = tgr(1:nt)
    do jt = 1, nt
       call cache_agent_props(ierr, atblp(ja+jt-1), tgr(jt))
    enddo
    ! atblp(ja:je)%isrc = src
    icsrc = atblp(src)%comm

    do jt = 1, nt
       if (ierr.eq.0) then
          call MPI_Comm_create(icsrc, tgr(jt), icnew, ierr)
          ! communicator name
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
!!!_  & add_entries_base
  subroutine add_entries_base &
       & (ierr, iagent, source)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: iagent
    integer,         intent(in)  :: source
    integer jau
    integer icroot

    ierr = 0
    if (ierr.eq.0) then
       ! jau is dummy
       ! clone base agent from source
       call add_entry_comm(ierr, iagent, asp_base, src=source, flag=flag_base)
    endif
    if (ierr.eq.0) then
       ! jau is dummy
       ! clone root agent
       jau = root_agent(source)
       if (jau.ge.0) then
          icroot = atblp(jau)%comm
          call add_entry_comm(ierr, jau, asp_root, icroot, src=source, flag=flag_root)
       endif
    endif

  end subroutine add_entries_base

!!!_  & add_entry_comm
  subroutine add_entry_comm &
       & (ierr, iagent, name, icomm, src, flag)
! #if OPT_USE_MPI
!     use MPI,only: MPI_Comm_group
! #endif
    use TOUZA_Ppp_std,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: iagent
    character(len=*),intent(in)  :: name
    integer,optional,intent(in)  :: icomm ! either icomm or src must be exit
    integer,optional,intent(in)  :: src
    integer,optional,intent(in)  :: flag

    integer jgrp
    integer isrc

    ierr = 0
    isrc = choice(-1, src)

    call add_agent(ierr, iagent, (/name/), isrc)
    if (ierr.lt.0) return

    atblp(iagent)%name = name
    atblp(iagent)%kflg = choice(flag_root, flag)
    if (present(icomm)) then
       atblp(iagent)%comm = icomm
       ! atblp(iagent)%isrc = isrc
       call MPI_Comm_group(icomm, jgrp, ierr)
       if (ierr.eq.0) then
          ! atblp(iagent)%mgrp = jgrp
          call cache_agent_props(ierr, atblp(iagent), jgrp)
       else
          atblp(iagent)%mgrp = MPI_GROUP_NULL
          ierr = -1
       endif
    else if (present(src)) then
       ! atblp(iagent)%isrc = isrc
       if (src.lt.0.or.src.ge.matbl) then
          ierr = -1
       else
          atblp(iagent)%comm = atblp(src)%comm
          ! atblp(iagent)%mgrp = atblp(src)%mgrp
          call cache_agent_props(ierr, atblp(iagent), atblp(src)%mgrp)
       endif
    else
       ierr = -1
    endif

    return
  end subroutine add_entry_comm

!!!_  & add_entry_group
  subroutine add_entry_group &
       & (ierr, iagent, name, igroup, src, flag)
! #if OPT_USE_MPI
!     use MPI,only: MPI_Comm_create
! #endif
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
    call add_agent(ierr, iagent, (/name/), src)
    if (ierr.lt.0) return

    atblp(iagent)%name = name
    atblp(iagent)%kflg = flag
    ! atblp(iagent)%isrc = src
    ! atblp(iagent)%mgrp = igroup
    call cache_agent_props(ierr, atblp(iagent), igroup)
    icsrc = atblp(src)%comm

    call MPI_Comm_create(icsrc, igroup, icnew, ierr)
    ! communicator name
    if (ierr.eq.0) atblp(iagent)%comm = icnew

    return
  end subroutine add_entry_group

!!!_  & add_entry_copy
  subroutine add_entry_copy &
       & (ierr, iagent, name, iaref, src, flag)
    use TOUZA_Ppp_std,only: choice
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: iagent
    character(len=*),intent(in)  :: name
    integer,         intent(in)  :: iaref
    integer,         intent(in)  :: src
    integer,         intent(in)  :: flag

    ierr = 0
    call add_agent(ierr, iagent, (/name/), src)
    if (ierr.lt.0) return

    atblp(iagent)%name = name
    atblp(iagent)%kflg = flag
    ! atblp(iagent)%mgrp = atblp(iaref)%mgrp
    atblp(iagent)%comm = atblp(iaref)%comm
    call cache_agent_props(ierr, atblp(iagent), atblp(iaref)%mgrp)

    return
  end subroutine add_entry_copy

!!!_  & merge_comm_table - merge agent table
  subroutine merge_comm_table &
       & (ierr, &
       &  ntotal, tbl_ci, tbl_ui, &
       &  affils, icomm)
    use TOUZA_Ppp_std,only: MPI_CHARACTER, MPI_INTEGER
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
#if OPT_USE_MPI
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
#endif /* OPT_USE_MPI */
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
#if OPT_USE_MPI
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
#endif /* OPT_USE_MPI */
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
    use TOUZA_Ppp_std,only: MPI_STATUS_SIZE
    use TOUZA_Ppp_std,only: MPI_CHARACTER, MPI_INTEGER
#if OPT_USE_MPI
    use MPI,only: MPI_Probe, MPI_Get_count
#endif
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

#if OPT_USE_MPI
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
#else /* not OPT_USE_MPI */
    nrcv = 0
#endif /* not OPT_USE_MPI */
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

!!!_  & merge_color_table - merge color-agent table
  subroutine merge_color_table &
       & (ierr, &
       &  ntotal, tbl_ci, tbl_ui, &
       &  aname,  icolor, ncolor, icomm)
    use TOUZA_Ppp_std,only: MPI_CHARACTER, MPI_INTEGER
#  if HAVE_FORTRAN_MPI_MPI_BCAST
    use MPI,only: MPI_Send, MPI_Bcast
#  endif
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ntotal     ! total number of agents
    character(len=*),intent(out) :: tbl_ci(0:*)  ! agent table merged
    integer,         intent(out) :: tbl_ui(0:*)  ! agent unit-id table
    character(len=*),intent(in)  :: aname      ! affiliaion corresponds to icolor
    integer,         intent(in)  :: icolor, ncolor
    integer,         intent(in)  :: icomm      ! mpi communicator

    integer nrank, irank, iroot
    integer,parameter :: lbuf = OPT_AGENTS_MAX
    integer,parameter :: ndiv = 3

    character(len=lagent) :: dbuf(0:lbuf-1)
    integer               :: ubuf(0:lbuf-1)
    integer mstp,  ktag
    integer ipsrc, ipdst
    integer msend
    integer j

    ierr = 0
    iroot = 0
    ntotal = -1

    call get_cnr(ierr, icomm, irank, nrank)

    if (ierr.eq.0) then
       ntotal = ncolor
       do j = 0, ncolor - 1
          ubuf(j) = j
       enddo
       dbuf(0:ncolor-1) = ' '
       dbuf(icolor) = aname
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
                   call recv_color_affils &
                        & (ierr,  ntotal, dbuf,  ubuf, &
                        &  icomm, ncolor, ipsrc, ktag)
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
#if OPT_USE_MPI
                if (ierr.eq.0) then
                   call MPI_Send &
                        & (ubuf(0:ntotal-1), ntotal, MPI_INTEGER, &
                        &  ipdst, ktag,    icomm,  ierr)
                endif
                if (ierr.eq.0) then
                   call MPI_Send &
                        & (dbuf(0:ntotal-1), msend, MPI_CHARACTER, &
                        &  ipdst, ktag,    icomm, ierr)
                endif
#endif /* OPT_USE_MPI */
                exit
             else
                ! recieve and continue
                ktag = 0
                do j = 1, ndiv - 1
                   ipsrc = irank + j * (mstp / ndiv)
                   if (ipsrc.ge.nrank) exit
                   ktag = ktag + 1
                   if (ierr.eq.0) then
                      call recv_color_affils &
                           & (ierr,  ntotal, dbuf,  ubuf, &
                           &  icomm, ncolor, ipsrc,  ktag)
                   endif
                enddo
                mstp = mstp * ndiv
             endif
          enddo
       endif
    endif
#if OPT_USE_MPI
    if (ierr.eq.0) then
       call MPI_Bcast(ntotal, 1, MPI_INTEGER, iroot, icomm, ierr)
    endif
    if (ierr.eq.0) then
       call MPI_Bcast(ubuf(0:ntotal-1), ntotal, MPI_INTEGER, iroot, icomm, ierr)
    endif
    if (ierr.eq.0) then
       msend = ntotal * lagent
       call MPI_Bcast(dbuf(0:ntotal-1), msend, MPI_CHARACTER, iroot, icomm, ierr)
    endif
#endif /* OPT_USE_MPI */
    if (ierr.eq.0) then
       tbl_ci(0:ntotal-1) = dbuf(0:ntotal-1)
       tbl_ui(0:ntotal-1) = ubuf(0:ntotal-1)
    endif
    return
  end subroutine merge_color_table

!!!_  & recv_color_affils
  subroutine recv_color_affils &
       & (ierr,  ntotal, dbuf,  ubuf, &
       &  icomm, ncolor, ipsrc, ktag)
    use TOUZA_Ppp_std,only: MPI_STATUS_SIZE
    use TOUZA_Ppp_std,only: MPI_CHARACTER, MPI_INTEGER
#if OPT_USE_MPI
    use MPI,only: MPI_Probe, MPI_Get_count
#endif
#  if HAVE_FORTRAN_MPI_MPI_BCAST
    use MPI,only: MPI_Recv
#  endif
    implicit none
    integer,              intent(out)   :: ierr
    integer,              intent(inout) :: ntotal
    character(len=lagent),intent(inout) :: dbuf(0:)
    integer,              intent(inout) :: ubuf(0:)
    integer,              intent(in)    :: icomm
    integer,              intent(in)    :: ncolor
    integer,              intent(in)    :: ipsrc, ktag

    integer,parameter     :: lrcv = OPT_AGENTS_MAX
    character(len=lagent) :: drcv(0:lrcv-1)
    integer               :: urcv(0:lrcv-1)
    integer :: istts(MPI_STATUS_SIZE)
    integer nrcv
    integer j, jj, jn

    ierr = 0

#if OPT_USE_MPI
    call MPI_Probe(ipsrc, ktag, icomm, istts, ierr)
    if (ierr.eq.0) call MPI_Get_count(istts, MPI_INTEGER, nrcv, ierr)
    if (ierr.eq.0) then
       call MPI_Recv &
            & (urcv(0:nrcv-1), nrcv, MPI_INTEGER, &
            &  ipsrc, ktag,  icomm, istts, ierr)
    endif
    if (ierr.eq.0) then
       call MPI_Recv &
            & (drcv(0:nrcv-1), (nrcv * lagent), MPI_CHARACTER, &
            &  ipsrc, ktag,  icomm, istts, ierr)
    endif
#else /* not OPT_USE_MPI */
    nrcv = 0
#endif /* not OPT_USE_MPI */
    if (ierr.eq.0) then
       ! write(*, *) 'n:', ntotal, nrcv
       ! do j = 0, ntotal - 1
       !    write(*, *) 'dbuf:', j, dbuf(j)
       ! enddo
       ! do j = 0, nrcv - 1
       !    write(*, *) 'drcv:', j, drcv(j)
       ! enddo
       do j = 0, ncolor - 1
          if (dbuf(j).eq.' ') then
             dbuf(j) = drcv(j)
          else if (drcv(j).eq.' ') then
             continue
          else if (drcv(j).eq.dbuf(j)) then
             continue
          else
             jn = ntotal
             do jj = ncolor, ntotal - 1
                if (drcv(j).eq.dbuf(jj)) then
                   jn = -1
                   exit
                endif
             enddo
             if (jn.ge.0) then
                dbuf(jn) = drcv(j)
                ubuf(jn) = jn
                ntotal = ntotal + 1
             endif
          endif
       enddo
       do j = ncolor, nrcv - 1
          jn = ntotal
          do jj = ncolor, ntotal - 1
             if (drcv(j).eq.dbuf(jj)) then
                jn = -1
                exit
             endif
          enddo
          if (jn.ge.0) then
             dbuf(jn) = drcv(j)
             ubuf(jn) = jn
             ntotal = ntotal + 1
          endif
       enddo
    endif
    return
  end subroutine recv_color_affils

!!!_  & batch_group_split
  subroutine batch_group_split &
       & (ierr,   ranks,  tbl_gr, &
       &  tbl_ci, ntotal, affils, icomm, igsrc, ir)
    use TOUZA_Ppp_std,only: MPI_UNDEFINED, MPI_COMM_NULL
    use TOUZA_Ppp_std,only: MPI_INTEGER
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
#if OPT_USE_MPI
          call MPI_Gather(ibuf, 1, MPI_INTEGER, ranks, 1, MPI_INTEGER, iroot, icomm, ierr)
          if (ierr.ne.0) exit
#endif /* OPT_USE_MPI */
          if (ir.eq.iroot) then
             m = COUNT(ranks(:).ge.0)
             ranks(0:m-1) = PACK(ranks(:), ranks(:).ge.0)
          endif
#if OPT_USE_MPI
          call MPI_Bcast(m, 1, MPI_INTEGER, iroot, icomm, ierr)
          if (ierr.eq.0) then
             call MPI_Bcast(ranks(0:m-1), m, MPI_INTEGER, iroot, icomm, ierr)
          endif
          if (ierr.eq.0) call MPI_Group_incl(igsrc, m, ranks(0:m-1), jgnew, ierr)
#else /* not OPT_USE_MPI */
          jgnew = MPI_GROUP_NULL
#endif /* not OPT_USE_MPI */
          ! write(*, *) 'incl', j, jgnew, m, ranks(0:m-1)
          if (ierr.eq.0) tbl_gr(j) = jgnew
       enddo
    endif
  end subroutine batch_group_split

!!!_  & batch_group_color
  subroutine batch_group_color &
       & (ierr,   ranks,  tbl_gr, &
       &  icolor, ncolor, icomm, igsrc, ir)
    use TOUZA_Ppp_std,only: MPI_UNDEFINED, MPI_COMM_NULL
    use TOUZA_Ppp_std,only: MPI_INTEGER
#  if HAVE_FORTRAN_MPI_MPI_BCAST
    use MPI,only: MPI_Gather, MPI_Bcast
#  endif
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ranks(0:)  ! work-array
    integer,         intent(out) :: tbl_gr(0:*)
    integer,         intent(in)  :: icolor, ncolor
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
       do j = 0, ncolor - 1
          if (icolor.eq.j) then
             ibuf(1) = ir
          else
             ibuf(1) = -1
          endif
#if OPT_USE_MPI
          call MPI_Gather(ibuf, 1, MPI_INTEGER, ranks, 1, MPI_INTEGER, iroot, icomm, ierr)
          if (ierr.ne.0) exit
#endif /* OPT_USE_MPI */
          if (ir.eq.iroot) then
             m = COUNT(ranks(:).ge.0)
             ranks(0:m-1) = PACK(ranks(:), ranks(:).ge.0)
          endif
#if OPT_USE_MPI
          call MPI_Bcast(m, 1, MPI_INTEGER, iroot, icomm, ierr)
          if (ierr.eq.0) then
             call MPI_Bcast(ranks(0:m-1), m, MPI_INTEGER, iroot, icomm, ierr)
          endif
          if (ierr.eq.0) call MPI_Group_incl(igsrc, m, ranks(0:m-1), jgnew, ierr)
#else  /* not OPT_USE_MPI */
          jgnew = MPI_GROUP_NULL
#endif /* not OPT_USE_MPI */
          ! write(*, *) 'incl', j, jgnew, m, ranks(0:m-1)
          if (ierr.eq.0) tbl_gr(j) = jgnew
       enddo
    endif
  end subroutine batch_group_color

!!!_  & gen_comm_unit
  subroutine gen_comm_unit &
       & (ierr,   tbl_gu, &
       &  tbl_ui, tbl_gr, ntotal, icomm)
    use TOUZA_Ppp_std,only: choice, MPI_UNDEFINED,  MPI_COMM_NULL, MPI_GROUP_EMPTY
! #if OPT_USE_MPI
!     use MPI,only: MPI_Group_union
! #endif
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: tbl_gu(0:*)
    integer,         intent(in)  :: tbl_ui(*)
    integer,         intent(in)  :: tbl_gr(*)
    integer,         intent(in)  :: ntotal
    integer,         intent(in)  :: icomm

    integer irank
    integer ju, munit
    integer js
    integer key
    integer igtmp, igdrv

    ierr = 0
    call get_cnr(ierr, icomm, irank)
    if (ierr.eq.0) then
       munit = maxval(tbl_ui(1:ntotal))
       do ju = 0, munit
          igdrv = MPI_GROUP_EMPTY
          do js = 1, ntotal
             if (tbl_ui(js).eq.ju) then
                call MPI_Group_size(tbl_gr(js), key, ierr)
                if (ierr.eq.0) call MPI_Group_union(igdrv, tbl_gr(js), igtmp, ierr)
                if (ierr.eq.0) igdrv = igtmp
             endif
          enddo
          if (ierr.eq.0) tbl_gu(ju) = igdrv
       enddo
    endif
  end subroutine gen_comm_unit

!!!_  & derive_group
  subroutine derive_group &
       & (ierr, igdrv, &
       &  tbl,  src, alist)
    use TOUZA_Ppp_std,only: MPI_GROUP_EMPTY
! #if OPT_USE_MPI
!     use MPI,only: MPI_Group_union
! #endif
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: igdrv
    type(aprop_t),   intent(in)  :: tbl(-1:*)
    character(len=*),intent(in)  :: alist(:)
    integer,         intent(in)  :: src

    integer jt
    integer igdst, igtmp
    integer ja

    ierr = 0
    igtmp = MPI_GROUP_EMPTY
    igdrv = igtmp
    do jt = 1, size(alist(:), 1)
       ja = query_agent_core(alist(jt), src)
       if (ja.lt.0) then
          ierr = ja
          exit
       endif
       if (ierr.eq.0) call MPI_Group_union(igtmp, tbl(ja)%mgrp, igdst, ierr)
       if (ierr.eq.0) igtmp = igdst
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

    ierr = 0
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

!!!_  - add_agent
  subroutine add_agent &
       & (ierr, iagent, names, source)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: iagent
    character(len=*),intent(in)  :: names(0:)
    integer,         intent(in)  :: source
    integer j, m, e
    ierr = 0
    iagent = matbl
    m = size(names, 1)
    do j = 0, m - 1
       e = add_agent_core(names(j), source)
       if (e.lt.0) then
          ierr = e
          return
       endif
    enddo
    if (matbl.ge.latbl) ierr = -1
    return
  end subroutine add_agent

!!!_  - add_agent_core
  integer function add_agent_core &
       & (name, source) &
       & result(n)
    use TOUZA_Ppp_std,only: choice
    implicit none
    character(len=*),intent(in) :: name
    integer,         intent(in) :: source
    integer e

    n = query_agent_core(name, source)
    if (n.ge.0) then
       n = _ERROR(ERR_DUPLICATE_SET)
       return
    endif
    n = matbl
    matbl = matbl + 1
    if (n.ge.latbl) then
       n = _ERROR(ERR_INSUFFICIENT_BUFFER)
       return
    endif
    e = reg_agent(name, source, n)
    atblp(n)%isrc = source
    ! write(*, *) 'reg:', source, n, ' ', trim(name)
    if (e.lt.0) n = e
  end function add_agent_core

!!!_  - add_agent_alias
  integer function add_agent_alias &
       & (name, source, iagent) &
       & result(n)
    implicit none
    character(len=*),intent(in) :: name
    integer,         intent(in) :: source
    integer,         intent(in) :: iagent
    integer e

    n = query_agent_core(name, source)
    if (n.ge.0) then
       n = _ERROR(ERR_DUPLICATE_SET)
       return
    endif
    e = reg_agent(name, source, iagent)
    n = min(0, e)
  end function add_agent_alias

!!!_  - reg_agent - reg_entry() wrapper
  integer function reg_agent &
       & (name, source, iagent) &
       & result(ee)
    use TOUZA_Ppp_std,only: reg_entry
    implicit none
    character(len=*),intent(in) :: name
    integer,         intent(in) :: source
    integer,         intent(in) :: iagent
    ee = reg_entry(hh_agent, name, (/source/), (/iagent/))
  end function reg_agent

!!!_  - query_agent_core - query_staus() wrapper
  integer function query_agent_core &
       & (name, source) &
       & result(n)
    use TOUZA_Ppp_std,only: query_status
    implicit none
    character(len=*),intent(in) :: name
    integer,         intent(in) :: source
    integer jerr
    call query_status(jerr, n, hh_agent, name, (/source/))
    if (jerr.ne.0) n = jerr
  end function query_agent_core

!!!_  - cache_agent_props - cache agent properties
  subroutine cache_agent_props(ierr, tbl, igrp)
    implicit none
    integer,      intent(out)   :: ierr
    type(aprop_t),intent(inout) :: tbl
    integer,      intent(in)    :: igrp
    integer ir, nr

    ierr = 0
    tbl%mgrp = igrp

    if (ierr.eq.0) call MPI_Group_rank(igrp, ir, ierr)
    if (ierr.eq.0) tbl%ir = ir
    if (ierr.eq.0) call MPI_Group_size(igrp, nr, ierr)
    if (ierr.eq.0) tbl%nr = nr

  end subroutine cache_agent_props

!!!_ + end TOUZA_Ppp_amng
end module TOUZA_Ppp_amng
!!!_@ test_ppp_amng - test program
#if TEST_PPP_AMNG
program test_ppp_amng
  ! use MPI
  use TOUZA_Std_mwe
  use TOUZA_Ppp_amng,tp_init=>init, tp_diag=>diag, tp_finalize=>finalize
  implicit none

  integer ierr
  integer icw, ibase
  integer irw, nrw
  integer mclr
  integer color
  integer jseq, icol, jdmy
  integer j
  integer jarg
  integer ktest
  integer nrlim
  character(len=256) :: T

  ierr = 0
  jarg = 0

101 format(A, ' = ', I0)
  call tp_init(ierr, levv=+9, stdv=+9)
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
  if (ierr.eq.0) then
     jarg = jarg + 1
     call get_command_argument(jarg, T, STATUS=ierr)
     if (ierr.ne.0) T = '17'
     read(T, *, IOSTAT=ierr) nrlim
     nrlim = max(0, nrlim)
     ierr = 0
  endif

  if (nrw.gt.nrlim.and.nrlim.gt.0) then
     jseq = irw - (nrw - nrlim) / 2
     icol = 0
     if (jseq.lt.0.or.jseq.ge.NRLIM) icol = MPI_UNDEFINED
     if (ierr.eq.0) call MPI_Comm_split(icw, icol, jseq, ibase, jdmy)
  else
     icol = 0
     ibase = icw
  endif

  if (icol.ge.0) then
     call MPI_Comm_rank(ibase, irw, ierr)
     call MPI_Comm_size(ibase, nrw, ierr)

     mclr = 11
     color = 0
     do j = 0, nrw / mclr
        if (irw.ge.j*mclr .and. irw.lt.(j+1)*mclr) exit
        color = color + 1
     enddo
     call test_ppp_agent(ibase, color, ktest)
  endif

  call show_status(ierr, tag='final')

  call tp_diag(ierr)
  write(*, 101) 'DIAG', ierr
  call tp_finalize(ierr)
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
    integer iaref

    ierr = 0

    if (ierr.eq.0) then
       call new_agent_root(ierr, icomm)
    endif
    ! write(*, *) 'root', ierr
    if (ierr.eq.0) call new_agent_color(ierr, color, switch=+1)
    ! write(*, *) 'color', ierr

    if (ierr.eq.0) call inquire_agent(ierr, irank=irank, nrank=nrank)
    if (ierr.eq.0) then
       call set_drivers(nself, drvs, irank, nrank, ktest)
    endif
    if (ierr.eq.0) then
       call new_agent_family(ierr, drvs(1:nself), switch=+1)
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
       ierr = 0
    endif
    if (ierr.eq.0) then
       if (mod(color,2).eq.1) then
          iaref = query_agent('@')
          call mod_agent_order(ierr, '@', OPR_SHIFT, (/1/))
          call mod_agent_order(ierr, 'H', OPR_IMPORT, (/iaref/))
          call mod_agent_order(ierr, 'A', OPR_IMPORT, (/iaref/))
       endif
       ! write(*, *) 'import = ', ierr
    endif
    if (ierr.eq.0) then
       call test_translate(ierr, 'A', 'B')
       call test_translate(ierr, 'A', 'W')
       call test_translate(ierr, 'A', 'Q')
       call test_translate(ierr, 'A', '/')
       call test_translate(ierr, 'A', '%')
       call test_translate(ierr, 'B', '/')
       call test_translate(ierr, 'B', '%')
       call test_translate(ierr, 'Q', '/')
       call test_translate(ierr, 'Q', '@')
    endif

    if (ierr.eq.0) then
       iaref = query_agent('A')
       if (is_member(iaref)) then
          call new_agent_family(ierr, (/'I'/), iaref, switch=+1)
          call pop_agent(ierr)
       endif
    endif
102 format('is_child:', A, 1x, L1)
    if (ierr.eq.0) then
       iaref = query_agent('B')
       if (is_member(iaref)) then
          write(*, 102) 'I', is_child_agent('I', iaref)
          call new_agent_family(ierr, (/'I'/), iaref, switch=-1)
          call push_agent(ierr, iaref)
          call switch_agent(ierr, '>')
          call push_agent(ierr, 'I')
          call switch_agent(ierr, '<')
          write(*, 102) 'I', is_child_agent('I', iaref)
       endif
    endif
    if (ierr.eq.0) then
       iaref = query_agent('X')
       if (is_member(iaref)) then
          ! write(*, *) 'spinoff:0:ref=', iaref
          call new_agent_spinoff(ierr, 'J', iaref, switch=-1)
          if (is_child_agent('I', iaref)) then
             write(*, 102) 'I', .TRUE.
          else
             write(*, 102) 'I', .FALSE.
             ! write(*, *) 'spinoff:1:ref=', iaref
             call new_agent_spinoff(ierr, 'I', iaref)
          endif
          write(*, 102) 'I', is_child_agent('I', iaref)
       endif
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

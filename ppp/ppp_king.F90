!!!_! ppp_king.F90 - TOUZA/ppp king control (xmcomm/xmking replacement)
! Maintainer: SAITO Fuyuki
! Created: Jan 28 2022
#define TIME_STAMP 'Time-stamp: <2022/03/02 08:19:29 fuyuki ppp_king.F90>'
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
#ifndef   TEST_PPP_KING
#  define TEST_PPP_KING 0
#endif
#ifndef   DEBUG_KING
#  define DEBUG_KING TEST_PPP_KING
#endif
#ifndef   OPT_KING_MODULE_LEN
#  define OPT_KING_MODULE_LEN 9
#endif
#ifndef   OPT_KING_FILTER_LIM
#  define OPT_KING_FILTER_LIM 256
#endif
#ifndef   OPT_KING_CACHE_LIM
#  define OPT_KING_CACHE_LIM 256
#endif
#ifndef   OPT_KING_HASH_BASE
#  define OPT_KING_HASH_BASE 13
#endif
#ifndef   OPT_KING_HASH_CARRY
#  define OPT_KING_HASH_CARRY 1
#endif
!!!_@ TOUZA_Ppp_king - MPI king-rank control
module TOUZA_Ppp_king
!!!_ + modules
  use TOUZA_Ppp_std,only: &
       & control_mode, control_deep, is_first_force, &
       & get_logu,     unit_global,  trace_fine,   trace_control
!!!_ + default
  implicit none
  private
!!!_ + parameter
  integer,parameter :: lmdl = OPT_KING_MODULE_LEN
  integer,parameter :: lctb = OPT_KING_CACHE_LIM

  integer,parameter :: mbaseh = OPT_KING_HASH_BASE
  integer,parameter :: mcarrh = OPT_KING_HASH_CARRY
!!!_ + public
  character,parameter,public :: punct = '.'
!!!_ + static
  integer,            save :: cache_k(-1:lctb-1) = -1    ! index -1 for sentry
  integer,            save :: cache_a(0:lctb-1)  = -1
  character(len=lmdl),save :: cache_p(0:lctb-1)  = ' '
  integer,            save :: cache_n(0:lctb-1)  = -9    ! count of matches (source agent) or -1 (reference agent)
  integer,            save :: cache_x(0:lctb-1)  = -1    ! hash extends
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = PPP_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
#define __MDL__ 'k'
!!!_ + overload
!!!_ + interfaces
  public init, diag, finalize
  public get_king, set_king
  public diag_cache
  ! public reg_pattern
!!!_ + common interfaces
contains
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm)
    use TOUZA_Ppp_std,only: choice, ps_init=>init
    use TOUZA_Ppp_amng,only: pc_init=>init
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
          if (ierr.eq.0) call pc_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
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
    use TOUZA_Ppp_amng,only: pc_diag=>diag
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
             if (is_msglev_normal(lv)) call diag_cache(ierr, utmp)
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ps_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call pc_diag(ierr, utmp, levv=lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Ppp_std,only: ps_finalize=>finalize, choice
    use TOUZA_Ppp_amng,only: pc_finalize=>finalize
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
          if (ierr.eq.0) call pc_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call ps_finalize(ierr, utmp, levv=lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
  end subroutine finalize

!!!_ + diag subcontracts
!!!_  - diag_cache
  subroutine diag_cache &
       & (ierr, u)
    use MPI,only: MPI_Comm_rank, MPI_COMM_WORLD
    use TOUZA_Ppp_std,only: get_wni_safe, msg
    use TOUZA_Ppp_amng,only: lagent, inquire_agent
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer utmp
    integer jc
    integer irank
    character(len=lagent) :: agent

    ierr = 0
    utmp = get_logu(u, ulog)

    call get_wni_safe(ierr, irank=irank)

101 format('cache/king:', I0, 1x, I0, 1x, A, 1x, A, ':', I0, 2x, I0)
    if (irank.lt.0) then
       call msg('mpi deactivated', __MDL__, utmp)
    else
       do jc = 0, lctb - 1
          if (cache_n(jc).lt.-1) cycle
          call inquire_agent(ierr, iagent=cache_a(jc), name=agent)
          if (utmp.ge.0) then
             write(utmp, 101) irank, jc, trim(cache_p(jc)), trim(agent), cache_k(jc), cache_n(jc)
          else if (utmp.eq.-1) then
             write(*,    101) irank, jc, trim(cache_p(jc)), trim(agent), cache_k(jc), cache_n(jc)
          endif
       enddo
    endif
  end subroutine diag_cache

!!!_ + user interfaces
!!!_  & get_king - get KING under AREF agent
  subroutine get_king &
       & (ierr, king, mdl, aref, adef)
    use MPI,only: MPI_UNDEFINED
#  if HAVE_FORTRAN_MPI_MPI_BCAST
    use MPI,only: MPI_Group_translate_ranks
#  endif
    use TOUZA_Ppp_amng,only: query_agent, source_agent, inquire_agent
    implicit none
    integer,                  intent(out) :: ierr
    integer,                  intent(out) :: king
    character(len=*),         intent(in)  :: mdl
    character(len=*),         intent(in)  :: aref   ! KING reference agent
    character(len=*),optional,intent(in)  :: adef   ! KING definition agent  (same as AREF if null)

    integer jcache
    integer jaref,   jasrc,  jadef
    integer jgref,   jgsrc
    integer kref(1), ksrc(1)
    character(len=lmdl) :: pat

    ierr = 0
    king = -1
    jaref = query_agent(aref)   !  need to switch agent if necessary
    if (jaref.lt.0) then
       ierr = -1
       return
    endif
    ! search cache
    pat = trim(mdl) // punct
    jcache = cache_search(pat, jaref)
    if (jcache.ge.0) then
       king = cache_k(jcache)
       if (king.eq.MPI_UNDEFINED) king = -1
       return
    endif
    ! bind king (relative to source)
    jasrc = source_agent(jaref)
    if (ierr.eq.0) call inquire_agent(ierr, iagent=jasrc, igroup=jgsrc)
    if (ierr.eq.0) then
       if (present(adef)) then
          jadef = query_agent(adef, jaref)
          if (jadef.lt.0) then
             ierr = -1
             return
          endif
       else
          jadef = jaref
       endif
    endif
    if (ierr.eq.0) call bind_king(ierr, ksrc(1), pat, jasrc, jadef)

    ! get king (relative to AREF)
    if (ierr.eq.0) call inquire_agent(ierr, iagent=jaref, igroup=jgref)
    if (ierr.eq.0) call MPI_Group_translate_ranks(jgsrc, 1, ksrc, jgref, kref, ierr)
    if (ierr.eq.0) then
       king = kref(1)
       if (king.eq.MPI_UNDEFINED) king = -1
    endif
    ! store cache
    if (ierr.eq.0) then
       jcache = cache_store(pat, jaref, kref(1)) ! NOT king (to allow MPI_UNDEFINED)
       ! write(*, *) 'store ', pat, '/', aref, jaref, kref(1), '/', jasrc, ksrc(1)
       if (jcache.lt.0) ierr = -1
    endif

  end subroutine get_king

!!!_  - bind_king
  subroutine bind_king &
       & (ierr, king, pat, jasrc, jadef)
    use MPI,only: MPI_UNDEFINED
#  if HAVE_FORTRAN_MPI_MPI_BCAST
    use MPI,only: MPI_Group_translate_ranks
#  endif
    use TOUZA_Ppp_amng,only: query_agent, inquire_agent
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: king
    character(len=*),intent(in)  :: pat
    integer,         intent(in)  :: jasrc
    integer,         intent(in)  :: jadef

    integer jad
    integer jcache
    integer lp, lm

    ierr = 0
    lp = len_trim(pat)

    jcache = cache_match(pat, jasrc)
    if (jcache.lt.0) then
       lm = 0
       king = 0
       jad = jadef
    else
       ! find partial or full match
       lm = len_trim(cache_p(jcache))
       king = cache_k(jcache)
       cache_n(jcache) = max(0, cache_n(jcache)) + 1
       jad = jasrc
    endif

    do
       lm = lm + 1
       if (lm.gt.lp) exit
       jcache = cache_register(king, jad, jasrc, pat(1:lm))
       if (jcache.lt.0) then
          ierr = -1
          return
       endif
       ! write(*, *) 'bind', jasrc, pat(1:lm), '/', jadef, king, cache_k(jcache)
       cache_n(jcache) = max(0, cache_n(jcache)) + 1
    enddo
    if (jcache.ge.0) king = cache_k(jcache)
    return
  end subroutine bind_king

!!!_  - set_king
  subroutine set_king &
       & (ierr, king, pat, adef)
    use MPI,only: MPI_UNDEFINED
#  if HAVE_FORTRAN_MPI_MPI_BCAST
    use MPI,only: MPI_Group_translate_ranks
#  endif
    use TOUZA_Ppp_amng,only: query_agent, source_agent, inquire_agent
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: king
    character(len=*),intent(in)  :: pat
    character(len=*),intent(in)  :: adef   ! KING definition agent

    integer jadef, jasrc
    integer jcache

    jadef = query_agent(adef)   !  need to switch agent if necessary
    if (jadef.lt.0) then
       ierr = -1
       return
    endif
    jasrc = source_agent(jadef)

    if (cache_check(pat, jasrc)) then
       ierr = -1
       return
    endif
    jcache = cache_register(king, jadef, jasrc, pat)
    if (jcache.lt.0) then
       ierr = -1
    else
       cache_n(jcache) = max(0, cache_n(jcache))
    endif
    return
  end subroutine set_king

!!!_ + cache
!!!_  - cache_register
  integer function cache_register &
       & (king, iaref, iasrc, pat) &
       & result(n)
#  if HAVE_FORTRAN_MPI_MPI_BCAST
    use MPI,only: MPI_Group_translate_ranks
#  endif
    use TOUZA_Ppp_amng,only: inquire_agent
    implicit none
    integer,         intent(in)  :: king
    integer,         intent(in)  :: iaref
    integer,         intent(in)  :: iasrc
    character(len=*),intent(in)  :: pat

    integer jerr
    integer jgsrc,   jgref
    integer ksrc(1), kref(1)

    n = -1
    jerr = 0

    kref(1) = king

    if (jerr.eq.0) call inquire_agent(jerr, iagent=iasrc, igroup=jgsrc)
    if (jerr.eq.0) call inquire_agent(jerr, iagent=iaref, igroup=jgref)

    if (jerr.eq.0) call MPI_Group_translate_ranks(jgref, 1, kref, jgsrc, ksrc, jerr)
    ! write(*, *) 'register', pat, iasrc, ksrc, '/', iaref, kref
    if (jerr.eq.0) then
       n = cache_store(pat, iasrc, ksrc(1))
    endif
  end function cache_register

!!!_  - cache_search
  integer function cache_search &
       & (pat, iagent) &
       & result(n)
    implicit none
    character(len=*),intent(in) :: pat
    integer,         intent(in) :: iagent
    integer m, jcoff, jc
    n = -1
    if (iagent.lt.0) return

    jcoff = cache_hash(pat, iagent)

    do m = 0, cache_x(jcoff)
       jc = mod(jcoff + m, lctb)
       if (cache_p(jc).eq.pat) then
          if (cache_a(jc).eq.iagent) then
             n = jc
             exit
          endif
       endif
    enddo
    return
  end function cache_search

!!!_  - cache_match - search (partial) matching entry
  integer function cache_match &
       & (pat, iagent) &
       & result(n)
    implicit none
    character(len=*),intent(in) :: pat
    integer,         intent(in) :: iagent

    integer m, jcoff, jc
    integer jp
    n = -1
    if (iagent.lt.0) return

    outer: do jp = len_trim(pat), 1, -1
       jcoff = cache_hash(pat(1:jp), iagent)
       do m = 0, cache_x(jcoff)
          jc = mod(jcoff + m, lctb)
          if (cache_p(jc).eq.pat(1:jp)) then
             if (cache_a(jc).eq.iagent) then
                n = jc
                exit outer
             endif
          endif
       enddo
    enddo outer
    return
  end function cache_match

!!!_  - cache_check - is used check matching entry
  logical function cache_check &
       & (pat, iagent) &
       & result(b)
    implicit none
    character(len=*),intent(in) :: pat
    integer,         intent(in) :: iagent
    integer jc

    b = .true.
    if (iagent.lt.0) return

    jc = cache_search(pat, iagent)
    if (jc.ge.0) then
       if (cache_n(jc).gt.0) return
    endif

    b = .false.
    return
  end function cache_check

!!!_  - cache_store - store entry
  integer function cache_store &
       & (pat, iagent, king) &
       & result(n)
    implicit none
    character(len=*),intent(in) :: pat
    integer,         intent(in) :: iagent
    integer,         intent(in) :: king

    integer m, jcoff, jc

    n = -1
    jcoff = cache_hash(pat, iagent)
    do m = 0, lctb - 1
       jc = mod(jcoff + m, lctb)
       if (cache_x(jc).ge.0) cycle
       n = jc
       cache_a(n) = iagent
       cache_p(n) = pat
       cache_k(n) = king
       cache_n(n) = -1
       cache_x(jcoff) = max(m, cache_x(jcoff))
       return
    enddo
  end function cache_store

!!!_  - cache_hash
  integer function cache_hash &
       & (pat, iagent) &
       & result(n)
    implicit none
    character(len=*),intent(in) :: pat
    integer,         intent(in) :: iagent
    integer j
    n = iagent
    do j = 1, len_trim(pat)
       n = mod(n * mbaseh + ICHAR(pat(j:j)), lctb)
    enddo
    n = mod(n * mcarrh, lctb)
    return
  end function cache_hash

!!!_ + end TOUZA_Ppp_king
end module TOUZA_Ppp_king
!!!_@ test_ppp_king - test program
#if TEST_PPP_KING
program test_ppp_king
  use MPI
  use TOUZA_Ppp_king
  implicit none
  integer ierr
  integer icw
  integer ktest

101 format(A, ' = ', I0)
  call init(ierr)
  write(*, 101) 'INIT', ierr

  icw = MPI_COMM_WORLD
  ktest = 0

  call batch_test_king(ierr, icw, ktest)
  write(*, 101) 'TEST', ierr
  call diag(ierr)
  write(*, 101) 'DIAG', ierr
  call finalize(ierr)
  write(*, 101) 'FINAL', ierr
  stop
contains
  subroutine batch_test_king (ierr, icomm, ktest)
    use TOUZA_Ppp_amng,only:&
         & inquire_agent, &
         & new_agent_root,new_agent_family
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: icomm
    integer,intent(in)  :: ktest

    integer,parameter :: ltest = 32
    character(len=16) :: drvs(ltest)
    integer nself
    integer irank, nrank

    character(len=1) :: refs0(6) = (/'A', 'B', 'C', 'D', '@', '%'/)
    character(len=1) :: refs1(2) = (/'%', '/'/)


    ierr = 0
    if (ierr.eq.0) call new_agent_root(ierr, icomm)
    if (ierr.eq.0) call inquire_agent(ierr, irank=irank, nrank=nrank)
    if (ierr.eq.0) call set_drivers(nself, drvs, irank, nrank, ktest)
    if (ierr.eq.0) call new_agent_family(ierr, drvs(1:nself))

    if (ierr.eq.0) then
       call test_register(ierr, irank, 'qrs', '%', min(nrank - 1, 1))
       call test_register(ierr, irank, 'q',   '%', min(nrank - 1, 3))
       call test_register(ierr, irank, 'qt.', '%', min(nrank - 1, 4))

       call test_pattern(ierr,  irank, 'q',   '%', refs1(:)) ! 3
       call test_pattern(ierr,  irank, 'qrs', '%', refs1(:)) ! 1
       call test_pattern(ierr,  irank, 'qt',  '%', refs1(:)) ! 4
       call test_pattern(ierr,  irank, 'qtx', '%', refs1(:)) ! 3
       call test_pattern(ierr,  irank, 'qu',  '%', refs1(:)) ! 3
    endif
    if (ierr.eq.0) then
       call test_register(ierr, irank, 'yy',   '%', min(nrank - 1, 2)) ! error
       call test_pattern(ierr,  irank, 'yyyy', '%', refs1(:)) ! 2

       call test_register(ierr, irank, 'yyy',    '%', min(nrank - 1, 0)) ! error
       call test_register(ierr, irank, 'yyy.',   '%', min(nrank - 1, 0)) ! good
       call test_register(ierr, irank, 'y',      '%', min(nrank - 1, 0)) ! error
       call test_register(ierr, irank, 'y.',     '%', min(nrank - 1, 0)) ! good
       call test_register(ierr, irank, 'yyyy',   '%', min(nrank - 1, 0)) ! error
       call test_register(ierr, irank, 'yyyy.',  '%', min(nrank - 1, 0)) ! error
       ierr = 0
    endif
    if (ierr.eq.0) then
       call test_pattern(ierr, irank, 'a', 'A', refs0(:))
       call test_pattern(ierr, irank, 'b', 'B', refs0(:))
       call test_pattern(ierr, irank, 'c', 'C', refs0(:))
       call test_pattern(ierr, irank, 'l', '@', refs0(:))
    endif

  end subroutine batch_test_king

  subroutine test_register &
       (ierr, irank, pat, adef, king)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: irank
    character(len=*),intent(in)  :: pat
    character(len=*),intent(in)  :: adef
    integer,         intent(in)  :: king

    ierr = 0
101 format('set:', I0, 1x, A, 1x, I0)
    call set_king(ierr, king, pat, adef)
    write(*, 101) irank, trim(pat), ierr
  end subroutine test_register

  subroutine test_pattern &
       (ierr, irank, pat, adef, refs)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: irank
    character(len=*),intent(in)  :: pat
    character(len=*),intent(in)  :: adef
    character(len=*),intent(in)  :: refs(:)

    integer kinit(128)

    integer l
    integer jr
    integer king

101 format('test:', I0, &
         & 1x, '[', A, ':', A, ']', &
         & 1x, I0, 1x, I0, 2x, A, '=', I0, 1x, L1)
    l = 1
    do jr = 1, size(refs)
       call get_king(ierr, king, pat, refs(jr), adef)
       write(*, 101) irank, trim(pat), trim(adef), l, ierr, trim(refs(jr)), king
       kinit(jr) = king
    enddo
    l = 2
    do jr = 1, size(refs)
       call get_king(ierr, king, pat, refs(jr), adef)
       write(*, 101) irank, trim(pat), trim(adef), l, ierr, trim(refs(jr)), king, (king.eq.kinit(jr))
    enddo
  end subroutine test_pattern

  subroutine set_drivers(nself, drvs, irank, nrank, ktest)
    implicit none
    integer,         intent(out) :: nself
    character(len=*),intent(out) :: drvs(:)
    integer,         intent(in)  :: irank, nrank
    integer,         intent(in)  :: ktest
    integer j

    drvs(:) = ' '
    if (irank.lt.1) then
       drvs(1:1) = (/ 'A' /)
    else if (irank.lt.(1+2)) then
       drvs(1:1) = (/ 'B' /)
    else if (irank.lt.(1+2+3)) then
       drvs(1:1) = (/ 'C' /)
    else if (irank.lt.(1+2+3+4)) then
       drvs(1:1) = (/ 'D' /)
    else
       drvs(1:1) = (/ 'Z' /)
    endif
    nself = 0
    do j = 1, size(drvs)
       if (drvs(j).eq.' ') exit
       nself = nself + 1
    enddo

101 format('drivers: ', I0, 1x, I0, 1x, A)
    do j = 1, nself
       write(*, 101) irank, j, trim(drvs(j))
    enddo
  end subroutine set_drivers
end program test_ppp_king
#endif /* TEST_PPP_KING */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

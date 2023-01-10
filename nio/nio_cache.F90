!!!_! nio_cache.F90 - TOUZA/Nio cache-record extension
! Maintainer: SAITO Fuyuki
! Created: Nov 9 2022
#define TIME_STAMP 'Time-stamp: <2023/02/15 21:23:36 fuyuki nio_cache.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022,2023
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_nio.h"
!!!_* macros
#if HAVE_F2003_ALLOCATABLE_MEMBER
#  define _POINTER allocatable
#else  /* not HAVE_F2003_ALLOCATABLE_MEMBER */
#  define _POINTER pointer
#endif /* not HAVE_F2003_ALLOCATABLE_MEMBER */
!!!_@ TOUZA_Nio_cache - nio with cache
module TOUZA_Nio_cache
!!!_ = declaration
  use TOUZA_Nio_std,only: &
       & KI32, KI64, KDBL, KFLT, KIOFS, &
       & control_mode, control_deep, is_first_force, &
       & get_logu,     unit_global,  trace_fine,   trace_control
  use TOUZA_Nio_header,nh_init=>init, nh_diag=>diag, nh_finalize=>finalize
  implicit none
  private
!!!_  - public parameters
  integer,parameter :: lax = 3

  integer,parameter,public :: coll_default = 0
  integer,parameter,public :: coll_strict = -1
  integer,parameter,public :: coll_std    = 1    ! ignore DFMT ITEM DATE TIME TDUR TIME2 UTIM2 SIZE MISS AITMn ASTRn AENDn
  integer,parameter,public :: coll_basic  = 2    ! plus ignore TITL UNIT EDIT ETTL MEMO DMIN DMAX DIVS DIVL STYP [CIR]OPTN
  integer,parameter,public :: coll_nosign = 3    ! plus ignore [CM]DATE [CM]SIGN
  integer,parameter,public :: coll_nonum  = 4    ! plus ignore DSET FNUM DNUM

!!!_  - private parameter
  integer,parameter :: ucache = 16
!!!_  - types
  type var_t
     character(len=litem) :: item = ' '
     character(len=litem) :: unit = ' '
     character(len=litem) :: fmt  = ' '
     character(len=litem) :: co(lax) = ' '
     integer              :: jbgn(lax) = 0
     integer              :: jend(lax) = 0
     integer              :: flag
  end type var_t

  type group_t
     integer :: nvar = -1                   ! size of v
     character(len=litem) :: h(nitem)       ! header
     type(var_t),pointer :: v(:) => NULL()
     integer :: nrec = -1
     character(len=litem),pointer :: d(:) => NULL()    ! date [rec]
     character(len=litem),pointer :: t(:) => NULL()    ! time [rec]
     integer(kind=KIOFS),pointer  :: o(:, :) => NULL() ! record offset [rec, var]
     integer(kind=KIOFS),pointer  :: l(:, :) => NULL() ! record size   [rec, var]
  end type group_t

  type cache_t
     integer :: ngrp = -1
     type(group_t),pointer :: g(:)
  end type cache_t
!!!_  - static
  integer,save :: cache_rev = 0
  character(len=litem),save :: DSET_specials(9) = &
       & (/ 'AXLOC  ', 'AXWGT  ', 'IAXLOC ', 'IAXWGT ', &
       &    'CAXLOC ', 'CAXWGT ', 'CIAXLOC', 'CIAXWGT', &
       &    'CCAXLOC'  /)
  ! CCAXLOC is used in DIUR02 of official gtool, might be a bug.

  character(len=4),save :: dup_sep = '~'
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = NIO_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
#define __MDL__ 'c'
!!!_  - interfaces
!!!_  - public procedures
  public init, diag, finalize
  public init_group
  public show_cache
  public cache_scan_file, cache_settle
  public cache_store_v0

  public cache_t
!!!_  - public shared
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init &
       & (ierr, u, levv, mode, stdv, icomm, sep)
    use TOUZA_Nio_std,   only: ns_init=>init, choice, get_size_bytes, KDBL
    use TOUZA_Nio_header,only: nh_init=>init
    use TOUZA_Nio_record,only: nr_init=>init
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv, mode, stdv
    integer,         intent(in),optional :: icomm
    character(len=*),intent(in),optional :: sep
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
          if (ierr.eq.0) call ns_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
          if (ierr.eq.0) call nh_init(ierr, u=ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_init(ierr, u=ulog, levv=lv, mode=lmd)
       endif
       if (present(sep)) then
          dup_sep = sep
       endif
       if (init_counts.eq.0) then
          continue
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Nio_std,   only: ns_diag=>diag, choice, msg, is_msglev_normal, is_msglev_info
    use TOUZA_Nio_header,only: nh_diag=>diag
    use TOUZA_Nio_record,only: nr_diag=>diag
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
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __MDL__, u=utmp)
             if (is_msglev_normal(lv)) call msg('(''duplicate separator = '', A)', (/dup_sep/), __MDL__, utmp)
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ns_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nh_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_diag(ierr, utmp, levv=lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Nio_std,   only: ns_finalize=>finalize, choice
    use TOUZA_Nio_header,only: nh_finalize=>finalize
    use TOUZA_Nio_record,only: nr_finalize=>finalize
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
          if (ierr.eq.0) call nh_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call ns_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_finalize(ierr, utmp, levv=lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize
!!!_  - init subcontracts
!!!_ + derived-type managers
!!!_  - init_group
  subroutine init_group (ierr, grp, recs, vars)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,      intent(out)         :: ierr
    type(group_t),intent(inout)       :: grp
    integer,      intent(in),optional :: recs, vars
    integer lr
    integer lv

    ierr = 0
    grp%nvar = 0
    grp%h(:) = ' '
    grp%nrec = 0
    ! lv = choice(16, vars) ! hard-coded
    ! lr = choice(12, recs) ! hard-coded
    lv = choice(1, vars) ! hard-coded
    lr = choice(1, recs) ! hard-coded
    allocate(grp%v(0:lv-1), &
         &   grp%d(0:lr-1),        grp%t(0:lr-1), &
         &   grp%o(0:lr-1,0:lv-1), grp%l(0:lr-1,0:lv-1), &
         &   STAT=ierr)
  end subroutine init_group
!!!_  - add_members
  subroutine add_members &
       & (ierr, grp, recs, vars)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,      intent(out)         :: ierr
    type(group_t),intent(inout)       :: grp
    integer,      intent(in),optional :: recs   ! number of records to add
    integer,      intent(in),optional :: vars   ! number of variables to add
    integer mr, nr, lr
    integer mv, nv, lv
    type(var_t),pointer :: tmpv(:)
    character(len=litem),pointer :: tmpd(:),   tmpt(:)
    integer(kind=KIOFS),pointer  :: tmpo(:,:), tmpl(:, :)

    ierr = 0
    mr = choice(0, recs)
    mv = choice(0, vars)
    nv = size(grp%v)
    lv = nv + mv
    if (mv.gt.0) then
       allocate(tmpv(0:lv-1), STAT=ierr)
       if (ierr.eq.0) then
          tmpv(0:nv-1) = grp%v(0:nv-1)
          deallocate(grp%v, STAT=ierr)
       endif
       if (ierr.eq.0) grp%v => tmpv
    endif
    if (mv.gt.0.or.mr.gt.0) then
       nr = size(grp%d)
       lr = nr + mr
       allocate(tmpd(0:lr-1), tmpt(0:lr-1), tmpo(0:lr-1,0:lv-1), tmpl(0:lr-1,0:lv-1), &
            &   STAT=ierr)
       if (ierr.eq.0) then
          tmpd(0:nr-1) = grp%d(0:nr-1)
          tmpt(0:nr-1) = grp%t(0:nr-1)
          tmpo(0:nr-1, 0:nv-1) = grp%o(0:nr-1, 0:nv-1)
          tmpl(0:nr-1, 0:nv-1) = grp%l(0:nr-1, 0:nv-1)

          tmpo(nr:lr-1, 0:lv-1) = -1
          tmpl(nr:lr-1, 0:lv-1) = -1
          tmpo(0:lr-1, nv:lv-1) = -1
          tmpl(0:lr-1, nv:lv-1) = -1
          deallocate(grp%d, grp%t, grp%o, grp%l, STAT=ierr)
       endif
       if (ierr.eq.0) then
          grp%d => tmpd
          grp%t => tmpt
          grp%o => tmpo
          grp%l => tmpl
       endif
    endif
  end subroutine add_members
!!!_  - add_record
  subroutine add_record &
       & (ierr, grp, d, t, o, jvar)
    implicit none
    integer,            intent(out)   :: ierr
    type(group_t),      intent(inout) :: grp
    character(len=*),   intent(in)    :: d
    character(len=*),   intent(in)    :: t
    integer(kind=KIOFS),intent(in)    :: o
    integer,            intent(in)    :: jvar
    integer jr, mr
    integer jtgt

    ierr = 0
    mr = 16
    jtgt = -1
    do jr = 0, grp%nrec - 1
       if (grp%d(jr).eq.d.and.grp%t(jr).eq.t) then
          jtgt = jr
          exit
       endif
    enddo
    if (jtgt.lt.0) then
       jtgt = max(0, grp%nrec)
       grp%nrec = grp%nrec + 1
       if (jtgt.ge.size(grp%d)) call add_members(ierr, grp, recs=mr)
       if (ierr.eq.0) then
          grp%d(jtgt) = d
          grp%t(jtgt) = t
       endif
    endif
    if (ierr.eq.0) then
       grp%o(jtgt, jvar) = o
       grp%l(jtgt, jvar) = 0  ! wait
    endif
  end subroutine add_record

!!!_ + cache
!!!_  & show_cache
  subroutine show_cache(ierr, c, tag, u, levv)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    type(cache_t),   intent(in)          :: c
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer utmp, lv
    integer jg
    character(len=128) :: ttmp
    ierr = 0
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)
101 format(A, '/', I0)
102 format('group/', I0)
    do jg = 0, c%ngrp - 1
       if (present(tag)) then
          write(ttmp, 101) trim(tag), jg
       else
          write(ttmp, 102) jg
       endif
       if (ierr.eq.0) call show_group(ierr, c%g(jg), ttmp, utmp, lv)
    enddo
  end subroutine show_cache

!!!_  & show_group
  subroutine show_group(ierr, grp, tag, u, levv)
    use TOUZA_Nio_std,only: choice, join_list, is_msglev_DETAIL, is_msglev_INFO
    implicit none
    integer,         intent(out)         :: ierr
    type(group_t),   intent(in)          :: grp
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer utmp, lv
    integer jvb, jve, nv, jvi
    integer,parameter :: lcol = litem + 1
    integer,parameter :: mv = 4
    integer,parameter :: lline = lcol * (mv + 4)
    character(len=128) :: ttmp
    character(len=lcol) :: cbufs(0:mv-1)
    character(len=lline) :: line
    character(len=litem*2+1) :: dt
    integer jr, jc

    ierr = 0
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)
101 format(A, 1x, I0, ':', I0, 1x, A)
211 format(A, 1x, I0, ' [', A, '] ', A)
201 format(Z8.8, '+', Z0)
202 format(A, 1x, A)
111 format(A, '/', I0, ':', I0)
121 format(A, 1x, I0, 1x, A)
    ! write(*, *) grp%nvar, grp%nrec
    if (present(tag)) then
       ttmp = tag
    else
       ttmp = 'cache'
    endif
    if (is_msglev_DETAIL(levv-1)) then
       if (ierr.eq.0) call show_header(ierr, grp%h, tag=ttmp, u=utmp, lev=levv)
    endif
    do jvb = 0, grp%nvar - 1, mv
       jve = min(jvb + mv, grp%nvar)
       nv = jve - jvb
       cbufs(0:nv-1) = grp%v(jvb:jve-1)%item
       if (ierr.eq.0) call join_list(ierr, line, cbufs(0:nv-1))
       ! write(*, *) jvb, jve, cbufs(0:nv-1), ierr, trim(line)
       if (ierr.eq.0) then
          if (utmp.ge.0) then
             write(utmp, 101) trim(ttmp), jvb, jve, trim(line)
          else if (utmp.eq.-1) then
             write(*, 101) trim(ttmp), jvb, jve, trim(line)
          endif
       endif
       do jc = 1, 3
          if (ierr.eq.0) then
             do jvi = 0, nv - 1
                write(cbufs(jvi), 111) trim(grp%v(jvb+jvi)%co(jc)), grp%v(jvb+jvi)%jbgn(jc), grp%v(jvb+jvi)%jend(jc)
             enddo
             call join_list(ierr, line, cbufs(0:nv-1))
          endif
          if (ierr.eq.0) then
             if (utmp.ge.0) then
                write(utmp, 121) trim(ttmp), jc, trim(line)
             else if (utmp.eq.-1) then
                write(*, 121) trim(ttmp), jc, trim(line)
             endif
          endif
       enddo
       if (is_msglev_DETAIL(lv)) then
          if (ierr.eq.0) then
             do jr = 0, grp%nrec - 1
                do jvi = 0, nv - 1
                   write(cbufs(jvi), 201) grp%o(jr, jvb+jvi), grp%l(jr, jvb+jvi)
                enddo
                if (ierr.eq.0) call join_list(ierr, line, cbufs(0:nv-1))
                write(dt, 202) trim(adjustl(grp%t(jr))), trim(adjustl(grp%d(jr)))
                if (utmp.ge.0) then
                   write(utmp, 211) trim(ttmp), jr, trim(dt), trim(line)
                else if (utmp.eq.-1) then
                   write(*,    211) trim(ttmp), jr, trim(dt), trim(line)
                endif
             enddo
          endif
       endif
    enddo

  end subroutine show_group

!!!_  & cache_scan_file
  subroutine cache_scan_file &
       & (ierr, c, u, flag, levv)
    use TOUZA_Nio_std,only: choice, condop, is_error_match
    use TOUZA_Nio_std,only: msg, is_msglev_DETAIL
    use TOUZA_Nio_record,only: nio_read_header, nio_skip_records
    implicit none
    integer,         intent(out)   :: ierr
    type(cache_t),   intent(inout) :: c
    integer,         intent(in)    :: u
    integer,optional,intent(in)    :: flag
    integer,optional,intent(in)    :: levv

    integer jdrec
    integer lv
    integer jg, lg, ng, mg
    integer jv, mv, mvdef
    integer jr, mr, mrdef
    logical newv, newr
    type(group_t),pointer :: grp(:), gtmp(:)
    character(len=litem) :: h(nitem)
    integer rect
    integer j
    integer(kind=KIOFS) :: jpos, msize

    ierr = 0
    lv = choice(lev_verbose, levv)

    ng = 0
    mg = 8
    lg = 24
    mvdef = 16
    mrdef = 12
    jr = -1
    jv = -1
    newr = .FALSE.
    jg = -1
    jdrec = 0

    if (ierr.eq.0) rewind(UNIT=u, IOSTAT=ierr)
    if (ierr.eq.0) allocate(grp(0:lg-1), STAT=ierr)

    do
       if (ierr.ne.0) exit
       if (ierr.eq.0) inquire(UNIT=u, POS=jpos, IOSTAT=ierr)
       if (ierr.eq.0) call nio_read_header(ierr, h, rect, u)
       if (is_msglev_DETAIL(lv)) then
          call msg('(''record = '', I0, 1x, I0)', (/jdrec, int(jpos-1)/), __MDL__)
       endif
       if (ierr.eq.0) call collate_header(ierr, jg, grp, ng, h, flag)
       if (ierr.eq.0) then
          if (jg.lt.0) then
             jg = ng
             if (ng.ge.lg) then
                ! write(*, *) jg, mg, ng, lg
                lg = lg + mg
                allocate(gtmp(0:lg-1), STAT=ierr)
                if (ierr.eq.0) then
                   gtmp(0:ng-1)%nvar = grp(0:ng-1)%nvar
                   gtmp(0:ng-1)%nrec = grp(0:ng-1)%nrec
                   do j = 0, ng - 1
                      gtmp(j)%h(:) = grp(j)%h(:)
                      gtmp(j)%v => grp(j)%v
                      gtmp(j)%d => grp(j)%d
                      gtmp(j)%t => grp(j)%t
                      gtmp(j)%o => grp(j)%o
                      gtmp(j)%l => grp(j)%l
                   enddo
                   deallocate(grp, STAT=ierr)
                endif
                if (ierr.eq.0) grp => gtmp
                ! do j = 0, ng - 1
                !    write(*, *) 'grp', j, size(grp(j)%v)
                ! enddo
             endif
             ng = ng + 1
             if (ierr.eq.0) call init_group(ierr, grp(jg))
             if (ierr.eq.0) grp(jg)%h(:) = h(:)
             if (is_msglev_DETAIL(lv)) then
                call msg('(''group = '', I0)', (/jg/), __MDL__)
             endif
          endif
       endif
       if (ierr.eq.0) call nio_skip_records(ierr, 1, u, head=h, krect=rect)
       if (ierr.eq.0) inquire(UNIT=u, POS=msize, IOSTAT=ierr)
       if (ierr.eq.0) call search_var(ierr, jv, grp(jg), h)
       if (ierr.eq.0) call search_rec(ierr, jr, grp(jg), h)
       ! write(*, *) 'search', ierr, jg, jr, jv, grp(jg)%nrec, grp(jg)%nvar
       if (ierr.eq.0) then
          newv = jv.lt.0
          newr = jr.lt.0
          if (.not.newv .and. .not.newr) then
             if (grp(jg)%o(jr, jv).ge.0) then
                call msg('(''dup = '', I0, 1x, I0)', (/jv, jr/), __MDL__)
                newv = .true.
             endif
          endif
          if (newr) then
             jr = grp(jg)%nrec
             grp(jg)%nrec = grp(jg)%nrec + 1
             if (is_msglev_DETAIL(lv)) then
                call msg('(''rec = '', I0)', (/jr/), __MDL__)
             endif
          endif
          if (newv) then
             jv = grp(jg)%nvar
             grp(jg)%nvar = grp(jg)%nvar + 1
             if (is_msglev_DETAIL(lv)) then
                call msg('(''var = '', I0)', (/jv/), __MDL__)
             endif
          endif
          mv = condop(jv.ge.size(grp(jg)%v), mvdef, 0)
          mr = condop(jr.ge.size(grp(jg)%d), mrdef, 0)
          ! write(*, *) 'new', ierr, jg, jr, jv, newr, newv, mr, mv
          if (mr.gt.0.or.mv.gt.0) then
             call add_members(ierr, grp(jg), mr, mv)
          endif
       endif
       if (ierr.eq.0) then
          if (newv) call new_var(ierr, grp(jg), jv, h)
          ! write(*, *) grp(jg)%v(:)
       endif
       if (ierr.eq.0) then
          if (newr) call new_rec(ierr, grp(jg), jr, h)
       endif
       if (ierr.eq.0) then
          msize = msize - jpos
          jpos = jpos - 1
       endif
       if (ierr.eq.0) then
          grp(jg)%o(jr, jv) = jpos
          grp(jg)%l(jr, jv) = msize
          ! write(*, *) 'scan', ierr, jg, jr, jv, jpos, msize
       endif
       jdrec = jdrec + 1
    enddo
    if (is_error_match(ierr, ERR_EOF)) ierr = 0
    if (ierr.eq.0) then
       c%ngrp = ng
       c%g => grp
    else
       c%ngrp = -1
       c%g => NULL()
    endif
  end subroutine cache_scan_file

!!!_  - cache_settle
  subroutine cache_settle(ierr, c)
    implicit none
    integer,      intent(out)   :: ierr
    type(cache_t),intent(inout) :: c
    integer jg, jvoff
    ierr = 0
    jvoff = 0
    do jg = 0, c%ngrp - 1
       call settle_group(ierr, c%g(jg), jvoff, jg)
    enddo
  end subroutine cache_settle

!!!_  - settle_group
  subroutine settle_group(ierr, grp, jvoff, jgrp)
    use TOUZA_Nio_record,only: put_header_cprop
    use TOUZA_Nio_header,only: put_item, hi_ITEM, hi_DFMT, hi_TITL1, hi_ETTL1, hi_UNIT
    implicit none
    integer,      intent(out)   :: ierr
    type(group_t),intent(inout) :: grp
    integer,      intent(inout) :: jvoff
    integer,      intent(in)    :: jgrp
    integer jv,  jt,  jc
    integer jvb, jve
    integer jrb, jre
    integer jgb, jge
    integer jerr
    integer ls
    character(len=litem*2) :: str

    ierr = 0
    ls = max(1, len_trim(dup_sep))
    jvb = jvoff
    jve = jvb + grp%nvar
    if (ierr.eq.0) call put_header_cprop(ierr, grp%h, 'VARIABLES', (/jvb + 1, jve/), 1)
    jrb = 0
    jre = grp%nrec
    if (ierr.eq.0) call put_header_cprop(ierr, grp%h, 'RECORDS', (/jrb + 1, jre/), 2)
    if (ierr.eq.0) then
       jgb = jgrp
       jge = jgb + 1
       call put_header_cprop(ierr, grp%h, 'GROUPS', (/jgb + 1, jge/), 3)
    endif
    if (ierr.eq.0) call put_item(ierr, grp%h, ' ', hi_TITL1, 0)
    if (ierr.eq.0) call put_item(ierr, grp%h, ' ', hi_UNIT)
    if (ierr.eq.0) call put_item(ierr, grp%h, ' ', hi_ETTL1, 0)
    if (ierr.eq.0) then
101    format('CACHE', I0)
       write(str, 101) cache_rev
       call put_item(ierr, grp%h, str, hi_ITEM)
    endif
    if (ierr.eq.0) then
102    format('XA', I0)
       write(str, 102) ucache
       call put_item(ierr, grp%h, str, hi_DFMT)
    endif
201 format(A, A, I0)
    do jv = 0, grp%nvar - 1
       if (index(grp%v(jv)%item, dup_sep(1:ls)).gt.0) cycle
       if (ANY(grp%v(jv)%item.eq.grp%v(jv+1:grp%nvar-1)%item)) then
          jc = 0
          do jt = jv + 1, grp%nvar - 1
             if (grp%v(jv)%item.eq.grp%v(jt)%item) then
                jc = jc + 1
                write(grp%v(jt)%item, 201, IOSTAT=jerr) trim(grp%v(jv)%item), dup_sep(1:ls), jc
             endif
          enddo
          jc = 0
          write(str, 201, IOSTAT=jerr) trim(grp%v(jv)%item), dup_sep(1:ls), jc
          grp%v(jv)%item = trim(str)
       endif
    enddo
    jvoff = jve
  end subroutine settle_group

!!!_  - collate_header
  subroutine collate_header &
       & (ierr, jgrp, grp, ngrp, head, flag, emask)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_header
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: jgrp
    type(group_t),   intent(in)  :: grp(0:*)
    character(len=*),intent(in)  :: head(*)
    integer,         intent(in)  :: ngrp
    integer,optional,intent(in)  :: flag
    logical,optional,intent(in)  :: emask(*)

    logical :: msk(nitem)
    integer j
    integer f
#if TEST_NIO_CACHE > 1
    integer ji
#endif
    ierr = 0

    if (present(emask)) then
       msk(1:nitem) = emask(1:nitem)
    else
       f = choice(coll_default, flag)
       if (f.eq.coll_default) f = coll_nosign
       msk(:) = .FALSE.
       if (f.ge.coll_std) then
          msk(hi_DFMT) = .TRUE.
          msk(hi_ITEM) = .TRUE.
          msk(hi_DATE) = .TRUE.
          msk(hi_TIME) = .TRUE.
          msk(hi_TDUR) = .TRUE.
          msk(hi_TIME2) = .TRUE.
          msk(hi_UTIM2) = .TRUE.
          msk(hi_SIZE) = .TRUE.
          msk(hi_MISS) = .TRUE.
          msk(hi_AITM1) = .TRUE.
          msk(hi_AITM2) = .TRUE.
          msk(hi_AITM3) = .TRUE.
          msk(hi_ASTR1) = .TRUE.
          msk(hi_ASTR2) = .TRUE.
          msk(hi_ASTR3) = .TRUE.
          msk(hi_AEND1) = .TRUE.
          msk(hi_AEND2) = .TRUE.
          msk(hi_AEND3) = .TRUE.
       endif
       if (f.ge.coll_basic) then
          msk(hi_UNIT) = .TRUE.
          msk(hi_TITL1:hi_TITL2) = .TRUE.
          msk(hi_EDIT1:hi_EDIT8) = .TRUE.
          msk(hi_ETTL1:hi_ETTL8) = .TRUE.
          msk(hi_MEMO1:hi_MEMO10) = .TRUE.
          msk(hi_DMIN) = .TRUE.
          msk(hi_DMAX) = .TRUE.
          msk(hi_DIVS) = .TRUE.
          msk(hi_DIVL) = .TRUE.
          msk(hi_STYP) = .TRUE.
          msk(hi_COPTN) = .TRUE.
          msk(hi_IOPTN) = .TRUE.
          msk(hi_ROPTN) = .TRUE.
       endif
       if (f.ge.coll_nosign) then
          msk(hi_CDATE) = .TRUE.
          msk(hi_CSIGN) = .TRUE.
          msk(hi_MDATE) = .TRUE.
          msk(hi_MSIGN) = .TRUE.
       endif
       if (f.ge.coll_nonum) then
          msk(hi_DSET) = .TRUE.
          msk(hi_FNUM) = .TRUE.
          msk(hi_DNUM) = .TRUE.
       endif
    endif

    jgrp = -1
    do j = 0, ngrp - 1
#if TEST_NIO_CACHE > 1
       do ji = 1, nitem
          if (grp(j)%h(ji).ne.head(ji)) then
             write(*, *) 'collate', j, ji, &
                  & '[' // trim(grp(j)%h(ji)) // ']', &
                  & '[' // trim(head(ji)) // ']', msk(ji)
          endif
       enddo
#endif
       if (ALL(grp(j)%h(1:nitem).eq.head(1:nitem) .or. msk(1:nitem))) then
          jgrp = j
          exit
       endif
    enddo
  end subroutine collate_header

!!!_  - new_var
  subroutine new_var &
       & (ierr, grp, jvar, head)
    use TOUZA_Nio_std,only: choice, parse_number
    use TOUZA_Nio_record,only: get_header_cprop
    use TOUZA_Nio_header
    implicit none
    integer,         intent(out)   :: ierr
    type(group_t),   intent(inout) :: grp
    integer,         intent(in)    :: jvar
    character(len=*),intent(in)    :: head(*)
    integer jc
    character(len=litem) :: name
    integer irange(2)

    ierr = 0
    ! write(*, *) 'new_var', jvar
    if (ierr.eq.0) then
       grp%v(jvar)%item =  head(hi_ITEM)
       grp%v(jvar)%unit =  head(hi_UNIT)
       do jc = 1, 3
          call get_header_cprop(name, irange, head, jc)
          grp%v(jvar)%co(jc) = trim(name)
          grp%v(jvar)%jbgn(jc) = irange(1)
          grp%v(jvar)%jend(jc) = irange(2)
       enddo
    endif
  end subroutine new_var

!!!_  - new_rec
  subroutine new_rec &
       & (ierr, grp, jrec, head)
    use TOUZA_Nio_header
    implicit none
    integer,         intent(out)   :: ierr
    type(group_t),   intent(inout) :: grp
    integer,         intent(in)    :: jrec
    character(len=*),intent(in)    :: head(*)
    ierr = 0
    grp%d(jrec) = head(hi_DATE)
    grp%t(jrec) = head(hi_TIME)
  end subroutine new_rec

!!!_  - search_var
  subroutine search_var &
       & (ierr, jvar, grp, head)
    use TOUZA_Nio_record,only: get_header_cprop
    use TOUZA_Nio_header
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: jvar
    type(group_t),   intent(in)  :: grp
    character(len=*),intent(in)  :: head(*)
    integer j, jc
    character(len=litem) :: name
    integer irange(2)
    ierr = 0
    jvar = -1
    loop_var: do j = 0, grp%nvar - 1
       ! write(*, *) 'search_var', j, trim(grp%v(j)%item)
       if (grp%v(j)%item.ne.head(hi_ITEM)) cycle loop_var
       if (grp%v(j)%unit.ne.head(hi_UNIT)) cycle loop_var
       do jc = 1, 3
          call get_header_cprop(name, irange, head, jc)
          if (grp%v(j)%co(jc).ne.name) cycle loop_var
          if (grp%v(j)%jbgn(jc).ne.irange(1)) cycle loop_var
          if (grp%v(j)%jend(jc).ne.irange(2)) cycle loop_var
       enddo
       jvar = j
       return
    enddo loop_var
  end subroutine search_var

!!!_  - search_rec
  subroutine search_rec &
       & (ierr, jrec, grp, head)
    use TOUZA_Nio_header
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: jrec
    type(group_t),   intent(in)  :: grp
    character(len=*),intent(in)  :: head(*)
    integer j
    ierr = 0
    jrec = -1
    loop_rec: do j = 0, grp%nrec - 1
       if (grp%d(j).ne.head(hi_DATE)) cycle loop_rec
       if (grp%t(j).ne.head(hi_TIME)) cycle loop_rec
       jrec = j
       return
    enddo loop_rec
  end subroutine search_rec

!!!_ + cached file manager
!!!_  & cache_inquire
!!!_  & cache_inquire_variable
!!!_  & cache_inquire_dimension
!!!_ + cache table manager
!!!_ + user interfaces
!!!_  & cache_open
!!!_  & cache_close
!!!_  & cache_read_header
!!!_  & cache_write_header
!!!_  & cache_read_data
!!!_  & cache_write_data
!!!_  & cache_verify_record
!!!_  & cache_read_record
!!!_  & cache_create_record
!!!_  & cache_write_record
!!!_  & cache_search_record
!!!_  & cache_store_v0 - copy header entry to cache (v0)
  subroutine cache_store_v0 &
       & (ierr, nentr, cache, head, ofs, lb, swap)
    implicit none
    integer,            intent(out) :: ierr
    integer,            intent(out) :: nentr
    character(len=*),   intent(out) :: cache(*)
    character(len=*),   intent(in)  :: head(*)
    integer(KIND=KIOFS),intent(in)  :: ofs, lb
    logical,            intent(in)  :: swap
    integer,parameter :: ver = 0
    integer jc, jh

    ierr = 0
    jc = 1
    do
       jh = c2hitem(jc, ver)
       if (jh.lt.-9) exit
       if (jh.ge.0) cache(jc) = head(jh)
       jc = jc + 1
    enddo
    nentr = jc - 1
    call store_cache_lset(ierr, cache(1), ofs, lb, swap)
    call store_cache_aset(ierr, cache(2), head(hi_ASTR1), head(hi_AEND1), swap)
    call store_cache_aset(ierr, cache(3), head(hi_ASTR2), head(hi_AEND2), swap)
    call store_cache_aset(ierr, cache(4), head(hi_ASTR3), head(hi_AEND3), swap)

  end subroutine cache_store_v0

!!!_  & c2hitem()
  integer function c2hitem(idx, ver) result(n)
    implicit none
    integer,intent(in) :: idx
    integer,intent(in) :: ver
    integer j
    integer,parameter :: nv0 = 16
    integer,parameter :: xv0(0:nv0+1) = &
         & (/-999, &
         &   -1,      -1,      -1,      -1,      hi_AITM1, hi_AITM2, hi_AITM3, hi_ITEM, &
         &   hi_DSET, hi_DFMT, hi_DATE, hi_TIME, hi_UTIM,  hi_TDUR,  -1,       -1,      &
         &   -999/)
    if (ver.eq.0) then
       j = min(max(0, idx), nv0+1)
       n = xv0(j)
    else
       n = -999
    endif
  end function c2hitem

!!!_  & c2hitem_vtype ()
  integer function c2hitem_vtype(idx, sub, ver) result(n)
    implicit none
    integer,intent(in) :: idx
    integer,intent(in) :: sub   ! record type (0=common 1=time)
    integer,intent(in) :: ver
    integer j
    integer,parameter :: nv0 = 8
    integer,parameter :: xv0(0:nv0+1) = &
         & (/-999,     &
         &   hi_DFMT,  -1,       -1,       -1, &
         &   hi_AITM1, hi_AITM2, hi_AITM3, -1, &
         &   -999/)
    integer,parameter :: xv1(0:nv0+1) = &
         & (/-999,     &
         &   -1,       hi_DATE, hi_TIME,  hi_TDUR, &
         &   hi_TIME2, hi_UTIM, hi_UTIM2, -1,      &
         &   -999/)
    if (ver.eq.0) then
       j = min(max(0, idx), nv0+1)
       if (sub.eq.0) then
          n = xv0(j)
       else
          n = xv1(j)
       endif
    else
       n = -999
    endif
  end function c2hitem_vtype

!!!_  & store_cache_lset
  subroutine store_cache_lset &
       & (ierr, centr, l0, l1, swap)
    use TOUZA_Std,only: sus_eswap
    implicit none
    integer,            intent(out) :: ierr
    character(len=*),   intent(out) :: centr
    integer(kind=KIOFS),intent(in)  :: l0, l1
    logical,            intent(in)  :: swap
    integer(kind=KI64)  :: lbuf(2)

    ierr = 0
    if (swap) then
       lbuf(1) = sus_eswap(l0)
       lbuf(2) = sus_eswap(l1)
    else
       lbuf(1) = l0
       lbuf(2) = l1
    endif
    centr = ' '
    centr(1:8)  = transfer(lbuf(1), centr(1:8))
    centr(9:16) = transfer(lbuf(2), centr(9:16))

  end subroutine store_cache_lset

!!!_  & store_cache_aset
  subroutine store_cache_aset &
       & (ierr, centr, a0, a1, swap)
    use TOUZA_Std,only: parse_number
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: centr
    character(len=*),intent(in)  :: a0, a1
    logical,         intent(in)  :: swap
    integer n, jerr
    integer(kind=KI64)  :: lbuf(2)

    call parse_number(jerr, n, a0)
    if (jerr.ne.0) n = -1
    lbuf(1) = n
    call parse_number(jerr, n, a1)
    if (jerr.ne.0) n = -1
    lbuf(2) = n
    call store_cache_lset(ierr, centr, lbuf(1), lbuf(2), swap)

  end subroutine store_cache_aset

!!!_ + end module
end module TOUZA_Nio_cache

!!!_@ test_nio_cache - test program
#ifdef TEST_NIO_CACHE
program test_nio_cache
  use TOUZA_Std,only: parse, get_param, arg_diag, arg_init, KIOFS
  use TOUZA_Std,only: sus_open
  use TOUZA_Nio_header,only: nitem, litem
  use TOUZA_Nio_record,only: get_default_header
  use TOUZA_Nio_cache
  implicit none
  integer ierr
  integer jarg
  integer nentr, j
  character(len=litem) :: ch(nitem)
  character(len=litem) :: hd(nitem)
  integer(kind=KIOFS) :: ofs, lb
  integer,parameter :: lpath = 256
  character(len=lpath) :: file

  type(cache_t) :: c
  integer u

  ierr = 0
  jarg = 0
101 format(A,' = ', I0)
  call init(ierr, stdv=-9)
  if (ierr.eq.0) call diag(ierr, levv=+9)
  if (ierr.eq.0) call arg_init(ierr, levv=-9)
  if (ierr.eq.0) call parse(ierr)
  if (ierr.eq.0) call arg_diag(ierr)

  if (ierr.eq.0) then
     jarg = jarg + 1
     call get_param(ierr, file, jarg, ' ')
     if (file.eq.' ') then
        write(*, *) 'need file to test.'
        ierr = -1
     endif
  endif
  u = 10
  if (ierr.eq.0) then
     call sus_open(ierr, u, file, ACTION='R', STATUS='O')
  endif
  ! if (ierr.eq.0) call cache_scan_file(ierr, c, u, coll_std, levv=+2)
  if (ierr.eq.0) call cache_scan_file(ierr, c, u, levv=+2)
  if (ierr.eq.0) call cache_settle(ierr, c)
  if (ierr.eq.0) call show_cache(ierr, c, tag='cache', levv=+2)

  ! if (ierr.eq.0) call get_default_header(hd)
  ! ofs = -123
  ! lb  = 456
  ! if (ierr.eq.0) call cache_store_v0(ierr, nentr, ch, hd, ofs, lb, .true.)

  if (ierr.eq.0) call finalize(ierr)
  write(*, 101) 'FINAL', ierr
  stop
contains
end program test_nio_cache

#endif /* TEST_NIO_CACHE */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

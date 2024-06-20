!!!_! convoy_util.F90 - TOUZA/Jmz/convoy utilities and parameters
! Maintainer: SAITO Fuyuki
! Created: Apr 10 2024
#define TIME_STAMP 'Time-stamp: <2024/06/28 07:16:04 fuyuki convoy_util.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2024
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "jmz_config.h"
#endif
#include "jmz.h"
!!!_@ TOUZA/Jmz/convoy_util - utilities and parameters
module convoy_util
!!!_ + Declaration
!!!_  - modules
  use Jmz_param
  use Jmz_base
  use TOUZA_Nio,only: litem, nitem
  use TOUZA_Nio,only: hi_DFMT, hi_ITEM, hi_DSET, hi_UNIT
  use TOUZA_Nio,only: put_header_cprop, put_item, get_default_header
  use TOUZA_Nio,only: nio_write_header, nio_write_data, nio_store_csr
  use TOUZA_Nio,only: lopts_sparse
!!!_  - default
  implicit none
  public
!!!_  - base parameters
  integer,parameter :: KTGT=KDBL
  integer,parameter :: lname=16

!!!_  - dset entries
  ! character(len=*),parameter :: DSET_PFX    = 'QX'
  ! character(len=*),parameter :: DSET_R2G    = DSET_PFX // 'RFILE'
  ! character(len=*),parameter :: DSET_PS2G   = DSET_PFX // 'PS2G'
  ! character(len=*),parameter :: DSET_PSPROP = DSET_PFX // 'PSPROP'

  integer,parameter :: xcmd_none = 0
  integer,parameter :: xcmd_noperm = 16384    ! permutation mask
  integer,parameter :: xcmd_r2g  = 1
  integer,parameter :: xcmd_ps2g = 2
  integer,parameter :: xcmd_loc  = 3
  integer,parameter :: xcmd_csr  = 16 + 1
  integer,parameter :: xcmd_qjds = 16 + 2
  integer,parameter :: xcmd_aux  = 32
  integer,parameter :: xcmd_fdplain  = 64 + 1 + xcmd_noperm
  integer,parameter :: xcmd_fdcycle  = 64 + 2 + xcmd_noperm

  character(len=*),parameter :: DSET_LOC  = 'AXLOC'
  character(len=*),parameter :: DSET_CLOC = 'C' // DSET_LOC
  character(len=*),parameter :: DSET_WGT  = 'AXWGT'
  character(len=*),parameter :: DSET_CWGT = 'C' // DSET_WGT

!!!_  - configurations
  ! character(len=*),parameter :: sep_item = ':'
  ! character(len=*),parameter :: sep_name = '/'
  ! character(len=*),parameter :: sep_coor = ','
  ! character(len=*),parameter :: sep_lpad = '-'
  ! character(len=*),parameter :: sep_rpad = '+'

  character(len=*),parameter :: opr_assign = '='
  character(len=*),parameter :: param_skip = '-'

  character(len=*),parameter :: disable_switch = '--'

!!!_  - flags
  integer,parameter :: span_positive = +1
  integer,parameter :: span_negative = -1
  integer,parameter :: span_both = 0

  integer,parameter :: cset_err = -1
  integer,parameter :: cset_ordered = 0
  integer,parameter :: cset_shuffled = 1

!!!_  - type
  ! transformation
  integer,parameter :: lcvax = 3
  integer,parameter :: lweights = 3
  integer,parameter :: lcaux = 3
  type transf_t
     integer k
     integer nxs, nxd     ! number of coordinates
     character(len=litem) :: srcx(0:lcvax-1)      ! source,destination coordinates
     character(len=litem) :: dstx(0:lcvax-1)
     integer msrc(0:lcvax-1)
     integer mdst(0:lcvax-1)
     integer auxsrc(0:lcaux-1, 0:lcvax-1)
     integer auxdst(0:lcaux-1, 0:lcvax-1)
     integer namin, namax
     integer nw
     integer h             ! cache handle (suite or group)
     integer idjv,  idwv(0:lweights-1)   ! index, weight variable id
     integer recj,  recw(0:lweights-1)   ! record index
     integer nbase, mcols, npack
     integer colc
     integer popts(lopts_sparse)
     real(kind=KTGT),allocatable :: wcsr(:, :)
     integer,        allocatable :: icsr(:)
     integer,        allocatable :: hcsr(:)
     character(len=litem) :: opr      ! operator name
  end type transf_t
  ! convoy
  type convoy_t
     integer :: nt = -1
     type(transf_t),pointer :: tr(:) => NULL()
     type(auxil_t), pointer :: aux(:) => NULL()
  end type convoy_t
  ! auxiliary data
  type auxil_t
     integer k
     integer nx
     character(len=litem) :: xname(0:lcvax-1)
     integer mem(0:lcvax-1)
     integer h   ! cache handle (suite or group)
     integer idv
     integer rec
     integer reftr  ! reference transform
     real(kind=KTGT),allocatable :: vd(:)
     integer,        allocatable :: vi(:)
  end type auxil_t

!!!_ + Procedures
  contains
!!!_   . set_header_geogr
  subroutine set_header_geogr &
       & (ierr, head, &
       &  jlatb, jlate, latname, &
       &  jlonb, jlone, lonname, &
       &  dfmt)
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(inout) :: head(*)
    integer,         intent(in)    :: jlatb, jlate
    integer,         intent(in)    :: jlonb, jlone
    character(len=*),intent(in)    :: latname, lonname
    character(len=*),intent(in)    :: dfmt
    integer jc

    ierr = 0
    jc = 1
    if (lonname.ne.' '.or.jlone.gt.jlonb) then
       if (ierr.eq.0) call put_header_cprop(ierr, head, lonname, (/jlonb+1, jlone/), jc)
       jc = jc + 1
    endif
    if (latname.ne.' '.or.jlate.gt.jlatb) then
       if (ierr.eq.0) call put_header_cprop(ierr, head, latname, (/jlatb+1, jlate/), jc)
       jc = jc + 1
    endif
    do
       if (jc.gt.3) exit
       if (ierr.eq.0) call put_header_cprop(ierr, head, ' ',   (/0, 0/),  jc)
       jc = jc + 1
    enddo

    if (ierr.eq.0) call put_item(ierr, head, dfmt,  hi_DFMT)

  end subroutine set_header_geogr

!!!_  & parse_oprfile
  subroutine parse_oprfile &
       & (ierr, pr, jpos, npos, handle, xsw, vidx, vwgt, vw2, vw3)
    use TOUZA_Nio,only: grp_suite, cache_var_id, cache_read_header
    use TOUZA_Nio,only: cache_unit, cache_co_name, cache_co_len
    use TOUZA_Nio,only: cache_group_recs, cache_sparse_review
    use TOUZA_Nio,only: nio_column_coor, nio_review_sparse
    use TOUZA_Nio,only: lopts_sparse, PROP_PTX_COLC
    use TOUZA_Nio,only: hi_MEMO1, get_item, restore_item
    implicit none
    integer,         intent(out)   :: ierr
    type(convoy_t),  intent(inout) :: pr
    integer,         intent(inout) :: jpos
    integer,         intent(in)    :: npos
    integer,         intent(in)    :: handle
    integer,         intent(in)    :: xsw
    character(len=*),intent(in)    :: vidx, vwgt
    character(len=*),intent(in),optional :: vw2, vw3

    character(len=*),parameter :: proc = 'parse_oprfile'

    integer id_jini, id_wini
    integer id_j
    integer nrec_j
    integer jt,      jrec
    integer utab
    real(kind=KTGT) :: mold = 0.0_KTGT
    character(len=litem) :: head(nitem)
    integer popts(lopts_sparse)
    integer nbase, mcols, npack, colc
    integer jx, ji
    integer jw, nw, jtbgn

    ierr = 0
    id_wini = 0

    utab = cache_unit(handle)
    colc = -1
    nw = 1
    if (present(vw2)) nw = nw + 1
    if (present(vw3)) nw = nw + 1

    id_jini = 0
    jtbgn = pr%nt
    do
       id_j = cache_var_id(vidx, handle, id_jini)
       if (id_j.lt.0) exit
       nrec_j = cache_group_recs(handle, id_j)
       if (nrec_j.lt.0) then
          ierr = ERR_PANIC
          exit
       endif
       do jrec = 0, nrec_j - 1
          call add_transf(ierr, jt, pr)
          if (ierr.eq.0) then
             pr%tr(jt)%idwv(:) = -1
             pr%tr(jt)%recw(:) = -1
             pr%tr(jt)%k        = xsw
             pr%tr(jt)%h        = handle
             pr%tr(jt)%idjv     = id_j
             pr%tr(jt)%recj     = jrec
             pr%tr(jt)%namin    = 1
             pr%tr(jt)%namax    = nw
             pr%tr(jt)%nw       = nw
          endif
          if (ierr.eq.0) then
             call cache_sparse_review &
                  & (ierr,   &
                  &  nbase,  mcols, npack, &
                  &  handle, id_j,  jrec,  mold, &
                  &  head,   popts, colc)
          endif
          if (ierr.eq.0) then
             pr%tr(jt)%nbase = nbase
             pr%tr(jt)%mcols = mcols
             pr%tr(jt)%npack = npack
             pr%tr(jt)%colc = colc
             if (colc.le.0) ierr = ERR_PANIC
          endif
          if (ierr.eq.0) then
             pr%tr(jt)%auxdst(:,:) = -1
             pr%tr(jt)%auxsrc(:,:) = -1
             pr%tr(jt)%nxd = colc - 1
             do jx = 0, colc - 2
                if (ierr.eq.0) then
                   call cache_co_name(ierr, pr%tr(jt)%dstx(jx), handle, id_j, jx)
                endif
                if (ierr.eq.0) then
                   pr%tr(jt)%mdst(jx) = cache_co_len(handle, id_j, jx)
                endif
             enddo
             pr%tr(jt)%nxs = 0
             do jx = 0, lcvax - 1
                ji = hi_MEMO1 + jx * 2
                if (ierr.eq.0) call get_item(ierr, head, pr%tr(jt)%srcx(jx),  ji)
                if (ierr.eq.0) then
                   if (pr%tr(jt)%srcx(jx).eq.' ') exit
                   pr%tr(jt)%nxs = pr%tr(jt)%nxs + 1
                   call restore_item(ierr, head, pr%tr(jt)%msrc(jx),  ji+1, def=-1)
                endif
             enddo
          endif
       enddo
       if (ierr.ne.0) exit
       id_jini = id_j + 1
    enddo
    jw = 0
    if (ierr.eq.0) then
       call add_opr_weight(ierr, pr, jtbgn, handle, jw, vwgt, vidx)
    endif
    jw = jw + 1
    if (present(vw2)) then
       if (ierr.eq.0) then
          call add_opr_weight(ierr, pr, jtbgn, handle, jw, vw2, vidx)
       endif
    endif
    jw = jw + 1
    if (present(vw3)) then
       if (ierr.eq.0) then
          call add_opr_weight(ierr, pr, jtbgn, handle, jw, vw3, vidx)
       endif
    endif

    call trace_err(ierr, fun=proc)
    return
  end subroutine parse_oprfile

!!!_  & add_transf
  subroutine add_transf(ierr, jt, pr)
    implicit none
    integer,       intent(out)   :: ierr
    integer,       intent(out)   :: jt
    type(convoy_t),intent(inout) :: pr

    type(transf_t),pointer :: tt(:)
    integer,parameter :: mt = 2

    ierr = 0
    jt = pr%nt
    if (jt.ge.size(pr%tr)) then
       allocate(tt(0:pr%nt+mt-1), STAT=ierr)
       if (ierr.eq.0) then
          tt(0:pr%nt-1) = pr%tr(0:pr%nt-1)
          deallocate(pr%tr, STAT=ierr)
       endif
       if (ierr.eq.0) pr%tr => tt
    endif
    if (ierr.eq.0) pr%nt = pr%nt + 1
    if (ierr.ne.0) ierr = ERR_ALLOCATION
  end subroutine add_transf

!!!_  - add_opr_weight
  subroutine add_opr_weight &
       & (ierr, pr, jtbgn, handle, jw, vwgt, vidx)
    use TOUZA_Nio,only: cache_var_id, cache_group_recs
    implicit none
    integer,         intent(out)   :: ierr
    type(convoy_t),  intent(inout) :: pr
    integer,         intent(in)    :: jtbgn
    integer,         intent(in)    :: handle
    integer,         intent(in)    :: jw
    character(len=*),intent(in)    :: vwgt
    character(len=*),intent(in)    :: vidx
    integer jt
    integer id_j, id_jprv, jreci, nreci
    integer id_w, id_wini, nrecw
    character(len=lmsg) :: txt

    ierr = 0
    id_wini = 0
    id_jprv = -1
    nrecw = -1

119 format('No pair record of index and weight: ', A, 1x, A)
    do jt = jtbgn, pr%nt - 1
       id_j  = pr%tr(jt)%idjv
       if (id_jprv.ne.id_j) then
          id_w = cache_var_id(vwgt, handle, id_wini)
          if (id_w.lt.0) exit
          nrecw = cache_group_recs(handle, id_w)
          nreci = cache_group_recs(handle, id_j)
          if (nrecw.ne.nreci) then
             ierr = ERR_PANIC
             write(txt, 119) trim(vidx), trim(vwgt)
             call message(ierr, txt, levm=msglev_CRITICAL)
             exit
          endif
          id_wini = id_w + 1
          id_jprv = id_j
       endif
       jreci = pr%tr(jt)%recj
       if (jreci.lt.nrecw) then
          pr%tr(jt)%idwv(jw) = id_w
          pr%tr(jt)%recw(jw) = jreci
       else
          ierr = ERR_PANIC
          write(txt, 119) trim(vidx), trim(vwgt)
          call message(ierr, txt, levm=msglev_CRITICAL)
          exit
       endif
    enddo
  end subroutine add_opr_weight

!!!_ + end module
end module convoy_util
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

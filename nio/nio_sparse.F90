!!!_! nio_sparse.F90 - TOUZA/Nio sparse matrix interfaces
! Maintainer: SAITO Fuyuki
! Created: Apr 1 2023
#define TIME_STAMP 'Time-stamp: <2024/04/14 09:46:55 fuyuki nio_sparse.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2023, 2024
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_nio.h"
!!!_@ TOUZA_Nio_sparse - nio sparse interfaces
module TOUZA_Nio_sparse
!!!_ = declaration
  use TOUZA_Nio_std,only: KI32, KI64, KDBL, KFLT
  use TOUZA_Nio_std,only: unit_global
  use TOUZA_Nio_record,only: KRMIS
  use TOUZA_Nio_record,only: lopts_rec=>lopts
  implicit none
  private
!!!_  - public
  character(len=*),parameter,public :: col_coor = 'COLUMN'

  integer,parameter,public :: column_def  = -1
  integer,parameter,public :: column_none = -9
!!!_  - property table
  integer,parameter :: PROP_SPARSE_ROWS   = lopts_rec + 1   ! number of rows
  integer,parameter :: PROP_SPARSE_CACHE  = lopts_rec + 2
  integer,parameter,public :: lopts_sparse = lopts_rec + 2
!!!_  - common work area
  integer,        allocatable,save :: worki(:)
  real(kind=KFLT),allocatable,save :: workf(:)
  real(kind=KDBL),allocatable,save :: workd(:)
  integer(kind=KI32),allocatable,save :: wsubv(:)

  integer,parameter :: lwtag = 32
  character(len=lwtag),save :: wtag_i = ' ', wtag_f  = ' ', wtag_d = ' '
  character(len=lwtag),save :: wtag_s = ' '

  integer,parameter :: ctag_mdl = 7
  integer,save :: ctag_wi = 1, ctag_wf = 2, ctag_wd = 3
  integer,save :: ctag_null = 0
  integer,parameter :: ctag_error = -1

  integer,parameter :: switch_flush = 0   ! deallocate work area at exit
  integer,parameter :: switch_keep = 1    ! keep work area

  integer,save :: wswitch = switch_flush
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = NIO_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
#define __MDL__ 'p'
#define _ERROR(E) (E - ERR_MASK_NIO_SPARSE)
!!!_  - interfaces
  interface alloc_work
     module procedure alloc_work_i, alloc_work_f, alloc_work_d
  end interface alloc_work

  interface nio_review_sparse
     module procedure nio_review_sparse_d, nio_review_sparse_f, nio_review_sparse_i
  end interface nio_review_sparse

  interface nio_store_csr
     module procedure nio_store_csr_d, nio_store_csr_f, nio_store_csr_i
  end interface nio_store_csr
  interface nio_store_qjds
     module procedure nio_store_qjds_d, nio_store_qjds_f, nio_store_qjds_i
  end interface nio_store_qjds

  interface nio_restore_csr
     module procedure nio_restore_csr_d, nio_restore_csr_f, nio_restore_csr_i
  end interface nio_restore_csr
  interface nio_restore_qjds
     module procedure nio_restore_qjds_d, nio_restore_qjds_f, nio_restore_qjds_i
  end interface nio_restore_qjds

  ! interface review_plain
  !    module procedure review_plain_d, review_plain_f, review_plain_i
  ! end interface review_plain

  interface qjds2csr
     module procedure qjds2csr_d, qjds2csr_f, qjds2csr_i
  end interface qjds2csr
  interface ptx2qjds
     module procedure ptx2qjds_d, ptx2qjds_f, ptx2qjds_i
  end interface ptx2qjds
  interface expand_qjds
     module procedure expand_qjds_d, expand_qjds_f, expand_qjds_i
  end interface expand_qjds
  interface pack_qjds
     module procedure pack_qjds_d, pack_qjds_f, pack_qjds_i
  end interface pack_qjds
!!!_  - public procedures
  public init, diag, finalize
  public nio_column_coor
  public nio_review_sparse, nio_inquire_sparse
  public nio_store_csr,     nio_restore_csr
  public nio_store_qjds,    nio_restore_qjds
!!!_  - public shared
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init &
       & (ierr, u, levv, mode, stdv, icomm)
    use TOUZA_Nio_std,only: control_mode, control_deep, is_first_force
    use TOUZA_Nio_std,only: ns_init=>init, choice
    use TOUZA_Nio_record,only: nr_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv, icomm
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
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ns_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
          if (ierr.eq.0) call nr_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
       endif
       if (md.ge.MODE_DEEP) then

       endif
       if (init_counts.eq.0) then

       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = _ERROR(ERR_FAILURE_INIT)
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Nio_std,only: control_mode, control_deep, is_first_force
    use TOUZA_Nio_std,only: trace_control
    use TOUZA_Nio_std,only: ns_diag=>diag, choice, get_logu
    use TOUZA_Nio_std,only: msg, is_msglev_NORMAL
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
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __MDL__, utmp)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ns_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_diag(ierr, utmp, levv=lv, mode=lmd)
       endif
       if (md.ge.MODE_DEEP) then

       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Nio_std,only: control_mode, control_deep, is_first_force
    use TOUZA_Nio_std,only: trace_fine
    use TOUZA_Nio_std,only: ns_finalize=>finalize, choice, get_logu
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
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ns_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_finalize(ierr, utmp, levv=lv, mode=lmd)
       endif
       if (md.ge.MODE_DEEP) then

       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize
!!!_ + work area
!!!_  & diag_alloc
  subroutine diag_alloc(cmd, name, otag, ntag, n, u, levv)
    use TOUZA_Nio_std,only: get_logu
    use TOUZA_Nio_std,only: choice, msg
    use TOUZA_Nio_std,only: is_msglev_NORMAL, is_msglev_INFO, is_msglev_FATAL
    implicit none
    character(len=*),intent(in)  :: cmd
    character(len=*),intent(in)  :: name
    character(len=*),intent(in)  :: otag, ntag
    integer,         intent(in)  :: n
    integer,optional,intent(in)  :: u, levv
    integer utmp, lv
    character(len=128) :: txt
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)

101 format('alloc/unlock [', A, '] ', I0, 1x, A, ' > ', A)
111 format('alloc/error [', A, '] ', I0, 1x, A, ' > ', A)
121 format('alloc/lock [', A, '] ', I0, 1x, A, ' > ', A)
131 format('alloc/free [', A, '] ', I0, 1x, A, ' > ', A)

    select case(cmd)
    case('u')
       if (otag.ne.ntag) then
          if (is_msglev_NORMAL(lv)) then
             write(txt, 101) trim(name), n, trim(otag), trim(ntag)
             call msg(txt, __MDL__, utmp)
          endif
       else
          if (is_msglev_INFO(lv)) then
             write(txt, 101) trim(name), n, trim(otag), trim(ntag)
             call msg(txt, __MDL__, utmp)
          endif
       endif
    case('f')
       if (otag.ne.ntag) then
          if (is_msglev_NORMAL(lv)) then
             write(txt, 131) trim(name), n, trim(otag), trim(ntag)
             call msg(txt, __MDL__, utmp)
          endif
       else
          if (is_msglev_INFO(lv)) then
             write(txt, 131) trim(name), n, trim(otag), trim(ntag)
             call msg(txt, __MDL__, utmp)
          endif
       endif
    case('e')
       if (is_msglev_FATAL(lv)) then
          write(txt, 111) trim(name), n, trim(otag), trim(ntag)
          call msg(txt, __MDL__, utmp)
       endif
    case default
       if (is_msglev_INFO(lv)) then
          write(txt, 121) trim(name), n, trim(otag), trim(ntag)
          call msg(txt, __MDL__, utmp)
       endif
    end select
  end subroutine diag_alloc
!!!_  & alloc_work
  subroutine alloc_work_i(ierr, n, tag, mold, def)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: n
    character(len=*),intent(in)          :: tag
    integer,         intent(in)          :: mold
    integer,         intent(in),optional :: def
    character(len=*),parameter :: wstr='worki'
#define _WTAG wtag_i
#define _WORK worki
    ierr = 0
    if (n.lt.0) then
       if (wswitch.eq.switch_flush) then
          call diag_alloc('f', wstr, _WTAG, tag, size(_WORK))
          deallocate(_WORK, STAT=ierr)
       else
          call diag_alloc('u', wstr, _WTAG, tag, size(_WORK))
       endif
       _WTAG = ' '
       return
    endif
    if (_WTAG.ne.' ') then
       call diag_alloc('e', wstr, _WTAG, tag, size(_WORK))
       ierr = _ERROR(ERR_PANIC)
       return
    endif
    call diag_alloc('l', wstr, _WTAG, tag, n)
    _WTAG = tag
    if (allocated(_WORK)) then
       if (n.gt.size(_WORK)) then
          deallocate(_WORK, STAT=ierr)
          if (ierr.eq.0) allocate(_WORK(0:n-1), STAT=ierr)
       endif
    else
       allocate(_WORK(0:n-1), STAT=ierr)
    endif
    if (present(def)) then
       if (ierr.eq.0) _WORK(0:n-1) = def
    endif

    if (mold.eq.0) continue     ! dummy
#undef _WTAG
#undef _WORK
  end subroutine alloc_work_i
  subroutine alloc_work_f(ierr, n, tag, mold, def)
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: n
    character(len=*),intent(in)          :: tag
    real(kind=KARG), intent(in)          :: mold
    real(kind=KARG), intent(in),optional :: def
    character(len=*),parameter :: wstr='workf'
#define _WTAG wtag_f
#define _WORK workf
    ierr = 0
    if (n.lt.0) then
       if (wswitch.eq.switch_flush) then
          call diag_alloc('f', wstr, _WTAG, tag, size(_WORK))
          deallocate(_WORK, STAT=ierr)
       else
          call diag_alloc('u', wstr, _WTAG, tag, size(_WORK))
       endif
       _WTAG = ' '
       return
    endif
    if (_WTAG.ne.' ') then
       call diag_alloc('e', wstr, _WTAG, tag, size(_WORK))
       ierr = _ERROR(ERR_PANIC)
       return
    endif
    call diag_alloc('l', wstr, _WTAG, tag, n)
    _WTAG = tag
    if (allocated(_WORK)) then
       if (n.gt.size(_WORK)) then
          deallocate(_WORK, STAT=ierr)
          if (ierr.eq.0) allocate(_WORK(0:n-1), STAT=ierr)
       endif
    else
       allocate(_WORK(0:n-1), STAT=ierr)
    endif
    if (present(def)) then
       if (ierr.eq.0) _WORK(0:n-1) = def
    endif

    if (mold.eq.0) continue     ! dummy
#undef _WTAG
#undef _WORK
  end subroutine alloc_work_f
  subroutine alloc_work_d(ierr, n, tag, mold, def)
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: n
    character(len=*),intent(in)          :: tag
    real(kind=KARG), intent(in)          :: mold
    real(kind=KARG), intent(in),optional :: def
    character(len=*),parameter :: wstr='workd'
#define _WTAG wtag_d
#define _WORK workd
    ierr = 0
    if (n.lt.0) then
       if (wswitch.eq.switch_flush) then
          call diag_alloc('f', wstr, _WTAG, tag, size(_WORK))
          deallocate(_WORK, STAT=ierr)
       else
          call diag_alloc('u', wstr, _WTAG, tag, size(_WORK))
       endif
       _WTAG = ' '
       return
    endif
    if (_WTAG.ne.' ') then
       call diag_alloc('e', wstr, _WTAG, tag, size(_WORK))
       ierr = _ERROR(ERR_PANIC)
       return
    endif
    call diag_alloc('l', wstr, _WTAG, tag, n)
    _WTAG = tag
    if (allocated(_WORK)) then
       if (n.gt.size(_WORK)) then
          deallocate(_WORK, STAT=ierr)
          if (ierr.eq.0) allocate(_WORK(0:n-1), STAT=ierr)
       endif
    else
       allocate(_WORK(0:n-1), STAT=ierr)
    endif
    if (present(def)) then
       if (ierr.eq.0) _WORK(0:n-1) = def
    endif

    if (mold.eq.0) continue     ! dummy
#undef _WTAG
#undef _WORK
  end subroutine alloc_work_d
!!!_  & alloc_wsubv
  subroutine alloc_wsubv(ierr, n, tag, def)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: n
    character(len=*),intent(in)          :: tag
    integer,         intent(in),optional :: def
    character(len=*),parameter :: wstr='wsubv'
#define _WTAG wtag_s
#define _WORK wsubv
    ierr = 0
    if (n.lt.0) then
       if (wswitch.eq.switch_flush) then
          call diag_alloc('f', wstr, _WTAG, tag, size(_WORK))
          deallocate(_WORK, STAT=ierr)
       else
          call diag_alloc('u', wstr, _WTAG, tag, size(_WORK))
       endif
       _WTAG = ' '
       return
    endif
    if (_WTAG.ne.' ') then
       call diag_alloc('e', wstr, _WTAG, tag, size(_WORK))
       ierr = _ERROR(ERR_PANIC)
       return
    endif
    call diag_alloc('l', wstr, _WTAG, tag, n)
    _WTAG = tag
    if (allocated(_WORK)) then
       if (n.gt.size(_WORK)) then
          deallocate(_WORK, STAT=ierr)
          if (ierr.eq.0) allocate(_WORK(0:n-1), STAT=ierr)
       endif
    else
       allocate(_WORK(0:n-1), STAT=ierr)
    endif
    if (present(def)) then
       if (ierr.eq.0) _WORK(0:n-1) = def
    endif

#undef _WTAG
#undef _WORK
  end subroutine alloc_wsubv

!!!_ + Sparse properties review
!!!_  & nio_column_coor()
  integer function nio_column_coor &
       & (head, cname) &
       & result(n)
    use TOUZA_Nio_std,only: choice_a
    use TOUZA_Nio_header,only: litem
    use TOUZA_Nio_record,only: inquire_header_coor
    implicit none
    character(len=*),intent(in)          :: head(*)
    character(len=*),intent(in),optional :: cname      ! column coordinate name
    character(len=litem) :: co

    ! blank cname when default
    call choice_a(co, ' ', cname)
    if (co.eq.' ') then
       call inquire_header_coor(head, col_coor, idx=n)
       if (n.le.0) n = column_def
    else
       call inquire_header_coor(head, co, idx=n)
       if (n.le.0) n = column_none
    endif
  end function nio_column_coor

!!!_  & nio_review_sparse
  subroutine nio_review_sparse_d &
       & (ierr,  popts, &
       &  head,  u,     krect, mold, colc, flag)
    use TOUZA_Nio_std,only: KIOFS
    use TOUZA_Nio_std,only: choice, msg
    use TOUZA_Nio_record,only: laxs
    use TOUZA_Nio_record,only: parse_header_size, is_match_format
    use TOUZA_Nio_record,only: parse_header_base
    use TOUZA_Nio_record,only: ptx_parse_array, nio_read_data
    use TOUZA_Nio_record,only: PROP_PTX_COLC
    use TOUZA_Nio_record,only: pre_review, post_review, is_review_leave, ptx_row_size
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: popts(*)
    character(len=*),intent(in)          :: head(*)
    integer,         intent(in)          :: krect
    integer,         intent(in)          :: u
    real(kind=KARG), intent(in)          :: mold
    integer,         intent(in),optional :: colc
    integer,         intent(in),optional :: flag
    integer kaxs(laxs)
    integer kfmt
    real(kind=KRMIS) :: vmiss
    integer nfull
    character(len=*),parameter :: proc='wsp'
    integer(kind=KIOFS) :: apini
    integer ci
#define _WORK workd
#define _CTAG ctag_wd

    ierr = 0
    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    if (ierr.eq.0) then
       if (is_match_format(kfmt, st='P')) then
          call nio_review_sparse_ptx &
               & (ierr,  popts, &
               &  u,     krect, kaxs, laxs, colc, flag)
       else
          ci = choice(column_def, colc)
          if (ci.eq.column_none) then
             call msg('no column coordinate', __MDL__)
             ierr = _ERROR(ERR_INVALID_PARAMETER)
          endif
          if (ierr.eq.0) call pre_review(ierr, apini, u, flag)
          if (ierr.eq.0) then
             nfull = parse_header_size(head, 0, 1)
             call alloc_work(ierr, nfull, proc, mold)
          endif
          if (ierr.eq.0) call nio_read_data(ierr, _WORK, nfull, head, krect, u)
          if (ierr.eq.0) then
             call ptx_parse_array &
                  & (ierr, popts, _WORK, nfull, vmiss, colc, kaxs, laxs)
          endif
          if (ierr.eq.0) then
             popts(PROP_SPARSE_ROWS) = ptx_row_size(popts, kaxs, laxs)
             popts(PROP_SPARSE_CACHE) = _CTAG
          endif
          if (ierr.eq.0) then
             if (.not.is_review_leave(flag)) &
                  & call alloc_work(ierr, -1, proc, mold)
          endif
          if (ierr.eq.0) call post_review(ierr, apini, u, flag)
       endif
    end if
#undef _WORK
#undef _CTAG
  end subroutine nio_review_sparse_d
  subroutine nio_review_sparse_f &
       & (ierr,  popts, &
       &  head,  u,     krect, mold, colc, flag)
    use TOUZA_Nio_std,only: KIOFS
    use TOUZA_Nio_std,only: choice, msg
    use TOUZA_Nio_record,only: laxs
    use TOUZA_Nio_record,only: parse_header_size, is_match_format
    use TOUZA_Nio_record,only: parse_header_base
    use TOUZA_Nio_record,only: ptx_parse_array, nio_read_data
    use TOUZA_Nio_record,only: PROP_PTX_COLC
    use TOUZA_Nio_record,only: pre_review, post_review, is_review_leave, ptx_row_size
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: popts(*)
    character(len=*),intent(in)          :: head(*)
    integer,         intent(in)          :: krect
    integer,         intent(in)          :: u
    real(kind=KARG), intent(in)          :: mold
    integer,         intent(in),optional :: colc
    integer,         intent(in),optional :: flag
    integer kaxs(laxs)
    integer kfmt
    real(kind=KRMIS) :: vmiss
    integer nfull
    character(len=*),parameter :: proc='wsp'
    integer(kind=KIOFS) :: apini
    integer ci
#define _WORK workf
#define _CTAG ctag_wf

    ierr = 0
    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    if (ierr.eq.0) then
       if (is_match_format(kfmt, st='P')) then
          call nio_review_sparse_ptx &
               & (ierr,  popts, &
               &  u,     krect, kaxs, laxs, colc, flag)
       else
          ci = choice(column_def, colc)
          if (ci.eq.column_none) then
             call msg('no column coordinate', __MDL__)
             ierr = _ERROR(ERR_INVALID_PARAMETER)
          endif
          if (ierr.eq.0) call pre_review(ierr, apini, u, flag)
          if (ierr.eq.0) then
             nfull = parse_header_size(head, 0, 1)
             call alloc_work(ierr, nfull, proc, mold)
          endif
          if (ierr.eq.0) call nio_read_data(ierr, _WORK, nfull, head, krect, u)
          if (ierr.eq.0) then
             call ptx_parse_array &
                  & (ierr, popts, _WORK, nfull, vmiss, colc, kaxs, laxs)
          endif
          if (ierr.eq.0) then
             popts(PROP_SPARSE_ROWS) = ptx_row_size(popts, kaxs, laxs)
             popts(PROP_SPARSE_CACHE) = _CTAG
          endif
          if (ierr.eq.0) then
             if (.not.is_review_leave(flag)) &
                  & call alloc_work(ierr, -1, proc, mold)
          endif
          if (ierr.eq.0) call post_review(ierr, apini, u, flag)
       endif
    end if
#undef _WORK
#undef _CTAG
  end subroutine nio_review_sparse_f
  subroutine nio_review_sparse_i &
       & (ierr,  popts, &
       &  head,  u,     krect, mold, colc, flag)
    use TOUZA_Nio_std,only: KIOFS
    use TOUZA_Nio_std,only: choice, msg
    use TOUZA_Nio_record,only: laxs
    use TOUZA_Nio_record,only: parse_header_size, is_match_format
    use TOUZA_Nio_record,only: parse_header_base
    use TOUZA_Nio_record,only: ptx_parse_array, nio_read_data
    use TOUZA_Nio_record,only: PROP_PTX_COLC
    use TOUZA_Nio_record,only: pre_review, post_review, is_review_leave, ptx_row_size
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)         :: ierr
    integer,           intent(out)         :: popts(*)
    character(len=*),  intent(in)          :: head(*)
    integer,           intent(in)          :: krect
    integer,           intent(in)          :: u
    integer(kind=KARG),intent(in)          :: mold
    integer,           intent(in),optional :: colc
    integer,           intent(in),optional :: flag
    integer kaxs(laxs)
    integer kfmt
    real(kind=KRMIS) :: vmiss
    integer nfull
    character(len=*),parameter :: proc='wsp'
    integer(kind=KIOFS) :: apini
    integer ci
#define _WORK worki
#define _CTAG ctag_wi

    ierr = 0
    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, head)
    if (ierr.eq.0) then
       if (is_match_format(kfmt, st='P')) then
          call nio_review_sparse_ptx &
               & (ierr,  popts, &
               &  u,     krect, kaxs, laxs, colc, flag)
       else
          ci = choice(column_def, colc)
          if (ci.eq.column_none) then
             call msg('no column coordinate', __MDL__)
             ierr = _ERROR(ERR_INVALID_PARAMETER)
          endif
          if (ierr.eq.0) call pre_review(ierr, apini, u, flag)
          if (ierr.eq.0) then
             nfull = parse_header_size(head, 0, 1)
             call alloc_work(ierr, nfull, proc, mold)
          endif
          if (ierr.eq.0) call nio_read_data(ierr, _WORK, nfull, head, krect, u)
          if (ierr.eq.0) then
             call ptx_parse_array &
                  & (ierr, popts, _WORK, nfull, vmiss, colc, kaxs, laxs)
          endif
          if (ierr.eq.0) then
             popts(PROP_SPARSE_ROWS) = ptx_row_size(popts, kaxs, laxs)
             popts(PROP_SPARSE_CACHE) = _CTAG
          endif
          if (ierr.eq.0) then
             if (.not.is_review_leave(flag)) &
                  & call alloc_work(ierr, -1, proc, mold)
          endif
          if (ierr.eq.0) call post_review(ierr, apini, u, flag)
       endif
    end if
#undef _WORK
#undef _CTAG
  end subroutine nio_review_sparse_i

!!!_  & nio_review_sparse_ptx
  subroutine nio_review_sparse_ptx &
       & (ierr,  popts, &
       &  u,     krect, kaxs, laxs, colc, flag)
    use TOUZA_Nio_std,only: choice, msg
    use TOUZA_Nio_record,only: ptx_review, parse_header_base, ptx_row_size
    use TOUZA_Nio_record,only: PROP_PTX_COLC
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: popts(*)
    integer,         intent(in)          :: krect
    integer,         intent(in)          :: u
    integer,         intent(in)          :: kaxs(*)
    integer,         intent(in)          :: laxs
    integer,         intent(in),optional :: colc
    integer,         intent(in),optional :: flag
    integer ci

    ierr = 0
    if (ierr.eq.0) then
       call ptx_review(ierr, popts, u, krect, flag)
       ci = choice(column_def, colc)
       if (ci.eq.column_def) ci = popts(PROP_PTX_COLC)
    endif
    if (ierr.eq.0) then
       if (ci.gt.0.and.ci.ne.popts(PROP_PTX_COLC)) then
          call msg('(''invalid column coordinate = '', I0)', (/ci/), __MDL__)
          ierr = _ERROR(ERR_INVALID_PARAMETER)
       endif
       if (ci.eq.column_none) then
          ci = popts(PROP_PTX_COLC)
          if (ci.ge.1.and.ci.le.laxs) then
             call msg('(''inconsistent column coordinate = '', I0)', (/ci/), __MDL__)
             ierr = _ERROR(ERR_INVALID_PARAMETER)
          endif
       endif
    endif
    if (ierr.eq.0) then
       popts(PROP_SPARSE_ROWS) = ptx_row_size(popts, kaxs, laxs)
       popts(PROP_SPARSE_CACHE) = ctag_null
    endif
    return
  end subroutine nio_review_sparse_ptx

!!!_  & nio_inquire_sparse
  subroutine nio_inquire_sparse &
       & (popts, nrows, mcols, ndefs)
    use TOUZA_Nio_record,only: PROP_PTX_DATA, PROP_PTX_MCOL
    implicit none
    integer,intent(in) :: popts(*)
    integer,intent(out),optional :: nrows
    integer,intent(out),optional :: mcols
    integer,intent(out),optional :: ndefs

    if (present(nrows)) then
       nrows = popts(PROP_SPARSE_ROWS)
    endif
    if (present(mcols)) then
       mcols = popts(PROP_PTX_MCOL)
    endif
    if (present(ndefs)) then
       ndefs = popts(PROP_PTX_DATA)
    endif
  end subroutine nio_inquire_sparse

!!!_ + CSR storage procedures
!!!_  & nio_restore_csr
  subroutine nio_restore_csr_d &
       & (ierr, d,  cofs,  &
       &  hd,   u,  krect, colc, flag, kopts)
    use TOUZA_Nio_record,only: laxs, rev_pos_leave
    use TOUZA_Nio_record,only: parse_header_base
    use TOUZA_Nio_record,only: ptx_read_data
    use TOUZA_Nio_record,only: ptx_gen_ccvec, ptx_pack_data
    use TOUZA_Nio_record,only: is_match_format, is_review_leave
    use TOUZA_Nio_record,only: PROP_PTX_DATA, PROP_PTX_MCOL
    use TOUZA_Nio_std,only: choice, msg
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(out)         :: d(0:*)
    integer,         intent(inout)       :: cofs(0:*)  ! offset vector
    character(len=*),intent(in)          :: hd(*)
    integer,         intent(in)          :: u
    integer,         intent(inout)       :: krect
    integer,         intent(in),optional :: colc
    integer,         intent(in),optional :: flag
    integer,         intent(in),optional :: kopts(:)   ! option cache

    integer popts(lopts_sparse)
    integer kaxs(laxs)
    integer kfmt
    integer npack, nbase
    real(kind=KRMIS) :: vmiss
    integer j

#define _WORK workd
#define _CTAG ctag_wd

    ierr = 0
    if (is_review_leave(flag)) then
       if (present(kopts)) then
          popts(1:lopts_sparse) = kopts(1:lopts_sparse)
       else
          call msg('Need property argument', __MDL__)
          ierr = _ERROR(ERR_INVALID_ITEM)
       endif
    else
       call nio_review_sparse &
            & (ierr,  popts, &
            &  hd,    u,     krect, d(0), colc, rev_pos_leave)
    endif

    if (ierr.eq.0) nbase = popts(PROP_SPARSE_ROWS)
    if (ierr.eq.0) npack = popts(PROP_PTX_DATA)

    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, hd)
    if (ierr.eq.0) then
       if (is_match_format(kfmt, st='P')) then
          call ptx_read_data &
               & (ierr,  &
               &  cofs(1:nbase), nbase, d,     npack, &
               &  popts,         u,     krect, kfmt)
       else
          if (ierr.eq.0) call ptx_gen_ccvec(ierr, popts, cofs(1:nbase), nbase, _WORK, kaxs, laxs, vmiss)
          if (ierr.eq.0) call ptx_pack_data(ierr, d, _WORK, cofs(1:nbase), kaxs, laxs, popts)
          if (ierr.eq.0) call alloc_work(ierr, -1, ' ', d(0))
       endif
       if (ierr.eq.0) then
          cofs(0) = 0
          do j = 1, nbase
             cofs(j) = cofs(j) + cofs(j-1)
          enddo
       endif
    endif
#undef _WORK
#undef _CTAG
  end subroutine nio_restore_csr_d
  subroutine nio_restore_csr_f &
       & (ierr, d,  cofs,  &
       &  hd,   u,  krect, colc, flag, kopts)
    use TOUZA_Nio_record,only: laxs, rev_pos_leave
    use TOUZA_Nio_record,only: parse_header_base
    use TOUZA_Nio_record,only: ptx_read_data
    use TOUZA_Nio_record,only: ptx_gen_ccvec, ptx_pack_data
    use TOUZA_Nio_record,only: is_match_format, is_review_leave
    use TOUZA_Nio_record,only: PROP_PTX_DATA, PROP_PTX_MCOL
    use TOUZA_Nio_std,only: choice, msg
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(out)         :: d(0:*)
    integer,         intent(inout)       :: cofs(0:*)  ! offset vector
    character(len=*),intent(in)          :: hd(*)
    integer,         intent(in)          :: u
    integer,         intent(inout)       :: krect
    integer,         intent(in),optional :: colc
    integer,         intent(in),optional :: flag
    integer,         intent(in),optional :: kopts(:)   ! option cache

    integer popts(lopts_sparse)
    integer kaxs(laxs)
    integer kfmt
    integer npack, nbase
    real(kind=KRMIS) :: vmiss
    integer j

#define _WORK workf
#define _CTAG ctag_wf

    ierr = 0
    if (is_review_leave(flag)) then
       if (present(kopts)) then
          popts(1:lopts_sparse) = kopts(1:lopts_sparse)
       else
          call msg('Need property argument', __MDL__)
          ierr = _ERROR(ERR_INVALID_ITEM)
       endif
    else
       call nio_review_sparse &
            & (ierr,  popts, &
            &  hd,    u,     krect, d(0), colc, rev_pos_leave)
    endif

    if (ierr.eq.0) nbase = popts(PROP_SPARSE_ROWS)
    if (ierr.eq.0) npack = popts(PROP_PTX_DATA)

    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, hd)
    if (ierr.eq.0) then
       if (is_match_format(kfmt, st='P')) then
          call ptx_read_data &
               & (ierr,  &
               &  cofs(1:nbase), nbase, d,     npack, &
               &  popts,         u,     krect, kfmt)
       else
          if (ierr.eq.0) call ptx_gen_ccvec(ierr, popts, cofs(1:nbase), nbase, _WORK, kaxs, laxs, vmiss)
          if (ierr.eq.0) call ptx_pack_data(ierr, d, _WORK, cofs(1:nbase), kaxs, laxs, popts)
          if (ierr.eq.0) call alloc_work(ierr, -1, ' ', d(0))
       endif
       if (ierr.eq.0) then
          cofs(0) = 0
          do j = 1, nbase
             cofs(j) = cofs(j) + cofs(j-1)
          enddo
       endif
    endif
#undef _WORK
#undef _CTAG
  end subroutine nio_restore_csr_f
  subroutine nio_restore_csr_i &
       & (ierr, d,  cofs,  &
       &  hd,   u,  krect, colc, flag, kopts)
    use TOUZA_Nio_record,only: laxs, rev_pos_leave
    use TOUZA_Nio_record,only: parse_header_base
    use TOUZA_Nio_record,only: ptx_read_data
    use TOUZA_Nio_record,only: ptx_gen_ccvec, ptx_pack_data
    use TOUZA_Nio_record,only: is_match_format, is_review_leave
    use TOUZA_Nio_record,only: PROP_PTX_DATA, PROP_PTX_MCOL
    use TOUZA_Nio_std,only: choice, msg
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)         :: ierr
    integer(kind=KARG),intent(out)         :: d(0:*)
    integer,           intent(inout)       :: cofs(0:*)  ! offset vector
    character(len=*),  intent(in)          :: hd(*)
    integer,           intent(in)          :: u
    integer,           intent(inout)       :: krect
    integer,           intent(in),optional :: colc
    integer,           intent(in),optional :: flag
    integer,           intent(in),optional :: kopts(:)   ! option cache

    integer popts(lopts_sparse)
    integer kaxs(laxs)
    integer kfmt
    integer npack, nbase
    real(kind=KRMIS) :: vmiss
    integer j

#define _WORK worki
#define _CTAG ctag_wi

    ierr = 0
    if (is_review_leave(flag)) then
       if (present(kopts)) then
          popts(1:lopts_sparse) = kopts(1:lopts_sparse)
       else
          call msg('Need property argument', __MDL__)
          ierr = _ERROR(ERR_INVALID_ITEM)
       endif
    else
       call nio_review_sparse &
            & (ierr,  popts, &
            &  hd,    u,     krect, d(0), colc, rev_pos_leave)
    endif

    if (ierr.eq.0) nbase = popts(PROP_SPARSE_ROWS)
    if (ierr.eq.0) npack = popts(PROP_PTX_DATA)

    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, hd)
    if (ierr.eq.0) then
       if (is_match_format(kfmt, st='P')) then
          call ptx_read_data &
               & (ierr,  &
               &  cofs(1:nbase), nbase, d,     npack, &
               &  popts,         u,     krect, kfmt)
       else
          if (ierr.eq.0) call ptx_gen_ccvec(ierr, popts, cofs(1:nbase), nbase, _WORK, kaxs, laxs, vmiss)
          if (ierr.eq.0) call ptx_pack_data(ierr, d, _WORK, cofs(1:nbase), kaxs, laxs, popts)
          if (ierr.eq.0) call alloc_work(ierr, -1, ' ', d(0))
       endif
       if (ierr.eq.0) then
          cofs(0) = 0
          do j = 1, nbase
             cofs(j) = cofs(j) + cofs(j-1)
          enddo
       endif
    endif
#undef _WORK
#undef _CTAG
  end subroutine nio_restore_csr_i

!!!_  & nio_store_csr
  subroutine nio_store_csr_d &
       & (ierr, d, cofs,  &
       &  hd,   u, krect, cname, kopts)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_record,only: laxs
    use TOUZA_Nio_record,only: nio_write_data, nio_write_header
    use TOUZA_Nio_record,only: ptx_def_options, ptx_write_data, ptx_expand_data
    use TOUZA_Nio_record,only: lopts_ptx
    use TOUZA_Nio_record,only: ptx_row_size
    use TOUZA_Nio_record,only: parse_header_base, is_match_format
    use TOUZA_Nio_header,only: litem, nitem
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(in)          :: d(0:*)
    integer,         intent(in)          :: cofs(0:*)  ! offset vector
    character(len=*),intent(in)          :: hd(*)
    integer,         intent(in)          :: u
    integer,         intent(inout)       :: krect
    character(len=*),intent(in),optional :: cname      ! column coordinate name
    integer,         intent(in),optional :: kopts(:)

    integer :: kfmt
    integer :: kaxs(laxs)
    integer :: popts(lopts_ptx)
    real(kind=KRMIS) :: vmiss
    integer :: mrow, ndata
    integer j
    integer nc

    character(len=litem) :: xhd(nitem)
    character(len=*),parameter :: proc='wcd'

#define _WORK workd

    ierr = 0
    xhd(1:nitem) = hd(1:nitem)

    call parse_header_base(ierr, kfmt, kaxs, vmiss, xhd)
    if (ierr.eq.0) call ptx_def_options(ierr, popts, kopts)
    if (ierr.eq.0) mrow = ptx_row_size(popts, kaxs, laxs)    ! need COLC only
    if (ierr.eq.0) call alloc_wsubv(ierr, mrow, proc)

    if (ierr.eq.0) then
       do j = 0, mrow - 1
          wsubv(j) = cofs(j+1) - cofs(j)
       enddo
       nc = maxval(wsubv(0:mrow-1))
    endif
    if (ierr.eq.0) then
       call tweak_header_coor &
            & (ierr, popts, xhd, nc, kfmt, kaxs, laxs, cname)
    endif

    if (ierr.eq.0) call nio_write_header(ierr, xhd, krect, u)
    if (ierr.eq.0) then
       if (is_match_format(kfmt, st='P')) then
          ndata = 0
          call ptx_write_data &
               & (ierr,  &
               &  popts, wsubv, mrow,  d,    ndata,   &
               &  u,     krect, kfmt)
       else
          ndata = nc * mrow
          call alloc_work(ierr, ndata, proc, d(0))
          if (ierr.eq.0) then
             call ptx_expand_data &
                  & (ierr, _WORK, d, wsubv, vmiss, kaxs, laxs, popts)
          endif
          if (ierr.eq.0) call nio_write_data(ierr, _WORK, ndata, xhd, krect, u)
          if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
       endif
    endif
    if (ierr.eq.0) call alloc_wsubv(ierr, -1, proc)
#undef _WORK
  end subroutine nio_store_csr_d
  subroutine nio_store_csr_f &
       & (ierr, d, cofs,  &
       &  hd,   u, krect, cname, kopts)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_record,only: laxs
    use TOUZA_Nio_record,only: nio_write_data, nio_write_header
    use TOUZA_Nio_record,only: ptx_def_options, ptx_write_data, ptx_expand_data
    use TOUZA_Nio_record,only: lopts_ptx
    use TOUZA_Nio_record,only: ptx_row_size
    use TOUZA_Nio_record,only: parse_header_base, is_match_format
    use TOUZA_Nio_header,only: litem, nitem
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(in)          :: d(0:*)
    integer,         intent(in)          :: cofs(0:*)  ! offset vector
    character(len=*),intent(in)          :: hd(*)
    integer,         intent(in)          :: u
    integer,         intent(inout)       :: krect
    character(len=*),intent(in),optional :: cname      ! column coordinate name
    integer,         intent(in),optional :: kopts(:)

    integer :: kfmt
    integer :: kaxs(laxs)
    integer :: popts(lopts_ptx)
    real(kind=KRMIS) :: vmiss
    integer :: mrow, ndata
    integer j
    integer nc

    character(len=litem) :: xhd(nitem)
    character(len=*),parameter :: proc='wcd'

#define _WORK workf

    ierr = 0
    xhd(1:nitem) = hd(1:nitem)

    call parse_header_base(ierr, kfmt, kaxs, vmiss, xhd)
    if (ierr.eq.0) call ptx_def_options(ierr, popts, kopts)
    if (ierr.eq.0) mrow = ptx_row_size(popts, kaxs, laxs)    ! need COLC only
    if (ierr.eq.0) call alloc_wsubv(ierr, mrow, proc)

    if (ierr.eq.0) then
       do j = 0, mrow - 1
          wsubv(j) = cofs(j+1) - cofs(j)
       enddo
       nc = maxval(wsubv(0:mrow-1))
    endif
    if (ierr.eq.0) then
       call tweak_header_coor &
            & (ierr, popts, xhd, nc, kfmt, kaxs, laxs, cname)
    endif

    if (ierr.eq.0) call nio_write_header(ierr, xhd, krect, u)
    if (ierr.eq.0) then
       if (is_match_format(kfmt, st='P')) then
          ndata = 0
          call ptx_write_data &
               & (ierr,  &
               &  popts, wsubv, mrow,  d,    ndata,   &
               &  u,     krect, kfmt)
       else
          ndata = nc * mrow
          call alloc_work(ierr, ndata, proc, d(0))
          if (ierr.eq.0) then
             call ptx_expand_data &
                  & (ierr, _WORK, d, wsubv, vmiss, kaxs, laxs, popts)
          endif
          if (ierr.eq.0) call nio_write_data(ierr, _WORK, ndata, xhd, krect, u)
          if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
       endif
    endif
    if (ierr.eq.0) call alloc_wsubv(ierr, -1, proc)
#undef _WORK
  end subroutine nio_store_csr_f
  subroutine nio_store_csr_i &
       & (ierr, d, cofs,  &
       &  hd,   u, krect, cname, kopts)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_record,only: laxs
    use TOUZA_Nio_record,only: nio_write_data, nio_write_header
    use TOUZA_Nio_record,only: ptx_def_options, ptx_write_data, ptx_expand_data
    use TOUZA_Nio_record,only: lopts_ptx
    use TOUZA_Nio_record,only: ptx_row_size
    use TOUZA_Nio_record,only: parse_header_base, is_match_format
    use TOUZA_Nio_header,only: litem, nitem
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)         :: ierr
    integer(kind=KARG),intent(in)          :: d(0:*)
    integer,           intent(in)          :: cofs(0:*)  ! offset vector
    character(len=*),  intent(in)          :: hd(*)
    integer,           intent(in)          :: u
    integer,           intent(inout)       :: krect
    character(len=*),  intent(in),optional :: cname      ! column coordinate name
    integer,           intent(in),optional :: kopts(:)

    integer :: kfmt
    integer :: kaxs(laxs)
    integer :: popts(lopts_ptx)
    real(kind=KRMIS) :: vmiss
    integer :: mrow, ndata
    integer j
    integer nc

    character(len=litem) :: xhd(nitem)
    character(len=*),parameter :: proc='wcd'

#define _WORK worki

    ierr = 0
    xhd(1:nitem) = hd(1:nitem)

    call parse_header_base(ierr, kfmt, kaxs, vmiss, xhd)
    if (ierr.eq.0) call ptx_def_options(ierr, popts, kopts)
    if (ierr.eq.0) mrow = ptx_row_size(popts, kaxs, laxs)    ! need COLC only
    if (ierr.eq.0) call alloc_wsubv(ierr, mrow, proc)

    if (ierr.eq.0) then
       do j = 0, mrow - 1
          wsubv(j) = cofs(j+1) - cofs(j)
       enddo
       nc = maxval(wsubv(0:mrow-1))
    endif
    if (ierr.eq.0) then
       call tweak_header_coor &
            & (ierr, popts, xhd, nc, kfmt, kaxs, laxs, cname)
    endif

    if (ierr.eq.0) call nio_write_header(ierr, xhd, krect, u)
    if (ierr.eq.0) then
       if (is_match_format(kfmt, st='P')) then
          ndata = 0
          call ptx_write_data &
               & (ierr,  &
               &  popts, wsubv, mrow,  d,    ndata,   &
               &  u,     krect, kfmt)
       else
          ndata = nc * mrow
          call alloc_work(ierr, ndata, proc, d(0))
          if (ierr.eq.0) then
             call ptx_expand_data &
                  & (ierr, _WORK, d, wsubv, vmiss, kaxs, laxs, popts)
          endif
          if (ierr.eq.0) call nio_write_data(ierr, _WORK, ndata, xhd, krect, u)
          if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
       endif
    endif
    if (ierr.eq.0) call alloc_wsubv(ierr, -1, proc)
#undef _WORK
  end subroutine nio_store_csr_i

!!!_ + COO+JDS-like storage procedures
!!!_  & nio_restore_qjds
  subroutine nio_restore_qjds_d &
       & (ierr, d,  ridx,  cofs, &
       &  hd,   u,  krect, colc, flag, kopts)
    use TOUZA_Nio_record,only: laxs, rev_pos_leave
    use TOUZA_Nio_record,only: parse_header_base
    use TOUZA_Nio_record,only: ptx_read_data
    use TOUZA_Nio_record,only: ptx_gen_ccvec, ptx_pack_data
    use TOUZA_Nio_record,only: is_match_format, is_review_leave
    use TOUZA_Nio_record,only: PROP_PTX_DATA, PROP_PTX_MCOL
    use TOUZA_Nio_std,only: choice, msg
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(out)         :: d(0:*)
    integer,         intent(out)         :: ridx(0:*)
    integer,         intent(out)         :: cofs(0:*)  ! offset vector
    character(len=*),intent(in)          :: hd(*)
    integer,         intent(in)          :: u
    integer,         intent(inout)       :: krect
    integer,         intent(in),optional :: colc
    integer,         intent(in),optional :: flag
    integer,         intent(in),optional :: kopts(:)   ! option cache

    integer popts(lopts_sparse)
    integer kaxs(laxs)
    integer kfmt
    integer npack, nbase, mcol
    real(kind=KRMIS) :: vmiss

    character(len=*),parameter :: proc='wqd'
#define _WORK workd
#define _CTAG ctag_wd

    ierr = 0
    if (is_review_leave(flag)) then
       if (present(kopts)) then
          popts(1:lopts_sparse) = kopts(1:lopts_sparse)
       else
          call msg('Need property argument', __MDL__)
          ierr = _ERROR(ERR_INVALID_ITEM)
       endif
    else
       call nio_review_sparse &
            & (ierr,  popts, &
            &  hd,    u,     krect, d(0), colc, rev_pos_leave)
    endif

    if (ierr.eq.0) nbase = popts(PROP_SPARSE_ROWS)
    if (ierr.eq.0) npack = popts(PROP_PTX_DATA)
    if (ierr.eq.0) mcol  = popts(PROP_PTX_MCOL)

    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, hd)
    if (ierr.eq.0) then
       if (is_match_format(kfmt, st='P')) then
          if (ierr.eq.0) call alloc_wsubv(ierr, nbase, proc)
          if (ierr.eq.0) call alloc_work(ierr, npack, proc, d(0))
          if (ierr.eq.0) then
             call ptx_read_data &
                  & (ierr,  &
                  &  wsubv, nbase, _WORK, npack, &
                  &  popts, u,     krect, kfmt)
          endif
          if (ierr.eq.0) then
             call ptx2qjds &
                  & (d, ridx, cofs, mcol, nbase, wsubv, _WORK)
          endif
          if (ierr.eq.0) call alloc_wsubv(ierr, -1, proc)
          if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
       else
          call pack_qjds &
               & (d,    ridx, cofs, &
               &  mcol, kaxs, laxs, popts, vmiss, _WORK)
          if (ierr.eq.0) call alloc_work(ierr, -1, ' ', d(0))
       endif
    endif
#undef _WORK
#undef _CTAG
  end subroutine nio_restore_qjds_d
  subroutine nio_restore_qjds_f &
       & (ierr, d,  ridx,  cofs, &
       &  hd,   u,  krect, colc, flag, kopts)
    use TOUZA_Nio_record,only: laxs, rev_pos_leave
    use TOUZA_Nio_record,only: parse_header_base
    use TOUZA_Nio_record,only: ptx_read_data
    use TOUZA_Nio_record,only: ptx_gen_ccvec, ptx_pack_data
    use TOUZA_Nio_record,only: is_match_format, is_review_leave
    use TOUZA_Nio_record,only: PROP_PTX_DATA, PROP_PTX_MCOL
    use TOUZA_Nio_std,only: choice, msg
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(out)         :: d(0:*)
    integer,         intent(out)         :: ridx(0:*)
    integer,         intent(out)         :: cofs(0:*)  ! offset vector
    character(len=*),intent(in)          :: hd(*)
    integer,         intent(in)          :: u
    integer,         intent(inout)       :: krect
    integer,         intent(in),optional :: colc
    integer,         intent(in),optional :: flag
    integer,         intent(in),optional :: kopts(:)   ! option cache

    integer popts(lopts_sparse)
    integer kaxs(laxs)
    integer kfmt
    integer npack, nbase, mcol
    real(kind=KRMIS) :: vmiss

    character(len=*),parameter :: proc='wqd'
#define _WORK workf
#define _CTAG ctag_wf

    ierr = 0
    if (is_review_leave(flag)) then
       if (present(kopts)) then
          popts(1:lopts_sparse) = kopts(1:lopts_sparse)
       else
          call msg('Need property argument', __MDL__)
          ierr = _ERROR(ERR_INVALID_ITEM)
       endif
    else
       call nio_review_sparse &
            & (ierr,  popts, &
            &  hd,    u,     krect, d(0), colc, rev_pos_leave)
    endif

    if (ierr.eq.0) nbase = popts(PROP_SPARSE_ROWS)
    if (ierr.eq.0) npack = popts(PROP_PTX_DATA)
    if (ierr.eq.0) mcol  = popts(PROP_PTX_MCOL)

    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, hd)
    if (ierr.eq.0) then
       if (is_match_format(kfmt, st='P')) then
          if (ierr.eq.0) call alloc_wsubv(ierr, nbase, proc)
          if (ierr.eq.0) call alloc_work(ierr, npack, proc, d(0))
          if (ierr.eq.0) then
             call ptx_read_data &
                  & (ierr,  &
                  &  wsubv, nbase, _WORK, npack, &
                  &  popts, u,     krect, kfmt)
          endif
          if (ierr.eq.0) then
             call ptx2qjds &
                  & (d, ridx, cofs, mcol, nbase, wsubv, _WORK)
          endif
          if (ierr.eq.0) call alloc_wsubv(ierr, -1, proc)
          if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
       else
          call pack_qjds &
               & (d,    ridx, cofs, &
               &  mcol, kaxs, laxs, popts, vmiss, _WORK)
          if (ierr.eq.0) call alloc_work(ierr, -1, ' ', d(0))
       endif
    endif
#undef _WORK
#undef _CTAG
  end subroutine nio_restore_qjds_f
  subroutine nio_restore_qjds_i &
       & (ierr, d,  ridx,  cofs, &
       &  hd,   u,  krect, colc, flag, kopts)
    use TOUZA_Nio_record,only: laxs, rev_pos_leave
    use TOUZA_Nio_record,only: parse_header_base
    use TOUZA_Nio_record,only: ptx_read_data
    use TOUZA_Nio_record,only: ptx_gen_ccvec, ptx_pack_data
    use TOUZA_Nio_record,only: is_match_format, is_review_leave
    use TOUZA_Nio_record,only: PROP_PTX_DATA, PROP_PTX_MCOL
    use TOUZA_Nio_std,only: choice, msg
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)         :: ierr
    integer(kind=KARG),intent(out)         :: d(0:*)
    integer,           intent(out)         :: ridx(0:*)
    integer,           intent(out)         :: cofs(0:*)  ! offset vector
    character(len=*),  intent(in)          :: hd(*)
    integer,           intent(in)          :: u
    integer,           intent(inout)       :: krect
    integer,           intent(in),optional :: colc
    integer,           intent(in),optional :: flag
    integer,           intent(in),optional :: kopts(:)   ! option cache

    integer popts(lopts_sparse)
    integer kaxs(laxs)
    integer kfmt
    integer npack, nbase, mcol
    real(kind=KRMIS) :: vmiss

    character(len=*),parameter :: proc='wqd'
#define _WORK worki
#define _CTAG ctag_wi

    ierr = 0
    if (is_review_leave(flag)) then
       if (present(kopts)) then
          popts(1:lopts_sparse) = kopts(1:lopts_sparse)
       else
          call msg('Need property argument', __MDL__)
          ierr = _ERROR(ERR_INVALID_ITEM)
       endif
    else
       call nio_review_sparse &
            & (ierr,  popts, &
            &  hd,    u,     krect, d(0), colc, rev_pos_leave)
    endif

    if (ierr.eq.0) nbase = popts(PROP_SPARSE_ROWS)
    if (ierr.eq.0) npack = popts(PROP_PTX_DATA)
    if (ierr.eq.0) mcol  = popts(PROP_PTX_MCOL)

    if (ierr.eq.0) call parse_header_base(ierr, kfmt, kaxs, vmiss, hd)
    if (ierr.eq.0) then
       if (is_match_format(kfmt, st='P')) then
          if (ierr.eq.0) call alloc_wsubv(ierr, nbase, proc)
          if (ierr.eq.0) call alloc_work(ierr, npack, proc, d(0))
          if (ierr.eq.0) then
             call ptx_read_data &
                  & (ierr,  &
                  &  wsubv, nbase, _WORK, npack, &
                  &  popts, u,     krect, kfmt)
          endif
          if (ierr.eq.0) then
             call ptx2qjds &
                  & (d, ridx, cofs, mcol, nbase, wsubv, _WORK)
          endif
          if (ierr.eq.0) call alloc_wsubv(ierr, -1, proc)
          if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
       else
          call pack_qjds &
               & (d,    ridx, cofs, &
               &  mcol, kaxs, laxs, popts, vmiss, _WORK)
          if (ierr.eq.0) call alloc_work(ierr, -1, ' ', d(0))
       endif
    endif
#undef _WORK
#undef _CTAG
  end subroutine nio_restore_qjds_i

!!!_  & nio_store_qjds
  subroutine nio_store_qjds_d &
       & (ierr, d, ridx,  posh,  nvec, &
       &  hd,   u, krect, cname, kopts)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_record,only: laxs
    use TOUZA_Nio_record,only: nio_write_data, nio_write_header
    use TOUZA_Nio_record,only: ptx_def_options, ptx_write_data, ptx_expand_data
    use TOUZA_Nio_record,only: lopts_ptx
    use TOUZA_Nio_record,only: ptx_row_size
    use TOUZA_Nio_record,only: parse_header_base, is_match_format
    use TOUZA_Nio_header,only: litem, nitem
    implicit none
    integer,parameter :: KARG=KDBL
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(in)          :: d(0:*)
    integer,         intent(in)          :: ridx(0:*)  ! row index
    integer,         intent(in)          :: posh(0:*)  ! head/end position
    integer,         intent(in)          :: nvec       ! posh size
    character(len=*),intent(in)          :: hd(*)
    integer,         intent(in)          :: u
    integer,         intent(inout)       :: krect
    character(len=*),intent(in),optional :: cname      ! column coordinate name
    integer,         intent(in),optional :: kopts(:)

    integer :: kfmt
    integer :: kaxs(laxs)
    integer :: popts(lopts_ptx)
    real(kind=KRMIS) :: vmiss
    integer :: mrow, ndata
    integer nc

    character(len=litem) :: xhd(nitem)
    character(len=*),parameter :: proc='wqd'

#define _WORK workd

    ierr = 0
    xhd(1:nitem) = hd(1:nitem)

    call parse_header_base(ierr, kfmt, kaxs, vmiss, xhd)
    if (ierr.eq.0) call ptx_def_options(ierr, popts, kopts)
    if (ierr.eq.0) mrow = ptx_row_size(popts, kaxs, laxs)    ! need COLC only

    if (ierr.eq.0) call alloc_wsubv(ierr, mrow + 1, proc)
    if (ierr.eq.0) then
       ndata = posh(nvec)
       wsubv(0) = 0
       call ridx2count(nc, wsubv(1), mrow, ridx, ndata)
    endif
    if (ierr.eq.0) then
       call tweak_header_coor &
            & (ierr, popts, xhd, nc, kfmt, kaxs, laxs, cname)
    endif

    if (ierr.eq.0) then
       if (is_match_format(kfmt, st='P')) then
          if (ierr.eq.0) call alloc_work(ierr, ndata, proc, d(0))
          if (ierr.eq.0) then
             call qjds2csr &
                  & (_WORK, wsubv, mrow, ridx, ndata, d)
          endif
          if (ierr.eq.0) call nio_write_header(ierr, xhd, krect, u)
          if (ierr.eq.0) then
             call ptx_write_data &
                  & (ierr,  &
                  &  popts, wsubv, mrow,  _WORK,    ndata,   &
                  &  u,     krect, kfmt)
          endif
       else
          ndata = mrow * nc
          if (ierr.eq.0) call alloc_work(ierr, ndata, proc, d(0))
          if (ierr.eq.0) then
             call expand_qjds   &
                  & (_WORK, nc, &
                  &  wsubv(1:), mrow, ridx, posh, nvec, kaxs, laxs, popts, vmiss, d)
          endif
          if (ierr.eq.0) call nio_write_header(ierr, xhd, krect, u)
          if (ierr.eq.0) call nio_write_data(ierr, _WORK, ndata, xhd, krect, u)
       endif
    endif
    if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
    if (ierr.eq.0) call alloc_wsubv(ierr, -1, proc)

#undef _WORK
  end subroutine nio_store_qjds_d
  subroutine nio_store_qjds_f &
       & (ierr, d, ridx,  posh,  nvec, &
       &  hd,   u, krect, cname, kopts)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_record,only: laxs
    use TOUZA_Nio_record,only: nio_write_data, nio_write_header
    use TOUZA_Nio_record,only: ptx_def_options, ptx_write_data, ptx_expand_data
    use TOUZA_Nio_record,only: lopts_ptx
    use TOUZA_Nio_record,only: ptx_row_size
    use TOUZA_Nio_record,only: parse_header_base, is_match_format
    use TOUZA_Nio_header,only: litem, nitem
    implicit none
    integer,parameter :: KARG=KFLT
    integer,         intent(out)         :: ierr
    real(kind=KARG), intent(in)          :: d(0:*)
    integer,         intent(in)          :: ridx(0:*)  ! row index
    integer,         intent(in)          :: posh(0:*)  ! head/end position
    integer,         intent(in)          :: nvec       ! posh size
    character(len=*),intent(in)          :: hd(*)
    integer,         intent(in)          :: u
    integer,         intent(inout)       :: krect
    character(len=*),intent(in),optional :: cname      ! column coordinate name
    integer,         intent(in),optional :: kopts(:)

    integer :: kfmt
    integer :: kaxs(laxs)
    integer :: popts(lopts_ptx)
    real(kind=KRMIS) :: vmiss
    integer :: mrow, ndata
    integer nc

    character(len=litem) :: xhd(nitem)
    character(len=*),parameter :: proc='wqd'

#define _WORK workf

    ierr = 0
    xhd(1:nitem) = hd(1:nitem)

    call parse_header_base(ierr, kfmt, kaxs, vmiss, xhd)
    if (ierr.eq.0) call ptx_def_options(ierr, popts, kopts)
    if (ierr.eq.0) mrow = ptx_row_size(popts, kaxs, laxs)    ! need COLC only

    if (ierr.eq.0) call alloc_wsubv(ierr, mrow + 1, proc)
    if (ierr.eq.0) then
       ndata = posh(nvec)
       wsubv(0) = 0
       call ridx2count(nc, wsubv(1), mrow, ridx, ndata)
    endif
    if (ierr.eq.0) then
       call tweak_header_coor &
            & (ierr, popts, xhd, nc, kfmt, kaxs, laxs, cname)
    endif

    if (ierr.eq.0) then
       if (is_match_format(kfmt, st='P')) then
          if (ierr.eq.0) call alloc_work(ierr, ndata, proc, d(0))
          if (ierr.eq.0) then
             call qjds2csr &
                  & (_WORK, wsubv, mrow, ridx, ndata, d)
          endif
          if (ierr.eq.0) call nio_write_header(ierr, xhd, krect, u)
          if (ierr.eq.0) then
             call ptx_write_data &
                  & (ierr,  &
                  &  popts, wsubv, mrow,  _WORK,    ndata,   &
                  &  u,     krect, kfmt)
          endif
       else
          ndata = mrow * nc
          if (ierr.eq.0) call alloc_work(ierr, ndata, proc, d(0))
          if (ierr.eq.0) then
             call expand_qjds   &
                  & (_WORK, nc, &
                  &  wsubv(1:), mrow, ridx, posh, nvec, kaxs, laxs, popts, vmiss, d)
          endif
          if (ierr.eq.0) call nio_write_header(ierr, xhd, krect, u)
          if (ierr.eq.0) call nio_write_data(ierr, _WORK, ndata, xhd, krect, u)
       endif
    endif
    if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
    if (ierr.eq.0) call alloc_wsubv(ierr, -1, proc)

#undef _WORK
  end subroutine nio_store_qjds_f
  subroutine nio_store_qjds_i &
       & (ierr, d, ridx,  posh,  nvec, &
       &  hd,   u, krect, cname, kopts)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_record,only: laxs
    use TOUZA_Nio_record,only: nio_write_data, nio_write_header
    use TOUZA_Nio_record,only: ptx_def_options, ptx_write_data, ptx_expand_data
    use TOUZA_Nio_record,only: lopts_ptx
    use TOUZA_Nio_record,only: ptx_row_size
    use TOUZA_Nio_record,only: parse_header_base, is_match_format
    use TOUZA_Nio_header,only: litem, nitem
    implicit none
    integer,parameter :: KARG=KI32
    integer,           intent(out)         :: ierr
    integer(kind=KARG),intent(in)          :: d(0:*)
    integer,           intent(in)          :: ridx(0:*)  ! row index
    integer,           intent(in)          :: posh(0:*)  ! head/end position
    integer,           intent(in)          :: nvec       ! posh size
    character(len=*),  intent(in)          :: hd(*)
    integer,           intent(in)          :: u
    integer,           intent(inout)       :: krect
    character(len=*),  intent(in),optional :: cname      ! column coordinate name
    integer,           intent(in),optional :: kopts(:)

    integer :: kfmt
    integer :: kaxs(laxs)
    integer :: popts(lopts_ptx)
    real(kind=KRMIS) :: vmiss
    integer :: mrow, ndata
    integer nc

    character(len=litem) :: xhd(nitem)
    character(len=*),parameter :: proc='wqd'

#define _WORK worki

    ierr = 0
    xhd(1:nitem) = hd(1:nitem)

    call parse_header_base(ierr, kfmt, kaxs, vmiss, xhd)
    if (ierr.eq.0) call ptx_def_options(ierr, popts, kopts)
    if (ierr.eq.0) mrow = ptx_row_size(popts, kaxs, laxs)    ! need COLC only

    if (ierr.eq.0) call alloc_wsubv(ierr, mrow + 1, proc)
    if (ierr.eq.0) then
       ndata = posh(nvec)
       wsubv(0) = 0
       call ridx2count(nc, wsubv(1), mrow, ridx, ndata)
    endif
    if (ierr.eq.0) then
       call tweak_header_coor &
            & (ierr, popts, xhd, nc, kfmt, kaxs, laxs, cname)
    endif

    if (ierr.eq.0) then
       if (is_match_format(kfmt, st='P')) then
          if (ierr.eq.0) call alloc_work(ierr, ndata, proc, d(0))
          if (ierr.eq.0) then
             call qjds2csr &
                  & (_WORK, wsubv, mrow, ridx, ndata, d)
          endif
          if (ierr.eq.0) call nio_write_header(ierr, xhd, krect, u)
          if (ierr.eq.0) then
             call ptx_write_data &
                  & (ierr,  &
                  &  popts, wsubv, mrow,  _WORK,    ndata,   &
                  &  u,     krect, kfmt)
          endif
       else
          ndata = mrow * nc
          if (ierr.eq.0) call alloc_work(ierr, ndata, proc, d(0))
          if (ierr.eq.0) then
             call expand_qjds   &
                  & (_WORK, nc, &
                  &  wsubv(1:), mrow, ridx, posh, nvec, kaxs, laxs, popts, vmiss, d)
          endif
          if (ierr.eq.0) call nio_write_header(ierr, xhd, krect, u)
          if (ierr.eq.0) call nio_write_data(ierr, _WORK, ndata, xhd, krect, u)
       endif
    endif
    if (ierr.eq.0) call alloc_work(ierr, -1, proc, d(0))
    if (ierr.eq.0) call alloc_wsubv(ierr, -1, proc)

#undef _WORK
  end subroutine nio_store_qjds_i
!!!_  & qjds2csr
  subroutine qjds2csr_d &
       & (ddest, cco, mrow, ridx, ndata, dsrc)
    implicit none
    integer,parameter :: KARG=KDBL
    real(kind=KARG),intent(out)   :: ddest(0:*)
    integer,        intent(inout) :: cco(0:*)     ! input starts from 1
    integer,        intent(in)    :: mrow
    integer,        intent(in)    :: ridx(0:*)
    integer,        intent(in)    :: ndata
    real(kind=KARG),intent(in)    :: dsrc(0:*)
    integer jr, j

    do jr = 1, mrow
       cco(jr) = cco(jr) + cco(jr-1)
    enddo
    do j = 0, ndata - 1
       jr = ridx(j)
       ddest(cco(jr)) = dsrc(j)
       cco(jr) = cco(jr) + 1
    enddo
    do jr = mrow - 1, 1, -1
       cco(jr) = cco(jr) - cco(jr-1)
    enddo
  end subroutine qjds2csr_d
  subroutine qjds2csr_f &
       & (ddest, cco, mrow, ridx, ndata, dsrc)
    implicit none
    integer,parameter :: KARG=KFLT
    real(kind=KARG),intent(out)   :: ddest(0:*)
    integer,        intent(inout) :: cco(0:*)     ! input starts from 1
    integer,        intent(in)    :: mrow
    integer,        intent(in)    :: ridx(0:*)
    integer,        intent(in)    :: ndata
    real(kind=KARG),intent(in)    :: dsrc(0:*)
    integer jr, j

    do jr = 1, mrow
       cco(jr) = cco(jr) + cco(jr-1)
    enddo
    do j = 0, ndata - 1
       jr = ridx(j)
       ddest(cco(jr)) = dsrc(j)
       cco(jr) = cco(jr) + 1
    enddo
    do jr = mrow - 1, 1, -1
       cco(jr) = cco(jr) - cco(jr-1)
    enddo
  end subroutine qjds2csr_f
  subroutine qjds2csr_i &
       & (ddest, cco, mrow, ridx, ndata, dsrc)
    implicit none
    integer,parameter :: KARG=KI32
    integer(kind=KARG),intent(out)   :: ddest(0:*)
    integer,           intent(inout) :: cco(0:*)     ! input starts from 1
    integer,           intent(in)    :: mrow
    integer,           intent(in)    :: ridx(0:*)
    integer,           intent(in)    :: ndata
    integer(kind=KARG),intent(in)    :: dsrc(0:*)
    integer jr, j

    do jr = 1, mrow
       cco(jr) = cco(jr) + cco(jr-1)
    enddo
    do j = 0, ndata - 1
       jr = ridx(j)
       ddest(cco(jr)) = dsrc(j)
       cco(jr) = cco(jr) + 1
    enddo
    do jr = mrow - 1, 1, -1
       cco(jr) = cco(jr) - cco(jr-1)
    enddo
  end subroutine qjds2csr_i
!!!_  & ptx2qjds
  subroutine ptx2qjds_d &
       & (ddest, ridx, colv, mcol, mrow, cco, dsrc)
    implicit none
    integer,parameter :: KARG=KDBL
    real(kind=KARG),intent(out) :: ddest(0:*)
    integer,        intent(out) :: ridx(0:*)
    integer,        intent(out) :: colv(0:*)
    integer,        intent(in)  :: mcol
    integer,        intent(in)  :: mrow
    integer,        intent(in)  :: cco(0:*)
    real(kind=KARG),intent(in)  :: dsrc(0:*)
    integer jo, j, jc, c, jd

    colv(0:mcol) = 0

    do j = 0, mrow - 1
       c = min(mcol - 1, cco(j)) + 1
       colv(2:c) = colv(2:c) + 1
    enddo
    do jc = 2, mcol
       colv(jc) = colv(jc) + colv(jc-1)
    enddo
    jd = 0
    jo = 0
    do j = 0, mrow - 1
       c = cco(j)
       do jc = 0, c - 1
          jd = colv(jc+1)
          ddest(jd) = dsrc(jo + jc)
          ridx(jd)  = j
          colv(jc+1) = colv(jc+1) + 1
       enddo
       jo = jo + c
    enddo
  end subroutine ptx2qjds_d
  subroutine ptx2qjds_f &
       & (ddest, ridx, colv, mcol, mrow, cco, dsrc)
    implicit none
    integer,parameter :: KARG=KFLT
    real(kind=KARG),intent(out) :: ddest(0:*)
    integer,        intent(out) :: ridx(0:*)
    integer,        intent(out) :: colv(0:*)
    integer,        intent(in)  :: mcol
    integer,        intent(in)  :: mrow
    integer,        intent(in)  :: cco(0:*)
    real(kind=KARG),intent(in)  :: dsrc(0:*)
    integer jo, j, jc, c, jd

    colv(0:mcol) = 0

    do j = 0, mrow - 1
       c = min(mcol - 1, cco(j)) + 1
       colv(2:c) = colv(2:c) + 1
    enddo
    do jc = 2, mcol
       colv(jc) = colv(jc) + colv(jc-1)
    enddo
    jd = 0
    jo = 0
    do j = 0, mrow - 1
       c = cco(j)
       do jc = 0, c - 1
          jd = colv(jc+1)
          ddest(jd) = dsrc(jo + jc)
          ridx(jd)  = j
          colv(jc+1) = colv(jc+1) + 1
       enddo
       jo = jo + c
    enddo
  end subroutine ptx2qjds_f
  subroutine ptx2qjds_i &
       & (ddest, ridx, colv, mcol, mrow, cco, dsrc)
    implicit none
    integer,parameter :: KARG=KI32
    integer(kind=KARG),intent(out) :: ddest(0:*)
    integer,           intent(out) :: ridx(0:*)
    integer,           intent(out) :: colv(0:*)
    integer,           intent(in)  :: mcol
    integer,           intent(in)  :: mrow
    integer,           intent(in)  :: cco(0:*)
    integer(kind=KARG),intent(in)  :: dsrc(0:*)
    integer jo, j, jc, c, jd

    colv(0:mcol) = 0

    do j = 0, mrow - 1
       c = min(mcol - 1, cco(j)) + 1
       colv(2:c) = colv(2:c) + 1
    enddo
    do jc = 2, mcol
       colv(jc) = colv(jc) + colv(jc-1)
    enddo
    jd = 0
    jo = 0
    do j = 0, mrow - 1
       c = cco(j)
       do jc = 0, c - 1
          jd = colv(jc+1)
          ddest(jd) = dsrc(jo + jc)
          ridx(jd)  = j
          colv(jc+1) = colv(jc+1) + 1
       enddo
       jo = jo + c
    enddo
  end subroutine ptx2qjds_i
!!!_  & expand_qjds
  subroutine expand_qjds_d &
       & (ddest, &
       &  ncol,  cco, mrow,  ridx,  posh, nvec, &
       &  xmems, mx,  popts, vmiss, dsrc)
    use TOUZA_Nio_record,only: ptx_set_loops
    implicit none
    integer,parameter :: KARG=KDBL
    real(kind=KARG), intent(out) :: ddest(0:*)
    integer,         intent(in)  :: ncol
    integer,         intent(in)  :: cco(0:*)
    integer,         intent(in)  :: mrow
    integer,         intent(in)  :: ridx(0:*)
    integer,         intent(in)  :: posh(0:*)
    integer,         intent(in)  :: nvec
    integer,         intent(in)  :: xmems(*)
    integer,         intent(in)  :: mx
    integer,         intent(in)  :: popts(*)
    real(kind=KRMIS),intent(in)  :: vmiss
    real(kind=KARG), intent(in)  :: dsrc(0:*)
    integer jr, jh
    integer jw, jv
    integer ji, jm, jo
    integer ni, nm, no
    real(kind=KARG) :: vu

    vu = real(vmiss, kind=KARG)
    call ptx_set_loops(ni, nm, no, xmems, mx, popts)

    do jr = 0, mrow - 1
       ji = mod(jr, ni)
       jo = jr / ni
       do jm = cco(jr), ncol - 1
          jw = (jo * nm + jm) * ni + ji
          ddest(jw) = vu
       enddo
    enddo
    do jv = 0, nvec - 1
       do jh = posh(jv), posh(jv+1) - 1
          jr = ridx(jh)
          ji = mod(jr, ni)
          jm = jv
          jo = jr / ni
          jw = (jo * nm + jm) * ni + ji
          ddest(jw) = dsrc(jh)
       enddo
    enddo
  end subroutine expand_qjds_d
  subroutine expand_qjds_f &
       & (ddest, &
       &  ncol,  cco, mrow,  ridx,  posh, nvec, &
       &  xmems, mx,  popts, vmiss, dsrc)
    use TOUZA_Nio_record,only: ptx_set_loops
    implicit none
    integer,parameter :: KARG=KFLT
    real(kind=KARG), intent(out) :: ddest(0:*)
    integer,         intent(in)  :: ncol
    integer,         intent(in)  :: cco(0:*)
    integer,         intent(in)  :: mrow
    integer,         intent(in)  :: ridx(0:*)
    integer,         intent(in)  :: posh(0:*)
    integer,         intent(in)  :: nvec
    integer,         intent(in)  :: xmems(*)
    integer,         intent(in)  :: mx
    integer,         intent(in)  :: popts(*)
    real(kind=KRMIS),intent(in)  :: vmiss
    real(kind=KARG), intent(in)  :: dsrc(0:*)
    integer jr, jh
    integer jw, jv
    integer ji, jm, jo
    integer ni, nm, no
    real(kind=KARG) :: vu

    vu = real(vmiss, kind=KARG)
    call ptx_set_loops(ni, nm, no, xmems, mx, popts)

    do jr = 0, mrow - 1
       ji = mod(jr, ni)
       jo = jr / ni
       do jm = cco(jr), ncol - 1
          jw = (jo * nm + jm) * ni + ji
          ddest(jw) = vu
       enddo
    enddo
    do jv = 0, nvec - 1
       do jh = posh(jv), posh(jv+1) - 1
          jr = ridx(jh)
          ji = mod(jr, ni)
          jm = jv
          jo = jr / ni
          jw = (jo * nm + jm) * ni + ji
          ddest(jw) = dsrc(jh)
       enddo
    enddo
  end subroutine expand_qjds_f
  subroutine expand_qjds_i &
       & (ddest, &
       &  ncol,  cco, mrow,  ridx,  posh, nvec, &
       &  xmems, mx,  popts, vmiss, dsrc)
    use TOUZA_Nio_record,only: ptx_set_loops
    implicit none
    integer,parameter :: KARG=KI32
    integer(kind=KARG),intent(out) :: ddest(0:*)
    integer,           intent(in)  :: ncol
    integer,           intent(in)  :: cco(0:*)
    integer,           intent(in)  :: mrow
    integer,           intent(in)  :: ridx(0:*)
    integer,           intent(in)  :: posh(0:*)
    integer,           intent(in)  :: nvec
    integer,           intent(in)  :: xmems(*)
    integer,           intent(in)  :: mx
    integer,           intent(in)  :: popts(*)
    real(kind=KRMIS),  intent(in)  :: vmiss
    integer(kind=KARG),intent(in)  :: dsrc(0:*)
    integer jr, jh
    integer jw, jv
    integer ji, jm, jo
    integer ni, nm, no
    integer(kind=KARG) :: vu

    vu = int(vmiss, kind=KARG)
    call ptx_set_loops(ni, nm, no, xmems, mx, popts)

    do jr = 0, mrow - 1
       ji = mod(jr, ni)
       jo = jr / ni
       do jm = cco(jr), ncol - 1
          jw = (jo * nm + jm) * ni + ji
          ddest(jw) = vu
       enddo
    enddo
    do jv = 0, nvec - 1
       do jh = posh(jv), posh(jv+1) - 1
          jr = ridx(jh)
          ji = mod(jr, ni)
          jm = jv
          jo = jr / ni
          jw = (jo * nm + jm) * ni + ji
          ddest(jw) = dsrc(jh)
       enddo
    enddo
  end subroutine expand_qjds_i
!!!_  & pack_qjds
  subroutine pack_qjds_d &
       & (ddest, ridx, posh,  &
       &  mcol, &
       &  xmems, mx,   popts, vmiss, dsrc)
    use TOUZA_Nio_record,only: ptx_set_loops
    implicit none
    integer,parameter :: KARG=KDBL
    real(kind=KARG), intent(out) :: ddest(0:*)
    integer,         intent(out) :: ridx(0:*)
    integer,         intent(out) :: posh(0:*)
    integer,         intent(in)  :: mcol
    integer,         intent(in)  :: xmems(*)
    integer,         intent(in)  :: mx
    integer,         intent(in)  :: popts(*)
    real(kind=KRMIS),intent(in)  :: vmiss
    real(kind=KARG), intent(in)  :: dsrc(0:*)
    integer jw, jv
    integer ji, jm, jo
    integer ni, nm, no
    real(kind=KARG) :: vu

    vu = real(vmiss, kind=KARG)
    call ptx_set_loops(ni, nm, no, xmems, mx, popts)

    posh(0:mcol) = 0
    do jo = 0, no - 1
       do jm = 0, nm - 2
          do ji = 0, ni - 1
             jw = (jo * nm + jm) * ni + ji
             if (dsrc(jw).ne.vmiss) posh(jm+2) = posh(jm+2) + 1
          enddo
       enddo
    enddo
    do jm = 2, nm
       posh(jm) = posh(jm) + posh(jm-1)
    enddo
    do jo = 0, no - 1
       do jm = 0, nm - 1
          do ji = 0, ni - 1
             jw = (jo * nm + jm) * ni + ji
             if (dsrc(jw).ne.vmiss) then
                jv = posh(jm+1)
                ddest(jv) = dsrc(jw)
                ridx(jv) = jo * ni + ji
                posh(jm+1) = posh(jm+1) + 1
             endif
          enddo
       enddo
    enddo
  end subroutine pack_qjds_d
  subroutine pack_qjds_f &
       & (ddest, ridx, posh,  &
       &  mcol, &
       &  xmems, mx,   popts, vmiss, dsrc)
    use TOUZA_Nio_record,only: ptx_set_loops
    implicit none
    integer,parameter :: KARG=KFLT
    real(kind=KARG),    intent(out) :: ddest(0:*)
    integer,         intent(out) :: ridx(0:*)
    integer,         intent(out) :: posh(0:*)
    integer,         intent(in)  :: mcol
    integer,         intent(in)  :: xmems(*)
    integer,         intent(in)  :: mx
    integer,         intent(in)  :: popts(*)
    real(kind=KRMIS),intent(in)  :: vmiss
    real(kind=KARG),    intent(in)  :: dsrc(0:*)
    integer jw, jv
    integer ji, jm, jo
    integer ni, nm, no
    real(kind=KARG) :: vu

    vu = real(vmiss, kind=KARG)
    call ptx_set_loops(ni, nm, no, xmems, mx, popts)

    posh(0:mcol) = 0
    do jo = 0, no - 1
       do jm = 0, nm - 2
          do ji = 0, ni - 1
             jw = (jo * nm + jm) * ni + ji
             if (dsrc(jw).ne.vmiss) posh(jm+2) = posh(jm+2) + 1
          enddo
       enddo
    enddo
    do jm = 2, nm
       posh(jm) = posh(jm) + posh(jm-1)
    enddo
    do jo = 0, no - 1
       do jm = 0, nm - 1
          do ji = 0, ni - 1
             jw = (jo * nm + jm) * ni + ji
             if (dsrc(jw).ne.vmiss) then
                jv = posh(jm+1)
                ddest(jv) = dsrc(jw)
                ridx(jv) = jo * ni + ji
                posh(jm+1) = posh(jm+1) + 1
             endif
          enddo
       enddo
    enddo
  end subroutine pack_qjds_f
  subroutine pack_qjds_i &
       & (ddest, ridx, posh,  &
       &  mcol, &
       &  xmems, mx,   popts, vmiss, dsrc)
    use TOUZA_Nio_record,only: ptx_set_loops
    implicit none
    integer,parameter :: KARG=KI32
    integer(kind=KARG),    intent(out) :: ddest(0:*)
    integer,         intent(out) :: ridx(0:*)
    integer,         intent(out) :: posh(0:*)
    integer,         intent(in)  :: mcol
    integer,         intent(in)  :: xmems(*)
    integer,         intent(in)  :: mx
    integer,         intent(in)  :: popts(*)
    real(kind=KRMIS),intent(in)  :: vmiss
    integer(kind=KARG),    intent(in)  :: dsrc(0:*)
    integer jw, jv
    integer ji, jm, jo
    integer ni, nm, no
    integer(kind=KARG) :: vu

    vu = int(vmiss, kind=KARG)
    call ptx_set_loops(ni, nm, no, xmems, mx, popts)

    posh(0:mcol) = 0
    do jo = 0, no - 1
       do jm = 0, nm - 2
          do ji = 0, ni - 1
             jw = (jo * nm + jm) * ni + ji
             if (dsrc(jw).ne.vmiss) posh(jm+2) = posh(jm+2) + 1
          enddo
       enddo
    enddo
    do jm = 2, nm
       posh(jm) = posh(jm) + posh(jm-1)
    enddo
    do jo = 0, no - 1
       do jm = 0, nm - 1
          do ji = 0, ni - 1
             jw = (jo * nm + jm) * ni + ji
             if (dsrc(jw).ne.vmiss) then
                jv = posh(jm+1)
                ddest(jv) = dsrc(jw)
                ridx(jv) = jo * ni + ji
                posh(jm+1) = posh(jm+1) + 1
             endif
          enddo
       enddo
    enddo
  end subroutine pack_qjds_i

!!!_  & ridx2count
  subroutine ridx2count &
       & (ncols, cco, mrow, ridx, ndata)
    implicit none
    integer,intent(out) :: ncols
    integer,intent(out) :: cco(0:*)
    integer,intent(in)  :: mrow
    integer,intent(in)  :: ridx(0:*)
    integer,intent(in)  :: ndata
    integer j, jr
    cco(0:mrow-1) = 0
    do j = 0, ndata - 1
       jr = ridx(j)
       cco(jr) = cco(jr) + 1
    enddo
    ncols = maxval(cco(0:mrow-1))
    return
  end subroutine ridx2count

!!!_ + utilities
!!!_  & tweak_header_coor
  subroutine tweak_header_coor &
       & (ierr,  popts, hdest, &
       &  ncols, kfmt,  kaxs,  laxs, cname)
    use TOUZA_Nio_record,only: ptx_def_options, inquire_header_coor
    use TOUZA_Nio_record,only: search_null_coor,shift_header_coor
    use TOUZA_Nio_record,only: put_header_cprop,get_header_cprop
    use TOUZA_Nio_record,only: ptx_set_shape, is_match_format
    use TOUZA_Nio_record,only: PROP_PTX_COLC
    use TOUZA_Nio_header,only: litem, nitem
    use TOUZA_Nio_std,only: choice_a
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(inout)       :: popts(*)
    character(len=*),intent(inout)       :: hdest(*)
    integer,         intent(in)          :: ncols
    integer,         intent(in)          :: kfmt
    integer,         intent(in)          :: kaxs(*)
    integer,         intent(in)          :: laxs
    character(len=*),intent(in),optional :: cname      ! column coordinate name

    character(len=litem) :: co
    integer colc, bco, mc
    integer irange(2)

    ierr = 0
    if (ierr.eq.0) then
       call choice_a(co, ' ', cname)
       if (co.eq.' ') then
          colc = column_def
          mc = -1
       else
          call inquire_header_coor(hdest, co, idx=colc, mem=mc)
          if (colc.eq.0) colc = column_none
       endif
       if (co.eq.' ') co = col_coor
       bco = search_null_coor(hdest)
       if (colc.ge.1.and.colc.le.laxs) then
          if (mc.gt.0) then
             if (mc.lt.ncols) ierr = _ERROR(ERR_INVALID_PARAMETER)
          else
             irange = (/1, ncols/)
             call put_header_cprop(ierr, hdest, co, irange, colc)
          endif
       else if (bco.le.0) then
          if (is_match_format(kfmt, st='P')) then
             continue
          else
             ierr = _ERROR(ERR_INVALID_PARAMETER)
          endif
       else
          colc = popts(PROP_PTX_COLC)
          if (colc.lt.0) colc = laxs
          colc = max(1, min(colc, laxs))
          call shift_header_coor(ierr, hdest, bco, colc)
          if (ierr.eq.0) then
             irange = (/1, ncols/)
             call put_header_cprop(ierr, hdest, co, irange, colc)
          endif
       endif
    endif
    if (ierr.eq.0) call ptx_set_shape(popts, kaxs, laxs, ncols)

  end subroutine tweak_header_coor

!!!_ + Plain storage procedures, to pass
!!!_ + end module TOUZA_Nio_sparse
end module TOUZA_Nio_sparse

!!!_@ test_nio_sparse - test program
#ifdef TEST_NIO_SPARSE
program test_nio_sparse
  use TOUZA_Nio_sparse
  use TOUZA_Std,only: parse, get_param, arg_diag, arg_init
  use TOUZA_Std,only: upcase, get_option
  use TOUZA_Std,only: MPI_COMM_NULL
  implicit none
  integer ierr
  integer jarg
  character(len=64) :: test

  ierr = 0
  jarg = 0
  test = ' '
101 format(A,' = ', I0)
  call init(ierr, stdv=-9, icomm=MPI_COMM_NULL)
  if (ierr.eq.0) call diag(ierr, levv=+9)
  if (ierr.eq.0) call arg_init(ierr, levv=-9)
  if (ierr.eq.0) call parse(ierr)
  if (ierr.eq.0) call arg_diag(ierr)
  if (ierr.eq.0) then
     jarg = jarg + 1
     call get_param(ierr, test, jarg, ' ')
     if (ierr.eq.0) call upcase(test)
  endif
  if (ierr.eq.0) then
     if (test.eq.' ') test = 'DEF'
201  format('##### TEST[', A, '] ', A)
     select case (test)
     case ('DEF')
        write(*, 201) trim(test), 'read as sparse'
        call test_read_sparse(ierr, jarg)
     case default

     end select
  endif
  if (ierr.eq.0) call finalize(ierr)
  write(*, 101) 'FINAL', ierr
  stop
contains
!!!_ + test_read_sparse
  subroutine test_read_sparse(ierr, jarg)
    use TOUZA_Std,only: KDBL, KFLT
    use TOUZA_Std,only: new_unit
    use TOUZA_Std,only: sus_open, is_eof_ss, is_error_match
    use TOUZA_Nio_header
    use TOUZA_Nio_record,only: nio_read_header
    use TOUZA_Nio_record,only: nio_check_magic_file, nio_skip_records
    use TOUZA_Nio_record,only: rev_pos_leave
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jarg
    character(len=1024) :: rfile
    character(len=litem) :: head(nitem)
    character(len=litem) :: cname
    integer flag
    integer uread
    integer krect
    integer popts(lopts_sparse)
    integer irec
    integer nbase, mcols, npack
    integer colc

    integer,parameter :: KPR = KDBL

    ! csr
    integer,       allocatable :: icsr(:)
    real(kind=KPR),allocatable :: dcsr(:)
    real(kind=KPR),parameter :: mold_csr = 0.0_KPR
    ! qjds
    integer,       allocatable :: hjds(:)
    integer,       allocatable :: cjds(:)
    real(kind=KPR),allocatable :: djds(:)
    real(kind=KPR),parameter :: mold_qjds = 0.0_KPR

    integer j, jc

    ierr = 0
    jarg = jarg + 1
    if (ierr.eq.0) call get_param(ierr, rfile, jarg, ' ')
    if (rfile.eq.' ') return

    popts = -999

    cname = ' '
    flag = rev_pos_leave

    uread = new_unit()
    ierr = min(0, uread)
    if (ierr.eq.0) call sus_open(ierr, uread, rfile, ACTION='R')
    if (ierr.eq.0) krect = nio_check_magic_file(uread)

    irec = 0
101 format('sparse/read:', I0, ': ', I0)
! 102 format('sparse/ctag:', I0, ': ', I0)
103 format('sparse/prop:', I0, ': ', 16(1x, I0))
104 format('sparse/shape:', I0, ': ', 3(1x, I0))
    do
       if (ierr.ne.0) exit

       ! csr
       if (ierr.eq.0) call nio_read_header(ierr, head, krect, uread)
       if (is_error_match(ierr, ERR_EOF)) then
          ierr = 0
          exit
       endif
       if (ierr.eq.0) then
          colc = nio_column_coor(head, cname)
       endif
       if (ierr.eq.0) then
          call nio_review_sparse(ierr, popts, head, uread, krect, mold_csr, colc, flag)
       endif
       if (ierr.eq.0) then
          call nio_inquire_sparse(popts, nbase, mcols, npack)
          write(*, 101) irec, ierr
          ! write(*, 102) irec, ctag
          write(*, 103) irec, popts
          write(*, 104) irec, nbase, mcols, npack
       endif
       if (ierr.eq.0) then
          allocate(icsr(0:nbase), dcsr(0:npack-1), STAT=ierr)
       endif
       if (ierr.eq.0) then
          call nio_restore_csr &
               & (ierr, dcsr,   icsr,  &
               &  head, uread,  krect, colc, flag, popts)
          ! call nio_skip_records(ierr, 0, uread, head=head, krect=krect)
       endif
       if (ierr.eq.0) then
201       format('CSR:', I0, 1x, I0, 1x, I0, 1x, F9.0)
          do j = 0, nbase - 1
             do jc = icsr(j), icsr(j+1) - 1
                write(*, 201) irec, j, jc, dcsr(jc)
             enddo
          enddo
       endif

       ! qjds
       if (ierr.eq.0) then
          call nio_skip_records(ierr, -1, uread, krect)
       endif

       if (ierr.eq.0) call nio_read_header(ierr, head, krect, uread)
       if (ierr.eq.0) then
          call nio_review_sparse(ierr, popts, head, uread, krect, mold_qjds, colc, flag)
       endif
       if (ierr.eq.0) then
          call nio_inquire_sparse(popts, nbase, mcols, npack)
          write(*, 101) irec, ierr
          ! write(*, 102) irec, ctag
          write(*, 103) irec, popts
          write(*, 104) irec, nbase, mcols, npack
       endif
       if (ierr.eq.0) then
          allocate(hjds(0:mcols), cjds(0:npack-1), djds(0:npack-1), STAT=ierr)
       endif
       if (ierr.eq.0) then
          call nio_restore_qjds &
               & (ierr, djds,   cjds,  hjds, &
               &  head, uread,  krect, colc, flag, popts)
       endif
       if (ierr.eq.0) then
211       format('QJDS:', I0, 1x, I0, 1x, I0, 1x, F9.0)
          do jc = 0, mcols - 1
             do j = hjds(jc), hjds(jc+1) - 1
                write(*, 211) irec, cjds(j), jc, djds(j)
             enddo
          enddo
       endif
       irec = irec + 1
    enddo
  end subroutine test_read_sparse

end program test_nio_sparse
#endif /* TEST_NIO_SPARSE */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

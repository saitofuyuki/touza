!!!_! ami_xfrom.F90. - TOUZA/Ami/table amida transformer general
! Maintainer: SAITO Fuyuki
! Created: Jul 20 2023
#define TIME_STAMP 'Time-stamp: <2025/07/16 18:01:59 fuyuki ami_xform.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2023
!           Japan Agency for Marine-Earth Science and Technology
!
!!!_* include
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_ami.h"
!!!_* macros
#ifndef   TEST_AMI_XFORM
#  define TEST_AMI_XFORM 0
#endif
!!!_@ TOUZA_Ami_xform - ami-da transformation
module TOUZA_Ami_xform
!!!_ + modules
  use TOUZA_Ami_std,only: unit_global
!!!_ + default
  implicit none
  private
!!!_ + parameter
!!!_ + public
  integer,parameter :: sw_default = 0
  integer,parameter :: sw_inner   = 1   ! inner rank to innermost loop
  integer,parameter :: sw_outer   = 2   ! outer rank to innermost loop
  integer,parameter :: sw_both    = 3   ! inner/outer rank to innermost loop
  integer,parameter :: sw_target  = 4   ! target rank to innermost loop

!!!_ + static
!!!_  - private static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = AMI_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global

#define __MDL__ 't'
#define _ERROR(E) (E - ERR_MASK_AMI_XFORM)
!!!_ + type
!!!_ + interfaces
  interface xform_csr
     module procedure xform_csr_d
  end interface xform_csr
  interface xform_qjds
     module procedure xform_qjds_d
  end interface xform_qjds
!!!_ + public procedures
  public init, diag, finalize
  public xform_csr, xform_qjds
!!!_ + function-like macros
!!!_ + procedures
contains
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm)
    use TOUZA_Ami_std,only: as_init=>init
    use TOUZA_Ami_std,only: choice
    use TOUZA_Ami_std,only: is_first_force, trace_control
    use TOUZA_Ami_std,only: control_deep, control_mode
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
       if (is_first_force(init_counts, mode)) then
          ulog = choice(ulog, u)
          lev_verbose = lv
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call as_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
       endif
       if (is_first_force(init_counts, mode)) then
          continue
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Ami_std,only: as_diag=>diag
    use TOUZA_Ami_std,only: choice, get_logu
    use TOUZA_Ami_std,only: msg
    use TOUZA_Ami_std,only: is_first_force, trace_control
    use TOUZA_Ami_std,only: control_deep, control_mode
    use TOUZA_Ami_std,only: is_msglev_NORMAL
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
          if (ierr.eq.0) call as_diag(ierr, utmp, levv=lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Ami_std,only: as_finalize=>finalize
    use TOUZA_Ami_std,only: is_first_force, trace_control
    use TOUZA_Ami_std,only: control_deep, control_mode
    use TOUZA_Ami_std,only: choice, get_logu
    use TOUZA_Ami_std,only: trace_fine
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
          if (ierr.eq.0) call as_finalize(ierr, utmp, levv=lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
  end subroutine finalize
!!!_ + main procedures
!!!_  - xform_csr - matrix multiplication (CSR: compressed sparse row)
  subroutine xform_csr_d &
       & (ierr, &
       &  z,   lzi, memz, &
       &  x,   lxi, memx, ni, no, &
       &  idx, val, cofs, sw)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    real(kind=KTGT), intent(inout) :: z(0:*)       ! [lzi, memz, *]
    real(kind=KTGT), intent(in)    :: x(0:*)       ! [lxi, memx, *]
    integer,         intent(in)    :: lzi, memz
    integer,         intent(in)    :: lxi, memx
    integer,         intent(in)    :: ni,  no
    integer,         intent(in)    :: idx(0:*)
    real(kind=KTGT), intent(in)    :: val(0:*)
    integer,         intent(in)    :: cofs(0:*)   ! [0:memz]
    integer,optional,intent(in)    :: sw

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    integer jo,  jc,  ji
    integer jzm
    integer jzp, jxp
    integer ksw

    ierr = 0
    ksw = choice(sw_default, sw)
    if (ksw.eq.sw_default) ksw = sw_both

    ! skipped elements are not reset but left as they are
    do jzm = 0, memz - 1
       if ((cofs(jzm+1) - cofs(jzm)).gt.0) then
          do jo = 0, no - 1
             jzp = (jo * memz + jzm) * lzi
             z(jzp:jzp+ni-1) = ZERO
          enddo
       endif
    enddo

    select case (ksw)
    case (sw_target)
       do jo = 0, no - 1
          do jzm = 0, memz - 1
             jzp = (jo * memz + jzm) * lzi
             do ji = 0, ni - 1
                do jc = cofs(jzm), cofs(jzm+1) - 1
                   jxp = (jo * memx + idx(jc)) * lxi
                   z(jzp+ji) = z(jzp+ji) + val(jc) * x(jxp+ji)
                enddo
             enddo
          enddo
       enddo
    case (sw_inner)
       do jo = 0, no - 1
          do jzm = 0, memz - 1
             jzp = (jo * memz + jzm) * lzi
             do jc = cofs(jzm), cofs(jzm+1) - 1
                jxp = (jo * memx + idx(jc)) * lxi
                z(jzp:jzp+ni-1) = z(jzp:jzp+ni-1) + val(jc) * x(jxp:jxp+ni-1)
             enddo
          enddo
       enddo
    case default
       do jzm = 0, memz - 1
          do jc = cofs(jzm), cofs(jzm+1) - 1
             do jo = 0, no - 1
                jzp = (jo * memz + jzm) * lzi
                jxp = (jo * memx + idx(jc)) * lxi
                z(jzp:jzp+ni-1) = z(jzp:jzp+ni-1) + val(jc) * x(jxp:jxp+ni-1)
             enddo
          enddo
       enddo
    end select
  end subroutine xform_csr_d

!!!_  - xform_qjds - matrix multiplication (coo+jds like storage)
  subroutine xform_qjds_d &
       & (ierr, &
       &  z,    lzi,  memz, &
       &  x,    lxi,  memx, ni,   no, &
       &  zidx, xidx, val,  cofs, nc, sw)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    use TOUZA_Ami_std,only: choice
    implicit none
    integer,         intent(out)   :: ierr
    real(kind=KTGT), intent(inout) :: z(0:*)       ! [lzi, memz, *]
    real(kind=KTGT), intent(in)    :: x(0:*)       ! [lxi, memx, *]
    integer,         intent(in)    :: lzi, memz
    integer,         intent(in)    :: lxi, memx
    integer,         intent(in)    :: ni,  no
    integer,         intent(in)    :: zidx(0:*)
    integer,         intent(in)    :: xidx(0:*)
    real(kind=KTGT), intent(in)    :: val(0:*)
    integer,         intent(in)    :: cofs(0:*)   ! [0:nc]
    integer,         intent(in)    :: nc
    integer,optional,intent(in)    :: sw

    real(kind=KTGT),parameter :: ZERO = 0.0_KTGT
    integer jo,  jc,  jr
    integer jzp, jxp
    integer ksw

    ! limitation
    !    Elements to be computed MUST BE stored in zidx(cofs(0):cofs(1))

    ierr = 0
    ksw = choice(sw_default, sw)
    if (ksw.eq.sw_default) ksw = sw_both

    ! skipped elements are not reset but left as they are

    select case (ksw)
    ! case (sw_target)
    ! case (sw_inner)
    case default
       do jc = 0, 0
          do jr = cofs(jc), cofs(jc+1) - 1
             do jo = 0, no - 1
                jzp = (jo * memz + zidx(jr)) * lzi
                jxp = (jo * memx + xidx(jr)) * lxi
                z(jzp:jzp+ni-1) = val(jr) * x(jxp:jxp+ni-1)
             enddo
          enddo
       enddo
       do jc = 1, nc - 1
          do jr = cofs(jc), cofs(jc+1) - 1
             do jo = 0, no - 1
                jzp = (jo * memz + zidx(jr)) * lzi
                jxp = (jo * memx + xidx(jr)) * lxi
                z(jzp:jzp+ni-1) = z(jzp:jzp+ni-1) + val(jr) * x(jxp:jxp+ni-1)
             enddo
          enddo
       enddo
    end select
  end subroutine xform_qjds_d

!!!_ + end Ami_xform
end module TOUZA_Ami_xform
!!!_@ test_ami_xform - test program
#if TEST_AMI_XFORM
program test_ami_xform
  use TOUZA_Ami_xform
  use TOUZA_Ami_std
  use TOUZA_Std,only: arg_init, arg_diag, parse, get_option, decl_pos_arg
  implicit none
  integer ierr
  integer ktest

  ierr = 0

  if (ierr.eq.0) call arg_init(ierr)
  if (ierr.eq.0) call decl_pos_arg(ierr, 'TEST')
  if (ierr.eq.0) call parse(ierr)
  if (ierr.eq.0) call arg_diag(ierr)

  if (ierr.eq.0) call get_option(ierr, ktest, 'TEST', 0)

  write(*,*) 'fine = ', ierr
  stop
contains
end program test_ami_xform
#endif /* TEST_AMI_XFORM */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

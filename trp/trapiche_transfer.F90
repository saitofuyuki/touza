!!!_! trapiche_transfer.F90 - TOUZA/Trapiche(trapiche) communication
! Maintainer: SAITO Fuyuki
! Created: May 21 2022
#define TIME_STAMP 'Time-stamp: <2022/10/20 07:45:06 fuyuki trapiche_transfer.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_trp.h"
!!!_* macros
!!!_@ TOUZA_Trp_transfer - trapiche floating-point manager
module TOUZA_Trp_transfer
!!!_ = declaration
  use TOUZA_Trp_std,only: KI32, KDBL, KFLT, &
       & control_mode, control_deep, is_first_force, &
       & unit_global,  trace_fine,   trace_control
  implicit none
  private
!!!_  - parameters
!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = TRP_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
# define __MDL__ 't'

  integer,save :: lworks, lworkr = 0
  integer,allocatable,save :: works(:)
  integer,allocatable,save :: workr(:)
!!!_  - common
  character(len=256) :: tmsg
!!!_  - interfaces
  interface trapiche_isend_core
     module procedure trapiche_isend_core_d
  end interface trapiche_isend_core
  interface trapiche_irecv_core
     module procedure trapiche_irecv_core_d
  end interface trapiche_irecv_core
!!!_  - public procedures
  public init, diag, finalize
  public trapiche_isend_core, trapiche_irecv_core
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init &
       & (ierr, &
       &  u,     levv,  mode,  stdv)
    use TOUZA_Trp_std,  only: choice
    use TOUZA_Trp_float,only: tf_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u, levv, mode, stdv

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
          if (ierr.eq.0) call tf_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif

    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Trp_std,  only: choice, msg, is_msglev_normal
    use TOUZA_Trp_float,only: tf_diag=>diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, md)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) then
                call msg(TIME_STAMP, __MDL__, utmp)
             endif
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call tf_diag(ierr, utmp, lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Trp_std,  only: choice
    use TOUZA_Trp_float,only: tf_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, md)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call tf_finalize (ierr, utmp, levv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + init subcontracts
!!!_ + diag subcontracts
!!!_ + user subroutines
!!!_  & trapiche_isend_core
  subroutine trapiche_isend_core_d &
       & (ierr, &
       &  v,     n,     irank, icomm, ktag,  ireq, &
       &  vmiss, mbits, xbits, xtop,  xbtm,  kcode)
    use TOUZA_Trp_std,only: MPI_INTEGER
#  if HAVE_FORTRAN_MPI_MPI_ISEND
    use MPI,only: MPI_Isend
#  endif
    use TOUZA_Std,only: KDBL
    use TOUZA_Trp_float,only: encode_alloc, retrieve_nbgz, KB_HEAD, show_bagazo_props
    implicit none
    integer,parameter :: KTGT  = KDBL
    integer,parameter :: KMTGT = MPI_INTEGER
    integer,        intent(out) :: ierr
    real(kind=KDBL),intent(in)  :: v(0:*)
    integer,        intent(in)  :: n
    integer,        intent(in)  :: irank, icomm
    integer,        intent(in)  :: ktag
    integer,        intent(out) :: ireq
    real(kind=KDBL),intent(in)  :: vmiss
    integer,        intent(in)  :: mbits
    integer,        intent(in)  :: xbits, xtop, xbtm
    integer,        intent(in)  :: kcode

    integer lw, nw
    integer,parameter :: mlim = DIGITS(0.0_KTGT) - 1
    integer mb

    ierr = 0
    lw = n * 3 + 64
    if (lw.gt.lworks) then
       lworks = lw
       allocate(works(0:lworks-1), STAT=ierr)
    endif
    if (ierr.eq.0) then
       mb = mbits
       if (mb.le.0) mb = mlim
       call encode_alloc &
            & (ierr,   &
            &  works,  &
            &  v,      n,     vmiss, &
            &  mb,     xbits, xtop,  xbtm, kcode)
    endif
    if (ierr.eq.0) nw = retrieve_nbgz(works) + KB_HEAD
    if (ierr.eq.0) then
       call MPI_Isend(works, nw, KMTGT, irank, ktag, icomm, ireq, ierr)
    endif
    ! if (ierr.eq.0) then
    !    call show_bagazo_props(ierr, works)
    ! endif
    return
  end subroutine trapiche_isend_core_d
!!!_  & trapiche_irecv_core
  subroutine trapiche_irecv_core_d &
       & (ierr, &
       &  v,     n,     irank, icomm, ktag,  ireq, &
       &  vmiss)
    use TOUZA_Trp_std,only: MPI_INTEGER, MPI_STATUS_SIZE
#if OPT_USE_MPI
    use MPI,only: MPI_Probe
#endif /* OPT_USE_MPI */
#  if HAVE_FORTRAN_MPI_MPI_IRECV
    use MPI,only: MPI_Irecv
#  endif
    use TOUZA_Std,only: KDBL
    use TOUZA_Trp_float,only: decode_alloc, retrieve_nbgz, KB_HEAD, &
         & retrieve_ncnz, KCODE_MANUAL
    implicit none
    integer,parameter :: KTGT  = KDBL
    integer,parameter :: KMTGT = MPI_INTEGER
    integer,        intent(out) :: ierr
    real(kind=KDBL),intent(out) :: v(0:*)
    integer,        intent(in)  :: n
    integer,        intent(in)  :: irank, icomm
    integer,        intent(in)  :: ktag
    integer,        intent(out) :: ireq
    real(kind=KDBL),intent(in)  :: vmiss

    integer nw, nc
    integer mbits
    integer xbits, xtop, xbtm
    integer kcode
    integer istt(MPI_STATUS_SIZE)

    ierr = 0
    if (ierr.eq.0) call MPI_Probe(irank, ktag, icomm, istt, ierr)
    if (ierr.eq.0) call MPI_Get_count(istt, KMTGT, nw, ierr)
    if (ierr.eq.0) then
       if (nw.gt.lworkr) then
          lworkr = nw
          allocate(workr(0:lworkr-1), STAT=ierr)
       endif
    endif
    if (ierr.eq.0) then
       call MPI_Irecv(workr, nw, KMTGT, irank, ktag, icomm, ireq, ierr)
    endif
    if (ierr.eq.0) then
       nc = retrieve_ncnz(workr)
       if (nc.gt.n) then
          ierr = -1
       else
          call decode_alloc(ierr, v, workr, nc, vmiss, kcode=KCODE_MANUAL)
       endif
    endif

    return
  end subroutine trapiche_irecv_core_d
!!!_ + end
end module TOUZA_Trp_transfer
!!!_@ test_trapiche_transfer - test program
#ifdef TEST_TRAPICHE_TRANSFER
program test_trapiche_transfer
  use TOUZA_Std,only: get_ni, get_comm, diag_real_props, &
       & std_init=>init, std_diag=>diag, std_finalize=>finalize, &
       & KDBL
  use TOUZA_Trp_pack,only: RELLENO_TRANSPOSE
  use TOUZA_Trp_std, only: binstr, MPI_STATUS_SIZE, MPI_DOUBLE_PRECISION
  use TOUZA_Trp_transfer
  implicit none

  character(len=*),parameter :: TEST_NEIGHBOR = 'N'
  character(len=*),parameter :: TEST_ENDS     = 'E'
  character(len=*),parameter :: TEST_BCAST    = 'B'
  character(len=*),parameter :: TEST_GATHER   = 'G'

  character(len=*),parameter :: SCHEME_PLAIN    = 'P'
  character(len=*),parameter :: SCHEME_SINGLE   = 'S'
  character(len=*),parameter :: SCHEME_TRAPICHE = 'T'

  integer ierr

  character(len=8) :: test, scheme
  integer n, itr
  integer ksign, xbits
  integer icomm, irank, nrank

  integer,parameter :: KMTGT = MPI_DOUBLE_PRECISION

  real(kind=KDBL),allocatable :: vsrc(:)
  real(kind=KDBL),allocatable :: vrec(:)
  real(kind=KDBL),allocatable :: vchk(:)

  ierr = 0
101 format(A, ' = ', I0)
  if (ierr.eq.0) call std_init(ierr, levv=+9)
  if (ierr.eq.0) call get_comm(ierr, icomm)
  if (ierr.eq.0) call get_ni(ierr, nrank, irank, icomm)

  if (ierr.eq.0) call parse_args(ierr, test, scheme, n, itr, ksign, xbits)

  if (ierr.eq.0) call init(ierr, levv=+9)

  if (ierr.eq.0) then
     allocate(vsrc(0:n-1), vrec(0:n-1), vchk(0:n-1), STAT=ierr)
  endif
  if (ierr.eq.0) call set_test_array(ierr, vsrc, n, ksign, xbits)

  if (ierr.eq.0) then
     select case (TEST(1:1))
     case (TEST_ENDS)
        call test_sub_ends &
             & (ierr, vchk, vrec, vsrc, n, itr, scheme, irank, nrank, icomm)
     end select
  endif

  if (ierr.eq.0) call std_diag(ierr)
  if (ierr.eq.0) call std_finalize(ierr)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call finalize(ierr)
  write(*, 101) 'Fine', ierr
  stop
contains
  subroutine test_sub_ends &
       & (ierr, v2, v1, v0, n, itr, scheme, ir, nr, icomm)
#if OPT_USE_MPI
    use MPI,only: MPI_Wait, MPI_Barrier
#endif /* OPT_USE_MPI */
#  if HAVE_FORTRAN_MPI_MPI_ISEND
    use MPI,only: MPI_Isend
#  endif
#  if HAVE_FORTRAN_MPI_MPI_IRECV
    use MPI,only: MPI_Irecv
#  endif
    use TOUZA_Trp,only: XNOTOP, XNOBTM, KCODE_TRANSPOSE, KCODE_MANUAL
    implicit none
    integer,         intent(out) :: ierr
    real(kind=KDBL), intent(out) :: v2(0:*)
    real(kind=KDBL), intent(out) :: v1(0:*)
    real(kind=KDBL), intent(in)  :: v0(0:*)
    integer,         intent(in)  :: n
    integer,         intent(in)  :: itr
    character(len=*),intent(in)  :: scheme
    integer,         intent(in)  :: ir, nr, icomm
    integer j

    integer irfrom, irto
    integer ktag
    integer ireqs, ireqr
    integer istts(MPI_STATUS_SIZE), isttr(MPI_STATUS_SIZE)

    integer,parameter :: mbits = 0   ! full fraction
    integer,parameter :: xbits = -1  ! automatic
    integer,parameter :: xtop  = XNOTOP
    integer,parameter :: xbtm  = XNOBTM
    integer,parameter :: kcode = KCODE_TRANSPOSE + KCODE_MANUAL

    real(kind=KDBL) :: vmiss = 999.0

    ierr = 0

    irfrom = 0
    irto = nr - 1
    ktag = 0
    v1(0:n-1) = 0.0_KDBL
    v2(0:n-1) = 0.0_KDBL

    if (ierr.eq.0) call MPI_Barrier(icomm, ierr)
101 format('iteration = ', I0)
    do j = 0, itr - 1
       write(*, 101) j
       select case (scheme(1:1))
       case (SCHEME_PLAIN)
          ! out-bound
          if (ir.eq.irfrom) then
             call MPI_Isend(v0, n, KMTGT, irto,   ktag, icomm, ireqs, ierr)
          endif
          if (ir.eq.irto) then
             call MPI_Irecv(v1, n, KMTGT, irfrom, ktag, icomm, ireqr, ierr)
          endif
          if (ir.eq.irfrom) call MPI_Wait(ireqs, istts, ierr)
          if (ir.eq.irto)   call MPI_Wait(ireqr, isttr, ierr)
          ! in-bound
          if (ir.eq.irto) then
             call MPI_Isend(v1, n, KMTGT, irfrom, ktag, icomm, ireqs, ierr)
          endif
          if (ir.eq.irfrom) then
             call MPI_Irecv(v2, n, KMTGT, irto,   ktag, icomm, ireqr, ierr)
          endif
          if (ir.eq.irto)   call MPI_Wait(ireqs, istts, ierr)
          if (ir.eq.irfrom) call MPI_Wait(ireqr, isttr, ierr)
       case (SCHEME_TRAPICHE)
          ! out-bound
          if (ir.eq.irfrom) then
             call trapiche_isend_core &
                  & (ierr, v0, n, irto,   icomm, ktag, ireqs, vmiss, mbits, xbits, xtop, xbtm, kcode)
          endif
          if (ir.eq.irto) then
             call trapiche_irecv_core &
                  & (ierr, v1, n, irfrom, icomm, ktag, ireqr, vmiss)
          endif
          if (ir.eq.irfrom) call MPI_Wait(ireqs, istts, ierr)
          if (ir.eq.irto)   call MPI_Wait(ireqr, isttr, ierr)
          ! in-bound
          if (ir.eq.irto) then
             call trapiche_isend_core &
                  & (ierr, v1, n, irfrom, icomm, ktag, ireqs, vmiss, mbits, xbits, xtop, xbtm, kcode)
          endif
          if (ir.eq.irfrom) then
             call trapiche_irecv_core &
                  & (ierr, v2, n, irto,   icomm, ktag, ireqr, vmiss)
          endif
          if (ir.eq.irto)   call MPI_Wait(ireqs, istts, ierr)
          if (ir.eq.irfrom) call MPI_Wait(ireqr, isttr, ierr)
       end select
    enddo
    if (ierr.eq.0) call MPI_Barrier(icomm, ierr)
    if (ir.eq.irfrom) then
       write(*, *) 'v0:', v0(0:15)
       write(*, *) 'v2:', v2(0:15)
    endif
    if (ir.eq.irto) then
       write(*, *) 'v1:', v1(0:15)
    endif

    if (ir.eq.irfrom) then
       if (ALL(v0(0:n-1).eq.v2(0:n-1))) then
          write(*, *) 'success'
       else
          write(*, *) 'fail'
       endif
    endif
  end subroutine test_sub_ends

  subroutine set_test_array &
       & (ierr, v, n, ksign, xbits)
    use TOUZA_Trp_float,only: show_pattern_float
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KDBL),intent(out) :: V(0:*)
    integer,        intent(in)  :: n
    integer,        intent(in)  :: ksign
    integer,        intent(in)  :: xbits
    integer j, m, jj
    integer jbgn, jend

    real(kind=KDBL),parameter :: ZERO = 0.0_KDBL
    real(kind=KDBL),parameter :: ONE  = 1.0_KDBL
    real(kind=KDBL),parameter :: BASE = RADIX(ZERO)

    integer,parameter :: lfrd = DIGITS(ONE) - 1
    integer,parameter :: kxone = exponent(ONE)

    real(kind=KDBL) :: FR111    ! 1.111
    real(kind=KDBL) :: FR110    ! 1.110
    real(kind=KDBL) :: FR100    ! 1.100
    real(kind=KDBL) :: FR011    ! 1.011
    real(kind=KDBL) :: FR001    ! 1.001
    real(kind=KDBL) :: FR000    ! 1.000

    integer,parameter :: mfr = 7
    character(len=32) :: tag
    integer kx

    ierr = 0
    FR111 = FRACTION(HUGE(ZERO))
    FR000 = FRACTION(ONE)
    FR100 = FR000 + FR000 / BASE
    FR011 = FR111 - FR000 / BASE
    FR001 = FR000 + (FR100 - FR011)
    FR110 = FR111 - (FR100 - FR011)
    ! call show_pattern_float(ierr, '111', FR111)
    ! call show_pattern_float(ierr, '110', FR110)
    ! call show_pattern_float(ierr, '100', FR100)
    ! call show_pattern_float(ierr, '011', FR011)
    ! call show_pattern_float(ierr, '001', FR001)
    ! call show_pattern_float(ierr, '000', FR000)

    v(0:n-1:mfr) = zero
    v(1:n-1:mfr) = FR000
    v(2:n-1:mfr) = FR001
    v(3:n-1:mfr) = FR011
    v(4:n-1:mfr) = FR100
    v(5:n-1:mfr) = FR110
    v(6:n-1:mfr) = FR111

    jj = 0
    do j = 0, n - 1, mfr
       kx = minexponent(ZERO) + (RADIX(ZERO) ** jj) - 1
       jbgn = j
       jend = min(n, j + mfr)
       v(jbgn:jend-1) = set_exponent(v(jbgn:jend-1), kx)
       jj = mod(jj + 1, xbits + 1)
    enddo
    if (ksign.eq.0) then
       m  = mfr + 1
       jj = +1
       do j = 0, n - 1, m
          jbgn = j
          jend = min(n, j + m)
          v(jbgn:jend-1) = v(jbgn:jend-1) * jj
          jj = - jj
       enddo
    endif

    ! 101 format('I', I8.8)
    !     do j = 0, n - 1
    !        write(tag, 101) j
    !        call show_pattern_float(ierr, tag, v(j))
    !     enddo
    return
  end subroutine set_test_array
  subroutine parse_args &
       & (ierr, test, scheme, n, itr, ksign, xbits)
    use TOUZA_Std, only: &
         & parse, decl_pos_arg, get_option, arg_init, arg_diag, &
         & banner
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: test
    character(len=*),intent(out) :: scheme
    integer,         intent(out) :: n
    integer,         intent(out) :: itr
    integer,         intent(out) :: ksign
    integer,         intent(out) :: xbits

    character(len=256) :: bmsg

    ierr = 0
    if (ierr.eq.0) call arg_init(ierr)

    if (ierr.eq.0) call decl_pos_arg(ierr, 'T') ! test
    if (ierr.eq.0) call decl_pos_arg(ierr, 'P') ! scheme
    if (ierr.eq.0) call decl_pos_arg(ierr, 'N') ! size
    if (ierr.eq.0) call decl_pos_arg(ierr, 'L') ! iteration

    if (ierr.eq.0) call parse(ierr)
    if (ierr.eq.0) call arg_diag(ierr)

    if (ierr.eq.0) call get_option(ierr, test,   'T', TEST_ENDS)
    if (ierr.eq.0) call get_option(ierr, scheme, 'P', SCHEME_TRAPICHE)
    if (ierr.eq.0) call get_option(ierr, n,      'N', 0)
    if (ierr.eq.0) call get_option(ierr, itr,    'L', 0)
    if (ierr.eq.0) call get_option(ierr, xbits,  'X', 0)
    if (ierr.eq.0) call get_option(ierr, ksign,  'S', 0)

    if (ierr.eq.0) then
       if (n.eq.0) n = 1024
       if (itr.eq.0) itr = 8
       if (test.eq.' ') test = TEST_ENDS
       if (scheme.eq.' ') scheme = SCHEME_TRAPICHE
       if (xbits.eq.0) xbits = 5
    endif
101 format('TEST:', A, ':', A, ':', I0, ':', I0, ':', I0, ':', I0)
    if (ierr.eq.0) then
       write(bmsg, 101) test(1:1), scheme(1:1), n, itr, ksign, xbits
       call banner(ierr, bmsg)
    endif

  end subroutine parse_args
end program test_trapiche_transfer

#endif /* TEST_TRAPICHE_TRANSFER */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

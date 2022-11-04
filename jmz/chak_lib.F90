!!!_! chak_lib.F90 - TOUZA/Jmz swiss(CH) army knife library
! Maintainer: SAITO Fuyuki
! Created: Oct 13 2022
#define TIME_STAMP 'Time-stamp: <2022/11/04 09:26:22 fuyuki chak_lib.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "jmz_config.h"
#endif
#include "jmz.h"
!!!_* macros
#ifndef    OPT_CHAK_PRECISION
#  define  OPT_CHAK_PRECISION  0
#endif
#if OPT_CHAK_PRECISION == 1
#  define __KBUF KFLT
#else
#  define __KBUF KDBL
#endif
!!!_@ TOUZA/Jmz/chak-lib - nio swiss army knife (library)
module chak_lib
!!!_ + Declaration
!!!_  - modules
  use TOUZA_Std,only: KFLT,  KDBL
  use TOUZA_Std,only: is_msglev
  use TOUZA_Std,only: msglev_NORMAL, msglev_INFO, msglev_DEBUG
  use TOUZA_Std,only: msglev_WARNING, msglev_DETAIL
  use TOUZA_Std,only: is_msglev_DETAIL, is_msglev_NORMAL, is_msglev_INFO, is_msglev_DEBUG
  use TOUZA_Nio,only: litem, nitem, GFMT_END
  implicit none
  public
!!!_  - parameters
  integer,parameter :: KBUF = __KBUF
  integer,parameter :: lcoor = 6

  real(kind=KBUF),parameter :: ZERO  = 0.0_KBUF
  real(kind=KBUF),parameter :: ONE   = 1.0_KBUF

  real(kind=KBUF),parameter :: TRUE  = ONE
  real(kind=KBUF),parameter :: FALSE = ZERO

  real(kind=KBUF),parameter :: ULIMIT = + HUGE(ZERO)
  real(kind=KBUF),parameter :: LLIMIT = - HUGE(ZERO)

  real(kind=KBUF),parameter :: UNDEF  = -999.0_KBUF
  ! real(kind=KBUF),parameter :: UNDEF  = LLIMIT

  integer,parameter :: cfmt_org = GFMT_END
  integer,parameter :: cfmt_ascii  = 1 + cfmt_org
  integer,parameter :: cfmt_binary = 2 + cfmt_org
  integer,parameter :: cfmt_flag_native = 0
  integer,parameter :: cfmt_flag_swap = 1
  integer,parameter :: cfmt_flag_big = 2
  integer,parameter :: cfmt_flag_little = 3
  integer,parameter :: cfmt_flags_bo = 4

  integer,parameter :: cfmt_binary_i4 = cfmt_binary
  integer,parameter :: cfmt_binary_r4 = cfmt_binary + cfmt_flags_bo
  integer,parameter :: cfmt_binary_r8 = cfmt_binary + cfmt_flags_bo * 2

  character(len=*),parameter :: paramd = '='

!!!_  - i/o units
  integer :: ulog = -1
  integer :: uerr = -1

!!!_  - global flags
  integer,save :: lev_verbose = 0
  integer,save :: dbgv = -1
  integer,save :: stdv = -1

!!!_  - common values
  real(kind=KBUF),save :: PI = ZERO
!!!_  - domain property
  type domain_t
     integer :: n                   ! total size
     integer :: mco                 ! coordinate size
     integer :: ofs(0:lcoor-1)
     integer :: iter(0:lcoor-1)
     integer :: strd(0:lcoor)       ! with sentry
     integer :: bgn(0:lcoor-1)
     integer :: end(0:lcoor-1)
     integer :: cidx(0:lcoor-1)     ! coordinate index (physical)
     integer :: lidx(0:lcoor-1)     ! coordinate index (logical)
  end type domain_t
contains
!!!_  - initialization
  subroutine init(ierr)
    use TOUZA_Std,only: env_init, MPI_COMM_NULL, stdout=>uout, stderr=>uerr
    implicit none
    integer,intent(out) :: ierr

    ierr = 0

    if (ierr.eq.0) call env_init(ierr, levv=stdv, icomm=MPI_COMM_NULL)
    if (ierr.eq.0) ulog = stdout
    if (ierr.eq.0) uerr = stderr

    if (PI.eq.ZERO) PI = ATAN2(ZERO, -ONE)
  end subroutine init
!!!_  - common utilities
!!!_   . message
  subroutine message(ierr, msg, iadd, fmt, levm, u, indent)
    use TOUZA_Std,only: choice, join_list
    implicit none
    integer,         intent(in)          :: ierr
    character(len=*),intent(in)          :: msg     ! msg
    integer,         intent(in),optional :: iadd(:)
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: levm    ! message level
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: indent
    integer jerr
    integer lv, utmp
    character(len=1024) :: txt
    integer skp
    jerr = 0
    lv = choice(0, levm)
    if (ierr.ne.0) then
       utmp = choice(uerr, u)
    else
       utmp = choice(ulog, u)
    endif
    skp = choice(0, indent)
    if (ierr.ne.0.or.is_msglev(lev_verbose, lv)) then
       if (present(iadd)) then
          if (size(iadd).gt.0) then
             if (present(fmt)) then
                write(txt, fmt) iadd(:)
             else
                call join_list(jerr, txt, iadd(:), ldelim='(', rdelim=')')
             endif
             txt = trim(msg) // ' ' // trim(txt)
          endif
       else
          txt = msg
       endif
102    format('error:', I0, ': ', A)
101    format(A, A)
       if (ierr.ne.0) then
          write(utmp, 102) ierr, trim(txt)
       else
          write(utmp, 101) repeat(' ', skp), trim(txt)
       endif
    endif
  end subroutine message

!!!_   . conv_physical_index
  PURE &
  integer function conv_physical_index (jlog, domL, domR) result(n)
    implicit none
    integer,       intent(in) :: jlog
    type(domain_t),intent(in) :: domL, domR
    integer jc
    integer jcur, ncur
    n = 0
    ncur = jlog
    !NEC$ novector
    do jc = 0, domL%mco - 1
       jcur = mod(ncur, domL%iter(jc))
       ncur = ncur / domL%iter(jc)
       if (domR%bgn(jc).le.jcur.and.jcur.lt.domR%end(jc)) then
          n = n + (domR%ofs(jc) + jcur) * domR%strd(jc)
       else
          n = -1
          exit
       endif
    enddo
  end function conv_physical_index

!!!_ + end chak
end module chak_lib
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

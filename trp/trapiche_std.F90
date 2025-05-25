!!!_! trapiche_std.F90 - TOUZA/Trapiche utilities (and bridge to Std)
! Maintainer: SAITO Fuyuki
! Created: Mar 30 2021
#define TIME_STAMP 'Time-stamp: <2025/05/23 11:11:54 fuyuki trapiche_std.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021-2025
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_trp.h"
!!!_@ TOUZA_Trp_std - trapiche utilities
module TOUZA_Trp_std
!!!_ = declaration
  use TOUZA_Std_prc,only: KI8, KI32, KI64, KDBL, KFLT
  use TOUZA_Std_prc,only: check_real_dnm
  use TOUZA_Std_utl,only: choice, choice_a, condop, control_mode, control_deep, is_first_force
  use TOUZA_Std_log,only: is_msglev
  use TOUZA_Std_log,only: is_msglev_severe, is_msglev_debug, is_msglev_normal, is_msglev_detail
  use TOUZA_Std_log,only: unit_global,      trace_fine,      trace_control
  use TOUZA_Std_mwe,only: MPI_INTEGER, MPI_STATUS_SIZE, MPI_DOUBLE_PRECISION
  use TOUZA_Std_mwe,only: MPI_PROBE,   MPI_GET_COUNT
  use TOUZA_Std_ipc,only: ipc_IBITS
  implicit none
  private
!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = TRP_MSG_LEVEL
  integer,save :: lev_stdv = TRP_MSG_LEVEL - 1
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global

# define __MDL__ 's'
!!!_  - common
  character(len=256) :: tmsg
!!!_  - interface
  interface show_pattern
     module procedure show_pattern_i
     module procedure show_pattern_l
  end interface show_pattern

  interface binstr
     module procedure binstr_i
     module procedure binstr_l
  end interface binstr

  interface first_bit
     module procedure first_bit_i
     module procedure first_bit_l
  end interface first_bit

  interface health_check
     module procedure health_check_i
     module procedure health_check_l
  end interface health_check

  interface msg
     module procedure msg_txt, msg_i
  end interface msg

!!!_  - public
  public init, diag, finalize
  public msg
  public health_check_all
  public health_check
  public show_pattern
  public binstr
  public first_bit

!!!_   . TOUZA_Std_prc
  public KI8,  KI32, KI64
  public KDBL, KFLT
  public check_real_dnm
!!!_   . TOUZA_Std_utl
  public choice, choice_a, condop
  public control_mode, control_deep, is_first_force
!!!_   . TOUZA_Std_log
  public is_msglev, is_msglev_severe, is_msglev_debug, is_msglev_normal
  public is_msglev_detail
  public trace_control, trace_fine
  public unit_global
!!!_   . TOUZA_Std_mwe
  public MPI_INTEGER, MPI_STATUS_SIZE, MPI_DOUBLE_PRECISION
  public MPI_PROBE,   MPI_GET_COUNT
!!!_   . TOUZA_Std_ipc
  public ipc_IBITS
!!!_ + common interfaces
contains
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv)
    use TOUZA_Std,only: utl_init, prc_init, log_init, ipc_init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode ! initialization mode
    integer,intent(in),optional :: stdv ! verbose level of TOUZA_Std
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
       lmd = MODE_SURFACE
       if (md.ge.MODE_DEEP) then
          lev_stdv = choice(lev_stdv, stdv)
          if (ierr.eq.0) call prc_init(ierr, u=ulog, levv=lev_stdv, mode=lmd)
          if (ierr.eq.0) call utl_init(ierr, u=ulog, levv=lev_stdv, mode=lmd)
          if (ierr.eq.0) call log_init(ierr, u=ulog, levv=lev_stdv, mode=lmd)
          if (ierr.eq.0) call ipc_init(ierr, u=ulog, levv=lev_stdv, mode=lmd)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif

101 format('init = ', I0)
    if (ierr.ne.0) then
       if (is_msglev_severe(lv)) then
          write(tmsg, 101) ierr
          call msg(tmsg, __MDL__)
       endif
    else
       if (is_msglev_debug(lv)) then
          write(tmsg, 101) ierr
          call msg(tmsg, __MDL__)
       endif
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Std,only: utl_diag, prc_diag, log_diag, ipc_diag
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
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __MDL__, utmp)
          endif
       endif
       lmd = MODE_SURFACE
       if (md.ge.MODE_DEEP) then
          if (ierr.eq.0) call prc_diag(ierr, utmp, lev_stdv, mode=lmd)
          if (ierr.eq.0) call utl_diag(ierr, utmp, lev_stdv, mode=lmd)
          if (ierr.eq.0) call log_diag(ierr, utmp, lev_stdv, mode=lmd)
          if (ierr.eq.0) call ipc_diag(ierr, utmp, lev_stdv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std,only: utl_finalize, prc_finalize, log_finalize, ipc_finalize
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
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = MODE_SURFACE
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call prc_finalize(ierr, utmp, lev_stdv, mode=lmd)
          if (ierr.eq.0) call utl_finalize(ierr, utmp, lev_stdv, mode=lmd)
          if (ierr.eq.0) call log_finalize(ierr, utmp, lev_stdv, mode=lmd)
          if (ierr.eq.0) call ipc_finalize(ierr, utmp, lev_stdv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_  & msg - message dispatcher
  subroutine msg_txt &
       & (txt, mdl, u)
    use TOUZA_Std,only: choice, std_msg=>msg, gen_tag
    implicit none
    character(len=*),intent(in)          :: txt
    character(len=*),intent(in),optional :: mdl
    integer,         intent(in),optional :: u
    character(len=1024) :: tag
    call gen_tag(tag, pkg=PACKAGE_TAG, grp=__GRP__, mdl=mdl)
    call std_msg(txt, tag, u)
    return
  end subroutine msg_txt

  subroutine msg_i &
       & (fmt, v, mdl, u)
    use TOUZA_Std,only: choice, std_msg=>msg, gen_tag
    implicit none
    character(len=*),intent(in)          :: fmt
    integer,         intent(in)          :: v
    character(len=*),intent(in),optional :: mdl
    integer,         intent(in),optional :: u
    character(len=1024) :: tag
    call gen_tag(tag, pkg=PACKAGE_TAG, grp=__GRP__, mdl=mdl)
    write(tmsg, fmt) v
    call std_msg(tmsg, tag, u)
    return
  end subroutine msg_i

!!!_ + user subroutines
!!!_  & health_check
  subroutine health_check_all &
       & (ierr, u, levv)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer jerri, jerrl
    ierr = 0
    call health_check_l(jerrl, 0_KI64, u, levv)
    call health_check_i(jerri, 0_KI32, u, levv)
    if (jerrl.ne.0) ierr = jerrl
    if (jerri.ne.0) ierr = jerri
    return
  end subroutine health_check_all

!!!_   . health_check_l - health checker for first_bit_l
  subroutine health_check_l &
       & (ierr, mold, u, levv)
    implicit none
    integer,parameter :: KITGT=KI64
    integer,            intent(out)         :: ierr
    integer(kind=KITGT),intent(in)          :: mold
    integer,            intent(in),optional :: u
    integer,            intent(in),optional :: levv

    integer,parameter :: lbits = BIT_SIZE(mold)
    integer lv

    ierr = 0
    lv = choice(lev_verbose, levv)

101 format('bit_size:', I0, ' = ', I0)
109 format('fail bit_size:', I0, ' = ', I0)
    if (is_msglev_detail(lv)) then
       write(tmsg, 101) KITGT, lbits
       call msg(tmsg, __MDL__, u)
    endif

    !! hard-coded
    if (lbits.lt.32 .or. lbits.gt.64) then
       ierr = -1
       write(tmsg, 109) KITGT, lbits
       call msg(tmsg, __MDL__, u)
    endif
    return
  end subroutine health_check_l

!!!_   . health_check_i - health checker for first_bit_i
  subroutine health_check_i &
       & (ierr, mold, u, levv)
    implicit none
    integer,parameter :: KITGT=KI32
    integer,            intent(out)         :: ierr
    integer(kind=KITGT),intent(in)          :: mold
    integer,            intent(in),optional :: u
    integer,            intent(in),optional :: levv

    integer,parameter :: lbits = BIT_SIZE(mold)
    integer lv

    ierr = 0
    lv = choice(lev_verbose, levv)

101 format('bit_size:', I0, ' = ', I0)
109 format('fail bit_size:', I0, ' = ', I0)
    if (is_msglev_detail(lv)) then
       write(tmsg, 101) KITGT, lbits
       call msg(tmsg, __MDL__, u)
    endif

    !! hard-coded
    if (lbits.lt.16 .or. lbits.gt.32) then
       ierr = -1
       write(tmsg, 109) KITGT, lbits
       call msg(tmsg, __MDL__, u)
    endif
    return
  end subroutine health_check_i

!!!_  & show_pattern
  subroutine show_pattern_i &
       & (ierr, tag, N, M, W, KM, KO, u)
    implicit none
    integer,parameter :: KTGT = KI32
    integer,           intent(out)         :: ierr
    character(len=*),  intent(in)          :: tag
    integer(kind=KTGT),intent(in)          :: N
    integer,           intent(in),optional :: M  ! mask bits
    integer,           intent(in),optional :: W  ! total width
    integer,           intent(in),optional :: KM ! mod
    integer,           intent(in),optional :: KO ! off
    integer,           intent(in),optional :: u

    integer ui
    character(len=128) :: bs

    ierr = 0
    call binstr(bs, N, M, W, KM, KO)
    ui = choice(-1, u)

101 format(A, ': ', A, 1x, I0)
    if (ui.ge.0) then
       write(ui, 101) trim(tag), trim(bs), N
    else
       write(*,  101) trim(tag), trim(bs), N
    endif
    return
  end subroutine show_pattern_i

  subroutine show_pattern_l &
       & (ierr, tag, N, M, W, KM, KO, u)
    implicit none
    integer,parameter :: KTGT = KI64
    integer,           intent(out)         :: ierr
    character(len=*),  intent(in)          :: tag
    integer(kind=KTGT),intent(in)          :: N
    integer,           intent(in),optional :: M  ! mask bits
    integer,           intent(in),optional :: W  ! total width
    integer,           intent(in),optional :: KM ! mod
    integer,           intent(in),optional :: KO ! off
    integer,           intent(in),optional :: u

    integer ui
    character(len=128) :: bs

    ierr = 0
    call binstr(bs, N, M, W, KM, KO)
    ui = choice(-1, u)

101 format(A, ': ', A, 1x, I0)
    if (ui.ge.0) then
       write(ui, 101) trim(tag), N, trim(bs)
    else
       write(*,  101) trim(tag), N, trim(bs)
    endif
    return
  end subroutine show_pattern_l

!!!_  & binstr - binary representation of integer
  subroutine binstr_i(T, N, M, W, KM, KO)
    implicit none
    character(len=*),  intent(out)         :: T
    integer(kind=KI32),intent(in)          :: N
    integer,           intent(in),optional :: M  ! mask bits
    integer,           intent(in),optional :: W  ! total width
    integer,           intent(in),optional :: KM ! mod
    integer,           intent(in),optional :: KO ! off

    integer KMD, WD, MBT, KOF
    integer j
    character,parameter :: SEP = '|'
    character,parameter :: SKP = '.'

    KMD = choice(8, KM)
    MBT = choice(INT(BIT_SIZE(N)), M)
    WD  = choice(0, W)
    if (WD.eq.0) then
       WD = MBT
    else if (WD.lt.0) then
       WD = BIT_SIZE(N)
    endif
    KOF = MOD(choice(0, KO), KMD)

    T = ' '
    if (mod(WD, KMD).eq.KOF) T = SEP

    do j = WD - 1, 0, -1
       if (j.ge.MBT) then
          T = trim(T) // SKP
       else if (BTEST(N, j)) then
          T = trim(T) // '1'
       else
          T = trim(T) // '0'
       endif
       if (mod(j, KMD).eq.KOF) T = trim(T) // SEP
    enddo
  end subroutine binstr_i

  subroutine binstr_l(T, N, M, W, KM, KO)
    implicit none
    character(len=*),  intent(out)         :: T
    integer(kind=KI64),intent(in)          :: N
    integer,           intent(in),optional :: M  ! mask bits
    integer,           intent(in),optional :: W  ! total width
    integer,           intent(in),optional :: KM ! mod
    integer,           intent(in),optional :: KO ! off

    integer KMD, WD, MBT, KOF
    integer j
    character,parameter :: SEP = '|'
    character,parameter :: SKP = '.'

    KMD = choice(8, KM)
    MBT = choice(INT(BIT_SIZE(N)), M)
    WD  = choice(0, W)
    if (WD.eq.0) then
       WD = MBT
    else if (WD.lt.0) then
       WD = BIT_SIZE(N)
    endif
    if (KMD.eq.0) then
       KMD = 1
       KOF = +2
    else
       KOF = MOD(choice(0, KO), KMD)
    endif

    T = ' '
    if (mod(WD, KMD).eq.KOF) T = SEP

    do j = WD - 1, 0, -1
       if (j.ge.MBT) then
          T = trim(T) // SKP
       else if (BTEST(N, j)) then
          T = trim(T) // '1'
       else
          T = trim(T) // '0'
       endif
       if (mod(j, KMD).eq.KOF) T = trim(T) // SEP
    enddo
  end subroutine binstr_l

!!!_  & first_bit() - find first set-bit (return -1 if none)
  ELEMENTAL integer function first_bit_l(v) result(n)
    implicit none
    integer(kind=KI64),intent(in) :: v
    integer(kind=KI64),parameter  :: r = 2
    integer(kind=KI64) :: m

    m = v
    if (m.lt.0) then
       n = BIT_SIZE(v) - 1
    else if (m.eq.0) then
       n = -1
    else
       ! n = -1 + int(min(1, m))
       n = 0
       if (m.ge.r**32) then
          m = ISHFT(m, -32)
          n = n + 32
       endif
       if (m.ge.r**16) then
          m = ISHFT(m, -16)
          n = n + 16
       endif
       if (m.ge.r**8) then
          m = ISHFT(m, -8)
          n = n + 8
       endif
       if (m.ge.r**4) then
          m = ISHFT(m, -4)
          n = n + 4
       endif
       if (m.ge.r**2) then
          m = ISHFT(m, -2)
          n = n + 2
       endif
       if (m.ge.r**1) then
          ! m = ISHFT(m, -1)
          n = n + 1
       endif
    endif
  end function first_bit_l

  ELEMENTAL integer function first_bit_i(v) result(n)
    implicit none
    integer(kind=KI32),intent(in) :: v
    integer(kind=KI32),parameter  :: r = 2
    integer(kind=KI32) :: m

    m = v
    if (m.lt.0) then
       n = BIT_SIZE(v) - 1
    else if (m.eq.0) then
       n = -1
    else
       n = 0
       if (m.ge.r**16) then
          m = ISHFT(m, -16)
          n = n + 16
       endif
       if (m.ge.r**8) then
          m = ISHFT(m, -8)
          n = n + 8
       endif
       if (m.ge.r**4) then
          m = ISHFT(m, -4)
          n = n + 4
       endif
       if (m.ge.r**2) then
          m = ISHFT(m, -2)
          n = n + 2
       endif
       if (m.ge.r**1) then
          ! m = ISHFT(m, -1)
          n = n + 1
       endif
    endif
  end function first_bit_i

!!!_ + end of TOUZA_Trp_std
end module TOUZA_Trp_std

!!!_@ test_trapiche_std - test program
#ifdef TEST_TRAPICHE_STD
program test_trp_std
  use TOUZA_Trp_std
  implicit none
  integer ierr
  integer lbits

  call init(ierr, levv=+10, stdv=+10)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call finalize(ierr, levv=+10)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop

end program test_trp_std

#endif /* TEST_TRAPICHE_STD */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

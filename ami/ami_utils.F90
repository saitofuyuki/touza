!!!_! ami_utils.F90. - TOUZA/Ami/misc utilities
! Maintainer: SAITO Fuyuki
! Created: Dec 23 2022
#define TIME_STAMP 'Time-stamp: <2022/12/23 12:17:34 fuyuki ami_utils.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022, 2023
!           Japan Agency for Marine-Earth Science and Technology
!
!!!_* include
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_ami.h"
!!!_* macros
#ifndef   TEST_AMI_UTILS
#  define TEST_AMI_UTILS 0
#endif
!!!_@ TOUZA_Ami_utils - ami-da utilities
module TOUZA_Ami_utils
!!!_ + modules
  use TOUZA_Ami_std, as_init=>init, as_diag=>diag, as_finalize=>finalize
!!!_ + default
  implicit none
  private
!!!_ + parameter
    real(kind=KDBL),   parameter :: PI = 4.0_KDBL * ATAN(1.0_KDBL)
    complex(kind=KDBL),parameter :: CUNITI = (0.0, 1.0)
!!!_ + public
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
!!!_ + type
!!!_ + interfaces
  interface ZTOLON
     module procedure ZTOLON_d
  end interface ZTOLON
  interface ZTOLAT
     module procedure ZTOLAT_d
  end interface ZTOLAT
  interface CONF
     module procedure CONF_d
  end interface CONF
  interface RCONF
     module procedure RCONF_d
  end interface RCONF
  interface DCONF
     module procedure DCONF_d
  end interface DCONF
  interface STOZ
     module procedure STOZ_d
  end interface STOZ
  interface FMTRC
     module procedure FMTRC_d
  end interface FMTRC
!!!_ + public procedures
  public init, diag, finalize
  public ZTOLON, ZTOLAT
  public CONF,   RCONF,  DCONF,  STOZ,  FMTRC
!!!_ + function-like macros
!!!_ + procedures
contains
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm)
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
!!!_  - user procedures
!!!_  - functions for Coupler/Exchanger/Mediator tables
!!!_   & STOZ()
  complex(kind=KTGT) function STOZ_d(LON, LAT) result(c)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    real(kind=KTGT),   intent(in) :: LON
    real(kind=KTGT),   intent(in) :: LAT

    c = TAN(PI * 0.25_KTGT - LAT * 0.5_KTGT) * EXP(LON * cuniti)
  end function STOZ_d
!!!_   . ZTOLON()
  real(kind=KTGT) function ZTOLON_d(Z) result(c)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    complex(kind=KTGT),intent(in) :: Z
    c = ATAN2(AIMAG(Z), REAL(Z, kind=KTGT))      ! kind argument is redundant for complex
  end function ZTOLON_d
!!!_   . ZTOLAT()
  real(kind=KTGT) function ZTOLAT_d(Z) result(c)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    complex(kind=KTGT),intent(in) :: Z
    c = PI * 0.5_KTGT - 2.0_KTGT * ATAN(ABS(Z))
  end function ZTOLAT_d
!!!_   . CONF()
  complex(kind=KTGT) function CONF_d(Z, ZA, ZB, ZC, WC) result(c)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    complex(kind=KTGT),intent(in) :: Z, ZA, ZB, ZC, WC
    c = (- ZB * (ZC - ZA) * Z / WC + ZA * (ZC - ZB)) &
         &    / (- (ZC - ZA) * Z / WC + ZC - ZB)
  end function CONF_d

!!!_   . RCONF()
  complex(kind=KTGT) function RCONF_d(W, ZA, ZB, ZC, WC) result(c)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    complex(kind=KTGT),intent(in) :: W, ZA, ZB, ZC, WC
    c = (WC * (W - ZA) * (ZC - ZB)) / ((W - ZB) * (ZC - ZA))
  end function RCONF_d

!!!_   . DCONF()
  complex(kind=KTGT) function DCONF_d(Z, ZA, ZB, ZC, WC) result(c)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    complex(kind=KTGT),intent(in) :: Z, ZA, ZB, ZC, WC
    c =    (ZC - ZA) * (ZC - ZB) * (ZA - ZB) / WC &
         & / (ZC - ZB - (ZC - ZA) * Z / WC) &
         & / (ZC - ZB - (ZC - ZA) * Z / WC)
  end function DCONF_d

!!!_   . FMTRC()
  real(kind=KTGT) function FMTRC_d(ZP, ZQ, ZA, ZB, ZC, WC) result(r)
    use TOUZA_Ami_std,only: KTGT=>KDBL
    implicit none
    complex(kind=KTGT),intent(in) :: ZP, ZQ, ZA, ZB, ZC, WC
    r = (1.0_KTGT + ABS(ZQ) * ABS(ZQ)) * ABS(DCONF_d(ZQ, ZA, ZB, ZC, WC)) &
         & / (1.0_KTGT + ABS(ZP) * ABS(ZP))
  end function FMTRC_d

!!!_ + end AMI_UTILS
end module TOUZA_Ami_utils
!!!_@ test_ami_utils - test program
#if TEST_AMI_UTILS
program test_ami_utils
  use TOUZA_Ami_utils
  implicit none
  integer ierr

  ierr = 0

  write(*,*) 'fine = ', ierr

  stop
contains
end program test_ami_utils
#endif /* TEST_AMI_UTILS */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

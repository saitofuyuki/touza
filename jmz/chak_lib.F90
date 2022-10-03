!!!_! chak_lib.F90 - TOUZA/Jmz swiss(CH) army knife library
! Maintainer: SAITO Fuyuki
! Created: Oct 13 2022
#define TIME_STAMP 'Time-stamp: <2022/10/15 21:28:20 fuyuki chak_lib.F90>'
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
  use TOUZA_Std,only: msglev_normal, msglev_info
  use TOUZA_Std,only: is_msglev_DETAIL, is_msglev_NORMAL, is_msglev_INFO, is_msglev_DEBUG
  use TOUZA_Nio,only: litem, nitem
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

  real(kind=KBUF),parameter :: UNDEF  = LLIMIT
!!!_  - types
!!!_   . domain property
  type domain_t
     integer :: n                   ! total size
     integer :: mco                 ! coordinate size
     integer :: ofs(0:lcoor-1)
     integer :: bgn(0:lcoor-1)
     integer :: end(0:lcoor-1)
     integer :: iter(0:lcoor-1)
     integer :: strd(0:lcoor)       ! with sentry
     integer :: cidx(0:lcoor-1)
  end type domain_t
contains
!!!_  - common utilities
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
       ! jcur = mod(jlog, domL%strd(jc + 1)) / domL%strd(jc)
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
! !!!_   . copy_buffer_fill
!   subroutine copy_buffer_fill &
!        & (ierr, &
!        &  vL,   domL, &
!        &  vR,   domR, fill)
!     implicit none
!     integer,        intent(out) :: ierr
!     real(kind=KBUF),intent(out) :: vL(0:*)
!     real(kind=KBUF),intent(in)  :: vR(0:*)
!     type(domain_t), intent(in)  :: domL
!     type(domain_t), intent(in)  :: domR
!     real(kind=KBUF),intent(in)  :: fill

!     integer jlog, jphy

!     ierr = 0

!     do jlog = 0, domL%n - 1
!        jphy = conv_physical_index(jlog, domL, domR)
!        if (jphy.ge.0) then
!           vL(jlog) = vR(jphy)
!        else
!           vL(jlog) = fill
!        endif
!     enddo
!   end subroutine copy_buffer_fill

!!!_  - unary operations
!!!_   . apply_UNARY_template
  subroutine apply_UNARY_template &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_UNARY_template(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_template
!!!_   . apply_BINARY_template
  subroutine apply_BINARY_template &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_binary_template(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_template
!!!_   . apply_BINARY_lazy_template
  subroutine apply_BINARY_lazy_template &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_binary_template(Z(jz), X(jx), FZ, FX)
       else
          continue
       endif
    enddo
  end subroutine apply_BINARY_lazy_template

!!!_  - unary operations
!!!_   . apply_UNARY_NEG
  subroutine apply_UNARY_NEG &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_NEG(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_NEG
!!!_   . apply_UNARY_INV
  subroutine apply_UNARY_INV &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_INV(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_INV
!!!_   . apply_UNARY_ABS
  subroutine apply_UNARY_ABS &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_ABS(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_ABS
!!!_   . apply_UNARY_SQR
  subroutine apply_UNARY_SQR &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_SQR(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_SQR
!!!_   . apply_UNARY_SQRT
  subroutine apply_UNARY_SQRT &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_SQRT(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_SQRT
!!!_   . apply_UNARY_SIGN
  subroutine apply_UNARY_SIGN &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_SIGN(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_SIGN
!!!_   . apply_UNARY_ZSIGN
  subroutine apply_UNARY_ZSIGN &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_ZSIGN(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_ZSIGN
!!!_   . apply_UNARY_FLOOR
  subroutine apply_UNARY_FLOOR &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_FLOOR(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_FLOOR
!!!_   . apply_UNARY_CEIL
  subroutine apply_UNARY_CEIL &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_CEIL(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_CEIL
!!!_   . apply_UNARY_ROUND
  subroutine apply_UNARY_ROUND &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_ROUND(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_ROUND
!!!_   . apply_UNARY_TRUNC
  subroutine apply_UNARY_TRUNC &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_TRUNC(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_TRUNC
!!!_   . apply_UNARY_EXP
  subroutine apply_UNARY_EXP &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_EXP(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_EXP
!!!_   . apply_UNARY_LOG
  subroutine apply_UNARY_LOG &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_LOG(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_LOG
!!!_   . apply_UNARY_LOG10
  subroutine apply_UNARY_LOG10 &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_LOG10(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_LOG10
!!!_   . apply_UNARY_SIN
  subroutine apply_UNARY_SIN &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_SIN(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_SIN
!!!_   . apply_UNARY_COS
  subroutine apply_UNARY_COS &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_COS(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_COS
!!!_   . apply_UNARY_TAN
  subroutine apply_UNARY_TAN &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_TAN(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_TAN
!!!_   . apply_UNARY_TANH
  subroutine apply_UNARY_TANH &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_TANH(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_TANH
!!!_   . apply_UNARY_ASIN
  subroutine apply_UNARY_ASIN &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_ASIN(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_ASIN
!!!_   . apply_UNARY_ACOS
  subroutine apply_UNARY_ACOS &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_ACOS(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_ACOS
!!!_   . apply_UNARY_EXPONENT
  subroutine apply_UNARY_EXPONENT &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_EXPONENT(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_EXPONENT
!!!_   . apply_UNARY_FRACTION
  subroutine apply_UNARY_FRACTION &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_FRACTION(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_FRACTION
!!!_  - copy
!!!_   . apply_COPY
  subroutine apply_COPY &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = X(jx)
       else
          Z(jz) = F
       endif
    enddo
    return
  end subroutine apply_COPY
!!!_  - ubool operations
!!!_   . apply_UNARY_NOT
  subroutine apply_UNARY_NOT &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_NOT(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_NOT
!!!_   . apply_UNARY_BOOL
  subroutine apply_UNARY_BOOL &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_BOOL(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_BOOL
!!!_   . apply_UNARY_BIN
  subroutine apply_UNARY_BIN &
       & (ierr, Z, domZ, X, domX, F)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: F
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_BIN(X(jx), F)
       else
          Z(jz) = FALSE
       endif
    enddo
  end subroutine apply_UNARY_BIN
!!!_  - bool operations
!!!_   . apply_BINARY_EQB
  subroutine apply_BINARY_EQB &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_EQB(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FALSE
       endif
    enddo
  end subroutine apply_BINARY_EQB
!!!_   . apply_BINARY_NEB
  subroutine apply_BINARY_NEB &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_NEB(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FALSE
       endif
    enddo
  end subroutine apply_BINARY_NEB
!!!_   . apply_BINARY_LTB
  subroutine apply_BINARY_LTB &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_LTB(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FALSE
       endif
    enddo
  end subroutine apply_BINARY_LTB
!!!_   . apply_BINARY_GTB
  subroutine apply_BINARY_GTB &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_GTB(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FALSE
       endif
    enddo
  end subroutine apply_BINARY_GTB
!!!_   . apply_BINARY_LEB
  subroutine apply_BINARY_LEB &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_LEB(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FALSE
       endif
    enddo
  end subroutine apply_BINARY_LEB
!!!_   . apply_BINARY_GEB
  subroutine apply_BINARY_GEB &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_GEB(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FALSE
       endif
    enddo
  end subroutine apply_BINARY_GEB
!!!_   . apply_BINARY_EQ
  subroutine apply_BINARY_EQ &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_EQ(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_EQ
!!!_   . apply_BINARY_NE
  subroutine apply_BINARY_NE &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_NE(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_NE
!!!_   . apply_BINARY_LT
  subroutine apply_BINARY_LT &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_LT(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_LT
!!!_   . apply_BINARY_GT
  subroutine apply_BINARY_GT &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_GT(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_GT
!!!_   . apply_BINARY_LE
  subroutine apply_BINARY_LE &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_LE(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_LE
!!!_   . apply_BINARY_GE
  subroutine apply_BINARY_GE &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_GE(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_GE
!!!_  - binary operations
!!!_   . apply_BINARY_AND
  subroutine apply_BINARY_AND &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_AND(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_AND
!!!_   . apply_BINARY_MASK
  subroutine apply_BINARY_MASK &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_MASK(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_MASK
!!!_   . apply_BINARY_ADD
  subroutine apply_BINARY_ADD &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_ADD(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_ADD
!!!_   . apply_BINARY_SUB
  subroutine apply_BINARY_SUB &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_SUB(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_SUB
!!!_   . apply_BINARY_MUL
  subroutine apply_BINARY_MUL &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_MUL(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_MUL
!!!_   . apply_BINARY_DIV
  subroutine apply_BINARY_DIV &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_DIV(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_DIV
!!!_   . apply_BINARY_IDIV
  subroutine apply_BINARY_IDIV &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_IDIV(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_IDIV
!!!_   . apply_BINARY_MOD
  subroutine apply_BINARY_MOD &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_MOD(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_MOD
!!!_   . apply_BINARY_POW
  subroutine apply_BINARY_POW &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_POW(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_POW
!!!_   . apply_BINARY_ATAN2
  subroutine apply_BINARY_ATAN2 &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_ATAN2(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_ATAN2
!!!_   . apply_BINARY_SCALE
  subroutine apply_BINARY_SCALE &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_SCALE(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_SCALE
!!!_   . apply_BINARY_MIN
  subroutine apply_BINARY_MIN &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_MIN(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_MIN
!!!_   . apply_BINARY_MAX
  subroutine apply_BINARY_MAX &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_MAX(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_MAX
!!!_   . apply_BINARY_EQF
  subroutine apply_BINARY_EQF &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_EQF(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_EQF
!!!_   . apply_BINARY_NEF
  subroutine apply_BINARY_NEF &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_NEF(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_NEF
!!!_   . apply_BINARY_LTF
  subroutine apply_BINARY_LTF &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_LTF(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_LTF
!!!_   . apply_BINARY_GTF
  subroutine apply_BINARY_GTF &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_GTF(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_GTF
!!!_   . apply_BINARY_LEF
  subroutine apply_BINARY_LEF &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_LEF(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_LEF
!!!_   . apply_BINARY_GEF
  subroutine apply_BINARY_GEF &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_GEF(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_GEF
!!!_  - lazy operations
!!!_   . apply_BINARY_lazy_OR
  subroutine apply_BINARY_lazy_OR &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_OR(Z(jz), X(jx), FZ, FX)
       else
          continue
       endif
    enddo
  end subroutine apply_BINARY_lazy_OR
!!!_   . apply_BINARY_lazy_ROR
  subroutine apply_BINARY_lazy_ROR &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_ROR(Z(jz), X(jx), FZ, FX)
       else
          continue
       endif
    enddo
  end subroutine apply_BINARY_lazy_ROR
!!!_   . apply_BINARY_lazy_XOR
  subroutine apply_BINARY_lazy_XOR &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_XOR(Z(jz), X(jx), FZ, FX)
       else
          continue
       endif
    enddo
  end subroutine apply_BINARY_lazy_XOR
!!!_   . apply_BINARY_lazy_AND
  subroutine apply_BINARY_lazy_AND &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_AND(Z(jz), X(jx), FZ, FX)
       else
          continue
       endif
    enddo
  end subroutine apply_BINARY_lazy_AND
!!!_   . apply_BINARY_lazy_MASK
  subroutine apply_BINARY_lazy_MASK &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_MASK(Z(jz), X(jx), FZ, FX)
       else
          continue
       endif
    enddo
  end subroutine apply_BINARY_lazy_MASK
!!!_   . apply_BINARY_lazy_ADD
  subroutine apply_BINARY_lazy_ADD &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_ADD(Z(jz), X(jx), FZ, FX)
       else
          continue
       endif
    enddo
  end subroutine apply_BINARY_lazy_ADD
!!!_   . apply_BINARY_lazy_SUB
  subroutine apply_BINARY_lazy_SUB &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_SUB(Z(jz), X(jx), FZ, FX)
       else
          continue
       endif
    enddo
  end subroutine apply_BINARY_lazy_SUB
!!!_   . apply_BINARY_lazy_MUL
  subroutine apply_BINARY_lazy_MUL &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_MUL(Z(jz), X(jx), FZ, FX)
       else
          continue
       endif
    enddo
  end subroutine apply_BINARY_lazy_MUL
!!!_   . apply_BINARY_lazy_DIV
  subroutine apply_BINARY_lazy_DIV &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_DIV(Z(jz), X(jx), FZ, FX)
       else
          continue
       endif
    enddo
  end subroutine apply_BINARY_lazy_DIV
!!!_   . apply_BINARY_lazy_LMIN
  subroutine apply_BINARY_lazy_LMIN &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_LMIN(Z(jz), X(jx), FZ, FX)
       else
          continue
       endif
    enddo
  end subroutine apply_BINARY_lazy_LMIN
!!!_   . apply_BINARY_lazy_LMAX
  subroutine apply_BINARY_lazy_LMAX &
       & (ierr, Z, domZ, FZ, X, domX, FX)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: Z(0:*)
    real(kind=KBUF),intent(in)  :: X(0:*)
    type(domain_t), intent(in)  :: domZ, domX
    real(kind=KBUF),intent(in)  :: FZ, FX
    integer jz, jx
    ierr = 0
    do jz = 0, domZ%n - 1
       jx = conv_physical_index(jz, domZ, domX)
       if (jx.ge.0) then
          Z(jz) = elem_LMAX(Z(jz), X(jx), FZ, FX)
       else
          continue
       endif
    enddo
  end subroutine apply_BINARY_lazy_LMAX

!!!_  - elemental operations
!!!_   . templates
!!!_    * elem_UNARY_template()
  ELEMENTAL &
  real(kind=KBUF) function elem_UNARY_template (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = ABS(X)
    endif
  end function elem_UNARY_template
!!!_    * elem_BINARY_template()
  ELEMENTAL &
  real(kind=KBUF) function elem_BINARY_template (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else
       Z = X + Y
    endif
  end function elem_BINARY_template
!!!_   . unary operators
!!!_    * elem_ABS()
  ELEMENTAL &
  real(kind=KBUF) function elem_ABS (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = ABS(X)
    endif
  end function elem_ABS
!!!_    * elem_NEG()
  ELEMENTAL &
  real(kind=KBUF) function elem_NEG (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = - X
    endif
  end function elem_NEG

!!!_    * elem_ZSIGN() - return -1,0,+1 if negative, zero, positive
  ELEMENTAL &
  real(kind=KBUF) function elem_ZSIGN (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else if (F.eq.ZERO) then
       Z = ZERO
    else
       Z = SIGN(ONE, X)
    endif
  end function elem_ZSIGN

!!!_    * elem_SIGN() - SIGN(1,Z)
  ELEMENTAL &
  real(kind=KBUF) function elem_SIGN (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = SIGN(ONE, X)
    endif
  end function elem_SIGN

!!!_    * elem_INV()
  ELEMENTAL &
  real(kind=KBUF) function elem_INV (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else if (X.eq.ZERO) then
       Z = F
    else
       Z = ONE / X
    endif
  end function elem_INV

!!!_    * elem_SQR()
  ELEMENTAL &
  real(kind=KBUF) function elem_SQR (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = X * X
    endif
  end function elem_SQR

!!!_    * elem_EXP()
  ELEMENTAL &
  real(kind=KBUF) function elem_EXP (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = EXP(X)
    endif
  end function elem_EXP

!!!_    * elem_LOG()
  ELEMENTAL &
  real(kind=KBUF) function elem_LOG (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else if (X.le.ZERO) then
       Z = F
    else
       Z = LOG(X)
    endif
  end function elem_LOG

!!!_    * elem_LOG10()
  ELEMENTAL &
  real(kind=KBUF) function elem_LOG10 (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else if (X.le.ZERO) then
       Z = F
    else
       Z = LOG10(X)
    endif
  end function elem_LOG10

!!!_    * elem_SQRT()
  ELEMENTAL &
  real(kind=KBUF) function elem_SQRT (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else if (X.lt.ZERO) then
       Z = F
    else
       Z = SQRT(X)
    endif
  end function elem_SQRT

!!!_    * elem_SIN()
  ELEMENTAL &
  real(kind=KBUF) function elem_SIN (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = SIN(X)
    endif
  end function elem_SIN

!!!_    * elem_COS()
  ELEMENTAL &
  real(kind=KBUF) function elem_COS (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = COS(X)
    endif
  end function elem_COS

!!!_    * elem_TAN()
  ELEMENTAL &
  real(kind=KBUF) function elem_TAN (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = TAN(X)
    endif
  end function elem_TAN

!!!_    * elem_ASIN()
  ELEMENTAL &
  real(kind=KBUF) function elem_ASIN (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = ASIN(X)
    endif
  end function elem_ASIN

!!!_    * elem_ACOS()
  ELEMENTAL &
  real(kind=KBUF) function elem_ACOS (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = ACOS(X)
    endif
  end function elem_ACOS

!!!_    * elem_TANH()
  ELEMENTAL &
  real(kind=KBUF) function elem_TANH (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = TANH(X)
    endif
  end function elem_TANH

!!!_    * elem_EXPONENT()
  ELEMENTAL &
  real(kind=KBUF) function elem_EXPONENT (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = real(EXPONENT(X), kind=KBUF)
    endif
  end function elem_EXPONENT

!!!_    * elem_FRACTION()
  ELEMENTAL &
  real(kind=KBUF) function elem_FRACTION (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = FRACTION(X)
    endif
  end function elem_FRACTION

!!!_    * elem_NOT()
  ELEMENTAL &
  real(kind=KBUF) function elem_NOT (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = TRUE
    else
       Z = F
    endif
  end function elem_NOT

!!!_    * elem_BOOL()
  ELEMENTAL &
  real(kind=KBUF) function elem_BOOL (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = TRUE
    endif
  end function elem_BOOL

!!!_    * elem_BIN()
  ELEMENTAL &
  real(kind=KBUF) function elem_BIN (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = TRUE
    else
       Z = FALSE
    endif
  end function elem_BIN

!!!_    * elem_FLOOR()
  ELEMENTAL &
  real(kind=KBUF) function elem_FLOOR (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = FLOOR(X)
    endif
  end function elem_FLOOR

!!!_    * elem_CEIL()
  ELEMENTAL &
  real(kind=KBUF) function elem_CEIL (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = CEILING(X)
    endif
  end function elem_CEIL

!!!_    * elem_ROUND()
  ELEMENTAL &
  real(kind=KBUF) function elem_ROUND (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = ANINT(X)
    endif
  end function elem_ROUND

!!!_    * elem_TRUNC()
  ELEMENTAL &
  real(kind=KBUF) function elem_TRUNC (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = AINT(X)
    endif
  end function elem_TRUNC
!!!_   . binary operators
!!!_    & elem_ADD() - X + Y
  ELEMENTAL &
  real(kind=KBUF) function elem_ADD (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else
       Z = X + Y
    endif
  end function elem_ADD
!!!_    & elem_SUB() - X - Y
  ELEMENTAL &
  real(kind=KBUF) function elem_SUB (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else
       Z = X - Y
    endif
  end function elem_SUB
!!!_    & elem_MUL() - X * Y
  ELEMENTAL &
  real(kind=KBUF) function elem_MUL (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else
       Z = X * Y
    endif
  end function elem_MUL
!!!_    & elem_DIV() - X / Y
  ELEMENTAL &
  real(kind=KBUF) function elem_DIV (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else if (Y.eq.ZERO) then
       Z = FX
    else
       Z = X / Y
    endif
  end function elem_DIV

!!!_    & elem_IDIV() - integer division
  ELEMENTAL &
  real(kind=KBUF) function elem_IDIV (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else if (Y.eq.ZERO) then
       Z = FX
    else
       Z = REAL(INT(X) / INT(Y), kind=KBUF)
    endif
  end function elem_IDIV

!!!_    & elem_MOD() - X % Y
  ELEMENTAL &
  real(kind=KBUF) function elem_MOD (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else if (Y.eq.ZERO) then
       Z = FX
    else
       Z = MOD(X, Y)
    endif
  end function elem_MOD

!!!_    & elem_POW() - X ** Y
  ELEMENTAL &
  real(kind=KBUF) function elem_POW (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else if (X.eq.ZERO.and.Y.le.ZERO) then
       Z = FX
    else if (X.lt.ZERO.and.ANINT(Y).ne.Y) then
       Z = FX
    else
       Z = X ** Y
    endif
  end function elem_POW

!!!_    & elem_MIN() - MIN(X, Y)
  ELEMENTAL &
  real(kind=KBUF) function elem_MIN (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else
       Z = MIN(X, Y)
    endif
  end function elem_MIN

!!!_    & elem_MAX() - MAX(X, Y)
  ELEMENTAL &
  real(kind=KBUF) function elem_MAX (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else
       Z = MAX(X, Y)
    endif
  end function elem_MAX

!!!_    & elem_LMIN() - MIN(X, Y) lazy  (X or Y if the other == NAN)
  ELEMENTAL &
  real(kind=KBUF) function elem_LMIN (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX) then
       if (Y.eq.FY) then
          Z = FX
       else
          Z = Y
       endif
    else if (Y.eq.FY) then
       Z = X
    else
       Z = MIN(X, Y)
    endif
  end function elem_LMIN

!!!_    & elem_LMAX() - MAX(X, Y) lazy  (X or Y if the other == NAN)
  ELEMENTAL &
  real(kind=KBUF) function elem_LMAX (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX) then
       if (Y.eq.FY) then
          Z = FX
       else
          Z = Y
       endif
    else if (Y.eq.FY) then
       Z = X
    else
       Z = MAX(X, Y)
    endif
  end function elem_LMAX

!!!_    & elem_AND() - Y if X != NAN else X
  ! (cf. gmtmath) Y if X == NAN, else X
  !    operands  jmz  gmt
  !    X Y       Y    X
  !    X N       N    X
  !    N Y       N    Y
  !    N N       N    N
  ELEMENTAL &
  real(kind=KBUF) function elem_AND (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX) then
       Z = FX
    else if (Y.eq.FY) then
       Z = FX
    else
       Z = Y
    endif
  end function elem_AND

!!!_    & elem_OR() - Y if X == NAN, else X  (same as gmt AND)
  ! (cf. gmtmath) NAN if Y == NAN, else X
  !    operands  jmz  gmt
  !    X Y       X    X
  !    X N       X    N
  !    N Y       Y    N
  !    N N       N    N
  ELEMENTAL &
  real(kind=KBUF) function elem_OR (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX) then
       if (Y.eq.FY) then
          Z = FX
       else
          Z = Y
       endif
    else
       Z = X
    endif
  end function elem_OR

!!!_    & elem_ROR() - X if Y == NAN, else Y
  ELEMENTAL &
  real(kind=KBUF) function elem_ROR (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (Y.eq.FY) then
       Z = X
    else
       Z = Y
    endif
  end function elem_ROR

!!!_    & elem_MASK() - NAN if Y == NAN, else X  (same as gmt OR)
  ELEMENTAL &
  real(kind=KBUF) function elem_MASK (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (Y.eq.FY) then
       Z = FX
    else
       Z = X
    endif
  end function elem_MASK

!!!_    & elem_XOR() - Y if X == NAN, X if Y == NAN, else NAN
  ! (cf. gmtmath) Y if X == NAN, else X (AND identical)
  !    operands  jmz  gmt
  !    X Y       N    X
  !    X N       X    X
  !    N Y       Y    Y
  !    N N       N    N
  ELEMENTAL &
  real(kind=KBUF) function elem_XOR (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX) then
       if (Y.eq.FY) then
          Z = FX
       else
          Z = Y
       endif
    else if (Y.eq.FY) then
       Z = X
    else
       Z = FX
    endif
  end function elem_XOR

!!!_    & elem_EQB() - binary for if X == Y
  ELEMENTAL &
  real(kind=KBUF) function elem_EQB (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FALSE
    else if (X.eq.Y) then
       Z = TRUE
    else
       Z = FALSE
    endif
  end function elem_EQB

!!!_    & elem_NEB() - binary for if X != Y
  ELEMENTAL &
  real(kind=KBUF) function elem_NEB (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FALSE
    else if (X.ne.Y) then
       Z = TRUE
    else
       Z = FALSE
    endif
  end function elem_NEB

!!!_    & elem_LTB() - binary for if X < Y
  ELEMENTAL &
  real(kind=KBUF) function elem_LTB (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FALSE
    else if (X.lt.Y) then
       Z = TRUE
    else
       Z = FALSE
    endif
  end function elem_LTB

!!!_    & elem_LEB() - binary for if X <= Y
  ELEMENTAL &
  real(kind=KBUF) function elem_LEB (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FALSE
    else if (X.le.Y) then
       Z = TRUE
    else
       Z = FALSE
    endif
  end function elem_LEB

!!!_    & elem_GTB() - binary for if X > Y
  ELEMENTAL &
  real(kind=KBUF) function elem_GTB (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FALSE
    else if (X.gt.Y) then
       Z = TRUE
    else
       Z = FALSE
    endif
  end function elem_GTB

!!!_    & elem_GEB() - binary for X if X >= Y
  ELEMENTAL &
  real(kind=KBUF) function elem_GEB (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FALSE
    else if (X.ge.Y) then
       Z = TRUE
    else
       Z = FALSE
    endif
  end function elem_GEB

!!!_    & elem_EQF() - X if X == Y, else NAN
  ELEMENTAL &
  real(kind=KBUF) function elem_EQF (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else if (X.eq.Y) then
       Z = X
    else
       Z = FX
    endif
  end function elem_EQF

!!!_    & elem_NEF() - X if X != Y, else NAN
  ELEMENTAL &
  real(kind=KBUF) function elem_NEF (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else if (X.ne.Y) then
       Z = X
    else
       Z = FX
    endif
  end function elem_NEF

!!!_    & elem_LTF() - X if X < Y, else NAN
  ELEMENTAL &
  real(kind=KBUF) function elem_LTF (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else if (X.lt.Y) then
       Z = X
    else
       Z = FX
    endif
  end function elem_LTF

!!!_    & elem_LEF() - X if X <= Y, else NAN
  ELEMENTAL &
  real(kind=KBUF) function elem_LEF (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else if (X.le.Y) then
       Z = X
    else
       Z = FX
    endif
  end function elem_LEF

!!!_    & elem_GTF() - X if X > Y, else NAN
  ELEMENTAL &
  real(kind=KBUF) function elem_GTF (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else if (X.gt.Y) then
       Z = X
    else
       Z = FX
    endif
  end function elem_GTF

!!!_    & elem_GEF() - X if X >= Y, else NAN
  ELEMENTAL &
  real(kind=KBUF) function elem_GEF (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else if (X.ge.Y) then
       Z = X
    else
       Z = FX
    endif
  end function elem_GEF

!!!_    & elem_EQ() - binary or UNDEF for if X == Y
  ELEMENTAL &
  real(kind=KBUF) function elem_EQ (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else if (X.eq.Y) then
       Z = TRUE
    else
       Z = FALSE
    endif
  end function elem_EQ

!!!_    & elem_NE() - binary or UNDEF for if X != Y
  ELEMENTAL &
  real(kind=KBUF) function elem_NE (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else if (X.ne.Y) then
       Z = TRUE
    else
       Z = FALSE
    endif
  end function elem_NE

!!!_    & elem_LT() - binary/UNDEF for if X < Y
  ELEMENTAL &
  real(kind=KBUF) function elem_LT (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else if (X.lt.Y) then
       Z = TRUE
    else
       Z = FALSE
    endif
  end function elem_LT

!!!_    & elem_LE() - binary/UNDEF for if X <= Y
  ELEMENTAL &
  real(kind=KBUF) function elem_LE (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else if (X.le.Y) then
       Z = TRUE
    else
       Z = FALSE
    endif
  end function elem_LE

!!!_    & elem_GT() - binary/UNDEF for if X > Y
  ELEMENTAL &
  real(kind=KBUF) function elem_GT (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else if (X.gt.Y) then
       Z = TRUE
    else
       Z = FALSE
    endif
  end function elem_GT

!!!_    & elem_GE() - X if X >= Y, else NAN
  ELEMENTAL &
  real(kind=KBUF) function elem_GE (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else if (X.ge.Y) then
       Z = X
    else
       Z = FX
    endif
  end function elem_GE

!!!_    & elem_ATAN2() - TAN2(X, Y)
  ELEMENTAL &
  real(kind=KBUF) function elem_ATAN2 (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else
       Z = ATAN2(X, Y)
    endif
  end function elem_ATAN2

!!!_    & elem_SCALE()
  ELEMENTAL &
  real(kind=KBUF) function elem_SCALE (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else
       Z = SCALE(X, INT(Y))
    endif
  end function elem_SCALE

!!!_ + end chak
end module chak_lib
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

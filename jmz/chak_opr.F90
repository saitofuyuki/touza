!!!_! chak_opr.F90 - TOUZA/Jmz CH(swiss) army knife operation primitives
! Maintainer: SAITO Fuyuki
! Created: Nov 4 2022
#define TIME_STAMP 'Time-stamp: <2023/06/15 19:56:20 fuyuki chak_opr.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022, 2023
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "jmz_config.h"
#endif
#include "jmz.h"
!!!_@ TOUZA/Jmz/chak-opr - nio swiss army knife (operations)
module chak_opr
!!!_ + Declaration
!!!_  - modules
  use chak_lib,lib_init=>init
  implicit none
  public
!!!_  - declaration
!!!_   . operators
#include "chak_decl.F90"

!!!_   . infix levels
  integer,parameter :: ilev_unset = 0
  integer,parameter :: ilev_term = 1
  integer,parameter :: ilev_call = 2
  integer,parameter :: ilev_neg = 3
  integer,parameter :: ilev_exp = 4
  integer,parameter :: ilev_mul = 5
  integer,parameter :: ilev_add = 6
  integer,parameter :: ilev_shift = 7
  integer,parameter :: ilev_and = 8
  integer,parameter :: ilev_xor = 9
  integer,parameter :: ilev_or = 10
  integer,parameter :: ilev_logical = 11

!!!_   . operator properties
  type opr_t
     integer :: push = -1
     integer :: pop = -1
     integer :: entr = -1
     integer :: ilev = ilev_unset
     character(len=8) :: istr = ' '
  end type opr_t
  type(opr_t),save,private :: oprop(0:lopr-1)

!!!_   . hash table
  integer,save,private :: htopr = -1
!!!_ + Procedures
contains
!!!_  - init
  subroutine init(ierr)
    implicit none
    integer,intent(out) :: ierr
    ierr = 0
    if (ierr.eq.0) then
       if (htopr.lt.0) call register_operators(ierr)
    endif
  end subroutine init
!!!_  - diag
  subroutine diag(ierr, u, levv)
    use TOUZA_Std,only: htb_diag
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    ierr = 0
    if (ierr.eq.0) then
       call htb_diag(ierr, u, levv)
    endif
  end subroutine diag
!!!_  - finalize
  subroutine finalize(ierr, u, levv)
    use TOUZA_Std,only: htb_finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    ierr = 0
    if (ierr.eq.0) then
       call htb_finalize(ierr, u, levv)
    endif
  end subroutine finalize
!!!_  - registration
!!!_   . register_operators
  subroutine register_operators(ierr)
    use TOUZA_Std,only: htb_init, new_htable, diag_htable
    implicit none
    integer,intent(out) :: ierr

    ierr = 0
    call htb_init(ierr)
    htopr = new_htable('operators', lopr, oprlen, nstt=1)
    ierr = min(0, htopr)

#   include "chak_reg.F90"

    ! call diag_htable(ierr, htopr)

  end subroutine register_operators
!!!_   . reg_opr_prop
  subroutine reg_opr_prop &
       & (ierr, idopr, str, pop, push, ilev, istr)
    use TOUZA_Std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: idopr
    character(len=*),intent(in)          :: str
    integer,         intent(in),optional :: pop
    integer,         intent(in),optional :: push
    integer,         intent(in),optional :: ilev
    character(len=*),intent(in),optional :: istr
    integer entr
    if (idopr.ge.lopr.or.idopr.lt.0) then
       ierr = ERR_PANIC
       call message(ierr, 'panic in operator registration')
       return
    endif
    entr = reg_opr_core(str, idopr)
    ierr = min(0, entr)
    if (ierr.eq.0) then
       if (oprop(idopr)%entr.lt.0) then
          oprop(idopr)%entr = entr
          oprop(idopr)%push = choice(-1, push)
          oprop(idopr)%pop = choice(-1, pop)
          oprop(idopr)%ilev = choice(ilev_unset, ilev)
          if (present(istr)) then
             oprop(idopr)%istr = istr
          else
             oprop(idopr)%istr = ' '
          endif
       else
          ! alias
          continue
       endif
       mopr = max(mopr, idopr + 1)
    endif
  end subroutine reg_opr_prop
!!!_   . reg_fake_opr
  subroutine reg_fake_opr &
       & (entr, handle, str)
    use TOUZA_Std,only: query_entry
    implicit none
    integer,         intent(out) :: entr
    integer,         intent(in)  :: handle
    character(len=*),intent(in)  :: str
    integer ho, jerr
    entr = query_entry(htopr, str)
    if (entr.ge.0) then
       ho = query_opr_handle_e(entr)
       if (ho.eq.handle) then
          continue
       else
          entr = ERR_DUPLICATE_SET
          call message(entr, 'duplicate registration ' // trim(str))
       endif
    else
       entr = reg_opr_core(str, handle)
    endif
  end subroutine reg_fake_opr
!!!_   . parse_term_operator()
  integer function parse_term_operator(str) result(ho)
    implicit none
    character(len=*),intent(in) :: str
    integer jb
    ho = query_opr_handle_n(str)
    if (ho.lt.0) then
       jb = index(str, param_sep)
       if (jb.gt.1) then
          ho = query_opr_handle_n(str(1:jb-1))
       endif
    endif
  end function parse_term_operator
!!!_   . inquire_opr_nstack
  subroutine inquire_opr_nstack(ierr, pop, push, handle)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: pop, push
    integer,intent(in)  :: handle
    ! integer jb
    ierr = 0

    if (handle.lt.0.or.handle.ge.mopr) then
       ! jb = buf_h2item(handle)
       ! if (jb.lt.0) then
       ierr = ERR_INVALID_ITEM
       call message(ierr, 'invalid operator handle to inquire', (/handle/))
       ! else
       !    pop = 0
       !    push = 1
       ! endif
    else
       pop = oprop(handle)%pop
       push = oprop(handle)%push
    endif
  end subroutine inquire_opr_nstack
!!!_   . inquire_opr_infix
  subroutine inquire_opr_infix(ierr, ilev, istr, handle)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: ilev
    character(len=*),intent(out) :: istr
    integer,         intent(in)  :: handle
    ierr = 0

    if (handle.lt.0.or.handle.ge.mopr) then
       ierr = ERR_INVALID_ITEM
       call message(ierr, 'invalid operator handle to inquire', (/handle/))
       ilev = ilev_unset
       istr = ' '
    else
       ilev = oprop(handle)%ilev
       istr = oprop(handle)%istr
       if (istr.eq.' ') then
          call query_opr_name(ierr, istr, handle)
       endif
    endif
  end subroutine inquire_opr_infix
!!!_   . is_operator_modify()
  logical function is_operator_modify(handle) result(b)
    implicit none
    integer,intent(in) :: handle
    b = handle.eq.opr_TRANSF
  end function is_operator_modify
!!!_   . is_operator_reusable()
  logical function is_operator_reusable(handle) result(b)
    implicit none
    integer,intent(in) :: handle
    integer pop, push
    integer jerr
    call inquire_opr_nstack(jerr, pop, push, handle)
    ! b = pop.eq.1 .and. push.eq.1
    b = .FALSE.
  end function is_operator_reusable
!!!_   . switch_shape_operator ()
  integer function switch_shape_operator(opr) result(n)
    implicit none
    integer,intent(in) :: opr
    select case(opr)
    case(opr_SHAPE,opr_TRANSF)
       n = shape_element
    case(opr_PERM)
       n = shape_coordinate
    case(opr_SIZE)
       n = shape_size
    case(opr_SHIFT)
       n = shape_shift
    case(opr_FLAT)
       n = shape_coordinate
    case default
       n = shape_error
    end select
    return
  end function switch_shape_operator

!!!_  - htb wrappers
!!!_   . reg_opr_core
  integer function reg_opr_core &
       & (name, idopr) &
       & result(ee)
    use TOUZA_Std,only: reg_entry
    implicit none
    character(len=*),intent(in) :: name
    integer,         intent(in) :: idopr
    ee = reg_entry(htopr, name, status=(/idopr/))
  end function reg_opr_core
!!!_   . query_opr_name - query operator name by handle
  subroutine query_opr_name &
       & (ierr, str, handle)
    use TOUZA_Std,only: query_key
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    integer,         intent(in)  :: handle
    if (handle.lt.0.or.handle.ge.mopr) then
       ierr = ERR_INVALID_ITEM
       call message(ierr, 'invalid operator handle', (/handle/))
       return
    endif
    call query_key(ierr, htopr, oprop(handle)%entr, str)
  end subroutine query_opr_name
!!!_   . query_opr_name_e - query operator name by entry
  subroutine query_opr_name_e &
       & (ierr, str, entr)
    use TOUZA_Std,only: query_key
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    integer,         intent(in)  :: entr
    call query_key(ierr, htopr, entr, str)
  end subroutine query_opr_name_e
!!!_   . query_opr_handle_n - query operator handle by name
  integer function query_opr_handle_n &
       & (name) &
       & result(ho)
    use TOUZA_Std,only: query_status
    implicit none
    character(len=*),intent(in) :: name
    integer jerr
    call query_status(jerr, ho, htopr, name)
    if (jerr.lt.0) ho = jerr
  end function query_opr_handle_n
!!!_   . query_opr_handle_e - query operator handle by table entry
  integer function query_opr_handle_e &
       & (entr) &
       & result(ho)
    use TOUZA_Std,only: query_status_entr
    implicit none
    integer,intent(in) :: entr
    integer jerr
    call query_status_entr(jerr, ho, htopr, entr)
    if (jerr.lt.0) ho = jerr
  end function query_opr_handle_e
!!!_  - templates
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
!!!_   . apply_UNARY_ATAN
  subroutine apply_UNARY_ATAN &
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
          Z(jz) = elem_ATAN(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_ATAN
!!!_   . apply_UNARY_SINH
  subroutine apply_UNARY_SINH &
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
          Z(jz) = elem_SINH(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_SINH
!!!_   . apply_UNARY_COSH
  subroutine apply_UNARY_COSH &
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
          Z(jz) = elem_COSH(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_COSH
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
!!!_   . apply_UNARY_R2D
  subroutine apply_UNARY_R2D &
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
          Z(jz) = elem_R2D(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_R2D
!!!_   . apply_UNARY_D2R
  subroutine apply_UNARY_D2R &
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
          Z(jz) = elem_D2R(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_D2R
!!!_   . apply_BINARY_HYPOT
  subroutine apply_BINARY_HYPOT &
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
          Z(jz) = elem_HYPOT(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_HYPOT
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
!!!_   . apply_BINARY_MODULO
  subroutine apply_BINARY_MODULO &
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
          Z(jz) = elem_MODULO(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_MODULO
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
!!!_   . apply_BINARY_lazy_LAY
  subroutine apply_BINARY_lazy_LAY &
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
          Z(jz) = elem_LAY(Z(jz), X(jx), FZ, FX)
       else
          continue
       endif
    enddo
  end subroutine apply_BINARY_lazy_LAY
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
!!!_   . apply_BINARY_NEAREST
  subroutine apply_BINARY_NEAREST &
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
          Z(jz) = elem_NEAREST(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_NEAREST

!!!_   . apply_UNARY_BITNOT
  subroutine apply_UNARY_BITNOT &
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
          Z(jz) = elem_BITNOT(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_BITNOT
!!!_   . apply_UNARY_SPACING
  subroutine apply_UNARY_SPACING &
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
          Z(jz) = elem_SPACING(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_SPACING
!!!_   . apply_UNARY_RRSP
  subroutine apply_UNARY_RRSP &
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
          Z(jz) = elem_RRSP(X(jx), F)
       else
          Z(jz) = F
       endif
    enddo
  end subroutine apply_UNARY_RRSP

!!!_   . apply_BINARY_BITAND
  subroutine apply_BINARY_BITAND &
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
          Z(jz) = elem_BITAND(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_BITAND
!!!_   . apply_BINARY_BITOR
  subroutine apply_BINARY_BITOR &
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
          Z(jz) = elem_BITOR(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_BITOR
!!!_   . apply_BINARY_BITXOR
  subroutine apply_BINARY_BITXOR &
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
          Z(jz) = elem_BITXOR(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_BITXOR
!!!_   . apply_BINARY_LSHIFT
  subroutine apply_BINARY_LSHIFT &
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
          Z(jz) = elem_LSHIFT(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_LSHIFT
!!!_   . apply_BINARY_RSHIFT
  subroutine apply_BINARY_RSHIFT &
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
          Z(jz) = elem_RSHIFT(Z(jz), X(jx), FZ, FX)
       else
          Z(jz) = FZ
       endif
    enddo
  end subroutine apply_BINARY_RSHIFT

!!!_  - elemental operatior templates
!!!_   . elem_UNARY_template()
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
!!!_   . elem_BINARY_template()
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
!!!_  - elemental unary operators
!!!_   & elem_ABS()
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
!!!_   & elem_NEG()
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
!!!_   & elem_ZSIGN() - return -1,0,+1 if negative, zero, positive
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
!!!_   & elem_SIGN() - SIGN(1,Z)
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
!!!_   & elem_INV()
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
!!!_   & elem_SQR()
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
!!!_   & elem_EXP()
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
!!!_   & elem_LOG()
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
!!!_   & elem_LOG10()
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
!!!_   & elem_SQRT()
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
!!!_   & elem_SIN()
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
!!!_   & elem_COS()
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
!!!_   & elem_TAN()
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
!!!_   & elem_ASIN()
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
!!!_   & elem_ACOS()
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
!!!_   & elem_ATAN()
  ELEMENTAL &
  real(kind=KBUF) function elem_ATAN (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = ATAN(X)
    endif
  end function elem_ATAN
!!!_   & elem_SINH ()
  ELEMENTAL &
  real(kind=KBUF) function elem_SINH (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = SINH(X)
    endif
  end function elem_SINH
!!!_   & elem_COSH ()
  ELEMENTAL &
  real(kind=KBUF) function elem_COSH (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = COSH(X)
    endif
  end function elem_COSH
!!!_   & elem_TANH()
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
!!!_   & elem_R2D ()
  ELEMENTAL &
  real(kind=KBUF) function elem_R2D (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = (X / PI) * 180.0_KBUF
    endif
  end function elem_R2D
!!!_   & elem_D2R ()
  ELEMENTAL &
  real(kind=KBUF) function elem_D2R (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = (X / 180.0_KBUF) * PI
    endif
  end function elem_D2R
!!!_   & elem_EXPONENT()
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
!!!_   & elem_FRACTION()
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
!!!_   & elem_NOT()
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
!!!_   & elem_BOOL()
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
!!!_   & elem_BIN()
  ELEMENTAL &
  real(kind=KBUF) function elem_BIN (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = FALSE
    else
       Z = TRUE
    endif
  end function elem_BIN
!!!_   & elem_FLOOR()
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
!!!_   & elem_CEIL()
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
!!!_   & elem_ROUND()
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
!!!_   & elem_TRUNC()
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
!!!_    * elem_SPACING ()
  ELEMENTAL &
  real(kind=KBUF) function elem_SPACING (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = SPACING(X)
    endif
  end function elem_SPACING
!!!_    * elem_RRSP ()
  ELEMENTAL &
  real(kind=KBUF) function elem_RRSP (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = RRSPACING(X)
    endif
  end function elem_RRSP
!!!_  - elemental binary operators
!!!_   & elem_ADD() - X + Y
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
!!!_   & elem_SUB() - X - Y
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
!!!_   & elem_MUL() - X * Y
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
!!!_   & elem_DIV() - X / Y
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
!!!_   & elem_IDIV() - integer division
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
!!!_   & elem_MOD() - X % Y
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
!!!_   & elem_POW() - X ** Y
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
!!!_   & elem_MODULO() - modulo(X, Y)
  ELEMENTAL &
  real(kind=KBUF) function elem_MODULO (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else
       Z = MODULO(X, Y)
    endif
  end function elem_MODULO
!!!_   & elem_MIN() - MIN(X, Y)
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
!!!_   & elem_MAX() - MAX(X, Y)
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
!!!_   & elem_LMIN() - MIN(X, Y) lazy  (X or Y if the other == NAN)
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
!!!_   & elem_LMAX() - MAX(X, Y) lazy  (X or Y if the other == NAN)
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
!!!_   & elem_AND() - Y if X != NAN else X
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
!!!_   & elem_OR() - Y if X == NAN, else X  (same as gmt AND)
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
!!!_   & elem_ROR() - X if Y == NAN, else Y
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
!!!_   & elem_MASK() - NAN if Y == NAN, else X  (same as gmt OR)
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
!!!_   & elem_LAY()
  ELEMENTAL &
  real(kind=KBUF) function elem_LAY (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (Y.eq.FY) then
       Z = FX
    else
       Z = Y
    endif
  end function elem_LAY

!!!_   & elem_XOR() - Y if X == NAN, X if Y == NAN, else NAN
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
!!!_   & elem_EQB() - binary for if X == Y
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
!!!_   & elem_NEB() - binary for if X != Y
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
!!!_   & elem_LTB() - binary for if X < Y
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
!!!_   & elem_LEB() - binary for if X <= Y
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
!!!_   & elem_GTB() - binary for if X > Y
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
!!!_   & elem_GEB() - binary for X if X >= Y
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
!!!_   & elem_EQF() - X if X == Y, else NAN
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
!!!_   & elem_NEF() - X if X != Y, else NAN
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
!!!_   & elem_LTF() - X if X < Y, else NAN
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
!!!_   & elem_LEF() - X if X <= Y, else NAN
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
!!!_   & elem_GTF() - X if X > Y, else NAN
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
!!!_   & elem_GEF() - X if X >= Y, else NAN
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
!!!_   & elem_EQ() - binary or UNDEF for if X == Y
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
!!!_   & elem_NE() - binary or UNDEF for if X != Y
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
!!!_   & elem_LT() - binary/UNDEF for if X < Y
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
!!!_   & elem_LE() - binary/UNDEF for if X <= Y
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
!!!_   & elem_GT() - binary/UNDEF for if X > Y
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
!!!_   & elem_GE() - X if X >= Y, else NAN
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
!!!_   & elem_ATAN2() - TAN2(X, Y)
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
!!!_   & elem_HYPOT()
  ELEMENTAL &
  real(kind=KBUF) function elem_HYPOT (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else if (ABS(X).gt.ABS(Y)) then
       Z = ABS(X) * SQRT(ONE + (Y / X) ** 2)
    else if (Y.ne.ZERO) then
       Z = ABS(Y) * SQRT(ONE + (X / Y) ** 2)
    else
       Z = ZERO
    endif
  end function elem_HYPOT
!!!_   & elem_SCALE()
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
!!!_   & elem_BITNOT () - (integer only) bitwise not
  ELEMENTAL &
  real(kind=KBUF) function elem_BITNOT (X, F) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X
    real(kind=KBUF),intent(in) :: F
    if (X.eq.F) then
       Z = F
    else
       Z = REAL(NOT(INT(X)), kind=KBUF)
    endif
  end function elem_BITNOT
!!!_   & elem_BITAND()
  ELEMENTAL &
  real(kind=KBUF) function elem_BITAND (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else
       Z = REAL(IAND(INT(X), INT(Y)), kind=KBUF)
    endif
  end function elem_BITAND
!!!_   & elem_BITOR()
  ELEMENTAL &
  real(kind=KBUF) function elem_BITOR (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else
       Z = REAL(IOR(INT(X), INT(Y)), kind=KBUF)
    endif
  end function elem_BITOR
!!!_   & elem_BITXOR()
  ELEMENTAL &
  real(kind=KBUF) function elem_BITXOR (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else
       Z = REAL(IEOR(INT(X), INT(Y)), kind=KBUF)
    endif
  end function elem_BITXOR
!!!_   & elem_LSHIFT()
  ELEMENTAL &
  real(kind=KBUF) function elem_LSHIFT (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else
       Z = REAL(ISHFT(INT(X), INT(Y)), kind=KBUF)
    endif
  end function elem_LSHIFT
!!!_   & elem_RSHIFT()
  ELEMENTAL &
  real(kind=KBUF) function elem_RSHIFT (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else
       Z = REAL(ISHFT(INT(X), -INT(Y)), kind=KBUF)
    endif
  end function elem_RSHIFT
!!!_    * elem_NEAREST()
  ELEMENTAL &
  real(kind=KBUF) function elem_NEAREST (X, Y, FX, FY) result(Z)
    implicit none
    real(kind=KBUF),intent(in) :: X,  Y
    real(kind=KBUF),intent(in) :: FX, FY
    if (X.eq.FX.or.Y.eq.FY) then
       Z = FX
    else
       Z = NEAREST(X, Y)
    endif
  end function elem_NEAREST

!!!_ + end chak_opr
end module chak_opr
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

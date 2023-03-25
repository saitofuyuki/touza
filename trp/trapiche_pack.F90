!!!_! trapiche_pack.F90 - TOUZA/Trapiche integer packing/unpacking
! Maintainer: SAITO Fuyuki
! Created: Feb 26 2021
#define TIME_STAMP 'Time-stamp: <2023/03/25 13:36:25 fuyuki trapiche_pack.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021,2022,2023
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_trp.h"
#ifndef   TEST_TRAPICHE_PACK
#  define TEST_TRAPICHE_PACK 0
#endif
!!!_@ TOUZA_Trp_pack - trapiche packing manager
module TOUZA_Trp_pack
!!!_ = declaration
  use TOUZA_Trp_std,only: KI8, KI32, KI64
  use TOUZA_Trp_std,only: unit_global,  trace_fine,   trace_control
#if OPT_USE_IPC_IBITS
  use TOUZA_Trp_std,only: ipc_IBITS
#endif
  implicit none
  private
!!!_  - public parameters
  ! relleno (filling method)
  integer,parameter,public :: RELLENO_TRANSPOSE  = 0                       ! transposed packing
  integer,parameter,public :: RELLENO_SEQUENTIAL = 1                       ! sequential packing
  integer,parameter,public :: RELLENO_STRIDE     = RELLENO_SEQUENTIAL + 2  ! sequential packing (stride)
  integer,parameter,public :: RELLENO_MANUAL     = 8                       ! manual expansion if possible

!!!_  - dunp - unpack property complex -- dunp(npropd, :)
  integer,parameter,public :: npropd = 4  ! size of dunp first rank
  integer,parameter,public :: p_cbgn = 1  ! unpack offset
  integer,parameter,public :: p_bofs = 2  ! buffer offset
  integer,parameter,public :: p_orgm = 3  ! buffer mask (origin)
  integer,parameter,public :: p_xofs = 4  ! extra buffer offset

!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = TRP_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global
# define __MDL__ 'p'
# define _ERROR(E) (E - ERR_MASK_TRP_PACK)
!!!_  - common
  ! character(len=256) :: tmsg
!!!_  - interfaces
  interface pack_store
     module procedure pack_store_ii, pack_store_ll
  end interface pack_store
  interface pack_restore
     module procedure pack_restore_ii, pack_restore_ll
  end interface pack_restore
  interface pack_restore_slice
     module procedure pack_restore_slice_ii
  end interface pack_restore_slice
  interface pack_restore_dunp
     module procedure pack_restore_dunp_ii
  end interface pack_restore_dunp

  interface pack_store_trn
     module procedure pack_store_trn_ii, pack_store_trn_ll
  end interface pack_store_trn
  interface pack_restore_trn
     module procedure pack_restore_trn_ii, pack_restore_trn_ll
  end interface pack_restore_trn

  interface pack_store_trn_sp1
     module procedure pack_store_trn_sp1_ii, pack_store_trn_sp1_ll
  end interface pack_store_trn_sp1
  interface pack_restore_trn_sp1
     module procedure pack_restore_trn_sp1_ii, pack_restore_trn_sp1_ll
  end interface pack_restore_trn_sp1

  interface pack_store_trn_spdiv
     module procedure pack_store_trn_spdiv_ii, pack_store_trn_spdiv_ll
  end interface pack_store_trn_spdiv
  interface pack_restore_trn_spdiv
     module procedure pack_restore_trn_spdiv_ii, pack_restore_trn_spdiv_ll
  end interface pack_restore_trn_spdiv

  interface pack_store_seq
     module procedure pack_store_seq_ii, pack_store_seq_ll
  end interface pack_store_seq
  interface pack_restore_seq
     module procedure pack_restore_seq_ii, pack_restore_seq_ll
  end interface pack_restore_seq

  interface pack_store_seq_sp1
     module procedure pack_store_seq_sp1_ii, pack_store_seq_sp1_ll
  end interface pack_store_seq_sp1
  interface pack_restore_seq_sp1
     module procedure pack_restore_seq_sp1_ii, pack_restore_seq_sp1_ll
  end interface pack_restore_seq_sp1

  interface pack_store_str
     module procedure pack_store_str_ii, pack_store_str_ll
  end interface pack_store_str
  interface pack_restore_str
     module procedure pack_restore_str_ii, pack_restore_str_ll
  end interface pack_restore_str

  interface pack_store_str_sp1
     module procedure pack_store_str_sp1_ii, pack_store_str_sp1_ll
  end interface pack_store_str_sp1
  interface pack_restore_str_sp1
     module procedure pack_restore_str_sp1_ii, pack_restore_str_sp1_ll
  end interface pack_restore_str_sp1

  interface count_packed
     module procedure count_packed_ii, count_packed_ll
  end interface count_packed

  interface show_packed
     module procedure show_packed_i, show_packed_l
  end interface show_packed

  interface mask_to_idxl
     module procedure mask_to_idxl_i
  end interface mask_to_idxl
  interface mask_to_idxl_seq
     module procedure mask_to_idxl_seq_i
  end interface mask_to_idxl_seq

  interface mask_count_defined
     module procedure mask_count_defined_i
  end interface mask_count_defined

  interface pack_gen_dspl
     module procedure pack_gen_dspl_i
  end interface pack_gen_dspl
  interface pack_gen_dspl_seq
     module procedure pack_gen_dspl_seq_i
  end interface pack_gen_dspl_seq

  interface pack_gen_runl
     module procedure pack_gen_runl_i
  end interface pack_gen_runl
  interface pack_gen_runl_seq
     module procedure pack_gen_runl_seq_i
  end interface pack_gen_runl_seq

  interface gen_bfc_slice
     module procedure gen_bfc_slice_i
  end interface gen_bfc_slice
  interface gen_bfc_slice_seq
     module procedure gen_bfc_slice_seq_i
  end interface gen_bfc_slice_seq

  interface gen_bfc_idxl
     module procedure gen_bfc_idxl_i
  end interface gen_bfc_idxl
  interface gen_bfc_idxl_seq
     module procedure gen_bfc_idxl_seq_i
  end interface gen_bfc_idxl_seq

  interface popcount_tab
     module procedure popcount_tab_i, popcount_tab_l
  end interface popcount_tab
!!!_  - public
  public init, diag, finalize
  public count_packed
  public pack_store,          pack_restore
  public pack_restore_slice,  pack_restore_dunp
#if TEST_TRAPICHE_PACK
  public pack_store_trn,      pack_restore_trn
  public pack_store_trn_sp1,  pack_restore_trn_sp1
  public pack_store_trn_spdiv,pack_restore_trn_spdiv
  public pack_store_seq,      pack_restore_seq
  public pack_store_seq_sp1,  pack_restore_seq_sp1
  public pack_store_str,      pack_restore_str
  public pack_store_str_sp1,  pack_restore_str_sp1
#endif
  public unparse_relleno
  public show_props_trn,    show_packed
  public div_ceiling,       div_ceiling_safe
  public mask_to_idxl,      mask_to_idxl_seq
  public mask_count_defined
  public pack_gen_dspl,     pack_gen_runl,     gen_bfc_slice
  public gen_bfc_idxl
#if TEST_TRAPICHE_PACK
  public pack_gen_dspl_seq, pack_gen_runl_seq, gen_bfc_slice_seq
  public gen_bfc_idxl_seq
#endif
  public popcount_tab
  public set_loop_slice
  public is_in_slice
#if TEST_TRAPICHE_PACK
  public count_zones
#endif
!!!_   . TOUZA_Trp_std
  public KI8, KI32, KI64

!!!_ + common interfaces
contains
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv)
    use TOUZA_Trp_std,only: control_mode, control_deep, is_first_force
    use TOUZA_Trp_std,only: choice, ts_init=>init
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
       if (is_first_force(init_counts, mode)) then
          ulog = choice(ulog, u)
          lev_verbose = lv
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ts_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = _ERROR(ERR_FAILURE_INIT)
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Trp_std,only: control_mode, control_deep, is_first_force
    use TOUZA_Trp_std,only: choice, msg, ts_diag=>diag, is_msglev_normal
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
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ts_diag(ierr, utmp, lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Trp_std,only: control_mode, control_deep, is_first_force
    use TOUZA_Trp_std,only: ts_finalize=>finalize, choice
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
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ts_finalize (ierr, utmp, levv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + user subroutines
!!!_  & pack_trim - trim arrays according to its range
  subroutine pack_trim_i &
       & (ierr, minc, nbits, icanaz, mem, nbskp, kxsp)
    use TOUZA_Trp_std,only: choice, first_bit
    implicit none
    integer,parameter :: KICNZ = KI32
    integer,            intent(out)         :: ierr
    integer(kind=KICNZ),intent(out)         :: minc, nbits
    integer(kind=KICNZ),intent(inout)       :: icanaz(0:*)  ! source buffer
    integer,            intent(in)          :: mem
    integer,            intent(in),optional :: nbskp
    integer(kind=KICNZ),intent(in),optional :: kxsp

    integer,parameter   :: mold  = 0_KICNZ
    integer(kind=KICNZ) :: maxp, minn
    integer(kind=KICNZ) :: kofs
    integer(kind=KICNZ) :: nw, nb, mc
    integer,parameter   :: lbits = BIT_SIZE(mold)
    integer(kind=KICNZ) :: lpos = + HUGE(mold)
    integer(kind=KICNZ) :: lneg = (- HUGE(mold)) - 1

    ierr = err_default
    kofs = max(-1_KICNZ, choice(-1_KICNZ, kxsp)) + 1_KICNZ

    nb   = min(lbits - 1, max(0, choice(lbits, nbskp)))
    mc   = (- (2 ** (nb - 1)) * 2) + kofs     ! - [max counts]

    maxp = MAXVAL(icanaz(0:mem-1))
    minn = MINVAL(icanaz(0:mem-1))
    if (maxp.eq.lneg) then

    else if (maxp.lt.0 .or. minn.ge.kofs) then
       nw = ((- maxp) + minn) - 1   ! - [counts]
    else if (maxp.lt.kofs) then

    else if (minn.lt.kofs) then

    else if ((minn + HUGE(0_KICNZ)).ge.maxp) then
       nw = ((- maxp) + minn) - 1   ! - [counts]
    else
       nw = 1
    endif

    if (nw.lt.0) then
       nbits = first_bit(- (nw + 1))
       if (nw .ge. mc) then         ! counts <= max
          minc = minn
          if (maxp.lt.0) then
             ! v < 0
             icanaz(0:mem-1) = icanaz(0:mem-1) - minc + kofs
          else if (minn.ge.0) then
             ! v > 0
             where (icanaz(0:mem-1).ge.kofs)
                icanaz(0:mem-1) = icanaz(0:mem-1) - minc + kofs
             endwhere
          else
             where (icanaz(0:mem-1).ge.kofs .or. icanaz(0:mem-1).lt.0)
                icanaz(0:mem-1) = icanaz(0:mem-1) - minc + kofs
             endwhere
          endif
       else
          minc = 0
       endif
    else
       nbits = lbits
       minc  = 0
    endif

  end subroutine pack_trim_i

#if 0 /* meta comment */
!!!_  & pack_trim_full - trim arrays according to its range (case if full bits)
  subroutine pack_trim_full_i &
       & (ierr, minc, nbits, icanaz, mem, nbskp, kxsp)
    use TOUZA_Trp_std,only: choice, first_bit
    implicit none
    integer,parameter :: KICNZ = KI32
    integer,            intent(out)   :: ierr
    integer(kind=KICNZ),intent(out)   :: minc, nbits
    integer(kind=KICNZ),intent(inout) :: icanaz(0:*)  ! source buffer
    integer,            intent(in)    :: mem
    integer,            intent(in)    :: nbskp
    integer(kind=KICNZ),intent(in)    :: kxsp

    integer(kind=KICNZ) :: maxp, minp, maxn, minn
    integer(kind=KICNZ) :: nc, nb
    integer(kind=KICNZ) :: ntgt
    integer(kind=KICNZ) :: kofs
    integer,parameter   :: mold  = 0_KICNZ
    integer,parameter   :: lbits = BIT_SIZE(mold)
    integer(kind=KICNZ) :: lpos = + HUGE(mold)
    integer(kind=KICNZ) :: lneg = - HUGE(mold) - 1

    ierr = 0

    minc  = 0
    nbits = lbits

    kofs = max(0, 1 + kxsp)
    nb   = min(lbits - 1, max(0, nbskp))
    ntgt = (- (2 ** (nb - 1)) * 2) + kofs ! negative counts, to allow 1+HUGE()

    minn = MINVAL(icanaz(0:mem-1))
    maxp = MAXVAL(icanaz(0:mem-1))
    if (maxp.le.kxsp) then
       ! negative AND special
       if (maxp.ge.0) then
          maxn = MAXVAL(icanaz(0:mem-1), icanaz(0:mem-1).lt.0)
       else
          maxn = maxp
       endif
       ! - (maxn - minn + 1)
       nc = minn - (maxn + 1)   ! negative counts
       ! -nc <= -ntgt
       if (nc.ge.ntgt) minc = minn
    else
       ! negative to positive
       if ((minn + lpos).ge.maxp) then
          nw = ((- maxp) + minn) - 1   ! - [counts]
       endif
    endif
    if (minc.eq.lneg) then
       where (icanaz(0:mem-1).ge.kofs .or. icanaz(0:mem-1).lt.0)
          ! icanaz(0:mem-1) = (icanaz(0:mem-1) - (llim+1)) + (kofs + 1)
          ! a-llim == a +(Huge+1) may not give correct answer
          icanaz(0:mem-1) = IBCLR(icanaz(0:mem-1), lbits-1) + kofs
       endwhere
    else if (minc.ne.0) then
       where (icanaz(0:mem-1).ge.kofs .or. icanaz(0:mem-1).lt.0)
          icanaz(0:mem-1) = (icanaz(0:mem-1) - minc) + kofs
       endwhere
    endif

  end subroutine pack_trim_full_i
#endif /* meta comment */

!!!_  & pack_store - pack_store dispatcher
  subroutine pack_store_ii &
       & (ierr,   ibagaz, &
       &  icanaz, mem,   nbits,  kpack)
    implicit none
    integer,parameter :: KIBGZ = KI32, KICNZ = KI32

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)  ! destination buffer
    integer(kind=KICNZ),intent(in)  :: icanaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes
    integer,            intent(in)  :: kpack        ! packing method
    integer,parameter :: lbits = bit_size(ibagaz(0))
    integer kp

    ierr = err_default
    if (nbits.eq.0) return
    kp = pack_method(+nbits, mem, kpack)
    select case (kp)
    case (RELLENO_SEQUENTIAL)
       if (is_enabled_manual(nbits, mem, kpack)) then
          if (nbits.eq.1) then
             call pack_store_seq_sp1_ii(ierr, ibagaz, icanaz, mem, nbits)
          else
             call pack_store_seq_ii(ierr, ibagaz, icanaz, mem, nbits)
          endif
       else
          call pack_store_seq_ii(ierr, ibagaz, icanaz, mem, nbits)
       endif
    case (RELLENO_STRIDE)
       if (is_enabled_manual(nbits, mem, kpack)) then
          if (nbits.eq.1) then
             call pack_store_str_sp1_ii(ierr, ibagaz, icanaz, mem, nbits)
          else
             call pack_store_str_ii(ierr, ibagaz, icanaz, mem, nbits)
          endif
       else
          call pack_store_str_ii(ierr, ibagaz, icanaz, mem, nbits)
       endif
    case (RELLENO_TRANSPOSE)
       if (is_enabled_manual(nbits, mem, kpack)) then
          if (nbits.eq.1) then
             call pack_store_trn_sp1_ii(ierr, ibagaz, icanaz, mem, nbits)
          else if (mod(lbits, nbits).eq.0) then
             call pack_store_trn_spdiv_ii(ierr, ibagaz, icanaz, mem, nbits)
          else
             call pack_store_trn_ii(ierr, ibagaz, icanaz, mem, nbits)
          endif
       else
          call pack_store_trn_ii(ierr, ibagaz, icanaz, mem, nbits)
       endif
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    return
  end subroutine pack_store_ii

  subroutine pack_store_ll &
       & (ierr,   ibagaz, &
       &  icanaz, mem,   nbits,  kpack)
    implicit none
    integer,parameter :: KIBGZ = KI64, KICNZ = KI64

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)  ! destination buffer
    integer(kind=KICNZ),intent(in)  :: icanaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes
    integer,            intent(in)  :: kpack        ! packing method
    integer kp

    ierr = err_default
    if (nbits.eq.0) return
    kp = pack_method(+nbits, mem, kpack)
    select case (kp)
    case (RELLENO_SEQUENTIAL)
       call pack_store_seq_ll(ierr, ibagaz, icanaz, mem, nbits)
    case (RELLENO_STRIDE)
       call pack_store_str_ll(ierr, ibagaz, icanaz, mem, nbits)
    case (RELLENO_TRANSPOSE)
       call pack_store_trn_ll(ierr, ibagaz, icanaz, mem, nbits)
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    return
  end subroutine pack_store_ll

!!!_  & pack_restore_slice - pack_restore wrapper (slice by runlength)
  subroutine pack_restore_slice_ii &
       & (ierr,   icanaz, &
       &  ibagaz, mem,    nbits, kpack, &
       &  lofs,   rsrc,   nfil,  bes,   nr)
    use TOUZA_Trp_std,only: choice
    implicit none
    integer,parameter :: KIBGZ = KI32, KICNZ = KI32
    integer,            intent(out) :: ierr
    integer(kind=KICNZ),intent(out) :: icanaz(0:*)  ! destination buffer
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes
    integer,            intent(in)  :: kpack        ! packing method
    integer,            intent(in)  :: lofs(2, 0:*)
    integer,            intent(in)  :: rsrc(0:*)
    integer,            intent(in)  :: nfil
    integer,            intent(in)  :: bes(3, 0:*)
    integer,            intent(in)  :: nr

    integer next(0:nr), span(0:nr-1), gstep(0:nr)
    integer idx(0:nr-1)
    integer jgb
    integer jr, nrc
    integer icont, inext
    integer jf
    integer jb,    mb
    integer jcbgn, jcend
    integer corg,  cskp

    ierr = err_default
    call set_loop_slice(jgb, gstep, next, span, bes, nr)
    nrc = 0
    do jr = 0, nr - 1
       nrc = nrc + 1
       if (next(jr).gt.0) exit
    enddo
    icont = product(span(0:nrc-1))
    inext = sum(next(0:nrc-1)) + icont
    idx(0:nr-1) = 0
    jb = 0
    do jf = 0, nfil / 2 - 1
       mb     = rsrc(2 * jf + 1)
       jcbgn  = lofs(1, jf)
       jcend  = lofs(1, jf + 1)
       corg   = lofs(2, jf)
       cskp   = 0
       do
          if (jcbgn.ge.jcend) exit
          call pack_restore_ii(ierr, icanaz(jcbgn), ibagaz(jb), icont, nbits, kpack, corg, cskp)
          jcbgn = jcbgn + icont
          cskp  = cskp  + inext
          do jr = nrc, nr - 1
             idx(jr) = idx(jr) + 1
             if (idx(jr).lt.span(jr)) exit
             cskp = cskp + next(jr)
             idx(jr) = 0
          enddo
       enddo
       jb = jb + mb
    enddo
  end subroutine pack_restore_slice_ii

!!!_  & pack_restore_dunp - pack_restore wrapper (displacement+length list)
  subroutine pack_restore_dunp_ii &
       & (ierr,   icanaz, &
       &  ibagaz, mem,    nbits, kpack, &
       &  dunp,   nunp)
    use TOUZA_Trp_std,only: choice
    implicit none
    integer,parameter :: KIBGZ = KI32, KICNZ = KI32
    integer,            intent(out) :: ierr
    integer(kind=KICNZ),intent(out) :: icanaz(0:*)  ! destination buffer
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes
    integer,            intent(in)  :: kpack        ! packing method
    integer,            intent(in)  :: dunp(npropd, 0:*)
    integer,            intent(in)  :: nunp

    integer ju
    integer jbofs
    integer jcbgn, ncont
    integer corg,  cskp

    ierr = err_default
    cskp = 0
    do ju = 0, nunp - 1
       jcbgn = dunp(p_cbgn, ju)
       ncont = dunp(p_cbgn, ju + 1) - jcbgn
       jbofs = dunp(p_bofs, ju)
       corg  = dunp(p_orgm, ju)
       call pack_restore_ii(ierr, icanaz(jcbgn), ibagaz(jbofs), ncont, nbits, kpack, corg, cskp)
    enddo
  end subroutine pack_restore_dunp_ii

!!!_  & pack_restore - pack_restore dispatcher
  subroutine pack_restore_ii &
       & (ierr,   icanaz, &
       &  ibagaz, mem,    nbits, kpack, org, skip)
    use TOUZA_Trp_std,only: choice
    implicit none
    integer,parameter :: KIBGZ = KI32, KICNZ = KI32

    integer,            intent(out) :: ierr
    integer(kind=KICNZ),intent(out) :: icanaz(0:*)  ! destination buffer
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes
    integer,            intent(in)  :: kpack        ! packing method
    integer,optional,   intent(in)  :: org          ! origin of source buffer
    integer,optional,   intent(in)  :: skip         ! cue size at source
    integer kp
    integer,parameter :: lbits = bit_size(ibagaz(0))
    integer xorg, xskip
    integer cofs, bofs
    ierr = err_default
    if (nbits.eq.0) then
       icanaz(0:mem-1) = 0
       return
    endif
    xskip = choice(0, skip)
    if (lbits.eq.nbits) then
       icanaz(0:mem-1) = ibagaz(xskip:xskip+mem-1)
       return
    endif
    xorg = choice(0, org)

    kp = pack_method(-nbits, mem, kpack)
    select case (kp)
    case (RELLENO_SEQUENTIAL)
       if (ierr.eq.0) then
          call pack_restore_seq_head_ii &
               & (ierr,   cofs,  bofs,  icanaz, &
               &  ibagaz, mem,   nbits, xorg,   xskip)
       endif
       if (ierr.eq.0) then
          if (is_enabled_manual(nbits, mem, kpack)) then
             if (nbits.eq.1) then
                call pack_restore_seq_sp1_ii(ierr, icanaz(cofs), ibagaz(bofs), mem - cofs, nbits)
             else
                call pack_restore_seq_ii(ierr, icanaz(cofs), ibagaz(bofs), mem - cofs, nbits)
             endif
          else
             call pack_restore_seq_ii(ierr, icanaz(cofs), ibagaz(bofs), mem - cofs, nbits)
          endif
       endif
    case (RELLENO_STRIDE)
       if (ierr.eq.0) then
          call pack_restore_seq_head_ii &
               & (ierr,   cofs,  bofs,  icanaz, &
               &  ibagaz, mem,   nbits, xorg,   xskip)
       endif
       if (ierr.eq.0) then
          if (is_enabled_manual(nbits, mem, kpack)) then
             if (nbits.eq.1) then
                call pack_restore_str_sp1_ii(ierr, icanaz(cofs), ibagaz(bofs), mem - cofs, nbits)
             else
                call pack_restore_str_ii(ierr, icanaz(cofs), ibagaz(bofs), mem - cofs, nbits)
             endif
          else
             call pack_restore_str_ii(ierr, icanaz(cofs), ibagaz(bofs), mem - cofs, nbits)
          endif
       endif
    case (RELLENO_TRANSPOSE)
       if (present(org)) then
          ierr = _ERROR(ERR_NOT_IMPLEMENTED)
          return
       endif
       if (is_enabled_manual(nbits, mem, kpack)) then
          if (nbits.eq.1) then
             call pack_restore_trn_sp1_ii(ierr, icanaz, ibagaz, mem, nbits)
          else if (mod(lbits, nbits).eq.0) then
             call pack_restore_trn_spdiv_ii(ierr, icanaz, ibagaz, mem, nbits)
          else
             call pack_restore_trn_ii(ierr, icanaz, ibagaz, mem, nbits)
          endif
       else
          call pack_restore_trn_ii(ierr, icanaz, ibagaz, mem, nbits)
       endif
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    return
  end subroutine pack_restore_ii

  subroutine pack_restore_ll &
       & (ierr,   icanaz, &
       &  ibagaz, mem,   nbits, kpack)
    implicit none
    integer,parameter :: KIBGZ = KI64, KICNZ = KI64

    integer,            intent(out) :: ierr
    integer(kind=KICNZ),intent(out) :: icanaz(0:*)  ! destination buffer
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes
    integer,            intent(in)  :: kpack        ! packing method
    integer kp

    ierr = err_default
    if (nbits.eq.0) then
       icanaz(0:mem-1) = 0
       return
    endif
    kp = pack_method(-nbits, mem, kpack)
    select case (kp)
    case (RELLENO_SEQUENTIAL)
       call pack_restore_seq_ll(ierr, icanaz, ibagaz, mem, nbits)
    case (RELLENO_STRIDE)
       call pack_restore_str_ll(ierr, icanaz, ibagaz, mem, nbits)
    case (RELLENO_TRANSPOSE)
       call pack_restore_trn_ll(ierr, icanaz, ibagaz, mem, nbits)
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
    return
  end subroutine pack_restore_ll

!!!_  & pack_restore_seq_head - special treatment on first some members
  subroutine pack_restore_seq_head_ii &
       & (ierr,   cofs,  bofs,  icanaz, &
       &  ibagaz, mem,   nbits, org,    skip)
    use TOUZA_Trp_std,only: choice
    implicit none
    integer,parameter :: KIBGZ = KI32, KICNZ = KI32

    integer,            intent(out) :: ierr
    integer,            intent(out) :: cofs, bofs   ! canaz/bagaz offsets for remnant
    integer(kind=KICNZ),intent(out) :: icanaz(0:*)  ! destination buffer
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes
    integer,            intent(in)  :: org          ! origin of source buffer
    integer,            intent(in)  :: skip         ! cue size at source
    integer,parameter :: lbits = bit_size(ibagaz(0))

    integer nz
    integer mskb, mske, msh
    integer jc,   mc
    integer corg, cskp

    ierr = err_default

    nz = count_zones(nbits, lbits)
    mc = (nz * lbits) / nbits   ! number of canaz in nz zones
    corg = mod(org, mc)
    cskp = skip + corg

    ! special treatment for the first line
    cofs = mod(cskp, mc)
    bofs = safe_div(cskp, nbits, lbits) -  safe_div(corg, nbits, lbits)
    if (cofs.gt.0) then
       mskb = mod(cofs * nbits, lbits)
       cofs = min(mem, mc - cofs)
       do jc = 0, cofs - 1
          msh = mskb + nbits
          mske = min(lbits, msh)
          icanaz(jc) = &
               & _IBITS(ibagaz(bofs), lbits - mske, mske - mskb)
          if (msh.gt.lbits) then
             bofs = bofs + 1
             mskb = msh - lbits
             icanaz(jc) = &
                  & IOR(ISHFT(icanaz(jc), mskb), &
                  &     _IBITS(ibagaz(bofs), lbits - mskb, mskb))
          else
             mskb = mske
          endif
       enddo
       bofs = bofs + 1
    endif
  end subroutine pack_restore_seq_head_ii
!!!_  & pack_store_seq - sequential packing
  subroutine pack_store_seq_ii &
       & (ierr,   ibagaz, &
       &  icanaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI32, KICNZ = KI32

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)  ! destination buffer
    integer(kind=KICNZ),intent(in)  :: icanaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))
    integer jz,    nz
    integer jbbgn, jbend, jb, nb  ! range of bagazo (destination) elements
    integer jline, nl
    integer mskb
    integer jcbgn, jcend, jc
    integer msh

    ierr = err_default

    if (lbits.eq.nbits) then
       ibagaz(0:mem-1) = icanaz(0:mem-1)
       return
    else if (nbits.eq.0) then
       return
    endif

    nz = count_zones(nbits, lbits)
    nb = count_packed(nbits, mem, ibagaz(0))

    nl = div_ceiling(nb, nz)
    do jline = 0, nl - 1
       jbbgn = jline * nz
       jbend = min(jbbgn + nz, nb)
       do jb = jbbgn, jbend - 1
          jz = jb - jbbgn
          mskb = initial_mask(jz, nbits, lbits)
          jcbgn = jb * lbits / nbits
          jcend = min(mem, (jb + 1) * lbits / nbits)
          do jc = jcbgn,     jcbgn
             msh = mskb + (jc - jcbgn) * nbits
             ibagaz(jb) = &
                  & ISHFT(_IBITS(icanaz(jc), 0, msh), lbits - msh)
          enddo
          do jc = jcbgn + 1, jcend - 1
             msh = mskb + (jc - jcbgn) * nbits
             ibagaz(jb) = &
                  & IOR(ibagaz(jb), ISHFT(_IBITS(icanaz(jc), 0, nbits), lbits - msh))
          enddo
          do jc = jcend, min(mem - 1, jcend)
             msh = mskb + (jc - jcbgn) * nbits
             msh = msh - lbits
             ibagaz(jb) = &
                  & IOR(ibagaz(jb), _IBITS(icanaz(jc), msh, nbits - msh))
          enddo
       enddo
    enddo

    return
  end subroutine pack_store_seq_ii

  subroutine pack_store_seq_ll &
       & (ierr,   ibagaz, &
       &  icanaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI64, KICNZ = KI64

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)  ! destination buffer
    integer(kind=KICNZ),intent(in)  :: icanaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))
    integer jz,    nz
    integer jbbgn, jbend, jb, nb  ! range of bagazo (destination) elements
    integer jline, nl
    integer mskb
    integer jcbgn, jcend, jc
    integer msh

    ierr = err_default

    if (lbits.eq.nbits) then
       ibagaz(0:mem-1) = icanaz(0:mem-1)
       return
    else if (nbits.eq.0) then
       return
    endif

    nz = count_zones(nbits, lbits)
    nb = count_packed(nbits, mem, ibagaz(0))

    nl = div_ceiling(nb, nz)
    do jline = 0, nl - 1
       jbbgn = jline * nz
       jbend = min(jbbgn + nz, nb)
       do jb = jbbgn, jbend - 1
          jz = jb - jbbgn
          mskb = initial_mask(jz, nbits, lbits)
          jcbgn = jb * lbits / nbits
          jcend = min(mem, (jb + 1) * lbits / nbits)
          do jc = jcbgn,     jcbgn
             msh = mskb + (jc - jcbgn) * nbits
             ibagaz(jb) = &
                  & ISHFT(_IBITS(icanaz(jc), 0, msh), lbits - msh)
          enddo
          do jc = jcbgn + 1, jcend - 1
             msh = mskb + (jc - jcbgn) * nbits
             ibagaz(jb) = &
                  & IOR(ibagaz(jb), ISHFT(_IBITS(icanaz(jc), 0, nbits), lbits - msh))
          enddo
          do jc = jcend, min(mem - 1, jcend)
             msh = mskb + (jc - jcbgn) * nbits
             msh = msh - lbits
             ibagaz(jb) = &
                  & IOR(ibagaz(jb), _IBITS(icanaz(jc), msh, nbits - msh))
          enddo
       enddo
    enddo

    return
  end subroutine pack_store_seq_ll

!!!_  & pack_restore_seq - sequential unpacking
  subroutine pack_restore_seq_ii &
       & (ierr,   icanaz, &
       &  ibagaz, mem,    nbits)
    use TOUZA_Trp_std,only: choice
    implicit none
    integer,parameter :: KIBGZ = KI32, KICNZ = KI32

    integer,            intent(out) :: ierr
    integer(kind=KICNZ),intent(out) :: icanaz(0:*)  ! destination buffer
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))

    integer jz,    nz
    integer jbbgn, jbend, jb, nb  ! range of bagazo (destination) elements
    integer jline, nl
    integer mskb
    integer jcbgn, jcend, jc
    integer msh

    ierr = err_default

    if (lbits.eq.nbits) then
       icanaz(0:mem-1) = ibagaz(0:mem-1)
       return
    else if (nbits.eq.0) then
       icanaz(0:mem-1) = 0
       return
    endif

    nz = count_zones(nbits, lbits)
    nb = count_packed(nbits, mem, ibagaz(0))

    nl = div_ceiling(nb, nz)
    do jline = 0, nl - 1
       jbbgn = jline * nz
       jbend = min(jbbgn + nz, nb)
       jc = jbbgn * lbits / nbits
       icanaz(jc) = 0
       do jb = jbbgn, jbend - 1
          jz = jb - jbbgn
          mskb = initial_mask(jz, nbits, lbits)
          jcbgn = jb * lbits / nbits
          jcend = min(mem, (jb + 1) * lbits / nbits)
          do jc = jcbgn,     jcbgn
             msh = mskb + (jc - jcbgn) * nbits
             icanaz(jc) = &
                  & IOR(icanaz(jc), &
                  &     _IBITS(ibagaz(jb), lbits - msh, msh))
          enddo
          do jc = jcbgn + 1, jcend - 1
             msh = mskb + (jc - jcbgn) * nbits
             icanaz(jc) = _IBITS(ibagaz(jb), lbits - msh, nbits)
          enddo
          do jc = jcend, min(mem - 1, jcend)
             msh = mskb + (jc - jcbgn) * nbits
             msh = msh - lbits
             icanaz(jc) = &
                  & ISHFT(_IBITS(ibagaz(jb), 0, nbits - msh), msh)
          enddo
       enddo
    enddo

  end subroutine pack_restore_seq_ii

  subroutine pack_restore_seq_ll &
       & (ierr,   icanaz, &
       &  ibagaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI64, KICNZ = KI64

    integer,            intent(out) :: ierr
    integer(kind=KICNZ),intent(out) :: icanaz(0:*)  ! destination buffer
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))

    integer jz,    nz
    integer jbbgn, jbend, jb, nb  ! range of bagazo (destination) elements
    integer jline, nl
    integer mskb
    integer jcbgn, jcend, jc
    integer msh

    ierr = err_default

    if (lbits.eq.nbits) then
       icanaz(0:mem-1) = ibagaz(0:mem-1)
       return
    else if (nbits.eq.0) then
       icanaz(0:mem-1) = 0
       return
    endif

    nz = count_zones(nbits, lbits)
    nb = count_packed(nbits, mem, ibagaz(0))

    nl = div_ceiling(nb, nz)
    do jline = 0, nl - 1
       jbbgn = jline * nz
       jbend = min(jbbgn + nz, nb)
       jc = jbbgn * lbits / nbits
       icanaz(jc) = 0
       do jb = jbbgn, jbend - 1
          jz = jb - jbbgn
          mskb = initial_mask(jz, nbits, lbits)
          jcbgn = jb * lbits / nbits
          jcend = min(mem, (jb + 1) * lbits / nbits)
          do jc = jcbgn,     jcbgn
             msh = mskb + (jc - jcbgn) * nbits
             icanaz(jc) = &
                  & IOR(icanaz(jc), &
                  &     _IBITS(ibagaz(jb), lbits - msh, msh))
          enddo
          do jc = jcbgn + 1, jcend - 1
             msh = mskb + (jc - jcbgn) * nbits
             icanaz(jc) = _IBITS(ibagaz(jb), lbits - msh, nbits)
          enddo
          do jc = jcend, min(mem - 1, jcend)
             msh = mskb + (jc - jcbgn) * nbits
             msh = msh - lbits
             icanaz(jc) = &
                  & ISHFT(_IBITS(ibagaz(jb), 0, nbits - msh), msh)
          enddo
       enddo
    enddo

  end subroutine pack_restore_seq_ll

!!!_  & pack_store_seq_sp1 - sequential packing special (1/32)
  subroutine pack_store_seq_sp1_ii &
       & (ierr,   ibagaz, &
       &  icanaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI32, KICNZ = KI32

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)  ! destination buffer
    integer(kind=KICNZ),intent(in)  :: icanaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))
    integer jb, jc, jr, nr

    ierr = err_default
    if (nbits.ne.1) then
       ierr = _ERROR(ERR_INVALID_PARAMETER)
       return
    endif

    do jb = 0, mem / lbits - 1
       jc = jb * lbits
       ibagaz(jb) = &
            & IOR(ISHFT(IBITS(icanaz(jc+0),0,1), 31), &
            &     +ISHFT(IBITS(icanaz(jc+1),0,1), 30)  &
            &     +ISHFT(IBITS(icanaz(jc+2),0,1), 29)  &
            &     +ISHFT(IBITS(icanaz(jc+3),0,1), 28)  &
            &     +ISHFT(IBITS(icanaz(jc+4),0,1), 27)  &
            &     +ISHFT(IBITS(icanaz(jc+5),0,1), 26)  &
            &     +ISHFT(IBITS(icanaz(jc+6),0,1), 25)  &
            &     +ISHFT(IBITS(icanaz(jc+7),0,1), 24)  &
            &     +ISHFT(IBITS(icanaz(jc+8),0,1), 23)  &
            &     +ISHFT(IBITS(icanaz(jc+9),0,1), 22)  &
            &     +ISHFT(IBITS(icanaz(jc+10),0,1), 21)  &
            &     +ISHFT(IBITS(icanaz(jc+11),0,1), 20)  &
            &     +ISHFT(IBITS(icanaz(jc+12),0,1), 19)  &
            &     +ISHFT(IBITS(icanaz(jc+13),0,1), 18)  &
            &     +ISHFT(IBITS(icanaz(jc+14),0,1), 17)  &
            &     +ISHFT(IBITS(icanaz(jc+15),0,1), 16)  &
            &     +ISHFT(IBITS(icanaz(jc+16),0,1), 15)  &
            &     +ISHFT(IBITS(icanaz(jc+17),0,1), 14)  &
            &     +ISHFT(IBITS(icanaz(jc+18),0,1), 13)  &
            &     +ISHFT(IBITS(icanaz(jc+19),0,1), 12)  &
            &     +ISHFT(IBITS(icanaz(jc+20),0,1), 11)  &
            &     +ISHFT(IBITS(icanaz(jc+21),0,1), 10)  &
            &     +ISHFT(IBITS(icanaz(jc+22),0,1), 9)  &
            &     +ISHFT(IBITS(icanaz(jc+23),0,1), 8)  &
            &     +ISHFT(IBITS(icanaz(jc+24),0,1), 7)  &
            &     +ISHFT(IBITS(icanaz(jc+25),0,1), 6)  &
            &     +ISHFT(IBITS(icanaz(jc+26),0,1), 5)  &
            &     +ISHFT(IBITS(icanaz(jc+27),0,1), 4)  &
            &     +ISHFT(IBITS(icanaz(jc+28),0,1), 3)  &
            &     +ISHFT(IBITS(icanaz(jc+29),0,1), 2)  &
            &     +ISHFT(IBITS(icanaz(jc+30),0,1), 1)  &
            &     +ISHFT(IBITS(icanaz(jc+31),0,1), 0))
    enddo
    jb = mem / lbits
    jc = jb * lbits
    nr = mem - jc
    if (nr.gt.0) then
       ibagaz(jb) = ISHFT(IBITS(icanaz(jc+0),0,1), lbits-0-1)
       do jr = 1, nr - 1
          ibagaz(jb) = IOR(ibagaz(jb), ISHFT(IBITS(icanaz(jc+jr),0,1), lbits-jr-1))
       enddo
    endif
    return
  end subroutine pack_store_seq_sp1_ii

  subroutine pack_store_seq_sp1_ll &
       & (ierr,   ibagaz, &
       &  icanaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI64, KICNZ = KI64

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)  ! destination buffer
    integer(kind=KICNZ),intent(in)  :: icanaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))

    ierr = _ERROR(ERR_NOT_IMPLEMENTED)

    return
  end subroutine pack_store_seq_sp1_ll

!!!_  & pack_restore_seq - sequential unpacking special (1/32)
  subroutine pack_restore_seq_sp1_ii &
       & (ierr,   icanaz, &
       &  ibagaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI32, KICNZ = KI32

    integer,            intent(out) :: ierr
    integer(kind=KICNZ),intent(out) :: icanaz(0:*)  ! destination buffer
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))

    integer jb, jc, jr, nr

    ierr = err_default
    if (nbits.ne.1) then
       ierr = _ERROR(ERR_INVALID_PARAMETER)
       return
    endif

    do jb = 0, mem / lbits - 1
       jc = jb * lbits
       icanaz(jc+0)  = IBITS(ibagaz(jb), 31, 1)
       icanaz(jc+1)  = IBITS(ibagaz(jb), 30, 1)
       icanaz(jc+2)  = IBITS(ibagaz(jb), 29, 1)
       icanaz(jc+3)  = IBITS(ibagaz(jb), 28, 1)
       icanaz(jc+4)  = IBITS(ibagaz(jb), 27, 1)
       icanaz(jc+5)  = IBITS(ibagaz(jb), 26, 1)
       icanaz(jc+6)  = IBITS(ibagaz(jb), 25, 1)
       icanaz(jc+7)  = IBITS(ibagaz(jb), 24, 1)
       icanaz(jc+8)  = IBITS(ibagaz(jb), 23, 1)
       icanaz(jc+9)  = IBITS(ibagaz(jb), 22, 1)
       icanaz(jc+10) = IBITS(ibagaz(jb), 21, 1)
       icanaz(jc+11) = IBITS(ibagaz(jb), 20, 1)
       icanaz(jc+12) = IBITS(ibagaz(jb), 19, 1)
       icanaz(jc+13) = IBITS(ibagaz(jb), 18, 1)
       icanaz(jc+14) = IBITS(ibagaz(jb), 17, 1)
       icanaz(jc+15) = IBITS(ibagaz(jb), 16, 1)
       icanaz(jc+16) = IBITS(ibagaz(jb), 15, 1)
       icanaz(jc+17) = IBITS(ibagaz(jb), 14, 1)
       icanaz(jc+18) = IBITS(ibagaz(jb), 13, 1)
       icanaz(jc+19) = IBITS(ibagaz(jb), 12, 1)
       icanaz(jc+20) = IBITS(ibagaz(jb), 11, 1)
       icanaz(jc+21) = IBITS(ibagaz(jb), 10, 1)
       icanaz(jc+22) = IBITS(ibagaz(jb), +9, 1)
       icanaz(jc+23) = IBITS(ibagaz(jb), +8, 1)
       icanaz(jc+24) = IBITS(ibagaz(jb), +7, 1)
       icanaz(jc+25) = IBITS(ibagaz(jb), +6, 1)
       icanaz(jc+26) = IBITS(ibagaz(jb), +5, 1)
       icanaz(jc+27) = IBITS(ibagaz(jb), +4, 1)
       icanaz(jc+28) = IBITS(ibagaz(jb), +3, 1)
       icanaz(jc+29) = IBITS(ibagaz(jb), +2, 1)
       icanaz(jc+30) = IBITS(ibagaz(jb), +1, 1)
       icanaz(jc+31) = IBITS(ibagaz(jb), +0, 1)
    enddo
    jb = mem / lbits
    jc = jb * lbits
    nr = mem - jc
    if (nr.gt.0) then
       do jr = 0, nr - 1
          icanaz(jc+jr) = IBITS(ibagaz(jb), lbits-jr-1, 1)
       enddo
    endif

  end subroutine pack_restore_seq_sp1_ii

  subroutine pack_restore_seq_sp1_ll &
       & (ierr,   icanaz, &
       &  ibagaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI64, KICNZ = KI64

    integer,            intent(out) :: ierr
    integer(kind=KICNZ),intent(out) :: icanaz(0:*)  ! destination buffer
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))

    ierr = _ERROR(ERR_NOT_IMPLEMENTED)

  end subroutine pack_restore_seq_sp1_ll

!!!_  & pack_store_str - sequential packing (strides)
  subroutine pack_store_str_ii &
       & (ierr,   ibagaz, &
       &  icanaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI32, KICNZ = KI32

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)  ! destination buffer
    integer(kind=KICNZ),intent(in)  :: icanaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))
    integer jz, nz
    integer jb, nb  ! range of bagazo (destination) elements
    integer mskb
    integer jcbgn, jcend, jc, ncs
    integer moff,  mc
    integer msh

    ierr = err_default

    if (lbits.eq.nbits) then
       ibagaz(0:mem-1) = icanaz(0:mem-1)
       return
    else if (nbits.eq.0) then
       return
    endif

    nz = count_zones(nbits, lbits)
    nb = count_packed(nbits, mem, ibagaz(0))
    ncs = (nz * lbits) / nbits
    moff = mem - 1 + ncs

    do jz = 0, nz - 1
       jb = jz
       mskb = initial_mask(jz, nbits, lbits)
       jcbgn = jb * lbits / nbits
       jcend = min(mem, (jb + 1) * lbits / nbits)
       do jc = jcbgn,     jcbgn
          msh = mskb + (jc - jcbgn) * nbits
          ibagaz(jb:nb-1:nz) = &
               & ISHFT(_IBITS(icanaz(jc:mem-1:ncs), 0, msh), lbits - msh)
       enddo
       do jc = jcbgn + 1, jcend - 1
          mc = (moff - jc) / ncs
          msh = mskb + (jc - jcbgn) * nbits
          ibagaz(jb:jb+mc*nz-1:nz) = &
               & IOR(ibagaz(jb:jb+mc*nz-1:nz), ISHFT(_IBITS(icanaz(jc:jc+mc*ncs-1:ncs), 0, nbits), lbits - msh))
       enddo
       do jc = jcend, min(mem - 1, jcend)
          msh = mskb + (jc - jcbgn) * nbits
          msh = msh - lbits
          mc = (moff - jc) / ncs
          ibagaz(jb:jb+mc*nz-1:nz) = &
               & IOR(ibagaz(jb:jb+mc*nz-1:nz), _IBITS(icanaz(jc:jc+mc*ncs-1:ncs), msh, nbits - msh))
       enddo
    enddo

    return
  end subroutine pack_store_str_ii

  subroutine pack_store_str_ll &
       & (ierr,   ibagaz, &
       &  icanaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI64, KICNZ = KI64

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)  ! destination buffer
    integer(kind=KICNZ),intent(in)  :: icanaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))
    integer jz, nz
    integer jb, nb  ! range of bagazo (destination) elements
    integer mskb
    integer jcbgn, jcend, jc, ncs
    integer moff,  mc
    integer msh

    ierr = err_default

    if (lbits.eq.nbits) then
       ibagaz(0:mem-1) = icanaz(0:mem-1)
       return
    else if (nbits.eq.0) then
       return
    endif

    nz = count_zones(nbits, lbits)
    nb = count_packed(nbits, mem, ibagaz(0))
    ncs = (nz * lbits) / nbits
    moff = mem - 1 + ncs

    do jz = 0, nz - 1
       jb = jz
       mskb = initial_mask(jz, nbits, lbits)
       jcbgn = jb * lbits / nbits
       jcend = min(mem, (jb + 1) * lbits / nbits)
       do jc = jcbgn,     jcbgn
          msh = mskb + (jc - jcbgn) * nbits
          ibagaz(jb:nb-1:nz) = &
               & ISHFT(_IBITS(icanaz(jc:mem-1:ncs), 0, msh), lbits - msh)
       enddo
       do jc = jcbgn + 1, jcend - 1
          mc = (moff - jc) / ncs
          msh = mskb + (jc - jcbgn) * nbits
          ibagaz(jb:jb+mc*nz-1:nz) = &
               & IOR(ibagaz(jb:jb+mc*nz-1:nz), ISHFT(_IBITS(icanaz(jc:jc+mc*ncs-1:ncs), 0, nbits), lbits - msh))
       enddo
       do jc = jcend, min(mem - 1, jcend)
          msh = mskb + (jc - jcbgn) * nbits
          msh = msh - lbits
          mc = (moff - jc) / ncs
          ibagaz(jb:jb+mc*nz-1:nz) = &
               & IOR(ibagaz(jb:jb+mc*nz-1:nz), _IBITS(icanaz(jc:jc+mc*ncs-1:ncs), msh, nbits - msh))
       enddo
    enddo

    return
  end subroutine pack_store_str_ll

!!!_  & pack_restore_str - sequential unpacking (strides)
  subroutine pack_restore_str_ii &
       & (ierr,   icanaz, &
       &  ibagaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI32, KICNZ = KI32

    integer,            intent(out) :: ierr
    integer(kind=KICNZ),intent(out) :: icanaz(0:*)  ! destination buffer
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))
    integer jz, nz
    integer jb, nb  ! range of bagazo (destination) elements
    integer mskb
    integer jcbgn, jcend, jc, ncs
    integer moff,  mc
    integer msh

    ierr = err_default

    if (lbits.eq.nbits) then
       icanaz(0:mem-1) = ibagaz(0:mem-1)
       return
    else if (nbits.eq.0) then
       icanaz(0:mem-1) = 0
       return
    endif

    nz = count_zones(nbits, lbits)
    nb = count_packed(nbits, mem, ibagaz(0))
    ncs = (nz * lbits) / nbits
    moff = mem - 1 + ncs

    icanaz(0:mem-1:ncs) = 0
    do jz = 0, nz - 1
       jb = jz
       mskb = initial_mask(jz, nbits, lbits)
       jcbgn = jb * lbits / nbits
       jcend = min(mem, (jb + 1) * lbits / nbits)
       do jc = jcbgn,     jcbgn
          msh = mskb + (jc - jcbgn) * nbits
          icanaz(jc:mem-1:ncs) = &
               & IOR(icanaz(jc:mem-1:ncs), &
               &     _IBITS(ibagaz(jb:nb-1:nz), lbits - msh, msh))
       enddo
       do jc = jcbgn + 1, jcend - 1
          msh = mskb + (jc - jcbgn) * nbits
          mc = (moff - jc) / ncs
          icanaz(jc:jc+mc*ncs-1:ncs) = _IBITS(ibagaz(jb:jb+mc*nz-1:nz), lbits - msh, nbits)
       enddo
       do jc = jcend, min(ncs - 1, jcend)
          msh = mskb + (jc - jcbgn) * nbits
          msh = msh - lbits
          mc = (moff - jc) / ncs
          icanaz(jc:jc+mc*ncs-1:ncs) = &
               & ISHFT(_IBITS(ibagaz(jb:jb+mc*nz-1:nz), 0, nbits - msh), msh)
       enddo
    enddo

    return
  end subroutine pack_restore_str_ii

  subroutine pack_restore_str_ll &
       & (ierr,   icanaz, &
       &  ibagaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI64, KICNZ = KI64

    integer,            intent(out) :: ierr
    integer(kind=KICNZ),intent(out) :: icanaz(0:*)  ! destination buffer
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))
    integer jz, nz
    integer jb, nb  ! range of bagazo (destination) elements
    integer mskb
    integer jcbgn, jcend, jc, ncs
    integer moff,  mc
    integer msh

    ierr = err_default

    if (lbits.eq.nbits) then
       icanaz(0:mem-1) = ibagaz(0:mem-1)
       return
    else if (nbits.eq.0) then
       icanaz(0:mem-1) = 0
       return
    endif

    nz = count_zones(nbits, lbits)
    nb = count_packed(nbits, mem, ibagaz(0))
    ncs = (nz * lbits) / nbits
    moff = mem - 1 + ncs

    icanaz(0:mem-1:ncs) = 0
    do jz = 0, nz - 1
       jb = jz
       mskb = initial_mask(jz, nbits, lbits)
       jcbgn = jb * lbits / nbits
       jcend = min(mem, (jb + 1) * lbits / nbits)
       do jc = jcbgn,     jcbgn
          msh = mskb + (jc - jcbgn) * nbits
          icanaz(jc:mem-1:ncs) = &
               & IOR(icanaz(jc:mem-1:ncs), &
               &     _IBITS(ibagaz(jb:nb-1:nz), lbits - msh, msh))
       enddo
       do jc = jcbgn + 1, jcend - 1
          msh = mskb + (jc - jcbgn) * nbits
          mc = (moff - jc) / ncs
          icanaz(jc:jc+mc*ncs-1:ncs) = _IBITS(ibagaz(jb:jb+mc*nz-1:nz), lbits - msh, nbits)
       enddo
       do jc = jcend, min(ncs - 1, jcend)
          msh = mskb + (jc - jcbgn) * nbits
          msh = msh - lbits
          mc = (moff - jc) / ncs
          icanaz(jc:jc+mc*ncs-1:ncs) = &
               & ISHFT(_IBITS(ibagaz(jb:jb+mc*nz-1:nz), 0, nbits - msh), msh)
       enddo
    enddo

    return
  end subroutine pack_restore_str_ll

!!!_  & pack_store_str_sp1 - sequential packing (strides) special 1/32
  subroutine pack_store_str_sp1_ii &
       & (ierr,   ibagaz, &
       &  icanaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI32, KICNZ = KI32

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)  ! destination buffer
    integer(kind=KICNZ),intent(in)  :: icanaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))
    integer jbend
    integer nc
    integer jb, jc, jr, nr

    ierr = err_default
    if (nbits.ne.1) then
       ierr = _ERROR(ERR_INVALID_PARAMETER)
       return
    endif
    jbend = mem / lbits
    if (jbend.ge.1) then
       nc = (jbend - 1) * lbits
       ibagaz(0:jbend-1) = &
            & IOR(ISHFT(IBITS(icanaz(0:0+nc:lbits),0,1), 31), &
            &     +ISHFT(IBITS(icanaz(1:1+nc:lbits),0,1), 30)  &
            &     +ISHFT(IBITS(icanaz(2:2+nc:lbits),0,1), 29)  &
            &     +ISHFT(IBITS(icanaz(3:3+nc:lbits),0,1), 28)  &
            &     +ISHFT(IBITS(icanaz(4:4+nc:lbits),0,1), 27)  &
            &     +ISHFT(IBITS(icanaz(5:5+nc:lbits),0,1), 26)  &
            &     +ISHFT(IBITS(icanaz(6:6+nc:lbits),0,1), 25)  &
            &     +ISHFT(IBITS(icanaz(7:7+nc:lbits),0,1), 24)  &
            &     +ISHFT(IBITS(icanaz(8:8+nc:lbits),0,1), 23)  &
            &     +ISHFT(IBITS(icanaz(9:9+nc:lbits),0,1), 22)  &
            &     +ISHFT(IBITS(icanaz(10:10+nc:lbits),0,1), 21)  &
            &     +ISHFT(IBITS(icanaz(11:11+nc:lbits),0,1), 20)  &
            &     +ISHFT(IBITS(icanaz(12:12+nc:lbits),0,1), 19)  &
            &     +ISHFT(IBITS(icanaz(13:13+nc:lbits),0,1), 18)  &
            &     +ISHFT(IBITS(icanaz(14:14+nc:lbits),0,1), 17)  &
            &     +ISHFT(IBITS(icanaz(15:15+nc:lbits),0,1), 16)  &
            &     +ISHFT(IBITS(icanaz(16:16+nc:lbits),0,1), 15)  &
            &     +ISHFT(IBITS(icanaz(17:17+nc:lbits),0,1), 14)  &
            &     +ISHFT(IBITS(icanaz(18:18+nc:lbits),0,1), 13)  &
            &     +ISHFT(IBITS(icanaz(19:19+nc:lbits),0,1), 12)  &
            &     +ISHFT(IBITS(icanaz(20:20+nc:lbits),0,1), 11)  &
            &     +ISHFT(IBITS(icanaz(21:21+nc:lbits),0,1), 10)  &
            &     +ISHFT(IBITS(icanaz(22:22+nc:lbits),0,1), 9)  &
            &     +ISHFT(IBITS(icanaz(23:23+nc:lbits),0,1), 8)  &
            &     +ISHFT(IBITS(icanaz(24:24+nc:lbits),0,1), 7)  &
            &     +ISHFT(IBITS(icanaz(25:25+nc:lbits),0,1), 6)  &
            &     +ISHFT(IBITS(icanaz(26:26+nc:lbits),0,1), 5)  &
            &     +ISHFT(IBITS(icanaz(27:27+nc:lbits),0,1), 4)  &
            &     +ISHFT(IBITS(icanaz(28:28+nc:lbits),0,1), 3)  &
            &     +ISHFT(IBITS(icanaz(29:29+nc:lbits),0,1), 2)  &
            &     +ISHFT(IBITS(icanaz(30:30+nc:lbits),0,1), 1)  &
            &     +ISHFT(IBITS(icanaz(31:31+nc:lbits),0,1), 0))
    endif
    jb = mem / lbits
    jc = jb * lbits
    nr = mem - jc
    if (nr.gt.0) then
       ibagaz(jb) = ISHFT(IBITS(icanaz(jc+0),0,1), lbits-0-1)
       do jr = 1, nr - 1
          ibagaz(jb) = IOR(ibagaz(jb), ISHFT(IBITS(icanaz(jc+jr),0,1), lbits-jr-1))
       enddo
    endif

    return
  end subroutine pack_store_str_sp1_ii

  subroutine pack_store_str_sp1_ll &
       & (ierr,   ibagaz, &
       &  icanaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI64, KICNZ = KI64

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)  ! destination buffer
    integer(kind=KICNZ),intent(in)  :: icanaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    ! integer,parameter :: lbits = bit_size(ibagaz(0))
    ! integer jz, nz
    ! integer jb, nb  ! range of bagazo (destination) elements
    ! integer mskb
    ! integer jcbgn, jcend, jc, ncs
    ! integer moff,  mc
    ! integer msh

    ierr = _ERROR(ERR_NOT_IMPLEMENTED)

    return
  end subroutine pack_store_str_sp1_ll

!!!_  & pack_restore_str_sp - sequential unpacking (strides) special 1/32
  subroutine pack_restore_str_sp1_ii &
       & (ierr,   icanaz, &
       &  ibagaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI32, KICNZ = KI32

    integer,            intent(out) :: ierr
    integer(kind=KICNZ),intent(out) :: icanaz(0:*)  ! destination buffer
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))
    integer jbend
    integer nc
    integer jb, jc, jr, nr

    ierr = err_default
    if (nbits.ne.1) then
       ierr = _ERROR(ERR_INVALID_PARAMETER)
       return
    endif

    jbend = mem / lbits
    if (jbend.ge.1) then
       nc = (jbend - 1) * lbits
       icanaz(0:0+nc:lbits)    = IBITS(ibagaz(0:jbend-1), lbits-1-0, 1)
       icanaz(1:1+nc:lbits)    = IBITS(ibagaz(0:jbend-1), lbits-1-1, 1)
       icanaz(2:2+nc:lbits)    = IBITS(ibagaz(0:jbend-1), lbits-1-2, 1)
       icanaz(3:3+nc:lbits)    = IBITS(ibagaz(0:jbend-1), lbits-1-3, 1)
       icanaz(4:4+nc:lbits)    = IBITS(ibagaz(0:jbend-1), lbits-1-4, 1)
       icanaz(5:5+nc:lbits)    = IBITS(ibagaz(0:jbend-1), lbits-1-5, 1)
       icanaz(6:6+nc:lbits)    = IBITS(ibagaz(0:jbend-1), lbits-1-6, 1)
       icanaz(7:7+nc:lbits)    = IBITS(ibagaz(0:jbend-1), lbits-1-7, 1)
       icanaz(8:8+nc:lbits)    = IBITS(ibagaz(0:jbend-1), lbits-1-8, 1)
       icanaz(9:9+nc:lbits)    = IBITS(ibagaz(0:jbend-1), lbits-1-9, 1)
       icanaz(10:10+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-10, 1)
       icanaz(11:11+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-11, 1)
       icanaz(12:12+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-12, 1)
       icanaz(13:13+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-13, 1)
       icanaz(14:14+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-14, 1)
       icanaz(15:15+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-15, 1)
       icanaz(16:16+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-16, 1)
       icanaz(17:17+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-17, 1)
       icanaz(18:18+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-18, 1)
       icanaz(19:19+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-19, 1)
       icanaz(20:20+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-20, 1)
       icanaz(21:21+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-21, 1)
       icanaz(22:22+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-22, 1)
       icanaz(23:23+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-23, 1)
       icanaz(24:24+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-24, 1)
       icanaz(25:25+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-25, 1)
       icanaz(26:26+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-26, 1)
       icanaz(27:27+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-27, 1)
       icanaz(28:28+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-28, 1)
       icanaz(29:29+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-29, 1)
       icanaz(30:30+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-30, 1)
       icanaz(31:31+nc:lbits)  = IBITS(ibagaz(0:jbend-1), lbits-1-31, 1)
    endif

    jb = mem / lbits
    jc = jb * lbits
    nr = mem - jc
    if (nr.gt.0) then
       do jr = 0, nr - 1
          icanaz(jc+jr) = IBITS(ibagaz(jb), lbits-jr-1, 1)
       enddo
    endif

    return
  end subroutine pack_restore_str_sp1_ii

  subroutine pack_restore_str_sp1_ll &
       & (ierr,   icanaz, &
       &  ibagaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI64, KICNZ = KI64

    integer,            intent(out) :: ierr
    integer(kind=KICNZ),intent(out) :: icanaz(0:*)  ! destination buffer
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    ! integer,parameter :: lbits = bit_size(ibagaz(0))
    ! integer jz, nz
    ! integer jb, nb  ! range of bagazo (destination) elements
    ! integer mskb
    ! integer jcbgn, jcend, jc, ncs
    ! integer moff,  mc
    ! integer msh

    ierr = _ERROR(ERR_NOT_IMPLEMENTED)

    return
  end subroutine pack_restore_str_sp1_ll

!!!_  & pack_store_trn - transposed packing
  subroutine pack_store_trn_ii &
       & (ierr,   ibagaz, &
       &  icanaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI32, KICNZ = KI32

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)  ! destination buffer
    integer(kind=KICNZ),intent(in)  :: icanaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))

    integer rgrps, ritms
    integer jz,    nz
    integer ngflr
    integer jobgn, joend, jo    ! serial index of pattern
    integer jbbgn, jbend        ! range of bagazo (destination) elements
    integer jcbgn, jcend, nc    ! range of cana-azucar (source) elements
    integer msh
    integer mskb

    ierr = err_default

    if (lbits.eq.nbits) then
       ibagaz(0:mem-1) = icanaz(0:mem-1)
       return
    else if (nbits.eq.0) then
       return
    endif

    nz = count_zones(nbits, lbits)
    call count_remnants(ngflr, rgrps, ritms, nz, nbits, lbits, mem)

    do jz = 0, nz - 1
       jobgn = pos_pattern(jz,   nbits, lbits)
       joend = min(pos_pattern(jz+1, nbits, lbits), mem)
       jbbgn = pos_storages(jz,   ngflr, rgrps)
       jbend = pos_storages(jz+1, ngflr, rgrps)

       mskb = initial_mask(jz, nbits, lbits)
       ! top entries (reset)
       do jo = jobgn, jobgn
          msh = mskb + (jo - jobgn) * nbits
          jcbgn = pos_source(jo,   ngflr, ritms)
          jcend = pos_source(jo+1, ngflr, ritms)
          nc = jcend - jcbgn
          jbend = jbbgn + nc
          ibagaz(jbbgn:jbend-1) = &
               & ISHFT(_IBITS(icanaz(jcbgn:jcend-1), 0, msh), lbits - msh)
       enddo
       ! medium entries (copy whole)
       do jo = jobgn + 1, joend - 1
          msh = mskb + (jo - jobgn) * nbits
          jcbgn = pos_source(jo,   ngflr, ritms)
          jcend = pos_source(jo+1, ngflr, ritms)
          nc = jcend - jcbgn
          jbend = jbbgn + nc
          ibagaz(jbbgn:jbend-1) = &
               & IOR(ibagaz(jbbgn:jbend-1), &
               &     ISHFT(_IBITS(icanaz(jcbgn:jcend-1), 0, nbits), lbits - msh))
       enddo
       ! bottom entries
       do jo = joend, joend
          msh = mskb + (jo - jobgn) * nbits
          msh = msh - lbits
          jcbgn = pos_source(jo,   ngflr, ritms)
          jcend = min(mem, pos_source(jo+1, ngflr, ritms))
          nc = jcend - jcbgn
          jbend = jbbgn + nc
          ibagaz(jbbgn:jbend-1) = &
               & IOR(ibagaz(jbbgn:jbend-1), &
               &     _IBITS(icanaz(jcbgn:jcend-1), msh, nbits - msh))
       enddo
    enddo
  end subroutine pack_store_trn_ii

  subroutine pack_store_trn_ll &
       & (ierr,   ibagaz, &
       &  icanaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI64, KICNZ = KI64

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)  ! destination buffer
    integer(kind=KICNZ),intent(in)  :: icanaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))

    integer rgrps, ritms
    integer jz,    nz
    integer ngflr
    integer jobgn, joend, jo    ! serial index of pattern
    integer jbbgn, jbend        ! range of bagazo (destination) elements
    integer jcbgn, jcend, nc    ! range of cana-azucar (source) elements
    integer msh
    integer mskb

    ierr = err_default

    if (lbits.eq.nbits) then
       ibagaz(0:mem-1) = icanaz(0:mem-1)
       return
    else if (nbits.eq.0) then
       return
    endif

    nz = count_zones(nbits, lbits)
    call count_remnants(ngflr, rgrps, ritms, nz, nbits, lbits, mem)

    do jz = 0, nz - 1
       jobgn = pos_pattern(jz,   nbits, lbits)
       joend = min(pos_pattern(jz+1, nbits, lbits), mem)
       jbbgn = pos_storages(jz,   ngflr, rgrps)
       jbend = pos_storages(jz+1, ngflr, rgrps)

       mskb = initial_mask(jz, nbits, lbits)
       ! top entries (reset)
       do jo = jobgn, jobgn
          msh = mskb + (jo - jobgn) * nbits
          jcbgn = pos_source(jo,   ngflr, ritms)
          jcend = pos_source(jo+1, ngflr, ritms)
          nc = jcend - jcbgn
          jbend = jbbgn + nc
          ibagaz(jbbgn:jbend-1) = &
               & ISHFT(_IBITS(icanaz(jcbgn:jcend-1), 0, msh), lbits - msh)
       enddo
       ! medium entries (copy whole)
       do jo = jobgn + 1, joend - 1
          msh = mskb + (jo - jobgn) * nbits
          jcbgn = pos_source(jo,   ngflr, ritms)
          jcend = pos_source(jo+1, ngflr, ritms)
          nc = jcend - jcbgn
          jbend = jbbgn + nc
          ibagaz(jbbgn:jbend-1) = &
               & IOR(ibagaz(jbbgn:jbend-1), &
               &     ISHFT(_IBITS(icanaz(jcbgn:jcend-1), 0, nbits), lbits - msh))
       enddo
       ! bottom entries
       do jo = joend, joend
          msh = mskb + (jo - jobgn) * nbits
          msh = msh - lbits
          jcbgn = pos_source(jo,   ngflr, ritms)
          jcend = min(mem, pos_source(jo+1, ngflr, ritms))
          nc = jcend - jcbgn
          jbend = jbbgn + nc
          ibagaz(jbbgn:jbend-1) = &
               & IOR(ibagaz(jbbgn:jbend-1), &
               &     _IBITS(icanaz(jcbgn:jcend-1), msh, nbits - msh))
       enddo
    enddo
  end subroutine pack_store_trn_ll

!!!_  & pack_restore_trn - transposed unpacking
  subroutine pack_restore_trn_ii &
       & (ierr,   icanaz, &
       &  ibagaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI32, KICNZ = KI32

    integer,            intent(out) :: ierr
    integer(kind=KICNZ),intent(out) :: icanaz(0:*)  ! destination buffer
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))

    integer rgrps, ritms
    integer jz,    nz
    integer ngflr
    integer jobgn, joend, jo  ! serial index of pattern
    integer jbbgn, jbend      ! range of bagazo (source) elements
    integer jcbgn, jcend, nc  ! range of cana-azucar (destination) elements
    integer msh
    integer mskb

    ierr = err_default

    if (lbits.eq.nbits) then
       icanaz(0:mem-1) = ibagaz(0:mem-1)
       return
    else if (nbits.eq.0) then
       icanaz(0:mem-1) = 0
       return
    endif

    nz = count_zones(nbits, lbits)
    call count_remnants(ngflr, rgrps, ritms, nz, nbits, lbits, mem)

    jo = 0
    ! reset at very beginning
    jcbgn = pos_source(jo,   ngflr, ritms)
    jcend = pos_source(jo+1, ngflr, ritms)
    icanaz(jcbgn:jcend-1) = 0

    do jz = 0, nz - 1
       jobgn = pos_pattern(jz,   nbits, lbits)
       joend = min(pos_pattern(jz+1, nbits, lbits), mem)
       jbbgn = pos_storages(jz,   ngflr, rgrps)
       jbend = pos_storages(jz+1, ngflr, rgrps)

       mskb = initial_mask(jz, nbits, lbits)
       ! top entries (reset)
       do jo = jobgn, jobgn
          msh = mskb + (jo - jobgn) * nbits
          jcbgn = pos_source(jo,   ngflr, ritms)
          jcend = pos_source(jo+1, ngflr, ritms)
          nc = jcend - jcbgn
          jbend = jbbgn + nc
          icanaz(jcbgn:jcend-1) = &
               & IOR(icanaz(jcbgn:jcend-1), &
               &     _IBITS(ibagaz(jbbgn:jbend-1), lbits - msh, msh))
       enddo
       ! medium entries (copy whole)
       do jo = jobgn + 1, joend - 1
          msh = mskb + (jo - jobgn) * nbits
          jcbgn = pos_source(jo,   ngflr, ritms)
          jcend = pos_source(jo+1, ngflr, ritms)
          nc = jcend - jcbgn
          jbend = jbbgn + nc
          icanaz(jcbgn:jcend-1) = &
               & _IBITS(ibagaz(jbbgn:jbend-1), lbits - msh, nbits)
       enddo
       ! bottom entries
       do jo = joend, joend
          msh = mskb + (jo - jobgn) * nbits
          msh = msh - lbits
          jcbgn = pos_source(jo,   ngflr, ritms)
          jcend = min(mem, pos_source(jo+1, ngflr, ritms))
          nc = jcend - jcbgn
          jbend = jbbgn + nc
          icanaz(jcbgn:jcend-1) = &
               & ISHFT(_IBITS(ibagaz(jbbgn:jbend-1), 0, nbits - msh), msh)
       enddo
    enddo
  end subroutine pack_restore_trn_ii

  subroutine pack_restore_trn_ll &
       & (ierr,   icanaz, &
       &  ibagaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI64, KICNZ = KI64

    integer,            intent(out) :: ierr
    integer(kind=KICNZ),intent(out) :: icanaz(0:*)  ! destination buffer
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))

    integer rgrps, ritms
    integer jz,    nz
    integer ngflr
    integer jobgn, joend, jo  ! serial index of pattern
    integer jbbgn, jbend      ! range of bagazo (source) elements
    integer jcbgn, jcend, nc  ! range of cana-azucar (destination) elements
    integer msh
    integer mskb

    ierr = err_default

    if (lbits.eq.nbits) then
       icanaz(0:mem-1) = ibagaz(0:mem-1)
       return
    else if (nbits.eq.0) then
       icanaz(0:mem-1) = 0
       return
    endif

    nz = count_zones(nbits, lbits)
    call count_remnants(ngflr, rgrps, ritms, nz, nbits, lbits, mem)

    jo = 0
    ! reset at very beginning
    jcbgn = pos_source(jo,   ngflr, ritms)
    jcend = pos_source(jo+1, ngflr, ritms)
    icanaz(jcbgn:jcend-1) = 0

    do jz = 0, nz - 1
       jobgn = pos_pattern(jz,   nbits, lbits)
       joend = min(pos_pattern(jz+1, nbits, lbits), mem)
       jbbgn = pos_storages(jz,   ngflr, rgrps)
       jbend = pos_storages(jz+1, ngflr, rgrps)

       mskb = initial_mask(jz, nbits, lbits)
       ! top entries (reset)
       do jo = jobgn, jobgn
          msh = mskb + (jo - jobgn) * nbits
          jcbgn = pos_source(jo,   ngflr, ritms)
          jcend = pos_source(jo+1, ngflr, ritms)
          nc = jcend - jcbgn
          jbend = jbbgn + nc
          icanaz(jcbgn:jcend-1) = &
               & IOR(icanaz(jcbgn:jcend-1), &
               &     _IBITS(ibagaz(jbbgn:jbend-1), lbits - msh, msh))
       enddo
       ! medium entries (copy whole)
       do jo = jobgn + 1, joend - 1
          msh = mskb + (jo - jobgn) * nbits
          jcbgn = pos_source(jo,   ngflr, ritms)
          jcend = pos_source(jo+1, ngflr, ritms)
          nc = jcend - jcbgn
          jbend = jbbgn + nc
          icanaz(jcbgn:jcend-1) = &
               & _IBITS(ibagaz(jbbgn:jbend-1), lbits - msh, nbits)
       enddo
       ! bottom entries
       do jo = joend, joend
          msh = mskb + (jo - jobgn) * nbits
          msh = msh - lbits
          jcbgn = pos_source(jo,   ngflr, ritms)
          jcend = min(mem, pos_source(jo+1, ngflr, ritms))
          nc = jcend - jcbgn
          jbend = jbbgn + nc
          icanaz(jcbgn:jcend-1) = &
               & ISHFT(_IBITS(ibagaz(jbbgn:jbend-1), 0, nbits - msh), msh)
       enddo
    enddo
  end subroutine pack_restore_trn_ll

!!!_  & pack_store_trn_sp1 - transposed packing special (1/32)
  subroutine pack_store_trn_sp1_ii &
       & (ierr,   ibagaz, &
       &  icanaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI32, KICNZ = KI32

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)  ! destination buffer
    integer(kind=KICNZ),intent(in)  :: icanaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))

    integer rgrps, ritms
    integer jz,    nz
    integer ngflr
    integer jobgn, joend, jo        ! serial index of pattern
    integer jbbgn, jbend            ! range of bagazo (destination) elements
    integer jcofs(0:lbits), nc      ! range of cana-azucar (source) elements

    ierr = err_default

    if (nbits.ne.1) then
       ierr = _ERROR(ERR_INVALID_PARAMETER)
       return
    endif

    nz = count_zones(nbits, lbits)
    call count_remnants(ngflr, rgrps, ritms, nz, nbits, lbits, mem)
    jz = 0
    jobgn = pos_pattern(jz,   nbits, lbits)
    joend = min(pos_pattern(jz+1, nbits, lbits), mem)
    jbbgn = pos_storages(jz,   ngflr, rgrps)
    jbend = pos_storages(jz+1, ngflr, rgrps)
    do jo = jobgn, joend
       jcofs(jo) = pos_source(jo,   ngflr, ritms)
    enddo
    jcofs(joend+1:) = 0
    nc = ngflr
    if (nc.gt.0) then
       ibagaz(jbbgn:jbbgn+nc-1) = &
            & IOR(ISHFT(IBITS(icanaz(jcofs(+0):jcofs(+0)+nc-1),0,1), 31), &
            &     +ISHFT(IBITS(icanaz(jcofs(+1):jcofs(+1)+nc-1),0,1), 30)  &
            &     +ISHFT(IBITS(icanaz(jcofs(+2):jcofs(+2)+nc-1),0,1), 29)  &
            &     +ISHFT(IBITS(icanaz(jcofs(+3):jcofs(+3)+nc-1),0,1), 28)  &
            &     +ISHFT(IBITS(icanaz(jcofs(+4):jcofs(+4)+nc-1),0,1), 27)  &
            &     +ISHFT(IBITS(icanaz(jcofs(+5):jcofs(+5)+nc-1),0,1), 26)  &
            &     +ISHFT(IBITS(icanaz(jcofs(+6):jcofs(+6)+nc-1),0,1), 25)  &
            &     +ISHFT(IBITS(icanaz(jcofs(+7):jcofs(+7)+nc-1),0,1), 24)  &
            &     +ISHFT(IBITS(icanaz(jcofs(+8):jcofs(+8)+nc-1),0,1), 23)  &
            &     +ISHFT(IBITS(icanaz(jcofs(+9):jcofs(+9)+nc-1),0,1), 22)  &
            &     +ISHFT(IBITS(icanaz(jcofs(10):jcofs(10)+nc-1),0,1), 21)  &
            &     +ISHFT(IBITS(icanaz(jcofs(11):jcofs(11)+nc-1),0,1), 20)  &
            &     +ISHFT(IBITS(icanaz(jcofs(12):jcofs(12)+nc-1),0,1), 19)  &
            &     +ISHFT(IBITS(icanaz(jcofs(13):jcofs(13)+nc-1),0,1), 18)  &
            &     +ISHFT(IBITS(icanaz(jcofs(14):jcofs(14)+nc-1),0,1), 17)  &
            &     +ISHFT(IBITS(icanaz(jcofs(15):jcofs(15)+nc-1),0,1), 16)  &
            &     +ISHFT(IBITS(icanaz(jcofs(16):jcofs(16)+nc-1),0,1), 15)  &
            &     +ISHFT(IBITS(icanaz(jcofs(17):jcofs(17)+nc-1),0,1), 14)  &
            &     +ISHFT(IBITS(icanaz(jcofs(18):jcofs(18)+nc-1),0,1), 13)  &
            &     +ISHFT(IBITS(icanaz(jcofs(19):jcofs(19)+nc-1),0,1), 12)  &
            &     +ISHFT(IBITS(icanaz(jcofs(20):jcofs(20)+nc-1),0,1), 11)  &
            &     +ISHFT(IBITS(icanaz(jcofs(21):jcofs(21)+nc-1),0,1), 10)  &
            &     +ISHFT(IBITS(icanaz(jcofs(22):jcofs(22)+nc-1),0,1), +9)  &
            &     +ISHFT(IBITS(icanaz(jcofs(23):jcofs(23)+nc-1),0,1), +8)  &
            &     +ISHFT(IBITS(icanaz(jcofs(24):jcofs(24)+nc-1),0,1), +7)  &
            &     +ISHFT(IBITS(icanaz(jcofs(25):jcofs(25)+nc-1),0,1), +6)  &
            &     +ISHFT(IBITS(icanaz(jcofs(26):jcofs(26)+nc-1),0,1), +5)  &
            &     +ISHFT(IBITS(icanaz(jcofs(27):jcofs(27)+nc-1),0,1), +4)  &
            &     +ISHFT(IBITS(icanaz(jcofs(28):jcofs(28)+nc-1),0,1), +3)  &
            &     +ISHFT(IBITS(icanaz(jcofs(29):jcofs(29)+nc-1),0,1), +2)  &
            &     +ISHFT(IBITS(icanaz(jcofs(30):jcofs(30)+nc-1),0,1), +1)  &
            &     +ISHFT(IBITS(icanaz(jcofs(31):jcofs(31)+nc-1),0,1), +0))
    endif
    if (rgrps.gt.0) then
       ibagaz(jbend-1) = ISHFT(_IBITS(icanaz(jcofs(1)-1),0,1), 31)
       do jo = 1, ritms - 1
          ibagaz(jbend-1) = IOR(ibagaz(jbend-1), ISHFT(IBITS(icanaz(jcofs(jo+1)-1),0,1), 32-jo-1))
       enddo
    endif

  end subroutine pack_store_trn_sp1_ii

  subroutine pack_store_trn_sp1_ll &
       & (ierr,   ibagaz, &
       &  icanaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI64, KICNZ = KI64

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)  ! destination buffer
    integer(kind=KICNZ),intent(in)  :: icanaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes
    ierr = _ERROR(ERR_NOT_IMPLEMENTED)
  end subroutine pack_store_trn_sp1_ll

!!!_  & pack_restore_trn_sp1 - transposed unpacking special (1/32)
  subroutine pack_restore_trn_sp1_ii &
       & (ierr,   icanaz, &
       &  ibagaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI32, KICNZ = KI32

    integer,            intent(out) :: ierr
    integer(kind=KICNZ),intent(out) :: icanaz(0:*)  ! destination buffer
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))

    integer rgrps, ritms
    integer jz,    nz
    integer ngflr
    integer jobgn, joend, jo        ! serial index of pattern
    integer jbbgn, jbend            ! range of bagazo (destination) elements
    integer jcofs(0:lbits), nc      ! range of cana-azucar (source) elements

    ierr = err_default

    if (nbits.ne.1) then
       ierr = _ERROR(ERR_INVALID_PARAMETER)
       return
    endif

    nz = count_zones(nbits, lbits)
    call count_remnants(ngflr, rgrps, ritms, nz, nbits, lbits, mem)
    jz = 0
    jobgn = pos_pattern(jz,   nbits, lbits)
    joend = min(pos_pattern(jz+1, nbits, lbits), mem)
    jbbgn = pos_storages(jz,   ngflr, rgrps)
    jbend = pos_storages(jz+1, ngflr, rgrps)
    do jo = jobgn, joend
       jcofs(jo) = pos_source(jo,   ngflr, ritms)
    enddo
    jcofs(joend+1:) = 0
    nc = ngflr
    if (nc.gt.0) then
       icanaz(jcofs(+0):jcofs(+0)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 31, 1)
       icanaz(jcofs(+1):jcofs(+1)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 30, 1)
       icanaz(jcofs(+2):jcofs(+2)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 29, 1)
       icanaz(jcofs(+3):jcofs(+3)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 28, 1)
       icanaz(jcofs(+4):jcofs(+4)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 27, 1)
       icanaz(jcofs(+5):jcofs(+5)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 26, 1)
       icanaz(jcofs(+6):jcofs(+6)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 25, 1)
       icanaz(jcofs(+7):jcofs(+7)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 24, 1)
       icanaz(jcofs(+8):jcofs(+8)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 23, 1)
       icanaz(jcofs(+9):jcofs(+9)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 22, 1)
       icanaz(jcofs(10):jcofs(10)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 21, 1)
       icanaz(jcofs(11):jcofs(11)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 20, 1)
       icanaz(jcofs(12):jcofs(12)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 19, 1)
       icanaz(jcofs(13):jcofs(13)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 18, 1)
       icanaz(jcofs(14):jcofs(14)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 17, 1)
       icanaz(jcofs(15):jcofs(15)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 16, 1)
       icanaz(jcofs(16):jcofs(16)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 15, 1)
       icanaz(jcofs(17):jcofs(17)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 14, 1)
       icanaz(jcofs(18):jcofs(18)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 13, 1)
       icanaz(jcofs(19):jcofs(19)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 12, 1)
       icanaz(jcofs(20):jcofs(20)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 11, 1)
       icanaz(jcofs(21):jcofs(21)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), 10, 1)
       icanaz(jcofs(22):jcofs(22)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), +9, 1)
       icanaz(jcofs(23):jcofs(23)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), +8, 1)
       icanaz(jcofs(24):jcofs(24)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), +7, 1)
       icanaz(jcofs(25):jcofs(25)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), +6, 1)
       icanaz(jcofs(26):jcofs(26)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), +5, 1)
       icanaz(jcofs(27):jcofs(27)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), +4, 1)
       icanaz(jcofs(28):jcofs(28)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), +3, 1)
       icanaz(jcofs(29):jcofs(29)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), +2, 1)
       icanaz(jcofs(30):jcofs(30)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), +1, 1)
       icanaz(jcofs(31):jcofs(31)+nc-1) = IBITS(ibagaz(jbbgn:jbbgn+nc-1), +0, 1)
    endif
    if (rgrps.gt.0) then
       do jo = 0, ritms - 1
          icanaz(jcofs(jo+1)-1) = IBITS(ibagaz(jbend-1), 32-jo-1, 1)
       enddo
    endif
  end subroutine pack_restore_trn_sp1_ii
  subroutine pack_restore_trn_sp1_ll &
       & (ierr,   icanaz, &
       &  ibagaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI64, KICNZ = KI64

    integer,            intent(out) :: ierr
    integer(kind=KICNZ),intent(out) :: icanaz(0:*)  ! destination buffer
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))

    ierr = _ERROR(ERR_NOT_IMPLEMENTED)
  end subroutine pack_restore_trn_sp1_ll

!!!_  & pack_store_trn_spdiv - transposed packing special (32 divisor)
  subroutine pack_store_trn_spdiv_ii &
       & (ierr,   ibagaz, &
       &  icanaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI32, KICNZ = KI32

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)  ! destination buffer
    integer(kind=KICNZ),intent(in)  :: icanaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))

    integer rgrps, ritms
    integer jz,    nz
    integer ngflr
    integer jobgn, joend, jo  ! serial index of pattern
    integer jbbgn, jbend      ! range of bagazo (source) elements
    integer jcbgn, jcend, nc  ! range of cana-azucar (destination) elements
    integer msh

    if (mod(lbits, nbits).ne.0) then
       ierr = _ERROR(ERR_INVALID_PARAMETER)
       return
    endif
    if (lbits.eq.nbits) then
       ibagaz(0:mem-1) = icanaz(0:mem-1)
       return
    endif

    nz = count_zones(nbits, lbits)
    call count_remnants(ngflr, rgrps, ritms, nz, nbits, lbits, mem)
    jz = 0
    jobgn = pos_pattern(jz,   nbits, lbits)
    joend = min(pos_pattern(jz+1, nbits, lbits), mem)
    jbbgn = pos_storages(jz,   ngflr, rgrps)
    jbend = pos_storages(jz+1, ngflr, rgrps)

    ibagaz(jbbgn:jbend-1) = 0
    do jo = jobgn, jobgn
       jcbgn = pos_source(jo,   ngflr, ritms)
       jcend = pos_source(jo+1, ngflr, ritms)
       nc = jcend - jcbgn
       jbend = jbbgn + nc
       msh = lbits - (jo + 1) * nbits
       ibagaz(jbbgn:jbend-1) = ISHFT(_IBITS(icanaz(jcbgn:jcend-1), 0, nbits), msh)
    enddo
    do jo = jobgn + 1, joend - 1
       jcbgn = pos_source(jo,   ngflr, ritms)
       jcend = pos_source(jo+1, ngflr, ritms)
       nc = jcend - jcbgn
       jbend = jbbgn + nc
       msh = lbits - (jo + 1) * nbits
       ibagaz(jbbgn:jbend-1) = IOR(ibagaz(jbbgn:jbend-1), ISHFT(_IBITS(icanaz(jcbgn:jcend-1), 0, nbits), msh))
    enddo
  end subroutine pack_store_trn_spdiv_ii
  subroutine pack_store_trn_spdiv_ll &
       & (ierr,   ibagaz, &
       &  icanaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI64, KICNZ = KI64

    integer,            intent(out) :: ierr
    integer(kind=KIBGZ),intent(out) :: ibagaz(0:*)  ! destination buffer
    integer(kind=KICNZ),intent(in)  :: icanaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))

    ierr = _ERROR(ERR_NOT_IMPLEMENTED)
  end subroutine pack_store_trn_spdiv_ll

!!!_  & pack_restore_trn_spdiv - transposed unpacking special (32 divisor)
  subroutine pack_restore_trn_spdiv_ii &
       & (ierr,   icanaz, &
       &  ibagaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI32, KICNZ = KI32

    integer,            intent(out) :: ierr
    integer(kind=KICNZ),intent(out) :: icanaz(0:*)  ! destination buffer
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))

    integer rgrps, ritms
    integer jz,    nz
    integer ngflr
    integer jobgn, joend, jo  ! serial index of pattern
    integer jbbgn, jbend      ! range of bagazo (source) elements
    integer jcbgn, jcend, nc  ! range of cana-azucar (destination) elements
    integer msh

    ierr = err_default

    if (mod(lbits, nbits).ne.0) then
       ierr = _ERROR(ERR_INVALID_PARAMETER)
       return
    endif
    if (lbits.eq.nbits) then
       icanaz(0:mem-1) = ibagaz(0:mem-1)
       return
    endif

    nz = count_zones(nbits, lbits)
    call count_remnants(ngflr, rgrps, ritms, nz, nbits, lbits, mem)
    jz = 0
    jobgn = pos_pattern(jz,   nbits, lbits)
    joend = min(pos_pattern(jz+1, nbits, lbits), mem)
    jbbgn = pos_storages(jz,   ngflr, rgrps)
    jbend = pos_storages(jz+1, ngflr, rgrps)

    do jo = jobgn, joend - 1
       jcbgn = pos_source(jo,   ngflr, ritms)
       jcend = pos_source(jo+1, ngflr, ritms)
       nc = jcend - jcbgn
       jbend = jbbgn + nc
       msh = lbits - (jo + 1) * nbits
       icanaz(jcbgn:jcend-1) = _IBITS(ibagaz(jbbgn:jbend-1), msh, nbits)
    enddo
  end subroutine pack_restore_trn_spdiv_ii

  subroutine pack_restore_trn_spdiv_ll &
       & (ierr,   icanaz, &
       &  ibagaz, mem,   nbits)
    implicit none
    integer,parameter :: KIBGZ = KI64, KICNZ = KI64

    integer,            intent(out) :: ierr
    integer(kind=KICNZ),intent(out) :: icanaz(0:*)  ! destination buffer
    integer(kind=KIBGZ),intent(in)  :: ibagaz(0:*)  ! source buffer
    integer,            intent(in)  :: mem          ! number of items
    integer,            intent(in)  :: nbits        ! target bit sizes

    integer,parameter :: lbits = bit_size(ibagaz(0))

    ierr = _ERROR(ERR_NOT_IMPLEMENTED)
  end subroutine pack_restore_trn_spdiv_ll

!!!_  & unparse_relleno - packing method id
  subroutine unparse_relleno (ierr, str, kpack)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    integer,         intent(in)  :: kpack
    integer,parameter :: mmask = RELLENO_MANUAL - 1
    integer kp
    ierr = 0
    str = ' '
    kp = IAND(mmask, kpack)
    select case (kp)
    case (RELLENO_TRANSPOSE)
       str(1:1) = 'T'
    case (RELLENO_SEQUENTIAL)
       str(1:1) = 'I'
    case (RELLENO_STRIDE)
       str(1:1) = 'S'
    case default
       str(1:1) = 'E'
    end select
    if (IAND(RELLENO_MANUAL, kpack).ne.0) then
       str(2:2) = 'M'
    else
       str(2:2) = 'D'
    endif
  end subroutine unparse_relleno

!!!_  & show_props_trn - show various properties
  subroutine show_props_trn &
       & (ierr, nbits, mem, lbits, tag, u)
    use TOUZA_Trp_std,only: choice, choice_a
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: mem
    integer,         intent(in)          :: nbits
    integer,         intent(in)          :: lbits
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u

    character(len=128) :: ti
    integer ui
    integer,parameter :: limb = 128
    integer :: kbuf(0:limb)

    integer jz, nz
    integer rgrps, ritms, ngflr
    integer mskb
    integer jobgn, joend, jo, no
    integer jbbgn, jbend

    ierr = err_default

    ui = choice(-1, u)
    call choice_a(ti, 'TrTr', tag)

    nz = count_zones(nbits, lbits)
    call count_remnants(ngflr, rgrps, ritms, nz, nbits, lbits, mem)

101 format(A, ': ', 'bits=', I0, '/', I0, 1x, 'members=', I0, 1x, 'zones=', I0)
102 format(A, ': ', 'rem/i=', I0, 1x, 'rem/g=', I0, 1x, 'grps=', I0)
    if (ui.ge.0) then
       write(ui, 101) trim(ti), nbits, lbits, mem, nz
       write(ui, 102) trim(ti), ritms, rgrps, ngflr
    else
       write(*, 101) trim(ti), nbits, lbits, mem, nz
       write(*, 102) trim(ti), ritms, rgrps, ngflr
    endif

201 format(A, ': ', 'zone ', I2, ':', 2x, I0, ':', I0)
213 format(A, ': ', 'bidx ', I2, ':', 2x, 128(1x, I7))
211 format(A, ': ', 'cpos ', I2, ':', 2x, 128(1x, I7))
212 format(A, ': ', 'ppos ', I2, ':', 2x, 128(1x, I7))
    do jz = 0, nz - 1
       jobgn = pos_pattern(jz,   nbits, lbits)
       joend = min(pos_pattern(jz+1, nbits, lbits), mem)
       jbbgn = pos_storages(jz,   ngflr, rgrps)
       jbend = pos_storages(jz+1, ngflr, rgrps)
       mskb = initial_mask(jz, nbits, lbits)
       if (ui.ge.0) then
          write(ui, 201) trim(ti), jz, jbbgn, jbend
       else
          write(*, 201) trim(ti), jz, jbbgn, jbend
       endif
       ! pattern order
       no = joend - jobgn
       do jo = jobgn, joend + 1
          kbuf(jo-jobgn) = jo
       enddo
       if (ui.ge.0) then
          write(ui, 213) trim(ti), jz, kbuf(0:no)
       else
          write(*, 213) trim(ti), jz, kbuf(0:no)
       endif
       ! bit pattern
       no = joend - jobgn
       do jo = jobgn, joend + 1
          kbuf(jo-jobgn) = mskb + (jo - jobgn) * nbits
       enddo
       if (jz.eq.nz - 1) then
          no = no - 1
       endif
       if (kbuf(no).gt.lbits) then
          no = no + 1
          kbuf(no) = lbits - kbuf(no-1)
       endif
       if (ui.ge.0) then
          write(ui, 212) trim(ti), jz, kbuf(0:no)
       else
          write(*, 212) trim(ti), jz, kbuf(0:no)
       endif
       ! canaz index
       no = joend - jobgn
       do jo = jobgn, joend + 1
          kbuf(jo-jobgn) = pos_source(jo,   ngflr, ritms)
       enddo
       if (ui.ge.0) then
          write(ui, 211) trim(ti), jz, kbuf(0:no)
       else
          write(*, 211) trim(ti), jz, kbuf(0:no)
       endif

    enddo

    return
  end subroutine show_props_trn
!!!_  & show_packed
  subroutine show_packed_i &
       & (ierr, ibagaz, mem, nbits, kpack, tag, u)
    implicit none
    integer,parameter :: KIBGZ = KI32
    integer,            intent(out)         :: ierr
    integer(kind=KIBGZ),intent(in)          :: ibagaz(0:*)
    integer,            intent(in)          :: mem
    integer,            intent(in)          :: nbits, kpack
    character(len=*),   intent(in),optional :: tag
    integer,            intent(in),optional :: u

    ierr = err_default
    if (IAND(kpack, RELLENO_SEQUENTIAL).eq.0) then
       call show_packed_trn_i &
            & (ierr, ibagaz, mem, nbits, tag, u)
    else
       call show_packed_seq_i &
            & (ierr, ibagaz, mem, nbits, tag, u)
    endif
    return
  end subroutine show_packed_i

  subroutine show_packed_l &
       & (ierr, ibagaz, mem, nbits, kpack, tag, u)
    implicit none
    integer,parameter :: KIBGZ = KI64
    integer,            intent(out)         :: ierr
    integer(kind=KIBGZ),intent(in)          :: ibagaz(0:*)
    integer,            intent(in)          :: mem
    integer,            intent(in)          :: nbits, kpack
    character(len=*),   intent(in),optional :: tag
    integer,            intent(in),optional :: u

    ierr = err_default
    if (IAND(kpack, RELLENO_SEQUENTIAL).eq.0) then
       call show_packed_trn_l &
            & (ierr, ibagaz, mem, nbits, tag, u)
    else
       call show_packed_seq_l &
            & (ierr, ibagaz, mem, nbits, tag, u)
    endif
    return
  end subroutine show_packed_l

!!!_   & show_packed_trn
  subroutine show_packed_trn_i &
       & (ierr, ibagaz, mem, nbits, tag, u)
    use TOUZA_Trp_std,only: binstr, choice, choice_a
    implicit none
    integer,parameter :: KIBGZ = KI32
    integer,            intent(out)         :: ierr
    integer(kind=KIBGZ),intent(in)          :: ibagaz(0:*)
    integer,            intent(in)          :: mem
    integer,            intent(in)          :: nbits
    character(len=*),   intent(in),optional :: tag
    integer,            intent(in),optional :: u

    integer,parameter :: lbits = bit_size(ibagaz(0))

    integer kofs
    integer ui
    character(len=128) :: ti
    character(len=128) :: bs
    integer jz, nz
    integer jbbgn, jbend, jb
    integer rgrps, ritms, ngflr

    ierr = err_default
    ui = choice(-1, u)
202 format(A, ':bagaz/t:')
203 format(    'bagaz/t:')
    if (present(tag)) then
       write(ti, 202) trim(tag)
    else
       write(ti, 203)
    endif

201 format(A, I0, ': ', A)
    nz = count_zones(nbits, lbits)
    call count_remnants(ngflr, rgrps, ritms, nz, nbits, lbits, mem)
    kofs = MOD(lbits, nbits)
    do jz = 0, nz - 1
       jbbgn = pos_storages(jz,   ngflr, rgrps)
       jbend = pos_storages(jz+1, ngflr, rgrps)
       do jb = jbbgn, jbend - 1
          call binstr(bs, ibagaz(jb), KM=nbits, KO=kofs)
          if (ui.ge.0) then
             write(ui, 201) trim(ti), jb, trim(bs)
          else
             write(*,  201) trim(ti), jb, trim(bs)
          endif
       enddo
       kofs = MOD(kofs + lbits, nbits)
    enddo

    return
  end subroutine show_packed_trn_i

  subroutine show_packed_trn_l &
       & (ierr, ibagaz, mem, nbits, tag, u)
    use TOUZA_Trp_std,only: binstr, choice, choice_a
    implicit none
    integer,parameter :: KIBGZ = KI64
    integer,            intent(out)         :: ierr
    integer(kind=KIBGZ),intent(in)          :: ibagaz(0:*)
    integer,            intent(in)          :: mem
    integer,            intent(in)          :: nbits
    character(len=*),   intent(in),optional :: tag
    integer,            intent(in),optional :: u

    integer,parameter :: lbits = bit_size(ibagaz(0))

    integer kofs
    integer ui
    character(len=128) :: ti
    character(len=128) :: bs
    integer jz, nz
    integer jbbgn, jbend, jb
    integer rgrps, ritms, ngflr

    ierr = err_default
    ui = choice(-1, u)
202 format(A, ':bagaz/t:')
203 format(    'bagaz/t:')
    if (present(tag)) then
       write(ti, 202) trim(tag)
    else
       write(ti, 203)
    endif

201 format(A, I0, ': ', A)
    nz = count_zones(nbits, lbits)
    call count_remnants(ngflr, rgrps, ritms, nz, nbits, lbits, mem)
    kofs = MOD(lbits, nbits)
    do jz = 0, nz - 1
       jbbgn = pos_storages(jz,   ngflr, rgrps)
       jbend = pos_storages(jz+1, ngflr, rgrps)
       do jb = jbbgn, jbend - 1
          call binstr(bs, ibagaz(jb), KM=nbits, KO=kofs)
          if (ui.ge.0) then
             write(ui, 201) trim(ti), jb, trim(bs)
          else
             write(*,  201) trim(ti), jb, trim(bs)
          endif
       enddo
       kofs = MOD(kofs + lbits, nbits)
    enddo

    return
  end subroutine show_packed_trn_l

!!!_   & show_packed_seq
  subroutine show_packed_seq_i &
       & (ierr, ibagaz, mem, nbits, tag, u)
    use TOUZA_Trp_std,only: binstr, choice, choice_a
    implicit none
    integer,parameter :: KIBGZ = KI32
    integer,            intent(out)         :: ierr
    integer(kind=KIBGZ),intent(in)          :: ibagaz(0:*)
    integer,            intent(in)          :: mem
    integer,            intent(in)          :: nbits
    character(len=*),   intent(in),optional :: tag
    integer,            intent(in),optional :: u

    integer,parameter :: lbits = bit_size(ibagaz(0))

    integer kofs
    integer jp, np
    integer ui
    character(len=128) :: ti
    character(len=128) :: bs

    ierr = err_default
    ui = choice(-1, u)
202 format(A, ':bagaz/s:')
203 format(    'bagaz/s:')
    if (present(tag)) then
       write(ti, 202) trim(tag)
    else
       write(ti, 203)
    endif

201 format(A, I0, ': ', A)

    np = count_packed(nbits, mem, ibagaz(0))

    kofs = MOD(lbits, nbits)
    do jp = 0, np - 1
       call binstr(bs, ibagaz(jp), KM=nbits, KO=kofs)
       if (ui.ge.0) then
          write(ui, 201) trim(ti), jp, trim(bs)
       else
          write(*,  201) trim(ti), jp, trim(bs)
       endif
       kofs = MOD(kofs + lbits, nbits)
    enddo

    return
  end subroutine show_packed_seq_i

  subroutine show_packed_seq_l &
       & (ierr, ibagaz, mem, nbits, tag, u)
    use TOUZA_Trp_std,only: binstr, choice, choice_a
    implicit none
    integer,parameter :: KIBGZ = KI64
    integer,            intent(out)         :: ierr
    integer(kind=KIBGZ),intent(in)          :: ibagaz(0:*)
    integer,            intent(in)          :: mem
    integer,            intent(in)          :: nbits
    character(len=*),   intent(in),optional :: tag
    integer,            intent(in),optional :: u

    integer,parameter :: lbits = bit_size(ibagaz(0))

    integer kofs
    integer jp, np
    integer ui
    character(len=128) :: ti
    character(len=128) :: bs

    ierr = err_default
    ui = choice(-1, u)
202 format(A, ':bagaz/s:')
203 format(    'bagaz/s:')
    if (present(tag)) then
       write(ti, 202) trim(tag)
    else
       write(ti, 203)
    endif

201 format(A, I0, ': ', A)

    np = count_packed(nbits, mem, ibagaz(0))

    kofs = MOD(lbits, nbits)
    do jp = 0, np - 1
       call binstr(bs, ibagaz(jp), KM=nbits, KO=kofs)
       if (ui.ge.0) then
          write(ui, 201) trim(ti), jp, trim(bs)
       else
          write(*,  201) trim(ti), jp, trim(bs)
       endif
       kofs = MOD(kofs + lbits, nbits)
    enddo

    return
  end subroutine show_packed_seq_l

!!!_ + custom-made procedures
!!!_  & mask_count_defined - 1/32 (mask) array count defined elements (of slice)
  subroutine mask_count_defined_i &
       & (ierr,  ndef, &
       &  imask, mem,  bes,  nr, kpack)
    implicit none
    integer,parameter :: KIBGZ = KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: ndef
    integer(kind=KIBGZ),intent(in)  :: imask(0:*)   ! source array (mask)
    integer,            intent(in)  :: mem          ! number of items (not size of ibagaz)
    integer,            intent(in)  :: bes(3,0:*)   ! begin/end/stride set
    integer,            intent(in)  :: nr           ! rank of bes
    integer,            intent(in)  :: kpack        ! packing method
    integer,parameter :: nbits = 1
    integer kp
    ierr = err_default
    kp = pack_method(-nbits, mem, kpack)
    select case (kp)
    case (RELLENO_SEQUENTIAL,RELLENO_STRIDE)
       call mask_count_defined_seq_i (ierr, ndef, imask, mem, bes, nr)
    case (RELLENO_TRANSPOSE)
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
  end subroutine mask_count_defined_i
!!!_  & mask_count_defined_seq - 1/32 (mask) array expansion to index list
  subroutine mask_count_defined_seq_i &
       & (ierr,  ndef, &
       &  imask, mem,  bes,  nr)
    implicit none
    integer,parameter :: KIBGZ = KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: ndef
    integer(kind=KIBGZ),intent(in)  :: imask(0:*)   ! source array (mask)
    integer,            intent(in)  :: mem          ! number of items (not size of ibagaz)
    integer,            intent(in)  :: bes(3,0:*)   ! begin/end/stride set
    integer,            intent(in)  :: nr           ! rank of bes

    integer,parameter :: mold = 0_KIBGZ
    integer,parameter :: nbits = 1
    integer,parameter :: lbits = bit_size(mold)
    integer jcbgn, jcend, jcnxt, jc
    integer jbbgn, jbend, rbbgn, rbend, jb, rb     ! j: element  r: mod
    integer mr_c,  cspan_c,  dspan_c
    integer mr_b,  cspan_b,  dspan_b
    integer idx(0:nr-1), next(0:nr), span(0:nr-1)
    integer gstep(0:nr)
    integer jr

    ierr = 0
    ndef = 0
    call set_loop_slice(jcbgn, gstep, next, span, bes, nr)
    ! mr_b, continuous ranks (gap allowed)
    mr_b = continuous_ranks(next, nr, nbits, lbits)
    ! mr_c, continuous ranks (no gap allowed)
    mr_c = continuous_ranks(next, nr, lbits, lbits)
    ! cspan_b, conitinuous span
    cspan_b = 1 + SUM((span(0:mr_b-1) - 1) * gstep(0:mr_b-1))
    ! cspan_b, conitinuous span
    cspan_c = 1 + SUM((span(0:mr_c-1) - 1) * gstep(0:mr_c-1))
    ! dspan_b, discontinuous span
    dspan_b = sum(next(0:mr_b-1))
    ! dspan_c, discontinuous span
    dspan_c = sum(next(0:mr_c-1))

    idx(0:nr-1) = 0
    jcend = 0
    loop_main: do
       ! continuous active span jcbgn:jcend[new]
       jcend = jcbgn
       jcnxt = jcbgn + cspan_b + dspan_b
       loop_cont: do
          ! filled
          jcend = jcbgn + cspan_c
          jbbgn = jcbgn / lbits
          jbend = jcend / lbits
          rbbgn = mod(jcbgn, lbits)
          rbend = mod(jcend, lbits)
          if (jbbgn.lt.jbend) then
             ndef = ndef &
                  & + popcount_tab(_IBITS(imask(jbbgn), 0, lbits - rbbgn)) &
                  & + SUM(popcount_tab(imask(jbbgn+1:jbend-1)))
             ! to avoid out of boundary
             if (rbend.gt.0) &
                  ndef = ndef + popcount_tab(_IBITS(imask(jbend), lbits - rbend, rbend))
          else if (jbbgn.eq.jbend) then
             ndef = ndef + popcount_tab(_IBITS(imask(jbbgn), lbits - rbend, rbend - rbbgn))
          endif
          jcbgn = jcend + dspan_c
          do jr = mr_c, mr_b - 1
             idx(jr) = idx(jr) + 1
             if (idx(jr).lt.span(jr)) cycle loop_cont
             jcbgn = jcbgn + next(jr)
             idx(jr) = 0
          enddo
          exit loop_cont
       enddo loop_cont
       ! remnant
       jb = jcend / lbits
       rb = mod(jcend, lbits)
       jcend = jcend + lbits - rb
       ! non-active
       jcbgn = jcnxt
       do jr = mr_b, nr - 1
          idx(jr) = idx(jr) + 1
          if (idx(jr).lt.span(jr)) cycle loop_main
          jcbgn = jcbgn + next(jr)
          idx(jr) = 0
       enddo
       exit loop_main
    enddo loop_main
    return
  end subroutine mask_count_defined_seq_i

!!!_  & mask_to_idxl - 1/32 (mask) array expansion to index list
  subroutine mask_to_idxl_i &
       & (ierr,  ldst, lsrc, nidx,  &
       &  imask, mem,  bes,  nr, kpack)
    implicit none
    integer,parameter :: KIBGZ = KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: ldst(0:*)    ! index-list (unpacked, local)
    integer,            intent(out) :: lsrc(0:*)    ! index-list (packed)
    integer,            intent(out) :: nidx         ! size of list
    integer(kind=KIBGZ),intent(in)  :: imask(0:*)   ! source array (mask)
    integer,            intent(in)  :: mem          ! number of items (not size of ibagaz)
    integer,            intent(in)  :: bes(3,0:*)   ! begin/end/stride set
    integer,            intent(in)  :: nr           ! rank of bes
    integer,            intent(in)  :: kpack        ! packing method
    integer,parameter :: nbits = 1
    integer kp
    ierr = err_default
    kp = pack_method(-nbits, mem, kpack)
    select case (kp)
    case (RELLENO_SEQUENTIAL,RELLENO_STRIDE)
       call mask_to_idxl_seq_i(ierr, ldst, lsrc, nidx, imask, mem, bes, nr)
    case (RELLENO_TRANSPOSE)
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
  end subroutine mask_to_idxl_i
!!!_  & mask_to_idxl - 1/32 (mask) array expansion to index list
  subroutine mask_to_idxl_seq_i &
       & (ierr,  ldst, lsrc, nidx,  &
       &  imask, mem,  bes,  nr)
    implicit none
    integer,parameter :: KIBGZ = KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: ldst(0:*)    ! index-list (unpacked, local)
    integer,            intent(out) :: lsrc(0:*)    ! index-list (packed)
    integer,            intent(out) :: nidx         ! size of list
    integer(kind=KIBGZ),intent(in)  :: imask(0:*)   ! source array (mask)
    integer,            intent(in)  :: mem          ! number of items (not size of ibagaz)
    integer,            intent(in)  :: bes(3,0:*)   ! begin/end/stride set
    integer,            intent(in)  :: nr           ! rank of bes

    integer,parameter :: mold = 0_KIBGZ
    integer,parameter :: nbits = 1
    integer,parameter :: lbits = bit_size(mold)
    integer jcbgn, jcend, jcnxt, jc
    integer jbbgn, jbend, rbbgn, rbend, jb, rb     ! j: element  r: mod
    integer mr_c,  cspan_c,  dspan_c
    integer mr_b,  cspan_b,  dspan_b
    integer idx(0:nr-1), next(0:nr), span(0:nr-1)
    integer gstep(0:nr)
    integer jr
    integer jset, jdst

    ierr = 0
    call set_loop_slice(jcbgn, gstep, next, span, bes, nr)
    ! mr_b, continuous ranks (gap allowed)
    mr_b = continuous_ranks(next, nr, nbits, lbits)
    ! mr_c, continuous ranks (no gap allowed)
    mr_c = continuous_ranks(next, nr, lbits, lbits)
    ! cspan_b, conitinuous span
    cspan_b = 1 + SUM((span(0:mr_b-1) - 1) * gstep(0:mr_b-1))
    ! cspan_b, conitinuous span
    cspan_c = 1 + SUM((span(0:mr_c-1) - 1) * gstep(0:mr_c-1))
    ! dspan_b, discontinuous span
    dspan_b = sum(next(0:mr_b-1))
    ! dspan_c, discontinuous span
    dspan_c = sum(next(0:mr_c-1))

    idx(0:nr-1) = 0
    jcend = 0
    nidx = 0
    jset = 0
    jdst = 0
    loop_main: do
       ! nonactive span jcend[prev]:jcbgn
       jbbgn = jcbgn / lbits            ! safe_div(jcbgn, nbits=1, lbits)
       rbbgn = mod(jcbgn, lbits)        ! safe_mod(jcbgn, nbits=1, lbits)
       jbend = jcend / lbits
       rbend = mod(jcend, lbits)
       !  e     b
       ! [xxxxx]+(... )
       jset = jset + SUM(popcount_tab(imask(jbend:jbbgn-1)))
       jset = jset + popcount_tab(_IBITS(imask(jbbgn), lbits - rbbgn, rbbgn))
       ! continuous active span jcbgn:jcend[new]
       jcend = jcbgn
       jcnxt = jcbgn + cspan_b + dspan_b
       loop_cont: do
          ! gap
          do jc = jcend, jcbgn - 1
             jb = jc / lbits
             rb = mod(jc, lbits)
             if (BTEST(imask(jb), lbits - rb - 1)) jset = jset + 1
          enddo
          ! filled
          jcend = jcbgn + cspan_c
          do jc = jcbgn, jcend - 1
             jb = jc / lbits
             rb = mod(jc, lbits)
             if (BTEST(imask(jb), lbits - rb - 1)) then
                lsrc(nidx) = jset
                ldst(nidx) = jdst
                nidx = nidx + 1
                jset = jset + 1
             endif
             jdst = jdst + 1
          enddo
          jcbgn = jcend + dspan_c
          do jr = mr_c, mr_b - 1
             idx(jr) = idx(jr) + 1
             if (idx(jr).lt.span(jr)) cycle loop_cont
             jcbgn = jcbgn + next(jr)
             idx(jr) = 0
          enddo
          exit loop_cont
       enddo loop_cont
       ! remnant
       jb = jcend / lbits
       rb = mod(jcend, lbits)
       jcend = jcend + lbits - rb
       ! non-active
       jcbgn = jcnxt
       do jr = mr_b, nr - 1
          idx(jr) = idx(jr) + 1
          if (idx(jr).lt.span(jr)) then
             ! update here to avoid out-of-bound access
             jset = jset + popcount_tab(_IBITS(imask(jb), 0, lbits - rb))
             cycle loop_main
          endif
          jcbgn = jcbgn + next(jr)
          idx(jr) = 0
       enddo
       exit loop_main
    enddo loop_main
    return
  end subroutine mask_to_idxl_seq_i
!!!_  & mask_to_runl - 1/32 (mask) array expansion to alternate runlength list
!!!_  & mask_to_runl_seq
  ! subroutine mask_to_runl_seq_i &
  !      & (ierr,  rfil, nfil, &
  !      &  imask, mem,  bes,  nr)
  !   implicit none
  !   integer,parameter :: KIBGZ = KI32
  !   integer,            intent(out) :: ierr
  !   integer,            intent(out) :: rfil(0:*)    ! runlength list (skip read, ..., final-skip)
  !   integer,            intent(out) :: nfil         ! size of rfil.  rfil[nfil] == final-skip
  !   integer(kind=KIBGZ),intent(in)  :: imask(0:*)   ! source array (mask)
  !   integer,            intent(in)  :: mem          ! number of expanded elements
  !   integer,            intent(in)  :: bes(3,0:*)   ! begin/end/stride set
  !   integer,            intent(in)  :: nr           ! rank of bes
  !   integer,parameter :: mold = 0_KIBGZ

  !   integer,parameter :: nbits = 1
  !   integer,parameter :: lbits = bit_size(mold)
  !   integer idx(0:nr-1), next(0:nr), span(0:nr-1)
  !   integer gstep(0:nr)
  !   integer jcbgn, jcend
  !   integer jbbgn, jbend, rbbgn, rbend     ! j: element  r: mod
  !   integer mr_c,  cspan_c,  dspan_c

  !   ierr = err_default
  !   call set_loop_slice(jcbgn, gstep, next, span, bes, nr)
  !   ! mr_c, continuous ranks (no gap allowed)
  !   mr_c = continuous_ranks(next, nr, lbits, lbits)
  !   cspan_c = 1 + SUM((span(0:mr_c-1) - 1) * gstep(0:mr_c-1))
  !   dspan_c = sum(next(0:mr_c-1))

  !   idx(0:nr-1) = 0
  !   nfil = 0
  !   jcend = 0
  !   jbend = 0
  !   rfil(nfil) = 0
  !   loop_main: do
  !      ! nonactive span jge:jgb
  !      jbbgn = safe_div(jcbgn, nbits, lbits)
  !      rbbgn = safe_mod(jcbgn, nbits, lbits)
  !   enddo loop_main

  ! end subroutine mask_to_runl_seq_i

!!!_  & pack_gen_dspl - 1/32 (mask) generate displacement+length list
  subroutine pack_gen_dspl_i &
       & (ierr,  lofs, dsrc, n,   &
       &  nbits, mem,  bes,  nr,  kpack, mold)
    implicit none
    integer,parameter :: KIBGZ = KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: lofs(2, 0:*)    ! offset (unpacked, local, bit offset)
    integer,            intent(out) :: dsrc(2, 0:*)    ! disp+length-list (packed)
    integer,            intent(out) :: n               ! size of list
    integer,            intent(in)  :: nbits           ! source bits
    integer,            intent(in)  :: mem             ! not used
    integer,            intent(in)  :: bes(3,0:*)      ! begin/end/stride set
    integer,            intent(in)  :: nr              ! rank of bes
    integer,            intent(in)  :: kpack           ! packing method
    integer(kind=KIBGZ),intent(in)  :: mold

    integer kp
    ierr = err_default
    kp = pack_method(-nbits, mem, kpack)
    select case (kp)
    case (RELLENO_SEQUENTIAL,RELLENO_STRIDE)
       call pack_gen_dspl_seq_i(ierr, lofs, dsrc, n, nbits, mem, bes, nr, mold)
    case (RELLENO_TRANSPOSE)
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
  end subroutine pack_gen_dspl_i
!!!_  & pack_gen_dspl_seq - 1/32 (mask) generate displacement+length list
  subroutine pack_gen_dspl_seq_i &
       & (ierr,  lofs, dsrc, n,   &
       &  nbits, mem,  bes,  nr,  mold)
    implicit none
    integer,parameter :: KIBGZ = KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: lofs(2, 0:*)    ! offset (unpacked, local, bit offset)
    integer,            intent(out) :: dsrc(2, 0:*)    ! disp+length-list (packed)
    integer,            intent(out) :: n               ! size of list
    integer,            intent(in)  :: nbits           ! source bits
    integer,            intent(in)  :: mem             ! not used
    integer,            intent(in)  :: bes(3,0:*)      ! begin/end/stride set
    integer,            intent(in)  :: nr              ! rank of bes
    integer(kind=KIBGZ),intent(in)  :: mold

    integer,parameter :: lbits = bit_size(mold)
    integer jgb, jge, ngi, ngx, ngz
    integer jmb, jme, rmb, rme
    integer idx(0:nr-1), next(0:nr), span(0:nr-1)
    integer gstep(0:nr), lstep(0:nr)
    integer jr, nri
    integer mc

    ierr = 0
    call set_loop_slice(jgb, gstep, next, span, bes, nr)
    lstep(0) = 1
    do jr = 0, nr - 1
       lstep(jr+1) = lstep(jr) * span(jr)
    enddo
    ! nri, continuous ranks
    nri = continuous_ranks(next, nr, nbits, lbits)
    ! ngi, innermost span
    ngi = max(0, span(0))
    ! ngx, conitinuous span
    ngx = 1 + SUM((span(0:nri-1) - 1) * gstep(0:nri-1))
    ! ngz, discontinuous step
    ngz = sum(next(0:nri-1))
    ! mc, number of canaz in nz zones
    mc = (count_zones(nbits, lbits) * lbits) / nbits
    ! create displacement+length list
    idx(0:nr-1) = 0
    jgb = sum(bes(1, 0:nr-1) * gstep(0:nr-1))
    rme = lbits
    n = 0
    loop_main: do
       jge = jgb + ngx
       ! continuous active span
       jmb = safe_div(jgb, nbits, lbits)
       rmb = safe_mod(jgb, nbits, lbits)
       jme = safe_div(jge, nbits, lbits)
       rme = safe_mod(jge, nbits, lbits)
       dsrc(1, n) = jmb
       dsrc(2, n) = jme - jmb + min(1, rme)
       lofs(1, n) = sum(idx(0:nr-1) * lstep(0:nr-1))
       lofs(2, n) = mod(jgb, mc)
       n = n + 1
       ! iterate nonactive
       jgb = jge + ngz
       do jr = nri, nr - 1
          idx(jr) = idx(jr) + 1
          if (idx(jr).lt.span(jr)) cycle loop_main
          jgb = jgb + next(jr)
          idx(jr) = 0
       enddo
       exit loop_main
    enddo loop_main
    ! sentry
    if (ierr.eq.0) then
       lofs(1, n) = product(span(0:nr-1))
       lofs(2, n) = product(bes(3, 0:nr-1))
    endif

    return
  end subroutine pack_gen_dspl_seq_i
  subroutine pack_gen_runl_i &
       & (ierr,  lofs, rsrc, n,   &
       &  nbits, mem,  bes,  nr,  kpack, mold)
    implicit none
    integer,parameter :: KIBGZ = KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: lofs(2, 0:*)    ! offset (unpacked, local, bit offset)
    integer,            intent(out) :: rsrc(0:*)       ! runlength list (packed)  skip,read,skip,...
    integer,            intent(out) :: n               ! size of list
    integer,            intent(in)  :: nbits           ! source bits
    integer,            intent(in)  :: mem             ! not used
    integer,            intent(in)  :: bes(3,0:*)      ! begin/end/stride set
    integer,            intent(in)  :: nr              ! rank of bes
    integer,            intent(in)  :: kpack           ! packing method
    integer(kind=KIBGZ),intent(in)  :: mold

    integer kp
    ierr = err_default
    kp = pack_method(-nbits, mem, kpack)
    select case (kp)
    case (RELLENO_SEQUENTIAL,RELLENO_STRIDE)
       call pack_gen_runl_seq_i(ierr, lofs, rsrc, n, nbits, mem, bes, nr, mold)
    case (RELLENO_TRANSPOSE)
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
  end subroutine pack_gen_runl_i
!!!_  & pack_gen_runl_seq - 1/32 (mask) generate runlength list
  subroutine pack_gen_runl_seq_i &
       & (ierr,  lofs, rsrc, n,   &
       &  nbits, mem,  bes,  nr,  mold)
    implicit none
    integer,parameter :: KIBGZ = KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: lofs(2, 0:*)    ! offset (unpacked, local, bit offset)
    integer,            intent(out) :: rsrc(0:*)       ! runlength list (packed)  skip,read,skip,...
    integer,            intent(out) :: n               ! size of list
    integer,            intent(in)  :: nbits           ! source bits
    integer,            intent(in)  :: mem             ! not used
    integer,            intent(in)  :: bes(3,0:*)      ! begin/end/stride set
    integer,            intent(in)  :: nr              ! rank of bes
    integer(kind=KIBGZ),intent(in)  :: mold

    integer,parameter :: lbits = bit_size(mold)
    integer jgb, jge, ngi, ngx, ngz
    integer jmb, jme, rmb, rme
    integer idx(0:nr-1), next(0:nr), span(0:nr-1)
    integer gstep(0:nr), lstep(0:nr)
    integer jr, nri
    integer mc

    ierr = 0
    call set_loop_slice(jgb, gstep, next, span, bes, nr)
    lstep(0) = 1
    do jr = 0, nr - 1
       lstep(jr+1) = lstep(jr) * span(jr)
    enddo
    ! nri, continuous ranks
    nri = continuous_ranks(next, nr, nbits, lbits)
    ! ngi, innermost span
    ngi = max(0, span(0))
    ! ngx, conitinuous span
    ngx = 1 + SUM((span(0:nri-1) - 1) * gstep(0:nri-1))
    ! ngz, discontinuous step
    ngz = sum(next(0:nri-1))

    ! mc, number of canaz in nz zones
    mc = (count_zones(nbits, lbits) * lbits) / nbits

    ! create displacement+length list
    idx(0:nr-1) = 0
    rme = lbits
    n = 0
    jge = 0
    jme = 0
    loop_main: do
       ! nonactive span jge:jgb
       jmb = safe_div(jgb, nbits, lbits)
       rmb = safe_mod(jgb, nbits, lbits)
       ! write(*, *) 'runl', n, jme, jmb, '/', jge, jgb
       rsrc(n) = jmb - jme
       n = n + 1
       ! continuous active span jgb:jge
       jge = jgb + ngx
       jme = safe_div(jge, nbits, lbits)
       rme = safe_mod(jge, nbits, lbits)
       ! write(*, *) 'runl', n, jmb, jme + min(1, rme), '/', jgb, jge
       rsrc(n) = jme + min(1, rme) - jmb
       lofs(1, n/2) = sum(idx(0:nr-1) * lstep(0:nr-1))
       lofs(2, n/2) = mod(jgb, mc)
       n = n + 1
       ! iterate nonactive
       jgb = jge + ngz
       jme = jme + min(1, rme)
       do jr = nri, nr - 1
          idx(jr) = idx(jr) + 1
          if (idx(jr).lt.span(jr)) cycle loop_main
          jgb = jgb + next(jr)
          idx(jr) = 0
       enddo
       exit loop_main
    enddo loop_main
    ! sentry
    if (ierr.eq.0) then
       lofs(1, n/2) = product(span(0:nr-1))
       lofs(2, n/2) = product(bes(3, 0:nr-1))
    endif
    return
  end subroutine pack_gen_runl_seq_i
!!!_  - gen_bfc_slice
  subroutine gen_bfc_slice_i &
       & (ierr,  dunp, nunp, rfil, nfil, &
       &  nbits, mem,  bes,  nr,   kpack, mold)
    implicit none
    integer,parameter :: KIBGZ = KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: dunp(npropd, 0:*)    ! unpack sequence (unp offset, buf offset, buf origin)
    integer,            intent(out) :: rfil(0:*)       ! bagaz to buffer runlength list (skip,read,skip,..., final-skip)
    integer,            intent(out) :: nunp, nfil      ! size of dunp rfil
    integer,            intent(in)  :: nbits           ! source bits
    integer,            intent(in)  :: mem             ! not used
    integer,            intent(in)  :: bes(3,0:*)      ! begin/end/stride set
    integer,            intent(in)  :: nr              ! rank of bes
    integer,            intent(in)  :: kpack           ! packing method
    integer(kind=KIBGZ),intent(in)  :: mold

    integer kp
    ierr = err_default
    kp = pack_method(-nbits, mem, kpack)
    select case (kp)
    case (RELLENO_SEQUENTIAL,RELLENO_STRIDE)
       call gen_bfc_slice_seq_i(ierr, dunp, nunp, rfil, nfil, nbits, mem, bes, nr, mold)
    case (RELLENO_TRANSPOSE)
       call gen_bfc_slice_trn_i(ierr, dunp, nunp, rfil, nfil, nbits, mem, bes, nr, mold)
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
  end subroutine gen_bfc_slice_i
!!!_  & gen_bfc_slice_seq - 1/32 (mask) generate bagaz/filter/canaz
  subroutine gen_bfc_slice_seq_i &
       & (ierr,  dunp, nunp, rfil, nfil, &
       &  nbits, mem,  bes,  nr,   mold)
    implicit none
    integer,parameter :: KIBGZ = KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: dunp(npropd, 0:*)    ! unpack sequence (unp offset, buf offset, buf origin, extra buf origin)
    integer,            intent(out) :: rfil(0:*)       ! bagaz to buffer runlength list (skip,read,skip,...)
    integer,            intent(out) :: nunp, nfil      ! size of dunp rfil
    integer,            intent(in)  :: nbits           ! source bits
    integer,            intent(in)  :: mem             ! not used
    integer,            intent(in)  :: bes(3,0:*)      ! begin/end/stride set
    integer,            intent(in)  :: nr              ! rank of bes
    integer(kind=KIBGZ),intent(in)  :: mold

    integer,parameter :: lbits = bit_size(mold)
    integer jcbgn, jcend
    integer jbbgn, jbend, rbbgn, rbend     ! j: element  r: mod
    integer idx(0:nr-1), next(0:nr), span(0:nr-1)
    integer gstep(0:nr)
    integer jr
    integer mc
    integer mr_b, cspan_b, dspan_b
    integer mr_c, cspan_c, dspan_c
    integer jfbgn, jfofs
    integer jubgn
    ! [b] bagaz elements, packed global
    ! [f] filtered bagaz elements
    ! [c] canaz elements, unpacked global
    ! [u] filtered canaz elements

    ierr = 0
    call set_loop_slice(jcbgn, gstep, next, span, bes, nr)
    ! mr_b, continuous ranks (gap allowed)
    mr_b = continuous_ranks(next, nr, nbits, lbits)
    ! mr_c, continuous ranks (no gap allowed)
    mr_c = continuous_ranks(next, nr, lbits, lbits)
    ! cspan_b, conitinuous span
    cspan_b = 1 + SUM((span(0:mr_b-1) - 1) * gstep(0:mr_b-1))
    ! cspan_b, conitinuous span
    cspan_c = 1 + SUM((span(0:mr_c-1) - 1) * gstep(0:mr_c-1))
    ! dspan_b, discontinuous span
    dspan_b = sum(next(0:mr_b-1))
    ! dspan_c, discontinuous span
    dspan_c = sum(next(0:mr_c-1))

    ! mc, number of canaz in nz zones
    mc = (count_zones(nbits, lbits) * lbits) / nbits

    idx(0:nr-1) = 0
    nfil = 0
    nunp = 0
    jcend = 0
    jbend = 0
    jfbgn = 0
    jubgn = 0
    loop_main: do
       ! nonactive span jcend[prev]:jcbgn
       jbbgn = safe_div(jcbgn, nbits, lbits)
       rbbgn = safe_mod(jcbgn, nbits, lbits)
       rfil(nfil) = jbbgn - jbend
       nfil = nfil + 1
       ! continuous active span jcbgn:jcend[new]
       jcend = jcbgn + cspan_b
       jbend = safe_div(jcend, nbits, lbits)
       rbend = safe_mod(jcend, nbits, lbits)
       rfil(nfil) = jbend + min(1, rbend) - jbbgn
       nfil = nfil + 1
       ! unpack list
       jfofs = safe_div(jcbgn, nbits, lbits)
       loop_unp: do
          ! write(*, *) 'bfc', nfil, nunp, jubgn, jcbgn
          dunp(p_cbgn, nunp) = jubgn
          dunp(p_bofs, nunp) = jfbgn + safe_div(jcbgn, nbits, lbits) - jfofs
          dunp(p_orgm, nunp) = mod(jcbgn, mc)
          nunp = nunp + 1
          jubgn = jubgn + cspan_c
          jcbgn = jcbgn + cspan_c + dspan_c
          do jr = mr_c, mr_b - 1
             idx(jr) = idx(jr) + 1
             if (idx(jr).lt.span(jr)) cycle loop_unp
             jcbgn = jcbgn + next(jr)
             idx(jr) = 0
          enddo
          exit loop_unp
       enddo loop_unp
       ! iterate nonactive
       jcbgn = jcend + dspan_b
       jbend = jbend + min(1, rbend)
       jfbgn = jfbgn + jbend - jbbgn
       do jr = mr_b, nr - 1
          idx(jr) = idx(jr) + 1
          if (idx(jr).lt.span(jr)) cycle loop_main
          jcbgn = jcbgn + next(jr)
          idx(jr) = 0
       enddo
       exit loop_main
    enddo loop_main
    ! sentry
    if (ierr.eq.0) then
       jbbgn = count_packed_ii(nbits, mem, mold)
       rfil(nfil) = jbbgn - jbend
       dunp(p_cbgn, nunp) = product(span(0:nr-1))
       dunp(p_bofs, nunp) = jfbgn
       dunp(p_orgm, nunp) = 0
    endif
    return
  end subroutine gen_bfc_slice_seq_i

!!!_  & gen_bfc_slice_trn - 1/32 (mask) generate bagaz/filter/canaz
  subroutine gen_bfc_slice_trn_i &
       & (ierr,  dunp, nunp, rfil, nfil, &
       &  nbits, mem,  bes,  nr,   mold)
    implicit none
    integer,parameter :: KIBGZ = KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: dunp(npropd, 0:*)    ! unpack sequence (unp offset, buf offset, buf origin)
    integer,            intent(out) :: rfil(0:*)       ! bagaz to buffer runlength list (skip,read,skip,...)
    integer,            intent(out) :: nunp, nfil      ! size of dunp rfil
    integer,            intent(in)  :: nbits           ! source bits
    integer,            intent(in)  :: mem             ! not used
    integer,            intent(in)  :: bes(3,0:*)      ! begin/end/stride set
    integer,            intent(in)  :: nr              ! rank of bes
    integer(kind=KIBGZ),intent(in)  :: mold

    integer,parameter :: lbits = bit_size(mold)
    integer cofs(0:lbits)
    integer jz, nz
    integer rgrps, ritms, ngflr
    integer jo, mo, jobgn, joend
    integer jbbgn, jbend
    integer jcbgn, jcend, jcorg
    integer idx(0:nr-1), next(0:nr), span(0:nr-1)
    integer gstep(0:nr)

    ierr = 0
    call set_loop_slice(jcorg, gstep, next, span, bes, nr)

    nz = count_zones(nbits, lbits)
    call count_remnants(ngflr, rgrps, ritms, nz, nbits, lbits, mem)

    mo = min(pos_pattern(nz, nbits, lbits), mem)

    do jo = 0, mo
       cofs(jo) = pos_source(jo, ngflr, ritms)
    enddo
    write(*, *) 'bfc/trn:zones ', nz, mo
    write(*, *) 'bfc/trn:cofs ', cofs(0:mo)
    do jz = 0, nz - 1
       jobgn = pos_pattern(jz,   nbits, lbits)
       joend = min(pos_pattern(jz+1, nbits, lbits), mem)
       jbbgn = pos_storages(jz,   ngflr, rgrps)
       jbend = pos_storages(jz+1, ngflr, rgrps)
       jcbgn = cofs(jobgn)  !! split for jz > 0
       jcend = cofs(joend)  !! non-split range
       ! jcend = cofs(min(mo, joend + 1)) !! including split
       write(*, *) 'bfc/trn:bagaz ', jz, jbbgn, jbend, jobgn, joend, jcbgn, jcend, jcorg
    enddo
  end subroutine gen_bfc_slice_trn_i
!!!_  & set_loop_slice
  subroutine set_loop_slice &
       & (jgorg, gstep, gnext, lspan, bes, nr)
    implicit none
    integer,intent(out) :: jgorg
    integer,intent(out) :: gstep(0:*)
    integer,intent(out) :: gnext(0:*)
    integer,intent(out) :: lspan(0:*)
    integer,intent(in)  :: bes(3, 0:*)    ! [begin, end, size] along rank
    integer,intent(in)  :: nr
    integer jr

    ! lspan: e - b along rank
    lspan(0:nr-1) = max(0, &
         &              max(0, min(bes(3, 0:nr-1), bes(2, 0:nr-1))) &
         &              - max(0, min(bes(3, 0:nr-1), bes(1, 0:nr-1))))

    ! gstep: (*,j,*) to (*,j+1,*)
    gstep(0) = 1
    do jr = 0, nr - 1
       gstep(jr+1) = gstep(jr) * max(0, bes(3, jr))
    enddo

    ! gnext: (*,e,j) to (*,b,j+1)
    gnext(0:nr-1) = gstep(0:nr-1) * max(0, bes(3, 0:nr-1) - lspan(0:nr-1))

    ! jgorg: (b,b,b)
    jgorg = sum(bes(1, 0:nr-1) * gstep(0:nr-1))

    return
  end subroutine set_loop_slice

!!!_  & continuous_ranks()
  integer function continuous_ranks(gnext, nr, nbits, lbits) result(n)
    implicit none
    integer,intent(in) :: gnext(0:*)
    integer,intent(in) :: nr
    integer,intent(in) :: nbits, lbits    ! tolerance of gap in bits
    integer m, jr
    n = 0
    m = 0
    do jr = 0, nr - 1
       n = n + 1
       m = m + gnext(jr)
       if (m * nbits.ge.lbits) exit
    enddo
  end function continuous_ranks

!!!_  & is_in_slice()
  PURE &
    logical function is_in_slice(j, bes, nr) result(b)
    implicit none
    integer,intent(in) :: j
    integer,intent(in) :: bes(3, 0:*)
    integer,intent(in) :: nr
    integer jr, jb, jx
    b = .FALSE.
    jb = j
    do jr = 0, nr - 1
       jx = mod(jb, bes(3, jr))
       if (jx.lt.bes(1, jr).or.bes(2, jr).le.jx) return
       jb = jb / bes(3, jr)
    enddo
    b = .TRUE.
  end function is_in_slice

!!!_  & gen_bfc_idxl - 1/32 (mask) generate bagaz/filter/canaz
  subroutine gen_bfc_idxl_i &
       & (ierr,  dunp, nunp, rfil, nfil,  &
       &  nbits, mem,  lsrc, nidx, kpack, mold)
    implicit none
    integer,parameter :: KIBGZ = KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: dunp(npropd, 0:*)    ! unpack sequence (unp offset, buf offset, buf origin)
    integer,            intent(out) :: rfil(0:*)       ! bagaz to buffer runlength list (skip,read,skip,...)
    integer,            intent(out) :: nunp, nfil      ! size of dunp rfil
    integer,            intent(in)  :: nbits           ! source bits
    integer,            intent(in)  :: mem             ! not used
    integer,            intent(in)  :: lsrc(0:*)       ! index list
    integer,            intent(in)  :: nidx            ! rank of bes
    integer,            intent(in)  :: kpack           ! packing method
    integer(kind=KIBGZ),intent(in)  :: mold

    integer kp
    ierr = err_default
    kp = pack_method(-nbits, mem, kpack)
    select case (kp)
    case (RELLENO_SEQUENTIAL,RELLENO_STRIDE)
       call gen_bfc_idxl_seq_i &
            & (ierr,  dunp, nunp, rfil, nfil, &
            &  nbits, mem,  lsrc, nidx, mold)
    case (RELLENO_TRANSPOSE)
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    case default
       ierr = _ERROR(ERR_INVALID_SWITCH)
    end select
  end subroutine gen_bfc_idxl_i

!!!_  & gen_bfc_idxl_seq - 1/32 (mask) generate bagaz/filter/canaz
  subroutine gen_bfc_idxl_seq_i &
       & (ierr,  dunp, nunp, rfil, nfil, &
       &  nbits, mem,  lsrc, nidx, mold)
    implicit none
    integer,parameter :: KIBGZ = KI32
    integer,            intent(out) :: ierr
    integer,            intent(out) :: dunp(npropd, 0:*)    ! unpack sequence (unp offset, buf offset, buf origin)
    integer,            intent(out) :: rfil(0:*)       ! bagaz to buffer runlength list (skip,read,skip,...)
    integer,            intent(out) :: nunp, nfil      ! size of dunp rfil
    integer,            intent(in)  :: nbits           ! source bits
    integer,            intent(in)  :: mem             ! not used
    integer,            intent(in)  :: lsrc(0:*)       ! index list
    integer,            intent(in)  :: nidx            ! rank of bes
    integer(kind=KIBGZ),intent(in)  :: mold

    integer,parameter :: lbits = bit_size(mold)
    integer ji
    integer jfbgn, jfend, rfend, jforg
    integer juorg, juend
    integer mc, mu
    integer mgap

    ierr = 0
    ! mc, number of canaz in nz zones
    mc = (count_zones(nbits, lbits) * lbits) / nbits
    mgap = (lbits - 1) / nbits + 1

    nunp = 0
    nfil = 0
    ji = 0
    jfbgn = 0
    jfend = 0
    jforg = 0
  ! write(*, *) 'bfc/lsrc:', lsrc(0:nidx-1)
    loop_out: do
       if (ji.ge.nidx) exit
       jfbgn = safe_div(lsrc(ji), nbits, lbits)
       ! write(*, *) 'bfc/idxl: ', ji, lsrc(ji), jfend, jforg
       rfil(nfil) = jfbgn - jfend
       nfil = nfil + 1
       loop_unp: do
          juorg = lsrc(ji)
          dunp(p_cbgn, nunp) = ji
          dunp(p_bofs, nunp) = jforg + safe_div(juorg, nbits, lbits) - jfbgn
          dunp(p_orgm, nunp) = mod(juorg, mc)
          nunp = nunp + 1
          ! juorg:juorg+nu  no-gap continuous sequence
          mu = 0
          do
             ji = ji + 1
             mu = mu + 1
             if (ji.ge.nidx) exit loop_unp
             if (lsrc(ji).gt.juorg + mu) exit
          enddo
          ! write(*, *) 'bfc/idxl/cont: ', ji, lsrc(ji-1), lsrc(ji), mgap
          !! identical
          !! if (lsrc(ji) - (lsrc(ji - 1) + 1) .ge. mgap) exit loop_unp
          if (lsrc(ji) - (lsrc(ji - 1)) .gt. mgap) exit loop_unp
       enddo loop_unp
       juend = lsrc(ji - 1) + 1
       jfend = safe_div(juend, nbits, lbits)
       rfend = safe_mod(juend, nbits, lbits)
       jfend = jfend + min(1, rfend)
       rfil(nfil) = jfend - jfbgn
       jforg = jforg + jfend - jfbgn
       nfil = nfil + 1
    enddo loop_out
    ! sentry
    if (ierr.eq.0) then
       jfbgn = count_packed_ii(nbits, mem, mold)
       rfil(nfil) = jfbgn - jfend
       dunp(p_cbgn, nunp) = nidx
       dunp(p_bofs, nunp) = jforg
       dunp(p_orgm, nunp) = 0
    endif
  end subroutine gen_bfc_idxl_seq_i
!!!_ + private procedures
!!!_  & pack_method ()
  PURE &
  integer function pack_method &
       & (nbits, nmem, kpack) &
       & result(m)
    implicit none
    integer,intent(in) :: nbits ! + to encode, - to decode
    integer,intent(in) :: nmem
    integer,intent(in) :: kpack
    integer,parameter :: mmask = RELLENO_MANUAL - 1
    m = IAND(mmask, kpack)
  end function pack_method

!!!_  & is_enabled_manual ()
  PURE &
  logical function is_enabled_manual &
       & (nbits, nmem, kpack) &
       & result(b)
    implicit none
    integer,intent(in) :: nbits ! + to encode, - to decode
    integer,intent(in) :: nmem
    integer,intent(in) :: kpack
    b = IAND(RELLENO_MANUAL, kpack).ne.0
  end function is_enabled_manual

!!!_  & count_zones() - count different padding patterns (maximum)
  PURE &
  integer function count_zones &
       & (nbits, lbits) result(n)
    implicit none
    integer,intent(in) :: nbits ! target bit sizes
    integer,intent(in) :: lbits ! storage bit sizes
    integer k, m
    if (nbits.le.0) then
       n = -1
    else
       ! get gcd
       k = lbits
       n = nbits
       do
          m = mod(k, n)
          if (m.eq.0) exit
          k = n
          n = m
       enddo
       n = nbits / n
    endif
    return
  end function count_zones
!!!_  & count_packed
  integer function count_packed_ii &
       & (nbits, mem, mold) result (n)
    implicit none
    integer,           intent(in) :: nbits
    integer,           intent(in) :: mem
    integer(kind=KI32),intent(in) :: mold
    integer,parameter :: lbits = bit_size(mold)
    n = div_ceiling_safe(nbits, mem, lbits)
    return
  end function count_packed_ii

  integer function count_packed_ll &
       & (nbits, mem, mold) result (n)
    implicit none
    integer,           intent(in) :: nbits
    integer,           intent(in) :: mem
    integer(kind=KI64),intent(in) :: mold
    integer,parameter :: lbits = bit_size(mold)
    n = div_ceiling_safe(nbits, mem, lbits)
    return
  end function count_packed_ll

!!!_  & count_remnants
  subroutine count_remnants &
       & (ngflr, rgrps, ritms, nzones, nbits, lbits, mem)
    implicit none
    integer,intent(out)   :: ngflr ! floor of group widths
    integer,intent(out)   :: rgrps ! remnant in groups
    integer,intent(out)   :: ritms ! remnant in items
    integer,intent(inout) :: nzones
    integer,intent(in)    :: nbits
    integer,intent(in)    :: lbits
    integer,intent(in)    :: mem   ! source item sizes

    integer mend

    mend = (lbits * nzones) / nbits
    ritms = mod(mem, mend)
    rgrps = div_ceiling_safe(nbits, ritms, lbits)

    ngflr = mem / mend

    if (mem.lt.mend) nzones = rgrps
  end subroutine count_remnants

!!!_  & div_ceiling() - ceil(n/d)
  ELEMENTAL integer function div_ceiling (num, den) result(r)
    implicit none
    integer,intent(in) :: num, den

    r = (num + den - 1) / den
    return
  end function div_ceiling

!!!_  & div_ceiling_safe() - ceil(f * n / d)
  ELEMENTAL integer function div_ceiling_safe (f, num, den) result(r)
    implicit none
    integer,intent(in) :: f, num, den

    r = f * (num / den) &
         & + (f * mod(num, den) + den - 1) / den
    return
  end function div_ceiling_safe

!!!_  & safe_div() - large * small / d
  ELEMENTAL integer function safe_div (l, s, d) result(r)
    implicit none
    !  L = da + b
    ! (L * S) / d = (Sda + Sb) / d = Sa + Sb / d
    integer,intent(in) :: l, s, d
    r = s * (l / d) + (s * mod(l, d)) / d
    return
  end function safe_div

!!!_  & safe_mod() - large * small % d
  ELEMENTAL integer function safe_mod (l, s, d) result(r)
    implicit none
    !  L = da + b
    ! (L * S) % d = (Sda + Sb) % d = Sb % d
    integer,intent(in) :: l, s, d
    r = mod((s * mod(l, d)), d)
    return
  end function safe_mod

!!!_  & pos_pattern ()
  ELEMENTAL integer function pos_pattern &
       & (jzone, nbits, lbits) result(r)
    implicit none
    integer,intent(in) :: jzone
    integer,intent(in) :: nbits, lbits
    r = (lbits * jzone) / nbits
    return
  end function pos_pattern

!!!_  & pos_storages ()
  ELEMENTAL integer function pos_storages &
       & (jzone, ngflr, rgrps) result(r)
    implicit none
    integer,intent(in) :: jzone
    integer,intent(in) :: ngflr, rgrps
    r = ngflr * jzone + min(jzone, rgrps)
    return
  end function pos_storages

!!!_  & pos_source ()
  ELEMENTAL integer function pos_source &
       & (jofs, ngflr, ritms) result(r)
    implicit none
    integer,intent(in) :: jofs
    integer,intent(in) :: ngflr, ritms
    r = jofs * ngflr + min(jofs, ritms)
    return
  end function pos_source

!!!_  & initial_mask ()
  ELEMENTAL integer function initial_mask &
       & (jzone, nbits, lbits) result(r)
    implicit none
    integer,intent(in) :: jzone
    integer,intent(in) :: nbits, lbits
    r = nbits - MOD(lbits * jzone, nbits)
    return
  end function initial_mask

!!!_  & popcount_tab()
  ELEMENTAL integer function popcount_tab_i(i) result(n)
    implicit none
    integer,parameter :: KITGT = KI32
    integer(kind=KITGT),intent(in) :: i
    integer,parameter :: bs = BIT_SIZE(i)
    integer,parameter :: r = 4
    integer,parameter :: rr = 2 ** r
    integer,parameter :: tab(0:rr-1) &
         & = (/0, 1, 1, 2,  1, 2, 2, 3,  1, 2, 2, 3,  2, 3, 3, 4/)
    integer j
    n = 0
    do j = 0, bs - 1, r
       n = n + tab(IBITS(i, j, r))
    enddo
  end function popcount_tab_i
  ELEMENTAL integer function popcount_tab_l(i) result(n)
    implicit none
    integer,parameter :: KITGT = KI64
    integer(kind=KITGT),intent(in) :: i
    integer,parameter :: bs = BIT_SIZE(i)
    integer,parameter :: r = 4
    integer,parameter :: rr = 2 ** r
    integer,parameter :: tab(0:rr-1) &
         & = (/0, 1, 1, 2,  1, 2, 2, 3,  1, 2, 2, 3,  2, 3, 3, 4/)
    integer j
    n = 0
    do j = 0, bs - 1, r
       n = n + tab(IBITS(i, j, r))
    enddo
  end function popcount_tab_l
!!!_ + end of TOUZA_Trp_pack
end module TOUZA_Trp_pack

!!!_@ test_trapiche_pack - test program
#if TEST_TRAPICHE_PACK
program test_trp_pack
  use TOUZA_Trp_std,only: show_pattern
  use TOUZA_Trp_pack
  implicit none
  integer ierr
  integer lbits
  integer mem, nbits
# if TEST_TRAPICHE_PACK == 64
#   define TEST_TRAPICHE_PACK_KIND KI64
# else
#   define TEST_TRAPICHE_PACK_KIND KI32
# endif
  integer,parameter :: KITEST = TEST_TRAPICHE_PACK_KIND
  integer(kind=KITEST),allocatable :: isrc(:), idest(:), irstr(:)
  integer :: loop
  integer :: kend, kstp, kini, verb
  integer jm, jb
!!!_ + usage
  !!   echo "BITS MEMBERS LOOP" | ./test_trapiche_pack_32  ! 32-bit storage
  !!   echo "BITS MEMBERS LOOP" | ./test_trapiche_pack_64  ! 64-bit storage
!!!_ + init
  call init(ierr)
  if (ierr.eq.0) call diag(ierr)
!!!_ + zones
  lbits = bit_size(int(0, kind=KITEST))
!!!_  - arguments
  if (ierr.eq.0) then
     nbits = 7
     mem = 100
     loop = 1
     verb = 1
     read(*, *, IOSTAT=ierr) nbits, mem, loop, verb
     ierr = 0
     if (nbits.ge.lbits) then
        write(*, *) 'Invalid target bits = ', nbits, lbits
        ierr = -1
     endif
     write(*, *) '#TEST: ', nbits, mem, loop, verb
  endif
!!!_  - count_zones test
  if (ierr.eq.0) then
301  format('zones: ', I0, 1x, I0, 1x, I0)
     do jb = 0, lbits
        write(*, 301) jb, lbits, count_zones(jb, lbits)
     enddo
  endif
!!!_  - create input array
  if (ierr.eq.0) allocate(isrc(0:mem-1), idest(0:mem-1), irstr(0:mem-1), STAT=ierr)
  if (ierr.eq.0) then
     kend = int(-1, kind=kind(kend))
     kini = ibits(kend, 0, nbits / 2)
     kend = ibits(kend, 0, nbits)
     call show_pattern(ierr, 'kend', kend)
     kstp = max(17, kend / max(1, mem / (11 * 13))) * 7
     call show_pattern(ierr, 'kstp', kstp)
     do jm = 0, mem - 1
        isrc(jm) = kini
        kini = ibits(kini + kstp, 0, nbits)
     enddo
     call show_props_trn(ierr, nbits, mem, lbits)
  endif
!!!_  - tests main
  if (ierr.eq.0) then
     call test_batch &
          & (ierr, irstr, idest, isrc, mem, nbits, loop, verb, 'trn', RELLENO_TRANSPOSE)
  endif
  if (ierr.eq.0) then
     call test_batch &
          & (ierr, irstr, idest, isrc, mem, nbits, loop, verb, 'seq', RELLENO_SEQUENTIAL)
  endif
  if (ierr.eq.0) then
     call test_batch &
          & (ierr, irstr, idest, isrc, mem, nbits, loop, verb, 'str', RELLENO_STRIDE)
  endif
  if (ierr.eq.0) then
     call test_batch_sp1 &
          & (ierr, irstr, idest, isrc, mem, nbits, loop, verb, 'seq/1', RELLENO_SEQUENTIAL)
  endif
  if (ierr.eq.0) then
     call test_batch_sp1 &
          & (ierr, irstr, idest, isrc, mem, nbits, loop, verb, 'str/1', RELLENO_STRIDE)
  endif
  if (ierr.eq.0) then
     call test_batch_sp1 &
          & (ierr, irstr, idest, isrc, mem, nbits, loop, verb, 'trn/1', RELLENO_TRANSPOSE)
  endif
  if (ierr.eq.0) then
     call test_batch_spdiv &
          & (ierr, irstr, idest, isrc, mem, nbits, loop, verb, 'trn/div', RELLENO_TRANSPOSE)
  endif
#if TEST_UINTPACK
  if (ierr.eq.0) then
     if (nbits.lt.32) then
        call test_batch_uip &
             & (ierr, irstr, idest, isrc, mem, nbits, loop, verb)
     endif
  endif
#endif /* TEST_UINTPACK */
  if (ierr.eq.0) then
     call test_batch_popcount(ierr, 0, 2**10, 39, verb)
  endif
# if TEST_TRAPICHE_PACK == 32
  if (ierr.eq.0) then
     call test_batch_list(ierr, (/0,32,32,  0,3,3/), verb, RELLENO_SEQUENTIAL, 3)
     call test_batch_list(ierr, (/0,32,63,  0,2,2/), verb, RELLENO_SEQUENTIAL, 3)
     call test_batch_list(ierr, (/0,32,64,  0,2,2/), verb, RELLENO_SEQUENTIAL, 3)
     call test_batch_list(ierr, (/0,32,65,  0,2,2/), verb, RELLENO_SEQUENTIAL, 3)

     call test_batch_list(ierr, (/1,4,5,  0,2,4,  2,4,4,  0,4,7/),   verb, RELLENO_SEQUENTIAL, 3)
     call test_batch_list(ierr, (/0,4,5,  0,2,4,  0,4,4,  0,4,7/),   verb, RELLENO_SEQUENTIAL, 11)

     call test_batch_list(ierr, (/0,5,5,  0,4,4,  0,9,9,  0,11,11/), verb, RELLENO_SEQUENTIAL, 7)
     call test_batch_list(ierr, (/3,4,5,  0,4,4,  0,9,9,  0,11,11/), verb, RELLENO_SEQUENTIAL, 7)
     call test_batch_list(ierr, (/0,5,5,  2,3,4,  0,9,9,  0,11,11/), verb, RELLENO_SEQUENTIAL, 7)
     call test_batch_list(ierr, (/0,5,5,  0,4,4,  5,6,9,  0,11,11/), verb, RELLENO_SEQUENTIAL, 7)
     call test_batch_list(ierr, (/0,5,5,  0,4,4,  5,6,9,  7,8,11/),  verb, RELLENO_SEQUENTIAL, 7)
  endif
  if (ierr.eq.0) then
     call test_batch_dspl(ierr, nbits, verb, 'seq', RELLENO_SEQUENTIAL)
  endif
  if (ierr.eq.0) then
     call test_batch_dspl(ierr, nbits, verb, 'str', RELLENO_STRIDE)
  endif
  if (ierr.eq.0) then
     call test_batch_dspl(ierr, nbits, verb, 'trn', RELLENO_TRANSPOSE)
  endif
#endif
  if (ierr.eq.0) then
     call finalize(ierr)
  endif
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop

contains
  subroutine test_batch &
       & (ierr,  irstr, idest, isrc, mem, nbits, loop, verb, &
       &  tag,   kpack)
    implicit none
    integer,             intent(out) :: ierr
    integer(kind=KITEST),intent(out) :: irstr(*)
    integer(kind=KITEST),intent(out) :: idest(*)
    integer(kind=KITEST),intent(in)  :: isrc(*)
    integer,             intent(in)  :: mem
    integer,             intent(in)  :: nbits
    integer,             intent(in)  :: loop
    integer,             intent(in)  :: kpack
    integer,             intent(in)  :: verb
    character(len=*),    intent(in)  :: tag
    integer l
    integer ncbgn, ncend, nr

    ierr = 0
    idest(1:mem) = 0
    irstr(1:mem) = 0
    call system_clock(ncbgn, nr)
    do l = 1, loop
       call pack_store  (ierr, idest, isrc,  mem, nbits, kpack)
       call pack_restore(ierr, irstr, idest, mem, nbits, kpack)
    enddo
    call system_clock(ncend)
    call check_results(ierr, irstr, idest, isrc, mem, nbits, tag, ncbgn, ncend, nr, verb)
    return
  end subroutine test_batch

  subroutine test_batch_sp1 &
       & (ierr,  irstr, idest, isrc, mem, nbits, loop, verb, &
       &  tag,   kpack)
    implicit none
    integer,             intent(out) :: ierr
    integer(kind=KITEST),intent(out) :: irstr(*)
    integer(kind=KITEST),intent(out) :: idest(*)
    integer(kind=KITEST),intent(in)  :: isrc(*)
    integer,             intent(in)  :: mem
    integer,             intent(in)  :: nbits
    integer,             intent(in)  :: loop
    integer,             intent(in)  :: verb
    integer,             intent(in)  :: kpack
    character(len=*),    intent(in)  :: tag
    integer l
    integer ncbgn, ncend, nr

    ierr = 0
    if (nbits.ne.1) return
    idest(1:mem) = 0
    irstr(1:mem) = 0
    if (kpack.eq.RELLENO_TRANSPOSE) then
       call system_clock(ncbgn, nr)
       do l = 1, loop
          call pack_store_trn_sp1(ierr, idest, isrc,  mem, nbits)
          call pack_restore_trn_sp1(ierr, irstr, idest, mem, nbits)
       enddo
       call system_clock(ncend)
    else if (kpack.eq.RELLENO_SEQUENTIAL) then
       call system_clock(ncbgn, nr)
       do l = 1, loop
          call pack_store_seq_sp1(ierr, idest, isrc,  mem, nbits)
          call pack_restore_seq_sp1(ierr, irstr, idest, mem, nbits)
       enddo
       call system_clock(ncend)
    else if (kpack.eq.RELLENO_STRIDE) then
       call system_clock(ncbgn, nr)
       do l = 1, loop
          call pack_store_str_sp1(ierr, idest, isrc,  mem, nbits)
          call pack_restore_str_sp1(ierr, irstr, idest, mem, nbits)
       enddo
       call system_clock(ncend)
    else
       return
    endif
    call check_results(ierr, irstr, idest, isrc, mem, nbits, tag, ncbgn, ncend, nr, verb)
    return
  end subroutine test_batch_sp1

  subroutine test_batch_spdiv &
       & (ierr,  irstr, idest, isrc, mem, nbits, loop, verb, &
       &  tag,   kpack)
    implicit none
    integer,             intent(out) :: ierr
    integer(kind=KITEST),intent(out) :: irstr(*)
    integer(kind=KITEST),intent(out) :: idest(*)
    integer(kind=KITEST),intent(in)  :: isrc(*)
    integer,             intent(in)  :: mem
    integer,             intent(in)  :: nbits
    integer,             intent(in)  :: loop
    integer,             intent(in)  :: kpack
    integer,             intent(in)  :: verb
    character(len=*),    intent(in)  :: tag
    integer l
    integer,parameter :: lbits = BIT_SIZE(0_KITEST)
    integer ncbgn, ncend, nr

    ierr = 0
    ! write(*, *) 'SPdiv', lbits, nbits

    if (mod(lbits, nbits).ne.0) return
    idest(1:mem) = 0
    irstr(1:mem) = 0
    call system_clock(ncbgn, nr)
    do l = 1, loop
       call pack_store_trn_spdiv(ierr, idest, isrc,  mem, nbits)
       call pack_restore_trn_spdiv(ierr, irstr, idest, mem, nbits)
    enddo
    call system_clock(ncend)
    call check_results(ierr, irstr, idest, isrc, mem, nbits, tag, ncbgn, ncend, nr, verb)
    return
  end subroutine test_batch_spdiv

#if TEST_UINTPACK
  subroutine test_batch_uip &
       & (ierr,  irstr, idest, isrc, mem, nbits, loop, verb)
    implicit none
    integer,             intent(out) :: ierr
    integer(kind=KITEST),intent(out) :: irstr(*)
    integer(kind=KITEST),intent(out) :: idest(*)
    integer(kind=KITEST),intent(in)  :: isrc(*)
    integer,             intent(in)  :: mem
    integer,             intent(in)  :: nbits
    integer,             intent(in)  :: loop
    integer,             intent(in)  :: verb
    integer l
    integer klen
    integer ncbgn, ncend, nr

    ierr = 0
    idest(1:mem) = 0
    irstr(1:mem) = 0
    call system_clock(ncbgn, nr)
    do l = 1, loop
       call PACK_BITS_INTO32(idest, klen, isrc,  mem, nbits)
       call UNPACK_BITS_FROM32(irstr, mem, idest,  nbits)
    enddo
    call system_clock(ncend)
    call check_results(ierr, irstr, idest, isrc, mem, nbits, 'uip', ncbgn, ncend, nr, verb)
    return
  end subroutine test_batch_uip
#endif /* TEST_UINTPACK */

  subroutine check_results &
       & (ierr, irstr, idest, isrc, mem, nbits, tag, ncb, nce, nr, verb)
    use TOUZA_Trp_std,only: binstr
    implicit none
    integer,             intent(out) :: ierr
    integer(kind=KITEST),intent(in)  :: irstr(*)
    integer(kind=KITEST),intent(in)  :: idest(*)
    integer(kind=KITEST),intent(in)  :: isrc(*)
    integer,             intent(in)  :: mem
    integer,             intent(in)  :: nbits
    character(len=*),    intent(in)  :: tag
    integer,             intent(in)  :: ncb, nce, nr
    integer,             intent(in)  :: verb

    character(len=128) :: T, TS, TR
    integer mpack
    integer jp, jm
    logical CHK

    ierr = 0

    mpack = count_packed(nbits, mem, idest(1))
301 format('packed:', I0, ': ', A)
302 format('source:', I0, ': ', A, 1x, A, 1x, L1)
303 format('success: ', A, 1x, I0, 1x, I0)
304 format('failed: ', A)
    if (verb.gt.1) then
       do jp = 1, mpack
          call binstr(T, idest(jp))
          write(*, 301) jp, trim(T)
       enddo
    endif
    CHK = ANY(isrc(1:mem).ne.irstr(1:mem))
    if (CHK) then
       write(*, 304) trim(tag)
       do jm = 1, mem
          call binstr(TS, isrc(jm), M=nbits)
          call binstr(TR, irstr(jm), M=nbits)
          CHK = isrc(jm).ne.irstr(jm)
          if (CHK) then
             write(*, 302) jm-1, trim(TS), trim(TR), CHK
          endif
       enddo
       ierr = -1
    else
       write(*, 303) trim(tag), nce - ncb, nr
    endif

    return
  end subroutine check_results

  subroutine test_batch_popcount &
       & (ierr,  jbgn, jend, jstp, verb)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: jbgn, jend, jstp
    integer,intent(in)  :: verb
    integer(kind=TEST_TRAPICHE_PACK_KIND) j
    integer ptab, pref

    ierr = 0
301 format('popcount: ', I0, 1x, L1, 1x, B16.16, 1x, I0, 1x, I0)
    do j = jbgn, jend, jstp
       ptab = popcount_tab(j)
       pref = popcount_ref(j)
       if (verb.gt.0.or.ptab.ne.pref) then
          write(*, 301) j, ptab.eq.pref, j, ptab, pref
       endif
    enddo
  end subroutine test_batch_popcount

  ELEMENTAL integer function popcount_ref(i) result(n)
    implicit none
    integer(kind=TEST_TRAPICHE_PACK_KIND),intent(in) :: i
    integer,parameter :: bs = BIT_SIZE(i)
    integer j
    n = 0
    do j = 0, bs - 1
       n = n + INT(IBITS(i, j, 1))
    enddo
  end function popcount_ref

  subroutine test_batch_list(ierr, bchk, verb, kpack, mdl)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: bchk(:)
    integer,intent(in)  :: verb
    integer,intent(in)  :: kpack
    integer,intent(in)  :: mdl
    integer jr, nr
    integer nidx
    integer j, jg
    integer mpack
    character(len=32) :: buf0, buf1

    integer,allocatable :: imask(:),  ival(:)
    integer,allocatable :: ibagaz(:), iextr(:), iref(:)
    integer,allocatable :: ldst(:),   lsrc(:)
    integer,parameter :: lr = 8
    integer idx(0:lr-1), bes(3, 0:lr-1)
    integer :: gmem, lmem
    integer jp, jl, km

    ierr = 0
    nr = size(bchk)
    if (mod(nr, 3).ne.0) then
       ierr = -1
       return
    endif
    nr = nr / 3
    do jr = 0, nr - 1
       bes(1:3, jr) = bchk(3*jr+1:3*jr+3)
    enddo
    do jr = 0, nr - 1
       write(*, *) 'bes = ', jr, bes(:, jr)
    enddo

    gmem = product(bes(3,0:nr-1))
    lmem = product(bes(2,0:nr-1) - bes(1,0:nr-1))

    mpack = count_packed(1, gmem, int(0, kind=KIND(imask)))
    write(*, *) 'mem = ', gmem, mpack
    if (ierr.eq.0) allocate(imask(0:gmem-1), ival(0:gmem-1), ibagaz(0:mpack-1), STAT=ierr)
    if (ierr.eq.0) allocate(iref(0:lmem-1), iextr(0:lmem-1), STAT=ierr)
    if (ierr.eq.0) allocate(ldst(0:gmem-1), lsrc(0:gmem-1), STAT=ierr)

    if (ierr.eq.0) then
       iref(:) = -1
       jp = 0
       jl = 0
       do j = 0, gmem - 1
          jg = j
          do jr = 0, nr - 1
             idx(jr) = mod(jg, bes(3, jr))
             jg = jg / bes(3, jr)
          enddo
          km = 0
          if (mod(j, mdl).eq.0) km = 1
          imask(j) = km
          if (km.gt.0) then
             ival(jp) = j
             jp = jp + 1
          endif
          if (ALL(bes(1,0:nr-1).le.idx(0:nr-1) .and. idx(0:nr-1).lt.bes(2,0:nr-1))) then
             if (km.gt.0) iref(jl) = j
             jl = jl + 1
          endif
       enddo
    endif
    if (ierr.eq.0) call pack_store(ierr, ibagaz, imask, gmem, 1, kpack)

    if (ierr.eq.0) then
       call mask_to_idxl_seq(ierr, ldst, lsrc, nidx, ibagaz, gmem, bes(1:3,0:nr-1), nr)
    endif
    if (ierr.eq.0) then
       do j = 0, nidx - 1
101       format('list: ', I0, 1x, I0, ' < ', I0)
          write(*, 101) j, ldst(j), lsrc(j)
       enddo
111    format('list:', I0, 1x, B32.32)
       do j = 0, mpack - 1
          write(*, 111) j, ibagaz(j)
       enddo
       iextr(:) = -1
       do jp = 0, nidx - 1
          iextr(ldst(jp)) = ival(lsrc(jp))
       enddo
       if (ALL(iref(0:lmem-1).eq.iextr(0:lmem-1))) then
122       format('list/result: success')
          write(*, 122)
       else
121       format('list/result:', I0, 1x, I0, 1x, I0)
          do jp = 0, lmem - 1
             if (iref(jp).ne.iextr(jp)) then
                write(*, 121) jp, iextr(jp), iref(jp)
             endif
          enddo
       endif
    endif
  end subroutine test_batch_list

  subroutine test_batch_dspl(ierr, nbits, verb, tag, kpack)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: nbits
    integer,         intent(in)  :: verb
    character(len=*),intent(in)  :: tag
    integer,         intent(in)  :: kpack

    ierr = 0
    if (ierr.eq.0) call test_dspl(ierr, 3, (/0,11,22,  0,3,3/), verb, tag, kpack)
    if (ierr.eq.0) call test_dspl(ierr, 3, (/0,11,21,  0,3,3/), verb, tag, kpack)

    if (ierr.eq.0) call test_dspl(ierr, nbits, (/0,32,32,  0,3,3/), verb, tag, kpack)
    if (ierr.eq.0) call test_dspl(ierr, nbits, (/0,31,32,  1,2,3/), verb, tag, kpack)
    if (ierr.eq.0) call test_dspl(ierr, nbits, (/0,32,63,  0,2,2/), verb, tag, kpack)
    if (ierr.eq.0) call test_dspl(ierr, nbits, (/0,32,64,  0,2,2/), verb, tag, kpack)
    if (ierr.eq.0) call test_dspl(ierr, nbits, (/0,32,65,  0,2,2/), verb, tag, kpack)
    if (ierr.eq.0) call test_dspl(ierr, nbits, (/0,24,32,  0,3,3/), verb, tag, kpack)
    if (ierr.eq.0) call test_dspl(ierr, nbits, (/0,24,32,  1,3,3,  0,2,2/), verb, tag, kpack)
    if (ierr.eq.0) call test_dspl(ierr, nbits, (/1,25,32,  1,3,3,  0,2,2/), verb, tag, kpack)

   end subroutine test_batch_dspl

  subroutine test_dspl(ierr, nbits, bchk, verb, tag, kpack)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: nbits
    integer,         intent(in)  :: bchk(:)
    integer,         intent(in)  :: verb
    character(len=*),intent(in)  :: tag
    integer,         intent(in)  :: kpack
    integer jr, nr
    integer,allocatable:: lofd(:, :), dsrc(:, :)
    integer,allocatable:: lofr(:, :), rsrc(:)

    integer,allocatable:: dunp(:, :), rfil(:)
    integer nunp, nfil

    integer nsrc
    integer,allocatable:: lsrc(:)
    integer,allocatable:: dunp2(:, :), rfil2(:)
    integer nunp2, nfil2

    integer j, nd, nu, jg
    integer mpack
    integer,parameter :: mold = 0

    integer,allocatable :: ival(:)
    integer,allocatable :: ibagaz(:), iextr(:), iref(:), ircv(:)
    integer,parameter :: lr = 8
    integer idx(0:lr-1), bes(3, 0:lr-1)
    integer :: gmem, lmem
    integer jp, jl, js, ms
    integer mdl

    ierr = 0
    mdl = 2 ** nbits - 2

    nr = size(bchk)
    if (mod(nr, 3).ne.0) then
       ierr = -1
       return
    endif
    nr = nr / 3
    do jr = 0, nr - 1
       bes(1:3, jr) = bchk(3*jr+1:3*jr+3)
    enddo
101 format('dspl:test/', A, 1x, I0, 1x, I0, ':', I0, '/', I0)
    do jr = 0, nr - 1
       write(*, 101) tag, jr, bes(:, jr)
    enddo

    gmem = product(bes(3,0:nr-1))
    lmem = product(bes(2,0:nr-1) - bes(1,0:nr-1))

    mpack = count_packed(nbits, gmem, int(0, kind=KIND(ival)))
102 format('dspl:set/', A, 1x, I0, 1x, I0, 1x, I0)
    write(*, 102) tag, gmem, mpack, nbits
    if (ierr.eq.0) allocate(ival(0:gmem-1), ibagaz(0:mpack-1), lsrc(0:gmem-1), STAT=ierr)
    if (ierr.eq.0) allocate(iref(0:lmem-1), ircv(0:lmem-1), iextr(0:gmem-1), STAT=ierr)
    if (ierr.eq.0) allocate(lofd(2, 0:gmem), lofr(2, 0:gmem), STAT=ierr)
    if (ierr.eq.0) allocate(dsrc(2, 0:gmem),  rsrc(0:2*gmem),  STAT=ierr)
    if (ierr.eq.0) allocate(dunp(npropd, 0:gmem),  rfil(0:2*gmem),  STAT=ierr)
    if (ierr.eq.0) allocate(dunp2(npropd, 0:gmem), rfil2(0:2*gmem), STAT=ierr)

    if (ierr.eq.0) then
       iref(:) = -1
       jp = 0
       jl = 0
       nsrc = 0
       do j = 0, gmem - 1
          jg = j
          do jr = 0, nr - 1
             idx(jr) = mod(jg, bes(3, jr))
             jg = jg / bes(3, jr)
          enddo
          if (ALL(bes(1,0:nr-1).le.idx(0:nr-1) .and. idx(0:nr-1).lt.bes(2,0:nr-1))) then
             lsrc(nsrc) = j
             ival(j) = mdl
             iref(jl) = mdl
             nsrc = nsrc + 1
             jl = jl + 1
          else
             ival(j) = 0
          endif
       enddo
    endif
    if (ierr.eq.0) call pack_store(ierr, ibagaz, ival, gmem, nbits, kpack)
    if (kpack.eq.RELLENO_TRANSPOSE) then
       if (ierr.eq.0) then
          call gen_bfc_slice &
               & (ierr, dunp, nunp, rfil, nfil, nbits, gmem, bes(1:3,0:nr-1), nr, kpack, mold)
       endif
    else
       if (ierr.eq.0) then
          call pack_gen_dspl &
               & (ierr, lofd, dsrc, nd, nbits, gmem, bes(1:3,0:nr-1), nr, kpack, mold)
          call pack_gen_runl &
               & (ierr, lofr, rsrc, nu, nbits, gmem, bes(1:3,0:nr-1), nr, kpack, mold)
          call gen_bfc_slice &
               & (ierr, dunp, nunp, rfil, nfil, nbits, gmem, bes(1:3,0:nr-1), nr, kpack, mold)
          call gen_bfc_idxl &
               & (ierr, dunp2, nunp2, rfil2, nfil2, nbits, gmem, lsrc, nsrc, kpack, mold)
       endif
    endif
    if (ierr.eq.0) then
111    format('dspl:dspl/', A, 1x, I0, 1x, I0, '/', I0, ' < ', I0, ':+', I0)
       do j = 0, nd - 1
          write(*, 111) tag, j, lofd(1:2, j), dsrc(:, j)
       enddo
       write(*, 111) tag, nd, lofd(1:2, nd)

112    format('dspl:runl/', A, 1x, I0, 1x, I0, '/', I0, ' < ', '+', I0, ':+', I0)
       do j = 0, nu - 1, 2
          write(*, 112) tag, j / 2, lofr(1:2, j / 2), rsrc(j:j+1)
       enddo
       write(*, 112) tag, nu / 2, lofr(1:2, nu / 2)

118    format('dspl:bfc/r/', A, 1x, I0, ' +', I0, ':+', I0)
       do j = 0, nfil - 1, 2
          write(*, 118) tag, j / 2, rfil(j:j+1)
       enddo
       write(*, 118) tag, nfil, rfil(nfil)
117    format('dspl:bfc/d/', A, 1x, I0, 1x, I0, ' < ', I0, '+', I0)
       do j = 0, nunp
          write(*, 117) tag, j, dunp(1, j), dunp(2:3, j)
       enddo

128    format('dspl:bfc/r2/', A, 1x, I0, ' +', I0, ':+', I0)
       do j = 0, nfil2 - 1, 2
          write(*, 128) tag, j / 2, rfil2(j:j+1)
       enddo
       write(*, 128) tag, nfil2, rfil2(nfil2)
127    format('dspl:bfc/d2/', A, 1x, I0, 1x, I0, ' < ', I0, '+', I0)
       do j = 0, nunp2
          write(*, 127) tag, j, dunp2(1, j), dunp2(2:3, j)
       enddo
       write(*, *) 'dspl/bfc/r/check: ', nfil2.eq.nfil, ALL(rfil2(0:nfil).eq.rfil(0:nfil))
       write(*, *) 'dspl/bfc/d/check: ', nunp2.eq.nunp, ALL(dunp(:,0:nunp).eq.dunp2(:,0:nunp))

113    format('dspl:src/', A, 1x, I0, 1x, B32.32)
       do j = 0, mpack - 1
          write(*, 113) tag, j, ibagaz(j)
       enddo
       iextr(:) = -1
       jp = 0
       js = 0
       do j = 0, nu - 1, 2
          js = js + rsrc(j)
          ms = rsrc(j+1)
          iextr(jp:jp+ms-1) = ibagaz(js:js+ms-1)
          js = js + ms
          jp = jp + ms
       enddo
       do j = 0, jp - 1
114       format('dspl:filter/', A, 1x, I4, 1x, B32.32)
          write(*, 114) tag, j, iextr(j)
       enddo
    endif
    if (ierr.eq.0) then
       call pack_restore_slice &
            & (ierr, ircv,  &
            &  iextr, gmem, nbits, kpack, &
            &  lofr,  rsrc, nu,    bes,   nr)
    endif
    if (ierr.eq.0) then
       if (ALL(ircv(0:lmem-1).eq.iref(0:lmem-1))) then
122       format('dspl:result/', A, ' success')
          write(*, 122) tag
       else
121       format('dspl:result/', A, 1x, I0, 1x, I0, 1x, I0)
          do jp = 0, lmem - 1
             if (ircv(jp).ne.iref(jp)) then
                write(*, 121) tag, jp, ircv(jp), iref(jp)
             endif
          enddo
       endif
    endif
    if (ierr.eq.0) then
       ircv(:) = -1
       call pack_restore_dunp &
            & (ierr,  ircv,  &
            &  iextr, gmem, nbits, kpack, &
            &  dunp,  nunp)
    endif
    if (ierr.eq.0) then
       if (ALL(ircv(0:lmem-1).eq.iref(0:lmem-1))) then
132       format('dspl:result/bfc/', A, ' success')
          write(*, 132) tag
       else
131       format('dspl:result/bfc/', A, 1x, I0, 1x, I0, 1x, I0)
          do jp = 0, lmem - 1
             if (ircv(jp).ne.iref(jp)) then
                write(*, 131) tag, jp, ircv(jp), iref(jp)
             endif
          enddo
       endif
    endif
  end subroutine test_dspl

end program test_trp_pack
!!!_   . procedure for debug
subroutine bes_idx(txt, jg, bes, nr, lbits)
  use TOUZA_Std_utl,only: join_list
  implicit none
  character(len=*),intent(out) :: txt
  integer,         intent(in)  :: jg
  integer,         intent(in)  :: bes(3,0:*)
  integer,         intent(in)  :: nr
  integer,optional,intent(in)  :: lbits
  integer :: idx(0:nr-1)
  integer jt, jr
  integer jerr
  character(len=64) :: buf
  jt = jg
  do jr = 0, nr - 1
     idx(jr) = mod(jt, bes(3, jr))
     jt      = jt / bes(3, jr)
  enddo
  call join_list(jerr, txt, idx, sep=',', ldelim='[', rdelim=']')
  if (present(lbits)) then
101  format('(', I0, '+', I0, ')')
     write(buf, 101) jg / lbits, mod(jg, lbits)
     txt = trim(txt) // trim(buf)
  endif
end subroutine bes_idx

#endif /* TEST_TRAPICHE_PACK */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

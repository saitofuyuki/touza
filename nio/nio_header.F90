!!!_! nio_header.F90 - TOUZA/Nio header sub records
! Maintainer: SAITO Fuyuki
! Created: Oct 21 2021
#define TIME_STAMP 'Time-stamp: <2023/03/07 14:00:08 fuyuki nio_header.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021, 2022, 2023
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_nio.h"
!!!_@ TOUZA_Nio_header - Nio header-record interfaces
module TOUZA_Nio_header
!!!_ = declaration
  use TOUZA_Nio_std,only: KFLT, KDBL
  use TOUZA_Nio_std,only: control_mode, control_deep, is_first_force
  use TOUZA_Nio_std,only: get_logu,     unit_global,  trace_fine,   trace_control
  implicit none
  private
!!!_  - public parameters
!!!_   . items
  integer,parameter,public :: nitem = 64
  integer,parameter,public :: litem = 16

  integer,parameter,public :: lhead = litem * nitem

  integer,parameter,public :: hi_IDFM   = 1
  integer,parameter,public :: hi_DSET   = 2
  integer,parameter,public :: hi_ITEM   = 3
  integer,parameter,public :: hi_EDIT1  = 4
  integer,parameter,public :: hi_EDIT2  = 5
  integer,parameter,public :: hi_EDIT3  = 6
  integer,parameter,public :: hi_EDIT4  = 7
  integer,parameter,public :: hi_EDIT5  = 8

  integer,parameter,public :: hi_EDIT6  = 9
  integer,parameter,public :: hi_EDIT7  = 10
  integer,parameter,public :: hi_EDIT8  = 11
  integer,parameter,public :: hi_FNUM   = 12
  integer,parameter,public :: hi_DNUM   = 13
  integer,parameter,public :: hi_TITL1  = 14
  integer,parameter,public :: hi_TITL2  = 15
  integer,parameter,public :: hi_UNIT   = 16

  integer,parameter,public :: hi_ETTL1  = 17
  integer,parameter,public :: hi_ETTL2  = 18
  integer,parameter,public :: hi_ETTL3  = 19
  integer,parameter,public :: hi_ETTL4  = 20
  integer,parameter,public :: hi_ETTL5  = 21
  integer,parameter,public :: hi_ETTL6  = 22
  integer,parameter,public :: hi_ETTL7  = 23
  integer,parameter,public :: hi_ETTL8  = 24

  integer,parameter,public :: hi_TIME   = 25
  integer,parameter,public :: hi_UTIM   = 26
  integer,parameter,public :: hi_DATE   = 27
  integer,parameter,public :: hi_TDUR   = 28
  integer,parameter,public :: hi_AITM1  = 29
  integer,parameter,public :: hi_ASTR1  = 30
  integer,parameter,public :: hi_AEND1  = 31
  integer,parameter,public :: hi_AITM2  = 32

  integer,parameter,public :: hi_ASTR2  = 33
  integer,parameter,public :: hi_AEND2  = 34
  integer,parameter,public :: hi_AITM3  = 35
  integer,parameter,public :: hi_ASTR3  = 36
  integer,parameter,public :: hi_AEND3  = 37
  integer,parameter,public :: hi_DFMT   = 38
  integer,parameter,public :: hi_MISS   = 39
  integer,parameter,public :: hi_DMIN   = 40

  integer,parameter,public :: hi_DMAX   = 41
  integer,parameter,public :: hi_DIVS   = 42
  integer,parameter,public :: hi_DIVL   = 43
  integer,parameter,public :: hi_STYP   = 44
  integer,parameter,public :: hi_COPTN  = 45
  integer,parameter,public :: hi_IOPTN  = 46
  integer,parameter,public :: hi_ROPTN  = 47
  integer,parameter,public :: hi_TIME2  = 48

  integer,parameter,public :: hi_UTIM2  = 49
  integer,parameter,public :: hi_MEMO1  = 50
  integer,parameter,public :: hi_MEMO2  = 51
  integer,parameter,public :: hi_MEMO3  = 52
  integer,parameter,public :: hi_MEMO4  = 53
  integer,parameter,public :: hi_MEMO5  = 54
  integer,parameter,public :: hi_MEMO6  = 55
  integer,parameter,public :: hi_MEMO7  = 56

  integer,parameter,public :: hi_MEMO8  = 57
  integer,parameter,public :: hi_MEMO9  = 58
  integer,parameter,public :: hi_MEMO10 = 59
  integer,parameter,public :: hi_CDATE  = 60
  integer,parameter,public :: hi_CSIGN  = 61
  integer,parameter,public :: hi_MDATE  = 62
  integer,parameter,public :: hi_MSIGN  = 63
  integer,parameter,public :: hi_SIZE   = 64

  ! miroc definition (different from gtool original)
  integer,parameter,public :: hi_DATE1  = 48
  integer,parameter,public :: hi_DATE2  = 49

  ! miroc definition is adopted
  integer,parameter :: mask_wide = max(128, nitem) * 2
  integer,parameter :: lname = 6
  character(len=lname),parameter,public :: hf_names(1:nitem) = &
       & (/ 'IDFM  ', 'DSET  ', 'ITEM  ', 'EDIT1 ', 'EDIT2 ', 'EDIT3 ', 'EDIT4 ', 'EDIT5 ', &
       &    'EDIT6 ', 'EDIT7 ', 'EDIT8 ', 'FNUM  ', 'DNUM  ', 'TITL1 ', 'TITL2 ', 'UNIT  ', &
       &    'ETTL1 ', 'ETTL2 ', 'ETTL3 ', 'ETTL4 ', 'ETTL5 ', 'ETTL6 ', 'ETTL7 ', 'ETTL8 ', &
       &    'TIME  ', 'UTIM  ', 'DATE  ', 'TDUR  ', 'AITM1 ', 'ASTR1 ', 'AEND1 ', 'AITM2 ', &
       &    'ASTR2 ', 'AEND2 ', 'AITM3 ', 'ASTR3 ', 'AEND3 ', 'DFMT  ', 'MISS  ', 'DMIN  ', &
       &    'DMAX  ', 'DIVS  ', 'DIVL  ', 'STYP  ', 'COPTN ', 'IOPTN ', 'ROPTN ', 'DATE1 ', &
       &    'DATE2 ', 'MEMO1 ', 'MEMO2 ', 'MEMO3 ', 'MEMO4 ', 'MEMO5 ', 'MEMO6 ', 'MEMO7 ', &
       &    'MEMO8 ', 'MEMO9 ', 'MEMO10', 'CDATE ', 'CSIGN ', 'MDATE ', 'MSIGN ', 'SIZE  ' /)

  character(len=*),parameter :: def_fmt_I = '(I16)'
  character(len=*),parameter :: def_fmt_R = '(E16.7)'
  character(len=*),parameter :: def_fmt_date_trad = '(I4.4,I2.2,I2.2,1X,I2.2,I2.2,I2.2)'
  character(len=*),parameter :: def_fmt_date_long = '(I5.5,I2.2,I2.2,1X,I2.2,I2.2,I2.2)'
  character(len=*),parameter :: def_fmt_date_full = '(I6.6,I2.2,I2.2,   I2.2,I2.2,I2.2)'

  integer,parameter :: ht_str    = 0
  integer,parameter :: ht_int    = 1
  integer,parameter :: ht_real   = 2
  integer,parameter :: ht_date   = 3
!!!_  - private static
  integer,save :: hitypes(nitem) = ht_str
  integer,save :: hiends(nitem)  = -1
  integer,save :: hh_items = -1
!!!_  - static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = NIO_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global

#define __MDL__ 'h'
#define _ERROR(E) (E - ERR_MASK_NIO_HEADER)
!!!_  - interfaces
  interface put_item
     module procedure put_item_a,    put_item_na
     module procedure put_item_i,    put_item_ni
     module procedure put_item_f,    put_item_nf
     module procedure put_item_d,    put_item_nd
     module procedure put_item_date, put_item_ndate
  end interface put_item

  interface get_item
     module procedure get_item_a,    get_item_na
     module procedure get_item_i,    get_item_ni
     module procedure get_item_f,    get_item_nf
     module procedure get_item_d,    get_item_nd
     module procedure get_item_date, get_item_ndate
  end interface get_item

  interface store_item
     module procedure store_item_a,    store_item_na
     module procedure store_item_i,    store_item_ni
     module procedure store_item_f,    store_item_nf
     module procedure store_item_d,    store_item_nd
     module procedure store_item_date, store_item_ndate
  end interface store_item

  interface restore_item
     module procedure restore_item_a,    restore_item_na
     module procedure restore_item_i,    restore_item_ni
     module procedure restore_item_f,    restore_item_nf
     module procedure restore_item_d,    restore_item_nd
     module procedure restore_item_date, restore_item_ndate
  end interface restore_item

!!!_  - public procedures
  public init, diag, finalize
  public put_item,   put_item_date, store_item
  public get_item,   get_item_date, restore_item
  public fill_header
  public show_header
!!!_  - todo notes
  ! subroutine append_item
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm)
    use TOUZA_Nio_std,only: choice, ns_init=>init
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
       if (is_first_force(init_counts, md)) then
          ulog = choice(ulog, u)
          lev_verbose = lv
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ns_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
       endif
       if (is_first_force(init_counts, md)) then
          if (ierr.eq.0) call set_def_types(ierr, hitypes)
          if (ierr.eq.0) call set_def_ranges(ierr, hiends)
          if (ierr.eq.0) call set_def_tables(ierr, hiends)
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = _ERROR(ERR_FAILURE_INIT)
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Nio_std,only: choice, msg, ns_diag=>diag, is_msglev_normal
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
       if (is_first_force(diag_counts, md)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, __MDL__, utmp)
          endif
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ns_diag(ierr, utmp, levv=lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Nio_std,only: ns_finalize=>finalize, choice
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
       if (is_first_force(fine_counts, md)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call ns_finalize(ierr, utmp, levv=lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
  end subroutine finalize

!!!_ + user interfaces
!!!_  - show_header - diag entries
  subroutine show_header &
       & (ierr, head, tag, u, lev)
    use TOUZA_Nio_std,only: choice, choice_a
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: lev
    integer ulog
    integer ji
    character(len=64) :: ti
    ierr = 0

    ulog = choice(-1, u)
    call choice_a(ti, ' ', tag)

101 format(I2, 1x, A)
102 format(A,  1x, I2, 1x, A)
    if (ulog.ge.0) then
       if (ti.eq.' ') then
          do ji = 1, nitem
             if (head(ji).ne.' ') then
                write (ulog, 101) ji, trim(head(ji))
             endif
          enddo
       else
          do ji = 1, nitem
             if (head(ji).ne.' ') then
                write (ulog, 102) trim(ti), ji, trim(head(ji))
             endif
          enddo
       endif
    else if (ulog.eq.-1) then
       if (ti.eq.' ') then
          do ji = 1, nitem
             if (head(ji).ne.' ') then
                write (*, 101) ji, trim(head(ji))
             endif
          enddo
       else
          do ji = 1, nitem
             if (head(ji).ne.' ') then
                write (*, 102) trim(ti), ji, trim(head(ji))
             endif
          enddo
       endif
    endif
    return
  end subroutine show_header

!!!_  - put_item - set entry (with type check)
  subroutine put_item_a &
       & (ierr, head, v, item, iteme, fmt, tol)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    character(len=*),intent(in)          :: v
    integer,         intent(in)          :: item
    integer,         intent(in),optional :: iteme ! (optional) end entry for long value
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol   ! tolerance
    integer jend
    if (present(iteme)) then
       jend = check_hitem_range(item, iteme)
       if (jend.gt.0) then
          call store_item(ierr, head, v, item, jend, fmt, tol)
       else if (jend.eq.0) then
          call store_item(ierr, head, v, item, fmt=fmt, tol=tol)
       else
          ierr = _ERROR(ERR_HITEM_INVALID_RANGE)
       endif
    else
       ierr = check_hitem_type(item, ht_str)
       if (ierr.eq.0) call store_item(ierr, head, v, item, iteme, fmt, tol)
    endif
    return
  end subroutine put_item_a
  subroutine put_item_i &
       & (ierr, head, v, item, fmt, tol)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    integer,         intent(in)          :: v
    integer,         intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol   ! tolerance
    ierr = check_hitem_type(item, ht_int)
    if (ierr.eq.0) call store_item(ierr, head, v, item, fmt, tol)
    return
  end subroutine put_item_i
  subroutine put_item_f &
       & (ierr, head, v, item, fmt, tol)
    use TOUZA_Nio_std,only: KFLT
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    real(kind=KFLT), intent(in)          :: v
    integer,         intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol   ! tolerance
    ierr = check_hitem_type(item, ht_real)
    if (ierr.eq.0) call store_item(ierr, head, v, item, fmt, tol)
    return
  end subroutine put_item_f
  subroutine put_item_d &
       & (ierr, head, v, item, fmt, tol)
    use TOUZA_Nio_std,only: KDBL
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    real(kind=KDBL), intent(in)          :: v
    integer,         intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol   ! tolerance
    ierr = check_hitem_type(item, ht_real)
    if (ierr.eq.0) call store_item(ierr, head, v, item, fmt, tol)
    return
  end subroutine put_item_d
  subroutine put_item_date &
       & (ierr, head, dt, item, fmt, tol)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    integer,         intent(in)          :: dt(:)
    integer,         intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol   ! tolerance
    ierr = check_hitem_types(item, (/ht_date, ht_str/))
    if (ierr.eq.0) call store_item(ierr, head, dt, item, fmt, tol)
    return
  end subroutine put_item_date

!!!_  - get_item - get entry (with type check)
  subroutine get_item_a &
       & (ierr, head, v, item, iteme, fmt)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    character(len=*),intent(out)         :: v
    integer,         intent(in)          :: item
    integer,         intent(in),optional :: iteme ! (optional) end entry for long value
    character(len=*),intent(in),optional :: fmt
    integer jend
    if (present(iteme)) then
       jend = check_hitem_range(item, iteme)
       if (jend.gt.0) then
          call restore_item(ierr, head, v, item, jend, fmt)
       else if (jend.eq.0) then
          call restore_item(ierr, head, v, item, fmt=fmt)
       else
          ierr = _ERROR(ERR_HITEM_INVALID_RANGE)
       endif
    else
       ierr = check_hitem_type(item, ht_str)
       if (ierr.eq.0) call restore_item(ierr, head, v, item, iteme, fmt)
    endif
    return
  end subroutine get_item_a
  subroutine get_item_i &
       & (ierr, head, v, item, fmt, def)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    integer,         intent(out)         :: v
    integer,         intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: def
    ierr = check_hitem_type(item, ht_int)
    if (ierr.eq.0) call restore_item(ierr, head, v, item, fmt, def)
    return
  end subroutine get_item_i
  subroutine get_item_f &
       & (ierr, head, v, item, fmt, def)
    use TOUZA_Nio_std,only: KFLT
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    real(kind=KFLT), intent(out)         :: v
    integer,         intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    real(kind=KFLT), intent(in),optional :: def
    ierr = check_hitem_type(item, ht_real)
    if (ierr.eq.0) call restore_item(ierr, head, v, item, fmt, def)
    return
  end subroutine get_item_f
  subroutine get_item_d &
       & (ierr, head, v, item, fmt, def)
    use TOUZA_Nio_std,only: KDBL
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    real(kind=KDBL), intent(out)         :: v
    integer,         intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    real(kind=KDBL), intent(in),optional :: def
    ierr = check_hitem_type(item, ht_real)
    if (ierr.eq.0) call restore_item(ierr, head, v, item, fmt, def)
    return
  end subroutine get_item_d
  subroutine get_item_date &
       & (ierr, head, dt, item, fmt)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    integer,         intent(out)         :: dt(*)
    integer,         intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    ierr = check_hitem_types(item, (/ht_date, ht_str/))
    if (ierr.eq.0) call restore_item(ierr, head, dt, item, fmt)
    return
  end subroutine get_item_date

!!!_  - put_item_n - set entry (with type check) by name
  subroutine put_item_na &
       & (ierr, head, v, item, fmt, tol)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    character(len=*),intent(in)          :: v
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol   ! tolerance
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       if (je.lt.0) then
          call put_item_a(ierr, head, v, ji, fmt=fmt, tol=tol)
       else
          call put_item_a(ierr, head, v, ji, je, fmt, tol)
       endif
    endif
    return
  end subroutine put_item_na
  subroutine put_item_ni &
       & (ierr, head, v, item, fmt, tol)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    integer,         intent(in)          :: v
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol   ! tolerance
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       call put_item_i(ierr, head, v, ji, fmt=fmt, tol=tol)
    endif
    return
  end subroutine put_item_ni
  subroutine put_item_nf &
       & (ierr, head, v, item, fmt, tol)
    use TOUZA_Nio_std,only: KFLT
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    real(kind=KFLT), intent(in)          :: v
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol   ! tolerance
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       call put_item_f(ierr, head, v, ji, fmt=fmt, tol=tol)
    endif
    return
  end subroutine put_item_nf
  subroutine put_item_nd &
       & (ierr, head, v, item, fmt, tol)
    use TOUZA_Nio_std,only: KDBL
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    real(kind=KDBL), intent(in)          :: v
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol   ! tolerance
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       call put_item_d(ierr, head, v, ji, fmt=fmt, tol=tol)
    endif
    return
  end subroutine put_item_nd
  subroutine put_item_ndate &
       & (ierr, head, dt, item, fmt, tol)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    integer,         intent(in)          :: dt(:)
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol   ! tolerance
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       call put_item_date(ierr, head, dt, ji, fmt=fmt, tol=tol)
    endif
    return
  end subroutine put_item_ndate

!!!_  - get_item_n - get entry (with type check) by name
  subroutine get_item_na &
       & (ierr, head, v, item, fmt)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    character(len=*),intent(out)         :: v
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       if (je.lt.0) then
          call get_item_a(ierr, head, v, ji, fmt=fmt)
       else
          call get_item_a(ierr, head, v, ji, je, fmt=fmt)
       endif
    endif
  end subroutine get_item_na
  subroutine get_item_ni &
       & (ierr, head, v, item, fmt, def)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    integer,         intent(out)         :: v
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: def
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       call get_item_i(ierr, head, v, ji, fmt, def)
    endif
  end subroutine get_item_ni
  subroutine get_item_nf &
       & (ierr, head, v, item, fmt, def)
    use TOUZA_Nio_std,only: KTGT=>KFLT
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    real(kind=KTGT), intent(out)         :: v
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    real(kind=KTGT), intent(in),optional :: def
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       call get_item_f(ierr, head, v, ji, fmt, def)
    endif
  end subroutine get_item_nf
  subroutine get_item_nd &
       & (ierr, head, v, item, fmt, def)
    use TOUZA_Nio_std,only: KTGT=>KDBL
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    real(kind=KTGT), intent(out)         :: v
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    real(kind=KTGT), intent(in),optional :: def
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       call get_item_d(ierr, head, v, ji, fmt, def)
    endif
  end subroutine get_item_nd
  subroutine get_item_ndate &
       & (ierr, head, dt, item, fmt)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    integer,         intent(out)         :: dt(*)
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       call get_item_date(ierr, head, dt, ji, fmt)
    endif
  end subroutine get_item_ndate

!!!_  - store_item - put entry (no type/range check)
  subroutine store_item_a &
       & (ierr, head, v, item, iteme, fmt, tol)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    character(len=*),intent(in)          :: v
    integer,         intent(in)          :: item
    integer,         intent(in),optional :: iteme
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol
    character(len=lhead) BUF
    integer ji, jb, je, l

    ierr = 0
    if (present(iteme)) then
       if (present(fmt)) then
          write(BUF, fmt, IOSTAT=ierr) trim(v)
       else
          write(BUF, '(A)', IOSTAT=ierr) trim(v)
       endif
       if (ierr.eq.0) then
          l = len_trim(BUF)
          if (l.eq.0) then
             head(item:iteme) = ' '
          else
             jb = 1
             do ji = item, iteme
                if (jb.gt.l) exit
                je = min(l, jb + litem - 1)
                head(ji) = BUF(jb:je)
                jb = jb + litem
             enddo
          endif
       endif
    else
       l = len_trim(v)
       if (l.eq.0) then
          head(item) = ' '
       else
          if (choice(0, tol).gt.0) then
             l = min(litem, l)
          endif
          if (present(fmt)) then
             write(head(item), fmt, IOSTAT=ierr) v(1:l)
          else
             write(head(item), '(A)', IOSTAT=ierr) v(1:l)
          endif
       endif
    endif
    return
  end subroutine store_item_a
  subroutine store_item_i &
       & (ierr, head, v, item, fmt, tol)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    integer,         intent(in)          :: v
    integer,         intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol   ! tolerance
    ierr = 0
    if (present(fmt)) then
       if (fmt.eq.' ') then
          write(head(item), def_fmt_I, IOSTAT=ierr) v
       else if (fmt.eq.'*') then
          write(head(item), *,         IOSTAT=ierr) v
       else
          write(head(item), fmt,       IOSTAT=ierr) v
       endif
    else
       write(head(item), def_fmt_I, IOSTAT=ierr) v
    endif
    if (choice(0, tol).gt.0) ierr = 0
    return
  end subroutine store_item_i
  subroutine store_item_f &
       & (ierr, head, v, item, fmt, tol)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_std,only: KFLT
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    real(kind=KFLT), intent(in)          :: v
    integer,         intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol   ! tolerance
    ierr = 0
    if (present(fmt)) then
       if (fmt.eq.' ') then
          write(head(item), def_fmt_R, IOSTAT=ierr) v
       else if (fmt.eq.'*') then
          write(head(item), *,         IOSTAT=ierr) v
       else
          write(head(item), fmt,       IOSTAT=ierr) v
       endif
    else
       write(head(item), def_fmt_R, IOSTAT=ierr) v
    endif
    if (choice(0, tol).gt.0) ierr = 0
    return
  end subroutine store_item_f
  subroutine store_item_d &
       & (ierr, head, v, item, fmt, tol)
    use TOUZA_Nio_std,only: choice
    use TOUZA_Nio_std,only: KDBL
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    real(kind=KDBL), intent(in)          :: v
    integer,         intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol   ! tolerance
    if (present(fmt)) then
       if (fmt.eq.' ') then
          write(head(item), def_fmt_R, IOSTAT=ierr) v
       else if (fmt.eq.'*') then
          write(head(item), *,         IOSTAT=ierr) v
       else
          write(head(item), fmt,       IOSTAT=ierr) v
       endif
    else
       write(head(item), def_fmt_R, IOSTAT=ierr) v
    endif
    if (choice(0, tol).gt.0) ierr = 0
    return
  end subroutine store_item_d
  subroutine store_item_date &
       & (ierr, head, dt, item, fmt, tol)
    use TOUZA_Nio_std,only: choice_a, choice
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    integer,         intent(in)          :: dt(:)
    integer,         intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol   ! tolerance
    character(len=128) f
    ierr = 0
    call choice_a(f, ' ', fmt)
    if (f.eq.' ') then
       if (dt(1).lt.10000) then
          write(head(item), def_fmt_date_trad, IOSTAT=ierr) dt(1:6)
       else if (dt(1).lt.100000) then
          write(head(item), def_fmt_date_long, IOSTAT=ierr) dt(1:6)
       else
          write(head(item), def_fmt_date_full, IOSTAT=ierr) dt(1:6)
       endif
    else if (f.eq.'*') then
       write(head(item), def_fmt_date_trad, IOSTAT=ierr) dt(1:6)
    else
       write(head(item), f, IOSTAT=ierr) dt(1:6)
    endif
    if (choice(0, tol).gt.0) ierr = 0
    return
  end subroutine store_item_date

!!!_  - restore_item - get entry (no type/range check)
  subroutine restore_item_a &
       & (ierr, head, v, item, iteme, fmt)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    character(len=*),intent(out)         :: v
    integer,         intent(in)          :: item
    integer,         intent(in),optional :: iteme
    character(len=*),intent(in),optional :: fmt
    character(len=lhead) BUF
    integer ji, jb, je

    ierr = 0
    if (present(iteme)) then
       if (ierr.eq.0) then
          buf = ' '
          jb = 1
          do ji = item, iteme
             je = jb + litem - 1
             BUF(jb:je) = head(ji)
             jb = je + 1
          enddo
       endif
       if (present(fmt)) then
          read(BUF, fmt, IOSTAT=ierr) v
       else
          read(BUF, '(A)', IOSTAT=ierr) v
       endif
    else
       if (head(item).eq.' ') then
          v = ' '
       else if (present(fmt)) then
          read(head(item), fmt, IOSTAT=ierr) v
       else
          read(head(item), '(A)', IOSTAT=ierr) v
       endif
    endif
    return
  end subroutine restore_item_a
  subroutine restore_item_i &
       & (ierr, head, v, item, fmt, def)
    use TOUZA_Nio_std,only: parse_number
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    integer,         intent(out)         :: v
    integer,         intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: def
    ierr = 0
    if (head(item).eq.' ') then
       if (present(def)) then
          v = def
       else
          ierr = -1
       endif
    else
       if (present(fmt)) then
          if (fmt.eq.' ') then
             read(head(item), def_fmt_I, IOSTAT=ierr) v
          else if (fmt.eq.'*') then
             ! read(head(item), *,         IOSTAT=ierr) v
             call parse_number(ierr, v, head(item))
          else
             read(head(item), fmt,       IOSTAT=ierr) v
          endif
       else
          read(head(item), def_fmt_I, IOSTAT=ierr) v
       endif
    endif
    return
  end subroutine restore_item_i
  subroutine restore_item_f &
       & (ierr, head, v, item, fmt, def)
    use TOUZA_Nio_std,only: parse_number
    implicit none
    integer,parameter :: KTGT=KFLT
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    real(kind=KTGT), intent(out)         :: v
    integer,         intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    real(kind=KTGT), intent(in),optional :: def
    ierr = 0
    if (head(item).eq.' ') then
       if (present(def)) then
          v = def
       else
          ierr = -1
       endif
    else
       if (present(fmt)) then
          if (fmt.eq.' ') then
             read(head(item), def_fmt_R, IOSTAT=ierr) v
          else if (fmt.eq.'*') then
             ! read(head(item), *,         IOSTAT=ierr) v
             call parse_number(ierr, v, head(item))
          else
             read(head(item), fmt,       IOSTAT=ierr) v
          endif
       else
          read(head(item), def_fmt_R, IOSTAT=ierr) v
       endif
    endif
    return
  end subroutine restore_item_f
  subroutine restore_item_d &
       & (ierr, head, v, item, fmt, def)
    use TOUZA_Nio_std,only: parse_number
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    real(kind=KTGT), intent(out)         :: v
    integer,         intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    real(kind=KTGT), intent(in),optional :: def
    ierr = 0
    if (head(item).eq.' ') then
       if (present(def)) then
          v = def
       else
          ierr = -1
       endif
    else
       if (present(fmt)) then
          if (fmt.eq.' ') then
             read(head(item), def_fmt_R, IOSTAT=ierr) v
          else if (fmt.eq.'*') then
             ! read(head(item), *,         IOSTAT=ierr) v
             call parse_number(ierr, v, head(item))
          else
             read(head(item), fmt,       IOSTAT=ierr) v
          endif
       else
          read(head(item), def_fmt_R, IOSTAT=ierr) v
       endif
    endif
    return
  end subroutine restore_item_d
  subroutine restore_item_date &
       & (ierr, head, dt, item, fmt)
    use TOUZA_Nio_std,only: choice_a
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    integer,         intent(out)         :: dt(*)
    integer,         intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    character(len=128) f
    integer je, j
    ierr = 0
    call choice_a(f, ' ', fmt)
    if (f.eq.' ') then
       je = INDEX(head(item), ' ')
       !! not perfect, though...
       if (je.gt.0) then
          if (ierr.eq.0) read(head(item)(je-2:je-1), *, IOSTAT=ierr) dt(3)
          if (ierr.eq.0) read(head(item)(je-4:je-3), *, IOSTAT=ierr) dt(2)
          if (ierr.eq.0) read(head(item)(1:je-5),    *, IOSTAT=ierr) dt(1)
          if (ierr.eq.0) read(head(item)(je+1:je+2), *, IOSTAT=ierr) dt(4)
          if (ierr.eq.0) read(head(item)(je+3:je+4), *, IOSTAT=ierr) dt(5)
          if (ierr.eq.0) read(head(item)(je+5:je+6), *, IOSTAT=ierr) dt(6)
       else
          je=len_trim(head(item))
          do j = 6, 2, -1
             if (ierr.eq.0) read(head(item)(je-1:je), *, IOSTAT=ierr) dt(j)
             je = je - 2
          enddo
          if (ierr.eq.0) read(head(item)(1:je), *, IOSTAT=ierr) dt(1)
       endif
    else if (f.eq.'*') then
       read(head(item), def_fmt_date_trad, IOSTAT=ierr) dt(1:6)
    else
       read(head(item), f, IOSTAT=ierr) dt(1:6)
    endif
    return
  end subroutine restore_item_date
!!!_  - store_item_na
  subroutine store_item_na &
       & (ierr, head, v, item, fmt, tol)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    character(len=*),intent(in)          :: v
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       if (je.lt.0) then
          call store_item_a(ierr, head, v, ji, fmt=fmt, tol=tol)
       else
          call store_item_a(ierr, head, v, ji, je, fmt, tol)
       endif
    endif
  end subroutine store_item_na
  subroutine store_item_ni &
       & (ierr, head, v, item, fmt, tol)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    integer,         intent(in)          :: v
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol   ! tolerance
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       call store_item_i(ierr, head, v, ji, fmt=fmt, tol=tol)
    endif
  end subroutine store_item_ni
  subroutine store_item_nf &
       & (ierr, head, v, item, fmt, tol)
    use TOUZA_Nio_std,only: KTGT=>KFLT
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    real(kind=KTGT), intent(in)          :: v
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol   ! tolerance
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       call store_item_f(ierr, head, v, ji, fmt=fmt, tol=tol)
    endif
  end subroutine store_item_nf
  subroutine store_item_nd &
       & (ierr, head, v, item, fmt, tol)
    use TOUZA_Nio_std,only: KTGT=>KDBL
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    real(kind=KTGT), intent(in)          :: v
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol   ! tolerance
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       call store_item_d(ierr, head, v, ji, fmt=fmt, tol=tol)
    endif
  end subroutine store_item_nd
  subroutine store_item_ndate &
       & (ierr, head, dt, item, fmt, tol)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: head(*)
    integer,         intent(in)          :: dt(:)
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: tol   ! tolerance
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       call store_item_date(ierr, head, dt, ji, fmt=fmt, tol=tol)
    endif
  end subroutine store_item_ndate
!!!_  - restore_item_n
  subroutine restore_item_na &
       & (ierr, head, v, item, fmt)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    character(len=*),intent(out)         :: v
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       if (je.lt.0) then
          call restore_item_a(ierr, head, v, ji, fmt=fmt)
       else
          call restore_item_a(ierr, head, v, ji, je, fmt=fmt)
       endif
    endif
  end subroutine restore_item_na
  subroutine restore_item_ni &
       & (ierr, head, v, item, fmt, def)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    integer,         intent(out)         :: v
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: def
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       call restore_item_i(ierr, head, v, ji, fmt, def)
    endif
  end subroutine restore_item_ni
  subroutine restore_item_nf &
       & (ierr, head, v, item, fmt, def)
    use TOUZA_Nio_std,only: KTGT=>KFLT
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    real(kind=KTGT), intent(out)         :: v
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    real(kind=KTGT), intent(in),optional :: def
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       call restore_item_f(ierr, head, v, ji, fmt, def)
    endif
  end subroutine restore_item_nf
  subroutine restore_item_nd &
       & (ierr, head, v, item, fmt, def)
    use TOUZA_Nio_std,only: KTGT=>KDBL
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    real(kind=KTGT), intent(out)         :: v
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    real(kind=KTGT), intent(in),optional :: def
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       call restore_item_d(ierr, head, v, ji, fmt, def)
    endif
  end subroutine restore_item_nd
  subroutine restore_item_ndate &
       & (ierr, head, dt, item, fmt)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in)          :: head(*)
    integer,         intent(out)         :: dt(*)
    character(len=*),intent(in)          :: item
    character(len=*),intent(in),optional :: fmt
    integer ji, je
    ierr = 0
    call get_hindex(ji, je, item)
    ierr = min(0, ji)
    if (ierr.eq.0) then
       call restore_item_date(ierr, head, dt, ji, fmt)
    endif
  end subroutine restore_item_ndate

!!!_  - fill_header
  subroutine fill_header &
       & (ierr, head, refh, pref)
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(inout) :: head(*)
    character(len=*),intent(in)    :: refh(*)
    integer,         intent(in)    :: pref     ! 0 or 1 to prefer head or refh
    integer ji, je
    ierr = 0
    if (pref.eq.0) then
       do ji = 1, nitem
          if (hiends(ji).lt.0) then
             if (head(ji).eq.' ') head(ji) = refh(ji)
          endif
       enddo
       ji = 1
       do
          if (ji.gt.nitem) exit
          je = hiends(ji)
          if (je.gt.0) then
             if (ALL(head(ji:je).eq.' ')) head(ji:je) = refh(ji:je)
             ji = je
          endif
          ji = ji + 1
       enddo
    else
       do ji = 1, nitem
          if (hiends(ji).lt.0) then
             if (refh(ji).ne.' ') head(ji) = refh(ji)
          endif
       enddo
       ji = 1
       do
          if (ji.gt.nitem) exit
          je = hiends(ji)
          if (je.gt.0) then
             if (ANY(refh(ji:je).ne.' ')) head(ji:je) = refh(ji:je)
             ji = je
          endif
          ji = ji + 1
       enddo
    endif
  end subroutine fill_header

!!!_ + private interfaces
!!!_  & check_hitem_type - check against header-item default type
  integer function check_hitem_type(item, t) result (ierr)
    implicit none
    integer,intent(in) :: item
    integer,intent(in) :: t

    if (item.le.0.or.item.gt.nitem) then
       ierr = _ERROR(ERR_HITEM_INVALID)
    else if (hitypes(item) .ne. t) then
       ierr = _ERROR(ERR_HITEM_TYPE_MISMATCH)
    else
       ierr = 0
    endif

    return
  end function check_hitem_type

!!!_  & check_hitem_types - check against header-item default type (list)
  integer function check_hitem_types(item, tt) result (ierr)
    implicit none
    integer,intent(in) :: item
    integer,intent(in) :: tt(:)

    if (item.le.0.or.item.gt.nitem) then
       ierr = _ERROR(ERR_HITEM_INVALID)
    else if (ANY(hitypes(item).eq.tt(:))) then
       ierr = 0
    else
       ierr = _ERROR(ERR_HITEM_TYPE_MISMATCH)
    endif

    return
  end function check_hitem_types

!!!_  & check_hitem_range - check header-item continuation
  integer function check_hitem_range(item, iteme) &
       & result (ierr)
    use TOUZA_Nio_std,only: choice
    implicit none
    integer,intent(in)          :: item
    integer,intent(in),optional :: iteme
    integer jend
    integer itmp
    ierr = check_hitem_type(item, ht_str)
    if (ierr.eq.0) then
       jend = hiends(item)
       itmp = choice(-1, iteme)
       if (itmp.eq.0) then
          if (jend.lt.0) then
             ierr = 0
          else
             ierr = jend
          endif
       else if (itmp.ge.item.and.itmp.le.jend) then
          ierr = iteme
       else if (itmp.lt.0) then
          ierr = 0
       else
          ierr = _ERROR(ERR_HITEM_INVALID_RANGE)
       endif
    endif

  end function check_hitem_range

!!!_  & set_def_types - set-up default item type
  subroutine set_def_types &
       & (ierr, ht)
    integer,intent(out) :: ierr
    integer,intent(out) :: ht(*)

    ierr = 0
    ht(hi_IDFM)   = ht_int
    ht(hi_DSET)   = ht_str
    ht(hi_ITEM)   = ht_str

    ht(hi_EDIT1)  = ht_str
    ht(hi_EDIT2)  = ht_str
    ht(hi_EDIT3)  = ht_str
    ht(hi_EDIT4)  = ht_str
    ht(hi_EDIT5)  = ht_str
    ht(hi_EDIT6)  = ht_str
    ht(hi_EDIT7)  = ht_str
    ht(hi_EDIT8)  = ht_str

    ht(hi_FNUM)   = ht_int
    ht(hi_DNUM)   = ht_int
    ht(hi_TITL1)  = ht_str
    ht(hi_TITL2)  = ht_str
    ht(hi_UNIT)   = ht_str

    ht(hi_ETTL1)  = ht_str
    ht(hi_ETTL2)  = ht_str
    ht(hi_ETTL3)  = ht_str
    ht(hi_ETTL4)  = ht_str
    ht(hi_ETTL5)  = ht_str
    ht(hi_ETTL6)  = ht_str
    ht(hi_ETTL7)  = ht_str
    ht(hi_ETTL8)  = ht_str

    ht(hi_TIME)   = ht_int
    ht(hi_UTIM)   = ht_str
    ht(hi_DATE)   = ht_str
    ht(hi_TDUR)   = ht_int

    ht(hi_AITM1)  = ht_str
    ht(hi_ASTR1)  = ht_int
    ht(hi_AEND1)  = ht_int
    ht(hi_AITM2)  = ht_str
    ht(hi_ASTR2)  = ht_int
    ht(hi_AEND2)  = ht_int
    ht(hi_AITM3)  = ht_str
    ht(hi_ASTR3)  = ht_int
    ht(hi_AEND3)  = ht_int

    ht(hi_DFMT)   = ht_str
    ht(hi_MISS)   = ht_real
    ht(hi_DMIN)   = ht_real
    ht(hi_DMAX)   = ht_real
    ht(hi_DIVS)   = ht_real
    ht(hi_DIVL)   = ht_real
    ht(hi_STYP)   = ht_int

    ht(hi_COPTN)  = ht_str
    ht(hi_IOPTN)  = ht_int
    ht(hi_ROPTN)  = ht_real

    ht(hi_TIME2)  = ht_str
    ht(hi_UTIM2)  = ht_str

    ht(hi_MEMO1)  = ht_str
    ht(hi_MEMO2)  = ht_str
    ht(hi_MEMO3)  = ht_str
    ht(hi_MEMO4)  = ht_str
    ht(hi_MEMO5)  = ht_str
    ht(hi_MEMO6)  = ht_str
    ht(hi_MEMO7)  = ht_str
    ht(hi_MEMO8)  = ht_str
    ht(hi_MEMO9)  = ht_str
    ht(hi_MEMO10) = ht_str

    ht(hi_CDATE)  = ht_str
    ht(hi_CSIGN)  = ht_str
    ht(hi_MDATE)  = ht_str
    ht(hi_MSIGN)  = ht_str
    ht(hi_SIZE)   = ht_int

  end subroutine set_def_types

!!!_  - set_def_ranges
  subroutine set_def_ranges &
       & (ierr, he)
    integer,intent(out) :: ierr
    integer,intent(out) :: he(*)

    ierr = 0
    he(1:nitem) = -1

    he(hi_EDIT1:hi_EDIT8)  = hi_EDIT8
    he(hi_TITL1:hi_TITL2)  = hi_TITL2
    he(hi_ETTL1:hi_ETTL8)  = hi_ETTL8
    he(hi_MEMO1:hi_MEMO10) = hi_MEMO10

    return
  end subroutine set_def_ranges
!!!_  - set_def_tables
  subroutine set_def_tables(ierr, he)
    use TOUZA_Nio_std,only: new_htable, reg_entry
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: he(*)
    integer j
    integer jp
    integer tgt
    if (hh_items.lt.0) then
       ! hard-coded members (+7 > duplicate + wide)
       hh_items = new_htable('nio_header', nitem + 7, lkey=lname, nkey=0, awidth=4)
       ierr = min(0, hh_items)
       do j = 1, nitem
          if (ierr.eq.0) then
             ierr = reg_entry(hh_items, akey=hf_names(j), status=(/j/), err=.TRUE.)
          endif
       enddo
       if (ierr.eq.0) then
          ierr = reg_entry(hh_items, akey='TIME2', status=(/hi_TIME2/), err=.TRUE.)
       endif
       if (ierr.eq.0) then
          ierr = reg_entry(hh_items, akey='UTIM2', status=(/hi_UTIM2/), err=.TRUE.)
       endif
       tgt = -1
       do j = 1, nitem
          if (he(j).gt.0) then
             if (tgt.lt.0) then
                tgt = he(j)
                jp = scan(hf_names(j), '0123456789')
                if (jp.gt.0) then
                   ierr = reg_entry(hh_items, akey=hf_names(j)(1:jp-1), status=(/j + mask_wide/), err=.TRUE.)
                endif
             else if (tgt.eq.j) then
                tgt = -1
             endif
          endif
          if (ierr.ne.0) exit
       enddo
    endif
  end subroutine set_def_tables
!!!_  - get_hindex
  subroutine get_hindex(item, iteme, name)
    use TOUZA_Std_utl,only: upcase
    use TOUZA_Std_htb,only: query_status
    implicit none
    integer,         intent(out) :: item, iteme
    character(len=*),intent(in)  :: name
    character(len=lname) :: buf
    integer jerr
    item  = -1
    iteme = -1
    call upcase(buf, name)
    call query_status(jerr, item, hh_items, akey=buf)
    if (jerr.eq.0) then
       if (IAND(item, mask_wide).gt.0) then
          item  = IEOR(item, mask_wide)
          iteme = 0
       endif
    endif
  end subroutine get_hindex

!!!_ + end TOUZA_Nio_header
end module TOUZA_Nio_header

!!!_@ test_nio_header - test program
#ifdef TEST_NIO_HEADER
program test_nio_header
  use TOUZA_Nio_header
  implicit none
  integer ierr
  character(len=litem) ha(nitem)
  character(len=litem*10) txt

101 format(A, ' = ', I0)
  call init(ierr, stdv=9)
  write(*, 101) 'INIT', ierr

  if (ierr.eq.0) call diag(ierr)
  write(*, 101) 'DIAG', ierr

  ha(:) = ' '
  if (ierr.eq.0) call put_item(ierr, ha, 9253,   hi_IDFM)
  if (ierr.eq.0) call put_item(ierr, ha, 123456, hi_FNUM, '(I8.8)')
  if (ierr.eq.0) call put_item(ierr, ha, 'NIO test', hi_DSET)
  if (ierr.eq.0) call put_item(ierr, ha, 'item 1', hi_ITEM)
  if (ierr.eq.0) call put_item(ierr, ha, -999.9d9, hi_MISS)
  if (ierr.eq.0) call store_item(ierr, ha, -999.9d9, hi_DNUM)  ! ignore type
  if (ierr.eq.0) call put_item(ierr, ha, (/ 1973 , 1, 30, 12, 34, 56 /), hi_DATE)
  if (ierr.eq.0) call put_item(ierr, ha, (/ 31973, 1, 30, 12, 34, 56 /), hi_CDATE)
  if (ierr.eq.0) call put_item(ierr, ha, 'THIS IS VERY LONG TITLE TO BE SPLITTED', hi_TITL1, hi_TITL2)
  if (ierr.eq.0) call put_item(ierr, ha, 'THIS IS VERY LONG TITLE TO BE SPLITTED', hi_MEMO1, 0)

  if (ierr.eq.0) call get_item(ierr, ha, txt, 'DFMT')
  if (ierr.eq.0) call get_item(ierr, ha, txt, 'TITL')
  if (ierr.eq.0) call get_item(ierr, ha, txt, hi_TITL1, 0)
  if (ierr.eq.0) call get_item(ierr, ha, txt, 'TITL1')

  if (ierr.eq.0) call show_header(ierr, ha)
  if (ierr.eq.0) call finalize(ierr)
  write(*, 101) 'FINAL', ierr
  stop
end program test_nio_header

#endif /* TEST_NIO_HEADER */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

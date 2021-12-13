!!!_! nng_miroc.F90 - TOUZA/Nng MIROC compatible interfaces
! Maintainer: SAITO Fuyuki
! Created: Dec 8 2021
#define TIME_STAMP 'Time-stamp: <2021/12/12 11:28:48 fuyuki nng_miroc.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_nng.h"
!!!_* Macros
#ifndef    MIROC_INTEGER
#  define  MIROC_INTEGER 4
#endif
#ifndef    MIROC_DOUBLE
#  define  MIROC_DOUBLE 8
#endif
#ifndef   WITH_MIROC
#  define WITH_MIROC 0
#endif
!!!_@ TOUZA_Nng_miroc - Nng miroc compatible interfaces
module TOUZA_Nng_miroc
!!!_ = declaration
!!!_  - default
  implicit none
  public
  integer,parameter :: KMD = MIROC_DOUBLE
  real(kind=KMD),parameter   :: vmiss_def = -999.0_KMD
  character(len=*),parameter :: csign_def = 'MIROC'
!!!_  - miroc include original
#if WITH_MIROC
# include "zhdim.F"  /* NCC NDC (No. of characters) */
# include "ziopara.F"
# define _NCC NCC
# define _NDC NDC
#else  /* not WITH_MIROC */
  integer,parameter :: NCC=0, NDC=0 ! dummy
# define _NCC *
# define _NDC *
#endif /* not WITH_MIROC */
#define __MDL__ 'm'
!!!_  - interfaces (external)
  interface
     subroutine GTZRDZ &
          & (DDATA, HEAD,  IEOD,  &
          &  ISTA,  IEND,  JSTA,  JEND,  KSTA, KEND, &
          &  IFILE, HITEM, HDFMT, HCLAS, &
          &  DSIZE)
       implicit none
       integer,            parameter   :: KMD = MIROC_DOUBLE
       integer,            intent(in)  :: DSIZE
       real(kind=KMD),     intent(out) :: DDATA(DSIZE)  !! data
       character(len=_NCC),intent(out) :: HEAD(_NDC)
       integer,            intent(out) :: IEOD
       integer,            intent(out) :: ISTA, IEND
       integer,            intent(out) :: JSTA, JEND
       integer,            intent(out) :: KSTA, KEND
       integer,            intent(in)  :: IFILE
       character(len=*),   intent(in)  :: HITEM(*)  !! name for identify
       character(len=*),   intent(in)  :: HDFMT(*)  !! data format : neglected
       character(len=*),   intent(in)  :: HCLAS(*)  !! driver : neglected
     end subroutine GTZRDZ
     subroutine GFPEEK &
          & (HEAD, IEOD, IFILE)
       implicit none
       character(len=_NCC),intent(out) :: HEAD(_NDC)
       integer,            intent(out) :: IEOD
       integer,            intent(in)  :: IFILE
     end subroutine GFPEEK
     subroutine GFSKIP &
          & (IEOD, IFILE)
       implicit none
       integer,intent(out) :: IEOD
       integer,intent(in)  :: IFILE
     end subroutine GFSKIP
  end interface
!!!_  - private
  logical,save,private :: binit = .FALSE.
  logical,save,private :: bdiag = .FALSE.

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv)
    use TOUZA_Nng,only: nng_init=>init, set_default_header
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    logical,save :: ofirst = .TRUE.
    ierr = 0
    if (.not.binit) then
       binit = .TRUE.
       if (ierr.eq.0) then
          call nng_init (ierr, u, levv, mode, stdv)
       endif
       if (ierr.eq.0) then
          call set_default_header &
               (ierr, vmiss=vmiss_def, csign=csign_def, msign=csign_def)
       endif
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    use TOUZA_Nng,only: nng_diag=>diag, nng_msg=>msg
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: levv
    ierr = 0
    if (.not.bdiag) then
       bdiag = .TRUE.
       if (ierr.eq.0) call nng_diag(ierr, u, levv, mode)
       if (ierr.eq.0) call nng_msg(TIME_STAMP, __MDL__, u)
       if (ierr.eq.0) call nng_msg('(''WITH_MIROC = '', I0, )', WITH_MIROC, __MDL__, u)
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Nng,only: nng_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    ierr = 0
    if (ierr.eq.0) call nng_finalize(ierr, u, levv, mode)
    return
  end subroutine finalize

!!!_ + user subroutines
  subroutine put_item_time(ierr, head, time, kentr)
    use TOUZA_Nng_header,only: put_item
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: head(*)
    real(kind=KMD),  intent(in)  :: time
    integer,         intent(in)  :: kentr
    integer idate(6)
    ierr = 0
    call css2yh(idate, time)
    call put_item(ierr, head, idate, kentr)
  end subroutine put_item_time
!!!_ + end module
end module TOUZA_Nng_Miroc
!!!_* /nonmodule/ interfaces
!!!_ + io/igtior.F
!!!_  & GTZRDZ
subroutine GTZRDZ &
     & (DDATA, HEAD,  IEOD,  &
     &  ISTA,  IEND,  JSTA,  JEND,  KSTA, KEND, &
     &  IFILE, HITEM, HDFMT, HCLAS, &
     &  DSIZE)
  use TOUZA_Nng,only: nng_read_header, parse_header_size, nng_read_data, &
       & hi_ASTR1, hi_ASTR2, hi_ASTR3, hi_AEND1, hi_AEND2, hi_AEND3
  use TOUZA_Nng_miroc,only: nm_init=>init, nm_diag=>diag, KMD, NCC, NDC
  implicit none
  integer,            intent(in)  :: DSIZE
  real(kind=kmd),     intent(out) :: DDATA(DSIZE)  !! data
  character(len=_NCC),intent(out) :: HEAD(_NDC)
  integer,            intent(out) :: IEOD
  integer,            intent(out) :: ISTA, IEND
  integer,            intent(out) :: JSTA, JEND
  integer,            intent(out) :: KSTA, KEND
  integer,            intent(in)  :: IFILE
  character(len=*),   intent(in)  :: HITEM(*)  !! name for identify
  character(len=*),   intent(in)  :: HDFMT(*)  !! data format : neglected
  character(len=*),   intent(in)  :: HCLAS(*)  !! driver : neglected

  logical,save :: ofirst = .TRUE.
  integer krect                 ! record type
  integer jerr
  integer n
  integer,save :: jfpar

  jerr = 0

  if (ofirst) then
#if WITH_MIROC
     call GETJFP(jfpar)
#else
     jfpar = 0  ! hard-coded
#endif
     call nm_init(jerr, u=jfpar)
     if (jerr.eq.0) call nm_diag(jerr)
     ofirst = .FALSE.
  endif

  IEOD = 0
  call nng_read_header(jerr, HEAD, krect, IFILE)
  if (jerr .ne. 0) then ! error or no data
     IEOD = ABS(jerr)
     return
  endif

  n = parse_header_size(HEAD, 0)
  ! check buffer size
  if (n .gt. DSIZE) then
#if WITH_MIROC
     write(jfpar, *) '### GTZRDZ : AREA TOO SMALL:',  &
          &          ' ITEM: ' // TRIM(HITEM) // ',', &
          &          ' DATA:', n, ',AREA:', DSIZE
     call XABORT(1)
#else  /* not WITH_MIROC */
     call nng_msg('overflow in GTZRDZ', __MDL__)
#endif /* not WITH_MIROC */
     ieod = 1
     RETURN
  endif

  if (jerr.eq.0) call nng_read_data(jerr, DDATA, n, HEAD, krect, IFILE)
  if (jerr .ne. 0) then ! error in reading data
     IEOD = ABS(jerr)
     return
  endif
  if (jerr.eq.0) call get_item(jerr, HEAD, ISTA, hi_ASTR1)
  if (jerr.eq.0) call get_item(jerr, HEAD, IEND, hi_AEND1)
  if (jerr.eq.0) call get_item(jerr, HEAD, JSTA, hi_ASTR2)
  if (jerr.eq.0) call get_item(jerr, HEAD, JEND, hi_AEND2)
  if (jerr.eq.0) call get_item(jerr, HEAD, KSTA, hi_ASTR3)
  if (jerr.eq.0) call get_item(jerr, HEAD, KEND, hi_AEND3)

  return
end subroutine GTZRDZ
!!!_  & GFPEEK - peek meta data in GTOOL3 record
subroutine GFPEEK &
     & (HEAD, IEOD, IFILE)
  use TOUZA_Std,only: is_error_match
  use TOUZA_Nng,only: nng_read_header,nitem,KIOFS,WHENCE_ABS,ssq_rseek
  use TOUZA_Nng_miroc,only: NCC, NDC
  implicit none
#if WITH_MIROC
# include "zhdim.F"  /* NCC NDC (No. of characters) */
# include "ziopara.F"
#else
  integer,parameter :: MM_EOF = -128, MM_ERR = -256 ! dummy
#endif
  character(len=_NCC),intent(out) :: HEAD(_NDC)
  integer,            intent(out) :: IEOD
  integer,            intent(in)  :: IFILE

  integer krect                 ! record type
  integer jerr
  integer(KIND=KIOFS) :: jpos

  IEOD = 0

  inquire(UNIT=IFILE, IOSTAT=jerr, POS=jpos)
  if (jerr.ne.0) then
     IEOD = MM_ERR
     return
  endif
  call nng_read_header(jerr, HEAD, krect, IFILE)
  if (jerr.ne.0) then
     HEAD(1:nitem) = ' '
     if (is_error_match(jerr, ERR_EOF)) then
        IEOD = MM_EOF
     else
        IEOD = MM_ERR
     endif
     return
  endif
  call ssq_rseek(jerr, IFILE, jpos, WHENCE_ABS)
  if (jerr.ne.0) IEOD = MM_ERR
  return
end subroutine GFPEEK
!!!_  & GFSKIP - skip a GTOOL3 record (header+body)
subroutine GFSKIP &
     & (IEOD, IFILE)
  use TOUZA_Nng,only: nng_skip_records
  implicit none
  integer,intent(out) :: IEOD
  integer,intent(in)  :: IFILE
  integer jerr
  IEOD = 0
  call nng_skip_records(jerr, 1, IFILE)
  if (jerr.ne.0) IEOD = -1
  return
end subroutine GFSKIP
!!!_ + io/igtiow.F
!!!_  & GTZWRZ
subroutine GTZWRZ &
     & (DDATA, &
     &  HITEM, HTITL, HUNIT, HDSET, &
     &  TIME,  TDUR,  JFILE, HDFMT, HCLAS, &
     &  TIME1, TIME2, RMISS, &
     &  HALON, HALAT, HASIG, &
     &  ISTA,  IEND,  JSTA,  JEND,  KSTA,  KEND, &
     &  DSIZE)
  use TOUZA_Nng_miroc,only:  nm_init=>init, nm_diag=>diag, vmiss_def
  use TOUZA_Nng_header,only: litem, nitem, put_item, &
       & hi_DSET,  hi_ITEM,  hi_TITL1, hi_TITL2, &
       & hi_UNIT,  hi_TIME,  hi_TDUR,  hi_DFMT,  &
       & hi_DATE,  hi_DATE1, hi_DATE2, &
       & hi_CDATE, hi_MDATE, hi_SIZE,  &
       & hi_AITM1, hi_AITM2, hi_AITM3, &
       & hi_ASTR1, hi_AEND1, hi_ASTR2, hi_AEND2, hi_ASTR3, hi_AEND3,  &
       & hi_MISS,  hi_DMIN,  hi_DMAX,  hi_DIVS,  hi_DIVL,  &
       & hi_EDIT1, hi_EDIT2, hi_EDIT3, &
       & hi_ETTL1, hi_ETTL2, hi_ETTL3
  use TOUZA_Nng_record,only: &
       & get_default_header, nng_write_header, nng_write_data, &
       & REC_DEFAULT
  implicit none
  integer,parameter :: KMD = MIROC_DOUBLE
  integer,         intent(in) :: DSIZE
  real(kind=KMD),  intent(in) :: DDATA(DSIZE)    !! data
  character(len=*),intent(in) :: HITEM
  character(len=*),intent(in) :: HTITL           !! title
  character(len=*),intent(in) :: HUNIT           !! unit
  character(len=*),intent(in) :: HDSET           !! name of dataset
  real(kind=KMD),  intent(in) :: TIME            !! time
  real(kind=KMD),  intent(in) :: TDUR            !! representative time
  integer,         intent(in) :: JFILE           !! output file No.
  character(len=*),intent(in) :: HDFMT           !! data format
  character(len=*),intent(in) :: HCLAS
  real(kind=KMD),  intent(in) :: RMISS           !! missing value
  real(kind=KMD),  intent(in) :: TIME1           !! time
  real(kind=KMD),  intent(in) :: TIME2           !! time
  character(len=*),intent(in) :: HALON
  character(len=*),intent(in) :: HALAT
  character(len=*),intent(in) :: HASIG
  integer,         intent(in) :: ISTA, IEND, JSTA, JEND, KSTA, KEND   !! for HEADER

  integer jerr
  logical,save :: ofirst = .TRUE.
  integer,save :: jfpar
  integer :: idtv(8)
  real(kind=KMD),save      :: TIME_PREV = - HUGE(0.0_KMD)
  real(kind=KMD),parameter :: LAZINESS  = 24 * 3600.0_KMD

  character(len=litem),save :: hdefv(nitem)
  character(len=litem)      :: head (nitem)

  real(kind=KMD),parameter :: TSCL = 3600.0_KMD

  integer krect
  integer n

  jerr = 0
  if (ofirst) then
     ofirst = .FALSE.
#if WITH_MIROC
     call GETJFP(jfpar)
#else
     jfpar = 0  ! hard-coded
#endif
     call nm_init(jerr, u=jfpar)
     if (jerr.eq.0) call nm_diag(jerr)
     if (jerr.eq.0) call get_default_header(hdefv)
  endif


  if (TIME .ge. TIME_PREV + LAZINESS) then
     call date_and_time(values=idtv(:))
     idtv(4:6) = idtv(5:7)
     call put_item(jerr, hdefv, idtv(1:6), hi_CDATE)
     call put_item(jerr, hdefv, idtv(1:6), hi_MDATE)
     TIME_PREV = TIME
  endif

  head(:) = hdefv(:)
  n = (IEND - ISTA + 1) * (JEND - JSTA + 1) * (KEND - KSTA + 1)
  call put_item(jerr, head, HDSET, hi_DSET)
  call put_item(jerr, head, HITEM, hi_ITEM)
  call put_item(jerr, head, HTITL, hi_TITL1, hi_TITL2)
  call put_item(jerr, head, HUNIT, hi_UNIT)
  call put_item(jerr, head, NINT(TIME/TSCL), hi_TIME)
  call put_item(jerr, head, NINT(TDUR/TSCL), hi_TDUR)

  if (RMISS .ne. vmiss_def) then
     call put_item(jerr, head, RMISS, hi_MISS)
     call put_item(jerr, head, RMISS, hi_DMIN)
     call put_item(jerr, head, RMISS, hi_DMAX)
     call put_item(jerr, head, RMISS, hi_DIVS)
     call put_item(jerr, head, RMISS, hi_DIVL)
  endif

  CALL CPERPO(.FALSE.)
  call put_item_time(jerr, head, time,  HI_DATE)
  call put_item_time(jerr, head, time1, HI_DATE1)
  call put_item_time(jerr, head, time2, HI_DATE2)
  CALL CPERPO(.TRUE.)

  if (HALON.eq.'AV' .or. HALON(1:1).eq.'=') then
     call put_item(jerr, head, 'LON'//HALON, hi_EDIT1)
     call put_item(jerr, head, 'LON'//HALON, hi_ETTL1)
  else
     call put_item(jerr, head, HALON, hi_AITM1)
     call put_item(jerr, head, ISTA,  hi_ASTR1)
     call put_item(jerr, head, IEND,  hi_AEND1)
  endif
  if (HALAT.eq.'AV' .or. HALAT(1:1).eq.'=') then
     call put_item(jerr, head, 'LAT'//HALAT, hi_EDIT2)
     call put_item(jerr, head, 'LAT'//HALAT, hi_ETTL2)
  else
     call put_item(jerr, head, HALAT, hi_AITM2)
     call put_item(jerr, head, JSTA,  hi_ASTR2)
     call put_item(jerr, head, JEND,  hi_AEND2)
  endif
  if (HASIG.eq.'AV' .or. HASIG(1:1).eq.'=') then
     call put_item(jerr, head, 'LEV'//HASIG, hi_EDIT3)
     call put_item(jerr, head, 'LEV'//HASIG, hi_ETTL3)
  else
     call put_item(jerr, head, HASIG, hi_AITM3)
     call put_item(jerr, head, KSTA,  hi_ASTR3)
     call put_item(jerr, head, KEND,  hi_AEND3)
  endif
  call put_item(jerr, head, n, hi_SIZE)

  ! no adjustment for obsolete formats
  call put_item(jerr, head, HDFMT, hi_DFMT)

  call nng_write_header(jerr, head, krect, JFILE)
  ! if (ierr.eq.0) then
  !    if (levv.gt.1) call switch_urt_diag(wfile, jrec, udiag)
  ! endif
  krect = REC_DEFAULT
  if (jerr.eq.0) call nng_write_data(jerr, DDATA, n, head, krect, JFILE)

  return
end subroutine GTZWRZ
!!!_ + io/igtmeta.F
!!!_  & GTINID
!!!_  & PUT_DATETUPLE
!!!_  & GET_DATETUPLE
!!!_@ test_nng_miroc - test program
#ifdef TEST_NNG_MIROC
program test_nng_miroc
  use TOUZA_Nng_miroc
  implicit none
  integer ierr

  ierr = 0
  if (ierr.eq.0) call init(ierr, levv=-1)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call finalize(ierr)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_nng_miroc

#endif /* TEST_NNG_MIROC */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

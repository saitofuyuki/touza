!!!_! nng_miroc.F90 - TOUZA/Nng MIROC compatible interfaces
! Maintainer: SAITO Fuyuki
! Created: Dec 8 2021
#define TIME_STAMP 'Time-stamp: <2021/12/09 16:57:15 fuyuki nng_miroc.F90>'
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
       INTEGER,            intent(in)  :: DSIZE
       REAL(kind=KMD),     intent(out) :: DDATA(DSIZE)  !! data
       CHARACTER(len=_NCC),intent(out) :: HEAD(_NDC)
       INTEGER,            intent(out) :: IEOD
       INTEGER,            intent(out) :: ISTA, IEND
       INTEGER,            intent(out) :: JSTA, JEND
       INTEGER,            intent(out) :: KSTA, KEND
       INTEGER,            intent(in)  :: IFILE
       CHARACTER(len=*),   intent(in)  :: HITEM(*)  !! name for identify
       CHARACTER(len=*),   intent(in)  :: HDFMT(*)  !! data format : neglected
       CHARACTER(len=*),   intent(in)  :: HCLAS(*)  !! driver : neglected
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
!!!_  - public
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv)
    use TOUZA_Nng,only: nng_init=>init
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    ierr = 0
    if (ierr.eq.0) call nng_init(ierr, u, levv, mode, stdv)
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
    if (ierr.eq.0) call nng_diag(ierr, u, levv, mode)
    if (ierr.eq.0) call nng_msg(TIME_STAMP, __MDL__, u)
    if (ierr.eq.0) call nng_msg('(''WITH_MIROC = '', I0, )', WITH_MIROC, __MDL__, u)
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
  INTEGER,            intent(in)  :: DSIZE
  REAL(kind=KMD),     intent(out) :: DDATA(DSIZE)  !! data
  CHARACTER(len=_NCC),intent(out) :: HEAD(_NDC)
  INTEGER,            intent(out) :: IEOD
  INTEGER,            intent(out) :: ISTA, IEND
  INTEGER,            intent(out) :: JSTA, JEND
  INTEGER,            intent(out) :: KSTA, KEND
  INTEGER,            intent(in)  :: IFILE
  CHARACTER(len=*),   intent(in)  :: HITEM(*)  !! name for identify
  CHARACTER(len=*),   intent(in)  :: HDFMT(*)  !! data format : neglected
  CHARACTER(len=*),   intent(in)  :: HCLAS(*)  !! driver : neglected

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

  inquire(UNIT=IFILE, IOSTAT=jerr, SIZE=jpos)
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

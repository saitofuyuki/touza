!!!_! nio_miroc.F90 - TOUZA/Nio MIROC compatible interfaces
! Maintainer: SAITO Fuyuki
! Created: Dec 8 2021
#define TIME_STAMP 'Time-stamp: <2023/03/16 12:17:15 fuyuki nio_miroc.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021,2022,2023
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
!!!_* Includes
#ifndef   WITH_MIROC
#  define WITH_MIROC 0
#endif
#if WITH_MIROC
#  include "miroc.h"
#  include "ztouza.h"
#endif
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_nio.h"
#ifndef  OPT_WITH_NCTCDF
# define OPT_WITH_NCTCDF 0
#endif
!!!_* Macros
#ifndef    MIROC_INTEGER
#  define  MIROC_INTEGER 4
#endif
#ifndef    MIROC_DOUBLE
#  define  MIROC_DOUBLE 8
#endif
!!!_@ TOUZA_Nio_miroc - Nio miroc compatible interfaces
module TOUZA_Nio_miroc
!!!_ = declaration
!!!_  - default
  implicit none
  private
  integer,         parameter,public :: KMD = MIROC_DOUBLE
  real(kind=KMD),  parameter,public :: vmiss_def = -999.0_KMD
  character(len=*),parameter,public :: csign_def = 'MIROC'

  integer,parameter,public :: categ_normal = 0
  integer,parameter,public :: categ_nio = 1
!!!_  - miroc include original
#if WITH_MIROC
# include "zhdim.F"  /* NCC NDC (No. of characters) */
#else  /* not WITH_MIROC */
  integer,parameter :: NCC=0, NDC=0 ! dummy
#endif /* not WITH_MIROC */

#if WITH_MIROC
# define _NCC NCC
# define _NDC NDC
#else  /* not WITH_MIROC */
# define _NCC *
# define _NDC *
#endif /* not WITH_MIROC */

#define __MDL__ 'm'
!!!_  - interfaces (external)
  interface
!!!_   . init_common
     subroutine init_common(u)
       implicit none
       integer,intent(in),optional :: u
     end subroutine init_common
!!!_   . GTZRDZ
     subroutine GTZRDZ &
          & (DDATA, HEAD,  IEOD,  &
          &  ISTA,  IEND,  JSTA,  JEND,  KSTA, KEND, &
          &  IFILE, HITEM, HDFMT, HCLAS, &
          &  DSIZE)
       implicit none
#if WITH_MIROC
# include "zhdim.F"  /* NCC NDC (No. of characters) */
#endif
       integer,            parameter   :: KMD = MIROC_DOUBLE
       integer,            intent(in)  :: DSIZE
       real(kind=KMD),     intent(out) :: DDATA(DSIZE)  !! data
       character(len=_NCC),intent(out) :: HEAD(_NDC)
       integer,            intent(out) :: IEOD
       integer,            intent(out) :: ISTA, IEND
       integer,            intent(out) :: JSTA, JEND
       integer,            intent(out) :: KSTA, KEND
       integer,            intent(in)  :: IFILE
       character(len=*),   intent(in)  :: HITEM  !! name for identify
       character(len=*),   intent(in)  :: HDFMT  !! data format : neglected
       character(len=*),   intent(in)  :: HCLAS  !! driver : neglected
     end subroutine GTZRDZ
!!!_   . GFPEEK
     subroutine GFPEEK &
          & (HEAD, IEOD, IFILE)
       implicit none
#if WITH_MIROC
# include "zhdim.F"  /* NCC NDC (No. of characters) */
#endif
       character(len=_NCC),intent(out) :: HEAD(_NDC)
       integer,            intent(out) :: IEOD
       integer,            intent(in)  :: IFILE
     end subroutine GFPEEK
!!!_   . GFSKIP
     subroutine GFSKIP &
          & (IEOD, IFILE)
       implicit none
       integer,intent(out) :: IEOD
       integer,intent(in)  :: IFILE
     end subroutine GFSKIP
!!!_   . FOPEN
     subroutine FOPEN &
          & (IOS, IFILE, HFILE, HACT, HFORM, HACCSS)
      implicit none
      integer,         intent(out)   :: IOS
      integer,         intent(inout) :: IFILE
      character(len=*),intent(in)    :: HFILE
      character(len=*),intent(in)    :: HACT
      character(len=*),intent(in)    :: HFORM
      character(len=*),intent(in)    :: HACCSS
    end subroutine FOPEN
!!!_   . FREWND
    subroutine FREWND &
         & (IFILE)
      implicit none
      integer,intent(in) :: IFILE
    end subroutine FREWND
!!!_   . FINQUX
    subroutine FINQUX &
         & (IERR,  OEXIST, OPND, IFILE, HFILE, HFORM)
      implicit none
      integer,         intent(out) :: IERR
      logical,         intent(out) :: OEXIST, OPND
      integer,         intent(out) :: IFILE
      character(len=*),intent(in)  :: HFILE
      character(len=*),intent(in)  :: HFORM
    end subroutine FINQUX
!!!_   . FNUINI
    subroutine FNUINI &
         & (IFILMN, IFILMX)
      implicit none
      integer,intent(in)  :: IFILMN, IFILMX
    end subroutine FNUINI
!!!_   . FNEWU
    subroutine FNEWU &
         & (OFOUND, IFILE,  HFORM, IFILED)
      implicit none
      integer,         intent(out) :: IFILE
      logical,         intent(out) :: OFOUND
      character(len=*),intent(in)  :: HFORM
      integer,         intent(in)  :: IFILED
    end subroutine FNEWU
  end interface
!!!_  - private
  logical,save,private :: binit = .FALSE.
  logical,save,private :: bdiag = .FALSE.

!!!_  - public
  public init, diag, finalize
  public put_item_time
  public NCC,  NDC
  public init_common
  public nio_tell, nio_seek
  public GTZRDZ, GFPEEK, GFSKIP, FOPEN, FREWND, FINQUX, FNUINI, FNEWU
contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv)
    use TOUZA_Nio,only: nio_init=>init, set_default_header
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    ierr = 0
    if (.not.binit) then
       binit = .TRUE.
       if (ierr.eq.0) then
          call nio_init (ierr, u, levv, mode, stdv)
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
    use TOUZA_Nio,only: nio_diag=>diag, nio_msg=>msg
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: levv
    ierr = 0
    if (.not.bdiag) then
       bdiag = .TRUE.
       if (ierr.eq.0) call nio_diag(ierr, u, levv, mode)
       if (ierr.eq.0) call nio_msg(TIME_STAMP, __MDL__, u)
       if (ierr.eq.0) call nio_msg('(''WITH_MIROC = '', I0)', WITH_MIROC, __MDL__, u)
       if (ierr.eq.0) call nio_msg('(''OPT_WITH_NCTCDF = '', I0)', OPT_WITH_NCTCDF, __MDL__, u)
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Nio,only: nio_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    ierr = 0
    if (ierr.eq.0) call nio_finalize(ierr, u, levv, mode)
    return
  end subroutine finalize

!!!_ + user subroutines
!!!_  - put_item_time
  subroutine put_item_time(ierr, head, time, kentr)
    use TOUZA_Nio_header,only: put_item_date
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: head(*)
    real(kind=KMD),  intent(in)  :: time
    integer,         intent(in)  :: kentr
    integer idate(6)
    ierr = 0
    call css2yh(idate, time)
    call put_item_date(ierr, head, idate, kentr)
  end subroutine put_item_time
!!!_  - nio_tell - return file position (ciof_tell emulation)
  subroutine nio_tell(u, jpos)
    use TOUZA_Nio_std,only: KI64
    implicit none
    integer,           intent(in)  :: u
    integer(KIND=KI64),intent(out) :: jpos  ! not KIOFS
    integer jerr
    inquire(UNIT=u, IOSTAT=jerr, POS=jpos)
    return
  end subroutine nio_tell
!!!_  - nio_seek - set file position (ciof_seek emulation)
  subroutine nio_seek(ierr, u, jpos)
    use TOUZA_Nio_std,only: KI64, WHENCE_ABS, sus_rseek, sus_eswap
    implicit none
    integer,           intent(out) :: ierr
    integer,           intent(in)  :: u
    integer(KIND=KI64),intent(in)  :: jpos  ! not KIOFS
    call sus_rseek(ierr, u, jpos, whence=WHENCE_ABS)
    return
  end subroutine nio_seek
!!!_ + end module
end module TOUZA_Nio_Miroc
!!!_* /nonmodule/ interfaces
!!!_ + init_common
subroutine init_common(u)
  use TOUZA_Std,only: choice
  use TOUZA_Nio_miroc,only: init, diag
  implicit none
  integer,intent(in),optional :: u
  integer jerr
  integer jfpar
  jerr = 0
#if WITH_MIROC
  if (present(u)) then
     jfpar = u
  else
     call GETJFP(jfpar)
  endif
#else
  jfpar = choice(0, u)  ! hard-coded
#endif
  call init(jerr, jfpar)
  if (jerr.eq.0) call diag(jerr)
end subroutine init_common
!!!_ + io/irwgd.F
! !!!_  - GTZWRT- write in GTOOL3 format
! subroutine GTZWRT &
!      & (GDATA, &
!      &  HITEM, HTITL,  HUNIT, HDSET, &
!      &  TIME,  TDUR,   JFILE, HDFMT, HCLAS, &
!      &  HCORD, IORANK, &
!      &  TIME1, TIME2,  RMISS, &
!      &  IXSEL, IYSEL,  IZSEL, &
!      &  HALON, HALAT,  HASIG, &
!      &  IMAXD, JMAXD,  &
!      &  IDIMD, JDIMD,  KDIMD)
!   use TOUZA_Ppp_miroc,only: top_agent, query_nprocs
!   use TOUZA_Ppp_miroc,only: get_king, is_king
!   use TOUZA_Nio_miroc,only: KMD, NCC, NDC
!   implicit none
!   INTEGER,         intent(in) :: IDIMD, JDIMD, KDIMD
!   INTEGER,         intent(in) :: IMAXD, JMAXD
!   REAL(kind=KMD),  intent(in) :: GDATA(IDIMD, JDIMD, *)       !! local data
!   CHARACTER(len=*),intent(in) :: HTITL                        !! title
!   CHARACTER(len=*),intent(in) :: HUNIT                        !! unit
!   CHARACTER(len=*),intent(in) :: HDSET                        !! name of dataset

!   REAL(kind=KMD),  intent(in) :: TIME                         !! time
!   REAL(kind=KMD),  intent(in) :: TDUR                         !! representative time
!   CHARACTER(len=*),intent(in) :: HALON
!   CHARACTER(len=*),intent(in) :: HALAT
!   CHARACTER(len=*),intent(in) :: HASIG

!   CHARACTER(len=*),intent(in) :: HITEM                        !! name for identify
!   CHARACTER(len=*),intent(in) :: HDFMT                        !! data format
!   CHARACTER(len=*),intent(in) :: HCLAS
!   CHARACTER(len=*),intent(in) :: HCORD

!   INTEGER,         intent(in) :: JFILE                        !! output file No.
!   INTEGER,         intent(in) :: IORANK                       !! I/O RANK

!   REAL(KIND=KMD),  intent(in) :: TIME1                        !! time
!   REAL(KIND=KMD),  intent(in) :: TIME2                        !! time
!   REAL(KIND=KMD),  intent(in) :: RMISS                        !! missing value

!   INTEGER,         intent(in) :: IXSEL ( 2 )                  !! selected type of X
!   INTEGER,         intent(in) :: IYSEL ( 2 )                  !! selected type of Y
!   INTEGER,         intent(in) :: IZSEL ( 2 )                  !! selected type of Z

!   REAL(KIND=KMD),ALLOCATABLE,SAVE :: DDATA(:) ! global data: DDATA( DSIZE )
!   INTEGER,save :: DSIZE0
!   INTEGER,save :: JFPAR
!   INTEGER,save :: MYRANK, NPROCS

!   LOGICAL    OOUT
!   INTEGER      :: DSIZE                      ! DSIZE = IMAXD*JMAXD*KDIMD
!   REAL(kinD=KMD) :: DELY (JMAXD)
!   REAL(KIND=KMD) :: GD
!   REAL(KIND=KMD) :: TLAT
!   CHARACTER(len=NCC) :: HITEMX
!   CHARACTER(len=20)  :: HTIME
!   INTEGER    I, J, K, IJK, IK
!   INTEGER    IJMAXD
!   INTEGER    ISTA, IEND
!   INTEGER    JSTA, JEND
!   INTEGER    KSTA, KEND
!   INTEGER    IORNKX
!   INTEGER    ISTAT
!   integer    iacur
!   integer    jerr

!   call top_agent(jerr, iacur)
!   call query_nprocs(NPROCS, MYRANK, iacur)

!   CALL CSS2CC(HTIME, TIME)
!   HITEMX = HITEM

!   IORNKX = IORANK
!   IF (MYRANK .EQ. IORNKX) THEN
!      OOUT = .TRUE.
!   ELSE
!      OOUT = .FALSE.
!   ENDIF
! #ifdef CGCM
!   IF (HCORD(1:2) .EQ. 'OC') THEN
! #ifdef OPT_PARALLEL
!      IF (KDIMD .EQ. 1) THEN
!         CALL HQCDMX(NXG, NX, 'OCSFC')
!         CALL HQCDMY(NYG, NY, 'OCSFC')
!         NZ = 1
!      ELSE
!         CALL HQCDMX(NXG, NX, 'OCLV')
!         CALL HQCDMY(NYG, NY, 'OCLV')
!         CALL HQCDMZ(NZ,      'OCLV')
!      ENDIF

!      IF (OOUT) THEN
!         DSIZE = NXG * NYG * NZ
!      ELSE
!         DSIZE = 1
!      ENDIF
!      IF (DSIZE .GT. DSIZE0) THEN
!         IF (ALLOCATED(DDATA)) DEALLOCATE(DDATA)
!         ALLOCATE(DDATA(DSIZE), STAT=ISTAT)
!         IF (ISTAT .NE. 0) THEN
!            WRITE(JFPAR,*) ' ### GTZWRT: ALLOCATION ERROR(DDATA)'
! #if WITH_MIROC
!            CALL XABORT(1)
! #else
!            return
! #endif
!         ENDIF
!         DSIZE0 = DSIZE
!      ENDIF
!      CALL GATHER_OCEAN_ACTIVE &
!           & (DDATA, &
!           &  GDATA, KDIMD, IORNKX)
! #else /* ! ifdef OPT_PARALLEL */
!      IJK = 0
!      DO K = 1, KDIMD
!         DO J = 1, JMAXD
!            DO I = 1, IMAXD
!               IJK = IJK + 1
!               DDATA(IJK) = GDATA(I,J,K)
!            ENDDO
!         ENDDO
!      ENDDO
! #endif /* OPT_PARALLEL */
!   ELSE ! IF (HCLAS(1:3) .EQ. 'OCN')
! #endif /* CGCM */
!      IF ( OOUT ) THEN
!         DSIZE = IMAXD * JMAXD * KDIMD
!      ELSE
!         DSIZE = 1
!      ENDIF
!      IF (DSIZE .GT. DSIZE0) THEN
!         IF (ALLOCATED(DDATA)) DEALLOCATE(DDATA)
!         ALLOCATE(DDATA(DSIZE), STAT=ISTAT)
!         IF (ISTAT .NE. 0) THEN
!            WRITE(JFPAR,*) ' ### GTZWRT: ALLOCATION ERROR(DDATA)'
! #if WITH_MIROC
!            CALL XABORT(1)
! #else
!            return
! #endif
!         ENDIF
!         DSIZE0 = DSIZE
!      ENDIF
!      CALL MMagBkGathD &
!           & (DDATA, &
!           &  GDATA, HCORD, IACUR, IORNKX, &
!           &  IMAXD, JMAXD, KDIMD, &
!           &  IDIMD, JDIMD, KDIMD, DSIZE)
! #ifdef CGCM
!   ENDIF                     ! IF (OCN)
! #endif
!   IF (IYSEL(2).EQ.0 .AND. IYSEL(1).EQ.0) THEN
!      if (OOUT) CALL HQCGDY(DELY, HCORD, 0, 0)
!   ENDIF

!   IF (OOUT) THEN
!      ISTA = 1
!      JSTA = 1
!      KSTA = 1
!      IEND = IMAXD
!      JEND = JMAXD
!      KEND = KDIMD

!      IF ((IXSEL(1).NE.0 .AND. IYSEL(1).NE.0)  &
!           & .AND. (    (IZSEL(1).GT.0 .AND. IZSEL(2).GE.IZSEL(1)) &
!           &        .OR.(IYSEL(1).GT.0 .AND. IYSEL(2).GE.IYSEL(1)) &
!           &        .OR.(IXSEL(1).GT.0 .AND. IXSEL(2).GE.IXSEL(1)))) &
!           &   THEN
!         ISTA = MAX(IXSEL(1), 1)
!         JSTA = MAX(IYSEL(1), 1)
!         KSTA = MAX(IZSEL(1), 1)
!         IEND = MIN(IXSEL(2), IMAXD)
!         JEND = MIN(IYSEL(2), JMAXD)
!         KEND = MIN(IZSEL(2), KDIMD)
!         IF (IXSEL(2) .LT. 0) IEND = IMAXD
!         IF (IYSEL(2) .LT. 0) JEND = JMAXD
!         IF (IZSEL(2) .LT. 0) KEND = KDIMD
!         IJMAXD = IMAXD*JMAXD

!         IJK = 0
!         DO K = KSTA, KEND
!            DO J = JSTA, JEND
!               DO I = ISTA, IEND
!                  IJK = IJK + 1
!                  DDATA(IJK) = DDATA(I + IMAXD * (J - 1) + IJMAXD * (K - 1))
!               ENDDO
!            ENDDO
!         ENDDO
!      ELSE IF (IYSEL(2).EQ.0 .AND. IYSEL(1).EQ.0) THEN
!         IJMAXD = IMAXD * JMAXD

!         IK = 0
!         DO K = KSTA, KEND
!            DO I = ISTA, IEND
!               GD   = 0.D0
!               TLAT = 0.D0
!               DO J = JSTA, JEND
!                  IJK = I + IMAXD * (J - 1) + IJMAXD * (K - 1)
!                  IF (DDATA(IJK) .NE. RMISS) THEN
!                     GD   = GD   + DDATA(IJK) * DELY(J)
!                     TLAT = TLAT + DELY(J)
!                  ENDIF
!               ENDDO
!               IK = IK + 1
!               IF (TLAT .GT. 0.D0) THEN
!                  DDATA(IK) = GD / TLAT
!               ELSE
!                  DDATA(IK) = RMISS
!               ENDIF
!            ENDDO
!         ENDDO

!         JEND = 1
!      ENDIF

!      CALL GTZWRZ &
!           & (DDATA, &
!           &  HITEM, HTITL, HUNIT, HDSET, &
!           &  TIME,  TDUR,  JFILE, HDFMT, HCLAS, &
!           &  TIME1, TIME2, RMISS, &
!           &  HALON, HALAT, HASIG, &
!           &  ISTA,  IEND,  JSTA,  JEND,  KSTA,  KEND, &
!           &  DSIZE)
!       WRITE(JFPAR, *) ' *** OUTPUT ', HITEMX, ' TIME= ', HTIME
!    ELSE
!       WRITE(JFPAR, *) ' *** OUTPUT ', HITEMX, ' TIME= ', HTIME, &
!            &                                  ' AT RANK= ', IORNKX
!    ENDIF

!    RETURN
!  END subroutine GTZWRT

! #define IRWGD_KING_MODULE ' '
! !!!_  & GDREAD - read data & time select
! subroutine GDREAD &
!      & (GDATA, IEOD,  TIME,  TDUR,  KLEVS, &
!      &  IFILE, HITEM, HDFMT, HCLAS, TSEL0, TSEL1, &
!      &  IMAXD, JMAXD, IDIMD, JDIMD, KDIMD, HCORD)
!   use TOUZA_Nio_miroc,only: KMD, NCC, NDC
!   implicit none
! #if WITH_MIROC
! #include        "ziopara.F"
! #else
!   integer,parameter :: SEARCH_FORWARD = 0           ! dummy
!   integer,parameter :: MM_EOF = -128, MM_ERR = -256 ! dummy
! #endif
!   integer,         intent(in)  :: IDIMD, JDIMD, KDIMD
!   real(kind=KMD),  intent(out) :: GDATA(IDIMD, JDIMD, KDIMD) !! data
!   integer,         intent(out) :: IEOD                       !! exist:0,no data:1
!   REAL(KIND=KMD),  intent(out) :: TIME                       !! time
!   REAL(KIND=KMD),  intent(out) :: TDUR                       !! representative time
!   INTEGER,         intent(out) :: KLEVS

!   INTEGER,         intent(in)  :: IFILE                      !! file unit  number
!   CHARACTER(len=*),intent(in)  :: HITEM                      !! selected value of ITEM
!   CHARACTER(len=*),intent(in)  :: HDFMT                      !! data format (ignored)
!   CHARACTER(len=*),intent(in)  :: HCLAS                      !! name of driver
!                                                              !! 'ALL' 'GA' 'GB' 'LND' 'OCN' 'RIV'
!   CHARACTER(len=*),intent(in)  :: HCORD                      !! axis coordinates
!   INTEGER,         intent(in)  :: IMAXD, JMAXD
!   REAL(KIND=KMD),  intent(in)  :: TSEL0, TSEL1               !! select time

!   call GDREAD2 &
!        & (GDATA, IEOD, TIME,  TDUR,  KLEVS, &
!        & IFILE, HITEM, HDFMT, HCLAS, TSEL0, TSEL1, &
!        & IMAXD, JMAXD, IDIMD, JDIMD, KDIMD, HCORD, SEARCH_FORWARD)
! end subroutine GDREAD
! !!!_  & GDREAD2 - read data & time select
! SUBROUTINE GDREAD2 &
!      & (GDATA, IEOD,  TIME,  TDUR,  KLEVS, &
!      &  IFILE, HITEM, HDFMT, HCLAS, TSEL0, TSEL1, &
!      &  IMAXD, JMAXD, IDIMD, JDIMD, KDIMD, HCORD, MODE)
!   use TOUZA_Nio_miroc,only: KMD, NCC, NDC
!   implicit none
! #if WITH_MIROC
! #include        "ziopara.F"
! #else
!   integer,parameter :: MM_EOF  = -128, MM_ERR = -256 ! dummy
!   integer,parameter :: MM_TIME = -512                ! dummy
! #endif
!   integer,         intent(in)  :: IDIMD, JDIMD, KDIMD
!   real(kind=KMD),  intent(out) :: GDATA(IDIMD, JDIMD, KDIMD) !! data
!   integer,         intent(out) :: IEOD                       !! exist:0,no data:1
!   REAL(KIND=KMD),  intent(out) :: TIME                       !! time
!   REAL(KIND=KMD),  intent(out) :: TDUR                       !! representative time
!   INTEGER,         intent(out) :: KLEVS

!   INTEGER,         intent(in)  :: IFILE                      !! file unit  number
!   CHARACTER(len=*),intent(in)  :: HITEM                      !! selected value of ITEM
!   CHARACTER(len=*),intent(in)  :: HDFMT                      !! data format (ignored)
!   CHARACTER(len=*),intent(in)  :: HCLAS                      !! name of driver
!                                                              !! 'ALL' 'GA' 'GB' 'LND' 'OCN' 'RIV'
!   CHARACTER(len=*),intent(in)  :: HCORD                      !! axis coordinates
!   INTEGER,         intent(in)  :: IMAXD, JMAXD
!   REAL(KIND=KMD),  intent(in)  :: TSEL0, TSEL1               !! select time
!   INTEGER,         intent(in)  :: MODE                       !! search mode

!   INTEGER    I, J, K
!   INTEGER    IMAXZ, JMAXZ
!   INTEGER    MATCH

!   CHARACTER(len=NCC)   ::  HITEMD                !! item read
!   CHARACTER(len=NCC*2) ::  HTITL                 !! title
!   CHARACTER(len=NCC)   ::  HUNIT                 !! unit
!   CHARACTER(len=NCC)   ::  HDSET                 !! name of dataset
!   CHARACTER(len=NCC)   ::  HAX
!   CHARACTER(len=NCC)   ::  HAY
!   CHARACTER(len=NCC)   ::  HAZ
!   CHARACTER(len=NCC)   ::  HDFMTD                !! data format
!   INTEGER    JFPAR

!   IEOD = 1
!   IF ( IFILE .EQ. 0 ) RETURN

!   CALL GDSEARCH &
!        & (MATCH, &
!        &  HITEM, HCLAS, TSEL0, TSEL1, IFILE, MODE)
!   IF (IAND(MATCH, IOR(MM_EOF, MM_ERR)) .NE. 0) RETURN
!   IF (IAND(MATCH, MM_TIME) .NE. 0) THEN
!      !! read (GTOOL3)
!      CALL GTZRED &
!           & (GDATA,  IEOD,  &
!           &  IMAXZ,  JMAXZ, KLEVS, &
!           &  HITEMD, HTITL, HUNIT, HDSET, &
!           &  TIME,   TDUR,  HDFMTD,&
!           &  HAX,    HAY,   HAZ,   &
!           &  IFILE,  HITEM, HDFMT, HCLAS, HCORD, &
!           &  IMAXD,  JMAXD, IDIMD, JDIMD, KDIMD)
!   ENDIF
!   IF ( IEOD .NE. 0 ) RETURN

!   ! Here GDATA contains the data at (1:IMAXZ,1:JMAXZ,1:KLEVS)
!   !  check dimension
!   IF (IMAXZ .NE. IMAXD  .OR.  JMAXZ .NE. JMAXD) THEN
!      CALL GETJFP(JFPAR)
!      WRITE (JFPAR,*) ' ### GDREAD: RESOLUTION MISMATCH: ', &
!           &      HITEMD, &
!           &      ' DATA:',  IMAXZ, JMAXZ, &
!           &      ' REQ:',   IMAXD, JMAXD
!      CALL XABORT(1)
!      IEOD = 1
!      RETURN
!   ENDIF
!   !  extend data
!   DO K = 1, KLEVS
!      DO J = 1, JDIMD
!         DO I = IMAXD+1, IDIMD
!            GDATA(I,J,K) = GDATA(1,J,K)
!         ENDDO
!      ENDDO
!   ENDDO
!   DO J = JMAXD+1, JDIMD
!      DO I = 1, IDIMD
!         GDATA(I,J,K) = GDATA(I,1,K)
!      ENDDO
!   ENDDO

!   RETURN
! END SUBROUTINE GDREAD2
! !!!_  - GDWRIT    !! write data
! SUBROUTINE GDWRIT &
!      & (GDATA, &
!      &  HITEM, HTITL, HUNIT, &
!      &  TIME,  TDUR,  JFILE, HDFMT, HCLAS, &
!      &  IMAXD, JMAXD, &
!      &  IDIMD, JDIMD, KDIMD, HCORD          )
! #if WITH_TOUZA_PPP
!   use TOUZA_Ppp_miroc,only: get_king, top_agent
! #endif
!   use TOUZA_Nio_miroc,only: KMD, NCC, NDC
! #if WITH_MIROC
!   use IPCOMM
! #endif
!   IMPLICIT NONE
!   INTEGER,         intent(in) :: IDIMD, JDIMD, KDIMD
!   REAL(KIND=KMD),  intent(in) :: GDATA(IDIMD, JDIMD, KDIMD) !! data
!   CHARACTER(len=*),intent(in) :: HITEM                      !! name for identify
!   CHARACTER(len=*),intent(in) :: HTITL                      !! title
!   CHARACTER(len=*),intent(in) :: HUNIT                      !! unit
!   REAL(KIND=KMD),  intent(in) :: TIME                       !! time
!   REAL(KIND=KMD),  intent(in) :: TDUR                       !! representative time
!   INTEGER,         intent(in) :: JFILE                      !! output file No.
!   CHARACTER(len=*),intent(in) :: HDFMT                      !! data format
!   CHARACTER(len=*),intent(in) :: HCORD                      !! axis coordinates
!   CHARACTER(len=*),intent(in) :: HCLAS                      !! name of driver
!   INTEGER,         intent(in) :: IMAXD, JMAXD

!   CHARACTER(len=NCC) :: HAX
!   CHARACTER(len=NCC) :: HAY
!   CHARACTER(len=NCC) :: HAZ
!   CHARACTER(len=NCC) :: HDSET

!   INTEGER    IORANK
!   REAL(KIND=KMD),save :: RMISS=-999.0_KMD
!   INTEGER,       save :: IXSEL(2)=-1, IYSEL(2)=-1, IZSEL(2)=-1
!   LOGICAL,       save :: OFIRST=.TRUE.

!   integer jerr
!   integer iacur

!   IF ( JFILE .EQ. 0 ) THEN
!      RETURN
!   ENDIF

!   call top_agent(jerr, iacur)
!   call get_king(jerr, IORANK, IRWGD_KING_MODULE, iacur)

!   CALL GETRUN( HDSET )

!   CALL HQCNMX( HAX, HCORD )
!   CALL HQCNMY( HAY, HCORD )
!   CALL HQCNMZ( HAZ, HCORD )

!   !! write (GTOOL3)
!   CALL GTZWRT &
!        & (GDATA, &
!        &  HITEM, HTITL,  HUNIT, HDSET, &
!        &  TIME,  TDUR,   JFILE, HDFMT, HCLAS, &
!        &  HCORD, IORANK, &
!        &  TIME,  TIME,   RMISS, &
!        &  IXSEL, IYSEL,  IZSEL, &
!        &  HAX,   HAY,    HAZ,   &
!        &  IMAXD, JMAXD,  &
!        &  IDIMD, JDIMD,  KDIMD)
!   RETURN
! END SUBROUTINE GDWRIT
! !!!_  - GTZRW - read and write in GTOOL3 data file
! !!!_  - GTPWRT - write in GTOOL3 parallel format
! SUBROUTINE GTPWRT &
!      & (GDATA, &
!      &  HITEM, HTITL, HUNIT, HDSET, &
!      &  TIME,  TDUR,  JFILE, HDFMT, HCLAS, &
!      &  HCORD, &
!      &  TIME1, TIME2, RMISS, &
!      &  HALON, HALAT, HASIG, &
!      &  IMAXD, JMAXD, &
!      &  IDIMD, JDIMD, KDIMD                  )
!   use TOUZA_Nio_miroc,only: KMD, NCC, NDC
! #if WITH_MIROC
!   use IPCOMM
! #endif
!   IMPLICIT NONE
!   INTEGER,         intent(in) :: IDIMD, JDIMD, KDIMD        !! dim of GDATA
!   INTEGER,         intent(in) :: IMAXD, JMAXD               !! global domain size

!   REAL(KIND=KMD),  intent(in) :: GDATA(IDIMD, JDIMD, KDIMD) !! local data
!   CHARACTER(len=*),intent(in) :: HITEM                      !! name for identify
!   CHARACTER(len=*),intent(in) :: HTITL                      !! title
!   CHARACTER(len=*),intent(in) :: HUNIT                      !! unit
!   CHARACTER(len=*),intent(in) :: HDSET                      !! name of dataset

!   REAL(KIND=KMD),  intent(in) :: TIME                       !! time
!   REAL(KIND=KMD),  intent(in) :: TDUR                       !! representative time
!   INTEGER,         intent(in) :: JFILE                      !! output file No.
!   CHARACTER(len=*),intent(in) :: HDFMT                      !! data format
!   CHARACTER(len=*),intent(in) :: HCORD                      !! name of coordinate
!   CHARACTER(len=*),intent(in) :: HCLAS

!   REAL(KIND=KMD),  intent(in) :: TIME1                      !! time
!   REAL(KIND=KMD),  intent(in) :: TIME2                      !! time
!   REAL(KIND=KMD),  intent(in) :: RMISS                      !! missing value

!   CHARACTER(len=*),intent(in) :: HALON
!   CHARACTER(len=*),intent(in) :: HALAT
!   CHARACTER(len=*),intent(in) :: HASIG

!   INTEGER    ISTAZ, IENDZ, JSTAZ, JENDZ
!   INTEGER    LSIZE               ! LSIZE = ISIZE*JSIZE*KDIMD

!   LOGICAL,save :: OFIRST = .TRUE.
!   INTEGER,save :: JFPAR

!   CHARACTER(len=20)  :: HTIME
!   CHARACTER(len=NCC) :: HITEMX

!   IF ( OFIRST ) THEN
!      CALL GETJFP( JFPAR )
!      OFIRST = .FALSE.
!   ENDIF

!   CALL CSS2CC( HTIME, TIME )
!   HITEMX = HITEM

!   call co2ofs_global(ISTAZ, JSTAZ, HCORD)
!   IENDZ = ISTAZ + IDIMD - 1
!   JENDZ = JSTAZ + JDIMD - 1
!   LSIZE = IDIMD * JDIMD * KDIMD

!   CALL GTZWRZ &
!        & (GDATA, &
!        &  HITEM, HTITL, HUNIT, HDSET, &
!        &  TIME,  TDUR,  JFILE, HDFMT, HCLAS, &
!        &  TIME1, TIME2, RMISS, &
!        &  HALON, HALAT, HASIG, &
!        &  ISTAZ, IENDZ, JSTAZ, JENDZ, 1,     KDIMD, &
!        &  LSIZE)

!   WRITE (JFPAR,*) ' *** OUTPUT ', HITEMX, ' TIME= ', HTIME
!   RETURN
! END SUBROUTINE GTPWRT
! !!!_  - GDSEARCH       !! search by item-name and time
!!!_ + io/igtior.F
!!!_  & GTZRDZ
subroutine GTZRDZ &
     & (DDATA, HEAD,  IEOD,  &
     &  ISTA,  IEND,  JSTA,  JEND,  KSTA, KEND, &
     &  IFILE, HITEM, HDFMT, HCLAS, &
     &  DSIZE)
  use TOUZA_Nio,only: nio_msg=>msg,    get_item
  use TOUZA_Nio,only: nio_read_header, parse_header_size, nio_read_data
  use TOUZA_Nio,only: hi_ASTR1, hi_ASTR2, hi_ASTR3, hi_AEND1, hi_AEND2, hi_AEND3
  use TOUZA_Nio_miroc,only: KMD, NCC, NDC, init_common
  implicit none
  integer,            intent(in)  :: DSIZE
  real(kind=kmd),     intent(out) :: DDATA(DSIZE)  !! data
  character(len=_NCC),intent(out) :: HEAD(_NDC)
  integer,            intent(out) :: IEOD
  integer,            intent(out) :: ISTA, IEND
  integer,            intent(out) :: JSTA, JEND
  integer,            intent(out) :: KSTA, KEND
  integer,            intent(in)  :: IFILE
  character(len=*),   intent(in)  :: HITEM  !! name for identify
  character(len=*),   intent(in)  :: HDFMT  !! data format : neglected
  character(len=*),   intent(in)  :: HCLAS  !! driver : neglected

  logical,save :: ofirst = .TRUE.
  integer krect                 ! record type
  integer jerr
  integer n
  integer jfpar

  jerr = 0

  if (ofirst) then
     call init_common()
     ofirst = .FALSE.
  endif

  IEOD = 0
  call nio_read_header(jerr, HEAD, krect, IFILE)
  if (jerr .ne. 0) then ! error or no data
     IEOD = ABS(jerr)
     return
  endif

  n = parse_header_size(HEAD, 0)
  ! check buffer size
  if (n .gt. DSIZE) then
#if WITH_MIROC
     call GETJFP(jfpar)
     write(jfpar, *) '### GTZRDZ : AREA TOO SMALL:',  &
          &          ' ITEM: ' // TRIM(HITEM) // ',', &
          &          ' DATA:', n, ',AREA:', DSIZE
     call XABORT(1)
#else  /* not WITH_MIROC */
     call nio_msg('overflow in GTZRDZ', __MDL__)
#endif /* not WITH_MIROC */
     ieod = 1
     RETURN
  endif

  if (jerr.eq.0) call nio_read_data(jerr, DDATA, n, HEAD, krect, IFILE)
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
  use TOUZA_Nio,only: nio_read_header,nitem,KIOFS,WHENCE_ABS,sus_rseek
  use TOUZA_Nio_miroc,only: NCC, NDC
  implicit none
#if WITH_MIROC
# include "zioparam.F"
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
  call nio_read_header(jerr, HEAD, krect, IFILE)
  if (jerr.ne.0) then
     HEAD(1:nitem) = ' '
     if (is_error_match(jerr, ERR_EOF)) then
        IEOD = MM_EOF
     else
        IEOD = MM_ERR
     endif
     return
  endif
  call sus_rseek(jerr, IFILE, jpos, WHENCE_ABS)
  if (jerr.ne.0) IEOD = MM_ERR
  return
end subroutine GFPEEK
!!!_  & GFSKIP - skip a GTOOL3 record (header+body)
subroutine GFSKIP &
     & (IEOD, IFILE)
  use TOUZA_Nio,only: nio_skip_records
  implicit none
  integer,intent(out) :: IEOD
  integer,intent(in)  :: IFILE
  integer jerr
  IEOD = 0
  call nio_skip_records(jerr, 1, IFILE)
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
  use TOUZA_Nio,only: nio_msg=>msg
  use TOUZA_Nio_miroc,only:  vmiss_def
  use TOUZA_Nio_miroc,only:  put_item_time, init_common
  use TOUZA_Nio_header,only: litem, nitem, put_item, put_item_date
  use TOUZA_Nio_header,only: hi_DSET,  hi_ITEM,  hi_TITL1, hi_TITL2
  use TOUZA_Nio_header,only: hi_UNIT,  hi_TIME,  hi_TDUR,  hi_DFMT
  use TOUZA_Nio_header,only: hi_DATE,  hi_DATE1, hi_DATE2
  use TOUZA_Nio_header,only: hi_CDATE, hi_MDATE, hi_SIZE
  use TOUZA_Nio_header,only: hi_AITM1, hi_AITM2, hi_AITM3
  use TOUZA_Nio_header,only: hi_ASTR1, hi_AEND1, hi_ASTR2, hi_AEND2, hi_ASTR3, hi_AEND3
  use TOUZA_Nio_header,only: hi_MISS,  hi_DMIN,  hi_DMAX,  hi_DIVS,  hi_DIVL
  use TOUZA_Nio_header,only: hi_EDIT1, hi_EDIT2, hi_EDIT3
  use TOUZA_Nio_header,only: hi_ETTL1, hi_ETTL2, hi_ETTL3
  use TOUZA_Nio_record,only: get_default_header, nio_write_header, nio_write_data
  use TOUZA_Nio_record,only: REC_DEFAULT
# if OPT_WITH_NCTCDF
  use TOUZA_Nio_nctcdf,only: nct_define_write, nct_write_data
# endif
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
     call init_common()
     if (jerr.eq.0) call get_default_header(hdefv)
     ofirst = .FALSE.
  endif

  if (TIME .ge. TIME_PREV + LAZINESS) then
     call date_and_time(values=idtv(:))
     idtv(4:6) = idtv(5:7)
     call put_item_date(jerr, hdefv, idtv(1:6), hi_CDATE)
     call put_item_date(jerr, hdefv, idtv(1:6), hi_MDATE)
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

  if (HDFMT(1:1).eq.'C') then
#   if OPT_WITH_NCTCDF
     call nct_define_write(jerr, JFILE, head)
     if (jerr.eq.0) call nct_write_data(jerr, DDATA, n, JFILE, head)
#   else /* not OPT_WITH_NCTCDF */
     call nio_msg('NCTCDF disabled', __MDL__)
#   endif /* not OPT_WITH_NCTCDF */
  else
     krect = REC_DEFAULT
     call nio_write_header(jerr, head, krect, JFILE)
     ! if (ierr.eq.0) then
     !    if (levv.gt.1) call switch_urt_diag(wfile, jrec, udiag)
     ! endif
     if (jerr.eq.0) call nio_write_data(jerr, DDATA, n, head, krect, JFILE)
  endif
  return
end subroutine GTZWRZ
!!!_ + io/igtmeta.F
!!!_  & GTINID
!!!_  & PUT_DATETUPLE
!!!_  & GET_DATETUPLE
!!!_ + io/ibase.F
!!!_  - FOPEN
subroutine FOPEN &
     & (IOS, IFILE, HFILE, HACT, HFORM, HACCSS)
  use TOUZA_Nio,only: sus_open
# if OPT_WITH_NCTCDF
  use TOUZA_Nio,only: nct_open_write
# endif
  implicit none
  integer,         intent(out)   :: IOS
  integer,         intent(inout) :: IFILE
  character(len=*),intent(in)    :: HFILE
  character(len=*),intent(in)    :: HACT
  character(len=*),intent(in)    :: HFORM
  character(len=*),intent(in)    :: HACCSS

  integer handle
  character(LEN=9)  :: status
  character(LEN=9)  :: action
  character(LEN=6)  :: position

  if (HACT == 'APPEND') then
     action = 'WRITE'
     position = 'APPEND'
  else
     action = HACT
     position = 'ASIS'
  endif

  if (HFORM == 'GTOOL3') then
     call sus_open(IOS, IFILE, HFILE, ACTION=action, position=position)
     return
  endif
# if OPT_WITH_NCTCDF
  if (HFORM == 'NETCDF4') then
     action = HACT
     if (action(1:1).eq.'A') then
        action = 'WRITE'
        status = 'OLD'
     else if (action(1:1).eq.'W') then
        status = 'N'
     else
        status = 'O'
     endif
     if (action(1:1).eq.'W') then
        call nct_open_write(IOS, IFILE, HFILE, status)
     else
        ! read
        IOS = -1
     endif
     return
  endif
# endif /* OPT_WITH_NCTCDF */

  if (HACCSS == 'DIRECT') then
     open(UNIT=IFILE, FILE=HFILE, IOSTAT=IOS, &
          & ACCESS=HACCSS, FORM=HFORM, ACTION=action)
  else
     open(UNIT=IFILE, FILE=HFILE, IOSTAT=IOS, &
          & ACCESS=HACCSS, FORM=HFORM, ACTION=action, POSITION=position)
  endif
  return
end subroutine FOPEN
!!!_  - FREWND
subroutine FREWND &
     & (IFILE)
  implicit none
  integer,intent(in) :: IFILE
  rewind(IFILE)
  return
end subroutine FREWND
!!!_  - FINQUX
subroutine FINQUX &
     & (IERR,  OEXIST, OPND, IFILE, HFILE, HFORM)
  implicit none
  integer,         intent(out) :: IERR
  logical,         intent(out) :: OEXIST, OPND
  integer,         intent(out) :: IFILE
  character(len=*),intent(in)  :: HFILE
  character(len=*),intent(in)  :: HFORM
  inquire(FILE=HFILE, IOSTAT=IERR, EXIST=OEXIST, OPENED=OPND, NUMBER=IFILE)
  return
end subroutine FINQUX
!!!_  - FNUINI
subroutine FNUINI &
     & (IFILMN, IFILMX)
  use TOUZA_Std,only: kucat_black, set_category_bound
  use TOUZA_Nio_miroc,only: categ_nio, categ_normal, init_common
  implicit none
  integer jerr
  integer,intent(in)  :: IFILMN, IFILMX
  logical,save :: ofirst = .true.
  if (ofirst) then
     call init_common()
     ofirst = .false.
  endif
  call set_category_bound(jerr, kucat_black, IFILMN)
  call set_category_bound(jerr, categ_normal, IFILMX+1)
  return
end subroutine FNUINI
!!!_  - FNEWU
subroutine FNEWU &
     & (OFOUND, IFILE,  HFORM, IFILED)
  use TOUZA_Std,only: search_from_last, search_from_head, new_unit
  use TOUZA_Nio_miroc,only: categ_nio, categ_normal
  implicit none
  integer,         intent(out) :: IFILE
  logical,         intent(out) :: OFOUND
  character(len=*),intent(in)  :: HFORM
  integer,         intent(in)  :: IFILED
  integer kc
  integer ubase
  if (HFORM .EQ. 'GTOOL3') THEN
     kc = categ_nio
     if (IFILED.lt.0) then
        ubase = search_from_last
     else
        ubase = IFILED
     endif
  else
     kc = categ_normal
     if (IFILED.lt.0) then
        ubase = search_from_head
     else
        ubase = IFILED
     endif
  endif
  IFILE = new_unit(ubase, kc)
  OFOUND = IFILE.ge.0
  return
end subroutine FNEWU
!!!_@ test_nio_miroc - test program
#ifdef TEST_NIO_MIROC
program test_nio_miroc
  use TOUZA_Nio_miroc
  implicit none
  integer ierr

  ierr = 0
  if (ierr.eq.0) call init(ierr, levv=-1)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call finalize(ierr)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_nio_miroc

#endif /* TEST_NIO_MIROC */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

!!!_! jmz_buf.F90 - TOUZA/Jmz buffer manipulation
! Maintainer: SAITO Fuyuki
! Created: Oct 6 2023
#define TIME_STAMP 'Time-stamp: <2023/10/06 14:02:31 fuyuki jmz_buf.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2023
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
#ifndef   OPT_DESC_LEN
#  define OPT_DESC_LEN 1024
#endif
!!!_@ TOUZA/Jmz/buf - jmz buffer library
module Jmz_buf
!!!_ + Declaration
!!!_  - modules
  use TOUZA_Std,only: KFLT, KDBL, KIOFS
  use TOUZA_Std,only: is_msglev
  use TOUZA_Std,only: msglev_DETAIL,    msglev_NORMAL,    msglev_INFO,    msglev_DEBUG
  use TOUZA_Std,only: msglev_WARNING
  use TOUZA_Std,only: is_msglev_DETAIL, is_msglev_NORMAL, is_msglev_INFO, is_msglev_DEBUG
  use TOUZA_Std,only: is_msglev_WARNING
  use TOUZA_Std,only: trace_err

  use TOUZA_Nio,only: litem

  use Jmz_coor,only: loop_t
!!!_  - default
  implicit none
  private
!!!_  - parameter
  integer,parameter :: lcoor = 6

!!!_  - variable types
  integer,parameter,public :: kv_null = 0
  integer,parameter,public :: kv_int  = 1
  integer,parameter,public :: kv_flt  = 2
  integer,parameter,public :: kv_dbl  = 3

!!!_  - i/o units
  integer,save :: ulog = -1
  integer,save :: uerr = -1
!!!_  - global flags
  integer,save :: lev_verbose = 0
  integer,save :: dbgv = -1
  integer,save :: stdv = -1

!!!_  - string length
  integer,parameter :: lname = litem * 4
  integer,parameter :: ldesc = OPT_DESC_LEN

!!!_  - buffer property
  type buffer_t
     integer                 :: stt

     character(len=lname)    :: name              ! buffer name
     character(len=ldesc)    :: desc              ! description
     character(len=ldesc)    :: desc2             ! description (infix notation)

     integer                 :: ilev              ! infix notation level
     integer                 :: reff              ! reference file handle

     integer                 :: ncoor             ! number of coordinate
     integer                 :: ci(0:lcoor-1)
     type(loop_t)            :: pcp(0:lcoor-1)    ! physical (source) coordinate properties

     integer                 :: k = kv_null

     real(kind=KDBL)         :: undef
     real(kind=KDBL),pointer :: vd(:) => NULL()
     real(kind=KFLT),pointer :: vf(:) => NULL()
     integer,pointer         :: vi(:) => NULL()
  end type buffer_t
!!!_ + Procedures
contains
!!!_  - init
  subroutine init(ierr)
    implicit none
    integer,intent(out) :: ierr

    ierr = 0
  end subroutine init
!!!_  - finalize
  subroutine finalize(ierr, u)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer utmp

    ierr = 0
  end subroutine finalize

!!!_ + End jmzlib
end module Jmz_buf
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

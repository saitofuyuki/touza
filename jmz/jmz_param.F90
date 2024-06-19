!!!_! jmz_param.F90 - TOUZA/Jmz parameters placeholder
! Maintainer: SAITO Fuyuki
! Created: Oct 6 2023
#define TIME_STAMP 'Time-stamp: <2024/02/15 10:54:34 fuyuki jmz_param.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2024
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
#ifndef   OPT_PATH_LEN
#  define OPT_PATH_LEN 1024
#endif
!!!_@ TOUZA/Jmz/param - jmz parameters
module Jmz_param
  use TOUZA_Std,only: KFLT, KDBL, KQPL, KIOFS
  use TOUZA_Nio,only: litem, nitem, laxs, GFMT_END
!!!_ + Declaration
!!!_  - default
  implicit none
  public
!!!_  - strings
  integer,parameter,public :: lpath = OPT_PATH_LEN
!!!_  - flag constants
  integer,parameter,public :: fmode_default    = 0     ! default
  integer,parameter,public :: fmode_new        = 1     ! error if exist
  integer,parameter,public :: fmode_write      = 2     ! force overwrite
  integer,parameter,public :: fmode_append     = 3     ! append
!!!_  - float parameters
  integer,parameter :: KRSTD = __KBUF
  integer,parameter :: KBUF  = KRSTD      ! for compatibility

!!!_  - literal
  real(kind=KRSTD),parameter :: ZERO  = 0.0_KRSTD
  real(kind=KRSTD),parameter :: ONE   = 1.0_KRSTD
  real(kind=KRSTD),parameter :: TWO   = 2.0_KRSTD

  real(kind=KRSTD),parameter :: TRUE  = ONE
  real(kind=KRSTD),parameter :: FALSE = ZERO

  real(kind=KRSTD),parameter :: ULIMIT = + HUGE(ZERO)
  real(kind=KRSTD),parameter :: LLIMIT = - HUGE(ZERO)

  real(kind=KRSTD),parameter :: UNDEF  = -999.0_KRSTD
  ! real(kind=KRSTD),parameter :: UNDEF  = LLIMIT

!!!_  - coordinates
  integer,parameter :: lcoor = 6
  integer,parameter :: mcoor = laxs    ! standard max coordinates

!!!_  - file formats (except for gtool formats)
  integer,parameter :: cfmt_error       = -1
  integer,parameter :: cfmt_default     = 0
  integer,parameter :: cfmt_gtool_seq   = 1
  integer,parameter :: cfmt_gtool_cache = 2
  integer,parameter :: cfmt_gtool       = 8
  integer,parameter :: cfmt_ascii  = 1 + cfmt_gtool
  integer,parameter :: cfmt_binary = 2 + cfmt_gtool
  integer,parameter :: cfmt_flag_native = 0
  integer,parameter :: cfmt_flag_swap = 1
  integer,parameter :: cfmt_flag_big = 2
  integer,parameter :: cfmt_flag_little = 3
  integer,parameter :: cfmt_flags_bo = 4

  integer,parameter :: cfmt_binary_i4 = cfmt_binary
  integer,parameter :: cfmt_binary_r4 = cfmt_binary + cfmt_flags_bo
  integer,parameter :: cfmt_binary_r8 = cfmt_binary + cfmt_flags_bo * 2

  integer,parameter :: cfmt_cdf = cfmt_binary_r8 + 16
  ! integer,parameter :: cfmt_cdf_i4 = cfmt_cdf + 1
  ! integer,parameter :: cfmt_cdf_r4 = cfmt_cdf + 2
  ! integer,parameter :: cfmt_cdf_r8 = cfmt_cdf + 3

!!!_  - variable types
  integer,parameter :: kv_null = 0
  integer,parameter :: kv_int  = 1
  integer,parameter :: kv_flt  = 2
  integer,parameter :: kv_dbl  = 3

!!!_  - character (symbols) for command-line
  character(len=*),parameter :: param_sep = '='
  character(len=*),parameter :: rename_sep = '/'
  character(len=*),parameter :: range_sep = ':'
  character(len=*),parameter :: item_sep = ','
  character(len=*),parameter :: rec_append_sep = '/'
  character(len=*),parameter :: rec_num_sep = '+'

  character(len=*),parameter :: insert_coor = '+'
  character(len=*),parameter :: delete_coor = '-'

  character(len=*),parameter :: shape_sweep_stack  = '+'
  character(len=*),parameter :: shape_sweep_accum  = '++'
  character(len=*),parameter :: shape_sweep_reduce = '='

  ! path_sep used also for file/item separation until file_item_level
  character(len=*),parameter :: path_sep = '/'
  integer,parameter :: file_item_level = 2
  ! letters for separation between file and item
  character(len=*),parameter :: file_item_sep = '?'

!!!_  - character (symbols) for ascii output
  character(len=*),parameter :: amiss = '_'  ! character for missing value
  character(len=*),parameter :: aext  = '.'  ! character for external

!!!_ + common variables
!!!_  - convention parameters
  integer,save :: user_offset_bgn = 0     ! begin-index offset (user-friendly)
  integer,save :: user_offset_end = 0     ! end-index offset (user-friendly)

!!!_ + Procedures
contains
!!!_  - set_user_offsets - control offset for users
  subroutine set_user_offsets &
       & (ierr, off_bgn, off_end)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: off_bgn
    integer,intent(in),optional :: off_end
    ierr = 0
    user_offset_bgn = choice(user_offset_bgn, off_bgn)
    user_offset_end = choice(user_offset_end, off_end)
  end subroutine set_user_offsets
!!!_ + End jmz_param
end module Jmz_param
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

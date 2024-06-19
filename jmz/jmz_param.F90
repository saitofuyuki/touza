!!!_! jmz_param.F90 - TOUZA/Jmz parameters placeholder
! Maintainer: SAITO Fuyuki
! Created: Oct 6 2023
#define TIME_STAMP 'Time-stamp: <2023/11/01 12:47:18 fuyuki jmz_param.F90>'
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
!!!_@ TOUZA/Jmz/param - jmz parameters
module Jmz_param
  use TOUZA_Std,only: KFLT, KDBL, KIOFS
  use TOUZA_Nio_record, only: laxs
!!!_ + Declaration
!!!_  - default
  implicit none
  public
!!!_  - float parameters
  integer,parameter :: KBUF = __KBUF

  real(kind=KBUF),parameter :: ZERO  = 0.0_KBUF
  real(kind=KBUF),parameter :: ONE   = 1.0_KBUF

  real(kind=KBUF),parameter :: TRUE  = ONE
  real(kind=KBUF),parameter :: FALSE = ZERO

  real(kind=KBUF),parameter :: ULIMIT = + HUGE(ZERO)
  real(kind=KBUF),parameter :: LLIMIT = - HUGE(ZERO)

  real(kind=KBUF),parameter :: UNDEF  = -999.0_KBUF
  ! real(kind=KBUF),parameter :: UNDEF  = LLIMIT

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
    ierr = 0
  end subroutine finalize
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

!!!_! jmz_param.F90 - TOUZA/Jmz parameters placeholder
! Maintainer: SAITO Fuyuki
! Created: Oct 6 2023
#define TIME_STAMP 'Time-stamp: <2023/10/06 15:02:03 fuyuki jmz_param.F90>'
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
!!!_ + Declaration
!!!_  - default
  implicit none
  public
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
!!!_ + End jmz_param
end module Jmz_param
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

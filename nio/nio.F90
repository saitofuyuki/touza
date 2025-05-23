!!!_! nio.F90 - TOUZA/Nio manager
! Maintainer: SAITO Fuyuki
! Created: Oct 11 2021
#define TIME_STAMP 'Time-stamp: <2025/05/23 11:50:34 fuyuki nio.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021,2022,2023,2024
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_nio.h"
!!!_@ TOUZA_Nio - Nio interfaces
module TOUZA_Nio
!!!_ = declaration
!!!_  - modules
  use TOUZA_Nio_std,     ns_init=>init, ns_diag=>diag, ns_finalize=>finalize
  use TOUZA_Nio_std,     nio_gen_tag=>gen_tag
  use TOUZA_Nio_header,  nh_init=>init, nh_diag=>diag, nh_finalize=>finalize
  use TOUZA_Nio_record,  nr_init=>init, nr_diag=>diag, nr_finalize=>finalize
  use TOUZA_Nio_axis,    na_init=>init, na_diag=>diag, na_finalize=>finalize
  use TOUZA_Nio_sparse,  np_init=>init, np_diag=>diag, np_finalize=>finalize
  use TOUZA_Nio_cache,   nc_init=>init, nc_diag=>diag, nc_finalize=>finalize
  use TOUZA_Nio_control, nx_init=>init, nx_diag=>diag, nx_finalize=>finalize
  use TOUZA_Nio_bindc,   nb_init=>init, nb_diag=>diag, nb_finalize=>finalize
# if OPT_WITH_NCTCDF
  use TOUZA_Nio_nctcdf,  nn_init=>init, nn_diag=>diag, nn_finalize=>finalize
# endif
!!!_  - default
  implicit none
  public
!!!_  - private static
  integer,save,private :: init_mode = 0
  integer,save,private :: init_counts = 0
  integer,save,private :: diag_counts = 0
  integer,save,private :: fine_counts = 0
  integer,save,private :: lev_verbose = NIO_MSG_LEVEL
  integer,save,private :: err_default = ERR_NO_INIT
  integer,save,private :: ulog = unit_global

contains
!!!_ + common interfaces
!!!_  & init
  subroutine init(ierr, u, levv, mode, stdv, icomm)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode, stdv
    integer,intent(in),optional :: icomm
    integer lv, md, lmd, chmd

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
          chmd = MODE_SURFACE
          if (ierr.eq.0) call ns_init(ierr, u=ulog, levv=lv, mode=lmd, stdv=stdv, icomm=icomm)
          ! if (ierr.eq.0) call nh_init(ierr, u=ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_init(ierr, u=ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call na_init(ierr, u=ulog, levv=lv, mode=chmd)
          if (ierr.eq.0) call nc_init(ierr, u=ulog, levv=lv, mode=chmd)
          if (ierr.eq.0) call np_init(ierr, u=ulog, levv=lv, mode=chmd)
          if (ierr.eq.0) call nx_init(ierr, u=ulog, levv=lv, mode=chmd)
          if (ierr.eq.0) call nb_init(ierr, u=ulog, levv=lv, mode=chmd)
# if OPT_WITH_NCTCDF
          if (ierr.eq.0) call nn_init(ierr, u=ulog, levv=lv, mode=chmd)
# endif
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = ERR_FAILURE_INIT
    endif
    return
  end subroutine init

!!!_  & diag
  subroutine diag(ierr, u, levv, mode)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: levv
    integer utmp, lv, md, lmd, chmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, pkg=PACKAGE_TAG, grp=__GRP__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (is_msglev_normal(lv)) call msg(TIME_STAMP, u=utmp)
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          chmd = MODE_SURFACE
          if (ierr.eq.0) call ns_diag(ierr, utmp, levv=lv, mode=lmd)
          ! if (ierr.eq.0) call nh_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_diag(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call na_diag(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call nc_diag(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call np_diag(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call nx_diag(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call nb_diag(ierr, utmp, levv=lv, mode=chmd)
# if OPT_WITH_NCTCDF
          if (ierr.eq.0) call nn_diag(ierr, utmp, levv=lv, mode=chmd)
# endif
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv, mode
    integer utmp, lv, md, lmd, chmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = get_logu(u, ulog)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          chmd = MODE_SURFACE
          if (ierr.eq.0) call ns_finalize(ierr, utmp, levv=lv, mode=lmd)
          ! if (ierr.eq.0) call nh_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call nr_finalize(ierr, utmp, levv=lv, mode=lmd)
          if (ierr.eq.0) call na_finalize(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call nc_finalize(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call np_finalize(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call nx_finalize(ierr, utmp, levv=lv, mode=chmd)
          if (ierr.eq.0) call nb_finalize(ierr, utmp, levv=lv, mode=chmd)
# if OPT_WITH_NCTCDF
          if (ierr.eq.0) call nn_finalize(ierr, utmp, levv=lv, mode=chmd)
# endif
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize

!!!_ + user subroutines
!!!_ + private subroutines
end module TOUZA_Nio

!!!_@ test_nio - test program
#ifdef TEST_NIO
program test_nio
  use TOUZA_Nio
  implicit none
  integer ierr

  ierr = 0
  if (ierr.eq.0) call init(ierr, levv=+9, stdv=+9)
  if (ierr.eq.0) call diag(ierr)
  if (ierr.eq.0) call finalize(ierr, levv=+9)
101 format('FINAL = ', I0)
  write(*, 101) ierr
  stop
end program test_nio

#endif /* TEST_NIO */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

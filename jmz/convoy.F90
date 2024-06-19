!!!_! convoy.F90 - TOUZA/Jmz Convoy is for Transform
! Maintainer: SAITO Fuyuki
! Created: Jun 16 2023
#define TIME_STAMP 'Time-stamp: <2023/06/16 08:40:04 fuyuki convoy.F90>'
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
!!!_@ TOUZA/Jmz/convoy - ami/nio Transform/Interpolate/Projection mapping
program convoy
!!!_ + Declaration
  implicit none
!!!_  - variables
  integer ierr
!!!_ + Body
  ierr = 0
  if (ierr.eq.0) call init(ierr)
  if (ierr.eq.0) call finalize(ierr)
#if HAVE_FORTRAN_EXIT
  if (ierr.ne.0) then
     call trace_err(ierr)
     call exit(1)
  endif
#elif HAVE_FORTRAN_ERROR_STOP
  if (ierr.ne.0) then
     call trace_err(ierr)
     error stop 1
  endif
#else /* not HAVE_FORTRAN_ERROR_STOP */
  if (ierr.ne.0) then
     call trace_err(ierr)
  endif
#endif /* not HAVE_FORTRAN_ERROR_STOP */
  stop
!!!_ + Subroutines
contains
!!!_  - commons
!!!_   . init
  subroutine init(ierr)
    use jmzlib,only: jl_init=>init
    implicit none
    integer,intent(out) :: ierr
    ierr = 0
    call jl_init(ierr)
  end subroutine init
!!!_   . finalize
  subroutine finalize(ierr, u)
    use jmzlib,only: jl_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    ierr = 0
    call jl_finalize(ierr, u)
  end subroutine finalize
!!!_   . show_usage
  subroutine show_usage (ierr, u, levv)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer utmp
    integer lv

    ierr = 0
    utmp = choice(ulog, u)
    lv = choice(0, levv)
    return
  end subroutine show_usage
!!!_ + end convoy
end program convoy
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

!!!_! jmzlib.F90 - TOUZA/Jmz library
! Maintainer: SAITO Fuyuki
! Created: Nov 25 2021
#define TIME_STAMP 'Time-stamp: <2023/06/16 09:57:31 fuyuki jmzlib.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022, 2023
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
!!!_@ TOUZA/Jmz/lib - jmz library
module jmzlib
!!!_ + Declaration
!!!_  - modules
  use TOUZA_Std,only: KFLT, KDBL, KIOFS
  use TOUZA_Std,only: is_msglev
  use TOUZA_Std,only: msglev_DETAIL,    msglev_NORMAL,    msglev_INFO,    msglev_DEBUG
  use TOUZA_Std,only: msglev_WARNING
  use TOUZA_Std,only: is_msglev_DETAIL, is_msglev_NORMAL, is_msglev_INFO, is_msglev_DEBUG
  use TOUZA_Std,only: is_msglev_WARNING
  use TOUZA_Std,only: trace_err
!!!_  - default
  implicit none
  public
!!!_  - string length
  integer,parameter :: lpath = OPT_PATH_LEN
!!!_  - i/o units
  integer,save :: ulog = -1
  integer,save :: uerr = -1
!!!_  - global flags
  integer,save :: lev_verbose = 0
  integer,save :: dbgv = -1
  integer,save :: stdv = -1
!!!_  - convention parameters
  integer,save :: user_offset_bgn = 0     ! begin-index offset (user-friendly)
  integer,save :: user_offset_end = 0     ! end-index offset (user-friendly)
!!!_ + Procedures
contains
!!!_  - init
  subroutine init(ierr)
    use TOUZA_Std,only: env_init, MPI_COMM_NULL, stdout=>uout, stderr=>uerr
    implicit none
    integer,intent(out) :: ierr

    ierr = 0

    if (ierr.eq.0) call env_init(ierr, levv=stdv, icomm=MPI_COMM_NULL)
    if (ierr.eq.0) ulog = stdout
    if (ierr.eq.0) uerr = stderr
  end subroutine init
!!!_  - finalize
  subroutine finalize(ierr, u)
    use TOUZA_Std,only: choice
    use TOUZA_Std,only: env_diag, env_finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer utmp

    ierr = 0
    utmp = choice(ulog, u)
    if (ierr.eq.0) call env_diag(ierr, utmp, levv=dbgv)
    if (ierr.eq.0) call env_finalize(ierr, utmp, levv=dbgv)
  end subroutine finalize

!!!_  - message
  subroutine message(ierr, msg, iadd, fmt, levm, u, indent)
    use TOUZA_Std,only: choice, join_list
    implicit none
    integer,         intent(in)          :: ierr
    character(len=*),intent(in)          :: msg     ! msg
    integer,         intent(in),optional :: iadd(:)
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: levm    ! message level
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: indent
    integer jerr
    integer lv, utmp
    character(len=1024) :: txt
    integer skp
    jerr = 0
    lv = choice(0, levm)
    if (ierr.ne.0) then
       utmp = choice(uerr, u)
    else
       utmp = choice(ulog, u)
    endif
    skp = choice(0, indent)
    if (ierr.ne.0.or.is_msglev(lev_verbose, lv)) then
       if (present(iadd)) then
          if (size(iadd).gt.0) then
             if (present(fmt)) then
                write(txt, fmt, IOSTAT=jerr) iadd(:)
             else
                call join_list(jerr, txt, iadd(:), ldelim='(', rdelim=')')
             endif
             txt = trim(msg) // ' ' // trim(txt)
          endif
       else
          txt = msg
       endif
102    format('error:', I0, ': ', A)
101    format(A, A)
       if (ierr.ne.0) then
          write(utmp, 102) ierr, trim(txt)
       else
          write(utmp, 101) repeat(' ', skp), trim(txt)
       endif
    endif
  end subroutine message
!!!_ + End jmzlib
end module jmzlib
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

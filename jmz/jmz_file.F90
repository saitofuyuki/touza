!!!_! jmz_file.F90 - TOUZA/Jmz file manipulation
! Maintainer: SAITO Fuyuki
! Created: Apr 6 2024
#define TIME_STAMP 'Time-stamp: <2024/04/07 11:48:48 fuyuki jmz_file.F90>'
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
!!!_@ TOUZA/Jmz/file - jmz file library
module Jmz_file
!!!_ + Declaration
!!!_  - modules
  use Jmz_param
  use Jmz_base
!!!_  - default
  implicit none
  private
!!!_  - interfaces
!!!_  - public
public :: parse_file_item
!!!_ + Procedures
contains
!!!_  - parse_file_item - parse file + property string
  subroutine parse_file_item &
       & (ierr, mpath, mitem, &
       &  file, utest, seps,  rlev)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: mpath, mitem
    character(len=*),intent(in)          :: file
    integer,         intent(in),optional :: utest
    character(len=*),intent(in),optional :: seps             ! file-property separators
    integer,         intent(in),optional :: rlev             ! recursion level

    integer jerr_full
    integer jerr_filter
    integer utmp
    logical btmp
    integer lf
    integer jc
    character(len=1) :: csep

    ! A/B/file             A/B/file   + (null)     sequential
    ! A/B/file/            A/B/file   + (null)     cache
    ! A/B/file/item        A/B/file   + item       cache
    ! A/B/file/group/item  A/B/file   + group/item cache

    ! not implemented
    ! (deprecated)
    ! A/B/file?item        A/B/file   + item       cache
    ! A/B/file?group/item  A/B/file   + group/item cache
    ! A/B/?file?           A/B/?file  + (null)     sequential
    ! A/B/?file/           A/B/?file  + (null)     cache

    ierr = 0
    mpath = -1    ! not path name
    mitem = -1    ! sequential mode

    utmp = choice(-1, utest)
    if (utmp.lt.0) utmp = new_unit()
    if (utmp.lt.0) ierr = utmp
    if (ierr.eq.0) inquire(unit=utmp, opened=btmp, IOSTAT=ierr)
    if (ierr.eq.0) then
       if (btmp) ierr = ERR_PANIC
    endif
    jerr_full = -1
    jerr_filter = -1
    if (ierr.eq.0) then
       ! full-name
       call sus_open(jerr_full, utmp, file, ACTION='R', STATUS='O')
       if (jerr_full.eq.0) call sus_close(ierr, utmp, file)
    endif
    if (ierr.eq.0) then
       if (jerr_full.ne.0) then
          lf = len_trim(file)
          mitem = lf
          do jc = 0, file_item_level - 1
             mitem = index(file(1:mitem), path_sep, back=.true.)
             if (mitem.le.0) exit
             mitem = mitem - 1
             call sus_open(jerr_filter, utmp, file(1:mitem), ACTION='R', STATUS='O')
             if (jerr_filter.eq.0) then
                mpath = mitem
                mitem = mitem + 2
                if (mitem.gt.lf) mitem = 0
                call sus_close(ierr, utmp, file)
                exit
             endif
          enddo
       endif
    endif
    if (jerr_full.eq.0) then
       mpath = 0
       mitem = 0
    endif
    ! if (ierr.eq.0) call sus_close(ierr, utmp, file)
  end subroutine parse_file_item
!!!_ + End Jmz_file
end module Jmz_file
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

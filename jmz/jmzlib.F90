!!!_! jmzlib.F90 - TOUZA/Jmz library
! Maintainer: SAITO Fuyuki
! Created: Nov 25 2021
#define TIME_STAMP 'Time-stamp: <2022/09/05 09:37:01 fuyuki jmzlib.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "jmz.h"
!!!_@ TOUZA/Jmz/lib - jmz library
module jmzlib
  implicit none
  public
!!!_ + Declaration
  integer,parameter :: kfmt_ascii  = 1
  integer,parameter :: kfmt_binary = 2
  integer,parameter :: kfmt_gtool_legacy   = 3
  integer,parameter :: kfmt_gtool_trapiche = 4

  integer,parameter :: bforce = 1  ! force overwrite

  character(len=*),parameter :: sep_rect = ':'
!!!_ + Subroutines
contains
!!!_  - parse_rectype
  subroutine parse_rectype &
       & (ierr, krect, fmt)
    use TOUZA_Nio
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: krect
    character(len=*),intent(inout) :: fmt

    integer jp
    character(len=1) :: c

    ierr = 0
    jp = index(fmt, sep_rect)
    if (jp.eq.0) then
       krect = REC_ASIS
    else
       c = fmt(jp+1:jp+1)
       call upcase(c)
       select case(c)
       case('N')
          krect = REC_DEFAULT
       case('S')
          krect = REC_SWAP
       case('B')
          krect = REC_BIG
       case('L')
          krect = REC_LITTLE
       case default
          krect = REC_ASIS
       end select
       fmt = fmt(1:jp-1)
    endif

  end subroutine parse_rectype
!!!_  - open_write - open file to write complex
  subroutine open_write &
       & (ierr, uwrite, kfmt, fmt, wfile, kflag)
    use TOUZA_Std,only: upcase, uout, uerr
    use TOUZA_Std,only: sus_open
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: uwrite
    integer,         intent(out)   :: kfmt
    character(len=*),intent(inout) :: fmt
    character(len=*),intent(in)    :: wfile
    integer,         intent(in)    :: kflag
    character(len=16) CSTT

    ierr = 0

    call upcase(fmt)

    if (wfile.eq.' ' .or. wfile.eq.'-') then
       ! stdout (ascii mode only)
       uwrite = uout
       kfmt = kfmt_ascii
       if (fmt.eq.' '.or.fmt(1:1).eq.'(') then
          continue
       else
          ierr = -1
       endif
    else
       CSTT = 'NEW'
       if (IAND(kflag, bforce).ne.0) CSTT = 'REPLACE'
       if (fmt.eq.' '.or.fmt(1:1).eq.'(') then
          ! ascii mode
          kfmt = kfmt_ascii
          open(UNIT=uwrite, FILE=wfile, IOSTAT=ierr, &
               & ACTION='WRITE', STATUS=CSTT, FORM='FORMATTED', ACCESS='SEQUENTIAL')
       else if (fmt(1:1).eq.'B') then
          ! binary mode
          kfmt = kfmt_binary
          if (ierr.eq.0) call sus_open(ierr, uwrite, wfile, ACTION='W', STATUS=CSTT)
       else
          if (fmt(2:3).eq.'RT') then
             kfmt = kfmt_gtool_trapiche
          else
             kfmt = kfmt_gtool_legacy
          endif
          ! gtool mode
          if (ierr.eq.0) call sus_open(ierr, uwrite, wfile, ACTION='W', STATUS=CSTT)
       endif
       if (ierr.ne.0) then
201       format('error = ', I0, ' to open ', A)
          write(uerr, 201) ierr, trim(wfile)
          return
       endif
    endif
    if (ierr.eq.0) then
       if (kfmt.eq.kfmt_ascii &
            & .and. fmt.eq.' ') fmt = '(E16.9)'
    endif
    return
  end subroutine open_write
!!!_ + End jmzlib
end module jmzlib
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

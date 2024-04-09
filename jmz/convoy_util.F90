!!!_! convoy_util.F90 - TOUZA/Jmz/convoy utilities and parameters
! Maintainer: SAITO Fuyuki
! Created: Apr 10 2024
#define TIME_STAMP 'Time-stamp: <2024/04/17 06:59:07 fuyuki convoy_util.F90>'
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
!!!_@ TOUZA/Jmz/convoy_util - utilities and parameters
module convoy_util
!!!_ + Declaration
!!!_  - modules
  use Jmz_param
  use Jmz_base
  use TOUZA_Nio,only: litem, nitem
  use TOUZA_Nio,only: hi_DFMT, hi_ITEM, hi_DSET, hi_UNIT
  use TOUZA_Nio,only: put_header_cprop, put_item, get_default_header
  use TOUZA_Nio,only: nio_write_header, nio_write_data, nio_store_csr
!!!_  - default
  implicit none
  public
!!!_  - base parameters
  integer,parameter :: KTGT=KDBL
  integer,parameter :: lname=16
  integer,parameter :: lmsg=256
!!!_  - cartesian plane properties
  real(kind=KTGT),parameter :: cp_undef = -HUGE(0.0_KTGT)
  integer,parameter :: cp_spacing = 1
  integer,parameter :: cp_low     = 2
  integer,parameter :: cp_high    = 3
  integer,parameter :: cp_anchor  = 4
  integer,parameter :: cp_shift   = 5
  integer,parameter :: cp_ofs     = 6
  integer,parameter :: mem_cp = 6

  integer,parameter :: cflag_center   = 0
  integer,parameter :: cflag_boundary = 1

!!!_  - dset entries
  character(len=*),parameter :: DSET_PFX    = 'QX'
  character(len=*),parameter :: DSET_R2G    = DSET_PFX // 'RFILE'
  character(len=*),parameter :: DSET_PS2G   = DSET_PFX // 'PS2G'
  character(len=*),parameter :: DSET_PSPROP = DSET_PFX // 'PSPROP'

  integer,parameter :: xcmd_none = 0
  integer,parameter :: xcmd_noperm = 16384    ! permutation mask
  integer,parameter :: xcmd_r2g  = 1
  integer,parameter :: xcmd_ps2g = 2
  integer,parameter :: xcmd_loc  = 3
  integer,parameter :: xcmd_csr  = 16 + 1
  integer,parameter :: xcmd_qjds = 16 + 2
  integer,parameter :: xcmd_aux  = 32
  integer,parameter :: xcmd_fdplain  = 64 + 1 + xcmd_noperm
  integer,parameter :: xcmd_fdcycle  = 64 + 2 + xcmd_noperm

  character(len=*),parameter :: DSET_LOC  = 'AXLOC'
  character(len=*),parameter :: DSET_CLOC = 'C' // DSET_LOC
  character(len=*),parameter :: DSET_WGT  = 'AXWGT'
  character(len=*),parameter :: DSET_CWGT = 'C' // DSET_WGT

!!!_  - configurations
  character(len=*),parameter :: sep_item = ':'
  character(len=*),parameter :: sep_name = '/'
  character(len=*),parameter :: sep_coor = ','
  character(len=*),parameter :: sep_lpad = '-'
  character(len=*),parameter :: sep_rpad = '+'

  character(len=*),parameter :: opr_assign = '='
  character(len=*),parameter :: param_skip = '-'

  character(len=*),parameter :: disable_switch = '--'

!!!_  - flags
  integer,parameter :: span_positive = +1
  integer,parameter :: span_negative = -1
  integer,parameter :: span_both = 0

  integer,parameter :: special_end_param = -1
  integer,parameter :: batch_null = 0

  integer,parameter :: cset_err = -1
  integer,parameter :: cset_ordered = 0
  integer,parameter :: cset_shuffled = 1

!!!_ + Procedures
  contains
!!!_  & parse_output_var
  subroutine parse_output_var &
       & (ierr,  jitem, kbatch, kitem, jpos,  npos, &
       &  bname, bmask, mb,     vname, vmask, nv)
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: jitem
    integer,         intent(inout) :: kbatch, kitem
    integer,         intent(inout) :: jpos
    integer,         intent(in)    :: npos
    character(len=*),intent(in)    :: bname(0:*), vname(0:*)
    integer,         intent(in)    :: bmask(0:*), vmask(0:*)
    integer,         intent(in)    :: mb, nv

    integer,parameter :: larg = 256
    character(len=larg) :: atxt, utxt

    ierr = 0
    jitem = special_end_param

    loop_parse: do
       if (kbatch.eq.batch_null) then
          if (jpos.gt.npos) exit loop_parse
          if (ierr.eq.0) call get_param(ierr, atxt, jpos, ' ')
          if (ierr.eq.0) call upcase(utxt, atxt)
          if (ierr.eq.0) then
             jitem = find_first(vname(0:nv-1), utxt)
             if (jitem.lt.0) then
                kbatch = find_first(bname(1:mb), utxt, offset=1, no=-1)
                if (kbatch.gt.0) then
                   kitem = 0
                else
                   ierr = ERR_INVALID_PARAMETER
                   write(*, *) 'unknown variable/set: ', trim(atxt)
                   exit loop_parse
                endif
             endif
          endif
          jpos = jpos + 1
       endif
       if (kbatch.gt.batch_null) then
          jitem = -1
          do
             if (kitem.ge.nv) exit
             if (IAND(vmask(kitem), bmask(kbatch)).eq.bmask(kbatch)) then
                jitem = kitem
                exit
             endif
             kitem = kitem + 1
          enddo
          if (jitem.lt.0) then
             kbatch = batch_null
             cycle
          endif
          kitem = jitem + 1
       endif
       exit loop_parse
    end do loop_parse
  end subroutine parse_output_var

!!!_   . set_header_plane
  subroutine set_header_plane &
       & (ierr, head, dfmt, mx, xname, my, yname, mz, zname)
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(inout) :: head(*)
    character(len=*),intent(in)    :: dfmt
    integer,         intent(in)    :: mx, my
    character(len=*),intent(in)    :: xname, yname
    integer,         intent(in),optional :: mz
    character(len=*),intent(in),optional :: zname

    ierr = 0
    if (ierr.eq.0) call put_item(ierr, head, dfmt,  hi_DFMT)
    if (ierr.eq.0) call put_header_cprop(ierr, head, xname, (/1, mx/), 1)
    if (ierr.eq.0) call put_header_cprop(ierr, head, yname, (/1, my/), 2)
    if (present(mz).and.present(zname)) then
       if (ierr.eq.0) call put_header_cprop(ierr, head, zname,   (/1, mz/),  3)
    else if (present(mz).or.present(zname)) then
       ierr = ERR_INVALID_ITEM
    else
       if (ierr.eq.0) call put_header_cprop(ierr, head, ' ',   (/1, 1/),  3)
    endif

  end subroutine set_header_plane

!!!_   . set_header_geogr
  subroutine set_header_geogr &
       & (ierr, head, &
       &  jlatb, jlate, latname, &
       &  jlonb, jlone, lonname, &
       &  dfmt)
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(inout) :: head(*)
    integer,         intent(in)    :: jlatb, jlate
    integer,         intent(in)    :: jlonb, jlone
    character(len=*),intent(in)    :: latname, lonname
    character(len=*),intent(in)    :: dfmt
    integer jc

    ierr = 0
    jc = 1
    if (lonname.ne.' '.or.jlone.gt.jlonb) then
       if (ierr.eq.0) call put_header_cprop(ierr, head, lonname, (/jlonb+1, jlone/), jc)
       jc = jc + 1
    endif
    if (latname.ne.' '.or.jlate.gt.jlatb) then
       if (ierr.eq.0) call put_header_cprop(ierr, head, latname, (/jlatb+1, jlate/), jc)
       jc = jc + 1
    endif
    do
       if (jc.gt.3) exit
       if (ierr.eq.0) call put_header_cprop(ierr, head, ' ',   (/0, 0/),  jc)
       jc = jc + 1
    enddo

    if (ierr.eq.0) call put_item(ierr, head, dfmt,  hi_DFMT)

  end subroutine set_header_geogr

!!!_ + end module
end module convoy_util
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

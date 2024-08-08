!!!_! convoy_r2g.F90 - TOUZA/Jmz/convoy r2g R[AO]FILE manipulation
! Maintainer: SAITO Fuyuki
! Created: Apr 10 2024
#define TIME_STAMP 'Time-stamp: <2024/06/29 10:14:53 fuyuki convoy_r2g.F90>'
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
#define _PFX 'convoy:r2g: '
!!!_@ TOUZA/Jmz/convoy_r2g - tripolar/geographic transform
module convoy_r2g
!!!_ + Declaration
!!!_  - modules
  use Jmz_param
  use Jmz_base
  use Jmz_geogr
  use convoy_util
  implicit none
  public
!!!_ + Body
contains
!!!_  & main_r2g
  subroutine main_r2g &
       & (ierr, jpos, npos)
    use TOUZA_Nio,only: nio_record_std
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jpos
    integer,intent(in)    :: npos

    integer :: ufile
    character(len=lpath) :: ofile
    character(len=litem) :: head(nitem)
    character(len=lpath) :: rafile, rofile

    character(len=*),parameter :: ratag = 'RAFILE'
    character(len=*),parameter :: rotag = 'ROFILE'

    integer :: krect

    ierr = 0
    if (ierr.eq.0) call push_basename(ierr, 'r2g')

    if (ierr.eq.0) then
       call get_param(ierr, ofile, jpos, ' ')
       if (ierr.eq.0) then
          jpos = jpos + 1
       else
          ofile = ' '
       endif
    endif
    if (ierr.eq.0) then
       if (ofile.eq.' ') then
          ufile = -1
          head(:) = ' '
          krect = 0
       else
          krect = nio_record_std()
          call open_write(ierr, ufile, ofile, cfmt_gtool_seq)
          if (ierr.eq.0) call get_default_header(head)
       endif
    endif
    if (ierr.eq.0) call get_last_option(ierr, rafile, ratag, ratag(1:2), def=' ')
    if (ierr.eq.0) call get_last_option(ierr, rofile, rotag, rotag(1:2), def=' ')
    if (ierr.eq.0) then
       if (rafile.eq.' '.and.rofile.eq.' ') then
          ierr = ERR_FEW_ARGUMENTS
          call message(ierr, 'neither RAfile nor ROfile specified.')
       endif
    endif
    if (ierr.eq.0) then
       call batch_r2g &
            & (ierr,   jpos,   npos, &
            &  rafile, rofile, &
            &  ufile,  head,   krect)
    endif
    if (ierr.eq.0) call pop_basename(ierr, 'r2g')
  end subroutine main_r2g
!!!_  & batch_r2g
  subroutine batch_r2g &
       & (ierr,   jpos,   npos,    &
       &  rafile, rofile, &
       &  ufile,  head,   krect)
    use TOUZA_Ami,only: open_rafile_legacy, read_rafile_legacy
    use TOUZA_Ami,only: open_rofile_legacy, read_rofile_legacy1, read_rofile_legacy2
    use TOUZA_Ami,only: normalize_radata,   normalize_rodata
    use TOUZA_Ami,only: set_raheader,       set_roheader
    use TOUZA_Ami,only: write_radata_nio,   write_rodata_nio
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: jpos
    integer,intent(in)    :: npos
    character(len=*),intent(in) :: rafile, rofile
    integer,         intent(in) :: ufile
    character(len=*),intent(inout) :: head(*)
    integer,         intent(inout) :: krect

    character(len=lmsg) :: txt
    integer ua, uo
    logical swap

    integer kra, ij_amax, len_a2m, nxygdm
    integer kro, ij_omax, len_o2a, ijdim

    integer,parameter :: KMD = KDBL
    integer,       allocatable :: ij_ahead(:), ijrecov_a2m(:), ijc2o(:)
    real(kind=KMD),allocatable :: satm(:), ru(:), rv(:), rocn(:)

    real(kind=KMD),allocatable :: ruo(:), rvo(:), flandg(:)
    integer,       allocatable :: ij_ohead(:), ijrecov_o2c(:), ijo2c(:)
    real(kind=KMD),allocatable :: socn(:)

    integer :: amlat,      amlon
    integer :: alatdiv,    alondiv
    integer :: alatpad(2), alonpad(2)
    real(kind=KTGT)  :: alatofs,  alonofs
    character(len=lname) :: alatname, alonname
    character(len=4) :: alattype, alontype

    integer :: owlat,      owlon
    integer :: omlat,      omlon
    integer :: olatdiv,    olondiv
    integer :: olatpad(2), olonpad(2)
    real(kind=KTGT)  :: olatofs,  olonofs
    character(len=lname) :: olatname, olonname
    character(len=4) :: olattype, olontype

    integer :: cplon
    integer :: cmlat, cmlon
    integer :: ugeof

    integer,parameter :: offset_legacy = 1
    character(len=16) :: rfmt

    ierr = 0
    rfmt = 'PS'
    ua = -1
    uo = -1

    if (ierr.eq.0) call put_item(ierr, head, DSET_R2G, hi_DSET)
    ! RAfile
    if (ierr.eq.0) then
       if (rafile.eq.' ') then
          nxygdm = -1
       else
          ua = new_unit()
          ierr = min(0, ua)
       endif
       if (ua.ge.0) then
          if (ierr.eq.0) then
             call open_rafile_legacy &
                  & (ierr, kra, swap, ij_amax, len_a2m, nxygdm, rafile, ua)
101          format('rafile: ', A, 1x, L1, 2(1x, I0), 1x, I0)
             write(txt, 101) trim(rafile), swap, ij_amax, nxygdm, len_a2m
             call message(ierr, txt)
          endif
          if (ierr.eq.0) then
             allocate(ij_ahead(0:ij_amax), &
                  &   ijrecov_a2m(len_a2m), ijc2o(len_a2m), satm(len_a2m), &
                  &   ru(nxygdm),           rv(nxygdm),     rocn(nxygdm),  &
                  &   STAT=ierr)
          endif
          if (ierr.eq.0) then
             call read_rafile_legacy &
                  & (ierr,     &
                  &  ij_ahead, ijrecov_a2m, ijc2o,  satm, ru, rv, rocn, &
                  &  ij_amax,  len_a2m,     nxygdm, ua,   swap,   kra)
          endif
          if (ierr.eq.0) call sus_close(ierr, ua, rafile)
       endif
    endif

    ! ROfile
    if (ierr.eq.0) then
       if (rofile.eq.' ') then
          ijdim = -1
       else
          uo = new_unit()
          ierr = min(0, uo)
       endif
       if (uo.ge.0) then
          if (ierr.eq.0) then
             call open_rofile_legacy &
                  & (ierr, kro, swap, ijdim, rofile, uo)
111          format('rofile:i: ', A, 1x, L1, 1x, I0)
             write(txt, 111) trim(rofile), swap, ijdim
             call message(ierr, txt)
          endif
          if (ierr.eq.0) then
             call read_rofile_legacy1 &
                  & (ierr,     &
                  &  ij_omax,  len_o2a, &
                  &  ijdim,    uo,      swap)
          endif
112       format('rofile:ii: ', A, 1x, L1, 2(1x, I0), 1x, I0)
          write(*, 112) trim(rofile), swap, ij_omax, ijdim, len_o2a
          if (ierr.eq.0) then
             allocate(ij_ohead(0:ij_omax),  &
                  &   ijrecov_o2c(len_o2a), ijo2c(len_o2a), socn(len_o2a), &
                  &   ruo(ijdim),           rvo(ijdim),     flandg(ijdim), &
                  &   STAT=ierr)
          endif
          if (ierr.eq.0) then
             ijo2c(:) = 0
             socn(:)  = 0.0_KDBL
             call read_rofile_legacy2 &
                  & (ierr,     &
                  &  ij_ohead, ijrecov_o2c, ijo2c, socn, flandg, ruo, rvo, &
                  &  ijdim,    ij_omax,     uo,    swap, kro)
          endif
          if (ierr.eq.0) call sus_close(ierr, uo, rofile)
       endif
    endif

    ! domains
    if (ierr.eq.0) then
       call parse_geogr_atmos_base &
            & (ierr, &
            &  amlat, alatdiv, alatpad, alatofs, alatname, alattype, &
            &  amlon, alondiv, alonpad, alonofs, alonname, alontype, &
            &  'ATM')
    endif
    if (ierr.eq.0) then
       call set_check_atmos &
            & (ierr, &
            &  amlat, alatdiv, alatpad, alatofs, alatname, alattype, &
            &  amlon, alondiv, alonpad, alonofs, alonname, alontype, &
            &  ijdim)
    endif
    if (ierr.eq.0) then
       call parse_geogr_ocean_base &
            & (ierr,  &
            &  ugeof, &
            &  omlat, olatdiv, olatpad, olatofs, olatname, olattype, &
            &  omlon, olondiv, olonpad, olonofs, olonname, olontype)
    endif
    if (ierr.eq.0) then
       call set_check_ocean &
            & (ierr, &
            &  omlat, olatdiv, olatpad, olatofs, olatname, olattype, &
            &  omlon, olondiv, olonpad, olonofs, olonname, olontype, &
            &  nxygdm)
    endif
    if (ierr.eq.0) then
       owlon = olonpad(lpad)
       owlat = olatpad(lpad)
       cmlon = amlon * alondiv
       cplon = alonpad(rpad) * alondiv
       cmlat = amlat * alatdiv
    endif
    if (ua.ge.0) then
       if (ierr.eq.0) then
          call normalize_radata &
               & (ierr,        &
               &  ijrecov_a2m, ijc2o,   satm,  ru,    rv,    rocn, &
               &  ij_ahead,    ij_amax, &
               &  nxygdm,      omlon,   owlon, omlat, owlat, &
               &  cmlon,       cplon,   cmlat, offset_legacy)
       endif
       if (ierr.eq.0) then
          call set_raheader(ierr, head, olonname, olatname, nxygdm, omlon, omlat)
       endif
       if (ierr.eq.0) then
          call write_radata_nio &
               & (ierr, krect, &
               &  head,  ufile, rfmt,  &
               &  ijrecov_a2m, ijc2o, satm, ru, rv, rocn, ij_ahead, ij_amax)
       endif
       if (ierr.eq.0) then
          deallocate(ij_ahead, ijrecov_a2m, ijc2o, satm, ru, rv, rocn, STAT=ierr)
       endif
    endif
    if (uo.ge.0) then
       if (ierr.eq.0) then
          call normalize_rodata &
               & (ierr,        &
               &  ijrecov_o2c, ijo2c,   socn,  ruo,   rvo, flandg, &
               &  ij_ohead,    ij_omax, &
               &  omlon,       owlon,   omlat, owlat, &
               &  ijdim,       cmlon,   cplon, cmlat, offset_legacy)
       endif
       if (ierr.eq.0) call set_roheader(ierr, head, alonname, alatname, ijdim, cmlon, cmlat)
       if (ierr.eq.0) then
          call write_rodata_nio &
               & (ierr,   krect, &
               &  head,   ufile, rfmt,   &
               &  ijrecov_o2c, ijo2c, socn, ruo, rvo, flandg, ij_ohead, ij_omax)
       endif
       if (ierr.eq.0) then
          deallocate(ij_ohead, ijrecov_o2c, ijo2c, socn, ruo, rvo, flandg, STAT=ierr)
       endif
    endif

  end subroutine batch_r2g
!!!_  & parse_convoy_r2g
  subroutine parse_convoy_r2g &
       & (ierr, prime, jpos, npos, handle)
    implicit none
    integer,         intent(out)   :: ierr
    type(convoy_t),  intent(inout) :: prime
    integer,         intent(inout) :: jpos
    integer,         intent(in)    :: npos
    integer,         intent(in)    :: handle
    character(len=*),parameter :: proc = 'parse_convoy_r2g'

    ierr = 0
    if (ierr.eq.0) call parse_oprfile(ierr, prime, jpos, npos, handle, xcmd_qjds, 'IJC2O', 'SATM')
    if (ierr.eq.0) call parse_oprfile(ierr, prime, jpos, npos, handle, xcmd_qjds, 'IJO2C', 'SOCN')
    call trace_err(ierr, fun=proc)
  end subroutine parse_convoy_r2g

!!!_ + end module convoy_ps2g
end module convoy_r2g
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

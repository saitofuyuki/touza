!!!_! jmzcmp.F90 - TOUZA/Jmz nio(nng) comparison
! Maintainer: SAITO Fuyuki
! Created: Nov 28 2021
#define TIME_STAMP 'Time-stamp: <2022/09/02 21:09:07 fuyuki jmzcmp.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021,2022
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "jmz.h"
!!!_@ TOUZA/Jmz/cmp - jmz: nio comparison
program jmzcmp
!!!_ + Declaration
!!!_  - modules
  use TOUZA_Std,only: arg_diag, env_init
  use TOUZA_Nio, nio_init=>init, nio_diag=>diag, nio_finalize=>finalize
!!!_  - variables
  implicit none

  integer,parameter :: KBUF = KDBL

  integer ierr
  integer levv, dbgv, stdv
  integer japos

  real(kind=KBUF) :: atol, rtol
  integer kflag
  integer,parameter :: bforce = 1  ! force overwrite
!!!_ + Body
  ierr = 0

  levv = 0
  dbgv = -1
  stdv = -2
  call parse_options &
       & (ierr, japos, levv, dbgv, stdv, &
       &  atol, rtol,  kflag)
  if (ierr.eq.0) call env_init(ierr, levv=stdv)
  if (ierr.eq.0) call nio_init(ierr, levv=dbgv, stdv=stdv)
  if (ierr.eq.0) then
     call cmp_main(ierr, japos, levv, kflag)
  endif
  if (ierr.eq.0) call nio_diag(ierr)
  if (ierr.eq.0) call nio_finalize(ierr)
  if (ierr.eq.0) call arg_diag(ierr, levv=stdv)
  if (ierr.ne.0) then
     write(*, *) 'exit = ', ierr
  endif
  stop
!!!_ + Subroutines
contains
!!!_  - comparison main
  subroutine cmp_main &
       & (ierr, japos, levv, kflag)
    use TOUZA_Std,only: get_param, get_option, upcase, uout, uerr, is_error_match
    use TOUZA_Nio_std,only: KI32, KFLT, KDBL
    use TOUZA_Nio_header
    use TOUZA_Nio_record,only: set_urt_defs, switch_urt_diag, KCODE_CLIPPING
    use TOUZA_Trp,only: helper_props, compare_element
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(inout) :: japos
    integer,intent(in)    :: levv
    integer,intent(in)    :: kflag

    integer,parameter :: lpath = 1024
    integer,parameter :: lfmt = 128
    integer mv

    character(len=lpath)        :: rfile,        tfile,        dfile
    integer                     :: uref,         utgt,         udif
    real(kind=KBUF),allocatable :: vref(:),      vtgt(:),      vdfr(:),  vdfa(:)
    real(kind=KBUF)             :: vmisr,        vmist,        vmisd
    character(len=litem)        :: headr(nitem), headt(nitem), headd(nitem)
    character(len=lfmt)         :: fmtr,         fmtt,         fmtd
    integer                     :: nref,         ntgt
    integer                     :: krectr,       krectt,       krectd

    integer nemiss_r
    integer nemiss_t

    real(kind=KBUF),parameter :: zero = 0.0
    real(kind=KBUF),parameter :: one  = 1.0
    real(kind=KBUF),parameter :: vhuge = HUGE(zero)

    integer jrec
    integer j
    ! REFERENCE TARGET [OUTPUT]
    ! -d diff
    ! -r relative difference
    ! -s summary

    real(kind=KBUF) :: abs_max,  abs_min
    real(kind=KBUF) :: aerr_max, rerr_max, zerr_max
    real(kind=KBUF) :: c, d
    character(len=64) :: tamax, tamin
    character(len=64) :: taerr, trerr, tzerr
    character(len=lhead) :: vitemr, vitemt, vitemd

    character(len=128) :: tag
    integer udiag
    character(len=16) CSTT

    ierr = 0
    udiag = uout

    japos = japos + 1
    if (ierr.eq.0) call get_param(ierr, rfile, japos, ' ')
    japos = japos + 1
    if (ierr.eq.0) call get_param(ierr, tfile, japos, ' ')
    japos = japos + 1
    if (ierr.eq.0) call get_param(ierr, dfile, japos, ' ')

    if (rfile.eq.' '.or.tfile.eq.' ') then
       ierr = -1
109    format('need arguments: REFERENCE TARGET [OUTPUT]')
       write(uerr, 109)
       return
    endif

119 format('error=', I0, ' at ', A, 1x A)
    uref  = max(uerr, uout, 10) + 1
    if (ierr.eq.0) call sus_open(ierr, uref, rfile, ACTION='R', STATUS='O')
    if (ierr.ne.0) then
       write(uerr, 119) ierr, 'read', trim(rfile)
       return
    endif
    utgt  = uref + 1
    if (ierr.eq.0) call sus_open(ierr, utgt, tfile, ACTION='R', STATUS='O')
    if (ierr.ne.0) then
       write(uerr, 119) ierr, 'read', trim(tfile)
       return
    endif
    if (dfile.eq.' ') then
       udif = -99
    else
       udif = utgt + 1
       CSTT = 'NEW'
       if (IAND(kflag, bforce).ne.0) CSTT = 'REPLACE'
       if (ierr.eq.0) call sus_open(ierr, udif, dfile, ACTION='W', STATUS=CSTT)
       if (ierr.ne.0) then
          write(uerr, 119) ierr, 'write', trim(dfile)
          return
       endif
    endif

    mv = 2**16
    if (ierr.eq.0) allocate(vref(mv), vtgt(mv), vdfr(mv), vdfa(mv), STAT=ierr)

    jrec = 0
139 format('inconsistent record size at ', I0, ' : ', I0, 1x, I0)

1001 format(I8, 1x, A16, 1x, A11, 1x, A11, 1x, A11, 1x, A11, 1x, A11, 1x, I8, 1x, I8)
1002 format(A8, 1x, A16, 1x, A11, 1x, A11, 1x, A11, 1x, A11, 1x, A11, 1x, A8, 1x, A8)
    write(uout, 1002) 'rec', 'item', &
         & 'abs.min',  'abs.max',  &
         & 'abs.diff', 'rel.diff', 'zero.diff', &
         & 'miss.tgt', 'miss.ref'
    do
       ! reference
       if (ierr.eq.0) call nio_read_header(ierr, headr, krectr, uref)
       if (is_error_match(ierr, ERR_EOF)) then
          ierr = 0
          exit
       endif
       if (ierr.eq.0) then
          nref = parse_header_size(headr, 0)
          if (nref.gt.mv) then
             mv = nref
             deallocate(vref, vtgt, vdfr, vdfa, STAT=ierr)
             if (ierr.eq.0) allocate(vref(mv), vtgt(mv), vdfr(mv), vdfa(mv), STAT=ierr)
          endif
       endif
       if (ierr.eq.0) then
          if (levv.gt.1) call switch_urt_diag(rfile, jrec, udiag)
       endif
       if (ierr.eq.0) call nio_read_data(ierr, vref, nref, headr, krectr, uref)
       ! target
       if (ierr.eq.0) call nio_read_header(ierr, headt, krectt, utgt)
       if (is_error_match(ierr, ERR_EOF)) then
          ierr = 0
          exit
       endif
       if (ierr.eq.0) then
          ntgt = parse_header_size(headt, 0)
          if (nref.ne.ntgt) then
             write(uerr, 139) jrec, nref, ntgt
             ierr = -1
             exit
          endif
       endif
       if (ierr.eq.0) then
          if (levv.gt.1) call switch_urt_diag(tfile, jrec, udiag)
       endif
       if (ierr.eq.0) call nio_read_data(ierr, vtgt, ntgt, headt, krectt, utgt)

       if (ierr.ne.0) exit

       ! comparison operation
       if (ierr.eq.0) call get_item(ierr, headr, vmisr, hi_MISS, def=zero)
       if (ierr.eq.0) call get_item(ierr, headt, vmist, hi_MISS, def=zero)
       if (ierr.eq.0) call get_item(ierr, headt, vitemt, hi_ITEM)
       if (ierr.eq.0) call get_item(ierr, headr, vitemr, hi_ITEM)

1021   format(A, '-', A)

       if (ierr.eq.0) then
          vmisd = vmisr
          aerr_max = -vhuge
          rerr_max = -vhuge
          zerr_max = -vhuge
          abs_max = -vhuge
          abs_min = +vhuge
          nemiss_t = 0
          nemiss_r = 0
          do j = 1, nref
             if (vref(j).eq.vmisr) then
                if (vtgt(j).eq.vmist) then
                   vdfr(j) = vmisd
                   vdfa(j) = vmisd
                else
                   vdfa(j) = - vhuge
                   vdfr(j) = - vhuge
                   nemiss_t = nemiss_t + 1
                endif
             else if (vtgt(j).eq.vmist) then
                vdfa(j) = + vhuge
                vdfr(j) = + vhuge
                nemiss_r = nemiss_r + 1
             else
                abs_max = max(abs_max, abs(vref(j)))
                d = abs(vtgt(j) - vref(j))
                aerr_max = max(aerr_max, d)
                if (vref(j).eq.zero) then
                   if (vtgt(j).eq.zero) then
                      if (sign(one, vref(j)).eq.sign(one, vtgt(j))) then
                         vdfr(j) = zero
                         vdfa(j) = zero
                      else
                         vdfr(j) = sign(zero, - one)
                         vdfa(j) = sign(zero, - one)
                      endif
                   else
                      zerr_max = max(zerr_max, d)
                      vdfr(j) = sign(vhuge, vtgt(j))
                      vdfa(j) = sign(vhuge, vtgt(j))
                   endif
                else
                   abs_min = min(abs_min, abs(vref(j)))
                   vdfa(j) = vtgt(j) - vref(j)
                   c = (vtgt(j) - vref(j)) / vref(j)
                   vdfr(j) = c
                   rerr_max = max(rerr_max, abs(c))
                endif
             endif
          enddo
          if (vitemr.eq.vitemt) then
             vitemd = vitemr
          else
             write(vitemd, 1021) trim(vitemt), trim(vitemr)
          endif
          call cmp2str(tamax, abs_max)
          call cmp2str(tamin, abs_min)
          call cmp2str(taerr, aerr_max)
          call cmp2str(trerr, rerr_max)
          call cmp2str(tzerr, zerr_max)
          write(uout, 1001) jrec, trim(vitemd), &
               & trim(tamin), trim(tamax), &
               & trim(taerr), trim(trerr), trim(tzerr), &
               & nemiss_t, nemiss_r
          if (levv.gt.0) then
             do j = 1, nref
1011            format(I0, ':', A, ':', I0)
1012            format(I0, ':', A, ':', I0)
                if (vdfr(j).ne.vmisd.and.vdfr(j).ne.zero) then
                   write(tag, 1011) jrec, trim(vitemd), j
                   if (abs(vdfr(j)).gt.rtol .and. abs(vdfa(j)).gt.atol) then
                      call compare_element(ierr, vref(j), vtgt(j), tag=tag, u=uout)
                   endif
                else if (vref(j).eq.zero.and.sign(one,vdfr(j)).eq.-one) then
                   write(tag, 1012) jrec, trim(vitemd), j
                   call compare_element(ierr, vref(j), vtgt(j), tag=tag, u=uout)
                endif
             enddo
          endif
       endif
       ! output
       if (udif.ge.0) then
          if (ierr.eq.0) then
             headd(:) = headr(:)
             krectd = krectr
          endif
          if (ierr.eq.0) call put_item(ierr, headd, 'UR8', hi_DFMT)
          if (ierr.eq.0) call put_item(ierr, headd, vmisd, hi_MISS)
          if (ierr.eq.0) call put_item(ierr, headd, vmisd, hi_MISS)
          if (ierr.eq.0) then
             write(vitemd, 1021) trim(vitemt), trim(vitemr)
             if (ierr.eq.0) call put_item(ierr, headd, trim(vitemd), hi_ITEM)
          endif
          if (ierr.eq.0) call nio_write_header(ierr, headd, krectd, udif)
          if (ierr.eq.0) call nio_write_data(ierr, vdfa, nref, headd, krectd, udif)
       endif
       jrec = jrec + 1
    enddo

    return
  end subroutine cmp_main
!!!_  - cmp2str
  subroutine cmp2str (str, v)
    implicit none
    character(len=*),intent(out) :: str
    real(kind=KBUF), intent(in)  :: v
    real(kind=KBUF),parameter :: zero = 0.0
101 format(E10.3)
    if (v.gt.zero) then
       write(str, 101) v
    else if (v.eq.zero) then
       str = '0'
    else
       str = '_'
    endif
  end subroutine cmp2str

!!!_  - options
  subroutine parse_options &
       & (ierr, japos, levv, dbgv, stdv, &
       &  atol, rtol,  kflag)
    use TOUZA_Std,only: arg_init, parse, get_param
    implicit none
    integer,        intent(out)   :: ierr
    integer,        intent(out)   :: japos
    integer,        intent(inout) :: levv
    integer,        intent(inout) :: dbgv, stdv
    integer,        intent(out)   :: kflag
    real(kind=KBUF),intent(out)   :: atol, rtol

    integer,parameter   :: lstr = 1024
    character(len=lstr) :: astr
    real(kind=KBUF),parameter :: zero = 0.0

    ierr = 0
    japos = 0
    kflag = 0
    atol = zero
    rtol = zero

    if (ierr.eq.0) call arg_init(ierr, levv=stdv)
    if (ierr.eq.0) call parse(ierr)
    do
       if (ierr.ne.0) then
          ierr = min(0, ierr)
          exit
       endif
       japos = japos + 1
       call get_param(ierr, astr, japos)
       if (ierr.eq.0) then
          if (astr(1:1).eq.'-') then
             if (astr.eq.'-v') then          ! -v    - increase verbosity
                levv = max(-1, levv) + 1
             else if (astr.eq.'-q') then     ! -q    - decrease verbosity
                levv = levv - 1
             else if (astr.eq.'-d') then     ! -d    - debug
                dbgv = 99
                stdv = 99
             else if (astr.eq.'-f') then     ! -f    - force overwrite
                kflag = IOR(kflag, bforce)
             else if (astr.eq.'-a') then     ! -a TOL   - absolute tolerance
                japos = japos + 1
                call get_param(ierr, atol, japos, zero)
                if (ierr.ne.0) exit
             else if (astr.eq.'-r') then     ! -r TOL   - relative tolerance
                japos = japos + 1
                call get_param(ierr, rtol, japos, zero)
                if (ierr.ne.0) exit
             else
                write(*, *) 'invalid arguments: ', trim(astr)
                ierr = -1
             endif
          else
             japos = japos - 1
             exit
          endif
       endif
    enddo
  end subroutine parse_options
!!!_ + End jmzcmp
end program jmzcmp
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

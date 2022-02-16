!!!_! jmzconv.F90 - TOUZA/Jmz nng conversion
! Maintainer: SAITO Fuyuki
! Created: Nov 25 2021
#define TIME_STAMP 'Time-stamp: <2021/12/24 11:00:00 fuyuki jmzconv.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2021
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "jmz.h"
!!!_@ TOUZA/Jmz/conv - jmz: nng convertion
program jmzconv
!!!_ + Declaration
!!!_  - modules
  use TOUZA_Std,only: arg_diag, env_init
  use TOUZA_Nng, nng_init=>init, nng_diag=>diag, nng_finalize=>finalize
!!!_  - variables
  implicit none
  integer ierr
  integer levv, dbgv, stdv
  integer japos

  integer,parameter :: kfmt_ascii  = 1
  integer,parameter :: kfmt_binary = 2
  integer,parameter :: kfmt_gtool_legacy   = 3
  integer,parameter :: kfmt_gtool_trapiche = 4

  integer kflag
  integer,parameter :: bforce = 1  ! force overwrite
  integer,parameter :: larg = 1024
  character(len=larg) :: tlists
!!!_ + Body
  ierr = 0

  levv = 0
  dbgv = -1
  stdv = -2
  call parse_options (ierr, japos, levv, dbgv, stdv, kflag, tlists)
  if (ierr.eq.0) call env_init(ierr, levv=stdv)
  if (ierr.eq.0) call nng_init(ierr, levv=dbgv, stdv=stdv)

  if (ierr.eq.0) call conv_main(ierr, japos, levv, kflag, tlists)

  if (ierr.eq.0) call nng_diag(ierr)
  if (ierr.eq.0) call nng_finalize(ierr)
  if (ierr.eq.0) call arg_diag(ierr, levv=stdv)
  if (ierr.ne.0) then
     write(*, *) 'exit = ', ierr
  endif
  stop
!!!_ + Subroutines
contains
!!!_  - conversion main
  subroutine conv_main &
       & (ierr, japos, levv, kflag, tlists)
    use TOUZA_Std,only: get_param, get_option, upcase, uout, uerr, is_error_match
    use TOUZA_Nng_std,only: KI32, KFLT, KDBL
    use TOUZA_Nng_header
    use TOUZA_Nng_record,only: set_urt_defs
    use TOUZA_Trp,only: helper_props, parse_codes
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: japos
    integer,         intent(in)    :: levv
    integer,         intent(in)    :: kflag
    character(len=*),intent(in)    :: tlists

    integer,parameter :: KBUF = KDBL

    integer,parameter :: lpath = 1024
    character(len=lpath) :: rfile, wfile
    integer uread, uwrite

    integer,parameter :: lfmt = 128
    character(len=lfmt)  :: fmt

    character(len=litem) :: head(nitem)
    integer mv, n
    real(kind=KBUF),pointer :: v(:)
    real(kind=KBUF) :: vmiss

    integer j, jrec
    integer krect
    integer kfmt
    integer kopts(16)
    integer xran(2), xbits, xbtm, mbits
    integer,parameter :: xdef = - HUGE(0) - 1
    character(len=32) :: ccode
    real(kind=KBUF) :: refr(3)
    real(kind=KBUF),parameter :: rdef = -1.0
    integer kcode

    integer,parameter :: lline = 64
    character(len=lline) :: txt
    integer udiag
    integer jrbgn, jrend
    integer jb, je, lt, jm

    ierr = 0
    kfmt = 0

    udiag = uout

    japos = japos + 1
    if (ierr.eq.0) call get_param(ierr, rfile, japos, ' ')
    japos = japos + 1
    if (ierr.eq.0) call get_param(ierr, wfile, japos, ' ')
    japos = japos + 1
    if (ierr.eq.0) call get_param(ierr, fmt, japos, ' ')

    if (rfile.eq.' ') then
       ierr = -1
109    format('need arguments: INPUT [OUTPUT [FORMAT]]')
       write(uerr, 109)
       return
    endif

    call upcase(fmt)

119 format('error=', I0, ' at ', A, 1x A)

    uread  = max(uout, 10) + 1
    if (ierr.eq.0) call sus_open(ierr, uread,  rfile, ACTION='R')
    if (ierr.ne.0) then
       write(uerr, 119) ierr, 'read', trim(rfile)
       return
    endif

    uwrite = uread + 1
    if (ierr.eq.0) call open_write (ierr, uwrite, kfmt, fmt, wfile, kflag)
    ! fmt:  [um]r[48][:[nsbl]]
    !       [um]i4
    !       [um]ry*
    !       [um]rt*[/properties...][:nsbl]
    !       br[48]:[nsbl]
    !       bi4:[nsbl]
    !       (FORMAT)
    call set_urt_defs(kopts)
    kcode = 0
    if (ierr.eq.0) then
       if (kfmt.eq.kfmt_gtool_trapiche) then
          xran(:) = xdef
          refr(:) = rdef
          if (ierr.eq.0) call get_option(ierr, xbits,     'x', -1)            ! x=EXPONENT-BITS
          if (ierr.eq.0) call get_option(ierr, xran(1:2), 'e', unset=.TRUE.)  ! e=LOWER,UPPER-EXPONENTS
          if (ierr.eq.0) call get_option(ierr, ccode,     'c', ' ')           ! c=[CR]+
          if (ierr.eq.0) call get_option(ierr, refr,      'r', unset=.TRUE.)  ! r=REFMIN,REFMAX[,RES]
          if (ierr.eq.0) then
             if (xran(1).le.xdef) kopts(PROP_URT_XBOTTOM) = xran(1)
             if (xran(2).le.xdef) kopts(PROP_URT_XTOP)    = xran(2)
             if (xbits.ge.0)      kopts(PROP_URT_XBITS)   = xbits
          endif
          if (ierr.eq.0) call parse_codes(ierr, kcode, ccode)
          if (ierr.eq.0) kopts(PROP_URT_CODES) = kcode
       endif
       if (ierr.eq.0) then
          if (refr(1).gt.0.0) then
             call helper_props(mbits, xbits, xbtm, refr(1), refr(2), refr(3))
             kopts(PROP_URT_XBOTTOM)  = xbtm
             kopts(PROP_URT_XBITS)    = xbits
             kopts(PROP_URT_MANTISSA) = mbits
          endif
       endif
    endif

    mv = 2**16
    if (ierr.eq.0) allocate(v(mv), STAT=ierr)

    jrec = 0
    if (tlists.eq.' ') then
       jrbgn = 0
       jrend = -1
       call conv_core_d &
            & (ierr,   jrec,   v,     &
            &  uread,  uwrite, udiag, jrbgn, jrend, &
            &  wfile,  fmt,    kfmt,  kopts)
    else
       lt = len_trim(tlists)
       jb = 0
       do
          if (jb.ge.lt) exit
          je = index(tlists(jb+1:lt), ',') + jb
          if (je.eq.jb) je = lt + 1
          jm = index(tlists(jb+1:je-1), '-')
          if (jm.gt.0) then
             jm = jm + jb
             if (jb+1.gt.jm-1) then
                jrbgn = 0
             else
                read(tlists(jb+1:jm-1), *, IOSTAT=ierr) jrbgn
             endif
             if (jm+1.gt.je-1) then
                jrend = -1
             else
                read(tlists(jm+1:je-1), *, IOSTAT=ierr) jrend
             endif
          else
             read(tlists(jb+1:je-1), *, IOSTAT=ierr) jrbgn
             jrend = jrbgn
          endif
          if (ierr.eq.0) then
             call conv_core_d &
                  & (ierr,   jrec,   v,     &
                  &  uread,  uwrite, udiag, jrbgn, jrend, &
                  &  wfile,  fmt,    kfmt,  kopts)
          endif
          if (ierr.ne.0) exit
          jb = je
       enddo
    endif
    if (ierr.eq.0) call sus_close(ierr, uread, rfile)
    if (ierr.eq.0.and.uwrite.ne.uout) call sus_close(ierr, uwrite, wfile)
    if (ierr.eq.0) deallocate(v, STAT=ierr)
    return
  end subroutine conv_main

!!!_  - conv_core - conversion core
  subroutine conv_core_d  &
       & (ierr,   jrec,   v,     &
       &  uread,  uwrite, udiag, jrbgn, jrend, &
       &  wfile,  fmt,    kfmt,  kopts)
    use TOUZA_Nng,only: KI32, KFLT, KDBL, sus_rseek, WHENCE_BEGIN
    use TOUZA_Std,only: is_error_match
    implicit none
    integer,parameter :: KARG = KDBL
    integer,         intent(out)         :: ierr
    integer,         intent(inout)       :: jrec
    real(kind=KARG), intent(out),pointer :: v(:)
    integer,         intent(in)          :: uread, uwrite, udiag
    integer,         intent(in)          :: jrbgn, jrend
    character(len=*),intent(in)          :: fmt
    character(len=*),intent(in)          :: wfile
    integer,         intent(in)          :: kfmt
    integer,         intent(in)          :: kopts(:)

    integer krect
    character(len=litem) :: head(nitem)
    integer mv, n, j
    real(kind=KDBL) :: vmiss
    integer,parameter :: lline = 64
    character(len=lline) :: txt
    integer jpos

    ierr = 0
    mv = size(v)

    if (jrbgn.lt.jrec) then
       jrec = 0
       call sus_rseek(ierr, uread, WHENCE=WHENCE_BEGIN)
       inquire(uread, POS=jpos)
    endif
    if (ierr.eq.0) then
       call nng_skip_records(ierr, jrbgn - jrec, uread)
       if (ierr.eq.0) jrec = jrbgn
    endif
    do
       if (ierr.eq.0) call nng_read_header(ierr, head, krect, uread)
       if (is_error_match(ierr, ERR_EOF)) then
          ierr = 0
          exit
       endif
       if (ierr.eq.0) then
          n = parse_header_size(head, 0)
          if (n.gt.mv) then
             mv = n
             deallocate(v, STAT=ierr)
             if (ierr.eq.0) allocate(v(mv), STAT=ierr)
          endif
       endif
       if (ierr.eq.0) call nng_read_data(ierr, v, n, head, krect, uread)
       if (kfmt.eq.kfmt_ascii) then
          if (ierr.eq.0) call get_item(ierr, head, vmiss, hi_MISS)
          if (ierr.eq.0) then
301          format(I0, 1x, I0, 1x, A)
             do j = 1, n
                if (v(j).eq.vmiss) then
                   write(txt, '(''_'')')
                else
                   write(txt, fmt) v(j)
                endif
                write(uwrite, 301) jrec, j, trim(txt)
             enddo
          endif
       else
          if (ierr.eq.0) call put_item(ierr, head, trim(fmt), hi_DFMT)
          if (ierr.eq.0) call nng_write_header(ierr, head, krect, uwrite)
          if (ierr.eq.0) then
             if (levv.gt.1) call switch_urt_diag(wfile, jrec, udiag)
          endif
          if (ierr.eq.0) call nng_write_data(ierr, v, n, head, krect, uwrite, kopts)
       endif
       if (ierr.ne.0) exit
       jrec = jrec + 1
       if (jrec-1.eq.jrend) exit
    enddo
    return
  end subroutine conv_core_d
!!!_  - open_write - open file to write complex
  subroutine open_write &
       & (ierr, uwrite, kfmt, fmt, wfile, kflag)
    use TOUZA_Std,only: upcase, uout, uerr
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

!!!_  - options
  subroutine parse_options (ierr, japos, levv, dbgv, stdv, kflag, tlists)
    use TOUZA_Std,only: arg_init, parse, get_param
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: japos
    integer,         intent(inout) :: levv
    integer,         intent(inout) :: dbgv, stdv
    integer,         intent(out)   :: kflag
    character(len=*),intent(out)   :: tlists

    integer,parameter   :: lstr = 1024
    character(len=lstr) :: astr

    ierr = 0
    japos = 0
    kflag = 0
    tlists = ' '
    if (ierr.eq.0) call arg_init(ierr, levv=stdv)
    if (ierr.eq.0) call parse(ierr)
    if (ierr.eq.0) then
       do
          japos = japos + 1
          call get_param(ierr, astr, japos)
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
             else if (astr.eq.'-t') then     ! -t LISTS   - target data number (from 0)
                japos = japos + 1
                call get_param(ierr, tlists, japos)
             else
                write(*, *) 'invalid arguments: ', trim(astr)
                ierr = -1
             endif
          else
             japos = japos - 1
             exit
          endif
          if (ierr.ne.0) then
             ierr = min(0, ierr)
             exit
          endif
       enddo
    endif
  end subroutine parse_options
!!!_ + End jmzconv
end program jmzconv
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

!!!_! jmzex.F90 - TOUZA/Jmz example GTOOL3 file creation
! Maintainer: SAITO Fuyuki
! Created: Sep 2 2022
#define TIME_STAMP 'Time-stamp: <2022/10/07 10:12:27 fuyuki jmzex.F90>'
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
!!!_@ TOUZA/Jmz/ex - jmz: example creation
program jmzex
!!!_ + Declaration
!!!_  - modules
  use TOUZA_Std,only: arg_diag, env_init
  use TOUZA_Nio, nio_init=>init, nio_diag=>diag, nio_finalize=>finalize
  use jmzlib
!!!_  - variables
  implicit none
  integer ierr
  integer levv, dbgv, stdv
  integer japos

  integer kflag
  integer,parameter :: larg = 1024
  integer,parameter :: nco = 3
  character(len=litem) :: cname(nco)
  integer,parameter :: mp = 3     ! size, begin, width
  integer :: cprop(mp, nco)
  integer :: trange(2)
  integer    offset
!!!_ + Body
  ierr = 0

  levv = 0
  dbgv = -1
  stdv = -2
  call get_options (ierr, japos, levv, dbgv, stdv, kflag)
  if (ierr.eq.0) call get_params(ierr, cname, cprop, trange, offset)

  if (ierr.eq.0) call env_init(ierr, levv=stdv)
  if (ierr.eq.0) call nio_init(ierr, levv=dbgv, stdv=stdv)
  if (ierr.eq.0) call nr_init(ierr, lazy=+1)

  if (ierr.eq.0) then
     call gen_example &
          & (ierr, japos, levv, kflag, cname, cprop, trange, offset)
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
!!!_  - gen_example
  subroutine gen_example &
       & (ierr, japos, levv, kflag, cname, cprop, trange, offset)
    use TOUZA_Std,only: get_param, get_option
    use TOUZA_Std,only: upcase, uout, uerr, is_error_match
    use TOUZA_Nio_std,only: KI32, KFLT, KDBL
    use TOUZA_Nio_record,only:REC_DEFAULT
    use TOUZA_Nio_header
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: japos
    integer,         intent(in)    :: levv
    integer,         intent(in)    :: kflag
    character(len=*),intent(in)    :: cname(*)
    integer,         intent(in)    :: cprop(mp, *)
    integer,         intent(in)    :: trange(*)
    integer,         intent(in)    :: offset

    real(kind=KDBL),allocatable :: vout(:)
    integer,allocatable :: tout(:), nout(:), vref(:)

    integer,parameter :: lpath = 1024
    character(len=lpath) :: file
    integer,parameter :: lfmt = 128
    character(len=lfmt)  :: fmt
    character(len=litem) :: head(nitem)
    character(len=litem*2) :: title
    integer kfmt
    integer udiag
    integer uwrite
    integer msrc(nco), lsrc
    integer mxtr(nco), lxtr
    integer cbgn(nco), cend(nco), cstp(nco)
    integer hrange(2, nco)

    integer m
    integer jvw, jvl, jvbgn, jvend
    integer jt
    integer jctgt, jcsrc
    integer j1, j2, j3
    integer jref,  jout

    integer krect


    ierr = 0

    udiag = uout

    japos = japos + 1
    if (ierr.eq.0) call get_param(ierr, file, japos, ' ')
    japos = japos + 1
    if (ierr.eq.0) call get_param(ierr, fmt, japos, ' ')

    if (ierr.eq.0) then
       if (fmt.eq.' ') fmt='ur4'
    endif

1001 format('debug[', I0, ']:', I0, 1x, A)

    if (is_msglev_debug(levv)) write(udiag, 1001) ierr, __LINE__, 'get_param'

    if (file.eq.' ') then
       ierr = -1
109    format('need arguments: OUTPUT [FORMAT]')
       write(uerr, 109)
       return
    endif
    if (ierr.eq.0) call upcase(fmt)
    if (ierr.eq.0) call parse_rectype(ierr, krect, fmt)
    if (ierr.eq.0) call get_default_header(head)

    if (ierr.eq.0) call put_item(ierr, head, trim(fmt), hi_DFMT)

    title = ' '
    if (ierr.eq.0) call get_option(ierr, title, 'TITLE', ' ')
    if (title.eq.' ') title = 'example'
    if (ierr.eq.0) call put_item(ierr, head, title, hi_TITL1, hi_TITL2)
    if (ierr.eq.0) call put_item(ierr, head, title, hi_ITEM)

    uwrite = new_unit()
    ierr = min(0, uwrite)
    if (ierr.eq.0) call open_write (ierr, uwrite, kfmt, fmt, file, kflag)
    if (is_msglev_debug(levv)) write(udiag, 1001) ierr, __LINE__, 'open_write'

    do jctgt = 1, 3
       if (ierr.eq.0) call put_header_cprop(ierr, head, ' ', (/0, 0/), jctgt)
    enddo
    !  SIZE,-1,-1    == SIZE,0,+SIZE
    !  SIZE,m,-1     == SIZE,m,+1

    do jcsrc = 1, nco
       msrc(jcsrc) = max(1, cprop(1, jcsrc))
       m = msrc(jcsrc)
       if (cprop(3, jcsrc).lt.0) then
          if (cprop(2, jcsrc).lt.0) then
             hrange(1:2, jcsrc) = (/1, m/)
             cbgn(jcsrc) = hrange(1, jcsrc) - 1
             cend(jcsrc) = hrange(2, jcsrc)
             cstp(jcsrc) = 1
          else
             if (cprop(3,jcsrc).eq.-1) then
                hrange(1:2, jcsrc) = cprop(2, jcsrc) + 1
                cbgn(jcsrc) = hrange(1, jcsrc) - 1
                cend(jcsrc) = cbgn(jcsrc) + 1
                cstp(jcsrc) = 1
             else
                hrange(1, jcsrc) = cprop(2, jcsrc) + 1
                hrange(2, jcsrc) = cprop(3, jcsrc)
                cbgn(jcsrc) = hrange(1, jcsrc) - 1
                cend(jcsrc) = hrange(1, jcsrc) - hrange(2, jcsrc) - 1
                cstp(jcsrc) = 0
             endif
          endif
       else if (cprop(2, jcsrc).lt.0) then
          ierr = -1
       else if (cprop(3, jcsrc).eq.0) then
          hrange(1:2, jcsrc) = (/0, 0/)
          cbgn(jcsrc) = 0
          cend(jcsrc) = m
          cstp(jcsrc) = 0
       else
          hrange(1, jcsrc) = cprop(2, jcsrc) + 1
          hrange(2, jcsrc) = cprop(2, jcsrc) + cprop(3, jcsrc)
          cbgn(jcsrc) = hrange(1, jcsrc) - 1
          cend(jcsrc) = hrange(2, jcsrc)
          cstp(jcsrc) = 1
       endif
       mxtr(jcsrc) = max(1, hrange(2, jcsrc) - hrange(1, jcsrc) + 1)
    enddo

    if (is_msglev_DETAIL(levv)) then
       do jcsrc = 1, 3
101       format('cprop[', I0, ':', A, '] ', I0, 1x, I0, ' / ', I0, SP, I0, 2x, SS, I0, ':', I0, ':', I0)
          write(*, 101) jcsrc, trim(cname(jcsrc)), &
               & msrc(jcsrc), mxtr(jcsrc), hrange(:, jcsrc), &
               & cbgn(jcsrc), cend(jcsrc), cstp(jcsrc)
       enddo
    endif
    if (is_msglev_debug(levv)) write(udiag, 1001) ierr, __LINE__, 'cprop'

    jctgt = 1
    do jcsrc = 1, 3
       if (ierr.eq.0) then
          if (cname(jcsrc).eq.' '.and.hrange(2, jcsrc).eq.0) then
             continue
          else
             if (cname(jcsrc).eq.'-') then
                call put_header_cprop(ierr, head, ' ', hrange(:, jcsrc), jctgt)
             else
                call put_header_cprop(ierr, head, cname(jcsrc), hrange(:, jcsrc), jctgt)
             endif
             if (ierr.eq.0) jctgt = jctgt + 1
          endif
       endif
    enddo
    if (is_msglev_debug(levv)) write(udiag, 1001) ierr, __LINE__, 'put_header_cprop'

    lsrc = max(1, msrc(1) * msrc(2) * msrc(3))
    lxtr = max(1, mxtr(1) * mxtr(2) * mxtr(3))
    if (ierr.eq.0) allocate(vref(0:lsrc-1), STAT=ierr)
    if (ierr.eq.0) allocate(vout(0:lxtr-1), tout(0:lxtr-1), nout(0:lxtr-1), STAT=ierr)

    do jt = trange(1), trange(2) - 1
       jvbgn = jt * lsrc
       jvend = (jt + 1) * lsrc
       do jvw = jvbgn, jvend - 1
          jvl = jvw - jvbgn
          vref(jvl) = jvw + offset
       enddo
       tout(:) = 0
       nout(:) = 0
       do j3 = cbgn(3), cend(3) - 1
          do j2 = cbgn(2), cend(2) - 1
             do j1 = cbgn(1), cend(1) - 1
                jref = j1 + msrc(1) * (j2 + msrc(2) * j3)
                jout =    (j1 - cbgn(1)) * cstp(1) &
                     & +  (j2 - cbgn(2)) * cstp(2)  * mxtr(1) &
                     & +  (j3 - cbgn(3)) * cstp(3)  * mxtr(1) * mxtr(2)
                ! write(*, *) j1, j2, j3, jref, jout
                tout(jout) = tout(jout) + vref(jref)
                nout(jout) = nout(jout) + 1
             enddo
          enddo
       enddo
       vout(:) = real(tout(:),kind=kind(vout(1))) / nout(:)
       ! write(*, *) lxtr
       ! write(*, *) vout
       ! write(*, *) tout
       ! write(*, *) nout
       if (ierr.eq.0) call put_item(ierr, head, jt, hi_TIME)

       if (ierr.eq.0) call nio_write_header(ierr, head, krect, uwrite)
       if (is_msglev_debug(levv)) write(udiag, 1001) ierr, __LINE__, 'nio_write_header'
       if (ierr.eq.0) call nio_write_data(ierr, vout, lxtr, head, krect, uwrite)
       if (is_msglev_debug(levv)) write(udiag, 1001) ierr, __LINE__, 'nio_write_data'
    enddo
  end subroutine gen_example

!!!_  - get_options
  subroutine get_options (ierr, japos, levv, dbgv, stdv, kflag)
    use TOUZA_Std,only: arg_init, parse, get_param
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: japos
    integer,         intent(inout) :: levv
    integer,         intent(inout) :: dbgv, stdv
    integer,         intent(out)   :: kflag

    integer,parameter   :: lstr = 1024
    character(len=lstr) :: astr

    ierr = 0
    japos = 0
    kflag = 0
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
             ! else if (astr.eq.'-t') then     ! -t LISTS   - target data number (from 0)
             !    japos = japos + 1
             !    call get_param(ierr, tlists, japos)
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
    ! if (ierr.eq.0) then
    !    japos = japos + 1
    !    call get_param(ierr, file, japos)
    !    if (ierr.ne.0) then
    !       write(*, *) 'no output file'
    !       ierr = -1
    !    endif
    ! endif
  end subroutine get_options

!!!_  - get_params
  subroutine get_params (ierr, cname, cprop, trange, offset)
    use TOUZA_Std,only: parse, get_option
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: cname(*)
    integer,         intent(out) :: cprop(mp, *)
    integer,         intent(out) :: trange(*)
    integer,         intent(out) :: offset

    ierr = 0
    if (ierr.eq.0) call get_arg_params(ierr, cname(1), cprop(1, 1), 'X')
    if (ierr.eq.0) call get_arg_params(ierr, cname(2), cprop(1, 2), 'Y')
    if (ierr.eq.0) call get_arg_params(ierr, cname(3), cprop(1, 3), 'Z')

    if (ierr.eq.0) then
       call get_option(ierr, trange(1:2), 'T', -1)
       if (ierr.eq.0) then
          if (trange(2).lt.0) then
             trange(2) = max(1, trange(1))
             trange(1) = 0
          else
             trange(2) = max(trange(1) + 1, trange(2))
          endif
       endif
    endif
    if (ierr.eq.0) call get_option(ierr, offset, 'O', 0)
  end subroutine get_params

  subroutine get_arg_params (ierr, cname, cprop, tag)
    use TOUZA_Std,only: parse, get_option, parse_param
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: cname
    integer,         intent(out) :: cprop(*)
    character(len=*),intent(in)  :: tag

    integer,parameter   :: lstr = 1024
    character(len=lstr) :: astr
    character(len=*),parameter :: sep=','
    integer jp

    ierr = 0
    if (ierr.eq.0) call get_option(ierr, astr, tag, ' ')
    ! TAG=NAME,SIZE[,BEGIN[,WIDTH]]
    if (ierr.eq.0) then
       jp = index(astr, sep)
       if (jp.eq.0) then
          cname = trim(astr)
          cprop(1:3) = (/0, 0, 0/)
       else
          cname = astr(1:jp-1)
          astr = astr(jp+1:)
          cprop(1:3) = (/0, -1, -1/)
          call parse_param(ierr, cprop(1:3), astr, unset=.TRUE.)
       endif
    endif
    ! if (ierr.eq.0) then
    !    cprop(1) = max(0, cprop(1))
    !    if (cprop(2).lt.0) then
    !       cprop(2:3) = (/0, 0/)
    !    else
    !       if (cprop(3).eq.-1) cprop(3) = +1
    !    endif
    ! endif

  end subroutine get_arg_params
!!!_ + End jmzex
end program jmzex
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

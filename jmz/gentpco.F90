!!!_! gentpco.F90 - Generate tripolar coordinate axis files
! Maintainer: SAITO Fuyuki
! Created: Feb 14 2025
#define TIME_STAMP 'Time-stamp: <2025/02/19 23:29:09 fuyuki gentpco.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2025
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
#define _PFX 'gentpco: '
!!!_@ TOUZA/Jmz/gentpco - Generate tripolar coordinate axis files
program gentpco
!!!_ + Declaration
!!!_  - modules
  use Jmz_param
  use Jmz_base, base_init=>init, base_finalize=>finalize
  use Jmz_coor,only: lname
  use Jmz_psprop
!!!_  - default
  implicit none
!!!_  - variables
  integer ierr
!!!_  - configurations
  integer jpos, npos
!!!_  - computation level
  integer,parameter :: llev = 2
  integer,parameter :: lev_ini = 1
  integer,parameter :: lev_max = 2
!!!_ + Driver
  ierr = 0

  if (ierr.eq.0) call base_init(ierr, basename='gentpco')
  if (ierr.eq.0) call parse_global(ierr, jpos, npos)

  if (ierr.eq.0) then
     if (npos.eq.0) then
109     format(_PFX, 'no command. exit')
        write(*, 109)
        call show_usage(ierr)
     else
        call tripolar_coordinate(ierr, jpos, npos)
     endif
  endif
  if (ierr.eq.0) call base_finalize(ierr)
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
!!!_  & show_usage
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

  end subroutine show_usage
!!!_  & tripolar_coordinate
  subroutine tripolar_coordinate &
       & (ierr, jpos, npos, levv)
    use TOUZA_Nio,only: nio_record_std
    use TOUZA_Nio,only: get_default_header
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(inout)       :: jpos
    integer,intent(in)          :: npos
    integer,intent(in),optional :: levv

    character(len=lpath) :: lofile, lafile, odir
    ! character(len=litem) :: lohead(nitem), lahead(nitem)

    character(len=*),parameter :: sep_dir = '/'
    real(kind=KTGT),parameter :: def = -HUGE(ZERO)
    integer nwlat
    real(kind=KTGT) :: plon, plat, olat, ofswla
    real(kind=KTGT) :: targ(NGEOG)
    real(kind=KTGT) :: tol
    character(len=litem) :: DFMT

    ierr = 0
    if (ierr.eq.0) call get_param(ierr, lofile, jpos)
    jpos = jpos + 1
    if (ierr.eq.0) call get_param(ierr, lafile, jpos)
    jpos = jpos + 1
    if (ierr.ne.0) then
       call message(ierr, "Need two coordinate files.")
       return
    endif
    if (ierr.eq.0) call get_param(ierr, plon, jpos)
    jpos = jpos + 1
    if (ierr.eq.0) call get_param(ierr, plat, jpos)
    jpos = jpos + 1
    if (ierr.ne.0) then
       call message(ierr, "Need plon plat arguments.")
       return
    endif
    if (ierr.eq.0) call get_param(ierr, odir, jpos, ' ')

    if (ierr.eq.0) call get_option(ierr, olat, 'op', def)
    if (ierr.eq.0) then
       if (olat.eq.def) olat = -90.0_KTGT
    endif
    if (ierr.eq.0) call get_option(ierr, targ(1:NGEOG), 'wp', def)
    if (ierr.eq.0) plat = - SIGN(plat, olat)
    if (ierr.eq.0) call get_option(ierr, ofswla, 'wo', def)
    if (ierr.eq.0) then
       if (ofswla.eq.def) ofswla = 0.0_KTGT
    endif
    if (ierr.eq.0) call get_option(ierr, nwlat, 'nw', -1)
    if (ierr.eq.0) call get_option(ierr, tol, 'tol', 0.0_KTGT)
    if (ierr.eq.0) call get_option(ierr, dfmt, 'dfmt', ' ')

    if (ierr.eq.0) then
       call batch_generate &
            & (ierr, &
            &  odir,  &
            &  lofile, &
            &  lafile, &
            &  olat, plon, plat, ofswla, nwlat, &
            &  tol,  sep_dir, dfmt)
    endif
  end subroutine tripolar_coordinate
!!!_  - batch_generate
  subroutine batch_generate &
       & (ierr, &
       &  odir,  &
       &  lofile, &
       &  lafile, &
       &  olat, plon, plat, ofswla, nwlat, &
       &  tol,  sep_dir, dfmt)
    use TOUZA_Nio,only: cache_open_read
    use TOUZA_Nio,only: show_cache, cache_var_size, cache_var_len
    use TOUZA_Nio,only: cache_get_attr, hi_DSET, hi_DFMT
    use TOUZA_Nio,only: cache_var_read, cache_read_header
    use TOUZA_Nio,only: grp_suite
    use TOUZA_Nio,only: put_header_cprop, nio_write_header, nio_write_data
    use TOUZA_Nio,only: REC_BIG
    use TOUZA_Emu,only: check_monotonic, round_degree
    use TOUZA_Emu,only: degree_modulo, span_latitude, span_longitude
    use TOUZA_Emu,only: set_sincos, ncache_stp_co, stp_set, stp_bwd_tr
    implicit none
    integer,intent(out) :: ierr
    character(len=*),intent(in) :: odir
    character(len=*),intent(in) :: lafile
    character(len=*),intent(in) :: lofile
    real(kind=KTGT),intent(in) :: olat, plon, plat
    real(kind=KTGT),intent(in) :: ofswla
    real(kind=KTGT),intent(in) :: tol
    character(len=*),intent(in) :: sep_dir
    character(len=*),intent(in) :: dfmt
    integer,intent(in) :: nwlat
    integer :: hlon, hlat
    integer :: mlon, mlat, mlonc
    integer :: jlo,  jla,  jg
    integer :: jvarlo, jvarla
    integer :: jreclo, jrecla
    integer :: latb, latm, late
    integer dir_lat, dir_lon
    character(len=lpath) :: ofile

    real(kind=KTGT), allocatable :: llongi(:), llati(:)  ! logical coordinates
    real(kind=KTGT), allocatable :: phlongi(:), phlati(:)  ! physical coordinates
    character(len=litem) :: attr, colo, cola

    real(kind=KTGT) :: wlon, wlat, wden
    real(kind=KTGT),parameter :: CSPAN = real(round_degree, kinD=KTGT)
    real(kind=KTGT) :: loround, laround

    real(kind=KTGT) :: phofs
    integer pole
    real(kind=KTGT) :: lattr(NTRIG), lontr(NTRIG)
    real(kind=KTGT) :: csco(ncache_stp_co)
    real(kind=KTGT) :: wg(NGEOG), zg(NGEOG)
    real(kind=KTGT) :: elat

    integer :: ufile
    integer :: ldest
    character(len=litem) :: head(nitem)
    integer :: krect

    ierr = 0
    if (ierr.eq.0) call cache_open_read(ierr, hlon, lofile, flag=0)
    if (ierr.eq.0) call cache_open_read(ierr, hlat, lafile, flag=0)
    if (ierr.eq.0) then
       call show_cache(ierr, hlon, levv=+2)
       call show_cache(ierr, hlat, levv=+2)
    endif
    if (ierr.eq.0) then
       jvarlo = 0
       jreclo = 0
       mlon = cache_var_len(hlon, jvarlo)
       ierr = min(0, mlon)
    endif
    if (ierr.eq.0) call cache_get_attr(ierr, attr, hi_DSET, hlon, jvarlo, jreclo)
    if (ierr.eq.0) then
       mlonc = mlon
       if (attr(1:1).eq.'C') mlonc = mlonc - 1
       write(*, *) mlon, mlonc
    endif
    if (ierr.eq.0) allocate(llongi(0:mlon), STAT=ierr)
    if (ierr.eq.0) then
       call cache_var_read(ierr, llongi, hlon, jvarlo, jreclo)
    endif
    if (ierr.eq.0) then
       jrecla = 0
       jvarla = 0
       mlat = cache_var_len(hlat, jvarla)
       ierr = min(0, mlat)
       write(*, *) mlat
    endif
    if (ierr.eq.0) allocate(llati(0:mlat-1), STAT=ierr)
    if (ierr.eq.0) then
       call cache_var_read(ierr, llati, hlat, jvarla, jrecla)
    endif
    if (ierr.eq.0) then
       do jlo = 0, mlonc - 1
          write(*, *) 'LON:', jlo, llongi(jlo)
       enddo
       do jla = 0, mlat - 1
          write(*, *) 'LAT:', jla, llati(jla)
       enddo
    endif
    if (ierr.eq.0) then
       dir_lon = check_monotonic(llongi, mlonc)
       if (dir_lon.ne.1.and.dir_lon.ne.-1) ierr = ERR_PANIC
       write(*, *) 'lon range:', llongi(0), llongi(mlonc), dir_lon
    endif
    if (ierr.eq.0) then
       dir_lat = check_monotonic(llati, mlat)
       if (dir_lat.ne.1.and.dir_lat.ne.-1) ierr = ERR_PANIC
    endif
    if (ierr.eq.0) then
       if (dir_lat.gt.0) then
          if (olat.lt.0.0_KTGT) then
             latb = 0
             late = mlat
             latm = late
             do jla = 0, mlat - 1
                if (llati(jla).ge.plat) then
                   latm = jla
                   exit
                endif
             enddo
          else
             ierr = ERR_PANIC
          endif
       else
          ierr = ERR_PANIC
       endif
       if (ierr.ne.0) then
          write(*, *) 'Not implemented yet.', dir_lat, olat
       endif
       write(*, *) dir_lat, olat, latb, latm, late, late - latm
       write(*, *) llati(latm-1:latm)
    endif
    if (ierr.eq.0) then
       ldest = mlonc * late
       allocate(phlongi(0:ldest-1), phlati(0:ldest-1), STAT=ierr)
    endif
    if (ierr.eq.0) then
       if (olat.le.0) then
          pole = +1
       else
          pole = -1
       endif
       call stp_set(ierr, csco, plat, plon, pole, loround=CSPAN, laround=CSPAN)
       write(*, *) 'CSCO = ', csco
    endif
    if (ierr.eq.0) then
       loround = span_longitude(CSPAN)
       laround = span_latitude(CSPAN)
       do jla = 0, latm - 1
          jg = jla * mlonc
          phlati(jg:jg+mlonc-1) = llati(jla)
          phlongi(jg:jg+mlonc-1) = llongi(0:mlonc-1)
       enddo
       if (nwlat.lt.0) then
          wden = real(late - latm, kind=KTGT)
       else
          wden = real(nwlat, kind=KTGT)
       endif
       do jla = latm, late - 1
          do jlo = 0, mlonc - 1
             wlat = modulo(llongi(jlo) - plon, loround)
             wlon = (real((jla - latm), KTGT) + ofswla) &
                  & * ((loround / 4.0_KTGT) / wden) - loround / 4.0_KTGT
             if (wlat.gt.laround) then
                wlat = loround - wlat
                wlon = -wlon
             endif
             wlat = wlat - (laround / 2.0_KTGT)
             lontr(:) = set_sincos(wlon, CSPAN)
             lattr(:) = set_sincos(wlat, CSPAN)
             zg(:) = stp_bwd_tr(lontr, lattr, csco)
             jg = jla * mlonc + jlo
             phlati(jg) = zg(JLATI)
             phlongi(jg) = modulo(zg(JLONGI), CSPAN) + (plon - CSPAN)
             ! phlongi(jg) = modulo(zg(JLONGI) + plon, CSPAN)
             if (jlo.gt.0.and.phlongi(jg).lt.phlongi(jg-1)) then
                phlongi(jg) = modulo(zg(JLONGI), CSPAN) + plon
             endif
             write(*, *) 'w:', jlo, jla, &
                  & wlon, wlat, phlongi(jg), phlati(jg), zg(JLONGI)
          enddo
       enddo
    endif
    if (ierr.eq.0) then
       do jla = 0, late - 1
          do jlo = 0, mlonc - 1
             jg = jla * mlonc + jlo
             ! phlongi(jg) = modulo(phlongi(jg), CSPAN)
             ! if (phlongi(jg).lt.(- CSPAN / 2.0_KTGT)) &
             !      & phlongi(jg) = phlongi(jg) + CSPAN
             ! phlongi(jg) = phlongi(jg) + CSPAN
             elat = ABS(phlati(jg) - plat)
             if (elat.le.tol) then
                if (phlongi(jg).ne.llongi(jlo).or.elat.gt.0.0_KTGT) then
                   write(*, *) 'adjust:', jlo, jla, phlongi(jg), phlati(jg), &
                        & ABS(phlati(jg) - plat)
                   phlongi(jg) = llongi(jlo)
                   phlati(jg) = plat
                endif
             endif
             write(*, *) 'PHY:', jlo, jla, phlongi(jg), phlati(jg)
          enddo
       enddo
    endif
    if (odir.ne.' ') then
       if (ierr.eq.0) then
          ufile = new_unit()
          ierr = min(0, ufile)
       endif

       krect = REC_BIG

       if (ierr.eq.0) call gen_filename(ierr, ofile, odir, lofile, sep_dir)
       if (ierr.eq.0) call sus_open(ierr, ufile, ofile, ACTION='W', STATUS='R')

       if (ierr.eq.0) call get_coname(ierr, colo, hlon, jvarlo)
       if (ierr.eq.0) call get_coname(ierr, cola, hlat, jvarla)
       if (ierr.eq.0) call cache_read_header(ierr, head, hlon, jvarlo, jreclo)
       if (ierr.eq.0) call put_header_cprop(ierr, head, colo, (/1, mlonc/), 1)
       if (ierr.eq.0) call put_header_cprop(ierr, head, cola, (/1, late/),  2)
       if (ierr.eq.0) call put_header_cprop(ierr, head, ' ', (/1, 1/),  3)
       if (ierr.eq.0) then
          attr = head(hi_DSET)
          if (attr(1:1).eq.'C') attr = attr(2:)
          head(hi_DSET) = attr
       endif
       if (ierr.eq.0) then
          if (dfmt.ne.' ') head(hi_DFMT) = dfmt
       endif

       if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
       if (ierr.eq.0) call nio_write_data(ierr, phlongi, ldest, head, krect, ufile)
       if (ierr.eq.0) call sus_close(ierr, ufile, ofile)

       if (ierr.eq.0) call gen_filename(ierr, ofile, odir, lafile, sep_dir)
       if (ierr.eq.0) call sus_open(ierr, ufile, ofile, ACTION='W', STATUS='R')

       if (ierr.eq.0) call cache_read_header(ierr, head, hlat, jvarla, jrecla)
       if (ierr.eq.0) call put_header_cprop(ierr, head, colo, (/1, mlonc/), 1)
       if (ierr.eq.0) call put_header_cprop(ierr, head, cola, (/1, late/),  2)
       if (ierr.eq.0) call put_header_cprop(ierr, head, ' ', (/1, 1/),  3)
       if (ierr.eq.0) then
          if (dfmt.ne.' ') head(hi_DFMT) = dfmt
       endif

       if (ierr.eq.0) call nio_write_header(ierr, head, krect, ufile)
       if (ierr.eq.0) call nio_write_data(ierr, phlati, ldest, head, krect, ufile)
       if (ierr.eq.0) call sus_close(ierr, ufile, ofile)
    endif
  end subroutine batch_generate

  subroutine gen_filename &
       & (ierr, file, &
       &  dir,  ref,  sep_dir)
    implicit none
    integer,intent(out) :: ierr
    character(len=*),intent(out) :: file
    character(len=*),intent(in) :: dir, ref
    character(len=*),intent(in) :: sep_dir
    integer :: jbase

    file = trim(dir)
    jbase = index(ref, trim(sep_dir), .TRUE.)
    if (jbase.eq.0) then
       if (file.eq.' ') then
          file = ref
       else
          file = trim(file) // trim(sep_dir) // trim(ref)
       endif
    else
       if (file.eq.' ') then
          file = ref(jbase+1:)
       else
          file = trim(file) // trim(sep_dir) // trim(ref(jbase+1:))
       endif
    endif
    if (ierr.eq.0) then
       if (is_file_exist(file)) then
101       format('Error: exists ', A)
          write(*, 101) trim(file)
          ierr = ERR_INVALID_PARAMETER
       endif
    endif
  end subroutine gen_filename

  subroutine get_coname &
       & (ierr, name, hfile, var)
    use TOUZA_Nio,only: cache_co_size, cache_co_name
    implicit none
    integer,intent(out) :: ierr
    character(len=*),intent(out) :: name
    integer,intent(in) :: hfile, var
    integer jx, nx, nf
    character(len=litem) :: buf

    ierr = 0

    nf = 0

    nx = cache_co_size(hfile, var)
    ierr = min(0, nx)
    if (ierr.eq.0) then
       do jx = 0, nx - 1
          call cache_co_name(ierr, buf, hfile, var, jx)
          if (ierr.eq.0.and.buf.ne.' ') then
             nf = nf + 1
             name = buf
          endif
       enddo
    endif
    if (ierr.eq.0) then
       if (nf.ne.1) then
          write(*, *) 'Invalid coordinates.'
          ierr = ERR_INVALID_ITEM
       endif
    endif
  end subroutine get_coname

!!!_ + end program
end program gentpco
!!!_* obsolete
#if 0 /* obsolete */
#endif /* obsolete */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

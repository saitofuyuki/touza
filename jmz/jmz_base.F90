!!!_! jmz_base.F90 - TOUZA/Jmz base utilities
! Maintainer: SAITO Fuyuki
! Created: Feb 14 2024
#define TIME_STAMP 'Time-stamp: <2024/06/21 21:54:22 fuyuki jmz_base.F90>'
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
!!!_@ TOUZA/Jmz/base - jmz base
module Jmz_base
!!!_ + Declaration
  use Jmz_param
!!!_  - to export
  use TOUZA_Std,only: KFLT, KDBL, KQPL, KIOFS
  use TOUZA_Std,only: choice, choice_a, upcase, split_list, join_list, parse_number
  use TOUZA_Std,only: find_first, set_if_present
  use TOUZA_Std,only: trace_err
  use TOUZA_Std,only: msglev_anyway
  use TOUZA_Std,only: msglev_panic,  msglev_fatal,   msglev_critical
  use TOUZA_Std,only: msglev_severe, msglev_warning, msglev_normal
  use TOUZA_Std,only: msglev_info,   msglev_detail,  msglev_debug
  use TOUZA_Std,only: is_msglev,       is_msglev_ANYWAY
  use TOUZA_Std,only: is_msglev_DEBUG, is_msglev_DETAIL, is_msglev_INFO
  use TOUZA_Std,only: is_msglev_NORMAL
  use TOUZA_Std,only: get_param, get_option
  use TOUZA_Std,only: new_unit,  is_file_exist
  use TOUZA_Std,only: sus_open,  sus_close
  use TOUZA_Emu,only: deg2rad, rad2deg
  use TOUZA_Emu,only: JLATI, JLONGI, NGEOG
  use TOUZA_Emu,only: JSIN,  JCOS,   NTRIG
!!!_  - default
  implicit none
  public
!!!_  - constant
  character(len=*),parameter,private :: sep_base = '/'

!!!_  - global options
  integer,save,public :: lev_verbose = 0
  integer,save,public :: lev_debug = -1

  integer,save,public :: fwrite_mode = fmode_default
  integer,save,public :: fread_mode = fmode_default
!!!_  - i/o units
  integer,save,public :: ulog = -1
  integer,save,public :: uerr = -1

!!!_  - static
  integer,parameter,private :: lbase = 128
  character(len=lbase),save,private :: base = 'touza:jmz'

  integer,parameter,private :: larg = 8
  character(len=larg),save,private :: apfxc = '-+'
  character(len=larg),save,private :: apass = '--'
!!!_  - interfaces
  interface get_last_option
     module procedure :: get_last_option_a
     module procedure :: get_last_option_i, get_last_option_d
  end interface get_last_option
!!!_  - private
  private :: count_option_levels
!!!_ + Standard procedures
contains
!!!_  & init
  subroutine init(ierr, basename)
    use TOUZA_Std,only: MPI_COMM_NULL, stdout=>uout, stderr=>uerr
    use TOUZA_Std,only: env_init
    use TOUZA_Emu,only: emu_init=>init
    use TOUZA_Nio,only: nio_init=>init
    implicit none
    integer,         intent(out)          :: ierr
    character(len=*),intent(in),optional  :: basename
    ierr = 0
    if (ierr.eq.0) call env_init(ierr, levv=lev_debug, icomm=MPI_COMM_NULL)
    if (ierr.eq.0) then
       ulog = stdout
       uerr = stderr
    endif
    if (ierr.eq.0) call emu_init(ierr, levv=lev_debug, mode=MODE_SHALLOW)
    if (ierr.eq.0) call nio_init(ierr, levv=lev_debug)
    if (ierr.eq.0) then
       if (present(basename)) then
          base = basename
       endif
    endif
  end subroutine init
!!!_  & finalize
  subroutine finalize(ierr, u)
    use TOUZA_Std,only: env_diag, env_finalize
    use TOUZA_Emu,only: emu_diag=>diag, emu_finalize=>finalize
    use TOUZA_Nio,only: nio_diag=>diag, nio_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer utmp
    ierr = 0
    utmp = choice(ulog, u)
    if (ierr.eq.0) call nio_diag(ierr, utmp, levv=lev_debug, mode=MODE_SHALLOW)
    if (ierr.eq.0) call emu_diag(ierr, utmp, levv=lev_debug, mode=MODE_SHALLOW)
    if (ierr.eq.0) call env_diag(ierr, utmp, levv=lev_debug)
    if (ierr.eq.0) call nio_finalize(ierr, utmp, levv=lev_debug, mode=MODE_SHALLOW)
    if (ierr.eq.0) call emu_finalize(ierr, utmp, levv=lev_debug, mode=MODE_SHALLOW)
    if (ierr.eq.0) call env_finalize(ierr, utmp, levv=lev_debug)
  end subroutine finalize

!!!_  & push_basename
  subroutine push_basename(ierr, name)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: name
    ierr = 0
    if (base.eq.' ') then
       base = trim(name)
    else
       base = trim(base) // trim(sep_base) // trim(name)
    endif
  end subroutine push_basename
!!!_  & pop_basename
  subroutine pop_basename(ierr, name)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in),optional :: name
    integer jp, js
    ierr = 0
    jp = index(base, trim(sep_base), back=.TRUE.)
    if (present(name)) then
       js = jp + len_trim(sep_base)
       if (trim(base(js:)).ne.name) then
          ierr = ERR_INVALID_PARAMETER
       endif
    endif
    if (ierr.eq.0) base = base(:jp-1)
  end subroutine pop_basename
!!!_  & is_verbose()
  logical function is_verbose(crit, levv) result(b)
    use TOUZA_Std,only: is_msglev
    implicit none
    integer,intent(in) :: crit
    integer,intent(in),optional :: levv
    integer lv
    lv = choice(lev_verbose, levv)
    b = is_msglev(lv, crit)
  end function is_verbose
!!!_  & message
  subroutine message(ierr, msg, iadd, fmt, levm, u, indent, trace)
    use TOUZA_Std,only: join_list
    use TOUZA_Std,only: is_msglev
    implicit none
    integer,         intent(in)          :: ierr
    character(len=*),intent(in)          :: msg     ! msg
    integer,         intent(in),optional :: iadd(:)
    character(len=*),intent(in),optional :: fmt
    integer,         intent(in),optional :: levm    ! message level
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: indent
    logical,         intent(in),optional :: trace
    integer jerr
    integer lv, utmp
    character(len=1024) :: txt
    integer skp
    logical bt

    jerr = 0
    lv = choice(0, levm)
    bt = choice(.TRUE., trace)
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
102    format(A, ':error:', I0, ': ', A)
101    format(A, ':', A, A)
112    format('error:', I0, ': ', A)
111    format(A, A)
       if (base.eq.' ') then
          if (ierr.ne.0) then
             write(utmp, 112) ierr, trim(txt)
             if (bt) call trace_err(ierr)
          else
             write(utmp, 111) repeat(' ', skp), trim(txt)
          endif
       else
          if (ierr.ne.0) then
             write(utmp, 102) trim(base), ierr, trim(txt)
             if (bt) call trace_err(ierr)
          else
             write(utmp, 101) trim(base), repeat(' ', skp), trim(txt)
          endif
       endif
    endif
  end subroutine message

!!!_ + arguments parser
!!!_  & parse_global
  subroutine parse_global (ierr, jpos, npos)
    use TOUZA_Std,only: upcase
    use TOUZA_Std,only: arg_init, arg_diag, ts_parse=>parse
    use TOUZA_Std,only: get_nparam, get_param
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: jpos
    integer,intent(out) :: npos

    character(len=lpath) :: arg
    integer n
    integer jerr

    ierr = 0
    jpos = 0
    if (ierr.eq.0) call arg_init(ierr, levv=lev_debug)
    if (ierr.eq.0) call ts_parse(ierr)

    if (ierr.eq.0) then
       npos = get_nparam()
       ierr = min(0, npos)
    endif
    if (ierr.eq.0) then
       do
          jpos = jpos + 1
          if (jpos.gt.npos) exit
          call get_param(ierr, arg, jpos)
          if (ierr.eq.0) then
             if (arg.eq.apass) exit
             if (index(apfxc, arg(1:1)).eq.0) exit
             ! verbosity
             if (arg(1:2).eq.'-v') then
                n = count_option_levels(arg(3:), arg(2:2))
                ierr = min(n, 0)
                if (ierr.eq.0) lev_verbose = + n
             else if (arg.eq.'+v') then
                lev_verbose = +999
             else if (arg(1:2).eq.'-q') then
                n = count_option_levels(arg(3:), arg(2:2))
                ierr = min(n, 0)
                if (ierr.eq.0) lev_verbose = - n
             else if (arg.eq.'+q') then
                lev_verbose = -999
             else if (arg(1:2).eq.'-d') then
                n = count_option_levels(arg(3:), arg(2:2))
                ierr = min(n, 0)
                if (ierr.eq.0) lev_debug = + n
             else if (arg.eq.'+d') then
                lev_debug = +999
             ! file write mode
             else if (arg.eq.'-a') then
                fwrite_mode = fmode_append
             else if (arg.eq.'-k') then
                fwrite_mode = fmode_new
             else if (arg.eq.'-f') then
                fwrite_mode = fmode_write
             else
                ierr = ERR_INVALID_ITEM
             endif
          endif
          if (ierr.ne.0) exit
       enddo
    endif
    if (ierr.ne.0) then
       call arg_diag(jerr, levv=max(1, lev_debug))
    else
       call arg_diag(ierr, levv=lev_debug)
    endif
  end subroutine parse_global
!!!_   & count_option_levels()
  integer function count_option_levels(str, ch) result(n)
    use TOUZA_Std,only: parse_number
    implicit none
    character(len=*),intent(in) :: str
    character(len=1),intent(in) :: ch
    integer jerr
    call parse_number(jerr, n, str)
    if (jerr.ne.0) then
       n = verify(trim(str), ch, .TRUE.)
       if (n.ne.0) then
          n = ERR_INVALID_ITEM
       else
          n = len_trim(str) + 1
       endif
    endif
  end function count_option_levels
!!!_  & get_last_option
  subroutine get_last_option_a &
       & (ierr, val, tag, a1, a2, a3, a4, def)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(out)         :: val
    character(len=*),intent(in)          :: tag
    character(len=*),intent(in),optional :: def
    character(len=*),intent(in),optional :: a1, a2, a3, a4
    call get_last_option_core(ierr, val, tag, a1, a2, a3, a4, def)
  end subroutine get_last_option_a
  subroutine get_last_option_i &
       & (ierr, val, tag, a1, a2, a3, a4, def)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: val
    character(len=*),intent(in)          :: tag
    integer,         intent(in),optional :: def
    character(len=*),intent(in),optional :: a1, a2, a3, a4
    character(len=128) :: buf
    call get_last_option_core(ierr, buf, tag, a1, a2, a3, a4, ' ')
    if (ierr.eq.0) then
       if (buf.eq.' ') then
          if (present(def)) then
             val = def
          else
             ierr = ERR_INVALID_PARAMETER
          endif
       else
          call parse_number(ierr, val, buf)
       endif
    endif
  end subroutine get_last_option_i
  subroutine get_last_option_d &
       & (ierr, val, tag, a1, a2, a3, a4, def)
    implicit none
    integer,parameter :: KTGT=KDBL
    integer,         intent(out)         :: ierr
    real(kind=KTGT), intent(out)         :: val
    character(len=*),intent(in)          :: tag
    real(kind=KTGT), intent(in),optional :: def
    character(len=*),intent(in),optional :: a1, a2, a3, a4
    character(len=128) :: buf
    call get_last_option_core(ierr, buf, tag, a1, a2, a3, a4, ' ')
    if (ierr.eq.0) then
       if (buf.eq.' ') then
          if (present(def)) then
             val = def
          else
             ierr = ERR_INVALID_PARAMETER
          endif
       else
          call parse_number(ierr, val, buf)
       endif
    endif
  end subroutine get_last_option_d
!!!_  & get_last_option_core
  subroutine get_last_option_core &
       & (ierr, val, tag, a1, a2, a3, a4, def)
    use TOUZA_Std,only: get_entry
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(out)         :: val
    character(len=*),intent(in)          :: tag
    character(len=*),intent(in),optional :: def
    character(len=*),intent(in),optional :: a1, a2, a3, a4

    character(len=128) :: txt
    character(len=128) :: atags
    integer,parameter :: nalias = 4
    integer nt, et, idx

    ierr = 0

    et = -1
    nt = 0
    idx = 0
    atags= ' '
    call parse_entries(et, idx, nt, atags, tag)
    call parse_entries(et, idx, nt, atags, a1)
    call parse_entries(et, idx, nt, atags, a2)
    call parse_entries(et, idx, nt, atags, a3)
    call parse_entries(et, idx, nt, atags, a4)

    if (nt.lt.0) then
       ierr = nt
    else if (nt.eq.0) then
       if (present(def)) then
          val = def
       else
          ierr = ERR_FEW_ARGUMENTS
       endif
    else
       call get_entry(ierr, val, et, def)
       if (ierr.eq.0) then
          if (nt.gt.1) then
101          format('Use last for multiple options: ', A)
             write(txt, 101) trim(atags(2:))
             call message(ierr, txt, levm=msglev_NORMAL)
          endif
       else
102       format('Fail to parse options: ', A)
          write(txt, 102) trim(atags(2:))
          call message(ierr, txt, levm=msglev_CRITICAL)
       endif
    endif
  contains
    subroutine parse_entries(entr, lidx, ndup, a, t)
      use TOUZA_Std,only: count_entries,query_nth_entry
      implicit none
      integer,         intent(inout)       :: entr, ndup, lidx
      character(len=*),intent(inout)       :: a
      character(len=*),intent(in),optional :: t
      integer n, e, j

      if (ndup.lt.0) return   ! error is recorded

      if (present(t)) then
         if (t.eq.' ') return

         n = count_entries(t)
         ndup = ndup + max(0, n)
         if (n.gt.0) then
            e = query_nth_entry(t, n - 1, j)
            if (e.ge.0) then
               if (j.gt.lidx) then
                  entr = e
                  lidx = j
               endif
               a = trim(a) // ' ' // trim(t)
            else
               ndup = e
            endif
         endif
      endif
    end subroutine parse_entries
  end subroutine get_last_option_core
!!!_ + file access
!!!_  & open_write
  subroutine open_write &
       & (ierr, u, file, fmt, mode, levv, uref)
    use TOUZA_Std,only: new_unit, sus_open, is_error_match, upcase
#if OPT_WITH_NCTCDF
    use TOUZA_Nio,only: nct_open_write
#endif
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: u
    character(len=*),intent(in)    :: file
    integer,         intent(in)    :: fmt
    integer,optional,intent(in)    :: mode
    integer,optional,intent(in)    :: levv
    integer,optional,intent(in)    :: uref
    integer md
    character(len=16)  :: stt, pos, act
    character(len=128) :: iomsg
    logical bx

    ierr = 0

    md = choice(fwrite_mode, mode)
    if (md.lt.0) then
       ierr = ERR_INVALID_PARAMETER
    else if (md.eq.fmode_default) then
       md = fmode_new
    endif

    if (ierr.eq.0) then
       inquire(file=file, exist=bx, IOSTAT=ierr)
101    format('exist: ', A)
       if (bx) then
          write(iomsg, 101) trim(file)
          call message(ierr, iomsg, levm=msglev_WARNING)
       endif
    endif
    iomsg = ' '
    if (ierr.eq.0) then
       if (is_cfmt_cdf(fmt)) then
#if OPT_WITH_NCTCDF
          select case (md)
          case (fmode_new)
             stt = 'N'
          case (fmode_write)
             stt = 'R'
          case (fmode_append)
             stt = 'U'
          case default
             stt = ' '
          end select
          call nct_open_write(ierr, u, file, status=stt, iomsg=iomsg)
#else /* not OPT_WITH_NCTCDF */
          ierr = ERR_NOT_IMPLEMENTED
#endif /* not OPT_WITH_NCTCDF */
       else
          u = choice(-1, uref)
          if (u.lt.0) u = new_unit()
          ierr = min(0, u)
          if (ierr.lt.0) then
             ierr = ERR_INVALID_PARAMETER
          else if (is_cfmt_ascii(fmt)) then
             select case (md)
             case (fmode_new)
                stt = 'NEW'
                pos = 'ASIS'
             case (fmode_write)
                stt = 'REPLACE'
                pos = 'ASIS'
             case (fmode_append)
                stt = 'UNKNOWN'
                pos = 'APPEND'
             case default
                stt = 'UNKNOWN'
                pos = 'ASIS'
             end select
             open(UNIT=u, FILE=file, IOSTAT=ierr, &
                  & ACTION='WRITE', FORM='FORMATTED', &
                  & ACCESS='SEQUENTIAL', STATUS=trim(stt), POSITION=trim(pos))
          else if (is_cfmt_binary(fmt) .or. is_cfmt_nio(fmt)) then
             select case (md)
             case (fmode_new)
                stt = 'N'
             case (fmode_write)
                stt = 'R'
                pos = ' '
             case (fmode_append)
                stt = 'U'
                pos = 'AP'
             case default
                stt = ' '
                pos = ' '
             end select
             act = 'RW'
             if (ierr.eq.0) then
                call sus_open &
                     & (ierr, u, file, ACTION=act, STATUS=stt, POSITION=pos, IOMSG=iomsg)
             endif
          else
             ierr = ERR_NOT_IMPLEMENTED
             iomsg = 'invalid format'
          endif
       endif
    endif
    if (ierr.ne.0) call message(ierr, iomsg, levm=levv)
  end subroutine open_write
!!!_  & is_cfmt_ascii()
  PURE &
  logical function is_cfmt_ascii(cfmt) result(b)
    implicit none
    integer,intent(in) :: cfmt
    b = cfmt.eq.cfmt_ascii
  end function is_cfmt_ascii
!!!_  & is_cfmt_binary()
  PURE &
  logical function is_cfmt_binary(cfmt) result(b)
    implicit none
    integer,intent(in) :: cfmt
    b = (cfmt_binary.le.cfmt) .and. (cfmt.lt.cfmt_cdf)
  end function is_cfmt_binary
!!!_  & is_cfmt_cdf()
  PURE &
  logical function is_cfmt_cdf(cfmt) result(b)
    implicit none
    integer,intent(in) :: cfmt
    b = cfmt.eq.cfmt_cdf
  end function is_cfmt_cdf
!!!_  & is_cfmt_nio()
  PURE &
  logical function is_cfmt_nio(cfmt) result(b)
    implicit none
    integer,intent(in) :: cfmt
    b = (cfmt_gtool_seq.le.cfmt) .and. (cfmt.lt.cfmt_gtool)
  end function is_cfmt_nio

!!!_  - set_header_plane
  subroutine set_header_plane &
       & (ierr, head, dfmt, mx, xname, my, yname, mz, zname)
    use TOUZA_Nio,only: put_header_cprop, put_item
    use TOUZA_Nio,only: hi_DFMT
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

!!!_ + End Jmz_base
end module Jmz_base
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

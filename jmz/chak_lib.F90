!!!_! chak_lib.F90 - TOUZA/Jmz swiss(CH) army knife library
! Maintainer: SAITO Fuyuki
! Created: Oct 13 2022
#define TIME_STAMP 'Time-stamp: <2023/01/07 22:22:37 fuyuki chak_lib.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022
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
#ifndef   OPT_DESC_LEN
#  define OPT_DESC_LEN 1024
#endif
#ifndef   OPT_CHAK_FILES
#  define OPT_CHAK_FILES    128
#endif
#ifndef   OPT_CHAK_BUFFERS
#  define OPT_CHAK_BUFFERS  256
#endif
!!!_@ TOUZA/Jmz/chak-lib - nio swiss army knife (library)
module chak_lib
!!!_ + Declaration
!!!_  - modules
  use TOUZA_Std,only: KFLT,  KDBL,   KIOFS
  use TOUZA_Std,only: is_msglev
  use TOUZA_Std,only: msglev_NORMAL, msglev_INFO, msglev_DEBUG
  use TOUZA_Std,only: msglev_WARNING, msglev_DETAIL
  use TOUZA_Std,only: is_msglev_DETAIL, is_msglev_NORMAL, is_msglev_INFO, is_msglev_DEBUG
  use TOUZA_Nio,only: litem, nitem, GFMT_END
  implicit none
  public
!!!_  - parameters
  integer,parameter :: KBUF = __KBUF

  integer,parameter :: lcoor = 6
  integer,parameter :: mcoor = 3    ! standard max coordinates

  real(kind=KBUF),parameter :: ZERO  = 0.0_KBUF
  real(kind=KBUF),parameter :: ONE   = 1.0_KBUF

  real(kind=KBUF),parameter :: TRUE  = ONE
  real(kind=KBUF),parameter :: FALSE = ZERO

  real(kind=KBUF),parameter :: ULIMIT = + HUGE(ZERO)
  real(kind=KBUF),parameter :: LLIMIT = - HUGE(ZERO)

  real(kind=KBUF),parameter :: UNDEF  = -999.0_KBUF
  ! real(kind=KBUF),parameter :: UNDEF  = LLIMIT

!!!_  - coordinate matching
  integer,parameter :: co_unset = -5
  integer,parameter :: co_del   = -4
  integer,parameter :: co_null  = -3
  integer,parameter :: co_ins   = -2
  integer,parameter :: co_wild  = -1
  integer,parameter :: co_normal = 0
!!!_  - file formats (except for gtool formats)
  integer,parameter :: cfmt_org = GFMT_END
  integer,parameter :: cfmt_ascii  = 1 + cfmt_org
  integer,parameter :: cfmt_binary = 2 + cfmt_org
  integer,parameter :: cfmt_flag_native = 0
  integer,parameter :: cfmt_flag_swap = 1
  integer,parameter :: cfmt_flag_big = 2
  integer,parameter :: cfmt_flag_little = 3
  integer,parameter :: cfmt_flags_bo = 4

  integer,parameter :: cfmt_binary_i4 = cfmt_binary
  integer,parameter :: cfmt_binary_r4 = cfmt_binary + cfmt_flags_bo
  integer,parameter :: cfmt_binary_r8 = cfmt_binary + cfmt_flags_bo * 2

  integer,parameter :: cfmt_cdf = cfmt_binary_r8 + 16
  ! integer,parameter :: cfmt_cdf_i4 = cfmt_cdf + 1
  ! integer,parameter :: cfmt_cdf_r4 = cfmt_cdf + 2
  ! integer,parameter :: cfmt_cdf_r8 = cfmt_cdf + 3

!!!_  - variable types
  integer,parameter :: kv_null = 0
  integer,parameter :: kv_int  = 1
  integer,parameter :: kv_flt  = 2
  integer,parameter :: kv_dbl  = 3

!!!_  - range special
  integer,parameter :: full_range = + HUGE(0)              ! case low:
  integer,parameter :: null_range = (- HUGE(0)) - 1        ! case low (MUST BE NEGATIVE)

!!!_  - character (symbols) for command-line
  character(len=*),parameter :: param_sep = '='
  character(len=*),parameter :: rename_sep = '/'
  character(len=*),parameter :: range_sep = ':'
  character(len=*),parameter :: item_sep = ','
  character(len=*),parameter :: rec_append_sep = '/'
  character(len=*),parameter :: rec_num_sep = '+'

  character(len=*),parameter :: insert_coor = '+'
  character(len=*),parameter :: delete_coor = '-'

!!!_  - character (symbols) for ascii output
  character(len=*),parameter :: amiss = '_'  ! character for missing value
  character(len=*),parameter :: aext  = '.'  ! character for external
!!!_  - i/o units
  integer,save :: ulog = -1
  integer,save :: uerr = -1

!!!_  - string length
  integer,parameter :: lname = litem * 4
  integer,parameter :: lpath = OPT_PATH_LEN
  integer,parameter :: ldesc = OPT_DESC_LEN

!!!_  - handles and types
  integer,parameter :: lfile = OPT_CHAK_FILES
  integer,parameter :: lbuffer = OPT_CHAK_BUFFERS
  integer,parameter :: lopr = 512

  integer,save :: mfile = 0
  integer,save :: mbuffer = 0
  integer,save :: mopr = 0

  integer,parameter :: lmodule = max(lopr, lfile, lbuffer) * 2
!!!_  - handle offsets and kinds
  integer,parameter :: hk_error = -1
  integer,parameter :: hk_opr = 0
  integer,parameter :: hk_file = 1
  integer,parameter :: hk_buffer = 2
  integer,parameter :: hk_anchor = 3
  integer,parameter :: hk_overflow = 4

  integer,parameter :: ofs_opr    = lmodule * hk_opr
  integer,parameter :: ofs_file   = lmodule * hk_file
  integer,parameter :: ofs_buffer = lmodule * hk_buffer
  integer,parameter :: ofs_anchor = lmodule * hk_anchor

!!!_  - global flags
  integer,save :: lev_verbose = 0
  integer,save :: dbgv = -1
  integer,save :: stdv = -1

  integer,save :: user_offset_bgn = 0     ! begin-index offset (user-friendly)
  integer,save :: user_offset_end = 0     ! end-index offset (user-friendly)
!!!_  - common values
  real(kind=KBUF),save :: PI = ZERO
!!!_  - domain property
  type domain_t
     integer :: n                   ! total size
     integer :: mco                 ! coordinate size
     integer :: ofs(0:lcoor-1)
     integer :: iter(0:lcoor-1)
     integer :: strd(0:lcoor)       ! with sentry
     integer :: bgn(0:lcoor-1)
     integer :: end(0:lcoor-1)
     integer :: cidx(0:lcoor-1)     ! coordinate index (physical)
     integer :: lidx(0:lcoor-1)     ! coordinate index (logical)
  end type domain_t
!!!_  - loop property
  type loop_t
     integer :: bgn = -1
     integer :: end = -1
     integer :: stp = -1
     character(len=lname) :: name
  end type loop_t

  type(loop_t),save :: def_loop  = loop_t(null_range, null_range, -1, ' ')

!!!_  - procedures
contains
!!!_  - initialization
!!!_   . init
  subroutine init(ierr)
    use TOUZA_Std,only: env_init, MPI_COMM_NULL, stdout=>uout, stderr=>uerr
    implicit none
    integer,intent(out) :: ierr

    ierr = 0

    if (ierr.eq.0) call env_init(ierr, levv=stdv, icomm=MPI_COMM_NULL)
    if (ierr.eq.0) ulog = stdout
    if (ierr.eq.0) uerr = stderr

    if (PI.eq.ZERO) PI = ATAN2(ZERO, -ONE)
  end subroutine init
!!!_  - utilities
!!!_   . message
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
                write(txt, fmt) iadd(:)
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

!!!_   . file_h2item()
  integer function file_h2item(handle) result(n)
    implicit none
    integer,intent(in) :: handle
    n = handle - ofs_file
    if (n.ge.0.and.n.lt.min(mfile, lfile)) then
       continue
    else
       n = ERR_INVALID_ITEM
    endif
  end function file_h2item
!!!_   . file_i2handle()
  integer function file_i2handle(item) result(n)
    implicit none
    integer,intent(in) :: item
    if (item.ge.0.and.item.lt.min(mfile, lfile)) then
       n = item + ofs_file
    else
       n = ERR_INVALID_ITEM
    endif
  end function file_i2handle
!!!_   . buf_h2item()
  ELEMENTAL integer function buf_h2item(handle) result(n)
    implicit none
    integer,intent(in) :: handle
    n = handle - ofs_buffer
    if (n.ge.0.and.n.lt.min(mbuffer, lbuffer)) then
       continue
    else
       n = ERR_INVALID_ITEM
    endif
  end function buf_h2item
!!!_   . buf_i2handle()
  integer function buf_i2handle(item) result(n)
    implicit none
    integer,intent(in) :: item
    if (item.ge.0.and.item.lt.min(mbuffer, lbuffer)) then
       n = item + ofs_buffer
    else
       n = ERR_INVALID_ITEM
    endif
  end function buf_i2handle

!!!_   . is_operator()
  logical function is_operator(handle) result(b)
    implicit none
    integer,intent(in) :: handle
    b = (handle_type(handle) .eq. hk_opr)
  end function is_operator

!!!_   . is_anchor()
  logical function is_anchor(handle) result(b)
    implicit none
    integer,intent(in) :: handle
    b = (handle_type(handle) .eq. hk_anchor)
  end function is_anchor

!!!_   . handle_type
  integer function handle_type(handle) result(k)
    implicit none
    integer,intent(in) :: handle

    if (handle.lt.0) then
       k = hk_error
    else
       k = handle / lmodule
       if (k.ge.hk_overflow) k = hk_error
    endif
    return
  end function handle_type

!!!_  - index
!!!_   . set_user_offsets
  subroutine set_user_offsets &
       & (ierr, off_bgn, off_end)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: off_bgn
    integer,intent(in),optional :: off_end
    ierr = 0
    user_offset_bgn = choice(user_offset_bgn, off_bgn)
    user_offset_end = choice(user_offset_end, off_end)
  end subroutine set_user_offsets

!!!_   . conv_physical_index
  PURE &
  integer function conv_physical_index (jlog, domL, domR) result(n)
    implicit none
    integer,       intent(in) :: jlog
    type(domain_t),intent(in) :: domL, domR
    integer jc
    integer jcur, ncur
    n = 0
    ncur = jlog
    !NEC$ novector
    do jc = 0, domL%mco - 1
       jcur = mod(ncur, domL%iter(jc))
       ncur = ncur / domL%iter(jc)
       if (domR%bgn(jc).le.jcur.and.jcur.lt.domR%end(jc)) then
          n = n + (domR%ofs(jc) + jcur) * domR%strd(jc)
       else
          n = -1
          exit
       endif
    enddo
  end function conv_physical_index

!!!_  - index function
!!!_   . user_index_bgn()
  ELEMENTAL integer function user_index_bgn(j, n) result(k)
    implicit none
    integer,intent(in)          :: j
    integer,intent(in),optional :: n
    if (j.eq.full_range .or. j.eq.null_range) then
       k = j
    else
       k = j + user_offset_bgn
    endif
  end function user_index_bgn
!!!_   . user_index_end()
  ELEMENTAL integer function user_index_end(j, n) result(k)
    implicit none
    integer,intent(in)          :: j
    integer,intent(in),optional :: n
    if (j.eq.full_range .or. j.eq.null_range) then
       k = j
    else
       k = j + user_offset_end
    endif
  end function user_index_end
!!!_   . system_index_bgn()
  ELEMENTAL integer function system_index_bgn(j, n) result(k)
    implicit none
    integer,intent(in)          :: j
    integer,intent(in),optional :: n
    if (j.eq.full_range .or. j.eq.null_range) then
       k = j
    else
       k = j - user_offset_bgn
    endif
  end function system_index_bgn
!!!_   . system_index_end()
  ELEMENTAL integer function system_index_end(j, n) result(k)
    implicit none
    integer,intent(in)          :: j
    integer,intent(in),optional :: n
    if (j.eq.full_range .or. j.eq.null_range) then
       k = j
    else
       k = j - user_offset_end
    endif
  end function system_index_end
!!!_   . logical_index
  ELEMENTAL integer function logical_index (l, p) result(n)
    implicit none
    integer,intent(in) :: l, p
    if (l.eq.null_range) then
       n = p
    else
       n = l
    endif
  end function logical_index
!!!_   . physical_index
  PURE integer function physical_index (lidx, dom) result(n)
    implicit none
    integer,       intent(in) :: lidx(0:*)
    type(domain_t),intent(in) :: dom
    integer jc
    n = 0
    do jc = 0, dom%mco - 1
       if (dom%bgn(jc).le.lidx(jc).and.lidx(jc).lt.dom%end(jc)) then
          n = n + (dom%ofs(jc) + lidx(jc)) * dom%strd(jc)
       else
          n = -1
          exit
       endif
    enddo
  end function physical_index

!!!_   . incr_logical_index
  subroutine incr_logical_index(idx, dom)
    implicit none
    integer,       intent(inout) :: idx(0:*)
    type(domain_t),intent(in)    :: dom
    integer jc, k
    k = 1
    do jc = 0, dom%mco - 1
       idx(jc) = idx(jc) + k
       k = idx(jc) / dom%iter(jc)
       idx(jc) = mod(idx(jc), dom%iter(jc))
    enddo
  end subroutine incr_logical_index


!!!_  - coordinate manipulation
!!!_   . coordinate_type()
  integer function coordinate_type(name, lcp, pcp) result(n)
    implicit none
    character(len=*),intent(in)          :: name
    type(loop_t),    intent(in)          :: lcp
    type(loop_t),    intent(in),optional :: pcp
    integer b, e

    if (name.ne.' ') then
       n = co_normal
       return
    endif
    n = co_unset
    if (present(pcp)) then
       if (lcp%stp.lt.0) then
          if (pcp%stp.gt.0) then
             n = co_wild
          else
             n = co_null
          endif
       else
          if (pcp%stp.le.0) then
             b = lcp%bgn
             e = lcp%end
          else
             b = logical_index(lcp%bgn, pcp%bgn)
             e = logical_index(lcp%end, pcp%end)
          endif
          if (b.eq.null_range .eqv. e.eq.null_range) then
             if (b.eq.null_range) then
                n = co_null
             else if (b.lt.e) then
                n = co_wild
             else
                n = co_null
             endif
          else
             n = co_wild
          endif
       endif
    else
       if (lcp%stp.lt.0) then
          n = co_null
       else
          b = lcp%bgn
          e = lcp%end
          if (b.eq.null_range .eqv. e.eq.null_range) then
             if (b.eq.null_range) then
                n = co_null
             else if (b.lt.e) then
                n = co_wild
             else
                n = co_null
             endif
          else
             n = co_wild
          endif
       endif
    endif

  end function coordinate_type

!!!_   . is_null_coor() - check if argument loop_t corresponds to null-coordinate
  logical function is_null_coor(lpp) result (b)
    implicit none
    type(loop_t),intent(in) :: lpp
    b = (lpp%stp.le.0.and.lpp%name.eq.' ')
  end function is_null_coor

!!!_   . parse_coordinate_repl - parse coordinate argument complex
  subroutine parse_coordinate_repl &
       & (ierr, cold, xold, crep, xrep, str)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: cold, crep
    logical,         intent(out) :: xold, xrep
    character(len=*),intent(in)  :: str          ! range parameter must be deleted before call

    integer lstr,  lsep
    integer jsep0, jsep1
    integer jp
    ierr = 0

    lstr = len_trim(str)
    if (lstr.eq.0) then
       ! (null)
       xold = .FALSE.
       xrep = .FALSE.
       cold = ' '
       crep = ' '
       return
    endif
    jsep0 = index(str, rename_sep)
    if (jsep0.eq.0) then
       if (index(str(1:lstr), insert_coor).eq.1) then
          ! +REPL
          xold = .FALSE.
          xrep = .TRUE.
          jp = len_trim(insert_coor)
          cold = str(1:jp)
          crep = str(jp+1:lstr)
       else
          ! NAME
          xold = .FALSE.
          xrep = .FALSE.
          cold = str(1:lstr)
          crep = ' '
       endif
       return
    endif

    ! [NAME]/[REPL[/]]
    xold = .TRUE.
    cold = str(1:jsep0-1)
    lsep = len(rename_sep)
    jsep1 = index(str(jsep0+lsep:), rename_sep)
    if (jsep1.eq.0) then
       ! [NAME]/REPL
       crep = str(jsep0+lsep:lstr)
       xrep = (jsep0+lsep) .le. lstr
    else
       ! [NAME]/REPL/
       jsep1 = jsep0 + lsep + jsep1 - 2
       crep = str(jsep0+lsep:jsep1)
       xrep = .TRUE.
    endif

  end subroutine parse_coordinate_repl

!!!_ + end chak
end module chak_lib
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

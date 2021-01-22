!!!_! std_arg.F90 - touza/std run-time argument parser
! Maintainer:  SAITO Fuyuki
! Created: May 17 2019 (for flageolet)
! Cloned: Sep 8 2020 (original: xsrc/parser.F90)
#define TIME_STAMP 'Time-stamp: <2021/01/21 14:58:42 fuyuki std_arg.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2019-2021
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
!!!_! include
#ifdef HAVE_CONFIG_H
#  include "touza_config.h"
#endif
#include "touza_std.h"
!!!_! Macros
#ifndef   ARG_ENTRY_LIM
#  define ARG_ENTRY_LIM 128 /* entry limit */
#endif
#ifndef   ARG_TAG_LEN
#  define ARG_TAG_LEN 32    /* tag length */
#endif
#ifndef   ARG_VALUE_LEN
#  define ARG_VALUE_LEN 256  /* parameter length */
#endif
#ifndef   ARG_LINE_LEN
#  define ARG_LINE_LEN  256  /* configuration line length */
#endif
#ifndef   TEST_STD_ARG
#  define TEST_STD_ARG 0
#endif
!!!_@ TOUZA_Std_arg - argument parser by command-line and/or input file
module TOUZA_Std_arg
!!!_ + default
  implicit none
  private
!!!_ + parameters
  integer,parameter :: lentry = ARG_ENTRY_LIM ! limit of entries

  integer,parameter :: ltag = ARG_TAG_LEN    ! tag length
  integer,parameter :: lval = ARG_VALUE_LEN  ! value length

  integer,parameter,public :: PARAM_DEF  = 0
  integer,parameter,public :: PARAM_POS  = 1  ! parameters as positional arguments
  integer,parameter,public :: PARAM_FILE = 2  ! parameters as external files
!!!_ + static
  logical,save :: ofirst = .TRUE.

  integer,            save :: nacc(0:lentry) = 0
  character(len=ltag),save :: atags(0:lentry) = ' '
  character(len=lval),save :: avals(0:lentry) = ' '

  integer,save :: lrecurs = 0
  integer,save :: mentry = 0
  integer,save :: jparam = 0, nposargs = -1
  integer,save :: mflags = 0

  integer,save :: kparse_mode = PARAM_DEF

  character(len=16),save :: cassign = '='
  character(len=16),save :: csep = ','
  character(len=16),save :: tag_file='F'
  character(len=16),save :: cstdin = '-'
  character(len=16),save :: ccomment = '#'

  character(len=16),save :: ctagend = '--'
  character(len=16),save :: cundef = ' ###'
!!!_ + interfaces
  interface tag_search
     module procedure tag_search_str
     module procedure tag_search_pos
  end interface tag_search

  interface get_param
     module procedure get_param_a
     module procedure get_param_i
     module procedure get_param_f
     module procedure get_param_d
     module procedure get_param_ia
     module procedure get_param_fa
     module procedure get_param_da
  end interface get_param

  interface get_array
     module procedure get_array_i
     module procedure get_array_f
     module procedure get_array_d
  end interface get_array

  interface get_option
     module procedure get_option_a
     module procedure get_option_i
     module procedure get_option_f
     module procedure get_option_d
     module procedure get_option_ia
     module procedure get_option_fa
     module procedure get_option_da
  end interface get_option

  interface get_arg
     module procedure get_arg_a
  end interface get_arg

  interface get_key
     module procedure get_key_a
  end interface get_key

  interface get_value
     module procedure get_value_a
  end interface get_value

  interface get_value_seq
     module procedure get_value_seq_a
  end interface get_value_seq
  ! interface get_param_seq
  !    module procedure get_param_seq_i2
  !    module procedure get_param_seq_i3
  ! end interface get_param_seq

!!!_ + public
  public :: init, diag, finalize
  public :: decl_pos_arg
  public :: parse
  public :: get_param, get_array, get_option, get_arg
  public :: get_key
  public :: get_value,     get_value_a
  public :: get_value_seq, get_value_seq_a
  public :: inq_end_flags
  public :: check_param
!!!_ + basic procedures
contains
!!!_  & init
  subroutine init &
       &  (ierr, ulog, &
       &   lrec, cha,  chs, tagf, kmode)
    use TOUZA_Std_prc,only: prc_init=>init
    use TOUZA_Std_env,only: env_init=>init
    use TOUZA_Std_fun,only: fun_init=>init
    use TOUZA_Std_utl,only: choice, choice_a
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in),optional :: ulog
    integer,         intent(in),optional :: lrec
    character(len=*),intent(in),optional :: cha, chs
    character(len=*),intent(in),optional :: tagf
    integer,         intent(in),optional :: kmode
    ierr = 0
    if (ofirst) then
       if (ierr.eq.0) call prc_init(ierr, ulog)
       if (ierr.eq.0) call env_init(ierr, ulog)
       if (ierr.eq.0) call fun_init(ierr, ulog)
       if (ierr.eq.0) then
          call choice_a(csep, chs)
          if (csep.eq.' ') csep = ','
          call choice_a(cassign, cha)
          if (cassign.eq.' ') cassign = '='
          lrecurs = choice(0, lrec)
          if (lrecurs.le.0) lrecurs = 5
          call choice_a(tag_file, tagf)
          kparse_mode = choice(PARAM_DEF, kmode)
          if (kparse_mode.eq.PARAM_DEF) kparse_mode = PARAM_POS
          ! call collect_entries(ierr, lrec)
       endif
       ofirst = .false.
    endif

    return
  end subroutine init

!!!_  & diag
  subroutine diag &
       & (ierr, ulog)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: ulog
    ierr = 0
    if (ierr.eq.0) then
       call report_entries &
            & (ierr, nposargs, atags, avals, nacc, mentry, lentry, ulog)
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, ulog)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: ulog
    ierr = 0
    if (present(ulog)) continue ! dummy
    return
  end subroutine finalize

!!!_ + parsers
!!!_  & decl_pos_arg
  subroutine decl_pos_arg &
       & (ierr, tag, jpos)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: jpos  ! start from 1

    integer jpi, jentr

    ierr = 0
    if (nposargs.ge.0) then
       ! error if parsed already
       ierr = -1
       return
    endif

    jpi = choice(0, jpos)
    if (jpi.le.0) jpi = mentry + 1
    jentr = jpi - 1
    if (jentr.gt.lentry) then
       ierr = -1
       return
    endif
    if (present(tag)) then
       atags(jentr) = trim(tag)
    else
       atags(jentr) = ' '
    endif
    avals(jentr) = cundef
    mentry = mentry + 1
    return
  end subroutine decl_pos_arg

!!!_  & parse - batch parser
  subroutine parse (ierr)
    use TOUZA_Std_fun,only: new_unit
    implicit none
    integer,intent(out) :: ierr
    integer ucfg
    ierr = 0

    ucfg = new_unit()
    if (ucfg.le.0) then
       ierr = -1
       return
    endif

    ! mark number of registered arguments
    nposargs = mentry

    jparam = 0
    if (ierr.eq.0) then
       call parse_command(ierr, ucfg)
    endif
    if (ierr.eq.0) then
       call parse_file(ierr, ucfg)
    endif
    if (ierr.eq.0) then
       call post_parse(ierr)
    endif

    return
  end subroutine parse

!!!_   & parse_command
  subroutine parse_command &
       & (ierr,  &
       &  ucfg)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: ucfg

    integer jarg, nargs
    integer,parameter :: lstr=ARG_LINE_LEN
    integer l
    character(len=lstr) :: S

    ierr = 0

    open(unit=ucfg, IOSTAT=ierr, FORM='FORMATTED', STATUS='SCRATCH', ACTION='READWRITE')
#if HAVE_GET_COMMAND_ARGUMENT
    jarg  = 0
    nargs = COMMAND_ARGUMENT_COUNT()
    do
       if (ierr.ne.0) exit
       jarg = jarg + 1
       if (jarg.gt.nargs) exit
       CALL GET_COMMAND_ARGUMENT(jarg, S, l, STATUS=ierr)
       ! write(*, *) 'G', jarg, trim(S)
       if (l.gt.lstr) then
101       format('too long argument at ', I0, ':', A)
          write(*, 101) jarg, trim(S)
          ierr = -1
       else if (ierr.eq.0) then
          write(ucfg, '(A)', IOSTAT=ierr) trim(S)
       endif
    enddo
#else  /* not HAVE_GET_COMMAND_ARGUMENT */
102 format(A, A, A)
    if (ierr.eq.0) then
       write(ucfg, 102) trim(tag_file), trim(cassign), trim(cstdin)
    endif
#endif /* not HAVE_GET_COMMAND_ARGUMENT */

    if (ierr.eq.0) rewind(ucfg, IOSTAT=ierr)

    if (ierr.eq.0) then
       call store_entries &
            & (ierr,   atags,    avals,    &
            &  jparam, nposargs, mentry,   lentry,  &
            &  ucfg,   cassign,  ccomment, ctagend, cundef)
    endif

    if (ierr.eq.0) close(ucfg, IOSTAT=ierr)
    return
  end subroutine parse_command

!!!_   & parse_file
  subroutine parse_file &
       & (ierr, ucfg)
    use TOUZA_Std_env,only: uin
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: ucfg

    integer jr
    character(len=ltag) :: fdone

    integer je, jt
    integer metmp, jptmp, nptmp
    character(len=ltag) :: ttags(0:lentry)
    character(len=lval) :: tvals(0:lentry)
    logical expand
    integer ucur

    ierr  = 0

    write(fdone, '(A, A)') trim(ccomment), trim(tag_file)

    looprec: do jr = 0, lrecurs
       expand = .false.
       je = 0
       if (ierr.ne.0) exit
       loope: do
          je = je + 1
          if (je.gt.mentry) exit
          ! write(*, *) 'P', jr, je, trim(atags(je)), trim(avals(je))
          if (trim(atags(je)).eq.trim(tag_file) &
               & .or. (atags(je).eq.' '.and.je.gt.nposargs &
               &       .and. kparse_mode.eq.PARAM_FILE)) then
             ! write(*, *) 'X', jr, je, trim(atags(je)), trim(avals(je))
             ! check if already expanded
             do jt = 1, mentry
                if (trim(atags(jt)).eq.trim(fdone) &
                     & .and. trim(avals(jt)).eq.trim(avals(je))) then
                   cycle loope
                endif
             enddo
             if (avals(je).eq.cstdin) then
                ucur = uin
             else
                ucur = ucfg
                open(unit=ucur, FILE=avals(je), IOSTAT=ierr, FORM='FORMATTED', STATUS='OLD', ACTION='READ')
                if (ierr.ne.0) then
                   write(*, *) 'cannot open ', trim(avals(je)), ierr
                   exit looprec
                endif
             endif
             metmp = 0
             jptmp = 0
             nptmp = 0
             call store_entries &
                  & (ierr,   ttags,    tvals,    &
                  &  jptmp,  nptmp,    metmp,    lentry,  &
                  &  ucur,   cassign,  ccomment, ctagend, cundef)
             if (ierr.eq.0) then
                if (ucur.eq.ucfg) close(ucur, IOSTAT=ierr)
                atags(je) = fdone
                expand = .TRUE.
             endif
             if (ierr.eq.0) then
                je = je + 1
                call insert_entries &
                     & (ierr, atags, avals, je,  mentry, lentry, &
                     &        ttags, tvals, metmp)
             endif
          endif
       enddo loope
       if (.not.expand) exit looprec
    enddo looprec

    return
  end subroutine parse_file

!!!_   . post_parse
  subroutine post_parse &
       & (ierr)
    implicit none
    integer,intent(out)   :: ierr

    integer jentr
    integer jpx
    character(len=ltag) :: tag

    ierr  = 0
    jpx = 0

    do jentr = 0, mentry - 1
       if (atags(jentr).eq.' ') then
          jpx = jpx + 1
          call tag_pos(tag, jpx)
          atags(jentr) = tag
       endif
    enddo
    return
  end subroutine post_parse

!!!_ + inquiries
!!!_  & get_param - get parameter (positional argument)
  subroutine get_param_a &
       & (ierr, val, jpos, def)
    use TOUZA_Std_utl,only: choice_a
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: val
    integer,         intent(in)          :: jpos
    character(len=*),intent(in),optional :: def
    integer jentr

    ierr = 0
    jentr = tag_search(jpos, atags, mentry)
    call extract_val_a(ierr, val, jentr, cundef, def)
    return
  end subroutine get_param_a

  subroutine get_param_i &
       & (ierr, val, jpos, def)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(inout)       :: val
    integer,intent(in)          :: jpos
    integer,intent(in),optional :: def
    integer jentr

    ierr = 0
    jentr = tag_search(jpos, atags, mentry)
    call extract_val_i(ierr, val, jentr, cundef, def)

    return
  end subroutine get_param_i

  subroutine get_param_f &
       & (ierr, val, jpos, def)
    use TOUZA_Std_prc,only: KFLT
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KFLT),intent(inout)       :: val
    integer,        intent(in)          :: jpos
    real(kind=KFLT),intent(in),optional :: def
    integer jentr

    ierr = 0
    jentr = tag_search(jpos, atags, mentry)
    call extract_val_f(ierr, val, jentr, cundef, def)

    return
  end subroutine get_param_f

  subroutine get_param_d &
       & (ierr, val, jpos, def)
    use TOUZA_Std_prc,only: KDBL
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KDBL),intent(inout)       :: val
    integer,        intent(in)          :: jpos
    real(kind=KDBL),intent(in),optional :: def
    integer jentr

    ierr = 0
    jentr = tag_search(jpos, atags, mentry)
    call extract_val_d(ierr, val, jentr, cundef, def)

    return
  end subroutine get_param_d

  subroutine get_param_ia &
       & (ierr, vals, jpos, def, sep)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(inout)       :: vals(:)
    integer,         intent(in)          :: jpos
    integer,         intent(in),optional :: def
    character(len=*),intent(in),optional :: sep
    integer jentr

    ierr = 0
    jentr = tag_search(jpos, atags, mentry)
    call extract_vals_i(ierr, vals(:), jentr, cundef, def, sep)

    return
  end subroutine get_param_ia

  subroutine get_param_fa &
       & (ierr, vals, jpos, def, sep)
    use TOUZA_Std_prc,only: KFLT
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KFLT), intent(inout)       :: vals(:)
    integer,         intent(in)          :: jpos
    real(kind=KFLT), intent(in),optional :: def
    character(len=*),intent(in),optional :: sep
    integer jentr

    ierr = 0
    jentr = tag_search(jpos, atags, mentry)
    call extract_vals_f(ierr, vals(:), jentr, cundef, def, sep)

    return
  end subroutine get_param_fa

  subroutine get_param_da &
       & (ierr, vals, jpos, def, sep)
    use TOUZA_Std_prc,only: KDBL
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KDBL), intent(inout)       :: vals(:)
    integer,         intent(in)          :: jpos
    real(kind=KDBL), intent(in),optional :: def
    character(len=*),intent(in),optional :: sep
    integer jentr

    ierr = 0
    jentr = tag_search(jpos, atags, mentry)
    call extract_vals_d(ierr, vals(:), jentr, cundef, def, sep)

    return
  end subroutine get_param_da

!!!_  & get_array - get parameter array (positional argument)
  subroutine get_array_i &
       & (ierr, nitem, vals, jpos, def, sep)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: nitem
    integer,         intent(inout)       :: vals(:)
    integer,         intent(in)          :: jpos
    integer,         intent(in),optional :: def
    character(len=*),intent(in),optional :: sep
    integer jentr

    ierr = 0
    jentr = tag_search(jpos, atags, mentry)
    call extract_vals_i(ierr, vals(:), jentr, cundef, def, sep, nitem)

    return
  end subroutine get_array_i

  subroutine get_array_f &
       & (ierr, nitem, vals, jpos, def, sep)
    use TOUZA_Std_prc,only: KFLT
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: nitem
    real(kind=KFLT), intent(inout)       :: vals(:)
    integer,         intent(in)          :: jpos
    real(kind=KFLT), intent(in),optional :: def
    character(len=*),intent(in),optional :: sep
    integer jentr

    ierr = 0
    jentr = tag_search(jpos, atags, mentry)
    call extract_vals_f(ierr, vals(:), jentr, cundef, def, sep, nitem)

    return
  end subroutine get_array_f

  subroutine get_array_d &
       & (ierr, nitem, vals, jpos, def, sep)
    use TOUZA_Std_prc,only: KDBL
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: nitem
    real(kind=KDBL), intent(inout)       :: vals(:)
    integer,         intent(in)          :: jpos
    real(kind=KDBL), intent(in),optional :: def
    character(len=*),intent(in),optional :: sep
    integer jentr

    ierr = 0
    jentr = tag_search(jpos, atags, mentry)
    call extract_vals_d(ierr, vals(:), jentr, cundef, def, sep, nitem)

    return
  end subroutine get_array_d

!!!_  & get_option - get option (key/value argument)
  subroutine get_option_a &
       & (ierr, val, tag, def, idx)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: val
    character(len=*),intent(in)          :: tag
    character(len=*),intent(in),optional :: def
    integer,         intent(in),optional :: idx
    integer jentr
    ierr = 0
    jentr = tag_search(tag, atags, mentry, idx)
    call extract_val_a(ierr, val, jentr, cundef, def)
    return
  end subroutine get_option_a

  subroutine get_option_i &
       & (ierr, val, tag, def, idx)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(inout)       :: val
    character(len=*),intent(in)          :: tag
    integer,         intent(in),optional :: def
    integer,         intent(in),optional :: idx
    integer jentr
    ierr = 0
    jentr = tag_search(tag, atags, mentry, idx)
    call extract_val_i(ierr, val, jentr, cundef, def)
    return
  end subroutine get_option_i

  subroutine get_option_f &
       & (ierr, val, tag, def, idx)
    use TOUZA_Std_prc,only: KFLT
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KFLT), intent(inout)       :: val
    character(len=*),intent(in)          :: tag
    real(kind=KFLT), intent(in),optional :: def
    integer,         intent(in),optional :: idx
    integer jentr
    ierr = 0
    jentr = tag_search(tag, atags, mentry, idx)
    call extract_val_f(ierr, val, jentr, cundef, def)
    return
  end subroutine get_option_f

  subroutine get_option_d &
       & (ierr, val, tag, def, idx)
    use TOUZA_Std_prc,only: KDBL
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KDBL), intent(inout)       :: val
    character(len=*),intent(in)          :: tag
    real(kind=KDBL), intent(in),optional :: def
    integer,         intent(in),optional :: idx
    integer jentr
    ierr = 0
    jentr = tag_search(tag, atags, mentry, idx)
    call extract_val_d(ierr, val, jentr, cundef, def)
    return
  end subroutine get_option_d

  subroutine get_option_ia &
       & (ierr, vals, tag, def, idx, sep)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(inout)       :: vals(:)
    character(len=*),intent(in)          :: tag
    integer,         intent(in),optional :: def
    integer,         intent(in),optional :: idx
    character(len=*),intent(in),optional :: sep
    integer jentr
    ierr = 0
    jentr = tag_search(tag, atags, mentry, idx)
    call extract_vals_i(ierr, vals(:), jentr, cundef, def, sep)
    return
  end subroutine get_option_ia

  subroutine get_option_fa &
       & (ierr, vals, tag, def, idx, sep)
    use TOUZA_Std_prc,only: KFLT
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KFLT), intent(inout)       :: vals(:)
    character(len=*),intent(in)          :: tag
    real(kind=KFLT), intent(in),optional :: def
    integer,         intent(in),optional :: idx
    character(len=*),intent(in),optional :: sep
    integer jentr
    ierr = 0
    jentr = tag_search(tag, atags, mentry, idx)
    call extract_vals_f(ierr, vals(:), jentr, cundef, def, sep)
    return
  end subroutine get_option_fa

  subroutine get_option_da &
       & (ierr, vals, tag, def, idx, sep)
    use TOUZA_Std_prc,only: KDBL
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KDBL), intent(inout)       :: vals(:)
    character(len=*),intent(in)          :: tag
    real(kind=KDBL), intent(in),optional :: def
    integer,         intent(in),optional :: idx
    character(len=*),intent(in),optional :: sep
    integer jentr
    ierr = 0
    jentr = tag_search(tag, atags, mentry, idx)
    call extract_vals_d(ierr, vals(:), jentr, cundef, def, sep)
    return
  end subroutine get_option_da

!!!_  * check_param() - check parameter to return integer
  integer function check_param &
       & (param, str, num, swch, ndef) &
       & result (idx)
    use TOUZA_Std_utl,only: choice
    implicit none
    character(len=*),intent(in)         :: param
    character(len=*),intent(in)         :: str
    integer,         intent(in)         :: num
    logical,         intent(in),optional:: swch
    integer,         intent(in),optional:: ndef

    if (choice(.false., swch)) then
       if (INDEX(str, param(1:1)).gt.0) then
          idx = num
       else
          idx = choice(-1, ndef)
       endif
    else
       if (param.eq.str) then
          idx = num
       else
          idx = choice(-1, ndef)
       endif
    endif
    return
  end function check_param

!!!_  & get_arg - get key/value at given entry
  subroutine get_arg_a &
       & (ierr, tag, val, jentr)
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(out)   :: tag
    character(len=*),intent(out)   :: val
    integer,         intent(inout) :: jentr

    ierr = 0
    tag = ' '
    val = ' '
    do
       if (jentr.ge.mentry) then
          jentr = -1
          return
       endif
       if (atags(jentr)(1:1).eq.ccomment(1:1)) then
          continue
       else if (atags(jentr)(1:1).ne.' ') then
          exit
       else if (avals(jentr).ne.' ') then
          exit
       endif
       jentr = jentr + 1
    enddo
    tag = trim(ADJUSTL(atags(jentr)))
    val = trim(ADJUSTL(avals(jentr)))
    jentr = jentr + 1
    return
  end subroutine get_arg_a

!!!_  & get_key - get key at given entry
  subroutine get_key_a &
       & (ierr, tag, jentr)
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(out)   :: tag
    integer,         intent(inout) :: jentr

    ierr = 0
    tag = ' '
    do
       if (jentr.ge.mentry) then
          jentr = -1
          return
       endif
       if (atags(jentr)(1:1).eq.ccomment(1:1)) then
          continue
       else if (atags(jentr)(1:1).ne.' ') then
          exit
       else if (avals(jentr).ne.' ') then
          exit
       endif
       jentr = jentr + 1
    enddo
    tag = trim(ADJUSTL(atags(jentr)))
    jentr = jentr + 1
    return
  end subroutine get_key_a

!!!_  & get_value - get value at given entry
  subroutine get_value_a &
       & (ierr, val, jentr)
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(out)   :: val
    integer,         intent(inout) :: jentr

    ierr = 0
    val = ' '
    do
       if (jentr.ge.mentry) then
          jentr = -1
          return
       endif
       if (atags(jentr)(1:1).eq.ccomment(1:1)) then
          continue
       else if (atags(jentr)(1:1).ne.' ') then
          exit
       else if (avals(jentr).ne.' ') then
          exit
       endif
       jentr = jentr + 1
    enddo
    val = trim(ADJUSTL(avals(jentr)))
    jentr = jentr + 1
    return
  end subroutine get_value_a

!!!_  & get_value_seq - get value sequence at given entry
  subroutine get_value_seq_a &
       & (ierr, val, num, jentr)
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(out)   :: val(*)
    integer,         intent(in)    :: num
    integer,         intent(inout) :: jentr

    integer j
    ierr = 0
    do j = 1, num
       if (jentr.lt.0) then
          ierr = -1
          exit
       endif
       call get_value_a(ierr, val(j), jentr)
    enddo
    return
  end subroutine get_value_seq_a

!!!_  & get_param_seq
#if 0
  subroutine get_param_seq_i2 &
       & (ierr, Tag, idef, iv0, iv1)
    implicit none
    integer,         intent(out)  :: ierr
    character(len=*),intent(in)   :: Tag
    integer,         intent(in)   :: idef
    integer,         intent(out)  :: iv0
    integer,         intent(out)  :: iv1

    integer ktmp(1:2)
    call get_param_ia(ierr, Tag, ktmp(:), idef)
    if (ierr.eq.0) then
       iv0 = ktmp(1)
       iv1 = ktmp(2)
    endif
    return
  end subroutine get_param_seq_i2

  subroutine get_param_seq_i3 &
       & (ierr, Tag, idef, iv0, iv1, iv2)
    implicit none
    integer,         intent(out)  :: ierr
    character(len=*),intent(in)   :: Tag
    integer,         intent(in)   :: idef
    integer,         intent(out)  :: iv0
    integer,         intent(out)  :: iv1
    integer,         intent(out)  :: iv2

    integer ktmp(1:3)
    call get_param_ia(ierr, Tag, ktmp(:), idef)
    if (ierr.eq.0) then
       iv0 = ktmp(1)
       iv1 = ktmp(2)
       iv2 = ktmp(3)
    endif
    return
  end subroutine get_param_seq_i3

#endif
!!!_  & inq_end_flags
  integer function inq_end_flags () &
       & result(r)
    r = mflags + 1
  end function inq_end_flags

!!!_ + internal procedures
!!!_  & report_entries
  subroutine report_entries &
       & (ierr, &
       &  NP,   &
       &  T,    V,   NA, me, le, &
       &  ulog)
    use TOUZA_Std_log,only: msg
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: NP
    character(len=*),intent(in)          :: T(0:*)
    character(len=*),intent(in)          :: V(0:*)
    integer,         intent(in)          :: NA(0:*)
    integer,         intent(in)          :: me
    integer,         intent(in)          :: le
    integer,         intent(in),optional :: ulog

    integer je
    character(len=1024) :: txt

    ierr  = 0
103 format(I0, 2x, A, 3x, A)
    do je = 0, me - 1
       write(txt, 103) NA(je), trim(T(je)), trim(V(je))
       call msg(txt, 'std/arg', 0, ulog)
    enddo

    return
  end subroutine report_entries

!!!_  & store_entries
  subroutine store_entries &
       & (ierr, &
       &  T,    V,   jpa,  npa,  me,   le,  &
       &  ucfg, cha, chc,  che,  chu)
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(out)   :: T(0:*)
    character(len=*),intent(out)   :: V(0:*)
    integer,         intent(inout) :: jpa
    integer,         intent(in)    :: npa
    integer,         intent(inout) :: me
    integer,         intent(in)    :: le
    integer,         intent(in)    :: ucfg
    character(len=*),intent(in)    :: cha ! assign
    character(len=*),intent(in)    :: chc ! comment
    character(len=*),intent(in)    :: che ! end
    character(len=*),intent(in)    :: chu ! unset

    integer j, l
    integer jpe
    integer jerr
    integer,parameter   :: lstr = 1024
    character(len=lstr) :: S
    integer lcha
    logical bset

    ierr  = 0
    lcha = len_trim(cha)
    do
       if (ierr.ne.0) exit
       if (ucfg.ge.0) then
          read(ucfg, '(A)', IOSTAT=jerr) S
       else
          read(*, '(A)', IOSTAT=jerr) S
       endif
       if (jerr.ne.0) exit
       ! write(*, *) 'F', jpa, npa, me, le, trim(S)
       S = ADJUSTL(S)
       if (S(1:1).eq.chc(1:1)) cycle
       if (ierr.eq.0) then
          l = len_trim(S)
          if (l.ge.lstr) then
101          format('too long line:', I0, 1x, A)
             write(*, 101) l, trim(S)
             ierr = 1
          endif
          if (l.eq.0) cycle
       endif
       if (ierr.eq.0) then
          if (Trim(S).eq.che) then
             jpa = npa + 1
             T(me) = Trim(S)
             V(me) = ' '
             me = me + 1
          else
             j = index(S, trim(cha))
             ! write(*, *) jpa, me, j, '/', trim(S(1:j-1)), '/', trim(S(j+lcha:)), '/'
             bset = .FALSE.
             if (j.gt.1) then
                do jpe = 0, npa - 1
                   if (T(jpe).eq.trim(S(1:j-1)) &
                        & .and. V(jpe).eq.cundef) then
                      V(jpe) = trim(S(j+lcha:))
                      bset = .TRUE.
                      exit
                   endif
                enddo
             endif
             if (.not.bset) then
                if (j.le.1) then
                   do jpe = jpa, npa - 1
                      if (V(jpe).eq.cundef) then
                         bset = .TRUE.
                         V(jpe) = trim(S(j+1:))
                         jpa = jpe
                         exit
                      endif
                   enddo
                endif
             endif
             if (.not.bset) then
                if (j.gt.0) then
                   T(me) = trim(S(1:j-1))
                   V(me) = trim(S(j+lcha:))
                else
                   T(me) = ' '
                   V(me) = trim(S)
                endif
                me = me + 1
             endif
          endif
       endif
       if (me.ge.le) then
          ierr = -1
       endif
    enddo

    return
  end subroutine store_entries

!!!_  & insert_entries
  subroutine insert_entries &
       & (ierr, T, V, je,  me, le, Tins, Vins, mins)
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(inout) :: T(0:*)
    character(len=*),intent(inout) :: V(0:*)
    integer,         intent(inout) :: je
    integer,         intent(inout) :: me
    integer,         intent(in)    :: le
    character(len=*),intent(in)    :: Tins(0:*)
    character(len=*),intent(in)    :: Vins(0:*)
    integer,         intent(in)    :: mins

    integer mbtm
    integer j
    ierr = 0

    mbtm = me
    me   = me + mins
    if (me.gt.le) then
       ierr = -1
       return
    endif

    do j = mbtm, je, -1
       T(j+mins) = T(j)
       V(j+mins) = V(j)
    enddo
    do j = 0, mins - 1
       T(je+j) = Tins(j)
       V(je+j) = Vins(j)
    enddo
    je = je + mins

    return
  end subroutine insert_entries

!!!_  & inquire_entry
  subroutine inquire_entry &
       & (jentr, tgt, T, me, jposflg)
    implicit none
    integer,         intent(out)            :: jentr
    character(len=*),intent(in)             :: tgt
    character(len=*),intent(in)             :: T(0:*)
    integer,         intent(in)             :: me
    integer,         intent(inout),optional :: jposflg

    integer jpx

    jpx = -1
    if (present(jposflg)) then
       jpx = jposflg
    endif
    jentr = tag_search(tgt, T, me, jpx)
    if (jentr.lt.0) then
       if (present(jposflg)) then
          jposflg = me
       endif
    else
       if (present(jposflg)) then
          jposflg = jentr + 1
       endif
    endif

    return
  end subroutine inquire_entry

!!!_  & tag_pos
  subroutine tag_pos(tag, jpos)
    implicit none
    character(len=*),intent(out) :: tag
    integer,         intent(in)  :: jpos
101 format(A, I0)
    write(tag, 101) trim(ccomment), jpos
    return
  end subroutine tag_pos

!!!_  & tag_search()
  integer function tag_search_pos &
       & (jpos, T, me, jbegin) &
       & result(r)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(in)          :: jpos
    character(len=*),intent(in)          :: T(0:*)
    integer,         intent(in)          :: me
    integer,         intent(in),optional :: jbegin

    character(len=ltag) :: tgt
    integer jb

    call tag_pos(tgt, jpos)
    jb = choice(0, jbegin)     ! default: search from first
    r = tag_search_str(tgt, T, me, jb)
    return
  end function tag_search_pos

  integer function tag_search_str &
       & (tgt, T, me, jbegin) &
       & result(r)
    use TOUZA_Std_utl,only: choice
    implicit none
    character(len=*),intent(in)          :: tgt
    character(len=*),intent(in)          :: T(0:*)
    integer,         intent(in)          :: me
    integer,         intent(in),optional :: jbegin

    integer je
    integer jb
    r = -1
    jb = choice(-1, jbegin)     ! default: search from last
    if (jb.lt.0) then
       do je = me + jb, 0, -1
          if (T(je).eq.Tgt) then
             r = je
             exit
          endif
       enddo
    else
       do je = jb, me - 1
          if (T(je).eq.Tgt) then
             r = je
             exit
          endif
       enddo
    endif
    return
  end function tag_search_str

!!!_  & extract_val - single
  subroutine extract_val_i &
       & (ierr, val, jentr, cud, def)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(inout)       :: val
    integer,         intent(in)          :: jentr
    character(len=*),intent(in)          :: cud
    integer,         intent(in),optional :: def

    ierr = 0
    if (jentr.ge.0.and.jentr.lt.mentry) then
       if (avals(jentr).eq.cud) ierr = 1
    else
       ierr = 1
    endif
    if (ierr.eq.0) then
       read(avals(jentr), *, IOSTAT=ierr) val
       if (ierr.ne.0) then
          val = choice(val, def)
       endif
    else if (present(def)) then
       val = choice(val, def)
       ierr = 0
    endif

  end subroutine extract_val_i

  subroutine extract_val_f &
       & (ierr, val, jentr, cud, def)
    use TOUZA_Std_prc,only: KFLT
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,          intent(out)         :: ierr
    real(kind=KFLT),  intent(inout)       :: val
    integer,          intent(in)          :: jentr
    character(len=*), intent(in)          :: cud
    real(kind=KFLT),  intent(in),optional :: def

    ierr = 0
    if (jentr.ge.0.and.jentr.lt.mentry) then
       if (avals(jentr).eq.cud) ierr = 1
    else
       ierr = 1
    endif
    if (ierr.eq.0) then
       read(avals(jentr), *, IOSTAT=ierr) val
       if (ierr.ne.0) then
          val = choice(val, def)
       endif
    else if (present(def)) then
       val = choice(val, def)
       ierr = 0
    endif

  end subroutine extract_val_f

  subroutine extract_val_d &
       & (ierr, val, jentr, cud, def)
    use TOUZA_Std_prc,only: KDBL
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KDBL), intent(inout)       :: val
    integer,         intent(in)          :: jentr
    character(len=*),intent(in)          :: cud
    real(kind=KDBL), intent(in),optional :: def

    ierr = 0
    if (jentr.ge.0.and.jentr.lt.mentry) then
       if (avals(jentr).eq.cud) ierr = 1
    else
       ierr = 1
    endif
    if (ierr.eq.0) then
       read(avals(jentr), *, IOSTAT=ierr) val
       if (ierr.ne.0) then
          val = choice(val, def)
       endif
    else if (present(def)) then
       val = choice(val, def)
       ierr = 0
    endif

  end subroutine extract_val_d

  subroutine extract_val_a &
       & (ierr, val, jentr, cud, def)
    use TOUZA_Std_utl,only: choice_a
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: val
    integer,         intent(in)          :: jentr
    character(len=*),intent(in)          :: cud
    character(len=*),intent(in),optional :: def

    ierr = 0
    if (jentr.ge.0.and.jentr.lt.mentry) then
       if (avals(jentr).eq.cud) ierr = 1
    else
       ierr = 1
    endif
    if (ierr.eq.0) then
       val = avals(jentr)
    else if (present(def)) then
       call choice_a(val, ' ', def)
       ierr = 0
    endif
    return
  end subroutine extract_val_a

!!!_  & extract_vals - array
  subroutine extract_vals_i &
       & (ierr, vals, jentr, cud, def, sep, nitem)
    use TOUZA_Std_utl,only: choice, choice_a
    implicit none
    integer,         intent(out)          :: ierr
    integer,         intent(inout)        :: vals(:)
    integer,         intent(in)           :: jentr
    character(len=*),intent(in)           :: cud
    integer,         intent(in), optional :: def
    character(len=*),intent(in), optional :: sep
    integer,         intent(out),optional :: nitem
    integer jb, je
    integer jv, nv
    character(len=ltag) :: chs

    ierr = 0
    jv = 0
    if (jentr.ge.0.and.jentr.lt.mentry) then
       if (avals(jentr).eq.cud) ierr = 1
    else
       ierr = 1
    endif
    if (ierr.eq.0) then
       call choice_a(chs, csep, sep)
       jb = 1
       nv = size(vals)
       do
          if (jb.le.0) exit
          if (jv.ge.nv) ierr = 1
          if (ierr.ne.0) exit
          je = search_next_sep(avals(jentr), jb, chs)
          if (je.gt.jb) then
             read(avals(jentr)(jb:je-1), *, IOSTAT=ierr) vals(1+jv)
          else if (jb.eq.je) then
             if (present(def)) then
                vals(jv) = def
             endif
          else
             read(avals(jentr)(jb:), *, IOSTAT=ierr) vals(1+jv)
          endif
          jv = jv + 1
          jb = je + 1
       enddo
    else
       if (present(def)) then
          vals(:) = def
          ierr = 0
       endif
    endif
    if (present(nitem)) then
       nitem = jv
    endif
    return
  end subroutine extract_vals_i

  subroutine extract_vals_f &
       & (ierr, vals, jentr, cud, def, sep, nitem)
    use TOUZA_Std_prc,only: KFLT
    use TOUZA_Std_utl,only: choice, choice_a
    implicit none
    integer,         intent(out)          :: ierr
    real(kind=KFLT), intent(inout)        :: vals(:)
    integer,         intent(in)           :: jentr
    character(len=*),intent(in)           :: cud
    real(kind=KFLT), intent(in), optional :: def
    character(len=*),intent(in), optional :: sep
    integer,         intent(out),optional :: nitem
    integer jb, je
    integer jv, nv
    character(len=ltag) :: chs

    ierr = 0
    jv = 0
    if (jentr.ge.0.and.jentr.lt.mentry) then
       if (avals(jentr).eq.cud) ierr = 1
    else
       ierr = 1
    endif
    if (ierr.eq.0) then
       call choice_a(chs, csep, sep)
       jb = 1
       nv = size(vals)
       do
          if (jb.le.0) exit
          if (jv.ge.nv) ierr = 1
          if (ierr.ne.0) exit
          je = search_next_sep(avals(jentr), jb, chs)
          if (je.gt.jb) then
             read(avals(jentr)(jb:je-1), *, IOSTAT=ierr) vals(1+jv)
          else if (jb.eq.je) then
             if (present(def)) then
                vals(jv) = def
             endif
          else
             read(avals(jentr)(jb:), *, IOSTAT=ierr) vals(1+jv)
          endif
          jv = jv + 1
          jb = je + 1
       enddo
    else
       if (present(def)) then
          vals(:) = def
          ierr = 0
       endif
    endif
    if (present(nitem)) then
       nitem = jv
    endif
    return
  end subroutine extract_vals_f

  subroutine extract_vals_d &
       & (ierr, vals, jentr, cud, def, sep, nitem)
    use TOUZA_Std_prc,only: KDBL
    use TOUZA_Std_utl,only: choice, choice_a
    implicit none
    integer,         intent(out)          :: ierr
    real(kind=KDBL), intent(inout)        :: vals(:)
    integer,         intent(in)           :: jentr
    character(len=*),intent(in)           :: cud
    real(kind=KDBL), intent(in), optional :: def
    character(len=*),intent(in), optional :: sep
    integer,         intent(out),optional :: nitem
    integer jb, je
    integer jv, nv
    character(len=ltag) :: chs

    ierr = 0
    jv = 0
    if (jentr.ge.0.and.jentr.lt.mentry) then
       if (avals(jentr).eq.cud) ierr = 1
    else
       ierr = 1
    endif
    if (ierr.eq.0) then
       call choice_a(chs, csep, sep)
       jb = 1
       nv = size(vals)
       do
          if (jb.le.0) exit
          if (jv.ge.nv) ierr = 1
          if (ierr.ne.0) exit
          je = search_next_sep(avals(jentr), jb, chs)
          if (je.gt.jb) then
             read(avals(jentr)(jb:je-1), *, IOSTAT=ierr) vals(1+jv)
          else if (jb.eq.je) then
             if (present(def)) then
                vals(jv) = def
             endif
          else
             read(avals(jentr)(jb:), *, IOSTAT=ierr) vals(1+jv)
          endif
          jv = jv + 1
          jb = je + 1
       enddo
    else
       if (present(def)) then
          vals(:) = def
          ierr = 0
       endif
    endif
    if (present(nitem)) then
       nitem = jv
    endif
    return
  end subroutine extract_vals_d

!!!_  & search_next_sep()
  integer function search_next_sep &
       & (str, jpbgn, c) &
       & result(r)
    implicit none
    integer,         intent(in) :: jpbgn
    character(len=*),intent(in) :: str
    character(len=*),intent(in) :: c

    r = index(str(jpbgn:), trim(c))
    if (r.gt.0) then
       r = jpbgn - 1 + r
    else
       r = -1
    endif
  end function search_next_sep

end module TOUZA_Std_arg

!!!_@ test_std_arg - test program
#if TEST_STD_ARG
program test_std_arg
  use TOUZA_Std_arg
  implicit none
  integer ierr
  character(len=1024) :: val
  integer,parameter :: nv = 3
  integer ival
  integer ivals(nv)
  integer jp
  character(len=128) :: tag

  ierr = 0
  call init(ierr, lrec=0)

  if (ierr.eq.0) call decl_pos_arg(ierr)
  if (ierr.eq.0) call decl_pos_arg(ierr, 'X')
  if (ierr.eq.0) call decl_pos_arg(ierr, 'X')
  if (ierr.eq.0) call decl_pos_arg(ierr, 'Y')

  if (ierr.eq.0) call parse(ierr)

  val = ' '
  ival = -999
  ivals(:)= -9999
  if (ierr.eq.0) then
     do jp = 1, 5
        call get_param(ierr, val, jp)
        write(*, *) 'POS=', jp, ierr, '[', trim(val), ']'
     enddo
     do jp = 1, 5
        call get_param(ierr, ival, jp)
        write(*, *) 'POS=', jp, ierr, '[', ival, ']'
     enddo
     do jp = 1, 5
        call get_param(ierr, ivals(:), jp)
        write(*, *) 'POS=', jp, ierr, '[', ivals(:), ']'
     enddo

     tag = 'X'
     call get_option(ierr, val, tag)
     write(*, *) 'TAG=', trim(tag), ' ', ierr, '[', trim(val), ']'

     tag = 'Y'
     call get_option(ierr, val, tag)
     write(*, *) 'TAG=', trim(tag), ' ', ierr, '[', trim(val), ']'

     ierr = 0
  endif
  if (ierr.eq.0) then
     jp = 0
     do
        call get_arg(ierr, tag, val, jp)
        if (jp.lt.0) exit
        if (ierr.ne.0) exit
        write(*, *) 'ARGS=', jp, trim(tag), ' [', trim(val), ']'
     enddo
  endif
  call diag(ierr)
  stop
end program test_std_arg
#endif /* TEST_STD_ARG */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

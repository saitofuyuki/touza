!!!_! std_arg.F90 - touza/std run-time argument parser
! Maintainer:  SAITO Fuyuki
! Created: May 17 2019 (for flageolet)
! Cloned: Sep 8 2020 (original: xsrc/parser.F90)
#define TIME_STAMP 'Time-stamp: <2023/04/17 14:27:50 fuyuki std_arg.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2019-2023
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
#  define ARG_ENTRY_LIM 512 /* entry limit */
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
!!!_ + command-line argument parser switch
#ifndef   OPT_USE_COMMAND_LINE_ARGS
#  if     HAVE_FORTRAN_GET_COMMAND_ARGUMENT && HAVE_FORTRAN_COMMAND_ARGUMENT_COUNT
#     define OPT_USE_COMMAND_LINE_ARGS 1
#  elif   HAVE_FORTRAN_GETARG && HAVE_FORTRAN_IARGC
#     define OPT_USE_COMMAND_LINE_ARGS 1
#  endif
#endif
#ifndef   OPT_USE_COMMAND_LINE_ARGS
#  define OPT_USE_COMMAND_LINE_ARGS 0
#endif
!!!_@ TOUZA_Std_arg - argument parser by command-line and/or input file
module TOUZA_Std_arg
  use TOUZA_Std_log,only: unit_global,  trace_fine,   trace_control,  get_logu
!!!_ + default
  implicit none
  private
# define __MDL__ 'arg'
# define _ERROR(E) (E - ERR_MASK_STD_ARG)
!!!_ + parameters
  integer,parameter :: lentry = ARG_ENTRY_LIM ! limit of entries

  integer,parameter :: ltag = ARG_TAG_LEN    ! tag length
  integer,parameter :: lval = ARG_VALUE_LEN  ! value length

  integer,parameter,public :: PARAM_DEF  = 0
  integer,parameter,public :: PARAM_POS  = 1  ! parameters as positional arguments
  integer,parameter,public :: PARAM_FILE = 2  ! parameters as external files

!!!_ + static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global

  integer,            save :: nacc(0:lentry) = 0
  character(len=ltag),save :: atags(0:lentry) = ' '
  character(len=lval),save :: avals(0:lentry) = ' '

  integer,save :: lrecurs = 0
  integer,save :: mentry = 0
  integer,save :: jparam = 0, nregs = -1, nposs = -1
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
     module procedure get_param_i,  get_param_f,  get_param_d
     module procedure get_param_ia, get_param_fa, get_param_da
  end interface get_param

  interface get_array
     module procedure get_array_i, get_array_f, get_array_d
  end interface get_array

  interface get_option
     module procedure get_option_a
     module procedure get_option_i,  get_option_f,  get_option_d
     module procedure get_option_ia, get_option_fa, get_option_da
  end interface get_option

  interface parse_param
     module procedure parse_param_ia, parse_param_fa, parse_param_da
  end interface parse_param

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
  public :: get_nparam
  public :: get_param, get_array, get_option, get_arg
  public :: get_key
  public :: get_value,     get_value_a
  public :: get_value_seq, get_value_seq_a
  public :: parse_param
  public :: inq_end_flags
  public :: check_param
  public :: cmdline_count_wrap, cmdline_arg_wrap
!!!_ + basic procedures
contains
!!!_  & init
  subroutine init &
       &  (ierr, &
       &   u,    levv, mode, &
       &   lrec, cha,  chs, tagf, kmode, icomm)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_prc,only: prc_init=>init
    use TOUZA_Std_env,only: env_init=>init
    use TOUZA_Std_fun,only: fun_init=>init
    use TOUZA_Std_log,only: log_init=>init
    use TOUZA_Std_utl,only: utl_init=>init, choice, choice_a
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv
    integer,         intent(in),optional :: mode
    integer,         intent(in),optional :: lrec       ! recursive limit
    character(len=*),intent(in),optional :: cha, chs   ! characters for assignment, separator
    character(len=*),intent(in),optional :: tagf       ! FILE tag
    integer,         intent(in),optional :: kmode
    integer,         intent(in),optional :: icomm

    integer md, lv, lmd

    ierr = 0

    md = control_mode(mode, MODE_DEEPEST)
    init_mode = md

    if (md.ge.MODE_SURFACE) then
       err_default = ERR_SUCCESS
       lv = choice(lev_verbose, levv)
       if (is_first_force(init_counts, mode)) then
          ulog = choice(ulog, u)
          lev_verbose = lv
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call prc_init(ierr, ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call utl_init(ierr, ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call log_init(ierr, ulog, levv=lv, mode=lmd)
          if (ierr.eq.0) call fun_init(ierr, ulog, levv=lv, mode=lmd, icomm=icomm)
          if (ierr.eq.0) call env_init(ierr, ulog, levv=lv, mode=lmd, icomm=icomm)
       endif
       if (is_first_force(init_counts, mode)) then
          if (ierr.eq.0) then
             call init_batch &
                  &  (ierr, lrec, cha,  chs, tagf, kmode, ulog, lv)
          endif
       endif
       init_counts = init_counts + 1
       if (ierr.ne.0) err_default = _ERROR(ERR_FAILURE_INIT)
    endif

    return
  end subroutine init

!!!_  & diag
  subroutine diag (ierr, u, levv, mode)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: utl_diag=>diag, choice
    use TOUZA_Std_prc,only: prc_diag=>diag
    use TOUZA_Std_fun,only: fun_diag=>diag
    use TOUZA_Std_env,only: env_diag=>diag
    use TOUZA_Std_log,only: log_diag=>diag, msg_mdl
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer utmp, md, lv, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       call trace_control &
            & (ierr, md, mdl=__MDL__, fun='diag', u=utmp, levv=lv)
       if (is_first_force(diag_counts, mode)) then
          if (ierr.eq.0) then
             if (VCHECK_NORMAL(lv)) then
                call msg_mdl(TIME_STAMP, __MDL__, utmp)
                call msg_mdl('(''command-line parser = '', I0)', &
                     & OPT_USE_COMMAND_LINE_ARGS, __MDL__, utmp)
             endif
             if (VCHECK_NORMAL(lv)) then
                call report_entries &
                     & (ierr, nregs, nposs, atags, avals, nacc, mentry, lentry, utmp)
             endif
          endif
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call prc_diag(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call utl_diag(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call log_diag(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call fun_diag(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call env_diag(ierr, utmp, lv, mode=lmd)
       endif
       diag_counts = diag_counts + 1
    endif
    return
  end subroutine diag

!!!_  & finalize
  subroutine finalize(ierr, u, levv, mode)
    use TOUZA_Std_utl,only: control_mode, control_deep, is_first_force
    use TOUZA_Std_utl,only: utl_finalize=>finalize, choice
    use TOUZA_Std_prc,only: prc_finalize=>finalize
    use TOUZA_Std_log,only: log_finalize=>finalize
    use TOUZA_Std_fun,only: fun_finalize=>finalize
    use TOUZA_Std_env,only: env_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: mode
    integer,intent(in),optional :: u
    integer utmp, lv, md, lmd

    ierr = err_default

    md = control_mode(mode, init_mode)
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

    if (md.ge.MODE_SURFACE) then
       if (is_first_force(fine_counts, mode)) then
          call trace_fine &
               & (ierr, md, init_counts, diag_counts, fine_counts, &
               &  pkg=__PKG__, grp=__GRP__, mdl=__MDL__, fun='finalize', u=utmp, levv=lv)
       endif
       lmd = control_deep(md, mode)
       if (md.ge.MODE_SHALLOW) then
          if (ierr.eq.0) call prc_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call utl_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call log_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call fun_finalize(ierr, utmp, lv, mode=lmd)
          if (ierr.eq.0) call env_finalize(ierr, utmp, lv, mode=lmd)
       endif
       fine_counts = fine_counts + 1
    endif
    return
  end subroutine finalize
!!!_ + init subcontracts
!!!_  & init_batch
  subroutine init_batch &
       &  (ierr, &
       &   lrec, cha,  chs, tagf, kmode, &
       &   u,    levv)
    use TOUZA_Std_utl,only: choice, choice_a
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in),optional :: lrec
    character(len=*),intent(in),optional :: cha, chs
    character(len=*),intent(in),optional :: tagf
    integer,         intent(in),optional :: kmode
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv

    ierr = 0

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

    return
  end subroutine init_batch
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

    ierr = err_default
    if (nregs.ge.0) then
       ! error if parsed already
       ierr = _ERROR(ERR_SECOND_INVOCATION)
       return
    endif

    jpi = choice(0, jpos)
    if (jpi.le.0) jpi = mentry + 1
    jentr = jpi - 1
    if (jentr.gt.lentry) then
       ierr = _ERROR(ERR_OUT_OF_RANGE)
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
    use TOUZA_Std_log,only: msg_mdl
    use TOUZA_Std_fun,only: new_unit
    implicit none
    integer,intent(out) :: ierr
    integer ucfg
    integer lu

    ierr = err_default
    if (nregs.ge.0) then
       lu = get_logu(ulog)
       call msg_mdl('parse twice.', __MDL__, lu)
       return
    endif

    ucfg = new_unit()
    if (ucfg.lt.0) then
       ierr = _ERROR(ERR_NO_IO_UNIT)
       return
    endif

    ! mark number of registered arguments
    nregs = mentry

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

#if OPT_USE_COMMAND_LINE_ARGS
    integer jarg, nargs
    integer l
    integer,parameter :: lstr=ARG_LINE_LEN
    character(len=lstr) :: S

#   if HAVE_FORTRAN_COMMAND_ARGUMENT_COUNT
#     define _COMMAND_ARGUMENT_COUNT() COMMAND_ARGUMENT_COUNT()
#   elif HAVE_FORTRAN_IARGC
#     define _COMMAND_ARGUMENT_COUNT() IARGC()
#   else
#     error "neither COMMAND_ARGUMENT_COUNT nor IARGC found"
#   endif

#endif /* OPT_USE_COMMAND_LINE_ARGS */

    ierr = err_default

    open(unit=ucfg, IOSTAT=ierr, FORM='FORMATTED', STATUS='SCRATCH', ACTION='READWRITE')
#if OPT_USE_COMMAND_LINE_ARGS
    jarg  = 0
    nargs = _COMMAND_ARGUMENT_COUNT()
    do
       if (ierr.ne.0) exit
       jarg = jarg + 1
       if (jarg.gt.nargs) exit
       call cmdline_arg_wrap(jarg, S, l, ierr)
       if (l.gt.lstr) then
101       format('too long argument at ', I0, ':', A)
          write(*, 101) jarg, trim(S)
          ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
       else if (ierr.eq.0) then
          write(ucfg, '(A)', IOSTAT=ierr) trim(S)
       endif
    enddo
#else  /* not OPT_USE_COMMAND_LINE_ARGS */
102 format(A, A, A)
    if (ierr.eq.0) then
       write(ucfg, 102) trim(tag_file), trim(cassign), trim(cstdin)
    endif
#endif /* not OPT_USE_COMMAND_LINE_ARGS */

    if (ierr.eq.0) rewind(ucfg, IOSTAT=ierr)

    if (ierr.eq.0) then
       call store_entries &
            & (ierr,   atags,    avals,    &
            &  jparam, nregs,    mentry,   lentry,  &
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

    ierr = err_default

    write(fdone, '(A, A)') trim(ccomment), trim(tag_file)

    looprec: do jr = 0, lrecurs
       expand = .false.
       je = -1
       if (ierr.ne.0) exit
       loope: do
          je = je + 1
          if (je.gt.mentry) exit
          ! write(*, *) 'P', jr, je, trim(atags(je)), trim(avals(je))
          if (trim(atags(je)).eq.trim(tag_file) &
               & .or. (atags(je).eq.' '.and.je.gt.nregs &
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
    character(len=ltag) :: tag

    ierr = err_default
    nposs = 0

    do jentr = 0, mentry - 1
       if (atags(jentr).eq.' ') then
          nposs = nposs + 1
          call tag_pos(tag, nposs)
          atags(jentr) = tag
       endif
    enddo
    return
  end subroutine post_parse

!!!_ + inquiries
!!!_  & get_nparam() - get number of positional parameters
  integer function get_nparam () result (n)
    implicit none
    integer jerr
    jerr = err_default
    if (jerr.eq.0) then
       n = nposs
    else
       n = -1
    endif
  end function get_nparam
!!!_  & get_param - get parameter (positional argument)
  subroutine get_param_a &
       & (ierr, val, jpos, def, unset)
    use TOUZA_Std_utl,only: choice_a
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: val
    integer,         intent(in)          :: jpos
    character(len=*),intent(in),optional :: def
    logical,         intent(in),optional :: unset
    integer jentr

    ierr = err_default
    jentr = tag_search(jpos, atags, mentry)
    call extract_val_a(ierr, val, jentr, cundef, def, unset=unset)
    return
  end subroutine get_param_a

  subroutine get_param_i &
       & (ierr, val, jpos, def, unset)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(inout)       :: val
    integer,intent(in)          :: jpos
    integer,intent(in),optional :: def
    logical,intent(in),optional :: unset
    integer jentr

    ierr = err_default
    jentr = tag_search(jpos, atags, mentry)
    call extract_val_i(ierr, val, jentr, cundef, def, unset=unset)
    return
  end subroutine get_param_i

  subroutine get_param_f &
       & (ierr, val, jpos, def, unset)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(inout)       :: val
    integer,        intent(in)          :: jpos
    real(kind=KTGT),intent(in),optional :: def
    logical,        intent(in),optional :: unset
    integer jentr

    ierr = err_default
    jentr = tag_search(jpos, atags, mentry)
    call extract_val_f(ierr, val, jentr, cundef, def, unset=unset)
    return
  end subroutine get_param_f

  subroutine get_param_d &
       & (ierr, val, jpos, def, unset)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(inout)       :: val
    integer,        intent(in)          :: jpos
    real(kind=KTGT),intent(in),optional :: def
    logical,        intent(in),optional :: unset
    integer jentr

    ierr = err_default
    jentr = tag_search(jpos, atags, mentry)
    call extract_val_d(ierr, val, jentr, cundef, def, unset=unset)
    return
  end subroutine get_param_d

  subroutine get_param_ia &
       & (ierr, vals, jpos, def, sep, unset)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(inout)       :: vals(:)
    integer,         intent(in)          :: jpos
    integer,         intent(in),optional :: def
    character(len=*),intent(in),optional :: sep
    logical,         intent(in),optional :: unset
    integer jentr

    ierr = err_default
    jentr = tag_search(jpos, atags, mentry)
    if (jentr.lt.0) then
       call default_vals_i(ierr, vals(:), def, unset=unset)
    else
       call extract_vals_i(ierr, vals(:), avals(jentr), cundef, def, sep, unset=unset)
    endif
    return
  end subroutine get_param_ia

  subroutine get_param_fa &
       & (ierr, vals, jpos, def, sep, unset)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KTGT), intent(inout)       :: vals(:)
    integer,         intent(in)          :: jpos
    real(kind=KTGT), intent(in),optional :: def
    character(len=*),intent(in),optional :: sep
    logical,         intent(in),optional :: unset
    integer jentr

    ierr = err_default
    jentr = tag_search(jpos, atags, mentry)
    if (jentr.lt.0) then
       call default_vals_f(ierr, vals(:), def, unset=unset)
    else
       call extract_vals_f(ierr, vals(:), avals(jentr), cundef, def, sep, unset=unset)
    endif
    return
  end subroutine get_param_fa

  subroutine get_param_da &
       & (ierr, vals, jpos, def, sep, unset)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KTGT), intent(inout)       :: vals(:)
    integer,         intent(in)          :: jpos
    real(kind=KTGT), intent(in),optional :: def
    character(len=*),intent(in),optional :: sep
    logical,         intent(in),optional :: unset
    integer jentr

    ierr = err_default
    jentr = tag_search(jpos, atags, mentry)
    if (jentr.lt.0) then
       call default_vals_d(ierr, vals(:), def, unset=unset)
    else
       call extract_vals_d(ierr, vals(:), avals(jentr), cundef, def, sep, unset=unset)
    endif
    return
  end subroutine get_param_da

!!!_  & get_array - get parameter array (positional argument)
  subroutine get_array_i &
       & (ierr, nitem, vals, jpos, def, sep, unset)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: nitem
    integer,         intent(inout)       :: vals(:)
    integer,         intent(in)          :: jpos
    integer,         intent(in),optional :: def
    character(len=*),intent(in),optional :: sep
    logical,         intent(in),optional :: unset
    integer jentr

    ierr = err_default
    jentr = tag_search(jpos, atags, mentry)
    if (jentr.lt.0) then
       call default_vals_i(ierr, vals(:), def, nitem, unset=unset)
    else
       call extract_vals_i(ierr, vals(:), avals(jentr), cundef, def, sep, nitem, unset=unset)
    endif
    return
  end subroutine get_array_i

  subroutine get_array_f &
       & (ierr, nitem, vals, jpos, def, sep, unset)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: nitem
    real(kind=KTGT), intent(inout)       :: vals(:)
    integer,         intent(in)          :: jpos
    real(kind=KTGT), intent(in),optional :: def
    character(len=*),intent(in),optional :: sep
    logical,         intent(in),optional :: unset
    integer jentr

    ierr = err_default
    jentr = tag_search(jpos, atags, mentry)
    if (jentr.lt.0) then
       call default_vals_f(ierr, vals(:), def, nitem, unset=unset)
    else
       call extract_vals_f(ierr, vals(:), avals(jentr), cundef, def, sep, nitem, unset=unset)
    endif
    return
  end subroutine get_array_f

  subroutine get_array_d &
       & (ierr, nitem, vals, jpos, def, sep, unset)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: nitem
    real(kind=KTGT), intent(inout)       :: vals(:)
    integer,         intent(in)          :: jpos
    real(kind=KTGT), intent(in),optional :: def
    character(len=*),intent(in),optional :: sep
    logical,         intent(in),optional :: unset
    integer jentr

    ierr = err_default
    jentr = tag_search(jpos, atags, mentry)
    if (jentr.lt.0) then
       call default_vals_d(ierr, vals(:), def, nitem, unset=unset)
    else
       call extract_vals_d(ierr, vals(:), avals(jentr), cundef, def, sep, nitem, unset=unset)
    endif
    return
  end subroutine get_array_d

!!!_  & get_option - get option (key/value argument)
  subroutine get_option_a &
       & (ierr, val, tag, def, idx, unset)
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: val
    character(len=*),intent(in)          :: tag
    character(len=*),intent(in),optional :: def
    integer,         intent(in),optional :: idx
    logical,         intent(in),optional :: unset
    integer jentr

    ierr = err_default
    jentr = tag_search(tag, atags, mentry, idx)
    call extract_val_a(ierr, val, jentr, cundef, def, unset=unset)
    return
  end subroutine get_option_a

  subroutine get_option_i &
       & (ierr, val, tag, def, idx, unset)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(inout)       :: val
    character(len=*),intent(in)          :: tag
    integer,         intent(in),optional :: def
    integer,         intent(in),optional :: idx
    logical,         intent(in),optional :: unset
    integer jentr

    ierr = err_default
    jentr = tag_search(tag, atags, mentry, idx)
    call extract_val_i(ierr, val, jentr, cundef, def, unset=unset)
    return
  end subroutine get_option_i

  subroutine get_option_f &
       & (ierr, val, tag, def, idx, unset)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KTGT), intent(inout)       :: val
    character(len=*),intent(in)          :: tag
    real(kind=KTGT), intent(in),optional :: def
    integer,         intent(in),optional :: idx
    logical,         intent(in),optional :: unset
    integer jentr

    ierr = err_default
    jentr = tag_search(tag, atags, mentry, idx)
    call extract_val_f(ierr, val, jentr, cundef, def, unset=unset)
    return
  end subroutine get_option_f

  subroutine get_option_d &
       & (ierr, val, tag, def, idx, unset)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KTGT), intent(inout)       :: val
    character(len=*),intent(in)          :: tag
    real(kind=KTGT), intent(in),optional :: def
    integer,         intent(in),optional :: idx
    logical,         intent(in),optional :: unset
    integer jentr

    ierr = err_default
    jentr = tag_search(tag, atags, mentry, idx)
    call extract_val_d(ierr, val, jentr, cundef, def, unset=unset)
    return
  end subroutine get_option_d

  subroutine get_option_ia &
       & (ierr, vals, tag, def, idx, sep, unset)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(inout)       :: vals(:)
    character(len=*),intent(in)          :: tag
    integer,         intent(in),optional :: def
    integer,         intent(in),optional :: idx
    character(len=*),intent(in),optional :: sep
    logical,         intent(in),optional :: unset
    integer jentr

    ierr = err_default
    jentr = tag_search(tag, atags, mentry, idx)
    if (jentr.lt.0) then
       call default_vals_i(ierr, vals(:), def, unset=unset)
    else
       call extract_vals_i(ierr, vals(:), avals(jentr), cundef, def, sep, unset=unset)
    endif
    return
  end subroutine get_option_ia

  subroutine get_option_fa &
       & (ierr, vals, tag, def, idx, sep, unset)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KTGT), intent(inout)       :: vals(:)
    character(len=*),intent(in)          :: tag
    real(kind=KTGT), intent(in),optional :: def
    integer,         intent(in),optional :: idx
    character(len=*),intent(in),optional :: sep
    logical,         intent(in),optional :: unset
    integer jentr

    ierr = err_default
    jentr = tag_search(tag, atags, mentry, idx)
    if (jentr.lt.0) then
       call default_vals_f(ierr, vals(:), def, unset=unset)
    else
       call extract_vals_f(ierr, vals(:), avals(jentr), cundef, def, sep, unset=unset)
    endif
    return
  end subroutine get_option_fa

  subroutine get_option_da &
       & (ierr, vals, tag, def, idx, sep, unset)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KTGT), intent(inout)       :: vals(:)
    character(len=*),intent(in)          :: tag
    real(kind=KTGT), intent(in),optional :: def
    integer,         intent(in),optional :: idx
    character(len=*),intent(in),optional :: sep
    logical,         intent(in),optional :: unset
    integer jentr

    ierr = err_default
    jentr = tag_search(tag, atags, mentry, idx)
    if (jentr.lt.0) then
       call default_vals_d(ierr, vals(:), def, unset=unset)
    else
       call extract_vals_d(ierr, vals(:), avals(jentr), cundef, def, sep, unset=unset)
    endif
    return
  end subroutine get_option_da

!!!_  & parse_param
  subroutine parse_param_ia &
       & (ierr, vals, str, def, sep, unset, nitem)
    implicit none
    integer,         intent(out)          :: ierr
    integer,         intent(inout)        :: vals(:)
    character(len=*),intent(in)           :: str
    integer,         intent(in),optional  :: def
    character(len=*),intent(in),optional  :: sep
    logical,         intent(in),optional  :: unset
    integer,         intent(out),optional :: nitem
    ierr = err_default
    if (ierr.eq.0) then
       call extract_vals_i(ierr, vals(:), str, cundef, def, sep, nitem=nitem, unset=unset)
    endif
    return
  end subroutine parse_param_ia
  subroutine parse_param_fa &
       & (ierr, vals, str, def, sep, unset, nitem)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    implicit none
    integer,         intent(out)          :: ierr
    real(kind=KTGT), intent(inout)        :: vals(:)
    character(len=*),intent(in)           :: str
    real(kind=KTGT), intent(in),optional  :: def
    character(len=*),intent(in),optional  :: sep
    logical,         intent(in),optional  :: unset
    integer,         intent(out),optional :: nitem
    ierr = err_default
    if (ierr.eq.0) then
       call extract_vals_f(ierr, vals(:), str, cundef, def, sep, nitem=nitem, unset=unset)
    endif
    return
  end subroutine parse_param_fa
  subroutine parse_param_da &
       & (ierr, vals, str, def, sep, unset, nitem)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    implicit none
    integer,         intent(out)          :: ierr
    real(kind=KTGT), intent(inout)        :: vals(:)
    character(len=*),intent(in)           :: str
    real(kind=KTGT), intent(in),optional  :: def
    character(len=*),intent(in),optional  :: sep
    logical,         intent(in),optional  :: unset
    integer,         intent(out),optional :: nitem
    ierr = err_default
    if (ierr.eq.0) then
       call extract_vals_d(ierr, vals(:), str, cundef, def, sep, nitem=nitem, unset=unset)
    endif
    return
  end subroutine parse_param_da

!!!_  & check_param() - check parameter to return integer
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

    ierr = err_default
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

    ierr = err_default
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

    ierr = err_default
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

    ierr = err_default
    do j = 1, num
       if (jentr.lt.0) then
          ierr = _ERROR(ERR_OUT_OF_RANGE)
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
!!!_  & inq_end_flags()
  integer function inq_end_flags () &
       & result(r)
    r = mflags + 1
  end function inq_end_flags

!!!_ + internal procedures
!!!_  & report_entries
  subroutine report_entries &
       & (ierr, &
       &  NR,   NP,  &
       &  T,    V,   NA, me, le, &
       &  ulog)
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: NR, NP
    character(len=*),intent(in)          :: T(0:*)
    character(len=*),intent(in)          :: V(0:*)
    integer,         intent(in)          :: NA(0:*)
    integer,         intent(in)          :: me
    integer,         intent(in)          :: le
    integer,         intent(in),optional :: ulog

    integer je
    character(len=1024) :: txt
    integer jerr

    ierr = err_default
104 format('arguments = ', I0, 1x, I0)
    write(txt, 104, IOSTAT=jerr) NP, NR
    call msg_mdl(txt, __MDL__, ulog)
103 format(I0, 2x, A, 3x, A)
    do je = 0, me - 1
       write(txt, 103, IOSTAT=jerr) NA(je), trim(T(je)), trim(V(je))
       call msg_mdl(txt, __MDL__, ulog)
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

    ierr = err_default
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
          ierr = _ERROR(ERR_OUT_OF_RANGE)
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

    ierr = err_default

    mbtm = me
    me   = me + mins
    if (me.gt.le) then
       ierr = _ERROR(ERR_OUT_OF_RANGE)
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
    integer jerr
101 format(A, I0)
    write(tag, 101, IOSTAT=jerr) trim(ccomment), jpos
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

  ! -1 <= r <  me
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
       & (ierr, val, jentr, cud, def, unset)
    use TOUZA_Std_utl,only: choice, parse_number
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(inout)       :: val
    integer,         intent(in)          :: jentr
    character(len=*),intent(in)          :: cud
    integer,         intent(in),optional :: def
    logical,         intent(in),optional :: unset

    ierr = err_default
    if (jentr.ge.0.and.jentr.lt.mentry) then
       if (avals(jentr).eq.cud) ierr = 1
    else
       ierr = 1
    endif
    if (ierr.eq.0) then
       if (avals(jentr).eq.' ') then
          if (present(def)) then
             val = def
          else
             ierr = _ERROR(ERR_NEED_ARGUMENT)
          endif
       else
          ! read(avals(jentr), *, IOSTAT=ierr) val
          call parse_number(ierr, val, avals(jentr))
          if (ierr.ne.0) val = choice(val, def)
       endif
    else if (present(def)) then
       val = choice(val, def)
       ierr = 0
    endif
    if (choice(.false.,unset)) ierr = min(0, ierr)
    return
  end subroutine extract_val_i

  subroutine extract_val_f &
       & (ierr, val, jentr, cud, def, unset)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    use TOUZA_Std_utl,only: choice, parse_number
    implicit none
    integer,          intent(out)         :: ierr
    real(kind=KTGT),  intent(inout)       :: val
    integer,          intent(in)          :: jentr
    character(len=*), intent(in)          :: cud
    real(kind=KTGT),  intent(in),optional :: def
    logical,          intent(in),optional :: unset

    ierr = err_default
    if (jentr.ge.0.and.jentr.lt.mentry) then
       if (avals(jentr).eq.cud) ierr = 1
    else
       ierr = 1
    endif
    if (ierr.eq.0) then
       if (avals(jentr).eq.' ') then
          if (present(def)) then
             val = def
          else
             ierr = _ERROR(ERR_NEED_ARGUMENT)
          endif
       else
          ! read(avals(jentr), *, IOSTAT=ierr) val
          call parse_number(ierr, val, avals(jentr))
          if (ierr.ne.0) val = choice(val, def)
       endif
    else if (present(def)) then
       val = choice(val, def)
       ierr = 0
    endif
    if (choice(.false.,unset)) ierr = min(0, ierr)
    return
  end subroutine extract_val_f

  subroutine extract_val_d &
       & (ierr, val, jentr, cud, def, unset)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    use TOUZA_Std_utl,only: choice, parse_number
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KTGT), intent(inout)       :: val
    integer,         intent(in)          :: jentr
    character(len=*),intent(in)          :: cud
    real(kind=KTGT), intent(in),optional :: def
    logical,         intent(in),optional :: unset

    ierr = err_default
    if (jentr.ge.0.and.jentr.lt.mentry) then
       if (avals(jentr).eq.cud) ierr = 1
    else
       ierr = 1
    endif
    if (ierr.eq.0) then
       if (avals(jentr).eq.' ') then
          if (present(def)) then
             val = def
          else
             ierr = _ERROR(ERR_NEED_ARGUMENT)
          endif
       else
          ! read(avals(jentr), *, IOSTAT=ierr) val
          call parse_number(ierr, val, avals(jentr))
          if (ierr.ne.0) val = choice(val, def)
       endif
    else if (present(def)) then
       val = choice(val, def)
       ierr = 0
    endif
    if (choice(.false.,unset)) ierr = min(0, ierr)
    return
  end subroutine extract_val_d

  subroutine extract_val_a &
       & (ierr, val, jentr, cud, def, unset)
    use TOUZA_Std_utl,only: choice_a, choice
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: val
    integer,         intent(in)          :: jentr
    character(len=*),intent(in)          :: cud
    character(len=*),intent(in),optional :: def
    logical,         intent(in),optional :: unset

    ierr = err_default
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
    if (choice(.false.,unset)) ierr = min(0, ierr)
    return
  end subroutine extract_val_a

!!!_  & extract_vals - array
  subroutine extract_vals_i &
       & (ierr, vals, str, cud, def, sep, nitem, unset)
    use TOUZA_Std_utl,only: choice, choice_a, parse_number
    implicit none
    integer,         intent(out)          :: ierr
    integer,         intent(inout)        :: vals(:)
    character(len=*),intent(in)           :: str
    character(len=*),intent(in)           :: cud
    integer,         intent(in), optional :: def
    character(len=*),intent(in), optional :: sep
    integer,         intent(out),optional :: nitem
    logical,         intent(in), optional :: unset
    integer jb, je, le
    integer jv, nv
    character(len=ltag) :: chs

    ierr = err_default
    jv = 0
    if (ierr.eq.0) then
       if (str.eq.cud) then
          if (present(def)) then
             vals(:) = def
          else
             ierr = 1
          endif
       else
          call choice_a(chs, csep, sep)
          jb = 1
          le = len_trim(str)
          nv = size(vals)
          if (present(def)) then
             vals(:) = def
          endif
          do
             if (jb.le.0.or.jb.gt.le) exit
             if (jv.ge.nv) ierr = 1
             if (ierr.ne.0) exit
             je = search_next_sep(str, jb, chs)
             if (je.gt.jb) then
                ! read(str(jb:je-1), *, IOSTAT=ierr) vals(1+jv)
                call parse_number(ierr, vals(1+jv), str(jb:je-1))
             else if (jb.eq.je) then
                continue
             else
                ! read(str(jb:), *, IOSTAT=ierr) vals(1+jv)
                call parse_number(ierr, vals(1+jv), str(jb:))
             endif
             jv = jv + 1
             jb = je + 1
          enddo
       endif
    endif
    if (present(nitem)) then
       nitem = jv
    endif
    ! write(*, *) nv, vals(:)
    if (choice(.false.,unset)) ierr = min(0, ierr)
    return
  end subroutine extract_vals_i

  subroutine extract_vals_f &
       & (ierr, vals, str, cud, def, sep, nitem, unset)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    use TOUZA_Std_utl,only: choice, choice_a, parse_number
    implicit none
    integer,            intent(out)          :: ierr
    real(kind=KTGT),    intent(inout)        :: vals(:)
    character(len=*),   intent(in)           :: str
    character(len=*),   intent(in)           :: cud
    real(kind=KTGT),    intent(in), optional :: def
    character(len=KTGT),intent(in), optional :: sep
    integer,            intent(out),optional :: nitem
    logical,            intent(in), optional :: unset
    integer jb, je, le
    integer jv, nv
    character(len=ltag) :: chs

    ierr = err_default
    jv = 0
    if (ierr.eq.0) then
       if (str.eq.cud) then
          if (present(def)) then
             vals(:) = def
          else
             ierr = 1
          endif
       else
          call choice_a(chs, csep, sep)
          jb = 1
          le = len_trim(str)
          nv = size(vals)
          if (present(def)) then
             vals(:) = def
          endif
          do
             if (jb.le.0.or.jb.gt.le) exit
             if (jv.ge.nv) ierr = 1
             if (ierr.ne.0) exit
             je = search_next_sep(str, jb, chs)
             if (je.gt.jb) then
                ! read(str(jb:je-1), *, IOSTAT=ierr) vals(1+jv)
                call parse_number(ierr, vals(1+jv), str(jb:je-1))
             else if (jb.eq.je) then
                continue
             else
                ! read(str(jb:), *, IOSTAT=ierr) vals(1+jv)
                call parse_number(ierr, vals(1+jv), str(jb:))
             endif
             jv = jv + 1
             jb = je + 1
          enddo
       endif
    endif
    if (present(nitem)) then
       nitem = jv
    endif
    if (choice(.false.,unset)) ierr = min(0, ierr)
    return
  end subroutine extract_vals_f

  subroutine extract_vals_d &
       & (ierr, vals, str, cud, def, sep, nitem, unset)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    use TOUZA_Std_utl,only: choice, choice_a, parse_number
    implicit none
    integer,         intent(out)          :: ierr
    real(kind=KTGT), intent(inout)        :: vals(:)
    character(len=*),intent(in)           :: str
    character(len=*),intent(in)           :: cud
    real(kind=KTGT), intent(in), optional :: def
    character(len=*),intent(in), optional :: sep
    integer,         intent(out),optional :: nitem
    logical,         intent(in), optional :: unset
    integer jb, je, le
    integer jv, nv
    character(len=ltag) :: chs

    ierr = err_default
    jv = 0
    if (ierr.eq.0) then
       if (str.eq.cud) then
          if (present(def)) then
             vals(:) = def
          else
             ierr = 1
          endif
       else
          call choice_a(chs, csep, sep)
          jb = 1
          le = len_trim(str)
          nv = size(vals)
          if (present(def)) then
             vals(:) = def
          endif
          do
             if (jb.le.0.or.jb.gt.le) exit
             if (jv.ge.nv) ierr = 1
             if (ierr.ne.0) exit
             je = search_next_sep(str, jb, chs)
             if (je.gt.jb) then
                ! read(str(jb:je-1), *, IOSTAT=ierr) vals(1+jv)
                call parse_number(ierr, vals(1+jv), str(jb:je-1))
             else if (jb.eq.je) then
                continue
             else
                ! read(str(jb:), *, IOSTAT=ierr) vals(1+jv)
                call parse_number(ierr, vals(1+jv), str(jb:))
             endif
             jv = jv + 1
             jb = je + 1
          enddo
       endif
    endif
    if (present(nitem)) then
       nitem = jv
    endif
    if (choice(.false.,unset)) ierr = min(0, ierr)
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

!!!_  & post_get() - adjust error if unset is true
  integer function post_get &
       & (jerr, unset) &
       & result(ierr)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(in)          :: jerr
    logical,intent(in),optional :: unset
    ierr = jerr
    if (choice(.false., unset)) ierr = min(0, jerr)
  end function post_get

!!!_  & default_vals - array
  subroutine default_vals_i &
       & (ierr, vals, def, nitem, unset)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)          :: ierr
    integer,intent(inout)        :: vals(:)
    integer,intent(in), optional :: def
    integer,intent(out),optional :: nitem
    logical,intent(in), optional :: unset
    ierr = err_default
    if (ierr.eq.0) then
       if (present(def)) then
          vals(:) = def
       else if (choice(.false.,unset)) then
          continue
       else
          ierr = 1
       endif
       if (present(nitem)) then
          nitem = 0
       endif
    endif
    return
  end subroutine default_vals_i

  subroutine default_vals_f &
       & (ierr, vals, def, nitem, unset)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,        intent(out)          :: ierr
    real(kind=KTGT),intent(inout)        :: vals(:)
    real(kind=KTGT),intent(in), optional :: def
    integer,        intent(out),optional :: nitem
    logical,        intent(in), optional :: unset
    ierr = err_default
    if (ierr.eq.0) then
       if (present(def)) then
          vals(:) = def
       else if (choice(.false.,unset)) then
          continue
       else
          ierr = 1
       endif
       if (present(nitem)) then
          nitem = 0
       endif
    endif
    return
  end subroutine default_vals_f

  subroutine default_vals_d &
       & (ierr, vals, def, nitem, unset)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,        intent(out)          :: ierr
    real(kind=KTGT),intent(inout)        :: vals(:)
    real(kind=KTGT),intent(in), optional :: def
    integer,        intent(out),optional :: nitem
    logical,        intent(in), optional :: unset
    ierr = err_default
    if (ierr.eq.0) then
       if (present(def)) then
          vals(:) = def
       else if (choice(.false.,unset)) then
          continue
       else
          ierr = 1
       endif
       if (present(nitem)) then
          nitem = 0
       endif
    endif
    return
  end subroutine default_vals_d

!!!_  & cmdline_count_wrap()
  integer function cmdline_count_wrap() result(n)
    implicit none
#   if HAVE_FORTRAN_COMMAND_ARGUMENT_COUNT
#     define _COMMAND_ARGUMENT_COUNT() COMMAND_ARGUMENT_COUNT()
#   elif HAVE_FORTRAN_IARGC
#     define _COMMAND_ARGUMENT_COUNT() IARGC()
#   else
#     error "neither COMMAND_ARGUMENT_COUNT nor IARGC found"
#   endif
    n = _COMMAND_ARGUMENT_COUNT()
    return
  end function cmdline_count_wrap

!!!_  & cmdline_arg_wrap
  subroutine cmdline_arg_wrap &
       & (n, v, l, s)
    implicit none
    integer,         intent(in)  :: n
    character(len=*),intent(out) :: v
    integer,optional,intent(out) :: l
    integer,optional,intent(out) :: s
#if OPT_USE_COMMAND_LINE_ARGS
#   if HAVE_FORTRAN_GET_COMMAND_ARGUMENT
    call GET_COMMAND_ARGUMENT(n, v, l, s)
#   elif HAVE_FORTRAN_GETARG
    call GETARG(n, v)
    if (present(l)) l = -1
    if (present(s)) s = -1
#   else
#     error "neither GET_COMMAND_ARGUMENT nor GETARG found"
#   endif
#else
    v = ' '
    if (present(l)) l = -1
    if (present(s)) s = -1
#endif
  end subroutine cmdline_arg_wrap

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
  if (ierr.eq.0) call parse(ierr)

  val = ' '
  ival = -999
  ivals(:)= -9999
  if (ierr.eq.0) then
     do jp = 1, 5
        val = ' '
        call get_param(ierr, val, jp)
        write(*, *) 'v:POS=', jp, ierr, '[', trim(val), ']'
     enddo
     do jp = 1, 5
        ival = -999
        call get_param(ierr, ival, jp)
        write(*, *) 'i:POS=', jp, ierr, '[', ival, ']'
     enddo
     do jp = 1, 5
        ivals(:) = -999
        call get_param(ierr, ivals(:), jp)
        write(*, *) 'ii:POS=', jp, ierr, '[', ivals(:), ']'
     enddo
     do jp = 1, 5
        ivals(:) = -999
        call get_param(ierr, ivals(:), jp, 1234)
        write(*, *) 'iid:POS=', jp, ierr, '[', ivals(:), ']'
     enddo

     tag = 'X'
     call get_option(ierr, val, tag)
     write(*, *) 'TAG=', trim(tag), ' ', ierr, '[', trim(val), ']'

     tag = 'Y'
     call get_option(ierr, val, tag)
     write(*, *) 'TAG=', trim(tag), ' ', ierr, '[', trim(val), ']'

     tag = 'X'
     ivals(:) = -999
     call get_option(ierr, ivals, tag)
     write(*, *) 'ii:TAG=', trim(tag), ' ', ierr, '[', ivals(:), ']'

     tag = 'Y'
     ivals(:) = -999
     call get_option(ierr, ivals, tag)
     write(*, *) 'ii:TAG=', trim(tag), ' ', ierr, '[', ivals(:), ']'

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
  call finalize(ierr, levv=+10)
  stop
end program test_std_arg
#endif /* TEST_STD_ARG */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

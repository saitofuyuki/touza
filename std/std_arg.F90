!!!_! std_arg.F90 - touza/std run-time argument parser
! Maintainer:  SAITO Fuyuki
! Created: May 17 2019 (for flageolet)
! Cloned: Sep 8 2020 (original: xsrc/parser.F90)
#define TIME_STAMP 'Time-stamp: <2024/02/02 09:15:33 fuyuki std_arg.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2019-2024
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
! #define OPT_USE_COMMAND_LINE_ARGS 0
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
  use TOUZA_Std_env,only: lpath
!!!_ + default
  implicit none
  private
# define __MDL__ 'arg'
# define _ERROR(E) (E - ERR_MASK_STD_ARG)
!!!_ + parameters
  integer,parameter :: ltag = ARG_TAG_LEN    ! tag length
  integer,parameter :: lval = ARG_VALUE_LEN  ! value length

  integer,parameter,public :: PARAM_DEF  = 0
  integer,parameter,public :: PARAM_POS  = 1  ! parameters as positional arguments
  integer,parameter,public :: PARAM_FILE = 2  ! parameters as external files

  character(len=*),parameter :: sp_pos  = '@'
  character(len=*),parameter :: sp_file = '+'

  integer,parameter :: nch_mdl = 4
  integer,parameter :: ngt_mdl = 128
  integer,parameter :: nap_mdl = 4
!!!_ + static
  integer,save :: init_mode = 0
  integer,save :: init_counts = 0
  integer,save :: diag_counts = 0
  integer,save :: fine_counts = 0
  integer,save :: lev_verbose = STD_MSG_LEVEL
  integer,save :: err_default = ERR_NO_INIT
  integer,save :: ulog = unit_global


  integer,save :: lrecurs = 0
  integer,save :: mflags = 0

  integer,save :: kparse_mode = PARAM_DEF

  character(len=16),save :: cassign = '='
  character(len=16),save :: cfile = '@'
  character(len=16),save :: csep = ','
  character(len=16),save :: tag_file=' '  ! deprecated

  character(len=16),save :: cstdin = '-'
  character(len=16),save :: ccomment = '#'

  character(len=16),save :: ctagend = '--'
  character(len=16),save :: cundef = ' ###'

  integer,save :: nparam = -1
!!!_ + argument entries
  type arg_chunk_t
     integer :: lev
     character(len=lpath) :: file
     integer :: prev, next ! pointer
     integer :: tbgn, tend ! key-value table
     integer :: pbgn, pend ! positional arguments
     integer :: abgn, aend ! key-value arguments
  end type arg_chunk_t

  integer,save :: mchunk = 0
  integer,save :: lchunk = 0
  type(arg_chunk_t),POINTER,SAVE :: achunk(:)

  integer,save :: mgtab = 0
  integer,save :: lgtab = 0
  character(len=ltag),POINTER,save :: atags(:)
  character(len=lval),POINTER,save :: avals(:)

  integer,save :: malias = 0
  integer,save :: lalias = 0
  character(len=ltag),POINTER,save :: ptags(:)
!!!_ + interfaces
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

  interface default_val
     module procedure default_val_a
     module procedure default_val_i, default_val_f, default_val_d
  end interface default_val

  interface default_vals
     module procedure default_vals_i, default_vals_f, default_vals_d
  end interface default_vals

  interface extract_val
     module procedure extract_val_a
     module procedure extract_val_i, extract_val_f, extract_val_d
  end interface extract_val

  interface extract_vals
     module procedure extract_vals_i, extract_vals_f, extract_vals_d
  end interface extract_vals

  ! interface get_param_seq
  !    module procedure get_param_seq_i2
  !    module procedure get_param_seq_i3
  ! end interface get_param_seq

!!!_ + public
  public :: init, diag, finalize
  public :: decl_pos_arg
  public :: parse
  public :: get_nparam, get_nargs
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
       &   lrec, cha,  chs,  chf, tagf, kmode, icomm)
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
    character(len=*),intent(in),optional :: chf        ! character for argument file
    character(len=*),intent(in),optional :: tagf       ! FILE tag (obsolete)
    integer,         intent(in),optional :: kmode
    integer,         intent(in),optional :: icomm

    ! to disable assignment or file, call with cha or chf as ' SOMETHING',
    ! which has initial blank followed by non-blank string.

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
                  &  (ierr, lrec, cha, chs, chf, tagf, kmode, ulog, lv)
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
                call diag_chunks(ierr, utmp)
                call diag_palias(ierr, utmp)
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
       if (ierr.eq.0) call free_pos_alias(ierr)
       if (ierr.eq.0) call free_atable(ierr)
       if (ierr.eq.0) call free_chunk(ierr)

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
       &   lrec, cha,  chs, chf, tagf, kmode, &
       &   u,    levv)
    use TOUZA_Std_log,only: msg_mdl
    use TOUZA_Std_utl,only: choice, choice_a
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in),optional :: lrec
    character(len=*),intent(in),optional :: cha, chs, chf
    character(len=*),intent(in),optional :: tagf
    integer,         intent(in),optional :: kmode
    integer,         intent(in),optional :: u
    integer,         intent(in),optional :: levv

    ierr = 0

    call choice_a(csep, chs)
    if (csep.eq.' ') csep = ','
    call choice_a(cassign, cha)
    if (cassign.eq.' ') cassign = '='
    call choice_a(cfile, chf)
    if (cfile.eq.' ') cfile = '@'

    lrecurs = choice(0, lrec)
    if (lrecurs.le.0) lrecurs = 5
    call choice_a(tag_file, tagf)
    if (tag_file.ne.' ') then
       call msg_mdl('Deprecated usage of file-argument', __MDL__)
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    endif

    kparse_mode = choice(PARAM_DEF, kmode)
    if (kparse_mode.eq.PARAM_DEF) kparse_mode = PARAM_POS
    ! call collect_entries(ierr, lrec)

    return
  end subroutine init_batch
!!!_ + diag subcontracts
!!!_  & diag_chunks
  subroutine diag_chunks &
       & (ierr, u, tag)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in),optional :: u
    character(len=*),intent(in),optional :: tag
    integer utmp

    utmp = choice(ulog, u)
    call report_chunks &
         & (ierr, achunk, mchunk, lchunk, atags, avals, utmp, tag)
  end subroutine diag_chunks

!!!_  & diag_palias
  subroutine diag_palias &
       & (ierr, u, tag)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in),optional :: u
    character(len=*),intent(in),optional :: tag
    integer utmp

    utmp = choice(ulog, u)
    call report_palias &
         & (ierr, ptags, malias, lalias, utmp, tag)
  end subroutine diag_palias

!!!_ + argument chunk manager
!!!_  & alloc_pos_alias
  subroutine alloc_pos_alias(ierr, nadd)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: nadd

    character(len=ltag),POINTER :: tt(:)

    integer l

    ierr = 0

    l = malias + nadd
    if (l.le.lalias) return

    if (ierr.eq.0) allocate(tt(0:l-1), STAT=ierr)
    if (ierr.eq.0) then
       if (lalias.gt.0) then
          tt(0:lalias-1) = ptags(0:lalias-1)
          deallocate(ptags, STAT=ierr)
       endif
    endif
    if (ierr.eq.0) then
       tt(lalias:l-1) = ' '

       ptags => tt

       lalias = l
    else
       ptags => NULL()
    endif

  end subroutine alloc_pos_alias

!!!_  & alloc_atable
  subroutine alloc_atable(ierr, nadd)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: nadd

    character(len=ltag),POINTER :: tt(:)
    character(len=lval),POINTER :: vv(:)

    integer l

    ierr = 0

    l = mgtab + nadd
    if (l.le.lgtab) return

    if (ierr.eq.0) allocate(tt(0:l-1), vv(0:l-1), STAT=ierr)
    if (ierr.eq.0) then
       if (lgtab.gt.0) then
          tt(0:lgtab-1) = atags(0:lgtab-1)
          vv(0:lgtab-1) = avals(0:lgtab-1)
          deallocate(atags, avals, STAT=ierr)
       endif
    endif
    if (ierr.eq.0) then
       tt(lgtab:l-1) = ' '
       vv(lgtab:l-1) = ' '

       atags => tt
       avals => vv

       lgtab = l
    else
       atags => NULL()
       avals => NULL()
    endif
  end subroutine alloc_atable

!!!_  & alloc_chunk
  subroutine alloc_chunk(ierr, nadd)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: nadd

    type(arg_chunk_t),POINTER :: tmp(:)
    integer l

    ierr = 0

    l = mchunk + nadd
    if (l.le.lchunk) return

    allocate(tmp(0:l-1), STAT=ierr)
    if (ierr.eq.0) then
       if (lchunk.gt.0) then
          tmp(0:lchunk-1) = achunk(0:lchunk-1)
          deallocate(achunk, STAT=ierr)
       endif
    endif
    if (ierr.eq.0) then
       tmp(lchunk:l-1)%lev  = -1
       tmp(lchunk:l-1)%tbgn = -1
       tmp(lchunk:l-1)%tend = -1
       tmp(lchunk:l-1)%pbgn = -1
       tmp(lchunk:l-1)%pend = -1
       tmp(lchunk:l-1)%abgn = -1
       tmp(lchunk:l-1)%aend = -1
       tmp(lchunk:l-1)%prev = -1
       tmp(lchunk:l-1)%next = -1

       achunk => tmp
       lchunk = l
    else
       achunk => NULL()
    endif
  end subroutine alloc_chunk

!!!_  & free_pos_alias
  subroutine free_pos_alias(ierr)
    implicit none
    integer,intent(out) :: ierr
    ierr = 0
    if (associated(ptags)) then
       deallocate(ptags, STAT=ierr)
    endif
    if (ierr.eq.0) then
       ptags => NULL()
       lalias = -1
       malias = -1
    endif
  end subroutine free_pos_alias

!!!_  & free_atable
  subroutine free_atable(ierr)
    implicit none
    integer,intent(out) :: ierr

    ierr = 0
    if (ierr.eq.0) then
       if (associated(atags)) then
          deallocate(atags, STAT=ierr)
       endif
    endif
    if (ierr.eq.0) then
       if (associated(avals)) then
          deallocate(avals, STAT=ierr)
       endif
    endif
    if (ierr.eq.0) then
       atags => NULL()
       avals => NULL()
       lgtab = -1
       mgtab = -1
    endif
  end subroutine free_atable

!!!_  - free_chunk
  subroutine free_chunk(ierr)
    implicit none
    integer,intent(out) :: ierr

    ierr = 0
    if (ierr.eq.0) then
       if (associated(achunk)) then
          deallocate(achunk, STAT=ierr)
       endif
    endif
    if (ierr.eq.0) then
       achunk => NULL()
       mchunk = -1
       lchunk = -1
    endif
  end subroutine free_chunk

!!!_  & new_chunk
  subroutine new_chunk(ierr, jpos, jref, file, jorg)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: jpos
    integer,intent(in)          :: jref   ! previous chunk
    character(len=*),intent(in) :: file
    integer,intent(in),optional :: jorg   ! search origin

    ierr = err_default
    if (ierr.eq.0) then
       jpos = choice(mchunk, jorg)
       if (jpos.ge.lchunk) then
          call alloc_chunk(ierr, nch_mdl)
          if (ierr.eq.0) jpos = mchunk
       endif
    endif
    if (ierr.eq.0) then
       achunk(jpos)%tbgn = mgtab
       achunk(jpos)%pbgn = 0
       achunk(jpos)%tend = -1
       achunk(jpos)%file = file

       achunk(jpos)%prev = jref
       if (jref.ge.0) achunk(jref)%next = jpos

       mchunk = mchunk + 1
    endif
  end subroutine new_chunk

!!!_ + parsers
!!!_  & decl_pos_arg
  subroutine decl_pos_arg &
       & (ierr, tag, jpos, dup)
    use TOUZA_Std_utl,only: choice
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(in),optional :: tag
    integer,         intent(in),optional :: jpos  ! start from 1
    logical,         intent(in),optional :: dup   ! allow duplication (default false)

    integer jpi
    logical bdup
    integer,parameter :: ltxt = 256
    character(len=ltxt) :: txt

    ierr = err_default
    bdup = choice(.FALSE., dup)
    if (bdup) then
       ierr = _ERROR(ERR_NOT_IMPLEMENTED)
    endif

    ! if (nregs.ge.0) then
    !    ! error if parsed already
    !    ierr = _ERROR(ERR_SECOND_INVOCATION)
    !    return
    ! endif

    ! jpi count from 1
    jpi = choice(0, jpos)
    if (jpi.le.0) jpi = malias + 1

    jpi = jpi - 1
    if (ierr.eq.0) call alloc_pos_alias(ierr, jpi + 1 - malias)
    if (ierr.eq.0) then
       if (present(tag)) then
          if (tag.eq.' ' .or. bdup) then
             ptags(jpi) = trim(tag)
          else if (ANY(ptags(0:malias-1).eq.tag)) then
109          format('Duplicated alias tag [', I0, '] ', A)
             write(txt, 109) 1 + jpi, trim(tag)
             call msg_mdl(txt, __MDL__)
             ierr = _ERROR(ERR_INVALID_ITEM)
          else
             ptags(jpi) = trim(tag)
          endif
       else
          ptags(jpi) = ' '
       endif
    endif
    if (ierr.eq.0) then
       malias = max(jpi + 1, malias)
    endif

    return
  end subroutine decl_pos_arg

!!!_  & parse - batch parser
  subroutine parse (ierr, levv)
    use TOUZA_Std_log,only: msg_mdl
    use TOUZA_Std_fun,only: new_unit
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: levv
    integer lu
    integer lv

    lv = choice(lev_verbose, levv)
    ierr = err_default
    if (nparam.ge.0) then
       lu = get_logu(ulog)
       call msg_mdl('parse twice.', __MDL__, lu)
       return
    endif

    if (ierr.eq.0) call alloc_chunk(ierr, nch_mdl)
    if (ierr.eq.0) call alloc_atable(ierr, ngt_mdl)
    if (ierr.eq.0) call parse_chunk_command(ierr)
    if (ierr.eq.0) call parse_chunk_files(ierr)
    if (ierr.eq.0) call post_parse_chunk(ierr)

    return
  end subroutine parse

!!!_   & parse_chunk_command
  subroutine parse_chunk_command &
       & (ierr, jchorg)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: jchorg

    integer jch
    integer prev
    integer,parameter :: lstr=ARG_LINE_LEN
    character(len=lstr) :: S
#if OPT_USE_COMMAND_LINE_ARGS
    integer jarg, nargs
    integer l

#   if HAVE_FORTRAN_COMMAND_ARGUMENT_COUNT
#     define _COMMAND_ARGUMENT_COUNT() COMMAND_ARGUMENT_COUNT()
#   elif HAVE_FORTRAN_IARGC
#     define _COMMAND_ARGUMENT_COUNT() IARGC()
#   else
#     error "neither COMMAND_ARGUMENT_COUNT nor IARGC found"
#   endif

#endif /* OPT_USE_COMMAND_LINE_ARGS */

    ierr = err_default
    prev = -1
    call new_chunk(ierr, jch, prev, ' ', jchorg)
    if (ierr.eq.0) achunk(jch)%lev = 0
    if (ierr.eq.0) then
#if OPT_USE_COMMAND_LINE_ARGS
       jarg  = 0
       nargs = _COMMAND_ARGUMENT_COUNT()
       do
          if (ierr.ne.0) exit
          jarg = jarg + 1
          if (jarg.gt.nargs) exit
          call cmdline_arg_wrap(jarg, S, l, ierr)
          ! write(*, *) 'arg:', jarg, S(1:l)
          if (l.gt.lstr) then
101          format('too long argument at ', I0, ':', A)
             write(*, 101) jarg, trim(S)
             ierr = _ERROR(ERR_INSUFFICIENT_BUFFER)
          else if (ierr.eq.0) then
             call store_aitem(ierr, jch, S, cassign, cfile, tag_file)
          endif
       enddo
       ! settle current chunk
       if (ierr.eq.0) then
          achunk(jch)%tend = mgtab
       endif
#else  /* not OPT_USE_COMMAND_LINE_ARGS */
       S = trim(cfile) // trim(cstdin)
       call store_aitem(ierr, jch, S, cassign, cfile, tag_file)
#endif /* not OPT_USE_COMMAND_LINE_ARGS */
    endif

  end subroutine parse_chunk_command

!!!_   & parse_chunk_files
  subroutine parse_chunk_files &
       & (ierr, jchorg, sep, levv)
    use TOUZA_Std_fun,only: new_unit
    use TOUZA_Std_utl,only: choice, choice_a, begin_with
    use TOUZA_Std_env,only: uin, is_eof_ss, is_new_line
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in),optional :: jchorg
    character(len=*),intent(in),optional :: sep   ! separator (default white space)
    integer,         intent(in),optional :: levv

    integer jch
    integer jb, je, ls, la
    integer ucfg, ucur
    integer,parameter :: ltxt = 1024
    character(len=ltxt) :: txt
    integer,parameter :: lsep = 8
    character(len=lsep) :: xsep
    integer lv

    ierr = err_default
    lv = choice(lev_verbose, levv)

    jch = choice(0, jchorg)
    call choice_a(xsep, ' ', sep)
    ls = max(1, len_trim(xsep))
    if (is_new_line(xsep(1:1))) ls = 0

    ucfg = -999

    if (ierr.eq.0) then
       ucfg = new_unit()
       ierr = min(0, ucfg)
    endif

    do
       if (jch.ge.mchunk) exit
       if (jch.lt.0) ierr = _ERROR(ERR_PANIC)
       if (ierr.ne.0) exit

       if (achunk(jch)%tbgn.lt.0) then
          if (ierr.eq.0) then
             achunk(jch)%tbgn = mgtab
             if (achunk(jch)%file .eq. cstdin) then
                ucur = uin
                if (VCHECK_DETAIL(lv)) then
                   call msg_mdl('Read arguments from stdin.', __MDL__)
                endif
             else if (achunk(jch)%file .ne. ' ') then
                ucur = ucfg
                open(unit=ucur, FILE=achunk(jch)%file, IOSTAT=ierr, &
                     & FORM='FORMATTED', STATUS='OLD', ACTION='READ')
                if (ierr.ne.0) then
109                format(I0, ': cannot open ', A)
                   write(txt, 109) ierr, trim(achunk(jch)%file)
                   call msg_mdl(txt, __MDL__)
                   ierr = min(-1, ierr)
                   exit
                endif
             else
                call msg_mdl('blank file', __MDL__)
                ierr = _ERROR(ERR_INVALID_PARAMETER)
             endif
          endif
          do
             if (ierr.ne.0) exit
             read(ucur, '(A)', IOSTAT=ierr) txt
             if (is_eof_ss(ierr)) then
                ierr = 0
                exit
             endif
             if (txt.eq.' ') cycle
             if (begin_with(txt, ccomment)) cycle
             if (ls.gt.0) then
                la = len_trim(txt)
                jb = 0
                do
                   if (jb.ge.la) exit
                   je = index(txt(jb+1:la), xsep(1:ls))
                   if (je.le.0) then
                      je = la
                   else
                      je = jb + je - 1
                   endif
                   call store_aitem(ierr, jch, txt(jb+1:je), cassign, cfile, tag_file)
                   jb = je + ls
                enddo
             else
                call store_aitem(ierr, jch, txt, cassign, cfile, tag_file)
             endif
          enddo
          if (ierr.eq.0) then
             if (ucur.eq.ucfg) close(ucur, IOSTAT=ierr)
          endif
       endif
       jch = jch + 1
    enddo

  end subroutine parse_chunk_files

!!!_   . post_parse_chunk
  subroutine post_parse_chunk(ierr)
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,intent(out) :: ierr

    integer jch, jarg, jpos
    integer narg

    integer jt, jf
    integer,allocatable :: kpflag(:)

    ierr = err_default

    if (ierr.eq.0) then
       allocate(kpflag(0:malias-1), STAT=ierr)
    endif
    if (ierr.eq.0) kpflag(:) = -1

    jch = 0
    jarg = 0
    jpos = 0
    if (ierr.eq.0) then
       do
          if (jch.lt.0) exit
          if (jch.ge.mchunk) then
             call msg_mdl('invalid chunk.', __MDL__)
             ierr = _ERROR(ERR_PANIC)
             exit
          endif
          narg = max(0, achunk(jch)%tend - achunk(jch)%tbgn)
          achunk(jch)%tend = achunk(jch)%tbgn + narg
          achunk(jch)%abgn = jarg
          achunk(jch)%aend = jarg + narg

          ! search alias
          do jt = achunk(jch)%tbgn, achunk(jch)%tend - 1
             if (atags(jt).ne.' ') then
                do jf = 0, malias - 1
                   if (atags(jt).eq.ptags(jf) &
                        & .and. kpflag(jf).lt.0) then
                      kpflag(jf) = jt
                      exit
                   endif
                enddo
             endif
          enddo

          jarg = jarg + narg
          jch = achunk(jch)%next
       enddo
    endif
    ! alias detection
    if (ierr.eq.0) then
       jch = 0
       jpos = 0
       jf = 0
       loop_full: do
          if (jch.lt.0) exit
          loop_chunk: do jt = achunk(jch)%tbgn, achunk(jch)%tend - 1
             if (jf.ge.malias) exit loop_chunk
             if (atags(jt).eq.' ') then
                do
                   if (jf.ge.malias) exit loop_chunk
                   if (kpflag(jf).lt.0) then
                      atags(jt) = ptags(jf)
                      if (ptags(jf).ne.' ') achunk(jch)%pbgn = achunk(jch)%pbgn - 1
                      jf = jf + 1
                      exit
                   endif
                   jf = jf + 1
                enddo
             endif
          enddo loop_chunk
          achunk(jch)%pend = jpos + achunk(jch)%pbgn
          achunk(jch)%pbgn = jpos
          jpos = achunk(jch)%pend
          jch = achunk(jch)%next
       enddo loop_full
    endif
    if (ierr.eq.0) nparam = jpos

    if (ierr.eq.0) deallocate(kpflag, STAT=ierr)
  end subroutine post_parse_chunk

!!!_   & store_aitem
  subroutine store_aitem(ierr, jchunk, str, cha, pfxf, tagf)
    use TOUZA_Std_utl,only: begin_with
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: jchunk
    character(len=*),intent(in)    :: str
    character(len=*),intent(in)    :: cha  ! assign operator
    character(len=*),intent(in)    :: pfxf ! file prefix
    character(len=*),intent(in)    :: tagf ! file tag (DEPRECATED, disabled by caller)
    integer jp, ls

    ierr = 0

    ! if file item
    !    reserve new chunk as file
    !    settle current chunk
    !    yet new chunk to continue
    ! else
    !    append tag value pair

    if (begin_with(str, pfxf)) then
       jp = 1 + len_trim(pfxf)
       call append_chunk(ierr, jchunk, str, jp)
    else
       ls = len_trim(cha)
       jp = index(str, cha(1:ls))
       if (jp.gt.1) then
          if (str(1:jp-1).eq.trim(tagf)) then
             jp = jp + ls
             call append_chunk(ierr, jchunk, str, jp)
          else if (is_valid_tag(str(1:jp-1))) then
             call append_aitem(ierr, jchunk, str(1:jp-1), str(jp+ls:))
          else
             call append_aitem(ierr, jchunk, ' ', str)
          endif
       else
          call append_aitem(ierr, jchunk, ' ', str)
       endif
    endif

  end subroutine store_aitem

!!!_   & is_valid_tag()
  logical function is_valid_tag (str) result(b)
    implicit none
    character(len=*),intent(in) :: str
    integer lstr, j
    integer c
    integer,parameter :: cLA = IACHAR('A')
    integer,parameter :: cLZ = IACHAR('Z')
    integer,parameter :: cSa = IACHAR('a')
    integer,parameter :: cSz = IACHAR('z')
    integer,parameter :: cN0 = IACHAR('0')
    integer,parameter :: cN9 = IACHAR('9')
    integer,parameter :: cUB = IACHAR('_')

    ! TAG = {alpha,_}{alnum,_}*

    lstr = len_trim(str)
    b = lstr.gt.0

    if (b) then
       c = IACHAR(str(1:1))
       b = (cLA.le.c .and. c.le.cLZ) &
            & .or. (cSa.le.c .and. c.le.cSz) &
            & .or. (c .eq. cUB)
    endif
    if (b) then
       do j = 2, lstr
          c = IACHAR(str(j:j))
          b = (cLA.le.c .and. c.le.cLZ) &
               & .or. (cSa.le.c .and. c.le.cSz) &
               & .or. (cN0.le.c .and. c.le.cN9) &
               & .or. (c .eq. cUB)
          if (.not.b) exit
       enddo
    endif
  end function is_valid_tag

!!!_   & append_chunk
  subroutine append_chunk(ierr, jchunk, arg, posf)
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: jchunk
    character(len=*),intent(in)    :: arg
    integer,         intent(in)    :: posf

    integer :: j
    integer :: jcf, jcx
    integer :: nadd
    character(len=lpath) :: txt

    ierr = err_default
    nadd = 2
    if (ierr.eq.0) then
       j = achunk(jchunk)%prev
       do
          if (j.lt.0) exit
          if (achunk(j)%file .eq. arg(posf:)) then
109          format('Recursion detected: ', A)
             write(txt, 109) trim(arg(posf:))
             call msg_mdl(txt, __MDL__)
             call diag_chunks(ierr)
             ierr = _ERROR(ERR_INVALID_ITEM)
             exit
          endif
          j = achunk(j)%prev
       enddo
    endif
    if (ierr.eq.0) call alloc_chunk(ierr, nadd)
    if (ierr.eq.0) then
       jcx = mchunk
       jcf = mchunk + 1

       ! initialize file chunk
       achunk(jcf)%lev  = achunk(jchunk)%lev + 1
       achunk(jcf)%prev = jchunk
       achunk(jcf)%next = jcx
       achunk(jcf)%file = arg(posf:)
       achunk(jcf)%pbgn = 0

       ! initialize continuation chunk
       achunk(jcx)%lev  = achunk(jchunk)%lev
       achunk(jcx)%prev = jcf
       achunk(jcx)%next = achunk(jchunk)%next
       achunk(jcx)%file = achunk(jchunk)%file
       achunk(jcx)%tbgn = mgtab
       achunk(jcx)%pbgn = 0

       ! settle current chunk
       achunk(jchunk)%tend = mgtab
       achunk(jchunk)%next = jcf

       mchunk = mchunk + nadd
       jchunk = jcx
    endif

  end subroutine append_chunk

!!!_   & append_aitem
  subroutine append_aitem(ierr, jchunk, tag, val)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: jchunk
    character(len=*),intent(in)  :: tag
    character(len=*),intent(in)  :: val

    ierr = err_default

    if (ierr.eq.0) call alloc_atable(ierr, 1)
    if (ierr.eq.0) then
       atags(mgtab) = tag
       avals(mgtab) = val
       mgtab = mgtab + 1
       achunk(jchunk)%tend = mgtab
       if (tag.eq.' ') achunk(jchunk)%pbgn = achunk(jchunk)%pbgn + 1
    endif

  end subroutine append_aitem

!!!_ + inquiries
!!!_  & get_nparam() - get number of positional parameters
  integer function get_nparam () result (n)
    implicit none
    integer jerr
    jerr = err_default
    if (jerr.eq.0) then
       n = nparam
    else
       n = -1
    endif
  end function get_nparam

!!!_  & get_nargs() - get number of arguments
  integer function get_nargs () result (n)
    implicit none
    integer jerr
    jerr = err_default
    if (jerr.eq.0) then
       n = mgtab
    else
       n = -1
    endif
  end function get_nargs

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
    integer jidx

    ierr = err_default
    jidx = pos_search(jpos, achunk, mchunk, atags)
    if (jidx.lt.0) then
       call default_val(ierr, val, def, unset=unset)
    else
       call extract_val(ierr, val, avals(jidx), cundef, def, unset=unset)
    endif
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
    integer jidx

    ierr = err_default
    jidx = pos_search(jpos, achunk, mchunk, atags)
    if (jidx.lt.0) then
       call default_val(ierr, val, def, unset=unset)
    else
       call extract_val(ierr, val, avals(jidx), cundef, def, unset=unset)
    endif
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
    integer jidx

    ierr = err_default
    jidx = pos_search(jpos, achunk, mchunk, atags)
    if (jidx.lt.0) then
       call default_val(ierr, val, def, unset=unset)
    else
       call extract_val(ierr, val, avals(jidx), cundef, def, unset=unset)
    endif
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
    integer jidx

    ierr = err_default
    jidx = pos_search(jpos, achunk, mchunk, atags)
    if (jidx.lt.0) then
       call default_val(ierr, val, def, unset=unset)
    else
       call extract_val(ierr, val, avals(jidx), cundef, def, unset=unset)
    endif
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
    integer jidx

    ierr = err_default
    jidx = pos_search(jpos, achunk, mchunk, atags)
    if (jidx.lt.0) then
       call default_vals(ierr, vals(:), def, unset=unset)
    else
       call extract_vals(ierr, vals(:), avals(jidx), cundef, def, sep, unset=unset)
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
    integer jidx

    ierr = err_default
    jidx = pos_search(jpos, achunk, mchunk, atags)
    if (jidx.lt.0) then
       call default_vals(ierr, vals(:), def, unset=unset)
    else
       call extract_vals(ierr, vals(:), avals(jidx), cundef, def, sep, unset=unset)
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
    integer jidx

    ierr = err_default
    jidx = pos_search(jpos, achunk, mchunk, atags)
    if (jidx.lt.0) then
       call default_vals(ierr, vals(:), def, unset=unset)
    else
       call extract_vals(ierr, vals(:), avals(jidx), cundef, def, sep, unset=unset)
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
    integer jidx

    ierr = err_default
    jidx = pos_search(jpos, achunk, mchunk, atags)
    if (jidx.lt.0) then
       call default_vals(ierr, vals(:), def, nitem, unset=unset)
    else
       call extract_vals(ierr, vals(:), avals(jidx), cundef, def, sep, nitem, unset=unset)
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
    integer jidx

    ierr = err_default
    jidx = pos_search(jpos, achunk, mchunk, atags)
    if (jidx.lt.0) then
       call default_vals(ierr, vals(:), def, nitem, unset=unset)
    else
       call extract_vals(ierr, vals(:), avals(jidx), cundef, def, sep, nitem, unset=unset)
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
    integer jidx

    ierr = err_default
    jidx = pos_search(jpos, achunk, mchunk, atags)
    if (jidx.lt.0) then
       call default_vals(ierr, vals(:), def, nitem, unset=unset)
    else
       call extract_vals(ierr, vals(:), avals(jidx), cundef, def, sep, nitem, unset=unset)
    endif
    return
  end subroutine get_array_d

!!!_  & get_option - get option (key/value argument)
  subroutine get_option_a &
       & (ierr, val, tag, def, ref, unset)
    use TOUZA_Std_utl,only: set_if_present
    implicit none
    integer,         intent(out)            :: ierr
    character(len=*),intent(inout)          :: val
    character(len=*),intent(in)             :: tag
    character(len=*),intent(in),optional    :: def
    integer,         intent(inout),optional :: ref    ! reference index to search from
    logical,         intent(in),optional    :: unset
    integer jidx

    ierr = err_default
    jidx = tag_search(tag, achunk, mchunk, atags, ref)
    if (jidx.lt.0) then
       call default_val(ierr, val, def, unset=unset)
    else
       call extract_val(ierr, val, avals(jidx), cundef, def, unset=unset)
    endif
    call set_if_present(ref, jidx)
    return
  end subroutine get_option_a

  subroutine get_option_i &
       & (ierr, val, tag, def, ref, unset)
    use TOUZA_Std_utl,only: set_if_present
    implicit none
    integer,         intent(out)            :: ierr
    integer,         intent(inout)          :: val
    character(len=*),intent(in)             :: tag
    integer,         intent(in),optional    :: def
    integer,         intent(inout),optional :: ref
    logical,         intent(in),optional    :: unset
    integer jidx

    ierr = err_default
    jidx = tag_search(tag, achunk, mchunk, atags, ref)
    if (jidx.lt.0) then
       call default_val(ierr, val, def, unset=unset)
    else
       call extract_val(ierr, val, avals(jidx), cundef, def, unset=unset)
    endif
    call set_if_present(ref, jidx)
    return
  end subroutine get_option_i

  subroutine get_option_f &
       & (ierr, val, tag, def, ref, unset)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    use TOUZA_Std_utl,only: set_if_present
    implicit none
    integer,         intent(out)            :: ierr
    real(kind=KTGT), intent(inout)          :: val
    character(len=*),intent(in)             :: tag
    real(kind=KTGT), intent(in),optional    :: def
    integer,         intent(inout),optional :: ref
    logical,         intent(in),optional    :: unset
    integer jidx

    ierr = err_default
    jidx = tag_search(tag, achunk, mchunk, atags, ref)
    if (jidx.lt.0) then
       call default_val(ierr, val, def, unset=unset)
    else
       call extract_val(ierr, val, avals(jidx), cundef, def, unset=unset)
    endif
    call set_if_present(ref, jidx)
    return
  end subroutine get_option_f

  subroutine get_option_d &
       & (ierr, val, tag, def, ref, unset)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    use TOUZA_Std_utl,only: set_if_present
    implicit none
    integer,         intent(out)            :: ierr
    real(kind=KTGT), intent(inout)          :: val
    character(len=*),intent(in)             :: tag
    real(kind=KTGT), intent(in),optional    :: def
    integer,         intent(inout),optional :: ref
    logical,         intent(in),optional    :: unset
    integer jidx

    ierr = err_default
    jidx = tag_search(tag, achunk, mchunk, atags, ref)
    if (jidx.lt.0) then
       call default_val(ierr, val, def, unset=unset)
    else
       call extract_val(ierr, val, avals(jidx), cundef, def, unset=unset)
    endif
    call set_if_present(ref, jidx)
    return
  end subroutine get_option_d

  subroutine get_option_ia &
       & (ierr, vals, tag, def, ref, sep, unset)
    use TOUZA_Std_utl,only: set_if_present
    implicit none
    integer,         intent(out)            :: ierr
    integer,         intent(inout)          :: vals(:)
    character(len=*),intent(in)             :: tag
    integer,         intent(in),optional    :: def
    integer,         intent(inout),optional :: ref
    character(len=*),intent(in),optional    :: sep
    logical,         intent(in),optional    :: unset
    integer jidx

    ierr = err_default
    jidx = tag_search(tag, achunk, mchunk, atags, ref)
    if (jidx.lt.0) then
       call default_vals(ierr, vals(:), def, unset=unset)
    else
       call extract_vals(ierr, vals(:), avals(jidx), cundef, def, sep, unset=unset)
    endif
    call set_if_present(ref, jidx)
    return
  end subroutine get_option_ia

  subroutine get_option_fa &
       & (ierr, vals, tag, def, ref, sep, unset)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    use TOUZA_Std_utl,only: set_if_present
    implicit none
    integer,         intent(out)            :: ierr
    real(kind=KTGT), intent(inout)          :: vals(:)
    character(len=*),intent(in)             :: tag
    real(kind=KTGT), intent(in),optional    :: def
    integer,         intent(inout),optional :: ref
    character(len=*),intent(in),optional    :: sep
    logical,         intent(in),optional    :: unset
    integer jidx

    ierr = err_default
    jidx = tag_search(tag, achunk, mchunk, atags, ref)
    if (jidx.lt.0) then
       call default_vals(ierr, vals(:), def, unset=unset)
    else
       call extract_vals(ierr, vals(:), avals(jidx), cundef, def, sep, unset=unset)
    endif
    call set_if_present(ref, jidx)
    return
  end subroutine get_option_fa

  subroutine get_option_da &
       & (ierr, vals, tag, def, ref, sep, unset)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    use TOUZA_Std_utl,only: set_if_present
    implicit none
    integer,         intent(out)            :: ierr
    real(kind=KTGT), intent(inout)          :: vals(:)
    character(len=*),intent(in)             :: tag
    real(kind=KTGT), intent(in),optional    :: def
    integer,         intent(inout),optional :: ref
    character(len=*),intent(in),optional    :: sep
    logical,         intent(in),optional    :: unset
    integer jidx

    ierr = err_default
    jidx = tag_search(tag, achunk, mchunk, atags, ref)
    if (jidx.lt.0) then
       call default_vals(ierr, vals(:), def, unset=unset)
    else
       call extract_vals(ierr, vals(:), avals(jidx), cundef, def, sep, unset=unset)
    endif
    call set_if_present(ref, jidx)
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
       call extract_vals(ierr, vals(:), str, cundef, def, sep, nitem=nitem, unset=unset)
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
       call extract_vals(ierr, vals(:), str, cundef, def, sep, nitem=nitem, unset=unset)
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
       call extract_vals(ierr, vals(:), str, cundef, def, sep, nitem=nitem, unset=unset)
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

!!!_  & get_arg - get key/value at given entry (logical index of all the arguments)
  subroutine get_arg_a &
       & (ierr, tag, val, jentr)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: tag
    character(len=*),intent(out) :: val
    integer,         intent(in)  :: jentr
    integer jidx

    ierr = err_default

    if (ierr.eq.0) then
       jidx = arg_search_entry(jentr, achunk, mchunk, mgtab)
       if (jidx.lt.0) then
          tag = ' '
          val = ' '
          ierr = _ERROR(ERR_OUT_OF_RANGE)
          return
       endif

       tag = trim(ADJUSTL(atags(jidx)))
       val = trim(ADJUSTL(avals(jidx)))
    endif
    return
  end subroutine get_arg_a

!!!_  & get_key - get key at given entry
  subroutine get_key_a &
       & (ierr, tag, jentr)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: tag
    integer,         intent(in)  :: jentr
    integer jidx

    ierr = err_default

    if (ierr.eq.0) then
       jidx = arg_search_entry(jentr, achunk, mchunk, mgtab)
       if (jidx.lt.0) then
          tag = ' '
          ierr = _ERROR(ERR_OUT_OF_RANGE)
          return
       endif

       tag = trim(ADJUSTL(atags(jidx)))
    endif
    return
  end subroutine get_key_a

!!!_  & get_value - get value at given entry
  subroutine get_value_a &
       & (ierr, val, jentr)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: val
    integer,         intent(in)  :: jentr
    integer jidx

    ierr = err_default

    if (ierr.eq.0) then
       jidx = arg_search_entry(jentr, achunk, mchunk, mgtab)
       if (jidx.lt.0) then
          val = ' '
          ierr = _ERROR(ERR_OUT_OF_RANGE)
          return
       endif

       val = trim(ADJUSTL(avals(jidx)))
    endif
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
!!!_  - report_palias
  subroutine report_palias &
       & (ierr, &
       &  tt,  mt,  lt, ulog, tag)
    use TOUZA_Std_utl,only: choice, choice_a
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,          intent(out)         :: ierr
    character(len=*), intent(in)          :: tt(0:*)
    integer,          intent(in)          :: mt, lt
    integer,          intent(in),optional :: ulog
    character(len=*), intent(in),optional :: tag

    character(len=64) :: pfx
    character(len=256) :: txt
    integer j

    ierr = 0
    call choice_a(pfx, ' ', tag)
101 format('ptags = ', I0, ' / ', I0)
102 format(A, ':ptags = ', I0, ' / ', I0)
    if (pfx.eq.' ') then
       write(txt, 101) mt, lt
    else
       write(txt, 102) trim(pfx), mt, lt
    endif
    call msg_mdl(txt, __MDL__, ulog)

    do j = 0, min(mt, lt) - 1
111    format('pos[', I0, '] ', A)
112    format(A, ':pos[', I0, '] ', A)
       if (pfx.eq.' ') then
          write(txt, 111) j+1, trim(tt(j))
       else
          write(txt, 112) trim(pfx), j+1, trim(tt(j))
       endif
       call msg_mdl(txt, __MDL__, ulog)
    enddo
  end subroutine report_palias

!!!_  - report_chunks
  subroutine report_chunks &
       & (ierr, &
       &  ach,  mch,  lch, gt, gv, ulog, tag)
    use TOUZA_Std_utl,only: choice, choice_a
    use TOUZA_Std_log,only: msg_mdl
    implicit none
    integer,          intent(out)         :: ierr
    type(arg_chunk_t),intent(in)          :: ach(0:)
    integer,          intent(in)          :: mch, lch
    character(len=*), intent(in)          :: gt(0:*), gv(0:*)
    integer,          intent(in),optional :: ulog
    character(len=*), intent(in),optional :: tag

    character(len=64) :: pfx
    character(len=256) :: txt
    integer j, jj, jf

    ierr = 0
    call choice_a(pfx, ' ', tag)
101 format('chunks = ', I0, ' / ', I0)
102 format(A, ':chunks = ', I0, ' / ', I0)
    if (pfx.eq.' ') then
       write(txt, 101) mch, lch
    else
       write(txt, 102) trim(pfx), mch, lch
    endif
    call msg_mdl(txt, __MDL__, ulog)

    ! do j = 0, lch - 1
    !    write(*, *) j, ach(j)%lev, ach(j)%prev, ach(j)%next, trim(ach(j)%file)
    ! enddo

    j = 0
    do
       if (j.lt.0) exit
       if (j.ge.mch) then
          call msg_mdl('invalid chunk.', __MDL__)
          ierr = _ERROR(ERR_PANIC)
          exit
       endif
111    format('[', I0, '] ', I0, ':', A)
112    format(A, ':[', I0, '] ', I0, ':', A)
131    format('[', I0, '] ', I0, 1x, I0, ' / ', I0, 1x, I0)
132    format(A, ':[', I0, '] ', I0, 1x, I0, ' / ', I0, 1x, I0)
121    format('  (', I0, ') ', '[', A, '] ', A)
122    format(A, '  (', I0, ') ', '[', A, ']  ', A)
141    format('  (', I0, ') ', A)
142    format(A, '  (', I0, ') ', A)
       if (pfx.eq.' ') then
          write(txt, 111) j, ach(j)%lev, trim(ach(j)%file)
          call msg_mdl(txt, __MDL__, ulog)
          write(txt, 131) j, ach(j)%pbgn, ach(j)%pend, ach(j)%abgn, ach(j)%aend
          call msg_mdl(txt, __MDL__, ulog)
          do jj = ach(j)%tbgn, ach(j)%tend - 1
             jf = jj - ach(j)%tbgn + ach(j)%abgn + 1
             if (gt(jj).ne.' ') then
                write(txt, 121) jf, trim(gt(jj)), trim(gv(jj))
             else
                write(txt, 141) jf, trim(gv(jj))
             endif
             call msg_mdl(txt, __MDL__, ulog)
          enddo
       else
          write(txt, 112) trim(pfx), j, ach(j)%lev, trim(ach(j)%file)
          call msg_mdl(txt, __MDL__, ulog)
          write(txt, 132) trim(pfx), j, ach(j)%pbgn, ach(j)%pend, ach(j)%pbgn, ach(j)%aend
          call msg_mdl(txt, __MDL__, ulog)
          do jj = ach(j)%tbgn, ach(j)%tend - 1
             jf = jj - ach(j)%tbgn + ach(j)%abgn + 1
             if (gt(jj).ne.' ') then
                write(txt, 122) trim(pfx), jf, trim(gt(jj)), trim(gv(jj))
             else
                write(txt, 142) trim(pfx), jf, trim(gv(jj))
             endif
             call msg_mdl(txt, __MDL__, ulog)
          enddo
       endif
       j = ach(j)%next
    enddo
  end subroutine report_chunks

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

!!!_  & pos_search()
  integer function pos_search &
       & (jpos, ach, mch, ttbl) &
       & result(n)
    implicit none
    integer,          intent(in) :: jpos    ! count from 1
    type(arg_chunk_t),intent(in) :: ach(0:*)
    integer,          intent(in) :: mch
    character(len=*), intent(in) :: ttbl(0:*)
    integer jc, ji, jp
    n = -1
    if (jpos.le.nparam) then
       loop_chunk: do jc = 0, mch - 1
          jp = ach(jc)%pbgn + 1
          if (jpos.lt.jp.or.jpos.gt.ach(jc)%pend) cycle
          do ji = ach(jc)%tbgn, ach(jc)%tend - 1
             if (ttbl(ji).eq.' ') then
                if (jp.eq.jpos) then
                   n = ji
                   exit loop_chunk
                else
                   jp = jp + 1
                endif
             endif
          enddo
       enddo loop_chunk
    endif
  end function pos_search
!!!_  & tag_search()
  integer function tag_search &
       & (tag, ach, mch, ttbl, ref) &
       & result(n)
    use TOUZA_Std_utl,only: choice
    implicit none
    character(len=*), intent(in)          :: tag
    type(arg_chunk_t),intent(in)          :: ach(0:*)
    integer,          intent(in)          :: mch
    character(len=*), intent(in)          :: ttbl(0:*)
    integer,          intent(in),optional :: ref

    integer jch, jbgn, jt

    jch = chunk_search_idx(ach, mch, ref)
    if (jch.lt.0) then
       n = _ERROR(ERR_INVALID_ITEM)
    else
       jbgn = choice(ach(jch)%tbgn - 1, ref) + 1
       n = -1
       do jt = jbgn, ach(jch)%tend - 1
          if (ttbl(jt).eq.tag) then
             n = jt
             exit
          endif
       enddo
       if (n.lt.0) then
          jch = ach(jch)%next
          loop_chunk: do
             if (jch.lt.0) exit
             do jt = ach(jch)%tbgn, ach(jch)%tend - 1
                if (ttbl(jt).eq.tag) then
                   n = jt
                   exit loop_chunk
                endif
             enddo
             jch = ach(jch)%next
          enddo loop_chunk
       endif
       if (n.lt.0) n = _ERROR(ERR_INVALID_PARAMETER)
    endif
  end function tag_search

!!!_  & arg_search_entry()
  integer function arg_search_entry &
       & (jentr, ach, mch, mtbl) &
       & result(n)
    implicit none
    integer,          intent(in) :: jentr    ! count from 1
    type(arg_chunk_t),intent(in) :: ach(0:*)
    integer,          intent(in) :: mch
    integer,          intent(in) :: mtbl
    integer jch

    jch = chunk_search_entry(ach, mch, mtbl, jentr)
    if (jch.lt.0) then
       n = jch
    else
       n = ((jentr - 1) - ach(jch)%abgn) + ach(jch)%tbgn
    endif
  end function arg_search_entry

!!!_  & chunk_search_idx()
  integer function chunk_search_idx(ach, mch, jidx) result(n)
    implicit none
    type(arg_chunk_t),intent(in)          :: ach(0:*)
    integer,          intent(in)          :: mch
    integer,          intent(in),optional :: jidx
    integer jch
    if (present(jidx)) then
       n = -1
       do jch = 0, mch - 1
          if (ach(jch)%tbgn.le.jidx.and.jidx.lt.ach(jch)%tend) then
             n = jch
             exit
          endif
       enddo
    else
       n = 0
    endif
  end function chunk_search_idx

!!!_  & chunk_search_entry()
  integer function chunk_search_entry(ach, mch, mtbl, jentr) result(n)
    implicit none
    type(arg_chunk_t),intent(in) :: ach(0:*)
    integer,          intent(in) :: mch
    integer,          intent(in) :: mtbl
    integer,          intent(in) :: jentr  ! count from 1
    integer jch

    if (jentr.lt.1.or.jentr.gt.mtbl) then
       n = -1
    else
       n = _ERROR(ERR_PANIC)
       do jch = 0, mch - 1
          if (ach(jch)%abgn.lt.jentr.and.jentr.le.ach(jch)%aend) then
             n = jch
             exit
          endif
       enddo
    endif
  end function chunk_search_entry

!!!_  & extract_val - single
  subroutine extract_val_i &
       & (ierr, val, str, cud, def, unset)
    use TOUZA_Std_utl,only: choice, parse_number
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(inout)       :: val
    character(len=*),intent(in)          :: str
    character(len=*),intent(in)          :: cud
    integer,         intent(in),optional :: def
    logical,         intent(in),optional :: unset

    ierr = err_default
    if (ierr.eq.0) then
       if (str.eq.' ') then
          if (present(def)) then
             val = def
          else if (choice(.false.,unset)) then
             continue
          else
             ierr = _ERROR(ERR_NEED_ARGUMENT)
          endif
       else
          call parse_number(ierr, val, str)
       endif
    else if (present(def)) then
       val = choice(val, def)
       ierr = 0
    endif
    return
  end subroutine extract_val_i

  subroutine extract_val_f &
       & (ierr, val, str, cud, def, unset)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    use TOUZA_Std_utl,only: choice, parse_number
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KTGT), intent(inout)       :: val
    character(len=*),intent(in)          :: str
    character(len=*),intent(in)          :: cud
    real(kind=KTGT), intent(in),optional :: def
    logical,         intent(in),optional :: unset

    ierr = err_default
    if (ierr.eq.0) then
       if (str.eq.' ') then
          if (present(def)) then
             val = def
          else if (choice(.false.,unset)) then
             continue
          else
             ierr = _ERROR(ERR_NEED_ARGUMENT)
          endif
       else
          call parse_number(ierr, val, str)
       endif
    else if (present(def)) then
       val = choice(val, def)
       ierr = 0
    endif
    return
  end subroutine extract_val_f

  subroutine extract_val_d &
       & (ierr, val, str, cud, def, unset)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    use TOUZA_Std_utl,only: choice, parse_number
    implicit none
    integer,         intent(out)         :: ierr
    real(kind=KTGT), intent(inout)       :: val
    character(len=*),intent(in)          :: str
    character(len=*),intent(in)          :: cud
    real(kind=KTGT), intent(in),optional :: def
    logical,         intent(in),optional :: unset

    ierr = err_default
    if (ierr.eq.0) then
       if (str.eq.' ') then
          if (present(def)) then
             val = def
          else if (choice(.false.,unset)) then
             continue
          else
             ierr = _ERROR(ERR_NEED_ARGUMENT)
          endif
       else
          call parse_number(ierr, val, str)
       endif
    else if (present(def)) then
       val = choice(val, def)
       ierr = 0
    endif
    return
  end subroutine extract_val_d

  subroutine extract_val_a &
       & (ierr, val, str, cud, def, unset)
    use TOUZA_Std_utl,only: choice_a, choice
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: val
    character(len=*),intent(in)          :: str
    character(len=*),intent(in)          :: cud
    character(len=*),intent(in),optional :: def
    logical,         intent(in),optional :: unset

    ierr = err_default
    if (ierr.eq.0) then
       if (str.eq.' ') then
          if (present(def)) then
             val = def
          else if (choice(.false.,unset)) then
             continue
          else
             ierr = _ERROR(ERR_NEED_ARGUMENT)
          endif
       else
          val = str
       endif
    else if (present(def)) then
       call choice_a(val, ' ', def)
       ierr = 0
    endif
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
    logical us

    ierr = err_default
    us = choice(.false.,unset)
    jv = 0
    if (ierr.eq.0) then
       if (str.eq.cud) then
          if (present(def)) then
             vals(:) = def
          else if (us) then
             continue
          else
             ierr = _ERROR(ERR_NEED_ARGUMENT)
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
             if (jv.ge.nv) ierr = _ERROR(ERR_OUT_OF_RANGE)
             if (ierr.ne.0) exit
             je = search_next_sep(str, jb, chs)
             if (je.gt.jb) then
                call parse_number(ierr, vals(1+jv), str(jb:je-1))
             else if (jb.eq.je) then
                if (.not.us) ierr = _ERROR(ERR_NEED_ARGUMENT)
             else
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
    logical us

    ierr = err_default
    us = choice(.false.,unset)

    jv = 0
    if (ierr.eq.0) then
       if (str.eq.cud) then
          if (present(def)) then
             vals(:) = def
          else if (us) then
             continue
          else
             ierr = _ERROR(ERR_NEED_ARGUMENT)
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
             if (jv.ge.nv) ierr = _ERROR(ERR_OUT_OF_RANGE)
             if (ierr.ne.0) exit
             je = search_next_sep(str, jb, chs)
             if (je.gt.jb) then
                call parse_number(ierr, vals(1+jv), str(jb:je-1))
             else if (jb.eq.je) then
                if (.not.us) ierr = _ERROR(ERR_NEED_ARGUMENT)
             else
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
    logical us

    ierr = err_default
    us = choice(.false.,unset)

    jv = 0
    if (ierr.eq.0) then
       if (str.eq.cud) then
          if (present(def)) then
             vals(:) = def
          else if (us) then
             continue
          else
             ierr = _ERROR(ERR_NEED_ARGUMENT)
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
             if (jv.ge.nv) ierr = _ERROR(ERR_OUT_OF_RANGE)
             if (ierr.ne.0) exit
             je = search_next_sep(str, jb, chs)
             if (je.gt.jb) then
                call parse_number(ierr, vals(1+jv), str(jb:je-1))
             else if (jb.eq.je) then
                if (.not.us) ierr = _ERROR(ERR_NEED_ARGUMENT)
             else
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
  ! integer function post_get &
  !      & (jerr, unset) &
  !      & result(ierr)
  !   use TOUZA_Std_utl,only: choice
  !   implicit none
  !   integer,intent(in)          :: jerr
  !   logical,intent(in),optional :: unset
  !   ierr = jerr
  !   if (choice(.false., unset)) ierr = min(0, jerr)
  ! end function post_get

!!!_  & default_val - scalar
  subroutine default_val_i &
       & (ierr, val, def, unset)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(inout)       :: val
    integer,intent(in),optional :: def
    logical,intent(in),optional :: unset
    ierr = err_default
    if (ierr.eq.0) then
       if (present(def)) then
          val = def
       else if (choice(.false.,unset)) then
          continue
       else
          ierr = _ERROR(ERR_NEED_ARGUMENT)
       endif
    endif
    return
  end subroutine default_val_i
  subroutine default_val_f &
       & (ierr, val, def, unset)
    use TOUZA_Std_prc,only: KTGT=>KFLT
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(inout)       :: val
    real(kind=KTGT),intent(in),optional :: def
    logical,        intent(in),optional :: unset
    ierr = err_default
    if (ierr.eq.0) then
       if (present(def)) then
          val = def
       else if (choice(.false.,unset)) then
          continue
       else
          ierr = _ERROR(ERR_NEED_ARGUMENT)
       endif
    endif
    return
  end subroutine default_val_f
  subroutine default_val_d &
       & (ierr, val, def, unset)
    use TOUZA_Std_prc,only: KTGT=>KDBL
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,        intent(out)         :: ierr
    real(kind=KTGT),intent(inout)       :: val
    real(kind=KTGT),intent(in),optional :: def
    logical,        intent(in),optional :: unset
    ierr = err_default
    if (ierr.eq.0) then
       if (present(def)) then
          val = def
       else if (choice(.false.,unset)) then
          continue
       else
          ierr = _ERROR(ERR_NEED_ARGUMENT)
       endif
    endif
    return
  end subroutine default_val_d
  subroutine default_val_a &
       & (ierr, val, def, unset)
    use TOUZA_Std_utl,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(inout)       :: val
    character(len=*),intent(in),optional :: def
    logical,         intent(in),optional :: unset
    ierr = err_default
    if (ierr.eq.0) then
       if (present(def)) then
          val = def
       else if (choice(.false.,unset)) then
          continue
       else
          ierr = _ERROR(ERR_NEED_ARGUMENT)
       endif
    endif
    return
  end subroutine default_val_a

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
          ierr = _ERROR(ERR_NEED_ARGUMENT)
       endif
    endif
    if (present(nitem)) then
       nitem = 0
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
          ierr = _ERROR(ERR_NEED_ARGUMENT)
       endif
    endif
    if (present(nitem)) then
       nitem = 0
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
          ierr = _ERROR(ERR_NEED_ARGUMENT)
       endif
    endif
    if (present(nitem)) then
       nitem = 0
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

  ierr = 0
  call init(ierr, lrec=0, levv=+9)

  call batch_test_arg(ierr, .FALSE.)
  call diag(ierr)
  call finalize(ierr, levv=+10)
  stop
contains
  subroutine batch_test_arg(ierr, dup)
    implicit none
    integer,intent(out) :: ierr
    logical,intent(in)  :: dup
    integer jp, np
    integer ja, na
    character(len=128) :: tag, arg
    integer,parameter :: nv = 3
    integer :: ivals(nv)

    ierr = 0
    if (ierr.eq.0) call decl_pos_arg(ierr)
    if (ierr.eq.0) call decl_pos_arg(ierr, 'X')
    if (ierr.eq.0) call decl_pos_arg(ierr, 'Z', 4)
    if (ierr.eq.0) call decl_pos_arg(ierr, 'Y')

    if (ierr.eq.0) then
       call decl_pos_arg(ierr, 'X', dup=dup)
       if (dup) then

       else if (ierr.eq.0) then
          ierr = ERR_PANIC
       else
          ierr = 0
       endif
    endif

    if (ierr.eq.0) call parse(ierr)

    if (ierr.eq.0) then
       np = get_nparam()
101    format('arg/param:', I0, 2x, I0, '/', I0, 1x, '[', A, ']')
       do jp = 1, np + 1
          call get_param(ierr, arg, jp)
          write(*, 101) ierr, jp, np, trim(arg)
          ierr = 0
       enddo

102    format('arg/param/ia:', I0, 2x, I0, '/', I0, 1x, 10(1x, I0))
       do jp = 1, np + 1
          call get_param(ierr, ivals(:), jp, -9)
          write(*, 102) ierr, jp, np, ivals(:)
          ierr = 0
       enddo

    endif
    if (ierr.eq.0) call test_get_option(ierr, 'X')
    if (ierr.eq.0) call test_get_option(ierr, 'Y')
    if (ierr.eq.0) call test_get_option(ierr, 'Z')
    if (ierr.eq.0) call test_get_option(ierr, 'W')
    if (ierr.eq.0) call test_get_option(ierr, 'V')

    if (ierr.eq.0) then
111    format('arg/arg:', I0, 2x, I0, '/', I0, 1x, '[', A, ']', 1x, A)
       na = get_nargs()
       do ja = 1, na + 1
          call get_arg(ierr, tag, arg, ja)
          write(*, 111) ierr, ja, na, trim(tag), trim(arg)
          ierr = 0
       enddo
    endif

  end subroutine batch_test_arg

  subroutine test_get_option(ierr, tag)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: tag

    character(len=128) :: val
    integer jref

    call get_option(ierr, val, tag, def='(null)')
101 format('arg/option:', I0, '[', A, ']', 1x, A)
    write(*, 101) ierr, trim(tag), trim(val)
    ierr = 0

    jref = 0
    do
       call get_option(ierr, val, tag, def='(null)', ref=jref)
102    format('arg/option/loop:', I0, '[', A, ']', I0, 1x, A)
       write(*, 102) ierr, trim(tag), jref, trim(val)
       if (jref.lt.0) exit
       jref = jref + 1
    enddo
    ierr = 0

  end subroutine test_get_option

end program test_std_arg
#endif /* TEST_STD_ARG */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

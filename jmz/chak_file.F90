!!!_! chak_file.F90 - TOUZA/Jmz CH(swiss) army knife file interfaces
! Maintainer: SAITO Fuyuki
! Created: Oct 26 2022
#define TIME_STAMP 'Time-stamp: <2024/06/21 17:25:00 fuyuki chak_file.F90>'
!!!_! MANIFESTO
!
! Copyright (C) 2022,2023
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
#ifndef TEST_CHAK_FILE
#  define TEST_CHAK_FILE 0
#endif
!!!_@ TOUZA/Jmz/chak_file - nio swiss army knife (file interfaces)
module chak_file
!!!_ + Declaration
!!!_  - modules
  use Jmz_param
  use Jmz_base, jb_init=>init, jb_finalize=>finalize
  ! use Jmz_base, jb_open_write_file => open_write_file
  use Jmz_coor, lname_co=>lname
  use Jmz_file
  ! use Jmz_coor,only: loop_null, loop_reduce, loop_normal, null_range
  ! use Jmz_coor,only: loop_unset
  use chak_lib,only: lib_init=>init, lib_finalize=>finalize
  use chak_lib,only: loop_t, lname, user_index_bgn, user_index_end
  use chak_lib,only: parse_format_shape
  implicit none
  public
!!!_  - parameters
  integer,parameter :: lfmt  = litem * 4

  ! access mode
  integer,parameter :: mode_unset      = -999
  integer,parameter :: mode_cycle      = -3    ! rewind if eof
  integer,parameter :: mode_persistent = -2    ! keep final if eof
  integer,parameter :: mode_terminate  = -1    ! terminate if eof
  integer,parameter :: mode_read       = 0

  integer,parameter :: mode_new        = 1     ! error if exist
  integer,parameter :: mode_write      = 2     ! force overwrite
  integer,parameter :: mode_append     = 3     ! append

  integer,parameter :: hflag_unset   = -1
  integer,parameter :: hflag_default = 0
  integer,parameter :: hflag_nulld   = 1       ! strict null-coordinate mode

  integer,parameter :: iflag_unset   = -1
  integer,parameter :: iflag_default = 0
  integer,parameter :: iflag_dset    = 1       ! dset prefix

  integer,parameter :: hedit_unset = -9
  integer,parameter :: hedit_sign = 0
  integer,parameter :: hedit_edit = 1
  integer,parameter :: hedit_title = 2
  integer,parameter :: hedit_item = 3
  integer,parameter :: hedit_all = 9

  ! record specials
  integer,parameter :: rsp_exhaust = -1
  integer,parameter :: rsp_beyond = -2
  integer,parameter :: rsp_remain = -3

  ! terminate status
  integer,parameter :: stt_error = -2
  integer,parameter :: stt_lack  = -1  ! insufficient records
  integer,parameter :: stt_term  = 0   ! MUST terminate
  integer,parameter :: stt_cont  = 1   ! continue loop
  integer,parameter :: stt_wait  = 2   ! CAN teriminate

  integer,parameter,private :: set_begin = -1
  integer,parameter,private :: set_currrent = 0
  integer,parameter,private :: set_sub      = 1
  integer,parameter,private :: set_filter   = 2
  integer,parameter,private :: set_end = 3

  ! Big-GTOOL mode
  ! integer,parameter :: bigg_rmagic  = 0   ! read:  only at magic-separators
  ! integer,parameter :: bigg_rlarge  = 1   ! read:  larger record or magic-separator
  ! integer,parameter :: bigg_woff    = 0   ! write: off, sub-record mode
  ! integer,parameter :: bigg_won     = 1   ! write: on, Big-GTOOL mode
  integer,parameter :: bigg_off = 0
  integer,parameter :: bigg_on  = 1

!!!_  - separator special
    character(len=*),parameter :: urt_osep = '+'
!!!_  - type
  integer,parameter :: rec_bgn = 0
  integer,parameter :: rec_end = 1
  integer,parameter :: rec_stp = 2

  ! record filter element   either loop or array
  type rec_t
     integer :: num = 0
     integer,pointer :: seq(:)
  end type rec_t
  ! seq is direct position array or element if rec_t%num < 0
  !        bgn-end-stp triplet if rec_t%num > 0

  type rgroup_t
     type(rec_t),pointer :: filter(:)   ! record filter
     integer             :: cur ! current filter index
     integer             :: sub ! sub-index in filter
     integer             :: term_flag ! terminate flag

     integer             :: rec ! target record
     integer(kind=KIOFS) :: pos ! file position cache

     integer             :: orec ! record cache
     integer(kind=KIOFS) :: opos ! file position cache
  end type rgroup_t

  type file_t
     character(len=lpath)   :: name
     character(len=litem)   :: h(nitem)
     character(len=lfmt)    :: fmt
     integer                :: u
     integer                :: t
     integer                :: irec, nrec
     integer                :: kfmt
     integer                :: mode      ! access mode
     integer                :: hedit     ! header edit level
     integer                :: hflag = hflag_unset ! header parser flag
     integer                :: iflag = iflag_unset ! header parser flag (for item)
     type(rgroup_t),pointer :: rgrp(:) => NULL()
     integer                :: bigg
     integer                :: opr       ! fake entry as operator
  end type file_t
!!!_ + Procedures
contains
!!!_  - init
  subroutine init(ierr)
    use TOUZA_Std_env,only: init_file_bodr
    implicit none
    integer,intent(out) :: ierr
    ierr = 0
    call init_file_bodr(ierr)
  end subroutine init

!!!_  - reset_file
  subroutine reset_file(ierr, file, name, mode, flag, bigg)
    use TOUZA_Std,only: choice
    use TOUZA_Nio,only: GFMT_ERR
    implicit none
    integer,         intent(out)         :: ierr
    type(file_t),    intent(inout)       :: file
    character(len=*),intent(in),optional :: name
    integer,         intent(in),optional :: mode
    integer,         intent(in),optional :: flag
    integer,         intent(in),optional :: bigg

    ierr = 0

    file%u = -1
    file%irec = 0
    file%nrec = -1
    file%h = ' '
    file%fmt = ' '
    file%kfmt  = cfmt_error
    file%hedit = hedit_all
    file%mode  = choice(mode_unset, mode)
    file%hflag = choice(hflag_unset, flag)
    file%bigg  = choice(bigg_on, bigg)
    file%iflag = iflag_unset
    file%opr = -1
    ! file%bh = -1

    if (present(name)) then
       file%name = name
    else
       file%name = ' '
    endif
  end subroutine reset_file
!!!_  - common
!!!_   . get_recs_str
  subroutine get_recs_str(ierr, str, file, nrg)
    use TOUZA_Std,only: join_list
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    type(file_t),    intent(in)  :: file
    integer,         intent(in)  :: nrg
    character(len=128) :: rec(0:nrg-1)
    integer jrg, jrf

    ierr = 0
101 format(I0, '+', I0)
102 format(I0)
111 format(I0, ':+', I0)
112 format(I0)
    do jrg = 0, nrg - 1
       jrf = file%rgrp(jrg)%cur
       if (file%rgrp(jrg)%filter(jrf)%num.lt.-1) then
          write(rec(jrg), 101, IOSTAT=ierr) file%rgrp(jrg)%rec, size(file%rgrp(jrg)%filter(jrf)%seq)
       else if (file%rgrp(jrg)%filter(jrf)%num.eq.-1) then
          write(rec(jrg), 102, IOSTAT=ierr) file%rgrp(jrg)%rec
       else if (file%rgrp(jrg)%filter(jrf)%num.gt.1) then
          write(rec(jrg), 111, IOSTAT=ierr) file%rgrp(jrg)%rec, file%rgrp(jrg)%filter(jrf)%num
       else
          write(rec(jrg), 112, IOSTAT=ierr) file%rgrp(jrg)%rec
       endif
       if (ierr.ne.0) exit
       ! rec(0:nrg-1) = file%rgrp(0:nrg-1)%rec
    enddo
    if (ierr.eq.0) call join_list(ierr, str, rec(0:nrg-1), sep=sep_item)
  end subroutine get_recs_str
!!!_   . show_file
  subroutine show_file &
       & (ierr, file, u, levv)
    use TOUZA_Std,only: choice
    implicit none
    integer,     intent(out)         :: ierr
    type(file_t),intent(in)          :: file
    integer,     intent(in),optional :: u
    integer,     intent(in),optional :: levv

    integer utmp, lv
    character(len=1) :: cm
    integer jrg

    ierr = 0
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

201 format('file:', I0, 1x, A1, 1x, I0, '/', I0, 1x, A)
    if (ierr.eq.0) then
       select case(file%mode)
       case (mode_read)
          cm = 'r'
       case (mode_cycle)
          cm = 'c'
       case (mode_persistent)
          cm = 'p'
       case (mode_terminate)
          cm = 'l'
       case (mode_new)
          cm = 'n'
       case (mode_write)
          cm = 'f'
       case (mode_append)
          cm = 'a'
       case default
          cm = 'e'
       end select
       write(utmp, 201) file%u, cm, file%irec, file%nrec, trim(file%name)
       if (is_msglev_DETAIL(lv)) then
101       format('  set: ', I0)
          if (associated(file%rgrp)) then
             do jrg = 0, size(file%rgrp) - 1
                write(utmp, 101) user_index_bgn(jrg)
                if(ierr.eq.0) call show_rec_group(ierr, file%rgrp(jrg), utmp, lv)
             enddo
          endif
       endif
    endif
  end subroutine show_file

!!!_   . show_rec_group
  subroutine show_rec_group &
       & (ierr, rgrp, u, levv)
    use TOUZA_Std,only: choice
    implicit none
    integer,       intent(out)         :: ierr
    type(rgroup_t),intent(in)          :: rgrp
    integer,       intent(in),optional :: u
    integer,       intent(in),optional :: levv
    integer jrf
    integer utmp, lv

    ierr = 0
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)
    do jrf = 0, size(rgrp%filter) - 1
       if (ierr.eq.0) call show_rec_felem(ierr, rgrp%filter(jrf), utmp, lv)
    enddo
  end subroutine show_rec_group

!!!_   . show_rec_felem
  subroutine show_rec_felem &
       & (ierr, filter, u, levv)
    use TOUZA_Std,only: join_list, choice
    implicit none
    integer,    intent(out)         :: ierr
    type(rec_t),intent(in)          :: filter
    integer,    intent(in),optional :: u
    integer,    intent(in),optional :: levv
    integer jb, je
    integer ms
    integer,parameter :: nb = max(2, 16)   !! must be greater than 1
    integer,parameter :: lb = nb * 12
    integer           :: r(0:nb-1)
    character(len=lb) :: buf
    integer utmp, lv
    ierr = 0
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)

101 format('    rec+', I0, 1x, A)
102 format('    rec+', I0, 1x, A, A)
111 format('    rec+', I0, 1x, I0, ':', I0, ':', I0)
    if (filter%num.gt.0) then
       r(0) = user_index_bgn(filter%seq(rec_bgn))
       r(1) = user_index_end(filter%seq(rec_end))
       write(utmp, 111) filter%num, r(0), r(1), filter%seq(rec_stp)
    else if (filter%num.lt.0) then
       ms = size(filter%seq)
       do jb = 0, ms - 1, nb
          je = min(ms, jb + nb)
          if (ierr.eq.0) r(0:je-jb-1) = user_index_bgn(filter%seq(jb:je-1))
          if (ierr.eq.0) call join_list(ierr, buf, r(0:je-jb-1), sep=sep_rec_append)
          if (ierr.eq.0) then
             if (jb.eq.0) then
                write(utmp, 101) ms, trim(buf)
             else
                write(utmp, 102) ms, sep_rec_append, trim(buf)
             endif
          endif
       enddo
    endif
  end subroutine show_rec_felem

!!!_   . set_file_format
  subroutine set_file_format &
       & (ierr, file, arg)
    use TOUZA_Std,only: upcase, find_next_sep
    use TOUZA_Nio,only: parse_record_fmt
    implicit none
    integer,         intent(out)   :: ierr
    type(file_t),    intent(inout) :: file
    character(len=*),intent(in)    :: arg
    character(len=litem*4) :: abuf

    character(len=*),parameter :: asep = sep_item
    character(len=*),parameter :: bsep = ':'
    integer jp
    integer kfmt

    ierr = 0

    abuf = ' '
    jp = find_next_sep(arg, asep)
    call upcase(abuf, arg(1:jp))
    abuf = abuf(1:jp) // arg(jp+1:)
    call parse_record_fmt(ierr, kfmt, abuf)
    if (ierr.eq.0) then
       file%kfmt = cfmt_gtool_seq
       file%fmt = trim(abuf)
    else
       ierr = 0
       select case (abuf(1:1))
       case ('A')               ! ascii
          jp = index(abuf, asep)
          if (jp.eq.0) then
             file%fmt = trim(abuf(2:))
          else
             file%fmt = trim(abuf(jp+1:))
          endif
          file%kfmt = cfmt_ascii
       case ('B')               ! binary
          jp = index(abuf, asep)
          if (jp.eq.0) then
             file%fmt = ' '
          else
             file%fmt = trim(abuf(jp+1:))
             ! clear shape
             abuf = abuf(1:jp-1)
          endif
          select case(abuf(2:3))
          case ('I4')
             file%kfmt = cfmt_binary_i4
          case ('R4')
             file%kfmt = cfmt_binary_r4
          case ('R8')
             file%kfmt = cfmt_binary_r8
          case default
             ierr = ERR_INVALID_PARAMETER
          end select
          if (ierr.eq.0) then
             jp = 4
             if (abuf(jp:jp).eq.bsep) jp = jp + 1
             select case(abuf(jp:jp))
             case('N')
                file%kfmt = file%kfmt + cfmt_flag_native
             case('S')
                file%kfmt = file%kfmt + cfmt_flag_swap
             case('B')
                file%kfmt = file%kfmt + cfmt_flag_big
             case('L')
                file%kfmt = file%kfmt + cfmt_flag_little
             case(' ')
                continue
             case default
                ierr = ERR_INVALID_PARAMETER
             end select
          endif
          if (ierr.ne.0) then
             call message(ierr, 'unknown format ' // trim(arg))
          endif
       case ('C')               ! binary
          file%fmt = trim(abuf)
          file%kfmt = cfmt_cdf
       case default
          ierr = ERR_INVALID_PARAMETER
          call message(ierr, 'unknown format ' // trim(arg))
       end select
    endif
    return
  end subroutine set_file_format

!!!_   . add_default_records
  subroutine add_default_records &
       & (ierr, file, recend)
    implicit none
    integer,     intent(out)         :: ierr
    type(file_t),intent(inout)       :: file
    integer,     intent(in),optional :: recend

    integer jrg

    ierr = 0
    if (ierr.eq.0) call alloc_rec_filter(ierr, file)
    if (ierr.eq.0) then
       jrg = size(file%rgrp) - 1
       call set_default_rgroup(ierr, file%rgrp(jrg), recend)
    endif
  end subroutine add_default_records

!!!_   . clone_default_rgroup
  subroutine clone_default_rgroup &
       & (ierr, file, def)
    implicit none
    integer,     intent(out)   :: ierr
    type(file_t),intent(inout) :: file
    type(file_t),intent(in)    :: def

    integer jrg, nrg
    integer nrf
    ierr = 0
    if (associated(file%rgrp, def%rgrp)) then
       nrg = size(def%rgrp)
       ! write(*, *) 'clone:', nrg, trim(file%name)
       allocate(file%rgrp(0:nrg-1), STAT=ierr)
       if (ierr.eq.0) then
          do jrg = 0, nrg - 1
             nrf = size(def%rgrp(jrg)%filter)
             if (ierr.eq.0) allocate(file%rgrp(jrg)%filter(0:nrf-1), STAT=ierr)
             if (ierr.eq.0) file%rgrp(jrg)%filter(0:nrf-1) = def%rgrp(jrg)%filter(0:nrf-1)
          enddo
       endif
    endif
    return
  end subroutine clone_default_rgroup

!!!_   . set_default_rgroup
  subroutine set_default_rgroup &
       & (ierr, rgrp, recend)
    implicit none
    integer,       intent(out)         :: ierr
    type(rgroup_t),intent(inout)       :: rgrp
    integer,       intent(in),optional :: recend

    ierr = 0
    allocate(rgrp%filter(0:0), STAT=ierr)
    if (ierr.eq.0) call set_default_filter(ierr, rgrp%filter(0), recend)
  end subroutine set_default_rgroup

!!!_   . set_default_filter
  subroutine set_default_filter &
       & (ierr, filter, recend)
    use TOUZA_Std,only: choice
    implicit none
    integer,    intent(out)         :: ierr
    type(rec_t),intent(inout)       :: filter
    integer,    intent(in),optional :: recend
    integer e

    ierr = 0
    e = choice(-1, recend)
    allocate(filter%seq(0:2), STAT=ierr)
    if (ierr.eq.0) then
       filter%seq = (/0, e, 1/)
       filter%num = 1
    endif
  end subroutine set_default_filter

!!!_   . parse_rec_filter
  subroutine parse_rec_filter &
       & (ierr, nbuf, file, arg)
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(out)   :: nbuf
    type(file_t),    intent(inout) :: file
    character(len=*),intent(in)    :: arg

    integer jrg

    ierr = 0

    if (ierr.eq.0) call alloc_rec_filter(ierr, file)
    if (ierr.eq.0) then
       jrg = size(file%rgrp) - 1
       call parse_rec_argument(ierr, file%rgrp(jrg), arg)
    endif
    if (ierr.eq.0) then
       nbuf = max_group_records(file%rgrp(jrg))
    else
       nbuf = -1
    endif
  end subroutine parse_rec_filter

!!!_   . alloc_rec_filter
  subroutine alloc_rec_filter &
       & (ierr, file)
    implicit none
    integer,     intent(out)   :: ierr
    type(file_t),intent(inout) :: file

    type(rgroup_t),pointer :: tmp(:)
    integer nfold

    ierr = 0
    if (associated(file%rgrp)) then
       nfold = size(file%rgrp)
       allocate(tmp(0:nfold), STAT=ierr)
       if (ierr.eq.0) then
          tmp(0:nfold-1) = file%rgrp(0:nfold-1)
          deallocate(file%rgrp, STAT=ierr)
       endif
       if (ierr.eq.0) file%rgrp => tmp
    else
       nfold = 0
       allocate(file%rgrp(0:nfold), STAT=ierr)
    endif
  end subroutine alloc_rec_filter

!!!_   . parse_rec_argument
  subroutine parse_rec_argument &
       & (ierr, rgrp, arg)
    use chak_lib,lsep=>sep_item, rsep=>sep_range
    use TOUZA_Std,only: split_list
    implicit none
    integer,         intent(out)   :: ierr
    type(rgroup_t),  intent(inout) :: rgrp
    character(len=*),intent(in)    :: arg
    integer jc, nc
    integer jb, je, larg, ls
    integer dummy(0:0)
    ierr = 0
    larg = len_trim(arg)
    ls = len(sep_item)
    call split_list(nc, dummy, arg(1:larg), sep_item, -1, empty=.FALSE.)
    ierr = min(0, nc)
    if (ierr.eq.0) allocate(rgrp%filter(0:nc-1), STAT=ierr)
    if (ierr.eq.0) then
       jb = 0
       jc = 0
       do
          if (jb.ge.larg) exit
          je = index(arg(jb+1:larg), sep_item) + jb
          if (je.eq.jb) je = larg + 1
          ! write(*, *) jc, jb, je, '[' // arg(jb+1:je-1) // ']'
          if (jb+1.lt.je) then
             call parse_rec_element(ierr, rgrp%filter(jc), arg(jb+1:je-1))
             if (ierr.ne.0) exit
             jc = jc + 1
          endif
          jb = je + ls - 1
       enddo
    endif
    if (ierr.ne.0) then
       call message(ierr, 'bad record argument ' // arg(1:larg))
    endif
  end subroutine parse_rec_argument

!!!_   . parse_rec_element - parse record-argument element
  subroutine parse_rec_element &
       & (ierr, relem, arg)
    use chak_lib,lsep=>sep_item, rsep=>sep_range
    use TOUZA_Std,only: split_list, parse_number
    implicit none
    integer,         intent(out)   :: ierr
    type(rec_t),     intent(inout) :: relem
    character(len=*),intent(in)    :: arg
    integer larg
    integer lsa
    integer jp,  nr,  ja
    integer jerr
    integer r
    integer,parameter :: nfield = 4
    integer rf(0:nfield-1)
    integer,save :: def(0:nfield-1) = null_range
    integer b, e

    ierr = 0
    ! rec[/rec...]
    ! [begin][:[end][:[step][:[num]]]]

    larg = len_trim(arg)
    lsa = len(sep_rec_append)
    jp = 0
    ja = index(arg(jp+1:larg), sep_rec_append)
    nr = 0
    if (ja.gt.0) then
       ! mutliple stack mode
       ! count element
       call split_list(nr, relem%seq, arg(jp+1:larg), sep_rec_append, -1, empty=.FALSE.)
       ierr = min(0, nr)
    else
       call parse_number(jerr, r, arg(jp+1:larg))
       if (jerr.eq.0) then
          ! single stack mode
          nr = 1
       else
          ! range complex mode
          call split_list &
               & (nr, rf, arg(jp+1:larg), sep_range, nfield, def)
          if (nr.eq.1) then
             rf(1:3) = (/null_range, 0, 1/)
          else if (nr.eq.2) then
             rf(2:3) = (/1, 1/)
          else if (nr.eq.3) then
             rf(3:3) = (/1/)
          else if (nr.eq.4) then
             ! adjust num
             b = system_index_bgn(rf(0))
             if (b.eq.null_range) b = 0
             e = system_index_end(rf(1))
             if (rf(3).eq.null_range) rf(3) = 1
             if (rf(3).eq.0) then
                if (rf(2).ne.null_range) rf(3) = abs(rf(2))
             endif
             if (rf(3).eq.0) then
                if (e.ge.0) then
                   rf(3) = e - b
                   if (rf(2).eq.null_range) rf(2) = 0
                endif
             endif
             if (rf(2).eq.null_range) rf(2) = rf(3)
          else
             ierr = ERR_INVALID_PARAMETER
          endif
          if (ierr.eq.0) then
             if (rf(2).eq.null_range) rf(2) = rf(3)
             if (rf(3).le.0) ierr = ERR_INVALID_PARAMETER
             if (rf(2).eq.null_range) ierr = ERR_INVALID_PARAMETER
          endif
          if (ierr.eq.0) allocate(relem%seq(0:2), STAT=ierr)
          if (ierr.eq.0) then
             b = system_index_bgn(rf(0))
             if (b.eq.null_range) b = 0
             relem%seq(rec_bgn) = b
             e = system_index_end(rf(1))
             if (e.eq.null_range) then
                if (rf(2).eq.0) then
                   e = b + rf(3)
                else
                   e = -1
                endif
             endif
             relem%seq(rec_end) = e
             relem%seq(rec_stp) = rf(2)
             relem%num = rf(3)
          endif
          nr = 0 ! reset nr to pass direct stack mode
       endif
    endif
    if (ierr.eq.0.and.nr.gt.0) then
       ! direct stack mode
       allocate(relem%seq(0:nr-1), STAT=ierr)
       if (ierr.eq.0) then
          call split_list(jerr, relem%seq, arg(jp+1:larg), sep_rec_append, nr, empty=.FALSE.)
          if (jerr.ge.0) then
             relem%num = -nr
             relem%seq(0:nr-1) = system_index_bgn(relem%seq(0:nr-1))
          else
             ierr = jerr
          endif
       endif
    endif
    if (ierr.ne.0) then
       call message(ierr, 'bad record argument ' // arg(jp+1:larg))
    endif
    return
  end subroutine parse_rec_element

!!!_   . count_file_stacks
  integer function count_file_stacks(file) result(n)
    implicit none
    type(file_t),intent(in) :: file
    integer jrg
    n = 0
    do jrg = 0, size(file%rgrp) - 1
       n = n + max_group_records(file%rgrp(jrg))
    enddo
  end function count_file_stacks

!!!_   . max_group_records
  integer function max_group_records(rgrp) result(n)
    implicit none
    type(rgroup_t),intent(in) :: rgrp
    integer jrf
    n = 0
    do jrf = 0, size(rgrp%filter) - 1
       n = max(n, count_filter_records(rgrp%filter(jrf)))
    enddo
  end function max_group_records
!!!_   . count_filter_records
  integer function count_filter_records(filter) result(n)
    implicit none
    type(rec_t),intent(in) :: filter
    if (filter%num.ge.0) then
       n = filter%num
    else
       n = size(filter%seq)
    endif
  end function count_filter_records

!!!_   . open_read_file
  subroutine open_read_file &
       & (ierr, file, def, levv)
    use TOUZA_Std,only: new_unit, sus_open, is_error_match, upcase
    use TOUZA_Std,only: check_byte_order
    use TOUZA_Nio,only: cache_open_read, show_cache
    implicit none
    integer,     intent(out)         :: ierr
    type(file_t),intent(inout)       :: file
    type(file_t),intent(in)          :: def
    integer,     intent(in),optional :: levv

    character(len=128) :: iomsg
    integer mpath, mitem
    integer ch

    ierr = 0

    if (file%u.lt.0) then
       if (file%hflag.eq.hflag_unset) file%hflag = def%hflag
       if (file%iflag.eq.iflag_unset) file%iflag = def%iflag
       if (file%mode.eq.mode_read) file%mode = def%mode

       file%u = new_unit()
       ierr = min(0, file%u)
       if (ierr.eq.0) then
          if (file%kfmt.eq.cfmt_ascii) then
             open(UNIT=file%u, FILE=file%name, IOSTAT=ierr, &
                  & ACTION='READ', STATUS='OLD', FORM='FORMATTED', &
                  & ACCESS='STREAM')
             if (ierr.eq.0) then
                call open_read_ascii(ierr, file)
             else
                ierr = ERR_INVALID_PARAMETER
             endif
          else if (file%kfmt.eq.cfmt_cdf) then
             ierr = ERR_NOT_IMPLEMENTED
          else if (file%kfmt.ge.cfmt_binary) then
             call check_byte_order(ierr, file%t, file%u, force=.TRUE., levv=levv)
             if (ierr.eq.0) then
                call sus_open(ierr, file%u, file%name, ACTION='R', STATUS='O', IOMSG=iomsg)
             endif
             if (ierr.eq.0) call open_read_binary(ierr, file)
             if (ierr.ne.0) ierr = ERR_INVALID_PARAMETER
          else
             call parse_file_item(ierr, mpath, mitem, file%name, file%u)
             if (ierr.eq.0) then
                if (mpath.eq.0) then
                   call sus_open(ierr, file%u, file%name, ACTION='R', STATUS='O', IOMSG=iomsg)
                else if (mpath.gt.0) then
                   file%name = file%name(1:mpath)
                   call cache_open_read(ierr, ch, file%name, flag=0, unit=file%u)
                   if (ierr.eq.0) then
                      file%kfmt = cfmt_gtool_cache
                      file%u = ch
                      ! if (is_msglev_DEBUG(levv)) call show_cache(ierr, ch, levv=levv)
                   endif
                endif
             endif
          endif
       endif
       if (ierr.eq.0) call clone_default_rgroup(ierr, file, def)
       if (ierr.eq.0) then
          file%irec = 0
          file%rgrp(:)%cur = 0
          file%rgrp(:)%sub = 0
          file%rgrp(:)%pos = 0
          file%rgrp(:)%opos = -1
          file%rgrp(:)%orec = -1
          file%rgrp(:)%term_flag = mode_read
       endif
       if (ierr.ne.0) then
          call message(ierr, trim(iomsg))
          call message(ierr, 'failed to read open: ' // trim(file%name))
          return
       endif
    endif
  end subroutine open_read_file

! !!!_   . parse_file_item
!   subroutine parse_file_item &
!        & (ierr, mpath, mitem, &
!        &  file, utest, seps,  rlev)
!     use TOUZA_Std,only: choice, choice_a
!     use TOUZA_Std,only: new_unit, sus_open, sus_close
!     implicit none
!     integer,         intent(out)         :: ierr
!     integer,         intent(out)         :: mpath, mitem
!     character(len=*),intent(in)          :: file
!     integer,         intent(in),optional :: utest
!     character(len=*),intent(in),optional :: seps
!     integer,         intent(in),optional :: rlev

!     integer jerr_full
!     integer jerr_filter
!     integer utmp
!     logical btmp
!     integer lf
!     integer jc
!     character(len=1) :: csep

!     ! A/B/file             A/B/file   + (null)     sequential
!     ! A/B/file/            A/B/file   + (null)     cache
!     ! A/B/file/item        A/B/file   + item       cache
!     ! A/B/file/group/item  A/B/file   + group/item cache

!     ! not implemented
!     ! (deprecated)
!     ! A/B/file?item        A/B/file   + item       cache
!     ! A/B/file?group/item  A/B/file   + group/item cache
!     ! A/B/?file?           A/B/?file  + (null)     sequential
!     ! A/B/?file/           A/B/?file  + (null)     cache

!     ierr = 0
!     mpath = 0     ! normal path
!     mitem = -1    ! sequential mode

!     utmp = choice(-1, utest)
!     if (utmp.lt.0) utmp = new_unit()
!     if (utmp.lt.0) then
!        ierr = utmp
!        return
!     endif
!     if (ierr.eq.0) inquire(unit=utmp, opened=btmp, IOSTAT=ierr)
!     if (ierr.eq.0) then
!        if (btmp) ierr = ERR_PANIC
!     endif
!     jerr_full = -1
!     jerr_filter = -1
!     if (ierr.eq.0) then
!        ! full-name
!        call sus_open(jerr_full, utmp, file, ACTION='R', STATUS='O')
!        if (jerr_full.eq.0) call sus_close(ierr, utmp, file)
!     endif
!     if (ierr.eq.0) then
!        if (jerr_full.ne.0) then
!           lf = len_trim(file)
!           mitem = lf
!           do jc = 0, file_item_level - 1
!              mitem = index(file(1:mitem), path_sep, back=.true.)
!              if (mitem.le.0) exit
!              mitem = mitem - 1
!              call sus_open(jerr_filter, utmp, file(1:mitem), ACTION='R', STATUS='O')
!              if (jerr_filter.eq.0) then
!                 mpath = mitem
!                 mitem = mitem + 2
!                 if (mitem.gt.lf) mitem = 0
!                 call sus_close(ierr, utmp, file)
!                 exit
!              endif
!           enddo
!        endif
!     endif
!     if (jerr_full.eq.0) then
!        mpath = 0
!        mitem = 0
!     endif
!     ! if (ierr.eq.0) call sus_close(ierr, utmp, file)
!   end subroutine parse_file_item

!!!_   . cue_read_file
  subroutine cue_read_file &
       & (ierr, file)
    use TOUZA_Std,only: new_unit
    implicit none
    integer,     intent(out)   :: ierr
    type(file_t),intent(inout) :: file
    integer :: jrg, nrg
    ! integer jrf
    ! integer ji

    ierr = 0
    if (file%kfmt.eq.cfmt_gtool_cache) return

    nrg = size(file%rgrp)
    file%rgrp(0:nrg-1)%pos = -1
    do jrg = 0, nrg - 1
       call set_group_record(ierr, file%rgrp(jrg))
    enddo
    select case(file%kfmt)
    case(cfmt_ascii)
       call cue_read_ascii(ierr, file)
    case(cfmt_binary:cfmt_cdf-1)
       call cue_read_binary(ierr, file)
    case(cfmt_cdf)
       ierr = ERR_NOT_IMPLEMENTED
    case default
       call cue_read_nio(ierr, file)
    end select
    if (ANY(file%rgrp(0:nrg-1)%pos.lt.0)) then
       ierr = ERR_INVALID_ITEM
       call message(ierr, 'failed at initial cue ' // trim(file%name))
    endif
    if (ierr.eq.0) then
       file%rgrp(0:nrg-1)%opos = file%rgrp(0:nrg-1)%pos
       file%rgrp(0:nrg-1)%orec = file%rgrp(0:nrg-1)%rec
    endif
    ! call show_file(ierr, file)
end subroutine cue_read_file

!!!_   . cue_next_read
  subroutine cue_next_read &
       & (ierr, stt, file)
    use TOUZA_Std,only: new_unit
    implicit none
    integer,     intent(out)   :: ierr
    integer,     intent(out)   :: stt
    type(file_t),intent(inout) :: file
    integer jrg, nrg
    integer nrf
    integer nc, nw, nt
    ierr = 0
    if (file%kfmt.eq.cfmt_gtool_cache) return
    ! store current status
    nrg = size(file%rgrp)
    if (file%mode.eq.mode_persistent) then
       file%rgrp(0:nrg-1)%opos = file%rgrp(0:nrg-1)%pos
       file%rgrp(0:nrg-1)%orec = file%rgrp(0:nrg-1)%rec
    endif
    do jrg = 0, nrg - 1
       if (file%rgrp(jrg)%term_flag.eq.mode_terminate) then
          file%rgrp(jrg)%pos = file%rgrp(jrg)%opos
       else
          file%rgrp(jrg)%pos = -1
          if (ierr.eq.0) call set_group_record(ierr, file%rgrp(jrg), set_sub)
       endif
    enddo
    loop_cue: do
       if (ierr.eq.0) then
          select case(file%kfmt)
          case(cfmt_ascii)
             call cue_read_ascii(ierr, file)
          case(cfmt_binary:cfmt_cdf-1)
             call cue_read_binary(ierr, file)
          case(cfmt_cdf)
             ierr = ERR_NOT_IMPLEMENTED
          case default
             call cue_read_nio(ierr, file)
          end select
       endif
       ! to next filter if needed
       if (ierr.eq.0) then
          nw = 0
          do jrg = 0, nrg - 1
             ! write (*, *) 'pos = ', jrg, file%rgrp(jrg)%pos
             if (file%rgrp(jrg)%pos.lt.0) then
                if (ierr.eq.0) call set_group_record(ierr, file%rgrp(jrg), set_filter)
                if (ierr.eq.0) then
                   if (file%rgrp(jrg)%rec.ne.rsp_exhaust) then
                      nw = nw + 1
                      cycle
                   endif
                   if (file%rgrp(jrg)%term_flag.eq.mode_read) then
                      nrf = size(file%rgrp(jrg)%filter)
                      if (nrf.eq.1.and.file%rgrp(jrg)%filter(0)%num.lt.0) then
                         file%rgrp(jrg)%term_flag = mode_persistent
                      else
                         select case (file%mode)
                         case (mode_cycle)
                            file%rgrp(jrg)%term_flag = mode_cycle
                         case (mode_persistent)
                            file%rgrp(jrg)%term_flag = mode_persistent
                         case default
                            file%rgrp(jrg)%term_flag = mode_terminate
                         end select
                      endif
                   endif
                   if (file%rgrp(jrg)%term_flag.eq.mode_cycle) then
                      file%rgrp(jrg)%rec = file%rgrp(jrg)%orec
                      file%rgrp(jrg)%pos = file%rgrp(jrg)%opos
                      call set_group_record(ierr, file%rgrp(jrg), set_begin)
                   else if (file%rgrp(jrg)%term_flag.eq.mode_persistent) then
                      ! write(*, *) 'term', file%rgrp(jrg)%opos, file%rgrp(jrg)%pos
                      file%rgrp(jrg)%rec = file%rgrp(jrg)%orec
                      file%rgrp(jrg)%pos = file%rgrp(jrg)%opos
                      call set_group_record(ierr, file%rgrp(jrg), set_end)
                   endif
                endif
             endif
          enddo
       endif
       if (nw.eq.0) exit loop_cue
       if (ierr.ne.0) exit loop_cue
    enddo loop_cue
    if (ierr.eq.0) then
       nt = count(file%rgrp(0:nrg-1)%term_flag .eq. mode_terminate)
       nc = count(file%rgrp(0:nrg-1)%term_flag .eq. mode_read)
       nw = nrg - (nt + nc)
       if (nt.eq.0) then
          if (nc.eq.0) then
             stt = stt_wait
          else
             stt = stt_cont
          endif
       else if (nc.eq.0) then
          stt = stt_term
       else
          stt = stt_lack
       endif
    else
       stt = stt_error
    endif
    return
  end subroutine cue_next_read

!!!_   . set_group_record
  subroutine set_group_record &
       & (ierr, rgrp, step)
    use TOUZA_Std,only: choice
    implicit none
    integer,       intent(out)         :: ierr
    type(rgroup_t),intent(inout)       :: rgrp
    integer,       intent(in),optional :: step   ! 0: current  1: next sub  2: next filter
    integer jcur, jsub
    integer xrec
    integer f
    integer b, e, s

    ierr = 0
    if (rgrp%term_flag.eq.mode_persistent) return
    f = choice(0, step)
    if (f.eq.set_currrent) then
       jcur = rgrp%cur
       jsub = rgrp%sub
    else if (f.eq.set_sub) then
       jcur = rgrp%cur
       jsub = rgrp%sub + 1
    else if (f.eq.set_filter) then
       jcur = rgrp%cur + 1
       jsub = 0
    else if (f.eq.set_begin) then
       jcur = 0
       jsub = 0
    else if (f.eq.set_end) then
       jcur = size(rgrp%filter) - 1
       if (rgrp%filter(jcur)%num.le.0) then
          jsub = 0
       else
          b = rgrp%filter(jcur)%seq(rec_bgn)
          e = rgrp%filter(jcur)%seq(rec_end)
          s = rgrp%filter(jcur)%seq(rec_stp)
          if (s.eq.0) then
             jsub = 0
          else
             jsub = (e - b) / s
          endif
       endif
    else
       ierr = ERR_INVALID_PARAMETER
       return
    endif
    do
       ! write(*, *) 'set', jcur, jsub
       if (jcur.ge.size(rgrp%filter)) then
          rgrp%rec = rsp_exhaust
          return
       endif
       xrec = get_current_rec(rgrp%filter(jcur), jsub)
       if (xrec.ne.rsp_exhaust) exit
       jcur = jcur + 1
       jsub = 0
    enddo
    rgrp%rec = xrec
    rgrp%cur = jcur
    rgrp%sub = jsub
    ! rgrp%rec = get_current_rec(rgrp%filter(jcur), jsub)
  end subroutine set_group_record

!!!_   . get_current_rec
  integer function get_current_rec (recf, jsub, jmem, nrec) result(n)
    use TOUZA_Std,only: choice
    implicit none
    type(rec_t),intent(in)          :: recf
    integer,    intent(in)          :: jsub
    integer,    intent(in),optional :: jmem
    integer,    intent(in),optional :: nrec
    integer jm, nr
    jm = choice(0, jmem)
    nr = choice(-1, nrec)
    if (recf%num.lt.0) then
       if (jsub.gt.0) then
          n = rsp_exhaust
       else if (jm.ge.size(recf%seq)) then
          n = rsp_exhaust
       else
          n = recf%seq(jm)
          if (nr.ge.0.and.n.ge.nr) n = rsp_beyond
       endif
    else if (recf%num.gt.0) then
       ! write(*, *) associated(recf%seq), size(recf%seq)
       n = recf%seq(rec_bgn) + jsub * recf%seq(rec_stp) + jm
       if (recf%seq(rec_stp).gt.0) then
          if (recf%seq(rec_end).ge.0) then
             if (n.ge.recf%seq(rec_end)) then
                n = rsp_exhaust
             else if (nr.ge.0.and.n.ge.nr) then
                n = rsp_beyond
             endif
          else if (nr.ge.0.and.n.ge.nr) then
             n = rsp_exhaust
          endif
       else if (recf%seq(rec_stp).lt.0) then
          if (n.le.recf%seq(rec_end)) n = rsp_exhaust
       else if (jsub.ne.0) then
          n = rsp_exhaust
       endif
    else
       n = rsp_exhaust
    endif
  end function get_current_rec

!!!_   . read_file_header
  subroutine read_file_header &
       & (ierr, head, file, rec, crec, def)
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(out)   :: head(*)
    type(file_t),    intent(inout) :: file
    integer,         intent(in)    :: rec    ! target record
    integer,         intent(in)    :: crec   ! current record
    type(file_t),    intent(in)    :: def

    ierr = 0
    if (ierr.eq.0) then
       select case(file%kfmt)
       case(cfmt_ascii)
          call emulate_header_ascii(ierr, head, file, rec, crec)
       case(cfmt_binary:cfmt_cdf-1)
          call emulate_header_binary(ierr, head, file, rec, crec)
       case(cfmt_cdf)
          ierr = ERR_NOT_IMPLEMENTED
       case default
          call read_header_nio(ierr, head, file, rec, crec)
       end select
    endif
  end subroutine read_file_header

!!!_   . open_write_file
  subroutine open_write_file &
       & (ierr, file, levv)
    use TOUZA_Std,only: new_unit, sus_open, is_error_match, upcase
    use TOUZA_Std,only: check_byte_order
#if OPT_WITH_NCTCDF
    use TOUZA_Nio,only: nct_open_write
#endif
    implicit none
    integer,         intent(out)   :: ierr
    type(file_t),    intent(inout) :: file
    integer,optional,intent(in)    :: levv
    character(len=16) :: stt, pos, act
    character(len=128) :: iomsg

    ierr = 0
    iomsg = ' '

    if (file%u.lt.0) then
       file%u = new_unit()
       ierr = min(0, file%u)
       if (ierr.eq.0) then
          if (file%kfmt.eq.cfmt_ascii) then
             select case (file%mode)
             case (mode_new)
                stt = 'NEW'
                pos = 'ASIS'
             case (mode_write)
                stt = 'REPLACE'
                pos = 'ASIS'
             case (mode_append)
                stt = 'UNKNOWN'
                pos = 'APPEND'
             case default
                stt = 'UNKNOWN'
                pos = 'ASIS'
             end select
             open(UNIT=file%u, FILE=file%name, IOSTAT=ierr, &
                  & ACTION='WRITE', FORM='FORMATTED', &
                  & ACCESS='SEQUENTIAL', STATUS=trim(stt), POSITION=trim(pos))
          else if (file%kfmt.lt.cfmt_cdf) then
             select case (file%mode)
             case (mode_new)
                stt = 'N'
             case (mode_write)
                stt = 'R'
                pos = ' '
             case (mode_append)
                stt = 'U'
                pos = 'AP'
             case default
                stt = ' '
                pos = ' '
             end select
             if (is_cfmt_binary(file%kfmt)) then
                act = 'W'
                call check_byte_order(ierr, file%t, file%u, force=.TRUE., levv=levv)
             else
                act = 'RW'
             endif
             if (ierr.eq.0) then
                call sus_open &
                  & (ierr, file%u, file%name, ACTION=act, STATUS=stt, POSITION=pos, IOMSG=iomsg)
             endif
          else if (file%kfmt.eq.cfmt_cdf) then
#if OPT_WITH_NCTCDF
             select case (file%mode)
             case (mode_new)
                stt = 'N'
             case (mode_write)
                stt = 'R'
             case (mode_append)
                stt = 'U'
             case default
                stt = ' '
             end select
             call nct_open_write(ierr, file%u, file%name, status=stt, iomsg=iomsg)
#else /* not OPT_WITH_NCTCDF */
             ierr = ERR_NOT_IMPLEMENTED
#endif /* not OPT_WITH_NCTCDF */
          else
             ierr = ERR_NOT_IMPLEMENTED
          endif
          if (ierr.ne.0) then
             call message(ierr, iomsg)
             return
          endif
       endif
       file%irec = 0
    endif
  end subroutine open_write_file

!!!_   . read_file_data
  subroutine read_file_data(ierr, v, n, file)
    use TOUZA_Nio,only: nio_read_data
    implicit none
    integer,        intent(out)   :: ierr
    real(kind=KBUF),intent(out)   :: v(0:*)
    integer,        intent(in)    :: n
    type(file_t),   intent(inout) :: file

    ierr = 0
    if (ierr.eq.0) then
       select case(file%kfmt)
       case(cfmt_ascii)
          call read_data_ascii(ierr, v, n, file)
       case(cfmt_binary:cfmt_cdf-1)
          call read_data_binary(ierr, v, n, file)
       case(cfmt_cdf)
          ierr = ERR_NOT_IMPLEMENTED
       case default
          call read_data_nio(ierr, v, n, file)
       end select
    endif
    if (ierr.eq.0) file%irec = file%irec + 1

    if (ierr.ne.0) then
       if (ierr.ne.ERR_EXHAUST) then
          call message(ierr, 'failed to read ' // trim(file%name))
       endif
    endif
  end subroutine read_file_data

!!!_   . write_file_data
  subroutine write_file_data &
       & (ierr, v, n, head, file, kv)
    implicit none
    integer,         intent(out)   :: ierr
    real(kind=KBUF), intent(in)    :: v(0:*)
    integer,         intent(in)    :: n
    character(len=*),intent(in)    :: head(*)
    type(file_t),    intent(inout) :: file
    integer,         intent(in)    :: kv

    ierr = 0
    if (ierr.eq.0) then
       select case(file%kfmt)
       case(cfmt_ascii)
          call write_data_ascii(ierr, v, n, head, file, kv)
       case(cfmt_binary:cfmt_cdf-1)
          call write_data_binary(ierr, v, n, head, file, kv)
       case(cfmt_cdf)
          call write_data_cdf(ierr, v, n, head, file, kv)
       case default
          call write_data_nio(ierr, v, n, head, file)
       end select
    endif

    if (ierr.ne.0) then
       call message(ierr, 'failed to write ' // trim(file%name))
    endif
  end subroutine write_file_data

!!!_   . get_swap_switch
  logical function get_swap_switch (flag, ref) result(b)
    use TOUZA_Std,only: endian_BIG, endian_LITTLE
    implicit none
    integer,intent(in) :: flag
    integer,intent(in) :: ref   ! reference byte_order detected by check_byte_order
    select case(flag)
    case(cfmt_flag_native)
       b = .FALSE.
    case(cfmt_flag_swap)
       b = .TRUE.
    case(cfmt_flag_little)
       b = ref.eq.endian_BIG
    case(cfmt_flag_big)
       b = ref.eq.endian_LITTLE
    case default
       b = .FALSE.
    end select
  end function get_swap_switch

!!!_   . is_read_mode
  logical function is_read_mode(mode) result(b)
    implicit none
    integer,intent(in) :: mode
    b = mode.le.mode_read
  end function is_read_mode

!!!_  - gtool/nio
!!!_   . cue_read_nio
  subroutine cue_read_nio &
       & (ierr, file)
    use TOUZA_Std,only: is_error_match, sus_rseek, WHENCE_ABS
    use TOUZA_Nio,only: nio_skip_records, nio_allow_sub
    implicit none
    integer,     intent(out)   :: ierr
    type(file_t),intent(inout) :: file
    integer jrg,  mrg, nrg
    integer nsk,  nsuc
    integer irec, crec
    integer(kind=KIOFS) :: cpos, fpos
    character(len=128) ::  msg
    integer ksubm
    integer jerr

    ! compute record positions (%pos) for group to wait at %rec
    ! set file record limit (%nrec) if found

    ierr = 0
    nrg = size(file%rgrp)
    mrg = 0

    ! call show_file(ierr, file)

    crec = file%irec
    inquire(unit=file%u, pos=cpos, size=fpos, IOSTAT=ierr)
    if (ierr.ne.0) then
       ierr = ERR_PANIC
       call message(ierr, 'failed to inquire ' // trim(file%name))
       return
    endif
    if (file%nrec.lt.0) then
       if (cpos.gt.fpos) file%nrec = crec
    endif

    ! backward skipping
    do
       if (mrg.ge.nrg) exit
       ! need offseting
       ! write(*, *) file%rgrp(0:nrg-1)%rec
       ! write(*, *) file%rgrp(0:nrg-1)%pos
       jrg = -1 + maxloc(file%rgrp(0:nrg-1)%rec, 1, &
            &            file%rgrp(0:nrg-1)%pos.lt.0 .and. file%rgrp(0:nrg-1)%rec.le.file%irec)
       if (jrg.lt.0) exit
       irec = file%rgrp(jrg)%rec
       if (irec.lt.0) exit
       nsk = irec - file%irec
       call nio_skip_records(ierr, nsk, file%u, nsuc)
       if (ierr.lt.0) then
          ! try from head
          ierr = 0
          cpos = 1
          crec = 0
          exit
       endif
       if (ierr.eq.0) inquire(unit=file%u, pos=file%rgrp(jrg)%pos, IOSTAT=ierr)
       if (ierr.eq.0) file%irec = file%irec + nsk
       if (ierr.ne.0) exit
       mrg = mrg + 1
    enddo
    if (ierr.eq.0) then
       call sus_rseek(ierr, file%u, cpos, whence=WHENCE_ABS)
       file%irec = crec
    endif
    ! forward skipping
    irec = 0
    ksubm = nio_allow_sub(0, file%bigg.eq.bigg_off)
    do
       if (mrg.ge.nrg) exit
       ! need offseting
       jrg = -1 + minloc(file%rgrp(0:nrg-1)%rec, 1, &
            &            file%rgrp(0:nrg-1)%pos.lt.0 .and. file%rgrp(0:nrg-1)%rec.ge.irec)
       if (jrg.lt.0) exit
       irec = file%rgrp(jrg)%rec
       if (irec.lt.0) exit
       nsk = irec - file%irec
       call nio_skip_records(ierr, nsk, file%u, nsuc, krect=ksubm)
       if (is_error_match(ierr, ERR_EOF)) then
          ierr = 0
          file%irec = file%irec + nsuc
          file%nrec = file%irec
          exit
       endif
       if (ierr.eq.0) inquire(unit=file%u, pos=file%rgrp(jrg)%pos, IOSTAT=ierr)
       if (ierr.eq.0) file%irec = file%irec + nsk
       mrg = mrg + 1
    enddo

    if (ierr.eq.0) then
       do jrg = 0, nrg - 1
          ! write(*, *) 'cue/nio', jrg, file%rgrp(jrg)%pos, fpos
          if (file%rgrp(jrg)%pos.gt.fpos) then
             if (file%nrec.lt.0) then
                file%nrec = file%rgrp(jrg)%rec
             else if (file%nrec.ne.file%rgrp(jrg)%rec) then
                ierr = ERR_PANIC
                call message(ierr, 'panic. inconsistent records ' // trim(file%name))
                return
             endif
             file%rgrp(jrg)%pos = -1
          endif
       enddo
    endif

    if (ierr.ne.0) then
       ierr = ERR_BROKEN_RECORD
109    format('failed to cue ', A, ' (', I0, ') record=', I0)
       write(msg, 109, IOSTAT=jerr) trim(file%name), jrg, user_index_bgn(irec)
       call message(ierr, msg)
    endif
! 101 format('pos = ', 128(1x, Z8.8))
!     write(*, 101) int(file%rgrp(0:nrg-1)%pos - 1)

  end subroutine cue_read_nio

!!!_   . read_header_nio
  subroutine read_header_nio &
       & (ierr, head, file, rec, crec)
    use TOUZA_Std,only: is_error_match
    use TOUZA_Nio,only: nio_skip_records, nio_read_header
    use TOUZA_Nio,only: get_item, hi_DFMT, parse_record_fmt
    use TOUZA_Nio,only: nio_allow_sub
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(out)   :: head(*)
    type(file_t),    intent(inout) :: file
    integer,         intent(in)    :: rec
    integer,         intent(in)    :: crec
    integer nskp, nsuc, ksubm

    ierr = 0
    nskp = rec - crec
    ksubm = nio_allow_sub(0, file%bigg.eq.bigg_off)
    call nio_skip_records(ierr, nskp, file%u, nskip=nsuc, krect=ksubm)
    if (ierr.eq.0) call nio_read_header(ierr, head, file%t, file%u)
    if (ierr.eq.0) call get_item(ierr, head, file%fmt, hi_DFMT)
    ! if (ierr.eq.0) call parse_record_fmt(ierr, file%kfmt, file%fmt)

  end subroutine read_header_nio

!!!_   . get_header_lprops
  subroutine get_header_lprops(ierr, lpp, head, flag)
    use TOUZA_Std,only: choice
    use TOUZA_Nio,only: get_item, hi_AITM1, hi_AITM2, hi_AITM3
    use TOUZA_Nio,only: get_header_cprop
    implicit none
    integer,         intent(out)   :: ierr
    type(loop_t),    intent(inout) :: lpp(0:*)
    character(len=*),intent(in)    :: head(*)
    integer,optional,intent(in)    :: flag

    integer jc
    integer irange(2, 0:mcoor-1)
    integer hf

    ierr = 0
    hf = choice(hflag_nulld, flag)

    irange = 0
    do jc = 0, mcoor - 1
       call get_header_cprop(lpp(jc)%name, irange(1, jc), head, 1+jc)
    enddo
    do jc = 0, mcoor - 1
       if (irange(2, jc).le.0) then
          lpp(jc)%bgn = irange(1, jc)
          lpp(jc)%end = irange(2, jc)
          lpp(jc)%flg = loop_null
       else
          lpp(jc)%bgn = irange(1, jc) - 1
          lpp(jc)%end = irange(2, jc)
          lpp(jc)%flg = loop_normal
       endif
       if (IAND(hf, hflag_nulld).eq.0) then
          if (lpp(jc)%name.eq.' ' &
               .and. (lpp(jc)%end - lpp(jc)%bgn).eq.1) then
             lpp(jc)%flg = loop_null
          endif
       endif
       lpp(jc)%ofs = 0
       if (lpp(jc)%name.eq.' ' .and. lpp(jc)%flg.le.loop_null) then
          lpp(jc)%cyc = -1
       else
          lpp(jc)%cyc = 0
       endif
    enddo
    do jc = mcoor, lcoor - 1
       lpp(jc)%name = ' '
       lpp(jc)%bgn = 0
       lpp(jc)%end = 0
       lpp(jc)%flg = loop_unset
       lpp(jc)%ofs = 0
       lpp(jc)%cyc = -1
    enddo
  end subroutine get_header_lprops

!!!_   . put_header_lprops
  subroutine put_header_lprops(ierr, head, lpp, flag)
    use TOUZA_Nio,only: put_header_cprop
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: head(*)
    type(loop_t),    intent(in)  :: lpp(0:*)
    integer,         intent(in)  :: flag

    integer jc
    integer irange(2)

    ierr = 0

    do jc = 0, mcoor - 1
       if (lpp(jc)%bgn.eq.null_range .eqv. lpp(jc)%end.eq.null_range) then
          if (lpp(jc)%bgn.eq.null_range) then
             irange(:) = 0
          else
             irange(1) = lpp(jc)%bgn
             irange(2) = lpp(jc)%end
             if (irange(2).gt.0) irange(1) = irange(1) + 1
          endif
       else
          ierr = ERR_PANIC
          call message(ierr, 'invalid coordinate ranges ', (/jc/))
          return
       endif
       if (IAND(flag, hflag_nulld).eq.0) then
          if (lpp(jc)%name.eq.' ' &
               .and.irange(2).eq.0) then
             if (irange(1).le.0) irange(1) = 1
             irange(2) = irange(1)
          endif
       else if (lpp(jc)%flg.eq.loop_null .or. lpp(jc)%flg.eq.loop_reduce) then
          ! if null coordinate enabled
          if (irange(1).eq.irange(2) .or. irange(1).eq.irange(2) - 1) then
             irange(:) = 0
          endif
       endif
       if (ierr.eq.0) call put_header_cprop(ierr, head, lpp(jc)%name, irange, 1+jc)
    enddo
    do jc = mcoor, lcoor - 1
       if (lpp(jc)%flg.ge.loop_null) then
          ierr = ERR_PANIC
          call message(ierr, 'reach coordinate limits')
       endif
    enddo
  end subroutine put_header_lprops

!!!_   . put_header_fmt
  subroutine put_header_fmt(ierr, head, fmt)
    use TOUZA_Std,only: upcase, find_next_sep
    use TOUZA_Nio,only: put_item, hi_DFMT
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: head(*)
    character(len=*),intent(in)  :: fmt

    character(len=litem) :: fbuf
    character(len=*),parameter :: osep = urt_osep
    integer jp

    ierr = 0
    call upcase(fbuf, fmt)
    jp = find_next_sep(fbuf, osep)
    call put_item(ierr, head, fbuf(1:jp),  hi_DFMT)
  end subroutine put_header_fmt

!!!_   . read_data_nio
  subroutine read_data_nio(ierr, v, n, file)
    use TOUZA_Nio,only: nio_read_data, parse_record_fmt, nio_allow_sub
    use TOUZA_Nio,only: switch_urt_diag
    implicit none
    integer,        intent(out)   :: ierr
    real(kind=KBUF),intent(out)   :: v(0:*)
    integer,        intent(in)    :: n
    type(file_t),   intent(inout) :: file
    integer rect

    ierr = 0
    if (is_msglev_DETAIL(lev_verbose)) call switch_urt_diag(' ', 0, ulog)
    rect = nio_allow_sub(file%t, file%bigg.eq.bigg_off)
    call nio_read_data(ierr, v, n, file%h, rect, file%u)
    if (is_msglev_DETAIL(lev_verbose)) call switch_urt_diag(' ', 0, -2)

  end subroutine read_data_nio

!!!_   . write_data_nio
  subroutine write_data_nio &
       & (ierr, v, n, head, file)
    use TOUZA_Nio,only: nio_write_data, lopts, parse_urt_options
    use TOUZA_Nio,only: GFMT_URT, GFMT_MRT
    use TOUZA_Nio,only: hi_DFMT
    use TOUZA_Nio,only: switch_urt_diag, nio_allow_sub
    use TOUZA_Nio,only: parse_record_fmt
    implicit none
    integer,         intent(out)   :: ierr
    real(kind=KBUF), intent(in)    :: v(0:*)
    integer,         intent(in)    :: n
    character(len=*),intent(in)    :: head(*)
    type(file_t),    intent(inout) :: file
    integer kopts(lopts)
    integer jo
    character(len=*),parameter :: osep = urt_osep
    integer krect
    integer kfmt

    ierr = 0
    krect = nio_allow_sub(file%t, file%bigg.eq.bigg_off)
    if (ierr.eq.0) call parse_record_fmt(ierr, kfmt, file%fmt)
    if (ierr.ne.0) then
       call parse_record_fmt(ierr, kfmt, head(hi_DFMT))
    endif
    if (ierr.eq.0) then
       if (kfmt.eq.GFMT_URT .or. kfmt.eq.GFMT_MRT) then
          jo = index(file%fmt, osep)
          if (jo.gt.0) then
             call parse_urt_options(ierr, kopts, file%fmt(jo+1:))
             if (ierr.ne.0) then
                ierr = ERR_INVALID_PARAMETER
                call message(ierr, 'invalid *rt options = ' // trim(file%fmt(jo+1:)))
                return
             endif
             if (is_msglev_DETAIL(lev_verbose)) call switch_urt_diag(' ', 0, ulog)
             call nio_write_data(ierr, v, n, head, krect, file%u, kopts)
             if (is_msglev_DETAIL(lev_verbose)) call switch_urt_diag(' ', 0, -2)
          else
             call nio_write_data(ierr, v, n, head, krect, file%u)
          endif
       else
          call nio_write_data(ierr, v, n, head, krect, file%u)
       endif
    endif
  end subroutine write_data_nio

!!!_  - ascii
!!!_   . open_read_ascii
  subroutine open_read_ascii &
       & (ierr, file)
    use TOUZA_Std,only: is_error_match
    use TOUZA_Nio,only: put_header_cprop, parse_header_size, put_item
    use TOUZA_Nio,only: get_default_header, hi_DFMT, show_header
    implicit none
    integer,     intent(out)   :: ierr
    type(file_t),intent(inout) :: file

    character(len=lname) :: cname(lcoor)
    integer irange(2, lcoor)
    integer nco, jc
    integer n
    integer jerr

    ierr = 0
    file%h(:) = ' '
    cname(1:lcoor) = ' '

    if (ierr.eq.0) call parse_format_shape(nco, cname, irange, lcoor, file%fmt)
    if (nco.eq.0) then
       n = 0
       do
          read(unit=file%u, fmt=*, IOSTAT=jerr)
          if (jerr.ne.0) exit
          n = n + 1
       enddo
       ! write(*, *) 'ascii: ', n
       rewind(unit=file%u, IOSTAT=ierr)
       if (ierr.eq.0) then
          nco = 1
          irange(1:2, 1) = (/1, n/)
          if (ierr.eq.0) file%nrec = 1
       endif
    else
       ierr = min(0, nco)
    endif
    if (ierr.eq.0) call get_default_header(file%h)
    if (ierr.eq.0) call put_item(ierr, file%h, 'UR8',  hi_DFMT)
    if (ierr.eq.0) then
       do jc = 1, nco
          call put_header_cprop(ierr, file%h, cname(jc), irange(1:2, jc), jc)
       enddo
       ! call show_header(ierr, file%h)
    endif
    return
  end subroutine open_read_ascii
!!!_   . cue_read_ascii
  subroutine cue_read_ascii &
       & (ierr, file)
    use TOUZA_Std,only: is_error_match
    use TOUZA_Nio,only: parse_header_size
    ! use TOUZA_Nio,only: get_default_header, hi_DFMT, fill_header
    implicit none
    integer,     intent(out)   :: ierr
    type(file_t),intent(inout) :: file
    integer jrg, mrg, nrg
    integer m
    integer irec, nsk
    integer(kind=KIOFS) :: cpos, fpos

    ierr = 0
    nrg = size(file%rgrp)
    mrg = 0

    inquire(unit=file%u, pos=cpos, size=fpos, IOSTAT=ierr)
    if (ierr.ne.0) then
       ierr = ERR_PANIC
       call message(ierr, 'failed to inquire ' // trim(file%name))
       return
    endif
    if (file%nrec.lt.0) then
       if (cpos.gt.fpos) file%nrec = file%irec
    endif
    ! write(*, *) cpos, fpos, file%nrec

    m = parse_header_size(file%h, 0, lazy=1)
    irec = 0
    do
       if (mrg.ge.nrg) exit
       ! need offseting
       jrg = -1 + minloc(file%rgrp(0:nrg-1)%rec, 1, &
            &            file%rgrp(0:nrg-1)%pos.lt.0 .and. file%rgrp(0:nrg-1)%rec.ge.irec)
       if (jrg.lt.0) exit
       irec = file%rgrp(jrg)%rec
       nsk = irec - file%irec
       call cue_ascii_lazy(ierr, file%u, nsk, m, file%irec)
       if (is_error_match(ierr, ERR_EOF)) then
          ierr = 0
          exit
       endif
       if (ierr.eq.0) inquire(unit=file%u, pos=file%rgrp(jrg)%pos, IOSTAT=ierr)
       if (ierr.eq.0) file%irec = file%irec + nsk
       if (ierr.ne.0) exit
       mrg = mrg + 1
    enddo
    return
  end subroutine cue_read_ascii

!!!_   . cue_ascii_lazy - forward/backward record to read ascii
  subroutine cue_ascii_lazy &
       & (ierr, u, skip, mem, rec)
    use TOUZA_Std,only: KIOFS
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(in)    :: u
    integer,intent(in)    :: skip    ! record to skip (negative to backward)
    integer,intent(in)    :: mem     ! array size of single record
    integer,intent(in)    :: rec     ! current record
    integer(kind=KIOFS) :: cpos, fpos

    integer j, jj
    integer xrec

    ierr = 0
    if (skip.ge.0) then
       xrec = skip
    else
       xrec = rec + skip
       rewind(u, IOSTAT=ierr)
       if (ierr.ne.0) then
          ierr = ERR_PANIC
          call message(ierr, 'rewind failed ', (/u/))
          return
       endif
    endif
    if (ierr.eq.0) inquire(unit=u, pos=cpos, size=fpos, IOSTAT=ierr)
    if (ierr.eq.0) then
       if (cpos.gt.fpos) then
          ierr = ERR_EOF
          return
       endif
    endif
    recskip: do j = 0, xrec - 1
       do jj = 0, mem - 1
          read(unit=u, fmt=*, IOSTAT=ierr)
          if (ierr.ne.0) then
             ierr = ERR_BROKEN_RECORD
             exit recskip
          endif
       enddo
       if (ierr.eq.0) inquire(unit=u, pos=cpos, IOSTAT=ierr)
       if (ierr.eq.0) then
          if (cpos.gt.fpos) then
             ierr = ERR_EOF
             exit recskip
          endif
       endif
    enddo recskip
    ! if (ierr.eq.0) then
    !    inquire(UNIT=u, IOSTAT=ierr, SIZE=fsize, POS=jpos)
    !    if (ierr.eq.0) then
    !       if (fsize.eq.jpos) ierr = ERR_EOF
    !    endif
    ! endif
  end subroutine cue_ascii_lazy

!!!_   . emulate_header_ascii
  subroutine emulate_header_ascii &
       & (ierr, head, file, rec, crec)
    use TOUZA_Nio,only: fill_header, get_default_header, parse_header_size
    use TOUZA_Nio,only: put_item, hi_TIME
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(out)   :: head(*)
    type(file_t),    intent(inout) :: file
    integer,         intent(in)    :: rec, crec
    integer m
    integer nsk
    ierr = 0
    if (ierr.eq.0) call get_default_header(head)
    if (ierr.eq.0) call fill_header(ierr, head, file%h, 1)

    if (ierr.eq.0) call put_item(ierr, head, rec, hi_TIME)

    if (ierr.eq.0) then
       m = parse_header_size(file%h, 0, lazy=1)
       nsk = rec - crec
       call cue_ascii_lazy(ierr, file%u, nsk, m, crec)
    endif
  end subroutine emulate_header_ascii

!!!_   . read_data_ascii
  subroutine read_data_ascii(ierr, v, n, file)
    use TOUZA_Std,only: is_error_match, KIOFS
    implicit none
    integer,        intent(out)   :: ierr
    real(kind=KBUF),intent(out)   :: v(0:*)
    integer,        intent(in)    :: n
    type(file_t),   intent(inout) :: file
    integer j

    ierr = 0
    if (ierr.eq.0) then
       do j = 0, n - 1
          read(unit=file%u, fmt=*, IOSTAT=ierr) v(j)
          if (ierr.ne.0) then
             ierr = ERR_BROKEN_RECORD
             exit
          endif
       enddo
    endif
    ! if (ierr.eq.0) file%irec = file%irec + 1
    ! if (ierr.ne.0) then
    !    if (ierr.ne.ERR_EXHAUST) then
    !       call message(ierr, 'failed to read ' // trim(file%name))
    !    endif
    ! endif
  end subroutine read_data_ascii

!!!_   . write_data_ascii
  subroutine write_data_ascii &
       & (ierr, v, n, head, file, kv)
    implicit none
    integer,         intent(out)   :: ierr
    real(kind=KBUF), intent(in)    :: v(0:*)
    integer,         intent(in)    :: n
    character(len=*),intent(in)    :: head(*)
    type(file_t),    intent(inout) :: file
    integer,         intent(in)    :: kv
    integer j
    character(len=lfmt) :: fmt

    ierr = 0
    fmt = file%fmt
    if (fmt.ne.' ') then
       if (fmt(1:1).ne.'(') fmt = '(' // trim(fmt) // ')'
    endif
    select case(kv)
    case(kv_int)
       if (fmt.eq.' ') then
          do j = 0, n - 1
             write(unit=file%u, fmt=*, IOSTAT=ierr) int(v(j))
          enddo
       else
          do j = 0, n - 1
             write(unit=file%u, fmt=fmt, IOSTAT=ierr) int(v(j))
          enddo
       endif
    case(kv_flt)
       if (fmt.eq.' ') then
          do j = 0, n - 1
             write(unit=file%u, fmt=*, IOSTAT=ierr) real(v(j), kind=KFLT)
          enddo
       else
          do j = 0, n - 1
             write(unit=file%u, fmt=fmt, IOSTAT=ierr) real(v(j), kind=KFLT)
          enddo
       endif
    case(kv_dbl)
       if (fmt.eq.' ') then
          do j = 0, n - 1
             write(unit=file%u, fmt=*, IOSTAT=ierr) real(v(j), kind=KDBL)
          enddo
       else
          do j = 0, n - 1
             write(unit=file%u, fmt=fmt, IOSTAT=ierr) real(v(j), kind=KDBL)
          enddo
       endif
    end select
  end subroutine write_data_ascii
!!!_  - binary
!!!_   . open_read_binary
  subroutine open_read_binary &
       & (ierr, file)
    use TOUZA_Std,only: is_error_match
    use TOUZA_Nio,only: put_header_cprop, parse_header_size, put_item
    use TOUZA_Nio,only: get_default_header, hi_DFMT, show_header
    implicit none
    integer,     intent(out)   :: ierr
    type(file_t),intent(inout) :: file
    integer(kind=KIOFS) :: fpos
    character(len=lname) :: cname(lcoor)
    integer irange(2, lcoor)
    integer nco, jc
    integer n
    integer usize

    ierr = 0
    file%h(:) = ' '
    cname(1:lcoor) = ' '

    inquire(unit=file%u, size=fpos, IOSTAT=ierr)
    usize = usize_binary(file%kfmt)

    if (ierr.eq.0) call parse_format_shape(nco, cname, irange, lcoor, file%fmt)
    if (ierr.eq.0) then
       if (nco.eq.0) then
          n = int((fpos - 1) / usize + 1, kind=kind(n))
          nco = 1
          irange(1:2, 1) = (/1, n/)
       endif
    endif
    if (ierr.eq.0) call get_default_header(file%h)
    if (ierr.eq.0) call put_item(ierr, file%h, 'UR8',  hi_DFMT)
    if (ierr.eq.0) then
       do jc = 1, nco
          call put_header_cprop(ierr, file%h, cname(jc), irange(1:2, jc), jc)
       enddo
       ! call show_header(ierr, file%h)
    endif
    if (ierr.eq.0) then
       n = parse_header_size(file%h, 0, lazy=1)
       file%nrec = int((fpos - 1) / (usize * n) + 1, kind=kind(file%nrec))
    endif
    return
  end subroutine open_read_binary

!!!_   . cue_read_binary
  subroutine cue_read_binary &
       & (ierr, file)
    use TOUZA_Std,only: is_error_match
    use TOUZA_Nio,only: parse_header_size
    implicit none
    integer,     intent(out)   :: ierr
    type(file_t),intent(inout) :: file
    integer jrg, nrg
    integer m, usize
    integer(kind=KIOFS) :: cpos, fpos, recl

    ierr = 0
    inquire(unit=file%u, pos=cpos, size=fpos, IOSTAT=ierr)
    m = parse_header_size(file%h, 0, lazy=1)
    usize = usize_binary(file%kfmt)
    if (usize.le.0) then
       ierr = ERR_PANIC
       return
    endif
    recl = INT(m, kind=KIOFS) * usize
    nrg = size(file%rgrp)
    do jrg = 0, nrg - 1
       file%rgrp(jrg)%pos = file%rgrp(jrg)%rec * recl + 1
       if (file%rgrp(jrg)%pos.gt.fpos) file%rgrp(jrg)%pos = -1
    enddo

  end subroutine cue_read_binary

!!!_   . cue_binary_pos()
  integer(kind=KIOFS) function cue_binary_pos(kfmt, m, rec) result (n)
    use TOUZA_Std,only: get_unit_strm
    implicit none
    integer,intent(in) :: kfmt
    integer,intent(in) :: m
    integer,intent(in) :: rec
    integer :: usize
    usize = usize_binary(kfmt)
    if (usize.gt.0) then
       n = rec * (m * int(usize, kind=KIOFS)) + 1
    else
       n = -1
    endif
  end function cue_binary_pos

!!!_   . usize_binary()
  integer function usize_binary(kfmt) result (n)
    use TOUZA_Std,only: get_unit_strm
    implicit none
    integer,intent(in) :: kfmt
    select case(kfmt)
    case(cfmt_binary_i4:cfmt_binary_i4+cfmt_flags_bo-1)
       n = get_unit_strm(0)
    case(cfmt_binary_r4:cfmt_binary_r4+cfmt_flags_bo-1)
       n = get_unit_strm(real(0, kind=KFLT))
    case(cfmt_binary_r8:cfmt_binary_r8+cfmt_flags_bo-1)
       n = get_unit_strm(real(0, kind=KDBL))
    case default
       n = 0
    end select
    return
  end function usize_binary

!!!_   . emulate_header_binary
  subroutine emulate_header_binary &
       & (ierr, head, file, rec, crec)
    use TOUZA_Nio,only: fill_header, get_default_header, parse_header_size
    use TOUZA_Std,only: sus_rseek, WHENCE_ABS
    use TOUZA_Nio,only: put_item, hi_TIME
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(out)   :: head(*)
    type(file_t),    intent(inout) :: file
    integer,         intent(in)    :: rec, crec
    integer m
    integer(kind=KIOFS) :: jpos
    ierr = 0
    if (ierr.eq.0) call get_default_header(head)
    if (ierr.eq.0) call fill_header(ierr, head, file%h, 1)

    if (ierr.eq.0) call put_item(ierr, head, rec, hi_TIME)

    if (ierr.eq.0) then
       m = parse_header_size(file%h, 0, lazy=1)
       jpos = cue_binary_pos(file%kfmt, m, rec)
       call sus_rseek(ierr, file%u, jpos, whence=WHENCE_ABS)
    endif
  end subroutine emulate_header_binary

!!!_   . read_data_binary
  subroutine read_data_binary(ierr, v, n, file)
    use TOUZA_Std,only: is_error_match, KIOFS, sus_read
    implicit none
    integer,        intent(out)   :: ierr
    real(kind=KBUF),intent(out)   :: v(0:*)
    integer,        intent(in)    :: n
    type(file_t),   intent(inout) :: file
    integer usize

    integer,        allocatable,save :: bufi(:)
    real(kind=KFLT),allocatable,save :: buff(:)
    real(kind=KDBL),allocatable,save :: bufd(:)
    logical swap

    ierr = 0
    usize = usize_binary(file%kfmt)
    select case(file%kfmt)
    case(cfmt_binary_i4:cfmt_binary_i4+cfmt_flags_bo-1)
       swap = get_swap_switch(file%kfmt - cfmt_binary_i4, file%t)
       if (.not.allocated(bufi)) then
          allocate(bufi(0:n-1), STAT=ierr)
       else if (size(bufi).lt.n) then
          deallocate(bufi, STAT=ierr)
          if (ierr.eq.0) allocate(bufi(0:n-1), STAT=ierr)
       endif
       if (ierr.eq.0) call sus_read(ierr, file%u, bufi, n, swap)
       if (ierr.eq.0) then
          v(0:n-1) = real(bufi(0:n-1), kind=KBUF)
       endif
    case(cfmt_binary_r4:cfmt_binary_r4+cfmt_flags_bo-1)
       swap = get_swap_switch(file%kfmt - cfmt_binary_r4, file%t)
       if (.not.allocated(buff)) then
          allocate(buff(0:n-1), STAT=ierr)
       else if (size(buff).lt.n) then
          deallocate(buff, STAT=ierr)
          if (ierr.eq.0) allocate(buff(0:n-1), STAT=ierr)
       endif
       if (ierr.eq.0) call sus_read(ierr, file%u, buff, n, swap)
       if (ierr.eq.0) then
          v(0:n-1) = real(buff(0:n-1), kind=KBUF)
       endif
    case(cfmt_binary_r8:cfmt_binary_r8+cfmt_flags_bo-1)
       swap = get_swap_switch(file%kfmt - cfmt_binary_r8, file%t)
       if (.not.allocated(bufd)) then
          allocate(bufd(0:n-1), STAT=ierr)
       else if (size(bufd).lt.n) then
          deallocate(bufd, STAT=ierr)
          if (ierr.eq.0) allocate(bufd(0:n-1), STAT=ierr)
       endif
       if (ierr.eq.0) call sus_read(ierr, file%u, bufd, n, swap)
       if (ierr.eq.0) then
          v(0:n-1) = real(bufd(0:n-1), kind=KBUF)
       endif
    end select
  end subroutine read_data_binary

!!!_   . write_data_binary
  subroutine write_data_binary &
       & (ierr, v, n, head, file, kv)
    use TOUZA_Std,only: sus_write
    implicit none
    integer,         intent(out)   :: ierr
    real(kind=KBUF), intent(in)    :: v(0:*)
    integer,         intent(in)    :: n
    character(len=*),intent(in)    :: head(*)
    type(file_t),    intent(inout) :: file
    integer,         intent(in)    :: kv

    integer,        allocatable,save :: bufi(:)
    real(kind=KFLT),allocatable,save :: buff(:)
    real(kind=KDBL),allocatable,save :: bufd(:)
    logical swap

    ierr = 0

    select case(file%kfmt)
    case(cfmt_binary_i4:cfmt_binary_i4+cfmt_flags_bo-1)
       swap = get_swap_switch(file%kfmt - cfmt_binary_i4, file%t)
       if (ierr.eq.0) then
          if (.not.allocated(bufi)) then
             allocate(bufi(0:n-1), STAT=ierr)
          else if (size(bufi).lt.n) then
             deallocate(bufi, STAT=ierr)
             if (ierr.eq.0) allocate(bufi(0:n-1), STAT=ierr)
          endif
          if (ierr.eq.0) then
             bufi(0:n-1) = int(v(0:n-1))
          endif
          if (ierr.eq.0) call sus_write(ierr, file%u, bufi, n, swap)
       endif
    case(cfmt_binary_r4:cfmt_binary_r4+cfmt_flags_bo-1)
       swap = get_swap_switch(file%kfmt - cfmt_binary_r4, file%t)
       if (ierr.eq.0) then
          if (.not.allocated(buff)) then
             allocate(buff(0:n-1), STAT=ierr)
          else if (size(buff).lt.n) then
             deallocate(buff, STAT=ierr)
             if (ierr.eq.0) allocate(buff(0:n-1), STAT=ierr)
          endif
          if (ierr.eq.0) then
             buff(0:n-1) = real(v(0:n-1),kind=KFLT)
          endif
          if (ierr.eq.0) call sus_write(ierr, file%u, buff, n, swap)
       endif
    case(cfmt_binary_r8:cfmt_binary_r8+cfmt_flags_bo-1)
       swap = get_swap_switch(file%kfmt - cfmt_binary_r8, file%t)
       if (ierr.eq.0) then
          if (.not.allocated(bufd)) then
             allocate(bufd(0:n-1), STAT=ierr)
          else if (size(bufd).lt.n) then
             deallocate(bufd, STAT=ierr)
             if (ierr.eq.0) allocate(bufd(0:n-1), STAT=ierr)
          endif
          if (ierr.eq.0) then
             bufd(0:n-1) = real(v(0:n-1),kind=KDBL)
          endif
          if (ierr.eq.0) call sus_write(ierr, file%u, bufd, n, swap)
       endif
    end select

  end subroutine write_data_binary

!!!_   . write_data_cdf
  subroutine write_data_cdf &
       & (ierr, v, n, head, file, kv)
#if OPT_WITH_NCTCDF
    use TOUZA_Nio,only: nct_define_write, nct_write_data
#endif
    implicit none
    integer,         intent(out)   :: ierr
    real(kind=KBUF), intent(in)    :: v(0:*)
    integer,         intent(in)    :: n
    character(len=*),intent(in)    :: head(*)
    type(file_t),    intent(inout) :: file
    integer,         intent(in)    :: kv

    ierr = 0
#if OPT_WITH_NCTCDF
    call nct_define_write(ierr, file%u, head)
    if (ierr.eq.0) call nct_write_data(ierr, v, n, file%u, head)
#else /* not OPT_WITH_NCTCDF */
    ierr = ERR_NOT_IMPLEMENTED
#endif /* not OPT_WITH_NCTCDF */

  end subroutine write_data_cdf
! !!!_  - is_cfmt_nio()
!   PURE &
!   logical function is_cfmt_nio(cfmt) result(b)
!     implicit none
!     integer,intent(in) :: cfmt
!     b = cfmt .le. GFMT_END
!   end function is_cfmt_nio
!!!_ + end chak_file
end module chak_file
!!!_@ test_chak_file
#if TEST_CHAK_FILE
program test_chak_file
  use chak_lib,only: lib_init=> init
  use chak_file
  implicit none
  integer ierr
  integer l

  call lib_init(ierr)
  call test_parse_rec_argument('1,2,3')
  call test_parse_rec_argument(',2,3')
  call test_parse_rec_argument('1,2,')
  call test_parse_rec_argument(',,1,,')

  do l = 0, 1
101  format('######## ', I0)
     write(*, 101) l
     call test_parse_rec_element('3')

     call test_parse_rec_element('3/')
     call test_parse_rec_element('/3')
     call test_parse_rec_element('/3/')

     call test_parse_rec_element('///5//6/7')

     call test_parse_rec_element('/3:7/')

     call test_parse_rec_element('2:3')
     call test_parse_rec_element(':3')
     call test_parse_rec_element('2:')
     call test_parse_rec_element(':')

     call test_parse_rec_element('2:12:3')
     call test_parse_rec_element(':12:3')
     call test_parse_rec_element('2::3')
     call test_parse_rec_element('::3')

     call test_parse_rec_element('2:12:0')
     call test_parse_rec_element(':12:0')
     call test_parse_rec_element('2::0')
     call test_parse_rec_element('::0')

     call test_parse_rec_element('2:12:')
     call test_parse_rec_element(':12:')
     call test_parse_rec_element('2::')
     call test_parse_rec_element('::')

     call test_parse_rec_element('12:2:-3')
     call test_parse_rec_element(':2:-3')
     call test_parse_rec_element('12::-3')
     call test_parse_rec_element('::-3')

     call test_parse_rec_element('2:12:3:2')
     call test_parse_rec_element(':12:3:2')
     call test_parse_rec_element('2::3:2')
     call test_parse_rec_element('::3:2')

     call test_parse_rec_element('2:12:0:2')
     call test_parse_rec_element(':12:0:2')
     call test_parse_rec_element('2::0:2')
     call test_parse_rec_element('::0:2')

     call test_parse_rec_element('12:2:-3:2')
     call test_parse_rec_element(':2:-3:2')
     call test_parse_rec_element('12::-3:2')
     call test_parse_rec_element('::-3:2')

     call test_parse_rec_element('2:12::2')
     call test_parse_rec_element(':12::2')
     call test_parse_rec_element('2:::2')
     call test_parse_rec_element(':::2')

     call test_parse_rec_element('2:12:3:0')
     call test_parse_rec_element(':12:3:0')
     call test_parse_rec_element('2::3:0')
     call test_parse_rec_element('::3:0')

     call test_parse_rec_element('2:12:0:0')
     call test_parse_rec_element(':12:0:0')
     call test_parse_rec_element('2::0:0')
     call test_parse_rec_element('::0:0')

     call test_parse_rec_element('12:2:-3:0')
     call test_parse_rec_element(':2:-3:0')
     call test_parse_rec_element('12::-3:0')
     call test_parse_rec_element('::-3:0')

     call test_parse_rec_element('2:12::0')
     call test_parse_rec_element(':12::0')
     call test_parse_rec_element('2:::0')
     call test_parse_rec_element(':::0')

     call set_user_offsets(ierr, 1, 0)
  enddo
  stop
contains
  subroutine test_parse_rec_element(str)
    implicit none
    character(len=*),intent(in) :: str
    integer ierr
    type(rec_t) :: relem

    call parse_rec_element(ierr, relem, str)

101 format('parse_rec_element:',I0, ' [', A, '] ', I0, ' / ', 64(1x,I0))
    write(*, 101) ierr, trim(str), relem%num, relem%seq(:)
  end subroutine test_parse_rec_element

  subroutine test_parse_rec_argument(str)
    implicit none
    character(len=*),intent(in) :: str
    integer ierr
    type(rgroup_t) :: rgrp
    integer j

    call parse_rec_argument(ierr, rgrp, str)
101 format('### parse_rec_argument [', A, ']')
    write(*, 101) trim(str)
    do j = 0, size(rgrp%filter) - 1
       write(*, *) j, rgrp%filter(j)%num
    enddo
  end subroutine test_parse_rec_argument

end program test_chak_file
#endif  /* TEST_CHAK_FILE */
!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

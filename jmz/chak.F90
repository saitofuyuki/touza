!!!_! chak.F90 - TOUZA/Jmz swiss(CH) army knife
! Maintainer: SAITO Fuyuki
! Created: Nov 25 2021
#define TIME_STAMP 'Time-stamp: <2022/09/30 13:43:54 fuyuki chak.F90>'
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
!!!_* macros
#ifndef   TEST_CHAK
#  define TEST_CHAK 0
#endif
!!!_ + sizes
#ifndef    OPT_CHAK_FILES
#  define  OPT_CHAK_FILES    128
#endif
#ifndef    OPT_CHAK_BUFFERS
#  define  OPT_CHAK_BUFFERS  256
#endif
#ifndef    OPT_CHAK_STACKS
#  define  OPT_CHAK_STACKS   512
#endif
#ifndef    OPT_CHAK_QUEUE
#  define  OPT_CHAK_QUEUE   1024
#endif
#ifndef    OPT_CHAK_PRECISION
#  define  OPT_CHAK_PRECISION  0
#endif
!!!_ + others
#ifndef   OPT_PATH_LEN
#  define OPT_PATH_LEN 1024
#endif
#ifndef   OPT_DESC_LEN
#  define OPT_DESC_LEN 1024
#endif
#if OPT_CHAK_PRECISION == 1
#  define __KBUF KFLT
#else
#  define __KBUF KDBL
#endif
! #ifndef   HAVE_FORTRAN_IEEE_ARITHMETIC
! #  define HAVE_FORTRAN_IEEE_ARITHMETIC 0
! #endif
#define ERR_EXHAUST 1   /* special recurn code at cueing */
!!!_@ TOUZA/Jmz/chak - nio swiss army knife
program chak
!!!_ + Declaration
!!!_  - modules
  use TOUZA_Std,only: KFLT,  KDBL
  use TOUZA_Std,only: is_msglev
  use TOUZA_Std,only: msglev_normal
  use TOUZA_Std,only: is_msglev_DETAIL, is_msglev_NORMAL, is_msglev_INFO, is_msglev_DEBUG
  use TOUZA_Nio,only: litem, nitem
! #if HAVE_FORTRAN_IEEE_ARITHMETIC
!   use IEEE_ARITHMETIC
! #endif
  implicit none
!!!_  - parameters
  integer,parameter :: KBUF = __KBUF
  integer,parameter :: kv_null = 0
  integer,parameter :: kv_int  = 1
  integer,parameter :: kv_flt  = 2
  integer,parameter :: kv_dbl  = 3

  integer,parameter :: lcoor = 3

  integer,parameter :: ERR_NO_CANDIDATE = 1
  integer,parameter :: ERR_FINISHED = 2

  real(kind=KBUF),parameter :: ZERO  = 0.0_KBUF
  real(kind=KBUF),parameter :: ONE   = 1.0_KBUF

  real(kind=KBUF),parameter :: TRUE  = 1.0_KBUF
  real(kind=KBUF),parameter :: FALSE = 0.0_KBUF

! #if HAVE_FORTRAN_IEEE_ARITHMETIC
!   real(kind=KBUF) :: UNDEF = - HUGE(ZERO)
! #else
  real(kind=KBUF),parameter :: UNDEF = - HUGE(ZERO)
! #endif
  real(kind=KBUF),parameter :: ULIMIT = + HUGE(ZERO)
  real(kind=KBUF),parameter :: LLIMIT = - HUGE(ZERO)

  character(len=*),parameter :: paramd = '='
!!!_  - operators
  integer,parameter :: lopr = 512

  !! system
  character(len=*),parameter :: str_INPUT = ' INPUT'
  character(len=*),parameter :: str_OUTPUT = ' OUTPUT'
  character(len=*),parameter :: str_ANCHOR = ' ANCHOR'
  !! output
  character(len=*),parameter :: str_OUTPUT_POP = '='
  character(len=*),parameter :: str_OUTPUT_KEEP = ':='
  !! anchor
  character(len=*),parameter :: str_MARK = 'MARK'
  character(len=*),parameter :: str_STOP = 'STOP'
  character(len=*),parameter :: str_GO = 'GO'
  !! stack
  character(len=*),parameter :: str_DUP = 'DUP'
  character(len=*),parameter :: str_POP = 'POP'
  character(len=*),parameter :: str_EXCH = 'EXCH'
  character(len=*),parameter :: str_NOP = 'NOP'
  character(len=*),parameter :: str_DIST = 'DIST'
  character(len=*),parameter :: str_INSERT = 'INSERT'
  character(len=*),parameter :: str_REPEAT = 'REPEAT'
  character(len=*),parameter :: str_FLUSH = 'FLUSH'
  !! queue
  character(len=*),parameter :: str_ITER = 'ITER'
  character(len=*),parameter :: str_CUM = 'CUM'
  character(len=*),parameter :: str_MAP = 'MAP'
  !! unary
  character(len=*),parameter :: str_NEG = 'NEG'
  character(len=*),parameter :: str_INV = 'INV'
  character(len=*),parameter :: str_ABS = 'ABS'
  character(len=*),parameter :: str_SQR = 'SQR'
  character(len=*),parameter :: str_SQRT = 'SQRT'
  character(len=*),parameter :: str_SIGN = 'SIGN'
  character(len=*),parameter :: str_ZSIGN = 'ZSIGN'
  character(len=*),parameter :: str_FLOOR = 'FLOOR'
  character(len=*),parameter :: str_CEIL = 'CEIL'
  character(len=*),parameter :: str_ROUND = 'ROUND'
  character(len=*),parameter :: str_TRUNC = 'TRUNC'
  character(len=*),parameter :: str_INT = 'INT'
  character(len=*),parameter :: str_EXP = 'EXP'
  character(len=*),parameter :: str_LOG = 'LOG'
  character(len=*),parameter :: str_LOG10 = 'LOG10'
  character(len=*),parameter :: str_SIN = 'SIN'
  character(len=*),parameter :: str_COS = 'COS'
  character(len=*),parameter :: str_TAN = 'TAN'
  character(len=*),parameter :: str_TANH = 'TANH'
  character(len=*),parameter :: str_ASIN = 'ASIN'
  character(len=*),parameter :: str_ACOS = 'ACOS'
  character(len=*),parameter :: str_EXPONENT = 'EXPONENT'
  character(len=*),parameter :: str_FRACTION = 'FRACTION'
  !! bool
  character(len=*),parameter :: str_NOT = 'NOT'
  character(len=*),parameter :: str_BOOL = 'BOOL'
  character(len=*),parameter :: str_BIN = 'BIN'
  character(len=*),parameter :: str_EQ = 'EQ'
  character(len=*),parameter :: str_NE = 'NE'
  character(len=*),parameter :: str_LT = 'LT'
  character(len=*),parameter :: str_GT = 'GT'
  character(len=*),parameter :: str_LE = 'LE'
  character(len=*),parameter :: str_GE = 'GE'
  character(len=*),parameter :: str_EQU = 'EQU'
  character(len=*),parameter :: str_NEU = 'NEU'
  character(len=*),parameter :: str_LTU = 'LTU'
  character(len=*),parameter :: str_GTU = 'GTU'
  character(len=*),parameter :: str_LEU = 'LEU'
  character(len=*),parameter :: str_GEU = 'GEU'
  !! binary
  character(len=*),parameter :: str_AND = 'AND'
  character(len=*),parameter :: str_MASK = 'MASK'
  character(len=*),parameter :: str_ADD = 'ADD'
  character(len=*),parameter :: str_SUB = 'SUB'
  character(len=*),parameter :: str_MUL = 'MUL'
  character(len=*),parameter :: str_DIV = 'DIV'
  character(len=*),parameter :: str_IDIV = 'IDIV'
  character(len=*),parameter :: str_MOD = 'MOD'
  character(len=*),parameter :: str_POW = 'POW'
  character(len=*),parameter :: str_ATAN2 = 'ATAN2'
  character(len=*),parameter :: str_SCALE = 'SCALE'
  character(len=*),parameter :: str_MIN = 'MIN'
  character(len=*),parameter :: str_MAX = 'MAX'
  character(len=*),parameter :: str_EQF = 'EQF'
  character(len=*),parameter :: str_NEF = 'NEF'
  character(len=*),parameter :: str_LTF = 'LTF'
  character(len=*),parameter :: str_GTF = 'GTF'
  character(len=*),parameter :: str_LEF = 'LEF'
  character(len=*),parameter :: str_GEF = 'GEF'
  !! lazy
  character(len=*),parameter :: str_OR = 'OR'
  character(len=*),parameter :: str_XOR = 'XOR'
  character(len=*),parameter :: str_LMASK = 'LMASK'
  character(len=*),parameter :: str_LADD = 'LADD'
  character(len=*),parameter :: str_LSUB = 'LSUB'
  character(len=*),parameter :: str_LMUL = 'LMUL'
  character(len=*),parameter :: str_LDIV = 'LDIV'
  character(len=*),parameter :: str_LMIN = 'LMIN'
  character(len=*),parameter :: str_LMAX = 'LMAX'
  !! other
  !! reduction
  character(len=*),parameter :: str_AVR = 'AVR'
  character(len=*),parameter :: str_COUNT = 'COUNT'
  !! header
  character(len=*),parameter :: str_DFMT = 'DFMT'
  character(len=*),parameter :: str_ITEM = 'ITEM'
  character(len=*),parameter :: str_UNIT = 'UNIT'
  character(len=*),parameter :: str_TITLE = 'TITLE'
  character(len=*),parameter :: str_EDIT = 'EDIT'
  character(len=*),parameter :: str_TSEL = 'T'
  !! buffer
  character(len=*),parameter :: str_TAG = 'TAG'
  character(len=*),parameter :: str_COOR = 'COOR'
  character(len=*),parameter :: str_C0 = 'C0'
  character(len=*),parameter :: str_C1 = 'C1'
  character(len=*),parameter :: str_C2 = 'C2'
  character(len=*),parameter :: str_C3 = 'C3'
  character(len=*),parameter :: str_X = 'X'
  character(len=*),parameter :: str_Y = 'Y'
  character(len=*),parameter :: str_Z = 'Z'
  character(len=*),parameter :: str_LON = 'LON'
  character(len=*),parameter :: str_LAT = 'LAT'
  character(len=*),parameter :: str_LEV = 'LEV'

  !! id
  integer,parameter :: grp_system_bgn = 0
  integer,parameter :: opr_INPUT = 0
  integer,parameter :: opr_OUTPUT = 1
  integer,parameter :: opr_ANCHOR = 2
  integer,parameter :: grp_system_end = 3

  integer,parameter :: grp_output_bgn = 3
  integer,parameter :: opr_OUTPUT_POP = 3
  integer,parameter :: opr_OUTPUT_KEEP = 4
  integer,parameter :: grp_output_end = 5

  integer,parameter :: grp_anchor_bgn = 5
  integer,parameter :: opr_MARK = 5
  integer,parameter :: opr_STOP = 6
  integer,parameter :: opr_GO = 7
  integer,parameter :: grp_anchor_end = 8

  integer,parameter :: grp_stack_bgn = 8
  integer,parameter :: opr_DUP = 8
  integer,parameter :: opr_POP = 9
  integer,parameter :: opr_EXCH = 10
  integer,parameter :: opr_NOP = 11
  integer,parameter :: opr_DIST = 12
  integer,parameter :: opr_INSERT = 13
  integer,parameter :: opr_REPEAT = 14
  integer,parameter :: opr_FLUSH = 15
  integer,parameter :: grp_stack_end = 16

  integer,parameter :: grp_queue_bgn = 16
  integer,parameter :: opr_ITER = 16
  integer,parameter :: opr_CUM = 17
  integer,parameter :: opr_MAP = 18
  integer,parameter :: grp_queue_end = 19

  integer,parameter :: grp_unary_bgn = 19
  integer,parameter :: opr_NEG = 19
  integer,parameter :: opr_INV = 20
  integer,parameter :: opr_ABS = 21
  integer,parameter :: opr_SQR = 22
  integer,parameter :: opr_SQRT = 23
  integer,parameter :: opr_SIGN = 24
  integer,parameter :: opr_ZSIGN = 25
  integer,parameter :: opr_FLOOR = 26
  integer,parameter :: opr_CEIL = 27
  integer,parameter :: opr_ROUND = 28
  integer,parameter :: opr_TRUNC = 29
  integer,parameter :: opr_INT = 30
  integer,parameter :: opr_EXP = 31
  integer,parameter :: opr_LOG = 32
  integer,parameter :: opr_LOG10 = 33
  integer,parameter :: opr_SIN = 34
  integer,parameter :: opr_COS = 35
  integer,parameter :: opr_TAN = 36
  integer,parameter :: opr_TANH = 37
  integer,parameter :: opr_ASIN = 38
  integer,parameter :: opr_ACOS = 39
  integer,parameter :: opr_EXPONENT = 40
  integer,parameter :: opr_FRACTION = 41
  integer,parameter :: grp_unary_end = 42

  integer,parameter :: grp_bool_bgn = 42
  integer,parameter :: opr_NOT = 42
  integer,parameter :: opr_BOOL = 43
  integer,parameter :: opr_BIN = 44
  integer,parameter :: opr_EQ = 45
  integer,parameter :: opr_NE = 46
  integer,parameter :: opr_LT = 47
  integer,parameter :: opr_GT = 48
  integer,parameter :: opr_LE = 49
  integer,parameter :: opr_GE = 50
  integer,parameter :: opr_EQU = 51
  integer,parameter :: opr_NEU = 52
  integer,parameter :: opr_LTU = 53
  integer,parameter :: opr_GTU = 54
  integer,parameter :: opr_LEU = 55
  integer,parameter :: opr_GEU = 56
  integer,parameter :: grp_bool_end = 57

  integer,parameter :: grp_binary_bgn = 57
  integer,parameter :: opr_AND = 57
  integer,parameter :: opr_MASK = 58
  integer,parameter :: opr_ADD = 59
  integer,parameter :: opr_SUB = 60
  integer,parameter :: opr_MUL = 61
  integer,parameter :: opr_DIV = 62
  integer,parameter :: opr_IDIV = 63
  integer,parameter :: opr_MOD = 64
  integer,parameter :: opr_POW = 65
  integer,parameter :: opr_ATAN2 = 66
  integer,parameter :: opr_SCALE = 67
  integer,parameter :: opr_MIN = 68
  integer,parameter :: opr_MAX = 69
  integer,parameter :: opr_EQF = 70
  integer,parameter :: opr_NEF = 71
  integer,parameter :: opr_LTF = 72
  integer,parameter :: opr_GTF = 73
  integer,parameter :: opr_LEF = 74
  integer,parameter :: opr_GEF = 75
  integer,parameter :: grp_binary_end = 76

  integer,parameter :: grp_lazy_bgn = 76
  integer,parameter :: opr_OR = 76
  integer,parameter :: opr_XOR = 77
  integer,parameter :: opr_LMASK = 78
  integer,parameter :: opr_LADD = 79
  integer,parameter :: opr_LSUB = 80
  integer,parameter :: opr_LMUL = 81
  integer,parameter :: opr_LDIV = 82
  integer,parameter :: opr_LMIN = 83
  integer,parameter :: opr_LMAX = 84
  integer,parameter :: grp_lazy_end = 85

  integer,parameter :: grp_other_bgn = 85
  integer,parameter :: grp_other_end = 85

  integer,parameter :: grp_reduction_bgn = 85
  integer,parameter :: opr_AVR = 85
  integer,parameter :: opr_COUNT = 86
  integer,parameter :: grp_reduction_end = 87

  integer,parameter :: grp_header_bgn = 87
  integer,parameter :: opr_DFMT = 87
  integer,parameter :: opr_ITEM = 88
  integer,parameter :: opr_UNIT = 89
  integer,parameter :: opr_TITLE = 90
  integer,parameter :: opr_EDIT = 91
  integer,parameter :: opr_TSEL = 92
  integer,parameter :: grp_header_end = 93

  integer,parameter :: grp_buffer_bgn = 93
  integer,parameter :: opr_TAG = 93
  integer,parameter :: opr_COOR = 94
  integer,parameter :: opr_C0 = 95
  integer,parameter :: opr_C1 = 96
  integer,parameter :: opr_C2 = 97
  integer,parameter :: opr_C3 = 98
  integer,parameter :: opr_X = 99
  integer,parameter :: opr_Y = 100
  integer,parameter :: opr_Z = 101
  integer,parameter :: opr_LON = 102
  integer,parameter :: opr_LAT = 103
  integer,parameter :: opr_LEV = 104
  integer,parameter :: grp_buffer_end = 105

  type opr_t
     integer :: push = -1
     integer :: pop = -1
     integer :: entr = -1
  end type opr_t
  type(opr_t) :: oprop(0:lopr-1)
  integer :: mopr = 0

!!!_  - variables
  integer ierr
!!!_  - files
  integer,save      :: rcount = 0   ! read file count
  integer,save      :: wcount = 0   ! write file count
  integer,save      :: wrefh  = -1  ! write file reference input
  integer,save      :: mfile=0
  integer,parameter :: lfile=OPT_CHAK_FILES
  integer,parameter :: lpath=OPT_PATH_LEN
  integer,parameter :: lname=litem*4
  integer,parameter :: ldesc=OPT_DESC_LEN
  integer,parameter :: lfmt =litem*4
  type file_t
     character(len=lpath) :: name
     integer              :: u
     integer              :: t
     integer              :: irec, nrec
     integer              :: mode      ! access mode
     integer              :: bh = -1
     character(len=litem) :: h(nitem)
     integer              :: jrf = -1  ! current record filter
     type(loop_t),pointer :: recf(:)   ! record filter
     character(len=lfmt)  :: fmt
  end type file_t
  type(file_t),target :: ofile(0:lfile-1)
  type(file_t),target :: def_read
  type(file_t),target :: def_write
  ! access mode
  integer,parameter :: mode_unset = -999
  integer,parameter :: mode_cycle = -3       ! rewind if eof
  integer,parameter :: mode_persistent = -2  ! keep final if eof
  integer,parameter :: mode_terminate = -1   ! terminate if eof
  integer,parameter :: mode_read = 0

  integer,parameter :: mode_new = 1          ! error if exist
  integer,parameter :: mode_write = 2        ! force overwrite
  integer,parameter :: mode_append = 3       ! append

  integer,parameter :: stt_none   = 0
  integer,parameter :: stt_free   = 1
  integer,parameter :: stt_locked = -1

  ! domain compromise mode for non-unary operations
  integer,parameter :: cmode_null      = 0
  integer,parameter :: cmode_inclusive = 1
  integer,parameter :: cmode_intersect = 2
  integer,parameter :: cmode_first     = 3

!!!_  - loop property
  type loop_t
     integer :: bgn = -1
     integer :: end = -1
     integer :: stp = -1
  end type loop_t
!!!_  - buffers
  integer,save      :: lcount = 0   ! literal count
  integer,save      :: consts=0     ! predefine constants
  integer,save      :: mbuffer=0
  integer,parameter :: lbuffer=OPT_CHAK_BUFFERS
  type buffer_t
     character(len=lname)    :: name          ! buffer name
     character(len=ldesc)    :: desc          ! description
     integer                 :: m = -1        ! m=0 as free
     integer                 :: k = kv_null
     integer                 :: stt
     integer                 :: ci(lcoor)
     character(len=litem)    :: cn(lcoor)
     type(loop_t)            :: lpp(lcoor)
     real(kind=KBUF)         :: undef
     real(kind=KBUF),pointer :: vd(:)
  end type buffer_t
  type(buffer_t),target :: obuffer(0:lbuffer-1)
!!!_  - buffer stack
  integer           :: mstack
  integer,parameter :: lstack=OPT_CHAK_STACKS
  integer           :: bstack(0:lstack-1)
!!!_  - argument queue
  type queue_t
     integer :: term = -1            ! term handle
     integer :: nopr                 ! number of operands
     integer :: iter                 ! number of iterates etc
     integer :: cmode = cmode_null   ! operation mode
     character(len=ldesc) :: desci
     character(len=ldesc) :: desco
     integer,pointer      :: lefth(:)  ! result buffer handle(s)
  end type queue_t
  integer           :: mqueue
  integer,parameter :: lqueue=OPT_CHAK_QUEUE
  type(queue_t)     :: aqueue(0:lqueue-1)
!!!_  - table
  integer,save :: htopr = -1
!!!_  - handle offsets and kinds
  integer,parameter :: hk_error = -1
  integer,parameter :: hk_opr = 0
  integer,parameter :: hk_file = 1
  integer,parameter :: hk_buffer = 2
  integer,parameter :: hk_anchor = 3
  integer,parameter :: hk_overflow = 4

  integer,parameter :: lmodule = max(lopr, lfile, lbuffer) * 2
  integer,parameter :: ofs_opr    = lmodule * hk_opr
  integer,parameter :: ofs_file   = lmodule * hk_file
  integer,parameter :: ofs_buffer = lmodule * hk_buffer
  integer,parameter :: ofs_anchor = lmodule * hk_anchor

  integer,parameter :: max_operands = 2
!!!_  - global flags
  integer :: def_cmode = cmode_inclusive   ! default compromise mode
  integer :: global_offset_bgn = 0     ! begin-index offset (user-friendly)
  integer :: global_offset_end = 0     ! end-index offset (user-friendly)

  integer lev_verbose, dbgv, stdv

  character(len=32) :: afmt_int = '(I0)'
  character(len=32) :: afmt_flt = '(E16.9)'
  character(len=32) :: afmt_dbl = '(E16.9)'
!!!_  - misc
  integer :: ulog = -1
  integer :: uerr = -1
  integer irecw
!!!_ + Body
  ierr = 0

  lev_verbose = 0
  dbgv = -1
  stdv = -1
  mqueue = 0

  if (ierr.eq.0) call init(ierr, stdv, dbgv)
  if (ierr.eq.0) call parse_args(ierr)

#if TEST_CHAK
  call test_tweak_coordinates(ierr)
#endif /* TEST_CHAK */

  irecw = 0
  do
     if (ierr.eq.0) call batch_operation(ierr, irecw, lev_verbose)
     if (ierr.ne.0) exit
     irecw = irecw + 1
  enddo
  ierr = min(0, ierr)

  if (ierr.eq.0) call finalize(ierr)
  if (ierr.ne.0) then
     write(*, *) 'exit = ', ierr
  endif
  stop
!!!_ + Subroutines
contains
!!!_  - init
  subroutine init(ierr, stdv, dbgv)
    use TOUZA_Nio,only: nio_init=>init, nr_init
    use TOUZA_Std,only: env_init, MPI_COMM_NULL
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: stdv, dbgv
    ierr = 0
    if (ierr.eq.0) call env_init(ierr, levv=stdv, icomm=MPI_COMM_NULL)
    if (ierr.eq.0) call register_operators(ierr)
    if (ierr.eq.0) call register_predefined(ierr)
    if (ierr.eq.0) call nio_init(ierr, levv=dbgv, stdv=stdv)
    if (ierr.eq.0) call nr_init(ierr, lazy=+1)

    if (ierr.eq.0) call reset_file(ierr, def_read,  ' ', mode_terminate)
    if (ierr.eq.0) call reset_file(ierr, def_write, ' ', mode_new)
! #if HAVE_FORTRAN_IEEE_ARITHMETIC
!     UNDEF = IEEE_VALUE(ZERO, IEEE_QUIET_NAN)
! #endif
  end subroutine init
!!!_  - finalize
  subroutine finalize(ierr, u)
    use TOUZA_Std,only: env_finalize, htb_finalize, htb_diag
    use TOUZA_Nio,only: nio_diag=>diag, nio_finalize=>finalize
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u

    ierr = 0
    if (is_msglev_INFO(lev_verbose)) then
       if (ierr.eq.0) call show_queue(ierr, u)
       if (ierr.eq.0) call show_files(ierr, u)
       if (ierr.eq.0) call show_buffers(ierr, u)
    endif
    if (ierr.eq.0) call nio_diag(ierr, levv=dbgv)
    if (ierr.eq.0) call htb_diag(ierr, levv=dbgv)
    if (ierr.eq.0) call nio_finalize(ierr, levv=dbgv)
    if (ierr.eq.0) call htb_finalize(ierr, levv=dbgv)
    if (ierr.eq.0) call env_finalize(ierr, levv=dbgv)
  end subroutine finalize

!!!_  - message
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
    utmp = choice(ulog, u)
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
          if (utmp.ge.0) then
             write(utmp, 102) ierr, trim(txt)
          else if (utmp.eq.-1) then
             write(*,    102) ierr, trim(txt)
          endif
       else
          if (utmp.ge.0) then
             write(utmp, 101) repeat(' ', skp), trim(txt)
          else if (utmp.eq.-1) then
             write(*,    101) repeat(' ', skp), trim(txt)
          endif
       endif
    endif
  end subroutine message
!!!_  - log arrays
!!!_   . show_queue
  subroutine show_queue(ierr, u)
    use TOUZA_Std,only: choice, join_list
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer utmp
    integer j
    integer js
    integer hk
    character(len=128) :: buf
    character(len=lpath) :: str
    character cmd

    ierr = 0
    utmp = choice(-1, u)
101 format('operator:', A,           ' -', I0, '+', I0, 1x, A, '>> ', A)
102 format('operator:', A, '(', I0, ') -', I0, '+', I0, 1x, A, '>> ', A)
111 format('file:', A)
121 format('stack:', A)
    js = 0
    do j = 0, min(mqueue, lqueue) - 1
       if (ierr.eq.0) then
          select case(aqueue(j)%cmode)
          case(cmode_first)
             cmd = 'l'
          case(cmode_inclusive)
             cmd = 'i'
          case(cmode_intersect)
             cmd = 'x'
          case default
             cmd = ' '
          end select
       endif
       if (ierr.eq.0) then
          hk = handle_type(aqueue(j)%term)
          select case(hk)
          case(hk_opr)
             call query_opr_name(ierr, buf, aqueue(j)%term)
             if (ierr.eq.0) then
                if (aqueue(j)%iter.ne.0) then
                   write(str, 102) trim(buf), aqueue(j)%iter, aqueue(j)%nopr, size(aqueue(j)%lefth), cmd, trim(aqueue(j)%desco)
                else
                   write(str, 101) trim(buf),                 aqueue(j)%nopr, size(aqueue(j)%lefth), cmd, trim(aqueue(j)%desco)
                endif
             endif
          case(hk_file)
             write(str, 111) trim(aqueue(j)%desco)
          case default
             write(str, 121) trim(aqueue(j)%desco)
          end select
       endif
201    format('queue[', I0, '] ', I0, 1x, A)
       if (ierr.eq.0) then
          js = js - aqueue(j)%nopr + size(aqueue(j)%lefth)
          if (utmp.ge.0) then
             write(utmp, 201) j + global_offset_bgn, js, trim(str)
          else if (utmp.eq.-1) then
             write(*,    201) j + global_offset_bgn, js, trim(str)
          endif
       endif
    enddo
  end subroutine show_queue

!!!_   . show_files
  subroutine show_files(ierr, u)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer utmp
    integer j, h, jb
    character(len=1) :: cm
    character(len=128) :: bname

    ierr = 0
    utmp = choice(-1, u)

    do j = 0, min(mfile, lfile) - 1
201    format('file:', A, 1x, I0, 1x, A1, 1x, I0, '/', I0, 1x, A)
       h = file_i2handle(j)
       select case(ofile(j)%mode)
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
       jb = buf_h2item(ofile(j)%bh)
       bname = obuffer(jb)%name
       if (utmp.ge.0) then
          write(utmp, 201) trim(bname), ofile(j)%u, cm, ofile(j)%irec, ofile(j)%nrec, trim(ofile(j)%name)
       else if (utmp.eq.-1) then
          write(*,    201) trim(bname), ofile(j)%u, cm, ofile(j)%irec, ofile(j)%nrec, trim(ofile(j)%name)
       endif
    enddo
  end subroutine show_files

!!!_   . show_buffers
  subroutine show_buffers(ierr, u, levv)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer utmp, lv
    integer j, h
    character(len=128) :: bname
    character(len=128) :: txt
    character(len=1) :: cs
    character(len=1),parameter :: CSTT(stt_locked:stt_free) = (/'L', 'N', 'F'/)
    integer jbgn

    ierr = 0
    utmp = choice(-1, u)
    lv = choice(lev_verbose, levv)

    if (is_msglev_DEBUG(lv)) then
       jbgn = 0
    else
       jbgn = consts
    endif

    do j = jbgn, min(mbuffer, lbuffer) - 1
101    format('I:', I0)
102    format('F:', I0)
103    format('D:', I0)
109    format('X/', I0)
       select case (obuffer(j)%k)
       case (kv_int)
          write(txt, 101) obuffer(j)%m
       case (kv_flt)
          write(txt, 102) obuffer(j)%m
       case (kv_dbl)
          write(txt, 103) obuffer(j)%m
       case default
          write(txt, 109) obuffer(j)%k
       end select
       cs = cstt(min(stt_free, max(stt_locked, obuffer(j)%stt)))

201    format(A, 1x, A, 1x, A, 1x, E10.3)
211    format('buffer:', I0)
212    format('buffer:', A)
       h = buf_i2handle(j)
       if (obuffer(j)%name.eq.' ') then
          write(bname, 211) h
       else
          write(bname, 212) trim(obuffer(j)%name)
       endif
       if (utmp.ge.0) then
          write(utmp, 201) trim(bname), cs, trim(txt), obuffer(j)%undef
       else if (utmp.eq.-1) then
          write(*,    201) trim(bname), cs, trim(txt), obuffer(j)%undef
       endif
    enddo
  end subroutine show_buffers

!!!_   . show_stack
  subroutine show_stack(ierr, u, levv)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: u
    integer,intent(in),optional :: levv
    integer utmp
    integer js, jb, m
    character(len=128) :: pfx
    character(len=lpath) :: str
    character(len=lpath) :: desc
    character(len=128)   :: domain
    integer lv
    integer alev
    integer jas

    ierr = 0
    lv = choice(lev_verbose, levv)
    utmp = choice(-1, u)
201 format('stack[', I0, ']')
    jas = global_offset_bgn
    do js = 0, min(mstack, lstack) - 1
       if (ierr.eq.0) call get_obj_string(ierr, str, bstack(js), lv)
       alev = anchor_h2level(bstack(js))
       if (alev.ge.0) then
211       format('--- ', I0)
212       format('---')
213       format('===')
          if (alev.gt.1) then
             write(str, 211) alev
          else if (alev.eq.1) then
             write(str, 212)
          else
             write(str, 213)
          endif
          call message(ierr, trim(str), u=utmp, indent=6)
       else
          jb = buf_h2item(bstack(js))
          if (jb.ge.0) then
             m = obuffer(jb)%m
          else
             m = -1
          endif
          if (obuffer(jb)%desc.ne.' ') then
202          format(1x, '<', A, '>')
             write(desc, 202) trim(obuffer(jb)%desc)
             str = trim(str) // trim(desc)
          endif
          if (is_msglev_DETAIL(lv)) then
             call get_domain_string(ierr, domain, obuffer(jb)%cn, obuffer(jb)%lpp)
             str = trim(str) // ' ' // trim(domain)
          endif
          write(pfx, 201) jas
          call message(ierr, trim(pfx) // ' ' // trim(str), u=utmp, indent=4)
          jas = jas + 1
       endif
    enddo
  end subroutine show_stack

!!!_  - operator table
!!!_   . register_operators
  subroutine register_operators(ierr)
    use TOUZA_Std,only: htb_init, new_htable, reg_entry
    implicit none
    integer,intent(out) :: ierr

    ierr = 0
    call htb_init(ierr)
    htopr = new_htable('operators', 0, nstt=1, mem=lopr)
    ierr = min(0, htopr)

    if (ierr.eq.0) call reg_opr_prop(ierr, opr_INPUT, str_INPUT, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_OUTPUT, str_OUTPUT, 1, 0)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_ANCHOR, str_ANCHOR)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_OUTPUT_POP, str_OUTPUT_POP, 1, 0)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_OUTPUT_KEEP, str_OUTPUT_KEEP, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_MARK, str_MARK)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_STOP, str_STOP)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_GO, str_GO)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_DUP, str_DUP, 1, 2)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_POP, str_POP, 1, 0)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_EXCH, str_EXCH, 2, 2)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_NOP, str_NOP, 0, 0)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_DIST, str_DIST)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_INSERT, str_INSERT)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_REPEAT, str_REPEAT)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_FLUSH, str_FLUSH)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_ITER, str_ITER)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_CUM, str_CUM)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_MAP, str_MAP)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_NEG, str_NEG, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_INV, str_INV, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_ABS, str_ABS, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_SQR, str_SQR, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_SQRT, str_SQRT, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_SIGN, str_SIGN, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_ZSIGN, str_ZSIGN, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_FLOOR, str_FLOOR, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_CEIL, str_CEIL, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_ROUND, str_ROUND, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_TRUNC, str_TRUNC, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_INT, str_INT, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_EXP, str_EXP, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_LOG, str_LOG, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_LOG10, str_LOG10, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_SIN, str_SIN, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_COS, str_COS, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_TAN, str_TAN, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_TANH, str_TANH, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_ASIN, str_ASIN, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_ACOS, str_ACOS, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_EXPONENT, str_EXPONENT, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_FRACTION, str_FRACTION, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_NOT, str_NOT, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_BOOL, str_BOOL, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_BIN, str_BIN, 1, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_EQ, str_EQ, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_NE, str_NE, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_LT, str_LT, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_GT, str_GT, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_LE, str_LE, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_GE, str_GE, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_EQU, str_EQU, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_NEU, str_NEU, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_LTU, str_LTU, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_GTU, str_GTU, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_LEU, str_LEU, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_GEU, str_GEU, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_AND, str_AND, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_MASK, str_MASK, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_ADD, str_ADD, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_SUB, str_SUB, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_MUL, str_MUL, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_DIV, str_DIV, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_IDIV, str_IDIV, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_MOD, str_MOD, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_POW, str_POW, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_ATAN2, str_ATAN2, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_SCALE, str_SCALE, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_MIN, str_MIN, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_MAX, str_MAX, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_EQF, str_EQF, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_NEF, str_NEF, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_LTF, str_LTF, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_GTF, str_GTF, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_LEF, str_LEF, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_GEF, str_GEF, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_OR, str_OR, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_XOR, str_XOR, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_LMASK, str_LMASK, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_LADD, str_LADD, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_LSUB, str_LSUB, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_LMUL, str_LMUL, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_LDIV, str_LDIV, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_LMIN, str_LMIN, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_LMAX, str_LMAX, 2, 1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_AVR, str_AVR)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_COUNT, str_COUNT)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_DFMT, str_DFMT)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_ITEM, str_ITEM)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_UNIT, str_UNIT)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_TITLE, str_TITLE)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_EDIT, str_EDIT)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_TSEL, str_TSEL)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_TAG, str_TAG)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_COOR, str_COOR)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_C0, str_C0)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_C1, str_C1)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_C2, str_C2)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_C3, str_C3)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_X, str_X)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_Y, str_Y)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_Z, str_Z)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_LON, str_LON)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_LAT, str_LAT)
    if (ierr.eq.0) call reg_opr_prop(ierr, opr_LEV, str_LEV)

  end subroutine register_operators
!!!_   . reg_opr_prop
  subroutine reg_opr_prop &
       & (ierr, idopr, str, pop, push)
    use TOUZA_Std,only: choice, reg_entry, store_xstatus
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: idopr
    character(len=*),intent(in)          :: str
    integer,         intent(in),optional :: pop
    integer,         intent(in),optional :: push
    integer entr
    if (idopr.ge.lopr.or.idopr.lt.0) then
       ierr = ERR_PANIC
       call message(ierr, 'panic in operator registration')
       return
    endif
    entr = reg_entry(str, htopr, idopr)
    ierr = min(0, entr)
    if (ierr.eq.0) then
       oprop(idopr)%entr = entr
       oprop(idopr)%push = choice(-1, push)
       oprop(idopr)%pop = choice(-1, pop)

       mopr = max(mopr, idopr + 1)
    endif
  end subroutine reg_opr_prop

!!!_   . reg_fake_opr
  subroutine reg_fake_opr &
       & (ierr, handle, str)
    use TOUZA_Std,only: reg_entry, query_entry
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: handle
    character(len=*),intent(in)  :: str
    integer entr
    ierr = 0
    entr = query_entry(str, htopr)
    if (entr.ge.0) then
       ierr = ERR_DUPLICATE_SET
       call message(ierr, 'duplicate registration ' // trim(str))
    else
       entr = reg_entry(str, htopr, handle)
    endif
  end subroutine reg_fake_opr

!!!_   . parse_term_operator()
  integer function parse_term_operator(str) result(n)
    use TOUZA_Std,only: query_status
    implicit none
    character(len=*),intent(in) :: str
    integer jb
    n = query_status(str, htopr)
    if (n.lt.0) then
       jb = index(str, paramd)
       if (jb.gt.1) then
          n = query_status(str(1:jb-1), htopr)
       endif
    endif
  end function parse_term_operator
!!!_   . inquire_opr_nstack
  subroutine inquire_opr_nstack(ierr, pop, push, handle)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: pop, push
    integer,intent(in)  :: handle
    integer jb
    ierr = 0

    if (handle.lt.0.or.handle.ge.mopr) then
       jb = buf_h2item(handle)
       if (jb.lt.0) then
          ierr = ERR_INVALID_ITEM
          call message(ierr, 'invalid operator handle to inquire', (/handle/))
       else
          pop = 0
          push = 1
       endif
    else
       pop = oprop(handle)%pop
       push = oprop(handle)%push
    endif
  end subroutine inquire_opr_nstack
!!!_   . is_operator_reusable()
  logical function is_operator_reusable(handle) result(b)
    implicit none
    integer,intent(in) :: handle
    integer pop, push
    integer jerr
    call inquire_opr_nstack(jerr, pop, push, handle)
    b = pop.eq.1 .and. push.eq.1
  end function is_operator_reusable

!!!_   . query_opr_name
  subroutine query_opr_name &
       & (ierr, str, handle)
    use TOUZA_Std,only: query_name
    implicit none
    integer,         intent(out)         :: ierr
    character(len=*),intent(out)         :: str
    integer,         intent(in)          :: handle
    if (handle.lt.0.or.handle.ge.mopr) then
       ierr = ERR_INVALID_ITEM
       call message(ierr, 'invalid operator handle', (/handle/))
       return
    endif
    call query_name(ierr, str, oprop(handle)%entr, htopr)
  end subroutine query_opr_name

!!!_  - argument parser
!!!_   . parse_args
  subroutine parse_args(ierr)
    use TOUZA_Std,only: arg_init, arg_diag, parse, get_param
    implicit none
    integer,intent(out) :: ierr

    integer japos
    character(len=lpath) :: arg
    integer jerr

    ierr = 0

    mqueue = 0
    mfile = 0

    consts = mbuffer  ! save number of predefined constants here

    mstack = 0

    if (ierr.eq.0) call arg_init(ierr, cha=' = ')   ! disable assignment
    if (ierr.eq.0) call parse(ierr)
    if (ierr.eq.0) then
       japos = 0
       ! hprev = opr_null
       do
          japos = japos + 1
          call get_param(jerr, arg, japos)
          if (jerr.ne.0) exit
          do
             if (ierr.eq.0) then
                call parse_arg_operator(ierr, arg)
                if (ierr.eq.0) exit
                ierr = min(0, ierr)
             endif
             if (ierr.eq.0) then
                call parse_arg_literal(ierr, arg)
                if (ierr.eq.0) exit
                ierr = min(0, ierr)
             endif
             if (ierr.eq.0) then
                call parse_option(ierr, japos, arg)
                if (ierr.eq.0) exit
                ierr = min(0, ierr)
             endif
             if (ierr.eq.0) then
                call parse_arg_file(ierr, arg)
                if (ierr.eq.0) exit
                ierr = min(0, ierr)
             endif
             if (ierr.eq.0) ierr = ERR_INVALID_ITEM
             exit
          enddo
          if (ierr.ne.0) exit
          ! call show_stack(ierr, levv=-99)
       enddo
       ! call show_buffers(ierr)
    endif
    if (ierr.eq.0) call set_rec_filter(ierr)
  end subroutine parse_args

!!!_   . parse_option
  subroutine parse_option &
       & (ierr, japos, arg)
    implicit none
    integer,         intent(out)   :: ierr
    integer,         intent(inout) :: japos
    character(len=*),intent(in)    :: arg
    character(len=lpath) :: abuf
    integer n
    integer jval, jvar

    integer md, cmd

    md = mode_unset
    cmd = mode_unset
    ierr = 0
    abuf = arg

    jvar = index(abuf, paramd)
    if (jvar.eq.1) then
       ierr = ERR_INVALID_PARAMETER
       call message(ierr, 'invalid option ' // trim(abuf), (/japos/))
       return
    else if (jvar.gt.0) then
       jval = jvar + 1
       jvar = jvar - 1
    else
       if (index('-+', abuf(1:1)).eq.0) then
          ierr = ERR_NO_CANDIDATE   ! not error
          return
       endif
       if      (abuf(1:2).eq.'-v') then
          n = verify(trim(abuf), 'v', .TRUE.)
          if (n.ne.1) then
             ierr = ERR_INVALID_ITEM
          else
             lev_verbose = + (len_trim(abuf) - 1)
          endif
       else if (abuf.eq.'+v') then
          lev_verbose = +999
       else if (abuf(1:2).eq.'-q') then
          n = verify(trim(abuf), 'q', .TRUE.)
          if (n.ne.1) then
             ierr = ERR_INVALID_ITEM
          else
             lev_verbose = - (len_trim(abuf) - 1)
          endif
       else if (abuf.eq.'+q') then
          lev_verbose = -999
       else if (abuf(1:2).eq.'-d') then
          n = verify(trim(abuf), 'd', .TRUE.)
          if (n.ne.1) then
             ierr = ERR_INVALID_ITEM
          else
             dbgv = + (len_trim(abuf) - 1)
          endif
       else if (abuf.eq.'+d') then
          dbgv = +999
       else if (abuf.eq.'-f') then   ! write-mode
          md = mode_write
       else if (abuf.eq.'-a') then
          md = mode_append
       else if (abuf.eq.'-n') then
          md = mode_new
       else if (abuf.eq.'-c') then   ! read-mode
          md = mode_cycle
       else if (abuf.eq.'-s') then
          md = mode_terminate
       else if (abuf.eq.'-p') then
          md = mode_persistent
       else if (abuf.eq.'-i') then   ! compromise mode
          cmd = cmode_inclusive
       else if (abuf.eq.'-x') then
          cmd = cmode_intersect
       else if (abuf.eq.'-l') then
          cmd = cmode_first
       else if (abuf.eq.'-P') then
          global_offset_bgn = 0
          global_offset_end = 0
       else if (abuf.eq.'-F') then
          global_offset_bgn = 1
          global_offset_end = 0
       else
          ierr = ERR_INVALID_ITEM
       endif
       if (ierr.eq.ERR_INVALID_ITEM) then
          call message(ierr, 'invalid option ' // trim(abuf), (/japos/))
       endif
       if (md.ne.mode_unset) then
          if (ierr.eq.0) call parse_file_option(ierr, md)
       else if (cmd.ne.mode_unset) then
          if (ierr.eq.0) call parse_operator_option(ierr, cmd)
       endif
    endif
  end subroutine parse_option
!!!_   . parse_file_option
  subroutine parse_file_option(ierr, mode)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: mode

    type(file_t),pointer :: pfile
    character(len=128) :: msg

    ierr = 0
    if (mfile.gt.lfile) then
       ierr = ERR_INSUFFICIENT_BUFFER
       return
    endif

    pfile => ofile(mfile-1)

    if (is_read_mode(mode)) then
       if (mfile.eq.0) pfile => def_read
       if (is_read_mode(pfile%mode)) then
          pfile%mode = mode
       else
          ierr = ERR_INVALID_SWITCH
       endif
    else
       if (mfile.eq.0) pfile => def_write
       if (.not.is_read_mode(pfile%mode)) then
          pfile%mode = mode
       else
          ierr = ERR_INVALID_SWITCH
       endif
    endif
    if (ierr.ne.0) then
101    format('cannot set file option for file ', I0)
       write(msg, 101) mfile
       call message(ierr, msg)
    endif
  end subroutine parse_file_option

!!!_   . parse_operator_option
  subroutine parse_operator_option(ierr, mode)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: mode

    ierr = 0
    if (mqueue.le.0) then
       def_cmode = mode
    else
       aqueue(mqueue-1)%cmode = mode
    endif
    return
  end subroutine parse_operator_option

!!!_   . parse_arg_file
  subroutine parse_arg_file &
       & (ierr, arg)
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(in)    :: arg
    integer hfile, hbuf
    integer jb
    integer lasth
    integer jf
    character(len=lname) :: bname
    ierr = 0
    call new_file(ierr, hfile, arg)
    if (ierr.eq.0) then
       if (hfile.lt.0) ierr = ERR_INVALID_ITEM
    endif
    if (ierr.eq.0) call last_queue(ierr, lasth)
    if (ierr.eq.0) then
       jf = file_h2item(hfile)
       if (lasth.eq.opr_OUTPUT) then
          ! file to write
          ! call show_stack(ierr)
          call pop_queue(ierr)
          ofile(jf)%mode = def_write%mode
          call append_queue(ierr, hfile, pop=1, push=0)
          if (ierr.eq.0) call pop_stack(ierr, hbuf)
          if (ierr.eq.0) then
             jb = buf_h2item(hbuf)
             obuffer(jb)%stt = stt_locked
             ofile(jf)%bh = hbuf
1011         format('W', I0)
             write(bname, 1011) wcount + global_offset_bgn
             obuffer(jb)%name = bname
             wcount = wcount + 1
          endif
          if (ierr.eq.0) call reg_fake_opr(ierr, hbuf, bname)
          ! if (ierr.eq.0) call register_obj(ierr, hbuf, bname)
       else
          ! file to read
          ofile(jf)%mode = mode_read
1001      format('F', I0)
          write(bname, 1001) rcount + global_offset_bgn
          rcount = rcount + 1
          call new_buffer(ierr, hbuf, bname)
          if (ierr.eq.0) call push_stack(ierr, hbuf)
          if (ierr.eq.0) then
             if (rcount-1.eq.0) wrefh = hfile
             ofile(jf)%bh = hbuf
             jb = buf_h2item(hbuf)
             obuffer(jb)%stt = stt_locked
             call append_queue(ierr, hfile, 0, 1, (/hbuf/))
          endif
          if (ierr.eq.0) call reg_fake_opr(ierr, hbuf, bname)
       endif
    endif
  end subroutine parse_arg_file

!!!_   . parse_arg_literal
  subroutine parse_arg_literal &
       & (ierr, arg)
    use TOUZA_Std,only: parse_number
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(in)    :: arg

    integer vi
    real(kind=KFLT) :: vf
    real(kind=KDBL) :: vd
    integer jerr
    integer handle

    ierr = 0
    handle = -1
    ! read(arg, *, IOSTAT=jerr) vi
    call parse_number(jerr, vi, arg)
    if (jerr.eq.0) then
       call new_buffer_literal(ierr, handle, kv_int, real(vi, kind=KBUF), arg)
    else
       ! read(arg, *, IOSTAT=jerr) vd
       call parse_number(jerr, vd, arg)
       if (jerr.eq.0) then
          call new_buffer_literal(ierr, handle, kv_dbl, real(vd, kind=KBUF), arg)
       else
          ! read(arg, *, IOSTAT=jerr) vf
          call parse_number(jerr, vf, arg)
          if (jerr.eq.0) then
             call new_buffer_literal(ierr, handle, kv_flt, real(vf, kind=KBUF), arg)
          endif
       endif
    endif
    if (ierr.eq.0) then
       if (handle.lt.0) then
          ierr = ERR_NO_CANDIDATE
       else
          call append_queue(ierr, handle, 0, 1, (/handle/))
       endif
       if (ierr.eq.0) call push_stack(ierr, handle)
    endif
  end subroutine parse_arg_literal

!!!_   . new_buffer_literal
  subroutine new_buffer_literal(ierr, handle, kv, v, repr, name)
    use TOUZA_Nio_header,only: hi_ASTR1, hi_ASTR2, hi_ASTR3
    use TOUZA_Nio_header,only: hi_AEND1, hi_AEND2, hi_AEND3
    use TOUZA_Nio_header,only: hi_DFMT
    use TOUZA_Nio_header,only: put_item
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: handle
    integer,         intent(in)          :: kv
    real(kind=KBUF), intent(in)          :: v
    character(len=*),intent(in)          :: repr
    character(len=*),intent(in),optional :: name

    integer jb
    integer m
    character(len=lname) :: bname

    ierr = 0
    call new_buffer(ierr, handle)
    if (ierr.eq.0) then
       jb = buf_h2item(handle)
       m = 1
       call alloc_buffer_t(ierr, obuffer(jb), m)
       if (ierr.eq.0) then
          if (present(name)) then
             call reg_fake_opr(ierr, handle, name)
             if (ierr.eq.0) obuffer(jb)%name = name
          else
101          format('L', I0)
             write(bname, 101) lcount + global_offset_bgn
             lcount = lcount + 1
             obuffer(jb)%name  = bname
          endif
       endif
       if (ierr.eq.0) then
          obuffer(jb)%stt   = stt_locked
          obuffer(jb)%k     = kv
          obuffer(jb)%vd(:) = v
          obuffer(jb)%desc  = repr
          if (v.eq.UNDEF) then
             obuffer(jb)%undef = - UNDEF
          else
             obuffer(jb)%undef = + UNDEF
          endif
       endif
    endif

  end subroutine new_buffer_literal

!!!_   . parse_arg_operator
  subroutine parse_arg_operator &
       & (ierr, arg)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: arg

    integer hopr, pop, hlast
    integer npop  ! for DUP
    integer,parameter :: loprnd = 3
    integer,parameter :: lpush = 2
    integer hbufo(lpush)
    integer iter

    ierr = 0

    hbufo = -1
    hopr = parse_term_operator(arg)
!!!_    * output special
    if (hopr.eq.opr_OUTPUT_KEEP) then
       call stack_DUP(ierr, opr_DUP, 0)
       pop = 1
       if (ierr.eq.0) call append_queue(ierr, opr_OUTPUT, pop, 0)
    else if (hopr.eq.opr_OUTPUT_POP) then
       pop = 1
       if (ierr.eq.0) call append_queue(ierr, opr_OUTPUT, pop, 0)
!!!_    * anchor
    else if (hopr.eq.opr_MARK) then
       if (ierr.eq.0) call stack_ANCHOR(ierr, hopr)
    else if (hopr.eq.opr_STOP) then
       if (ierr.eq.0) call stack_ANCHOR(ierr, hopr)
    else if (hopr.eq.opr_GO) then
       if (ierr.eq.0) call stack_ANCHOR(ierr, hopr)
!!!_    * que special (with last queue)
    else if (hopr.eq.opr_ITER) then
       if (ierr.eq.0) call stack_ITER_opr(ierr, hopr)
    else if (hopr.eq.opr_CUM) then
       if (ierr.eq.0) call stack_CUM_opr(ierr, hopr)
!!!_    * stacking special (with top stack)
    else if (hopr.eq.opr_DIST) then
       if (ierr.eq.0) call stack_DIST(ierr, hopr)
    else if (hopr.eq.opr_INSERT) then
       if (ierr.eq.0) call stack_INSERT(ierr, hopr)
    else if (hopr.eq.opr_REPEAT) then
       if (ierr.eq.0) call stack_REPEAT(ierr, hopr)
    else if (hopr.eq.opr_DUP) then
       call stack_DUP(ierr, hopr, 0)
    else if (hopr.eq.opr_EXCH) then
       if (ierr.eq.0) call stack_EXCH(ierr, hopr, 0)
    else if (hopr.eq.opr_POP) then
       if (ierr.eq.0) call last_queue(ierr, hlast, pop, iter=iter)
       if (ierr.eq.0) then
          if (hlast.eq.hopr.and.iter.eq.0) then
             continue
          else
             pop = 0
          endif
          npop = pop + 1
       endif
       if (ierr.eq.0) call stack_POP(ierr, pop, npop, iter, arg=arg)
    else if (hopr.eq.opr_FLUSH) then
       pop = 0
       npop = mstack
       if (ierr.eq.0) call stack_POP(ierr, pop, npop, iter, opr_FLUSH)
!!!_    * buffer property operator
    else if (grp_buffer_bgn.le.hopr .and. hopr.lt.grp_buffer_end) then
       call parse_buffer_opr(ierr, hopr, arg)
    else if (grp_header_bgn.le.hopr .and. hopr.lt.grp_header_end) then
       call parse_header_opr(ierr, hopr, arg)
!!!_    * normal operators
    else if (hopr.ge.0) then
       if (is_operator(hopr)) then
          if (ierr.eq.0) call stack_normal_opr(ierr, 0, 0, hopr, 0)
       else
          ! fake operator
          if (ierr.eq.0) call append_queue(ierr, hopr, 0, 1, (/hopr/))
          if (ierr.eq.0) call push_stack(ierr, hopr)
       endif
    else
       ierr = ERR_NO_CANDIDATE   ! not error
    endif
  end subroutine parse_arg_operator

!!!_   . parse_buffer_opr
  subroutine parse_buffer_opr (ierr, hopr, arg)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: hopr
    character(len=*),intent(in)  :: arg
    integer jpar, jend
    integer hbuf, jb

    ierr = 0
    call pop_stack(ierr, hbuf, .TRUE.)
    if (ierr.eq.0) jb = buf_h2item(hbuf)
    if (ierr.eq.0) ierr = min(0, jb)
    if (ierr.eq.0) then
       jpar = index(arg, paramd) + 1
       jend = len_trim(arg) + 1
       if (jpar.eq.1) jpar = jend
       select case(hopr)
       case(opr_TAG)
          if (jpar.lt.jend) then
             obuffer(jb)%name = arg(jpar:jend)
             obuffer(jb)%stt  = stt_locked
             call reg_fake_opr(ierr, hbuf, obuffer(jb)%name)
          endif
       case default
          ierr = ERR_NOT_IMPLEMENTED
          call message(ierr, 'reserved operator ' // trim(arg))
       end select
    endif
    return
  end subroutine parse_buffer_opr
!!!_   . parse_header_opr
  subroutine parse_header_opr (ierr, hopr, arg)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(in)  :: hopr
    character(len=*),intent(in)  :: arg
    integer jpar, jend
    integer hbuf, jf
    integer jerr

    ierr = 0
    call last_queue(jerr, hbuf)
    if (jerr.eq.0) then
       jf = file_h2item(hbuf)
    else
       jf = -1
    endif

    if (ierr.eq.0) then
       jpar = index(arg, paramd) + 1
       jend = len_trim(arg) + 1
       if (jpar.eq.1) jpar = jend
       select case(hopr)
       case(opr_DFMT)
          if (ierr.eq.0) then
             if (is_read_mode(ofile(jf)%mode)) then
                ierr = ERR_INVALID_ITEM
                call message(ierr, 'invalid operator' // trim(arg))
             endif
          endif
       case(opr_TSEL)
          if (mfile.eq.0) then
             call parse_rec_filter(ierr, def_read, arg(jpar:))
          else if (is_read_buffer(hbuf)) then
             call parse_rec_filter(ierr, ofile(jf), arg(jpar:))
          else
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'no file to set record filter' // trim(arg))
          endif
       case default
          ierr = ERR_NOT_IMPLEMENTED
          call message(ierr, 'reserved operator ' // trim(arg))
       end select
    endif
    return
  end subroutine parse_header_opr

!!!_   . parse_rec_filter
  subroutine parse_rec_filter &
       & (ierr, file, arg)
    use TOUZA_Std,only: split_list
    implicit none
    integer,         intent(out)   :: ierr
    type(file_t),    intent(inout) :: file
    character(len=*),intent(in)    :: arg

    character,parameter :: lsep=','
    character,parameter :: rsep=':'
    integer jp, la, jc, je, nc
    integer jf, nf
    integer rpos(3)
    integer,parameter :: hmin = (- HUGE(0)) - 1
    integer,parameter :: hmax = HUGE(0)

    ierr = 0
    la = len_trim(arg)
    if (la.eq.0) then
       continue
    else
       nf = 0
       jp = 0
       do
          jc = index(arg(jp+1:la), lsep)
          ! write(*, *) jp, jc, nf, arg(jp+1:la)
          if (jc.eq.0) then
             if (jp.lt.la) nf = nf + 1
             exit
          endif
          if (jc.gt.1) nf = nf + 1
          jp = jp + jc + len(lsep) - 1
       enddo
       ! write(*,*) arg(1:la), nf
       if (nf.gt.0) then
          allocate(file%recf(0:nf-1), STAT=ierr)
          if (ierr.eq.0) then
             file%jrf = 0
             jp = 0
             do jf = 0, nf - 1
                do
                   je = index(arg(jp+1:la), lsep)
                   if (je.eq.1) then
                      jp = jp + len(lsep)
                      cycle
                   endif
                   exit
                enddo
                if (je.eq.0) then
                   je = la + 1
                else
                   je = jp + je
                endif
                call split_list(nc, rpos, arg(jp+1:je-1), rsep, 3, (/hmin, hmax, 1/))
                if (nc.le.0) then
                   ierr = ERR_INVALID_PARAMETER
                   call message(ierr, 'bad range ' // arg(jp+1:je-1))
                   exit
                else
                   file%recf(jf)%bgn = rpos(1)
                   if (nc.gt.1) then
                      file%recf(jf)%end = rpos(2)
                   else
                      file%recf(jf)%end = hmin
                   endif
                   if (nc.gt.2) then
                      file%recf(jf)%stp = rpos(3)
                   else
                      file%recf(jf)%stp = 1
                   endif
                endif
                jp = je
             enddo
          endif
       endif
    endif
  end subroutine parse_rec_filter

!!!_   . set_rec_filter
  subroutine set_rec_filter(ierr)
    implicit none
    integer,intent(out) :: ierr
    integer jfile
    ! integer jr
    ierr = 0
    if (def_read%jrf.lt.0) then
       if (ierr.eq.0) allocate(def_read%recf(0:0), STAT=ierr)
       if (ierr.eq.0) then
          def_read%jrf = 0
          def_read%recf(0)%bgn=0
          def_read%recf(0)%end=-1
          def_read%recf(0)%stp=1
       endif
    else
       call adjust_rec_filter(def_read)
    endif
    do jfile = 0, min(mfile, lfile) - 1
       if (is_read_mode(ofile(jfile)%mode)) then
          if (ofile(jfile)%jrf.lt.0) then
             ofile(jfile)%jrf = 0
             ofile(jfile)%recf => def_read%recf
          else
             call adjust_rec_filter(ofile(jfile))
          endif
       endif
    enddo

  end subroutine set_rec_filter

!!!_    * adjust_rec_filter
  subroutine adjust_rec_filter(file)
    implicit none
    type(file_t),intent(inout) :: file
    integer jr
    integer,parameter :: hmin = (- HUGE(0)) - 1
    integer,parameter :: hmax = HUGE(0)

    type(loop_t),pointer :: recf(:)

    integer nspec

    recf => file%recf

    nspec = 0
    do jr = lbound(recf,1), ubound(recf,1)
       if (recf(jr)%bgn.eq.hmin) then
          recf(jr)%bgn = 0
       else
          recf(jr)%bgn = recf(jr)%bgn - global_offset_bgn
       endif
       if (recf(jr)%end.eq.hmin) then
          recf(jr)%end = recf(jr)%bgn + 1
       else if (recf(jr)%end.eq.hmax) then
          recf(jr)%end = -1
       else
          recf(jr)%end = recf(jr)%end - global_offset_end
       endif
       if (recf(jr)%stp.eq.0) then
          if (recf(jr)%end.lt.0.or.recf(jr)%bgn.lt.recf(jr)%end) then
             recf(jr)%stp = +1
          else
             recf(jr)%stp = -1
          endif
       endif
       if (recf(jr)%end.lt.0) then
          nspec = -1
       else if (nspec.ge.0) then
          nspec = nspec + (recf(jr)%end - recf(jr)%bgn) / recf(jr)%stp
       endif
    enddo
    if (nspec.eq.1.and.file%mode.eq.mode_read) file%mode = mode_persistent
  end subroutine adjust_rec_filter

!!!_   . stack_normal_opr
  subroutine stack_normal_opr(ierr, opop, npop, hopr, iter)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: opop   ! old pop size (for iter > 0)
    integer,intent(in)  :: npop   ! new pop size (for iter > 0)
    integer,intent(in)  :: hopr
    integer,intent(in)  :: iter
    integer opush
    integer pop,   push
    integer upop,  upush
    logical reuse
    integer j, hb
    integer apos, jpos, jdst
    integer lefth(0:max_operands-1)
    character(len=8) :: opr

    ierr = 0

    if (ierr.eq.0) call inquire_opr_nstack(ierr, upop, upush, hopr)
    if (ierr.eq.0) then
       if (upop.le.0.or.upush.le.0) then
          call query_opr_name(ierr, opr, hopr)
          ierr = ERR_NOT_IMPLEMENTED
          call message(ierr, 'reserved operator ' // trim(opr))
       endif
    endif
    if (ierr.eq.0) then
       if (iter.eq.0) then
          pop = upop
          push = upush
          opush = 0
          if (mstack.lt.pop) then
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'few operands')
          else
             apos = last_anchor()
             if (mstack - (apos + 1).lt.pop) then
                ierr = ERR_INVALID_ITEM
                call message(ierr, 'anchor inside operands')
             endif
          endif
       else if (mod(npop, upop).ne.0) then
          ierr = ERR_INVALID_ITEM
          call message(ierr, 'invalid operands to iterate.')
       else
          opush = (opop / upop) * upush
          push = (npop / upop) * upush
          pop  = npop
       endif
    endif
    ! write(*, *) 'normal/0', upop, upush
    ! write(*, *) 'normal/1', opop, opush
    ! write(*, *) 'normal/2', pop,  push
    if (ierr.eq.0) then
       reuse = is_operator_reusable(hopr)
       apos = mstack - pop - opush + opop
       if (reuse) then
          ! pop == push
          do j = 0, pop - opop - 1
             jpos = apos + j
             hb = bstack(jpos)
             if (is_buffer_locked(hb)) then
                if (ierr.eq.0) call search_free_buffer(ierr, bstack(jpos), 1)
             endif
          enddo
          ! write(*,*) 'normal/2', bstack(mstack-push:mstack-1)
       else
          if (max_operands.lt.upop.or.upop.lt.upush) then
             ierr = ERR_PANIC
             call message(ierr, 'PANIC, invalid operands')
             return
          endif
          jdst = apos
          ! write(*, *) 'normal/3', bstack(apos:mstack-1)
          do j = 0, pop - opop - 1, upop
             jpos = apos + j
             if (ierr.eq.0) call search_free_buffer(ierr, lefth, upush)
             bstack(jpos:jpos+upop-1) = -1
             bstack(jdst:jdst+upush-1) = lefth(0:upush-1)
             jdst = jdst + upush
          enddo
          if (opop.gt.0) then
             jpos = mstack - opush
             ! write(*, *) 'normal/33', jdst, jpos, bstack(jpos:jpos+upush-1)
             bstack(jdst:jdst+upush-1) = bstack(jpos:jpos+upush-1)
          endif
          ! write(*, *) 'normal/4', bstack(apos:mstack-1)
          if (ierr.eq.0) call mpop_stack(ierr, n=(pop - push - (opop - opush)))
          ! write(*, *) 'normal/5', bstack(apos:mstack-1)
          ! write(*, *) 'normal/6', apos, mstack
       endif
    endif
    if (ierr.eq.0) then
       if (iter.eq.0) then
          call append_queue(ierr, hopr, pop, push, bstack(mstack-push:mstack-1), iter)
       else
          call modify_queue(ierr, pop, push, bstack(mstack-push:mstack-1), niter=iter)
       endif
    endif
    call message(ierr, 'stack_normal_opr', levm=-9)
  end subroutine stack_normal_opr

!!!_   . stack_ITER_opr
  subroutine stack_ITER_opr(ierr, hopr)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hopr
    integer lasth
    integer opop, opush, oiter
    integer npop, niter
    integer pop,  push
    integer apos, alev
!!!_    * note
    !  put the last queue term on every input step or 1 until the mark
    ! -1+1   A B C    SQR  ITER   ==  A SQR     B SQR   C SQR
    ! -2+1   A B C    SUB  ITER   ==  (error)
    ! -2+1   A B C D  SUB  ITER   ==  A B SUB   C D SUB
    ! -1+2   A B C    DUP  ITER   ==  A DUP     B DUP   C DUP    special
    ! -2+2   A B C D  EXCH ITER   ==  A B EXCH  C D EXCH         special
!!!_    * body
    ierr = 0
    if (ierr.eq.0) call last_queue(ierr, lasth, opop, opush, oiter)
    if (ierr.eq.0) then
       if (.not.is_operator(lasth)) then
          ierr = ERR_INVALID_ITEM
          call message(ierr, 'ITER requires operator')
       endif
    endif
    if (ierr.eq.0) then
       if (lasth.eq.opr_DUP) then
          call stack_DUP(ierr, lasth, 1)
       else if (lasth.eq.opr_EXCH) then
          call stack_EXCH(ierr, lasth, 1)
       ! else if (lasth.eq.opr_POP) then
       !    call stack_POP(ierr, pop, npop, iter+1)
       else if (lasth.eq.opr_DIST) then
          if (ierr.eq.0) call stack_DIST(ierr, lasth)
       else if (lasth.eq.opr_INSERT) then
          if (ierr.eq.0) call stack_INSERT(ierr, lasth)
       else if (lasth.eq.opr_REPEAT) then
          if (ierr.eq.0) call stack_REPEAT(ierr, lasth)
       else
          call inquire_opr_nstack(ierr, pop, push, lasth)
          if (push.le.0.or.pop.le.0) then
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'non ITERable operator')
          else if (oiter.ne.0) then
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'cannot repeat ITER and/or CUM')
          else
             niter = 1
             apos = last_anchor()
             npop = mstack - (apos + 1) + opop - opush
             ! write(*, *) 'ITER/0', opop, opush, oiter, '/', npop
             call stack_normal_opr(ierr, opop, npop, lasth, niter)
             alev = anchor_level(apos)
             if (alev.gt.0) then
                bstack(apos:mstack-2) = bstack(apos+1:mstack-1)
                call modify_queue(ierr, npop+1)
                call pop_stack(ierr)
             endif
          endif
       endif
    endif
  end subroutine stack_ITER_opr

!!!_   . stack_CUM_opr
  subroutine stack_CUM_opr(ierr, hopr)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hopr

    integer lasth, opop, opush, iter
    integer upop, upush
    integer npop, npush
    integer alev, apos
    integer jsrc
!!!_    * note
!!!_    * body
    ierr = 0
    if (ierr.eq.0) call last_queue(ierr, lasth, opop, opush, iter=iter)
    if (ierr.eq.0) then
       if (.not.is_operator(lasth)) then
          ierr = ERR_INVALID_ITEM
          call message(ierr, 'missing operation for CUM')
       else if (iter.ne.0) then
          ierr = ERR_INVALID_ITEM
          call message(ierr, 'cannot repeat CUM and/or ITER')
       endif
    endif
    if (ierr.eq.0) call inquire_opr_nstack(ierr, upop, upush, lasth)
    if (ierr.eq.0) then
       if (upush.ne.1.or.upop.le.1) then
          ierr = ERR_INVALID_ITEM
          call message(ierr, 'invalid operation for CUM')
          return
       endif
    endif

    if (ierr.eq.0) then
       apos = last_anchor()
       alev = anchor_level(apos)
       npush = upush
       npop = mstack - (apos + 1) + opop - opush
       jsrc = mstack - opush
       bstack(apos+1:apos+upush) = bstack(jsrc:jsrc+upush-1)
       if (ierr.eq.0) call mpop_stack(ierr, n=(npop - npush - (opop - opush)))
       if (alev.gt.0) then
          bstack(apos:mstack-2) = bstack(apos+1:mstack-1)
          call pop_stack(ierr)
          npop = npop + 1
       endif
    endif
    if (ierr.eq.0) call modify_queue(ierr, npop, niter=-1)
  end subroutine stack_CUM_opr

!!!_   . stack_DIST - cumulative DIST
  subroutine stack_DIST(ierr, hopr)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hopr

    integer lasth
    integer opop, opush, oiter
    integer npop, npush, niter
    integer apos, alev,  nopr
    integer jsrc, jdst,  j
    integer dpos
    integer nadd
!!!_    * note
    !    MARK A B C D E DIST                     == A E      B E     C E   D E    -1-5+8
    !    MARK A B C D E DIST DIST                == A D E    B D E   C D E        -1-5+9
    !    MARK A B C D E DIST DIST DIST           == A C D E  B C D E              -1-5+8
    !    MARK A B C D E DIST DIST DIST DIST      == A B C D E                     -1-5+5
    !    MARK A B C D E DIST DIST DIST DIST DIST == error
!!!_    * body
    ierr = 0
    if (ierr.eq.0) call last_queue(ierr, lasth, opop, opush, oiter)
    if (ierr.eq.0) then
       if (lasth.ne.hopr) then
          opop = 0
          opush = 0
          oiter = 0
       endif
    endif
    if (ierr.eq.0) then
       niter = oiter + 1
       if (oiter.eq.0) then
          apos = last_anchor()
          alev = anchor_level(apos)
          nopr = mstack - (apos + 1) - 1
          npop = nopr + 1
          opush = npop
          if (alev.gt.0) npop = npop + 1
       else
          apos = mstack - opush - 1
          alev = 0
          nopr = opush / niter - 1
          npop = opop
       endif
       npush = nopr * (niter + 1)
       dpos = mstack - niter
       ! write(*, *) 'dist/0', opop, opush, oiter
       ! write(*, *) 'dist/1', npop, npush, niter, nopr
       ! write(*, *) 'dist/2', bstack(apos+1:mstack-1)
       ! write(*, *) 'dist/3', bstack(dpos:mstack-1)
       if (npush.eq.0) then
          ierr = ERR_INVALID_ITEM
          call message(ierr, 'reached maximum DIST sequence.')
       endif
    endif
    if (ierr.eq.0) then
       nadd = npush - opush
       if (nadd.gt.0) then
          call mpush_stack(ierr, n=nadd)
       else if (nadd.lt.0) then
          call mpop_stack(ierr, n=-nadd)
       endif
       ! write(*, *) 'dist/4', bstack(apos+1:mstack-1)
    endif
    if (ierr.eq.0) then
       bstack(mstack-niter:mstack-1) = bstack(dpos:dpos+niter-1)
       ! write(*, *) 'dist/5', bstack(apos+1:mstack-1)
       do j = nopr - 1, 0, -1
          jsrc = apos + 1 + j * niter
          jdst = apos + 1 + j * (niter + 1)
          ! bstack(jdst+niter:jdst+niter+oiter) = bstack(mstack-niter:mstack-1)
          bstack(jdst) = bstack(jsrc)
          bstack(jdst+1:jdst+niter) = bstack(mstack-niter:mstack-1)
          ! write(*, *) 'dist/6', j, jsrc, jdst, bstack(jsrc)
          ! write(*, *) 'dist/7', bstack(apos+1:mstack-1)
       enddo
       if (alev.gt.0) then
          ! fragile anchor
          bstack(apos:mstack-2) = bstack(apos+1:mstack-1)
          apos = apos - 1
          call pop_stack(ierr)
       endif
    endif
    if (ierr.eq.0) then
       if (oiter.eq.0) then
          call append_queue(ierr, hopr, npop, npush, bstack(apos+1:mstack-1), niter)
       else
          call modify_queue(ierr, npop, npush, bstack(apos+1:mstack-1), niter=niter)
       endif
    endif
  end subroutine stack_DIST

!!!_   . stack_REPEAT - cumulative REPEAT
  subroutine stack_REPEAT(ierr, hopr)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hopr

    integer lasth
    integer apop, apush
    integer opop, opush, oiter
    integer npop, npush, niter
    integer apos, alev
    integer nopr
!!!_    * note (example)
    !      STOP A B C      REPEAT             ==  STOP A B C      A B C
    !      MARK A B C      REPEAT             ==       A B C      A B C
    !      MARK A B C MARK REPEAT             ==       A B C MARK A B C
    !      STOP A B C MARK REPEAT             ==  STOP A B C MARK A B C
    !      MARK A B C STOP REPEAT             ==       A B C STOP A B C
    !      STOP A B C STOP REPEAT             ==  STOP A B C STOP A B C
    !      MARK A B C MARK REPEAT REPEAT      ==       A B C MARK A B C      A B C
    !      STOP A B C      REPEAT REPEAT      ==  STOP A B C      A B C      A B C
    !      STOP A B C      REPEAT STOP REPEAT ==  STOP A B C STOP A B C STOP A B C

    !      X MARK A B C REPEAT NOP REPEAT     ==  X A B C A B C  X A B C A B C

    !  anchor prefix is kept, not consumed during REPEAT

!!!_    * note (queue%iter usage)
    !    queue%iter stores number of operands for this operation.
!!!_    * body
    ierr = 0
    if (ierr.eq.0) call last_queue(ierr, lasth, apop, apush, oiter)
    if (ierr.eq.0) then
       if (lasth.eq.opr_ANCHOR.and.apop.eq.0) then
          call pop_queue(ierr)
          if (ierr.eq.0) call last_queue(ierr, lasth, opop, opush, oiter)
       else
          opop = apop
          opush = apush
          apop = 0
          apush = 0
       endif
    endif
    if (ierr.eq.0) then
       if (lasth.ne.hopr) then
          oiter = 0
          opop = 0
          opush = 0
       endif
    endif
    if (ierr.eq.0) then
       ! write(*, *) 'repeat/0', apop, apush, '/', opop, opush, oiter
       ! call show_stack(ierr)
       if (oiter.eq.0) then
          apos = last_anchor(- apush - 1)
          alev = anchor_level(apos)
          nopr = mstack - (apos + 1) - apush
          npush = nopr + apush + nopr
       else
          apos = mstack - opush - apush - 1
          alev = 0
          nopr = oiter
          npush = opush + apush + nopr
       endif
       npop = nopr
       ! write(*, *) 'repeat/1', apos, alev, nopr, '/', bstack(mstack-opush)
       ! write(*, *) 'repeat/2', bstack(apos+1:apos+nopr)
       ! write(*, *) 'repeat/3', bstack(apos+nopr+1:mstack-1)
       call mpush_stack(ierr, bstack(apos+1:apos+nopr), nopr)
       ! write(*, *) 'repeat/4', bstack(apos+1:mstack-1)
       niter = nopr
       if (alev.gt.0) then
          ! fragile anchor
          bstack(apos:mstack-2) = bstack(apos+1:mstack-1)
          call pop_stack(ierr)
          apos = apos - 1
          ! write(*, *) 'repeat/5', bstack(apos:mstack-1)
          npop = npop + 1
       endif
       ! write(*, *) 'repeat/6', npop, npush, niter
       if (oiter.eq.0) then
          call append_queue(ierr, hopr, npop, npush, bstack(apos+1:mstack-1), niter)
       else
          call modify_queue(ierr, npop, npush, bstack(apos+1:mstack-1), niter=niter)
       endif
    endif
  end subroutine stack_REPEAT

!!!_   . stack_INSERT - cumulative INSERT
  subroutine stack_INSERT(ierr, hopr)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hopr

    integer lasth
    integer opop, opush, oiter
    integer npop, npush, niter
    integer apos, alev
    integer hins
!!!_    * note
    !      MARK A B C D INSERT          ==      D A B C
    !      MARK A B C D INSERT INSERT   ==      C D A B
    !      STOP A B C D INSERT INSERT   == STOP C D A B
!!!_    * body
    ierr = 0
    if (ierr.eq.0) call last_queue(ierr, lasth, opop, opush, iter=oiter)
    if (ierr.eq.0) then
       if (lasth.ne.hopr) then
          opop = 0
          opush = 0
          oiter = 0
       endif
    endif
    if (ierr.eq.0) then
       if (oiter.eq.0) then
          apos = last_anchor()
          alev = anchor_level(apos)
          npop = mstack - (apos + 1)
          npush = npop
          if (alev.gt.0) npop = npop + 1
       else
          apos = mstack - opush - 1
          alev = 0
          npush = opush
          npop = opop
       endif
       niter = oiter + 1
       hins = bstack(mstack - 1)
       bstack(apos+2:mstack-1) = bstack(apos+1:mstack-2)
       bstack(apos+1) = hins
       if (alev.gt.0) then
          bstack(apos:mstack-2) = bstack(apos+1:mstack-1)
          call pop_stack(ierr)
       endif
    endif
    if (ierr.eq.0) then
       if (oiter.eq.0) then
          call append_queue(ierr, hopr, npop, npush, bstack(mstack-npush:mstack-1), iter=niter)
       else
          call modify_queue(ierr, npop, npush, bstack(mstack-npush:mstack-1), niter=niter)
       endif
    endif
  end subroutine stack_INSERT

!!!_   . stack_DUP - cumulative DUP
  subroutine stack_DUP(ierr, hopr, iter)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hopr
    integer,intent(in)  :: iter   ! 0 if bare DUP
    integer lasth
    integer toph
    integer oiter, opop, opush
    integer niter, npop, npush
    integer apos,  alev, nadd,  nopr
    integer jsrc,  jdst, j
!!!_    * Note
    !      A     DUP                   ==  A DUP[0+1]   AA       (fake -1+2 stacking, to avoid anchor between)
    !      A     DUP DUP               ==  A DUP[0+2]   AAA
    !      A     DUP DUP DUP           ==  A DUP[0+3]   AAAA
    ! STOP A     DUP         ITER      ==  A DUP[-1+2]  AA            (*)
    ! STOP A     DUP DUP     ITER      ==  A DUP[-1+3]  AAA           (*)
    ! STOP A     DUP         ITER ITER ==  A DUP[-1+3]  AAA
    ! STOP A B C DUP         ITER      ==  A DUP[-3+6]  AA BB CC      (*)
    ! STOP A B C DUP         ITER ITER ==  A DUP[-3+9]  AAA BBB CCC
    ! STOP A B C DUP DUP     ITER      ==  A DUP[-3+9]  AAA BBB CCC
    ! STOP A B C DUP DUP     ITER ITER ==  A DUP[-3+12] AAAA BBBB CCCC

    ! (*) First ITER distributes DUP at every operand,
    !     which means STOP A DUP ITER is equivalent to A DUP.

    ! Special case with anchor

    ! A STOP DUP      == A STOP A
    ! A STOP DUP DUP  == A STOP A A
    ! A STOP DUP ITER == *error*      Anchor inside operands to ITERated DUP raise an error.

!!!_    * body
    ierr = 0

    call last_queue(ierr, lasth, opop, opush, oiter)
    if (iter.eq.0) then
       ! bare DUP
       if (ierr.eq.0) then
          npop = 0
          call pop_stack(ierr, toph, keep=.TRUE., anchor=.FALSE.)
          if (ierr.eq.0) call push_stack(ierr, toph)
          if (lasth.ne.hopr) then
             ! initial DUP
             npush = 1
             if (ierr.eq.0) call append_queue(ierr, hopr, npop, npush, (/toph/))
          else
             npush = opush + 1
             ! succesive DUP
             if (ierr.eq.0) call modify_queue(ierr, npop, npush, bstack(mstack - npush:mstack - 1))
          endif
       endif
    else
       ! ITERated DUP
       apos = last_anchor()
       alev = anchor_level(apos)
       if (oiter.eq.0) then
          oiter = 1
          opush = opush + 1
          opop = 1
          niter = opush
          nopr  = mstack - (apos + 1) - (opush - 1)
          npop  = nopr
          npush = nopr * opush
          nadd = (npush - npop) - (opush - opop)
       else
          niter = oiter + 1
          nopr  = opush / oiter
          npop  = opop
          npush = opush + nopr
          nadd  = nopr
       endif
       if (npop.le.0.or.nopr.le.0) then
          ierr = ERR_INVALID_ITEM
          call message(ierr, 'Anchor inside DUPlication')
       endif
       if (ierr.eq.0) then
          if (mod(mstack + nadd - (apos + 1), nopr).ne.0) then
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'Anchor shifted during ITERated DUP')
          endif
       endif
       if (ierr.eq.0) then
          call mpush_stack(ierr, n=nadd)
          do j = nopr - 1, 0, -1
             jsrc = (apos + 1) + j * oiter
             jdst = (apos + 1) + j * niter
             bstack(jdst:jdst+niter-1) = bstack(jsrc)
          enddo
          if (alev.gt.0) then
             npop = npop + 1
             bstack(apos:mstack-2) = bstack(apos+1:mstack-1)
             call pop_stack(ierr)
          endif
          call modify_queue(ierr, npop, npush, bstack(mstack-npush:mstack-1), niter=niter)
       endif
    endif
  end subroutine stack_DUP

!!!_   . stack_EXCH - cumulative EXCH.  ITERated EXCH queue is compacted.
  subroutine stack_EXCH(ierr, hopr, iter)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hopr
    integer,intent(in)  :: iter

    integer lasth
    integer oiter, opop, opush
    integer niter, npop, npush
    integer apos,  alev, nopr
    integer hexch
    integer jpos
    integer jsrc,  jdst, j
!!!_    * note i
    !      A B         EXCH      =  B A
    ! STOP A B         EXCH ITER =  B A
    ! STOP A B C D     EXCH ITER =  B A D C
    ! STOP A B C D E F EXCH ITER =  B A D C F E

    ! A STOP B EXCH              =  B STOP A
    ! A B C STOP D EXCH ITER     =  *error*
!!!_    * note ii
    ! Due to the limit of implementation, strange operation can be
    ! occured for ITERated EXCH.
    ! STOP A B  C D EXCH ITER ITER = A B  C D
    ! STOP A B  C D EXCH EXCH ITER = A B  D C
    ! Successive EXCH is meaningless and rarely used, so this feature is left.
!!!_    * note iii
    ! Although EXCH EXCH is identical to nothing,
    ! a special meaning is NOT given for successive EXCH queue.
    ! I implemeted once, but it is rather confusing.
!!!_    * body
    ierr = 0

    call last_queue(ierr, lasth, opop, opush, oiter)

    ! write(*, *) 'exch/0', iter, lasth, '/', opop, opush, oiter
    if (iter.eq.0) then
       ! bare EXCH
       if (ierr.eq.0) then
          if (lasth.eq.hopr.and.oiter.eq.0) then
             ! cumulative EXCH
             hexch = bstack(mstack - 1)
             bstack(mstack - 1)    = bstack(mstack - opop)
             bstack(mstack - opop) = hexch
             call modify_queue(ierr, push=opop, bufh=bstack(mstack-opop:mstack-1))
             ! write(*, *) 'exch/1', bstack(mstack-opop:mstack-1)
          else
             ! initial EXCH
             hexch = bstack(mstack - 1)
             if (is_anchor(hexch)) then
                ierr = ERR_INVALID_ITEM
                call message(ierr, 'Anchor on the top')
             endif
             if (ierr.eq.0) then
                opop = 1
                do
                   opop = opop + 1
                   jpos = mstack - opop
                   if (jpos.lt.0) then
                      ierr = ERR_INVALID_ITEM
                      call message(ierr, 'No other operands to EXCH')
                      exit
                   endif
                   if (.not.is_anchor(bstack(jpos))) exit
                enddo
             endif
             if (ierr.eq.0) then
                bstack(mstack - 1) = bstack(jpos)
                bstack(jpos) = hexch
                call append_queue(ierr, hopr, opop, opop, bstack(mstack-opop:mstack-1))
             endif
          endif
       endif
    else
       ! ITERated EXCH
       if (ierr.eq.0) then
          niter = oiter + 1
          if (oiter.eq.0) then
             apos = last_anchor()
             alev = anchor_level(apos)
             nopr = mstack - (apos + 1)
             npush = nopr
             npop = nopr
             opush = nopr - 2
             if (alev.gt.0) npop = npop + 1
          else
             apos = mstack - opush - 1
             alev = 0
             nopr = opush
             npush = nopr
             npop = opop
          endif
          if (mod(nopr, 2).ne.0) then
             ierr = ERR_INVALID_ITEM
             call message(ierr, 'EXCH operation on odd operands.')
          endif
       endif
       if (ierr.eq.0) then
          ! write(*, *) 'exch/3', apos, alev, nopr
          ! write(*, *) 'exch/4', npop, niter
          ! write(*, *) 'exch/5', opop, oiter
          do j = 0, opush / 2 - 1
             jsrc = apos + 1 + j * 2
             jdst = jsrc + 1
             hexch = bstack(jsrc)
             bstack(jsrc) = bstack(jdst)
             bstack(jdst) = hexch
          enddo
          if (alev.gt.0) then
             bstack(apos:mstack-2) = bstack(apos+1:mstack-1)
             call pop_stack(ierr)
          endif
       endif
       if (ierr.eq.0) then
          call modify_queue(ierr, npop, npush, bstack(mstack-npush:mstack-1), niter=niter)
       endif
    endif
    ! write(*, *) 'exch/9', ierr
  end subroutine stack_EXCH

!!!_   . stack_POP - cumulative POP.  Succesive and/or ITERated POP queue is compacted.
  subroutine stack_POP(ierr, opop, npop, niter, hopr, arg)
    use TOUZA_Std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(in)          :: opop
    integer,         intent(in)          :: npop
    integer,         intent(in)          :: niter
    integer,         intent(in),optional :: hopr
    character(len=*),intent(in),optional :: arg

    integer h
    integer righth(npop)
    integer lefth(1) ! dummy

    ierr = 0
    ! write(*, *) 'pop', opop, npop
    if (present(arg)) then
       call parse_buffer_opr(ierr, opr_TAG, arg)
    endif
    if (ierr.eq.0) call mpop_stack(ierr, righth, npop - opop)
    if (ierr.eq.0) then
       h = choice(opr_POP, hopr)
       if (opop.eq.0) then
          call append_queue(ierr, h, npop, 0, lefth)
       else
          call modify_queue(ierr, npop, 0, lefth, niter=niter)
       endif
    endif
  end subroutine stack_POP
!!!_   . stack_ANCHOR
  subroutine stack_ANCHOR(ierr, hopr)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: hopr
    integer alev
    integer anch
    integer apos,  npop, npush
    integer lasth, opop, opush
    integer jdmy
    integer oprq

    ierr = 0
    alev = 0
    oprq = opr_ANCHOR

    select case(hopr)
    case(opr_MARK)
       alev = +1
    case(opr_STOP)
       alev = 0
    case(opr_GO)
       alev = -1
    case default
       ierr = ERR_INVALID_PARAMETER
       call message(ierr, 'unknown operation for anchor')
    end select

    if (ierr.eq.0) then
       call last_queue(jdmy, lasth, opop, opush)
       if (lasth.ne.oprq) then
          opop = 0
          opush = 0
       endif
    endif

    if (ierr.eq.0) then
       if (alev.lt.0) then
          apos = last_anchor()
          if (apos.ge.0) then
             npop = mstack - apos
             npush = npop - 1
             if (opop.eq.0) then
                call append_queue(ierr, oprq, npop, npush, bstack(apos+1:mstack))
             else
                npop = opop - opush + npop
                ! write(*, *) opop, opush, npop, npush
                call modify_queue(ierr, npop, npush, bstack(apos+1:mstack))
             endif
             mstack = mstack - 1
             ! hard-coded shift
             bstack(apos:mstack-1) = bstack(apos+1:mstack)
          endif
       else
          anch = anchor_handle(alev)
          if (ierr.eq.0) call push_stack(ierr, anch)
          if (ierr.eq.0) then
             if (opush.eq.0) then
                call append_queue(ierr, oprq, 0, 1, (/anch/))
             else
                npush = opush + 1
                call modify_queue(ierr, 0, npush, bstack(mstack - npush:mstack - 1))
             endif
          endif
       endif
    endif

  end subroutine stack_ANCHOR

!!!_   & anchor_handle ()
  integer function anchor_handle (lev) result(h)
    implicit none
    integer,intent(in) :: lev
    if (lev.ge.0) then
       h = ofs_anchor + lev
    else
       h = -1
    endif
  end function anchor_handle

!!!_   & anchor_level ()
  integer function anchor_level (pos) result(n)
    integer,intent(in) :: pos
    if (pos.lt.0) then
       n = 0
    else if (pos.lt.mstack) then
       n = anchor_h2level(bstack(pos))
    else
       n = -1
    endif
  end function anchor_level

!!!_   & anchor_h2level ()
  integer function anchor_h2level (handle) result(n)
    implicit none
    integer,intent(in) :: handle
    ! negative if robust, positive if fragile, 0 otherwise
    n = handle_type(handle)
    if (n.eq.hk_anchor) then
       n = handle - ofs_anchor
    else
       n = -1
    endif
    return
  end function anchor_h2level

!!!_   & last_anchor ()
  integer function last_anchor(start) result(n)
    use TOUZA_Std,only: choice
    implicit none
    integer,optional,intent(in) :: start
    integer j, m
    n = -1
    m = choice(-1, start)
    if (m.lt.0) then
       do j = min(mstack,lstack) + m, 0, -1
          if (is_anchor(bstack(j))) then
             n = j
             exit
          endif
       enddo
    else
       do j = m, min(mstack,lstack) - 1
          if (is_anchor(bstack(j))) then
             n = j
             exit
          endif
       enddo
    endif
  end function last_anchor

!!!_  - [a-]queue manager
!!!_   . append_queue
  subroutine append_queue &
       & (ierr, handle, pop, push, bufh, iter)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: handle
    integer,intent(in)          :: pop, push
    integer,intent(in),optional :: bufh(0:*)
    integer,intent(in),optional :: iter

    integer jq

    ierr = 0

    jq = mqueue
    mqueue = mqueue + 1
    if (jq.lt.lqueue) then
       if (aqueue(jq)%term.ge.0) then
          deallocate(aqueue(jq)%lefth, STAT=ierr)
       endif
       if (ierr.eq.0) then
          aqueue(jq)%term = handle
          aqueue(jq)%nopr = pop
          aqueue(jq)%desci = ' '
          aqueue(jq)%desco = ' '
          aqueue(jq)%iter = choice(0, iter)
          allocate(aqueue(jq)%lefth(0:push-1), STAT=ierr)
       endif
       if (ierr.eq.0) then
          aqueue(jq)%lefth(:) = -1
          if (present(bufh)) then
             aqueue(jq)%lefth(0:push-1) = bufh(0:push-1)
          endif
       endif
       ! write(*, *) 'append_queue', ierr, jq, push, pop
    else
       ierr = ERR_INSUFFICIENT_BUFFER
    endif
  end subroutine append_queue

!!!_   . modify_queue
  subroutine modify_queue &
       & (ierr, pop, push, bufh, term, niter, cmode, jqueue)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: pop
    integer,intent(in),optional :: push
    integer,intent(in),optional :: bufh(0:*)
    integer,intent(in),optional :: term
    integer,intent(in),optional :: niter
    integer,intent(in),optional :: cmode
    integer,intent(in),optional :: jqueue

    integer jq

    ierr = 0
    jq = choice(-1, jqueue)
    if (jq.lt.0) jq = mqueue + jq
    if (jq.lt.0.or.jq.ge.lqueue) then
       ierr = ERR_INVALID_PARAMETER
    endif
    if (ierr.eq.0) then
       if (present(pop)) then
          aqueue(jq)%nopr = pop
       endif
       if (present(push).NEQV.present(bufh)) then
          ierr = ERR_INVALID_ITEM
       else if (present(push)) then
          deallocate(aqueue(jq)%lefth, STAT=ierr)
          if (ierr.eq.0) allocate(aqueue(jq)%lefth(0:push-1), STAT=ierr)
          if (ierr.eq.0) aqueue(jq)%lefth(0:push-1) = bufh(0:push-1)
       endif
       if (present(term)) then
          aqueue(jq)%term = term
       endif
       if (present(niter)) then
          aqueue(jq)%iter = niter
       endif
       if (present(cmode)) then
          aqueue(jq)%cmode = cmode
       endif
    endif

  end subroutine modify_queue

!!!_   . last_queue
  subroutine last_queue &
       & (ierr, handle, pop, push, iter)
    use TOUZA_Std,only: set_if_present
    implicit none
    integer,intent(out)          :: ierr
    integer,intent(out)          :: handle
    integer,intent(out),optional :: pop
    integer,intent(out),optional :: push
    integer,intent(out),optional :: iter
    integer jq

    ierr = 0
    jq = mqueue - 1
    if (jq.lt.0) then
       handle = -1
       call set_if_present(pop, -1)
       call set_if_present(push, -1)
    else
       handle = aqueue(jq)%term
       call set_if_present(pop, aqueue(jq)%nopr)
       call set_if_present(push, size(aqueue(jq)%lefth))
       call set_if_present(iter, aqueue(jq)%iter)
    endif
  end subroutine last_queue
!!!_   . pop_queue
  subroutine pop_queue &
       & (ierr, handle)
    integer,intent(out)          :: ierr
    integer,intent(out),optional :: handle
    integer jq

    ierr = 0
    mqueue = mqueue - 1
    jq = mqueue
    if (jq.lt.0) then
       ierr = ERR_INVALID_ITEM
       if (present(handle)) then
          handle = -1
       endif
    else
       if (present(handle)) then
          handle = aqueue(jq)%term
       endif
    endif
  end subroutine pop_queue

!!!_   . set_queue_descr
  subroutine set_queue_descr(ierr, aq)
    implicit none
    integer,      intent(out)   :: ierr
    type(queue_t),intent(inout) :: aq
    integer push, pop
    ierr = 0
    push = size(aq%lefth)
    pop  = aq%nopr
    if (ierr.eq.0) call get_obj_list(ierr, aq%desco, aq%lefth, push)
    if (ierr.eq.0) call get_obj_list(ierr, aq%desci, bstack(mstack-pop:mstack-1), pop)
    call message(ierr, 'trace:set_queue_descr', levm=-9)
  end subroutine set_queue_descr

!!!_   . trace_queue
  subroutine trace_queue (ierr, aq, levv, u)
    use TOUZA_Std,only: choice
    implicit none
    integer,         intent(out) :: ierr
    type(queue_t),   intent(in)  :: aq
    integer,optional,intent(in)  :: levv
    integer,optional,intent(in)  :: u
    integer lv, utmp
    integer handle
    integer j

    ierr = 0
    lv = choice(lev_verbose, levv)
    utmp = choice(ulog, u)

    handle = aq%term

    j = file_h2item(handle)
    if (j.ge.0) then
       call trace_file_access(ierr, handle, lv, utmp)
    else
       j = buf_h2item(handle)
       if (j.ge.0) then
          call trace_buffer_access(ierr, handle, utmp)
       else
          call trace_operation(ierr, aq, utmp)
       endif
    endif
    call message(ierr, 'trace_queue', levm=-9)
  end subroutine trace_queue

!!!_   . trace_file_access
  subroutine trace_file_access (ierr, fileh, levv, u)
    use TOUZA_Std,only: join_list, choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: fileh
    integer,intent(in),optional :: levv
    integer,intent(in),optional :: u
    integer lv, utmp
    character(len=lpath) :: str
    integer jfile
    character(len=16) :: acc
    character(len=lname) :: bname
    character(len=128)   :: domain
    type(loop_t) :: lpp(lcoor)
    character(litem) :: cname(lcoor)

    ierr = 0
    utmp = choice(ulog, u)
    lv = choice(lev_verbose, levv)
    jfile = file_h2item(fileh)
    ierr = min(0, jfile)
    if (ierr.eq.0) call get_obj_string(ierr, str, fileh)
    if (ierr.eq.0) call get_obj_string(ierr, bname, ofile(jfile)%bh)
    if (ierr.eq.0) then
       select case(ofile(jfile)%mode)
       case (mode_read)
          acc = 'read'
       case (mode_new)
          acc = 'new'
       case (mode_write)
          acc = 'write'
       case (mode_append)
          acc = 'append'
       case (mode_cycle)
          acc = 'cyclic'
       case (mode_persistent)
          acc = 'persistent'
       case (mode_terminate)
          acc = 'terminate'
       case default
          acc = 'unknown'
       end select
201    format('file:', A, 1x, A, '[', I0, '] > ', A)
       write(str, 201) trim(acc), trim(ofile(jfile)%name), &
            & ofile(jfile)%irec-1 + global_offset_bgn, trim(bname)
       if (is_msglev_DETAIL(lv)) then
          cname(:) = ' '
          if (ierr.eq.0) call get_header_lprops(ierr, cname, lpp, ofile(jfile)%h)
          if (ierr.eq.0) call get_domain_string(ierr, domain, cname, lpp)
          str = trim(str) // ' ' // trim(domain)
       endif
       call message(ierr, str, levm=-1, u=u, indent=2)
    endif
    ! call message(ierr, str, levm=-9)
    return
  end subroutine trace_file_access

  subroutine trace_buffer_access (ierr, bufh, u)
    use TOUZA_Std,only: join_list, choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: bufh
    integer,intent(in),optional :: u
    integer utmp
    character(len=256) :: str

    ierr = 0
    utmp = choice(-1, u)
    if (ierr.eq.0) call get_obj_string(ierr, str, bufh)
    call message(ierr, 'stack:' // str,u=utmp, indent=+2)
    return
  end subroutine trace_buffer_access

!!!_   . trace_operation
  subroutine trace_operation (ierr, aq, u)
    use TOUZA_Std,only: join_list, choice
    implicit none
    integer,      intent(out)         :: ierr
    type(queue_t),intent(in)          :: aq
    integer,      intent(in),optional :: u
    integer utmp
    integer push, pop
    character(len=256) :: str
    character(len=64)  :: opr

    ierr = 0
    utmp = choice(ulog, u)

    push = size(aq%lefth)
    pop  = aq%nopr

101 format('operator:', A, ' -', I0, '+', I0, 1x, A, ' >> ', A)
    call query_opr_name(ierr, opr, aq%term)

    write(str, 101) trim(opr), pop, push, trim(aq%desci), trim(aq%desco)
    call message(ierr, str, u=utmp, indent=+2)
    return
  end subroutine trace_operation

!!!_  - [b-]stack manager
!!!_   . push_stack
  subroutine push_stack &
       & (ierr, handle)
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(in)    :: handle

    ierr = 0
    if (mstack.ge.lstack.or.mstack.lt.0) then
       ierr = ERR_INSUFFICIENT_BUFFER
    else
       bstack(mstack) = handle
       ! mark(mstack) = 0
    endif
    mstack = mstack + 1
  end subroutine push_stack

!!!_   . mpush_stack
  subroutine mpush_stack &
       & (ierr, handle, n)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: handle(*)
    integer,intent(in)          :: n
    integer jb, je

    ierr = 0
    jb = mstack
    je = jb + n
    if (je.ge.lstack.or.jb.lt.0) then
       ierr = ERR_INSUFFICIENT_BUFFER
    else if (present(handle)) then
       bstack(jb:je-1) = handle(1:n)
    else
       bstack(jb:je-1) = -1 ! dummy
    endif
    mstack = mstack + n
  end subroutine mpush_stack

  !!!_  - pop_stack
  subroutine pop_stack &
       & (ierr, handle, keep, anchor)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)          :: ierr
    integer,intent(out),optional :: handle
    logical,intent(in),optional  :: keep
    logical,intent(in),optional  :: anchor
    ! integer news
    integer hbuf(1)

    call mpop_stack(ierr, hbuf, 1, keep, anchor)
    if (present(handle)) then
       if (ierr.eq.0) handle = hbuf(1)
    endif
  end subroutine pop_stack
!!!_   . mpop_stack
  subroutine mpop_stack &
       & (ierr, handle, n, keep, anchor)
    use TOUZA_Std,only: choice, condop
    implicit none
    integer,intent(out)          :: ierr
    integer,intent(out),optional :: handle(0:*)
    integer,intent(in)           :: n
    logical,intent(in),optional  :: keep
    logical,intent(in),optional  :: anchor   ! whether to include anchors
    integer jh, js
    ! integer jb, je
    integer adj

    ierr = 0
    adj = condop(choice(.TRUE., anchor), 0, 1)

    jh = n
    js = mstack

    do
       ! write(*, *) jh, js, '/', handle(0:n-1)
       if (jh.eq.0) exit
       if (js.le.0) then
          ierr = ERR_INSUFFICIENT_BUFFER
          call message(ierr, 'empty stack')
          return
       endif
       js = js - 1
       jh = jh - 1
       if (present(handle)) then
          handle(jh) = bstack(js)
       endif
       if (is_anchor(bstack(js))) jh = jh + adj
    enddo
    if (.not.choice(.FALSE.,keep)) mstack = js
  end subroutine mpop_stack

!!!_  - buffer manager
!!!_   . new_buffer
  subroutine new_buffer(ierr, handle, name)
    implicit none
    integer,         intent(out)         :: ierr
    integer,         intent(out)         :: handle
    character(len=*),intent(in),optional :: name
    integer jb

    ierr = 0

    jb = mbuffer
    mbuffer = mbuffer + 1
    if (jb.ge.lbuffer) then
       ierr = ERR_INSUFFICIENT_BUFFER
       handle = -1
    else
       handle = buf_i2handle(jb)
       obuffer(jb)%m = 0
       obuffer(jb)%stt = stt_none
       if (present(name)) then
          obuffer(jb)%name = name
       else
          obuffer(jb)%name = ' '
       endif
       obuffer(jb)%desc = ' '

       obuffer(jb)%cn(:) = ' '
       obuffer(jb)%ci(:) = -1
       obuffer(jb)%lpp(:)%bgn = 0
       obuffer(jb)%lpp(:)%end = 0
       obuffer(jb)%lpp(:)%stp = -1
    endif
  end subroutine new_buffer

!!!_   . alloc_buffer
  subroutine alloc_buffer(ierr, handle)
    use TOUZA_Nio,only: parse_header_size
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: handle
    integer jb
    integer n

    jb = buf_h2item(handle)
    ierr = min(0, jb)
    if (ierr.eq.0) then
       n = get_buffer_size(handle)
       ierr = min(0, n)
    endif
    if (ierr.eq.0) then
       call alloc_buffer_t(ierr, obuffer(jb), n)
    endif
  end subroutine alloc_buffer

!!!_   . alloc_buffer_t
  subroutine alloc_buffer_t &
       & (ierr, buf, n)
    implicit none
    integer,       intent(out)   :: ierr
    type(buffer_t),intent(inout) :: buf
    integer,       intent(in)    :: n

    integer nold

    ierr = 0
    if (associated(buf%vd)) then
       nold = size(buf%vd, 1)
       if (nold.lt.n) then
          deallocate(buf%vd, STAT=ierr)
          if (ierr.eq.0) allocate(buf%vd(0:n-1), STAT=ierr)
          if (ierr.eq.0) buf%m = n
       endif
    else
       if (ierr.eq.0) allocate(buf%vd(0:n-1), STAT=ierr)
       if (ierr.eq.0) buf%m = n
    endif
    return
  end subroutine alloc_buffer_t

!!!_   . get_buffer_size
  integer function get_buffer_size(handle) result(n)
    implicit none
    integer,intent(in) :: handle
    integer jb, jc
    jb = buf_h2item(handle)
    if (jb.ge.0) then
       n = 1
       do jc = 1, lcoor
          n = n * max(1, obuffer(jb)%lpp(jc)%end - obuffer(jb)%lpp(jc)%bgn)
       enddo
    else
       n = jb
    endif
  end function get_buffer_size

!!!_   . search_free_buffer
  subroutine search_free_buffer &
       & (ierr, lefth, n)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: lefth(*)
    integer,intent(in)  :: n

    integer jo, jb, hb, mb

    ierr = 0
    if (n.le.0) return

    lefth(1:n) = -1
    jb = 0
    mb = min(lbuffer, mbuffer)
    biter: do jo = 1, n
       do
          if (jb.ge.mb) exit
          hb = buf_i2handle(jb)
          jb = jb + 1
          if (obuffer(jb-1)%stt.eq.stt_locked &
               & .or. ANY(bstack(0:mstack-1).eq.hb)) cycle
          lefth(jo) = hb
          cycle biter
       enddo
       call new_buffer(ierr, hb)
       if (ierr.eq.0) lefth(jo) = hb
    enddo biter
  end subroutine search_free_buffer

!!!_   . is_buffer_locked()
  logical function is_buffer_locked(handle) result(b)
    implicit none
    integer,intent(in) :: handle
    integer jb
    jb = buf_h2item(handle)
    if (jb.lt.0) then
       b = .TRUE.
    else
       b = obuffer(jb)%stt.eq.stt_locked
    endif
  end function is_buffer_locked

!!!_   . register_predefined
  subroutine register_predefined(ierr)
    ! use TOUZA_Std,only: htb_init, new_htable
    implicit none
    integer,intent(out) :: ierr
    integer handle

    ierr = 0
    if (ierr.eq.0) call new_buffer_literal(ierr, handle, kv_dbl, ATAN2(ZERO, -ONE), 'PI', 'PI')
    if (ierr.eq.0) call new_buffer_literal(ierr, handle, kv_dbl, EXP(ONE),   'E',    'E')
    if (ierr.eq.0) call new_buffer_literal(ierr, handle, kv_dbl, HUGE(ZERO), 'HUGE', 'HUGE')
    if (ierr.eq.0) call new_buffer_literal(ierr, handle, kv_dbl, TINY(ZERO), 'TINY', 'TINY')

  end subroutine register_predefined

! !!!_   . register_obj
!   subroutine register_obj(ierr, handle, name)
!     use TOUZA_Std,only: reg_entry
!     implicit none
!     integer,         intent(out) :: ierr
!     integer,         intent(in)  :: handle
!     character(len=*),intent(in)  :: name
!     integer entr

!     ierr = 0
!     entr = reg_entry(name, htobj, handle)
!     ierr = min(0, entr)
!   end subroutine register_obj

!!!_  - file manager
!!!_   . new_file
  subroutine new_file &
       & (ierr, handle, name)
    implicit none
    integer,         intent(out) :: ierr
    integer,         intent(out) :: handle
    character(len=*),intent(in)  :: name
    integer jf

    ierr = 0
    jf = mfile
    mfile = mfile + 1
    if (jf.ge.lfile) then
       ierr = -1
       handle = -1
    else
       handle = file_i2handle(jf)
       call reset_file(ierr, ofile(jf), name)
    endif
  end subroutine new_file

!!!_   . reset_file
  subroutine reset_file(ierr, file, name, mode)
    use TOUZA_Std,only: choice
    implicit none
    integer,         intent(out)         :: ierr
    type(file_t),    intent(inout)       :: file
    character(len=*),intent(in),optional :: name
    integer,         intent(in),optional :: mode

    ierr = 0

    file%u = -1
    file%irec = 0
    file%nrec = -1
    file%h = ' '
    file%fmt = ' '
    file%mode = choice(mode_unset, mode)
    if (present(name)) then
       file%name = name
    else
       file%name = ' '
    endif
  end subroutine reset_file
!!!_   . read_file
  subroutine read_file(ierr, neof, nterm, file)
    use TOUZA_Std,only: new_unit, sus_open, is_error_match
    use TOUZA_Nio,only: nio_read_header, parse_header_size, nio_read_data, nio_skip_records
    use TOUZA_Nio,only: show_header, get_item, restore_item
    use TOUZA_Nio,only: hi_MISS, hi_ITEM, hi_DATE, hi_TIME
    implicit none
    integer,     intent(out)   :: ierr
    integer,     intent(inout) :: neof, nterm
    type(file_t),intent(inout) :: file
    character(len=litem) :: head(nitem)
    integer n
    integer jb
    character(len=128) :: txt
    character(len=litem) :: tstr
    integer :: dt(6)
    integer jerr
    integer xrec

    ierr = 0

    jb = 0
    if (ierr.eq.0) then
       if (file%bh.lt.0) call new_buffer(ierr, file%bh)
    endif
    if (ierr.eq.0) jb = buf_h2item(file%bh)

    if (ierr.eq.0) then
       if (file%u.lt.0) then
          if (file%mode.eq.mode_read) file%mode = def_read%mode
          file%u = new_unit()
          ierr = min(0, file%u)
          if (ierr.eq.0) then
             call sus_open(ierr, file%u, file%name, ACTION='R', STATUS='O')
             if (ierr.ne.0) then
                call message(ierr, 'failed to read open:'// trim(file%name))
                return
             endif
          endif
          file%irec = 0
          call init_read_rec(ierr, xrec, file)
       else
          call next_read_rec(ierr, xrec, file)
       endif
    endif
    ! cueing
    if (ierr.eq.0) call cue_read_header(ierr, head, file, xrec)
    if (ierr.eq.ERR_EXHAUST) then
       ierr = 0
       neof = neof + 1
       if (file%mode.eq.mode_terminate) nterm = nterm + 1
       return
    endif
    if (is_error_match(ierr, ERR_EOF)) return
    if (ierr.eq.0) call banner_record(ierr)
    if (ierr.eq.0) then
       file%h(:) = head(:)
       n = parse_header_size(file%h, 0, lazy=1)
       obuffer(jb)%k = suggest_type(file%h)
       if (ierr.eq.0) call get_item(ierr, file%h, obuffer(jb)%undef, hi_MISS, def=UNDEF)
    endif
    if (ierr.eq.0) call alloc_buffer_t(ierr, obuffer(jb), n)
    if (ierr.eq.0) then
       call nio_read_data(ierr, obuffer(jb)%vd, n, file%h, file%t, file%u)
       if (ierr.eq.0) file%irec = file%irec + 1
    endif
    if (ierr.eq.0) then
       call get_item(ierr, file%h, obuffer(jb)%desc, hi_ITEM)
       if (obuffer(jb)%desc.eq.' ') then
          obuffer(jb)%desc = obuffer(jb)%name
       endif
    endif
    if (ierr.eq.0) call get_header_lprops(ierr, obuffer(jb)%cn, obuffer(jb)%lpp, file%h)

    if (ierr.eq.0) then
       call get_item(jerr, file%h, dt(:), hi_DATE)
       if (jerr.ne.0) dt(:) = -1
       call restore_item(jerr, file%h, tstr, hi_TIME)
       if (jerr.ne.0) tstr = ' '
    endif
101 format('  read:', A, 1x, A, ' T = ', A, ' DATE = ', I0, '/', I0, '/', I0, 1x, I2.2, ':', I2.2, ':', I2.2)
    write(txt, 101) trim(obuffer(jb)%name), trim(obuffer(jb)%desc), trim(adjustl(tstr)), dt(:)
    call message(ierr, txt, levm=msglev_normal)

    return
  end subroutine read_file

!!!_   . cue_read_header
  subroutine cue_read_header &
       & (ierr, head, file, rec)
    use TOUZA_Std,only: is_error_match
    use TOUZA_Nio,only: nio_skip_records, nio_read_header
    implicit none
    integer,         intent(out)   :: ierr
    character(len=*),intent(out)   :: head(*)
    type(file_t),    intent(inout) :: file
    integer,         intent(in)    :: rec

    type(loop_t),pointer :: recf
    integer nrf
    integer nfwd, nsuc
    integer xrec

    ierr = 0
    recf => file%recf(file%jrf)
    nrf = size(file%recf)
    ! write(*, *) 'cue/recf', nrf, file%jrf, recf, trim(file%name)
    xrec = rec
    proc: do
       ! write(*, *) 'cue/0', xrec, '/', file%irec, file%nrec, '/', recf
       if (file%nrec.ge.0.and.xrec.ge.file%nrec) then
          ierr = ERR_EOF
       else
          if (xrec.lt.file%irec) then
             rewind(file%u, IOSTAT=ierr)
             if (ierr.ne.0) then
                ierr = ERR_PANIC
                call message(ierr, 'rewind failed')
                return
             endif
             file%irec = 0
          endif

          nfwd = xrec - file%irec
          call nio_skip_records(ierr, nfwd, file%u, nskip=nsuc)
          file%irec = file%irec + nsuc
          if (xrec.lt.file%irec) ierr = ERR_EOF
          ! write(*, *) 'cue/00', ierr, nfwd, nsuc
       endif
       ! write(*, *) 'cue/1', xrec, '/', file%irec, file%nrec, '/', recf
       if (ierr.eq.0) call nio_read_header(ierr, head, file%t, file%u)
       ! write(*, *) 'cue/2', ierr

       if (is_error_match(ierr, ERR_EOF)) then
          file%nrec = file%irec
          if (recf%end.lt.0.and.recf%bgn.le.file%irec) then
             ! recf%end = file%nrec
             file%jrf = file%jrf + 1
             if (file%jrf.ge.nrf) then
                ierr = ERR_EXHAUST     ! exhaust record filters
             else
                recf => file%recf(file%jrf)
                xrec = recf%bgn
                cycle proc
             endif
          else
             call message(ierr, 'reach eof')
          endif
       endif
       exit proc
    enddo proc
    ! write(*, *) 'cue/8', xrec, '/', file%irec, file%nrec, '/', recf
    ! write(*, *) 'cue/9', ierr
  end subroutine cue_read_header
!!!_   . init_read_rec
  subroutine init_read_rec(ierr, xrec, file)
    implicit none
    integer,     intent(out)   :: ierr
    integer,     intent(out)   :: xrec
    type(file_t),intent(inout) :: file

    ierr = 0
    file%jrf = 0
    xrec = file%recf(file%jrf)%bgn
  end subroutine init_read_rec

!!!_   . next_read_rec
  subroutine next_read_rec(ierr, xrec, file)
    implicit none
    integer,     intent(out)   :: ierr
    integer,     intent(out)   :: xrec
    type(file_t),intent(inout) :: file

    type(loop_t),pointer :: recf
    integer nrf

    ierr = 0
    nrf = size(file%recf)
    if (file%jrf.lt.nrf) then
       recf => file%recf(file%jrf)
       if (recf%stp.gt.0) then
          xrec = file%irec + (recf%stp - 1)
          if (recf%end.lt.0) then
             continue
          else if (xrec.lt.recf%end) then
             continue
          else
             file%jrf = file%jrf + 1
             if (file%jrf.lt.nrf) then
                recf => file%recf(file%jrf)
                xrec = recf%bgn
             else
                ierr = ERR_EXHAUST
                xrec = -1
             endif
          endif
       else if (recf%stp.lt.0) then
          xrec = file%irec + (recf%stp - 1)
          if (xrec.gt.recf%end) then
             continue
          else
             file%jrf = file%jrf + 1
             if (file%jrf.lt.nrf) then
                recf => file%recf(file%jrf)
                xrec = recf%bgn
             else
                ierr = ERR_EXHAUST
                xrec = -1
             endif
          endif
       else
          ierr = ERR_PANIC
          xrec = -1
          call message(ierr, 'invalid record filter')
          return
       endif
    else
       ierr = ERR_EXHAUST
    endif
  end subroutine next_read_rec

!!!_   . write_file
  subroutine write_file(ierr, file)
    use TOUZA_Std,only: new_unit, sus_open, is_error_match
    use TOUZA_Nio,only: nio_write_header, parse_header_size, nio_write_data
    use TOUZA_Nio,only: get_default_header, show_header
    use TOUZA_Nio,only: REC_DEFAULT, REC_BIG
    use TOUZA_Nio,only: put_item, get_item, restore_item
    use TOUZA_Nio,only: hi_MISS, hi_ITEM, hi_DFMT, hi_EDIT1
    use TOUZA_Nio,only: hi_DATE, hi_TIME
    implicit none
    integer,     intent(out)   :: ierr
    type(file_t),intent(inout) :: file
    integer n
    integer jb, jrefh
    character(len=2) stt, pos
    character(len=128) :: txt
    character(len=litem) :: tstr
    integer :: dt(6)
    integer jerr

    ierr = 0
    jb = 0
    if (ierr.eq.0) then
       if (file%bh.lt.0) ierr = ERR_PANIC
    endif
    if (ierr.eq.0) jb = buf_h2item(file%bh)
    ! write(*, *) 'write/handle', ierr, jb, file%bh

    if (ierr.eq.0) then
       if (file%u.lt.0) then
          file%u = new_unit()
          ierr = min(0, file%u)
          if (ierr.eq.0) then
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
             call sus_open(ierr, file%u, file%name, ACTION='W', STATUS=stt, POSITION=pos)
             if (ierr.ne.0) then
                write(*, *) 'failed to write open:', trim(file%name)
                return
             endif
          endif
          file%irec = 0
       endif
    endif
    ! write(*, *) 'write/open', ierr

    if (ierr.eq.0) then
       jrefh = file_h2item(wrefh)
       if (jrefh.ge.0) then
          file%h = ofile(jrefh)%h
       else
          call get_default_header(file%h)
       endif
       call put_header_lprops(ierr, file%h, obuffer(jb)%cn, obuffer(jb)%lpp)
    endif
    if (ierr.eq.0) call put_item(ierr, file%h, obuffer(jb)%undef, hi_MISS)
    if (ierr.eq.0) call put_item(ierr, file%h, obuffer(jb)%desc,  hi_ITEM, tol=1)
    if (ierr.eq.0) call put_item(ierr, file%h, obuffer(jb)%desc,  hi_EDIT1, 0, tol=1)
    if (ierr.eq.0) call put_item(ierr, file%h, 'UR4',  hi_DFMT)
    call message(ierr, 'put_item', levm=-9)

    if (ierr.eq.0) then
       ! file%t = REC_DEFAULT
       file%t = REC_BIG
       call nio_write_header(ierr, file%h, file%t, file%u)
       ! if (is_error_match(ierr, ERR_EOF)) then
       !    ierr = 0
       !    file%nrec = file%irec + 1
       !    return
       ! endif
       if (ierr.eq.0) file%irec = file%irec + 1
       call message(ierr, 'write_header', levm=-9)
    endif
    if (ierr.eq.0) then
       n = parse_header_size(file%h, 0, lazy=1)
    endif
    if (ierr.eq.0) then
       call nio_write_data(ierr, obuffer(jb)%vd, n, file%h, file%t, file%u)
    endif
    if (ierr.eq.0) then
       call get_item(jerr, file%h, dt(:), hi_DATE)
       if (jerr.ne.0) dt(:) = -1
       call restore_item(jerr, file%h, tstr, hi_TIME)
       if (jerr.ne.0) tstr = ' '
    endif
101 format('  write:', A, 1x, A, ' T = ', A, ' DATE = ', I0, '/', I0, '/', I0, 1x, I2.2, ':', I2.2, ':', I2.2)
    write(txt, 101) trim(obuffer(jb)%name), trim(obuffer(jb)%desc), trim(adjustl(tstr)), dt(:)
    call message(ierr, txt, levm=msglev_normal)

    if (ierr.ne.0) call show_header(ierr, file%h)
    return
  end subroutine write_file

!!!_   . suggest_type
  integer function suggest_type(head) result(k)
    use TOUZA_Nio_record
    use TOUZA_Nio_header,only: hi_DFMT, get_item
    implicit none
    character(len=*),intent(in) :: head(*)
    integer jerr
    character(len=litem) :: vp
    integer kfmt
    jerr = 0
    if (jerr.eq.0) call get_item(jerr, head, vp, hi_DFMT)
    if (jerr.eq.0) call parse_record_fmt(jerr, kfmt, vp)
    if (jerr.eq.0) then
       select case (kfmt)
       case (GFMT_UR4, GFMT_MR4)
          k = kv_flt
       case (GFMT_UR8, GFMT_MR8)
          k = kv_dbl
       case (GFMT_UI4, GFMT_MI4)
          k = kv_int
       case (GFMT_URC,GFMT_URC2)
          k = kv_dbl
       case (GFMT_URY:GFMT_URYend, GFMT_MRY:GFMT_MRYend)
          k = kv_dbl
       case (GFMT_URT:GFMT_URTend, GFMT_MRT:GFMT_MRTend)
          k = kv_dbl
       case default
          k = kv_dbl
       end select
    endif
    if (jerr.ne.0) k = jerr
  end function suggest_type

!!!_   . get_header_lprops
  subroutine get_header_lprops(ierr, name, lpp, head)
    use TOUZA_Nio,only: get_item, hi_AITM1, hi_AITM2, hi_AITM3
    use TOUZA_Nio,only: get_header_cprop
    implicit none
    integer,              intent(out)   :: ierr
    character(len=*),     intent(out)   :: name(*)
    type(loop_t),optional,intent(inout) :: lpp(*)
    character(len=*),     intent(in)    :: head(*)

    integer jc
    integer irange(2, lcoor)

    ierr = 0
    irange = 0
    do jc = 1, lcoor
       call get_header_cprop(name(jc), irange(1, jc), head, jc)
    enddo
    if (present(lpp)) then
       do jc = 1, lcoor
          if (irange(2, jc).le.0) then
             lpp(jc)%bgn = irange(1, jc)
             lpp(jc)%end = irange(2, jc)
             lpp(jc)%stp = 0
          else
             lpp(jc)%bgn = irange(1, jc) - 1
             lpp(jc)%end = irange(2, jc)
             lpp(jc)%stp = 1
          endif
       enddo
    endif

  end subroutine get_header_lprops

!!!_   . put_header_lprops
  subroutine put_header_lprops(ierr, head, name, lpp)
    use TOUZA_Nio,only: put_header_cprop
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: head(*)
    character(len=*),intent(in)  :: name(*)
    type(loop_t),    intent(in)  :: lpp(*)

    integer jc
    integer irange(2)

    ierr = 0

    do jc = 1, lcoor
       irange(1) = lpp(jc)%bgn
       irange(2) = lpp(jc)%end
       if (irange(2).gt.0) irange(1) = irange(1) + 1
       if (ierr.eq.0) call put_header_cprop(ierr, head, name(jc), irange, jc)
    enddo

  end subroutine put_header_lprops

!!!_   . is_read_buffer
  logical function is_read_buffer(handle) result(b)
    integer,intent(in) :: handle
    integer jf
    jf = file_h2item(handle)
    if (jf.lt.0) then
       b = .FALSE.
    else
       b = is_read_mode(ofile(jf)%mode)
    end if
  end function is_read_buffer

!!!_   . is_read_mode
  logical function is_read_mode(mode) result(b)
    implicit none
    integer,intent(in) :: mode
    b = mode.le.mode_read
  end function is_read_mode
!!!_  - handle manager
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

!!!_   . get_obj_list
  subroutine get_obj_list &
       & (ierr, str, handle, n)
    use TOUZA_Std,only: join_list
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    integer,         intent(in)  :: handle(:)
    integer,         intent(in)  :: n
    character(len=lname) :: buf(n)
    integer j
    ierr = 0
    do j = 1, n
       if (ierr.eq.0) call get_obj_string(ierr, buf(j), handle(j))
       ! write(*, *) j, ierr, buf(j)
    enddo
    if (ierr.eq.0) call join_list(ierr, str, buf(1:n), sep=CHAR(0))
    ! if (ierr.eq.0) call join_list(ierr, str, buf(1:n))
    ! write(*, *) ierr, str
  end subroutine get_obj_list

!!!_   . get_obj_string
  subroutine get_obj_string &
       & (ierr, str, handle, levv)
    use TOUZA_Std,only: query_name, choice
    use TOUZA_Nio,only: get_header_cprop
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    integer,         intent(in)  :: handle
    integer,optional,intent(in)  :: levv
    integer j
    integer lv
    character(len=litem+16) :: buf

    ierr = 0
    lv = choice(lev_verbose, levv)
111 format('{', I0, '}')
112 format('{', A, '}')

102 format('file[', I0, ']')
103 format('unknown[', I0, ']')
104 format('opr[', A, ']')
105 format('anchor[', I0, ']')
106 format('anchor[-]')
    j = file_h2item(handle)
    if (j.ge.0) then
       write(str, 102) handle
       return
    endif
    j = buf_h2item(handle)
    if (j.ge.0) then
       if (obuffer(j)%name.ne.' ') then
          write(str, 112) trim(obuffer(j)%name)
       else
          write(str, 111) handle
       endif
       return
    endif
    j = anchor_h2level(handle)
    if (j.gt.0) then
       write(str, 105) j
    else if (j.eq.0) then
       write(str, 106)
    else
       call query_opr_name(ierr, buf, handle)
       if (ierr.eq.0) then
          write(str, 104) trim(buf)
       endif
       if (ierr.ne.0) write(str, 103) handle
    endif
  end subroutine get_obj_string

!!!_   . get_domain_string
  subroutine get_domain_string(ierr, str, name, lpp)
    use TOUZA_Std,only: join_list
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: str
    character(len=*),intent(in)  :: name(*)
    type(loop_t),    intent(in)  :: lpp(*)

    character(len=64) cstr(lcoor)

    integer jc, nc
    integer b,  e, s
    integer bb, ee

    ierr = 0
    nc = 1
    do jc = 1, lcoor
       b = lpp(jc)%bgn
       e = lpp(jc)%end
       s = lpp(jc)%stp
       bb = b + global_offset_bgn
       ee = e + global_offset_end
101    format(I0, ':', I0)
102    format(A, ',', I0, ':', I0)
       if (name(jc).eq.' ') then
          if (s.gt.0) then
             write(cstr(nc), 101) bb, ee
          else if (b.eq.0.and.e.eq.0) then
             cstr(nc) = '-'
          else
             write(cstr(nc), 101) bb, ee
          endif
       else
          write(cstr(nc), 102) trim(name(jc)), bb, ee
       endif
       nc = nc + 1
    enddo
    call join_list(ierr, str, cstr(1:nc-1), ldelim='[', rdelim=']')
  end subroutine get_domain_string

!!!_  - stacked operation dispatcher
!!!_   . batch_operation
  subroutine batch_operation(ierr, irecw, levv)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: irecw
    integer,intent(in)  :: levv
    integer jq
    integer hterm
    integer termk
    integer push, pop
    integer jerr
    integer jfile
    integer neof, nterm

    ierr = 0

    neof = 0
    nterm = 0

    call banner_record(ierr, irecw)

    do jfile = 0, min(mfile, lfile) - 1
       if (is_read_mode(ofile(jfile)%mode)) then
          if (ierr.eq.0) call read_file(ierr, neof, nterm, ofile(jfile))
       endif
    enddo
    if (rcount.gt.0) then
       if (neof.eq.rcount) then
          ierr = ERR_FINISHED
          return
       endif
       if (nterm.gt.0) then
          ierr = ERR_EOF
          call message(ierr, 'insufficient records')
          call show_files(jerr)
       endif
    endif
    if (ierr.ne.0) return

    mstack = 0
    do jq = 0, min(mqueue, lqueue) - 1
       hterm = aqueue(jq)%term
       termk = handle_type(hterm)
       if (aqueue(jq)%desci.eq.' ') call set_queue_descr(ierr, aqueue(jq))
       select case(termk)
       case(hk_file)
          if (ierr.eq.0) call push_file(ierr, hterm, levv)
       case(hk_buffer)
          if (ierr.eq.0) call push_buffer(ierr, hterm, levv)
       case(hk_opr)
          push = size(aqueue(jq)%lefth)
          pop = aqueue(jq)%nopr
          if (ierr.eq.0) then
             call apply_operator &
                  & (ierr, hterm, aqueue(jq)%lefth, pop, push, aqueue(jq)%cmode, levv)
          endif
       end select
       if (is_msglev_DETAIL(levv)) then
          call trace_queue(jerr, aqueue(jq), levv)
          call show_stack(jerr)
       endif
       if (ierr.ne.0) exit
    enddo

    if (ierr.eq.0) then
       if (rcount.eq.0) ierr = ERR_FINISHED
    endif
  end subroutine batch_operation

!!!_   . banner_record
  subroutine banner_record(ierr, irecw)
    integer,intent(out)         :: ierr
    integer,intent(in),optional :: irecw   ! store if present, release otherwise
    integer,save :: brec = -1
    ierr = 0
    if (present(irecw)) then
       brec = irecw
    else if (brec.ge.0) then
       call message &
            & (ierr, 'record:', (/brec + global_offset_bgn/), &
            &  fmt='(I0)', levm=msglev_normal+1)
       brec = -1
    endif
  end subroutine banner_record

!!!_   . push_file
  subroutine push_file &
       & (ierr, handle, levv)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: handle
    integer,intent(in)  :: levv
    integer jfile

    jfile = file_h2item(handle)
    ierr = min(0, jfile)
    if (ierr.eq.0) then
       if (is_read_mode(ofile(jfile)%mode)) then
          if (ierr.eq.0) call push_stack(ierr, ofile(jfile)%bh)
       else
          call write_file(ierr, ofile(jfile))
          if (ierr.eq.0) call pop_stack(ierr)
       endif
    endif
  end subroutine push_file

!!!_   . push_buffer
  subroutine push_buffer &
       & (ierr, handle, levv)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: handle
    integer,intent(in)  :: levv
    ierr = 0
    if (ierr.eq.0) call push_stack(ierr, handle)
  end subroutine push_buffer

!!!_  - operations
!!!_   . apply_operator
  subroutine apply_operator &
       & (ierr, handle, lefth, pop, push, cmode, levv)
    implicit none
    integer,intent(out)   :: ierr
    integer,intent(in)    :: handle
    integer,intent(in)    :: lefth(*)
    integer,intent(in)    :: push, pop
    integer,intent(in)    :: cmode
    integer,intent(in)    :: levv

    integer,parameter :: lmaxo = 8
    integer righth(pop)
    integer j, jb
    character(len=8) :: opr

    ierr = 0
    ! write(*, *) rlev, size(lefth)
    if (ierr.eq.0) call mpop_stack(ierr, righth, pop)
    if (ierr.eq.0) then
!!!_    * output
       if (handle.eq.opr_OUTPUT) then
          call flush_buffer(ierr, pop, righth, cmode_null)
       else if (handle.eq.opr_FLUSH) then
          call flush_buffer(ierr, pop, righth, cmode)
!!!_    * operators
       else if (handle.eq.opr_NEG) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_NEG)
       else if (handle.eq.opr_INV) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_INV)
       else if (handle.eq.opr_ABS) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_ABS)
       else if (handle.eq.opr_SQR) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_SQR)
       else if (handle.eq.opr_SQRT) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_SQRT)
       else if (handle.eq.opr_SIGN) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_SIGN)
       else if (handle.eq.opr_ZSIGN) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_ZSIGN)
       else if (handle.eq.opr_FLOOR) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_FLOOR)
       else if (handle.eq.opr_CEIL) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_CEIL)
       else if (handle.eq.opr_ROUND) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_ROUND)
       else if (handle.eq.opr_TRUNC) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_TRUNC)
       else if (handle.eq.opr_INT) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_TRUNC)
       else if (handle.eq.opr_EXP) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_EXP)
       else if (handle.eq.opr_LOG) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_LOG)
       else if (handle.eq.opr_LOG10) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_LOG10)
       else if (handle.eq.opr_SIN) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_SIN)
       else if (handle.eq.opr_COS) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_COS)
       else if (handle.eq.opr_TAN) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_TAN)
       else if (handle.eq.opr_TANH) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_TANH)
       else if (handle.eq.opr_ASIN) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_ASIN)
       else if (handle.eq.opr_ACOS) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_ACOS)
       else if (handle.eq.opr_EXPONENT) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_EXPONENT)
       else if (handle.eq.opr_FRACTION) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_FRACTION)
       else if (handle.eq.opr_NOT) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_NOT, .TRUE.)
       else if (handle.eq.opr_BOOL) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_BOOL, .TRUE.)
       else if (handle.eq.opr_BIN) then
          call apply_elem_UNARY(ierr, handle, lefth(1:push), righth(1:pop), elem_BIN, .TRUE.)
       else if (handle.eq.opr_EQ) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_EQ, .TRUE.)
       else if (handle.eq.opr_NE) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_NE, .TRUE.)
       else if (handle.eq.opr_LT) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_LT, .TRUE.)
       else if (handle.eq.opr_GT) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_GT, .TRUE.)
       else if (handle.eq.opr_LE) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_LE, .TRUE.)
       else if (handle.eq.opr_GE) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_GE, .TRUE.)
       else if (handle.eq.opr_EQU) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_EQU, .TRUE.)
       else if (handle.eq.opr_NEU) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_NEU, .TRUE.)
       else if (handle.eq.opr_LTU) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_LTU, .TRUE.)
       else if (handle.eq.opr_GTU) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_GTU, .TRUE.)
       else if (handle.eq.opr_LEU) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_LEU, .TRUE.)
       else if (handle.eq.opr_GEU) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_GEU, .TRUE.)
       else if (handle.eq.opr_AND) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_AND)
       else if (handle.eq.opr_MASK) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_MASK)
       else if (handle.eq.opr_ADD) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_ADD)
       else if (handle.eq.opr_SUB) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_SUB)
       else if (handle.eq.opr_MUL) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_MUL)
       else if (handle.eq.opr_DIV) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_DIV)
       else if (handle.eq.opr_IDIV) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_IDIV)
       else if (handle.eq.opr_MOD) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_MOD)
       else if (handle.eq.opr_POW) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_POW)
       else if (handle.eq.opr_ATAN2) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_ATAN2)
       else if (handle.eq.opr_SCALE) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_SCALE)
       else if (handle.eq.opr_MIN) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_MIN)
       else if (handle.eq.opr_MAX) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_MAX)
       else if (handle.eq.opr_EQF) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_EQF)
       else if (handle.eq.opr_NEF) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_NEF)
       else if (handle.eq.opr_LTF) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_LTF)
       else if (handle.eq.opr_GTF) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_GTF)
       else if (handle.eq.opr_LEF) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_LEF)
       else if (handle.eq.opr_GEF) then
          call apply_elem_BINARY(ierr, handle, lefth(1:push), righth(1:pop), elem_GEF)
       else if (handle.eq.opr_OR) then
          call apply_elem_BINARY_lazy(ierr, handle, lefth(1:push), righth(1:pop), elem_OR)
       else if (handle.eq.opr_XOR) then
          call apply_elem_BINARY_lazy(ierr, handle, lefth(1:push), righth(1:pop), elem_XOR)
       else if (handle.eq.opr_LMASK) then
          call apply_elem_BINARY_lazy(ierr, handle, lefth(1:push), righth(1:pop), elem_MASK)
       else if (handle.eq.opr_LADD) then
          call apply_elem_BINARY_lazy(ierr, handle, lefth(1:push), righth(1:pop), elem_ADD, ZERO)
       else if (handle.eq.opr_LSUB) then
          call apply_elem_BINARY_lazy(ierr, handle, lefth(1:push), righth(1:pop), elem_SUB, ZERO)
       else if (handle.eq.opr_LMUL) then
          call apply_elem_BINARY_lazy(ierr, handle, lefth(1:push), righth(1:pop), elem_MUL, ONE)
       else if (handle.eq.opr_LDIV) then
          call apply_elem_BINARY_lazy(ierr, handle, lefth(1:push), righth(1:pop), elem_DIV, ONE)
       else if (handle.eq.opr_LMIN) then
          call apply_elem_BINARY_lazy(ierr, handle, lefth(1:push), righth(1:pop), elem_LMIN, ULIMIT)
       else if (handle.eq.opr_LMAX) then
          call apply_elem_BINARY_lazy(ierr, handle, lefth(1:push), righth(1:pop), elem_LMAX, LLIMIT)
!!!_    * ignored
       else if (grp_system_bgn.le.handle.and.handle.lt.grp_system_end) then
          continue
       else if (grp_stack_bgn.le.handle.and.handle.lt.grp_stack_end) then
          continue
!!!_    * reserved
       else
          call query_opr_name(ierr, opr, handle)
          ierr = ERR_NOT_IMPLEMENTED
          call message(ierr, 'reserved operator ' // trim(opr))
       endif
    endif
    if (ierr.eq.0) then
       if (ANY(handle.eq.(/opr_IDIV, opr_INT/))) then
          do j = 1, push
             jb = buf_h2item(lefth(j))
             obuffer(jb)%k = kv_int
          enddo
       endif
    endif
    if (ierr.eq.0) then
       if (push.gt.0) call mpush_stack(ierr, lefth, push)
    endif
  end subroutine apply_operator

!!!_   . flush_buffer
  subroutine flush_buffer(ierr, pop, righth, cmode, u)
    use TOUZA_Std,only: choice, join_list
    use TOUZA_Nio,only: show_header, parse_header_size
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: pop
    integer,intent(in)          :: righth(0:*)
    integer,intent(in)          :: cmode
    integer,intent(in),optional :: u

    integer jinp
    integer jb, hb

    integer nbuf
    integer bufh(0:pop-1)
    integer bufj(0:pop-1)

    ierr = 0

    nbuf = 0
    do jinp = 0, pop - 1
       hb = righth(jinp)
       jb = buf_h2item(hb)
       if (jb.ge.0) then
          bufj(nbuf) = jb
          bufh(nbuf) = hb
          nbuf = nbuf + 1
       endif
    enddo

    select case(cmode)
    case (cmode_null)
       call flush_buffer_div(ierr, nbuf, bufh, bufj, u)
    case default
       call flush_buffer_inclusive(ierr, nbuf, bufh, bufj, u)
    end select
  end subroutine flush_buffer

!!!_   . flush_buffer_div
  subroutine flush_buffer_div(ierr, nbuf, bufh, bufj, u)
    use TOUZA_Std,only: choice, join_list
    use TOUZA_Nio,only: show_header, parse_header_size
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: nbuf
    integer,intent(in)          :: bufh(0:*)
    integer,intent(in)          :: bufj(0:*)
    integer,intent(in),optional :: u

    integer utmp
    integer jb, hb
    integer jbgnL(lcoor), iterL(lcoor), strdL(lcoor)
    integer jX1, jX2, jX3
    integer jL1, jL2, jL3, jL0

    integer jbuf
    character(len=64) :: val

    ierr = 0
    utmp = choice(-1, u)

    do jbuf = 0, nbuf - 1
       jb = bufj(jbuf)
       hb = bufh(jbuf)
202    format('# stack[', I0, '] ', A)
       if (utmp.ge.0) then
          write(utmp, 202) jbuf + global_offset_bgn, trim(obuffer(jb)%desc)
       else if (utmp.eq.-1) then
          write(*,    202) jbuf + global_offset_bgn, trim(obuffer(jb)%desc)
       endif
       if (ierr.eq.0) then
          call get_buffer_strides_h &
               & (ierr, jbgnL, iterL, strdL, hb)
       endif
       jbgnL(:) = jbgnL(:) + global_offset_bgn
201    format(I0, 1x, I0, 1x, I0, 1x, A)
       do jX3 = 0, iterL(3) - 1
          jL3 = jbgnL(3) + jX3
          do jX2 = 0, iterL(2) - 1
             jL2 = jbgnL(2) + jX2
             do jX1 = 0, iterL(1) - 1
                jL1 = jbgnL(1) + jX1
                jL0 = jX1 * strdL(1) + jX2 * strdL(2) + jX3 * strdL(3)
                if (obuffer(jb)%vd(jL0).eq.obuffer(jb)%undef) then
                   val = '_'
                else
                   select case(obuffer(jb)%k)
                   case (kv_int)
                      write(val, afmt_int) INT(obuffer(jb)%vd(jL0))
                   case (kv_flt)
                      write(val, afmt_flt) REAL(obuffer(jb)%vd(jL0), kind=KFLT)
                   case (kv_dbl)
                      write(val, afmt_dbl) REAL(obuffer(jb)%vd(jL0), kind=KDBL)
                   case default
                      val = '*'
                   end select
                end if
                if (utmp.ge.0) then
                   write(utmp, 201) jL1, jL2, jL3, trim(val)
                else if (utmp.eq.-1) then
                   write(*,    201) jL1, jL2, jL3, trim(val)
                endif
             enddo
          enddo
       enddo
    enddo
  end subroutine flush_buffer_div

!!!_   . flush_buffer_inclusive
  subroutine flush_buffer_inclusive(ierr, nbuf, bufh, bufj, u)
    use TOUZA_Std,only: choice, join_list
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: nbuf
    integer,intent(in)          :: bufh(0:*)
    integer,intent(in)          :: bufj(0:*)
    integer,intent(in),optional :: u

    integer utmp
    integer jb, hb

    integer,parameter    :: lline = 1024
    character(len=lline) :: aline
    integer jbuf
    type(buffer_t) :: obuf
    integer jbgnL(lcoor),           iterL(lcoor),           strdL(lcoor)
    integer jbgnR(lcoor, 0:nbuf-1), jendR(lcoor, 0:nbuf-1), strdR(lcoor, 0:nbuf-1)

    integer jX1, jX2, jX3
    integer jL1, jL2, jL3
    integer jR1, jR2, jR3, jR0

    character(len=64) :: vals(0:nbuf-1)
    character(len=64) :: name

    ierr = 0
    utmp = choice(-1, u)

    ! write(*, *) nbuf, pop, bufh(0:nbuf-1)

    if (ierr.eq.0) call inclusive_buffer(ierr, obuf, bufh(0:nbuf-1), cmode_inclusive)
    if (ierr.eq.0) call get_buffer_strides(ierr, jbgnL, iterL, strdL, obuf)
    ! write(*, *) jbgnL
    ! write(*, *) iterL
    ! write(*, *) strdL

    do jbuf = 0, nbuf - 1
       if (ierr.eq.0) then
          hb = bufh(jbuf)
          call get_buffer_strides_h &
               & (ierr, jbgnR(:,jbuf), jendR(:,jbuf), strdR(:,jbuf), hb, jbgnL, iterL)
          jendR(:, jbuf) = jendR(:, jbuf) + jbgnR(:,jbuf)
          ! write(*, *) jbgnR(:,jbuf), jbuf
          ! write(*, *) jendR(:,jbuf), jbuf
          ! write(*, *) strdR(:,jbuf), jbuf
       endif
    enddo

    jbgnL(:) = jbgnL(:) + global_offset_bgn

    do jbuf = 0, nbuf - 1
       jb = bufj(jbuf)
       hb = bufh(jbuf)
202    format('# ', I0, 1x, A, 1x, A)
       call get_obj_string(ierr, name, hb)
       if (utmp.ge.0) then
          write(utmp, 202) jbuf + global_offset_bgn, trim(name), trim(obuffer(jb)%desc)
       else if (utmp.eq.-1) then
          write(*,    202) jbuf + global_offset_bgn, trim(name), trim(obuffer(jb)%desc)
       endif
    enddo

201 format(I0, 1x, I0, 1x, I0, 1x, A)
    do jX3 = 0, iterL(3) - 1
       jL3 = jbgnL(3) + jX3
       do jX2 = 0, iterL(2) - 1
          jL2 = jbgnL(2) + jX2
          do jX1 = 0, iterL(1) - 1
             jL1 = jbgnL(1) + jX1
             do jbuf = 0, nbuf - 1
                jb = bufj(jbuf)
                if (jX1.lt.jbgnR(1,jbuf).or.jendR(1,jbuf).le.jX1 &
                     & .or. jX2.lt.jbgnR(2,jbuf).or.jendR(2,jbuf).le.jX2 &
                     & .or. jX3.lt.jbgnR(3,jbuf).or.jendR(3,jbuf).le.jX3) then
                   vals(jbuf) = '.'
                else
                   jR1 = jX1 - jbgnR(1,jbuf)
                   jR2 = jX2 - jbgnR(2,jbuf)
                   jR3 = jX3 - jbgnR(3,jbuf)
                   jR0 = jR1 * strdR(1,jbuf) + jR2 * strdR(2,jbuf) + jR3 * strdR(3,jbuf)
                   if (obuffer(jb)%vd(jR0).eq.obuffer(jb)%undef) then
                      vals(jbuf) = '_'
                   else
                      select case(obuffer(jb)%k)
                      case (kv_int)
                         write(vals(jbuf), afmt_int) INT(obuffer(jb)%vd(jR0))
                      case (kv_flt)
                         write(vals(jbuf), afmt_flt) REAL(obuffer(jb)%vd(jR0), kind=KFLT)
                      case (kv_dbl)
                         write(vals(jbuf), afmt_dbl) REAL(obuffer(jb)%vd(jR0), kind=KDBL)
                      case default
                         vals(jbuf) = '*'
                      end select
                   end if
                endif
             enddo
             call join_list(ierr, aline, vals(0:nbuf-1))
             if (utmp.ge.0) then
                write(utmp, 201) jL1, jL2, jL3, trim(aline)
             else if (utmp.eq.-1) then
                write(*,    201) jL1, jL2, jL3, trim(aline)
             endif
          enddo
       enddo
    enddo

  end subroutine flush_buffer_inclusive

!!!_   . copy_set_header
  subroutine copy_set_header &
       & (ierr, bufh, bref, pop)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: bufh ! output buffer
    integer,intent(in)  :: bref ! reference buffer
    integer,intent(in)  :: pop

    integer jref
    integer jb

    ierr = 0
    if (pop.gt.0) then
       jref = buf_h2item(bref)
       jb = buf_h2item(bufh)
       obuffer(jb)%lpp = obuffer(jref)%lpp
       obuffer(jb)%cn  = obuffer(jref)%cn
       obuffer(jb)%undef = obuffer(jref)%undef
    else
       jb = buf_h2item(bufh)
       obuffer(jb)%undef = UNDEF
       obuffer(jb)%lpp(:)%bgn = 0
       obuffer(jb)%lpp(:)%end = 1
       obuffer(jb)%lpp(:)%stp = -1
       obuffer(jb)%cn = ' '
    endif
    return
  end subroutine copy_set_header

!!!_   . inclusive_buffer_h - set result buffer as inclusive domain (with allocation)
  subroutine inclusive_buffer_h &
       & (ierr, hbufo, hbufi, mode)
    use TOUZA_std,only: find_first, choice
    use TOUZA_Nio,only: parse_header_size, put_header_cprop
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: hbufo    ! output buffer handle
    integer,intent(in)          :: hbufi(:) ! input buffer handles
    integer,intent(in),optional :: mode     ! compromise mode.  mandatory for non-unary operators

    integer jb

    ierr = 0
    jb = buf_h2item(hbufo)
    ierr = min(0, jb)
    if (ierr.eq.0) call inclusive_buffer(ierr, obuffer(jb), hbufi, mode)
    if (ierr.eq.0) call alloc_buffer(ierr, hbufo)
  end subroutine inclusive_buffer_h

!!!_   . inclusive_buffer - set result buffer as inclusive domain (core)
  subroutine inclusive_buffer &
       & (ierr, buf, hbufi, mode)
    use TOUZA_std,only: find_first, choice
    use TOUZA_Nio,only: parse_header_size, put_header_cprop
    implicit none
    integer,       intent(out)         :: ierr
    type(buffer_t),intent(inout)       :: buf
    integer,       intent(in)          :: hbufi(:) ! input buffer handles
    integer,       intent(in),optional :: mode     ! compromise mode.  mandatory for non-unary operators
    integer jinp, ninp

    character(len=litem) :: c1(lcoor), c2(lcoor)
    character(len=litem) :: newc(lcoor * 2)

    type(loop_t) :: nloop(lcoor)   ! loop property for output (inclusive)
    type(loop_t) :: defp
    integer kv
    integer jb
    integer md

    ierr = 0

    md = choice(def_cmode, mode)

    defp%bgn = 0
    defp%end = 1
    defp%stp = -1

    ninp = size(hbufi)

    ! input buffer arranger
    jinp = 1
    if (ierr.eq.0) then
       jb = buf_h2item(hbufi(jinp))
       newc(1:lcoor) = obuffer(jb)%cn(1:lcoor)
    endif
    do jinp = 2, ninp
       if (ierr.eq.0) c1(1:lcoor) = newc(1:lcoor)
       if (ierr.eq.0) then
          jb = buf_h2item(hbufi(jinp))
          c2(1:lcoor) = obuffer(jb)%cn(1:lcoor)
       endif
       if (ierr.eq.0) call tweak_coordinates(ierr, newc, c1, c2, lcoor)
    enddo
    do jinp = 1, ninp
       if (ierr.eq.0) call set_buffer_lprops(ierr, newc, hbufi(jinp))
    enddo
    nloop(:) = defp
    if (md.eq.cmode_first) then
       jinp = 1
       if (ierr.eq.0) call get_inclusive_lprops(ierr, nloop, hbufi(jinp), md)
    else
       do jinp = 1, ninp
          if (ierr.eq.0) call get_inclusive_lprops(ierr, nloop, hbufi(jinp), md)
       enddo
    endif
    ! set output buffer
    if (ierr.eq.0) then
       ! kv = kv_flt
       kv = kv_int
       do jinp = 1, ninp
          jb = buf_h2item(hbufi(jinp))
          kv = max(kv, obuffer(jb)%k)
       enddo
       buf%k = kv
    endif
    if (ierr.eq.0) then
       buf%cn(1:lcoor)  = newc(1:lcoor)
       buf%lpp(1:lcoor) = nloop(1:lcoor)
       ! jb = buf_h2item(bufh)
       ! obuffer(jb)%cn(1:lcoor)  = newc(1:lcoor)
       ! obuffer(jb)%lpp(1:lcoor) = nloop(1:lcoor)
    endif
    ! if (ierr.eq.0) call alloc_buffer(ierr, bufh, kv)
  end subroutine inclusive_buffer

!!!_   . set_buffer_lprops
  subroutine set_buffer_lprops(ierr, name, handle)
    use TOUZA_std,only: find_first
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: name(*)
    integer,         intent(in)  :: handle

    integer jc, kc
    integer jb
    character(len=litem) :: ctgt(lcoor)
    type(loop_t) :: iloop(lcoor)

    jb = buf_h2item(handle)
    ierr = min(0, jb)
    if (ierr.eq.0) then
       ctgt(1:lcoor)  = obuffer(jb)%cn(1:lcoor)
       iloop(1:lcoor) = obuffer(jb)%lpp(1:lcoor)
    endif
    if (ierr.eq.0) then
       do jc = 1, lcoor
          if (name(jc).eq.' ') then
             kc = -1
          else
             kc = find_first(ctgt(1:lcoor), name(jc), offset=1)
          endif
          if (kc.gt.0) then
             obuffer(jb)%lpp(jc) = iloop(kc)
          else
             obuffer(jb)%lpp(jc)%bgn = 0
             obuffer(jb)%lpp(jc)%end = 1
             obuffer(jb)%lpp(jc)%stp = -1
          endif
       enddo
    endif
    return
  end subroutine set_buffer_lprops

!!!_   . get_inclusive_lprops
  subroutine get_inclusive_lprops(ierr, lpp, handle, mode)
    use TOUZA_std,only: find_first
    implicit none
    integer,     intent(out)   :: ierr
    type(loop_t),intent(inout) :: lpp(*)
    integer,     intent(in)    :: handle
    integer,     intent(in)    :: mode

    integer jb
    integer jc
    type(loop_t),pointer :: bpp(:)

    jb = buf_h2item(handle)
    ierr = min(0, jb)
    if (ierr.eq.0) then
       bpp => obuffer(jb)%lpp(:)
       do jc = 1, lcoor
          if (bpp(jc)%stp.gt.0) then
             if (lpp(jc)%stp.le.0) then
                lpp(jc)%bgn = bpp(jc)%bgn
                lpp(jc)%end = bpp(jc)%end
                lpp(jc)%stp = 1
             else if (mode.eq.cmode_inclusive) then
                lpp(jc)%bgn = min(lpp(jc)%bgn, bpp(jc)%bgn)
                lpp(jc)%end = max(lpp(jc)%end, bpp(jc)%end)
             else
                lpp(jc)%bgn = max(lpp(jc)%bgn, bpp(jc)%bgn)
                lpp(jc)%end = min(lpp(jc)%end, bpp(jc)%end)
             endif
          else if (lpp(jc)%stp.eq.0) then
             if (lpp(jc)%bgn.eq.bpp(jc)%bgn &
                  & .and. lpp(jc)%end.eq.bpp(jc)%end) then
                continue
             else if (bpp(jc)%stp.lt.0) then
                continue
             else
                lpp(jc)%bgn = 0
                lpp(jc)%end = 0
                lpp(jc)%stp = 0
             endif
          else if (lpp(jc)%stp.lt.0) then
             lpp(jc) = bpp(jc)
          endif
       enddo
    endif

    return
  end subroutine get_inclusive_lprops

!!!_   . tweak_coordinates
  subroutine tweak_coordinates &
       & (ierr, cout, ca, cb, mc)
    implicit none
    integer,         intent(out) :: ierr
    character(len=*),intent(out) :: cout(*)        ! mc * 2 size
    character(len=*),intent(in)  :: ca(*),  cb(*)  ! mc size
    integer,         intent(in)  :: mc

    integer,parameter :: cnull = 0

    integer j, jj, ja, jb, jo, ma, nb, nt, lo
    integer tmpa(mc * 2), tmpb(mc * 2)
    character(len=litem) :: cbuf(mc*2)

    ierr = 0

    lo = mc * 2

    tmpa(:) = cnull
    tmpb(:) = cnull

    ma = mc
    do j = 1, mc
       tmpa(j) = j
    enddo
    jb = 1
    do j = 1, mc
       nb = jb + 1
       if (cb(j).eq.' ') then
          continue
       else
          do ja = 1, ma
             jj = tmpa(ja)
             if (jj.eq.cnull) cycle
             if (ca(jj).eq.' ') cycle
             if (ca(jj).eq.cb(j)) then
                if (ja.lt.jb) then
                   tmpa(jb:jb+(ma-ja)) = tmpa(ja:ma)
                   tmpa(ja:jb-1) = cnull
                   ma = ma + (jb - ja)
                   nb = jb + (jb - ja) + 1
                else
                   nt = ja - jb
                   tmpa(ja+1+nt:ma+nt) = tmpa(ja+1:ma)
                   tmpa(ja+1:ja+nt) = cnull
                   ma = ma + nt
                   jb = ja
                   nb = jb + 1
                endif
                exit
             endif
          enddo
       endif
       tmpb(jb) = j
       jb = nb
    enddo

    ! result packing on buffer
    cbuf(:) = ' '
    jo = 1
    do j = 1, lo
       ja = tmpa(j)
       jb = tmpb(j)
       if (ja.eq.cnull.and.jb.eq.cnull) cycle
       if (ja.eq.cnull) then
          if (cb(jb).eq.' ') cycle
          cbuf(jo) = cb(jb)
       else if (jb.eq.cnull) then
          if (ca(ja).eq.' ') cycle
          cbuf(jo) = ca(ja)
       else if (ca(ja).eq.' ') then
          cbuf(jo) = cb(jb)
       else if (cb(jb).eq.' ') then
          cbuf(jo) = ca(ja)
       else if (ca(ja).eq.cb(jb)) then
          cbuf(jo) = ca(ja)
       else
          cbuf(jo) = '*'
          ierr = ERR_PANIC
       endif
       jo = jo + 1
    enddo

    do
       if (jo.eq.1) exit
       jo = jo - 1
       if (cbuf(jo).ne.' ') exit
    enddo

    cout(1:lo) = ' '
    cout(1:lo) = cbuf(1:lo)
    if (jo.gt.mc) ierr = ERR_PANIC

  end subroutine tweak_coordinates

!!!_    * test_tweak_coordinates
#if TEST_CHAK
  subroutine test_tweak_coordinates_swap(ierr, ca, cb)
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: ca(*)
    character(len=*),intent(in)  :: cb(*)
    integer,parameter :: ltest = 3
    character(len=16) :: cout(ltest), cswp(ltest)
    integer jerr0, jerr1
    ierr = 0

    call tweak_coordinates(jerr0, cout, ca, cb, ltest)
    call tweak_coordinates(jerr1, cswp, cb, ca, ltest)
101 format('TWEAK:success ', L1, 1x, '[', 3(1x, A1), '|', 3(1x, A1), '] >> ', 3(1x, A1), '/', 3(1x, A1))
102 format('TWEAK:fail... ', L1, 1x, '[', 3(1x, A1), '|', 3(1x, A1), '] >> ', 3(1x, A1), '/', 3(1x, A1))
    if (jerr0.eq.0.and.jerr1.eq.0) then
       write(*, 101) ALL(cout(:).eq.cswp(:)), ca(1:ltest), cb(1:ltest), cout(:), cswp(:)
    else
       write(*, 102) ALL(cout(:).eq.cswp(:)), ca(1:ltest), cb(1:ltest), cout(:), cswp(:)
    endif
  end subroutine test_tweak_coordinates_swap

  subroutine test_tweak_coordinates_sub(ierr, ca)
    integer,         intent(out) :: ierr
    character(len=*),intent(in)  :: ca(*)
    integer,parameter :: ltest = 3

101 format('#### ', 3(1x, A))
    write(*, 101) ca(1:ltest)

    call test_tweak_coordinates_swap(ierr, ca, (/' ', ' ', ' '/))
    call test_tweak_coordinates_swap(ierr, ca, (/'X', 'Y', 'Z'/))

    call test_tweak_coordinates_swap(ierr, ca, (/'Z', ' ', ' '/))
    call test_tweak_coordinates_swap(ierr, ca, (/' ', 'Z', ' '/))
    call test_tweak_coordinates_swap(ierr, ca, (/' ', ' ', 'Z'/))

    call test_tweak_coordinates_swap(ierr, ca, (/'X', 'Z', ' '/))
    call test_tweak_coordinates_swap(ierr, ca, (/'X', ' ', 'Z'/))
    call test_tweak_coordinates_swap(ierr, ca, (/' ', 'X', 'Z'/))
    call test_tweak_coordinates_swap(ierr, ca, (/'Y', 'Z', ' '/))
    call test_tweak_coordinates_swap(ierr, ca, (/'Y', ' ', 'Z'/))
    call test_tweak_coordinates_swap(ierr, ca, (/' ', 'Y', 'Z'/))
    call test_tweak_coordinates_swap(ierr, ca, (/'X', 'Y', ' '/))
    call test_tweak_coordinates_swap(ierr, ca, (/'X', ' ', 'Y'/))
    call test_tweak_coordinates_swap(ierr, ca, (/' ', 'X', 'Y'/))

    call test_tweak_coordinates_swap(ierr, ca, (/'X', 'Y', 'W'/))
    call test_tweak_coordinates_swap(ierr, ca, (/'X', 'W', 'Y'/))
    call test_tweak_coordinates_swap(ierr, ca, (/'W', 'X', 'Y'/))
    call test_tweak_coordinates_swap(ierr, ca, (/'X', 'Z', 'W'/))
    call test_tweak_coordinates_swap(ierr, ca, (/'X', 'W', 'Z'/))
    call test_tweak_coordinates_swap(ierr, ca, (/'W', 'X', 'Z'/))
    call test_tweak_coordinates_swap(ierr, ca, (/'Y', 'Z', 'W'/))
    call test_tweak_coordinates_swap(ierr, ca, (/'Y', 'W', 'Z'/))
    call test_tweak_coordinates_swap(ierr, ca, (/'W', 'Y', 'Z'/))

  end subroutine test_tweak_coordinates_sub

  subroutine test_tweak_coordinates(ierr)
    integer,intent(out) :: ierr
    ierr = 0
    ! call test_tweak_coordinates_swap(ierr, (/'X', 'Y', ' '/), (/' ', 'X', 'Z'/))
    ! call test_tweak_coordinates_swap(ierr, (/'X', 'Y', 'Z'/), (/'X', 'Z', ' '/))
    ! return
    ! call test_tweak_coordinates_swap(ierr, (/'X', 'Y', ' '/), (/' ', 'Y', ' '/))

    call test_tweak_coordinates_sub(ierr, (/'X', 'Y', 'Z'/))
    ! return
    call test_tweak_coordinates_sub(ierr, (/'X', 'Y', ' '/))
    call test_tweak_coordinates_sub(ierr, (/'X', ' ', 'Y'/))
    call test_tweak_coordinates_sub(ierr, (/' ', 'X', 'Y'/))
    call test_tweak_coordinates_sub(ierr, (/'X', 'Z', ' '/))
    call test_tweak_coordinates_sub(ierr, (/'X', ' ', 'Z'/))
    call test_tweak_coordinates_sub(ierr, (/' ', 'X', 'Z'/))
    call test_tweak_coordinates_sub(ierr, (/'Y', 'Z', ' '/))
    call test_tweak_coordinates_sub(ierr, (/'Y', ' ', 'Z'/))
    call test_tweak_coordinates_sub(ierr, (/' ', 'Y', 'Z'/))

    call test_tweak_coordinates_sub(ierr, (/'X', ' ', ' '/))
    call test_tweak_coordinates_sub(ierr, (/' ', 'X', ' '/))
    call test_tweak_coordinates_sub(ierr, (/' ', ' ', 'X'/))
    call test_tweak_coordinates_sub(ierr, (/'Y', ' ', ' '/))
    call test_tweak_coordinates_sub(ierr, (/' ', 'Y', ' '/))
    call test_tweak_coordinates_sub(ierr, (/' ', ' ', 'Y'/))
    call test_tweak_coordinates_sub(ierr, (/'Y', ' ', ' '/))
    call test_tweak_coordinates_sub(ierr, (/' ', 'Y', ' '/))
    call test_tweak_coordinates_sub(ierr, (/' ', ' ', 'Y'/))

  end subroutine test_tweak_coordinates
#endif /* TEST_CHAK */

!!!_   . apply_elem_UNARY
  subroutine apply_elem_UNARY &
       & (ierr, hopr, bufo, bufi, func, bin)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: hopr
    integer,intent(in)          :: bufo(0:)
    integer,intent(in)          :: bufi(0:)
    logical,intent(in),optional :: bin
    interface
       real(kind=__KBUF) function func(A, NA)
         use TOUZA_std,only: KFLT, KDBL
         implicit none
         real(kind=__KBUF),intent(in) :: A
         real(kind=__KBUF),intent(in) :: NA
       end function func
    end interface

    integer jout
    integer jinp, ninp
    integer hbL, hbR
    integer jbL, jbR
    integer jbgnL(lcoor), iterL(lcoor), strdL(lcoor)
    integer jbgnR(lcoor), iterR(lcoor), strdR(lcoor)
    real(kind=KBUF) :: fillR
    logical check
    integer ofsi

    ierr = 0
    check = choice(.FALSE., bin)

    ninp = size(bufi)
    ofsi = 0
    if (is_anchor(bufi(ofsi))) then
       ninp = ninp - 1
       ofsi = ofsi + 1
    endif

    if (size(bufo).ne.ninp) ierr = ERR_INVALID_PARAMETER
    if (ierr.ne.0) then
       call message(ierr, 'invalid argument length for unary operation.')
       return
    endif

    do jout = 0, ninp - 1
       jinp = ofsi + jout
       if (ierr.eq.0) then
          hbL = bufo(jout)
          hbR = bufi(jinp)
          jbL = buf_h2item(hbL)
          jbR = buf_h2item(hbR)
          fillR = obuffer(jbR)%undef
          if (check.and. (fillR.eq.TRUE.or.fillR.eq.FALSE)) then
             ierr = ERR_PANIC
             call message(ierr, 'MISS value cannot be 1 nor 0')
          endif
       endif
       if (ierr.eq.0) call copy_set_header(ierr, bufo(jout), bufi(jinp), 1)
       if (ierr.eq.0) call inclusive_buffer_h(ierr, bufo(jout), bufi(jinp:jinp))
       if (ierr.eq.0) call get_buffer_strides_h(ierr, jbgnL, iterL, strdL, hbL)
       if (ierr.eq.0) call get_buffer_strides_h(ierr, jbgnR, iterR, strdR, hbR, jbgnL, iterL)
       if (ierr.eq.0) then
          call apply_buffer_UNARY &
               & (ierr, &
               &  obuffer(jbL)%vd, iterL, strdL,        &
               &  obuffer(jbR)%vd, iterR, strdR, jbgnR, fillR, func)
       endif
       if (ierr.eq.0) call set_unary_descr(ierr, hbL, hbR, hopr)
    enddo

  end subroutine apply_elem_UNARY

!!!_   . set_unary_descr
  subroutine set_unary_descr &
       & (ierr, lefth, righth, oprh)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: lefth
    integer,intent(in)  :: righth
    integer,intent(in)  :: oprh
    integer jbl,  jbr
    character(len=64) :: opr

    ierr = 0
    if (ierr.eq.0) call query_opr_name(ierr, opr, oprh)

    jbl = buf_h2item(lefth)
    jbr = buf_h2item(righth)
    obuffer(jbl)%desc = trim(obuffer(jbr)%desc) // ' ' // trim(opr)

  end subroutine set_unary_descr

!!!_   . apply_elem_BINARY_lazy
  subroutine apply_elem_BINARY_lazy &
       & (ierr, hopr, bufo, bufi, func, neutral)
    use TOUZA_std,only: choice
    implicit none
    integer,        intent(out)         :: ierr
    integer,        intent(in)          :: hopr
    integer,        intent(in)          :: bufo(0:)
    integer,        intent(in)          :: bufi(0:)
    real(kind=KBUF),intent(in),optional :: neutral
    interface
       real(kind=__KBUF) function func(A, B, NA, NB)
         use TOUZA_std,only: KFLT, KDBL
         implicit none
         real(kind=__KBUF),intent(in) :: A,  B
         real(kind=__KBUF),intent(in) :: NA, NB
       end function func
    end interface

    integer nopr
    integer jout, nout
    integer jinp, ninp, jj
    integer hbx, hba
    integer jbL, jbR
    integer jbgnL(lcoor), iterL(lcoor), strdL(lcoor)
    integer jbgnR(lcoor), strdR(lcoor)
    integer jbgnX(lcoor), iterX(lcoor)
    integer ofsi

    real(kind=KBUF) :: fillL, fillR

    ierr = 0

    ninp = size(bufi)
    ofsi = 0
    if (is_anchor(bufi(ofsi))) then
       ninp = ninp - 1
       ofsi = ofsi + 1
    endif
    nout = size(bufo)
    nopr = ninp / nout

    ! Reduce every (ninp / nout) buffer to nout.
    ! i.e., error if (ninp % nout) != 0 or (ninp / nout) < 2

    if (mod(ninp, nout).ne.0) ierr = ERR_INVALID_PARAMETER
    if (nopr.lt.2) ierr = ERR_INVALID_PARAMETER
    if (ierr.ne.0) then
       call message(ierr, 'invalid operands for binary operation.')
       return
    endif

    do jout = 0, nout - 1
       jinp = jout * nopr + ofsi
       if (ierr.eq.0) call copy_set_header(ierr, bufo(jout), bufi(jinp), nopr)
       if (ierr.eq.0) call inclusive_buffer_h(ierr, bufo(jout), bufi(jinp:jinp+nopr-1))

       if (ierr.eq.0) then
          hbx = bufo(jout)
          call get_buffer_strides_h(ierr, jbgnL, iterL, strdL, hbx)
          jbL = buf_h2item(hbx)
       endif

       jbgnR(:) = 0
       if (ierr.eq.0) then
          hba = bufi(jinp)
          call get_buffer_strides_h(ierr, jbgnX, iterX, strdR, hba, jbgnL, iterL)
          jbR = buf_h2item(hba)
          fillR = choice(obuffer(jbR)%undef, neutral)
          if (ierr.eq.0) then
             call copy_buffer_fill &
                  & (ierr, &
                  &  obuffer(jbL)%vd, iterL, strdL, jbgnX, &
                  &  obuffer(jbR)%vd, iterX, strdR, jbgnR, fillR)
          endif
       endif
       if (ierr.eq.0) then
          fillL = obuffer(jbR)%undef
          do jj = jinp + 1, jinp + nopr - 1
             hba = bufi(jj)
             jbR = buf_h2item(hba)
             fillR = obuffer(jbR)%undef
             call get_buffer_strides_h(ierr, jbgnX, iterX, strdR, hba, jbgnL, iterL)
             call apply_buffer_BINARY &
                  & (ierr, &
                  &  obuffer(jbL)%vd, iterX, strdL, jbgnX, fillL, &
                  &  obuffer(jbR)%vd,        strdR, jbgnR, fillR, func)
          enddo
       endif
       if (ierr.eq.0) then
          call set_binary_descr(ierr, bufo(jout), bufi(jinp:jinp+nopr-1), hopr)
       endif
    enddo
  end subroutine apply_elem_BINARY_lazy

!!!_   . apply_elem_BINARY
  subroutine apply_elem_BINARY &
       & (ierr, hopr, bufo, bufi, func, bin)
    use TOUZA_Std,only: choice
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(in)          :: hopr
    integer,intent(in)          :: bufo(0:)
    integer,intent(in)          :: bufi(0:)
    logical,intent(in),optional :: bin
    interface
       real(kind=__KBUF) function func(A, B, NA, NB)
         use TOUZA_std,only: KFLT, KDBL
         implicit none
         real(kind=__KBUF),intent(in) :: A,  B
         real(kind=__KBUF),intent(in) :: NA, NB
       end function func
    end interface

    integer nopr
    integer jout, nout
    integer jinp, ninp, jj
    integer hbx, hba
    integer jbL, jbR
    integer jbgnL(lcoor), iterL(lcoor), strdL(lcoor)
    integer jbgnR(lcoor), iterR(lcoor), strdR(lcoor)
    integer jbgnX(lcoor), iterX(lcoor)
    logical check
    integer ofsi

    real(kind=KBUF) :: fillL, fillR

    ierr = 0
    check = choice(.FALSE., bin)

    ninp = size(bufi)
    ofsi = 0
    if (is_anchor(bufi(ofsi))) then
       ninp = ninp - 1
       ofsi = ofsi + 1
    endif
    nout = size(bufo)
    nopr = ninp / nout

    ! Reduce every (ninp / nout) buffer to nout.
    ! i.e., error if (ninp % nout) != 0 or (ninp / nout) < 2

    if (mod(ninp, nout).ne.0) ierr = ERR_INVALID_PARAMETER
    if (nopr.lt.2) ierr = ERR_INVALID_PARAMETER
    if (ierr.ne.0) then
       call message(ierr, 'invalid operands for binary operation.')
       return
    endif

    do jout = 0, nout - 1
       jinp = jout * nopr + ofsi
       if (ierr.eq.0) call copy_set_header(ierr, bufo(jout), bufi(jinp), nopr)
       if (ierr.eq.0) call inclusive_buffer_h(ierr, bufo(jout), bufi(jinp:jinp+nopr-1))

       if (ierr.eq.0) then
          hbx = bufo(jout)
          call get_buffer_strides_h(ierr, jbgnL, iterL, strdL, hbx)
          jbL = buf_h2item(hbx)
          ! write(*, *) 'bgnL/0', jbgnL
          ! write(*, *) 'iterL/0', iterL
          ! write(*, *) 'strdL/0', strdL
          ! write(*, 101) jbgnL, iterL, strdL
       endif

       if (ierr.eq.0) call get_buffer_intersects(ierr, jbgnX, iterX, bufi(jinp:jinp+nopr-1), jbgnL)
       if (ierr.eq.0) then
          hba = bufi(jinp)
          call get_buffer_strides_h(ierr, jbgnR, iterR, strdR, hba, jbgnL, iterL)
          ! write(*, *) 'bgnX/0', jbgnX
          ! write(*, *) 'bgnR/0', jbgnR
          jbR = buf_h2item(hba)
          fillL = obuffer(jbR)%undef
          jbgnR(:) = jbgnX(:) - jbgnR(:)
          ! write(*, *) 'bgnR/1', jbgnR
          if (ierr.eq.0) then
             call copy_buffer_fill &
                  & (ierr, &
                  &  obuffer(jbL)%vd, iterL, strdL, jbgnX, &
                  &  obuffer(jbR)%vd, iterX, strdR, jbgnR, fillL)
             ! write(*, *) obuffer(jbL)%vd
             ! write(*, *) obuffer(jbR)%vd
          endif
       endif
       if (ierr.eq.0) then
          fillL = obuffer(jbR)%undef
          if (check.and. (fillL.eq.TRUE.or.fillL.eq.FALSE)) then
             ierr = ERR_PANIC
             call message(ierr, 'MISS value cannot be 1 nor 0')
             return
          endif
          do jj = jinp + 1, jinp + nopr - 1
             hba = bufi(jj)
             jbR = buf_h2item(hba)
             fillR = obuffer(jbR)%undef
             if (check.and. (fillR.eq.TRUE.or.fillR.eq.FALSE)) then
                ierr = ERR_PANIC
                call message(ierr, 'MISS value cannot be 1 nor 0')
                return
             endif
             call get_buffer_strides_h(ierr, jbgnR, iterR, strdR, hba, jbgnL, iterL)
             jbgnR(:) = jbgnX(:) - jbgnR(:)
             call apply_buffer_BINARY &
                  & (ierr, &
                  &  obuffer(jbL)%vd, iterX, strdL, jbgnX, fillL, &
                  &  obuffer(jbR)%vd,        strdR, jbgnR, fillR, func)
          enddo
       endif
       if (ierr.eq.0) then
          call set_binary_descr(ierr, bufo(jout), bufi(jinp:jinp+nopr-1), hopr)
       endif
    enddo
  end subroutine apply_elem_BINARY

!!!_   . set_binary_descr
  subroutine set_binary_descr &
       & (ierr, bufo, bufi, oprh)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(in)  :: bufo
    integer,intent(in)  :: bufi(:)
    integer,intent(in)  :: oprh
    integer jinp, ninp
    integer jbl,  jbr, hbr
    character(len=64) :: opr
    ierr = 0
    ninp = size(bufi)
    jbl = buf_h2item(bufo)

    jinp = 1
    hbr = bufi(jinp)
    jbr = buf_h2item(hbr)
    obuffer(jbl)%desc = trim(obuffer(jbr)%desc)
    do jinp = 2, ninp
       hbr = bufi(jinp)
       jbr = buf_h2item(hbr)
       obuffer(jbl)%desc = trim(obuffer(jbl)%desc) // ' ' // trim(obuffer(jbr)%desc)
    enddo
    if (ierr.eq.0) call query_opr_name(ierr, opr, oprh)
    if (ierr.eq.0) then
       obuffer(jbl)%desc = trim(obuffer(jbl)%desc) // ' ' // trim(opr)
       if (ninp.gt.2) then
          obuffer(jbl)%desc = str_MARK // ' ' // trim(obuffer(jbl)%desc) // ' ' // str_CUM
       endif
    endif
  end subroutine set_binary_descr

!!!_   . get_buffer_intersects
  subroutine get_buffer_intersects(ierr, bgn, iter, bufi, bref)
    implicit none
    integer,intent(out) :: ierr
    integer,intent(out) :: bgn(*)
    integer,intent(out) :: iter(*)
    integer,intent(in)  :: bufi(:)
    integer,intent(in)  :: bref(*)

    integer jinp, ninp
    type(loop_t) :: xloop(lcoor)
    integer jc

    ierr = 0
    xloop(:)%bgn = 0
    xloop(:)%end = 1
    xloop(:)%stp = -1

    ninp = size(bufi)
    do jinp = 1, ninp
       if (ierr.eq.0) call get_intersect_lprops(ierr, xloop, bufi(jinp))
    enddo
    ! write(*, *) 'xloop', xloop(:)%bgn
    ! write(*, *) 'xloop', xloop(:)%end
    ! write(*, *) 'xloop', xloop(:)%stp
    if (ierr.eq.0) then
       do jc = 1, lcoor
          iter(jc) = xloop(jc)%end - xloop(jc)%bgn
          if (xloop(jc)%stp.ge.0) then
             bgn(jc) = xloop(jc)%bgn - bref(jc)
          else
             bgn(jc) = bref(jc)
          endif
       enddo
    endif
    return
  end subroutine get_buffer_intersects

!!!_   . get_intersect_lprops
  subroutine get_intersect_lprops(ierr, lpp, handle)
    use TOUZA_std,only: find_first
    implicit none
    integer,     intent(out)   :: ierr
    type(loop_t),intent(inout) :: lpp(*)
    integer,     intent(in)    :: handle

    integer jb
    integer jc
    type(loop_t),pointer :: bpp(:)

    jb = buf_h2item(handle)
    ierr = min(0, jb)
    if (ierr.eq.0) then
       bpp => obuffer(jb)%lpp(:)
       do jc = 1, lcoor
          if (bpp(jc)%stp.gt.0) then
             if (lpp(jc)%stp.le.0) then
                lpp(jc)%bgn = bpp(jc)%bgn
                lpp(jc)%end = bpp(jc)%end
                lpp(jc)%stp = 1
             else
                lpp(jc)%bgn = max(lpp(jc)%bgn, bpp(jc)%bgn)
                lpp(jc)%end = min(lpp(jc)%end, bpp(jc)%end)
             endif
          endif
       enddo
    endif

    return
  end subroutine get_intersect_lprops

!!!_   . get_buffer_strides_h
  subroutine get_buffer_strides_h(ierr, bgn, iter, stride, handle, bref, iref)
    implicit none
    integer,intent(out)         :: ierr
    integer,intent(out)         :: bgn(*)
    integer,intent(out)         :: iter(*)
    integer,intent(out)         :: stride(*)
    integer,intent(in)          :: handle
    integer,intent(in),optional :: bref(*)
    integer,intent(in),optional :: iref(*)
    integer jb
    jb = buf_h2item(handle)
    ierr = min(0, jb)
    if (ierr.eq.0) then
       call get_buffer_strides(ierr, bgn, iter, stride, obuffer(jb), bref, iref)
    endif
    return
  end subroutine get_buffer_strides_h
!!!_   . get_buffer_strides
  subroutine get_buffer_strides(ierr, bgn, iter, stride, buf, bref, iref)
    implicit none
    integer,       intent(out)         :: ierr
    integer,       intent(out)         :: bgn(*)
    integer,       intent(out)         :: iter(*)
    integer,       intent(out)         :: stride(*)
    type(buffer_t),intent(in)          :: buf
    integer,       intent(in),optional :: bref(*)
    integer,       intent(in),optional :: iref(*)
    integer jc
    !! todo: coordinate permutation

    ierr = 0
    if (ierr.eq.0) then
       bgn(1:lcoor) = buf%lpp(1:lcoor)%bgn
       iter(1:lcoor) = max(1, (buf%lpp(1:lcoor)%end - bgn(1:lcoor)))
       stride(1) = 1
       do jc = 2, lcoor
          stride(jc) = stride(jc-1) * iter(jc-1)
       enddo
       stride(1:lcoor) = stride(1:lcoor) * max(0, buf%lpp(1:lcoor)%stp)
    endif
    if (ierr.eq.0) then
       if (present(iref).NEQV.present(iref)) then
          ierr = ERR_INVALID_PARAMETER
       else if (present(iref)) then
          do jc = 1, lcoor
             if (buf%lpp(jc)%stp.le.0) then
                bgn(jc)  = 0
                iter(jc) = iref(jc)
             else
                bgn(jc) = bgn(jc) - bref(jc)
             endif
          enddo
       endif
    endif
  end subroutine get_buffer_strides

!!!_   . copy_buffer_fill
  subroutine copy_buffer_fill &
       & (ierr, &
       &  vL,   iterL, strdL, bgnX,  &
       &  vR,   iterR, strdR, bgnR,  fill)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: vL(0:*)
    real(kind=KBUF),intent(in)  :: vR(0:*)
    integer,        intent(in)  :: iterL(*), strdL(*), bgnX(*)
    integer,        intent(in)  :: iterR(*), strdR(*), bgnR(*)
    real(kind=KBUF),intent(in)  :: fill

    integer jL1, jL2, jL3, jL0
    integer jR1, jR2, jR3, jR0
    integer jX1, jX2, jX3

    ierr = 0
    ! write(*, *) 'bgnX',  bgnX(1:3)
    ! write(*, *) 'iterL', iterL(1:3)
    ! write(*, *) 'strdL', strdL(1:3)
    ! write(*, *) 'bgnR',  bgnR(1:3)
    ! write(*, *) 'iterR', iterR(1:3)
    ! write(*, *) 'strdR', strdR(1:3)

    ! low[3]
    do       jL3 = 0, bgnX(3)  - 1
       do    jL2 = 0, iterL(2) - 1
          do jL1 = 0, iterL(1) - 1
             jL0 = jL1 * strdL(1) + jL2 * strdL(2) + jL3 * strdL(3)
             ! write(*, *) 'l3', jL1, jL2, jL3
             vL(jL0) = fill
          enddo
       enddo
    enddo
    ! internal[3]
    do jX3 = 0, iterR(3) - 1
       jR3 = bgnR(3) + jX3
       jL3 = bgnX(3) + jX3
       ! low[2]
       do    jL2 = 0, bgnX(2) - 1
          do jL1 = 0, iterL(1) - 1
             jL0 = jL1 * strdL(1) + jL2 * strdL(2) + jL3 * strdL(3)
             ! write(*, *) 'l2', jL1, jL2, jL3
             vL(jL0) = fill
          enddo
       enddo
       ! internal[2]
       do jX2 = 0, iterR(2) - 1
          jR2 = bgnR(2) + jX2
          jL2 = bgnX(2) + jX2
          ! low[1]
          do jL1 = 0, bgnX(1) - 1
             jL0 = jL1 * strdL(1) + jL2 * strdL(2) + jL3 * strdL(3)
             ! write(*, *) 'l1', jL1, jL2, jL3
             vL(jL0) = fill
          enddo
          ! internal[1]
          do jX1 = 0, iterR(1) - 1
             jR1 = bgnR(1) + jX1
             jL1 = bgnX(1) + jX1
             jL0 = jL1 * strdL(1) + jL2 * strdL(2) + jL3 * strdL(3)
             jR0 = jR1 * strdR(1) + jR2 * strdR(2) + jR3 * strdR(3)
             vL(jL0) = vR(jR0)
             ! write(*, *) 'm1', jL0, jL1, jL2, jL3, '/', jR0, jR1, jR2, jR3, vL(jL0)
          enddo
          ! high[1]
          do jL1 = bgnX(1) + iterR(1), iterL(1) - 1
             jL0 = jL1 * strdL(1) + jL2 * strdL(2) + jL3 * strdL(3)
             ! write(*, *) 'h1', jL1, jL2, jL3
             vL(jL0) = fill
          enddo
       enddo
       ! high[2]
       do    jL2 = bgnX(2) + iterR(2), iterL(2) - 1
          do jL1 = 0,                  iterL(1) - 1
             jL0 = jL1 * strdL(1) + jL2 * strdL(2) + jL3 * strdL(3)
             ! write(*, *) 'h2', jL1, jL2, jL3
             vL(jL0) = fill
          enddo
       enddo
    enddo
    ! high[3]
    do       jL3 = bgnX(3) + iterR(3), iterL(3) - 1
       do    jL2 = 0,                  iterL(2) - 1
          do jL1 = 0,                  iterL(1) - 1
             jL0 = jL1 * strdL(1) + jL2 * strdL(2) + jL3 * strdL(3)
             ! write(*, *) 'h3', jL1, jL2, jL3
             vL(jL0) = fill
          enddo
       enddo
    enddo

  end subroutine copy_buffer_fill

!!!_   . apply_buffer_UNARY
  subroutine apply_buffer_UNARY &
       & (ierr, &
       &  vL,   iterL, strdL, &
       &  vR,   iterR, strdR, bgnR,  fill, func)
    implicit none
    integer,        intent(out) :: ierr
    real(kind=KBUF),intent(out) :: vL(0:*)
    real(kind=KBUF),intent(in)  :: vR(0:*)
    integer,        intent(in)  :: iterL(*), strdL(*)
    integer,        intent(in)  :: iterR(*), strdR(*), bgnR(*)
    real(kind=KBUF),intent(in)  :: fill
    interface
       real(kind=__KBUF) function func(A, NA)
         use TOUZA_std,only: KFLT, KDBL
         implicit none
         real(kind=__KBUF),intent(in) :: A
         real(kind=__KBUF),intent(in) :: NA
       end function func
    end interface

    integer jL1, jL2, jL3, jL0
    integer jR1, jR2, jR3, jR0

    ierr = 0

    ! low[3]
    do       jL3 = 0, bgnR(3)  - 1
       do    jL2 = 0, iterL(2) - 1
          do jL1 = 0, iterL(1) - 1
             jL0 = jL1 * strdL(1) + jL2 * strdL(2) + jL3 * strdL(3)
             vL(jL0) = fill
          enddo
       enddo
    enddo
    ! internal[3]
    do jR3 = 0, iterR(3) - 1
       jL3 = bgnR(3) + jR3
       ! low[2]
       do    jL2 = 0, bgnR(2) - 1
          do jL1 = 0, iterL(1) - 1
             jL0 = jL1 * strdL(1) + jL2 * strdL(2) + jL3 * strdL(3)
             vL(jL0) = fill
          enddo
       enddo
       ! internal[2]
       do jR2 = 0, iterR(2) - 1
          jL2 = bgnR(2) + jR2
          ! low[1]
          do jL1 = 0, bgnR(1) - 1
             jL0 = jL1 * strdL(1) + jL2 * strdL(2) + jL3 * strdL(3)
             vL(jL0) = fill
          enddo
          ! internal[1]
          do jR1 = 0, iterR(1) - 1
             jL1 = bgnR(1) + jR1
             jL0 = jL1 * strdL(1) + jL2 * strdL(2) + jL3 * strdL(3)
             jR0 = jR1 * strdR(1) + jR2 * strdR(2) + jR3 * strdR(3)
             vL(jL0) = func(vR(jR0), fill)
          enddo
          ! high[1]
          do jL1 = bgnR(1) + iterR(1), iterL(1) - 1
             jL0 = jL1 * strdL(1) + jL2 * strdL(2) + jL3 * strdL(3)
             vL(jL0) = fill
          enddo
       enddo
       ! high[2]
       do    jL2 = bgnR(2) + iterR(2), iterL(2) - 1
          do jL1 = 0,                  iterL(1) - 1
             jL0 = jL1 * strdL(1) + jL2 * strdL(2) + jL3 * strdL(3)
             vL(jL0) = fill
          enddo
       enddo
    enddo
    ! high[3]
    do       jL3 = bgnR(3) + iterR(3), iterL(3) - 1
       do    jL2 = 0,                  iterL(2) - 1
          do jL1 = 0,                  iterL(1) - 1
             jL0 = jL1 * strdL(1) + jL2 * strdL(2) + jL3 * strdL(3)
             vL(jL0) = fill
          enddo
       enddo
    enddo

  end subroutine apply_buffer_UNARY

!!!_   . apply_buffer_BINARY
  subroutine apply_buffer_BINARY &
       & (ierr, &
       &  vL,   iterX, strdL, bgnX,  fillL, &
       &  vR,          strdR, bgnR,  fillR, func)
    implicit none
    integer,        intent(out)   :: ierr
    real(kind=KBUF),intent(inout) :: vL(0:*)
    real(kind=KBUF),intent(in)    :: vR(0:*)
    integer,        intent(in)    :: iterX(*), strdL(*), bgnX(*)
    integer,        intent(in)    :: strdR(*), bgnR(*)
    real(kind=KBUF),intent(in)    :: fillL, fillR
    interface
       real(kind=__KBUF) function func(A, B, NA, NB)
         use TOUZA_std,only: KFLT, KDBL
         implicit none
         real(kind=__KBUF),intent(in) :: A,  B
         real(kind=__KBUF),intent(in) :: NA, NB
       end function func
    end interface

    integer jL1, jL2, jL3, jL0
    integer jR1, jR2, jR3, jR0
    integer jX1, jX2, jX3

    ierr = 0

    ! internal[3]
    do jX3 = 0, iterX(3) - 1
       jR3 = bgnR(3) + jX3
       jL3 = bgnX(3) + jX3
       ! internal[2]
       do jX2 = 0, iterX(2) - 1
          jR2 = bgnR(2) + jX2
          jL2 = bgnX(2) + jX2
          ! internal[1]
          do jX1 = 0, iterX(1) - 1
             jR1 = bgnR(1) + jX1
             jL1 = bgnX(1) + jX1
             jL0 = jL1 * strdL(1) + jL2 * strdL(2) + jL3 * strdL(3)
             jR0 = jR1 * strdR(1) + jR2 * strdR(2) + jR3 * strdR(3)
             ! write(*, *) 'apply', jL0, jL1, jL2, jL3, '/', jR0, jR1, jR2, jR3
             ! write(*, *) 'm1', jL1, jL2, jL3, jR1, jR2, jR3
             vL(jL0) = func(vL(jL0), vR(jR0), fillL, fillR)
          enddo
       enddo
    enddo
  end subroutine apply_buffer_BINARY

!!!_  - unary operators
!!!_   . elem_ABS()
  real(kind=KBUF) function elem_ABS (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else
       X = ABS(A)
    endif
  end function elem_ABS

!!!_   . elem_NEG()
  real(kind=KBUF) function elem_NEG (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else
       X = - A
    endif
  end function elem_NEG

!!!_   . elem_ZSIGN() - return -1,0,+1 if negative, zero, positive
  real(kind=KBUF) function elem_ZSIGN (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else if (NA.eq.ZERO) then
       X = ZERO
    else
       X = SIGN(ONE, A)
    endif
  end function elem_ZSIGN

!!!_   . elem_SIGN() - SIGN(1,X)
  real(kind=KBUF) function elem_SIGN (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else
       X = SIGN(ONE, A)
    endif
  end function elem_SIGN

!!!_   . elem_INV()
  real(kind=KBUF) function elem_INV (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else if (A.eq.ZERO) then
       X = NA
    else
       X = ONE / A
    endif
  end function elem_INV

!!!_   . elem_SQR()
  real(kind=KBUF) function elem_SQR (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else
       X = A * A
    endif
  end function elem_SQR

!!!_   . elem_EXP()
  real(kind=KBUF) function elem_EXP (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else
       X = EXP(A)
    endif
  end function elem_EXP

!!!_   . elem_LOG()
  real(kind=KBUF) function elem_LOG (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else if (A.le.ZERO) then
       X = NA
    else
       X = LOG(A)
    endif
  end function elem_LOG

!!!_   . elem_LOG10()
  real(kind=KBUF) function elem_LOG10 (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else if (A.le.ZERO) then
       X = NA
    else
       X = LOG10(A)
    endif
  end function elem_LOG10

!!!_   . elem_SQRT()
  real(kind=KBUF) function elem_SQRT (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else if (A.lt.ZERO) then
       X = NA
    else
       X = SQRT(A)
    endif
  end function elem_SQRT

!!!_   . elem_SIN()
  real(kind=KBUF) function elem_SIN (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else
       X = SIN(A)
    endif
  end function elem_SIN

!!!_   . elem_COS()
  real(kind=KBUF) function elem_COS (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else
       X = COS(A)
    endif
  end function elem_COS

!!!_   . elem_TAN()
  real(kind=KBUF) function elem_TAN (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else
       X = TAN(A)
    endif
  end function elem_TAN

!!!_   . elem_ASIN()
  real(kind=KBUF) function elem_ASIN (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else
       X = ASIN(A)
    endif
  end function elem_ASIN

!!!_   . elem_ACOS()
  real(kind=KBUF) function elem_ACOS (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else
       X = ACOS(A)
    endif
  end function elem_ACOS

!!!_   . elem_TANH()
  real(kind=KBUF) function elem_TANH (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else
       X = TANH(A)
    endif
  end function elem_TANH

!!!_   . elem_EXPONENT()
  real(kind=KBUF) function elem_EXPONENT (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else
       X = real(EXPONENT(A), kind=KBUF)
    endif
  end function elem_EXPONENT

!!!_   . elem_FRACTION()
  real(kind=KBUF) function elem_FRACTION (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else
       X = FRACTION(A)
    endif
  end function elem_FRACTION

!!!_   . elem_NOT()
  real(kind=KBUF) function elem_NOT (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = TRUE
    else
       X = NA
    endif
  end function elem_NOT

!!!_   . elem_BOOL()
  real(kind=KBUF) function elem_BOOL (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else
       X = TRUE
    endif
  end function elem_BOOL

!!!_   . elem_BIN()
  real(kind=KBUF) function elem_BIN (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = TRUE
    else
       X = FALSE
    endif
  end function elem_BIN

!!!_   . elem_FLOOR()
  real(kind=KBUF) function elem_FLOOR (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else
       X = FLOOR(A)
    endif
  end function elem_FLOOR

!!!_   . elem_CEIL()
  real(kind=KBUF) function elem_CEIL (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else
       X = CEILING(A)
    endif
  end function elem_CEIL

!!!_   . elem_ROUND()
  real(kind=KBUF) function elem_ROUND (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else
       X = ANINT(A)
    endif
  end function elem_ROUND

!!!_   . elem_TRUNC()
  real(kind=KBUF) function elem_TRUNC (A, NA) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A
    real(kind=KBUF),intent(in) :: NA
    if (A.eq.NA) then
       X = NA
    else
       X = AINT(A)
    endif
  end function elem_TRUNC

!!!_  - binary operators
!!!_   & elem_ADD() - A + B
  real(kind=KBUF) function elem_ADD (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else
       X = A + B
    endif
  end function elem_ADD
!!!_   & elem_SUB() - A - B
  real(kind=KBUF) function elem_SUB (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else
       X = A - B
    endif
  end function elem_SUB
!!!_   & elem_MUL() - A * B
  real(kind=KBUF) function elem_MUL (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else
       X = A * B
    endif
  end function elem_MUL
!!!_   & elem_DIV() - A / B
  real(kind=KBUF) function elem_DIV (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else if (B.eq.ZERO) then
       X = NA
    else
       X = A / B
    endif
  end function elem_DIV

!!!_   & elem_IDIV() - integer division
  real(kind=KBUF) function elem_IDIV (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else if (B.eq.ZERO) then
       X = NA
    else
       X = REAL(INT(A) / INT(B), kind=KBUF)
    endif
  end function elem_IDIV

!!!_   & elem_MOD() - A % B
  real(kind=KBUF) function elem_MOD (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else if (B.eq.ZERO) then
       X = NA
    else
       X = MOD(A, B)
    endif
  end function elem_MOD

!!!_   & elem_POW() - A ** B
  real(kind=KBUF) function elem_POW (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else if (A.eq.ZERO.and.B.le.ZERO) then
       X = NA
    else if (A.lt.ZERO.and.ANINT(B).ne.B) then
       X = NA
    else
       X = A ** B
    endif
  end function elem_POW

!!!_   & elem_MIN() - MIN(A, B)
  real(kind=KBUF) function elem_MIN (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else
       X = MIN(A, B)
    endif
  end function elem_MIN

!!!_   & elem_MAX() - MAX(A, B)
  real(kind=KBUF) function elem_MAX (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else
       X = MAX(A, B)
    endif
  end function elem_MAX

!!!_   & elem_LMIN() - MIN(A, B) lazy  (A or B if the other == NAN)
  real(kind=KBUF) function elem_LMIN (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA) then
       if (B.eq.NB) then
          X = NA
       else
          X = B
       endif
    else if (B.eq.NB) then
       X = A
    else
       X = MIN(A, B)
    endif
  end function elem_LMIN

!!!_   & elem_LMAX() - MAX(A, B) lazy  (A or B if the other == NAN)
  real(kind=KBUF) function elem_LMAX (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA) then
       if (B.eq.NB) then
          X = NA
       else
          X = B
       endif
    else if (B.eq.NB) then
       X = A
    else
       X = MAX(A, B)
    endif
  end function elem_LMAX

!!!_   & elem_AND() - B if A != NAN else A
  ! (cf. gmtmath) B if A == NAN, else A
  !    operands  jmz  gmt
  !    A B       B    A
  !    A N       N    A
  !    N B       N    B
  !    N N       N    N
  real(kind=KBUF) function elem_AND (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA) then
       X = NA
    else if (B.eq.NB) then
       X = NA
    else
       X = B
    endif
  end function elem_AND

!!!_   & elem_OR() - B if A == NAN, else A  (same as gmt AND)
  ! (cf. gmtmath) NAN if B == NAN, else A
  !    operands  jmz  gmt
  !    A B       A    A
  !    A N       A    N
  !    N B       B    N
  !    N N       N    N
  real(kind=KBUF) function elem_OR (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA) then
       if (B.eq.NB) then
          X = NA
       else
          X = B
       endif
    else
       X = A
    endif
  end function elem_OR

!!!_   & elem_MASK() - NAN if B == NAN, else A  (same as gmt OR)
  real(kind=KBUF) function elem_MASK (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (B.eq.NB) then
       X = NA
    else
       X = A
    endif
  end function elem_MASK

!!!_   & elem_XOR() - B if A == NAN, A if B == NAN, else NAN
  ! (cf. gmtmath) B if A == NAN, else A (AND identical)
  !    operands  jmz  gmt
  !    A B       N    A
  !    A N       A    A
  !    N B       B    B
  !    N N       N    N
  real(kind=KBUF) function elem_XOR (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA) then
       if (B.eq.NB) then
          X = NA
       else
          X = B
       endif
    else if (B.eq.NB) then
       X = A
    else
       X = NA
    endif
  end function elem_XOR

!!!_   & elem_EQ() - binary for if A == B
  real(kind=KBUF) function elem_EQ (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.and.B.eq.NB) then
       X = TRUE
    else if (A.eq.B) then
       X = TRUE
    else
       X = FALSE
    endif
  end function elem_EQ

!!!_   & elem_NE() - binary for if A != B
  real(kind=KBUF) function elem_NE (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.and.B.eq.NB) then
       X = FALSE
    else if (A.eq.B) then
       X = FALSE
    else
       X = TRUE
    endif
  end function elem_NE

!!!_   & elem_LT() - binary for if A < B
  real(kind=KBUF) function elem_LT (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.and.B.eq.NB) then
       X = FALSE
    else if (A.lt.B) then
       X = TRUE
    else
       X = FALSE
    endif
  end function elem_LT

!!!_   & elem_LE() - binary for if A <= B
  real(kind=KBUF) function elem_LE (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.and.B.eq.NB) then
       X = TRUE
    else if (A.le.B) then
       X = TRUE
    else
       X = FALSE
    endif
  end function elem_LE

!!!_   & elem_GT() - binary for if A > B
  real(kind=KBUF) function elem_GT (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.and.B.eq.NB) then
       X = FALSE
    else if (A.gt.B) then
       X = TRUE
    else
       X = FALSE
    endif
  end function elem_GT

!!!_   & elem_GE() - binary for A if A >= B
  real(kind=KBUF) function elem_GE (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.and.B.eq.NB) then
       X = TRUE
    else if (A.ge.B) then
       X = TRUE
    else
       X = FALSE
    endif
  end function elem_GE

!!!_   & elem_EQF() - A if A == B, else NAN
  real(kind=KBUF) function elem_EQF (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else if (A.eq.B) then
       X = A
    else
       X = NA
    endif
  end function elem_EQF

!!!_   & elem_NEF() - A if A != B, else NAN
  real(kind=KBUF) function elem_NEF (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else if (A.ne.B) then
       X = A
    else
       X = NA
    endif
  end function elem_NEF

!!!_   & elem_LTF() - A if A < B, else NAN
  real(kind=KBUF) function elem_LTF (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else if (A.lt.B) then
       X = A
    else
       X = NA
    endif
  end function elem_LTF

!!!_   & elem_LEF() - A if A <= B, else NAN
  real(kind=KBUF) function elem_LEF (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else if (A.le.B) then
       X = A
    else
       X = NA
    endif
  end function elem_LEF

!!!_   & elem_GTF() - A if A > B, else NAN
  real(kind=KBUF) function elem_GTF (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else if (A.gt.B) then
       X = A
    else
       X = NA
    endif
  end function elem_GTF

!!!_   & elem_GEF() - A if A >= B, else NAN
  real(kind=KBUF) function elem_GEF (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else if (A.ge.B) then
       X = A
    else
       X = NA
    endif
  end function elem_GEF

!!!_   & elem_EQU() - binary or UNDEF for if A == B
  real(kind=KBUF) function elem_EQU (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else if (A.eq.B) then
       X = TRUE
    else
       X = FALSE
    endif
  end function elem_EQU

!!!_   & elem_NEU() - binary or UNDEF for if A != B
  real(kind=KBUF) function elem_NEU (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else if (A.ne.B) then
       X = TRUE
    else
       X = FALSE
    endif
  end function elem_NEU

!!!_   & elem_LTU() - binary/UNDEF for if A < B
  real(kind=KBUF) function elem_LTU (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else if (A.lt.B) then
       X = TRUE
    else
       X = FALSE
    endif
  end function elem_LTU

!!!_   & elem_LEU() - binary/UNDEF for if A <= B
  real(kind=KBUF) function elem_LEU (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else if (A.le.B) then
       X = TRUE
    else
       X = FALSE
    endif
  end function elem_LEU

!!!_   & elem_GTU() - binary/UNDEF for if A > B
  real(kind=KBUF) function elem_GTU (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else if (A.gt.B) then
       X = TRUE
    else
       X = FALSE
    endif
  end function elem_GTU

!!!_   & elem_GEU() - A if A >= B, else NAN
  real(kind=KBUF) function elem_GEU (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else if (A.ge.B) then
       X = A
    else
       X = NA
    endif
  end function elem_GEU

!!!_   & elem_ATAN2() - TAN2(A, B)
  real(kind=KBUF) function elem_ATAN2 (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else
       X = ATAN2(A, B)
    endif
  end function elem_ATAN2

!!!_   & elem_SCALE()
  real(kind=KBUF) function elem_SCALE (A, B, NA, NB) result(X)
    implicit none
    real(kind=KBUF),intent(in) :: A,  B
    real(kind=KBUF),intent(in) :: NA, NB
    if (A.eq.NA.or.B.eq.NB) then
       X = NA
    else
       X = SCALE(A, INT(B))
    endif
  end function elem_SCALE

!!!_   & is_undef()
!   elemental logical function is_undef(A, N) result(b)
!     implicit none
!     real(kind=KBUF),intent(in) :: A
!     real(kind=KBUF),intent(in) :: N
! #if HAVE_FORTRAN_IEEE_ARITHMETIC
!     b = ieee_is_nan(A)
! #else
!     b = A .eq. N
! #endif
!   end function is_undef

!!!_ + end chak
end program chak

!!!_! FOOTER
!!!_ + Local variables
! Local Variables:
! End:

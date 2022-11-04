!!!_! jmz/chak_decl.F90 - TOUZA/Jmz swiss(CH) army knife operator symbol declaration
! Maintainer: SAITO Fuyuki
! Created by genopr.sh at 2022-11-04T09:18:12+09:00
!!!_! MANIFESTO
!
! Copyright (C) 2022
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
  !! operation symbols
  !! group: system
  character(len=*),parameter :: str_INPUT = ' INPUT'
  character(len=*),parameter :: str_OUTPUT = ' OUTPUT'
  character(len=*),parameter :: str_ANCHOR = ' ANCHOR'
  character(len=*),parameter :: str_TRANSF = ' TRANSF'
  !! group: output
  character(len=*),parameter :: str_OUTPUT_POP = '='
  character(len=*),parameter :: str_OUTPUT_KEEP = ':='
  !! group: anchor
  character(len=*),parameter :: str_MARK = 'MARK'
  character(len=*),parameter :: str_STOP = 'STOP'
  character(len=*),parameter :: str_GO = 'GO'
  !! group: stack
  character(len=*),parameter :: str_DUP = 'DUP'
  character(len=*),parameter :: str_COPY = 'COPY'
  character(len=*),parameter :: str_CLONE = 'CLONE'
  character(len=*),parameter :: str_POP = 'POP'
  character(len=*),parameter :: str_EXCH = 'EXCH'
  character(len=*),parameter :: str_NOP = 'NOP'
  character(len=*),parameter :: str_DIST = 'DIST'
  character(len=*),parameter :: str_INSERT = 'INSERT'
  character(len=*),parameter :: str_REPEAT = 'REPEAT'
  character(len=*),parameter :: str_FLUSH = 'FLUSH'
  !! group: queue
  character(len=*),parameter :: str_ITER = 'ITER'
  character(len=*),parameter :: str_CUM = 'CUM'
  character(len=*),parameter :: str_MAP = 'MAP'
  !! group: unary
  character(len=*),parameter :: str_NEG = 'NEG'
  character(len=*),parameter :: str_INV = 'INV'
  character(len=*),parameter :: str_ABS = 'ABS'
  character(len=*),parameter :: str_SQR = 'SQR'
  character(len=*),parameter :: str_SIGN = 'SIGN'
  character(len=*),parameter :: str_ZSIGN = 'ZSIGN'
  character(len=*),parameter :: str_FLOOR = 'FLOOR'
  character(len=*),parameter :: str_CEIL = 'CEIL'
  character(len=*),parameter :: str_ROUND = 'ROUND'
  character(len=*),parameter :: str_TRUNC = 'TRUNC'
  character(len=*),parameter :: str_INT = 'INT'
  character(len=*),parameter :: str_BITNOT = 'BITNOT'
  !! group: ubool
  character(len=*),parameter :: str_NOT = 'NOT'
  character(len=*),parameter :: str_BOOL = 'BOOL'
  character(len=*),parameter :: str_BIN = 'BIN'
  !! group: bool
  character(len=*),parameter :: str_EQB = 'EQB'
  character(len=*),parameter :: str_NEB = 'NEB'
  character(len=*),parameter :: str_LTB = 'LTB'
  character(len=*),parameter :: str_GTB = 'GTB'
  character(len=*),parameter :: str_LEB = 'LEB'
  character(len=*),parameter :: str_GEB = 'GEB'
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
  !! group: binary
  character(len=*),parameter :: str_AND = 'AND'
  character(len=*),parameter :: str_MASK = 'MASK'
  character(len=*),parameter :: str_ADD = 'ADD'
  character(len=*),parameter :: str_SUB = 'SUB'
  character(len=*),parameter :: str_MUL = 'MUL'
  character(len=*),parameter :: str_DIV = 'DIV'
  character(len=*),parameter :: str_IDIV = 'IDIV'
  character(len=*),parameter :: str_MOD = 'MOD'
  character(len=*),parameter :: str_POW = 'POW'
  character(len=*),parameter :: str_BITAND = 'BITAND'
  character(len=*),parameter :: str_BITOR = 'BITOR'
  character(len=*),parameter :: str_BITXOR = 'BITXOR'
  character(len=*),parameter :: str_LSHIFT = 'LSHIFT'
  character(len=*),parameter :: str_RSHIFT = 'RSHIFT'
  character(len=*),parameter :: str_MIN = 'MIN'
  character(len=*),parameter :: str_MAX = 'MAX'
  character(len=*),parameter :: str_EQF = 'EQF'
  character(len=*),parameter :: str_NEF = 'NEF'
  character(len=*),parameter :: str_LTF = 'LTF'
  character(len=*),parameter :: str_GTF = 'GTF'
  character(len=*),parameter :: str_LEF = 'LEF'
  character(len=*),parameter :: str_GEF = 'GEF'
  !! group: lazy
  character(len=*),parameter :: str_OR = 'OR'
  character(len=*),parameter :: str_LOR = 'LOR'
  character(len=*),parameter :: str_ROR = 'ROR'
  character(len=*),parameter :: str_XOR = 'XOR'
  character(len=*),parameter :: str_LAND = 'LAND'
  character(len=*),parameter :: str_LMASK = 'LMASK'
  character(len=*),parameter :: str_LADD = 'LADD'
  character(len=*),parameter :: str_LSUB = 'LSUB'
  character(len=*),parameter :: str_LMUL = 'LMUL'
  character(len=*),parameter :: str_LDIV = 'LDIV'
  character(len=*),parameter :: str_LMIN = 'LMIN'
  character(len=*),parameter :: str_LMAX = 'LMAX'
  !! group: float
  character(len=*),parameter :: str_SQRT = 'SQRT'
  character(len=*),parameter :: str_EXP = 'EXP'
  character(len=*),parameter :: str_LOG = 'LOG'
  character(len=*),parameter :: str_LOG10 = 'LOG10'
  character(len=*),parameter :: str_SIN = 'SIN'
  character(len=*),parameter :: str_COS = 'COS'
  character(len=*),parameter :: str_TAN = 'TAN'
  character(len=*),parameter :: str_ASIN = 'ASIN'
  character(len=*),parameter :: str_ACOS = 'ACOS'
  character(len=*),parameter :: str_ATAN2 = 'ATAN2'
  character(len=*),parameter :: str_SINH = 'SINH'
  character(len=*),parameter :: str_COSH = 'COSH'
  character(len=*),parameter :: str_TANH = 'TANH'
  character(len=*),parameter :: str_R2D = 'R2D'
  character(len=*),parameter :: str_D2R = 'D2R'
  character(len=*),parameter :: str_HYPOT = 'HYPOT'
  character(len=*),parameter :: str_EXPONENT = 'EXPONENT'
  character(len=*),parameter :: str_FRACTION = 'FRACTION'
  character(len=*),parameter :: str_SCALE = 'SCALE'
  !! group: other
  !! group: reduction
  character(len=*),parameter :: str_NORM = 'NORM'
  character(len=*),parameter :: str_SUM = 'SUM'
  character(len=*),parameter :: str_AVR = 'AVR'
  character(len=*),parameter :: str_COUNT = 'COUNT'
  !! group: index
  !! group: header
  character(len=*),parameter :: str_FMT = 'FMT'
  character(len=*),parameter :: str_ITEM = 'ITEM'
  character(len=*),parameter :: str_UNIT = 'UNIT'
  character(len=*),parameter :: str_TITLE = 'TITLE'
  character(len=*),parameter :: str_EDIT = 'EDIT'
  character(len=*),parameter :: str_TSEL = 'T'
  character(len=*),parameter :: str_MISS = 'MISS'
  !! group: buffer
  character(len=*),parameter :: str_TAG = 'TAG'
  character(len=*),parameter :: str_PERM = 'PERM'
  character(len=*),parameter :: str_SHAPE = 'SHAPE'
  character(len=*),parameter :: str_C0 = 'C0'
  character(len=*),parameter :: str_C1 = 'C1'
  character(len=*),parameter :: str_C2 = 'C2'
  character(len=*),parameter :: str_C3 = 'C3'
  character(len=*),parameter :: str_X = 'X'
  character(len=*),parameter :: str_Y = 'Y'
  character(len=*),parameter :: str_Z = 'Z'
  !! operation id
  !! group: system
  integer,parameter :: grp_system_bgn = 0
  integer,parameter :: opr_INPUT = 0
  integer,parameter :: opr_OUTPUT = 1
  integer,parameter :: opr_ANCHOR = 2
  integer,parameter :: opr_TRANSF = 3
  integer,parameter :: grp_system_end = 4
  !! group: output
  integer,parameter :: grp_output_bgn = 4
  integer,parameter :: opr_OUTPUT_POP = 4
  integer,parameter :: opr_OUTPUT_KEEP = 5
  integer,parameter :: grp_output_end = 6
  !! group: anchor
  integer,parameter :: grp_anchor_bgn = 6
  integer,parameter :: opr_MARK = 6
  integer,parameter :: opr_STOP = 7
  integer,parameter :: opr_GO = 8
  integer,parameter :: grp_anchor_end = 9
  !! group: stack
  integer,parameter :: grp_stack_bgn = 9
  integer,parameter :: opr_DUP = 9
  integer,parameter :: opr_COPY = 10
  integer,parameter :: opr_CLONE = 11
  integer,parameter :: opr_POP = 12
  integer,parameter :: opr_EXCH = 13
  integer,parameter :: opr_NOP = 14
  integer,parameter :: opr_DIST = 15
  integer,parameter :: opr_INSERT = 16
  integer,parameter :: opr_REPEAT = 17
  integer,parameter :: opr_FLUSH = 18
  integer,parameter :: grp_stack_end = 19
  !! group: queue
  integer,parameter :: grp_queue_bgn = 19
  integer,parameter :: opr_ITER = 19
  integer,parameter :: opr_CUM = 20
  integer,parameter :: opr_MAP = 21
  integer,parameter :: grp_queue_end = 22
  !! group: unary
  integer,parameter :: grp_unary_bgn = 22
  integer,parameter :: opr_NEG = 22
  integer,parameter :: opr_INV = 23
  integer,parameter :: opr_ABS = 24
  integer,parameter :: opr_SQR = 25
  integer,parameter :: opr_SIGN = 26
  integer,parameter :: opr_ZSIGN = 27
  integer,parameter :: opr_FLOOR = 28
  integer,parameter :: opr_CEIL = 29
  integer,parameter :: opr_ROUND = 30
  integer,parameter :: opr_TRUNC = 31
  integer,parameter :: opr_INT = 32
  integer,parameter :: opr_BITNOT = 33
  integer,parameter :: grp_unary_end = 34
  !! group: ubool
  integer,parameter :: grp_ubool_bgn = 34
  integer,parameter :: opr_NOT = 34
  integer,parameter :: opr_BOOL = 35
  integer,parameter :: opr_BIN = 36
  integer,parameter :: grp_ubool_end = 37
  !! group: bool
  integer,parameter :: grp_bool_bgn = 37
  integer,parameter :: opr_EQB = 37
  integer,parameter :: opr_NEB = 38
  integer,parameter :: opr_LTB = 39
  integer,parameter :: opr_GTB = 40
  integer,parameter :: opr_LEB = 41
  integer,parameter :: opr_GEB = 42
  integer,parameter :: opr_EQ = 43
  integer,parameter :: opr_NE = 44
  integer,parameter :: opr_LT = 45
  integer,parameter :: opr_GT = 46
  integer,parameter :: opr_LE = 47
  integer,parameter :: opr_GE = 48
  integer,parameter :: opr_EQU = opr_EQ
  integer,parameter :: opr_NEU = opr_NE
  integer,parameter :: opr_LTU = opr_LT
  integer,parameter :: opr_GTU = opr_GT
  integer,parameter :: opr_LEU = opr_LE
  integer,parameter :: opr_GEU = opr_GE
  integer,parameter :: grp_bool_end = 49
  !! group: binary
  integer,parameter :: grp_binary_bgn = 49
  integer,parameter :: opr_AND = 49
  integer,parameter :: opr_MASK = 50
  integer,parameter :: opr_ADD = 51
  integer,parameter :: opr_SUB = 52
  integer,parameter :: opr_MUL = 53
  integer,parameter :: opr_DIV = 54
  integer,parameter :: opr_IDIV = 55
  integer,parameter :: opr_MOD = 56
  integer,parameter :: opr_POW = 57
  integer,parameter :: opr_BITAND = 58
  integer,parameter :: opr_BITOR = 59
  integer,parameter :: opr_BITXOR = 60
  integer,parameter :: opr_LSHIFT = 61
  integer,parameter :: opr_RSHIFT = 62
  integer,parameter :: opr_MIN = 63
  integer,parameter :: opr_MAX = 64
  integer,parameter :: opr_EQF = 65
  integer,parameter :: opr_NEF = 66
  integer,parameter :: opr_LTF = 67
  integer,parameter :: opr_GTF = 68
  integer,parameter :: opr_LEF = 69
  integer,parameter :: opr_GEF = 70
  integer,parameter :: grp_binary_end = 71
  !! group: lazy
  integer,parameter :: grp_lazy_bgn = 71
  integer,parameter :: opr_OR = 71
  integer,parameter :: opr_LOR = opr_OR
  integer,parameter :: opr_ROR = 72
  integer,parameter :: opr_XOR = 73
  integer,parameter :: opr_LAND = 74
  integer,parameter :: opr_LMASK = 75
  integer,parameter :: opr_LADD = 76
  integer,parameter :: opr_LSUB = 77
  integer,parameter :: opr_LMUL = 78
  integer,parameter :: opr_LDIV = 79
  integer,parameter :: opr_LMIN = 80
  integer,parameter :: opr_LMAX = 81
  integer,parameter :: grp_lazy_end = 82
  !! group: float
  integer,parameter :: grp_float_bgn = 82
  integer,parameter :: opr_SQRT = 82
  integer,parameter :: opr_EXP = 83
  integer,parameter :: opr_LOG = 84
  integer,parameter :: opr_LOG10 = 85
  integer,parameter :: opr_SIN = 86
  integer,parameter :: opr_COS = 87
  integer,parameter :: opr_TAN = 88
  integer,parameter :: opr_ASIN = 89
  integer,parameter :: opr_ACOS = 90
  integer,parameter :: opr_ATAN2 = 91
  integer,parameter :: opr_SINH = 92
  integer,parameter :: opr_COSH = 93
  integer,parameter :: opr_TANH = 94
  integer,parameter :: opr_R2D = 95
  integer,parameter :: opr_D2R = 96
  integer,parameter :: opr_HYPOT = 97
  integer,parameter :: opr_EXPONENT = 98
  integer,parameter :: opr_FRACTION = 99
  integer,parameter :: opr_SCALE = 100
  integer,parameter :: grp_float_end = 101
  !! group: other
  integer,parameter :: grp_other_bgn = 101
  integer,parameter :: grp_other_end = 101
  !! group: reduction
  integer,parameter :: grp_reduction_bgn = 101
  integer,parameter :: opr_NORM = 101
  integer,parameter :: opr_SUM = 102
  integer,parameter :: opr_AVR = 103
  integer,parameter :: opr_COUNT = 104
  integer,parameter :: grp_reduction_end = 105
  !! group: index
  integer,parameter :: grp_index_bgn = 105
  integer,parameter :: grp_index_end = 105
  !! group: header
  integer,parameter :: grp_header_bgn = 105
  integer,parameter :: opr_FMT = 105
  integer,parameter :: opr_ITEM = 106
  integer,parameter :: opr_UNIT = 107
  integer,parameter :: opr_TITLE = 108
  integer,parameter :: opr_EDIT = 109
  integer,parameter :: opr_TSEL = 110
  integer,parameter :: opr_MISS = 111
  integer,parameter :: grp_header_end = 112
  !! group: buffer
  integer,parameter :: grp_buffer_bgn = 112
  integer,parameter :: opr_TAG = 112
  integer,parameter :: opr_PERM = 113
  integer,parameter :: opr_SHAPE = opr_PERM
  integer,parameter :: opr_C0 = 114
  integer,parameter :: opr_C1 = 115
  integer,parameter :: opr_C2 = 116
  integer,parameter :: opr_C3 = 117
  integer,parameter :: opr_X = 118
  integer,parameter :: opr_Y = 119
  integer,parameter :: opr_Z = 120
  integer,parameter :: grp_buffer_end = 121

!!!_! jmz/chak_decl.F90 - TOUZA/Jmz CH(swiss) army knife operator symbol declaration
! Maintainer: SAITO Fuyuki
! Created by genopr.sh at 2023-07-08T16:59:48+09:00
!!!_! MANIFESTO
!
! Copyright (C) 2022,2023
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
  character(len=*),parameter :: str_PROP = 'PROP'
  character(len=*),parameter :: str_EXCH = 'EXCH'
  character(len=*),parameter :: str_NOP = 'NOP'
  character(len=*),parameter :: str_DIST = 'DIST'
  character(len=*),parameter :: str_ROLL = 'ROLL'
  character(len=*),parameter :: str_DEAL = 'DEAL'
  character(len=*),parameter :: str_INSERT = 'INSERT'
  character(len=*),parameter :: str_REPEAT = 'REPEAT'
  character(len=*),parameter :: str_FLUSH = 'FLUSH'
  character(len=*),parameter :: str_DFLUSH = 'DFLUSH'
  character(len=*),parameter :: str_CFLUSH = 'CFLUSH'
  character(len=*),parameter :: str_FLOAT = 'FLOAT'
  !! group: queue
  character(len=*),parameter :: str_ITER = 'ITER'
  character(len=*),parameter :: str_CUM = 'CUM'
  character(len=*),parameter :: str_MAP = 'MAP'
  !! group: unary
  character(len=*),parameter :: str_NEG = 'NEG'
  character(len=*),parameter :: str_INV = 'INV'
  character(len=*),parameter :: str_ABS = 'ABS'
  character(len=*),parameter :: str_SQR = 'SQR'
  character(len=*),parameter :: str_SIGN1 = 'SIGN1'
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
  character(len=*),parameter :: str_RDIV = 'RDIV'
  character(len=*),parameter :: str_IDIV = 'IDIV'
  character(len=*),parameter :: str_MOD = 'MOD'
  character(len=*),parameter :: str_POW = 'POW'
  character(len=*),parameter :: str_MODULO = 'MODULO'
  character(len=*),parameter :: str_SIGN = 'SIGN'
  character(len=*),parameter :: str_BITAND = 'BITAND'
  character(len=*),parameter :: str_BITOR = 'BITOR'
  character(len=*),parameter :: str_BITXOR = 'BITXOR'
  character(len=*),parameter :: str_LSHIFT = 'LSHIFT'
  character(len=*),parameter :: str_RSHIFT = 'RSHIFT'
  character(len=*),parameter :: str_MIN = 'MIN'
  character(len=*),parameter :: str_MAX = 'MAX'
  character(len=*),parameter :: str_EXTR = 'EXTR'
  character(len=*),parameter :: str_CDIFF = 'CDIFF'
  character(len=*),parameter :: str_FDIFF = 'FDIFF'
  character(len=*),parameter :: str_BDIFF = 'BDIFF'
  !! group: filter
  character(len=*),parameter :: str_EQF = 'EQF'
  character(len=*),parameter :: str_NEF = 'NEF'
  character(len=*),parameter :: str_LTF = 'LTF'
  character(len=*),parameter :: str_GTF = 'GTF'
  character(len=*),parameter :: str_LEF = 'LEF'
  character(len=*),parameter :: str_GEF = 'GEF'
  character(len=*),parameter :: str_ID = 'ID'
  !! group: lazy
  character(len=*),parameter :: str_OR = 'OR'
  character(len=*),parameter :: str_LOR = 'LOR'
  character(len=*),parameter :: str_ROR = 'ROR'
  character(len=*),parameter :: str_XOR = 'XOR'
  character(len=*),parameter :: str_LAND = 'LAND'
  character(len=*),parameter :: str_LMASK = 'LMASK'
  character(len=*),parameter :: str_LLAY = 'LLAY'
  character(len=*),parameter :: str_LAY = 'LAY'
  character(len=*),parameter :: str_RLAY = 'RLAY'
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
  character(len=*),parameter :: str_ATAN = 'ATAN'
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
  character(len=*),parameter :: str_NEAREST = 'NEAREST'
  character(len=*),parameter :: str_SPACING = 'SPACING'
  character(len=*),parameter :: str_RRSP = 'RRSP'
  !! group: ternary
  character(len=*),parameter :: str_IFELSE = 'IFELSE'
  character(len=*),parameter :: str_INRANGE = 'INRANGE'
  character(len=*),parameter :: str_BLEND = 'BLEND'
  !! group: reduce
  character(len=*),parameter :: str_NORM = 'NORM'
  character(len=*),parameter :: str_AVR = 'AVR'
  character(len=*),parameter :: str_SUM = 'SUM'
  character(len=*),parameter :: str_COUNT = 'COUNT'
  character(len=*),parameter :: str_WSUM = 'WSUM'
  character(len=*),parameter :: str_WMV = 'WMV'
  !! group: other
  !! group: index
  character(len=*),parameter :: str_C0 = 'C0'
  character(len=*),parameter :: str_C1 = 'C1'
  character(len=*),parameter :: str_C2 = 'C2'
  character(len=*),parameter :: str_C3 = 'C3'
  character(len=*),parameter :: str_X = 'X'
  character(len=*),parameter :: str_Y = 'Y'
  character(len=*),parameter :: str_Z = 'Z'
  character(len=*),parameter :: str_FLAT = 'FLAT'
  character(len=*),parameter :: str_INDEX = 'INDEX'
  !! group: shape
  character(len=*),parameter :: str_PERM = 'PERM'
  character(len=*),parameter :: str_SHAPE = 'SHAPE'
  character(len=*),parameter :: str_SIZE = 'SIZE'
  character(len=*),parameter :: str_SHIFT = 'SHIFT'
  character(len=*),parameter :: str_CSHIFT = 'CSHIFT'
  character(len=*),parameter :: str_EOSHIFT = 'EOSHIFT'
  !! group: header
  character(len=*),parameter :: str_FMT = 'FMT'
  character(len=*),parameter :: str_ITEM = 'ITEM'
  character(len=*),parameter :: str_UNIT = 'UNIT'
  character(len=*),parameter :: str_TITLE = 'TITLE'
  character(len=*),parameter :: str_EDIT = 'EDIT'
  character(len=*),parameter :: str_DSET = 'DSET'
  character(len=*),parameter :: str_TSEL = 'T'
  character(len=*),parameter :: str_RSEL = 'R'
  character(len=*),parameter :: str_MISS = 'MISS'
  character(len=*),parameter :: str_DUR = 'DUR'
  !! group: buffer
  character(len=*),parameter :: str_TAG = 'TAG'
  character(len=*),parameter :: str_DESC = 'DESC'
  character(len=*),parameter :: str_FUNC = 'FUNC'
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
  integer,parameter :: opr_PROP = 13
  integer,parameter :: opr_EXCH = 14
  integer,parameter :: opr_NOP = 15
  integer,parameter :: opr_DIST = 16
  integer,parameter :: opr_ROLL = 17
  integer,parameter :: opr_DEAL = 18
  integer,parameter :: opr_INSERT = 19
  integer,parameter :: opr_REPEAT = 20
  integer,parameter :: opr_FLUSH = 21
  integer,parameter :: opr_DFLUSH = 22
  integer,parameter :: opr_CFLUSH = 23
  integer,parameter :: opr_FLOAT = 24
  integer,parameter :: grp_stack_end = 25
  !! group: queue
  integer,parameter :: grp_queue_bgn = 25
  integer,parameter :: opr_ITER = 25
  integer,parameter :: opr_CUM = 26
  integer,parameter :: opr_MAP = 27
  integer,parameter :: grp_queue_end = 28
  !! group: unary
  integer,parameter :: grp_unary_bgn = 28
  integer,parameter :: opr_NEG = 28
  integer,parameter :: opr_INV = 29
  integer,parameter :: opr_ABS = 30
  integer,parameter :: opr_SQR = 31
  integer,parameter :: opr_SIGN1 = 32
  integer,parameter :: opr_ZSIGN = 33
  integer,parameter :: opr_FLOOR = 34
  integer,parameter :: opr_CEIL = 35
  integer,parameter :: opr_ROUND = 36
  integer,parameter :: opr_TRUNC = 37
  integer,parameter :: opr_INT = 38
  integer,parameter :: opr_BITNOT = 39
  integer,parameter :: grp_unary_end = 40
  !! group: ubool
  integer,parameter :: grp_ubool_bgn = 40
  integer,parameter :: opr_NOT = 40
  integer,parameter :: opr_BOOL = 41
  integer,parameter :: opr_BIN = 42
  integer,parameter :: grp_ubool_end = 43
  !! group: bool
  integer,parameter :: grp_bool_bgn = 43
  integer,parameter :: opr_EQB = 43
  integer,parameter :: opr_NEB = 44
  integer,parameter :: opr_LTB = 45
  integer,parameter :: opr_GTB = 46
  integer,parameter :: opr_LEB = 47
  integer,parameter :: opr_GEB = 48
  integer,parameter :: opr_EQ = 49
  integer,parameter :: opr_NE = 50
  integer,parameter :: opr_LT = 51
  integer,parameter :: opr_GT = 52
  integer,parameter :: opr_LE = 53
  integer,parameter :: opr_GE = 54
  integer,parameter :: opr_EQU = opr_EQ
  integer,parameter :: opr_NEU = opr_NE
  integer,parameter :: opr_LTU = opr_LT
  integer,parameter :: opr_GTU = opr_GT
  integer,parameter :: opr_LEU = opr_LE
  integer,parameter :: opr_GEU = opr_GE
  integer,parameter :: grp_bool_end = 55
  !! group: binary
  integer,parameter :: grp_binary_bgn = 55
  integer,parameter :: opr_AND = 55
  integer,parameter :: opr_MASK = 56
  integer,parameter :: opr_ADD = 57
  integer,parameter :: rdc_ADD = opr_ADD + 1
  integer,parameter :: opr_SUB = 59
  integer,parameter :: opr_MUL = 60
  integer,parameter :: rdc_MUL = opr_MUL + 1
  integer,parameter :: opr_DIV = 62
  integer,parameter :: opr_RDIV = 63
  integer,parameter :: opr_IDIV = 64
  integer,parameter :: opr_MOD = 65
  integer,parameter :: opr_POW = 66
  integer,parameter :: opr_MODULO = 67
  integer,parameter :: opr_SIGN = 68
  integer,parameter :: opr_BITAND = 69
  integer,parameter :: opr_BITOR = 70
  integer,parameter :: opr_BITXOR = 71
  integer,parameter :: opr_LSHIFT = 72
  integer,parameter :: opr_RSHIFT = 73
  integer,parameter :: opr_MIN = 74
  integer,parameter :: rdc_MIN = opr_MIN + 1
  integer,parameter :: opr_MAX = 76
  integer,parameter :: rdc_MAX = opr_MAX + 1
  integer,parameter :: opr_EXTR = 78
  integer,parameter :: opr_CDIFF = 79
  integer,parameter :: opr_FDIFF = 80
  integer,parameter :: opr_BDIFF = 81
  integer,parameter :: grp_binary_end = 82
  !! group: filter
  integer,parameter :: grp_filter_bgn = 82
  integer,parameter :: opr_EQF = 82
  integer,parameter :: opr_NEF = 83
  integer,parameter :: opr_LTF = 84
  integer,parameter :: opr_GTF = 85
  integer,parameter :: opr_LEF = 86
  integer,parameter :: opr_GEF = 87
  integer,parameter :: opr_ID = 88
  integer,parameter :: grp_filter_end = 89
  !! group: lazy
  integer,parameter :: grp_lazy_bgn = 89
  integer,parameter :: opr_OR = 89
  integer,parameter :: opr_LOR = opr_OR
  integer,parameter :: opr_ROR = 90
  integer,parameter :: opr_XOR = 91
  integer,parameter :: opr_LAND = 92
  integer,parameter :: opr_LMASK = 93
  integer,parameter :: opr_LLAY = 94
  integer,parameter :: opr_LAY = opr_LLAY
  integer,parameter :: opr_RLAY = 95
  integer,parameter :: opr_LADD = 96
  integer,parameter :: rdc_LADD = opr_LADD + 1
  integer,parameter :: opr_LSUB = 98
  integer,parameter :: opr_LMUL = 99
  integer,parameter :: rdc_LMUL = opr_LMUL + 1
  integer,parameter :: opr_LDIV = 101
  integer,parameter :: opr_LMIN = 102
  integer,parameter :: rdc_LMIN = opr_LMIN + 1
  integer,parameter :: opr_LMAX = 104
  integer,parameter :: rdc_LMAX = opr_LMAX + 1
  integer,parameter :: grp_lazy_end = 106
  !! group: float
  integer,parameter :: grp_float_bgn = 106
  integer,parameter :: opr_SQRT = 106
  integer,parameter :: opr_EXP = 107
  integer,parameter :: opr_LOG = 108
  integer,parameter :: opr_LOG10 = 109
  integer,parameter :: opr_SIN = 110
  integer,parameter :: opr_COS = 111
  integer,parameter :: opr_TAN = 112
  integer,parameter :: opr_ASIN = 113
  integer,parameter :: opr_ACOS = 114
  integer,parameter :: opr_ATAN = 115
  integer,parameter :: opr_ATAN2 = 116
  integer,parameter :: opr_SINH = 117
  integer,parameter :: opr_COSH = 118
  integer,parameter :: opr_TANH = 119
  integer,parameter :: opr_R2D = 120
  integer,parameter :: opr_D2R = 121
  integer,parameter :: opr_HYPOT = 122
  integer,parameter :: opr_EXPONENT = 123
  integer,parameter :: opr_FRACTION = 124
  integer,parameter :: opr_SCALE = 125
  integer,parameter :: opr_NEAREST = 126
  integer,parameter :: opr_SPACING = 127
  integer,parameter :: opr_RRSP = 128
  integer,parameter :: grp_float_end = 129
  !! group: ternary
  integer,parameter :: grp_ternary_bgn = 129
  integer,parameter :: opr_IFELSE = 129
  integer,parameter :: opr_INRANGE = 130
  integer,parameter :: opr_BLEND = 131
  integer,parameter :: grp_ternary_end = 132
  !! group: reduce
  integer,parameter :: grp_reduce_bgn = 132
  integer,parameter :: acc_NORM = 132
  integer,parameter :: opr_NORM = 133
  integer,parameter :: acc_AVR = 134
  integer,parameter :: opr_AVR = 135
  integer,parameter :: acc_SUM = 136
  integer,parameter :: opr_SUM = 137
  integer,parameter :: acc_COUNT = 138
  integer,parameter :: opr_COUNT = 139
  integer,parameter :: acc_WSUM = 140
  integer,parameter :: opr_WSUM = 141
  integer,parameter :: acc_WMV = 142
  integer,parameter :: opr_WMV = 143
  integer,parameter :: grp_reduce_end = 144
  !! group: other
  integer,parameter :: grp_other_bgn = 144
  integer,parameter :: grp_other_end = 144
  !! group: index
  integer,parameter :: grp_index_bgn = 144
  integer,parameter :: opr_C0 = 144
  integer,parameter :: opr_C1 = 145
  integer,parameter :: opr_C2 = 146
  integer,parameter :: opr_C3 = 147
  integer,parameter :: opr_X = 148
  integer,parameter :: opr_Y = 149
  integer,parameter :: opr_Z = 150
  integer,parameter :: opr_FLAT = 151
  integer,parameter :: opr_INDEX = opr_FLAT
  integer,parameter :: grp_index_end = 152
  !! group: shape
  integer,parameter :: grp_shape_bgn = 152
  integer,parameter :: opr_PERM = 152
  integer,parameter :: opr_SHAPE = 153
  integer,parameter :: opr_SIZE = 154
  integer,parameter :: opr_SHIFT = 155
  integer,parameter :: opr_CSHIFT = 156
  integer,parameter :: opr_EOSHIFT = 157
  integer,parameter :: grp_shape_end = 158
  !! group: header
  integer,parameter :: grp_header_bgn = 158
  integer,parameter :: opr_FMT = 158
  integer,parameter :: opr_ITEM = 159
  integer,parameter :: opr_UNIT = 160
  integer,parameter :: opr_TITLE = 161
  integer,parameter :: opr_EDIT = 162
  integer,parameter :: opr_DSET = 163
  integer,parameter :: opr_TSEL = 164
  integer,parameter :: opr_RSEL = 165
  integer,parameter :: opr_MISS = 166
  integer,parameter :: opr_DUR = 167
  integer,parameter :: grp_header_end = 168
  !! group: buffer
  integer,parameter :: grp_buffer_bgn = 168
  integer,parameter :: opr_TAG = 168
  integer,parameter :: opr_DESC = 169
  integer,parameter :: opr_FUNC = 170
  integer,parameter :: grp_buffer_end = 171

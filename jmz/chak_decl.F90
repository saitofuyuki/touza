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
  !! group: other
  !! group: reduction
  character(len=*),parameter :: str_AVR = 'AVR'
  character(len=*),parameter :: str_COUNT = 'COUNT'
  !! group: header
  character(len=*),parameter :: str_DFMT = 'DFMT'
  character(len=*),parameter :: str_ITEM = 'ITEM'
  character(len=*),parameter :: str_UNIT = 'UNIT'
  character(len=*),parameter :: str_TITLE = 'TITLE'
  character(len=*),parameter :: str_EDIT = 'EDIT'
  character(len=*),parameter :: str_TSEL = 'T'
  !! group: buffer
  character(len=*),parameter :: str_TAG = 'TAG'
  character(len=*),parameter :: str_C = 'C'
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
  character(len=*),parameter :: str_MISS = 'MISS'
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
  integer,parameter :: opr_POP = 11
  integer,parameter :: opr_EXCH = 12
  integer,parameter :: opr_NOP = 13
  integer,parameter :: opr_DIST = 14
  integer,parameter :: opr_INSERT = 15
  integer,parameter :: opr_REPEAT = 16
  integer,parameter :: opr_FLUSH = 17
  integer,parameter :: grp_stack_end = 18
  !! group: queue
  integer,parameter :: grp_queue_bgn = 18
  integer,parameter :: opr_ITER = 18
  integer,parameter :: opr_CUM = 19
  integer,parameter :: opr_MAP = 20
  integer,parameter :: grp_queue_end = 21
  !! group: unary
  integer,parameter :: grp_unary_bgn = 21
  integer,parameter :: opr_NEG = 21
  integer,parameter :: opr_INV = 22
  integer,parameter :: opr_ABS = 23
  integer,parameter :: opr_SQR = 24
  integer,parameter :: opr_SQRT = 25
  integer,parameter :: opr_SIGN = 26
  integer,parameter :: opr_ZSIGN = 27
  integer,parameter :: opr_FLOOR = 28
  integer,parameter :: opr_CEIL = 29
  integer,parameter :: opr_ROUND = 30
  integer,parameter :: opr_TRUNC = 31
  integer,parameter :: opr_INT = 32
  integer,parameter :: opr_EXP = 33
  integer,parameter :: opr_LOG = 34
  integer,parameter :: opr_LOG10 = 35
  integer,parameter :: opr_SIN = 36
  integer,parameter :: opr_COS = 37
  integer,parameter :: opr_TAN = 38
  integer,parameter :: opr_TANH = 39
  integer,parameter :: opr_ASIN = 40
  integer,parameter :: opr_ACOS = 41
  integer,parameter :: opr_EXPONENT = 42
  integer,parameter :: opr_FRACTION = 43
  integer,parameter :: grp_unary_end = 44
  !! group: ubool
  integer,parameter :: grp_ubool_bgn = 44
  integer,parameter :: opr_NOT = 44
  integer,parameter :: opr_BOOL = 45
  integer,parameter :: opr_BIN = 46
  integer,parameter :: grp_ubool_end = 47
  !! group: bool
  integer,parameter :: grp_bool_bgn = 47
  integer,parameter :: opr_EQB = 47
  integer,parameter :: opr_NEB = 48
  integer,parameter :: opr_LTB = 49
  integer,parameter :: opr_GTB = 50
  integer,parameter :: opr_LEB = 51
  integer,parameter :: opr_GEB = 52
  integer,parameter :: opr_EQ = 53
  integer,parameter :: opr_NE = 54
  integer,parameter :: opr_LT = 55
  integer,parameter :: opr_GT = 56
  integer,parameter :: opr_LE = 57
  integer,parameter :: opr_GE = 58
  integer,parameter :: opr_EQU = opr_EQ
  integer,parameter :: opr_NEU = opr_NE
  integer,parameter :: opr_LTU = opr_LT
  integer,parameter :: opr_GTU = opr_GT
  integer,parameter :: opr_LEU = opr_LE
  integer,parameter :: opr_GEU = opr_GE
  integer,parameter :: grp_bool_end = 59
  !! group: binary
  integer,parameter :: grp_binary_bgn = 59
  integer,parameter :: opr_AND = 59
  integer,parameter :: opr_MASK = 60
  integer,parameter :: opr_ADD = 61
  integer,parameter :: opr_SUB = 62
  integer,parameter :: opr_MUL = 63
  integer,parameter :: opr_DIV = 64
  integer,parameter :: opr_IDIV = 65
  integer,parameter :: opr_MOD = 66
  integer,parameter :: opr_POW = 67
  integer,parameter :: opr_ATAN2 = 68
  integer,parameter :: opr_SCALE = 69
  integer,parameter :: opr_MIN = 70
  integer,parameter :: opr_MAX = 71
  integer,parameter :: opr_EQF = 72
  integer,parameter :: opr_NEF = 73
  integer,parameter :: opr_LTF = 74
  integer,parameter :: opr_GTF = 75
  integer,parameter :: opr_LEF = 76
  integer,parameter :: opr_GEF = 77
  integer,parameter :: grp_binary_end = 78
  !! group: lazy
  integer,parameter :: grp_lazy_bgn = 78
  integer,parameter :: opr_OR = 78
  integer,parameter :: opr_LOR = opr_OR
  integer,parameter :: opr_ROR = 79
  integer,parameter :: opr_XOR = 80
  integer,parameter :: opr_LAND = 81
  integer,parameter :: opr_LMASK = 82
  integer,parameter :: opr_LADD = 83
  integer,parameter :: opr_LSUB = 84
  integer,parameter :: opr_LMUL = 85
  integer,parameter :: opr_LDIV = 86
  integer,parameter :: opr_LMIN = 87
  integer,parameter :: opr_LMAX = 88
  integer,parameter :: grp_lazy_end = 89
  !! group: other
  integer,parameter :: grp_other_bgn = 89
  integer,parameter :: grp_other_end = 89
  !! group: reduction
  integer,parameter :: grp_reduction_bgn = 89
  integer,parameter :: opr_AVR = 89
  integer,parameter :: opr_COUNT = 90
  integer,parameter :: grp_reduction_end = 91
  !! group: header
  integer,parameter :: grp_header_bgn = 91
  integer,parameter :: opr_DFMT = 91
  integer,parameter :: opr_ITEM = 92
  integer,parameter :: opr_UNIT = 93
  integer,parameter :: opr_TITLE = 94
  integer,parameter :: opr_EDIT = 95
  integer,parameter :: opr_TSEL = 96
  integer,parameter :: grp_header_end = 97
  !! group: buffer
  integer,parameter :: grp_buffer_bgn = 97
  integer,parameter :: opr_TAG = 97
  integer,parameter :: opr_C = 98
  integer,parameter :: opr_C0 = 99
  integer,parameter :: opr_C1 = 100
  integer,parameter :: opr_C2 = 101
  integer,parameter :: opr_C3 = 102
  integer,parameter :: opr_X = 103
  integer,parameter :: opr_Y = 104
  integer,parameter :: opr_Z = 105
  integer,parameter :: opr_LON = 106
  integer,parameter :: opr_LAT = 107
  integer,parameter :: opr_LEV = 108
  integer,parameter :: opr_MISS = 109
  integer,parameter :: grp_buffer_end = 110

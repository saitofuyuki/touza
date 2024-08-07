!!!_! jmz/chak_reg.F90 - TOUZA/Jmz CH(swiss) army knife operator registration
! Maintainer: SAITO Fuyuki
! Created by genopr.sh at 2023-11-19T18:39:15+09:00
!!!_! MANIFESTO
!
! Copyright (C) 2022,2023
!           Japan Agency for Marine-Earth Science and Technology
!
! Licensed under the Apache License, Version 2.0
!   (https://www.apache.org/licenses/LICENSE-2.0)
!
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_INPUT, str_INPUT, 1, 1)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_OUTPUT, str_OUTPUT, 1, 0)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_ANCHOR, str_ANCHOR)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_TRANSF, str_TRANSF, 1, 1)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_OUTPUT_POP, str_OUTPUT_POP, 1, 0)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_OUTPUT_KEEP, str_OUTPUT_KEEP, 1, 1)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_MARK, str_MARK)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_STOP, str_STOP)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_GO, str_GO)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_DUP, str_DUP, 1, 2)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_COPY, str_COPY, 1, 2)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_CLONE, str_CLONE, 1, 1)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_POP, str_POP, 1, 0)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_PROP, str_PROP, 1, 0)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_EXCH, str_EXCH, 2, 2)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_NOP, str_NOP, 0, 0)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_DIST, str_DIST)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_ROLL, str_ROLL)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_DEAL, str_DEAL)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_INSERT, str_INSERT)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_REPEAT, str_REPEAT)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_FLUSH, str_FLUSH)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_DFLUSH, str_DFLUSH)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_CFLUSH, str_CFLUSH)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_FLOAT, str_FLOAT, 1, 1, conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_ITER, str_ITER)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_CUM, str_CUM)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_MAP, str_MAP)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_NEG, str_NEG, 1, 1, ilev=ilev_neg, istr='-')
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_INV, str_INV, 1, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_ABS, str_ABS, 1, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_SQR, str_SQR, 1, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_SIGN1, str_SIGN1, 1, 1, ilev=ilev_call,  &
      & conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_ZSIGN, str_ZSIGN, 1, 1, ilev=ilev_call,  &
      & conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_FLOOR, str_FLOOR, 1, 1, ilev=ilev_call,  &
      & conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_CEIL, str_CEIL, 1, 1, ilev=ilev_call,  &
      & conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_ROUND, str_ROUND, 1, 1, ilev=ilev_call,  &
      & conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_TRUNC, str_TRUNC, 1, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_INT, str_INT, 1, 1, ilev=ilev_call, conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_BITNOT, str_BITNOT, 1, 1, ilev=ilev_neg, istr='~')
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_NOT, str_NOT, 1, 1, ilev=ilev_neg, istr='!',  &
      & conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_BOOL, str_BOOL, 1, 1, ilev=ilev_call,  &
      & conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_BIN, str_BIN, 1, 1, ilev=ilev_call, conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_EQB, str_EQB, 2, 1, ilev=ilev_call, conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_NEB, str_NEB, 2, 1, ilev=ilev_call, conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LTB, str_LTB, 2, 1, ilev=ilev_call, conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_GTB, str_GTB, 2, 1, ilev=ilev_call, conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LEB, str_LEB, 2, 1, ilev=ilev_call, conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_GEB, str_GEB, 2, 1, ilev=ilev_call, conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_EQ, str_EQ, 2, 1, ilev=ilev_call, conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_NE, str_NE, 2, 1, ilev=ilev_call, conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LT, str_LT, 2, 1, ilev=ilev_call, conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_GT, str_GT, 2, 1, ilev=ilev_call, conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LE, str_LE, 2, 1, ilev=ilev_call, conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_GE, str_GE, 2, 1, ilev=ilev_call, conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_EQU, str_EQU)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_NEU, str_NEU)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LTU, str_LTU)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_GTU, str_GTU)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LEU, str_LEU)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_GEU, str_GEU)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_AND, str_AND, 2, 1, ilev=ilev_logical, istr='&&')
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_MASK, str_MASK, 2, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_ADD, str_ADD, 2, 1, ilev=ilev_add, istr='+',  &
      & sweep=sweep_stack)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, rdc_ADD, rdc_pfx // str_ADD, 1, 1, sweep=sweep_reduce)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_SUB, str_SUB, 2, 1, ilev=ilev_add, istr='-')
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_MUL, str_MUL, 2, 1, ilev=ilev_mul, istr='*',  &
      & sweep=sweep_stack)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, rdc_MUL, rdc_pfx // str_MUL, 1, 1, sweep=sweep_reduce)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_DIV, str_DIV, 2, 1, ilev=ilev_mul, istr='/')
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_RDIV, str_RDIV, 2, 1, ilev=ilev_mul, istr='/',  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_IDIV, str_IDIV, 2, 1, ilev=ilev_mul, istr='//',  &
      & conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_MOD, str_MOD, 2, 1, ilev=ilev_mul, istr='%')
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_POW, str_POW, 2, 1, ilev=ilev_exp, istr='**')
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_MODULO, str_MODULO, 2, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_SIGN, str_SIGN, 2, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_BITAND, str_BITAND, 2, 1, ilev=ilev_and, istr='&')
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_BITOR, str_BITOR, 2, 1, ilev=ilev_or, istr='|')
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_BITXOR, str_BITXOR, 2, 1, ilev=ilev_xor, istr='^')
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LSHIFT, str_LSHIFT, 2, 1, ilev=ilev_shift,  &
      & istr='<<')
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_RSHIFT, str_RSHIFT, 2, 1, ilev=ilev_shift,  &
      & istr='>>')
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_MIN, str_MIN, 2, 1, ilev=ilev_call,  &
      & sweep=sweep_stack)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, rdc_MIN, rdc_pfx // str_MIN, 1, 1, sweep=sweep_reduce)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_MAX, str_MAX, 2, 1, ilev=ilev_call,  &
      & sweep=sweep_stack)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, rdc_MAX, rdc_pfx // str_MAX, 1, 1, sweep=sweep_reduce)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_EXTR, str_EXTR, 2, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_CDIFF, str_CDIFF, 2, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_FDIFF, str_FDIFF, 2, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_BDIFF, str_BDIFF, 2, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_EQF, str_EQF, 2, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_NEF, str_NEF, 2, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LTF, str_LTF, 2, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_GTF, str_GTF, 2, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LEF, str_LEF, 2, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_GEF, str_GEF, 2, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_ID, str_ID, 2, 1, ilev=ilev_call, conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_OR, str_OR, 2, 1, ilev=ilev_logical, istr='||')
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LOR, str_LOR)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_ROR, str_ROR, 2, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_XOR, str_XOR, 2, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LAND, str_LAND, 2, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LMASK, str_LMASK, 2, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LLAY, str_LLAY, 2, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LAY, str_LAY)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_RLAY, str_RLAY, 2, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LADD, str_LADD, 2, 1, ilev=ilev_add, istr='+',  &
      & sweep=sweep_stack)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, rdc_LADD, rdc_pfx // str_LADD, 1, 1, sweep=sweep_reduce)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LSUB, str_LSUB, 2, 1, ilev=ilev_add, istr='-')
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LMUL, str_LMUL, 2, 1, ilev=ilev_mul, istr='*',  &
      & sweep=sweep_stack)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, rdc_LMUL, rdc_pfx // str_LMUL, 1, 1, sweep=sweep_reduce)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LDIV, str_LDIV, 2, 1, ilev=ilev_mul, istr='/')
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LMIN, str_LMIN, 2, 1, ilev=ilev_call,  &
      & sweep=sweep_stack)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, rdc_LMIN, rdc_pfx // str_LMIN, 1, 1, sweep=sweep_reduce)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LMAX, str_LMAX, 2, 1, ilev=ilev_call,  &
      & sweep=sweep_stack)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, rdc_LMAX, rdc_pfx // str_LMAX, 1, 1, sweep=sweep_reduce)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_SQRT, str_SQRT, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_EXP, str_EXP, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LOG, str_LOG, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_LOG10, str_LOG10, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_SIN, str_SIN, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_COS, str_COS, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_TAN, str_TAN, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_ASIN, str_ASIN, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_ACOS, str_ACOS, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_ATAN, str_ATAN, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_ATAN2, str_ATAN2, 2, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_SINH, str_SINH, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_COSH, str_COSH, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_TANH, str_TANH, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_ASINH, str_ASINH, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_ACOSH, str_ACOSH, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_ATANH, str_ATANH, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_R2D, str_R2D, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_D2R, str_D2R, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_HYPOT, str_HYPOT, 2, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_EXPONENT, str_EXPONENT, 1, 1, ilev=ilev_call,  &
      & conv=result_int)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_FRACTION, str_FRACTION, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_SCALE, str_SCALE, 2, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_NEAREST, str_NEAREST, 2, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_SPACING, str_SPACING, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_RRSP, str_RRSP, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_SETE, str_SETE, 2, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_FTRUNC, str_FTRUNC, 1, 1, ilev=ilev_call,  &
      & conv=result_float)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_IFELSE, str_IFELSE, 3, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_INRANGE, str_INRANGE, 3, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_BLEND, str_BLEND, 3, 1, ilev=ilev_call)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_NORM, str_NORM, ilev=ilev_call, sweep=sweep_reduce)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, acc_NORM, acc_pfx // str_NORM, sweep=sweep_accum)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_AVR, str_AVR, ilev=ilev_call, sweep=sweep_reduce)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, acc_AVR, acc_pfx // str_AVR, sweep=sweep_accum)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_SUM, str_SUM, 1, 1, ilev=ilev_call,  &
      & sweep=sweep_reduce)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, acc_SUM, acc_pfx // str_SUM, 1, 1, sweep=sweep_accum)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_COUNT, str_COUNT, 1, 1, ilev=ilev_call,  &
      & conv=result_int, sweep=sweep_reduce)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, acc_COUNT, acc_pfx // str_COUNT, 1, 1, conv=result_int,  &
      & sweep=sweep_accum)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_WSUM, str_WSUM, 2, 2, ilev=ilev_call,  &
      & sweep=sweep_reduce)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, acc_WSUM, acc_pfx // str_WSUM, 2, 2, sweep=sweep_accum)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_WMV, str_WMV, 2, 3, ilev=ilev_call,  &
      & sweep=sweep_reduce)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, acc_WMV, acc_pfx // str_WMV, 2, 3, sweep=sweep_accum)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_C0, str_C0)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_C1, str_C1)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_C2, str_C2)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_C3, str_C3)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_X, str_X)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_Y, str_Y)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_Z, str_Z)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_FLAT, str_FLAT)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_INDEX, str_INDEX)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_PERM, str_PERM, 1, 1)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_SHAPE, str_SHAPE, 1, 1)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_SIZE, str_SIZE, 1, 1)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_SHIFT, str_SHIFT, 1, 1)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_CSHIFT, str_CSHIFT, 1, 1)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_EOSHIFT, str_EOSHIFT, 1, 1)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_FMT, str_FMT)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_ITEM, str_ITEM)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_UNIT, str_UNIT)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_TITLE, str_TITLE)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_ETITLE, str_ETITLE)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_EDIT, str_EDIT)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_DSET, str_DSET)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_TSEL, str_TSEL)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_RSEL, str_RSEL)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_MISS, str_MISS)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_DUR, str_DUR)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_TAG, str_TAG)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_DESC, str_DESC)
    if (ierr.eq.0) &
      & call reg_opr_prop(ierr, opr_FUNC, str_FUNC)

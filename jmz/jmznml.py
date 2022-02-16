#!/usr/bin/env python3
# Created: Apr 5 2017 (called ginnan originally)
# Created: Feb 1 2022
# Maintainer: SAITO Fuyuki
# Time-stamp: <2022/02/01 12:05:10 fuyuki jmznml.py>
#
#_ MANIFESTO
#
# Copyright (C) 2017-2022
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

import string
import pyparsing as pps
import pprint    as ppr
import sys

class Prop():
    pass

class FortranSets():
    """Fortran character sets."""
    def __init__(self):
        self.alpha = string.ascii_letters
        self.numeral = string.digits
        self.ws = r' '
        self.symbols = Prop()
        self.symbols.eq = r'='
        self.symbols.plus = r'+'
        self.symbols.minus = r'-'
        self.symbols.star = r'*'
        self.symbols.slash = r'/'
        self.symbols.lpar = r'('
        self.symbols.rpar = r')'
        self.symbols.period = r'.'
        self.symbols.comma = r','
        self.symbols.dollar = r'$'
        self.symbols.sq = r"'"
        self.symbols.colon = r':'

        self.symbols.amp = r'&'
        self.symbols.us = r'_'
        self.symbols.xm = r'!'
        self.symbols.sharp = r'#'
        self.symbols.dq = r'"'

class FortranNamelist():
    """Fortran namelist input parser"""
    def __init__(self, cs, err=None):
        self.errf = err or sys.stderr

        sign = pps.Literal(cs.symbols.plus) | pps.Literal(cs.symbols.minus)
        nseq = pps.Word(cs.numeral)
        expe = pps.CaselessLiteral('e') | pps.CaselessLiteral('d')

        decimal = cs.symbols.period
        # mantissa
        m0 = nseq + decimal + pps.Optional(nseq)
        m1 = decimal + nseq
        m2 = nseq
        mantissa = m0 | m1 | m2
        # exponent
        exponent = expe + pps.Optional(sign) + nseq

        # logical
        log = pps.CaselessKeyword('T') | pps.CaselessKeyword('F')
        log = log + pps.FollowedBy(pps.ZeroOrMore(pps.White()) + pps.NotAny(cs.symbols.eq))

        # character
        sq = cs.symbols.sq
        sst = pps.QuotedString(quoteChar=sq, escQuote=sq + sq, unquoteResults=False)
        dq = cs.symbols.dq
        dst = pps.QuotedString(quoteChar=dq, escQuote=dq + dq, unquoteResults=False)

        bsep = cs.symbols.comma \
          + cs.symbols.eq + cs.symbols.slash + cs.symbols.amp + cs.symbols.dollar
        bare_str = pps.CharsNotIn(bsep)

        v_int = pps.Combine(pps.Optional(sign) + nseq)
        v_real = pps.Combine(pps.Optional(sign) + mantissa + pps.Optional(exponent))
        v_log = pps.Combine(log)
        v_char = pps.Combine(sst | dst)

        # values
        val1 = v_real | v_int | v_log | v_char
        val =pps.Combine(v_int + cs.symbols.star + val1) | val1

        sep = pps.Suppress(cs.symbols.comma)

        # variable
        aseq = pps.Word(cs.alpha + cs.symbols.us,
                           cs.alpha + cs.numeral + cs.symbols.us)
        variable = pps.Combine(aseq + pps.Optional (cs.symbols.lpar + v_int + cs.symbols.rpar))

        # assignments
        eq = pps.Suppress(cs.symbols.eq)
        left = variable + eq

        values = val + pps.ZeroOrMore(sep + val)
        assign = pps.Group(variable + eq + pps.Group(values|bare_str|pps.empty()))

        # namelist entry
        head = pps.Literal(cs.symbols.dollar) | pps.Literal(cs.symbols.amp)
        nstart = pps.Group(pps.Suppress(head) + aseq)

        t1 = head + pps.CaselessLiteral('end')
        t2 = pps.Literal(cs.symbols.slash)

        nend = pps.Combine(t1 | t2)

        namelist = nstart + pps.ZeroOrMore(sep | assign) + nend.suppress()

        # non-standard comment
        comment = pps.LineStart() + cs.symbols.sharp + pps.SkipTo('\n')

        # error
        err = nstart.copy()
        err.setParseAction(self.report)
        # warning
        bare_str.setParseAction(self.warning)

        self.parser = (namelist | err.suppress())
        self.parser.ignore(comment)
        pass

    def report(self, st, locn, toks):
        self.errf.write("Cannot parse line %d.\n" % (pps.lineno(locn, st)))
        self.errf.write(">> %s\n" % (pps.line(locn, st)))

    def warning(self, st, locn, toks):
        pass

def main(argv):
    cs = FortranSets()
    fn = FortranNamelist(cs)
    for f in argv:
        fin = open(f)
        src = ''.join(list(fin))
        # fn.parser.setDebug(True)
        for t in fn.parser.searchString(src):
            ppr.pprint(t.asList())

    pass

if __name__ == '__main__':
    main(sys.argv[1:])
    pass

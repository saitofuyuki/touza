#!/usr/bin/env python3
# Created: Apr 5 2017 (called ginnan originally)
# Created: Feb 1 2022
# Maintainer: SAITO Fuyuki
# Time-stamp: <2022/02/25 16:26:34 fuyuki jmznml.py>
#
# MANIFESTO
#
# Copyright (C) 2017-2022
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

import sys
import string
import pprint as ppr
import pyparsing as pps


class Prop():
    """Placeholder."""


class FortranSets():
    """Fortran character sets."""

    def __init__(self):
        """Set default Fortran characters."""
        self.alpha = string.ascii_letters
        self.numeral = string.digits
        self.wspace = r' '
        self.symbols = Prop()
        self.symbols.equal = r'='
        self.symbols.plus = r'+'
        self.symbols.minus = r'-'
        self.symbols.star = r'*'
        self.symbols.slash = r'/'
        self.symbols.lpar = r'('
        self.symbols.rpar = r')'
        self.symbols.lbra = r'['
        self.symbols.rbra = r']'
        self.symbols.period = r'.'
        self.symbols.comma = r','
        self.symbols.dollar = r'$'
        self.symbols.squote = r"'"
        self.symbols.colon = r':'

        self.symbols.amp = r'&'
        self.symbols.uscore = r'_'
        self.symbols.xclam = r'!'
        self.symbols.sharp = r'#'
        self.symbols.dquote = r'"'


class FortranNamelist():
    """Fortran namelist input parser"""

    def __init__(self, cset, err=None):
        """Initialize namelist parser using CSET character set."""
        self.errf = err or sys.stderr

        sign = pps.Literal(cset.symbols.plus) | pps.Literal(cset.symbols.minus)
        nseq = pps.Word(cset.numeral)
        expe = pps.CaselessLiteral('e') | pps.CaselessLiteral('d')

        decimal = cset.symbols.period
        # mantissa
        mtsa0 = nseq + decimal + pps.Optional(nseq)
        mtsa1 = decimal + nseq
        mtsa2 = nseq
        mantissa = mtsa0 | mtsa1 | mtsa2
        # exponent
        exponent = expe + pps.Optional(sign) + nseq

        # logical
        log = pps.CaselessKeyword('T') | pps.CaselessKeyword('F')
        log = log + pps.FollowedBy(pps.ZeroOrMore(pps.White())
                                   + pps.NotAny(cset.symbols.equal))

        # character
        squote = cset.symbols.squote
        sst = pps.QuotedString(quoteChar=squote,
                               escQuote=(squote + squote),
                               unquoteResults=False)
        dquote = cset.symbols.dquote
        dst = pps.QuotedString(quoteChar=dquote,
                               escQuote=(dquote + dquote),
                               unquoteResults=False)

        def nmlz_string(toks):
            """normalize bare string."""
            r = toks[0].strip()
            if r == '':
                r = ' '
            return(r)

        bsep = (cset.symbols.comma
                + cset.symbols.equal + cset.symbols.slash
                + cset.symbols.amp + cset.symbols.dollar)
        bare_str = pps.CharsNotIn(bsep)
        bare_str.setParseAction(nmlz_string)
        # bare_str.set_debug()
        bare_str.set_name('bare_str')

        v_int = pps.Combine(pps.Optional(sign) + nseq)
        v_real = pps.Combine(pps.Optional(sign) + mantissa
                             + pps.Optional(exponent))
        v_log = pps.Combine(log)
        v_char = pps.Combine(sst | dst)

        # values
        val1 = v_real | v_int | v_log | v_char
        vitr = v_int.set_parse_action(lambda toks: int(toks[0]))
        # vrep = pps.Group(pps.Combine(vitr + cset.symbols.star + val1))
        vrep = pps.Group(vitr + pps.Suppress(cset.symbols.star) + val1)
        vrep.set_parse_action(lambda toks: tuple(toks[0]))
        val = (vrep | val1)

        sep = pps.Suppress(cset.symbols.comma)

        # variable
        aseq = pps.Word(cset.alpha + cset.symbols.uscore,
                        cset.alpha + cset.numeral + cset.symbols.uscore)
        variable = pps.Combine(aseq
                               + pps.Optional(cset.symbols.lpar
                                              + v_int + cset.symbols.rpar))

        # assignments
        equal = pps.Suppress(cset.symbols.equal)
        left = variable + equal

        values = val + pps.ZeroOrMore(sep + val)
        # values.set_debug()
        assign = pps.Group(variable + equal
                           + pps.Group(values | bare_str | pps.empty()))

        # namelist entry
        head = pps.Literal(cset.symbols.dollar) | pps.Literal(cset.symbols.amp)
        nstart = pps.Group(pps.Suppress(head) + aseq)

        term1 = head + pps.CaselessLiteral('end')
        term2 = pps.Literal(cset.symbols.slash)

        nend = pps.Combine(term1 | term2)

        namelist = nstart + pps.ZeroOrMore(sep | assign) + nend.suppress()

        # non-standard comment
        comment = pps.LineStart() + cset.symbols.sharp + pps.SkipTo('\n')

        # error
        err = nstart.copy()
        err.setParseAction(self.report)
        # warning
        # bare_str.setParseAction(self.warning)
        bare_str.addParseAction(self.warning)

        self.parser = (namelist | err.suppress())
        self.parser.ignore(comment)


    def report(self, st, locn, toks):
        self.errf.write("Cannot parse line %d.\n" % (pps.lineno(locn, st)))
        self.errf.write(">> %s\n" % (pps.line(locn, st)))

    def warning(self, st, locn, toks):
        pass


def main(argv):
    """Sample parser driver."""
    cset = FortranSets()
    fnml = FortranNamelist(cset)
    for f in argv:
        fin = open(f)
        src = ''.join(list(fin))
        # fn.parser.setDebug(True)
        for t in fnml.parser.searchString(src):
            ppr.pprint(t.asList())


if __name__ == '__main__':
    main(sys.argv[1:])

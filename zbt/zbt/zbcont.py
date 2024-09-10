#!/usr/bin/env python3
# Time-stamp: <2024/10/15 11:40:29 fuyuki zbcont.py>

import sys
import math
import argparse as ap
import pathlib as plib
import cProfile
import tomllib as toml

import cartopy
import cartopy.util
# import numpy as np
import xarray as xr
import matplotlib as mplib
import matplotlib.pyplot as plt
# import matplotlib.ticker as mtc
# import matplotlib.gridspec as mgs
# import mpl_toolkits.axes_grid1 as mag

# import os
# sys.path.insert(0, os.getcwd())

import zbt.util as zu
import zbt.control as zctl
import zbt.plot as zplt

mplib.use('Qt5Agg')

class Options(ap.Namespace):
    """Namespace to hold options"""

    method_table = {'f': 'contourf', 'c': 'contour',
                    'p': 'pcolormesh', 'i': 'imshow',
                    's': 'surface', }

    lsep = ','

    def __init__(self, argv, cmd=None):
        """Wrap argument parser."""
        epilog = """contour spec
 * contour specification
   INTERVAL[/....]      contour intervals (e.g., -C10/20)
   LEVEL,[...]          explicit contour levels (e.g., -C133,)
   NUMBER:[STEP]        number of contour levels (e.g., -C16:)

 * color specification
   INTERVAL
   LEVEL,[...]
   NUMBER:
"""

        parser = ap.ArgumentParser(prog=plib.Path(cmd).name,
                                   epilog=epilog,
                                   formatter_class=ap.RawTextHelpFormatter)
        parser.add_argument('--verbose',
                            action='store_const', const=1,
                            help='Be verbose')

        parser.add_argument('--no-decode_coords',
                            dest='decode_coords', action='store_false',
                            help='skip auto coordinate inclusion')
        parser.add_argument('files', metavar='FILE[/SPEC]',
                            type=str, nargs='+',
                            help='files, possibly with specifiers')
        parser.add_argument('-c', '--contours',
                            metavar='SPEC', default=None, type=str,
                            help='contour intervals or levels specification')
        parser.add_argument('-C', '--colors',
                            metavar='SPEC', default=None, type=str,
                            help='color intervals or levels specification.')
        parser.add_argument('-M', '--color-method',
                            metavar='METHOD/CMAP',
                            default=None, type=str,
                            help='coloring method and map'
                            ' {contour(c) contourf(f) pcolormesh(p)'
                            ' imshow(i) surface(s)}')
        parser.add_argument('-r', '--range',
                            metavar='[LOW][:[HIGH]]', dest='limit', type=str,
                            help='data range to draw')
        parser.add_argument('-d', '--dim',
                            metavar='DIM,[[LOW]:[HIGH]]', action='append',
                            type=str,
                            help='coordinate clipping')
        parser.add_argument('-v', '--variable',
                            metavar='VAR[,VAR...]', default=[], action='append',
                            type=str,
                            help='variable filter')
        parser.add_argument('-x', '--coordinate',
                            dest='coors',
                            metavar='HORIZONTAL,VERTICAL', default=None,
                            type=str,
                            help='figure coordinates')
        parser.add_argument('-o', '--output',
                            metavar='FILE', type=str,
                            help='output filename')
        parser.add_argument('-i', '--interactive',
                            action='store_const', const=True, default=None,
                            help='interactive mode')
        parser.parse_args(argv, namespace=self)

        self.coors = self.parse_coors(self.coors)

        self.tweak()

    def parse_coors(self, coors=None):
        if coors:
            coors = tuple(zu.toint(c) for c in coors.split(self.lsep))
        return coors

    def tweak(self, method=None, colors=None, contours=None):
        """Tweak options"""
        method = method or self.color_method
        colors = colors or self.colors
        contours = contours or self.contours

        method = method or ''
        method = tuple(method.split('/')) + (None, None)
        method, cmap = method[:2]
        method = self.method_table.get(method, method)

        if method in self.method_table:
            pass
        elif cmap is None:
            if method in mplib.colormaps:
                method, cmap = None, method
        method = method or 'contourf'

        self.colors_first = True

        if method == 'contour':
            # if color==contour, then contours are disabled default
            self.colors_first = False
            if contours is None:
                contours = False

        self.cmap = cmap
        self.color_method = method
        self.colors = colors
        self.contours = contours

        var = []
        self.variable = self.variable or []

        for v in self.variable:
            var.extend(v.split(','))
        if not var:
            self.variable = True
        else:
            self.variable = var

        dim = {}
        self.dim = self.dim or []
        for d in self.dim:
            dd = d.split(',')
            if len(dd) != 2:
                raise ValueError(f"Invalid dim filter: {d}.")
            dn = dd[0]
            dj = [None if j == '' else int(j) for j in dd[1].split(':')]
            try:
                dn = int(dn)
            except ValueError:
                pass
            if len(dj) == 1:
                # dim[dn] = slice(dj[0], dj[0]+1, None)
                dim[dn] = dj[0]
            else:
                dim[dn] = slice(dj[0], dj[1], None)
        self.dim = dim

        if self.limit is not None:
            r = self.limit + ':'
            r = r.split(':')
            self.limit = ((float(r[0]) if r[0] != '' else None), )
            self.limit = self.limit + ((float(r[1]) if r[1] != '' else None), )
        else:
            self.limit = None, None


def load_config(opts, *files, cmd=None, base=None):
    """Load multiple configuration files."""
    base = base or 'zbtrc.toml'
    config = {}
    files = list(files)
    if cmd:
        files.insert(0, cmd)
    for f in files:
        if f is None:
            continue
        f = plib.Path(f)
        if f.is_dir():
            f = f / base
        if f.exists():
            with open(f, "rb") as fp:
                c = toml.load(fp)
                config.update(c)

    # if cmd:
    #     kbase = config.get(cmd.stem, {})
    # else:
    #     kbase = config
    # opts.keymap = {}
    # parse_keymap(opts.keymap, kbase.get('keymap', {}))

    return config


def parse_keymap(opts, config, group=None):
    group = group or ()
    for f, c in config.items():
        if isinstance(c, dict):
            parse_keymap(opts, c, group + (f, ))
        elif isinstance(c, list):
            for k in c:
                opts[k] = (f, ) + group
        elif c != '':
            opts[c] = (f, ) + group


def main(argv, cmd=None):
    """Contour plot with TOUZA/Zbt."""
    # opts = parse_arguments(argv, cmd)
    files = []
    cstem = None
    if cmd:
        cmd = plib.Path(cmd)
        files.append(cmd.parent)
        cstem = cmd.stem

    opts = Options(argv, cmd)

    cfg = load_config(opts, *files, cmd=cstem)

    Array = zctl.ArrayIter()
    Var = zctl.VariableIter(child=Array, dim=opts.dim, coors=opts.coors)
    File = zctl.FileIter(opts.files, child=Var,
                         variables=opts.variable)

    Plot = zplt.ContourPlot(limit=opts.limit,
                            contour=opts.contours, color=opts.colors,
                            colors_first=opts.colors_first,
                            method=opts.color_method, cmap=opts.cmap)

    Figs = zctl.FigureControl(Plot, File,
                              output=opts.output,
                              config=cfg[cstem], params=plt.rcParams)
    try:
        Figs()
    except StopIteration as err:
        print(err)
        sys.exit(1)

def _driver():
    """Command line driver."""
    main(sys.argv[1:], sys.argv[0])
    # with cProfile.Profile() as pr:
    #     main(sys.argv[1:], sys.argv[0])
    #     pr.print_stats()


if __name__ == '__main__':
    _driver()

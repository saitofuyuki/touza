#!/usr/bin/env python3
# Time-stamp: <2024/10/24 15:03:52 fuyuki zbcont.py>

import sys
# import math
import argparse as ap
import pathlib as plib
import cProfile
import tomllib as toml
import re

import cartopy
# import cartopy.util
import cartopy.crs as ccrs

# import numpy as np
import xarray as xr
import matplotlib as mplib
import matplotlib.pyplot as plt
import matplotlib.backends.backend_pdf as mppdf

import matplotlib.ticker as mtc
# import matplotlib.gridspec as mgs
# import mpl_toolkits.axes_grid1 as mag

# import os
# sys.path.insert(0, os.getcwd())

import zbt.util as zu
import zbt.control as zctl
import zbt.plot as zplt

# mplib.use('Qt5Agg')

class Options(ap.Namespace):
    """Namespace to hold options"""

    method_table = {'f': 'contourf', 'c': 'contour',
                    'p': 'pcolormesh', 'i': 'imshow', }
    # reserved:  {'s': 'surface'}

    isep = '/'
    lsep = ','
    nsep = ':'
    psep = '+'

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
                            action='store_const', default=0, const=1,
                            help='Be verbose')
        parser.add_argument('--quiet', '--silent',
                            dest='quiet',
                            action='store_const', default=0, const=-1,
                            help='Be quiet')

        parser.add_argument('--debug',
                            action='store_const', default=0, const=1,
                            help='show debug information')

        parser.add_argument('--no-decode_coords',
                            dest='decode_coords', action='store_false',
                            help='skip auto coordinate inclusion')
        parser.add_argument('files', metavar='FILE[/SPEC]',
                            type=str, nargs='*',
                            help='files, possibly with specifiers')
        parser.add_argument('-c', '--contours', '--contour',
                            dest='contour',
                            metavar='SPEC', default=None, type=str,
                            help='contour intervals or levels specification')
        parser.add_argument('-C', '--colors', '--color',
                            dest='color',
                            metavar='SPEC', default=None, type=str,
                            help='color intervals or levels specification.')
        parser.add_argument('-M', '--color-method',
                            metavar='METHOD/CMAP',
                            default=None, type=str,
                            help='coloring method and map'
                            ' {contour(c) contourf(f) pcolormesh(p)'
                            ' imshow(i)}')
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
                            metavar='VERTICAL[,HORIZONTAL]', default=None,
                            type=str,
                            help='figure coordinates')
        parser.add_argument('-o', '--output',
                            metavar='FILE', type=str,
                            help='output filename')
        parser.add_argument('-i', '--interactive',
                            action='store_const', const=True, default=None,
                            help='interactive mode')
        parser.add_argument('-m', '--map',
                            metavar='MAP-SPECS', type=str,
                            default=None,
                            # nargs='?', default=None, const=True,
                            help='map overlay')
        parser.add_argument('-p', '--projection',
                            metavar='PROJECTION', type=str,
                            default=None,
                            help='map projection')
        parser.add_argument('-W', '--each-file',
                            dest='window',
                            action='store_const', default=0, const=1,
                            help='Create figure for each file')

        parser.parse_args(argv, namespace=self)

        self.coors = self.parse_coors(self.coors)
        self.output = self.parse_output(self.output)

        color = {}
        contour = {}
        # method, cmap, clevs =
        color, clevs = \
            self.parse_method(self.color_method, self.contour, color=color)

        contour['levels'] = self.parse_levels(clevs, single=False)
        color['levels'] = self.parse_levels(self.color, single=True)
        # color['method'] = method
        # color['cmap'] = cmap

        limit = self.parse_limit(self.limit)
        contour.update(limit)
        color.update(limit)

        self.color = color
        self.contour = contour

        self.styles = self.parse_styles(self.map, self.projection)

        if self.interactive is None:
            self.interactive = not bool(self.output)

        self.tweak()

        self.verbose = self.verbose - self.quiet

        self.parser = parser

    def parse_coors(self, coors=None):
        if coors:
            coors = tuple(zu.toint(c) for c in coors.split(self.lsep))
            if len(coors) == 1:
                coors = coors + ('', )
        # print(f"{coors=}")
        return coors

    def parse_output(self, output=None):
        if output:
            output = plib.Path(output)
        return output

    def parse_method(self, method, contour=None, color=None):
        color = color or {}

        method = method or ''
        method ,_, params = method.partition(self.psep)

        method = tuple(method.split('/')) + (None, None)
        method, cmap = method[:2]
        method = self.method_table.get(method, method)

        if method in self.method_table:
            pass
        elif cmap is None:
            if method in mplib.colormaps:
                method, cmap = None, method
        method = method or 'contourf'

        if method == 'contour':
            if contour is None:
                contour = False
        color['method'] = method
        color['cmap'] = cmap
        if params:
            color['alpha'] = float(params)
        return color, contour

    def parse_levels(self, text, single=False):
        # --contours=0              no contour
        # --contours=INT[/INT...]   contour intervals
        # --contours=LEV,[LEV,...]  explicit contour levels
        # --contours=NUM:[NUM,...]  total number of contour lines

        # --colors=0                 no fill
        # --colors=INT               intervals
        # --colors=LEV,[LEV,...]     explicit levels
        # --colors=NUM:              total number of colors
        if text is False:
            pat = [False]
        elif text is True:
            pat = [True]
        else:
            text = text or ''
            pat = []
            for item in text.split(self.isep):
                if self.lsep in item:
                    pat.append([float(jj)
                                for jj in item.split(self.lsep) if jj])
                elif self.nsep in item:
                    for n in [int(jj)
                              for jj in item.split(self.nsep) if jj]:
                        loc = mtc.MaxNLocator(n + 1)
                        pat.append(loc.tick_values)
                elif item:
                    item = float(item)
                    if item > 0:
                        loc = mtc.MultipleLocator(item)
                        pat.append(loc.tick_values)
                    else:
                        pat.append(False)
        if single:
            if len(pat) == 0:
                pat = None
            elif len(pat) == 1:
                pat = pat[0]
            else:
                raise ValueError(f"Non single level specification {text}.")
        # print(pat)
        return pat

    def parse_limit(self, limit=None):
        limit = limit or ''
        limit = limit.split(':')
        low = float(limit[0]) if limit[0] != '' else None
        limit = limit[1:] or ('', )
        high = float(limit[0]) if limit[0] != '' else None

        limit = {}
        if low is not None:
            limit['vmin'] = low
        if high is not None:
            limit['vmax'] = high
        return limit

    def tweak(self):
        """Tweak options"""

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

    def parse_styles(self, maps=None, projection=None,
                     crs=None, transform=None):
        styles = {}
        # print(maps, type(maps))
        if isinstance(maps, str):
            f = []
            if self.lsep in maps:
                maps = maps.split(self.lsep)
            for c in maps:
                if c in ['c', 'coast', ]:
                    f.append('COASTLINE')
                elif c in ['l', 'land', ]:
                    f.append('LAND')
                elif c in ['o', 'ocean', ]:
                    f.append('OCEAN')
                elif c in ['b', 'border', ]:
                    f.append('BORDERS')
                elif c in ['r', 'river', ]:
                    f.append('RIVERS')
                elif c in ['L', 'lake', ]:
                    f.append('LAKES')
                elif c in ['-', '', ]:
                    f.append(None)
                else:
                    raise ValueError(f"Invalid feature {c}")
            maps = f or [None]
        elif isinstance(maps, list):
            pass
        elif maps is not None:
            maps = [maps or None]
        # else:

        # --projection=PROJ[<+->lon[<+->lat]][,Y,X]
        #   +NUM > +NUM    +-NUM > -NUM   ++NUM > +NUM
        #   -NUM > -NUM    -+NUM > +NUM   --NUM > -NUM
        if isinstance(projection, str):
            projection = projection.split(self.lsep)
            proj = projection.pop(0)
            coor = projection

            pm = re.compile(r'(\w+)((?:[-+]{1,2}\w+)*)')
            m = pm.match(proj)
            proj = m.group(1)
            pm = re.compile(r'[-+]{1,2}\w+')
            params = pm.findall(m.group(2)) + [None] * 3

            clon = self.parse_float(params[0]) or 0
            clat = self.parse_float(params[1]) or 0
            height = self.parse_float(params[2]) or None

            if proj in ['m', 'mercator', ]:
                projection = ccrs.Mercator(central_longitude=clon)
            elif proj in ['w', 'mo', 'mollweide', ]:
                projection = ccrs.Mollweide(central_longitude=clon)
            elif proj in ['nps', 'northpolarstereo', ]:
                projection = ccrs.NorthPolarStereo(central_longitude=clon)
            elif proj in ['sps', 'southpolarstereo', ]:
                projection = ccrs.SouthPolarStereo(central_longitude=clon)
            elif proj in ['np', 'nearsideperspective', ]:
                kw = {}
                if height is not None:
                    kw['satellite_height'] = height
                projection = ccrs.NearsidePerspective(central_longitude=clon,
                                                      central_latitude=clat,
                                                      **kw,)
            elif proj in ['g', 'orthographic', ]:
                projection = ccrs.Orthographic(central_longitude=clon,
                                               central_latitude=clat, )
            elif proj in ['h', 'hammer', ]:
                projection = ccrs.Hammer(central_longitude=clon)
            elif proj in ['', 'pc', 'platecarree', ]:
                projection = ccrs.PlateCarree(central_longitude=clon)
            else:
                raise ValueError(f"Invalid feature {proj}")
            # print(projection)
            # print(f"{maps=}")
        if bool(projection):
            maps = maps or [None]
        if bool(maps):
            if not transform:
                transform = ccrs.PlateCarree()
            if not crs:
                crs = transform
            if not projection:
                projection = transform

        styles['features'] = maps
        styles['projection'] = projection
        styles['crs'] = crs
        styles['transform'] = transform

        # print(f"{styles=}")

        coors = (-2, -1)

        return {coors: styles}

    def parse_float(self, text):
        if text:
            if text[1] in ['+', '-']:
                text = text[1:]
            return float(text)
        return None

    def print_help(self, *args, **kwds):
        self.parser.print_help(*args, **kwds)

    def print_usage(self, *args, **kwds):
        self.parser.print_usage(*args, **kwds)

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
    for attr in ['verbose', ]:
        if hasattr(opts, attr):
            param = getattr(opts, attr)
            config[cmd][attr] = param
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

class Output:
    def __init__(self, name, method=None):
        method = method or 'serial'
        self._method = getattr(self, method, None)
        if self._method is None:
            raise ValueError(f"Unknown output method {method}")
        self.name = plib.Path(name)
        self.num = 0

    def __call__(self, *args):
        return self._method(*args)

    def format(self, num):
        return f"-{num}"

    def serial(self, *args):
        rev = self.format(self.num)
        base = self.name.stem + rev + self.name.suffix
        path = self.name.parent / base
        if path.exists():
            raise ValueError(f"Exists {path}")
        self.num = self.num + 1
        return path

def main(argv, cmd=None):
    """Contour plot with TOUZA/Zbt."""
    # opts = parse_arguments(argv, cmd)
    files = []
    cstem = None
    if cmd:
        cmd = plib.Path(cmd)
        files.append(cmd.parent)
        cstem = cmd.stem
    loc = plib.Path(__file__)
    files.append(loc.parent)

    opts = Options(argv, cmd)

    if opts.debug > 0:
        import zbt.dsnio as znio
        znio.TouzaNioDataset.debug()
        print(f"{loc}")
        for m in [zu, zctl, zplt, ]:
            print(f"{m}")

    cfg = load_config(opts, *files, cmd=cstem)

    if len(opts.files) == 0:
        opts.print_usage()
        sys.exit(0)

    if opts.window > 0:
        Iter = []
        for f in opts.files:
            Array = zctl.ArrayIter()
            Var = zctl.VariableIter(child=Array, dim=opts.dim,
                                    coors=opts.coors)
            Iter.append(zctl.FileIter([f], child=Var,
                                      variables=opts.variable))
    else:
        Array = zctl.ArrayIter()
        Var = zctl.VariableIter(child=Array, dim=opts.dim, coors=opts.coors)
        Iter = zctl.FileIter(opts.files, child=Var,
                             variables=opts.variable)

    Layout = zplt.LayoutLegacy3

    Plot = zplt.ContourPlot(contour=opts.contour, color=opts.color)

    Figs = zctl.FigureControl(Plot, Iter,
                              interactive=opts.interactive,
                              layout=Layout,
                              styles=opts.styles,
                              config=cfg.get(cstem), params=plt.rcParams)

    output = opts.output
    try:
        if output and output.suffix == '.pdf':
            if output and output.exists():
                raise ValueError(f"Exists {output}")
            with mppdf.PdfPages(output) as output:
                Figs(output)
        else:
            if output:
                output = Output(name=output)
            Figs(output)
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

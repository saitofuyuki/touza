#!/usr/bin/env python3
# Time-stamp: <2024/11/11 09:06:20 fuyuki zbcont.py>

import sys
# import math
import argparse as ap
import pathlib as plib
import cProfile
import tomllib as toml
import re
import functools as ft

import matplotlib as mplib
import matplotlib.pyplot as plt
import matplotlib.backends.backend_pdf as mppdf
import matplotlib.ticker as mtc

# import matplotlib.gridspec as mgs
# import mpl_toolkits.axes_grid1 as mag

import cartopy
# import cartopy.util
import cartopy.crs as ccrs
# import numpy as np
import xarray as xr

# import os
# sys.path.insert(0, os.getcwd())

import zbt.util as zu
import zbt.control as zctl
import zbt.plot as zplt

class ParserUtils():
    """Common parameters and methods for parser."""

    isep = '/'
    lsep = ','
    nsep = ':'
    psep = '+'

    def parse_float(self, text):
        if text:
            if text[1] in ['+', '-']:
                text = text[1:]
            return float(text)
        return None


class Projection(ParserUtils):
    table = {'platecarree': (ccrs.PlateCarree, 'central_longitude', ),
             'mercator': (ccrs.Mercator, 'central_longitude', ),
             'mollweide': (ccrs.Mollweide, 'central_longitude', ),
             'northpolarstereo': (ccrs.NorthPolarStereo,
                                  'central_longitude', ),
             'southpolarstereo': (ccrs.SouthPolarStereo,
                                  'central_longitude', ),
             'nearsideperspective': (ccrs.NearsidePerspective,
                                     'central_longitude',
                                     'central_latitude',
                                     'satellite_height', ),
             'orthographic': (ccrs.Orthographic,
                              'central_longitude', 'central_latitude',),
             'hammer': (ccrs.Hammer, 'central_longitude', ), }
    alias = {'': 'platecarree', 'pc':'platecarree',
             'm': 'mercator',
             'w': 'mollweide', 'mo': 'mollweide',
             'nps': 'northpolarstereo',
             'sps': 'southpolarstereo',
             'np': 'nearsideperspective',
             'g': 'orthographic',
             'h': 'hammer', }

    keys = list(table.keys())

    ppr = re.compile(r'(\w+)((?:[-+]{1,2}\w+)*)')
    ppo = re.compile(r'[-+]{1,2}\w+')

    # --projection=PROJ[<+->lon[<+->lat]][,Y,X]
    #   +NUM > +NUM    +-NUM > -NUM   ++NUM > +NUM
    #   -NUM > -NUM    -+NUM > +NUM   --NUM > -NUM

    def __init__(self, param):
        param = param.split(self.lsep)
        proj = param.pop(0)
        self.coor = param

        m = self.ppr.match(proj)
        proj = m.group(1)
        opts = self.ppo.findall(m.group(2)) + [None] * 3

        self.lon = self.parse_float(opts[0]) or 0
        self.lat = self.parse_float(opts[1]) or 0
        self.height = self.parse_float(opts[2]) or None

        proj = self.alias.get(proj, proj)
        if proj not in self.table:
            raise ValueError(f"Invalid projection {proj}")
        self.proj = proj

        self.cache = {}

    def __call__(self, **kwds):
        if self.proj not in self.table:
            raise ValueError(f"Invalid projection {proj}")
        args = self.table[self.proj]
        cls = args[0]
        args = args[1:]
        for a in args:
            if a == 'central_longitude':
                kwds.setdefault(a, self.lon)
            elif a == 'central_latitude':
                kwds.setdefault(a, self.lat)
            elif a == 'satellite_height':
                if self.height is not None:
                    kwds.setdefault(a, self.height)
            else:
                raise ValueError(f"Panic. Unknown projection parameter {a}")
        ck = (cls, ) + tuple(kwds.items())
        projection = self.cache.get(ck)
        if projection is None:
            projection = cls(**kwds)
            self.cache[ck] = projection
        return projection

    def switch(self, key=None):
        if isinstance(key, int):
            j = self.keys.index(self.proj)
            if key > 0:
                j = (j + 1) % len(self.keys)
            else:
                j = j - 1
            key = self.table[self.keys[j]]
        if key not in self.table:
            raise ValueError(f"Invalid projection {key}")
        self.proj = key


class Options(ParserUtils, ap.Namespace):
    """Namespace to hold options"""

    method_table = {'f': 'contourf', 'c': 'contour',
                    'p': 'pcolormesh', 'i': 'imshow', }
    # reserved:  {'s': 'surface'}

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

        parser.add_argument('--force',
                            dest='force', action='store_true',
                            help='overwrite outputs')

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
        parser.add_argument('-v', '--variable',
                            metavar='VAR[,VAR...]', default=[], action='append',
                            type=str,
                            help='variable filter')
        # --dim DIM/LOW:HIGH
        # --dim DIM,LOW:HIGH       (deprecated)
        parser.add_argument('-d', '--dim',
                            metavar='DIM,[[LOW]:[HIGH]]', action='append',
                            type=str,
                            help='coordinate clipping')
        # --draw DIM/NAME/LOW:HIGH,DIM/NAME/LOW:HIGH
        parser.add_argument('-D', '--draw',
                            dest='draw',
                            metavar='COORDINATE/RANGE[,...]', action='append',
                            type=str,
                            help='figure coordinates')
        # --coordinate VERTICAL,HORIZONTAL       (deprecated)
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
        self.verbose = self.verbose - self.quiet

        self.coors = self.parse_coors(self.coors)
        self.output = self.parse_output(self.output)
        if self.coors:
            print(f"deprecated: {self.coors}")
            self.coors = self.parse_coors(self.coors)
            self.draw, _ = self.parse_draw(self.draw)
        else:
            self.draw, self.coors = self.parse_draw(self.draw)

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

        self.parser = parser

    def parse_coors(self, coors=None):
        """Parse coordinate(selection) parameters."""
        if coors:
            coors = tuple(zu.toint(c) for c in coors.split(self.lsep))
            if len(coors) == 1:
                coors = coors + ('', )
        # print(f"{coors=}")
        return coors

    def parse_draw(self, draw=None):
        """Parse draw(view) parameters.
        --dimension=[COORDINATE[/SELECTION]][,[COORDINATE[/SELECTION]][,[...]]]
        COORAINATE can be either index or (long-)name.
        SELECTION can be RANGE, POINT, or empty.

        --dimension=-3          Set dims[-3] as y-coordinate.
        --dimension=,-2         Set dims[-2] as x-coordinate.
        --dimension=-1/10       Set dims[-1] anchor as index 10.
                                Plot will be with other effective coordinates.
        --dimension=-1/10 --dimension=-3
        --dimension=-1/10,-3    Set dims[-1] anchor as index 10,
                                and set dims[-3] as y-coordinate.
        --dimension=-1/10,,-2   Set dims[-1] anchor as index 10,
                                and set dims[-2] as x-coordinate.
        --dimension=-3/10:      Set dims[-3] as y-coordinate,
                                with initial view of index 10 to the end
                                (== 10 to -1)
        --dimension=-3/:20      Set dims[-3] as y-coordinate,
                                with initial view of index 0 to 20 (== 0 to 19)
        --dimension=-3/10:20    Set dims[-3] as y-coordinate,
                                with initial view of index 10:20  (== 10 to 19)
        --dimension=-3/10:20.   Set dims[-3] as y-coordinate,
                                with initial view from index 10 to point 20.0
                                (inclusive)
        --dimension=-3/10.:20.  Set dims[-3] as y-coordinate,
                                with initial view range from point 10.0 to 20.0
                                (inclusive)
        --dimension=time        Set dimension to match with `time'
                                as y-coordinate.
        """
        draw = draw or {}
        if draw:
            _draw = {}
            for od in self.chain_split(draw, self.lsep):
                d = od.split(self.isep)
                if len(d) == 0:
                    pass
                elif len(d) == 1:
                    c = zu.toint(d[0])
                    _draw[c] = None
                elif len(d) == 2:
                    c = zu.toint(d[0])
                    d[1] = d[1] or self.nsep
                    s = [None if j == '' else zu.tonumber(j)
                         for j in d[1].split(self.nsep)]
                    if len(s) == 2:
                        _draw[c] = slice(s[0], s[1], None)
                    elif len(s) == 1:
                        _draw[c] = s[0]
                    else:
                        raise ValueError(f'Need valid range {od}')
                else:
                    raise ValueError(f'invalid draw {od}')
            # for od in draw or []:
            #     for od in od.split(self.lsep):
            #         d = od.split(self.isep)
            #         if len(d) == 0:
            #             pass
            #         elif len(d) == 1:
            #             c = zu.toint(d[0])
            #             _draw[c] = None
            #         elif len(d) == 2:
            #             c = zu.toint(d[0])
            #             d[1] = d[1] or self.nsep
            #             s = [None if j == '' else zu.tonumber(j)
            #                  for j in d[1].split(self.nsep)]
            #             if len(s) == 2:
            #                 _draw[c] = slice(s[0], s[1], None)
            #             elif len(s) == 1:
            #                 _draw[c] = s[0]
            #             else:
            #                 raise ValueError(f'Need valid range {od}')
            #         else:
            #             raise ValueError(f'invalid draw {od}')
            draw = _draw
        coors = []
        for d, s in draw.items():
            if isinstance(s, slice) or s is None:
                coors.append(d)
        coors = tuple(coors + ['', '', ])[:2]
        # coors = list(draw.keys()) + ['', '']
        # coors = tuple(coors[:2])
        if self.debug > 0:
            print(f"{draw=} {coors=}")

        return draw, coors

    def chain_split(self, args, sep=None):
        """Iterator to split all the list items."""
        for a in args or []:
            a = a or ''
            yield from a.split(sep)

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
            dd = d.split(self.isep)
            if len(dd) != 2:
                dd = d.split(self.lsep)
                if len(dd) != 2:
                    raise ValueError(f"Invalid dim filter: {d}.")
                else:
                    print(f"Deprecated.  Use --dim/LOW:HIGH format.")

            dn = dd[0]
            dj = [None if j == '' else zu.tonumber(j)
                  for j in dd[1].split(':')]
            dn = zu.toint(dn)
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
        self.proj = None
        if isinstance(projection, str):
            # projection = Projection(projection)
            self.proj = Projection(projection)
            projection = self.proj()

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
    def __init__(self, name, force=None, method=None):
        method = method or 'serial'
        self._method = getattr(self, method, None)
        if self._method is None:
            raise ValueError(f"Unknown output method {method}")
        self.name = plib.Path(name)
        self.num = 0
        self.force = zu.set_default(force, False)

    def __call__(self, *args, **kwds):
        return self._method(*args, **kwds)

    def format(self, num, ref=None, pad=None, sep=None,
               **kwds):
        if ref:
            if ref <= 1:
                pad = pad or False
            else:
                pad = pad or len(str(ref))

        if pad is False:
            return ''

        if pad:
            num = f"{num:>0{pad}}"
        else:
            num = str(num)

        if sep is None:
            sep = '-'
        return sep + num

    def serial(self, *args, **kwds):
        rev = self.format(self.num, **kwds)
        base = self.name.stem + rev + self.name.suffix
        path = self.name.parent / base
        if path.exists():
            if not self.force:
                raise RuntimeError(f"Exists {path}")
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

    fopts = dict(decode_coords=opts.decode_coords)

    viter = ft.partial(zctl.VariableIter,
                       dim=opts.dim, coors=opts.coors,
                       anchors=opts.draw, debug=opts.debug)

    if opts.window > 0:
        Iter = []
        for f in opts.files:
            Array = zctl.ArrayIter()
            Var = viter(child=Array)
            # Var = zctl.VariableIter(child=Array, dim=opts.dim,
            #                         coors=opts.coors)
            Iter.append(zctl.FileIter([f], child=Var,
                                      variables=opts.variable, opts=fopts))
    else:
        Array = zctl.ArrayIter()
        # Var = zctl.VariableIter(child=Array, dim=opts.dim, coors=opts.coors)
        Var = viter(child=Array)
        Iter = zctl.FileIter(opts.files, child=Var,
                             variables=opts.variable, opts=fopts)

    Layout = zplt.LayoutLegacy3
    Layout.config(cfg, verbose=True)
    if opts.debug > 0:
        Layout.diag(strip=False)

    Plot = zplt.ContourPlot
    Plot.config(cfg, verbose=True)
    if opts.debug > 0:
        Plot.diag(strip=False)

    Cmap = zctl.CmapIter()
    if opts.color.get('cmap') is None:
        opts.color['cmap'] = Cmap.value

    Pic = zplt.Picture
    plot = Plot(contour=opts.contour, color=opts.color)
    # pic = zplt.Picture(Layout)
    # Plot = zplt.ContourPlot(contour=opts.contour, color=opts.color)

    Ctl = zctl.FigureControl(Pic, plot, Iter,
                             interactive=opts.interactive,
                             cmap=Cmap,
                             layout=Layout,
                             styles=opts.styles,
                             draw=opts.draw,
                             config=cfg.get(cstem), params=plt.rcParams)
    output = opts.output
    try:
        if output and output.suffix == '.pdf':
            if output and output.exists():
                if not opts.force:
                    raise RuntimeError(f"Exists {output}")
            with mppdf.PdfPages(output) as output:
                Ctl(output)
        else:
            if output:
                output = Output(name=output, force=opts.force)
            Ctl(output)
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

#!/usr/bin/env python3
# Time-stamp: <2024/10/24 17:04:41 fuyuki plot.py>

__doc__ = \
    """
zbt.plot
~~~~~~~~
Plotter collections.

:Source:     zbt/plot.py
:Maintainer: SAITO Fuyuki <saitofuyuki@jamstec.go.jp>
:Created:    Oct 9 2024
"""

import sys
import collections.abc as cabc
import numbers as nums

import numpy as np
import matplotlib as mplib
import matplotlib.ticker as mtc
import matplotlib.pyplot as plt
import matplotlib.backend_tools as mpbt

import xarray as xr
import xarray.plot.utils as xrpu
import cartopy.crs as ccrs
import cartopy.mpl.geoaxes as cmgeo
import cartopy.mpl.ticker as cmtick
import cartopy.feature as cfeature
import cartopy.util as cutil

# import cartopy.crs as ccrs

from . import util


class FigureBase(mplib.figure.Figure):
    """Abstract base layer of matplotlib figure for zbt."""


class TestTools(mpbt.ToolBase):
    """Test."""
    default_keymap = None
    description = 'Test'


class LayoutBase():
    """Base layer of figure layout.
    Contains three matplotlib axes.
    """

    def __init__(self, figsize, layout=None, cls=None, **kw):
        self.figsize = figsize
        self.enlarge = 0
        self.layout = layout or "none"
        self.figkw = kw

        # set cls=False to clear cls
        cls = FigureBase if cls is None else cls
        self.cls = cls or None

        self.axkw = {}
        self.atbl = {}

    def __call__(self, axs=None,
                 figsize=None, layout=None, FigureClass=None,
                 **kwds):
        """Create figure and axes."""
        figsize = figsize or self.figsize
        layout = layout or self.layout
        FigureClass = FigureClass or self.cls

        fig = plt.figure(figsize=figsize,
                         layout=layout, FigureClass=FigureClass,
                         **self.figkw)
        if self.figsize is None:
            self.figsize = fig.get_size_inches()
        self.enlarge = 0.0
        # dummy
        axs = axs or self
        self.axkw = kwds
        return fig, axs

    def reset(self, fig, axs=None, **kwds):
        """Dummy procedure to reset figure."""

    def _resize_calc(self, base, rate):
        if rate >= 0:
            return base * (1 + rate)
        return base / (1 - rate)

    def _resize_prop(self, org, cur, rate):
        if rate is None:
            return 0
        cur = self._resize_calc(cur, rate)
        r = cur / org
        if r >= 1:
            return r - 1
        return 1 - org / cur

    def resize(self, fig, axs=None, rate=None):
        """Resize figure."""
        geo = fig.get_size_inches()
        self.enlarge = self._resize_prop(self.figsize[0], geo[0], rate)
        geo = self._resize_calc(np.array(self.figsize), self.enlarge)
        fig.set_size_inches(geo)

    def parse_coor(self, coor, item=False):
        """Fallback coordinate limit parser"""
        dmax = coor[-1]
        dmin = coor[0]
        if item:
            dmax = dmax.item()
            dmin = dmin.item()
        if dmin > dmax:
            dmin, dmax = dmax, dmin
        # print(coor[-1], dmax)
        # print(coor[0], dmin)
        return (dmin, dmax)

    def parse_ticks(self, _, default=0):
        """Fall back major and minor locators."""
        major = default
        minor = default
        return (major, minor)

    def set_ticks(self, x, y, axs=None, major=None, minor=None, key=None):
        """Set tick labels."""
        axs = axs or self
        key = key or 'body'
        ax = axs.atbl.get(key, None)

        major = major or {}
        minor = minor or {}
        for co, axis, limf in [(x, ax.xaxis, ax.set_xlim),
                               (y, ax.yaxis, ax.set_ylim)]:
            dmin, dmax = self.parse_coor(co)
            span = abs(dmax - dmin)
            vmaj, vmin = self.parse_ticks(co.attrs)
            if span > vmaj > 0:
                axis.set_major_locator(mtc.MultipleLocator(vmaj))
            if vmin > 0:
                axis.set_minor_locator(mtc.MultipleLocator(vmin))
            axis.set_label_text(xrpu.label_from_attrs(co), fontsize=13)
            axis.set_tick_params(which='major', **major)
            axis.set_tick_params(which='minor', **minor)
            if dmin != dmax:
                limf(dmin, dmax)

    def set_geoticks(self, x, y, axs=None, major=None, minor=None,
                     crs=None, key=None):
        """Set GeoAxes extent and tick labels."""
        axs = axs or self
        key = key or 'body'
        ax = axs.atbl.get(key, None)

        lon = x
        lat = y

        x0, x1 = self.parse_coor(lon)
        y0, y1 = self.parse_coor(lat)

        # print(x0, x1, y0, y1)
        try:
            if abs(x1 - x0) > 359 and abs(y1 - y0) > 179:
                # raise ValueError(f"Force to set global {x0}:{x1}")
                # raise ValueError(f"Force to set global.")
                raise ValueError('')
            if abs(x1 - x0) >= 360:
                if x1 > x0:
                    x1 = x1 + 0.01
                else:
                    x0 = x0 + 0.01
            ax.set_extent((x0, x1, y0, y1), crs=crs)
        except ValueError as err:
            if str(err):
                print(err)
            ax.set_global()

        major = major or dict(labelsize=13)
        minor = minor or {}

        try:
            self.set_lon_ticks(ax, lon, crs, major, minor)
            self.set_lat_ticks(ax, lat, crs, major, minor)
        except RuntimeError as err:
            print(err)
            gl = ax.gridlines(draw_labels=True)
            gl.top_labels = False
            gl.right_labels = False
        ax.xaxis.set_label_text(xrpu.label_from_attrs(lon), fontsize=13)
        ax.yaxis.set_label_text(xrpu.label_from_attrs(lat), fontsize=13)

    def set_lon_ticks(self, ax, lon, crs, major=None, minor=None):
        major = major or {}
        minor = minor or {}
        x0, x1 = self.parse_coor(lon, item=True)
        span = abs(x1 - x0)
        vmaj, vmin = self.parse_ticks(lon.attrs)
        # print(f"lon:{span=} {vmaj=}")
        if span > vmaj > 0:
            loc = mtc.MultipleLocator(vmaj)
        else:
            loc = cmtick.LongitudeLocator()
        # print(type(loc), loc)
        loc = [lo for lo in loc.tick_values(x0, x1)
               if x0 <= lo <= x1]
        # print((x0, x1), loc)
        loc = sorted([lo - 360 if lo >= 180 else lo for lo in loc])
        ax.set_xticks(ticks=loc, crs=crs)

        if vmin > 0:
            loc = mtc.MultipleLocator(vmin)
        else:
            loc = cmtick.LongitudeLocator()
        loc = [lo for lo in loc.tick_values(x0, x1)
               if x0 <= lo <= x1]
        loc = sorted([lo - 360 if lo >= 180 else lo for lo in loc])
        ax.set_xticks(ticks=loc, minor=True, crs=crs)

        fmt = cmtick.LongitudeFormatter()
        ax.xaxis.set_major_formatter(fmt)
        # ax.xaxis.set_minor_formatter(fmt)
        ax.xaxis.set_tick_params(which='major', **major)
        ax.xaxis.set_tick_params(which='minor', **minor)

    def set_lat_ticks(self, ax, lat, crs, major=None, minor=None):
        major = major or {}
        minor = minor or {}
        x0, x1 = self.parse_coor(lat, item=True)
        if x0 > x1:
            x0, x1 = x1, x0
        # print(x0, x1)
        span = abs(x1 - x0)
        vmaj, vmin = self.parse_ticks(lat.attrs)
        # print(f"{span=}\n{vmaj=}\n{vmin=}")
        # print((x0, x1))
        if span > vmaj > 0:
            loc = mtc.MultipleLocator(vmaj)
        else:
            loc = cmtick.LatitudeLocator()
        loc = [lo for lo in loc.tick_values(x0, x1)
               if x0 <= lo <= x1]
        ax.set_yticks(ticks=loc, crs=crs)

        if vmin > 0:
            loc = mtc.MultipleLocator(vmin)
        else:
            loc = cmtick.LatitudeLocator()
        loc = [lo for lo in loc.tick_values(x0, x1)
               if x0 <= lo <= x1]
        ax.set_yticks(ticks=loc, minor=True, crs=crs)

        fmt = cmtick.LatitudeFormatter()
        ax.yaxis.set_major_formatter(fmt)
        ax.yaxis.set_tick_params(which='major', **major)
        ax.yaxis.set_tick_params(which='minor', **minor)

    def add_features(self, /, *features, axs=None, key=None):
        """Set GeoAxes extent and tick labels."""
        axs = axs or self
        key = key or 'body'
        ax = axs.atbl.get(key, None)

        for ft in features:
            if isinstance(ft, str):
                ft = getattr(cfeature, ft)
            if ft:
                ax.add_feature(ft)

    def cbar_set_ticks(self, axs=None,
                       major=None, minor=None, key=None):
        """Set tick labels of colorbar."""
        axs = axs or self
        key = key or 'colorbar'
        cax = axs.atbl.get(key, None)

        major = major or {}
        minor = minor or {}
        cax.tick_params(which='major', **major)
        cax.tick_params(which='minor', **minor)


class LegacyParser(LayoutBase):
    """GTOOL3 legacy attribute parser."""

    def extract_titles(self, attrs):
        """Extract title properites from zbt-like data."""
        title = attrs.get('long_name', None)
        if title is None:
            title = util.join_attrs(attrs, 'TITL', sep='', strip=True)
        units = attrs.get('units', None)
        if units is None:
            units = attrs.get('UNIT', '').strip()
        if units == 'ND':
            units = ''
        return title, units

    def parse_slices(self, coords):
        """Parse slice properties."""
        sel = []
        for d, c in coords.items():
            # print(d, c)
            if c.size == 1:
                dt, du = self.extract_titles(c.attrs)
                dt = dt or d
                if dt.lower() in ['time', 'record', ]:
                    continue
                ds = f"{dt}={c.values:.4f}"
                if du:
                    ds = ds + f'[{du}]'
                sel.append(ds)
        sel = ' '.join(sel)
        return sel

    def parse_titles(self, attrs, coords, default=None):
        """Parse basic title properties.
        Return a tuple of (title, unit, others). """

        size = 3
        if isinstance(default, tuple):
            default = default + (None, ) * size
        elif isinstance(default, str):
            default = (default, ) * size
        elif not isinstance(default, cabc.Iterable):
            default = (default, ) * size
        else:
            raise ValueError(f"invalid argument: {default=}")

        title, units = self.extract_titles(attrs)
        item = attrs.get('ITEM', None)
        ettl = util.join_attrs(attrs, 'ETTL', sep='', strip=True)
        ## hack
        if len(ettl) > 16:
            ettl = ettl[:16] + '...'
        sel = self.parse_slices(coords)

        if item:
            if ettl:
                item = item + ' ' + ettl
        elif ettl:
            item = ettl
        else:
            item = ''
        if sel:
            item = item + ' ' + sel

        ret = tuple(v or d for v, d in zip((title, units, item), default))
        return ret

    def parse_dataset(self, attrs, default=None):
        """Parse dataset property."""
        ret = attrs.get('DSET', None)
        ret = ret or default
        return ret

    def parse_ticks(self, attrs, default=0):
        """Parse major and minor locators."""
        major = attrs.get('DIVL', None)
        minor = attrs.get('DIVS', None)

        def subst(v):
            """Result converter."""
            return default if v is None else float(v)

        major = subst(major)
        minor = subst(minor)
        return (major, minor)

    def parse_coor(self, coor, item=False):
        """Parse axis limit."""
        miss = coor.attrs.get('MISS', '').strip()

        def parse_sub(key):
            ikey = f'_ignore_{key}'
            if coor.attrs.get(ikey, None):
                d = ''
            else:
                d = coor.attrs.get(key, '').strip()
                d = '' if d == miss else d
            return d
        dmax = parse_sub('DMAX')
        dmin = parse_sub('DMIN')
        if dmax and dmin:
            dmax = float(dmax)
            dmin = float(dmin)
        else:
            vmin, vmax = super().parse_coor(coor, item=item)
            dmax = float(dmax) if dmax else vmax
            dmin = float(dmin) if dmin else vmin
        # print(f"{dmin=} {dmax=}")
        return (dmin, dmax)

    def parse_calendar(self, attrs, default=None):
        """Parse calendar properties."""
        dt0 = self.parse_date(attrs, 'DATE')
        dt1 = self.parse_date(attrs, 'DATE1')
        dt2 = self.parse_date(attrs, 'DATE2')
        if (dt0 != dt1 or dt1 != dt2 or dt2 != dt0) and dt1 and dt2:
            cal = '/'.join(dt1[:3]) + ' ' + ':'.join(dt1[3:6])
            cal = cal + '-'
            cal = cal + '/'.join(dt2[:3]) + ' ' + ':'.join(dt2[3:6])
        elif dt0:
            cal = '/'.join(dt0[:3]) + ' ' + ':'.join(dt0[3:6])
        else:
            cal = None
        return cal or default

    def parse_date(self, attrs, key):
        """Parse date attribute to tuple."""
        dt = attrs.get(key, '')
        if isinstance(dt, str):
            dt = dt.strip()
            if dt:
                dt = dt.split(' ')
                if len(dt) == 1:
                    d = dt[-2:]
                    m = dt[-4:-2]
                    y = dt[:-4]
                    h, mi, s = '', '', ''
                else:
                    tm = dt[1]
                    dt = dt[0]
                    d = dt[-2:]
                    m = dt[-4:-2]
                    y = dt[:-4]
                    s = tm[-2:]
                    mi = tm[-4:-2]
                    h = tm[:-4]

                return (y, m, d, h, mi, s)
        return None


class LayoutLegacy3(LegacyParser, LayoutBase):
    """gtcont layout 3 emulation."""
    name = 'Legacy3'

    pos = {'bottom': True, 'top': True, 'right': True, 'left': True, }
    major = {'length': 10.8, 'width': 1, 'pad': 5.8, 'labelsize': 14, }
    minor = {'length': 5.4, 'width': 0.7, }
    major.update(pos)
    minor.update(pos)

    cbar_major = {'bottom': True, 'length': 10.8,
                  'width': 1, 'pad': 5.8, 'labelsize': 14, }
    cbar_minor = False

    geometry = (10.45, 7.39)

    ofx, ofy = 0.2, 0.02
    rect = {'body': (0.129, 0.225, 0.830, 0.585),
            # 'info': (0.129 + ofx, 0.070 - ofy, 0.830 - ofx * 2, 0.057 + ofy),
            'info': (0.129 + ofx, ofy, 0.830 - ofx * 2, 0.1042 - ofy),
            'colorbar': (0.061, 0.070, 0.245, 0.057), }

    title_text = {'pad': 18.7,
                  'linespacing': 1.3,
                  'fontsize': 14, }
    contour_text = {'fontsize': 12, }

    def __init__(self, *args, figsize=None, config=None, **kw):
        self.config(config)
        figsize = figsize or self.geometry
        super().__init__(*args, figsize=figsize, **kw)

    def __call__(self, axs=None, figsize=None, **kw):
        """Create figure and standard axes."""
        fig, axs = super().__call__(axs=axs, **kw)

        if figsize:
            if figsize is True:
                figsize = self.geometry
            elif isinstance(figsize, (int, float)):
                geo = figsize.get_size_inches()
                if figsize >= 0:
                    figsize = geo * (1.0 + figsize)
                else:
                    figsize = geo / (1.0 - figsize)
            elif hasattr(figsize, 'get_size_inches'):
                figsize = figsize.get_size_inches()
            fig.set_size_inches(figsize)

        self.reset(fig, axs)
        return fig, axs

    def config(self, config=None):
        config = config or {}
        verbose = config.get('verbose') or 0
        config = config.get(self.name) or {}
        if verbose > 0:
            verbose = sys.stdout
        util.update_config(self, config, pfx=f'config[{self.name}]',
                           verbose=verbose)

    def reset(self, fig, axs=None, **kwds):
        """Bind standard axes."""
        axs = axs or self

        fig.clf()

        for lab, rc in self.rect.items():
            axkw = (self.axkw.get(lab, None) or {})\
                | (kwds.get(lab, None) or {})
            # print(lab, axkw)
            self.atbl[lab] = fig.add_axes(rc, label=lab, **axkw)
            # print(lab, self.atbl[lab])
        self.atbl['info'].set_axis_off()

        return axs

    def set_ticks(self, *args, major=None, minor=None, **kw):
        """Set tick labels."""
        major = major or self.major
        minor = minor or self.minor
        super().set_ticks(*args, major=major, minor=minor, **kw)

    def cbar_set_ticks(self, *args, major=None, minor=None, **kw):
        """Set colorbar ticks."""
        major = major or self.cbar_major
        minor = minor or self.cbar_minor
        super().cbar_set_ticks(*args, major=major, minor=minor, **kw)

    def add_titles(self, data, axs=None, contours=None,
                   key=None, tkey=None, **kw):
        """Add title."""

        axs = axs or self
        key = key or 'body'
        tkey = tkey or 'info'
        ax = axs.atbl.get(key, None)
        tax = axs.atbl.get(tkey, None)

        tkw = self.title_text | kw

        left = self.parse_titles(data.attrs, data.coords, default='')
        ax.set_title('\n'.join(left), loc='left', **tkw)

        dset = self.parse_dataset(data.attrs, default='')
        cal = self.parse_calendar(data.attrs, default='')
        right = (dset, cal, '')

        ax.set_title('\n'.join(right), loc='right', **tkw)

        if contours:
            dc = contours[0].levels[1:] - contours[0].levels[:-1]
            if len(set(dc)) == 1:
                ctext = f'CONTOUR INTERVAL = {dc[0]}'
                tax.text(0.5, 1.0, ctext,
                         horizontalalignment='center',
                         verticalalignment='top',
                         **self.contour_text)

    def colorbar(self, fig, *args, axs=None, orientation=None, key=None, **kw):
        """Wrap colorbar"""
        axs = axs or self
        key = key or 'colorbar'
        cax = axs.atbl.get(key, None)
        orientation = orientation or 'horizontal'
        return fig.colorbar(*args, cax=cax, orientation=orientation, **kw)


class ContourPlot:
    """Contour with fill-color."""

    isep = '/'
    lsep = ','
    nsep = ':'

    color_method = 'contourf'
    color_method = 'imshow'

    opts = {'add_labels': False, }

    method_table = {'f': 'contourf', 'c': 'contour',
                    'p': 'pcolormesh', 'i': 'imshow',
                    's': 'surface', }

    def __init__(self, contour=None, color=None):
        self._contour = {'colors': 'black'}
        self._color = {}

        self._contour.update(contour or {})
        self._color.update(color or {})

    def __call__(self, fig, axs, data, cyclic=None, title=None, **kwds):
        """Batch plotter."""
        if any(w <= 1 for w in data.shape):
            raise UserWarning("virtually less than two dimensions")

        coords = data.coords
        cj = [d for d in data.dims if coords[d].size > 1]
        if len(cj) != 2:
            raise ValueError(f"Not 2d shape={cj}")
        xco = coords[cj[1]]
        yco = coords[cj[0]]

        if cyclic is None:
            for ck in [0, 1, ]:
                c = coords[cj[ck]]
                cc = c.attrs.get('cyclic_coordinate')
                if not cc:
                    continue
                w, org, dup = cc
                if len(c) == w - 1 \
                   and c[0] == org \
                   and c[-1] != dup:
                    cd, cx = cutil.add_cyclic(data, c, axis=ck,
                                              cyclic=dup)
                    nco = dict(data.coords.items())
                    nco[cj[ck]] = cx
                    data = xr.DataArray(cd, coords=nco,
                                        dims=data.dims, attrs=data.attrs)
                    break

        ax = axs.atbl.get('body')
        body = (axs.axkw.get('body') or {}) | (kwds.get('body') or {})

        if isinstance(ax, cmgeo.GeoAxes):
            if cj[0] in coords and cj[1] in coords:
                axs.set_geoticks(x=xco, y=yco, crs=body.get('crs'))
                args = body.get('features') or []
                axs.add_features(*args)
            else:
                raise ValueError("Physical coordinate not defined:"
                                 f"{cj[0]} {cj[1]}.")
        else:
            # pass
            axs.set_ticks(x=xco, y=yco)

        stat = {}
        contour = self._contour | (kwds.get('contour') or {})
        color = self._color | (kwds.get('color') or {})

        if isinstance(ax, cmgeo.GeoAxes):
            transf = body.get('transform')
            if transf:
                contour.setdefault('transform', transf)
                color.setdefault('transform', transf)

        if color['method'] == 'contour':
            col, cbr = self.color(fig, axs, data, stat, **color)
            con = self.contour(axs, data, **stat, **contour)
        else:
            con = self.contour(axs, data, stat, **contour)
            col, cbr = self.color(fig, axs, data, **stat, **color)

        # print(col, col.cmap)
        if cbr:
            for c in con:
                cbr.add_lines(c, erase=False)
        else:
            cbr = axs.colorbar(fig, con[0])
            for c in con[1:]:
                cbr.add_lines(c, erase=False)

        axs.add_titles(data, contours=con)
        axs.cbar_set_ticks()

        # Not yet working....
        if title:
            for a in ['set_window_title', 'setWindowTitle', ]:
                f = getattr(fig.canvas, a, None)
                if f:
                    f(f'Figure: {title}')
                    break

    def contour(self, axs, data, stat=None,
                levels=None, clabel=None, key=None, **kw):
        """matplotlib contour wrapper."""
        key = key or 'body'
        ax = axs.atbl.get(key)

        cc = []
        levels = levels or True
        if not isinstance(levels, cabc.Iterable):
            levels = [levels]
        elif isinstance(levels[0], nums.Number):
            if isinstance(levels[0], bool):
                pass
            else:
                levels = [levels]

        clabel = util.set_default(clabel, True)
        if clabel is True:
            clabel = slice(min(1, len(levels) - 1), None, None)
        if isinstance(clabel, slice):
            clabel = range(*clabel.indices(len(levels)))
        if isinstance(clabel, cabc.Iterable):
            _clabel = clabel
            # print(levels, list(_clabel))
            def check_clabel(idx):
                return bool(idx in _clabel)
            clabel = check_clabel
        if clabel is False:
            clabel = lambda idx: False
        if not isinstance(clabel, cabc.Callable):
            raise(f"invalid clabel argument {clabel}")

        def run(idx, **args):
            cp = data.plot.contour(ax=ax, **args,
                                     **(self.opts | kw))
            if clabel(idx):
                ax.clabel(cp)
            return cp

        for j, lev in enumerate(levels):
            if lev is True:
                c = run(j)
            elif lev is False:
                continue
            elif isinstance(lev, list):
                c = run(j, levels=lev)
            elif isinstance(lev, cabc.Callable):
                vmin, vmax = self.get_range(data, stat, **kw)
                c = run(j, levels=lev(vmin, vmax))
            else:
                raise TypeError(f"invalid level specifier {c}.")
            cc.append(c)
        return cc

    def color(self, fig, axs, data, stat=None,
              method=None, cmap=None, levels=None,
              key=None, **kw):
        """matplotlib contour wrapper."""
        key = key or 'body'
        ax = axs.atbl.get(key)
        # print(ax)
        if method is None:
            method = True
        if method is True:
            method = self.color_method
        if method is False:
            col, cbr = None, None
        else:
            # print(data.dims)
            func = getattr(data.plot, method, None)
            if func is None:
                raise ValueError(f"invalid method {method}.")
            # print(func)

            if levels in [True, None]:
                levels = None
            elif levels is False:
                pass
            elif isinstance(levels, list):
                pass
            elif isinstance(levels, cabc.Callable):
                vmin, vmax = self.get_range(data, stat, **kw)
                levels = levels(vmin, vmax)
            else:
                raise TypeError(f"invalid level specifier {levels}.")

            if levels is False:
                col, cbr = None, None
            else:
                col = func(ax=ax, add_colorbar=False,
                           # x=data.dims[-1], y=data.dims[0],
                           # transform=None,
                           levels=levels, cmap=cmap,
                           **(self.opts | kw))
                if method == 'contour' and len(col.levels) <= 1:
                    cbr = None
                else:
                    cbr = axs.colorbar(fig, col)
        return (col, cbr)

    def get_range(self, data, stat, vmin=None, vmax=None, **kw):
        stat = stat or {}
        vmin = vmin or stat.get('vmin')
        vmax = vmax or stat.get('vmax')
        if vmin is None:
            vmin = data.min().values
            stat['vmin'] = vmin
        if vmax is None:
            vmax = data.max().values
            stat['vmax'] = vmax
        return vmin, vmax

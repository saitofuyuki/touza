#!/usr/bin/env python3
# Time-stamp: <2024/11/05 22:15:44 fuyuki plot.py>

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
import pprint as ppr

import numpy as np

import matplotlib as mplib
import matplotlib.ticker as mtc
import matplotlib.pyplot as plt
import matplotlib.backend_tools as mpbt
import matplotlib.contour as mcnt

import xarray as xr
import xarray.plot.utils as xrpu

import cartopy.crs as ccrs
import cartopy.feature as cfeature
import cartopy.util as cutil
import cartopy.mpl.geoaxes as cmgeo
import cartopy.mpl.ticker as cmtick

import zbt.util as zu
import zbt.config as zcfg

_ConfigType = zcfg.ConfigRigid

# ### helper functions
def is_xr(obj):
    """Check if obj is DataArray instance"""
    return isinstance(obj, xr.DataArray)

# ### Picture

class PictureCore():
    """Abstract base layer of figure/axes creation for zbt."""
    def __init__(self, LayoutClass=None,
                 FigureClass=None, layout=None, **kwds):
        FigureClass = zu.set_default(FigureClass, FigureCore)
        LayoutClass = zu.set_default(LayoutClass, LayoutBase)

        self.fcls = FigureClass or None
        self.lcls = LayoutClass
        self.figkw = kwds
        self.figkw['layout'] = layout or 'none'

        if not issubclass(self.lcls, LayoutBase):
            raise ValueError(f"Invalid LayoutClass {self.lcls}.")

    def __call__(self, **kwds):
        fig = plt.figure(FigureClass=self.fcls, **self.figkw)
        axs = self.lcls(fig, **kwds)
        return fig, axs


class Picture(PictureCore):
    """User layer of figure/axes creation for zbt."""


# ### Figure

class FigureCore(mplib.figure.Figure):
    """Abstract base layer of matplotlib figure for zbt."""

    def __str__(self):
        return super().__str__()


# # ### Axes
# class EmptyAxes(mplib.axes.Axes):
#     """Simple axes without axis."""
#     def __init__(self, *args, **kwds):
#         super().__init__(*args, **kwds)
#         self.format_coord = lambda x, y: ''
#         self.set_axis_off()
#         self.add_artist(self.patch)


# ### Parser

class ParserBase():
    """Fallback attribute parser methods."""

    def parse_coor(self, coor, item=False):
        """Fallback coordinate limit and scale parser"""
        dmax = coor[-1]
        dmin = coor[0]
        scale = ''
        if item:
            dmax = dmax.item()
            dmin = dmin.item()
        return (dmin, dmax, scale)


class LegacyParser(ParserBase):
    """GTOOL3 legacy attribute parser methods."""

    def extract_titles(self, attrs):
        """Extract title properites from zbt-like data."""
        title = attrs.get('long_name', None)
        if title is None:
            title = zu.join_attrs(attrs, 'TITL', sep='', strip=True)
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
        ettl = zu.join_attrs(attrs, 'ETTL', sep='', strip=True)
        # ## hack
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
        styp = int(parse_sub('STYP') or 0)
        sc = ''
        # print(f"{dmin=} {dmax=}")
        # print(styp)
        if abs(styp) == 2:
            sc = 'log'
        if dmax and dmin:
            dmax = float(dmax)
            dmin = float(dmin)
            if dmin > dmax:
                dmin, dmax = dmax, dmin
            if sc == 'log' and dmin <= 0:
                vmin, vmax, _ = super().parse_coor(coor, item=item)
                dmin = min(vmin, vmax)
            if styp < 0:
                dmin, dmax = dmax, dmin
        else:
            vmin, vmax, _ = super().parse_coor(coor, item=item)
            dmax = float(dmax) if dmax else vmax
            dmin = float(dmin) if dmin else vmin
        # print(f"{dmin=} {dmax=}")
        # print(dmin, dmax, sc)
        return dmin, dmax, sc

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


# ### Layout

class LayoutBase(zcfg.ConfigBase):
    """Base layer of figure/axes manipulation."""
    names = ('layout', )
    _axes_keys = ()
    geometry = None
    gunit = 0.0

    def __init__(self, fig, figsize=None, **kwds):
        # _axes stores arguments to create axes.
        # Once created, it stores generated axes instance.
        self._axes = {}
        self._refuges = {}
        for lab, rect in self._iter_axes():
            axkw = kwds.get(lab) or {}
            self._axes[lab] = (dict(rect=rect, label=lab,) | axkw)
        # side effects (to set figure size)
        self.resize(fig, figsize=figsize, ref=self.geometry)
        self.orig_size = fig.get_size_inches()

    def _iter_axes(self):
        try:
            for grp in self._axes_keys:
                amap = getattr(self, grp)
                yield from amap.items()
        except AttributeError as exc:
            raise exc(f"No layout/axes map {grp} in {self}.")

    def tweak_axes(self, ax, lab):
        """Dummy axes modifier."""
        return ax

    def __str__(self):
        axes = ' '.join(str(key) for key in self._axes)
        name = self.names[-1]
        return f"<{name}> {{{axes}}}"

    def __getitem__(self, key):
        return self._axes[key]

    def __setitem__(self, key, val):
        self._axes[key] = val
        return self._axes[key]

    def __iter__(self):
        yield from self._axes.keys()

    def __len__(self):
        return len(self._axes)

    def get(self, key, default=None):
        return self._axes.get(key, default)

    def keys(self):
        yield from self._axes.keys()

    def values(self):
        yield from self._axes.values()

    def items(self):
        yield from self._axes.items()

    def _get_axes(self, key, default=None):
        """Return key if Axes, otherwise lookup internal dict."""
        if isinstance(key, mplib.axes.Axes):
            return key
        return self.get(key, default)

    # figure methods
    def _resize_calc(self, base, rate):
        if rate >= 0:
            return base * (1.0 + rate)
        return base / (1.0 - rate)

    def _resize_prop(self, org, cur, rate):
        if rate is None:
            return 0
        cur = self._resize_calc(cur, rate)
        r = cur / org
        if r >= 1.0:
            return r - 1.0
        return 1.0 - org / cur

    def resize(self, fig, figsize=None, ref=None):
        """Resize figure."""
        if ref is None:
            ref = fig.get_size_inches()
        elif ref is False:
            ref = self.orig_size
        elif ref is True:
            ref = self.geometry
        # print(f"in: {ref=} {figsize=}")

        figsize = zu.set_default(figsize, True)
        if figsize is True:
            figsize = ref
        elif not figsize:
            figsize = None
        elif isinstance(figsize, nums.Number):
            if ref is None:
                raise ValueError(f"geometry not set for {self}.")
            figsize = (self._resize_calc(ref[0], figsize),
                       self._resize_calc(ref[1], figsize), )
        elif isinstance(figsize, tuple):
            if ref is None:
                raise ValueError(f"geometry not set for {self}.")
            if len(figsize) == 1:
                figsize = figsize + (None, )
            figsize = (figsize[0] or ref[0],
                       figsize[1] or ref[1], )
        elif hasattr(figsize, 'get_size_inches'):
            figsize = figsize.get_size_inches()

        fig.set_size_inches(*figsize, forward=True)
        # print(f"out: {fig.get_size_inches()} {figsize=}")

    # plot methods
    def reset(self, fig, default=None, **kwds):
        """Reset or regenerate specific axes.
        kwds = {label: order}
           order == False          skip
           order == True           cla only
           order == None or dict   reset (possibly regenerate) axes
        """
        default = zu.set_default(default, True)
        for lab, rect in self._iter_axes():
            axkw = kwds.get(lab, default)
            if axkw is False:   # no touch
                continue
            ax_args = self.get(lab)
            if not isinstance(ax_args, mplib.axes.Axes):
                kpr = 'projection'
                oproj = ax_args.get(kpr)
                if axkw is True:
                    axkw = {}
                    nproj = oproj
                else:
                    nproj = axkw.get(kpr)
                    if nproj is True:
                        nproj = oproj
                    elif not nproj:
                        nproj = None
                axkw = ax_args | axkw
                axkw[kpr] = nproj
                if oproj and nproj is None:
                    print(f"warning: {kpr}={oproj} is cleared.")
                axkw.setdefault('label', lab)
                rect = axkw.pop('rect', rect)
                axkw.pop('fresh', None)
                ax = fig.add_axes(rect, **axkw)
                # print(f"[ADD]{lab}")
                # ppr.pprint(ax.findobj(match=mplib.spines.Spine))
                # ppr.pprint(ax.findobj())
            else:
                if axkw is True:
                    axkw = None
                axkw = dict(rect=rect) | (axkw or {})
                ax = self.scan_or_new(fig, lab, ax_args, **axkw)
            # if axkw is not True:
            #     ax = self.scan_or_new(fig, lab, ax_args, **(axkw or {}))
            # ax.cla()
            # print(f"[RESET]{lab}")
            # ppr.pprint(ax.findobj(match=mplib.spines.Spine))
            self[lab] = self.tweak_axes(ax, lab)
            # print(f"[TWEAK]{lab}")
            # ppr.pprint(ax.findobj(match=mplib.spines.Spine))

    def scan_or_new(self, fig, lab, ax, fresh=None,
                    projection=None, rect=None, **kwds):
        # print(f"scan: {lab=} {fresh=}")
        if fresh is True:
            ax.set_visible(False)
            ax = None
        if ax:
            op = getattr(ax, 'projection', None)
            # print(f"scan: {lab=} {str(op)[:10]} > {str(projection)[:10]}")
            if op != projection:
                self.store(lab, ax, projection=op, **kwds)
                ax = self.restore(lab, projection=projection, **kwds)
        if not ax:
            # print(f"new: {lab=}")
            ax = fig.add_axes(rect, label=lab, projection=projection, **kwds)
        # op = getattr(ax, 'projection', None)
        # print(f"ret: {ax=} {str(op)[:10]} {type(op)}")
        return ax

    def store(self, lab, ax, projection=None, **kwds):
        key = lab, projection
        # for k, v in self._refuges.items():
        #     k = str(k)[:20]
        #     print(f"stack {k} = {type(v)} {id(v)}")
        if key in self._refuges:
            if self._refuges[key] is ax:
                # print(f"leave {str(key)[:30]} {id(ax)}")
                pass
            else:
                raise ValueError(f"Duplicate refuges {key} .")
        else:
            self._refuges[key] = ax
            # print(f"push {str(key)[:30]} {id(ax)}")
        ax.set_visible(False)
        return ax

    def restore(self, lab, projection=None, **kwds):
        key = lab, projection
        ax = self._refuges.get(key, None)
        if ax:
            ax.set_visible(True)
        return ax

    def cla(self, axs=None):
        if axs is None:
            axs = self.keys()
        elif isinstance(axs, list):
            pass
        else:
            axs = [axs]
        for ax in axs:
            ax = self._get_axes(ax)
            if ax:
                ax.cla()
                lab = ax.get_label()
                # caution,  kept.
                _ = self.tweak_axes(ax, lab)

    def contour(self, data, *args,
                ax=None, artists=None, clabel=None, gid=None, **kwds):
        """matplotlib contour wrapper."""
        ax = self._get_axes(ax)
        artists = artists or []
        if ax:
            if is_xr(data):
                cp = data.plot.contour(ax=ax, *args, **kwds)
            else:
                cp = ax.contour(data, *args, **kwds)
            cp.set_gid(gid)
            artists.append(cp)
            if clabel:
                artists.extend(ax.clabel(cp))
        return artists

    def clabel(self, contour, ax=None, artists=None, gid=None, **kwds):
        ax = self._get_axes(ax)
        artists = artists or []
        if ax:
            for la in ax.clabel(contour, **kwds):
                la.set_gid(gid)
                artists.append(la)
        return artists

    def color(self, data, *args,
              ax=None, artists=None,
              method=None, gid=None, **kwds):
        """matplotlib contour wrapper."""
        ax = self._get_axes(ax)
        artists = artists or []
        if ax:
            if is_xr(data):
                cp = method(ax=ax, *args, **kwds)
            else:
                cp = method(data, *args, **kwds)
            cp.set_gid(gid)
            artists.append(cp)
        return artists

    def colorbar(self, fig, *args,
                 cax=None, artists=None, **kwds):
        """Wrap figure.colorbar()"""
        cax = self._get_axes(cax)
        artists = artists or []
        # artists.append(fig.colorbar(*args, cax=cax, **kwds))
        bar = fig.colorbar(*args, cax=cax, **kwds)
        artists.extend([bar, cax, ])
        return artists

    def set_view(self, data, ax=None, artists=None,
                 x=None, y=None, **kwds):
        artists = artists or []
        ax = self._get_axes(ax)
        if isinstance(ax, cmgeo.GeoAxes):
            artists = self.set_geo_view(data, ax=ax, artists=artists,
                                        x=x, y=y, **kwds)
        elif ax:
            co = [d for d in data.dims if data.coords[d].size > 1]
            if len(co) != 2:
                raise ValueError(f"Not 2d shape={co}")
            x = x or co[-1]
            y = y or co[0]

            xc = data.coords[x]
            yc = data.coords[y]

            self._set_tick(xc, ax.xaxis, ax.set_xlim, ax.set_xscale,
                           **(kwds.get(x) or {}))
            self._set_tick(yc, ax.yaxis, ax.set_ylim, ax.set_yscale,
                           **(kwds.get(y) or {}))

            artists.extend([ax.xaxis, ax.yaxis,
                            ax.xaxis.get_offset_text(),
                            ax.yaxis.get_offset_text()])

            # ax.xaxis.get_offset_text().set_fontsize(24)
            # ax.yaxis.get_offset_text().set_fontsize(24)
            # print('xaxis:', ax.xaxis.get_offset_text())
            # print('yaxis:', ax.yaxis.get_offset_text())
            return artists

    def _set_tick(self, co, axis, limf, scf,
                  major=None, minor=None, dmin=None, dmax=None,
                  **kwds):
        cmin, cmax, scale = self.parse_coor(co)
        dmin = zu.set_default(dmin, cmin)
        dmax = zu.set_default(dmax, cmax)
        span = abs(dmax - dmin)
        vmaj, vmin = self.parse_ticks(co.attrs)
        if span > vmaj > 0:
            axis.set_major_locator(mtc.MultipleLocator(vmaj))
        if vmin > 0:
            axis.set_minor_locator(mtc.MultipleLocator(vmin))

        txt = xrpu.label_from_attrs(co)
        ltx = axis.set_label_text(txt, fontsize=13)
        ltx.set_picker(True)
        ltx.set_gid(f"coordinate:{co.name}")
        axis.set_picker(True)
        # axis.set_picker(self.axis_picker)
        axis.set_gid(f"tick:{co.name}")
        axis.set_tick_params(which='major', **(major or {}))
        axis.set_tick_params(which='minor', **(minor or {}))
        if dmin != dmax:
            limf(dmin, dmax)
        if scale:
            scf(scale)

    def set_geo_view(self, data, ax=None, artists=None,
                     x=None, y=None, crs=None, **kwds):
        artists = artists or []
        if not ax:
            return artists

        co = [d for d in data.dims if data.coords[d].size > 1]
        if len(co) != 2:
            raise ValueError(f"Not 2d shape={co}")
        x = x or co[-1]
        y = y or co[0]

        xc = data.coords[x]
        yc = data.coords[y]

        view_x = kwds.get(x) or {}
        view_y = kwds.get(y) or {}

        if x in data.coords and y in data.coords:
            try:
                self.set_lon_view(ax, xc, crs, **view_x)
                self.set_lat_view(ax, yc, crs, **view_y)
            except RuntimeError as err:
                print(err)
                gl = ax.gridlines(draw_labels=True)
                gl.top_labels = False
                gl.right_labels = False
                # gl.xlabel_style = dict(size=20)
                # gl.ylabel_style = dict(size=20)
        else:
            raise ValueError("Physical coordinate not defined:"
                             f"{x} {y}")

        x0, x1, sx = self.parse_coor(xc)
        y0, y1, sy = self.parse_coor(yc)

        x0 = zu.set_default(view_x.get('dmin'), x0)
        x1 = zu.set_default(view_x.get('dmax'), x1)
        y0 = zu.set_default(view_y.get('dmin'), y0)
        y1 = zu.set_default(view_y.get('dmax'), y1)

        try:
            if abs(x1 - x0) > 359 and abs(y1 - y0) > 179:
                raise ValueError('')
            if abs(x1 - x0) >= 360:
                if x1 > x0:
                    x1 = x1 + 0.01
                else:
                    x0 = x0 + 0.01
            # print(x0, x1, y0, y1)
            ax.set_extent((x0, x1, y0, y1), crs=crs)
        except ValueError as err:
            if str(err):
                print(err)
            ax.set_global()
        artists.extend([ax.xaxis, ax.yaxis])
        return artists

    def set_lon_view(self, ax, lon, crs,
                     major=None, minor=None, dmin=None, dmax=None,
                     **kwds):
        x0, x1, sc = self.parse_coor(lon, item=True)
        span = abs(x1 - x0)
        vmaj, vmin = self.parse_ticks(lon.attrs)
        if span > vmaj > 0:
            loc = mtc.MultipleLocator(vmaj)
        else:
            loc = cmtick.LongitudeLocator()
        loc = [lo for lo in loc.tick_values(x0, x1)
               if x0 <= lo <= x1]
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
        ax.xaxis.set_tick_params(which='major', **(major or {}))
        ax.xaxis.set_tick_params(which='minor', **(minor or {}))

    def set_lat_view(self, ax, lat, crs,
                     major=None, minor=None, dmin=None, dmax=None,
                     **kwds):
        x0, x1, sc = self.parse_coor(lat, item=True)
        span = abs(x1 - x0)
        vmaj, vmin = self.parse_ticks(lat.attrs)
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
        ax.yaxis.set_tick_params(which='major', **(major or {}))
        ax.yaxis.set_tick_params(which='minor', **(minor or {}))

    def add_features(self, /, *features,
                     ax=None, artists=None, gid=None, **kwds):
        """Set GeoAxes extent and tick labels."""
        ax = self._get_axes(ax)
        artists = artists or []
        if ax:
            for ft in features:
                if isinstance(ft, str):
                    ft = getattr(cfeature, ft)
                if ft:
                    artists.append(ax.add_feature(ft))
        return artists

    def add_titles(self, *args,
                   ax=None, artists=None, **kwds):
        """Add title."""
        ax = self._get_axes(ax)
        artists = artists or []
        if ax:
            lab = ax.get_label()
            # kwds = self.prop(lab, {}) | kwds
            artists.append(ax.text(*args, **kwds))
        return artists

    def toggle_axis(self, which, ax=None):
        ax = self._get_axes(ax)
        if ax:
            if which.lower() in ['x', 'h', ]:
                f = ax.xaxis
            else:
                f = ax.yaxis
            f.set_inverted(not f.get_inverted())
        return ax

    def toggle_visible(self, b=None, ax=None):
        ax = self._get_axes(ax)
        if ax:
            if b is None:
                b = not ax.get_visible()
            ax.set_visible(b)
        return ax

    def set_visible(self, b, ax=None):
        ax = self._get_axes(ax)
        if ax:
            ax.set_visible(b)
        return ax

    def findobj(self, *args, ax=None, **kwds):
        ax = self._get_axes(ax)
        if ax:
            return ax.findobj(*args, **kwds)
        return None


class LayoutLegacy3(LayoutBase, LegacyParser, _ConfigType):
    """Emulate GTOOL3/gtcont layout 3."""

    _config = False

    names = ('Legacy3', )    # no dependency

    geometry = (10.45, 7.39)
    gunit = 1.0 / 128

    _pos = {'bottom': True, 'top': True, 'right': True, 'left': True, }
    _major = {'length': 10.8, 'width': 1.0, 'pad': 5.8, 'labelsize': 14.0, }
    _minor = {'length': 5.4, 'width': 0.7, }
    _major.update(_pos)
    _minor.update(_pos)

    body = {'major': _major, 'minor': _minor, }
    axis_text = {'fontsize': 14.0, }

    colorbar_ = {'major': {'bottom': True, 'length': 10.8,
                           'width': 1.0, 'pad': 5.8, 'labelsize': 14.0, },
                 'minor': False, }

    _title_text = {'linespacing': 1.3, 'fontsize': 14.0, }
    _contour_text = {'fontsize': 12.0, }

    left_title_ = {'title': True, } | _title_text
    right_title_ = {'title': True, } | _title_text
    info_ = {'title': ['contour', ] }
    info_ |=  _contour_text

    _ofx, _ofy = 0.2, 0.02
    _lb, _wb = 0.129, 0.830
    _bt, _ht = 0.81, 0.125

    graph = {'body': (_lb, 0.225, _wb, 0.585),
             'colorbar': (0.061, 0.070, 0.245, 0.057), }

    text = {'info': (_lb + _ofx, _ofy, _wb - _ofx * 2, 0.1042 - _ofy),
            'left_title': (_lb, _bt, _wb / 2, _ht),
            'right_title': (_lb + _wb / 2, _bt, _wb / 2, _ht), }

    _axes_keys = LayoutBase._axes_keys + ('graph', 'text', )

    def tweak_axes(self, ax, lab):
        """Tweaking of axes at creation."""
        attr = self.prop(lab, {})
        gid = attr.setdefault('gid', lab)
        ax.set_gid(gid)
        if lab not in self.prop('graph'):
            ax.format_coord = lambda x, y: ''
            ax.set_axis_off()
            ax.add_artist(ax.patch)
            if lab.endswith('_title'):
                ax.set(zorder=-1)
        if lab in self.prop('graph'):
            if lab == 'body':
                for axis in [ax.xaxis, ax.yaxis]:
                    axis.label.set_gid(gid)
                    axis.label.set(**self.prop('axis_text', {}))
                    for m in ['major', 'minor']:
                        a = attr.get(m) or {}
                        axis.set_tick_params(which=m, **a)
                    axis.set_gid(gid)
            elif lab == 'colorbar':
                for m in ['major', 'minor']:
                    ax.tick_params(which=m, **(attr.get(m) or {}))
        return ax

    def which(self, func, ax=None):
        ax = ax or 'body'
        return self._get_axes(ax)

    def contour(self, *args, ax=None, **kwds):
        """Wrapper to call ax.contour()"""
        ax = ax or 'body'
        return super().contour(*args, ax=ax, **kwds)

    def clabel(self, *args, ax=None, **kwds):
        ax = ax or 'body'
        return super().clabel(*args, ax=ax, **kwds)

    def color(self, *args, ax=None, **kwds):
        """Wrapper to call ax.color()"""
        ax = ax or 'body'
        return super().color(*args, ax=ax, **kwds)

    def toggle_axis(self, *args, ax=None, **kwds):
        """Toggle axis direction."""
        ax = ax or 'body'
        return super().toggle_axis(*args, ax=ax, **kwds)

    def toggle_visible(self, *args, ax=None, **kwds):
        ax = ax or 'body'
        return super().toggle_visible(*args, ax=ax, **kwds)

    def colorbar(self, fig, *args, cax=None, orientation=None, **kwds):
        """Wrap colorbar"""
        cax = cax or 'colorbar'
        orientation = zu.set_default(orientation, 'horizontal')
        return super().colorbar(fig, *args, cax=cax,
                                orientation=orientation, **kwds)

    def findobj(self, *args, ax=None, **kwds):
        ax = self._get_axes(ax)
        ax = ax or 'body'
        return super().findobj(*args, ax=ax, **kwds)

    def set_view(self, *args, ax=None, **kwds):
        ax = ax or 'body'
        return super().set_view(*args, ax=ax, **kwds)

    def add_features(self, /, *args, ax=None, **kwds):
        ax = ax or 'body'
        return super().add_features(*args, ax=ax, **kwds)

    def add_titles(self, data, aux=None,
                   artists=None,
                   info=None, left_title=None, right_title=None, **kwds):
        """Batch procedures to add several text."""
        artists = artists or []

        a = self.left_title(data, **(left_title or {}))
        artists.extend(a)
        a = self.right_title(data, **(right_title or {}))
        artists.extend(a)
        a = self.info(data, aux=aux, **(info or {}))
        artists.extend(a)

        return artists

    def info(self, data, ax=None, aux=None,
             artists=None,
             horizontalalignment=None, verticalalignment=None,
             **kwds):
        """Proccess info axes."""
        artists = artists or []
        ax = ax or 'info'
        kwds = self.prop(ax, {}) | kwds
        kwds.setdefault('gid', ax)
        title = kwds.pop('title')
        title = zu.set_default(title, True)
        if title is True:
            title = aux or []
        aux = aux or {}
        text = []
        for sk in title:
            if sk not in aux:
                continue
            sv = aux.get(sk)
            if sk == 'contour':
                dc = sv[0].levels[1:] - sv[0].levels[:-1]
                mi, ma = min(dc), max(dc)
                if mi == ma:
                    text.append(f'CONTOUR INTERVAL = {mi}')
                elif (ma - mi) < max(abs(ma), abs(mi)) * 1.e-3:
                    text.append(f'CONTOUR INTERVAL = ~{mi}')
                else:
                    levs = ','.join(str(l) for l in sv[0].levels)
                    text.append(f'CONTOUR = {levs}')
            else:
                continue
        if text:
            text = '\n'.join(str(s) for s in text)
            ha = zu.set_default(horizontalalignment, 'center')
            va = zu.set_default(verticalalignment, 'top')
            a = super().add_titles(0.5, 1.0, text, ax=ax,
                                   horizontalalignment=ha,
                                   verticalalignment=va, **kwds)
            artists.extend(a)
        return artists

    def left_title(self, data, ax=None,
                   artists=None,
                   horizontalalignment=None, verticalalignment=None,
                   **kwds):
        """Proccess left_title axes."""
        artists = artists or []
        ax = ax or 'left_title'
        kwds = self.prop(ax, {}) | kwds
        kwds.setdefault('gid', ax)
        title = kwds.pop('title')
        title = zu.set_default(title, True)
        if title is True:
            title = self.parse_titles(data.attrs, data.coords, default='')
        if isinstance(title, (list, tuple)):
            title = '\n'.join(str(s) for s in title)
        if title:
            ha = zu.set_default(horizontalalignment, 'left')
            va = zu.set_default(verticalalignment, 'top')
            a = super().add_titles(0, 1.0, title, ax=ax,
                                   horizontalalignment=ha,
                                   verticalalignment=va, **kwds)
            artists.extend(a)
        return artists

    def right_title(self, data, ax=None,
                    artists=None,
                    horizontalalignment=None, verticalalignment=None,
                    **kwds):
        """Proccess right_title axes."""
        artists = artists or []
        ax = ax or 'right_title'
        kwds = self.prop(ax, {}) | kwds
        kwds.setdefault('gid', ax)
        title = kwds.pop('title')
        title = zu.set_default(title, True)
        if title is True:
            dset = self.parse_dataset(data.attrs, default='')
            cal = self.parse_calendar(data.attrs, default='')
            title = (dset, cal, '')
        if isinstance(title, (list, tuple)):
            title = '\n'.join(str(s) for s in title)
        if title:
            ha = zu.set_default(horizontalalignment, 'right')
            va = zu.set_default(verticalalignment, 'top')
            a = super().add_titles(1.0, 1.0, title, ax=ax,
                                   horizontalalignment=ha,
                                   verticalalignment=va, **kwds)
            artists.extend(a)
        return artists


# ## Plot ############################################################
class PlotBase(zcfg.ConfigBase):
    names = ('plot', )
    pass


class ContourPlot(PlotBase, _ConfigType):
    """Contour with fill-color."""
    names = PlotBase.names + ('contour', )

    _opts = {'add_labels': False, }

    method_table = {'f': 'contourf', 'c': 'contour',
                    'p': 'pcolormesh', 'i': 'imshow',
                    's': 'surface', }
    color_ = {'method': 'imshow', } | _opts
    contour_ = {'colors': 'black', } | _opts

    def __init__(self, contour=None, color=None):
        self._contour = self.prop('contour') | (contour or {})
        self._color = self.prop('color') | (color or {})
        self._contour.setdefault('gid', 'contour')
        self._color.setdefault('gid', 'color')

    def __call__(self, fig, axs, data, view=None,
                 artists=None, **kwds):
        """Batch plotter."""
        if any(w <= 1 for w in data.shape):
            raise UserWarning("virtually less than two dimensions")

        artists = artists or []

        # ppr.pprint(axs.findobj(ax='body', match=mplib.spines.Spine))

        coords = data.coords
        cj = [d for d in data.dims if coords[d].size > 1]
        if len(cj) != 2:
            raise ValueError(f"Not 2d shape={cj}")
        xco = coords[cj[1]]
        yco = coords[cj[0]]

        contour = self._contour | (kwds.get('contour') or {})
        color = self._color | (kwds.get('color') or {})

        # if isinstance(ax, cmgeo.GeoAxes):
        body = kwds.get('body') or {}
        transf = body.get('transform')
        if transf:
            contour.setdefault('transform', transf)
            color.setdefault('transform', transf)

        con = self.contour(axs, data, **contour)
        col = self.color(axs, data, **color)
        bar, cax, = self.colorbar(fig, axs, con, col,
                                  contour=contour, color=color)
        # print(bar)

        artists.extend(con)
        artists.extend(col)
        artists.append(cax)
        # # Colorbar object does not work with ArtistAnimation, with
        # # the error:
        # #  AttributeError: 'Colorbar' object has no attribute 'set_visible'
        if False:
            artists.append(bar)

        aux = dict(contour=con)
        ttl = axs.add_titles(data, aux=aux, **kwds)
        artists.extend(ttl)

        artists = self.set_view(axs, data, artists=artists,
                                crs=body.get('crs'),
                                **(view or {}))

        fts = body.get('features') or []
        fts = axs.add_features(*fts)

        return artists

        if color['method'] == 'contour':
            col, cbr = self.color(fig, axs, data, stat, **color)
            con = self.contour(axs, data, **stat, **contour)
        else:
            con = self.contour(axs, data, stat, **contour)
            col, cbr = self.color(fig, axs, data, **stat, **color)

        return
        # print(col, col.cmap)
        if cbr:
            for c in con:
                cbr.add_lines(c, erase=False)
        else:
            cbr = axs.colorbar(fig, con[0])
            for c in con[1:]:
                cbr.add_lines(c, erase=False)

        axs.add_titles(data, contours=con, **kwds)
        axs.cbar_set_ticks()

        view = view or {}
        vx = view.get(cj[1]) or {}
        vy = view.get(cj[0]) or {}
        # print(vx, vy)
        # print(ax)
        if isinstance(ax, cmgeo.GeoAxes):
            if cj[0] in coords and cj[1] in coords:
                axs.set_geoticks(x=xco, y=yco, view_x=vx, view_y=vy,
                                 crs=body.get('crs'))
                args = body.get('features') or []
                axs.add_features(*args)
            else:
                raise ValueError("Physical coordinate not defined:"
                                 f"{cj[0]} {cj[1]}.")
        else:
            axs.set_ticks(x=xco, y=yco, view_x=vx, view_y=vy)

        # Not yet working....
        if title:
            for a in ['set_window_title', 'setWindowTitle', ]:
                f = getattr(fig.canvas, a, None)
                if f:
                    f(f'Figure: {title}')
                    break
        return col

    def annotation(self, clabel, levels):
        """Create helper function to decide whether contours
        should be annotated."""
        labels = zu.set_default(clabel, True)

        if labels is True:
            # labels = slice(min(1, len(levels) - 1), None, None)
            labels = slice(-1, None, None)
        if isinstance(labels, slice):
            labels = range(*labels.indices(len(levels)))

        if isinstance(labels, cabc.Iterable):
            def check_clabel(idx):
                return bool(idx in labels)
            clabel = check_clabel
        elif labels is False:
            clabel = lambda idx: False
        elif not isinstance(labels, cabc.Callable):
            raise(f"invalid clabel argument {labels}")

        return clabel

    def nml_levels(self, levels):
        """Normalize levels argument."""
        levels = levels or True
        if not isinstance(levels, cabc.Iterable):
            levels = [levels]
        elif isinstance(levels[0], nums.Number):
            if isinstance(levels[0], bool):
                pass
            else:
                levels = [levels]
        return levels

    def set_view(self, axs, data,
                 key=None, artists=None, **kwds):
        artists = artists or []
        vx = axs.set_view(data, ax=key, **kwds)
        artists.extend(vx or [])
        return artists

    def contour(self, axs, data,
                levels=None, clabel=None, key=None,
                artists=None, **kwds):
        """matplotlib contour wrapper."""
        artists = artists or []
        levels = self.nml_levels(levels)
        clabel = self.annotation(clabel, levels)

        def run(**rkw):
            cp = axs.contour(data, ax=key,
                             clabel=False, **rkw, **kwds)
            return cp

        alab = []
        for j, lev in enumerate(levels):
            if lev is True:
                c = run()
            elif lev is False:
                continue
            elif isinstance(lev, list):
                c = run(levels=lev)
            elif isinstance(lev, cabc.Callable):
                vmin, vmax = self.get_range(data)
                c = run(levels=lev(vmin, vmax))
            else:
                raise TypeError(f"invalid level specifier {c}.")
            artists.extend(c)
            if clabel(j):
                alab = axs.clabel(*c, ax=key, artists=alab, gid='clabel')
        return artists + alab

    def check_method(self, axs, data, method, key=None):
        """Detect color method."""
        if method is None:
            method = True
        if method is True:
            method = self._color.get('method')
        if method is None:
            method = True
        if method is True:
            method = 'imshow'

        if method is False:
            return method

        if is_xr(data):
            func = getattr(data.plot, method, None)
        else:
            ax = axs.which('color', key)
            func = getattr(ax, method, None)
        if func:
            return func
        else:
            raise ValueError(f"invalid method {method}.")

    def color(self, axs, data,
              method=None, cmap=None, levels=None,
              key=None, artists=None, **kwds):
        """matplotlib contour wrapper."""
        artists = artists or []

        method = self.check_method(axs, data, method, key)

        if method is False:
            pass
        else:
            if levels in [True, None]:
                levels = None
            elif levels is False:
                pass
            elif isinstance(levels, list):
                pass
            elif isinstance(levels, cabc.Callable):
                vmin, vmax = self.get_range(data)
                levels = levels(vmin, vmax)
            else:
                raise TypeError(f"invalid level specifier {levels}.")

            if levels is False:
                pass
            else:
                if isinstance(cmap, cabc.Callable):
                    cmap = cmap()

                col = axs.color(data, ax=key,
                                method=method,
                                add_colorbar=False,
                                levels=levels, cmap=cmap,
                                **kwds)
                artists.extend(col)
        return artists
        # if method is None:
        #     method = True
        # if method is True:
        #     method = self.color_method
        # if method is False:
        #     col, cbr = None, None
        # else:
        #     # print(data.dims)
        #     func = getattr(data.plot, method, None)
        #     if func is None:
        #         raise ValueError(f"invalid method {method}.")
        #     # print(func)

        #     if levels in [True, None]:
        #         levels = None
        #     elif levels is False:
        #         pass
        #     elif isinstance(levels, list):
        #         pass
        #     elif isinstance(levels, cabc.Callable):
        #         vmin, vmax = self.get_range(data, stat, **kwds)
        #         levels = levels(vmin, vmax)
        #     else:
        #         raise TypeError(f"invalid level specifier {levels}.")

        #     if levels is False:
        #         col, cbr = None, None
        #     else:
        #         if isinstance(cmap, cabc.Callable):
        #             cmap = cmap()
        #         col = func(ax=ax, add_colorbar=False,
        #                    # x=data.dims[-1], y=data.dims[0],
        #                    # transform=None,
        #                    levels=levels, cmap=cmap,
        #                    **(self.opts | kwds))
        #         if method == 'contour' and len(col.levels) <= 1:
        #             cbr = None
        #         else:
        #             cbr = axs.colorbar(fig, col)
        # return (col, cbr)

    def colorbar(self, fig, axs, cons=None, cols=None,
                 contour=None, color=None,
                 key=None, artists=None, **kwds):
        artists = artists or []
        cons = cons or [None]
        cols = cols or [None]
        contour = contour or {}
        color = color or {}

        method = color.get('method')

        bar = None
        ticks = []
        # print(method, cols[0])
        if method == 'contour':
            ticks = cols[0].levels
        else:
            bar, cax, = axs.colorbar(fig, cols[0],
                                     alpha=color.get('alpha'))

        for cs in cons:
            if isinstance(cs, mcnt.QuadContourSet):
                if bar:
                    bar.add_lines(cs, erase=False)
                else:
                    if len(ticks) > len(cs.levels):
                        ticks = cs.levels
                    m = len(ticks)
                    if m > 5:
                        ticks = ticks[::m//5]
                    bar, cax, = axs.colorbar(fig, cs, ticks=ticks)

        if method == 'contour':
            for cs in cols:
                if isinstance(cs, mcnt.QuadContourSet) \
                   and len(cs.levels) > 1:
                    if bar:
                        bar.add_lines(cs, erase=False)
                    else:
                        bar, cax, = axs.colorbar(fig, cs)
        artists.extend([bar, cax, ])
        return artists

    def get_range(self, data):
        """Data range computation."""
        vmin = data.attrs.get('vmin')
        vmax = data.attrs.get('vmax')

        if vmin is None:
            vmin = data.min().values
        if vmax is None:
            vmax = data.max().values
        return vmin, vmax


def main(*args):
    """Test driver."""
    import tomllib as toml

    config = {}

    cfgf = 'zbt/zbt/zbtrc.toml'
    with open(cfgf, "rb") as fp:
        c = toml.load(fp)
        config.update(c)
    # ppr.pprint(config)

    LayoutTest = LayoutLegacy3

    LayoutTest.config(config, groups=('zbcont', 'test', ),
                      verbose=True)
    LayoutTest.diag(strip=False)

    # fig = plt.figure(FigureClass=FigureCore)
    # lay = LayoutTest(fig)

    Pic = Picture(LayoutClass=LayoutTest)
    fig1, lay1 = Pic()
    fig2, lay2 = Pic()
    lay1['body'].plot([0, 1, 2, 3], [3, 2, 1, 0])
    lay2['body'].plot([0, 1, 2, 3], [0, 3, 1, 2])


    ContourPlot.config(config, groups=('zbcont', 'test', ),
                       verbose=True)

    ContourPlot.diag(strip=False)

    plt.show()

    pass


if __name__ == '__main__':
    import sys
    main(sys.argv[1:])

########################################################################

# from . import util


# class FigureCore(mplib.figure.Figure):
#     """Abstract base layer of matplotlib figure for zbt."""

#     def __init__(self, *args, **kwds):
#         """Initialize figure+axes complex."""
#         super().__init__(*args, **kwds)
#         self._map_axes = {}

#     def add_axes(self, *args, key=None, label=None, **kwds):
#         """Register axes into internal dict using key or label."""
#         axes = super().add_axes(*args, label=label, **kwds)
#         if key is None:
#             key = label
#         if key is None:
#             key = len(self._map_axes)
#         if key in self._map_axes:
#             raise ValueError(f"Duplicate axes-key {key}.")
#         self._map_axes[key] = axes
#         return axes

#     def __str__(self):
#         head = super().__str__()
#         this = ' '.join(str(key) for key in self._map_axes)
#         return f"{head} {{{this}}}"

#     def __getitem__(self, key):
#         return self._map_axes[key]

#     def __iter__(self):
#         yield from self._map_axes.keys()

#     #### introduction of len() cause a problem.
#     # def __len__(self):
#     #     return len(self._axes)

#     def get(self, key, default=None):
#         return self._map_axes.get(key, default)

#     def keys(self):
#         yield from self._map_axes.keys()

#     def values(self):
#         yield from self._map_axes.values()

#     def items(self):
#         yield from self._map_axes.items()

#     # def _get_axes(self, key, default=None):
#     #     """Return key if Axes, otherwise lookup internal dict."""
#     #     if isinstance(key, mplib.axes.Axes):
#     #         return key
#     #     return self.get(key, default)


# # class PictureBase():
# #     """Base layer of figure/axes manipulation."""

# #     def __init__(self, figsize, layout=None, cls=None, **kwds):
# #         self.figsize = figsize
# #         self.layout = layout or "none"
# #         self.figkw = kwds

# #         cls = FigureCore if cls is None else cls
# #         self.cls = cls or None

# #     def __call__(self, **kwds):
# #         """Create figure and axes."""

# #         fig = plt.figure(figsize=self.figsize,
# #                          layout=self.layout, FigureClass=self.cls,
# #                          **self.figkw)
# #         if issubclass(seflf.cls, FigureCore):
# #             axes = fig   # FigureCore instance itself acts as an axes map
# #         else:
# #             axes = {}
# #         return fig, axes


# class PictureBase():
#     """Base layer of figure/axes manipulation."""

#     def __init__(self, cls=None, **kwds):
#         self.figsize = figsize
#         self.layout = layout or "none"
#         self.figkw = kwds

#         cls = FigureCore if cls is None else cls
#         self.cls = cls or None

#     def __call__(self, **kwds):
#         """Create figure and axes."""

#         fig = plt.figure(figsize=self.figsize,
#                          layout=self.layout, FigureClass=self.cls,
#                          **self.figkw)
#         if issubclass(self.cls, FigureCore):
#             axes = fig   # FigureCore instance itself acts as an axes map
#         else:
#             axes = {}
#         return fig, axes


# class TestTools(mpbt.ToolBase):
#     """Test."""
#     default_keymap = None
#     description = 'Test'


# class LayoutBase():
#     """Base layer of figure layout.
#     Contains three matplotlib axes.
#     """

#     def __init__(self, figsize, layout=None, cls=None, **kwds):
#         self.figsize = figsize
#         self.enlarge = 0
#         self.layout = layout or "none"
#         self.figkw = kwds

#         # set cls=False to clear cls
#         cls = FigureCore if cls is None else cls
#         self.cls = cls or None

#         self.axkw = {}
#         self.atbl = {}

#     def __call__(self, axs=None,
#                  figsize=None, layout=None, FigureClass=None,
#                  **kwds):
#         """Create figure and axes."""
#         figsize = figsize or self.figsize
#         layout = layout or self.layout
#         FigureClass = FigureClass or self.cls

#         fig = plt.figure(figsize=figsize,
#                          layout=layout, FigureClass=FigureClass,
#                          **self.figkw)
#         if self.figsize is None:
#             self.figsize = fig.get_size_inches()
#         self.enlarge = 0.0
#         # dummy
#         axs = axs or self
#         self.axkw = kwds
#         return fig, axs

#     def reset(self, fig, axs=None, **kwds):
#         """Dummy procedure to reset figure."""

#     def _resize_calc(self, base, rate):
#         if rate >= 0:
#             return base * (1 + rate)
#         return base / (1 - rate)

#     def _resize_prop(self, org, cur, rate):
#         if rate is None:
#             return 0
#         cur = self._resize_calc(cur, rate)
#         r = cur / org
#         if r >= 1:
#             return r - 1
#         return 1 - org / cur

#     def resize(self, fig, axs=None, rate=None):
#         """Resize figure."""
#         geo = fig.get_size_inches()
#         self.enlarge = self._resize_prop(self.figsize[0], geo[0], rate)
#         geo = self._resize_calc(np.array(self.figsize), self.enlarge)
#         fig.set_size_inches(geo)

#     def parse_coor(self, coor, item=False):
#         """Fallback coordinate limit and scale parser"""
#         dmax = coor[-1]
#         dmin = coor[0]
#         scale = ''
#         if item:
#             dmax = dmax.item()
#             dmin = dmin.item()
#         return (dmin, dmax, scale)

#     def parse_ticks(self, _, default=0):
#         """Fall back major and minor locators."""
#         major = default
#         minor = default
#         return (major, minor)

#     def set_ticks(self, x, y, view_x=None, view_y=None,
#                   axs=None, major=None, minor=None, key=None):
#         """Set tick labels."""
#         axs = axs or self
#         key = key or 'body'
#         ax = axs.atbl.get(key, None)
#         view_x = view_x or {}
#         view_y = view_y or {}

#         self._set_tick(x, ax.xaxis, ax.set_xlim, ax.set_xscale,
#                        major=major, minor=minor, **view_x)
#         self._set_tick(y, ax.yaxis, ax.set_ylim, ax.set_yscale,
#                        major=major, minor=minor, **view_y)

#     def _set_tick(self, co, axis, limf, scf,
#                   major=None, minor=None, dmin=None, dmax=None,
#                   **kwds):
#         major = major or {}
#         minor = minor or {}
#         cmin, cmax, scale = self.parse_coor(co)
#         dmin = util.set_default(dmin, cmin)
#         dmax = util.set_default(dmax, cmax)
#         span = abs(dmax - dmin)
#         vmaj, vmin = self.parse_ticks(co.attrs)
#         if span > vmaj > 0:
#             axis.set_major_locator(mtc.MultipleLocator(vmaj))
#         if vmin > 0:
#             axis.set_minor_locator(mtc.MultipleLocator(vmin))
#         txt = xrpu.label_from_attrs(co)
#         ltx = axis.set_label_text(txt, fontsize=13)
#         ltx.set_picker(True)
#         ltx.set_gid(f"coordinate:{co.name}")
#         axis.set_picker(True)
#         # axis.set_picker(self.axis_picker)
#         axis.set_gid(f"tick:{co.name}")
#         axis.set_tick_params(which='major', **major)
#         axis.set_tick_params(which='minor', **minor)
#         if dmin != dmax:
#             limf(dmin, dmax)
#         if scale:
#             scf(scale)

#     def axis_picker(self, artist, mouseevent):
#         props = {}
#         bbox = artist.get_tightbbox()
#         x, y = mouseevent.x, mouseevent.y
#         hit = bbox.x0 < x < bbox.x1 and bbox.y0 < y < bbox.y1
#         if hit:
#             print(artist.get_children())
#         return hit, props

#     def set_geoticks(self, x, y, view_x=None, view_y=None,
#                      axs=None, major=None, minor=None, crs=None, key=None):
#         """Set GeoAxes extent and tick labels."""
#         axs = axs or self
#         key = key or 'body'
#         ax = axs.atbl.get(key, None)
#         view_x = view_x or {}
#         view_y = view_y or {}

#         lon = x
#         lat = y

#         x0, x1, sx = self.parse_coor(lon)
#         y0, y1, sy = self.parse_coor(lat)

#         x0 = util.set_default(view_x.get('dmin'), x0)
#         x1 = util.set_default(view_x.get('dmax'), x1)
#         y0 = util.set_default(view_y.get('dmin'), y0)
#         y1 = util.set_default(view_y.get('dmax'), y1)
#         # print(x0, x1, y0, y1)
#         # try:
#         #     if abs(x1 - x0) > 359 and abs(y1 - y0) > 179:
#         #         # raise ValueError(f"Force to set global {x0}:{x1}")
#         #         # raise ValueError(f"Force to set global.")
#         #         raise ValueError('')
#         #     if abs(x1 - x0) >= 360:
#         #         if x1 > x0:
#         #             x1 = x1 + 0.01
#         #         else:
#         #             x0 = x0 + 0.01
#         #     ax.set_extent((x0, x1, y0, y1), crs=crs)
#         # except ValueError as err:
#         #     if str(err):
#         #         print(err)
#         #     ax.set_global()

#         major = major or dict(labelsize=13)
#         minor = minor or {}

#         try:
#             self.set_lon_ticks(ax, lon, crs, major, minor)
#             self.set_lat_ticks(ax, lat, crs, major, minor)
#         except RuntimeError as err:
#             print(err)
#             gl = ax.gridlines(draw_labels=True)
#             gl.top_labels = False
#             gl.right_labels = False
#         ax.xaxis.set_label_text(xrpu.label_from_attrs(lon), fontsize=13)
#         ax.yaxis.set_label_text(xrpu.label_from_attrs(lat), fontsize=13)

#         try:
#             if abs(x1 - x0) > 359 and abs(y1 - y0) > 179:
#                 # raise ValueError(f"Force to set global {x0}:{x1}")
#                 # raise ValueError(f"Force to set global.")
#                 raise ValueError('')
#             if abs(x1 - x0) >= 360:
#                 if x1 > x0:
#                     x1 = x1 + 0.01
#                 else:
#                     x0 = x0 + 0.01
#             # print(ax.set_extent)
#             ax.set_extent((x0, x1, y0, y1), crs=crs)
#         except ValueError as err:
#             if str(err):
#                 print(err)
#             ax.set_global()

#     def set_lon_ticks(self, ax, lon, crs, major=None, minor=None):
#         major = major or {}
#         minor = minor or {}
#         x0, x1, sc = self.parse_coor(lon, item=True)
#         span = abs(x1 - x0)
#         vmaj, vmin = self.parse_ticks(lon.attrs)
#         # print(f"lon:{span=} {vmaj=}")
#         if span > vmaj > 0:
#             loc = mtc.MultipleLocator(vmaj)
#         else:
#             loc = cmtick.LongitudeLocator()
#         # print(type(loc), loc)
#         loc = [lo for lo in loc.tick_values(x0, x1)
#                if x0 <= lo <= x1]
#         # print((x0, x1), loc)
#         loc = sorted([lo - 360 if lo >= 180 else lo for lo in loc])
#         ax.set_xticks(ticks=loc, crs=crs)

#         if vmin > 0:
#             loc = mtc.MultipleLocator(vmin)
#         else:
#             loc = cmtick.LongitudeLocator()
#         loc = [lo for lo in loc.tick_values(x0, x1)
#                if x0 <= lo <= x1]
#         loc = sorted([lo - 360 if lo >= 180 else lo for lo in loc])
#         ax.set_xticks(ticks=loc, minor=True, crs=crs)

#         fmt = cmtick.LongitudeFormatter()
#         ax.xaxis.set_major_formatter(fmt)
#         # ax.xaxis.set_minor_formatter(fmt)
#         ax.xaxis.set_tick_params(which='major', **major)
#         ax.xaxis.set_tick_params(which='minor', **minor)

#     def set_lat_ticks(self, ax, lat, crs, major=None, minor=None):
#         major = major or {}
#         minor = minor or {}
#         x0, x1, sc = self.parse_coor(lat, item=True)
#         # if x0 > x1:
#         #     x0, x1 = x1, x0
#         # print(x0, x1)
#         span = abs(x1 - x0)
#         vmaj, vmin = self.parse_ticks(lat.attrs)
#         # print(f"{span=}\n{vmaj=}\n{vmin=}")
#         # print((x0, x1))
#         if span > vmaj > 0:
#             loc = mtc.MultipleLocator(vmaj)
#         else:
#             loc = cmtick.LatitudeLocator()
#         loc = [lo for lo in loc.tick_values(x0, x1)
#                if x0 <= lo <= x1]
#         ax.set_yticks(ticks=loc, crs=crs)

#         if vmin > 0:
#             loc = mtc.MultipleLocator(vmin)
#         else:
#             loc = cmtick.LatitudeLocator()
#         loc = [lo for lo in loc.tick_values(x0, x1)
#                if x0 <= lo <= x1]
#         ax.set_yticks(ticks=loc, minor=True, crs=crs)

#         fmt = cmtick.LatitudeFormatter()
#         ax.yaxis.set_major_formatter(fmt)
#         ax.yaxis.set_tick_params(which='major', **major)
#         ax.yaxis.set_tick_params(which='minor', **minor)

#     def add_features(self, /, *features, axs=None, key=None):
#         """Set GeoAxes extent and tick labels."""
#         axs = axs or self
#         key = key or 'body'
#         ax = axs.atbl.get(key, None)

#         for ft in features:
#             if isinstance(ft, str):
#                 ft = getattr(cfeature, ft)
#             if ft:
#                 ax.add_feature(ft)

#     def cbar_set_ticks(self, axs=None,
#                        major=None, minor=None, key=None):
#         """Set tick labels of colorbar."""
#         axs = axs or self
#         key = key or 'colorbar'
#         cax = axs.atbl.get(key, None)

#         major = major or {}
#         minor = minor or {}
#         cax.tick_params(which='major', **major)
#         cax.tick_params(which='minor', **minor)


# class LayoutAxes():
#     """Base layer of figure axes.
#     """

#     def __init__(self, figsize, layout=None, FigureClass=None, **kwds):
#         self.figsize = figsize

#         FigureClass = FigureClass or FigureCore
#         layout = layout or "none"

#         if not issubclass(FigureClass, FigureCore):
#             raise ValueError(f"{FigureClass} is not subclass of FigureCore")

#         fig = plt.figure(figsize=figsize,
#                          layout=layout, FigureClass=FigureClass,
#                          **self.figkwds)
#         self.axkw = {}
#         self.atbl = {}

#         if self.figsize is None:
#             self.figsize = fig.get_size_inches()
#         self.enlarge = 0.0

#     def reset(self, fig, axs=None, **kwds):
#         """Bind standard axes."""
#         axs = axs or self

#         for lab, rc in axs.rect.items():
#             if lab in axs.atbl:
#                 ax = axs.atbl[lab]
#                 ax.cla()
#                 print(f"clear: {fig.number} {lab} {id(ax)}")
#             else:
#                 axkw = (axs.axkw.get(lab, None) or {})\
#                     | (kwds.get(lab, None) or {})
#                 ax = fig.add_axes(rc, label=lab, **axkw)
#                 axs.atbl[lab] = ax
#                 print(f"create: {fig.number} {lab} {id(ax)}")
#             if lab not in ['body', 'colorbar', ]:
#                 ax.format_coord = lambda x, y: ''
#                 ax.set_axis_off()
#                 ax.add_artist(ax.patch)
#                 if lab.endswith('_title'):
#                     ax.set(zorder=-1)
#             print(fig.number, fig, lab, id(ax))
#         return axs


# class LayoutLegacy3(LegacyParser, LayoutBase):
#     """gtcont layout 3 emulation."""
#     name = 'Legacy3'

#     pos = {'bottom': True, 'top': True, 'right': True, 'left': True, }
#     major = {'length': 10.8, 'width': 1, 'pad': 5.8, 'labelsize': 14, }
#     minor = {'length': 5.4, 'width': 0.7, }
#     major.update(pos)
#     minor.update(pos)

#     cbar_major = {'bottom': True, 'length': 10.8,
#                   'width': 1, 'pad': 5.8, 'labelsize': 14, }
#     cbar_minor = False

#     geometry = (10.45, 7.39)

#     ofx, ofy = 0.2, 0.02
#     wb = 0.830
#     rect = {'body': (0.129, 0.225, wb, 0.585),
#             # 'info': (0.129 + ofx, 0.070 - ofy, 0.830 - ofx * 2, 0.057 + ofy),
#             'info': (0.129 + ofx, ofy, wb - ofx * 2, 0.1042 - ofy),
#             'colorbar': (0.061, 0.070, 0.245, 0.057),
#             'left_title': (0.129, 0.81, wb/2, 0.125),
#             'right_title': (0.129+wb/2, 0.81, wb/2, 0.125),
#             }
#     title_text = {'linespacing': 1.3, 'fontsize': 14, }
#     contour_text = {'fontsize': 12, }

#     def __init__(self, *args, figsize=None, config=None, **kwds):
#         self.config(config)
#         figsize = figsize or self.geometry
#         super().__init__(*args, figsize=figsize, **kwds)

#     def __call__(self, axs=None, figsize=None, **kwds):
#         """Create figure and standard axes."""
#         fig, axs = super().__call__(axs=axs, **kwds)

#         if figsize:
#             if figsize is True:
#                 figsize = self.geometry
#             elif isinstance(figsize, (int, float)):
#                 geo = figsize.get_size_inches()
#                 if figsize >= 0:
#                     figsize = geo * (1.0 + figsize)
#                 else:
#                     figsize = geo / (1.0 - figsize)
#             elif hasattr(figsize, 'get_size_inches'):
#                 figsize = figsize.get_size_inches()
#             fig.set_size_inches(figsize)

#         self.reset(fig, axs)
#         return fig, axs

#     def config(self, config=None):
#         config = config or {}
#         verbose = config.get('verbose') or 0
#         config = config.get(self.name) or {}
#         if verbose > 0:
#             verbose = sys.stdout
#         util.update_config(self, config, pfx=f'config[{self.name}]',
#                            verbose=verbose)

#     def reset(self, fig, axs=None, **kwds):
#         """Bind standard axes."""
#         axs = axs or self

#         # for lab, ax in axs.atbl.items():
#         #     print(lab, ax.get_xlim(), ax.get_ylim())

#         for lab, rc in axs.rect.items():
#             if lab in axs.atbl:
#                 ax = axs.atbl[lab]
#                 ax.cla()
#                 print(f"clear: {fig.number} {lab} {id(ax)}")
#             else:
#                 axkw = (axs.axkw.get(lab, None) or {})\
#                     | (kwds.get(lab, None) or {})
#                 ax = fig.add_axes(rc, label=lab, **axkw)
#                 axs.atbl[lab] = ax
#                 print(f"create: {fig.number} {lab} {id(ax)}")
#             if lab not in ['body', 'colorbar', ]:
#                 ax.format_coord = lambda x, y: ''
#                 ax.set_axis_off()
#                 ax.add_artist(ax.patch)
#                 if lab.endswith('_title'):
#                     ax.set(zorder=-1)
#             print(fig.number, fig, lab, id(ax))
#         return axs

#         fig.clf()

#         for lab, rc in axs.rect.items():
#             axkw = (axs.axkw.get(lab, None) or {})\
#                 | (kwds.get(lab, None) or {})
#             # print(lab, axkw)
#             axs.atbl[lab] = fig.add_axes(rc, label=lab, **axkw)
#             # print(lab, self.atbl[lab])
#         for lab, ax in axs.atbl.items():
#             if lab in ['body', 'colorbar', ]:
#                 continue
#             else:
#                 ax.format_coord = lambda x, y: ''
#                 ax.set_axis_off()
#                 ax.add_artist(ax.patch)
#                 if lab.endswith('_title'):
#                     ax.set(zorder=-1)
#         return axs

#     def set_ticks(self, *args, major=None, minor=None, **kwds):
#         """Set tick labels."""
#         major = major or self.major
#         minor = minor or self.minor
#         super().set_ticks(*args, major=major, minor=minor, **kwds)

#     def cbar_set_ticks(self, *args, major=None, minor=None, **kwds):
#         """Set colorbar ticks."""
#         major = major or self.cbar_major
#         minor = minor or self.cbar_minor
#         super().cbar_set_ticks(*args, major=major, minor=minor, **kwds)

#     def add_titles(self, data, axs=None, contours=None,
#                    key=None, tkey=None, lkey=None, rkey=None, **kwds):
#         """Add title."""

#         axs = axs or self
#         key = key or 'body'
#         tkey = tkey or 'info'
#         lkey = lkey or 'left_title'
#         rkey = rkey or 'right_title'
#         ax = axs.atbl.get(key, None)
#         tax = axs.atbl.get(tkey, None)
#         lax = axs.atbl.get(lkey, None)
#         rax = axs.atbl.get(rkey, None)

#         tkw = self.title_text | kwds

#         left = self.parse_titles(data.attrs, data.coords, default='')
#         lax.text(0, 1.0, '\n'.join(left),
#                  verticalalignment='top', **tkw)

#         dset = self.parse_dataset(data.attrs, default='')
#         cal = self.parse_calendar(data.attrs, default='')
#         right = (dset, cal, '')
#         rax.text(1.0, 1.0, '\n'.join(right),
#                  horizontalalignment='right',
#                  verticalalignment='top', **tkw)

#         if contours:
#             dc = contours[0].levels[1:] - contours[0].levels[:-1]
#             if len(set(dc)) == 1:
#                 ctext = f'CONTOUR INTERVAL = {dc[0]}'
#                 tax.text(0.5, 1.0, ctext,
#                          horizontalalignment='center',
#                          verticalalignment='top',
#                          **self.contour_text)

#     def colorbar(self, fig, *args, axs=None, orientation=None, key=None, **kwds):
#         """Wrap colorbar"""
#         axs = axs or self
#         key = key or 'colorbar'
#         cax = axs.atbl.get(key, None)
#         orientation = orientation or 'horizontal'
#         return fig.colorbar(*args, cax=cax, orientation=orientation, **kwds)

#     def toggle_axis(self, which, fig=None, axs=None, key=None):
#         axs = axs or self
#         key = key or 'body'
#         ax = axs.atbl.get(key)
#         if which == 'h':
#             f = ax.xaxis
#         else:
#             f = ax.yaxis
#         f.set_inverted(not f.get_inverted())

# class ContourPlot:
#     """Contour with fill-color."""

#     isep = '/'
#     lsep = ','
#     nsep = ':'

#     color_method = 'contourf'
#     color_method = 'imshow'

#     opts = {'add_labels': False, }

#     method_table = {'f': 'contourf', 'c': 'contour',
#                     'p': 'pcolormesh', 'i': 'imshow',
#                     's': 'surface', }

#     def __init__(self, contour=None, color=None):
#         self._contour = {'colors': 'black'}
#         self._color = {}

#         self._contour.update(contour or {})
#         self._color.update(color or {})

#     def __call__(self, fig, axs, data, cyclic=None, view=None,
#                  title=None, **kwds):
#         """Batch plotter."""
#         if any(w <= 1 for w in data.shape):
#             raise UserWarning("virtually less than two dimensions")

#         coords = data.coords
#         cj = [d for d in data.dims if coords[d].size > 1]
#         if len(cj) != 2:
#             raise ValueError(f"Not 2d shape={cj}")
#         xco = coords[cj[1]]
#         yco = coords[cj[0]]

#         if cyclic is None:
#             for ck in [0, 1, ]:
#                 c = coords[cj[ck]]
#                 cc = c.attrs.get('cyclic_coordinate')
#                 if not cc:
#                     continue
#                 w, org, dup = cc
#                 if len(c) == w - 1 \
#                    and c[0] == org \
#                    and c[-1] != dup:
#                     cd, cx = cutil.add_cyclic(data, c, axis=ck,
#                                               cyclic=dup-org)
#                     nco = dict(data.coords.items())
#                     nco[cj[ck]] = cx
#                     data = xr.DataArray(cd, coords=nco,
#                                         dims=data.dims, attrs=data.attrs)
#                     # print(f'cyclic condition: {cj[ck]}')
#                     break

#         ax = axs.atbl.get('body')
#         body = (axs.axkw.get('body') or {}) | (kwds.get('body') or {})

#         stat = {}
#         contour = self._contour | (kwds.get('contour') or {})
#         color = self._color | (kwds.get('color') or {})

#         if isinstance(ax, cmgeo.GeoAxes):
#             transf = body.get('transform')
#             if transf:
#                 contour.setdefault('transform', transf)
#                 color.setdefault('transform', transf)

#         if color['method'] == 'contour':
#             col, cbr = self.color(fig, axs, data, stat, **color)
#             con = self.contour(axs, data, **stat, **contour)
#         else:
#             con = self.contour(axs, data, stat, **contour)
#             col, cbr = self.color(fig, axs, data, **stat, **color)

#         # print(col, col.cmap)
#         if cbr:
#             for c in con:
#                 cbr.add_lines(c, erase=False)
#         else:
#             cbr = axs.colorbar(fig, con[0])
#             for c in con[1:]:
#                 cbr.add_lines(c, erase=False)

#         axs.add_titles(data, contours=con)
#         axs.cbar_set_ticks()

#         view = view or {}
#         vx = view.get(cj[1]) or {}
#         vy = view.get(cj[0]) or {}
#         # print(vx, vy)
#         # print(ax)
#         if isinstance(ax, cmgeo.GeoAxes):
#             if cj[0] in coords and cj[1] in coords:
#                 axs.set_geoticks(x=xco, y=yco, view_x=vx, view_y=vy,
#                                  crs=body.get('crs'))
#                 args = body.get('features') or []
#                 axs.add_features(*args)
#             else:
#                 raise ValueError("Physical coordinate not defined:"
#                                  f"{cj[0]} {cj[1]}.")
#         else:
#             axs.set_ticks(x=xco, y=yco, view_x=vx, view_y=vy)

#         # Not yet working....
#         if title:
#             for a in ['set_window_title', 'setWindowTitle', ]:
#                 f = getattr(fig.canvas, a, None)
#                 if f:
#                     f(f'Figure: {title}')
#                     break
#         return col

#     def contour(self, axs, data, stat=None,
#                 levels=None, clabel=None, key=None, **kwds):
#         """matplotlib contour wrapper."""
#         key = key or 'body'
#         ax = axs.atbl.get(key)

#         cc = []
#         levels = levels or True
#         if not isinstance(levels, cabc.Iterable):
#             levels = [levels]
#         elif isinstance(levels[0], nums.Number):
#             if isinstance(levels[0], bool):
#                 pass
#             else:
#                 levels = [levels]

#         clabel = util.set_default(clabel, True)
#         if clabel is True:
#             clabel = slice(min(1, len(levels) - 1), None, None)
#         if isinstance(clabel, slice):
#             clabel = range(*clabel.indices(len(levels)))
#         if isinstance(clabel, cabc.Iterable):
#             _clabel = clabel
#             # print(levels, list(_clabel))
#             def check_clabel(idx):
#                 return bool(idx in _clabel)
#             clabel = check_clabel
#         if clabel is False:
#             clabel = lambda idx: False
#         if not isinstance(clabel, cabc.Callable):
#             raise(f"invalid clabel argument {clabel}")

#         def run(idx, **args):
#             cp = data.plot.contour(ax=ax, **args,
#                                      **(self.opts | kwds))
#             if clabel(idx):
#                 ax.clabel(cp)
#             return cp

#         for j, lev in enumerate(levels):
#             if lev is True:
#                 c = run(j)
#             elif lev is False:
#                 continue
#             elif isinstance(lev, list):
#                 c = run(j, levels=lev)
#             elif isinstance(lev, cabc.Callable):
#                 vmin, vmax = self.get_range(data, stat, **kwds)
#                 c = run(j, levels=lev(vmin, vmax))
#             else:
#                 raise TypeError(f"invalid level specifier {c}.")
#             cc.append(c)
#         return cc

#     def color(self, fig, axs, data, stat=None,
#               method=None, cmap=None, levels=None,
#               key=None, **kwds):
#         """matplotlib contour wrapper."""
#         key = key or 'body'
#         ax = axs.atbl.get(key)
#         # print(ax)
#         if method is None:
#             method = True
#         if method is True:
#             method = self.color_method
#         if method is False:
#             col, cbr = None, None
#         else:
#             # print(data.dims)
#             func = getattr(data.plot, method, None)
#             if func is None:
#                 raise ValueError(f"invalid method {method}.")
#             # print(func)

#             if levels in [True, None]:
#                 levels = None
#             elif levels is False:
#                 pass
#             elif isinstance(levels, list):
#                 pass
#             elif isinstance(levels, cabc.Callable):
#                 vmin, vmax = self.get_range(data, stat, **kwds)
#                 levels = levels(vmin, vmax)
#             else:
#                 raise TypeError(f"invalid level specifier {levels}.")

#             if levels is False:
#                 col, cbr = None, None
#             else:
#                 if isinstance(cmap, cabc.Callable):
#                     cmap = cmap()
#                 col = func(ax=ax, add_colorbar=False,
#                            # x=data.dims[-1], y=data.dims[0],
#                            # transform=None,
#                            levels=levels, cmap=cmap,
#                            **(self.opts | kwds))
#                 if method == 'contour' and len(col.levels) <= 1:
#                     cbr = None
#                 else:
#                     cbr = axs.colorbar(fig, col)
#         return (col, cbr)

#     def get_range(self, data, stat, vmin=None, vmax=None, **kwds):
#         stat = stat or {}
#         vmin = vmin or stat.get('vmin')
#         vmax = vmax or stat.get('vmax')
#         if vmin is None:
#             vmin = data.min().values
#             stat['vmin'] = vmin
#         if vmax is None:
#             vmax = data.max().values
#             stat['vmax'] = vmax
#         return vmin, vmax

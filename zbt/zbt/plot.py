#!/usr/bin/env python3
# Time-stamp: <2024/12/23 10:15:06 fuyuki plot.py>

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
import logging

import numpy as np

import matplotlib as mplib
import matplotlib.ticker as mtc
import matplotlib.pyplot as plt
import matplotlib.backend_tools as mpbt
import matplotlib.contour as mcnt
import matplotlib.patches as mpatches
import matplotlib.transforms as mtr

import xarray as xr
import xarray.plot.utils as xrpu

import cartopy.crs as ccrs
import cartopy.feature as cfeature
import cartopy.util as cutil
import cartopy.mpl.geoaxes as cmgeo
import cartopy.mpl.ticker as cmtick

import zbt.util as zu
import zbt.config as zcfg

locallog = zu.LocalAdapter('plot')


_ConfigType = zcfg.ConfigRigid

# ### helper functions
def is_xr(obj):
    """Check if obj is DataArray instance"""
    return isinstance(obj, xr.DataArray)


class VarLevelsCore():
    """Base layer of cmap/contour-levels control."""

    nimsg = r"VarLevelsCore.{}() not implemented."

    def _raise(self, name):
        msg = f"VarLevelsCore.{name}() not implemented."
        raise NotImplementedError(msg)

    def get_cmap(self, *args, **kwds):
        """Dummy method to retrieve current cmap"""
        self._raise('get_cmap')

    def put_cmap(self, *args, **kwds):
        """Dummy method to register current cmap"""
        self._raise('put_cmap')

    def get_contour(self, *args, **kwds):
        """Dummy method to retrieve current contour levels"""
        self._raise('get_contour')

    def put_contour(self, *args, **kwds):
        """Dummy method to register current contour levels"""
        self._raise('put_contour')


class VarLevelsMinimum(VarLevelsCore):
    """Minimum Cmap Controler to get a constant map."""

    def __init__(self, cmap=None, contour=None):
        self.cmap = cmap
        self.contour = contour

    def put_cmap(self, *args, **kwds):
        pass

    def put_contour(self, *args, **kwds):
        pass

    def get_cmap(self, *args, **kwds):
        if isinstance(self.cmap, cabc.Callable):
            return self.cmap()
        return self.cmap

    def get_contour(self, *args, **kwds):
        return self.contour


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

    _cache_patches = ['facecolor', ]

    def __init__(self, *args, **kwds):
        super().__init__(*args, **kwds)
        self._stack_patches = []

    def __str__(self):
        return super().__str__()

    def _store_patch(self):
        c = {}
        for p in self._cache_patches:
            f = getattr(self.patch, f'get_{p}')
            c[p] = f()
        self._stack_patches.append(c)

    def push_patch(self, **kwds):
        self._store_patch()
        self.patch.set(**kwds)
        return kwds

    def switch_patch(self, pos):
        try:
            kwds = self._stack_patches[pos]
            self._store_patch()
            self.patch.set(**kwds)
        except IndexError:
            self._store_patch()
            kwds = None
        return kwds

    def pop_patch(self, pos=None):
        if pos is None:
            pos = -1
        kwds = self._stack_patches.pop(pos)
        self.patch.set(**kwds)
        return kwds

    def get_patch(self, pos=None):
        if pos is None:
            pos = -1
        return self._stack_patches[pos]

    def diag_patches(self):
        for j, s in enumerate(self._stack_patches):
            s = ' '.join(f"[{k}]={v}" for k, v in s.items())
            print(f"stack[{j}]: {s}")

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

    def parse_ticks(self, attrs, default=0):
        """Fallback for major and minor locators."""
        return (default, default)


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

    def parse_titles(self, data, default=None):
        """Parse basic title properties.
        Return a tuple of (title, unit, others). """

        attrs = data.attrs
        coords = data.coords

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
        title = title or data.name

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

class LayoutBase(ParserBase, zcfg.ConfigBase):
    """Base layer of figure/axes manipulation."""
    names = ('layout', )
    _axes_keys = ()
    geometry = None
    gunit = 0.0

    guide_ = {'color': 'red',
              'alpha': 0.5,
              'linewidth': 10.0 }

    def __init__(self, fig, figsize=None, reset=None, **kwds):
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
        # self.crs = {}
        self.projp = {}   # projection properites == (crs, transform)
        self.bg = {}
        self.fragiles = []
        self.guides = {}
        reset = True if reset is None else reset
        if bool(reset):
            self.reset(fig)

    def _iter_axes(self):
        try:
            for grp in self._axes_keys:
                amap = getattr(self, grp)
                yield from amap.items()
        except AttributeError as exc:
            raise AttributeError(f"No layout/axes map {grp} in {self}.") from exc

    def tweak_axes(self, ax, lab):
        """Dummy axes modifier."""
        return ax

    def __str__(self):
        axes = ' '.join(f"{str(key)}{type(ax)}"
                        for key, ax in self._axes.items())
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
        ax = self.get(key, None)
        if isinstance(ax, mplib.axes.Axes):
            return ax
        return default

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
                    locallog.warning(f"{kpr}={oproj} is cleared.")
                axkw.setdefault('label', lab)
                rect = axkw.pop('rect', rect)
                axkw.pop('fresh', None)
                ax = fig.add_axes(rect, **axkw)
            else:
                if axkw is True:
                    axkw = None
                axkw = {'rect': rect} | (axkw or {})
                ax = self.scan_or_new(fig, lab, ax_args, **axkw)
            self[lab] = self.tweak_axes(ax, lab)

    def scan_or_new(self, fig, lab, ax, fresh=None,
                    projection=None, rect=None, **kwds):
        if fresh is True:
            ax.set_visible(False)
            ax = None
        if ax:
            op = getattr(ax, 'projection', None)
            if op != projection:
                self.store(lab, ax, projection=op, **kwds)
                ax = self.restore(lab, projection=projection, **kwds)
        if not ax:
            ax = fig.add_axes(rect, label=lab, projection=projection, **kwds)
        return ax

    def store(self, lab, ax, projection=None, **kwds):
        key = lab, projection
        if key in self._refuges:
            if self._refuges[key] is ax:
                pass
            else:
                raise ValueError(f"Duplicate refuges {key} .")
        else:
            self._refuges[key] = ax
        ax.set_visible(False)
        return ax

    def restore(self, lab, projection=None, **kwds):
        key = lab, projection
        ax = self._refuges.get(key, None)
        if ax:
            ax.set_visible(True)
        return ax

    def cla(self, fig, axs=None, fragiles=None):
        self.cla_fragiles(fig, fragiles)
        if axs is None:
            axs = self.keys()
        elif isinstance(axs, list):
            pass
        else:
            axs = [axs]
        for ax in axs:
            ax_ = ax
            ax = self._get_axes(ax)
            if ax:
                ax.cla()
                lab = ax.get_label()
                # caution,  kept.
                _ = self.tweak_axes(ax, lab)

    def cla_fragiles(self, fig, axs=None):
        # if axs is None:
        #     self.guides = {}
        if axs is None:
            axs = self.fragiles[:]
        elif isinstance(axs, list):
            pass
        else:
            axs = [axs]
        for ax in axs:
            if ax in self.fragiles:
                fig.delaxes(ax)
                self.fragiles.remove(ax)


    # def cla_guides(self, axs=None):
    #     # if axs is None:
    #     #     self.guides = {}
    #     if axs is None:
    #         axs = list(self.guides.keys())
    #     elif isinstance(axs, list):
    #         pass
    #     else:
    #         axs = [axs]
    #     for ax in axs:
    #         at = self.guides.get(ax)
    #         if at:
    #             _, at = at
    #             at.set_visible(False)
    #         del(self.guides[ax])
    #         fig = ax.figure
    #         fig.delaxes(ax)

    def toggle_guides(self, fig, switch=None):
        if bool(switch):
            pass
        else:
            for ax in self.guides.keys():
                self.clear_guide(fig, ax)
        # switch = bool(switch)
        # for ax in self.guides.keys():
        #     # print('clear:', ax.get_gid())
        #     ax = self._get_axes(ax)
        #     at = self.guides.get(ax) or [None]
        #     for g in at:
        #         if g:
        #             g.set_visible(switch)

    def add_guides(self, ax, which=None, visible=None, **kwds):
        ax = self._get_axes(ax)
        if ax:
            # bg = fig.canvas.copy_from_bbox(ax.bbox)
            # self.bg[ax] = bg
            # print(ax.get_lines())
            if which is None:
                which = ['x', 'y']
            elif isinstance(which, (list, tuple, str)):
                pass
            else:
                which = [which]
            g = [None] * 2
            for w in which:
                if w == 'x':
                    at = ax.axvline(x=0, **kwds)
                    g[0] = at
                else:
                    at = ax.axhline(y=0, **kwds)
                    g[1] = at
                at.set_visible(bool(visible))
            self.guides[ax] = g

    def get_position(self, x, y, ax=None, crs=None):
        """Get position on data coordinates"""
        ax = self._get_axes(ax)
        pp = self.projp.get(ax) or None
        proj = getattr(ax, 'projection', None)

        if crs is None:
            crs = pp
        elif crs is False:
            crs = None
        if proj and crs:
            x, y = crs.transform_point(x, y, src_crs=proj)
        return x, y

    def position_transform(self, x, y, ax=None, crs=None):
        """Transform (display) position to data coordinates"""
        ax = self._get_axes(ax)
        if x is not None:
            transf = ax.get_xaxis_transform().inverted()
            x = transf.transform((x, 0))[0]
        if y is not None:
            transf = ax.get_yaxis_transform().inverted()
            y = transf.transform((0, y))[1]
        # pp = self.projp.get(ax) or None
        # proj = getattr(ax, 'projection', None)

        # if crs is None:
        #     crs = pp
        # elif crs is False:
        #     crs = None
        # if proj and crs:
        #     x, y = crs.transform_point(x, y, src_crs=proj)
        return x, y

    def get_extent(self, ax=None, crs=None, **kwds):
        """Get extent tuple of tuples.
        Return (xl, xh), (yl, yh), (crs, extent)"""
        ax = self._get_axes(ax)
        pp = self.projp.get(ax) or None
        if crs is None:
            crs = pp
            # crs = self.crs.get(ax)
        elif crs is False:
            crs = None
        cxy = None
        if ax:
            try:
                xy = ax.get_extent(crs=crs)
                xlim = xy[:2]
                ylim = xy[2:]
                if crs:
                    cxy = (crs, ax.get_extent())
            except AttributeError as exc:
                xlim = ax.get_xlim()
                ylim = ax.get_ylim()
        else:
            xlim = None
            ylim = None
        return xlim, ylim, cxy

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
            locallog.debug(f"{args=} {kwds=}")
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
        locallog.debug(f"colorbar: {args} {kwds}")
        bar = fig.colorbar(*args, cax=cax, **kwds)
        artists.extend([bar, cax, ])
        return artists

    def set_view(self, data, ax=None, artists=None,
                 x=None, y=None, crs=None, **kwds):
        artists = artists or []
        ax = self._get_axes(ax)
        if isinstance(ax, cmgeo.GeoAxes):
            artists = self.set_geo_view(data, ax=ax, artists=artists,
                                        x=x, y=y, crs=crs, **kwds)
            # self.crs[ax] = crs
            self.projp[ax] = crs
        elif ax:
            co = [d for d in data.dims if data.coords[d].size > 1]
            if len(co) != 2:
                raise ValueError(f"Not 2d shape={co}")
            x = x or co[-1]
            y = y or co[0]

            xc = data.coords[x]
            yc = data.coords[y]

            self.set_tick(xc, ax, 'x', **(kwds.get(x) or {}))
            self.set_tick(yc, ax, 'y', **(kwds.get(y) or {}))

            artists.extend([ax.xaxis, ax.yaxis,
                            ax.xaxis.get_offset_text(),
                            ax.yaxis.get_offset_text()])
            # ax.xaxis.get_offset_text().set_fontsize(24)
            # ax.yaxis.get_offset_text().set_fontsize(24)
            return artists

    def set_tick(self, co, ax, which,
                 major=None, minor=None, dmin=None, dmax=None,
                 **kwds):
        """Draw axis."""
        if which == 'x':
            axis, limf, scf = ax.xaxis, ax.set_xlim, ax.set_xscale
        else:
            axis, limf, scf = ax.yaxis, ax.set_ylim, ax.set_yscale

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
        # ltx.set_gid(f"axis:{co.name}")
        ltx.set_gid(f"axis:{which}")
        axis.set_picker(True)
        # axis.set_picker(self.axis_picker)
        # axis.set_gid(f"tick:{co.name}")
        axis.set_gid(f"tick:{which}")
        axis.set_tick_params(which='major', **(major or {}))
        axis.set_tick_params(which='minor', **(minor or {}))
        if dmin != dmax:
            limf(dmin, dmax)
        if scale:
            scf(scale)

    def set_geo_view(self, data, ax=None, artists=None,
                     x=None, y=None,
                     crs=None, extent=None, **kwds):
        print(f"{extent=}")
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

        print(f"{view_x=} {view_y=}")
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
            if extent:
                proj = getattr(ax, 'projection', None)
                ax.set_extent(extent, crs=proj)
            else:
                if abs(x1 - x0) > 359 and abs(y1 - y0) > 179:
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

    def add_aux_axes(self, fig, ax, which, tol=None, **kwds):
        """Introduce auxiliary axes, such as axis and spine."""
        ax = self._get_axes(ax)
        if which == 'x':
            axis = ax.xaxis
            skeys = ['top', 'bottom']
            other = 'y'
        elif which == 'y':
            axis = ax.yaxis
            skeys = ['left', 'right']
            other = 'x'
        else:
            raise ValueError(f'Panic.  Invalid argument {which}')

        # tol = 20.0
        if tol is None:
            tol = 10.0

        lab = ax.get_label()
        gid = ax.get_gid()

        txt = axis.get_label()
        xlabel = list(self.extent(txt, which))
        wlabel = self.extent(txt, other, ext=tol)
        locallog.debug(f"{lab}/{which} <{txt.get_text()}> {xlabel=} {wlabel=}")

        if txt.get_text():
            taxis = self.tightbbox(axis, which)
            waxis = self.tightbbox(axis, other)
            aspine = self.search_labeled_spine(taxis, ax, which)
        else:
            taxis = None
            waxis = None
            aspine = None
        locallog.debug(f"{lab}/{which} {aspine=} {taxis=} {waxis=}")
        if aspine and aspine not in skeys:
            skeys.insert(aspine, 0)

        xbody = self.extent(ax, which)

        gprop = self.prop('guide') or {}

        for k in skeys:
            sp = ax.spines[k]
            xspine = list(self.extent(sp, which))
            wspine = list(self.extent(sp, other))

            xspine = self.adjust_spine_bb(xspine, xbody, xlabel)
            locallog.debug(f"{lab}/{which} <{k}> {xspine=}")

            sax = self.register_bg(fig, xspine, wspine, which,
                                   ('spine', gid, k), zorder=-2)
            if sax:
                if which == 'x':
                    at = sax.axvline(x=0, **gprop)
                    self.guides[sax] = (at, None)
                else:
                    at = sax.axhline(y=0, **gprop)
                    self.guides[sax] = (None, at)
                at.set_visible(False)

            if aspine == k:
                xlabel[1] = max(xlabel[1] + tol, xspine[0])
                xlabel[0] = min(xlabel[0] - tol, xspine[1])
                locallog.debug(f"{lab}/{which} <{k}> {xlabel=}")
                self.register_bg(fig, xlabel, wlabel, which,
                                 ('axis', gid, k), zorder=-1)

    def register_bg(self, fig, bbw, bbo, which, key, gid=None, **kwds):
        if gid is None:
            gid = key
        if which == 'x':
            bb = (bbo[0], bbw[0], bbo[1], bbw[1])
        else:
            bb = (bbw[0], bbo[0], bbw[1], bbo[1])
        if bb[0] != bb[2] and bb[1] != bb[3]:
            reg = mtr.Bbox.from_extents(*bb)
            inv = fig.transFigure.inverted()
            fbb = reg.transformed(inv)
            ax = fig.add_axes(fbb)
            self.fragiles.append(ax)
            ax.set_axis_off()
            ax.add_artist(ax.patch)
            ax.set_gid(gid)
            ax.set(**kwds)
            bg = fig.canvas.copy_from_bbox(ax.bbox)
            self.bg[ax] = bg
        else:
            ax = None
        return ax

    def search_labeled_spine(self, bb, ax, which):
        if bb:
            aorg = bb[1] + bb[0]
            dist = []
            for k, sp in ax.spines.items():
                xsp = self.extent(sp, which)
                sorg = xsp[1] + xsp[0]
                dist.append((abs(aorg - sorg), k))
            _, spine = min(dist)
        else:
            spine = None
        return spine

    def adjust_spine_bb(self, spine, body, label=None, rate=None):
        rate = 0.5 if rate is None else rate
        # rate = 1 if rate is None else rate
        if label:
            # LABEL SPINE
            if spine[1] > label[1]:
                end = min(spine[0], label[1])
                spine[0] = spine[0] + (end - spine[0]) * rate
            # SPINE LABEL
            elif spine[0] < label[0]:
                end = max(spine[1], label[0])
                spine[1] = spine[1] + (end - spine[1]) * rate

        if spine[1] > body[1]:
            spine[0] = max(spine[0], body[1])
        elif spine[0] < body[0]:
            spine[1] = min(spine[1], body[0])

        return spine

    def bounds(self, bb, which, ext=None):
        try:
            if which == 'x':
                bb = bb.ymin, bb.ymax    # intentional
            else:
                bb = bb.xmin, bb.xmax
        except AttributeError:
            return None
        if isinstance(ext, tuple):
            bb = bb[0] + ext[0], bb[1] + ext[1]
        elif isinstance(ext, (int, float)):
            bb = bb[0] - ext, bb[1] + ext
        return bb

    def extent(self, obj, which, ext=None):
        return self.bounds(obj.get_window_extent(), which, ext)

    def tightbbox(self, obj, which, ext=None):
        return self.bounds(obj.get_tightbbox(), which, ext)

    def clear_guide(self, fig, ax):
        ax = self._get_axes(ax)
        bg = self.bg.get(ax)
        at = self.guides.get(ax) or [None]
        for g in at:
            if g:
                g.set_visible(False)
        if bg:
            fig.canvas.restore_region(bg)
            fig.canvas.blit(ax.bbox)

    def draw_guide(self, fig, ax, x=None, y=None, pos=None, **kwds):
        ax = self._get_axes(ax)
        at = self.guides.get(ax)

        # locallog.debug(f"{ax=} {at=}")
        # print(ax.get_lines())
        if at:
            xg, yg = at
            bg = self.bg.get(ax)
            if bg:
                fig.canvas.restore_region(bg)
            if xg and x is not None:
                xg.set_xdata([x])
                xg.set_visible(True)
                ax.draw_artist(xg)
            if yg and y is not None:
                yg.set_ydata([y])
                yg.set_visible(True)
                ax.draw_artist(yg)
            fig.canvas.blit(ax.bbox)

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
            which = which.lower()
            if which in ['x', 'h', 'bottom', 'top', ]:
                f = ax.xaxis
            elif which in ['y', 'v', 'left', 'right', ]:
                f = ax.yaxis
            else:
                raise ValueError(f"invalid axis target {which}.")
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

    def retrieve_event(self, event):
        """Get axes label corresponding to event."""
        ax = event.inaxes
        if ax:
            for lab, a in self.items():
                if a == ax:
                    return lab
            else:
                gid = ax.get_gid()
                if gid:
                    return gid
                return ax
        return None


class LayoutLegacy3(LayoutBase, LegacyParser, _ConfigType):
    """Emulate GTOOL3/gtcont layout 3."""

    _config = False

    # names = ('Legacy3', ) + LayoutBase.names
    names = ('Legacy3', )   # no dependency

    geometry = (10.45, 7.39)
    gunit = 1.0 / 128

    axis_ = {'tol': 5.0 }

    _pos = {'bottom': True, 'top': True, 'right': True, 'left': True, }
    _major = {'length': 10.8, 'width': 1.0, 'pad': 5.8, 'labelsize': 14.0, }
    _minor = {'length': 5.4, 'width': 0.7, }
    _major.update(_pos)
    _minor.update(_pos)

    body = {'major': _major, 'minor': _minor, }
    axis_text = {'fontsize': 14.0, }

    colorbar_ = {'major': {'bottom': True, 'length': 10.8,
                           'width': 1.0, 'pad': 5.8, 'labelsize': 14.0, },
                 'minor': False,
                 'guide': {'color': True, 'alpha': 1.0, 'linewidth': 20.0 }, }

    _title_text = {'linespacing': 1.3, 'fontsize': 14.0, }
    _contour_text = {'fontsize': 12.0, }

    left_title_ = {'title': True, } | _title_text
    right_title_ = {'title': True, } | _title_text
    info_ = {'title':
             {'contour': {'format': '.3'}, }}
    info_ |=  _contour_text
    monitor_ = {'fontsize': 15.0 }

    _ofx, _ofy = 0.2, 0.02
    _lb, _wb = 0.129, 0.830
    _bt, _ht = 0.81, 0.125

    graph = {'body': (_lb, 0.225, _wb, 0.585),
             'colorbar': (0.061, 0.070, 0.245, 0.057), }

    text = {'info': (_lb + _ofx, _ofy, _wb - _ofx * 2, 0.1042 - _ofy),
            'left_title': (_lb, _bt, _wb / 2, _ht),
            'right_title': (_lb + _wb / 2, _bt, _wb / 2, _ht),
            'monitor': (1 - 0.061 - 0.245, 0.070 - 0.03,
                        0.245, 0.057 + 0.03), }

    _axes_keys = LayoutBase._axes_keys + ('graph', 'text', )

    def __init__(self, *args, **kwds):
        super().__init__(*args, **kwds)
        self.bg = {}

    def tweak_axes(self, ax, lab, attr=None):
        """Tweaking of axes at creation."""
        if attr is None:
            attr = self.prop(lab, {})
        gid = attr.setdefault('gid', lab)
        ax.set_gid(gid)
        if lab not in self.prop('graph'):
            ax.format_coord = lambda x, y: ''
            ax.set_axis_off()
            ax.add_artist(ax.patch)
            if isinstance(lab, str) and lab.endswith('_title'):
                ax.set(zorder=-3)
        gprop = self.prop('guide') or {}
        if lab in self.prop('graph'):
            if lab == 'body':
                for axis in [ax.xaxis, ax.yaxis]:
                    axis.label.set_gid(gid)
                    axis.label.set(**self.prop('axis_text', {}))
                    for m in ['major', 'minor']:
                        a = attr.get(m) or {}
                        axis.set_tick_params(which=m, **a)
                    axis.set_gid(gid)
                self.add_guides(ax, 'xy', **gprop)
            elif lab == 'colorbar':
                for m in ['major', 'minor']:
                    ax.tick_params(which=m, **(attr.get(m) or {}))
        if lab in ['monitor']:
            kwds = self.prop(lab, {})
            ax.text(0, 0.98, '',
                    transform=ax.transAxes,
                    animated=True,
                    va='top', ha='left', color='grey', **kwds)
            # bg = fig.canvas.copy_from_bbox(fig.bbox)
        # if lab in ['body']:
        #     kwds = self.prop(lab, {})
        #     ax.text(0.5, 0.5, 'BODY',
        #             transform=ax.transAxes,
        #             animated=True,
        #             va='top', ha='left', color='red')
        return ax

    def cla(self, fig, axs=None):
        # if axs is None:
        #     axs = list(self.keys())
        #     axs.remove('monitor')
        # print(axs)
        super().cla(fig, axs)

    def which(self, func, ax=None):
        ax = ax or 'body'
        return self._get_axes(ax)

    def get_extent(self, ax=None, **kwds):
        ax = ax or 'body'
        return super().get_extent(ax=ax, **kwds)

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
            sopts = title[sk]
            sv = aux.get(sk) or []
            if sk == 'contour' and len(sv) > 0:
                fmt = sopts.get('format', '')
                dc = sv[0].levels[1:] - sv[0].levels[:-1]
                if len(dc) == 0:
                    text.append(f'CONTOUR = {sv[0].levels[0]}')
                else:
                    mi, ma = min(dc), max(dc)
                    if mi == ma:
                        mi = f"{mi:{fmt}}"
                        text.append(f'CONTOUR INTERVAL = {mi}')
                    elif (ma - mi) < max(abs(ma), abs(mi)) * 1.e-3:
                        mi = f"{mi:{fmt}}"
                        text.append(f'CONTOUR INTERVAL ~ {mi}')
                    else:
                        levs = ','.join(f"{lv:{fmt}}" for lv in sv[0].levels)
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
            title = self.parse_titles(data, default='')
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

    def pop_monitor(self, ax=None):
        """matplotlib contour wrapper."""
        ax = ax or 'monitor'
        ax = self._get_axes(ax)
        if ax:
            at = ax.texts[0]
            text = at.get_text()
            at.set_visible(False)
            return text
        return ''

    def on_draw(self, fig, ax=None, **kwds):
        self.bg = {}
        self.cla_fragiles(fig)
        # locallog.info(f'on_draw')
        # if self.skip_fragiles:
        #     self.skip_fragiles = False
        # else:
        for lab in self.keys():
            ax = self._get_axes(lab)
            if isinstance(ax, cmgeo.GeoAxes):
                continue
            if lab in self.prop('graph'):
                self.add_aux_axes(fig, lab, 'x', **kwds)
                self.add_aux_axes(fig, lab, 'y', **kwds)
                bg = fig.canvas.copy_from_bbox(ax.bbox)
                self.bg[ax] = bg

    def monitor(self, fig, text, *args,
                ax=None, **kwds):
        """matplotlib contour wrapper."""
        ax = ax or 'monitor'
        # ax = ax or 'body'
        ax = self._get_axes(ax)
        if ax:
            bg = self.bg.get(ax)
            if not bg:
                bg = fig.canvas.copy_from_bbox(ax.bbox)
                self.bg[ax] = bg
            at = ax.texts[0]
            at.set_visible(True)
            # at.set_visible(False)
            at.set_text(text, **kwds)
            fig.canvas.restore_region(bg)
            ax.draw_artist(at)
            fig.canvas.blit(ax.bbox)
        return

    # def retrieve_event(self, event, tol=None):
    #     if tol is None:
    #         axp = self.prop('axis')
    #         tol = axp.get('tol')
    #     lab = super().retrieve_event(event, tol=tol)
    #     return lab


# ## Plot ############################################################
class PlotBase(zcfg.ConfigBase):
    names = ('plot', )
    pass


class ContourPlot(PlotBase, _ConfigType):
    """Contour with fill-color."""
    names = PlotBase.names + ('contour', )

    _opts = {'add_labels': False, }

    color_ = {'method': 'pcolormesh', } | _opts
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

        coords = data.coords
        cj = [d for d in data.dims if coords[d].size > 1]
        if len(cj) != 2:
            raise ValueError(f"Not 2d shape={cj}")
        xco = coords[cj[1]]
        yco = coords[cj[0]]

        contour = self._contour | (kwds.get('contour') or {})
        color = self._color | (kwds.get('color') or {})
        locallog.debug(f"{color=}")

        # if isinstance(ax, cmgeo.GeoAxes):
        body = kwds.get('body') or {}
        transf = body.get('transform')
        crs = body.get('crs')

        if transf:
            contour.setdefault('transform', transf)
            color.setdefault('transform', transf)

        con = self.contour(axs, data, **contour)
        col = self.color(axs, data, **color)
        bar, cax, = self.colorbar(fig, axs, con, col,
                                  contour=contour, color=color)

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

        # print(f"{view=}")
        artists = self.set_view(axs, data, artists=artists,
                                crs=crs, **(view or {}))

        fts = body.get('features') or []
        fts = axs.add_features(*fts)

        return artists

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
            raise ValueError(f"invalid clabel argument {labels}")

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
                vmin, vmax = self.get_range(data, **kwds)
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
            method = 'pcolormesh'

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
              method=None, cmap=None, norm=None, levels=None,
              key=None, artists=None, bind=None, **kwds):
        """matplotlib contour wrapper."""
        artists = artists or []

        method = self.check_method(axs, data, method, key)
        # locallog.debug(f"color:{kwds=}")

        if method is False:
            pass
        else:
            revc = False
            # if isinstance(norm, cabc.Callable):
            #     norm = norm()
            #     if norm:
            #         norm, revc = norm
            if levels in [True, None]:
                levels = None
            elif levels is False:
                pass
            elif isinstance(levels, list):
                pass
            elif isinstance(levels, cabc.Callable):
                vmin, vmax = self.get_range(data, **kwds)
                levels = levels(vmin, vmax)
            else:
                raise TypeError(f"invalid level specifier {levels}.")

            if levels is False:
                pass
            else:
                cm = cmap
                # if isinstance(cmap, cabc.Callable):
                #     cm = cmap()
                if isinstance(cm, mplib.cm.ScalarMappable):
                    locallog.debug(f'colors: {cm.get_clim()=}')
                    cm = cm.get_cmap()
                # if revc and cm:
                #     if isinstance(cm, list):
                #         cm = list(reversed(cm))
                #     elif isinstance(cm, mplib.colors.Colormap):
                #         cm = cm.reversed()
                #     else:
                #         cmt = mplib.colormaps.get(cm)
                #         if cmt:
                #             cm = cmt.reversed()
                # # print(revc, cmap)
                # if isinstance(cm, list):
                #     ckw = {'colors': cm}
                # else:
                #     ckw = {'cmap': cm}
                #     # locallog.debug(f"color: {cm=} {ckw}")
                # print(levels)
                ckw = {'cmap': cm, 'norm': norm, }
                col = axs.color(data, ax=key,
                                method=method,
                                add_colorbar=False,
                                levels=levels,
                                **ckw,
                                **kwds)
                if isinstance(bind, cabc.Callable):
                    bind(artist=col[0])
                artists.extend(col)
        # locallog.debug(f"color:return: {artists}")
        return artists

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
        if cols[0]:
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
                    m = len(ticks)
                    if m == 0 or m > len(cs.levels):
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

    def get_range(self, data, vmin=None, vmax=None, **kwds):
        """Data range computation."""
        if vmin is None:
            vmin = data.attrs.get('vmin')
        if vmax is None:
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

    logger = zu.logger
    logger.setLevel(logging.INFO)

    LayoutTest = LayoutLegacy3

    LayoutTest.config(config, groups=('zbcont', 'test', ))
    LayoutTest.diag(strip=False)

    # fig = plt.figure(FigureClass=FigureCore)
    # lay = LayoutTest(fig)

    Pic = Picture(LayoutClass=LayoutTest)
    fig1, lay1 = Pic(reset=True)
    fig2, lay2 = Pic(reset=True)

    # print(fig1, lay1)
    # print(lay1['body'])
    # lay1.reset(fig1)
    # lay2.reset(fig2)

    lay1['body'].plot([0, 1, 2, 3], [3, 2, 1, 0])
    lay2['body'].plot([0, 1, 2, 3], [0, 3, 1, 2])

    ContourPlot.config(config, groups=('zbcont', 'test', ))

    ContourPlot.diag(strip=False)

    plt.show()

    pass


if __name__ == '__main__':
    import sys
    main(sys.argv[1:])

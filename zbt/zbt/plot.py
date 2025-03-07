#!/usr/bin/env python3
# Time-stamp: <2025/03/07 09:23:22 fuyuki plot.py>
#
# Copyright (C) 2024, 2025
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

"""
Plotter collections.

:Source:     zbt/plot.py
:Maintainer: SAITO Fuyuki
:Created:    Oct 9 2024
"""

import sys
import collections.abc as cabc
import functools as ft
import numbers as nums
import pprint as ppr
import logging
import copy
import math
import datetime

import numpy as np

import matplotlib as mplib
import matplotlib.ticker as mtc
import matplotlib.pyplot as plt
import matplotlib.backend_tools as mpbt
import matplotlib.contour as mcnt
import matplotlib.patches as mpatches
import matplotlib.transforms as mtr
import matplotlib.artist as mart

import xarray as xr
import xarray.plot.utils as xrpu

import cartopy.crs as ccrs
import cartopy.feature as cfeature
import cartopy.util as cutil
import cartopy.mpl.geoaxes as cmgeo
import cartopy.mpl.ticker as cmtick

import cftime
import pandas

import zbt.util as zu
import zbt.config as zcfg

locallog = zu.LocalAdapter(__name__)

_ConfigType = zcfg.ConfigRigid

# ### helper functions
def is_xr(obj) -> bool:
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

        try:
            if not issubclass(self.lcls, LayoutBase):
                raise ValueError(f"Invalid LayoutClass {self.lcls}.")
        except TypeError as err:
            if isinstance(self.lcls, cabc.Callable):
                pass
            else:
                raise err(f"Invalid LayoutClass {self.lcls}.") from None

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
        self._stack_patches = {}

    def __str__(self):
        return super().__str__()

    def _store_patch(self, ax):
        c = {}
        pp = self._stack_patches.setdefault(ax, [])
        for p in self._cache_patches:
            f = getattr(self.patch, f'get_{p}')
            c[p] = f()
        pp.append(c)
        # self._stack_patches.append(c)

    def push_patch(self, ax=None, **kwds):
        self._store_patch(ax)
        if ax is None:
            self.patch.set(**kwds)
        else:
            ax.patch.set(**kwds)
        return kwds

    def switch_patch(self, pos, ax=None):
        try:
            kwds = self._stack_patches[ax][pos]
            self._store_patch(ax)
            self.patch.set(**kwds)
        except (KeyError, IndexError):
            self._store_patch(ax)
            kwds = None
        return kwds

    def pop_patch(self, ax=None, pos=None):
        if pos is None:
            pos = -1
        kwds = self._stack_patches[ax].pop(pos)
        self.patch.set(**kwds)
        return kwds

    def get_patch(self, ax=None, pos=None):
        if pos is None:
            pos = -1
        return self._stack_patches[ax][pos]

    def diag_patches(self):
        for ax, pp in self._stack_patches.items:
            for j, s in enumerate(pp):
                s = ' '.join(f"[{k}]={v}" for k, v in s.items())
                print(f"<{ax}> stack[{j}]: {s}")

    def show_info(self, props=None, **kwds):
        if props is None:
            props = True
        if props in [True, False]:
            def check(k):
                return props
        else:
            if not isinstance(props, (list, tuple)):
                props = [props]
            def check(k):
                return k in props

        if check('size'):
            print(f'figure size: {self.get_size_inches()}')
        if check('dpi'):
            print(f'figure dpi: {self.get_dpi()}')
        return

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
        if len(coor.shape) > 1:
            dmax = max(coor.values.flat)
            dmin = min(coor.values.flat)
        else:
            dmax = coor[-1]
            dmin = coor[0]
        scale = ''
        if item:
            dmax = dmax.item()
            dmin = dmin.item()
        if dmin > dmax:
            dmin, dmax = dmax, dmin
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

    def parse_slices(self, coords, ignore_time=None):
        """Parse slice properties."""
        sel = []
        for d, c in coords.items():
            if c.size == 1:
                dt, du = self.extract_titles(c.attrs)
                dt = dt or d
                if dt.lower() in ['time', 'record', ]:
                    if bool(ignore_time):
                        continue
                    ds = f"{dt}={c.values}"
                else:
                    try:
                        ds = f"{dt}={c.values:.4f}"
                    except ValueError as err:
                        locallog.warning(err)
                        ds = f"{dt}={c.values}"
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
        sel = self.parse_slices(coords, ('IDFM' in attrs))

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

    def parse_coor(self, coor, dmin=None, dmax=None,
                   direction=None, item=False):
        """Parse axis limit."""
        locallog.debug(f"parse_coor: {dmin} {dmax}")
        ATTR = r'attr'
        MAX, MIN = r'max', r'min'
        miss = coor.attrs.get('MISS', '').strip()
        def parse_sub(key, arg, *special):
            arg = ATTR if arg is None else arg
            if isinstance(arg, str) and ATTR.startswith(arg):
                d = coor.attrs.get(key, '').strip()
                d = None if d == miss else d
            else:
                d = arg
            if isinstance(d, str):
                for s in special:
                    if s.startswith(d):
                        d = s
                        break
            elif d is None and special:
                d = special[0]
            return d
        dmax = zu.tonumber(parse_sub('DMAX', dmax, MAX, MIN))
        dmin = zu.tonumber(parse_sub('DMIN', dmin, MIN, MAX))
        locallog.debug(f"parse_coor:1: {dmin} {dmax}")

        styp = int(parse_sub('STYP', None) or 0)
        sc = ''
        if abs(styp) == 2:
            sc = 'log'
        locallog.debug(f"parse_coor: {styp=}")
        def get_special(v, vmin, vmax):
            if v == MAX:
                return vmax
            if v == MIN:
                return vmin
            return v

        if any(v in [MAX, MIN] for v in [dmax, dmin]):
            vmin, vmax, _ = super().parse_coor(coor, item=item)
            dmin = get_special(dmin, vmin, vmax)
            dmax = get_special(dmax, vmin, vmax)

        if all(isinstance(v, (int, float)) for v in [dmax,  dmin]):
            dmax = float(dmax)
            dmin = float(dmin)

        ## to do: set direction by sign.
        if direction is True:
            pass
        elif styp < 0:
            dmin, dmax = dmax, dmin
        locallog.debug(f"parse_coor:final: {dmin=} {dmax=}")
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
    _category = {}
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
        # self.scales = None
        # self.cur_size = None
        self.resize(fig, figsize=figsize, ref=self.geometry)
        self._orig_size = self._cur_size
        # self.orig_size = fig.get_size_inches()
        # self.crs = {}
        self.projp = {}   # projection properites == (crs, transform)
        self.bg = {}
        self.bbox = {}
        self.cb = {}
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

    def get_axes(self, key, default=None):
        """Return key if Axes, otherwise lookup internal dict."""
        return self._get_axes(key, default)

    def index(self, ax, default=None):
        for k, v in self._axes.items():
            if v is ax:
                return k
        return default

    # figure methods
    def _resize_calc(self, rate, base):
        if rate >= 0:
            return base * (1.0 + rate)
        return base / (1.0 - rate)

    # def _resize_prop(self, org, cur, rate):
    #     if rate is None:
    #         return 0
    #     cur = self._resize_calc(cur, rate)
    #     r = cur / org
    #     if r >= 1.0:
    #         return r - 1.0
    #     return 1.0 - org / cur

    def resize_by_rate(self,
                       rate: nums.Number|bool|None,
                       ref: tuple[nums.Number, nums.Number]) \
                       -> tuple[nums.Number, nums.Number]|None:
        try:
            if rate is True:
                figsize = ref[0:2]
            elif rate in [None, False]:
                figsize = None
            else:
                figsize = tuple(self._resize_calc(rate, b)
                                for b in ref)
        except:
            pass
        return figsize

    def resize_by_tuple(self,
                        figsize:tuple[nums.Number|bool|None,...],
                        ref: tuple[nums.Number, nums.Number]|None) \
                        -> tuple[nums.Number, nums.Number]:
        """
        Resize figure.

        Parameters
        ----------
        figsize : tuple[nums.Number|bool|None,...]
            Horizontal and vertical sizes.
            If an element is False, None, or negative, corresponding reference
            size is applied.
            If an element is True or 0, the size to keep the aspect ratio
            is applied.

        ref : tuple
            Reference size.

        Returns
        -------
        tuple[number, number]
            New figure size.

        Raises
        ------
        ValueError
            Raise if geometry property to refer is not set.
        """
        if len(figsize) > 2:
            raise ValueError(f"Invalid argument {figsize=}.")
        elif len(figsize) < 2:
            figsize = (figsize + (None, None, ))[:2]
        figsize = tuple(True if f == 0
                        else False if not f or f < 0
                        else f for f in figsize)

        try:
            if all(isinstance(f, bool) for f in figsize):
                figsize = ref[:2]
            elif figsize[0] is False:
                figsize = (ref[0], figsize[1])
            elif figsize[1] is False:
                figsize = (figsize[0], ref[1])
            elif figsize[0] is True:
                figsize = (figsize[1] * (ref[0] / ref[1]), figsize[1])
            elif figsize[1] is True:
                figsize = (figsize[0], figsize[0] * (ref[1] / ref[0]))
            else:
                pass
        except TypeError as err:
            if not isinstance(ref, tuple):
                raise ValueError(f"geometry not set for {self}.") from None
            else:
                raise err
        return figsize

    def _get_fig_scale(self):
        scales = [c / g for c, g in zip(self._cur_size, self.geometry)]
        sc = min(scales)
        return sc

    def normalize_size_params(self, props, sc=None, copy=None):
        if sc is None:
            sc = self._get_fig_scale()
        if bool(copy):
            params = props.copy()
        else:
            params = {}
        for k in ['labelsize', 'length', 'pad', 'width',
                  'fontsize', ]:
            cu = props.get(k)
            if cu:
                params[k] = cu * sc
        return params

    def update_size(self, fig, size=None, forward=None, **kwds):
        if forward is None:
            forward = True
        if size is not None:
            fig.set_size_inches(*size, forward=forward, **kwds)
        self._cur_size = fig.get_size_inches()

    def resize_scale(self, fig, sc=None):
        sc = sc or self._get_fig_scale()
        for o in fig.findobj(mplib.axis.Axis):
            gid = o.get_gid()
            if not gid:
                continue
            ref = gid
            if ref in self._axes:
                prop = self.prop(ref)
                for m in ['major', 'minor']:
                    mp = prop.get(m) or {}
                    params = self.normalize_size_params(mp, sc)
                    if params:
                        o.set_tick_params(which=m, **params)
            label = o.label
            ref = label.get_gid()
            if ref in self._axes:
                prop = self.prop(ref)
                ap = prop.get('axis') or {}
                ap = self.normalize_size_params(ap, sc)
                label.set(**ap)

        for o in fig.findobj(mplib.text.Text):
            gid = o.get_gid()
            if gid is None:
                continue
            if gid in self._axes:
                prop = self.prop(gid)
                ap = self.normalize_size_params(prop, sc)
                o.set(**ap)
            try:
                gid, ref = gid[0:2]
            except (TypeError, ValueError):
                continue
            if ref in self._axes:
                prop = self.prop(ref)
                prop = prop.get(gid) or {}
                prop = self.normalize_size_params(prop, sc)
                o.set(**prop)

        def match(artist: mplib.artist.Artist) -> bool:
            return isinstance(artist,
                              (mplib.collections.QuadMesh,
                               mplib.collections.LineCollection,
                               mplib.contour.QuadContourSet))

        for o in fig.findobj(match):
            gid = o.get_gid()
            try:
                gid, ref, w = gid
            except (TypeError, ValueError):
                # print(gid)
                continue
            prop = self.prop(ref)
            prop = prop.get(gid) or {}
            u = prop.get('unitwidth') or 0.0
            u = u * sc
            u = u * w
            if u:
                lw = o.get_linewidth()
                o.set_linewidth([u] * len(lw))

    def resize(self,
               fig: mplib.figure.Figure|None,
               figsize: mplib.figure.Figure|nums.Number|bool|cabc.Iterable|None,
               ref: mplib.figure.Figure|bool|cabc.Iterable|None = None):
        """
        Resize figure.

        Parameters
        ----------
        fig : Figure
            Target figure.
        figsize : nums.Number|bool|tuple|None
            New figure size.
            If True or None, import reference figure size.
            If False, skip resizing.
            If Number, it is passed to _resize_calc method.
            If tuple, it is passed to resize_by_tuple method.

        ref : Figure|tuple|None
            Reference figure.
            If ref is None, the current size of fig is adopted.
            If ref is True, the original size is adopted.
            If ref is False, the initial size, which may be set
            by command-line, is adopted.
            Otherwise ref must be tuple of geometry.
        """
        debug = locallog.debug
        # debug = locallog.info

        if ref is None:
            ref = fig.get_size_inches()
        elif ref is False:
            ref = self._orig_size
        elif ref is True:
            ref = self.geometry

        figsize = zu.set_default(figsize, True)
        debug(f"{ref=} {figsize=}")
        if hasattr(figsize, 'get_size_inches'):
            figsize = figsize.get_size_inches()
        elif isinstance(figsize, cabc.Iterable):
            figsize = self.resize_by_tuple(figsize, ref)
        else:
            figsize = self.resize_by_rate(figsize, ref)
        self.update_size(fig, figsize)
        self.resize_scale(fig)
        # if figsize is not None:
        #     fig.set_size_inches(*figsize, forward=True)

    def get_scales(self, size):
        pass

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
                    # at = ax.axvline(x=0, **kwds)
                    at, _ = self.guide_hvline(ax, x=0, **kwds)
                    g[0] = at
                else:
                    # at = ax.axhline(y=0, **kwds)
                    _, at = self.guide_hvline(ax, y=0, **kwds)
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
        # print(x, y, self.get_position(x, y, ax=ax))
        if x is not None:
            transf = ax.get_xaxis_transform().inverted()
            # x = transf.transform((x, 0))[0]
            x, tmp = transf.transform((x, 0))
            if crs is not False:
                x, tmp = self.get_position(x, tmp, ax=ax, crs=crs)
        if y is not None:
            transf = ax.get_yaxis_transform().inverted()
            tmp, y = transf.transform((0, y))
            if crs is not False:
                tmp, y = self.get_position(tmp, y, ax=ax, crs=crs)
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
        # print(f"{xlim=} {ylim=} {cxy=}")
        return xlim, ylim, cxy

    def plot(self, data, *args,
             ax=None, artists=None, gid=None,
             **kwds):
        ax = self._get_axes(ax)
        artists = artists or []
        if ax:
            if is_xr(data):
                lp = data.plot.line(ax=ax, *args, **kwds)
            else:
                lp = ax.plot(data, *args, **kwds)
            # lp.set_gid(gid)
            artists.extend(lp)
        return artists

    def contour(self, data, *args,
                ax=None, artists=None, clabel=None,
                unitwidth=None, **kwds):
        """matplotlib contour wrapper."""
        lw = kwds.get('linewidths', 0)
        gid = ("contour", f"{ax}", lw)

        ax = self._get_axes(ax)
        artists = artists or []
        unitwidth = unitwidth or 0.0
        lw = lw * unitwidth * self._get_fig_scale()
        if lw:
            kwds['linewidths'] = lw
        # print(kwds)
        if ax:
            # if False:
            if is_xr(data):
                # print(f"{args=}")
                cp = data.plot.contour(*args, ax=ax, **kwds)
            else:
                cp = ax.contour(*args, data, **kwds)
            cp.set_gid(gid)
            artists.append(cp)
            if clabel:
                clabel = ax.clabel(cp)
                artists.extend(clabel)
        return artists

    def clabel(self, contour, ax=None, artists=None, **kwds):
        gid = ("contour", f"{ax}")
        ax = self._get_axes(ax)
        artists = artists or []
        if ax:
            for la in ax.clabel(contour, **kwds):
                la.set_gid(gid)
                artists.append(la)
        return artists

    def color(self, data, *args,
              ax=None, artists=None,
              method=None, **kwds):
        """matplotlib contour wrapper."""
        gid = ("color", f"{ax}")
        ax = self._get_axes(ax)
        artists = artists or []
        # print(type(ax.transData))
        if ax:
            # locallog.debug(f"{args=} {kwds=}")
            if is_xr(data):
                cp = method(ax=ax, *args, **kwds)
            else:
                cp = method(*args, data, **kwds)
            cp.set_gid(gid)
            artists.append(cp)
        return artists

    def colorbar(self, fig, *args,
                 cax=None, artists=None, **kwds):
        """Wrap figure.colorbar()"""
        cax = self._get_axes(cax)
        artists = artists or []

        # need refresh axes_locator to avoid
        # undesired inheritance of colormap bounding box
        cax.set_axes_locator(None)
        bar = fig.colorbar(*args, cax=cax, **kwds)
        artists.extend([bar, cax, ])

        return artists

    def set_view(self, data, ax=None, artists=None,
                 x=None, y=None, crs=None, axisp=None):
        artists = artists or []
        ax = self._get_axes(ax)
        axisp = axisp or {}
        if isinstance(ax, cmgeo.GeoAxes):
            artists = self.set_geo_view(data, ax=ax, artists=artists,
                                        x=x, y=y, crs=crs, axisp=axisp)
            self.projp[ax] = crs
        elif ax:
            co = [d for d in data.dims if data.coords[d].size > 1]
            if len(co) != 2:
                raise ValueError(f"Not 2d shape={co}")
            x = x or co[-1]
            y = y or co[0]

            xc = data.coords[x]
            yc = data.coords[y]

            ar = axisp.get('aspect')
            if ar:
                ax.set_aspect(ar)
            ap = axisp.get(x) or {}
            locallog.debug(f'set_view:x: {ap=}')
            self.set_tick(xc, ax, 'x', **(ap or {}))

            ap = axisp.get(y) or {}
            locallog.debug(f'set_view:y: {ap=}')
            self.set_tick(yc, ax, 'y', **(ap or {}))

            artists.extend([ax.xaxis, ax.yaxis,
                            ax.xaxis.get_offset_text(),
                            ax.yaxis.get_offset_text()])
            return artists

    def set_tick(self, co, ax, which,
                 major=None, minor=None, dmin=None, dmax=None,
                 scale=None, direction=None, **kwds):
        """Draw axis."""
        if which == 'x':
            axis, limf, scf = ax.xaxis, ax.set_xlim, ax.set_xscale
            # getf = ax.get_xlim
        else:
            axis, limf, scf = ax.yaxis, ax.set_ylim, ax.set_yscale
            # getf = ax.get_ylim
        dmin, dmax, sc = self.parse_coor(co, dmin=dmin, dmax=dmax,
                                         direction=direction)
        scale = scale or sc

        dmin = adapt_values(dmin, co)
        dmax = adapt_values(dmax, co)

        span = abs(dmax - dmin)

        locallog.debug(f"{type(dmin)=} {type(dmax)=}")
        locallog.debug(f"{span=}")
        vmaj, vmin = self.parse_ticks(co.attrs)
        if isinstance(span, datetime.timedelta):
            pass
        elif span > vmaj > 0:
            axis.set_major_locator(mtc.MultipleLocator(vmaj))
        if vmin > 0:
            axis.set_minor_locator(mtc.MultipleLocator(vmin))

        txt = xrpu.label_from_attrs(co)
        ltx = axis.set_label_text(txt)
        ltx.set_picker(True)
        axis.set_picker(True)
        # axis.set_picker(self.axis_picker)
        gid = ax.get_gid()
        axis.set_tick_params(which='major', **(major or {}))
        axis.set_tick_params(which='minor', **(minor or {}))
        if isinstance(dmin, xr.DataArray):
            dmin = dmin.values
        if isinstance(dmax, xr.DataArray):
            dmax = dmax.values
        if dmin != dmax:
            limf(dmin, dmax)

        if scale == 'log':
            if isinstance(span, datetime.timedelta):
                locallog.warning(f"Bypass log-scale check ({dmin}:{dmax}).")
            elif dmin <= 0.0 or dmax <= 0.0:
                locallog.warning(f"log-scale axis is ignored ({dmin}:{dmax}).")
                scale = None
        if scale:
            scf(scale)

    def set_geo_view(self, data, ax=None, artists=None,
                     x=None, y=None,
                     crs=None, axisp=None):
        axisp = axisp or {}
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

        view_x = axisp.get(x) or {}
        view_y = axisp.get(y) or {}

        if x in data.coords and y in data.coords:
            try:
                self.set_lon_view(ax, xc, crs, **view_x)
                self.set_lat_view(ax, yc, crs, **view_y)
            except RuntimeError as err:
                locallog.debug(err)
                gl = ax.gridlines(draw_labels=True)
                gl.top_labels = False
                gl.right_labels = False
        else:
            raise ValueError("Physical coordinate not defined:"
                             f"{x} {y}")
        def wrap_parse_coor(co, **opts):
            kw = {}
            for k in ['dmin', 'dmax', 'direction']:
                if k in opts:
                    kw[k] = opts[k]
            return self.parse_coor(co, **kw)

        x0, x1, sx = wrap_parse_coor(xc, **view_x)
        y0, y1, sy = wrap_parse_coor(yc, **view_y)

        extent = axisp.get('extent')
        try:
            if extent:
                # print(f"{extent=}")
                proj = getattr(ax, 'projection', None)
                # ax.set_extent(extent, crs=proj)
                ax.set_xlim(extent[:2])
                ax.set_ylim(extent[2:])
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

    def add_features(self, *, features=None,
                     ax=None, artists=None, gid=None, **kwds):
        """Set GeoAxes extent and tick labels."""
        ax = self._get_axes(ax)
        artists = artists or []
        if ax:
            for ft, opts in features.items():
                if isinstance(ft, str):
                    ft = getattr(cfeature, ft)
                if ft:
                    artists.append(ax.add_feature(ft, **opts))
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
        # locallog.debug(f"{lab}/{which} {aspine=} {taxis=} {waxis=}")
        if aspine and aspine not in skeys:
            skeys.insert(aspine, 0)

        xbody = self.extent(ax, which)

        gprop = self.prop('guide') or {}

        for k in skeys:
            sp = ax.spines[k]
            xspine = list(self.extent(sp, which))
            wspine = list(self.extent(sp, other))

            xspine = self.adjust_spine_bb(xspine, xbody, xlabel)
            # locallog.debug(f"{lab}/{which} <{k}> {xspine=}")

            sax = self.register_bg(fig, xspine, wspine, which,
                                   ('spine', gid, k), zorder=-2)
            if sax:
                if which == 'x':
                    # at = sax.axvline(x=0, **gprop)
                    at, _ = self.guide_hvline(sax, x=0, **gprop)
                    self.guides[sax] = (at, None)
                else:
                    # at = sax.axhline(y=0, **gprop)
                    _, at = self.guide_hvline(sax, y=0, **gprop)
                    self.guides[sax] = (None, at)
                at.set_visible(False)

            if aspine == k:
                xlabel[1] = max(xlabel[1] + tol, xspine[0])
                xlabel[0] = min(xlabel[0] - tol, xspine[1])
                # locallog.debug(f"{lab}/{which} <{k}> {xlabel=}")
                self.register_bg(fig, xlabel, wlabel, which,
                                 ('axis', gid, k), zorder=-1)

    def guide_hvline(self, ax, x=None, y=None, **kwds):
        atx = None
        aty = None
        if x is not None:
            atx = ax.vlines(x, ymin=0, ymax=1, transform=ax.transAxes, **kwds)
        if y is not None:
            aty = ax.hlines(y, xmin=0, xmax=1, transform=ax.transAxes, **kwds)
        return atx, aty

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
            bb = ax.bbox
            bg, bb = self.copy_from_bbox(fig, bb)
            self.bg[ax] = bg, bb
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
            bg, bb = bg
            fig.canvas.restore_region(bg)
            # fig.canvas.blit(ax.bbox)
            # fig.canvas.blit(bb)
            fig.canvas.blit()

    def draw_guide(self, fig, ax, x=None, y=None, pos=None, **kwds):
        ax = self._get_axes(ax)
        at = self.guides.get(ax)

        # locallog.debug(f"{ax=} {at=}")
        if at:
            xg, yg = at
            bg = self.bg.get(ax)
            if bg:
                bg, bb = bg
                fig.canvas.restore_region(bg)
            else:
                bb = None
            if xg and x is not None:
                xg.set_segments([np.array([[x, 0], [x, 1]])])
                xg.set_visible(True)
                ax.draw_artist(xg)
            if yg and y is not None:
                yg.set_segments([np.array([[0, y], [1, y]])])
                yg.set_visible(True)
                ax.draw_artist(yg)
            fig.canvas.blit()

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

    def push_patch(self, fig, ax=None, **kwds):
        ax = self._get_axes(ax)
        return fig.push_patch(ax=ax, **kwds)

    def pop_patch(self, fig, ax=None, pos=None):
        ax = self._get_axes(ax)
        return fig.pop_patch(ax=ax, pos=pos)

    def copy_from_bbox(self, fig, bb):
        """call copy_from_bbox() with bb rounded."""
        ibb = ((math.floor(bb.xmin), math.floor(bb.ymin)),
               (math.ceil(bb.xmax), math.ceil(bb.ymax)))
        ibb = mplib.transforms.Bbox(ibb)
        bg = fig.canvas.copy_from_bbox(ibb)
        return bg, ibb

    def show_info(self, ax=None, props=None, **kwds):
        if props is None:
            props = True
        if props in [True, False]:
            def check(k):
                return props
        else:
            if not isinstance(props, (list, tuple)):
                props = [props]
            def check(k):
                return k in props

        def float_array(fu, *args):
            v = fu(*args)
            if isinstance(v, cabc.Iterable):
                return type(v)(float(j) for j in v)
            return v

        if ax:
            lab = self.index(ax) or ax.get_gid()
            print(f"<axes: {lab}>")
            for kf in [('xlim', float_array, ax.get_xlim),
                       ('xbound', float_array, ax.get_xbound),
                       ('xlabel', ax.get_xlabel),
                       ('xscale', ax.get_xscale),
                       ('ylim', float_array, ax.get_ylim),
                       ('ybound', float_array, ax.get_ybound),
                       ('ylabel', ax.get_ylabel),
                       ('yscale', ax.get_yscale),
                       ('title', ax.get_title),
                       ('aspect', ax.get_aspect),
                       ('box_aspect', ax.get_box_aspect),
                       ('adjustable', ax.get_adjustable),
                       ]:
                k, f, a = kf[0], kf[1], kf[2:]
                if check(k):
                    print(f'  {k}: ', f(*a))
        return

    def set_aspect(self, aspect, ax=None):
        ax = self._get_axes(ax)
        return ax.set_aspect(aspect)

class LayoutLegacyBase(LayoutBase, LegacyParser, _ConfigType):
    """Emulate GTOOL3/gtcont layout (common part)."""

    _config = False

    # names = ('Legacy3', ) + LayoutBase.names
    names = ('Legacy', )   # no dependency

    geometry = (10.45, 7.39)
    gunit = 1.0 / 128

    axis_ = {'tol': 5.0 }

    _pos = {'bottom': True, 'top': True, 'right': True, 'left': True, }
    _major = {'length': 10.8, 'width': 1.0, 'pad': 5.8, 'labelsize': 14.0, }
    _minor = {'length': 5.4, 'width': 0.7, }
    _major.update(_pos)
    _minor.update(_pos)

    # body = {'major': _major, 'minor': _minor, }
    _label = {'fontsize': 14.0, }
    body = {'major': _major, 'minor': _minor, 'axis': _label, }
    axis_text = {}

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

    _category = {'body': ['body', ],
                 'colorbar': ['colorbar', ], }

    def __init__(self, *args,
                 aspect=None, box_aspect=None, body=None,
                 **kwds):
        body = body or {}
        if aspect is not None:
            body['aspect'] = aspect
        if box_aspect is not None:
            body['box_aspect'] = box_aspect
        super().__init__(*args, body=body, **kwds)
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
            for axis in [ax.xaxis, ax.yaxis]:
                axis.set_gid(gid)
                axis.label.set_gid(gid)
                # axis.label.set(**self.prop('axis_text', {}))
                a = attr.get('axis', {})
                a = self.normalize_size_params(a, copy=True)
                axis.label.set(**a)
                for m in ['major', 'minor']:
                    a = attr.get(m) or {}
                    a = self.normalize_size_params(a, copy=True)
                    axis.set_tick_params(which=m, **a)
            if lab == 'body':
                # for axis in [ax.xaxis, ax.yaxis]:
                #     axis.label.set_gid(gid)
                #     # axis.label.set(**self.prop('axis_text', {}))
                #     a = attr.get('axis', {})
                #     a = self.normalize_size_params(a)
                #     axis.label.set(**a)
                #     for m in ['major', 'minor']:
                #         a = attr.get(m) or {}
                #         a = self.normalize_size_params(a)
                #         axis.set_tick_params(which=m, **a)
                #     axis.set_gid(gid)
                self.add_guides(ax, 'xy', **gprop)
                props = {}
                for k in ['aspect', ]:
                    if k in attr:
                        props[k] = attr[k]
                ax.set(**props)
            # elif lab == 'colorbar':
            #     for axis in [ax.xaxis, ax.yaxis]:
            #         axis.label.set_gid(gid)
            #         axis.set_gid(gid)
            #     for m in ['major', 'minor']:
            #         ax.tick_params(which=m, **(attr.get(m) or {}))
        if lab in ['monitor']:
            kwds = self.prop(lab, {})
            kwds = self.normalize_size_params(kwds, copy=True)
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

    def plot(self, *args, ax=None, **kwds):
        """Wrapper to call ax.plot()"""
        ax = ax or 'body'
        return super().plot(*args, ax=ax, **kwds)

    def contour(self, *args, ax=None, **kwds):
        """Wrapper to call ax.contour()"""
        ax = ax or 'body'
        # print(args[1:])
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

    def set_aspect(self, *args, ax=None, **kwds):
        ax = ax or 'body'
        return super().set_aspect(*args, ax=ax, **kwds)

    def add_features(self, features, /, ax=None, **kwds):
        ax = ax or 'body'
        return super().add_features(ax=ax, features=features, **kwds)

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
                full = []
                for item in sv:
                    try:
                        full = full + list(item.levels)
                    except AttributeError:
                        pass
                full = np.array(sorted(set(full)))
                # print(full)
                locallog.debug(f"full contours: {full}")
                # dc = sv[0].levels[1:] - sv[0].levels[:-1]
                dc = full[1:] - full[:-1]
                if len(dc) == 0:
                    text.append(f'CONTOUR = {full[0]}')
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
            kwds = self.normalize_size_params(kwds, copy=True)
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
            kwds = self.normalize_size_params(kwds, copy=True)
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
            # cal = self.parse_calendar(data.attrs, default='')
            cal = self.parse_calendar(data.nio, default='')
            title = (dset, cal, '')
        if isinstance(title, (list, tuple)):
            title = '\n'.join(str(s) for s in title)
        if title:
            ha = zu.set_default(horizontalalignment, 'right')
            va = zu.set_default(verticalalignment, 'top')
            kwds = self.normalize_size_params(kwds, copy=True)
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
        osc = self._get_fig_scale()
        self.update_size(fig)
        sc = self._get_fig_scale()
        if osc != sc:
            self.resize_scale(fig, sc=sc)
            # fig.canvas.draw()
            locallog.warning("Figure resize detected.  Please refresh.")
        # locallog.info(f'on_draw')
        # if self.skip_fragiles:
        #     self.skip_fragiles = False
        # else:
        for lab in self.keys():
            ax = self._get_axes(lab)
            if isinstance(ax, cmgeo.GeoAxes):
                # pp = self.projp.get(ax) or None
                proj = getattr(ax, 'projection', None)
                if not isinstance(proj, ccrs.PlateCarree):
                    continue
                # print(type(pp), type(proj))
                # continue
            if lab in self.prop('graph'):
                self.add_aux_axes(fig, lab, 'x', **kwds)
                self.add_aux_axes(fig, lab, 'y', **kwds)

                bb = ax.bbox
                bg, bb = self.copy_from_bbox(fig, bb)
                # bg = fig.canvas.copy_from_bbox(bb)
                # print(f"on_draw: {bg=}")
                self.bg[ax] = bg, bb

    def monitor(self, fig, text, *args,
                ax=None, **kwds):
        """matplotlib contour wrapper."""
        ax = ax or 'monitor'
        # ax = ax or 'body'
        ax = self._get_axes(ax)
        if ax:
            bg = self.bg.get(ax)
            if not bg:
                bb = ax.bbox
                bg, bb = self.copy_from_bbox(fig, bb)
                # bg = fig.canvas.copy_from_bbox(bb)
                # print(f"monitor: {bg=}")
                self.bg[ax] = bg, bb
            else:
                bg, bb = bg
            at = ax.texts[0]
            at.set_visible(True)
            # at.set_visible(False)
            at.set_text(text, **kwds)
            fig.canvas.restore_region(bg)
            # print(fig.canvas.restore_region)
            # fig.canvas.restore_region(bg)
            ax.draw_artist(at)
            # fig.canvas.blit(ax.bbox)
            # fig.canvas.blit(bb)
            fig.canvas.blit()
        return

    def show_info(self, ax=None, props=None, **kwds):
        if ax is None:
            ax = True
        if ax is True:
            axs = self.graph.keys()
        elif not isinstance(ax, (list, tuple)):
            axs = [ax]
        else:
            axs = ax

        for ax in axs:
            ax = self._get_axes(ax)
            if ax:
                super().show_info(ax, props=props, **kwds)
        return

    # def retrieve_event(self, event, tol=None):
    #     if tol is None:
    #         axp = self.prop('axis')
    #         tol = axp.get('tol')
    #     lab = super().retrieve_event(event, tol=tol)
    #     return lab


class LayoutLegacy3(LayoutLegacyBase, LegacyParser, _ConfigType):
    """Emulate GTOOL3/gtcont layout 3."""

    _config = False

    # names = ('Legacy3', ) + LayoutBase.names
    names = ('Legacy3', ) + LayoutLegacyBase.names



# ## Plot ############################################################
class PlotBase(zcfg.ConfigBase):
    names = ('plot', )

    def extract_plot_coords(self, data, num):
        coords = data.coords
        dims = data.dims
        xco = [coords[d] for d in dims if coords[d].size > 1]

        if len(xco) != num:
            raise ValueError(f"Not {num}d shape={xco}")
        if num == 1:
            return xco[0]
        return tuple(xco)


class ContourPlot(PlotBase, _ConfigType):
    """Contour with fill-color."""
    names = PlotBase.names + ('contour', )

    _opts = {'add_labels': False, }

    color_ = {'method': 'pcolormesh', } | _opts
    contour_ = {'colors': 'black',
                # 'unitwidth': 0.8,
                'unitwidth': 1.0,
                'fontsize': 11.0,
                } | _opts

    def __init__(self, contour=None, color=None):
        self._contour = self.prop('contour') | (contour or {})
        self._color = self.prop('color') | (color or {})

    def __call__(self, fig, axs, data, view=None,
                 artists=None, **kwds):
        """Batch plotter."""
        if any(w <= 1 for w in data.shape):
            raise UserWarning("virtually less than two dimensions")

        artists = artists or []
        view = view or {}

        # to do: data_col data_con 2d check
        axisp = kwds.get('axis') or {}
        coords = view.pop("coords", None)
        if coords:
            yco, xco = coords
            xy = dict(x=xco, y=yco)
        else:
            xy = {}
            coords = data.coords

            cj = [d for d in data.dims if coords[d].size > 1]
            if len(cj) != 2:
                raise ValueError(f"Not 2d shape={cj}")
            xco = coords[cj[1]]
            yco = coords[cj[0]]
            coords = ()

        color = self._color | (kwds.get('color') or {})
        # if isinstance(ax, cmgeo.GeoAxes):
        body = kwds.get('body') or {}
        transf = body.get('transform')
        crs = body.get('crs')

        if isinstance(transf, tuple):
            # for t in transf:
            #     if t is not None:
            #         break
            # else:
            #     t = None
            # # transf = mplib.transforms.blended_transform_factory(*transf)
            # transf = t
            transf = None

        if transf:
            # contour.setdefault('transform', transf)
            color.setdefault('transform', transf)

        cset = kwds.get('contour') or {}
        axs.update('body', {'contour': self._contour})
        if isinstance(cset, dict):
            contour = self._contour | cset
            if transf:
                contour.setdefault('transform', transf)
            data_con = contour.pop('data', None)
            if data_con is None:
                data_con = data
            zargs = self.data_transform(data_con, *coords)
            # print(f"{len(zargs)=}")
            contour = axs.normalize_size_params(contour, copy=True)
            con = self.contour(axs, *zargs, **contour)
        else:
            con = []
            data_con = data
            zargs = self.data_transform(data_con, *coords)
            for contour in cset:
                contour = self._contour | contour
                if transf:
                    contour.setdefault('transform', transf)
                # print(f"{contour=}")
                contour = axs.normalize_size_params(contour, copy=True)
                c = self.contour(axs, *zargs, **contour)
                con.extend(c)
        data_col = color.pop('data', None)
        if data_col is None:
            data_col = data
        zargs = self.data_transform(data_col, *coords)

        col = self.color(axs, *zargs, **color)
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

        vopts = {}
        vopts.update(**view)
        # print(f"{vopts=}")
        for k, v in axisp.items():
            vopts.setdefault(k, {})
            vopts[k].update(v)
        artists = self.set_view(axs, data, artists=artists,
                                **xy, crs=crs, axisp=vopts)

        fts = body.get('features') or {}
        fts = axs.add_features(fts)

        return artists

    def data_transform(self, data, *coords):
        if np.issubdtype(data, np.datetime64):
            locallog.warning(f"Cannot plot {data.dtype} array {data.name}")
            data = data.astype('float')
        args = [data]
        if coords:
            # print(coords, data.coords)
            args.extend(list(reversed(coords)))
        return args

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

    def contour(self, axs, *data,
                levels=None, clabel=None, key=None,
                artists=None, bind=None, fontsize=None, **kwds):
        """matplotlib contour wrapper."""
        artists = artists or []
        # levels = self.nml_levels(levels)
        # clabel = self.annotation(clabel, levels)
        run = ft.partial(axs.contour, *data,
                         ax=key, clabel=False, **kwds)
        alab = []
        if levels is True:
            c = run()
        elif levels is False:
            c = []
        else:
            c = run(levels=levels)
        artists.extend(c)
        if clabel and c:
            # alab = axs.clabel(*c, ax=key, artists=alab, gid='clabel')
            alab = axs.clabel(*c, ax=key, artists=alab, fontsize=fontsize)

        ret = artists + alab
        if isinstance(bind, cabc.Callable):
            bind(artist=ret)
        return ret

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
            if method == 'pcolormesh':
                # func = ft.partial(func, edgecolor='face')
                # func = ft.partial(func, edgecolor='none', linewidth=0,
                #                   rasterized=True)
                func = ft.partial(func, edgecolor='none')
            return func
        else:
            raise ValueError(f"invalid method {method}.")

    def color(self, axs, data, *args,
              method=None, cmap=None, norm=None, levels=None,
              key=None, artists=None, bind=None, **kwds):
        """matplotlib contour wrapper."""
        artists = artists or []

        # if method == 'contourf':
        #     if 'locator' not in kwds:
        #         kwds['locator']=mtc.LogLocator()
        ## Need special treatment for logscale contourf
        if method in ['contourf', 'contour', ]:
            if isinstance(norm, mplib.colors.LogNorm):
                is_logscale = 'log'
            elif isinstance(norm, mplib.colors.SymLogNorm):
                is_logscale = 'symlog'
            else:
                is_logscale = ''
        else:
            is_logscale = False

        method = self.check_method(axs, data, method, key)

        # locallog.debug(f"color:{kwds=}")
        if norm:
            locallog.debug(f"color:{norm.vmin=} {norm.vmax=}")

        if method is False:
            pass
        else:
            revc = False
            # if isinstance(norm, cabc.Callable):
            #     norm = norm()
            #     if norm:
            #         norm, revc = norm
            locallog.debug(f"color: ini {levels=}")
            if is_logscale and levels is None:
                levels = 0
            if levels in [True, None]:
                levels = None
            elif levels is False:
                pass
            elif isinstance(levels, int):
                if is_logscale == 'log':
                    levels = levels or None
                    base = 10
                    vmin, vmax = norm.vmin, norm.vmax
                    # vmin = math.pow(base, math.floor(math.log(vmin, base)))
                    # vmax = math.pow(base, math.ceil(math.log(vmax, base)))
                    loc = mtc.LogLocator(numticks=levels)
                    # levels = [z for z in loc.tick_values(vmin, vmax)
                    #           if vmin <= z <= vmax]
                    levels = loc.tick_values(vmin, vmax)
                elif is_logscale == 'symlog':
                    levels = levels or None
                    base = 10
                    vmin, vmax = norm.vmin, norm.vmax
                    # vmin = math.pow(base, math.floor(math.log(vmin, base)))
                    # vmax = math.pow(base, math.ceil(math.log(vmax, base)))
                    loc = mtc.SymmetricalLogLocator(linthresh=norm.linthresh,
                                                    base=base)
                    loc.set_params(numticks=levels)
                    vmin = vmin or (- norm.linthresh * 10)
                    vmax = vmax or (+ norm.linthresh * 10)
                    levels = loc.tick_values(vmin, vmax)
                    if len(levels) == 1:
                        levels = [levels[0] / 10, levels[0], levels[0] * 10]
                else:
                    vmin, vmax = self.get_range(data, **kwds)
                    loc = mtc.MaxNLocator(levels)
                    levels = loc.tick_values(vmin, vmax)
            elif isinstance(levels, list):
                pass
            elif isinstance(levels, cabc.Callable):
                vmin, vmax = self.get_range(data, **kwds)
                levels = levels(vmin, vmax)
            else:
                raise TypeError(f"invalid level specifier {levels}.")

            locallog.debug(f"color: fin {levels=}")
            if levels is False:
                pass
            else:
                cm = cmap
                if isinstance(cm, mplib.cm.ScalarMappable):
                    # locallog.debug(f'colors: {cm.get_clim()=}')
                    cm = cm.get_cmap()
                ckw = {'cmap': cm, 'norm': norm, }
                col = axs.color(data, *args,
                                ax=key,
                                method=method,
                                add_colorbar=False,
                                levels=levels,
                                **ckw,
                                **kwds)
                if isinstance(bind, cabc.Callable):
                    bind(artist=col[0])
                artists.extend(col)
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
        cax = None
        ticks = []
        alpha = color.get('alpha')
        # print(f"{alpha=} {cols[0]=}")

        if cols[0]:
            if method == 'contour':
                ticks = cols[0].levels
            else:
                # bar, cax, = axs.colorbar(fig, cols[0],
                #                          alpha=color.get('alpha'))
                bar, cax, = axs.colorbar(fig, cols[0])

        for cs in cons:
            if isinstance(cs, mcnt.QuadContourSet):
                if bar:
                    try:
                        bar.add_lines(cs, erase=False)
                        cid = cs.get_gid()
                        bar.lines[-1].set_gid(cid)
                    except ValueError as err:
                        # soft-landing for (possibly) empty array
                        # : zero-size array to reduction operation
                        # : maximum which has no identity
                        locallog.warning(err)
                        locallog.warning(f"levels = {cs.levels}")
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
                        cid = cs.get_gid()
                        bar.lines[-1].set_gid(cid)
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


class CurvePlot(PlotBase, _ConfigType):
    """Line plot."""
    names = PlotBase.names + ('curve', )

    curve_ = {'colors': 'black',
              'linewidths': 1.5,
              }

    def __init__(self, gid=None):
        gid = gid or 'curve'
        self._curve = {}
        self._curve.setdefault('gid', gid)

    def __call__(self,
                 fig,
                 axs,
                 data,
                 view: dict|None=None,
                 artists: list|None=None,
                 **kwds) -> list:
        """Batch plotter."""
        artists = artists or []
        xco = self.extract_plot_coords(data, 1)
        # print(xco)

        line = self.plot(axs, data)
        artists.extend(line)
        return artists

    def plot(self, axs, data,
             artists=None, bind=None, **kwds):
        """matplotlib plot wrapper."""
        artists = artists or []
        lp = axs.plot(data)
        artists.extend(lp)
        ret = artists
        if isinstance(bind, cabc.Callable):
            bind(artist=ret)
        return ret


def adapt_values(src, ref):
    """Convert data type corresponds to ones of array."""
    # print(f"{type(src)=} {type(ref)=}")
    try:
        dtsrc = src.dtype
        # print(f"{src.dtype=}")
    except AttributeError:
        dtsrc = False
        # print("no src.dtype")
    try:
        dtref = ref.dtype
        # print(f"{ref.dtype=}")
    except AttributeError:
        dtref = False
        # print("no ref.dtype")
    if dtref:
        if dtsrc:
            v = src.astype(dtref)
            # print(f"{type(v)=}")
            # return v
            if isinstance(v, xr.DataArray):
                # print(mplib.dates.date2num(v.values))
                # print(v.values, type(v.values), v.item())
                return v.values
            else:
                return v
        elif np.issubdtype(dtref, np.datetime64):
            return pandas.to_datetime(src)
        else:
            dtref = ref.values.flat[0]
            if isinstance(dtref, cftime.datetime):
                cal = dtref.calendar
                if isinstance(src, str):
                    src = pandas.Timestamp(src)
                    src = cftime.to_tuple(src)
                    return cftime.datetime(*src, calendar=cal)
                elif not isinstance(src, cftime.datetime):
                    return cftime.num2date(src, units=zu.NC_EPOCH,
                                           calendar=cal)
    return src

def date_normalize(v, ref):
    if isinstance(v, xr.DataArray):
        # print(v.dtype, np.issubdtype(v.dtype, np.datetime64))
        if not np.issubdtype(v.dtype, np.datetime64):
            vt = v.item()
        else:
            return v
    else:
        vt = v
    # print(f"{type(vt)=} {vt=} {v}")
    if isinstance(ref, np.datetime64):
        if isinstance(vt, str):
            v = pandas.Timestamp(v)
        elif not isinstance(vt, np.datetime64):
            v = np.datetime64(mplib.dates.num2date(v))
    elif isinstance(ref, cftime.datetime):
        cal = ref.calendar
        if isinstance(vt, str):
            pts = pandas.Timestamp(v)
            cft = cftime.to_tuple(pts)
            v = cftime.datetime(*cft, calendar=cal)
        elif not isinstance(vt, cftime.datetime):
            v = cftime.num2date(v, units=zu.NC_EPOCH, calendar=cal)
    return v


def main(args):
    """Test driver."""
    import tomllib as toml
    import zbt.xrnio as zxr
    import matplotlib.animation as animation

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
    # fig1, lay1 = Pic(reset=True)
    # fig2, lay2 = Pic(reset=True)

    # print(fig1, lay1)
    # print(lay1['body'])
    # lay1.reset(fig1)
    # lay2.reset(fig2)

    ### layout demo
    if False:
        lay1['body'].plot([0, 1, 2, 3], [3, 2, 1, 0])
        lay2['body'].plot([0, 1, 2, 3], [0, 3, 1, 2])
        plt.show()

    ContourPlot.config(config, groups=('zbcont', 'test', ))

    ContourPlot.diag(strip=False)

    plot = ContourPlot()
    curve = CurvePlot()
    # print(args)
    for a in args:
        print(f"open: {a}")
        ds = zxr.open_dataset(a)
        for vk in ds.data_vars:
            vv = ds[vk]
            print(f"var: {vk} {vv.shape}")
            fig, axs = Pic(reset=True)
            artists = None
            labels = []
            for y in range(10):
                x1d = (0, ) * (len(vv.shape) - 2) + (y, )
                v1d = vv[x1d]
                zxr.diag(v1d)
                artists = curve(fig, axs, v1d, artists=artists,
                                label=f"{y}")
                labels.append(f"{y}")
            ppr.pprint(artists)
            leg = fig.legend(artists, labels, loc='center',
                             bbox_to_anchor=(0.5, 0.5))
            print(leg)
            ppr.pprint(leg.get_lines())

            if len(vv.shape) < 3:
                continue
            nz = vv.shape[-3]
            AA = []
            # for z in range(min(3, nz)):
            for z in []:
                fig, axs = Pic(reset=True)
                frames = []
                z = 0
                vmin = vv[:,z,...].min()
                vmax = vv[:,z,...].max()

                for r in range(vv.shape[0]):
                    zz = vv[r,z,...]
                    print(f"rec: {z} {r} {zz.shape}")
                    axs.reset(fig)
                    # axs.cla(fig)
                    aa = plot(fig, axs, zz, vmin=vmin, vmax=vmax)
                    frames.append(aa)
                    # print(aa)
                ani = animation.ArtistAnimation(
                    fig,
                    frames,
                    interval=30,
                    blit=False,  # blitting can't be used with Figure artists
                    repeat_delay=10,
                )
                AA.append(ani)
            # r = 0
            # zz = vv[r,z,...]
            # plot(fig, axs, zz, vmin=vmin, vmax=vmax)
            # fig.canvas.draw()

            plt.show()
    pass

__debug_artist = {}
def debug_artist(artist, tag=None):
    num = __debug_artist.setdefault(tag, 0)
    A = mart.ArtistInspector(artist)
    print(r'=' * 20 + (tag or '') + f" {num}")
    ppr.pprint(A.properties())
    ppr.pprint(artist.get_children())
    try:
        ppr.pprint(list(artist._children))
        ppr.pprint(dict(artist.spines.items()))
        ppr.pprint(list(artist.spines.values()))
        ppr.pprint(list(artist._axis_map.values()))
        ppr.pprint(list(artist.child_axes))
    except:
        pass
    print(r'=' * 20 + (tag or '') + f" {num} done")
    __debug_artist[tag] = __debug_artist[tag] + 1

if __name__ == '__main__':
    import sys
    main(sys.argv[1:])

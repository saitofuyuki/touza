#!/usr/bin/env python3
# Time-stamp: <2024/10/16 14:35:38 fuyuki plot.py>

__doc__ = \
    """
zbt.plot
~~~~~~~~
Plotter collections.

:Source:     zbt/plot.py
:Maintainer: SAITO Fuyuki <saitofuyuki@jamstec.go.jp>
:Created:    Oct 9 2024
"""

import collections.abc as cabc
import numbers as nums

import matplotlib as mplib
import matplotlib.ticker as mtc
import matplotlib.pyplot as plt
import matplotlib.backend_tools as mpbt

import xarray.plot.utils as xrpu

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

    def __init__(self, figsize, layout=None, **kw):
        self.figsize = figsize
        self.layout = layout or "none"
        self.figkw = kw

        self.ax = None
        self.cax = None
        self.tax = None

    def __call__(self, cls=None, axs=None, **kw):
        """Create figure and axes."""
        cls = cls or FigureBase
        if cls is True:
            cls = None
        fig = plt.figure(figsize=self.figsize, layout=self.layout,
                         FigureClass=cls, **self.figkw)

        # fig.canvas.manager.toolmanager.add_tool('TEST00', TestTools)

        axs = axs or self
        return fig, axs

    def reset(self, fig, axs=None):
        """Dummy procedure to reset figure."""

    def parse_coor(self, coor):
        """Fallback coordinate limit parser"""
        dmax = coor[-1]
        dmin = coor[0]
        return (dmin, dmax)

    def parse_ticks(self, _, default=0):
        """Fall back major and minor locators."""
        major = default
        minor = default
        return (major, minor)

    def set_ticks(self, x, y, axs=None, major=None, minor=None):
        """Set tick labels."""
        axs = axs or self
        major = major or {}
        minor = minor or {}
        for co, axis, limf in [(x, axs.ax.xaxis, axs.ax.set_xlim),
                               (y, axs.ax.yaxis, axs.ax.set_ylim)]:
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

    def cbar_set_ticks(self, axs=None,
                       major=None, minor=None):
        """Set tick labels of colorbar."""
        axs = axs or self
        major = major or {}
        minor = minor or {}
        axs.cax.tick_params(which='major', **major)
        axs.cax.tick_params(which='minor', **minor)


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

    def parse_coor(self, coor):
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
            vmin, vmax = super().parse_coor(coor)
            dmax = float(dmax) if dmax else vmax
            dmin = float(dmin) if dmin else vmin
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

    pos = {'bottom': True, 'top': True, 'right': True, 'left': True, }
    major = {'length': 10.8, 'width': 1, 'pad': 5.8, 'labelsize': 14, }
    minor = {'length': 5.4, 'width': 0.7, }
    major.update(pos)
    minor.update(pos)

    cbar_major = {'bottom': True, 'length': 10.8,
                  'width': 1, 'pad': 5.8, 'labelsize': 14, }
    cbar_minor = False

    geometry = (10.45, 7.39)

    def __init__(self, *args,
                 figsize=None, layout=None, **kw):
        figsize = figsize or self.geometry
        # layout = layout or "none"
        layout = layout or "none"
        super().__init__(*args,
                         figsize=figsize, layout=layout,
                         **kw)

    def __call__(self, *args, figsize=None, **kw):
        """Create figure and standard axes."""
        fig, axs = super().__call__(*args, **kw)

        if figsize:
            if figsize is True:
                figsize = self.geometry
            elif hasattr(figsize, 'get_size_inches'):
                figsize = figsize.get_size_inches()
            fig.set_size_inches(figsize)

        self.reset(fig, axs)
        return fig, axs

    def reset(self, fig, axs=None, **kw):
        """Bind standard axes."""
        axs = axs or self

        fig.clf()

        axs.ax = fig.add_axes([0.129, 0.225, 0.830, 0.585],
                              label='body', **kw)
        axs.tax = fig.add_axes([0.129, 0.070, 0.830, 0.057],
                               label='info', )
        axs.cax = fig.add_axes([0.061, 0.070, 0.245, 0.057],
                               label='colorbar', )
        axs.tax.set_axis_off()

        return axs

    def set_ticks(self, *args, major=None, minor=None, **kw):
        """Set tick labels."""
        major = major or self.major
        minor = minor or self.minor
        super().set_ticks(*args, major=major, minor=minor, **kw)

    def cbar_set_ticks(self, axs=None,
                       major=None, minor=None):
        """Set colorbar ticks."""
        major = major or self.cbar_major
        minor = minor or self.cbar_minor
        super().cbar_set_ticks(axs=axs, major=major, minor=minor)

    def add_titles(self, data, axs=None, contours=None, **kw):
        """Add title."""
        axs = axs or self
        tkw = {'pad': 18.7,
               'linespacing': 1.3,
               'fontsize': 14, }
        tkw.update(kw)

        left = self.parse_titles(data.attrs, data.coords, default='')
        axs.ax.set_title('\n'.join(left), loc='left', **tkw)

        dset = self.parse_dataset(data.attrs, default='')
        cal = self.parse_calendar(data.attrs, default='')
        right = (dset, cal, '')

        axs.ax.set_title('\n'.join(right), loc='right', **tkw)

        if contours:
            dc = contours[0].levels[1:] - contours[0].levels[:-1]
            if len(set(dc)) == 1:
                ctext = f'CONTOUR INTERVAL = {dc[0]}'
                axs.tax.text(0.5, 0.5, ctext,
                             horizontalalignment='center',
                             verticalalignment='top',
                             fontsize=12)

    def colorbar(self, fig, *args, axs=None, orientation=None, **kw):
        """Wrap colorbar"""
        axs = axs or self
        orientation = orientation or 'horizontal'
        return fig.colorbar(*args, cax=axs.cax, orientation=orientation, **kw)


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

    def __init__(self,
                 limit=None,
                 common=None, contour=None, color=None,
                 colors_first=None,
                 method=None, cmap=None):
        self._common = common or {}
        self._contour = {'colors': 'black'}
        self._color = {}
        if isinstance(contour, str):
            contour = self.parse_levels(contour, single=False)
        self._contour['levels'] = contour or []
        if isinstance(color, str):
            color = self.parse_levels(color, single=True)
        self._color['levels'] = color or None
        # if isinstance(method, str):
        #     method, cmap = self.parse_method(method, cmap)
        # print(cmap)
        self._color['method'] = method
        self._color['cmap'] = cmap
        self.colors_first = colors_first

        limit = limit or (None, None)
        if limit[0] is not None:
            self._common['vmin'] = limit[0]
        if limit[1] is not None:
            self._common['vmax'] = limit[1]

        # print(self._color)

    def __call__(self, fig, axs, data, title=None, **kwds):
        """Batch plotter."""
        if any(w <= 1 for w in data.shape):
            raise UserWarning("virtually less than two dimensions")

        axs.reset(fig)
        coords = data.coords
        # cj = [d for d, c in coords.items() if c.size > 1]
        cj = [d for d in data.dims if coords[d].size > 1]
        if len(cj) != 2:
            raise ValueError(f"Not 2d shape={cj}")
        xco = coords[cj[1]]
        yco = coords[cj[0]]
        # print(xco)
        axs.set_ticks(x=xco, y=yco)
        axs.cbar_set_ticks()

        common = self._common
        contour = self._contour
        color = self._color

        if self.colors_first:
            col, cbr = self.color(fig, axs, data, **common, **color)
            con = self.contour(axs, data, **common, **contour)
        else:
            con = self.contour(axs, data, **common, **contour)
            col, cbr = self.color(fig, axs, data, **common, **color)

        if cbr:
            for c in con:
                cbr.add_lines(c, erase=False)
        else:
            bkw = {}
            cbr = fig.colorbar(con[0], **bkw)
            for c in con[1:]:
                cbr.add_lines(c, erase=False)

        axs.add_titles(data, contours=con)

        fig.canvas.draw()
        fig.canvas.flush_events()
        # Not yet working....
        if title:
            for a in ['set_window_title', 'setWindowTitle', ]:
                f = getattr(fig.canvas, a, None)
                if f:
                    f(f'Figure: {title}')
                    break

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
            pat = []
        elif text is True:
            pat = [True]
        else:
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

    def parse_method(self, method, cmap=None):
        # print(method, cmap)
        method = method or ''
        method = tuple(method.split('/'))
        if len(method) >= 2:
            method, cmap = method[:2]
        elif method:
            method = method[0]
        else:
            method = None
        # print(method, cmap)
        method = self.method_table.get(method, method)
        if method in self.method_table:
            pass
        elif cmap is None:
            if method in mplib.colormaps:
                method, cmap = None, method
        method = method or 'contourf'
        return method, cmap

    def contour(self, axs, data, levels=None, **kw):
        """matplotlib contour wrapper."""
        cc = []
        levels = levels or True
        if not isinstance(levels, cabc.Iterable):
            levels = [levels]
        elif isinstance(levels[0], nums.Number):
            if isinstance(levels[0], bool):
                pass
            else:
                levels = [levels]

        def run(idx, **args):
            return data.plot.contour(ax=axs.ax, **args,
                                     **(self.opts | kw))
        for j, lev in enumerate(levels):
            if lev is True:
                c = run(j)
            elif lev is False:
                continue
            elif isinstance(lev, list):
                c = run(j, levels=lev)
            elif isinstance(lev, cabc.Callable):
                vmin = kw.get('vmin')
                if vmin is None:
                    vmin = data.min().values
                vmax = kw.get('vmax')
                if vmax is None:
                    vmax = data.max().values
                c = run(j, levels=lev(vmin, vmax))
            else:
                raise TypeError(f"invalid level specifier {c}.")
            cc.append(c)
        return cc

    def color(self, fig, axs, data,
              method=None, cmap=None, levels=None, **kw):
        """matplotlib contour wrapper."""
        if method is None:
            method = True
        if method is True:
            method = self.color_method
        if method is False:
            col, cbr = None, None
        else:
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
                vmin = kw.get('vmin')
                if vmin is None:
                    vmin = data.min().values
                vmax = kw.get('vmax')
                if vmax is None:
                    vmax = data.max().values
                levels = levels(vmin, vmax)
            else:
                raise TypeError(f"invalid level specifier {levels}.")

            if levels is False:
                col, cbr = None, None
            else:
                col = func(ax=axs.ax, add_colorbar=False,
                           levels=levels, cmap=cmap,
                           **(self.opts | kw))
                cbr = axs.colorbar(fig, col)
        return (col, cbr)

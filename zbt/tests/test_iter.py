#!/usr/bin/env python3
# Time-stamp: <2024/10/04 22:23:04 fuyuki test_iter.py>

import sys
import math
import argparse as ap
import pathlib as plib
import cProfile
import tomllib as toml
import numbers as nums
import collections.abc as cabc

import cartopy
import cartopy.util
import numpy as np
import xarray as xr
import matplotlib as mplib
import matplotlib.pyplot as plt
import matplotlib.ticker as mtc
# import matplotlib.gridspec as mgs
# import mpl_toolkits.axes_grid1 as mag

from xarray.plot.utils import (
    _infer_xy_labels,
    label_from_attrs,
)

import zbt.util as zu


class LayoutBase(mplib.figure.Figure):
    """Base layer of figure layout."""

    def __init__(self, *args, **kw):
        super().__init__(*args, **kw)
        self.ax = None
        self.cax = None
        self.tax = None

    def reset(self):
        pass

    def set_ticks(self, array, ax=None, x=None, y=None,
                  major=None, minor=None, **kw):
        """Set tick labels."""
        x, y = _infer_xy_labels(array, x, y)
        ax = ax or self.ax
        major = major or {}
        minor = minor or {}
        for cj, axis in [(x, ax.xaxis),
                         (y, ax.yaxis)]:
            co = array.coords[cj]
            span = co[-1]-co[0]
            val = co.attrs.get('DIVL', 0)
            if span > val and val > 0:
                axis.set_major_locator(mtc.MultipleLocator(val))
            val = co.attrs.get('DIVS', 0)
            if val > 0:
                axis.set_minor_locator(mtc.MultipleLocator(val))
            axis.set_label_text(label_from_attrs(co), fontsize=13)
            axis.set_tick_params(which='major', **major)
            axis.set_tick_params(which='minor', **minor)

    def cbar_set_ticks(self, ax=None,
                       major=None, minor=None, **kw):
        """Set tick labels of colorbar."""
        ax = ax or self.cax
        major = major or {}
        minor = minor or {}
        ax.tick_params(which='major', **major)
        ax.tick_params(which='minor', **minor)

    def parse_date(self, attrs, key):
        """Parse date attribute to tuple."""
        dt = attrs.get(key, '').strip()
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


class LayoutLegacy3(LayoutBase):
    """gtcont layout 3 emulation."""

    pos = {'bottom': True, 'top': True, 'right': True, 'left': True, }
    major = {'length': 10.8, 'width': 1, 'pad': 5.8, 'labelsize': 14, }
    minor = {'length': 5.4, 'width': 0.7, }
    major.update(pos)
    minor.update(pos)

    cbar_major = {'bottom': True, 'length': 10.8,
                  'width': 1, 'pad': 5.8, 'labelsize': 14, }
    cbar_minor = False

    def __init__(self, *args,
                 figsize=None, layout=None, **kw):
        figsize = figsize or (10.45, 7.39)
        layout = layout or "none"
        super().__init__(*args,
                         figsize=figsize, layout=layout,
                         **kw)

    def reset(self):
        """Reset figure and create new axes."""
        self.clf()
        # akw = {'projection': cartopy.crs.Orthographic(-80, 35), }
        akw = {}

        self.ax = self.add_axes([0.129, 0.225, 0.830, 0.585], **akw)
        self.tax = self.add_axes([0.129, 0.070, 0.830, 0.057])
        self.cax = self.add_axes([0.061, 0.070, 0.245, 0.057])
        # self.ax.set_aspect(1)
        self.tax.set_axis_off()
        # self.ax.set_global()

        return self.ax, self.cax

    def set_ticks(self, array, ax=None, x=None, y=None,
                  major=None, minor=None, **kw):
        """Set tick labels."""
        major = major or self.major
        minor = minor or self.minor
        super().set_ticks(array, ax=ax, x=x, y=y,
                          major=major, minor=minor,
                          **kw)

    def cbar_set_ticks(self, ax=None, **kw):
        super().cbar_set_ticks(ax=ax,
                               major=self.cbar_major, minor=self.cbar_minor,
                               **kw)

    def add_titles(self, array, ax=None, contours=None, **kw):
        ax = ax or self.ax

        tkw = {'pad': 18.7,
               'linespacing': 1.3,
               'fontsize': 14, }
        tkw.update(kw)

        left = []
        left.append(array.attrs.get('long_name', ''))
        left.append(array.attrs.get('units', ''))
        left.append(array.attrs.get('ITEM', '')
                    + ' '
                    + ' '.join(array.attrs.get('ETTL', [''])))

        ax.set_title('\n'.join(left), loc='left', **tkw)

        right = []
        right.append(array.attrs.get('DSET', ''))
        dt0 = self.parse_date(array.attrs, 'DATE')
        dt1 = self.parse_date(array.attrs, 'DATE1')
        dt2 = self.parse_date(array.attrs, 'DATE2')
        if (dt0 != dt1 or dt1 != dt2 or dt2 != dt0) and dt1 and dt2:
            cal = '/'.join(dt1[:3]) + ' ' + ':'.join(dt1[3:6])
            cal = cal + '-'
            cal = cal + '/'.join(dt2[:3]) + ' ' + ':'.join(dt2[3:6])
        elif dt0:
            cal = '/'.join(dt0[:3]) + ' ' + ':'.join(dt0[3:6])
        else:
            cal = ''
        right.append(cal)
        right.append('')

        ax.set_title('\n'.join(right), loc='right', **tkw)

        if contours:
            dc = contours[0].levels[1:] - contours[0].levels[:-1]
            if len(set(dc)) == 1:
                ctext = f'CONTOUR INTERVAL = {dc[0]}'
                self.tax.text(0.5, 0.5, ctext,
                              horizontalalignment='center',
                              verticalalignment='top',
                              fontsize=12)

    def colorbar(self, *args, cax=None, orientation=None, **kw):
        """Wrap colorbar"""
        cax = cax or self.cax
        orientation = orientation or 'horizontal'
        return super().colorbar(*args, cax=cax, orientation=orientation, **kw)


class ArrayIter(zu.TreeIter):
    def __init__(self, array, coors=None, init=None, *args, **kwds):
        sh = array.shape
        colon = slice(None, None, None)
        # colon = zu.Selection(None, dim=sh)
        coors = coors or (-2, -1, )
        mask = (None, ) * len(sh)
        for c in coors:
            if c == -1:
                mask = mask[:c] + (colon, )
            else:
                mask = mask[:c] + (colon, ) + mask[c+1:]

        super().__init__(array, key=sh, mask=mask, name='A',
                         *args, **kwds)
        print(f'arrayiter: {init=}')
        self(init=init)

    def get_full(self, key):
        ret = super().get_full(key)
        return ret

    def update(self, key):
        print(f'update/a:{key=}:{self.key_full=}')
        # v = self.value()
        # v.plot()
        # plt.show()
        # print(f"array:update: f{key=} {self.value()=}")
        # print(f"array:update: {key=}")
        pass

class VariableIter(zu.TreeIter):
    """Variable iterator."""
    def __init__(self, *args, **kwds):
        super().__init__(*args, name='V', **kwds)

    def update(self, key):
        print(f'update/v:{key=}:{self.key_full=}')
        init = None
        if self.child:
            init = self.child.current
            print(f'update/v:{self.child.current=}')
        # k = keys[0]
        # k = self.l2p(keys)
        v = self[key]
        self.bind_child(ArrayIter, v, init=init)

    def get_full(self, key):
        # print('Var:', type(self[key[0]]).mro())
        return super().get_full(key)


class FileIter(zu.TreeIter):
    """File arguments iterator."""
    def __init__(self, files, *args, decode_coords=None, **kwds):
        self.files = files
        self.decode_coords = decode_coords
        nf = len(files)
        array = [None] * nf
        super().__init__(array=array, key=nf, *args, name='F', **kwds)

    def get_full(self, key):
        return super().get_full(key)

    def update(self, key):
        print(f'update/f:{key=}:{self.key_full=}')
        k = key[0]
        ds = self[key]
        if ds is None:
            f = self.files[k]
            ds = xr.open_dataset(f,
                                 decode_coords=self.decode_coords)
            # self.array[k] = ds
            self[k] = ds

        vkeys = [vn for vn in ds.data_vars]
        self.bind_child(VariableIter, ds, key=vkeys)


class FigureIter(zu.TreeIter):
    def __init__(self, files, plot, /,
                 layout=None, num=None,
                 *args, **kwds):
        layout = layout or LayoutLegacy3
        num = max((num or 0), 1)
        figs = [plt.figure(FigureClass=layout) for j in range(num)]
        super().__init__(array=figs, *args, name='T', **kwds)
        self.files = files
        self.bind_child(FileIter, files)
        self.plot = plot
        self.loop = None
        # figs[0].canvas.manager.show()
        # self.cid = self.fig.canvas.mpl_connect('key_press_event', self)

    def get_full(self, key):
        return super().get_full(key)

    def update(self, key):
        # k = keys[0]
        fig = self[key]
        fig.canvas.manager.show()
        self.cid = fig.canvas.mpl_connect('key_press_event', self.event_handler)

    def event_handler(self, event):
        """Event handler"""
        kev = event.key
        print(f"{kev=}")
        try:
            if kev in [' ', ]:
                # print(self.key_full())
                self.switch(reverse=False)
                self._draw()
                # tree = self.next_loop()
                # # tree = next(self.loop)
                # # print(tree)
                # stat = self.get_full(tree)
                # self.plot.draw(layout=stat[0], data=stat[-1])
            elif kev in ['backspace', ]:
                self.switch(reverse=True)
                self._draw()
            elif kev in ['v', ]:
                # print(self.key_full())
                self.child.child.child.switch(mask=True, recurse=False)
                print(f'event(v): {self.key_full=}')
                # self.loop = iter(self.iter_full(init=False))
                # print(self.next_loop())
                # print(self.key_full())
                # print(self)
                self._draw()
                # self.child.switch(mask=None)
            elif kev == 'q':
                return
        except StopIteration:
            print("No more")
            self.loop = None
            self._draw()

    def gen_loop(self):
        if self.loop is None:
            self.loop = iter(self.iter_full())
        return self.loop

    def next_loop(self):
        self.gen_loop()
        return next(self.loop)

    def _draw(self):
        fig = self.value()
        fig.canvas.mpl_disconnect(self.cid)
        tree = self.next_loop()
        print(f'draw: {tree=}')
        stat = self.get_full(tree)
        self.plot.draw(layout=stat[0], data=stat[-1])
        self.cid = stat[0].canvas.mpl_connect('key_press_event', self.event_handler)

    def draw(self):
        self._draw()
        # self.gen_loop()
        # self.loop = iter(self.iter_full())

class ContourPlot:
    isep = '/'
    lsep = ','
    nsep = ':'

    color_method = 'contourf'

    opts =  {'add_labels': False, }

    def draw(self, layout, data,
             common=None, contour=None, color=None, **kwds):

        layout.reset()
        layout.set_ticks(data)
        layout.cbar_set_ticks()

        common = common or {}
        contour = contour or {}
        color = color or {}

        con = self.contour(layout, data, **common, **contour)
        col = self.color(layout, data, **common, **color)
        layout.canvas.draw()
        layout.canvas.flush_events()

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
                    for nums in [int(jj) for jj in item.split(self.nsep) if jj]:
                        loc = mtc.MaxNLocator(nums + 1)
                        pat.append(loc.tick_values)
                elif item:
                    item = float(item)
                    if item > 0:
                        loc = mtc.MultipleLocator(item)
                        pat.append(loc.tick_values)
        if single:
            if len(pat) == 0:
                pat = None
            elif len(pat) == 1:
                pat = pat[0]
            else:
                raise ValueError(f"Non single level specification {text}.")
        return pat

    def contour(self, layout, data, levels=None, **kw):
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
            return data.plot.contour(ax=layout.ax, **args,
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

    def color(self, layout, data,
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

            col = func(ax=layout.ax, add_colorbar=False, **(self.opts | kw))
            cbr = layout.colorbar(col, **kw)
        return (col, cbr)


def main(argv, cmd=None):
    Plot = ContourPlot()
    Figs = FigureIter(argv, Plot)
    # print(Figs)
    for j, fig in Figs.items():
        print(j, fig)
        Figs.draw()
        # for tree in Figs.child.iter_full():
        #     print(tree)
        # x = iter(Figs.child.iter_full())
        # print(next(x))
        # print(next(x))

    # for tree in Figs.iter_full():
    #     objs = Figs.get_full(tree)
    #     fig = objs[0]
    #     data = objs[-1]
    #     print(tree)
    #     Plot.draw(layout=fig, data=data)

        # print(objs[-1][...])
    plt.show()


def _driver():
    """Command line driver."""
    main(sys.argv[1:], sys.argv[0])


if __name__ == '__main__':
    _driver()

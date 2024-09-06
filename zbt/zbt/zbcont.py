#!/usr/bin/env python3
# Time-stamp: <2024/09/10 09:47:00 fuyuki zbcont.py>

import sys
import math
import argparse as ap
import pathlib as plib
import cProfile

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


class Options(ap.Namespace):
    """Namespace to hold options"""

    method_table = {'f': 'contourf', 'c': 'contour',
                    'p': 'pcolormesh', 'i': 'imshow', }

    def __init__(self, argv, root=None):
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

        parser = ap.ArgumentParser(prog=plib.Path(root).name,
                                   epilog=epilog,
                                   formatter_class=ap.RawTextHelpFormatter)
        parser.add_argument('--no-decode_coords',
                            dest='decode_coords',
                            action='store_false',
                            help='skip auto coordinate inclusion')
        parser.add_argument('files', metavar='FILE[/SPEC]',
                            type=str,
                            nargs='+',
                            help='files, possibly with specifiers')
        parser.add_argument('-c', '--contours',
                            metavar='SPEC',
                            default=None,
                            type=str,
                            help='contour intervals or levels specification')
        parser.add_argument('-C', '--colors',
                            metavar='SPEC',
                            default=None,
                            type=str,
                            help='color intervals or levels specification.')
        parser.add_argument('-M', '--color-method',
                            metavar='METHOD/CMAP',
                            default=None,
                            type=str,
                            help='coloring method and map'
                            ' {contour(c) contourf(f) pcolormesh(p) imshow(i)}')
        parser.add_argument('-r', '--range',
                            metavar='[LOW][:[HIGH]]',
                            type=str,
                            help='data range to draw')
        parser.add_argument('-d', '--dim',
                            metavar='DIM,[[LOW]:[HIGH]]',
                            action='append',
                            type=str,
                            help='coordinate clipping')
        parser.add_argument('-v', '--variable',
                            metavar='VAR[,VAR...]',
                            default=[],
                            action='append',
                            type=str,
                            help='variable filter')
        parser.parse_args(argv, namespace=self)

        self.tweak()

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
            if len(dj) == 1:
                dim[dn] = slice(dj[0], dj[0]+1, None)
            else:
                dim[dn] = slice(dj[0], dj[1], None)
        self.dim = dim

        if self.range is not None:
            r = self.range + ':'
            r = r.split(':')
            self.range = ((float(r[0]) if r[0] != '' else None), )
            self.range = self.range + ((float(r[1]) if r[1] != '' else None), )
        else:
            self.range = None, None

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


class SliceStatus:
    """tuple of slices with bidirectional increment"""
    __slots__ = ("shape", "sel", "status", "array", "slices", )

    def __init__(self, array, dim=None, mask=0, back=False):
        self.array = array
        self.status = 0
        if array is None:
            self.shape = ()
            return

        shape = array.shape
        dim = dim or {}
        sel = {}
        nv = array.attrs.get('_nio_var')
        ndful = len(self.array.dims)
        ndeff = ndful
        if nv:
            if nv.recdim:
                ndeff = ndeff - 1
        for d, s in dim.items():
            for jd, dt in enumerate(self.array.dims):
                if dt == d or dt.startswith(d + '~'):
                    break
            else:
                d = d.upper()
                jd = None
                if d == 'X':
                    jd = -1
                elif d == 'Y':
                    if ndeff >= 2:
                        jd = -2
                elif d == 'Z':
                    if ndeff >= 3:
                        jd = -3
                elif d == 'T':
                    if ndeff < ndful:
                        jd = 0

                if jd is None:
                    continue
                dt = self.array.dims[jd]

            n = shape[jd]
            b = s.start or 0
            if b < 0:
                b = n + b
            e = s.stop or n
            if e < 0:
                e = n + e
            sel[dt] = b, e
        self.shape = ()
        self.slices = ()
        for jd, dt in enumerate(self.array.dims):
            if dt in sel:
                b, e = sel[dt]
                n = e - b
                s = slice(b, e)
            else:
                b = 0
                n = shape[jd]
                e = n
                s = slice(None, None)
            self.shape = self.shape + (n, )
            self.slices = self.slices + (s, )
        # print('slices:', self.slices)

        if back:
            self.sel = tuple(j - 1 for j in self.shape)
        else:
            self.sel = (0, ) * len(self.shape)

        if mask < 0:
            nfull = - mask
            jfull = 0
            sel = ()
            for n in reversed(self.shape):
                b = (n > 1) and jfull < nfull
                sel = (b, ) + sel
                if b:
                    jfull = jfull + 1
            self.sel = tuple(None if b else n for n, b in zip(self.sel, sel))
        elif mask > 0:
            nfull = mask
            jfull = 0
            sel = ()
            for n in self.shape:
                b = (n > 1) and jfull < nfull
                sel = sel + (b, )
                if not b:
                    jfull = jfull + 1
            self.sel = tuple(None if b else n for n, b in zip(self.sel, sel))

    def count_span(self):
        """Count active dimensions"""
        return self.sel.count(None)

    def wait(self):
        """Set condition to wait"""
        self.shape = ()

    def incr(self):
        """Increment selection.
        Return 0 if succeed or return number of calls
        after reaching the final."""
        if self.status == 0:
            sel = ()
            for j, h in zip(self.sel, self.shape):
                if j is None:
                    sel = sel + (None, )
                else:
                    j = j + 1
                    if j < h:
                        sel = sel + (j, )
                        rem = len(sel)
                        self.sel = sel + self.sel[rem:]
                        break
                    sel = sel + (0, )
            else:
                self.status = 1  # reached last
        else:
            self.status = self.status + 1
        return self.status

    def decr(self):
        """Increment selection.
        Return 0 if succeed or return negative number of calls
        after reaching the final."""
        if self.status == 0:
            sel = ()
            for j, h in zip(self.sel, self.shape):
                if j is None:
                    sel = sel + (None, )
                else:
                    j = j - 1
                    if j >= 0:
                        sel = sel + (j, )
                        rem = len(sel)
                        self.sel = sel + self.sel[rem:]
                        break
                    sel = sel + (h - 1, )
            else:
                self.status = -1   # reached last
        else:
            self.status = self.status - 1
        return self.status

    def is_wait(self):
        """Check if in the wait"""
        return not self.shape

    def is_loop(self):
        """Check if in the loop"""
        return self.status == 0

    def is_reach_last(self):
        """Check if exhausted at final end"""
        return self.status > 0

    def is_reach_first(self):
        """Check if exhausted at first end"""
        return self.status < 0

    def extract(self, var):
        """Slice extraction.  A workaround to update record attributes."""
        if self.status == 0:
            sel = ()
            for s, sl in zip(self.sel, self.slices):
                if s is None:
                    sel = sel + (sl, )
                else:
                    sel = sel + (s + (sl.start or 0), )
            # print(sel)
            # sel = tuple(s if s is not None else slice(None, None)
            #             for s in self.sel)
            attrs = {}
            nv = var.attrs.get('_nio_var')
            if nv:
                # assume record dimension is the first
                if nv.recdim:
                    rec = sel[0]
                else:
                    rec = None
                for a, ai in nv.attrs():
                    av = nv.getattr(a, rec=rec).strip()
                    ai = zu.tostr(ai)
                    if av:
                        attrs[ai] = av
                for ak in ['ETTL', 'EDIT', ]:
                    aa = []
                    for a, ai in nv.attrs():
                        ai = zu.tostr(ai)
                        if ai.startswith(ak):
                            av = nv.getattr(a, rec=rec).strip()
                            if av:
                                aa.append(av)
                    if aa:
                        attrs[ak] = aa

                for ai in ['DIVL', 'DIVS', 'DMIN', 'DMAX', ]:
                    if ai in attrs:
                        attrs[ai] = float(attrs[ai])
                for src in [('units', 'UNIT'),
                            ('long_name', 'TITL1', 'TITL2'), ]:
                    dst = src[0]
                    av = ''
                    for i in src[1:]:
                        av = av + attrs.get(i, '')
                    if av:
                        attrs[dst] = av

            vsel = var[sel]
            # cdata = cartopy.util.add_cyclic_point(vsel)
            vsel.attrs.update(attrs)
            return vsel
        raise ValueError(f"Extraction out of bounds {self.status}")

    def pos_phys(self, var, dataset):
        """Get physical coordinate of the slice"""
        dims = var.dims
        coords = var.coords
        # print(self.slices)
        # print(dataset.dims)
        # print(dataset.coords)
        pos = ()
        for j, sel in enumerate(self.sel):
            if sel is None:
                continue
            d = dims[j]
            slc = self.slices[j]
            # print(sel, slc)
            if d in dataset.coords:
                sel = sel + (slc.start or 0)
                c = coords[d]
                p = c.data[sel]
                pos = pos + ((d, sel, p), )
            else:
                pos = pos + ((d, sel), )
        return pos

    def __str__(self):
        if self.status == 0:
            ret = []
            # print(self.sel)
            # print(self.slices)
            for s, sl in zip(self.sel, self.slices):
                b, e = sl.start, sl.stop
                if s is None:
                    p = (('' if b is None else str(b))
                         + ':'
                         + ('' if e is None else str(e)))
                else:
                    p = str(s + (b or 0))
                ret.append(p)
            return ','.join(ret)

        return f"{self.status:+d}"


class ContourPlot:
    """Contour plot with key-press control"""
    figdim = 2
    prompt = '>'
    bar_ratio = (20, 1)
    contour_linewidths = [1.0, 1.5, 2.0, ]

    def __init__(self,  opts):
        self.opts = opts
        self.files = opts.files
        self.dstab = [None] * len(self.files)
        self.jfile = 0
        self.jvar = None
        self.jsel = SliceStatus(None)
        self.vlist = None

        self.cid = None

        self.new_figure()

        self.update()

    def __call__(self, event):
        """Event handler"""
        # print(event.key, self.jfile, self.jvar, self.jsel)
        kev = event.key
        if kev == 'q':
            plt.close()
            return

        if kev == 'ctrl+down':
            self.jfile = self.jfile - 1
            self.jvar = None
            self.update()
        elif kev == 'ctrl+up':
            self.jfile = self.jfile + 1
            self.jvar = None
            self.update()
        elif kev == 'down':
            if self.jvar is None:
                self.jfile = self.jfile - 1
            else:
                self.jvar = self.jvar - 1
            self.jsel.wait()
            self.update()
        elif kev == 'up':
            if self.jvar is None:
                self.jfile = self.jfile + 1
            else:
                self.jvar = self.jvar + 1
            self.jsel.wait()
            self.update()
        elif kev in [' ', 'shift+up', ]:
            self.jsel.incr()
            self.jfile = max(0, self.jfile)
            self.update()
        elif kev == 'shift+down':
            self.jsel.decr()
            self.update()
        elif kev == 'D':
            self.new_figure()
            self.update()

    def new_figure(self):
        """Allocate new figure."""
        # if self.cid:
        #     self.fig.canvas.flush_events()
        self.fig = plt.figure(FigureClass=LayoutLegacy3)
        self.fig.canvas.manager.show()
        self.cid = self.fig.canvas.mpl_connect('key_press_event', self)

    def update(self):
        """Update plot"""
        while True:
            if self.jsel.status < -1 or self.jsel.status > +1:
                plt.close()
                return
            if self.jfile == -1:
                print("Reach first file.  Backword again to quit.")
                self.jsel.decr()
                return
            if self.jfile == len(self.files):
                print("Reach last file.   Forward again to quit.")
                self.jsel.incr()
                return
            if self.jfile > len(self.files) or self.jfile < 0:
                plt.close()
                return
            ds = self.dstab[self.jfile]
            f = self.files[self.jfile]
            if ds is None:
                ds = xr.open_dataset(f, decode_coords=self.opts.decode_coords)
                self.dstab[self.jfile] = ds

            if self.jvar is None:
                self.jvar = 0
                # self.vlist = list(ds.data_vars.items())
                self.vlist = self.filter_variables(ds, self.opts.variable)
                if not self.vlist:
                    print(f"{f}: no effective variables")
                    self.jvar = None
                    self.jfile = self.jfile + 1
                    continue
            elif self.jvar >= len(self.vlist):
                self.jvar = None
                self.jfile = self.jfile + 1
                continue
            elif self.jvar < 0:
                self.jvar = None
                self.jfile = self.jfile - 1
                continue

            vk, vv = self.vlist[self.jvar]
            # if self.jsel.is_wait():
            #     pass
            if self.jsel.is_wait():
                pass
            elif self.jsel.is_loop():
                break
            elif self.jsel.is_reach_last():
                self.jvar = self.jvar + 1
                self.jsel.wait()
                continue
            elif self.jsel.is_reach_first():
                self.jvar = self.jvar - 1
                self.jsel.wait()
                continue

            self.jsel = SliceStatus(vv,
                                    dim=self.opts.dim,
                                    mask=-self.figdim,
                                    back=self.jsel.status < 0)
            if self.jsel.count_span() < self.figdim:
                print(f"{f}/{vk}: virtually less than two dimensions")
                self.jvar = self.jvar + 1
                self.jsel.wait()
                continue
            break

        pos = []
        for p in self.jsel.pos_phys(vv, ds):
            s = f"{p[0]}[{p[1]}]"
            if len(p) > 2:
                s = s + f"={p[2]}"
            pos.append(s)
        pos = ' '.join(pos)
        print(f"\rplot: {vk}[{self.jsel}] <{pos}>")

        self.fig.reset()
        xv = self.jsel.extract(vv)
        self.fig.set_ticks(xv)
        self.fig.cbar_set_ticks()

        vkw = {'add_labels': False, }
        # vkw = {}
        if self.opts.range[0] is not None:
            vkw['vmin'] = self.opts.range[0]
        if self.opts.range[1] is not None:
            vkw['vmax'] = self.opts.range[1]
        # vkw['transform'] = cartopy.crs.PlateCarree()

        if self.opts.colors_first:
            col, cbr = self.color(self.opts.colors, xv,
                                  self.opts.color_method, self.opts.cmap,
                                  **vkw)
            con = self.contour(self.opts.contours, xv, colors='k', **vkw)
        else:
            con = self.contour(self.opts.contours, xv, colors='k', **vkw)
            col, cbr = self.color(self.opts.colors, xv,
                                  self.opts.color_method, self.opts.cmap,
                                  **vkw)
        if cbr:
            for c in con:
                cbr.add_lines(c, erase=False)
        else:
            bkw = {}
            cbr = self.fig.colorbar(con[0], **bkw)
            for c in con[1:]:
                cbr.add_lines(c, erase=False)

        self.fig.add_titles(xv, contours=con)

        # self.fig.ax.coastlines()
        # self.fig.ax.gridlines()

        self.fig.canvas.draw()
        self.fig.canvas.flush_events()
        # wpx = int(self.fig.get_figwidth()  * self.fig.get_dpi())
        # hpx = int(self.fig.get_figheight() * self.fig.get_dpi())
        # print(f'figure size: {wpx} x {hpx} [px]')
        print(self.prompt, end=' ', flush=True)

    # Notes: xarray.plot.contour(f) and locator
    #    Since locator argument is not passed to matplotlib in xarray,
    #    a different approach from contour().is needed.
    def filter_variables(self, ds, variables):
        if variables is True:
            return list(ds.data_vars.items())
        vlist = []
        for v in variables:
            for vn, vv in ds.data_vars.items():
                if vn == v:
                    vlist.append((vn, vv))
        return vlist

    def contour(self, opts, array, **vkw):
        """matplotlib contour wrapper."""
        # --contours=0              no contour
        # --contours=INT[/INT...]   contour intervals
        # --contours=LEV,[LEV,...]  explicit contour levels
        # --contours=NUM:[STEP]     total number of contour lines
        cc = []
        if opts is False:
            pass
        elif opts is None:
            c = array.plot.contour(ax=self.fig.ax, **vkw)
            self.fig.ax.clabel(c)
            cc.append(c)
        else:
            isep = '/'
            lsep = ','
            nsep = ':'

            for j, item in enumerate(opts.split(isep)):
                jw = min(j, len(self.contour_linewidths) - 1)
                w = self.contour_linewidths[jw]
                step = 0
                kw = {}
                if lsep in item:
                    kw['levels'] = [float(jj) for jj in item.split(lsep) if jj]
                elif nsep in item:
                    nums = [int(jj) for jj in item.split(nsep) if jj]
                    loc = mtc.MaxNLocator(nums[0] + 1)
                    vmin = vkw.get('vmin')
                    if vmin is None:
                        vmin = array.min().values
                    vmax = vkw.get('vmax')
                    if vmax is None:
                        vmax = array.max().values
                    kw['levels'] = loc.tick_values(vmin, vmax)
                    # kw['locator'] = mtc.MaxNLocator(nums[0] + 1)
                    if len(nums) > 1:
                        step = nums[1]
                elif item:
                    item = float(item)
                    if item > 0:
                        loc = mtc.MultipleLocator(item)
                        vmin = vkw.get('vmin')
                        if vmin is None:
                            vmin = array.min().values
                        vmax = vkw.get('vmax')
                        if vmax is None:
                            vmax = array.max().values
                        kw['levels'] = loc.tick_values(vmin, vmax)
                    else:
                        continue
                    # kw['locator'] = mtc.MultipleLocator(item)

                c = array.plot.contour(ax=self.fig.ax, linewidths=w, **vkw, **kw)
                self.fig.ax.clabel(c)
                cc.append(c)
                # print(c.levels)
                if step > 1:
                    jw2 = min(jw + 1, len(self.contour_linewidths) - 1)
                    w2 = self.contour_linewidths[jw2]
                    c = array.plot.contour(ax=self.fig.ax, levels=c.levels[::step],
                                           linewidths=w2)
                    cc.append(c)
        return cc

    def color(self, opts, array, method=None, cmap=None, **vkw):
        """matplotlib contour wrapper."""
        # --colors=0                 no fill
        # --colors=INT               intervals
        # --colors=LEV,[LEV,...]     explicit levels
        # --colors=NUM:              total number of colors
        kw = {}
        if cmap:
            kw['cmap'] = cmap
        item = opts
        # print(item)
        if item is None:
            kw['levels'] = None
        else:
            lsep = ','
            nsep = ':'
            if lsep in item:
                kw['levels'] = [float(jj) for jj in item.split(lsep) if jj]
            elif nsep in item:
                nums = [int(jj) for jj in item.split(nsep) if jj]
                loc = mtc.MaxNLocator(nums[0] + 1)
                vmin = vkw.get('vmin')
                if vmin is None:
                    vmin = array.min().values
                vmax = vkw.get('vmax')
                if vmax is None:
                    vmax = array.max().values
                kw['levels'] = loc.tick_values(vmin, vmax)
            else:
                item = float(item)
                if item > 0:
                    loc = mtc.MultipleLocator(item)
                    vmin = vkw.get('vmin')
                    if vmin is None:
                        vmin = array.min().values
                    vmax = vkw.get('vmax')
                    if vmax is None:
                        vmax = array.max().values
                    kw['levels'] = loc.tick_values(vmin, vmax)
                else:
                    return None, None
        func = getattr(array.plot, method, None)
        if func is None:
            raise ValueError(f"invalid method {method}.")
        if method == 'contour':
            vkw['linewidths'] = 2
        col = func(ax=self.fig.ax, add_colorbar=False, **vkw, **kw)
        bkw = {}
        cbr = self.fig.colorbar(col, **bkw)

        return (col, cbr)


# def num_or_list(arg, array, lsep=None, isep=None):
#     """Parse arg as number if single or list if with separator."""
#     if arg is None:
#         return None
#     lsep = lsep or ','
#     if lsep in arg:
#         return [int(j) for j in arg.split(lsep)]
#     isep = isep or '/'
#     if isep in arg:
#         low, high = array.min().values, array.max().values
#         ret = []
#         for ci in arg.split(isep):
#             if ci:
#                 ci = float(ci)
#                 le = math.floor(low / ci) * ci
#                 he = math.ceil(high / ci) * ci
#                 ret.append(np.arange(le, he, ci))
#         return ret
#     return int(arg)


def main(argv, root=None):
    """Contour plot with TOUZA/Zbt."""
    # opts = parse_arguments(argv, root)
    opts = Options(argv, root)

    plt.rcParams.update({'figure.autolayout': True})
    ContourPlot(opts)
    # plt.savefig('test.pdf')
    plt.show()


def _driver():
    """Command line driver."""
    main(sys.argv[1:], sys.argv[0])
    # with cProfile.Profile() as pr:
    #     main(sys.argv[1:], sys.argv[0])
    #     pr.print_stats()


if __name__ == '__main__':
    _driver()

#!/usr/bin/env python3
# Time-stamp: <2024/09/04 08:53:56 fuyuki zbcont.py>

import sys
import math
import numpy as np
import argparse as ap
import xarray as xr
import cProfile
import matplotlib as mplib
import matplotlib.pyplot as plt
import matplotlib.ticker as mtc
import matplotlib.gridspec as mgs
import mpl_toolkits.axes_grid1 as mag
import zbt.util as zu
import pathlib as plib

class SliceStatus:
    """tuple of slices with bidirectional increment"""
    __slots__ = ("shape", "sel", "status", )

    def __init__(self, shape, mask=0, back=False):
        self.shape = shape or ()
        self.status = 0

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
            sel = tuple(s if s is not None else slice(None, None)
                        for s in self.sel)
            # print(sel)
            nv = var.attrs.get('_nio_var')
            # assume record dimension is the first
            if nv.recdim:
                rec = sel[0]
            else:
                rec = None
            attrs = {}
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
            vsel.attrs.update(attrs)
            return vsel
        raise ValueError(f"Extraction out of bounds {self.status}")

    def pos_phys(self, var, dataset):
        """Get physical coordinate of the slice"""
        dims = var.dims
        coords = var.coords
        # print(dataset.dims)
        # print(dataset.coords)
        pos = ()
        for j, sel in enumerate(self.sel):
            if sel is None:
                continue
            d = dims[j]
            if d in dataset.coords:
                c = coords[d]
                pos = pos + ((d, sel, c.data[sel]), )
            else:
                pos = pos + ((d, sel), )
        return pos

    def __str__(self):
        if self.status == 0:
            return ','.join(str(s) if s is not None else ':'
                            for s in self.sel)
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
        if self.cid:
            self.fig.canvas.flush_events()
            # self.fig.canvas.mpl_disconnect(self.cid)

        self.fig = plt.figure(figsize=[9.6, 4.8])
        # self.gs = mgs.GridSpec(1, 2, width_ratios=self.bar_ratio)

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
                self.vlist = list(ds.data_vars.items())
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

            self.jsel = SliceStatus(vv.shape, mask=-self.figdim,
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

        self.fig.clf()
        self.ax = self.fig.add_subplot()
        self.ax.set_aspect(1)

        xv = self.jsel.extract(vv)
        self.set_ticks(xv.dims, xv.coords)

        vkw = {}
        if self.opts.range is not None:
            r = self.opts.range + ','
            r = r.split(',')
            if r[0]:
                vkw['vmin'] = float(r[0])
            if r[1]:
                vkw['vmax'] = float(r[1])

        self.color(self.opts.colors, xv, self.opts.color_method, **vkw)
        self.contour(self.opts.contours, xv, colors='k', **vkw)
        self.add_titles(xv, self.opts)

        # self.ax.set_title('Left title\n' '123', loc='left')

        self.fig.canvas.draw()
        self.fig.canvas.flush_events()
        # wpx = int(self.fig.get_figwidth()  * self.fig.get_dpi())
        # hpx = int(self.fig.get_figheight() * self.fig.get_dpi())
        # print(f'figure size: {wpx} x {hpx} [px]')
        print(self.prompt, end=' ', flush=True)

    def add_titles(self, vv, opts, **kw):
        """annotation"""
        # layout 3 of gtool
        left = []
        left.append(vv.attrs.get('long_name', ''))
        left.append(vv.attrs.get('units', ''))
        left.append(vv.attrs.get('ITEM', '')
                    + ' '
                    + ' '.join(vv.attrs.get('ETTL', [''])))

        self.ax.set_title('\n'.join(left), loc='left')

        right = []
        right.append(vv.attrs.get('DSET', ''))
        dt0 = self.parse_date(vv.attrs, 'DATE')
        dt1 = self.parse_date(vv.attrs, 'DATE1')
        dt2 = self.parse_date(vv.attrs, 'DATE2')
        if (dt0 != dt1 or dt1 != dt2 or dt2 != dt0) and dt1 and dt2:
            cal = '/'.join(dt1[:3]) + ' ' + ':'.join(dt1[3:6])
            cal = cal + '-'
            cal = cal + '/'.join(dt2[:3]) + ' ' + ':'.join(dt2[3:6])
        else:
            cal = '/'.join(dt0[:3]) + ' ' + ':'.join(dt0[3:6])
        right.append(cal)
        right.append('')

        self.ax.set_title('\n'.join(right), loc='right')

    def parse_date(self, attrs, key):
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
        else:
            return None

    def contour(self, opts, array, **vkw):
        """matplotlib contour wrapper."""
        # --contours=0              no contour
        # --contours=INT[/INT...]   contour intervals
        # --contours=LEV,[LEV,...]  explicit contour levels
        # --contours=NUM:[STEP]     total number of contour lines
        if opts is None:
            c = array.plot.contour(ax=self.ax, **vkw)
            self.ax.clabel(c)
            return
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
                kw['locator'] = mtc.MaxNLocator(nums[0] + 1)
                step = nums[1]
            else:
                item = float(item)
                if item <= 0:
                    continue
                kw['locator'] = mtc.MultipleLocator(item)

            c = array.plot.contour(ax=self.ax, linewidths=w, **vkw, **kw)
            self.ax.clabel(c)
            print(c.levels)
            if step > 1:
                jw2 = min(jw + 1, len(self.contour_linewidths) - 1)
                w2 = self.contour_linewidths[jw2]
                c = array.plot.contour(ax=self.ax, levels=c.levels[::step],
                                       linewidths=w2)

    def color(self, opts, array, method=None, **vkw):
        """matplotlib contour wrapper."""
        # --colors=0                 no fill
        # --colors=INT               intervals
        # --colors=LEV,[LEV,...]     explicit levels
        # --colors=NUM:              total number of colors

        # Notes: xarray.plot.contourf and locator
        #    Since locator argument is not passed to matplotlib in xarray,
        #    a different approach from contour().is needed.

        method = method or 'contourf'

        kw = {}
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
                vmin = vkw.get('vmin') or array.min().values
                vmax = vkw.get('vmax') or array.max().values
                kw['levels'] = loc.tick_values(vmin, vmax)
            else:
                item = float(item)
                if item > 0:
                    loc = mtc.MultipleLocator(item)
                    vmin = vkw.get('vmin') or array.min().values
                    vmax = vkw.get('vmax') or array.max().values
                    kw['levels'] = loc.tick_values(vmin, vmax)
                else:
                    return
        func = getattr(array.plot, method, None)
        if func is None:
            raise ValueError(f"invalid method {method}.")
        c = func(ax=self.ax, add_colorbar=True,
                 cbar_kwargs={'shrink': 0.6},
                 **vkw, **kw)
        # c = array.plot.contourf(ax=self.ax, add_colorbar=True,
        #                         cbar_kwargs={'shrink': 0.6},
        #                         **vkw, **kw)

    def set_ticks(self, dims, coords):
        for c, f in [(-1, self.ax.set_xticks),
                     (-2, self.ax.set_yticks)]:
            x = coords[dims[c]]
            l = x.data[0]
            h = x.data[-1]
            if l > h:
                l, h = h, l
            v = x.attrs.get('DIVL')
            if v:
                l = math.floor(l / v) * v
                h = math.ceil(h / v) * v
                f(np.arange(l, h + v, v))
            v = x.attrs.get('DIVS')
            if v:
                l = math.floor(l / v) * v
                h = math.ceil(h / v) * v
                f(np.arange(l, h + v, v), minor=True)

def num_or_list(arg, array, lsep=None, isep=None):
    """Parse arg as number if single or list if with separator."""
    if arg is None:
        return None
    lsep = lsep or ','
    if lsep in arg:
        return [int(j) for j in arg.split(lsep)]
    isep = isep or '/'
    if isep in arg:
        low, high = array.min().values, array.max().values
        ret = []
        for ci in arg.split(isep):
            if ci:
                ci = float(ci)
                le = math.floor(low / ci) * ci
                he = math.ceil(high / ci) * ci
                ret.append(np.arange(le, he, ci))
        return ret
    return int(arg)

def main(argv, root=None):
    """Contour plot with TOUZA/Zbt."""
    opts = parse_arguments(argv, root)

    plt.rcParams.update({'figure.autolayout': True})
    cp = ContourPlot(opts)
    plt.show()

def parse_arguments(argv, root=None):
    """Command line parser."""
    parser = ap.ArgumentParser(prog=plib.Path(root).name)
    parser.add_argument('--no-decode_coords',
                        dest='decode_coords',
                        action='store_false',
                        help='skip auto coordinate inclusion')
    parser.add_argument('files', metavar='FILE[/SPEC]',
                        type=str,
                        nargs='+',
                        help='files, possibly with specifiers')
    parser.add_argument('--contours',
                        metavar='INTERVAL[/...]|LEVEL,[...]|NUMBER:[STEP]',
                        type=str,
                        help='list of contour intervals, '
                             'list of specified levels, '
                             'or number of contour levels')
    parser.add_argument('--colors',
                        metavar='INTERVAL|LEVEL,[...]|NUMBER:',
                        type=str,
                        help='list of color intervals, '
                             'list of specified levels, '
                             'or number of color levels')
    parser.add_argument('--color-method',
                        metavar='METHOD',
                        choices=['contourf', 'pcolormesh', 'imshow'],
                        help='coloring method')

    parser.add_argument('--range',
                        metavar='[LOW][,[HIGH]]',
                        type=str,
                        help='data range to draw')
    return parser.parse_args(argv)


def _driver():
    """Command line driver."""
    main(sys.argv[1:], sys.argv[0])
    # with cProfile.Profile() as pr:
        # main(sys.argv[1:], sys.argv[0])
        # pr.print_stats()

if __name__ == '__main__':
    _driver()

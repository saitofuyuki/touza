#!/usr/bin/env python3
# Time-stamp: <2024/08/13 11:28:57 fuyuki zbcont.py>

import sys
import math
import numpy as np
import argparse as ap
import xarray as xr
import matplotlib.pyplot as plt

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
        """Slice extraction"""
        if self.status == 0:
            sel = tuple(s if s is not None else slice(None, None)
                        for s in self.sel)
            return var[sel]
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
        # fig, ax = plt.subplots()
        # cax = ax.inset_axes([1.03, 0, 0.05, 1], transform=ax.transAxes)

        # self.cid = fig.canvas.mpl_connect('key_press_event', self)
        # self.fig = fig
        # self.ax = ax
        # self.cax = cax

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

        self.fig, self.ax = plt.subplots()
        self.fig.canvas.manager.show()
        self.cax = self.ax.inset_axes([1.03, 0, 0.05, 1],

                                      transform=self.ax.transAxes)
        self.cid = self.fig.canvas.mpl_connect('key_press_event', self)


    def update(self):
        """Update plot"""
        # print('status:', self.jsel.status, self.jsel.sel, self.jvar, self.jfile)
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
        self.set_ticks(vv.dims, vv.coords)

        pos = []
        for p in self.jsel.pos_phys(vv, ds):
            s = f"{p[0]}[{p[1]}]"
            if len(p) > 2:
                s = s + f"={p[2]}"
            pos.append(s)
        pos = ' '.join(pos)
        print(f"# plot: {vk}[{self.jsel}] <{pos}>")
        # print(vv.dims)
        # print(self.jsel.pos_phys(vv, ds))
        xv = self.jsel.extract(vv)
        # print(xv)
        xv.plot(ax=self.ax, cbar_ax=self.cax)
        self.fig.canvas.draw()

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

def main(argv, root=None):
    """Contour plot with TOUZA/Zbt."""
    opts = parse_arguments(argv, root)

    plt.rcParams.update({'figure.autolayout': True})
    cp = ContourPlot(opts)
    plt.show()

def parse_arguments(argv, root=None):
    """Command line parser."""
    parser = ap.ArgumentParser(prog=root)
    parser.add_argument('--no-decode_coords',
                        dest='decode_coords',
                        action='store_false',
                        help='skip auto coordinate inclusion')
    parser.add_argument('files', metavar='FILE[/SPEC]',
                        type=str,
                        nargs='+',
                        help='files, possibly with specifiers')
    return parser.parse_args(argv)


def _driver():
    """Command line driver."""
    main(sys.argv[1:], sys.argv[0])


if __name__ == '__main__':
    _driver()

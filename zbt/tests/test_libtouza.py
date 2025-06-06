#!/usr/bin/env python3
# Time-stamp: <2025/01/24 09:10:00 fuyuki test_libtouza.py>

import sys
import functools as ft
import operator as op
import ctypes as CT
import pathlib as plib
import numpy as np

sys.path.insert(0, str(plib.Path(__file__).parents[1]))

from zbt import libtouza as zlt
from zbt import util as zu


class Shape(tuple):
    def __new__(cls, *shape):
        return super(Shape, cls).__new__(cls, shape)

    def step(self, cur, step, cycle=False):
        ret = ()
        n = 0
        mw = min(map(len, [cur, step, self]))
        for j in reversed(range(mw)):
            c, s, w = cur[j], step[j], self[j]
            n, m = divmod(s + n, w)
            m = m + (c + w if c < 0 else c)
            r, m = divmod(m, w)
            n = n + r
            if c < 0:
                m = m - w
            ret = (m, ) + ret
        ret = ret + cur[mw:]
        if not cycle:
            if n > 0:
                raise OverflowError(f"[{n}] {cur} + {step} > {self}")
            if n < 0:
                raise OverflowError(f"[{n}] {cur} + {step} < 0 [{self}]")
        return ret

    def __call__(self, step, ini=None):
        if all(s == 0 for s in step):
            raise ValueError(f"invalid step {step}")
        zero = (0, ) * len(self)
        ini = ini or zero
        chk = tuple(w + i if i < 0 else i
                    for i, w in zip(ini, self))
        if chk < zero or chk >= self:
            return
        while True:
            try:
                yield ini
                ini = self.step(ini, step)
            except OverflowError:
                break

    def __str__(self):
       return f"Shape{super().__str__()}"


def TestShape():
    Sh = Shape(3, 4, 5)
    for step in [(0, 0, 1), (0, 0, 7), (0, 1), ]:
        off = (0, ) * len(Sh)
        ini = off
        while True:
            try:
                nxt = Sh.step(off, step)
                print(f"Shape[{Sh}] {off} + {step} = {nxt}")
                off = nxt
            except OverflowError as err:
                print(err)
                break
        for cur in Sh(step, ini):
            print(f"iter{step}:Shape[{Sh}] > {cur}")

    for step in [(0, 0, -1), (0, 0, -7), ]:
        off = tuple(w - 1 for w in Sh)
        while True:
            try:
                nxt = Sh.step(off, step)
                print(f"Shape[{Sh}] {off} + {step} = {nxt}")
                off = nxt
            except OverflowError as err:
                print(err)
                break

    for step in [(0, 0, -1), (0, 0, -7), ]:
        off = (-1, ) * len(Sh)
        while True:
            try:
                nxt = Sh.step(off, step)
                print(f"Shape[{Sh}] {off} + {step} = {nxt}")
                off = nxt
            except OverflowError as err:
                print(err)
                break


def main(argv):
    """Sample driver."""

    lib = zlt.LibTouzaNio(name=None)
    print(f"{zlt=}")
    print(f"{lib=}")

    def show_file(hfile, indent=None, diag=False, array=True, **kwds):
        """Show file information."""
        if diag:
            if diag is True:
                diag = 0
            lib.tnb_file_diag(hfile, diag)

        indent = indent or 0

        # hfile works as the suite group
        show_group(hfile, gidx=None, indent=indent + 2)
        ngrps = lib.tnb_file_groups(hfile)
        for gidx in range(ngrps):
            hgrp = lib.tnb_group(hfile, gidx)
            show_group(hgrp, gidx=gidx, indent=indent + 2,
                       array=array, **kwds)

    def show_group(hgrp, gidx=None, indent=None, **kwds):
        indent = indent or 0
        tab = ' ' * indent

        if gidx is None:
            print(f"{tab}suite[{hgrp}]")
        else:
            lg = lib.tnb_attr_len(' ', hgrp)
            gname = CT.create_string_buffer(lg + 1)
            lib.tnb_group_name(gname, hgrp)
            print(f"{tab}group[{gidx}/{hgrp}] {gname.value}")

        show_coors(hgrp, indent=indent + 2, **kwds)

        if gidx is not None:
            show_recs(hgrp, indent=indent + 2, **kwds)

        show_vars(hgrp, indent=indent + 2, **kwds)

    def show_coors(hgrp, indent=None, **kwds):
        indent = indent or 0
        tab = ' ' * indent
        ncos = lib.tnb_group_coors(hgrp)
        print(f"{tab}coors: {ncos}")

        for cidx in range(ncos):
            lc = lib.tnb_attr_len(' ', hgrp)
            coor = CT.create_string_buffer(lc + 1)
            lib.tnb_group_co_name(coor, hgrp, cidx)
            (jbgn, jend) = lib.group_co_range(hgrp, cidx)
            print(f"{tab}  coor<{cidx}>: {coor.value} {jbgn}:{jend}")

    def show_recs(hgrp, vidx=None, indent=None, **kwds):
        indent = indent or 0
        tab = ' ' * indent

        nrecs = lib.tnb_group_recs(hgrp)
        vidx = vidx or -1

        print(f"{tab}recs: {nrecs}")
        ld = lib.tnb_attr_len(' ', hgrp)
        lt = lib.tnb_attr_len(' ', hgrp)
        date = CT.create_string_buffer(ld + 1)
        time = CT.create_string_buffer(lt + 1)
        for rec in range(nrecs):
            lib.tnb_rec_time(time, hgrp, vidx, rec)
            lib.tnb_rec_date(date, hgrp, vidx, rec)
            print(f"{tab}  rec[{rec}]: {time.value} {date.value}")

    def show_vars(hgrp, indent=None, **kwds):
        indent = indent or 0
        tab = ' ' * indent

        nvars = lib.tnb_group_vars(hgrp)
        print(f"{tab}vars: {nvars}")
        for vidx in range(nvars):
            show_var(vidx, hgrp, indent=indent + 2, **kwds)

    def show_var(vidx, hgrp, indent=None, array=False, **kwds):
        indent = indent or 0
        tab = " " * indent
        lv = lib.tnb_attr_len(' ', hgrp, vidx)
        var = CT.create_string_buffer(lv + 1)
        lib.tnb_var_name(var, hgrp, vidx)
        nco = lib.tnb_co_size(hgrp, vidx)
        nrecs = lib.tnb_var_recs(hgrp, vidx)
        print(f"{tab}var[{vidx}]: {var.value}[{nrecs}]")
        print(f"{tab}  coors: {nco}")
        for cidx in range(nco):
            cmem = lib.tnb_co_len(hgrp, vidx, cidx)
            hco = lib.tnb_co_serial(hgrp, vidx, cidx)
            lc = lib.tnb_attr_len(' ', hgrp, vidx)
            coor = CT.create_string_buffer(lc + 1)
            lib.tnb_co_name(coor, hgrp, vidx, cidx)
            print(f"{tab}    coor[{cidx}]:"
                  f"<{hco}> {coor.value} {cmem}")
        if array:
            show_array(vidx, hgrp, indent + 2)

    def show_array(vidx, hgrp, indent=None, mdl=None, **kwds):
        indent = indent or 0
        tab = " " * indent

        mdl = mdl or 8
        nco = lib.tnb_co_size(hgrp, vidx)
        nrecs = lib.tnb_var_recs(hgrp, vidx)
        start = (0, ) * nco
        shape = Shape(*tuple(lib.tnb_co_len(hgrp, vidx, c)
                             for c in range(nco)))
        count = (1, ) * (nco - 1) + shape[-1:]
        step  = (0, ) * (nco - 2) + (1, )
        full = ft.reduce(op.mul, count, 1)

        print(f"{shape=} {count=} {step=}")
        # start = (CT.c_size_t * len(start))(*start)
        # count = (CT.c_size_t * len(count))(*count)

        ct = CT.c_double
        cbuf = (ct * full)()
        p = CT.byref(cbuf)
        p = CT.cast(p, CT.POINTER(ct))

        for r in range(nrecs):
            print(f"{tab}rec[{r}]:")
            show_attr(vidx, hgrp, r, indent + 2)
            pos = start
            for pos in shape(step, start):
                lib.tnb_var_read(p, r, pos, count, hgrp, vidx)
                print(f"{tab}  pos[{pos}]:")
                for j in range(0, full, mdl):
                    data = np.around(cbuf[j:min(j+mdl, full)], decimals=2)
                    print(f"{tab}    {data}")
            # while True:
            #     try:
            #         lib.tnb_var_read(p, r, pos, count, hgrp, vidx)
            #         print(f"{tab}  pos[{pos}]:")
            #         for j in range(0, full, mdl):
            #             data = np.around(cbuf[j:min(j+mdl, full)], decimals=2)
            #             print(f"{tab}    {data}")
            #         pos = shape.step(pos, step)
            #     except OverflowError:
            #         break

    def show_attr(vidx, hgrp, rec, indent=None):
        indent = indent or 0
        tab = " " * indent

        li = lib.tnb_attr_len(' ', hgrp, vidx)
        la = lib.tnb_attr_len(' ', hgrp, vidx)

        item = CT.create_string_buffer(li + 1)
        attr = CT.create_string_buffer(la + 1)

        for i in range(lib.tnb_attr_size(hgrp, vidx, rec)):
            i = i + 1
            lib.tnb_get_attr_name(item, i)
            lib.tnb_get_attr_byid(attr, i, hgrp, vidx, rec)
            if attr.value:
                print(f"{tab}attr[{item.value}]: {attr.value}")

    for a in argv:
        b = lib.tnb_file_is_nio(path=a)
        if not b:
            print(f"Not NIO format {b}: {a}")
            continue

        try:
            print(f"file: {a}")
            fh = lib.tnb_file_open(a)
            show_file(fh)
        except Exception as err:
            print(f"Unexpected {err=}, {type(err)=}")
            raise


if __name__ == '__main__':
    main(sys.argv[1:])

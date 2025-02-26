#!/usr/bin/env python3
# Time-stamp: <2025/02/22 21:27:28 fuyuki control.py>
#
# Copyright (C) 2024, 2025
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

__doc__ = \
    """
zbt.control
~~~~~~~~~~~
Figure traverse controller.

:Source:     zbt/control.py
:Maintainer:  SAITO Fuyuki <saitofuyuki@jamstec.go.jp>
:Created:    Oct 9 2024
"""

import sys
import pprint as ppr
import collections.abc as cabc
import pathlib as plib
import math
import copy
import time
import inspect
import functools as ft

import pandas as pd
import numpy as np
import xarray as xr
import matplotlib as mplib
import matplotlib.pyplot as plt
import matplotlib.artist as mart
import matplotlib.animation as animation
import matplotlib.backend_bases as mbb
import matplotlib.backend_tools as mbt
import mpl_toolkits.axes_grid1.inset_locator as m1i
import cftime

import cartopy.util as cutil

import zbt.plot as zplt
try:
    import zbt.xrnio as zxr
except ModuleNotFoundError:
    zxr = xr

import zbt.util as zu
import zbt.config as zcfg

locallog = zu.LocalAdapter(__name__)

_ConfigType = zcfg.ConfigRigid

class LinkedArray:
    """Helper class of recursive iteration."""

    def __init__(self, base, key=None, mask=None,
                 name=None, child=False):
        self.name = name
        self.child = child

        self.base = None
        self.shape = None
        self._keys = None
        self._l2i = None
        self.mask = False
        self.aux = None         # auxiliary properties

        # step:
        #   +1: forward
        #   0:  pin
        #   -1: backword
        self.step = +1

        # current
        #   None:  exhausted
        self._current = None
        self.recurse = True

        # init
        #   True:        natural
        #   False/None:  keep current unless None
        #   tuple:       initial lkey
        self.init = True

        self.refresh(base=base, key=key, mask=mask)

        # child
        #    True:  wait
        #    False/None: final node
        if child in [True, False, None]:
            pass
        elif not isinstance(child, LinkedArray):
            raise ValueError(f"Invalid child type {type(child)}.")

    def update(self, key):
        """Dummy hook to update current status."""

    def refresh(self, base, key, *args, **kwds):
        """Refresh base array and relating properties.
        Wrapper to switch(), and should be overloaded."""

        debug = ft.partial(self.debug, func='refresh')

        # base array
        #   None:  wait
        #   dict, list:  set
        if base is not None:
            self.base = base
        # key
        #   number: 1d length
        #   tuple:  shape
        #   list:   1d phyiscal key
        #   None:   use key of input array
        if key is not None:
            if self.base is None:
                pass
            elif not hasattr(self.base, '__getitem__'):
                raise ValueError(f"no mapping in {self.base}")

            if isinstance(key, int):
                self._keys = None
                self.shape = (key, )
                self._l2i = self._l2i_single
            elif isinstance(key, tuple):
                self._keys = None
                self.shape = key
                self._l2i = self._l2i_raw
            elif isinstance(key, list):
                self._keys = list(key)
                self.shape = (len(self._keys), )
                self._l2i = self._l2i_single
            else:
                raise ValueError(f"invalid key = {key}")
        elif base is not None:
            if self.base is None:
                # base is reserved
                self.shape = None
                self._keys = None
                self._l2i = None
            elif isinstance(self.base, dict):
                self.shape = (len(self.base), )
                self._keys = list(self.base.keys())
                self._l2i = self._l2i_single
            elif isinstance(self.base, cabc.Iterable):
                self.shape = (len(self.base), )
                self._keys = None
                self._l2i = self._l2i_single
            else:
                raise ValueError(f"invalid base object = {self.base}")

        cls = False
        return self.switch(*args, **kwds)

    def switch(self, cls=None,
               mask=None, step=None, init=None,
               lev=None, aux=None):
        """Recursive switcher of attributes."""
        lev = lev or 0
        cls, do_this, do_next = self._class_range(cls, lev)
        if do_this:
            if step is not None:
                self.step = step
            if mask is not None:
                self.mask = mask
            if init is not None:
                self.cue(init)
            if aux is None:
                aux = True
            if aux is True:
                pass
            elif aux is False:
                self.aux = None
            else:
                self.aux = aux

        if do_next:
            if self.child:
                self.child.switch(cls=cls,
                                  mask=mask, step=step,
                                  init=init, lev=lev+1, aux=aux)
            elif cls[0]:
                raise TypeError(self.msg(f"No class as {cls[0]}"))
        return self

    def reset(self):
        """Reset self and children."""
        if 0 in self.shape:
            # always return None if no elements
            init = None
        else:
            init = self.init
            if init:   # natural or prescribed initial
                if init is True:
                    if self._current:
                        init = self._current
                    else:
                        init = (True, ) * len(self.shape)
                if self.step > 0:
                    init = tuple(0 if j is True else j
                                 for j in init)
                elif self.step < 0:
                    init = tuple(w - 1 if j is True else j
                                 for j, w in zip(init, self.shape))
                else:
                    init = False
            if init is False:
                if self._current and len(self._current) == len(self.shape):
                    # keep current
                    init = self._current
                    if self.step > 0:
                        init = tuple(0 if j is True else j
                                     for j in init)
                    elif self.step < 0:
                        init = tuple(w - 1 if j is True else j
                                     for j, w in zip(init, self.shape))
                else:
                    init = (0, ) * len(self.shape)
        return init

    def linked(self, cls):
        """Inquire linked instance of cls."""
        return self.inquire('self', cls, single=True)

    def inquire(self, prop, cls=None, lev=None, single=False):
        """Inquire current property."""
        lev = lev or 0
        cls, do_this, do_next = self._class_range(cls, lev)
        ret = []
        if do_this:
            if prop == 'mask':
                ret.append(self.mask)
            elif prop == 'self':
                ret.append(self)
            elif prop == 'shape':
                ret.append(self.shape)
            else:
                raise ValueError(f"Invalid property {prop}.")
        if do_next:
            if self.child:
                ret.extend(self.child.inquire(prop, cls=cls,
                                              lev=lev+1, single=False))
        if single:
            if len(ret) == 0:
                ret = None
            elif len(ret) == 1:
                ret = ret[0]
        return ret

    def cue(self, init):
        """Cueing."""
        if init is True:
            self._current = None
            self.init = True
        else:
            self.init = False
            self._current = init

    def copy(self, init=None, recurse=True):
        """Clone instance."""
        cp = copy.copy(self)
        if init is False:
            cp.cue(self._current)
        if recurse and self.child:
            cp.child = self.child.copy(init=init, recurse=recurse)
        return cp

    def l2p(self, key=None):
        """Logical to physical element-key conversion."""
        pkey = self.l2i(key, mask=True)
        if self._keys:
            if pkey is not None:
                pkey = self._keys[pkey]
        return pkey

    def l2i(self, key=None, mask=False):
        """Logical to unmasked element-key conversion."""
        if key is None:
            key = self._current
        if key is None:
            return key
        if mask:
            key = self._mask(key, self.mask)
        if bool(self._l2i):
            return self._l2i(key)
        raise ValueError(self.msg("l2i function not bound."))

    def _l2i_raw(self, key):
        return key

    def _l2i_single(self, key):
        return key[0]

    def value(self, key=None):
        # self.debug('value enter')
        return self._value(key)

    def append(self, item, key=None):
        """Append item to array."""
        if isinstance(self.base, list):
            self.base.append(item)
            self.refresh(base=self.base, key=key)
        else:
            raise TypeError(f"invalid item to append on {type(self.base)}.")

    def put(self, key=None, value=None):
        """Put base value."""
        k = self.l2p(key)
        self.base[k] = value
        return self.base[k]

    def get(self, key=None, default=None):
        """Get base value."""
        k = self.l2p(key)
        return self.base.get(k, default)

    def items(self, recurse=True):
        """Iterator of key-value tuples."""
        for lkey in self(recurse=recurse):
            yield lkey, self[lkey]

    @property
    def current(self):
        """Current physical element-key tuple."""
        if self._current:
            ret = self._mask(self._current, self.mask)
        else:
            ret = None
        ret = (ret, )
        if self.child and self.recurse:
            ret = ret + self.child.current
        return ret

    def __str__(self):
        head = self.name or type(self)
        base = f"<{head}>{self.base}{self.shape}[{self._current}]"
        if self.child:
            base = base + f'+{self.child}'
        return base

    def __call__(self, recurse=True):
        self.recurse = recurse
        return self

    def __getitem__(self, key):
        # self.debug("__getitem__ in")
        if key:
            head = key[0]
            rest = key[1:]
        else:
            head = (self._current, )
            rest = key
        head = self._value(head)
        # self.debug("__getitem__ out")
        if self.child and self.recurse:
            return (head, ) + self.child[rest]
        return (head, )

    def _class_range(self, cls, lev):
        """Normalize target class range."""
        # cls = (start, stop)
        #    (None, A)    first to [A Exclusive]
        #    (False, A)   first to [A Exclusive]
        #    (True, A)    first to [A Inclusive]
        #    (_, True)    first only
        #    (A, True)    A only
        #    (A, None)    A to last
        #    (A, False)   A to last
        #    (A, B)       A to [B Exlusive]
        if cls is None:
            cls = (None, None)
        elif isinstance(cls, tuple):
            pass
        else:
            cls = (cls, True)

        if cls[0] in [None, False, ]:
            do_this = True
            if cls[1] is True:
                do_next = False
            elif cls[1] in [None, False]:
                do_next = True
            elif isinstance(self, cls[1]):
                do_this = False
                do_next = False
            else:
                do_next = True
        elif cls[0] is True:
            do_this = True
            if cls[1] is True:
                do_next = False
            elif cls[1] in [None, False]:
                do_next = True
            elif isinstance(self, cls[1]):
                do_this = True
                do_next = False
            else:
                do_next = True
        else:
            do_this = isinstance(self, cls[0])
            if cls[1] is True:
                do_next = not do_this
            elif cls[1] in [None, False]:
                do_next = True
            else:
                do_this = do_this and (not isinstance(self, cls[1]))
                do_next = do_this
                if do_this:
                    cls = None, cls[1]
        return cls, do_this, do_next

    def __bool__(self):
        return True

    def __len__(self):
        n = 0
        ret = self.reset()

        if self.child and self.recurse:
            while ret is not None:
                self.update(ret)
                self._current = ret
                ret = self._mask(ret, self.mask)
                n = n + len(self.child)
                if self.step == 0:
                    break
                ret = self._next()
            self._current = None
        else:
            n = 1
            for j in self._mask(self.shape, self.mask):
                if isinstance(j, int):
                    n = n * j
        return n

    def __iter__(self):
        ret = self.reset()

        while ret is not None:
            # locallog.debug(f"__iter__ {self}")
            # locallog.debug(f"__iter__ {ret} {id(self)} {self._current=}")
            self.update(ret)
            self._current = ret

            ret = self._mask(ret, self.mask)
            if self.child and self.recurse:
                for c in self.child:
                    yield (ret, ) + c
            else:
                yield (ret, )

            if self.step == 0:
                break
            ret = self._next()
        else:
            if self.mask is False:
                self._current = None
            else:
                self._current = tuple(c if m is True else True
                                      for c, m in zip(self._current,
                                                      self.mask))

    def _mask(self, val, mask):
        ret = ()
        mask = mask or (False, ) * len(val)
        for v, m in zip(val, mask):
            if m in [False, True, ]:
                ret = ret + (v, )
            elif m is None:
                pass
            else:
                ret = ret + (m, )
        return ret

    def _next(self):
        """Return key-tree tuple next to current."""
        mask = self.mask or (False, ) * len(self.shape)

        if self.step > 0:
            b = ()
            for j in reversed(range(len(self.shape))):
                if mask[j]:
                    b = (self._current[j], ) + b
                else:
                    w = self.shape[j]
                    n = self._current[j] + 1
                    if n < w:
                        b = self._current[:j] + (n, ) + b
                        break
                    b = (0, ) + b
            else:
                b = None
        elif self.step < 0:
            b = ()
            for j in reversed(range(len(self.shape))):
                if mask[j]:
                    b = (self._current[j], ) + b
                else:
                    w = self.shape[j]
                    n = self._current[j] - 1
                    if n >= 0:
                        b = self._current[:j] + (n, ) + b
                        break
                    b = (w - 1, ) + b
            else:
                b = None
        else:
            b = self._current
        return b

    def _value(self, key=None):
        if key is None:
            key = self._current
        return self.base[self.l2p(key)]

    def msg(self, txt):
        """Messages"""
        name = self.name or type(self)
        return f"<{name}>: {txt}"

    def diag(self):
        locallog.debug(f"{self._current=}")

    def debug(self, msg, func=None, **kwds):
        tags = [self.name]
        if func:
            tags.append(func)
        tag = ':'.join(t for t in tags if t)
        locallog.debug(f"({tag}) {msg}", **kwds)


class ArrayIter(LinkedArray):
    """Array level iterator."""

    def __init__(self, coords=None, anchors=None, **kw):
        self.src = None
        self.nsel = None      # normalized selection (defined only)
        self.xsel = None      # source selection (whole coordinates)
        super().__init__(base=None, name='ARRAY', **kw)

        self.coords = coords or ()  # default choice of coordinates
        self.draw = ()              # draw cache

        self.data = None            # data cache

        anchors = anchors or {}
        self.anchors = {}
        for co, sp in anchors.items():
            if isinstance(sp, slice) or sp is None:
                pass
            else:
                self.anchors[co] = sp

    def refresh(self, *, base, src=None, nsel=None, xsel=None, **kwds):
        """Postprocess of switch()."""
        debug = ft.partial(self.debug, func='refresh')

        # recover previous status
        pdims = self.src.dims if self.src is not None else ()

        # update current auxiliary status
        if src is not None:
            self.src = src
        if nsel is not None:
            self.nsel = nsel
        if xsel is not None:
            self.xsel = xsel

        if base is not None:
            # debug(f"{pdims=}")
            # debug(f"{self.draw=}")
            # debug(f"{src.dims=}")
            # debug(f"{base.dims=}")

            asel = filter_coords(base, src.dims, self.anchors)
            # debug(f"{asel=}")
            csel = draw_coords(base, src, pdims, self.draw,
                               self.coords, anchors=asel)
            # debug(f"{csel=}")
            mask = gen_mask(base, csel, asel)
            # debug(f"{mask=}")
            cue = self.adjust_cue(base, asel) or None
            # debug(f"{cue=}")

            # debug("return")
            self.draw = csel
            self.data = None

            return super().refresh(base=base,
                                   key=base.shape, mask=mask, init=cue, **kwds)

        # debug("return (no base)")
        return super().refresh(base=base, **kwds)

    def permute_draw(self, switch, skip_single=True, req=None):
        """Draw coordinates permutation."""
        debug = ft.partial(self.debug, func='permute_draw')
        mask = self.mask
        shape = self.shape

        reverse = switch < 0

        pat = [j for j, m in enumerate(reversed(mask))
               if isinstance(m, slice) is not reverse]
        if skip_single:
            fskip = []
            for w, m in zip(shape, mask):
                if m is True or w < 2:
                    fskip = [1] + fskip
                else:
                    fskip = [0] + fskip
        else:
            fskip = [0] * len(shape)
        mpat = len(pat)
        mskip = sum(fskip[pat[j]] for j in range(mpat))
        ntgt = sum(fskip) if reverse else 0
        mshp = len(shape)

        pat.append(mshp)
        fskip.append(0)

        if req is None:
            chk = -1
        else:
            chk = mshp - req - 1
            if chk < 0 or chk > mshp:
                raise ValueError(f"invalid request dimension {req}")
        # print(f"{chk=} {req=} {mshp=} {pat=}")

        for _ in range(math.comb(mshp, mpat)):
            pos = 0
            while pos < mpat:
                mskip += - fskip[pat[pos]] + fskip[pat[pos]+1]
                pat[pos] += 1
                if pat[pos] < pat[pos+1]:
                    break
                mskip += - fskip[pat[pos]] + fskip[pos]
                pat[pos] = pos
                pos = pos + 1
            if mskip == ntgt:
                if not chk in pat:
                    break
                # print(pat)
        else:
            locallog.info("draw coordinate reverted.")
            return
            # raise ValueError(f"Panic in permute_draw {req}")
        # print(f"{req=} {mshp=} {pat=} done")

        y, n = slice(None, None, None), False
        if reverse:
            y, n = n, y
        nmask = [n] * mshp
        pat = pat[:-1]
        while pat:
            j = - 1 - pat[0]
            nmask[j] = y
            pat = pat[1:]
        cidx = tuple(- mshp + j for j, m in enumerate(nmask)
                     if isinstance(m, slice))

        self.draw = tuple(self.base.dims[j] for j in cidx)
        mask = [m if m is True else n for m, n in zip(mask, nmask)]
        # debug(f"{self.draw=}")
        # debug(f"{mask=}")
        self.switch(mask=tuple(mask))

        self.data = None

        return self.draw

    def switch_draw(self, aux, step, skip_single=True):
        """Draw coordinates switching."""
        debug = ft.partial(self.debug, func='switch_draw')
        mask = self.mask
        shape = self.shape
        coord = None
        which = aux[0]
        if which in ['left', 'right', ]:
            coord = self.draw[0]
        elif which in ['top', 'bottom', ]:
            coord = self.draw[-1]

        dims = self.base.dims
        if coord not in dims:
            # debug(f"return ({coord})")
            return

        tgt = dims.index(coord)
        if step < 0:
            tgt = tgt - len(dims)
            d = 1
        else:
            d = -1
        # -2 -1 0 1 2
        for jc in range(len(dims)):
            jc = tgt + d * jc
            # print(jc, dims[jc], shape[jc])
            if skip_single and shape[jc] < 2:
                continue
            if dims[jc] not in self.draw:
                tgt = dims[jc]
                break
        else:
            locallog.warning(f"Cannot switch coordinate from {coord}")
            return None

        jd = self.draw.index(coord)
        nd = self.draw[:jd] + (tgt, ) + self.draw[jd+1:]

        asel = filter_coords(self.base, self.src.dims, self.anchors)
        mask = gen_mask(self.base, nd, asel)

        # debug(f"{coord} to {tgt}")
        # debug(f"{self.draw} to {nd}")

        self.switch(mask=tuple(mask))
        self.data = None
        self.draw = tuple(nd)
        return self.draw

    def permute_anchor(self, switch):
        """Anchor permutation."""
        debug = ft.partial(self.debug, func='permute_anchor')

        mask = self.mask
        cidx = self.l2i()

        w = len(mask)
        if switch == 0:
            npat = [False if m in [True, False] else m
                    for m in mask]
        else:
            reverse = switch > 0

            pat = {j: (m is reverse)
                   for j, m in enumerate(reversed(mask)) if m in [True, False]}
            npat = list(mask)
            for j in pat.keys():
                npat[w - j - 1] = (not pat[j]) is reverse
                if not pat[j]:
                    break

        self.switch(mask=tuple(npat))
        self.anchors = self.update_anchors(npat, cidx, self.anchors)
        # debug(f"{self.anchors=}")
        return self.anchors

    def update_anchors(self, mask, cidx, anchors=None):
        """Get lock coordinates, return index-list."""
        anchors = anchors or {}

        for j, m in enumerate(mask):
            d = self.src.dims[j]
            if m is True:
                c = self.src.coords[d]
                pos = c[cidx[j]]
                anchors[d] = pos.item()
            elif d in anchors:
                # del anchors[d]
                anchors[d] = False
        return anchors

    def turn_or_switch(self, target, switch, skip_single=True):
        """Set target dimension as iterate or anchor coordinate.
        If already, then turn slice according to switch."""
        debug = ft.partial(self.debug, func='turn_or_switch')
        # debug = locallog.info
        debug(f"{target=} {switch=}")
        base = self.base
        debug(f"{base.dims=}")
        try:
            target = parse_coord(self.src, target)
        except UserWarning as err:
            locallog.warning(err)
            return
        if not target in base.dims:
            locallog.warning(f"Non-selectable coordinate {target}")
            return
        dtgt = base.dims.index(target)
        # debug(f"{dtgt=} {target=}")
        dstt = self.mask[dtgt]
        # debug(f"{dstt=}")
        if dstt in [True, False]:
            # turn-mode
            ret = self.turn_slice(switch, target=dtgt)
        else:
            # permute mode
            self.permute_draw(+1, skip_single, req=dtgt)
            ret = None
        return ret

    def turn_slice(self, step, target):
        """Turn slice along target only."""
        sav = self.mask
        mask = [True if isinstance(m, bool) else m
                for m in self.mask]
        mask[target] = False
        mask = tuple(mask)
        # print(target, mask, self.mask)
        self.switch(step=step, mask=mask)
        return sav

    def post_turn(self, state):
        self.switch(step=0, mask=state)

    def rewind(self, anchor=None, draw=None):
        """Rewind iterate and other coordinates."""
        debug = ft.partial(self.debug, func='rewind')
        # debug = locallog.info

        base = self.base
        mask = self.mask
        shape = self.shape
        cidx = self.l2i()

        # debug(f"{base.dims=}")
        # debug(f"{mask=}")
        # debug(f"{shape=}")
        # debug(f"{cidx=}")

        ini = 0

        cue = []
        for m, c in zip(mask, cidx):
            if m is False:
                cue.append(ini)
            elif m is True:
                cue.append(ini if bool(anchor) else c)
            else:
                cue.append(ini if bool(draw) else c)
        cue = tuple(cue)
        self.data = None
        self.switch(init=cue)

    def point_selection(self, sel, anchor=None, skip_single=True):
        """Point selection."""
        debug = ft.partial(self.debug, func='point_selection')
        # debug = ft.partial(self.info, func='point_selection')
        # debug(f"{type(self)=}")

        debug(f"{sel=}")

        base = self.base
        mask = self.mask
        shape = self.shape
        # debug(f"{base.dims=}")
        # debug(f"{mask=}")
        # debug(f"{shape=}")

        arr = self.value()

        for d in arr.dims:
            if len(arr.coords[d].shape) > 1:
                locallog.warning("Cannot handle multi-dimension coordinate"
                                 f" {d}")
                return

        # debug(f"{arr.dims=} {arr.shape=} {arr.coords.keys()=}")
        nco = len(arr.dims)

        asel = filter_coords(base, self.src.dims, self.anchors)
        # debug(f"{asel=}")

        ctmp = []
        # base
        for d in arr.dims:
            if d in sel:
                continue
            ctmp.append(d)
        for t in [0, 1]:
            if t == 0:
                skips = [sel, asel]
            else:
                skips = [sel]
            for j in reversed(range(len(base.dims))):
                d, w = base.dims[j], shape[j]
                if d in ctmp:
                    continue
                if any(d in s for s in skips):
                    continue
                if skip_single and w < 2:
                    continue
                ctmp.append(d)
            # debug(f"try[{t}]: {ctmp}")
            if len(ctmp) >= nco:
                break
        else:
            locallog.warning(f"Not enough coordinates to plot {ctmp}")
            return
        ctmp = ctmp[:nco]
        csel = list(arr.dims)
        nrem = []
        for c in ctmp:
            if not c in csel:
                nrem.append(c)
        for j in range(nco):
            if not csel[j] in ctmp:
                csel[j] = nrem.pop(0)
        # debug(f"{csel=}")
        if anchor:
            asel.update(sel)
        mask = gen_mask(base, csel, asel)
        # debug(f"{asel=} {sel=}")
        cue = self.adjust_cue(base, asel, cue=sel) or None
        debug(f"{cue=}")
        if None in cue:
            return None

        # debug("return")
        self.draw = tuple(csel)
        self.data = None

        self.switch(mask=tuple(mask), init=cue)

        return csel

    def adjust_cue(self, base, anchors=None, cue=None):
        """Cueing."""
        debug = ft.partial(self.debug, func='adjust_cue')

        anchors = anchors or {}

        shape = base.shape
        dims = base.dims
        coords = base.coords
        cue = cue or {}

        cidx = self.l2i()
        prev = self.base
        # debug(f"{cidx=}")
        cprv = dict(zip(prev.dims, cidx)) if prev is not None else {}
        cnxt = dict.fromkeys(base.dims)

        for nd in cnxt:
            if nd in cue:
                cnxt[nd] = cue[nd]
            elif nd in cprv:
                cnxt[nd] = cprv[nd]
            else:
                cnxt[nd] = anchors.get(nd)
        for jd, nd in enumerate(cnxt):
            if cnxt[nd] is not None:
                continue
            try:
                cj = cidx[jd]
            except (TypeError, IndexError):
                cnxt[nd] = 0
                continue
            if cj < shape[jd]:
                cnxt[nd] = cj
            else:
                cnxt[nd] = shape[jd] - 1
                locallog.info(f"rewind in the coordinate {nd}")
        # debug(f"{cnxt=}")
        cue = []
        for d in dims:
            sel = cnxt.get(d)
            if sel is None:
                sel = True
            elif isinstance(sel, (int, slice)):
                pass
            else:
                sel = normalize_selection(coords[d], sel, index=True)
            cue.append(sel)
        cue = tuple(cue)
        # debug(f"{cue=}")

        return cue

    def update(self, key):
        debug = ft.partial(self.debug, func='update')
        # debug(f"{key=}")
        self.data = None

    @staticmethod
    def is_isel(s):
        """Check if s is integer-only slice"""
        if isinstance(s, slice):
            b = isinstance(s.start, int) or s.start is None
            e = isinstance(s.stop, int) or s.stop is None
            return b and e
        return False

    def status(self, fmt=True, recurse=False, dims=None):
        """Generate current status."""
        debug = ft.partial(self.debug, func='status')
        # debug("enter")
        data = super()._value()
        stt = []
        pkey = self.l2p()
        mask = self.mask
        for xk, xs in self.xsel.items():
            anc = 0
            if xk in self.base.dims:
                jk = self.base.dims.index(xk)
                pk = pkey[jk]
                xs = zu.set_default(xs, slice(None, None, None))
                if mask[jk] is True:
                    anc = 1
                if isinstance(pk, slice):
                    pass
                elif self.is_isel(xs):
                    xs = (xs.start or 0) + pk
                else:
                    xs = data.coords[xk].item()
            else:               # squeezed
                anc = 2
                if isinstance(xs, int):
                    pass
                else:
                    xs = self.nsel.get(xk, xs)
            if isinstance(xs, slice):
                b, e, s = xs.start, xs.stop, xs.step
                p = ''
                p = p + ('' if b is None else str(b))
                p = p + ':'
                p = p + ('' if e is None else str(e))
                if s is None or s == 1:
                    pass
                else:
                    p = p + ':' + str(s)
                xs = p
            else:
                xs = str(xs)
                if anc == 1:
                    xs = '(' + xs + ')'
                elif anc == 2:
                    xs = '[' + xs + ']'
            stt.append(xs)
        txt = ', '.join(stt)
        txt = f'[{txt}]'
        if recurse and self.child:
            ch = self.child.status(fmt=fmt, recurse=recurse)
            if ch:
                txt = txt + ' ' + ch
        # debug("exit")
        return txt

    def offset(self, nsel, xsel):
        """Store selections."""
        self.nsel = nsel
        self.xsel = xsel

    def transpose(self, switch=None):
        """draw-array transposition."""
        debug = ft.partial(self.debug, func='transpose')
        if isinstance(switch, (tuple, list)):
            self.draw = switch
        elif self.draw:
            switch = switch or 0
            if switch < 0:
                self.draw = self.draw[-1:] + self.draw[:-1]
            else:
                self.draw = self.draw[1:] + self.draw[:1]
        self.data = None
        # debug(f"{self.draw}")
        return self.draw

    def _value(self, key=None):
        """draw-array adjustment."""
        debug = ft.partial(self.debug, func='_value')
        # debug(f"{key=}")

        if self.data is not None:
            # debug(f'cached={key}')
            return self.data

        data = super()._value(key)
        # debug(f"{data.dims=}")

        data = data.transpose(*self.draw)
        # debug(f"{data.name=} {data.shape=}")
        # debug('cyclic in')
        odata = data
        for cn in data.coords:
            if cn not in data.dims:
                continue
            ck = data.dims.index(cn)
            co = data[cn]
            cc = is_cyclic_coord(co)
            # debug(f"{cn=} {cc=}")
            if not cc:
                continue
            w, org, dup = cc
            if len(co) == w - 1 and co[0] == org and co[-1] != dup:
                # for j, p in enumerate(co):
                #     if p >= 180:
                #         break
                # print(j, p.item())
                # data = data.roll(**{co.name: j}, roll_coords=True)
                # co = data[co.name]
                cd, cx = cutil.add_cyclic(data, co, axis=ck,
                                          cyclic=dup-org)
                nco = dict(data.coords.items())
                nco[cn] = xr.DataArray(cx, dims=co.dims, attrs=co.attrs)
                # for j, p in enumerate(nco[cn]):
                #     if p >= 180:
                #         break
                # print(j, p.item())
                # nco[cn] = (nco[cn] + 180) % 360 - 180
                # nco[cn][-1] = nco[cn][-1] + 360
                data = xr.DataArray(cd, coords=nco,
                                    dims=data.dims, attrs=data.attrs)
                # need to activate nio accessor
                _ = data.nio
                # data = data.roll(**{cn: j}, roll_coords=True)
                # print(cn, j)
                # print(data)
                # print(cn, nco[cn])
        if not data is odata:
            data.name = odata.name
            data.assign_attrs(**odata.attrs)
        # debug('cyclic out')
        # debug(f"{data.name=} {data.shape=}")
        # store cache
        self.data = data
        return data

    # def __getitem__(self, key):
    #     return super().__getitem__(key)

    def l2p(self, key=None):
        """Logical to physical element-key conversion."""
        r = super().l2p(key)
        return r


class VariableIter(LinkedArray):
    """Variable level iterator."""

    def __init__(self, dims=None, **kwds):
        super().__init__(base=None, name='VAR', **kwds)
        self.lims = dims

    def update(self, key):
        """Update current and child status."""
        debug = ft.partial(self.debug, func='update')
        # define base data-array
        src = self.get(key)
        psrc = self.get()
        pdim = psrc.dims if psrc is not None else ()
        # debug(f"{pdim} to {src.dims}")

        alink = self.linked(ArrayIter)

        if src is None:
            alink.refresh(base=None, key=0, )
            return

        src = self.assign_coord(src)
        # debug(f"source='{src.name}'{src.dims}{src.shape}")

        try:
            xsel, nsel = extract_sels(self.lims, src)
            # debug(f"{nsel=}")
            # debug(f"{xsel=}")
        except UserWarning:
            alink.refresh(base=None, )
            return

        base = extract_base_array(src, nsel, xsel)
        # debug(f"base='{base.name}'{base.dims}{base.shape}")

        alink.refresh(base=base, src=src, nsel=nsel, xsel=xsel)

    def assign_coord(self, data):
        """Assign default (index) coordinate if empty."""
        for d in data.dims:
            if not d in data.coords and len(data[d]) > 1:
                # locallog.debug(f"Dummy index coordinate introduced [{d}].")
                data[d] = np.arange(len(data[d]), dtype=float)
        return data

    def status(self, fmt=True, recurse=False):
        # self.debug('status in')
        idx = self.l2i()
        name = self.l2p()
        txt = f'<{idx}:{name}>'
        v = self.value()
        if v is not None:
            v = v.dims
        if recurse and self.child:
            ch = self.child.status(fmt=fmt, recurse=recurse, dims=v)
            if ch:
                txt = txt + ' ' + ch
        # self.debug(f'status out {txt}')
        return txt

    def l2p(self, key=None):
        """Logical to physical element-key conversion."""
        r = super().l2p(key)
        return r

    def set_coords(self, coords, data, prop=None, anchors=None,
                  skip_single=True):
        """Set coordinate tuple."""
        anchors = anchors or {}
        clist = []
        for c in coords:
            try:
                c = parse_coord(data, c)
                cj = data.dims.index(c)
                if skip_single and data.shape[cj] < 2:
                    continue
                # m = match_coordinate(data, c, anchors)
                clist.append(cj)
            except UserWarning:
                if c == '':
                    clist.append(c)
                else:
                    msg = f"no coordinate to plot {c}.  Available={data.dims}"
                    locallog.warning(msg)
        w = len(data.dims)
        rem = w
        nlist = []
        for cj in reversed(clist):
            if cj == '':
                cj = rem - 1
                while cj in clist or data.dims[cj] in anchors \
                      or (skip_single and data.shape[cj] < 2):
                    cj = cj - 1
                rem = cj
            nlist.insert(0, cj)
        if prop is None:
            return tuple(data.dims[c] for c in nlist if c >= 0)
        if prop < 0:
            return tuple(c - w for c in nlist if c >= 0)
        return tuple(c for c in nlist if c >= 0)


class FileIter(LinkedArray):
    """File (or dataset) level iterator."""

    def __init__(self, files, variables=None, opts=None, **kw):
        super().__init__(base={}, key=files, name='FILE', **kw)
        variables = False if variables is None else variables
        self.variables = variables
        self.vlist = {}
        self.opts = opts or {}

    def update(self, key):
        # debug = ft.partial(self.debug, func='update')
        f = self.l2p(key)
        ds = self.base.get(f)
        if ds is None:
            ds = zxr.open_dataset(f, **self.opts)
            self.base[f] = ds

        if key not in self.vlist:
            if self.variables is False:
                vlist = list(ds.data_vars)
            elif self.variables is True:
                vlist = []
                for vn, vv in ds.data_vars.items():
                    ne = sum(1 for w in vv.shape if w > 1)
                    if ne > 1:
                        vlist.append(vn)
            elif isinstance(self.variables, cabc.Callable):
                vlist = list(filter(self.variables, ds.data_vars))
            elif isinstance(self.variables, cabc.Iterable):
                vlist = []
                for v in self.variables:
                    for vn, _ in ds.data_vars.items():
                        if vn == v:
                            vlist.append(vn)
            else:
                raise TypeError(f"invalid variable filter {self.variables}")
            self.vlist[key] = vlist
            if not vlist:
                locallog.warning(f'No variables to match in {f}.')
            else:
                locallog.info("variables: " + ','.join(v for v in vlist))
        vlink = self.linked(VariableIter)
        vlink.refresh(base=ds, key=self.vlist[key])

    def status(self, fmt=True, recurse=False):
        """Recursive status string generation."""
        idx = self.l2i()
        path = plib.Path(self.l2p())
        txt = f'{idx}:{path.name}'
        if recurse and self.child:
            ch = self.child.status(fmt=fmt, recurse=recurse)
            if ch:
                txt = txt + ' ' + ch
        return txt


class DataTree():
    """Iterator method collection with data suite."""

    def __init__(self, root, **kwds):
        self.root = root

    def permute_anchor(self, switch):
        """Call permute_anchor() on array-link."""
        try:
            self._alink.permute_anchor(switch)
        except AttributeError:
            pass

    def permute_draw(self, switch):
        """Call permute_draw() on array-link."""
        try:
            self._alink.permute_draw(switch)
        except AttributeError:
            pass

    def switch_draw(self, coord, switch):
        """Call switch_draw() on array-link."""
        try:
            self._alink.switch_draw(coord, switch)
        except AttributeError:
            pass

    def turn_or_switch(self, target, switch=None):
        """Call turn_or_switch() on array-link."""
        try:
            return self._alink.turn_or_switch(target, switch)
        except AttributeError:
            pass

    def post_turn(self, state):
        """Call post_turn() on array-link."""
        try:
            return self._alink.post_turn(state)
        except AttributeError:
            pass


    def transpose(self, switch=None):
        """Call transpose() on array-link."""
        try:
            self._alink.transpose(switch)
        except AttributeError:
            pass

    def point_selection(self, sel, anchor=None):
        # locallog.debug(f"datatree {sel=}")
        try:
            self._alink.point_selection(sel, anchor=anchor)
        except AttributeError as exc:
            locallog.debug(f"{exc}")

    def rewind(self, anchor=None):
        # locallog.debug(f"datatree {sel=}")
        try:
            self._alink.rewind(anchor=anchor)
        except AttributeError as exc:
            locallog.debug(f"{exc}")

    # def get_points(self, x=None, y=None):
    #     """Get coordinate index and value on the source array."""
    #     r = {}
    #     arr = self.draw_data()
    #     src = self.source_data()
    #     # locallog.debug(f"get_points {arr.dims=} {src.dims=} {kwds=}")

    #     for j, pos in [(1, x), (0, y), ]:
    #         if pos:
    #             co = arr.dims[j]
    #             xsel = src.coords[co].sel({co: pos}, method='nearest')
    #             xsel = xsel.item()

    #             cidx = src.coords[co].to_index()
    #             jsel = cidx.get_loc(xsel)

    #             # locallog.debug(f"get_points {co}[{jsel}:{xsel}]={pos}")
    #             r[co] = (jsel, xsel)
    #         # pass
    #     return r

    def source_data(self, key=None):
        """Get source (file) data-array."""
        try:
            var = self._vlink.value(key)
            return var
        except AttributeError:
            return None

    def base_data(self):
        """Get base (buffered) data-array."""
        try:
            base = self._alink.base
            return base
        except AttributeError:
            return None

    def draw_data(self, key=None):
        """Get draw (array) data-array."""
        try:
            arr = self._alink.value(key)
            return arr
        except AttributeError:
            return None

    def source_dims(self, key=None):
        """Get source (file) dimensions."""
        try:
            var = self.source_data(key)
            return var.dims
        except AttributeError:
            return None

    def base_dims(self):
        """Get base (buffered) dimensions."""
        try:
            base = self.base_data()
            return base.dims
        except AttributeError:
            return None

    def draw_dims(self, key=None):
        """Get draw (array) dimensions."""
        try:
            arr = self.draw_data(key)
            return arr.dims
        except AttributeError:
            return None

    @property
    def _flink(self):
        return self.root.linked(cls=FileIter)

    @property
    def _vlink(self):
        return self.root.linked(cls=VariableIter)

    @property
    def _alink(self):
        return self.root.linked(cls=ArrayIter)


class PlotParams():
    """Plot options complex."""

    def __init__(self, nml=None):
        nml = 'variable' if nml is None else nml
        if not isinstance(nml, (tuple, list)):
            nml = (nml, )
        self._nml = nml
        self.root = {}
        self.cache = {}
        self.nml = {}

    def reg_entry(self, arg, entry, nml=None):
        # if not isinstance(entry, (tuple, list)):
        #     entry = [entry]
        self.root[arg] = entry
        if nml is None:
            nml = self._nml
        if not isinstance(nml, (tuple, list)):
            nml = (nml, )
        self.nml[arg] = nml
        self.cache[arg] = {}

    def __call__(self, *args, **kwds):
        # locallog.debug(f"__call__: {kwds.keys()}")
        params = {}
        if args:
            if args[0] in [True, False]:
                d = args[0]
                args = args[1:]
            else:
                d = False
        else:
            d = False
            args = self.root.keys()

        if d:
            args = [a for a in self.root.keys() if not a in args]
        for a in args:
            disp = self.get_disp(a, default=None, **kwds)
            if disp is None:
                disp = self.reg_disp(a, **kwds)
            dp = disp.params(**kwds)
            params[a] = dp
        return params

    def get_disp(self, arg, /, default=None, **kwds):
        """Get parameter dispatcher."""
        k = self.normalize(arg, **kwds)
        return self.cache[arg].get(k, default)

    def reg_disp(self, arg, **kwds):
        """Register parameter dispatcher."""
        k = self.normalize(arg, **kwds)
        disps = self.root[arg]()
        self.cache[arg][k] = disps
        return disps

    def normalize(self, arg, fig=None, src=None, array=None, name=None,
                  coords=None, **kwds):
        nkeys = self.nml[arg]
        # print(f"{arg=} {}")
        nk = (fig, )
        if 'variable' in nkeys:
            if name is None:
                name = ''
                if array is not None:
                    name = array.name
                if not name:
                    if src is not None:
                        name = src.name
            nk = nk + (name, )
        if 'coords' in nkeys:
            if coords:
                pass
            elif array is not None:
                coords = array.dims
            if coords is not None:
                nk = nk + (coords, )
        return nk


class ParamsDispatcher():
    def params(self, **kwds):
        raise NotImplementedError

    # def bind(self, *args, **kwds):
    #     raise NotImplementedError


class CmapStatus():
    """cmap and norm placeholder."""
    def __init__(self, artist, reverse=None):
        self.mixin = artist
        self.norm = artist.norm
        if reverse:
            self.cmap_r = artist.get_cmap()
            self.cmap = None
        else:
            self.cmap = artist.get_cmap()
            self.cmap_r = None

    def get_cmap(self, reverse=None):
        reverse = bool(reverse)
        if reverse:
            if not self.cmap_r and self.cmap:
                self.cmap_r = self.cmap.reversed()
            if self.cmap_r is None:
                locallog.info("cannot provide reversed cmap")
                cm = self.cmap
            else:
                cm = self.cmap_r
        else:
            if not self.cmap and self.cmap_r:
                self.cmap = self.cmap_r.reversed()
            if self.cmap is None:
                locallog.info("cannot provide cmap")
                cm = self.cmap_r
            else:
                cm = self.cmap

        return cm


class SimpleLink(LinkedArray):
    def __init__(self, *args, **kwds):
        super().__init__(*args, **kwds)
        self._loop = None

    def loop(self):
        self._loop = iter(self.items())
        _ = next(self._loop)

    def advance(self, step):
        self.switch(step=step)
        try:
            _ = next(self._loop)
        except StopIteration:
            self._loop = iter(self.items())
            _ = next(self._loop)
        return self.value()

    def fwd(self):
        return self.advance(+1)

    def bwd(self):
        return self.advance(-1)


class AxisScaleLink(SimpleLink, ParamsDispatcher):
    # to do: 'index'
    scales_ = ['', 'linear', 'log', ]

    def __init__(self, scales=None, **kw):
        if scales is None:
            scales = self.scales_
        if not isinstance(scales, (tuple, list)):
            scales = [scales]

        name = "SCALE"
        super().__init__(base=scales[:], name=name, **kw)
        # self._loop = None

    def params(self, **kwds):
        p = {}

        if not self._loop:
            self.loop()
        scale = self.value()

        p['scale'] = scale
        return p

    # def loop(self):
    #     self._loop = iter(self.items())
    #     _ = next(self._loop)

    # def advance(self, step):
    #     self.switch(step=step)
    #     try:
    #         _ = next(self._loop)
    #     except StopIteration:
    #         self._loop = iter(self.items())
    #         _ = next(self._loop)
    #     return self.value()

    # def fwd(self):
    #     return self.advance(+1)

    # def bwd(self):
    #     return self.advance(-1)


class AspectRatioLink(SimpleLink, ParamsDispatcher):
    # to do: 'index'
    def __init__(self, ratio=None, **kw):
        self.ratios = ['auto', 'equal']
        if ratio is None:
            pass
        elif not isinstance(ratio, (tuple, list)):
            self.ratios.insert(0, ratio)
        else:
            ar = list(ratio)
            for a in self.ratios:
                if a not in ar:
                    ar.append(a)
            self.ratios = ar
        name = "ARATIO"
        super().__init__(base=self.ratios[:], name=name, **kw)

    def params(self, **kwds):
        p = {}

        if not self._loop:
            self.loop()
        ar = self.value()

        p['aspect'] = ar
        return p


class LimitParams():
    def __init__(self, vmin=None, vmax=None, **kwds):
        self._lock = False
        self._array = None
        self._lims = (vmin, vmax) # user-defined range
        self.lims = (None, None)  # run-time (possibly locked) range
        self.refs = (None, None)  # run-time reference range
                                  # (to check whether changed)
        super().__init__(**kwds)

    def update_lims(self, array):
        vmin, vmax = self.lims
        if self._lock:
            upd = False
        elif self._array is array:
            upd = False
        elif array is not None:
            upd = True
            # if vmin is None:
            #     vmin = self._lims[0]
            # if vmax is None:
            #     vmax = self._lims[1]
            if self._lims[0] is None:
                amin = array.min().item()
                if vmin is not None:
                    vmin = min(amin, vmin)
                else:
                    vmin = amin
            else:
                vmin = self._lims[0]
            if self._lims[1] is None:
                amax = array.max().item()
                if vmax is not None:
                    vmax = max(amax, vmax)
                else:
                    vmax = amax
            else:
                vmax = self._lims[1]
            if vmin == vmax:
                locallog.info(f"Constant field ({vmin})")
        else:
            upd = False
        # update current target array
        self.lims = vmin, vmax
        self._array = array
        return upd, self.lims

    def toggle_lock(self, switch=None):
        if switch is not None:
            self._lock = bool(switch)
        else:
            self._lock = not self._lock
        return self._lock


class NormLink(LimitParams, LinkedArray, ParamsDispatcher):
    """Norm parameter dispatcher."""
    debug = locallog.debug
    # debug = locallog.info

    _serial = 0
    norms_ = ['',
              ('sym', 0.0),
              'log',
              ('symlog', 4, 'max'), ('symlog', 4, 'min'), ('symlog', 4),
              ('symlog', 8, 'max'), ('symlog', 8, 'min'), ('symlog', 8),
              'asinh',
              ('twoslope', 0.0), ]

    def __init__(self, norms=None, levels=None, **kw):
        # vmin=None, vmax=None,
        if not norms:
            norms = self.norms_
        if not isinstance(norms, (tuple, list)):
            norms = [norms]
        # locallog.debug(f"{norms=}")

        name = f"NORM-{self._serial}"
        self._serial = self._serial + 1
        super().__init__(base=norms[:], name=name, **kw)

        self.levels = levels

        self._loop = None
        # self._lock = False
        # self._lims = (vmin, vmax) # user-defined range
        # self.lims = (None, None)  # run-time (possibly locked) range
        # self._array = None
        self.cache = {}
        self._to_adjust = None

    def loop(self):
        self._loop = iter(self.items())
        _ = next(self._loop)

    def params(self, array=None, **kwds):
        self.debug(f"norm:params: {kwds.keys()=}")
        self.debug(f"norm:params: {self._array is array=}")

        p = {}

        if not self._loop:
            self.loop()
        norm = self.value()
        self.debug(f"norm:params: {self.lims=}")
        upd, lims = self.update_lims(array)
        self.debug(f"norm:params: {norm=}")
        self.debug(f"norm:params: {lims=}")

        k = self.l2p(None)
        saved = self.cache.get(k)
        if saved is not None:
            saved = saved.norm
        self.debug(f"norm:value: {norm} {lims} {self.lims} {saved}")
        if upd or self._to_adjust:
            p['norm'] = self.adjust_norm(norm, saved, lims, **kwds)
        else:
            p['norm'] = saved
        self._to_adjust = False
        norm = p['norm']
        if norm is not None:
            self.refs = norm.vmin, norm.vmax
        self.debug(f"norm:params: {norm=}")
        self.debug(f"norm:params: {self.refs=}")
        self.debug(f"norm:params: {self.lims=}")
        self.debug(f"norm:params: return={p}")
        return p

    def adjust_norm(self, norm, saved, lims, **kwds):
        norm = norm or ''
        if norm in ['', 'linear', ]:
            if saved:
                norm = saved
                norm.autoscale(lims)
            else:
                norm = mplib.colors.Normalize(vmin=lims[0], vmax=lims[1])
            self.debug(f"{norm=} {norm.vmin=} {norm.vmax=}")
        elif norm == 'log':
            norm = self.LogNorm(lims)
        elif norm == 'asinh':
            norm = self.AsinhNorm(lims, sym='max')
        elif norm[0] == 'symlog':
            sym = None if len(norm) <= 2 else norm[2]
            norm = self.SymLogNorm(lims, width=norm[1], sym=sym)
        elif norm[0] in 'sym':
            org = norm[1] or 0.0
            vmax = max(abs(lims[0] - org), (lims[1] - org))
            vmin = - vmax + org
            vmax = + vmax + org
            norm = mplib.colors.Normalize(vmin=vmin, vmax=vmax)
        elif norm[0] == 'twoslope':
            self.debug(f"adjust_norm: {lims=}")
            norm = self.TwoSlopeNorm(lims, vcenter=norm[1])
            self.debug(f"{norm=} {norm.vmin=} {norm.vmax=}")
            self.debug(f"{norm=} {norm._vmin=} {norm._vmax=}")
        elif isinstance(norm, str):
            if norm == 'symlog':
                raise ValueError(f"{norm} norm requires one parameter.")
            if norm in ['sym', 'twoslope']:
                raise ValueError(f"{norm} norm requires one parameter.")
            raise ValueError(f"Unknown colormap norm {norm}")
        elif isinstance(norm, mplib.colors.Normalize):
            sc = [v for v in lims if v is not None]
            norm.autoscale(sc or [0])
        # print(f"{self._norm=}")
        # locallog.debug(f"norm:adjust: {saved=}")
        # locallog.debug(f"norm:adjust: {norm=}")
        # locallog.debug(f"norm:range: {norm.vmin}:{norm.vmax}")

        return norm

    def LogNorm(self, lims, width=None):
        width = width or 12
        vmin, vmax = lims
        if vmin is None or vmin <= 0:
            if vmax is None or vmax <= 0:
                vmin = math.pow(10, - width)
                vmax = math.pow(10, - width + 1)
            else:
                vmin = math.pow(10, math.floor(math.log(vmax, 10)) - width)
        return mplib.colors.LogNorm(vmin, vmax)

    def AsinhNorm(self, lims, sym=None):
        vmin, vmax = lims
        vmin, vmax = self.adjust_norm_range(vmin, vmax, sym)
        return mplib.colors.AsinhNorm(vmin=vmin, vmax=vmax)

    def SymLogNorm(self, lims, width=None, sym=None, linthresh=None):
        vmin, vmax = lims
        if sym == 'min':
            am = min(abs(vmin), abs(vmax))
            vmin, vmax = -am, +am
        else:
            am = max(abs(vmin), abs(vmax))
            if sym == 'max':
                vmin, vmax = -am, +am

        if linthresh is None:
            width = width or 12
            if am == 0.0:
                linthresh = math.pow(10, -width)
                vmin, vmax = None, None
            else:
                linthresh = math.pow(10, math.floor(math.log(am, 10)) - width)
        n = mplib.colors.SymLogNorm(linthresh, vmin=vmin, vmax=vmax)
        # print(linthresh, lims, vmin, vmax, n.vmin, n.vmax)
        return n

    def adjust_norm_range(self, vmin, vmax, sym):
        if sym == 'min':
            am = min(abs(vmin), abs(vmax))
            vmin, vmax = -am, +am
        else:
            am = max(abs(vmin), abs(vmax))
            if sym == 'max':
                vmin, vmax = -am, +am
        return vmin, vmax

    def TwoSlopeNorm(self, lims, vcenter=None, fac=None):
        vmin, vmax = lims
        vcenter = vcenter or 0
        fac = fac or 0
        fac = fac or 0.01
        if vmin == vmax:
            vmin, vmax = None, None
        elif vmax < vcenter:
            vmax = vcenter - (vmin - vcenter) * fac
        elif vmin > vcenter:
            vmin = vcenter - (vmax - vcenter) * fac
        return mplib.colors.TwoSlopeNorm(vcenter, vmin=vmin, vmax=vmax)

    def toggle_lock(self, fig, switch=None):
        p = self._lock
        b = super().toggle_lock(switch)
        # if switch is not None:
        #     self._lock = bool(switch)
        # else:
        #     self._lock = not self._lock

        # if self._lock:
        #     if not p:
        #         fig.push_patch(**self.mark)
        # else:
        #     if p:
        #         fig.pop_patch()
        if b:
            key = self._current
            k = self.l2p(key)
            v = self.cache.get(k)
            if v is not None:
                self.lims = v.norm.vmin, v.norm.vmax

        return b

    def record(self, key=None):
        if key is None:
            key = self._current
        k = self.l2p(key)
        v = self.cache.get(k)
        n = self.value()
        self.debug(f"norm:record: {self.refs=}")
        lmin, lmax = self.lims
        if lmin == lmax:
            locallog.warning("Dynamic range is not propagated"
                             " when constant field.")
        elif v is not None:
            vmin, vmax = v.norm.vmin, v.norm.vmax
            upd = not (self.refs == (vmin, vmax))
            self.debug(f"norm:record: [{k}] {upd} {lmin}:{lmax} {vmin}:{vmax}")
            if not upd:
                locallog.info("Untouched dynamic range is not propagated.")
            else:
                if not isinstance(n, tuple):
                    n = (n, None, None)
                else:
                    n = n + (None, None)
                if n[0] == 'symlog':
                    amin, amax = abs(lmin), abs(lmax)
                    amp = None
                    if n[2] == 'max':
                        amp = max(amin, amax)
                    elif n[2] == 'min':
                        amp = min(amin, amax)
                    if amp is not None:
                        if vmin != -amp or vmax != +amp:
                            locallog.warning("Dynamic range is not propagated"
                                             " when max/min symlog mode.")
                        vmin, vmax = lmin, lmax
                elif n[0] == 'log':
                    if lmin < 0:
                        locallog.warning("Dynamic range minimum is not propagated"
                                         " when log mode with negatives.")
                        vmin = lmin
                self.lims = (vmin, vmax)
            # self.lims = v.get_clim()
        self.debug(f"norm:record: [{k}]={v} {n} {self.lims}")

    def advance(self, step):
        # locallog.info(f"norm:advance: {self._current=}")
        self.record()

        self._to_adjust = bool(step != 0)
        self.switch(step=step)
        try:
            v = next(self._loop)
        except StopIteration:
            self._loop = iter(self.items())
            v = next(self._loop)
        return self.value()

    def fwd(self):
        return self.advance(+1)

    def bwd(self):
        return self.advance(-1)

    def bind(self, *, key=None, artist=None, **kwds):
        if key is None:
            key = self._current
        k = self.l2p(key)
        self.cache[k] = artist
        locallog.debug(f"norm:bind: [{k}:{self.value()}]={self.cache[k]}"
                       f"{self.cache[k].norm} {artist.norm}")


class CmapLink(LinkedArray, ParamsDispatcher, _ConfigType):
    serial = 0
    _dgrp = ''

    cmaps_ = {_dgrp: [],
              'seq0': ['viridis', 'plasma', 'inferno', 'magma', 'cividis'],
              'seq1': ['Greys', 'Purples', 'Blues', 'Greens', 'Oranges',
                       'Reds',
                       'YlOrBr', 'YlOrRd', 'OrRd', 'PuRd', 'RdPu', 'BuPu',
                       'GnBu', 'PuBu', 'YlGnBu', 'PuBuGn', 'BuGn', 'YlGn'],
              'seq2': ['binary', 'gist_yarg', 'gist_gray', 'gray', 'bone',
                       'pink', 'spring', 'summer', 'autumn', 'winter', 'cool',
                       'Wistia', 'hot', 'afmhot', 'gist_heat', 'copper'],
              'div': ['PiYG', 'PRGn', 'BrBG', 'PuOr', 'RdGy', 'RdBu', 'RdYlBu',
                      'RdYlGn', 'Spectral', 'coolwarm', 'bwr', 'seismic', ],
              # 'berlin', 'managua', 'vanimo'
              'cyclic': ['twilight', 'twilight_shifted', 'hsv'],
              'qual': ['Pastel1', 'Pastel2', 'Paired', 'Accent', 'Dark2',
                       'Set1', 'Set2', 'Set3', 'tab10', 'tab20', 'tab20b',
                       'tab20c'],
              'misc': ['flag', 'prism', 'ocean', 'gist_earth', 'terrain',
                       'gist_stern', 'gnuplot', 'gnuplot2', 'CMRmap',
                       'cubehelix', 'brg', 'gist_rainbow', 'rainbow', 'jet',
                       'turbo', 'nipy_spectral', 'gist_ncar']}

    # cmaps_ = {dgrp: [],
    #           'seq0': ['viridis', 'plasma', 'inferno', 'magma', 'cividis'],
    #           'seq1': ['Greys', 'Purples', 'Blues', 'Greens', 'Oranges',], }

    cmaps_[_dgrp] = [v[0] for v in cmaps_.values() if v]
    # cited from matplotlib document
    #
    # plot_color_gradients('Perceptually Uniform Sequential',
    #                      ['viridis', 'plasma', 'inferno', 'magma', 'cividis'])
    # plot_color_gradients('Sequential',
    #                      ['Greys', 'Purples', 'Blues', 'Greens', 'Oranges', 'Reds',
    #                       'YlOrBr', 'YlOrRd', 'OrRd', 'PuRd', 'RdPu', 'BuPu',
    #                       'GnBu', 'PuBu', 'YlGnBu', 'PuBuGn', 'BuGn', 'YlGn'])
    # plot_color_gradients('Sequential (2)',
    #                      ['binary', 'gist_yarg', 'gist_gray', 'gray', 'bone',
    #                       'pink', 'spring', 'summer', 'autumn', 'winter', 'cool',
    #                       'Wistia', 'hot', 'afmhot', 'gist_heat', 'copper'])
    # plot_color_gradients('Diverging',
    #                      ['PiYG', 'PRGn', 'BrBG', 'PuOr', 'RdGy', 'RdBu', 'RdYlBu',
    #                       'RdYlGn', 'Spectral', 'coolwarm', 'bwr', 'seismic',
    #                       'berlin', 'managua', 'vanimo'])
    # plot_color_gradients('Cyclic', ['twilight', 'twilight_shifted', 'hsv'])
    # plot_color_gradients('Qualitative',
    #                      ['Pastel1', 'Pastel2', 'Paired', 'Accent', 'Dark2',
    #                       'Set1', 'Set2', 'Set3', 'tab10', 'tab20', 'tab20b',
    #                       'tab20c'])
    # plot_color_gradients('Miscellaneous',
    #                      ['flag', 'prism', 'ocean', 'gist_earth', 'terrain',
    #                       'gist_stern', 'gnuplot', 'gnuplot2', 'CMRmap',
    #                       'cubehelix', 'brg', 'gist_rainbow', 'rainbow', 'jet',
    #                       'turbo', 'nipy_spectral', 'gist_ncar'])

    def __init__(self, cmap=None, reverse=None,
                 method=None, levels=None, alpha=None,
                 chain=None, **kw):

        # locallog.debug(f"{args=}")
        self.method = method
        self.levels = levels
        self.alpha = alpha

        if not isinstance(cmap, (tuple, list)):
            cmap = [cmap]

        dk = self._dgrp
        omap = self.prop('cmaps')

        group = []
        dmap = []
        for c in cmap:
            # print(c)
            if c in omap:
                group.append(c)
            else:
                if dk not in group:
                    group.append(dk)
                dmap.append(c)
        # if group and dmap:
        #     if dk not in group:
        #         group.append(dk)
        if not group:
            group = omap.keys()

        if not dmap:
            # dmap = dmap + [c for c in omap[dk] if c not in dmap]
            dmap = omap[dk]
        # dmap = list(cmap) + [c for c in omap[dk] if c not in cmap]
        # print(list(group))
        # print(list(dmap))
        gmap = {}
        for g in group:
            gmap[g] = omap[g]
        if dk in gmap:
            gmap[dk] = dmap
        # omap = omap.copy()
        # omap[dk] = dmap

        self.reverse = bool(reverse)
        self.cpos = dict.fromkeys(gmap, 0)

        # mask = False, False
        # mask = None
        # mask = (True, False)
        name = f"CMAP-{self.serial}"
        sub = LinkedArray(base=None, name=name)
        super().__init__(base=gmap, name=name, child=sub, **kw)

        self.serial = self.serial + 1

        self._loop = None
        self.var = 0
        self.cache = {}

        if chain:
            chain = chain()
        self.chain = chain

    def update(self, key):
        # idx = self.l2i(key)
        name = self.l2p(key)
        # locallog.debug(f"cmap:update: {key=} {idx=} {name=} {self.value(key)}")
        # base = self.base.get(name) or []
        cue = self.cpos[name]
        self.child.refresh(base=self.value(key), key=None, init=(cue, ))
        locallog.debug(f"cmap:update: {key=} {name=} {cue=}")
        # self.child.cue()

    def params(self, array=None, **kwds):
        locallog.debug(f"params: {kwds.keys()}")
        if not self._loop:
            self.loop()
        p = {}
        cmap = self.child.value()
        if cmap in self.cache:
            cc = self.cache[cmap]
            cmap = cc.get_cmap(self.reverse)
        elif self.reverse:
            cc = mplib.colormaps.get(cmap)
            if cc:
                cmap = cc.reversed()
        locallog.debug(f"{self.reverse=} {cmap=}")

        p['bind'] = self.bind
        p['method'] = self.method
        p['levels'] = self.levels
        p['alpha'] = self.alpha

        p['cmap'] = cmap

        locallog.debug(f"params: {self.chain=}")
        if self.chain:
            # self.chain.record()
            pp = self.chain.params(array, **kwds)
            # print(pp)
            p.update(pp)
        return p

    def loop(self):
        self._loop = iter(self.items())
        _ = next(self._loop)

    def bind(self, *, key=None, artist=None, **kwds):
        if key is None:
            key = self._current
        cname = self.child.value()
        # locallog.debug(f"bind: [{cname}]={artist}")
        # locallog.debug(f"bind: {artist.norm=}")
        # locallog.debug(f"bind: {artist.get_clim()=}")
        # locallog.debug(f"bind: {artist.get_array()=}")
        if cname not in self.cache:
            self.cache[cname] = CmapStatus(artist, self.reverse)
        if self.chain:
            self.chain.bind(artist=artist, **kwds)

    def transpose(self, switch=None, key=None):
        if switch is None:
            self.reverse = not bool(self.reverse)
        else:
            self.reverse = bool(switch)

        return self.reverse

    def toggle_lock(self, fig, switch=None, lev=None):
        if lev == 'norm':
            return self.chain.toggle_lock(fig, switch)

    def advance(self, step, lev=None):
        if lev == 'norm':
            return self.chain.advance(step)

        if lev is None:
            lev = 1

        if lev == 1:
            self.switch(step=0)
            self.child.switch(step=step)
        else:
            # remember previous
            pos = self.child.l2i()
            name = self.l2p()
            self.cpos[name] = pos

            self.switch(step=step)
            self.child.switch(step=0)

        try:
            v = next(self._loop)
        except StopIteration:
            self._loop = iter(self.items())
            v = next(self._loop)

        if lev == 1:
            # refresh current
            name = self.l2p()
            self.cpos[name] = True

        grp = self.l2p()
        cname = self.child.value()
        # print(grp, cname)
        return grp, cname

    def fwd(self, lev=None):
        return self.advance(+1, lev=lev)

    def bwd(self, lev=None):
        return self.advance(-1, lev=lev)

    @classmethod
    def show(self, stream=None, div=None, indent=None):
        lines = []
        if div is None:
            div = 10
        indent = ' ' * (indent or 0)
        for g in self.cmaps_:
            grp = g or 'default'
            nc = len(self.cmaps_[g])
            if div == 0:
                cc = ' '.join(c for c in self.cmaps_[g])
                lines.append(f"{indent}{grp}: {cc}")
            else:
                pfx = f"{indent}{grp}: "
                for j in range(0, nc, div):
                    cc = ' '.join(c for c in self.cmaps_[g][j:j+div])
                    lines.append(f"{pfx}{cc}")
                    pfx = ' ' * len(pfx)

        lines = '\n'.join(lines)
        if stream is False:
            return lines
        if stream is None:
            stream = sys.stdout
        return stream.write(lines + '\n')


class ContourParams(LimitParams, ParamsDispatcher, _ConfigType):
    """Contour parameter dispatcher."""
    # debug = locallog.info
    debug = locallog.debug

    colors_ = 'black'
    linewidths_ = [1.0, 2.0, 3.0, 4.0]
    clabels_ = -1               # contour annotation level

    def __init__(self, levels=None, **kwds):
        if not levels:
            if levels is not False:
                levels = True
        self.levels = levels
        self.debug(f"contour:levels: {levels}")
        super().__init__(**kwds)

    def params(self, array=None, **kwds):
        self.debug(f"{array.dims=}")
        p = {}

        upd, lims = self.update_lims(array)
        self.debug(f"contour:params:lims: {lims}")

        levels = []
        idxs = {}

        p['bind'] = self.bind

        if self.levels is False:
            p['levels'] = False
            ret = p
        elif self.levels is True:
            opts = self.get_opts(0, 1)
            p['levels'] = None
            p.update(opts)
            ret = p
        else:
            ret = []
            for j, lev in enumerate(self.levels):
                self.debug(f"{j}: {lev}")
                pp = p.copy()
                if lev in [True, False]:
                    lev = lev
                elif isinstance(lev, list):
                    lev = lev
                elif isinstance(lev, cabc.Callable):
                    lev = lev(*lims)
                else:
                    raise TypeError(f"invalid level specifier {lev}.")
                if isinstance(lev, cabc.Iterable):
                    lev = list(lev)
                    # if j == 0 and len(lev) > 1:
                    #     dc = [lev[j+1] - lev[j] for j in range(len(lev) - 1)]
                    #     mi, ma = min(dc), max(dc)
                    #     if (ma - mi) < max(abs(ma), abs(mi)) * 1.e-3:
                    #         pp['_info'] = (ma + mi) / 2
                    #     else:
                    #         pp['_info'] = lev
                    for prev in ret:
                        ladj = prev['levels']
                        if not isinstance(ladj, cabc.Iterable):
                            continue
                        for x in lev:
                            if x in ladj:
                                ladj.remove(x)
                        # print(f'{ladj=}')
                    # print(f'iterable: {lev}')
                pp['levels'] = lev
                opts = self.get_opts(j, len(self.levels))
                pp.update(opts)
                ret.append(pp)
            # self.debug(f"params={ret}")
            # ret = []
        self.debug(f"params={ret}")
        return ret

    def get_opts(self, idx, size):
        ignores = ['names', 'clabels', ]
        kw = {}
        for k in self.dict_recursive():
            if k in ignores:
                continue
            v = self.prop(k)
            if v is None:
                pass
            if isinstance(v, str):
                kw[k] = [v]
            elif isinstance(v, cabc.Iterable):
                if len(v) > idx:
                    kw[k] = v[idx]
                else:
                    kw[k] = v[-1]
            else:
                kw[k] = [v]
        clab = self.prop('clabels', 0)
        if clab < 0:
            clab = size + clab
        kw['clabel'] = (clab == idx)

        return kw

    def bind(self, *, key=None, artist=None, **kwds):
        self.debug(f"{artist=}")
        pass


class FigureInteractive(zplt.FigureCore, DataTree):
    """Matplotlib figure class with interactive methods."""

    _cache_patches = ['facecolor', 'linewidth', 'edgecolor', ]

    mark = {'linewidth': 20.0, 'edgecolor': 'blue', }
    cmap_lock = {'linewidth': 3.0, 'edgecolor': 'cyan', }

    def __init__(self, *args, **kwds):
        super().__init__(*args, **kwds)
        self.cid = {}
        self._loop = None
        self.trees = None
        self._lock = False
        self.sel = None
        self.base = None

        self.view = {}

        self.coll = None
        self.hooks = []
        self._hooks = True

        self.params = None      # plot parameter complex

        self.turn_dir = +1
        self.coords = None

    def connect(self, **handlers):
        for k, h in handlers.items():
            self.cid[k] = self.canvas.mpl_connect(k, h)

    def set_draw_hooks(self, *args):
        """Add on_draw hook"""
        self.connect(draw_event=self.run_hooks)
        self.hooks.extend(args)

    def toggle_hooks(self, switch=None):
        if switch is None:
            self._hooks = not self._hooks
        else:
            self._hooks = bool(switch)

    def run_hooks(self, event):
        if self._hooks:
            for h in self.hooks:
                # locallog.info(f'run_hooks: {h}')
                h(event)

    def add_draw_hooks(self, *args):
        self.hooks.extend(args)

    def disconnect(self, *keys):
        keys = keys or self.cid.keys()
        for k in keys:
            if k in self.cid:
                self.canvas.mpl_disconnect(self.cid[k])
                self.cid[k] = None

    def bind(self, trees, base, view=None):
        self.root = trees
        self.trees = trees
        self.base = base
        if view:
            self.view = view.copy()
        else:
            self.view = {}

    def toggle_lock(self, force=None):
        p = self._lock
        if force is not None:
            self._lock = bool(force)
        else:
            self._lock = not self._lock
        if self._lock:
            if not p:
                self.push_patch(**self.mark)
                # self.diag_patches()
        else:
            if p:
                self.pop_patch()
                # self.diag_patches()
        return self._lock

    def is_locked(self):
        return self._lock

    def info(self, pfx=None, msg=None, sync=False):
        if sync:
            self.sync()
        pfx = pfx or ''
        txt = pfx + self.sel
        if self._lock:
            txt = txt + ' *'
        if msg:
            txt = txt + f' -- {msg}'
        if msg is not False:
            print(txt)

    def message(self, msg=None, *, pfx=None, sel=None,
                cr=None, end=None, stream=None, flush=None):
        """Show message on steram."""
        if msg is False:
            return
        if stream is None:
            stream = sys.stdout

        if pfx is None:
            pfx = True
        if pfx is True:
            jfig = self.number
            pfx = f"({jfig}) "

        # cr = False
        if cr is None:
            cr = True
        cr = '\r' if cr else ''

        if end is None:
            end = True
        if end is True:
            end = '\n'

        if sel is None:
            sel = self.sel

        txt = cr + pfx
        if sel:
            txt = txt + sel
        if self._lock:
            txt = txt + ' *'
        if msg:
            txt = txt + f' -- {msg}'
        if end:
            txt = txt + end
        stream.write(txt)
        if bool(flush):
            stream.flush()

    def sync_message(self, *args, **kwds):
        """Message after sync."""
        self.sync()
        self.message(*args, **kwds)

    def sync(self):
        """Synchronize current status"""
        locallog.debug('sync/status enter')
        self.sel = self.trees.status(recurse=True)
        locallog.debug('sync/status exit')

    def len(self):
        n = len(self.trees)
        return n

    def loop(self, step=True):
        if step:
            if self._loop is None:
                self._loop = iter(self.trees.items())

            try:
                cur = next(self._loop)
                # locallog.debug(f'loop/status enter {step}')
                self.sel = self.trees.status(recurse=True)
                # locallog.debug(f'loop/status exit {step}')
                return cur
            except StopIteration:
                self._loop = None
                cur = None
                self.sel = None
                raise
        elif self._loop is None:
            cur = None, None
        else:
            # locallog.debug(f'loop/status enter {step}')
            k = self.trees.current
            cur = k, self.trees[k]
            self.sel = self.trees.status(recurse=True)
            # locallog.debug(f'loop/status exit {step}')
        return cur

    def restore_view(self, axs):
        """Restore current (previous) figure extent."""
        prev = self.loop(step=False) or (None, None)
        _, pdata = prev
        if axs:
            ext = axs.get_extent()
        else:
            ext = None
        if pdata and ext:
            parr = pdata[-1]
            y, x, c = ext
            # print(f"restore_view: {parr.coords}")
            if self.coords:
                dims = self.coords
            else:
                dims = parr.dims
            # print(f"restore_view: {self.coords} {parr.dims} {x=} {y=}")
            # dict order must be guaranteed (python >= 3.9)
            # print(self.restore_clims(parr, parr.dims[0], x[0], x[1]))
            # print(self.restore_clims(parr, parr.dims[1], y[0], y[1]))
            prev = self.restore_clims(parr, dims[0], x[0], x[1])
            prev = self.restore_clims(parr, dims[1], y[0], y[1], view=prev)
            # prev[parr.dims[0]] = self.restore_clims(parr, parr.dims[0], x[0], x[1])
            # prev = {parr.dims[0]: slice(x[0], x[1], None),
            #         parr.dims[-1]: slice(y[0], y[1], None), }
            if c:
                prev[c[0]] = c[1]
        else:
            prev = {}
        # locallog.info(f"{prev}")
        return prev

    def restore_clims(self, arr, dim, low, high, step=None, view=None):
        view = view or {}
        co = arr[dim]
        if np.issubdtype(co.dtype, np.datetime64):
            low = mplib.dates.num2date(low)
            high = mplib.dates.num2date(high)
        view[dim] = slice(low, high, step)
        return view

    def cache_view(self, prev=None):
        """Update cache of view properties."""
        self.view.update(prev or {})
        return self.view

    def set_coords(self, arr, coords=None):
        """Set figure coordinate."""
        coords = coords or ()
        # print(f"{arr.dims=} {coords=}")
        odims = list(arr.dims)
        for c in coords:
            if c not in arr.coords:
                break
            if any(d not in arr.dims for d in arr[c].dims):
                break
            for d in arr[c].dims:
                if d in odims:
                    odims.remove(d)
        else:
            if not odims:
                self.coords = tuple(coords)
                return
        self.coords = ()

    def parse_view(self, arr, src, draw=None, crs=None, **kwds):
        """Set figure coordinate."""
        # debug = locallog.info
        debug = locallog.debug
        # coords = coords or ()
        view = {}
        if self.coords:
            # print(f"{self.coords=}")
            view['coords'] = tuple(self.coords)
        # for c in coords:
        #     if c not in arr.coords:
        #         break
        #     if any(d not in arr.dims for d in arr[c].dims):
        #         break
        # else:
        #     view['coords'] = tuple(coords)
        # if all(c in arr.coords for c in coords):
        #     # view['coords'] = [arr[c] for c in coords]
        # debug(f"{draw=} {coords=}")
        # debug(f"{arr.dims=}")
        # print(arr.coords)
        draw = draw or {}
        dims = src.dims
        # print(f"parse_view: {self.view=}")
        if crs:
            view['extent'] = self.view.get(crs)

        # for co in arr.coords.keys():
        #     print(co)
        for co in arr.dims + (self.coords or ()):
            try:
                s = coord_prop(co, arr, dims, self.view, draw)
            except KeyError:
                continue
            # print(f"parse_view/coord_prop:{co}: {s}")
            if isinstance(s, slice):
                mm = {}
                if isinstance(s.start, int):
                    mm['dmin'] = src.coords[co][s.start].item()
                elif s.start is not None:
                    mm['dmin'] = s.start
                if isinstance(s.stop, int):
                    mm['dmax'] = src.coords[co][s.stop-1].item()
                elif s.start is not None:
                    mm['dmax'] = s.stop
                if mm:
                    view[co] = mm
        locallog.debug(f"parse_view: {view=}")
        return view


class FigureControl():
    """Figure iteration controller."""

    def __init__(self, pic, plot, root,
                 interactive=True,
                 figure=None, layout=None,
                 params=None,
                 styles=None, draw=None, coords=None,
                 config=None, rc=None):
        self.parse_config(config, rc)

        self.draw = draw or {}
        self.coords = coords or ()
        self.plot = plot
        self.pic = pic
        self.layout = layout
        self.figure = figure or FigureInteractive

        if inspect.isclass(self.pic):
            self.pic = self.pic(FigureClass=self.figure,
                                LayoutClass=self.layout)

        self.figs = {}
        if not isinstance(root, (list, tuple, )):
            root = [root]
        self.root = root
        self.output = None
        self._interactive = interactive
        self.styles = styles or {}
        self.params = params
        # print(f"{self.coords=}")
        self._cache_events = []
        self.view = None

        if self._interactive:
            self.load()

        self.pc = 0

    def load(self, backends=None):
        backends = backends or ['qt5agg', 'qtagg', ]
        for be in backends:
            try:
                mplib.use(be)
                break
            except ValueError as exc:
                pass
        else:
            be = mplib.get_backend()
            locallog.warning(f"{be} may not work as expected at window resizing.")

    def __call__(self, output=None, path=None, **kwds):
        self.output = output or None
        self.opath = path
        if self._interactive:
            sub = self.interactive
            # sub = self.animate
        else:
            sub = self.batch

        for base in self.root:
            fig, axs = self.new_control(**kwds)
            self.figs[fig] = axs
            trees = base.copy()
            fig.bind(trees, base=base)
            sub(fig)
        if self._interactive:
            plt.show()

    def new_control(self, *args, **kwds):
        fig, axs = self.pic(*args, **kwds)
        mpfunc = ft.partial(self.mouse_press, fig=fig, axs=axs)
        mrfunc = ft.partial(self.mouse_release, fig=fig, axs=axs)
        mmfunc = ft.partial(self.mouse_motion, fig=fig, axs=axs)
        # mofunc = ft.partial(self.mouse_pick, fig=fig, axs=axs)
        odfunc = ft.partial(self.on_draw, fig=fig, axs=axs)
        # mmfunc = None
        if self._interactive:
            fig.connect(button_press_event=mpfunc,
                        scroll_event=mpfunc,
                        button_release_event=mrfunc,
                        motion_notify_event=mmfunc, )
            # pick_event=mofunc
            fig.set_draw_hooks(self.prompt, odfunc)

        eafunc = ft.partial(self.enter_axes, fig=fig, axs=axs)
        lafunc = ft.partial(self.leave_axes, fig=fig, axs=axs)

        fig.connect(axes_enter_event=eafunc,
                    axes_leave_event=lafunc, )

        # button_release_event=self.test_release
        # print(fx)
        # fig = fx[0]
        # fig.canvas.mpl_connect('pick_event', self.onpick)
        # fig.canvas.mpl_connect('button_press_event', self.mouse_press)
        # fig.canvas.mpl_connect('figure_enter_event', self.enter_figure)
        # fig.canvas.mpl_connect('figure_leave_event', self.leave_figure)
        return fig, axs

    def interactive(self, fig, step=True, msg=None, prev=None):
        axs = self.figs[fig]
        jfig = fig.number
        fig.disconnect('key_press_event')
        fig.canvas.set_cursor(mbt.Cursors.WAIT)
        prev = prev or fig.restore_view(axs)
        try:
            trees, stat = fig.loop(step)
        except StopIteration:
            fig.message("no more data.")
            try:
                trees, stat = fig.loop(step)
            except StopIteration:
                raise StopIteration(f"\r({jfig}) no effective data.") from None

        try:
            fig.message("drawing...", end='', sel=False, flush=True)
            artists = self.invoke(trees, stat, fig, axs, prev)
            # for a in artists:
            #     gid = a.get_gid()
            #     print(gid, a)
            # for a in artists:
            #     if isinstance(a, mplib.text.Text):
            #         fm = a.get_fontproperties()
            #         gid = a.get_gid()
            #         print(gid, a, a.get_size(), fm.get_size())
            #         # fm.set_size(20.5)
            #         # print(gid, a, a.get_size(), fm.get_size())
            # print(artists)      #
            fig.message(msg)
        except UserWarning as err:
            fig.message(err)

        if artists:
            fig.canvas.draw()

    def batch(self, fig):
        axs = self.figs[fig]
        n = fig.len()
        while True:
            try:
                trees, stat = fig.loop()
                artists = self.invoke(trees, stat, fig, axs)
                self.savefig(fig, ref=n)
            except StopIteration:
                break

    def animate(self, fig):
        # nfig, axs = self.new_control(figsize=fig, )
        # ref = fig.base.copy(init=True, recurse=True)
        # nfig.bind(ref, base=fig.base)
        # fig = nfig
        nfig, axs = self.pic(figsize=fig, )
        # axs = self.figs[fig]
        frames = []
        # fig.disconnect('key_press_event')
        # fig.canvas.stop_event_loop()
        # axs.cla(fig)
        while True:
            try:
                trees, stat = fig.loop(step=True)
                artists = self.invoke(trees, stat, fig, axs, cla=False)
                # frames.append([artists[0]])
                frames.append(artists)
                # print(trees)
            except StopIteration:
                break
        ani = animation.ArtistAnimation(
            nfig,
            frames,
            interval=50,
            blit=False,  # blitting can't be used with Figure artists
            repeat_delay=10,
        )

        nfig.draw_artist(frames[0][0])
        nfig.canvas.draw()

        # nfig.canvas.manager.show()

        # ani.save("movie.mp4")
        plt.show()
        return

    def invoke(self, trees, stat, fig, axs, view=None, cla=None):
        """Draw core."""
        arr = stat[-1].load()
        src = fig.source_data()
        style = self.view_style(arr, src) or {}
        layout = {k: style.get(k) for k in ['projection', ]}
        fig.cache_view(view)
        fig.set_coords(arr, self.coords)
        view = fig.parse_view(arr, src, self.draw, **style)
        self.view = view.get('coords')
        # print(f"{self.view=}")
        # print(f"{view=}")
        cla = True if cla is None else bool(cla)
        if cla:
            axs.reset(fig, body=layout)
            axs.cla(fig)
        # if locallog.is_debug():
        #     for ch in fig.get_children():
        #         gid = ch.get_gid()
        #         locallog.debug(f"[{gid}]={ch}")
        # fig.set_hook(self.prompt)
        # params = self.params.get_link(key=trees, fig=fig, array=arr)
        # exclude axis
        params = self.view_params(fig, True, 'axis', src=src, array=arr)
        vv = self.view_params(fig, 'axes', src=src, array=arr)
        view = view | (vv.get('axes') or {})
        # print(f"{vv=}")
        # print(f"{view=}")
        axis = self.axis_params(fig, array=arr)
        # print(f"{axis=}")
        # print(params.params())
        # locallog.info("invoke: before plot")
        # print(view, style)
        # print(f"{arr.shape=} {view=}")
        r = self.plot(fig=fig, axs=axs, data=arr,
                      view=view, body=style, **params, **axis)
        # locallog.info(f"invoke: after plot")
        return r

    def view_params(self, fig, *args, src=None, array=None,
                    coords=None, params=None, **kwds):
        src = fig.source_data() if src is None else src
        array = fig.draw_data() if array is None else array
        coords = coords or self.view
        pp = self.params(*args, fig=fig, src=src, array=array,
                         coords=coords, **kwds)
        if params:
            pp = params | pp
        return pp

    def axis_params(self, fig, array=None, **kwds):
        arg = 'axis'
        array = fig.draw_data() if array is None else array
        params = {}
        for dim in array.dims:
            p = self.params(arg, fig=fig, name=dim, **kwds)
            params[dim] = p[arg]
        params = {arg: params}
        return params

    def get_disp(self, arg, fig, src=None, array=None,
                 coords=None, default=None, **kwds):
        src = fig.source_data() if src is None else src
        array = fig.draw_data() if array is None else array
        coords = coords or self.view
        disp = self.params.get_disp(arg, fig=fig, src=src, array=array,
                                    coords=coords,
                                    default=default, **kwds)
        return disp

    def prompt(self, event):
        """Event handler"""
        if locallog.is_debug():
            pfx = f"{self.pc}"
            self.pc = self.pc + 1
        else:
            pfx = ''
        print(f'\r{pfx}> ', end=' ', flush=True)
        if self._interactive and event:
            event.canvas.flush_events()
            fig = event.canvas.figure
            fig.connect(key_press_event=self.event_handler)
            fig.canvas.set_cursor(mbt.Cursors.POINTER)

    def view_style(self, arr, src, default=None):
        """Get style properties corresponding coordinate combination
        pattern."""
        try:
            style = view_prop(arr, src.dims, self.styles)
        except KeyError:
            return default
        return style

    def diag(self, fig):
        child = fig.trees
        while True:
            print(f"<{child.name}> {child.step}")
            child = child.child
            if not child:
                break

    # def _is_locked(self, jfig):
    #     fig, _ = self.figs[jfig]
    #     return fig.is_locked()

    def event_handler(self, event):
        """Event handler"""
        kev = event.key

        fig = event.canvas.figure
        hseq = self.kmap.get(kev)
        clstab = {'variable': VariableIter,
                  'file': FileIter,
                  None: ArrayIter,
                  }
        if hseq:
            cmd, hseq = hseq[0], hseq[1:]
            sub = hseq[0] if hseq else None
            cls = clstab.get(sub)
            if cmd == 'quit':
                if hseq:
                    plt.close(fig)
                    del self.figs[fig]
                    fig.disconnect('key_press_event')
                    fig.message("closed", sel=False)
                    self.prompt(event=None)
                else:
                    for fig in self.figs:
                        plt.close(fig)
                    self.figs = []
                if not self.figs:
                    print("quit.")

            elif cmd == 'animate':
                locallog.warning("not yet implemetend.")
                self.prompt(event=None)
                # self.animate_mode(fig, cls=cls, step=+1)
                pass
            elif cmd == 'rewind':
                self.map_figures(self.rewind, fig)
            elif cmd.startswith('next_inner_'):
                target = cmd.removeprefix('next_inner_')
                target = -1 - zu.toint(target)
                self.map_figures(self.turn_or_switch, fig, target, +1)
            elif cmd.startswith('next_outer_'):
                target = cmd.removeprefix('next_outer_')
                target = zu.toint(target)
                self.map_figures(self.turn_or_switch, fig, target, +1)
            elif cmd.startswith('turn_inner_'):
                target = cmd.removeprefix('turn_inner_')
                target = -1 - zu.toint(target)
                self.map_figures(self.turn_or_switch, fig, target)
            elif cmd.startswith('turn_outer_'):
                target = cmd.removeprefix('turn_outer_')
                target = zu.toint(target)
                self.map_figures(self.turn_or_switch, fig, target)
            elif cmd.startswith('toggle_turn'):
                self.map_figures(self.toggle_turn, fig)
            elif cmd == 'next_aspect_ratio':
                self.map_figures(self.turn_aratio, fig)
            elif cmd == 'next_cyclic':
                if sub == 'coordinate':
                    self.map_figures(self.permute_draw, fig, +1)
                elif sub == 'anchor':
                    self.map_figures(self.permute_anchor, fig, +1)
                else:
                    self.iterate_slice(fig, cls=cls, step=+1)
            elif cmd == 'prev_cyclic':
                if sub == 'coordinate':
                    self.map_figures(self.permute_draw, fig, -1)
                elif sub == 'anchor':
                    self.map_figures(self.permute_anchor, fig, -1)
                else:
                    self.iterate_slice(fig, cls=cls, step=-1)
            elif cmd == 'next':
                if sub == 'coordinate':
                    self.map_figures(self.permute_draw, fig, +1)
                else:
                    self.iterate_slice(fig, cls=(True, cls), step=+1)
            elif cmd == 'prev':
                if sub == 'coordinate':
                    self.map_figures(self.permute_draw, fig, -1)
                else:
                    self.iterate_slice(fig, cls=(True, cls), step=-1)
            elif cmd == 'clear':
                if sub == 'anchor':
                    self.map_figures(self.permute_anchor, fig, 0)
            elif cmd == 'info':
                figs = [fig] if hseq else self.figs
                for f in figs:
                    self.show_info(f)
            elif cmd == 'info_detail':
                figs = [fig] if hseq else self.figs
                for f in figs:
                    self.show_info(f, props=True)
            elif cmd == 'mark':
                if hseq:
                    self.toggle_mark(fig)
                else:
                    for fig in self.figs:
                        self.toggle_mark(fig)
            elif cmd == 'unmark':
                for fig in self.figs:
                    self.toggle_mark(fig, force=False)
            elif cmd == 'transpose':
                self.map_figures(self.transpose, fig)
            elif cmd == 'enlarge':
                self.map_figures(self.resize, fig, +self.opts['resize_step'])
            elif cmd == 'shrink':
                self.map_figures(self.resize, fig, -self.opts['resize_step'])
            elif cmd == 'sync_geometry':
                self.map_figures(self.resize, fig, fig)
            elif cmd == 'reset_geometry':
                self.map_figures(self.resize, fig, True, ref=False)
            elif cmd == 'print':
                self.map_figures(self.savefig, fig)
            elif cmd == 'turn_cmap':
                self.map_figures(self.turn_cmap, fig, step=+1)
            elif cmd == 'turn_norm':
                self.map_figures(self.turn_norm, fig, step=+1)
            elif cmd == 'reverse_horizontal':
                self.map_figures(self.toggle_axis, fig, 'h')
            elif cmd == 'reverse_vertical':
                self.map_figures(self.toggle_axis, fig, 'v')
            elif cmd == 'redraw':
                self.map_figures(self.redraw, fig)
            elif cmd == 'toggle_visible':
                self.map_figures(self._toggle_visible, fig)
            elif cmd == 'entire_view':
                self.map_figures(self.entire_view, fig)
            elif hasattr(self, cmd):
                cmd = getattr(self, cmd)
                cmd(fig)
            # else:
            #     print(f"Not yet implemented command={cmd}.")

    def mouse_handler(self, event, fig, axs, lab, cmd, aux=None, step=None):
        """Mouse event handler core."""
        wlock = fig.canvas.widgetlock.locked()
        locallog.debug(f"[{wlock}] {lab=} {cmd=} {aux=}")
        cmd = cmd or ''

        clstab = {'variable': VariableIter,
                  'file': FileIter,
                  None: ArrayIter, }

        if cmd == 'select' and not wlock:
            if lab == 'body':
                self.spine_selection(fig, axs, lab, aux, event)
        elif cmd == 'anchor' and not wlock:
            if lab == 'body':
                self.spine_selection(fig, axs, lab, aux, event,
                                     anchor=True)
        elif cmd.startswith('select_') and not wlock:
            sub = cmd.index('_')
            sub = (cmd[sub+1:], )
            if lab == 'body':
                self.point_selection(fig, axs, lab, sub, event)
        elif cmd.startswith('anchor_') and not wlock:
            sub = cmd.index('_')
            sub = (cmd[sub+1:], )
            if lab == 'body':
                self.point_selection(fig, axs, lab, sub, event, anchor=True)
        elif cmd == 'next_cyclic':
            if lab == 'body':
                if aux:
                    self.switch_draw(fig, aux, +1)
                elif step != 0:
                    self.iterate_slice(fig, cls=clstab[None], step=step)
            elif lab == 'colorbar' and not wlock:
                self.map_figures(self.turn_cmap, fig, step=+1)
        elif cmd == 'prev_cyclic':
            if lab == 'body':
                if aux:
                    self.switch_draw(fig, aux, -1)
                elif step != 0:
                    self.iterate_slice(fig, cls=clstab[None], step=step)
            elif lab == 'colorbar'  and not wlock:
                self.map_figures(self.turn_cmap, fig, step=-1)
        elif cmd == 'next_group':
            if lab == 'colorbar'  and not wlock:
                self.map_figures(self.turn_cmap, fig, lev=0, step=+1)
        elif cmd == 'prev_group':
            if lab == 'colorbar'  and not wlock:
                self.map_figures(self.turn_cmap, fig, lev=0, step=-1)
        elif cmd == 'reverse':
            if lab == 'body' and aux:
                self.map_figures(self.toggle_axis, fig,
                                 ax=lab, which=aux[0])
            elif lab == 'colorbar':
                self.map_figures(self.toggle_axis, fig,
                                 ax=lab, which=None)
        elif cmd == 'transpose':
            if lab == 'body' and aux:
                self.map_figures(self.transpose, fig)
        elif cmd == 'next_norm':
            if lab == 'colorbar' and not wlock:
                self.map_figures(self.turn_norm, fig, step=+1)
            elif lab == 'body' and not wlock:
                self.map_figures(self.turn_axis_scale, fig,
                                 which=aux[0], step=+1)
        elif cmd == 'lock':
            if lab == 'colorbar':
                self.map_figures(self.lock_cmap_range, fig, ax=lab)
            elif lab == 'info':
                self.map_figures(self.lock_contour_range, fig, ax=lab)

        # print()
        # ax = event.inaxes
        # if ax:
        #     gid = ax.get_gid()
        #     print(f"mouse_press[{gid}] {axs} {event.button} {event.dblclick} {event.step}"
        #           f" ({event.x}, {event.y})"
        #           f" ({event.xdata}, {event.ydata})")
        # if ax:
        # event.inaxes.patch.set_facecolor('yellow')
        # event.canvas.draw()

    def animate_mode(self, fig, *args, step=None, **kwds):
        if fig.is_locked():
            targets = [f for f in self.figs if f.is_locked()]
        else:
            targets = [fig]
        for ff in targets:
            child = ff.trees
            child.switch(*args, step=0)
            child.switch(*args, step=step, **kwds)
            self.animate(ff)
        return

    def iterate_slice(self, fig, *args, step=None, **kwds):
        if fig.is_locked():
            targets = [f for f in self.figs if f.is_locked()]
        else:
            targets = [fig]

        for ff in targets:
            axs = self.figs[ff]
            prev = ff.restore_view(axs)
            child = ff.trees
            child.switch(*args, step=0)
            child.switch(*args, step=step, **kwds)
            self.interactive(ff, prev=prev)

    def map_figures(self, func, fig, *args, **kw):
        if fig.is_locked():
            targets = [f for f in self.figs if f.is_locked()]
        else:
            targets = [fig]
        # if self._is_locked(jfig):
        #     targets = filter(self._is_locked, self.figs.keys())
        # else:
        #     targets = [jfig]
        for ff in targets:
            func(ff, *args, **kw)

    def savefig(self, fig, ref=None):
        axs = self.figs[fig]
        output = self.output
        if isinstance(output, cabc.Callable):
            output = self.output(fig, axs, ref=ref)
        if output:
            # fig.diag_patches()
            if self._interactive:
                axs.toggle_guides(fig, False)
                fig.switch_patch(0)
                text = axs.pop_monitor()
            # locallog.info('before savefig')
            fig.toggle_hooks(False)
            canvas = fig.canvas

            if hasattr(output, 'savefig'):
                output.savefig(fig)
            else:
                fig.savefig(output)

            fig.toggle_hooks(True)
            fig.canvas = canvas
            # locallog.info('after savefig')
            if self._interactive:
                axs.toggle_guides(fig, True)
                fig.pop_patch()
                axs.monitor(fig, text)

            path = self.opath or output
            try:
                pc = output.get_pagecount()
                saved = f'{path}[{pc}]'
            except AttributeError:
                saved = path
            fig.message(f"Saved: {saved}")
            self.prompt(event=None)
            # locallog.info('before monitor')
            # locallog.info('after monitor')
        else:
            locallog.warning(f"No output defined.")

    def toggle_mark(self, fig, force=None):
        """Toggle figure grouping."""
        axs = self.figs[fig]
        axs.toggle_guides(fig, False)
        fig.toggle_lock(force)
        fig.canvas.draw()

    def permute_anchor(self, fig, step):
        """Entry for anchor-coordinate permutation."""
        fig.permute_anchor(step)
        msg = 'anchor permuted.' if step else 'anchor cleared.'
        fig.sync_message(msg)
        self.prompt(event=None)

    def permute_draw(self, fig, step):
        """Entry for draw-coordinate permutation."""
        axs = self.figs[fig]
        prev = fig.restore_view(axs)
        fig.permute_draw(step)
        self.interactive(fig, step=False, msg='permuted', prev=prev)

    def switch_draw(self, fig, coord, step):
        """Entry for draw-coordinate switching."""
        axs = self.figs[fig]
        prev = fig.restore_view(axs)
        fig.switch_draw(coord, step)
        self.interactive(fig, step=False, msg='switched', prev=prev)

    def turn_or_switch(self, fig, target, step=None):
        """Entry for draw-coordinate switching or turning."""
        axs = self.figs[fig]
        prev = fig.restore_view(axs)
        if step is None:
            step = fig.turn_dir
        rcv = fig.turn_or_switch(target, step)
        if rcv is None:
            self.interactive(fig, step=False, msg='permuted', prev=prev)
        else:
            self.interactive(fig, msg=False, prev=prev)
            fig.post_turn(rcv)
            fig.sync_message('turned')
            self.prompt(event=None)

    def toggle_turn(self, fig, turn=None):
        """Entry for toggle to turn direction."""
        cur = fig.turn_dir
        if turn is None:
            turn = - fig.turn_dir
        fig.turn_dir = turn
        if cur != fig.turn_dir:
            msg = f'toggle turn to {turn}'
            fig.message(msg)
            self.prompt(event=None)

    def transpose(self, fig):
        """Entry for draw-coordinate transposition."""
        axs = self.figs[fig]
        prev = fig.restore_view(axs)
        fig.transpose()
        self.interactive(fig, step=False, msg='transposed', prev=prev)

    def resize(self, fig, figsize=None, ref=None):
        axs = self.figs[fig]
        axs.toggle_guides(fig, False)
        axs.resize(fig, figsize=figsize, ref=ref)
        # fig.canvas.draw()

    def redraw(self, fig):
        axs = self.figs[fig]
        axs.toggle_guides(fig, False)
        self.interactive(fig, step=False, msg='refresh')
        # fig.canvas.draw()

    def turn_cmap(self, fig, step=None, lev=None):
        step = step or +1
        disp = self.get_disp('color', fig)
        if disp:
            if step > 0:
                g, c = disp.fwd(lev=lev)
            else:
                g, c = disp.bwd(lev=lev)
            try:
                c = c.name
            except AttributeError:
                pass
            g = g or 'default'
            self.interactive(fig, step=False, msg=f'colormap[{g}] {c}')

    def turn_norm(self, fig, step=None):
        step = step or +1
        disp = self.get_disp('color', fig)
        # locallog.info(f"turn_norm: {step} {disp=}")
        if disp:
            if step > 0:
                n = disp.fwd(lev='norm')
            else:
                n = disp.bwd(lev='norm')
            n = n or 'default'
            self.interactive(fig, step=False, msg=f'colormap norm: {n}.')

    def turn_axis_scale(self, fig, which, step=None):
        step = step or +1
        array = fig.draw_data()

        if which in ['left', 'right', ]:
            dim = array.dims[0]
        elif which in ['top', 'bottom', ]:
            dim = array.dims[1]
        else:
            return
        disp = self.get_disp('axis', fig, name=dim)
        if disp:
            if step > 0:
                sc = disp.fwd()
            else:
                sc = disp.bwd()
            # locallog.info(f"turn_axis_scale: {which=} {step=} {disp=} {sc=}")
            sc = sc or 'default'
            self.interactive(fig, step=False,
                             msg=f'{which} ({dim}) scale: {sc}.')

    def lock_cmap_range(self, fig, ax=None, switch=None):
        axs = self.figs[fig]
        disp = self.get_disp('color', fig)
        if disp:
            b = disp.toggle_lock(fig, switch, lev='norm')
            # if b:
            #     fig.group_push_patch(facecolor='gray', ax=(ax, 'spine'))
            # else:
            #     fig.group_pop_patch(ax=(ax, 'spine'))
            # ax = axs.get_axes(ax)
            # bg = axs.bg.get(ax)
            # if bg:
            #     fig.canvas.restore_region(bg)
            #     if switch != b:
            #         if b:
            #             at = m1i.BboxPatch(ax.bbox,
            #                                edgecolor='cyan', linewidth=10.0)
            #             ax.draw_artist(at)
            #             fig.canvas.blit(ax.bbox)
            b = 'locked' if b else 'unlocked'
            self.interactive(fig, step=False, msg=f'colormap range: {b}')

    def lock_contour_range(self, fig, ax=None, switch=None):
        axs = self.figs[fig]
        disp = self.get_disp('contour', fig)
        if disp:
            b = disp.toggle_lock(switch)
            wh = 'locked' if b else 'unlocked'
            msg = f'contour range: {wh}'
            fig.message(msg)
            self.prompt(event=None)

    def turn_aratio(self, fig, step=None):
        step = step or +1
        disp = self.get_disp('axes', fig)
        # locallog.info(f"turn_norm: {step} {disp=}")
        if disp:
            if step > 0:
                ar = disp.fwd()
            else:
                ar = disp.bwd()
            self.interactive(fig, step=False, msg=f'aspect ratio: {ar}.')
        axs = self.figs[fig]
        axs.set_aspect(ar)
        fig.canvas.draw()

    def _toggle_visible(self, fig):
        axs = self.figs[fig]
        axs.toggle_visible(ax='colorbar')
        fig.canvas.draw()

    def toggle_axis(self, fig, which, ax=None):
        axs = self.figs[fig]
        if ax in ['colorbar', ]:
            disp = self.get_disp('color', fig)
            if disp:
                disp.transpose()
            self.interactive(fig, step=False, msg='reverse colormap')
        else:
            axs.toggle_guides(fig, False)
            axs.toggle_axis(which, ax=ax)
            fig.canvas.draw()

    def entire_view(self, fig):
        """Entire view mode."""
        axs = self.figs[fig]
        prev = fig.restore_view(axs)
        prev = prev.fromkeys(prev)
        self.interactive(fig, step=False, msg='entire', prev=prev)

    def rewind(self, fig, anchor=None):
        axs = self.figs[fig]
        prev = fig.restore_view(axs)
        fig.rewind(anchor=anchor)
        self.interactive(fig, step=False, msg='rewind', prev=prev)

    def duplicate(self, fig):
        """Duplicate figure."""
        axs = self.figs[fig]
        prev = fig.restore_view(axs)
        ref = fig.trees.copy(init=False, recurse=True)
        return self._duplicate(fig, ref, prev)

    def refresh(self, fig):
        """New fresh figure, close old."""
        nfig = self.duplicate(fig)
        jnew = nfig.number
        plt.close(fig)
        del self.figs[fig]
        fig.disconnect('key_press_event')
        fig.message(f"regenerated > ({jnew})")
        self.prompt(event=None)
        # print(f"\r({jfig}) regenerated > ({jnew})")

    def new(self, fig):
        """New fresh figure."""
        ref = fig.base.copy(init=True, recurse=True)
        self._duplicate(fig, ref)

    def _duplicate(self, fig, trees, view=None):
        """Duplicate figure core"""
        nfig, nax = self.new_control(figsize=fig, )
        self.figs[nfig] = nax
        nfig.canvas.manager.show()

        nfig.bind(trees, base=fig.base, view=view)
        self.interactive(nfig)

        return nfig

    def diag_childlen(self, fig):
        jfig = fig.number
        CH = []
        for ch in fig.get_children():
            gid = ch.get_gid()
            CH.append(f"({jfig}) [{gid}]={ch}")
        for ch in sorted(CH):
            locallog.debug(ch)

    def info_data(self, fig):
        src = fig.source_data()
        array = fig.draw_data()

        txt = f"<{src.name}>"
        name = src.attrs.get('long_name')
        units = src.attrs.get('units')
        if name:
            txt = txt + f" {name}"
        if units:
            txt = txt + f" [{units}]"
        print(txt)
        print("Dimensions:")
        for d, s in zip(src.dims, src.shape):
            print(f"  {d}: {s}")
        print("Coordinates:")
        for cn in reversed(list(src.coords.keys())):
            co = src[cn]
            dims = ','.join(d for d in co.dims)
            txt = f"{cn}[{dims}]"
            name = co.attrs.get('long_name')
            units = co.attrs.get('units')
            if name:
                txt = txt + f" - {name}"
            if units:
                txt = txt + f" [{units}]"
            ca = array[cn]
            if ca.shape == ():
                txt = txt + f" {ca.item()}"
            print(f"  {txt}")
        # print(src.coords.xindexes)
        # print(src.coords)

    def show_info(self, fig, **kwds):
        fig.message('info')
        self.info_data(fig)
        if kwds:
            fig.show_info(**kwds)
            axs = self.figs[fig]
            axs.show_info(**kwds)
        # if locallog.is_debug():
        #     self.diag_childlen(fig)
        #     axs = self.figs[fig]
        #     for ax, bg in axs.bg.items():
        #         gid = ax.get_gid() or ax
        #         print(f"bg[{gid}]={bg}")
        self.prompt(event=None)

    def mouse_release(self, event, fig, axs):
        if fig.canvas.widgetlock.locked():
            self._cache_events.pop(-1)

    def mouse_motion(self, event, fig, axs):
        aux = None
        if fig.canvas.widgetlock.locked():
            if event.button:
                aux = self._cache_events[-1]
        lab = axs.retrieve_event(event)
        # locallog.debug(f"mouse_motion {lab=}")
        # self.monitor_point(fig, axs, lab, event, aux=aux)
        if lab == 'body' or locallog.is_debug():
            self.monitor_point(fig, axs, lab, event, aux=aux)
        if fig.canvas.widgetlock.locked():
            pass
        else:
            lgrp = lab[0] if isinstance(lab, tuple) else lab
            if lgrp == 'spine':
                self.monitor_spine(fig, axs, lab, event)

    def enter_axes(self, event, fig, axs):
        ax = event.inaxes
        bg = axs.bg.get(ax)
        gid = ax.get_gid() or ax
        gid = gid if isinstance(gid, tuple) else (gid, )
        # print(f'enter_axes {gid} {bg}')
        # if False:
        if bg:
            bg, bb = bg
            if gid[0] in ['spine', 'axis']:
            # at = ax.text(0, 0, 'yellow')
                # at = m1i.BboxPatch(ax.bbox, facecolor='gray', alpha=0.2)
                at = m1i.BboxPatch(bb, facecolor='gray', alpha=0.2)
                # at.set_visible(True)
                fig.canvas.restore_region(bg)
                ax.draw_artist(at)
                # fig.canvas.blit(ax.bbox)
                # fig.canvas.blit(bb)
                fig.canvas.blit()
        # event.inaxes.patch.set_facecolor('yellow')
        # event.canvas.draw()

    def leave_axes(self, event, fig, axs):
        ax = event.inaxes
        axs.clear_guide(fig, ax)
        gid = ax.get_gid() or ax
        gid = gid if isinstance(gid, tuple) else (gid, )
        # print(f'leave {gid=}')
        if len(gid) > 1:
            axs.clear_guide(fig, gid[1])
        # bg = axs.bg.get(ax)
        # gid = ax.get_gid() or ax
        # gid = gid if isinstance(gid, tuple) else (gid, )
        # # gid = ax.get_gid() or ax
        # # print(f'leave_axes {gid} {bg}')
        # if bg:
        #     if gid[0] in ['spine', 'axis']:
        #         fig.canvas.restore_region(bg)
        #         fig.canvas.blit(ax.bbox)
        # print('leave_axes', event.inaxes)
        # event.inaxes.patch.set_facecolor('white')
        # event.canvas.draw()

    # def enter_figure(self, event):
    #     print('enter_figure', event.canvas.figure)
    #     event.canvas.figure.patch.set_facecolor(('red', 0.2))
    #     event.canvas.draw()

    # def leave_figure(self, event):
    #     print('leave_figure', event.canvas.figure)
    #     event.canvas.figure.patch.set_facecolor(('grey', 0.2))
    #     event.canvas.draw()

    def point_selection(self, fig, axs, lab, sub, event, anchor=None):
        x, y = (event.xdata, event.ydata)
        x, y = axs.get_position(x, y, lab)
        prev = fig.restore_view(axs)
        co = list(prev.keys())

        # pos = (event.x, event.y)
        # dpos = axs.position_transform(*pos, ax=lab)
        # print((x, y), dpos)

        if sub[0] in ['x', 'h', ]:
            nsel = {co[1]: x}
        elif sub[0] in ['xy', 'hv', ]:
            nsel = {co[0]: y, co[1]: x, }
        elif sub[0] in ['y', 'v', ]:
            nsel = {co[0]: y}
        else:
            return
        fig.point_selection(nsel, anchor=anchor)
        self.interactive(fig, step=False, msg='point', prev=prev)

    def spine_selection(self, fig, axs, lab, aux, event, anchor=None):
        prev = fig.restore_view(axs)
        co = list(prev.keys())
        which = aux[0]
        if which in ['left', 'right', ]:
            pos = (None, event.y)
        elif which in ['top', 'bottom', ]:
            pos = (event.x, None)
        else:
            return
        dpos = axs.position_transform(*pos, ax=lab)
        nsel = {}
        for c, p in zip(co, dpos[::-1]):
            if p is not None:
                nsel[c] = float(p)
        # locallog.debug(f"{nsel=}")
        locallog.debug(f"{aux=} ({pos}) ({dpos})")
        fig.point_selection(nsel, anchor=anchor)
        self.interactive(fig, step=False, msg='section', prev=prev)

    def monitor_spine(self, fig, axs, lab, event, aux=None):
        debug = ft.partial(self.debug, func='monitor_spine')
        body = lab[1]
        # ## Need to disable (further) transformation for guides
        dpos = axs.position_transform(event.x, event.y, ax=body, crs=False)
        mpos = axs.position_transform(event.x, event.y, ax=body)
        axs.draw_guide(fig, event.inaxes, event.xdata, event.ydata, pos=dpos)

        fmt = r'{:.2f}'
        if lab[2] in ['left', 'right']:
            # axs.draw_guide(fig, body, None, dpos[1])
            axs.draw_guide(fig, body, None, event.ydata)
            text = 'y=' + (fmt.format(mpos[1]))
        else:
            # axs.draw_guide(fig, body, dpos[0], None)
            axs.draw_guide(fig, body, event.xdata, None)
            text = 'x=' + (fmt.format(mpos[0]))
        axs.monitor(fig, text)

    def monitor_point(self, fig, axs, lab, event, aux=None, fmt=None):
        """Monitor point."""
        fmt = fmt or r'({x:.2f}, {y:.2f})'
        # fmt = fmt or r'({x}, {y})'

        x, y = (event.xdata, event.ydata)
        # print(x, y)
        x, y = axs.get_position(x, y, lab)
        if all(j is not None for j in [x, y]):
            if aux:
                ox, oy = (aux.xdata, aux.ydata)
                ox, oy = axs.get_position(ox, oy, lab)
                if ox and oy:
                    if fig.canvas.toolbar.mode == 'pan/zoom':
                        ex, ey, _ = axs.get_extent()
                        text = (fmt.format(x=ox, y=oy)
                                + '\n' + fmt.format(x=ex[0], y=ey[0])
                                + '\n--' + fmt.format(x=ex[1], y=ey[1]))
                    else:
                        text = (fmt.format(x=ox, y=oy)
                                + '\n--' + fmt.format(x=x, y=y))
                else:
                    text = fmt.format(x=x, y=y)
            else:
                text = fmt.format(x=x, y=y)
                # print(fig.get_points(x=x, y=y))
        else:
            text = '(null)'
        if locallog.is_debug():
            dbgfmt = r'<{x:.0f}, {y:.0f}>'
            x, y = (event.x, event.y)
            text = dbgfmt.format(x=x, y=y)+ '\n' + text
            ax = event.inaxes
            if ax:
                gid = ax.get_gid()
                if gid:
                    text = f"{gid}\n" + text
        axs.monitor(fig, text)

    def on_draw(self, event, fig, axs):
        # print('-' * 20)
        # for st in inspect.stack():
        #     stf = plib.Path(st.filename)
        #     stf = '/'.join(stf.parts[-2:])
        #     print(f"{stf}:{st.lineno}:{st.function}: {st.code_context[0]}", end='')
        # print('-' * 20)
        # locallog.info(f'on_draw {event}')

        # self.diag_childlen(fig)
        axs.on_draw(fig)
        # self.diag_childlen(fig)

        # for ax in fig.get_children():
        #     gid = ax.get_gid()
        #     if gid == 'body':
        #         for ch in ax.get_children():
        #             try:
        #                 print(ch, ch.get_cmap())
        #             except AttributeError:
        #                 pass

    # def mouse_pick(self, event, fig, axs):
    #     """Mouse pick actions on artists."""
    #     debug = ft.partial(self.debug, func='mouse_pick')
    #     this = event.artist
    #     mouse = event.mouseevent

    #     gid = this.get_gid()
    #     aux = gid.split(':')
    #     lab = aux.pop(0)

    #     nm = self.normalize_mouse(mouse)
    #     mseq = self.mmap.get(nm) or {}
    #     cmds = mseq.get(lab) or ()
    #     # debug(f"{mouse=} {mseq=} {lab=} {cmds=}")
    #     cmds = cmds or (None, )

    #     if False:
    #         self.mouse_handler(event, fig, axs, lab, cmds[0], cmds[1:], aux)

    def mouse_press(self, event, fig, axs):
        """Mouse button actions on axes."""
        debug = ft.partial(self.debug, func='mouse_press')
        lab = axs.retrieve_event(event)

        if fig.canvas.widgetlock.locked():
            self._cache_events.append(event)

        mouse, step = self.normalize_mouse(event)
        mseq = self.mmap.get(mouse) or {}

        debug(f"{lab=}")
        debug(f"{mseq=} {step=}")
        if isinstance(lab, tuple):
            k = lab
            aux = lab[2:]
            lab = lab[1]
            while len(k) > 1:
                if k in mseq:
                    cmds = mseq[k]
                    break
                k = k[:-1]
            else:
                k = k[0]
                cmds = mseq.get(k) or ()
        else:
            cmds = mseq.get(lab) or ()
            aux = None
        self.mouse_handler(event, fig, axs, lab, cmds, aux=aux, step=step)

    def normalize_mouse(self, event):
        """Normalize mouse event to string."""
        # locallog.debug(f"normalize_mouse: {event.button}")
        mm = []
        step = None
        if event.key:
            if event.key == 'alt':
                if self.opts.get('ignore_alt') is not False:
                    locallog.warning('alt-key press is ignored.')
                else:
                    mm.append(event.key)
            else:
                mm.append(event.key)
        if event.dblclick:
            mm.append('double')
        if event.button == mbb.MouseButton.LEFT:
            mm.append('left')
        elif event.button == mbb.MouseButton.RIGHT:
            mm.append('right')
        elif event.button == mbb.MouseButton.MIDDLE:
            mm.append('middle')
        elif event.button == mbb.MouseButton.FORWARD:
            mm.append('fwd')
            step = event.step
        elif event.button == mbb.MouseButton.BACK:
            mm.append('bwd')
            step = event.step
        elif event.button == 'up':
            mm.append('fwd')
            step = event.step
        elif event.button == 'down':
            mm.append('bwd')
            step = event.step
        else:
            mm.append(str(event.button))
        if step is not None:
            u = self.opts.get('scroll_unit') or 0.0
            if u == 0.0:
                if step > 0:
                    step = +1
                elif step < 0:
                    step = -1
                else:
                    step = 0
            else:
                step = int(step / u)
        mm = '+'.join(mm)
        return mm, step

    def parse_config(self, config, rcparams):
        config = config or {}
        verbose = config.get('verbose', 0)
        self.kmap = self.parse_keymap({}, config.get('keymap', {}))
        self.mmap = self.parse_mousemap({}, config.get('mouse', {}))
        # locallog.debug(self.mmap)
        # ppr.pprint(config.get('mouse'))
        # ppr.pprint(self.kmap)
        # ppr.pprint(self.mmap)
        self.opts = config.get('option', {})
        self.opts['resize_step'] = self.opts.get('resize_step') or 0.25

        if rcparams:
            for k in rcparams.find_all(r'^keymap\.*'):
                for p in rcparams[k]:
                    if p in self.kmap:
                        locallog.warning(f"Remove default keymap for <{p}>"
                                         f" from {k}.")
                        rcparams[k].remove(p)

    def parse_keymap(self, kmap, config, group=()):
        for f, c in config.items():
            v = (f, ) + group
            if isinstance(c, dict):
                kmap = self.parse_keymap(kmap, c, v)
            elif isinstance(c, list):
                for k in c:
                    kmap[k] = v
            elif c != '':
                kmap[c] = v
        return kmap

    def parse_mousemap(self, mmap, config, group=(), lev=0):
        for f, c in config.items():
            # v = (f, ) + group
            v = group + (f, )
            if isinstance(c, dict):
                # self.parse_mousemap(mmap, c, group + (f, ))
                mmap = self.parse_mousemap(mmap, c, v, lev+1)
            else:
                lab = v[:-1]
                if len(lab) <= 1:
                    lab = lab[0]
                cmd = v[-1]
                if isinstance(c, list):
                    for k in c:
                        # mmap.setdefault(k, {})[v[0]] = v[1:]
                        mmap.setdefault(k, {})[lab] = cmd
                elif c != '':
                    # mmap.setdefault(c, {})[v[0]] = v[1:]
                    mmap.setdefault(c, {})[lab] = cmd
        return mmap

    def debug(self, msg, func=None, **kwds):
        tags = ['control', ]
        if func:
            tags.append(func)
        tag = ':'.join(t for t in tags if t)
        locallog.debug(f"({tag}) {msg}", **kwds)


def extract_sels(sel, data, strict=None):
    """Extract effective and normalized selections for data."""
    esel = dict.fromkeys(data.dims)  # extracted selection
    nsel = {}                        # normalized selection
    sel = sel or {}
    for co, sp in sel.items():
        try:
            co = parse_coord(data, co)
            esel[co] = sp
            assert (co in data.coords), f"{co} is non-coordinate dimension."
            cv = data.coords[co]
            cw = len(cv)
            if isinstance(sp, slice):
                start, stop = sp.start, sp.stop
                bstart = isinstance(start, int)
                bstop = isinstance(stop, int)
                if bstart:
                    if start < 0:
                        start = start + cw
                    start = cv[start].item()
                if bstop:
                    if stop < 0:
                        stop = stop + cw
                    stop = cv[stop-1].item()
                # elif stop is not None:
                #     # start = float(start)
                #     stop = math.ceil(stop - 1)
                if not (bstart and bstop):
                    if cv[0].item() > cv[-1].item():
                        start, stop = stop, start
                        locallog.info(f"Exchange {co} limit range as"
                                      f"({start}, {stop}).")
                sp = slice(start, stop, sp.step)
            elif isinstance(sp, int):
                sp = cv[sp].item()
            else:
                sp = normalize_selection(cv, sp)
            nsel[co] = sp
        except UserWarning as exc:
            msg = f"no coordinate to select: {co}"
            if strict is True:
                raise UserWarning(msg) from exc
            locallog.warning(msg)
        except (IndexError, KeyError) as exc:
            msg = f"out of range to select {co}=[{sp}]."
            locallog.warning(msg)
            raise UserWarning(msg) from exc

    # workaround for NotImplementedError in xarray
    if any(isinstance(sp, slice) for sp in nsel.values()):
        for co in nsel:
            # workaround for NotImplementedError in xarray
            xs = esel[co]
            if not isinstance(xs, (slice, int)):
                nsel[co] = normalize_selection(data.coords[co],
                                               xs, index=False)
                # xs = data.coords[co].sel({co: xs}, method='nearest')
                # nsel[co] = xs.item()
    return esel, nsel


def extract_base_array(data, nsel, xsel):
    """Extract base array according to normalized selection."""
    base = data
    if not nsel:
        pass
    elif any(isinstance(j, slice) for j in nsel.values()):
        base = base.sel(nsel)
    else:
        isel = {}
        fsel = {}
        for co, sp in nsel.items():
            # print(co, sp)
            if co not in data.coords:
                if isinstance(sp, float):
                    msg = f"{co} has no associated coordinate {sp}."
                    raise ValueError(msg)
                isel[co] = sp
            else:
                fsel[co] = sp
        if isel:
            base = base.isel(isel)
        if fsel:
            base = base.sel(fsel, method='nearest')
            # print(nsel)
    # print(base.shape)
    # print(base)
    return base


def gen_mask(data, coords, anchors=None, empty=None):
    """Create mask array for iteration."""
    if not isinstance(coords, (tuple, list)):
        coords = tuple(coords)
    anchors = anchors or {}
    empty = empty or slice(None, None, None)

    dims = data.dims
    # w = len(shape)
    mask = []
    for d in dims:
        mask.append(d in anchors)
    for c in coords:
        if c in dims:
            c = dims.index(c)
            mask[c] = empty
    return type(coords)(mask)


def draw_coords(data, src, dims, coords, default,
                anchors=None, skip_single=True):
    """Extract target coordinates to draw figure.
    Number of coordinates to return is the same as that of argument default.
    Input data should be source data for most cases."""

    # debug = locallog.info
    debug = locallog.debug

    coords = coords or default
    if not isinstance(coords, (tuple, list)):
        coords = tuple(coords)

    debug(f"{coords} << {src.dims}")
    debug(f"   {anchors=}")
    debug(f"   {data.shape}")
    debug(f"   {data.dims}")

    xco = []
    for c in coords:
        if c in src.coords and not c in src.dims:
            co = src.coords[c]
            for d in co.dims:
                if d not in xco:
                    xco.append(d)
            cdims = ','.join(d for d in co.dims)
            locallog.info(f"Non-dimension coordinate {c}[{cdims}].")
        else:
            xco.append(c)
    xco = type(coords)(xco)
    if xco != coords:
        xco = type(coords)(d for d in xco if d)
        # locallog.info(xco)

    anchors = anchors or {}
    targets = []
    # for c in coords:
    for c in xco:
        try:
            c = parse_coord(src, c)
            cj = src.dims.index(c)
            if skip_single and src.shape[cj] < 2:
                continue
            targets.append(c)
        except UserWarning:
            if c in dims:
                targets.append(dims.index(c) - len(dims))
            else:
                targets.append('')
            if c != '':
                locallog.warning(f"no coordinate to plot {c}.")
    # locallog.debug(f"first try: {targets=}")
    rem = len(src.dims)
    ntargets = []
    def is_valid(c, default=''):
        try:
            d = src.dims[c]
        except IndexError:
            return default
        if d not in data.dims:
            return default
        if d in targets:
            return default
        if d in anchors:
            return default
        if skip_single and src.shape[c] < 2:
            return default
        return d

    for c in reversed(targets):
        if isinstance(c, int):
            c = is_valid(c)
        if c == '':
            for c in reversed(range(rem)):
                d = is_valid(c)
                if d == '':
                    continue
                break
            else:
                msg = f"no valid coordinate to plot for {src.dims}"
                raise UserWarning(msg)
            rem = c
            c = d
        ntargets.insert(0, c)
    ntargets = type(coords)(ntargets)
    if len(set(ntargets)) != len(coords):
        locallog.error(f"data shape: {data.dims} = {data.shape}")
        raise ValueError(f"Invalid target coordinates {coords} >> {ntargets}")

    debug(f"result: {ntargets=}")
    return ntargets


def filter_coords(arr, dims, *args):
    """Filter property from arr coordinates."""
    props = {}
    for co in arr.dims:
        try:
            v = coord_prop(co, arr, dims, *args)
            if v is not False:
                props[co] = v
        except KeyError:
            pass
    return props


def coord_prop(co, arr, dims, *args):
    """Extract property of coordinate from maps."""
    for kw in args:
        try:
            v = match_coordinate(arr, co, kw)
            return v
        except KeyError:
            continue
    if co in dims:
        co = dims.index(co)
        for kw in args:
            for j in [co, co - len(dims)]:
                if j in kw:
                    v = kw[j]
                    return v
    elif co in arr.coords:
        for kw in args:
            if co in kw:
                return kw[co]
        return None
    raise KeyError(f"No match to coordinate {co}")


def view_prop(arr, dims, *args):
    """Extract property of coordinate combination from maps."""
    for kw in args:
        for cc in kw:
            if not isinstance(cc, tuple):
                cc = (cc, )
            if len(cc) != len(arr.dims):
                continue
            for ct, co in zip(cc, arr.dims):
                if not collate_coord(ct, co, arr, dims):
                    break
            else:
                # print(cc, kw[cc].keys())
                return kw[cc]
    prop = {}
    for kw in args:
        for cc, cd in kw.items():
            if isinstance(cc, tuple):
                continue
            if isinstance(cc, int):
                cc = dims[cc]
            for cj, co in enumerate(arr.dims):
                # print(cj, co, cc)
                if co == cc:
                    for ck, cv in cd.items():
                        if ck not in prop:
                            prop[ck] = [None] * len(arr.dims)
                        prop[ck][cj] = cv
    if not prop:
        raise KeyError(f"No match to view {arr.dims}")
    else:
        for ck, cv in prop.items():
            prop[ck] = tuple(cv)
            # print(ck, tuple(type(t) for t in cv))
            # for t in cv:
            #     if t:
            #         print(t.transform_point(359, 100, t))
        return prop


def collate_coord(pat, co, arr, dims):
    """check if coordinate matches to pattern."""
    if pat == co:
        return True
    for a in ['long_name', 'standard_name', ]:
        if arr.coords[co].attrs.get(a) == pat:
            return True
    if isinstance(pat, int):
        if co in dims:
            co = dims.index(co)
            if co == pat:
                return True
            if co - len(dims) == pat:
                return True
    return False


def parse_coord(data, name):
    """Cooridnate either by index or (long-)name."""
    md = len(data.dims)
    if isinstance(name, int):
        if name >= md or name + md < 0:
            raise UserWarning(f"coordinate index {name} out of range.")
        name = data.dims[name]
    elif name in data.dims:
        pass
    else:
        try:
            name = search_coordinate(data, name)
        except KeyError as exc:
            raise UserWarning from exc
    return name


def is_match_coordinate(array, name, pat):
    """check if coordinate matches to pattern."""
    if name == pat:
        return True
    for a in ['long_name', 'standard_name', ]:
        if array.coords[name].attrs.get(a) == pat:
            return True
    return False


def search_coordinate(array, name):
    """Search coordinate key corresponding to name using conventions."""
    if name in array.dims:
        return name

    for d in array.dims:
        for a in ['long_name', 'standard_name', ]:
            if array.coords[d].attrs.get(a) == name:
                return d

    raise KeyError(f"No coordinate corresponding to {name}")


def match_coordinate(array, name, kwds):
    """Search coordinate matches in kw."""
    if name in array.dims:
        if name in kwds:
            return kwds[name]
        co = array.coords[name]
        for a in ['long_name', 'standard_name', ]:
            a = co.attrs.get(a)
            if a and a in kwds:
                return kwds[a]

    raise KeyError(f"No match corresponding to coordinate {name}")


def is_cyclic_coord(co):
    """Check if coordinate is cyclic."""
    return co.attrs.get('cyclic_coordinate', False)


def normalize_selection(co, sel, method=None, index=None):
    """Return index or normalized coordinate corresponding to sel."""
    method = method or 'nearest'
    locallog.debug(f"[{co.name}]{co.shape} {sel=}")
    if len(co.shape) > 1:
        locallog.warning("Cannot handle multi-dimension coordinate"
                         f" {co.name}")
        return None

    c0 = co.values[0]
    locallog.debug(f"{c0=} {type(c0)=} {co.dtype=}")
    if isinstance(c0, np.datetime64):
        if isinstance(sel, str):
            sel = pd.Timestamp(sel)
        else:
            sel = np.datetime64(mplib.dates.num2date(sel))
        locallog.debug(f"date: {sel=} {type(sel)=} ")
    elif isinstance(c0, cftime.datetime):
        cal = c0.calendar
        if isinstance(sel, str):
            pts = pd.Timestamp(sel)
            cft = cftime.to_tuple(pts)
            sel = cftime.datetime(*cft, calendar=cal)
        else:
            # print(type(c0), cal)
            ### "days since 2000-01-01",
            sel = cftime.num2date(sel, units=zu.NC_EPOCH, calendar=cal)
        locallog.debug(f"date: {sel=} {type(sel)=} ")

    cc = is_cyclic_coord(co)
    if cc:
        _, org, dup = cc
        if org > dup:
            org, dup = dup, org
        cyc = dup - org
        if sel < org:
            while sel < org:
                sel = sel + cyc
        elif sel > dup:
            while sel > dup:
                sel = sel - cyc
    d = co.name
    sel = co.sel({d: sel}, method=method)
    locallog.debug(f"sel: {sel=} {type(sel)}")
    if isinstance(c0, np.datetime64):
        if bool(index):
            for j, v in enumerate(co.values):
                if v == sel:
                    # print(f"found: {j} {sel}")
                    sel = j
                    break
            else:
                # print(f"not found")
                sel = None
        return sel
        # cp = co.to_numpy()
        # print(cp)
        # print([np.where(cp == sel)])
    else:
        sel = sel.item()
    # print(f"sel.item {sel=} {type(sel)}")
    if bool(index):
        # print(type(co), co)
        co = co.to_index()
        # print(type(co), co)
        sel = co.get_loc(sel)
    return sel

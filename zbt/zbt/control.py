#!/usr/bin/env python3
# Time-stamp: <2024/12/13 15:37:31 fuyuki control.py>

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

import numpy as np
import xarray as xr
import matplotlib as mplib
import matplotlib.pyplot as plt
import matplotlib.artist as mart
import matplotlib.animation as animation
import matplotlib.backend_bases as mbb
import mpl_toolkits.axes_grid1.inset_locator as m1i

import cartopy.util as cutil

import zbt.plot as zplt
import zbt.xrnio as zxr
import zbt.util as zu


locallog = zu.LocalAdapter('control')


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

    def append(self, item, key=None):
        """Append item to array."""
        if isinstance(self.base, list):
            self.base.append(item)
            self.refresh(base=self.base, key=key)
        else:
            raise TypeError(f"invalid item to append on {type(self.base)}.")

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
            debug(f"{pdims=}")
            debug(f"{self.draw=}")
            debug(f"{src.dims=}")
            debug(f"{base.dims=}")

            asel = filter_coords(base, src.dims, self.anchors)
            debug(f"{asel=}")
            csel = draw_coords(base, src, pdims, self.draw,
                               self.coords, anchors=asel)
            debug(f"{csel=}")
            mask = gen_mask(base, csel, asel)
            debug(f"{mask=}")
            cue = self.adjust_cue(base, asel) or None
            debug(f"{cue=}")

            debug("return")
            self.draw = csel
            self.data = None

            return super().refresh(base=base,
                                   key=base.shape, mask=mask, init=cue, **kwds)

        debug("return (no base)")
        return super().refresh(base=base, **kwds)

    def permute_draw(self, switch, skip_single=True):
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
                break
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
        debug(f"{self.draw=}")
        debug(f"{mask=}")
        self.switch(mask=tuple(mask))

        self.data = None

        return self.draw

    def switch_draw(self, aux, step, skip_single=True):
        """Draw coordinates switching."""
        debug = ft.partial(self.debug, func='switch_draw')
        mask = self.mask
        shape = self.shape
        coord = None
        which = aux[1]
        if which in ['left', 'right', ]:
            coord = self.draw[0]
        elif which in ['top', 'bottom', ]:
            coord = self.draw[-1]

        dims = self.base.dims
        if coord not in dims:
            debug(f"return ({coord})")
            return

        tgt = dims.index(coord)
        if step > 0:
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

        debug(f"{coord} to {tgt}")
        debug(f"{self.draw} to {nd}")

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
        debug(f"{self.anchors=}")
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

    def point_selection(self, sel, anchor=None, skip_single=True):
        """Point selection."""
        debug = ft.partial(self.debug, func='point_selection')
        debug(f"{type(self)=}")

        debug(f"{sel=}")

        base = self.base
        mask = self.mask
        shape = self.shape
        debug(f"{base.dims=}")
        debug(f"{mask=}")
        debug(f"{shape=}")

        arr = self.value()
        # debug(f"{arr.dims=} {arr.shape=}")
        nco = len(arr.dims)

        asel = filter_coords(base, self.src.dims, self.anchors)
        debug(f"{asel=}")

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
            debug(f"try[{t}]: {ctmp}")
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
        debug(f"{csel=}")
        if anchor:
            asel.update(sel)
        mask = gen_mask(base, csel, asel)
        debug(f"{mask=}")
        cue = self.adjust_cue(base, asel, cue=sel) or None
        debug(f"{cue=}")

        debug("return")
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
        debug(f"{cidx=}")
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
        debug(f"{cnxt=}")
        cue = []
        for d in dims:
            sel = cnxt.get(d)
            if sel is None:
                sel = True
            elif isinstance(sel, (int, slice)):
                pass
            else:
                sel = coords[d].sel({d: sel}, method='nearest')
                sel = sel.item()
                co = coords[d].to_index()
                sel = co.get_loc(sel)
            cue.append(sel)
        cue = tuple(cue)
        debug(f"{cue=}")

        return cue

    def update(self, key):
        debug = ft.partial(self.debug, func='update')
        debug(f"{key=}")
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
        debug(f"{self.draw}")
        return self.draw

    def _value(self, key=None):
        """draw-array adjustment."""
        debug = ft.partial(self.debug, func='_value')
        debug(f"{key=}")

        if self.data is not None:
            debug(f'cached=={key}')
            return self.data

        data = super()._value(key)
        debug(f"{data.dims=}")

        data = data.transpose(*self.draw)
        debug(f"{data.shape=}")
        debug('cyclic in')
        for cn in data.coords:
            if cn not in data.dims:
                continue
            ck = data.dims.index(cn)
            co = data[cn]
            cc = co.attrs.get('cyclic_coordinate')
            debug(f"{cn} {cc}")
            if not cc:
                continue
            w, org, dup = cc
            if len(co) == w - 1 and co[0] == org and co[-1] != dup:
                cd, cx = cutil.add_cyclic(data, co, axis=ck,
                                          cyclic=dup-org)
                nco = dict(data.coords.items())
                nco[cn] = xr.DataArray(cx, dims=co.dims, attrs=co.attrs)
                data = xr.DataArray(cd, coords=nco,
                                    dims=data.dims, attrs=data.attrs)
        debug('cyclic out')
        debug(f"{data.shape=}")
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

    def __init__(self, coords=None, dims=None, anchors=None, **kwds):
        super().__init__(base=None, name='VAR', **kwds)
        self.coords = coords
        self.lims = dims
        self.anchors = anchors or {}

    def update(self, key):
        """Update current and child status."""
        debug = ft.partial(self.debug, func='update')
        # define base data-array
        src = self.get(key)
        psrc = self.get()
        pdim = psrc.dims if psrc is not None else ()
        debug(f"{pdim} to {src.dims}")

        alink = self.linked(ArrayIter)

        if src is None:
            alink.refresh(base=None, key=0, )
            return

        src = self.assign_coord(src)
        debug(f"source='{src.name}'{src.dims}{src.shape}")

        try:
            xsel, nsel = extract_sels(self.lims, src)
            debug(f"{nsel=}")
            debug(f"{xsel=}")
        except UserWarning:
            alink.refresh(base=None, )
            return

        base = extract_base_array(src, nsel, xsel)
        debug(f"base='{base.name}'{base.dims}{base.shape}")

        alink.refresh(base=base, src=src, nsel=nsel, xsel=xsel)

    def assign_coord(self, data):
        """Assign default (index) coordinate if empty."""
        for d in data.dims:
            if not d in data.coords and len(data[d]) > 1:
                locallog.debug(f"Dummy index coordinate introduced [{d}].")
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

    def transpose(self, switch=None):
        """Call transpose() on array-link."""
        try:
            self._alink.transpose(switch)
        except AttributeError:
            pass

    def point_selection(self, sel, anchor=None):
        locallog.debug(f"datatree {sel=}")
        try:
            self._alink.point_selection(sel, anchor=anchor)
        except AttributeError as exc:
            locallog.debug(f"{exc}")

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


class NormIter(LinkedArray):
    def __init__(self, *args, **kw):
        if not args:
            args = ['', ]
        self.Norms = list(args)
        self.Rev = [False, True]

        shape = (len(self.Norms), len(self.Rev))
        super().__init__(base=None, key=shape, name='NORM', **kw)

        self._loop = iter(self.items())
        _ = next(self._loop)

    def _value(self, key=None):
        if key is None:
            key = self._current
        if isinstance(key, tuple):
            return self.Norms[key[0]], self.Rev[key[1]]
        return None

    def advance(self, step):
        self.switch(step=step)
        try:
            next(self._loop)
        except StopIteration:
            self._loop = iter(self.items())
            _ = next(self._loop)

    def fwd(self):
        self.advance(+1)

    def bwd(self):
        self.advance(-1)


class CmapIter(LinkedArray):
    def __init__(self, *args, **kw):
        if not args:
            args = [None]

        cmaps = [*args] + [k for k in mplib.colormaps.keys()
                           if not k.endswith(r'_r')]
        self._force = None
        super().__init__(base=cmaps[:10], name='CMAP', **kw)

        # cgrp = {}
        # for k, cm in mplib.colormaps.items():
        #     if k.endswith(r'_r'):
        #         continue
        #     g = type(cm)
        #     cgrp.setdefault(g, {})
        #     cgrp[g][k] = cm
        # ppr.pprint(cgrp)

        self._loop = iter(self.items())
        _ = next(self._loop)

    def put_or_get(self, cmap=None):
        if cmap is None:
            return self.value()
        return self.set_force(cmap or None)

    def set_force(self, cmap=None):
        self._force = cmap

    def _value(self, key=None):
        v = super()._value(key)
        print(v, self._force)
        return self._force or v

    def fwd(self):
        self.set_force()
        self.switch(step=+1)
        try:
            next(self._loop)
        except StopIteration:
            self._loop = iter(self.items())
            _ = next(self._loop)

    def bwd(self):
        self.set_force()
        self.switch(step=-1)
        try:
            next(self._loop)
        except StopIteration:
            self._loop = iter(self.items())
            _ = next(self._loop)


class FigureInteractive(zplt.FigureCore, DataTree):
    """Matplotlib figure class with interactive methods."""

    _cache_patches = ['facecolor', 'linewidth', 'edgecolor', ]

    mark = {'linewidth': 20.0, 'edgecolor': 'blue', }

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

    def connect(self, **handlers):
        for k, h in handlers.items():
            self.cid[k] = self.canvas.mpl_connect(k, h)

    def set_draw_hooks(self, *args):
        """Add on_draw hook"""
        self.connect(draw_event=self.run_hooks)
        self.hooks.extend(args)

    def run_hooks(self, event):
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

    # def permute_coords(self, switch):
    #     base = self.trees.inquire(prop='self', cls=VariableIter, single=True)
    #     if base:
    #         base.permute_coords(switch)

    # def point_selection(self, sel):
    #     base = self.trees.inquire(prop='self', cls=VariableIter, single=True)
    #     if base:
    #         base.point_selection(sel)

    # def permute_anchor(self, switch):
    #     base = self.trees.inquire(prop='self', cls=VariableIter, single=True)
    #     if base:
    #         base.permute_anchor(switch)

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
        self.canvas.draw()
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
        print(txt)

    def sync(self):
        """Synchronize current status"""
        locallog.debug('sync/status enter')
        self.sel = self.trees.status(recurse=True)
        locallog.debug('sync/status exit')

    def len(self):
        n = len(self.trees)
        return n

    def loop(self, step=True):
        # if self._loop is None:
        #     self._loop = iter(self.trees.items())
        #     step = True
        if step:
            if self._loop is None:
                self._loop = iter(self.trees.items())

            try:
                cur = next(self._loop)
                locallog.debug(f'loop/status enter {step}')
                self.sel = self.trees.status(recurse=True)
                locallog.debug(f'loop/status exit {step}')
                return cur
            except StopIteration:
                self._loop = None
                cur = None
                self.sel = None
                raise
        elif self._loop is None:
            cur = None, None
        else:
            locallog.debug(f'loop/status enter {step}')
            k = self.trees.current
            cur = k, self.trees[k]
            self.sel = self.trees.status(recurse=True)
            locallog.debug(f'loop/status exit {step}')
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
            # dict order must be guaranteed (python >= 3.9)
            prev = {parr.dims[0]: slice(x[0], x[1], None),
                    parr.dims[-1]: slice(y[0], y[1], None), }
            if c:
                prev[c[0]] = c[1]
        else:
            prev = {}
        return prev

    def cache_view(self, prev=None):
        """Update cache of view properties."""
        self.view.update(prev or {})
        return self.view

    def parse_view(self, arr, src, draw=None, crs=None, **kwds):
        """Set figure coordinate."""
        view = {}
        draw = draw or {}
        dims = src.dims

        if crs:
            view['extent'] = self.view.get(crs)

        for co in arr.dims:
            try:
                s = coord_prop(co, arr, dims, self.view, draw)
            except KeyError:
                continue
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
        return view


class FigureControl():
    """Figure iteration controller."""

    def __init__(self, pic, plot, root,
                 interactive=True,
                 figure=None, layout=None,
                 cmap=None, norm=None, styles=None, draw=None,
                 config=None, params=None):
        self.parse_config(config, params)

        self.draw = draw or {}
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
        self.cmap = cmap
        self.norm = norm

        self._cache_events = []

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

    def __call__(self, output=None, **kwds):
        self.output = output or None
        if self._interactive:
            sub = self.interactive
            # sub = self._animate
        else:
            # sub = self._animate
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
        prev = prev or fig.restore_view(axs)
        try:
            trees, stat = fig.loop(step)
        except StopIteration:
            print(f"\r({jfig}) no more data.")
            try:
                trees, stat = fig.loop(step)
            except StopIteration:
                raise StopIteration(f"\r({jfig}) no effective data.") from None

        arr = stat[-1]
        try:
            print(f'\r({jfig}) drawing...', end='', flush=True)
            artists = self.invoke(arr, fig, axs, prev)
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
            fig.info(pfx=f'\r({jfig}) ', msg=msg)
        except UserWarning as err:
            fig.info(pfx=f'\r({jfig}) ', msg=err)
        fig.canvas.draw()

    def batch(self, fig):
        axs = self.figs[fig]
        n = fig.len()
        while True:
            try:
                trees, stat = fig.loop()
                arr = stat[-1]
                artists = self.invoke(arr, fig, axs)
                self.savefig(fig, ref=n)
            except StopIteration:
                break

    # def _animate(self, jfig):
    #     fig, axs = self.figs[jfig]
    #     frames = []
    #     while True:
    #         try:
    #             trees, stat = fig.loop()
    #             data = stat[-1]
    #             p = self.invoke(data, fig, axs)
    #             frames.append([p])
    #             # self._savefig(jfig)
    #         except StopIteration:
    #             break
    #     def anim(p):
    #         print(p)
    #         return [p]
    #     print(frames)
    #     print(fig)
    #     # ani = animation.FuncAnimation(
    #     #     fig,
    #     #     anim,
    #     #     interval=50,
    #     #     blit=False,
    #     #     frames=frames,
    #     #     repeat_delay=100, )
    #     ani = animation.ArtistAnimation(
    #         fig,
    #         frames,
    #         interval=50,
    #         blit=False,  # blitting can't be used with Figure artists
    #         repeat_delay=100,
    #     )
    #     fig.draw_artist(frames[0][0])
    #     fig.canvas.draw()

    #     ani.save("movie.mp4")
    #     plt.show()

    def invoke(self, arr, fig, axs, view=None):
        """Draw core."""
        src = fig.source_data()
        style = self.view_style(arr, src) or {}
        layout = {k: style.get(k) for k in ['projection', ]}
        fig.cache_view(view)
        view = fig.parse_view(arr, src, self.draw, **style)
        axs.reset(fig, body=layout)
        axs.cla(fig)
        if locallog.is_debug():
            for ch in fig.get_children():
                gid = ch.get_gid()
                locallog.debug(f"[{gid}]={ch}")
        # fig.set_hook(self.prompt)
        # locallog.info("invoke: before plot")
        # print(view, style)
        r = self.plot(fig=fig, axs=axs, data=arr, view=view, body=style)
        # locallog.info(f"invoke: after plot")
        return r

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

    def switch(self, fig, *args, step=None, **kwds):
        if fig.is_locked():
            targets = [f for f in self.figs if f.is_locked()]
        else:
            targets = [fig]
        for fig in targets:
            axs = self.figs[fig]
            prev = fig.restore_view(axs)
            child = fig.trees
            child.switch(*args, step=0)
            child.switch(*args, step=step, **kwds)
            self.interactive(fig, prev=prev)

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
        jfig = fig.number
        axs = self.figs[fig]
        output = self.output
        if isinstance(output, cabc.Callable):
            output = self.output(fig, axs, ref=ref)
        if output:
            # fig.diag_patches()
            axs.toggle_guides(fig, False)
            fig.switch_patch(0)
            text = axs.pop_monitor()
            locallog.info('before savefig')
            if hasattr(output, 'savefig'):
                output.savefig(fig)
            else:
                fig.savefig(output)
            locallog.info('after savefig')
            axs.toggle_guides(fig, True)
            fig.pop_patch()
            fig.info(pfx=f'\r({jfig}) ', msg=f"Saved: {output}")
            locallog.info('before monitor')
            axs.monitor(fig, text)
            locallog.info('after monitor')
        else:
            locallog.warning(f"No output defined.")

    def permute_anchor(self, fig, step):
        """Entry for anchor-coordinate permutation."""
        jfig = fig.number
        fig.permute_anchor(step)
        msg = 'anchor permuted.' if step else 'anchor cleared.'
        fig.info(pfx=f'\r({jfig}) ', msg=msg, sync=True)
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

    def transpose(self, fig):
        """Entry for draw-coordinate transposition."""
        axs = self.figs[fig]
        prev = fig.restore_view(axs)
        fig.transpose()
        self.interactive(fig, step=False, msg='transposed', prev=prev)

    def resize(self, fig, figsize=None, ref=None):
        axs = self.figs[fig]
        axs.resize(fig, figsize=figsize, ref=ref)
        # fig.canvas.draw()

    def redraw(self, fig):
        fig.canvas.draw()

    def toggle_cmap(self, fig, step=None):
        step = step or +1
        if self.cmap:
            if step > 0:
                self.cmap.fwd()
            else:
                self.cmap.bwd()
            cmap = self.cmap.value()
            self.interactive(fig, step=False, msg=f'colormap: {cmap}')

    def toggle_norm(self, fig, step=None):
        step = step or +1
        self.norm.advance(step)
        norm = self.norm.value()
        # self.cmap.set_force('viridis')
        cmap = self.cmap.value()
        if norm:
            norm, revc = norm
            norm = norm or ''
            norm = [f"{norm}"]
            if revc:
                norm = norm + ["reversed"]
        else:
            norm = []
        norm = ','.join(norm)
        if norm:
            norm = f'[{norm}]'
        self.interactive(fig, step=False, msg=f'colormap: {cmap}{norm}')

    def _toggle_visible(self, fig):
        axs = self.figs[fig]
        axs.toggle_visible(ax='colorbar')
        fig.canvas.draw()

    def toggle_axis(self, fig, which, ax=None):
        axs = self.figs[fig]
        axs.toggle_guides(fig, False)
        axs.toggle_axis(which, ax=ax)
        fig.canvas.draw()

    def entire_view(self, fig):
        """Entire view mode."""
        axs = self.figs[fig]
        prev = fig.restore_view(axs)
        prev = prev.fromkeys(prev)
        self.interactive(fig, step=False, msg='entire', prev=prev)

    def duplicate(self, fig):
        """Duplicate figure."""
        axs = self.figs[fig]
        prev = fig.restore_view(axs)
        ref = fig.trees.copy(init=False, recurse=True)
        return self._duplicate(fig, ref, prev)

    def refresh(self, fig):
        """New fresh figure, close old."""
        nfig = self.duplicate(fig)
        jfig = fig.number
        jnew = nfig.number
        plt.close(fig)
        del self.figs[fig]
        fig.disconnect('key_press_event')
        print(f"\r({jfig}) regenerated > ({jnew})")

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

    def mouse_handler(self, event, fig, axs, lab, cmd, sub, aux=None):
        """Mouse event handler core."""
        wlock = fig.canvas.widgetlock.locked()
        locallog.debug(f"[{wlock}] {lab=} {cmd=} {sub=} {aux=}")
        if cmd == 'select' and not wlock:
            if lab == 'body':
                self.point_selection(fig, axs, lab, sub, event)
            elif lab == 'spine':
                if aux[0] == 'body':
                    self.spine_selection(fig, axs, lab, sub, aux, event)
        elif cmd == 'anchor' and not wlock:
            if lab == 'body':
                self.point_selection(fig, axs, lab, sub, event,
                                     anchor=True)
            elif lab == 'spine':
                if aux[0] == 'body':
                    self.spine_selection(fig, axs, lab, sub, aux, event,
                                         anchor=True)
        elif cmd == 'next_cyclic':
            if lab == 'axis':
                self.switch_draw(fig, aux, +1)
            elif lab == 'colorbar' and not wlock:
                self.map_figures(self.toggle_cmap, fig, step=+1)
        elif cmd == 'prev_cyclic':
            if lab == 'axis':
                self.switch_draw(fig, aux, -1)
            elif lab == 'colorbar'  and not wlock:
                self.map_figures(self.toggle_cmap, fig, step=-1)
        elif cmd == 'transpose':
            if lab == 'axis':
                self.map_figures(self.transpose, fig)
            elif lab == 'spine':
                self.map_figures(self.toggle_axis, fig,
                                 ax=aux[0], which=aux[1])
        elif cmd == 'next_norm':
            if lab == 'colorbar' and not wlock:
                self.map_figures(self.toggle_norm, fig, step=+1)

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
                    jfig = fig.number
                    print(f"\r({jfig}) closed")
                else:
                    for fig in self.figs:
                        plt.close(fig)
            elif cmd == 'next_cyclic':
                if sub == 'coordinate':
                    self.map_figures(self.permute_draw, fig, +1)
                elif sub == 'anchor':
                    self.map_figures(self.permute_anchor, fig, +1)
                else:
                    self.switch(fig, cls=cls, step=+1)
            elif cmd == 'prev_cyclic':
                if sub == 'coordinate':
                    self.map_figures(self.permute_draw, fig, -1)
                elif sub == 'anchor':
                    self.map_figures(self.permute_anchor, fig, -1)
                else:
                    self.switch(fig, cls=cls, step=-1)
            elif cmd == 'next':
                if sub == 'coordinate':
                    self.map_figures(self.permute_draw, fig, +1)
                else:
                    self.switch(fig, cls=(True, cls), step=+1)
            elif cmd == 'prev':
                if sub == 'coordinate':
                    self.map_figures(self.permute_draw, fig, -1)
                else:
                    self.switch(fig, cls=(True, cls), step=-1)
            elif cmd == 'clear':
                if sub == 'anchor':
                    self.map_figures(self.permute_anchor, fig, 0)
            elif cmd == 'info':
                if hseq:
                    jfig = fig.number
                    fig.info(pfx=f'\r({jfig}) info: ')
                else:
                    for fig in self.figs:
                        jfig = fig.number
                        fig.info(pfx=f'\r({jfig}) info: ')
            elif cmd == 'mark':
                if hseq:
                    fig.toggle_lock()
                else:
                    for fig in self.figs:
                        fig.toggle_lock()
            elif cmd == 'unmark':
                for fig in self.figs:
                    fig.toggle_lock(force=False)
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
            elif cmd == 'toggle_cmap':
                self.map_figures(self.toggle_cmap, fig)
            elif cmd == 'toggle_norm':
                self.map_figures(self.toggle_norm, fig)
            elif cmd == 'toggle_horizontal':
                self.map_figures(self.toggle_axis, fig, 'h')
            elif cmd == 'toggle_vertical':
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

    def normalize_mouse(self, event):
        """Normalize mouse event to string."""
        mm = []
        if event.key:
            mm.append(event.key)
        if event.dblclick:
            mm.append('double')
        if event.button == mbb.MouseButton.LEFT:
            mm.append('left')
        elif event.button == mbb.MouseButton.RIGHT:
            mm.append('right')
        elif event.button == mbb.MouseButton.MIDDLE:
            mm.append('middle')
        else:
            mm.append(str(event.button))
        return '+'.join(mm)

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

    # class Cursor:
    #     """
    #     A cross hair cursor.
    #     """
    #     def __init__(self, ax):
    #         self.ax = ax
    #         self.horizontal_line = ax.axhline(color='k', lw=0.8, ls='--')
    #         self.vertical_line = ax.axvline(color='k', lw=0.8, ls='--')
    #         # text location in axes coordinates
    #         self.text = ax.text(0.72, 0.9, '', transform=ax.transAxes)

    #     def set_cross_hair_visible(self, visible):
    #         need_redraw = self.horizontal_line.get_visible() != visible
    #         self.horizontal_line.set_visible(visible)
    #         self.vertical_line.set_visible(visible)
    #         self.text.set_visible(visible)
    #         return need_redraw

    #     def on_mouse_move(self, event):
    #         if not event.inaxes:
    #             need_redraw = self.set_cross_hair_visible(False)
    #             if need_redraw:
    #                 self.ax.figure.canvas.draw()
    #         else:
    #             self.set_cross_hair_visible(True)
    #             x, y = event.xdata, event.ydata
    #             # update the line positions
    #             self.horizontal_line.set_ydata([y])
    #             self.vertical_line.set_xdata([x])
    #             self.text.set_text(f'x={x:1.2f}, y={y:1.2f}')
    #             self.ax.figure.canvas.draw()

    def enter_axes(self, event, fig, axs):
        ax = event.inaxes
        bg = axs.bg.get(ax)
        gid = ax.get_gid() or ax
        gid = gid if isinstance(gid, tuple) else (gid, )
        # print(f'enter_axes {gid} {bg}')
        if bg:
            if gid[0] in ['spine', 'axis']:
            # at = ax.text(0, 0, 'yellow')
                at = m1i.BboxPatch(ax.bbox, facecolor='gray', alpha=0.2)
                # at.set_visible(True)
                fig.canvas.restore_region(bg)
                ax.draw_artist(at)
                fig.canvas.blit(ax.bbox)
        # event.inaxes.patch.set_facecolor('yellow')
        # event.canvas.draw()

    def leave_axes(self, event, fig, axs):
        ax = event.inaxes
        axs.clear_guide(fig, ax)
        gid = ax.get_gid() or ax
        gid = gid if isinstance(gid, tuple) else (gid, )
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
        if sub[0] == 'x':
            nsel = {co[1]: x}
        elif sub[0] == 'xy':
            nsel = {co[0]: y, co[1]: x, }
        elif sub[0] == 'y':
            nsel = {co[0]: y}
        else:
            return
        fig.point_selection(nsel, anchor=anchor)
        self.interactive(fig, step=False, msg='point', prev=prev)

    def spine_selection(self, fig, axs, lab, sub, aux, event, anchor=None):
        prev = fig.restore_view(axs)
        co = list(prev.keys())
        which = aux[1]
        if which in ['left', 'right', ]:
            pos = (None, event.y)
        elif which in ['top', 'bottom', ]:
            pos = (event.x, None)
        else:
            return
        dpos = axs.position_transform(*pos, ax=aux[0])
        nsel = {}
        for c, p in zip(co, dpos[::-1]):
            if p is not None:
                nsel[c] = float(p)
        locallog.debug(f"{nsel=}")
        # locallog.debug(f"{sub=} {aux=} ({pos}) ({dpos})")
        fig.point_selection(nsel, anchor=anchor)
        self.interactive(fig, step=False, msg='section', prev=prev)
        # x, y = axs.get_position(x, y, lab)
        # prev = fig.restore_view(axs)
        # co = list(prev.keys())
        # if sub[0] == 'x':
        #     nsel = {co[1]: x}
        # elif sub[0] == 'xy':
        #     nsel = {co[0]: y, co[1]: x, }
        # elif sub[0] == 'y':
        #     nsel = {co[0]: y}
        # else:
        #     return
        # fig.point_selection(nsel, anchor=anchor)
        # self.interactive(fig, step=False, msg='point', prev=prev)

    def monitor_spine(self, fig, axs, lab, event, aux=None):
        debug = ft.partial(self.debug, func='monitor_spine')
        body = lab[1]
        dpos = axs.position_transform(event.x, event.y, ax=body)
        axs.draw_guide(fig, event.inaxes, event.xdata, event.ydata, pos=dpos)
        axs.draw_guide(fig, body, *dpos)
        fmt = r'{:.2f}'
        if lab[2] in ['left', 'right']:
            text = 'y=' + (fmt.format(dpos[1]))
        else:
            text = 'x=' + (fmt.format(dpos[0]))
        axs.monitor(fig, text)

    def monitor_point(self, fig, axs, lab, event, aux=None, fmt=None):
        """Point selection by mouse click."""
        fmt = fmt or r'({x:.2f}, {y:.2f})'

        x, y = (event.xdata, event.ydata)
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
        axs.on_draw(fig)
        # for ax in fig.get_children():
        #     gid = ax.get_gid()
        #     if gid == 'body':
        #         for ch in ax.get_children():
        #             try:
        #                 print(ch, ch.get_cmap())
        #             except AttributeError:
        #                 pass

    def mouse_pick(self, event, fig, axs):
        """Mouse pick actions on artists."""
        debug = ft.partial(self.debug, func='mouse_pick')
        this = event.artist
        mouse = event.mouseevent
        # x, y = mouse.x, mouse.y
        # xdata = thisline.get_xdata()
        # ydata = thisline.get_ydata()
        # ind = event.ind
        # points = tuple(zip(xdata[ind], ydata[ind]))
        gid = this.get_gid()
        aux = gid.split(':')
        lab = aux.pop(0)

        # bbox = this.get_tightbbox()
        # extent = this.get_window_extent()
        # cbox = this.get_clip_box()
        # if cbox:
        #     cont = cbox.contains(x, y)
        # else:
        #     cont = None
        # debug(f'onpick: {this}')
        # debug(f'event: {x} {y}')
        # debug(f'prop: {gid=} {bbox=} {cont=} {extent=}')

        nm = self.normalize_mouse(mouse)
        mseq = self.mmap.get(nm) or {}
        cmds = mseq.get(lab) or ()
        debug(f"{mouse=} {mseq=} {lab=} {cmds=}")
        cmds = cmds or (None, )

        if False:
            self.mouse_handler(event, fig, axs, lab, cmds[0], cmds[1:], aux)
        # if cmds[0] == 'next_cyclic':
        #     if lab == 'axis':
        #         self.switch_draw(fig.number, aux[0], +1)
        # elif cmds[0] == 'prev_cyclic':
        #     if lab == 'axis':
        #         self.switch_draw(fig.number, aux[0], -1)
        # elif cmds[0] == 'transpose':
        #     if lab == 'axis':
        #         self.map_figures(self.transpose, fig.number)

        # a = mart.ArtistInspector(this)
        # ppr.pprint(a.properties())

    def mouse_press(self, event, fig, axs):
        """Mouse button actions on axes."""
        debug = ft.partial(self.debug, func='mouse_press')
        lab = axs.retrieve_event(event)
        debug(f"{lab=}")
        if isinstance(lab, tuple):
            aux = lab[1:]
            lab = lab[0]
        elif lab is not None:
            aux = lab.split(':')
            lab = aux.pop(0)
        else:
            aux = None

        if fig.canvas.widgetlock.locked():
            self._cache_events.append(event)

        mouse = self.normalize_mouse(event)
        mseq = self.mmap.get(mouse) or {}
        cmds = mseq.get(lab) or ()
        debug(f"({event.x},{event.y})")
        debug(f"{mouse=} <{lab=} {aux=}> {cmds=}")
        # if lab == 'body':
        #     self.point_selection(fig, axs, lab, mouse, event)
        cmds = cmds or (None, )
        self.mouse_handler(event, fig, axs, lab, cmds[0], cmds[1:], aux)

        # if cmds[0] == 'select':
        #     if lab == 'body':
        #         self.point_selection(fig, axs, lab, cmds[1:], event)
        # elif cmds[0] == 'anchor':
        #     if lab == 'body':
        #         self.point_selection(fig, axs, lab, cmds[1:], event,
        #                              anchor=True)

    def parse_config(self, config, params):
        config = config or {}
        verbose = config.get('verbose', 0)
        self.kmap = self.parse_keymap({}, config.get('keymap', {}))
        self.mmap = self.parse_mousemap({}, config.get('mouse', {}))
        locallog.debug(self.mmap)
        # ppr.pprint(config.get('mouse'))
        # ppr.pprint(self.kmap)
        # ppr.pprint(self.mmap)
        self.opts = config.get('option', {})
        self.opts['resize_step'] = self.opts.get('resize_step') or 0.25

        if params:
            for k in params.find_all(r'^keymap\.*'):
                for p in params[k]:
                    if p in self.kmap:
                        locallog.warning(f"Remove default keymap for <{p}> from {k}.")
                        params[k].remove(p)

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
            elif isinstance(c, list):
                for k in c:
                    mmap.setdefault(k, {})[v[0]] = v[1:]
            elif c != '':
                mmap.setdefault(c, {})[v[0]] = v[1:]
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
                xs = data.coords[co].sel({co: xs}, method='nearest')
                nsel[co] = xs.item()
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

    coords = coords or default
    if not isinstance(coords, (tuple, list)):
        coords = tuple(coords)

    locallog.debug(f"{coords} << {src.dims}")
    locallog.debug(f"   {anchors=}")
    locallog.debug(f"   {data.shape}")
    locallog.debug(f"   {data.dims}")

    anchors = anchors or {}
    targets = []
    for c in coords:
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
    locallog.debug(f"first try: {targets=}")
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
        raise ValueError(f"Invalid target coordinates {coords} >> {ntargets}")
    locallog.debug(f"result: {ntargets=}")
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
    co = dims.index(co)
    for kw in args:
        for j in [co, co - len(dims)]:
            if j in kw:
                v = kw[j]
                return v
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
                return kw[cc]
    raise KeyError(f"No match to view {arr.dims}")


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
            raise UserWarning
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

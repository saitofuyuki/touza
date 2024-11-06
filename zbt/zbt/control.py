#!/usr/bin/env python3
# Time-stamp: <2024/11/11 09:43:19 fuyuki control.py>

__doc__ = \
    """
zbt.control
~~~~~~~~~~~
Figure traverse controller.

:Source:     zbt/control.py
:Maintainer:  SAITO Fuyuki <saitofuyuki@jamstec.go.jp>
:Created:    Oct 9 2024
"""

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

import cartopy.util as cutil

import zbt.plot as zplt
import zbt.xrnio as zxr
import zbt.util as zu


class LinkedArray:
    """Helper class of recursive iteration."""

    def __init__(self,
                 array, key=None, mask=None,
                 name=None, child=False, opts=None):
        self.name = name
        self.child = child
        self.opts = opts

        self.array = None
        self.shape = None
        self._keys = None
        self._l2i = None
        self.mask = False

        # step:
        #   +1: forward
        #   0:  pin
        #   -1: backword
        self.step = +1

        self.switch(cls=False, array=array, key=key, mask=mask)

        # current
        #   None:  exhausted
        self._current = None
        self.recurse = True

        # init
        #   True:        natural
        #   False/None:  keep current unless None
        #   tuple:       initial lkey
        self.init = True

        # child
        #    True:  wait
        #    False/None: final node
        if child in [True, False, None]:
            pass
        elif not isinstance(child, LinkedArray):
            raise ValueError(f"Invalid child type {type(child)}.")

    def append(self, item, key=None):
        """Append item to array."""
        if isinstance(self.array, list):
            self.array.append(item)
            self.switch(array=self.array)
        else:
            raise TypeError(f"invalid item to append on {type(self.array)}.")

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

    def switch(self, cls=None,
               array=None, key=None, mask=None, step=None, lev=None):
        """Recursive switcher of attributes."""
        # print(f"<{self.name}:switch> {cls=} {step=} {key=}", end='')
        # print(f"<{self.name}:switch> {mask=}")
        lev = lev or 0
        cls, do_this, do_next = self._class_range(cls, lev)
        # print(f" >> {do_this} {do_next}")
        if do_this:
            if step is not None:
                self.step = step
            if mask is not None:
                self.mask = mask
            # array
            #   None:  wait
            #   dict, list:  set
            if array is not None:
                self.array = array
            # key
            #   number: 1d length
            #   tuple:  shape
            #   list:   1d phyiscal key
            #   None:   use key of input array
            if key is not None:
                if self.array is None:
                    pass
                elif not hasattr(self.array, '__getitem__'):
                    raise ValueError(f"no mapping in {self.array}")

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
            elif array is not None:
                if self.array is None:
                    # array is reserved
                    self.shape = None
                    self._keys = None
                    self._l2i = None
                elif isinstance(self.array, dict):
                    self.shape = (len(self.array), )
                    self._keys = list(self.array.keys())
                    self._l2i = self._l2i_single
                elif isinstance(self.array, cabc.Iterable):
                    self.shape = (len(self.array), )
                    self._keys = None
                    self._l2i = self._l2i_single
                else:
                    raise ValueError(f"invalid array = {self.array}")
            # print(f'<{self.name}:switch> result:'
            #       f'{self.step=} {self.shape=} {self.mask=}')
        if do_next:
            if self.child:
                self.child.switch(cls=cls,
                                  array=array, key=key, mask=mask, step=step,
                                  lev=lev+1)
            elif cls[0]:
                raise TypeError(self.msg(f"No class as {cls[0]}"))
        return self

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

    def __str__(self):
        head = self.name or type(self)
        base = f"<{head}>{self.array}{self.shape}[{self._current}]"
        if self.child:
            base = base + f'+{self.child}'
        return base

    def items(self, recurse=True):
        """Iterator of key-value tuples."""
        for lkey in self(recurse=recurse):
            yield lkey, self[lkey]

    def __getitem__(self, key):
        if key:
            head = key[0]
            rest = key[1:]
        else:
            head = (self._current, )
            rest = key
        head = self._value(head)
        if self.child and self.recurse:
            return (head, ) + self.child[rest]
        return (head, )

    def __call__(self, recurse=True):
        self.recurse = recurse
        return self

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
        # print(f"<{self.name}:__iter__> enter {self.mask=}")
        ret = self.reset()
        # print(f"<{self.name}:__iter__> {self.init=} {ret=} {self.shape=}")
        # print(f"<{self.name}:__iter__> reset {self.mask=}")

        while ret is not None:
            # ret = self._mask(ret, self.mask)

            self.update(ret)
            self._current = ret

            ret = self._mask(ret, self.mask)
            # print(ret, self.mask)
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
            # print(self._current, self.mask)

    def _mask(self, val, mask):
        ret = ()
        mask = mask or (False, ) * len(val)
        for v, m in zip(val, mask):
            # if m is False:
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
        return self.array[self.l2p(key)]

    def value(self, key=None):
        return self._value(key)

    def reset(self):
        """Reset self and children."""
        # print(f'reset<{self.name=}>: {self.init} {self._current}')
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
                else:
                    init = (0, ) * len(self.shape)
        # print(f"{init=}")
        return init

    def cue(self, init):
        """Cueing."""
        # print(f"cue: {init}")
        if init is True:
            self._current = None
            self.init = True
        else:
            self.init = False
            self._current = init

    def update(self, key):
        """Dummy hook to update current status."""

    def msg(self, txt):
        """Messages"""
        name = self.name or type(self)
        return f"<{name}>: {txt}"

    def copy(self, init=None, recurse=True):
        """Clone instance."""
        cp = copy.copy(self)
        if init is False:
            # print(f"<{self.name}:copy> {self._current}")
            cp.cue(self._current)
        if recurse and self.child:
            cp.child = self.child.copy(init=init, recurse=recurse)
        return cp


class ArrayIter(LinkedArray):
    """Array level iterator."""

    __slot__ = ["_transpose", "_nsel", "_xsel", "_dims", ]

    def __init__(self, **kw):
        super().__init__(array=None, name='ARRAY', **kw)
        self._transpose = False
        self._nsel = None      # normalized selection (defined only)
        self._xsel = None      # source selection (whole coordinates)
        self._dims = None      # data dimension (possibly squeezed)

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
        data = super()._value()
        # print(f'data: {data.dims}')
        # # print(f'indexes: {list(data.indexes)}')
        # print(f'coords: {list(data.coords.keys())}')
        # print(f'pkey: {self.l2p()}')
        # print(f'dims: {self._dims}')
        # print(f'nsel: {self._nsel}')
        # print(f'xsel: {self._xsel}')
        stt = []
        pkey = self.l2p()
        mask = self.mask
        # print(pkey, mask)
        for xk, xs in self._xsel.items():
            anc = 0
            if xk in self._dims:
                jk = self._dims.index(xk)
                pk = pkey[jk]
                xs = zu.set_default(xs, slice(None, None, None))
                if mask[jk] is True:
                    anc = 1
                if isinstance(pk, slice):
                    # stt.append(xs)
                    pass
                elif self.is_isel(xs):
                    xs = (xs.start or 0) + pk
                    # stt.append((xs.start or 0) + pk)
                else:
                    xs = data.coords[xk].item()
                    # stt.append(data.coords[xk].item())
            else:               # squeezed
                anc = 2
                if isinstance(xs, int):
                    pass
                    # stt.append(xs)
                else:
                    xs = self._nsel.get(xk, xs)
                    # stt.append(self._nsel.get(xk, xs))
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
        return txt

    def update(self, key):
        # v = self.value(key)
        # print(f"update:array: {key} {v.dims} {v.coords}")
        pass

    def offset(self, nsel, xsel, dims):
        """Store selections."""
        self._nsel = nsel
        self._xsel = xsel
        self._dims = dims

    def transpose(self, switch=None):
        if isinstance(switch, tuple):
            self._transpose = switch
        elif switch is not None:
            self._transpose = bool(switch)
        else:
            if isinstance(self._transpose, tuple):
                self._transpose = self._transpose[1:] + self._transpose[:1]
            else:
                self._transpose = not self._transpose

    def _value(self, key=None):
        data = super()._value(key)
        if self._transpose is True:
            data = data.T
        elif isinstance(self._transpose, tuple):
            if len(self._transpose) == 2:
                if data.dims == self._transpose:
                    pass
                else:
                    data = data.T
            else:
                data = data.transpose(*self._transpose)
        for cn in data.coords:
            if cn not in data.dims:
                continue
            ck = data.dims.index(cn)
            co = data[cn]
            cc = co.attrs.get('cyclic_coordinate')
            if not cc:
                continue
            w, org, dup = cc
            if len(co) == w - 1 and co[0] == org and co[-1] != dup:
                cd, cx = cutil.add_cyclic(data, co, axis=ck,
                                          cyclic=dup-org)
                nco = dict(data.coords.items())
                # print(co.dims)
                # print(cx.shape)
                nco[cn] = xr.DataArray(cx, dims=co.dims, attrs=co.attrs)
                data = xr.DataArray(cd, coords=nco,
                                    dims=data.dims, attrs=data.attrs)
        return data

    def __getitem__(self, key):
        return super().__getitem__(key)

    def l2p(self, key=None):
        """Logical to physical element-key conversion."""
        r = super().l2p(key)
        # print(f"l2p:a: {r}")
        return r


class VariableIter(LinkedArray):
    """Variable level iterator."""

    def __init__(self, coors=None, dim=None, anchors=None, debug=None, **kw):
        super().__init__(array=None, name='VAR', **kw)
        self.anchors = anchors or ()
        self.debug = debug or 0
        self.coors = coors or (-2, -1)
        self.dim = dim
        self.nsel = None
        self.xsel = None

    def update(self, key):
        """Update current and child status."""
        k = self.l2p(key)
        ov = self.array.get(k)
        v = ov
        if v is None:
            self.switch(cls=ArrayIter, array=None, key=0, )
            return

        self.xsel, self.nsel = extract_sels(self.dim, v)

        if not self.nsel:
            pass
        elif any(isinstance(j, slice) for j in self.nsel.values()):
            for c in self.nsel.keys():
                # workaround for NotImplementedError in xarray
                xs = self.xsel[c]
                if not isinstance(xs, (slice, int)):
                    xs = v.coords[c].sel({c: xs}, method='nearest')
                    self.nsel[c] = xs.item()
            v = v.sel(self.nsel)
        else:
            v = v.sel(self.nsel, method='nearest')

        shape = v.shape
        cue = self.adjust_cue(shape)

        opts = self.opts or {}

        asel = self.set_anchors(self.anchors, v)
        csel = self.set_coors(self.coors, v)
        mask = self.c2mask(csel, asel, shape, v.dims)
        self.child.transpose(csel)
        self.child.offset(self.nsel, self.xsel, v.dims)
        self.switch(cls=ArrayIter,
                    array=v, key=shape, mask=mask)
        if cue:
            self.child.cue(cue)
        if self.debug > 0:
            print(f"Variable:{shape=}")
            print(f"Variable:{cue=}")
            print(f"Variable:{csel=}")
            print(f"Variable:{asel=}")
            print(f"Variable:{mask=}")

    def adjust_cue(self, shape):
        """Update child cue."""
        cue = False
        cidx = self.child.l2i()
        if cidx is None:
            cue = True
        elif len(cidx) != len(shape):
            cue = True
        else:
            init = ()
            for c, w in zip(cidx, shape):
                if c >= w:
                    cue = True
                    init = init + (w - 1, )
                else:
                    init = init + (c, )
            if cue:
                cue = init
        return cue

    def transpose(self, switch=None, key=None):
        """Update transpose of child array."""
        k = self.l2p(key)
        v = self.array.get(k)
        # print(k, v)

        # mask = self.inquire(prop='mask', cls=ArrayIter, single=True)
        # shape = self.inquire(prop='shape', cls=ArrayIter, single=True)
        arr = self.inquire(prop='self', cls=ArrayIter, single=True)
        arr = arr.array
        # print(mask, shape, k, v.dims, arr.array.dims)

        if isinstance(switch, tuple):
            self.coors = switch
        elif switch is not None:
            # order = sorted(list(self.set_coors(self.coors, v)))
            order = sorted(list(self.set_coors(self.coors, arr)))
            if not switch:
                order = reversed(order)
            self.coors = tuple(order)
        else:
            # csel = self.set_coors(self.coors, v)
            csel = self.set_coors(self.coors, arr)
            # self.coors = self.coors[1:] + self.coors[:1]
            self.coors = csel[1:] + csel[:1]
            # print(f" >> {switch=} {self.coors=} {csel=} {v.dims=} {v.coords}")

        # switch = self.set_coors(self.coors, v)
        switch = self.set_coors(self.coors, arr)
        # print(f" >> {switch=} {self.coors=} {v.dims=} {v.coords}")
        return self.child.transpose(switch)

    def set_anchors(self, anchors, data):
        """Set lock coordinates."""
        clist = []
        for c in anchors:
            try:
                c = parse_coord(data, c)
                print("set_anchors:", c, data.dims, data.coords[c])
                clist.append(c)
            except UserWarning:
                print(f"no coordinate to anchor {c}.")
        return clist

    def set_coors(self, coors, data, prop=None):
        """Set coordinate tuple."""
        clist = []
        for c in coors:
            try:
                c = parse_coord(data, c)
                clist.append(data.dims.index(c))
            except UserWarning:
                if c == '':
                    clist.append(c)
                else:
                    print(f"no coordinate to plot {c}.  Available={data.dims}")
        w = len(data.dims)
        # print(f"set_coors: {coors=} {w=} {data.dims=}")
        # shape = self.inquire(prop='shape', cls=ArrayIter, single=True)
        rem = w
        nlist = []
        for c in reversed(clist):
            if c == '':
                c = rem - 1
                # print(f"{c=} {shape[c]=}")
                while c in clist:
                    c = c - 1
                rem = c
            nlist.insert(0, c)
        if prop is None:
            return tuple(data.dims[c] for c in nlist if c >= 0)
        if prop < 0:
            return tuple(c - w for c in nlist if c >= 0)
        return tuple(c for c in nlist if c >= 0)

    def c2mask(self, coors, anchors, shape, dims):
        w = len(shape)
        # print(shape, anchors)
        # print(coors, dims, anchors)
        mask = ()
        for d in dims:
            d = d in anchors
            mask = mask + (d, )
        # mask = (False, ) * w
        colon = slice(None, None, None)
        for c in coors:
            if c in dims:
                c = dims.index(c)
            mask = mask[:c] + (colon, ) + mask[c+1:]
        mask = mask[:w]
        return mask

    def permute_anchor(self, switch):
        """Anchor permutation."""
        mask = self.inquire(prop='mask', cls=ArrayIter, single=True)
        cidx = self.child.l2i()
        print(f"{cidx=}")

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
        self.switch(cls=ArrayIter, mask=tuple(npat))
        self.anchors = tuple(- w + j
                             for j, m in enumerate(npat) if m is True)
        if self.debug > 0:
            print(f"Variable:permute_anchor: {self.anchors=}")

    def permute_axis(self, switch, skip_single=True):
        """Axis permutation."""
        mask = self.inquire(prop='mask', cls=ArrayIter, single=True)
        shape = self.inquire(prop='shape', cls=ArrayIter, single=True)

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
            #     print(w, m)
            # fskip = [2 - min(2, max(1, w)) for w in reversed(shape)]
        else:
            fskip = [0] * len(shape)
        mpat = len(pat)
        mskip = sum(fskip[pat[j]] for j in range(mpat))
        ntgt = sum(fskip) if reverse else 0
        mshp = len(shape)

        pat.append(mshp)
        fskip.append(0)
        # print(pat)
        # print(shape)
        # print(fskip)

        for _ in range(math.comb(mshp, mpat)):
            pos = 0
            # print('permute:', pos, pat)
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
        self.coors = tuple(- mshp + j for j, m in enumerate(nmask)
                           if isinstance(m, slice))

        arr = self.inquire(prop='self', cls=ArrayIter, single=True)
        v = arr.array
        # print(v.dims)
        # v = self.value()
        # print(v.dims)
        csel = self.set_coors(self.coors, v)
        self.child.transpose(csel)

        ### hardcoded test
        # if mask[-1] is False:
        #     mask[-1] = True
        mask = [m if m is True else n
                for m, n in zip(mask, nmask)]
        # print(mask)
        self.switch(cls=ArrayIter, mask=tuple(mask))
        # print(f"permute: {csel=} {mask=} {v.shape}")

    def point_selection(self, sel, skip_single=True):
        """Axis permutation."""
        mask = self.inquire(prop='mask', cls=ArrayIter, single=True)
        shape = self.inquire(prop='shape', cls=ArrayIter, single=True)

        print(f"{sel=}")
        print(f"{mask=}")
        print(f"{shape=}")

        return

        pat = [j for j, m in enumerate(reversed(mask))
               if isinstance(m, slice) is not reverse]
        if skip_single:
            fskip = []
            for w, m in zip(shape, mask):
                if m is True or w < 2:
                    fskip = [1] + fskip
                else:
                    fskip = [0] + fskip
            #     print(w, m)
            # fskip = [2 - min(2, max(1, w)) for w in reversed(shape)]
        else:
            fskip = [0] * len(shape)
        mpat = len(pat)
        mskip = sum(fskip[pat[j]] for j in range(mpat))
        ntgt = sum(fskip) if reverse else 0
        mshp = len(shape)

        pat.append(mshp)
        fskip.append(0)
        # print(pat)
        # print(shape)
        # print(fskip)

        for _ in range(math.comb(mshp, mpat)):
            pos = 0
            # print('permute:', pos, pat)
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
        self.coors = tuple(- mshp + j for j, m in enumerate(nmask)
                           if isinstance(m, slice))

        arr = self.inquire(prop='self', cls=ArrayIter, single=True)
        v = arr.array
        # print(v.dims)
        # v = self.value()
        # print(v.dims)
        csel = self.set_coors(self.coors, v)
        self.child.transpose(csel)

        ### hardcoded test
        # if mask[-1] is False:
        #     mask[-1] = True
        mask = [m if m is True else n
                for m, n in zip(mask, nmask)]
        # print(mask)
        self.switch(cls=ArrayIter, mask=tuple(mask))
        # print(f"permute: {csel=} {mask=} {v.shape}")

    def status(self, fmt=True, recurse=False):
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
        return txt

    def l2p(self, key=None):
        """Logical to physical element-key conversion."""
        r = super().l2p(key)
        # print(f"l2p:v: {r}")
        return r


class FileIter(LinkedArray):
    """File (or dataset) level iterator."""

    def __init__(self, files, variables=None, **kw):
        super().__init__(array={}, key=files, name='FILE', **kw)
        variables = False if variables is None else variables
        self.variables = variables

    def update(self, key):
        f = self.l2p(key)
        ds = self.array.get(f)
        if ds is None:
            opts = self.opts or {}
            ds = zxr.open_dataset(f, **opts)
            self.array[f] = ds

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
        self.switch(cls=VariableIter, array=ds, key=vlist)
        if not vlist:
            print('No (matched) variables.')

    def status(self, fmt=True, recurse=False):
        idx = self.l2i()
        path = plib.Path(self.l2p())
        txt = f'{idx}:{path.name}'
        if recurse and self.child:
            ch = self.child.status(fmt=fmt, recurse=recurse)
            if ch:
                txt = txt + ' ' + ch
        return txt

    def plot_coordinates(self, data):
        coors = None
        var = self.inquire(prop='self', cls=VariableIter, single=True)
        v = var.value(None)
        coors = var.set_coors(var.coors, v, prop=-1)
        return coors


class CmapIter(LinkedArray):
    def __init__(self, **kw):
        array = [None] + [k for k in mplib.colormaps.keys()
                          if not k.endswith(r'_r')]
        super().__init__(array=array[:10], name='CMAP', **kw)

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

    def fwd(self):
        try:
            next(self._loop)
        except StopIteration:
            self._loop = iter(self.items())
            _ = next(self._loop)


class FigureInteractive(zplt.FigureCore):
    """Matplotlib figure class with interactive methods."""

    def __init__(self, *args, **kwds):
        super().__init__(*args, **kwds)
        self.cid = {}
        self._loop = None
        self.trees = None
        self._lock = False
        self.fc = []
        self.sel = None
        self.base = None

        self.view = {}

    def connect(self, **handlers):
        for k, h in handlers.items():
            self.cid[k] = self.canvas.mpl_connect(k, h)

    def set_hook(self, handler):
        """Add on_draw hook"""
        self.connect(draw_event=handler)

    def disconnect(self, *keys):
        keys = keys or self.cid.keys()
        for k in keys:
            if k in self.cid:
                self.canvas.mpl_disconnect(self.cid[k])
                self.cid[k] = None

    def bind(self, trees, base, view=None):
        self.trees = trees
        self.base = base
        if view:
            self.view = view.copy()
        else:
            self.view = {}

    def permute_axis(self, switch):
        base = self.trees.inquire(prop='self', cls=VariableIter, single=True)
        if base:
            base.permute_axis(switch)

    def point_selection(self, sel):
        base = self.trees.inquire(prop='self', cls=VariableIter, single=True)
        if base:
            base.point_selection(sel)

    def permute_anchor(self, switch):
        base = self.trees.inquire(prop='self', cls=VariableIter, single=True)
        if base:
            base.permute_anchor(switch)

    def toggle_lock(self, force=None):
        p = self._lock
        if force is not None:
            self._lock = bool(force)
        else:
            self._lock = not self._lock
        if self._lock:
            if not p:
                self.fc.append(self.patch.get_facecolor())
                # self.patch.set_facecolor((0.5, 0.5, 0.8))
                self.patch.set_facecolor(('blue', 0.3))
        else:
            if p:
                c = self.fc.pop(-1)
                self.patch.set_facecolor(c)
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
        self.sel = self.trees.status(recurse=True)

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
                self.sel = self.trees.status(recurse=True)
                return cur
            except StopIteration:
                self._loop = None
                cur = None
                self.sel = None
                raise
        elif self._loop is None:
            cur = None, None
        else:
            k = self.trees.current
            cur = k, self.trees[k]
            self.sel = self.trees.status(recurse=True)
        return cur

    def restore_view(self, axs):
        """Restore current (previous) figure extent."""
        prev = self.loop(step=False) or (None, None)
        # prev = prev or (None, None)
        _, pdata = prev
        if axs:
            ext = axs.get_extent()
        else:
            ext = None
        if pdata and ext:
            parr = pdata[-1]
            y, x = ext
            ## dict order must be guaranteed (python >= 3.9)
            prev = {parr.dims[0]: slice(x[0], x[1], None),
                    parr.dims[-1]: slice(y[0], y[1], None)}
            # print(f"{parr.dims=} {ext}")
            # print(f"{parr.coords=}")
        else:
            prev = {}
        # print(prev)
        return prev

    def parse_view(self, coors, draw=None, prev=None):
        """Set figure coordinate."""
        var = self.trees.inquire(prop='self', cls=VariableIter, single=True)
        data = var.value(None)

        view = {}
        draw = draw or {}
        prev = prev or {}
        # print(f"{draw=}")
        # print(f"{prev=}")
        self.view.update(prev)
        # draw.update(prev)
        # print(f"{coors=} {prev=} {data.dims=} {self.view=}")
        for c in coors:
            d = data.dims[c]
            s = None
            if d in self.view:
                s = self.view.get(d)
            elif d in draw:
                s = draw.get(d)
            else:
                try:
                    s = zxr.match_coordinate(data, d, draw)
                except KeyError as exc:
                    if isinstance(c, int):
                        for j in [c, len(data.dims) + c, ]:
                            if j in draw:
                                s = draw.get(j)
                                break

            if s:
                mm = {}
                if isinstance(s.start, int):
                    mm['dmin'] = data.coords[d][s.start].item()
                elif s.start is not None:
                    mm['dmin'] = s.start
                if isinstance(s.stop, int):
                    mm['dmax'] = data.coords[d][s.stop].item()
                elif s.start is not None:
                    mm['dmax'] = s.stop
                if mm:
                    view[d] = mm
        return view


class FigureControl():
    """Figure iteration controller."""

    def __init__(self, pic, plot, root,
                 interactive=True,
                 layout=None, cmap=None,
                 styles=None, draw=None,
                 config=None, params=None):
        self.parse_config(config, params)

        self.draw = draw or {}
        self.plot = plot
        self.pic = pic
        self.layout = layout

        if inspect.isclass(self.pic):
            self.pic = self.pic(FigureClass=FigureInteractive,
                                LayoutClass=self.layout)

        self.figs = {}
        if not isinstance(root, (list, tuple, )):
            root = [root]
        self.root = root
        self.output = None
        self.interactive = interactive
        self.styles = styles or {}
        self.cmap = cmap

        if self.interactive:
            self.load()

    def __call__(self, output=None, **kwds):
        self.output = output or None
        if self.interactive:
            sub = self._interactive
            # sub = self._animate
        else:
            # sub = self._animate
            sub = self._batch

        for base in self.root:
            fig, axs = self._new_control(**kwds)
            jfig = fig.number
            self.figs[jfig] = fig, axs
            trees = base.copy()
            fig.bind(trees, base=base)
            sub(jfig)
        if self.interactive:
            plt.show()

    def _new_control(self, *args, **kwds):
        fig, axs = self.pic(*args, **kwds)
        mfunc = ft.partial(self.mouse_press, fig=fig, axs=axs)
        fig.connect(button_press_event=mfunc,
                    scroll_event=mfunc, )
        # print(fx)
        # fig = fx[0]
        # fig.canvas.mpl_connect('pick_event', self.onpick)
        # fig.canvas.mpl_connect('button_press_event', self.mouse_press)
        # fig.canvas.mpl_connect('figure_enter_event', self.enter_figure)
        # fig.canvas.mpl_connect('figure_leave_event', self.leave_figure)
        # fig.canvas.mpl_connect('axes_enter_event', self.enter_axes)
        # fig.canvas.mpl_connect('axes_leave_event', self.leave_axes)
        return fig, axs

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
            print(f"Warning: {be} may not work as expected at window resizing.")

    def mouse_press(self, event, fig, axs):
        lab = axs.retrieve_event(event)
        if lab == 'body':
            self.point_selection(fig, axs, lab, event)

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

    def onpick(self, event):
        this = event.artist
        mouse = event.mouseevent
        x, y = mouse.x, mouse.y
        # xdata = thisline.get_xdata()
        # ydata = thisline.get_ydata()
        # ind = event.ind
        # points = tuple(zip(xdata[ind], ydata[ind]))
        gid = this.get_gid()
        bbox = this.get_tightbbox()
        extent = this.get_window_extent()
        cbox = this.get_clip_box()
        cont = cbox.contains(x, y)
        print(f'onpick: {this}')
        print(f'event: {x} {y}')
        print(f'prop: {gid=} {bbox=} {cont=} {extent=}')
        a = mart.ArtistInspector(this)
        ppr.pprint(a.properties())

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

    # def enter_axes(self, event):
    #     print('enter_axes', event.inaxes)
    #     event.inaxes.patch.set_facecolor('yellow')
    #     event.canvas.draw()

    # def leave_axes(self, event):
    #     print('leave_axes', event.inaxes)
    #     event.inaxes.patch.set_facecolor('white')
    #     event.canvas.draw()

    # def enter_figure(self, event):
    #     print('enter_figure', event.canvas.figure)
    #     event.canvas.figure.patch.set_facecolor(('red', 0.2))
    #     event.canvas.draw()

    # def leave_figure(self, event):
    #     print('leave_figure', event.canvas.figure)
    #     event.canvas.figure.patch.set_facecolor(('grey', 0.2))
    #     event.canvas.draw()

    def _prompt(self, event):
        """Event handler"""
        # print(f'{event}> ', end=' ', flush=True)
        print('\r> ', end=' ', flush=True)
        if self.interactive and event:
            event.canvas.flush_events()
            cf = event.canvas.figure
            jfig = cf.number
            fig, _ = self.figs[jfig]
            # 'key_press_event', handler
            fig.connect(key_press_event=self.event_handler)


    def _interactive(self, jfig, step=True, msg=None, prev=None):
        fig, axs = self.figs[jfig]
        fig.disconnect('key_press_event')
        # prev = prev or fig.loop(step=False)
        prev = prev or fig.restore_view(axs)
        # prev = fig.loop(step=False)
        # print(f"{prev[0]=}")
        try:
            trees, stat = fig.loop(step)
        except StopIteration:
            print(f"\r({jfig}) no more data.")
            try:
                trees, stat = fig.loop(step)
            except StopIteration:
                raise StopIteration(f"\r({jfig}) no effective data.") from None

        # print(cur[0])
        # print(trees)
        data = stat[-1]
        # print(data.indexes)
        # print(data.dims)
        # print(data.coords)
        try:
            print(f'\r({jfig}) drawing...', end='', flush=True)
            artists = self._update(data, fig, axs, prev)
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

    def _batch(self, jfig):
        fig, axs = self.figs[jfig]
        n = fig.len()
        while True:
            try:
                trees, stat = fig.loop()
                data = stat[-1]
                artists = self._update(data, fig, axs)
                self._savefig(jfig, ref=n)
            except StopIteration:
                break

    # def _animate(self, jfig):
    #     fig, axs = self.figs[jfig]
    #     frames = []
    #     while True:
    #         try:
    #             trees, stat = fig.loop()
    #             data = stat[-1]
    #             p = self._update(data, fig, axs)
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

    def _update(self, data, fig, axs, view=None):
        """Draw core."""
        pco = fig.trees.plot_coordinates(data)
        view = fig.parse_view(pco, self.draw, view)
        body = self.styles.get(pco) or {}
        layout = {k: body.get(k) for k in ['projection', ]}
        # axs.reset(fig, body=layout, colorbar=dict(fresh=True))
        axs.reset(fig, body=layout)
        axs.cla()
        fig.set_hook(self._prompt)
        return self.plot(fig=fig, axs=axs, data=data, title=fig.sel,
                         view=view, body=body)

    def diag(self, jfig):
        fig, _ = self.figs[jfig]
        child = fig.trees
        while True:
            print(f"<{child.name}> {child.step}")
            child = child.child
            if not child:
                break

    def _is_locked(self, jfig):
        fig, _ = self.figs[jfig]
        return fig.is_locked()

    def switch(self, jfig, draw=True, *args, **kw):
        if self._is_locked(jfig):
            targets = filter(self._is_locked, self.figs.keys())
        else:
            targets = [jfig]
        for jf in targets:
            fig, axs = self.figs[jf]
            # prev = fig.loop(step=False)
            prev = fig.restore_view(axs)
            child = fig.trees
            child.switch(*args, **kw)
            if draw:
                self._interactive(jf, prev=prev)

    def map_figures(self, func, jfig, *args, **kw):
        if self._is_locked(jfig):
            targets = filter(self._is_locked, self.figs.keys())
        else:
            targets = [jfig]
        for jf in targets:
            func(jf, *args, **kw)

    def _savefig(self, jfig, ref=None):
        fig, axs = self.figs[jfig]
        output = self.output
        if isinstance(output, cabc.Callable):
            output = self.output(jfig, fig, axs, ref=ref)
        if output:
            if fig.is_locked():
                face = fig.fc[-1]
            else:
                face = 'auto'
            if hasattr(output, 'savefig'):
                output.savefig(fig, facecolor=face)
            else:
                fig.savefig(output, facecolor=face)
            fig.info(pfx=f'\r({jfig}) ', msg=f"Saved: {output}")
        else:
            print(f"No output defined.")

    def point_selection(self, fig, axs, lab, event):
        """Point selection by mouse click."""
        button = event.button
        x, y = (event.xdata, event.ydata)
        prev = fig.restore_view(axs)
        ## dict order must be guaranteed (python >= 3.9)
        co = list(prev.keys())
        if button == 1:
            nsel = {co[-1]: x}
        elif button == 2:
            nsel = {co[0]: y, co[-1]: x, }
        elif button == 3:
            nsel = {co[0]: y}
        else:
            return
        fig.point_selection(nsel)
        # prev = fig.restore_view(axs)
        # # prev = fig.loop(step=False)
        # fig.permute_axis(step)
        # self._interactive(jfig, step=False, msg='permuted', prev=prev)

    def _permute(self, jfig, step):
        fig, axs = self.figs[jfig]
        prev = fig.restore_view(axs)
        # prev = fig.loop(step=False)
        fig.permute_axis(step)
        self._interactive(jfig, step=False, msg='permuted', prev=prev)

    def _anchor(self, jfig, step):
        fig, _ = self.figs[jfig]
        fig.permute_anchor(step)
        fig.info(pfx=f'\r({jfig}) ', msg='anchor permuted.', sync=True)
        self._prompt(event=None)

    def _transpose(self, jfig):
        fig, axs = self.figs[jfig]
        prev = fig.restore_view(axs)
        # prev = fig.loop(step=False)
        base = fig.trees.inquire(prop='self', cls=VariableIter, single=True)
        if base:
            base.transpose()
        self._interactive(jfig, step=False, msg='transposed', prev=prev)

    def _resize(self, jfig, figsize=None, ref=None):
        fig, axs = self.figs[jfig]
        axs.resize(fig, figsize=figsize, ref=ref)
        # fig.canvas.draw()

    def _toggle_cmap(self, jfig):
        fig, _ = self.figs[jfig]
        if self.cmap:
            self.cmap.fwd()
            cmap = self.cmap.value()
            self._interactive(jfig, step=False, msg=f'colormap: {cmap}')
        # for k, cm in mplib.colormaps.items():
        #     print(k, cm)

    def _toggle_visible(self, jfig):
        fig, axs = self.figs[jfig]
        # print(jfig, 'toggle')
        axs.toggle_visible(ax='colorbar')
        fig.canvas.draw()

    def _toggle_axis(self, jfig, which):
        fig, axs = self.figs[jfig]
        axs.toggle_axis(which)
        # self._draw(jfig, step=False, msg=f'toggle: {which}')
        # ax = axs.atbl['body']
        fig.canvas.draw()

    def entire_view(self, jfig):
        """Entire view mode."""
        fig, axs = self.figs[jfig]
        # prev = fig.loop(step=False)
        prev = fig.restore_view(axs)
        prev = prev.fromkeys(prev)
        self._interactive(jfig, step=False, msg='entire', prev=prev)

    def duplicate(self, jfig):
        """Duplicate figure."""
        fig, axs = self.figs[jfig]
        prev = fig.restore_view(axs)
        ref = fig.trees.copy(init=False, recurse=True)
        return self._duplicate(fig, ref, prev)

    def refresh(self, jfig):
        """New fresh figure, close old."""
        jnew = self.duplicate(jfig)

        fig, _ = self.figs[jfig]
        plt.close(fig)
        del self.figs[jfig]
        fig.disconnect('key_press_event')
        print(f"\r({jfig}) regenerated > ({jnew})")

    def new(self, jfig):
        """New fresh figure."""
        fig, _ = self.figs[jfig]
        ref = fig.base.copy(init=True, recurse=True)
        self._duplicate(fig, ref)

    def _duplicate(self, fig, trees, view=None):
        """Duplicate figure core"""
        nfig, nax = self._new_control(figsize=fig, )
        jnew = nfig.number
        self.figs[jnew] = nfig, nax
        nfig.canvas.manager.show()

        nfig.bind(trees, base=fig.base, view=view)
        self._interactive(jnew)

        return jnew

    def event_handler(self, event):
        """Event handler"""
        kev = event.key
        # print('press:', event, kev)

        cf = event.canvas.figure
        jfig = cf.number
        fig, axs = self.figs[jfig]
        child = fig.trees
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
                    del self.figs[jfig]
                    fig.disconnect('key_press_event')
                    print(f"\r({jfig}) closed")
                else:
                    for fig, _ in self.figs.values():
                        plt.close(fig)
            elif cmd == 'next_cyclic':
                if sub == 'coordinate':
                    self.map_figures(self._permute, jfig, +1)
                elif sub == 'anchor':
                    self.map_figures(self._anchor, jfig, +1)
                else:
                    self.switch(jfig, draw=False, step=0)
                    self.switch(jfig, cls=cls, step=+1)
            elif cmd == 'prev_cyclic':
                if sub == 'coordinate':
                    self.map_figures(self._permute, jfig, -1)
                elif sub == 'anchor':
                    self.map_figures(self._anchor, jfig, -1)
                else:
                    self.switch(jfig, draw=False, step=0)
                    self.switch(jfig, cls=cls, step=-1)
            elif cmd == 'next':
                if sub == 'coordinate':
                    self.map_figures(self._permute, jfig, +1)
                else:
                    self.switch(jfig, draw=False, step=0)
                    self.switch(jfig, cls=(True, cls), step=+1)
            elif cmd == 'prev':
                if sub == 'coordinate':
                    self.map_figures(self._permute, jfig, -1)
                else:
                    self.switch(jfig, draw=False, step=0)
                    self.switch(jfig, cls=(True, cls), step=-1)
            elif cmd == 'clear':
                if sub == 'anchor':
                    self.map_figures(self._anchor, jfig, 0)
            elif cmd == 'info':
                if hseq:
                    fig.info(pfx=f'\r({jfig}) info: ')
                else:
                    for jfig, fx in self.figs.items():
                        fx[0].info(pfx=f'\r({jfig}) info: ')
            elif cmd == 'mark':
                if hseq:
                    fig.toggle_lock()
                else:
                    for fig, _ in self.figs.values():
                        fig.toggle_lock()
            elif cmd == 'unmark':
                for fig, _ in self.figs.values():
                    fig.toggle_lock(force=False)
            elif cmd == 'transpose':
                self.map_figures(self._transpose, jfig)
            elif cmd == 'enlarge':
                self.map_figures(self._resize, jfig, +self.opts['resize_step'])
            elif cmd == 'shrink':
                self.map_figures(self._resize, jfig, -self.opts['resize_step'])
            elif cmd == 'sync_geometry':
                self.map_figures(self._resize, jfig, fig)
            elif cmd == 'reset_geometry':
                self.map_figures(self._resize, jfig, True, ref=False)
            elif cmd == 'print':
                self.map_figures(self._savefig, jfig)
            elif cmd == 'toggle_cmap':
                self.map_figures(self._toggle_cmap, jfig)
            elif cmd == 'toggle_horizontal':
                self.map_figures(self._toggle_axis, jfig, 'h')
            elif cmd == 'toggle_vertical':
                self.map_figures(self._toggle_axis, jfig, 'v')
            elif cmd == 'toggle_visible':
                self.map_figures(self._toggle_visible, jfig)
            elif cmd == 'entire_view':
                self.map_figures(self.entire_view, jfig)
            elif hasattr(self, cmd):
                cmd = getattr(self, cmd)
                cmd(jfig)
            # else:
            #     print(f"Not yet implemented command={cmd}.")

    def parse_config(self, config, params):
        config = config or {}
        verbose = config.get('verbose', 0)
        self.kmap = self.parse_keymap({}, config.get('keymap', {}))
        self.opts = config.get('option', {})
        self.opts['resize_step'] = self.opts.get('resize_step') or 0.25

        if params:
            for k in params.find_all(r'^keymap\.*'):
                for p in params[k]:
                    if p in self.kmap:
                        if verbose > 0:
                            print(f"Remove default keymap for <{p}> from {k}.")
                        params[k].remove(p)

    def parse_keymap(self, kmap, config, group=()):
        for f, c in config.items():
            if isinstance(c, dict):
                self.parse_keymap(kmap, c, group + (f, ))
            elif isinstance(c, list):
                for k in c:
                    kmap[k] = (f, ) + group
            elif c != '':
                kmap[c] = (f, ) + group
        return kmap


def extract_sels(sel, data, strict=None):
    """Extract effective and normalized selections for data."""
    esel = dict.fromkeys(data.dims)  # extracted selection
    nsel = {}                        # normalized selection
    # md = len(data.dims)
    sel = sel or {}
    for co, sp in sel.items():
        try:
            co = parse_coord(data, co)
            esel[co] = sp
            if isinstance(sp, slice):
                start, stop = sp.start, sp.stop
                if isinstance(start, int):
                    start = data.coords[co][start].item()
                if isinstance(stop, int):
                    # stop = data.coords[co][stop].item()
                    stop = data.coords[co][stop-1].item()
                sp = slice(start, stop, sp.step)
            elif isinstance(sp, int):
                sp = data.coords[co][sp].item()
            nsel[co] = sp
        except UserWarning as exc:
            msg = f"no coordinate to select: {co}"
            if strict is True:
                raise UserWarning(msg) from exc
            print(msg)
    return esel, nsel


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
            name = zxr.search_coordinate(data, name)
        except KeyError as exc:
            raise UserWarning from exc
    return name

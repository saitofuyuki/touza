#!/usr/bin/env python3
# Time-stamp: <2024/10/22 12:05:30 fuyuki control.py>

__doc__ = \
    """
zbt.control
~~~~~~~~~~~
Figure traverse controller.

:Source:     zbt/control.py
:Maintainer:  SAITO Fuyuki <saitofuyuki@jamstec.go.jp>
:Created:    Oct 9 2024
"""

import collections.abc as cabc
import pathlib as plib
import math
import copy

import xarray as xr
import matplotlib.pyplot as plt

import zbt.plot as zplt
import zbt.xrnio as zxr


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
                    # self._l2p = self._l2p_single
                    self._l2i = self._l2i_single
                elif isinstance(key, tuple):
                    self._keys = None
                    self.shape = key
                    # self._l2p = self._l2p_tuple
                    self._l2i = self._l2i_raw
                elif isinstance(key, list):
                    self._keys = list(key)
                    self.shape = (len(self._keys), )
                    # self._l2p = self._l2p_key
                    self._l2i = self._l2i_single
                else:
                    raise ValueError(f"invalid key = {key}")
            elif array is not None:
                if self.array is None:
                    # array is reserved
                    self.shape = None
                    self._keys = None
                    # self._l2p = None
                    self._l2i = None
                elif isinstance(self.array, dict):
                    self.shape = (len(self.array), )
                    self._keys = list(self.array.keys())
                    # self._l2p = self._l2p_key
                    self._l2i = self._l2i_single
                elif isinstance(self.array, cabc.Iterable):
                    self.shape = (len(self.array), )
                    self._keys = None
                    # self._l2p = self._l2p_single
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
        # if bool(self._l2p):
        #     if key is None:
        #         key = self._current
        #     pkey = self._mask(key, self.mask)
        #     return self._l2p(pkey)
        pkey = self.l2i(key, mask=True)
        if self._keys:
            if pkey is not None:
                pkey = self._keys[pkey]
        return pkey
        # raise ValueError(self.msg("l2p function not bound."))

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
        # head = self.array[self.l2p(head)]
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

    def __iter__(self):
        ret = self.reset()
        # print(f"<{self.name}:__iter__> {self.init=} {ret=} {self.shape=}")
        while ret is not None:
            # ret = self._mask(ret, self.mask)

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
            self._current = None

    def _mask(self, val, mask):
        ret = ()
        mask = mask or (False, ) * len(val)
        for v, m in zip(val, mask):
            if m is False:
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
        return init

    def cue(self, init):
        """Cueing."""
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

    __slot__ = ["_transpose", "_sel", ]

    def __init__(self, **kw):
        super().__init__(array=None, name='ARRAY', **kw)
        self._transpose = False
        self._sel = None

    def status(self, fmt=True, recurse=False, dims=None):
        dlim = self._sel or {}
        sel = []
        dims = dims or []
        for c, x in enumerate(self.l2p()):
            if c >= len(dims):
                c = None
            else:
                c = dims[c]
            lim = dlim.get(c, slice(None, None, None))
            if isinstance(lim, int):
                lim = slice(lim, lim, 1)

            if isinstance(x, slice):
                b, e, s = x.start, x.stop, x.step
                p = ''
                if lim.start is not None:
                    b = (b or 0) + lim.start
                if lim.stop is not None:
                    if e is None:
                        e = lim.stop
                p = p + ('' if b is None else str(b))
                p = p + ':'
                p = p + ('' if e is None else str(e))
                if s is None or s == 1:
                    pass
                else:
                    p = p + ':' + str(s)
            else:
                p = str(x + (lim.start or 0))
            sel.append(p)
        txt = ','.join(sel)
        txt = f'[{txt}]'
        if recurse and self.child:
            ch = self.child.status(fmt=fmt, recurse=recurse)
            if ch:
                txt = txt + ' ' + ch
        # return f"{id(self)}: {txt}"
        return txt

    def update(self, key):
        # v = self.value(key)
        # print(v.dims)
        pass

    def offset(self, sel=None):
        self._sel = sel

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
        # print(f"_array  {id(self)}: {self._transpose}")

    def _value(self, key=None):
        data = super()._value(key)
        # print(self._transpose)
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
        # print(f'_array  {id(self)}: {self._transpose}, {data.shape}')
        return data

    def __getitem__(self, key):
        return super().__getitem__(key)


class VariableIter(LinkedArray):
    """Variable level iterator."""

    def __init__(self, coors=None, dim=None, **kw):
        super().__init__(array=None, name='VAR', **kw)
        self.coors = coors or (-2, -1)
        self.dim = dim
        self.sel = None

    def update(self, key):
        k = self.l2p(key)
        v = self.array.get(k)
        # print(f"<{self.name}:update> {self.current} {self.child.current}"
        #       f"> {key} {k} {v.shape}")
        if v is None:
            self.switch(cls=ArrayIter, array=None, key=0, )
        else:
            cue = False
            cidx = self.child.l2i()
            if self.dim:
                sel = {}
                w = len(v.dims)
                for c, s in self.dim.items():
                    try:
                        if isinstance(c, int):
                            if c >= w or c + w < 0:
                                raise UserWarning
                            sel[v.dims[c]] = s
                        else:
                            if c in v.dims:
                                sel[c] = s
                            else:
                                raise UserWarning
                    except UserWarning:
                        print(f"no coordinate to select: {c}")
                v = v.isel(sel)
                self.sel = sel
            else:
                pass

            shape = v.shape
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

            opts = self.opts or {}

            csel = self.set_coors(self.coors, v.dims)
            # mask = self.c2mask(self.coors, shape)
            mask = self.c2mask(csel, shape, v.dims)
            self.child.transpose(csel)
            self.child.offset(self.sel)
            self.switch(cls=ArrayIter,
                        array=v, key=shape, mask=mask)
            if cue:
                self.child.cue(cue)

    def set_coors(self, coors, dims):
        """Set coordinate tuple."""
        clist = []
        w = len(dims)
        for c in coors:
            try:
                if isinstance(c, int):
                    if c >= w or c + w < 0:
                        raise UserWarning
                    if c < 0:
                        c = c + w
                elif c == '':
                    pass
                else:
                    try:
                        c = dims.index(c)
                    except ValueError:
                        raise UserWarning
            except UserWarning:
                print(f"no coordinate to plot {c}.  Available={dims}")
            else:
                clist.append(c)
        rem = w
        nlist = []
        # print(f"{clist=}")
        for c in reversed(clist):
            if c == '':
                c = rem - 1
                while c in clist:
                    c = c - 1
                rem = c
            nlist.insert(0, c)
        # print(f"{nlist=}")
        return tuple(dims[c] for c in nlist if c >= 0)

    def c2mask(self, coors, shape, dims):
        w = len(shape)
        mask = (False, ) * len(shape)

        colon = slice(None, None, None)
        for c in coors:
            if c in dims:
                c = dims.index(c)
            mask = mask[:c] + (colon, ) + mask[c+1:]
        return mask[:w]

    def permute_axis(self, switch, skip_single=True):
        """Axis permutation."""
        mask = self.inquire(prop='mask', cls=ArrayIter, single=True)
        shape = self.inquire(prop='shape', cls=ArrayIter, single=True)

        reverse = switch < 0

        pat = [j for j, m in enumerate(reversed(mask)) if bool(m) != reverse]
        if skip_single:
            fskip = [2 - min(2, max(1, w)) for w in reversed(shape)]
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
        mask = [n] * mshp
        pat = pat[:-1]
        while pat:
            j = - 1 - pat[0]
            mask[j] = y
            pat = pat[1:]
        self.coors = tuple(- mshp + j for j, m in enumerate(mask) if m)
        # print(f"permute: {self.coors}")

        # k = self.l2p(None)
        # v = self.array.get(k)
        v = self.value()
        csel = self.set_coors(self.coors, v.dims)
        self.child.transpose(csel)

        self.switch(cls=ArrayIter, mask=mask)

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


class FileIter(LinkedArray):
    """File (or dataset) level iterator."""

    def __init__(self, files, variables=None, **kw):
        super().__init__(array={}, key=files, name='FILE', **kw)
        variables = False if variables is None else variables
        self.variables = variables

    def update(self, key):
        # print(f"<{self.name}:update> {self.current} > {key} {self.l2p(key)}")
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
        arr = self.inquire(prop='self', cls=ArrayIter, single=True)
        print(f"{var.coors=}")
        print(f"{arr._transpose=}")
        print(f"{arr.mask=}")
        print(f"{data.dims=}")
        return coors


class FigureInteractive(zplt.FigureBase):
    """Matplotlib figure class with interactive methods."""

    def __init__(self, *args, **kwds):
        super().__init__(*args, **kwds)
        self.cid = None
        self._loop = None
        self.trees = None
        self._lock = False
        self.fc = []
        self.sel = None

    def connect(self, handler=None):
        self.cid = self.canvas.mpl_connect('key_press_event', handler)
        return self.cid

    def disconnect(self):
        self.canvas.mpl_disconnect(self.cid)
        self.cid = None

    def bind(self, trees):
        self.trees = trees

    def permute_axis(self, switch):
        base = self.trees.inquire(prop='self', cls=VariableIter, single=True)
        if base:
            base.permute_axis(switch)

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

    def info(self, pfx=None, msg=None):
        pfx = pfx or ''
        txt = pfx + self.sel
        if self._lock:
            txt = txt + ' *'
        if msg:
            txt = txt + f' -- {msg}'
        print(txt)

    def loop(self, step=True):
        if self._loop is None:
            self._loop = iter(self.trees.items())
            step = True
        if step:
            try:
                cur = next(self._loop)
                self.sel = self.trees.status(recurse=True)
                return cur
            except StopIteration:
                self._loop = None
                cur = None
                self.sel = None
                raise
        else:
            k = self.trees.current
            cur = k, self.trees[k]
            self.sel = self.trees.status(recurse=True)
        return cur


class FigureControl():
    """Figure iteration controller."""

    def __init__(self, plot, trees,
                 interactive=True,
                 styles=None,
                 layout=None, config=None, params=None):
        self.parse_config(config, params)

        self.plot = plot
        self.layout = layout or zplt.LayoutLegacy3(cls=FigureInteractive)
        self.figs = {}
        self.ref_trees = trees
        self.output = None
        self.interactive = interactive
        self.styles = styles or {}

    def __call__(self, output=None):
        self.output = output or None
        fx = self.layout()
        fig = fx[0]
        jfig = fig.number
        self.figs[jfig] = fx
        # trees = copy.deepcopy(self.trees)
        trees = self.ref_trees.copy()
        fig.bind(trees)
        if self.interactive:
            self._draw(jfig)
            plt.show()
        else:
            self._batch(jfig)

    def _prompt(self):
        print('> ', end=' ', flush=True)

    def _draw(self, jfig, step=True, msg=None):
        fig, axs = self.figs[jfig]
        fig.disconnect()
        try:
            trees, stat = fig.loop(step)
        except StopIteration:
            print(f"\r({jfig}) no more data.")
            try:
                trees, stat = fig.loop(step)
            except StopIteration:
                raise StopIteration(f"\r({jfig}) no effective data.") from None

        data = stat[-1]
        # print(self.styles)
        var = fig.trees.inquire(prop='self', cls=VariableIter, single=True)
        arr = fig.trees.inquire(prop='self', cls=ArrayIter, single=True)
        print(fig.trees.plot_coordinates(data))
        # print(f"{data.dims=}")
        # print(trees[-1])
        # print(f'_draw: {data.shape=}')
        try:
            self.layout.reset(fig, axs)
            self.plot(fig=fig, axs=axs, data=data, title=fig.sel)
            fig.info(pfx=f'\r({jfig}) ', msg=msg)
        except UserWarning as err:
            fig.info(pfx=f'\r({jfig}) ', msg=err)
        self._prompt()
        fig.connect(self.event_handler)

    def _batch(self, jfig):
        fig, axs = self.figs[jfig]
        while True:
            try:
                trees, stat = fig.loop()
                data = stat[-1]
                self.plot(fig=fig, axs=axs, data=data, title=fig.sel)
                self._savefig(jfig)
            except StopIteration:
                break

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
            fx = self.figs[jf]
            fig = fx[0]
            child = fig.trees
            child.switch(*args, **kw)
            if draw:
                self._draw(jf)

    def map_figures(self, func, jfig, *args, **kw):
        if self._is_locked(jfig):
            targets = filter(self._is_locked, self.figs.keys())
        else:
            targets = [jfig]
        for jf in targets:
            func(jf, *args, **kw)

    def _savefig(self, jfig):
        if self.output:
            fx = self.figs[jfig]
            fig = fx[0]
            if fig.is_locked():
                face = fig.fc[-1]
            else:
                face = 'auto'
            if hasattr(self.output, 'savefig'):
                self.output.savefig(fig, facecolor=face)
            else:
                fig.savefig(self.output, facecolor=face)
            print(f"Saved: {self.output}")
        else:
            print(f"No output defined.")
        self._prompt()

    def _permute(self, jfig, step):
        fx = self.figs[jfig]
        fig = fx[0]
        fig.permute_axis(step)
        self._draw(jfig, step=False, msg='permuted')

    def _transpose(self, jfig):
        fx = self.figs[jfig]
        fig = fx[0]
        base = fig.trees.inquire(prop='self', cls=ArrayIter, single=True)
        if base:
            base.transpose()
        self._draw(jfig, step=False, msg='transposed')

    def _resize(self, jfig, rate=None):
        fx = self.figs[jfig]
        fig = fx[0]
        self.layout.resize(fig, rate=rate)

    def event_handler(self, event):
        """Event handler"""
        kev = event.key
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
                    fig.disconnect()
                    print(f"\r({jfig}) closed")
                else:
                    for fx in self.figs.values():
                        plt.close(fx[0])
            elif cmd == 'next_cyclic':
                if sub == 'coordinate':
                    self.map_figures(self._permute, jfig, +1)
                else:
                    self.switch(jfig, draw=False, step=0)
                    self.switch(jfig, cls=cls, step=+1)
            elif cmd == 'prev_cyclic':
                if sub == 'coordinate':
                    self.map_figures(self._permute, jfig, -1)
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
            elif cmd in ['duplicate', 'new', ]:
                fig.disconnect()
                nfx = self.layout(figsize=fig)
                nfig = nfx[0]
                jfig = nfig.number
                self.figs[jfig] = nfx
                nfig.canvas.manager.show()
                if cmd == 'duplicate':
                    trees = child.copy(init=False, recurse=True)
                else:
                    trees = self.trees.copy(init=True, recurse=True)
                # trees.status()
                nfig.bind(trees)
                fig.connect(self.event_handler)
                nfig.connect(self.event_handler)
                self._draw(jfig)

            elif cmd == 'info':
                if hseq:
                    fig.info(pfx=f'\r({jfig}) info: ')
                else:
                    for jfig, fx in self.figs.items():
                        fx[0].info(pfx=f'\r({jfig}) info: ')
                self._prompt()
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
            elif cmd == 'reset_geometry':
                self.map_figures(self._resize, jfig)
            elif cmd == 'print':
                self.map_figures(self._savefig, jfig)

    def parse_config(self, config, params):
        config = config or {}
        self.kmap = self.parse_keymap({}, config.get('keymap', {}))
        self.opts = config.get('option', {})
        self.opts['resize_step'] = self.opts.get('resize_step') or 0.25

        if params:
            for k in params.find_all(r'^keymap\.*'):
                for p in params[k]:
                    if p in self.kmap:
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

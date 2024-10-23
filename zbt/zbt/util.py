#!/usr/bin/env python3
# Time-stamp <2024/07/16 13:09:06 fuyuki touza.py>

__doc__ = \
    """
zbt.util
~~~~~~~~
Common helper utilities for TOUZA/zbt

:Source:     zbt/util.py
:Maintainer: SAITO Fuyuki <saitofuyuki@jamstec.go.jp>
:Created:    Jul 16 2024
"""

import ctypes as CT
import collections.abc as cabc
import numpy as np
# import traceback
# import collections as cols
import pprint as ppr
# import pynput.keyboard as PK
# import termios

__all__ = ['WrapCDLL', 'AutoString', 'NameMap', 'tostr', 'toint',
           'expand', 'flatten', 'map_recursive', 'join_attrs', ]


# pylint: disable=too-few-public-methods
class WrapCDLL(CT.CDLL):
    """Abstract CDLL layer of TOUZA utilities."""
    intent_in = 1
    intent_out = 2
    intent_inout = intent_in + intent_out
    intent_reset = 4

    def __init__(self, *args, **kwds):
        """Initialize CDLL."""
        super().__init__(*args, **kwds)

    def register_function(self, name, res, *args, err=None):
        """Wrap CFUNCTYPE declarative."""
        p = []
        a = []
        for i in args:
            p.append(i[0])
            a.append(i[1:])
        pt = CT.CFUNCTYPE(res, *p)
        f = pt((name, self), (tuple(a)))
        if err:
            f.errcheck = err
        self.__dict__[name] = f
        return f


class AutoString(CT.c_char_p):
    """Wrap string encoder for character pointer."""
    @classmethod
    def from_param(cls, value):
        """Encoder according to input value."""
        if isinstance(value, bytes):
            return value
        if isinstance(value, str):
            return value.encode()
        return value

_AutoArray = {}

def AutoArray(type):
    """Wrapper for ctypes.POINTER() function for automatic conversion."""
    ncls = _AutoArray.get(type)
    if ncls:
        return ncls
    class ncls(CT.POINTER(type)):
        """POINTER class of {type} with automatic encoding."""
        @classmethod
        def from_param(cls, value):
            """Encoder according to input value."""
            # print(f"{type}: {value}")
            return (type * len(value))(*value)
    _AutoArray[type] = ncls
    return ncls


# class AutoSizeT(CT.POINTER(CT.c_size_t)):
#     @classmethod
#     def from_param(cls, value):
#         """Encoder according to input value."""
#         return (CT.c_size_t * len(value))(*value)


class NameMap(dict):
    """Dict-like for zbt names."""

    def __init__(self, sep_=None, **kwargs):
        """Bypass parent initialization."""
        super().__init__()
        self.sep = sep_
        for key, val in kwargs.items():
            self.__setitem__(key, val)

    def __getitem__(self, key):
        if isinstance(key, tuple):
            key, idx = self._decompose_key(key)
            arr = super().__getitem__(key)
            if arr[idx] is None:
                raise IndexError(f"Not set for key [{key}, {idx}]")
            return arr[idx]
        arr = super().__getitem__(key)
        if len(arr) == 1:
            return arr[0]
        raise IndexError(f"Need explict index for key '{key}'")

    def __setitem__(self, key, value):
        arr = self._ensure_base(key)
        if isinstance(key, tuple):
            key, idx = self._decompose_key(key)
            la = len(arr)
            arr.extend([None] * (idx - la + 1))
            arr[idx] = value
        else:
            if arr[-1] is None:
                arr[-1] = value
            else:
                arr.append(value)

    def __iter__(self):
        for key in super().__iter__():
            arr = super().__getitem__(key)
            if len(arr) == 1:
                if arr[0] is not None:
                    yield key
            else:
                for idx, val in enumerate(arr):
                    if val is not None:
                        yield (key, idx, )

    def __contains__(self, key):
        # print(f"# contains {key}")
        # ppr.pprint(super().items())
        if isinstance(key, tuple):
            key, idx = self._decompose_key(key)
            if super().__contains__(key):
                arr = super().__getitem__(key)
                if idx < len(arr):
                    val = arr[idx]
                    return val is not None
            return False
        if super().__contains__(key):
            arr = super().__getitem__(key)
            return len(arr) == 1
        return False

    def get(self, key, default=None, single=True):
        if isinstance(key, tuple):
            key, idx = self._decompose_key(key)
            if super().__contains__(key):
                arr = super().__getitem__(key)
                val = arr[idx]
                return val or default
            return default
        default = default or []
        if not isinstance(default, list):
            default = [default]
        if super().__contains__(key):
            arr = super().get(key, default)
            if single:
                if len(arr) == 1:
                    return arr[0]
                if len(arr) == 0:
                    return None
            # print(f"{key} {arr=}")
            return arr
        if single:
            return default[0]
        return default

    def keys(self):
        return iter(self)

    def prefix_keys(self):
        """Iterator of base-class."""
        yield from super().keys()

    def values(self):
        for arr in super().values():
            for val in arr:
                if val is not None:
                    yield val

    def items(self):
        for key in super().__iter__():
            arr = super().__getitem__(key)
            if len(arr) == 1:
                if arr[0] is not None:
                    yield (key, arr[0], )
            else:
                for idx, val in enumerate(arr):
                    if val is not None:
                        yield ((key, idx), val, )

    def insert(self, key, num):
        """Insert num blank item at pos of key."""
        key, idx = self._decompose_key(key)
        arr = super().__getitem__(key)
        arr[idx:idx] = [None] * num

    def get_family(self, key, default=None):
        """Get all the values or default corresponding to key-name."""
        key, _ = self._decompose_key(key)
        if super().__contains__(key):
            return [val for val in super().__getitem__(key) if val is not None]
        return default

    def list_family(self, key):
        """Get all the values corresponding to key-name."""
        key, _ = self._decompose_key(key)
        return [val for val in super().__getitem__(key) if val is not None]

    def iter_family(self, key):
        """Iterate all the values corresponding to key-name."""
        key, _ = self._decompose_key(key)
        for val in super().__getitem__(key):
            if val is not None:
                yield val

    def format_key_all(self, sep=True):
        """Return list of canonicalized keys"""
        return [self.format_key(key, sep) for key in self.keys()]

    def format_key(self, key, sep=True):
        """Return canonicalized string of key"""
        key, idx = self._decompose_key(key)
        arr = super().__getitem__(key)
        if len(arr) == 1:
            return key
        if sep is True:
            sep = self.sep
        if sep:
            return tostr(key) + sep + str(idx)
        return (key, idx)

    def rev_map(self, val, sep=True):
        """Reverse mapping from val"""
        # print('val:', repr(val))
        # print('items:', super().items())
        for pfx, arr in super().items():
            try:
                idx = arr.index(val)
                return self.format_key((pfx, idx), sep=sep)
            except ValueError:
                pass
        raise ValueError

    def _ensure_base(self, key):
        """Initialize base mapping as empty list if key is not set"""
        if isinstance(key, tuple):
            key, _ = key
        if not super().__contains__(key):
            arr = [None]
            super().__setitem__(key, arr)
        else:
            arr = super().__getitem__(key)
        return arr

    def _decompose_key(self, key):
        """Decompose key (name, idx) tuple"""
        if isinstance(key, tuple):
            return (key[0], key[1], )
        return (key, 0,)

    def _normalize_key(self, key):
        """Normalize key to tuple"""
        if isinstance(key, tuple):
            return key
        return (key, 0, )

    def __str__(self):
        return ppr.pformat(dict(super().items()))

# ###  Not applicable
# class SliceTuple(slice):
#     """Easy access of slice attributes."""
#     def __getitem__(self, elem):
#         if elem in [0, 'start']:
#             return self.start
#         if elem in [1, 'stop']:
#             return self.stop
#         if elem in [2, -1, 'step']:
#             return self.step
#         raise ValueError(f"Invalid element {elem} for SliceTuple")


class Selection(tuple):
    """Tuple of Slice or index for array elements selection."""

    def __new__(cls, elem, dim=None):
        if dim is None:
            dim = elem
        elif np.iterable(dim):
            dim = len(dim)

        if np.iterable(elem):
            ne = elem.count(Ellipsis)
            if ne > 1:
                raise TypeError(f"Multiple ellipsis {elem}")
            je = elem.index(Ellipsis) if ne == 1 else len(elem)
        else:
            if elem is Ellipsis:
                je = 0
                elem = (elem, )
            else:
                je = 1
                elem = (elem, Ellipsis, )
        # ehead = tuple(slice(e, e + 1)
        #               if isinstance(e, int) else e for e in elem[:je])
        # etail = tuple(slice(e, e + 1)
        #               if isinstance(e, int) else e for e in elem[je+1:])
        ehead = elem[:je]
        etail = elem[je+1:]
        emid = (slice(None, None, None), ) * (dim - len(ehead + etail))
        # print (ehead, etail, emid)
        return super(Selection, cls).__new__(cls, ehead + emid + etail)

    def __str__(self):
        ret = []
        for slc in self:
            if isinstance(slc, int):
                p = str(slc)
            else:
                b, e, s = slc.start, slc.stop, slc.step
                p = ''
                p = p + ('' if b is None else str(b))
                p = p + ':'
                p = p + ('' if e is None else str(e))
                if s is None or s == 1:
                    pass
                else:
                    p = p + ':' + str(s)
            ret.append(p)
        return ','.join(ret)


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
            elif n < 0:
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


def tostr(s):
    """String conversion"""
    try:
        ss = s.decode()
# pylint: disable=bare-except
    except:
        try:
            ss = str(s)
# pylint: disable=bare-except
        except:
            ss = s
    return ss


def toint(s):
    """Integer conversion if possible."""
    try:
        n = int(s)
        return n
    except ValueError:
        return s


def expand(array, mask, default=None):
    """Expand array to mask length accorinding to mask boolean."""
    r = []
    j = 0
    for b in mask:
        if b:
            if j < len(array):
                r.append(array[j])
                j = j + 1
            else:
                raise ValueError("Out of bounds.")
        else:
            r.append(default)
    return type(array)(r)


def flatten(data):
    """Flatten list."""
    # other sequences?
    if isinstance(data, (list, tuple, )):
        for item in data:
            yield from flatten(item)
    else:
        yield data


def map_recursive(func, data):
    """Recursive mapping to apply function."""
    # other sequences?
    if isinstance(data, (list, tuple, )):
        return [map_recursive(func, v) for v in data]
    return func(data)


def join_attrs(attrs: dict, head: str,
               strip: bool = False,
               sep: str | None = None,
               sort: cabc.Callable | None = None) -> list | str:
    """Join all the values of keys as head*."""
    a = {}
    lh = len(head)
    for k, v in attrs.items():
        if k.startswith(head):
            k = k[lh:]
            a[k] = v
    def do_strip(s):
        return s.strip()
    if strip:
        # ret = [str(a[k]).strip() for k in sorted(a.keys(), key=sort)]
        ret = [map_recursive(do_strip, a[k])
               for k in sorted(a.keys(), key=sort)]
    else:
        ret = [a[k] for k in sorted(a.keys(), key=sort)]
    if sep is None:
        return ret
    return sep.join(flatten(ret))


def main(argv):
    """Test driver."""
    import pprint as ppr
    X = NameMap()
    k = 'a'
    X[k] = f"{k}/0"
    X[k] = f"{k}/1"
    X[k] = f"{k}/2"
    X[k, 1] = f"{k}/1 change"
    X[k, 4] = f"{k}/4 skip"

    k = 'b'
    X[k] = f"{k}/0"
    X[k] = f"{k}/1"

    k = 'c'
    X[k] = f"{k}/0 only"

    print("object")
    for k in X:
        print(k, X[k])

    print("keys()")
    for k, v in X.items():
        print(k, v)

    print("values()")
    for v in X.values():
        print(v)

    print("items()")
    for kv in X.items():
        print(kv)

    print("format_key()")
    for k, v in X.items():
        k = X.format_key(k, '~')
        print(f"[{k}]={v}")

    print("pprint()")
    ppr.pprint(X)

    print("insert")
    X.insert('b', 2)

    print("pprint()")
    ppr.pprint(X)

    for k in ['a', 'b', 'c', ]:
        print(f"family[{k}]: {list(X.iter_family(k))}")

    for k in ['b', 'c', ]:
        chk = k in X
        try:
            print(f"name-only[{k}]: {X[k]} / {chk}")
        except Exception as e:
            print(f"name-only[{k}] failed / {chk}")
            print(f"{type(e)}{e}")

    attrs = {'x0': 'X000 ', 'x10': ' X010 ', 'x2': ' X002',
             'y0': 'Y000 ', 'y10': ' Y010 ', 'y2': ' Y002', }
    print(join_attrs(attrs, 'x'))
    print(join_attrs(attrs, 'x', sep='--'))
    print(join_attrs(attrs, 'x', sep='--', strip=True))
    print(join_attrs(attrs, 'x', sep='--', sort=int))

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

    # FI = TupleIterator('figure', None, ['Fig-a', 'Figb', ], )
    # print(FI)

# def on_press(key):
#     try:
#         print('alphanumeric key {0} pressed'.format(
#             key.char))
#     except AttributeError:
#         print('special key {0} pressed'.format(
#             key))

# def on_release(key):
#     print('{0} released'.format(
#         key))
#     if key == keyboard.Key.esc:
#         # Stop listener
#         return False


# def wait_press():
#     # Collect events until released
#     with PK.Listener(on_press=on_press) as listener:
#         listener.join()
#     termios.tcflush(sys.stdin, termios.TCIOFLUSH)


if __name__ == '__main__':
    import sys
    main(sys.argv[1:])

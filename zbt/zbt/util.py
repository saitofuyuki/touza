#!/usr/bin/env python3
# Time-stamp: <2025/02/17 10:50:32 fuyuki util.py>
#
# Copyright (C) 2024, 2025
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

"""
Common helper utilities for TOUZA/Zbt.

:Source:     zbt/util.py
:Maintainer: SAITO Fuyuki
:Created:    Jul 16 2024
"""

import sys
import logging
import collections.abc as cabc
import ctypes as CT
import pprint as ppr

from typing import Any, MutableMapping

import numpy

# import numbers as nums
# import traceback
# import collections as cols
# import pynput.keyboard as PK
# import termios

__all__ = ['NC_EPOCH',
           'LocalAdapter',
           'WrapCDLL', 'AutoString', 'NameMap',
           'tostr', 'toint', 'tonumber',
           'flatten', 'map_recursive', 'join_attrs',
           'set_default', 'logger',
           'diag', ]

# nc_time_axis wrapper
try:
    import nc_time_axis
    NC_EPOCH = nc_time_axis._TIME_UNITS
except ModuleNotFoundError:
    NC_EPOCH = None

# library logging
logger = logging.getLogger(__name__)
handler = logging.StreamHandler()
FMT = '[{levelname}] {message}'
formatter = logging.Formatter(FMT, style='{')
handler.setFormatter(formatter)
logger.addHandler(handler)

_logger = logger                # alias

class LocalAdapter(logging.LoggerAdapter):
    """
    Common logging class for TOUZA/Zbt.
    """

    def __init__(self,
                 module: str,
                 logger: logging.Logger|None = None,
                 extra: dict|None = None):
        """
        Wrap logging.LoggerAdapter instance with some properties.

        Parameters
        ----------
        module : str
           Module tag string used as prefix in log messages.
        logger : loggin.Logger
           Pass to parent method.
        extra : dict
           Pass to parent method.
        """

        extra = {'module': module} | (extra or {})
        logger = logger or _logger
        super().__init__(logger, extra)

    def process(self,
                msg: Any,
                kwargs: MutableMapping[str, Any]) \
                -> tuple[str, MutableMapping[str, Any]]:
        """
        Override parent.process() method.

        Returns:
        str
           Formatted message.

        See Also
        --------
        logging.LoggerAdapter.process : parent method to override.
        """

        extra = self.extra or {}
        tags = [str(extra[k])
                for k in ['module', 'class_', 'func', 'aux', ]
                if k in extra]
        tag = ':'.join(tags)
        msg = f"<{tag}> {msg}"
        return msg, kwargs

    def is_debug(self) -> bool:
        """Check if loggin.DEBUG level."""
        return self.isEnabledFor(logging.DEBUG)

    def is_info(self) -> bool:
        """Check if loggin.INFO level."""
        return self.isEnabledFor(logging.INFO)


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

    def register_function(self,
                          name: str,
                          res,
                          *args,
                          err: cabc.Callable|None = None):
        """
        Wrap CFUNCTYPE declarative.

        Parameters
        ----------
        name : str
           Name of function to register.  It also defines
           self.str method.
        res : any
           Return type.
        *args
          Function arguments definition.
          Tuple of type, intent, variable name, and optional
          default value.
        err : callable
          Error check method.

        Examples
        --------

        Create self.tnb_init() method which returns CT.c_int, and require
        two arguments 'levv' and 'mode'.  Both arguments are optional and
        default values are 0.

        >>> self.register_function("tnb_init",
                                   CT.c_int,
                                   (CT.c_int, self.intent_in, 'levv', 0),
                                   (CT.c_int, self.intent_in, 'mode', 0),
                                   err=self.errcheck_strict)
        """
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

_AutoArray: dict[type, type] = {}

def AutoArray(type_, /):
    """
    Wrapper for ctypes.POINTER() function for automatic conversion.

    Once created, it is stored in internal buffer and reused.

    Parameters
    ----------
    type_ : type
       Data type defined in ctypes.

    Returns
    -------
    class
       Subclass of one generated by ctypes.POINTER.

    See Also
    --------
    ctypes.POINTER.
       Reference method.

    ctypes._CData.from_param
       Reference function.
    """
    ncls = _AutoArray.get(type_)
    # print(type(type_).__mro__)
    # print(type(ncls).__mro__)
    if ncls:
        return ncls

    _name = f"Pointer_{type_}"
    _doc = f"""POINTER class of {type_} with automatic encoding."""
    _module = __name__

    def from_param(_cls, value):
        """Encoder according to input value."""
        # print(f"{type_}: {value}")
        return (type_ * len(value))(*value)

    attr = {'__module__': _module,
            '__doc__': _doc,
            'from_param': classmethod(from_param),
            '__static_attributes__': (), }

    ncls = type(_name,
                (CT.POINTER(type_), ),
                attr)
    _AutoArray[type_] = ncls
    return ncls


class NameMap(dict):
    """
    Dict-like for zbt names.

    Keys can be duplicated, which results in generation of list
    to contain all the values.

    """

    def __init__(self,
                 sep_: str|None=None,
                 /, **kwargs):
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

    def get(self, key, /, default=None, single=True):
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
            # print(pfx, arr)
            try:
                idx = arr.index(val)
                return self.format_key((pfx, idx), sep=sep)
            except ValueError:
                pass
        raise ValueError(f"{val}")

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
        """
        Generator method to wrap builtin tuple.

        Parameters
        ----------
        elem : tuple or atom
           Element(s) which may some items.
        dim : tuple or int
           Expected tuple shape.

        Returns
        -------
        tuple
           Expanded tuple to fill null-slice.

        Raises
        ------
        TypeError
           If elem contains two or more Ellipsis.

        Examples
        --------
        >>> Selection(0, 3)
        (0, slice(None, None, None), slice(None, None, None))
        >>> print(Selection(0, 3))
        0,:,:
        >>> print(Selection((0, 1), 4))
        0,1,:,:
        >>> print(Selection((0, Ellipsis), 4))
        0,:,:,:
        >>> print(Selection((0, Ellipsis, 1), 4))
        0,:,:,1
        >>> print(Selection((Ellipsis, 1), 4))
        :,:,:,1
        >>> Selection((0, Ellipsis, 1, Ellipsis), 4)
        TypeError: Multiple ellipsis (0, Ellipsis, 1, Ellipsis)
        """

        fill = slice(None, None, None)

        if dim is None:
            dim = len(elem)
        elif numpy.iterable(dim):
            dim = len(dim)

        if numpy.iterable(elem):
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
        ehead = elem[:je]
        etail = elem[je+1:]
        emid = (fill, ) * (dim - len(ehead + etail))
        return super(Selection, cls).__new__(cls, ehead + emid + etail)

    def __str__(self):
        ret = []
        fill = ':'
        for slc in self:
            if isinstance(slc, int):
                p = str(slc)
            else:
                b, e, s = slc.start, slc.stop, slc.step
                p = ''
                p = p + ('' if b is None else str(b))
                p = p + fill
                p = p + ('' if e is None else str(e))
                if s is None or s == 1:
                    pass
                else:
                    p = p + fill + str(s)
            ret.append(p)
        return ','.join(ret)


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


def tonumber(s):
    """Integer or float conversion if possible."""
    try:
        n = int(s)
        return n
    except ValueError:
        pass
    try:
        n = float(s)
        return n
    except ValueError:
        pass
    return s


# def expand(array, mask, default=None):
#     """Expand array to mask length accorinding to mask boolean."""
#     r = []
#     j = 0
#     for b in mask:
#         if b:
#             if j < len(array):
#                 r.append(array[j])
#                 j = j + 1
#             else:
#                 raise ValueError("Out of bounds.")
#         else:
#             r.append(default)
#     return type(array)(r)


def flatten(data):
    """Flatten list."""
    # other sequences?
    if isinstance(data, (list, tuple, )):
        for item in data:
            yield from flatten(item)
    else:
        yield data


def map_recursive(func: cabc.Callable, data: Any) -> list:
    """
    Recursive mapping to apply function.

    Parameters
    ----------
    func : callable
       Function of single argument.
    data : iterable or atom
       Collection of arguments to call func().
    """
    # other sequences?
    if isinstance(data, (list, tuple, )):
        return [map_recursive(func, v) for v in data]
    return func(data)


def join_attrs(attrs: dict, head: str,
               strip: bool = False,
               sep: str | None = None,
               sort: cabc.Callable | None = None) -> list | str:
    """
    Join or extract all the values where key starts with head.

    Parameters
    ----------
    attrs : dict
       Source key-value array.
    head : str
       Prefix string to compare with attrs.keys().
    strip : bool
       Whether to strip the values.
    sep : str|None
       Separator to concatenate into single string.
       If None, returns list.
    sort : callable
       Sort method to iterate keys.

    Returns
    -------
    str
       Concatenation of those values correponding to keys.
    list
       Filtered list of values correponding to keys.

    Examples
    --------
    >>> src = {'x0': 'X000 ', 'x10': ' X010 ', 'x2': ' X002',
    ...        'y0': 'Y000 ', 'y10': ' Y010 ', 'y2': ' Y002', }
    >>> join_attrs(src, 'x')
    ['X000 ', ' X010 ', ' X002']
    >>> join_attrs(src, 'x', sep='--')
    'X000 -- X010 -- X002'
    >>> join_attrs(src, 'x', sep='--', strip=True)
    'X000--X010--X002'
    >>> join_attrs(src, 'x', sep='--', sort=int)
    'X000 -- X002-- X010'
    """
    a = {}
    lh = len(head)
    for k, v in attrs.items():
        if k.startswith(head):
            k = k[lh:]
            a[k] = v
    def do_strip(s):
        """Call strip method of s."""
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


def set_default(var: Any, default: Any, null:Any=None):
    """
    Return default if var is null.

    Parameters
    ----------
    var : Any
       Object to check.
    default : Any
       Default value if object equals to null.
    null : Any
       Target object.

    Returns
    -------
    object
      Value either var or default.
    """
    if var == null:
        return default
    return var


def diag():
    """Diagnose module properties for debug."""
    locallog = LocalAdapter('util')
    locallog.info(f"{numpy=}")


def main(argv):
    """Test driver."""
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
        print(f"{k=} {X[k]=}")

    print("keys()")
    for k, v in X.items():
        print(f"{k}: {v}")

    print("values()")
    for v in X.values():
        print(f"{v=}")

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
        except IndexError as e:
            print(f"name-only[{k}] failed / {chk}")
            print(f"error={e}")

    attrs = {'x0': 'X000 ', 'x10': ' X010 ', 'x2': ' X002',
             'y0': 'Y000 ', 'y10': ' Y010 ', 'y2': ' Y002', }
    print(join_attrs(attrs, 'x'))
    print(join_attrs(attrs, 'x', sep='--'))
    print(join_attrs(attrs, 'x', sep='--', strip=True))
    print(join_attrs(attrs, 'x', sep='--', sort=int))

    for dim in [3, 4, 5]:
        for src in [0,
                    (0, ),
                    (0, 1),
                    (0, Ellipsis),
                    (Ellipsis, 1),
                    (0, Ellipsis, 1),
                    (0, Ellipsis, 1, Ellipsis)]:
            try:
                elem = Selection(src, dim)
                print(src, dim, elem)
            except TypeError as err:
                # raise err
                print(src, dim, err)

if __name__ == '__main__':
    main(sys.argv[1:])

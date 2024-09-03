#!/usr/bin/env python3
# Time-stamp <2024/07/16 13:09:06 fuyuki touza.py>

__doc__ = \
    """
zbt.util
~~~~~~~~
Common helper utilities for TOUZA/tupy

:Source:     zbt/util.py
:Maintainer: SAITO Fuyuki <saitofuyuki@jamstec.go.jp>
:Created:    Jul 16 2024
"""

import ctypes as CT
# import pprint as ppr
# import pynput.keyboard as PK
# import termios

__all__ = ['WrapCDLL', 'AutoString', 'NameMap', 'tostr', ]


class WrapCDLL(CT.CDLL):
    """Abstract layer of TOUZA utilities."""
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
        if isinstance(value, bytes):
            return value
        if isinstance(value, str):
            return value.encode()
        return value


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

    def get(self, key, default=None):
        if isinstance(key, tuple):
            key, idx = self._decompose_key(key)
            if super().__contains__(key):
                arr = super().__getitem__(key)
                val = arr[idx]
                return val or default
            return default
        if super().__contains__(key):
            arr = super().__getitem__(key)
            if len(arr) == 1:
                return arr[0] or default
            return default
        return default

    def keys(self):
        return iter(self)

    def prefix_keys(self):
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


def tostr(s):
    """String conversion"""
    try:
        ss = s.decode()
    except:
        try:
            ss = str(s)
        except:
            ss = s
    return ss




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


def main(argv):
    import pprint as ppr
    X = NameMap()
    k = 'a'
    X[k] = f"{k}/0"
    X[k] = f"{k}/1"
    X[k] = f"{k}/2"
    X[k,1] = f"{k}/1 change"
    X[k,4] = f"{k}/4 skip"

    k = 'b'
    X[k] = f"{k}/0"
    X[k] = f"{k}/1"

    k = 'c'
    X[k] = f"{k}/0 only"

    print("object")
    for k in X:
        print(k, X[k])

    print("keys()")
    for k in X.keys():
        print(k, X[k])

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

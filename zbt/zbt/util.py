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
import sys
# import pynput.keyboard as PK
# import termios


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

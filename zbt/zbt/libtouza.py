#!/usr/bin/env python3
# Time-stamp: <2025/02/12 08:08:25 fuyuki libtouza.py>

"""
Python interface on TOUZA library.

:Source:     zbt/libtouza.py
:Maintainer: SAITO Fuyuki <saitofuyuki@jamstec.go.jp>
:Created:    Jul 16 2024
"""

import os
import sys
import logging
import pathlib as plib
import ctypes as CT
import ctypes.util as CTU
import collections.abc as cabc

from . import util as zutl
from . import param as zpar

locallog = zutl.LocalAdapter(__name__)

try:
    import zbt.env as zenv
except ModuleNotFoundError:
    class PlaceHolder():
        """Default placeholder for some environments."""
        TOUZA_NAME = 'touza'
        LIBRARY_DIR = '.'
    zenv = PlaceHolder()

ENV_TOUZA_LIB = 'TOUZA_LIBRARY'

""""""


class LibTouzaException(Exception):
    """Base exception for TOUZA library."""


class LibTouzaCore(zutl.WrapCDLL):
    """Low-level class for TOUZA library."""

    def __init__(self,
                 name: str|os.PathLike|bool|None,
                 *args,
                 path: str|os.PathLike|list|None=None,
                 touza: dict|None=None,
                 **kwds):
        """
        Initialize CDLL after TOUZA.

        Parameters
        ----------
        name : PathLike, bool
            Name of touza shared library.
        *args : any
            Arguments to pass to WrapCDLL.
        path : list, pathlike
            Path to search the library.
        touza : dict
            Arguments and their values to pass to touza.
        **kwds : dict
            Keywords to pass to WrapCLL.

        Raises
        ------
        UserWarning
            Raised if touza library is disbled.
        OSError
            Raised if touza library is not found.
        """

        search = []
        if name is None:
            if isinstance(path, list):
                search.extend(path)
            elif not path is None:
                search.append(path)
            search.append(zenv.LIBRARY_DIR)
            search.append(os.environ.get(ENV_TOUZA_LIB))

            base = 'lib' + zenv.TOUZA_NAME + '.so'

            for p in search:
                if not p:
                    continue
                p = plib.Path(p)
                if p.is_dir():
                    p = p / base
                if p.exists():
                    name = p
                    break
        self._touza = touza or {}
        if name is False:
            # locallog.warning(f"Disable touza library {name}.")
            raise UserWarning("Disable touza library.")
        if name in [True, None, ]:
            name = CTU.find_library(zenv.TOUZA_NAME)
        try:
            super().__init__(name, *args, **kwds)
        except OSError:
            locallog.error("Cannot load touza library {name}.  "
                           f"Setting {ENV_TOUZA_LIB} environment may help.")
            raise

    def __del__(self, *args, **kwds):
        """Destructor."""


class LibTouzaNio(LibTouzaCore, zpar.ParamTouzaNio):
    """TOUZA/Nio friendly interfaces."""

    def __init__(self, *args, **kwds):
        """Initialize CDLL after TOUZA."""
        self._load = False
        try:
            super().__init__(*args, **kwds)
        except UserWarning as err:
            locallog.warning(err)
            return
        try:
            self._register_all()
        except AttributeError as err:
            locallog.warning(err)
            raise UserWarning from None
        self._load = True

    def _register_all(self):
        # self.len_item = CT.c_int.in_dll(self, 'len_item')
        # extern int tnb_init(const int levv, const int mode);
        self.register_function("tnb_init",
                               CT.c_int,
                               (CT.c_int, self.intent_in, 'levv', 0),
                               (CT.c_int, self.intent_in, 'mode', 0),
                               err=self.errcheck_strict)
        # extern int tnb_diag(const int levv, const int mode);
        self.register_function("tnb_diag",
                               CT.c_int,
                               (CT.c_int, self.intent_in, 'levv', 0),
                               (CT.c_int, self.intent_in, 'mode', 0),
                               err=self.errcheck_strict)
        # extern int tnb_finalize(const int levv, const int mode);
        self.register_function("tnb_finalize",
                               CT.c_int,
                               (CT.c_int, self.intent_in, 'levv', 0),
                               (CT.c_int, self.intent_in, 'mode', 0),
                               err=self.errcheck_strict)
        # extern int tnb_file_is_nio(const char *path);
        self.register_function("tnb_file_is_nio",
                               CT.c_int,
                               (zutl.AutoString, self.intent_in, 'path',),
                               err=self.errcheck_format)
        # extern int tnb_file_open(const char *path, const int flag);
        self.register_function("tnb_file_open",
                               CT.c_int,
                               (zutl.AutoString, self.intent_in, 'path',),
                               (CT.c_int, self.intent_in, 'flag',
                                self.NIO_CACHE_COLL_DEFAULT, ),
                               err=self.errcheck_handle)
        # extern int tnb_file_open(const char *path, const int flag);
        self.register_function("tnb_file_is_opened",
                               CT.c_int,
                               (zutl.AutoString, self.intent_in, 'path',),
                               err=None)
        # extern int tnb_file_diag(const int handle, const int lev);
        self.register_function("tnb_file_diag",
                               CT.c_int,
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'lev', 0),
                               err=self.errcheck_strict)
        # extern int tnb_file_close(const int handle);
        self.register_function("tnb_file_close",
                               CT.c_int, (CT.c_int, self.intent_in, 'handle',),
                               err=self.errcheck_strict)
        # extern int tnb_file_groups(const int handle);
        self.register_function("tnb_file_groups",
                               CT.c_int, (CT.c_int, self.intent_in, 'handle',))
        # extern int tnb_group_co_idx(const int handle, const int cid);
        self.register_function("tnb_group_co_idx",
                               CT.c_int,
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'cid',))
        # extern int tnb_group_coors(const int handle);
        self.register_function("tnb_group_coors",
                               CT.c_int, (CT.c_int, self.intent_in, 'handle',))
        # extern int tnb_group_co_name(char * const name,
        #                              const int handle, const int cid);
        self.register_function("tnb_group_co_name",
                               CT.c_int,
                               (zutl.AutoString, self.intent_in, 'name',),
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'cid',),
                               err=self.errcheck_strict)
        # extern int tnb_group_co_range(int *jbgn, int *jend,
        #                               const int handle, const int cid);
        self.register_function("tnb_group_co_range",
                               CT.c_int,
                               (CT.POINTER(CT.c_int), self.intent_in, 'jbgn'),
                               (CT.POINTER(CT.c_int), self.intent_in, 'jend'),
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'cid',),
                               err=self.errcheck_strict)
        # extern int tnb_group_vars(const int handle);
        self.register_function("tnb_group_vars",
                               CT.c_int, (CT.c_int, self.intent_in, 'handle',))
        # extern int tnb_group_recs(const int handle);
        self.register_function("tnb_group_recs",
                               CT.c_int, (CT.c_int, self.intent_in, 'handle',))
        # extern int tnb_group(const int handle, const int gidx);
        self.register_function("tnb_group",
                               CT.c_int,
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'gidx',),
                               err=self.errcheck_handle)
        # extern int tnb_search_group(const int handle,
        #                             const char *name, const int refh);
        self.register_function("tnb_search_group",
                               CT.c_int,
                               (CT.c_int, self.intent_in, 'handle',),
                               (zutl.AutoString, self.intent_in, 'name',),
                               (CT.c_int, self.intent_in, 'refh',),
                               err=self.errcheck_handle)
        # extern int tnb_group_name(const char *name, const int handle);
        self.register_function("tnb_group_name",
                               CT.c_int,
                               (zutl.AutoString, self.intent_in, 'name',),
                               (CT.c_int, self.intent_in, 'handle',),
                               err=self.errcheck_handle)
        # extern int tnb_var_name(const char *name,
        #                         const int handle, const int vid);
        self.register_function("tnb_var_name",
                               CT.c_int,
                               (zutl.AutoString, self.intent_in, 'name',),
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid',),
                               err=self.errcheck_strict)
        # extern int tnb_search_var(const int handle, const char *name);
        self.register_function("tnb_search_var",
                               CT.c_int,
                               (CT.c_int, self.intent_in, 'handle',),
                               (zutl.AutoString, self.intent_in, 'name',),
                               err=self.errcheck_handle)
        # extern int tnb_var_recs(const int handle, const int vid);
        self.register_function("tnb_var_recs",
                               CT.c_int,
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid',))
        # extern int tnb_co_size(const int handle, const int vid);
        self.register_function("tnb_co_size",
                               CT.c_int,
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid',))
        # extern int tnb_co_serial(const int handle,
        #                          const int vid, const int cid);
        self.register_function("tnb_co_serial",
                               CT.c_int,
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid',),
                               (CT.c_int, self.intent_in, 'cid',))
        # extern int tnb_co_len(const int handle,
        #                       const int vid, const int cid);
        self.register_function("tnb_co_len",
                               CT.c_int,
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid',),
                               (CT.c_int, self.intent_in, 'cid',))
        # extern int tnb_co_idx(const int handle,
        #                       const int vid, const char *name);
        self.register_function("tnb_co_idx",
                               CT.c_int,
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid',),
                               (zutl.AutoString, self.intent_in, 'name',),
                               err=self.errcheck_handle)
        # extern int tnb_co_name(char * const name,
        #                        const int handle, const int vid,
        #                        const int cid);
        self.register_function("tnb_co_name",
                               CT.c_int,
                               (zutl.AutoString, self.intent_in, 'name',),
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid',),
                               (CT.c_int, self.intent_in, 'cid',),
                               err=self.errcheck_strict)
        # extern int tnb_attr_size(const int handle, const int vid,
        #                          const int rec);
        self.register_function("tnb_attr_size",
                               CT.c_int,
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid',),
                               (CT.c_int, self.intent_in, 'rec',))
        # extern int tnb_attr_len(const char *item,
        #                         const int handle, const int vid,
        #                         const int rec);
        self.register_function("tnb_attr_len",
                               CT.c_int,
                               (zutl.AutoString,
                                self.intent_in, 'attr'),
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid', -1),
                               (CT.c_int, self.intent_in, 'rec', -1))
        # extern int tnb_get_header(char * const head,
        #                           const int handle, const int vid,
        #                           const int rec);
        self.register_function("tnb_get_header",
                               CT.c_int,
                               (zutl.AutoString, self.intent_in, 'head',),
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid', -1),
                               (CT.c_int, self.intent_in, 'rec', -1),
                               err=self.errcheck_strict)

        # extern int tnb_get_attr(char * const attr, const char *item,
        #                         const int handle, const int vid,
        #                         const int rec);
        self.register_function("tnb_get_attr",
                               CT.c_int,
                               (zutl.AutoString, self.intent_in, 'attr',),
                               (zutl.AutoString, self.intent_in, 'item',),
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid', -1),
                               (CT.c_int, self.intent_in, 'rec', -1),
                               err=self.errcheck_strict)
        # extern int tnb_get_attr_float(float * const attr, const char *item,
        #                               const int handle, const int vid,
        #                               const int rec);
        self.register_function("tnb_get_attr_float",
                               CT.c_int,
                               (CT.POINTER(CT.c_float),
                                self.intent_in, 'attr',),
                               (zutl.AutoString, self.intent_in, 'item',),
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid',),
                               (CT.c_int, self.intent_in, 'rec',),
                               err=self.errcheck_strict)
        # extern int tnb_get_attr_byid(char * const attr, const int item,
        #                              const int handle, const int vid,
        #                              const int rec);
        self.register_function("tnb_get_attr_byid",
                               CT.c_int,
                               (zutl.AutoString, self.intent_in, 'attr',),
                               (CT.c_int, self.intent_in, 'item',),
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid',),
                               (CT.c_int, self.intent_in, 'rec',),
                               err=self.errcheck_strict)
        # extern int tnb_get_attr_name(char * const attr, const int item);
        self.register_function("tnb_get_attr_name",
                               CT.c_int,
                               (zutl.AutoString, self.intent_in, 'attr',),
                               (CT.c_int, self.intent_in, 'item',),
                               err=self.errcheck_strict)
        # extern int tnb_rec_time(char * const time,
        #                         const int handle, const int vid,
        #                         const int rec);
        self.register_function("tnb_rec_time",
                               CT.c_int,
                               (zutl.AutoString, self.intent_in, 'time',),
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid', ),
                               (CT.c_int, self.intent_in, 'rec',),
                               err=self.errcheck_strict)
        # extern int tnb_rec_date(char * const time,
        #                         const int handle, const int vid,
        #                         const int rec);
        self.register_function("tnb_rec_date",
                               CT.c_int,
                               (zutl.AutoString, self.intent_in, 'date',),
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid', ),
                               (CT.c_int, self.intent_in, 'rec',),
                               err=self.errcheck_strict)
        # extern int tnb_var_read_int(int * const d,
        #                             const size_t rec,
        #                             const size_t *start, const size_t *count,
        #                             const int handle, const int vid);
        self.register_function("tnb_var_read_int",
                               CT.c_int,
                               (CT.POINTER(CT.c_int), self.intent_in, 'd',),
                               (CT.c_size_t, self.intent_in, "rec"),
                               (zutl.AutoArray(CT.c_size_t),
                                self.intent_in, "start"),
                               (zutl.AutoArray(CT.c_size_t),
                                self.intent_in, "count"),
                               (CT.c_int, self.intent_in, 'handle'),
                               (CT.c_int, self.intent_in, 'vid'),
                               err=self.errcheck_strict)
        # extern int tnb_var_read_float(float * const d,
        #                            const size_t rec,
        #                            const size_t *start, const size_t *count,
        #                            const int handle, const int vid);
        self.register_function("tnb_var_read_float",
                               CT.c_int,
                               (CT.POINTER(CT.c_float), self.intent_in, 'd',),
                               (CT.c_size_t, self.intent_in, "rec"),
                               (zutl.AutoArray(CT.c_size_t),
                                self.intent_in, "start"),
                               (zutl.AutoArray(CT.c_size_t),
                                self.intent_in, "count"),
                               (CT.c_int, self.intent_in, 'handle'),
                               (CT.c_int, self.intent_in, 'vid'),
                               err=self.errcheck_strict)
        # extern int tnb_var_read_double(double * const d,
        #                             const size_t rec,
        #                             const size_t *start, const size_t *count,
        #                             const int handle, const int vid);
        self.register_function("tnb_var_read_double",
                               CT.c_int,
                               (CT.POINTER(CT.c_double), self.intent_in, 'd',),
                               (CT.c_size_t, self.intent_in, "rec"),
                               (zutl.AutoArray(CT.c_size_t),
                                self.intent_in, "start"),
                               (zutl.AutoArray(CT.c_size_t),
                                self.intent_in, "count"),
                               (CT.c_int, self.intent_in, 'handle'),
                               (CT.c_int, self.intent_in, 'vid'),
                               err=self.errcheck_strict)

        lev = self._touza.get('lev', 0)
        mode = self._touza.get('mode', 0)

        self.tnb_init(lev, mode)

    def __del__(self, *args, **kwds):
        """Destructor."""
        if self._load:
            lev = self._touza.get('lev', 0)
            mode = self._touza.get('mode', 0)
            self.tnb_finalize(lev, mode)

        super().__del__(*args, **kwds)

    def group_co_range(self, handle:int, cid:int, raw:bool=False):
        """
        Wrap tnb_group_co_range() to return ranges.

        Parameters
        ----------
        handle : int
             Original Nio-handle object used in TOUZA.
        cid : int
             Coordinate id.
        raw : bool
             True to return raw objects, otherwise their values.

        Examples
        --------
        >>> (begin, end) = group_co_range(handle, coord)
        (0, 64)
        """

        jbgn = CT.c_int()
        jend = CT.c_int()
        self.tnb_group_co_range(CT.byref(jbgn), CT.byref(jend), handle, cid)
        if raw:
            return (jbgn, jend, )
        return (jbgn.value, jend.value, )

    def header_list_attrs(self, handle:int,
                          vid: int|None=None,
                          rec: int|None=None,
                          conv: bool|None=None):
        """
        Generate attribute list of a Nio record.

        Parameters
        ----------
        handle : int
             Original Nio-handle object used in TOUZA.
        vid : int
            Variable id.
        rec : int
            Record index.
        conv : bool
            True to decode the return values (default False).

        Examples
        --------
        >>> attrs = header_list_attrs(handle, var, rec)
        """

        if vid is None:
            vid = -1
        if rec is None:
            rec = -1
        la = self.tnb_attr_len(0, handle, vid, rec)
        na = self.tnb_attr_size(handle, vid, rec)
        buf = CT.create_string_buffer(na * la + 1)

        self.tnb_get_header(buf, handle, vid, rec)
        if conv is True:
            buf = buf.value.decode()
        hd = [buf[j:j+la] for j in range(0, la * na, la)]
        return hd

    # pylint: disable=too-many-arguments
    def header_get_attr(self,
                        item: int|str,
                        handle: int,
                        vid: int|None=None,
                        rec: int|None=None,
                        conv: type|None=None):
        """
        Wrapper method of tnb_get_attr().

        Parameters
        ----------
        item : int or str
             Attribute id or keyword.
        handle : int
             Original Nio-handle object used in TOUZA.
        vid : int
             Variable id.
        rec : int
             Record index.
        conv : type
             Type to convert.

        Examples
        --------
        >>> attr = header_get_attr('dfmt', handle, var=vid, rec=0)
        """

        if vid is None:
            vid = -1
        if rec is None:
            rec = -1
        la = self.tnb_attr_len(item, handle, vid, rec)
        attr = CT.create_string_buffer(la + 1)
        if isinstance(item, int):
            self.tnb_get_attr_byid(attr, item, handle, vid, rec)
        else:
            self.tnb_get_attr(attr, item, handle, vid, rec)
        if conv is False:
            return attr
        attr = attr.value.decode()
        try:
            if conv in [int, CT.c_int, ]:
                a = int(float(attr))
            elif conv in [float, CT.c_float, CT.c_double, ]:
                a = float(attr)
            else:
                a = attr
            return a
        except:
            return attr

    def tnb_var_read(self, d,
                     rec: int,
                     start: tuple[int]|list[int],
                     count: tuple[int]|list[int],
                     handle: int, vid: int):
        """
        Switcher to call tnb_var_read() variation according to input type.

        Parameters
        ----------
        d : pointer
            Array to read.
        rec : int
            Record index.
        start : list, tuple
            Slice start index.
        count : list, tuple
            Numboer of elements along slices.
        handle : int
            Original Nio-handle object used in TOUZA.
        vid : int
            Variable id.

        Returns
        -------
        int
            Error-code from libtouza.
        Raises
        ------
        TypeError
           Invalid type of argument d.

        Examples
        --------
        >>> ret = tnb_var_read(pointer, rec, (0, 0), (5, 2), handle, vid)
        """

        for f, t in [(self.tnb_var_read_int, CT.c_int),
                     (self.tnb_var_read_float, CT.c_float),
                     (self.tnb_var_read_double, CT.c_double),]:
            if isinstance(d, CT.POINTER(t)):
                # import traceback
                # locallog.info(f"tnb_var_read[{t}]: {handle}:{vid}[{rec}]"
                #               f"({start[:]} {count[:]})")
                # traceback.print_stack()
                return f(d, rec, start, count, handle, vid)
        raise TypeError

    def register_function(self, *args,
                          err: cabc.Callable|bool|None=False,
                          **kwds):
        """
        Wrapper method to call register_function with default error handler.

        Parameters
        ----------
        *args :
           Pass to parent method.
        err : method
           Error check function.
        **kwds :
           Pass to parent method.
        """

        if err is None:
            pass
        else:
            err = err or self.errcheck_std
        super().register_function(*args, **kwds, err=err)

    @staticmethod
    def errcheck_format(result: int, _func, _args):
        """
        Result checker for format.

        Parameters
        ----------
        result : int
            Error code.
        _func, _args : any
            Dummy.
        """
        return result >= 0

    @staticmethod
    def errcheck_handle(handle: int, _func, _args):
        """
        Result checker for handle.

        Parameters
        ----------
        handle : int
            Error code or handle.
        _func, _args : any
            Dummy.
        """
        if handle >= 0:
            return handle
        raise ValueError

    @staticmethod
    def errcheck_strict(ierr: int, func, args):
        """
        Strict ierr checker.

        Parameters
        ----------
        ierr : int
            Error code.
        func, args : any
            Pass to exception.
        """
        if ierr == 0:
            return ierr
        raise LibTouzaException(ierr, (func, args))

    @staticmethod
    def errcheck_std(ierr: int, func, args):
        """
        Standard ierr checker.

        Parameters
        ----------
        ierr : int
            Error code.
        func, args : any
            Pass to exception.
        """
        if ierr >= 0:
            return ierr
        raise LibTouzaException(ierr, (func, args))

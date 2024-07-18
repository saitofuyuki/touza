#!/usr/bin/env python3
# Time-stamp: <2024/07/24 14:01:17 fuyuki libtouza.py>

__doc__ = \
    """
zbt.libtouza
~~~~~~~~~~~~
Python interface on TOUZA library

:Source:     zbt/libtouza.py
:Maintainer: SAITO Fuyuki <saitofuyuki@jamstec.go.jp>
:Created:    Jul 16 2024
"""

import os
import ctypes as CT

from . import helper


ENV_TOUZA_LIB = 'TOUZA_LIB'
LIBTOUZA = 'libtouza.so'


class LibTouzaException(Exception):
    """Abstract exception for TOUZA library."""


class LibTouzaCore(helper.WrapCDLL):
    """Abstract base class for TOUZA library"""

    def __init__(self, name, *args, touza=None, **kwds):
        """Initialize CDLL after TOUZA."""
        if not name:
            name = os.environ.get(ENV_TOUZA_LIB)
            name = name or LIBTOUZA
        try:
            super().__init__(name, *args, **kwds)
        except OSError:
            print("Cannot load touza library.  "
                  f"Setting {ENV_TOUZA_LIB} environment may help.")
            raise
        self._touza = touza or {}

    def __del__(self, *args, **kwds):
        """Destructor."""
        pass

    # def __str__(self):
    #     raise ValueError

    # def __repr__(self):
    #     raise ValueError


class LibTouzaNio(LibTouzaCore):
    """TOUZA/Nio friendly interfaces."""

    def __init__(self, *args, **kwds):
        """Initialize CDLL after TOUZA."""
        super().__init__(*args, **kwds)

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
                               (helper.AutoString, self.intent_in, 'path',),
                               err=self.errcheck_format)
        # extern int tnb_file_open(const char *path, const int flag);
        self.register_function("tnb_file_open",
                               CT.c_int,
                               (helper.AutoString, self.intent_in, 'path',),
                               (CT.c_int, self.intent_in, 'flag', 0, ),
                               err=self.errcheck_handle)
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
                               (helper.AutoString, self.intent_in, 'name',),
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
                               (helper.AutoString, self.intent_in, 'name',),
                               (CT.c_int, self.intent_in, 'refh',),
                               err=self.errcheck_handle)
        # extern int tnb_group_name(const char *name, const int handle);
        self.register_function("tnb_group_name",
                               CT.c_int,
                               (helper.AutoString, self.intent_in, 'name',),
                               (CT.c_int, self.intent_in, 'handle',),
                               err=self.errcheck_handle)
        # extern int tnb_var_name(const char *name,
        #                         const int handle, const int vid);
        self.register_function("tnb_var_name",
                               CT.c_int,
                               (helper.AutoString, self.intent_in, 'name',),
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid',),
                               err=self.errcheck_strict)
        # extern int tnb_search_var(const int handle, const char *name);
        self.register_function("tnb_search_var",
                               CT.c_int,
                               (CT.c_int, self.intent_in, 'handle',),
                               (helper.AutoString, self.intent_in, 'name',),
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
                               (helper.AutoString, self.intent_in, 'name',),
                               err=self.errcheck_handle)
        # extern int tnb_co_name(char * const name,
        #                        const int handle, const int vid,
        #                        const int cid);
        self.register_function("tnb_co_name",
                               CT.c_int,
                               (helper.AutoString, self.intent_in, 'name',),
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
                               (helper.AutoString,
                                self.intent_in, 'attr'),
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid', -1),
                               (CT.c_int, self.intent_in, 'rec', -1))
        # extern int tnb_get_attr(char * const attr, const char *item,
        #                         const int handle, const int vid,
        #                         const int rec);
        self.register_function("tnb_get_attr",
                               CT.c_int,
                               (helper.AutoString, self.intent_in, 'attr',),
                               (helper.AutoString, self.intent_in, 'item',),
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
                               (helper.AutoString, self.intent_in, 'item',),
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid',),
                               (CT.c_int, self.intent_in, 'rec',),
                               err=self.errcheck_strict)
        # extern int tnb_get_attr_byid(char * const attr, const int item,
        #                              const int handle, const int vid,
        #                              const int rec);
        self.register_function("tnb_get_attr_byid",
                               CT.c_int,
                               (helper.AutoString, self.intent_in, 'attr',),
                               (CT.c_int, self.intent_in, 'item',),
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid',),
                               (CT.c_int, self.intent_in, 'rec',),
                               err=self.errcheck_strict)
        # extern int tnb_get_attr_name(char * const attr, const int item);
        self.register_function("tnb_get_attr_name",
                               CT.c_int,
                               (helper.AutoString, self.intent_in, 'attr',),
                               (CT.c_int, self.intent_in, 'item',),
                               err=self.errcheck_strict)
        # extern int tnb_rec_time(char * const time,
        #                         const int handle, const int vid,
        #                         const int rec);
        self.register_function("tnb_rec_time",
                               CT.c_int,
                               (helper.AutoString, self.intent_in, 'time',),
                               (CT.c_int, self.intent_in, 'handle',),
                               (CT.c_int, self.intent_in, 'vid', ),
                               (CT.c_int, self.intent_in, 'rec',),
                               err=self.errcheck_strict)
        # extern int tnb_rec_date(char * const time,
        #                         const int handle, const int vid,
        #                         const int rec);
        self.register_function("tnb_rec_date",
                               CT.c_int,
                               (helper.AutoString, self.intent_in, 'date',),
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
                               (CT.POINTER(CT.c_size_t),
                                self.intent_in, "start"),
                               (CT.POINTER(CT.c_size_t),
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
                               (CT.POINTER(CT.c_size_t),
                                self.intent_in, "start"),
                               (CT.POINTER(CT.c_size_t),
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
                               (CT.POINTER(CT.c_size_t),
                                self.intent_in, "start"),
                               (CT.POINTER(CT.c_size_t),
                                self.intent_in, "count"),
                               (CT.c_int, self.intent_in, 'handle'),
                               (CT.c_int, self.intent_in, 'vid'),
                               err=self.errcheck_strict)

        lev = self._touza.get('lev', 0)
        mode = self._touza.get('mode', 0)

        self.tnb_init(lev, mode)

    def __del__(self, *args, **kwds):
        """Destructor."""
        lev = self._touza.get('lev', 0)
        mode = self._touza.get('mode', 0)
        self.tnb_finalize(lev, mode)
        super().__del__(*args, **kwds)

    def group_co_range(self, handle, cid, raw=False):
        """Wrap tnb_group_co_range to return ranges."""
        jbgn = CT.c_int()
        jend = CT.c_int()
        self.tnb_group_co_range(CT.byref(jbgn), CT.byref(jend), handle, cid)
        if raw:
            return (jbgn, jend, )
        return (jbgn.value, jend.value, )

    def header_get_attr(self, item, handle, vid=None, rec=None, raw=False):
        """tnb_get_attr wrapper."""
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
        if raw:
            return attr
        return attr.value.decode()

    def tnb_var_read(self, d, rec, start, count, handle, vid):
        """tnb_var_read switcher according to type of d."""
        for f, t in [(self.tnb_var_read_int, CT.c_int),
                     (self.tnb_var_read_float, CT.c_float),
                     (self.tnb_var_read_double, CT.c_double),]:
            if isinstance(d, CT.POINTER(t)):
                return f(d, rec, start, count, handle, vid)
        raise TypeError

    def register_function(self, *args, err=False, **kwds):
        """Wrap register_function with default error handler."""
        if err is None:
            pass
        else:
            err = err or self.errcheck_std
        super().register_function(*args, **kwds, err=err)

    @staticmethod
    def errcheck_format(result, _func, _args):
        """Result checker for format."""
        return result >= 0

    @staticmethod
    def errcheck_handle(handle, _func, _args):
        """Result checker for handle."""
        if handle >= 0:
            return handle
        raise ValueError

    @staticmethod
    def errcheck_strict(ierr, func, args):
        """Strict ierr checker."""
        if ierr == 0:
            return ierr
        raise LibTouzaException(ierr, (func, args))

    @staticmethod
    def errcheck_std(ierr, func, args):
        """Standard ierr checker."""
        if ierr >= 0:
            return ierr
        raise LibTouzaException(ierr, (func, args))

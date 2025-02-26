#!/usr/bin/env python3
# Time-stamp: <2025/02/21 12:44:42 fuyuki dsnio.py>
#
# Copyright (C) 2024, 2025
#           Japan Agency for Marine-Earth Science and Technology
#
# Licensed under the Apache License, Version 2.0
#   (https://www.apache.org/licenses/LICENSE-2.0)

"""
Numpy + TOUZA/Nio extension.

:Source:     zbt/dsnio.py
:Maintainer: SAITO Fuyuki <saitofuyuki@jamstec.go.jp>
:Created:    Jul 16 2024
"""

import sys
import os
import pathlib as plib
import ctypes as CT
import datetime
# import pandas as pd
import warnings
import typing

import numpy
import cftime

# from . import util
from . import util as zutl
from . import libtouza as zlt
from . import param as zpar

locallog = zutl.LocalAdapter(__name__)

LOAD_TOUZA = None

GTAX_PATH = 'GTAX_PATH'

# calendar names in cftime and the epoch years
calendar_epoch = {
    'proleptic_gregorian': 0,
    '360_day': 0,
    'noleap': 0,
    'all_leap': 0,
    'julian': -1,
    'gregorian': -1,
    'standard': -1, }


class _TouzaNio(zpar.ParamTouzaNio):
    """Common procedures among TouzaNio classes."""
    __slots__ = ('sub', 'sep', )

    try:
        lib = zlt.LibTouzaNio(name=LOAD_TOUZA)
    except UserWarning:
        lib = None

    def __init__(self, sub=None, sep=None):
        """
        Bind TOUZA/Nio properties from file.

        Parameters
        ----------
        sub : str
            Separator used when duplicates of the variables.
        sep : str
            Separator to represent group hierarchies.
        """
        self.sub = sub or '~'
        self.sep = sep or '/'

    @classmethod
    def is_nio_file(cls, path: str|os.PathLike) -> bool:
        """
        Check if path is TOUZA/Nio format file.

        Parameters
        ----------
        path : path-like
            File to check if TOUZA/Nio format.

        Returns
        -------
        bool
            True if TOUZA/Nio format.
        """

        path = os.fspath(path)
        if cls.lib:
            return cls.lib.tnb_file_is_nio(path)
        return False

    @classmethod
    def debug(cls):
        """Show debug properties."""
        locallog.debug(f"{cls}: {cls.lib}")

    @classmethod
    def is_loaded(cls):
        """Check if touza library is loaded."""
        return not cls.lib is None


class TouzaNioDimension(_TouzaNio):
    """Dimension (axis) property"""

    __slots__ = ('handle', 'name', 'size', 'extent',
                 'dataset', 'suite', )

    # pylint: disable=too-many-arguments
    def __init__(self,
                 name: str,
                 size: int|None = None,
                 handle: int|None = None,
                 begin: int|None = None,
                 end: int|None = None,
                 group=None,
                 **kwds):
        """
        Constructor.

        Parameters
        ----------
        name : str
            Name of dimension.
        size : int
            Dimension size.
        handle :
            Original Nio-handle object of dimension used in TOUZA.
        begin, end : int
            Dimension range.
        group : TouzaNioDataset
            Dataset which the dimension belongs to.
        **kwds :
            Pass to the parent.

        Raises
        ------
        ValueError
             Raise if invalid argument combination.
        """

        self.handle = handle
        self.name = name
        self.dataset = group
        if (size is None) == ((begin is None) or (end is None)):
            raise ValueError
        if size is not None:
            self.size = size
            self.extent = (0, size, )
        else:
            self.extent = (begin, end, )
            self.size = end - begin

        super().__init__(**kwds)

    def __len__(self):
        """Dimension size"""
        return self.size

    def __str__(self):
        """return str"""
        n = zutl.tostr(self.name)
        p = [f"name = '{n}'",
             f"size = {self.size}", ]
        return f"{repr(type(self))}: " + ', '.join(p)

    def is_record(self) -> bool:
        """Check if record dimension"""
        return self.handle < 0

    @property
    def shape(self):
        """1d dimension shape"""
        return (self.size, )


class _TouzaCoreVar(_TouzaNio):
    """Variable core property."""

    __slots__ = ('handle', 'dataset', 'name', 'dimensions', 'recdim',
                 'dtype', 'logical', )

    def __init__(self,
                 group,
                 name: str|bytes,
                 datatype,
                 dimensions: tuple[TouzaNioDimension],
                 handle: int,
                 recdim: TouzaNioDimension|None = None,
                 **kwds):
        """
        Constructor.

        Parameters
        ----------
        group :
            Group which the variable belongs to.
        name : str|bytes
            Name of variable.
        datatype :
            Object to be converted to a data type object, which is passed to
            numpy.dtype().
        dimensions : tuple[TouzaNioDimension]
            Variable dimensions.
        handle : int
            Original Nio-handle object of dimension used in TOUZA.
        recdim : TouzaNioDimension|None
            Dimension corresonds to the record.
        **kwds :
            Pass to parent.
        """
        self.handle = handle
        self.dataset = group
        self.logical = group    # Default logical group equals to the dataset.
        self.name = name
        self.dimensions = dimensions
        self.dtype = numpy.dtype(datatype)
        self.recdim = recdim
        super().__init__(**kwds)

    def copy(self, logical=None):
        """Simple copy."""
        return self.__copy__(logical)

    def replace_dim(self, dims:list[TouzaNioDimension]):
        """
        Replace dimension element.

        Parameters
        ----------
        dim : List[TouzaNioDimension]
            Target dimension to replace with corresonding self value.

        Raises
        ------
        ValueError
            Raise if not found.
        """
        ndim = []
        for cd in self.dimensions:
            for dd in dims:
                if cd.name == dd.name:
                    ndim.append(dd)
                    break
            else:
                ndim.append(cd)
        self.dimensions = tuple(ndim)
        # for j, d in enumerate(self.dimensions):
        #     if dim.name == d.name:
        #         break
        # else:
        #     raise ValueError(f"Not found dimension {dim.name}.")
        # self.dimensions = self.dimensions[:j] + (dim, ) + self.dimensions[j+1:]
        return self.dimensions

    def squeeze(self):
        """Squeeze along record dimension."""
        # to do: non-record dimension
        if self.recdim:
            if self.recdim.size <= 1:
                j = self.dimensions.index(self.recdim)
                self.dimensions = self.dimensions[:j] + self.dimensions[j+1:]
                self.recdim = None

        self.dimensions = tuple(filter(lambda d: d.size > 1, self.dimensions))

    def __copy__(self, *args, **kwds):
        """Placeholder."""
        raise NotImplementedError

    def getattr(self, item, **kwds):
        """Placeholder."""
        raise NotImplementedError

    def _attrs(self, *args, **kwds):
        """Placeholder."""
        raise NotImplementedError

    def attrs(self, rec=None):
        """Iterator of the attributes."""
        yield from self._attrs(rec=rec)

    @property
    def recidx(self) -> int|None:
        """Record coordinate index or None"""
        if self.recdim:
            return self.dimensions.index(self.recdim)
        return None

    @property
    def shape(self) -> tuple[int]:
        """Array shape"""
        return tuple(d.size for d in self.dimensions)
        # sh = ()
        # for d in self.dimensions:
        #     sh = sh + (d.size, )
        # return sh

    def dimensions_suite(self, group=None) -> tuple:
        """Canonicalized dimension tuple"""
        group = group or self.dataset.root
        return tuple(self.format_dims(group=group))

    @property
    def dimensions_names(self) -> tuple[str]:
        """Normalize dimension names"""
        return tuple(zutl.tostr(d.name) for d in self.dimensions)

    @property
    def dimensions_map(self):
        """Dimension mapping"""
        return dict((d.name, d) for d in self.dimensions)

    def format_name(self, sep=True, group=None):
        """Return canonicalized name under group"""
        if group is None:
            group = True
        if group is True:
            # group = self.dataset
            group = self.logical
        if group is False:
            return self.name
        if sep is True:
            sep = self.sub
        return group.variables.rev_map(self, sep=sep)

    def format_dims(self, sep=True, group=None):
        """Return canonicalized name under group"""
        if group is None:
            group = True
        if group is True:
            group = self.logical
            # group = self.dataset
        if group is False:
            dims = [d.name for d in self.dimensions]
        else:
            if sep is True:
                sep = self.sub
            dims = [group.dimensions.rev_map(d, sep=sep)
                    for d in self.dimensions]
        return dims

    def short(self):
        """return short str"""
        return self._info(attrs=False)

    def __str__(self):
        """return str"""
        return self._info(attrs=True)

    def _info(self, attrs=None) -> str:
        """return str"""
        attrs = True if attrs is None else attrs
        dump = [repr(type(self))]
        tab = ' ' * 4
        vt = zutl.tostr(self.dtype)
        vn = self.format_name()
        ds = ', '.join(self.format_dims())
        dump.append(f"{vt} {vn}({ds})")
        if attrs:
            for a, ai in self.attrs():
                av = self.getattr(a)
                if isinstance(av, str):
                    av = av.strip()
                ai = zutl.tostr(ai)
                if av:
                    dump.append(f"{tab}{ai}: {av}")
        dump.append(f"shape = {self.shape}")
        return '\n'.join(dump)

    def get_amap(self, rec=None, conv=None, uniq=False, **kwds):
        """Dummy procedure to overload."""
        return {}


class TouzaMemVar(_TouzaCoreVar):
    """Variable property"""

    __slots__ = ('array', '_mattrs')

    def __init__(self, *args, array=None, **kwds):
        """Constructor."""
        super().__init__(*args, **kwds)
        self._mattrs = {}
        if array is not None:
            self.array = numpy.asarray(array)
        else:
            self.array = None

    def __copy__(self, logical=None):
        """Create copy object."""
        obj = self.__class__(self.dataset, self.name,
                             self.dtype, self.dimensions, self.handle,
                             self.recdim, array=self.array)
        if logical:
            obj.logical = logical
        return obj

    def __getitem__(self, elem):
        return self.array

    def getattr(self, item, **kwds):
        """Get attribute corresponding to item (number or name)."""
        return self._mattrs.get(item, '')

    def setattr(self, item, val):
        """Set attribute."""
        self._mattrs[item] = val
        return self._mattrs[item]

    def _attrs(self, **kwds):
        """Iterator of attributes in Nio variable"""
        yield from enumerate(self._mattrs)


class TouzaNioVar(_TouzaCoreVar):
    """Variable property"""

    def __init__(self, *args, **kwds):
        """Constructor."""
        super().__init__(*args, **kwds)
        self._internal_attrs = {}

    def __copy__(self, logical=None):
        """Create copy object."""
        obj = self.__class__(self.dataset, self.name,
                             self.dtype, self.dimensions, self.handle,
                             self.recdim)
        if logical:
            obj.logical = logical
        return obj

    def __getitem__(self, elem):
        return self._getitem_(elem)

    def _getitem_(self, elem):
        """__getitem__() core to call original function."""
        elem = zutl.Selection(elem, self.dimensions)
        start = ()
        count = ()
        shape = ()
        # slicing follows numpy design:
        #    An integer, i, returns the same values as i:i+1
        #    except the dimensionality of the returned object is reduced by 1.
        for e, d in zip(elem, self.dimensions):
            o = d.extent[0]
            if isinstance(e, slice):
                if e.step is not None and e.step != 1:
                    raise ValueError("Only value 1 "
                                     f"is allowed for slice step {e}.")
                st, en, _ = e.indices(d.size)
            else:
                st = e
                if st < 0:
                    st = d.size + st
                en = st + 1
            co = en - st
            start = start + (st + o, )
            count = count + (co, )
            if isinstance(e, slice):
                shape = shape + (co, )
        if self.recdim:
            jrec = self.dimensions.index(self.recdim)
            recs = (start[jrec], count[jrec])
            start = start[:jrec] + start[jrec+1:]
            count = count[:jrec] + count[jrec+1:]
        else:
            recs = (0, 1)
        unit = numpy.prod(count)
        full = unit * recs[1]
        # size_t converstion must be later
        start = (CT.c_size_t * len(start))(*start)
        count = (CT.c_size_t * len(count))(*count)
        ds = self.dataset
        ct = numpy.ctypeslib.as_ctypes_type(self.dtype)

        # check if miss is identical among records
        miss = ds.lib.header_get_attr('miss',
                                      ds.handle, vid=self.handle, rec=recs[0],
                                      conv=ct)
        if recs[1] > 1:
            for j in range(recs[1]):
                r = recs[0] + j
                mchk = ds.lib.header_get_attr('miss',
                                              ds.handle, vid=self.handle,
                                              rec=r, conv=ct)
                if mchk != miss:
                    sys.stderr.write("Inconsistent missing value"
                                     f" at record {r}"
                                     f" ({miss} vs {mchk})\n")
                    raise ValueError

        if isinstance(miss, str):
            if miss.strip() == '':
                miss = None
        cbuf = (ct * full)()
        step = CT.sizeof(ct) * unit
        for j in range(recs[1]):
            r = recs[0] + j
            p = CT.byref(cbuf, step * j)
            p = CT.cast(p, CT.POINTER(ct))
            # print(f"read {self.handle} {r} {start} {count}")
            locallog.debug(f"read <{ds.handle}/{self.handle}>"
                           f"[{r}] {start[:]} {count[:]}")
            ds.lib.tnb_var_read(p, r, start, count,
                                ds.handle, self.handle)
        buf = numpy.ctypeslib.as_array(cbuf)
        if miss:
            buf = numpy.ma.masked_equal(buf, miss)
        buf = buf.reshape(shape)
        return buf

    def attr_map(self, rec=None, cls=None, conv=None, **kwds):
        ds = self.dataset
        rec = rec or 0
        hd = ds.lib.header_list_attrs(ds.handle, self.handle, rec=rec,
                                      conv=conv)
        cls = cls or dict
        if issubclass(cls, list):
            return cls(hd)
        if issubclass(cls, tuple):
            return cls(hd)
        if issubclass(cls, dict):
            ret = {}
            la = ds.lib.tnb_attr_len(' ', ds.handle, self.handle)
            for a in range(ds.lib.tnb_attr_size(ds.handle, self.handle, rec)):
                a = a + 1
                item = CT.create_string_buffer(la + 1)
                ds.lib.tnb_get_attr_name(item, a)
                k = item.value
                if conv:
                    k = k.decode()
                ret[k] = hd[a-1]
            return ret
        return hd

    def get_amap(self, rec=None, conv=None, uniq=False, **kwds):
        if isinstance(rec, slice):
            if self.recdim:
                r = rec.indices(self.recdim.size)
                rec = range(r[0], r[1])
            else:
                rec = [None]
        elif numpy.iterable(rec):
            pass
        else:
            rec = [rec, ]
        res = {}
        for r in rec:
            buf = self.attr_map(r, dict, conv=conv)
            for k, v in buf.items():
                if k in res:
                    res[k].append(v)
                else:
                    res[k] = [v]
        if uniq:
            for k, v in res.items():
                if len(set(v)) == 1:
                    res[k] = v[0]
        return res

    def getattr(self, item, rec=None, conv=None, uniq=False, **kwds):
        """Get attribute corresponding to item (number or name)."""
        if item in self._internal_attrs:
            return self._internal_attrs[item]

        ds = self.dataset
        rec = rec or 0
        if isinstance(rec, slice):
            if self.recdim:
                r = rec.indices(self.recdim.size)
                rec = range(r[0], r[1])
            else:
                rec = [None]
            res = [ds.lib.header_get_attr(item, ds.handle, self.handle,
                                          rec=r, conv=conv)
                   for r in rec]
            if uniq:
                if len(set(res)) == 1:
                    return res[0]
            return tuple(res)
        elif numpy.iterable(rec):
            res = [ds.lib.header_get_attr(item, ds.handle, self.handle,
                                          rec=r, conv=conv)
                   for r in rec]
            if uniq:
                if len(set(res)) == 1:
                    return res[0]
            return type(rec)(res)
        else:
            return ds.lib.header_get_attr(item, ds.handle, self.handle,
                                          rec=rec, conv=conv)

    def setattr(self, item, val):
        """Set attribute."""
        self._internal_attrs[item] = val

    def _attrs(self, rec=None):
        """Iterator of attributes in Nio variable"""
        ds = self.dataset
        la = ds.lib.tnb_attr_len(' ', ds.handle, self.handle)
        rec = rec or 0
        for a in range(ds.lib.tnb_attr_size(ds.handle, self.handle, rec)):
            a = a + 1
            item = CT.create_string_buffer(la + 1)
            ds.lib.tnb_get_attr_name(item, a)
            yield (a, item.value)
        for a in self._internal_attrs.keys():
            yield (a, a)


# pylint: disable=too-many-ancestors
# pylint: disable=too-many-instance-attributes
class TouzaNioDataset(_TouzaNio):
    """Abstract layer of xarray.Dataset for TOUZA/Nio."""

    __slots__ = ('handle', 'root', 'parent', 'name',
                 'groups', 'variables',
                 'dimensions',
                 'record', 'recdim', 'cls_var', 'cls_arr',)

    def __init__(self,
                 filename: str|os.PathLike,
                 record=None,
                 flatten: bool=True,
                 cls_var: type[_TouzaCoreVar]|None=None,
                 cls_arr: type[_TouzaCoreVar]|None=None,
                 **kwds):
        """
        Bind TOUZA/Nio properties from file.

        Parameters
        ----------
        filename : PathLike
            File to open as TOUZA/Nio format.
        record : Any
            Dummy.
        flatten : bool
            Whether or not to flatten the group hierarchies.
        cls_var : _TouzaCoreVar
            Class of variable.
        cls_arr : _TouzaCoreVar
            Class of array.
        **kwds :
            Pass to the parent.

        Raises
        ------
        ValueError
            Raise if filename is not TOUZA/Nio format.
        """
        if not self.lib.tnb_file_is_nio(filename):
            raise ValueError(f"{filename} is not nio/gtool format.")

        flag = self.lib.NIO_CACHE_COLL_DEFAULT
        if flatten:
            flag = (flag
                    | self.lib.NIO_CACHE_ALLOW_VAR_DUP
                    | self.lib.NIO_CACHE_ALLOW_GRP_DUP
                    | self.lib.NIO_CACHE_ALLOW_COOR_DUP)

        super().__init__(**kwds)

        handle = self.lib.tnb_file_open(filename, flag=flag)
        self.assign_nio(handle, filename, self, None, None,
                        flatten, cls_var, cls_arr)

    def assign_nio(self,
                   handle: int,
                   filename: str|os.PathLike,
                   root: typing.Self|None = None,
                   parent: typing.Self|None = None,
                   record=None,
                   flatten: bool = True,
                   cls_var: type[_TouzaCoreVar]|None = None,
                   cls_arr: type[_TouzaCoreVar]|None = None):
        """
        Bind TOUZA/Nio properties from cache handle.

        Parameters
        ----------
        handle : int
            Original Nio-handle object used in TOUZA.
        filename : str|os.PathLike
            File to open as TOUZA/Nio format.
        root : Self
            Root dataset instance.
        parent : Self
            Parent dataset instance.
        record : Any
            Dummy.
        flatten : bool
            Whether or not to flatten the group hierarchies.
        cls_var : _TouzaCoreVar
            Class of variable.
        cls_arr : _TouzaCoreVar
            Class of array.
        """

        _DataSets[handle] = self

        self.handle = handle
        self.name = filename
        self.root = root
        self.parent = parent
        self.cls_var = cls_var or TouzaNioVar
        self.cls_arr = cls_arr or TouzaMemVar
        self.record = record or 'record'  # name of record dimension

        self.recdim = None

        self.groups = zutl.NameMap()
        self.dimensions = zutl.NameMap()
        # add dimensions first to share among groups
        self._add_dimensions()
        self._add_groups()
        self.variables = zutl.NameMap()
        self._flatten_suites(flatten)

    def close(self):
        """Close file to free io unit."""
        self.lib.tnb_file_close(self.handle)
        self.handle = -1

    def __del__(self, *args, **kwds):
        """Destructor."""
        if self.handle >= 0 and self.root is None:
            self.close()

    # pylint: disable=invalid-name
    def createGroup(self, groupname,
                    handle=None):
        """Construct group instance."""
        return TouzaNioGroup(groupname, parent=self,
                             handle=handle)

    # pylint: disable=invalid-name
    def createVariable(self, varname, datatype, dimensions,
                       handle=None, recdim=None, cls=None, **kwds):
        """Construct variable instance."""
        cls = cls or self.cls_var
        return cls(self,
                   varname, datatype, dimensions,
                   handle=handle, recdim=recdim, **kwds)

    def createArray(self, varname, datatype, dimensions,
                    handle=None, recdim=None, cls=None, **kwds):
        """Construct variable instance."""
        cls = cls or self.cls_arr
        return cls(self,
                   varname, datatype, dimensions,
                   handle=handle, recdim=recdim, **kwds)

    # pylint: disable=invalid-name,too-many-arguments
    def createDimension(self, dimname, size=None,
                        handle=None, begin=None, end=None,
                        group=None):
        """Construct variable instance."""
        group = group or self.root
        return TouzaNioDimension(dimname, size,
                                 handle=handle, begin=begin, end=end,
                                 group=group)

    def search_dim(self, ser):
        """Search parent corresponding to serial"""
        for d in self.dimensions.values():
            if d.handle == ser:
                return d
        return None

    def datatype_from_dfmt(self, fmt):
        """NIO/Gtool format to numpy datatype conversion."""
        fmt = fmt.strip().lower()
        if fmt[1:3] == 'i4':
            return 'i4'
        if fmt[1:3] == 'r4':
            return 'f4'
        return 'f8'

    def attrs(self):
        """Iterator of the attributes."""
        yield from self._attrs()

    def getattr(self, item, conv=None, **kwds):
        """Get attribute corresponding to item (number or name)."""
        return self.lib.header_get_attr(item, self.handle, conv=conv)

    def _add_groups(self):
        """Bind NIO groups in NIO object"""
        for gh, gname in self._groups():
            grp = self.createGroup(gname, gh)
            gk = zutl.tostr(gname)
            self.groups[gk] = grp
        return self.groups

    def _add_dimensions(self):
        nr = self._recs()
        if nr:
            cname = self.record
            self.recdim = self.createDimension(cname, size=nr, handle=-1,
                                               group=self)
            ck = zutl.tostr(cname)
            self.dimensions[ck] = self.recdim
        for ch, cname in self._dimensions():
            (begin, end) = self.lib.group_co_range(self.handle, ch)
            dim = None
            if self.parent:
                cs = self.lib.tnb_group_co_idx(self.handle, ch)
                dim = self.parent.search_dim(cs)
            if not dim:
                dim = self.createDimension(cname, begin=begin, end=end,
                                           handle=ch)
            ck = zutl.tostr(cname)
            self.dimensions[ck] = dim
        return self.dimensions

    def _flatten_suites(self, flatten):
        if flatten:
            for g in self.groups.values():
                for v in g.variables.values():
                    vk = zutl.tostr(v.name)
                    self.variables[vk] = v
                for d in g.dimensions.values():
                    if d not in self.dimensions.values():
                        dk = zutl.tostr(d.name)
                        self.dimensions[dk] = d

        return self

    def _add_variables(self, recdim=None):
        for vh, vname in self._vars():
            dims = []
            if recdim:
                dims.append(recdim)
            for cj, ch, cname in self._coors(vh):
                dims.append(self.search_dim(ch))
            attr = self.lib.header_get_attr('dfmt', self.handle, vid=vh, rec=0)
            dfmt = self.datatype_from_dfmt(attr)
            var = self.createVariable(vname, dfmt, tuple(dims),
                                      handle=vh, recdim=recdim)
            vk = zutl.tostr(vname)
            self.variables[vk] = var
        return self.variables

    def _groups(self):
        """Iterator of group in Nio file."""
        for g in range(self.lib.tnb_file_groups(self.handle)):
            gh = self.lib.tnb_group(self.handle, g)
            gname = CT.create_string_buffer(20)
            self.lib.tnb_group_name(gname, gh)
            yield (gh, gname.value)

    def _dimensions(self):
        """Iterator of dimension in Nio file or group."""
        for ch in range(self.lib.tnb_group_coors(self.handle)):
            cname = CT.create_string_buffer(20)
            self.lib.tnb_group_co_name(cname, self.handle, ch)
            yield (ch, cname.value)

    def _vars(self):
        """Iterator of variable in Nio file or group"""
        for v in range(self.lib.tnb_group_vars(self.handle)):
            vname = CT.create_string_buffer(20)
            self.lib.tnb_var_name(vname, self.handle, v)
            yield (v, vname.value)

    def _attrs(self):
        """Dummy procedure to return 0 for number of attributes"""
        yield from ()

    def _coors(self, vh):
        """Iterator of coordinate in variable"""
        for c in range(self.lib.tnb_co_size(self.handle, vh)):
            cname = CT.create_string_buffer(20)
            ch = self.lib.tnb_co_serial(self.handle, vh, c)
            self.lib.tnb_co_name(cname, self.handle, vh, c)
            yield (c, ch, cname.value)

    def _recs(self):
        """Dummy procedure to return 0 for number of records"""
        return 0

    def get(self, elem, default=None, sep=None, family=False):
        """Recursive search of group/variable."""
        sep = self.sep
        if isinstance(elem, str):
            path = elem.split(sep)
        elif numpy.iterable(elem):
            path = tuple(elem)
        else:
            raise ValueError

        g = self
        while path:
            if path[0] in g.groups:
                g = g.groups.get(path[0])
                path = path[1:]
            else:
                break
        if path:
            v = sep.join(path)
            fam = g.variables.get_family(v, [])
            if family:
                return fam or default
            if len(fam) == 1:
                return fam[0]
            return default
        return g

    def __getitem__(self, elem, sep=None):
        r = self.get(elem, sep=sep)
        if r is None:
            raise IndexError(f"{elem} not found in {self}")
        return r

    def diag(self):
        """Call diagnose function in the library"""
        self.lib.tnb_file_diag(self.handle)

    def short(self):
        """Retrun short str version."""
        dump = []
        return dump

    def __str__(self):
        """Return str version."""
        return self._info(attrs=True)

    def short(self):
        """Return short str version."""
        return self._info(attrs=False)

    def _info(self, attrs=None, lev=None):
        """Return str version."""
        attrs = True if attrs is None else attrs
        dump = [repr(type(self))]
        tab = ' ' * 4
        if self.parent is None:
            dump.append("root(suite) group")
        else:
            dump.append(f"group: {self.name}")
        if attrs:
            for a, ai in self.attrs():
                av = self.getattr(a).strip()
                ai = zutl.tostr(ai)
                if av:
                    dump.append(f"{tab}{ai}: {av}")

        ds = tuple(f"{self.dimensions.format_key(dk, sep=self.sub)}"
                   f"({len(dd)})"
                   for dk, dd in self.dimensions.items())

        vs = tuple(f"{str(vv.dtype)}"
                   f" {self.variables.format_key(vk, sep=self.sub)}"
                   + "(" + ','.join(vv.format_dims(sep=self.sub,
                                                   group=self.root)) + ")"
                   for vk, vv in self.variables.items())

        gs = tuple(self.groups.format_key_all(sep=self.sub))

        dump.append(f"    dimensions(sizes): {', '.join(ds)}")
        dump.append(f"    variables(dimensions): {', '.join(vs)}")
        dump.append(f"    groups: {', '.join(gs)}")

        return '\n'.join(dump)


class TouzaNioGroup(TouzaNioDataset):
    """Emulate a hierarchical namespace of dataset."""

    # pylint: disable=super-init-not-called
    def __init__(self, name, parent, handle):
        """Constructor."""
        self.handle = handle
        self.parent = parent
        self.root = parent.root
        self.name = name

        self.record = parent.record
        self.sub = parent.sub
        self.cls_var = parent.cls_var
        self.cls_arr = parent.cls_arr

        self.groups = zutl.NameMap()
        self.dimensions = zutl.NameMap()

        self._add_groups()
        self._add_dimensions()

        recdim = self.dimensions.get(self.record)
        self.variables = zutl.NameMap()
        self._add_variables(recdim)

    def _attrs(self, vid=None, rec=None):
        """Iterator of attributes in Nio file or group"""
        la = self.lib.tnb_attr_len(' ', self.handle, -1)
        for a in range(self.lib.tnb_attr_size(self.handle, -1, -1)):
            a = a + 1
            item = CT.create_string_buffer(la + 1)
            self.lib.tnb_get_attr_name(item, a)
            yield (a, item.value)

    def _groups(self):
        """Iterator of group in Nio group (empty list)."""
        yield from ()

    def _recs(self):
        """Return number of records in Nio group."""
        return self.lib.tnb_group_recs(self.handle)


class TouzaNioCoDataset(TouzaNioDataset):
    """TOUZA/Nio dataset accompanied by coordinate variables."""

    _paths = os.environ.get(GTAX_PATH, '.:')
    _paths = _paths.split(':')

    _embedded = ''

    attr_cyclic = 'cyclic_coordinate'

    #  Null element corresponding to internal field.
    #  If the environemnt does not contain null element,
    #  then null element is inserted at the first candidate.
    #  Therefore, if you need to set precedence of external files than
    #  the internal, then you must set explicitly the null element
    #  somewhere in the environment, as 'dir-a:dire-b::dir-c:dir-d'.
    #  Default candidate is ':.' (internal, then current directory).
    #  If you want to exclude the coordinate binding, you should
    #  not use this class, but use TouzaNioDataset instead.

    def __init__(self, *args, calendar=None, **kwds):
        self._calendar = calendar
        super().__init__(*args, **kwds)
        self._init_(**kwds)

    def _init_(self, co_int=None, **kwds):
        """Internal init procedures."""
        # whether to introduce integer-array coordinate
        if co_int is None:
            co_int = True
        self.co_int = co_int
        self.shift_variables()
        self.bind_coordinates()

    def createGroup(self, groupname,
                    handle=None):
        """Construct group instance."""
        return TouzaNioCoGroup(groupname, parent=self,
                               calendar=self._calendar,
                               handle=handle)

    def shift_variables(self):
        """Shift variables to reserve spaces for coordinates."""
        for pfx in self.dimensions.prefix_keys():
            shift = len(self.dimensions.list_family(pfx))
            if shift > 0:
                if (pfx, 0) in self.variables:
                    self.variables.insert(pfx, shift)
                    # append dummy dimensions to force tuple-style key
                    self.dimensions[pfx] = None

    def bind_coordinates(self):
        """Bind coordinates as additional variables"""
        paths = self.coordinate_paths()
        dims = list(self.dimensions.values())

        for d in dims:
            if d.is_record():
                self.bind_time_coordinate(d)
                continue
            c = self.get_coordinate(d, dims, paths, kind='loc')
            if c:
                okey = self.dimensions.rev_map(d)
                locallog.debug(f"found {okey}, {c.name}")
                if okey in self.variables:
                    raise ValueError(f"Panic.  {okey} {self.variables}")
                self.variables[okey] = c

    def bind_time_coordinate(self, d):
        """Special procedure to bind time-like coordinate."""
        time = []
        date = []
        cals = self._calendar
        if cals in [True, None, []]:
            cals = None
        elif isinstance(cals, str):
            cals = [cals]
        for rec in range(*d.extent):
            t = self.lib.header_get_attr('time',
                                         handle=d.dataset.handle,
                                         vid=0,
                                         rec=rec)
            try:
                t = int(t)
            except ValueError:
                time = list(range(*d.extent))
                date = None
                break
            time.append(t)
            if cals is False:
                pass
            else:
                dt = self.lib.header_get_attr('date',
                                              handle=d.dataset.handle,
                                              vid=0, rec=rec)
                ut = self.lib.header_get_attr('utim',
                                              handle=d.dataset.handle,
                                              vid=0, rec=rec)
                cd = auto_calendar(ut, t, dt, checks=cals)
                cals = cd.keys()
                date.append(cd)

        locallog.debug(f"detected calendar(s): {cals}")
        # print(cals)
        # for t in zip(time, date):
        #     print(t)
        if cals is False:
            date = None
        else:
            if len(cals or []) == 0:
                locallog.warning("failed to detect calendar.")
                date = None

        if date is None:
            c = self.createArray(d.name, 'f8', (d, ), -1, None,
                                 array=time)
            ut = self.lib.header_get_attr('utim',
                                          handle=d.dataset.handle,
                                          vid=0, rec=0)
            c.setattr('UNIT', ut)
        elif cals:
            cals = list(cals)
            cal = cals[0]
            time = [dt[cal] for dt in date]
            # print(time)
            tt = type(time[0])
            # tt = time[0]
            c = self.createArray(d.name, tt, (d, ), -1, None,
                                 array=time)
        c.setattr('TITL1', 'time')

        okey = self.dimensions.rev_map(d)
        self.variables[okey] = c

    @classmethod
    def set_paths(cls, paths):
        cls._paths = paths or []

    @classmethod
    def get_paths(cls):
        return cls._paths

    def coordinate_paths(self, paths=None):
        """Parse axis file paths"""
        # env = env or self.gtax_env
        # paths = os.environ.get(env, '.:')
        # paths = paths.split(':')
        if paths in [True, None, ]:
            paths = self._paths
        elif paths is False:
            paths = []

        if self._embedded not in paths:
            paths.insert(0, self._embedded)
        return paths

    def get_coordinate(self, dtgt, dims, paths, kind=None):
        """Get physical coordinate Nio-handle."""
        # debug = locallog.info
        debug = locallog.debug
        dexts = {d.name:d.extent for d in dims}

        item = zutl.tostr(dtgt.name)
        kind = kind or 'loc'
        if kind == 'loc':
            pfx = 'GTAXLOC.'
            xgrp = ['AXLOC', 'CAXLOC']
        elif kind == 'wgt':
            pfx = 'GTAXWGT.'
            xgrp = ['AXWGT', 'CAXWGT']
        else:
            raise ValueError(f"Unknown coordinate kind {kind}")
        for p in paths:
            if p == self._embedded:
                c = self.check_coordinate_embedded(xgrp, item)
            else:
                c = self.check_coordinate_external(xgrp, item, pfx, p)
            if not c:
                continue
            debug(f"{c.name=} {c.dimensions} {c.shape} {p}")
            for cc in c.dimensions:
                # print(f"{cc.name=} {dsizes}")
                if cc.size > 1:
                    if cc.name not in dexts:
                        debug(f"{cc.size=} {cc.name=}")
                        break
                    dx = dexts[cc.name]
                    if dx[0] < cc.extent[0] or dx[1] > cc.extent[1]:
                        debug(f"{dx=} {cc.extent=}")
                        break
            else:
                # print(f"Match: {c.dimensions} {dexts}")
                c = c.copy(logical=self)
                c.squeeze()
                c.replace_dim(dims)
                return c

            for cc in c.dimensions:
                ## check first coordinate with the same name
                # print(cc.name, cc.extent, cc.size, dtgt.name, dtgt.extent)
                if cc.name == dtgt.name:
                    if dtgt.extent[0] < cc.extent[0] \
                       or dtgt.extent[1] > cc.extent[1]:
                        break
                elif cc.size > 1:
                    ## break if the first coordinate is non scalar.
                    break
            else:
                if True:
                    c = c.copy(logical=self)
                    c.squeeze()
                if c.getattr('DSET', '').startswith('C'):
                    cyclic = (c.shape[0],
                              c._getitem_(0).item(), c._getitem_(-1).item())
                    c.setattr(self.attr_cyclic, cyclic)
                c.replace_dim(dims)
                return c

        if self.co_int:
            c = self.createArray(dtgt.name, 'f8', (dtgt, ), -1, None,
                                 array=list(range(dtgt.size)))
            return c
        return None

    def check_coordinate_embedded(self, grps, item):
        for gn in grps:
            for g in self.groups.get(gn, single=False):
                for cv in g.variables.get(item, single=False):
                    if cv:
                        return cv
        return None

    def check_coordinate_external(self, grps, item, pfx, path):
        """Get variable object corresponding to coordinate."""
        p = plib.Path(path)
        if p.is_dir():
            p = p / (pfx + item)
        if p.is_file():
            h = self.lib.tnb_file_is_opened(str(p))
            if h >= 0:
                ds = _DataSets.get(h)
            else:
                ds = TouzaNioDataset(str(p), cls_var=self.cls_var)
                # ds = TouzaNioDataset(str(p))
            if ds:
                if isinstance(grps, str):
                    grps = [grps]
                for g in grps:
                    if self.is_eta_coordinate(item):
                        cc = ds.get((g, item,), family=True)
                        if cc:
                            return cc[0]
                    else:
                        c = ds.get((g, item,))
                        if c is not None:
                            return c
        return None

    def is_eta_coordinate(self, item):
        item = item.lower()
        return item[1:].startswith('eta')

    def search_cofile(self, base, paths):
        """Search gtool axis file in paths"""
        for p in paths:
            if not p:
                continue
            p = plib.Path(p)
            if p.is_dir():
                p = p / base
                return TouzaNioDataset(str(p))
            if p.is_file():
                pass
        return None


class TouzaNioCoGroup(TouzaNioCoDataset, TouzaNioGroup):
    """TOUZA/Nio group accompanied by coordinate variables."""

    def bind_coordinates(self):
        """Bind coordinates inheriting parent."""


def auto_calendar(unit, time, date, checks=None, first=None):
    """Convert time to cftime.datetime object.
    Try calendar types in checks (or default table if None),
    and generate a dict of calendar:datetime if the conversion
    corresponds to date string.
    """

    cd = {}
    if checks is None:
        checks = calendar_epoch.keys()
    if checks is False or not date:
        return cd

    dt = date.strip()
    dt = dt.split(' ')
    if len(dt) == 1:
        dt = dt[:-6], dt[-6:]
    if len(dt) == 2:
        dt = (dt[0][:-4], dt[0][-4:-2], dt[0][-2:],
              dt[1][:-4], dt[1][-4:-2], dt[1][-2:])
    else:
        raise UserWarning(f"cannot parse {date} as date.")
    try:
        dt = tuple(int(d) if d else 0 for d in dt)
    except ValueError as err:
        raise UserWarning(f"cannot parse {date} as date.") from err

    first = False if first is None else bool(first)

    if not locallog.is_debug():
        warnings.filterwarnings("ignore", category=cftime.CFWarning)

    for c in checks:
        e = calendar_epoch[c]
        u = f"{unit} since {e}-1-1"
        cft = cftime.num2date(time, units=u, calendar=c)
        ref = cftime.datetime(*dt, calendar=c)
        # print(c, cft == ref, cft, ref)
        if cft == ref:
            if first:
                return c
            cd[c] = cft
    return cd

# handle to DataSet mapping
if sys.version_info[:2] > (3, 9):
    _DataSets: dict[int, TouzaNioDataset] = {}
else:
    _DataSets = {}


def diag_datasets():
    """Show registered dataset table for debug."""
    for h, ds in _DataSets.items():
        print(f"handle: {h}")
        print(ds)
        for gn, gg in ds.groups.items():
            print(f"group: {gn}")
            print(gg.short())
        print()

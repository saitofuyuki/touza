#!/usr/bin/env python3
# Time-stamp <2024/07/16 13:09:06 fuyuki touza.py>

__doc__ = \
    """
zbt.dsnio
~~~~~~~~~
numpy + TOUZA/Nio extension

:Source:     zbt/dsnio.py
:Maintainer: SAITO Fuyuki <saitofuyuki@jamstec.go.jp>
:Created:    Jul 16 2024
"""

import sys
import os
import pathlib as plib
import ctypes as CT
import numpy

from . import libtouza
from . import util
from . import param


def diag_datasets():
    """Show registered dataset table for debug."""
    for h, ds in _DataSets.items():
        print(f"handle: {h}")
        print(ds)
        print()


class _TouzaNio(param.ParamTouzaNio):
    """Common procedures among TouzaNio classes."""
    __slots__ = ('sub', 'sep', )

    lib = libtouza.LibTouzaNio(name=None)

    def __init__(self, sub=None, sep=None):
        """Bind TOUZA/Nio properties from file."""
        self.sub = sub or '~'
        self.sep = sep or '/'

    @classmethod
    def is_nio_file(cls, path):
        """Check if path is TOUZA/Nio format file."""
        if isinstance(path, str):
            return cls.lib.tnb_file_is_nio(path)
        return False


# pylint: disable=too-many-ancestors
# pylint: disable=too-many-instance-attributes
class TouzaNioDataset(_TouzaNio):
    """Abstract layer of xarray.Dataset for TOUZA/Nio."""

    __slots__ = ('handle', 'root', 'parent', 'name',
                 'groups', 'variables',
                 'dimensions',
                 'record', 'recdim', 'cls_var')

    def __init__(self, filename,
                 record=None,
                 flatten=True, cls_var=None, **kwds):
        """Bind TOUZA/Nio properties from file."""

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
        self.assign_nio(handle, filename, self, None, None, flatten, cls_var)

    def assign_nio(self, handle, filename,
                   root=None, parent=None, record=None, flatten=True,
                   cls_var=None):
        """Bind TOUZA/Nio properties from cache handle"""

        _DataSets[handle] = self

        self.handle = handle
        self.name = filename
        self.root = root
        self.parent = parent
        self.cls_var = cls_var or TouzaNioVar
        self.record = record or 'record'  # name of record dimension

        self.recdim = None

        self.groups = util.NameMap()
        self.dimensions = util.NameMap()
        # add dimensions first to share among groups
        self._add_dimensions()
        self._add_groups()
        self.variables = util.NameMap()
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
                       handle=None, recdim=None):
        """Construct variable instance."""
        return self.cls_var(self,
                            varname, datatype, dimensions,
                            handle=handle, recdim=recdim)

    # pylint: disable=invalid-name,too-many-arguments
    def createDimension(self, dimname, size=None,
                        handle=None, begin=None, end=None):
        """Construct variable instance."""
        return TouzaNioDimension(dimname, size,
                                 handle=handle, begin=begin, end=end,
                                 group=self.root)

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

    def getattr(self, item, conv=None):
        """Get attribute corresponding to item (number or name)."""
        return self.lib.header_get_attr(item, self.handle, conv=conv)

    def _add_groups(self):
        """Bind NIO groups in NIO object"""
        for gh, gname in self._groups():
            grp = self.createGroup(gname, gh)
            gk = util.tostr(gname)
            self.groups[gk] = grp
        return self.groups

    def _add_dimensions(self):
        nr = self._recs()
        if nr:
            cname = self.record
            self.recdim = self.createDimension(cname, size=nr, handle=-1)
            ck = util.tostr(cname)
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
            ck = util.tostr(cname)
            self.dimensions[ck] = dim
        return self.dimensions

    def _flatten_suites(self, flatten):
        if flatten:
            for g in self.groups.values():
                for v in g.variables.values():
                    vk = util.tostr(v.name)
                    self.variables[vk] = v
                for d in g.dimensions.values():
                    if d not in self.dimensions.values():
                        dk = util.tostr(d.name)
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
            vk = util.tostr(vname)
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

    def get(self, elem, default=None, sep=None):
        """Recursive search of group/variable."""
        sep = self.sep
        if isinstance(elem, str):
            path = sep.split(elem)
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
            if v in g.variables:
                return g.variables[v]
            return default
        else:
            return g

    def __getitem__(self, elem, sep=None):
        r = self.get(elem, sep=sep)
        if r is None:
            raise IndexError(f"{elem} not found in {self}")
        return r

    def diag(self):
        """Call diagnose function in the library"""
        self.lib.tnb_file_diag(self.handle)

    def __str__(self):
        """Return str version."""
        dump = [repr(type(self))]
        tab = ' ' * 4
        if self.parent is None:
            dump.append("root(suite) group")
        else:
            dump.append(f"group: {self.name}")
        for a, ai in self.attrs():
            av = self.getattr(a).strip()
            ai = util.tostr(ai)
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

        self.groups = util.NameMap()
        self.dimensions = util.NameMap()

        self._add_groups()
        self._add_dimensions()

        recdim = self.dimensions.get(self.record)
        self.variables = util.NameMap()
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


class TouzaNioVar(_TouzaNio):
    """Variable property"""

    __slots__ = ('handle', 'dataset', 'name', 'dimensions', 'recdim',
                 'dtype', 'logical', )

    def __init__(self, group, name, datatype, dimensions,
                 handle, recdim=None, **kwds):
        """Constructor."""
        self.handle = handle
        self.dataset = group
        self.logical = group
        self.name = name
        self.dimensions = dimensions
        self.dtype = numpy.dtype(datatype)
        self.recdim = recdim
        super().__init__(**kwds)

    def copy(self, logical=None):
        return self.__copy__(logical)

    def __copy__(self, logical=None):
        """Create copy object."""
        obj = self.__class__(self.dataset, self.name,
                             self.dtype, self.dimensions, self.handle,
                             self.recdim)
        if logical:
            obj.logical = logical
        return obj

    def replace_dim(self, dim):
        for j, d in enumerate(self.dimensions):
            if dim.name == d.name:
                break
        else:
            raise ValueError
        self.dimensions = self.dimensions[:j] + (dim, ) + self.dimensions[j+1:]
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

    def __getitem__(self, elem):
        nd = len(self.dimensions)
        if numpy.iterable(elem):
            if len(elem) > nd:
                raise TypeError
            elem = tuple(elem)
            elem = elem + (slice(None, None, None), ) * (nd - len(elem))
        else:
            elem = (elem,) + (slice(None, None, None), ) * (nd - 1)
        start = ()
        count = ()
        shape = ()
        for e, d in zip(elem, self.dimensions):
            o = d.extent[0]
            if isinstance(e, int):
                start = start + (e + o, )
                count = count + (1, )
            elif isinstance(e, slice):
                if e.step is not None and e.step != 1:
                    raise ValueError \
                        (f"Only value 1 is allowed for slice step {e}.")
                st = e.start or 0
                if st < 0:
                    st = d.size + st
                en = e.stop or d.size
                if en < 0:
                    en = d.size + en
                co = en - st
                start = start + (st + o, )
                count = count + (co, )
                shape = shape + (co, )
        if self.recdim:
            recs = (start[0], count[0])
            start = start[1:]
            count = count[1:]
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
            ds.lib.tnb_var_read(p, r, start, count,
                                ds.handle, self.handle)
        buf = numpy.ctypeslib.as_array(cbuf)
        if miss:
            buf = numpy.ma.masked_equal(buf, miss)
        buf = buf.reshape(shape)
        return buf

    def attrs(self, rec=None):
        """Iterator of the attributes."""
        yield from self._attrs(rec)

    def getattr(self, item, rec=None, conv=None):
        """Get attribute corresponding to item (number or name)."""
        ds = self.dataset
        rec = rec or 0
        return ds.lib.header_get_attr(item, ds.handle, self.handle,
                                      rec=rec, conv=conv)

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

    @property
    def shape(self):
        """Array shape"""
        sh = ()
        for d in self.dimensions:
            sh = sh + (d.size, )
        return sh

    def dimensions_suite(self, group=None):
        group = group or self.dataset.root
        return tuple(self.format_dims(group=group))

    @property
    def dimensions_names(self):
        return tuple(util.tostr(d.name) for d in self.dimensions)

    @property
    def dimensions_map(self):
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
        # print(group.name, repr(group))
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

    def __str__(self):
        """return str"""
        dump = [repr(type(self))]
        tab = ' ' * 4
        vt = util.tostr(self.dtype)
        vn = self.format_name()
        ds = ', '.join(self.format_dims())
        dump.append(f"{vt} {vn}({ds})")
        for a, ai in self.attrs():
            av = self.getattr(a).strip()
            ai = util.tostr(ai)
            if av:
                dump.append(f"{tab}{ai}: {av}")
        dump.append(f"shape = {self.shape}")
        return '\n'.join(dump)


class TouzaNioDimension(_TouzaNio):
    """Dimension (axis) property"""

    __slots__ = ('handle', 'name', 'size', 'extent',
                 'dataset', 'suite', )

    # pylint: disable=too-many-arguments
    def __init__(self, name, size=None,
                 handle=None, begin=None, end=None, group=None,
                 **kwds):
        """Constructor."""

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
        n = util.tostr(self.name)
        p = [f"name = '{n}'",
             f"size = {self.size}", ]
        return f"{repr(type(self))}: " + ', '.join(p)

    def is_record(self):
        """Check if record dimension"""
        return self.handle < 0

    @property
    def shape(self):
        """1d dimension shape"""
        return (self.size, )


class TouzaNioCoDataset(TouzaNioDataset):
    """TOUZA/Nio dataset accompanied by coordinate variables."""

    gtax_env = 'GTAX_PATH'
    #  Null element corresponding to internal field.
    #  If the environemnt does not contain null element,
    #  then null element is inserted at the first candidate.
    #  Therefore, if you need to set precedence of external files than
    #  the internal, then you must set explicitly the null element
    #  somewhere in the environment, as 'dir-a:dire-b::dir-c:dir-d'.
    #  Default candidate is ':.' (internal, then current directory).
    #  If you want to exclude the coordinate binding, you should
    #  not use this class, but use TouzaNioDataset instead.

    def __init__(self, *args, **kwds):
        super().__init__(*args, **kwds)
        self.shift_variables()
        self.bind_coordinates()

    def createGroup(self, groupname,
                    handle=None):
        """Construct group instance."""
        return TouzaNioCoGroup(groupname, parent=self,
                               handle=handle)

    def shift_variables(self):
        """Shift variables to reserve spaces for coodinates."""
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

        for d in self.dimensions.values():
            if d.is_record():
                # print(f"dim/rec: {d.name}[{d.extent}]")
                continue
            c = self.get_coordinate(d, paths, kind='loc')
            if c:
                okey = self.dimensions.rev_map(d)
                if okey in self.variables:
                    raise ValueError(f"Panic.  {okey} {self.variables}")
                self.variables[okey] = c

    def coordinate_paths(self, env=None):
        env = env or self.gtax_env
        paths = os.environ.get(env, '.:')
        return paths.split(':')

    def get_coordinate(self, dim, paths, kind=None):
        """Get physical coordinate Nio-handle."""
        item = util.tostr(dim.name)
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
            if not p:
                # to implement in-file coordinate
                continue
            c = self.check_coordinate(xgrp, item, pfx, p)
            if not c:
                continue
            for cc in c.dimensions:
                if cc.name == dim.name:
                    if dim.extent[0] < cc.extent[0] \
                       or dim.extent[1] > cc.extent[1]:
                        break
                elif cc.size > 1:
                    break
            else:
                c = c.copy(logical=self)
                c.squeeze()
                c.replace_dim(dim)
                return c

        return None

    def check_coordinate(self, grps, item, pfx, path):
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
            if ds:
                if isinstance(grps, str):
                    grps = [grps]
                for g in grps:
                    c = ds.get((g, item,))
                    if c is not None:
                        return c
        return None

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
        pass


# handle to DataSet mapping
_DataSets: dict[int, TouzaNioDataset] = {}

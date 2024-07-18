#!/usr/bin/env python3
# Time-stamp <2024/07/16 13:09:06 fuyuki touza.py>

__doc__ = \
    """
zbt.xrnio
~~~~~~~~~
numpy xarray + TOUZA/Nio extension

:Source:     zbt/xrnio.py
:Maintainer: SAITO Fuyuki <saitofuyuki@jamstec.go.jp>
:Created:    Jul 16 2024
"""

# import xarray as xr
import ctypes as CT
import numpy

from . import libtouza
from . import helper


class _TouzaNio:
    """Common procedures among TouzaNio classes."""
    __slots__ = ('sub', )

    def __init__(self, sub=None):
        """Bind TOUZA/Nio properties from file."""
        self.sub = sub or '~'

    def _fmt_dim(self, d, dn=None):
        dn = dn or d.name
        if isinstance(dn, tuple):
            dn = dn[0] + self.sub + ','.join(str(s) for s in dn[1:])
        if d.handle >= 0:
            h = f"<{d.handle}>"
        else:
            h = ''
        return f"{helper.tostr(dn)}{h}"

    def _fmt_var(self, v, vn=None):
        vn = vn or v.name
        return str(v.dtype) \
            + ' ' + helper.tostr(vn) \
            + '(' + ','.join(f"{self._fmt_dim(d)}"
                             for d in v.dimensions) + ')'

    def _fmt_grp(self, g, gn=None):
        gn = gn or g.name
        return helper.tostr(gn)


# pylint: disable=too-many-ancestors
# pylint: disable=too-many-instance-attributes
class TouzaNioDataset(_TouzaNio):
    """Abstract layer of xarray.Dataset for TOUZA/Nio."""

    __slots__ = ('handle', 'parent', 'name',
                 'groups', 'variables', 'dimensions',
                 'record', 'recdim')

    lib = libtouza.LibTouzaNio(name=None)
    print(lib)

    def __init__(self, filename, rec=None, **kwds):
        """Bind TOUZA/Nio properties from file."""

        if not self.lib.tnb_file_is_nio(filename):
            raise ValueError

        self.handle = self.lib.tnb_file_open(filename)
        self.name = filename
        self.parent = None

        self.recdim = None
        self.record = rec or 'record'  # name of record dimension

        super().__init__(**kwds)

        # add dimensions first to share among groups
        self._add_dimensions()
        self._add_groups()
        self._flatten_variables()

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
        return TouzaNioVar(self,
                           varname, datatype, dimensions,
                           handle=handle, recdim=recdim)

    # pylint: disable=invalid-name,too-many-arguments
    def createDimension(self, dimname, size=None,
                        handle=None, begin=None, end=None):
        """Construct variable instance."""
        return TouzaNioDimension(dimname, size,
                                 handle=handle, begin=begin, end=end)

    def search_dim(self, ser):
        """Search parent corresponding to serial"""
        for d in self.dimensions.values():
            if d.handle == ser:
                return d
        return None

    def datatype_from_dfmt(self, fmt):
        """NIO/Gtool format to numpy datatype conversion."""
        fmt = fmt.strip()
        fmt = fmt.lower()
        if fmt[1:3] == 'i4':
            return 'i4'
        if fmt[1:3] == 'r4':
            return 'f4'
        return 'f8'

    def attrs(self):
        """Iterator of the attributes."""
        yield from self._attrs()

    def getattr(self, item, raw=None):
        """Get attribute corresponding to item (number or name)."""
        return self.lib.header_get_attr(item, self.handle, raw=raw)

    def _add_groups(self):
        """Bind NIO groups in NIO object"""
        self.groups = {}
        for gh, gname in self._groups():
            grp = self.createGroup(gname, gh)
            gk = helper.tostr(gname)
            self.groups[gk] = grp
        return self.groups

    def _add_dimensions(self):
        self.dimensions = {}
        nr = self._recs()
        if nr:
            cname = self.record
            self.recdim = self.createDimension(cname, size=nr, handle=-1)
            self._add_dim(self.recdim, cname)

        for ch, cname in self._dimensions():
            (begin, end) = self.lib.group_co_range(self.handle, ch)
            dim = None
            if self.parent:
                cs = self.lib.tnb_group_co_idx(self.handle, ch)
                dim = self.parent.search_dim(cs)
            if not dim:
                dim = self.createDimension(cname, begin=begin, end=end,
                                           handle=ch)
            self._add_dim(dim, cname)
        return self.dimensions

    def _add_dim(self, dim, cname):
        ck = helper.tostr(cname)
        if ck in self.dimensions:
            self.dimensions[(ck, 0)] = self.dimensions[ck]
            del self.dimensions[ck]
            self.dimensions[(ck, 1)] = dim
        elif (ck, 0) in self.dimensions:
            j = 1
            while (ck, j) in self.dimensions:
                j = j + 1
            self.dimensions[(ck, j)] = dim
        else:
            self.dimensions[ck] = dim
        return self.dimensions

    def _flatten_variables(self):
        self.variables = {}
        return self.variables

    def _add_variables(self, recdim=None):
        self.variables = {}
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
            vk = helper.tostr(vname)
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
            ai = helper.tostr(ai)
            if av:
                dump.append(f"{tab}{ai}: {av}")

        ds = tuple(f"{self._fmt_dim(d, dn)}({len(d)})"
                   for dn, d in sorted(self.dimensions.items(),
                                       key=lambda di: di[1].handle))
        vs = tuple(f"{self._fmt_var(v, vn)}"
                   for vn, v in self.variables.items())

        gs = tuple(self._fmt_grp(g, gn) for gn, g in self.groups.items())
        dump.append(f"    dimensions(sizes): {', '.join(ds)}")
        dump.append(f"    variables(dimensions): {' '.join(vs)}")
        dump.append(f"    groups: {', '.join(gs)}")
        return '\n'.join(dump)


class TouzaNioGroup(TouzaNioDataset):
    """Emulate a hierarchical namespace of dataset."""

    # pylint: disable=super-init-not-called
    def __init__(self, name, parent, handle):
        """Constructor."""
        self.handle = handle
        self.parent = parent
        self.name = name

        self.record = parent.record
        self.sub = parent.sub

        self._add_groups()
        self._add_dimensions()

        recdim = self.dimensions.get(self.record)
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

    __slots__ = ('handle', 'dataset', 'name', 'dimensions', 'recdim', 'dtype')

    def __init__(self, group, name, datatype, dimensions,
                 handle, recdim=None, **kwds):
        """Constructor."""
        self.handle = handle
        self.dataset = group
        self.name = name
        self.dimensions = dimensions
        self.dtype = numpy.dtype(datatype)
        self.recdim = recdim

        super().__init__(**kwds)

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
            if isinstance(e, int):
                start = start + (e, )
                count = count + (1, )
            elif isinstance(e, slice):
                st = e.start or 0
                if st < 0:
                    st = d.size + st
                en = e.stop or d.size
                if en < 0:
                    en = d.size + en
                co = en - st
                start = start + (st, )
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

        cbuf = (ct * full)()
        step = CT.sizeof(ct) * unit
        for j in range(recs[1]):
            r = recs[0] + j
            p = CT.byref(cbuf, step * j)
            p = CT.cast(p, CT.POINTER(ct))
            ds.lib.tnb_var_read(p, r, start, count,
                                ds.handle, self.handle)
        buf = numpy.ctypeslib.as_array(cbuf)
        return buf.reshape(shape)

    def attrs(self, rec=None):
        """Iterator of the attributes."""
        yield from self._attrs(rec)

    def getattr(self, item, rec=None, raw=None):
        """Get attribute corresponding to item (number or name)."""
        ds = self.dataset
        rec = rec or 0
        return ds.lib.header_get_attr(item, ds.handle, self.handle,
                                      rec=rec, raw=raw)

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
        c = ()
        for d in self.dimensions:
            c = c + (d.size, )
        return c

    def __str__(self):
        """return str"""
        dump = [repr(type(self))]
        tab = ' ' * 4
        dump.append(self._fmt_var(self))
        for a, ai in self.attrs():
            av = self.getattr(a).strip()
            ai = helper.tostr(ai)
            if av:
                dump.append(f"{tab}{ai}: {av}")
        dump.append(f"shape = {self.shape}")
        return '\n'.join(dump)


class TouzaNioDimension(_TouzaNio):
    """Dimension (axis) property"""

    __slots__ = ('handle', 'name', 'size', 'begin', 'end', )

    # pylint: disable=too-many-arguments
    def __init__(self, name, size=None,
                 handle=None, begin=None, end=None, **kwds):
        """Constructor."""

        self.handle = handle
        self.name = name
        if (size is None) == ((begin is None) or (end is None)):
            raise ValueError
        if size is not None:
            self.size = size
            self.begin = 0
            self.end = size
        else:
            self.begin = begin
            self.end = end
            self.size = end - begin

        super().__init__(**kwds)

    def __len__(self):
        """Dimension size"""
        return self.size

    def __str__(self):
        """return str"""
        n = helper.tostr(self.name)
        p = [f"name = '{n}'",
             f"size = {self.size}", ]
        return f"{repr(type(self))}: " + ', '.join(p)

# class TouzaNioBackendEntrypoint(xr.backends.BackendEntrypoint):
#     """Xarray extension for TOUZA/Nio format."""

#     description = "Use TOUZA/Nio(gtool-3.5 extension) files in Xarray"
#     url = "https://github.com/saitofuyuki/touza"

#     open_dataset_parameters = ["filename_or_obj",
#                                "drop_variables",
#                                "decode_times",
#                                "decode_timedelta",
#                                "decode_coords",
#                                "touza_nio_option"]

#     def open_dataset(self, filename_or_obj, *,
#                      drop_variables=None,
#                      decode_times=True,
#                      decode_timedelta=True,
#                      decode_coords=True,
#                      touza_nio_option=None):
#         pass

#     def guess_can_open(self, filename_or_obj):
#         return self.lib.tnb_file_is_nio(filename_or_obj)

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

import xarray as xr
import numpy as np

from . import libtouza
from . import dsnio
from . import util


class xrNioDataArray(xr.DataArray):
    """Wrap class Xarray.DataArray for TOUZA/Nio integration."""
    __slots__ = ()


class xrNioVariable(xr.Variable):
    """Wrap class Xarray.Variable for TOUZA/Nio integration."""


class xrNioDataset(xr.Dataset):
    """Wrap class Xarray.Dataset for TOUZA/Nio integration."""
    __slots__ = ('data_arrays', '_dataset', )

    def __init__(self, *args, org_dataset=None, **kwds):
        """Wrap Xarray.Dataset to assign data_arrays map."""
        super().__init__(*args, **kwds)
        self._dataset = org_dataset


class xrNioBackendArray(xr.backends.BackendArray, dsnio.TouzaNioVar):
    """Xarray extension for TOUZA/Nio format."""

    def __init__(self, *args, **kwds):
        super().__init__(*args, **kwds)
        # print('ba:', self.dimensions)
        self.dims = []
        for d in self.dimensions:
            self.dims.append(util.tostr(d.name))

    def __getitem__(self,
                    key: xr.core.indexing.ExplicitIndexer) \
                    -> np.typing.ArrayLike:
        return xr.core.indexing.explicit_indexing_adapter(
            key,
            self.shape,
            xr.core.indexing.IndexingSupport.BASIC,
            self._raw_indexing_method,
        )

    def _raw_indexing_method(self, key: tuple) \
            -> np.typing.ArrayLike:
        arr = super().__getitem__(key)
        if self.dtype.kind in ['i', 'u', ]:
            return arr
        return arr.filled(fill_value=np.nan)


class xrMemBackendArray(xr.backends.BackendArray, dsnio.TouzaMemVar):
    """Xarray extension for TOUZA/Nio format."""

    def __init__(self, *args, **kwds):
        super().__init__(*args, **kwds)
        self.dims = []
        for d in self.dimensions:
            self.dims.append(util.tostr(d.name))

    def __getitem__(self,
                    key: xr.core.indexing.ExplicitIndexer) \
                    -> np.typing.ArrayLike:
        r = xr.core.indexing.explicit_indexing_adapter(
            key,
            self.shape,
            xr.core.indexing.IndexingSupport.BASIC,
            self._raw_indexing_method, )
        return r

    def _raw_indexing_method(self, key: tuple) \
            -> np.typing.ArrayLike:
        arr = super().__getitem__(key)
        return arr


class xrNioBackendEntrypoint(xr.backends.BackendEntrypoint):
    """Xarray extension for TOUZA/Nio format."""

    description = "Use TOUZA/Nio(gtool-3.5 extension) files in Xarray"
    url = "https://github.com/saitofuyuki/touza"

    open_dataset_parameters = ["filename_or_obj",
                               "drop_variables",
                               "decode_times",
                               "decode_timedelta",
                               "decode_coords",
                               "touza_nio_option"]

    lib = libtouza.LibTouzaNio(name=None)

    def open_dataset(self, filename_or_obj, *,
                     drop_variables=None,
                     decode_times=True,
                     decode_timedelta=True,
                     decode_coords=True,
                     decode_attrs=True,
                     **kwds):
        if decode_coords:
            dscls = dsnio.TouzaNioCoDataset
        else:
            dscls = dsnio.TouzaNioDataset

        ds = dscls(filename_or_obj,
                   cls_var=xrNioBackendArray,
                   cls_arr=xrMemBackendArray,
                   **kwds)

        variables = {}
        for v in ds.variables.values():
            vn = ds.variables.rev_map(v, sep=ds.sub)
            dims = v.dimensions_suite(group=ds)

            attrs = {}
            for _, k in v.attrs():
                a = v.getattr(k).strip()
                if a:
                    k = util.tostr(k)
                    if k in ['DIVL', 'DIVS', 'DMIN', 'DMAX', ]:
                        a = float(a)
                    attrs[k] = a
            if decode_attrs:
                for src in [('units', 'UNIT'),
                            ('long_name', 'TITL1', 'TITL2'), ]:
                    dst = src[0]
                    a = ''
                    for i in src[1:]:
                        a = a + v.getattr(i)
                    if a:
                        attrs[dst] = a
            attrs['_nio_var'] = v
            data = xr.core.indexing.LazilyIndexedArray(v)
            # var = xr.Variable(dims, data, attrs=attrs)
            var = xrNioVariable(dims, data, attrs=attrs)
            variables[util.tostr(vn)] = var

        return xrNioDataset(variables, org_dataset=ds)

    def guess_can_open(self, filename_or_obj):
        if isinstance(filename_or_obj, str):
            return self.lib.tnb_file_is_nio(filename_or_obj)
        return False

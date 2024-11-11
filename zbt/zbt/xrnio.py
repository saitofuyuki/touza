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
import zbt.util as zu


locallog = zu.LocalAdapter('xrnio')


_RECDIM_ATTR = '_nio_recdim'

__all__ = ["xrNioDataArray", "xrNioVariable", "xrNioDataset",
           "xrNioBackendArray", "xrMemBackendArray",
           "xrNioBackendEntrypoint", "open_dataset", ]

class xrNioDataArray(xr.DataArray):
    """Wrap class Xarray.DataArray for TOUZA/Nio integration."""
    __slots__ = ()

    def sel(self, indexers=None,
            method=None, tolerance=None, drop=False, **indexers_kwargs):
        va = super().sel(indexers, method=method, tolerance=tolerance,
                         drop=drop, **indexers_kwargs)
        recdim = va.attrs.get(_RECDIM_ATTR, None)
        rsel = None
        if recdim:
            rsel = indexers.get(recdim, None)
            if rsel is not None:
                crec = self.coords[recdim].to_index()
                if isinstance(rsel, slice):
                    start = rsel.start
                    if start is not None:
                        start = crec.get_loc(start)
                    stop = rsel.stop
                    if stop is not None:
                        stop = crec.get_loc(stop) + 1
                    rsel = slice(start, stop, rsel.step)
                else:
                    # xs = v.coords[c].sel({c: xs}, method='nearest')
                    rsel = crec.get_loc(rsel)
        va = self._tweak(va, indexers, rsel=rsel)
        return va

    def isel(self, indexers=None, drop=False,
             missing_dims='raise', **indexers_kwargs):
        va = super().isel(indexers, drop, missing_dims, **indexers_kwargs)
        va = self._tweak(va, indexers)
        return va

    def _tweak(self, va, indexers=None, rsel=None):
        """Attribute tweaking."""
        if indexers is None:
            return va

        recdim = va.attrs.get(_RECDIM_ATTR, None)
        if recdim:
            rsel = zu.set_default(rsel, indexers.get(recdim, None))
            if rsel is not None:
                for k, v in va.attrs.items():
                    if isinstance(v, tuple):
                        va.attrs[k] = v[rsel]
        for co, sel in indexers.items():
            oco = self.coords[co]
            nco = va.coords.get(co, None)
            if nco is None:
                continue
            if isinstance(sel, slice):
                dmin, dmax = 'DMIN', 'DMAX'
                if oco[0] > oco[-1]:
                    dmin, dmax = dmax, dmin
                if sel.start is not None:
                    nco.attrs[f'_ignore_{dmin}'] = True
                if sel.stop is not None:
                    nco.attrs[f'_ignore_{dmax}'] = True
        return va


class xrNioVariable(xr.Variable):
    """Wrap class Xarray.Variable for TOUZA/Nio integration."""


class xrNioDataset(xr.Dataset):
    """Wrap class Xarray.Dataset for TOUZA/Nio integration."""
    __slots__ = ()
    _enable_modify_class = True
    # _enable_modify_class = False

    def __init__(self, *args, **kwds):
        """Wrap Xarray.Dataset to assign data_arrays map."""
        super().__init__(*args, **kwds)
        # self._dataset = org_dataset

    def _construct_dataarray(self, name):
        """Construct a xrNioDataArray by indexing this dataset"""

        va = super()._construct_dataarray(name)
        # hack solution to class promotion
        if self._enable_modify_class:
            va.__class__ = xrNioDataArray
        return va


class xrNioBackendArray(xr.backends.BackendArray, dsnio.TouzaNioVar):
    """Xarray extension for TOUZA/Nio format."""

    def __init__(self, *args, **kwds):
        super().__init__(*args, **kwds)
        self.dims = []
        for d in self.dimensions:
            self.dims.append(zu.tostr(d.name))

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
            self.dims.append(zu.tostr(d.name))

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

            # attrs = {_special_attr:  v}
            attrs = {}
            if v.recidx is None:
                pass
            else:
                recdim = dims[v.recidx]
                attrs[_RECDIM_ATTR] = recdim

            if decode_attrs:
                for src in [('units', 'UNIT'),
                            ('long_name', 'TITL1', 'TITL2'), ]:
                    dst = src[0]
                    a = ''
                    for i in src[1:]:
                        a = a + v.getattr(i)
                    if a:
                        attrs[dst] = a
                for a, ai in v.attrs():
                    av = v.getattr(a, rec=slice(None, None),
                                   strip=True, uniq=True)
                    dst = zu.tostr(ai)
                    attrs[dst] = av

            data = xr.core.indexing.LazilyIndexedArray(v)
            var = xrNioVariable(dims, data, attrs=attrs)
            variables[zu.tostr(vn)] = var

        return xrNioDataset(variables)

    def guess_can_open(self, filename_or_obj):
        if isinstance(filename_or_obj, str):
            return self.lib.tnb_file_is_nio(filename_or_obj)
        return False


def open_dataset(filename_or_obj, *args, engine=None, **kwargs):
    """Wrap xarray.open_dataset() with TOUZA/Nio engine."""
    xds = None
    if engine is None or engine == 'zbt':
        nbe = xrNioBackendEntrypoint()
        if nbe.guess_can_open(filename_or_obj):
            xds = nbe.open_dataset(filename_or_obj, *args, **kwargs)
        elif engine == 'zbt':
            raise ValueError(f'Not zbt file: {filename_or_obj}.')
    if xds is None:
        xds = xr.open_dataset(filename_or_obj, *args, engine=engine, **kwargs)

    return xds

#!/usr/bin/env python3
# Time-stamp: <2025/02/05 10:04:50 fuyuki xrnio.py>

__doc__ = \
    """
zbt.xrnio
~~~~~~~~~
numpy xarray + TOUZA/Nio extension

:Source:     zbt/xrnio.py
:Maintainer: SAITO Fuyuki <saitofuyuki@jamstec.go.jp>
:Created:    Jul 16 2024
"""

import collections.abc as cabc

import xarray as xr
import numpy as np
import cftime

# import zbt.util as zu
from . import util as zu
from . import libtouza
from . import dsnio

locallog = zu.LocalAdapter(__name__)

_RECDIM_ATTR = '_nio_recdim'

__all__ = ["xrNioDataArray", "xrNioVariable", "xrNioDataset",
           "xrNioBackendArray", "xrMemBackendArray",
           "xrNioBackendEntrypoint", "open_dataset", "diag", ]

@xr.register_dataarray_accessor("nio")
class TouzaNioAccessor:
    """
    Accessor/container class to inquire record-each attributes.
    """

    def __init__(self, arr):
        """
        Constructor used in DataArray.

        Parameters
        ----------
        arr : DataArray
            Base attributes holder.
        """

        self._array = arr
        self._attrs = {}
        self._recidx = None

    def __setitem__(self, key, value):
        self._attrs[key] = value

    def __getitem__(self, key):
        if key in self._attrs:
            return self._attrs[key]
        if not key in self._array.attrs:
            raise KeyError
        ov = self._array.attrs[key]
        if not isinstance(ov, (tuple, list)):
            self._attrs[key] = ov
            return self._attrs[key]
        if self._recidx is None:
            self._recidx = self.parse_recs(self._array)
        if self._recidx is False:
            self._attrs[key] = False
        elif isinstance(self._recidx, (list, tuple)):
            rt = type(ov)
            self._attrs[key] = rt(ov[j] for j in self._recidx)
        else:
            self._attrs[key] = ov[self._recidx]

        return self._attrs[key]

    def get(self, key, default=None):
        try:
            return self[key]
        except KeyError:
            return default

    def parse_recs(self, src: xr.DataArray) -> list[int]|int|bool:
        """
        Parse record coordinate to extract corresponding record index.

        Parameters
        ----------
        src : xr.DataArray
            Array to hold the record coordinate and attributes.

        Returns
        -------
        list|int|bool
            List of record index or False.
        """

        recs = src.attrs.get(_RECDIM_ATTR)

        if recs is None:
            return False

        recco, recs = recs
        if recco not in src.coords:
            return False

        def index(arr, v):
            """First find."""
            return next((idx for idx, val in enumerate(arr)
                          if val == v), None)

        recco = src.coords[recco]
        if recco.shape:
            rr = []
            for item in recco:
                print(f"{item=}")
                # item = item.item()
                idx = index(recs, item)
                # print(f"[{idx}]{item.item()=}")
                rr.append(idx)
            recs = rr
        else:
            item = recco
            # item = recco.item()
            idx = index(recs, item)
            # print(f"[{idx}]{item.item()=}")
            recs = idx
        # print(f"{recs=}")
        return recs


class xrNioDataArray(xr.DataArray):
    """Wrap class Xarray.DataArray for TOUZA/Nio integration."""
    __slots__ = ()


class xrNioVariable(xr.Variable):
    """Wrap class Xarray.Variable for TOUZA/Nio integration."""


class xrNioDataset(xr.Dataset):
    """Wrap class Xarray.Dataset for TOUZA/Nio integration."""
    __slots__ = ()


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

    xrnio_parameters = ("calendar",
                        "touza_nio_option")

    open_dataset_parameters = ("filename_or_obj",
                               "drop_variables",
                               "decode_times",
                               "decode_timedelta",
                               "decode_coords", ) + xrnio_parameters

    def open_dataset(self, filename_or_obj, *,
                     drop_variables=None,
                     decode_times=True,
                     decode_timedelta=True,
                     decode_coords=True,
                     decode_attrs=True,
                     calendar=None,
                     **kwds):

        if decode_coords:
            dscls = dsnio.TouzaNioCoDataset
            dtt, cal = self.parse_calendar(calendar, decode_times)
            kwds['calendar'] = cal
        else:
            dtt = False
            dscls = dsnio.TouzaNioDataset
        # print(kwds)
        ds = dscls(filename_or_obj,
                   cls_var=xrNioBackendArray,
                   cls_arr=xrMemBackendArray,
                   **kwds)
        # print(f'ds={type(ds)}')
        variables = {}
        for v in ds.variables.values():
            vn = ds.variables.rev_map(v, sep=ds.sub)
            try:
                dims = v.dimensions_suite(group=ds)
            except AttributeError:
                dims = ('record', )

            # attrs = {_special_attr:  v}
            attrs = {}
            # print(dims, getattr(v, 'recidx', None))
            recidx = getattr(v, 'recidx', None)
            if recidx is None:
                pass
            else:
                # set temporary
                attrs[_RECDIM_ATTR] = dims[recidx]

            if decode_attrs:
                try:
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
                except AttributeError:
                    attrs = {}

            data = xr.core.indexing.LazilyIndexedArray(v)
            var = xrNioVariable(dims, data, attrs=attrs)
            variables[zu.tostr(vn)] = var

        dset = xrNioDataset(variables)
        if dtt == 'numpy':
            dtt = np.datetime64
        else:
            dtt = None
        if dtt:
            for cn in dset.coords:
                co = dset[cn]
                c0 = co.values[0]
                if isinstance(c0, cftime.datetime):
                    # print(cn, co.dtype, type(c0))
                    ntime = []
                    for t in co.values:
                        ndt = dtt(t)
                        # ndt = np.datetime64(t)
                        ntime.append(ndt)
                    try:
                        nda = xr.DataArray(ntime, dims=(cn, ),
                                           name=cn)
                        dset[cn] = nda
                    except ValueError as err:
                        locallog.warning(err)
                        msg = f"Failed at calendar conversion to {dtt}."
                        raise RuntimeError(msg) from None
        if decode_attrs:
            for var in dset.data_vars:
                var = dset[var]
                recdim = var.attrs.get(_RECDIM_ATTR, None)
                if recdim:
                    var.attrs[_RECDIM_ATTR] = (recdim,
                                               var.coords[recdim].data)
                _ = var.nio

        return dset

    def guess_can_open(self, filename_or_obj):
        if isinstance(filename_or_obj, str):
            return dsnio._TouzaNio.is_nio_file(filename_or_obj)
        return False

    def parse_calendar(self, cal, decode_times):
        """Parse calendar argument complex and return tuple.of
        type, [calendars]"""

        dtype = False
        if cal is None:
            cal = bool(decode_times)
        if cal is False:
            pass
        else:
            if cal is True:
                cal = []
            elif isinstance(cal, str):
                cal = [cal]
            if isinstance(cal, (tuple, list)):
                tmp = []
                for k in cal:
                    if k in ['numpy', ]:
                        dtype = 'numpy'
                    elif k == 'auto':
                        tmp = []
                    elif k in dsnio.calendar_epoch:
                        tmp.append(k)
                cal = tmp

        return dtype, cal


def diag(obj=None,
         fmt: str | None=None,
         show: cabc.Callable | None=None):
    """
    Diagnose objects.

    Parameters
    ----------
    obj : any
        Object to diagnose.
        If None, module properties are diagnosed.
        Otherwise, obj is diagnosed according to its type.
        At the moment, only xarray.DataArray is treated particularly.
    fmt : str
        Format string.  Need '{}' to replace by generated string.
    show : (method, None)
        Function to output the results.

    Examples
    --------
    >>> xrnio.diag(vsel, fmt='vsel=<{}>')
    vsel=<T2 (GGLA128: 128, GLON256: 256)+[record: 2001-03-16 12:00:00]>
    """
    show = locallog.info
    if obj is None:
        show(f"{zu=}")
        show(f"{libtouza=}")
        show(f"{dsnio=}")
        return
    if isinstance(obj, xr.DataArray):
        dims = ', '.join(f"{d}: {s}" for d, s in zip(obj.dims, obj.shape))
        # rval = va.coords[recdim].item()
        ndc = ', '.join(f"{c}: {obj[c].item()}" for c in obj.coords.keys()
                       if c not in obj.dims)
        txt = f"{obj.name} ({dims})"
        if ndc:
            txt = txt + f'+[{ndc}]'
    else:
        txt = f"{obj}"
    if fmt:
        txt = fmt.format(txt)
    show(txt)


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
        for k in xrNioBackendEntrypoint.xrnio_parameters:
            if k in kwargs:
                locallog.warning(f"{k} argument removed"
                                 " to call open_dataset().")
                del kwargs[k]
        try:
            xds = xr.open_dataset(filename_or_obj, *args,
                                  engine=engine, **kwargs)
        except ValueError as err:
            locallog.error("open_dataset() fails.")
            if not dsnio._TouzaNio.is_loaded():
                locallog.error("touza library seems not be loaded.")
            # locallog.error(err)
            raise ValueError(err) from None
    return xds

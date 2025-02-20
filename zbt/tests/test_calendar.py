#!/usr/bin/env python3
# Time-stamp: <2025/01/23 15:56:48 fuyuki test_calendar.py>

import sys
import xarray as xr
import itertools
import pathlib as plib
import logging

sys.path.insert(0, str(plib.Path(__file__).parents[1]))

import zbt.util as zu
import zbt.xrnio as zxr
import zbt.plot as zplt
import cftime
import numpy as np
import pandas as pd

def main(args):
    """Test driver."""
    logger = zu.logger
    logger.setLevel(logging.DEBUG)
    zu.diag()
    zxr.diag(show=True)

    for a in args:
        print(f"open: {a}")
        ds = zxr.open_dataset(a)
        for vk in ds.data_vars:
            vv = ds[vk]
            print(f"var: {vk} {vv.shape} {vv.dims}")
            tdim = None
            cal = None
            for dn in vv.dims:
                dim = vv[dn]
                dl = len(dim)
                dt = dim.dtype
                d0 = dim[0].item()
                print(f"dim: {dn} {dl} {dt} {d0} {type(d0)}")
                if isinstance(d0, (cftime.datetime, np.datetime64)):
                    tdim = dn
                    cal = d0.calendar
                elif np.issubdtype(dt, np.datetime64):
                    tdim = dn

            if tdim:
                print("time:")
                print(ds[tdim].values)
                for date in ['2001-3-16', '2001-7', ]:
                    pts = pd.Timestamp(date)
                    if cal:
                        cft = cftime.to_tuple(pts)
                        cft = cftime.datetime(*cft, calendar=cal)
                    else:
                        cft = pts
                    print(f"date: {date} {pts} {cft}")
                    vsel = vv.sel({tdim: cft}, method='nearest')
                    zxr.diag(vsel, fmt='vsel=<{}>')
                    # print(f"{vsel.dims=}")
                    # print(f"{vsel.shape=}")
                    # print(f"{vsel=}")
                    # print(vv.sel({tdim: slice(cft, None)}))


if __name__ == '__main__':
    import sys
    main(sys.argv[1:])

#!/usr/bin/env python3
# Time-stamp: <2025/02/05 09:13:47 fuyuki test_xrnio.py>

import sys
import xarray as xr
import itertools
import pathlib as plib
import numpy as np
import pandas as pd
import cftime

import matplotlib.pyplot as plt
import cartopy.crs as ccrs

sys.path.insert(0, str(plib.Path(__file__).parents[1]))

import zbt.xrnio as zxr

# @xr.register_dataarray_accessor("nio")
# class NioAccessor:
#     def __init__(self, xarray_obj):
#         print(f"accessor: {xarray_obj}")


def main(argv):
    """Sample driver."""
    decode_coords = False
    plot = False
    proj = None
    check_record = None
    calendar = None
    # xr.set_options(enable_cftimeindex=True)

    while argv:
        if argv[0][0] != '-':
            break
        if argv[0] == '-c':
            decode_coords = True
        elif argv[0] == '-P':
            plot = True
        elif argv[0] == '-C':
            # proj = ccrs.Robinson()
            proj = ccrs.PlateCarree()
        elif argv[0].startswith('-r'):
            check_record = 'record'
            sub = argv[0][2:]
            if sub== 'r':
                calendar = False
            elif sub == 'c':
                calendar = 'cftime'
            elif sub == 'n':
                calendar = 'numpy'
        argv = argv[1:]

    for a in argv:
        print(f'##### file: {a}')
        try:
            xds = xr.open_dataset(a, decode_coords=decode_coords,
                                  calendar=calendar)
        except TypeError as err:
            print(err)
            print("reopen without calendar.")
            xds = xr.open_dataset(a, decode_coords=decode_coords)
        print('### dataset')
        print(xds)
        for g in [xds]:
            for vn, vv in g.data_vars.items():
                print(f"# var:{vn} {vv.shape} {type(vv)}")
                print(vv)
                print(f"{vv.attrs=}")
                # print(f"{vv.nio.recco=}")
                # need activate nio attributes by vv.nio access.
                # _ = vv.nio
                # print(f"{vv.nio=}")
                # print(f"{vv.attrs=}")
                # print(g[vn].attrs)
                print(f"{type(vv[0])=}")
                print(f"{vv[0].attrs=}")
                print(f"{vv.attrs=}")
                # vv.nio._recidx = True
                # print(f"{vv.nio._recidx=}")
                # dt1 = vv.nio.get('DATE1')
                dt1 = vv.nio.get('DATE1')
                print(f"{dt1=}")
                dset = vv.nio.get('DSET')
                print(f"{dset=}")
                for cn, co in vv.coords.items():
                    print(f"coord[{cn}]: {type(co)=}")
                    print(f"coord[{cn}]: {type(co.data)=}")
                    print(f"coord[{cn}]: {co.dtype=}")
                    print(f"coord[{cn}]: {repr(co.item(0))=}")
                    print(f"coord[{cn}]: {repr(co[0])=}")
                    print(f"coord[{cn}]: {np.issubdtype(co.dtype, np.datetime64)=}")
                    has_dt = hasattr(co, 'dt')
                    print(f"coord[{cn}]: {has_dt=}")
                    # print(f"coord[{cn}]: {co.nio=}")
                    if hasattr(co, 'dt'):
                        print(co.dt.strftime("%Y%m%d"))
                        print(f"{co.dt.calendar=}")
                        dt = '2000-05'
                        vsel = None
                        try:
                            print(f"co[{dt}]")
                            print(co.sel({cn: dt}, method='nearest'))
                        except (TypeError, KeyError) as err:
                            print(f"Retry: {err}")
                            dt = pd.Timestamp(dt)
                            dt = cftime.to_tuple(dt)
                            dt = cftime.datetime(*dt, calendar=co.dt.calendar)
                            print(f"Retry: {dt=}")
                            print(f"co[{dt}]")
                            print(co.sel({cn: dt}, method='nearest'))
                        try:
                            print(f"vv[{dt}]")
                            vsel = vv.sel({cn: dt}, method='nearest')
                            print(vsel)
                            # print(f"{vsel.nio._recidx=}")
                            dt1 = vsel.nio.get('DATE1')
                            print(f"{dt1=}")
                            dset = vsel.nio.get('DSET')
                            print(f"{dset=}")
                            # nac = vsel.nio
                            # print(f"{vsel.nio.recco=}")
                            print(vsel.attrs)
                        except (TypeError, KeyError) as err:
                            print(f"Error: {err}")

                # if check_record:
                #     print(vv.coords)
                #     try:
                #         rc = vv[check_record]
                #         # print(rc)
                #         ntime = []
                #         for t in rc.values:
                #             ndt = numpy.datetime64(t)
                #             print(f"{t} {t=} {ndt}")
                #             ntime.append(ndt)
                #             #         # ntime = [numpy.datetime64(t) for t in time]
                #         try:
                #             nda = xr.DataArray(ntime, dims=(check_record, ),
                #                                name='ndate')
                #             print(nda)
                #             vv[check_record] = nda
                #         except ValueError as err:
                #             print(err)
                #         print(vv.coords)
                #     except KeyError as err:
                #         print(err)
                if plot:
                    if check_record:
                        ext = vv.shape[1:-1]
                    else:
                        ext = vv.shape[:-2]
                    for x in itertools.product(*tuple(range(n) for n in ext)):
                        # x = x + plane
                        s = ','.join([str(idx) for idx in x])
                        if check_record:
                            x = (slice(None, None, None), ) + x
                            print(f"# plot: {vn}[:,{s},:]")
                            sel = vv[x]
                        else:
                            print(f"# plot: {vn}[{s},:,:]")
                            sel = vv[x]
                        # sel = sel.squeeze()
                        # print(sel)
                        # print(sel.shape)
                        print(sel.dims)
                        print(sel.coords)
                        print(sel.attrs)
                        # fig, ax = plt.subplots()
                        fig = plt.figure(figsize=(10, 5))
                        ax = fig.add_subplot(1, 1, 1, projection=proj)
                        if proj:
                            ax.set_global()
                            # ax.stock_img()
                            ax.coastlines()
                            try:
                                sel.plot(ax=ax, transform=ccrs.PlateCarree())
                                # sel.plot(ax=ax,
                                #          transform=ccrs.PlateCarree(),
                                #          subplot_kws=dict(projection=proj))
                            except Exception as x:
                                print(x)
                        else:
                            try:
                                sel.plot(ax=ax)
                            except ImportError as err:
                                print(err)
                                ax.contourf(sel)
                        plt.show()
                        # plt.close(fig)

if __name__ == '__main__':
    main(sys.argv[1:])

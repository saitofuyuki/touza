#!/usr/bin/env python3
# Time-stamp: <2024/10/18 12:38:59 fuyuki test_xrnio.py>

import sys
import xarray as xr
import itertools
import pathlib as plib

import matplotlib.pyplot as plt
import cartopy.crs as ccrs

sys.path.insert(0, str(plib.Path(__file__).parents[1]))

import zbt.xrnio as zxr


def main(argv):
    """Sample driver."""
    decode_coords = False
    plot = False
    proj = None
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
        argv = argv[1:]

    for a in argv:
        print(f'##### file: {a}')
        xds = xr.open_dataset(a, decode_coords=decode_coords)
        print('### dataset')
        print(xds)
        for g in [xds]:
            for vn, vv in g.data_vars.items():
                print(f"# var:{vn} {vv.shape} {type(vv)}")
                # print(vv)
                # print(vv.attrs)
                if plot:
                    ext = vv.shape[:-2]
                    for x in itertools.product(*tuple(range(n) for n in ext)):
                        # x = x + plane
                        s = ','.join([str(idx) for idx in x])
                        print(f"# plot: {vn}[{s},:,:]")
                        sel = vv[x]
                        # sel = sel.squeeze()
                        # print(sel)
                        # print(sel.shape)
                        # print(sel.dims)
                        # print(sel.coords)
                        # print(sel.attrs)
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
                        plt.show()
                        # plt.close(fig)


if __name__ == '__main__':
    main(sys.argv[1:])

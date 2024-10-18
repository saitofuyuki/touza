#!/usr/bin/env python3
# Time-stamp: <2024/10/18 13:27:50 fuyuki test_plot.py>

import sys
import xarray as xr
import itertools
import pathlib as plib

import matplotlib.pyplot as plt
import cartopy.crs as ccrs

sys.path.insert(0, str(plib.Path(__file__).parents[1]))

import zbt.xrnio as zxr
import zbt.plot as zplt


def main(argv):
    """Sample driver."""
    decode_coords = False
    proj = None
    transf = None
    while argv:
        if argv[0][0] != '-':
            break
        if argv[0] == '-c':
            decode_coords = True
        elif argv[0] == '-C':
            proj = ccrs.Robinson()
            # proj = ccrs.PlateCarree()
            transf = ccrs.PlateCarree()
        argv = argv[1:]

    Plot = zplt.ContourPlot(method='imshow')
    Lay = zplt.LayoutLegacy3()
    fig, axs = Lay(projection=proj)

    for a in argv:
        print(f'##### file: {a}')
        xds = zxr.open_dataset(a, decode_coords=decode_coords)
        print('### dataset')
        print(xds)
        for g in [xds]:
            for vn, vv in g.data_vars.items():
                print(f"# var:{vn} {vv.shape} {type(vv)}")
                ext = vv.shape[:-2]
                for x in itertools.product(*tuple(range(n) for n in ext)):
                    s = ','.join([str(idx) for idx in x])
                    print(f"# plot: {vn}[{s},:,:]")
                    sel = vv[x]
                    Plot(fig, axs, sel, transform=transf)
                    break
                break
            break
    plt.show()


if __name__ == '__main__':
    main(sys.argv[1:])

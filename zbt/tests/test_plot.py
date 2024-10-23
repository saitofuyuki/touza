#!/usr/bin/env python3
# Time-stamp: <2024/10/22 08:54:50 fuyuki test_plot.py>

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
    crs = None
    features = []
    while argv:
        if argv[0][0] != '-':
            break
        if argv[0] == '-c':
            decode_coords = True
        elif argv[0][:2] == '-C':
            if argv[0][2:] == '':
                proj = ccrs.PlateCarree()
            elif argv[0][2:] == 'm':
                proj = ccrs.Mercator()
            elif argv[0][2:] == 'mo':
                proj = ccrs.Mollweide()
            elif argv[0][2:] == 'ns':
                proj = ccrs.NorthPolarStereo()
            elif argv[0][2:] == 'ss':
                proj = ccrs.SouthPolarStereo()
            elif argv[0][2:] == 'np':
                proj = ccrs.NearsidePerspective()
            elif argv[0][2:] == 'h':
                proj = ccrs.Hammer()
            else:
                proj = ccrs.Robinson()
            transf = ccrs.PlateCarree()
        elif argv[0][:2] == '-M':
            if argv[0][2:] == '':
                features.append('COASTLINE')
            else:
                if 'c' in argv[0][2:]:
                    features.append('COASTLINE')
                if 'l' in argv[0][2:]:
                    features.append('LAND')
                if 'o' in argv[0][2:]:
                    features.append('OCEAN')
                if 'b' in argv[0][2:]:
                    features.append('BORDERS')
                if 'r' in argv[0][2:]:
                    features.append('RIVERS')
                if 'L' in argv[0][2:]:
                    features.append('LAKES')
        argv = argv[1:]

    crs = transf if crs is None else crs
    color = dict(method='imshow', alpha=0.2)
    contour = {}
    # contour = {'transform': transf }
    # if proj:
    #     color['transform'] = transf
    Plot = zplt.ContourPlot(color=color, contour=contour)
    body = dict(transform=transf, crs=crs, features=features)
    # Plot = zplt.ContourPlot(method='contourf')
    # Plot = zplt.ContourPlot(method='pcolormesh')
    Lay = zplt.LayoutLegacy3()
    fig, axs = Lay(body=dict(projection=proj))
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
                    # Plot(fig, axs, sel, transform=transf, crs=crs)
                    Lay.reset(fig, axs)
                    Plot(fig, axs, sel, body=body)
                    # Plot(fig, axs, sel)
                    break
                break
            break
    plt.show()


if __name__ == '__main__':
    main(sys.argv[1:])

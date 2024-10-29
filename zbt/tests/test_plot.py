#!/usr/bin/env python3
# Time-stamp: <2024/11/04 20:39:55 fuyuki test_plot.py>

import sys
import xarray as xr
import itertools
import pathlib as plib
import tomllib as toml

import matplotlib as mplib
import matplotlib.pyplot as plt
import matplotlib.ticker as mtc
import matplotlib.animation as animation
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
    anim = False
    method = 'imshow'
    cint = None
    cfgf = None
    while argv:
        if argv[0][0] != '-':
            break
        if argv[0] == '-X':
            decode_coords = True
        elif argv[0] == '-a':
            anim = True
        elif argv[0][:2] == '-p':
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
        elif argv[0][:2] == '-c':
            cint = float(argv[0][2:])
        elif argv[0][:2] == '-m':
            method = argv[0][2:]

        elif argv[0] == '--cfg':
            argv = argv[1:]
            cfgf = argv[0]

        argv = argv[1:]

    config = {}
    if cfgf:
        with open(cfgf, "rb") as fp:
            c = toml.load(fp)
            config.update(c)

    crs = transf if crs is None else crs
    color = dict(method=method)
    # color = dict(method='imshow', alpha=0.2)
    # color = dict(method='contour', alpha=0.2)
    contour = {}
    if cint:
        contour['levels'] = [mtc.MultipleLocator(cint).tick_values]
    # contour = {'transform': transf }
    # if proj:
    #     color['transform'] = transf
    zplt.ContourPlot.config(config, groups=['test'], )
    Plot = zplt.ContourPlot(color=color, contour=contour)
    Plot.diag()

    Pic = zplt.Picture(zplt.LayoutLegacy3)
    # Lay = zplt.LayoutLegacy3()
    body = dict(transform=transf, crs=crs, features=features)
    fig, axs = Pic(body=dict(projection=proj))

    for a in argv:
        print(f'##### file: {a}')
        xds = zxr.open_dataset(a, decode_coords=decode_coords)
        print('### dataset')
        print(xds)
        for g in [xds]:
            for vn, vv in g.data_vars.items():
                print(f"# var:{vn} {vv.shape} {type(vv)}")
                ext = vv.shape[:-2]
                aseq = []
                isel = None
                for x in itertools.product(*tuple(range(n) for n in ext)):
                    s = ','.join([str(idx) for idx in x])
                    print(f"# plot: {vn}[{s},:,:]")
                    sel = vv[x]
                    if isel is None:
                        isel = vv[x]
                    # Lay.reset(fig, axs)
                    axs.reset(fig, default=True, colorbar=dict(fresh=True))
                    # axs.reset(fig, default=False, colorbar=dict(fresh=True))
                    artists = Plot(fig, axs, sel, body=body)
                    # print(artists)
                    if not anim:
                        break
                    aseq.append(artists)
                    # axs.reset(fig, default=False, colorbar=dict(fresh=True))
                    # break
                # Plot(fig, axs, isel, body=body)
                # print(fig)
                break
            # axs.cla()
            break
        break
    # Plot(fig, axs, sel, body=body)
    # axs.cla()

    for ax in fig.axes:
        gid = ax.get_gid()
        print(f"[{gid}] {ax}")
        for aa in ax.findobj():
            print(f"  [{aa.get_gid()}] {aa}")
            # if isinstance(aa, mplib.spines.Spine):
            #     aa.set_visible(False)
            # aa.set_visible(False)
    # plt.show()

    if anim:
        for ja, aa in enumerate(aseq):
            for a in aa:
                gid = a.get_gid()
                b = a.get_visible()
                print(f"({ja}) [{gid}] {b} {a}")

    # if anim:
    #     # print(aseq[0])
    #     for a in fig.axes:
    #         # a.set_visible(False)
    #         gid = a.get_gid()
    #         if gid == 'colorbar':
    #             a.set_visible(False)
    #             # print(a, a.get_visible(), id(a))

    # # for ja, aa in enumerate(aseq):
    # #     for a in aa:
    # #         gid = a.get_gid()
    # #         print(f"({ja}) [{gid}] {a}")
    # #         a.set_visible(False)

    # # for a in aseq[0]:
    # #     a.set_visible(True)
    # # plt.show()

    if anim:
        anim = animation.ArtistAnimation(fig=fig, artists=aseq,
                                         interval=10, repeat_delay=1000)
        print(anim)

    # fig.canvas.draw()
    # fig.draw_artist(*aseq[0])
    plt.show()


if __name__ == '__main__':
    main(sys.argv[1:])

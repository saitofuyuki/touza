#!/usr/bin/env python3
# Time-stamp: <2024/08/07 10:27:55 fuyuki test_xrnio.py>

import sys
import xarray as xr
import itertools
import matplotlib.pyplot as plt

from zbt.xrnio import *

def main(argv):
    """Sample driver."""
    decode_coords = False
    plot = False
    while argv:
        if argv[0][0] != '-':
            break
        if argv[0] == '-c':
            decode_coords = True
        elif argv[0] == '-P':
            plot = True
        argv = argv[1:]

    for a in argv:
        print(f'##### file: {a}')
        xds = xr.open_dataset(a, decode_coords=decode_coords)
        print('### dataset')
        print(xds)
        for g in [xds]:
            for vn, vv in g.data_vars.items():
                print(f"# var:{vn} {vv.shape} {type(vv)}")
                print(vv)
                if plot:
                    ext = vv.shape[:-2]
                    for x in itertools.product(*tuple(range(n) for n in ext)):
                        # x = x + plane
                        s = ','.join([str(idx) for idx in x])
                        print(f"# plot: {vn}[{s},:,:]")
                        sel = vv[x]

                        fig, ax = plt.subplots()
                        try:
                            sel.plot(ax=ax)
                        except Exception as x:
                            print(x)
                        plt.show()
                        plt.close(fig)


if __name__ == '__main__':
    main(sys.argv[1:])

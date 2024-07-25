#!/usr/bin/env python3
# Time-stamp: <2024/07/25 12:50:13 fuyuki test_plot.py>

import sys
import itertools
import matplotlib.pyplot as plt

from zbt.dsnio import *


def main(argv):
    """Sample driver."""
    plane = (slice(None, None, None), ) * 2
    for a in argv:
        print(f'##### file: {a}')
        ds = TouzaNioDataset(a)
        for grp in ds.groups.values():
            print(f'### group: {grp.name}')
            for var in grp.variables.values():
                print(f"# var:{var.name} {var.shape}")
                ext = var.shape[:-2]
                for x in itertools.product(*tuple(range(n) for n in ext)):
                    # x = x + plane
                    s = ','.join([str(idx) for idx in x])
                    print(f"# plot: {var.name}[{s},:,:]")
                    fig, ax = plt.subplots()
                    sel = var[x]
                    # print(sel[0])
                    CS = ax.contour(sel, levels=16)
                    ax.clabel(CS, inline=True, fontsize=10)
                    plt.show()

if __name__ == '__main__':
    main(sys.argv[1:])

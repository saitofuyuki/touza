#!/usr/bin/env python3
# Time-stamp: <2024/10/16 09:25:31 fuyuki test_dsnio.py>

import os
import sys
import itertools
import matplotlib.pyplot as plt

import netCDF4 as nc4

sys.path.insert(0, os.getcwd())

import zbt.dsnio as znio


def main(argv):
    """Sample driver."""
    test_class = znio.TouzaNioDataset
    diag = False
    show = False
    suite = False
    plot = False
    while argv:
        if argv[0][0] != '-':
            break
        if argv[0] == '-c':
            test_class = znio.TouzaNioCoDataset
        elif argv[0] == '-d':
            diag = True
        elif argv[0] == '-v':
            show = True
        elif argv[0] == '-s':
            suite = True
        elif argv[0] == '-P':
            plot = True
        argv = argv[1:]

    for a in argv:
        print(f'##### file: {a}')
        if test_class.is_nio_file(a):
            ds = test_class(a)
        else:
            ds = nc4.Dataset(a)
            suite = True
        print('### dataset')
        print(ds)
        print('### dimensions')
        for d, dim in ds.dimensions.items():
            print(f'{d}: {str(dim)}')
        print('### groups')
        if suite:
            groups = [ds]
        else:
            groups = ds.groups.values()
            print(ds.groups)
        for g in groups:
            print(f'### group: {g.name}')
            print(g)
            print('### dimensions')
            print(g.dimensions)
            for dn, dd in g.dimensions.items():
                print(f"# dim:{dn}")
                print(dd)
            for vn, vv in g.variables.items():
                # print(f"# var:{vn} {vv.shape} {vv.dimensions_suite()} {vv.dataset.root.handle}")
                print(f"# var:{vn} {vv.shape}")
                print(vv)
                if show:
                    print(vv[:])
                if plot:
                    print(f"# var:{vv.name} {vv.shape}")
                    ext = vv.shape[:-2]
                    if len(vv.shape) >= 2:
                        for x in itertools.product(*tuple(range(n) for n in ext)):
                            # x = x + plane
                            s = ','.join([str(idx) for idx in x])
                            print(f"# plot: {vv.name}[{s},:,:]")
                            fig, ax = plt.subplots()
                            sel = vv[x]
                            CS = ax.contourf(sel, levels=16)
                            ax.clabel(CS, inline=True, fontsize=10)
                            plt.show()
    if diag:
        print()
        print('#' * 10, 'diag')
        znio.diag_datasets()

    ds.close()

if __name__ == '__main__':
    main(sys.argv[1:])

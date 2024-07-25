#!/usr/bin/env python3
# Time-stamp: <2024/07/25 10:34:01 fuyuki test_dsnio.py>

import sys

from zbt.dsnio import *


def main(argv):
    """Sample driver."""
    for a in argv:
        print(f'##### file: {a}')
        ds = TouzaNioDataset(a)
        print('### dataset')
        print(ds)
        print('### dimensions')
        for d, dim in ds.dimensions.items():
            print(f'{d}: {str(dim)}')
        print('### groups')
        print(ds.groups)
        for g in ds.groups.values():
            print(f'### group: {g.name}')
            print(g)
            print('### dimensions')
            print(g.dimensions)
            for dn, dd in g.dimensions.items():
                print(f"# dim:{dn}")
                print(dd)
            for vn, vv in g.variables.items():
                print(f"# var:{vn} {vv.shape}")
                print(vv)
                print(vv[:])


if __name__ == '__main__':
    main(sys.argv[1:])

#!/usr/bin/env python3
# Time-stamp: <2024/08/02 17:56:55 fuyuki test_dsnio.py>

import sys
# import textwrap as tw

from zbt.dsnio import *
from zbt.libtouza import *


def main(argv):
    """Sample driver."""
    test_class = TouzaNioDataset
    diag = False
    show = False
    suite = False
    while argv:
        if argv[0][0] != '-':
            break
        if argv[0] == '-c':
            test_class = TouzaNioCoDataset
        elif argv[0] == '-d':
            diag = True
        elif argv[0] == '-v':
            show = True
        elif argv[0] == '-s':
            suite = True
        argv = argv[1:]

    for a in argv:
        print(f'##### file: {a}')
        ds = test_class(a)
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
                print(f"# var:{vn} {vv.shape} {vv.dimensions_suite}")
                print(vv)
                if show:
                    print(vv[:])
        ds.close()

    if diag:
        print()
        print('#' * 10, 'diag')
        diag_datasets()


if __name__ == '__main__':
    main(sys.argv[1:])

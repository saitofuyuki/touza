#!/usr/bin/env python3
# Time-stamp: <2025/01/22 18:08:46 fuyuki test_curve.py>

import sys
import xarray as xr
import itertools
import pathlib as plib

sys.path.insert(0, str(plib.Path(__file__).parents[1]))

import zbt.xrnio as zxr
import zbt.plot as zplt
import cftime
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def main(args):
    """Test driver."""
    for a in args:
        print(f"open: {a}")
        ds = zxr.open_dataset(a)
        for vk in ds.data_vars:
            fig = plt.figure()
            ax = fig.add_subplot()
            vv = ds[vk]
            shape = vv.shape
            print(f"var: {vk} {shape} {vv.dims}")
            sel = (0, ) * (len(shape) - 2)
            for y in range(8):
                xsel = sel + (y, )
                vsel = vv[xsel]
                vsel.plot.line(ax=ax)
            plt.show()
if __name__ == '__main__':
    import sys
    main(sys.argv[1:])

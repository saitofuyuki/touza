#!/usr/bin/env python3
# Time-stamp: <2024/10/09 16:55:44 fuyuki test_linkedarray.py>

import sys
import math
import argparse as ap
import pathlib as plib
import cProfile
import tomllib as toml
import numbers as nums
import collections.abc as cabc

import xarray as xr
import xarray.plot.utils as xrpu

import matplotlib as mplib
import matplotlib.pyplot as plt
import matplotlib.ticker as mtc

import zbt.layout as zblo
import zbt.util as zu
import zbt.plot as zplt
import zbt.control as zctl

# mplib.use('TkAgg')
# mplib.use('WebAgg')
# mplib.use('nbAgg')
# mplib.use('wxAgg')
mplib.use('Qt5Agg')

def main(argv, cmd=None):
    Array = zctl.ArrayIter()
    Var = zctl.VariableIter(child=Array)
    File = zctl.FileIter(argv, child=Var)
    Plot = zplt.ContourPlot()
    Figs = zctl.FigureControl(Plot, File)
    Figs()

    return

def _driver():
    """Command line driver."""
    main(sys.argv[1:], sys.argv[0])


if __name__ == '__main__':
    _driver()

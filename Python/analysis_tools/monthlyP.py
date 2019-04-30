#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""pmaps
.. module:: genmaps
    :platform: Unix
    :synopis:
.. moduleauther: CEMAC (UoL)
.. description: This module was developed by CEMAC as part of the UNRESP
   Project. This Script plots CALPUFF concrec data on a map.
   :copyright: © 2019 University of Leeds.
   :license: BSD-2 Clause.
Example:
    To use::
        ./genemaps.py <date>
        <date> - Date string, format YYYYMMDD, of the current CALPUFF run.
                 Used to locate directory containing the SO2 output files
                 (with assumed naming convention 'concrec0100**.dat',
                  where '**' goes from '01' through to '48')
.. CEMAC_UNRESPForcastingSystem:
   https://github.com/cemac/UNRESPForcastingSystem
"""
from __future__ import print_function
import argparse
from dateutil.parser import parse
import datetime
from maptoolkit import *


def conc_array_plain(ny, nx, filePaths):
    # Read in concentration data:
    f = open(filePaths, 'r')
    lines = f.read().splitlines()
    f.close
    # Process concentration data into desired format:
    conc = np.array([float(X) for X in lines]) * 100**3  # ug/cm^3 -> ug/m^3
    concAry = np.reshape(conc, (ny, nx))  # Reshape data onto latlon grid
    return concAry, conc


# READ IN COMMAND LINE ARGUMENTS
dstring = ("Date string, format YYYYMMDD, START date and End Date")
parser = argparse.ArgumentParser(description=dstring)
parser.add_argument("--start", help=dstring, type=str)
parser.add_argument("--end", help=dstring, type=str)
args = parser.parse_args()

# Required files and info
start = datetime.datetime.strptime(args.start, "%Y%m%d")
end = datetime.datetime.strptime(args.end, "%Y%m%d")
datahome = "/nfs/see-fs-01_users/earhbu/UNRESP_SPACE/UNRESPForecastingSystem/"
nConcFiles = 48
binLims = [10, 350, 600, 2600, 9000, 14000]  # SO2 bin limits
xyFile = "../../data/xy_masaya.dat"
glat, glon, latMin, latMax, lonMin, lonMax, ny, nx = genxy(xyFile)
datelist = [start + datetime.timedelta(days=x) for x in range(0, (end-start).days)]
nhours = 0
concsum = np.zeros(18000)
threshold = 100
countersum = np.zeros((ny, nx))
concAsum = np.zeros((ny, nx))
concsum = np.zeros(18000)
concsumcount = np.zeros(18000)
for date in datelist:
    month = str(date.strftime("%Y%m"))
    year = str(date.strftime("%Y"))
    try:
        concDir = datahome + "CALPUFF_OUT/CALPUFF/" + year + "/m" + month + '/' + str(date.strftime("%Y%m%d"))
        filenames, filePaths = concfiles(nConcFiles, concDir, SOX='SO2')
    except AssertionError:
        print(concDir + "does not exist skipping")
        continue
    for i, fname in enumerate(filePaths):
        nhours += 1  # number of simulated hours
        concA, concx = conc_array(ny, nx, fname, binLims)
        concsum = concx + concsum
        counter = np.zeros_like(concA)
        counter[concA >= 100] = 1
        concAsum = concAsum + concA
        countersum = counter + countersum
        c = np.zeros(18000)
        c[concx>=100] = 1
        concsumcount = c + concsumcount

# Average and write out
Concav = concsumcount/nhours
with open(year + month +'avP.txt', 'w') as f:
    for item in Concav:
        f.write("%s\n" % item)

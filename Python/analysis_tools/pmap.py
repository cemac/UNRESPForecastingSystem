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
import netCDF4


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
xyFile = "../data/xy_masaya.dat"
glat, glon, latMin, latMax, lonMin, lonMax, ny, nx = genxy(xyFile)
datelist = [start + datetime.timedelta(days=x) for x in range(0, (end-start).days)]
"""
the probability is simply calculated by checking each single hourly file and
"counting" how many time the concentration in each grid point is over a
specific threshold (here 100 micrograms/m3). This number (obtained for each
grid point) is then normalized over the total number of simulated hours."""

threshold = 100

nhours = 0
countersum = np.zeros((ny, nx))
concAsum = np.zeros((ny, nx))
concsum = np.zeros(18000)
concsumcount = np.zeros(18000)
for date in datelist:
    month = str(date.strftime("%Y%m"))
    try:
        concDir = datahome + "CALPUFF_OUT/CALPUFF/m" + month + '/' + str(date.strftime("%Y%m%d"))
        filenames, filePaths = concfiles(nConcFiles, concDir, SOX='SO2')
    except AssertionError:
        print(concDir + "does not exist skipping")
        continue
    for i, fname in enumerate(filePaths):
        nhours += 1  # number of simulated hours
        concA, concx = conc_array(ny, nx, fname, binLims)
        counter = np.zeros_like(concA)
        counter[concA >= 100] = 1
        concAsum = concAsum + concA
        countersum = counter + countersum
        concsum = concx + concsum
        c = np.zeros(18000)
        c[concx>=100] = 1
        concsumcount = c + concsumcount
# Write Netcdf out
f = netCDF4.Dataset('conc.nc', 'w')
f.createDimension('lon', nx)
f.createDimension('lat', ny)
f.createDimension('N', 1)
f.createDimension('points', 18000)
h2 = f.createVariable('count', 'float', ('lon', 'lat'))
h2[:] = countersum
h1 = f.createVariable('conc', 'float', ('lon', 'lat'))
h1[:] = concAsum
h3 = f.createVariable('long', 'float', ('lon'))
h3[:] = glon[0, :]
h4 = f.createVariable('lati', 'float', ('lat'))
h4[:] = glat[:, 0]
h5 = f.createVariable('nhours', 'float', ('N'))
h5[:] = nhours
h6 = f.createVariable('concpoint', 'float', ('points'))
h6[:] = concsum
h7 = f.createVariable('countpoints', 'float', ('points'))
h7[:] = concsumcount
f.close()

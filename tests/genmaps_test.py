#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""genmaps
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
import maptoolkit as mtk
import sys

# Set everything to off
TopoMaps = False
SatelliteMaps = False
GoogleMaps = False
SO2 = False
SO4 = False


# READ IN COMMAND LINE ARGUMENTS
dstring = ("Used to generate a series (48hrs) of static and interactive" +
           "(google) maps \n showing SO2 concentrations around the Masaya " +
           "volcano, as predicted by the CALPUFF dispersion model")
hstring = ("Date string, format YYYYMMDD, of the current CALPUFF run. Used " +
           "to + locate \n  directory containing the SO2 output files (with " +
           "assumed naming convention 'concrec0100**.dat', \n where '**' " +
           " goes from '01' through to '48'")
hstring = ("number of concrec files e.g. 24 or 48")
parser = argparse.ArgumentParser(description=dstring)
parser.add_argument("date", help=hstring, type=str)
parser.add_argument("--conc", help=hstring, type=str)
# Switches
parser.add_argument('--all', help='Plots all types of maps',
                    action='store_true')
parser.add_argument('--SO2', help=r'Plot SO$_2$',
                    action='store_true')
parser.add_argument('--SO4', help=r'Plot SO$_4$'',
                    action='store_true')
parser.add_argument('--topo', help='Turn on basic maps',
                    action='store_true')
parser.add_argument('--satellite', help='Turn on satellite maps',
                    action='store_true')
parser.add_argument('--google', help='Turn on googlemaps',
                    action='store_true')
args = parser.parse_args()

if args.SO2:
    SO2 = True

if args.SO4:
    SO4 = True

if args.satellite:
    SatelliteMaps = True

if args.topo:
    TopoMaps = True

if args.google:
    GoogleMaps = True

if args.all:
    SO2 = True
    SO4 = True
    GoogleMaps = True
    TopoMaps = True
    SatelliteMaps = True


# echo what is being done
print('Generating Maps with following settings:')
print(r"SO$_2$ = ", SO2)
print(r"SO$_4$ = ", SO4)
print("Basic Maps =", TopoMaps)
print("Satallite Maps =", SatelliteMaps)
print("Gooogle Maps =", GoogleMaps)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

date = args.date
if args.conc:
    nconc = args.conc
else:
    nconc = 48

try:
    mpt = mtk.MasayaMaps(date, n_conc_files=int(nconc))
except AssertionError:
    print('CALPUFF output directory does not exist for' + date)
    print('Stopping, please check output exists for ' + date)

if TopoMaps:
    if SO2 and SO4:
        print('topo SO2 and SO4')
        # mpt.plot_staticmaps('topo', SOX='SO2')
        # mpt.plot_staticmaps('topo', SOX='SO4')
    elif SO4:
        print('SO4')
        # mpt.plot_staticmaps('topo', SOX='SO4')
    elif SO2:
        print('SO2')
        # mpt.plot_staticmaps('topo', SOX='SO2')
    else:
        print(r'Conc must be set to SO$_2$ or SO$_4$ or both (default)')
        print(r'most likely the --custom flag has been used with out:')
        print(' --SO2', 'or  --SO4',)

if SatelliteMaps:
    if SO2 and SO4:
        print('satellite SO2 and SO4')
    elif SO2:
        print('sat SO2')
        # mpt.plot_staticmaps('satellite', SOX='SO2')
    elif SO4:
        print('sat SO4')
        # mpt.plot_staticmaps('satellite', SOX='SO4')
    else:
        print(r'Conc must be set to SO$_2$ or SO$_4$ or both (default)')
        print(r'most likely the --custom flag has been used with out:')
        print(' --SO24', ' --SO2', 'or  --SO4',)

if GoogleMaps:
    if SO2 and SO4:
        print('google SO2 and SO4')
    elif SO2:
        print('sat SO2')
        # mpt.plot_google(SOX='SO2')
    elif SO4:
        print('sat SO4')
        # mpt.plot_google(SOX='SO4')
    else:
        print(r'Conc must be set to SO$_2$ and/or SO$_4$ ')
        print(r'most likely the --custom flag has been used with out:')
        print('--SO2', 'or  --SO4',)

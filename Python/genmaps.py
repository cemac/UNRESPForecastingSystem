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

# Manual Flags (will be overwrittend by commandline flags)
StaticMaps = True
GoogleMaps = False
SO24 = True
SO2 = False
SO4 = False
# Not yet Functioning
diffMaps = False
windfield = False
Layers = False
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
parser.add_argument('--layers', help='Plots as raster layers',
                    action='store_true')
parser.add_argument('--custom', help='Allows single switches',
                    action='store_true')
parser.add_argument('--SO24', help=r'Plot both SO$_2$ and SO$_4$',
                    action='store_true')
parser.add_argument('--SO2', help=r'Plot SO$_2$ only',
                    action='store_true')
parser.add_argument('--SO4', help=r'Plot SO$_4$ only',
                    action='store_true')
parser.add_argument('--static', help='Turn on static',
                    action='store_true')
parser.add_argument('--google', help='Turn on googlemaps',
                    action='store_true')
parser.add_argument('--diff', help='Turn on IMO vs UoL',
                    action='store_true')
parser.add_argument('--wind', help='Turn on wind quiverplots (NAM)',
                    action='store_true')
args = parser.parse_args()

if args.SO2:
    SO24 = False
    SO2 = True

if args.all:
    SO24 = True
    StaticMaps = True
    GoogleMaps = True
    diffMaps = True
    windfield = True
    Layers = False
    print('some options selected are not yet available')
    diffMaps = False
    windfield = False

if args.all and args.custom:
    print('--all and --custom can not be used together, setting to custom')

if args.layers:
    Layers = True
    print('some options selected are not yet available')
    Layers = False

# Bespoke options
if args.custom:
    SO24 = False
    StaticMaps = False
    GoogleMaps = False
    diffMaps = False
    windfield = False
    Layers = False

if args.static:
    StaticMaps = True
if args.google:
    GoogleMaps = True
if args.diff:
    diffMaps = True
    print('some options selected are not yet available')
    diffMaps = False
if args.wind:
    windfield = True
    print('some options selected are not yet available')
    windfield = False
# echo what is being done
print('Generating Maps with following settings:')
print("StaticMaps = ", StaticMaps)
print("GooogleMaps =", GoogleMaps)
print(r"Both SO$_2$ and SO$_4$ = ", SO24)
print("IMO vs UoL =", diffMaps)
print("NAM quiver plots", windfield)
print("Plot as raster layers", Layers)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

date = args.date
if args.conc:
    nconc = args.conc
else:
    nconc = 48
mpt = mtk.MasayaMaps(date, n_conc_files=int(nconc))

if StaticMaps:
    if SO24:
        mpt.plot_staticmaps('topo', SOX='SO2')
        mpt.plot_staticmaps('topo', SOX='SO4')
        mpt.plot_staticmaps('satellite', SOX='SO2')
        mpt.plot_staticmaps('satellite', SOX='SO4')
    elif SO2:
        mpt.plot_staticmaps('topo', SOX='SO2')
        mpt.plot_staticmaps('satellite', SOX='SO2')
    elif SO4:
        mpt.plot_staticmaps('topo', SOX='SO4')
        mpt.plot_staticmaps('satellite', SOX='SO4')
    else:
        print(r'Conc must be set to SO$_2$ or SO$_4$ or both (default)')
        print(r'most likely the --custom flag has been used with out:')
        print(' --SO24', ' --SO2', 'or  --SO4',)
if GoogleMaps:
    if SO24:
        mpt.plot_google(SOX='SO2')
        mpt.plot_google(SOX='SO4')
    elif SO2:
        mpt.plot_google(SOX='SO2')
    elif SO4:
        mpt.plot_google(SOX='SO4')
    else:
        print(r'Conc must be set to SO$_2$ or SO$_4$ or both (default)')
        print(r'most likely the --custom flag has been used with out:')
        print(' --SO24', ' --SO2', 'or  --SO4',)

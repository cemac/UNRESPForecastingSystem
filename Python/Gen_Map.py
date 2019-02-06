#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""Gen_Maps
.. module:: Gen_Maps
    :platform: Unix
    :synopis:
.. moduleauther: CEMAC (UoL)
.. description: This module was developed by CEMAC as part of the UNRESP
   Project. This Script plots CALPUFF concrec data on a map.
   :copyright: © 2018 University of Leeds.
   :license: BSD-2 Clause.
Example:
    To use::
        ./generateMaps.py <date>
        <date> - Date string, format YYYYMMDD, of the current CALPUFF run.
                 Used to locate directory containing the SO2 output files
                 (with assumed naming convention 'concrec0100**.dat',
                  where '**' goes from '01' through to '48')
.. CEMAC_UNRESPForcastingSystem:
   https://github.com/cemac/UNRESPForcastingSystem
"""

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
from mpl_toolkits.mplot3d import axes3d
from matplotlib.font_manager import FontProperties
import os
import datetime as dt
import pytz
import utm
import cartopy.crs as ccrs
import cartopy.io.img_tiles as cimgt
import cartopy
import cartopy.feature as cfeat
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER
import netCDF4
from dateutil.parser import parse


# Flags
StaticMaps = True
GoogleMaps = True

# Standalone Functions


def Read_Two_Column_File(file_name):
    with open(file_name, 'r') as data:
        x = []
        y = []
        for line in data:
            p = line.split()
            x.append(float(p[0]))
            y.append(float(p[1]))

    return x, y


def concfiles(nConcFiles, concDir):
    filenames = []
    filePaths = []
    for i in range(nConcFiles):
        s = str('{:02}'.format(i + 1))  # Ensures e.g. '1' is converted to '01'
        fileName = 'concrec0100' + s + '.dat'
        filenames.append(fileName)
        filePath = os.path.join(concDir, fileName)
        filePaths.append(filePath)
        assert os.path.exists(filePath), "File " + \
            filePath + " not found. Check path."
    return filenames, filePaths


def genxy(xyFile):
    x, y = Read_Two_Column_File(xyFile)  # read in x,y data
    xunq, yunq = np.unique(x), np.unique(y)  # get unique x,y coordinates
    nx, ny = len(xunq), len(yunq)  # number of unique x,y coordinates
    # Use utm package to convert from x,y to lat,lon...
    # ...Nicaragua is UTM zone 16P, and we must convert to metres first:
    lat = [utm.to_latlon(x[i] * 1000, y[i] * 1000, 16, 'P')[0]
           for i in np.arange(0, len(x))]
    lon = [utm.to_latlon(x[i] * 1000, y[i] * 1000, 16, 'P')[1]
           for i in np.arange(0, len(x))]
    # Create gridded field of lat,lon of appropriate size:
    glat, glon = np.reshape(lat, (ny, nx)),  np.reshape(lon, (ny, nx))
    # Also grab range for static plots
    latMin = min(lat)
    latMax = max(lat)
    lonMin = min(lon)
    lonMax = max(lon)
    return glat, glon, latMin, latMax, lonMin, lonMax, ny, nx


def genGxy(xyFile):
    # READ IN X,Y DATA AND CONVERT TO LAT,LON
    x, y = Read_Two_Column_File(xyFile)  # read in x,y data
    xunq, yunq = np.unique(x), np.unique(y)  # get unique x,y coordinates
    # Get x,y coordinates of all the corners of the square cells centred on
    # each x,y (for google plots):
    x2unq = [v - (xunq[1] - xunq[0]) / 2. for v in xunq]
    x2unq.append(x2unq[-1] + (xunq[1] - xunq[0]))
    y2unq = [v - (yunq[1] - yunq[0]) / 2. for v in yunq]
    y2unq.append(y2unq[-1] + (yunq[1] - yunq[0]))
    nx, ny = len(x2unq), len(y2unq)
    x2grd, y2grd = np.meshgrid(x2unq, y2unq)
    x2, y2 = np.reshape(x2grd, (nx * ny)), np.reshape(y2grd, (nx * ny))
    lat = [utm.to_latlon(x2[i] * 1000, y2[i] * 1000, 16, 'P')[0]
           for i in np.arange(0, len(x2))]
    lon = [utm.to_latlon(x2[i] * 1000, y2[i] * 1000, 16, 'P')[1]
           for i in np.arange(0, len(x2))]
    glat, glon = np.reshape(lat, (ny, nx)),  np.reshape(lon, (ny, nx))
    return glat, glon, lat, lon, ny, nx


def conc_array(ny, nx, filePaths, dates):
    for fle, dat in zip(filePaths, dates):
        # Read in concentration data:
        f = open(filePaths[30], 'r')
        lines = f.read().splitlines()
        f.close
        # Process concentration data into desired format:
        conc = np.array([float(X) for X in lines]) * 100**3  # ug/cm^3 -> ug/m^3
        concAry = np.reshape(conc, (ny, nx))  # Reshape data onto latlon grid
    return concAry

# READ IN COMMAND LINE ARGUMENTS
parser = argparse.ArgumentParser(description="Used to generate a series (48hrs) of static and interactive (google) maps \
         showing SO2 concentrations around the Masaya volcano, as predicted by the CALPUFF dispersion model")
parser.add_argument("date", help="Date string, format YYYYMMDD, of the current CALPUFF run. Used to locate \
                    directory containing the SO2 output files (with assumed naming convention 'concrec0100**.dat', \
                    where '**' goes from '01' through to '48'", type=str)
args = parser.parse_args()
date = args.date


class Masaya_Maps(object):

    def _init_(s, date, generateStaticMaps=StaticMaps,
               generateGoogleMaps=GoogleMaps):
        concDir = "../CALPUFF_OUT/CALPUFF/" + date
        xyFile = "../data/xy_masaya.dat"
        outDir = "../vis/" + date
        nConcFiles = 48  # Number of conc files to process (48 = full 2 days)
        s.binLims = [10, 350, 600, 2600, 9000, 14000]  # SO2 bin limits
        s.colsHex = ['#FFFFFF', '#0cec0c', '#FFFF00', '#FF6600', '#FF0000',
                     '#800080', '#8F246B']  # Hex codes for SO2 colour bins
        s.towns = (' El Panama', ' Rigoberto', ' Pacaya', ' El Crucero',
                   ' La Concepcion', ' Masaya', ' San Marcos',
                   ' San Rafael del Sur', ' Diriamba', ' Jinotepe',
                   ' Masatepe')
        s.townCoords = ((-86.2058, 11.972), (-86.2021, 11.9617),
                        (-86.3013, 11.9553), (-86.3113, 11.9923),
                        (-86.189772, 11.936161), (-86.096053, 11.973523),
                        (-86.20317, 11.906584), (-86.43639, 11.847034),
                        (-86.239592, 11.85632), (-86.19993, 11.85017),
                        (-86.143758, 11.91512))
        s.cities = (' MANAGUA',)
        s.cityCoords = ((-86.29, 12.12),)
        s.volcCoords = (-86.1608, 11.9854)
        s.so2title = ('Atmospheric SO$_{2}$ concentrations at ground level' +
                      ' (hourly means). \n GCRF UNRESP')
        s.so4title = ('Atmospheric SO$_{4}$ concentrations at ground level' +
                      ' (hourly means). \n GCRF UNRESP')
        s.font = FontProperties()
        s.font.set_weight('bold')
        s.font.set_family('monospace')

        # CHECK PATHS/FILES EXIST
        assert os.path.exists(
            concDir), "CALPUFF output directory does not exist for this date."
        assert os.path.exists(
            xyFile), "Cannot find data/xy_masaya.dat coordinate data file."
        assert os.path.exists(outDir), "Output directory vis/<date> does not exist."
        filenames, filePaths = concfiles(nConcFiles, concDir)

        # GET DATES/TIMES
        startDate = pytz.utc.localize(parse(date))
        dates = []
        for i in range(nConcFiles):
            iDate = startDate + dt.timedelta(hours=i + 1)
            dates.append(iDate)

        # SET BIN COLOURS
        s.cmap = mpl.colors.ListedColormap(colsHex[1:-1])
        s.cmap.set_under(colsHex[0])
        s.cmap.set_over(colsHex[-1])
        s.norm = mpl.colors.BoundaryNorm(boundaries=binLims, ncolors=5)
        #####

        s.glat, s.glon, s.latMin, s.latMax, s.lonMin, s.lonMax, s.ny, s.nx =  genxy(xyFile)

        def plot_googlemaps(s, concA, xyFile):

            if generateGoogleMaps:
                codesFile = os.path.join('GM_API_KEY.txt')
                gmstring = ("Can't find file GM_API_KEY.txt in same" +
                            " directory as python script")
                assert os.path.exists(codesFile), gmstring
                glat, glon, lat, lon, ny, nx= genGxy(xyFile)
                f = open(codesFile, 'r')
                lines = f.readlines()
                f.close()
                googlekey = lines[0].strip()
                gmap = gmplot.GoogleMapPlotter(min(lat) + np.ptp(lat) / 2.,
                                               min(lon) + np.ptp(lon) / 2., zoom=11,
                                               apikey=googlekey)
                for i in np.arange(0, nx):
                    for j in np.arange(0, ny):
                        for k in np.arange(0, len(s.binLims) - 1):
                            if concA[j, i] > s.binLims[k] and concA[j, i] <= s.binLims[k + 1]:
                                gmap.polygon((glat[j + 1, i], glat[j, i],
                                              glat[j, i + 1], glat[j + 1, i + 1]),
                                             (glon[j + 1, i], glon[j, i],
                                              glon[j, i + 1], glon[j + 1, i + 1]),
                                             color=colsHex[k + 1], edge_width=0.001)
                        if conc[j] > s.binLims[-1]:
                            gmap.polygon((glat[j + 1, i], glat[j, i],
                                          glat[j, i + 1], glat[j + 1, i + 1]),
                                         (glon[j + 1, i], glon[j, i],
                                          glon[j, i + 1], glon[j + 1, i + 1]),
                                         color=colsHex[-1], edge_width=0.001)
                HTMLfile = 'google_' + fle[-17:-4] + '.html'
                print("Writing out file " + HTMLfile)
                gmap.draw(os.path.join(outDir, HTMLfile))

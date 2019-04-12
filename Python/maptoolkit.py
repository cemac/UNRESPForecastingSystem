#!/usr/bin/env python
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
import matplotlib.image as mpimg
from mpl_toolkits.mplot3d import axes3d
import matplotlib as mpl
from mpl_toolkits.basemap import Basemap
from matplotlib.font_manager import FontProperties
import os
import sys
import datetime as dt
import pytz
import utm
import gmplot
from dateutil.parser import parse
# University System python may be broken
# If some one insists on using it...
backend = mpl.get_backend()
if backend == 'Qt4Agg' and sys.version_info[0] == 2:
    # Fix the backend
    print('swapping to Agg Backend')
    print('Please consider using anaconda')
    mpl.use('Agg')
# DO NOT MOVE ABOVE BACKEND FIX
import matplotlib.pyplot as plt  # KEEP ME HERE!!!
################################


def Read_Two_Column_File(file_name):
    with open(file_name, 'r') as data:
        x = []
        y = []
        for line in data:
            p = line.split()
            x.append(float(p[0]))
            y.append(float(p[1]))

    return x, y


def concfiles(nConcFiles, concDir, SOX='SO2'):
    filenames = []
    filePaths = []
    if SOX == 'SO2':
        concrecx = 'concrec0100'
    elif SOX == 'SO4':
        concrecx = 'concrec0200'
    else:
        concrecx = 'concrec0100'
        print("WARNING: SOX option not valid setting to 'SO2'")
        print("Options available are 'SO2' or 'SO4'")
    for i in range(nConcFiles):
        s = str('{:02}'.format(i + 1))  # Ensures e.g. '1' is converted to '01'
        fileName = concrecx + s + '.dat'
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


def conc_array(ny, nx, filePaths, binLims):
    # Read in concentration data:
    f = open(filePaths, 'r')
    lines = f.read().splitlines()
    f.close
    # Process concentration data into desired format:
    conc = np.array([float(X) for X in lines]) * 100**3  # ug/cm^3 -> ug/m^3
    concAry = np.reshape(conc, (ny, nx))  # Reshape data onto latlon grid
    concA = np.ma.masked_array(concAry, concAry < binLims[0])
    return concA, conc


def gen_im(lonMin, latMin, lonMax, latMax, imtype="World_Imagery",):
    """imtype='World_Imagery' or imtype='World_Shaded_Relief'
    """
    xpixels = 1700  # Zoom lvl for satellite basemap (higher=bigger file sizes)
    bmap = Basemap(llcrnrlon=lonMin, llcrnrlat=latMin,
                   urcrnrlon=lonMax, urcrnrlat=latMax)
    esri_url = \
        "http://server.arcgisonline.com/ArcGIS/rest/services/" + imtype + "/MapServer/export?\
bbox=%s,%s,%s,%s&\
bboxSR=%s&\
imageSR=%s&\
size=%s,%s&\
dpi=%s&\
format=png32&\
f=image" %\
        (bmap.llcrnrlon, bmap.llcrnrlat, bmap.urcrnrlon, bmap.urcrnrlat,
         bmap.epsg, bmap.epsg, xpixels, bmap.aspect * xpixels, 96)
    ESRIimg = mpimg.imread(esri_url)
    return ESRIimg


class Masaya_Maps(object):
    '''Plot Masaya Maps
    Consitsts of X plotting functions that output 48 static maps
    members:
        plot_staticmap: plot either topo or statellite
        plot_googlemaps: plot google maps html
        plot_diff: comming soon
        plot_NAMWIND: comming soon
        plot_CAL_WIND_IN: comming soon
        plot_CAL_WIND_OUT: comming soon

    '''
    def __init__(s, date):
        """Initialise with date

        Args:
            date (str): YYYYMMDD string
        """
        s.concDir = "../CALPUFF_OUT/CALPUFF/" + date
        s.xyFile = "../data/xy_masaya.dat"
        s.outDir = "../vis/" + date
        s.sat = 'World_Imagery'
        s.topo = 'World_Shaded_Relief'
        s.nConcFiles = 48  # Number of conc files to process (48 = full 2 days)
        s.binLims = [10, 350, 600, 2600, 9000, 14000]  # SO2 bin limits
        s.binLimsSO4 = [1E-8, 12, 35, 55, 150, 250]  # SO4 bin limits from:
        # http://mkwc.ifa.hawaii.edu/vmap/hysplit/
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
        s.font = FontProperties()
        s.font.set_weight('bold')
        s.font.set_family('monospace')

        # CHECK PATHS/FILES EXIST
        assert os.path.exists(
            s.concDir), "CALPUFF output directory does not exist for this date."
        assert os.path.exists(
            s.xyFile), "Cannot find data/xy_masaya.dat coordinate data file."
        assert os.path.exists(s.outDir), "Output directory vis/<date> does not exist."
        s.filenames, s.filePaths = concfiles(s.nConcFiles,
                                             s.concDir, SOX='SO2')

        # GET DATES/TIMES
        startDate = pytz.utc.localize(parse(date))
        dates = []
        for i in range(s.nConcFiles):
            iDate = startDate + dt.timedelta(hours=i + 1)
            dates.append(iDate)
        s.dates = dates
        # SET BIN COLOURS
        s.cmap = mpl.colors.ListedColormap(s.colsHex[1:-1])
        s.cmap.set_under(s.colsHex[0])
        s.cmap.set_over(s.colsHex[-1])
        s.normso4 = mpl.colors.BoundaryNorm(boundaries=s.binLimsSO4, ncolors=5)
        s.norm = mpl.colors.BoundaryNorm(boundaries=s.binLims, ncolors=5)
        s.glat, s.glon, s.latMin, s.latMax, s.lonMin, s.lonMax, s.ny, s.nx = genxy(s.xyFile)
        s.Gglat, s.Gglon, s.Glat, s.Glon, s.Gny, s.Gnx = genGxy(s.xyFile)

    def plot_staticmaps(s, maptype, SOX='SO2'):
        """loop through and plot all static maps
        """
        if maptype == 'satellite':
            Imflag = s.sat
            tc = 'w'
            out = ''
        elif maptype == 'topo':
            Imflag = s.topo
            tc = 'k'
            out = 'topo'
        else:
            print('Not a valid option... setting to topo')
            Imflag = s.topo
            tc = 'k'
            out = 'topo'
        im = gen_im(s.lonMin, s.latMin, s.lonMax, s.latMax, imtype=Imflag)
        filenames, filePaths = concfiles(s.nConcFiles, s.concDir, SOX=SOX)
        for i, fname in enumerate(s.filePaths):
            s.plot_staticmap1(i, im, tc, filePaths, out, SOX=SOX)

    def plot_staticmap1(s, ita, im, tc, filePaths, out, SOX):
        """Plot static maps
        """
        SOXf = r'SO$_' + SOX[-1] + '$'
        so2title = ('Atmospheric ' + SOXf + ' concentrations at ' +
                    'ground level (hourly means). \n GCRF UNRESP')
        plt.figure(figsize=(16, 12))
        fle = filePaths[ita]
        if SOX == "SO4":
            binLims = s.binLimsSO4
            norm = s.normso4
        else:
            binLims = s.binLims
            norm = s.norm
        concA, concx = conc_array(s.ny, s.nx, fle, binLims)
        latMin, latMax, lonMin = s.latMin, s.latMax, s.lonMin
        lonMax = s.lonMax
        bmap = Basemap(llcrnrlon=lonMin, llcrnrlat=latMin,
                       urcrnrlon=lonMax, urcrnrlat=latMax)
        bmap.imshow(im, origin='upper')
        bmap.pcolormesh(s.glon, s.glat, concA,
                        norm=norm, cmap=s.cmap, alpha=0.5)
        cbar = bmap.colorbar(location='bottom', pad='20%', cmap=s.cmap,
                             norm=norm, boundaries=[0.] + binLims
                             + [100000.], extend='both', extendfrac='auto',
                             ticks=binLims, spacing='uniform')
        cbar.ax.set_xticklabels(['v low', 'low', 'moderate', 'mod high',
                                 'high', 'v high'])  # horizontal colorbar
        cbar.set_label(label=(SOX + ' concentration'), fontsize=18)
        cbar.ax.tick_params(labelsize=16)
        cbar.solids.set(alpha=1)
        latTicks = np.arange(round(latMin, 1), round(latMax, 1) + 0.1, 0.1)
        lonTicks = np.arange(round(lonMin, 1), round(lonMax, 1) + 0.1, 0.2)
        bmap.drawparallels(latTicks, labels=[1, 0, 0, 0], linewidth=0.0,
                           fontsize=16)
        bmap.drawmeridians(lonTicks, labels=[0, 0, 0, 1], linewidth=0.0,
                           fontsize=16)
        for i, town in enumerate(s.towns):
            plt.plot(s.townCoords[i][0], s.townCoords[i]
                     [1], 'ok', markersize=4)
            plt.text(s.townCoords[i][0], s.townCoords[i][1], town,
                     color=tc, fontproperties=s.font, fontsize=12)
        for i, city in enumerate(s.cities):
            plt.plot(s.cityCoords[i][0], s.cityCoords[i]
                     [1], 'sk', markersize=6)
            plt.text(s.cityCoords[i][0], s.cityCoords[i][1], city,
                     fontproperties=s.font, fontsize=16)
        font0 = FontProperties()
        font0.set_family('monospace')
        plt.plot(s.volcCoords[0], s.volcCoords[1], '^r', markersize=6)
        plt.suptitle(so2title, fontsize=24)
        plt.title(s.dates[ita].strftime('%c'), fontsize=18)
        PNGfile = SOX + '_static_' + out + fle[-17:-4] + '.png'
        print("Writing out file " + PNGfile)
        PNGpath = os.path.join(s.outDir, PNGfile)
        plt.savefig(PNGpath, dpi=250)
        plt.close()

    def plot_google(s, SOX='SO2'):
        """loop through and plot all static maps
        """
        codesFile = os.path.join('GM_API_KEY.txt')
        gmstring = ("Can't find file GM_API_KEY.txt in same" +
                    " directory as python script")
        try:
            f = open(codesFile, 'r')
            lines = f.readlines()
            f.close()
            s.googlekey = lines[0].strip()
            filenames, filePaths = concfiles(s.nConcFiles, s.concDir, SOX=SOX)
            for i, fname in enumerate(filePaths):
                s.plot_googlemap1(i, filePaths, SOX)
        except FileNotFoundError:
            print("### WARNING #### GM_API_KEY.txt not found \n turning off google maps plotter")
            print(" If you would like to plot goolge maps \n please see README" +
                  " for API key information.")

    def plot_googlemap1(s, ita, filePaths, SOX):
        """plot goolemaps
        """
        gKey = s.googlekey
        fle = filePaths[ita]
        if SOX == "SO4":
            binLims = s.binLimsSO4
            norm = s.normso4
        else:
            binLims = s.binLims
            norm = s.norm
        glat, glon, lat, lon = s.Gglat, s.Gglon, s.Glat, s.Glon
        concA, conc = conc_array(s.ny, s.nx, fle, binLims)
        gmap = gmplot.GoogleMapPlotter(min(lat) + np.ptp(lat) / 2.,
                                       min(lon) + np.ptp(lon) / 2., zoom=11,
                                       apikey=gKey)
        for i in np.arange(0, s.nx):
            for j in np.arange(0, s.ny):
                for k in np.arange(0, len(s.binLims) - 1):
                    if concA[j, i] > s.binLims[k] and concA[j, i] <= binLims[k + 1]:
                        gmap.polygon((glat[j + 1, i], glat[j, i],
                                      glat[j, i + 1], glat[j + 1, i + 1]),
                                     (glon[j + 1, i], glon[j, i],
                                      glon[j, i + 1], glon[j + 1, i + 1]),
                                     color=s.colsHex[k + 1], edge_width=0.001)
                if conc[j] > binLims[-1]:
                    gmap.polygon((glat[j + 1, i], glat[j, i],
                                  glat[j, i + 1], glat[j + 1, i + 1]),
                                 (glon[j + 1, i], glon[j, i],
                                  glon[j, i + 1], glon[j + 1, i + 1]),
                                 color=s.colsHex[-1], edge_width=0.001)
        HTMLfile = SOX + '_google_' + fle[-17:-4] + '.html'
        print("Writing out file " + HTMLfile)
        gmap.draw(os.path.join(s.outDir, HTMLfile))

    def plot_diff(s):
        """
        """
        print('This feature does not exist yet')

    def plot_NAMWIND(s):
        """
        """
        print('This feature does not exist yet')

    def plot_CAL_WIND_IN(s):
        """
        """
        print('This feature does not exist yet')

    def plot_CAL_WIND_OUT(s):
        """
        """
        print('This feature does not exist yet')

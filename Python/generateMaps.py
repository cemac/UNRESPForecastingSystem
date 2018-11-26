#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Script name: generateMaps.py
Author: JO'N, CEMAC (University of Leeds)
Date: March 2018
Purpose: Used to generate a series (48hrs) of static and interactive (google)
         maps showing SO2 concentrations around the
         Masaya volcano, as predicted by the CALPUFF dispersion model
Usage: ./generateMaps.py <date>
        <date> - Date string, format YYYYMMDD, of the current CALPUFF run.
                 Used to locate directory containing the SO2 output files
                 (with assumed naming convention 'concrec0100**.dat',
                  where '**' goes from '01' through to '48')
Output: vis/<date>/static_concrec0100**.png and
        vis/<date>/google_concrec0100**.html
        - A series of static and interactive maps of the SO2 plume

.. CEMAC_UNRESP:
   https://github.com/cemac/UNRESPForecastingSystem
"""

import numpy as np
import matplotlib as mpl
from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
from matplotlib.font_manager import FontProperties
import os
import utm
import datetime as dt
import pytz
from dateutil.parser import parse
import argparse
import gmplot
mpl.use('Agg')


def Read_Two_Column_File(file_name):
    with open(file_name, 'r') as data:
        x = []
        y = []
        for line in data:
            p = line.split()
            x.append(float(p[0]))
            y.append(float(p[1]))

    return x, y

# READ IN COMMAND LINE ARGUMENTS
parser = argparse.ArgumentParser(description = "Used to generate a series (48hrs) of static and interactive (google) maps \
         showing SO2 concentrations around the Masaya volcano, as predicted by the CALPUFF dispersion model")
parser.add_argument("date", help="Date string, format YYYYMMDD, of the current CALPUFF run. Used to locate \
                    directory containing the SO2 output files (with assumed naming convention 'concrec0100**.dat', \
                    where '**' goes from '01' through to '48'", type=str)
args = parser.parse_args()
date = args.date
#####

# PARAMETERS
generateStaticMaps = True
generateGoogleMaps = True
concDir = "../CALPUFF_OUT/CALPUFF/"+date
xyFile = "../data/xy_masaya.dat"
outDir = "../vis/"+date
nConcFiles = 48  # Number of conc files to process (48 = full 2 days)
binLims = [10, 350, 600, 2600, 9000, 14000]  # SO2 bin limits
colsHex = ['#FFFFFF', '#0cec0c', '#FFFF00', '#FF6600', '#FF0000', '#800080',
           '#8F246B']  # Hex codes for SO2 colour bins
xpixels = 1700  # Zoom lvel for satellite basemap (higher=bigger file sizes)
towns = (' El Panama', ' Rigoberto', ' Pacaya', ' El Crucero',
         ' La Concepcion', ' Masaya', ' San Marcos',
         ' San Rafael del Sur', ' Diriamba', ' Jinotepe', ' Masatepe')
townCoords = ((-86.2058, 11.972), (-86.2021, 11.9617), (-86.3013, 11.9553),
              (-86.3113, 11.9923), (-86.189772, 11.936161),
              (-86.096053, 11.973523), (-86.20317, 11.906584),
              (-86.43639, 11.847034), (-86.239592, 11.85632),
              (-86.19993, 11.85017), (-86.143758, 11.91512))
cities = (' MANAGUA',)
cityCoords = ((-86.29, 12.12),)
volcCoords = (-86.1608, 11.9854)
so2title = 'Atmospheric SO2 concentrations at ground level (hourly means). \n GCRF UNRESP'
#####

# CHECK PATHS/FILES EXIST
assert os.path.exists(concDir), "CALPUFF output directory does not exist for this date."
assert os.path.exists(xyFile), "Cannot find data/xy_masaya.dat coordinate data file."
assert os.path.exists(outDir), "Output directory vis/<date> does not exist."
filenames = []
filePaths = []
for i in range(nConcFiles):
    s = str('{:02}'.format(i+1))  # Ensures e.g. '1' is converted to '01'
    fileName = 'concrec0100'+s+'.dat'
    filenames.append(fileName)
    filePath = os.path.join(concDir, fileName)
    filePaths.append(filePath)
    assert os.path.exists(filePath), "File "+filePath+" not found. Check path."
#####

# GET DATES/TIMES
startDate = pytz.utc.localize(parse(date))
dates = []
for i in range(nConcFiles):
    iDate = startDate+dt.timedelta(hours=i+1)
    dates.append(iDate)
#####

# READ IN X,Y DATA AND CONVERT TO LAT,LON
x, y = Read_Two_Column_File(xyFile)  # read in x,y data
xunq, yunq = np.unique(x), np.unique(y)  # get unique x,y coordinates
nx, ny = len(xunq), len(yunq)  # number of unique x,y coordinates
# Use utm package to convert from x,y to lat,lon...
# ...Nicaragua is UTM zone 16P, and we must convert to metres first:
lat = [utm.to_latlon(x[i]*1000,y[i]*1000,16,'P')[0] for i in np.arange(0,len(x))]
lon = [utm.to_latlon(x[i]*1000,y[i]*1000,16,'P')[1] for i in np.arange(0,len(x))]
# Create gridded field of lat,lon of appropriate size:
glat, glon = np.reshape(lat, (ny, nx)),  np.reshape(lon, (ny, nx))
# Also grab range for static plots
latMin = min(lat)
latMax = max(lat)
lonMin = min(lon)
lonMax = max(lon)
# Get x,y coordinates of all the corners of the square cells centred on
# each x,y (for google plots):
x2unq = [v-(xunq[1]-xunq[0])/2. for v in xunq]
x2unq.append(x2unq[-1]+(xunq[1]-xunq[0]))
y2unq = [v-(yunq[1]-yunq[0])/2. for v in yunq]
y2unq.append(y2unq[-1]+(yunq[1]-yunq[0]))
nx2, ny2 = len(x2unq), len(y2unq)
x2grd, y2grd = np.meshgrid(x2unq, y2unq)
x2, y2 = np.reshape(x2grd, (nx2*ny2)), np.reshape(y2grd, (nx2*ny2))
lat2 = [utm.to_latlon(x2[i]*1000,y2[i]*1000,16,'P')[0] for i in np.arange(0,len(x2))]
lon2 = [utm.to_latlon(x2[i]*1000,y2[i]*1000,16,'P')[1] for i in np.arange(0,len(x2))]
glat2, glon2 = np.reshape(lat2, (ny2, nx2)),  np.reshape(lon2, (ny2, nx2))
#####

# SET BIN COLOURS
cmap = mpl.colors.ListedColormap(colsHex[1:-1])
cmap.set_under(colsHex[0])
cmap.set_over(colsHex[-1])
norm = mpl.colors.BoundaryNorm(boundaries=binLims, ncolors=5)
#####

# PLOT
plt.ioff()  # turn off interactive plotting
if generateStaticMaps:
    # Download ESRI image only once:
    bmap = Basemap(llcrnrlon=lonMin, llcrnrlat=latMin, urcrnrlon=lonMax, urcrnrlat=latMax)
    esri_url = \
    "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/export?\
bbox=%s,%s,%s,%s&\
bboxSR=%s&\
imageSR=%s&\
size=%s,%s&\
dpi=%s&\
format=png32&\
f=image" %\
(bmap.llcrnrlon, bmap.llcrnrlat, bmap.urcrnrlon, bmap.urcrnrlat, bmap.epsg, bmap.epsg, xpixels, bmap.aspect*xpixels,96)
    ESRIimg = mpimg.imread(esri_url)
    esri_url2 = \
        "http://server.arcgisonline.com/ArcGIS/rest/services/World_Shaded_Relief/MapServer/export?\
bbox=%s,%s,%s,%s&\
bboxSR=%s&\
imageSR=%s&\
size=%s,%s&\
dpi=%s&\
format=png32&\
f=image" %\
(bmap.llcrnrlon, bmap.llcrnrlat, bmap.urcrnrlon, bmap.urcrnrlat, bmap.epsg, bmap.epsg, xpixels, bmap.aspect*xpixels,96)
    ESRIimg2 = mpimg.imread(esri_url2)
for j, file in enumerate(filePaths):
    # Read in concentration data:
    f = open(file, 'r')
    lines = f.read().splitlines()
    f.close
    # Process concentration data into desired format:
    conc = np.array([float(X) for X in lines])*100**3  # ug/cm^3 -> ug/m^3
    concAry = np.reshape(conc, (ny, nx))  # Reshape data onto latlon grid
    concMask = np.ma.masked_array(concAry, concAry<binLims[0])  # apply mask to all concs below lower limit
    # Plot static maps:
    if generateStaticMaps:
        plt.figure(figsize=(16, 12))
        bmap = Basemap(llcrnrlon=lonMin, llcrnrlat=latMin, urcrnrlon=lonMax, urcrnrlat=latMax)
        bmap.imshow(ESRIimg, origin='upper')
        bmap.pcolormesh(glon, glat, concMask, norm=norm, cmap=cmap, alpha=0.5)
        cbar = bmap.colorbar(location='bottom', pad='20%', cmap=cmap, norm=norm, boundaries=[0.] + binLims + [100000.],
                             extend='both', extendfrac='auto', ticks=binLims,
                             spacing='uniform')
        cbar.set_label(label=r'SO$_{2}$ concentration (ug/m$^{3}$)', fontsize=18)
        cbar.ax.tick_params(labelsize=16)
        cbar.solids.set(alpha=1)
        latTicks = np.arange(round(latMin, 1), round(latMax, 1)+0.1, 0.1)
        lonTicks = np.arange(round(lonMin, 1), round(lonMax, 1)+0.1, 0.2)
        bmap.drawparallels(latTicks, labels=[1, 0, 0, 0], linewidth=0.0, fontsize=16)
        bmap.drawmeridians(lonTicks, labels=[0, 0, 0, 1], linewidth=0.0, fontsize=16)
        font = FontProperties()
        font.set_weight('bold')
        #font.set_family('sans-serif')
        font.set_family('monospace')
        for i, town in enumerate(towns):
            plt.plot(townCoords[i][0], townCoords[i][1], 'ok', markersize=4)
            plt.text(townCoords[i][0], townCoords[i][1], town, color='white', fontproperties=font, fontsize=12)
        for i, city in enumerate(cities):
            plt.plot(cityCoords[i][0], cityCoords[i][1], 'sk', markersize=6)
            plt.text(cityCoords[i][0], cityCoords[i][1], city, fontproperties=font, fontsize=16)
        font0 = FontProperties()
        font0.set_family('monospace')
        plt.text(-86.76, 11.73, 'Generated by CEMAC (UoL)', color='white', fontsize=14, fontproperties=font0)
        plt.plot(volcCoords[0], volcCoords[1], '^r', markersize=6)
        plt.suptitle(so2title, fontsize=24)
        plt.title(dates[j].strftime('%c'), fontsize=18)
        PNGfile = 'static_' + file[-17:-4] + '.png'
        print("Writing out file "+PNGfile)
        PNGpath = os.path.join(outDir, PNGfile)
        plt.savefig(PNGpath, dpi=250)
        plt.close()

        plt.figure(figsize=(16, 12))
        bmap = Basemap(llcrnrlon=lonMin, llcrnrlat=latMin, urcrnrlon=lonMax, urcrnrlat=latMax)
        bmap.imshow(ESRIimg2, origin='upper')
        bmap.pcolormesh(glon, glat, concMask, norm=norm, cmap=cmap, alpha=0.5)
        cbar = bmap.colorbar(location='bottom', pad='20%', cmap=cmap, norm=norm, boundaries=[0.] + binLims + [100000.],
                             extend='both', extendfrac='auto', ticks=binLims,
                             spacing='uniform')
        cbar.set_label(label=r'SO$_{2}$ concentration (ug/m$^{3}$)', fontsize=18)
        cbar.ax.tick_params(labelsize=16)
        cbar.solids.set(alpha=1)
        latTicks = np.arange(round(latMin, 1), round(latMax, 1)+0.1, 0.1)
        lonTicks = np.arange(round(lonMin, 1), round(lonMax, 1)+0.1, 0.2)
        bmap.drawparallels(latTicks, labels=[1, 0, 0, 0], linewidth=0.0, fontsize=16)
        bmap.drawmeridians(lonTicks, labels=[0, 0, 0, 1], linewidth=0.0, fontsize=16)
        font = FontProperties()
        font.set_weight('bold')
        #font.set_family('sans-serif')
        font.set_family('monospace')
        for i, town in enumerate(towns):
            plt.plot(townCoords[i][0], townCoords[i][1], 'ok', markersize=4)
            plt.text(townCoords[i][0], townCoords[i][1], town, color='black', fontproperties=font, fontsize=12)
        for i, city in enumerate(cities):
            plt.plot(cityCoords[i][0], cityCoords[i][1], 'sk', markersize=6)
            plt.text(cityCoords[i][0], cityCoords[i][1], city, fontproperties=font, fontsize=16)
        font0 = FontProperties()
        font0.set_family('monospace')
        plt.plot(volcCoords[0], volcCoords[1], '^r', markersize=6)
        plt.suptitle(so2title, fontsize=24)
        plt.title(dates[j].strftime('%c'), fontsize=18)
        PNGfile = 'static_topo' + file[-17:-4] + '.png'
        print("Writing out file "+PNGfile)
        PNGpath = os.path.join(outDir, PNGfile)
        plt.savefig(PNGpath, dpi=250)
        plt.close()
    # Plot google maps:
    '''

    if generateGoogleMaps:
        gmap = gmplot.GoogleMapPlotter(min(lat)+np.ptp(lat)/2.,min(lon)+np.ptp(lon)/2.,zoom=11)
        for i in np.arange(0, nx):
            for j in np.arange(0, ny):
                for k in np.arange(0, len(binLims)-1):
                    if concAry[j, i] > binLims[k] and concAry[j, i] <= binLims[k+1]:
                        gmap.polygon((glat2[j+1,i],glat2[j,i],glat2[j,i+1],glat2[j+1,i+1]),
                                      (glon2[j+1,i],glon2[j,i],glon2[j,i+1],glon2[j+1,i+1]),
                                      color=colsHex[k+1],edge_width=0.001)
                if conc[j] > binLims[-1]:
                    gmap.polygon((glat2[j+1,i],glat2[j,i],glat2[j,i+1],glat2[j+1,i+1]),
                                      (glon2[j+1,i],glon2[j,i],glon2[j,i+1],glon2[j+1,i+1]),
                                      color=colsHex[-1],edge_width=0.001)
        HTMLfile = 'google_' + file[-17:-4] + '.html'
        print("Writing out file "+HTMLfile)
        gmap.draw(os.path.join(outDir, HTMLfile))
plt.ion()  # turn on interactive ploting
'''

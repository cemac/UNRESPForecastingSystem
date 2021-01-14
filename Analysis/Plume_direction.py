#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Timeseries Analysis
.. module:: Plume Direction
    :platform: Unix
    :synopis:
.. moduleauther: CEMAC (UoL)
.. description: This module was developed by CEMAC as part of the UNRESP
   Project. This script takes CALPUFF concrec data from 2 models and compares
   with observations.
   :copyright: © 2019 University of Leeds.
   :license: BSD-2 Clause.
Example:
    To use::
     coming soon
.. CEMAC_UNRESPForcastingSystem:
   https://github.com/cemac/UNRESPForcastingSystem
"""
import os
import glob
import matplotlib as mpl
import pandas as pd
import warnings
import utm
import numpy as np
import matplotlib.pyplot as plt
from datetime import datetime
import maptoolkit as mtk
from mpl_toolkits.basemap import Basemap
from matplotlib.font_manager import FontProperties
import math
import matplotlib.image as mpimg
warnings.filterwarnings("ignore")
# University System python may be broken
# If some one insists on using it...
BACKEND = mpl.get_backend()
if BACKEND == 'Qt4Agg' and sys.version_info[0] == 2:
    # Fix the backend
    print('swapping to Agg Backend')
    mpl.pyplot.switch_backend('Agg')

# --------------------------------------------------------------------------- #
#            Data information: Emission Type, Stations, Model runs            #
#                                                                             #
# --------------------------------------------------------------------------- #

# Default location of xy ascii file
XYFILE = "../data/xy_masaya.dat"
# observations

# stations and coordinates
Airport= ['Airport', (-86.2058, 11.972)]
volcano= ['masaya', (-86.1608, 11.9854)]
# Models
ECMWF = '../ECMWF'
NAM = '../CALPUFF_OUT'
Tripomi = 'plume.csv'
# Unit Testing
Stage1 = False
Stage2 = False
Stage3 = False
Stage4 = False


# --------------------------------------------------------------------------- #
#                   Stage 1: Extract Observational data                       #
#                                                                             #
# --------------------------------------------------------------------------- #


def ExtractTropomi(csvfile):
    """ExtractTTropomi
    Description
    Args:
        obs(str): path to observation csvs
        station(str): station name string
        Em(str): Emmission SO2 only for now
    Returns:
        TS_raw(DataFrame): Raw data full of nans
        unit(str): units of data e.g. ug/m3
        TS_KNN(DataFrame): Missing values predicted by KNN
    """
    # Reading and use datetime index
    # Tell it the weird format...
    df = pd.read_csv(csvfile)
    #Plumedir=df[df['date'].isin(Dateselec)]
    Plumedir=df[['date','UTC_start','UTC_end','Plume_dir']]
    return Plumedir

Plumedir=ExtractTropomi(Tripomi)
# --------------------------------------------------------------------------- #
#                   Stage 2: Extract Model data                               #
#                       create a compass mask centered on volcano             #
# --------------------------------------------------------------------------- #


def genxy(xy_file):
    """genxy
    description:
        Reading the xy_file and Convert the list of UTM points and to lat lons
    args:
        xy_file (str): filename for ascii file of xy points (2 cols)
    returns:
        glat (array): gridded latitudes
        glon (array): gridded longitudes
        latMin (float): min latitude extent
        latMax (float): max latitude extent
        lonMin (float): min longitude extent
        lonMax (float): max longitude extent
        ny (int): number of x points
        nx (int): number of y points
    """
    x, y = read_two_column_file(xy_file)  # read in x,y data
    xunq, yunq = np.unique(x), np.unique(y)  # get unique x,y coordinates
    nx, ny = len(xunq), len(yunq)  # number of unique x,y coordinates
    # Use utm package to convert from x,y to lat,lon...
    # ...Nicaragua is UTM zone 16P, and we must convert to metres first:
    lat = [utm.to_latlon(x[i] * 1000, y[i] * 1000, 16, 'P')[0]
           for i in np.arange(0, len(x))]
    lon = [utm.to_latlon(x[i] * 1000, y[i] * 1000, 16, 'P')[1]
           for i in np.arange(0, len(x))]
    # Create gridded field of lat,lon of appropriate size:
    glat, glon = np.reshape(lat, (ny, nx)), np.reshape(lon, (ny, nx))
    # Also grab range for static plots
    latMin = min(lat)
    latMax = max(lat)
    lonMin = min(lon)
    lonMax = max(lon)
    return glat, glon, latMin, latMax, lonMin, lonMax, ny, nx

def conc_array(ny, nx, file_path, binLims):
    """conc_array
    description:
        create an array of concentrations
    args:
        ny (int): number of x points
        nx (int): number of y points
        file_path (str): filename
        binLims (list): list of concentrations to bin the data
    returns:
        concA (array): array of concentrations in ug/m^3
                       (greater than value of smallest bin)
        conc (array): array of concentrations in ug/m^3
    """
    # Read in concentration data:
    f = open(file_path, 'r')
    lines = f.read().splitlines()
    f.close()
    # Process concentration data into desired format:
    conc = np.array([float(X) for X in lines]) * 100**3  # ug/cm^3 -> ug/m^3
    concAry = np.reshape(conc, (ny, nx))  # Reshape data onto latlon grid
    concA = np.ma.masked_array(concAry, concAry < binLims[0])
    return concA, conc

def concfiles(n_conc_files, conc_dir, SOX='SO2'):
    """concfiles
    description: generate list of concentration files
        n_conc_files (int): number of concentration files e.g. 48 or 24
        conc_dir (str): path to directory containing conc files
        SOX (str): SO species, default = 'SO2'
        modified to not assert files (will make missing files show up)
    returns:
        filenames (list): list of filenames e.g. concrec0100.dat
        file_paths (list): list of filepaths e.g
    """
    filenames = []
    file_paths = []
    if SOX == 'SO2':
        concrecx = 'concrec0100'
    elif SOX == 'SO4':
        concrecx = 'concrec0200'
    else:
        concrecx = 'concrec0100'
        print("WARNING: SOX option not valid setting to 'SO2'")
        print("Options available are 'SO2' or 'SO4'")
    for i in range(n_conc_files):
        # Ensure e.g. '1' is converted to '01'
        f_name = concrecx + str('{:02}'.format(i + 1)) + '.dat'
        filenames.append(f_name)
        f_path = os.path.join(conc_dir, f_name)
        file_paths.append(f_path)
    return filenames, file_paths


def read_two_column_file(file_name):
    """read_two_column_file
    description: extracts x and y from calpuff x and y file
    args:
        file_name (str): file to read containing ascii x and y
    returns:
        x (list): list of x values
        y (list): list of y values
    """
    with open(file_name, 'r') as data:
        x = []
        y = []
        for line in data:
            xy_vals = line.split()
            x.append(float(xy_vals[0]))
            y.append(float(xy_vals[1]))

    return x, y

(glat, glon, latMin, latMax, lonMin, lonMax, ny,nx) = genxy(xy_file)
volcano= ['masaya', (-86.1608, 11.9854)]
binLims = [10, 350, 600, 2600, 9000, 14000]  # SO2 bin limits
fle='../ECMWF/20190506/concrec010001.dat'
concA = conc_array(ny, nx, fle, binLims)[0]
from statistics import mode
def FindNearestLatLon(lat, lon, glat, glon):
    """
    description:
        return index Array(lat, lon) closest match to station lat lon
    args:
        lat (float): latitude
        lon (float): longitude
        glat (array): latitude array
        glon (array): longitude array
    returns:
        ilat (int): index for latitude
        ilon (int): index for longitude
    """
    # Find the closes point
    ilat = (np.abs(glat - lat)).argmin(axis=0)
    ilon = (np.abs(glon - lon)).argmin(axis=1)
    if hasattr(ilat, "__len__"):
        ilat = mode(ilat)
    if hasattr(ilon, "__len__"):
        ilon = mode(ilon)
    return ilat, ilon
cityCoords = ((-86.29, 12.12),)
volcCoords = (-86.1608, 11.9854)
x,y=FindNearestLatLon(volcCoords[0],volcCoords[1], glat, glon)
compassmask = np.zeros_like(concA)
compass_brackets = ["N",   "NE",  "E", "SE", "S",  "SW", "W",  "NW", "N"]
compass_brackets2 = ["N", "NNE", "NE","ENE", "E", "ESE","SE", "SSE", "S",
                   "SSW",  "SW", "WSW", "W","WNW", "NW", "NNW", "N"]
for j in np.arange(0,ny-1):
    for i in np.arange(0,nx-1):
        degs=math.atan2(i-x, j-y)/math.pi*180
        if degs<0:
            degs = 360 + degs
        compassmask[j,i]=round(degs / 45)

outDir = "../vis/comp"
sat = 'World_Imagery'
topo = 'World_Shaded_Relief'
# http://mkwc.ifa.hawaii.edu/vmap/hysplit/
colsHex = ['#FFFFFF', '#0cec0c', '#FFFF00', '#FF6600', '#FF0000',
                '#800080', '#8F246B']  # Hex codes for SO2 colour bins
# Number of conc files to process (48 = full 2 days)
cmap = mpl.colors.ListedColormap(colsHex[1:-1])
cmap.set_under(colsHex[0])
cmap.set_over(colsHex[-1])
norm = mpl.colors.BoundaryNorm(boundaries=binLims, ncolors=5)
n_conc_files = 24
binLims = [10, 350, 600, 2600, 9000, 14000]  # SO2 bin limits
binLimsSO4 = [1E-8, 12, 35, 55, 150, 250]  # SO4 bin limits from:

cities = (' MANAGUA',)
font = FontProperties()
font.set_weight('bold')
font.set_family('monospace')
sat = 'World_Imagery'
topo = 'World_Shaded_Relief'
# Number of conc files to process (48 = full 2 days)
n_conc_files = n_conc_files
binLims = [10, 350, 600, 2600, 9000, 14000]  # SO2 bin limits
binLimsSO4 = [1E-8, 12, 35, 55, 150, 250]  # SO4 bin limits from:
cityCoords = ((-86.29, 12.12),)
volcCoords = (-86.1608, 11.9854)
font = FontProperties()
font.set_weight('bold')
font.set_family('monospace')
plumetable=Plumedir
plumetable.reset_index(inplace=True)
plumetable['date'] = plumetable['date'].astype(str).apply(lambda x: x.zfill(8))
plumetable['NAM']='0'
plumetable['ECMWF']='0'
def gen_im(lonMin, latMin, lonMax, latMax, imtype="World_Street_Map",):
    """gen_im
    description
    args:
        lonMin (int): minimum longitude
        latMin (int): minimum latitude
        lonMax (int): maximum longitude
        latMax (int): maximum latitude
        imtype (str): 'World_Imagery' or 'World_Shaded_Relief'
    returns:
        ESRIimg (basemap image): background image either satellite or
        topography for concentration plots
    """
    xpixels = 1700  # Zoom lvl for satellite basemap (higher=bigger file sizes)
    bmap = Basemap(llcrnrlon=lonMin, llcrnrlat=latMin,
                   urcrnrlon=lonMax, urcrnrlat=latMax)
    esri_url = ("http://server.arcgisonline.com/ArcGIS/rest/services/" +
                imtype + "/MapServer/export?\
bbox=%s,%s,%s,%s&\
bboxSR=%s&\
imageSR=%s&\
size=%s,%s&\
dpi=%s&\
format=png32&\
f=image" % (bmap.llcrnrlon, bmap.llcrnrlat, bmap.urcrnrlon, bmap.urcrnrlat,
            bmap.epsg, bmap.epsg, xpixels, bmap.aspect * xpixels, 96))
    ESRIimg = mpimg.imread(esri_url)
    return ESRIimg
im = gen_im(lonMin, latMin, lonMax,
                    latMax)
for i,row in plumetable.iterrows():
    #if i ==9:
    #    continue
    #if i==12:
    #    continue
    #if i==17:
    #    continue
    date=row.date
    date=timestamp=datetime.strptime(date, "%d%m%Y")
    date= date.strftime("%Y%m%d")
    if i !=9:
        hour=timestamp=datetime.strptime(row.UTC_end, "%H:%M").hour
    else:
        hour=18
    x,y=FindNearestLatLon(11.9854,-86.1608, glat, glon)
    conc_arrayAA1=np.zeros([100,180,24])
    conc_arrayAA2=np.zeros([100,180,24])
    for ihr in np.arange(0,24):
        hr=str('{:02}'.format(ihr + 1))
        try:
            fle1='../ECMWF/'+date+'/concrec0100'+str(hr)+'.dat'
            fle2='../CALPUFF_OUT/CALPUFF/'+date+'/concrec0100'+str(hr)+'.dat'
            concA1 = conc_array(ny, nx, fle1, binLims)[0]
            concA2 = conc_array(ny, nx, fle2, binLims)[0]
            conc_arrayAA1[:,:,ihr]=concA1
            conc_arrayAA2[:,:,ihr]=concA2
        except (FileNotFoundError,ValueError):
            pass
    ii,jj,zz=np.where(conc_arrayAA1==conc_arrayAA1.max())
    concA1=conc_arrayAA1[:,:,hour-1]
    if concA1.max()<1:
        concA1=conc_arrayAA1[:,:,zz]
    concA1=concA1.squeeze()
    hr1=zz
    ii,jj,zz=np.where(conc_arrayAA2==conc_arrayAA2.max())
    concA2=conc_arrayAA2[:,:,zz]
    concA2=concA2.squeeze()
    hr2=zz
    fig = plt.figure()
    for ix,concA in enumerate([concA1,concA2]):

        if ix==0:
            ax = fig.add_subplot(211)
        if ix==1:
            ax = fig.add_subplot(212)
        latMin, latMax, lonMin = latMin, latMax, lonMin
        lonMax = lonMax
        bmap = Basemap(llcrnrlon=lonMin, llcrnrlat=latMin,
                       urcrnrlon=lonMax, urcrnrlat=latMax,ax=ax)
        bmap.imshow(im, origin='upper')
        bmap.pcolormesh(glon, glat, concA,
                        norm=norm, cmap=cmap, alpha=0.5, ax=ax)
        #cbar = ax.colorbar(location='bottom', pad='20%', cmap=cmap,
        #                     norm=norm, boundaries=[0.] + binLims
        #                     + [100000.], extend='both', extendfrac='auto',
        #                     ticks=binLims, spacing='uniform')
        #cbar.ax.set_xticklabels(['v low', 'low', 'moderate', 'mod high',
        #                         'high', 'v high'])  # horizontal colorbar
        #cbar.set_label(label=(SOX + ' concentration'), fontsize=18)
        #cbar.ax.tick_params(labelsize=16)
        #cbar.solids.set(alpha=1)
        latTicks = np.arange(round(latMin, 1), round(latMax, 1) + 0.1, 0.1)
        lonTicks = np.arange(round(lonMin, 1), round(lonMax, 1) + 0.1, 0.2)
        bmap.drawparallels(latTicks, labels=[1, 0, 0, 0], linewidth=0.0,
                           fontsize=16)
        bmap.drawmeridians(lonTicks, labels=[0, 0, 0, 1], linewidth=0.0,
                           fontsize=16)
        for iy, city in enumerate(cities):
            ax.plot(cityCoords[iy][0], cityCoords[iy]
                     [1], 'sk', markersize=6)
            ax.text(cityCoords[iy][0], cityCoords[iy][1], city,
                     fontproperties=font, fontsize=16)
        font0 = FontProperties()
        font0.set_family('monospace')
        ax.plot(volcCoords[0], volcCoords[1], '^r', markersize=6)
        #plt.suptitle(so2title, fontsize=24)

        if ix==0:
            ax.set_title('ECMWF '+str(date)+str(hr1), fontsize=18)
            PNGfile = 'ECMWF_' + date + str(hour)+ '.png'
            print("Writing out file " + PNGfile)
            PNGpath = os.path.join(outDir, PNGfile)
        else:
            ax.set_title('NAM '+str(date)+str(hr2), fontsize=18)
            plt.tight_layout()
            PNGfile = date + str(hour)+ '.png'
            print("Writing out file " + PNGfile)
            PNGpath = os.path.join(outDir, PNGfile)
    plt.savefig(PNGpath, dpi=250)
    plt.close()
    concA1[x-5:x+5,y-5:y+5]=0
    concA2[x-5:x+5,y-5:y+5]=0
    ii,jj=np.where(concA1==concA1.max())
    degs=math.atan2(ii-x, jj-y)/math.pi*180
    if degs<0:
            degs = 360 + degs
    ECMWF=compass_brackets2[round(degs / 22.5)]
    ii,jj=np.where(concA2==concA2.max())
    degs=math.atan2(ii[0]-x, jj[0]-y)/math.pi*180
    if degs<0:
            degs = 360 + degs
    NAM=compass_brackets2[round(degs / 22.5)]
    plumetable.at[i,'ECMWF']=ECMWF
    plumetable.at[i,'NAM']=NAM

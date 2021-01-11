#!/usr/bin/env python
"""
Script name: Create3DDAT.py
Author: JO'N, CEMAC (University of Leeds)
Date: March 2018
Purpose: Generate input file to CALMET from NAM met data
Usage: ./Create3DDAT.py <date>
        <date> - Start date of NAM data in format YYYYMMDD, e.g. 20171204
Output: File written to <root>/NAM_data/processed/met_<date>.dat
.. CEMAC_UNRESP:
   https://github.com/cemac/UNRESPForecastingSystem
"""
import argparse
import matplotlib.pyplot as plt
import datetime as dt
from mpl_toolkits.basemap import Basemap
from dateutil.parser import parse
import numpy as np
import gribapi
import os
import sys


#date='20201204'
date='20170331'
# PARAMETERS
latMinCP = 11.7  # Min lat of CALPUFF grid
latMaxCP = 12.2  # Max lat of CALPUFF grid
lonMinCP = 273.2  # Min lon of CALPUFF grid
lonMaxCP = 274.1  # Max lon of CALPUFF grid
inDir = 'NAM_raw_data/'+date   # cdirectory containing GRIB files
# Number of GRIB files (files are 6 hourly,
# so 24 hours is 5 files including hours 00 24)
nfiles = 5
outFile =  'test_met_'+date+'.dat'   # Output file path
# pressure levels to include in output
levsIncl = [1000, 950, 925, 900, 850, 800, 700, 600, 500, 400, 300, 250, 200,
            150, 100, 75, 50, 30, 20, 10, 7, 5, 2]

# SET FILENAMES
filePrefix = 'nam.t'
fileSuffix = 'z.afwaca00.tm00.grib2'
#fileSuffix = 'z.afwaca00.grb2.tm00'
#filePrefix = 'nam.t00z.afwaca'
#fileSuffix = '.tm00.grib2'
filenames = []
filePaths = []
for i in range(nfiles):
    filenames.append(filePrefix + '{:02d}'.format(i * 6) + fileSuffix)
    filePaths.append(os.path.join(inDir, filenames[i]))
#####

# OPEN FIRST GRIB FILE, GET MESSAGE HANDLERS AND CLOSE
f = open(filePaths[0], 'r')
gribapi.grib_multi_support_on()
mcount = gribapi.grib_count_in_file(f)  # number of messages in file
gids = [gribapi.grib_new_from_file(f) for i in range(mcount)]
f.close()
#####

# GET NAME AND LEVEL OF EACH MESSAGE
varNames = []
levels = []
for i in range(mcount):
    gid = gids[i]
    varNames.append(gribapi.grib_get(gid, 'shortName'))
    levels.append(gribapi.grib_get(gid, 'level'))
#####

# GET REQUIRED GIDS (AT REQUIRED LEVELS)
gidPRMSL = varNames.index("prmsl") + 1  # Pressure reduced to mean sea level
# Height
gidHGT = np.flipud([i + 1 for i in range(len(varNames))
                    if (varNames[i] == 'gh' and levels[i] in levsIncl)])
# Temperature
gidTMP = np.flipud([i + 1 for i in range(len(varNames))
                    if (varNames[i] == 't' and levels[i] in levsIncl)])
# U-component of wind
gidU = np.flipud([i + 1 for i in range(len(varNames))
                  if (varNames[i] == 'u' and levels[i] in levsIncl)])
# V-component of wind
gidV = np.flipud([i + 1 for i in range(len(varNames))
                  if (varNames[i] == 'v' and levels[i] in levsIncl)])
# W-component of wind
gidW = np.flipud([i + 1 for i in range(len(varNames))
                  if (varNames[i] == 'wz' and levels[i] in levsIncl)])
# Relative humiditys
gidRH = np.flipud([i + 1 for i in range(len(varNames))
                   if (varNames[i] == 'r' and levels[i] in levsIncl)])
# GET LATS, LONS, NI AND NJ
lats = gribapi.grib_get_array(gidPRMSL, 'distinctLatitudes')
lons = gribapi.grib_get_array(gidPRMSL, 'distinctLongitudes')
Ni = gribapi.grib_get(gidPRMSL, 'Ni')
Nj = gribapi.grib_get(gidPRMSL, 'Nj')
#####

# DETERMINE SUBDOMAIN INDICES BASED ON CALPUFF GRID EXTENT
for i in range(len(lats) - 1):
    if lats[i + 1] >= latMinCP:
        iLatMinGRIB = i
        break
for i in range(len(lats) - 1):
    if lats[i + 1] > latMaxCP:
        iLatMaxGRIB = i + 1
        break
for i in range(len(lons) - 1):
    if lons[i + 1] >= lonMinCP:
        iLonMinGRIB = i
        break
for i in range(len(lons) - 1):
    if lons[i + 1] > lonMaxCP:
        iLonMaxGRIB = i + 1
        break
#####

# SET SUBDOMAIN SIZE
# NX, i.e. number of longitudes in GRIB subset grid
NX = iLonMaxGRIB - iLonMinGRIB + 1
# NY, i.e. number of latitudes in GRIB subset grid
NY = iLatMaxGRIB - iLatMinGRIB + 1
# NZ, i.e. number of levels to be extracted from GRIB subset grid
NZ = len(levsIncl)
#####

# PLOT A MESSAGE
gids2plot=[gidHGT, gidRH, gidTMP, gidU, gidV, gidW]
gidnames=['HGT','RH','TMP','U','V','W']
for i, var in enumerate(gids2plot):
    plt.clf()
    plt.cla()
    gidPlot=var[-2]
    Ni=gribapi.grib_get(int(gidPlot),'Ni')
    Nj=gribapi.grib_get(int(gidPlot),'Nj')
    missingValue=gribapi.grib_get(int(gidPlot),"missingValue")
    values=gribapi.grib_get_values(int(gidPlot))
    msg=np.reshape(values,(Nj,Ni),'C')
    msgmasked = np.ma.masked_values(msg,missingValue)
    xx,yy=np.meshgrid(lons,lats)
    map = Basemap(llcrnrlon=250,llcrnrlat=-10,urcrnrlon=310,urcrnrlat=40)
    map.drawcoastlines()
    map.drawcountries()
    cs=map.contourf(xx,yy,msgmasked)
    map.colorbar(cs)
    plt.tight_layout()
    plt.savefig(gidnames[i]+'2017grib2.png')
#####

# RELEASE ALL MESSAGES
for i in range(mcount):
    gribapi.grib_release(i + 1)
#####

# OPEN OUTPUT FILE
fout = open(outFile, 'w')
#####

def take_along_axis(arr, ind, axis):
    """
    ... here means a "pack" of dimensions, possibly empty

    arr: array_like of shape (A..., M, B...)
        source array
    ind: array_like of shape (A..., K..., B...)
        indices to take along each 1d slice of `arr`
    axis: int
        index of the axis with dimension M

    out: array_like of shape (A..., K..., B...)
        out[a..., k..., b...] = arr[a..., inds[a..., k..., b...], b...]
    """
    if axis < 0:
       if axis >= -arr.ndim:
           axis += arr.ndim
       else:
           raise IndexError('axis out of range')
    ind_shape = (1,) * ind.ndim
    ins_ndim = ind.ndim - (arr.ndim - 1)   #inserted dimensions

    dest_dims = list(range(axis)) + [None] + list(range(axis+ins_ndim, ind.ndim))

    # could also call np.ix_ here with some dummy arguments, then throw those results away
    inds = []
    for dim, n in zip(dest_dims, arr.shape):
        if dim is None:
            inds.append(ind)
        else:
            ind_shape_dim = ind_shape[:dim] + (-1,) + ind_shape[dim+1:]
            inds.append(np.arange(n).reshape(ind_shape_dim))

    return arr[tuple(inds)]


def writeRec1():
    # DATASET='3D.DAT' #Dataset name
    # DATAVER='2.1' #Dataset version
    # DATAMOD='Created using Create3DDAT.py' #Dataset message field
    # fout.write('{:16}{:16}{}\n'.format(DATASET,DATAVER,DATAMOD))
    fout.write(
        'M3D file Created from ETA AWIPS 212 Grid for Falconbridge CALMET\n')
    # just replicate Sara's data file for now


def writeRec2():
    # NCOMM=1 #Number of comment records to follow
    # fout.write('{:1d}\n'.format(NCOMM))
    # COMMENT="Currently set up to process GRIB data file from NAM's Central
    # American/Caribbean domain" #Comments
    # fout.write('{}\n'.format(COMMENT))
    fout.write('MM53D.DAT   1.0         020715      \n')
    # just replicate Sara's data file afor now


def writeRec3():
    IOUTW = 1  # Vertical velocity flag
    IOUTQ = 1  # Relative humidity flag
    IOUTC = 0  # cloud/rain mixing ration flag
    IOUTI = 0  # ice/snow MR flag
    IOUTG = 0  # graupel MP flag
    IOSRF = 0  # create surface 2D files flag
    fout.write(('{:3d}' * 6 + '\n').format(IOUTW,
               IOUTQ, IOUTC, IOUTI, IOUTG, IOSRF))


def writeRec4():
    MAPTXT = 'LLC'  # Map projection (LLC=lat/lon)
    RLATC = (lats[iLatMinGRIB] + lats[iLatMaxGRIB]) / 2.
    # centre latitude of GRIB subset grid
    RLONC = (lons[iLonMinGRIB] + lons[iLonMaxGRIB]) / 2.
    # centre longitude of GRIB subset grid
    TRUELAT1 = lats[iLatMinGRIB]  # First latitude in GRIB subset grid
    TRUELAT2 = lats[iLatMinGRIB + 1]  # Second latitude in GRIB subset grid
    X1DMN = 0.0  # Not used so set to zero
    Y1DMN = 0.0  # Not used so set to zero
    DXY = 0.0  # Not used so set to zero
    fout.write(('{:4}{:9.4f}{:10.4f}' + '{:7.2f}' * 2 + '{:10.3f}' * 2 + '{:8.3f}' + '{:4d}' * 2 + '{:3d}\n').
               format(MAPTXT, RLATC, RLONC, TRUELAT1, TRUELAT2, X1DMN, Y1DMN, DXY, NX, NY, NZ))


def writeRec5():
    # Flags that aren't used unless using MM5 model
    fout.write(('{:3d}' * 23 + '\n').format(0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))


def writeRec6():
    IBYRM = int(date[0:4])  # Beginning year of GRIB data
    IBMOM = int(date[4:6])  # Beginning month of GRIB data
    IBDYM = int(date[6:8])  # Beginning day of GRIB data
    IBHRM = 0  # Beginning hour (GMT) of GRIB data
    # Length of period (hours of data in file (Replicate Sara's file but should possibly be 3* this value?)
    NHRSMM5 = (nfiles-1)*6
    fout.write(('{:4d}' + '{:02d}' * 3 + '{:5d}' + '{:4d}' * 3 +
               '\n').format(IBYRM, IBMOM, IBDYM, IBHRM, NHRSMM5, NX, NY, NZ))


def writeRec7():
    NX1 = 1  # I-index (x-direction) of lower left corner
    NY1 = 1  # J-index (y-direction) of lower left corner
    NX2 = NX  # I-index (x-direction) of upper right corner
    NY2 = NY  # J-index (y-direction) of upper right corner
    NZ1 = 1  # K-index of lowest extracted layer
    NZ2 = NZ  # K-index of highest extracted layer
    RXMIN = lons[iLonMinGRIB]  # W-most E longitude
    RXMAX = lons[iLonMaxGRIB]  # E-most E longitude
    RYMIN = lats[iLatMinGRIB]  # S-most N latitude
    RYMAX = lats[iLatMaxGRIB]  # N-most N latitude
    fout.write(('{:4d}' * 6 + '{:10.4f}' * 2 + '{:9.4f}' * 2 +
               '\n').format(NX1, NY1, NX2, NY2, NZ1, NZ2, RXMIN, RXMAX, RYMIN, RYMAX))
    SIGMA = np.array(levsIncl) / 1013.25
    # Sigma-p values for each vertical layer (pressure/reference pressure)
    for s in SIGMA:
        fout.write('{:6.3f}\n'.format(s))


def writeRec8():
    # terrain elevation above MSL (m). Set to zero for now to replicate Sara's file
    IELEVDOT = 0
    ILAND = -9  # Lansuse categories (set to -9 to replicae Sara's file)
    XLATCRS = -999  # Not used
    XLONGCRS = -999  # Not used
    IELEVCRS = -999  # Not used
    for j in range(NY):
        JINDEX = j + 1  # J-index (y-direction) of grid point
        XLATDOT = lats[iLatMinGRIB + j]  # N latitude of grid point
        for i in range(NX):
            IINDEX = i + 1  # I-index (x-direction) of grid point
            XLONGDOT = lons[iLonMinGRIB + i]  # E longitude of grid point
            fout.write(('{:4d}' * 2 + '{:9.4f}{:10.4f}{:5d}{:3d} {:9.4f}{:10.4f}{:5d}\n').format(
                IINDEX, JINDEX, XLATDOT, XLONGDOT, IELEVDOT, ILAND, XLATCRS, XLONGCRS, IELEVCRS))


def writeRec10():
    PRES = 1013.0  # Sea level presure (replicate Sara's file)
    # total accumulated rainfall from past hour (replicate Sara's file)
    RAIN = 0.0
    SC = 0  # snow cover
    RADSW = 0.0  # SW radiation at surface (replicate Sara's file)
    RADLW = 0.0  # LW radiation at top (replicate Sara's file)
    VAPMR = 0.0  # Vagour mixing ratio (replicate Sara's file)
    for t in range(nfiles):
        print("Processing file " + filenames[t])
        dateTime = parse(date) + dt.timedelta(hours=t * 6)
        MYR = dateTime.year  # Year of data block
        MMO = dateTime.month  # Month of data block
        MDAY = dateTime.day  # Day of data block
        MHR = dateTime.hour  # Hour of data block
        # GRIB file processing:
        f = open(filePaths[t], 'r')  # Open GRIB file
        gribapi.grib_multi_support_on()  # Turn on multi-message support
        mcount = gribapi.grib_count_in_file(f)  # number of messages in file
        [gribapi.grib_new_from_file(f) for i in range(
            mcount)]  # Get handles for all messages
        f.close()  # Close GRIB file
        # Initialse 3D arrays for holding required fields:
        HGTgrd = np.zeros(shape=(Nj, Ni, NZ))
        TMPgrd = np.zeros(shape=(Nj, Ni, NZ))
        Ugrd = np.zeros(shape=(Nj, Ni, NZ))
        Vgrd = np.zeros(shape=(Nj, Ni, NZ))
        Wgrd = np.zeros(shape=(Nj, Ni, NZ))
        RHgrd = np.zeros(shape=(Nj, Ni, NZ))
        # Loop through included levels and store the values in the appropriate k index of the 3D arrays
        for k in range(NZ):
            HGTvals = gribapi.grib_get_values(int(gidHGT[k]))
            HGTgrd[:, :, k] = np.reshape(HGTvals, (Nj, Ni), 'C')
            TMPvals = gribapi.grib_get_values(int(gidTMP[k]))
            TMPgrd[:, :, k] = np.reshape(TMPvals, (Nj, Ni), 'C')
            Uvals = gribapi.grib_get_values(int(gidU[k]))
            Ugrd[:, :, k] = np.reshape(Uvals, (Nj, Ni), 'C')
            Vvals = gribapi.grib_get_values(int(gidV[k]))
            Vgrd[:, :, k] = np.reshape(Vvals, (Nj, Ni), 'C')
            Wvals = gribapi.grib_get_values(int(gidW[k]))
            Wgrd[:, :, k] = np.reshape(Wvals, (Nj, Ni), 'C')
            RHvals = gribapi.grib_get_values(int(gidRH[k]))
            RHgrd[:, :, k] = np.reshape(RHvals, (Nj, Ni), 'C')
        WSgrd = np.sqrt(Ugrd**2 + Vgrd**2)  # Calculate wins speed (pythagoras)
        # Calculate wind direction:
        # radians, between [-pi,pi], positive anticlockwise from positive x-axis
        WDgrd = np.arctan2(Vgrd, Ugrd)
        # degrees, between [-180,180], positive anticlockwise from positive x-axis
        WDgrd *= 180 / np.pi
        # degrees, between [0,360], positive anticlockwise from negative x-axis (Since we specify the direction the wind is blowing FROM, not TO)
        WDgrd += 180
        # degrees, between [-360,0], positive clockwise from negative x-axis (Since wind direction is positive clockwise)
        WDgrd = - WDgrd
        # degrees, between [-270,90], positive clockwise from positive y-axis (Since wind direction is from North)
        WDgrd += 90
        # degrees, between [0,360], positive clockwise from positive y-axis (DONE!)
        WDgrd = np.mod(WDgrd, 360)
        # Loop over grid cells:
        for j in range(NY):
            JX = j + 1  # J-index of grid cell
            for i in range(NX):
                IX = i + 1  # i-index of grid cell
                fout.write(('{:4d}' + '{:02d}' * 3 + '{:3d}' * 2 + '{:7.1f}{:5.2f}{:2d}' + '{:8.1f}'
                           * 2 + '\n').format(MYR, MMO, MDAY, MHR, IX, JX, PRES, RAIN, SC, RADSW, RADLW))
                for k in range(NZ):
                    PRES2 = levsIncl[k]  # Pressure (mb)
                    # Elevation (m above sea level)
                    Z = int(HGTgrd[iLatMinGRIB + j, iLonMinGRIB + i, k])
                    # Temperature (Kelvin)
                    TEMPK = TMPgrd[iLatMinGRIB + j, iLonMinGRIB + i, k]
                    # Wind direction (degrees)
                    WD = int(WDgrd[iLatMinGRIB + j, iLonMinGRIB + i, k])
                    # Wind speed (m/s)
                    WS = WSgrd[iLatMinGRIB + j, iLonMinGRIB + i, k]
                    # Vertical velocity (m/s)
                    W = Wgrd[iLatMinGRIB + j, iLonMinGRIB + i, k]
                    # Relative humidity (%)
                    RH = int(RHgrd[iLatMinGRIB + j, iLonMinGRIB + i, k])
                    fout.write(('{:4d}{:6d}{:6.1f}{:4d}{:5.1f}{:6.2f}{:3d}{:5.2f}\n').format(
                        PRES2, Z, TEMPK, WD, WS, W, RH, VAPMR))
        # Release all messages:
        for i in range(mcount):
            gribapi.grib_release(i + 1)


def writeRec9():
    PRES = 1013.0  # Sea level presure (replicate Sara's file)
    # total accumulated rainfall from past hour (replicate Sara's file)
    RAIN = 0.0
    SC = 0  # snow cover
    RADSW = 0.0  # SW radiation at surface (replicate Sara's file)
    RADLW = 0.0  # LW radiation at top (replicate Sara's file)
    VAPMR = 0.0  # Vagour mixing ratio (replicate Sara's file)
    for t in range(nfiles):
        print("Processing file " + filenames[t])
        dateTime = parse(date) + dt.timedelta(hours=t * 6)
        MYR = dateTime.year  # Year of data block
        MMO = dateTime.month  # Month of data block
        MDAY = dateTime.day  # Day of data block
        MHR = dateTime.hour  # Hour of data block
        # GRIB file processing:
        f = open(filePaths[t], 'r')  # Open GRIB file
        gribapi.grib_multi_support_on()  # Turn on multi-message support
        mcount = gribapi.grib_count_in_file(f)  # number of messages in file
        [gribapi.grib_new_from_file(f) for i in range(
            mcount)]  # Get handles for all messages
        f.close()  # Close GRIB file
        # Initialse 3D arrays for holding required fields:
        HGTgrd = np.zeros(shape=(Nj, Ni, NZ))
        TMPgrd = np.zeros(shape=(Nj, Ni, NZ))
        Ugrd = np.zeros(shape=(Nj, Ni, NZ))
        Vgrd = np.zeros(shape=(Nj, Ni, NZ))
        Wgrd = np.zeros(shape=(Nj, Ni, NZ))
        RHgrd = np.zeros(shape=(Nj, Ni, NZ))
        # Loop through included levels and store the values in the appropriate k index of the 3D arrays
        for k in range(NZ):
            HGTvals = gribapi.grib_get_values(int(gidHGT[k]))
            HGTgrd[:, :, k] = np.reshape(HGTvals, (Nj, Ni), 'C')
            TMPvals = gribapi.grib_get_values(int(gidTMP[k]))
            TMPgrd[:, :, k] = np.reshape(TMPvals, (Nj, Ni), 'C')
            Uvals = gribapi.grib_get_values(int(gidU[k]))
            Ugrd[:, :, k] = np.reshape(Uvals, (Nj, Ni), 'C')
            Vvals = gribapi.grib_get_values(int(gidV[k]))
            Vgrd[:, :, k] = np.reshape(Vvals, (Nj, Ni), 'C')
            Wvals = gribapi.grib_get_values(int(gidW[k]))
            Wgrd[:, :, k] = np.reshape(Wvals, (Nj, Ni), 'C')
            RHvals = gribapi.grib_get_values(int(gidRH[k]))
            RHgrd[:, :, k] = np.reshape(RHvals, (Nj, Ni), 'C')
        WSgrd = np.sqrt(Ugrd**2 + Vgrd**2)  # Calculate wins speed (pythagoras)
        # Calculate wind direction:
        # radians, between [-pi,pi], positive anticlockwise from positive x-axis
        WDgrd = np.arctan2(Vgrd, Ugrd)
        # degrees, between [-180,180], positive anticlockwise from positive x-axis
        WDgrd *= 180 / np.pi
        # degrees, between [0,360], positive anticlockwise from negative x-axis (Since we specify the direction the wind is blowing FROM, not TO)
        WDgrd += 180
        # degrees, between [-360,0], positive clockwise from negative x-axis (Since wind direction is positive clockwise)
        WDgrd = - WDgrd
        # degrees, between [-270,90], positive clockwise from positive y-axis (Since wind direction is from North)
        WDgrd += 90
        # degrees, between [0,360], positive clockwise from positive y-axis (DONE!)
        WDgrd = np.mod(WDgrd, 360)
        if t > 0:
            dateTime = parse(date) + dt.timedelta(hours=(t * 6)-3)
            print('interpolating')
            print(dateTime)
            MYR = dateTime.year  # Year of data block
            MMO = dateTime.month  # Month of data block
            MDAY = dateTime.day  # Day of data block
            MHR = dateTime.hour  # Hour of data block
            HGTgrd = np.mean(np.array([HGTgrd + HGTgrd_ini]), axis=0)
            TMPgrd = np.mean(np.array([TMPgrd + TMPgrd_ini]), axis=0)
            Wgrd = np.mean(np.array([Wgrd + Wgrd_ini]), axis=0)
            RHgrd = np.mean(np.array([RHgrd + RHgrd_ini]), axis=0)
            WDgrd = np.mean(np.array([WDgrd + WDgrd_ini]), axis=0)
            WSgrd = np.mean(np.array([WSgrd + WSgrd_ini]), axis=0)
            inds = HGTgrd.argsort(axis=2)
            HGTgrd = take_along_axis(HGTgrd, inds, axis=2)
            TMPgrd = take_along_axis(TMPgrd, inds, axis=2)
            Wgrd = take_along_axis(Wgrd, inds, axis=2)
            RHgrd = take_along_axis(RHgrd, inds, axis=2)
            WSgrd = take_along_axis(WSgrd, inds, axis=2)
            WDgrd = take_along_axis(WDgrd, inds, axis=2)
            # Loop over grid cells:
            for j in range(NY):
                JX = j + 1  # J-index of grid cell
                for i in range(NX):
                    IX = i + 1  # i-index of grid cell
                    fout.write(('{:4d}' + '{:02d}' * 3 + '{:3d}' * 2 + '{:7.1f}{:5.2f}{:2d}' + '{:8.1f}'
                               * 2 + '\n').format(MYR, MMO, MDAY, MHR, IX, JX, PRES, RAIN, SC, RADSW, RADLW))
                    for k in range(NZ):
                        PRES2 = levsIncl[k]  # Pressure (mb)
                        # Elevation (m above sea level)
                        Z = int(HGTgrd[iLatMinGRIB + j, iLonMinGRIB + i, k])
                        # Temperature (Kelvin)
                        TEMPK = TMPgrd[iLatMinGRIB + j, iLonMinGRIB + i, k]
                        # Wind direction (degrees)
                        WD = int(WDgrd[iLatMinGRIB + j, iLonMinGRIB + i, k])
                        # Wind speed (m/s)
                        WS = WSgrd[iLatMinGRIB + j, iLonMinGRIB + i, k]
                        # Vertical velocity (m/s)
                        W = Wgrd[iLatMinGRIB + j, iLonMinGRIB + i, k]
                        # Relative humidity (%)
                        RH = int(RHgrd[iLatMinGRIB + j, iLonMinGRIB + i, k])
                        fout.write(('{:4d}{:6d}{:6.1f}{:4d}{:5.1f}{:6.2f}{:3d}{:5.2f}\n').format(
                            PRES2, Z, TEMPK, WD, WS, W, RH, VAPMR))
            # Release all messages:
            for i in range(mcount):
                gribapi.grib_release(i + 1)
        # GRIB file processing:
        dateTime = parse(date) + dt.timedelta(hours=t * 6)
        print('processing')
        print(dateTime)
        MYR = dateTime.year  # Year of data block
        MMO = dateTime.month  # Month of data block
        MDAY = dateTime.day  # Day of data block
        MHR = dateTime.hour  # Hour of data block
        f = open(filePaths[t], 'r')  # Open GRIB file
        gribapi.grib_multi_support_on()  # Turn on multi-message support
        mcount = gribapi.grib_count_in_file(f)  # number of messages in file
        [gribapi.grib_new_from_file(f) for i in range(
            mcount)]  # Get handles for all messages
        f.close()  # Close GRIB file
        # Initialse 3D arrays for holding required fields:
        HGTgrd = np.zeros(shape=(Nj, Ni, NZ))
        TMPgrd = np.zeros(shape=(Nj, Ni, NZ))
        Ugrd = np.zeros(shape=(Nj, Ni, NZ))
        Vgrd = np.zeros(shape=(Nj, Ni, NZ))
        Wgrd = np.zeros(shape=(Nj, Ni, NZ))
        RHgrd = np.zeros(shape=(Nj, Ni, NZ))
        # Loop through included levels and store the values in the appropriate k index of the 3D arrays
        for k in range(NZ):
            HGTvals = gribapi.grib_get_values(int(gidHGT[k]))
            HGTgrd[:, :, k] = np.reshape(HGTvals, (Nj, Ni), 'C')
            TMPvals = gribapi.grib_get_values(int(gidTMP[k]))
            TMPgrd[:, :, k] = np.reshape(TMPvals, (Nj, Ni), 'C')
            Uvals = gribapi.grib_get_values(int(gidU[k]))
            Ugrd[:, :, k] = np.reshape(Uvals, (Nj, Ni), 'C')
            Vvals = gribapi.grib_get_values(int(gidV[k]))
            Vgrd[:, :, k] = np.reshape(Vvals, (Nj, Ni), 'C')
            Wvals = gribapi.grib_get_values(int(gidW[k]))
            Wgrd[:, :, k] = np.reshape(Wvals, (Nj, Ni), 'C')
            RHvals = gribapi.grib_get_values(int(gidRH[k]))
            RHgrd[:, :, k] = np.reshape(RHvals, (Nj, Ni), 'C')
        HGTgrd_ini = HGTgrd
        TMPgrd_ini = TMPgrd
        Wgrd_ini = Wgrd
        RHgrd_ini = RHgrd
        WSgrd = np.sqrt(Ugrd**2 + Vgrd**2)  # Calculate wins speed (pythagoras)
        # Calculate wind direction:
        # radians, between [-pi,pi], positive anticlockwise from positive x-axis
        WDgrd = np.arctan2(Vgrd, Ugrd)
        # degrees, between [-180,180], positive anticlockwise from positive x-axis
        WDgrd *= 180 / np.pi
        # degrees, between [0,360], positive anticlockwise from negative x-axis (Since we specify the direction the wind is blowing FROM, not TO)
        WDgrd += 180
        # degrees, between [-360,0], positive clockwise from negative x-axis (Since wind direction is positive clockwise)
        WDgrd = - WDgrd
        # degrees, between [-270,90], positive clockwise from positive y-axis (Since wind direction is from North)
        WDgrd += 90
        # degrees, between [0,360], positive clockwise from positive y-axis (DONE!)
        WDgrd = np.mod(WDgrd, 360)
        WDgrd_ini = WDgrd
        WSgrd_ini = WSgrd
        inds = HGTgrd.argsort(axis=2)
        HGTgrd = take_along_axis(HGTgrd, inds, axis=2)
        TMPgrd = take_along_axis(TMPgrd, inds, axis=2)
        Wgrd = take_along_axis(Wgrd, inds, axis=2)
        RHgrd = take_along_axis(RHgrd, inds, axis=2)
        WSgrd = take_along_axis(WSgrd, inds, axis=2)
        WDgrd = take_along_axis(WDgrd, inds, axis=2)
        # Loop over grid cells:
        for j in range(NY):
            JX = j + 1  # J-index of grid cell
            for i in range(NX):
                IX = i + 1  # i-index of grid cell
                fout.write(('{:4d}' + '{:02d}' * 3 + '{:3d}' * 2 + '{:7.1f}{:5.2f}{:2d}' + '{:8.1f}'
                           * 2 + '\n').format(MYR, MMO, MDAY, MHR, IX, JX, PRES, RAIN, SC, RADSW, RADLW))
                for k in range(NZ):
                    PRES2 = levsIncl[k]  # Pressure (mb)
                    # Elevation (m above sea level)
                    Z = int(HGTgrd[iLatMinGRIB + j, iLonMinGRIB + i, k])
                    # Temperature (Kelvin)
                    TEMPK = TMPgrd[iLatMinGRIB + j, iLonMinGRIB + i, k]
                    # Wind direction (degrees)
                    WD = int(WDgrd[iLatMinGRIB + j, iLonMinGRIB + i, k])
                    # Wind speed (m/s)
                    WS = WSgrd[iLatMinGRIB + j, iLonMinGRIB + i, k]
                    # Vertical velocity (m/s)
                    W = Wgrd[iLatMinGRIB + j, iLonMinGRIB + i, k]
                    # Relative humidity (%)
                    RH = int(RHgrd[iLatMinGRIB + j, iLonMinGRIB + i, k])
                    fout.write(('{:4d}{:6d}{:6.1f}{:4d}{:5.1f}{:6.2f}{:3d}{:5.2f}\n').format(
                        PRES2, Z, TEMPK, WD, WS, W, RH, VAPMR))
        # Release all messages:
        for i in range(mcount):
            gribapi.grib_release(i + 1)



# WRITE RECORDS
writeRec1()
writeRec2()
writeRec3()
writeRec4()
writeRec5()
writeRec6()
writeRec7()
writeRec8()
writeRec9()

# CLOSE OUTPUT FILE
fout.close()
#####

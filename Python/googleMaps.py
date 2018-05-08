#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Script name: googleMaps.py
Author: JO'N
Date: January 2018
Purpose: Used to generate a series (48hrs) of interactive google maps showing SO2 concentrations around the
         Masaya volcano, as predicted by the CALPUFF dispersion model
Usage: ./googleMaps.py <date>
       <date> - Date string, format YYYYMMDD, of the current CALPUFF run. Used to locate 
           directory containing the SO2 output files (with assumed naming convention 'concrec0100**.dat',
           where '**' goes from '01' through to '48')
Output: vis/<date>/google_concrec******.html - A set of html files showing an interactive google basemap
        with the SO2 plume overlain.
"""

import numpy as np
import matplotlib as mpl
import os
import utm
import gmplot
import argparse

def Read_Two_Column_File(file_name):
    with open(file_name, 'r') as data:
        x = []
        y = []
        for line in data:
            p = line.split()
            x.append(float(p[0]))
            y.append(float(p[1]))

    return x, y

#####READ IN COMMAND LINE ARGUMENTS
parser = argparse.ArgumentParser(description = "Used to generate a series (48hrs) of interactive google maps showing \
         SO2 concentrations around the Masaya volcano, as predicted by the CALPUFF dispersion model")
parser.add_argument("date", help="Date string, format YYYYMMDD, of the current CALPUFF run. Used to locate \
                    directory containing the SO2 output files (with assumed naming convention 'concrec0100**.dat', \
                    where '**' goes from '01' through to '48'",type=str)
args = parser.parse_args()
date=args.date
#####

#####PARAMETERS
concDir="../CALPUFF_OUT/CALPUFF/"+date
xyFile="../data/xy_masaya.dat"
outDir="../vis/"+date
nConcFiles=48 #Number of conc files to process (48 = full 2 days)
binLims=[10,350,600,2600,9000,14000] #SO2 bin limits
colsHex=['#FFFFFF','#008000','#FFFF00','#FF6600','#FF0000','#800080','#8F246B'] #Hex codes for SO2 colour bins
#####

#####CHECK PATHS/FILES EXIST
assert os.path.exists(concDir), "CALPUFF output directory does not exist for this date."
assert os.path.exists(xyFile), "Cannot find data/xy_masaya.dat coordinate data file."
assert os.path.exists(outDir), "Output directory vis/<date> does not exist."
filenames=[]
filePaths=[]
for i in range(nConcFiles):
    s = str('{:02}'.format(i+1)) #Ensures e.g. '1' is converted to '01'
    fileName='concrec0100'+s+'.dat'
    filenames.append(fileName)
    filePath=os.path.join(concDir,fileName)
    filePaths.append(filePath)
    assert os.path.exists(filePath), "File "+filePath+" not found. Check path."
#####

#####SPATIAL DATA
x, y = Read_Two_Column_File(xyFile) #reads in x,y data
#get unique x,y coordinates and their respective lat,lons:
xunq, yunq = np.unique(x), np.unique(y)
nx, ny = len(xunq), len(yunq)
lat = [ utm.to_latlon(x[i]*1000,y[i]*1000,16,'P')[0] for i in np.arange(0,len(x)) ]
lon = [ utm.to_latlon(x[i]*1000,y[i]*1000,16,'P')[1] for i in np.arange(0,len(x)) ]
glat, glon = np.reshape(lat,(ny,nx)),  np.reshape(lon,(ny,nx))
#plt.figure()
#plt.imshow(glat)
#plt.colorbar()
#plt.show()
#plt.figure()
#plt.imshow(glon)
#plt.colorbar()
#plt.show()
#Get x,y coordinates of all the corners of the square cells centred on each x,y:
x2unq = [v-(xunq[1]-xunq[0])/2. for v in xunq]
x2unq.append(x2unq[-1]+(xunq[1]-xunq[0]))
y2unq = [v-(yunq[1]-yunq[0])/2. for v in yunq]
y2unq.append(y2unq[-1]+(yunq[1]-yunq[0]))
nx2, ny2 = len(x2unq), len(y2unq)
x2grd,y2grd=np.meshgrid(x2unq,y2unq)
x2, y2 = np.reshape(x2grd,(nx2*ny2)), np.reshape(y2grd,(nx2*ny2))
lat2 = [ utm.to_latlon(x2[i]*1000,y2[i]*1000,16,'P')[0] for i in np.arange(0,len(x2)) ]
lon2 = [ utm.to_latlon(x2[i]*1000,y2[i]*1000,16,'P')[1] for i in np.arange(0,len(x2)) ]
glat2, glon2 = np.reshape(lat2,(ny2,nx2)),  np.reshape(lon2,(ny2,nx2))
#plt.figure()
#plt.imshow(glat2)
#plt.colorbar()
#plt.show()
#plt.figure()
#plt.imshow(glon2)
#plt.colorbar()
#plt.show()
#####

#####SET BIN COLOURS
cmap = mpl.colors.ListedColormap(colsHex[1:-1])
cmap.set_under(colsHex[0])
cmap.set_over(colsHex[-1])
norm = mpl.colors.BoundaryNorm(boundaries=binLims,ncolors=5)
#####

###conc data plots:
for j,file in enumerate(filePaths):
    #Read in concentration data:
    f = open(file,'r')
    lines = f.read().splitlines()
    f.close
    #Process concentration data into desired format:
    conc = np.array([float(X) for X in lines])*100**3 #ug/cm^3 -> ug/m^3
    concAry=np.reshape(conc,(ny,nx)) #Reshape data onto latlon grid
    concMask = np.ma.masked_array(concAry, concAry<binLims[0]) #apply mask to all concs below lower limit
    #Plot on google maps:
    gmap = gmplot.GoogleMapPlotter(min(lat)+np.ptp(lat)/2.,min(lon)+np.ptp(lon)/2.,zoom=11)
    for i in np.arange(0,nx):
        for j in np.arange(0,ny):
            for k in np.arange(0,len(binLims)-1):
                if concAry[j,i] > binLims[k] and concAry[j,i] <= binLims[k+1]:
                    gmap.polygon((glat2[j+1,i],glat2[j,i],glat2[j,i+1],glat2[j+1,i+1]),
                                  (glon2[j+1,i],glon2[j,i],glon2[j,i+1],glon2[j+1,i+1]),
                                  color=colsHex[k+1],edge_width=0.001)
            if conc[j] > binLims[-1]:
                gmap.polygon((glat2[j+1,i],glat2[j,i],glat2[j,i+1],glat2[j+1,i+1]),
                                  (glon2[j+1,i],glon2[j,i],glon2[j,i+1],glon2[j+1,i+1]),
                                  color=colsHex[-1],edge_width=0.001)
    HTMLfile = 'google_'+ file[-17:-4] +'.html'
    print("Writing out file "+HTMLfile)
    gmap.draw(os.path.join(outDir,HTMLfile))
#####



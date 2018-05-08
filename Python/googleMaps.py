#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Script name: masaya_conc.py
Author: JO'N
Date: January 2018
Purpose: Used to generate an interactive Google map showing SO2 concentrations around the
         Masaya volcano, for one output time, as predicted by the CALPUFF dispersion model
Usage: python masaya_conc.py <concFile>
        <concFile> - CALPUFF SO2 output file with naming convention 'concrec******.dat'
Output: map\_concrec******.png - A static image file of the SO2 plume (with no basemap).
        map\_concrec******.html - An interactive webpage of the SO2 plume (Google basemap).
"""

import numpy as np
import matplotlib.pyplot as plt
import matplotlib as mpl
import os
import utm
import gmplot
import argparse

###Read in command line arguments:
parser = argparse.ArgumentParser()
parser.add_argument("concFile", help="absolute/relative path to concrec data file",type=str)
args = parser.parse_args()
assert os.path.exists(args.concFile), "concrec data file not found. Check path."
concFile=args.concFile
#concFile=os.environ['HOME']+'/Data/concrec010001.dat'

scriptPath=os.path.dirname(os.path.realpath(__file__))

def Read_Two_Column_File(file_name):
    with open(file_name, 'r') as data:
        x = []
        y = []
        for line in data:
            p = line.split()
            x.append(float(p[0]))
            y.append(float(p[1]))

    return x, y

###spatial data:
x, y = Read_Two_Column_File(os.path.abspath(os.path.join(scriptPath,'../Data/xy_masaya.dat'))) #reads in data
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

###set area for static plot based on x,y extent, and colour bins basd on AQ limits:
xrange, yrange = np.ptp(x), np.ptp(y)
longaxs=7
xrplt = xrange/max(xrange,yrange) * longaxs
yrplt = yrange/max(xrange,yrange) * longaxs
binLims=[20,350,600,2600,9000,14000]
colsHex=['#FFFFFF','#008000','#FFFF00','#FF6600','#FF0000','#800080','#8F246B']
cmap = mpl.colors.ListedColormap(colsHex[1:-1])
cmap.set_under(colsHex[0])
cmap.set_over(colsHex[-1])
norm = mpl.colors.BoundaryNorm(boundaries=binLims,ncolors=5)

###conc data plots:
f = open(concFile,'r')
lines = f.read().splitlines()
f.close
conc = np.array([float(x) for x in lines])*100**3 #ug/cm^3 -> ug/m^3
concAry=np.reshape(conc,(ny,nx))
concMask = np.ma.masked_array(concAry, concAry<binLims[0])
#static plot (no basemap):
fig = plt.figure(figsize=(xrplt+xrplt/3.,yrplt))
figaxs = fig.add_axes([0.15,0.15,0.55,0.8])
plt.pcolormesh(glon,glat,concMask,norm=norm,cmap=cmap)
figaxs.set_xlabel('longitude (decimal deg)')
figaxs.set_ylabel('latitude (decimal deg)')
cbaxs = fig.add_axes([0.75,0.05,0.1,0.9])
cb = mpl.colorbar.ColorbarBase(cbaxs, cmap=cmap,
                            norm=norm,
                            boundaries=[0.] + binLims + [100000.],
                            extend='both',
                            extendfrac='auto',
                            ticks=binLims,
                            spacing='uniform',
                            orientation='vertical')
cb.set_label('SO2 concentration (ug/m^3)')
PNGfile = 'map_'+ concFile[-17:-4] +'.png'
plt.savefig(PNGfile)
#interactive plot (gmplot):
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
HTMLfile = 'map_'+ concFile[-17:-4] +'.html'
gmap.draw(HTMLfile)




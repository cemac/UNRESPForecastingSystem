#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Script name: staticMaps.py
Author: JO'N
Date: March 2018
Purpose: Used to generate a series (48hrs) of static maps showing SO2 concentrations around the
         Masaya volcano, as predicted by the CALPUFF dispersion model
Usage: ./staticMaps.py <concDir>
        <concDir> - Directory containing CALPUFF SO2 output files with naming convention 'concrec010**.dat', \
        where '**' goe from '01' through to '48'
        <xyFile> - Path to data file containing the x,y coordinates of each output point
Output: static_concrec010**.png - A series of static image files of the SO2 plume (with a basemap).
"""

import numpy as np
from mpl_toolkits.basemap import Basemap
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
import matplotlib as mpl
import os
import utm
import datetime as dt
import pytz
from dateutil.parser import parse
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
#parser = argparse.ArgumentParser(description = "Used to generate a series (48hrs) of static maps showing \
#         SO2 concentrations around the Masaya volcano, as predicted by the CALPUFF dispersion model")
#parser.add_argument("concDir", help="absolute/relative path to directory containing CALPUFF SO2 output files, \
#           with expected naming convention 'concrec010**.dat', where '**' goes from '01' through to '48'",type=str)
#parser.add_argument("xyFile", help="absolute/relative path to data file containing the x,y coordinates of each output point",type=str)
#parser.add_argument("date", help="date of forecast in format YYYYMMDD",type=str)
#parser.add_argument("outDir", help="absolute/relative path to output directory for the generated png files",type=str)
#args = parser.parse_args()
#concDir=args.concDir
#xyFile=args.xyFile
#date=args.date
#outDir=args.outDir
#
concDir=os.getenv('HOME')+'/Data/UNRESP/CalpuffOutput_JJO'
xyFile=os.getenv('HOME')+'/gitRepos/UNRESP/Data/xy_masaya.dat'
date='20171204'
outDir=os.getenv('HOME')+'/Data/UNRESP/JJO_out'
#####

####PARAMETERS
nConcFiles=48 #Number of conc files to process (48 = full 2 days)
binLims=[10,350,600,2600,9000,14000] #SO2 bin limits
colsHex=['#FFFFFF','#008000','#FFFF00','#FF6600','#FF0000','#800080','#8F246B'] #Hex codes for SO2 colour bins
xpixels=1500 #Zoom lvel for satellite basemap (higher=bigger file sizes)
towns=(' El Panama',' Rigoberto',' Pacaya',' El Crucero',' La Concepcion',' Masaya',' San Marcos',
       ' San Rafael del Sur',' Diriamba',' Jinotepe',' Masatepe')
townCoords=((-86.2058,11.972),(-86.2021,11.9617),(-86.3013,11.9553),
            (-86.3113,11.9923),(-86.189772,11.936161),(-86.096053,11.973523),
            (-86.20317,11.906584),(-86.43639,11.847034),(-86.239592,11.85632),
            (-86.19993,11.85017),(-86.143758,11.91512))
cities=(' MANAGUA',)
cityCoords=((-86.29,12.12),)
volcCoords=(-86.1608, 11.9854)
so2title='Atmospheric SO2 concentrations at ground level (hourly means). GCRF UNRESP'
#####

#####CHECK PATHS/FILES EXIST
assert os.path.exists(concDir), "concDir directory does not exist. Check path."
assert os.path.exists(xyFile), "xyFile data file does not exist. Check path."
assert os.path.exists(outDir), "outDir directory does not exist. Check path."
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

####GET DATES/TIMES
startDate=pytz.utc.localize(parse(date))
dates=[]
for i in range(nConcFiles):
    iDate=startDate+dt.timedelta(hours=i+1)
    dates.append(iDate)
#####

#####READ IN X,Y DATA AND CONVERT TO LAT,LON
x, y = Read_Two_Column_File(xyFile) #read in x,y data
xunq, yunq = np.unique(x), np.unique(y) #get unique x,y coordinates
nx, ny = len(xunq), len(yunq) #number of unique x,y coordinates
#Use utm package to convert from x,y to lat,lon...
#...Nicaragua is UTM zone 16P, and we must convert to metres first:
lat = [ utm.to_latlon(x[i]*1000,y[i]*1000,16,'P')[0] for i in np.arange(0,len(x)) ]
lon = [ utm.to_latlon(x[i]*1000,y[i]*1000,16,'P')[1] for i in np.arange(0,len(x)) ]
#Create gridded field of lat,lon of appropriate size:
glat, glon = np.reshape(lat,(ny,nx)),  np.reshape(lon,(ny,nx))
#Also grab range for later plot
latMin=min(lat)
latMax=max(lat)
lonMin=min(lon)
lonMax=max(lon)
#####

#####SET BIN COLOURS
cmap = mpl.colors.ListedColormap(colsHex[1:-1])
cmap.set_under(colsHex[0])
cmap.set_over(colsHex[-1])
norm = mpl.colors.BoundaryNorm(boundaries=binLims,ncolors=5)
#####

#####PLOT
plt.ioff() #turn off interactive plotting
#Download ESRI image only once:
bmap = Basemap(llcrnrlon=lonMin,llcrnrlat=latMin,urcrnrlon=lonMax,urcrnrlat=latMax)
esri_url = \
"http://server.arcgisonline.com/ArcGIS/rest/services/ESRI_Imagery_World_2D/MapServer/export?\
bbox=%s,%s,%s,%s&\
bboxSR=%s&\
imageSR=%s&\
size=%s,%s&\
dpi=%s&\
format=png32&\
f=image" %\
(bmap.llcrnrlon,bmap.llcrnrlat,bmap.urcrnrlon,bmap.urcrnrlat,bmap.epsg,bmap.epsg,xpixels,bmap.aspect*xpixels,96)
ESRIimg = mpimg.imread(esri_url)
for j,file in enumerate(filePaths):
    #Read in concentration data:
    f = open(file,'r')
    lines = f.read().splitlines()
    f.close
    #Process concentration data into desired format:
    conc = np.array([float(x) for x in lines])*100**3 #ug/cm^3 -> ug/m^3
    concAry=np.reshape(conc,(ny,nx)) #Reshape data onto latlon grid
    concMask = np.ma.masked_array(concAry, concAry<binLims[0]) #apply mask to all concs below lower limit
    #Plot on basemap:
    plt.figure(figsize=(12,9))
    bmap = Basemap(llcrnrlon=lonMin,llcrnrlat=latMin,urcrnrlon=lonMax,urcrnrlat=latMax)
    bmap.imshow(ESRIimg,origin='upper')
    bmap.pcolormesh(glon,glat,concMask,norm=norm,cmap=cmap,alpha=0.5)
    cbar=bmap.colorbar(location='bottom',pad='20%',cmap=cmap,norm=norm,boundaries=[0.] + binLims + [100000.],
                 extend='both',extendfrac='auto',ticks=binLims,spacing='uniform',label='SO2 concentration (ug/m3)')
    cbar.solids.set(alpha=1)
    latTicks=np.arange(round(latMin,1),round(latMax,1)+0.1,0.1)
    lonTicks=np.arange(round(lonMin,1),round(lonMax,1)+0.1,0.2)
    bmap.drawparallels(latTicks,labels=[1,0,0,0],linewidth=0.0)#labels=[left,right,top,bottom]
    bmap.drawmeridians(lonTicks,labels=[0,0,0,1],linewidth=0.0)
    for i,town in enumerate(towns):
        plt.plot(townCoords[i][0],townCoords[i][1],'ok',markersize=3)
        plt.text(townCoords[i][0],townCoords[i][1],town,fontsize=8)
    for i,city in enumerate(cities):
        plt.plot(cityCoords[i][0],cityCoords[i][1],'sk',markersize=4)
        plt.text(cityCoords[i][0],cityCoords[i][1],city,fontsize=11)
    plt.plot(volcCoords[0],volcCoords[1],'^r',markersize=5)
    plt.suptitle(so2title)
    plt.title(dates[j].strftime('%c'),fontsize=24)
    PNGfile = 'static_'+ file[-17:-4] +'.png'
    PNGpath=os.path.join(outDir,PNGfile)
    plt.savefig(PNGpath)
    plt.close()
plt.ion() #turn on interactive ploting
#####    

import netCDF4
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.image as mpimg
from mpl_toolkits.mplot3d import axes3d
import matplotlib as mpl
from mpl_toolkits.basemap import Basemap
from matplotlib.font_manager import FontProperties
import os
import datetime as dt
import pytz
import utm
import gmplot
import cartopy.crs as ccrs
import cartopy.io.img_tiles as cimgt
import cartopy
import cartopy.feature as cfeat
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER
import netCDF4
from dateutil.parser import parse
from maptoolkit import *
file2read = netCDF4.Dataset('conc.nc', 'r')
lat = file2read.variables['lati'][:]
lon = file2read.variables['long'][:]
nhours = file2read.variables['nhours'][:]
count = file2read.variables['countpoints'][:]
datahome = "/nfs/see-fs-01_users/earhbu/UNRESP_SPACE/UNRESPForecastingSystem/"
nConcFiles = 48
binLims = [10, 350, 600, 2600, 9000, 14000]  # SO2 bin limits
xyFile = "../data/xy_masaya.dat"
glat, glon, latMin, latMax, lonMin, lonMax, ny, nx = genxy(xyFile)
plt.figure(figsize=(16, 12))
im = gen_im(lonMin, latMin, lonMax, latMax, imtype="World_Shaded_Relief",)
so2title = (r'june 2018 - feb 2019. probability greater than 100ug/m$^3$'+' \n (cut off reduced to 0.001)')
bmap = Basemap(llcrnrlon=lonMin, llcrnrlat=latMin,
               urcrnrlon=lonMax, urcrnrlat=latMax)
bmap.imshow(im, origin='upper')
concAry = np.reshape(count, (ny, nx))
#concAry[concAry < 0.05] = np.nan
concmap = concAry / nhours*100
concmap = np.ma.masked_array(concmap, concmap < 0.001)
bmap.contourf(glon, glat, concmap, cmap=plt.cm.plasma_r)
cbar = bmap.colorbar(location='bottom', pad='20%', extend='both', extendfrac='auto', cmap=plt.cm.plasma_r)
cbar.set_label(label=('probability %'), fontsize=18)
cbar.ax.tick_params(labelsize=16)
cbar.solids.set(alpha=1)
latTicks = np.arange(round(latMin, 1), round(latMax, 1) + 0.1, 0.1)
lonTicks = np.arange(round(lonMin, 1), round(lonMax, 1) + 0.1, 0.2)
bmap.drawparallels(latTicks, labels=[1, 0, 0, 0], linewidth=0.0,
                   fontsize=16)
bmap.drawmeridians(lonTicks, labels=[0, 0, 0, 1], linewidth=0.0,
                   fontsize=16)
towns = (' El Panama', ' Rigoberto', ' Pacaya', ' El Crucero',
         ' La Concepcion', ' Masaya', ' San Marcos',
         ' San Rafael del Sur', ' Diriamba', ' Jinotepe',
         ' Masatepe')
townCoords = ((-86.2058, 11.972), (-86.2021, 11.9617),
            (-86.3013, 11.9553), (-86.3113, 11.9923),
            (-86.189772, 11.936161), (-86.096053, 11.973523),
            (-86.20317, 11.906584), (-86.43639, 11.847034),
            (-86.239592, 11.85632), (-86.19993, 11.85017),
            (-86.143758, 11.91512))
cities = (' MANAGUA',)
cityCoords = ((-86.29, 12.12),)
volcCoords = (-86.1608, 11.9854)
font = FontProperties()
font.set_weight('bold')
font.set_family('monospace')
tc = 'k'
for i, town in enumerate(towns):
    plt.plot(townCoords[i][0], townCoords[i]
             [1], 'ok', markersize=4)
    plt.text(townCoords[i][0], townCoords[i][1], town,
             color=tc, fontproperties=font, fontsize=12)
for i, city in enumerate(cities):
    plt.plot(cityCoords[i][0], cityCoords[i]
             [1], 'sk', markersize=6)
    plt.text(cityCoords[i][0], cityCoords[i][1], city,
             fontproperties=font, fontsize=16)
font0 = FontProperties()
font0.set_family('monospace')
plt.plot(volcCoords[0], volcCoords[1], '^r', markersize=6)
plt.suptitle(so2title, fontsize=24)
plt.savefig('test.png', dpi=250)
plt.close()

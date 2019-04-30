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
from dateutil.parser import parse
from maptoolkit import *


class stamps(object):
    def _init_():
        s.nConcFiles = 48
        s.binLims = [10, 350, 600, 2600, 9000, 14000]  # SO2 bin limits
        xyFile = "../../data/xy_masaya.dat"
        s.glat, s.glon, s.latMin, s.latMax, s.lonMin, s.lonMax, s.ny, nx = genxy(xyFile)
        im = gen_im(lonMin, latMin, lonMax, latMax, imtype="World_Shaded_Relief",)
        topo = 'World_Shaded_Relief'
        binLimsSO4 = [1E-8, 12, 35, 55, 150, 250]  # SO4 bin limits from:
        # http://mkwc.ifa.hawaii.edu/vmap/hysplit/
        colsHex = ['#FFFFFF', '#0cec0c', '#FFFF00', '#FF6600', '#FF0000',
                   '#800080', '#8F246B']  # Hex codes for SO2 colour bins
        towns = (' El Panama', ' Rigoberto', ' Pacaya', ' El Crucero',
                 ' La Concepcion', ' Masaya', ' San Marcos', ' San Rafael del Sur',
                 ' Diriamba', ' Jinotepe', ' Masatepe')
        townCoords = ((-86.2058, 11.972), (-86.2021, 11.9617), (-86.3013, 11.9553),
                  (-86.3113, 11.9923), (-86.189772, 11.936161),
                  (-86.096053, 11.973523), (-86.20317, 11.906584),
                  (-86.43639, 11.847034), (-86.239592, 11.85632),
                  (-86.19993, 11.85017), (-86.143758, 11.91512))
        cities = (' MANAGUA',)
        cityCoords = ((-86.29, 12.12),)
        volcCoords = (-86.1608, 11.9854)
        font = FontProperties()
        font.set_weight('bold')
        font.set_family('monospace')
        # SET BIN COLOURS
        cmap = mpl.colors.ListedColormap(colsHex[1:-1])
        cmap.set_under(colsHex[0])
        cmap.set_over(colsHex[-1])
        normso4 = mpl.colors.BoundaryNorm(boundaries=binLimsSO4, ncolors=5)
        norm = mpl.colors.BoundaryNorm(boundaries=binLims, ncolors=5)
        glat, glon, latMin, latMax, lonMin, lonMax, ny, nx = genxy(xyFile)
        fle = '2018201806av.txt'

    def plot_av_stamp(s, ita, im, fle, SOX):
        """Plot static maps
        """
        font = s.font
        SOXf = r'SO$_' + SOX[-1] + '$'
        so2title = ('Atmospheric ' + SOX + ' concentrations at ' +
                    'ground level (hourly means). \n GCRF UNRESP')
        plt.figure(figsize=(16, 12))
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
        bmap.pcolormesh(glon, glat, concA,
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
                     color='k', fontproperties=font, fontsize=12)
        for i, city in enumerate(cities):
            plt.plot(s.cityCoords[i][0], s.cityCoords[i]
                     [1], 'sk', markersize=6)
            plt.text(s.cityCoords[i][0], s.cityCoords[i][1], city,
                     fontproperties=font, fontsize=16)
        font0 = FontProperties()
        font0.set_family('monospace')
        plt.plot(s.volcCoords[0], volcCoords[1], '^r', markersize=6)
        PNGfile = 'test.png'
        print("Writing out file " + PNGfile)
        PNGpath = os.path.join('images/', PNGfile)
        plt.savefig(PNGpath, dpi=250)
        plt.close()

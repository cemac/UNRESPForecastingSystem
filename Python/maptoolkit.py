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
import os
import sys
import datetime as dt
import numpy as np
import matplotlib.image as mpimg
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib.font_manager import FontProperties
from mpl_toolkits.basemap import Basemap
import pytz
import utm
import gmplot
from dateutil.parser import parse
import warnings
warnings.filterwarnings("ignore")
# University System python may be broken
# If some one insists on using it...
BACKEND = mpl.get_backend()
if BACKEND == 'Qt4Agg' and sys.version_info[0] == 2:
    # Fix the backend
    print('swapping to Agg Backend')
    mpl.pyplot.switch_backend('Agg')


# defaults
# Defualt directory concentrations are in
CONCDIR = "../CALPUFF_OUT/CALPUFF/"
# Default location of xy ascii file
XYFILE = "../data/xy_masaya.dat"


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


def concfiles(n_conc_files, conc_dir, SOX='SO2'):
    """concfiles
    description: generate list of concentration files
        n_conc_files (int): number of concentration files e.g. 48 or 24
        conc_dir (str): path to directory containing conc files
        SOX (str): SO species, default = 'SO2'
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
        assert os.path.exists(f_path), "File " + \
            f_path + " not found. Check path."
    return filenames, file_paths


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


def gen_gxy(xy_file):
    """gen_gxy
    description:
        Reading the xy_file and Convert the list of UTM points and to lat lons
        In format required for google
    args:
        xy_file (str): filename for ascii file of xy points (2 cols)
    returns:
        glat (array): gridded latitudes
        glon (array): gridded longitudes
        lat (list): list of latitudes (corners)
        lon (list): list of longitudes (corners)
        ny (int): number of x points
        nx (int): number of y points
    """
    # READ IN X,Y DATA AND CONVERT TO LAT,LON
    x, y = read_two_column_file(xy_file)  # read in x,y data
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
    glat, glon = np.reshape(lat, (ny, nx)), np.reshape(lon, (ny, nx))
    return glat, glon, lat, lon, ny, nx


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


class MasayaMaps():
    '''Plot Masaya Maps
    Consitsts of X plotting functions that output 48 static maps
    members:
        plot_staticmap: plot either topo or statellite
        plot_staticmap1: plot either topo or statellite
        plot_googlemaps: plot google maps html
        plot_googlemap1: plot google maps html
    Args:
        date (str): YYYYMMDD string
        conc_dir (str):path to conc directory
                       default: "../CALPUFF_OUT/CALPUFF/"
        xy_file (str): path to xy file, default: "../data/xy_masaya.dat"
        n_conc_files (int) = 48
    attributes:
        outDir = "../vis/" + date
        sat = 'World_Imagery'
        topo = 'World_Shaded_Relief'
        n_conc_files = 48
        binLims: [10, 350, 600, 2600, 9000, 14000]  SO2 bin limits
        binLimsSO4: [1E-8, 12, 35, 55, 150, 250] SO4 bin limits
        colsHex: set hex codes for colours
        towns = El Panama, Rigoberto, Pacaya, El Crucero, La Concepcion,
                Masaya, San Marcos, San Rafael del Sur, Diriamba, Jinotepe,
                Masatepe
        townCoords: latlons of the above towns
        cities: MANAGUA
        cityCoords: latlon MANAGUA -86.29, 12.12)
        volcCoords: latlon of Maysaya -86.1608, 11.9854
        googlekey: API Key will be read in file if there and required
    '''

    def __init__(self, date, conc_dir=CONCDIR, xy_file=XYFILE,
                 n_conc_files=48):
        """Initialise with date, concrec file location, xy file locations and
           forecast length e.g 48 hours or 24
        """
        self.conc_dir = conc_dir + date
        self.xy_file = xy_file
        self.outDir = "../vis/" + date
        self.sat = 'World_Imagery'
        self.topo = 'World_Shaded_Relief'
        # Number of conc files to process (48 = full 2 days)
        self.n_conc_files = n_conc_files
        self.binLims = [10, 350, 600, 2600, 9000, 14000]  # SO2 bin limits
        self.binLimsSO4 = [1E-8, 12, 35, 55, 150, 250]  # SO4 bin limits from:
        # http://mkwc.ifa.hawaii.edu/vmap/hysplit/
        self.colsHex = ['#FFFFFF', '#0cec0c', '#FFFF00', '#FF6600', '#FF0000',
                        '#800080', '#8F246B']  # Hex codes for SO2 colour bins
        self.towns = (' El Panama', ' Rigoberto', ' Pacaya', ' El Crucero',
                      ' La Concepcion', ' Masaya', ' San Marcos',
                      ' San Rafael del Sur', ' Diriamba', ' Jinotepe',
                      ' Masatepe')
        self.townCoords = ((-86.2058, 11.972), (-86.2021, 11.9617),
                           (-86.3013, 11.9553), (-86.3113, 11.9923),
                           (-86.189772, 11.936161), (-86.096053, 11.973523),
                           (-86.20317, 11.906584), (-86.43639, 11.847034),
                           (-86.239592, 11.85632), (-86.19993, 11.85017),
                           (-86.143758, 11.91512))
        self.cities = (' MANAGUA',)
        self.cityCoords = ((-86.29, 12.12),)
        self.volcCoords = (-86.1608, 11.9854)
        self.font = FontProperties()
        self.font.set_weight('bold')
        self.font.set_family('monospace')

        # CHECK PATHS/FILES EXIST
        assert os.path.exists(self.conc_dir), ("CALPUFF output directory " +
                                               "does not exist for this date.")
        assert os.path.exists(self.xy_file), ("Cannot find data/xy_masaya.da" +
                                              "t coordinate data file.")
        assert os.path.exists(self.outDir), ("Output directory vis/<date> " +
                                             "does not exist.")
        self.filenames, self.file_paths = concfiles(self.n_conc_files,
                                                    self.conc_dir, SOX='SO2')

        # GET DATES/TIMES
        startDate = pytz.utc.localize(parse(date))
        dates = []
        for i in range(self.n_conc_files):
            iDate = startDate + dt.timedelta(hours=i + 1)
            dates.append(iDate)
        self.dates = dates
        # SET BIN COLOURS
        self.cmap = mpl.colors.ListedColormap(self.colsHex[1:-1])
        self.cmap.set_under(self.colsHex[0])
        self.cmap.set_over(self.colsHex[-1])
        self.normso4 = mpl.colors.BoundaryNorm(
            boundaries=self.binLimsSO4, ncolors=5)
        self.norm = mpl.colors.BoundaryNorm(boundaries=self.binLims, ncolors=5)
        (self.glat, self.glon, self.latMin, self.latMax,
         self.lonMin, self.lonMax, self.ny, self.nx) = genxy(self.xy_file)
        (self.Gglat, self.Gglon, self.Glat, self.Glon, self.Gny,
         self.Gnx) = gen_gxy(self.xy_file)
        # GoogleKey assigned later
        self.googlekey = ""

    def plot_staticmaps(self, maptype, SOX='SO2'):
        """plot_staticmaps
        description
            plot png per hour for either topo or satellite and SOX set to SO2
            SO4
        args:
            maptype (str): satellite or topo
            SOX (str): SO2 or SO4, default'SO2'
        returns:
            24/48 (n_conc_files) pngs of concentration on map
        """
        if maptype == 'satellite':
            Imflag = self.sat
            tc = 'w'
            out = ''
        elif maptype == 'topo':
            Imflag = self.topo
            tc = 'k'
            out = 'topo'
        else:
            print('Not a valid option... setting to topo')
            Imflag = self.topo
            tc = 'k'
            out = 'topo'
        im = gen_im(self.lonMin, self.latMin, self.lonMax,
                    self.latMax, imtype=Imflag)
        file_paths = concfiles(self.n_conc_files, self.conc_dir, SOX=SOX)[1]
        for i, fname in enumerate(file_paths):
            self.plot_staticmap1(i, im, tc, fname, out, SOX=SOX)

    def plot_staticmap1(self, ita, im, tc, fname, out, SOX):
        """plot_staticmap1
        description
            plot a single for either topo or satellite and SOX set to SO2
            SO4
        args:
            ita (int): index of conc file list (which sim hour)
            im (basemap image): background image
            tc (str): text colour
            file_paths: list of conc files
            out (str): filename
            SOX (str): SO2 or SO4, default'SO2'
        returns:
            png of concentration on map
        """
        SOXf = r'SO$_' + SOX[-1] + '$'
        so2title = ('Atmospheric ' + SOXf + ' concentrations at ' +
                    'ground level (hourly means). \n GCRF UNRESP')
        plt.figure(figsize=(16, 12))
        fle = fname
        if SOX == "SO4":
            binLims = self.binLimsSO4
            norm = self.normso4
        else:
            binLims = self.binLims
            norm = self.norm
        concA = conc_array(self.ny, self.nx, fle, binLims)[0]
        latMin, latMax, lonMin = self.latMin, self.latMax, self.lonMin
        lonMax = self.lonMax
        bmap = Basemap(llcrnrlon=lonMin, llcrnrlat=latMin,
                       urcrnrlon=lonMax, urcrnrlat=latMax)
        bmap.imshow(im, origin='upper')
        bmap.pcolormesh(self.glon, self.glat, concA,
                        norm=norm, cmap=self.cmap, alpha=0.5)
        cbar = bmap.colorbar(location='bottom', pad='20%', cmap=self.cmap,
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
        for i, town in enumerate(self.towns):
            plt.plot(self.townCoords[i][0], self.townCoords[i]
                     [1], 'ok', markersize=4)
            plt.text(self.townCoords[i][0], self.townCoords[i][1], town,
                     color=tc, fontproperties=self.font, fontsize=12)
        for i, city in enumerate(self.cities):
            plt.plot(self.cityCoords[i][0], self.cityCoords[i]
                     [1], 'sk', markersize=6)
            plt.text(self.cityCoords[i][0], self.cityCoords[i][1], city,
                     fontproperties=self.font, fontsize=16)
        font0 = FontProperties()
        font0.set_family('monospace')
        plt.plot(self.volcCoords[0], self.volcCoords[1], '^r', markersize=6)
        plt.suptitle(so2title, fontsize=24)
        plt.title(self.dates[ita].strftime('%c %z'), fontsize=18)
        PNGfile = SOX + '_static_' + out + fle[-17:-4] + '.png'
        print("Writing out file " + PNGfile)
        PNGpath = os.path.join(self.outDir, PNGfile)
        plt.savefig(PNGpath, dpi=250)
        plt.close()

    def plot_google(self, SOX='SO2'):
        """plot_google
        description
            plot google html per hour for either SOX set to SO2 concentrations
            SO4. Requires API key - will exit gracefully if no key found.
        args:
            SOX (str): SO2 or SO4, default'SO2'
        returns:
            24/48 (n_conc_files) google htmls
        """
        codesFile = os.path.join('GM_API_KEY.txt')
        try:
            f = open(codesFile, 'r')
            lines = f.readlines()
            f.close()
            self.googlekey = lines[0].strip()
            file_paths = concfiles(self.n_conc_files,
                                   self.conc_dir, SOX=SOX)[1]
            for i, fname in enumerate(file_paths):
                self.plot_googlemap1(i, fname, SOX)
        except FileNotFoundError:
            print("### WARNING #### GM_API_KEY.txt not found \n turning off " +
                  "google maps plotter")
            print(" If you would like to plot goolge maps \n please see" +
                  " README for API key information.")

    def plot_googlemap1(self, ita, file_paths, SOX):
        """plot_staticmaps
        description
            plot a single google html for either SOX set to SO2 or SO4
        args:
            ita (int): index of conc file list (which sim hour)
            file_paths: list of conc files
            SOX (str): SO2 or SO4, default'SO2'
        returns:
            png of concentration on map
        """
        gKey = self.googlekey
        fle = file_paths
        if SOX == "SO4":
            binLims = self.binLimsSO4
        else:
            binLims = self.binLims
        glat, glon, lat, lon = self.Gglat, self.Gglon, self.Glat, self.Glon
        concA, conc = conc_array(self.ny, self.nx, fle, binLims)
        gmap = gmplot.GoogleMapPlotter(min(lat) + np.ptp(lat) / 2.,
                                       min(lon) + np.ptp(lon) / 2., zoom=11,
                                       apikey=gKey)
        for i in np.arange(0, self.nx):
            for j in np.arange(0, self.ny):
                for k in np.arange(0, len(self.binLims) - 1):
                    if (
                            concA[j, i] > self.binLims[k] and
                            concA[j, i] <= binLims[k + 1]
                    ):
                        gmap.polygon((glat[j + 1, i], glat[j, i],
                                      glat[j, i + 1], glat[j + 1, i + 1]),
                                     (glon[j + 1, i], glon[j, i],
                                      glon[j, i + 1], glon[j + 1, i + 1]),
                                     color=self.colsHex[k + 1],
                                     edge_width=0.001)
                if conc[j] > binLims[-1]:
                    gmap.polygon((glat[j + 1, i], glat[j, i],
                                  glat[j, i + 1], glat[j + 1, i + 1]),
                                 (glon[j + 1, i], glon[j, i],
                                  glon[j, i + 1], glon[j + 1, i + 1]),
                                 color=self.colsHex[-1], edge_width=0.001)
        HTMLfile = SOX + '_google_' + fle[-17:-4] + '.html'
        print("Writing out file " + HTMLfile)
        gmap.draw(os.path.join(self.outDir, HTMLfile))

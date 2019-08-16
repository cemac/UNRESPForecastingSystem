#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Timeseries Analysis
.. module:: Timeseries
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
import numpy as np
import matplotlib.pyplot as plt
from sklearn import neighbors
from statistics import mode
from datetime import datetime
import maptoolkit as mtk
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
# Emission
Em = "SO2"
# Default location of xy ascii file
XYFILE = "../data/xy_masaya.dat"
# observations
obs = '/scratch/Projects/UNRESP_Data/OBS'
# stations and coordinates
station1 = ['ElPanama', (-86.2058, 11.972)]
station2 = ['Pacaya', (-86.3013, 11.9553)]
# Models
ECMWF = '/scratch/Projects/UNRESP_Data/ECMWF'
NAM = '/scratch/Projects/UNRESP_Data/NAM'
# Unit Testing
Stage1 = False
Stage2 = False
Stage3 = True
Stage4 = False
Stage5 = False
Stage6 = False
# --------------------------------------------------------------------------- #
#                   Stage 1: Extract Observational data                       #
#                                                                             #
# --------------------------------------------------------------------------- #


def ExtractTimeSeries(obs, station, Em):
    """ExtractTimeSeries
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
    df = pd.read_csv(glob.glob(obs + '/' + station + '*.csv')[0], index_col=0,
                     parse_dates=True)
    if Em == "SO2" and station == 'ElPanama':
        data = df[[df.columns[2]]]
        # extract units
        unit = df[[df.columns[1]]]
        unit = unit.dropna().iloc[0][0]
    elif Em == "SO2" and station == 'Pacaya':
        # Pacaya file formatted completely different
        data = df[[df.columns[0]]]
        unit = 'ug/m3'
    else:
        print('Current set up for SO2 only')
        return
    # Select March
    TS_raw = data['2017-03']
    # KNN Regressor (i.e. using fancy stats to fill in nans)
    # Use whole data set to train on minus the nans
    train = data.dropna()
    # Works on numpy arrays
    y = train.as_matrix()
    # Correspoing x values
    X = train.index.values
    # Whole time
    T = data.index.values
    n_neighbors = 48  # use surrounding 2 days
    weights = 'distance'  # Near values are weighted higher
    # Use scikit learns knn
    knn = neighbors.KNeighborsRegressor(n_neighbors, weights=weights)
    # Generate predicted values
    y_ = knn.fit(X.reshape(-1, 1), y.reshape(-1, 1)).predict(T.reshape(-1, 1))
    TS_KNN_ALL = data
    TS_KNN_ALL['KNN'] = y_
    # Extract March
    TS_KNN = TS_KNN_ALL['2017-03']
    return TS_raw, unit, TS_KNN


def plot_obs(TS_KNN, unit, station):
    """
    Description
    Args:
        TS_KNN(dataframe): Dataframe indexed by time with raw values and KNN
                           values
        unit(str): unit string for y label
        station(str): Station name string for title
    Returns:
        Scatter plot of Timeseries with KNN filled missing values.
    """
    TS_KNN['KNN'].plot(style='.')
    TS_KNN[TS_KNN.columns[0]].plot(style='.')
    plt.legend()
    plt.title('SO2 data for March (' + station + ')')
    plt.ylabel(unit)
    plt.tight_layout()
    plt.savefig(station + '_March2017_Obs.png')


if Stage1 is True:
    TS_raw, unit, TS_KNN = ExtractTimeSeries(obs, station1[0], Em)
    # plot_obs(TS_KNN, unit, station1[0])
    print('Extracted Observational Data')
    print('Please Note KNN filled data, is questionable in large data gaps')

# --------------------------------------------------------------------------- #
#                   Stage 2: Clean Observational data                         #
#                                                                             #
# --------------------------------------------------------------------------- #
if Stage2 is True:
    print('Stage2')
# --------------------------------------------------------------------------- #
#                   Stage 3: Extract Model data                               #
#                                                                             #
# --------------------------------------------------------------------------- #


def conc_array(ny, nx, file_path):
    """conc_array
    description:
        create an array of concentrations
    args:
        ny (int): number of x points
        nx (int): number of y points
        file_path (str): filename
    returns:
        conc (array): array of concentrations in ug/m^3
    """
    # Read in concentration data:
    f = open(file_path, 'r')
    lines = f.read().splitlines()
    f.close()
    # Process concentration data into desired format:
    conc = np.array([float(X) for X in lines]) * 100**3  # ug/cm^3 -> ug/m^3
    concAry = np.reshape(conc, (ny, nx))  # Reshape data onto latlon grid
    return concAry


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


def ExtractModelData(loc, model, XYFILE, Em):
    """ExtractModelData
    Description: extract data from concrec files (uses custom maptoolkit)
    Args:
        loc(Array):  Lat Lon of Station
        station(str): Station string for labeling data
        model(str):  Model string NAM or ECMWF
    Returns:
        calpuff_ts_df(DataFrame): Time series at that point
        calpuff_ts_array(Array): Numpy array area centered on station
    """
    # Generate xy grid
    (glat, glon, latMin, latMax, lonMin, lonMax, ny, nx) = mtk.genxy(XYFILE)
    # For each day
    for day in glob.glob(model + '/*'):
        conc_dir = day
        filenames, filePaths = concfiles(24, conc_dir, SOX='SO2')
        for i, fname in enumerate(filePaths):
            try:
                conc = conc_array(ny, nx, fname)
            except FileNotFoundError:
                conc = np.zeros_like(conc)
                conc[:] = np.nan
            if 'concall' in locals():
                concall = np.dstack((concall, conc))
            else:
                concall = conc
    # Convention: T, Y, X
    concall = concall.T
    # Now use loc to locate corresponding grid points
    # Extract only that point
    ilat, ilon = FindNearestLatLon(loc[1], loc[0], glat, glon)
    TS_station_point = concall[:, ilon, ilat]
    # Build array of points surrounding that point (9 points)
    TS_array = concall[:, ilon-1:ilon+2, ilat-1:ilat+2]
    TS_flat = TS_array.reshape(744, 9)
    # Average Array
    df = pd.DataFrame()
    df['TS_station_point'] = TS_station_point
    df['9pntmin'] = TS_flat.min(axis=1)
    df['9pntmax'] = TS_flat.max(axis=1)
    df['9ptmean'] = TS_flat.mean(axis=1)
    march = pd.date_range(start='2017-03-01', end='2017-04-01', freq='H')[0:-1]
    df.index = march
    # Gen 1D Timte array
    # Gen X, Y for conc array
    # Gen data frame (time, conc, conc av, conc max, conc mean)
    # return calpuff_ts_df, calpuff_ts_array, T, X, Y
    return df, TS_array, glat, glon


if Stage3 is True:
    ecmwf_df, E_TS_array, glat, glon = ExtractModelData(station1[1], ECMWF, XYFILE, Em)
    nam_df, n_TS_array, glat, glon = ExtractModelData(station1[1], NAM, XYFILE, Em)
    print('Stage3')
# --------------------------------------------------------------------------- #
#                         Stage 4: Statistics                                 #
#                                                                             #
# --------------------------------------------------------------------------- #
if Stage4 is True:
    print('Stage4')
# --------------------------------------------------------------------------- #
#                        Stage 5: Comparitons                                 #
#                                                                             #
# --------------------------------------------------------------------------- #
if Stage5 is True:
    print('Stage5')
# --------------------------------------------------------------------------- #
#                        Stage 6: Plotting                                    #
#                                                                             #
# --------------------------------------------------------------------------- #
if Stage6 is True:
    print('Stage6')

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
import pandas_profiling
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
station = station1
# Models
ECMWF = '/scratch/Projects/UNRESP_Data/ECMWF'
NAM = '/scratch/Projects/UNRESP_Data/NAM'
# Unit Testing
Stage1 = False
Stage2 = False
Stage3 = False
Stage4 = False


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
    # Tell it the weird format...
    def dateparse(x): return pd.datetime.strptime(x, '%d/%m/%Y %H:%M')
    df = pd.read_csv(glob.glob(obs + '/' + station + '*.csv')[0], index_col=0,
                     parse_dates=True, date_parser=dateparse)
    if Em == "SO2" and station == 'ElPanama':
        data = df[[df.columns[2]]]
        # Fill missing hours
        data = data.asfreq('H')
        # extract units
        unit = df[[df.columns[1]]]
        unit = unit.dropna().iloc[0][0]
    elif Em == "SO2" and station == 'Pacaya':
        # Pacaya file formatted completely different
        data = df[[df.columns[0]]]
        data = data.asfreq('H')
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


def plot_obs(observations, unit, station):
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
    observations[station[0] + '_KNN'].plot(style='.')
    observations[station[0] + '_raw'].plot(style='.')
    plt.legend()
    plt.title('SO2 data for March (' + station + ')')
    plt.ylabel(unit)
    plt.tight_layout()


if Stage1 is True:
    TS_raw, unit, TS_KNN = ExtractTimeSeries(obs, station[0], Em)
    observations = TS_KNN.rename(columns={TS_KNN.columns[0]: station[0] + '_raw',
                                          'KNN': station[0] + '_KNN'})
    observations.to_csv(station[0] + '_cleaned.csv')
    # plot_obs(TS_KNN, unit, station)
    print('Extracted Observational Data')
    print('Please Note KNN filled data, is questionable in large data gaps')

# --------------------------------------------------------------------------- #
#                   Stage 2: Extract Model data                               #
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
    TS_array = concall[:, ilon - 1:ilon + 2, ilat - 1:ilat + 2]
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


if Stage2 is True:
    ecmwf_df, E_TS_array, glat, glon = ExtractModelData(station[1], ECMWF,
                                                        XYFILE, Em)
    nam_df, n_TS_array, glat, glon = ExtractModelData(station[1], NAM,
                                                      XYFILE, Em)
    nam_df.to_csv('NAM_' + station[0] + '.csv')
    ecmwf_df.to_csv('ECMWF_' + station[0] + '.csv')
    print('Extracted Model Data')

# --------------------------------------------------------------------------- #
#                         Stage 3: Statistics                                 #
#                                                                             #
# --------------------------------------------------------------------------- #


def gencsvfile(station, ecmwf_df, nam_df, observations):
    """gencsvfile
    Combine all data extractions into 1 DataFrame
    """
    All = observations
    All['ECMWF_raw'] = ecmwf_df.TS_station_point
    All['ECMWF_min'] = ecmwf_df['9pntmin']
    All['ECMWF_max'] = ecmwf_df['9pntmax']
    All['ECMWF_area'] = ecmwf_df['9ptmean']
    All['NAM_raw'] = nam_df.TS_station_point
    All['NAM_min'] = nam_df['9pntmin']
    All['NAM_max'] = nam_df['9pntmax']
    All['NAM_area'] = nam_df['9ptmean']
    All.to_csv(station + 'Timeseries_obs_model_raw_processed.csv')


def RMSE(df, p, x):
    RMSE = ((df.p - df.x) ** 2).mean() ** .5
    return RMSE


# Genetate a HTML page of Full statistical report
def gen_stats(station):
    """gen stats
    """
    All = pd.read_csv(station + '_Timeseries_obs_model_raw_processed.csv',
                      index_col=0, parse_dates=True)
    profile = All.profile_report(title='Profile of Timeseries data for ' +
                                 'observations, NAM and ECMWF runs for ' +
                                 station[0])
    profile.to_file(output_file=station[0] + "Stats.html")


if Stage3 is True:
    gen_stats(station[0])
    print('HTMLfile generated')

# --------------------------------------------------------------------------- #
#                        Stage 4: Plotting                                    #
#                                                                             #
# --------------------------------------------------------------------------- #


if Stage4 is True:
    ecmwf_df['TS_station_point'].plot(style='X')
    nam_df['TS_station_point'].plot(style='.')
    TS_KNN['KNN'].plot(style='v')
    plt.title(station[0] + ' SO2 Concs (KNN interpolated observations,' +
              ' raw model data)')
    plt.ylabel('SO2 conc ug/m3')
    plt.legend(['ECMWF', 'NAM', 'Obs'])
    plt.xlabel('Date (hourly data)')
    plt.show()
    ecmwf_df['9ptmean'].plot(style='X')
    nam_df['9ptmean'].plot(style='.')
    TS_KNN['KNN'].plot(style='v')
    plt.title(station[0] + ' SO2 Concs (KNN interpolated observations,\n approx' +
              ' area model values')
    plt.ylabel('SO2 conc ug/m3')
    plt.legend(['ECMWF', 'NAM', 'Obs'])
    plt.xlabel('Date (hourly data)')
    print('Stage4')

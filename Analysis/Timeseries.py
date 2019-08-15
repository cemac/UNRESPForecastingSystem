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

import glob
import matplotlib as mpl
import pandas as pd
import warnings
import numpy as np
import matplotlib.pyplot as plt
from sklearn import neighbors
from datetime import datetime
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
# stations
station1 = 'ElPanama'
station2 = 'Pacaya'
# Models
ECMWF = '/scratch/Projects/UNRESP_Data/ECMWF'
NAM = '/scratch/Projects/UNRESP_Data/NAM'
# Unit Testing
Stage1 = True
Stage2 = False
Stage3 = False
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
    TS_KNN['KNN'].plot(style='.')
    TS_KNN[TS_KNN.columns[0]].plot(style='.')
    plt.legend()
    plt.title('SO2 data for March (' + station + ')')
    plt.ylabel(unit)
    plt.tight_layout()
    plt.savefig(station + '_March2017_Obs.png')


if Stage1 is True:
    TS_raw, unit, TS_KNN = ExtractTimeSeries(obs, station1, Em)
    plot_obs(TS_KNN, unit, station1)
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
if Stage3 is True:
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

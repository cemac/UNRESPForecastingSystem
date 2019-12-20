#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Timeseries Analysis
.. module:: Stats
    :platform: Unix
    :synopis:
.. moduleauther: CEMAC (UoL)
.. description: This module was developed by CEMAC as part of the UNRESP
   Project. This script takes CALPUFF concrec data from 2 models and compares
   with observations.
   From the output of timeseries.py normalise the data
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
from datetime import datetime
from sklearn import preprocessing
import pandas_profiling
warnings.filterwarnings("ignore")
# University System python may be broken
# If some one insists on using it...
BACKEND = mpl.get_backend()
if BACKEND == 'Qt4Agg' and sys.version_info[0] == 2:
    # Fix the backend
    print('swapping to Agg Backend')
    mpl.pyplot.switch_backend('Agg')

Towns = ['ElPanama', 'Pacaya']


def dateparse(x): return pd.datetime.strptime(x, '%d/%m/%Y %H:%M')
# -------------------------------------------------------------------------- #
#          Take data out put from timeseries.py and nomalise                 #
#                                                                            #
#                                                                            #
# ------------------------ 1. Normalise ------------------------------------ #


def gen_normalised_plots(town):
    """gen_normalised_plots
    ..description: generate normalised plots from data sets, rescale to maxium
                   = 1?
    ..args:
        town(str)
        dataset(str)
    """
    fname = 'Timeseries_obs_model_raw_processed.csv'
    try:
        ts = pd.read_csv('TimeSeries_Data/' + town + '/' + town + fname,
                         index_col=0, parse_dates=True)
    except FileNotFoundError:
        ts = pd.read_csv('TimeSeries_Data/' + town + '/' + town + '_' + fname,
                         index_col=0, parse_dates=True)
    Obs = ts[town + '_KNN']
    NAM = ts['NAM_area']
    ECMWF = ts['ECMWF_area']
    All = ts[[town + '_KNN', 'NAM_area', 'ECMWF_area']]
    # The end of March is rubbish for El Panama
    if town == 'ElPanama':
        # Get rid of that
        start_remove = pd.to_datetime('2017-3-25')
        end_remove = pd.to_datetime('2017-4-01')
        All = All.query('index < @start_remove or index > @end_remove')
    # Turn to numpy array
    x = All.values
    # scikit learn makes this quick
    min_max_scaler = preprocessing.MinMaxScaler()
    x_scaled = min_max_scaler.fit_transform(x)
    # Pop back into a dataframe
    df = pd.DataFrame(x_scaled, columns=['Observations', 'NAM', 'ECMWF'],
                      index=All.index)
    # Plot
    fig, ax = plt.subplots(figsize=(10, 5))
    df.plot(ax=ax)
    plt.title('Normalised Observation, NAM, ECMWF for ' + town +
              '\n (model data = area average)')
    plt.ylabel('Normalised (via min/max scaler) concentration)')
    plt.xlabel('Date in March 2017 (hourly data)')
    plt.savefig('Timeseries_Normalised.png')
    return df

# Notes ElPanama
# ECMWF Looked Crazy at the end of March and we had no data for end of March


plt.clf()
for town in Towns:
    df = gen_normalised_plots(town)
    profile = pandas_profiling.ProfileReport(df)
    profile.to_file(town + 'normalised_stats.html')

# ------------------------ 2. Dayly maxium (normailised)-------------------- #


# ------------------------ 3. Comosite days (normalised)-------------------- #

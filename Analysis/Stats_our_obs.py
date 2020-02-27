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
NormalisePlots = False
CompositePlots = False
ScatterPlots = False


# 0 get our data

def getdatamarch(StationName, var1='PM2.5', var2=None):
    pname = 'AQMeshData'+'/data/' + StationName + '/AQMeshData*_2017*.csv'
    for rw in glob.iglob(pname):
        day_data = pd.read_csv(rw, index_col=1, parse_dates=True)
        data = day_data[day_data.SensorLabel == var1]
        alldata = data[data.Status == 'Valid']
        if var2 is not None:
            data2 = day_data[day_data.SensorLabel == var2]
            alldata2 = data2[data2.Status == 'Valid']
    alldata = alldata.loc['2017-03-01':'2017-04-01']
    alldata2 = alldata2.loc['2017-03-01':'2017-04-01']
    return alldata, alldata2


def dateparse(x): return pd.datetime.strptime(x, '%d/%m/%Y %H:%M')
# -------------------------------------------------------------------------- #
#          Take data out put from timeseries.py and nomalise                 #
#                                                                            #
#                                                                            #
# ------------------------ 1. Normalise ------------------------------------ #


def gen_normalised_plots(town, plot=None):
    """gen_normalised_plots
    ..description: generate normalised plots from data sets, rescale to maxium
                   = 1?
    ..args:
        town(str)
        dataset(str)
    """
    PM25, SO2 = getdatamarch(town, var1='PM2.5', var2='SO2')
    # Turn to numpy array
    pm25so2 = pd.DataFrame()
    pm25so2['PM25'] = PM25['Scaled']
    pm25so2['SO2'] = SO2['Scaled']
    x = pm25so2.values
    # scikit learn makes this quick
    min_max_scaler = preprocessing.MinMaxScaler()
    x_scaled = min_max_scaler.fit_transform(x)
    # Pop back into a dataframe
    datanorm = pd.DataFrame(x_scaled, columns=['PM25','SO2'],
                      index=pm25so2.index)
    return pm25so2, datanorm


# Notes ElPanama
# ECMWF Looked Crazy at the end of March and we had no data for end of March
# so i removed that data?
pm25so2, datanorm = gen_normalised_plots('ElPanama')
datanorm.to_csv('NormalisedPM25SO_ElPanama_Mar2017.csv')
pm25so2.to_csv('PM25SO_ElPanama_Mar2017.csv')
fig, ax = plt.subplots(figsize=(10, 5))
pm25so2.plot(ax=ax)
pm25so2, datanorm = gen_normalised_plots('Pacaya')
datanorm.to_csv('NormalisedPM25SO_Pacaya_Mar2017.csv')
pm25so2.to_csv('PM25SO_Pacaya_Mar2017.csv')
fig, ax = plt.subplots(figsize=(10, 5))
pm25so2.plot(ax=ax)

if NormalisePlots is True:
    for town in Towns:
        df = gen_normalised_plots(town, plot='Y')[0]
        plt.clf()
        profile = pandas_profiling.ProfileReport(df)
        profile.to_file(town + 'normalised_stats.html')

# ------------------------ 2. Composite days (normalised)-------------------- #
if CompositePlots is True:
    for town in Towns:
        for i in np.arange(2):
            fig, ax = plt.subplots(figsize=(10, 5))
            df = gen_normalised_plots(town)[i]
            df = df.groupby(df.index.hour).mean()
            if i == 1:
                x = df.values
                # scikit learn makes this quick
                min_max_scaler = preprocessing.MinMaxScaler()
                x_scaled = min_max_scaler.fit_transform(x)
                # Pop back into a dataframe
                df = pd.DataFrame(x_scaled, columns=['Observations', 'NAM', 'ECMWF'],
                                  index=df.index)
            df.plot(ax=ax)
            plt.title('Composite mean hourly concentrations (Normalised)')
            plt.ylabel('Normalised concentration')
            plt.xlabel(' Hour in day')
            plt.savefig(town + 'Composite_day' + str(i) + '.png')
            plt.clf()

if ScatterPlots is True:
    for town in Towns:
        df = gen_normalised_plots(town)[0]
        # add an hour of day column
        df['hour'] = df.index.hour
        # Plot as scatter plot
        fig, ax = plt.subplots(figsize=(10, 5))
        plt.scatter(df.hour, df.Observations, marker='d', alpha=0.5)
        plt.scatter(df.hour, df.NAM, marker='*', alpha=0.6)
        plt.scatter(df.hour, df.ECMWF, marker='x', alpha=0.6)
        plt.title('Scatter plot of Normalised concentrations grouped by hour' +
                  ' of day \n' + town)
        plt.ylabel('Normalised concentrations')
        plt.xlabel('hour of day')
        plt.legend(['Obs', 'NAM', 'ECMWF'])
        plt.savefig(town+'scatterbyhour.png')
        plt.clf()
        # Plot as rose/ clock thing..
        times = [6, 3, 0,  21, 18, 15, 12, 9]
        fig = plt.figure(figsize=(12, 10))
        ax = fig.add_subplot(221, projection='polar')
        plt.scatter(np.pi / 2.0 - 2 * np.pi * df.hour / 24, df.Observations,
                    alpha=0.60, marker='d', label='observations')
        ax.set_xticklabels(times)
        ax.set_title('\n Observations')
        plt.legend(scatterpoints=5, loc="upper left", bbox_to_anchor=(1.04, 1))
        ax = fig.add_subplot(222, projection='polar')
        plt.scatter(np.pi / 2.0 - 2 * np.pi * df.hour / 24, df.NAM,
                    alpha=0.80, marker='x', color='orange', label='NAM')
        ax.set_xticklabels(times)
        ax.set_title('NAM')
        plt.legend(scatterpoints=5, loc="upper left", bbox_to_anchor=(1.04, 1))
        ax = fig.add_subplot(223, projection='polar')
        plt.scatter(np.pi / 2.0 - 2 * np.pi * df.hour / 24, df.ECMWF,
                    alpha=0.60, marker='+', color='g', label='ECMWF')
        ax.set_xticklabels(times)
        ax.set_title('ECMWF')
        plt.legend(scatterpoints=5, loc="upper left", bbox_to_anchor=(1.04, 1))
        ax = fig.add_subplot(224, projection='polar')
        plt.scatter(np.pi / 2.0 - 2 * np.pi * df.hour / 24, df.Observations,
                    alpha=0.60, marker='d', label='observations')
        plt.scatter(np.pi / 2.0 - 2 * np.pi * df.hour / 24, df.NAM,
                    alpha=0.80, marker='x', label='NAM')
        plt.scatter(np.pi / 2.0 - 2 * np.pi * df.hour / 24, df.ECMWF,
                    alpha=0.60, marker='+', label='ECMWF')
        ax.set_xticklabels(times)
        ax.set_title('All')
        plt.legend(scatterpoints=5, loc="upper left", bbox_to_anchor=(1.04, 1))
        plt.suptitle('Scatter plots in polar co-ordinates of normalised ' +
                     'concentrations \n for ' + town + '\n')
        plt.tight_layout()
        plt.savefig(town + 'normalised_clock_scatter.png')
        plt.clf()

# ------------------------ 3. Dayly maxium (normailised)-------------------- #

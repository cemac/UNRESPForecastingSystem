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
BinScatterPlots = True
# https://www2.dmu.dk/AtmosphericEnvironment/Expost/database/docs/PPM_conversion.pdf
# my data for SO2 is ppb
# 1 ppb = 2.62 μg/m3
# must divide by 2.6
# 0 get our data


def getdatamarch(StationName, var1='PM2.5', var2='SO2'):
    pname = 'AQMeshData'+'/data/' + StationName + '/AQMeshData*_2017*.csv'
    for rw in glob.iglob(pname):
        day_data = pd.read_csv(rw, index_col=0, parse_dates=True, )
        data = day_data[day_data.SensorLabel == var1]
        alldata = data
        #alldata = data[data.Status == 'Valid']
        if var2 is not None:
            data2 = day_data[day_data.SensorLabel == var2]
            alldata2 = data2
            #alldata2 = data2[data2.Status == 'Valid']
    alldata = alldata.loc['2017-03-01':'2017-04-01']
    alldata2 = alldata2.loc['2017-03-01':'2017-04-01']
    return alldata, alldata2


def dateparse(x): return pd.datetime.strptime(x, '%d/%m/%Y %H:%M')


def getdatamarch_origin():
    obs = '/scratch/Projects/UNRESP_Data/OBS'
    station = 'ElPanama'
    def dateparse(x): return pd.datetime.strptime(x, '%d/%m/%Y %H:%M')
    df = pd.read_csv(glob.glob(obs + '/' + station + '*.csv')[0], index_col=0,
                     parse_dates=True, date_parser=dateparse)
    pm25 = pd.DataFrame()
    so2 = pd.DataFrame()
    SO2 = df['1733_SO2_calibrated']
    PM25 = df['1733150_PM2.5_Scaled']
    SO2 = SO2.loc['2017-03-01':'2017-04-01']
    PM25 = PM25.loc['2017-03-01':'2017-04-01']
    so2['Scaled'] = SO2
    pm25['Scaled'] = PM25
    return so2, pm25
# -------------------------------------------------------------------------- #
#          Take data out put from timeseries.py and nomalise                 #
#                                                                            #
#                                                                            #
# ------------------------ 1. Normalise ------------------------------------ #


def gen_normalised_plots1(town, plot=None):
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
    pm25so2['SO2'] = SO2['Scaled']*2.62
    x = pm25so2.values
    # scikit learn makes this quick
    min_max_scaler = preprocessing.MinMaxScaler()
    x_scaled = min_max_scaler.fit_transform(x)
    # Pop back into a dataframe
    datanorm = pd.DataFrame(x_scaled, columns=['PM25', 'SO2'],
                            index=pm25so2.index)
    return pm25so2, datanorm


def gen_normalised_plots():
    """gen_normalised_plots
    ..description: generate normalised plots from data sets, rescale to maxium
                   = 1?
    ..args:
        town(str)
        dataset(str)
    """
    PM25, SO2 = getdatamarch_origin()
    # Turn to numpy array
    pm25so2 = pd.DataFrame()
    pm25so2['PM25'] = PM25['Scaled']
    pm25so2['SO2'] = SO2['Scaled']
    x = pm25so2.values
    # scikit learn makes this quick
    min_max_scaler = preprocessing.MinMaxScaler()
    x_scaled = min_max_scaler.fit_transform(x)
    # Pop back into a dataframe
    datanorm = pd.DataFrame(x_scaled, columns=['PM25', 'SO2'],
                            index=pm25so2.index)
    return pm25so2, datanorm


# Notes ElPanama
# ECMWF Looked Crazy at the end of March and we had no data for end of March
# so i removed that data?
"""
pm25so2, datanorm = gen_normalised_plots('ElPanama')
datanorm.to_csv('NormalisedPM25SO_ElPanama_Mar2017.csv')
pm25so2.to_csv('PM25SO_ElPanama_Mar2017.csv')
test = datanorm.reset_index()
fig, ax = plt.subplots(figsize=(10, 5))
test.plot.scatter(x='TETimestamp',y='SO2',style='.',ax=ax, s=1)
test.plot.scatter(x='TETimestamp',y='PM25',style='.',ax=ax, s=1, color='r')
plt.legend(['PM25','SO2'])
plt.show()
fig, ax = plt.subplots(figsize=(10, 5))
test[['SO2','PM25']].plot(ax=ax)
plt.show()
test = pm25so2.reset_index()
fig, ax = plt.subplots(figsize=(10, 5))
test.plot.scatter(x='TETimestamp',y='SO2',style='.',ax=ax, s=1)
test.plot.scatter(x='TETimestamp',y='PM25',style='.',ax=ax, s=1, color='r')
plt.legend(['PM25','SO2'])
plt.show()
fig, ax = plt.subplots(figsize=(10, 5))
test[['SO2','PM25']].plot(ax=ax)
plt.show()
pm25so2, datanorm = gNormalisedPen_normalised_plots('Pacaya')
datanorm.to_csv('NormalisedPM25SO_Pacaya_Mar2017.csv')
pm25so2.to_csv('PM25SO_Pacaya_Mar2017.csv')
fig, ax = plt.subplots(figsize=(10, 5))
pm25so2.plot(ax=ax)
"""


if NormalisePlots is True:
    for town in Towns:
        df = gen_normalised_plots(town, plot='Y')[0]
        plt.clf()
        # profile = pandas_profiling.ProfileReport(df)
        # profile.to_file(town + 'normalised_stats.html')

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
                df = pd.DataFrame(x_scaled, columns=['Observations', 'NAM',
                                  'ECMWF'], index=df.index)
            df.plot(ax=ax)
            plt.title('Composite mean hourly concentrations (Normalised)')
            plt.ylabel('Normalised concentration')
            plt.xlabel(' Hour in day')
            plt.savefig(town + 'Composite_day' + str(i) + '.png')
            plt.clf()

if ScatterPlots is True:
    for town in Towns:
        pm25so2, df = gen_normalised_plots(town)
        # add an hour of day column
        th = 0.2
        df['hour'] = df.index.hour
        df['minute'] = df.index.minute
        df['minod'] = df.hour*60+df.minute
        # Plot as scatter plot
        fig, ax = plt.subplots(figsize=(10, 5))
        plt.scatter(df.minod[df.PM25.values>=th], df.PM25[df.PM25.values>=th], marker='*', alpha=0.4)
        plt.scatter(df.minod[df.SO2.values>=th], df.SO2[df.SO2.values>=th], marker='d', alpha=0.2)
        plt.title('Scatter plot of Normalised concentrations grouped by min' +
                  ' of day \n' + town)
        plt.ylabel('Normalised concentrations')
        plt.xlabel('min of day')
        plt.legend(['PM25', 'SO2'])
        plt.savefig(town+'scatterbymin_newgrpbymin_threshold_'+str(th)+'.png')
        plt.clf()
        # Plot as rose/ clock thing..
        times = [6, 3, 0,  21, 18, 15, 12, 9]
        fig = plt.figure(figsize=(12, 10))
        ax = fig.add_subplot(221, projection='polar')
        plt.scatter(np.pi / 2.0 - 2 * np.pi * df.minod / (24*60), df.PM25,
                    alpha=0.40, marker='d', label='PM25')
        ax.set_xticklabels(times)
        ax.set_title('\n PM25')
        plt.legend(scatterpoints=5, loc="upper left", bbox_to_anchor=(1.04, 1))
        ax = fig.add_subplot(222, projection='polar')
        plt.scatter(np.pi / 2.0 - 2 * np.pi * df.minod[df.SO2.values>=th] / (24*60), df.SO2[df.SO2.values>=th],
                    alpha=0.50, marker='x', color='orange', label='SO2')
        times = [6, 3, 0,  21, 18, 15, 12, 9]
        ax.set_xticklabels(times)
        ax.set_title('SO2')
        plt.legend(scatterpoints=5, loc="upper left", bbox_to_anchor=(1.04, 1))
        ax = fig.add_subplot(224, projection='polar')
        plt.scatter(np.pi / 2.0 - 2 * np.pi * df.minod / (24*60), df.PM25,
                    alpha=0.40, marker='d', label='PM25')
        plt.scatter(np.pi / 2.0 - 2 * np.pi * df.minod[df.SO2.values>=th] / (24*60), df.SO2[df.SO2.values>=th],
                    alpha=0.50, marker='x', label='SO2')
        ax.set_xticklabels(times)
        ax.set_title('both')
        plt.legend(scatterpoints=5, loc="upper left", bbox_to_anchor=(1.04, 1))
        plt.suptitle('Scatter plots in polar co-ordinates of normalised ' +
                     'concentrations \n for ' + town + '\n')
        plt.tight_layout()
        plt.savefig(town + 'normalised_clock_scatter_newdata_grpbymin_threshold_'+str(th)+'.png')
        plt.clf()

if ScatterPlots is True:
    for town in Towns:
        #pm25so2, df = gen_normalised_plots(town)
        # add an hour of day column
        th = 0.1
        df['hour'] = df.index.hour
        df['minute'] = df.index.minute
        df['minod'] = df.hour
        # Plot as scatter plot
        fig, ax = plt.subplots(figsize=(10, 5))
        plt.scatter(df.minod[df.PM25.values>=th], df.PM25[df.PM25.values>=th], marker='*', alpha=0.4)
        plt.scatter(df.minod[df.SO2.values>=th], df.SO2[df.SO2.values>=th], marker='d', alpha=0.2)
        plt.title('Scatter plot of Normalised concentrations grouped by hour' +
                  ' of day \n' + town)
        plt.ylabel('Normalised concentrations')
        plt.xlabel('min of day')
        plt.legend(['PM25', 'SO2'])
        plt.savefig(town+'scatterbyhour_new_grphour_threshold_'+str(th)+'.png')
        plt.clf()
        # Plot as rose/ clock thing..
        times = [6, 3, 0,  21, 18, 15, 12, 9]
        fig = plt.figure(figsize=(12, 10))
        ax = fig.add_subplot(221, projection='polar')
        plt.scatter(np.pi / 2.0 - 2 * np.pi * df.minod / (24), df.PM25,
                    alpha=0.40, marker='d', label='PM25')
        ax.set_xticklabels(times)
        ax.set_title('\n PM25')
        plt.legend(scatterpoints=5, loc="upper left", bbox_to_anchor=(1.04, 1))
        ax = fig.add_subplot(222, projection='polar')
        plt.scatter(np.pi / 2.0 - 2 * np.pi * df.minod[df.SO2.values>=th] / (24), df.SO2[df.SO2.values>=th],
                    alpha=0.50, marker='x', color='orange', label='SO2')
        times = [6, 3, 0,  21, 18, 15, 12, 9]
        ax.set_xticklabels(times)
        ax.set_title('SO2')
        plt.legend(scatterpoints=5, loc="upper left", bbox_to_anchor=(1.04, 1))
        ax = fig.add_subplot(224, projection='polar')
        plt.scatter(np.pi / 2.0 - 2 * np.pi * df.minod / (24), df.PM25,
                    alpha=0.40, marker='d', label='PM25')
        plt.scatter(np.pi / 2.0 - 2 * np.pi * df.minod[df.SO2.values>=th] / (24), df.SO2[df.SO2.values>=th],
                    alpha=0.50, marker='x', label='SO2')
        ax.set_xticklabels(times)
        ax.set_title('both')
        plt.legend(scatterpoints=5, loc="upper left", bbox_to_anchor=(1.04, 1))
        plt.suptitle('Scatter plots in polar co-ordinates of normalised ' +
                     'concentrations \n for ' + town + '\n')
        plt.tight_layout()
        plt.savefig(town + 'normalised_clock_scatter_newdata_grphour_threshold_' + str(th)+'.png')
        plt.clf()

if BinScatterPlots is True:
    # pm25so2, datanorm = gen_normalised_plots('ElPanama')
    # pm25so2.plot.scatter(x='TETimestamp', y='SO2', style='.', ax=ax, s=1)
    # pm25so2.plot.scatter(x='TETimestamp', y='PM25', style='.', ax=ax, s=1, color='r')
    binLims = [0*2.62, 10*2.62, 20*2.62, 40*2.62, 2000*2.62]
    binLims = [0, 10, 20, 40, 2000]
    bin_lables = ["vlow", "low", "mod", "high"]
    for town in Towns:
        df, norm = gen_normalised_plots1(town)
        SO2bins = pd.cut(df.SO2, binLims, labels=bin_lables)
        PM25bins = pd.cut(df.PM25, binLims, labels=bin_lables)
        df.PM25[PM25bins == 'vlow'] = 0
        df.PM25[PM25bins == 'low'] = binLims[1]
        df.PM25[PM25bins == 'mod'] = binLims[2]
        df.PM25[PM25bins == 'high'] = binLims[3]
        df.SO2[SO2bins == 'vlow'] = binLims[0]
        df.SO2[SO2bins == 'low'] = binLims[1]
        df.SO2[SO2bins == 'mod'] = binLims[2]
        df.SO2[SO2bins == 'high'] = binLims[3]
        fig, ax = plt.subplots(figsize=(10, 5))
        print(df.columns)
        df[['PM25', 'SO2']].plot()
        plt.show()
        # add an hour of day column
        th = 1
        df['hour'] = df.index.hour
        df['minute'] = df.index.minute
        df['minod'] = df.hour
        # Plot as scatter plot
        fig, ax = plt.subplots(figsize=(10, 5))
        plt.scatter(df.minod[df.PM25.values >= th],
                    df.PM25[df.PM25.values >= th], marker='*', alpha=0.4)
        plt.scatter(df.minod[df.SO2.values >= th],
                    df.SO2[df.SO2.values >= th], marker='d', alpha=0.2)
        plt.title('Scatter plot of Normalised concentrations grouped by hour' +
                  ' of day \n' + town)
        plt.ylabel('Normalised concentrations')
        plt.xlabel('min of day')
        plt.legend(['PM25', 'SO2'])
        plt.savefig('Binned' + town+'scatterbyhour_new_grphour_orig.png')
        plt.clf()
        # Plot as rose/ clock thing..
        times = [6, 3, 0,  21, 18, 15, 12, 9]
        fig = plt.figure(figsize=(12, 10))
        ax = fig.add_subplot(221, projection='polar')
        plt.scatter(np.pi / 2.0 - 2 * np.pi * df.minod / (24), df.PM25,
                    alpha=0.40, marker='d', label='PM25')
        ax.set_xticklabels(times)
        ax.set_title('\n PM25')
        plt.legend(scatterpoints=5, loc="upper left", bbox_to_anchor=(1.04, 1))
        ax = fig.add_subplot(222, projection='polar')
        plt.scatter(np.pi / 2.0 - 2 * np.pi * df.minod[df.SO2.values>=th] / (24), df.SO2[df.SO2.values>=th],
                    alpha=0.50, marker='x', color='orange', label='SO2')
        times = [6, 3, 0,  21, 18, 15, 12, 9]
        ax.set_xticklabels(times)
        ax.set_title('SO2')
        plt.legend(scatterpoints=5, loc="upper left", bbox_to_anchor=(1.04, 1))
        ax = fig.add_subplot(224, projection='polar')
        plt.scatter(np.pi / 2.0 - 2 * np.pi * df.minod / (24), df.PM25,
                    alpha=0.40, marker='d', label='PM25')
        plt.scatter(np.pi / 2.0 - 2 * np.pi * df.minod[df.SO2.values>=th] / (24), df.SO2[df.SO2.values>=th],
                    alpha=0.50, marker='x', label='SO2')
        ax.set_xticklabels(times)
        ax.set_title('both')
        plt.legend(scatterpoints=5, loc="upper left", bbox_to_anchor=(1.04, 1))
        plt.suptitle('Scatter plots in polar co-ordinates of normalised ' +
                     'concentrations \n for ' + town + '\n')
        plt.tight_layout()
        plt.savefig('Binned' + town + 'normalised_clock_scatter_newdata_grphour_orig.png')
        plt.clf()

# UNRESP Forecasting System

[![GitHub release](https://img.shields.io/github/release/cemac/UNRESPForecastingSystem.svg)](https://github.com/cemac/UNRESPForecastingSystem/releases) [![GitHub top language](https://img.shields.io/github/languages/top/cemac/UNRESPForecastingSystem.svg)](https://github.com/cemac/UNRESPForecastingSystem) [![GitHub issues](https://img.shields.io/github/issues/cemac/UNRESPForecastingSystem.svg)](https://github.com/cemac/UNRESPForecastingSystem/issues) [![GitHub last commit](https://img.shields.io/github/last-commit/cemac/UNRESPForecastingSystem.svg)](https://github.com/cemac/UNRESPForecastingSystem/commits/master) [![GitHub All Releases](https://img.shields.io/github/downloads/cemac/UNRESPForecastingSystem/total.svg)](https://github.com/cemac/UNRESPForecastingSystem/releases) ![GitHub](https://img.shields.io/github/license/cemac/UNRESPForecastingSystem.svg) [![DOI](https://zenodo.org/badge/131827149.svg)](https://zenodo.org/badge/latestdoi/131827149)
[![HitCount](http://hits.dwyl.io/{cemac}/{UNRESPForecastingSystem}.svg)](http://hits.dwyl.io/{cemac}/{UNRESPForecastingSystem})


<hr>

Repository for the [UNRESP](https://vumo.cloud/) Forecasting System:

An automated forecasting system has been created that uses the [CALPUFF](http://www.src.com/) dispersion model to predict S0<sub>2</sub> and S0<sub>4</sub> concentrations around the Masaya volcano. This is based on the current forecasting system implemented by IMO, but with modifications and improvements.

This work is displayed at: [homepages.see.leeds.ac.uk/~earunres](https://homepages.see.leeds.ac.uk/~earunres)


The repository hosts the scripts required to run the CALPUFF dispersion model to predict SO<sub>2</sub> concentrations around the Masaya volcano forecasting for 48 hours using NAM data. The hourly output is plotted in individual png and googlemaps files (visualisation can be turned on or off).

## [DOCUMENTATION](https://github.com/cemac/UNRESPForecastingSystem/wiki) ##

**Full** Documentation can be found on this Repository's [wiki](https://github.com/cemac/UNRESPForecastingSystem/wiki)

Summary documention
- [Requirements](#Requirements)
- [Installation](#Installation)
- [Usage-Quick-Start](#Usage-Quick-Start)
- [Visualization](#Visualization)
- [Contributions](#Contributions)
- [Licence](#Licence)
- [Acknowledgements](#Acknowledgements)

- [Full-User-Guide](https://github.com/cemac/UNRESPForecastingSystem/wiki/User-Guide)
- [Developer-Guide](https://github.com/cemac/UNRESPForecastingSystem/wiki/Developer-Guide)

## Requirements ##

* UNIX operating system (tested: CentOS Ubuntu)
* [anaconda python](https://www.anaconda.com/distribution/#download-section)(recommended code works in python 2 and 3) (conda => 4.6.14 recommended)
  * requirements in environment.yml (python 3)
  * non environment set up can be followed using requirements.txt if desired
* Intel compiler **OR** executables and library (only for similar architecture as built)

Non anaconda installations require a separate build of ecCodes python API:
* [ecCodes python API](https://confluence.ecmwf.int//display/ECC/Releases)

## Installation ##

Anaconda python (conda 4.6.14), unix systems (recommended), intel compilers or compiled executables

```
git clone https://github.com/cemac/UNRESPForecastingSystem.git
cd UNRESPForecastingSystem
./installcalpuff.sh
conda env create -f environment.yml
```

## Usage Quick-Start ##

For external users, once installed to run full forecast and visualisation with default options:

```bash
cd $HOME/UNRESPForecastingSystem
./Run_ext.sh
```

**NB** If no intel compilers the executables and libraries must be copied over to CALPUFF_EXE

For help run `.\Run_ext.sh -h`

```
 Run_ext.sh

 A CEMAC script to Run CALPUFF WITH NAM DATA input
 winds and produces plots of SO2 and SO4.

 Usage:
  .\Run_ext.sh <opts>

 No options runs a default production configuration:
 Today, Viz on, plots production area (~earunres).

 Options:
  -d <date> YYYYMMDD DEFAULT: <today's date>
  -n <home> name of viz defaults to ~earunres
 **
 The following switches can be used to overwrite
 Default behaviour.
 **
  -s turn OFF SO4 plotting
  -m turn OFF Forecasting model (e.g to run viz only)
  -p turn OFF viz steps (no jpgs etc to be produced)
  -f turn ON ffmpeg mp4 production
 ** TROUBLESHOOTING
 * Missing .so file --> most like intel library
   Try loading system intel e.g. module load intel or set LD_LIBRARY_PATH
 * Missing python modules --> mostly likely conda environment failure
   try `source activate unresp`
   or `conda activate unresp`
   or `load your system python libraries`
 ^^^ these fixes can be added to .env file for bespoke Setup
```

Run.sh is set up default to leeds production behaviour to run as a chronjob displaying at [~earunres](https://homepages.see.leeds.ac.uk/~earunres/UNRESP_VIZ/index.html)

## Visualization

The output can be viewed by running:

```bash
cd $HOME/UNRESPForecastingSystem/VIZ_SITE_CODE/public_html
python -m http.server
```
And opening http://0.0.0.0:8000/ in any browser

All the code can be transported to desired location e.g. Apache server and the
forecasting scripts ran with a `-n` option to move to that location.

<hr>

## Contributions ##

*coming soon: issues/suggestions - documentation improvements - code improvements/developments welcome*


## Licence ##

*This code is Open Source EXCEPT the CALPUFF code you will download*. The code changes to set up the model to run
CALPUFF for Masaya Region, forecast pipeline tools (preprocessing, postprocessing and visualisation), Python tools and static site image viewer (VIZ_SITE_CODE) are all covered under the MIT Licence.

<hr>

## Acknowledgements ##

This repository has been developed in Collaboration with Sara Barsotti (Icelandic meteorological office),
 Evgenia Ilyinskaya (University of Leeds) as well as the code authors in the commit history. The Air quality sensor data stored in the visualization folders was produced in Collaboration with INITER.

Resources:

* Calpuff model code provided by Exponent

<hr>

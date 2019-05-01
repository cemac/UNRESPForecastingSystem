<div align="center">
<a href="https://www.cemac.leeds.ac.uk/">
  <img src="https://github.com/cemac/cemac_generic/blob/master/Images/cemac.png"></a>
  <br>
</div>

# UNRESP Forecasting System

[![GitHub release](https://img.shields.io/github/release/cemac/UNRESPForecastingSystem.svg)](https://github.com/cemac/UNRESPForecastingSystem/releases) [![GitHub top language](https://img.shields.io/github/languages/top/cemac/UNRESPForecastingSystem.svg)](https://github.com/cemac/UNRESPForecastingSystem) [![GitHub issues](https://img.shields.io/github/issues/cemac/UNRESPForecastingSystem.svg)](https://github.com/cemac/UNRESPForecastingSystem/issues) [![GitHub last commit](https://img.shields.io/github/last-commit/cemac/UNRESPForecastingSystem.svg)](https://github.com/cemac/UNRESPForecastingSystem/commits/master) [![GitHub All Releases](https://img.shields.io/github/downloads/cemac/UNRESPForecastingSystem/total.svg)](https://github.com/cemac/UNRESPForecastingSystem/releases)

<hr>

Repository for the [UNRESP](https://vumo.cloud/) Forecasting System:

An automated forecasting system has been created that uses the [CALPUFF](http://www.src.com/) dispersion model to predict S0<sub>2</sub> and S0<sub>4</sub> concentrations around the Masaya volcano. This is based on the current forecasting system implemented by IMO, but with modifications and improvements.

This work is displayed at: [homepages.see.leeds.ac.uk/~earunres](https://homepages.see.leeds.ac.uk/~earunres)


The repository hosts the scripts required to run the CALPUFF dispersion model to predict SO<sub>2</sub> concentrations around the Masaya volcano forecasting for 48 hours using NAM data. The hourly output is plotted in individual png files and collated into a mp4 movie.

## DOCUMENTATION ##

**Full** Documentation can be found on this Repository's [wiki](https://github.com/cemac/UNRESPForecastingSystem/wiki)

Summary documention
- [Requirements](Requirements)
- [Installation](Installation)
- [Usage-Quick-Start](Usage-Quick-Start)
- [Visualization](Visualization)
- [Usage-Further](Usage-Further)
- [Contributions](Contributions)
- [Licence](Licence)
- [Acknowledgements](Acknowledgements)

## Requirements ##

* UNIX operating system (tested: CentOS Ubuntu)
* [anaconda python](https://www.anaconda.com/distribution/#download-section)(recommended code works in python 2 and 3)
  * requirements in environment.yml (python 3)
  * non environment set up can be followed using requirements.txt if desired
* Intel compiler **OR** executables and library (only for similar architecture as built)

Non anaconda installations require a separate build of ecCodes python API:
* [ecCodes python API](https://confluence.ecmwf.int//display/ECC/Releases)

## Installation ##

Anaconda python, unix systems (recommended)

```
git clone https://github.com/cemac/UNRESPForecastingSystem.git
cd UNRESPForecastingSystem
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

## Usage Further

* In Run_ext.sh various parameters can be set:
  1. `res` to alter the resolution between 100 - 1000 m
  2. by default the model run and visualisation can be turned on or off to be
    run together or separately however `Run_ext.sh` can be edited to turn off
    separate parts
  3. `runTERREL=true` - The part of the CALPUFF
  4. `runCTGPROC=true` - The part of the CALPUFF system that grids the land-use data
  5. `runMAKEGEO=true` - The part of the CALPUFF system that combines the gridded terrain and land-use data into a file appropriate for input to CALMET
  6. `run3DDAT=true` - Downloads the required met (NAM) data and runs a python script (Create3DDAT.py) to extract the required data into a file appropriate for input to CALMET.
  7. `runCALMET=true` - The 3-D diagnostic meteorological model part of the CALPUFF system
  8. `runCALPUFF=true` - The main dispersion model part of the CALPUFF system
* To forecast for the current day default visualization home to the GitHub repo
  Viz site code if stored in home dir (override with `-n` flag):
  ```bash
  ./Run_ext.sh
  ```
* To forecast for a specific day:
  ```bash
  ./Run_ext.sh -d YYYYMMDD
  ```
  Note, however, that the external met (NAM) data that the script will try to download is only accessible for around 10 days after the original date before it is removed from the ftp site.
* Chronjobs: 2 chronjobs are required
  1 . Everyday at 10.30am run the forecast
    ```bash
    30 10 * * * cd <path-to-repo> && ./Run.sh
    ```
  2. Everyday at 10.30am etches the IMO CALPUFF output and plots it onto a map for viewing on the web [here](http://homepages.see.leeds.ac.uk/~earunres/masayaSO2.html).
    ```bash
    30 10 * * * <path-to-repo/extrascripts> makeMasayaFig.gmt
    ```
  3. transfers the output data from the forecast runs to the shared UNRESP space on the N-drive at 10:45am
  ```sh
  45 10 * * * <path-to-repo> updateNDrive.sh
  ```

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

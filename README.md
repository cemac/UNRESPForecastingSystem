<div align="center">
<a href="https://www.cemac.leeds.ac.uk/">
  <img src="https://github.com/cemac/cemac_generic/blob/master/Images/cemac.png"></a>
  <br>
</div>

# UNRESP Forecasting System

Repository for the [UNRESP](https://vumo.cloud/) Forecasting System:

An automated forecasting system has been created that uses the [CALPUFF](http://www.src.com/) dispersion model to predict S02 and S04 concentrations around the Masaya volcano. This is based on the current forecasting system implemented by IMO, but with modifications and improvements.

This work is displayed [here]([~earunres](https://homepages.see.leeds.ac.uk/~earunres))

## Description ##

The repository hosts the scripts required to run the CALPUFF dispersion model to predict SO2 concentrations around the Masaya volcano forecasting for 48 hours using NAM data. The hourly output is plotted in individual png files and collated into a mp4 movie.

## Requirements ##

This has been designed to run on CentOS systems. **[currently](https://confluence.ecmwf.int/display/ECC/ecCodes+installation) ecCodes does not work with
python 3**

* [ecCodes python API](https://confluence.ecmwf.int//display/ECC/Releases)
* [Python 2](https://www.anaconda.com/download)

## Usage ##

Aimed at running on the Leeds Linux systems

```bash
./Run.sh
optional arguments
-d Date YYYYMMDD, defaults to  
-n specify home directory, defaults to ~earunres
-h help prints Usage statement and exits
```
This set up defaults to production behaviour to run as a chronjob displaying at [~earunres](https://homepages.see.leeds.ac.uk/~earunres/UNRESP_VIZ/index.html)

* In Run.sh various parameters can be set:
  1. `res` to alter the resolution between 100 - 1000 m
  2. `runVis=True` Enable visualization  creating static and movie visualisations of the CALPUFF model output via a python script (generateMaps.py) and the Linux tool 'ffmpeg', respectively.
  3. `runTERREL=true` - The part of the CALPUFF
  4. `runCTGPROC=true` - The part of the CALPUFF system that grids the land-use data
  5. `runMAKEGEO=true` - The part of the CALPUFF system that combines the gridded terrain and land-use data into a file appropriate for input to CALMET
  6. `run3DDAT=true` - Downloads the required met (NAM) data and runs a python script (Create3DDAT.py) to extract the required data into a file appropriate for input to CALMET.
  7. `runCALMET=true` - The 3-D diagnostic meteorological model part of the CALPUFF system
  8. `runCALPUFF=true` - The main dispersion model part of the CALPUFF system
* To forecast for the current day default visualization home to ~earunres (production):
  ```bash
  ./Run.sh
  ```
* To forecast for a specific day:
  ```bash
  ./Run.sh -d YYYYMMDD
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

## Overview of Repository ##

The directory structure of the repository is as follows:
- The source code for the various parts of the CALPUFF system is stored in subdirectory `CALPUFF_SRC`. The source data corresponds to Version 7, which is downloadable from [here](http://www.src.com/calpuff/download/mod7_codes.htm), with a few minimal code changes required to allow the model to be built on a Linux system with Intel compilers; these changes are described in [this](https://github.com/cemac-tech/UNRESP/blob/master/Docs/CEMACUserGuide_UNRESP.tex) version-controlled file.
- All user-editable input files for the various parts of the CALPUFF system are stored in subdirectory `CALPUFF_INP`. Template versions of these files have been set up specifically for the Masaya case and are version controlled. Various run-specific fields (e.g. run date) are then filled in at run-time. Information about setting up these input files for the Masaya case is also within [this](https://github.com/cemac-tech/UNRESP/blob/master/Docs/CEMACUserGuide_UNRESP.tex) file.
- All other input data files are stored in the `data` subdirectory. These include the 90m resolution DEM data files from the SRTM 3-sec dataset covering the Masaya region (4 .bil files), the old 1km resolution DEM data file from the GTOPO30 dataset (w100n40.dem) which isn't used any more but is retained for reference, the USGS land-use data file covering North/Central America (nalulcl20.bil), and a two-column file of UTM zone 16P coordinates which specify discrete receptor points at which we want output data (xy_masaya.dat). Some of the output files from parts of the forecasting system that serve as input files to subsequent parts are also copied into this directory during run-time.
- All executables for the various parts of the CALPUFF system are stored in subdirectory `CALPUFF_EXE`. These are not version-controlled but will be built automatically with the run script is first used. They are only then rebuilt if deleted, so if you make any changes to the source code, be sure to delete the old executables so that new ones are made.
- All output from the various parts of the CALPUFF system are moved to within subdirectory `CALPUFF_OUT` during runtime.
- All downloaded and processed NAM data during run-time are stored within subdirectory `NAM_data`.
- All visualisation output data files are stored within subdirectory `vis`.
- All python scripts are stored within subdirectory `Python`.

<hr>

## Licence information ##

*Coming soon*

<hr>

## Version information


<!--- release table --->
|  Version            | Release          |
|---------------------|------------------|
| **UoL original**  | [![GitHubrelease](https://img.shields.io/badge/release-v.1.0-blue.svg)](https://github.com/cemac/DECAF_workflow/releases/tag/v1.0)|
| **Viz update**    |  [![GitHubrelease](https://img.shields.io/badge/release-v.2.0-blue.svg)](https://github.com/cemac/UNRESPForcastingSystem/releases/tag/v2.0) |
| **SO4** |  *coming soon*     |
<!--- table --->

## Acknowledgements ##

*Comming soon*
*IMO, UoL, Exponent*

<hr>

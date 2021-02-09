#!/usr/bin/bash -

# This script was created by CEMAC (University of Leeds) as
# part of the UNRESP Project
# Setup environment (should not need to be edited)
set -e # stop at first error
# load modules (Leeds)
if [ $USER == earmgr ]; then
  module load intel/17.0.0
  module load python2 python-libs
  # For Mark only:
  export PYTHONPATH="/nfs/see-fs-02_users/earmgr/SW/eccodes-2.6.0/lib/python2.7/site-packages:${PYTHONPATH}"
  vizhome=~earunres
else
  if [ $CONDA_DEFAULT_ENV != unresp ]; then
    echo "trying to activate unresp python environment..."
    eval "$(conda shell.bash hook)"
    conda activate
    conda activate unresp
  fi
  # Put any bespoke setup steps in .env
  source .env
  vizhome=$HOME/UNRESPForecastingSystem/VIZ_SITE_CODE
fi
# Resolution (m) of intended CALPUFF grid.  100 < (integer) < 1000
res=500
# Defaults that can be overwritten by editing HERE:
# Command line option m switches all to false
runTERREL=true
runCTGPROC=true
runMAKEGEO=true
run3DDAT=true
runCALMET=true
runCALPUFF=true
runmodel=true

set -e # stop at first error
# Set other parameters (unlikely to need editing)
let NX=90000/$res+1
let NY=54000/$res+1
DGRIDKM=$(echo "scale=3; $res/1000" | bc)
let MESHGLAZ=1000/$res+1
cwd=$(pwd)
# Number of nam files for 48 hours
NAMno=17

#------------------------------------------------------------------------#
#------------------- DO NOT ALTER BELOW THIS LINE------------------------#
#------------------------------------------------------------------------#

#                       COMMAND LINE FLAG HANDELING                      #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Defaults that can be overwritten via command line
rundate=$(date +%Y%m%d)
timeforfile=$(date +%d%m%Y)
numhours=24
runVIS=false
runallVIS=false
rungoogle=false
runsatellite=false
runtopo=false
runSO4=false
runSO2=false
runSO24=false
runffmpeg=false

print_usage() {
  echo "
 Run.sh

 A CEMAC script to Run CALPUFF WITH NAM DATA input
 winds and produces plots of SO2 and SO4.

 Usage:
  .\Run.sh <opts>

 No options runs a default production configuration:
 Today, Viz off, 48 hours.

 Options:
  -d <date> YYYYMMDD DEFAULT: <today's date>
  -v <home> name of viz defaults to UNRESPForecastingSystem/VIZ_SITE_CODE
  -n <numhours> forescast hours defaults to 48
  -x <res> resolution in m (100 < x < 1000)
 **
 The following switches can be used to overwrite
 Default behaviour.

 DEFAULT: output todays SO2 concrec files on topography
          background
 **
  -m turn OFF Forecasting model
  -p turn ON viz steps: default to SO2 on topography only
  -a turn ON all viz options except ffmpeg
  -b plot BOTH SO2 and SO4
  -t output BOTH satellite and topo backgrounds
  -g turn ON GOOGLE PLOTS
  -r SWITCH to satellite background
  -s SWITCH to SO4
  -y plot ONLY GOOGLE PLOTS
  -f turn ON ffmpeg mp4 production
  -h HELP: prints this message!

 long options are currently not avaible.

 ------------------------------------------------

 Other Code Possible Options:

 The model is split into various components, these can
 be induvidually turned on or off for development purposes
 via editing the upper part of this script.

  runTERREL=true
  runCTGPROC=true
  runMAKEGEO=true
  run3DDAT=true
  runCALMET=true
  runCALPUFF=true
  runmodel=true

** TROUBLESHOOTING
 * Missing .so file --> most like intel library
   Try loading system intel e.g. module load intel or set LD_LIBRARY_PATH
 * Missing python modules --> mostly likely conda environment failure
   try `source activate unresp`
   or `conda activate unresp`
   or `load your system python libraries`
 ^^^ these fixes can be added to .env file for bespoke Setup

  "
}

set_viz() {
  # description flags
  runVIS=true
  runSO2=true
  runtopo=true
  # code option
  SOopt=" --SO2 "
  vizopt=" --topo "
}

set_allviz() {
  # description flags
  runallVIS=true
  runVIS=true
  runtopo=true
  runSO24=true
  rungoogle=ture
  runsatellite=false
  # code option
  vizopt=" --all "
  SOopt=" --SO2 --SO4 "
}

set_SO4() {
  # description flags
  runSO4=true
  runSO2=false
  # code option
  SOopt=" --SO4 "
}

set_SO24() {
  # description flags
  runSO24=true
  runSO2=false
  runSO4=false
  # code option
  SOopt=" --SO2 --SO4 "
}

add_google() {
  googleopt=" --google "
}
only_google() {
  # description flags
  rungoogle=ture
  runsatellite=false
  runtopo=false
  # code option
  vizopt=" --google "
}

set_satellite() {
  # description flags
  runsatellite=true
  runtopo=false
  # code option
  vizopt=" --satellite "
}

set_sattopo() {
  # description flags
  runsatellite=true
  runtopo=true
  # code option
  vizopt=" --topo --satellite"
}

set_ffmpeg() {
  runffmpeg=true
}

set_model() {
  runTERREL=false
  runCTGPROC=false
  runMAKEGEO=false
  run3DDAT=false
  runCALMET=false
  runCALPUFF=false
  runmodel=false
}
while getopts 'd:n:v:x:pamsbgrtyfh' flag; do
  case "${flag}" in
    d) rundate="${OPTARG}" ;;
    n) numhours="${OPTARG}" ;;
    v) vizhome="${OPTARG}" ;;
    x) res="${OPTARG}" ;;
    p) set_viz ;;
    a) set_allviz ;;
    m) set_model ;;
    s) set_SO4 ;;
    b) set_SO24 ;;
    y) set_google ;;
    y) only_google ;;
    r) set_satellite ;;
    t) set_sattopo ;;
    f) set_ffmpeg ;;
    h) print_usage
      exit 1 ;;
    *) print_usage
      exit 1 ;;
  esac
done

## Checking for inconsistent flags

has_param() {
    local term="$1"
    shift
    for arg; do
        if [[ $arg == "$term" ]]; then
            return 0
        fi
    done
    return 1
}

# SO24
if  has_param '-b' "$@" ; then
if has_param '-s' "$@" ; then
  echo "WARNING: inconsistent settings"
  echo "-b sets both SO2 and SO4"
  echo "-s sets ONLY SO4"
  exit 0
fi
fi

# plot both
if  has_param '-t' "$@" ; then
if has_param '-r' "$@" ; then
  echo "WARNING: inconsistent settings"
  echo "-t sets both satellite and topography"
  echo "-r sets ONLY statellite"
  exit 0
fi
if has_param '-y' "$@" ; then
  echo "WARNING: inconsistent settings"
  echo "-t sets both satellite and topography"
  echo "-y sets ONLY googleplots"
  exit 0
fi
fi

if ! has_param '-p' "$@" ; then
  if has_param '-b' "$@" ; then
    echo "WARNING viz turned off"
    exit 0
  fi
  if has_param '-s' "$@" ; then
    echo "WARNING viz turned off"
    exit 0
  fi
  if has_param '-g' "$@" ; then
    echo "WARNING viz turned off"
    exit 0
  fi
  if has_param '-y' "$@" ; then
    echo "WARNING viz turned off"
    exit 0
  fi
  if has_param '-r' "$@" ; then
    echo "WARNING viz turned off"
    exit 0
  fi
  if has_param '-t' "$@" ; then
    echo "WARNING viz turned off"
    exit 0
  fi
  if has_param '-f' "$@" ; then
    echo "WARNING viz turned off"
    exit 0
  fi
  if has_param '-a' "$@" ; then
    echo "WARNING viz turned off"
    exit 0
  fi
fi

#                       Description of Settings                          #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

echo 'Running with the following options set:'
echo 'CALPUFF grid resolution: ' $res
echo 'date: '$rundate
echo 'forecast hours: ' $numhours
echo 'run model: '$runmodel
echo 'resoltuion: '$res
echo 'vizulisation: '$runVIS
if [ ${runVIS} = true ]; then
  echo 'vizulisation options:'
  echo '..defaults..'
  echo 'basic plots on: '$runtopo
  echo 'plot SO2: '$runSO2
  echo '..extra..'
  echo 'plot BOTH SO2 and SO4: '$runSO24
  echo 'plot ONLY SO4: '$runSO4
  echo 'include goolge htmls: '$rungoogle
  echo 'plot ONLY SO4: '$runSO4
  echo 'plot ONLY high res set_satellite: '$runsatellite
  echo 'make mp4: ' $runffmpeg
  # VISUALISATION  PATH --> public_html/UNRESP_VIZ/ folders must exist in
  # viz destination.
  VIZPATH=$vizhome/public_html/UNRESP_VIZ/
  echo 'vizulisation output to: '$VIZPATH
fi

if [ ${runVIS} = false ] && [ ${runmodel} = false ]; then
  echo 'running model and vizulisation turned off'
  echo 'terminating programme'
  echo 'plrease review options'
  print_usage
  exit 1
fi

#                               RUN DATE                                 #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
case $numhours in
  48)
    prevdate=$(date -d "$rundate - 1 day" +%Y%m%d)
    middate=$(date -d "$rundate + 1 day" +%Y%m%d)
    enddate=$(date -d "$rundate + 2 days" +%Y%m%d)
  ;;
  24)
    prevdate=$(date -d "$rundate + 0 day" +%Y%m%d)
    middate=$(date -d "$rundate + 1 day" +%Y%m%d)
    enddate=$(date -d "$rundate + 1 days" +%Y%m%d)
  ;;
esac
startYear=${rundate:0:4}
startMonth=${rundate:4:2}
startDay=${rundate:6:2}
midYear=${middate:0:4}
midMonth=${middate:4:2}
midDay=${middate:6:2}
endYear=${enddate:0:4}
endMonth=${enddate:4:2}
endDay=${enddate:6:2}

#                               RUN MODEL                                #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
if [ "$runmodel" = true ]; then
  echo "### RUNNING FORECAST SYSTEM FOR DATE "${rundate}" ###"
fi
### TERREL ###
if [ "$runTERREL" = true ]; then
  # Compile TERREL if required:
  cd CALPUFF_EXE
  if [ ! -f ./terrel_intel.exe ]; then
    echo -n "### COMPILING TERREL"
      ifort -O0 -fltconsistency -w ../CALPUFF_SRC/TERREL/terrel.for -o terrel_intel.exe
    echo " ---> FINISHED ###"
  else
    echo "### TERREL ALREADY COMPILED ###"
  fi
  cd ..
  # Remove any old files before running:
  echo -n "### DELETING ANY OLD TERREL OUTPUT FILES"
  rm -rf *.dat *.grd *.lst *.sav *.log
  cd CALPUFF_OUT/TERREL
  find . ! -name 'README' -type f -exec rm -f {} +
  cd ../..
  echo " ---> FINISHED ###"
  # Update input file:
  echo -n "### SETTING UP TERREL INPUT FILE"
  sed -e "s/?NX?/$NX/g" -e "s/?NY?/$NY/g" -e "s/?DGRIDKM?/$DGRIDKM/g" ./CALPUFF_INP/terrel_template.inp > ./CALPUFF_INP/terrel.inp
  echo " ---> FINISHED ###"
  # Run TERREL:
  echo "### RUNNING TERREL"
  ./CALPUFF_EXE/terrel_intel.exe ./CALPUFF_INP/terrel.inp > terrel.log
  echo " ---> FINISHED ###"
  # Move output files:
  echo -n "### MOVING TERREL OUTPUT FILES"
  mv *.dat *.grd *.lst *.sav *.log ./CALPUFF_OUT/TERREL/.
  echo " ---> FINISHED ###"
fi

### CTGPROC ###
if [ "$runCTGPROC" = true ]; then
  # Compile CTGPROC if required:
  cd CALPUFF_EXE
  if [ ! -f ./ctgproc_intel.exe ]; then
    echo -n "### COMPILING CTGPROC"
    ifort -O0 -fltconsistency -mcmodel=medium -w ../CALPUFF_SRC/CTGPROC/ctgproc.for -o ctgproc_intel.exe
    echo " ---> FINISHED ###"
  else
    echo "### CTGPROC ALREADY COMPILED ###"
  fi
  cd ..
  # Remove any old files before running:
  echo -n "### DELETING ANY OLD CTGPROC OUTPUT FILES"
  rm -rf *.dat *.lst *.log
  cd CALPUFF_OUT/CTGPROC
  find . ! -name 'README' -type f -exec rm -f {} +
  cd ../..
  echo " ---> FINISHED ###"
  # Update input file:
  echo -n "### SETTING UP CTGPROC INPUT FILE"
  sed -e "s/?MESHGLAZ?/$MESHGLAZ/g" -e "s/?NX?/$NX/g" -e "s/?NY?/$NY/g" -e "s/?DGRIDKM?/$DGRIDKM/g" ./CALPUFF_INP/ctgproc_template.inp > ./CALPUFF_INP/ctgproc.inp
  echo " ---> FINISHED ###"
  # Run CTGPROC:
  echo "### RUNNING CTGPROC"
  ./CALPUFF_EXE/ctgproc_intel.exe ./CALPUFF_INP/ctgproc.inp > ctgproc.log
  echo " ---> FINISHED ###"
  # Move output files:
  echo -n "### MOVING CTGPROC OUTPUT FILES"
  mv *.dat *.lst *.log ./CALPUFF_OUT/CTGPROC/.
  echo " ---> FINISHED ###"
fi

### MAKEGEO ###
if [ "$runMAKEGEO" = true ]; then
  # Compile MAKEGEO if required:
  cd CALPUFF_EXE
  if [ ! -f ./makegeo_intel.exe ]; then
    echo -n "### COMPILING MAKEGEO"
    ifort -O0 -fltconsistency -w ../CALPUFF_SRC/MAKEGEO/makegeo.for -o makegeo_intel.exe
    echo " ---> FINISHED ###"
  else
    echo "### MAKEGEO ALREADY COMPILED ###"
  fi
  cd ..
  # Copy data files from TERREL and CTGPROC across to the data directory
  echo -n "### COPYING GEO DATA FILES ACROSS"
  cp -f ./CALPUFF_OUT/TERREL/masaya.dat data/.
  cp -f ./CALPUFF_OUT/CTGPROC/lulc1km_masaya.dat data/.
  echo " ---> FINISHED ###"
  # Remove any old files before running:
  echo -n "### DELETING ANY OLD MAKEGEO OUTPUT FILES"
  rm -rf *.dat *.lst *.clr *.log *.grd
  cd CALPUFF_OUT/MAKEGEO
  find . ! -name 'README' -type f -exec rm -f {} +
  cd ../..
  echo " ---> FINISHED ###"
  # Update input file:
  echo -n "### SETTING UP MAKEGEO INPUT FILE"
  sed -e "s/?NX?/$NX/g" -e "s/?NY?/$NY/g" -e "s/?DGRIDKM?/$DGRIDKM/g" ./CALPUFF_INP/makegeo_template.inp > ./CALPUFF_INP/makegeo.inp
  echo " ---> FINISHED ###"
  # Run MAKEGEO:
  echo "### RUNNING MAKEGEO"
  ./CALPUFF_EXE/makegeo_intel.exe ./CALPUFF_INP/makegeo.inp > makegeo.log
  echo " ---> FINISHED ###"
  # Move output files:
  echo -n "### MOVING MAKEGEO OUTPUT FILES"
  mv *.dat *.lst *.clr *.log *.grd ./CALPUFF_OUT/MAKEGEO/.
  echo " ---> FINISHED ###"
fi

#              GET AND PROCESS NAM DATA              #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
if [ "$run3DDAT" = true ]; then
  ## Download NAM data if required:
  # How many files downloaded already?:
  if [ -d ./NAM_data/raw/${rundate} ]; then
    eval numfiles=$(ls ./NAM_data/raw/${rundate} | wc -l)
  else
    numfiles=0
  fi
  # if not correct no files, need to download more:
  if [ ${numfiles} != $NAMno ]; then
    echo "### ATTEMPTING TO DOWNLOAD NAM DATA"
    # Make data directory if required:
    if [ ! -d ./NAM_data/raw/${rundate}  ]; then
      mkdir NAM_data/raw/${rundate}
    fi
    cd NAM_data/raw/${rundate}
    # Download each NAM data file if required:
    for i in `seq 0 3 48`; do
      hour=`printf "%02d" $i`
      if [ ! -f nam.t00z.afwaca${hour}.tm00.grib2 ]; then
	echo "### DOWNLOADING DATA FOR FORECAST HOUR "${hour}" ###"
	# Entire GRIB file:
	# wget http://www.ftp.ncep.noaa.gov/data/nccf/com/nam/prod/nam.${rundate}/nam.t00z.afwaca${hour}.tm00.grib2
	# Subset of GRIB file using GRIB filter (http://nomads.ncep.noaa.gov/cgi-bin/filter_nam_crb.pl):
  #WARNING https not http as of Jan 2019
	curl "https://nomads.ncep.noaa.gov/cgi-bin/filter_nam_crb.pl?file=nam.t00z.afwaca"${hour}".tm00.grib2&"\
"lev_1000_mb=on&lev_100_mb=on&lev_10_mb=on&lev_150_mb=on&lev_200_mb=on&lev_20_mb=on&lev_250_mb=on&"\
"lev_2_mb=on&lev_300_mb=on&lev_30_mb=on&lev_400_mb=on&lev_500_mb=on&lev_50_mb=on&lev_5_mb=on&"\
"lev_600_mb=on&lev_700_mb=on&lev_75_mb=on&lev_7_mb=on&lev_800_mb=on&lev_850_mb=on&lev_900_mb=on&"\
"lev_925_mb=on&lev_950_mb=on&lev_mean_sea_level=on&var_HGT=on&var_PRMSL=on&var_RH=on&var_TMP=on&var_UGRD=on&var_VGRD=on&"\
"var_DZDT=on&subregion=&leftlon=272&rightlon=278&toplat=16&bottomlat=10&dir=%2Fnam."${rundate} \
-o nam.t00z.afwaca${hour}.tm00.grib2
      fi
    done
    cd ../../..
    echo " ---> FINISHED ###"
  fi
  # CHECK files are all as expected!
  cd NAM_data/raw/${rundate}
  eval checkgrib=$(file -b --mime-type * | sed 's|/.*||' | grep text | wc -l)
  cd ../../..
  # Extract NAM data into CALMET input file format:
  if [ ${checkgrib} != 0 ]; then
    echo "Grib check failed, checkcd internet connect or NAM data availability"
    exit 0
  fi
  echo "### EXTRACTING NAM DATA INTO CALMET INPUT FILE FORMAT"
  rm -f NAM_data/processed/met_${rundate}.dat
  cd NAM_data/raw/input
  for hr in `seq 0 3 48`; do
    hr=`printf "%02d" $hr`
    ln -sf ../${rundate}/nam.t00z.afwaca${hr}.tm00.grib2 VOLC_MAS${hr}${timeforfile}.grib
  done
  cd ../../..
  echo 00${timeforfile} > timestmp.dat
  #echo ${rundate}00 > timestmp.dat
  cp IMO/Caleta/grid.dat .
  ./CALPUFF_EXE/caleta_masaya
  mv out_caleta.m3d NAM_data/processed/met_${rundate}.dat
  echo " ---> FINISHED ###"
fi

### CALMET ###
if [ "$runCALMET" = true ]; then
  # Compile CALMET if required:
  cd CALPUFF_EXE
  if [ ! -f ./calmet_intel.exe ]; then
    echo -n "### COMPILING CALMET"
    ifort -O0 -fltconsistency -mcmodel=medium -w ../CALPUFF_SRC/CALMET/calmet.for -o calmet_intel.exe
    echo " ---> FINISHED ###"
  else
    echo "### CALMET ALREADY COMPILED ###"
  fi
  cd ..
  # Remove any old data files and copy relevant new files into the data directory
  echo -n "### SETTING UP DATA DIRECTORY"
  rm -f data/geo_masaya.dat
  cp -f ./CALPUFF_OUT/MAKEGEO/geo_masaya.dat data/.
  rm -f data/met_*.dat
  cp -f ./NAM_data/processed/met_${rundate}.dat data/.
  echo " ---> FINISHED ###"
  # Remove any old CALMET files before running:
  echo -n "### DELETING ANY OLD CALMET OUTPUT FILES"
  rm -rf *.dat *.DAT *.bna *.lst *.aux
  rm -rf ./CALPUFF_OUT/CALMET/${rundate}
  echo " ---> FINISHED ###"
  # Update input file:
  echo -n "### SETTING UP CALMET INPUT FILE"
  sed -e "s/YYYYb/$startYear/g" -e "s/MMb/$startMonth/g" -e "s/DDb/$startDay/g" -e "s/YYYYe/$endYear/g" \
-e "s/MMe/$endMonth/g" -e "s/DDe/$endDay/g" -e "s/?3DDAT?/met_${rundate}.dat/g" \
-e "s/?NX?/$NX/g" -e "s/?NY?/$NY/g" -e "s/?DGRIDKM?/$DGRIDKM/g" ./CALPUFF_INP/calmet_template.inp > ./CALPUFF_INP/calmet.inp
  echo " ---> FINISHED ###"
  # Run CALMET:
  echo "### RUNNING CALMET"
  ./CALPUFF_EXE/calmet_intel.exe ./CALPUFF_INP/calmet.inp
  echo " ---> FINISHED ###"
  # Move output files:
  echo -n "### MOVING CALMET OUTPUT FILES"
  mkdir ./CALPUFF_OUT/CALMET/${rundate}
  mv *.dat *.DAT *.bna *.lst *.aux ./CALPUFF_OUT/CALMET/${rundate}/.
  echo " ---> FINISHED ###"
fi

### CALPUFF ###
if [ "$runCALPUFF" = true ]; then
  # Compile CALPUFF if required:
  if [ ! -f ./CALPUFF_EXE/calpuff_intel.exe ]; then
    echo -n "### COMPILING CALPUFF"
    cd CALPUFF_SRC/CALPUFF
    ifort -c modules.for
    cd ../../CALPUFF_EXE
    ifort -O0 -fltconsistency -mcmodel=medium -w ../CALPUFF_SRC/CALPUFF/calpuff.for ../CALPUFF_SRC/CALPUFF/modules.o -o calpuff_intel.exe
    cd ..
    echo " ---> FINISHED ###"
  else
    echo "### CALPUFF ALREADY COMPILED ###"
  fi
  # Remove old and copy new CALMET data file across to the data directory
  echo -n "### SETTING UP DATA DIRECTORY"
  rm -f data/calmet_*.dat
  cp -f ./CALPUFF_OUT/CALMET/${rundate}/calmet.dat data/calmet_${rundate}.dat
  echo " ---> FINISHED ###"
  # Remove any old files before running:
  echo -n "### DELETING ANY OLD CALPUFF OUTPUT FILES"
  rm -rf *.con *.lst *.dat *.clr *.bna *.grd
  rm -rf ./CALPUFF_OUT/CALPUFF/${rundate}
  echo " ---> FINISHED ###"
  # Set up input file for first 24hrs:
  echo -n "### SETTING UP CALPUFF INPUT FILE FOR FIRST 24 HOURS"
  if [ -f ./CALPUFF_OUT/CALPUFF/${prevdate}/restart_${rundate}.dat ]; then
    mres=3
    cp CALPUFF_OUT/CALPUFF/${prevdate}/restart_${rundate}.dat .
    echo -n " ---> RESTART FILE FOUND"
  else
    mres=2
    echo -n " ---> NO RESTART FILE FOUND"
  fi
  sed -e "s/YYYYb/$startYear/g" -e "s/MMb/$startMonth/g" -e "s/DDb/$startDay/g" -e "s/YYYYe/$midYear/g" \
-e "s/MMe/$midMonth/g" -e "s/DDe/$midDay/g" -e "s/?METDAT?/calmet_${rundate}.dat/g" \
-e "s/?RSTARTB?/restart_$rundate.dat/g" -e "s/?RSTARTE?/restart_$middate.dat/g" \
-e "s/?MRES?/$mres/g" -e "s/?NX?/$NX/g" -e "s/?NY?/$NY/g" -e "s/?DGRIDKM?/$DGRIDKM/g" \
./CALPUFF_INP/calpuff_template.inp > ./CALPUFF_INP/calpuff.inp
  echo " ---> FINISHED ###"
  # Run CALPUFF for first 24 hours:
  echo "### RUNNING CALPUFF FOR FIRST 24 HOURS"
  ./CALPUFF_EXE/calpuff_intel.exe ./CALPUFF_INP/calpuff.inp
  echo " ---> FINISHED ###"
  # Move output files from first 24 hours:
  echo -n "### MOVING CALPUFF OUTPUT FILES FROM FIRST 24 HOURS"
  mkdir ./CALPUFF_OUT/CALPUFF/${rundate}
  mv concrec*.dat restart_${middate}.dat ./CALPUFF_OUT/CALPUFF/${rundate}/.
  rm -rf *.con *.lst *.dat *.clr *.bna *.grd
  if [ ${numhours} == 48 ] ; then
    cp CALPUFF_OUT/CALPUFF/${rundate}/restart_${middate}.dat .
    echo " ---> FINISHED ###"
    # Set up input file for second 24hrs:
    echo -n "### SETTING UP CALPUFF INPUT FILE FOR SECOND 24 HOURS"
    sed -e "s/YYYYb/$midYear/g" -e "s/MMb/$midMonth/g" -e "s/DDb/$midDay/g" -e "s/YYYYe/$endYear/g" \
-e "s/MMe/$endMonth/g" -e "s/DDe/$endDay/g" -e "s/?METDAT?/calmet_${rundate}.dat/g" \
-e "s/?RSTARTB?/restart_$middate.dat/g" -e "s/?RSTARTE?/restart_$enddate.dat/g" \
-e "s/?MRES?/1/g" -e "s/?NX?/$NX/g" -e "s/?NY?/$NY/g" -e "s/?DGRIDKM?/$DGRIDKM/g" \
./CALPUFF_INP/calpuff_template.inp > ./CALPUFF_INP/calpuff.inp
    echo " ---> FINISHED ###"
    # Run CALPUFF for second 24 hours:
    echo "### RUNNING CALPUFF FOR SECOND 24 HOURS"
    ./CALPUFF_EXE/calpuff_intel.exe ./CALPUFF_INP/calpuff.inp
    echo " ---> FINISHED ###"
    # Rename and move output files from second 24 hours:
    echo -n "### RENAMING AND MOVING CALPUFF OUTPUT FILES FROM SECOND 24 HOURS"
    for i in `seq 1 24`; do
      let "j = i + 24"
      i2=`printf "%02d" $i`
      j2=`printf "%02d" $j`
      mv concrec0100${i2}.dat concrec0100${j2}.dat
      mv concrec0200${i2}.dat concrec0200${j2}.dat
    done
    mv concrec*.dat ./CALPUFF_OUT/CALPUFF/${rundate}/.
    rm -f *.con *.lst *.dat *.clr *.bna *.grd
    echo " ---> FINISHED ###"
  fi
fi

#                              RUN VISUALIZATION                         #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
if [ ${runVIS} = true ]; then
  echo "### RUNNING VISUALISATION TOOLS"
  rm -rf ./vis/${rundate}
  mkdir ./vis/${rundate}
  cd Python
  case $numhours in
    48)
      python genmaps.py $rundate $vizopt $SOopt $googleopt
      ;;
    24)
      python genmaps.py $rundate --conc 24 $vizopt $SOopt $googleopt
      ;;
  esac
  cd ..
  cd vis/${rundate}
  if [ ${runffmpeg} = true ]; then
    echo "Running ffmpeg"
    ffmpeg -i SO2_static_concrec0100%02d.png -c:v libx264 -crf 23 -profile:v baseline -level 3.0 -pix_fmt yuv420p -c:a aac -ac 2 -b:a 128k -r 4 -movflags faststart movie_${rundate}.mp4
  fi
  cd ../..
  echo " ---> FINISHED ###"

  echo "Adding latest VISUALISATION to website at "$VIZPATH

  cd vis/${rundate}
  echo "Reformatting png to jpg"
  mogrify -format jpg *.png
  rm -f *.png
  echo 'making readable by all'
  setfacl -m other:r *.jpg
  chmod og+rx *.jpg
  if [ ! -e $VIZPATH${rundate} ]
  then
    echo 'making folder'
    mkdir $VIZPATH${rundate}
  fi
  echo 'checking for google files'
  # add in a check for goolge files incase missing API key
  count=`ls -1 *.html 2>/dev/null | wc -l`
  if [ $count != 0 ]
  then
    setfacl -m other:r-x *.html
    chmod og+rx *.html
    mv *.html $VIZPATH${rundate}
  fi
  echo 'moving to public_html'
  mv *.jpg $VIZPATH${rundate}
  cd $VIZPATH
  echo 'Linking run to Today'
  rm -f Today
  ln -sf $(date +%Y%m%d) Today
  cd $cwd
  echo 'COMPLETED all visualisation steps'
fi

#------------------------------------------------------------------------#
#------------------- BESPOKE LEEDS ARCHIVNG FLAGS------------------------#
#------------------------------------------------------------------------#
if [ $USER == earmgr ];  then
# On the first day of each month archive last month.
day=`date '+%d'`
if [[ "$day" == 01 ]];
then
  echo "### WARNING: Time to Archive Previous month ###"
fi
fi

if [ "$runmodel" = true ]; then
  echo "### SUCCESSFULLY COMPLETED FORECAST ###"
else
  echo "### SUCCESSFULLY COMPLETED TASK ###"
fi

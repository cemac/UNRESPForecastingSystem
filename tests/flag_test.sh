#!/usr/bin/bash --login

# This script was created by CEMAC (University of Leeds) as
# part of the UNRESP Project
# Setup environment (should not need to be edited)
set -e # stop at first error
# load modules (Leeds)
module load intel/17.0.0
module load python2 python-libs
# For Mark only:
export PYTHONPATH="/nfs/see-fs-02_users/earmgr/SW/eccodes-2.6.0/lib/python2.7/site-packages:${PYTHONPATH}"

# Resolution (m) of intended CALPUFF grid.  100 < (integer) < 1000
res=1000
# Defaults that can be overwritten by editing HERE:
# Command line option m switches all to false
runTERREL=true
runCTGPROC=true
runMAKEGEO=true
run3DDAT=true
runCALMET=true
runCALPUFF=true
runmodel=true

# Set other parameters (unlikely to need editing)
let NX=90000/$res+1
let NY=54000/$res+1
DGRIDKM=$(echo "scale=3; $res/1000" | bc)
let MESHGLAZ=1000/$res+1
echo 'CALPUFF grid resolution: ' $res
cwd=$(pwd)


#------------------------------------------------------------------------#
#------------------- DO NOT ALTER BELOW THIS LINE------------------------#
#------------------------------------------------------------------------#

# Defaults that can be overwritten via command line
rundate=$(date +%Y%m%d)
vizhome=~earunres
runVIS=false
rungoogle=false
runsatellite=false
runtopo=true
runSO4=false
runSO2=true
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
 Today, Viz on, plots production area (~earunres).

 Options:
  -d <date> YYYYMMDD DEFAULT: <today's date>
  -n <home> name of viz defaults to ~earunres
  -x <res> resolution in m (100 < x < 1000)
 **
 The following switches can be used to overwrite
 Default behaviour.

 DEFAULT: output todays SO2 concrec files on topography
          background
 **
  -m turn OFF Forecasting model
  -p turn ON viz steps: SO2 on topography only
  -f turn ON ffmpeg mp4 production
  -s SWITCH to SO4
  -b plot BOTH SO2 and SO4
  -g turn ON GOOGLE PLOTS
  -r SWITCH to satellite background
  -t output BOTH satellite and topo backgrounds
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


  "
}

set_viz() {
  runVIS=true
}

set_SO4() {
  runSO4=true
  runSO2=false
}

set_SO24() {
  runSO24=true
}

set_google() {
  rungoogle=ture
}
set_satellite() {
  runsatellite=true
  runtopo=false
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
while getopts 'd:n:x:pmsbgrfh' flag; do
  case "${flag}" in
    d) rundate="${OPTARG}" ;;
    n) vizhome="${OPTARG}" ;;
    x) res="${OPTARG}" ;;
    p) set_viz ;;
    m) set_model ;;
    s) set_SO4 ;;
    b) set_SO24 ;;
    g) set_google ;;
    r) set_satellite ;;
    t) set_sattopo ;;
    f) set_ffmpeg ;;
    h) print_usage
      exit 1 ;;
    *) print_usage
      exit 1 ;;
  esac
done

echo 'Running with the following options set:'
echo 'date: '$rundate
echo 'run model: '$runmodel
echo 'vizulisation: '$runVIS
if [ ${runVIS} = true ]; then
  echo 'vizulisation options:'
  echo 'plot SO2: '$runVIS
  echo 'plot SO4: '$runSO4
  echo 'plot high res set_satellite: '$runsatellite
  echo 'output goolge htmls: '$rungoogle
  echo 'make mp4: ' $runffmpeg
  # VISUALISATION  PATH --> public_html/UNRESP_VIZ/ folders must exist in
  # viz destination.
  VIZPATH=$vizhome/public_html/UNRESP_VIZ/
  echo 'vizulisation output to: '$VIZPATH
fi

if [ ${runVIS} = false ] & [ ${runmodel} = false ]; then
  echo 'running model and vizulisation turned off'
  echo 'terminating programme'
  echo 'plrease review options'
  print_usage
  exit 1
fi

prevdate=$(date -d "$rundate - 1 day" +%Y%m%d)
middate=$(date -d "$rundate + 1 day" +%Y%m%d)
enddate=$(date -d "$rundate + 2 days" +%Y%m%d)
startYear=${rundate:0:4}
startMonth=${rundate:4:2}
startDay=${rundate:6:2}
midYear=${middate:0:4}
midMonth=${middate:4:2}
midDay=${middate:6:2}
endYear=${enddate:0:4}
endMonth=${enddate:4:2}
endDay=${enddate:6:2}

if [ "$runmodel" = true ]; then
  echo "### RUNNING FORECAST SYSTEM FOR DATE "${rundate}" ###"
fi
### TERREL ###
if [ "$runTERREL" = true ]; then
  echo "runTERREL"
  # Compile TERREL if required:
  if [ ! -f ./terrel_intel.exe ]; then
    echo -n "### COMPILING TERREL"
    echo " ---> FINISHED ###"
  else
    echo "### TERREL ALREADY COMPILED ###"
  fi
fi

### CTGPROC ###
if [ "$runCTGPROC" = true ]; then
  echo "runCTGPROC"
  if [ ! -f ./ctgproc_intel.exe ]; then
    echo -n "### COMPILING CTGPROC"
    echo " ---> FINISHED ###"
  else
    echo "### CTGPROC ALREADY COMPILED ###"
  fi
fi

### MAKEGEO ###
if [ "$runMAKEGEO" = true ]; then
  echo "runMAKEGEO"
  if [ ! -f ./makegeo_intel.exe ]; then
    echo -n "### COMPILING MAKEGEO"
    echo " ---> FINISHED ###"
  else
    echo "### MAKEGEO ALREADY COMPILED ###"
  fi
fi

### NAM data ###
if [ "$run3DDAT" = true ]; then
  echo "run3DDAT"
  if [ -d ./NAM_data/raw/${rundate} ]; then
    eval numfiles=$(ls ./NAM_data/raw/${rundate} | wc -l)
  else
    numfiles=0
  fi
  # if not 17 files, need to download more:
  if [ ${numfiles} != 17 ]; then
    echo "### ATTEMPTING TO DOWNLOAD NAM DATA"
    # Make data directory if required:
    if [ ! -d ./NAM_data/raw/${rundate}  ]; then
      echo "making NAM folder"
    fi
    for i in `seq 0 3 48`; do
      hour=`printf "%02d" $i`
      if [ ! -f nam.t00z.afwaca${hour}.tm00.grib2 ]; then
        echo "### DOWNLOADING DATA FOR FORECAST HOUR "${hour}" ###"
      fi
    done
  fi
  # Extract NAM data into CALMET input file format:
  eval checkgrib=$(file -b --mime-type * | sed 's|/.*||' | grep text | wc -l)
  if [ ${checkgrib} != 0 ]; then
    echo "Grib check failed, check internet connect or NAM data availability"
    #exit 0
  fi
  echo " ---> FINISHED ###"
fi

### CALMET ###
if [ "$runCALMET" = true ]; then
  echo "CALMET"
  if [ ! -f ./calmet_intel.exe ]; then
    echo -n "### COMPILING CALMET"
    echo " ---> FINISHED ###"
  else
    echo "### CTGPROC ALREADY COMPILED ###"
  fi
fi

### CALPUFF ###
if [ "$runCALPUFF" = true ]; then
  echo "CALPUFF"
  if [ ! -f ./CALPUFF_EXE/calpuff_intel.exe ]; then
    echo -n "### COMPILING CALPUFF"
    echo " ---> FINISHED ###"
  else
    echo "### CALPUFF ALREADY COMPILED ###"
  fi
  echo " ---> FINISHED ###"
fi

### VISUALISATION ###
if [ ${runVIS} = true ]; then
  echo "VIZ"
  if [ ${runffmpeg} = true ]; then
    echo "Running ffmpeg"
  fi
  echo " ---> FINISHED ###"

  echo 'checking for google files'
  # add in a check for goolge files incase missing API key
  count=`ls -1 *.html 2>/dev/null | wc -l`
  if [ $count != 0 ]
  then
    echo "making googlefiles readable"
  fi
  echo 'COMPLETED all visualisation steps'
fi
#------------------------------------------------------------------------#
#------------------- BESPOKE LEEDS ARCHIVNG FLAGS------------------------#
#------------------------------------------------------------------------#

# On the first day of each month archive last month.
day=`date '+%d'`
if [[ "$day" == 01 ]];
then
  echo "### WARNING: Time to Archive Previous month ###"
fi
if [ "$runmodel" = true ]; then
  echo "### SUCCESSFULLY COMPLETED FORECAST ###"
else
  echo "### SUCCESSFULLY COMPLETED TASK ###"
fi

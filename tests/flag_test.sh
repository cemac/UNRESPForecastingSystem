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
cwd=$(pwd)


#------------------------------------------------------------------------#
#------------------- DO NOT ALTER BELOW THIS LINE------------------------#
#------------------------------------------------------------------------#

#                       COMMAND LINE FLAG HANDELING                      #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Defaults that can be overwritten via command line
rundate=$(date +%Y%m%d)
vizhome=~earunres
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
while getopts 'd:n:x:pamsbgrtyfh' flag; do
  case "${flag}" in
    d) rundate="${OPTARG}" ;;
    n) vizhome="${OPTARG}" ;;
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
  echo 'plot BOTH satellite and topo plots: '$runsatopo
  echo 'include goolge htmls: '$rungoogle
  echo 'plot ONLY SO4: '$runSO4
  echo 'plot ONLY high res set_satellite: '$runsatellite
  echo 'plot ONLY only to google: '$rungoogle
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

#                               RUN DATE                                 #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

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

#                               RUN MODEL                                #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
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

#              GET AND PROCESS NAM DATA              #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
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

#                              RUN VISUALIZATION                         #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
if [ ${runVIS} = true ]; then
  echo "VIZ"
  python genmaps_test.py $rundate $vizopt $SOopt $googleopt
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
